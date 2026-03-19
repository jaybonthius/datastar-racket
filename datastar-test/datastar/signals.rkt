#lang racket/base

(require datastar
         (only-in datastar/private/sse make-test-sse get-test-output)
         net/url
         racket/promise
         racket/string
         rackunit
         web-server/http/request-structs
         web-server/http/response-structs)

(provide signals-tests)

;; read-signals helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-get-request signals-json)
  (make-request #"GET"
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                '()
                (delay
                  (list (make-binding:form #"datastar" (string->bytes/utf-8 signals-json))))
                #f
                "127.0.0.1"
                8080
                "127.0.0.1"))

(define (make-post-request body-json)
  (make-request #"POST"
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                '()
                (delay
                  '())
                (string->bytes/utf-8 body-json)
                "127.0.0.1"
                8080
                "127.0.0.1"))

(define (make-request-with-headers method headers)
  (make-request (string->bytes/utf-8 method)
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                headers
                (delay
                  '())
                #f
                "127.0.0.1"
                8080
                "127.0.0.1"))

(define signals-tests
  (test-suite "signals"

    (test-case "patch-signals with hash produces correct format"
      (define-values (gen out) (make-test-sse))
      (patch-signals gen (hash 'count 1))
      (define result (get-test-output out))
      (check-true (string-contains? result "event: datastar-patch-signals"))
      (check-true (string-contains? result "data: signals {")))

    (test-case "patch-signals with only-if-missing"
      (define-values (gen out) (make-test-sse))
      (patch-signals gen (hash 'x 1) #:only-if-missing #t)
      (define result (get-test-output out))
      (check-true (string-contains? result "data: onlyIfMissing true")))

    (test-case "read-signals parses GET request with datastar query param"
      (define req (make-get-request "{\"count\":42,\"msg\":\"hello\"}"))
      (define signals (read-signals req))
      (check-equal? (hash-ref signals 'count) 42)
      (check-equal? (hash-ref signals 'msg) "hello"))

    (test-case "read-signals parses POST request with JSON body"
      (define req (make-post-request "{\"name\":\"test\",\"value\":true}"))
      (define signals (read-signals req))
      (check-equal? (hash-ref signals 'name) "test")
      (check-equal? (hash-ref signals 'value) #t))

    (test-case "read-signals parses nested JSON structures"
      (define req (make-post-request "{\"user\":{\"name\":\"jay\",\"age\":30},\"items\":[1,2,3]}"))
      (define signals (read-signals req))
      (check-equal? (hash-ref (hash-ref signals 'user) 'name) "jay")
      (check-equal? (hash-ref signals 'items) '(1 2 3)))

    (test-case "read-signals errors on GET without datastar param"
      (define req
        (make-request #"GET"
                      (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                      '()
                      (delay
                        '())
                      #f
                      "127.0.0.1"
                      8080
                      "127.0.0.1"))
      (check-exn exn:fail? (lambda () (read-signals req))))

    (test-case "read-signals errors on POST without body"
      (define req
        (make-request #"POST"
                      (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                      '()
                      (delay
                        '())
                      #f
                      "127.0.0.1"
                      8080
                      "127.0.0.1"))
      (check-exn exn:fail? (lambda () (read-signals req))))

    (test-case "datastar-request? returns #t when header is present"
      (define req (make-request-with-headers "GET" (list (make-header #"Datastar-Request" #"true"))))
      (check-true (datastar-request? req)))

    (test-case "datastar-request? returns #f without header"
      (define req (make-request-with-headers "GET" '()))
      (check-false (datastar-request? req)))

    (test-case "datastar-request? is case-insensitive"
      (define req (make-request-with-headers "POST" (list (make-header #"datastar-request" #"True"))))
      (check-true (datastar-request? req)))

    (test-case "datastar-request? returns #f when header value is not true"
      (define req (make-request-with-headers "GET" (list (make-header #"Datastar-Request" #"false"))))
      (check-false (datastar-request? req)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests signals-tests))
