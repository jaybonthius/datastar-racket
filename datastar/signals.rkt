#lang racket/base

(require json
         racket/contract/base
         racket/match
         racket/string
         web-server/http/request-structs
         "constants.rkt"
         "private/utils.rkt"
         "sse.rkt")

(provide (contract-out [patch-signals
                        (->* [sse? (or/c string? jsexpr?)]
                             [#:event-id (or/c string? #f)
                              #:only-if-missing (or/c boolean? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             void?)]
                       [read-signals (-> request? jsexpr?)]))

(define (patch-signals gen
                       signals
                       #:event-id [event-id #f]
                       #:only-if-missing [only-if-missing #f]
                       #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-patch-signals signals
                                 #:event-id event-id
                                 #:only-if-missing only-if-missing
                                 #:retry-duration retry-duration)))

(define (read-signals request)
  (match (request-method request)
    [#"GET"
     (define datastar-binding
       (findf (lambda (binding)
                (equal? (bytes->string/utf-8 (binding-id binding)) (symbol->string datastar-key)))
              (request-bindings/raw request)))
     (unless datastar-binding
       (error "No datastar parameter found in request bindings"))
     (string->jsexpr (bytes->string/utf-8 (binding:form-value datastar-binding)))]
    [_
     (define body (request-post-data/raw request))
     (unless body
       (error "No request body found"))
     (string->jsexpr (bytes->string/utf-8 body))]))

(define (build-patch-signals signals
                             #:event-id [event-id #f]
                             #:only-if-missing [only-if-missing #f]
                             #:retry-duration [retry-duration #f])
  (define signals-str
    (if (string? signals)
        signals
        (jsexpr->string signals)))

  (define data-lines
    (append (filter values
                    (list (and only-if-missing
                               (not (eq? only-if-missing default-patch-signals-only-if-missing))
                               (string-append (symbol->string only-if-missing-dataline-literal)
                                              " "
                                              (js-bool only-if-missing)))))
            (map (lambda (line) (string-append (symbol->string signals-dataline-literal) " " line))
                 (string-split signals-str "\n"))))

  (send-event event-type-patch-signals
              data-lines
              #:event-id event-id
              #:retry-duration retry-duration))

(module+ test
  (require net/url
           racket/promise
           rackunit
           web-server/http/response-structs
           (only-in (submod "sse.rkt" internal) make-test-sse get-test-output))

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

  ;; read-signals helpers
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
    (check-exn exn:fail? (lambda () (read-signals req)))))
