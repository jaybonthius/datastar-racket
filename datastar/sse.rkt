#lang racket/base

(require racket/contract/base
         racket/string
         web-server/http
         web-server/http/response
         web-server/private/connection-manager
         web-server/servlet/servlet-structs
         "constants.rkt")

(provide (contract-out [datastar-sse
                        (->* [request? (-> sse? any)]
                             [#:on-close (or/c (-> sse? any) #f) #:write-profile write-profile?]
                             response?)]
                       [dispatch/datastar
                        (-> (-> request? can-be-response?) (-> connection? request? any))]
                       [close-sse (-> sse? void?)]
                       [sse? (-> any/c boolean?)]
                       [sse-send (-> sse? string? void?)]
                       [write-profile? (-> any/c boolean?)]
                       [basic-write-profile write-profile?]))

(define current-datastar-connection (make-parameter #f))

(struct write-profile (wrap-output flush! content-encoding) #:constructor-name make-write-profile)

(define basic-write-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) #f))

(struct sse (out raw-out flush! semaphore closed-box) #:constructor-name make-sse)

(define (dispatch/datastar servlet)
  (lambda (conn req)
    (parameterize ([current-datastar-connection conn])
      (output-response conn (servlet req)))))

(define (close-sse gen)
  (unless (unbox (sse-closed-box gen))
    (set-box! (sse-closed-box gen) #t)
    (with-handlers ([exn:fail? void])
      (unless (eq? (sse-out gen) (sse-raw-out gen))
        (close-output-port (sse-out gen)))
      (flush-output (sse-raw-out gen)))))

(define (accepts-encoding? request encoding)
  (define accept-header
    (for/or ([h (in-list (request-headers/raw request))])
      (and (equal? (string-downcase (bytes->string/utf-8 (header-field h))) "accept-encoding")
           (header-value h))))
  (and accept-header
       (for/or ([part (in-list (regexp-split #rx"," (bytes->string/utf-8 accept-header)))])
         (string-ci=? (string-trim (car (string-split part ";"))) encoding))))

(define (datastar-sse request
                      on-open
                      #:on-close [on-close #f]
                      #:write-profile [wp basic-write-profile])
  (define wp*
    (let ([enc (write-profile-content-encoding wp)])
      (if (and enc (not (accepts-encoding? request enc))) basic-write-profile wp)))
  (define encoding (write-profile-content-encoding wp*))
  (define extra-headers
    (for/list ([(k v) (in-hash SSE-HEADERS)])
      (make-header (string->bytes/utf-8 k) (string->bytes/utf-8 v))))
  (define all-headers
    (if encoding
        (cons (make-header #"Content-Encoding" (string->bytes/utf-8 encoding)) extra-headers)
        extra-headers))
  (define conn (current-datastar-connection))
  (response 200
            #"OK"
            (current-seconds)
            #"text/event-stream"
            all-headers
            (lambda (out)
              (define wrapped-out ((write-profile-wrap-output wp*) out))
              (define gen
                (make-sse wrapped-out out (write-profile-flush! wp*) (make-semaphore 1) (box #f)))
              (define on-open-thread (current-thread))
              (define monitor-thread
                (and conn
                     (thread (lambda ()
                               (sync (connection-i-port conn))
                               (break-thread on-open-thread)))))
              (dynamic-wind void
                            (lambda ()
                              (with-handlers ([exn:break? void]
                                              [exn:fail? void])
                                (on-open gen)))
                            (lambda ()
                              (when monitor-thread
                                (kill-thread monitor-thread))
                              (close-sse gen)
                              (when on-close
                                (on-close gen)))))))

(define (sse-closed? gen)
  (or (unbox (sse-closed-box gen)) (port-closed? (sse-out gen))))

(define (sse-send gen event-str)
  (call-with-semaphore (sse-semaphore gen)
                       (lambda ()
                         (when (sse-closed? gen)
                           (error 'sse-send "connection is closed"))
                         (write-string event-str (sse-out gen))
                         ((sse-flush! gen) (sse-out gen) (sse-raw-out gen)))))

(module+ internal
  (provide make-sse
           make-write-profile
           make-test-sse
           get-test-output
           write-profile?
           write-profile-wrap-output
           write-profile-flush!
           write-profile-content-encoding)

  (define (make-test-sse)
    (define out (open-output-string))
    (values (make-sse out out (lambda (_wrapped raw) (flush-output raw)) (make-semaphore 1) (box #f))
            out))

  (define (get-test-output port)
    (get-output-string port)))

(module+ test
  (require net/url
           racket/promise
           racket/string
           rackunit
           web-server/http/request-structs
           web-server/http/response-structs
           (submod ".." internal))

  (test-case "send succeeds on open connection"
    (define-values (gen _out) (make-test-sse))
    (check-not-exn (lambda () (sse-send gen "event: test\ndata: hello\n\n"))))

  (test-case "send raises after port is closed"
    (define-values (gen out) (make-test-sse))
    (close-output-port out)
    (check-exn exn:fail? (lambda () (sse-send gen "event: test\ndata: hello\n\n"))))

  (test-case "send raises after close-sse"
    (define-values (gen _out) (make-test-sse))
    (close-sse gen)
    (check-exn exn:fail? (lambda () (sse-send gen "event: test\ndata: hello\n\n"))))

  (test-case "close-sse is idempotent"
    (define-values (gen _out) (make-test-sse))
    (close-sse gen)
    (check-not-exn (lambda () (close-sse gen))))

  (test-case "concurrent sends do not interleave"
    (define-values (gen out) (make-test-sse))
    (define threads
      (for/list ([i (in-range 10)])
        (thread (lambda () (sse-send gen (format "event: test\ndata: thread ~a\n\n" i))))))
    (for-each thread-wait threads)
    (define result (get-output-string out))
    (define events (regexp-match* #rx"event: test\n" result))
    (check-equal? (length events) 10)
    (define blocks (string-split result "\n\n"))
    (define non-empty-blocks (filter (lambda (s) (not (string=? s ""))) blocks))
    (check-equal? (length non-empty-blocks) 10))

  ;; Accept-Encoding negotiation tests
  (define fake-br-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) "br"))

  (define (make-request-with-accept-encoding accept-encoding)
    (make-request #"GET"
                  (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                  (if accept-encoding
                      (list (make-header #"Accept-Encoding" (string->bytes/utf-8 accept-encoding)))
                      '())
                  (delay
                    '())
                  #f
                  "127.0.0.1"
                  8080
                  "127.0.0.1"))

  (define (response-has-content-encoding? resp encoding)
    (for/or ([h (in-list (response-headers resp))])
      (and (equal? (header-field h) #"Content-Encoding")
           (equal? (header-value h) (string->bytes/utf-8 encoding)))))

  (test-case "accept-encoding: brotli used when client accepts br"
    (define req (make-request-with-accept-encoding "gzip, deflate, br"))
    (define resp (datastar-sse req (lambda (_sse) (void)) #:write-profile fake-br-profile))
    (check-true (response-has-content-encoding? resp "br")))

  (test-case "accept-encoding: falls back when client does not accept br"
    (define req (make-request-with-accept-encoding "gzip, deflate"))
    (define resp (datastar-sse req (lambda (_sse) (void)) #:write-profile fake-br-profile))
    (check-false (response-has-content-encoding? resp "br")))

  (test-case "accept-encoding: falls back when no Accept-Encoding header"
    (define req (make-request-with-accept-encoding #f))
    (define resp (datastar-sse req (lambda (_sse) (void)) #:write-profile fake-br-profile))
    (check-false (response-has-content-encoding? resp "br")))

  (test-case "accept-encoding: quality weights are stripped"
    (define req (make-request-with-accept-encoding "gzip;q=1.0, br;q=0.5"))
    (define resp (datastar-sse req (lambda (_sse) (void)) #:write-profile fake-br-profile))
    (check-true (response-has-content-encoding? resp "br")))

  (test-case "accept-encoding: basic profile always works regardless of headers"
    (define req (make-request-with-accept-encoding "gzip, deflate"))
    (define resp (datastar-sse req (lambda (_sse) (void)) #:write-profile basic-write-profile))
    (check-false (response-has-content-encoding? resp "br"))))
