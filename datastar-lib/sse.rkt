#lang racket/base

(require racket/contract/base
         racket/string
         web-server/http
         web-server/http/response
         web-server/private/connection-manager
         web-server/servlet/servlet-structs
         "private/constants.rkt"
         "private/sse.rkt")

(provide (contract-out [datastar-sse
                        (->* [request? (-> sse? any)]
                             [#:on-close (or/c (-> sse? any) #f) #:write-profile write-profile?]
                             response?)]
                       [dispatch/datastar
                        (-> (-> request? can-be-response?) (-> connection? request? any))]
                       [close-sse (-> sse? void?)]
                       [sse-closed? (-> sse? boolean?)]
                       [sse? (-> any/c boolean?)]
                       [call-with-sse-lock (-> sse? (-> any) any)]
                       [write-profile? (-> any/c boolean?)]
                       [basic-write-profile write-profile?])
         with-sse-lock)

(define current-datastar-connection (make-parameter #f))

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
  (define encoding-str (symbol->string encoding))
  (define accept-header
    (for/or ([h (in-list (request-headers/raw request))])
      (and (equal? (string-downcase (bytes->string/utf-8 (header-field h))) "accept-encoding")
           (header-value h))))
  (and accept-header
       (for/or ([part (in-list (regexp-split #rx"," (bytes->string/utf-8 accept-header)))])
         (string-ci=? (string-trim (car (string-split part ";"))) encoding-str))))

(define (datastar-sse request
                      on-open
                      #:on-close [on-close #f]
                      #:write-profile [wp basic-write-profile])
  (define wp*
    (let ([enc (write-profile-content-encoding wp)])
      (if (and enc (not (accepts-encoding? request enc))) basic-write-profile wp)))
  (define encoding (write-profile-content-encoding wp*))
  (define extra-headers
    (for/list ([(k v) (in-hash sse-headers)])
      (make-header (string->bytes/utf-8 k) (string->bytes/utf-8 v))))
  (define all-headers
    (if encoding
        (cons (make-header #"Content-Encoding" (string->bytes/utf-8 (symbol->string encoding)))
              extra-headers)
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
                (make-sse wrapped-out
                          out
                          (write-profile-flush! wp*)
                          (make-semaphore 1)
                          (box #f)
                          (make-thread-cell #f #f)))
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

(define-syntax-rule (with-sse-lock gen body ...)
  (call-with-sse-lock gen
                      (lambda ()
                        body ...)))
