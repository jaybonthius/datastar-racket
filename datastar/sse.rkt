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
                       [call-with-sse-lock (-> sse? (-> any) any)]
                       [write-profile? (-> any/c boolean?)]
                       [basic-write-profile write-profile?])
         with-sse-lock)

(define current-datastar-connection (make-parameter #f))

(struct write-profile (wrap-output flush! content-encoding) #:constructor-name make-write-profile)

(define basic-write-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) #f))

(struct sse (out raw-out flush! semaphore closed-box lock-held?) #:constructor-name make-sse)

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

(define (sse-closed? gen)
  (or (unbox (sse-closed-box gen)) (port-closed? (sse-out gen))))

(define (call-with-sse-lock gen thunk)
  (if (thread-cell-ref (sse-lock-held? gen))
      (thunk)
      (call-with-semaphore
       (sse-semaphore gen)
       (lambda ()
         (thread-cell-set! (sse-lock-held? gen) #t)
         (dynamic-wind void thunk (lambda () (thread-cell-set! (sse-lock-held? gen) #f)))))))

(define-syntax-rule (with-sse-lock gen body ...)
  (call-with-sse-lock gen
                      (lambda ()
                        body ...)))

(define (sse-send gen event-str)
  (call-with-sse-lock gen
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
    (values (make-sse out
                      out
                      (lambda (_wrapped raw) (flush-output raw))
                      (make-semaphore 1)
                      (box #f)
                      (make-thread-cell #f #f))
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

  (test-case "with-sse-lock: sse-send works inside lock (reentrant)"
    (define-values (gen out) (make-test-sse))
    (check-not-exn (lambda ()
                     (with-sse-lock gen
                                    (sse-send gen "event: test\ndata: first\n\n")
                                    (sse-send gen "event: test\ndata: second\n\n"))))
    (define result (get-output-string out))
    (check-true (string-contains? result "data: first"))
    (check-true (string-contains? result "data: second")))

  (test-case "without with-sse-lock: sends from other threads CAN interleave"
    ;; Proves that batch locking is actually needed: without it, a sleep
    ;; between two sends lets another thread's event slip in between.
    (define-values (gen out) (make-test-sse))
    (define barrier (make-semaphore 0))
    (define threads
      (for/list ([i (in-range 5)])
        (thread (lambda ()
                  ;; All threads start together
                  (semaphore-wait barrier)
                  ;; NO with-sse-lock — each sse-send locks independently
                  (sse-send gen (format "event: test\ndata: start-~a\n\n" i))
                  (sleep 0.01)
                  (sse-send gen (format "event: test\ndata: end-~a\n\n" i))))))
    ;; Release all threads at once
    (for ([_ (in-range 5)])
      (semaphore-post barrier))
    (for-each thread-wait threads)
    (define result (get-output-string out))
    ;; Extract the sequence of data values in order
    (define data-values (regexp-match* #rx"data: ([a-z]+-[0-9])" result #:match-select cadr))
    ;; Without batch locking, at least one start-N should NOT be immediately
    ;; followed by its matching end-N (another thread's event slipped in).
    (define adjacent-pairs
      (for/sum
       ([i (in-range (sub1 (length data-values)))])
       (define this (list-ref data-values i))
       (define next (list-ref data-values (add1 i)))
       (define num (substring this (sub1 (string-length this))))
       (if (and (string-prefix? this "start-") (equal? next (string-append "end-" num))) 1 0)))
    (check-true (< adjacent-pairs 5) "without batch lock, some pairs should be interleaved"))

  (test-case "with-sse-lock: batch sends are not interleaved by other threads"
    (define-values (gen out) (make-test-sse))
    (define threads
      (for/list ([i (in-range 5)])
        (thread (lambda ()
                  (with-sse-lock gen
                                 (sse-send gen (format "event: test\ndata: start-~a\n\n" i))
                                 (sleep 0.01)
                                 (sse-send gen (format "event: test\ndata: end-~a\n\n" i)))))))
    (for-each thread-wait threads)
    (define result (get-output-string out))
    ;; Each start-N must be immediately followed by end-N (no interleaving)
    (define pairs (regexp-match* #rx"start-[0-9].*?end-[0-9]" result))
    (check-equal? (length pairs) 5)
    (for ([p (in-list pairs)])
      (define nums (regexp-match* #rx"[0-9]" p))
      (check-equal? (car nums) (cadr nums))))

  (test-case "with-sse-lock: lock released after exception"
    (define-values (gen out) (make-test-sse))
    (with-handlers ([exn:fail? void])
      (with-sse-lock gen (error "boom!")))
    ;; Lock should be released: subsequent send must succeed
    (check-not-exn (lambda () (sse-send gen "event: test\ndata: after-error\n\n")))
    (check-true (string-contains? (get-output-string out) "data: after-error")))

  (test-case "call-with-sse-lock: nested locks do not deadlock"
    (define-values (gen out) (make-test-sse))
    (check-not-exn
     (lambda ()
       (call-with-sse-lock
        gen
        (lambda ()
          (call-with-sse-lock gen (lambda () (sse-send gen "event: test\ndata: nested\n\n")))))))
    (check-true (string-contains? (get-output-string out) "data: nested")))

  (test-case "with-sse-lock: child thread does not inherit lock"
    (define-values (gen out) (make-test-sse))
    (define result-ch (make-channel))
    (with-sse-lock gen
                   ;; Spawn a child thread that tries to send.  Because the child does NOT
                   ;; inherit the lock-held? thread-cell, it must wait for the semaphore.
                   ;; We verify this by checking that the child's send only completes after
                   ;; the outer lock is released.
                   (define child
                     (thread (lambda ()
                               (sse-send gen "event: test\ndata: child\n\n")
                               (channel-put result-ch 'child-done))))
                   (sleep 0.05)
                   ;; Child should still be blocked (semaphore held by us)
                   (define got (sync/timeout 0 result-ch))
                   (check-false got "child should be blocked while parent holds lock")
                   (sse-send gen "event: test\ndata: parent\n\n"))
    ;; Now lock is released, child can proceed
    (sync/timeout 2 result-ch)
    (define result (get-output-string out))
    ;; Parent's send must appear before child's send
    (define parent-pos (caar (regexp-match-positions #rx"parent" result)))
    (define child-pos (caar (regexp-match-positions #rx"child" result)))
    (check-true (< parent-pos child-pos) "parent send should come before child send"))

  ;; Accept-Encoding negotiation tests
  (define fake-br-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) 'br))

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
