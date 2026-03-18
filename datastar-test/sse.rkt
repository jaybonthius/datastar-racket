#lang racket/base

(require datastar
         (only-in (submod datastar/sse internal) make-write-profile make-test-sse get-test-output)
         datastar/testing
         net/url
         racket/promise
         racket/string
         rackunit
         web-server/http/request-structs
         web-server/http/response-structs)

(provide sse-tests)

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

(define sse-tests
  (test-suite "sse"

    (test-case "send succeeds on open connection"
      (define-values (gen _out) (make-test-sse))
      (check-not-exn (lambda () (patch-elements gen "<div>hello</div>"))))

    (test-case "send raises after port is closed"
      (define-values (gen out) (make-test-sse))
      (close-output-port out)
      (check-exn exn:fail? (lambda () (patch-elements gen "<div>hello</div>"))))

    (test-case "send raises after close-sse"
      (define-values (gen _out) (make-test-sse))
      (close-sse gen)
      (check-exn exn:fail? (lambda () (patch-elements gen "<div>hello</div>"))))

    (test-case "close-sse is idempotent"
      (define-values (gen _out) (make-test-sse))
      (close-sse gen)
      (check-not-exn (lambda () (close-sse gen))))

    (test-case "sse-closed? returns #f on fresh connection"
      (define-values (gen _out) (make-test-sse))
      (check-false (sse-closed? gen)))

    (test-case "sse-closed? returns #t after close-sse"
      (define-values (gen _out) (make-test-sse))
      (close-sse gen)
      (check-true (sse-closed? gen)))

    (test-case "sse-closed? returns #t when output port closed"
      (define-values (gen out) (make-test-sse))
      (close-output-port out)
      (check-true (sse-closed? gen)))

    (test-case "concurrent sends do not interleave"
      (define-values (gen out) (make-test-sse))
      (define threads
        (for/list ([i (in-range 10)])
          (thread (lambda () (patch-elements gen (format "<div>thread ~a</div>" i))))))
      (for-each thread-wait threads)
      (define result (get-output-string out))
      (define events (regexp-match* #rx"event: datastar-patch-elements\n" result))
      (check-equal? (length events) 10))

    (test-case "with-sse-lock: sse-send works inside lock (reentrant)"
      (define-values (gen out) (make-test-sse))
      (check-not-exn (lambda ()
                       (with-sse-lock gen
                                      (patch-elements gen "<div>first</div>")
                                      (patch-elements gen "<div>second</div>"))))
      (define result (get-output-string out))
      (check-true (string-contains? result "first"))
      (check-true (string-contains? result "second")))

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
                    ;; NO with-sse-lock — each send locks independently
                    (patch-elements gen (format "<div>start-~a</div>" i))
                    (sleep 0.01)
                    (patch-elements gen (format "<div>end-~a</div>" i))))))
      ;; Release all threads at once
      (for ([_ (in-range 5)])
        (semaphore-post barrier))
      (for-each thread-wait threads)
      (define result (get-output-string out))
      ;; Extract the sequence of data values in order
      (define data-values
        (regexp-match* #rx"data: elements <div>([a-z]+-[0-9])" result #:match-select cadr))
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
                                   (patch-elements gen (format "<div>start-~a</div>" i))
                                   (sleep 0.01)
                                   (patch-elements gen (format "<div>end-~a</div>" i)))))))
      (for-each thread-wait threads)
      (define result (get-output-string out))
      (define pairs (regexp-match* #rx"start-[0-9].*?end-[0-9]" result))
      (check-equal? (length pairs) 5)
      (for ([p (in-list pairs)])
        (define nums (regexp-match* #rx"[0-9]" p))
        (check-equal? (car nums) (cadr nums))))

    (test-case "with-sse-lock: lock released after exception"
      (define-values (gen out) (make-test-sse))
      (with-handlers ([exn:fail? void])
        (with-sse-lock gen (error "boom!")))
      (check-not-exn (lambda () (patch-elements gen "<div>after-error</div>")))
      (check-true (string-contains? (get-output-string out) "after-error")))

    (test-case "call-with-sse-lock: nested locks do not deadlock"
      (define-values (gen out) (make-test-sse))
      (check-not-exn
       (lambda ()
         (call-with-sse-lock
          gen
          (lambda () (call-with-sse-lock gen (lambda () (patch-elements gen "<div>nested</div>")))))))
      (check-true (string-contains? (get-output-string out) "nested")))

    (test-case "with-sse-lock: child thread does not inherit lock"
      (define-values (gen out) (make-test-sse))
      (define result-ch (make-channel))
      (with-sse-lock gen
                     (define _child
                       (thread (lambda ()
                                 (patch-elements gen "<div>child</div>")
                                 (channel-put result-ch 'child-done))))
                     (sleep 0.05)
                     (define got (sync/timeout 0 result-ch))
                     (check-false got "child should be blocked while parent holds lock")
                     (patch-elements gen "<div>parent</div>"))
      (sync/timeout 2 result-ch)
      (define result (get-output-string out))
      (define parent-pos (caar (regexp-match-positions #rx"parent" result)))
      (define child-pos (caar (regexp-match-positions #rx"child" result)))
      (check-true (< parent-pos child-pos) "parent send should come before child send"))

    ;; Accept-Encoding negotiation tests

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
      (check-false (response-has-content-encoding? resp "br")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sse-tests))
