#lang racket/base

(provide make-write-profile
         write-profile?
         write-profile-wrap-output
         write-profile-flush!
         write-profile-content-encoding
         basic-write-profile
         make-sse
         sse?
         sse-out
         sse-raw-out
         sse-flush!
         sse-semaphore
         sse-closed-box
         sse-lock-held?
         sse-closed?
         call-with-sse-lock
         sse-send
         make-test-sse
         get-test-output)

(struct write-profile (wrap-output flush! content-encoding) #:constructor-name make-write-profile)

(define basic-write-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) #f))

(struct sse (out raw-out flush! semaphore closed-box lock-held?) #:constructor-name make-sse)

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

(define (sse-send gen event-str)
  (call-with-sse-lock gen
                      (lambda ()
                        (when (sse-closed? gen)
                          (error 'sse-send "connection is closed"))
                        (write-string event-str (sse-out gen))
                        ((sse-flush! gen) (sse-out gen) (sse-raw-out gen)))))

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
  (get-output-string port))
