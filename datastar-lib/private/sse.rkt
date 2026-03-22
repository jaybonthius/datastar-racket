#lang racket/base

(provide make-sse ;; review: ignore
         sse?
         sse-out
         sse-semaphore
         sse-closed-box
         sse-lock-held?
         sse-closed?
         call-with-sse-lock
         sse-send
         make-test-sse
         get-test-output)

(struct sse (out semaphore closed-box lock-held?) #:constructor-name make-sse) ;; review: ignore

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
                        (flush-output (sse-out gen)))))

(define (make-test-sse)
  (define out (open-output-string))
  (values (make-sse out (make-semaphore 1) (box #f) (make-thread-cell #f #f)) out))

(define (get-test-output port)
  (get-output-string port))
