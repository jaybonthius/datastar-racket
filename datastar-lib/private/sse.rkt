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

(define (sse-closed? sse)
  (or (unbox (sse-closed-box sse)) (port-closed? (sse-out sse))))

(define (call-with-sse-lock sse thunk)
  (if (thread-cell-ref (sse-lock-held? sse))
      (thunk)
      (call-with-semaphore
       (sse-semaphore sse)
       (lambda ()
         (thread-cell-set! (sse-lock-held? sse) #t)
         (dynamic-wind void thunk (lambda () (thread-cell-set! (sse-lock-held? sse) #f)))))))

(define (sse-send sse event-str)
  (call-with-sse-lock sse
                      (lambda ()
                        (when (sse-closed? sse)
                          (error 'sse-send "connection is closed"))
                        (write-string event-str (sse-out sse))
                        (flush-output (sse-out sse)))))

(define (make-test-sse)
  (define out (open-output-string))
  (values (make-sse out (make-semaphore 1) (box #f) (make-thread-cell #f #f)) out))

(define (get-test-output port)
  (get-output-string port))
