#lang racket

(require datastar
         web-server/servlet-env)

(provide start-test-server)

(define (handle-execute-script sse event)
  (execute-script sse
                  (hash-ref event 'script "")
                  #:auto-remove (hash-ref event 'autoRemove #t)
                  #:attributes (hash-ref event 'attributes #f)
                  #:event-id (hash-ref event 'eventId #f)
                  #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-patch-elements sse event)
  (patch-elements sse
                  (hash-ref event 'elements #f)
                  #:selector (hash-ref event 'selector #f)
                  #:mode (hash-ref event 'mode #f)
                  #:namespace (hash-ref event 'namespace #f)
                  #:use-view-transitions (hash-ref event 'useViewTransition #f)
                  #:event-id (hash-ref event 'eventId #f)
                  #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-patch-signals sse event)
  (define signals-data
    (cond
      [(hash-has-key? event 'signals-raw) (hash-ref event 'signals-raw)]
      [(hash-has-key? event 'signals) (hash-ref event 'signals)]
      [else (hash)]))
  (patch-signals sse
                 signals-data
                 #:only-if-missing (hash-ref event 'onlyIfMissing #f)
                 #:event-id (hash-ref event 'eventId #f)
                 #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-remove-elements sse event)
  (remove-elements sse
                   (hash-ref event 'selector "")
                   #:event-id (hash-ref event 'eventId #f)
                   #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-remove-signals sse event)
  (patch-signals sse
                 (hash)
                 #:event-id (hash-ref event 'eventId #f)
                 #:retry-duration (hash-ref event 'retryDuration #f)))

(define event-handlers
  (hash "executeScript"
        handle-execute-script
        "patchElements"
        handle-patch-elements
        "patchSignals"
        handle-patch-signals
        "removeElements"
        handle-remove-elements
        "removeSignals"
        handle-remove-signals))

(define (process-single-event sse event)
  (define event-type (hash-ref event 'type ""))
  (define handler (hash-ref event-handlers event-type #f))
  (when handler
    (handler sse event)))

(define (test-handler req)
  (define signals (read-signals req))
  (define events (hash-ref signals 'events '()))
  (datastar-sse req
                (lambda (sse)
                  (for ([event events])
                    (process-single-event sse event)))))

(define (start-test-server #:port [port 7331])
  (serve/servlet test-handler
                 #:port port
                 #:servlet-regexp #rx""
                 #:launch-browser? #f
                 #:stateless? #t))
