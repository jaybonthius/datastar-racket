#lang racket

(require datastar
         web-server/servlet-env)

(provide start-test-server)

(define (handle-execute-script event)
  (execute-script (hash-ref event 'script "")
                  #:auto-remove (hash-ref event 'autoRemove #t)
                  #:attributes (hash-ref event 'attributes #f)
                  #:event-id (hash-ref event 'eventId #f)
                  #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-patch-elements event)
  (patch-elements (hash-ref event 'elements #f)
                  #:selector (hash-ref event 'selector #f)
                  #:mode (hash-ref event 'mode #f)
                  #:use-view-transitions (hash-ref event 'useViewTransition #f)
                  #:event-id (hash-ref event 'eventId #f)
                  #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-patch-signals event)
  (define signals-data
    (cond
      [(hash-has-key? event 'signals-raw) (hash-ref event 'signals-raw)]
      [(hash-has-key? event 'signals) (hash-ref event 'signals)]
      [else (hash)]))
  (patch-signals signals-data
                 #:only-if-missing (hash-ref event 'onlyIfMissing #f)
                 #:event-id (hash-ref event 'eventId #f)
                 #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-remove-elements event)
  (remove-elements (hash-ref event 'selector "")
                   #:event-id (hash-ref event 'eventId #f)
                   #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-remove-signals event)
  (patch-signals (hash)
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

(define (test-handler req)
  (define signals (read-signals req))
  (define events (hash-ref signals 'events '()))
  (process-events req events))

(define (process-events req events)
  (define results
    (for/list ([event events])
      (process-single-event event)))
  (datastar-response results))

(define (process-single-event event)
  (define event-type (hash-ref event 'type ""))
  (define handler (hash-ref event-handlers event-type #f))
  (handler event))

(define (start-test-server #:port [port 7331])
  (serve/servlet test-handler
                 #:port port
                 #:servlet-regexp #rx""
                 #:launch-browser? #f
                 #:stateless? #t))
