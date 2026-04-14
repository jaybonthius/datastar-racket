#lang racket

(require datastar
         web-server/servlet-env)

(provide start-test-server)

(define (string->mode s)
  (and s (string->symbol s)))
(define (string->namespace s)
  (and s (string->symbol s)))

(define (handle-execute-script sse event)
  (execute-script sse
                  (hash-ref event 'script "")
                  #:auto-remove? (hash-ref event 'autoRemove #t)
                  #:attributes (hash-ref event 'attributes #f)
                  #:event-id (hash-ref event 'eventId #f)
                  #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-patch-elements sse event)
  (patch-elements sse
                  (hash-ref event 'elements #f)
                  #:selector (hash-ref event 'selector #f)
                  #:mode (string->mode (hash-ref event 'mode #f))
                  #:namespace (string->namespace (hash-ref event 'namespace #f))
                  #:use-view-transitions? (hash-ref event 'useViewTransition #f)
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
                 #:only-if-missing? (hash-ref event 'onlyIfMissing #f)
                 #:event-id (hash-ref event 'eventId #f)
                 #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-remove-elements sse event)
  (remove-elements sse
                   (hash-ref event 'selector "")
                   #:event-id (hash-ref event 'eventId #f)
                   #:retry-duration (hash-ref event 'retryDuration #f)))

(define (handle-remove-signals sse event)
  (define remove-payload
    (cond
      [(hash-has-key? event 'paths) (hash-ref event 'paths)]
      [(hash-has-key? event 'signals) (hash-ref event 'signals)]
      [else #f]))
  (cond
    [(string? remove-payload)
     (remove-signals sse
                     remove-payload
                     #:event-id (hash-ref event 'eventId #f)
                     #:retry-duration (hash-ref event 'retryDuration #f))]
    [(and (list? remove-payload) (andmap string? remove-payload))
     (remove-signals sse
                     remove-payload
                     #:event-id (hash-ref event 'eventId #f)
                     #:retry-duration (hash-ref event 'retryDuration #f))]
    [(hash? remove-payload)
     (patch-signals sse
                    remove-payload
                    #:event-id (hash-ref event 'eventId #f)
                    #:retry-duration (hash-ref event 'retryDuration #f))]
    [else
     (remove-signals sse
                     '()
                     #:event-id (hash-ref event 'eventId #f)
                     #:retry-duration (hash-ref event 'retryDuration #f))]))

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
  (datastar-sse (lambda (sse)
                  (for ([event events])
                    (process-single-event sse event)))))

(define (start-test-server #:port [port 7331])
  (serve/servlet test-handler
                 #:port port
                 #:servlet-regexp #rx""
                 #:launch-browser? #f
                 #:stateless? #t))
