#lang racket

(require datastar
         web-server/servlet-env)

(provide start-test-server)

(define (string->mode s)
  (and s (string->symbol s)))

(define (string->namespace s)
  (and s (string->symbol s)))

(define (handle-execute-script sse event)
  (define kw (make-hash))
  (when (hash-has-key? event 'autoRemove)
    (hash-set! kw '#:auto-remove? (hash-ref event 'autoRemove)))
  (when (hash-has-key? event 'attributes)
    (hash-set! kw '#:attributes (hash-ref event 'attributes)))
  (when (hash-has-key? event 'eventId)
    (hash-set! kw '#:event-id (hash-ref event 'eventId)))
  (when (hash-has-key? event 'retryDuration)
    (hash-set! kw '#:retry-duration (hash-ref event 'retryDuration)))
  (keyword-apply/dict execute-script
                      kw
                      sse
                      (hash-ref event 'script "")
                      '()))

(define (handle-patch-elements sse event)
  (define kw (make-hash))
  (when (hash-has-key? event 'selector)
    (hash-set! kw '#:selector (hash-ref event 'selector)))
  (when (hash-has-key? event 'mode)
    (hash-set! kw '#:mode (string->mode (hash-ref event 'mode))))
  (when (hash-has-key? event 'namespace)
    (hash-set! kw '#:namespace (string->namespace (hash-ref event 'namespace))))
  (when (hash-has-key? event 'useViewTransition)
    (hash-set! kw '#:use-view-transitions? (hash-ref event 'useViewTransition)))
  (when (hash-has-key? event 'eventId)
    (hash-set! kw '#:event-id (hash-ref event 'eventId)))
  (when (hash-has-key? event 'retryDuration)
    (hash-set! kw '#:retry-duration (hash-ref event 'retryDuration)))
  (keyword-apply/dict patch-elements
                      kw
                      sse
                      (hash-ref event 'elements #f)
                      '()))

(define (handle-patch-signals sse event)
  (define signals-data
    (cond
      [(hash-has-key? event 'signals-raw) (hash-ref event 'signals-raw)]
      [(hash-has-key? event 'signals) (hash-ref event 'signals)]
      [else (hash)]))
  (define kw (make-hash))
  (when (hash-has-key? event 'onlyIfMissing)
    (hash-set! kw '#:only-if-missing? (hash-ref event 'onlyIfMissing)))
  (when (hash-has-key? event 'eventId)
    (hash-set! kw '#:event-id (hash-ref event 'eventId)))
  (when (hash-has-key? event 'retryDuration)
    (hash-set! kw '#:retry-duration (hash-ref event 'retryDuration)))
  (keyword-apply/dict patch-signals kw sse signals-data '()))

(define (handle-remove-elements sse event)
  (define kw (make-hash))
  (when (hash-has-key? event 'eventId)
    (hash-set! kw '#:event-id (hash-ref event 'eventId)))
  (when (hash-has-key? event 'retryDuration)
    (hash-set! kw '#:retry-duration (hash-ref event 'retryDuration)))
  (keyword-apply/dict remove-elements
                      kw
                      sse
                      (hash-ref event 'selector "")
                      '()))

(define (handle-remove-signals sse event)
  (define remove-payload
    (cond
      [(hash-has-key? event 'paths) (hash-ref event 'paths)]
      [(hash-has-key? event 'signals) (hash-ref event 'signals)]
      [else #f]))
  (define kw (make-hash))
  (when (hash-has-key? event 'eventId)
    (hash-set! kw '#:event-id (hash-ref event 'eventId)))
  (when (hash-has-key? event 'retryDuration)
    (hash-set! kw '#:retry-duration (hash-ref event 'retryDuration)))
  (cond
    [(string? remove-payload)
     (keyword-apply/dict remove-signals kw sse remove-payload '())]
    [(and (list? remove-payload) (andmap string? remove-payload))
     (keyword-apply/dict remove-signals kw sse remove-payload '())]
    [(hash? remove-payload)
     (keyword-apply/dict patch-signals kw sse remove-payload '())]
    [else
     (keyword-apply/dict remove-signals kw sse '() '())]))

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
