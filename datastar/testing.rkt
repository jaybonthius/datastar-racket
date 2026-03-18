#lang racket/base

(require racket/contract/base
         racket/list
         racket/match
         racket/string
         (only-in "sse.rkt" sse?)
         (only-in (submod "sse.rkt" internal) make-test-sse get-test-output))

;; -------- sse-event struct --------

(struct sse-event (type id retry data-lines) #:transparent)

;; -------- SSE text parser --------

(define (parse-sse-events raw)
  (cond
    [(string=? raw "") '()]
    [else
     (define chunks (regexp-split #rx"\n\n" raw))
     (filter-map parse-one-event chunks)]))

(define (parse-one-event chunk)
  (define lines (string-split chunk "\n"))
  (define non-empty (filter (lambda (l) (not (string=? l ""))) lines))
  (cond
    [(null? non-empty) #f]
    [else
     (define type #f)
     (define id #f)
     (define retry #f)
     (define data '())
     (for ([line (in-list non-empty)])
       (match line
         [(regexp #rx"^event: (.+)$" (list _ t)) (set! type t)]
         [(regexp #rx"^id: (.+)$" (list _ i)) (set! id i)]
         [(regexp #rx"^retry: (.+)$" (list _ r)) (set! retry (string->number r))]
         [(regexp #rx"^data: (.*)$" (list _ d)) (set! data (append data (list d)))]
         [_ (void)]))
     (and type (sse-event type id retry data))]))

;; -------- Public API --------

(define (make-mock-sse)
  (define-values (gen out) (make-test-sse))
  (values gen (lambda () (get-test-output out))))

(define (make-recording-sse)
  (define-values (gen out) (make-test-sse))
  (values gen (lambda () (parse-sse-events (get-test-output out)))))

;; -------- Contracts & provide --------

(provide (contract-out (struct sse-event
                               ([type string?] [id (or/c string? #f)]
                                               [retry (or/c exact-nonnegative-integer? #f)]
                                               [data-lines (listof string?)]))
                       [make-mock-sse (-> (values sse? (-> string?)))]
                       [make-recording-sse (-> (values sse? (-> (listof sse-event?))))]))

;; -------- Tests --------

(module+ test
  (require rackunit
           "constants.rkt"
           "elements.rkt"
           "scripts.rkt"
           "signals.rkt")

  ;; -- make-mock-sse tests --

  (test-case "make-mock-sse: empty output before any sends"
    (define-values (_sse get-output) (make-mock-sse))
    (check-equal? (get-output) ""))

  (test-case "make-mock-sse: captures raw SSE text"
    (define-values (sse get-output) (make-mock-sse))
    (patch-elements sse "<div id=\"x\">hi</div>")
    (define out (get-output))
    (check-true (string-contains? out "event: datastar-patch-elements"))
    (check-true (string-contains? out "data: elements <div id=\"x\">hi</div>")))

  (test-case "make-mock-sse: multiple events accumulate"
    (define-values (sse get-output) (make-mock-sse))
    (patch-elements sse "<div>1</div>")
    (patch-signals sse "{\"x\":1}")
    (define out (get-output))
    (check-true (string-contains? out "event: datastar-patch-elements"))
    (check-true (string-contains? out "event: datastar-patch-signals")))

  ;; -- make-recording-sse tests --

  (test-case "make-recording-sse: empty list before any sends"
    (define-values (_sse get-events) (make-recording-sse))
    (check-equal? (get-events) '()))

  (test-case "make-recording-sse: single patch-elements event"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse "<div>test</div>")
    (define events (get-events))
    (check-equal? (length events) 1)
    (define evt (first events))
    (check-equal? (sse-event-type evt) "datastar-patch-elements")
    (check-false (sse-event-id evt))
    (check-false (sse-event-retry evt))
    (check-equal? (sse-event-data-lines evt) '("elements <div>test</div>")))

  (test-case "make-recording-sse: patch-signals event"
    (define-values (sse get-events) (make-recording-sse))
    (patch-signals sse "{\"count\":42}")
    (define events (get-events))
    (check-equal? (length events) 1)
    (define evt (first events))
    (check-equal? (sse-event-type evt) "datastar-patch-signals")
    (check-not-false (member "signals {\"count\":42}" (sse-event-data-lines evt))))

  (test-case "make-recording-sse: execute-script event"
    (define-values (sse get-events) (make-recording-sse))
    (execute-script sse "console.log('hi')")
    (define events (get-events))
    (check-equal? (length events) 1)
    (define evt (first events))
    ;; execute-script uses patch-elements internally
    (check-equal? (sse-event-type evt) "datastar-patch-elements"))

  (test-case "make-recording-sse: event with id and retry"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse "<div>test</div>" #:event-id "evt-1" #:retry-duration 2000)
    (define events (get-events))
    (check-equal? (length events) 1)
    (define evt (first events))
    (check-equal? (sse-event-id evt) "evt-1")
    (check-equal? (sse-event-retry evt) 2000))

  (test-case "make-recording-sse: default retry duration omitted"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse "<div>test</div>" #:retry-duration 1000)
    (define evt (first (get-events)))
    ;; 1000 is the default, so it should be omitted from the SSE output
    (check-false (sse-event-retry evt)))

  (test-case "make-recording-sse: multiple events in order"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse "<div>first</div>")
    (patch-signals sse "{\"x\":1}")
    (patch-elements sse "<div>third</div>")
    (define events (get-events))
    (check-equal? (length events) 3)
    (check-equal? (sse-event-type (first events)) "datastar-patch-elements")
    (check-equal? (sse-event-type (second events)) "datastar-patch-signals")
    (check-equal? (sse-event-type (third events)) "datastar-patch-elements"))

  (test-case "make-recording-sse: multi-line elements"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse "<div>\n  hello\n</div>")
    (define evt (first (get-events)))
    (check-equal? (sse-event-data-lines evt)
                  '("elements <div>" "elements   hello" "elements </div>")))

  (test-case "make-recording-sse: patch-elements with all options"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse
                    "<p>content</p>"
                    #:selector "#target"
                    #:mode patch-mode-inner
                    #:namespace element-namespace-svg
                    #:use-view-transitions #t
                    #:event-id "e1"
                    #:retry-duration 3000)
    (define evt (first (get-events)))
    (check-equal? (sse-event-type evt) "datastar-patch-elements")
    (check-equal? (sse-event-id evt) "e1")
    (check-equal? (sse-event-retry evt) 3000)
    (check-not-false (member "mode inner" (sse-event-data-lines evt)))
    (check-not-false (member "selector #target" (sse-event-data-lines evt)))
    (check-not-false (member "namespace svg" (sse-event-data-lines evt)))
    (check-not-false (member "useViewTransition true" (sse-event-data-lines evt)))
    (check-not-false (member "elements <p>content</p>" (sse-event-data-lines evt))))

  (test-case "make-recording-sse: close-sse prevents further sends"
    (define-values (sse get-events) (make-recording-sse))
    (patch-elements sse "<div>before</div>")
    ((dynamic-require "sse.rkt" 'close-sse) sse)
    (check-exn exn:fail? (lambda () (patch-elements sse "<div>after</div>")))
    ;; Only the first event should be recorded
    (check-equal? (length (get-events)) 1))

  (test-case "make-recording-sse: only-if-missing signal"
    (define-values (sse get-events) (make-recording-sse))
    (patch-signals sse "{\"x\":1}" #:only-if-missing #t)
    (define evt (first (get-events)))
    (check-not-false (member "onlyIfMissing true" (sse-event-data-lines evt))))

  ;; -- parse-sse-events edge cases --

  (test-case "parse-sse-events: empty string"
    (check-equal? (parse-sse-events "") '()))

  (test-case "parse-sse-events: roundtrip single event"
    (define raw "event: datastar-patch-elements\ndata: elements <div>hi</div>\n\n")
    (define events (parse-sse-events raw))
    (check-equal? (length events) 1)
    (check-equal? (sse-event-type (first events)) "datastar-patch-elements")
    (check-equal? (sse-event-data-lines (first events)) '("elements <div>hi</div>")))

  (test-case "parse-sse-events: roundtrip with id and retry"
    (define raw
      "event: datastar-patch-elements\nid: abc\nretry: 5000\ndata: elements <div>x</div>\n\n")
    (define events (parse-sse-events raw))
    (define evt (first events))
    (check-equal? (sse-event-id evt) "abc")
    (check-equal? (sse-event-retry evt) 5000)))
