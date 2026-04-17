#lang racket/base

(require datastar
         datastar/testing
         racket/list
         racket/string
         rackunit)

(provide testing-tests)

(define testing-tests
  (test-suite "testing"

    ;; make-mock-sse tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

    ;; make-recording-sse tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                      #:mode 'inner
                      #:namespace 'svg
                      #:use-view-transitions? #t
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

    (test-case "make-recording-sse: explicit default patch-elements datalines are omitted"
      (define-values (sse get-events) (make-recording-sse))
      (patch-elements sse
                      "<p>content</p>"
                      #:mode 'outer
                      #:namespace 'html
                      #:use-view-transitions? #f)
      (define evt (first (get-events)))
      (check-false (member "mode outer" (sse-event-data-lines evt)))
      (check-false (member "namespace html" (sse-event-data-lines evt)))
      (check-false (member "useViewTransition false" (sse-event-data-lines evt))))

    (test-case "make-recording-sse: close-sse prevents further sends"
      (define-values (sse get-events) (make-recording-sse))
      (patch-elements sse "<div>before</div>")
      (close-sse sse)
      (check-exn exn:fail? (lambda () (patch-elements sse "<div>after</div>")))
      (check-equal? (length (get-events)) 1))

    (test-case "make-recording-sse: only-if-missing signal"
      (define-values (sse get-events) (make-recording-sse))
      (patch-signals sse "{\"x\":1}" #:only-if-missing? #t)
      (define evt (first (get-events)))
      (check-not-false (member "onlyIfMissing true" (sse-event-data-lines evt))))

    (test-case "make-recording-sse: default only-if-missing dataline omitted"
      (define-values (sse get-events) (make-recording-sse))
      (patch-signals sse "{\"x\":1}" #:only-if-missing? #f)
      (define evt (first (get-events)))
      (check-false (member "onlyIfMissing false" (sse-event-data-lines evt))))

    ;; parse-sse-events edge cases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "parse-sse-events: empty string"
      (define-values (_sse get-events) (make-recording-sse))
      (check-equal? (get-events) '()))

    (test-case "parse-sse-events: roundtrip single event"
      (define-values (_sse _get-output) (make-mock-sse))
      (patch-elements _sse "<div>hi</div>")
      (define-values (sse2 get-events) (make-recording-sse))
      (patch-elements sse2 "<div>hi</div>")
      (define events (get-events))
      (check-equal? (length events) 1)
      (check-equal? (sse-event-type (first events)) "datastar-patch-elements")
      (check-equal? (sse-event-data-lines (first events)) '("elements <div>hi</div>")))

    (test-case "parse-sse-events: roundtrip with id and retry"
      (define-values (sse get-events) (make-recording-sse))
      (patch-elements sse "<div>x</div>" #:event-id "abc" #:retry-duration 5000)
      (define events (get-events))
      (define evt (first events))
      (check-equal? (sse-event-id evt) "abc")
      (check-equal? (sse-event-retry evt) 5000))))

(module+ test
  (require rackunit/text-ui)
  (run-tests testing-tests))
