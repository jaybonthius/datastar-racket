#lang racket/base

(require datastar
         (only-in datastar/private/sse make-test-sse get-test-output)
         racket/string
         rackunit
         xml)

(provide elements-tests)

(define elements-tests
  (test-suite "elements"

    (test-case "patch-elements produces correct SSE format"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div id=\"out\">hi</div>")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: elements <div id=\"out\">hi</div>\n"
                                   "\n")))

    (test-case "patch-elements with all options"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen
                      "<div>content</div>"
                      #:selector "#target"
                      #:mode patch-mode-inner
                      #:namespace element-namespace-svg
                      #:use-view-transitions #t
                      #:event-id "evt-1"
                      #:retry-duration 2000)
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "id: evt-1\n"
                                   "retry: 2000\n"
                                   "data: mode inner\n"
                                   "data: selector #target\n"
                                   "data: namespace svg\n"
                                   "data: useViewTransition true\n"
                                   "data: elements <div>content</div>\n"
                                   "\n")))

    (test-case "default mode outer is omitted"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>test</div>" #:mode patch-mode-outer)
      (define result (get-test-output out))
      (check-false (string-contains? result "mode")))

    (test-case "default retry duration 1000 is omitted"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>test</div>" #:retry-duration 1000)
      (define result (get-test-output out))
      (check-false (string-contains? result "retry")))

    (test-case "non-default retry duration is included"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>test</div>" #:retry-duration 3000)
      (define result (get-test-output out))
      (check-true (string-contains? result "retry: 3000")))

    (test-case "multi-line elements get separate data lines"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>\n  hello\n</div>")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: elements <div>\n"
                                   "data: elements   hello\n"
                                   "data: elements </div>\n"
                                   "\n")))

    (test-case "remove-elements produces correct format"
      (define-values (gen out) (make-test-sse))
      (remove-elements gen "#gone")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: mode remove\n"
                                   "data: selector #gone\n"
                                   "\n")))

    (test-case "patch-elements/xexpr produces same output as patch-elements"
      (define-values (gen1 out1) (make-test-sse))
      (define-values (gen2 out2) (make-test-sse))
      (patch-elements gen1 (xexpr->string '(div ((id "out")) "hi")))
      (patch-elements/xexpr gen2 '(div ((id "out")) "hi"))
      (check-equal? (get-test-output out1) (get-test-output out2)))

    (test-case "patch-elements/xexpr with all options"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexpr gen
                            '(div "content")
                            #:selector "#target"
                            #:mode patch-mode-inner
                            #:namespace element-namespace-svg
                            #:use-view-transitions #t
                            #:event-id "evt-1"
                            #:retry-duration 2000)
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "id: evt-1\n"
                                   "retry: 2000\n"
                                   "data: mode inner\n"
                                   "data: selector #target\n"
                                   "data: namespace svg\n"
                                   "data: useViewTransition true\n"
                                   "data: elements <div>content</div>\n"
                                   "\n")))

    (test-case "patch-elements/xexpr with nested xexpr"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexpr gen '(div ((id "x")) (span "hello") " " (span "world")))
      (define result (get-test-output out))
      (check-true (string-contains?
                   result
                   "elements <div id=\"x\"><span>hello</span> <span>world</span></div>")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests elements-tests))
