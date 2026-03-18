#lang racket/base

(require racket/contract/base
         racket/string
         "constants.rkt"
         (submod "constants.rkt" internal)
         "private/utils.rkt"
         "sse.rkt"
         (only-in (submod "sse.rkt" internal) sse-send))

(define element-patch-mode/c
  (or/c patch-mode-outer
        patch-mode-inner
        patch-mode-remove
        patch-mode-replace
        patch-mode-prepend
        patch-mode-append
        patch-mode-before
        patch-mode-after
        #f))
(define element-namespace/c
  (or/c element-namespace-html element-namespace-svg element-namespace-mathml #f))

(provide element-patch-mode/c
         element-namespace/c
         (contract-out [patch-elements
                        (->* [sse? (or/c string? #f)]
                             [#:selector (or/c string? #f)
                              #:mode element-patch-mode/c
                              #:namespace element-namespace/c
                              #:use-view-transitions (or/c boolean? #f)
                              #:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             void?)]
                       [remove-elements
                        (->* [sse? string?]
                             [#:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             void?)]))

(define (patch-elements gen
                        elements
                        #:selector [selector #f]
                        #:mode [mode #f]
                        #:namespace [namespace #f]
                        #:use-view-transitions [use-view-transitions #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-patch-elements elements
                                  #:selector selector
                                  #:mode mode
                                  #:namespace namespace
                                  #:use-view-transitions use-view-transitions
                                  #:event-id event-id
                                  #:retry-duration retry-duration)))

(define (remove-elements gen selector #:event-id [event-id #f] #:retry-duration [retry-duration #f])
  (patch-elements gen
                  #f
                  #:selector selector
                  #:mode patch-mode-remove
                  #:event-id event-id
                  #:retry-duration retry-duration))

(define (build-patch-elements elements
                              #:selector [selector #f]
                              #:mode [mode #f]
                              #:namespace [namespace #f]
                              #:use-view-transitions [use-view-transitions #f]
                              #:event-id [event-id #f]
                              #:retry-duration [retry-duration #f])
  (define data-lines
    (append
     (filter
      values
      (list (and mode
                 (not (eq? mode default-element-patch-mode))
                 (string-append (symbol->string mode-dataline-literal) " " (symbol->string mode)))
            (and selector (string-append (symbol->string selector-dataline-literal) " " selector))
            (and namespace
                 (not (eq? namespace default-element-namespace))
                 (string-append (symbol->string namespace-dataline-literal)
                                " "
                                (symbol->string namespace)))
            (and use-view-transitions
                 (not (eq? use-view-transitions default-elements-use-view-transitions))
                 (string-append (symbol->string use-view-transition-dataline-literal)
                                " "
                                (js-bool use-view-transitions)))))
     (if elements
         (map (lambda (line) (string-append (symbol->string elements-dataline-literal) " " line))
              (string-split elements "\n"))
         '())))

  (send-event event-type-patch-elements
              data-lines
              #:event-id event-id
              #:retry-duration retry-duration))

(module+ internal
  (provide build-patch-elements))

(module+ test
  (require rackunit
           (only-in (submod "sse.rkt" internal) make-test-sse get-test-output))

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
                                 "\n"))))
