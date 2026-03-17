#lang racket/base

(require racket/contract/base
         racket/string
         "constants.rkt"
         "private/utils.rkt"
         "sse.rkt")

(define element-patch-mode/c
  (or/c "outer" "inner" "remove" "replace" "prepend" "append" "before" "after" #f))
(define element-namespace/c (or/c "html" "svg" "mathml" #f))

(provide (contract-out [patch-elements
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
                  #:mode "remove"
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
    (append (filter values
                    (list (and mode
                               (not (string=? mode DEFAULT-ELEMENT-PATCH-MODE))
                               (string-append MODE-DATALINE-LITERAL " " mode))
                          (and selector (string-append SELECTOR-DATALINE-LITERAL " " selector))
                          (and namespace
                               (not (string=? namespace DEFAULT-ELEMENT-NAMESPACE))
                               (string-append NAMESPACE-DATALINE-LITERAL " " namespace))
                          (and use-view-transitions
                               (not (eq? use-view-transitions DEFAULT-ELEMENTS-USE-VIEW-TRANSITIONS))
                               (string-append USE-VIEW-TRANSITION-DATALINE-LITERAL
                                              " "
                                              (js-bool use-view-transitions)))))
            (if elements
                (map (lambda (line) (string-append ELEMENTS-DATALINE-LITERAL " " line))
                     (string-split elements "\n"))
                '())))

  (send-event EVENT-TYPE-PATCH-ELEMENTS
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
                    #:mode "inner"
                    #:namespace "svg"
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
    (patch-elements gen "<div>test</div>" #:mode "outer")
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
