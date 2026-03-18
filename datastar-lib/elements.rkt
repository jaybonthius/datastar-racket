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
