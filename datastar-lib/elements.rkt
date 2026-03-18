#lang racket/base

(require racket/contract/base
         "constants.rkt"
         "private/elements.rkt"
         "private/sse.rkt"
         "private/utils.rkt")

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
