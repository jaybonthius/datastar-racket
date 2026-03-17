#lang racket/base

(provide (all-defined-out))

(define datastar-key 'datastar)
(define datastar-version "1.0.0-RC.8")

(define sse-headers
  (hash "Cache-Control"
        "no-cache"
        "Content-Type"
        "text/event-stream"
        "Connection"
        "keep-alive"
        "X-Accel-Buffering"
        "no"))

;; Element patch modes
(define patch-mode-outer 'outer)
(define patch-mode-inner 'inner)
(define patch-mode-remove 'remove)
(define patch-mode-replace 'replace)
(define patch-mode-prepend 'prepend)
(define patch-mode-append 'append)
(define patch-mode-before 'before)
(define patch-mode-after 'after)

(define default-element-patch-mode patch-mode-outer)

;; Element namespaces
(define element-namespace-html 'html)
(define element-namespace-svg 'svg)
(define element-namespace-mathml 'mathml)

(define default-element-namespace element-namespace-html)

;; Event types
(define event-type-patch-elements 'datastar-patch-elements)
(define event-type-patch-signals 'datastar-patch-signals)

;; The default duration for retrying SSE on connection reset.
(define default-sse-retry-duration 1000)

;; Dataline literals
(define selector-dataline-literal 'selector)
(define mode-dataline-literal 'mode)
(define namespace-dataline-literal 'namespace)
(define elements-dataline-literal 'elements)
(define use-view-transition-dataline-literal 'useViewTransition)
(define signals-dataline-literal 'signals)
(define only-if-missing-dataline-literal 'onlyIfMissing)

;; Should elements be patched using the ViewTransition API?
(define default-elements-use-view-transitions #f)
;; Should a given set of signals patch if they are missing?
(define default-patch-signals-only-if-missing #f)
