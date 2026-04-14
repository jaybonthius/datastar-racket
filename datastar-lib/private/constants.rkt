#lang racket/base

(provide datastar-key
         sse-headers
         default-element-patch-mode
         default-element-namespace
         event-type-patch-elements
         event-type-patch-signals
         default-sse-retry-duration
         selector-dataline-literal
         mode-dataline-literal
         namespace-dataline-literal
         elements-dataline-literal
         use-view-transition-dataline-literal
         signals-dataline-literal
         only-if-missing-dataline-literal
         default-elements-use-view-transitions
         default-patch-signals-only-if-missing)

(define datastar-key 'datastar)

(define sse-headers
  (hash "Cache-Control" "no-cache" "Connection" "keep-alive" "X-Accel-Buffering" "no"))

(define default-element-patch-mode 'outer)
(define default-element-namespace 'html)

(define event-type-patch-elements 'datastar-patch-elements)
(define event-type-patch-signals 'datastar-patch-signals)

(define default-sse-retry-duration 1000)

(define selector-dataline-literal 'selector)
(define mode-dataline-literal 'mode)
(define namespace-dataline-literal 'namespace)
(define elements-dataline-literal 'elements)
(define use-view-transition-dataline-literal 'useViewTransition)
(define signals-dataline-literal 'signals)
(define only-if-missing-dataline-literal 'onlyIfMissing)

(define default-elements-use-view-transitions #f)
(define default-patch-signals-only-if-missing #f)
