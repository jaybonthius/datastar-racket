#lang racket/base

(provide datastar-version
         datastar-cdn-url
         datastar-cdn-map-url
         patch-mode-outer
         patch-mode-inner
         patch-mode-remove
         patch-mode-replace
         patch-mode-prepend
         patch-mode-append
         patch-mode-before
         patch-mode-after
         element-namespace-html
         element-namespace-svg
         element-namespace-mathml)

(define datastar-version "1.0.0-RC.8")

(define datastar-cdn-url
  (string-append "https://cdn.jsdelivr.net/gh/starfederation/datastar@"
                 datastar-version
                 "/bundles/datastar.js"))

(define datastar-cdn-map-url
  (string-append "https://cdn.jsdelivr.net/gh/starfederation/datastar@"
                 datastar-version
                 "/bundles/datastar.js.map"))

(define patch-mode-outer 'outer)
(define patch-mode-inner 'inner)
(define patch-mode-remove 'remove)
(define patch-mode-replace 'replace)
(define patch-mode-prepend 'prepend)
(define patch-mode-append 'append)
(define patch-mode-before 'before)
(define patch-mode-after 'after)

(define element-namespace-html 'html)
(define element-namespace-svg 'svg)
(define element-namespace-mathml 'mathml)

(module+ internal
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

  (define default-element-patch-mode patch-mode-outer)
  (define default-element-namespace element-namespace-html)

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
  (define default-patch-signals-only-if-missing #f))

(module+ test
  (require racket/string
           rackunit)

  (test-case "CDN URLs contain version"
    (check-true (string-contains? datastar-cdn-url datastar-version))
    (check-true (string-contains? datastar-cdn-map-url datastar-version)))

  (test-case "CDN URLs have correct suffix"
    (check-true (string-suffix? datastar-cdn-url "/bundles/datastar.js"))
    (check-true (string-suffix? datastar-cdn-map-url "/bundles/datastar.js.map"))))
