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
