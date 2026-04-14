#lang racket/base

(require "sugar/actions.rkt"
         "sugar/attributes.rkt")

(provide (all-from-out "sugar/actions.rkt"
                       "sugar/attributes.rkt")
         datastar-version
         datastar-cdn-url
         datastar-cdn-map-url)

(define datastar-version "1.0.0-RC.8")

(define datastar-cdn-url
  (string-append "https://cdn.jsdelivr.net/gh/starfederation/datastar@"
                 datastar-version
                 "/bundles/datastar.js"))

(define datastar-cdn-map-url
  (string-append "https://cdn.jsdelivr.net/gh/starfederation/datastar@"
                 datastar-version
                 "/bundles/datastar.js.map"))
