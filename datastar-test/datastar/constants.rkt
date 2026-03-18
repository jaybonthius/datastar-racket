#lang racket/base

(require datastar/constants
         racket/string
         rackunit)

(provide constants-tests)

(define constants-tests
  (test-suite "constants"

    (test-case "CDN URLs contain version"
      (check-true (string-contains? datastar-cdn-url datastar-version))
      (check-true (string-contains? datastar-cdn-map-url datastar-version)))

    (test-case "CDN URLs have correct suffix"
      (check-true (string-suffix? datastar-cdn-url "/bundles/datastar.js"))
      (check-true (string-suffix? datastar-cdn-map-url "/bundles/datastar.js.map")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests constants-tests))
