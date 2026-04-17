#lang racket/base

(require rackunit
         rackunit/text-ui
         "request.rkt"
         "response.rkt"
         "sugar.rkt"
         "testing.rkt")

(provide main-tests)

(define main-tests
  (test-suite "datastar"
    request-tests
    response-tests
    sugar-tests
    testing-tests))

(module+ test
  (dynamic-require "integration.rkt" #f)
  (run-tests main-tests))
