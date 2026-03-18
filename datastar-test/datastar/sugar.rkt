#lang racket/base

(require datastar/sugar
         rackunit)

(provide sugar-tests)

(define sugar-tests
  (test-suite "sugar"

    (test-case "sse-get basic"
      (check-equal? (sse-get "/events") "@get('/events')"))

    (test-case "sse-post basic"
      (check-equal? (sse-post "/submit") "@post('/submit')"))

    (test-case "sse-put with args"
      (check-equal? (sse-put "/update" "{includeLocal: true}")
                    "@put('/update', {includeLocal: true})"))

    (test-case "sse-patch basic"
      (check-equal? (sse-patch "/data") "@patch('/data')"))

    (test-case "sse-delete basic"
      (check-equal? (sse-delete "/item") "@delete('/item')"))

    (test-case "sse-post with formatted url"
      (check-equal? (sse-post (format "/todo/delete/~a" 42)) "@post('/todo/delete/42')"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sugar-tests))
