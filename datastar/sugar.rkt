#lang racket/base

(provide sse-get
         sse-post
         sse-put
         sse-patch
         sse-delete)

(define (sse-action method url args)
  (if args
      (format "@~a('~a', ~a)" method url args)
      (format "@~a('~a')" method url)))

(define (sse-get url [args #f])
  (sse-action "get" url args))
(define (sse-post url [args #f])
  (sse-action "post" url args))
(define (sse-put url [args #f])
  (sse-action "put" url args))
(define (sse-patch url [args #f])
  (sse-action "patch" url args))
(define (sse-delete url [args #f])
  (sse-action "delete" url args))

(module+ test
  (require rackunit)

  (test-case "sse-get basic"
    (check-equal? (sse-get "/events") "@get('/events')"))

  (test-case "sse-post basic"
    (check-equal? (sse-post "/submit") "@post('/submit')"))

  (test-case "sse-put with args"
    (check-equal? (sse-put "/update" "{includeLocal: true}") "@put('/update', {includeLocal: true})"))

  (test-case "sse-patch basic"
    (check-equal? (sse-patch "/data") "@patch('/data')"))

  (test-case "sse-delete basic"
    (check-equal? (sse-delete "/item") "@delete('/item')"))

  (test-case "sse-post with formatted url"
    (check-equal? (sse-post (format "/todo/delete/~a" 42)) "@post('/todo/delete/42')")))
