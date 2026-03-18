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
