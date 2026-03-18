#lang racket/base

(require racket/string
         "constants.rkt")

(provide js-bool
         escape
         send-event)

(define (js-bool b)
  (if b "true" "false"))

(define (escape str)
  (regexp-replace* #px"[&'\"<>]"
                   str
                   (lambda (m)
                     (case m
                       [("&") "&amp;"]
                       [("'") "&#39;"]
                       [("\"") "&#34;"]
                       [(">") "&gt;"]
                       [("<") "&lt;"]))))

(define (send-event event-type
                    data-lines
                    #:event-id [event-id #f]
                    #:retry-duration [retry-duration #f])
  (define prefix-lines
    (filter values
            (list (string-append "event: " (symbol->string event-type))
                  (and event-id (string-append "id: " event-id))
                  (and retry-duration
                       (not (= retry-duration default-sse-retry-duration))
                       (string-append "retry: " (number->string retry-duration))))))

  (define formatted-data-lines (map (lambda (line) (string-append "data: " line)) data-lines))

  (string-join (append prefix-lines formatted-data-lines) "\n" #:after-last "\n\n"))
