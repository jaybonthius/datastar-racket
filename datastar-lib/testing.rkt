#lang racket/base

(require racket/contract/base
         racket/list
         racket/match
         racket/string
         "private/sse.rkt")

;; sse-event struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct sse-event (type id retry data-lines) #:transparent)

;; SSE text parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-sse-events raw)
  (cond
    [(string=? raw "") '()]
    [else
     (define chunks (regexp-split #rx"\n\n" raw))
     (filter-map parse-one-event chunks)]))

(define (parse-one-event chunk)
  (define lines (string-split chunk "\n"))
  (define non-empty (filter (lambda (l) (not (string=? l ""))) lines))
  (cond
    [(null? non-empty) #f]
    [else
     (define type #f)
     (define id #f)
     (define retry #f)
     (define data '())
     (for ([line (in-list non-empty)])
       (match line
         [(regexp #rx"^event: (.+)$" (list _ t)) (set! type t)]
         [(regexp #rx"^id: (.+)$" (list _ i)) (set! id i)]
         [(regexp #rx"^retry: (.+)$" (list _ r)) (set! retry (string->number r))]
         [(regexp #rx"^data: (.*)$" (list _ d)) (set! data (append data (list d)))]
         [_ (void)]))
     (and type (sse-event type id retry data))]))

;; public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-mock-sse)
  (define-values (gen out) (make-test-sse))
  (values gen (lambda () (get-test-output out))))

(define (make-recording-sse)
  (define-values (gen out) (make-test-sse))
  (values gen (lambda () (parse-sse-events (get-test-output out)))))

;; contracts & provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (contract-out (struct sse-event
                               ([type string?] [id (or/c string? #f)]
                                               [retry (or/c exact-nonnegative-integer? #f)]
                                               [data-lines (listof string?)]))
                       [make-mock-sse (-> (values sse? (-> string?)))]
                       [make-recording-sse (-> (values sse? (-> (listof sse-event?))))]))
