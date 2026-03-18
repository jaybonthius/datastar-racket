#lang racket/base

(require json
         racket/contract/base
         racket/match
         racket/string
         web-server/http/request-structs
         "constants.rkt"
         (submod "constants.rkt" internal)
         "private/utils.rkt"
         "sse.rkt"
         (only-in (submod "sse.rkt" internal) sse-send))

(provide (contract-out [patch-signals
                        (->* [sse? (or/c string? jsexpr?)]
                             [#:event-id (or/c string? #f)
                              #:only-if-missing (or/c boolean? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             void?)]
                       [read-signals (-> request? jsexpr?)]
                       [datastar-request? (-> request? boolean?)]))

(define (patch-signals gen
                       signals
                       #:event-id [event-id #f]
                       #:only-if-missing [only-if-missing #f]
                       #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-patch-signals signals
                                 #:event-id event-id
                                 #:only-if-missing only-if-missing
                                 #:retry-duration retry-duration)))

(define (read-signals request)
  (match (request-method request)
    [#"GET"
     (define datastar-binding
       (findf (lambda (binding)
                (equal? (bytes->string/utf-8 (binding-id binding)) (symbol->string datastar-key)))
              (request-bindings/raw request)))
     (unless datastar-binding
       (error "No datastar parameter found in request bindings"))
     (string->jsexpr (bytes->string/utf-8 (binding:form-value datastar-binding)))]
    [_
     (define body (request-post-data/raw request))
     (unless body
       (error "No request body found"))
     (string->jsexpr (bytes->string/utf-8 body))]))

(define (datastar-request? request)
  (and (for/or ([h (in-list (request-headers/raw request))])
         (and (equal? (string-downcase (bytes->string/utf-8 (header-field h))) "datastar-request")
              (equal? (string-downcase (bytes->string/utf-8 (header-value h))) "true")))
       #t))

(define (build-patch-signals signals
                             #:event-id [event-id #f]
                             #:only-if-missing [only-if-missing #f]
                             #:retry-duration [retry-duration #f])
  (define signals-str
    (if (string? signals)
        signals
        (jsexpr->string signals)))

  (define data-lines
    (append (filter values
                    (list (and only-if-missing
                               (not (eq? only-if-missing default-patch-signals-only-if-missing))
                               (string-append (symbol->string only-if-missing-dataline-literal)
                                              " "
                                              (js-bool only-if-missing)))))
            (map (lambda (line) (string-append (symbol->string signals-dataline-literal) " " line))
                 (string-split signals-str "\n"))))

  (send-event event-type-patch-signals
              data-lines
              #:event-id event-id
              #:retry-duration retry-duration))
