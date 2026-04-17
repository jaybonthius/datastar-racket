#lang racket/base

(require json
         racket/contract/base
         racket/match
         racket/string
         web-server/http/request-structs
         "../private/constants.rkt")

(provide (contract-out [read-signals (-> request? jsexpr?)]
                       [datastar-request? (-> request? boolean?)]))

(define (read-signals request)
  (match (request-method request)
    [(or #"GET" #"DELETE")
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
