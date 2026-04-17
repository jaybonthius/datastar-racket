#lang racket/base

(require xml)

(provide xexpr-or-xexprs->html)

(define (xexpr-or-xexprs->html xexpr-or-xexprs)
  (define xexprs
    (if (xexpr? xexpr-or-xexprs)
        (list xexpr-or-xexprs)
        xexpr-or-xexprs))
  (apply string-append (map xexpr->string xexprs)))
