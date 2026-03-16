#lang racket/base

(require (submod datastar/sse internal)
         libbrotli
         racket/contract/base)

(provide (contract-out
          [make-brotli-write-profile
           (->* [] [#:quality quality/c #:window window/c #:mode mode/c] write-profile?)])
         BROTLI_MODE_GENERIC
         BROTLI_MODE_TEXT
         BROTLI_MODE_FONT)

(define BROTLI-CONTENT-ENCODING "br")

(define (make-brotli-write-profile #:quality [quality 5]
                                   #:window [window 22]
                                   #:mode [mode BROTLI_MODE_TEXT])
  (make-write-profile
   (lambda (raw-out)
     (open-brotli-output raw-out #:quality quality #:window window #:mode mode #:close? #f))
   (lambda (wrapped raw)
     (flush-output wrapped)
     (flush-output raw))
   BROTLI-CONTENT-ENCODING))
