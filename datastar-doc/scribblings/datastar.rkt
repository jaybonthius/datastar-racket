#lang racket/base

(require scribble/example
         scribble/manual)

(provide (all-from-out scribble/example scribble/manual)
         ev)

(define ev (make-base-eval))
(ev '(require datastar
              datastar/testing
              racket/list))
