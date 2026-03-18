#lang racket/base

(require racket/string
         "constants.rkt"
         "utils.rkt")

(provide build-patch-elements)

(define (build-patch-elements elements
                              #:selector [selector #f]
                              #:mode [mode #f]
                              #:namespace [namespace #f]
                              #:use-view-transitions [use-view-transitions #f]
                              #:event-id [event-id #f]
                              #:retry-duration [retry-duration #f])
  (define data-lines
    (append
     (filter
      values
      (list (and mode
                 (not (eq? mode default-element-patch-mode))
                 (string-append (symbol->string mode-dataline-literal) " " (symbol->string mode)))
            (and selector (string-append (symbol->string selector-dataline-literal) " " selector))
            (and namespace
                 (not (eq? namespace default-element-namespace))
                 (string-append (symbol->string namespace-dataline-literal)
                                " "
                                (symbol->string namespace)))
            (and use-view-transitions
                 (not (eq? use-view-transitions default-elements-use-view-transitions))
                 (string-append (symbol->string use-view-transition-dataline-literal)
                                " "
                                (js-bool use-view-transitions)))))
     (if elements
         (map (lambda (line) (string-append (symbol->string elements-dataline-literal) " " line))
              (string-split elements "\n"))
         '())))

  (send-event event-type-patch-elements
              data-lines
              #:event-id event-id
              #:retry-duration retry-duration))
