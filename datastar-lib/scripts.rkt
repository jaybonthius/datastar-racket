#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         "constants.rkt"
         (submod "elements.rkt" internal)
         "private/utils.rkt"
         "sse.rkt"
         (only-in (submod "sse.rkt" internal) sse-send))

(provide (contract-out [execute-script
                        (->* [sse? string?]
                             [#:auto-remove boolean?
                              #:attributes (or/c (hash/c symbol? any/c) (listof string?) #f)
                              #:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             void?)]
                       [redirect (-> sse? string? void?)]
                       [replace-url (-> sse? string? void?)]
                       [console-log (-> sse? string? void?)]
                       [console-error (-> sse? string? void?)]))

(define (escape-js-string str)
  (regexp-replace* #px"[\\\\'\"\n\r\t\v\f\0]"
                   str
                   (lambda (m)
                     (case m
                       [("\\") "\\\\"]
                       [("'") "\\'"]
                       [("\"") "\\\""]
                       [("\n") "\\n"]
                       [("\r") "\\r"]
                       [("\t") "\\t"]
                       [("\v") "\\v"]
                       [("\f") "\\f"]
                       [("\0") "\\0"]))))

(define (execute-script gen
                        script
                        #:auto-remove [auto-remove #t]
                        #:attributes [attributes #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-execute-script script
                                  #:auto-remove auto-remove
                                  #:attributes attributes
                                  #:event-id event-id
                                  #:retry-duration retry-duration)))

(define (redirect gen location)
  (execute-script gen
                  (format "setTimeout(() => window.location = '~a')" (escape-js-string location))))

(define (replace-url gen location)
  (execute-script gen
                  (format "window.history.replaceState({}, '', '~a')" (escape-js-string location))))

(define (console-log gen message)
  (execute-script gen (format "console.log('~a')" (escape-js-string message))))

(define (console-error gen message)
  (execute-script gen (format "console.error('~a')" (escape-js-string message))))

(define (build-execute-script script
                              #:auto-remove [auto-remove #t]
                              #:attributes [attributes #f]
                              #:event-id [event-id #f]
                              #:retry-duration [retry-duration #f])
  (define attribute-string
    (string-join (filter values
                         (list (and auto-remove "data-effect=\"el.remove()\"")
                               (match attributes
                                 [(? hash?)
                                  (string-join (for/list ([(k v) (in-hash attributes)])
                                                 (format "~a=\"~a\"" k (escape (format "~a" v))))
                                               " ")]
                                 [(? list?) (string-join attributes " ")]
                                 [#f #f])))
                 " "))

  (define script-tag
    (format "<script~a>~a</script>"
            (if (string=? attribute-string "")
                ""
                (string-append " " attribute-string))
            script))

  (build-patch-elements script-tag
                        #:mode patch-mode-append
                        #:selector "body"
                        #:event-id event-id
                        #:retry-duration retry-duration))
