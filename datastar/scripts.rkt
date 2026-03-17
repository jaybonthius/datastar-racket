#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         (submod "elements.rkt" internal)
         "private/utils.rkt"
         "sse.rkt")

(provide (contract-out [execute-script
                        (->* [sse? string?]
                             [#:auto-remove boolean?
                              #:attributes (or/c (hash/c symbol? any/c) (listof string?) #f)
                              #:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             void?)]
                       [redirect (-> sse? string? void?)]
                       [console-log (-> sse? string? void?)]))

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
  (execute-script gen (format "setTimeout(() => window.location = '~a')" location))
  (void))

(define (console-log gen message)
  (execute-script gen (format "console.log('~a')" message))
  (void))

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
                        #:mode "append"
                        #:selector "body"
                        #:event-id event-id
                        #:retry-duration retry-duration))

(module+ test
  (require rackunit
           (only-in (submod "sse.rkt" internal) make-test-sse get-test-output))

  (test-case "execute-script wraps in script tag with auto-remove"
    (define-values (gen out) (make-test-sse))
    (execute-script gen "console.log('hello')")
    (define result (get-test-output out))
    (check-true (string-contains? result "data-effect=\"el.remove()\""))
    (check-true (string-contains? result "console.log('hello')"))
    (check-true (string-contains? result "<script"))
    (check-true (string-contains? result "data: mode append"))
    (check-true (string-contains? result "data: selector body")))

  (test-case "execute-script with custom attributes (hash)"
    (define-values (gen out) (make-test-sse))
    (execute-script gen "doStuff()" #:attributes (hash 'type "module"))
    (define result (get-test-output out))
    (check-true (string-contains? result "type=\"module\"")))

  (test-case "execute-script without auto-remove"
    (define-values (gen out) (make-test-sse))
    (execute-script gen "persist()" #:auto-remove #f)
    (define result (get-test-output out))
    (check-false (string-contains? result "data-effect"))))
