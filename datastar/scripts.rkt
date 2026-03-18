#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         (submod "elements.rkt" internal)
         "constants.rkt"
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
    (check-false (string-contains? result "data-effect")))

  (test-case "console-error wraps in console.error"
    (define-values (gen out) (make-test-sse))
    (console-error gen "something broke")
    (define result (get-test-output out))
    (check-true (string-contains? result "console.error('something broke')")))

  (test-case "replace-url wraps in history.replaceState"
    (define-values (gen out) (make-test-sse))
    (replace-url gen "/new/path?q=foo")
    (define result (get-test-output out))
    (check-true (string-contains? result "window.history.replaceState({}, '', '/new/path?q=foo')")))

  (test-case "console-log escapes single quotes in message"
    (define-values (gen out) (make-test-sse))
    (console-log gen "it's a test")
    (define result (get-test-output out))
    (check-true (string-contains? result "console.log('it\\'s a test')")))

  (test-case "redirect escapes single quotes in URL"
    (define-values (gen out) (make-test-sse))
    (redirect gen "/path?x='foo'")
    (define result (get-test-output out))
    (check-true (string-contains? result "window.location = '/path?x=\\'foo\\''")))

  (test-case "console-error escapes backslashes and newlines"
    (define-values (gen out) (make-test-sse))
    (console-error gen "line1\nline2\\end")
    (define result (get-test-output out))
    (check-true (string-contains? result "console.error('line1\\nline2\\\\end')"))))
