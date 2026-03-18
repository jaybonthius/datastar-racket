#lang racket/base

(require datastar
         (only-in (submod datastar/sse internal) make-test-sse get-test-output)
         racket/string
         rackunit)

(provide scripts-tests)

(define scripts-tests
  (test-suite "scripts"

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
      (check-true (string-contains? result "console.error('line1\\nline2\\\\end')")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests scripts-tests))
