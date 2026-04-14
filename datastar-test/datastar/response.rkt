#lang racket/base

(require datastar/http/response
         (only-in datastar/private/sse make-test-sse get-test-output)
         json
         racket/string
         rackunit
         web-server/http
         xml)

(provide response-tests)

(define (response-body-bytes resp)
  (define out (open-output-bytes))
  ((response-output resp) out)
  (get-output-bytes out))

(define (response-body-string resp)
  (bytes->string/utf-8 (response-body-bytes resp)))

(define (find-header resp name)
  (for/or ([h (in-list (response-headers resp))])
    (and (string-ci=? (bytes->string/utf-8 (header-field h)) name)
         (bytes->string/utf-8 (header-value h)))))

(define responses-tests
  (test-suite "responses"

    (test-case "response/datastar-elements basic"
      (define resp (response/datastar-elements "<div id=\"target\">hi</div>"))
      (check-equal? (response-code resp) 200)
      (check-equal? (bytes->string/utf-8 (response-mime resp)) "text/html")
      (check-equal? (response-body-string resp) "<div id=\"target\">hi</div>")
      (check-false (find-header resp "datastar-selector"))
      (check-false (find-header resp "datastar-mode"))
      (check-false (find-header resp "datastar-namespace"))
      (check-false (find-header resp "datastar-use-view-transition")))

    (test-case "response/datastar-elements with options"
      (define resp
        (response/datastar-elements "<div>content</div>"
                                    #:selector "#target"
                                    #:mode 'inner
                                    #:namespace 'svg
                                    #:use-view-transitions? #t
                                    #:status 202
                                    #:headers (hash "X-Test" "yes")))
      (check-equal? (response-code resp) 202)
      (check-equal? (find-header resp "datastar-selector") "#target")
      (check-equal? (find-header resp "datastar-mode") "inner")
      (check-equal? (find-header resp "datastar-namespace") "svg")
      (check-equal? (find-header resp "datastar-use-view-transition") "true")
      (check-equal? (find-header resp "x-test") "yes"))

    (test-case "response/datastar-elements/xexprs single"
      (define resp (response/datastar-elements/xexprs '(div ((id "target")) "hello")))
      (check-equal? (bytes->string/utf-8 (response-mime resp)) "text/html")
      (check-equal? (response-body-string resp) "<div id=\"target\">hello</div>"))

    (test-case "response/datastar-elements/xexprs multiple"
      (define resp
        (response/datastar-elements/xexprs '((div ((id "a")) "one") (div ((id "b")) "two"))))
      (check-equal? (response-body-string resp) "<div id=\"a\">one</div><div id=\"b\">two</div>"))

    (test-case "response/datastar-signals from jsexpr"
      (define resp (response/datastar-signals (hash 'count 1 'ok #t)))
      (check-equal? (bytes->string/utf-8 (response-mime resp)) "application/json")
      (define body (response-body-string resp))
      (check-true (string-contains? body "\"count\":1"))
      (check-true (string-contains? body "\"ok\":true"))
      (check-false (find-header resp "datastar-only-if-missing")))

    (test-case "response/datastar-signals from JSON string"
      (define resp (response/datastar-signals "{\"msg\":\"hello\"}"))
      (check-equal? (response-body-string resp) "{\"msg\":\"hello\"}"))

    (test-case "response/datastar-signals only-if-missing true"
      (define resp (response/datastar-signals (hash 'x 1) #:only-if-missing? #t))
      (check-equal? (find-header resp "datastar-only-if-missing") "true"))

    (test-case "response/datastar-signals only-if-missing false is omitted"
      (define resp (response/datastar-signals (hash 'x 1) #:only-if-missing? #f))
      (check-false (find-header resp "datastar-only-if-missing")))

    (test-case "response/datastar-script basic"
      (define resp (response/datastar-script "console.log('hello')"))
      (check-equal? (bytes->string/utf-8 (response-mime resp)) "text/javascript")
      (check-equal? (response-body-string resp) "console.log('hello')")
      (check-false (find-header resp "datastar-script-attributes")))

    (test-case "response/datastar-script with attributes"
      (define resp
        (response/datastar-script
         "console.log('hi')"
         #:attributes (hash 'type "module" "nonce" "abc" 'async #t 'defer #f 'priority 5)))
      (define attrs-header (find-header resp "datastar-script-attributes"))
      (check-not-false attrs-header)
      (define attrs-json (string->jsexpr attrs-header))
      (check-equal? (hash-ref attrs-json 'type) "module")
      (check-equal? (hash-ref attrs-json 'nonce) "abc")
      (check-equal? (hash-ref attrs-json 'async) #t)
      (check-equal? (hash-ref attrs-json 'defer) #f)
      (check-equal? (hash-ref attrs-json 'priority) 5))

    (test-case "response helpers enforce contracts"
      (check-exn exn:fail:contract? (lambda () (response/datastar-elements "<div/>" #:mode 'invalid)))
      (check-exn exn:fail:contract?
                 (lambda () (response/datastar-elements "<div/>" #:namespace 'invalid)))
      (check-exn exn:fail:contract? (lambda () (response/datastar-elements "<div/>" #:status 0)))
      (check-exn exn:fail:contract? (lambda () (response/datastar-elements "<div/>" #:headers '())))
      (check-exn exn:fail:contract?
                 (lambda () (response/datastar-script "x" #:attributes (hash 1 "v")))))))

(define elements-tests
  (test-suite "elements"

    (test-case "patch-elements produces correct SSE format"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div id=\"out\">hi</div>")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: elements <div id=\"out\">hi</div>\n"
                                   "\n")))

    (test-case "patch-elements with all options"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen
                      "<div>content</div>"
                      #:selector "#target"
                      #:mode 'inner
                      #:namespace 'svg
                      #:use-view-transitions? #t
                      #:event-id "evt-1"
                      #:retry-duration 2000)
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "id: evt-1\n"
                                   "retry: 2000\n"
                                   "data: mode inner\n"
                                   "data: selector #target\n"
                                   "data: namespace svg\n"
                                   "data: useViewTransition true\n"
                                   "data: elements <div>content</div>\n"
                                   "\n")))

    (test-case "default mode outer is omitted"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>test</div>" #:mode 'outer)
      (define result (get-test-output out))
      (check-false (string-contains? result "mode")))

    (test-case "default retry duration 1000 is omitted"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>test</div>" #:retry-duration 1000)
      (define result (get-test-output out))
      (check-false (string-contains? result "retry")))

    (test-case "non-default retry duration is included"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>test</div>" #:retry-duration 3000)
      (define result (get-test-output out))
      (check-true (string-contains? result "retry: 3000")))

    (test-case "multi-line elements get separate data lines"
      (define-values (gen out) (make-test-sse))
      (patch-elements gen "<div>\n  hello\n</div>")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: elements <div>\n"
                                   "data: elements   hello\n"
                                   "data: elements </div>\n"
                                   "\n")))

    (test-case "remove-elements produces correct format"
      (define-values (gen out) (make-test-sse))
      (remove-elements gen "#gone")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: mode remove\n"
                                   "data: selector #gone\n"
                                   "\n")))

    (test-case "patch-elements/xexprs produces same output as patch-elements"
      (define-values (gen1 out1) (make-test-sse))
      (define-values (gen2 out2) (make-test-sse))
      (patch-elements gen1 (xexpr->string '(div ((id "out")) "hi")))
      (patch-elements/xexprs gen2 '(div ((id "out")) "hi"))
      (check-equal? (get-test-output out1) (get-test-output out2)))

    (test-case "patch-elements/xexprs with all options"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexprs gen
                             '(div "content")
                             #:selector "#target"
                             #:mode 'inner
                             #:namespace 'svg
                             #:use-view-transitions? #t
                             #:event-id "evt-1"
                             #:retry-duration 2000)
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "id: evt-1\n"
                                   "retry: 2000\n"
                                   "data: mode inner\n"
                                   "data: selector #target\n"
                                   "data: namespace svg\n"
                                   "data: useViewTransition true\n"
                                   "data: elements <div>content</div>\n"
                                   "\n")))

    (test-case "patch-elements/xexprs with nested xexpr"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexprs gen '(div ((id "x")) (span "hello") " " (span "world")))
      (define result (get-test-output out))
      (check-true (string-contains?
                   result
                   "elements <div id=\"x\"><span>hello</span> <span>world</span></div>")))

    (test-case "patch-elements/xexprs with multiple xexprs"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexprs gen '((div ((id "a")) "one") (div ((id "b")) "two")))
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: elements <div id=\"a\">one</div><div id=\"b\">two</div>\n"
                                   "\n")))

    (test-case "patch-elements/xexprs with empty list emits empty patch event"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexprs gen '())
      (check-equal? (get-test-output out) (string-append "event: datastar-patch-elements\n" "\n")))

    (test-case "patch-elements/xexprs empty list with selector emits selector-only event"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexprs gen '() #:selector "#x")
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n" "data: selector #x\n" "\n")))

    (test-case "patch-elements/xexprs empty list with remove mode emits remove event"
      (define-values (gen out) (make-test-sse))
      (patch-elements/xexprs gen '() #:selector "#x" #:mode 'remove)
      (check-equal? (get-test-output out)
                    (string-append "event: datastar-patch-elements\n"
                                   "data: mode remove\n"
                                   "data: selector #x\n"
                                   "\n")))))

(define signals-tests
  (test-suite "signals"

    (test-case "patch-signals with hash produces correct format"
      (define-values (gen out) (make-test-sse))
      (patch-signals gen (hash 'count 1))
      (define result (get-test-output out))
      (check-true (string-contains? result "event: datastar-patch-signals"))
      (check-true (string-contains? result "data: signals {")))

    (test-case "patch-signals with only-if-missing"
      (define-values (gen out) (make-test-sse))
      (patch-signals gen (hash 'x 1) #:only-if-missing? #t)
      (define result (get-test-output out))
      (check-true (string-contains? result "data: onlyIfMissing true")))

    (test-case "remove-signals with single path produces null patch"
      (define-values (gen out) (make-test-sse))
      (remove-signals gen "count")
      (define result (get-test-output out))
      (check-true (string-contains? result "event: datastar-patch-signals"))
      (check-true (string-contains? result "data: signals {\"count\":null}")))

    (test-case "remove-signals with dotted path produces nested null patch"
      (define-values (gen out) (make-test-sse))
      (remove-signals gen "user.name")
      (define result (get-test-output out))
      (check-true (string-contains? result "\"user\":{\"name\":null}"))
      (check-false (string-contains? result "\"user.name\":null")))

    (test-case "remove-signals with multiple paths supports SSE options"
      (define-values (gen out) (make-test-sse))
      (remove-signals gen '("user.name" "active") #:event-id "evt-1" #:retry-duration 2000)
      (define result (get-test-output out))
      (check-true (string-contains? result "id: evt-1"))
      (check-true (string-contains? result "retry: 2000"))
      (check-true (string-contains? result "\"user\":{\"name\":null}"))
      (check-true (string-contains? result "\"active\":null")))

    (test-case "remove-signals parent path takes precedence over child paths"
      (define-values (gen out) (make-test-sse))
      (remove-signals gen '("user" "user.name"))
      (define result (get-test-output out))
      (check-true (string-contains? result "\"user\":null"))
      (check-false (string-contains? result "\"user\":{\"name\":null}")))))

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
      (execute-script gen "persist()" #:auto-remove? #f)
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

(define sse-tests
  (test-suite "sse"

    (test-case "send succeeds on open connection"
      (define-values (gen _out) (make-test-sse))
      (check-not-exn (lambda () (patch-elements gen "<div>hello</div>"))))

    (test-case "send raises after port is closed"
      (define-values (gen out) (make-test-sse))
      (close-output-port out)
      (check-exn exn:fail? (lambda () (patch-elements gen "<div>hello</div>"))))

    (test-case "send raises after close-sse"
      (define-values (gen _out) (make-test-sse))
      (close-sse gen)
      (check-exn exn:fail? (lambda () (patch-elements gen "<div>hello</div>"))))

    (test-case "close-sse is idempotent"
      (define-values (gen _out) (make-test-sse))
      (close-sse gen)
      (check-not-exn (lambda () (close-sse gen))))

    (test-case "sse-closed? returns #f on fresh connection"
      (define-values (gen _out) (make-test-sse))
      (check-false (sse-closed? gen)))

    (test-case "sse-closed? returns #t after close-sse"
      (define-values (gen _out) (make-test-sse))
      (close-sse gen)
      (check-true (sse-closed? gen)))

    (test-case "sse-closed? returns #t when output port closed"
      (define-values (gen out) (make-test-sse))
      (close-output-port out)
      (check-true (sse-closed? gen)))

    (test-case "concurrent sends do not interleave"
      (define-values (gen out) (make-test-sse))
      (define threads
        (for/list ([i (in-range 10)])
          (thread (lambda () (patch-elements gen (format "<div>thread ~a</div>" i))))))
      (for-each thread-wait threads)
      (define result (get-output-string out))
      (define events (regexp-match* #rx"event: datastar-patch-elements\n" result))
      (check-equal? (length events) 10))

    (test-case "with-sse-lock: sse-send works inside lock (reentrant)"
      (define-values (gen out) (make-test-sse))
      (check-not-exn (lambda ()
                       (with-sse-lock gen
                                      (patch-elements gen "<div>first</div>")
                                      (patch-elements gen "<div>second</div>"))))
      (define result (get-output-string out))
      (check-true (string-contains? result "first"))
      (check-true (string-contains? result "second")))

    (test-case "without with-sse-lock: sends from other threads CAN interleave"
      ;; Without batch locking, a sleep between two sends lets another
      ;; thread's event slip in between.
      (define-values (gen out) (make-test-sse))
      (define barrier (make-semaphore 0))
      (define threads
        (for/list ([i (in-range 5)])
          (thread (lambda ()
                    (semaphore-wait barrier)
                    (patch-elements gen (format "<div>start-~a</div>" i))
                    (sleep 0.01)
                    (patch-elements gen (format "<div>end-~a</div>" i))))))
      (for ([_ (in-range 5)])
        (semaphore-post barrier))
      (for-each thread-wait threads)
      (define result (get-output-string out))
      (define data-values
        (regexp-match* #rx"data: elements <div>([a-z]+-[0-9])" result #:match-select cadr))
      (define adjacent-pairs
        (for/sum
         ([i (in-range (sub1 (length data-values)))])
         (define this (list-ref data-values i))
         (define next (list-ref data-values (add1 i)))
         (define num (substring this (sub1 (string-length this))))
         (if (and (string-prefix? this "start-") (equal? next (string-append "end-" num))) 1 0)))
      (check-true (< adjacent-pairs 5) "without batch lock, some pairs should be interleaved"))

    (test-case "with-sse-lock: batch sends are not interleaved by other threads"
      (define-values (gen out) (make-test-sse))
      (define threads
        (for/list ([i (in-range 5)])
          (thread (lambda ()
                    (with-sse-lock gen
                                   (patch-elements gen (format "<div>start-~a</div>" i))
                                   (sleep 0.01)
                                   (patch-elements gen (format "<div>end-~a</div>" i)))))))
      (for-each thread-wait threads)
      (define result (get-output-string out))
      (define pairs (regexp-match* #rx"start-[0-9].*?end-[0-9]" result))
      (check-equal? (length pairs) 5)
      (for ([p (in-list pairs)])
        (define nums (regexp-match* #rx"[0-9]" p))
        (check-equal? (car nums) (cadr nums))))

    (test-case "with-sse-lock: lock released after exception"
      (define-values (gen out) (make-test-sse))
      (with-handlers ([exn:fail? void])
        (with-sse-lock gen (error "boom!")))
      (check-not-exn (lambda () (patch-elements gen "<div>after-error</div>")))
      (check-true (string-contains? (get-output-string out) "after-error")))

    (test-case "call-with-sse-lock: nested locks do not deadlock"
      (define-values (gen out) (make-test-sse))
      (check-not-exn
       (lambda ()
         (call-with-sse-lock
          gen
          (lambda () (call-with-sse-lock gen (lambda () (patch-elements gen "<div>nested</div>")))))))
      (check-true (string-contains? (get-output-string out) "nested")))

    (test-case "with-sse-lock: child thread does not inherit lock"
      (define-values (gen out) (make-test-sse))
      (define result-ch (make-channel))
      (with-sse-lock gen
                     (define _child
                       (thread (lambda ()
                                 (patch-elements gen "<div>child</div>")
                                 (channel-put result-ch 'child-done))))
                     (sleep 0.05)
                     (define got (sync/timeout 0 result-ch))
                     (check-false got "child should be blocked while parent holds lock")
                     (patch-elements gen "<div>parent</div>"))
      (sync/timeout 2 result-ch)
      (define result (get-output-string out))
      (define parent-pos (caar (regexp-match-positions #rx"parent" result)))
      (define child-pos (caar (regexp-match-positions #rx"child" result)))
      (check-true (< parent-pos child-pos) "parent send should come before child send"))))

(define response-tests
  (test-suite "response"
    responses-tests
    elements-tests
    signals-tests
    scripts-tests
    sse-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests response-tests))
