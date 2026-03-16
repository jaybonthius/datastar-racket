#lang racket

;; Unit tests for the Datastar Racket SDK.
;;
;; These test the SSE generator and event formatting without a running server,
;; using string output ports as mock writers — analogous to Go's
;; httptest.NewRecorder and Clojure's ReturnMsgGen pattern.

(require datastar
         (only-in (submod datastar/sse internal) make-sse make-write-profile)
         json
         net/url
         rackunit
         web-server/http/request-structs
         web-server/http/response-structs)

;; ============================================================================
;; Helpers
;; ============================================================================

;; Creates a fresh sse generator backed by an output-string port.
;; Returns (values gen port) so tests can inspect written output.
;; Uses the basic (no compression) flush: just flush the raw port.
(define (make-test-sse)
  (define out (open-output-string))
  (values (make-sse out out (lambda (_wrapped raw) (flush-output raw)) (make-semaphore 1) (box #f))
          out))

;; Reads everything written to the string port so far and resets it.
(define (get-output port)
  (get-output-string port))

;; ============================================================================
;; Send behavior tests
;; ============================================================================

(test-case "send returns #t on open connection"
  ;; Analogous to Go TestSSESendWithActiveConnection
  (define-values (gen out) (make-test-sse))
  (check-true (patch-elements gen "<div id=\"x\">hi</div>")
              "patch-elements should return #t on a live connection"))

(test-case "send returns #f after port is closed"
  ;; Analogous to Go TestSSEContextCancellation (send after cancel)
  (define-values (gen out) (make-test-sse))
  (close-output-port out)
  (check-false (patch-elements gen "<div>test</div>")
               "patch-elements should return #f when output port is closed"))

(test-case "send returns #f after close-sse"
  ;; Analogous to Go TestSSEContextCancellation
  (define-values (gen out) (make-test-sse))
  (close-sse gen)
  (check-false (patch-elements gen "<div>test</div>")
               "patch-elements should return #f after close-sse"))

(test-case "close-sse is idempotent"
  ;; Analogous to Clojure sentinel pattern — calling close twice must not error
  (define-values (gen out) (make-test-sse))
  (close-sse gen)
  (check-not-exn (lambda () (close-sse gen)) "calling close-sse twice should not raise"))

;; ============================================================================
;; Event format tests — patch-elements
;; ============================================================================

(test-case "patch-elements produces correct SSE format"
  ;; Analogous to Clojure api_test.clj basic patch-elements assertion
  (define-values (gen out) (make-test-sse))
  (patch-elements gen "<div id=\"out\">hi</div>")
  (check-equal? (get-output out)
                (string-append "event: datastar-patch-elements\n"
                               "data: elements <div id=\"out\">hi</div>\n"
                               "\n")))

(test-case "patch-elements with all options"
  ;; Analogous to Clojure api_test.clj "handles all options"
  (define-values (gen out) (make-test-sse))
  (patch-elements gen
                  "<div>content</div>"
                  #:selector "#target"
                  #:mode "inner"
                  #:namespace "svg"
                  #:use-view-transitions #t
                  #:event-id "evt-1"
                  #:retry-duration 2000)
  (check-equal? (get-output out)
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
  ;; The outer mode is the default and should not appear in output
  (define-values (gen out) (make-test-sse))
  (patch-elements gen "<div>test</div>" #:mode "outer")
  (define result (get-output out))
  (check-false (string-contains? result "mode")
               "mode line should be omitted when mode is outer (default)"))

(test-case "default retry duration 1000 is omitted"
  ;; Retry duration matching the default (1000ms) should not appear
  (define-values (gen out) (make-test-sse))
  (patch-elements gen "<div>test</div>" #:retry-duration 1000)
  (define result (get-output out))
  (check-false (string-contains? result "retry")
               "retry line should be omitted when duration is the default 1000ms"))

(test-case "non-default retry duration is included"
  (define-values (gen out) (make-test-sse))
  (patch-elements gen "<div>test</div>" #:retry-duration 3000)
  (define result (get-output out))
  (check-true (string-contains? result "retry: 3000")
              "retry line should be present for non-default duration"))

(test-case "multi-line elements get separate data lines"
  ;; Analogous to Clojure api_test.clj div-element multi-line test
  (define-values (gen out) (make-test-sse))
  (patch-elements gen "<div>\n  hello\n</div>")
  (check-equal? (get-output out)
                (string-append "event: datastar-patch-elements\n"
                               "data: elements <div>\n"
                               "data: elements   hello\n"
                               "data: elements </div>\n"
                               "\n")))

;; ============================================================================
;; Event format tests — patch-signals
;; ============================================================================

(test-case "patch-signals with hash produces correct format"
  ;; Analogous to Clojure api_test.clj signal tests
  (define-values (gen out) (make-test-sse))
  (patch-signals gen (hash 'count 1))
  (define result (get-output out))
  (check-true (string-contains? result "event: datastar-patch-signals")
              "should have correct event type")
  (check-true (string-contains? result "data: signals {") "should have signals data line"))

(test-case "patch-signals with only-if-missing"
  (define-values (gen out) (make-test-sse))
  (patch-signals gen (hash 'x 1) #:only-if-missing #t)
  (define result (get-output out))
  (check-true (string-contains? result "data: onlyIfMissing true")
              "onlyIfMissing line should be present when #t"))

;; ============================================================================
;; Event format tests — remove-elements
;; ============================================================================

(test-case "remove-elements produces correct format"
  ;; Analogous to Clojure api_test.clj remove test
  (define-values (gen out) (make-test-sse))
  (remove-elements gen "#gone")
  (check-equal? (get-output out)
                (string-append "event: datastar-patch-elements\n"
                               "data: mode remove\n"
                               "data: selector #gone\n"
                               "\n")))

;; ============================================================================
;; Event format tests — execute-script
;; ============================================================================

(test-case "execute-script wraps in script tag with auto-remove"
  ;; Analogous to Clojure api_test.clj script tests
  (define-values (gen out) (make-test-sse))
  (execute-script gen "console.log('hello')")
  (define result (get-output out))
  (check-true (string-contains? result "data-effect=\"el.remove()\"")
              "should include auto-remove attribute")
  (check-true (string-contains? result "console.log('hello')") "should include the script content")
  (check-true (string-contains? result "<script") "should be wrapped in script tags")
  (check-true (string-contains? result "data: mode append") "scripts should use append mode")
  (check-true (string-contains? result "data: selector body") "scripts should target body"))

(test-case "execute-script with custom attributes (hash)"
  (define-values (gen out) (make-test-sse))
  (execute-script gen "doStuff()" #:attributes (hash 'type "module"))
  (define result (get-output out))
  (check-true (string-contains? result "type=\"module\"") "should include custom attribute"))

(test-case "execute-script without auto-remove"
  (define-values (gen out) (make-test-sse))
  (execute-script gen "persist()" #:auto-remove #f)
  (define result (get-output out))
  (check-false (string-contains? result "data-effect")
               "should not include auto-remove attribute when disabled"))

;; ============================================================================
;; Thread safety test
;; ============================================================================

(test-case "concurrent sends do not interleave"
  ;; Analogous to Clojure RecordMsgGen lock-based concurrency tests
  (define-values (gen out) (make-test-sse))
  (define threads
    (for/list ([i (in-range 10)])
      (thread (lambda () (patch-elements gen (format "<div id=\"t~a\">thread ~a</div>" i i))))))
  ;; Wait for all threads to finish
  (for-each thread-wait threads)
  (define result (get-output out))
  ;; Each event block must be intact — check that we have exactly 10
  ;; complete event blocks (each ending with \n\n)
  (define events (regexp-match* #rx"event: datastar-patch-elements\n" result))
  (check-equal? (length events) 10 "should have exactly 10 complete events")
  ;; Verify no interleaving: each "event:" should be followed by data lines
  ;; then a blank line, never another "event:" before the blank line
  (define blocks (string-split result "\n\n"))
  ;; Last split produces an empty trailing string
  (define non-empty-blocks (filter (lambda (s) (not (string=? s ""))) blocks))
  (check-equal? (length non-empty-blocks)
                10
                "should have 10 non-empty event blocks with no interleaving"))

;; ============================================================================
;; read-signals tests
;; ============================================================================

;; Helper: constructs a mock GET request with a datastar query parameter.
(define (make-get-request signals-json)
  (make-request #"GET"
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                '()
                (delay
                  (list (make-binding:form #"datastar" (string->bytes/utf-8 signals-json))))
                #f
                "127.0.0.1"
                8080
                "127.0.0.1"))

;; Helper: constructs a mock POST request with a JSON body.
(define (make-post-request body-json)
  (make-request #"POST"
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                '()
                (delay
                  '())
                (string->bytes/utf-8 body-json)
                "127.0.0.1"
                8080
                "127.0.0.1"))

(test-case "read-signals parses GET request with datastar query param"
  (define req (make-get-request "{\"count\":42,\"msg\":\"hello\"}"))
  (define signals (read-signals req))
  (check-equal? (hash-ref signals 'count) 42)
  (check-equal? (hash-ref signals 'msg) "hello"))

(test-case "read-signals parses POST request with JSON body"
  (define req (make-post-request "{\"name\":\"test\",\"value\":true}"))
  (define signals (read-signals req))
  (check-equal? (hash-ref signals 'name) "test")
  (check-equal? (hash-ref signals 'value) #t))

(test-case "read-signals parses nested JSON structures"
  (define req (make-post-request "{\"user\":{\"name\":\"jay\",\"age\":30},\"items\":[1,2,3]}"))
  (define signals (read-signals req))
  (check-equal? (hash-ref (hash-ref signals 'user) 'name) "jay")
  (check-equal? (hash-ref signals 'items) '(1 2 3)))

(test-case "read-signals errors on GET without datastar param"
  (define req
    (make-request #"GET"
                  (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                  '()
                  (delay
                    '())
                  #f
                  "127.0.0.1"
                  8080
                  "127.0.0.1"))
  (check-exn exn:fail?
             (lambda () (read-signals req))
             "should raise when datastar binding is missing"))

(test-case "read-signals errors on POST without body"
  (define req
    (make-request #"POST"
                  (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                  '()
                  (delay
                    '())
                  #f
                  "127.0.0.1"
                  8080
                  "127.0.0.1"))
  (check-exn exn:fail? (lambda () (read-signals req)) "should raise when POST body is missing"))

;; ============================================================================
;; Accept-Encoding fallback tests
;; ============================================================================

;; A fake write-profile that claims content-encoding "br" but does no
;; actual compression — just enough to test the negotiation logic.
(define fake-br-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) "br"))

(define (make-request-with-accept-encoding accept-encoding)
  (make-request #"GET"
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                (if accept-encoding
                    (list (make-header #"Accept-Encoding" (string->bytes/utf-8 accept-encoding)))
                    '())
                (delay
                  '())
                #f
                "127.0.0.1"
                8080
                "127.0.0.1"))

(define (response-has-content-encoding? resp encoding)
  (for/or ([h (in-list (response-headers resp))])
    (and (equal? (header-field h) #"Content-Encoding")
         (equal? (header-value h) (string->bytes/utf-8 encoding)))))

(test-case "accept-encoding: brotli used when client accepts br"
  (define req (make-request-with-accept-encoding "gzip, deflate, br"))
  (define resp (datastar-sse req (lambda (sse) (void)) #:write-profile fake-br-profile))
  (check-true (response-has-content-encoding? resp "br")))

(test-case "accept-encoding: falls back when client does not accept br"
  (define req (make-request-with-accept-encoding "gzip, deflate"))
  (define resp (datastar-sse req (lambda (sse) (void)) #:write-profile fake-br-profile))
  (check-false (response-has-content-encoding? resp "br")))

(test-case "accept-encoding: falls back when no Accept-Encoding header"
  (define req (make-request-with-accept-encoding #f))
  (define resp (datastar-sse req (lambda (sse) (void)) #:write-profile fake-br-profile))
  (check-false (response-has-content-encoding? resp "br")))

(test-case "accept-encoding: quality weights are stripped"
  (define req (make-request-with-accept-encoding "gzip;q=1.0, br;q=0.5"))
  (define resp (datastar-sse req (lambda (sse) (void)) #:write-profile fake-br-profile))
  (check-true (response-has-content-encoding? resp "br")))

(test-case "accept-encoding: basic profile always works regardless of headers"
  (define req (make-request-with-accept-encoding "gzip, deflate"))
  (define resp (datastar-sse req (lambda (sse) (void)) #:write-profile basic-write-profile))
  (check-false (response-has-content-encoding? resp "br")))

(module+ test
  (displayln "Unit tests complete."))

(module+ main
  (require rackunit/text-ui)
  (displayln "Running unit tests...")
  (void))
