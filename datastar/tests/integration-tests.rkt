#lang racket

;; Integration tests for the Datastar Racket SDK.
;;
;; These test the full SSE lifecycle through a real web server and HTTP client,
;; analogous to Clojure's persistent_connection.clj and Go's httptest pattern.

(require datastar
         json
         net/http-client
         net/url
         racket/async-channel
         racket/tcp
         rackunit
         web-server/dispatch
         web-server/http
         web-server/safety-limits
         web-server/servlet-env)

;; ============================================================================
;; Test infrastructure
;; ============================================================================

;; Finds a free port by briefly binding and releasing.
(define (find-free-port)
  (define listener (tcp-listen 0 5 #t "127.0.0.1"))
  (define-values (_1 port _2 _3) (tcp-addresses listener #t))
  (tcp-close listener)
  port)

;; Waits until a TCP connection to 127.0.0.1:port succeeds or times out.
(define (wait-for-server! port #:timeout [timeout 5])
  (define deadline (+ (current-inexact-milliseconds) (* timeout 1000)))
  (let loop ()
    (cond
      [(> (current-inexact-milliseconds) deadline)
       (error 'wait-for-server! "server did not start within ~a seconds" timeout)]
      [(with-handlers ([exn:fail:network? (lambda (_) #f)])
         (define-values (in out) (tcp-connect "127.0.0.1" port))
         (close-input-port in)
         (close-output-port out)
         #t)
       (void)]
      [else
       (sleep 0.05)
       (loop)])))

;; Starts a test server with the given dispatch handler. Returns (values port stop-thunk).
(define (start-test-server! handler)
  (define port (find-free-port))
  (define server-thread
    (thread (lambda ()
              (serve/servlet handler
                             #:command-line? #t
                             #:listen-ip "127.0.0.1"
                             #:port port
                             #:servlet-regexp #rx""
                             #:connection-close? #t
                             #:safety-limits (make-safety-limits #:response-timeout +inf.0
                                                                 #:response-send-timeout +inf.0)))))
  (wait-for-server! port)
  (values port (lambda () (kill-thread server-thread))))

;; Opens an SSE connection and returns (values http-conn status headers input-port).
;; Uses sendrecv! which blocks until the response is complete — suitable for
;; short-lived connections (tests 1-7).
(define (open-sse-connection port path)
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (define-values (status headers in)
    (http-conn-sendrecv! conn path #:method "GET" #:headers '("Accept: text/event-stream")))
  (values conn status headers in))

;; Opens a streaming SSE connection and returns (values http-conn status headers input-port).
;; Uses send! + recv! with #:close? #f so the input port is returned immediately
;; without waiting for the full response body — suitable for long-lived persistent
;; connections (tests 8-9).
(define (open-sse-stream port path)
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (http-conn-send! conn path #:method "GET" #:headers '("Accept: text/event-stream"))
  (define-values (status headers in) (http-conn-recv! conn #:close? #f))
  (values conn status headers in))

;; Reads a single SSE event from the input port.
;; Returns the event as a string, or #f on timeout/eof.
(define (read-sse-event in #:timeout [timeout 5])
  (define deadline (+ (current-inexact-milliseconds) (* timeout 1000)))
  (let loop ([lines '()])
    (define remaining (/ (- deadline (current-inexact-milliseconds)) 1000))
    (cond
      [(<= remaining 0)
       (if (null? lines)
           #f
           (apply string-append (reverse lines)))]
      [else
       (define ready (sync/timeout remaining in))
       (cond
         [(not ready)
          (if (null? lines)
              #f
              (apply string-append (reverse lines)))]
         [else
          (define line (read-line in 'any))
          (cond
            [(eof-object? line)
             (if (null? lines)
                 #f
                 (apply string-append (reverse lines)))]
            [(string=? line "")
             ;; Empty line terminates an SSE event
             (if (null? lines)
                 (loop lines) ; skip leading blank lines
                 (apply string-append (reverse lines)))]
            [else (loop (cons (string-append line "\n") lines))])])])))

;; Reads all remaining lines from an input port until eof or timeout.
(define (read-all-sse in #:timeout [timeout 5])
  (define deadline (+ (current-inexact-milliseconds) (* timeout 1000)))
  (let loop ([lines '()])
    (define remaining (/ (- deadline (current-inexact-milliseconds)) 1000))
    (cond
      [(<= remaining 0) (apply string-append (reverse lines))]
      [else
       (define ready (sync/timeout remaining in))
       (cond
         [(not ready) (apply string-append (reverse lines))]
         [else
          (define line (read-line in 'any))
          (cond
            [(eof-object? line) (apply string-append (reverse lines))]
            [else (loop (cons (string-append line "\n") lines))])])])))

;; ============================================================================
;; Test 1: SSE response has status 200
;; ============================================================================

(test-case "SSE response has status 200"
  ;; Analogous to Clojure sse-status-ok?
  (define (handler req)
    (datastar-sse req (lambda (sse) (patch-elements sse "<div id=\"t\">ok</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn status _headers in) (open-sse-connection port "/"))
  (check-regexp-match #rx"200" status)
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 2: SSE response has correct headers
;; ============================================================================

(test-case "SSE response has correct headers"
  ;; Analogous to Go TestSSESendWithActiveConnection header check,
  ;; Clojure sse-http1-headers-ok?
  (define (handler req)
    (datastar-sse req (lambda (sse) (patch-elements sse "<div>test</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn status headers in) (open-sse-connection port "/"))
  ;; Check Content-Type
  (define (find-header name)
    (for/or ([h (in-list headers)])
      (define s
        (if (bytes? h)
            (bytes->string/utf-8 h)
            h))
      (and (string-prefix? (string-downcase s) (string-downcase (string-append name ":"))) s)))
  (check-not-false (find-header "Content-Type") "should have Content-Type header")
  (check-true (string-contains? (string-downcase (or (find-header "Content-Type") ""))
                                "text/event-stream")
              "Content-Type should be text/event-stream")
  (check-not-false (find-header "Cache-Control") "should have Cache-Control header")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 3: SSE body matches expected event format
;; ============================================================================

(test-case "SSE body matches expected event format for single event"
  ;; Analogous to Clojure sse-body-ok?
  (define (handler req)
    (datastar-sse req (lambda (sse) (patch-elements sse "<div id=\"out\">hello</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-connection port "/"))
  (define evt (read-sse-event in))
  (check-not-false evt "should receive an event")
  (check-true (string-contains? evt "event: datastar-patch-elements")
              "should contain correct event type")
  (check-true (string-contains? evt "data: elements <div id=\"out\">hello</div>")
              "should contain the elements data line")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 4: Multiple events delivered in correct order
;; ============================================================================

(test-case "multiple events delivered in correct order"
  ;; Analogous to Clojure persistent connection sse-interaction! pattern
  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    (patch-elements sse "<div id=\"a\">first</div>")
                    (patch-elements sse "<div id=\"b\">second</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-connection port "/"))
  (define evt1 (read-sse-event in))
  (define evt2 (read-sse-event in))
  (check-not-false evt1 "should receive first event")
  (check-not-false evt2 "should receive second event")
  (check-true (string-contains? evt1 "first") "first event should contain 'first'")
  (check-true (string-contains? evt2 "second") "second event should contain 'second'")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 5: on-close fires when on-open returns normally
;; ============================================================================

(test-case "on-close fires when on-open returns normally"
  ;; Analogous to Clojure on-close callback tests
  (define close-called (box #f))
  (define (handler req)
    (datastar-sse req
                  (lambda (sse) (patch-elements sse "<div>done</div>"))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-connection port "/"))
  ;; Read the event and wait for connection to close
  (read-sse-event in)
  ;; Wait for eof (handler returned, so connection should close)
  (define result (read-sse-event in #:timeout 3))
  ;; Give on-close a moment to execute
  (sleep 0.5)
  (check-true (unbox close-called) "on-close should have been called")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 6: on-close fires when on-open raises an exception
;; ============================================================================

(test-case "on-close fires when on-open raises an exception"
  ;; Tests the dynamic-wind guarantee — unique to Racket but critical for correctness
  (define close-called (box #f))
  (define (handler req)
    (datastar-sse req
                  (lambda (sse) (error "intentional test error"))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))
  (define-values (port stop) (start-test-server! handler))
  (define conn (http-conn-open "127.0.0.1" #:port port))
  ;; The request may fail or return an error response because the handler errors,
  ;; but on-close should still fire due to dynamic-wind.
  (with-handlers ([exn:fail? void])
    (define-values (_status _headers in)
      (http-conn-sendrecv! conn "/" #:method "GET" #:headers '("Accept: text/event-stream")))
    (with-handlers ([exn:fail? void])
      (read-sse-event in #:timeout 2))
    (with-handlers ([exn:fail? void])
      (close-input-port in)))
  ;; Give on-close a moment to execute
  (sleep 1)
  (check-true (unbox close-called) "on-close should fire even when on-open raises")
  (with-handlers ([exn:fail? void])
    (http-conn-close! conn))
  (stop))

;; ============================================================================
;; Test 7: close-sse inside handler terminates the response
;; ============================================================================

(test-case "close-sse inside handler terminates the response"
  ;; Analogous to Clojure d*/close-sse! in persistent connection test
  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    (patch-elements sse "<div>before-close</div>")
                    (close-sse sse)
                    ;; Send after close should raise
                    (patch-elements sse "<div>after-close</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-connection port "/"))
  (define all-data (read-all-sse in #:timeout 3))
  (check-true (string-contains? all-data "before-close") "should contain data sent before close")
  (check-false (string-contains? all-data "after-close") "should NOT contain data sent after close")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 8: Persistent connection — send events from outside the handler
;; ============================================================================

(test-case "persistent connection: events sent from outside handler"
  ;; Analogous to Clojure persistent_connection.clj ->persistent-sse-handler
  ;; pattern, where the sse-gen is captured in a promise and events are sent
  ;; from outside the request handler. This is how the CQRS example works.
  (define notify-ch (make-async-channel))
  (define close-called (box #f))

  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    (patch-elements sse "<div id=\"v\">initial</div>")
                    ;; Block until signaled — real async-channel-get, just like
                    ;; the CQRS example. The test client uses open-sse-stream
                    ;; (non-blocking) so this doesn't cause a hang.
                    (let loop ()
                      (define msg (async-channel-get notify-ch))
                      (cond
                        [(eq? msg 'stop) (void)]
                        [else
                         (patch-elements sse (format "<div id=\"v\">~a</div>" msg))
                         (loop)])))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))

  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-stream port "/"))

  ;; Read initial event
  (define evt0 (read-sse-event in))
  (check-not-false evt0 "should receive initial event")
  (check-true (string-contains? evt0 "initial") "initial event should have initial content")

  ;; Send events from outside the handler via the async-channel
  (async-channel-put notify-ch "update-1")
  (define evt1 (read-sse-event in))
  (check-not-false evt1 "should receive first update")
  (check-true (string-contains? evt1 "update-1") "should contain update-1")

  (async-channel-put notify-ch "update-2")
  (define evt2 (read-sse-event in))
  (check-not-false evt2 "should receive second update")
  (check-true (string-contains? evt2 "update-2") "should contain update-2")

  ;; Signal the handler to stop
  (async-channel-put notify-ch 'stop)

  ;; Wait for connection to close
  (read-sse-event in #:timeout 3)
  (sleep 0.5)
  (check-true (unbox close-called) "on-close should fire after handler exits")

  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; ============================================================================
;; Test 9: Persistent connection — on-close fires on client disconnect
;; ============================================================================

(test-case "persistent connection: on-close fires on client disconnect"
  ;; Tests that cleanup happens when the client side drops the connection,
  ;; analogous to the CQRS unsubscribe pattern.
  (define close-called (box #f))
  (define notify-ch (make-async-channel))

  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    ;; When the client disconnects, the monitor thread
                    ;; breaks this thread, and dynamic-wind fires on-close.
                    (let loop ()
                      (define msg (async-channel-get notify-ch))
                      (patch-elements sse (format "<div>~a</div>" msg))
                      (loop)))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))

  (define-values (port stop) (start-test-server! handler))

  ;; Connect and immediately disconnect using raw send! — we don't call recv!
  ;; because the handler blocks on async-channel-get before writing, so recv!
  ;; would hang waiting for response headers that haven't been sent yet.
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (http-conn-send! conn "/" #:method "GET" #:headers '("Accept: text/event-stream"))
  (http-conn-close! conn)

  ;; Push messages to force the handler to attempt writes on the dead connection.
  ;; Eventually patch-elements raises, the error is caught by datastar-sse's
  ;; with-handlers, and dynamic-wind triggers on-close.
  (for ([_ (in-range 5)])
    (async-channel-put notify-ch "ping")
    (sleep 0.3))

  (sleep 1)
  (check-true (unbox close-called) "on-close should fire after client disconnects and a write fails")
  (stop))

(module+ test
  (displayln "Integration tests complete."))

(module+ main
  (require rackunit/text-ui)
  (displayln "Running integration tests...")
  (void))
