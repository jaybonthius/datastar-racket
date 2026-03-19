#lang racket

(require datastar
         net/http-client
         racket/async-channel
         rackunit
         web-server/safety-limits
         web-server/servlet-dispatch
         web-server/web-server)

;; test infrastructure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-test-server! handler)
  (define confirm-ch (make-async-channel))
  (define stop
    (serve #:dispatch (dispatch/servlet handler)
           #:tcp@ datastar-tcp@
           #:listen-ip "127.0.0.1"
           #:port 0
           #:connection-close? #t
           #:safety-limits (make-safety-limits #:response-timeout +inf.0
                                               #:response-send-timeout +inf.0)
           #:confirmation-channel confirm-ch))
  (define port (async-channel-get confirm-ch))
  (values port stop))

(define (open-sse-connection port path)
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (define-values (status headers in)
    (http-conn-sendrecv! conn path #:method "GET" #:headers '("Accept: text/event-stream")))
  (values conn status headers in))

(define (open-sse-stream port path)
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (http-conn-send! conn path #:method "GET" #:headers '("Accept: text/event-stream"))
  (define-values (status headers in) (http-conn-recv! conn #:close? #f))
  (values conn status headers in))

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
             (if (null? lines)
                 (loop lines)
                 (apply string-append (reverse lines)))]
            [else (loop (cons (string-append line "\n") lines))])])])))

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

;; test 1: SSE response has status 200 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "SSE response has status 200"
  (define (handler req)
    (datastar-sse req (lambda (sse) (patch-elements sse "<div id=\"t\">ok</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn status _headers in) (open-sse-connection port "/"))
  (check-regexp-match #rx"200" status)
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; test 2: SSE response has correct headers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "SSE response has correct headers"
  (define (handler req)
    (datastar-sse req (lambda (sse) (patch-elements sse "<div>test</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status headers in) (open-sse-connection port "/"))
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

;; test 3: SSE body matches expected event format ;;;;;;;;;;;;;;;;;;;;;;

(test-case "SSE body matches expected event format for single event"
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

;; test 4: multiple events delivered in correct order ;;;;;;;;;;;;;;;;;;

(test-case "multiple events delivered in correct order"
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

;; test 5: on-close fires when on-open returns normally ;;;;;;;;;;;;;;;;

(test-case "on-close fires when on-open returns normally"
  (define close-called (box #f))
  (define (handler req)
    (datastar-sse req
                  (lambda (sse) (patch-elements sse "<div>done</div>"))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-connection port "/"))
  (read-sse-event in)
  (read-sse-event in #:timeout 3)
  (sleep 0.5)
  (check-true (unbox close-called) "on-close should have been called")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; test 6: on-close fires when on-open raises an exception ;;;;;;;;;;;;;

(test-case "on-close fires when on-open raises an exception"
  ;; Tests the dynamic-wind guarantee
  (define close-called (box #f))
  (define (handler req)
    (datastar-sse req
                  (lambda (_sse) (error "intentional test error"))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))
  (define-values (port stop) (start-test-server! handler))
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (with-handlers ([exn:fail? void])
    (define-values (_status _headers in)
      (http-conn-sendrecv! conn "/" #:method "GET" #:headers '("Accept: text/event-stream")))
    (with-handlers ([exn:fail? void])
      (read-sse-event in #:timeout 2))
    (with-handlers ([exn:fail? void])
      (close-input-port in)))
  (sleep 1)
  (check-true (unbox close-called) "on-close should fire even when on-open raises")
  (with-handlers ([exn:fail? void])
    (http-conn-close! conn))
  (stop))

;; test 7: close-sse inside handler terminates the response ;;;;;;;;;;;;

(test-case "close-sse inside handler terminates the response"
  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    (patch-elements sse "<div>before-close</div>")
                    (close-sse sse)
                    (patch-elements sse "<div>after-close</div>"))))
  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-connection port "/"))
  (define all-data (read-all-sse in #:timeout 3))
  (check-true (string-contains? all-data "before-close") "should contain data sent before close")
  (check-false (string-contains? all-data "after-close") "should NOT contain data sent after close")
  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; test 8: persistent connection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "persistent connection: events sent from outside handler"
  (define notify-ch (make-async-channel))
  (define close-called (box #f))

  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    (patch-elements sse "<div id=\"v\">initial</div>")
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

  (define evt0 (read-sse-event in))
  (check-not-false evt0 "should receive initial event")
  (check-true (string-contains? evt0 "initial") "initial event should have initial content")

  (async-channel-put notify-ch "update-1")
  (define evt1 (read-sse-event in))
  (check-not-false evt1 "should receive first update")
  (check-true (string-contains? evt1 "update-1") "should contain update-1")

  (async-channel-put notify-ch "update-2")
  (define evt2 (read-sse-event in))
  (check-not-false evt2 "should receive second update")
  (check-true (string-contains? evt2 "update-2") "should contain update-2")

  (async-channel-put notify-ch 'stop)

  (read-sse-event in #:timeout 3)
  (sleep 0.5)
  (check-true (unbox close-called) "on-close should fire after handler exits")

  (close-input-port in)
  (http-conn-close! conn)
  (stop))

;; test 9: on-close fires on client disconnect ;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "persistent connection: on-close fires on client disconnect"
  (define close-called (box #f))
  (define notify-ch (make-async-channel))

  (define (handler req)
    (datastar-sse req
                  (lambda (sse)
                    (let loop ()
                      (define msg (async-channel-get notify-ch))
                      (patch-elements sse (format "<div>~a</div>" msg))
                      (loop)))
                  #:on-close (lambda (_sse) (set-box! close-called #t))))

  (define-values (port stop) (start-test-server! handler))

  ;; With datastar-tcp@, the monitor thread detects the disconnect immediately
  ;; via the TCP input port, without needing to force writes.
  (define conn (http-conn-open "127.0.0.1" #:port port))
  (http-conn-send! conn "/" #:method "GET" #:headers '("Accept: text/event-stream"))
  (http-conn-close! conn)

  (sleep 2)
  (check-true (unbox close-called) "on-close should fire after client disconnects")
  (stop))

;; test 10: batch locking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "batch locking: with-sse-lock delivers events as atomic batch"
  (define notify-ch (make-async-channel))
  (define close-called (box #f))

  (define (handler req)
    (datastar-sse
     req
     (lambda (sse)
       (patch-elements sse "<div id=\"init\">ready</div>")
       (let loop ()
         (define msg (async-channel-get notify-ch))
         (cond
           [(eq? msg 'stop) (void)]
           [else
            (with-sse-lock sse
                           (patch-elements sse (format "<div id=\"start\">~a-start</div>" msg))
                           (patch-elements sse (format "<div id=\"end\">~a-end</div>" msg)))
            (loop)])))
     #:on-close (lambda (_sse) (set-box! close-called #t))))

  (define-values (port stop) (start-test-server! handler))
  (define-values (conn _status _headers in) (open-sse-stream port "/"))

  (define evt0 (read-sse-event in))
  (check-not-false evt0 "should receive initial event")
  (check-true (string-contains? evt0 "ready") "initial event should have ready content")

  (async-channel-put notify-ch "batch-1")
  (define evt1 (read-sse-event in))
  (define evt2 (read-sse-event in))
  (check-not-false evt1 "should receive first event of batch-1")
  (check-not-false evt2 "should receive second event of batch-1")
  (check-true (string-contains? evt1 "batch-1-start") "first event should be start")
  (check-true (string-contains? evt2 "batch-1-end") "second event should be end")

  (async-channel-put notify-ch "batch-2")
  (define evt3 (read-sse-event in))
  (define evt4 (read-sse-event in))
  (check-not-false evt3 "should receive first event of batch-2")
  (check-not-false evt4 "should receive second event of batch-2")
  (check-true (string-contains? evt3 "batch-2-start") "first event should be start")
  (check-true (string-contains? evt4 "batch-2-end") "second event should be end")

  (async-channel-put notify-ch 'stop)
  (read-sse-event in #:timeout 3)
  (sleep 0.5)
  (check-true (unbox close-called) "on-close should fire")

  (close-input-port in)
  (http-conn-close! conn)
  (stop))
