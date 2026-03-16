#lang racket

;; This example requires koyo/session
;; Install with `raco pkg install koyo-lib`

(require datastar
         datastar-brotli
         koyo/session
         racket/async-channel
         racket/random
         web-server/dispatch
         web-server/http
         web-server/web-server
         xml)

(file-stream-buffer-mode (current-output-port) 'line)

;; ---------------------------------------------------------------------------
;; Session setup
;; ---------------------------------------------------------------------------

(define session-store (make-memory-session-store))

(define session-manager
  ((make-session-manager-factory #:cookie-name "sid"
                                 #:cookie-secure? #f
                                 #:cookie-http-only? #t
                                 #:cookie-same-site 'lax
                                 #:shelf-life 86400
                                 #:secret-key (crypto-random-bytes 16)
                                 #:store session-store)))

;; ---------------------------------------------------------------------------
;; Per-session todo store
;; ---------------------------------------------------------------------------

(struct session-data (id-counter todos subscribers) #:transparent)

(define stores (make-hash))

(define (get-store sid)
  (hash-ref! stores sid (lambda () (session-data (box 0) (box '()) (box '())))))

(define (next-id! sid)
  (define store (get-store sid))
  (define id (add1 (unbox (session-data-id-counter store))))
  (set-box! (session-data-id-counter store) id)
  id)

(define (get-todos sid)
  (unbox (session-data-todos (get-store sid))))

(define (subscribe! sid)
  (define store (get-store sid))
  (define ch (make-async-channel))
  (set-box! (session-data-subscribers store) (cons ch (unbox (session-data-subscribers store))))
  ch)

(define (unsubscribe! sid ch)
  (define store (get-store sid))
  (set-box! (session-data-subscribers store)
            (filter (lambda (c) (not (eq? c ch))) (unbox (session-data-subscribers store)))))

(define (notify! sid cmd)
  (for ([ch (in-list (unbox (session-data-subscribers (get-store sid))))])
    (async-channel-put ch cmd)))

;; ---------------------------------------------------------------------------
;; Main view
;; ---------------------------------------------------------------------------

(define (render-main sid)
  (define items (get-todos sid))
  `(main ((id "main") (data-init "@get('/events')"))
         (h1 "CQRS Todo Example")
         (form ((data-on:submit "@post('/todo/create')"))
               (input ((id "todo-input") (name "todo-input")
                                         (placeholder "What needs to be done?")
                                         (data-bind:input "")))
               (button ((type "submit")) "Add"))
         (ul ,@(for/list ([todo (in-list items)])
                 (define tid (number->string (hash-ref todo 'id)))
                 `(li ,(hash-ref todo 'text)
                      " "
                      (button ((data-on:click ,(format "@post('/todo/delete/~a')" tid)))
                              "Delete"))))))

;; ---------------------------------------------------------------------------
;; Read side -- long-lived SSE connection with fat morphs
;; ---------------------------------------------------------------------------

(define brotli-profile (make-brotli-write-profile))

(define (events-handler req)
  (define sid (current-session-id))
  (define ch (subscribe! sid))
  (datastar-sse req
                (lambda (sse)
                  (printf "Client connected (session: ~a)~n" sid)
                  (patch-elements sse (xexpr->string (render-main sid)))
                  (let loop ()
                    (define cmd (async-channel-get ch))
                    (patch-elements sse (xexpr->string (render-main sid)))
                    (when (eq? cmd 'create)
                      (patch-signals sse (hash 'input "")))
                    (loop)))
                #:on-close (lambda (_sse)
                             (printf "Client disconnected (session: ~a)~n" sid)
                             (unsubscribe! sid ch))
                #:write-profile brotli-profile))

;; ---------------------------------------------------------------------------
;; Write side -- mutate state and notify SSE loop
;; ---------------------------------------------------------------------------

(define (todo-create req)
  (define sid (current-session-id))
  (define signals (read-signals req))
  (define text (string-trim (hash-ref signals 'input "")))
  (unless (string=? text "")
    (define store (get-store sid))
    (define id (next-id! sid))
    (set-box! (session-data-todos store)
              (append (unbox (session-data-todos store)) (list (hash 'id id 'text text))))
    (notify! sid 'create))
  (response/empty))

(define (todo-delete _req id-str)
  (define sid (current-session-id))
  (define id (string->number id-str))
  (when id
    (define store (get-store sid))
    (set-box! (session-data-todos store)
              (filter (lambda (t) (not (= (hash-ref t 'id) id))) (unbox (session-data-todos store))))
    (notify! sid 'delete))
  (response/empty))

;; ---------------------------------------------------------------------------
;; Home page
;; ---------------------------------------------------------------------------

(define (home-handler _req)
  (response/xexpr
   `(html
     (head
      (script
       ((type "module")
        (src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.8/bundles/datastar.js"))))
     (body ,(render-main (current-session-id))))))

(define (not-found-handler _req)
  (response/xexpr '(html (body "Not found"))))

;; ---------------------------------------------------------------------------
;; Routing
;; ---------------------------------------------------------------------------

(define-values (app _reverse-uri)
  (dispatch-rules [("") home-handler]
                  [("events") events-handler]
                  [("todo" "create") #:method "post" todo-create]
                  [("todo" "delete" (string-arg)) #:method "post" todo-delete]
                  [else not-found-handler]))

(printf "Starting CQRS example on http://127.0.0.1:8080~n")

(define stop
  (serve #:dispatch (dispatch/datastar ((wrap-session session-manager) app))
         #:listen-ip "127.0.0.1"
         #:port 8080
         #:connection-close? #t))

(with-handlers ([exn:break? (lambda (_e) (stop))])
  (sync/enable-break never-evt))
