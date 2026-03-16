#lang racket

(require datastar
         datastar-brotli
         racket/async-channel
         web-server/dispatch
         web-server/http
         web-server/safety-limits
         web-server/servlet-env
         xml)

(file-stream-buffer-mode (current-output-port) 'line)

;; ---------------------------------------------------------------------------
;; In-memory todo store
;; ---------------------------------------------------------------------------

(define id-counter (box 0))
(define todos (box '()))
(define subscribers (box '()))

(define (next-id!)
  (define id (add1 (unbox id-counter)))
  (set-box! id-counter id)
  id)

(define (subscribe!)
  (define ch (make-async-channel))
  (set-box! subscribers (cons ch (unbox subscribers)))
  ch)

(define (unsubscribe! ch)
  (set-box! subscribers (filter (lambda (c) (not (eq? c ch))) (unbox subscribers))))

(define (notify! cmd)
  (for ([ch (in-list (unbox subscribers))])
    (async-channel-put ch cmd)))

;; ---------------------------------------------------------------------------
;; Main view
;; ---------------------------------------------------------------------------

(define (render-main)
  (define items (unbox todos))
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
;; Read side — long-lived SSE connection with fat morphs
;; ---------------------------------------------------------------------------

(define brotli-profile (make-brotli-write-profile))

(define (events-handler req)
  (define ch (subscribe!))
  (datastar-sse req
                (lambda (sse)
                  (patch-elements sse (xexpr->string (render-main)))
                  (let loop ()
                    (define cmd (async-channel-get ch))
                    (define ok (patch-elements sse (xexpr->string (render-main))))
                    (when (and ok (eq? cmd 'create))
                      (patch-signals sse (hash 'input "")))
                    (when ok
                      (loop))))
                #:on-close (lambda (_sse) (unsubscribe! ch))
                #:write-profile brotli-profile))

;; ---------------------------------------------------------------------------
;; Write side — mutate state and notify SSE loop
;; ---------------------------------------------------------------------------

(define (todo-create req)
  (define signals (read-signals req))
  (define text (string-trim (hash-ref signals 'input "")))
  (unless (string=? text "")
    (define id (next-id!))
    (set-box! todos (append (unbox todos) (list (hash 'id id 'text text))))
    (notify! 'create))
  (response/empty))

(define (todo-delete _req id-str)
  (define id (string->number id-str))
  (when id
    (set-box! todos (filter (lambda (t) (not (= (hash-ref t 'id) id))) (unbox todos)))
    (notify! 'delete))
  (response/empty))

;; ---------------------------------------------------------------------------
;; Home page - serve view with initial state
;; ---------------------------------------------------------------------------

(define (home-handler _req)
  (response/xexpr
   `(html
     (head
      (script
       ((type "module")
        (src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.8/bundles/datastar.js"))))
     (body ,(render-main)))))

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

(serve/servlet app
               #:command-line? #t
               #:listen-ip "127.0.0.1"
               #:port 8080
               #:servlet-regexp #rx""
               #:connection-close? #t
               #:safety-limits (make-safety-limits #:response-send-timeout +inf.0))
