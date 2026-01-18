#lang racket

(require datastar
         gregor
         racket/generator
         web-server/dispatch
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server
         xml)

(struct store (message count) #:transparent)

(define (patch-elements-handler _req)
  (datastar-response (patch-elements "<div id=\"output\">Hello from Datastar!</div>")))

(define (remove-elements-handler _req)
  (datastar-response (remove-elements "#temporary-element")))

(define (patch-signals-handler req)
  (define signals-data (read-signals req))
  (define current-store (store (hash-ref signals-data 'message "") (hash-ref signals-data 'count 0)))
  (datastar-response
   (patch-signals (hash 'message "Updated message" 'count (+ (store-count current-store) 1)))))

(define (execute-script-handler _req)
  (datastar-response (execute-script "console.log(\"Hello from server!\")")))

(define (redirect-handler _req)
  (datastar-response (redirect "/new-page")))

(define (streaming-handler _req)
  (datastar-response
   (in-generator (let loop ()
                   (define current-time (~t (now) "yyyy-MM-dd'T'HH:mm:ss.SSS"))
                   (yield (patch-signals (hash 'currentTime current-time)))
                   (yield (patch-elements (xexpr->string `(span ([id "streamingTimeElement"])
                                                                ,current-time))))
                   (sleep 0.1)
                   (loop)))))

(define (new-page-handler _req)
  (response/xexpr '(html (body "You've been redirected to the new page!"))))

(define (home-handler _req)
  (response/xexpr
   `(html
     (head
      (style
       "html,body{height:100%;width:100%;margin:0;padding:0}body{background-image:linear-gradient(to right bottom,oklch(0.424958 0.052808 253.972015),oklch(0.189627 0.038744 264.832977));font-family:monospace,sans-serif}.container{max-width:600px;margin:2rem auto;padding:1.5rem;background-color:oklch(0.916374 0.034554 90.5157);border-radius:8px}div{margin-bottom:1rem;padding:1rem;background-color:oklch(0.8 0.04 90);border-radius:4px;box-shadow:0 1px 4px 0 oklch(0.8 0.01 90/0.08)}h1{color:oklch(0.265104 0.006243 0.522862);text-align:center;margin-bottom:1rem}button{background-color:oklch(0.8 0.04 90);color:oklch(0.265104 0.006243 0.522862);border:none;padding:8px 16px;margin:4px;border-radius:6px;cursor:pointer;font-weight:600}button:hover{background-color:oklch(0.75 0.05 90)}")
      (script ((type "module")
               (src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"))))
     (body ((data-signals "{message: '', count: 0, currentTime: 'Not started'}"))
           (div ((class "container"))
                (h1 "Datastar Usage Example")
                (div ((id "temporary-element")) "This element will be removed")
                (div ((id "output")) "Initial content - will be replaced")
                (p "Message: " (span ((data-text "$message"))))
                (p "Count: " (span ((data-text "$count"))))
                (p "Streaming time (via signals): " (span ((data-text "$currentTime"))))
                (p "Streaming time (via elements): "
                   (span ((id "streamingTimeElement")) "Not started"))
                (button ((data-on:click "@patch('patch-elements')")) "Patch Elements")
                (button ((data-on:click "@delete('remove-elements')")) "Remove Elements")
                (button ((data-on:click "@patch('patch-signals')")) "Patch Signals")
                (button ((data-on:click "@post('execute-script')")) "Execute Script")
                (button ((data-on:click "@get('streaming')")) "Start Streaming")
                (button ((data-on:click "@post('redirect')")) "Redirect"))))))

(define (not-found-handler _req)
  (response/xexpr '(html (body "Not found"))))

(define-values (app _reverse-uri)
  (dispatch-rules [("") home-handler]
                  [("patch-elements") #:method "patch" patch-elements-handler]
                  [("remove-elements") #:method "delete" remove-elements-handler]
                  [("patch-signals") #:method "patch" patch-signals-handler]
                  [("execute-script") #:method "post" execute-script-handler]
                  [("streaming") streaming-handler]
                  [("redirect") #:method "post" redirect-handler]
                  [("new-page") new-page-handler]
                  [else not-found-handler]))

(printf "Starting server on http://127.0.0.1:8080\n")

(define stop (serve #:dispatch (dispatch/servlet app) #:listen-ip "127.0.0.1" #:port 8080))

(with-handlers ([exn:break? (lambda (_e)
                              (printf "Shutting down server...\n")
                              (stop))])
  (sync/enable-break never-evt))
