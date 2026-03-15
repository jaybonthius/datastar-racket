#lang racket

(require datastar
         gregor
         racket/generator
         web-server/dispatch
         web-server/http
         web-server/servlet-env
         xml)

(struct store (message count) #:transparent)

(define (patch-elements-handler _req)
  (datastar-response (patch-elements (xexpr->string '(div ([id "output"]) "Hello from Datastar!")))))

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
      (link ([rel "stylesheet"] [href "/style.css"]))
      (script ((type "module")
               (src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.8/bundles/datastar.js"))))
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

(serve/servlet app
               #:command-line? #t
               #:listen-ip "127.0.0.1"
               #:port 8080
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path (current-directory) "static")))
