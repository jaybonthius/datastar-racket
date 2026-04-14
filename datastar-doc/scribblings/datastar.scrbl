#lang scribble/doc

@(require (for-label datastar
                     racket/base)
          scribble/manual
          "datastar.rkt")

@title{Datastar Racket SDK}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[datastar]

Datastar is a hypermedia framework that provides backend reactivity like htmx, and frontend reactivity like Alpine.js in  lightweight frontend framework that doesn’t require any npm packages or other dependencies.

@link["https://data-star.dev"]{Datastar} is a hypermedia framework that provides JS client handles the frontend reactivity, and the server drives the UI over SSE:. 

@itemlist[
  @item{@link["https://data-star.dev/reference/attributes"]{Attributes} bind signals, handle events, and add reactivity. @link["https://data-star.dev/reference/actions"]{Actions} like @tt|{@get()}| make requests to the server and communicate the frontend state via signals.}
  @item{The backend responds with SSE events that patch the DOM, update signals, and run scripts.}
]

This Racket SDK provides server-side helpers for reading signals from requests and sending SSE events, as well as client-side helpers for generating @tt{data-*} attributes and action expressions.

@local-table-of-contents[]

@(require (for-label datastar
                     racket/base
                     racket/contract
                     web-server/safety-limits
                     web-server/servlet-dispatch
                     web-server/web-server)
          "datastar.rkt")


@section{How Datastar Works}

On the frontend, include the Datastar client library via a script tag (see the @link["https://data-star.dev/guide/getting_started#installation"]{installation guide} for options):

@verbatim|{
<script type="module"
  src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1/bundles/datastar.js">
</script>
}|

@link["https://data-star.dev/reference/attributes"]{@tt{data-*} attributes} add reactivity to the frontend. For example, @tt{data-on} attaches event listeners. @link["https://data-star.dev/reference/actions"]{Actions} like @tt|{@get()}| make requests to the server:

@verbatim|{
<button data-on:click="@get('/endpoint')">Click me</button>
<div id="output"></div>
}|

The server responds with SSE events containing HTML elements. Datastar morphs those elements into the existing DOM by matching element IDs, updating only what changed. This is what the SDK helps with from Racket.

@section{Minimal Example}

A handler that patches a single element into the page:

@codeblock{
#lang racket

(require datastar
         web-server/safety-limits
         web-server/servlet-dispatch
         web-server/web-server)

(define (handler req)
  (datastar-sse
   (lambda (sse)
     (patch-elements/xexprs sse '(div ((id "output")) "Hello from Datastar!")))))

(define stop
  (serve #:dispatch (dispatch/servlet handler)
         #:tcp@"@" datastar-tcp@"@"
         #:listen-ip "127.0.0.1"
         #:port 8000
         #:connection-close? #t
         #:safety-limits (make-safety-limits #:response-timeout +inf.0
                                             #:response-send-timeout +inf.0)))

(with-handlers ([exn:break? (lambda (e) (stop))])
  (sync/enable-break never-evt))
}

@subsection{One-Shot Responses}

For single-request updates, you can return a normal HTTP response using @racketmodname[datastar/http/response] helpers instead of opening an SSE stream:

@racketblock[
(define (save-handler req)
  (response/datastar-signals (hash 'flash "Saved!" 'form (hash 'name ""))))
]

@section{Streaming Updates}

For streaming updates, loop inside the callback. If the client disconnects or a send fails, an exception is raised which @racket[datastar-sse] catches automatically, triggering cleanup via @racket[on-close]:

@racketblock[
(define (streaming-handler req)
  (datastar-sse (lambda (sse)
                  (for ([i (in-range 10)])
                    (patch-elements/xexprs sse
                                           `(div ((id "counter"))
                                                 ,(format "Count: ~a" i)))
                    (patch-signals sse (hash 'counter i))
                    (sleep 1)))))
]

You can also use the @racket[#:on-close] callback for cleanup when the connection ends:

@codeblock{
(define connections (mutable-set))

(define (streaming-handler req)
  (datastar-sse (lambda (sse)
                  (set-add! connections sse)
                  (console-log sse "connected"))
                #:on-close (lambda (sse) (set-remove! connections sse))))
}

For more examples, see the @link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory} on GitHub.
@include-section["http.scrbl"]
@include-section["sugar.scrbl"]
@include-section["testing.scrbl"]
