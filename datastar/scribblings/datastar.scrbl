#lang scribble/doc

@(require scribble/manual
          (for-label datastar
                     datastar/testing
                     racket/base
                     racket/contract
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/private/connection-manager
                     web-server/servlet/servlet-structs
                     web-server/web-server
                     json))

@title{Datastar Racket SDK}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[datastar]

This package provides a Racket SDK for working with @link["https://data-star.dev"]{Datastar}.

@section{Usage}

Example usage (adapted from the @link["https://github.com/starfederation/datastar-go?tab=readme-ov-file#usage"]{Datastar Go SDK docs})

@codeblock{
#lang racket

(require datastar
         web-server/http
         web-server/safety-limits
         web-server/web-server
         json)

(struct store (message count) #:transparent)

(define (handler req)
  ; Read signals from the request
  (define signals-data (read-signals req))
  (define current-store
    (store (hash-ref signals-data 'message "")
           (hash-ref signals-data 'count 0)))

  ; Create a Server-Sent Event response
  (datastar-sse req
    (lambda (sse)
      ; Patch elements in the DOM
      (patch-elements sse "<div id=\"output\">Hello from Datastar!</div>")

      ; Remove elements from the DOM
      (remove-elements sse "#temporary-element")

      ; Patch signals (update client-side state)
      (patch-signals sse (hash 'message "Updated message"
                                'count (+ (store-count current-store) 1)))

      ; Execute JavaScript in the browser
      (execute-script sse "console.log(\"Hello from server!\")")

      ; Redirect the browser
      (redirect sse "/new-page"))))

(define stop
  (serve
   #:dispatch (dispatch/datastar handler)
   #:listen-ip "127.0.0.1"
   #:port 8000
   #:connection-close? #t
   #:safety-limits (make-safety-limits
                    #:response-timeout +inf.0
                    #:response-send-timeout +inf.0)))

(with-handlers ([exn:break? (lambda (e) (stop))])
  (sync/enable-break never-evt))
}

For more advanced usage with streaming updates, use the callback to loop directly.
If the client disconnects or a send fails, an exception is raised which
@racket[datastar-sse] catches automatically, triggering cleanup via @racket[on-close]:

@codeblock{
(define (streaming-handler req)
  (datastar-sse req
    (lambda (sse)
      (for ([i (in-range 10)])
        (patch-elements sse
          (format "<div id=\"counter\">Count: ~a</div>" i))
        (patch-signals sse (hash 'counter i))
        (sleep 1)))))
}

You can also use the @racket[#:on-close] callback for cleanup when the connection ends:

@codeblock{
(define connections (mutable-set))

(define (streaming-handler req)
  (datastar-sse req
    (lambda (sse)
      (set-add! connections sse)
      (console-log sse "connected"))
    #:on-close
    (lambda (sse)
      (set-remove! connections sse))))
}

For more examples, see the @link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory} on GitHub.

@section{Dispatcher}

@defproc[(dispatch/datastar [servlet (-> request? can-be-response?)]) (-> connection? request? any)]{
Wraps a servlet into a @link["https://docs.racket-lang.org/web-server-internal/dispatch.html"]{dispatcher}
for use with @racket[serve]. This is the recommended way to run Datastar applications.

When used, @racket[datastar-sse] automatically monitors the underlying TCP connection and
detects client disconnections immediately. This ensures that the @racket[on-open] callback
is interrupted and @racket[on-close] fires as soon as the client goes away, rather than
waiting for the next failed write.

@racket[dispatch/datastar] can be composed with the standard web server dispatchers
(@tt{dispatch-sequencer}, @tt{dispatch-filter}, @tt{dispatch-files}, etc.) for routing
and static file serving. See the
@link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory}
for a full example.

If @racket[dispatch/servlet] is used instead of @racket[dispatch/datastar], everything
still works but immediate disconnect detection is not available; disconnections will only
be detected on the next failed write.
}

@section{SSE Generator}

@defproc[(datastar-sse [request request?]
                       [on-open (-> sse? any)]
                       [#:on-close on-close (or/c (-> sse? any) #f) #f]
                       [#:write-profile write-profile write-profile? basic-write-profile]) response?]{
Creates an HTTP response with proper SSE headers. Calls @racket[on-open] with a fresh
@racket[sse?] generator that can be used to send events to the client. When @racket[on-open]
returns (or raises an exception), the connection is closed and @racket[on-close] is called
if provided.

When the server is set up with @racket[dispatch/datastar], client disconnections are detected
immediately: the SDK monitors the TCP input port and interrupts @racket[on-open] as soon as
the client goes away, ensuring prompt cleanup via @racket[on-close].

The @racket[write-profile] controls how SSE bytes are written to the connection. The default
@racket[basic-write-profile] writes uncompressed. Custom write profiles can add compression
(see @secref["write-profiles"]). If the client does not advertise support for the profile's
content encoding in @tt{Accept-Encoding}, the SDK automatically falls back to
@racket[basic-write-profile].

@bold{Important:} When using @racket[serve], two settings are required for SSE to work correctly:

@itemlist[
  @item{@racket[#:connection-close?] must be @racket[#t]. Without this, the web server uses
  chunked transfer encoding with an internal pipe that silently absorbs writes to dead
  connections, preventing disconnect detection from working and @racket[on-close] from firing.}

  @item{@racket[#:safety-limits] must disable both @racket[#:response-timeout] and
  @racket[#:response-send-timeout] (set to @racket[+inf.0]). The defaults of 60 seconds
  will kill idle SSE connections. @racket[#:response-timeout] limits the total time a handler
  can run, and @racket[#:response-send-timeout] limits the time between successive writes.
  Both must be infinite for long-lived SSE connections.}
]
}

@defproc[(sse? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an SSE generator created by @racket[datastar-sse].
}

@defproc[(close-sse [sse sse?]) void?]{
Explicitly closes the SSE connection. This is called automatically when the @racket[on-open]
callback returns, but can be called earlier if needed. Safe to call multiple times.
}

@defproc[(sse-closed? [sse sse?]) boolean?]{
Returns @racket[#t] if the SSE connection is closed, either because @racket[close-sse]
was called or because the underlying output port was closed (e.g., client disconnected).
This is a non-destructive check that does not attempt to write to the connection.
}

@defproc[(call-with-sse-lock [sse sse?] [thunk (-> any)]) any]{
Holds the SSE generator's lock for the duration of @racket[thunk], preventing
concurrent sending of SSE events. This ensures that multiple sends are delivered
as an atomic batch without events from other threads interleaving.

This is only needed when multiple threads send through the same @racket[sse?]
generator. If each generator is used by a single thread (the common case),
individual sends are already thread-safe and this is not needed.

The lock is re-entrant: @racket[sse-send] (and therefore all send functions like
@racket[patch-elements], @racket[patch-signals], etc.) uses @racket[call-with-sse-lock]
internally, so calling them inside a locked region does not deadlock.

@codeblock{
(call-with-sse-lock sse
  (lambda ()
    (patch-elements sse "<div id=\"a\">part 1</div>")
    (patch-elements sse "<div id=\"b\">part 2</div>")
    (patch-signals sse (hash 'status "updated"))))
}

If an exception is raised inside @racket[thunk], the lock is released via
@racket[dynamic-wind], so subsequent sends can still proceed.
}

@defform[(with-sse-lock sse body ...)]{
Syntax form that wraps @racket[body ...] in a call to @racket[call-with-sse-lock].

@codeblock{
(with-sse-lock sse
  (patch-elements sse "<div id=\"a\">part 1</div>")
  (patch-elements sse "<div id=\"b\">part 2</div>")
  (patch-signals sse (hash 'status "updated")))
}
}

@section{Sending Events}

All send functions take an @racket[sse?] generator as their first argument. If the
connection is closed or an I/O error occurs, an exception is raised. Within
@racket[datastar-sse], these exceptions are caught automatically, triggering cleanup
via @racket[on-close]. Sends are thread-safe: multiple threads can send events through
the same generator and delivery order is serialized. If multiple threads share a
single generator, use @racket[with-sse-lock] to send a group of events without
interleaving.

@defproc[(patch-elements [sse sse?]
                          [elements (or/c string? #f)]
                          [#:selector selector (or/c string? #f) #f]
                          [#:mode mode element-patch-mode/c #f]
                          [#:namespace namespace element-namespace/c #f]
                          [#:use-view-transitions use-view-transitions (or/c boolean? #f) #f]
                          [#:event-id event-id (or/c string? #f) #f]
                          [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Patches HTML elements into the DOM.

The @racket[#:mode] parameter controls how elements are patched. Use the named constants:
@racket[patch-mode-outer] (morph entire element, default), @racket[patch-mode-inner]
(morph inner HTML), @racket[patch-mode-replace] (replace entire element),
@racket[patch-mode-prepend], @racket[patch-mode-append], @racket[patch-mode-before],
@racket[patch-mode-after], and @racket[patch-mode-remove].
When @racket[#f] or @racket[patch-mode-outer], the mode data line is omitted.

The @racket[#:namespace] parameter specifies the namespace for creating new elements:
@racket[element-namespace-html] (default), @racket[element-namespace-svg], or
@racket[element-namespace-mathml].

@codeblock{
(patch-elements sse "<div id=\"out\">hello</div>")
(patch-elements sse "<svg>...</svg>" #:namespace element-namespace-svg)
(patch-elements sse "<li>item</li>" #:selector "#list" #:mode patch-mode-append)
}
}

@defproc[(remove-elements [sse sse?]
                           [selector string?]
                           [#:event-id event-id (or/c string? #f) #f]
                           [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Removes elements from the DOM by CSS selector. This is a convenience function that calls
@racket[patch-elements] with @racket[patch-mode-remove].
}

@defproc[(patch-signals [sse sse?]
                         [signals (or/c string? jsexpr?)]
                         [#:event-id event-id (or/c string? #f) #f]
                         [#:only-if-missing only-if-missing (or/c boolean? #f) #f]
                         [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Patches signals into the signal store using RFC 7386 JSON Merge Patch semantics. Supports
add/update operations, removal by setting to @tt{null}, and nested recursive patching.
}

@defproc[(execute-script [sse sse?]
                          [script string?]
                          [#:auto-remove auto-remove boolean? #t]
                          [#:attributes attributes (or/c (hash/c symbol? any/c) (listof string?) #f) #f]
                          [#:event-id event-id (or/c string? #f) #f]
                          [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Executes JavaScript in the browser by injecting a script tag. The script is automatically
removed after execution unless @racket[auto-remove] is @racket[#f].
}

@defproc[(redirect [sse sse?]
                    [location string?]) void?]{
Redirects the browser to a new location using @tt{window.location}. This is a convenience
function that calls @racket[execute-script].
}

@defproc[(console-log [sse sse?]
                       [message string?]) void?]{
Logs a message to the browser console via @tt{console.log}. The message is automatically
quoted as a JavaScript string. This is a convenience function that calls @racket[execute-script].
}

@defproc[(console-error [sse sse?]
                         [message string?]) void?]{
Same as @racket[console-log] but uses @tt{console.error}.
}

@defproc[(replace-url [sse sse?]
                       [location string?]) void?]{
Updates the browser URL without navigating, using @tt{window.history.replaceState}.
This is a convenience function that calls @racket[execute-script].
}

@section{Reading Signals}

@defproc[(read-signals [request request?]) jsexpr?]{
Parses incoming signal data from the browser. For GET requests, extracts data from the
@tt{datastar} query parameter. For other methods, parses the request body as JSON. This is
a standalone function that operates on the request and does not require an SSE generator.
}

@defproc[(datastar-request? [request request?]) boolean?]{
Returns @racket[#t] if the request has a @tt{Datastar-Request: true} header, meaning
it came from a Datastar action. The check is case-insensitive.

@codeblock{
(define (handler req)
  (if (datastar-request? req)
      (datastar-sse req (lambda (sse) (patch-elements sse "<div id=\"out\">SSE</div>")))
      (response/xexpr '(html (body (div ((id "out")) "Initial"))))))
}
}

@section{Action Helpers}

Convenience functions for generating Datastar
@link["https://data-star.dev/reference/action_plugins"]{backend action} attribute strings.

@codeblock{
`(main ((id "main") ,(ds:init (sse-get "/events")))
       (form (,(ds:on "submit" (sse-post "/todo/create")))
             (button (,(ds:on "click" (sse-post (format "/todo/delete/~a" tid))))
                     "Delete")))
}

@defproc[(sse-get [url string?] [args string? #f]) string?]{
Returns a @tt{@"@"get} action string. When @racket[args] is provided, it is included as a
second argument.

@codeblock{
(sse-get "/events")           ; => "@"@"get('/events')"
(sse-get "/events" "{includeLocal: true}")
                              ; => "@"@"get('/events', {includeLocal: true})"
}
}

@defproc[(sse-post [url string?] [args string? #f]) string?]{
Returns a @tt{@"@"post} action string.
}

@defproc[(sse-put [url string?] [args string? #f]) string?]{
Returns a @tt{@"@"put} action string.
}

@defproc[(sse-patch [url string?] [args string? #f]) string?]{
Returns a @tt{@"@"patch} action string.
}

@defproc[(sse-delete [url string?] [args string? #f]) string?]{
Returns a @tt{@"@"delete} action string.
}

@section{Attribute Helpers}

Functions for generating Datastar @tt{data-*}
@link["https://data-star.dev/reference/attributes"]{HTML attributes} as x-expression
attribute pairs. Each function returns @racket[(list 'attr-name "value")] which drops
directly into x-expression templates via unquote:

@codeblock{
;; Without attribute helpers:
`(input ((data-bind:filter "")
         (data-on:input "@"@"post('/search')")))

;; With attribute helpers:
`(input (,(ds:bind "filter")
         ,(ds:on "input" (sse-post "/search") #:debounce "250ms")))
}

Modifiers (debounce, throttle, once, etc.) are expressed as keyword arguments rather
than method chaining. Boolean modifiers take @racket[#t]; parameterized modifiers take
their value directly.

If you want to use a prefix other than @tt{ds:}, use @racket[prefix-in]:
@racket[(require (prefix-in my: datastar/attributes))].

For complete documentation of every attribute helper with examples, see the source at @link["https://github.com/jaybonthius/datastar-racket/tree/main/datastar/attributes.rkt"]{GitHub repository}.

@section[#:tag "write-profiles"]{Write Profiles}

A write profile controls how SSE bytes are written to the underlying connection. This
abstraction allows pluggable compression (or other transforms) without changing the
event-sending code. The @racket[#:write-profile] parameter on @racket[datastar-sse]
accepts any @racket[write-profile?] value.

@defproc[(write-profile? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a write profile.
}

@defthing[basic-write-profile write-profile?]{
The default write profile. Writes SSE events uncompressed with no transformation.
}

For compression support, see the @tt{datastar-brotli} package, which provides a
Brotli write profile.

@section{Constants}

@subsection{CDN URLs}

@defthing[datastar-cdn-url string?]{
URL for the Datastar JavaScript bundle on the jsDelivr CDN, derived from
@racket[datastar-version]. Use this instead of hardcoding the CDN URL:

@codeblock{
`(script ((type "module") (src ,datastar-cdn-url)))
}
}

@defthing[datastar-cdn-map-url string?]{
URL for the source map corresponding to @racket[datastar-cdn-url].
}

@defthing[datastar-version string?]{
The Datastar version string (e.g., @racket["1.0.0-RC.8"]). Used to derive
@racket[datastar-cdn-url] and @racket[datastar-cdn-map-url].
}

@subsection{Patch Modes}

Named constants for the @racket[#:mode] parameter of @racket[patch-elements].
Values are symbols (@racket['outer], @racket['inner], etc.).

@defthing[patch-mode-outer symbol?]{Morph the entire element (default).}
@defthing[patch-mode-inner symbol?]{Replace inner HTML.}
@defthing[patch-mode-remove symbol?]{Remove the element.}
@defthing[patch-mode-replace symbol?]{Replace the element without morphing.}
@defthing[patch-mode-prepend symbol?]{Prepend inside the element.}
@defthing[patch-mode-append symbol?]{Append inside the element.}
@defthing[patch-mode-before symbol?]{Insert before the element.}
@defthing[patch-mode-after symbol?]{Insert after the element.}

@subsection{Element Namespaces}

Named constants for the @racket[#:namespace] parameter of @racket[patch-elements].
Values are symbols (@racket['html], @racket['svg], @racket['mathml]).

@defthing[element-namespace-html symbol?]{HTML namespace (default).}
@defthing[element-namespace-svg symbol?]{SVG namespace.}
@defthing[element-namespace-mathml symbol?]{MathML namespace.}

@subsection{Contracts}

@defthing[element-patch-mode/c flat-contract?]{
Contract for valid patch modes: any of the @racket[patch-mode-*] constants or @racket[#f].
}

@defthing[element-namespace/c flat-contract?]{
Contract for valid namespaces: any of the @racket[element-namespace-*] constants or @racket[#f].
}

@subsection{Defaults}

@defthing[default-element-patch-mode symbol?]{
The default element patch mode (@racket['outer]). When @racket[#:mode] is
@racket[patch-mode-outer] or @racket[#f], the mode data line is omitted.
}

@defthing[default-element-namespace symbol?]{
The default element namespace (@racket['html]).
}

@subsection{Event Types}

@defthing[event-type-patch-elements symbol?]{
SSE event type for patching elements: @racket['datastar-patch-elements].
}

@defthing[event-type-patch-signals symbol?]{
SSE event type for patching signals: @racket['datastar-patch-signals].
}

@section{Testing Utilities}

@defmodule[datastar/testing]

Mock SSE generators for testing Datastar handlers without a real HTTP connection.
Require this module separately from @racket[datastar].

@defproc[(make-mock-sse) (values sse? (-> string?))]{
Creates a mock @racket[sse?] generator that works with all the normal send functions
(@racket[patch-elements], @racket[patch-signals], etc.) but doesn't touch the network.
Returns two values: the generator, and a thunk that returns all the SSE text that
has been sent through it so far.

@codeblock{
(require datastar datastar/testing)

(define-values (sse get-output) (make-mock-sse))
(patch-elements sse "<div id=\"x\">hi</div>")
(get-output)
;; => "event: datastar-patch-elements\ndata: elements <div id=\"x\">hi</div>\n\n"
}
}

@defproc[(make-recording-sse) (values sse? (-> (listof sse-event?)))]{
Like @racket[make-mock-sse], but instead of returning raw text, the retrieval thunk
returns a list of @racket[sse-event] structs, one per event sent.

The events are parsed from the same SSE text that would go over the wire, so they
reflect exactly what a real client would receive.

@codeblock{
(require datastar datastar/testing)

(define-values (sse get-events) (make-recording-sse))
(patch-elements sse "<div>test</div>")
(patch-signals sse (hash 'x 1))
(define events (get-events))
(length events)            ;; => 2
(sse-event-type (first events))  ;; => "datastar-patch-elements"
(sse-event-type (second events)) ;; => "datastar-patch-signals"
}
}

@defstruct*[sse-event ([type string?]
                        [id (or/c string? #f)]
                        [retry (or/c exact-nonnegative-integer? #f)]
                        [data-lines (listof string?)])
                       #:transparent]{
A parsed SSE event. Transparent, so @racket[check-equal?] works on it directly.

@itemlist[
  @item{@racket[type] -- event type string (e.g. @racket["datastar-patch-elements"]).}
  @item{@racket[id] -- event ID, or @racket[#f] if none was set.}
  @item{@racket[retry] -- retry duration in milliseconds, or @racket[#f] if the default
  was used (defaults are omitted from SSE output).}
  @item{@racket[data-lines] -- list of data line contents without the @tt{data: } prefix.
  For example, @racket['("elements <div>hello</div>")].}
]
}
