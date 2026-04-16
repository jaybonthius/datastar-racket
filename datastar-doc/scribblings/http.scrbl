#lang scribble/doc

@(require (for-label datastar
                     json
                     net/tcp-sig
                     racket/base
                     racket/contract
                     racket/unit
                     web-server-compress
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/safety-limits
                     web-server/servlet-dispatch
                     web-server/web-server
                     xml)
          "datastar.rkt")

@title[#:tag "http"]{HTTP}

@defmodule[datastar/http]

Provides helper functions for Datastar backend actions:

@itemlist[
  @item{@bold{@seclink["requests"]{Requests}} --- Parse Datastar action request signals from incoming HTTP requests.}
  @item{@bold{@seclink["responses"]{Responses}} --- Construct Datastar-compatible responses, including @seclink["sse"]{SSE event streams} and @seclink["one-shot-responses"]{one-shot HTTP responses}.}
]


@section[#:tag "requests"]{Requests}

@defmodule[datastar/http/request]

@defproc[(read-signals [request request?]) jsexpr?]{
Parses incoming signal data from the browser. For GET requests, extracts data from the @tt{datastar} query parameter. For other methods, parses the request body as JSON.
}

@defproc[(datastar-request? [request request?]) boolean?]{
Returns @racket[#t] if the request has a @tt{Datastar-Request: true} header, meaning it came from a Datastar action. The check is case-insensitive.
}

@section[#:tag "responses"]{Responses}

Datastar response helpers are split across two modules:

@itemlist[
  @item{@bold{@seclink["sse"]{SSE event streams}} (@racketmodname[datastar/http/sse]) --- @tt{text/event-stream} responses for Datastar events, from single immediate updates to long-lived streams.}
  @item{@bold{@seclink["one-shot-responses"]{One-shot HTTP responses}} (@racketmodname[datastar/http/response]) --- non-SSE responses with Datastar-aware headers using @tt{text/html}, @tt{application/json}, or @tt{text/javascript}.}
]

The @link["https://data-star.dev/guide/the_tao_of_datastar#sse-responses"]{Tao of Datastar} recommends SSE event streams as the default for most backend actions.

@bold{Serializer omission policy:} default-valued Datastar fields are omitted only by wire serializers in these modules (SSE event serialization in @racketmodname[datastar/http/sse] and one-shot response header serialization in @racketmodname[datastar/http/response]).

@subsection[#:tag "sse"]{SSE}

@defmodule[datastar/http/sse]

Functions for creating and managing @link["https://data-star.dev/reference/sse_events"]{Datastar SSE events}. See the @link["https://data-star.dev/reference/sse_events"]{Datastar SSE events reference} for full details on event types and their data lines.

@subsubsection{Server Setup}

@defthing[datastar-tcp@ (unit/c (import) (export tcp^))]{
A @racket[tcp^] unit for use with @racket[serve]'s @racket[#:tcp@] parameter. Enables instant client disconnect detection for SSE connections.

When provided, @racket[datastar-sse] monitors the underlying TCP input port and interrupts @racket[on-open] as soon as the client goes away, ensuring prompt cleanup via @racket[on-close]. Without it, everything still works but disconnections are only detected on the next failed write.

Use @racket[dispatch/servlet] from @racket[web-server/servlet-dispatch] to convert your servlet into a dispatcher for @racket[serve]:

@codeblock{
(serve #:dispatch (dispatch/servlet handler)
       #:tcp@"@" datastar-tcp@"@"
       #:connection-close? #t
       #:safety-limits (make-safety-limits #:response-timeout +inf.0
                                           #:response-send-timeout +inf.0))
}

@racket[dispatch/servlet] composes with the standard web server dispatchers (@tt{dispatch-sequencer}, @tt{dispatch-filter}, @tt{dispatch-files}, etc.) for routing and static file serving. See the @link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory} for a full example.
}

@subsubsection{SSE Generator}

@defproc[(datastar-sse [on-open (-> sse? any)]
                       [#:on-close on-close (-> sse? any)]) response?]{
Creates an HTTP response with proper SSE headers. Calls @racket[on-open] with a fresh @racket[sse?] generator that can be used to send events to the client. When @racket[on-open] returns (or raises an exception), the connection is closed and @racket[on-close] is called if provided.

When the server is set up with @racket[datastar-tcp@], client disconnections are detected immediately: the SDK monitors the TCP input port and interrupts @racket[on-open] as soon as the client goes away, ensuring prompt cleanup via @racket[on-close].

@bold{Important:} When using @racket[serve], the following settings are required for SSE to work correctly:

@itemlist[
  @item{@racket[#:tcp@] should be @racket[datastar-tcp@] for instant disconnect detection. Without this, disconnections are only detected on the next failed write, which means @racket[on-close] may not fire promptly if the handler is blocked waiting for data.}

  @item{@racket[#:connection-close?] must be @racket[#t]. Without this, the web server uses chunked transfer encoding with an internal pipe that silently absorbs writes to dead connections, preventing disconnect detection from working and @racket[on-close] from firing.}

  @item{@racket[#:safety-limits] must disable both @racket[#:response-timeout] and @racket[#:response-send-timeout] (set to @racket[+inf.0]). The defaults of 60 seconds will kill idle SSE connections. @racket[#:response-timeout] limits the total time a handler can run, and @racket[#:response-send-timeout] limits the time between successive writes. Both must be infinite for long-lived SSE connections.}
]
}

@defproc[(sse? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an SSE generator value supported by this SDK (including values produced by @racket[datastar-sse] and the testing helpers in @racketmodname[datastar/testing]).
}

@defproc[(close-sse [sse sse?]) void?]{
Explicitly closes the SSE connection. This is called automatically when the @racket[on-open] callback returns, but can be called earlier if needed. Safe to call multiple times.
}

@defproc[(sse-closed? [sse sse?]) boolean?]{
Returns @racket[#t] if the SSE connection is closed, either because @racket[close-sse] was called or because the underlying output port was closed (e.g., client disconnected). This is a non-destructive check that does not attempt to write to the connection.
}

@subsubsection{Locking}

All send functions are thread-safe: multiple threads can send events through the same generator and delivery order is serialized. If multiple threads share a single generator, use @racket[with-sse-lock] to send a group of events without interleaving.

@defproc[(call-with-sse-lock [sse sse?] [thunk (-> any)]) any]{
Holds the SSE generator's lock for the duration of @racket[thunk], preventing concurrent sending of SSE events. This ensures that multiple sends are delivered as an atomic batch without events from other threads interleaving.

Use this only when multiple threads send through the same @racket[sse?] generator. If each generator is used by a single thread (the common case), individual sends are already thread-safe and this locking is unnecessary.

The lock is re-entrant: all send functions (@racket[patch-elements], @racket[patch-signals], etc.) use @racket[call-with-sse-lock] internally, so calling them inside a locked region does not deadlock.

@racketblock[
(call-with-sse-lock sse
                    (lambda ()
                      (patch-elements/xexprs sse '(div ((id "a")) "part 1"))
                      (patch-elements/xexprs sse '(div ((id "b")) "part 2"))
                      (patch-signals sse (hash 'status "updated"))))
]

If an exception is raised inside @racket[thunk], the lock is released via @racket[dynamic-wind], so subsequent sends can still proceed.
}

@defform[(with-sse-lock sse body ...)]{
Syntax form that wraps @racket[body ...] in a call to @racket[call-with-sse-lock].

@racketblock[
(with-sse-lock sse
               (patch-elements/xexprs sse '(div ((id "a")) "part 1"))
               (patch-elements/xexprs sse '(div ((id "b")) "part 2"))
               (patch-signals sse (hash 'status "updated")))
]
}

@subsubsection[#:tag "signals"]{Signals}

@defproc[(patch-signals [sse sse?]
                         [signals (or/c string? jsexpr?)]
                         [#:event-id event-id string?]
                         [#:only-if-missing? only-if-missing? boolean? #f]
                         [#:retry-duration retry-duration exact-positive-integer?]) void?]{
Sends a @link["https://data-star.dev/reference/sse_events#datastar-patch-signals"]{@tt{datastar-patch-signals}} SSE event that patches signals into the existing signals on the page. The @racket[#:only-if-missing?] option determines whether to update each signal only if a signal with that name does not yet exist.

Serializer behavior omits default-equivalent wire fields: @tt{data: onlyIfMissing ...} is emitted only when @racket[#:only-if-missing?] is @racket[#t], and @tt{retry: ...} is emitted only when @racket[#:retry-duration] is non-default.
}

@defproc[(remove-signals [sse sse?]
                          [signal-path-or-paths (or/c string? (listof string?))]
                          [#:event-id event-id string?]
                          [#:retry-duration retry-duration exact-positive-integer?]) void?]{
Convenience wrapper around @racket[patch-signals] for removing one or more signal paths. Builds a patch object with each provided path set to @racket['null], then sends a @tt{datastar-patch-signals} event.

Dot notation paths are expanded to nested objects, so @racket["user.name"] becomes @tt{{"user":{"name":null}}}.
}

@subsubsection[#:tag "elements"]{Elements}

@defproc[(patch-elements [sse sse?]
                          [elements (or/c string? #f)]
                          [#:selector selector string?]
                          [#:mode mode element-patch-mode/c]
                          [#:namespace namespace element-namespace/c]
                          [#:use-view-transitions? use-view-transitions? boolean? #f]
                          [#:event-id event-id string?]
                          [#:retry-duration retry-duration exact-positive-integer?]) void?]{
Sends a @link["https://data-star.dev/reference/sse_events#datastar-patch-elements"]{@tt{datastar-patch-elements}} SSE event that patches one or more elements in the DOM. By default, Datastar morphs elements by matching top-level elements based on their ID.

The @racket[#:mode] parameter controls how elements are patched. Use symbol values; see @racket[element-patch-mode/c] for allowed values.

Serializer behavior omits default-equivalent wire fields: @tt{data: mode ...} is omitted for @racket['outer], @tt{data: namespace ...} is omitted for @racket['html], @tt{data: useViewTransition ...} is omitted for @racket[#f], and @tt{retry: ...} is omitted for the default retry duration.

@racketblock[
(patch-elements sse "<div id=\"out\">hello</div>")
(patch-elements sse "<svg>...</svg>" #:namespace 'svg)
(patch-elements sse "<li>item</li>" #:selector "#list" #:mode 'append)
]
}

@defproc[(patch-elements/xexprs [sse sse?]
                                 [xexpr-or-xexprs (or/c xexpr/c (listof xexpr/c))]
                                 [#:selector selector string?]
                                 [#:mode mode element-patch-mode/c]
                                 [#:namespace namespace element-namespace/c]
                                 [#:use-view-transitions? use-view-transitions? boolean? #f]
                                 [#:event-id event-id string?]
                                 [#:retry-duration retry-duration exact-positive-integer?]) void?]{
Like @racket[patch-elements], but accepts either a single x-expression or a list of x-expressions instead of a raw HTML string. Converts each x-expression via @racket[xexpr->string], concatenates resulting HTML fragments, and delegates to @racket[patch-elements]. If an empty list is provided, a @tt{datastar-patch-elements} event is still emitted, but with no @tt{elements} data lines.

@racketblock[
(patch-elements/xexprs sse '(div ((id "out")) "hello"))
(patch-elements/xexprs sse '(svg "...") #:namespace 'svg)
(patch-elements/xexprs sse
                       '((li "one")
                         (li "two"))
                       #:selector "#list"
                       #:mode 'append)
]
}

Contracts used by @racket[patch-elements] and @racket[patch-elements/xexprs] option arguments:

@defthing[element-patch-mode/c flat-contract?]{
Contract for valid patch modes: @racket['outer], @racket['inner], @racket['remove], @racket['replace], @racket['prepend], @racket['append], @racket['before], or @racket['after].
}

@defthing[element-namespace/c flat-contract?]{
Contract for valid namespaces: @racket['html], @racket['svg], or @racket['mathml].
}

@defproc[(remove-elements [sse sse?]
                           [selector string?]
                           [#:event-id event-id string?]
                           [#:retry-duration retry-duration exact-positive-integer?]) void?]{
Removes elements from the DOM by CSS selector. Convenience function that calls @racket[patch-elements] with @racket['remove].
}

@subsubsection[#:tag "scripts"]{Scripts}

@defproc[(execute-script [sse sse?]
                          [script string?]
                          [#:auto-remove? auto-remove? boolean? #t]
                          [#:attributes attributes (or/c (hash/c symbol? any/c) (listof string?))]
                          [#:event-id event-id string?]
                          [#:retry-duration retry-duration exact-positive-integer?]) void?]{
Sends a @tt{datastar-patch-elements} SSE event that appends a @tt{<script>} element to @tt{body}. The script element is automatically removed after execution unless @racket[auto-remove?] is @racket[#f].
}

@defproc[(redirect [sse sse?]
                    [location string?]) void?]{
Redirects the browser to a new location using @tt{window.location}. This is a convenience function that calls @racket[execute-script].
}

@defproc[(console-log [sse sse?]
                       [message string?]) void?]{
Logs a message to the browser console via @tt{console.log}. The message is automatically quoted as a JavaScript string. This is a convenience function that calls @racket[execute-script].
}

@defproc[(console-error [sse sse?]
                         [message string?]) void?]{
Logs a message to the browser console via @tt{console.error}. This is a convenience function that calls @racket[execute-script].
}

@defproc[(replace-url [sse sse?]
                       [location string?]) void?]{
Updates the browser URL without navigating, using @tt{window.history.replaceState}. This is a convenience function that calls @racket[execute-script].
}

@subsubsection[#:tag "compression"]{Compression}

To add compression, wrap your app handler with @racket[wrap-compress] from @racketmodname[web-server-compress]:

@codeblock{
(require datastar
         web-server-compress
         web-server/dispatch
         web-server/servlet-dispatch
         web-server/web-server)

(define (events-handler req)
  (datastar-sse (lambda (sse) ...)))

(define-values (app _uri) (dispatch-rules ...))

(serve #:dispatch (dispatch/servlet (wrap-compress app)) ...)
}

By default, @racket[wrap-compress] negotiates with the client's @tt{Accept-Encoding} header using server-priority order: zstd first, then Brotli. If the client doesn't support either, responses pass through uncompressed. Each SSE event is flushed to the client immediately.

To customize the encoding priority or tune per-encoding parameters:

@racketblock[
(wrap-compress app
  #:encodings '(br zstd)
  #:brotli-quality 9
  #:zstd-level 6)
]

For single-encoding use, @racket[wrap-brotli-compress] and @racket[wrap-zstd-compress] are also available. See the @racketmodname[web-server-compress] docs for the full API.

@subsection[#:tag "one-shot-responses"]{One-Shot Responses (Non-SSE)}

@defmodule[datastar/http/response]

The @link["https://data-star.dev/guide/the_tao_of_datastar#sse-responses"]{Tao of Datastar} recommends using @seclink["sse"]{SSE event streams} by default. Non-SSE responses can still be useful when:

@itemlist[
  @item{you are integrating with an existing non-SSE endpoint}
  @item{a single immediate update is sufficient and you want a regular HTTP body/headers response}
]

These helpers build regular HTTP responses for Datastar backend actions without opening an SSE stream.

@defproc[(response/datastar-elements [elements (or/c string? bytes?)]
                                     [#:selector selector string?]
                                     [#:mode mode element-patch-mode/c]
                                     [#:namespace namespace element-namespace/c]
                                     [#:use-view-transitions? use-view-transitions? boolean? #f]
                                     [#:status status exact-positive-integer? 200]
                                     [#:headers headers (hash/c string? string?)])
         response?]{
Builds a @tt{text/html} response interpreted by Datastar as an elements patch.
The response body may contain one or more top-level elements.

Optional Datastar headers:
@itemlist[
  @item{@tt{datastar-selector}}
  @item{@tt{datastar-mode}}
  @item{@tt{datastar-namespace}}
  @item{@tt{datastar-use-view-transition}}
]

Serializer behavior matches SSE omission defaults: @tt{datastar-mode} is omitted for @racket['outer], @tt{datastar-namespace} is omitted for @racket['html], and @tt{datastar-use-view-transition} is omitted for @racket[#f].
}

@defproc[(response/datastar-elements/xexprs [xexpr-or-xexprs (or/c xexpr/c (listof xexpr/c))]
                                            [#:selector selector string?]
                                            [#:mode mode element-patch-mode/c]
                                            [#:namespace namespace element-namespace/c]
                                            [#:use-view-transitions? use-view-transitions? boolean? #f]
                                            [#:status status exact-positive-integer? 200]
                                            [#:headers headers (hash/c string? string?)])
         response?]{
Like @racket[response/datastar-elements], but takes x-expressions and renders them to HTML first.
}

@defproc[(response/datastar-signals [signals (or/c string? bytes? jsexpr?)]
                                    [#:only-if-missing? only-if-missing? boolean? #f]
                                    [#:status status exact-positive-integer? 200]
                                    [#:headers headers (hash/c string? string?)])
         response?]{
Builds an @tt{application/json} response interpreted by Datastar as a signals patch.

Serializer behavior matches SSE omission defaults: @tt{datastar-only-if-missing} is emitted only when @racket[#:only-if-missing?] is @racket[#t] (explicit @racket[#f] is omitted).
}

@defproc[(response/datastar-script [script (or/c string? bytes?)]
                                   [#:attributes attributes (hash/c (or/c symbol? string?) any/c)]
                                   [#:status status exact-positive-integer? 200]
                                   [#:headers headers (hash/c string? string?)])
         response?]{
Builds a @tt{text/javascript} response interpreted by Datastar as executable script.

When provided, @racket[#:attributes] is JSON-encoded into the @tt{datastar-script-attributes} response header.
}
