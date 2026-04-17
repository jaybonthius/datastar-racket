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

Provides Datastar backend HTTP modules for reading incoming signals and producing SSE or one-shot responses.

@itemlist[
  @item{@bold{Requests} (@racketmodname[datastar/http/request]) --- Parse Datastar action request signals from incoming HTTP requests.}
@item{@bold{SSE event streams} (@racketmodname[datastar/http/sse]) --- @tt{text/event-stream} responses for Datastar events, from single immediate updates to long-lived streams.}
  @item{@bold{One-shot HTTP responses} (@racketmodname[datastar/http/response]) --- non-SSE responses with Datastar-aware headers using @tt{text/html}, @tt{application/json}, or @tt{text/javascript}.}
]

For protocol context, see @link["https://data-star.dev/guide/backend_requests"]{Datastar Backend Requests} and @link["https://data-star.dev/reference/actions#response-handling"]{Datastar Response Handling}.

@section[#:tag "requests"]{Requests}

@defmodule[datastar/http/request]

Read Datastar request payloads from incoming backend action calls. For request/transport details, see @link["https://data-star.dev/guide/backend_requests"]{Datastar Backend Requests}.

@defproc[(read-signals [request request?]) jsexpr?]{
Parses incoming signal data from the browser. For GET and DELETE requests, extracts data from the @tt{datastar} query parameter. For other methods, parses the request body as JSON.
}

@defproc[(datastar-request? [request request?]) boolean?]{
Returns @racket[#t] if the request has a @tt{Datastar-Request: true} header, meaning it came from a Datastar action. The check is case-insensitive.
}


@section[#:tag "sse"]{SSE}

@defmodule[datastar/http/sse]

Send Datastar SSE events from backend actions. See the @link["https://data-star.dev/reference/sse_events"]{Datastar SSE events reference} for event formats and data-line semantics.

@subsection[#:tag "server-setup"]{Server Setup}

Use @racket[dispatch/servlet] from @racket[web-server/servlet-dispatch] to convert your servlet into a dispatcher, then choose a @racket[serve] configuration that matches your stream behavior.

@bold{Decision guide}

@itemlist[
  @item{@bold{Short-lived SSE} (send one/few events, then return): defaults usually work; these transport knobs are optional.}
  @item{@bold{Long-lived or mostly idle streams} (subscriptions, optional CQRS read-model updates): configure @racket[#:safety-limits] so response timeouts do not terminate idle handlers (for example, @racket[+inf.0], or periodic chunks/heartbeats).}
  @item{@bold{Blocked @racket[on-open] loop + prompt disconnect cleanup}: use both @racket[#:connection-close? #t] and @racket[#:tcp@ datastar-tcp@].}
]

@bold{Minimal setup}

@codeblock{
(serve #:dispatch (dispatch/servlet handler))
}

@bold{Robust setup (long-lived streams / prompt blocked-loop disconnect cleanup)}

@codeblock{
(serve #:dispatch (dispatch/servlet handler)
       #:tcp@"@" datastar-tcp@"@"
       #:connection-close? #t
       #:safety-limits (make-safety-limits #:response-timeout +inf.0
                                           #:response-send-timeout +inf.0))
}

@defthing[datastar-tcp@ (unit/c (import) (export tcp^))]{
A @racket[tcp^] unit for @racket[serve]'s @racket[#:tcp@] parameter. It enables faster disconnect detection for SSE connections.

When provided, @racket[datastar-sse] monitors the underlying TCP input port and interrupts @racket[on-open] when the client disconnects, so @racket[on-close] can run sooner.
}

@subsection{SSE Generator}

Create SSE responses and send events through an @racket[sse?] generator.

@defproc[(datastar-sse [on-open (-> sse? any)]
                       [#:on-close on-close (-> sse? any)]) response?]{
Creates an HTTP response with proper SSE headers. Calls @racket[on-open] with a fresh @racket[sse?] generator that can be used to send events to the client. When @racket[on-open] returns (or raises an exception), the connection is closed and @racket[on-close] is called if provided.

Configure @racket[serve] using @seclink["server-setup"]{Server Setup}: minimal setup for short-lived streams, robust setup for long-lived streams or fast disconnect handling.
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

@subsection{Locking}

Coordinate concurrent senders with explicit SSE locking. All send functions are thread-safe: multiple threads can send events through the same generator and delivery order is serialized. If multiple threads share a single generator, use @racket[with-sse-lock] to send a group of events without interleaving.

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

@subsection[#:tag "signals"]{Signals}

Send signal patch events to add, update, or remove client-side signals.

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

@subsection[#:tag "elements"]{Elements}

Send element patch events to morph, insert, replace, or remove DOM content.

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

@subsection[#:tag "scripts"]{Scripts}

Run browser-side scripts from SSE responses. See @link["https://data-star.dev/guide/datastar_expressions#executing-scripts"]{Datastar script execution guidance} for expression semantics.

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

@subsection[#:tag "compression"]{Compression}

Compress SSE responses by wrapping your app handler with @racket[wrap-compress] from @racketmodname[web-server-compress]:

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

@section[#:tag "one-shot-responses"]{One-Shot Responses (Non-SSE)}

@defmodule[datastar/http/response]

Build Datastar-compatible non-SSE responses when a regular HTTP body is sufficient. The @link["https://data-star.dev/guide/the_tao_of_datastar#sse-responses"]{Tao of Datastar} recommends using @seclink["sse"]{SSE event streams} by default; see @link["https://data-star.dev/reference/actions#response-handling"]{Datastar Response Handling} for client-side behavior.

Non-SSE responses can still be useful when:

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
