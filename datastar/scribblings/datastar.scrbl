#lang scribble/doc

@(require scribble/manual
          (for-label datastar
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
   #:connection-close? #t))

(with-handlers ([exn:break? (lambda (e) (stop))])
  (sync/enable-break never-evt))
}

For more advanced usage with streaming updates, use the callback to loop directly.
Send functions return @racket[#t] on success and @racket[#f] when the client has
disconnected, so you can use the return value to stop the loop:

@codeblock{
(define (streaming-handler req)
  (datastar-sse req
    (lambda (sse)
      (let loop ([i 0])
        (when (and (< i 10)
                   (patch-elements sse
                     (format "<div id=\"counter\">Count: ~a</div>" i)))
          (patch-signals sse (hash 'counter i))
          (sleep 1)
          (loop (+ i 1)))))))
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

@bold{Important:} When using @racket[serve], the @racket[#:connection-close?] setting must be
@racket[#t] for SSE to work correctly. Without this, the web server uses chunked transfer
encoding with an internal pipe that silently absorbs writes to dead connections, preventing
send functions from returning @racket[#f], disconnect detection from working, and
@racket[on-close] from firing.
}

@defproc[(sse? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an SSE generator created by @racket[datastar-sse].
}

@defproc[(close-sse [sse sse?]) void?]{
Explicitly closes the SSE connection. This is called automatically when the @racket[on-open]
callback returns, but can be called earlier if needed. Safe to call multiple times.
}

@section{Sending Events}

All send functions take an @racket[sse?] generator as their first argument and return
@racket[#t] on success or @racket[#f] if the connection is closed or an I/O error occurs.
Sends are thread-safe: multiple threads can send events through the same generator and
delivery order is serialized.

@defproc[(patch-elements [sse sse?]
                          [elements (or/c string? #f)]
                          [#:selector selector (or/c string? #f) #f]
                          [#:mode mode (or/c "outer" "inner" "remove" "replace" "prepend" "append" "before" "after" #f) #f]
                          [#:namespace namespace (or/c "html" "svg" "mathml" #f) #f]
                          [#:use-view-transitions use-view-transitions (or/c boolean? #f) #f]
                          [#:event-id event-id (or/c string? #f) #f]
                          [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) boolean?]{
Patches HTML elements into the DOM.

The @racket[#:mode] parameter controls how elements are patched. Valid modes are
@racket["outer"] (morph entire element, default), @racket["inner"] (morph inner HTML),
@racket["replace"] (replace entire element), @racket["prepend"], @racket["append"],
@racket["before"], @racket["after"], and @racket["remove"]. When @racket[#f] or
@racket["outer"], the mode data line is omitted.

The @racket[#:namespace] parameter specifies the namespace for creating new elements:
@racket["html"] (default), @racket["svg"], or @racket["mathml"].
}

@defproc[(remove-elements [sse sse?]
                           [selector string?]
                           [#:event-id event-id (or/c string? #f) #f]
                           [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) boolean?]{
Removes elements from the DOM by CSS selector. This is a convenience function that calls
@racket[patch-elements] with mode @tt{remove}.
}

@defproc[(patch-signals [sse sse?]
                         [signals (or/c string? jsexpr?)]
                         [#:event-id event-id (or/c string? #f) #f]
                         [#:only-if-missing only-if-missing (or/c boolean? #f) #f]
                         [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) boolean?]{
Patches signals into the signal store using RFC 7386 JSON Merge Patch semantics. Supports
add/update operations, removal by setting to @tt{null}, and nested recursive patching.
}

@defproc[(execute-script [sse sse?]
                          [script string?]
                          [#:auto-remove auto-remove boolean? #t]
                          [#:attributes attributes (or/c (hash/c symbol? any/c) (listof string?) #f) #f]
                          [#:event-id event-id (or/c string? #f) #f]
                          [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) boolean?]{
Executes JavaScript in the browser by injecting a script tag. The script is automatically
removed after execution unless @racket[auto-remove] is @racket[#f].
}

@defproc[(redirect [sse sse?]
                    [location string?]) boolean?]{
Redirects the browser to a new location using @tt{window.location}. This is a convenience
function that calls @racket[execute-script].
}

@defproc[(console-log [sse sse?]
                       [message string?]) boolean?]{
Logs a message to the browser console via @tt{console.log}. The message is automatically
quoted as a JavaScript string. This is a convenience function that calls @racket[execute-script].
}

@section{Reading Signals}

@defproc[(read-signals [request request?]) jsexpr?]{
Parses incoming signal data from the browser. For GET requests, extracts data from the
@tt{datastar} query parameter. For other methods, parses the request body as JSON. This is
a standalone function that operates on the request and does not require an SSE generator.
}

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

@subsection{Defaults}

@defthing[DEFAULT-ELEMENT-PATCH-MODE string? #:value "outer"]{
The default element patch mode. When a @racket[#:mode] of @racket["outer"] or @racket[#f] is
passed to @racket[patch-elements], the mode data line is omitted from the SSE event.
}

@defthing[DEFAULT-ELEMENT-NAMESPACE string? #:value "html"]{
The default element namespace.
}

@subsection{Event Types}

@defthing[EVENT-TYPE-PATCH-ELEMENTS string? #:value "datastar-patch-elements"]{
An event for patching HTML elements into the DOM.
}

@defthing[EVENT-TYPE-PATCH-SIGNALS string? #:value "datastar-patch-signals"]{
An event for patching signals.
}
