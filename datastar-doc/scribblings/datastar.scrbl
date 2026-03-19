#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label datastar
                     datastar/testing
                     net/tcp-sig
                     racket/base
                     racket/contract
                     racket/set
                     racket/unit
                     rackunit
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/safety-limits
                     web-server/servlet-dispatch
                     web-server/web-server
                     json
                     xml))

@(define ev (make-base-eval))
@(ev '(require datastar datastar/testing racket/list))

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
         web-server/servlet-dispatch
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
  (datastar-sse
   req
   (lambda (sse)
     ; Patch elements in the DOM
     (patch-elements/xexpr sse '(div ((id "output")) "Hello from Datastar!"))

     ; Remove elements from the DOM
     (remove-elements sse "#temporary-element")

     ; Patch signals (update client-side state)
     (patch-signals sse
                    (hash 'message
                          "Updated message"
                          'count
                          (+ (store-count current-store) 1)))

     ; Execute JavaScript in the browser
     (execute-script sse "console.log(\"Hello from server!\")")

     ; Redirect the browser
     (redirect sse "/new-page"))))

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

For more advanced usage with streaming updates, use the callback to loop directly.
If the client disconnects or a send fails, an exception is raised which
@racket[datastar-sse] catches automatically, triggering cleanup via @racket[on-close]:

@codeblock{
(define (streaming-handler req)
  (datastar-sse req
                (lambda (sse)
                  (for ([i (in-range 10)])
                    (patch-elements/xexpr sse
                                          `(div ((id "counter"))
                                                ,(format "Count: ~a" i)))
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
                #:on-close (lambda (sse) (set-remove! connections sse))))
}

For more examples, see the @link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory} on GitHub.

@section{Server Setup}

@defthing[datastar-tcp@ (unit/c (import) (export tcp^))]{
A @racket[tcp^] unit for use with @racket[serve]'s @racket[#:tcp@] parameter. Enables
instant client disconnect detection for SSE connections.

When provided, @racket[datastar-sse] monitors the underlying TCP input port and interrupts
@racket[on-open] as soon as the client goes away, ensuring prompt cleanup via
@racket[on-close]. Without it, everything still works but disconnections are only detected
on the next failed write.

Use @racket[dispatch/servlet] from @racket[web-server/servlet-dispatch] to convert your
servlet into a dispatcher for @racket[serve]:

@codeblock{
(serve #:dispatch (dispatch/servlet handler)
       #:tcp@"@" datastar-tcp@"@"
       #:connection-close? #t
       #:safety-limits (make-safety-limits #:response-timeout +inf.0
                                           #:response-send-timeout +inf.0))
}

@racket[dispatch/servlet] composes with the standard web server dispatchers
(@tt{dispatch-sequencer}, @tt{dispatch-filter}, @tt{dispatch-files}, etc.) for routing
and static file serving. See the
@link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory}
for a full example.
}

@section{Attribute Helpers}

Functions for generating Datastar @tt{data-*}
@link["https://data-star.dev/reference/attributes"]{HTML attributes} as x-expression
attribute pairs. Each function returns @racket[(list 'attr-name "value")] which drops
directly into x-expression templates via unquote. See the
@link["https://data-star.dev/reference/attributes"]{Datastar attribute reference} for
full details on each attribute's behavior.

Without attribute helpers:

@examples[#:eval ev #:label #f
`(button ((data-on:click__debounce.500ms "@post('/search')")
          (data-class:active "$enabled")
          (data-show "$query != ''")))
]

With attribute helpers:

@examples[#:eval ev #:label #f
`(button (,(ds:on "click" (sse-post "/search") #:debounce "500ms")
          ,(ds:class 'active "$enabled")
          ,(ds:show "$query != ''")))
]

Modifiers (debounce, throttle, once, etc.) are expressed as keyword arguments rather
than method chaining. Boolean modifiers take @racket[#t]; parameterized modifiers take
their value directly.

If you want to use a prefix other than @tt{ds:}, use @racket[prefix-in]:
@racket[(require (prefix-in my: datastar/attributes))].

@defproc[(ds:attr [key-or-hash (or/c symbol? string? hash?)]
                  [value-or-unused any/c #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-attr"]{@tt{data-attr}}
attribute that sets HTML attribute values based on expressions and keeps them in sync.

In the keyed form, @racket[key-or-hash] is the attribute name and
@racket[value-or-unused] is the expression.

In the hash form, @racket[key-or-hash] is a hash mapping attribute names to expressions.

Keyed form:

@examples[#:eval ev #:label #f
`(button (,(ds:attr 'disabled "$loading")))
]

Hash form:

@examples[#:eval ev #:label #f
`(button (,(ds:attr (hash "disabled" "$loading" "aria-busy" "$loading"))))
]
}

@defproc[(ds:bind [signal string?] [value string? ""]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-bind"]{@tt{data-bind}}
attribute that sets up two-way data binding between a signal and an element's value.

The @racket[signal] is the signal name. The optional @racket[value] is rarely needed
since the element's own value is used.

@examples[#:eval ev #:label #f
`(input (,(ds:bind "username")))
]

@examples[#:eval ev #:label #f
`(select (,(ds:bind "choice"))
         (option ((value "a")) "A")
         (option ((value "b")) "B"))
]
}

@defproc[(ds:class [key-or-hash (or/c symbol? string? hash?)]
                   [value-or-unused any/c #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-class"]{@tt{data-class}}
attribute that adds or removes CSS classes based on expressions.

In the keyed form, @racket[key-or-hash] is the class name and @racket[value-or-unused]
is a boolean expression.

In the hash form, @racket[key-or-hash] is a hash mapping class names to boolean
expressions.

Keyed form:

@examples[#:eval ev #:label #f
`(button (,(ds:class 'active "$selected")))
]

Hash form:

@examples[#:eval ev #:label #f
`(div (,(ds:class (hash "font-bold" "$important" "text-red" "$error"))))
]
}

@defproc[(ds:computed [key-or-hash (or/c symbol? string? hash?)]
                      [value-or-unused any/c #f]) (or/c list? (listof list?))]{
Generates a @link["https://data-star.dev/reference/attributes#data-computed"]{@tt{data-computed}}
attribute that creates read-only computed signals derived from reactive expressions.

In the keyed form, @racket[key-or-hash] is the signal name and @racket[value-or-unused]
is the expression.

In the hash form, @racket[key-or-hash] is a hash mapping signal names to expressions.
The hash form returns a @emph{list of attribute pairs}, not a single pair. Use
@racket[,@"@"] (unquote-splicing) to insert them into an x-expression.

Keyed form:

@examples[#:eval ev #:label #f
`(div (,(ds:computed 'total "$price * $quantity")))
]

Hash form (returns list of pairs, use @racket[,@"@"]):

@examples[#:eval ev #:label #f
`(div (,@(ds:computed (hash 'total "$price * $qty" 'valid "$total > 0"))))
]
}

@defproc[(ds:effect [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-effect"]{@tt{data-effect}}
attribute that runs @racket[expression] reactively whenever any signals it references change.

@examples[#:eval ev #:label #f
`(div (,(ds:effect "$total = $price * $quantity")))
]
}

@defproc[(ds:ignore [#:self self boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-ignore"]{@tt{data-ignore}}
attribute that tells Datastar to skip processing this element and its descendants.

When @racket[#:self] is @racket[#t], only the element itself is ignored; its children
are still processed.

@examples[#:eval ev #:label #f
`(div (,(ds:ignore)) "Datastar will not process this or its children")
]

@examples[#:eval ev #:label #f
`(div (,(ds:ignore #:self #t)) "Only this element is ignored")
]
}

@defproc[(ds:ignore-morph) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-ignore-morph"]{@tt{data-ignore-morph}}
attribute that tells Datastar's element patcher to skip this element and its children
when morphing. Takes no arguments.

@examples[#:eval ev #:label #f
`(div (,(ds:ignore-morph)) "This content will not be morphed")
]
}

@defproc[(ds:indicator [signal string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-indicator"]{@tt{data-indicator}}
attribute that creates a signal set to @tt{true} while a fetch request is in flight.

@examples[#:eval ev #:label #f
`(button (,(ds:indicator "loading") ,(ds:on "click" (sse-get "/data"))) "Fetch")
]

@examples[#:eval ev #:label #f
`(div (,(ds:show "$loading")) "Loading...")
]
}

@defproc[(ds:init [expression string?]
                  [#:once once boolean? #f]
                  [#:delay delay (or/c string? number? #f) #f]
                  [#:viewtransition viewtransition boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-init"]{@tt{data-init}}
attribute that runs @racket[expression] when the attribute is first processed.

@examples[#:eval ev #:label #f
(ds:init (sse-get "/events"))
]

@examples[#:eval ev #:label #f
(ds:init "$count = 0" #:once #t)
]
}

@defproc[(ds:json-signals [#:include include (or/c string? #f) #f]
                          [#:exclude exclude (or/c string? #f) #f]
                          [#:terse terse boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-json-signals"]{@tt{data-json-signals}}
attribute that sets the text content of an element to a reactive JSON representation of
the current signals. Useful for debugging.

@examples[#:eval ev #:label #f
`(pre (,(ds:json-signals)))
]

@examples[#:eval ev #:label #f
`(pre (,(ds:json-signals #:include "/^user/" #:terse #t)))
]
}

@defproc[(ds:on [event string?]
                [expression string?]
                [#:once once boolean? #f]
                [#:passive passive boolean? #f]
                [#:capture capture boolean? #f]
                [#:window window boolean? #f]
                [#:outside outside boolean? #f]
                [#:prevent prevent boolean? #f]
                [#:stop stop boolean? #f]
                [#:trust trust boolean? #f]
                [#:debounce debounce (or/c string? number? #f) #f]
                [#:debounce-leading debounce-leading boolean? #f]
                [#:debounce-notrailing debounce-notrailing boolean? #f]
                [#:throttle throttle (or/c string? number? #f) #f]
                [#:throttle-noleading throttle-noleading boolean? #f]
                [#:throttle-trailing throttle-trailing boolean? #f]
                [#:delay delay (or/c string? number? #f) #f]
                [#:viewtransition viewtransition boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on"]{@tt{data-on}}
attribute that attaches an event listener running @racket[expression] when @racket[event]
is triggered. Keyword arguments correspond to Datastar modifiers.

@examples[#:eval ev #:label #f
(ds:on "click" "$count++")
]

@examples[#:eval ev #:label #f
(ds:on "input" (sse-post "/search") #:debounce "250ms")
]

@examples[#:eval ev #:label #f
(ds:on "click" (sse-get "/data") #:once #t #:prevent #t)
]

@examples[#:eval ev #:label #f
(ds:on "keydown" "$handleKey(evt)" #:window #t)
]
}

@defproc[(ds:on-intersect [expression string?]
                          [#:once once boolean? #f]
                          [#:half half boolean? #f]
                          [#:full full boolean? #f]
                          [#:exit exit boolean? #f]
                          [#:threshold threshold (or/c string? number? #f) #f]
                          [#:debounce debounce (or/c string? number? #f) #f]
                          [#:debounce-leading debounce-leading boolean? #f]
                          [#:debounce-notrailing debounce-notrailing boolean? #f]
                          [#:throttle throttle (or/c string? number? #f) #f]
                          [#:throttle-noleading throttle-noleading boolean? #f]
                          [#:throttle-trailing throttle-trailing boolean? #f]
                          [#:delay delay (or/c string? number? #f) #f]
                          [#:viewtransition viewtransition boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-intersect"]{@tt{data-on-intersect}}
attribute that runs @racket[expression] when the element intersects with the viewport.
Keyword arguments correspond to Datastar modifiers.

@examples[#:eval ev #:label #f
(ds:on-intersect (sse-get "/load-more") #:once #t #:half #t)
]
}

@defproc[(ds:on-interval [expression string?]
                         [#:duration duration (or/c string? number? #f) #f]
                         [#:duration-leading duration-leading boolean? #f]
                         [#:viewtransition viewtransition boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-interval"]{@tt{data-on-interval}}
attribute that runs @racket[expression] at a regular interval (default one second).
Keyword arguments correspond to Datastar modifiers.

@examples[#:eval ev #:label #f
(ds:on-interval "$count++" #:duration "2s")
]

@examples[#:eval ev #:label #f
(ds:on-interval (sse-get "/poll") #:duration "5s" #:duration-leading #t)
]
}

@defproc[(ds:on-signal-patch [expression string?]
                             [#:include include (or/c string? #f) #f]
                             [#:exclude exclude (or/c string? #f) #f]
                             [#:debounce debounce (or/c string? number? #f) #f]
                             [#:debounce-leading debounce-leading boolean? #f]
                             [#:debounce-notrailing debounce-notrailing boolean? #f]
                             [#:throttle throttle (or/c string? number? #f) #f]
                             [#:throttle-noleading throttle-noleading boolean? #f]
                             [#:throttle-trailing throttle-trailing boolean? #f]
                             [#:delay delay (or/c string? number? #f) #f]) (or/c list? (listof list?))]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-signal-patch"]{@tt{data-on-signal-patch}}
attribute that runs @racket[expression] whenever signals are patched.

When @racket[#:include] or @racket[#:exclude] are provided, a separate
@tt{data-on-signal-patch-filter} attribute is generated alongside the main attribute.
In this case, the function returns a @emph{list of two attribute pairs} instead of a
single pair. Use @racket[,@"@"] (unquote-splicing) to insert them.

No filter (returns single pair):

@examples[#:eval ev #:label #f
`(div (,(ds:on-signal-patch "console.log('patched')")))
]

With filter (returns list of two pairs, use @racket[,@"@"]):

@examples[#:eval ev #:label #f
`(div (,@(ds:on-signal-patch "console.log('counter changed')"
                             #:include "/^counter$/"
                             #:debounce "300ms")))
]
}

@defproc[(ds:preserve-attrs [attrs (or/c string? (listof string?))]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-preserve-attr"]{@tt{data-preserve-attr}}
attribute that preserves specified attributes when Datastar morphs DOM elements.

@racket[attrs] can be a single attribute name string or a list of attribute name strings.

Preserve the @tt{open} attribute on a @tt{<details>} element:

@examples[#:eval ev #:label #f
`(details ((open "")) (,(ds:preserve-attrs "open")) (summary "Title") "Content")
]

Preserve multiple attributes:

@examples[#:eval ev #:label #f
`(details ((open "") (class "custom"))
          (,(ds:preserve-attrs '("open" "class")))
          (summary "Title")
          "Content")
]
}

@defproc[(ds:ref [signal string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-ref"]{@tt{data-ref}}
attribute that creates a signal referencing the DOM element.

@examples[#:eval ev #:label #f
`(div (,(ds:ref "myDiv")))
]
}

@defproc[(ds:show [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-show"]{@tt{data-show}}
attribute that shows or hides an element based on whether @racket[expression] evaluates
to @tt{true} or @tt{false}.

@examples[#:eval ev #:label #f
`(div (,(ds:show "$loggedIn")) "Welcome back")
]
}

@defproc[(ds:signals [key-or-hash (or/c symbol? string? hash?)]
                     [value-or-unused any/c #f]
                     [#:ifmissing ifmissing boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-signals"]{@tt{data-signals}}
attribute that patches one or more signals into the signal store.

In the keyed form, @racket[key-or-hash] is a symbol or string signal name and
@racket[value-or-unused] is the signal's initial value as a string expression.

In the hash form, @racket[key-or-hash] is a Racket hash that is serialized to JSON.
Nested hashes produce nested signals.

When @racket[#:ifmissing] is @racket[#t], signals are only set if they don't already exist.

Keyed form:

@examples[#:eval ev #:label #f
`(div (,(ds:signals 'count "0")))
]

Keyed form with ifmissing:

@examples[#:eval ev #:label #f
`(div (,(ds:signals 'count "0" #:ifmissing #t)))
]

Hash form:

@examples[#:eval ev #:label #f
`(div (,(ds:signals (hash 'count 0 'name "hello"))))
]

Nested signals:

@examples[#:eval ev #:label #f
`(div (,(ds:signals (hash 'form (hash 'name "" 'email "")))))
]
}

@defproc[(ds:style [key-or-hash (or/c symbol? string? hash?)]
                   [value-or-unused any/c #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-style"]{@tt{data-style}}
attribute that sets inline CSS style properties based on expressions.

In the keyed form, @racket[key-or-hash] is the CSS property name and
@racket[value-or-unused] is the expression.

In the hash form, @racket[key-or-hash] is a hash mapping CSS property names to
expressions.

Keyed form:

@examples[#:eval ev #:label #f
`(div (,(ds:style 'background-color "$dark ? 'black' : 'white'")))
]

Hash form:

@examples[#:eval ev #:label #f
`(div
  (,(ds:style
     (hash "display" "$hidden && 'none'" "color" "$error ? 'red' : 'black'"))))
]
}

@defproc[(ds:text [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-text"]{@tt{data-text}}
attribute that sets the text content of an element to the result of @racket[expression].

@examples[#:eval ev #:label #f
`(span (,(ds:text "$count")))
]
}


@subsection{Pro Attributes}

@defproc[(ds:custom-validity [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-custom-validity"]{@tt{data-custom-validity}}
attribute for custom form validation. An empty string means valid; a non-empty string is the
error message. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(input (,(ds:bind "password")
         ,(ds:custom-validity
           "$password.length < 8 ? 'Must be 8+ characters' : ''")))
]
}

@defproc[(ds:match-media [signal string?] [query string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-match-media"]{@tt{data-match-media}}
attribute that creates a signal tracking whether a media query matches. This is a Datastar
Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(ds:match-media "is-dark" "'prefers-color-scheme: dark'")))
]
}

@defproc[(ds:on-raf [expression string?]
                    [#:throttle throttle (or/c string? number? #f) #f]
                    [#:throttle-noleading throttle-noleading boolean? #f]
                    [#:throttle-trailing throttle-trailing boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-raf"]{@tt{data-on-raf}}
attribute that runs @racket[expression] on every @tt{requestAnimationFrame} callback.
This is a Datastar Pro attribute.
}

@defproc[(ds:on-resize [expression string?]
                       [#:debounce debounce (or/c string? number? #f) #f]
                       [#:debounce-leading debounce-leading boolean? #f]
                       [#:debounce-notrailing debounce-notrailing boolean? #f]
                       [#:throttle throttle (or/c string? number? #f) #f]
                       [#:throttle-noleading throttle-noleading boolean? #f]
                       [#:throttle-trailing throttle-trailing boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-resize"]{@tt{data-on-resize}}
attribute that runs @racket[expression] whenever the element's dimensions change.
This is a Datastar Pro attribute.
}

@defproc[(ds:persist [#:key key (or/c string? #f) #f]
                     [#:include include (or/c string? #f) #f]
                     [#:exclude exclude (or/c string? #f) #f]
                     [#:session session boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-persist"]{@tt{data-persist}}
attribute that persists signals in @tt{localStorage} (or @tt{sessionStorage} with
@racket[#:session]). This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(ds:persist)))
]

@examples[#:eval ev #:label #f
`(div (,(ds:persist #:key "myapp" #:include "/^user\\./" #:session #t)))
]
}

@defproc[(ds:query-string [#:include include (or/c string? #f) #f]
                          [#:exclude exclude (or/c string? #f) #f]
                          [#:filter filter boolean? #f]
                          [#:history history boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-query-string"]{@tt{data-query-string}}
attribute that syncs signal values to and from URL query string parameters. This is a
Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(ds:query-string #:filter #t #:history #t)))
]
}

@defproc[(ds:replace-url [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-replace-url"]{@tt{data-replace-url}}
attribute that replaces the browser URL without reloading. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(ds:replace-url "`/page${$page}`")))
]
}

@defproc[(ds:scroll-into-view [#:smooth smooth boolean? #f]
                              [#:instant instant boolean? #f]
                              [#:auto auto boolean? #f]
                              [#:hstart hstart boolean? #f]
                              [#:hcenter hcenter boolean? #f]
                              [#:hend hend boolean? #f]
                              [#:hnearest hnearest boolean? #f]
                              [#:vstart vstart boolean? #f]
                              [#:vcenter vcenter boolean? #f]
                              [#:vend vend boolean? #f]
                              [#:vnearest vnearest boolean? #f]
                              [#:focus focus boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-scroll-into-view"]{@tt{data-scroll-into-view}}
attribute that scrolls the element into view. Keyword arguments correspond to Datastar
modifiers for scrolling behavior, horizontal/vertical alignment, and focus. This is a
Datastar Pro attribute.

@examples[#:eval ev #:label #f
(ds:scroll-into-view #:smooth #t #:vcenter #t)
]
}

@defproc[(ds:view-transition [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-view-transition"]{@tt{data-view-transition}}
attribute that sets the @tt{view-transition-name} style attribute. This is a Datastar Pro
attribute.

@examples[#:eval ev #:label #f
`(div (,(ds:view-transition "$transitionName")))
]
}

@section{Action Helpers}

Convenience functions for generating Datastar
@link["https://data-star.dev/reference/actions#backend-actions"]{backend action} attribute strings.

@examples[#:eval ev #:label #f
(define tid 42)
`(main ((id "main") ,(ds:init (sse-get "/events")))
       (form (,(ds:on "submit" (sse-post "/todo/create")))
             (button (,(ds:on "click"
                              (sse-post (format "/todo/delete/~a" tid))))
                     "Delete")))
]

@defproc[(sse-get [url string?] [args string? #f]) string?]{
Returns a @tt{@"@"get} action string. When @racket[args] is provided, it is included as a
second argument.

@examples[#:eval ev #:label #f
(sse-get "/events")
]

@examples[#:eval ev #:label #f
(sse-get "/events" "{includeLocal: true}")
]
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

@section{Reading Requests}

@defproc[(read-signals [request request?]) jsexpr?]{
Parses incoming signal data from the browser. For GET requests, extracts data from the
@tt{datastar} query parameter. For other methods, parses the request body as JSON. This is
a standalone function that operates on the request and does not require an SSE generator.
}

@defproc[(datastar-request? [request request?]) boolean?]{
Returns @racket[#t] if the request has a @tt{Datastar-Request: true} header, meaning
it came from a Datastar action. The check is case-insensitive.
}

@section{SSE Events}

Functions for creating and sending
@link["https://data-star.dev/reference/sse_events"]{Datastar SSE events}. See the
@link["https://data-star.dev/reference/sse_events"]{Datastar SSE events reference} for
full details on event types and their data lines.

@subsection{SSE Generator}

@defproc[(datastar-sse [request request?]
                       [on-open (-> sse? any)]
                       [#:on-close on-close (or/c (-> sse? any) #f) #f]
                       [#:write-profile write-profile write-profile? basic-write-profile]) response?]{
Creates an HTTP response with proper SSE headers. Calls @racket[on-open] with a fresh
@racket[sse?] generator that can be used to send events to the client. When @racket[on-open]
returns (or raises an exception), the connection is closed and @racket[on-close] is called
if provided.

When the server is set up with @racket[datastar-tcp@], client disconnections are detected
immediately: the SDK monitors the TCP input port and interrupts @racket[on-open] as soon as
the client goes away, ensuring prompt cleanup via @racket[on-close].

The @racket[write-profile] controls how SSE bytes are written to the connection. The default
@racket[basic-write-profile] writes uncompressed. Custom write profiles can add compression
(see @secref["write-profiles"]). If the client does not advertise support for the profile's
content encoding in @tt{Accept-Encoding}, the SDK automatically falls back to
@racket[basic-write-profile].

@bold{Important:} When using @racket[serve], the following settings are required for SSE to
work correctly:

@itemlist[
  @item{@racket[#:tcp@] should be @racket[datastar-tcp@] for instant disconnect detection.
  Without this, disconnections are only detected on the next failed write, which means
  @racket[on-close] may not fire promptly if the handler is blocked waiting for data.}

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

The lock is re-entrant: all send functions (@racket[patch-elements],
@racket[patch-signals], etc.) use @racket[call-with-sse-lock]
internally, so calling them inside a locked region does not deadlock.

@codeblock{
(call-with-sse-lock sse
                    (lambda ()
                      (patch-elements/xexpr sse '(div ((id "a")) "part 1"))
                      (patch-elements/xexpr sse '(div ((id "b")) "part 2"))
                      (patch-signals sse (hash 'status "updated"))))
}

If an exception is raised inside @racket[thunk], the lock is released via
@racket[dynamic-wind], so subsequent sends can still proceed.
}

@defform[(with-sse-lock sse body ...)]{
Syntax form that wraps @racket[body ...] in a call to @racket[call-with-sse-lock].

@codeblock{
(with-sse-lock sse
               (patch-elements/xexpr sse '(div ((id "a")) "part 1"))
               (patch-elements/xexpr sse '(div ((id "b")) "part 2"))
               (patch-signals sse (hash 'status "updated")))
}
}

@subsection{Sending Events}

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
Sends a @link["https://data-star.dev/reference/sse_events#datastar-patch-elements"]{@tt{datastar-patch-elements}}
SSE event that patches HTML elements into the DOM.

The @racket[#:mode] parameter controls how elements are patched. Use the named constants:
@racket[patch-mode-outer] (morph entire element, default), @racket[patch-mode-inner]
(morph inner HTML), @racket[patch-mode-replace] (replace entire element),
@racket[patch-mode-prepend], @racket[patch-mode-append], @racket[patch-mode-before],
@racket[patch-mode-after], and @racket[patch-mode-remove].
When @racket[#f] or @racket[patch-mode-outer], the mode data line is omitted.

@codeblock{
(patch-elements sse "<div id=\"out\">hello</div>")
(patch-elements sse "<svg>...</svg>" #:namespace element-namespace-svg)
(patch-elements sse "<li>item</li>" #:selector "#list" #:mode patch-mode-append)
}
}

@defproc[(patch-elements/xexpr [sse sse?]
                                [xexpr xexpr/c]
                                [#:selector selector (or/c string? #f) #f]
                                [#:mode mode element-patch-mode/c #f]
                                [#:namespace namespace element-namespace/c #f]
                                [#:use-view-transitions use-view-transitions (or/c boolean? #f) #f]
                                [#:event-id event-id (or/c string? #f) #f]
                                [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Like @racket[patch-elements], but accepts an x-expression instead of a raw HTML string.
Converts @racket[xexpr] via @racket[xexpr->string] and delegates to @racket[patch-elements].

@codeblock{
(patch-elements/xexpr sse '(div ((id "out")) "hello"))
(patch-elements/xexpr sse '(svg "...") #:namespace element-namespace-svg)
(patch-elements/xexpr sse
                      '(li "item")
                      #:selector "#list"
                      #:mode patch-mode-append)
}
}

@defproc[(remove-elements [sse sse?]
                           [selector string?]
                           [#:event-id event-id (or/c string? #f) #f]
                           [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Removes elements from the DOM by CSS selector. Convenience function that calls
@racket[patch-elements] with @racket[patch-mode-remove].
}

@defproc[(patch-signals [sse sse?]
                         [signals (or/c string? jsexpr?)]
                         [#:event-id event-id (or/c string? #f) #f]
                         [#:only-if-missing only-if-missing (or/c boolean? #f) #f]
                         [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Sends a @link["https://data-star.dev/reference/sse_events#datastar-patch-signals"]{@tt{datastar-patch-signals}}
SSE event that patches signals into the signal store using RFC 7386 JSON Merge Patch
semantics.
}

@defproc[(execute-script [sse sse?]
                          [script string?]
                          [#:auto-remove auto-remove boolean? #t]
                          [#:attributes attributes (or/c (hash/c symbol? any/c) (listof string?) #f) #f]
                          [#:event-id event-id (or/c string? #f) #f]
                          [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) void?]{
Sends a @link["https://data-star.dev/reference/sse_events#datastar-execute-script"]{@tt{datastar-execute-script}}
SSE event that executes JavaScript in the browser by injecting a script tag. The script is
automatically removed after execution unless @racket[auto-remove] is @racket[#f].
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

@examples[#:eval ev #:label #f
`(script ((type "module") (src ,datastar-cdn-url)))
]
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

@section{Testing Utilities}

@defmodule[datastar/testing]

Mock SSE generators for testing Datastar handlers without a real HTTP connection.
Require this module separately from @racket[datastar].

@defproc[(make-mock-sse) (values sse? (-> string?))]{
Creates a mock @racket[sse?] generator that works with all the normal send functions
(@racket[patch-elements], @racket[patch-signals], etc.) but doesn't touch the network.
Returns two values: the generator, and a thunk that returns all the SSE text that
has been sent through it so far.

@examples[#:eval ev #:label #f
(define-values (sse get-output) (make-mock-sse))
(patch-elements/xexpr sse '(div ((id "x")) "hi"))
(get-output)
]
}

@defproc[(make-recording-sse) (values sse? (-> (listof sse-event?)))]{
Like @racket[make-mock-sse], but instead of returning raw text, the retrieval thunk
returns a list of @racket[sse-event] structs, one per event sent.

The events are parsed from the same SSE text that would go over the wire, so they
reflect exactly what a real client would receive.

@examples[#:eval ev #:label #f
(define-values (sse2 get-events) (make-recording-sse))
(patch-elements/xexpr sse2 '(div "test"))
(patch-signals sse2 (hash 'x 1))
(define events (get-events))
(length events)
(sse-event-type (first events))
(sse-event-type (second events))
]
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
