#lang scribble/doc

@(require (for-label datastar
                     datastar/sugar
                     racket/base
                     racket/contract)
          "datastar.rkt")

@title[#:tag "sugar"]{Sugar}

Convenience functions for building Datastar HTML attributes and action expressions.

@bold{Pass-through policy:} sugar helpers build expressions/attributes from the arguments you provide. They do not apply serializer-style default pruning. For example, passing @racket[#:open-when-hidden? #f] still emits @tt{openWhenHidden: false}, and passing @racket[#:duration "1s"] still emits @tt{__duration.1s}.

@defmodule[datastar/sugar]

@defthing[datastar-version string?]{
The Datastar version string (e.g., @racket["v1.0.0"]).
}

@defthing[datastar-cdn-url string?]{
URL for the Datastar JavaScript bundle on the jsDelivr CDN, derived from @racket[datastar-version].

@examples[#:eval ev #:label #f
`(script ((type "module") (src ,datastar-cdn-url)))
]
}

@defthing[datastar-cdn-map-url string?]{
URL for the source map corresponding to @racket[datastar-cdn-url].
}

@section[#:tag "attributes"]{Attributes}

@defmodule[datastar/sugar/attributes]

Functions for generating Datastar @tt{data-*} @link["https://data-star.dev/reference/attributes"]{HTML attributes} as x-expression attribute pairs. Helpers return @racket[(list 'attr-name "value")] and drop directly into x-expression templates via unquote. See the @link["https://data-star.dev/reference/attributes"]{Datastar attribute reference} for full details on each attribute's behavior.

Without attribute helpers:

@examples[#:eval ev #:label #f
`(button ((data-on:click__debounce.500ms "@post('/search')")
          (data-class:active "$enabled")
          (data-show "$query != ''")))
]

With attribute helpers:

@examples[#:eval ev #:label #f
`(button (,(data-on "click" (post "/search") #:debounce "500ms")
          ,(data-class 'active "$enabled")
          ,(data-show "$query != ''")))
]

Modifiers (debounce, throttle, once, etc.) are expressed as keyword arguments rather than method chaining. Boolean modifiers take @racket[#t]; parameterized modifiers take their value directly.

@defthing[case-style/c flat-contract?]{
Contract for Datastar case-modifier values: @racket[(or/c 'camel 'kebab 'snake 'pascal)].
}

@defproc[(data-attr [key (or/c symbol? string?)]
                  [expression string?]) list?]{
Generates keyed @link["https://data-star.dev/reference/attributes#data-attr"]{@tt{data-attr}} form: @tt{data-attr:key="expression"}.

@examples[#:eval ev #:label #f
`(button (,(data-attr 'disabled "$loading")))
]
}

@defproc[(data-attr/hash [attrs hash?]) list?]{
Generates value-form @link["https://data-star.dev/reference/attributes#data-attr"]{@tt{data-attr}} from a hash of attribute names to expressions.

Hash keys must be symbols or strings. Values may be expression strings, JSON values, or nested hashes with symbol/string keys.

@examples[#:eval ev #:label #f
`(button (,(data-attr/hash (hash "disabled" "$loading" "aria-busy" "$loading"))))
]
}

@defproc[(data-bind [signal (or/c symbol? string?)]
                    [#:case case case-style/c]
                    [#:prop prop (or/c symbol? string?)]
                    [#:event event (or/c (or/c symbol? string?)
                                         (listof (or/c symbol? string?)))]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-bind"]{@tt{data-bind}} attribute that creates a signal (if one doesn't already exist) and sets up two-way data binding between it and an element's value.

This SDK uses keyed form only (@tt{data-bind:signal}).

@itemlist[
  @item{@racket[#:case] emits Datastar's @tt{__case.*} modifier.}
  @item{@racket[#:prop] emits @tt{__prop.<property>}.}
  @item{@racket[#:event] emits @tt{__event.<event...>} (single event or list).}
]

When multiple modifiers are used, this helper emits them in deterministic order: @tt{__case}, then @tt{__prop}, then @tt{__event}.

@examples[#:eval ev #:label #f
`(input (,(data-bind 'username)))
]

@examples[#:eval ev #:label #f
`(my-toggle (,(data-bind "is-checked" #:prop "checked" #:event "change")))
]

@examples[#:eval ev #:label #f
`(input (,(data-bind "query" #:prop 'value #:event '("input" "change"))))
]
}

@defproc[(data-class [key (or/c symbol? string?)]
                   [expression string?]
                   [#:case case case-style/c]) list?]{
Generates keyed @link["https://data-star.dev/reference/attributes#data-class"]{@tt{data-class}} form: @tt{data-class:key="expression"}.

@racket[#:case] emits Datastar's @tt{__case.*} modifier.

@examples[#:eval ev #:label #f
`(button (,(data-class 'active "$selected")))
]

@examples[#:eval ev #:label #f
`(button (,(data-class "my-class" "$selected" #:case 'camel)))
]
}

@defproc[(data-class/hash [classes hash?]) list?]{
Generates value-form @link["https://data-star.dev/reference/attributes#data-class"]{@tt{data-class}} from a hash of class names to expressions.

Hash keys must be symbols or strings. Values may be expression strings, JSON values, or nested hashes with symbol/string keys.

@examples[#:eval ev #:label #f
`(div (,(data-class/hash (hash "font-bold" "$important" "text-red" "$error"))))
]
}

@defproc[(data-computed [key (or/c symbol? string?)]
                      [expression string?]
                      [#:case case case-style/c]) list?]{
Generates keyed @link["https://data-star.dev/reference/attributes#data-computed"]{@tt{data-computed}} form: @tt{data-computed:key="expression"}.

@racket[#:case] emits Datastar's @tt{__case.*} modifier.

@examples[#:eval ev #:label #f
`(div (,(data-computed 'total "$price * $quantity")))
]

@examples[#:eval ev #:label #f
`(div (,(data-computed "my-signal" "$price * $quantity" #:case 'camel)))
]
}

@defproc[(data-computed/hash [computed hash?]) list?]{
Generates value-form @link["https://data-star.dev/reference/attributes#data-computed"]{@tt{data-computed}} from a hash of signal names to callable expression strings.

Hash keys must be symbols or strings. Values must be callable expression strings.

@examples[#:eval ev #:label #f
`(div (,(data-computed/hash (hash 'total "() => $price * $qty" 'valid "() => $total > 0"))))
]
}

@defproc[(data-effect [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-effect"]{@tt{data-effect}} attribute that executes @racket[expression] on page load and whenever any signals in the expression change. This is useful for performing side effects.

@examples[#:eval ev #:label #f
`(div (,(data-effect "$total = $price * $quantity")))
]
}

@defproc[(data-ignore [#:self? self? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-ignore"]{@tt{data-ignore}} attribute that tells Datastar to ignore this element and its descendants when walking the DOM.

When @racket[#:self?] is @racket[#t], only the element itself is ignored; its children are still processed.

@examples[#:eval ev #:label #f
`(div (,(data-ignore)) "Datastar will not process this or its children")
]

@examples[#:eval ev #:label #f
`(div (,(data-ignore #:self? #t)) "Only this element is ignored")
]
}

@defproc[(data-ignore-morph) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-ignore-morph"]{@tt{data-ignore-morph}} attribute that tells Datastar's element patcher to skip this element and its children when morphing. Takes no arguments.

@examples[#:eval ev #:label #f
`(div (,(data-ignore-morph)) "This content will not be morphed")
]
}

@defproc[(data-indicator [signal (or/c symbol? string?)]
                         [#:case case case-style/c]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-indicator"]{@tt{data-indicator}} attribute that creates a signal and sets its value to @tt{true} while a fetch request is in flight, otherwise @tt{false}.

This SDK uses keyed form only (@tt{data-indicator:signal}).

@racket[#:case] emits Datastar's @tt{__case.*} modifier.

@examples[#:eval ev #:label #f
`(button (,(data-indicator 'loading) ,(data-on "click" (get "/data"))) "Fetch")
]

@examples[#:eval ev #:label #f
`(div (,(data-show "$loading")) "Loading...")
]
}

@defproc[(data-init [expression string?]
                  [#:delay delay (or/c string? number?)]
                  [#:viewtransition? viewtransition? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-init"]{@tt{data-init}} attribute that runs @racket[expression] when the attribute is initialized. This can happen on page load, when an element is patched into the DOM, and any time the attribute is modified.

@examples[#:eval ev #:label #f
(data-init (get "/events"))
]
}

@defproc[(data-json-signals [#:include include string?]
                          [#:exclude exclude string?]
                          [#:terse? terse? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-json-signals"]{@tt{data-json-signals}} attribute that sets the text content of an element to a reactive JSON stringified version of signals. Useful for troubleshooting.

@examples[#:eval ev #:label #f
`(pre (,(data-json-signals)))
]

@examples[#:eval ev #:label #f
`(pre (,(data-json-signals #:include "/^user/" #:terse? #t)))
]
}

@defproc[(data-on [event (or/c symbol? string?)]
                [expression string?]
                [#:once? once? boolean? #f]
                [#:passive? passive? boolean? #f]
                [#:capture? capture? boolean? #f]
                [#:case case case-style/c]
                [#:window? window? boolean? #f]
                [#:document? document? boolean? #f]
                [#:outside? outside? boolean? #f]
                [#:prevent? prevent? boolean? #f]
                [#:stop? stop? boolean? #f]
                [#:debounce debounce (or/c string? number?)]
                [#:debounce-leading? debounce-leading? boolean? #f]
                [#:debounce-notrailing? debounce-notrailing? boolean? #f]
                [#:throttle throttle (or/c string? number?)]
                [#:throttle-noleading? throttle-noleading? boolean? #f]
                [#:throttle-trailing? throttle-trailing? boolean? #f]
                [#:delay delay (or/c string? number?)]
                [#:viewtransition? viewtransition? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on"]{@tt{data-on}} attribute that attaches an event listener to an element and executes @racket[expression] whenever @racket[event] is triggered. Keyword arguments correspond to Datastar modifiers.

Leading/trailing timing modifiers require their base value keyword (for example, @racket[#:debounce-leading?] requires @racket[#:debounce]).

@examples[#:eval ev #:label #f
(data-on "click" "$count++")
]

@examples[#:eval ev #:label #f
(data-on "input" (post "/search") #:debounce "250ms")
]

@examples[#:eval ev #:label #f
(data-on "click" (get "/data") #:once? #t #:prevent? #t)
]

@examples[#:eval ev #:label #f
(data-on "keydown" "$handleKey(evt)" #:window? #t)
]

@examples[#:eval ev #:label #f
(data-on "selectionchange" "doSomething()" #:document? #t)
]

@examples[#:eval ev #:label #f
(data-on 'my-event "fn()" #:case 'camel)
]
}

@defproc[(data-on-intersect [expression string?]
                          [#:once? once? boolean? #f]
                          [#:half? half? boolean? #f]
                          [#:full? full? boolean? #f]
                          [#:exit? exit? boolean? #f]
                          [#:threshold threshold (or/c string? number?)]
                          [#:debounce debounce (or/c string? number?)]
                          [#:debounce-leading? debounce-leading? boolean? #f]
                          [#:debounce-notrailing? debounce-notrailing? boolean? #f]
                          [#:throttle throttle (or/c string? number?)]
                          [#:throttle-noleading? throttle-noleading? boolean? #f]
                          [#:throttle-trailing? throttle-trailing? boolean? #f]
                          [#:delay delay (or/c string? number?)]
                          [#:viewtransition? viewtransition? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-intersect"]{@tt{data-on-intersect}} attribute that runs @racket[expression] when the element intersects with the viewport. Keyword arguments correspond to Datastar modifiers.

Leading/trailing timing modifiers require their base value keyword.

@examples[#:eval ev #:label #f
(data-on-intersect (get "/load-more") #:once? #t #:half? #t)
]
}

@defproc[(data-on-interval [expression string?]
                         [#:duration duration (or/c string? number?)]
                         [#:duration-leading? duration-leading? boolean? #f]
                         [#:viewtransition? viewtransition? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-interval"]{@tt{data-on-interval}} attribute that runs @racket[expression] at a regular interval. The interval duration defaults to one second and can be modified using @racket[#:duration]. Keyword arguments correspond to Datastar modifiers.

@racket[#:duration-leading?] requires @racket[#:duration].

@examples[#:eval ev #:label #f
(data-on-interval "$count++" #:duration "2s")
]

@examples[#:eval ev #:label #f
(data-on-interval (get "/poll") #:duration "5s" #:duration-leading? #t)
]
}

@defproc[(data-on-signal-patch [expression string?]
                             [#:debounce debounce (or/c string? number?)]
                             [#:debounce-leading? debounce-leading? boolean? #f]
                             [#:debounce-notrailing? debounce-notrailing? boolean? #f]
                             [#:throttle throttle (or/c string? number?)]
                             [#:throttle-noleading? throttle-noleading? boolean? #f]
                             [#:throttle-trailing? throttle-trailing? boolean? #f]
                             [#:delay delay (or/c string? number?)]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-signal-patch"]{@tt{data-on-signal-patch}} attribute that runs @racket[expression] whenever any signals are patched.

Leading/trailing timing modifiers require their base value keyword.

Use @racket[data-on-signal-patch-filter] as a separate helper when you want to apply include/exclude filtering.

@examples[#:eval ev #:label #f
`(div (,(data-on-signal-patch "console.log('patched')")))
]

@examples[#:eval ev #:label #f
`(div (,(data-on-signal-patch "console.log('counter changed')" #:debounce "300ms")
       ,(data-on-signal-patch-filter #:include "/^counter$/")))
]
}

@defproc[(data-on-signal-patch-filter [#:include include string?]
                                    [#:exclude exclude string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-signal-patch-filter"]{@tt{data-on-signal-patch-filter}} attribute directly.

Use this helper when you want to construct the @tt{data-on-signal-patch-filter} attribute separately from @racket[data-on-signal-patch].

@examples[#:eval ev #:label #f
`(div (,(data-on-signal-patch "console.log(patch)")
       ,(data-on-signal-patch-filter #:include "/^counter$/")))
]
}

@defproc[(data-preserve-attr [attrs (or/c string? (listof string?))]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-preserve-attr"]{@tt{data-preserve-attr}} attribute that preserves the values of specified attributes when morphing DOM elements.

@racket[attrs] can be a single attribute-name string or a list of attribute-name strings.

Preserve @tt{open} attribute on @tt{<details>} element:

@examples[#:eval ev #:label #f
`(details (,(data-preserve-attr "open") (open ""))
          (summary "Title")
          "Content")
]

Preserve multiple attributes:

@examples[#:eval ev #:label #f
`(details (,(data-preserve-attr '("open" "class")) (open "") (class "custom"))
          (summary "Title")
          "Content")
]
}

@defproc[(data-ref [signal (or/c symbol? string?)]
                   [#:case case case-style/c]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-ref"]{@tt{data-ref}} attribute that creates a signal reference to the element where the attribute is placed.

This SDK uses keyed form only (@tt{data-ref:signal}).

@racket[#:case] emits Datastar's @tt{__case.*} modifier.

@examples[#:eval ev #:label #f
`(div (,(data-ref 'myDiv)))
]
}

@defproc[(data-show [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-show"]{@tt{data-show}} attribute that shows or hides an element based on whether @racket[expression] evaluates to @tt{true} or @tt{false}.

@examples[#:eval ev #:label #f
`(div (,(data-show "$loggedIn")) "Welcome back")
]
}

@defproc[(data-signals [key (or/c symbol? string?)]
                     [expression string?]
                     [#:ifmissing? ifmissing? boolean? #f]
                     [#:case case case-style/c]) list?]{
Generates keyed @link["https://data-star.dev/reference/attributes#data-signals"]{@tt{data-signals}} form: @tt{data-signals:key="expression"}.

When @racket[#:ifmissing?] is @racket[#t], the signal is only set when missing.

@racket[#:case] emits Datastar's @tt{__case.*} modifier.

@examples[#:eval ev #:label #f
`(div (,(data-signals 'count "0")))
]

@examples[#:eval ev #:label #f
`(div (,(data-signals 'count "0" #:ifmissing? #t)))
]

@examples[#:eval ev #:label #f
`(div (,(data-signals "my-signal" "1" #:case 'kebab)))
]
}

@defproc[(data-signals/hash [signals hash?]
                          [#:ifmissing? ifmissing? boolean? #f]) list?]{
Generates value-form @link["https://data-star.dev/reference/attributes#data-signals"]{@tt{data-signals}} from a hash serialized as JSON.

Hash keys must be symbols or strings. Values may be JSON values, expression strings, lists, or nested hashes with symbol/string keys.

Nested hashes produce nested signals.

@examples[#:eval ev #:label #f
`(div (,(data-signals/hash (hash 'count 0 'name "hello"))))
]

@examples[#:eval ev #:label #f
`(div (,(data-signals/hash (hash "theme" "dark"))))
]

@examples[#:eval ev #:label #f
`(div (,(data-signals/hash (hash 'form (hash 'name "" 'email "")))))
]
}

@defproc[(data-style [key (or/c symbol? string?)]
                   [expression string?]) list?]{
Generates keyed @link["https://data-star.dev/reference/attributes#data-style"]{@tt{data-style}} form: @tt{data-style:key="expression"}.

@examples[#:eval ev #:label #f
`(div (,(data-style 'background-color "$dark ? 'black' : 'white'")))
]
}

@defproc[(data-style/hash [styles hash?]) list?]{
Generates value-form @link["https://data-star.dev/reference/attributes#data-style"]{@tt{data-style}} from a hash of CSS properties to expressions.

Hash keys must be symbols or strings. Values may be expression strings, JSON values, or nested hashes with symbol/string keys.

@examples[#:eval ev #:label #f
`(div
  (,(data-style/hash
     (hash "display" "$hidden && 'none'" "color" "$error ? 'red' : 'black'"))))
]
}

@defproc[(data-text [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-text"]{@tt{data-text}} attribute that binds the text content of an element to @racket[expression].

@examples[#:eval ev #:label #f
`(span (,(data-text "$count")))
]
}


@subsection{Pro Attributes}

@defproc[(data-animate [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-animate"]{@tt{data-animate}} attribute for Datastar Pro animations.
}

@defproc[(data-custom-validity [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-custom-validity"]{@tt{data-custom-validity}} attribute for custom form validation. The expression must evaluate to a string: an empty string means the input is valid; a non-empty string is used as the validation error message. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(input (,(data-bind "password")
         ,(data-custom-validity
           "$password.length < 8 ? 'Must be 8+ characters' : ''")))
]
}

@defproc[(data-match-media [signal (or/c symbol? string?)] [query string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-match-media"]{@tt{data-match-media}} attribute that sets a signal to whether a media query matches, and keeps it in sync whenever the query changes. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(data-match-media 'is-dark "'prefers-color-scheme: dark'")))
]
}

@defproc[(data-on-raf [expression string?]
                    [#:throttle throttle (or/c string? number?)]
                    [#:throttle-noleading? throttle-noleading? boolean? #f]
                    [#:throttle-trailing? throttle-trailing? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-raf"]{@tt{data-on-raf}} attribute that runs @racket[expression] on every @tt{requestAnimationFrame} callback. This is a Datastar Pro attribute.

@racket[#:throttle-noleading?] and @racket[#:throttle-trailing?] require @racket[#:throttle].
}

@defproc[(data-on-resize [expression string?]
                       [#:debounce debounce (or/c string? number?)]
                       [#:debounce-leading? debounce-leading? boolean? #f]
                       [#:debounce-notrailing? debounce-notrailing? boolean? #f]
                       [#:throttle throttle (or/c string? number?)]
                       [#:throttle-noleading? throttle-noleading? boolean? #f]
                       [#:throttle-trailing? throttle-trailing? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-on-resize"]{@tt{data-on-resize}} attribute that runs @racket[expression] whenever the element's dimensions change. This is a Datastar Pro attribute.

Leading/trailing timing modifiers require their base value keyword.
}

@defproc[(data-persist [#:key key (or/c symbol? string?)]
                     [#:include include string?]
                     [#:exclude exclude string?]
                     [#:session? session? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-persist"]{@tt{data-persist}} attribute that persists signals in @tt{localStorage} (or @tt{sessionStorage} with @racket[#:session?]). Useful for storing signal values between page loads. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(data-persist)))
]

@examples[#:eval ev #:label #f
`(div (,(data-persist #:key 'myapp #:include "/^user\\./" #:session? #t)))
]
}

@defproc[(data-query-string [#:include include string?]
                          [#:exclude exclude string?]
                          [#:filter? filter? boolean? #f]
                          [#:history? history? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-query-string"]{@tt{data-query-string}} attribute that syncs query string parameters to signal values on page load, and syncs signal values to query string parameters on change. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(data-query-string #:filter? #t #:history? #t)))
]
}

@defproc[(data-replace-url [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-replace-url"]{@tt{data-replace-url}} attribute that replaces the URL in the browser without reloading the page. The value can be a relative or absolute URL. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(data-replace-url "`/page${page}`")))
]
}

@defproc[(data-scroll-into-view [#:smooth? smooth? boolean? #f]
                              [#:instant? instant? boolean? #f]
                              [#:auto? auto? boolean? #f]
                              [#:hstart? hstart? boolean? #f]
                              [#:hcenter? hcenter? boolean? #f]
                              [#:hend? hend? boolean? #f]
                              [#:hnearest? hnearest? boolean? #f]
                              [#:vstart? vstart? boolean? #f]
                              [#:vcenter? vcenter? boolean? #f]
                              [#:vend? vend? boolean? #f]
                              [#:vnearest? vnearest? boolean? #f]
                              [#:focus? focus? boolean? #f]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-scroll-into-view"]{@tt{data-scroll-into-view}} attribute that scrolls the element into view. Useful when updating the DOM from the backend and you want to scroll to new content. Keyword arguments correspond to Datastar modifiers for scrolling behavior, horizontal/vertical alignment, and focus. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
(data-scroll-into-view #:smooth? #t #:vcenter? #t)
]
}

@defproc[(data-view-transition [expression string?]) list?]{
Generates a @link["https://data-star.dev/reference/attributes#data-view-transition"]{@tt{data-view-transition}} attribute that sets the @tt{view-transition-name} style attribute explicitly. This is a Datastar Pro attribute.

@examples[#:eval ev #:label #f
`(div (,(data-view-transition "$transitionName")))
]
}

@section[#:tag "actions"]{Actions}

@defmodule[datastar/sugar/actions]

Convenience functions for generating Datastar @link["https://data-star.dev/reference/actions"]{action} strings in x-expression templates. These map to the @link["https://data-star.dev/reference/actions"]{Actions} section of the Datastar docs. Also available via @racket[(require datastar)]. If any names conflict with other imports, use @racket[(require (prefix-in ds: datastar/sugar/actions))] to prefix just the action helpers.

@examples[#:eval ev #:label #f
(define tid 42)
`(main ((id "main") ,(data-init (get "/events")))
       (form (,(data-on "submit" (post "/todo/create")))
             (button (,(data-on "click"
                              (post (format "/todo/delete/~a" tid))))
                     "Delete")))
]

@subsection{Signal Actions}

@defproc[(peek [callable string?]) string?]{
Returns a @link["https://data-star.dev/reference/actions#peek"]{@tt{@"@"peek}} action string, which allows accessing signals without subscribing to changes.

@examples[#:eval ev #:label #f
(peek "$bar")
(peek "() => $bar")
]
}

@defproc[(set-all [value string?]
                      [#:include include string?]
                      [#:exclude exclude string?]) string?]{
Returns a @link["https://data-star.dev/reference/actions#setall"]{@tt{@"@"setAll}} action string. Sets all matching signals to the given value. The @racket[#:include] and @racket[#:exclude] keywords accept JavaScript regex literal strings.

@examples[#:eval ev #:label #f
(set-all "''")
(set-all "''" #:include "/^form\\./")
(set-all "0" #:include "/.*/" #:exclude "/^_/")
]
}

@defproc[(toggle-all [#:include include string?]
                         [#:exclude exclude string?]) string?]{
Returns a @link["https://data-star.dev/reference/actions#toggleall"]{@tt{@"@"toggleAll}} action string. Toggles the boolean value of all matching signals.

@examples[#:eval ev #:label #f
(toggle-all)
(toggle-all #:include "/^menu\\./")
]
}

@subsection{Backend Actions}

Backend actions send HTTP requests and handle SSE responses. Signals transmit by default.

All keyword options are pass-through: explicitly provided values are serialized into the action string, including explicit default-equivalent values.

@defproc[(get [url string?]
              [#:content-type content-type (or/c 'json 'form)]
              [#:filter-signals-include filter-signals-include string?]
              [#:filter-signals-exclude filter-signals-exclude string?]
              [#:selector selector string?]
              [#:headers headers hash?]
              [#:open-when-hidden? open-when-hidden? boolean?]
              [#:payload payload string?]
              [#:retry retry (or/c 'auto 'error 'always 'never)]
              [#:retry-interval retry-interval exact-nonnegative-integer?]
              [#:retry-scaler retry-scaler number?]
              [#:retry-max-wait retry-max-wait exact-nonnegative-integer?]
              [#:retry-max-count retry-max-count exact-nonnegative-integer?]
              [#:request-cancellation request-cancellation (or/c 'auto 'cleanup 'disabled string?)])
         string?]{
Returns a @link["https://data-star.dev/reference/actions#get"]{@tt{@"@"get}} action string.

The @racket[#:payload] keyword accepts a raw JavaScript expression string. The @racket[#:filter-signals-include] and @racket[#:filter-signals-exclude] keywords accept JavaScript regex literal strings (e.g., @racket["/^foo\\./"]). For @racket[#:request-cancellation], pass one of @racket['auto], @racket['cleanup], @racket['disabled], or a JavaScript expression string that evaluates to an @tt{AbortController} (e.g., @racket["$controller"]).

@tabular[#:sep @hspace[2] #:style 'boxed
(list (list @bold{Keyword}                      @bold{JS Property}            @bold{Datastar Client Default})
      (list @racket[#:content-type]              @tt{contentType}              @racket['json])
      (list @racket[#:filter-signals-include]    @tt{filterSignals.include}    @tt{/.*/})
      (list @racket[#:filter-signals-exclude]    @tt{filterSignals.exclude}    @tt{/(^_|\._).*/})
      (list @racket[#:selector]                  @tt{selector}                 @tt{null})
      (list @racket[#:headers]                   @tt{headers}                  @tt{@"{"@"}"})
      (list @racket[#:open-when-hidden?]          @tt{openWhenHidden}           @elem{@racket[#f] for GET, @racket[#t] for others})
      (list @racket[#:payload]                   @tt{payload}                  "—")
      (list @racket[#:retry]                     @tt{retry}                    @racket['auto])
      (list @racket[#:retry-interval]            @tt{retryInterval}            @racket[1000])
      (list @racket[#:retry-scaler]              @tt{retryScaler}              @racket[2])
      (list @racket[#:retry-max-wait]         @tt{retryMaxWait}           @racket[30000])
      (list @racket[#:retry-max-count]           @tt{retryMaxCount}            @racket[10])
      (list @racket[#:request-cancellation]      @tt{requestCancellation}      @racket['auto]))]

@examples[#:eval ev #:label #f
(get "/events")
(get "/events" #:retry 'never)
(get "/endpoint"
     #:filter-signals-include "/^foo\\./"
     #:headers (hash "X-Csrf-Token" "mytoken")
     #:open-when-hidden? #t
     #:request-cancellation 'disabled)
(get "/endpoint" #:request-cancellation "$controller")
]
}

@defproc[(post [url string?]
               [#:content-type content-type (or/c 'json 'form)]
               [#:filter-signals-include filter-signals-include string?]
               [#:filter-signals-exclude filter-signals-exclude string?]
               [#:selector selector string?]
               [#:headers headers hash?]
               [#:open-when-hidden? open-when-hidden? boolean?]
               [#:payload payload string?]
               [#:retry retry (or/c 'auto 'error 'always 'never)]
               [#:retry-interval retry-interval exact-nonnegative-integer?]
               [#:retry-scaler retry-scaler number?]
               [#:retry-max-wait retry-max-wait exact-nonnegative-integer?]
               [#:retry-max-count retry-max-count exact-nonnegative-integer?]
               [#:request-cancellation request-cancellation (or/c 'auto 'cleanup 'disabled string?)])
          string?]{
Like @link["https://data-star.dev/reference/actions#get"]{@racket[get]}, but returns a @link["https://data-star.dev/reference/actions#post"]{@tt{@"@"post}} action string that sends a POST request.

@examples[#:eval ev #:label #f
(post "/submit" #:content-type 'form #:selector "#myForm")
]
}

@defproc[(put [url string?]
              [#:content-type content-type (or/c 'json 'form)]
              [#:filter-signals-include filter-signals-include string?]
              [#:filter-signals-exclude filter-signals-exclude string?]
              [#:selector selector string?]
              [#:headers headers hash?]
              [#:open-when-hidden? open-when-hidden? boolean?]
              [#:payload payload string?]
              [#:retry retry (or/c 'auto 'error 'always 'never)]
              [#:retry-interval retry-interval exact-nonnegative-integer?]
              [#:retry-scaler retry-scaler number?]
              [#:retry-max-wait retry-max-wait exact-nonnegative-integer?]
              [#:retry-max-count retry-max-count exact-nonnegative-integer?]
              [#:request-cancellation request-cancellation (or/c 'auto 'cleanup 'disabled string?)])
         string?]{
Like @link["https://data-star.dev/reference/actions#get"]{@racket[get]}, but returns a @link["https://data-star.dev/reference/actions#put"]{@tt{@"@"put}} action string that sends a PUT request.
}

@defproc[(patch [url string?]
                [#:content-type content-type (or/c 'json 'form)]
                [#:filter-signals-include filter-signals-include string?]
                [#:filter-signals-exclude filter-signals-exclude string?]
                [#:selector selector string?]
                [#:headers headers hash?]
                [#:open-when-hidden? open-when-hidden? boolean?]
                [#:payload payload string?]
                [#:retry retry (or/c 'auto 'error 'always 'never)]
                [#:retry-interval retry-interval exact-nonnegative-integer?]
                [#:retry-scaler retry-scaler number?]
                [#:retry-max-wait retry-max-wait exact-nonnegative-integer?]
                [#:retry-max-count retry-max-count exact-nonnegative-integer?]
                [#:request-cancellation request-cancellation (or/c 'auto 'cleanup 'disabled string?)])
           string?]{
Like @link["https://data-star.dev/reference/actions#get"]{@racket[get]}, but returns a @link["https://data-star.dev/reference/actions#patch"]{@tt{@"@"patch}} action string that sends a PATCH request.
}

@defproc[(delete [url string?]
                 [#:content-type content-type (or/c 'json 'form)]
                 [#:filter-signals-include filter-signals-include string?]
                 [#:filter-signals-exclude filter-signals-exclude string?]
                 [#:selector selector string?]
                 [#:headers headers hash?]
                 [#:open-when-hidden? open-when-hidden? boolean?]
                 [#:payload payload string?]
                 [#:retry retry (or/c 'auto 'error 'always 'never)]
                 [#:retry-interval retry-interval exact-nonnegative-integer?]
                 [#:retry-scaler retry-scaler number?]
                 [#:retry-max-wait retry-max-wait exact-nonnegative-integer?]
                 [#:retry-max-count retry-max-count exact-nonnegative-integer?]
                 [#:request-cancellation request-cancellation (or/c 'auto 'cleanup 'disabled string?)])
            string?]{
Like @link["https://data-star.dev/reference/actions#get"]{@racket[get]}, but returns a @link["https://data-star.dev/reference/actions#delete"]{@tt{@"@"delete}} action string that sends a DELETE request.
}

@subsection{Pro Actions}

@link["https://data-star.dev/reference/actions#pro-actions"]{Pro actions} provide additional utilities for clipboard, interpolation, and internationalization.

@defproc[(clipboard [text string?]
                        [#:base64? base64? boolean?]) string?]{
Returns a @link["https://data-star.dev/reference/actions#clipboard"]{@tt{@"@"clipboard}} action string. Copies text to the clipboard. Omitting @racket[#:base64?] emits @tt{@"@"clipboard(text)}; passing @racket[#t] or @racket[#f] emits the explicit second boolean argument.

@examples[#:eval ev #:label #f
(clipboard "'Hello'")
(clipboard "'SGVsbG8='" #:base64? #t)
]
}

@defproc[(fit [v string?]
                  [old-min number?]
                  [old-max number?]
                  [new-min number?]
                  [new-max number?]
                  [#:clamp? clamp? boolean?]
                  [#:round? round? boolean?]) string?]{
Returns a @link["https://data-star.dev/reference/actions#fit"]{@tt{@"@"fit}} action string. Performs linear interpolation of @racket[v] from the range @racket[old-min]–@racket[old-max] to @racket[new-min]–@racket[new-max].

Omitting both keywords emits the 5-argument form. Passing only @racket[#:clamp?] emits one extra argument. Passing only @racket[#:round?] emits @racket[false] as the positional clamp placeholder before the round value.

@examples[#:eval ev #:label #f
(fit "$val" 0 100 0 1)
(fit "$x" 0 255 0 1 #:clamp? #t)
(fit "$x" 0 100 0 10 #:clamp? #t #:round? #t)
]
}

@defproc[(intl [type string?]
                   [value string?]
                   [#:options options hash?]
                   [#:locale locale string?]) string?]{
Returns a @link["https://data-star.dev/reference/actions#intl"]{@tt{@"@"intl}} action string. Formats values using the browser's @tt{Intl} APIs. The @racket[type] selects the formatter (e.g., @racket["'number'"], @racket["'datetime'"], @racket["'relativeTime'"]).

@examples[#:eval ev #:label #f
(intl "'number'" "$price")
(intl "'number'" "$price" #:options (hash 'style "currency" 'currency "USD"))
(intl "'datetime'" "$date" #:options (hash 'dateStyle "full") #:locale "'en-US'")
]
}

@subsection{Chaining Actions}

@defproc[(chain [first string?] [rest string?] ...) string?]{
Joins parts with @racket["; "] in order.

All parts must be strings; non-string values raise an argument error.

@examples[#:eval ev #:label #f
(chain "$loading = true" (post "/search"))
(chain "$a = 1" "$b = 2" "$c = 3")
]
}

@defproc[(chain/and [first string?] [rest string?] ...) string?]{
Joins parts with @racket[" && "] in order.

All parts must be strings; non-string values raise an argument error. Result keeps JavaScript short-circuit behavior.

@examples[#:eval ev #:label #f
(chain/and "$canSubmit" (post "/submit"))
(chain/and "$isReady" "$hasToken" "$canSync")
]
}
