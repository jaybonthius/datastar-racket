#lang scribble/doc

@(require scribble/manual
          racket/file
          (for-label datastar
                     racket/base
                     racket/contract
                     web-server/http/request-structs
                     web-server/http/response-structs
                     json)
          scribble/examples)

@title{Datastar Racket SDK}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[datastar]

This package provides a Racket SDK for working with @link["https://data-star.dev"]{Datastar}.

@section{Usage}

Example usage (adapted from the @link["https://github.com/starfederation/datastar-go?tab=readme-ov-file#usage"]{Datastar Go SDK docs})

@codeblock{
#lang racket

(require web-server/dispatch
         web-server/servlet-dispatch
         web-server/http
         datastar
         json)

(struct store (message count) #:transparent)

(define (handler req)
  ; Read signals from the request
  (define signals-data (read-signals req))
  (define current-store
    (store (hash-ref signals-data 'message "")
           (hash-ref signals-data 'count 0)))

  ; Create a Server-Sent Event response
  (datastar-response
    (list
      ; Patch elements in the DOM
      (patch-elements "<div id=\"output\">Hello from Datastar!</div>")

      ; Remove elements from the DOM
      (remove-elements "#temporary-element")

      ; Patch signals (update client-side state)
      (patch-signals (hash 'message "Updated message"
                            'count (+ (store-count current-store) 1)))

      ; Execute JavaScript in the browser
      (execute-script "console.log(\"Hello from server!\")")

      ; Redirect the browser
      (redirect "/new-page"))))
}

For more advanced usage with streaming updates, you can use generators:

@codeblock{
(define (streaming-handler req)
  (datastar-response
    (in-generator
      (let loop ([i 0])
        (when (< i 10)
          (yield (patch-signals (hash 'counter i)))
          (yield (patch-elements
                   (format "<div id=\"counter\">Count: ~a</div>" i)))
          (sleep 1)
          (loop (+ i 1)))))))
}

For more examples, see the @link["https://github.com/jaybonthius/datastar-racket/tree/main/examples"]{examples directory} on GitHub.

@section{Methods}

@defproc[(datastar-response [events (or/c string? (sequence/c string?))]) response?]{
Creates an HTTP response with proper SSE headers for streaming Datastar events to the browser.
}

@defproc[(patch-elements [elements (or/c string? #f)]
                         [#:selector selector (or/c string? #f) #f]
                         [#:mode mode (or/c string? #f) #f]
                         [#:namespace namespace (or/c string? #f) #f]
                         [#:use-view-transitions use-view-transitions (or/c boolean? #f) #f]
                         [#:event-id event-id (or/c string? #f) #f]
                         [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) string?]{
Patches HTML elements into the DOM. Supports various patch modes including outer, inner, replace, prepend, append, before, after, and remove. The @racket[namespace] parameter can be used to specify SVG or MathML namespaces when patching elements.
}

@defproc[(remove-elements [selector string?]
                          [#:event-id event-id (or/c string? #f) #f]
                          [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) string?]{
Removes elements from the DOM by CSS selector. This is a convenience function that calls @racket[patch-elements] with mode @tt{remove}.
}

@defproc[(read-signals [request request?]) jsexpr?]{
Parses incoming signal data from the browser. For GET requests, extracts data from the @tt{datastar} query parameter. For other methods, parses the request body as JSON.
}

@defproc[(patch-signals [signals (or/c string? jsexpr?)]
                        [#:event-id event-id (or/c string? #f) #f]
                        [#:only-if-missing only-if-missing (or/c boolean? #f) #f]
                        [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) string?]{
Patches signals into the signal store using RFC 7386 JSON Merge Patch semantics. Supports add/update operations, removal by setting to @tt{null}, and nested recursive patching.
}

@defproc[(execute-script [script string?]
                         [#:auto-remove auto-remove boolean? #t]
                         [#:attributes attributes (or/c (hash/c symbol? any/c) (listof string?) #f) #f]
                         [#:event-id event-id (or/c string? #f) #f]
                         [#:retry-duration retry-duration (or/c exact-positive-integer? #f) #f]) string?]{
Executes JavaScript in the browser by injecting a script tag. The script is automatically removed after execution unless auto-remove is @racket[#f].
}

@defproc[(redirect [location string?]) string?]{
Redirects the browser to a new location using @tt{window.location}. This is a convenience function that calls @racket[execute-script].
}

@section{Constants}

@subsection{Element Patch Modes}

An element patch mode is the mode in which an element is patched into the DOM.

@defthing[ELEMENT-PATCH-MODE-OUTER string? #:value "outer"]{
Morphs the element into the existing element.
}

@defthing[ELEMENT-PATCH-MODE-INNER string? #:value "inner"]{
Replaces the inner HTML of the existing element.
}

@defthing[ELEMENT-PATCH-MODE-REMOVE string? #:value "remove"]{
Removes the existing element.
}

@defthing[ELEMENT-PATCH-MODE-REPLACE string? #:value "replace"]{
Replaces the existing element with the new element.
}

@defthing[ELEMENT-PATCH-MODE-PREPEND string? #:value "prepend"]{
Prepends the element inside to the existing element.
}

@defthing[ELEMENT-PATCH-MODE-APPEND string? #:value "append"]{
Appends the element inside the existing element.
}

@defthing[ELEMENT-PATCH-MODE-BEFORE string? #:value "before"]{
Inserts the element before the existing element.
}

@defthing[ELEMENT-PATCH-MODE-AFTER string? #:value "after"]{
Inserts the element after the existing element.
}

@subsection{Event Types}

@defthing[EVENT-TYPE-PATCH-ELEMENTS string? #:value "datastar-patch-elements"]{
An event for patching HTML elements into the DOM.
}

@defthing[EVENT-TYPE-PATCH-SIGNALS string? #:value "datastar-patch-signals"]{
An event for patching signals.
}
