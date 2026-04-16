#lang scribble/doc

@(require (for-label datastar
                     datastar/testing
                     racket/base
                     racket/contract
                     rackunit)
          "datastar.rkt")

@title[#:tag "testing"]{Testing}

@defmodule[datastar/testing]

Mock SSE generators for testing Datastar handlers without a real HTTP connection. Require this module separately from @racket[datastar].

@defproc[(make-mock-sse) (values sse? (-> string?))]{
Creates a mock @racket[sse?] generator that works with all the normal send functions (@racket[patch-elements], @racket[patch-signals], etc.) but doesn't touch the network. Returns two values: the generator, and a thunk that returns all the SSE text that has been sent through it so far.

@examples[#:eval ev #:label #f
(define-values (sse get-output) (make-mock-sse))
(patch-elements/xexprs sse '(div ((id "x")) "hi"))
(get-output)
]
}

@defproc[(make-recording-sse) (values sse? (-> (listof sse-event?)))]{
Like @racket[make-mock-sse], but instead of returning raw text, the retrieval thunk returns a list of @racket[sse-event] structs, one per event sent.

The events are parsed from the same SSE text that would go over the wire, so they reflect exactly what a real client would receive, including serializer-side omission of default wire fields (for example mode @racket['outer], namespace @racket['html], @tt{useViewTransition false}, @tt{onlyIfMissing false}, and default retry).

@examples[#:eval ev #:label #f
(define-values (sse2 get-events) (make-recording-sse))
(patch-elements/xexprs sse2 '(div "test"))
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
  @item{@racket[retry] -- retry duration in milliseconds, or @racket[#f] if the default was used (default-equivalent retry is omitted by the serializer).}
  @item{@racket[data-lines] -- list of data line contents without the @tt{data: } prefix. For example, @racket['("elements <div>hello</div>")].}
]
}
