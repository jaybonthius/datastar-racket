#lang scribble/doc

@(require (for-label datastar
                     racket/base
                     racket/contract)
          scribble/manual
          "datastar.rkt")

@title{Datastar Racket SDK}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[datastar]

Datastar is a lightweight hypermedia framework that provides backend reactivity like htmx and frontend reactivity like Alpine.js, without requiring npm packages or other dependencies.

In @link["https://data-star.dev"]{Datastar}, the JavaScript client handles frontend reactivity, while the server drives UI updates through Datastar-compatible HTTP responses and SSE.

@itemlist[
  @item{Attributes bind signals, handle events, and add reactivity. Actions like @tt|{@get()}| make requests to the server and communicate the frontend state via signals.}
  @item{The backend can stream SSE events or send one-shot responses to patch the DOM, update signals, and run scripts.}
]

The Racket SDK provides server-side helpers for reading signals from requests and sending SSE events, as well as client-side helpers for generating @tt{data-*} attributes and action expressions.

The table below maps common features to this SDK reference and, if relevant, corresponding Datastar documentation.

@tabular[#:sep @hspace[2]
         #:style 'boxed
         #:row-properties '(bottom-border ())
         (list
          (list @bold{Feature} @bold{SDK docs} @bold{Datastar docs})
          (list "Backend requests"
                @seclink["requests"]{HTTP: Requests}
                @link["https://data-star.dev/guide/backend_requests"]{Backend Requests})
          (list "SSE lifecycle"
                @seclink["sse"]{HTTP: SSE}
                "-")
          (list "Element patches"
                @seclink["elements"]{HTTP: Elements}
                @link["https://data-star.dev/reference/sse_events#datastar-patch-elements"]{Patch Elements event})
          (list "Signal patches"
                @seclink["signals"]{HTTP: Signals}
                @link["https://data-star.dev/reference/sse_events#datastar-patch-signals"]{Patch Signals event})
          (list "Script execution"
                @seclink["scripts"]{HTTP: Scripts}
                @link["https://data-star.dev/guide/datastar_expressions#executing-scripts"]{Executing Scripts})
          (list "One-shot responses"
                @seclink["one-shot-responses"]{HTTP: One-Shot Responses}
                @link["https://data-star.dev/reference/actions#response-handling"]{Response Handling})
          (list "Data attributes"
                @seclink["attributes"]{Sugar: Attributes}
                @link["https://data-star.dev/reference/attributes"]{Attributes})
          (list "Actions"
                @seclink["actions"]{Sugar: Actions}
                @link["https://data-star.dev/reference/actions"]{Actions})
          (list "Testing"
                @seclink["testing"]{Testing}
                "-"))]

@local-table-of-contents[]

@include-section["http.scrbl"]
@include-section["sugar.scrbl"]
@include-section["testing.scrbl"]
