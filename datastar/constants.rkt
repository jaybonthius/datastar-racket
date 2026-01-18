#lang racket/base

(provide (all-defined-out))

(define DATASTAR-KEY "datastar")
(define VERSION "1.0.0-RC.7")

(define SSE-HEADERS
  (hash "Cache-Control"
        "no-cache"
        "Content-Type"
        "text/event-stream"
        "Connection"
        "keep-alive"
        "X-Accel-Buffering"
        "no"))

; The mode in which an element is patched into the DOM.
(define ELEMENT-PATCH-MODE-OUTER "outer")
(define ELEMENT-PATCH-MODE-INNER "inner")
(define ELEMENT-PATCH-MODE-REMOVE "remove")
(define ELEMENT-PATCH-MODE-REPLACE "replace")
(define ELEMENT-PATCH-MODE-PREPEND "prepend")
(define ELEMENT-PATCH-MODE-APPEND "append")
(define ELEMENT-PATCH-MODE-BEFORE "before")
(define ELEMENT-PATCH-MODE-AFTER "after")

; The type protocol on top of SSE which allows for core pushed based communication between the server and the client.
(define EVENT-TYPE-PATCH-ELEMENTS "datastar-patch-elements")
(define EVENT-TYPE-PATCH-SIGNALS "datastar-patch-signals")

; The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
(define DEFAULT-SSE-RETRY-DURATION 1000)

; Dataline literals
(define SELECTOR-DATALINE-LITERAL "selector")
(define MODE-DATALINE-LITERAL "mode")
(define NAMESPACE-DATALINE-LITERAL "namespace")
(define ELEMENTS-DATALINE-LITERAL "elements")
(define USE-VIEW-TRANSITION-DATALINE-LITERAL "useViewTransition")
(define SIGNALS-DATALINE-LITERAL "signals")
(define ONLY-IF-MISSING-DATALINE-LITERAL "onlyIfMissing")

; Should elements be patched using the ViewTransition API?
(define DEFAULT-ELEMENTS-USE-VIEW-TRANSITIONS #f)
; Should a given set of signals patch if they are missing?
(define DEFAULT-PATCH-SIGNALS-ONLY-IF-MISSING #f)
