#lang scribble/doc

@(require (for-label datastar
                     racket/base
                     racket/contract)
          scribble/manual
          "datastar.rkt")

@title[#:tag "datastar-racket-sdk"]{Datastar Racket SDK}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[datastar]

@link["https://data-star.dev"]{Datastar} is a lightweight framework for building everything from simple sites to real-time collaborative web applications.

The Racket SDK for Datastar provides server-side helpers for reading signals from requests and sending SSE events, as well as client-side helpers for generating Datastar attributes and action expressions.

This reference is organized into three parts:

@itemlist[
  @item{@bold{@seclink["http"]{HTTP}} --- Read datastar signals from incoming HTTP requests and send SSE or one-shot responses.}
  @item{@bold{@seclink["sugar"]{Sugar}} --- Generate Datastar @tt{data-*} attributes and action expressions for x-expressions.}
  @item{@bold{@seclink["testing"]{Testing}} --- Test handlers with mock SSE generators and parsed event values.}
]

@local-table-of-contents[]

@include-section["http.scrbl"]
@include-section["sugar.scrbl"]
@include-section["testing.scrbl"]
