#lang scribble/doc

@(require scribble/manual
          (for-label datastar-brotli
                     datastar
                     racket/base
                     racket/contract))

@title{Datastar Brotli Compression}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[datastar-brotli]

This package provides @link["https://en.wikipedia.org/wiki/Brotli"]{Brotli} compression
support for the @racketmodname[datastar] SDK. Pass the resulting write profile to
@racket[datastar-sse] via the @racket[#:write-profile] keyword argument to compress SSE
events on the wire. The SDK automatically falls back to uncompressed output when the client
does not advertise @tt{br} in its @tt{Accept-Encoding} header.

@section{Usage}

@codeblock{
(require datastar
         datastar-brotli)

(define brotli-profile (make-brotli-write-profile #:quality 5))

(define (handler req)
  (datastar-sse req
    (lambda (sse)
      (patch-elements sse "<div id=\"out\">Hello, compressed!</div>"))
    #:write-profile brotli-profile))
}

@section{API}

@defproc[(make-brotli-write-profile [#:quality quality (integer-in 0 11) 5]
                                    [#:window window (integer-in 10 24) 22]
                                    [#:mode mode exact-nonneg-integer? BROTLI_MODE_TEXT]) write-profile?]{
Creates a @racket[write-profile?] that compresses SSE output with Brotli.

@itemlist[
  @item{@racket[quality] --- Compression level from @racket[0] (fastest) to @racket[11]
        (smallest). Default @racket[5] is a reasonable balance for streaming.}
  @item{@racket[window] --- Sliding window size from @racket[10] to @racket[24].
        Larger values may improve compression at the cost of memory.}
  @item{@racket[mode] --- One of @racket[BROTLI_MODE_GENERIC], @racket[BROTLI_MODE_TEXT],
        or @racket[BROTLI_MODE_FONT]. Use @racket[BROTLI_MODE_TEXT] (the default) for
        SSE, which is UTF-8 text.}
]
}

@defthing[BROTLI_MODE_GENERIC exact-nonneg-integer?]{
Generic mode, no assumptions about content type. Re-exported from @tt{libbrotli}.
}

@defthing[BROTLI_MODE_TEXT exact-nonneg-integer?]{
Text mode, optimized for UTF-8 input. Re-exported from @tt{libbrotli}.
}

@defthing[BROTLI_MODE_FONT exact-nonneg-integer?]{
Font mode, optimized for WOFF 2.0 fonts. Re-exported from @tt{libbrotli}.
}
