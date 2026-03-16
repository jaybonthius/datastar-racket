#lang racket/base

;; Brotli compression write profile for Datastar SSE.
;;
;; This module is an optional extension — it requires the `libbrotli` package.
;; Users who do not require this module will never load the brotli native library.
;;
;; Usage:
;;   (require datastar datastar/brotli)
;;
;;   (define bp (make-brotli-write-profile #:quality 5))
;;
;;   (define (handler request)
;;     (datastar-sse request
;;                   (lambda (sse)
;;                     (patch-elements sse "<div>compressed!</div>"))
;;                   #:write-profile bp))

(require racket/contract/base
         libbrotli
         "sse.rkt")

(provide (contract-out
          [make-brotli-write-profile
           (->* () (#:quality quality/c #:window window/c #:mode mode/c) write-profile?)])
         ;; Re-export libbrotli constants for convenience so users don't need
         ;; to require libbrotli directly just to set mode.
         BROTLI_MODE_GENERIC
         BROTLI_MODE_TEXT
         BROTLI_MODE_FONT)

(define BROTLI-CONTENT-ENCODING "br")

;; Creates a write profile that compresses SSE output with brotli.
;;
;; The defaults are tuned for real-time SSE streaming:
;;   quality 5  — good balance of compression ratio and speed
;;   window  22 — ~4MB window, matches libbrotli default
;;   mode    TEXT — optimized for UTF-8 text (SSE events)
;;
;; The brotli output port is created with #:close? #f so that closing
;; the compression port finalizes the brotli stream without closing the
;; underlying response port (which the web server manages).
(define (make-brotli-write-profile #:quality [quality 5]
                                   #:window [window 22]
                                   #:mode [mode BROTLI_MODE_TEXT])
  (make-write-profile
   ;; wrap-output: wrap raw port with a streaming brotli compression port
   (lambda (raw-out)
     (open-brotli-output raw-out #:quality quality #:window window #:mode mode #:close? #f))
   ;; flush!: double flush — brotli port then raw port
   ;; Flushing the brotli port compresses buffered data and writes compressed
   ;; bytes to the underlying port. Flushing the raw port pushes those bytes
   ;; over the network. Both are needed for real-time SSE delivery.
   (lambda (wrapped raw)
     (flush-output wrapped)
     (flush-output raw))
   ;; content-encoding
   BROTLI-CONTENT-ENCODING))
