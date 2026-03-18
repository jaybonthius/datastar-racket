#lang info

(define collection "datastar-brotli")

(define deps '("base" "datastar-lib" "libbrotli"))

(define build-deps '("scribble-lib" "racket-doc"))

(define scribblings '(("scribblings/datastar-brotli.scrbl")))

(define pkg-desc "Brotli compression support for the Datastar SDK")

(define version "0.1")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
