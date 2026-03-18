#lang info

(define collection "datastar-brotli")

(define deps '("base" "datastar" "libbrotli"))

(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))

(define scribblings '(("scribblings/datastar-brotli.scrbl")))

(define compile-omit-paths '("tests"))
(define test-omit-paths '("tests"))

(define pkg-desc "Brotli compression support for the Datastar SDK")

(define version "0.1")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
