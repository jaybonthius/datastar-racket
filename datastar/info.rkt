#lang info

(define collection "datastar")

(define deps '("base" "web-server-lib" "net-lib"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "web-server-doc"))

(define scribblings '(("scribblings/datastar.scrbl")))

(define compile-omit-paths '("tests"))
(define test-omit-paths '("tests"))

(define pkg-desc "Datastar SDK for Racket")

(define version "0.2")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
