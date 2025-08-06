#lang info

(define collection "datastar")

(define deps '("base" "web-server-lib" "net-lib"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define test-include-paths '("tests/sdk-tests/sdk-test-runner.rkt"))

(define scribblings '(("scribblings/datastar.scrbl")))

(define pkg-desc "Datastar SDK for Racket")

(define version "0.1.0")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
