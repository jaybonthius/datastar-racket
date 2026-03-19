#lang info

(define collection "datastar")

(define scribblings '(("scribblings/datastar.scrbl")))

(define deps '("base"))

(define build-deps '("datastar-lib"
                     "net-doc"
                     "net-lib"
                     "rackunit-doc"
                     "rackunit-lib"
                     "scribble-lib"
                     "racket-doc"
                     "web-server-doc"
                     "web-server-lib"))

(define update-implies '("datastar-lib"))

(define pkg-desc "documentation part of \"datastar\"")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
