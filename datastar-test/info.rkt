#lang info
#|review: ignore|#

(define collection "tests")

(define deps '())

(define build-deps '("base" "datastar-lib" "net-lib" "rackunit-lib" "web-server-lib"))

(define update-implies '("datastar-lib"))

(define pkg-desc "tests for \"datastar\"")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
