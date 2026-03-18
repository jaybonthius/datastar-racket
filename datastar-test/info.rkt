#lang info

(define collection "tests")

(define deps '())

(define build-deps
  '("base" "datastar-brotli" "datastar-lib" "libbrotli" "net-lib" "rackunit-lib" "web-server-lib"))

(define update-implies '("datastar-lib"))

(define pkg-desc "tests for \"datastar\"")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
