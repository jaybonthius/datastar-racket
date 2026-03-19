#lang racket

(require datastar
         datastar-brotli
         (only-in datastar/private/sse make-sse)
         libbrotli
         rackunit)

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-brotli-test-sse #:quality [quality 5]
                              #:window [window 22]
                              #:mode [mode BROTLI_MODE_TEXT])
  (define raw-out (open-output-bytes))
  (define wrapped-out
    (open-brotli-output raw-out #:quality quality #:window window #:mode mode #:close? #f))
  (define gen
    (make-sse wrapped-out
              raw-out
              (lambda (wrapped raw)
                (flush-output wrapped)
                (flush-output raw))
              (make-semaphore 1)
              (box #f)
              (make-thread-cell #f #f)))
  (values gen wrapped-out raw-out))

(define (decompress-output raw-out)
  (bytes->string/utf-8 (brotli-decompress (get-output-bytes raw-out))))

;; write profile construction tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "make-brotli-write-profile returns a write-profile"
  (define bp (make-brotli-write-profile))
  (check-true (write-profile? bp) "should be a write-profile"))

(test-case "make-brotli-write-profile accepts quality, window, mode"
  (check-not-exn (lambda ()
                   (make-brotli-write-profile #:quality 1 #:window 16 #:mode BROTLI_MODE_GENERIC))
                 "should accept valid options without error"))

;; compression round-trip tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "brotli: single event round-trip"
  (define-values (gen wrapped raw) (make-brotli-test-sse))
  (patch-elements gen "<div id=\"out\">hello</div>")
  (close-output-port wrapped)
  (flush-output raw)
  (define result (decompress-output raw))
  (check-true (string-contains? result "event: datastar-patch-elements")
              "decompressed output should contain event type")
  (check-true (string-contains? result "data: elements <div id=\"out\">hello</div>")
              "decompressed output should contain elements data"))

(test-case "brotli: multiple events round-trip"
  (define-values (gen wrapped raw) (make-brotli-test-sse))
  (patch-elements gen "<div>first</div>")
  (patch-elements gen "<div>second</div>")
  (patch-signals gen "{\"count\":42}")
  (close-output-port wrapped)
  (flush-output raw)
  (define result (decompress-output raw))
  (check-true (string-contains? result "first") "should contain first event")
  (check-true (string-contains? result "second") "should contain second event")
  (check-true (string-contains? result "datastar-patch-signals") "should contain signals event")
  (check-true (string-contains? result "42") "should contain signal value"))

(test-case "brotli: execute-script round-trip"
  (define-values (gen wrapped raw) (make-brotli-test-sse))
  (execute-script gen "console.log('compressed')")
  (close-output-port wrapped)
  (flush-output raw)
  (define result (decompress-output raw))
  (check-true (string-contains? result "console.log('compressed')")
              "decompressed output should contain script content")
  (check-true (string-contains? result "<script") "should have script tag"))

(test-case "brotli: flush produces decodable output mid-stream"
  ;; Each event must be decodable after flush, not just after closing the stream.
  (define raw-out (open-output-bytes))
  (define bport (open-brotli-output raw-out #:quality 1 #:close? #f))

  (write-string "event: datastar-patch-elements\ndata: elements <div>hello</div>\n\n" bport)
  (flush-output bport)
  (flush-output raw-out)
  (check-true (> (bytes-length (get-output-bytes raw-out)) 0)
              "flushing should produce compressed bytes immediately")

  (write-string "event: datastar-patch-signals\ndata: signals {\"count\":1}\n\n" bport)
  (flush-output bport)
  (flush-output raw-out)

  (close-output-port bport)
  (flush-output raw-out)

  (define compressed (get-output-bytes raw-out))
  (define result (bytes->string/utf-8 (brotli-decompress compressed)))
  (check-true (string-contains? result "datastar-patch-elements") "should contain first event type")
  (check-true (string-contains? result "datastar-patch-signals") "should contain second event type")
  (check-true (string-contains? result "<div>hello</div>") "should contain first event data")
  (check-true (string-contains? result "{\"count\":1}") "should contain second event data"))

(test-case "brotli: sliding window reduces incremental cost of repeated SSE events"
  (define-values (gen wrapped raw) (make-brotli-test-sse #:quality 5 #:window 22))

  (define sizes
    (for/list ([n (in-range 5)])
      (define before (bytes-length (get-output-bytes raw)))
      (patch-elements gen (format "<div id=\"out\">event ~a</div>" n))
      (define after (bytes-length (get-output-bytes raw)))
      (- after before)))

  (close-output-port wrapped)
  (flush-output raw)
  (define result (decompress-output raw))
  (for ([n (in-range 5)])
    (check-true (string-contains? result (format "event ~a" n))))

  (define first-size (first sizes))
  (define avg-later (/ (apply + (rest sizes)) (length (rest sizes))))
  (check-true (< avg-later first-size)
              (format "avg later event size ~a should be < first event size ~a"
                      (exact->inexact avg-later)
                      first-size)))

(test-case "brotli: different quality levels all produce valid output"
  (for ([q (in-range 0 12)])
    (define-values (gen wrapped raw) (make-brotli-test-sse #:quality q))
    (patch-elements gen (format "<div>quality-~a</div>" q))
    (close-output-port wrapped)
    (flush-output raw)
    (define result (decompress-output raw))
    (check-true (string-contains? result (format "quality-~a" q))
                (format "quality ~a should produce valid compressed output" q))))

(test-case "brotli: close-sse finalizes compression stream"
  (define-values (gen _wrapped raw) (make-brotli-test-sse))
  (patch-elements gen "<div>before-close</div>")
  (close-sse gen)
  (define result (decompress-output raw))
  (check-true (string-contains? result "before-close")
              "data sent before close should be decompressible"))

(test-case "brotli: send raises after close-sse"
  (define-values (gen _wrapped _raw) (make-brotli-test-sse))
  (close-sse gen)
  (check-exn exn:fail?
             (lambda () (patch-elements gen "<div>after-close</div>"))
             "should raise after close-sse"))
