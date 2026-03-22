#lang racket/base

(require net/tcp-sig
         racket/contract/base
         (prefix-in racket: racket/tcp)
         racket/unit
         web-server/http
         "private/constants.rkt"
         "private/sse.rkt")

(provide (contract-out [datastar-sse
                        (->* [request? (-> sse? any)] [#:on-close (or/c (-> sse? any) #f)] response?)]
                       [close-sse (-> sse? void?)]
                       [sse-closed? (-> sse? boolean?)]
                       [sse? (-> any/c boolean?)]
                       [call-with-sse-lock (-> sse? (-> any) any)])
         with-sse-lock
         datastar-tcp@)

(define current-datastar-input-port (make-parameter #f))

(define-unit datastar-tcp@
             (import)
             (export tcp^)
             (define tcp-listen racket:tcp-listen)
             (define tcp-listener? racket:tcp-listener?)
             (define tcp-close racket:tcp-close)
             (define tcp-connect racket:tcp-connect)
             (define tcp-connect/enable-break racket:tcp-connect/enable-break)
             (define tcp-accept-ready? racket:tcp-accept-ready?)
             (define tcp-addresses racket:tcp-addresses)
             (define tcp-abandon-port racket:tcp-abandon-port)
             (define (tcp-accept listener)
               (define-values (ip op) (racket:tcp-accept listener))
               (current-datastar-input-port ip)
               (values ip op))
             (define (tcp-accept/enable-break listener)
               (define-values (ip op) (racket:tcp-accept/enable-break listener))
               (current-datastar-input-port ip)
               (values ip op)))

(define (close-sse gen)
  (unless (unbox (sse-closed-box gen))
    (set-box! (sse-closed-box gen) #t)
    (with-handlers ([exn:fail? void])
      (close-output-port (sse-out gen)))))

(define (datastar-sse request on-open #:on-close [on-close #f])
  (define extra-headers
    (for/list ([(k v) (in-hash sse-headers)])
      (make-header (string->bytes/utf-8 k) (string->bytes/utf-8 v))))
  (define ip (current-datastar-input-port))
  (response 200
            #"OK"
            (current-seconds)
            #"text/event-stream"
            extra-headers
            (lambda (out)
              (define gen (make-sse out (make-semaphore 1) (box #f) (make-thread-cell #f #f)))
              (define on-open-thread (current-thread))
              (define monitor-thread
                (and ip
                     (thread (lambda ()
                               (sync ip)
                               (break-thread on-open-thread)))))
              (dynamic-wind void
                            (lambda ()
                              (with-handlers ([exn:break? void]
                                              [exn:fail? void])
                                (on-open gen)))
                            (lambda ()
                              (when monitor-thread
                                (kill-thread monitor-thread))
                              (close-sse gen)
                              (when on-close
                                (on-close gen)))))))

(define-syntax-rule (with-sse-lock gen body ...)
  (call-with-sse-lock gen
                      (lambda ()
                        body ...)))
