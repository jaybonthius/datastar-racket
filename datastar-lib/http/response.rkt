#lang racket/base

(require json
         racket/contract/base
         web-server/http
         xml
         "../private/constants.rkt"
         "../private/utils.rkt"
         "../private/xexpr.rkt")

(define extra-headers/c (hash/c string? string?))
(define script-attributes/c (hash/c (or/c symbol? string?) any/c))

(define element-patch-mode/c (or/c 'outer 'inner 'remove 'replace 'prepend 'append 'before 'after))
(define element-namespace/c (or/c 'html 'svg 'mathml))

(provide (contract-out [response/datastar-elements
                        (->* [(or/c string? bytes?)]
                             [#:selector string?
                              #:mode element-patch-mode/c
                              #:namespace element-namespace/c
                              #:use-view-transitions? boolean?
                              #:status exact-positive-integer?
                              #:headers extra-headers/c]
                             response?)]
                       [response/datastar-elements/xexprs
                        (->* [(or/c xexpr/c (listof xexpr/c))]
                             [#:selector string?
                              #:mode element-patch-mode/c
                              #:namespace element-namespace/c
                              #:use-view-transitions? boolean?
                              #:status exact-positive-integer?
                              #:headers extra-headers/c]
                             response?)]
                       [response/datastar-signals
                        (->* [(or/c string? bytes? jsexpr?)]
                             [#:only-if-missing? boolean?
                              #:status exact-positive-integer?
                              #:headers extra-headers/c]
                             response?)]
                       [response/datastar-script
                        (->* [(or/c string? bytes?)]
                             [#:attributes script-attributes/c
                              #:status exact-positive-integer?
                              #:headers extra-headers/c]
                             response?)]))

;; internal helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (->bytes v)
  (if (bytes? v)
      v
      (string->bytes/utf-8 v)))

(define (->header key value)
  (make-header (string->bytes/utf-8 key) (string->bytes/utf-8 value)))

(define (append-extra-headers datastar-headers extra-headers)
  (append datastar-headers
          (if extra-headers
              (for/list ([(k v) (in-hash extra-headers)])
                (->header k v))
              '())))

(define (make-datastar-response content-type
                                body
                                datastar-headers
                                #:status [status 200]
                                #:headers [extra-headers #f])
  (response/full status
                 #"OK"
                 (current-seconds)
                 (string->bytes/utf-8 content-type)
                 (append-extra-headers datastar-headers extra-headers)
                 (list (->bytes body))))

(define (normalize-script-attributes attributes)
  (for/hash ([(k v) (in-hash attributes)])
    (values (if (symbol? k)
                k
                (string->symbol k))
            v)))

;; one-shot response API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (response/datastar-elements elements
                                    #:selector [selector #f]
                                    #:mode [mode default-element-patch-mode]
                                    #:namespace [namespace default-element-namespace]
                                    #:use-view-transitions?
                                    [use-view-transitions? default-elements-use-view-transitions]
                                    #:status [status 200]
                                    #:headers [headers #f])
  (define datastar-headers
    (filter values
            (list (and selector (->header "datastar-selector" selector))
                  (and (not (eq? mode default-element-patch-mode))
                       (->header "datastar-mode" (symbol->string mode)))
                  (and (not (eq? namespace default-element-namespace))
                       (->header "datastar-namespace" (symbol->string namespace)))
                  (and (not (eq? use-view-transitions? default-elements-use-view-transitions))
                       (->header "datastar-use-view-transition" (js-bool use-view-transitions?))))))
  (make-datastar-response "text/html" elements datastar-headers #:status status #:headers headers))

(define (response/datastar-elements/xexprs xexpr-or-xexprs
                                           #:selector [selector #f]
                                           #:mode [mode default-element-patch-mode]
                                           #:namespace [namespace default-element-namespace]
                                           #:use-view-transitions?
                                           [use-view-transitions? default-elements-use-view-transitions]
                                           #:status [status 200]
                                           #:headers [headers #f])
  (response/datastar-elements (xexpr-or-xexprs->html xexpr-or-xexprs)
                              #:selector selector
                              #:mode mode
                              #:namespace namespace
                              #:use-view-transitions? use-view-transitions?
                              #:status status
                              #:headers headers))

(define (response/datastar-signals signals
                                   #:only-if-missing?
                                   [only-if-missing? default-patch-signals-only-if-missing]
                                   #:status [status 200]
                                   #:headers [headers #f])
  (define body
    (cond
      [(bytes? signals) signals]
      [(string? signals) signals]
      [else (jsexpr->string signals)]))
  (define datastar-headers
    (filter values
            (list (and (not (eq? only-if-missing? default-patch-signals-only-if-missing))
                       (->header "datastar-only-if-missing" (js-bool only-if-missing?))))))
  (make-datastar-response "application/json" body datastar-headers #:status status #:headers headers))

(define (response/datastar-script script
                                  #:attributes [attributes #f]
                                  #:status [status 200]
                                  #:headers [headers #f])
  (define datastar-headers
    (filter values
            (list (and attributes
                       (->header "datastar-script-attributes"
                                 (jsexpr->string (normalize-script-attributes attributes)))))))
  (make-datastar-response "text/javascript"
                          script
                          datastar-headers
                          #:status status
                          #:headers headers))
