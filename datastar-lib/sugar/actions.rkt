#lang racket/base

(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/string)

(provide (contract-out
          [chain (->* [string?] [] #:rest (listof string?) string?)]
          [chain/and (->* [string?] [] #:rest (listof string?) string?)]
          [peek (-> string? string?)]
          [set-all (->* [string?] [#:include (or/c string? #f) #:exclude (or/c string? #f)] string?)]
          [toggle-all (->* [] [#:include (or/c string? #f) #:exclude (or/c string? #f)] string?)]
          [get
           (->* [string?]
                [#:content-type (or/c 'json 'form #f)
                 #:filter-signals-include (or/c string? #f)
                 #:filter-signals-exclude (or/c string? #f)
                 #:selector (or/c string? #f)
                 #:headers (or/c hash? #f)
                 #:open-when-hidden? boolean?
                 #:payload (or/c string? #f)
                 #:retry (or/c 'auto 'error 'always 'never #f)
                 #:retry-interval (or/c exact-nonnegative-integer? #f)
                 #:retry-scaler (or/c number? #f)
                 #:retry-max-wait-ms (or/c exact-nonnegative-integer? #f)
                 #:retry-max-count (or/c exact-nonnegative-integer? #f)
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string? #f)]
                string?)]
          [post
           (->* [string?]
                [#:content-type (or/c 'json 'form #f)
                 #:filter-signals-include (or/c string? #f)
                 #:filter-signals-exclude (or/c string? #f)
                 #:selector (or/c string? #f)
                 #:headers (or/c hash? #f)
                 #:open-when-hidden? boolean?
                 #:payload (or/c string? #f)
                 #:retry (or/c 'auto 'error 'always 'never #f)
                 #:retry-interval (or/c exact-nonnegative-integer? #f)
                 #:retry-scaler (or/c number? #f)
                 #:retry-max-wait-ms (or/c exact-nonnegative-integer? #f)
                 #:retry-max-count (or/c exact-nonnegative-integer? #f)
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string? #f)]
                string?)]
          [put
           (->* [string?]
                [#:content-type (or/c 'json 'form #f)
                 #:filter-signals-include (or/c string? #f)
                 #:filter-signals-exclude (or/c string? #f)
                 #:selector (or/c string? #f)
                 #:headers (or/c hash? #f)
                 #:open-when-hidden? boolean?
                 #:payload (or/c string? #f)
                 #:retry (or/c 'auto 'error 'always 'never #f)
                 #:retry-interval (or/c exact-nonnegative-integer? #f)
                 #:retry-scaler (or/c number? #f)
                 #:retry-max-wait-ms (or/c exact-nonnegative-integer? #f)
                 #:retry-max-count (or/c exact-nonnegative-integer? #f)
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string? #f)]
                string?)]
          [patch
           (->* [string?]
                [#:content-type (or/c 'json 'form #f)
                 #:filter-signals-include (or/c string? #f)
                 #:filter-signals-exclude (or/c string? #f)
                 #:selector (or/c string? #f)
                 #:headers (or/c hash? #f)
                 #:open-when-hidden? boolean?
                 #:payload (or/c string? #f)
                 #:retry (or/c 'auto 'error 'always 'never #f)
                 #:retry-interval (or/c exact-nonnegative-integer? #f)
                 #:retry-scaler (or/c number? #f)
                 #:retry-max-wait-ms (or/c exact-nonnegative-integer? #f)
                 #:retry-max-count (or/c exact-nonnegative-integer? #f)
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string? #f)]
                string?)]
          [delete
           (->* [string?]
                [#:content-type (or/c 'json 'form #f)
                 #:filter-signals-include (or/c string? #f)
                 #:filter-signals-exclude (or/c string? #f)
                 #:selector (or/c string? #f)
                 #:headers (or/c hash? #f)
                 #:open-when-hidden? boolean?
                 #:payload (or/c string? #f)
                 #:retry (or/c 'auto 'error 'always 'never #f)
                 #:retry-interval (or/c exact-nonnegative-integer? #f)
                 #:retry-scaler (or/c number? #f)
                 #:retry-max-wait-ms (or/c exact-nonnegative-integer? #f)
                 #:retry-max-count (or/c exact-nonnegative-integer? #f)
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string? #f)]
                string?)]
          [clipboard (->* [string?] [#:base64? boolean?] string?)]
          [fit
           (->* [string? number? number? number? number?]
                [#:clamp? boolean? #:round? boolean?]
                string?)]
          [intl
           (->* [string? string?] [#:options (or/c hash? #f) #:locale (or/c string? #f)] string?)]))

;; internal helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These produce JS expression syntax (unquoted keys, regex literals)
;; rather than JSON syntax (quoted keys).

(define (js-value v)
  (cond
    [(boolean? v) (if v "true" "false")]
    [(number? v) (number->string v)]
    [(symbol? v) (string-append "'" (symbol->string v) "'")]
    [(string? v) v]
    [else (jsexpr->string v)]))

(define (js-object-from-hash h)
  (string-join (for/list ([(k v) (in-hash h)])
                 (define key-str
                   (if (symbol? k)
                       (symbol->string k)
                       k))
                 (define val-str
                   (cond
                     [(string? v) (jsexpr->string v)]
                     [(hash? v) (js-object-from-hash v)]
                     [else (js-value v)]))
                 (string-append (jsexpr->string key-str) ": " val-str))
               ", "
               #:before-first "{"
               #:after-last "}"))

(define (build-filter-signals include exclude)
  (define parts
    (append (if include
                (list (string-append "include: " include))
                '())
            (if exclude
                (list (string-append "exclude: " exclude))
                '())))
  (string-join parts ", " #:before-first "{" #:after-last "}"))

(define (build-options-string content-type ;; review: ignore
                              fs-include
                              fs-exclude
                              selector
                              headers
                              open-when-hidden?
                              payload
                              retry
                              retry-interval
                              retry-scaler
                              retry-max-wait-ms
                              retry-max-count
                              request-cancellation)
  (define parts
    (filter values
            (list (and content-type (string-append "contentType: " (js-value content-type)))
                  (and (or fs-include fs-exclude)
                       (string-append "filterSignals: " (build-filter-signals fs-include fs-exclude)))
                  (and selector (string-append "selector: " (jsexpr->string selector)))
                  (and headers (string-append "headers: " (js-object-from-hash headers)))
                  (and (not (eq? open-when-hidden? 'unset))
                       (string-append "openWhenHidden: " (js-value open-when-hidden?)))
                  (and payload (string-append "payload: " payload))
                  (and retry (string-append "retry: " (js-value retry)))
                  (and retry-interval (string-append "retryInterval: " (js-value retry-interval)))
                  (and retry-scaler (string-append "retryScaler: " (js-value retry-scaler)))
                  (and retry-max-wait-ms
                       (string-append "retryMaxWaitMs: " (js-value retry-max-wait-ms)))
                  (and retry-max-count (string-append "retryMaxCount: " (js-value retry-max-count)))
                  (and request-cancellation
                       (string-append "requestCancellation: " (js-value request-cancellation))))))
  (if (null? parts)
      #f
      (string-join parts ", " #:before-first "{" #:after-last "}")))

;; backend actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-action-method name method-str)
  (define (name url
                #:content-type [content-type #f]
                #:filter-signals-include [fs-include #f]
                #:filter-signals-exclude [fs-exclude #f]
                #:selector [selector #f]
                #:headers [headers #f]
                #:open-when-hidden? [open-when-hidden? 'unset]
                #:payload [payload #f]
                #:retry [retry #f]
                #:retry-interval [retry-interval #f]
                #:retry-scaler [retry-scaler #f]
                #:retry-max-wait-ms [retry-max-wait-ms #f]
                #:retry-max-count [retry-max-count #f]
                #:request-cancellation [request-cancellation #f])
    (define opts-str
      (build-options-string content-type
                            fs-include
                            fs-exclude
                            selector
                            headers
                            open-when-hidden?
                            payload
                            retry
                            retry-interval
                            retry-scaler
                            retry-max-wait-ms
                            retry-max-count
                            request-cancellation))
    (if opts-str
        (format "@~a('~a', ~a)" method-str url opts-str)
        (format "@~a('~a')" method-str url))))

(define-action-method get "get")
(define-action-method post "post")
(define-action-method put "put")
(define-action-method patch "patch")
(define-action-method delete "delete")

;; signal actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (peek callable)
  (format "@peek(~a)" callable))

(define (set-all value #:include [include #f] #:exclude [exclude #f])
  (if (or include exclude)
      (format "@setAll(~a, ~a)" value (build-filter-signals include exclude))
      (format "@setAll(~a)" value)))

(define (toggle-all #:include [include #f] #:exclude [exclude #f])
  (if (or include exclude)
      (format "@toggleAll(~a)" (build-filter-signals include exclude))
      "@toggleAll()"))

;; pro actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clipboard text #:base64? [base64? 'unset])
  (if (eq? base64? 'unset)
      (format "@clipboard(~a)" text)
      (format "@clipboard(~a, ~a)" text (js-value base64?))))

(define (fit v old-min old-max new-min new-max #:clamp? [clamp? 'unset] #:round? [round? 'unset])
  (define base (format "@fit(~a, ~a, ~a, ~a, ~a" v old-min old-max new-min new-max))
  (cond
    [(and (eq? clamp? 'unset) (eq? round? 'unset)) (string-append base ")")]
    [(eq? round? 'unset) (format "~a, ~a)" base (js-value clamp?))]
    [(eq? clamp? 'unset) (format "~a, false, ~a)" base (js-value round?))]
    [else (format "~a, ~a, ~a)" base (js-value clamp?) (js-value round?))]))

(define (intl type value #:options [options #f] #:locale [locale #f])
  (cond
    [(and (not options) (not locale)) (format "@intl(~a, ~a)" type value)]
    [(not locale) (format "@intl(~a, ~a, ~a)" type value (js-object-from-hash options))]
    [(not options) (format "@intl(~a, ~a, {}, ~a)" type value locale)]
    [else (format "@intl(~a, ~a, ~a, ~a)" type value (js-object-from-hash options) locale)]))

;; chaining actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (join-chain-parts separator first rest)
  (string-join (cons first rest) separator))

(define (chain first . rest)
  (join-chain-parts "; " first rest))

(define (chain/and first . rest)
  (join-chain-parts " && " first rest))
