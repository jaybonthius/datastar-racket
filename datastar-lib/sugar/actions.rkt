#lang racket/base

(require (for-syntax racket/base)
         json
         racket/contract/base
         racket/string)

(provide (contract-out
          [chain (->* [string?] [] #:rest (listof string?) string?)]
          [chain/and (->* [string?] [] #:rest (listof string?) string?)]
          [peek (-> string? string?)]
          [set-all (->* [string?] [#:include string? #:exclude string?] string?)]
          [toggle-all (->* [] [#:include string? #:exclude string?] string?)]
          [get
           (->* [string?]
                [#:content-type (or/c 'json 'form)
                 #:filter-signals-include string?
                 #:filter-signals-exclude string?
                 #:selector string?
                 #:headers hash?
                 #:open-when-hidden? boolean?
                 #:payload string?
                 #:retry (or/c 'auto 'error 'always 'never)
                 #:retry-interval exact-nonnegative-integer?
                 #:retry-scaler number?
                 #:retry-max-wait-ms exact-nonnegative-integer?
                 #:retry-max-count exact-nonnegative-integer?
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string?)]
                string?)]
          [post
           (->* [string?]
                [#:content-type (or/c 'json 'form)
                 #:filter-signals-include string?
                 #:filter-signals-exclude string?
                 #:selector string?
                 #:headers hash?
                 #:open-when-hidden? boolean?
                 #:payload string?
                 #:retry (or/c 'auto 'error 'always 'never)
                 #:retry-interval exact-nonnegative-integer?
                 #:retry-scaler number?
                 #:retry-max-wait-ms exact-nonnegative-integer?
                 #:retry-max-count exact-nonnegative-integer?
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string?)]
                string?)]
          [put
           (->* [string?]
                [#:content-type (or/c 'json 'form)
                 #:filter-signals-include string?
                 #:filter-signals-exclude string?
                 #:selector string?
                 #:headers hash?
                 #:open-when-hidden? boolean?
                 #:payload string?
                 #:retry (or/c 'auto 'error 'always 'never)
                 #:retry-interval exact-nonnegative-integer?
                 #:retry-scaler number?
                 #:retry-max-wait-ms exact-nonnegative-integer?
                 #:retry-max-count exact-nonnegative-integer?
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string?)]
                string?)]
          [patch
           (->* [string?]
                [#:content-type (or/c 'json 'form)
                 #:filter-signals-include string?
                 #:filter-signals-exclude string?
                 #:selector string?
                 #:headers hash?
                 #:open-when-hidden? boolean?
                 #:payload string?
                 #:retry (or/c 'auto 'error 'always 'never)
                 #:retry-interval exact-nonnegative-integer?
                 #:retry-scaler number?
                 #:retry-max-wait-ms exact-nonnegative-integer?
                 #:retry-max-count exact-nonnegative-integer?
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string?)]
                string?)]
          [delete
           (->* [string?]
                [#:content-type (or/c 'json 'form)
                 #:filter-signals-include string?
                 #:filter-signals-exclude string?
                 #:selector string?
                 #:headers hash?
                 #:open-when-hidden? boolean?
                 #:payload string?
                 #:retry (or/c 'auto 'error 'always 'never)
                 #:retry-interval exact-nonnegative-integer?
                 #:retry-scaler number?
                 #:retry-max-wait-ms exact-nonnegative-integer?
                 #:retry-max-count exact-nonnegative-integer?
                 #:request-cancellation (or/c 'auto 'cleanup 'disabled string?)]
                string?)]
          [clipboard (->* [string?] [#:base64? boolean?] string?)]
          [fit
           (->* [string? number? number? number? number?]
                [#:clamp? boolean? #:round? boolean?]
                string?)]
          [intl
           (->* [string? string?] [#:options hash? #:locale string?] string?)]))

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

(define (provided? value)
  (not (unsupplied-arg? value)))

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
  (define fs-include? (provided? fs-include))
  (define fs-exclude? (provided? fs-exclude))
  (define parts
    (filter values
            (list (and (provided? content-type)
                       (string-append "contentType: " (js-value content-type)))
                  (and (or fs-include? fs-exclude?)
                       (string-append "filterSignals: "
                                      (build-filter-signals (and fs-include? fs-include)
                                                            (and fs-exclude? fs-exclude))))
                  (and (provided? selector)
                       (string-append "selector: " (jsexpr->string selector)))
                  (and (provided? headers)
                       (string-append "headers: " (js-object-from-hash headers)))
                  (and (provided? open-when-hidden?)
                       (string-append "openWhenHidden: " (js-value open-when-hidden?)))
                  (and (provided? payload)
                       (string-append "payload: " payload))
                  (and (provided? retry)
                       (string-append "retry: " (js-value retry)))
                  (and (provided? retry-interval)
                       (string-append "retryInterval: " (js-value retry-interval)))
                  (and (provided? retry-scaler)
                       (string-append "retryScaler: " (js-value retry-scaler)))
                  (and (provided? retry-max-wait-ms)
                       (string-append "retryMaxWaitMs: " (js-value retry-max-wait-ms)))
                  (and (provided? retry-max-count)
                       (string-append "retryMaxCount: " (js-value retry-max-count)))
                  (and (provided? request-cancellation)
                       (string-append "requestCancellation: " (js-value request-cancellation))))))
  (if (null? parts)
      #f
      (string-join parts ", " #:before-first "{" #:after-last "}")))

;; backend actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-action-method name method-str)
  (define (name url
                #:content-type [content-type the-unsupplied-arg]
                #:filter-signals-include [fs-include the-unsupplied-arg]
                #:filter-signals-exclude [fs-exclude the-unsupplied-arg]
                #:selector [selector the-unsupplied-arg]
                #:headers [headers the-unsupplied-arg]
                #:open-when-hidden? [open-when-hidden? the-unsupplied-arg]
                #:payload [payload the-unsupplied-arg]
                #:retry [retry the-unsupplied-arg]
                #:retry-interval [retry-interval the-unsupplied-arg]
                #:retry-scaler [retry-scaler the-unsupplied-arg]
                #:retry-max-wait-ms [retry-max-wait-ms the-unsupplied-arg]
                #:retry-max-count [retry-max-count the-unsupplied-arg]
                #:request-cancellation [request-cancellation the-unsupplied-arg])
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

(define (clipboard text #:base64? [base64? the-unsupplied-arg])
  (if (unsupplied-arg? base64?)
      (format "@clipboard(~a)" text)
      (format "@clipboard(~a, ~a)" text (js-value base64?))))

(define (fit v old-min old-max new-min new-max
             #:clamp? [clamp? the-unsupplied-arg]
             #:round? [round? the-unsupplied-arg])
  (define base (format "@fit(~a, ~a, ~a, ~a, ~a" v old-min old-max new-min new-max))
  (cond
    [(and (unsupplied-arg? clamp?) (unsupplied-arg? round?)) (string-append base ")")]
    [(unsupplied-arg? round?) (format "~a, ~a)" base (js-value clamp?))]
    [(unsupplied-arg? clamp?) (format "~a, false, ~a)" base (js-value round?))]
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
