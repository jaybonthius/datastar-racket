#lang racket/base

(require json
         "constants.rkt"
         racket/sequence
         racket/match
         racket/string
         racket/contract
         web-server/http)

(provide (contract-out
          [datastar-response (-> (or/c string? (sequence/c string?)) response?)]
          [patch-elements
           (->* ((or/c string? #f))
                (#:selector (or/c string? #f)
                            #:mode (or/c string? #f)
                            #:use-view-transitions (or/c boolean? #f)
                            #:event-id (or/c string? #f)
                            #:retry-duration (or/c exact-positive-integer? #f))
                string?)]
          [remove-elements
           (->* (string?)
                (#:event-id (or/c string? #f) #:retry-duration (or/c exact-positive-integer? #f))
                string?)]
          [read-signals (-> request? jsexpr?)]
          [patch-signals
           (->* ((or/c string? jsexpr?))
                (#:event-id (or/c string? #f)
                            #:only-if-missing (or/c boolean? #f)
                            #:retry-duration (or/c exact-positive-integer? #f))
                string?)]
          [execute-script
           (->* (string?)
                (#:auto-remove boolean?
                               #:attributes (or/c (hash/c symbol? any/c) (listof string?) #f)
                               #:event-id (or/c string? #f)
                               #:retry-duration (or/c exact-positive-integer? #f))
                string?)]
          [redirect (-> string? string?)]))

; ==========================================================
; UTILS
; ==========================================================

(define (_js-bool b)
  (if b "true" "false"))

(define (escape str)
  (regexp-replace* #px"[&'\"<>]"
                   str
                   (λ (m)
                     (case m
                       [("&") "&amp;"]
                       [("'") "&#39;"]
                       [("\"") "&#34;"]
                       [(">") "&gt;"]
                       [("<") "&lt;"]))))

(define (_send-event event-type
                     data-lines
                     #:event-id [event-id #f]
                     #:retry-duration [retry-duration #f])
  (define prefix-lines
    (filter values
            (list (string-append "event: " event-type)
                  (and event-id (string-append "id: " event-id))
                  (and retry-duration
                       (not (= retry-duration DEFAULT-SSE-RETRY-DURATION))
                       (string-append "retry: " (number->string retry-duration))))))

  (define formatted-data-lines (map (λ (line) (string-append "data: " line)) data-lines))

  (string-join (append prefix-lines formatted-data-lines) "\n" #:after-last "\n\n"))

(define (datastar-response events-generator)
  (response 200
            #"OK"
            (current-seconds)
            #"text/event-stream"
            (for/list ([(k v) (in-hash SSE-HEADERS)])
              (make-header (string->bytes/utf-8 k) (string->bytes/utf-8 v)))
            (λ (out)
              (parameterize ([current-output-port out])
                (cond
                  [(string? events-generator)
                   (write-string events-generator out)
                   (flush-output out)]
                  [(sequence? events-generator)
                   (for ([event events-generator])
                     (write-string event out)
                     (flush-output out))]
                  [else
                   (write-string events-generator out)
                   (flush-output out)])))))

; ==========================================================
; ELEMENTS
; ==========================================================

(define (patch-elements elements
                        #:selector [selector #f]
                        #:mode [mode #f]
                        #:use-view-transitions [use-view-transitions #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (define data-lines
    (append (filter values
                    (list (and mode
                               (not (string=? mode ELEMENT-PATCH-MODE-OUTER))
                               (string-append MODE-DATALINE-LITERAL " " mode))
                          (and selector (string-append SELECTOR-DATALINE-LITERAL " " selector))
                          (and use-view-transitions
                               (not (eq? use-view-transitions DEFAULT-ELEMENTS-USE-VIEW-TRANSITIONS))
                               (string-append USE-VIEW-TRANSITION-DATALINE-LITERAL
                                              " "
                                              (_js-bool use-view-transitions)))))
            (if elements
                (map (λ (line) (string-append ELEMENTS-DATALINE-LITERAL " " line))
                     (string-split elements "\n"))
                '())))

  (_send-event EVENT-TYPE-PATCH-ELEMENTS
               data-lines
               #:event-id event-id
               #:retry-duration retry-duration))

(define (remove-elements selector #:event-id [event-id #f] #:retry-duration [retry-duration #f])
  (patch-elements #f
                  #:selector selector
                  #:mode ELEMENT-PATCH-MODE-REMOVE
                  #:event-id event-id
                  #:retry-duration retry-duration))

(define (execute-script script
                        #:auto-remove [auto-remove #t]
                        #:attributes [attributes #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (define attribute-string
    (string-join (filter values
                         (list (and auto-remove "data-effect=\"el.remove()\"")
                               (match attributes
                                 [(? hash?)
                                  (string-join (for/list ([(k v) (in-hash attributes)])
                                                 (format "~a=\"~a\"" k (escape (format "~a" v))))
                                               " ")]
                                 [(? list?) (string-join attributes " ")]
                                 [#f #f])))
                 " "))

  (define script-tag
    (format "<script~a>~a</script>"
            (if (string=? attribute-string "")
                ""
                (string-append " " attribute-string))
            script))

  (patch-elements script-tag
                  #:mode ELEMENT-PATCH-MODE-APPEND
                  #:selector "body"
                  #:event-id event-id
                  #:retry-duration retry-duration))

(define (redirect location)
  (execute-script (format "setTimeout(() => window.location = '~a')" location)))

; ==========================================================
; SIGNALS
; ==========================================================

(define (read-signals request)
  (match (request-method request)
    [#"GET"
     (define datastar-binding
       (findf (λ (binding) (equal? (bytes->string/utf-8 (binding-id binding)) DATASTAR-KEY))
              (request-bindings/raw request)))
     (unless datastar-binding
       (error "No datastar parameter found in request bindings"))
     (string->jsexpr (bytes->string/utf-8 (binding:form-value datastar-binding)))]
    [_
     (define body (request-post-data/raw request))
     (unless body
       (error "No request body found"))
     (string->jsexpr (bytes->string/utf-8 body))]))

(define (patch-signals signals
                       #:event-id [event-id #f]
                       #:only-if-missing [only-if-missing #f]
                       #:retry-duration [retry-duration #f])
  (define signals-str
    (if (string? signals)
        signals
        (jsexpr->string signals)))

  (define data-lines
    (append (filter values
                    (list (and only-if-missing
                               (not (eq? only-if-missing DEFAULT-PATCH-SIGNALS-ONLY-IF-MISSING))
                               (string-append ONLY-IF-MISSING-DATALINE-LITERAL
                                              " "
                                              (_js-bool only-if-missing)))))
            (map (λ (line) (string-append SIGNALS-DATALINE-LITERAL " " line))
                 (string-split signals-str "\n"))))

  (_send-event EVENT-TYPE-PATCH-SIGNALS
               data-lines
               #:event-id event-id
               #:retry-duration retry-duration))
