#lang racket/base

(require json
         racket/contract/base
         racket/match
         racket/string
         web-server/http
         "constants.rkt")

(provide (contract-out [datastar-sse
                        (->* [request? (-> sse? any)]
                             [#:on-close (or/c (-> sse? any) #f) #:write-profile write-profile?]
                             response?)]
                       [patch-elements
                        (->* [sse? (or/c string? #f)]
                             [#:selector (or/c string? #f)
                              #:mode (or/c string? #f)
                              #:namespace (or/c string? #f)
                              #:use-view-transitions (or/c boolean? #f)
                              #:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             boolean?)]
                       [remove-elements
                        (->* [sse? string?]
                             [#:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             boolean?)]
                       [patch-signals
                        (->* [sse? (or/c string? jsexpr?)]
                             [#:event-id (or/c string? #f)
                              #:only-if-missing (or/c boolean? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             boolean?)]
                       [execute-script
                        (->* [sse? string?]
                             [#:auto-remove boolean?
                              #:attributes (or/c (hash/c symbol? any/c) (listof string?) #f)
                              #:event-id (or/c string? #f)
                              #:retry-duration (or/c exact-positive-integer? #f)]
                             boolean?)]
                       [redirect (-> sse? string? boolean?)]
                       [console-log (-> sse? string? boolean?)]
                       [close-sse (-> sse? void?)]
                       [read-signals (-> request? jsexpr?)]
                       [sse? (-> any/c boolean?)]
                       [write-profile? (-> any/c boolean?)]
                       [basic-write-profile write-profile?]
                       [make-write-profile
                        (-> (-> output-port? output-port?)
                            (-> output-port? output-port? any)
                            (or/c string? #f)
                            write-profile?)]))

; ==========================================================
; WRITE PROFILES
; ==========================================================

;; A write profile controls how SSE event data is written to the output port.
;; This abstraction allows optional compression (e.g., brotli) to be layered
;; onto the SSE output stream without changing the core event-building logic.
;;
;; Fields:
;;   wrap-output    : (-> output-port? output-port?)
;;                    Wraps the raw output port (e.g., with a compression port).
;;                    For no compression, this is `values` (identity).
;;   flush!         : (-> output-port? output-port? void?)
;;                    Flush strategy taking (wrapped-port raw-port).
;;                    For compression, this must "double flush" — first the
;;                    compression port (to emit compressed bytes), then the
;;                    underlying raw port (to push bytes over the network).
;;   content-encoding : (or/c string? #f)
;;                      Value for the Content-Encoding response header.
;;                      #f means no encoding header is added.
(struct write-profile (wrap-output flush! content-encoding) #:constructor-name make-write-profile)

;; The default write profile: no compression, simple flush, no Content-Encoding.
(define basic-write-profile (make-write-profile values (lambda (_wrapped raw) (flush-output raw)) #f))

; ==========================================================
; PUBLIC API
; ==========================================================

(struct sse (out raw-out flush! semaphore closed-box) #:constructor-name make-sse)

(define (close-sse gen)
  (unless (unbox (sse-closed-box gen))
    (set-box! (sse-closed-box gen) #t)
    (with-handlers ([exn:fail? void])
      ;; If a compression port wraps the raw port, close it to finalize
      ;; the compression stream. The brotli port is created with #:close? #f
      ;; so this does NOT close the underlying raw port.
      (unless (eq? (sse-out gen) (sse-raw-out gen))
        (close-output-port (sse-out gen)))
      (flush-output (sse-raw-out gen)))))

(define (datastar-sse request
                      on-open
                      #:on-close [on-close #f]
                      #:write-profile [wp basic-write-profile])
  (define encoding (write-profile-content-encoding wp))
  (define extra-headers
    (for/list ([(k v) (in-hash SSE-HEADERS)])
      (make-header (string->bytes/utf-8 k) (string->bytes/utf-8 v))))
  (define all-headers
    (if encoding
        (cons (make-header #"Content-Encoding" (string->bytes/utf-8 encoding)) extra-headers)
        extra-headers))
  (response 200
            #"OK"
            (current-seconds)
            #"text/event-stream"
            all-headers
            (lambda (out)
              (define wrapped-out ((write-profile-wrap-output wp) out))
              (define gen
                (make-sse wrapped-out out (write-profile-flush! wp) (make-semaphore 1) (box #f)))
              (dynamic-wind void
                            (lambda () (on-open gen))
                            (lambda ()
                              (close-sse gen)
                              (when on-close
                                (on-close gen)))))))

(define (read-signals request)
  (match (request-method request)
    [#"GET"
     (define datastar-binding
       (findf (lambda (binding) (equal? (bytes->string/utf-8 (binding-id binding)) DATASTAR-KEY))
              (request-bindings/raw request)))
     (unless datastar-binding
       (error "No datastar parameter found in request bindings"))
     (string->jsexpr (bytes->string/utf-8 (binding:form-value datastar-binding)))]
    [_
     (define body (request-post-data/raw request))
     (unless body
       (error "No request body found"))
     (string->jsexpr (bytes->string/utf-8 body))]))

(define (patch-elements gen
                        elements
                        #:selector [selector #f]
                        #:mode [mode #f]
                        #:namespace [namespace #f]
                        #:use-view-transitions [use-view-transitions #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (_sse-send gen
             (_build-patch-elements elements
                                    #:selector selector
                                    #:mode mode
                                    #:namespace namespace
                                    #:use-view-transitions use-view-transitions
                                    #:event-id event-id
                                    #:retry-duration retry-duration)))

(define (remove-elements gen selector #:event-id [event-id #f] #:retry-duration [retry-duration #f])
  (patch-elements gen
                  #f
                  #:selector selector
                  #:mode ELEMENT-PATCH-MODE-REMOVE
                  #:event-id event-id
                  #:retry-duration retry-duration))

(define (patch-signals gen
                       signals
                       #:event-id [event-id #f]
                       #:only-if-missing [only-if-missing #f]
                       #:retry-duration [retry-duration #f])
  (_sse-send gen
             (_build-patch-signals signals
                                   #:event-id event-id
                                   #:only-if-missing only-if-missing
                                   #:retry-duration retry-duration)))

(define (execute-script gen
                        script
                        #:auto-remove [auto-remove #t]
                        #:attributes [attributes #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (_sse-send gen
             (_build-execute-script script
                                    #:auto-remove auto-remove
                                    #:attributes attributes
                                    #:event-id event-id
                                    #:retry-duration retry-duration)))

(define (redirect gen location)
  (execute-script gen (format "setTimeout(() => window.location = '~a')" location)))

(define (console-log gen message)
  (execute-script gen (format "console.log('~a')" message)))

; ==========================================================
; INTERNAL
; ==========================================================

(define (_js-bool b)
  (if b "true" "false"))

(define (_escape str)
  (regexp-replace* #px"[&'\"<>]"
                   str
                   (lambda (m)
                     (case m
                       [("&") "&amp;"]
                       [("'") "&#39;"]
                       [("\"") "&#34;"]
                       [(">") "&gt;"]
                       [("<") "&lt;"]))))

(define (_sse-closed? gen)
  (or (unbox (sse-closed-box gen)) (port-closed? (sse-out gen))))

(define (_sse-send gen event-str)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (call-with-semaphore (sse-semaphore gen)
                         (lambda ()
                           (cond
                             [(_sse-closed? gen) #f]
                             [else
                              (write-string event-str (sse-out gen))
                              ((sse-flush! gen) (sse-out gen) (sse-raw-out gen))
                              #t])))))

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

  (define formatted-data-lines (map (lambda (line) (string-append "data: " line)) data-lines))

  (string-join (append prefix-lines formatted-data-lines) "\n" #:after-last "\n\n"))

(define (_build-patch-elements elements
                               #:selector [selector #f]
                               #:mode [mode #f]
                               #:namespace [namespace #f]
                               #:use-view-transitions [use-view-transitions #f]
                               #:event-id [event-id #f]
                               #:retry-duration [retry-duration #f])
  (define data-lines
    (append (filter values
                    (list (and mode
                               (not (string=? mode ELEMENT-PATCH-MODE-OUTER))
                               (string-append MODE-DATALINE-LITERAL " " mode))
                          (and selector (string-append SELECTOR-DATALINE-LITERAL " " selector))
                          (and namespace (string-append NAMESPACE-DATALINE-LITERAL " " namespace))
                          (and use-view-transitions
                               (not (eq? use-view-transitions DEFAULT-ELEMENTS-USE-VIEW-TRANSITIONS))
                               (string-append USE-VIEW-TRANSITION-DATALINE-LITERAL
                                              " "
                                              (_js-bool use-view-transitions)))))
            (if elements
                (map (lambda (line) (string-append ELEMENTS-DATALINE-LITERAL " " line))
                     (string-split elements "\n"))
                '())))

  (_send-event EVENT-TYPE-PATCH-ELEMENTS
               data-lines
               #:event-id event-id
               #:retry-duration retry-duration))

(define (_build-patch-signals signals
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
            (map (lambda (line) (string-append SIGNALS-DATALINE-LITERAL " " line))
                 (string-split signals-str "\n"))))

  (_send-event EVENT-TYPE-PATCH-SIGNALS
               data-lines
               #:event-id event-id
               #:retry-duration retry-duration))

(define (_build-execute-script script
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
                                                 (format "~a=\"~a\"" k (_escape (format "~a" v))))
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

  (_build-patch-elements script-tag
                         #:mode ELEMENT-PATCH-MODE-APPEND
                         #:selector "body"
                         #:event-id event-id
                         #:retry-duration retry-duration))

; ==========================================================
; TEST SUPPORT
; ==========================================================

(module+ test-support
  (provide make-sse
           make-write-profile
           basic-write-profile
           write-profile?
           write-profile-wrap-output
           write-profile-flush!
           write-profile-content-encoding))
