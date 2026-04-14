#lang racket/base

(require json
         net/tcp-sig
         racket/contract/base
         racket/match
         racket/string
         (prefix-in racket: racket/tcp)
         racket/unit
         web-server/http
         xml
         "../../private/constants.rkt"
         "../../private/sse.rkt"
         "../../private/utils.rkt"
         "../../private/xexpr.rkt")

(define element-patch-mode/c (or/c 'outer 'inner 'remove 'replace 'prepend 'append 'before 'after #f))
(define element-namespace/c (or/c 'html 'svg 'mathml #f))

(provide element-patch-mode/c
         element-namespace/c
         (contract-out
          [datastar-sse (->* [(-> sse? any)] [#:on-close (or/c (-> sse? any) #f)] response?)]
          [close-sse (-> sse? void?)]
          [sse-closed? (-> sse? boolean?)]
          [sse? (-> any/c boolean?)]
          [call-with-sse-lock (-> sse? (-> any) any)]
          [patch-elements
           (->* [sse? (or/c string? #f)]
                [#:selector (or/c string? #f)
                 #:mode element-patch-mode/c
                 #:namespace element-namespace/c
                 #:use-view-transitions? (or/c boolean? #f)
                 #:event-id (or/c string? #f)
                 #:retry-duration (or/c exact-positive-integer? #f)]
                void?)]
          [patch-elements/xexprs
           (->* [sse? (or/c xexpr/c (listof xexpr/c))]
                [#:selector (or/c string? #f)
                 #:mode element-patch-mode/c
                 #:namespace element-namespace/c
                 #:use-view-transitions? (or/c boolean? #f)
                 #:event-id (or/c string? #f)
                 #:retry-duration (or/c exact-positive-integer? #f)]
                void?)]
          [remove-elements
           (->* [sse? string?]
                [#:event-id (or/c string? #f) #:retry-duration (or/c exact-positive-integer? #f)]
                void?)]
          [patch-signals
           (->* [sse? (or/c string? jsexpr?)]
                [#:event-id (or/c string? #f)
                 #:only-if-missing? (or/c boolean? #f)
                 #:retry-duration (or/c exact-positive-integer? #f)]
                void?)]
          [remove-signals
           (->* [sse? (or/c string? (listof string?))]
                [#:event-id (or/c string? #f) #:retry-duration (or/c exact-positive-integer? #f)]
                void?)]
          [execute-script
           (->* [sse? string?]
                [#:auto-remove? boolean?
                 #:attributes (or/c (hash/c symbol? any/c) (listof string?) #f)
                 #:event-id (or/c string? #f)
                 #:retry-duration (or/c exact-positive-integer? #f)]
                void?)]
          [redirect (-> sse? string? void?)]
          [replace-url (-> sse? string? void?)]
          [console-log (-> sse? string? void?)]
          [console-error (-> sse? string? void?)])
         with-sse-lock
         datastar-tcp@)

;; internal helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (->header key value)
  (make-header (string->bytes/utf-8 key) (string->bytes/utf-8 value)))

(define (build-patch-elements elements
                              #:selector [selector #f]
                              #:mode [mode #f]
                              #:namespace [namespace #f]
                              #:use-view-transitions? [use-view-transitions? #f]
                              #:event-id [event-id #f]
                              #:retry-duration [retry-duration #f])
  (define data-lines
    (append
     (filter
      values
      (list (and mode
                 (not (eq? mode default-element-patch-mode))
                 (string-append (symbol->string mode-dataline-literal) " " (symbol->string mode)))
            (and selector (string-append (symbol->string selector-dataline-literal) " " selector))
            (and namespace
                 (not (eq? namespace default-element-namespace))
                 (string-append (symbol->string namespace-dataline-literal)
                                " "
                                (symbol->string namespace)))
            (and use-view-transitions?
                 (not (eq? use-view-transitions? default-elements-use-view-transitions))
                 (string-append (symbol->string use-view-transition-dataline-literal)
                                " "
                                (js-bool use-view-transitions?)))))
     (if elements
         (map (lambda (line) (string-append (symbol->string elements-dataline-literal) " " line))
              (string-split elements "\n"))
         '())))

  (send-event event-type-patch-elements
              data-lines
              #:event-id event-id
              #:retry-duration retry-duration))

(define (set-path-to-null signals path)
  (define path-segments (string-split path "." #:trim? #f))
  (define (set-path segments target)
    (define segment (car segments))
    (define segment-key (string->symbol segment))
    (define rest-segments (cdr segments))
    (cond
      [(null? rest-segments) (hash-set target segment-key 'null)]
      [else
       (define existing (hash-ref target segment-key #f))
       (cond
         [(eq? existing 'null) target]
         [else
          (define existing-hash
            (if (hash? existing)
                existing
                (hash)))
          (hash-set target segment-key (set-path rest-segments existing-hash))])]))
  (set-path path-segments signals))

(define (build-remove-signals-jsexpr signal-paths)
  (for/fold ([signals (hash)]) ([path (in-list signal-paths)])
    (set-path-to-null signals path)))

(define (build-patch-signals signals
                             #:event-id [event-id #f]
                             #:only-if-missing? [only-if-missing? #f]
                             #:retry-duration [retry-duration #f])
  (define signals-str
    (if (string? signals)
        signals
        (jsexpr->string signals)))

  (define data-lines
    (append (filter values
                    (list (and only-if-missing?
                               (not (eq? only-if-missing? default-patch-signals-only-if-missing))
                               (string-append (symbol->string only-if-missing-dataline-literal)
                                              " "
                                              (js-bool only-if-missing?)))))
            (map (lambda (line) (string-append (symbol->string signals-dataline-literal) " " line))
                 (string-split signals-str "\n"))))

  (send-event event-type-patch-signals
              data-lines
              #:event-id event-id
              #:retry-duration retry-duration))

(define (escape-js-string str)
  (regexp-replace* #px"[\\\\'\"\n\r\t\v\f\0]"
                   str
                   (lambda (m)
                     (case m
                       [("\\") "\\\\"]
                       [("'") "\\'"]
                       [("\"") "\\\""]
                       [("\n") "\\n"]
                       [("\r") "\\r"]
                       [("\t") "\\t"]
                       [("\v") "\\v"]
                       [("\f") "\\f"]
                       [("\0") "\\0"]))))

(define (build-execute-script script
                              #:auto-remove? [auto-remove? #t]
                              #:attributes [attributes #f]
                              #:event-id [event-id #f]
                              #:retry-duration [retry-duration #f])
  (define attribute-string
    (string-join (filter values
                         (list (and auto-remove? "data-effect=\"el.remove()\"")
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

  (build-patch-elements script-tag
                        #:mode 'append
                        #:selector "body"
                        #:event-id event-id
                        #:retry-duration retry-duration))

;; sse lifecycle API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (set-box! (sse-closed-box gen) #t))

(define (datastar-sse on-open #:on-close [on-close #f])
  (define extra-headers
    (for/list ([(k v) (in-hash sse-headers)])
      (->header k v)))
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

;; sse events API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-elements gen
                        elements
                        #:selector [selector #f]
                        #:mode [mode #f]
                        #:namespace [namespace #f]
                        #:use-view-transitions? [use-view-transitions? #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-patch-elements elements
                                  #:selector selector
                                  #:mode mode
                                  #:namespace namespace
                                  #:use-view-transitions? use-view-transitions?
                                  #:event-id event-id
                                  #:retry-duration retry-duration)))

(define (remove-elements gen selector #:event-id [event-id #f] #:retry-duration [retry-duration #f])
  (patch-elements gen
                  #f
                  #:selector selector
                  #:mode 'remove
                  #:event-id event-id
                  #:retry-duration retry-duration))

(define (patch-elements/xexprs gen
                               xexpr-or-xexprs
                               #:selector [selector #f]
                               #:mode [mode #f]
                               #:namespace [namespace #f]
                               #:use-view-transitions? [use-view-transitions? #f]
                               #:event-id [event-id #f]
                               #:retry-duration [retry-duration #f])
  (patch-elements gen
                  (xexpr-or-xexprs->html xexpr-or-xexprs)
                  #:selector selector
                  #:mode mode
                  #:namespace namespace
                  #:use-view-transitions? use-view-transitions?
                  #:event-id event-id
                  #:retry-duration retry-duration))

(define (patch-signals gen
                       signals
                       #:event-id [event-id #f]
                       #:only-if-missing? [only-if-missing? #f]
                       #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-patch-signals signals
                                 #:event-id event-id
                                 #:only-if-missing? only-if-missing?
                                 #:retry-duration retry-duration)))

(define (remove-signals gen
                        signal-path-or-paths
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (define signal-paths
    (if (string? signal-path-or-paths)
        (list signal-path-or-paths)
        signal-path-or-paths))
  (patch-signals gen
                 (build-remove-signals-jsexpr signal-paths)
                 #:event-id event-id
                 #:retry-duration retry-duration))

(define (execute-script gen
                        script
                        #:auto-remove? [auto-remove? #t]
                        #:attributes [attributes #f]
                        #:event-id [event-id #f]
                        #:retry-duration [retry-duration #f])
  (sse-send gen
            (build-execute-script script
                                  #:auto-remove? auto-remove?
                                  #:attributes attributes
                                  #:event-id event-id
                                  #:retry-duration retry-duration)))

(define (redirect gen location)
  (execute-script gen
                  (format "setTimeout(() => window.location = '~a')" (escape-js-string location))))

(define (replace-url gen location)
  (execute-script gen
                  (format "window.history.replaceState({}, '', '~a')" (escape-js-string location))))

(define (console-log gen message)
  (execute-script gen (format "console.log('~a')" (escape-js-string message))))

(define (console-error gen message)
  (execute-script gen (format "console.error('~a')" (escape-js-string message))))
