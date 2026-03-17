#lang racket/base

;; Datastar HTML Attribute Generators
;;
;; Produces x-expression attribute pairs: (list 'data-on:click "$count++")
;; that drop directly into x-expression templates:
;;
;;   `(button (,(ds:on "click" "$count++") (class "btn")) "Click")

(require json
         racket/string)

;; Simple value attributes
(provide ds:show
         ds:text
         ds:effect
         ds:bind
         ds:ref
         ds:indicator
         ds:ignore-morph
         ds:view-transition
         ds:custom-validity
         ds:replace-url
         ds:match-media

         ;; Hash-based attributes
         ds:signals
         ds:computed
         ds:attr
         ds:class
         ds:style

         ;; Event handlers with modifiers
         ds:on
         ds:init
         ds:on-intersect
         ds:on-interval
         ds:on-signal-patch
         ds:on-raf
         ds:on-resize

         ;; Property-like / no-value attributes
         ds:ignore
         ds:scroll-into-view
         ds:persist
         ds:query-string
         ds:json-signals
         ds:preserve-attrs)

;; ============================================================================
;; Internal helpers
;; ============================================================================

;; Build an x-expression attribute pair: (list 'name "value").
;; The attribute name is constructed as a string (for string-append with
;; modifiers) and converted to a symbol here — the single conversion point.
;; X-expressions require attribute names to be symbols.
(define (xattr name value)
  (list (string->symbol name) value))

;; Build the modifier suffix string from a list of (name . values) pairs.
;; Each pair becomes __name or __name.val1.val2
;; Only pairs where the value is non-#f are included.
;;
;; Example: (build-modifier-string '(("debounce" "300ms") ("prevent") ("stop")))
;;          => "__debounce.300ms__prevent__stop"
(define (build-modifier-string mods)
  (define active
    (for/list ([m (in-list mods)]
               #:when m)
      (define name (car m))
      (define vals (cdr m))
      (if (null? vals)
          (string-append "__" name)
          (string-append "__" name "." (string-join vals ".")))))
  (string-join active ""))

;; Collect timing modifiers (debounce, throttle) from keyword args into modifier list entries.
;; Returns a list of modifier entries (possibly empty).
(define (timing-mods #:debounce [debounce #f]
                     #:debounce-leading [debounce-leading #f]
                     #:debounce-notrailing [debounce-notrailing #f]
                     #:throttle [throttle #f]
                     #:throttle-noleading [throttle-noleading #f]
                     #:throttle-trailing [throttle-trailing #f])
  (append (if debounce
              (list (cons "debounce"
                          (append (list (if (number? debounce)
                                            (number->string debounce)
                                            debounce))
                                  (if debounce-leading
                                      '("leading")
                                      '())
                                  (if debounce-notrailing
                                      '("notrailing")
                                      '()))))
              '())
          (if throttle
              (list (cons "throttle"
                          (append (list (if (number? throttle)
                                            (number->string throttle)
                                            throttle))
                                  (if throttle-noleading
                                      '("noleading")
                                      '())
                                  (if throttle-trailing
                                      '("trailing")
                                      '()))))
              '())))

;; Delay modifier entry
(define (delay-mod #:delay (delay
                             #f))
  (if delay
      (list (list "delay"
                  (if (number? delay)
                      (number->string delay)
                      delay)))
      '()))

;; Viewtransition modifier entry
(define (viewtransition-mod #:viewtransition [vt #f])
  (if vt
      (list (list "viewtransition"))
      '()))

;; Convert a Racket hash to a JSON string (for literal signal values).
(define (hash->json h)
  (jsexpr->string h))

;; Convert a Racket hash to a JS object notation string where values are expressions
;; (not quoted). E.g., (hash "aria-label" "$foo") => "{\"aria-label\": $foo}"
(define (hash->js-object h)
  (string-join (for/list ([(k v) (in-hash h)])
                 (define key-str
                   (if (symbol? k)
                       (symbol->string k)
                       k))
                 (if (hash? v)
                     (string-append (jsexpr->string key-str) ": " (hash->js-object v))
                     (string-append (jsexpr->string key-str)
                                    ": "
                                    (if (string? v)
                                        v
                                        (jsexpr->string v)))))
               ", "
               #:before-first "{"
               #:after-last "}"))

;; Build a filter object for include/exclude parameters
(define (build-filter-value include exclude)
  (define parts
    (append (if include
                (list (string-append "\"include\": " (jsexpr->string include)))
                '())
            (if exclude
                (list (string-append "\"exclude\": " (jsexpr->string exclude)))
                '())))
  (string-join parts ", " #:before-first "{" #:after-last "}"))

;; ============================================================================
;; Simple value attributes
;; ============================================================================

;; (ds:show expr) => '(data-show "expr")
(define (ds:show expression)
  (xattr "data-show" expression))

;; (ds:text expr) => '(data-text "expr")
(define (ds:text expression)
  (xattr "data-text" expression))

;; (ds:effect expr) => '(data-effect "expr")
(define (ds:effect expression)
  (xattr "data-effect" expression))

;; (ds:bind signal) => '(data-bind:signal "")
(define (ds:bind signal [value ""])
  (xattr (string-append "data-bind:" signal) value))

;; (ds:ref signal) => '(data-ref:signal "")
(define (ds:ref signal)
  (xattr (string-append "data-ref:" signal) ""))

;; (ds:indicator signal) => '(data-indicator:signal "")
(define (ds:indicator signal)
  (xattr (string-append "data-indicator:" signal) ""))

;; (ds:ignore-morph) => '(data-ignore-morph "")
(define (ds:ignore-morph)
  (xattr "data-ignore-morph" ""))

;; (ds:view-transition expr) => '(data-view-transition "expr")
(define (ds:view-transition expression)
  (xattr "data-view-transition" expression))

;; (ds:custom-validity expr) => '(data-custom-validity "expr")
(define (ds:custom-validity expression)
  (xattr "data-custom-validity" expression))

;; (ds:replace-url expr) => '(data-replace-url "expr")
(define (ds:replace-url expression)
  (xattr "data-replace-url" expression))

;; (ds:match-media signal query) => '(data-match-media:signal "query")
(define (ds:match-media signal query)
  (xattr (string-append "data-match-media:" signal) query))

;; ============================================================================
;; Hash-based attributes
;; ============================================================================

;; ds:signals has two forms:
;;   Keyed:  (ds:signals 'foo "1")             => '(data-signals:foo "1")
;;   Hash:   (ds:signals (hash 'count 0))      => '(data-signals "{\"count\":0}")
;;   Hash with ifmissing: (ds:signals (hash 'count 0) #:ifmissing #t)
;;           => '(data-signals__ifmissing "{\"count\":0}")
(define (ds:signals key-or-hash [value-or-unused #f] #:ifmissing [ifmissing #f])
  (cond
    ;; Keyed form: (ds:signals 'foo "1") or (ds:signals "foo" "1")
    [(or (symbol? key-or-hash) (and (string? key-or-hash) value-or-unused))
     (define key
       (if (symbol? key-or-hash)
           (symbol->string key-or-hash)
           key-or-hash))
     (define mod-str
       (build-modifier-string (if ifmissing
                                  (list (list "ifmissing"))
                                  '())))
     (xattr (string-append "data-signals:" key mod-str) value-or-unused)]
    ;; Hash form: (ds:signals (hash 'count 0))
    [(hash? key-or-hash)
     (define mod-str
       (build-modifier-string (if ifmissing
                                  (list (list "ifmissing"))
                                  '())))
     (xattr (string-append "data-signals" mod-str) (hash->json key-or-hash))]
    [else (error 'ds:signals "expected a symbol/string key or hash, got: ~e" key-or-hash)]))

;; ds:computed has two forms:
;;   Keyed:  (ds:computed 'doubled "$count * 2")  => '(data-computed:doubled "$count * 2")
;;   Hash:   (ds:computed (hash 'a "expr1" 'b "expr2"))
;;           => '((data-computed:a "expr1") (data-computed:b "expr2"))
(define (ds:computed key-or-hash [value-or-unused #f])
  (cond
    [(or (symbol? key-or-hash) (and (string? key-or-hash) value-or-unused))
     (define key
       (if (symbol? key-or-hash)
           (symbol->string key-or-hash)
           key-or-hash))
     (xattr (string-append "data-computed:" key) value-or-unused)]
    [(hash? key-or-hash)
     (for/list ([(k v) (in-hash key-or-hash)])
       (define key
         (if (symbol? k)
             (symbol->string k)
             k))
       (xattr (string-append "data-computed:" key) v))]
    [else (error 'ds:computed "expected a symbol/string key or hash, got: ~e" key-or-hash)]))

;; (ds:attr key-or-hash [value]) — set HTML attributes to expressions.
;; Keyed form: (ds:attr 'aria-label "$foo") => '(data-attr:aria-label "$foo")
;; Hash form:  (ds:attr (hash ...))         => '(data-attr "{...}")
(define (ds:attr key-or-hash [value-or-unused #f])
  (cond
    [(or (symbol? key-or-hash) (and (string? key-or-hash) value-or-unused))
     (define key
       (if (symbol? key-or-hash)
           (symbol->string key-or-hash)
           key-or-hash))
     (xattr (string-append "data-attr:" key) value-or-unused)]
    [(hash? key-or-hash) (xattr "data-attr" (hash->js-object key-or-hash))]
    [else (error 'ds:attr "expected a symbol/string key or hash, got: ~e" key-or-hash)]))

;; (ds:class key-or-hash [value]) — add/remove classes based on expressions.
(define (ds:class key-or-hash [value-or-unused #f])
  (cond
    [(or (symbol? key-or-hash) (and (string? key-or-hash) value-or-unused))
     (define key
       (if (symbol? key-or-hash)
           (symbol->string key-or-hash)
           key-or-hash))
     (xattr (string-append "data-class:" key) value-or-unused)]
    [(hash? key-or-hash) (xattr "data-class" (hash->js-object key-or-hash))]
    [else (error 'ds:class "expected a symbol/string key or hash, got: ~e" key-or-hash)]))

;; (ds:style key-or-hash [value]) — set inline CSS styles based on expressions.
(define (ds:style key-or-hash [value-or-unused #f])
  (cond
    [(or (symbol? key-or-hash) (and (string? key-or-hash) value-or-unused))
     (define key
       (if (symbol? key-or-hash)
           (symbol->string key-or-hash)
           key-or-hash))
     (xattr (string-append "data-style:" key) value-or-unused)]
    [(hash? key-or-hash) (xattr "data-style" (hash->js-object key-or-hash))]
    [else (error 'ds:style "expected a symbol/string key or hash, got: ~e" key-or-hash)]))

;; ============================================================================
;; Event handler: ds:on
;; ============================================================================

;; (ds:on event expr [modifiers ...])
;; Full modifier support for data-on attributes.
(define (ds:on event
               expression
               #:once [once #f]
               #:passive [passive #f]
               #:capture [capture #f]
               #:window [window #f]
               #:outside [outside #f]
               #:prevent [prevent #f]
               #:stop [stop #f]
               #:trust [trust #f]
               #:debounce [debounce #f]
               #:debounce-leading [debounce-leading #f]
               #:debounce-notrailing [debounce-notrailing #f]
               #:throttle [throttle #f]
               #:throttle-noleading [throttle-noleading #f]
               #:throttle-trailing [throttle-trailing #f]
               #:delay (delay
                         #f)
               #:viewtransition [viewtransition #f])
  (define mods
    (append (if once
                (list (list "once"))
                '())
            (if passive
                (list (list "passive"))
                '())
            (if capture
                (list (list "capture"))
                '())
            (if window
                (list (list "window"))
                '())
            (if outside
                (list (list "outside"))
                '())
            (if prevent
                (list (list "prevent"))
                '())
            (if stop
                (list (list "stop"))
                '())
            (if trust
                (list (list "trust"))
                '())
            (timing-mods #:debounce debounce
                         #:debounce-leading debounce-leading
                         #:debounce-notrailing debounce-notrailing
                         #:throttle throttle
                         #:throttle-noleading throttle-noleading
                         #:throttle-trailing throttle-trailing)
            (delay-mod #:delay delay)
            (viewtransition-mod #:viewtransition viewtransition)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on:" event mod-str) expression))

;; ============================================================================
;; Other event attributes
;; ============================================================================

;; (ds:init expr [modifiers ...])
(define (ds:init expression
                 #:once [once #f]
                 #:delay (delay
                           #f)
                 #:viewtransition [viewtransition #f])
  (define mods
    (append (if once
                (list (list "once"))
                '())
            (delay-mod #:delay delay)
            (viewtransition-mod #:viewtransition viewtransition)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-init" mod-str) expression))

;; (ds:on-intersect expr [modifiers ...])
(define (ds:on-intersect expression
                         #:once [once #f]
                         #:half [half #f]
                         #:full [full #f]
                         #:exit [exit #f]
                         #:threshold [threshold #f]
                         #:debounce [debounce #f]
                         #:debounce-leading [debounce-leading #f]
                         #:debounce-notrailing [debounce-notrailing #f]
                         #:throttle [throttle #f]
                         #:throttle-noleading [throttle-noleading #f]
                         #:throttle-trailing [throttle-trailing #f]
                         #:delay (delay
                                   #f)
                         #:viewtransition [viewtransition #f])
  (define mods
    (append (if once
                (list (list "once"))
                '())
            (if half
                (list (list "half"))
                '())
            (if full
                (list (list "full"))
                '())
            (if exit
                (list (list "exit"))
                '())
            (if threshold
                (list (list "threshold"
                            (if (number? threshold)
                                (number->string threshold)
                                threshold)))
                '())
            (timing-mods #:debounce debounce
                         #:debounce-leading debounce-leading
                         #:debounce-notrailing debounce-notrailing
                         #:throttle throttle
                         #:throttle-noleading throttle-noleading
                         #:throttle-trailing throttle-trailing)
            (delay-mod #:delay delay)
            (viewtransition-mod #:viewtransition viewtransition)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-intersect" mod-str) expression))

;; (ds:on-interval expr [modifiers ...])
(define (ds:on-interval expression
                        #:duration [duration #f]
                        #:duration-leading [duration-leading #f]
                        #:viewtransition [viewtransition #f])
  (define mods
    (append (if duration
                (list (cons "duration"
                            (append (list (if (number? duration)
                                              (number->string duration)
                                              duration))
                                    (if duration-leading
                                        '("leading")
                                        '()))))
                '())
            (viewtransition-mod #:viewtransition viewtransition)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-interval" mod-str) expression))

;; (ds:on-signal-patch expr [modifiers ...])
;; When #:include or #:exclude are provided, returns a list of two attribute pairs:
;;   the main on-signal-patch and the on-signal-patch-filter
(define (ds:on-signal-patch expression
                            #:include [include #f]
                            #:exclude [exclude #f]
                            #:debounce [debounce #f]
                            #:debounce-leading [debounce-leading #f]
                            #:debounce-notrailing [debounce-notrailing #f]
                            #:throttle [throttle #f]
                            #:throttle-noleading [throttle-noleading #f]
                            #:throttle-trailing [throttle-trailing #f]
                            #:delay (delay
                                      #f))
  (define mods
    (append (timing-mods #:debounce debounce
                         #:debounce-leading debounce-leading
                         #:debounce-notrailing debounce-notrailing
                         #:throttle throttle
                         #:throttle-noleading throttle-noleading
                         #:throttle-trailing throttle-trailing)
            (delay-mod #:delay delay)))
  (define mod-str (build-modifier-string mods))
  (define main-attr (xattr (string-append "data-on-signal-patch" mod-str) expression))
  (if (or include exclude)
      (list main-attr (xattr "data-on-signal-patch-filter" (build-filter-value include exclude)))
      main-attr))

;; (ds:on-raf expr [modifiers ...])
(define (ds:on-raf expression
                   #:throttle [throttle #f]
                   #:throttle-noleading [throttle-noleading #f]
                   #:throttle-trailing [throttle-trailing #f])
  (define mods
    (timing-mods #:throttle throttle
                 #:throttle-noleading throttle-noleading
                 #:throttle-trailing throttle-trailing))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-raf" mod-str) expression))

;; (ds:on-resize expr [modifiers ...])
(define (ds:on-resize expression
                      #:debounce [debounce #f]
                      #:debounce-leading [debounce-leading #f]
                      #:debounce-notrailing [debounce-notrailing #f]
                      #:throttle [throttle #f]
                      #:throttle-noleading [throttle-noleading #f]
                      #:throttle-trailing [throttle-trailing #f])
  (define mods
    (timing-mods #:debounce debounce
                 #:debounce-leading debounce-leading
                 #:debounce-notrailing debounce-notrailing
                 #:throttle throttle
                 #:throttle-noleading throttle-noleading
                 #:throttle-trailing throttle-trailing))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-resize" mod-str) expression))

;; ============================================================================
;; Property-like / no-value attributes
;; ============================================================================

;; (ds:ignore [#:self #f]) => '(data-ignore "")
;; (ds:ignore #:self #t)   => '(data-ignore__self "")
(define (ds:ignore #:self [self #f])
  (define mod-str
    (build-modifier-string (if self
                               (list (list "self"))
                               '())))
  (xattr (string-append "data-ignore" mod-str) ""))

;; (ds:scroll-into-view [modifiers ...])
(define (ds:scroll-into-view #:smooth [smooth #f]
                             #:instant [instant #f]
                             #:auto [auto #f]
                             #:hstart [hstart #f]
                             #:hcenter [hcenter #f]
                             #:hend [hend #f]
                             #:hnearest [hnearest #f]
                             #:vstart [vstart #f]
                             #:vcenter [vcenter #f]
                             #:vend [vend #f]
                             #:vnearest [vnearest #f]
                             #:focus [focus #f])
  (define mods
    (append (if smooth
                (list (list "smooth"))
                '())
            (if instant
                (list (list "instant"))
                '())
            (if auto
                (list (list "auto"))
                '())
            (if hstart
                (list (list "hstart"))
                '())
            (if hcenter
                (list (list "hcenter"))
                '())
            (if hend
                (list (list "hend"))
                '())
            (if hnearest
                (list (list "hnearest"))
                '())
            (if vstart
                (list (list "vstart"))
                '())
            (if vcenter
                (list (list "vcenter"))
                '())
            (if vend
                (list (list "vend"))
                '())
            (if vnearest
                (list (list "vnearest"))
                '())
            (if focus
                (list (list "focus"))
                '())))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-scroll-into-view" mod-str) ""))

;; (ds:persist [modifiers ...])
(define (ds:persist #:key [key #f]
                    #:include [include #f]
                    #:exclude [exclude #f]
                    #:session [session #f])
  (define mod-str
    (build-modifier-string (if session
                               (list (list "session"))
                               '())))
  (define key-part
    (if key
        (string-append ":" key)
        ""))
  (define value
    (if (or include exclude)
        (build-filter-value include exclude)
        ""))
  (xattr (string-append "data-persist" key-part mod-str) value))

;; (ds:query-string [modifiers ...])
(define (ds:query-string #:include [include #f]
                         #:exclude [exclude #f]
                         #:filter [filter #f]
                         #:history [history #f])
  (define mods
    (append (if filter
                (list (list "filter"))
                '())
            (if history
                (list (list "history"))
                '())))
  (define mod-str (build-modifier-string mods))
  (define value
    (if (or include exclude)
        (build-filter-value include exclude)
        ""))
  (xattr (string-append "data-query-string" mod-str) value))

;; (ds:json-signals [modifiers ...])
(define (ds:json-signals #:include [include #f] #:exclude [exclude #f] #:terse [terse #f])
  (define mod-str
    (build-modifier-string (if terse
                               (list (list "terse"))
                               '())))
  (define value
    (if (or include exclude)
        (build-filter-value include exclude)
        ""))
  (xattr (string-append "data-json-signals" mod-str) value))

;; (ds:preserve-attrs attr-or-list)
;; (ds:preserve-attrs "open")          => '(data-preserve-attr "open")
;; (ds:preserve-attrs '("open" "class")) => '(data-preserve-attr "open class")
(define (ds:preserve-attrs attrs)
  (define value
    (if (list? attrs)
        (string-join attrs " ")
        attrs))
  (xattr "data-preserve-attr" value))

(module+ test
  (require rackunit)

  (test-case "ds:show basic"
    (check-equal? (ds:show "$count > 0") '(data-show "$count > 0")))

  (test-case "ds:bind basic"
    (check-equal? (ds:bind "foo") (list 'data-bind:foo "")))

  (test-case "ds:signals hash form"
    (check-equal? (ds:signals (hash 'count 0)) '(data-signals "{\"count\":0}")))

  (test-case "ds:signals keyed form"
    (check-equal? (ds:signals 'foo "1") (list 'data-signals:foo "1")))

  (test-case "ds:on basic click"
    (check-equal? (ds:on "click" "$foo = ''") (list 'data-on:click "$foo = ''")))

  (test-case "ds:on with debounce"
    (check-equal? (ds:on "input" "fn()" #:debounce "300ms")
                  (list (string->symbol "data-on:input__debounce.300ms") "fn()")))

  (test-case "ds:on with prevent"
    (check-equal? (ds:on "submit" "fn()" #:prevent #t) (list 'data-on:submit__prevent "fn()")))

  (test-case "ds:init basic"
    (check-equal? (ds:init "$count = 1") '(data-init "$count = 1")))

  (test-case "ds:ignore basic"
    (check-equal? (ds:ignore) '(data-ignore "")))

  (test-case "ds:persist basic"
    (check-equal? (ds:persist) '(data-persist "")))

  (test-case "ds:signals error on invalid input"
    (check-exn exn:fail? (lambda () (ds:signals 42)))))
