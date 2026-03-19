#lang racket/base

(require json
         racket/string)

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

         ds:signals
         ds:computed
         ds:attr
         ds:class
         ds:style

         ds:on
         ds:init
         ds:on-intersect
         ds:on-interval
         ds:on-signal-patch
         ds:on-raf
         ds:on-resize

         ds:ignore
         ds:scroll-into-view
         ds:persist
         ds:query-string
         ds:json-signals
         ds:preserve-attrs)

;; internal helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xattr name value)
  (list (string->symbol name) value))

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

(define (delay-mod #:delay (delay
                             #f))
  (if delay
      (list (list "delay"
                  (if (number? delay)
                      (number->string delay)
                      delay)))
      '()))

(define (viewtransition-mod #:viewtransition [vt #f])
  (if vt
      (list (list "viewtransition"))
      '()))

(define (hash->json h)
  (jsexpr->string h))

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

(define (build-filter-value include exclude)
  (define parts
    (append (if include
                (list (string-append "\"include\": " (jsexpr->string include)))
                '())
            (if exclude
                (list (string-append "\"exclude\": " (jsexpr->string exclude)))
                '())))
  (string-join parts ", " #:before-first "{" #:after-last "}"))

;; simple value attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ds:show expression)
  (xattr "data-show" expression))

(define (ds:text expression)
  (xattr "data-text" expression))

(define (ds:effect expression)
  (xattr "data-effect" expression))

(define (ds:bind signal [value ""])
  (xattr (string-append "data-bind:" signal) value))

(define (ds:ref signal)
  (xattr (string-append "data-ref:" signal) ""))

(define (ds:indicator signal)
  (xattr (string-append "data-indicator:" signal) ""))

(define (ds:ignore-morph)
  (xattr "data-ignore-morph" ""))

(define (ds:view-transition expression)
  (xattr "data-view-transition" expression))

(define (ds:custom-validity expression)
  (xattr "data-custom-validity" expression))

(define (ds:replace-url expression)
  (xattr "data-replace-url" expression))

(define (ds:match-media signal query)
  (xattr (string-append "data-match-media:" signal) query))

;; hash-based attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ds:signals key-or-hash [value-or-unused #f] #:ifmissing [ifmissing #f])
  (cond
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
    [(hash? key-or-hash)
     (define mod-str
       (build-modifier-string (if ifmissing
                                  (list (list "ifmissing"))
                                  '())))
     (xattr (string-append "data-signals" mod-str) (hash->json key-or-hash))]
    [else (error 'ds:signals "expected a symbol/string key or hash, got: ~e" key-or-hash)]))

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

;; event handler: ds:on ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; other event attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ds:init expression
                 #:delay (delay
                           #f)
                 #:viewtransition [viewtransition #f])
  (define mods
    (append (delay-mod #:delay delay) (viewtransition-mod #:viewtransition viewtransition)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-init" mod-str) expression))

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

;; property-like / no-value attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ds:ignore #:self [self #f])
  (define mod-str
    (build-modifier-string (if self
                               (list (list "self"))
                               '())))
  (xattr (string-append "data-ignore" mod-str) ""))

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

(define (ds:preserve-attrs attrs)
  (define value
    (if (list? attrs)
        (string-join attrs " ")
        attrs))
  (xattr "data-preserve-attr" value))
