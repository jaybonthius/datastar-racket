#lang racket/base

(require json
         racket/contract/base
         racket/string)

(define case-style/c (or/c 'camel 'kebab 'snake 'pascal #f))
(define attr-pair/c (list/c symbol? string?))
(define hash-key/c (or/c symbol? string?))

(define (hash-js-value? v)
  (or (string? v)
      (jsexpr? v)
      (and (hash? v)
           (for/and ([(k subv) (in-hash v)])
             (and (or (symbol? k) (string? k)) (hash-js-value? subv))))))

(define hash-js-value/c (flat-named-contract 'hash-js-value/c hash-js-value?))

(define hash-values/c (hash/c hash-key/c hash-js-value/c))
(define signals-hash/c (and/c hash? jsexpr?))
(define computed-hash/c (hash/c hash-key/c string?))

(provide (contract-out
          [data-show (-> string? attr-pair/c)]
          [data-text (-> string? attr-pair/c)]
          [data-effect (-> string? attr-pair/c)]
          [data-animate (-> string? attr-pair/c)]
          [data-bind (->* [string?] [#:case case-style/c] attr-pair/c)]
          [data-ref (->* [string?] [#:case case-style/c] attr-pair/c)]
          [data-indicator (->* [string?] [#:case case-style/c] attr-pair/c)]
          [data-ignore-morph (-> attr-pair/c)]
          [data-view-transition (-> string? attr-pair/c)]
          [data-custom-validity (-> string? attr-pair/c)]
          [data-replace-url (-> string? attr-pair/c)]
          [data-match-media (-> string? string? attr-pair/c)]
          [data-signals
           (->* [(or/c symbol? string?) string?]
                [#:ifmissing? boolean? #:case case-style/c]
                attr-pair/c)]
          [data-signals/hash (->* [signals-hash/c] [#:ifmissing? boolean?] attr-pair/c)]
          [data-computed (->* [(or/c symbol? string?) string?] [#:case case-style/c] attr-pair/c)]
          [data-computed/hash (-> computed-hash/c attr-pair/c)]
          [data-attr (-> (or/c symbol? string?) string? attr-pair/c)]
          [data-attr/hash (-> hash-values/c attr-pair/c)]
          [data-class (->* [(or/c symbol? string?) string?] [#:case case-style/c] attr-pair/c)]
          [data-class/hash (-> hash-values/c attr-pair/c)]
          [data-style (-> (or/c symbol? string?) string? attr-pair/c)]
          [data-style/hash (-> hash-values/c attr-pair/c)]
          [data-on
           (->* [string? string?]
                [#:once? boolean?
                 #:passive? boolean?
                 #:capture? boolean?
                 #:case case-style/c
                 #:window? boolean?
                 #:document? boolean?
                 #:outside? boolean?
                 #:prevent? boolean?
                 #:stop? boolean?
                 #:debounce (or/c string? number? #f)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number? #f)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?
                 #:delay (or/c string? number? #f)
                 #:viewtransition? boolean?]
                attr-pair/c)]
          [data-init
           (->* [string?] [#:delay (or/c string? number? #f) #:viewtransition? boolean?] attr-pair/c)]
          [data-on-intersect
           (->* [string?]
                [#:once? boolean?
                 #:half? boolean?
                 #:full? boolean?
                 #:exit? boolean?
                 #:threshold (or/c string? number? #f)
                 #:debounce (or/c string? number? #f)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number? #f)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?
                 #:delay (or/c string? number? #f)
                 #:viewtransition? boolean?]
                attr-pair/c)]
          [data-on-interval
           (->* [string?]
                [#:duration (or/c string? number? #f)
                 #:duration-leading? boolean?
                 #:viewtransition? boolean?]
                attr-pair/c)]
          [data-on-signal-patch
           (->* [string?]
                [#:debounce (or/c string? number? #f)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number? #f)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?
                 #:delay (or/c string? number? #f)]
                attr-pair/c)]
          [data-on-signal-patch-filter
           (->* [] [#:include (or/c string? #f) #:exclude (or/c string? #f)] attr-pair/c)]
          [data-on-raf
           (->* [string?]
                [#:throttle (or/c string? number? #f)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?]
                attr-pair/c)]
          [data-on-resize
           (->* [string?]
                [#:debounce (or/c string? number? #f)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number? #f)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?]
                attr-pair/c)]
          [data-ignore (->* [] [#:self? boolean?] attr-pair/c)]
          [data-scroll-into-view
           (->* []
                [#:smooth? boolean?
                 #:instant? boolean?
                 #:auto? boolean?
                 #:hstart? boolean?
                 #:hcenter? boolean?
                 #:hend? boolean?
                 #:hnearest? boolean?
                 #:vstart? boolean?
                 #:vcenter? boolean?
                 #:vend? boolean?
                 #:vnearest? boolean?
                 #:focus? boolean?]
                attr-pair/c)]
          [data-persist
           (->* []
                [#:key (or/c string? #f)
                 #:include (or/c string? #f)
                 #:exclude (or/c string? #f)
                 #:session? boolean?]
                attr-pair/c)]
          [data-query-string
           (->* []
                [#:include (or/c string? #f)
                 #:exclude (or/c string? #f)
                 #:filter? boolean?
                 #:history? boolean?]
                attr-pair/c)]
          [data-json-signals
           (->* []
                [#:include (or/c string? #f) #:exclude (or/c string? #f) #:terse? boolean?]
                attr-pair/c)]
          [data-preserve-attr (-> (or/c string? (listof string?)) attr-pair/c)]
          [data-preserve-attrs (-> (or/c string? (listof string?)) attr-pair/c)]))

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

(define (case-mod #:case (case #f))
  (if case
      (list (list "case" (symbol->string case)))
      '()))

(define (timing-mods #:debounce [debounce #f]
                     #:debounce-leading? [debounce-leading? #f]
                     #:debounce-notrailing? [debounce-notrailing? #f]
                     #:throttle [throttle #f]
                     #:throttle-noleading? [throttle-noleading? #f]
                     #:throttle-trailing? [throttle-trailing? #f])
  (append (if debounce
              (list (cons "debounce"
                          (append (list (if (number? debounce)
                                            (number->string debounce)
                                            debounce))
                                  (if debounce-leading?
                                      '("leading")
                                      '())
                                  (if debounce-notrailing?
                                      '("notrailing")
                                      '()))))
              '())
          (if throttle
              (list (cons "throttle"
                          (append (list (if (number? throttle)
                                            (number->string throttle)
                                            throttle))
                                  (if throttle-noleading?
                                      '("noleading")
                                      '())
                                  (if throttle-trailing?
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

(define (viewtransition-mod #:viewtransition? [vt? #f])
  (if vt?
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

(define (key->string key)
  (if (symbol? key)
      (symbol->string key)
      key))

;; simple value attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-show expression)
  (xattr "data-show" expression))

(define (data-text expression)
  (xattr "data-text" expression))

(define (data-effect expression)
  (xattr "data-effect" expression))

(define (data-animate expression)
  (xattr "data-animate" expression))

(define (data-bind signal #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-bind:" signal mod-str) ""))

(define (data-ref signal #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-ref:" signal mod-str) ""))

(define (data-indicator signal #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-indicator:" signal mod-str) ""))

(define (data-ignore-morph)
  (xattr "data-ignore-morph" ""))

(define (data-view-transition expression)
  (xattr "data-view-transition" expression))

(define (data-custom-validity expression)
  (xattr "data-custom-validity" expression))

(define (data-replace-url expression)
  (xattr "data-replace-url" expression))

(define (data-match-media signal query)
  (xattr (string-append "data-match-media:" signal) query))

;; keyed + hash-suffixed attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-signals key value #:ifmissing? [ifmissing? #f] #:case (case #f))
  (define ifmissing-mods
    (if ifmissing?
        (list (list "ifmissing"))
        '()))
  (define mod-str (build-modifier-string (append (case-mod #:case case) ifmissing-mods)))
  (xattr (string-append "data-signals:" (key->string key) mod-str) value))

(define (data-signals/hash h #:ifmissing? [ifmissing? #f])
  (define mod-str
    (build-modifier-string (if ifmissing?
                               (list (list "ifmissing"))
                               '())))
  (xattr (string-append "data-signals" mod-str) (hash->json h)))

(define (data-computed key value #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-computed:" (key->string key) mod-str) value))

(define (data-computed/hash h)
  (xattr "data-computed" (hash->js-object h)))

(define (data-attr key value)
  (xattr (string-append "data-attr:" (key->string key)) value))

(define (data-attr/hash h)
  (xattr "data-attr" (hash->js-object h)))

(define (data-class key value #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-class:" (key->string key) mod-str) value))

(define (data-class/hash h)
  (xattr "data-class" (hash->js-object h)))

(define (data-style key value)
  (xattr (string-append "data-style:" (key->string key)) value))

(define (data-style/hash h)
  (xattr "data-style" (hash->js-object h)))

;; event handler: data-on ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-on event
                 expression
                 #:once? [once? #f]
                 #:passive? [passive? #f]
                 #:capture? [capture? #f]
                 #:case (case #f)
                 #:window? [window? #f]
                 #:document? [document? #f]
                 #:outside? [outside? #f]
                 #:prevent? [prevent? #f]
                 #:stop? [stop? #f]
                 #:debounce [debounce #f]
                 #:debounce-leading? [debounce-leading? #f]
                 #:debounce-notrailing? [debounce-notrailing? #f]
                 #:throttle [throttle #f]
                 #:throttle-noleading? [throttle-noleading? #f]
                 #:throttle-trailing? [throttle-trailing? #f]
                 #:delay (delay
                           #f)
                 #:viewtransition? [viewtransition? #f])
  (define mods
    (append (if once?
                (list (list "once"))
                '())
            (if passive?
                (list (list "passive"))
                '())
            (if capture?
                (list (list "capture"))
                '())
            (case-mod #:case case)
            (if window?
                (list (list "window"))
                '())
            (if document?
                (list (list "document"))
                '())
            (if outside?
                (list (list "outside"))
                '())
            (if prevent?
                (list (list "prevent"))
                '())
            (if stop?
                (list (list "stop"))
                '())
            (timing-mods #:debounce debounce
                         #:debounce-leading? debounce-leading?
                         #:debounce-notrailing? debounce-notrailing?
                         #:throttle throttle
                         #:throttle-noleading? throttle-noleading?
                         #:throttle-trailing? throttle-trailing?)
            (delay-mod #:delay delay)
            (viewtransition-mod #:viewtransition? viewtransition?)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on:" event mod-str) expression))

;; other event attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-init expression
                   #:delay (delay
                             #f)
                   #:viewtransition? [viewtransition? #f])
  (define mods
    (append (delay-mod #:delay delay) (viewtransition-mod #:viewtransition? viewtransition?)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-init" mod-str) expression))

(define (data-on-intersect expression
                           #:once? [once? #f]
                           #:half? [half? #f]
                           #:full? [full? #f]
                           #:exit? [exit? #f]
                           #:threshold [threshold #f]
                           #:debounce [debounce #f]
                           #:debounce-leading? [debounce-leading? #f]
                           #:debounce-notrailing? [debounce-notrailing? #f]
                           #:throttle [throttle #f]
                           #:throttle-noleading? [throttle-noleading? #f]
                           #:throttle-trailing? [throttle-trailing? #f]
                           #:delay (delay
                                     #f)
                           #:viewtransition? [viewtransition? #f])
  (define mods
    (append (if once?
                (list (list "once"))
                '())
            (if half?
                (list (list "half"))
                '())
            (if full?
                (list (list "full"))
                '())
            (if exit?
                (list (list "exit"))
                '())
            (if threshold
                (list (list "threshold"
                            (if (number? threshold)
                                (number->string threshold)
                                threshold)))
                '())
            (timing-mods #:debounce debounce
                         #:debounce-leading? debounce-leading?
                         #:debounce-notrailing? debounce-notrailing?
                         #:throttle throttle
                         #:throttle-noleading? throttle-noleading?
                         #:throttle-trailing? throttle-trailing?)
            (delay-mod #:delay delay)
            (viewtransition-mod #:viewtransition? viewtransition?)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-intersect" mod-str) expression))

(define (data-on-interval expression
                          #:duration [duration #f]
                          #:duration-leading? [duration-leading? #f]
                          #:viewtransition? [viewtransition? #f])
  (define mods
    (append (if duration
                (list (cons "duration"
                            (append (list (if (number? duration)
                                              (number->string duration)
                                              duration))
                                    (if duration-leading?
                                        '("leading")
                                        '()))))
                '())
            (viewtransition-mod #:viewtransition? viewtransition?)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-interval" mod-str) expression))

(define (data-on-signal-patch-filter #:include [include #f] #:exclude [exclude #f])
  (xattr "data-on-signal-patch-filter" (build-filter-value include exclude)))

(define (data-on-signal-patch expression
                              #:debounce [debounce #f]
                              #:debounce-leading? [debounce-leading? #f]
                              #:debounce-notrailing? [debounce-notrailing? #f]
                              #:throttle [throttle #f]
                              #:throttle-noleading? [throttle-noleading? #f]
                              #:throttle-trailing? [throttle-trailing? #f]
                              #:delay (delay
                                        #f))
  (define mods
    (append (timing-mods #:debounce debounce
                         #:debounce-leading? debounce-leading?
                         #:debounce-notrailing? debounce-notrailing?
                         #:throttle throttle
                         #:throttle-noleading? throttle-noleading?
                         #:throttle-trailing? throttle-trailing?)
            (delay-mod #:delay delay)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-signal-patch" mod-str) expression))

(define (data-on-raf expression
                     #:throttle [throttle #f]
                     #:throttle-noleading? [throttle-noleading? #f]
                     #:throttle-trailing? [throttle-trailing? #f])
  (define mods
    (timing-mods #:throttle throttle
                 #:throttle-noleading? throttle-noleading?
                 #:throttle-trailing? throttle-trailing?))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-raf" mod-str) expression))

(define (data-on-resize expression
                        #:debounce [debounce #f]
                        #:debounce-leading? [debounce-leading? #f]
                        #:debounce-notrailing? [debounce-notrailing? #f]
                        #:throttle [throttle #f]
                        #:throttle-noleading? [throttle-noleading? #f]
                        #:throttle-trailing? [throttle-trailing? #f])
  (define mods
    (timing-mods #:debounce debounce
                 #:debounce-leading? debounce-leading?
                 #:debounce-notrailing? debounce-notrailing?
                 #:throttle throttle
                 #:throttle-noleading? throttle-noleading?
                 #:throttle-trailing? throttle-trailing?))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on-resize" mod-str) expression))

;; property-like / no-value attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-ignore #:self? [self? #f])
  (define mod-str
    (build-modifier-string (if self?
                               (list (list "self"))
                               '())))
  (xattr (string-append "data-ignore" mod-str) ""))

(define (data-scroll-into-view #:smooth? [smooth? #f]
                               #:instant? [instant? #f]
                               #:auto? [auto? #f]
                               #:hstart? [hstart? #f]
                               #:hcenter? [hcenter? #f]
                               #:hend? [hend? #f]
                               #:hnearest? [hnearest? #f]
                               #:vstart? [vstart? #f]
                               #:vcenter? [vcenter? #f]
                               #:vend? [vend? #f]
                               #:vnearest? [vnearest? #f]
                               #:focus? [focus? #f])
  (define mods
    (append (if smooth?
                (list (list "smooth"))
                '())
            (if instant?
                (list (list "instant"))
                '())
            (if auto?
                (list (list "auto"))
                '())
            (if hstart?
                (list (list "hstart"))
                '())
            (if hcenter?
                (list (list "hcenter"))
                '())
            (if hend?
                (list (list "hend"))
                '())
            (if hnearest?
                (list (list "hnearest"))
                '())
            (if vstart?
                (list (list "vstart"))
                '())
            (if vcenter?
                (list (list "vcenter"))
                '())
            (if vend?
                (list (list "vend"))
                '())
            (if vnearest?
                (list (list "vnearest"))
                '())
            (if focus?
                (list (list "focus"))
                '())))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-scroll-into-view" mod-str) ""))

(define (data-persist #:key [key #f]
                      #:include [include #f]
                      #:exclude [exclude #f]
                      #:session? [session? #f])
  (define mod-str
    (build-modifier-string (if session?
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

(define (data-query-string #:include [include #f]
                           #:exclude [exclude #f]
                           #:filter? [filter? #f]
                           #:history? [history? #f])
  (define mods
    (append (if filter?
                (list (list "filter"))
                '())
            (if history?
                (list (list "history"))
                '())))
  (define mod-str (build-modifier-string mods))
  (define value
    (if (or include exclude)
        (build-filter-value include exclude)
        ""))
  (xattr (string-append "data-query-string" mod-str) value))

(define (data-json-signals #:include [include #f] #:exclude [exclude #f] #:terse? [terse? #f])
  (define mod-str
    (build-modifier-string (if terse?
                               (list (list "terse"))
                               '())))
  (define value
    (if (or include exclude)
        (build-filter-value include exclude)
        ""))
  (xattr (string-append "data-json-signals" mod-str) value))

(define (data-preserve-attr attrs)
  (define value
    (if (list? attrs)
        (string-join attrs " ")
        attrs))
  (xattr "data-preserve-attr" value))

(define (data-preserve-attrs attrs)
  (data-preserve-attr attrs))
