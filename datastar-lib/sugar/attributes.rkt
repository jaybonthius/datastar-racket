#lang racket/base

(require json
         racket/contract/base
         racket/string)

(define case-style/c (or/c 'camel 'kebab 'snake 'pascal))
(define bind-token/c (or/c symbol? string?))
(define bind-events/c (or/c bind-token/c (listof bind-token/c)))

(provide case-style/c
         (contract-out
          [data-show (-> string? (list/c symbol? string?))]
          [data-text (-> string? (list/c symbol? string?))]
          [data-effect (-> string? (list/c symbol? string?))]
          [data-animate (-> string? (list/c symbol? string?))]
          [data-bind
           (->* [bind-token/c]
                [#:case case-style/c #:prop bind-token/c #:event bind-events/c]
                (list/c symbol? string?))]
          [data-ref (->* [(or/c symbol? string?)] [#:case case-style/c] (list/c symbol? string?))]
          [data-indicator
           (->* [(or/c symbol? string?)] [#:case case-style/c] (list/c symbol? string?))]
          [data-ignore-morph (-> (list/c symbol? string?))]
          [data-view-transition (-> string? (list/c symbol? string?))]
          [data-custom-validity (-> string? (list/c symbol? string?))]
          [data-replace-url (-> string? (list/c symbol? string?))]
          [data-match-media (-> (or/c symbol? string?) string? (list/c symbol? string?))]
          [data-signals
           (->* [(or/c symbol? string?) string?]
                [#:ifmissing? boolean? #:case case-style/c]
                (list/c symbol? string?))]
          [data-signals/hash
           (->* [(hash/c (or/c symbol? string?) any/c)]
                [#:ifmissing? boolean?]
                (list/c symbol? string?))]
          [data-computed
           (->* [(or/c symbol? string?) string?]
                [#:case case-style/c]
                (list/c symbol? string?))]
          [data-computed/hash (-> (hash/c (or/c symbol? string?) string?) (list/c symbol? string?))]
          [data-attr (-> (or/c symbol? string?) string? (list/c symbol? string?))]
          [data-attr/hash (-> (hash/c (or/c symbol? string?) any/c) (list/c symbol? string?))]
          [data-class
           (->* [(or/c symbol? string?) string?]
                [#:case case-style/c]
                (list/c symbol? string?))]
          [data-class/hash (-> (hash/c (or/c symbol? string?) any/c) (list/c symbol? string?))]
          [data-style (-> (or/c symbol? string?) string? (list/c symbol? string?))]
          [data-style/hash (-> (hash/c (or/c symbol? string?) any/c) (list/c symbol? string?))]
          [data-on
           (->* [(or/c symbol? string?) string?]
                [#:once? boolean?
                 #:passive? boolean?
                 #:capture? boolean?
                 #:case case-style/c
                 #:window? boolean?
                 #:document? boolean?
                 #:outside? boolean?
                 #:prevent? boolean?
                 #:stop? boolean?
                 #:debounce (or/c string? number?)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number?)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?
                 #:delay (or/c string? number?)
                 #:viewtransition? boolean?]
                (list/c symbol? string?))]
          [data-init
           (->* [string?]
                [#:delay (or/c string? number?) #:viewtransition? boolean?]
                (list/c symbol? string?))]
          [data-on-intersect
           (->* [string?]
                [#:once? boolean?
                 #:half? boolean?
                 #:full? boolean?
                 #:exit? boolean?
                 #:threshold (or/c string? number?)
                 #:debounce (or/c string? number?)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number?)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?
                 #:delay (or/c string? number?)
                 #:viewtransition? boolean?]
                (list/c symbol? string?))]
          [data-on-interval
           (->* [string?]
                [#:duration (or/c string? number?)
                 #:duration-leading? boolean?
                 #:viewtransition? boolean?]
                (list/c symbol? string?))]
          [data-on-signal-patch
           (->* [string?]
                [#:debounce (or/c string? number?)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number?)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?
                 #:delay (or/c string? number?)]
                (list/c symbol? string?))]
          [data-on-signal-patch-filter
           (->* []
                [#:include string? #:exclude string?]
                (list/c symbol? string?))]
          [data-on-raf
           (->* [string?]
                [#:throttle (or/c string? number?)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?]
                (list/c symbol? string?))]
          [data-on-resize
           (->* [string?]
                [#:debounce (or/c string? number?)
                 #:debounce-leading? boolean?
                 #:debounce-notrailing? boolean?
                 #:throttle (or/c string? number?)
                 #:throttle-noleading? boolean?
                 #:throttle-trailing? boolean?]
                (list/c symbol? string?))]
          [data-ignore (->* [] [#:self? boolean?] (list/c symbol? string?))]
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
                (list/c symbol? string?))]
          [data-persist
           (->* []
                [#:key (or/c symbol? string?)
                 #:include string?
                 #:exclude string?
                 #:session? boolean?]
                (list/c symbol? string?))]
          [data-query-string
           (->* []
                [#:include string?
                 #:exclude string?
                 #:filter? boolean?
                 #:history? boolean?]
                (list/c symbol? string?))]
          [data-json-signals
           (->* []
                [#:include string?
                 #:exclude string?
                 #:terse? boolean?]
                (list/c symbol? string?))]
          [data-preserve-attr (-> (or/c string? (listof string?)) (list/c symbol? string?))]))

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

(define (require-parent-modifier who child-key child? parent-key parent)
  (when (and child? (not parent))
    (raise-arguments-error who
                           (format "~a requires ~a" child-key parent-key)
                           child-key child?
                           parent-key parent)))

(define (timing-mods #:who [who 'timing-mods]
                     #:debounce [debounce #f]
                     #:debounce-leading? [debounce-leading? #f]
                     #:debounce-notrailing? [debounce-notrailing? #f]
                     #:throttle [throttle #f]
                     #:throttle-noleading? [throttle-noleading? #f]
                     #:throttle-trailing? [throttle-trailing? #f])
  (require-parent-modifier who "#:debounce-leading?" debounce-leading? "#:debounce" debounce)
  (require-parent-modifier who "#:debounce-notrailing?" debounce-notrailing? "#:debounce" debounce)
  (require-parent-modifier who "#:throttle-noleading?" throttle-noleading? "#:throttle" throttle)
  (require-parent-modifier who "#:throttle-trailing?" throttle-trailing? "#:throttle" throttle)
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

(define (normalize-json-keys v)
  (cond
    [(hash? v)
     (for/hash ([(k subv) (in-hash v)])
       (values (if (symbol? k)
                   k
                   (string->symbol k))
               (normalize-json-keys subv)))]
    [(list? v)
     (for/list ([subv (in-list v)])
       (normalize-json-keys subv))]
    [else v]))

(define (hash->json h)
  (jsexpr->string (normalize-json-keys h)))

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

(define (normalize-bind-events events)
  (define event-list
    (if (list? events)
        events
        (list events)))
  (for/list ([event-name (in-list event-list)])
    (key->string event-name)))

;; simple value attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-show expression)
  (xattr "data-show" expression))

(define (data-text expression)
  (xattr "data-text" expression))

(define (data-effect expression)
  (xattr "data-effect" expression))

(define (data-animate expression)
  (xattr "data-animate" expression))

(define (data-bind signal
                   #:case (case #f)
                   #:prop [prop #f]
                   #:event [event #f])
  (define prop-mods
    (if prop
        (list (list "prop" (key->string prop)))
        '()))
  (define event-mods
    (if event
        (list (cons "event" (normalize-bind-events event)))
        '()))
  (define mod-str (build-modifier-string (append (case-mod #:case case) prop-mods event-mods)))
  (xattr (string-append "data-bind:" (key->string signal) mod-str) ""))

(define (data-ref signal #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-ref:" (key->string signal) mod-str) ""))

(define (data-indicator signal #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-indicator:" (key->string signal) mod-str) ""))

(define (data-ignore-morph)
  (xattr "data-ignore-morph" ""))

(define (data-view-transition expression)
  (xattr "data-view-transition" expression))

(define (data-custom-validity expression)
  (xattr "data-custom-validity" expression))

(define (data-replace-url expression)
  (xattr "data-replace-url" expression))

(define (data-match-media signal query)
  (xattr (string-append "data-match-media:" (key->string signal)) query))

;; keyed + hash-suffixed attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-signals key expression #:ifmissing? [ifmissing? #f] #:case (case #f))
  (define ifmissing-mods
    (if ifmissing?
        (list (list "ifmissing"))
        '()))
  (define mod-str (build-modifier-string (append (case-mod #:case case) ifmissing-mods)))
  (xattr (string-append "data-signals:" (key->string key) mod-str) expression))

(define (data-signals/hash h #:ifmissing? [ifmissing? #f])
  (define mod-str
    (build-modifier-string (if ifmissing?
                               (list (list "ifmissing"))
                               '())))
  (xattr (string-append "data-signals" mod-str) (hash->json h)))

(define (data-computed key expression #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-computed:" (key->string key) mod-str) expression))

(define (data-computed/hash h)
  (xattr "data-computed" (hash->js-object h)))

(define (data-attr key expression)
  (xattr (string-append "data-attr:" (key->string key)) expression))

(define (data-attr/hash h)
  (xattr "data-attr" (hash->js-object h)))

(define (data-class key expression #:case (case #f))
  (define mod-str (build-modifier-string (case-mod #:case case)))
  (xattr (string-append "data-class:" (key->string key) mod-str) expression))

(define (data-class/hash h)
  (xattr "data-class" (hash->js-object h)))

(define (data-style key expression)
  (xattr (string-append "data-style:" (key->string key)) expression))

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
            (timing-mods #:who 'data-on
                         #:debounce debounce
                         #:debounce-leading? debounce-leading?
                         #:debounce-notrailing? debounce-notrailing?
                         #:throttle throttle
                         #:throttle-noleading? throttle-noleading?
                         #:throttle-trailing? throttle-trailing?)
            (delay-mod #:delay delay)
            (viewtransition-mod #:viewtransition? viewtransition?)))
  (define mod-str (build-modifier-string mods))
  (xattr (string-append "data-on:" (key->string event) mod-str) expression))

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
            (timing-mods #:who 'data-on-intersect
                         #:debounce debounce
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
  (require-parent-modifier 'data-on-interval
                           "#:duration-leading?"
                           duration-leading?
                           "#:duration"
                           duration)
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
    (append (timing-mods #:who 'data-on-signal-patch
                         #:debounce debounce
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
    (timing-mods #:who 'data-on-raf
                 #:throttle throttle
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
    (timing-mods #:who 'data-on-resize
                 #:debounce debounce
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
        (string-append ":" (key->string key))
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
