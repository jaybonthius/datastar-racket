#lang racket

;; Comprehensive unit tests for Datastar attribute generators.
;;
;; Tests every attribute function, every modifier, every combination,
;; and edge cases — covering the full Datastar attribute spec.
;; Each test-case checks both the s-expression output and (where applicable)
;; the rendered HTML via xexpr->string.

(require datastar
         rackunit
         xml)

;; ============================================================================
;; Simple value attributes
;; ============================================================================

(test-case "ds:show basic"
  (check-equal? (ds:show "$count > 0") '(data-show "$count > 0"))
  (check-equal? (xexpr->string `(div (,(ds:show "$count > 0")) ""))
                "<div data-show=\"$count &gt; 0\"></div>"))

(test-case "ds:show with complex expression"
  (check-equal? (ds:show "$foo && $bar || $baz") '(data-show "$foo && $bar || $baz"))
  (check-equal? (xexpr->string `(div (,(ds:show "$foo && $bar")) ""))
                "<div data-show=\"$foo &amp;&amp; $bar\"></div>"))

(test-case "ds:text basic"
  (check-equal? (ds:text "$foo") '(data-text "$foo"))
  (check-equal? (xexpr->string `(span (,(ds:text "$foo")) "")) "<span data-text=\"$foo\"></span>"))

(test-case "ds:text with expression"
  (check-equal? (ds:text "$firstName + ' ' + $lastName") '(data-text "$firstName + ' ' + $lastName")))

(test-case "ds:effect basic"
  (check-equal? (ds:effect "$foo = $bar + $baz") '(data-effect "$foo = $bar + $baz"))
  (check-equal? (xexpr->string `(div (,(ds:effect "$foo = $bar + $baz")) ""))
                "<div data-effect=\"$foo = $bar + $baz\"></div>"))

(test-case "ds:bind basic"
  (check-equal? (ds:bind "foo") (list 'data-bind:foo ""))
  (check-equal? (xexpr->string `(input (,(ds:bind "foo")))) "<input data-bind:foo=\"\"/>"))

(test-case "ds:bind with value"
  (check-equal? (ds:bind "foo" "fooBar") (list 'data-bind:foo "fooBar"))
  (check-equal? (xexpr->string `(input (,(ds:bind "foo" "fooBar"))))
                "<input data-bind:foo=\"fooBar\"/>"))

(test-case "ds:bind with hyphenated signal"
  (check-equal? (ds:bind "foo-bar") (list 'data-bind:foo-bar "")))

(test-case "ds:ref basic"
  (check-equal? (ds:ref "foo") (list 'data-ref:foo ""))
  (check-equal? (xexpr->string `(div (,(ds:ref "myEl")) "")) "<div data-ref:myEl=\"\"></div>"))

(test-case "ds:ref with hyphenated name"
  (check-equal? (ds:ref "my-element") (list 'data-ref:my-element "")))

(test-case "ds:indicator basic"
  (check-equal? (ds:indicator "fetching") (list 'data-indicator:fetching ""))
  (check-equal? (xexpr->string `(button (,(ds:indicator "fetching")) "Go"))
                "<button data-indicator:fetching=\"\">Go</button>"))

(test-case "ds:indicator with hyphenated signal"
  (check-equal? (ds:indicator "is-loading") (list 'data-indicator:is-loading "")))

(test-case "ds:ignore-morph"
  (check-equal? (ds:ignore-morph) '(data-ignore-morph ""))
  (check-equal? (xexpr->string `(div (,(ds:ignore-morph)) "keep"))
                "<div data-ignore-morph=\"\">keep</div>"))

(test-case "ds:view-transition basic"
  (check-equal? (ds:view-transition "$foo") '(data-view-transition "$foo"))
  (check-equal? (xexpr->string `(div (,(ds:view-transition "$foo")) ""))
                "<div data-view-transition=\"$foo\"></div>"))

(test-case "ds:custom-validity basic"
  (check-equal? (ds:custom-validity "$foo === $bar ? '' : 'Values must match'")
                '(data-custom-validity "$foo === $bar ? '' : 'Values must match'"))
  (check-equal?
   (xexpr->string `(input (,(ds:custom-validity "$foo === $bar ? '' : 'Values must match'"))))
   "<input data-custom-validity=\"$foo === $bar ? '' : 'Values must match'\"/>"))

(test-case "ds:replace-url basic"
  (check-equal? (ds:replace-url "`/page${page}`") '(data-replace-url "`/page${page}`"))
  (check-equal? (xexpr->string `(div (,(ds:replace-url "`/page${page}`")) ""))
                "<div data-replace-url=\"`/page${page}`\"></div>"))

(test-case "ds:replace-url with simple path"
  (check-equal? (ds:replace-url "'/new/path'") '(data-replace-url "'/new/path'")))

(test-case "ds:match-media basic"
  (check-equal? (ds:match-media "is-dark" "'prefers-color-scheme: dark'")
                (list 'data-match-media:is-dark "'prefers-color-scheme: dark'"))
  (check-equal? (xexpr->string `(div (,(ds:match-media "is-dark" "'prefers-color-scheme: dark'")) ""))
                "<div data-match-media:is-dark=\"'prefers-color-scheme: dark'\"></div>"))

;; ============================================================================
;; Hash-based attributes: ds:signals
;; ============================================================================

(test-case "ds:signals hash form"
  (check-equal? (ds:signals (hash 'count 0)) '(data-signals "{\"count\":0}"))
  (check-equal? (xexpr->string `(div (,(ds:signals (hash 'count 0))) ""))
                "<div data-signals=\"{&quot;count&quot;:0}\"></div>"))

(test-case "ds:signals hash with multiple values"
  (define result (ds:signals (hash 'count 0 'name "Alice")))
  (check-equal? (first result) 'data-signals)
  ;; JSON key order may vary, so check both are present
  (check-true (string-contains? (second result) "\"count\":0"))
  (check-true (string-contains? (second result) "\"name\":\"Alice\"")))

(test-case "ds:signals hash with nested values"
  (define result (ds:signals (hash 'user (hash 'name "Jay" 'age 30))))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "\"user\":")))

(test-case "ds:signals hash with ifmissing"
  (define result (ds:signals (hash 'count 0) #:ifmissing #t))
  (check-equal? (first result) 'data-signals__ifmissing)
  (check-true (string-contains? (second result) "\"count\":0"))
  (check-equal? (xexpr->string `(div (,(ds:signals (hash 'count 0) #:ifmissing #t)) ""))
                "<div data-signals__ifmissing=\"{&quot;count&quot;:0}\"></div>"))

(test-case "ds:signals keyed form with symbol"
  (check-equal? (ds:signals 'foo "1") (list 'data-signals:foo "1"))
  (check-equal? (xexpr->string `(div (,(ds:signals 'count "0")) ""))
                "<div data-signals:count=\"0\"></div>"))

(test-case "ds:signals keyed form with string"
  (check-equal? (ds:signals "foo" "1") (list 'data-signals:foo "1")))

(test-case "ds:signals keyed form with ifmissing"
  (check-equal? (ds:signals 'foo "1" #:ifmissing #t) (list 'data-signals:foo__ifmissing "1"))
  (check-equal? (xexpr->string `(div (,(ds:signals 'foo "1" #:ifmissing #t)) ""))
                "<div data-signals:foo__ifmissing=\"1\"></div>"))

(test-case "ds:signals keyed form with dot-notation"
  (check-equal? (ds:signals "foo.bar" "1") (list (string->symbol "data-signals:foo.bar") "1")))

(test-case "ds:signals hash with null (signal removal)"
  (define result (ds:signals (hash 'foo 'null)))
  (check-equal? (first result) 'data-signals)
  ;; 'null in Racket's json library serializes to JSON null (unquoted)
  (check-true (string-contains? (second result) "null")))

(test-case "ds:signals hash with boolean"
  (define result (ds:signals (hash 'active #t)))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "true")))

(test-case "ds:signals hash with array"
  (define result (ds:signals (hash 'items '(1 2 3))))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "[1,2,3]")))

;; ============================================================================
;; Hash-based attributes: ds:computed
;; ============================================================================

(test-case "ds:computed keyed form with symbol"
  (check-equal? (ds:computed 'doubled "$count * 2") (list 'data-computed:doubled "$count * 2"))
  (check-equal? (xexpr->string `(div (,(ds:computed 'doubled "$count * 2")) ""))
                "<div data-computed:doubled=\"$count * 2\"></div>"))

(test-case "ds:computed keyed form with string"
  (check-equal? (ds:computed "tripled" "$count * 3") (list 'data-computed:tripled "$count * 3")))

(test-case "ds:computed hash form returns list of pairs"
  (define result (ds:computed (hash 'doubled "$count * 2" 'tripled "$count * 3")))
  (check-true (list? result))
  (check-equal? (length result) 2)
  ;; Each element should be a pair with a symbol key
  (for ([pair (in-list result)])
    (check-true (list? pair))
    (check-equal? (length pair) 2)
    (check-true (string-prefix? (symbol->string (first pair)) "data-computed:"))))

(test-case "ds:computed hash form values"
  (define result (ds:computed (hash 'a "$x + 1" 'b "$y + 2")))
  (define result-hash
    (for/hash ([pair (in-list result)])
      (values (first pair) (second pair))))
  (check-equal? (hash-ref result-hash 'data-computed:a) "$x + 1")
  (check-equal? (hash-ref result-hash 'data-computed:b) "$y + 2"))

(test-case "ds:computed hash form renders in x-expression"
  ;; Hash form returns list of pairs; splice them into the attribute list
  (define pairs (ds:computed (hash 'a "$x + 1")))
  (check-equal? (xexpr->string `(div ,pairs "")) "<div data-computed:a=\"$x + 1\"></div>"))

;; ============================================================================
;; Hash-based attributes: ds:attr
;; ============================================================================

(test-case "ds:attr keyed form"
  (check-equal? (ds:attr "aria-label" "$foo") (list 'data-attr:aria-label "$foo"))
  (check-equal? (xexpr->string `(button (,(ds:attr "disabled" "$foo == ''")) "Save"))
                "<button data-attr:disabled=\"$foo == ''\">Save</button>"))

(test-case "ds:attr keyed form with symbol"
  (check-equal? (ds:attr 'disabled "$bar") (list 'data-attr:disabled "$bar")))

(test-case "ds:attr hash form"
  (define result (ds:attr (hash "aria-label" "$foo")))
  (check-equal? (first result) 'data-attr)
  (check-true (string-contains? (second result) "\"aria-label\": $foo"))
  (check-equal? (xexpr->string `(button (,(ds:attr (hash "aria-label" "$foo"))) "Save"))
                "<button data-attr=\"{&quot;aria-label&quot;: $foo}\">Save</button>"))

(test-case "ds:attr hash form multiple"
  (define result (ds:attr (hash "aria-label" "$foo" "disabled" "$bar")))
  (check-equal? (first result) 'data-attr)
  (check-true (string-contains? (second result) "$foo"))
  (check-true (string-contains? (second result) "$bar")))

;; ============================================================================
;; Hash-based attributes: ds:class
;; ============================================================================

(test-case "ds:class keyed form"
  (check-equal? (ds:class "font-bold" "$foo == 'strong'")
                (list 'data-class:font-bold "$foo == 'strong'"))
  (check-equal? (xexpr->string `(button (,(ds:class "font-bold" "$foo == 'strong'")) "Save"))
                "<button data-class:font-bold=\"$foo == 'strong'\">Save</button>"))

(test-case "ds:class keyed form with symbol"
  (check-equal? (ds:class 'hidden "$!visible") (list 'data-class:hidden "$!visible")))

(test-case "ds:class hash form"
  (define result (ds:class (hash "font-bold" "$foo == 'strong'")))
  (check-equal? (first result) 'data-class)
  (check-true (string-contains? (second result) "\"font-bold\": $foo == 'strong'"))
  (check-equal? (xexpr->string `(button (,(ds:class (hash "active" "$isActive"))) "Save"))
                "<button data-class=\"{&quot;active&quot;: $isActive}\">Save</button>"))

;; ============================================================================
;; Hash-based attributes: ds:style
;; ============================================================================

(test-case "ds:style keyed form"
  (check-equal? (ds:style "display" "$hiding && 'none'")
                (list 'data-style:display "$hiding && 'none'"))
  (check-equal? (xexpr->string `(div (,(ds:style "display" "$hiding && 'none'")) ""))
                "<div data-style:display=\"$hiding &amp;&amp; 'none'\"></div>"))

(test-case "ds:style keyed form with symbol"
  (check-equal? (ds:style 'background-color "$red ? 'red' : 'blue'")
                (list 'data-style:background-color "$red ? 'red' : 'blue'")))

(test-case "ds:style hash form"
  (define result (ds:style (hash "display" "$hiding ? 'none' : 'flex'")))
  (check-equal? (first result) 'data-style)
  (check-true (string-contains? (second result) "\"display\": $hiding ? 'none' : 'flex'"))
  (check-equal? (xexpr->string `(div (,(ds:style (hash "display" "$visible ? 'block' : 'none'"))) ""))
                "<div data-style=\"{&quot;display&quot;: $visible ? 'block' : 'none'}\"></div>"))

;; ============================================================================
;; ds:on -- basic
;; ============================================================================

(test-case "ds:on basic click"
  (check-equal? (ds:on "click" "$foo = ''") (list 'data-on:click "$foo = ''"))
  (check-equal? (xexpr->string `(button (,(ds:on "click" "$foo = ''")) "Reset"))
                "<button data-on:click=\"$foo = ''\">Reset</button>"))

(test-case "ds:on submit"
  (check-equal? (ds:on "submit" "@post('/create')") (list 'data-on:submit "@post('/create')")))

(test-case "ds:on custom event"
  (check-equal? (ds:on "my-event" "$foo = evt.detail") (list 'data-on:my-event "$foo = evt.detail")))

(test-case "ds:on with sse-post integration"
  (check-equal? (ds:on "click" (sse-post "/todo/create"))
                (list 'data-on:click "@post('/todo/create')"))
  (check-equal? (xexpr->string `(button (,(ds:on "click" (sse-post "/create"))) "Go"))
                "<button data-on:click=\"@post('/create')\">Go</button>"))

;; ============================================================================
;; ds:on -- individual modifiers
;; ============================================================================

(test-case "ds:on with once"
  (check-equal? (ds:on "click" "fn()" #:once #t) (list 'data-on:click__once "fn()")))

(test-case "ds:on with passive"
  (check-equal? (ds:on "scroll" "fn()" #:passive #t) (list 'data-on:scroll__passive "fn()")))

(test-case "ds:on with capture"
  (check-equal? (ds:on "click" "fn()" #:capture #t) (list 'data-on:click__capture "fn()")))

(test-case "ds:on with window"
  (check-equal? (ds:on "keydown" "fn()" #:window #t) (list 'data-on:keydown__window "fn()")))

(test-case "ds:on with outside"
  (check-equal? (ds:on "click" "fn()" #:outside #t) (list 'data-on:click__outside "fn()")))

(test-case "ds:on with prevent"
  (check-equal? (ds:on "submit" "fn()" #:prevent #t) (list 'data-on:submit__prevent "fn()"))
  (check-equal? (xexpr->string `(form (,(ds:on "submit" (sse-post "/create") #:prevent #t)) ""))
                "<form data-on:submit__prevent=\"@post('/create')\"></form>"))

(test-case "ds:on with stop"
  (check-equal? (ds:on "click" "fn()" #:stop #t) (list 'data-on:click__stop "fn()")))

(test-case "ds:on with trust"
  (check-equal? (ds:on "click" "fn()" #:trust #t) (list 'data-on:click__trust "fn()")))

;; ============================================================================
;; ds:on -- timing modifiers
;; ============================================================================

(test-case "ds:on with debounce string"
  (check-equal? (ds:on "input" "fn()" #:debounce "300ms")
                (list (string->symbol "data-on:input__debounce.300ms") "fn()"))
  (check-equal? (xexpr->string `(input (,(ds:on "input" (sse-post "/search") #:debounce "300ms"))))
                "<input data-on:input__debounce.300ms=\"@post('/search')\"/>"))

(test-case "ds:on with debounce number"
  (check-equal? (ds:on "input" "fn()" #:debounce 300)
                (list (string->symbol "data-on:input__debounce.300") "fn()")))

(test-case "ds:on with debounce leading"
  (check-equal? (ds:on "click" "fn()" #:debounce "500ms" #:debounce-leading #t)
                (list (string->symbol "data-on:click__debounce.500ms.leading") "fn()"))
  (check-equal?
   (xexpr->string `(button (,(ds:on "click" "fn()" #:debounce "500ms" #:debounce-leading #t)) "Go"))
   "<button data-on:click__debounce.500ms.leading=\"fn()\">Go</button>"))

(test-case "ds:on with debounce notrailing"
  (check-equal? (ds:on "click" "fn()" #:debounce "500ms" #:debounce-notrailing #t)
                (list (string->symbol "data-on:click__debounce.500ms.notrailing") "fn()")))

(test-case "ds:on with debounce leading and notrailing"
  (check-equal?
   (ds:on "click" "fn()" #:debounce "500ms" #:debounce-leading #t #:debounce-notrailing #t)
   (list (string->symbol "data-on:click__debounce.500ms.leading.notrailing") "fn()")))

(test-case "ds:on with throttle string"
  (check-equal? (ds:on "scroll" "fn()" #:throttle "1s")
                (list (string->symbol "data-on:scroll__throttle.1s") "fn()"))
  (check-equal? (xexpr->string `(div (,(ds:on "scroll" "fn()" #:throttle "1s")) ""))
                "<div data-on:scroll__throttle.1s=\"fn()\"></div>"))

(test-case "ds:on with throttle number"
  (check-equal? (ds:on "scroll" "fn()" #:throttle 500)
                (list (string->symbol "data-on:scroll__throttle.500") "fn()")))

(test-case "ds:on with throttle noleading"
  (check-equal? (ds:on "scroll" "fn()" #:throttle "500ms" #:throttle-noleading #t)
                (list (string->symbol "data-on:scroll__throttle.500ms.noleading") "fn()")))

(test-case "ds:on with throttle trailing"
  (check-equal? (ds:on "scroll" "fn()" #:throttle "500ms" #:throttle-trailing #t)
                (list (string->symbol "data-on:scroll__throttle.500ms.trailing") "fn()")))

(test-case "ds:on with delay"
  (check-equal? (ds:on "click" "fn()" #:delay "500ms")
                (list (string->symbol "data-on:click__delay.500ms") "fn()"))
  (check-equal? (xexpr->string `(button (,(ds:on "click" "fn()" #:delay "500ms")) ""))
                "<button data-on:click__delay.500ms=\"fn()\"></button>"))

(test-case "ds:on with delay number"
  (check-equal? (ds:on "click" "fn()" #:delay 1000)
                (list (string->symbol "data-on:click__delay.1000") "fn()")))

(test-case "ds:on with viewtransition"
  (check-equal? (ds:on "click" "fn()" #:viewtransition #t)
                (list 'data-on:click__viewtransition "fn()"))
  (check-equal? (xexpr->string `(button (,(ds:on "click" "fn()" #:viewtransition #t)) ""))
                "<button data-on:click__viewtransition=\"fn()\"></button>"))

;; ============================================================================
;; ds:on -- combined modifiers
;; ============================================================================

(test-case "ds:on with window + debounce"
  (check-equal? (ds:on "click" "$foo = ''" #:window #t #:debounce "500ms")
                (list (string->symbol "data-on:click__window__debounce.500ms") "$foo = ''")))

(test-case "ds:on with window + debounce + leading (matches Datastar docs example)"
  ;; Official Datastar example: data-on:click__window__debounce.500ms.leading="$foo = ''"
  (check-equal? (ds:on "click" "$foo = ''" #:window #t #:debounce "500ms" #:debounce-leading #t)
                (list (string->symbol "data-on:click__window__debounce.500ms.leading") "$foo = ''"))
  (check-equal?
   (xexpr->string
    `(button (,(ds:on "click" "$foo = ''" #:window #t #:debounce "500ms" #:debounce-leading #t)) ""))
   "<button data-on:click__window__debounce.500ms.leading=\"$foo = ''\"></button>"))

(test-case "ds:on with debounce + stop + prevent"
  (check-equal? (ds:on "input" "@post('/search')" #:debounce "300ms" #:prevent #t #:stop #t)
                (list (string->symbol "data-on:input__prevent__stop__debounce.300ms")
                      "@post('/search')"))
  (check-equal?
   (xexpr->string
    `(input (,(ds:on "input" "@post('/search')" #:debounce "300ms" #:prevent #t #:stop #t))))
   "<input data-on:input__prevent__stop__debounce.300ms=\"@post('/search')\"/>"))

(test-case "ds:on with all boolean modifiers"
  (define result
    (ds:on "click"
           "fn()"
           #:once #t
           #:passive #t
           #:capture #t
           #:window #t
           #:outside #t
           #:prevent #t
           #:stop #t
           #:trust #t))
  (define attr-name (symbol->string (first result)))
  (check-true (string-contains? attr-name "__once"))
  (check-true (string-contains? attr-name "__passive"))
  (check-true (string-contains? attr-name "__capture"))
  (check-true (string-contains? attr-name "__window"))
  (check-true (string-contains? attr-name "__outside"))
  (check-true (string-contains? attr-name "__prevent"))
  (check-true (string-contains? attr-name "__stop"))
  (check-true (string-contains? attr-name "__trust"))
  (check-equal? (second result) "fn()"))

;; ============================================================================
;; ds:init
;; ============================================================================

(test-case "ds:init basic"
  (check-equal? (ds:init "$count = 1") '(data-init "$count = 1"))
  (check-equal? (xexpr->string `(div (,(ds:init "@get('/events')")) ""))
                "<div data-init=\"@get('/events')\"></div>"))

(test-case "ds:init with sse-get"
  (check-equal? (ds:init (sse-get "/events")) '(data-init "@get('/events')")))

(test-case "ds:init with delay"
  (check-equal? (ds:init "$count = 1" #:delay "500ms")
                (list (string->symbol "data-init__delay.500ms") "$count = 1"))
  (check-equal? (xexpr->string `(div (,(ds:init "$count = 1" #:delay "500ms")) ""))
                "<div data-init__delay.500ms=\"$count = 1\"></div>"))

(test-case "ds:init with viewtransition"
  (check-equal? (ds:init "fn()" #:viewtransition #t) '(data-init__viewtransition "fn()")))

(test-case "ds:init with delay + viewtransition"
  (check-equal? (ds:init "fn()" #:delay "500ms" #:viewtransition #t)
                (list (string->symbol "data-init__delay.500ms__viewtransition") "fn()")))

;; ============================================================================
;; ds:on-intersect
;; ============================================================================

(test-case "ds:on-intersect basic"
  (check-equal? (ds:on-intersect "$intersected = true") '(data-on-intersect "$intersected = true")))

(test-case "ds:on-intersect with once"
  (check-equal? (ds:on-intersect "fn()" #:once #t) '(data-on-intersect__once "fn()")))

(test-case "ds:on-intersect with half"
  (check-equal? (ds:on-intersect "fn()" #:half #t) '(data-on-intersect__half "fn()")))

(test-case "ds:on-intersect with full"
  (check-equal? (ds:on-intersect "fn()" #:full #t) '(data-on-intersect__full "fn()")))

(test-case "ds:on-intersect with exit"
  (check-equal? (ds:on-intersect "fn()" #:exit #t) '(data-on-intersect__exit "fn()")))

(test-case "ds:on-intersect with threshold number"
  (check-equal? (ds:on-intersect "fn()" #:threshold 75)
                (list (string->symbol "data-on-intersect__threshold.75") "fn()"))
  (check-equal? (xexpr->string `(div (,(ds:on-intersect "fn()" #:threshold 75)) ""))
                "<div data-on-intersect__threshold.75=\"fn()\"></div>"))

(test-case "ds:on-intersect with threshold string"
  (check-equal? (ds:on-intersect "fn()" #:threshold "25")
                (list (string->symbol "data-on-intersect__threshold.25") "fn()")))

(test-case "ds:on-intersect with once + full (matches Datastar docs)"
  ;; Official Datastar example: data-on-intersect__once__full="$fullyIntersected = true"
  (check-equal? (ds:on-intersect "$fullyIntersected = true" #:once #t #:full #t)
                '(data-on-intersect__once__full "$fullyIntersected = true"))
  (check-equal?
   (xexpr->string `(div (,(ds:on-intersect "$fullyIntersected = true" #:once #t #:full #t)) ""))
   "<div data-on-intersect__once__full=\"$fullyIntersected = true\"></div>"))

(test-case "ds:on-intersect with debounce"
  (check-equal? (ds:on-intersect "fn()" #:debounce "500ms")
                (list (string->symbol "data-on-intersect__debounce.500ms") "fn()")))

(test-case "ds:on-intersect with throttle"
  (check-equal? (ds:on-intersect "fn()" #:throttle "1s")
                (list (string->symbol "data-on-intersect__throttle.1s") "fn()")))

(test-case "ds:on-intersect with delay"
  (check-equal? (ds:on-intersect "fn()" #:delay "500ms")
                (list (string->symbol "data-on-intersect__delay.500ms") "fn()")))

(test-case "ds:on-intersect with viewtransition"
  (check-equal? (ds:on-intersect "fn()" #:viewtransition #t)
                '(data-on-intersect__viewtransition "fn()")))

(test-case "ds:on-intersect all modifiers combined"
  (define result (ds:on-intersect "fn()" #:once #t #:half #t #:debounce "500ms" #:viewtransition #t))
  (define attr-name (symbol->string (first result)))
  (check-true (string-contains? attr-name "__once"))
  (check-true (string-contains? attr-name "__half"))
  (check-true (string-contains? attr-name "__debounce.500ms"))
  (check-true (string-contains? attr-name "__viewtransition")))

;; ============================================================================
;; ds:on-interval
;; ============================================================================

(test-case "ds:on-interval basic"
  (check-equal? (ds:on-interval "$count++") '(data-on-interval "$count++"))
  (check-equal? (xexpr->string `(div (,(ds:on-interval "$count++")) ""))
                "<div data-on-interval=\"$count++\"></div>"))

(test-case "ds:on-interval with duration string"
  ;; Official Datastar example: data-on-interval__duration.500ms="$count++"
  (check-equal? (ds:on-interval "$count++" #:duration "500ms")
                (list (string->symbol "data-on-interval__duration.500ms") "$count++"))
  (check-equal? (xexpr->string `(div (,(ds:on-interval "$count++" #:duration "500ms")) ""))
                "<div data-on-interval__duration.500ms=\"$count++\"></div>"))

(test-case "ds:on-interval with duration number"
  (check-equal? (ds:on-interval "$count++" #:duration 2000)
                (list (string->symbol "data-on-interval__duration.2000") "$count++")))

(test-case "ds:on-interval with duration + leading"
  (check-equal? (ds:on-interval "$count++" #:duration "500ms" #:duration-leading #t)
                (list (string->symbol "data-on-interval__duration.500ms.leading") "$count++"))
  (check-equal?
   (xexpr->string `(div (,(ds:on-interval "$count++" #:duration "500ms" #:duration-leading #t)) ""))
   "<div data-on-interval__duration.500ms.leading=\"$count++\"></div>"))

(test-case "ds:on-interval with viewtransition"
  (check-equal? (ds:on-interval "fn()" #:viewtransition #t)
                '(data-on-interval__viewtransition "fn()")))

(test-case "ds:on-interval with duration + viewtransition"
  (check-equal? (ds:on-interval "fn()" #:duration "1s" #:viewtransition #t)
                (list (string->symbol "data-on-interval__duration.1s__viewtransition") "fn()")))

;; ============================================================================
;; ds:on-signal-patch
;; ============================================================================

(test-case "ds:on-signal-patch basic"
  (check-equal? (ds:on-signal-patch "console.log('changed')")
                '(data-on-signal-patch "console.log('changed')")))

(test-case "ds:on-signal-patch with debounce"
  ;; Official Datastar example: data-on-signal-patch__debounce.500ms="doSomething()"
  (check-equal? (ds:on-signal-patch "doSomething()" #:debounce "500ms")
                (list (string->symbol "data-on-signal-patch__debounce.500ms") "doSomething()"))
  (check-equal? (xexpr->string `(div (,(ds:on-signal-patch "doSomething()" #:debounce "500ms")) ""))
                "<div data-on-signal-patch__debounce.500ms=\"doSomething()\"></div>"))

(test-case "ds:on-signal-patch with throttle"
  (check-equal? (ds:on-signal-patch "fn()" #:throttle "1s")
                (list (string->symbol "data-on-signal-patch__throttle.1s") "fn()")))

(test-case "ds:on-signal-patch with delay"
  (check-equal? (ds:on-signal-patch "fn()" #:delay "500ms")
                (list (string->symbol "data-on-signal-patch__delay.500ms") "fn()")))

(test-case "ds:on-signal-patch with filter include"
  (define result (ds:on-signal-patch "fn()" #:include "counter"))
  (check-true (list? (first result))) ; returns list of two attribute pairs
  (check-equal? (first (first result)) 'data-on-signal-patch)
  (check-equal? (second (first result)) "fn()")
  (check-equal? (first (second result)) 'data-on-signal-patch-filter)
  (check-true (string-contains? (second (second result)) "\"include\""))
  ;; HTML rendering with multi-attribute splice
  (check-equal?
   (xexpr->string `(div ,result ""))
   "<div data-on-signal-patch=\"fn()\" data-on-signal-patch-filter=\"{&quot;include&quot;: &quot;counter&quot;}\"></div>"))

(test-case "ds:on-signal-patch with filter exclude"
  (define result (ds:on-signal-patch "fn()" #:exclude "temp"))
  (check-true (list? (first result)))
  (check-true (string-contains? (second (second result)) "\"exclude\"")))

(test-case "ds:on-signal-patch with filter include + exclude"
  (define result (ds:on-signal-patch "fn()" #:include "user" #:exclude "password"))
  (check-true (list? (first result)))
  (check-true (string-contains? (second (second result)) "\"include\""))
  (check-true (string-contains? (second (second result)) "\"exclude\"")))

(test-case "ds:on-signal-patch with debounce + filter"
  (define result (ds:on-signal-patch "fn()" #:debounce "500ms" #:include "counter"))
  (check-true (list? (first result)))
  (check-equal? (first (first result)) (string->symbol "data-on-signal-patch__debounce.500ms")))

;; ============================================================================
;; ds:on-raf
;; ============================================================================

(test-case "ds:on-raf basic"
  (check-equal? (ds:on-raf "$count++") '(data-on-raf "$count++")))

(test-case "ds:on-raf with throttle"
  ;; Official Datastar example: data-on-raf__throttle.10ms="$count++"
  (check-equal? (ds:on-raf "$count++" #:throttle "10ms")
                (list (string->symbol "data-on-raf__throttle.10ms") "$count++"))
  (check-equal? (xexpr->string `(div (,(ds:on-raf "$count++" #:throttle "10ms")) ""))
                "<div data-on-raf__throttle.10ms=\"$count++\"></div>"))

(test-case "ds:on-raf with throttle noleading"
  (check-equal? (ds:on-raf "fn()" #:throttle "10ms" #:throttle-noleading #t)
                (list (string->symbol "data-on-raf__throttle.10ms.noleading") "fn()")))

;; ============================================================================
;; ds:on-resize
;; ============================================================================

(test-case "ds:on-resize basic"
  (check-equal? (ds:on-resize "$count++") '(data-on-resize "$count++")))

(test-case "ds:on-resize with debounce"
  ;; Official Datastar example: data-on-resize__debounce.10ms="$count++"
  (check-equal? (ds:on-resize "$count++" #:debounce "10ms")
                (list (string->symbol "data-on-resize__debounce.10ms") "$count++"))
  (check-equal? (xexpr->string `(div (,(ds:on-resize "$count++" #:debounce "10ms")) ""))
                "<div data-on-resize__debounce.10ms=\"$count++\"></div>"))

(test-case "ds:on-resize with throttle"
  (check-equal? (ds:on-resize "fn()" #:throttle "100ms")
                (list (string->symbol "data-on-resize__throttle.100ms") "fn()")))

(test-case "ds:on-resize with debounce leading"
  (check-equal? (ds:on-resize "fn()" #:debounce "10ms" #:debounce-leading #t)
                (list (string->symbol "data-on-resize__debounce.10ms.leading") "fn()")))

;; ============================================================================
;; ds:ignore
;; ============================================================================

(test-case "ds:ignore basic"
  (check-equal? (ds:ignore) '(data-ignore ""))
  (check-equal? (xexpr->string `(div (,(ds:ignore)) "")) "<div data-ignore=\"\"></div>"))

(test-case "ds:ignore with self"
  (check-equal? (ds:ignore #:self #t) '(data-ignore__self ""))
  (check-equal? (xexpr->string `(div (,(ds:ignore #:self #t)) ""))
                "<div data-ignore__self=\"\"></div>"))

(test-case "ds:ignore self false is same as basic"
  (check-equal? (ds:ignore #:self #f) '(data-ignore "")))

;; ============================================================================
;; ds:scroll-into-view
;; ============================================================================

(test-case "ds:scroll-into-view basic"
  (check-equal? (ds:scroll-into-view) '(data-scroll-into-view ""))
  (check-equal? (xexpr->string `(div (,(ds:scroll-into-view)) ""))
                "<div data-scroll-into-view=\"\"></div>"))

(test-case "ds:scroll-into-view with smooth"
  (check-equal? (ds:scroll-into-view #:smooth #t) '(data-scroll-into-view__smooth "")))

(test-case "ds:scroll-into-view with instant"
  (check-equal? (ds:scroll-into-view #:instant #t) '(data-scroll-into-view__instant "")))

(test-case "ds:scroll-into-view with auto"
  (check-equal? (ds:scroll-into-view #:auto #t) '(data-scroll-into-view__auto "")))

(test-case "ds:scroll-into-view with hstart"
  (check-equal? (ds:scroll-into-view #:hstart #t) '(data-scroll-into-view__hstart "")))

(test-case "ds:scroll-into-view with hcenter"
  (check-equal? (ds:scroll-into-view #:hcenter #t) '(data-scroll-into-view__hcenter "")))

(test-case "ds:scroll-into-view with hend"
  (check-equal? (ds:scroll-into-view #:hend #t) '(data-scroll-into-view__hend "")))

(test-case "ds:scroll-into-view with hnearest"
  (check-equal? (ds:scroll-into-view #:hnearest #t) '(data-scroll-into-view__hnearest "")))

(test-case "ds:scroll-into-view with vstart"
  (check-equal? (ds:scroll-into-view #:vstart #t) '(data-scroll-into-view__vstart "")))

(test-case "ds:scroll-into-view with vcenter"
  (check-equal? (ds:scroll-into-view #:vcenter #t) '(data-scroll-into-view__vcenter "")))

(test-case "ds:scroll-into-view with vend"
  (check-equal? (ds:scroll-into-view #:vend #t) '(data-scroll-into-view__vend "")))

(test-case "ds:scroll-into-view with vnearest"
  (check-equal? (ds:scroll-into-view #:vnearest #t) '(data-scroll-into-view__vnearest "")))

(test-case "ds:scroll-into-view with focus"
  (check-equal? (ds:scroll-into-view #:focus #t) '(data-scroll-into-view__focus "")))

(test-case "ds:scroll-into-view with smooth + vend"
  (check-equal? (ds:scroll-into-view #:smooth #t #:vend #t) '(data-scroll-into-view__smooth__vend ""))
  (check-equal? (xexpr->string `(div (,(ds:scroll-into-view #:smooth #t #:vend #t)) ""))
                "<div data-scroll-into-view__smooth__vend=\"\"></div>"))

(test-case "ds:scroll-into-view with smooth + hcenter + vcenter + focus"
  (define result (ds:scroll-into-view #:smooth #t #:hcenter #t #:vcenter #t #:focus #t))
  (define attr-name (symbol->string (first result)))
  (check-true (string-contains? attr-name "__smooth"))
  (check-true (string-contains? attr-name "__hcenter"))
  (check-true (string-contains? attr-name "__vcenter"))
  (check-true (string-contains? attr-name "__focus")))

;; ============================================================================
;; ds:persist
;; ============================================================================

(test-case "ds:persist basic"
  (check-equal? (ds:persist) '(data-persist ""))
  (check-equal? (xexpr->string `(div (,(ds:persist)) "")) "<div data-persist=\"\"></div>"))

(test-case "ds:persist with key"
  (check-equal? (ds:persist #:key "mykey") (list 'data-persist:mykey ""))
  (check-equal? (xexpr->string `(div (,(ds:persist #:key "mykey")) ""))
                "<div data-persist:mykey=\"\"></div>"))

(test-case "ds:persist with session"
  (check-equal? (ds:persist #:session #t) '(data-persist__session "")))

(test-case "ds:persist with key + session"
  ;; Official Datastar example: data-persist:mykey__session
  (check-equal? (ds:persist #:key "mykey" #:session #t) (list 'data-persist:mykey__session ""))
  (check-equal? (xexpr->string `(div (,(ds:persist #:key "mykey" #:session #t)) ""))
                "<div data-persist:mykey__session=\"\"></div>"))

(test-case "ds:persist with include"
  (define result (ds:persist #:include "foo"))
  (check-equal? (first result) 'data-persist)
  (check-true (string-contains? (second result) "\"include\"")))

(test-case "ds:persist with exclude"
  (define result (ds:persist #:exclude "bar"))
  (check-true (string-contains? (second result) "\"exclude\"")))

(test-case "ds:persist with include + exclude"
  (define result (ds:persist #:include "foo" #:exclude "bar"))
  (check-true (string-contains? (second result) "\"include\""))
  (check-true (string-contains? (second result) "\"exclude\""))
  (check-equal?
   (xexpr->string `(div (,(ds:persist #:include "foo" #:exclude "bar")) ""))
   "<div data-persist=\"{&quot;include&quot;: &quot;foo&quot;, &quot;exclude&quot;: &quot;bar&quot;}\"></div>"))

;; ============================================================================
;; ds:query-string
;; ============================================================================

(test-case "ds:query-string basic"
  (check-equal? (ds:query-string) '(data-query-string ""))
  (check-equal? (xexpr->string `(div (,(ds:query-string)) "")) "<div data-query-string=\"\"></div>"))

(test-case "ds:query-string with history"
  (check-equal? (ds:query-string #:history #t) '(data-query-string__history "")))

(test-case "ds:query-string with filter"
  (check-equal? (ds:query-string #:filter #t) '(data-query-string__filter "")))

(test-case "ds:query-string with filter + history"
  ;; Official Datastar example: data-query-string__filter__history
  (check-equal? (ds:query-string #:filter #t #:history #t) '(data-query-string__filter__history ""))
  (check-equal? (xexpr->string `(div (,(ds:query-string #:filter #t #:history #t)) ""))
                "<div data-query-string__filter__history=\"\"></div>"))

(test-case "ds:query-string with include"
  (define result (ds:query-string #:include "foo"))
  (check-true (string-contains? (second result) "\"include\"")))

(test-case "ds:query-string with include + exclude + history"
  (define result (ds:query-string #:include "foo" #:exclude "bar" #:history #t))
  (check-equal? (first result) 'data-query-string__history)
  (check-true (string-contains? (second result) "\"include\""))
  (check-true (string-contains? (second result) "\"exclude\"")))

;; ============================================================================
;; ds:json-signals
;; ============================================================================

(test-case "ds:json-signals basic"
  (check-equal? (ds:json-signals) '(data-json-signals ""))
  (check-equal? (xexpr->string `(pre (,(ds:json-signals)) "")) "<pre data-json-signals=\"\"></pre>"))

(test-case "ds:json-signals with terse"
  (check-equal? (ds:json-signals #:terse #t) '(data-json-signals__terse ""))
  (check-equal? (xexpr->string `(pre (,(ds:json-signals #:terse #t)) ""))
                "<pre data-json-signals__terse=\"\"></pre>"))

(test-case "ds:json-signals with include"
  (define result (ds:json-signals #:include "counter"))
  (check-true (string-contains? (second result) "\"include\"")))

(test-case "ds:json-signals with include + terse"
  (define result (ds:json-signals #:include "counter" #:terse #t))
  (check-equal? (first result) 'data-json-signals__terse)
  (check-true (string-contains? (second result) "\"include\""))
  (check-equal?
   (xexpr->string `(pre (,(ds:json-signals #:include "counter" #:terse #t)) ""))
   "<pre data-json-signals__terse=\"{&quot;include&quot;: &quot;counter&quot;}\"></pre>"))

;; ============================================================================
;; ds:preserve-attrs
;; ============================================================================

(test-case "ds:preserve-attrs single"
  (check-equal? (ds:preserve-attrs "open") '(data-preserve-attr "open"))
  (check-equal? (xexpr->string `(details (,(ds:preserve-attrs "open") (open "")) "Content"))
                "<details data-preserve-attr=\"open\" open=\"\">Content</details>"))

(test-case "ds:preserve-attrs multiple"
  (check-equal? (ds:preserve-attrs '("open" "class")) '(data-preserve-attr "open class"))
  (check-equal? (xexpr->string `(details (,(ds:preserve-attrs '("open" "class")) (open ""))
                                         "Content"))
                "<details data-preserve-attr=\"open class\" open=\"\">Content</details>"))

(test-case "ds:preserve-attrs single in list"
  (check-equal? (ds:preserve-attrs '("open")) '(data-preserve-attr "open")))

;; ============================================================================
;; X-expression integration -- usage patterns
;; ============================================================================

(test-case "attribute in x-expression template"
  ;; Verify the attribute pair format works in x-expression context
  (define attr (ds:on "click" "$count++"))
  (check-equal? (length attr) 2)
  (check-true (symbol? (first attr)))
  (check-true (string? (second attr))))

(test-case "multiple attributes in x-expression"
  ;; Simulates: `(div (,(ds:bind "filter") ,(ds:indicator "filtering")))
  (define attrs (list (ds:bind "filter") (ds:indicator "filtering")))
  (check-equal? (length attrs) 2)
  (check-equal? (first (first attrs)) 'data-bind:filter)
  (check-equal? (first (second attrs)) 'data-indicator:filtering))

(test-case "init with sse-get in x-expression"
  ;; Simulates: `(main ((id "main") ,(ds:init (sse-get "/events"))))
  (define attr (ds:init (sse-get "/events")))
  (check-equal? attr '(data-init "@get('/events')")))

(test-case "on with sse-post and format in x-expression"
  ;; Simulates: (ds:on "click" (sse-post (format "/todo/delete/~a" 42)))
  (define tid 42)
  (define attr (ds:on "click" (sse-post (format "/todo/delete/~a" tid))))
  (check-equal? attr (list 'data-on:click "@post('/todo/delete/42')")))

(test-case "complex form with debounce pattern"
  ;; Simulates the common search input pattern
  (define input-attr (ds:on "input" (sse-post "/search") #:debounce "250ms"))
  (define bind-attr (ds:bind "filter"))
  (define indicator-attr (ds:indicator "filtering"))
  (check-equal? input-attr (list (string->symbol "data-on:input__debounce.250ms") "@post('/search')"))
  (check-equal? bind-attr (list 'data-bind:filter ""))
  (check-equal? indicator-attr (list 'data-indicator:filtering "")))

(test-case "xexpr->string round-trip"
  (check-equal? (xexpr->string `(div (,(ds:show "$visible") (id "test")) "hello"))
                "<div data-show=\"$visible\" id=\"test\">hello</div>"))

;; ============================================================================
;; X-expression integration -- realistic templates
;; ============================================================================

(test-case "search input pattern renders correct HTML"
  (check-equal?
   (xexpr->string `(input (,(ds:on "input" (sse-post "/search") #:debounce "250ms")
                           ,(ds:bind "filter")
                           ,(ds:indicator "filtering"))))
   "<input data-on:input__debounce.250ms=\"@post('/search')\" data-bind:filter=\"\" data-indicator:filtering=\"\"/>"))

(test-case "form with sse-post renders correct HTML"
  (check-equal?
   (xexpr->string `(form (,(ds:on "submit" (sse-post "/todo/create") #:prevent #t))
                         (input ((type "text") ,(ds:bind "newTodo")))
                         (button ((type "submit")) "Add")))
   "<form data-on:submit__prevent=\"@post('/todo/create')\"><input type=\"text\" data-bind:newTodo=\"\"/><button type=\"submit\">Add</button></form>"))

(test-case "page with init + signals renders correct HTML"
  (check-equal?
   (xexpr->string `(body (,(ds:init (sse-get "/updates")) ,(ds:signals (hash 'count 0)))
                         (span (,(ds:text "$count")) "")))
   "<body data-init=\"@get('/updates')\" data-signals=\"{&quot;count&quot;:0}\"><span data-text=\"$count\"></span></body>"))

;; ============================================================================
;; Edge cases
;; ============================================================================

(test-case "ds:on with no modifiers returns clean attribute"
  (define attr (ds:on "click" "fn()"))
  (check-false (string-contains? (symbol->string (first attr)) "__")
               "no modifiers should mean no __ in attribute name"))

(test-case "ds:on-intersect with no modifiers returns clean attribute"
  (define attr (ds:on-intersect "fn()"))
  (check-false (string-contains? (symbol->string (first attr)) "__")))

(test-case "ds:on-interval with no modifiers returns clean attribute"
  (define attr (ds:on-interval "fn()"))
  (check-false (string-contains? (symbol->string (first attr)) "__")))

(test-case "ds:signals with empty hash"
  (check-equal? (ds:signals (hash)) '(data-signals "{}")))

(test-case "ds:computed single entry hash returns list with one pair"
  (define result (ds:computed (hash 'x "$y")))
  (check-true (list? result))
  (check-equal? (length result) 1)
  (check-equal? (first result) (list 'data-computed:x "$y")))

(test-case "ds:on-signal-patch without filter returns single pair"
  (define result (ds:on-signal-patch "fn()"))
  ;; Without filter, returns a flat pair
  (check-equal? (length result) 2)
  (check-true (symbol? (first result))))

(test-case "ds:on-signal-patch with filter returns list of pairs"
  (define result (ds:on-signal-patch "fn()" #:include "x"))
  ;; With filter, returns a list of two pairs
  (check-true (list? (first result)))
  (check-equal? (length result) 2))

(test-case "ds:scroll-into-view with no modifiers"
  (define attr (ds:scroll-into-view))
  (check-false (string-contains? (symbol->string (first attr)) "__")))

(test-case "ds:persist with no options"
  (define attr (ds:persist))
  (check-false (string-contains? (symbol->string (first attr)) "__"))
  (check-false (string-contains? (symbol->string (first attr)) ":")))

(test-case "ds:signals error on invalid input"
  (check-exn exn:fail? (lambda () (ds:signals 42))))

(test-case "ds:computed error on invalid input"
  (check-exn exn:fail? (lambda () (ds:computed 42))))

(test-case "ds:attr error on invalid input"
  (check-exn exn:fail? (lambda () (ds:attr 42))))

(test-case "ds:class error on invalid input"
  (check-exn exn:fail? (lambda () (ds:class 42))))

(test-case "ds:style error on invalid input"
  (check-exn exn:fail? (lambda () (ds:style 42))))
