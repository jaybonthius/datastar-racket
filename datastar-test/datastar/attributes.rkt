#lang racket

(require datastar
         rackunit
         xml)

;; simple value attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-show basic"
  (check-equal? (data-show "$count > 0") '(data-show "$count > 0"))
  (check-equal? (xexpr->string `(div (,(data-show "$count > 0")) ""))
                "<div data-show=\"$count &gt; 0\"></div>"))

(test-case "data-show with complex expression"
  (check-equal? (data-show "$foo && $bar || $baz") '(data-show "$foo && $bar || $baz"))
  (check-equal? (xexpr->string `(div (,(data-show "$foo && $bar")) ""))
                "<div data-show=\"$foo &amp;&amp; $bar\"></div>"))

(test-case "data-text basic"
  (check-equal? (data-text "$foo") '(data-text "$foo"))
  (check-equal? (xexpr->string `(span (,(data-text "$foo")) "")) "<span data-text=\"$foo\"></span>"))

(test-case "data-text with expression"
  (check-equal? (data-text "$firstName + ' ' + $lastName")
                '(data-text "$firstName + ' ' + $lastName")))

(test-case "data-effect basic"
  (check-equal? (data-effect "$foo = $bar + $baz") '(data-effect "$foo = $bar + $baz"))
  (check-equal? (xexpr->string `(div (,(data-effect "$foo = $bar + $baz")) ""))
                "<div data-effect=\"$foo = $bar + $baz\"></div>"))

(test-case "data-bind basic"
  (check-equal? (data-bind "foo") (list 'data-bind:foo ""))
  (check-equal? (xexpr->string `(input (,(data-bind "foo")))) "<input data-bind:foo=\"\"/>"))

(test-case "data-bind with value"
  (check-equal? (data-bind "foo" "fooBar") (list 'data-bind:foo "fooBar"))
  (check-equal? (xexpr->string `(input (,(data-bind "foo" "fooBar"))))
                "<input data-bind:foo=\"fooBar\"/>"))

(test-case "data-bind with hyphenated signal"
  (check-equal? (data-bind "foo-bar") (list 'data-bind:foo-bar "")))

(test-case "data-ref basic"
  (check-equal? (data-ref "foo") (list 'data-ref:foo ""))
  (check-equal? (xexpr->string `(div (,(data-ref "myEl")) "")) "<div data-ref:myEl=\"\"></div>"))

(test-case "data-ref with hyphenated name"
  (check-equal? (data-ref "my-element") (list 'data-ref:my-element "")))

(test-case "data-indicator basic"
  (check-equal? (data-indicator "fetching") (list 'data-indicator:fetching ""))
  (check-equal? (xexpr->string `(button (,(data-indicator "fetching")) "Go"))
                "<button data-indicator:fetching=\"\">Go</button>"))

(test-case "data-indicator with hyphenated signal"
  (check-equal? (data-indicator "is-loading") (list 'data-indicator:is-loading "")))

(test-case "data-ignore-morph"
  (check-equal? (data-ignore-morph) '(data-ignore-morph ""))
  (check-equal? (xexpr->string `(div (,(data-ignore-morph)) "keep"))
                "<div data-ignore-morph=\"\">keep</div>"))

(test-case "data-view-transition basic"
  (check-equal? (data-view-transition "$foo") '(data-view-transition "$foo"))
  (check-equal? (xexpr->string `(div (,(data-view-transition "$foo")) ""))
                "<div data-view-transition=\"$foo\"></div>"))

(test-case "data-custom-validity basic"
  (check-equal? (data-custom-validity "$foo === $bar ? '' : 'Values must match'")
                '(data-custom-validity "$foo === $bar ? '' : 'Values must match'"))
  (check-equal?
   (xexpr->string `(input (,(data-custom-validity "$foo === $bar ? '' : 'Values must match'"))))
   "<input data-custom-validity=\"$foo === $bar ? '' : 'Values must match'\"/>"))

(test-case "data-replace-url basic"
  (check-equal? (data-replace-url "`/page${page}`") '(data-replace-url "`/page${page}`"))
  (check-equal? (xexpr->string `(div (,(data-replace-url "`/page${page}`")) ""))
                "<div data-replace-url=\"`/page${page}`\"></div>"))

(test-case "data-replace-url with simple path"
  (check-equal? (data-replace-url "'/new/path'") '(data-replace-url "'/new/path'")))

(test-case "data-match-media basic"
  (check-equal? (data-match-media "is-dark" "'prefers-color-scheme: dark'")
                (list 'data-match-media:is-dark "'prefers-color-scheme: dark'"))
  (check-equal? (xexpr->string `(div (,(data-match-media "is-dark" "'prefers-color-scheme: dark'"))
                                     ""))
                "<div data-match-media:is-dark=\"'prefers-color-scheme: dark'\"></div>"))

;; hash-based attributes: data-signals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-signals hash form"
  (check-equal? (data-signals (hash 'count 0)) '(data-signals "{\"count\":0}"))
  (check-equal? (xexpr->string `(div (,(data-signals (hash 'count 0))) ""))
                "<div data-signals=\"{&quot;count&quot;:0}\"></div>"))

(test-case "data-signals hash with multiple values"
  (define result (data-signals (hash 'count 0 'name "Alice")))
  (check-equal? (first result) 'data-signals)
  ;; JSON key order may vary
  (check-true (string-contains? (second result) "\"count\":0"))
  (check-true (string-contains? (second result) "\"name\":\"Alice\"")))

(test-case "data-signals hash with nested values"
  (define result (data-signals (hash 'user (hash 'name "Jay" 'age 30))))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "\"user\":")))

(test-case "data-signals hash with ifmissing"
  (define result (data-signals (hash 'count 0) #:ifmissing #t))
  (check-equal? (first result) 'data-signals__ifmissing)
  (check-true (string-contains? (second result) "\"count\":0"))
  (check-equal? (xexpr->string `(div (,(data-signals (hash 'count 0) #:ifmissing #t)) ""))
                "<div data-signals__ifmissing=\"{&quot;count&quot;:0}\"></div>"))

(test-case "data-signals keyed form with symbol"
  (check-equal? (data-signals 'foo "1") (list 'data-signals:foo "1"))
  (check-equal? (xexpr->string `(div (,(data-signals 'count "0")) ""))
                "<div data-signals:count=\"0\"></div>"))

(test-case "data-signals keyed form with string"
  (check-equal? (data-signals "foo" "1") (list 'data-signals:foo "1")))

(test-case "data-signals keyed form with ifmissing"
  (check-equal? (data-signals 'foo "1" #:ifmissing #t) (list 'data-signals:foo__ifmissing "1"))
  (check-equal? (xexpr->string `(div (,(data-signals 'foo "1" #:ifmissing #t)) ""))
                "<div data-signals:foo__ifmissing=\"1\"></div>"))

(test-case "data-signals keyed form with dot-notation"
  (check-equal? (data-signals "foo.bar" "1") (list (string->symbol "data-signals:foo.bar") "1")))

(test-case "data-signals hash with null (signal removal)"
  (define result (data-signals (hash 'foo 'null)))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "null")))

(test-case "data-signals hash with boolean"
  (define result (data-signals (hash 'active #t)))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "true")))

(test-case "data-signals hash with array"
  (define result (data-signals (hash 'items '(1 2 3))))
  (check-equal? (first result) 'data-signals)
  (check-true (string-contains? (second result) "[1,2,3]")))

;; hash-based attributes: data-computed ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-computed keyed form with symbol"
  (check-equal? (data-computed 'doubled "$count * 2") (list 'data-computed:doubled "$count * 2"))
  (check-equal? (xexpr->string `(div (,(data-computed 'doubled "$count * 2")) ""))
                "<div data-computed:doubled=\"$count * 2\"></div>"))

(test-case "data-computed keyed form with string"
  (check-equal? (data-computed "tripled" "$count * 3") (list 'data-computed:tripled "$count * 3")))

(test-case "data-computed hash form returns list of pairs"
  (define result (data-computed (hash 'doubled "$count * 2" 'tripled "$count * 3")))
  (check-true (list? result))
  (check-equal? (length result) 2)
  (for ([pair (in-list result)])
    (check-true (list? pair))
    (check-equal? (length pair) 2)
    (check-true (string-prefix? (symbol->string (first pair)) "data-computed:"))))

(test-case "data-computed hash form values"
  (define result (data-computed (hash 'a "$x + 1" 'b "$y + 2")))
  (define result-hash
    (for/hash ([pair (in-list result)])
      (values (first pair) (second pair))))
  (check-equal? (hash-ref result-hash 'data-computed:a) "$x + 1")
  (check-equal? (hash-ref result-hash 'data-computed:b) "$y + 2"))

(test-case "data-computed hash form renders in x-expression"
  (define pairs (data-computed (hash 'a "$x + 1")))
  (check-equal? (xexpr->string `(div ,pairs "")) "<div data-computed:a=\"$x + 1\"></div>"))

;; hash-based attributes: data-attr ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-attr keyed form"
  (check-equal? (data-attr "aria-label" "$foo") (list 'data-attr:aria-label "$foo"))
  (check-equal? (xexpr->string `(button (,(data-attr "disabled" "$foo == ''")) "Save"))
                "<button data-attr:disabled=\"$foo == ''\">Save</button>"))

(test-case "data-attr keyed form with symbol"
  (check-equal? (data-attr 'disabled "$bar") (list 'data-attr:disabled "$bar")))

(test-case "data-attr hash form"
  (define result (data-attr (hash "aria-label" "$foo")))
  (check-equal? (first result) 'data-attr)
  (check-true (string-contains? (second result) "\"aria-label\": $foo"))
  (check-equal? (xexpr->string `(button (,(data-attr (hash "aria-label" "$foo"))) "Save"))
                "<button data-attr=\"{&quot;aria-label&quot;: $foo}\">Save</button>"))

(test-case "data-attr hash form multiple"
  (define result (data-attr (hash "aria-label" "$foo" "disabled" "$bar")))
  (check-equal? (first result) 'data-attr)
  (check-true (string-contains? (second result) "$foo"))
  (check-true (string-contains? (second result) "$bar")))

;; hash-based attributes: data-class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-class keyed form"
  (check-equal? (data-class "font-bold" "$foo == 'strong'")
                (list 'data-class:font-bold "$foo == 'strong'"))
  (check-equal? (xexpr->string `(button (,(data-class "font-bold" "$foo == 'strong'")) "Save"))
                "<button data-class:font-bold=\"$foo == 'strong'\">Save</button>"))

(test-case "data-class keyed form with symbol"
  (check-equal? (data-class 'hidden "$!visible") (list 'data-class:hidden "$!visible")))

(test-case "data-class hash form"
  (define result (data-class (hash "font-bold" "$foo == 'strong'")))
  (check-equal? (first result) 'data-class)
  (check-true (string-contains? (second result) "\"font-bold\": $foo == 'strong'"))
  (check-equal? (xexpr->string `(button (,(data-class (hash "active" "$isActive"))) "Save"))
                "<button data-class=\"{&quot;active&quot;: $isActive}\">Save</button>"))

;; hash-based attributes: data-style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-style keyed form"
  (check-equal? (data-style "display" "$hiding && 'none'")
                (list 'data-style:display "$hiding && 'none'"))
  (check-equal? (xexpr->string `(div (,(data-style "display" "$hiding && 'none'")) ""))
                "<div data-style:display=\"$hiding &amp;&amp; 'none'\"></div>"))

(test-case "data-style keyed form with symbol"
  (check-equal? (data-style 'background-color "$red ? 'red' : 'blue'")
                (list 'data-style:background-color "$red ? 'red' : 'blue'")))

(test-case "data-style hash form"
  (define result (data-style (hash "display" "$hiding ? 'none' : 'flex'")))
  (check-equal? (first result) 'data-style)
  (check-true (string-contains? (second result) "\"display\": $hiding ? 'none' : 'flex'"))
  (check-equal? (xexpr->string `(div (,(data-style (hash "display" "$visible ? 'block' : 'none'")))
                                     ""))
                "<div data-style=\"{&quot;display&quot;: $visible ? 'block' : 'none'}\"></div>"))

;; data-on -- basic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on basic click"
  (check-equal? (data-on "click" "$foo = ''") (list 'data-on:click "$foo = ''"))
  (check-equal? (xexpr->string `(button (,(data-on "click" "$foo = ''")) "Reset"))
                "<button data-on:click=\"$foo = ''\">Reset</button>"))

(test-case "data-on submit"
  (check-equal? (data-on "submit" "@post('/create')") (list 'data-on:submit "@post('/create')")))

(test-case "data-on custom event"
  (check-equal? (data-on "my-event" "$foo = evt.detail")
                (list 'data-on:my-event "$foo = evt.detail")))

(test-case "data-on with sse-post integration"
  (check-equal? (data-on "click" (sse-post "/todo/create"))
                (list 'data-on:click "@post('/todo/create')"))
  (check-equal? (xexpr->string `(button (,(data-on "click" (sse-post "/create"))) "Go"))
                "<button data-on:click=\"@post('/create')\">Go</button>"))

;; data-on -- individual modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on with once"
  (check-equal? (data-on "click" "fn()" #:once #t) (list 'data-on:click__once "fn()")))

(test-case "data-on with passive"
  (check-equal? (data-on "scroll" "fn()" #:passive #t) (list 'data-on:scroll__passive "fn()")))

(test-case "data-on with capture"
  (check-equal? (data-on "click" "fn()" #:capture #t) (list 'data-on:click__capture "fn()")))

(test-case "data-on with window"
  (check-equal? (data-on "keydown" "fn()" #:window #t) (list 'data-on:keydown__window "fn()")))

(test-case "data-on with outside"
  (check-equal? (data-on "click" "fn()" #:outside #t) (list 'data-on:click__outside "fn()")))

(test-case "data-on with prevent"
  (check-equal? (data-on "submit" "fn()" #:prevent #t) (list 'data-on:submit__prevent "fn()"))
  (check-equal? (xexpr->string `(form (,(data-on "submit" (sse-post "/create") #:prevent #t)) ""))
                "<form data-on:submit__prevent=\"@post('/create')\"></form>"))

(test-case "data-on with stop"
  (check-equal? (data-on "click" "fn()" #:stop #t) (list 'data-on:click__stop "fn()")))

(test-case "data-on with trust"
  (check-equal? (data-on "click" "fn()" #:trust #t) (list 'data-on:click__trust "fn()")))

;; data-on -- timing modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on with debounce string"
  (check-equal? (data-on "input" "fn()" #:debounce "300ms")
                (list (string->symbol "data-on:input__debounce.300ms") "fn()"))
  (check-equal? (xexpr->string `(input (,(data-on "input" (sse-post "/search") #:debounce "300ms"))))
                "<input data-on:input__debounce.300ms=\"@post('/search')\"/>"))

(test-case "data-on with debounce number"
  (check-equal? (data-on "input" "fn()" #:debounce 300)
                (list (string->symbol "data-on:input__debounce.300") "fn()")))

(test-case "data-on with debounce leading"
  (check-equal? (data-on "click" "fn()" #:debounce "500ms" #:debounce-leading #t)
                (list (string->symbol "data-on:click__debounce.500ms.leading") "fn()"))
  (check-equal?
   (xexpr->string `(button (,(data-on "click" "fn()" #:debounce "500ms" #:debounce-leading #t)) "Go"))
   "<button data-on:click__debounce.500ms.leading=\"fn()\">Go</button>"))

(test-case "data-on with debounce notrailing"
  (check-equal? (data-on "click" "fn()" #:debounce "500ms" #:debounce-notrailing #t)
                (list (string->symbol "data-on:click__debounce.500ms.notrailing") "fn()")))

(test-case "data-on with debounce leading and notrailing"
  (check-equal?
   (data-on "click" "fn()" #:debounce "500ms" #:debounce-leading #t #:debounce-notrailing #t)
   (list (string->symbol "data-on:click__debounce.500ms.leading.notrailing") "fn()")))

(test-case "data-on with throttle string"
  (check-equal? (data-on "scroll" "fn()" #:throttle "1s")
                (list (string->symbol "data-on:scroll__throttle.1s") "fn()"))
  (check-equal? (xexpr->string `(div (,(data-on "scroll" "fn()" #:throttle "1s")) ""))
                "<div data-on:scroll__throttle.1s=\"fn()\"></div>"))

(test-case "data-on with throttle number"
  (check-equal? (data-on "scroll" "fn()" #:throttle 500)
                (list (string->symbol "data-on:scroll__throttle.500") "fn()")))

(test-case "data-on with throttle noleading"
  (check-equal? (data-on "scroll" "fn()" #:throttle "500ms" #:throttle-noleading #t)
                (list (string->symbol "data-on:scroll__throttle.500ms.noleading") "fn()")))

(test-case "data-on with throttle trailing"
  (check-equal? (data-on "scroll" "fn()" #:throttle "500ms" #:throttle-trailing #t)
                (list (string->symbol "data-on:scroll__throttle.500ms.trailing") "fn()")))

(test-case "data-on with delay"
  (check-equal? (data-on "click" "fn()" #:delay "500ms")
                (list (string->symbol "data-on:click__delay.500ms") "fn()"))
  (check-equal? (xexpr->string `(button (,(data-on "click" "fn()" #:delay "500ms")) ""))
                "<button data-on:click__delay.500ms=\"fn()\"></button>"))

(test-case "data-on with delay number"
  (check-equal? (data-on "click" "fn()" #:delay 1000)
                (list (string->symbol "data-on:click__delay.1000") "fn()")))

(test-case "data-on with viewtransition"
  (check-equal? (data-on "click" "fn()" #:viewtransition #t)
                (list 'data-on:click__viewtransition "fn()"))
  (check-equal? (xexpr->string `(button (,(data-on "click" "fn()" #:viewtransition #t)) ""))
                "<button data-on:click__viewtransition=\"fn()\"></button>"))

;; data-on -- combined modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on with window + debounce"
  (check-equal? (data-on "click" "$foo = ''" #:window #t #:debounce "500ms")
                (list (string->symbol "data-on:click__window__debounce.500ms") "$foo = ''")))

(test-case "data-on with window + debounce + leading (matches Datastar docs example)"
  (check-equal? (data-on "click" "$foo = ''" #:window #t #:debounce "500ms" #:debounce-leading #t)
                (list (string->symbol "data-on:click__window__debounce.500ms.leading") "$foo = ''"))
  (check-equal?
   (xexpr->string
    `(button (,(data-on "click" "$foo = ''" #:window #t #:debounce "500ms" #:debounce-leading #t))
             ""))
   "<button data-on:click__window__debounce.500ms.leading=\"$foo = ''\"></button>"))

(test-case "data-on with debounce + stop + prevent"
  (check-equal? (data-on "input" "@post('/search')" #:debounce "300ms" #:prevent #t #:stop #t)
                (list (string->symbol "data-on:input__prevent__stop__debounce.300ms")
                      "@post('/search')"))
  (check-equal?
   (xexpr->string
    `(input (,(data-on "input" "@post('/search')" #:debounce "300ms" #:prevent #t #:stop #t))))
   "<input data-on:input__prevent__stop__debounce.300ms=\"@post('/search')\"/>"))

(test-case "data-on with all boolean modifiers"
  (define result
    (data-on "click"
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

;; data-init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-init basic"
  (check-equal? (data-init "$count = 1") '(data-init "$count = 1"))
  (check-equal? (xexpr->string `(div (,(data-init "@get('/events')")) ""))
                "<div data-init=\"@get('/events')\"></div>"))

(test-case "data-init with sse-get"
  (check-equal? (data-init (sse-get "/events")) '(data-init "@get('/events')")))

(test-case "data-init with delay"
  (check-equal? (data-init "$count = 1" #:delay "500ms")
                (list (string->symbol "data-init__delay.500ms") "$count = 1"))
  (check-equal? (xexpr->string `(div (,(data-init "$count = 1" #:delay "500ms")) ""))
                "<div data-init__delay.500ms=\"$count = 1\"></div>"))

(test-case "data-init with viewtransition"
  (check-equal? (data-init "fn()" #:viewtransition #t) '(data-init__viewtransition "fn()")))

(test-case "data-init with delay + viewtransition"
  (check-equal? (data-init "fn()" #:delay "500ms" #:viewtransition #t)
                (list (string->symbol "data-init__delay.500ms__viewtransition") "fn()")))

;; data-on-intersect ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on-intersect basic"
  (check-equal? (data-on-intersect "$intersected = true") '(data-on-intersect "$intersected = true")))

(test-case "data-on-intersect with once"
  (check-equal? (data-on-intersect "fn()" #:once #t) '(data-on-intersect__once "fn()")))

(test-case "data-on-intersect with half"
  (check-equal? (data-on-intersect "fn()" #:half #t) '(data-on-intersect__half "fn()")))

(test-case "data-on-intersect with full"
  (check-equal? (data-on-intersect "fn()" #:full #t) '(data-on-intersect__full "fn()")))

(test-case "data-on-intersect with exit"
  (check-equal? (data-on-intersect "fn()" #:exit #t) '(data-on-intersect__exit "fn()")))

(test-case "data-on-intersect with threshold number"
  (check-equal? (data-on-intersect "fn()" #:threshold 75)
                (list (string->symbol "data-on-intersect__threshold.75") "fn()"))
  (check-equal? (xexpr->string `(div (,(data-on-intersect "fn()" #:threshold 75)) ""))
                "<div data-on-intersect__threshold.75=\"fn()\"></div>"))

(test-case "data-on-intersect with threshold string"
  (check-equal? (data-on-intersect "fn()" #:threshold "25")
                (list (string->symbol "data-on-intersect__threshold.25") "fn()")))

(test-case "data-on-intersect with once + full (matches Datastar docs)"
  (check-equal? (data-on-intersect "$fullyIntersected = true" #:once #t #:full #t)
                '(data-on-intersect__once__full "$fullyIntersected = true"))
  (check-equal?
   (xexpr->string `(div (,(data-on-intersect "$fullyIntersected = true" #:once #t #:full #t)) ""))
   "<div data-on-intersect__once__full=\"$fullyIntersected = true\"></div>"))

(test-case "data-on-intersect with debounce"
  (check-equal? (data-on-intersect "fn()" #:debounce "500ms")
                (list (string->symbol "data-on-intersect__debounce.500ms") "fn()")))

(test-case "data-on-intersect with throttle"
  (check-equal? (data-on-intersect "fn()" #:throttle "1s")
                (list (string->symbol "data-on-intersect__throttle.1s") "fn()")))

(test-case "data-on-intersect with delay"
  (check-equal? (data-on-intersect "fn()" #:delay "500ms")
                (list (string->symbol "data-on-intersect__delay.500ms") "fn()")))

(test-case "data-on-intersect with viewtransition"
  (check-equal? (data-on-intersect "fn()" #:viewtransition #t)
                '(data-on-intersect__viewtransition "fn()")))

(test-case "data-on-intersect all modifiers combined"
  (define result
    (data-on-intersect "fn()" #:once #t #:half #t #:debounce "500ms" #:viewtransition #t))
  (define attr-name (symbol->string (first result)))
  (check-true (string-contains? attr-name "__once"))
  (check-true (string-contains? attr-name "__half"))
  (check-true (string-contains? attr-name "__debounce.500ms"))
  (check-true (string-contains? attr-name "__viewtransition")))

;; data-on-interval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on-interval basic"
  (check-equal? (data-on-interval "$count++") '(data-on-interval "$count++"))
  (check-equal? (xexpr->string `(div (,(data-on-interval "$count++")) ""))
                "<div data-on-interval=\"$count++\"></div>"))

(test-case "data-on-interval with duration string"
  (check-equal? (data-on-interval "$count++" #:duration "500ms")
                (list (string->symbol "data-on-interval__duration.500ms") "$count++"))
  (check-equal? (xexpr->string `(div (,(data-on-interval "$count++" #:duration "500ms")) ""))
                "<div data-on-interval__duration.500ms=\"$count++\"></div>"))

(test-case "data-on-interval with duration number"
  (check-equal? (data-on-interval "$count++" #:duration 2000)
                (list (string->symbol "data-on-interval__duration.2000") "$count++")))

(test-case "data-on-interval with duration + leading"
  (check-equal? (data-on-interval "$count++" #:duration "500ms" #:duration-leading #t)
                (list (string->symbol "data-on-interval__duration.500ms.leading") "$count++"))
  (check-equal?
   (xexpr->string `(div (,(data-on-interval "$count++" #:duration "500ms" #:duration-leading #t)) ""))
   "<div data-on-interval__duration.500ms.leading=\"$count++\"></div>"))

(test-case "data-on-interval with viewtransition"
  (check-equal? (data-on-interval "fn()" #:viewtransition #t)
                '(data-on-interval__viewtransition "fn()")))

(test-case "data-on-interval with duration + viewtransition"
  (check-equal? (data-on-interval "fn()" #:duration "1s" #:viewtransition #t)
                (list (string->symbol "data-on-interval__duration.1s__viewtransition") "fn()")))

;; data-on-signal-patch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on-signal-patch basic"
  (check-equal? (data-on-signal-patch "console.log('changed')")
                '(data-on-signal-patch "console.log('changed')")))

(test-case "data-on-signal-patch with debounce"
  (check-equal? (data-on-signal-patch "doSomething()" #:debounce "500ms")
                (list (string->symbol "data-on-signal-patch__debounce.500ms") "doSomething()"))
  (check-equal? (xexpr->string `(div (,(data-on-signal-patch "doSomething()" #:debounce "500ms")) ""))
                "<div data-on-signal-patch__debounce.500ms=\"doSomething()\"></div>"))

(test-case "data-on-signal-patch with throttle"
  (check-equal? (data-on-signal-patch "fn()" #:throttle "1s")
                (list (string->symbol "data-on-signal-patch__throttle.1s") "fn()")))

(test-case "data-on-signal-patch with delay"
  (check-equal? (data-on-signal-patch "fn()" #:delay "500ms")
                (list (string->symbol "data-on-signal-patch__delay.500ms") "fn()")))

(test-case "data-on-signal-patch with filter include"
  (define result (data-on-signal-patch "fn()" #:include "counter"))
  (check-true (list? (first result)))
  (check-equal? (first (first result)) 'data-on-signal-patch)
  (check-equal? (second (first result)) "fn()")
  (check-equal? (first (second result)) 'data-on-signal-patch-filter)
  (check-true (string-contains? (second (second result)) "\"include\""))
  (check-equal?
   (xexpr->string `(div ,result ""))
   "<div data-on-signal-patch=\"fn()\" data-on-signal-patch-filter=\"{&quot;include&quot;: &quot;counter&quot;}\"></div>"))

(test-case "data-on-signal-patch with filter exclude"
  (define result (data-on-signal-patch "fn()" #:exclude "temp"))
  (check-true (list? (first result)))
  (check-true (string-contains? (second (second result)) "\"exclude\"")))

(test-case "data-on-signal-patch with filter include + exclude"
  (define result (data-on-signal-patch "fn()" #:include "user" #:exclude "password"))
  (check-true (list? (first result)))
  (check-true (string-contains? (second (second result)) "\"include\""))
  (check-true (string-contains? (second (second result)) "\"exclude\"")))

(test-case "data-on-signal-patch with debounce + filter"
  (define result (data-on-signal-patch "fn()" #:debounce "500ms" #:include "counter"))
  (check-true (list? (first result)))
  (check-equal? (first (first result)) (string->symbol "data-on-signal-patch__debounce.500ms")))

;; data-on-raf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on-raf basic"
  (check-equal? (data-on-raf "$count++") '(data-on-raf "$count++")))

(test-case "data-on-raf with throttle"
  (check-equal? (data-on-raf "$count++" #:throttle "10ms")
                (list (string->symbol "data-on-raf__throttle.10ms") "$count++"))
  (check-equal? (xexpr->string `(div (,(data-on-raf "$count++" #:throttle "10ms")) ""))
                "<div data-on-raf__throttle.10ms=\"$count++\"></div>"))

(test-case "data-on-raf with throttle noleading"
  (check-equal? (data-on-raf "fn()" #:throttle "10ms" #:throttle-noleading #t)
                (list (string->symbol "data-on-raf__throttle.10ms.noleading") "fn()")))

;; data-on-resize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on-resize basic"
  (check-equal? (data-on-resize "$count++") '(data-on-resize "$count++")))

(test-case "data-on-resize with debounce"
  (check-equal? (data-on-resize "$count++" #:debounce "10ms")
                (list (string->symbol "data-on-resize__debounce.10ms") "$count++"))
  (check-equal? (xexpr->string `(div (,(data-on-resize "$count++" #:debounce "10ms")) ""))
                "<div data-on-resize__debounce.10ms=\"$count++\"></div>"))

(test-case "data-on-resize with throttle"
  (check-equal? (data-on-resize "fn()" #:throttle "100ms")
                (list (string->symbol "data-on-resize__throttle.100ms") "fn()")))

(test-case "data-on-resize with debounce leading"
  (check-equal? (data-on-resize "fn()" #:debounce "10ms" #:debounce-leading #t)
                (list (string->symbol "data-on-resize__debounce.10ms.leading") "fn()")))

;; data-ignore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-ignore basic"
  (check-equal? (data-ignore) '(data-ignore ""))
  (check-equal? (xexpr->string `(div (,(data-ignore)) "")) "<div data-ignore=\"\"></div>"))

(test-case "data-ignore with self"
  (check-equal? (data-ignore #:self #t) '(data-ignore__self ""))
  (check-equal? (xexpr->string `(div (,(data-ignore #:self #t)) ""))
                "<div data-ignore__self=\"\"></div>"))

(test-case "data-ignore self false is same as basic"
  (check-equal? (data-ignore #:self #f) '(data-ignore "")))

;; data-scroll-into-view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-scroll-into-view basic"
  (check-equal? (data-scroll-into-view) '(data-scroll-into-view ""))
  (check-equal? (xexpr->string `(div (,(data-scroll-into-view)) ""))
                "<div data-scroll-into-view=\"\"></div>"))

(test-case "data-scroll-into-view with smooth"
  (check-equal? (data-scroll-into-view #:smooth #t) '(data-scroll-into-view__smooth "")))

(test-case "data-scroll-into-view with instant"
  (check-equal? (data-scroll-into-view #:instant #t) '(data-scroll-into-view__instant "")))

(test-case "data-scroll-into-view with auto"
  (check-equal? (data-scroll-into-view #:auto #t) '(data-scroll-into-view__auto "")))

(test-case "data-scroll-into-view with hstart"
  (check-equal? (data-scroll-into-view #:hstart #t) '(data-scroll-into-view__hstart "")))

(test-case "data-scroll-into-view with hcenter"
  (check-equal? (data-scroll-into-view #:hcenter #t) '(data-scroll-into-view__hcenter "")))

(test-case "data-scroll-into-view with hend"
  (check-equal? (data-scroll-into-view #:hend #t) '(data-scroll-into-view__hend "")))

(test-case "data-scroll-into-view with hnearest"
  (check-equal? (data-scroll-into-view #:hnearest #t) '(data-scroll-into-view__hnearest "")))

(test-case "data-scroll-into-view with vstart"
  (check-equal? (data-scroll-into-view #:vstart #t) '(data-scroll-into-view__vstart "")))

(test-case "data-scroll-into-view with vcenter"
  (check-equal? (data-scroll-into-view #:vcenter #t) '(data-scroll-into-view__vcenter "")))

(test-case "data-scroll-into-view with vend"
  (check-equal? (data-scroll-into-view #:vend #t) '(data-scroll-into-view__vend "")))

(test-case "data-scroll-into-view with vnearest"
  (check-equal? (data-scroll-into-view #:vnearest #t) '(data-scroll-into-view__vnearest "")))

(test-case "data-scroll-into-view with focus"
  (check-equal? (data-scroll-into-view #:focus #t) '(data-scroll-into-view__focus "")))

(test-case "data-scroll-into-view with smooth + vend"
  (check-equal? (data-scroll-into-view #:smooth #t #:vend #t)
                '(data-scroll-into-view__smooth__vend ""))
  (check-equal? (xexpr->string `(div (,(data-scroll-into-view #:smooth #t #:vend #t)) ""))
                "<div data-scroll-into-view__smooth__vend=\"\"></div>"))

(test-case "data-scroll-into-view with smooth + hcenter + vcenter + focus"
  (define result (data-scroll-into-view #:smooth #t #:hcenter #t #:vcenter #t #:focus #t))
  (define attr-name (symbol->string (first result)))
  (check-true (string-contains? attr-name "__smooth"))
  (check-true (string-contains? attr-name "__hcenter"))
  (check-true (string-contains? attr-name "__vcenter"))
  (check-true (string-contains? attr-name "__focus")))

;; data-persist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-persist basic"
  (check-equal? (data-persist) '(data-persist ""))
  (check-equal? (xexpr->string `(div (,(data-persist)) "")) "<div data-persist=\"\"></div>"))

(test-case "data-persist with key"
  (check-equal? (data-persist #:key "mykey") (list 'data-persist:mykey ""))
  (check-equal? (xexpr->string `(div (,(data-persist #:key "mykey")) ""))
                "<div data-persist:mykey=\"\"></div>"))

(test-case "data-persist with session"
  (check-equal? (data-persist #:session #t) '(data-persist__session "")))

(test-case "data-persist with key + session"
  (check-equal? (data-persist #:key "mykey" #:session #t) (list 'data-persist:mykey__session ""))
  (check-equal? (xexpr->string `(div (,(data-persist #:key "mykey" #:session #t)) ""))
                "<div data-persist:mykey__session=\"\"></div>"))

(test-case "data-persist with include"
  (define result (data-persist #:include "foo"))
  (check-equal? (first result) 'data-persist)
  (check-true (string-contains? (second result) "\"include\"")))

(test-case "data-persist with exclude"
  (define result (data-persist #:exclude "bar"))
  (check-true (string-contains? (second result) "\"exclude\"")))

(test-case "data-persist with include + exclude"
  (define result (data-persist #:include "foo" #:exclude "bar"))
  (check-true (string-contains? (second result) "\"include\""))
  (check-true (string-contains? (second result) "\"exclude\""))
  (check-equal?
   (xexpr->string `(div (,(data-persist #:include "foo" #:exclude "bar")) ""))
   "<div data-persist=\"{&quot;include&quot;: &quot;foo&quot;, &quot;exclude&quot;: &quot;bar&quot;}\"></div>"))

;; data-query-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-query-string basic"
  (check-equal? (data-query-string) '(data-query-string ""))
  (check-equal? (xexpr->string `(div (,(data-query-string)) ""))
                "<div data-query-string=\"\"></div>"))

(test-case "data-query-string with history"
  (check-equal? (data-query-string #:history #t) '(data-query-string__history "")))

(test-case "data-query-string with filter"
  (check-equal? (data-query-string #:filter #t) '(data-query-string__filter "")))

(test-case "data-query-string with filter + history"
  (check-equal? (data-query-string #:filter #t #:history #t) '(data-query-string__filter__history ""))
  (check-equal? (xexpr->string `(div (,(data-query-string #:filter #t #:history #t)) ""))
                "<div data-query-string__filter__history=\"\"></div>"))

(test-case "data-query-string with include"
  (define result (data-query-string #:include "foo"))
  (check-true (string-contains? (second result) "\"include\"")))

(test-case "data-query-string with include + exclude + history"
  (define result (data-query-string #:include "foo" #:exclude "bar" #:history #t))
  (check-equal? (first result) 'data-query-string__history)
  (check-true (string-contains? (second result) "\"include\""))
  (check-true (string-contains? (second result) "\"exclude\"")))

;; data-json-signals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-json-signals basic"
  (check-equal? (data-json-signals) '(data-json-signals ""))
  (check-equal? (xexpr->string `(pre (,(data-json-signals)) ""))
                "<pre data-json-signals=\"\"></pre>"))

(test-case "data-json-signals with terse"
  (check-equal? (data-json-signals #:terse #t) '(data-json-signals__terse ""))
  (check-equal? (xexpr->string `(pre (,(data-json-signals #:terse #t)) ""))
                "<pre data-json-signals__terse=\"\"></pre>"))

(test-case "data-json-signals with include"
  (define result (data-json-signals #:include "counter"))
  (check-true (string-contains? (second result) "\"include\"")))

(test-case "data-json-signals with include + terse"
  (define result (data-json-signals #:include "counter" #:terse #t))
  (check-equal? (first result) 'data-json-signals__terse)
  (check-true (string-contains? (second result) "\"include\""))
  (check-equal?
   (xexpr->string `(pre (,(data-json-signals #:include "counter" #:terse #t)) ""))
   "<pre data-json-signals__terse=\"{&quot;include&quot;: &quot;counter&quot;}\"></pre>"))

;; data-preserve-attrs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-preserve-attrs single"
  (check-equal? (data-preserve-attrs "open") '(data-preserve-attr "open"))
  (check-equal? (xexpr->string `(details (,(data-preserve-attrs "open") (open "")) "Content"))
                "<details data-preserve-attr=\"open\" open=\"\">Content</details>"))

(test-case "data-preserve-attrs multiple"
  (check-equal? (data-preserve-attrs '("open" "class")) '(data-preserve-attr "open class"))
  (check-equal? (xexpr->string `(details (,(data-preserve-attrs '("open" "class")) (open ""))
                                         "Content"))
                "<details data-preserve-attr=\"open class\" open=\"\">Content</details>"))

(test-case "data-preserve-attrs single in list"
  (check-equal? (data-preserve-attrs '("open")) '(data-preserve-attr "open")))

;; x-expression integration -- usage patterns ;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "attribute in x-expression template"
  (define attr (data-on "click" "$count++"))
  (check-equal? (length attr) 2)
  (check-true (symbol? (first attr)))
  (check-true (string? (second attr))))

(test-case "multiple attributes in x-expression"
  (define attrs (list (data-bind "filter") (data-indicator "filtering")))
  (check-equal? (length attrs) 2)
  (check-equal? (first (first attrs)) 'data-bind:filter)
  (check-equal? (first (second attrs)) 'data-indicator:filtering))

(test-case "init with sse-get in x-expression"
  (define attr (data-init (sse-get "/events")))
  (check-equal? attr '(data-init "@get('/events')")))

(test-case "on with sse-post and format in x-expression"
  (define tid 42)
  (define attr (data-on "click" (sse-post (format "/todo/delete/~a" tid))))
  (check-equal? attr (list 'data-on:click "@post('/todo/delete/42')")))

(test-case "complex form with debounce pattern"
  (define input-attr (data-on "input" (sse-post "/search") #:debounce "250ms"))
  (define bind-attr (data-bind "filter"))
  (define indicator-attr (data-indicator "filtering"))
  (check-equal? input-attr (list (string->symbol "data-on:input__debounce.250ms") "@post('/search')"))
  (check-equal? bind-attr (list 'data-bind:filter ""))
  (check-equal? indicator-attr (list 'data-indicator:filtering "")))

(test-case "xexpr->string round-trip"
  (check-equal? (xexpr->string `(div (,(data-show "$visible") (id "test")) "hello"))
                "<div data-show=\"$visible\" id=\"test\">hello</div>"))

;; x-expression integration -- realistic templates ;;;;;;;;;;;;;;;;;;;;;

(test-case "search input pattern renders correct HTML"
  (check-equal?
   (xexpr->string `(input (,(data-on "input" (sse-post "/search") #:debounce "250ms")
                           ,(data-bind "filter")
                           ,(data-indicator "filtering"))))
   "<input data-on:input__debounce.250ms=\"@post('/search')\" data-bind:filter=\"\" data-indicator:filtering=\"\"/>"))

(test-case "form with sse-post renders correct HTML"
  (check-equal?
   (xexpr->string `(form (,(data-on "submit" (sse-post "/todo/create") #:prevent #t))
                         (input ((type "text") ,(data-bind "newTodo")))
                         (button ((type "submit")) "Add")))
   "<form data-on:submit__prevent=\"@post('/todo/create')\"><input type=\"text\" data-bind:newTodo=\"\"/><button type=\"submit\">Add</button></form>"))

(test-case "page with init + signals renders correct HTML"
  (check-equal?
   (xexpr->string `(body (,(data-init (sse-get "/updates")) ,(data-signals (hash 'count 0)))
                         (span (,(data-text "$count")) "")))
   "<body data-init=\"@get('/updates')\" data-signals=\"{&quot;count&quot;:0}\"><span data-text=\"$count\"></span></body>"))

;; edge cases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "data-on with no modifiers returns clean attribute"
  (define attr (data-on "click" "fn()"))
  (check-false (string-contains? (symbol->string (first attr)) "__")
               "no modifiers should mean no __ in attribute name"))

(test-case "data-on-intersect with no modifiers returns clean attribute"
  (define attr (data-on-intersect "fn()"))
  (check-false (string-contains? (symbol->string (first attr)) "__")))

(test-case "data-on-interval with no modifiers returns clean attribute"
  (define attr (data-on-interval "fn()"))
  (check-false (string-contains? (symbol->string (first attr)) "__")))

(test-case "data-signals with empty hash"
  (check-equal? (data-signals (hash)) '(data-signals "{}")))

(test-case "data-computed single entry hash returns list with one pair"
  (define result (data-computed (hash 'x "$y")))
  (check-true (list? result))
  (check-equal? (length result) 1)
  (check-equal? (first result) (list 'data-computed:x "$y")))

(test-case "data-on-signal-patch without filter returns single pair"
  (define result (data-on-signal-patch "fn()"))
  (check-equal? (length result) 2)
  (check-true (symbol? (first result))))

(test-case "data-on-signal-patch with filter returns list of pairs"
  (define result (data-on-signal-patch "fn()" #:include "x"))
  (check-true (list? (first result)))
  (check-equal? (length result) 2))

(test-case "data-scroll-into-view with no modifiers"
  (define attr (data-scroll-into-view))
  (check-false (string-contains? (symbol->string (first attr)) "__")))

(test-case "data-persist with no options"
  (define attr (data-persist))
  (check-false (string-contains? (symbol->string (first attr)) "__"))
  (check-false (string-contains? (symbol->string (first attr)) ":")))

(test-case "data-signals error on invalid input"
  (check-exn exn:fail? (lambda () (data-signals 42))))

(test-case "data-computed error on invalid input"
  (check-exn exn:fail? (lambda () (data-computed 42))))

(test-case "data-attr error on invalid input"
  (check-exn exn:fail? (lambda () (data-attr 42))))

(test-case "data-class error on invalid input"
  (check-exn exn:fail? (lambda () (data-class 42))))

(test-case "data-style error on invalid input"
  (check-exn exn:fail? (lambda () (data-style 42))))
