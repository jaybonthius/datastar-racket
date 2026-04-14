#lang racket

(require datastar/sugar
         rackunit
         xml)

(provide sugar-tests)

(define actions-tests
  (test-suite "actions"

    ;; backend actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "get basic"
      (check-equal? (get "/events") "@get('/events')"))

    (test-case "post basic"
      (check-equal? (post "/submit") "@post('/submit')"))

    (test-case "patch basic"
      (check-equal? (patch "/data") "@patch('/data')"))

    (test-case "delete basic"
      (check-equal? (delete "/item") "@delete('/item')"))

    (test-case "put basic"
      (check-equal? (put "/update") "@put('/update')"))

    (test-case "post with formatted url"
      (check-equal? (post (format "/todo/delete/~a" 42)) "@post('/todo/delete/42')"))

    ;; content-type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "content-type form"
      (check-equal? (post "/submit" #:content-type 'form) "@post('/submit', {contentType: 'form'})"))

    (test-case "content-type json"
      (check-equal? (post "/submit" #:content-type 'json) "@post('/submit', {contentType: 'json'})"))

    ;; filter-signals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "filter-signals include only"
      (check-equal? (post "/submit" #:filter-signals-include "/^foo\\./")
                    "@post('/submit', {filterSignals: {include: /^foo\\./}})"))

    (test-case "filter-signals exclude only"
      (check-equal? (post "/submit" #:filter-signals-exclude "/^_/")
                    "@post('/submit', {filterSignals: {exclude: /^_/}})"))

    (test-case "filter-signals include and exclude"
      (check-equal?
       (post "/submit" #:filter-signals-include "/^foo\\./" #:filter-signals-exclude "/^_/")
       "@post('/submit', {filterSignals: {include: /^foo\\./, exclude: /^_/}})"))

    ;; selector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "selector"
      (check-equal? (post "/submit" #:content-type 'form #:selector "#myForm")
                    "@post('/submit', {contentType: 'form', selector: \"#myForm\"})"))

    ;; headers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "headers"
      (check-equal? (post "/api" #:headers (hash "X-Token" "abc123"))
                    "@post('/api', {headers: {\"X-Token\": \"abc123\"}})"))

    ;; open-when-hidden ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "open-when-hidden true"
      (check-equal? (get "/events" #:open-when-hidden? #t) "@get('/events', {openWhenHidden: true})"))

    (test-case "open-when-hidden false"
      (check-equal? (post "/submit" #:open-when-hidden? #f)
                    "@post('/submit', {openWhenHidden: false})"))

    (test-case "open-when-hidden unset (not included)"
      (check-equal? (get "/events" #:retry 'never) "@get('/events', {retry: 'never'})"))

    ;; payload ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "payload"
      (check-equal? (post "/submit" #:payload "{custom: 'data'}")
                    "@post('/submit', {payload: {custom: 'data'}})"))

    ;; retry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "retry never"
      (check-equal? (get "/events" #:retry 'never) "@get('/events', {retry: 'never'})"))

    (test-case "retry error"
      (check-equal? (get "/events" #:retry 'error) "@get('/events', {retry: 'error'})"))

    (test-case "retry always"
      (check-equal? (get "/events" #:retry 'always) "@get('/events', {retry: 'always'})"))

    (test-case "retry auto"
      (check-equal? (get "/events" #:retry 'auto) "@get('/events', {retry: 'auto'})"))

    ;; retry-interval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "retry-interval"
      (check-equal? (get "/events" #:retry-interval 5000) "@get('/events', {retryInterval: 5000})"))

    ;; retry-scaler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "retry-scaler integer"
      (check-equal? (get "/events" #:retry-scaler 3) "@get('/events', {retryScaler: 3})"))

    (test-case "retry-scaler fractional"
      (check-equal? (get "/events" #:retry-scaler 1.5) "@get('/events', {retryScaler: 1.5})"))

    ;; retry-max-wait-ms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "retry-max-wait-ms"
      (check-equal? (get "/events" #:retry-max-wait-ms 60000)
                    "@get('/events', {retryMaxWaitMs: 60000})"))

    ;; retry-max-count ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "retry-max-count"
      (check-equal? (get "/events" #:retry-max-count 5) "@get('/events', {retryMaxCount: 5})"))

    ;; request-cancellation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "request-cancellation disabled"
      (check-equal? (get "/events" #:request-cancellation 'disabled)
                    "@get('/events', {requestCancellation: 'disabled'})"))

    (test-case "request-cancellation cleanup"
      (check-equal? (get "/events" #:request-cancellation 'cleanup)
                    "@get('/events', {requestCancellation: 'cleanup'})"))

    (test-case "request-cancellation abortcontroller expression"
      (check-equal? (get "/events" #:request-cancellation "$controller")
                    "@get('/events', {requestCancellation: $controller})"))

    ;; combined keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "multiple options combined"
      (define result (get "/data" #:retry 'never #:open-when-hidden? #t #:retry-interval 5000))
      (check-regexp-match #rx"@get\\('/data', \\{" result)
      (check-regexp-match #rx"openWhenHidden: true" result)
      (check-regexp-match #rx"retry: 'never'" result)
      (check-regexp-match #rx"retryInterval: 5000" result))

    (test-case "full example from datastar docs"
      (define result
        (get "/endpoint"
             #:filter-signals-include "/^foo\\./"
             #:headers (hash "X-Csrf-Token" "JImikTbsoCYQ9oGOcvugov0Awc5LbqFsZW6ObRCxuq")
             #:open-when-hidden? #t
             #:request-cancellation 'disabled))
      (check-regexp-match #rx"@get\\('/endpoint', \\{" result)
      (check-regexp-match #rx"filterSignals: \\{include: /\\^foo\\\\./\\}" result)
      (check-regexp-match #rx"X-Csrf-Token" result)
      (check-regexp-match #rx"openWhenHidden: true" result)
      (check-regexp-match #rx"requestCancellation: 'disabled'" result))

    ;; all methods work with kwargs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "put with kwargs"
      (check-equal? (put "/update" #:content-type 'form) "@put('/update', {contentType: 'form'})"))

    (test-case "patch with kwargs"
      (check-equal? (patch "/data" #:retry 'never) "@patch('/data', {retry: 'never'})"))

    (test-case "delete with kwargs"
      (check-equal? (delete "/item" #:request-cancellation 'disabled)
                    "@delete('/item', {requestCancellation: 'disabled'})"))

    (test-case "get rejects invalid arguments"
      (check-exn exn:fail:contract? (lambda () (get 123)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:content-type 'xml)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:filter-signals-include 123)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:filter-signals-exclude 123)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:selector 123)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:headers '("X" "Y"))))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:open-when-hidden? 'yes)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:open-when-hidden? 'unset)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:payload 123)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:retry 'sometimes)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:retry-interval -1)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:retry-scaler "2")))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:retry-max-wait-ms -1)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:retry-max-count -1)))
      (check-exn exn:fail:contract? (lambda () (get "/events" #:request-cancellation 123))))

    (test-case "other backend methods enforce url contract"
      (check-exn exn:fail:contract? (lambda () (post 123)))
      (check-exn exn:fail:contract? (lambda () (put 123)))
      (check-exn exn:fail:contract? (lambda () (patch 123)))
      (check-exn exn:fail:contract? (lambda () (delete 123))))

    ;; peek ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "peek basic"
      (check-equal? (peek "$bar") "@peek($bar)"))

    (test-case "peek arrow function"
      (check-equal? (peek "() => $bar") "@peek(() => $bar)"))

    (test-case "peek rejects non-string callable"
      (check-exn exn:fail:contract? (lambda () (peek 123))))

    ;; set-all ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "set-all value only"
      (check-equal? (set-all "''") "@setAll('')"))

    (test-case "set-all with include filter"
      (check-equal? (set-all "''" #:include "/^form\\./") "@setAll('', {include: /^form\\./})"))

    (test-case "set-all with include and exclude"
      (check-equal? (set-all "0" #:include "/.*/" #:exclude "/^_/")
                    "@setAll(0, {include: /.*/, exclude: /^_/})"))

    (test-case "set-all boolean value"
      (check-equal? (set-all "false") "@setAll(false)"))

    (test-case "set-all rejects invalid arguments"
      (check-exn exn:fail:contract? (lambda () (set-all 123)))
      (check-exn exn:fail:contract? (lambda () (set-all "x" #:include 123)))
      (check-exn exn:fail:contract? (lambda () (set-all "x" #:exclude 123))))

    ;; toggle-all ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "toggle-all no filter"
      (check-equal? (toggle-all) "@toggleAll()"))

    (test-case "toggle-all with include"
      (check-equal? (toggle-all #:include "/^menu\\./") "@toggleAll({include: /^menu\\./})"))

    (test-case "toggle-all with include and exclude"
      (check-equal? (toggle-all #:include "/.*/" #:exclude "/^_/")
                    "@toggleAll({include: /.*/, exclude: /^_/})"))

    (test-case "toggle-all rejects invalid arguments"
      (check-exn exn:fail:contract? (lambda () (toggle-all #:include 123)))
      (check-exn exn:fail:contract? (lambda () (toggle-all #:exclude 123))))

    ;; expression chaining ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "chain with one expression is identity"
      (check-equal? (chain "$count = $count + 1") "$count = $count + 1"))

    (test-case "chain with two expressions uses semicolon separator"
      (check-equal? (chain "$count = $count + 1" "$status = 'done'")
                    "$count = $count + 1; $status = 'done'"))

    (test-case "chain preserves order with 3+ expressions"
      (check-equal? (chain "$a = 1" "$b = 2" "$c = 3") "$a = 1; $b = 2; $c = 3"))

    (test-case "chain/and with one expression is identity"
      (check-equal? (chain/and "$isValid") "$isValid"))

    (test-case "chain/and with two expressions uses logical-and separator"
      (check-equal? (chain/and "$isValid" "$isReady") "$isValid && $isReady"))

    (test-case "chain/and preserves order with 3+ expressions"
      (check-equal? (chain/and "$a" "$b" "$c") "$a && $b && $c"))

    (test-case "nested chain composition"
      (check-equal? (chain "$saving = true" (chain/and "$isValid" "$isReady") (post "/save"))
                    "$saving = true; $isValid && $isReady; @post('/save')"))

    (test-case "chain composes with existing action helpers"
      (check-equal? (chain "$saving = true" (post "/save") (set-all "false"))
                    "$saving = true; @post('/save'); @setAll(false)"))

    (test-case "chain helpers require at least one part"
      (check-exn exn:fail:contract? (lambda () (chain)))
      (check-exn exn:fail:contract? (lambda () (chain/and))))

    (test-case "chain helpers reject non-string parts"
      (check-exn exn:fail:contract? (lambda () (chain "$ok" 123)))
      (check-exn exn:fail:contract? (lambda () (chain "$ok" #f)))
      (check-exn exn:fail:contract? (lambda () (chain/and "$ok" 123))))

    ;; clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "clipboard basic"
      (check-equal? (clipboard "'Hello'") "@clipboard('Hello')"))

    (test-case "clipboard with base64 true"
      (check-equal? (clipboard "'SGVsbG8='" #:base64? #t) "@clipboard('SGVsbG8=', true)"))

    (test-case "clipboard with base64 false"
      (check-equal? (clipboard "'text'" #:base64? #f) "@clipboard('text', false)"))

    (test-case "clipboard rejects invalid arguments"
      (check-exn exn:fail:contract? (lambda () (clipboard 123)))
      (check-exn exn:fail:contract? (lambda () (clipboard "'x'" #:base64? 'yes)))
      (check-exn exn:fail:contract? (lambda () (clipboard "'x'" #:base64? 'unset))))

    ;; fit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "fit basic"
      (check-equal? (fit "$val" 0 100 0 1) "@fit($val, 0, 100, 0, 1)"))

    (test-case "fit with clamp"
      (check-equal? (fit "$x" 0 255 0 1 #:clamp? #t) "@fit($x, 0, 255, 0, 1, true)"))

    (test-case "fit with clamp and round"
      (check-equal? (fit "$x" 0 100 0 10 #:clamp? #t #:round? #t)
                    "@fit($x, 0, 100, 0, 10, true, true)"))

    (test-case "fit round only (clamp false explicit)"
      (check-equal? (fit "$x" 0 100 0 10 #:clamp? #f #:round? #t)
                    "@fit($x, 0, 100, 0, 10, false, true)"))

    (test-case "fit round only uses datastar default clamp false"
      (check-equal? (fit "$x" 0 100 0 10 #:round? #t) "@fit($x, 0, 100, 0, 10, false, true)"))

    (test-case "fit rejects invalid arguments"
      (check-exn exn:fail:contract? (lambda () (fit 1 0 100 0 10)))
      (check-exn exn:fail:contract? (lambda () (fit "$x" "0" 100 0 10)))
      (check-exn exn:fail:contract? (lambda () (fit "$x" 0 100 0 10 #:clamp? 'yes)))
      (check-exn exn:fail:contract? (lambda () (fit "$x" 0 100 0 10 #:clamp? 'unset)))
      (check-exn exn:fail:contract? (lambda () (fit "$x" 0 100 0 10 #:round? 'yes)))
      (check-exn exn:fail:contract? (lambda () (fit "$x" 0 100 0 10 #:round? 'unset))))

    ;; intl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "intl basic"
      (check-equal? (intl "'number'" "$price") "@intl('number', $price)"))

    (test-case "intl with options"
      (define result (intl "'number'" "$price" #:options (hash 'style "currency" 'currency "USD")))
      (check-regexp-match #rx"@intl\\('number', \\$price, \\{" result)
      (check-regexp-match #rx"\"style\": \"currency\"" result)
      (check-regexp-match #rx"\"currency\": \"USD\"" result))

    (test-case "intl with options and locale"
      (define result
        (intl "'datetime'" "$date" #:options (hash 'dateStyle "full") #:locale "'en-US'"))
      (check-regexp-match #rx"@intl\\('datetime', \\$date, \\{" result)
      (check-regexp-match #rx"\"dateStyle\": \"full\"" result)
      (check-regexp-match #rx"'en-US'\\)" result))

    (test-case "intl locale only"
      (check-equal? (intl "'number'" "$val" #:locale "'de-DE'") "@intl('number', $val, {}, 'de-DE')"))

    (test-case "intl rejects invalid arguments"
      (check-exn exn:fail:contract? (lambda () (intl 123 "$val")))
      (check-exn exn:fail:contract? (lambda () (intl "'number'" 123)))
      (check-exn exn:fail:contract? (lambda () (intl "'number'" "$val" #:options "{}")))
      (check-exn exn:fail:contract? (lambda () (intl "'number'" "$val" #:locale 123))))))

(define attributes-tests
  (test-suite "attributes"
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
      (check-equal? (xexpr->string `(span (,(data-text "$foo")) ""))
                    "<span data-text=\"$foo\"></span>"))

    (test-case "data-text with expression"
      (check-equal? (data-text "$firstName + ' ' + $lastName")
                    '(data-text "$firstName + ' ' + $lastName")))

    (test-case "data-effect basic"
      (check-equal? (data-effect "$foo = $bar + $baz") '(data-effect "$foo = $bar + $baz"))
      (check-equal? (xexpr->string `(div (,(data-effect "$foo = $bar + $baz")) ""))
                    "<div data-effect=\"$foo = $bar + $baz\"></div>"))

    (test-case "data-animate basic"
      (check-equal? (data-animate "$x") '(data-animate "$x"))
      (check-equal? (xexpr->string `(div (,(data-animate "$x")) ""))
                    "<div data-animate=\"$x\"></div>"))

    (test-case "data-bind basic"
      (check-equal? (data-bind "foo") (list 'data-bind:foo ""))
      (check-equal? (xexpr->string `(input (,(data-bind "foo")))) "<input data-bind:foo=\"\"/>"))

    (test-case "data-bind rejects #:as keyword"
      (check-exn exn:fail? (lambda () (data-bind "foo" #:as 'value))))

    (test-case "data-bind case modifier"
      (check-equal? (data-bind "foo-bar" #:case 'kebab)
                    (list (string->symbol "data-bind:foo-bar__case.kebab") "")))

    (test-case "data-bind with positional value no longer supported"
      (check-exn exn:fail? (lambda () (data-bind "foo" "fooBar"))))

    (test-case "data-bind with hyphenated signal"
      (check-equal? (data-bind "foo-bar") (list 'data-bind:foo-bar "")))

    (test-case "data-ref basic"
      (check-equal? (data-ref "foo") (list 'data-ref:foo ""))
      (check-equal? (xexpr->string `(div (,(data-ref "myEl")) "")) "<div data-ref:myEl=\"\"></div>"))

    (test-case "data-ref rejects #:as keyword"
      (check-exn exn:fail? (lambda () (data-ref "foo" #:as 'value))))

    (test-case "data-ref case modifier"
      (check-equal? (data-ref "my-signal" #:case 'snake)
                    (list (string->symbol "data-ref:my-signal__case.snake") "")))

    (test-case "data-ref with hyphenated name"
      (check-equal? (data-ref "my-element") (list 'data-ref:my-element "")))

    (test-case "data-indicator basic"
      (check-equal? (data-indicator "fetching") (list 'data-indicator:fetching ""))
      (check-equal? (xexpr->string `(button (,(data-indicator "fetching")) "Go"))
                    "<button data-indicator:fetching=\"\">Go</button>"))

    (test-case "data-indicator rejects #:as keyword"
      (check-exn exn:fail? (lambda () (data-indicator "fetching" #:as 'value))))

    (test-case "data-indicator case modifier"
      (check-equal? (data-indicator "is-loading" #:case 'camel)
                    (list (string->symbol "data-indicator:is-loading__case.camel") "")))

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
      (check-equal?
       (xexpr->string `(div (,(data-match-media "is-dark" "'prefers-color-scheme: dark'")) ""))
       "<div data-match-media:is-dark=\"'prefers-color-scheme: dark'\"></div>"))

    ;; keyed + hash helpers: data-signals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-signals/hash basic"
      (check-equal? (data-signals/hash (hash 'count 0)) '(data-signals "{\"count\":0}"))
      (check-equal? (xexpr->string `(div (,(data-signals/hash (hash 'count 0))) ""))
                    "<div data-signals=\"{&quot;count&quot;:0}\"></div>"))

    (test-case "data-signals/hash with multiple values"
      (define result (data-signals/hash (hash 'count 0 'name "Alice")))
      (check-equal? (first result) 'data-signals)
      ;; JSON key order may vary
      (check-true (string-contains? (second result) "\"count\":0"))
      (check-true (string-contains? (second result) "\"name\":\"Alice\"")))

    (test-case "data-signals/hash with nested values"
      (define result (data-signals/hash (hash 'user (hash 'name "Jay" 'age 30))))
      (check-equal? (first result) 'data-signals)
      (check-true (string-contains? (second result) "\"user\":")))

    (test-case "data-signals/hash with ifmissing"
      (define result (data-signals/hash (hash 'count 0) #:ifmissing? #t))
      (check-equal? (first result) 'data-signals__ifmissing)
      (check-true (string-contains? (second result) "\"count\":0"))
      (check-equal? (xexpr->string `(div (,(data-signals/hash (hash 'count 0) #:ifmissing? #t)) ""))
                    "<div data-signals__ifmissing=\"{&quot;count&quot;:0}\"></div>"))

    (test-case "data-signals keyed form with symbol"
      (check-equal? (data-signals 'foo "1") (list 'data-signals:foo "1"))
      (check-equal? (xexpr->string `(div (,(data-signals 'count "0")) ""))
                    "<div data-signals:count=\"0\"></div>"))

    (test-case "data-signals keyed form with string"
      (check-equal? (data-signals "foo" "1") (list 'data-signals:foo "1")))

    (test-case "data-signals keyed form with ifmissing"
      (check-equal? (data-signals 'foo "1" #:ifmissing? #t) (list 'data-signals:foo__ifmissing "1"))
      (check-equal? (xexpr->string `(div (,(data-signals 'foo "1" #:ifmissing? #t)) ""))
                    "<div data-signals:foo__ifmissing=\"1\"></div>"))

    (test-case "data-signals keyed form with dot-notation"
      (check-equal? (data-signals "foo.bar" "1") (list (string->symbol "data-signals:foo.bar") "1")))

    (test-case "data-signals keyed form with case modifier"
      (check-equal? (data-signals "foo-bar" "1" #:case 'snake)
                    (list (string->symbol "data-signals:foo-bar__case.snake") "1")))

    (test-case "data-signals/hash with null (signal removal)"
      (define result (data-signals/hash (hash 'foo 'null)))
      (check-equal? (first result) 'data-signals)
      (check-true (string-contains? (second result) "null")))

    (test-case "data-signals/hash with boolean"
      (define result (data-signals/hash (hash 'active #t)))
      (check-equal? (first result) 'data-signals)
      (check-true (string-contains? (second result) "true")))

    (test-case "data-signals/hash with array"
      (define result (data-signals/hash (hash 'items '(1 2 3))))
      (check-equal? (first result) 'data-signals)
      (check-true (string-contains? (second result) "[1,2,3]")))

    ;; keyed + hash helpers: data-computed ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-computed keyed form with symbol"
      (check-equal? (data-computed 'doubled "$count * 2") (list 'data-computed:doubled "$count * 2"))
      (check-equal? (xexpr->string `(div (,(data-computed 'doubled "$count * 2")) ""))
                    "<div data-computed:doubled=\"$count * 2\"></div>"))

    (test-case "data-computed keyed form with string"
      (check-equal? (data-computed "tripled" "$count * 3")
                    (list 'data-computed:tripled "$count * 3")))

    (test-case "data-computed keyed form with case modifier"
      (check-equal? (data-computed "foo-bar" "$count" #:case 'camel)
                    (list (string->symbol "data-computed:foo-bar__case.camel") "$count")))

    (test-case "data-computed/hash value form"
      (define result
        (data-computed/hash (hash 'doubled "() => $count * 2" 'tripled "() => $count * 3")))
      (check-equal? (first result) 'data-computed)
      (check-true (string-contains? (second result) "\"doubled\": () => $count * 2"))
      (check-true (string-contains? (second result) "\"tripled\": () => $count * 3")))

    (test-case "data-computed/hash renders in x-expression"
      (check-equal? (xexpr->string `(div (,(data-computed/hash (hash 'a "() => $x + 1"))) ""))
                    "<div data-computed=\"{&quot;a&quot;: () =&gt; $x + 1}\"></div>"))

    ;; keyed + hash helpers: data-attr ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-attr keyed form"
      (check-equal? (data-attr "aria-label" "$foo") (list 'data-attr:aria-label "$foo"))
      (check-equal? (xexpr->string `(button (,(data-attr "disabled" "$foo == ''")) "Save"))
                    "<button data-attr:disabled=\"$foo == ''\">Save</button>"))

    (test-case "data-attr keyed form with symbol"
      (check-equal? (data-attr 'disabled "$bar") (list 'data-attr:disabled "$bar")))

    (test-case "data-attr/hash"
      (define result (data-attr/hash (hash "aria-label" "$foo")))
      (check-equal? (first result) 'data-attr)
      (check-true (string-contains? (second result) "\"aria-label\": $foo"))
      (check-equal? (xexpr->string `(button (,(data-attr/hash (hash "aria-label" "$foo"))) "Save"))
                    "<button data-attr=\"{&quot;aria-label&quot;: $foo}\">Save</button>"))

    (test-case "data-attr/hash multiple"
      (define result (data-attr/hash (hash "aria-label" "$foo" "disabled" "$bar")))
      (check-equal? (first result) 'data-attr)
      (check-true (string-contains? (second result) "$foo"))
      (check-true (string-contains? (second result) "$bar")))

    ;; keyed + hash helpers: data-class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-class keyed form"
      (check-equal? (data-class "font-bold" "$foo == 'strong'")
                    (list 'data-class:font-bold "$foo == 'strong'"))
      (check-equal? (xexpr->string `(button (,(data-class "font-bold" "$foo == 'strong'")) "Save"))
                    "<button data-class:font-bold=\"$foo == 'strong'\">Save</button>"))

    (test-case "data-class keyed form with symbol"
      (check-equal? (data-class 'hidden "$!visible") (list 'data-class:hidden "$!visible")))

    (test-case "data-class keyed form with case modifier"
      (check-equal? (data-class "my-class" "$x" #:case 'camel)
                    (list (string->symbol "data-class:my-class__case.camel") "$x")))

    (test-case "data-class/hash"
      (define result (data-class/hash (hash "font-bold" "$foo == 'strong'")))
      (check-equal? (first result) 'data-class)
      (check-true (string-contains? (second result) "\"font-bold\": $foo == 'strong'"))
      (check-equal? (xexpr->string `(button (,(data-class/hash (hash "active" "$isActive"))) "Save"))
                    "<button data-class=\"{&quot;active&quot;: $isActive}\">Save</button>"))

    ;; keyed + hash helpers: data-style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-style keyed form"
      (check-equal? (data-style "display" "$hiding && 'none'")
                    (list 'data-style:display "$hiding && 'none'"))
      (check-equal? (xexpr->string `(div (,(data-style "display" "$hiding && 'none'")) ""))
                    "<div data-style:display=\"$hiding &amp;&amp; 'none'\"></div>"))

    (test-case "data-style keyed form with symbol"
      (check-equal? (data-style 'background-color "$red ? 'red' : 'blue'")
                    (list 'data-style:background-color "$red ? 'red' : 'blue'")))

    (test-case "data-style/hash"
      (define result (data-style/hash (hash "display" "$hiding ? 'none' : 'flex'")))
      (check-equal? (first result) 'data-style)
      (check-true (string-contains? (second result) "\"display\": $hiding ? 'none' : 'flex'"))
      (check-equal?
       (xexpr->string `(div (,(data-style/hash (hash "display" "$visible ? 'block' : 'none'"))) ""))
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

    (test-case "data-on with post integration"
      (check-equal? (data-on "click" (post "/todo/create"))
                    (list 'data-on:click "@post('/todo/create')"))
      (check-equal? (xexpr->string `(button (,(data-on "click" (post "/create"))) "Go"))
                    "<button data-on:click=\"@post('/create')\">Go</button>"))

    ;; data-on -- individual modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-on with once"
      (check-equal? (data-on "click" "fn()" #:once? #t) (list 'data-on:click__once "fn()")))

    (test-case "data-on with passive"
      (check-equal? (data-on "scroll" "fn()" #:passive? #t) (list 'data-on:scroll__passive "fn()")))

    (test-case "data-on with capture"
      (check-equal? (data-on "click" "fn()" #:capture? #t) (list 'data-on:click__capture "fn()")))

    (test-case "data-on with case"
      (check-equal? (data-on "my-event" "fn()" #:case 'camel)
                    (list (string->symbol "data-on:my-event__case.camel") "fn()")))

    (test-case "data-on with window"
      (check-equal? (data-on "keydown" "fn()" #:window? #t) (list 'data-on:keydown__window "fn()")))

    (test-case "data-on with document"
      (check-equal? (data-on "selectionchange" "fn()" #:document? #t)
                    (list 'data-on:selectionchange__document "fn()")))

    (test-case "data-on with outside"
      (check-equal? (data-on "click" "fn()" #:outside? #t) (list 'data-on:click__outside "fn()")))

    (test-case "data-on with prevent"
      (check-equal? (data-on "submit" "fn()" #:prevent? #t) (list 'data-on:submit__prevent "fn()"))
      (check-equal? (xexpr->string `(form (,(data-on "submit" (post "/create") #:prevent? #t)) ""))
                    "<form data-on:submit__prevent=\"@post('/create')\"></form>"))

    (test-case "data-on with stop"
      (check-equal? (data-on "click" "fn()" #:stop? #t) (list 'data-on:click__stop "fn()")))

    ;; data-on -- timing modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-on with debounce string"
      (check-equal? (data-on "input" "fn()" #:debounce "300ms")
                    (list (string->symbol "data-on:input__debounce.300ms") "fn()"))
      (check-equal? (xexpr->string `(input (,(data-on "input" (post "/search") #:debounce "300ms"))))
                    "<input data-on:input__debounce.300ms=\"@post('/search')\"/>"))

    (test-case "data-on with debounce number"
      (check-equal? (data-on "input" "fn()" #:debounce 300)
                    (list (string->symbol "data-on:input__debounce.300") "fn()")))

    (test-case "data-on with debounce leading"
      (check-equal? (data-on "click" "fn()" #:debounce "500ms" #:debounce-leading? #t)
                    (list (string->symbol "data-on:click__debounce.500ms.leading") "fn()"))
      (check-equal?
       (xexpr->string `(button (,(data-on "click" "fn()" #:debounce "500ms" #:debounce-leading? #t))
                               "Go"))
       "<button data-on:click__debounce.500ms.leading=\"fn()\">Go</button>"))

    (test-case "data-on with debounce notrailing"
      (check-equal? (data-on "click" "fn()" #:debounce "500ms" #:debounce-notrailing? #t)
                    (list (string->symbol "data-on:click__debounce.500ms.notrailing") "fn()")))

    (test-case "data-on with debounce leading and notrailing"
      (check-equal?
       (data-on "click" "fn()" #:debounce "500ms" #:debounce-leading? #t #:debounce-notrailing? #t)
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
      (check-equal? (data-on "scroll" "fn()" #:throttle "500ms" #:throttle-noleading? #t)
                    (list (string->symbol "data-on:scroll__throttle.500ms.noleading") "fn()")))

    (test-case "data-on with throttle trailing"
      (check-equal? (data-on "scroll" "fn()" #:throttle "500ms" #:throttle-trailing? #t)
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
      (check-equal? (data-on "click" "fn()" #:viewtransition? #t)
                    (list 'data-on:click__viewtransition "fn()"))
      (check-equal? (xexpr->string `(button (,(data-on "click" "fn()" #:viewtransition? #t)) ""))
                    "<button data-on:click__viewtransition=\"fn()\"></button>"))

    ;; data-on -- combined modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-on with window + debounce"
      (check-equal? (data-on "click" "$foo = ''" #:window? #t #:debounce "500ms")
                    (list (string->symbol "data-on:click__window__debounce.500ms") "$foo = ''")))

    (test-case "data-on with window + debounce + leading (matches Datastar docs example)"
      (check-equal?
       (data-on "click" "$foo = ''" #:window? #t #:debounce "500ms" #:debounce-leading? #t)
       (list (string->symbol "data-on:click__window__debounce.500ms.leading") "$foo = ''"))
      (check-equal?
       (xexpr->string
        `(button
          (,(data-on "click" "$foo = ''" #:window? #t #:debounce "500ms" #:debounce-leading? #t))
          ""))
       "<button data-on:click__window__debounce.500ms.leading=\"$foo = ''\"></button>"))

    (test-case "data-on with debounce + stop + prevent"
      (check-equal? (data-on "input" "@post('/search')" #:debounce "300ms" #:prevent? #t #:stop? #t)
                    (list (string->symbol "data-on:input__prevent__stop__debounce.300ms")
                          "@post('/search')"))
      (check-equal?
       (xexpr->string
        `(input (,(data-on "input" "@post('/search')" #:debounce "300ms" #:prevent? #t #:stop? #t))))
       "<input data-on:input__prevent__stop__debounce.300ms=\"@post('/search')\"/>"))

    (test-case "data-on with all boolean modifiers"
      (define result
        (data-on "click"
                 "fn()"
                 #:once? #t
                 #:passive? #t
                 #:capture? #t
                 #:window? #t
                 #:outside? #t
                 #:prevent? #t
                 #:stop? #t))
      (define attr-name (symbol->string (first result)))
      (check-true (string-contains? attr-name "__once"))
      (check-true (string-contains? attr-name "__passive"))
      (check-true (string-contains? attr-name "__capture"))
      (check-true (string-contains? attr-name "__window"))
      (check-true (string-contains? attr-name "__outside"))
      (check-true (string-contains? attr-name "__prevent"))
      (check-true (string-contains? attr-name "__stop"))
      (check-equal? (second result) "fn()"))

    ;; data-init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-init basic"
      (check-equal? (data-init "$count = 1") '(data-init "$count = 1"))
      (check-equal? (xexpr->string `(div (,(data-init "@get('/events')")) ""))
                    "<div data-init=\"@get('/events')\"></div>"))

    (test-case "data-init with get"
      (check-equal? (data-init (get "/events")) '(data-init "@get('/events')")))

    (test-case "data-init with delay"
      (check-equal? (data-init "$count = 1" #:delay "500ms")
                    (list (string->symbol "data-init__delay.500ms") "$count = 1"))
      (check-equal? (xexpr->string `(div (,(data-init "$count = 1" #:delay "500ms")) ""))
                    "<div data-init__delay.500ms=\"$count = 1\"></div>"))

    (test-case "data-init with viewtransition"
      (check-equal? (data-init "fn()" #:viewtransition? #t) '(data-init__viewtransition "fn()")))

    (test-case "data-init with delay + viewtransition"
      (check-equal? (data-init "fn()" #:delay "500ms" #:viewtransition? #t)
                    (list (string->symbol "data-init__delay.500ms__viewtransition") "fn()")))

    ;; data-on-intersect ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-on-intersect basic"
      (check-equal? (data-on-intersect "$intersected = true")
                    '(data-on-intersect "$intersected = true")))

    (test-case "data-on-intersect with once"
      (check-equal? (data-on-intersect "fn()" #:once? #t) '(data-on-intersect__once "fn()")))

    (test-case "data-on-intersect with half"
      (check-equal? (data-on-intersect "fn()" #:half? #t) '(data-on-intersect__half "fn()")))

    (test-case "data-on-intersect with full"
      (check-equal? (data-on-intersect "fn()" #:full? #t) '(data-on-intersect__full "fn()")))

    (test-case "data-on-intersect with exit"
      (check-equal? (data-on-intersect "fn()" #:exit? #t) '(data-on-intersect__exit "fn()")))

    (test-case "data-on-intersect with threshold number"
      (check-equal? (data-on-intersect "fn()" #:threshold 75)
                    (list (string->symbol "data-on-intersect__threshold.75") "fn()"))
      (check-equal? (xexpr->string `(div (,(data-on-intersect "fn()" #:threshold 75)) ""))
                    "<div data-on-intersect__threshold.75=\"fn()\"></div>"))

    (test-case "data-on-intersect with threshold string"
      (check-equal? (data-on-intersect "fn()" #:threshold "25")
                    (list (string->symbol "data-on-intersect__threshold.25") "fn()")))

    (test-case "data-on-intersect with once + full (matches Datastar docs)"
      (check-equal? (data-on-intersect "$fullyIntersected = true" #:once? #t #:full? #t)
                    '(data-on-intersect__once__full "$fullyIntersected = true"))
      (check-equal?
       (xexpr->string `(div (,(data-on-intersect "$fullyIntersected = true" #:once? #t #:full? #t))
                            ""))
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
      (check-equal? (data-on-intersect "fn()" #:viewtransition? #t)
                    '(data-on-intersect__viewtransition "fn()")))

    (test-case "data-on-intersect all modifiers combined"
      (define result
        (data-on-intersect "fn()" #:once? #t #:half? #t #:debounce "500ms" #:viewtransition? #t))
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
      (check-equal? (data-on-interval "$count++" #:duration "500ms" #:duration-leading? #t)
                    (list (string->symbol "data-on-interval__duration.500ms.leading") "$count++"))
      (check-equal?
       (xexpr->string `(div (,(data-on-interval "$count++" #:duration "500ms" #:duration-leading? #t))
                            ""))
       "<div data-on-interval__duration.500ms.leading=\"$count++\"></div>"))

    (test-case "data-on-interval with viewtransition"
      (check-equal? (data-on-interval "fn()" #:viewtransition? #t)
                    '(data-on-interval__viewtransition "fn()")))

    (test-case "data-on-interval with duration + viewtransition"
      (check-equal? (data-on-interval "fn()" #:duration "1s" #:viewtransition? #t)
                    (list (string->symbol "data-on-interval__duration.1s__viewtransition") "fn()")))

    ;; data-on-signal-patch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-on-signal-patch-filter helper"
      (define result (data-on-signal-patch-filter #:include "counter" #:exclude "temp"))
      (check-equal? (first result) 'data-on-signal-patch-filter)
      (check-true (string-contains? (second result) "\"include\""))
      (check-true (string-contains? (second result) "\"exclude\"")))

    (test-case "data-on-signal-patch basic"
      (check-equal? (data-on-signal-patch "console.log('changed')")
                    '(data-on-signal-patch "console.log('changed')")))

    (test-case "data-on-signal-patch with debounce"
      (check-equal? (data-on-signal-patch "doSomething()" #:debounce "500ms")
                    (list (string->symbol "data-on-signal-patch__debounce.500ms") "doSomething()"))
      (check-equal? (xexpr->string `(div (,(data-on-signal-patch "doSomething()" #:debounce "500ms"))
                                         ""))
                    "<div data-on-signal-patch__debounce.500ms=\"doSomething()\"></div>"))

    (test-case "data-on-signal-patch with throttle"
      (check-equal? (data-on-signal-patch "fn()" #:throttle "1s")
                    (list (string->symbol "data-on-signal-patch__throttle.1s") "fn()")))

    (test-case "data-on-signal-patch with delay"
      (check-equal? (data-on-signal-patch "fn()" #:delay "500ms")
                    (list (string->symbol "data-on-signal-patch__delay.500ms") "fn()")))

    (test-case "compose data-on-signal-patch with data-on-signal-patch-filter"
      (define attrs
        (list (data-on-signal-patch "fn()") (data-on-signal-patch-filter #:include "counter")))
      (check-equal? (first (first attrs)) 'data-on-signal-patch)
      (check-equal? (first (second attrs)) 'data-on-signal-patch-filter)
      (check-equal?
       (xexpr->string `(div ,attrs ""))
       "<div data-on-signal-patch=\"fn()\" data-on-signal-patch-filter=\"{&quot;include&quot;: &quot;counter&quot;}\"></div>"))

    (test-case "data-on-signal-patch rejects include/exclude keywords"
      (check-exn exn:fail? (lambda () (data-on-signal-patch "fn()" #:include "counter")))
      (check-exn exn:fail? (lambda () (data-on-signal-patch "fn()" #:exclude "temp")))
      (check-exn exn:fail?
                 (lambda () (data-on-signal-patch "fn()" #:include "user" #:exclude "password"))))

    ;; data-on-raf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-on-raf basic"
      (check-equal? (data-on-raf "$count++") '(data-on-raf "$count++")))

    (test-case "data-on-raf with throttle"
      (check-equal? (data-on-raf "$count++" #:throttle "10ms")
                    (list (string->symbol "data-on-raf__throttle.10ms") "$count++"))
      (check-equal? (xexpr->string `(div (,(data-on-raf "$count++" #:throttle "10ms")) ""))
                    "<div data-on-raf__throttle.10ms=\"$count++\"></div>"))

    (test-case "data-on-raf with throttle noleading"
      (check-equal? (data-on-raf "fn()" #:throttle "10ms" #:throttle-noleading? #t)
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
      (check-equal? (data-on-resize "fn()" #:debounce "10ms" #:debounce-leading? #t)
                    (list (string->symbol "data-on-resize__debounce.10ms.leading") "fn()")))

    ;; data-ignore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-ignore basic"
      (check-equal? (data-ignore) '(data-ignore ""))
      (check-equal? (xexpr->string `(div (,(data-ignore)) "")) "<div data-ignore=\"\"></div>"))

    (test-case "data-ignore with self"
      (check-equal? (data-ignore #:self? #t) '(data-ignore__self ""))
      (check-equal? (xexpr->string `(div (,(data-ignore #:self? #t)) ""))
                    "<div data-ignore__self=\"\"></div>"))

    (test-case "data-ignore self false is same as basic"
      (check-equal? (data-ignore #:self? #f) '(data-ignore "")))

    ;; data-scroll-into-view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-scroll-into-view basic"
      (check-equal? (data-scroll-into-view) '(data-scroll-into-view ""))
      (check-equal? (xexpr->string `(div (,(data-scroll-into-view)) ""))
                    "<div data-scroll-into-view=\"\"></div>"))

    (test-case "data-scroll-into-view with smooth"
      (check-equal? (data-scroll-into-view #:smooth? #t) '(data-scroll-into-view__smooth "")))

    (test-case "data-scroll-into-view with instant"
      (check-equal? (data-scroll-into-view #:instant? #t) '(data-scroll-into-view__instant "")))

    (test-case "data-scroll-into-view with auto"
      (check-equal? (data-scroll-into-view #:auto? #t) '(data-scroll-into-view__auto "")))

    (test-case "data-scroll-into-view with hstart"
      (check-equal? (data-scroll-into-view #:hstart? #t) '(data-scroll-into-view__hstart "")))

    (test-case "data-scroll-into-view with hcenter"
      (check-equal? (data-scroll-into-view #:hcenter? #t) '(data-scroll-into-view__hcenter "")))

    (test-case "data-scroll-into-view with hend"
      (check-equal? (data-scroll-into-view #:hend? #t) '(data-scroll-into-view__hend "")))

    (test-case "data-scroll-into-view with hnearest"
      (check-equal? (data-scroll-into-view #:hnearest? #t) '(data-scroll-into-view__hnearest "")))

    (test-case "data-scroll-into-view with vstart"
      (check-equal? (data-scroll-into-view #:vstart? #t) '(data-scroll-into-view__vstart "")))

    (test-case "data-scroll-into-view with vcenter"
      (check-equal? (data-scroll-into-view #:vcenter? #t) '(data-scroll-into-view__vcenter "")))

    (test-case "data-scroll-into-view with vend"
      (check-equal? (data-scroll-into-view #:vend? #t) '(data-scroll-into-view__vend "")))

    (test-case "data-scroll-into-view with vnearest"
      (check-equal? (data-scroll-into-view #:vnearest? #t) '(data-scroll-into-view__vnearest "")))

    (test-case "data-scroll-into-view with focus"
      (check-equal? (data-scroll-into-view #:focus? #t) '(data-scroll-into-view__focus "")))

    (test-case "data-scroll-into-view with smooth + vend"
      (check-equal? (data-scroll-into-view #:smooth? #t #:vend? #t)
                    '(data-scroll-into-view__smooth__vend ""))
      (check-equal? (xexpr->string `(div (,(data-scroll-into-view #:smooth? #t #:vend? #t)) ""))
                    "<div data-scroll-into-view__smooth__vend=\"\"></div>"))

    (test-case "data-scroll-into-view with smooth + hcenter + vcenter + focus"
      (define result (data-scroll-into-view #:smooth? #t #:hcenter? #t #:vcenter? #t #:focus? #t))
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
      (check-equal? (data-persist #:session? #t) '(data-persist__session "")))

    (test-case "data-persist with key + session"
      (check-equal? (data-persist #:key "mykey" #:session? #t) (list 'data-persist:mykey__session ""))
      (check-equal? (xexpr->string `(div (,(data-persist #:key "mykey" #:session? #t)) ""))
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
      (check-equal? (data-query-string #:history? #t) '(data-query-string__history "")))

    (test-case "data-query-string with filter"
      (check-equal? (data-query-string #:filter? #t) '(data-query-string__filter "")))

    (test-case "data-query-string with filter + history"
      (check-equal? (data-query-string #:filter? #t #:history? #t)
                    '(data-query-string__filter__history ""))
      (check-equal? (xexpr->string `(div (,(data-query-string #:filter? #t #:history? #t)) ""))
                    "<div data-query-string__filter__history=\"\"></div>"))

    (test-case "data-query-string with include"
      (define result (data-query-string #:include "foo"))
      (check-true (string-contains? (second result) "\"include\"")))

    (test-case "data-query-string with include + exclude + history"
      (define result (data-query-string #:include "foo" #:exclude "bar" #:history? #t))
      (check-equal? (first result) 'data-query-string__history)
      (check-true (string-contains? (second result) "\"include\""))
      (check-true (string-contains? (second result) "\"exclude\"")))

    ;; data-json-signals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-json-signals basic"
      (check-equal? (data-json-signals) '(data-json-signals ""))
      (check-equal? (xexpr->string `(pre (,(data-json-signals)) ""))
                    "<pre data-json-signals=\"\"></pre>"))

    (test-case "data-json-signals with terse"
      (check-equal? (data-json-signals #:terse? #t) '(data-json-signals__terse ""))
      (check-equal? (xexpr->string `(pre (,(data-json-signals #:terse? #t)) ""))
                    "<pre data-json-signals__terse=\"\"></pre>"))

    (test-case "data-json-signals with include"
      (define result (data-json-signals #:include "counter"))
      (check-true (string-contains? (second result) "\"include\"")))

    (test-case "data-json-signals with include + terse"
      (define result (data-json-signals #:include "counter" #:terse? #t))
      (check-equal? (first result) 'data-json-signals__terse)
      (check-true (string-contains? (second result) "\"include\""))
      (check-equal?
       (xexpr->string `(pre (,(data-json-signals #:include "counter" #:terse? #t)) ""))
       "<pre data-json-signals__terse=\"{&quot;include&quot;: &quot;counter&quot;}\"></pre>"))

    ;; data-preserve-attrs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-case "data-preserve-attr alias"
      (check-equal? (data-preserve-attr "open") '(data-preserve-attr "open"))
      (check-equal? (data-preserve-attr '("open" "class")) '(data-preserve-attr "open class")))

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

    (test-case "init with get in x-expression"
      (define attr (data-init (get "/events")))
      (check-equal? attr '(data-init "@get('/events')")))

    (test-case "on with post and format in x-expression"
      (define tid 42)
      (define attr (data-on "click" (post (format "/todo/delete/~a" tid))))
      (check-equal? attr (list 'data-on:click "@post('/todo/delete/42')")))

    (test-case "complex form with debounce pattern"
      (define input-attr (data-on "input" (post "/search") #:debounce "250ms"))
      (define bind-attr (data-bind "filter"))
      (define indicator-attr (data-indicator "filtering"))
      (check-equal? input-attr
                    (list (string->symbol "data-on:input__debounce.250ms") "@post('/search')"))
      (check-equal? bind-attr (list 'data-bind:filter ""))
      (check-equal? indicator-attr (list 'data-indicator:filtering "")))

    (test-case "xexpr->string round-trip"
      (check-equal? (xexpr->string `(div (,(data-show "$visible") (id "test")) "hello"))
                    "<div data-show=\"$visible\" id=\"test\">hello</div>"))

    ;; x-expression integration -- realistic templates ;;;;;;;;;;;;;;;;;;;;;

    (test-case "search input pattern renders correct HTML"
      (check-equal?
       (xexpr->string `(input (,(data-on "input" (post "/search") #:debounce "250ms")
                               ,(data-bind "filter")
                               ,(data-indicator "filtering"))))
       "<input data-on:input__debounce.250ms=\"@post('/search')\" data-bind:filter=\"\" data-indicator:filtering=\"\"/>"))

    (test-case "form with post renders correct HTML"
      (check-equal?
       (xexpr->string `(form (,(data-on "submit" (post "/todo/create") #:prevent? #t))
                             (input ((type "text") ,(data-bind "newTodo")))
                             (button ((type "submit")) "Add")))
       "<form data-on:submit__prevent=\"@post('/todo/create')\"><input type=\"text\" data-bind:newTodo=\"\"/><button type=\"submit\">Add</button></form>"))

    (test-case "page with init + signals renders correct HTML"
      (check-equal?
       (xexpr->string `(body (,(data-init (get "/updates")) ,(data-signals/hash (hash 'count 0)))
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

    (test-case "data-signals/hash with empty hash"
      (check-equal? (data-signals/hash (hash)) '(data-signals "{}")))

    (test-case "data-computed/hash single entry returns attribute pair"
      (check-equal? (data-computed/hash (hash 'x "() => $y")) '(data-computed "{\"x\": () => $y}")))

    (test-case "data-on-signal-patch returns single pair"
      (define result (data-on-signal-patch "fn()"))
      (check-equal? (length result) 2)
      (check-true (symbol? (first result))))

    (test-case "data-on-signal-patch-filter returns single pair"
      (define result (data-on-signal-patch-filter #:include "x"))
      (check-equal? (length result) 2)
      (check-true (symbol? (first result))))

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

    (test-case "hash helpers error on non-hash input"
      (check-exn exn:fail? (lambda () (data-signals/hash 'foo)))
      (check-exn exn:fail? (lambda () (data-computed/hash 'foo)))
      (check-exn exn:fail? (lambda () (data-attr/hash 'foo)))
      (check-exn exn:fail? (lambda () (data-class/hash 'foo)))
      (check-exn exn:fail? (lambda () (data-style/hash 'foo))))

    (test-case "hash helpers enforce key contracts"
      (check-exn exn:fail:contract? (lambda () (data-signals/hash (hash 1 0))))
      (check-exn exn:fail:contract? (lambda () (data-signals/hash (hash "x" 0))))
      (check-exn exn:fail:contract? (lambda () (data-computed/hash (hash 1 "$x"))))
      (check-exn exn:fail:contract? (lambda () (data-attr/hash (hash 1 "$x"))))
      (check-exn exn:fail:contract? (lambda () (data-class/hash (hash 1 "$x"))))
      (check-exn exn:fail:contract? (lambda () (data-style/hash (hash 1 "$x")))))

    (test-case "data-signals keyed form requires value"
      (check-exn exn:fail? (lambda () (data-signals 'foo))))

    (test-case "data-computed keyed form requires value"
      (check-exn exn:fail? (lambda () (data-computed 'foo))))

    (test-case "data-attr keyed form requires value"
      (check-exn exn:fail? (lambda () (data-attr 'disabled))))

    (test-case "data-class keyed form requires value"
      (check-exn exn:fail? (lambda () (data-class 'active))))

    (test-case "data-style keyed form requires value"
      (check-exn exn:fail? (lambda () (data-style 'display))))

    (test-case "keyed helpers reject hash input"
      (check-exn exn:fail? (lambda () (data-signals (hash 'x 1) "1")))
      (check-exn exn:fail? (lambda () (data-computed (hash 'x "$x") "$x")))
      (check-exn exn:fail? (lambda () (data-attr (hash "disabled" "$x") "$x")))
      (check-exn exn:fail? (lambda () (data-class (hash "active" "$x") "$x")))
      (check-exn exn:fail? (lambda () (data-style (hash "display" "$x") "$x"))))

    (test-case "hash helpers reject extra positional value"
      (check-exn exn:fail? (lambda () (data-signals/hash (hash 'x 1) "1")))
      (check-exn exn:fail? (lambda () (data-computed/hash (hash 'x "$x") "$x")))
      (check-exn exn:fail? (lambda () (data-attr/hash (hash "disabled" "$x") "$x")))
      (check-exn exn:fail? (lambda () (data-class/hash (hash "active" "$x") "$x")))
      (check-exn exn:fail? (lambda () (data-style/hash (hash "display" "$x") "$x"))))

    (test-case "hash helpers reject keyed-only #:case modifier"
      (check-exn exn:fail? (lambda () (data-signals/hash (hash 'x 1) #:case 'camel)))
      (check-exn exn:fail? (lambda () (data-computed/hash (hash 'x "$x") #:case 'camel)))
      (check-exn exn:fail? (lambda () (data-class/hash (hash "active" "$x") #:case 'camel))))

    (test-case "data-computed/hash values must be string callables"
      (check-exn exn:fail? (lambda () (data-computed/hash (hash 'x 1)))))

    (test-case "invalid case keyword value raises contract"
      (check-exn exn:fail:contract? (lambda () (data-on "click" "fn()" #:case 'upper)))
      (check-exn exn:fail:contract? (lambda () (data-signals "foo" "1" #:case 'upper))))

    (test-case "removed keywords reject"
      (check-exn exn:fail? (lambda () (data-on "click" "fn()" #:trust #t)))
      (check-exn exn:fail? (lambda () (data-bind "foo" #:as 'value)))
      (check-exn exn:fail? (lambda () (data-ref "foo" #:as 'value)))
      (check-exn exn:fail? (lambda () (data-indicator "foo" #:as 'value))))))

(define constants-tests
  (test-suite "constants"

    (test-case "CDN URLs contain version"
      (check-true (string-contains? datastar-cdn-url datastar-version))
      (check-true (string-contains? datastar-cdn-map-url datastar-version)))

    (test-case "CDN URLs have correct suffix"
      (check-true (string-suffix? datastar-cdn-url "/bundles/datastar.js"))
      (check-true (string-suffix? datastar-cdn-map-url "/bundles/datastar.js.map")))))

(define sugar-tests
  (test-suite "sugar"
    constants-tests
    actions-tests
    attributes-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests sugar-tests))
