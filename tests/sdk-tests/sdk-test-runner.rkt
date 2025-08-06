#lang racket

(require racket/system
         racket/tcp
         racket/match
         rackunit
         "sdk-test-server.rkt")

(define SERVER_PORT 7331)
(define SERVER_READY_TIMEOUT 10)
(define TEST_TIMEOUT 30)

(define (wait-for-server-ready port timeout)
  (define end-time (+ (current-seconds) timeout))
  (let loop ()
    (cond
      [(> (current-seconds) end-time) #f]
      [(with-handlers ([exn:fail:network? (λ (_) #f)])
         (define-values (in out) (tcp-connect "localhost" port))
         (close-input-port in)
         (close-output-port out)
         #t)
       (printf "Server ready on port ~a~n" port)
       #t]
      [else
       (sleep 0.5)
       (loop)])))

(define (run-server-and-tests)
  (printf "Starting Datastar SDK tests...~n")

  (with-handlers ([exn:fail:network? (λ (e) (error "Port ~a is already in use" SERVER_PORT))])
    (define listener (tcp-listen SERVER_PORT))
    (tcp-close listener))

  (printf "Starting test server on port ~a...~n" SERVER_PORT)
  (define server-thread (thread (λ () (start-test-server #:port SERVER_PORT))))

  (define test-success? #f)

  (dynamic-wind
   (λ ()
     (unless (wait-for-server-ready SERVER_PORT SERVER_READY_TIMEOUT)
       (kill-thread server-thread)
       (error "Test server failed to start within ~a seconds" SERVER_READY_TIMEOUT)))
   (λ ()
     (printf "Running SDK tests...~n")
     (define test-result
       (system* (find-executable-path "go")
                "run"
                "github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest"))
     (set! test-success? test-result)
     (if test-result
         (printf "✓ SDK tests passed!~n")
         (printf "✗ SDK tests failed~n")))
   (λ ()
     (printf "Shutting down test server...~n")
     (kill-thread server-thread)
     (printf "Test server stopped~n")))

  test-success?)

(module+ test
  (test-case "Datastar SDK Integration Test"
    (check-true (run-server-and-tests) "SDK tests should pass against Racket server")))

(module+ main
  (define success? (run-server-and-tests))
  (exit (if success? 0 1)))
