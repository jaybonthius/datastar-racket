# Datastar SDK Tests for Racket

Tests the Racket Datastar implementation against the official SDK test suite from https://github.com/starfederation/datastar/tree/develop/sdk/tests.

## Files

- [**`sdk-test-server.rkt`**](./sdk-test-server.rkt): A web server that implements the `/test` endpoint required by the SDK test suite. Handles various Datastar events and returns appropriate SSE responses.
- [**`sdk-test-runner.rkt`**](./sdk-test-runner.rkt): Test runner that starts the test server on port 7331, runs the official Go-based SDK tests against it, then shuts down the server.

## Prerequisites

- Go must be installed (the official test suite is written in Go)
- Racket with the `datastar` package installed

## Running the Tests

```sh
raco test tests/sdk-tests
```
