.PHONY: lint test

RKT_FILES := $(shell find . -name '*.rkt' -not -path './.git/*')

lint:
	@for f in $(RKT_FILES); do raco review $$f; done

fmt:
	@for f in $(RKT_FILES); do raco fmt -i $$f; done

test:
	raco test datastar/
	raco test datastar-brotli/
	raco test sdk-tests/sdk-test-runner.rkt
