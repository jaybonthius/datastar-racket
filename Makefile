.PHONY: lint test fmt

RKT_FILES := $(shell find datastar-lib datastar-test datastar-doc datastar-brotli sdk-tests examples -name '*.rkt' -not -path './.git/*' 2>/dev/null)

lint:
	@for f in $(RKT_FILES); do raco review $$f; done

fmt:
	@for f in $(RKT_FILES); do raco fmt -i $$f; done

test:
	raco test datastar-test/
	raco test sdk-tests/sdk-test-runner.rkt
