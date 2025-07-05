ALL_TARGETS = help clean test compile
EMACS_TEST_JUNIT_REPORT=1

.PHONY: ${ALL_TARGETS}

help:
	@echo Targets:
	@for t in ${ALL_TARGETS}; do echo "- "$$t; done

clean:
	@rm -rf *.elc test/*.elc

test:
	emacs --batch -L . -L tests -l test-flymake-pyrefly \
	-f ert-run-tests-batch-and-exit

compile:
	emacs --batch -L . \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile flymake-pyrefly.el
