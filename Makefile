EMACS ?= emacs

EL  := flamegraph.el test/flamegraph-tests.el
ELC := $(EL:.el=.elc)

.PHONY: check compile test clean

check: compile test

compile:
	$(EMACS) -Q -batch -L . \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(EL)

test:
	$(EMACS) -Q -batch -L . \
	  -l test/flamegraph-tests.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f $(ELC)
