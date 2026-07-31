EMACS ?= emacs

.PHONY: compile test clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile tabspaces.el

test:
	$(EMACS) -Q --batch -L . -l tabspaces.el -l tabspaces-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
