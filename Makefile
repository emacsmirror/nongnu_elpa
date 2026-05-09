## ################################################################
CASK ?= cask

tests:
	$(CASK) emacs -batch -load test/ert-helper.el -f ert-run-tests-batch-and-exit

build:
	$(CASK) build

clean:
	$(CASK) clean-elc
