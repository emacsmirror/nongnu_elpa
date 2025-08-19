## ################################################################

tests:
	cask emacs -batch -load test/ert-helper.el -f ert-run-tests-batch-and-exit

build:
	cask build

clean:
	cask clean-elc
