.POSIX:
.PHONY: all doc clean
.SUFFIXES: .el .elc

EMACS = emacs
ORG := doc/gnosis.org
TEXI := doc/gnosis.texi
INFO := doc/gnosis.info
TEST_FILE := tests/gnosis-test-algorithm.el

all: doc

doc:	$(ORG)
	$(EMACS) --batch \
	-Q \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-texinfo) (org-texinfo-export-to-info) (save-buffer))" \
	--kill


test:
	rm -f *.elc
	$(EMACS) --batch \
	-q \
	--eval "(add-to-list 'load-path \"$(shell pwd)\")" \
	--load $(TEST_FILE) \
	--eval "(ert-run-tests-batch-and-exit)"

clean:
	rm -f $(TEXI) $(INFO) *.elc *-pkg.el*
