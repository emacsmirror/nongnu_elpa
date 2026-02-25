.POSIX:
.PHONY: all doc clean
.SUFFIXES: .el .elc

EMACS = emacs
ORG := doc/gnosis.org
TEXI := doc/gnosis.texi
INFO := doc/gnosis.info
TEST_FILES := tests/gnosis-test-algorithm.el tests/gnosis-test-export-import.el tests/gnosis-test-dashboard.el

all: doc

doc:	$(ORG)
	$(EMACS) --batch \
	-Q \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-texinfo) (org-texinfo-export-to-info) (save-buffer))" \
	--kill


test:
	rm -f *.elc
	@for f in $(TEST_FILES); do \
		echo "Running $$f..."; \
		$(EMACS) --batch \
		-q \
		--eval "(add-to-list 'load-path \"$(shell pwd)\")" \
		--load $$f; \
	done

clean:
	rm -f $(TEXI) $(INFO) *.elc *-pkg.el*
