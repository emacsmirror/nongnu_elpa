# Makefile for codex-ide

EMACS ?= emacs
ELS = codex-ide.el codex-ide-term.el codex-ide-transient.el codex-ide-debug.el codex-ide-context.el codex-ide-mcp.el
TESTS = test/codex-ide-tests.el test/codex-ide-context-tests.el test/codex-ide-mcp-tests.el
CHECKDOC_FILES = $(foreach f,$(ELS) $(TESTS),"$(f)")

.PHONY: compile test clean checkdoc

compile:
	$(EMACS) -Q --batch -L . -L test \
	  -f batch-byte-compile $(ELS)

test: compile
	$(EMACS) -Q --batch -L . -L test $(patsubst %,-l %,$(TESTS)) \
	  -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q --batch -L . \
	  --eval '(require (quote checkdoc))' \
	  --eval '(let ((ok t)) (dolist (file (list $(CHECKDOC_FILES))) (with-current-buffer (find-file-noselect file) (unless (checkdoc-eval-current-buffer) (setq ok nil)) (kill-buffer))) (kill-emacs (if ok 0 1)))'

clean:
	rm -f *.elc test/*.elc
