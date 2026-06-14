EMACS ?= emacs
KEYMAP_POPUP ?=
LOAD_PATH = -L lisp -L tests $(if $(KEYMAP_POPUP),-L $(KEYMAP_POPUP))
ELISP_FILES = lisp/hermes-transport.el lisp/hermes-chat.el lisp/hermes.el

.PHONY: test compile check clean

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l ert -l tests/hermes-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch $(LOAD_PATH) -f batch-byte-compile $(ELISP_FILES)

check: compile test

clean:
	rm -f lisp/*.elc tests/*.elc
