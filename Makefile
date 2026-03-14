.phony: all compile lint clean test

ifndef EMACS_CMD
GUIX := $(shell command -v guix 2>/dev/null)
ifdef GUIX
EMACS_CMD = guix shell -m manifest.scm -- emacs
else
EMACS_CMD = emacs
endif
endif

TESTS ?= tests/jabber-xml-tests.el tests/jabber-util-tests.el \
         tests/jabber-muc-tests.el tests/jabber-roster-tests.el \
         tests/jabber-chat-tests.el tests/jabber-db-tests.el

build: autoload compile module

dev: autoload compile module lint test

autoload:
	$(EMACS_CMD) -q -Q --batch -L lisp -L lisp/jabber-fallback-lib \
	--eval="(loaddefs-generate \"lisp\" \"lisp/jabber-autoloads.el\")"

module:
ifdef GUIX
	guix shell -m manifest.scm -- $(MAKE) -C src
else
	$(MAKE) -C src
endif

compile:
	$(EMACS_CMD) -q -Q -L . -L lisp -L lisp/jabber-fallback-lib --batch \
	--eval="(setq print-length nil load-prefer-newer t)" \
	-f batch-byte-compile lisp/*.el

lint-check-declare:
	for file in lisp/*.el ; do \
	$(EMACS_CMD) -q -Q --batch --eval="(check-declare-file \"$$file\")" ; \
	done

lint-checkdoc:
	for file in lisp/*.el ; do \
	$(EMACS_CMD) -q -Q --batch --eval="(checkdoc-file \"$$file\")" ; \
	done

lint-package-lint:
	$(EMACS_CMD) -Q --batch \
	--eval='(package-initialize)' --eval="(require 'package-lint)" \
        -f 'package-lint-batch-and-exit' $(wildcard lisp/*.el)

lint-relint:
	$(EMACS_CMD) -Q --batch \
	--eval='(package-initialize)' --eval="(require 'relint)" \
	-f 'relint-batch' "lisp"

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint

test:
	$(EMACS_CMD) -Q --batch -L lisp -L lisp/jabber-fallback-lib -L tests \
	-l ert $(foreach t,$(TESTS),-l $(t)) \
	-f ert-run-tests-batch-and-exit

clean-elc:
	find . -name '*.elc' -delete

clean-module:
ifdef GUIX
	guix shell -m manifest.scm -- $(MAKE) -C src clean
else
	$(MAKE) -C src clean
endif

clean: clean-elc clean-module
