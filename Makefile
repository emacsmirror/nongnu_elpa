.PHONY: all build clean install uninstall check test

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

all: build

build: autoload compile module

dev: autoload compile module lint test

autoload:
	$(EMACS_CMD) -q -Q --batch -L lisp \
	--eval="(loaddefs-generate \"lisp\" \"lisp/jabber-autoloads.el\")"

module:
	@if [ -z "$$(ls -A src/picomemo 2>/dev/null)" ]; then \
	  git submodule update --init --recursive; \
	fi
ifdef GUIX
	guix shell -m manifest.scm -- $(MAKE) -C src
else
	$(MAKE) -C src
endif

compile: autoload
	$(EMACS_CMD) -q -Q -L . -L lisp --batch \
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

lint-test-compile:
	$(EMACS_CMD) -q -Q --batch -L lisp -L tests \
	-f batch-byte-compile tests/*.el

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint lint-test-compile

test:
	$(EMACS_CMD) -Q --batch -L lisp -L tests \
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

prefix      ?= /usr/local
datarootdir ?= $(prefix)/share
lispdir     ?= $(datarootdir)/emacs/site-lisp/jabber

check: test

install: build
	install -d $(DESTDIR)$(lispdir)
	install -m 644 lisp/*.el $(DESTDIR)$(lispdir)/
	-install -m 644 lisp/*.elc $(DESTDIR)$(lispdir)/
	-install -m 755 lisp/jabber-omemo-core.so $(DESTDIR)$(lispdir)/

uninstall:
	rm -rf $(DESTDIR)$(lispdir)
