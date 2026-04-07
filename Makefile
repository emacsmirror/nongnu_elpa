.PHONY: all build clean install uninstall check test load \
        do-test do-lint-check-declare do-lint-checkdoc

ifndef EMACS_CMD
GUIX := $(shell command -v guix 2>/dev/null)
ifdef GUIX
GUIX_SHELL := guix shell -D -f guix.scm emacs-next emacs-package-lint emacs-relint --
EMACS_CMD := $(GUIX_SHELL) emacs
else
GUIX_SHELL :=
EMACS_CMD := emacs
endif
endif

# For loop targets (test, per-file linters), enter guix shell once and
# re-exec make so the inner loop calls plain `emacs` from the profile.
GUIX_WRAP = $(if $(GUIX_SHELL),$(GUIX_SHELL) $(MAKE) --no-print-directory EMACS_CMD=emacs,$(MAKE) --no-print-directory)

TESTS ?= tests/jabber-activity-tests.el \
         tests/jabber-bookmarks-tests.el \
         tests/jabber-carbons-tests.el \
         tests/jabber-chatbuffer-tests.el \
         tests/jabber-chatstates-tests.el \
         tests/jabber-chat-tests.el \
         tests/jabber-csi-tests.el \
         tests/jabber-db-tests.el \
         tests/jabber-disco-tests.el \
         tests/jabber-mam-tests.el \
         tests/jabber-message-correct-tests.el \
         tests/jabber-message-reply-tests.el \
         tests/jabber-modeline-tests.el \
         tests/jabber-moderation-tests.el \
         tests/jabber-muc-tests.el \
         tests/jabber-omemo-message-tests.el \
         tests/jabber-omemo-module-tests.el \
         tests/jabber-omemo-protocol-tests.el \
         tests/jabber-omemo-store-tests.el \
         tests/jabber-omemo-trust-tests.el \
         tests/jabber-openpgp-legacy-tests.el \
         tests/jabber-presence-tests.el \
         tests/jabber-pubsub-tests.el \
         tests/jabber-receipts-tests.el \
         tests/jabber-roster-tests.el \
         tests/jabber-sm-tests.el \
         tests/jabber-styling-tests.el \
         tests/jabber-transient-tests.el \
         tests/jabber-util-tests.el \
         tests/jabber-xml-tests.el

all: build

build: autoload compile module

dev: autoload compile module lint test

autoload:
	$(EMACS_CMD) -q -Q --batch -L lisp \
	--eval="(loaddefs-generate \"lisp\" \"lisp/jabber-autoloads.el\")"

PICOMEMO_REPO = https://github.com/mierenhoop/picomemo.git
PICOMEMO_COMMIT = 7ac189ad2461d99b765abcc28e8439e81a047bc8

src/picomemo/omemo.c:
	git submodule update --init --recursive 2>/dev/null || \
	(git clone $(PICOMEMO_REPO) src/picomemo && \
	 git -C src/picomemo checkout $(PICOMEMO_COMMIT))

module: src/picomemo/omemo.c
ifdef GUIX
	guix shell -D -f guix.scm -- $(MAKE) -C src
else
	$(MAKE) -C src
endif

compile: autoload
	$(EMACS_CMD) -q -Q -L . -L lisp --batch \
	--eval="(setq print-length nil load-prefer-newer t)" \
	-f batch-byte-compile lisp/*.el

lint-check-declare:
	@$(GUIX_WRAP) do-lint-check-declare

do-lint-check-declare:
	for file in lisp/*.el ; do \
	$(EMACS_CMD) -q -Q --batch --eval="(check-declare-file \"$$file\")" ; \
	done

lint-checkdoc:
	@$(GUIX_WRAP) do-lint-checkdoc

do-lint-checkdoc:
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
	@$(GUIX_WRAP) do-test

do-test:
	@passed=0; failed=0; total=0; failed_files=""; \
	for t in $(TESTS); do \
	  output=$$($(EMACS_CMD) -Q --batch -L lisp -L tests \
	    -l ert -l $$t -f ert-run-tests-batch-and-exit 2>&1); \
	  rc=$$?; \
	  n=$$(echo "$$output" | grep -o 'Ran [0-9]*' | grep -o '[0-9]*'); \
	  total=$$((total + n)); \
	  if [ $$rc -ne 0 ]; then \
	    failed=$$((failed + n)); \
	    failed_files="$$failed_files $$t"; \
	    printf "\033[31mFAIL\033[0m $$t ($$n tests)\n"; \
	    echo "$$output" | grep '  FAILED'; \
	  else \
	    passed=$$((passed + n)); \
	    printf "\033[32m  OK\033[0m $$t ($$n tests)\n"; \
	  fi; \
	done; \
	echo ""; \
	if [ $$failed -eq 0 ]; then \
	  printf "\033[32m$$total tests, $$passed passed, 0 failed\033[0m\n"; \
	else \
	  printf "\033[31m$$total tests, $$passed passed, $$failed failed\033[0m\n"; \
	  for f in $$failed_files; do echo "  $$f"; done; \
	fi; \
	[ $$failed -eq 0 ]

load: clean-elc
	@for f in lisp/*.el; do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@printf "\033[32mLoaded all lisp/*.el into Emacs\033[0m\n"

clean-elc:
	find . -name '*.elc' -delete

clean-module:
ifdef GUIX
	guix shell -D -f guix.scm -- $(MAKE) -C src clean
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
