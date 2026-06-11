.PHONY: all build clean install uninstall check test load \
        do-test do-test-summary do-lint-check-declare do-lint-checkdoc \
        do-lint-native-comp

GUIX := $(shell command -v guix 2>/dev/null)
NIX := $(shell command -v nix-shell 2>/dev/null)

ifneq ($(IN_NIX_SHELL),)
NIX_SHELL :=
else ifneq ($(wildcard shell.nix),)
ifdef NIX
NIX_SHELL := nix-shell --pure shell.nix --run
endif
endif

ifndef EMACS_CMD
ifdef GUIX
GUIX_SHELL := guix shell --pure -D -f guix.scm emacs-no-x emacs-package-lint emacs-relint --
EMACS_CMD := $(GUIX_SHELL) emacs
else
GUIX_SHELL :=
EMACS_CMD := emacs
endif
endif

# For loop targets (test, per-file linters), enter guix shell once and
# re-exec make so the inner loop calls plain `emacs` from the profile.
GUIX_WRAP = $(if $(GUIX_SHELL),$(GUIX_SHELL) $(MAKE) --no-print-directory EMACS_CMD=emacs,$(MAKE) --no-print-directory)

JOBS         ?= $(shell nproc 2>/dev/null || echo 4)
TEST_RESULTS := .test-results

TESTS ?= tests/jabber-test-activity.el \
         tests/jabber-test-bookmarks.el \
         tests/jabber-test-carbons.el \
         tests/jabber-test-chat.el \
         tests/jabber-test-chatbuffer.el \
         tests/jabber-test-chatstates.el \
         tests/jabber-test-csi.el \
         tests/jabber-test-db.el \
         tests/jabber-test-disco.el \
         tests/jabber-test-mam.el \
         tests/jabber-test-menu.el \
         tests/jabber-test-message-correct.el \
         tests/jabber-test-message-reply.el \
         tests/jabber-test-modeline.el \
         tests/jabber-test-moderation.el \
         tests/jabber-test-muc.el \
         tests/jabber-test-omemo-message.el \
         tests/jabber-test-omemo-module.el \
         tests/jabber-test-omemo-protocol.el \
         tests/jabber-test-omemo-store.el \
         tests/jabber-test-omemo-trust.el \
         tests/jabber-test-openpgp-legacy.el \
         tests/jabber-test-presence.el \
         tests/jabber-test-pubsub.el \
         tests/jabber-test-vcard-avatars.el \
         tests/jabber-test-reactions.el \
         tests/jabber-test-receipts.el \
         tests/jabber-test-roster.el \
         tests/jabber-test-sm.el \
         tests/jabber-test-srv.el \
         tests/jabber-test-styling.el \
         tests/jabber-test-util.el \
         tests/jabber-test-xml.el

TEST_STAMPS := $(patsubst tests/%.el,$(TEST_RESULTS)/%.stamp,$(TESTS))

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
else ifdef NIX_SHELL
	$(NIX_SHELL) '$(MAKE) -C src'
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
	case "$$file" in lisp/jabber-autoloads.el) continue;; esac; \
	$(EMACS_CMD) -q -Q --batch --eval="(checkdoc-file \"$$file\")" ; \
	done

lint-package-lint:
	$(EMACS_CMD) -Q --batch \
	--eval='(package-initialize)' --eval="(require 'package-lint)" \
	--eval="(setq package-lint-main-file \"lisp/jabber.el\")" \
        -f 'package-lint-batch-and-exit' $(wildcard lisp/*.el)

lint-relint:
	$(EMACS_CMD) -Q --batch \
	--eval='(package-initialize)' --eval="(require 'relint)" \
	-f 'relint-batch' "lisp"

lint-test-compile:
	$(EMACS_CMD) -q -Q --batch -L lisp -L tests \
	-f batch-byte-compile tests/*.el

lint-native-comp: autoload
	@$(GUIX_WRAP) do-lint-native-comp

do-lint-native-comp:
	@fails=0; \
	for file in lisp/*.el ; do \
	  case "$$file" in *autoloads*) continue;; esac; \
	  output=$$($(EMACS_CMD) -q -Q --batch -L lisp \
	    --eval="(native-compile \"$$file\")" 2>&1); \
	  matched=$$(echo "$$output" | grep "is not known to be defined" || true); \
	  if [ -n "$$matched" ]; then \
	    echo "$$matched"; \
	    fails=1; \
	  fi; \
	done; \
	exit $$fails

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint lint-test-compile

test: module
	@rm -rf $(TEST_RESULTS)
	@mkdir -p $(TEST_RESULTS)
	@$(GUIX_WRAP) -j$(JOBS) -Otarget do-test

do-test: do-test-summary

$(TEST_RESULTS)/%.stamp: tests/%.el
	@output=$$($(EMACS_CMD) -Q --batch -L lisp -L tests \
	  -l ert -l $< -f ert-run-tests-batch-and-exit 2>&1); \
	rc=$$?; \
	n=$$(echo "$$output" | grep -o 'Ran [0-9]*' | grep -o '[0-9]*'); \
	if [ $$rc -ne 0 ]; then \
	  printf "\033[31mFAIL\033[0m $< ($$n tests)\n"; \
	  echo "$$output" | grep '  FAILED'; \
	  printf "FAIL %s\n" "$$n" > $@; \
	else \
	  printf "\033[32m  OK\033[0m $< ($$n tests)\n"; \
	  printf "OK %s\n" "$$n" > $@; \
	fi

do-test-summary: $(TEST_STAMPS)
	@total=0; passed=0; failed=0; failed_files=""; \
	for f in $(TEST_STAMPS); do \
	  read status n < $$f; \
	  total=$$((total + n)); \
	  if [ "$$status" = "FAIL" ]; then \
	    failed=$$((failed + n)); \
	    base=$$(basename $$f .stamp); \
	    failed_files="$$failed_files tests/$$base.el"; \
	  else \
	    passed=$$((passed + n)); \
	  fi; \
	done; \
	echo ""; \
	if [ $$failed -eq 0 ]; then \
	  printf "\033[32m$$total tests, $$passed passed, 0 failed\033[0m\n"; \
	  rm -rf $(TEST_RESULTS); \
	else \
	  printf "\033[31m$$total tests, $$passed passed, $$failed failed\033[0m\n"; \
	  for f in $$failed_files; do echo "  $$f"; done; \
	  printf "\nStamps preserved in $(TEST_RESULTS)/ for debugging.\n"; \
	fi; \
	[ $$failed -eq 0 ]

load: clean-elc
	@emacsclient --eval "(progn \
	  (dolist (sym '(jabber-global-keymap jabber-common-keymap \
	               jabber-chat-mode-map jabber-console-mode-map \
	               jabber-browse-mode-map \
	               jabber-roster-popup-map \
	               jabber-roster-presence-map jabber-roster-discovery-map \
	               jabber-roster-contact-action-map \
	               jabber-info-menu-map jabber-muc-menu-map \
	               jabber-service-menu-map \
	               jabber-chat-encryption-menu-map \
	               jabber-chat-operations-menu-map \
	               jabber-omemo-trust-mode-map \
	               jabber-bookmarks-edit-map jabber-bookmarks-mode-map)) \
	    (when (boundp sym) (makunbound sym))))" > /dev/null
	@emacsclient --eval "(load-file \"$(CURDIR)/lisp/jabber-util.el\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m lisp/jabber-util.el\n"
	@for f in lisp/*.el; do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@printf "\033[32mLoaded all lisp/*.el into Emacs\033[0m\n"

clean-elc:
	find . -name '*.elc' -delete
	find . -name '.#*' -delete
	find . -name '#*#' -delete

clean-module:
ifdef GUIX
	guix shell -D -f guix.scm -- $(MAKE) -C src clean
else ifdef NIX_SHELL
	$(NIX_SHELL) '$(MAKE) -C src clean'
else
	$(MAKE) -C src clean
endif

clean: clean-elc clean-module
	rm -rf $(TEST_RESULTS)

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
