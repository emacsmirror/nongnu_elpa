.PHONY: all build dev autoload module compile lint lint-check-declare lint-checkdoc \
        lint-package-lint lint-relint lint-test-compile lint-native-comp \
        clean clean-elc clean-module install uninstall check test test-oneshot load \
        do-build do-dev do-compile do-lint do-module do-test do-test-oneshot do-test-summary \
        do-lint-check-declare do-lint-checkdoc do-lint-native-comp

NIX := $(shell command -v nix 2>/dev/null)

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(JABBER_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop path:$(CURDIR) --command env JABBER_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS_CMD ?= emacs
EMACS_OPTS ?= -Q --batch
EMACSCLIENT ?= emacsclient

JOBS         ?= $(shell nproc 2>/dev/null || echo 4)
TEST_RESULTS := .test-results

TESTS ?= tests/jabber-test-activity.el \
         tests/jabber-test-avatar.el \
         tests/jabber-test-bookmarks.el \
         tests/jabber-test-carbons.el \
         tests/jabber-test-chat.el \
         tests/jabber-test-chatbuffer.el \
         tests/jabber-test-chatstates.el \
         tests/jabber-test-conn.el \
         tests/jabber-test-csi.el \
         tests/jabber-test-db.el \
         tests/jabber-test-disco.el \
         tests/jabber-test-httpupload.el \
         tests/jabber-test-image.el \
         tests/jabber-test-keepalive.el \
         tests/jabber-test-mam.el \
         tests/jabber-test-menu.el \
         tests/jabber-test-message-correct.el \
         tests/jabber-test-message-reply.el \
         tests/jabber-test-modeline.el \
         tests/jabber-test-moderation.el \
         tests/jabber-test-muc.el \
         tests/jabber-test-notifications.el \
         tests/jabber-test-omemo-message.el \
         tests/jabber-test-omemo-module.el \
         tests/jabber-test-omemo-protocol.el \
         tests/jabber-test-omemo-store.el \
         tests/jabber-test-omemo-trust.el \
         tests/jabber-test-openpgp.el \
         tests/jabber-test-openpgp-legacy.el \
         tests/jabber-test-presence.el \
         tests/jabber-test-pubsub.el \
         tests/jabber-test-vcard-avatars.el \
         tests/jabber-test-reactions.el \
         tests/jabber-test-receipts.el \
         tests/jabber-test-reload.el \
         tests/jabber-test-roster.el \
         tests/jabber-test-sm.el \
         tests/jabber-test-srv.el \
         tests/jabber-test-styling.el \
         tests/jabber-test-time.el \
         tests/jabber-test-util.el \
         tests/jabber-test-xml.el

TEST_STAMPS := $(patsubst tests/%.el,$(TEST_RESULTS)/%.stamp,$(TESTS))

all: build

build:
	@$(ENV_MAKE) do-build

do-build: do-compile do-module

dev:
	@$(ENV_MAKE) do-dev

do-dev: do-compile do-module do-lint
	$(MAKE) do-test
	$(MAKE) do-test-oneshot

autoload:
	$(EMACS_CMD) $(EMACS_OPTS) -L lisp \
	--eval="(loaddefs-generate \"lisp\" \"lisp/jabber-autoloads.el\")"

module:
	@$(ENV_MAKE) do-module

do-module:
	$(MAKE) -C src

compile:
	@$(ENV_MAKE) do-compile

do-compile: autoload
	$(EMACS_CMD) $(EMACS_OPTS) -L . -L lisp \
	--eval="(setq print-length nil load-prefer-newer t)" \
	-f batch-byte-compile lisp/*.el

lint-check-declare:
	@$(ENV_MAKE) do-lint-check-declare

do-lint-check-declare:
	for file in admin/*.el lisp/*.el ; do \
	$(EMACS_CMD) $(EMACS_OPTS) --eval="(check-declare-file \"$$file\")" ; \
	done

lint-checkdoc:
	@$(ENV_MAKE) do-lint-checkdoc

do-lint-checkdoc:
	for file in admin/*.el lisp/*.el ; do \
	case "$$file" in lisp/jabber-autoloads.el) continue;; esac; \
	$(EMACS_CMD) $(EMACS_OPTS) --eval="(checkdoc-file \"$$file\")" ; \
	done

lint-package-lint:
	$(EMACS_CMD) $(EMACS_OPTS) \
	--eval='(package-initialize)' --eval="(require 'package-lint)" \
	--eval="(setq package-lint-main-file \"lisp/jabber.el\")" \
        -f 'package-lint-batch-and-exit' $(wildcard lisp/*.el)

lint-relint:
	$(EMACS_CMD) $(EMACS_OPTS) \
	--eval='(package-initialize)' --eval="(require 'relint)" \
	-f 'relint-batch' "lisp"

lint-test-compile:
	$(EMACS_CMD) $(EMACS_OPTS) -L admin -L lisp -L tests \
	-f batch-byte-compile admin/*.el tests/*.el

lint-native-comp: autoload
	@$(ENV_MAKE) do-lint-native-comp

do-lint-native-comp:
	@fails=0; \
	for file in lisp/*.el ; do \
	  case "$$file" in *autoloads*) continue;; esac; \
	  output=$$($(EMACS_CMD) $(EMACS_OPTS) -L lisp \
	    --eval="(native-compile \"$$file\")" 2>&1); \
	  matched=$$(echo "$$output" | grep "is not known to be defined" || true); \
	  if [ -n "$$matched" ]; then \
	    echo "$$matched"; \
	    fails=1; \
	  fi; \
	done; \
	exit $$fails

lint:
	@$(ENV_MAKE) do-lint

do-lint: do-lint-check-declare do-lint-checkdoc lint-package-lint lint-relint lint-test-compile

test:
	@$(ENV_MAKE) -j$(JOBS) -Otarget do-test

do-test: autoload do-module
	@rm -rf $(TEST_RESULTS)
	@mkdir -p $(TEST_RESULTS)
	@$(MAKE) --no-print-directory -j$(JOBS) -Otarget do-test-summary

# jabber-db-path is preset to nil so no test can ever open the user's
# real database; tests that need storage let-bind it to a temp file.
$(TEST_RESULTS)/%.stamp: tests/%.el
	@output=$$($(EMACS_CMD) $(EMACS_OPTS) -L admin -L lisp -L tests \
	  --eval="(setq jabber-db-path nil)" \
	  -l ert -l $< -f ert-run-tests-batch-and-exit 2>&1); \
	rc=$$?; \
	n=$$(echo "$$output" | grep -o 'Ran [0-9]*' | grep -o '[0-9]*'); \
	if [ $$rc -ne 0 ]; then \
	  printf "\033[31mFAIL\033[0m $< ($${n:-0} tests)\n"; \
	  echo "$$output" | grep '  FAILED'; \
	  printf "FAIL %s\n" "$${n:-1}" > $@; \
	else \
	  printf "\033[32m  OK\033[0m $< ($$n tests)\n"; \
	  printf "OK %s\n" "$$n" > $@; \
	fi

test-oneshot:
	@$(ENV_MAKE) do-test-oneshot

# Mirror Debian's dh_elpa_test: load every test file into one Emacs
# process and run the whole suite twice.  Surfaces cross-test state
# pollution and in-place mutation of shared literals that the per-file
# `do-test' runs (one Emacs per file) cannot see.
do-test-oneshot: autoload do-module
	$(EMACS_CMD) $(EMACS_OPTS) -L admin -L lisp -L tests -l ert \
	  --eval="(setq jabber-db-path nil)" \
	  --eval="(require 'jabber)" \
	  $(addprefix -l ,$(TESTS)) \
	  --eval="(let ((bad 0)) \
	            (dotimes (_ 2) \
	              (setq bad (+ bad (ert-stats-completed-unexpected \
	                                (ert-run-tests-batch t))))) \
	            (kill-emacs (if (zerop bad) 0 1)))"

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
	@$(EMACSCLIENT) --eval "(progn \
	  (load-file \"$(CURDIR)/admin/jabber-reload.el\") \
	  (jabber-reload \"$(CURDIR)\"))" > /dev/null
	@printf "\033[32mLoaded all lisp/*.el into Emacs\033[0m\n"

clean-elc:
	find . -name '*.elc' -delete
	find . -name '.#*' -delete
	find . -name '#*#' -delete

clean-module:
	$(MAKE) -C src clean

clean: clean-elc clean-module
	rm -rf $(TEST_RESULTS)

prefix      ?= /usr/local
datarootdir ?= $(prefix)/share
lispdir     ?= $(datarootdir)/emacs/site-lisp/jabber

check:
	$(MAKE) test
	$(MAKE) test-oneshot

install: build
	install -d $(DESTDIR)$(lispdir)
	install -m 644 lisp/*.el $(DESTDIR)$(lispdir)/
	-install -m 644 lisp/*.elc $(DESTDIR)$(lispdir)/
	-install -m 755 lisp/jabber-omemo-core.so $(DESTDIR)$(lispdir)/

uninstall:
	rm -rf $(DESTDIR)$(lispdir)
