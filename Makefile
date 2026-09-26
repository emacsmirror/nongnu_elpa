.PHONY: all build dev autoload module compile lint lint-check-declare lint-checkdoc \
        lint-test-autoloads \
        lint-package-lint lint-relint lint-test-compile lint-byte-comp lint-native-comp lint-compile-check \
        clean clean-elc clean-module install uninstall check test test-oneshot test-debian \
        release-check load native-check do-native-check lint-gate-check do-lint-gate-check \
        do-build do-dev do-compile do-lint do-module do-test do-test-oneshot do-test-summary \
        do-lint-check-declare do-lint-checkdoc do-lint-byte-comp do-lint-native-comp

NIX := $(shell command -v nix 2>/dev/null)

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(JABBER_ENV_WRAPPED)$(IN_NIX_SHELL),)
ifneq ($(wildcard flake.nix),)
ifneq ($(NIX),)
ENV_MAKE = nix develop 'git+file://$(CURDIR)' --command env JABBER_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif
endif

EMACS_CMD ?= emacs
EMACS_OPTS ?= -Q --batch
EMACSCLIENT ?= emacsclient

JOBS         ?= $(shell nproc 2>/dev/null || echo 4)
TEST_RESULTS := .test-results

# Ordinary ERT suites have one inventory, shared by both execution modes.
# Keep TESTS overridable for focused runs.
TESTS ?= $(sort $(wildcard tests/jabber-test-*.el))
LINT_FILES ?= $(wildcard admin/*.el lisp/*.el)

TEST_STAMPS := $(patsubst tests/%.el,$(TEST_RESULTS)/%.stamp,$(TESTS))

all: build

build:
	@$(ENV_MAKE) do-build

do-build: do-compile do-module

dev:
	@$(ENV_MAKE) do-dev

do-dev: do-compile do-module do-lint
	$(MAKE) do-native-check
	$(MAKE) do-test
	$(MAKE) do-test-oneshot

autoload:
	$(EMACS_CMD) $(EMACS_OPTS) -L lisp \
	--eval="(loaddefs-generate \"lisp\" \"lisp/jabber-autoloads.el\")"

module:
	@$(ENV_MAKE) do-module

do-module:
	$(MAKE) -C src

# CBC/store/RNG faults; API simulations do not certify native OS builds.
native-check:
	@$(ENV_MAKE) do-native-check

do-native-check:
	$(MAKE) -C src check

compile:
	@$(ENV_MAKE) do-compile

do-compile: autoload
	$(EMACS_CMD) $(EMACS_OPTS) -L . -L lisp \
	--eval="(setq jabber-db-path nil print-length nil load-prefer-newer t byte-compile-error-on-warn t)" \
	-f batch-byte-compile lisp/*.el

lint-check-declare:
	@$(ENV_MAKE) do-lint-check-declare

# Both declaration checks and their regression fixtures consume native exports.
do-lint-check-declare: do-module
	@set -e; for file in $(LINT_FILES); do \
	  JABBER_LINT_MODE=declare JABBER_LINT_FILE="$$file" \
	  $(EMACS_CMD) $(EMACS_OPTS) -L admin -L lisp -l admin/check-lisp; \
	done

lint-checkdoc:
	@$(ENV_MAKE) do-lint-checkdoc

do-lint-checkdoc:
	@set -e; for file in $(LINT_FILES); do \
	  case "$$file" in lisp/jabber-autoloads.el) continue;; esac; \
	  JABBER_LINT_MODE=checkdoc JABBER_LINT_FILE="$$file" \
	  $(EMACS_CMD) $(EMACS_OPTS) -L admin -L lisp -l admin/check-lisp; \
	done

lint-package-lint:
	@set -e; for file in lisp/*.el; do \
	  $(EMACS_CMD) $(EMACS_OPTS) -l admin/check-package-lint "$$file"; \
	done

lint-relint:
	$(EMACS_CMD) $(EMACS_OPTS) \
	--eval='(package-initialize)' --eval="(require 'relint)" \
	-f 'relint-batch' "lisp"

lint-test-compile:
	$(EMACS_CMD) $(EMACS_OPTS) -L admin -L lisp -L tests \
	--eval="(setq jabber-db-path nil byte-compile-error-on-warn t)" \
	-f batch-byte-compile admin/*.el tests/*.el

lint-test-autoloads:
	@$(EMACS_CMD) $(EMACS_OPTS) --script admin/check-test-autoloads tests/*.el

lint-byte-comp:
	@$(ENV_MAKE) do-lint-byte-comp

do-lint-byte-comp:
	EMACS_CMD="$(EMACS_CMD)" EMACS_OPTS="$(EMACS_OPTS)" ./admin/check-compile byte

lint-native-comp:
	@$(ENV_MAKE) do-lint-native-comp

do-lint-native-comp:
	EMACS_CMD="$(EMACS_CMD)" EMACS_OPTS="$(EMACS_OPTS)" ./admin/check-compile native

lint-compile-check:
	EMACS_CMD="$(EMACS_CMD)" EMACS_OPTS="$(EMACS_OPTS)" ./admin/test-compile-check

lint-gate-check:
	@$(ENV_MAKE) do-lint-gate-check

do-lint-gate-check: do-module
	EMACS_CMD="$(EMACS_CMD)" EMACS_OPTS="$(EMACS_OPTS)" \
	  $(EMACS_CMD) $(EMACS_OPTS) -l admin/test-gates -f ert-run-tests-batch-and-exit

lint:
	@$(ENV_MAKE) do-lint

do-lint: do-lint-check-declare do-lint-checkdoc lint-package-lint lint-relint \
         lint-test-compile lint-test-autoloads do-lint-byte-comp do-lint-native-comp lint-compile-check do-lint-gate-check

# Resolve Thanos' installed fork in the calling environment, never Nix's PATH.
# Other contributors can supply their own installed executable.
THANOS_EMACS ?= emacs

.PHONY: test-matrix test-matrix-runner test-matrix-public
test-matrix:
	@THANOS_EMACS="$(THANOS_EMACS)" MATRIX_TESTS="$(TESTS)" MATRIX_JOBS="$(JOBS)" \
	  python3 admin/test-matrix

test-matrix-runner:
	@python3 admin/test-matrix-runner.py

# Real public-entrypoint controls; needs host Nix and an installed Emacs.
test-matrix-public:
	@python3 admin/test-matrix-public.py --emacs "$(THANOS_EMACS)"

test:
	@$(ENV_MAKE) -j$(JOBS) -Otarget do-test

do-test: autoload do-module
	@rm -rf $(TEST_RESULTS)
	@mkdir -p $(TEST_RESULTS)
	@$(MAKE) --no-print-directory -j$(JOBS) -Otarget do-test-summary

# jabber-db-path is preset to nil so no test can ever open the user's
# real database; tests that need storage let-bind it to a temp file.
$(TEST_RESULTS)/%.stamp: tests/%.el
	@EMACS_CMD="$(EMACS_CMD)" EMACS_OPTS="$(EMACS_OPTS)" \
	  ./admin/run-test "$@" "$<"

test-oneshot:
	@$(ENV_MAKE) do-test-oneshot

test-debian:
	./admin/test-debian

# Run the complete local and Debian gates before version commits and tags.
release-check: dev
	@if [ -n "$(NIX)" ]; then nix flake check; fi
	$(MAKE) test-debian

# Mirror Debian's dh_elpa_test: load every test file into one Emacs
# process and run the whole suite twice.  Surfaces cross-test state
# pollution and in-place mutation of shared literals that the per-file
# `do-test' runs (one Emacs per file) cannot see.
do-test-oneshot: autoload do-module
	@JABBER_TEST_RUNS=2 EMACS_CMD="$(EMACS_CMD)" EMACS_OPTS="$(EMACS_OPTS)" \
	  ./admin/run-test "$(TEST_RESULTS)/oneshot.stamp" $(TESTS)
	@$(EMACS_CMD) $(EMACS_OPTS) -l admin/test-summary "$(TEST_RESULTS)/oneshot.stamp"
	@if [ -z "$$JABBER_MATRIX_EVIDENCE" ]; then \
	  rm -f "$(TEST_RESULTS)/oneshot.stamp" "$(TEST_RESULTS)/oneshot.stamp.ert" "$(TEST_RESULTS)/oneshot.stamp.log"; fi

do-test-summary: $(TEST_STAMPS)
	@$(EMACS_CMD) $(EMACS_OPTS) -l admin/test-summary $(TEST_STAMPS)
	@if [ -z "$$JABBER_MATRIX_EVIDENCE" ]; then rm -rf $(TEST_RESULTS); fi

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
