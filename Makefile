.POSIX:

NIX := $(shell command -v nix 2>/dev/null)

-include local.mk

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(HERMES_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop --no-write-lock-file . --command env HERMES_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS ?= emacs
EMACS_CMD ?= $(EMACS)
KEYMAP_POPUP ?=

SRCS = lisp/hermes-promise.el lisp/hermes-notifications.el lisp/hermes-session-title.el lisp/hermes-transport.el lisp/hermes-transport-cli.el lisp/hermes-dashboard-api.el lisp/hermes-dashboard-transport.el lisp/hermes-dashboard-rpc.el lisp/hermes-chat-format.el lisp/hermes-chat-render.el lisp/hermes-chat-buffer.el lisp/hermes-chat-prompts.el lisp/hermes-chat-dashboard.el lisp/hermes-chat-models.el lisp/hermes-chat-handoff.el lisp/hermes-chat-slash.el lisp/hermes-chat.el lisp/hermes-browser.el lisp/hermes-sessions.el lisp/hermes-inventory.el lisp/hermes-rollback.el lisp/hermes-subagents.el lisp/hermes-cron.el lisp/hermes-profiles.el lisp/hermes-kanban-log.el lisp/hermes-kanban-events.el lisp/hermes-kanban.el lisp/hermes-mcp.el lisp/hermes-config.el lisp/hermes-system.el lisp/hermes-command-palette.el lisp/hermes-exec.el lisp/hermes-onboarding.el lisp/hermes-capabilities.el lisp/hermes.el
TEST_SUPPORT = tests/hermes-test-helpers.el
TESTS = tests/hermes-notifications-tests.el tests/hermes-transport-tests.el tests/hermes-chat-tests.el tests/hermes-chat-handoff-tests.el tests/hermes-chat-models-tests.el tests/hermes-chat-prompts-tests.el tests/hermes-chat-dashboard-tests.el tests/hermes-chat-reducer-tests.el \
	tests/hermes-dashboard-tests.el tests/hermes-ui-tests.el tests/hermes-kanban-tests.el \
	tests/hermes-cron-tests.el tests/hermes-mcp-tests.el tests/hermes-config-tests.el \
	tests/hermes-system-tests.el \
	tests/hermes-command-palette-tests.el \
	tests/hermes-inventory-tests.el tests/hermes-sessions-tests.el \
	tests/hermes-browsers-tests.el tests/hermes-exec-tests.el \
	tests/hermes-promise-tests.el tests/hermes-onboarding-tests.el \
	tests/hermes-capabilities-tests.el

SELECTOR ?= t
ERT_OPTS ?=

LOAD_PATH = -L lisp -L tests $(if $(KEYMAP_POPUP),-L $(KEYMAP_POPUP))
BATCH = $(EMACS_CMD) -Q --batch $(LOAD_PATH)

.PHONY: all verify-sources compile do-compile test do-test lint do-lint native-comp do-native-comp dev check pre-commit pre-handoff-check load clean

all: compile

verify-sources:
	@set -eu; \
	  srcs=$$(mktemp); lisp=$$(mktemp); tests=$$(mktemp); tree_tests=$$(mktemp); \
	  trap 'rm -f "$$srcs" "$$lisp" "$$tests" "$$tree_tests"' 0 1 2 15; \
	  printf '%s\n' $(SRCS) | sed 's|^\./||' | sort > "$$srcs"; \
	  printf '%s\n' lisp/*.el | sed 's|^\./||' | sort > "$$lisp"; \
	  printf '%s\n' $(TESTS) $(TEST_SUPPORT) | sed 's|^\./||' | sort > "$$tests"; \
	  printf '%s\n' tests/*.el | sed 's|^\./||' | sort > "$$tree_tests"; \
	  if ! cmp -s "$$srcs" "$$lisp"; then \
	    echo "SRCS must match lisp/*.el exactly (normalized set equality):"; \
	    diff -u "$$lisp" "$$srcs" || true; \
	    exit 1; \
	  fi; \
	  if ! cmp -s "$$tests" "$$tree_tests"; then \
	    echo "TESTS + TEST_SUPPORT must match tests/*.el exactly (normalized set equality):"; \
	    diff -u "$$tree_tests" "$$tests" || true; \
	    exit 1; \
	  fi

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	    -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(ENV_MAKE) do-test

do-test:
	@test_root=$$(mktemp -d) || exit 1; \
	  trap 'rm -rf "$$test_root"' 0 1 2 15; \
	  mkdir -p "$$test_root/home" "$$test_root/cache" \
	    "$$test_root/config" "$$test_root/data" "$$test_root/state" \
	    "$$test_root/tmp" || exit 1; \
	  run_isolated () { \
	    env -i HOME="$$test_root/home" \
	      XDG_CACHE_HOME="$$test_root/cache" \
	      XDG_CONFIG_HOME="$$test_root/config" \
	      XDG_DATA_HOME="$$test_root/data" \
	      XDG_STATE_HOME="$$test_root/state" \
	      TMPDIR="$$test_root/tmp" \
	      LANG=C.UTF-8 PATH="$$PATH" TERM=dumb "$$@"; \
	  }; \
	  run_isolated $(BATCH) --eval \
	    '(when (or (getenv "HERMES_DASHBOARD_SESSION_TOKEN") (getenv "EMACS_EXEC_TOKEN")) (error "Test environment contains Hermes credentials"))' || exit 1; \
	  for f in $(TESTS); do \
	    echo "Testing $$f..."; \
	    run_isolated $(BATCH) -l ert $(ERT_OPTS) -l $$f \
	      --eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))' || exit 1; \
	  done

lint:
	@$(ENV_MAKE) do-lint

do-lint:
	@echo "Running checkdoc..."
	@for f in $(SRCS); do \
	  out=$$($(BATCH) --eval "(checkdoc-file \"$$f\")" 2>&1); \
	  if [ -n "$$out" ]; then echo "$$out"; echo "checkdoc warnings in $$f"; exit 1; fi; \
	done

native-comp:
	@$(ENV_MAKE) do-native-comp

do-native-comp:
	@for f in $(SRCS); do \
	  echo "Native-compiling $$f..."; \
	  out=$$($(BATCH) --eval "(native-compile \"$$f\")" 2>&1); \
	  warn=$$(printf '%s\n' "$$out" | grep -E "Warning:|is not known to be defined" || true); \
	  if [ -n "$$warn" ]; then echo "$$warn"; echo "native-comp warnings in $$f"; exit 1; fi; \
	done

dev:
	@$(ENV_MAKE) do-compile do-lint do-test

check: verify-sources dev

pre-commit: verify-sources
	git diff --check
	@$(ENV_MAKE) do-compile do-lint do-native-comp do-test

pre-handoff-check:
	git status --short --branch
	git diff --check
	nix --extra-experimental-features 'nix-command flakes' \
	  develop --no-write-lock-file . --command env HERMES_ENV_WRAPPED=1 $(MAKE) --no-print-directory check
	nix --extra-experimental-features 'nix-command flakes' \
	  flake check --all-systems --no-build --no-write-lock-file
	@system=$$(nix --extra-experimental-features 'nix-command flakes' eval --impure --raw --expr builtins.currentSystem); \
	  nix --extra-experimental-features 'nix-command flakes' \
	    build --no-link --no-write-lock-file \
	    ".#checks.$$system.package" \
	    ".#checks.$$system.package-smoke"

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (mapatoms (lambda (s) \
	    (when (and (string-prefix-p \"hermes-\" (symbol-name s)) \
	               (boundp s) (keymapp (symbol-value s))) \
	      (makunbound s)))))" > /dev/null
	@for f in $(SRCS); do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@emacsclient --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (let ((map (intern-soft (format \"%s-map\" major-mode)))) \
	      (when (and (string-prefix-p \"hermes-\" (symbol-name major-mode)) \
	                 map (boundp map) (keymapp (symbol-value map))) \
	        (use-local-map (symbol-value map)))) \
	    (when (and (derived-mode-p 'hermes-kanban-log-mode) \
	               (fboundp 'hermes-kanban-log--refontify-buffer)) \
	      (hermes-kanban-log--refontify-buffer))))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc lisp/*.elc tests/*.elc
