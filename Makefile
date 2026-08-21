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
EMACSCLIENT ?= emacsclient
KEYMAP_POPUP ?=

export EMACSCLIENT

SRCS = lisp/hermes-promise.el lisp/hermes-notifications.el lisp/hermes-session-title.el lisp/hermes-transport.el lisp/hermes-transport-cli.el lisp/hermes-dashboard-api.el lisp/hermes-dashboard-transport.el lisp/hermes-dashboard-rpc.el lisp/hermes-chat-format.el lisp/hermes-chat-render.el lisp/hermes-chat-buffer.el lisp/hermes-chat-prompts.el lisp/hermes-chat-dashboard.el lisp/hermes-chat-models.el lisp/hermes-chat-handoff.el lisp/hermes-chat-slash.el lisp/hermes-chat.el lisp/hermes-browser.el lisp/hermes-sessions.el lisp/hermes-inventory.el lisp/hermes-rollback.el lisp/hermes-subagents.el lisp/hermes-cron.el lisp/hermes-profiles.el lisp/hermes-messaging.el lisp/hermes-kanban-log.el lisp/hermes-kanban-events.el lisp/hermes-kanban.el lisp/hermes-mcp.el lisp/hermes-config.el lisp/hermes-system.el lisp/hermes-command-palette.el lisp/hermes-exec.el lisp/hermes-onboarding.el lisp/hermes-capabilities.el lisp/hermes.el
TEST_SUPPORT = tests/hermes-test-helpers.el
TESTS = tests/hermes-notifications-tests.el tests/hermes-transport-tests.el tests/hermes-chat-tests.el tests/hermes-chat-handoff-tests.el tests/hermes-chat-models-tests.el tests/hermes-chat-prompts-tests.el tests/hermes-chat-dashboard-tests.el tests/hermes-chat-reducer-tests.el \
	tests/hermes-dashboard-tests.el tests/hermes-ui-tests.el tests/hermes-kanban-tests.el \
	tests/hermes-cron-tests.el tests/hermes-mcp-tests.el tests/hermes-config-tests.el \
	tests/hermes-system-tests.el \
	tests/hermes-command-palette-tests.el \
	tests/hermes-inventory-tests.el tests/hermes-sessions-tests.el \
	tests/hermes-messaging-tests.el \
	tests/hermes-browsers-tests.el tests/hermes-exec-tests.el \
	tests/hermes-promise-tests.el tests/hermes-onboarding-tests.el \
	tests/hermes-capabilities-tests.el

SELECTOR ?= t
ERT_OPTS ?=

LOAD_PATH = -L lisp -L tests $(if $(KEYMAP_POPUP),-L $(KEYMAP_POPUP))
BATCH = $(EMACS_CMD) -Q --batch $(LOAD_PATH)
HERMES_CLIENT_LIVE_ABI = (and (fboundp 'hermes-dashboard-transport-client-p) (mapcar (function car) (cl-struct-slot-info 'hermes-dashboard-transport-client)))

.PHONY: all verify-sources compile do-compile test do-test test-load lint do-lint native-comp do-native-comp dev check pre-commit pre-handoff-check load do-load clean

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

test-load:
	@test_root=$$(mktemp -d "$(CURDIR)/.test-load.XXXXXX") || exit 1; \
	  test_dir=$${test_root#$(CURDIR)/}; \
	  home="$$test_root/home"; server=hermes-load-$${test_root##*.}; \
	  before="$$test_root/before.el"; fail="$$test_root/fail.el"; \
	  after="$$test_root/after.el"; \
	  api_src=lisp/hermes-dashboard-api.el; \
	  before_src="$$test_dir/before.el"; fail_src="$$test_dir/fail.el"; \
	  after_src="$$test_dir/after.el"; conflict="$$test_root/conflict.mk"; \
	  cleanup () { \
	    HOME="$$home" emacsclient -s "$$server" --eval '(kill-emacs)' \
	      > /dev/null 2>&1 || true; \
	    rm -rf "$$test_root"; \
	  }; \
	  trap cleanup 0 1 2 15; \
	  mkdir -p "$$home" || exit 1; \
	  printf '%s\n' "(setq hermes-review-before 'loaded)" > "$$before" || exit 1; \
	  printf '%s\n' '(error "intentional load failure")' > "$$fail" || exit 1; \
	  printf '%s\n' "(setq hermes-review-after 'loaded)" > "$$after" || exit 1; \
	  printf '%s\n' 'EMACSCLIENT := false' > "$$conflict" || exit 1; \
	  HOME="$$home" $(EMACS_CMD) -Q --daemon="$$server" \
	    > "$$test_root/daemon-output" 2>&1 || exit 1; \
	  HOME="$$home" emacsclient -s "$$server" --eval \
	    "(progn (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	            (require 'hermes-dashboard-api))" > /dev/null || exit 1; \
	  client="env HOME=$$home emacsclient -s $$server"; \
	  MAKEFILES="$$conflict" $(MAKE) --no-print-directory do-load \
	    EMACSCLIENT="$$client" SRCS="$$api_src $$before_src $$after_src" \
	    > "$$test_root/success-output" 2>&1 || exit 1; \
	  state=$$(HOME="$$home" emacsclient -s "$$server" --eval \
	    '(list hermes-review-before hermes-review-after)') || exit 1; \
	  test "$$state" = '(loaded loaded)' || exit 1; \
	  HOME="$$home" emacsclient -s "$$server" --eval \
	    "(mapc (lambda (symbol) (when (boundp symbol) (makunbound symbol))) \
	           '(hermes-review-before hermes-review-after))" > /dev/null || exit 1; \
	  if MAKEFILES="$$conflict" $(MAKE) --no-print-directory do-load \
	      EMACSCLIENT="$$client" \
	      SRCS="$$api_src $$before_src $$fail_src $$after_src" \
	      > "$$test_root/failure-output" 2>&1; then \
	    echo "failing module load unexpectedly succeeded"; exit 1; \
	  fi; \
	  state=$$(HOME="$$home" emacsclient -s "$$server" --eval \
	    '(list (boundp (quote hermes-review-before)) \
	           (boundp (quote hermes-review-after)))') || exit 1; \
	  test "$$state" = '(nil nil)' || exit 1; \
	  grep -q 'intentional load failure' "$$test_root/failure-output" || exit 1; \
	  HOME="$$home" emacsclient -s "$$server" --eval \
	    "(eval '(cl-defstruct hermes-dashboard-transport-client incompatible))" \
	    > /dev/null || exit 1; \
	  if MAKEFILES="$$conflict" $(MAKE) --no-print-directory do-load \
	      EMACSCLIENT="$$client" SRCS="$$api_src $$before_src $$after_src" \
	      > "$$test_root/abi-output" 2>&1; then \
	    echo "incompatible client layout unexpectedly loaded"; exit 1; \
	  fi; \
	  state=$$(HOME="$$home" emacsclient -s "$$server" --eval \
	    '(list (boundp (quote hermes-review-before)) \
	           (boundp (quote hermes-review-after)))') || exit 1; \
	  test "$$state" = '(nil nil)' || exit 1; \
	  grep -q 'Hermes client layout changed' "$$test_root/abi-output" || exit 1

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

check: verify-sources test-load dev

pre-commit: verify-sources test-load
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
	@$(ENV_MAKE) do-load

do-load:
	@client_abi=$$($(BATCH) $(foreach file,$(SRCS),-l $(file)) \
	    --eval "(prin1 (mapcar (function car) \
	      (cl-struct-slot-info (quote hermes-dashboard-transport-client))))") || exit 1; \
	$(EMACSCLIENT) --eval "(progn \
	  (require 'cl-lib) \
	  (require 'subr-x) \
	  (let* ((source-abi '$$client_abi) \
	         (live-abi $(HERMES_CLIENT_LIVE_ABI))) \
	    (when (and live-abi (not (equal source-abi live-abi))) \
	      (error \"Hermes client layout changed; restart Emacs before make load\")) \
	    (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	    (mapatoms (lambda (symbol) \
	      (when (and (string-prefix-p \"hermes-\" (symbol-name symbol)) \
	                 (boundp symbol) (keymapp (symbol-value symbol))) \
	        (makunbound symbol)))) \
	    (mapc (lambda (file) \
	            (load-file (expand-file-name file \"$(CURDIR)\"))) \
	          '($(foreach file,$(SRCS),\"$(file)\"))) \
	    (dolist (buf (buffer-list)) \
	      (with-current-buffer buf \
	        (let ((map (intern-soft (format \"%s-map\" major-mode)))) \
	          (when (and (string-prefix-p \"hermes-\" (symbol-name major-mode)) \
	                     map (boundp map) (keymapp (symbol-value map))) \
	            (use-local-map (symbol-value map)))) \
	        (when (and (derived-mode-p 'hermes-kanban-log-mode) \
	                   (fboundp 'hermes-kanban-log--refontify-buffer)) \
	          (hermes-kanban-log--refontify-buffer))))))" > /dev/null || exit 1; \
	printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc lisp/*.elc tests/*.elc
