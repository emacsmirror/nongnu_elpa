.POSIX:

NIX := $(shell command -v nix 2>/dev/null)

-include local.mk

ENV ?= nix
ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(CODEX_ENV_WRAPPED),)
ifeq ($(ENV),nix)
ifneq ($(NIX),)
ENV_MAKE = nix develop --no-write-lock-file path:$(CURDIR) --command env CODEX_ENV_WRAPPED=1 ENV=$(ENV) $(MAKE) --no-print-directory
endif
endif
endif

EMACS ?= emacs
EMACS_CMD ?= $(EMACS)
KEYMAP_POPUP ?=

SRCS = lisp/codex-ide-debug.el lisp/codex-ide-term.el \
       lisp/codex-ide-context.el lisp/codex-ide-diff.el \
       lisp/codex-ide-mcp.el lisp/codex-ide-appserver.el \
       lisp/codex-ide.el lisp/codex-ide-menu.el

TESTS = tests/codex-ide-tests.el tests/codex-ide-context-tests.el \
        tests/codex-ide-mcp-tests.el tests/codex-ide-appserver-tests.el \
        tests/codex-ide-diff-tests.el

SELECTOR ?= t
ERT_OPTS ?=

LOAD_PATH = -L lisp -L tests $(if $(KEYMAP_POPUP),-L $(KEYMAP_POPUP))
BATCH = $(EMACS_CMD) -Q --batch $(LOAD_PATH)

.PHONY: all compile do-compile compile-tests do-compile-tests test do-test lint do-lint native-comp do-native-comp dev do-dev check pre-commit pre-handoff-check load do-load clean

all: compile

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	    -f batch-byte-compile $$f || exit 1; \
	done

compile-tests:
	@$(ENV_MAKE) do-compile-tests

do-compile-tests:
	@for f in $(TESTS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l ert -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(ENV_MAKE) clean do-compile-tests do-test

do-test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(BATCH) -l ert $(ERT_OPTS) -l $$f \
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
	@$(ENV_MAKE) clean do-dev

do-dev: do-compile do-lint do-compile-tests do-test

check: dev

pre-commit:
	git diff --check
	@$(ENV_MAKE) do-compile do-lint do-native-comp do-compile-tests do-test

pre-handoff-check:
	git status --short --branch
	git diff --check
	nix --extra-experimental-features 'nix-command flakes' \
	  develop --no-write-lock-file path:$(CURDIR) --command env CODEX_ENV_WRAPPED=1 $(MAKE) --no-print-directory check
	nix --extra-experimental-features 'nix-command flakes' \
	  flake check --no-write-lock-file

load:
	@$(ENV_MAKE) do-load

do-load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (mapatoms (lambda (s) \
	    (when (and (string-prefix-p \"codex-ide-\" (symbol-name s)) \
	               (boundp s) (keymapp (symbol-value s))) \
	      (makunbound s)))))" > /dev/null
	@for f in $(SRCS); do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@emacsclient --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (let ((map (intern-soft (format \"%s-map\" major-mode)))) \
	      (when (and (string-prefix-p \"codex-ide-\" (symbol-name major-mode)) \
	                 map (boundp map) (keymapp (symbol-value map))) \
	        (use-local-map (symbol-value map))))))" > /dev/null
	@emacsclient --eval "(when (and (boundp 'codex-ide--processes) \
	                                (fboundp 'codex-ide--setup-terminal-keybindings)) \
	  (maphash (lambda (directory process) \
	             (when (process-live-p process) \
	               (let ((buffer (get-buffer (codex-ide--get-buffer-name directory)))) \
	                 (when buffer \
	                   (with-current-buffer buffer \
	                     (codex-ide--setup-terminal-keybindings)))))) \
	           codex-ide--processes))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc lisp/*.elc tests/*.elc
