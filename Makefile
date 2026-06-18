.POSIX:

NIX := $(shell command -v nix 2>/dev/null)

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(HERMES_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop --no-write-lock-file path:$(CURDIR) --command env HERMES_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS ?= emacs
EMACS_CMD ?= $(EMACS)
KEYMAP_POPUP ?=

SRCS = lisp/hermes-transport.el lisp/hermes-dashboard-transport.el lisp/hermes-chat.el lisp/hermes-sessions.el lisp/hermes-inventory.el lisp/hermes-rollback.el lisp/hermes-subagents.el lisp/hermes-cron.el lisp/hermes-kanban.el lisp/hermes.el
TESTS = tests/hermes-tests.el

SELECTOR ?= t
ERT_OPTS ?=

LOAD_PATH = -L lisp -L tests $(if $(KEYMAP_POPUP),-L $(KEYMAP_POPUP))
BATCH = $(EMACS_CMD) -Q --batch $(LOAD_PATH)

.PHONY: all compile do-compile test do-test lint do-lint dev check pre-handoff-check load clean

all: compile

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(ENV_MAKE) do-test

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
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

dev:
	@$(ENV_MAKE) do-compile do-lint do-test

check: dev

pre-handoff-check:
	git status --short --branch
	git diff --check
	nix --extra-experimental-features 'nix-command flakes' \
	  develop --no-write-lock-file path:$(CURDIR) --command env HERMES_ENV_WRAPPED=1 $(MAKE) --no-print-directory check
	nix --extra-experimental-features 'nix-command flakes' \
	  flake check --no-write-lock-file

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (dolist (sym '(hermes-dashboard-mode-map hermes-chat-mode-map)) \
	    (when (boundp sym) (makunbound sym))))" > /dev/null
	@for f in $(SRCS); do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@emacsclient --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (cond ((derived-mode-p 'hermes-dashboard-mode) \
	           (use-local-map hermes-dashboard-mode-map)) \
	          ((derived-mode-p 'hermes-chat-mode) \
	           (use-local-map hermes-chat-mode-map)))))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc lisp/*.elc tests/*.elc
