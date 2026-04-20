# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VM (View Mail) is an Emacs mail reader supporting GNU Emacs 28.1+ and XEmacs. It handles POP/IMAP servers, MIME, UNIX mailbox format, and BABYL format. Features include virtual folders for searching and multi-folder management.

## Build Commands

```bash
# Configure (run first, or after configure.ac changes)
./configure                                    # Default GNU Emacs
./configure --with-emacs=xemacs               # For XEmacs
./configure --with-other-dirs=/path/to/bbdb   # Include external libs

# Build
make                    # Compile lisp, info docs, pixmaps

# Install
make install            # Install to configured prefix

# Clean
make clean              # Remove compiled files
make distclean          # Full cleanup including Makefile
```

## Linting

```bash
make byte-compile-lint   # Byte compile with strict warnings (primary check)
make native-compile-lint # Native compilation check
make package-lint        # Package metadata check (vm.el only)
make relint-lint         # Regular expression linting
```

Note: `make elint-lint` is broken (max-lisp-eval-depth), `make elisp-lint` has many false positives.

## Architecture

### Module Organization (lisp/)

The 54 Elisp modules follow clear functional separation:

**Entry Point:** `vm.el` - Main package, requires core modules

**Core Subsystems:**
- `vm-folder.el` (5.5k lines) - Folder management, buffer handling
- `vm-mime.el` (7.9k lines) - MIME parsing and encoding, largest module
- `vm-vars.el` (7.4k lines) - All defcustom/defvar declarations
- `vm-imap.el` (4.8k lines) - IMAP protocol implementation
- `vm-pop.el` - POP3 protocol implementation

**UI Layer:**
- `vm-summary.el` - Message list display
- `vm-page.el` / `vm-motion.el` - Message viewing and navigation
- `vm-menu.el` / `vm-toolbar.el` / `vm-mouse.el` - Interactive elements
- `vm-window.el` - Window/frame management

**Operations:**
- `vm-reply.el` - Composing replies
- `vm-delete.el` / `vm-save.el` / `vm-mark.el` - Message operations
- `vm-sort.el` / `vm-search.el` - Sorting and searching
- `vm-virtual.el` / `vm-avirtual.el` - Virtual folder implementation

**Build System:**
- `vm-build.el` - Compilation harness
- `vm-autoloads.el` - Generated autoloads (do not edit)
- `vm-cus-load.el` - Generated custom groups (do not edit)
- `vm-version-conf.el` - Generated version info (do not edit)

### Generated Files

These files are auto-generated during build - do not edit directly:
- `lisp/vm-autoloads.el`
- `lisp/vm-cus-load.el`
- `lisp/vm-version-conf.el`
- `Makefile` (from Makefile.in via configure)
- `vm-load.el` (from vm-load.el.in)

Edit the `.in` templates or `configure.ac` instead.

### Design Documentation

Architecture docs in `dev/docs/design/`:
- Virtual folder implementation
- Threading design
- Password handling
- Folder data structures
- **async-imap.org** - Planned async IMAP refactor (IMAP currently blocks Emacs)

### Known Issues

**IMAP blocks Emacs**: `vm-imap.el` uses synchronous `accept-process-output` in loops. See `dev/docs/design/async-imap.org` for the planned fix using CPS with process filters.

## Development Notes

- Byte-compiled files are NOT compatible between GNU Emacs and XEmacs
- Run from build directory by adding `lisp/` to load-path and requiring `vm-autoloads`
- Companion packages: BBDB (address book), emacs-w3m/w3 (HTML rendering)
- Bug reports: https://gitlab.com/emacs-vm/vm/-/issues
