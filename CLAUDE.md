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

## Testing

```bash
cd test && make test            # whole suite (ert, batch)
cd test && make test-verbose    # with deeper printing
cd test && make test-one testel=vm-imap-test.el
```

Every bug fix ships a regression test in the matching `test/vm-*-test.el`, in
the same commit. **Verify the test actually fails without the fix**: stash the
lisp change, run the test, restore.

Gotchas found the hard way:

- **Delete the stale `.elc` first** (`rm -f lisp/*.elc`). ert loads the
  byte-compiled file in preference to newer source, so a fix-reverted run that
  still passes is usually this, not a bad test.
- **`error` formats through `format-message`**, so expected message strings come
  back with curved quotes. Bind `text-quoting-style` to `'grave` in the test.
- **`vm-interactive-p` is a macro** over `called-interactively-p`. Stubbing
  `(symbol-function 'vm-interactive-p)` does nothing; stub
  `called-interactively-p` instead.
- Tests that reach into folder machinery need `vm-select-folder-buffer-and-validate`,
  `vm-select-operable-messages` and friends stubbed; see `vm-test-with-folder`
  in `test/vm-test-init.el` and the existing stub macros for the pattern.

## Contributing workflow

One branch and one merge request per issue:

```sh
git switch -c issue-NNN-brief-description central/alpha
# work, test, commit with "Closes #NNN" (or "Re #NNN" if it does not resolve it)
git push -o merge_request.create \
         -o merge_request.target_project=emacs-vm/vm \
         -o merge_request.target=alpha \
         -o merge_request.remove_source_branch \
         -u origin issue-NNN-brief-description
```

- **Cut branches from `central/alpha`, never from a local integration branch.**
  A local branch that has other topic branches merged into it silently stacks
  them into the next MR; GitLab then takes the MR title and description from
  the *oldest* commit in the range, so the MR ends up describing — and closing
  — the wrong issue. Check with `git rev-list --count central/alpha..<branch>`.
- `origin` is the personal fork, `central` is `emacs-vm/vm` (project id
  59241204). Issues and merge requests live on `central`; branches go to
  `origin` and the MR is cross-project.
- `alpha` is the integration branch and is not the default branch, so
  merging an MR there does **not** auto-close the issue. That happens when
  `alpha` reaches `main`.
- Editing an existing MR (target, title, description) or labelling and closing
  an issue needs the REST API and a token with `api` scope — push options
  cannot do it.
- An issue investigated but not reproducible gets the `irreproducible` label,
  and is closed too when it is a Launchpad import.

Test files are conflict-prone, since independent branches all append new tests
to the end of the same file. The resolution is always keep-both.

### Issue labels

- `Analyzed` — investigated and commented on, but left open. Use it whenever
  findings are posted without the issue being closed, so a reader can tell an
  answered issue from an untouched one.
- `Pending` — the fix is merged into `alpha` but has not reached `main`, so
  the issue is still open only because merging to `alpha` does not close it.
  The set is derivable: take the `Closes #NNN` / `Re #NNN` trailers of
  `git log central/main..central/alpha`. Do not put it on a closed issue —
  nothing is pending there.
- `irreproducible` — as above.

### Attributing comments written by Claude

The API token belongs to Mark, so anything posted with it appears under his
name. A comment Claude wrote must say so, as its first line:

```
> 🤖 Written by [Claude Code](https://claude.com/claude-code), not by @diekhans, and posted from his account.
```

This is not a formality. These comments state what was and was not
reproduced, and how; a reader deciding whether to trust that needs to know it
came from a tool run rather than from the maintainer's own testing. The same
goes for anything else posted through the API under his account — issue
descriptions, MR descriptions.

Commits carry the equivalent through their `Co-Authored-By:` trailer.

## NEWS

`NEWS` records new functionality and user-visible changes of behaviour —
new commands, renamed or removed variables, changed defaults. **Bug fixes do
not go in NEWS**; that is what the issue tracker is for.

## Architecture

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
