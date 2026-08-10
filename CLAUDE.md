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

**Run both compile lints before committing**, not just the byte one:

```bash
make byte-compile-lint && make native-compile-lint
```

Native compilation reports things the byte compiler does not, and a commit
that passes one and not the other is a commit that breaks a user's build the
first time their Emacs compiles the file.

```bash
make byte-compile-lint   # Byte compile with strict warnings (primary check)
make cross-file-lint     # Each file compiled alone: calls nothing defines
make native-compile-lint # Native compilation check
make package-lint        # Package metadata check (vm.el only)
make relint-lint         # Regular expression linting
```

Note: `make elint-lint` is broken (max-lisp-eval-depth), `make elisp-lint` has many false positives.

**`byte-compile-lint` cannot see a call across files.** It compiles the whole
directory in one Emacs, so compiling `vm-imap.el` teaches that session the
functions `vm-reply.el` goes on to call and the warning never appears. A user's
Emacs native-compiles one file at a time and prints them — which is how seven
went unnoticed until a maintainer pasted them in. `make cross-file-lint`
compiles each file alone and fails on them; the fix is a `declare-function`
beside the others at the top of the file, with the arglist the definition has.

## Testing

```bash
cd test && make test            # whole suite (ert, batch)
cd test && make test-verbose    # with deeper printing
cd test && make test-one testel=vm-imap-test.el
cd test && make test-imap       # live IMAP only (needs a server; skips without one)
cd test && make test-pop        # POP: mock server always, live if configured
cd test && make test-leaks      # report tests that leave global state behind
cd test && make test-assert     # whole suite with VM's own assertions checked
```

`test/vm-fuzz-test.el` drives a folder through random operation sequences and
checks its invariants after each one. It runs a small search as part of the
suite; `VM_FUZZ_SEEDS` and `VM_FUZZ_OPS` make it search harder, and a failure
reports the sequence that caused it.

Every bug fix ships a regression test in the matching `test/vm-*-test.el`, in
the same commit. **Verify the test actually fails without the fix**: stash the
lisp change, run the test, restore. Where a fix removes unreachable code there
is nothing to fail — say so in the commit rather than implying a verification
that cannot exist, and pin the invariant that made it unreachable instead.

Live IMAP and POP tests read `test/vm-live-config.el`, which is gitignored
because it holds account passwords; copy `test/vm-live-config.el.template` and
fill it in. Every live test skips when nothing is configured, so `make test`
works without it.

Each test runs with the global value of every VM variable saved and restored,
and buffers it created killed — see `vm-test-isolate-global-state` in
`test/vm-test-init.el`. Do not rely on state from an earlier test, and do not
assume a test that leaks is harmless: `make test-leaks` shows what is being
leaked, and advice, non-VM hooks and files on disk are *not* restored.

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
- **`cl-letf` cannot stub a `defsubst` against compiled code.** The accessors are
  `defsubst`s, so a caller compiled with the definition in scope has it inlined
  and never looks at the symbol —
  `vm-set-body-to-be-retrieved-of`, `vm-th-parent-of` and
  `vm-select-folder-buffer-and-validate` have all cost time this way. Stub
  something further out, or assert on the effect rather than the call. The same
  applies to macros, `vm-interactive-p` above being one.
- **`vm-assert` does nothing by default.** `vm-assertion-checking-off` defaults
  to t, so an assertion in the code under test is not a check you can rely on in
  the field. It also binds `debug-on-error`, so a test that wants assertions on
  needs `inhibit-debugger` for batch. `make test-assert` runs the whole suite
  with them on and is expected to pass: an assertion that fires there is either
  a broken invariant or a test setting up a state no real caller is in, which is
  what four IMAP tests were doing until they were given a `process` buffer type.

## Contributing workflow

One branch and one merge request per issue:

```sh
git switch -c issue-NNN-brief-description central/develop
# work, test, commit with "Closes #NNN" (or "Re #NNN" if it does not resolve it)
git push -o merge_request.create \
         -o merge_request.target_project=emacs-vm/vm \
         -o merge_request.target=develop \
         -o merge_request.remove_source_branch \
         -u origin issue-NNN-brief-description
```

- **Cut branches from `central/develop`, never from a local integration branch.**
  A local branch that has other topic branches merged into it silently stacks
  them into the next MR; GitLab then takes the MR title and description from
  the *oldest* commit in the range, so the MR ends up describing — and closing
  — the wrong issue. Check with `git rev-list --count central/develop..<branch>`.
- `origin` is the personal fork, `central` is `emacs-vm/vm` (project id
  59241204). Issues and merge requests live on `central`; branches go to
  `origin` and the MR is cross-project.
- `develop` is the integration branch and is not the default branch, so
  merging an MR there does **not** auto-close the issue. That happens when
  `develop` reaches `main`. It was called `alpha` until 2026-08-03; the old
  name suggested a release channel, which it is not.
- Editing an existing MR (target, title, description) or labelling and closing
  an issue needs the REST API and a token with `api` scope — push options
  cannot do it.
- **Wait for `detailed_merge_status` to reach `mergeable` before merging.** For a
  short while after an MR is created GitLab reports `preparing`, and a merge
  request made then fails with a response that is not even JSON. Poll the MR
  until it says `mergeable`, then merge.
- **Decisions and design questions belong in the issue, not only in a file.**
  `dev/docs/design/` is the right place for the long form, but those files live
  on `develop`, which runs a long way ahead of `main` — so they are invisible
  from a `main` checkout and from GitLab's default file view. Put the questions
  themselves in the ticket, which is visible whatever branch anyone is on, and
  reference the file for the detail.
- An issue investigated but not reproducible gets the `irreproducible` label,
  and is closed too when it is a Launchpad import.

Test files are conflict-prone, since independent branches all append new tests
to the end of the same file. The resolution is always keep-both.

### Naming an issue or merge request in a reply

Give the full URL, as plain text, every time an issue or merge request is
mentioned:

```
https://gitlab.com/emacs-vm/vm/-/work_items/453
https://gitlab.com/emacs-vm/vm/-/merge_requests/184
```

`#453` on its own is not clickable, and neither is a Markdown link, whose
target the terminal does not show — so both leave the reader to go and look the
number up. Issues take the `work_items` path, merge requests `merge_requests`.

Give the title too, and when several are mentioned at once make them a list of
URL and title rather than a run of bare numbers:

```
- https://gitlab.com/emacs-vm/vm/-/work_items/38 — Saving to IMAP folders loses attributes
- https://gitlab.com/emacs-vm/vm/-/work_items/270 — IMAP server reliability in storing labels (flags)
```

A bare number says nothing about what the issue is, so a list of them cannot be
read without opening every one.

### Issue labels

- `Analyzed` — investigated and commented on, but left open. Use it whenever
  findings are posted without the issue being closed, so a reader can tell an
  answered issue from an untouched one.
- `Pending` — the fix is merged into `develop` but has not reached `main`, so
  the issue is still open only because merging to `develop` does not close it.
  The set is derivable: take the `Closes #NNN` / `Re #NNN` trailers of
  `git log central/main..central/develop`. Do not put it on a closed issue —
  nothing is pending there. Note that a `Re #NNN` trailer means the commit only
  *mentions* the issue, so it is not on its own grounds for `Pending`; check
  which trailer it was before labelling.
- `Decision Needed` — waiting on a maintainer decision rather than on effort:
  the analysis is on the issue and the next step is a choice. Assign the issue
  to the maintainer as well, so it shows up as theirs and not merely unowned.
  Take it off once the decision is made, whichever way it goes. It was called
  `Decide` until 2026-08-06.

  Put it on an issue whose *next step* is a choice. An aside offering further
  work on an issue that is otherwise finished is not one: nothing is blocked
  there, and labelling those would make the label mean "mentions a choice".

  Label it **and post a comment stating the choice as a checklist**, one
  `- [ ]` per option, with a recommendation. The analysis that led to the
  choice is usually long and the options end up buried in it — on #532 they
  were at the bottom of the description, where the maintainer could not find
  them. A checklist in its own comment is what he acts on, and ticking a box
  is the answer.

  Not the same as `Undecided`, which is Launchpad's imported *importance* field
  and appears only alongside `Launchpad`, next to `Confirmed`, `Incomplete` and
  `In Progress`. Nor the same as `Kick can`, which records a decision already
  taken — to defer. An issue can be both: deferred, and now wanting a second
  look.
- `Close by hand` — `Pending`, but the commit trailer is `Re #NNN` or names
  another issue, so merging `develop` into `main` will not close it. The work
  is done; someone has to close it at that point. Derivable the same way
  `Pending` is: an issue whose only trailers in
  `git log central/main..central/develop` are `Re`, or none at all.
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

### Autoloads

`lisp/vm-autoloads.el` is what a user's init file loads, so it decides what
works before VM itself is loaded.

- **Every command the manual documents carries `;;;###autoload`.** `M-x` has
  to find what the manual tells the reader to type. Twenty-two did not, among
  them `vm-compact-folder` and `vm-toggle-thread`, while 455 others did.
  `vm-reference-test-documented-commands-are-autoloaded` checks it. A function
  the manual names only as a value for an option is not a command and is
  exempt; so are the Personality Crisis conditions and actions, which are
  written into `vmpc-conditions` and `vmpc-actions` and run from there.
- **Autoload nothing from `vm-vars.el`.** Every VM file requires it, so a
  cookie there gains nothing, and an autoloaded default that reads another
  variable puts that value form in the loaddefs, where the variable may not
  exist yet. That is how issue #608 broke startup with "Symbol's value as
  variable is void: vm-included-text-prefix".
- **An alias needs the explicit autoload form,** not a bare cookie:

  ```elisp
  ;;;###autoload (autoload 'vm-compact-folder "vm-delete" nil t)
  (defalias 'vm-compact-folder 'vm-expunge-folder)
  ```

  A bare `;;;###autoload` copies the whole `defalias` into
  `lisp/vm-autoloads.el`, so that is where `symbol-file` says the alias is
  defined — and the manual's appendix is built by asking `symbol-file` and
  skipping the generated files, so the command silently drops out of the
  manual. Four did. `vm-reference-test-no-command-is-attributed-to-a-generated-file`
  checks it. Seven of VM's autoloaded commands are aliases.
- Tests of loaddefs behaviour need a subprocess Emacs: the suite's own Emacs
  has all of VM loaded, so it cannot tell an autoloaded command from a loaded
  one, and could never see a loaddefs file that fails to load on its own.

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
