# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VM (View Mail) is an Emacs mail reader supporting GNU Emacs 28.1+. It handles POP/IMAP servers, MIME, UNIX mailbox format, and BABYL format. Features include virtual folders for searching and multi-folder management.

## Build Commands

```bash
# Configure (run first, or after configure.ac changes)
./configure                                    # Default GNU Emacs
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
cd test && ./test-runner                        # every pass this machine can run
cd test && ./test-runner --probe                # what is available, run nothing
cd test && ./test-runner --one vm-imap-test.el  # one file
cd test && ./test-runner --help                 # the rest of the options
cd test && make test                            # the same, ARGS=... to pass options
```

**`test/test-runner` is the only thing that runs tests.** The Makefile has no
Emacs of its own: `make test` forwards to the runner, and `ARGS` goes through
(`make test ARGS="--one vm-imap-test.el"`). Add a pass to the runner, not a
target to the Makefile.

`make test` fetches the optional packages first (BBDB, emacs-w3m, vcard) and
records that with the stamp file `test/opt/installed`, so a run covers as much
as the machine can. It needs the network once. A failure leaves no stamp and
stops the run, so the next one tries again — a stamp written anyway would claim
the packages are here and their tests would skip from then on with nothing to
show for it. `make test-no-opt` runs everything without fetching, for a machine
with no network.

It probes first, because an exit status does not say whether VM is broken or
the machine is short of a server: what is unconfigured skips, what is
configured but unreachable fails. `test/vm-test-probe.el` logs in to every
configured server, and the runner prints what answered, where mail would be
sent, which optional packages are installed and whether gpg is there.

Then it runs each pass in its own Emacs, and by default runs every one this
machine can:

| pass | what it adds |
|------|--------------|
| suite | everything, with whatever this machine has |
| mock | IMAP and POP again with the live servers skipped, where a live config exists |
| no-optional | the test files that name BBDB, emacs-w3m or vcard, run without them, where they are installed |
| send | real mail, sent and read back, where `vm-send-test-config` says where to |

Every pass runs whichever fails, and the summary at the end names each one.
`--one FILE`, `--imap`, `--pop`, `--send`, `--mock`, `--no-optional`,
`--assert`, `--leaks`, `--coverage` and `--forms-coverage` each run that
alone; `--skip-live`, `--skip-send`, `--verbose` and `--no-build` modify a
run.

**Two coverage reports, and they answer different questions.** `--coverage`
advises every `vm-` function and says which were called, so a function of
forty lines entered once counts as covered. `--forms-coverage` instruments
every form with `testcover` and says what ran inside: per definition, how
many forms were never evaluated, how many always returned the same value, and
how many varied. A never-evaluated `cond` arm is a branch no test took, which
is as close to branch coverage as Emacs gets; nothing in Emacs does path
coverage. The always-one-value count is the more useful signal, a predicate
that never returned nil being one no test varied.

`--forms-coverage` writes three files. `test/forms-coverage-results.txt` is
per definition; `test/line-coverage-results.txt` is per line, naming the
source lines that hold a form which never ran; `test/line-coverage.info` is
the same in lcov, for `genhtml` and the coverage viewers. A line is counted
only where it carries an instrumented form, so a comment is not reported as
uncovered. Currently 32273 lines with forms, 9167 never reached.

The line numbers come from edebug's own data: `(get SYMBOL 'edebug)` holds a
marker at the definition and a vector of per-form offsets from it, which is
how `testcover-mark` places the splotches it shows interactively. So they are
exact. lcov counts executions and this knows only whether a form ran, so a
line is written as run once or not at all.

**undercover.el was evaluated and not adopted** (2026-09-04). It works on VM
and writes lcov, Coveralls, Codecov, simplecov and text reports. Against it:

- it is far slower. The `--forms-coverage` pass does the whole suite in 343
  seconds; undercover had not reached 40% of it in 600, one large-folder test
  taking 297 seconds on its own. Both instrument through edebug, so the
  difference is in the recording, not the approach.
- it reports a percentage per file and no line numbers, where
  `line-coverage-results.txt` names the lines.
- it has no equivalent of the always-one-value signal, which is the more
  useful half of what testcover gives.
- its Coveralls and Codecov integration is the reason it exists, and VM has no
  CI to send a report to.

Where the two can be compared directly they agree: on `vm-misc.el` with only
`vm-misc-test.el` run, undercover counts 1124 relevant lines and the
testcover pass 1113, within 1%. **They disagree on coverage of that same
input, 46% against 54%, and I did not establish why.** The line rule here
counts a line as unreached when any form on it went unevaluated, which is the
conservative direction, so the gap is not the way round that explains itself.
Worth an hour if the numbers are ever used for anything that matters.

`--forms-coverage` depends on edebug being able to read every file, and two
arglists stopped it doing so (#795): `&key` in a plain `defun`, which Emacs
Lisp does not understand, and an `&optional` with no arguments after it. Both
failed **quietly**, the run finishing with plausible numbers while saying
nothing about the two largest files in the tree.
`vm-integration-test-every-file-can-be-instrumented` and
`vm-integration-test-only-cl-defun-takes-keyword-arguments` hold that shut.

- **A test that instruments the tree needs a subprocess Emacs.**
  `testcover-start` leaves its instrumentation in place, and instrumented code
  raises an error of testcover's own the moment a form it thought constant
  returns something else. Run in the suite's own Emacs, the instrumentation
  check killed a later test with `Value of form expected to be constant does
  vary` inside `vm-postpone.el`. It calls a child Emacs now, the same way the
  loaddefs tests do.

`VM_TEST_LIVE=0` (also `no`, `off`, `mock`) refuses the live servers and
`VM_TEST_OPTIONAL=0` the optional packages, for a pass run by hand. The mock
servers always run: on a configured machine a live run covers the same ground,
so a mock that has stopped agreeing with the client would otherwise fail
nowhere until it reached a machine with no config.

`test/vm-fuzz-test.el` drives a folder through random operation sequences and
checks its invariants after each one. It runs a small search as part of the
suite; `VM_FUZZ_SEEDS` and `VM_FUZZ_OPS` make it search harder, and a failure
reports the sequence that caused it.

**A round trip through VM proves less than it looks.** VM's readers forgive
what VM's writers do, so a folder that reads back correctly in VM can still be
one nothing else can read. `test/vm-interop-test.el` writes folders with VM
and counts the messages with Python's `mailbox` module, which owes nothing to
VM; `test/vm-interop-count.py` is the reader. It skips where python3 is
missing. That is how #801 was found, and it is the way to settle any claim
about what another program makes of a VM folder.

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
assume a test that leaks is harmless: `./test-runner --leaks` shows what is being
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
  needs `inhibit-debugger` for batch. `./test-runner --assert` runs the whole suite
  with them on and is expected to pass: an assertion that fires there is either
  a broken invariant or a test setting up a state no real caller is in, which is
  what four IMAP tests were doing until they were given a `process` buffer type.
- **Anything that runs gpg needs a GNUPGHOME of its own.** Without one a test
  writes into the keyring of whoever is running it, and signs with their real
  key. `vm-epg-test--with-a-test-keyring` makes a temporary home, generates an
  ed25519 key in it (0.4s, so no key is committed), and kills that home's
  gpg-agent afterwards. A mutation run that removed the isolation put a test
  key in the maintainer's real keyring.
- **A mutation that makes a test skip reads as a surviving mutation.** ert
  counts a skip among its expected results, so a harness watching only for
  FAILED reports the test as blind when it never ran. Count skips too.
- **A batch test of a blocking IMAP path needs the password seeded, or it
  blocks on stdin.** VM records a maildrop in `vm-imap-retrieved-messages`
  with the password stripped, so the blocking session asks for one:
  `IMAP password for ...: Error reading from stdin`, and because Emacs is
  blocked reading stdin rather than waiting on a process, **no timer fires**
  and it reads as a hang with nothing to show for it. Seed `vm-imap-passwords`
  under `vm-imapdrop-sans-password-and-mailbox` of the spec, which is the key
  `vm-imap-make-session` looks up. `SIGUSR2` with
  `(setq debug-on-event 'sigusr2)` is what got the answer out.
- **A mock IMAP test can pass without running the code it claims to test.**
  Visiting a folder and getting mail both go through the asynchronous driver,
  and the blocking `vm-imap-synchronize-folder` runs only where the driver
  declines -- so nothing in `vm-imap-retrieve-messages` is reached by an
  ordinary mock test. A regression test for that path has to make the driver
  decline, by stubbing `vm-imap-net-get-spooled-mail` to nil, which is what
  happens in the field when a password cannot be asked for. The first test
  written for #765 passed against the unfixed code for exactly this reason, and
  reverting the fix is what caught it.

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

### The async work

The asynchronous IMAP and POP conversion (emacs-vm/vm#473) is on `develop`.
It was developed on `develop-async`, which was merged in on 2026-08-25 and
deleted; there is no integration branch any more, and async work is cut from
`central/develop` like everything else.

**Do not start another integration branch.** Nobody was testing
`develop-async`, and the maintainer's reason for ending it is that one branch
is easier: whoever tests `develop` tests everything together, where a long
running branch gets the new work exercised apart from the rest of the tree and
needs `develop` merged into it for ever to stay honest. A large conversion goes
to `develop` in the same one-branch-per-issue steps as anything else.

- The decision it was built under is **non-blocking only**: no synchronous
  driver and no dual mode, so every converted path is asynchronous. The
  blocking implementation is still in the tree and serves what the driver
  declines — a maildrop whose password VM has not been told, since nobody can
  be asked from inside a process filter.
- `dev/docs/design/async-imap.org` has the reasoning, the table of what was
  converted, and the two places that still wait on purpose.
- A pause a reader can feel is a bug in this code and is measured, not
  guessed: `dev/tools/vm-fetch-latency.el` times a fetch by the step and
  `dev/tools/fill-imap-mailbox.py` makes a mailbox big enough to show one.

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
- `Pending` — the work is done and sitting on `develop`, ready to reach
  `main`. That is all it means. How the issue closes is a separate question,
  answered by `Close by hand`.

  The set is derivable from the `Closes #NNN` / `Re #NNN` trailers of one
  range:

  ```sh
  git log central/main..central/develop
  ```

  It used to take two, `develop-async` being an integration branch of its own,
  and deriving it from `main..develop` alone missed nine async issues. That
  branch is merged and gone, so one range is now the whole of it.

  **A `Re #NNN` trailer is no bar to `Pending`.** A fix often lands under
  another issue's number, and three pending issues have no trailer of their own
  at all. What a trailer cannot tell you is whether the work is finished, so
  read the issue before labelling.

  Do not put it on a closed issue, nothing is pending there. Do not put it on
  an issue whose fix is only partly done, or whose remaining step is outside
  the repo: #487 wants a page on nongnu.org edited, which no merge to `main`
  will do, so it carries `Human` instead. Anything that is not simply waiting
  for `main` needs its own label, not this one.
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
- `Close by hand` — `Pending`, and the merge will not close it: the trailer is
  `Re #NNN`, or names another issue, or the fix landed under another number and
  there is no trailer here at all. Someone has to close it when the branch
  reaches `main`.

  **Additive, never an alternative.** Every `Close by hand` issue carries
  `Pending` too, and a filter on `Pending` is expected to return it. Derivable
  from the same two ranges as `Pending`: an issue whose trailers there are all
  `Re`, or which has none.
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

## Writing

These apply to everything written for this repo: the manual, NEWS, docstrings,
code comments, commit messages, and issue and merge request text.

- **No em dashes in prose.** Not the character, and not `---`, which is what
  Texinfo renders as one. Use a comma, a colon, or two sentences.

  This is about the punctuation mark, not about the characters wherever they
  appear. Leave these alone, none of them being an em dash:

  - the Emacs file header, `;;; vm-foo.el --- description`, which is a required
    convention and uses `---` (135 files have one)
  - `--` in a symbol name, as in `vm-folder-test--octets`
  - command-line options, `--with-emacs`
  - a rule in an example, a literal MIME boundary, makeinfo's own ` -- Command:`
    in the generated appendix, and the verbatim GPL text
  - anything a test asserts on or that VM itself prints: `vm-icalendar.el`
    emits `" -- accepted"`, so the manual's example has to match it
- **No static version numbers in the manual, except when the sentence is
  about history.** "As of version 8.2.0, the only context ..." dates what VM
  simply does now, and has to be edited again at every release; say what it
  does. "Prior to version 8.2.0 it was possible to ..." is history and
  stays, as does the Selected Releases appendix and the minimum Emacs
  version, which is a requirement rather than a date. For a change made in
  the release being prepared write "In earlier releases", which needs no
  upkeep and which
  `vm-reference-test-the-manual-does-not-name-the-unreleased-version` exists
  to enforce.
- **Do not write "shape".** Say "structure", "form", "layout", or name the thing.
  It is vague where "structure" is specific.

## NEWS

The NEWS files record new functionality and user-visible changes of
behaviour: new commands, renamed or removed variables, changed defaults.
**Bug fixes do not go in NEWS** unless they change what VM does that a user
could depend on, such as what goes on the wire or what a command answers to
a key; a repair of something that never worked belongs in the issue tracker
and nowhere else.

**Write new entries at the front of the highest-numbered file**, which is
`NEWS-3.md`. The history is split the way Emacs splits `ChangeLog`: numbered
files, `NEWS-1.md` for the releases up to 7.19, `NEWS-2.md` for 8.0.0 through
8.2.0b1, `NEWS-3.md` for 8.3.0 onwards. There is deliberately no file called
`NEWS`: a file is never renamed, so every archived path stays valid for ever
and only the newest number moves. Start `NEWS-4.md` when `NEWS-3.md` grows
unwieldy, and leave the older files alone.

## Architecture

### Generated Files

These files are auto-generated during build - do not edit directly:
- `lisp/vm-autoloads.el`
- `lisp/vm-cus-load.el`
- `lisp/vm-version-conf.el`
- `Makefile` (from Makefile.in via configure)
- `vm-load.el` (from vm-load.el.in)

Edit the `.in` templates or `configure.ac` instead.

`info/vm-reference.texinfo` and `info/vm-docstrings.texinfo` are generated from
the docstrings in `lisp/` and are **committed**, because `vm.texinfo` includes
them and makeinfo writes no manual at all when they are missing. So a changed
docstring means regenerating them and committing the result:

```bash
make -C info vm-reference.texinfo vm-docstrings.texinfo
```

`make check-reference` compares the committed copy with what the docstrings say
now and fails when they differ. A plain `make` rewrites them in the working
tree, so forgetting this leaves the next person a dirty tree they did not
cause. Only commands and user options appear there: a docstring on a plain
function is not in the appendix and needs nothing.

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

- A byte-compiled file can be stale between Emacs versions; `rm -f lisp/*.elc` when a build behaves oddly
- Run from build directory by adding `lisp/` to load-path and requiring `vm-autoloads`
- Companion packages: BBDB (address book), emacs-w3m/w3 (HTML rendering)
- Bug reports: https://gitlab.com/emacs-vm/vm/-/issues
