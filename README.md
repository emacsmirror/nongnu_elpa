# sapling.el

A fast Emacs interface for [Sapling](https://sapling-scm.com/)
(`sl`). It is designed to feel lighter while being intentionally
small. Sapling.el strives to make emacs version control in MS Windows more comfortable.

For Emacs's generic version-control commands (`C-x v d`, `C-x v =`,
`C-x v l`, ...), install the separate [`vc-sapling`](https://github.com/swithinchan/vc-sapling)
package.

## Features

- `M-x sapling-status` opens a status buffer showing the smartlog and working
  copy changes.
- Async process output: status, smartlog, log, diff, pull, push, rebase, etc.
  do not block Emacs.
- Editable commit/amend message buffers with `C-c C-c` / `C-c C-k`.
- Mark files in the status buffer and commit or amend only those files
  (`sl commit -I ...` / `sl amend -I ...`).
- Magit-style single-key bindings in the status buffer (`c`, `a`, `d`, `m`,
  `x`, `r`, `z`, `F`, `P`, `n`, `p`, ...).
- Emacs-style diff key: `=` or `C-x v =` in the status buffer, matching
  `vc-diff`.
- `sapling-diff` uses `diff-mode`, so removed lines are shown in red and added
  lines in green using Emacs-native font locking (no terminal color-code
  parsing).
- Other output commands (`log`, `smartlog`, `show`, `grep`, etc.) run with
  ANSI color enabled and `sapling-status` highlights smartlog changesets in
  yellow, matching colored `sl` output in a terminal.
- A transient-based dispatch menu (`M-x sapling-menu`) for the main Sapling
  workflows.
- A dynamic command builder (`M-x sapling-command`, or `C-c c` in the status
  buffer) that reads Sapling's own help text and offers the documented
  command-line subcommands and options without constructing a heavy transient
  UI.
- Stack workflows: `fold`, `graft`, `hide`, `unhide`, `uncommit`, `unamend`,
  `undo`, `redo`, `next`, `previous`.
- Working-copy workflows: `add`, `remove`, `forget`, `revert`, `clean`.
- Supporting commands: `grep`, `annotate`, `journal`, `metaedit`, `doctor`,
  `config`, bookmark management, `clone`, and `init`.

## Requirements

- Emacs 26.1 or later.
- Transient 0.3.0 or later.
- Sapling `sl` on `exec-path`.

## Installation

For now, clone or copy this directory and add it to `load-path`:

```elisp
(add-to-list 'load-path "/path/to/sapling")
(require 'sapling)

;; Optional: bind the dispatch menu or status buffer to Magit-like keys.
(global-set-key (kbd "C-x g") #'sapling-status)
(global-set-key (kbd "C-x v s") #'sapling-menu)
```

`sapling.el` has `;;;###autoload` cookies, so it can also be installed with
`package-install-file`.

## Windows notes

The package targets Windows Emacs explicitly:

- It runs `sl` through `make-process` with pipe connections rather than going
  through a shell, avoiding shell-quoting problems.
- `.bat` / `.cmd` Sapling wrappers are detected and invoked through the shell
  automatically.
- Output is decoded as UTF-8 (`utf-8-auto`), which also normalizes CRLF output
  from Windows processes.
- `HGPLAIN=1` and `SL_AUTOMATION=1` are set for every `sl` process so output is
  not paginated or interactive.  When `sapling-use-color` is non-nil,
  `SL_AUTOMATION_EXCEPT=color` and `--config color.mode=ansi --color=always`
  force ANSI color codes, which Emacs translates into faces.
- `w32-pipe-read-delay` is lowered to `sapling-w32-pipe-read-delay` (default
  `0`) while talking to `sl`, which makes process output significantly faster
  on Windows.

If your Sapling is installed as `sl.exe` but `sl` is not on `exec-path`,
customize `sapling-program`:

```elisp
(setq sapling-program "C:/Program Files/Sapling/sl.exe")
```

## Debugging and profiling

Sapling has two different debugging channels, and they are easy to confuse:

- `sapling-toggle-debug` (`C-c d` in the status buffer, or `C-c d` in
  `M-x sapling-menu`) logs the `sl` commands that `sapling.el` runs into
  `*sapling-debug*`.  This shows how Emacs invokes Sapling.
- Sapling's own `SL_LOG`, `SL_BTLOG`, `--debug`, `--verbose`, and
  `--profile` controls show what is happening inside the `sl` process.

### Rust tracing with `SL_LOG`

Rust tracing is enabled by setting `SL_LOG`:

```shell
SL_LOG=debug sl status
SL_LOG=commands::run=trace sl log -r .
SL_LOG=info,dag=debug,commands::run=trace sl log -r .
```

`SL_LOG` uses `tracing_subscriber` `EnvFilter` syntax.  The useful subset is:

- `debug` — enable a level globally.
- `commands::run=trace` — enable a level for one Rust module target.
- `info,dag=debug` — combine several directives with commas.

Start broad with `SL_LOG=debug`, identify noisy or useful targets, then narrow
the filter.

On Windows, the usual Unix `VAR=value command` prefix does not work in `cmd`.
Run these examples inside Emacs instead:

- `M-x eshell` is preferred and built in.  Eshell accepts
  `SL_LOG=debug sl status` and the other examples above directly.
- `M-x ghostel` also works and has better terminal graphics.  Ghostel is on
  MELPA; check it with `M-: (require 'ghostel nil t)`.  If that returns nil,
  install it from MELPA first.

### Python debugging from the transient menu

For Python-heavy command paths, the most useful first step is Sapling's
global debug output:

```shell
sl --debug --verbose log -r .
```

`--debug` and `--verbose` are understood by Sapling's Python command layer
and extensions.  They are usually more useful than `SL_LOG` for Python-only
behavior, while `SL_LOG` still helps in mixed Python/native paths.

From the transient menu, `M-x sapling-menu` and then `C-c D` (or
`M-x sapling-debug-log`) runs exactly this command and shows the result in
`*sapling-output*`.

### Backtraces at tracing points

`SL_BTLOG` prints a native backtrace when a matching tracing event or span
transition happens:

```shell
SL_BTLOG=dag::lifecycle::create=debug sl log -r .
```

It uses the same `EnvFilter` syntax as `SL_LOG`, but the output is much
larger because every matching event or span transition prints a backtrace.
Keep the filter narrow.  On supported builds, Sapling can also resolve Python
frames inside native backtraces, showing entries such as
`static:sapling.commands:3874`.

### Profiling

Add `--profile` to enable Sapling's native sampling profiler:

```shell
sl log -r . --profile
```

It samples native stacks and, on supported builds, Python frames, then prints
an ASCII summary to stderr.  Useful configuration examples:

```shell
sl log -r . --profile --config profiling.interval=1ms
sl log -r . --profile --config profiling.output=/tmp/sl-profile.txt
```

For Python-only profiles, enable `profiling.enabled-python` and choose a
`profiling.type`:

```shell
sl log -r . --config profiling.enabled-python=true --config profiling.type=stat
```

The available Python profiler types are:

- `stat` — statistical profiler; best for long-running commands.
- `ls` — Python's built-in instrumenting profiler; works broadly, but line
  reporting is tied to function start lines.
- `traceprof` — tracing profiler; useful for tree-shaped reports of small
  functions called many times.

In the ASCII profiler summary, follow large `Dur` values downward until the
time stops concentrating in one child; that split is usually the next place
to investigate.  See `sl help config.profiling` for more knobs.

## Status buffer keys

| Key | Command |
|-----|---------|
| `g` | refresh status |
| `RET` | visit file at point |
| `SPC` | diff file at point |
| `m` / `u` | mark / unmark file |
| `M` / `U` | mark / unmark all |
| `c` | commit (marked files, or all changes if none are marked) |
| `a` | amend current commit |
| `d` | diff (marked files, file at point, or all changes) |
| `=` / `C-x v =` | diff, like Emacs `vc-diff` |
| `l` | smartlog |
| `L` | graph log |
| `b` | smartlog (alias) |
| `x` | absorb (`C-u x` dry-run) |
| `r` | rebase onto revision |
| `R` | continue rebase |
| `z` / `Z` | shelve / unshelve |
| `n` / `p` | next / previous commit in the stack |
| `G` | goto revision |
| `B` | create bookmark |
| `A` | add file at point / marked files |
| `D` | remove file at point / marked files |
| `K` | forget file at point / marked files |
| `V` | revert file at point / marked files (prefix skips confirmation) |
| `e` | metaedit current commit message |
| `o` | show current commit |
| `J` | journal |
| `f` | fold commits |
| `h` / `H` | hide / unhide a commit |
| `C-c g` | graft a commit |
| `C-c u` | undo last local command |
| `C-c R` | redo last undo |
| `C-c c` | build any Sapling command from its help/options |
| `C-c d` | toggle Sapling debug logging |
| `F` | pull |
| `P` | push |
| `q` | quit window |

## Configuration

- `sapling-program` — Sapling executable name or path (default `"sl"`).
- `sapling-use-color` — colorize command output with ANSI colors (default `t`).
- `sapling-log-limit` — number of commits in `sapling-log` (default `100`).
- `sapling-w32-pipe-read-delay` — Windows pipe read delay in ms.  Defaults to
  the current `w32-pipe-read-delay`, or `0`; set to `nil` to leave the
  global value unchanged.
- `sapling-diff-use-diff-mode` — show diffs in `diff-mode` for native red/green
  coloring (default `t`).
- `sapling-diff-ignore-space-at-eol` — pass `--ignore-space-at-eol` to `sl diff`
  on Windows by default, avoiding CRLF noise.
- `sapling-debug` — log Sapling command invocations and output.
- `sapling-debug-buffer-name` — debug log buffer name.
- `sapling-debug-output-limit` — truncation limit for debug output.
- `sapling-status-buffer-name`, `sapling-smartlog-buffer-name`,
  `sapling-log-buffer-name`, `sapling-diff-buffer-name`,
  `sapling-output-buffer-name` — buffer names.

## Status and roadmap

This is an early, intentionally small implementation. Obvious next steps:

- Make smartlog commit lines actionable (`RET` to show a commit, `x` to
  absorb onto a commit, etc.).
- Add an Emacs-native interface for `sl split` and `sl histedit` (both are
  interactive terminal programs, so they need a dedicated selection UI).
- Add per-command transient menus for the most-used Sapling commands.
- Integrate `sapling-status` with `project.el`.

## Thank you
Thanks to feedback and comment by Russel Mok, Philip Kaluđerčić, Stéphane Marks.
Thanks to my wife, Sophia Li for always supporting me. 
