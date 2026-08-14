# sl.el

A fast, Magit-inspired Emacs interface for [Sapling](https://sapling-scm.com/)
(`sl`). It is designed to feel familiar to Magit users while being intentionally
small and Windows-friendly.

## Features

- `M-x sl-status` opens a status buffer showing the smartlog and working
  copy changes.
- Async process output: status, smartlog, log, diff, pull, push, rebase, etc.
  do not block Emacs.
- Editable commit/amend message buffers with `C-c C-c` / `C-c C-k`.
- Mark files in the status buffer and commit or amend only those files
  (`sl commit -I ...` / `sl amend -I ...`).
- Magit-style single-key bindings in the status buffer (`c`, `a`, `d`, `m`,
  `x`, `r`, `z`, `F`, `P`, `n`, `p`, ...).
- A lightweight dispatch menu (`M-x sl-menu`) with no external
  dependencies.
- Stack workflows: `fold`, `graft`, `hide`, `unhide`, `uncommit`, `unamend`,
  `undo`, `redo`, `next`, `previous`.
- Working-copy workflows: `add`, `remove`, `forget`, `revert`, `clean`.
- Supporting commands: `grep`, `annotate`, `journal`, `metaedit`, `doctor`,
  `config`, bookmark management, `clone`, and `init`.

## Requirements

- Emacs 26.1 or later.
- Sapling `sl` on `exec-path`.

## Installation

For now, clone or copy this directory and add it to `load-path`:

```elisp
(add-to-list 'load-path "/path/to/sl")
(require 'sl)

;; Optional: bind the dispatch menu or status buffer to Magit-like keys.
(global-set-key (kbd "C-x g") #'sl-status)
(global-set-key (kbd "C-x v s") #'sl-menu)
```

`sl.el` has `;;;###autoload` cookies, so it can also be installed with
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
  not paginated or colorized.
- `w32-pipe-read-delay` is lowered to `sl-w32-pipe-read-delay` (default
  `0`) while talking to `sl`, which makes process output significantly faster
  on Windows.

If your Sapling is installed as `sl.exe` but `sl` is not on `exec-path`,
customize `sl-program`:

```elisp
(setq sl-program "C:/Program Files/Sapling/sl.exe")
```

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
| `l` | graph log |
| `b` | smartlog |
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
| `F` | pull |
| `P` | push |
| `q` | quit window |

## Configuration

- `sl-program` — Sapling executable name or path (default `"sl"`).
- `sl-log-limit` — number of commits in `sl-log` (default `100`).
- `sl-w32-pipe-read-delay` — Windows pipe read delay in ms (default `0`).
- `sl-status-buffer-name`, `sl-smartlog-buffer-name`,
  `sl-log-buffer-name`, `sl-diff-buffer-name`,
  `sl-output-buffer-name` — buffer names.

## Status and roadmap

This is an early, intentionally small implementation. Obvious next steps:

- Make smartlog commit lines actionable (`RET` to show a commit, `x` to
  absorb onto a commit, etc.).
- Add an Emacs-native interface for `sl split` and `sl histedit` (both are
  interactive terminal programs, so they need a dedicated selection UI).
- Optional `transient` menus when `transient` is installed.
- Integrate with `project.el` and `vc`.
