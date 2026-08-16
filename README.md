# vc-sl.el

A [VC](https://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html)
backend for [Sapling](https://sapling-scm.com/) (`sl`).

`vc-sl` provides the generic Emacs VC commands for native Sapling
repositories.  Sapling-specific smartlog, stack, and working-copy workflows
are provided by the separate [`sl`](https://github.com/swithinchan/sl)
package.

## Requirements

- Emacs 26.1 or later.
- Sapling `sl` on `exec-path`.
- The `sl` Emacs package.

## Installation

Install the `sl` package first, then install `vc-sl`:

```elisp
(require 'sl)
(require 'vc-sl)
(add-to-list 'vc-handled-backends 'Sl)
```

The VC backend recognizes native Sapling repositories (identified by their
`.sl` directory).  Git-backed Sapling working copies are left to `vc-git`,
which is already a good fit for them.

## Supported VC commands

- `C-x v d` / `vc-dir` — directory status
- `C-x v =` / `vc-diff` — diffs between working copy and revisions
- `C-x v l` / `vc-print-log` — commit history
- `C-x v v` / `vc-next-action` — commit changes
- `C-x v g` / `vc-annotate` — per-line revision annotations
- `C-x v u` / `vc-revert` — revert working-copy changes
- `C-x v b` / `vc-retrieve-tag` — check out Sapling bookmarks
- `C-x v s` / `vc-create-tag` — create Sapling bookmarks

## Configuration

- `vc-sl-global-switches` — extra switches passed to every `sl` command
  run by VC.
- `vc-sl-diff-switches` — switches for `sl diff` under VC.

## Thank you

Thanks to feedback and comment by Philip Kaluđerčić. Thanks to my wife, Sophia Li for always supporting me, this package is also a shorthard for her name.
- `vc-sl-log-short-format` / `vc-sl-log-format` — templates used for the
  short and long VC log buffers.
