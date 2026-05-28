# flamegraph.el

[Flame graphs][bg] for the Emacs profiler — and for folded-stacks
files produced by `perf`, `py-spy`, `rbspy`, and friends.

[bg]: https://www.brendangregg.com/flamegraphs.html

Single-file package, text renderer (an SVG renderer is planned).
Top-down "icicle" orientation: the outermost frame sits on the top
row, and the stack grows downward. Identical call paths are merged
and children are sorted heaviest-first — what speedscope calls the
"Left Heavy" view.

Frames are interactive: `d` opens a description with samples, parent,
and callees, plus a snippet highlighting where those callees are
called from in source; `f` jumps to the source line. The snippet and
`f` need the data to include `file:line` — Elisp profiles always do;
for perf, fold with `-F +srcline` (see
[Recording](#recording-profile-data) below).

## Installation

Until this lands on a package archive, clone the repo and add it to
your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/emacs-flamegraph")
(autoload 'flamegraph-profiler-report "flamegraph" nil t)
(autoload 'flamegraph-find-file       "flamegraph" nil t)
```

Requires Emacs 30.1+.

## Quick start

### From a live profile

```
M-x profiler-start RET cpu RET     ; or `cpu+mem'
…run the code you want to measure…
M-x flamegraph-profiler-report
```

### From a saved profile or folded-stacks file

```
M-x flamegraph-find-file RET path/to/profile-or-stacks RET
```

Auto-detects between a profile saved with `profiler-report-write-profile`
and a `.folded` file. `.folded` is the common flame-graph interchange
format (one `;`-separated stack per line, followed by the sample count).

## Key bindings (in the `*Flamegraph*` buffer)

| Key | Action |
| --- | --- |
| `RET`, `mouse-1` | Zoom into the frame at point (make it the new root) |
| `u` | Zoom out one level |
| `t` | Reset zoom to the full graph |
| `l` / `r` | Go back / forward through the zoom history |
| `n`, `TAB` | Next frame |
| `p`, `S-TAB` | Previous frame |
| `d` | Describe the frame at point (samples, parent, callees, source) |
| `mouse-2` | Describe the clicked frame |
| `f` | Visit the frame's source (definition or executing line) |
| `g` | Redraw (e.g. after resizing the window) |
| `q` | Quit |

## The describe buffer (`d`)

For the frame at point, shows:

- A clickable heading — for an Elisp symbol, opens the standard
  `describe-function` help; for a frame with an embedded source
  location, the `file:line` is a second button that visits that line.
- A source snippet of the enclosing function, with the sampled line
  marked, structurally-enclosing lines kept (so the nesting that leads
  to the sampled line stays visible), and **every direct callee of the
  frame highlighted at its call site**, with skip-through special-form
  layers descended transparently.
- Sample count and self-time.
- Buttons to the frame's parent and to each callee. Callees rendered
  through `if`/`let`/`progn`/etc. are expanded inline so the path to
  the real callees stays visible.

## Recording profile data

### Emacs's native profiler

Just `M-x profiler-start` / `M-x flamegraph-profiler-report`. Or save
with `profiler-report-write-profile` and view later with
`flamegraph-find-file`.

### perf (Linux)

Recording:

```sh
perf record -F 99 -g --call-graph fp -p <pid> -- sleep 30
```

Use frame-pointer unwinding (`--call-graph fp`) when possible — DWARF
unwinding (`--call-graph dwarf,N`) caps the captured stack at N bytes
and turns into `[unknown]` frames once Emacs's redisplay/bidi recursion
overflows it. This needs the binary built with `-fno-omit-frame-pointer`.

Folding with source locations:

```sh
perf script --max-stack 512 --no-inline -F +srcline --full-source-path \
            -i out.perf.data \
  | perl stackcollapse-perf.pl --srcline \
  > out.folded
```

Both `-F +srcline` (perf has to be asked to emit srclines) and
`--no-inline` (perf 6.8's `addr2line` subprocess protocol desyncs on
binutils ≥2.39's variable inline-record count) are required. Use
`--full-source-path` so resolved paths are absolute.

If you want to keep inlined frames despite this, you'll need either
perf ≥ 6.9 (`--addr2line`) or an Emacs built `-no-pie` so the script's
own `addr2line -i` can resolve PIE addresses.

### py-spy, rbspy

```sh
py-spy record --format raw -o out.folded …
rbspy record --format flamegraph --raw-file out.folded …
```

Both emit frames in `NAME (file:line)` / `NAME - file:line` shape; the
package parses both.

## Customization

| Variable | Purpose |
| --- | --- |
| `flamegraph-width` | Canvas width in columns (default: window width) |
| `flamegraph-frame-border` | Pixel gap between adjacent frame boxes |
| `flamegraph-text-padding` | Pixels between a frame's left edge and its label |
| `flamegraph-source-directory` | Where to resolve relative paths when visiting source |

| Face | Purpose |
| --- | --- |
| `flamegraph-call-site` | Highlight for callee occurrences in the describe-buffer snippet |

## Development

```sh
make check     # byte-compile (warnings → errors) + ERT suite
make compile
make test
make clean
```

Use `EMACS=/path/to/emacs make check` to test against a particular
Emacs build. CI runs the same targets on Emacs 30.1 and snapshot.

## License

GPL-3.0-or-later.
