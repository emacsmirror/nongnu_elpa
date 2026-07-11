# hermes-el

A fresh Emacs Lisp frontend for Hermes Agent.

## Reference sources

This frontend mirrors the Hermes dashboard/TUI gateway protocol and borrows
idioms from core Emacs packages.  Keep machine-specific absolute paths out of
this file: record your own checkout locations in an untracked `AGENTS.local.md`
(copy `AGENTS.local.example.md`), and rediscover anything missing with file
search.

- **Hermes Agent backend** -- authoritative for the gateway protocol and the
  kanban dashboard plugin.  Files of interest (relative to the backend checkout):
  - dashboard server: `hermes_cli/web_server.py`
  - dashboard auth middleware: `hermes_cli/dashboard_auth/middleware.py`
  - kanban dashboard HTTP plugin (authoritative for kanban UI):
    `plugins/kanban/dashboard/plugin_api.py`
  - kanban CLI / task JSON shape: `hermes_cli/kanban.py`
  - TUI gateway JSON-RPC methods: `tui_gateway/server.py`
  - WebSocket bridge: `tui_gateway/ws.py`
  - API server fallback/reference: `gateway/platforms/api_server.py`
- **Hermes Desktop** -- reference frontend (`src/main/hermes.ts`).
- **hermes-emacs-plugin** -- the Python stdio MCP bridge paired with
  `hermes-exec.el`: exposes namespaced `emacs_*` MCP tools to the Hermes agent
  and forwards conservative elisp to Emacs's HTTP-JSON `/eval` endpoint
  (`hermes_emacs_plugin/server.py`, `elisp.py`, `eval_client.py`).
- **Core Emacs** -- EWOC (`lisp/emacs-lisp/ewoc.el`) and ERC (`lisp/erc/`) for
  buffer and transport idioms.
- **emacs-jabber** -- chat-buffer shape (`lisp/jabber-chatbuffer.el`).
- **gnosis** -- dashboard / `keymap-popup` style (`lisp/gnosis-dashboard.el`).
- **keymap-popup** -- the popup keymap implementation.

## Architecture (module map)

Layered bottom-up; each layer depends only on the ones above it in this list.

- `hermes-promise.el` -- the async primitive (`make`/`resolve`/`reject`/`then`/
  `map`/`catch`/`all`/`finally`). Zero package deps. Every async path composes
  through promises; nothing else implements callback plumbing.
- `hermes-transport.el` -- PURE event/model normalization. Shared field
  accessors (`--get`, `--field`, `--field-present-p`, `--scalar-string`,
  `--non-empty-string`) and the `hermes-transport--normalize-*` family that turn
  a raw gateway frame into a normalized event plist. No I/O lives here.
- `hermes-transport-cli.el` -- the sanctioned `hermes chat -Q -q` one-shot
  subprocess fallback (`hermes-transport-send`) and the
  `hermes-transport-send-function` seam. Smoke-test transport only.
- `hermes-dashboard-api.el` -- the HTTP/REST bottom of the dashboard stack:
  the dashboard URL options, URL/WebSocket endpoint builders, secret
  redaction, the promise-based url.el HTTP plumbing, the client struct (data
  model only), the REST API + auth, the profile/model caches, and remote
  credential resolution. Symbol names keep the
  `hermes-dashboard-transport-` prefix.
- `hermes-dashboard-transport.el` -- the connection lifecycle (requires
  `hermes-dashboard-api`): process spawn, WebSocket connect + ready
  handshake, subscribers, JSON-RPC request/promise plumbing,
  reconnect/heartbeat/idle-close, and event dispatch. Also exposes
  `hermes-dashboard-transport-open-websocket`, a redaction-wrapped raw-socket
  opener for callers (e.g. the kanban live-events tail) that own a separate
  plain-JSON stream rather than the chat JSON-RPC client.
- `hermes-dashboard-rpc.el` -- the typed JSON-RPC method wrappers: the
  `hermes-dashboard-transport-define-rpc` macro plus one generated wrapper per
  gateway method (wrapper names keep the `hermes-dashboard-transport-` prefix).
  Modules that call gateway methods require this; the transport core does not
  depend on it.
- `hermes-chat*` -- one logical module (see the require note in `hermes-chat.el`):
  - `hermes-chat-format.el` -- pure render helpers: markdown fontification, diff
    detection, ANSI stripping, the `hermes-chat--{ready,error,active}-statuses`
    keyword tables, and pure event classification/diagnostics.
  - `hermes-chat-render.el` -- transcript rendering effects between format
    and buffer: markdown/shadow insertion, diff View Diff links, the
    dedicated diff/background result buffers, entry-expansion metadata.
  - `hermes-chat-buffer.el` -- the EWOC buffer/mode, the writable compose tail
    after `hermes-chat--input-marker`, node insertion, the shared
    entry/header-state primitives, the header line, and the queue/drain
    input flow (submitting through `hermes-chat--submit-function`).
  - `hermes-chat.el` -- the `hermes-chat--turn-reduce` reducer + effects, event
    handling, commands, session actions; populates the registry variables
    at load.
  - `hermes-chat-slash.el` -- slash commands: `/command arg' parsing, the
    `commands.catalog` cache and `completion-at-point` function, the native
    in-client command table, and `slash.exec`/`command.dispatch` dispatch.
  - `hermes-chat-models.el` -- model/provider selection: `model.options`
    completion candidates, the `config.set` model switch with its
    expensive-model confirmation loop, API-key provider connect.
  - `hermes-chat-handoff.el` -- session handoff: the `handoff.request`
    command, live-platform target prompt, and the backoff-polled
    `handoff.state` watcher with its `handoff.fail` timeout.
  - `hermes-chat-prompts.el` -- the approval/clarify/sudo/secret prompt flows.
  - `hermes-chat-dashboard.el` -- chat<->dashboard glue, session
    restore/reattach, server session titles, and `/btw` background tasks;
    routes events upward only through `hermes-chat--turn-event-function`.
- `hermes.el` -- the `M-x hermes` EWOC dashboard entry; requires the world.
- Browser modules via `hermes-browser.el` + the `hermes-define-list-browser`
  macro: `hermes-sessions`, `hermes-kanban`, `hermes-mcp`, `hermes-cron`,
  `hermes-profiles` (profile list with in-place model editing via
  `PUT /api/profiles/{name}/model`),
  `hermes-inventory`, `hermes-rollback`, `hermes-subagents`. Each derives its
  base URL from `hermes-dashboard-transport-url` and uses the async REST helpers.
  `hermes-kanban` is split along its pure seams: `hermes-kanban-log.el` holds
  the worker-log diff engine (detection/validation/fontification plus the two
  hunk-navigation commands; deliberately duplicates part of the
  `hermes-chat-format` diff walker because the kanban variant accepts the
  gateway `a/path → b/path` header and rejects hunks with leftover counts),
  and `hermes-kanban-events.el` holds the live-events tail: a dedicated raw
  WebSocket to `/api/plugins/kanban/events` (plain `{events,cursor}` JSON parsed
  with `json-parse-string`, debounced in-place revert, bounded-backoff
  reconnect) -- never routed through the chat JSON-RPC `--handle-frame`.
  `hermes-kanban.el` requires both and keeps the buffers, modes, and commands.
- `hermes-onboarding.el` -- provider onboarding: lists the dashboard's
  unauthenticated providers from `model.options`, reads an API key, and saves it
  via `model.save_key`. Entry points: the dashboard auth gate, the chat model
  picker, and `C-c C-o K`. Independent of the chat transcript stack.
- `hermes-capabilities.el` -- native Emacs capability provider: registers over
  a dedicated second `/api/ws` connection (independent of chat buffers), takes
  JSON-RPC `emacs.request` frames, dispatches through the
  `hermes-capabilities-define` method registry, and replies with JSON-RPC
  `result`/`error` frames.
- `hermes-exec.el` -- optional local eval endpoint (FastMCP bridge); independent
  of the chat transcript stack, but reuses `hermes-dashboard-transport` for its
  loopback-URL check.

Data flow: outbound chat input -> `prompt.submit` RPC over the WebSocket.
Inbound WS frame -> `hermes-dashboard-transport--normalize-event-frame` /
`hermes-transport--normalize-*` (pure) -> `hermes-chat--turn-reduce` produces new
state + effects -> effects insert/replace EWOC nodes -> format helpers fontify
markdown and swap diffs for View Diff links. The agent reply stays the last node.

Extension seams (prefer these over ad hoc additions):

- New RPC method -> add a `hermes-dashboard-transport-define-rpc` form in
  `hermes-dashboard-rpc.el`.
- New event type -> handle it in the `hermes-transport--normalize-*` dispatch and
  the `hermes-chat--turn-reduce` reducer, and add a keyword to the status tables
  in `hermes-chat-format.el`.
- New management/list surface -> `hermes-define-list-browser`.
- New live event stream (server push, non-RPC) -> open a dedicated socket with
  `hermes-dashboard-transport-open-websocket`, parse frames with
  `json-parse-string`, debounce an in-place refresh, and reconnect with bounded
  backoff that stops when the buffer dies; never route it through the chat
  `--handle-frame` (mirror the `hermes-kanban` events tail).
- New interactive prompt (approval-like) -> follow the `hermes-chat-prompts.el`
  pattern: request event -> buffer UI -> `*.respond` RPC.
- New REST-backed module -> derive the base URL from
  `hermes-dashboard-transport-url`, reuse the async `api-request`/auth helpers;
  never shell to the CLI, never add a synchronous network call on the main thread.

## Architecture constraints

- `M-x hermes` opens a dashboard/welcome page, not a direct chat buffer.
- Dashboard interaction surface is `keymap-popup`, following Gnosis.
- MVP dashboard action is Chat.
- Chat buffer follows ERC/emacs-jabber shape: writable input tail, EWOC transcript entries, async transport.
- Assistant replies render through `markdown-mode` fontification (a temp-buffer font-lock pass, not a major-mode switch of the chat buffer).
- Diffs are not shown inline: each detected diff (markdown `diff`/`patch` fence, inline unified diff, or a whole `diff` event whose gateway-rendered header is `a/path → b/path`, ANSI stripped first) is replaced by a shadow-faced file label plus a `[View Diff]` link (`link` face) that opens the diff in a dedicated `diff-mode` buffer via `hermes-chat--show-diff`.  Ordinary chat lines beginning with `+`/`-` without a hunk header must not be detected as diffs.
- The agent's reply is the last entry in a turn: tool/status/diff/thinking entries are inserted before the pending assistant node so the final answer stays at the bottom.
- Multiline `tool`/`progress` entries (e.g. a multi-line terminal command) collapse to a one-line `▸`/`▾` toggle showing the first line, expandable to the full output -- same disclosure mechanism as the thinking entry.  Diff content stays a View Diff link; status/command-result entries are not collapsed.
- Input area follows emacs-jabber: the EWOC footer is a full-width separator rule (`hermes-chat--separator`, `hermes-chat-separator` face), not a `> ` prompt; the whole writable tail after `hermes-chat--input-marker` is the compose area, so line motion works natively.  The `> ` prefix is kept only on submitted user turns in the transcript.
- Slash commands complete at point: `hermes-chat--slash-capf` (in `completion-at-point-functions`) completes `/command` names from a buffer-cached `commands.catalog`, gated to the writable tail; any command dispatches via `command.dispatch`/`slash.exec`.  `C-c C-o` opens `hermes-chat-actions-map` (a `keymap-popup`) of in-turn actions (steer/interrupt/queue/prompt/session/model).
- Treat `hermes-el` as an Emacs-native Hermes Desktop frontend, not as a terminal UI wrapper.

## Transport direction

GOAL: every hermes-el operation goes through the Hermes dashboard API at the URL
in `hermes-dashboard-transport-url` (honor its obsolete alias
`hermes-dashboard-transport-remote-url`).  Never shell out to the local `hermes`
CLI for feature work: the CLI reads local SQLite and ignores the configured
remote dashboard, so it returns stale/empty data.  Sanctioned backends are the
dashboard HTTP plugin routes (e.g. `/api/plugins/kanban/*`) and the TUI gateway
WebSocket JSON-RPC.  Every module derives its base URL from
`hermes-dashboard-transport-url` and reuses the auth/HTTP helpers in
`hermes-dashboard-transport.el`.  The only CLI exception is the documented
`hermes chat -Q -q` smoke-test fallback below.

The real frontend should use the Hermes dashboard/TUI gateway protocol, mirroring Hermes Desktop:

1. Start/connect to `hermes dashboard --no-open --tui --host 127.0.0.1 --port <port>`.
2. Set `HERMES_DASHBOARD_SESSION_TOKEN` and usually `HERMES_DASHBOARD_TUI=1` when spawning.
3. Connect to `ws://127.0.0.1:<port>/api/ws?token=<token>`.
4. Drive JSON-RPC methods such as:
   - `session.create`
   - `session.resume`
   - `prompt.submit`
   - `session.interrupt`
   - `approval.respond`
   - `clarify.respond`
   - `sudo.respond`
   - `secret.respond`
5. Render `event` frames into the Emacs chat UI.

`hermes chat -Q -q` is fallback/smoke-test transport only.  Do not build the main UX around one-shot CLI subprocesses.

The API server `/v1/runs` + `/v1/runs/{id}/events` can be a simpler HTTP/SSE fallback/reference, but the dashboard/TUI gateway is the richer surface for Desktop-like UX because it supports session controls, slash flows, approvals, clarify/sudo/secret prompts, interrupts, and structured live events.

## Busy / pending reply behavior

`A Hermes reply is still pending` is expected single-session backpressure, not the root bug.

Do not remove that guard or force a second prompt into the same session while a turn is active.  Instead expose explicit busy actions:

- interrupt current run via `session.interrupt`,
- queue a next message,
- steer the current run when supported,
- open a new session/chat buffer,
- answer approval/clarify/sudo/secret prompts from Emacs UI.

The header/modeline should make current state visible: waiting, streaming, active tool, approval requested, error, or done.

## Development rules

- Keep code small, idiomatic, and functional where practical.
- Prefer clean functional style: pure helpers, explicit inputs/outputs, and data transformations over mutation-heavy blobs.
- Executor-authored functions should generally stay under ~35 LOC; split larger routines unless a flat dispatch/table is clearer.
- Separate pure entry/model/event transformations from buffer/process/WebSocket side effects.
- No `declare-function` between the `hermes-chat-*` siblings: sink a helper to
  the layer that owns its state, or register upward wiring through the
  registry variables (`hermes-chat--turn-event-function`,
  `hermes-chat--submit-function`, `hermes-chat--native-slash-commands`,
  `hermes-chat-cleanup-functions`).  `declare-function` is reserved for soft
  external dependencies (`ext:websocket`, `markdown-mode`, `string-edit`),
  documented load-cycle exceptions, and downstream autoloaded commands.
- Preserve existing WIP.  Do not run `git reset`, `git checkout --`, `git clean`, `git stash`, amend, rebase, or push unless the maintainer explicitly asks.
- Kanban workers should make real local commits after verified code-changing work/review. Use normal `git commit`, author with the repo/default git identity, short subjects like `module: change` or `[fix] module: change`, and no AI/co-author metadata. Commit only scoped, verified changes; if concurrent WIP makes staging unsafe, block with `commit-required` and state the exact safe split needed.
- Verify code changes with targeted ERT, `make test`, byte-compilation/`make compile`, and `git diff --check` where appropriate.  Live `emacsclient` dogfood is additional verification, not a replacement for clean batch tests.
- Run `make pre-commit` before every commit: it runs `git diff --check` plus strict byte-compilation (`byte-compile-error-on-warn`), strict `checkdoc`, native-compilation, and ERT, and fails on any warning.  Modeled on the emacs-jabber Makefile dev/lint flow.  `make native-comp` runs the native-comp warning gate alone.

## Code style (functional Elisp)

The default is functional and data-oriented.  These are house rules; follow them
in new and edited code (no obligation to retrofit untouched code unless asked).

- **Pure functions for computation.** A function that computes a value takes its
  inputs as arguments and returns a result with no side effect.  Keep I/O
  (buffer/process/WebSocket/network) in a thin effect layer that calls the pure
  helpers, per the module map above.  This is what makes the code testable with
  plain temp-buffer/return-value ERT.
- **Transform, don't iterate.** Build lists by transforming data, not by
  accumulating side effects.  Prefer `mapcar`, `mapconcat`, `seq-map`/`seq-filter`,
  and `cl-loop ... collect` (with `when`/`if`/`append` clauses for filtering and
  flattening).  Do NOT write the `dolist` + `push` + `nreverse` accumulator
  pattern -- it is the imperative spelling of a `mapcar`/`cl-loop collect`.
  Reserve `dolist`/`while` for genuine side-effecting loops (inserting EWOC nodes,
  emitting events), not for producing a value.
- **Plain data.** Model values as alists and plists (and `cl-defstruct` for
  records with a stable shape, e.g. the transport client).  Read them through the
  shared `hermes-transport--get`/`--field`/`--field-present-p` accessors rather
  than ad hoc `assoc`/`plist-get` chains, so hash/alist/plist sources stay
  interchangeable.  Avoid mutable hash tables for transient computation.
- Existing conventions still hold: `when`/`when-let*` for side effects,
  `and`/`and-let*` for value-returning conjuncts; `string-join` over
  `(mapconcat #'identity ...)`; functions under ~35 LOC.

## Kanban workflow

Decompose one coherent feature at a time into dependency-ordered cards --
typically a planner/design card, one or more executor cards, and a reviewer
card.  Create linked parent/child cards in dependency order (do not create ready
children and link them afterward).

Worker handoffs should include changed files, exact commands and real results, a
diff/stat summary, remaining blockers with the exact decisions needed, and
whether live Emacs dogfood was run.  Avoid status-only JSON blobs that do not
prove the artifact works.
