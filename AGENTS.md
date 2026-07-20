# hermes-el

An Emacs-native frontend for Hermes Agent. The dashboard, chat UI, management
browsers, Tracker cockpit, capability provider, and optional eval bridge live in
this repository.

## Public document policy

Treat this file as public project documentation.

- Never add machine-specific absolute paths, usernames, hostnames, IP addresses,
  private service topology, auth-source entry names, tokens, secrets, or private
  issue details.
- Use repository-relative paths for source references and generic values in
  examples.
- Keep durable project architecture and contributor practices here. Keep
  temporary plans, task state, build output, and personal configuration out.
- Verify current behavior from source, tests, or the authoritative backend. Do
  not preserve stale facts merely because they are documented here.

## Authoritative references

Hermes Agent is authoritative for the dashboard and TUI gateway protocols.
Relevant paths in a Hermes Agent checkout include:

- `hermes_cli/web_server.py` and `hermes_cli/dashboard_auth/middleware.py`
- `tui_gateway/server.py` and `tui_gateway/ws.py`
- `plugins/kanban/dashboard/plugin_api.py`
- `hermes_cli/kanban.py`
- `gateway/platforms/api_server.py`
- `apps/desktop/src/hermes.ts`
- `apps/desktop/src/app/desktop-controller.tsx`

Hermes Tracker is authoritative for its separate `/api/v1` contract, especially
`src/hermes_tracker/api/v1.py` and the API contract tests. The Python
`hermes-emacs-plugin` is authoritative for the stdio MCP bridge paired with
`hermes-exec.el`.

Use core Emacs EWOC and ERC, emacs-jabber, Gnosis, and keymap-popup as design
references.

## Architecture

The main dependency direction is bottom-up. Optional UI integrations may have
documented lazy cross-links, but lower transport layers must not depend on chat
or browser modules.

- `hermes-promise.el`: zero-package async primitive for dashboard, REST, and
  other composable operations. Process filters and socket callbacks stay at
  their I/O boundaries.
- `hermes-transport.el`: pure gateway frame and event normalization plus the
  shared field accessors. No I/O belongs here.
- `hermes-transport-cli.el`: sanctioned `hermes chat -Q -q` one-shot smoke-test
  fallback only.
- `hermes-dashboard-api.el`: dashboard URL handling, secret redaction,
  asynchronous HTTP, REST authentication, remote credentials, endpoint-scoped
  caches, and the dashboard client data structure.
- `hermes-dashboard-transport.el`: dashboard process and WebSocket lifecycle,
  ready handshake, session subscribers, owner-scoped requests, JSON-RPC
  promises, reconnect, heartbeat, idle close, and event routing.
- `hermes-dashboard-rpc.el`: typed gateway wrappers declared with
  `hermes-dashboard-transport-define-rpc`.
- `hermes-notifications.el`: shared desktop notification policy, safe previews,
  focus suppression, fallback behavior, and click-to-open actions. Feature
  modules classify domain events and call this boundary.

The chat stack is one logical module with explicit ownership boundaries:

- `hermes-chat-format.el`: pure event classification, diagnostics, markdown
  fontification, ANSI handling, and diff detection.
- `hermes-chat-render.el`: transcript rendering effects, disclosure entries,
  View Diff links, and result buffers.
- `hermes-chat-buffer.el`: EWOC buffer state, writable compose tail, entries,
  header state, and the ownership-aware FIFO queue.
- `hermes-chat-prompts.el`: approval, clarify, sudo, and secret prompt flows.
- `hermes-chat-dashboard.el`: chat-to-dashboard session lifecycle, restore,
  reattach, request ownership, event routing, and background tasks.
- `hermes-chat-models.el`: model and provider selection.
- `hermes-chat-handoff.el`: session handoff and bounded polling.
- `hermes-chat-slash.el`: slash completion, native commands, and gateway command
  dispatch.
- `hermes-chat.el`: turn reducer, effects, interactive commands, interrupt and
  queue settlement, plus assembly of the chat feature modules.

Management surfaces share `hermes-browser.el`, which owns dashboard client
provisioning, semantic faces, responsive columns, request generations, and the
`hermes-define-list-browser` macro.

- Simple list surfaces: `hermes-sessions.el`, `hermes-profiles.el`,
  `hermes-rollback.el`, `hermes-subagents.el`, and `hermes-cron.el`.
- Custom multi-view surfaces: `hermes-inventory.el`, `hermes-mcp.el`, and
  `hermes-kanban.el`.
- `hermes-kanban-log.el` owns worker-log diff parsing and navigation.
- `hermes-kanban-events.el` owns the dedicated plain-JSON Kanban event socket.
- `hermes-tracker.el` is an optional client for the separate Hermes Tracker
  service. It owns Tracker auth, TODO views and mutations, evidence-gated
  closure, and canonical Tracker-to-Kanban references.

Other independent integrations:

- `hermes-onboarding.el`: provider discovery and API-key onboarding.
- `hermes-capabilities.el`: bounded native Emacs capabilities over a dedicated
  provider WebSocket, with its own registry, lifecycle, and reconnect state.
- `hermes-exec.el`: optional HTTP-JSON eval endpoint for the Python MCP bridge.
  It is disabled by default and owns approval, bind, token, size, queue, and
  timeout controls.
- `hermes.el`: `M-x hermes` EWOC dashboard and entry hub, plus `M-x
  hermes-close` for confirmed local teardown and restart. It loads the
  user-facing chat and management surfaces; capabilities and exec remain
  optional.

## Transport boundaries

All Hermes Agent feature work goes through the dashboard configured by
`hermes-dashboard-transport-url`. Use its HTTP plugin routes or TUI gateway
WebSocket JSON-RPC. Never shell to the local Hermes CLI for feature data because
it reads local state and ignores the configured remote dashboard. The one-shot
chat fallback remains a smoke-test exception.

Hermes Tracker is a separate service with separate credentials. It uses
`hermes-tracker-url`, HTTPS outside loopback, auth-source credentials, and
idempotency keys for mutations. Tracker and dashboard credentials must never be
shared. Cross-link operations may compose Tracker REST calls with dashboard
Kanban plugin calls.

Never perform synchronous network I/O on Emacs's main thread. New work should
use these seams:

- New gateway RPC: add a wrapper in `hermes-dashboard-rpc.el`.
- New gateway event: normalize it in `hermes-transport.el`, reduce it in
  `hermes-chat.el`, and update status classification when needed.
- Simple one-result list: use `hermes-define-list-browser`.
- Complex browser: reuse browser faces, client lifecycle, request generations,
  and responsive column helpers without forcing it through the macro.
- Dashboard REST feature: use the asynchronous helpers in
  `hermes-dashboard-api.el`.
- Non-RPC server stream: own a dedicated socket, parse its protocol directly,
  debounce refreshes, reconnect with bounded backoff, and stop when its owner
  dies. Never route plain JSON through the chat JSON-RPC frame handler.
- Interactive request: follow the prompt modules and reply through the typed
  `*.respond` RPC.

## Async ownership rules

Late callbacks are a primary correctness risk.

- Capture the target buffer and request, turn, session, or connection identity
  before starting asynchronous work.
- Before applying a callback, prove the buffer is live, still has the expected
  mode, and still owns the captured generation or request token.
- Treat stale callbacks as no-ops. They must not mutate replacement buffers,
  settle newer turns, or reconnect superseded sockets.
- Tag transport requests with their owner and cancel owner requests during
  teardown without reporting expected cleanup as a user-facing failure.
- Buffers own their timers, transient clients, processes, subscriptions, and
  sockets. Install buffer-local cleanup hooks and make teardown idempotent.
- A replaced owner must not tear down its successor. Increment generations
  before closing old resources.
- Test rejection, timeout, disconnect, buffer-kill, rapid refresh, and stale
  callback paths, not only successful resolution.

## User-interface contracts

- `M-x hermes` opens the EWOC dashboard and `keymap-popup`, not a terminal UI.
- Chat uses an EWOC transcript and a writable tail after
  `hermes-chat--input-marker`. Submitted user turns alone receive the `> `
  prefix.
- The assistant reply stays last in a turn. Tool, status, thinking, and diff
  entries are inserted before the pending assistant node.
- Markdown is fontified through a temporary `markdown-mode` pass. The chat
  buffer does not change major mode.
- Unified diffs are replaced by a file label and `[View Diff]` link. Ordinary
  prose beginning with `+` or `-` is not a diff.
- Multiline tool and progress entries use the shared disclosure UI. Diff and
  command-result entries keep their dedicated rendering.
- Active-session backpressure is intentional. Preserve the busy guard and use
  interrupt, steer, the ownership-aware FIFO queue, prompt responses, or a new
  session.
- Desktop notifications default to chat replies and errors, input requests,
  background-task results, Kanban review, blocked, or failed transitions, and
  cron failures when failure monitoring is enabled. Routine Kanban completion
  is opt-in. Suppress notices for a target already visible on the focused frame,
  keep sensitive prompt details out of bodies, and make click actions return to
  the owning buffer or task row.
- Use `read-string-from-buffer` for multiline Tracker descriptions, comments,
  acceptance criteria, and verification evidence. Use `read-string` for
  single-line scalar fields.

## Functional Elisp practices

- Keep computation pure where practical. Pass explicit inputs and return plain
  data; keep buffer, process, timer, socket, and network mutation in thin effect
  functions.
- Prefer alists, plists, and stable `cl-defstruct` records. Read protocol data
  through `hermes-transport--get`, `hermes-transport--field`, and related shared
  accessors so hash tables, alists, and plists remain interchangeable.
- Transform collections with `mapcar`, `seq-map`, `seq-filter`, `mapconcat`, or
  `cl-loop ... collect`. Do not build values with `dolist`, `push`, and
  `nreverse`. Reserve imperative loops for real effects.
- Prefer `when` and `when-let*` for effects, `and` and `and-let*` for
  value-returning conjunctions, and `string-join` for joining existing strings.
- Keep functions around 35 lines or fewer unless a flat dispatch table or
  schema is clearer than artificial splitting.
- Do not use `declare-function` between `hermes-chat-*` siblings. Move helpers
  to the state-owning layer or connect upward behavior through the documented
  registries and cleanup hooks. Reserve declarations for soft external
  dependencies and documented lazy load boundaries.
- Use one space between tokens. Do not column-align forms.
- Comments explain why. Use plain `;;; Section name` headers and no decorative
  separators or filler commentary.

## Development workflow

1. Inspect the current git state, scoped source, nearby tests, and authoritative
   backend contract before proposing a change.
2. Discuss the implementation boundary before non-trivial edits. Use a written
   plan for cross-module, protocol, lifecycle, security, or migration work.
   Tiny documentation and mechanical fixes do not need ceremony.
3. For behavior changes, start with a focused failing ERT or a concrete
   reproducer when practical. Test the public contract and the important
   failure boundary, not implementation trivia.
4. Make the smallest coherent change. Preserve unrelated work in a dirty tree
   and avoid drive-by refactors.
5. Run targeted checks first, then broader project gates in proportion to risk.
6. Review the complete diff against the request and architecture. Fix findings
   and repeat verification before handoff.

Useful gates:

- Focused ERT: `make test TESTS=tests/hermes-chat-tests.el`
- Strict byte compilation: `make compile`
- Full compile, checkdoc, native compilation, and ERT gate: `make pre-commit`
- Nix-backed handoff and flake checks: `make pre-handoff-check`
- Live reload for dogfood after batch gates: `make load`
- Whitespace validation: `git diff --check`

Run `make pre-commit` before every authorized commit. Run
`make pre-handoff-check` for large, cross-cutting, public-release, dependency,
or flake-facing work. Live Emacs dogfood is additional evidence, not a
replacement for batch tests.

Never commit, amend, rebase, push, deploy, or run destructive Git commands
without explicit maintainer or task authorization. When a commit is authorized,
stage only scoped verified changes, use the repository identity, write a short
subject such as `module: Change`, and add no generated-by or co-author metadata.

## Adversarial review

Review depth follows risk, not line count.

- Trivial changes receive a complete diff self-review and the smallest relevant
  check.
- Small bounded behavior changes receive targeted tests plus a deliberate
  full-diff self-review.
- Cross-module lifecycle work, protocol changes, public API changes,
  authentication, secret handling, destructive operations, and broad
  refactors require a fresh independent review.

For required independent review:

1. Pin the reviewer to an immutable diff or commit and a precise acceptance
   boundary.
2. Give the first pass the artifact without the author's reasoning. Ask the
   reviewer to assume the plausible-looking change is wrong and hunt for
   regressions, stale state, ownership errors, protocol drift, secret exposure,
   cleanup failures, and tests that pass without proving the contract.
3. The reviewer reports findings without editing the candidate.
4. After the blind pass, provide the plan, architecture constraints, and test
   evidence for a completeness pass.
5. Resolve all blocking findings, rerun the relevant gates, and repeat review
   until the immutable candidate passes.

Reviewers should inspect source and tests, not trust summaries. A green test is
evidence only when its assertions exercise the claimed behavior.

## Kanban and Tracker handoffs

- Keep one coherent feature in one execution unit unless a real dependency,
  different accountable role, high-risk review gate, or publication boundary
  requires a split.
- Create dependent cards in dependency order. Do not create ready children and
  link them afterward.
- Use separate planner or reviewer cards only when the work's risk and size
  justify them.
- Handoffs include changed files, exact commands and observed results, diff or
  commit scope, review verdict, remaining blockers, and whether live Emacs
  dogfood ran.
- Close Tracker work only with concrete verification evidence. A status label
  or success claim is not proof.
- Do not include private paths, credentials, host details, raw logs, or local
  scratch files in cards, comments, commits, or handoffs.
