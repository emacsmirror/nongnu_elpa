# hermes-el

Emacs frontend for Hermes Agent: dashboard, chat, browsers, capabilities, and
optional eval bridge.

## Public safety

- Public document. No local paths, users, hosts, IPs, private topology,
  auth-source names, secrets, tokens, private issues, logs, or personal config.
- Use repository-relative paths and generic examples.
- Keep only durable architecture and contributor rules. Verify current facts.

## Authority

Hermes Agent defines wire contracts. Check `hermes_cli/web_server.py`,
`hermes_cli/dashboard_auth/middleware.py`, `tui_gateway/{server,ws}.py`,
`plugins/kanban/dashboard/plugin_api.py`, `hermes_cli/kanban.py`,
`gateway/platforms/api_server.py`, and `apps/desktop/src/{hermes.ts,app/desktop-controller.tsx}`.
`hermes-emacs-plugin` defines `hermes-exec.el`'s stdio MCP bridge. Backend wins
for wire behavior; this repository wins for client behavior. Fix conflicting
tests or docs.

Design references: Emacs EWOC/ERC, emacs-jabber, Gnosis, keymap-popup.

## Architecture

Dependencies flow upward. Transport never depends on chat or browsers. Optional
UI links stay lazy.

- Core: `hermes-promise.el`; pure normalization in `hermes-transport.el`.
- Dashboard: HTTP/auth/cache in `hermes-dashboard-api.el`; WebSocket/JSON-RPC
  lifecycle in `hermes-dashboard-transport.el`; typed RPCs in
  `hermes-dashboard-rpc.el`.
- `hermes-transport-cli.el`: one-shot chat smoke fallback only.
- Shared policy: notifications in `hermes-notifications.el`; browser lifecycle
  in `hermes-browser.el`.
- Browsers: simple lists for sessions, profiles, rollback, subagents, cron;
  custom inventory, MCP, Kanban views.
- Kanban: log parsing in `hermes-kanban-log.el`; plain-JSON socket in
  `hermes-kanban-events.el`; addons use
  `hermes-kanban-task-detail-functions`.
- Capabilities owns its provider WebSocket. Exec stays optional and default-off,
  owning approval, bind, token, size, queue, and timeout controls.
- `hermes.el`: entry hub and confirmed local teardown.

Chat ownership:

- Format: pure classification, diagnostics, Markdown, ANSI, diffs.
- Render: transcript effects, disclosures, diff links, result buffers.
- Buffer: EWOC state, compose tail, header, FIFO queue.
- Prompts: approval, clarify, sudo, secret flows.
- Dashboard: sessions, restore, reattach, routing, background tasks.
- Models, handoff, slash: named feature modules.
- `hermes-chat.el`: reducer, effects, commands, interrupt, settlement, assembly.

## Transport and ownership

- Every feature must work with a released, unmodified Hermes Agent. Newer
  protocol fields may enhance behavior, but must not be required by the
  baseline path.
- Use dashboard HTTP or TUI WebSocket through
  `hermes-dashboard-transport-url`. Never shell to local Hermes CLI for feature
  data. One-shot chat smoke is sole exception.
- Never block Emacs main thread with network I/O.
- Add RPCs in `hermes-dashboard-rpc.el`; normalize events in
  `hermes-transport.el`; reduce chat events in `hermes-chat.el`.
- Use `hermes-define-list-browser` for simple lists and shared browser lifecycle
  for complex views. Use dashboard API async helpers for REST.
- Non-RPC streams own socket, parser, debounce, bounded reconnect, and cleanup.
  Never feed plain JSON to chat JSON-RPC code.
- Reply to interactive requests through typed `*.respond` RPCs.
- Capture buffer plus request, turn, session, connection, or generation before
  async work. Mutate only if buffer, mode, and identity remain current.
- Stale callbacks do nothing. Old owners never affect successors or newer turns.
- Owners hold requests, timers, clients, processes, subscriptions, and sockets.
  Cleanup is local, idempotent, and quiet. Increment generation before closing
  replaced resources.
- Test rejection, timeout, disconnect, buffer kill, rapid refresh, replacement,
  and stale callbacks.

## UI contracts

- `M-x hermes` opens EWOC dashboard plus keymap-popup, not terminal UI.
- Chat keeps EWOC transcript plus writable tail after
  `hermes-chat--input-marker`. Only submitted user turns get `> `.
- Assistant reply stays last; tool, status, thinking, and diff entries precede
  pending assistant node.
- Fontify Markdown in temporary `markdown-mode`. Never change chat major mode.
- Replace real unified diffs with label and `[View Diff]`; preserve ordinary
  `+` or `-` prose.
- Use shared disclosure UI for multiline tool/progress entries. Keep dedicated
  diff and command-result rendering.
- Preserve busy guard. Use interrupt, steer, FIFO queue, prompt reply, or new
  session.
- Notify replies, errors, input, background results, attention transitions, and
  configured cron failures. Kanban completion stays opt-in. Suppress visible
  focused targets, redact prompts, and return clicks to owner.
- Use `read-string-from-buffer` for multiline input; `read-string` otherwise.

## Elisp

- Prefer pure helpers and plain values. Keep I/O and state mutation thin.
- Read protocol values through shared `hermes-transport-*` accessors; support
  hash tables, alists, and plists.
- Prefer collection transforms when clearer; imperative loops remain valid.
- Prefer `when` for effects, `and` for values.
- Keep functions easy to review, usually under 35 lines.
- No `declare-function` between `hermes-chat-*` siblings. Move helpers to owner
  or use registries/hooks. Reserve declarations for soft external dependencies.
- One space between tokens. No column alignment. Comments explain why. Use plain
  `;;; Section name` headings.

## Workflow

1. Inspect Git state, scoped source/tests, and backend contract.
2. Discuss non-trivial boundaries. Plan cross-module, protocol, lifecycle,
   security, or migration work.
3. Start behavior changes with focused failing ERT or reproducer when practical.
4. Make smallest coherent change. Preserve unrelated work.
5. Run focused checks, then risk-appropriate gates. Review full diff and repeat.

- Focused ERT: `make test TESTS=tests/hermes-chat-tests.el`
- Compile: `make compile`
- Commit gate: `make pre-commit`
- Large/release/dependency/flake gate: `make pre-handoff-check`
- Live dogfood: `make load`
- Whitespace: `git diff --check`

Run `make pre-commit` before authorized commits. Live dogfood supplements batch
tests. Never commit, amend, rebase, push, deploy, or run destructive Git without
explicit authorization. Stage only scoped verified files. Use repository
identity and short `module: Change` subjects. No generated-by or co-author
metadata.

## Review and handoff

- Trivial: full diff self-review plus smallest check.
- Behavior: targeted tests plus full diff self-review.
- Lifecycle, protocol, public API, auth, secrets, destructive work, broad
  refactor: independent adversarial review against fixed commit, tree, or patch
  hash. Review code/tests, fix findings, rerun gates, review changed candidate.
- Green tests count only when assertions prove claimed behavior.
- Keep one feature per execution unit unless dependency, ownership, risk, or
  publication needs a split. Create dependent cards in dependency order.
- Handoffs include changed files, commands/results, candidate identity, verdict,
  blockers, and dogfood status. Close work only with evidence.
- Keep cards, comments, commits, and handoffs public-safe.
