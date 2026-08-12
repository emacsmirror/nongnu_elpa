# emacs-codex-ide

Codex CLI in Emacs via eat. Emacs 29.1+. Sources `lisp/`, tests `tests/`.
Deps: `compat`, `keymap-popup`, `eat`.

## Public safety

Public doc. No local paths, hosts, private topology, secrets, personal config.
Repo-relative paths only. Durable architecture only.

## Architecture

Terminal-first. Order fixed: eat session wrapper, `/ide` context IPC, local
MCP tools. Terminal buffers primary surface. No MCP/subprocess shortcut that
replaces terminal unless task asks that design change.

- Sessions group by project root; multi buffer per root OK.
- Context IPC: Unix socket, length-prefixed JSON; Codex TUI frame format wins.
- MCP: transient `-c mcp_servers.emacs_tools.url` only. Never write
  `~/.codex/config.toml` except `codex-ide-mcp-install-codex-config`.
- Diff preview writes no files; caller owns path.
- `codex-ide-stop` kills active project session only; same-root siblings stay.
- Owner holds process/server/socket/timer/client. Cleanup local, idempotent,
  quiet. Capture buffer + session/root/id before async. Mutate only if buffer,
  mode, identity still current. Stale callback no-op.

## Elisp

Lexical binding. Public `codex-ide-`; internal `codex-ide--`. Thin interactive
commands. Pure helpers off buffers/processes/windows/hooks/UI state. Explicit
args/returns over hidden globals. Plist/alist for transient session data unless
stronger record needed. Comments explain why. Plain `;;; Section` headers.

## Verification

```sh
make dev                 # compile, checkdoc, ERT
make test                # ERT
make pre-commit          # whitespace + compile + lint + native-comp + ERT
make pre-handoff-check   # status, diff check, nix develop + flake check
git diff --check
```

Iterate: targeted ERT + `git diff --check`. ERT on package load path; temp
buffers/dirs only. Cover terminal, process ownership, session recovery,
context/MCP boundaries, reject/disconnect/kill/replace/stale callback. Never
touch user config or real user data.
