# emacs-codex

Project-specific guidance for the Emacs Codex integration.

## Local context

- Keep machine-specific paths in untracked `AGENTS.local.md`; start from
  `AGENTS.local.example.md`.
- When local reference checkouts are needed, read `AGENTS.local.md` first.
  If it is missing, use repo search and normal discovery.
- Useful references are the Codex source checkout for CLI behavior, Emacs core
  for process/window/buffer conventions, and `keymap-popup` for menu behavior.

## Architecture

- Preserve the terminal-first design: eat session wrapper first, `/ide`
  context IPC second, local MCP tools third.
- Terminal buffers are the primary user surface.  Live sessions are grouped by
  project root and may have multiple buffers per root.
- Do not replace terminal behavior with MCP or subprocess shortcuts unless the
  task asks for that design change.

## Development

- Keep edits scoped to the requested behavior and preserve existing WIP.
- For normal validation run `make dev`.
- While iterating, prefer targeted ERT plus `git diff --check`.
- For serious handoff when Nix is available, run `make pre-handoff-check`.

## Elisp

- All Emacs Lisp files use `; -*- lexical-binding: t; -*-`.
- Public APIs use `codex-ide-`; internals use `codex-ide--`.
- Keep interactive commands thin: gather context, call helpers, apply effects.
- Keep pure computation separate from buffers, processes, windows, hooks, and
  user-visible state.
- Prefer explicit data flow through arguments and return values over hidden
  globals.
- Use plists or alists for transient session data unless a stronger record
  contract is needed.
- Comments should explain why.  Section headers are plain, for example
  `;;; Process lifecycle`.

## Tests

- Tests use ERT, lexical binding, and package code from the project load path.
- Use temp buffers and temp directories; never modify user config or real user
  data.
- Keep tests focused on one contract, especially around terminal buffers,
  process ownership, session recovery, and context/MCP boundaries.
