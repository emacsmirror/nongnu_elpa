# AGENTS.md

Public guidance for contributors and coding agents working on `emacs-jabber`.

## Project

`emacs-jabber` is an XMPP client for GNU Emacs 29.1+. Emacs Lisp sources live
in `lisp/`, ERT tests in `tests/`, and the optional OMEMO module in `src/`.
Runtime package dependencies are `fsm` and `keymap-popup`.

## Architecture

- `jabber-core.el` owns the `jabber-connection` finite-state machine;
  `jabber-conn.el` owns transport. Preserve state transitions for connection,
  authentication, binding, Stream Management, resume, and disconnect.
- `jabber-stanza.el` dispatches incoming IQ, message, and presence stanzas
  through `jabber-iq-chain`, `jabber-message-chain`, and
  `jabber-presence-chain`. Add protocol behavior through the appropriate chain.
- Keep module dependencies layered. In particular, `jabber-chatbuffer.el` must
  remain independent of `jabber-chat.el`, which already depends on it.
- Keep parsing and decisions separate from network, SQLite, process, buffer,
  timer, and notification effects. Async callbacks must verify that their
  connection, process, and buffer are still live.

## Protocol and data safety

- Read the relevant XEP before changing protocol behavior. Account for every
  applicable MUST and SHOULD, preserve namespaces and fallback rules, and add
  focused stanza tests. Update `doap.xml` when support status changes.
- SQLite contains real user history and encryption state. Schema changes need a
  forward migration in `jabber-db--migrate`, an updated fresh schema/version,
  adjusted queries, and migration tests. Never replace migration with a
  destructive database rebuild.
- Treat OMEMO and OpenPGP changes as security-sensitive. Preserve encrypted
  stanza deduplication: the Double Ratchet must not process the same ciphertext
  twice. Never replace valid stored text with a failed-decryption placeholder.
- Never expose credentials, message plaintext, keys, or trust data in logs,
  errors, fixtures, or test artifacts.

## Emacs Lisp conventions

- Use lexical binding. Public names use `jabber-`; internal helpers use
  `jabber--` or `jabber-MODULE--`.
- Keep interactive commands thin. Prefer small pure helpers and explicit data
  flow; apply effects at module boundaries.
- Use `defvar-local` for buffer state. Every `defcustom` needs an accurate
  `:type` and `:group`.
- Use `require` for real module dependencies. Reserve `declare-function` for a
  verified load cycle or the external OMEMO module.
- Keep changes focused and add ERT coverage for changed behavior, especially
  stanza handling, reconnection, persistence, and encryption paths.

## Verification

The Makefile enters the Nix development environment when available.

```sh
make test      # Parallel ERT files plus the OMEMO module
make lint      # check-declare, checkdoc, package-lint, relint, test compile
make module    # Build the OMEMO dynamic module
make dev       # Autoloads, compile, module, lint, isolated and one-shot tests
make release-check # Local gates plus the Debian dh_elpa build and installed tests
```

Use `make dev` before submission. Tests must use temporary databases and must
never open a user's live Jabber database; the Makefile sets `jabber-db-path` to
nil for batch tests.

Always run `make release-check` before creating or pushing a release tag. Do not
tag or push a release unless it passes.

## Contributions

Send patches to <patches@thanosapollo.org> with a subject like
`[PATCH emacs-jabber] Short description`.

Send bugs and feature requests to <bugs@thanosapollo.org> with a subject like
`[BUG emacs-jabber] Short description`.
