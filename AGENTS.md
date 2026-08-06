# emacs-jabber

XMPP client for GNU Emacs 29.1+. Sources `lisp/`, ERT `tests/`, optional OMEMO
module `src/`. Runtime deps: `fsm`, `keymap-popup`.

## Architecture

- `jabber-core.el` owns `jabber-connection` FSM; `jabber-conn.el` owns
  transport. Preserve connect, auth, bind, Stream Management, resume,
  disconnect.
- `jabber-stanza.el` dispatches IQ/message/presence via `jabber-iq-chain`,
  `jabber-message-chain`, `jabber-presence-chain`. Add protocol behavior on the
  right chain.
- Keep modules layered. `jabber-chatbuffer.el` must stay independent of
  `jabber-chat.el` (chat already depends on it).
- Separate parsing/decisions from network, SQLite, process, buffer, timer, and
  notification effects. Async callbacks must prove connection, process, and
  buffer still live.

## Protocol and data safety

- Read the XEP first. Cover applicable MUST/SHOULD, keep namespaces and
  fallbacks, add focused stanza tests. Update `doap.xml` when support status
  changes.
- SQLite holds history and encryption state. Schema changes need forward
  migration via `jabber-db--migrate`, fresh schema/version, query updates, and
  migration tests. Never rebuild the DB destructively.
- OMEMO/OpenPGP are security-sensitive. Preserve encrypted-stanza dedup: Double
  Ratchet must not process the same ciphertext twice. Never replace valid
  stored text with a failed-decryption placeholder.
- Never expose credentials, message plaintext, keys, or trust data in logs,
  errors, fixtures, or test artifacts.

## Elisp

- Lexical binding. Public `jabber-`; internal `jabber--` or `jabber-MODULE--`.
- Thin interactive commands. Small pure helpers; effects at module boundaries.
- `defvar-local` for buffer state. Every `defcustom` needs accurate `:type` and
  `:group`.
- `require` real deps. `declare-function` only for verified load cycles or the
  external OMEMO module.
- Focused diffs. ERT for stanza, reconnection, persistence, encryption changes.

## Verification

Makefile enters Nix dev env when available.

```sh
make test           # Parallel ERT + OMEMO module
make lint           # check-declare, checkdoc, package-lint, relint, test compile
make module         # OMEMO dynamic module
make dev            # Autoloads, compile, module, lint, isolated + one-shot tests
make release-check  # Local gates + Debian dh_elpa build and installed tests
```

Run `make dev` before submission. Tests use temporary DBs only; never open a
user's live Jabber DB (batch sets `jabber-db-path` nil). Run
`make release-check` before any release tag; do not tag or push a release
without it.

## Contributions

Patches: <patches@thanosapollo.org> subject `[PATCH emacs-jabber] …`

Bugs/features: <bugs@thanosapollo.org> subject `[BUG emacs-jabber] …`
