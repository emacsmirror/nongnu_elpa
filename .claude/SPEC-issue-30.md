# Spec: restore non-file-visiting buffers on session restore (v5)

Tracking: https://github.com/mclear-tools/tabspaces/issues/30

## 1. Goal

Restore non-file-visiting buffers as part of `tabspaces-restore-session`. The
package ships handlers for three core-Emacs kinds (`dired`, `eshell`, `shell`)
and exposes a registration API so users teach tabspaces about any other kind
(`term`, `vterm`, `eat`, `ielm`, ...) from their config. The hook is the
headline; the three built-ins cover the buffer kinds every Emacs install has
and that #30 reporters explicitly named.

Out of scope: replaying shell/eshell history or output; reviving running
processes; magit/help/info/compilation/image-dired buffers; vterm/eat/term
(covered by user registration with snippets in README).

## 2. Current state

Save (`tabspaces.el:846-852`):
```elisp
(defun tabspaces--buffile (b)
  (cl-remove-if nil (buffer-file-name b)))     ; broken: predicate cannot be nil

(defun tabspaces--store-buffers (bufs)
  (flatten-tree (mapcar #'tabspaces--buffile bufs)))
```
Returns a flat list of file paths; non-file buffers are dropped. The
`cl-remove-if nil ...` is broken-by-accident: `cl-remove-if` requires a
function as the first argument; the call "works" only because
`(buffer-file-name b)` is a string which `flatten-tree` then passes through
unchanged.

Session entry shape: `(BUFFER-LIST TAB-NAME WINDOW-STATE)`. On-disk format is
`(setq tabspaces--session-list '(...))` written by four save functions
(`tabspaces.el:856-1008`).

Restore (`tabspaces.el:1094-1110`):
```elisp
(cl-loop for elm in tabspaces--session-list do
         (switch-to-buffer "*tabspaces--placeholder*")
         (tabspaces-switch-or-create-workspace (cadr elm))
         (mapc #'find-file (car elm))
         (when (caddr elm)
           (window-state-put (caddr elm) nil 'safe)))
(cl-loop for elm in tabspaces--session-list do        ; placeholder cleanup
         (tabspaces-switch-or-create-workspace (cadr elm))
         (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
(kill-buffer "*tabspaces--placeholder*")
(message "Restored session from %s" session-file)
```

## 3. Record format

A buffer record is one of:

- **Bare string**: file path, restored with `find-file`. Existing session
  files contain only strings.
- **Plist**: `(:kind SYMBOL :dir DIR :name NAME ...)` where `:kind` identifies
  the registered handler. `:dir` is `default-directory` at save time; `:name`
  is `(buffer-name)` at save time.

**Format policy.** File records remain bare strings indefinitely. If richer
per-file metadata is added later (point, narrowing), it is encoded as
`(:kind file :path PATH ...)` plists, and bare strings stay valid as the
no-metadata shorthand. `stringp` is the implicit type-tag for file records;
the `stringp` arm of the restore dispatch guarantees forever-compat.

**Compat direction.** Old → new restore: works (only strings). New → old
restore: **breaks**. Old code calls `find-file` on every element; `find-file`
on a plist signals `wrong-type-argument` and aborts that tab's restore loop
mid-flight, leaving placeholder buffers behind. Recovery: kill the placeholder
manually and delete the session file. Document in commit and README NEWS.

## 4. Registration API

```elisp
(defvar tabspaces--buffer-kind-handlers nil
  "Alist of (KIND SAVE-FN RESTORE-FN) for non-file buffer kinds.
SAVE-FN takes a buffer; returns a plist record (with :kind) or nil to skip.
RESTORE-FN takes a plist record; returns the created buffer or nil.
KIND is the symbol used as the :kind value in serialized records.

The alist is walked front-to-back on save: the first SAVE-FN that
returns non-nil for a given buffer wins. On restore, the entry is
looked up by KIND via assq.")

(defvar tabspaces--restore-unknown-kinds nil
  "Accumulator for unknown :kind values encountered during restore.
Dynamically bound inside `tabspaces-restore-session'; defvared so the
helper `tabspaces--restore-buffer-record' can push to it from outside
the let-binding scope under lexical-binding.")

(defun tabspaces-register-buffer-kind (kind save-fn restore-fn)
  "Register handlers for KIND.

Re-registering KIND replaces any prior entry. Re-registrations are
prepended to the alist, so the most recently registered handler runs
first on save. The three built-in handlers (`dired', `eshell', `shell')
are registered at the end of tabspaces.el; user registrations issued
after `(require 'tabspaces)' therefore take precedence on save."
  (setq tabspaces--buffer-kind-handlers
        (cons (list kind save-fn restore-fn)
              (assq-delete-all kind tabspaces--buffer-kind-handlers))))
```

**Override-ordering trap.** Register at top level after `(require 'tabspaces)`,
not inside `(with-eval-after-load 'THIRD-PARTY-PACKAGE ...)`. Here's why:

When `tabspaces-session-auto-restore` is t, `tabspaces-mode 1` fires
`tabspaces--restore-session-on-startup` immediately. If the user's vterm
handler is wrapped in `(with-eval-after-load 'vterm ...)`, vterm itself
won't be loaded yet (it's typically autoloaded lazily), so the registration
hasn't run when the restore fires. The vterm records get the unknown-kind
treatment: silently skipped, contributing to a single summary message at end
of restore ("tabspaces: unknown buffer kinds in session: (vterm)"). The user
opens vterm later, the autoload fires, the handler registers — too late.

Canonical pattern:

```elisp
;; In init.el, after (require 'tabspaces):
(tabspaces-register-buffer-kind
 'vterm
 (lambda (b)                                          ; save-fn closure
   (with-current-buffer b
     (when (derived-mode-p 'vterm-mode)               ; vterm not loaded? fine; nil
       (list :kind 'vterm :dir default-directory :name (buffer-name)))))
 (lambda (rec)                                        ; restore-fn closure
   (require 'vterm)                                   ; load lazily, on demand
   (let ((default-directory (plist-get rec :dir)))
     (vterm (plist-get rec :name)))))
```

The closures themselves don't require vterm at definition time. The save-fn's
`derived-mode-p` check is safe to evaluate without vterm loaded (it just
returns nil). The restore-fn loads vterm only when a vterm record is being
restored. For users whose vterm is not pre-compiled, the first restore pays
the C-module build cost; subsequent restores are free.

**Custom-handler constraint.** A restore-fn should create buffers but must
NOT call window-configuration-changing functions (`pop-to-buffer-other-window`,
`switch-to-buffer-other-window`, `delete-other-windows`). Those operations
are reverted by the outer loop's `save-window-excursion`; the user-visible
result is "my handler ran but the window layout is wrong." The outer loop
calls `window-state-put` to set the layout; restore-fns must not.

## 5. Save-side

```elisp
(defun tabspaces--buffer-record (b)
  "Return a serializable record for buffer B, or nil to skip."
  (when (buffer-live-p b)
    (with-current-buffer b
      (cond
       (buffer-file-name buffer-file-name)            ; file: bare string
       (t (catch 'found
            (dolist (entry tabspaces--buffer-kind-handlers)
              (let ((rec (funcall (nth 1 entry) b)))
                (when rec (throw 'found rec))))
            nil))))))

(defun tabspaces--store-buffers (bufs)
  "Return list of serializable records, skipping unhandled buffers."
  (delq nil (mapcar #'tabspaces--buffer-record bufs)))
```

The four existing save call sites (lines 869, 905, 946, 986) keep their
existing `(tabspaces--store-buffers (tabspaces--buffer-list))` invocation.
List element type widens from `string` to `string-or-plist`; no downstream
code depends on element type.

`buffer-live-p` guards against the save aborting when a hook kills a buffer
between `tabspaces--buffer-list` returning and `mapcar` reaching it. Single-
threaded Emacs makes this rare in practice; the guard is one line of insurance.

## 6. Restore-side

### 6.1 Dispatch

```elisp
(defun tabspaces--restore-buffer-record (rec)
  "Materialize one buffer from a session record.
Returns the buffer created/reused, or nil if skipped.
Pushes unknown kinds into the dynamically-bound
`tabspaces--restore-unknown-kinds' accumulator.
Catches errors from user-registered handlers so one buggy handler
does not abort the entire restore loop."
  (cond
   ((stringp rec) (find-file rec))
   ((and (consp rec) (plist-get rec :kind))
    (let* ((kind (plist-get rec :kind))
           (entry (assq kind tabspaces--buffer-kind-handlers))
           (restore-fn (and entry (nth 2 entry))))
      (cond
       (restore-fn
        (condition-case err
            (funcall restore-fn rec)
          (error
           (message "tabspaces: handler for %s signalled: %S" kind err)
           nil)))
       (t (push kind tabspaces--restore-unknown-kinds)
          nil))))))
```

The `condition-case` wraps the user-handler call, not the user-handler body.
Built-in handlers also have their own internal `condition-case` (see §7) so a
stale `:dir` doesn't even reach this layer; but a user's broken handler
signals here and gets contained with a single message.

### 6.2 Per-tab dedup helper

```elisp
(defun tabspaces--reuse-existing-buffer (name)
  "Return the buffer named NAME iff it is already in this tab's buffer-list.
Returns nil if no such buffer exists, or if it exists in another tab.
Callers should fall through to create a fresh buffer when nil is returned."
  (let ((existing (get-buffer name)))
    (and existing
         (memq existing (tabspaces--buffer-list))
         existing)))
```

Tab-isolated dedup. If a buffer with the saved name exists in *this* tab
(because `tabspaces-mode` startup hooks already rebuilt it, or desktop.el
cooperation), reuse it. If it exists in another tab, treat as a name collision
and create fresh — workspace isolation wins over name preservation.

**Single-frame assumption.** `(tabspaces--buffer-list)` reads the current
frame's tab buffer-list. The whole restore loop drives a single frame (the
one selected when `tabspaces-restore-session` is called). Calling
`tabspaces-restore-session` from a second frame performs a second full
restore; same-name buffers from the first frame are not reused (the dedup
check sees them as "in another frame's tab" and falls through). This is the
intended behavior; multi-frame coordinated restore is not in scope.

### 6.3 Window-state name rewriter

```elisp
(defun tabspaces--rewrite-window-state (state subst)
  "Return STATE with buffer NAMEs updated per SUBST.
SUBST is an alist of (saved-name . actual-name)."
  (cond
   ((null subst) state)
   ((and (consp state) (eq (car state) 'buffer) (stringp (cadr state)))
    (let ((repl (assoc (cadr state) subst)))
      (if repl (cons 'buffer (cons (cdr repl) (cddr state))) state)))
   ((and (consp state) (eq (car state) 'next-buffers))
    (cons 'next-buffers
          (mapcar (lambda (n)
                    (let ((r (assoc n subst))) (if r (cdr r) n)))
                  (cdr state))))
   ((and (consp state) (eq (car state) 'prev-buffers))
    (cons 'prev-buffers
          (mapcar (lambda (e)
                    (let ((r (assoc (car e) subst)))
                      (cond
                       ;; Substitute: drop stale markers, use buffer-start
                       ;; positions. `window--state-put-2' calls
                       ;; `set-marker (make-marker) 1 buffer' for integer
                       ;; positions -- defined behavior, navigation lands at
                       ;; point-min instead of relying on the nil-position
                       ;; detached-marker path.
                       (r (list (cdr r) 1 1))
                       (t e))))
                  (cdr state))))
   ((consp state)
    (cons (tabspaces--rewrite-window-state (car state) subst)
          (tabspaces--rewrite-window-state (cdr state) subst)))
   (t state)))
```

Three buffer-name reference shapes from `window-state-get t`:
- `(buffer NAME . _)` for the selected buffer per window.
- `(next-buffers . (NAMES))` for forward navigation history.
- `(prev-buffers . ((NAME M1 M2) ...))` for backward history with markers.

For substituted `prev-buffers` entries we emit `(NAME 1 1)`. Markers from a
serialized session file are stale by restore time anyway; substituted entries
get fresh markers at position 1 via `window--state-put-2`'s integer-position
path. Navigation works; saved point is lost (lands at point-min for
substituted slots only). Non-substituted entries pass through unchanged.

### 6.4 Main restore loop (replaces `tabspaces.el:1094-1110`)

```elisp
(let ((tabspaces--restore-unknown-kinds nil)         ; dynamic rebinding
      (skipped-remote 0))
  (cl-loop for elm in tabspaces--session-list do
           (switch-to-buffer "*tabspaces--placeholder*")
           (tabspaces-switch-or-create-workspace (cadr elm))
           (let ((subst nil))
             (save-window-excursion
               (dolist (rec (car elm))
                 (let* ((remote
                         (cond ((stringp rec) (file-remote-p rec))
                               ((consp rec) (file-remote-p (plist-get rec :dir))))))
                   (cond
                    (remote (cl-incf skipped-remote))
                    (t (let ((buf (tabspaces--restore-buffer-record rec))
                             (sname (and (consp rec) (plist-get rec :name))))
                         (when buf
                           (switch-to-buffer buf)        ; force into buffer-list
                           (when (and sname
                                      (not (equal sname (buffer-name buf))))
                             (push (cons sname (buffer-name buf)) subst)))))))))
             (when (caddr elm)
               (window-state-put
                (tabspaces--rewrite-window-state (caddr elm) subst)
                nil 'safe))))

  ;; Placeholder cleanup (unchanged from current code at lines 1105-1108)
  (cl-loop for elm in tabspaces--session-list do
           (tabspaces-switch-or-create-workspace (cadr elm))
           (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
  (when (get-buffer "*tabspaces--placeholder*")
    (kill-buffer "*tabspaces--placeholder*"))

  ;; Summary messages (informational; the final confirmation message below
  ;; is what shows in the minibuffer)
  (when (> skipped-remote 0)
    (message "tabspaces: %d remote buffer(s) skipped (TRAMP)" skipped-remote))
  (when tabspaces--restore-unknown-kinds
    (message "tabspaces: unknown buffer kinds in session: %S"
             (delete-dups tabspaces--restore-unknown-kinds)))

  ;; Final confirmation message (preserve from current line 1109)
  (message "Restored session from %s" session-file))
```

Four correctness moves over current code:

1. **TRAMP check at dispatch level for BOTH strings and plists.** File records
   that happen to be TRAMP paths are also skipped.
2. **`save-window-excursion`** around the per-record dolist. The per-record
   `switch-to-buffer` calls add buffers to `frame-parameter 'buffer-list`
   (side effect that persists across the excursion since buffer-list isn't
   part of window state); the window layout is reverted to the
   post-workspace-switch state, so `window-state-put` sees a clean canvas.
3. **Name substitution** before `window-state-put` handles cases where a
   handler returned a buffer with a different name than the saved one.
4. **Single summary message** for TRAMP skips and unknown kinds. The "Restored
   session from %s" message remains last so the minibuffer shows the
   confirmation, not the warnings.

## 7. Built-in handlers

Registered at the end of tabspaces.el's session section. Each handler body
is wrapped in `condition-case` that logs the error and returns nil, so a
single broken `:dir` (missing directory, permission error, eshell process
failure) drops just that buffer and leaves a breadcrumb in `*Messages*`
instead of poisoning the rest of the tab's restore.

Each handler also validates `:dir` is a usable directory before proceeding,
since `(let ((default-directory nil)) ...)` doesn't raise but produces silently
wrong results.

### 7.1 dired

```elisp
(tabspaces-register-buffer-kind
 'dired
 (lambda (b)
   (with-current-buffer b
     (when (and (derived-mode-p 'dired-mode)
                (not (derived-mode-p 'image-dired-thumbnail-mode))
                (not (consp dired-directory)))         ; skip virtual dired
       (list :kind 'dired :dir default-directory :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      (t (or (tabspaces--reuse-existing-buffer name)
             (condition-case err
                 (let* ((default-directory dir)
                        ;; Remove only the stale cross-tab entry for DIR from
                        ;; the global dired-buffers cache. Setting the whole
                        ;; alist to nil would orphan the new advertise from
                        ;; the global registry (dired-mode's dired-advertise
                        ;; setqs into the locally-bound nil), breaking dired's
                        ;; cross-buffer tracking.
                        (dired-buffers
                         (assq-delete-all (expand-file-name dir) dired-buffers))
                        (buf (dired-noselect dir)))
                   (when (and buf
                              (not (equal name (buffer-name buf)))
                              (not (get-buffer name)))    ; rename only if free
                     (with-current-buffer buf (rename-buffer name)))
                   buf)
               (error
                (message "tabspaces: dired restore skipped (%s): %S" dir err)
                nil))))))))
```

Notes:
- `dired-noselect` returns the buffer without popping a window. The outer
  restore loop handles display via `switch-to-buffer` and `window-state-put`.
- The `(dired-buffers (assq-delete-all (expand-file-name dir) dired-buffers))`
  rebind removes the cross-tab cache entry for DIR but leaves the rest of
  the alist intact. When `dired-mode` then calls `dired-advertise` on the
  new buffer, the `setq dired-buffers` mutation lands in the global registry
  (via the dynamic binding inherited from the rest of the alist), preserving
  dired's tracking machinery. Setting `dired-buffers` to nil entirely would
  let the advertise mutate only the local binding, orphaning the new buffer
  from cross-buffer dired operations.
- `image-dired-thumbnail-mode` derives from `dired-mode` but has its own
  display semantics; explicitly excluded on save.
- Virtual dired (`(consp dired-directory)` from `find-dired`, `tar-mode`
  contents listings) is excluded; no reasonable reconstruction.

### 7.2 eshell

```elisp
(tabspaces-register-buffer-kind
 'eshell
 (lambda (b)
   (with-current-buffer b
     (when (derived-mode-p 'eshell-mode)
       (list :kind 'eshell :dir default-directory :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      (t (or (tabspaces--reuse-existing-buffer name)
             (condition-case err
                 (let* ((default-directory dir)
                        ;; Use the saved buffer name. (eshell t) calls
                        ;; (generate-new-buffer eshell-buffer-name), which
                        ;; adds `<N>' on collision. No manual rename needed.
                        (eshell-buffer-name name)
                        (buf (eshell t)))                ; returns the buffer
                   buf)
               (error
                (message "tabspaces: eshell restore skipped (%s): %S" dir err)
                nil))))))))
```

Critical notes:
- `project-eshell` is NOT used. It internally rebinds `default-directory` to
  the project root, discarding any saved sub-directory; and its `(eshell
  current-prefix-arg)` invocation does not force a new buffer in
  non-interactive contexts, which causes cross-tab buffer sharing. Instead,
  the handler binds `default-directory` AND `eshell-buffer-name` (to the
  saved `:name`, which carries any project prefix that was applied at save
  time) and calls `(eshell t)` directly. The force-new arg ensures
  `generate-new-buffer` is used; `default-directory` propagates to the
  eshell process; no project state is needed at restore time.
- `(eshell t)` returns the newly created buffer. Verified against eshell.el
  in Emacs 27/28/29/30: the `t` arg routes through `generate-new-buffer
  eshell-buffer-name` and the function returns that buffer.
- The `<N>` suffix on cross-tab collision is captured by the outer loop's
  substitution alist; `window-state-put` uses the actual buffer name.

### 7.3 shell

```elisp
(tabspaces-register-buffer-kind
 'shell
 (lambda (b)
   (with-current-buffer b
     (when (derived-mode-p 'shell-mode)
       (list :kind 'shell :dir default-directory :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      (t (or (tabspaces--reuse-existing-buffer name)
             (condition-case err
                 (let* ((default-directory dir)
                        ;; shell reuses an existing buffer with the given
                        ;; name (cross-tab leak risk); generate-new-buffer-name
                        ;; pre-resolves to a guaranteed-unique name.
                        (buf (shell (generate-new-buffer-name name))))
                   buf)
               (error
                (message "tabspaces: shell restore skipped (%s): %S" dir err)
                nil))))))))
```

Notes:
- `shell` consults `explicit-shell-file-name` then `(getenv "SHELL")` then
  `shell-file-name` to pick the program. Default behavior; the handler
  doesn't override.
- **Restore semantics for shell.** Unlike eshell (pure elisp, no process to
  revive), shell-mode spawns an external process. Restore creates a *fresh*
  shell process at the saved directory with NO history continuity — the old
  process is gone. For most users this is the right outcome (a shell in the
  right place); users with named shells like `*shell-prod*` who depend on
  history should know the restored buffer starts empty. Documented in README
  caveats.

## 8. TRAMP handling

The dispatch in §6.4 checks `(file-remote-p ...)` for both string and plist
records before invoking any handler. Remote records are counted; one summary
message ("tabspaces: N remote buffer(s) skipped (TRAMP)") is emitted at the
end of restore.

Users who do want their TRAMP buffers back currently have no first-class
support — they'd need to manually re-open after Emacs starts. Lazy TRAMP
restore (deferred-open stubs that connect on first selection) is the right
answer for a future iteration; not in scope here.

## 9. Backward compatibility

- **Old sessions → new restore:** works. `stringp` arm handles every record.
- **New sessions → old restore:** breaks. Old code's `find-file` on a plist
  signals `wrong-type-argument` mid-`mapc`, aborting that tab's restore and
  leaving the placeholder buffer behind in unprocessed tabs. Recovery: kill
  the `*tabspaces--placeholder*` buffer manually and delete the session file.
- **No defcustom rename, no schema-version field.** Mixed-type list uses
  `stringp` as the implicit type-tag. §3 documents the policy.
- **Session file trust boundary unchanged.** The session file is already
  `load-file`-d (arbitrary elisp executes). New plist records pass `:dir`
  values to handlers that may create processes (eshell, shell). This is the
  same trust model as the existing `find-file` on saved paths — a corrupted
  or attacker-controlled session file can already direct file or process
  creation. Treat session files as trusted as your config directory.
- **`cl-remove-if nil` dead/broken code is dropped.** The current
  `tabspaces--buffile` passes `nil` as the predicate to `cl-remove-if`, which
  is invalid; the function "worked" only because `flatten-tree` on a string
  return value passes through. Commit message should note this as an
  incidental fix.

## 10. Out of scope (v1 of this feature)

- term, vterm, eat, ielm, term-mode (any terminal/REPL kind beyond shell).
  All supportable via user-registered handlers; README provides snippets.
- Shell/eshell history or output replay.
- magit/help/info/compilation buffers (no clean reconstruction).
- image-dired thumbnails (explicitly excluded on save).
- Fixing the pre-existing TRAMP hang for file records (now skipped, but
  proper lazy restore is a separate feature).
- Multi-frame coordinated restore.
- Warning on re-registration of an already-known kind (silent overwrite).

## 11. Implementation outline

1. **Registration API** (~20 LOC) near the session defcustoms
   (`tabspaces.el:800-840`): add `tabspaces--buffer-kind-handlers` defvar,
   `tabspaces--restore-unknown-kinds` defvar (needed for dynamic binding),
   and `tabspaces-register-buffer-kind` function.
2. **Save helpers** (~20 LOC): replace `tabspaces--buffile` /
   `tabspaces--store-buffers` at `tabspaces.el:846-852` per §5.
3. **Restore helpers** (~50 LOC): add `tabspaces--restore-buffer-record`,
   `tabspaces--reuse-existing-buffer`, `tabspaces--rewrite-window-state`.
4. **Restore loop** (~40 LOC): patch `tabspaces.el:1094-1110` per §6.4.
   Preserve the "Restored session from %s" confirmation message as the
   final line of the restore body.
5. **Built-in handlers** (~50 LOC total): three `tabspaces-register-buffer-kind`
   calls per §7. **MUST be the last top-level forms in tabspaces.el's session
   section** so user registrations after `(require 'tabspaces)` land ahead
   of them. Add an inline code comment pointing to §4 of this spec.
6. **README updates** (~90 lines of prose + code): see §12.
7. **Commit message**: notes one-way compat break, mentions `cl-remove-if nil`
   incidental fix.

Total: ~180 LOC + README. Estimate 4-6 hours including testing, debugging,
and docs.

## 12. README plan

Add a new subsection to the existing "Persistent Tabspaces" section
documenting the registration API. Budget: 85-100 lines of prose and code.

1. **Intro paragraph (~8 lines).** Names what's built in (dired, eshell,
   shell), and what's user-extensible. Notes that file buffers were already
   supported and remain so.

2. **API signature with semantics (~12 lines).**
   `(tabspaces-register-buffer-kind KIND SAVE-FN RESTORE-FN)`. SAVE-FN
   contract (takes a buffer; returns a plist with `:kind` or nil to skip).
   RESTORE-FN contract (takes a plist; returns the created buffer or nil
   to skip). When each is called. Handler-body constraint: must not call
   window-configuration-changing functions.

3. **Annotated vterm example (~28 lines).** Working handler with inline
   comments covering: `derived-mode-p` save-time check, the `:dir`/`:name`
   plist shape, dedup via `tabspaces--reuse-existing-buffer`, why
   `default-directory` is let-bound, why TRAMP-skip doesn't need handler
   code (dispatch handles it), and the recommendation that user handlers
   wrap their body in `condition-case` for resilience.

4. **Override-ordering documentation (~12 lines).** The trap: registering
   inside `(with-eval-after-load 'vterm ...)` plus `tabspaces-session-auto-restore`
   means vterm records get the silent unknown-kind skip on startup. The
   canonical pattern: register at top-level after `(require 'tabspaces)`,
   load the third-party package lazily inside the closure body. Note that
   `(require 'vterm)` inside the restore-fn pays the package's first-load
   cost during restore — for users whose package is not pre-compiled, this
   is a one-time pause.

5. **Caveats (~15 lines).**
   - Cross-tab name collisions produce `<N>`-suffix buffers (per-tab dedup,
     not global; workspace isolation wins).
   - TRAMP paths are skipped on restore with a summary message; no per-record
     spam, no hang.
   - Restored shell buffers are *fresh processes* at the saved directory; no
     command history, no running state. Same is true for any user-registered
     process-spawning handler (term, vterm, eat).
   - Session files written by v5 cannot be read by older versions.
   - Single-frame restore: calling `tabspaces-restore-session` on a second
     frame performs a second full restore.

6. **Snippet appendix (~25 lines).** Two short additional handlers users can
   copy: `term` and `ielm`. Each ~10-12 lines, structured like the eshell
   built-in but without the project-prefixed-name wrinkle. Comment on each
   notes what's different (e.g., for `term`: program argument). For users
   with vterm or eat, the §3 annotated example is their template.

## 13. Test plan

No formal test infra in the repo. Manual matrix run inside `emacs -Q -l
tabspaces.el` against two scratch projects (see `/tmp/tabspaces-bug80-test/`
for the layout from #80's fix). Each row has a concrete predicate.

| #   | Setup                                                          | Verify                                                            |
|-----|----------------------------------------------------------------|-------------------------------------------------------------------|
| 1   | Tab with 2 file buffers; save+restart+restore                  | `(length (tabspaces--buffer-list))` ≥ 2; both files present       |
| 2   | Tab with file + project-eshell; save+restore                   | Buffer named `*project-FOO-eshell*` present; mode = eshell-mode   |
| 3   | Tab with only dired; save+restore                              | `(eq major-mode 'dired-mode)` in selected window; tab buffer-list shows the buffer |
| 4   | Tab with only `*shell*`; save+restore                          | `(eq major-mode 'shell-mode)`; `default-directory` matches saved  |
| 5   | Old session file (strings only); restore                       | Identical to v1.x behavior; no error                              |
| 6   | Session w/ `(:kind unknown ...)` for ≥1 record; restore        | Single summary message logged; other buffers restored             |
| 7   | Two tabs each with `*eshell*` in different `:dir`s; save+restore | Both eshells live as distinct buffers; isolation preserved        |
| 7a  | Same as 7, then in each tab `M-x switch-to-prev-buffer`        | Lands on correct historical buffer (window-history walker works)  |
| 8   | Tab with dired pointing at deleted directory; save+restore     | Save records buffer; restore logs single skip message; tab opens with other buffers intact |
| 8a  | Tab with dired created via `(let ((dired-buffers nil)) ...)` test: verify dired-find-buffer-nocreate sees the new buffer | `(dired-find-buffer-nocreate dir)` returns the restored buffer (not nil) |
| 9   | TRAMP-rooted dired (`/ssh:dead.host:/tmp/`); save+restore      | "1 remote buffer(s) skipped (TRAMP)" logged; no hang              |
| 9a  | Tab with bare-string TRAMP file record (legacy format); restore | Same summary message; no hang (v5-specific: bare-string TRAMP fix) |
| 10  | Save with `advice-add 'tabspaces--buffer-record :before (lambda (b) (when (eq b TARGET) (kill-buffer b)))`; trigger save | Save completes; TARGET absent from session file |
| 11  | User registers `:kind rename-test` handler whose restore-fn renames the result; save+restore | Substitution alist captures rename; window-state-put applies new name |
| 12  | User registers vterm handler per §4 canonical pattern; save+restore vterm-only tab | vterm buffer created at right `default-directory`; right name |
| 13  | User handler with deliberate typo (`(undefined-function ...)`); restore | Single error message logged; other tabs/records restore normally  |

Row 7 + 7a together exercise cross-tab dedup, isolation, and the window-
history walker. Row 8a verifies the `(dired-buffers (assq-delete-all ...))`
rebind preserves dired's tracking. Row 11 exercises substitution explicitly.
Rows 9 and 9a together confirm the TRAMP symmetry fix. Row 13 verifies
user-handler error containment.

For all rows: post-restore, the active buffer per window matches what was
active at save time (not `*scratch*` fallback).

## 14. Estimated effort

4-6 hours, including the manual test matrix and README. Broken down:

- Registration API + accumulator defvar: 20 min
- Save helpers (incl. `buffer-live-p`): 20 min
- Restore dispatch + dedup + window-state walker: 80 min
- Restore loop rewrite (`save-window-excursion`, TRAMP summary,
  placeholder cleanup, confirmation message preservation): 45 min
- Three built-in handlers: 40 min
- README (85-100 lines, two snippet examples in appendix): 60 min
- Manual test matrix (13 rows × ~5 min each plus inter-test setup): 90 min
- Buffer of 45-60 min for window-state walker debugging or
  unexpected `save-window-excursion` interactions.

Half-day implementation session is realistic.

## 15. Resolved questions

All v4 issues from the v4 review pass resolved by v5:

- ~~B-V4-1 `dired-buffers nil` orphans the buffer from dired's registry~~:
  use `(assq-delete-all (expand-file-name dir) dired-buffers)` to drop only
  the cross-tab cache entry while preserving the rest of the alist; new
  advertise lands in global registry via inherited dynamic binding.
- ~~I-V4-1 user handler errors abort restore loop~~: dispatch wraps user
  handler call in `condition-case` with logged message; built-in handlers
  retain their internal `condition-case` for stale-`:dir` paths.
- ~~I-V4-2 silent built-in errors~~: each built-in's `condition-case` now
  logs the error class (e.g., "tabspaces: eshell restore skipped (DIR): ERR").
- ~~I-V4-3 fragile `(eshell t)` then `(current-buffer)`~~: use `(eshell t)`
  return value directly.
- ~~I-V4-4 `tabspaces--eshell-buffer-name-for-dir` double-calls `project-current`~~:
  helper eliminated. Bind `eshell-buffer-name` to the saved `:name`
  directly; `generate-new-buffer` adds suffix on collision, substitution
  machinery captures the diff. Simpler and correct.
- ~~M-V4-2 prev-buffers walker uses undocumented nil-position marker path~~:
  substituted entries emit `(NAME 1 1)` for documented `set-marker`
  behavior. Navigation works; saved point is lost for substituted slots
  (lands at point-min).
- ~~M-V4-3 `:dir nil` produces silent wrong project~~: each handler validates
  `(and dir (stringp dir) (file-directory-p dir))` before proceeding.
- ~~GPT §6: "Restored session from %s" message could be dropped accidentally~~:
  §6.4 and §11 step 4 explicitly preserve the confirmation message as the
  final line of the restore block.
- ~~GPT §7 second: handlers calling window-config-changing functions~~: §4
  documents the constraint; §12 README item 2 calls it out.
- ~~M-V4-4 implementation should comment load-order~~: §11 step 5 says
  built-ins MUST be last top-level forms in session section, with code
  comment pointing to §4.
- ~~M-V4-6 row 9a setup ambiguity~~: clarified to "bare-string TRAMP file
  record (legacy format)."
- ~~Adversarial M-V4-1 vterm first-load cost framing~~: §12 item 4 notes
  the one-time pause for non-pre-built packages.

Remaining v2-future:

- Lazy TRAMP restore (deferred-open stubs).
- Warning on re-registration of a known kind.
- Schema version field (not needed; mixed-type list with `stringp` type-tag
  + opaque kind-symbol lookup is sufficient).
- Multi-frame coordinated restore.
- Replay of shell/eshell history.
