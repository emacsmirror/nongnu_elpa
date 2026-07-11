;;; hermes-dashboard-rpc.el --- Typed JSON-RPC wrappers for the Hermes dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The typed wrappers over the dashboard WebSocket JSON-RPC methods:
;; the `hermes-dashboard-transport-define-rpc' macro and one generated
;; wrapper per gateway method, split out of `hermes-dashboard-transport'
;; (which keeps the client lifecycle, WebSocket, REST, and auth plumbing).
;; Wrapper names keep the `hermes-dashboard-transport-' prefix so callers
;; only had to add this require.  New gateway methods belong here, one
;; `hermes-dashboard-transport-define-rpc' form each.

;;; Code:

(require 'cl-lib)
(require 'hermes-dashboard-transport)

(defun hermes-dashboard-transport--alist-without-nil (alist)
  "Return ALIST without nil-valued cells."
  (cl-remove-if (lambda (cell) (null (cdr cell))) alist))

(defun hermes-dashboard-transport--session-param (_client session-id)
  "Return the explicit SESSION-ID, or nil when the caller omitted it.
The shared dashboard client carries no ambient session identity: every
session-scoped RPC must pass its own live `:session-id'.  This keeps two
chat buffers sharing one socket from leaking session state into each other."
  session-id)

;;; RPC method wrappers

(defmacro hermes-dashboard-transport-define-rpc (name method docstring &rest spec)
  "Define NAME as a wrapper sending METHOD over the dashboard WebSocket.
DOCSTRING documents the generated `cl-defun'.  SPEC is a plist: :args lists
positional arguments after CLIENT, :keys lists `&key' parameters, :session when
non-nil sends the resolved session id, and :params adds extra
\(REQUEST-KEY . VALUE-FORM) cells.  Each :args and :keys symbol contributes a
request parameter keyed by its snake_case name with the symbol as the value;
nil values are dropped.  RESOLVE and REJECT keys are always added."
  (declare (indent 2)
           (debug (&define name stringp stringp &rest sexp)))
  (let* ((args (plist-get spec :args))
         (keys (plist-get spec :keys))
         (session (plist-get spec :session))
         (extra (plist-get spec :params))
         (snake (lambda (sym)
                  (intern (replace-regexp-in-string "-" "_" (symbol-name sym)))))
         (cells (append
                 (and session
                      (list `(cons 'session_id
                                   (hermes-dashboard-transport--session-param
                                    client session-id))))
                 (mapcar (lambda (s) `(cons ',(funcall snake s) ,s))
                         (append args keys))
                 (mapcar (lambda (c) `(cons ',(car c) ,(cdr c))) extra)))
         (params (and cells
                      `(hermes-dashboard-transport--alist-without-nil
                        (list ,@cells)))))
    `(cl-defun ,name (client ,@args &key ,@keys
                             ,@(and session '(session-id)) resolve reject)
       ,docstring
       (hermes-dashboard-transport-request
        client ,method ,params resolve reject))))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-create "session.create"
  "Send a `session.create' request for CLIENT.
COLS, MESSAGES, TITLE, PROFILE, and CWD become request parameters; the
handler reads nothing else, so runtime overrides such as the model are
applied to the fresh session through `config.set' instead.  RESOLVE and
REJECT receive the asynchronous result or error."
  :keys (cols messages title profile cwd))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-resume "session.resume"
  "Send a `session.resume' request for SESSION-ID on CLIENT.
COLS and PROFILE are optional; RESOLVE and REJECT receive the result or error."
  :args (session-id) :keys (cols profile))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-list "session.list"
  "Send a `session.list' request for CLIENT.
LIMIT caps the number of sessions returned.  RESOLVE and REJECT receive the
asynchronous result or error."
  :keys (limit))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-history "session.history"
  "Send a `session.history' request for SESSION-ID on CLIENT.
The handler always returns the full conversation (it reads no paging
parameters).  RESOLVE and REJECT receive the result or error."
  :args (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-delete "session.delete"
  "Send a `session.delete' request for SESSION-ID on CLIENT.
RESOLVE and REJECT receive the asynchronous result or error."
  :args (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-model-options "model.options"
  "Send a `model.options' request for CLIENT.
SESSION-ID scopes the current-model hints to that session.  RESOLVE and REJECT
receive the asynchronous result or error."
  :keys (session-id))

(cl-defun hermes-dashboard-transport-model-options-cached
    (client &key session-id force resolve reject)
  "Resolve `model.options' for CLIENT, serving the shared cache when possible.
With FORCE non-nil, bypass the cache and refetch.  SESSION-ID is forwarded on a
live fetch but does not key the cache: the provider/model catalog is
dashboard-global, so it is shared across sessions and chat buffers.  RESOLVE and
REJECT receive the payload or an error message, matching the plain RPC wrapper.

The underlying request defers until CLIENT is ready, so callers may warm the
cache immediately after starting a client."
  (let ((cached (and (not force)
                     (hermes-dashboard-transport-cached-model-options))))
    (if cached
        (when resolve (funcall resolve cached))
      (hermes-dashboard-transport-model-options
       client
       :session-id session-id
       :resolve (lambda (result)
                  (hermes-dashboard-transport--store-model-options result)
                  (when resolve (funcall resolve result)))
       :reject (or reject #'ignore)))))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-config-set "config.set"
  "Send a `config.set' request setting KEY to VALUE on CLIENT.
SESSION-ID scopes the change; CONFIRM-EXPENSIVE-MODEL acknowledges a pricier
model when `config.set' asks for confirmation.  RESOLVE and REJECT receive the
asynchronous result or error."
  :args (key value) :keys (session-id confirm-expensive-model))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-config-get "config.get"
  "Send a `config.get' request reading KEY on CLIENT.
CWD scopes the `project' key; SESSION-ID scopes the `fast' key.  RESOLVE and
REJECT receive the asynchronous result or error."
  :args (key) :keys (cwd session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-setup-status "setup.status"
  "Send a `setup.status' request for CLIENT.
The result carries `provider_configured'.  RESOLVE and REJECT receive the
asynchronous result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-setup-runtime-check "setup.runtime_check"
  "Send a `setup.runtime_check' request for CLIENT.
The result carries `ok' (nil on a credential failure -- never a JSON-RPC error)
with provider/model/source, or an `error' string.  RESOLVE and REJECT receive
the asynchronous result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-model-save-key "model.save_key"
  "Send a `model.save_key' request saving API-KEY for provider SLUG on CLIENT.
SESSION-ID scopes the live agent update.  The result carries the updated
provider object on success, or an error whose code is 4002 (unknown provider),
4003 (wrong auth type), 4006 (managed install), etc.  RESOLVE and REJECT receive
the asynchronous result or error."
  :args (slug api-key) :keys (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-tools-configure "tools.configure"
  "Send a `tools.configure' request for NAMES and ACTION on CLIENT.
ACTION is `enable' or `disable'.  SESSION-ID scopes a live session reset when
the dashboard backend supports it.  RESOLVE and REJECT receive the result or
error."
  :args (names action) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-skills-reload "skills.reload"
  "Send a `skills.reload' request for CLIENT.
RESOLVE and REJECT receive the result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-rollback-list "rollback.list"
  "Send a `rollback.list' request for CLIENT.
SESSION-ID scopes the checkpoints.  RESOLVE and REJECT receive the result
or error."
  :keys (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-rollback-diff "rollback.diff"
  "Send a `rollback.diff' request for checkpoint HASH on CLIENT.
SESSION-ID scopes the checkpoint.  RESOLVE and REJECT receive the result
or error."
  :args (hash) :keys (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-rollback-restore "rollback.restore"
  "Send a `rollback.restore' request for checkpoint HASH on CLIENT.
FILE-PATH restores a single file; SESSION-ID scopes the checkpoint.  RESOLVE
and REJECT receive the result or error."
  :args (hash) :keys (session-id file-path))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-delegation-status "delegation.status"
  "Send a `delegation.status' request for CLIENT.
RESOLVE and REJECT receive the result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-subagent-interrupt "subagent.interrupt"
  "Send a `subagent.interrupt' request for SUBAGENT-ID on CLIENT.
RESOLVE and REJECT receive the result or error."
  :args (subagent-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-cron-manage "cron.manage"
  "Send a `cron.manage' request for CLIENT.
ACTION is one of list, add, remove, pause, or resume; NAME identifies the job;
SCHEDULE and PROMPT are used by add.  RESOLVE and REJECT receive the result
or error."
  :keys (action name schedule prompt))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-prompt-submit "prompt.submit"
  "Send TEXT through `prompt.submit' on CLIENT.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
result or error."
  :args (text) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-prompt-background "prompt.background"
  "Run TEXT as a background task on CLIENT's SESSION-ID via `prompt.background'.
The task runs in its own session; its answer arrives later as a
`background.complete' event rather than in the resolve RESULT, which only
carries the assigned task id.  RESOLVE and REJECT receive the result or error."
  :args (text) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-interrupt "session.interrupt"
  "Send `session.interrupt' for CLIENT's SESSION-ID or active session.
RESOLVE and REJECT receive the asynchronous result or error."
  :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-process-stop "process.stop"
  "Send `process.stop' for CLIENT to terminate running background processes.
RESOLVE and REJECT receive the asynchronous result or error.  This stops
background/tool processes; it does not interrupt the current model turn -- use
`hermes-dashboard-transport-session-interrupt' for that.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-title "session.title"
  "Set CLIENT's SESSION-ID title to TITLE via `session.title'.
RESOLVE and REJECT receive the asynchronous result or error.  The gateway
resolves the session from the live SESSION-ID and may reply with a pending
title when the session row does not exist yet."
  :keys (title) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-title-fetch "session.title"
  "Fetch CLIENT's current SESSION-ID title via `session.title' without setting it.
Omitting the title makes the gateway return the stored or auto-generated title.
RESOLVE and REJECT receive the asynchronous result or error."
  :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-steer "session.steer"
  "Send TEXT through `session.steer' for CLIENT's active session.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
result or error."
  :args (text) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-handoff-request "handoff.request"
  "Queue a handoff of CLIENT's SESSION-ID to PLATFORM via `handoff.request'.
The gateway validates the platform and its home channel, marks the session
pending, and a separate watcher performs the transfer; poll
`hermes-dashboard-transport-handoff-state' for the terminal result.  RESOLVE
and REJECT receive the asynchronous result or error."
  :args (platform) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-handoff-state "handoff.state"
  "Poll the handoff state for CLIENT's SESSION-ID via `handoff.state'.
RESOLVE receives a result whose state is one of pending, running, completed, or
failed, and is empty when no handoff record exists; REJECT receives any error."
  :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-handoff-fail "handoff.fail"
  "Mark CLIENT's SESSION-ID handoff failed via `handoff.fail' with reason ERROR.
Called when a bounded client poll times out so the session is not left pending;
a late gateway success is not clobbered.  RESOLVE and REJECT receive the
asynchronous result or error."
  :keys (error) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-commands-catalog "commands.catalog"
  "Request the dashboard `commands.catalog' for CLIENT.
RESOLVE and REJECT receive the asynchronous result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-command-dispatch "command.dispatch"
  "Dispatch slash command NAME with ARG through CLIENT's `command.dispatch'.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
asynchronous result or error."
  :args (name arg) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-complete-slash "complete.slash"
  "Request slash-command completions for TEXT via `complete.slash'.
TEXT is the partial command line, for example \"/handoff \" to list the
gateway's connected handoff platforms.  RESOLVE receives a result whose `items'
each carry text/display/meta; REJECT receives any error."
  :args (text))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-slash-exec "slash.exec"
  "Run COMMAND through CLIENT's dashboard `slash.exec'.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
asynchronous result or error."
  :args (command) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-approval-respond "approval.respond"
  "Send an `approval.respond' CHOICE for CLIENT.
SESSION-ID selects the live dashboard session.  ALL applies CHOICE broadly when
non-nil.  RESOLVE and REJECT receive the asynchronous result or error."
  :keys (choice all) :session t)

(defun hermes-dashboard-transport-clarify-respond
    (client request-id answer &optional resolve reject)
  "Send ANSWER for clarify REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "clarify.respond" `((request_id . ,request-id) (answer . ,answer))
   resolve reject))

(defun hermes-dashboard-transport-sudo-respond
    (client request-id password &optional resolve reject)
  "Send PASSWORD for sudo REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "sudo.respond" `((request_id . ,request-id) (password . ,password))
   resolve reject))

(defun hermes-dashboard-transport-secret-respond
    (client request-id value &optional resolve reject)
  "Send VALUE for secret REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "secret.respond" `((request_id . ,request-id) (value . ,value))
   resolve reject))

(defun hermes-dashboard-transport-terminal-read-respond
    (client request-id text &optional resolve reject)
  "Send TEXT for terminal-read REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "terminal.read.respond"
   `((request_id . ,request-id) (text . ,text))
   resolve reject))


(provide 'hermes-dashboard-rpc)
;;; hermes-dashboard-rpc.el ends here
