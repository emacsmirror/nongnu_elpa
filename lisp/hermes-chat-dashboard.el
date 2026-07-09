;;; hermes-chat-dashboard.el --- Dashboard lifecycle for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (websocket "1.15") (markdown-mode "2.6"))

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

;; Dashboard transport, session, and event lifecycle helpers for
;; `hermes-chat'.  The public chat commands and UI selectors remain in
;; `hermes-chat.el'; this module preserves the existing `hermes-chat--*'
;; symbols while isolating dashboard-specific transport state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-chat-format)
(require 'hermes-chat-buffer)
(require 'hermes-chat-prompts)

(declare-function hermes-chat--active-tool-summaries "hermes-chat" ())
(declare-function hermes-chat--active-turn-p "hermes-chat" ())
(declare-function hermes-chat--clear-active-tools "hermes-chat" ())
(declare-function hermes-chat--command-error "hermes-chat" (message))
(declare-function hermes-chat--drain-queued-message "hermes-chat" ())
(declare-function hermes-chat--handoff-stop "hermes-chat" ())
(declare-function hermes-chat--message-start-status-event-p "hermes-chat" (event))
(declare-function hermes-chat--make-entry "hermes-chat" (role content &optional status id metadata))
(declare-function hermes-chat--maybe-refresh-session-title "hermes-chat" ())
(declare-function hermes-chat--notify-state-change "hermes-chat" ())
(declare-function hermes-chat--preserve-control-content "hermes-chat" (content))
(declare-function hermes-chat--queue-or-submit-content "hermes-chat" (content &optional display))
(declare-function hermes-chat--session-info-event-p "hermes-chat" (event))
(declare-function hermes-chat--set-header-state "hermes-chat" (&rest props))
(declare-function hermes-chat--unknown-event-content "hermes-chat" (event))
(declare-function hermes-chat--update-header-for-event "hermes-chat" (event))
(declare-function hermes-chat--render-turn-event "hermes-chat" (assistant-id event))
(declare-function hermes-chat--run-turn-reducer "hermes-chat" (assistant-id event))
(declare-function hermes-chat--handle-background-complete "hermes-chat" (event))
(declare-function hermes-chat--insert-local-status "hermes-chat" (content &optional status))

(defvar hermes-chat-dashboard-session-title)
(defvar hermes-chat-use-dashboard-transport)

(defvar hermes-chat--active-tools)
(defvar hermes-chat--pending-assistant-id)
(defvar hermes-chat--process)
(defvar hermes-chat--profile)
(defvar hermes-chat--session-id)
(defvar hermes-chat--status-state)
(defvar hermes-chat--title)
(defvar hermes-chat--transport-generation)

;; Connection state owned by `hermes-chat-buffer'; declared here for the
;; byte-compiler.
(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-token)
(defvar hermes-chat--dashboard-detached-assistant-id)
(defvar hermes-chat--dashboard-session-ready-p)
(defvar hermes-chat--dashboard-stream-assistant-id)
(defvar hermes-chat--dashboard-suppress-stream-p)

(defun hermes-chat--closed-status-event-p (event)
  "Return non-nil when EVENT reports a closed live transport."
  (and (eq (plist-get event :type) 'status)
       (equal (hermes-chat--status-name (plist-get event :status))
              "closed")))

(defun hermes-chat--reconnecting-status-event-p (event)
  "Return non-nil when EVENT reports the shared transport is reconnecting."
  (and (eq (plist-get event :type) 'status)
       (equal (hermes-chat--status-name (plist-get event :status))
              "reconnecting")))

(defun hermes-chat--reconnected-status-event-p (event)
  "Return non-nil when EVENT reports the shared transport reconnected."
  (and (eq (plist-get event :type) 'status)
       (equal (hermes-chat--status-name (plist-get event :status))
              "reconnected")))

(defun hermes-chat--closed-status-error-event (event)
  "Return an error event corresponding to transport close EVENT."
  (list :type 'error
        :event (or (plist-get event :event) "dashboard.closed")
        :content (or (hermes-chat--transport-entry-content event)
                     "Hermes dashboard WebSocket closed")))

(defun hermes-chat--event-session-id (event)
  "Return EVENT's dashboard session id, or nil."
  (hermes-chat--event-string event '(:session-id :session_id)))

(defun hermes-chat--clear-terminal-prompts (event)
  "Remove pending prompt requests associated with terminal EVENT."
  (hermes-chat--clear-pending-prompts
   (hermes-chat--event-session-id event)))

(defun hermes-chat--dashboard-connection-label ()
  "Return a compact dashboard connection label for the current chat."
  (cond
   ((hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    "connected")
   ((and hermes-chat--dashboard-client (hermes-chat--active-turn-p))
    "connecting")
   (hermes-chat--dashboard-client "disconnected")
   (hermes-chat--process "transport active")
   (t nil)))

(defun hermes-chat--dashboard-snapshot ()
  "Return display-safe dashboard state for the current chat buffer."
  (list :buffer (current-buffer)
        :title (or hermes-chat--title (buffer-name))
        :session-id hermes-chat--session-id
        :connection (hermes-chat--dashboard-connection-label)
        :status (or (plist-get hermes-chat--status-state :status) 'ready)
        :activity (plist-get hermes-chat--status-state :activity)
        :active-tools (hermes-chat--active-tool-summaries)
        :pending-prompts (hermes-chat--pending-prompt-count)
        :pending-assistant-p (and hermes-chat--pending-assistant-id t)
        :updated (plist-get hermes-chat--status-state :updated)))

(defun hermes-chat--forget-live-dashboard-session ()
  "Forget the live dashboard session while preserving the durable session key."
  (setq hermes-chat--dashboard-session-ready-p nil
        hermes-chat--dashboard-active-session-id nil))

(defun hermes-chat--stop-dashboard-client ()
  "Drop this buffer's reference to the shared dashboard client.
The buffer's subscriber is removed and its reference released; the shared client
is torn down only when the last buffer detaches.  The buffer-local client,
token, and live-session state are always cleared, even after a partial teardown,
so a new session can be started afterwards."
  (when-let* ((client hermes-chat--dashboard-client))
    (when hermes-chat--dashboard-token
      (hermes-dashboard-transport-unsubscribe client hermes-chat--dashboard-token))
    (hermes-dashboard-transport-release client)
    (when (eq hermes-chat--process client)
      (setq hermes-chat--process nil))
    (setq hermes-chat--dashboard-client nil
          hermes-chat--dashboard-token nil))
  (hermes-chat--forget-live-dashboard-session))

(defun hermes-chat--cleanup-buffer ()
  "Release per-buffer Hermes chat resources before killing the buffer."
  (hermes-chat--handoff-stop)
  (hermes-chat--stop-dashboard-client)
  (hermes-chat--notify-state-change))

(defun hermes-chat--next-transport-generation ()
  "Advance and return this buffer's transport callback generation."
  (cl-incf hermes-chat--transport-generation))

(defun hermes-chat--current-transport-generation-p (generation)
  "Return non-nil when GENERATION is the current transport generation."
  (= generation hermes-chat--transport-generation))

(defun hermes-chat--dashboard-terminal-event-p (event)
  "Return non-nil when EVENT should settle a suppressed dashboard stream."
  (or (memq (plist-get event :type) '(done error))
      (hermes-chat--closed-status-event-p event)))

(defun hermes-chat--dashboard-suppressed-content-event-p (event)
  "Return non-nil when suppressed EVENT must not update reply text."
  (and hermes-chat--dashboard-suppress-stream-p
       (memq (plist-get event :type) '(done error))))

(defun hermes-chat--dashboard-suppressed-terminal-status (event)
  "Return assistant status for suppressed dashboard terminal EVENT."
  (if (eq (plist-get event :type) 'error)
      (hermes-chat--error-status event)
    'done))

(defun hermes-chat--dashboard-suppressed-header-event (event)
  "Return a safe header event for suppressed dashboard terminal EVENT."
  (if (eq (plist-get event :type) 'error)
      '(:type error
              :content "Hermes session ended; prompt was not submitted")
    '(:type done)))

(defun hermes-chat--dashboard-control-error-event-p (event)
  "Return non-nil when EVENT is an error from a control RPC."
  (and (eq (plist-get event :type) 'error)
       (and-let* ((method (hermes-chat--event-string event '(:method))))
         (not (member method '("prompt.submit" "session.create"
                               "session.resume"))))))

(defun hermes-chat--dashboard-event-assistant-id (fallback-id event)
  "Return assistant id that should receive dashboard EVENT.
FALLBACK-ID is the assistant id captured by the transport callback."
  (cond
   (hermes-chat--dashboard-stream-assistant-id)
   (hermes-chat--dashboard-suppress-stream-p
    (and (hermes-chat--dashboard-terminal-event-p event) fallback-id))
   (t fallback-id)))

(defun hermes-chat--dashboard-finish-assistant (assistant-id)
  "Clear dashboard stream bookkeeping for ASSISTANT-ID when applicable."
  (when (equal hermes-chat--dashboard-stream-assistant-id assistant-id)
    (setq hermes-chat--dashboard-stream-assistant-id nil))
  (when (and hermes-chat--dashboard-suppress-stream-p
             (equal hermes-chat--pending-assistant-id assistant-id))
    (setq hermes-chat--dashboard-suppress-stream-p nil))
  (when (equal hermes-chat--dashboard-detached-assistant-id assistant-id)
    (setq hermes-chat--dashboard-detached-assistant-id nil)))

(defun hermes-chat--handle-suppressed-dashboard-terminal-event
    (assistant-id event)
  "Settle suppressed dashboard terminal EVENT for ASSISTANT-ID.
The event belongs to a resumed in-flight turn without a local assistant entry,
so its final content must not reach the unsubmitted retry placeholder.  The
turn lifecycle itself runs through the reducer's `suppressed-terminal' case so
settlement order lives in one place."
  (hermes-chat--run-turn-reducer
   assistant-id
   (list :type 'suppressed-terminal
         :settle-status (hermes-chat--dashboard-suppressed-terminal-status event)
         :header (hermes-chat--dashboard-suppressed-header-event event)
         :original event)))

(defun hermes-chat--stale-assistant-event-p (assistant-id)
  "Return non-nil when ASSISTANT-ID is older than the active pending turn."
  (and hermes-chat--pending-assistant-id
       (not (equal hermes-chat--pending-assistant-id assistant-id))))

(defun hermes-chat--handle-closed-status (assistant-id event)
  "Handle a transport closed status EVENT for ASSISTANT-ID."
  (hermes-chat--forget-live-dashboard-session)
  (hermes-chat--clear-terminal-prompts event)
  (if (equal hermes-chat--pending-assistant-id assistant-id)
      (progn
        (hermes-chat--handle-transport-event
         assistant-id (hermes-chat--closed-status-error-event event))
        (setq hermes-chat--dashboard-detached-assistant-id assistant-id
              hermes-chat--dashboard-stream-assistant-id nil
              hermes-chat--dashboard-suppress-stream-p nil))
    (hermes-chat--render-turn-event assistant-id event)))

(defun hermes-chat--handle-reconnecting-status (event)
  "Handle a manual dashboard socket reconnect status EVENT."
  (hermes-chat--forget-live-dashboard-session)
  (hermes-chat--clear-terminal-prompts event)
  (hermes-chat--insert-local-status
   (or (hermes-chat--transport-entry-content event)
       "Hermes dashboard socket reconnecting")
   'reconnecting)
  (hermes-chat--set-header-state
   :status 'reconnecting :activity "Reconnecting dashboard socket"))

(defun hermes-chat--handle-transport-event (assistant-id event)
  "Apply transport EVENT to ASSISTANT-ID in the current chat buffer."
  (cond
   ;; A background (`/btw') result is owned by its own session and arrives out
   ;; of band, so handle it before the stale-turn guard would drop it and apart
   ;; from the active turn's assistant entry.
   ((eq (plist-get event :type) 'background)
    (hermes-chat--handle-background-complete event))
   ;; A reconnect signal is a transport-wide broadcast, not a turn event, so
   ;; handle it before the stale-turn guard would drop it.
   ((hermes-chat--reconnecting-status-event-p event)
    (hermes-chat--handle-reconnecting-status event))
   ((hermes-chat--reconnected-status-event-p event)
    (hermes-chat--dashboard-handle-reconnected event))
   ((hermes-chat--stale-assistant-event-p assistant-id) nil)
   ((hermes-chat--closed-status-event-p event)
    (hermes-chat--handle-closed-status assistant-id event))
   ((hermes-chat--message-start-status-event-p event) nil)
   (t
    (when (hermes-chat--prompt-request-event-p event)
      (setq event (hermes-chat--record-prompt-request event assistant-id))
      (hermes-chat--schedule-auto-prompt event))
    ;; Every recognized event -- header, tool, transcript, streaming delta, and
    ;; the done/error turn lifecycle -- is rendered by the reducer effects in
    ;; `hermes-chat--render-turn-event'.  Only a truly unknown type warns here.
    (hermes-chat--render-turn-event assistant-id event)
    (when (eq (plist-get event :type) 'done)
      (hermes-chat--maybe-refresh-session-title))
    (pcase (plist-get event :type)
      ((or 'delta 'done 'error 'thinking 'status 'progress 'tool 'commentary
           'diff 'unknown)
       nil)
      (_
       (message "Unknown Hermes transport event: %S" event))))))

(defun hermes-chat--dashboard-default-transport-p ()
  "Return non-nil when chat should use the dashboard transport."
  (and hermes-chat-use-dashboard-transport
       (eq hermes-transport-send-function #'hermes-transport-send)))

(defun hermes-chat--dashboard-client-live-p (client)
  "Return non-nil when CLIENT has an open dashboard WebSocket."
  (and (hermes-dashboard-transport-client-p client)
       (hermes-dashboard-transport-client-websocket client)))

(defun hermes-chat--dashboard-cols ()
  "Return the current chat width for dashboard session requests."
  (max 20 (window-total-width)))

(defun hermes-chat--dashboard-result-string (result keys)
  "Return RESULT's first scalar string value among KEYS."
  (hermes-transport--scalar-string
   (hermes-transport--get-any result keys)))

(defun hermes-chat--dashboard-active-id-from-result (_client result)
  "Return the live dashboard session id from RPC RESULT.
The shared CLIENT is transport-only; session identity is read from the
result alone so one buffer's session never leaks onto another."
  (hermes-chat--dashboard-result-string result '(session_id id)))

(defun hermes-chat--dashboard-stored-id-from-result (_client result active-id)
  "Return durable session key from RPC RESULT, falling back to ACTIVE-ID.
The shared CLIENT contributes no session identity on a shared socket."
  (or (hermes-chat--dashboard-result-string
       result '(stored_session_id resumed session_key))
      active-id))

(defun hermes-chat--dashboard-record-session (client result)
  "Record live and durable session identifiers from RPC RESULT in this buffer.
CLIENT is the shared transport client; it is used only to bind the
subscriber token for event routing.  No session state is written onto the
shared client."
  (when-let* ((active-id
               (hermes-chat--dashboard-active-id-from-result client result)))
    (let ((stored-id
           (hermes-chat--dashboard-stored-id-from-result client result active-id)))
      (setq hermes-chat--dashboard-active-session-id active-id
            hermes-chat--session-id stored-id
            hermes-chat--dashboard-session-ready-p t)
      (when (and (hermes-dashboard-transport-client-p client)
                 hermes-chat--dashboard-token)
        (hermes-dashboard-transport-subscribe-session
         client hermes-chat--dashboard-token active-id)))))

(defun hermes-chat--dashboard-result-live-turn-p (result)
  "Return non-nil when RESULT reports the resumed session is still busy."
  (or (hermes-transport--get result 'running)
      (hermes-transport--get result 'inflight)))

(defun hermes-chat--dashboard-mark-unsubmitted-retry (assistant-id)
  "Mark ASSISTANT-ID as an unsubmitted retry placeholder."
  (hermes-chat--mark-assistant
   assistant-id 'error
   "Hermes session is still running; prompt was not submitted." t)
  (hermes-chat--settle-transport-entries assistant-id 'error))

(defun hermes-chat--dashboard-insert-inflight-assistant ()
  "Insert and return an assistant entry for a resumed in-flight turn."
  (let* ((entry (hermes-chat--make-entry 'assistant "" 'streaming))
         (assistant-id (plist-get entry :id)))
    (hermes-chat--insert-entry entry)
    assistant-id))

(defun hermes-chat--transport-callback
    (buffer assistant-id dashboard-p generation)
  "Return transport callback for BUFFER, ASSISTANT-ID, DASHBOARD-P, and GENERATION."
  (lambda (event)
    (hermes-chat--in-buffer buffer
      (when (and (hermes-chat--current-transport-generation-p generation)
                 (or (not dashboard-p)
                     (and (not (hermes-chat--dashboard-control-error-event-p
                                event))
                          (hermes-chat--dashboard-event-for-session-p event))))
        (when-let* ((target-id (if dashboard-p
                                   (hermes-chat--dashboard-event-assistant-id
                                    assistant-id event)
                                 assistant-id)))
          (if (and dashboard-p
                   (hermes-chat--dashboard-suppressed-content-event-p
                    event))
              (hermes-chat--handle-suppressed-dashboard-terminal-event
               target-id event)
            (hermes-chat--handle-transport-event target-id event)))))))

(defun hermes-chat--dashboard-bind-stream-callback (client assistant-id)
  "Bind CLIENT events to ASSISTANT-ID in the current buffer."
  (when (and (hermes-dashboard-transport-client-p client) assistant-id)
    (hermes-chat--dashboard-set-subscriber
     client
     (hermes-chat--transport-callback
      (current-buffer) assistant-id t
      (hermes-chat--next-transport-generation)))))

(defun hermes-chat--background-listener-callback (buffer)
  "Return a client callback that renders background results in BUFFER.
Only `background.complete' events for BUFFER's session are handled; everything
else is ignored.  Bound on an otherwise idle chat so a `/btw' result is still
delivered when no turn is streaming a callback of its own."
  (lambda (event)
    (when (eq (plist-get event :type) 'background)
      (hermes-chat--in-buffer buffer
        (when (hermes-chat--dashboard-event-for-session-p event)
          (hermes-chat--handle-background-complete event))))))

(defun hermes-chat--ensure-background-listener (client buffer)
  "Subscribe a BUFFER background-result listener on CLIENT when it has no token.
A no-op once BUFFER holds a subscriber token: its turn callback already routes
background events.  A buffer that has never streamed a turn has no token yet --
exactly the case a fresh `/btw' must cover.  The listener filters by session, so
a result for a since-replaced session is dropped rather than misrouted."
  (when (and (hermes-dashboard-transport-client-p client)
             (not hermes-chat--dashboard-token))
    (setq hermes-chat--dashboard-token
          (hermes-dashboard-transport-subscribe
           client (hermes-chat--background-listener-callback buffer)))))

(defun hermes-chat--dashboard-reattach-status-event ()
  "Return a fresh status event announcing a reattached running session.
Built with `list' so each call yields its own plist; the result is handed to
`hermes-chat--handle-transport-event', which may destructively extend it."
  (list :type 'status
        :status-key "session.resume"
        :status "running"
        :content "Hermes session is still running; reattached"))

(defun hermes-chat--dashboard-restore-inflight-turn (client)
  "Restore local busy state for CLIENT's resumed in-flight turn."
  (let* ((retry-id hermes-chat--pending-assistant-id)
         (stream-id (or hermes-chat--dashboard-detached-assistant-id
                        (and hermes-chat--dashboard-stream-assistant-id
                             (not (equal hermes-chat--dashboard-stream-assistant-id
                                         retry-id))
                             hermes-chat--dashboard-stream-assistant-id))))
    (cond
     (stream-id
      (when (and retry-id (not (equal retry-id stream-id)))
        (hermes-chat--dashboard-mark-unsubmitted-retry retry-id))
      (hermes-chat--clear-active-tools)
      (hermes-chat--mark-assistant stream-id 'streaming "" t)
      (setq hermes-chat--pending-assistant-id stream-id
            hermes-chat--process client
            hermes-chat--dashboard-stream-assistant-id stream-id
            hermes-chat--dashboard-suppress-stream-p nil)
      (hermes-chat--handle-transport-event
       stream-id
       (hermes-chat--dashboard-reattach-status-event)))
     (retry-id
      (hermes-chat--clear-active-tools)
      (hermes-chat--mark-assistant
       retry-id 'streaming
       "Hermes session is still running; prompt was not submitted." t)
      (setq hermes-chat--pending-assistant-id retry-id
            hermes-chat--process client
            hermes-chat--dashboard-stream-assistant-id nil
            hermes-chat--dashboard-suppress-stream-p t)
      (hermes-chat--set-header-state
       :status 'running
       :activity "Hermes session is still running"
       :assistant-id retry-id))
     (t
      (let ((assistant-id (hermes-chat--dashboard-insert-inflight-assistant)))
        (hermes-chat--clear-active-tools)
        (setq hermes-chat--pending-assistant-id assistant-id
              hermes-chat--process client
              hermes-chat--dashboard-stream-assistant-id assistant-id
              hermes-chat--dashboard-suppress-stream-p nil)
        (hermes-chat--handle-transport-event
         assistant-id
         (hermes-chat--dashboard-reattach-status-event)))))))

(defun hermes-chat--dashboard-ensure-client (&optional callback)
  "Return this buffer's shared dashboard client, acquiring one when needed.
A live attached client is reused; otherwise this buffer's stale reference is
released and a shared client for the configured endpoint is acquired and warmed.
CALLBACK seeds a freshly created client's fallback callback; per-buffer events
still route through this buffer's subscriber, so the fallback only matters once
no buffer is attached."
  (if (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
      hermes-chat--dashboard-client
    (hermes-chat--stop-dashboard-client)
    (setq hermes-chat--dashboard-session-ready-p nil
          hermes-chat--dashboard-active-session-id nil
          hermes-chat--dashboard-client
          (hermes-dashboard-transport-acquire :callback (or callback #'ignore)))
    (hermes-chat--warm-model-options hermes-chat--dashboard-client)
    hermes-chat--dashboard-client))

(defun hermes-chat--dashboard-set-subscriber (client callback)
  "Bind CALLBACK as this buffer's subscriber function on shared CLIENT.
Reuse this buffer's existing subscriber token when it is still valid; otherwise
register a fresh one."
  (unless (and hermes-chat--dashboard-token
               (hermes-dashboard-transport-set-subscriber-fn
                client hermes-chat--dashboard-token callback))
    (setq hermes-chat--dashboard-token
          (hermes-dashboard-transport-subscribe client callback))))

(defun hermes-chat--dashboard-start (callback)
  "Return this buffer's shared dashboard client, streaming events to CALLBACK."
  (let ((client (hermes-chat--dashboard-ensure-client callback)))
    (hermes-chat--dashboard-set-subscriber client callback)
    client))

(defun hermes-chat--warm-model-options (client)
  "Warm the shared model-options cache once CLIENT's connection is ready.
The catalog is dashboard-global, so the first chat to connect populates it for
every model picker; later chats reuse the cache without a round-trip.  The fetch
waits on CLIENT's readiness promise rather than running on the synchronous send
path; a client with no readiness promise (such as a test stub) is left alone."
  (when-let* (((not (hermes-dashboard-transport-cached-model-options)))
              (ready (hermes-dashboard-transport-client-ready-promise client))
              ((hermes--promise-p ready)))
    (hermes--promise-then
     ready
     (lambda (_value)
       (hermes-dashboard-transport-model-options-cached
        client :resolve #'ignore :reject #'ignore))
     #'ignore)))

(defun hermes-chat--dashboard-submit-prompt (client prompt)
  "Submit PROMPT to CLIENT's active dashboard session."
  (unless hermes-chat--dashboard-active-session-id
    (user-error "Hermes dashboard did not return a live session id"))
  (hermes-dashboard-transport-prompt-submit
   client prompt :session-id hermes-chat--dashboard-active-session-id))

(defun hermes-chat--dashboard-after-session
    (client prompt result &optional resume-p)
  "Record CLIENT session RESULT and submit PROMPT.
When RESUME-P is non-nil and RESULT reports a live turn, keep local busy
state instead of submitting another prompt into that durable session."
  (hermes-chat--dashboard-record-session client result)
  (if (and resume-p (hermes-chat--dashboard-result-live-turn-p result))
      (hermes-chat--dashboard-restore-inflight-turn client)
    (setq hermes-chat--dashboard-detached-assistant-id nil)
    (hermes-chat--dashboard-submit-prompt client prompt)))

(defun hermes-chat--dashboard-session-resolver (buffer client prompt &optional resume-p)
  "Return a callback that records CLIENT's session in BUFFER and sends PROMPT.
RESUME-P means the callback handles a `session.resume' response."
  (lambda (result)
    (hermes-chat--in-buffer buffer
      (hermes-chat--dashboard-after-session
       client prompt result resume-p))))

(defun hermes-chat--dashboard-session-attached-p ()
  "Return non-nil when the current buffer has a live dashboard session."
  (and hermes-chat--dashboard-session-ready-p
       hermes-chat--dashboard-active-session-id))

(defun hermes-chat--dashboard-create-runtime-params ()
  "Return a plist of non-nil buffer-local create-time runtime overrides.
Values are read from the current buffer's `hermes-chat--dashboard-create-*'
vars and forwarded to `session.create' only; `session.resume' owns its
stored runtime and must not receive them."
  (cl-loop for (var key)
           in '((hermes-chat--dashboard-create-model :model)
                 (hermes-chat--dashboard-create-provider :provider)
                 (hermes-chat--dashboard-create-reasoning-effort :reasoning-effort)
                 (hermes-chat--dashboard-create-fast-p :fast))
           when (symbol-value var)
           append (list key (symbol-value var))))

(defun hermes-chat--dashboard-ensure-session (client prompt buffer)
  "Create or resume CLIENT's dashboard session before submitting PROMPT.
Record asynchronous session results in BUFFER."
  (cond
   ((hermes-chat--dashboard-session-attached-p)
    (hermes-chat--dashboard-submit-prompt client prompt))
   (hermes-chat--session-id
    (hermes-dashboard-transport-session-resume
     client hermes-chat--session-id
     :cols (hermes-chat--dashboard-cols)
     :resolve (hermes-chat--dashboard-session-resolver
               buffer client prompt t)))
   (t
    (apply #'hermes-dashboard-transport-session-create
     client
     :cols (hermes-chat--dashboard-cols)
     :title hermes-chat-dashboard-session-title
     :profile hermes-chat--profile
     (append (hermes-chat--dashboard-create-runtime-params)
             (list :resolve
                   (hermes-chat--dashboard-session-resolver
                    buffer client prompt)))))))

(defun hermes-chat--dashboard-event-for-session-p (event)
  "Return non-nil when EVENT belongs to this buffer's live dashboard session."
  (let ((session-id (plist-get event :session-id)))
    (or (null session-id)
        (and hermes-chat--dashboard-active-session-id
             (equal session-id hermes-chat--dashboard-active-session-id)))))

(defun hermes-chat--dashboard-send (prompt callback)
  "Send PROMPT through the dashboard transport and stream to CALLBACK."
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-start callback)))
    (hermes-chat--dashboard-ensure-session client prompt buffer)
    client))

(defun hermes-chat--send-prompt (prompt callback)
  "Send PROMPT to Hermes and stream transport events to CALLBACK."
  (if (hermes-chat--dashboard-default-transport-p)
      (hermes-chat--dashboard-send prompt callback)
    (funcall hermes-transport-send-function prompt callback)))

(defun hermes-chat--dashboard-bootstrap-error (message &optional content)
  "Render dashboard session bootstrap MESSAGE and preserve CONTENT."
  (hermes-chat--command-error (format "Dashboard session failed: %s" message))
  (hermes-chat--preserve-control-content content))

(defun hermes-chat--call-with-dashboard-bootstrap-error (content thunk)
  "Call THUNK, preserving CONTENT if dashboard bootstrap signals."
  (condition-case err
      (funcall thunk)
    (error
     (hermes-chat--dashboard-bootstrap-error (error-message-string err)
                                             content))))

(defun hermes-chat--dashboard-control-client ()
  "Return a shared dashboard client for control RPCs without seizing callbacks."
  (cond
   ((hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    hermes-chat--dashboard-client)
   ((hermes-chat--dashboard-default-transport-p)
    (hermes-chat--dashboard-ensure-client))
   (t
    (user-error "Hermes dashboard transport controls are unavailable"))))

(defun hermes-chat--dashboard-action-resolver (buffer client action)
  "Return a resolver to record CLIENT's session in BUFFER, then call ACTION."
  (lambda (result)
    (hermes-chat--in-buffer buffer
      (hermes-chat--dashboard-record-session client result)
      (when (hermes-chat--dashboard-result-live-turn-p result)
        (hermes-chat--dashboard-restore-inflight-turn client)
        (hermes-chat--dashboard-bind-stream-callback
         client hermes-chat--pending-assistant-id))
      (funcall action client))))

(defun hermes-chat--dashboard-action-rejecter (buffer reject)
  "Return a reject callback to run REJECT visibly in BUFFER."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (if reject
          (funcall reject message)
        (hermes-chat--command-error message)))))

(defun hermes-chat--dashboard-ensure-session-action
    (client buffer action &optional reject)
  "Ensure CLIENT has a session in BUFFER, then call ACTION with CLIENT.
When dashboard session bootstrap fails, call REJECT with the error message."
  (cond
   ((hermes-chat--dashboard-session-attached-p)
    (funcall action client))
   (hermes-chat--session-id
    (hermes-dashboard-transport-session-resume
     client hermes-chat--session-id
     :cols (hermes-chat--dashboard-cols)
     :resolve (hermes-chat--dashboard-action-resolver buffer client action)
     :reject (hermes-chat--dashboard-action-rejecter buffer reject)))
   (t
    (apply #'hermes-dashboard-transport-session-create
     client
     :cols (hermes-chat--dashboard-cols)
     :title hermes-chat-dashboard-session-title
     (append (hermes-chat--dashboard-create-runtime-params)
             (list :resolve
                   (hermes-chat--dashboard-action-resolver buffer client action)
                   :reject (hermes-chat--dashboard-action-rejecter buffer reject)))))))

(defun hermes-chat--dashboard-stored-session-needs-resume-p ()
  "Return non-nil when a durable dashboard session may be active remotely."
  (and (hermes-chat--dashboard-default-transport-p)
       hermes-chat--session-id
       (not (hermes-chat--dashboard-session-attached-p))
       (not (hermes-chat--active-turn-p))))

(defun hermes-chat--dashboard-queue-or-submit (content buffer &optional display)
  "Resume stored dashboard session in BUFFER before queuing or submitting CONTENT.
DISPLAY is the compact user-turn text shown instead of CONTENT."
  (if (hermes-chat--dashboard-stored-session-needs-resume-p)
      (hermes-chat--call-with-dashboard-bootstrap-error
       content
       (lambda ()
         (let ((client (hermes-chat--dashboard-control-client)))
           (hermes-chat--dashboard-ensure-session-action
            client buffer
            (lambda (_live-client)
              (hermes-chat--queue-or-submit-content content display))
            (lambda (message)
              (hermes-chat--dashboard-bootstrap-error message content))))))
    (hermes-chat--queue-or-submit-content content display)))

(defun hermes-chat--dashboard-handle-reconnected (_event)
  "Re-attach this buffer's stored dashboard session after a socket reconnect.
A no-op unless the buffer has a durable session that is not currently attached
and no turn is active -- the same guard a later send uses -- so a reconnected
shared socket re-resumes every attached chat without waiting for the next send."
  (when (hermes-chat--dashboard-stored-session-needs-resume-p)
    (let ((buffer (current-buffer)))
      (hermes-chat--call-with-dashboard-bootstrap-error
       nil
       (lambda ()
         (hermes-chat--dashboard-ensure-session-action
          (hermes-chat--dashboard-control-client) buffer #'ignore
          (lambda (message)
            (hermes-chat--command-error
             (format "Hermes reconnect resume failed: %s" message)))))))))

(provide 'hermes-chat-dashboard)
;;; hermes-chat-dashboard.el ends here
