;;; hermes-chat-dashboard.el --- Dashboard lifecycle for Hermes chat  -*- lexical-binding: t; -*-

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

;; Dashboard transport, session, and event lifecycle helpers for
;; `hermes-chat', plus the session-scoped features that ride the lifecycle:
;; server session titles and `/btw' background tasks.  Events route upward
;; only through `hermes-chat--turn-event-function'; per-buffer teardown runs
;; `hermes-chat-cleanup-functions'.  This module preserves the existing
;; `hermes-chat--*' symbols while isolating dashboard-specific state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-transport-cli)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-format)
(require 'hermes-chat-buffer)
(require 'hermes-chat-prompts)
(require 'hermes-notifications)


(defvar hermes-chat-dashboard-session-title)
(defvar hermes-chat-use-dashboard-transport)

(defvar hermes-chat--active-tools)
(defvar hermes-chat--pending-assistant-id)
(defvar hermes-chat--process)
(defvar hermes-chat--profile)
(defvar hermes-chat--session-id)
(defvar hermes-chat--status-state)
(defvar hermes-chat--transport-generation)
(defvar hermes-chat--lifecycle-generation)

(defvar-local hermes-chat--title nil
  "Human title for this chat session.
Set by `hermes-chat-rename'.  Shown in the buffer name and reported to the
dashboard; nil falls back to the buffer name.")

(defvar-local hermes-chat--title-manual-p nil
  "Non-nil when the user set this chat's title via `hermes-chat-rename'.
A manual title is preserved against the automatic session-title refresh.")

(defvar-local hermes-chat--background-counter 0
  "Number of background (`/btw') tasks launched from this chat buffer.")

(defvar-local hermes-chat--background-tasks nil
  "Alist mapping a background task id to its (:number :preview) plist.
Populated when `prompt.background' accepts a task and consumed when the matching
`background.complete' event arrives, so the result entry can show the task's
number and the prompt that launched it.")


;; Connection state owned by `hermes-chat-buffer'; declared here for the
;; byte-compiler.
(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-token)
(defvar hermes-chat--dashboard-detached-assistant-id)
(defvar hermes-chat--dashboard-running-p)
(defvar hermes-chat--dashboard-session-ready-p)
(defvar hermes-chat--dashboard-stream-assistant-id)
(defvar hermes-chat--dashboard-suppress-stream-p)
(defvar hermes-chat--interrupted-assistant-id)
(defvar hermes-chat--interrupted-events)
(defvar hermes-chat--interrupt-request-pending-p)
(defvar hermes-chat--server-queued-assistant-id)
(defvar hermes-chat--server-queued-user-id)
(defvar hermes-chat--server-queued-after-idle-count)
(defvar hermes-chat--busy-submit-context)
(defvar hermes-chat--dashboard-idle-count)
(defvar hermes-chat--dashboard-last-start-idle-count)
(defvar hermes-chat--unsettled-submit-context)
(defvar hermes-chat--prepared-submit-assistant-id)
(defvar hermes-dashboard-transport-request-owner)

(defvar hermes-chat--turn-event-function #'ignore
  "Function reducing one transport event, set by `hermes-chat'.
Takes (ASSISTANT-ID EVENT); a nil ASSISTANT-ID reduces for the header only.
Routing through a registry keeps this file free of upward references into
the reducer defined in `hermes-chat'.")

(defvar hermes-chat--busy-submit-event-function #'ignore
  "Function holding an event while busy-submit policy is unresolved.")

(defvar hermes-chat--busy-submit-abandon-function #'ignore
  "Function abandoning an unresolved busy submission after session loss.")

(defvar hermes-chat-cleanup-functions nil
  "Abnormal-free hook run when a chat buffer releases its resources.
Modules add their per-buffer teardown here (e.g. `hermes-chat-handoff'
stops its poll) instead of being called by name from this file.")

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
        hermes-chat--dashboard-running-p nil
        hermes-chat--dashboard-active-session-id nil))

(defun hermes-chat--stop-dashboard-client ()
  "Drop this buffer's reference to the shared dashboard client.
The buffer's subscriber is removed and its reference released; the shared client
is torn down only when the last buffer detaches.  The buffer-local client,
token, and live-session state are always cleared, even after a partial teardown,
so a new session can be started afterwards."
  (when-let* ((client hermes-chat--dashboard-client))
    (hermes-dashboard-transport-cancel-owner-requests
     client (current-buffer))
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
  (run-hooks 'hermes-chat-cleanup-functions)
  (hermes-chat--invalidate-transport-state)
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

(defun hermes-chat--interrupted-assistant-event-p (assistant-id event)
  "Return non-nil when EVENT must wait for ASSISTANT-ID's interrupt result."
  (and (equal assistant-id hermes-chat--interrupted-assistant-id)
       hermes-chat--interrupt-request-pending-p
       (not (hermes-chat--session-info-event-p event))
       (memq (plist-get event :type)
             '(delta done error thinking commentary progress tool diff
                     status unknown))))

(defun hermes-chat--interrupted-trailing-event-p (assistant-id event)
  "Return non-nil when accepted interrupt EVENT for ASSISTANT-ID is trailing."
  (and (equal assistant-id hermes-chat--interrupted-assistant-id)
       (not hermes-chat--interrupt-request-pending-p)
       (not (hermes-chat--session-info-event-p event))
       (not (memq (plist-get event :type) '(done error)))))

(defun hermes-chat--interrupted-terminal-event (assistant-id event)
  "Return status-only interruption EVENT for interrupted ASSISTANT-ID."
  (if (and (equal assistant-id hermes-chat--interrupted-assistant-id)
           (memq (plist-get event :type) '(done error)))
      (let ((result (copy-sequence event)))
        (setq result (plist-put result :type 'error))
        (setq result (plist-put result :status "interrupted"))
        (plist-put result :content nil))
    event))

(defun hermes-chat--hold-interrupted-event (event)
  "Hold EVENT until the pending interrupt request settles."
  (push (copy-sequence event) hermes-chat--interrupted-events))

(defun hermes-chat--dashboard-activate-server-queued-turn (assistant-id)
  "Start rendering the backend-owned queued turn for ASSISTANT-ID."
  (when (and (equal assistant-id hermes-chat--server-queued-assistant-id)
             (numberp hermes-chat--server-queued-after-idle-count)
             (> hermes-chat--dashboard-last-start-idle-count
                hermes-chat--server-queued-after-idle-count))
    (setq hermes-chat--server-queued-assistant-id nil
          hermes-chat--server-queued-user-id nil
          hermes-chat--server-queued-after-idle-count nil
          hermes-chat--dashboard-running-p t
          hermes-chat--pending-assistant-id assistant-id
          hermes-chat--process hermes-chat--dashboard-client
          hermes-chat--dashboard-stream-assistant-id assistant-id
          hermes-chat--dashboard-suppress-stream-p nil)
    (hermes-chat--mark-assistant assistant-id 'streaming)
    (hermes-chat--set-header-state
     :status 'streaming :activity "Hermes is responding"
     :assistant-id assistant-id)))

(defun hermes-chat--dashboard-clear-server-queued-turn (message)
  "Settle the backend-queued placeholder after session loss with MESSAGE."
  (when-let* ((assistant-id hermes-chat--server-queued-assistant-id))
    (hermes-chat--mark-assistant assistant-id 'error message t)
    (hermes-chat--settle-transport-entries assistant-id 'error)
    (when (equal assistant-id hermes-chat--pending-assistant-id)
      (setq hermes-chat--pending-assistant-id nil
            hermes-chat--process nil)
      (hermes-chat--dashboard-finish-assistant assistant-id)))
  (setq hermes-chat--server-queued-assistant-id nil
        hermes-chat--server-queued-user-id nil
        hermes-chat--server-queued-after-idle-count nil))

(defun hermes-chat--dashboard-handle-message-start (assistant-id)
  "Record a message boundary and activate ASSISTANT-ID when server-queued."
  (setq hermes-chat--dashboard-last-start-idle-count
        hermes-chat--dashboard-idle-count)
  (when-let* ((context hermes-chat--unsettled-submit-context)
              ((equal assistant-id (plist-get context :assistant-id)))
              ((> hermes-chat--dashboard-idle-count
                  (or (plist-get context :idle-count) 0))))
    (hermes-chat--reset-submit-assistant assistant-id)
    (setq hermes-chat--prepared-submit-assistant-id assistant-id))
  (hermes-chat--dashboard-activate-server-queued-turn assistant-id))

(defun hermes-chat--dashboard-note-session-info (event)
  "Record an explicit idle transition carried by session-info EVENT."
  (when (and (hermes-chat--session-info-event-p event)
             (plist-member event :running)
             (not (plist-get event :running)))
    (cl-incf hermes-chat--dashboard-idle-count)))

(defun hermes-chat--dashboard-note-unsettled-terminal (assistant-id event)
  "Record terminal EVENT after an early submit boundary for ASSISTANT-ID."
  (when-let* ((context hermes-chat--unsettled-submit-context)
              ((equal assistant-id hermes-chat--prepared-submit-assistant-id))
              ((memq (plist-get event :type) '(done error))))
    (setf (plist-get context :post-start-terminal-p) t)
    (hermes-chat--clear-submit-context context)))

(defun hermes-chat--server-queued-prior-event-p (assistant-id event)
  "Return non-nil when EVENT predates ASSISTANT-ID's server-queued turn."
  (and (equal assistant-id hermes-chat--server-queued-assistant-id)
       (not (hermes-chat--session-info-event-p event))))

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
   ((and hermes-chat--server-queued-assistant-id
         (hermes-chat--message-start-status-event-p event)
         (> hermes-chat--dashboard-idle-count
            (or hermes-chat--server-queued-after-idle-count 0)))
    hermes-chat--server-queued-assistant-id)
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
  (funcall hermes-chat--turn-event-function
   assistant-id
   (list :type 'suppressed-terminal
         :settle-status (hermes-chat--dashboard-suppressed-terminal-status event)
         :header (hermes-chat--dashboard-suppressed-header-event event)
         :original event)))

(defun hermes-chat--stale-assistant-event-p (assistant-id event)
  "Return non-nil when EVENT belongs to an inactive ASSISTANT-ID."
  (and (not (hermes-chat--session-info-event-p event))
       (or (and hermes-chat--pending-assistant-id
                (not (equal hermes-chat--pending-assistant-id assistant-id)))
           (when-let* ((node (and hermes-chat--nodes
                                  (gethash assistant-id hermes-chat--nodes)))
                       (entry (ignore-errors (ewoc-data node))))
             (and (not (equal assistant-id
                              hermes-chat--interrupted-assistant-id))
                  (hermes-chat--finished-status-p
                   (plist-get entry :status)))))))

(defun hermes-chat--handle-closed-status (assistant-id event)
  "Handle a transport closed status EVENT for ASSISTANT-ID."
  (let ((context (and hermes-chat--unsettled-submit-context
                      (equal assistant-id
                             (plist-get hermes-chat--unsettled-submit-context
                                        :assistant-id))
                      hermes-chat--unsettled-submit-context)))
    (funcall hermes-chat--busy-submit-abandon-function)
    (hermes-chat--dashboard-clear-server-queued-turn
     "Hermes connection closed before queued turn started")
    (hermes-chat--forget-live-dashboard-session)
    (hermes-chat--clear-terminal-prompts event)
    (if (equal hermes-chat--pending-assistant-id assistant-id)
        (progn
          (when (equal assistant-id hermes-chat--server-queued-assistant-id)
            (setq hermes-chat--server-queued-assistant-id nil
                  hermes-chat--server-queued-user-id nil
                  hermes-chat--server-queued-after-idle-count nil))
          (hermes-chat--handle-transport-event
           assistant-id (hermes-chat--closed-status-error-event event))
          (setq hermes-chat--dashboard-detached-assistant-id assistant-id
                hermes-chat--dashboard-stream-assistant-id nil
                hermes-chat--dashboard-suppress-stream-p nil))
      (funcall hermes-chat--turn-event-function assistant-id event))
    (when context
      (hermes-chat--clear-submit-context context))))

(defun hermes-chat--handle-reconnecting-status (event)
  "Handle a manual dashboard socket reconnect status EVENT."
  (funcall hermes-chat--busy-submit-abandon-function)
  (hermes-chat--dashboard-clear-server-queued-turn
   "Hermes connection changed before queued turn started")
  (hermes-chat--forget-live-dashboard-session)
  (hermes-chat--clear-terminal-prompts event)
  (hermes-chat--insert-local-status
   (or (hermes-chat--transport-entry-content event)
       "Hermes dashboard socket reconnecting")
   'reconnecting)
  (hermes-chat--set-header-state
   :status 'reconnecting :activity "Reconnecting dashboard socket"))

(defun hermes-chat--dashboard-settle-terminal (interrupted-p)
  "Settle dashboard bookkeeping for a terminal event.
When INTERRUPTED-P is non-nil, also clear the interrupt request state."
  (when interrupted-p
    (setq hermes-chat--interrupted-assistant-id nil
          hermes-chat--interrupted-events nil
          hermes-chat--interrupt-request-pending-p nil))
  (hermes-chat--dashboard-schedule-idle-reconciliation
   #'hermes-chat--drain-queued-message))

(defun hermes-chat--interrupted-error-event-p (event)
  "Return whether error EVENT represents an intentional interruption."
  (member (hermes-chat--status-name (hermes-chat--error-status event))
          '("interrupted" "cancelled" "canceled")))

(defun hermes-chat--notify-terminal-event (assistant-id event)
  "Notify for terminal EVENT belonging to ASSISTANT-ID."
  (pcase (plist-get event :type)
    ('done
     (hermes-notifications-notify
      'chat-reply "Hermes reply ready"
      (let ((preview (hermes-notifications-preview
                      (hermes-chat--entry-content-by-id assistant-id))))
        (if (string-empty-p preview) "Reply completed" preview))
      :buffer (current-buffer) :category "hermes.chat" :urgency 'normal))
    ('error
     (unless (hermes-chat--interrupted-error-event-p event)
       (hermes-notifications-notify
        'chat-error "Hermes chat error"
        (let ((preview (hermes-notifications-preview
                        (plist-get event :content))))
          (if (string-empty-p preview) "The Hermes turn failed" preview))
        :buffer (current-buffer) :category "hermes.chat.error"
        :urgency 'critical)))))

(defun hermes-chat--notify-prompt-request (prompt)
  "Notify that PROMPT needs input without exposing its contents."
  (hermes-notifications-notify
   'prompt "Hermes input required"
   (format "%s requested in %s"
           (hermes-chat--prompt-display-name prompt) (buffer-name))
   :buffer (current-buffer) :category "hermes.chat.prompt"
   :urgency 'critical))

(defun hermes-chat--render-dashboard-turn-event (assistant-id event)
  "Render ordinary dashboard EVENT for ASSISTANT-ID and settle its lifecycle."
  (hermes-chat--dashboard-note-unsettled-terminal assistant-id event)
  (let ((interrupted-p
         (equal assistant-id hermes-chat--interrupted-assistant-id)))
    (setq event (hermes-chat--interrupted-terminal-event assistant-id event))
    (when (hermes-chat--prompt-request-event-p event)
      (setq event (hermes-chat--record-prompt-request event assistant-id))
      (hermes-chat--notify-prompt-request event)
      (hermes-chat--schedule-auto-prompt event))
    (funcall hermes-chat--turn-event-function assistant-id event)
    (when (memq (plist-get event :type) '(done error))
      (hermes-chat--notify-terminal-event assistant-id event))
    (when (memq (plist-get event :type) '(done error))
      (hermes-chat--dashboard-settle-terminal interrupted-p))
    (when (eq (plist-get event :type) 'done)
      (hermes-chat--maybe-refresh-session-title))
    (unless (memq (plist-get event :type)
                  '(delta interim done error thinking status progress tool
                          commentary diff unknown))
      (message "Unknown Hermes transport event: %S" event))))

(defun hermes-chat--handle-transport-event (assistant-id event)
  "Apply transport EVENT to ASSISTANT-ID in the current chat buffer."
  (hermes-chat--dashboard-note-session-info event)
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
   ((hermes-chat--message-start-status-event-p event)
    (hermes-chat--dashboard-handle-message-start assistant-id))
   ((hermes-chat--closed-status-event-p event)
    (hermes-chat--handle-closed-status assistant-id event))
   ((hermes-chat--stale-assistant-event-p assistant-id event) nil)
   ((hermes-chat--server-queued-prior-event-p assistant-id event) nil)
   ((hermes-chat--interrupted-assistant-event-p assistant-id event)
    (hermes-chat--hold-interrupted-event event))
   ((hermes-chat--interrupted-trailing-event-p assistant-id event) nil)
   (t (hermes-chat--render-dashboard-turn-event assistant-id event))))

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
      (when (hermes-transport--field-present-p result 'running)
        (setq hermes-chat--dashboard-running-p
              (eq (hermes-transport--get result 'running) t)))
      (when (and (hermes-dashboard-transport-client-p client)
                 hermes-chat--dashboard-token)
        (hermes-dashboard-transport-subscribe-session
         client hermes-chat--dashboard-token active-id)))))

(defun hermes-chat--dashboard-result-live-turn-p (result)
  "Return non-nil when RESULT reports the resumed session is still busy."
  (or (hermes-transport--get result 'running)
      (hermes-transport--get result 'inflight)))

(defun hermes-chat--dashboard-context-current-p
    (client generation &optional session-id)
  "Return non-nil when CLIENT, GENERATION, and SESSION-ID still own this chat."
  (and (eq client hermes-chat--dashboard-client)
       (= generation hermes-chat--lifecycle-generation)
       (or (null session-id)
           (equal session-id hermes-chat--dashboard-active-session-id))))

(defun hermes-chat--dashboard-idle-context (on-idle)
  "Return an idle-reconciliation context calling ON-IDLE when settled."
  (list :buffer (current-buffer)
        :client hermes-chat--dashboard-client
        :active-id hermes-chat--dashboard-active-session-id
        :stored-id (or hermes-chat--session-id
                       hermes-chat--dashboard-active-session-id)
        :generation hermes-chat--transport-generation
        :delay 0.1
        :on-idle on-idle))

(defun hermes-chat--dashboard-idle-context-valid-p (context)
  "Return non-nil when idle reconciliation CONTEXT still owns this chat."
  (and hermes-chat--dashboard-running-p
       (eq (plist-get context :client) hermes-chat--dashboard-client)
       (equal (plist-get context :active-id)
              hermes-chat--dashboard-active-session-id)
       (hermes-chat--current-transport-generation-p
        (plist-get context :generation))))

(defun hermes-chat--dashboard-next-idle-context (context)
  "Return CONTEXT with its polling delay increased up to one second."
  (plist-put (copy-sequence context) :delay
             (min 1.0 (* 2 (plist-get context :delay)))))

(defun hermes-chat--dashboard-reconcile-idle-later (context)
  "Schedule the next idle reconciliation described by CONTEXT."
  (run-at-time (plist-get context :delay) nil
               #'hermes-chat--dashboard-reconcile-idle context))

(defun hermes-chat--dashboard-handle-idle-result (context result)
  "Handle session resume RESULT for idle reconciliation CONTEXT."
  (when (hermes-chat--dashboard-idle-context-valid-p context)
    (if (hermes-chat--dashboard-result-live-turn-p result)
        (hermes-chat--dashboard-reconcile-idle-later
         (hermes-chat--dashboard-next-idle-context context))
      (setq hermes-chat--dashboard-running-p nil)
      (funcall (plist-get context :on-idle)))))

(defun hermes-chat--dashboard-idle-reject (context message)
  "Handle idle reconciliation rejection MESSAGE for CONTEXT."
  (when (hermes-chat--dashboard-idle-context-valid-p context)
    (if (and (stringp message)
             (null hermes-chat--pending-assistant-id)
             (string-match-p "session not found" (downcase message)))
        (progn
          (setq hermes-chat--dashboard-running-p nil)
          (funcall (plist-get context :on-idle)))
      (hermes-chat--dashboard-reconcile-idle-later
       (hermes-chat--dashboard-next-idle-context context)))))

(defun hermes-chat--dashboard-reconcile-idle (context)
  "Poll the session described by CONTEXT until the backend reports idle."
  (hermes-chat--in-buffer (plist-get context :buffer)
    (when (hermes-chat--dashboard-idle-context-valid-p context)
      (condition-case nil
          (hermes-dashboard-transport-session-resume
           (plist-get context :client) (plist-get context :stored-id)
           :cols (hermes-chat--dashboard-cols)
           :profile hermes-chat--profile
           :resolve (lambda (result)
                      (hermes-chat--in-buffer (plist-get context :buffer)
                        (hermes-chat--dashboard-handle-idle-result
                         context result)))
           :reject (lambda (message)
                     (hermes-chat--in-buffer (plist-get context :buffer)
                       (hermes-chat--dashboard-idle-reject context message))))
        (error
         (hermes-chat--dashboard-reconcile-idle-later
          (hermes-chat--dashboard-next-idle-context context)))))))

(defun hermes-chat--dashboard-schedule-idle-reconciliation (on-idle)
  "Schedule ON-IDLE after this session is authoritatively no longer running."
  (if (not hermes-chat--dashboard-running-p)
      (funcall on-idle)
    (when (and hermes-chat--dashboard-client
               hermes-chat--dashboard-active-session-id)
      (hermes-chat--dashboard-reconcile-idle-later
       (hermes-chat--dashboard-idle-context on-idle)))))

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
        (unless (and dashboard-p
                     (funcall hermes-chat--busy-submit-event-function event))
          (when-let* ((target-id (if dashboard-p
                                     (hermes-chat--dashboard-event-assistant-id
                                      assistant-id event)
                                   assistant-id)))
            (if (and dashboard-p
                     (hermes-chat--dashboard-suppressed-content-event-p
                      event))
                (hermes-chat--handle-suppressed-dashboard-terminal-event
                 target-id event)
              (hermes-chat--handle-transport-event target-id event))))))))

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
  (setq hermes-chat--dashboard-running-p t)
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
  (setq-local hermes-dashboard-transport-request-owner (current-buffer))
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

(defun hermes-chat--dashboard-submit-prompt
    (client prompt &optional resolve reject)
  "Submit PROMPT to CLIENT's active dashboard session.
RESOLVE and REJECT receive the asynchronous request result."
  (unless hermes-chat--dashboard-active-session-id
    (user-error "Hermes dashboard did not return a live session id"))
  (setq hermes-chat--dashboard-running-p t)
  (hermes-dashboard-transport-prompt-submit
   client prompt
   :session-id hermes-chat--dashboard-active-session-id
   :resolve resolve
   :reject reject))

(defun hermes-chat--dashboard-after-session
    (client prompt result &optional resume-p resolve reject queued-p generation)
  "Record CLIENT session RESULT and submit PROMPT.
When RESUME-P is non-nil and RESULT reports a live turn, keep local busy
state instead of submitting another prompt into that durable session.  On a
fresh session, pending create-time runtime overrides are applied through
`config.set' before the prompt is submitted.  RESOLVE and REJECT receive the
prompt request result.  QUEUED-P means a local FIFO entry owns the request.
GENERATION scopes asynchronous create-time overrides."
  (hermes-chat--dashboard-record-session client result)
  (cond
   ((and resume-p (hermes-chat--dashboard-result-live-turn-p result))
    (when (and queued-p reject)
      (funcall reject "session is still running"))
    (hermes-chat--dashboard-restore-inflight-turn client))
   (resume-p
    (setq hermes-chat--dashboard-detached-assistant-id nil)
    (hermes-chat--dashboard-submit-prompt client prompt resolve reject))
   (t
    (setq hermes-chat--dashboard-detached-assistant-id nil)
    (hermes-chat--dashboard-apply-create-overrides
     client (lambda ()
              (hermes-chat--dashboard-submit-prompt
               client prompt resolve reject))
     generation))))

(defun hermes-chat--dashboard-session-resolver
    (buffer client prompt &optional resume-p resolve reject queued-p)
  "Return a callback that records CLIENT's session in BUFFER and sends PROMPT.
RESUME-P means the callback handles a `session.resume' response.  RESOLVE and
REJECT receive the following prompt request result.  QUEUED-P identifies a
local FIFO submission."
  (let ((generation hermes-chat--lifecycle-generation))
    (lambda (result)
      (hermes-chat--in-buffer buffer
        (when (hermes-chat--dashboard-context-current-p client generation)
          (hermes-chat--dashboard-after-session
           client prompt result resume-p resolve reject queued-p generation))))))

(defun hermes-chat--dashboard-session-attached-p ()
  "Return non-nil when the current buffer has a live dashboard session."
  (and hermes-chat--dashboard-session-ready-p
       hermes-chat--dashboard-active-session-id))

(defun hermes-chat--dashboard-create-config-cells ()
  "Return pending (KEY . VALUE) `config.set' cells for this buffer.
The cells carry the `hermes-chat--dashboard-create-*' runtime overrides
picked before the session existed.  The `session.create' handler ignores
runtime override parameters, so the overrides are applied to the fresh
session through `config.set'; `session.resume' owns its stored runtime and
must not receive them."
  (append
   (and hermes-chat--dashboard-create-model
        (list (cons "model"
                    (hermes-chat--model-config-value
                     (list :model hermes-chat--dashboard-create-model
                           :provider hermes-chat--dashboard-create-provider)))))
   (and hermes-chat--dashboard-create-reasoning-effort
        (list (cons "reasoning" hermes-chat--dashboard-create-reasoning-effort)))
   (and hermes-chat--dashboard-create-fast-p
        (list (cons "fast" "fast")))))

(defun hermes-chat--dashboard-clear-create-overrides ()
  "Reset this buffer's create-time runtime override variables."
  (setq hermes-chat--dashboard-create-model nil
        hermes-chat--dashboard-create-provider nil
        hermes-chat--dashboard-create-reasoning-effort nil
        hermes-chat--dashboard-create-fast-p nil))

(defun hermes-chat--dashboard-apply-create-overrides
    (client continue &optional generation)
  "Apply pending create-time overrides to CLIENT's fresh session, then CONTINUE.
Each override is sent as a `config.set' scoped to the session recorded in
the current buffer.  CONTINUE runs in this buffer once every request has
settled, so a following `prompt.submit' cannot race the model switch; a
failed override is reported as a chat error without blocking CONTINUE.
GENERATION prevents a cleared buffer from receiving late results."
  (let ((cells (hermes-chat--dashboard-create-config-cells))
        (buffer (current-buffer))
        (session-id hermes-chat--dashboard-active-session-id))
    (hermes-chat--dashboard-clear-create-overrides)
    (if (null cells)
        (funcall continue)
      (hermes--promise-then
       (hermes--promise-all
        (mapcar (lambda (cell)
                  (hermes-dashboard-transport-call-fn
                   #'hermes-dashboard-transport-config-set
                   client (car cell) (cdr cell) :session-id session-id))
                cells))
       (lambda (_values)
         (hermes-chat--in-buffer buffer
           (when (and generation
                      (hermes-chat--dashboard-context-current-p
                       client generation session-id))
             (funcall continue))))
       (lambda (message)
         (hermes-chat--in-buffer buffer
           (when (and generation
                      (hermes-chat--dashboard-context-current-p
                       client generation session-id))
             (hermes-chat--command-error
              (format "Pre-session override failed: %s" message))
             (funcall continue))))))))

(defun hermes-chat--dashboard-ensure-session
    (client prompt buffer &optional resolve reject queued-p)
  "Create or resume CLIENT's dashboard session before submitting PROMPT.
Record asynchronous session results in BUFFER.  RESOLVE and REJECT receive the
prompt request result or a session bootstrap error.  QUEUED-P identifies a
local FIFO submission."
  (cond
   ((hermes-chat--dashboard-session-attached-p)
    (hermes-chat--dashboard-submit-prompt client prompt resolve reject))
   (hermes-chat--session-id
    (hermes-dashboard-transport-session-resume
     client hermes-chat--session-id
     :cols (hermes-chat--dashboard-cols)
     :profile hermes-chat--profile
     :resolve (hermes-chat--dashboard-session-resolver
               buffer client prompt t resolve reject queued-p)
     :reject reject))
   (t
    (hermes-dashboard-transport-session-create
     client
     :cols (hermes-chat--dashboard-cols)
     :title hermes-chat-dashboard-session-title
     :profile hermes-chat--profile
     :resolve (hermes-chat--dashboard-session-resolver
               buffer client prompt nil resolve reject queued-p)
     :reject reject))))

(defun hermes-chat--dashboard-event-for-session-p (event)
  "Return non-nil when EVENT belongs to this buffer's live dashboard session."
  (let ((session-id (plist-get event :session-id)))
    (or (null session-id)
        (and hermes-chat--dashboard-active-session-id
             (equal session-id hermes-chat--dashboard-active-session-id)))))

(defun hermes-chat--dashboard-send
    (prompt callback &optional resolve reject queued-p)
  "Send PROMPT through the dashboard transport and stream to CALLBACK.
RESOLVE and REJECT receive the prompt request result.  QUEUED-P identifies a
local FIFO submission."
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-start callback)))
    (hermes-chat--dashboard-ensure-session
     client prompt buffer resolve reject queued-p)
    client))

(defun hermes-chat--send-prompt
    (prompt callback &optional resolve reject queued-p)
  "Send PROMPT to Hermes and stream transport events to CALLBACK.
RESOLVE and REJECT apply to dashboard request acceptance.  QUEUED-P identifies
a local FIFO submission."
  (if (hermes-chat--dashboard-default-transport-p)
      (hermes-chat--dashboard-send prompt callback resolve reject queued-p)
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
  (setq-local hermes-dashboard-transport-request-owner (current-buffer))
  (cond
   ((hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    hermes-chat--dashboard-client)
   ((hermes-chat--dashboard-default-transport-p)
    (hermes-chat--dashboard-ensure-client))
   (t
    (user-error "Hermes dashboard transport controls are unavailable"))))

(defun hermes-chat--dashboard-action-resolver
    (buffer client action generation &optional create-p)
  "Return a resolver to record CLIENT's session in BUFFER, then call ACTION.
With CREATE-P non-nil the resolver handles a fresh `session.create' result,
so pending create-time runtime overrides are applied before ACTION."
  (lambda (result)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--dashboard-context-current-p client generation)
        (hermes-chat--dashboard-record-session client result)
        (when (hermes-chat--dashboard-result-live-turn-p result)
          (hermes-chat--dashboard-restore-inflight-turn client)
          (hermes-chat--dashboard-bind-stream-callback
           client hermes-chat--pending-assistant-id))
        (if create-p
            (hermes-chat--dashboard-apply-create-overrides
             client (lambda () (funcall action client)) generation)
          (funcall action client))))))

(defun hermes-chat--dashboard-action-rejecter
    (buffer client generation reject)
  "Return BUFFER callback for REJECT scoped to CLIENT and GENERATION."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--dashboard-context-current-p client generation)
        (if reject
            (funcall reject message)
          (hermes-chat--command-error message))))))

(defun hermes-chat--dashboard-ensure-session-action
    (client buffer action &optional reject)
  "Ensure CLIENT has a session in BUFFER, then call ACTION with CLIENT.
When dashboard session bootstrap fails, call REJECT with the error message."
  (let ((generation hermes-chat--lifecycle-generation))
    (cond
     ((hermes-chat--dashboard-session-attached-p)
      (funcall action client))
     (hermes-chat--session-id
      (hermes-dashboard-transport-session-resume
       client hermes-chat--session-id
       :cols (hermes-chat--dashboard-cols)
       :profile hermes-chat--profile
       :resolve (hermes-chat--dashboard-action-resolver
                 buffer client action generation)
       :reject (hermes-chat--dashboard-action-rejecter
                buffer client generation reject)))
     (t
      (hermes-dashboard-transport-session-create
       client
       :cols (hermes-chat--dashboard-cols)
       :title hermes-chat-dashboard-session-title
       :resolve (hermes-chat--dashboard-action-resolver
                 buffer client action generation t)
       :reject (hermes-chat--dashboard-action-rejecter
                buffer client generation reject))))))

(defun hermes-chat--with-dashboard-session (content buffer action &optional reject)
  "Ensure a live dashboard session for BUFFER, then call ACTION with the client.
CONTENT is restored to the input tail when session bootstrap fails.  REJECT
overrides the default failure handler, which renders the error and preserves
CONTENT.  This is the one spelling of the control-RPC bootstrap stack; call it
instead of nesting `hermes-chat--call-with-dashboard-bootstrap-error',
`hermes-chat--dashboard-control-client', and
`hermes-chat--dashboard-ensure-session-action' by hand."
  (hermes-chat--call-with-dashboard-bootstrap-error
   content
   (lambda ()
     (hermes-chat--dashboard-ensure-session-action
      (hermes-chat--dashboard-control-client) buffer action
      (or reject
          (lambda (message)
            (hermes-chat--dashboard-bootstrap-error message content)))))))

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
      (hermes-chat--with-dashboard-session
       content buffer
       (lambda (_live-client)
         (hermes-chat--queue-or-submit-content content display)))
    (hermes-chat--queue-or-submit-content content display)))

(defun hermes-chat--dashboard-handle-reconnected (_event)
  "Re-attach this buffer's stored dashboard session after a socket reconnect.
A no-op unless the buffer has a durable session that is not currently attached
and no turn is active -- the same guard a later send uses -- so a reconnected
shared socket re-resumes every attached chat without waiting for the next send."
  (when (hermes-chat--dashboard-stored-session-needs-resume-p)
    (hermes-chat--with-dashboard-session
     nil (current-buffer) #'ignore
     (lambda (message)
       (hermes-chat--command-error
        (format "Hermes reconnect resume failed: %s" message))))))

;;; Session title and background tasks

;; Dashboard-session behavior: server titles and /btw background
;; tasks live with the session lifecycle that drives them.

(defun hermes-chat--buffer-name-for-title (profile title)
  "Return a chat buffer name from PROFILE and TITLE.
PROFILE nil means the default profile.  A nil or empty TITLE yields a name with
just the profile, so buffers stay distinct before a session title arrives."
  (let ((profile (or profile "default")))
    (if (and title (not (string-empty-p title)))
        (format "*Hermes@%s: %s*" profile title)
      (format "*Hermes@%s*" profile))))

(defun hermes-chat--push-session-title (title)
  "Push TITLE to the server with `session.title' when a session is attached.
With no live session the rename stays buffer-local; report that instead."
  (if (and (hermes-chat--dashboard-session-attached-p)
           hermes-chat--dashboard-active-session-id)
      (let ((buffer (current-buffer)))
        (hermes-dashboard-transport-session-title
         hermes-chat--dashboard-client
         :session-id hermes-chat--dashboard-active-session-id
         :title title
         :resolve (lambda (result)
                    (when (and (buffer-live-p buffer)
                               (eq (hermes-transport--get result 'pending) t))
                      (message "Title queued; applies once the session is saved")))
         :reject (lambda (message)
                   (hermes-chat--in-buffer buffer
                     (hermes-chat--command-error message)))))
    (message "Renamed buffer; no live session to update on the server")))

(defun hermes-chat--apply-session-title (title)
  "Record TITLE and rename this buffer to match, without updating the server."
  (setq hermes-chat--title title)
  (let ((newname (hermes-chat--buffer-name-for-title
                  hermes-chat--profile title)))
    (unless (equal (buffer-name) newname)
      (rename-buffer newname t)))
  (force-mode-line-update))

(defun hermes-chat--should-apply-title-p (title current manual-p)
  "Return non-nil when TITLE should replace CURRENT in the buffer name.
TITLE applies only when it is a non-empty string, differs from CURRENT, and
MANUAL-P is nil (the user has not pinned a title)."
  (and (not manual-p)
       (stringp title)
       (not (string-empty-p title))
       (not (equal title current))))

(defun hermes-chat--apply-fetched-title (buffer result)
  "Apply the session title carried by RESULT to BUFFER when it should change."
  (hermes-chat--in-buffer buffer
    (let ((title (string-trim
                  (or (hermes-transport--scalar-string
                       (hermes-transport--get result 'title))
                      ""))))
      (when (hermes-chat--should-apply-title-p
             title hermes-chat--title hermes-chat--title-manual-p)
        (hermes-chat--apply-session-title title)))))

(defun hermes-chat--fetch-session-title (buffer)
  "Fetch BUFFER's server session title and apply it to the buffer name.
Guards are re-checked here since this runs after the turn settles."
  (hermes-chat--in-buffer buffer
    (when (and (hermes-chat--dashboard-session-attached-p)
               (not hermes-chat--title-manual-p))
      (hermes-dashboard-transport-session-title-fetch
       hermes-chat--dashboard-client
       :session-id hermes-chat--dashboard-active-session-id
       :resolve (lambda (result)
                  (hermes-chat--apply-fetched-title buffer result))
       ;; A background title fetch must never surface as a chat error; swallow
       ;; failures rather than letting them reach the transport callback.
       :reject #'ignore))))

(defun hermes-chat--maybe-refresh-session-title ()
  "Schedule a server session-title refresh for this buffer after a turn settles.
Deferred to the next idle moment so no network I/O runs inside the transport
event handler.  A no-op without a live dashboard session or with a manual title."
  (when (and (hermes-chat--dashboard-session-attached-p)
             (not hermes-chat--title-manual-p))
    (run-at-time 0 nil #'hermes-chat--fetch-session-title (current-buffer))))

(defun hermes-chat-rename (title)
  "Rename this chat session to TITLE.
Renames the buffer and, when a live dashboard session is attached, updates the
server title via `session.title'.  A manual rename is kept against the automatic
session-title refresh."
  (interactive
   (list (read-string "Hermes chat title: " (or hermes-chat--title ""))))
  (let ((title (string-trim title)))
    (when (string-empty-p title)
      (user-error "Title must not be empty"))
    (setq hermes-chat--title-manual-p t)
    (hermes-chat--apply-session-title title)
    (hermes-chat--push-session-title title)))

(defun hermes-chat-background (&optional prompt)
  "Run PROMPT as a Hermes background task, delivering its result to this chat.
With no PROMPT, use the input tail.  The task runs in its own session via
`prompt.background', so it does not block the current turn; its answer returns
later as a `background.complete' event rendered as a persistent [View Result]
entry."
  (interactive)
  (let ((content (string-trim (or prompt (hermes-chat-input-string))))
        (buffer (current-buffer)))
    (when (string-empty-p content)
      (user-error "No Hermes background prompt given"))
    (unless prompt
      (hermes-chat--delete-input-tail))
    (hermes-chat--background-submit content buffer)))

(defun hermes-chat--background-started (result prompt buffer)
  "Record the background task in RESULT for PROMPT and show a started notice.
BUFFER's client gains a result listener when no turn is streaming, so the
`background.complete' event is delivered even on an otherwise idle chat."
  (let ((task-id (hermes-transport--scalar-string
                  (hermes-transport--get result 'task_id)))
        (number (cl-incf hermes-chat--background-counter))
        (preview (hermes-chat--preview prompt)))
    (when task-id
      (push (cons task-id (list :number number :preview preview))
            hermes-chat--background-tasks))
    (hermes-chat--ensure-background-listener hermes-chat--dashboard-client buffer)
    ;; Insert above any pending reply so the active turn's answer stays last.
    (hermes-chat--insert-entry
     (hermes-chat--make-entry
      'status (format "Background #%d started: %s" number preview) 'running)
     (hermes-chat--pending-assistant-node))))

(defun hermes-chat--background-submit (content buffer)
  "Launch CONTENT as a background task for BUFFER's dashboard session."
  (hermes-chat--with-dashboard-session
   content buffer
   (lambda (live-client)
     (hermes-dashboard-transport-prompt-background
      live-client content
      :session-id hermes-chat--dashboard-active-session-id
      :resolve (lambda (result)
                 (hermes-chat--in-buffer buffer
                   (hermes-chat--background-started result content buffer)))
      :reject (lambda (message)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--command-error message)))))))

(defun hermes-chat--handle-background-complete (event)
  "Insert a persistent result entry for a `background' EVENT.
EVENT's `:task-id' is paired with the launching task's number and preview.  The
entry is inserted before any pending assistant reply -- nil before-node when the
chat is idle, so it simply lands last -- so a result arriving mid-turn keeps the
active turn's answer at the bottom.  The counter is owned by the launch path and
is not advanced here; an unrecorded result falls back to its current value."
  (let* ((task-id (plist-get event :task-id))
         (info (and task-id (cdr (assoc task-id hermes-chat--background-tasks))))
         (number (or (plist-get info :number) hermes-chat--background-counter))
         (preview (or (plist-get info :preview) ""))
         (content (or (plist-get event :content) "")))
    (when task-id
      (setq hermes-chat--background-tasks
            (assoc-delete-all task-id hermes-chat--background-tasks)))
    (hermes-chat--insert-entry
     (hermes-chat--make-entry
      'background content 'done nil
      (list :number number :preview preview))
     (hermes-chat--pending-assistant-node))
    (hermes-notifications-notify
     'background "Hermes background task finished"
     (if (string-empty-p preview)
         (format "Background #%d finished" number)
       (format "Background #%d: %s" number preview))
     :buffer (current-buffer) :category "hermes.chat.background"
     :urgency 'normal)))


(provide 'hermes-chat-dashboard)
;;; hermes-chat-dashboard.el ends here
