;;; hermes-chat-dashboard.el --- Dashboard lifecycle for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
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
(require 'hermes-session-title)


(defvar hermes-chat-dashboard-session-title)
(defvar hermes-chat-use-dashboard-transport)

(defvar hermes-chat--active-tools)
(defvar hermes-chat--pending-assistant-id)
(defvar hermes-chat--process)
(defvar hermes-chat--profile)
(defvar hermes-chat--session-id)
(defvar hermes-chat--status-state)
(defvar hermes-chat--working-directory)
(defvar hermes-chat--transport-generation)
(defvar hermes-chat--lifecycle-generation)

(defvar-local hermes-chat--title nil
  "Human title for this chat session.
Set by `hermes-chat-rename' and reported to the dashboard.  Buffer identity is
derived separately from the owning instance, profile, and working directory.")

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
(defvar hermes-chat--server-queued-prior-terminal-p)
(defvar hermes-chat--busy-submit-context)
(defvar hermes-chat--dashboard-idle-count)
(defvar hermes-chat--dashboard-last-start-idle-count)
(defvar hermes-chat--unsettled-submit-context)
(defvar hermes-chat--prepared-submit-assistant-id)
(defvar hermes-chat--create-override-owner)
(defvar hermes-chat--create-overrides-retry-session-id)
(defvar hermes-dashboard-transport-request-owner)

(defconst hermes-chat--terminal-clear-fields
  '(hermes-chat--dashboard-client
    hermes-chat--dashboard-token
    hermes-chat--process
    hermes-chat--dashboard-active-session-id
    hermes-chat--dashboard-session-ready-p
    hermes-chat--dashboard-running-p
    hermes-chat--pending-assistant-id
    hermes-chat--dashboard-stream-assistant-id
    hermes-chat--dashboard-interim-assistant-id
    hermes-chat--dashboard-detached-assistant-id
    hermes-chat--dashboard-suppress-stream-p
    hermes-chat--dashboard-last-start-idle-count
    hermes-chat--server-queued-assistant-id
    hermes-chat--server-queued-user-id
    hermes-chat--server-queued-after-idle-count
    hermes-chat--server-queued-prior-terminal-p
    hermes-chat--busy-submit-context
    hermes-chat--unsettled-submit-context
    hermes-chat--prepared-submit-assistant-id
    hermes-chat--queued-submit-id
    hermes-chat--interrupted-assistant-id
    hermes-chat--interrupted-events
    hermes-chat--interrupt-request-pending-p
    hermes-dashboard-transport-request-owner
    hermes-chat--active-tools)
  "Ephemeral fields cleared by a terminal state commit.")

(defun hermes-chat--terminal-fingerprint (value)
  "Return VALUE's deterministic private SHA-256 fingerprint."
  (let ((print-length nil)
        (print-level nil)
        (print-circle t)
        (print-gensym t)
        (print-quoted t)
        (print-continuous-numbering nil)
        (print-number-table nil)
        (print-escape-newlines t)
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-escape-multibyte t)
        (print-charset-text-property t)
        (print-unreadable-function nil)
        (print-integers-as-characters nil)
        (print-symbols-bare nil)
        (float-output-format nil))
    (secure-hash 'sha256 (prin1-to-string value))))

(defun hermes-chat--terminal-field-record (field value)
  "Return exact terminal FIELD record for VALUE."
  (list field value (hermes-chat--terminal-fingerprint value)))

(defun hermes-chat--terminal-field-record-schema-p (record field)
  "Return non-nil when RECORD has the exact schema for FIELD."
  (and (proper-list-p record)
       (= (length record) 3)
       (eq (car record) field)
       (stringp (nth 2 record))
       (string-match-p "\\`[0-9a-fA-F]\\{64\\}\\'" (nth 2 record))))

(defun hermes-chat--terminal-fields-schema-p (records)
  "Return non-nil when RECORDS exactly match the terminal field catalog."
  (and (proper-list-p records)
       (= (length records) (length hermes-chat--terminal-clear-fields))
       (cl-every #'hermes-chat--terminal-field-record-schema-p
                 records hermes-chat--terminal-clear-fields)))

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

(defvar hermes-chat--terminal-owner-functions nil
  "Ordered (CAPTURE . TAKE) functions for chat terminal authority.")

(defun hermes-chat--capture-terminal-owners ()
  "Capture each registered terminal owner as plain data in registry order."
  (mapcar (lambda (functions)
            (cons (cdr functions) (funcall (car functions))))
          hermes-chat--terminal-owner-functions))

(defconst hermes-chat--terminal-owner-registry-schema
  '((hermes-chat--capture-terminal-prompts
     . hermes-chat--take-terminal-prompts)
    (hermes-chat--capture-command-terminal-owner
     . hermes-chat--take-command-terminal-owner)
    (hermes-chat--capture-handoff-terminal-owner
     . hermes-chat--take-handoff-terminal-owner))
  "Exact registered terminal owner schemas in capture order.")

(defun hermes-chat--terminal-exact-plist-p (value keys)
  "Return non-nil when VALUE is a plist with exactly ordered KEYS."
  (and (proper-list-p value)
       (= (length value) (* 2 (length keys)))
       (equal (cl-loop for tail on value by #'cddr collect (car tail)) keys)))

(defun hermes-chat--terminal-owner-registry-schema-p (registry)
  "Return non-nil when REGISTRY has the exact known owner functions."
  (and (proper-list-p registry)
       (equal registry hermes-chat--terminal-owner-registry-schema)))

(defun hermes-chat--terminal-prompt-entry-schema-p (entry)
  "Return non-nil when ENTRY has the exact terminal prompt entry schema."
  (hermes-chat--terminal-exact-plist-p
   entry '(:key :prompt :response-token :approval-p
           :approval-members :session-id)))

(defun hermes-chat--terminal-auto-claim-schema-p (claim)
  "Return non-nil when CLAIM has the exact A3a record schema."
  (hermes-chat--terminal-exact-plist-p claim '(:key :claim :prompt)))

(defun hermes-chat--terminal-prompt-snapshot-schema-p (snapshot)
  "Return non-nil when SNAPSHOT has the exact prompt authority schema."
  (and (hermes-chat--terminal-exact-plist-p
        snapshot '(:buffer :generation :prompt-table :auto-table :auto-claims
                   :retained-owners :entries))
       (proper-list-p (plist-get snapshot :retained-owners))
       (proper-list-p (plist-get snapshot :entries))
       (cl-every #'hermes-chat--terminal-prompt-entry-schema-p
                 (plist-get snapshot :entries))
       (let ((claims (plist-get snapshot :auto-claims)))
         (and (not (eq claims hermes-chat--invalid-terminal-auto-claims))
              (proper-list-p claims)
              (cl-every #'hermes-chat--terminal-auto-claim-schema-p claims)))))

(defun hermes-chat--terminal-owner-records-schema-p (owners)
  "Return non-nil when OWNERS contain only exact known ordered schemas."
  (and (proper-list-p owners)
       (= (length owners) 3)
       (equal (mapcar #'car owners)
              (mapcar #'cdr hermes-chat--terminal-owner-registry-schema))
       (hermes-chat--terminal-prompt-snapshot-schema-p (cdr (nth 0 owners)))
       (hermes-chat--terminal-exact-plist-p (cdr (nth 1 owners)) '(:owner))
       (hermes-chat--terminal-exact-plist-p
        (cdr (nth 2 owners)) '(:owner :poll :timer))))

(defun hermes-chat--terminal-exact-occurrences-p (saved current)
  "Return non-nil when SAVED and CURRENT contain the same occurrences."
  (and (proper-list-p saved) (proper-list-p current)
       (= (length saved) (length current))
       (cl-every #'eq saved current)))

(defun hermes-chat--terminal-prompt-entry-current-p (saved current)
  "Return non-nil when CURRENT retains SAVED prompt entry authority."
  (and (equal (plist-get saved :key) (plist-get current :key))
       (eq (plist-get saved :prompt) (plist-get current :prompt))
       (eq (plist-get saved :response-token) (plist-get current :response-token))
       (eq (plist-get saved :approval-p) (plist-get current :approval-p))
       (equal (plist-get saved :session-id) (plist-get current :session-id))
       (hermes-chat--terminal-exact-occurrences-p
        (plist-get saved :approval-members)
        (plist-get current :approval-members))))

(defun hermes-chat--terminal-auto-claim-current-p (saved current)
  "Return non-nil when CURRENT retains SAVED exact auto-claim authority."
  (and (equal (plist-get saved :key) (plist-get current :key))
       (eq (plist-get saved :claim) (plist-get current :claim))
       (eq (plist-get saved :prompt) (plist-get current :prompt))))

(defun hermes-chat--terminal-prompt-snapshot-current-p (saved current)
  "Return non-nil when CURRENT retains all SAVED prompt authority."
  (and (eq (plist-get saved :buffer) (plist-get current :buffer))
       (eql (plist-get saved :generation) (plist-get current :generation))
       (eq (plist-get saved :prompt-table) (plist-get current :prompt-table))
       (eq (plist-get saved :auto-table) (plist-get current :auto-table))
       (hermes-chat--terminal-exact-occurrences-p
        (plist-get saved :retained-owners) (plist-get current :retained-owners))
       (cl-every #'hermes-chat--terminal-prompt-entry-current-p
                 (plist-get saved :entries) (plist-get current :entries))
       (cl-every #'hermes-chat--terminal-auto-claim-current-p
                 (plist-get saved :auto-claims)
                 (plist-get current :auto-claims))))

(defun hermes-chat--terminal-owner-records-current-p (saved current)
  "Return non-nil when CURRENT retains all SAVED registered owner authority."
  (and (hermes-chat--terminal-owner-records-schema-p saved)
       (hermes-chat--terminal-owner-records-schema-p current)
       (hermes-chat--terminal-prompt-snapshot-current-p
        (cdr (nth 0 saved)) (cdr (nth 0 current)))
       (eq (plist-get (cdr (nth 1 saved)) :owner)
           (plist-get (cdr (nth 1 current)) :owner))
       (cl-every #'eq
                 (mapcar (lambda (key) (plist-get (cdr (nth 2 saved)) key))
                         '(:owner :poll :timer))
                 (mapcar (lambda (key) (plist-get (cdr (nth 2 current)) key))
                         '(:owner :poll :timer)))))

(defun hermes-chat--capture-terminal-owner-records ()
  "Return exact current owner records; return nil after a registry change."
  (let ((registry hermes-chat--terminal-owner-functions))
    (when (hermes-chat--terminal-owner-registry-schema-p registry)
      (let ((owners (hermes-chat--capture-terminal-owners)))
        (and (eq registry hermes-chat--terminal-owner-functions)
             (hermes-chat--terminal-owner-registry-schema-p registry)
             owners)))))

(defun hermes-chat--capture-terminal-owner-authority ()
  "Capture inert digest-backed authority for all registered terminal owners."
  (condition-case nil
      (when-let* ((registry hermes-chat--terminal-owner-functions)
                  (owners (hermes-chat--capture-terminal-owner-records)))
        (list :registry registry :owners owners
              :digest (hermes-chat--terminal-fingerprint owners)))
    (error nil)
    (quit nil)))

(defun hermes-chat--terminal-owner-authority-current-p (authority)
  "Return non-nil when AUTHORITY retains every registered terminal owner."
  (condition-case nil
      (and (hermes-chat--terminal-exact-plist-p
            authority '(:registry :owners :digest))
           (eq (plist-get authority :registry)
               hermes-chat--terminal-owner-functions)
           (hermes-chat--terminal-owner-registry-schema-p
            hermes-chat--terminal-owner-functions)
           (hermes-chat--terminal-owner-records-schema-p
            (plist-get authority :owners))
           (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                           (plist-get authority :digest))
           (equal (plist-get authority :digest)
                  (hermes-chat--terminal-fingerprint
                   (plist-get authority :owners)))
           (let ((buffer (plist-get (cdr (car (plist-get authority :owners)))
                                    :buffer)))
             (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'hermes-chat-mode)
                         (eq (plist-get authority :registry)
                             hermes-chat--terminal-owner-functions)
                         (when-let* ((current
                                     (hermes-chat--capture-terminal-owner-records)))
                           (and (equal (plist-get authority :digest)
                                       (hermes-chat--terminal-fingerprint current))
                                (hermes-chat--terminal-owner-records-current-p
                                 (plist-get authority :owners) current))))))))
    (error nil)
    (quit nil)))

(defun hermes-chat--take-terminal-owners (snapshot)
  "Take current owners in SNAPSHOT and return ordered dormant effects."
  (mapcan (lambda (entry)
            (funcall (car entry) (cdr entry)))
          snapshot))

(defun hermes-chat-register-cleanup-function (function)
  "Register FUNCTION to release per-buffer chat resources."
  (add-hook 'hermes-chat-cleanup-functions function))

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
        :title (or (and hermes-chat--title
                        (hermes-session-title-chat-display hermes-chat--title))
                   (buffer-name))
        :instance (and (hermes-instance--valid-p hermes-instance)
                       (hermes-instance-name hermes-instance))
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
  "Release this chat lifetime's resources before mode exit or buffer kill."
  (unless hermes-chat--cleanup-done-p
    (setq hermes-chat--cleanup-done-p t)
    (hermes-chat--invalidate-transport-state)
    (run-hooks 'hermes-chat-cleanup-functions)
    (hermes-chat--stop-dashboard-client)
    (hermes-chat--notify-state-change)))

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

(defun hermes-chat--server-queued-start-ready-p (idle-count)
  "Return non-nil when IDLE-COUNT or a terminal event permits queue handoff."
  (or hermes-chat--server-queued-prior-terminal-p
      (> idle-count (or hermes-chat--server-queued-after-idle-count 0))))

(defun hermes-chat--dashboard-activate-server-queued-turn (assistant-id)
  "Start rendering the backend-owned queued turn for ASSISTANT-ID."
  (when (and (equal assistant-id hermes-chat--server-queued-assistant-id)
             (numberp hermes-chat--server-queued-after-idle-count)
             (hermes-chat--server-queued-start-ready-p
              hermes-chat--dashboard-last-start-idle-count))
    (setq hermes-chat--server-queued-assistant-id nil
          hermes-chat--server-queued-user-id nil
          hermes-chat--server-queued-after-idle-count nil
          hermes-chat--server-queued-prior-terminal-p nil
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
        hermes-chat--server-queued-after-idle-count nil
        hermes-chat--server-queued-prior-terminal-p nil))

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
  "Record working-directory and idle state from session-info EVENT."
  (when-let* (((hermes-chat--session-info-event-p event))
              (cwd (plist-get event :cwd)))
    (hermes-chat--record-working-directory cwd))
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

(defun hermes-chat--dashboard-note-server-queued-terminal (event)
  "Record terminal EVENT as the boundary before a server-queued turn."
  (when (and hermes-chat--server-queued-assistant-id
             (hermes-chat--dashboard-terminal-event-p event))
    (setq hermes-chat--server-queued-prior-terminal-p t)))

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
         (hermes-chat--server-queued-start-ready-p
          hermes-chat--dashboard-idle-count))
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
                  hermes-chat--server-queued-after-idle-count nil
                  hermes-chat--server-queued-prior-terminal-p nil))
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

(defun hermes-chat--dashboard-settle-terminal (assistant-id interrupted-p)
  "Settle dashboard bookkeeping for ASSISTANT-ID's terminal event.
When INTERRUPTED-P is non-nil, also clear the interrupt request state."
  (when interrupted-p
    (setq hermes-chat--interrupted-assistant-id nil
          hermes-chat--interrupted-events nil
          hermes-chat--interrupt-request-pending-p nil))
  (unless (and hermes-chat--pending-assistant-id
               (not (equal hermes-chat--pending-assistant-id assistant-id)))
    (hermes-chat--dashboard-schedule-idle-reconciliation
     #'hermes-chat--drain-queued-message)))

(defun hermes-chat--interrupted-error-event-p (event)
  "Return whether error EVENT represents an intentional interruption."
  (member (hermes-chat--status-name (hermes-chat--error-status event))
          '("interrupted" "cancelled" "canceled")))

(defun hermes-chat--notify-terminal-event (assistant-id event)
  "Notify for terminal EVENT belonging to ASSISTANT-ID."
  (pcase (plist-get event :type)
    ('done
     (let* ((preview (hermes-notifications-preview
                      (hermes-chat--entry-content-by-id assistant-id)))
            (message (if (string-empty-p preview) "Reply completed" preview)))
       (hermes-notifications-notify
        'chat-reply (format "%s: %s" (buffer-name) message) message
        :buffer (current-buffer) :category "hermes.chat" :urgency 'normal)))
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
    (when (hermes-chat--prompt-expire-event-p event)
      (hermes-chat--expire-pending-prompt event)
      (hermes-chat--show-pending-prompt-state))
    (when (memq (plist-get event :type) '(done error))
      (hermes-chat--notify-terminal-event assistant-id event))
    (when (memq (plist-get event :type) '(done error))
      (hermes-chat--dashboard-settle-terminal assistant-id interrupted-p))
    (when (eq (plist-get event :type) 'done)
      (hermes-chat--maybe-refresh-session-title))
    (unless (memq (plist-get event :type)
                  '(delta interim done error thinking status progress tool
                          commentary diff unknown))
      (message "Unknown Hermes transport event: %S" event))))

(defun hermes-chat--handle-transport-event (assistant-id event)
  "Apply transport EVENT to ASSISTANT-ID in the current chat buffer."
  (hermes-chat--dashboard-note-session-info event)
  (hermes-chat--dashboard-note-server-queued-terminal event)
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

(defun hermes-chat--dashboard-result-cwd (result)
  "Return the gateway working directory carried by RPC RESULT, or nil."
  (hermes-transport--non-empty-string
   (hermes-transport--scalar-string
    (or (hermes-transport--get result 'cwd)
        (and-let* ((info (hermes-transport--get result 'info)))
          (hermes-transport--get info 'cwd))))))

(defun hermes-chat--record-working-directory (directory)
  "Record gateway-native DIRECTORY without changing Emacs's local directory."
  (when (and directory
             (not (equal directory hermes-chat--working-directory)))
    (setq hermes-chat--working-directory directory)
    (hermes-chat--refresh-buffer-name)
    (force-mode-line-update)))

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

(defun hermes-chat--dashboard-goal-status-projection (output)
  "Return compact goal projection parsed from vanilla Hermes OUTPUT.
Return nil for unknown output and leave the current goal state unchanged."
  (cond
   ((and (stringp output) (string-prefix-p "No active goal." output))
    '(:goal nil))
   ((and (stringp output)
         (string-match "\\([0-9]+\\)/\\([0-9]+\\) turns" output))
    (let ((running (string-prefix-p "⊙ Goal (active," output)))
      (list :goal
            (list :status (if running "active" "inactive")
                  :running running
                  :turns-used (string-to-number (match-string 1 output))
                  :max-turns (string-to-number (match-string 2 output))))))))

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
      (when-let* ((cwd (hermes-chat--dashboard-result-cwd result)))
        (hermes-chat--record-working-directory cwd))
      (when (hermes-dashboard-transport-client-p client)
        (hermes-chat--ensure-idle-listener client (current-buffer))
        (hermes-dashboard-transport-subscribe-session
         client hermes-chat--dashboard-token active-id))
      (hermes-chat--dashboard-refresh-goal))))

(defun hermes-chat--dashboard-result-live-turn-p (result)
  "Return non-nil when RESULT reports the resumed session is still busy."
  (or (hermes-transport--get result 'running)
      (hermes-transport--get result 'inflight)))

(defun hermes-chat--dashboard-context-current-p
    (client generation &optional session-id)
  "Return non-nil when CLIENT, GENERATION, and SESSION-ID still own this chat."
  (and (hermes-chat--current-lifetime-p generation)
       (eq client hermes-chat--dashboard-client)
       (or (null session-id)
           (equal session-id hermes-chat--dashboard-active-session-id))))

(defun hermes-chat--dashboard-refresh-goal ()
  "Refresh compact goal state through vanilla Hermes `/goal status'."
  (when (and (hermes-chat--dashboard-client-live-p
              hermes-chat--dashboard-client)
             hermes-chat--dashboard-active-session-id)
    (let ((buffer (current-buffer))
          (client hermes-chat--dashboard-client)
          (generation hermes-chat--lifecycle-generation)
          (session-id hermes-chat--dashboard-active-session-id))
      (hermes-dashboard-transport-command-dispatch
       client "goal" "status"
       :session-id session-id
       :resolve
       (lambda (result)
         (hermes-chat--in-buffer buffer
           (when (hermes-chat--dashboard-context-current-p
                  client generation session-id)
             (when-let* ((output (hermes-chat--dashboard-result-string
                                  result '(output)))
                         (projection
                          (hermes-chat--dashboard-goal-status-projection output)))
               (setq hermes-chat--goal (plist-get projection :goal))
               (force-mode-line-update)))))
       :reject #'ignore))))

(defun hermes-chat--dashboard-idle-context (on-idle)
  "Return an idle-reconciliation context calling ON-IDLE when settled."
  (list :buffer (current-buffer)
        :client hermes-chat--dashboard-client
        :active-id hermes-chat--dashboard-active-session-id
        :stored-id (or hermes-chat--session-id
                       hermes-chat--dashboard-active-session-id)
        :generation hermes-chat--transport-generation
        :delay 0.1
        :terminal-p nil
        :on-idle on-idle))

(defun hermes-chat--dashboard-idle-context-valid-p (context)
  "Return non-nil when idle reconciliation CONTEXT still owns this chat."
  (and (not (plist-get context :terminal-p))
       hermes-chat--dashboard-running-p
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

(defun hermes-chat--dashboard-finish-idle-context (context)
  "Finish CONTEXT exactly once before running its idle effect."
  (setf (plist-get context :terminal-p) t)
  (setq hermes-chat--dashboard-running-p nil)
  (funcall (plist-get context :on-idle)))

(defun hermes-chat--dashboard-handle-idle-result (context result)
  "Handle session resume RESULT for idle reconciliation CONTEXT."
  (when (hermes-chat--dashboard-idle-context-valid-p context)
    (if (hermes-chat--dashboard-result-live-turn-p result)
        (hermes-chat--dashboard-reconcile-idle-later
         (hermes-chat--dashboard-next-idle-context context))
      (hermes-chat--dashboard-finish-idle-context context))))

(defun hermes-chat--dashboard-idle-reject (context message)
  "Handle idle reconciliation rejection MESSAGE for CONTEXT."
  (when (hermes-chat--dashboard-idle-context-valid-p context)
    (if (and (stringp message)
             (string-match-p "session not found" (downcase message)))
        (hermes-chat--dashboard-finish-idle-context context)
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
         (when (hermes-chat--dashboard-idle-context-valid-p context)
           (hermes-chat--dashboard-reconcile-idle-later
            (hermes-chat--dashboard-next-idle-context context))))))))

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
  (let ((lifetime (buffer-local-value 'hermes-chat--lifecycle-generation buffer)))
    (lambda (event)
      (hermes-chat--in-lifetime buffer lifetime
        (when (and (hermes-chat--current-transport-generation-p generation)
                   (or (not dashboard-p)
                       (and (not (hermes-chat--dashboard-control-error-event-p
                                  event))
                            (hermes-chat--dashboard-event-for-session-p event))))
          (unless (and dashboard-p
                       (funcall hermes-chat--busy-submit-event-function event))
            (if (and dashboard-p
                     (hermes-chat--assistant-independent-event-p event))
                (hermes-chat--handle-transport-event nil event)
              (when dashboard-p
                (hermes-chat--dashboard-start-server-turn
                 hermes-chat--dashboard-client event))
              (when-let* ((target-id
                           (if dashboard-p
                               (hermes-chat--dashboard-event-assistant-id
                                assistant-id event)
                             assistant-id)))
                (if (and dashboard-p
                         (hermes-chat--dashboard-suppressed-content-event-p
                          event))
                    (hermes-chat--handle-suppressed-dashboard-terminal-event
                     target-id event)
                  (hermes-chat--handle-transport-event target-id event))))))))))

(defun hermes-chat--assistant-independent-event-p (event)
  "Return non-nil when dashboard EVENT does not belong to an assistant turn."
  (or (eq (plist-get event :type) 'background)
      (hermes-chat--reconnecting-status-event-p event)
      (hermes-chat--reconnected-status-event-p event)))

(defun hermes-chat--dashboard-bind-stream-callback (client assistant-id)
  "Bind CLIENT events to ASSISTANT-ID in the current buffer."
  (when (and (hermes-dashboard-transport-client-p client) assistant-id)
    (hermes-chat--dashboard-set-subscriber
     client
     (hermes-chat--transport-callback
      (current-buffer) assistant-id t
      (hermes-chat--next-transport-generation)))))

(defun hermes-chat--dashboard-start-server-turn (client event)
  "Create local turn state for a backend-owned CLIENT turn announced by EVENT."
  (when (and (hermes-chat--message-start-status-event-p event)
             (null hermes-chat--pending-assistant-id)
             (null hermes-chat--dashboard-stream-assistant-id)
             (null hermes-chat--server-queued-assistant-id))
    (let ((assistant-id (hermes-chat--dashboard-insert-inflight-assistant)))
      (hermes-chat--clear-active-tools)
      (setq hermes-chat--dashboard-running-p t
            hermes-chat--pending-assistant-id assistant-id
            hermes-chat--process client
            hermes-chat--dashboard-stream-assistant-id assistant-id
            hermes-chat--dashboard-suppress-stream-p nil)
      (hermes-chat--set-header-state
       :status 'streaming :activity "Hermes is responding"
       :assistant-id assistant-id)
      assistant-id)))

(defun hermes-chat--ensure-idle-listener (client buffer)
  "Subscribe BUFFER to CLIENT events when it has no turn callback yet.
The ordinary transport callback also creates a local assistant when an idle
session later announces a backend-owned turn, such as a `/loop' wakeup."
  (when (and (hermes-dashboard-transport-client-p client)
             (not hermes-chat--dashboard-token))
    (setq hermes-chat--dashboard-token
          (hermes-dashboard-transport-subscribe
           client (hermes-chat--transport-callback
                   buffer nil t (hermes-chat--next-transport-generation))))))

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
    (let* ((instance (hermes-instance-resolve))
           (hermes-dashboard-transport-url (hermes-instance-url instance)))
      (setq hermes-chat--dashboard-session-ready-p nil
            hermes-chat--dashboard-active-session-id nil
            hermes-chat--dashboard-client
            (hermes-dashboard-transport-acquire
             :callback (or callback #'ignore))))
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

(defun hermes-chat--dashboard-claim-submit-context (client)
  "Bind the current unsettled submission to CLIENT and its live session."
  (when-let* ((context hermes-chat--unsettled-submit-context))
    (setf (plist-get context :client) client
          (plist-get context :session-id)
          hermes-chat--dashboard-active-session-id)))

(defun hermes-chat--dashboard-submit-prompt
    (client prompt &optional resolve reject)
  "Submit PROMPT to CLIENT's active dashboard session.
RESOLVE and REJECT receive the asynchronous request result."
  (unless hermes-chat--dashboard-active-session-id
    (user-error "Hermes dashboard did not return a live session id"))
  (hermes-chat--dashboard-claim-submit-context client)
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
  (hermes-chat--dashboard-claim-submit-context client)
  (cond
   ((and resume-p (hermes-chat--dashboard-result-live-turn-p result))
    (when (and queued-p reject)
      (funcall reject "session is still running"))
    (hermes-chat--dashboard-restore-inflight-turn client))
   (resume-p
    (setq hermes-chat--dashboard-detached-assistant-id nil)
    (hermes-chat--dashboard-apply-retry-overrides
     client
     (lambda ()
       (hermes-chat--dashboard-submit-prompt client prompt resolve reject))
     generation reject))
   (t
    (setq hermes-chat--dashboard-detached-assistant-id nil)
    (hermes-chat--dashboard-apply-create-overrides
     client (lambda ()
              (hermes-chat--dashboard-submit-prompt
               client prompt resolve reject))
     generation reject))))

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

(defun hermes-chat--dashboard-queue-drain-ready-p ()
  "Return non-nil when the current chat queue may submit."
  (or (not (hermes-chat--dashboard-default-transport-p))
      (and (hermes-chat--dashboard-session-attached-p)
           (hermes-chat--dashboard-client-live-p
            hermes-chat--dashboard-client))))

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
        hermes-chat--dashboard-create-fast-p nil
        hermes-chat--create-overrides-retry-session-id nil))

(defun hermes-chat--dashboard-create-owner-current-p
    (owner client generation session-id)
  "Return non-nil when OWNER controls CLIENT's SESSION-ID batch at GENERATION."
  (and (eq owner hermes-chat--create-override-owner)
       (hermes-chat--dashboard-context-current-p
        client generation session-id)))

(defun hermes-chat--create-override-submit-inhibit-reason ()
  "Return the submission guard while create-time overrides are being applied."
  (and hermes-chat--create-override-owner
       "Pre-session runtime configuration is in progress"))

(defun hermes-chat--clear-create-override-owner ()
  "Release any create-time override operation owned by this buffer."
  (setq hermes-chat--create-override-owner nil))

(defun hermes-chat--dashboard-handle-create-model-result
    (promise owner buffer client value session-id generation confirmed result)
  "Settle OWNER's create-model PROMISE from RESULT under exact identity."
  (hermes-chat--in-buffer buffer
    (cond
     ((not (hermes-chat--dashboard-create-owner-current-p
            owner client generation session-id))
      (hermes--promise-reject promise "Model switch was superseded"))
     ((hermes-transport--get result 'confirm_required)
      (if confirmed
          (hermes--promise-reject promise "Model switch still requires confirmation")
        (condition-case nil
            (if (yes-or-no-p
                 (or (hermes-transport--scalar-string
                      (hermes-transport--get result 'confirm_message))
                     "Confirm switching to this model? "))
                (hermes-chat--dashboard-request-create-model
                 promise owner buffer client value session-id generation t)
              (hermes--promise-reject promise "Model switch cancelled"))
          (quit (hermes--promise-reject promise "Model switch cancelled")))))
     (t (hermes--promise-resolve promise result)))))

(defun hermes-chat--dashboard-request-create-model
    (promise owner buffer client value session-id generation &optional confirmed)
  "Set model VALUE through CLIENT and settle OWNER's PROMISE."
  (condition-case err
      (if (not (hermes-chat--dashboard-create-owner-current-p
                owner client generation session-id))
          (hermes--promise-reject promise "Model switch was superseded")
        (hermes-dashboard-transport-config-set
         client "model" value :session-id session-id
         :confirm-expensive-model confirmed
         :resolve
         (lambda (result)
           (hermes-chat--dashboard-handle-create-model-result
            promise owner buffer client value session-id generation confirmed result))
         :reject (lambda (message) (hermes--promise-reject promise message))))
    ((error quit)
     (hermes--promise-reject promise (error-message-string err)))))

(defun hermes-chat--dashboard-create-config-promise
    (owner client cell session-id generation)
  "Return OWNER's promise applying CELL to CLIENT's SESSION-ID at GENERATION."
  (if (equal (car cell) "model")
      (let ((promise (hermes--promise-make)))
        (hermes-chat--dashboard-request-create-model
         promise owner (current-buffer) client (cdr cell) session-id generation)
        promise)
    (hermes-dashboard-transport-call-fn
     #'hermes-dashboard-transport-config-set
     client (car cell) (cdr cell) :session-id session-id)))

(defun hermes-chat--dashboard-fail-create-overrides
    (owner session-id message abort)
  "Release OWNER, preserve SESSION-ID overrides, and report MESSAGE via ABORT."
  (when (eq owner hermes-chat--create-override-owner)
    (setq hermes-chat--create-override-owner nil
          hermes-chat--create-overrides-retry-session-id session-id)
    (let ((message (format "Pre-session override failed: %s" message)))
      (if abort (funcall abort message)
        (hermes-chat--command-error message)))))

(defun hermes-chat--dashboard-step-create-overrides
    (owner buffer client cells session-id generation continue abort)
  "Apply CELLS to CLIENT's SESSION-ID for BUFFER at GENERATION under OWNER.
Call CONTINUE on success or ABORT on failure."
  (hermes-chat--in-buffer buffer
    (cond
     ((not (eq owner hermes-chat--create-override-owner)) nil)
     ((not (hermes-chat--dashboard-context-current-p
            client generation session-id))
      (hermes-chat--dashboard-fail-create-overrides
       owner session-id "session changed before it settled" abort))
     ((null cells)
      (hermes-chat--dashboard-clear-create-overrides)
      (setq hermes-chat--create-override-owner nil)
      (funcall continue))
     (t
      (condition-case err
          (hermes--promise-then
           (hermes-chat--dashboard-create-config-promise
            owner client (car cells) session-id generation)
           (lambda (_result)
             (hermes-chat--dashboard-step-create-overrides
              owner buffer client (cdr cells) session-id generation continue abort))
           (lambda (message)
             (hermes-chat--in-buffer buffer
               (hermes-chat--dashboard-fail-create-overrides
                owner session-id message abort))))
        ((error quit)
         (hermes-chat--dashboard-fail-create-overrides
          owner session-id (error-message-string err) abort)))))))

(defun hermes-chat--dashboard-apply-create-overrides
    (client continue &optional generation abort)
  "Apply pending create-time overrides to CLIENT in order, then CONTINUE.
GENERATION and an exact local owner reject stale results.  ABORT receives a
failure; overrides remain pending until the whole batch succeeds."
  (let ((cells (hermes-chat--dashboard-create-config-cells)))
    (if (null cells)
        (funcall continue)
      (let ((owner (gensym "hermes-create-overrides-")))
        (setq hermes-chat--create-override-owner owner)
        (hermes-chat--dashboard-step-create-overrides
         owner (current-buffer) client cells
         hermes-chat--dashboard-active-session-id generation continue abort)))))

(defun hermes-chat--dashboard-apply-retry-overrides
    (client continue generation abort)
  "Apply CLIENT overrides at GENERATION to their retry session, else CONTINUE.
ABORT receives a failure."
  (if (equal hermes-chat--create-overrides-retry-session-id
             hermes-chat--dashboard-active-session-id)
      (hermes-chat--dashboard-apply-create-overrides
       client continue generation abort)
    (hermes-chat--dashboard-clear-create-overrides)
    (funcall continue)))

(defun hermes-chat--dashboard-ensure-session
    (client prompt buffer &optional resolve reject queued-p)
  "Create or resume CLIENT's dashboard session before submitting PROMPT.
Record asynchronous session results in BUFFER.  RESOLVE and REJECT receive the
prompt request result or a session bootstrap error.  QUEUED-P identifies a
local FIFO submission."
  (cond
   ((hermes-chat--dashboard-session-attached-p)
    (hermes-chat--dashboard-apply-retry-overrides
     client
     (lambda ()
       (hermes-chat--dashboard-submit-prompt client prompt resolve reject))
     hermes-chat--lifecycle-generation reject))
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
     :title (hermes-chat--dashboard-create-title)
     :profile hermes-chat--profile
     :cwd (hermes-chat--current-working-directory)
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
    (hermes-chat--dashboard-claim-submit-context client)
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

(defun hermes-chat--apply-directory (directory)
  "Apply gateway-native DIRECTORY to this chat's remote and local context."
  (setq-local default-directory (file-name-as-directory directory))
  (hermes-chat--record-working-directory directory)
  (hermes-chat--insert-local-status
   (format "Working directory: %s" directory)
   'ready))

(defun hermes-chat--set-live-directory
    (client directory &optional generation session-id)
  "Set CLIENT's live session working directory to DIRECTORY.
GENERATION and SESSION-ID, when non-nil, retain an interactive request's owner."
  (let ((buffer (current-buffer))
        (generation (or generation hermes-chat--lifecycle-generation))
        (session-id (or session-id hermes-chat--dashboard-active-session-id)))
    (when (hermes-chat--dashboard-context-current-p client generation session-id)
      (hermes-dashboard-transport-session-cwd-set
       client directory
       :session-id session-id
       :resolve
       (lambda (result)
         (hermes-chat--in-buffer buffer
           (when (hermes-chat--dashboard-context-current-p
                  client generation session-id)
             (hermes-chat--apply-directory
              (or (hermes-chat--dashboard-result-cwd result) directory)))))
       :reject (hermes-chat--dashboard-action-rejecter
                buffer client generation nil session-id)))))

(defun hermes-chat--directory-parent (directory)
  "Return the lexical parent of gateway-native DIRECTORY."
  (cond
   ((string-match-p "\\`[/\\\\]+\\'" directory) directory)
   ((string-match-p "\\`[[:alpha:]]:[/\\\\]*\\'" directory) directory)
   (t
    (let* ((trimmed (replace-regexp-in-string "[/\\\\]+\\'" "" directory))
           (separator (string-match "[/\\\\][^/\\\\]*\\'" trimmed)))
      (cond
       ((null separator) directory)
       ((zerop separator) (substring trimmed 0 1))
       ((and (= separator 2)
             (string-match-p "\\`[[:alpha:]]:" trimmed))
        (substring trimmed 0 3))
       (t (substring trimmed 0 separator)))))))

(defun hermes-chat--directory-entry-candidate (entry)
  "Return a browse completion candidate for directory ENTRY, or nil."
  (when (eq (hermes-transport--get entry 'isDirectory) t)
    (when-let* ((name (hermes-transport--field entry 'name))
                (path (hermes-transport--field entry 'path)))
      (cons (format "%s/" name) (list :action 'browse :path path)))))

(defun hermes-chat--directory-candidates (directory result)
  "Return completion candidates for gateway DIRECTORY from API RESULT."
  (let* ((parent (hermes-chat--directory-parent directory))
         (entries (hermes-transport--get result 'entries))
         (children (delq nil
                         (mapcar #'hermes-chat--directory-entry-candidate
                                 (append entries nil)))))
    (append
     (list (cons (format "[Use current] %s" directory)
                 (list :action 'select :path directory)))
     (unless (equal parent directory)
       (list (cons (format "../  %s" parent)
                   (list :action 'browse :path parent))))
     children
     (list (cons "[Enter path manually]" (list :action 'manual))))))

(defun hermes-chat--read-instance-directory
    (client generation session-id &optional reason)
  "Read a gateway path manually and set it through CLIENT.
GENERATION and SESSION-ID identify the owning interaction.
REASON, when non-nil, explains why remote completion was unavailable."
  (when reason
    (message "%s" reason))
  (let ((directory
         (read-string "Hermes instance directory: "
                      (hermes-chat--current-working-directory))))
    (when (and (not (string-empty-p directory))
               (hermes-chat--dashboard-context-current-p
                client generation session-id))
      (hermes-chat--set-live-directory
       client directory generation session-id))))

(defun hermes-chat--choose-directory-candidate
    (client generation session-id directory result)
  "Choose CLIENT's next action at DIRECTORY using API RESULT.
GENERATION and SESSION-ID identify the owning interaction."
  (let* ((candidates (hermes-chat--directory-candidates directory result))
         (label (completing-read "Hermes instance directory: "
                                 candidates nil t nil nil (caar candidates)))
         (choice (cdr (assoc label candidates))))
    (when (hermes-chat--dashboard-context-current-p
           client generation session-id)
      (pcase (plist-get choice :action)
        ('select (hermes-chat--set-live-directory
                  client (plist-get choice :path) generation session-id))
        ('browse (hermes-chat--browse-instance-directory
                  client (plist-get choice :path) generation session-id))
        ('manual (hermes-chat--read-instance-directory
                  client generation session-id))))))

(defun hermes-chat--browse-instance-directory
    (client directory &optional generation session-id)
  "Asynchronously browse gateway DIRECTORY through CLIENT.
GENERATION and SESSION-ID, when non-nil, retain an interactive request's owner."
  (let ((buffer (current-buffer))
        (generation (or generation hermes-chat--lifecycle-generation))
        (session-id (or session-id hermes-chat--dashboard-active-session-id)))
    (when (hermes-chat--dashboard-context-current-p client generation session-id)
      (hermes--promise-then
       (hermes-dashboard-transport-api-request-async
        "GET" "/api/fs/list" :query (list (cons 'path directory)) :client client)
       (lambda (result)
         (hermes-chat--in-buffer buffer
           (when (hermes-chat--dashboard-context-current-p
                  client generation session-id)
             (condition-case nil
                 (if-let* ((error-message (hermes-transport--field result 'error)))
                     (hermes-chat--read-instance-directory
                      client generation session-id error-message)
                   (hermes-chat--choose-directory-candidate
                    client generation session-id directory result))
               (quit nil)))))
       (lambda (error-message)
         (hermes-chat--in-buffer buffer
           (when (hermes-chat--dashboard-context-current-p
                  client generation session-id)
             (condition-case nil
                 (hermes-chat--read-instance-directory
                  client generation session-id
                  (format "Remote directory listing failed: %s" error-message))
               (quit nil)))))))))

;;;###autoload
(defun hermes-chat-set-directory (&optional directory)
  "Select or set this Hermes chat's gateway working DIRECTORY.
Interactively, browse directories reported by the owning Hermes instance.
With explicit DIRECTORY, pass its gateway-native spelling to the backend."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (when (hermes-chat--active-turn-p)
    (user-error "Interrupt the active turn before changing directory"))
  (when (and directory
             (or (not (stringp directory)) (string-empty-p directory)))
    (user-error "Working directory must be a non-empty path"))
  (hermes-chat--with-dashboard-session
   nil (current-buffer)
   (lambda (client)
     (if directory
         (hermes-chat--set-live-directory client directory)
       (hermes-chat--browse-instance-directory
        client (hermes-chat--current-working-directory))))))

(defun hermes-chat--dashboard-action-resolver
    (buffer client action generation &optional create-p reject resume-p)
  "Return a resolver to record CLIENT's session in BUFFER, then call ACTION.
With CREATE-P non-nil the resolver handles a fresh `session.create' result,
so pending create-time runtime overrides are applied before ACTION.  RESUME-P
applies only retry-owned overrides and otherwise discards them.  REJECT receives
an override failure."
  (lambda (result)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--dashboard-context-current-p client generation)
        (hermes-chat--dashboard-record-session client result)
        (when (hermes-chat--dashboard-result-live-turn-p result)
          (hermes-chat--dashboard-restore-inflight-turn client)
          (hermes-chat--dashboard-bind-stream-callback
           client hermes-chat--pending-assistant-id))
        (cond
         (create-p
          (hermes-chat--dashboard-apply-create-overrides
           client (lambda () (funcall action client)) generation reject))
         (resume-p
          (hermes-chat--dashboard-apply-retry-overrides
           client (lambda () (funcall action client)) generation reject))
         (t
          (funcall action client)))))))

(defun hermes-chat--dashboard-create-title ()
  "Return the canonical title for a fresh dashboard session."
  (or hermes-chat--title
      (hermes-session-title-canonicalize
       (hermes-session-title-project-label
        hermes-chat-dashboard-session-title))))

(defun hermes-chat--dashboard-action-rejecter
    (buffer client generation reject &optional session-id)
  "Return BUFFER callback for REJECT scoped to CLIENT and GENERATION.
When SESSION-ID is non-nil, reject only while it still owns the buffer."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--dashboard-context-current-p
             client generation session-id)
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
      (hermes-chat--dashboard-apply-retry-overrides
       client (lambda () (funcall action client)) generation reject))
     (hermes-chat--session-id
      (hermes-dashboard-transport-session-resume
       client hermes-chat--session-id
       :cols (hermes-chat--dashboard-cols)
       :profile hermes-chat--profile
       :resolve (hermes-chat--dashboard-action-resolver
                 buffer client action generation nil reject t)
       :reject (hermes-chat--dashboard-action-rejecter
                buffer client generation reject)))
     (t
      (hermes-dashboard-transport-session-create
       client
       :cols (hermes-chat--dashboard-cols)
       :title (hermes-chat--dashboard-create-title)
       :cwd (hermes-chat--current-working-directory)
       :resolve (hermes-chat--dashboard-action-resolver
                 buffer client action generation t reject)
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
  "Render a ready socket while leaving the durable session lazy."
  (hermes-chat--insert-local-status "Dashboard socket reconnected" 'ready)
  (hermes-chat--set-header-state
   :status 'ready :activity "Socket ready; session resumes on next action"))

;;; Session title and background tasks

;; Dashboard-session behavior: server titles and /btw background
;; tasks live with the session lifecycle that drives them.

(defun hermes-chat--buffer-name (profile &optional instance directory)
  "Return a project-specific chat name for PROFILE, INSTANCE, and DIRECTORY.
PROFILE nil means the default profile.  An explicitly configured, valid named
INSTANCE identifies the owner; otherwise use the Hermes brand.  DIRECTORY
defaults to this chat's gateway working directory."
  (let* ((profile (or profile "default"))
         (instance (or instance (hermes-instance-context)))
         (owner (if (and hermes-instances
                         (hermes-instance--valid-p instance))
                    (hermes-instance-name instance)
                  "Hermes"))
         (project (hermes-chat--directory-basename directory)))
    (format "*%s@%s: [%s]*" owner profile project)))

(defun hermes-chat--refresh-buffer-name ()
  "Rename the current chat buffer from its live identity and working directory."
  (let ((name (hermes-chat--buffer-name
               hermes-chat--profile hermes-instance)))
    (unless (equal name (buffer-name))
      (rename-buffer name t))))

(defun hermes-chat--push-session-title (title)
  "Push TITLE to the server with `session.title' when a session is attached.
With no live session, keep the title as local session metadata."
  (if (and (hermes-chat--dashboard-session-attached-p)
           hermes-chat--dashboard-active-session-id)
      (let ((buffer (current-buffer))
            (lifetime hermes-chat--lifecycle-generation))
        (hermes-dashboard-transport-session-title
         hermes-chat--dashboard-client
         :session-id hermes-chat--dashboard-active-session-id
         :title title
         :resolve (lambda (result)
                    (hermes-chat--in-lifetime buffer lifetime
                      (when (eq (hermes-transport--get result 'pending) t)
                        (message "Title queued; applies once the session is saved"))))
         :reject (lambda (message)
                   (hermes-chat--in-lifetime buffer lifetime
                     (hermes-chat--command-error message)))))
    (message "Session title saved locally; no live session to update")))

(defun hermes-chat--apply-session-title (title)
  "Record TITLE without updating the server or project-specific buffer name."
  (setq hermes-chat--title title)
  (force-mode-line-update))

(defun hermes-chat--should-apply-title-p (title current manual-p)
  "Return non-nil when TITLE should replace CURRENT session metadata.
TITLE applies only when it is a non-empty string, differs from CURRENT, and
MANUAL-P is nil (the user has not pinned a title)."
  (and (not manual-p)
       (stringp title)
       (not (string-empty-p title))
       (not (equal title current))))

(defun hermes-chat--apply-fetched-title (buffer lifetime result)
  "Apply RESULT's title when BUFFER still owns LIFETIME."
  (hermes-chat--in-lifetime buffer lifetime
    (let ((title (string-trim
                  (or (hermes-transport--scalar-string
                       (hermes-transport--get result 'title))
                      ""))))
      (when (hermes-chat--should-apply-title-p
             title hermes-chat--title hermes-chat--title-manual-p)
        (hermes-chat--apply-session-title title)))))

(defun hermes-chat--fetch-session-title (buffer lifetime)
  "Fetch BUFFER's title while it still owns LIFETIME.
Guards are re-checked here since this runs after the turn settles."
  (hermes-chat--in-lifetime buffer lifetime
    (when (and (hermes-chat--dashboard-session-attached-p)
               (not hermes-chat--title-manual-p))
      (hermes-dashboard-transport-session-title-fetch
       hermes-chat--dashboard-client
       :session-id hermes-chat--dashboard-active-session-id
       :resolve (lambda (result)
                  (hermes-chat--apply-fetched-title buffer lifetime result))
       ;; A background title fetch must never surface as a chat error; swallow
       ;; failures rather than letting them reach the transport callback.
       :reject #'ignore))))

(defun hermes-chat--maybe-refresh-session-title ()
  "Schedule a server session-title refresh for this buffer after a turn settles.
Deferred to the next idle moment so no network I/O runs inside the transport
event handler.  A no-op without a live dashboard session or with a manual title."
  (when (and (hermes-chat--dashboard-session-attached-p)
             (not hermes-chat--title-manual-p))
    (run-at-time 0 nil #'hermes-chat--fetch-session-title
                 (current-buffer) hermes-chat--lifecycle-generation)))

(defun hermes-chat-rename (title)
  "Rename this chat session to TITLE.
When a live dashboard session is attached, update its server title via
`session.title'.  A manual title is kept against automatic title refreshes and
does not alter the project-specific buffer name."
  (interactive
   (list (read-string
          "Hermes chat title: "
          (or (and hermes-chat--title
                   (hermes-session-title-chat-display hermes-chat--title))
              ""))))
  (let ((title (string-trim title)))
    (when (string-empty-p title)
      (user-error "Title must not be empty"))
    (let ((canonical-title
           (hermes-session-title-canonicalize title hermes-chat--title)))
      (setq hermes-chat--title-manual-p t)
      (hermes-chat--apply-session-title canonical-title)
      (hermes-chat--push-session-title canonical-title))))

(defun hermes-chat-background (&optional prompt)
  "Run PROMPT as a Hermes background task, delivering its result to this chat.
With no PROMPT, use the input tail.  The task runs in its own session via
`prompt.background', so it does not block the current turn; its answer returns
later as a `background.complete' event rendered as a persistent [View Result]
entry."
  (interactive)
  (hermes-chat--ensure-submit-allowed)
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
    (hermes-chat--ensure-idle-listener hermes-chat--dashboard-client buffer)
    ;; Insert above any pending reply so the active turn's answer stays last.
    (hermes-chat--insert-entry
     (hermes-chat--make-entry
      'status (format "Background #%d started: %s" number preview) 'running)
     (hermes-chat--pending-assistant-node))))

(defun hermes-chat--background-submit (content buffer)
  "Launch CONTENT as a background task for BUFFER's dashboard session."
  (let ((lifetime hermes-chat--lifecycle-generation))
    (hermes-chat--with-dashboard-session
     content buffer
     (lambda (live-client)
       (hermes-dashboard-transport-prompt-background
        live-client content
        :session-id hermes-chat--dashboard-active-session-id
        :resolve (lambda (result)
                   (hermes-chat--in-lifetime buffer lifetime
                     (hermes-chat--background-started result content buffer)))
        :reject (lambda (message)
                  (hermes-chat--in-lifetime buffer lifetime
                    (hermes-chat--command-error message)
                    (hermes-chat--preserve-control-content content))))))))

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

(hermes-chat-register-submit-inhibit-function
 #'hermes-chat--create-override-submit-inhibit-reason)
(hermes-chat-register-cleanup-function #'hermes-chat--clear-create-override-owner)

(provide 'hermes-chat-dashboard)
;;; hermes-chat-dashboard.el ends here
