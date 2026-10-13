;;; hermes-chat.el --- EWOC chat buffer for Hermes  -*- lexical-binding: t; -*-

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

;; The chat facade of hermes-el: the pure `hermes-chat--turn-reduce'
;; reducer with its effect interpreter, the transport event handling, the
;; user-facing commands and keymaps, and the load-time population of the
;; sibling registries (submit pipeline, turn-event routing, native slash
;; commands).  The ERC/emacs-jabber-shaped buffer itself -- EWOC transcript
;; before a writable input tail -- lives in `hermes-chat-buffer', with
;; formatting in `hermes-chat-format' and rendering in `hermes-chat-render'.

;;; Code:

(require 'hermes-buffer)
(require 'cl-lib)
(require 'button)
(require 'diff-mode)
(require 'ewoc)
(require 'goto-addr)
(require 'keymap-popup)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-format)

(defcustom hermes-chat-buffer-name "*Hermes Chat*"
  "Name used while constructing a fresh Hermes chat buffer."
  :type 'string
  :group 'hermes)

(defcustom hermes-chat-buffer-name-function #'hermes-chat-default-buffer-name
  "Function used to produce a Hermes chat buffer name.
The function receives PROFILE, INSTANCE, and DIRECTORY.  PROFILE is a non-empty
profile name.  INSTANCE is the owning legacy pair or typed identity; use
`hermes-instance-id', `hermes-instance-name', and `hermes-instance-url' to read
it.  DIRECTORY names the chat's project and is selected in order from an
explicit caller argument, the launch-project root, the first gateway working
directory recorded for the session, or, until one is recorded, the current
gateway or editor `default-directory'.  It does not follow later gateway
directory changes, which the header line shows instead.  DIRECTORY is
gateway-native and need not exist on Emacs's host; the default function names
it with `project-name' only when it exists locally.  The function must return
the complete non-empty buffer name."
  :type 'function
  :group 'hermes)

(defcustom hermes-chat-use-dashboard-transport t
  "Whether chat sends use the dashboard transport by default.
When non-nil, `hermes-chat-send' uses the dashboard/TUI WebSocket path while
`hermes-transport-send-function' remains at its default CLI fallback function.
Rebinding `hermes-transport-send-function' still overrides the chat transport,
which keeps tests and user custom transports working."
  :type 'boolean
  :group 'hermes)

(defcustom hermes-chat-dashboard-session-title "Hermes Chat"
  "Fallback label for a dashboard chat created outside an Emacs project.
Inside a project, its root basename becomes the canonical session label."
  :type 'string
  :group 'hermes)

(defface hermes-chat-user-input
  '((t :inherit highlight))
  "Face for submitted user turns in the chat transcript."
  :group 'hermes)

(defface hermes-chat-separator
  '((((background light)) :strike-through "gray70" :foreground "gray70")
    (t :strike-through "gray30" :foreground "gray30"))
  "Face for the full-width rule above the Hermes chat input area."
  :group 'hermes)

;; Buffer/EWOC state owned by `hermes-chat-buffer'; re-declared here for the
;; byte-compiler.  See that file for the authoritative defvar-locals and docs.
(defvar hermes-chat--ewoc)
(defvar hermes-chat--input-marker)
(defvar hermes-chat--nodes)

;; Connection state owned by `hermes-chat-buffer'; re-declared here for the
;; byte-compiler.  See that file for the authoritative defvar-locals and docs.
(defvar hermes-chat--process)
(defvar hermes-chat--status-state)
(defvar hermes-chat--model)
(defvar hermes-chat--agent-name)
(defvar hermes-chat--context)
(defvar hermes-chat--goal)
(defvar hermes-chat--runtime-flags)
(defvar hermes-chat--profile)
(defvar hermes-chat--launch-project-root)
(defvar hermes-chat--working-directory)
(defvar hermes-chat--resolved-start-mode)
(defvar hermes-chat--active-tools)
(defvar hermes-chat--project-chat-root nil
  "Dynamically bound project root for a newly launched project chat.")
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-session-ready-p)
(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-running-p)
(defvar hermes-chat--session-id)

;; Owned by `hermes-chat-buffer'; declared here for the byte-compiler.
(defvar hermes-chat--pending-assistant-id)

;; Owned by `hermes-chat-buffer'; declared here for the byte-compiler.
(defvar hermes-chat--transport-generation)

(defvar hermes-chat--dashboard-detached-assistant-id)
(defvar hermes-chat--dashboard-stream-assistant-id)
(defvar hermes-chat--dashboard-interim-assistant-id)
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

;; Queue and stream state owned by `hermes-chat-buffer'; re-declared here for
;; the byte-compiler.
(defvar hermes-chat--queued-messages)
(defvar hermes-chat--queued-submit-id)

;; Assemble the sibling areas before any callers, including reducer helpers
;; that reuse their pure projections.  The reducer below remains separate
;; from effect dispatch.  Upward wiring uses callbacks, never declarations.
(require 'hermes-chat-buffer)
(require 'hermes-chat-draft)
(require 'hermes-chat-prompts)
(require 'hermes-chat-images)
(require 'hermes-chat-attachments)
(require 'hermes-chat-todos)
(require 'hermes-chat-dashboard)
(require 'hermes-chat-cards)
(require 'hermes-chat-models)
(require 'hermes-chat-handoff)
(require 'hermes-chat-slash)
(require 'hermes-chat-failure)

(defconst hermes-chat--transient-entry-roles '(status progress tool)
  "Entry roles used for compact transport status/progress lines.")

(defun hermes-chat--header-tool-key (event)
  "Return stable header key for EVENT's tool-like activity."
  (or (hermes-chat--transport-entry-id event)
      (hermes-chat--tool-name event)
      (hermes-chat--event-string event '(:event :seq :index))))

(defun hermes-chat--header-tool-summary (event)
  "Return compact header summary for tool-like EVENT."
  (hermes-transport--non-empty-string
   (pcase (plist-get event :type)
     ('progress (hermes-chat--format-progress-event event))
     ('tool (hermes-chat--format-tool-event event))
     (_ nil))))

(defun hermes-chat--capture-session-identity (event)
  "Record the model, agent name, flags, and context usage carried by EVENT."
  (when-let* ((model (plist-get event :model)))
    (setq hermes-chat--model model))
  (when-let* ((agent (plist-get event :agent-name)))
    (setq hermes-chat--agent-name agent))
  (dolist (key '(:reasoning-effort :service-tier :fast :yolo))
    (when-let* ((tail (plist-member event key)))
      (setq hermes-chat--runtime-flags
            (plist-put hermes-chat--runtime-flags key (cadr tail)))))
  (when-let* ((context (plist-get event :context)))
    (setq hermes-chat--context context)))

(defun hermes-chat--status-event-activity (event)
  "Return the header activity for a status EVENT.
`session.info' carries the model/provider, now shown in their own header
fields, so it collapses to a plain ready state instead of repeating them."
  (if (hermes-chat--session-info-event-p event)
      (if (plist-get event :running) "Working" "Ready")
    (or (hermes-chat--header-activity-for-event event) "Working")))

;;; Turn-state reducer
;;
;; The header-affecting state of a turn -- the status line and the active-tool
;; set -- is computed by the pure `hermes-chat--turn-reduce'.  The reducer takes
;; the wall-clock NOW as data so it can stamp the status line itself, and returns
;; (NEW-STATE . EFFECTS) where each effect is a uniform (TYPE . PAYLOAD) tool
;; delta.  The boundary only persists NEW-STATE and replays the deltas; it makes
;; no decisions of its own.

(defun hermes-chat--turn-state (&rest kvs)
  "Return a turn-state plist built from KVS."
  kvs)

(defun hermes-chat--turn-state-get (state key)
  "Return KEY from turn-state STATE."
  (plist-get state key))

(defun hermes-chat--turn-state-put (state key value)
  "Return a copy of turn-state STATE with KEY set to VALUE."
  (plist-put (copy-sequence state) key value))

(defun hermes-chat--status-header-props (event)
  "Return the (:status :activity) header props for a status EVENT."
  (list :status (if (plist-get event :prompt-request-p)
                    (if (equal (hermes-chat--prompt-event-type event) "approval")
                        'approval-requested
                      'requested)
                  (hermes-chat--transport-entry-status event))
        :activity (hermes-chat--status-event-activity event)))

(defun hermes-chat--thinking-header-props (event)
  "Return header props for a `thinking.delta' EVENT.
Provider notices share this channel with spinner updates; neither proves
reasoning.  Keep the activity neutral and use empty content only to clear it."
  (list :status 'running
        :activity (hermes-chat--thinking-activity (plist-get event :content))))

(defun hermes-chat--interrupted-status-p (status)
  "Return non-nil when STATUS denotes an interrupted turn."
  (member (hermes-chat--status-name status)
          '("interrupted" "cancelled" "canceled")))

(defun hermes-chat--turn-header-props (event)
  "Return header props for any header-affecting EVENT, or nil for none."
  (pcase (plist-get event :type)
    ('status (hermes-chat--status-header-props event))
    ('commentary '(:status running :activity "Reasoning"))
    ('thinking (hermes-chat--thinking-header-props event))
    ('diff '(:status running :activity "Reviewing diff"))
    ('done (list :status 'ready :activity "Ready"
                 :usage (plist-get event :usage)))
    ('error
     (let ((status (hermes-chat--error-status event)))
       (list :status status
             :activity (if (hermes-chat--interrupted-status-p status)
                           "Interrupted"
                         (or (and-let* ((failure (plist-get event :failure)))
                               (hermes-chat-failure-title failure))
                             (hermes-chat--event-string
                              event '(:content :error))
                             "Transport error")))))
    ('unknown (list :status 'error
                    :activity (hermes-chat--unknown-event-content event)))))

(defun hermes-chat--turn-tool-effect (event)
  "Return a (TYPE . PAYLOAD) active-tool delta for tool-like EVENT, or nil.
`tool-put' carries (KEY . SUMMARY) and `tool-remove' carries KEY.  Pure."
  (and-let* ((summary (hermes-chat--header-tool-summary event)))
    (let ((key (or (hermes-chat--header-tool-key event) summary)))
      (if (hermes-chat--finished-status-p
           (hermes-chat--transport-entry-status event))
          (cons 'tool-remove key)
        (cons 'tool-put (cons key summary))))))

(defun hermes-chat--turn-status-state (state event now)
  "Return the merged :status-state for header EVENT at NOW, given turn-state STATE."
  (apply #'hermes-chat--entry-with
         (hermes-chat--turn-state-get state :status-state)
         (append (hermes-chat--turn-header-props event)
                 (list :updated now))))

(defun hermes-chat--compress-bar-clear-event-p (event)
  "Return non-nil when EVENT is the gateway's post-compress ready bar clear."
  (and (eq (plist-get event :type) 'status)
       (equal (hermes-chat--status-name (plist-get event :status)) "status")
       (equal (hermes-chat--event-string event '(:content :text)) "ready")))

(defun hermes-chat--transcript-event-p (event)
  "Return non-nil when EVENT should render a compact transcript entry.
`session.info' feeds the header only, and `notification.clear' retracts a
keyed notice without carrying text, so neither becomes an entry.  The
gateway's post-`session.compress' ready bar-clear is also not a transcript
line."
  (pcase (plist-get event :type)
    ('status (not (or (hermes-chat--session-info-event-p event)
                      (hermes-chat--compress-bar-clear-event-p event)
                      (equal (hermes-chat--event-string event '(:event))
                             "notification.clear"))))
    ((or 'progress 'tool 'commentary 'diff 'unknown) t)))

(defun hermes-chat--turn-entry-effect (event)
  "Return an (upsert-entry . EVENT) transcript effect for EVENT, or nil.  Pure."
  (and (hermes-chat--transcript-event-p event)
       (cons 'upsert-entry event)))

(defun hermes-chat--turn-session-info-effects (event)
  "Return dashboard-running effects carried by session-info EVENT."
  (when (and (hermes-chat--session-info-event-p event)
             (plist-member event :running))
    (let ((running (plist-get event :running)))
      (append (list (cons 'set-dashboard-running running))
              (unless running '((drain)))))))

(defun hermes-chat--turn-terminal-suffix (status)
  "Return the ordered terminal settlement effects for STATUS."
  (list (cons 'settle status) '(finish) '(clear-pending)
        '(set-dashboard-running) '(drain)))

(defun hermes-chat--turn-done-effects (event status)
  "Return the ordered effect list for a `done' EVENT with header STATUS.
`refresh-header' precedes the lifecycle so the header settles before `drain'
re-submits any queued turn."
  (append
   (delq nil
         (list '(clear-tools)
               (cons 'refresh-header status)
               (cons 'clear-prompts event)
               (cons (cond ((plist-get event :response-transformed)
                            'mark-transformed)
                           ((plist-get event :response-previewed)
                            'mark-previewed)
                           (t 'mark-done))
                     (plist-get event :content))
               (and-let* ((warning (plist-get event :warning)))
                 (cons 'warning warning))
               '(drop-thinking)))
   (hermes-chat--turn-terminal-suffix 'done)))

(defun hermes-chat--turn-suppressed-effects (event status)
  "Return the ordered effect list for a `suppressed-terminal' EVENT.
STATUS is the merged header state.  Mirrors `hermes-chat--turn-done-effects'
minus content copying: the turn was resumed in flight without a local
assistant entry, so the reply placeholder keeps its text."
  (append
   (list '(clear-tools)
         (cons 'refresh-header status)
         (cons 'clear-prompts (plist-get event :original))
         (cons 'mark-status (plist-get event :settle-status))
         '(drop-thinking))
   (hermes-chat--turn-terminal-suffix (plist-get event :settle-status))))

(defun hermes-chat--turn-error-effects (event status)
  "Return the ordered effect list for an `error' EVENT with header STATUS."
  (let ((estatus (hermes-chat--error-status event)))
    (append
     (if (hermes-chat--interrupted-status-p estatus)
         (list '(clear-tools)
               (cons 'refresh-header status)
               (cons 'clear-prompts event)
               (cons 'mark-status estatus))
       (let ((content (let ((value (or (plist-get event :content) "")))
                        (if (string-empty-p value) "Transport error" value))))
         (list '(clear-tools)
               (cons 'refresh-header status)
               (cons 'clear-prompts event)
               (if-let* ((failure (plist-get event :failure)))
                   (cons 'mark-failure (cons failure estatus))
                 (cons 'append-error (cons content estatus))))))
     (hermes-chat--turn-terminal-suffix estatus))))

(defun hermes-chat--turn-reduce-status (state event now)
  "Return (NEW-STATE . EFFECTS) for a status EVENT on STATE at time NOW.
Keep compression clearing, goal notices and session-info effects ordered."
  (cond
   ((hermes-chat--compress-bar-clear-event-p event)
    (let ((status (hermes-chat--entry-with
                   (hermes-chat--turn-state-get state :status-state)
                   :status 'ready
                   :activity "Ready"
                   :updated now)))
      (cons (hermes-chat--turn-state-put state :status-state status)
            (list (cons 'refresh-header status)))))
   ((equal (hermes-chat--status-name (plist-get event :status)) "goal")
    (cons state (delq nil (list (hermes-chat--turn-entry-effect event)))))
   (t
    (let* ((next-state
            (if (plist-member event :goal)
                (hermes-chat--turn-state-put state :goal (plist-get event :goal))
              state))
           (status (hermes-chat--turn-status-state next-state event now)))
      (cons (hermes-chat--turn-state-put next-state :status-state status)
            (append
             (delq nil (list (cons 'refresh-header status)
                             (hermes-chat--turn-entry-effect event)))
             (hermes-chat--turn-session-info-effects event)))))))

(defun hermes-chat--turn-reduce (state event now)
  "Return (NEW-STATE . EFFECTS) for domain EVENT applied to STATE at time NOW.
Pure: no buffer, EWOC, process, header, or message side effects.  EFFECTS is an
ordered list the boundary replays: a header change leads with `refresh-header',
`done'/`error' append the turn lifecycle, and tool/transcript events emit deltas
and `upsert-entry'.  Other types return (STATE)."
  (pcase (plist-get event :type)
    ('status (hermes-chat--turn-reduce-status state event now))
    ('goal
     (cons (hermes-chat--turn-state-put state :goal (plist-get event :goal))
           '((refresh-header))))
    ('thinking
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (list (cons 'refresh-header status)
                   (cons 'reasoning-row
                         (and (not (equal (plist-get event :event) "tool.generating"))
                              (not (string-empty-p (or (plist-get event :content) "")))))))))
    ((or 'commentary 'diff)
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (delq nil (list (cons 'refresh-header status)
                             (hermes-chat--turn-entry-effect event))))))
    ('unknown
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (list (cons 'refresh-header status)
                   (cons 'message (hermes-chat--unknown-event-content event))
                   (cons 'upsert-entry event)))))
    ('done
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (hermes-chat--turn-done-effects event status))))
    ('error
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (hermes-chat--turn-error-effects event status))))
    ('suppressed-terminal
     (let ((status (hermes-chat--turn-status-state
                    state (plist-get event :header) now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (hermes-chat--turn-suppressed-effects event status))))
    ((or 'progress 'tool)
     (cons state (delq nil (list '(reasoning-row) (hermes-chat--turn-tool-effect event)
                                 (hermes-chat--turn-entry-effect event)))))
    ('delta
     (cons state (list '(reasoning-row) (cons 'append-delta (or (plist-get event :content) "")))))
    ('interim
     (cons state (list (cons 'seal-interim
                             (or (plist-get event :content) "")))))
    (_ (cons state nil))))

(defun hermes-chat--rotate-assistant (assistant-id)
  "Rotate ASSISTANT-ID's presentation without starting another backend turn."
  (hermes-chat--reasoning-row assistant-id nil)
  (hermes-chat--mark-assistant assistant-id 'done nil t)
  (hermes-chat--continue-assistant assistant-id))

(defun hermes-chat--continue-assistant (assistant-id)
  "Create a streaming successor for the already settled ASSISTANT-ID."
  (let* ((entry (hermes-chat--make-entry 'assistant "" 'streaming))
         (next-id (plist-get entry :id)))
    (hermes-chat--insert-entry entry)
    (hermes-chat--images-rotate assistant-id next-id)
    (hermes-chat-todos--rotate assistant-id next-id)
    (hermes-chat--rotate-live-tools assistant-id next-id)
    (setq hermes-chat--pending-assistant-id next-id
          hermes-chat--dashboard-stream-assistant-id next-id)
    next-id))

(defun hermes-chat--seal-interim-assistant (assistant-id content)
  "Seal ASSISTANT-ID with interim CONTENT and rotate the live stream entry."
  (hermes-chat--clear-ansi-fragment
   (hermes-chat--assistant-ansi-key assistant-id))
  (let ((text (hermes-chat--sanitize-assistant-content
               (hermes-chat--assistant-segment-content assistant-id content) t)))
    (hermes-chat--update-entry
     assistant-id
     (lambda (entry)
       (hermes-chat--entry-with entry :status 'done :content text
                                :interim-content content))))
  (hermes-chat--reasoning-row assistant-id nil)
  (hermes-chat--continue-assistant assistant-id)
  ;; The sealed segment is finished, like a final reply: drop a reasoning
  ;; preview that only repeats it and settle rows still marked running.
  ;; Live tool rows already moved to the successor above.
  (hermes-chat--drop-duplicate-thinking assistant-id)
  (hermes-chat--settle-transport-entries assistant-id 'done)
  (setq hermes-chat--dashboard-interim-assistant-id assistant-id))

(defun hermes-chat--interim-precedes-p (interim-id assistant-id)
  "Return non-nil when INTERIM-ID is ASSISTANT-ID's sealed predecessor.
Walk back from ASSISTANT-ID and stop at the first user entry, so an
interim from before a steer or redirect never counts."
  (let ((node (gethash assistant-id hermes-chat--nodes))
        found)
    (while (and node (not found)
                (setq node (ewoc-prev hermes-chat--ewoc node)))
      (let ((entry (ewoc-data node)))
        (cond ((equal (plist-get entry :id) interim-id) (setq found t))
              ((eq (plist-get entry :role) 'user) (setq node nil)))))
    found))

(defun hermes-chat--mark-transformed-assistant
    (assistant-id content &optional status)
  "Settle transformed final CONTENT as ASSISTANT-ID's authoritative reply.
An output hook rewrote the final after streaming, so CONTENT replaces the
streamed text instead of extending it.  When nothing streamed since the
latest interim of this turn, CONTENT replaces that sealed interim.
STATUS defaults to `done'.  Return the ID of the entry now holding CONTENT."
  (let ((interim-id hermes-chat--dashboard-interim-assistant-id)
        (text (or content ""))
        (status (or status 'done)))
    (if (and interim-id
             (not (equal interim-id assistant-id))
             (string-empty-p
              (or (hermes-chat--entry-content-by-id assistant-id) ""))
             (hermes-chat--interim-precedes-p interim-id assistant-id))
        (progn
          (hermes-chat--remove-entry assistant-id)
          (hermes-chat--mark-assistant interim-id status text t)
          interim-id)
      (hermes-chat--mark-assistant assistant-id status text t)
      assistant-id)))

(defun hermes-chat--mark-previewed-assistant (assistant-id content)
  "Settle previewed CONTENT on its interim entry, or ASSISTANT-ID as fallback."
  (let* ((interim-id hermes-chat--dashboard-interim-assistant-id)
         (interim-node (and interim-id hermes-chat--nodes
                            (gethash interim-id hermes-chat--nodes)))
         (interim-entry (and interim-node (ewoc-data interim-node)))
         ;; Live reload retains interim entries created before this metadata.
         (interim-content (plist-get interim-entry
                                     (if (plist-member interim-entry :interim-content)
                                         :interim-content
                                       :content))))
    (if (and interim-content (equal interim-content content))
        (if (string-empty-p (or (hermes-chat--entry-content-by-id assistant-id) ""))
            (progn
              (hermes-chat--remove-entry assistant-id)
              (hermes-chat--mark-assistant interim-id 'done nil t))
          (hermes-chat--mark-assistant assistant-id 'done nil t))
      (hermes-chat--mark-assistant
       assistant-id 'done
       (hermes-chat--assistant-done-content assistant-id content) t))))

(defun hermes-chat--apply-turn-effect (assistant-id effect)
  "Apply one boundary EFFECT for ASSISTANT-ID.
Header and tool effects always apply; transcript, message, and turn-lifecycle
effects apply only when ASSISTANT-ID is non-nil, so a header-only reduction
stays side-effect-light."
  (pcase effect
    ;; The reduced :status-state is persisted by the boundary
    ;; (`hermes-chat--run-turn-reducer'); this effect only redisplays.
    (`(refresh-header . ,_status)
     (force-mode-line-update)
     (hermes-chat--notify-state-change))
    ('(clear-tools) (hermes-chat--clear-active-tools))
    (`(tool-put ,key . ,summary)
     (puthash key summary (hermes-chat--active-tools-table)))
    (`(tool-remove . ,key) (remhash key (hermes-chat--active-tools-table)))
    (`(set-dashboard-running . ,running)
     (setq hermes-chat--dashboard-running-p running))
    ('(drain) (hermes-chat--drain-queued-message))
    ((guard (null assistant-id)) nil)
    (`(reasoning-row . ,active) (hermes-chat--reasoning-row assistant-id active))
    (`(upsert-entry . ,event)
     (hermes-chat--upsert-transport-entry assistant-id event)
     (hermes-chat--cards-note-tool assistant-id event))
    (`(message . ,text) (message "%s" text))
    (`(warning . ,text)
     (hermes-chat--insert-local-status (format "warning: %s" text) 'done))
    (`(clear-prompts . ,event) (hermes-chat--clear-terminal-prompts event))
    (`(mark-done . ,content)
     (hermes-chat--mark-assistant
      assistant-id 'done
      (hermes-chat--assistant-done-content assistant-id content) t))
    (`(mark-transformed . ,content)
     (hermes-chat--mark-transformed-assistant assistant-id content))
    (`(mark-previewed . ,content)
     (hermes-chat--mark-previewed-assistant assistant-id content))
    (`(append-error ,content . ,status)
     (hermes-chat--append-assistant-content assistant-id content status))
    (`(mark-failure ,failure . ,status)
     (hermes-chat--mark-assistant-failure assistant-id failure status))
    (`(mark-status . ,status)
     (hermes-chat--mark-assistant assistant-id status nil t))
    ('(drop-thinking) (hermes-chat--drop-duplicate-thinking assistant-id))
    (`(settle . ,status)
     (hermes-chat--images-settle assistant-id status)
     (hermes-chat--settle-transport-entries assistant-id status))
    ('(finish) (hermes-chat--dashboard-finish-assistant assistant-id))
    ('(clear-pending)
     (setq hermes-chat--pending-assistant-id nil
           hermes-chat--dashboard-interim-assistant-id nil
           hermes-chat--process nil))
    (`(append-delta . ,content)
     (unless (hermes-chat--thinking-echo-delta-p assistant-id content)
       (hermes-chat--append-assistant-content assistant-id content 'streaming)))
    (`(seal-interim . ,content)
     (hermes-chat--seal-interim-assistant assistant-id content))))

(defun hermes-chat--run-turn-reducer (assistant-id event)
  "Reduce EVENT, persist the new turn state, and apply its effects in order.
ASSISTANT-ID scopes task and transcript effects; session identity is captured
before reducing the transcript.  The boundary persists NEW-STATE and replays
its effects; it makes no decisions
of its own."
  (hermes-chat-todos--accept assistant-id event)
  (hermes-chat--capture-session-identity event)
  (pcase-let ((`(,new-state . ,effects)
               (hermes-chat--turn-reduce
                (hermes-chat--turn-state
                 :status-state hermes-chat--status-state
                 :goal hermes-chat--goal)
                event (current-time))))
    (setq hermes-chat--status-state
          (hermes-chat--turn-state-get new-state :status-state)
          hermes-chat--goal
          (hermes-chat--turn-state-get new-state :goal))
    (dolist (effect effects)
      (hermes-chat--apply-turn-effect assistant-id effect))
    (when (and (eq (plist-get event :type) 'status)
               (equal (plist-get event :status) "goal"))
      (hermes-chat--dashboard-refresh-goal))))

(defun hermes-chat--busy-message ()
  "Return the user-facing busy/backpressure message."
  (concat "A Hermes reply is still pending; use C-c C-i to interrupt, "
          "C-c C-q to queue, C-c C-s to steer, C-c C-k to "
          "interrupt+send, "
          (and (hermes-chat--pending-prompt-p)
               "C-c C-a to answer the prompt, C-c C-d to cancel it, ")
          "or C-c C-n for a new chat"))

(defun hermes-chat--insert-backend-turn (content)
  "Insert CONTENT and its pending assistant; return their ids."
  (let ((user (hermes-chat--make-entry 'user content 'done))
        (assistant (hermes-chat--make-entry 'assistant "" 'pending)))
    (hermes-chat--insert-entry user)
    (hermes-chat--insert-entry assistant)
    (cons (plist-get user :id) (plist-get assistant :id))))

(defun hermes-chat--merge-server-queued-content (content)
  "Merge CONTENT into the backend-owned queued user entry."
  (hermes-chat--update-entry
   hermes-chat--server-queued-user-id
   (lambda (entry)
     (hermes-chat--entry-with
      entry :content
      (string-join (list (plist-get entry :content) content) "\n\n")))))

(defun hermes-chat--record-server-queued-content (content)
  "Record CONTENT as accepted by the backend's queued next turn."
  (if (and hermes-chat--server-queued-user-id
           hermes-chat--server-queued-assistant-id)
      (hermes-chat--merge-server-queued-content content)
    (pcase-let ((`(,user-id . ,assistant-id)
                 (hermes-chat--insert-backend-turn content)))
      (setq hermes-chat--server-queued-user-id user-id
            hermes-chat--server-queued-assistant-id assistant-id
            hermes-chat--server-queued-after-idle-count
            hermes-chat--dashboard-idle-count
            hermes-chat--server-queued-prior-terminal-p nil))))

(defun hermes-chat--activate-backend-turn (content)
  "Record CONTENT as a backend-started turn and make it current."
  (when-let* ((assistant-id hermes-chat--pending-assistant-id))
    (hermes-chat--mark-assistant assistant-id 'done nil t)
    (hermes-chat--settle-transport-entries assistant-id 'done)
    (hermes-chat--dashboard-finish-assistant assistant-id))
  (pcase-let ((`(,_user-id . ,assistant-id)
               (hermes-chat--insert-backend-turn content)))
    (hermes-chat--clear-active-tools)
    (hermes-chat-todos--begin assistant-id)
    (setq hermes-chat--pending-assistant-id assistant-id
          hermes-chat--dashboard-stream-assistant-id assistant-id
          hermes-chat--dashboard-running-p t
          hermes-chat--server-queued-assistant-id nil
          hermes-chat--server-queued-user-id nil
          hermes-chat--server-queued-after-idle-count nil
          hermes-chat--server-queued-prior-terminal-p nil
          hermes-chat--process hermes-chat--dashboard-client)
    (hermes-chat--set-header-state
     :status 'pending :activity "Waiting for Hermes"
     :assistant-id assistant-id)))

(defun hermes-chat--accept-redirected-content (context)
  "Insert CONTEXT's accepted input at the held stream boundary.
Events have been held since submission, so the original assistant still ends
at that boundary even when a terminal event preceded the receipt."
  (let* ((assistant-id (plist-get context :assistant-id))
         (node (and hermes-chat--nodes (gethash assistant-id hermes-chat--nodes)))
         (entry (and node (ewoc-data node))))
    (unless (string-empty-p (or (plist-get entry :content) ""))
      (let ((next-id (hermes-chat--rotate-assistant assistant-id))
            (prefix (concat (plist-get entry :stream-prefix) (plist-get entry :content))))
        (hermes-chat--update-entry
         next-id (lambda (next) (hermes-chat--entry-with next :stream-prefix prefix))))))
  (hermes-chat--insert-entry
   (hermes-chat--make-entry 'user (plist-get context :content) 'done)
   (hermes-chat--pending-assistant-node))
  (when-let* ((admission (plist-get context :admission)))
    (setf (plist-get admission :assistant-id) hermes-chat--pending-assistant-id)))

(defun hermes-chat--message-start-event-p (event)
  "Return non-nil when EVENT is an assistant message start."
  (hermes-chat--message-start-status-event-p event))

(defun hermes-chat--busy-submit-events (context)
  "Return CONTEXT's held dashboard events in arrival order."
  (nreverse (copy-sequence (plist-get context :events))))

(defun hermes-chat--replay-busy-submit-events (context events)
  "Replay held dashboard EVENTS through CONTEXT's original turn callback."
  (let ((callback
         (hermes-chat--transport-callback
          (current-buffer) (plist-get context :assistant-id) t
          (plist-get context :generation))))
    (dolist (event events)
      (funcall callback event))))

(defun hermes-chat--resolve-streaming-busy-submit (context content events)
  "Activate CONTEXT's streaming CONTENT and replay EVENTS at their boundary."
  (let* ((start (cl-position-if #'hermes-chat--message-start-event-p events))
         (before (if start (seq-take events start) events))
         (after (and start (nthcdr start events))))
    (hermes-chat--replay-busy-submit-events context before)
    (hermes-chat--activate-backend-turn content)
    (when-let* ((admission (plist-get context :admission)))
      (setf (plist-get admission :assistant-id) hermes-chat--pending-assistant-id))
    (hermes-chat--replay-busy-submit-events context after)))

(defun hermes-chat--settle-busy-submit (context result)
  "Settle busy submission CONTEXT from backend RESULT and replay held events."
  (let ((content (plist-get context :content))
        (events (hermes-chat--busy-submit-events context))
        (status (hermes-chat--status-name
                 (hermes-chat--result-string result 'status))))
    (hermes-chat--image-admission-ack (plist-get context :admission) result)
    (setq hermes-chat--busy-submit-context nil)
    (pcase status
      ("queued"
       (hermes-chat--record-server-queued-content content)
       (hermes-chat--replay-busy-submit-events context events))
      ((or "steered" "redirected")
       (hermes-chat--accept-redirected-content context)
       (hermes-chat--replay-busy-submit-events context events))
      (_ (hermes-chat--resolve-streaming-busy-submit context content events)))))

(defun hermes-chat--hold-busy-submit-event (event)
  "Hold EVENT while a busy submission awaits the backend policy result."
  (when (and hermes-chat--busy-submit-context
             (not (or (hermes-chat--closed-status-event-p event)
                      (hermes-chat--reconnecting-status-event-p event))))
    (push (copy-sequence event)
          (plist-get hermes-chat--busy-submit-context :events))
    t))

(defun hermes-chat--abandon-busy-submit ()
  "Restore a busy submission whose dashboard session was lost."
  (when-let* ((context hermes-chat--busy-submit-context))
    (hermes-chat--image-admission-finish (plist-get context :admission) t)
    (let ((events (hermes-chat--busy-submit-events context)))
      (setq hermes-chat--busy-submit-context nil)
      (hermes-chat--replay-busy-submit-events context events)
      (hermes-chat--preserve-control-content (plist-get context :content)))))

(defun hermes-chat--busy-submit-rejected (content message)
  "Report rejected busy CONTENT with MESSAGE and preserve the text."
  (hermes-chat--command-error message)
  (hermes-chat--preserve-control-content content))

(defun hermes-chat--fail-busy-submit (context message)
  "Reject current busy submission CONTEXT with MESSAGE."
  (when (eq context hermes-chat--busy-submit-context)
    (hermes-chat--image-admission-finish (plist-get context :admission) t)
    (let ((events (hermes-chat--busy-submit-events context)))
      (setq hermes-chat--busy-submit-context nil)
      (hermes-chat--replay-busy-submit-events context events)
      (hermes-chat--busy-submit-rejected
       (plist-get context :content) message))))

(defun hermes-chat--submit-busy-dashboard-content (content)
  "Submit busy CONTENT under the dashboard's configured policy.
Return non-nil when the transport request starts."
  (let* ((buffer (current-buffer))
         (generation hermes-chat--transport-generation)
         (session-id hermes-chat--dashboard-active-session-id)
         (assistant-id (or hermes-chat--dashboard-stream-assistant-id
                           hermes-chat--pending-assistant-id))
         (context
          (list :content content :generation generation :session-id session-id
                :assistant-id assistant-id :admission nil :events nil)))
    (setq hermes-chat--busy-submit-context context)
    (condition-case err
        (progn
          (hermes-chat--ensure-submit-allowed)
          (setf (plist-get context :admission) (hermes-chat--image-admission-start nil))
          (hermes-dashboard-transport-prompt-submit
           (hermes-chat--dashboard-control-client) content
           :session-id session-id
           :resolve (lambda (result)
                      (hermes-chat--in-buffer buffer
                        (when (and (hermes-chat--current-transport-generation-p
                                    generation)
                                   (eq context hermes-chat--busy-submit-context)
                                   (equal session-id
                                          hermes-chat--dashboard-active-session-id))
                          (hermes-chat--settle-busy-submit context result))))
           :reject (lambda (message)
                     (hermes-chat--in-buffer buffer
                       (when (and (hermes-chat--current-transport-generation-p
                                   generation)
                                  (eq context hermes-chat--busy-submit-context)
                                  (equal session-id
                                         hermes-chat--dashboard-active-session-id))
                         (hermes-chat--fail-busy-submit context message)))))
          t)
      (error
       (hermes-chat--fail-busy-submit context (error-message-string err))
       nil))))

(defun hermes-chat--trimmed-input ()
  "Return the current input tail trimmed for sending."
  (string-trim (hermes-chat-input-string)))

(defun hermes-chat-newline ()
  "Insert a literal newline in the Hermes chat input tail.
Outside the tail, move to the end of the draft first so the newline
extends the input instead of prepending a blank line to it."
  (interactive nil hermes-chat-mode)
  (unless (hermes-chat--point-in-input-p)
    (goto-char (point-max)))
  (insert "\n"))

(defun hermes-chat--busy-submit-steered (context)
  "Accept CONTEXT's optimistic turn as the backend's active steered turn."
  ;; A locally idle client may race another backend turn.  Its full user
  ;; entry is already in place, so the receipt must not replace it with a preview.
  (when (equal hermes-chat--pending-assistant-id
               (plist-get context :assistant-id))
    (hermes-chat--mark-assistant (plist-get context :assistant-id) 'streaming))
  (when-let* ((queue-id (plist-get context :queue-id)))
    (hermes-chat--queue-submit-accepted queue-id)))

(defun hermes-chat--prepare-server-queued-turn (context)
  "Prepare CONTEXT's assistant for a backend-owned queued turn."
  (let ((assistant-id (plist-get context :assistant-id)))
    (unless (equal assistant-id hermes-chat--prepared-submit-assistant-id)
      (hermes-chat--reset-submit-assistant assistant-id))
    (setq hermes-chat--pending-assistant-id assistant-id
          hermes-chat--process hermes-chat--dashboard-client
          hermes-chat--dashboard-stream-assistant-id assistant-id
          hermes-chat--dashboard-suppress-stream-p nil
          hermes-chat--dashboard-running-p t
          hermes-chat--server-queued-assistant-id assistant-id
          hermes-chat--server-queued-user-id (plist-get context :user-id)
          hermes-chat--server-queued-after-idle-count
          (plist-get context :idle-count)
          hermes-chat--server-queued-prior-terminal-p
          (plist-get context :prior-terminal-p))
    (hermes-chat--set-header-state
     :status 'pending :activity "Queued by Hermes"
     :assistant-id assistant-id)))

(defun hermes-chat--busy-submit-queued (context)
  "Transfer CONTEXT from the local FIFO to the backend-owned busy queue."
  (let ((current-p (hermes-chat--current-transport-generation-p
                    (plist-get context :generation)))
        (terminal-p (plist-get context :post-start-terminal-p)))
    (when (and current-p (not terminal-p))
      (hermes-chat--prepare-server-queued-turn context))
    (hermes-chat--insert-local-status "Queued by Hermes" 'done)
    (when-let* ((queue-id (plist-get context :queue-id)))
      (hermes-chat--queue-submit-accepted queue-id))
    (when (and current-p (not terminal-p)
               (equal (plist-get context :assistant-id)
                      hermes-chat--prepared-submit-assistant-id))
      (hermes-chat--dashboard-activate-server-queued-turn
       (plist-get context :assistant-id)))))

(defun hermes-chat--submit-resolved (context result)
  "Settle CONTEXT from dashboard prompt RESULT."
  (hermes-chat--image-admission-ack (plist-get context :admission) result)
  (hermes-chat--bind-row-id (plist-get context :user-id)
                            (hermes-transport--get result 'user_row_id)
                            (not (plist-get context :display)))
  (when (plist-get context :truncation)
    (hermes-chat--request-history-reconcile))
  (when-let* ((record (plist-get (plist-get context :queue-entry) :image-record)))
    (unless (eq (plist-get record :state) 'uncertain)
      (setf (plist-get record :state) 'accepted))
    ;; Only queued acknowledgment proves the backend took staging.  Idle
    ;; streaming acknowledgment precedes admission and must keep the lock.
    (when (equal (hermes-chat--result-string result 'status) "queued")
      (hermes-chat--images-release record)))
  (pcase (hermes-chat--status-name
          (hermes-chat--result-string result 'status))
    ("queued" (hermes-chat--busy-submit-queued context))
    ("steered" (hermes-chat--busy-submit-steered context))
    (_
     (when-let* ((queue-id (plist-get context :queue-id)))
       (hermes-chat--queue-submit-accepted queue-id)))))

(defun hermes-chat--submit-context-current-p (context)
  "Return non-nil when CONTEXT still owns the current dashboard submission."
  (let ((queue-id (plist-get context :queue-id)))
    (and (eq context hermes-chat--unsettled-submit-context)
         (or (not (plist-get context :application-guard))
             (eq hermes-buffer--owner (plist-get context :application-claim)))
         (hermes-chat--current-lifetime-p (plist-get context :lifetime))
         (hermes-chat--current-transport-generation-p
          (plist-get context :generation))
         (eq (plist-get context :client) hermes-chat--dashboard-client)
         (equal (plist-get context :session-id)
                hermes-chat--dashboard-active-session-id)
         ;; The assistant may finish before the submit RPC acknowledges it.
         ;; Request identity and generation, not its pending node, own settlement.
         (and hermes-chat--nodes
              (gethash (plist-get context :user-id) hermes-chat--nodes))
         (or (null queue-id)
             (hermes-chat--queue-submit-current-p queue-id)))))

(defun hermes-chat--submit-resolve-callback (buffer context)
  "Return BUFFER callback settling dashboard submission CONTEXT."
  (let (settled)
    (lambda (result)
      (hermes-chat--in-buffer buffer
        (when (and (not settled)
                   (hermes-chat--submit-context-current-p context))
          (setq settled t)
          (let ((record (plist-get (plist-get context :queue-entry) :image-record)))
            (if (and record
                     (not (member (hermes-chat--result-string result 'status)
                                  '("streaming" "queued"))))
                (progn
                  (setf (plist-get record :state) 'uncertain)
                  (funcall (hermes-chat--queue-reject-callback buffer context)
                           "Image acceptance unknown; use image recovery"))
              (hermes-chat--submit-resolved context result)
              (hermes-chat--application-notify context 'admitted result)
              (hermes-chat--clear-submit-context context))))))))

(defun hermes-chat--queue-reject-callback (buffer context)
  "Return BUFFER callback rejecting the queued turn described by CONTEXT."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--submit-context-current-p context)
        (hermes-chat--application-notify context 'rejected message)
        (hermes-chat--image-admission-finish (plist-get context :admission) t)
        (when-let* ((record (plist-get (plist-get context :queue-entry) :image-record)))
          (when (eq (plist-get record :state) 'submitted)
            (setf (plist-get record :state) 'uncertain)))
        (hermes-chat--queue-submit-rejected
         (plist-get context :queue-id)
         (plist-get context :user-id)
         (plist-get context :assistant-id)
         message)
        (hermes-chat--clear-submit-context context t)))))

(defun hermes-chat--submit-reject-callback (buffer context)
  "Return BUFFER callback rejecting the turn described by CONTEXT."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--submit-context-current-p context)
        (hermes-chat--application-notify context 'rejected message)
        (hermes-chat--image-admission-finish (plist-get context :admission) t)
        (setq hermes-chat--dashboard-running-p nil)
        (hermes-chat--handle-transport-event
         (plist-get context :assistant-id)
         (list :type 'error :content message))
        (hermes-chat--clear-submit-context context)))))

(defun hermes-chat--begin-pending-turn (user-entry assistant-entry context)
  "Insert USER-ENTRY and ASSISTANT-ENTRY, then activate CONTEXT."
  (let ((assistant-id (plist-get context :assistant-id))
        (dashboard-p (plist-get context :dashboard-p)))
    (hermes-chat--insert-entry user-entry)
    (hermes-chat--insert-entry assistant-entry)
    (hermes-chat--clear-active-tools)
    (hermes-chat--set-header-state
     :status 'pending :activity "Waiting for Hermes"
     :assistant-id assistant-id :last-tool nil :started (current-time))
    (hermes-chat-todos--begin assistant-id)
    (setq hermes-chat--pending-assistant-id assistant-id
          hermes-chat--dashboard-stream-assistant-id (and dashboard-p assistant-id)
          hermes-chat--dashboard-suppress-stream-p nil
          hermes-chat--server-queued-assistant-id nil
          hermes-chat--server-queued-user-id nil
          hermes-chat--server-queued-after-idle-count nil
          hermes-chat--server-queued-prior-terminal-p nil
          hermes-chat--unsettled-submit-context (and dashboard-p context)
          hermes-chat--prepared-submit-assistant-id nil
          hermes-chat--interrupted-assistant-id nil
          hermes-chat--interrupted-events nil
          hermes-chat--interrupt-request-pending-p nil)))

(defun hermes-chat--submit-through-transport (content context resolve reject)
  "Submit CONTENT using CONTEXT with RESOLVE and REJECT callbacks."
  (when-let* ((guard (plist-get context :application-guard)))
    (unless (funcall guard) (user-error "Application prompt owner retired")))
  (let* ((buffer (plist-get context :buffer))
         (assistant-id (plist-get context :assistant-id))
         (dashboard-p (plist-get context :dashboard-p))
         (generation (plist-get context :generation))
         (queue-id (plist-get context :queue-id))
         (claim (plist-get context :application-claim))
         (callback (hermes-chat--transport-callback
                    buffer assistant-id dashboard-p generation))
         (transport
          (hermes-chat--send-prompt
           content
           (if claim
               (lambda (event)
                 (when (and (buffer-live-p buffer)
                            (eq claim (buffer-local-value 'hermes-buffer--owner buffer)))
                   (funcall callback event)))
             callback)
           resolve reject (and queue-id t))))
    (when (equal hermes-chat--pending-assistant-id assistant-id)
      (setq hermes-chat--process transport))
    (when (and queue-id (not dashboard-p))
      (hermes-chat--queue-submit-accepted queue-id))))

(defun hermes-chat--submit-signal-error (context err)
  "Apply synchronous submit ERR to the turn described by CONTEXT."
  (let ((queue-id (plist-get context :queue-id))
        (user-id (plist-get context :user-id))
        (assistant-id (plist-get context :assistant-id))
        (message (error-message-string err)))
    (hermes-chat--image-admission-finish (plist-get context :admission) t)
    (when (plist-get context :dashboard-p)
      (setq hermes-chat--dashboard-running-p nil))
    (if queue-id
        (hermes-chat--queue-submit-rejected
         queue-id user-id assistant-id message)
      (hermes-chat--handle-transport-event
       assistant-id (list :type 'error :content message)))
    (hermes-chat--application-notify context 'rejected message)
    (hermes-chat--clear-submit-context context queue-id)
    (message "Hermes transport failed: %s" message)))

(defvar hermes-chat--submit-truncation nil
  "Truncation plist for the submission being started, or nil.
Let-bound by the edit commands: :row-id names the durable user row to rewind
before, :deep and :empty carry the user's confirmation of removing later
turns or the whole transcript.")

(defun hermes-chat--make-submit-context (content display queue-entry user assistant)
  "Return transport context for CONTENT, DISPLAY, QUEUE-ENTRY, USER, and ASSISTANT."
  (let ((dashboard-p (hermes-chat--dashboard-default-transport-p)))
    (list :buffer (current-buffer)
          :lifetime hermes-chat--lifecycle-generation
          :client nil
          :admission nil
          :truncation hermes-chat--submit-truncation
          :application-guard nil
          :application-claim nil
          :application-observer nil :application-admitted nil
          :application-terminal nil :application-boundary nil
          :session-id nil
          :user-id (plist-get user :id)
          :assistant-id (plist-get assistant :id)
          :dashboard-p dashboard-p
          :generation (hermes-chat--next-transport-generation)
          :idle-count hermes-chat--dashboard-idle-count
          :prior-terminal-p nil :post-start-terminal-p nil
          :queue-id (plist-get queue-entry :id)
          :queue-entry queue-entry
          :content content
          :display display)))

(defun hermes-chat--observe-resumed-application (observer)
  "Observe this ordinarily resumed turn with request-scoped OBSERVER.
The caller must reconcile complete raw history and exact application identity
before using terminal output.  This function neither submits nor resumes work."
  (unless (and (hermes-buffer--owned-p 'hermes-chat-mode)
               hermes-chat--dashboard-session-ready-p
               (not hermes-chat--session-bootstrap)
               (not hermes-chat--application-context))
    (user-error "No available resumed application turn"))
  (setq hermes-chat--application-context
        (list :application-observer observer :application-claim hermes-buffer--owner
              :application-admitted '((status . "streaming"))
              :application-terminal nil :application-boundary nil
              :lifetime hermes-chat--lifecycle-generation
              :generation hermes-chat--transport-generation
              :client hermes-chat--dashboard-client
              :session-id hermes-chat--dashboard-active-session-id
              :idle-count hermes-chat--dashboard-idle-count))
  (add-hook 'after-set-visited-file-name-hook #'hermes-chat--application-retire nil t)
  hermes-chat--application-context)

(defun hermes-chat--submit-callbacks (context)
  "Return the dashboard acceptance callbacks for CONTEXT."
  (let ((buffer (plist-get context :buffer)))
    (cons (hermes-chat--submit-resolve-callback buffer context)
          (if (plist-get context :queue-id)
              (hermes-chat--queue-reject-callback buffer context)
            (hermes-chat--submit-reject-callback buffer context)))))

(defun hermes-chat--submit-content (content &optional display queue-entry application-guard
                                         application-observer)
  "Submit CONTENT as a new user turn, echoing DISPLAY when non-nil.
DISPLAY lets a slash skill send its full payload while showing a compact line.
QUEUE-ENTRY identifies a queued message retained until transport acceptance.
APPLICATION-GUARD, when non-nil, must still authorize this application prompt
at dispatch; it also forces non-interrupting backend admission.
APPLICATION-OBSERVER receives (CONTEXT KIND PAYLOAD), where KIND is admitted,
terminal or rejected.  Terminal retains exact normalized final text and status.
It requires APPLICATION-GUARD and is independent of transcript rendering.
Return non-nil when the transport request starts."
  (hermes-chat--ensure-submit-allowed)
  (when (and (hermes-chat--active-turn-p) (null queue-entry))
    (user-error "%s" (hermes-chat--busy-message)))
  (let* ((user-entry (hermes-chat--make-entry
                      (if application-guard 'application 'user)
                      (or display content) 'done))
         (assistant-entry (hermes-chat--make-entry 'assistant "" 'pending))
         (context (hermes-chat--make-submit-context
                   content display queue-entry user-entry assistant-entry))
         (callbacks (and (plist-get context :dashboard-p)
                         (hermes-chat--submit-callbacks context))))
    (when application-guard
      (setf (plist-get context :application-guard) application-guard
            (plist-get context :application-claim) hermes-buffer--owner))
    (when application-observer
      (unless application-guard (user-error "Application observer needs an owner guard"))
      (when hermes-chat--application-context (user-error "Application turn still pending"))
      (setf (plist-get context :application-observer) application-observer)
      (setq hermes-chat--application-context context)
      (add-hook 'after-set-visited-file-name-hook #'hermes-chat--application-retire nil t))
    (hermes-chat--begin-pending-turn user-entry assistant-entry context)
    (condition-case err
        (progn
          (hermes-chat--submit-through-transport
           content context (car callbacks) (cdr callbacks))
          t)
      (error
       (when (or (not application-guard)
                 (and (eq hermes-buffer--owner (plist-get context :application-claim))
                      (hermes-chat--current-lifetime-p (plist-get context :lifetime))
                      (hermes-chat--current-transport-generation-p
                       (plist-get context :generation))))
         (hermes-chat--submit-signal-error context err))
       nil))))

;; The registry installation near the end of this file wires this submit
;; pipeline into lower chat layers without upward references.


(defun hermes-chat--queue-image-draft ()
  "Transfer exact composer text and image bytes to a recoverable FIFO entry."
  (unless (hermes-chat--dashboard-default-transport-p)
    (user-error "Images require the dashboard transport"))
  (when (or (hermes-chat--pending-prompt-p)
            (hermes-chat--parse-slash (hermes-chat-input-string)))
    (user-error "Images require an ordinary message, not a command or prompt reply"))
  (let* ((record hermes-chat--image-draft-record)
         (content (hermes-chat-input-string))
         (display (format "%s\n[%d image(s)]" content
                          (length hermes-chat--draft-images))))
    (setf (plist-get record :content) content
          (plist-get record :state) 'local)
    (hermes-chat--queue-content content "Queued message with local images" display record)
    (setq hermes-chat--draft-images nil
          hermes-chat--image-draft-record nil)
    (hermes-chat--delete-input-tail)
    (hermes-chat--drain-queued-message)
    t))

(defun hermes-chat-queue-message (&optional message)
  "Queue MESSAGE to send after the active Hermes turn, or send now if idle."
  (interactive nil hermes-chat-mode)
  (hermes-chat--ensure-submit-allowed)
  (if (and (null message) hermes-chat--draft-images)
      (hermes-chat--queue-image-draft)
    (let ((content (string-trim (or message (hermes-chat-input-string)))))
      (when (string-empty-p content)
	(user-error "No Hermes input to queue"))
      (unless message
	(hermes-chat--delete-input-tail))
      (hermes-chat--dashboard-queue-or-submit content (current-buffer)))))

(defun hermes-chat--steer-rejected (content message)
  "Handle rejected steer CONTENT with fallback MESSAGE."
  (hermes-chat--insert-local-status
   (format "Steer unavailable (%s); queued next message" message) 'error)
  (hermes-chat--queue-or-submit-content content))

(defun hermes-chat--steer-pending-status (content)
  "Insert an immediate pending steer entry for CONTENT; return its entry id.
Gives instant feedback that the steer was sent, before the gateway acks the
`session.steer' round-trip."
  (let ((id (hermes-chat--next-id 'steer)))
    (hermes-chat--insert-entry
     (hermes-chat--make-entry
      'status (format "Steering… %s" (hermes-chat--preview content))
      'running id)
     (hermes-chat--pending-assistant-node))
    id))

(defun hermes-chat--steer-acknowledged (id content)
  "Settle the pending steer entry ID as an accepted steer of CONTENT.
The gateway injects the text into the running turn -- it reaches the agent on
its next step -- so this is an acknowledgment, not the deferred queue fallback.
A no-op when the entry is gone (e.g. the chat was cleared mid-steer)."
  (when (and hermes-chat--nodes (gethash id hermes-chat--nodes))
    (hermes-chat--update-entry
     id (lambda (entry)
          (hermes-chat--entry-with
           entry
           :content (format "Steering: %s" (hermes-chat--preview content))
           :status 'done)))))

(defun hermes-chat--steer-failed (id content message)
  "Drop the pending steer entry ID, then queue CONTENT after MESSAGE fallback."
  (when (and hermes-chat--nodes (gethash id hermes-chat--nodes))
    (hermes-chat--remove-entry id)
    (hermes-chat--steer-rejected content message)))

(defun hermes-chat--steer-active-turn (content buffer)
  "Steer active dashboard turn with CONTENT in BUFFER, or queue when unsupported."
  (if (not (hermes-chat--dashboard-session-attached-p))
      (hermes-chat--queue-content content "Steer unavailable; queued next message")
    (let ((client hermes-chat--dashboard-client)
          (session-id hermes-chat--dashboard-active-session-id)
          (generation hermes-chat--lifecycle-generation)
          (id (hermes-chat--steer-pending-status content))
          (owner (list :text content)))
      (setq hermes-chat--pending-steers
            (append hermes-chat--pending-steers (list owner)))
      (hermes-dashboard-transport-session-steer
       client content
       :session-id session-id
       :resolve (lambda (result)
                  (hermes-chat--in-buffer buffer
                    (when (and (memq owner hermes-chat--pending-steers)
                               (hermes-chat--dashboard-context-current-p
                                client generation session-id))
                      (setq hermes-chat--pending-steers
                            (delq owner hermes-chat--pending-steers))
                      (if (equal (hermes-chat--status-name
                                  (hermes-chat--result-string result 'status))
                                 "rejected")
                          (hermes-chat--steer-failed id content "rejected")
                        (hermes-chat--steer-acknowledged id content)))))
       :reject (lambda (err)
                 (hermes-chat--in-buffer buffer
                   (when (and (memq owner hermes-chat--pending-steers)
                              (hermes-chat--dashboard-context-current-p
                               client generation session-id))
                     (setq hermes-chat--pending-steers
                           (delq owner hermes-chat--pending-steers))
                     (hermes-chat--steer-failed id content err))))))))

(defun hermes-chat--steer-or-submit (content buffer)
  "Steer active turn with CONTENT in BUFFER, or submit CONTENT when idle."
  (if hermes-chat--pending-assistant-id
      (hermes-chat--steer-active-turn content buffer)
    (hermes-chat--queue-or-submit-content content)))

(defun hermes-chat--dashboard-steer-or-submit (content buffer)
  "Resume stored dashboard session in BUFFER before steering or submitting CONTENT."
  (if (hermes-chat--dashboard-stored-session-needs-resume-p)
      (hermes-chat--with-dashboard-session
       content buffer
       (lambda (_live-client)
         (hermes-chat--steer-or-submit content buffer)))
    (hermes-chat--steer-or-submit content buffer)))

(defun hermes-chat-steer-message (&optional message)
  "Steer the active dashboard run with MESSAGE, falling back to queue."
  (interactive nil hermes-chat-mode)
  (when (and (null message) hermes-chat--draft-images)
    (user-error "Images cannot steer a turn; use Send or Queue message"))
  (hermes-chat--ensure-submit-allowed)
  (let ((content (string-trim (or message (hermes-chat-input-string))))
        (buffer (current-buffer)))
    (when (string-empty-p content)
      (user-error "No Hermes input to steer"))
    (unless message
      (hermes-chat--delete-input-tail))
    (hermes-chat--dashboard-steer-or-submit content buffer)))

(defun hermes-chat--interrupt-rejected (assistant-id generation message)
  "Restore ASSISTANT-ID after its GENERATION interrupt fails with MESSAGE."
  (when (and (hermes-chat--current-transport-generation-p generation)
             (equal hermes-chat--interrupted-assistant-id assistant-id)
             hermes-chat--interrupt-request-pending-p)
    (let ((events (nreverse hermes-chat--interrupted-events)))
      (setq hermes-chat--interrupted-assistant-id nil
            hermes-chat--interrupted-events nil
            hermes-chat--interrupt-request-pending-p nil)
      (when (equal hermes-chat--pending-assistant-id assistant-id)
        (hermes-chat--mark-assistant assistant-id 'streaming))
      (mapc (lambda (event)
              ;; Interim replay can rotate presentation, but a terminal can
              ;; drain the FIFO and replace the turn's generation entirely.
              (when (hermes-chat--current-transport-generation-p generation)
                (hermes-chat--handle-transport-event
                 (or hermes-chat--pending-assistant-id assistant-id) event)))
            events))
    (hermes-chat--insert-local-status
     (format "Interrupt failed: %s" message) 'error)
    (when (equal hermes-chat--pending-assistant-id assistant-id)
      (hermes-chat--set-header-state
       :status 'running :activity "Interrupt failed"))))

(defun hermes-chat--interrupt-reject-callback
    (buffer assistant-id generation)
  "Return BUFFER callback rejecting ASSISTANT-ID at GENERATION."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (hermes-chat--interrupt-rejected assistant-id generation message))))

(defun hermes-chat--discard-server-queued-turn ()
  "Settle the backend-queued turn discarded by an accepted interrupt."
  (when-let* ((assistant-id hermes-chat--server-queued-assistant-id))
    (hermes-chat--mark-assistant
     assistant-id 'interrupted "Queued turn canceled by interrupt" t)
    (hermes-chat--settle-transport-entries assistant-id 'interrupted)
    (when (equal assistant-id hermes-chat--pending-assistant-id)
      (setq hermes-chat--pending-assistant-id nil
            hermes-chat--process nil)
      (hermes-chat--dashboard-finish-assistant assistant-id)))
  (setq hermes-chat--server-queued-assistant-id nil
        hermes-chat--server-queued-user-id nil
        hermes-chat--server-queued-after-idle-count nil
        hermes-chat--server-queued-prior-terminal-p nil))

(defun hermes-chat--finish-reconciled-interrupt (assistant-id generation)
  "Finish ASSISTANT-ID when its GENERATION interrupt reaches backend idle."
  (when (and (hermes-chat--current-transport-generation-p generation)
             (equal hermes-chat--pending-assistant-id assistant-id)
             (equal hermes-chat--interrupted-assistant-id assistant-id))
    (hermes-chat--discard-server-queued-turn)
    (setq hermes-chat--interrupted-events nil)
    (hermes-chat--handle-transport-event
     assistant-id '(:type error :status "interrupted"))))

(defun hermes-chat--held-interrupt-terminal ()
  "Return the first held terminal event in arrival order, or nil."
  (seq-find (lambda (event)
              (memq (plist-get event :type) '(done error)))
            (nreverse hermes-chat--interrupted-events)))

(defun hermes-chat--interrupt-resolve-callback
    (buffer assistant-id generation)
  "Return BUFFER callback reconciling ASSISTANT-ID at GENERATION after acceptance."
  (lambda (_result)
    (hermes-chat--in-buffer buffer
      (when (and (hermes-chat--current-transport-generation-p generation)
                 (equal hermes-chat--interrupted-assistant-id assistant-id)
                 hermes-chat--interrupt-request-pending-p)
        (let ((terminal (hermes-chat--held-interrupt-terminal)))
          (setq hermes-chat--interrupt-request-pending-p nil
                hermes-chat--interrupted-events nil)
          (hermes-chat--discard-server-queued-turn)
          (if terminal
              (hermes-chat--handle-transport-event assistant-id terminal)
            (hermes-chat--dashboard-schedule-idle-reconciliation
             (lambda ()
               (hermes-chat--finish-reconciled-interrupt
                assistant-id generation)))))))))

(defun hermes-chat-interrupt ()
  "Interrupt image preparation locally, or request interruption of the run."
  (interactive nil hermes-chat-mode)
  (when (gethash (hermes-chat--image-session-key) hermes-chat--image-prior-submits)
    (puthash (hermes-chat--image-session-key) '(uncertain) hermes-chat--image-prior-submits))
  (when-let* ((owner (gethash (hermes-chat--image-session-key)
                             hermes-chat--image-session-blocks)))
    (when (memq (plist-get owner :state) '(submitted accepted))
      (setf (plist-get owner :state) 'uncertain)))
  (let* ((context hermes-chat--unsettled-submit-context)
         (record (plist-get (plist-get context :queue-entry) :image-record))
         (phase (plist-get record :state)))
    (if (or (memq phase '(uploading attaching))
            (and (eq phase 'local) hermes-chat--session-bootstrap))
        (progn
          ;; Retire fresh-session callbacks before releasing the FIFO owner.
          ;; A late create receipt must not submit canceled text without images.
          (when (eq phase 'local)
            (setq hermes-chat--session-bootstrap nil))
          (setf (plist-get record :state)
                (if (eq phase 'attaching) 'uncertain 'local))
          (when (eq phase 'uploading)
            (hermes-chat--images-release record))
          (funcall (hermes-chat--queue-reject-callback (current-buffer) context)
                   "Image preparation interrupted; bytes retained"))
      (hermes-chat--interrupt-run))))

(defun hermes-chat-interrupt-turn ()
  "Interrupt the running turn, or only report that nothing is running.
Unlike `hermes-chat-interrupt', an idle chat is not an error, so the
shell-style \\[hermes-chat-interrupt-turn] is always safe to press."
  (interactive nil hermes-chat-mode)
  (if (hermes-chat--interrupt-unavailable-p)
      (message "No running Hermes turn to interrupt")
    (hermes-chat-interrupt)))

(defun hermes-chat--interrupt-run ()
  "Request interruption of the active dashboard run."
  (interactive nil hermes-chat-mode)
  (when hermes-chat--busy-submit-context
    (user-error "Hermes is accepting the previous message"))
  (unless hermes-chat--pending-assistant-id
    (user-error "No active Hermes run to interrupt"))
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support interrupt"))
  (let ((buffer (current-buffer))
        (assistant-id hermes-chat--pending-assistant-id)
        (generation hermes-chat--transport-generation))
    (setq hermes-chat--interrupted-assistant-id assistant-id
          hermes-chat--interrupted-events nil
          hermes-chat--interrupt-request-pending-p t)
    (hermes-chat--reasoning-row assistant-id nil)
    (hermes-chat--mark-assistant assistant-id 'interrupted)
    (hermes-chat--insert-local-status "Interrupt requested" 'interrupted)
    (hermes-chat--set-header-state
     :status 'interrupted :activity "Interrupt requested")
    (condition-case err
        (hermes-dashboard-transport-session-interrupt
         hermes-chat--dashboard-client
         :session-id hermes-chat--dashboard-active-session-id
         :resolve (hermes-chat--interrupt-resolve-callback
                   buffer assistant-id generation)
         :reject (hermes-chat--interrupt-reject-callback
                  buffer assistant-id generation))
      (error
       (hermes-chat--interrupt-rejected
        assistant-id generation (error-message-string err))))))

(defun hermes-chat-interrupt-and-send (&optional message)
  "Interrupt the active run, then queue MESSAGE for the next turn when non-empty.
MESSAGE defaults to the input tail.  The interrupt fires first and
unconditionally, so an empty input still stops the run instead of erroring."
  (interactive nil hermes-chat-mode)
  (unless hermes-chat--pending-assistant-id
    (user-error "No active Hermes run to interrupt"))
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support interrupt"))
  (let ((content (string-trim (or message (hermes-chat-input-string)))))
    (hermes-chat-interrupt)
    (unless (string-empty-p content)
      (hermes-chat-queue-message message))))

(defun hermes-chat-disconnect ()
  "End this chat's dashboard session so a new one can be started.
Tears down the live client when present (best effort, even when it is stale
or in an error state) and clears the live session state.  The durable
session key is preserved, so the conversation can still be resumed.
Local input is copied to an editable recovery buffer for manual sending;
the current draft stays here.  Uncertain deliveries require history inspection."
  (interactive nil hermes-chat-mode)
  (unless (or hermes-chat--dashboard-client
              hermes-chat--process
              hermes-chat--dashboard-active-session-id)
    (user-error "This Hermes chat has no session to disconnect"))
  (when hermes-chat--disconnect-in-progress
    (user-error "Disconnect is already in progress"))
  (let ((hermes-chat--disconnect-in-progress t))
    (hermes-chat--capture-recovery)
    (when (buffer-live-p hermes-chat--recovery-buffer)
      (display-buffer hermes-chat--recovery-buffer))
    ;; Mark before final input capture, but do not publish into work-list
    ;; display hooks until the attachment has released its resources.
    (when-let* ((assistant-id hermes-chat--pending-assistant-id))
      (hermes-chat--mark-assistant assistant-id 'disconnected nil t t))
    (run-hooks 'hermes-chat-cleanup-functions)
    (hermes-chat--invalidate-transport-state t)
    (hermes-chat--stop-dashboard-client)
    (hermes-chat--insert-offline-notice "Session disconnected — input kept")
    (when (buffer-live-p hermes-chat--recovery-buffer)
      (hermes-chat--insert-local-status
       (format "Input preserved in %s; resume via Sessions and send manually"
               (buffer-name hermes-chat--recovery-buffer)))
      (display-buffer hermes-chat--recovery-buffer))
    (hermes-chat--set-header-state :status 'disconnected :activity "Disconnected")))

(defun hermes-chat-show-recovery ()
  "Display the editable document holding this chat's preserved input.
It is filled by disconnect and restart; nothing in it is sent automatically."
  (interactive nil hermes-chat-mode)
  (unless (buffer-live-p hermes-chat--recovery-buffer)
    (user-error "This chat has no recovery draft"))
  (display-buffer hermes-chat--recovery-buffer))

(defun hermes-chat--dashboard-client-reconnectable-p (client)
  "Return non-nil when CLIENT's lost socket can be restarted in place.
The client must still be attached, own its registry entry and not be
stopping; a stopped client is replaced by the next send instead."
  (and (hermes-dashboard-transport-client-p client)
       (> (or (hermes-dashboard-transport-client-refcount client) 0) 0)
       (not (hermes-dashboard-transport-client-stopping-p client))
       (hermes-dashboard-transport--current-registry-owner-p client)))

(defun hermes-chat--dashboard-client-active-turn-p (client)
  "Return non-nil when a chat sharing CLIENT has an active turn."
  (cl-some
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'hermes-chat-mode)
            (eq hermes-chat--dashboard-client client)
            (hermes-chat--active-turn-p))))
   (buffer-list)))

;;;###autoload
(defun hermes-dashboard-reconnect ()
  "Reconnect this chat's shared dashboard socket when every owner is idle."
  (interactive nil hermes-chat-mode)
  (unless (or (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
              (hermes-chat--dashboard-client-reconnectable-p
               hermes-chat--dashboard-client))
    (user-error "This chat has no live dashboard client to reconnect"))
  (when (hermes-chat--dashboard-client-active-turn-p
         hermes-chat--dashboard-client)
    (user-error "Interrupt every active turn sharing this dashboard first"))
  (hermes-dashboard-transport-reconnect hermes-chat--dashboard-client))

;;;###autoload
(defalias 'hermes-reconnect #'hermes-dashboard-reconnect)

(defvar hermes-chat--dashboard-restarting nil
  "Non-nil during synchronous shared dashboard replacement.")

(defun hermes-chat--dashboard-buffers (client)
  "Return live chat buffers sharing exact CLIENT."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'hermes-chat-mode)
                       (eq client hermes-chat--dashboard-client))))
              (buffer-list)))

(defun hermes-chat--restart-current-p (owner)
  "Return non-nil when restart OWNER still owns this chat's attachment."
  (and (eq owner hermes-chat--session-bootstrap)
       (hermes-chat--restart-context-current-p owner)))

(defun hermes-chat--restart-context-current-p (owner &optional terminal)
  "Return non-nil when OWNER's captured attachment still owns this chat.
For TERMINAL settlement, allow the captured socket generation to retire."
  (and (eq (plist-get owner :buffer) (current-buffer))
       (hermes-chat--dashboard-context-current-p
        (plist-get owner :client) (plist-get owner :generation))
       (equal (plist-get owner :stored) hermes-chat--session-id)
       (equal (plist-get owner :session-id) hermes-chat--dashboard-active-session-id)
       (or terminal
           (null (plist-get owner :connection))
           (= (plist-get owner :connection)
              (hermes-dashboard-transport-client-generation
               (plist-get owner :client))))))

(defun hermes-chat--restart-failed (owner message)
  "Settle current restart OWNER with MESSAGE, retaining local data."
  ;; Transport retires its generation before rejecting pending resume requests.
  (when (and (eq owner hermes-chat--session-bootstrap)
             (hermes-chat--restart-context-current-p owner t))
    (setq hermes-chat--session-bootstrap nil)
    (hermes-chat--set-header-state :status 'error :activity message)
    (message "Hermes restart (%s): %s" (buffer-name) message)))

(defun hermes-chat--restart-resumed (owner result)
  "Attach restart OWNER to RESULT without replaying the transcript or input."
  (when (hermes-chat--restart-current-p owner)
    (let* ((client (plist-get owner :client))
           (active (hermes-chat--dashboard-active-id-from-result client result))
           (first (null (plist-get owner :session-id))))
      (if (not active)
          (hermes-chat--restart-failed owner "Resume returned no live session")
        (when first
          ;; Accept only the identity derived from this owned backend result,
          ;; not whatever a recording callback may install in the buffer.
          (setf (plist-get owner :session-id) active
                (plist-get owner :stored)
                (hermes-chat--dashboard-stored-id-from-result client result active))
          (hermes-chat--dashboard-record-session client result))
        (when (hermes-chat--restart-current-p owner)
          (if (and first (hermes-transport--get result 'auto_continue))
              ;; Cold resume may say running=false while its recovery thread
              ;; already emitted message.start.  Bind routing first, then read
              ;; the live session once; this never submits a continuation.
              (hermes-chat--restart-resume owner)
            (when (and (hermes-chat--dashboard-result-live-turn-p result)
                       (not hermes-chat--pending-assistant-id))
              (setq hermes-chat--dashboard-running-p t)
              (hermes-chat--dashboard-restore-inflight-turn client))
            (when (hermes-chat--restart-current-p owner)
              (unless (hermes-chat--active-turn-p)
                (hermes-chat--set-header-state
                 :status 'ready :activity "Dashboard restarted; session resumed"))
              (when (hermes-chat--restart-current-p owner)
                (hermes-chat--dashboard-restore-pending-clarify result)
                (when (hermes-chat--restart-current-p owner)
                  (setq hermes-chat--session-bootstrap nil))))))))))

(defun hermes-chat--restart-resume (owner)
  "Resume only the original durable session for restart OWNER."
  (when (hermes-chat--restart-current-p owner)
    (let* ((buffer (current-buffer))
           (client (plist-get owner :client))
           (fail (lambda (message)
                   (hermes-chat--in-buffer buffer
                     (hermes-chat--restart-failed owner message)))))
      (if (null (plist-get owner :stored))
          (progn
            (setq hermes-chat--session-bootstrap nil)
            (hermes-chat--set-header-state :status 'ready :activity "Dashboard ready"))
        (condition-case err
            (hermes-dashboard-transport-session-resume
             client (plist-get owner :stored)
             :cols (hermes-chat--dashboard-cols) :profile hermes-chat--profile
             :resolve
             (lambda (result)
               (hermes-chat--in-buffer buffer
                 (hermes-chat--restart-resumed owner result)))
             :reject fail)
          ((error quit) (funcall fail (error-message-string err))))))))

(defun hermes-chat--restart-reserve (record)
  "Publish restart RECORD's reservation before invoking display callbacks."
  (let ((owner (append (hermes-chat--dashboard-begin-bootstrap
                        hermes-chat--dashboard-client 'restart #'ignore)
                       (list :buffer (current-buffer)
                             :stored hermes-chat--session-id :connection nil))))
    (setcdr record owner)
    (setq hermes-chat--session-bootstrap owner)
    (when (buffer-live-p hermes-chat--recovery-buffer)
      (hermes-chat--insert-local-status
       (format "Restart: input preserved in %s; inspect history and send manually"
               (buffer-name hermes-chat--recovery-buffer))))
    (when (hermes-chat--restart-current-p owner)
      (hermes-chat--set-header-state
       :status 'reconnecting
       :activity (if (buffer-live-p hermes-chat--recovery-buffer)
                     (format "Restarting; unsent input in %s (send manually)"
                             (buffer-name hermes-chat--recovery-buffer))
                   "Restarting dashboard")))))

(defun hermes-chat--restart-prepare (record)
  "Detach the captured attachment in RECORD and reserve it for restart."
  (hermes-chat--in-buffer (car record)
    (catch 'retired
      (when (hermes-chat--restart-context-current-p (cdr record))
        (dolist (assistant-id (delete-dups
                               (delq nil (list hermes-chat--pending-assistant-id
                                               hermes-chat--server-queued-assistant-id))))
          (hermes-chat--mark-assistant assistant-id 'interrupted nil t t)
          (hermes-chat--settle-transport-entries assistant-id 'interrupted))
        (run-hook-wrapped
         'hermes-chat-cleanup-functions
         (lambda (function)
           (funcall function)
           (not (hermes-chat--restart-context-current-p (cdr record)))))
        ;; Cleanup may replace this attachment or kill a later participant.
        (when (hermes-chat--restart-context-current-p (cdr record))
          ;; Invalidation advances the lifetime before calling its hooks.  Track
          ;; that deliberate transition, but leave immediately on replacement.
          (let* ((hooks hermes-chat-lifecycle-invalidation-hook)
                 (hermes-chat-lifecycle-invalidation-hook
                  (list (lambda ()
                          (setf (plist-get (cdr record) :generation)
                                hermes-chat--lifecycle-generation)
                          (let ((hermes-chat-lifecycle-invalidation-hook hooks))
                            (run-hook-wrapped
                             'hermes-chat-lifecycle-invalidation-hook
                             (lambda (function)
                               (funcall function)
                               (unless (hermes-chat--restart-context-current-p (cdr record))
                                 (throw 'retired nil))
                               nil)))))))
            (hermes-chat--invalidate-transport-state t))
          (unless (hermes-chat--restart-context-current-p (cdr record))
            (throw 'retired nil))
          (hermes-chat--clear-terminal-prompts '(:type error))
          (unless (hermes-chat--restart-context-current-p (cdr record))
            (throw 'retired nil))
          (hermes-chat--clear-active-tools)
          (setf (plist-get (cdr record) :session-id) nil)
          (hermes-chat--forget-live-dashboard-session)
          (unless (hermes-chat--restart-context-current-p (cdr record))
            (throw 'retired nil))
          (setq hermes-chat--dashboard-token nil
                hermes-chat--dashboard-detached-assistant-id nil)
          (hermes-chat--restart-reserve record))))))

(defun hermes-chat--restart-attach (record client)
  "Attach restart RECORD to replacement CLIENT and await readiness."
  (hermes-chat--in-buffer (car record)
    (let ((owner (cdr record)))
      (when (hermes-chat--restart-current-p owner)
        (cl-incf (hermes-dashboard-transport-client-refcount client))
        (setq hermes-chat--dashboard-client client)
        (setq-local hermes-dashboard-transport-request-owner (current-buffer))
        (setf (plist-get owner :client) client
              (plist-get owner :connection)
              (hermes-dashboard-transport-client-generation client))
        (hermes-chat--ensure-idle-listener client (current-buffer))
        (hermes--promise-then
         (hermes-dashboard-transport-client-ready-promise client)
         (lambda (_value)
           (hermes-chat--in-buffer (car record)
             (hermes-chat--restart-resume owner)))
         (lambda (message)
           (hermes-chat--in-buffer (car record)
             ;; Startup failure terminally increments the connection generation.
             (when (hermes-chat--dashboard-bootstrap-current-p owner)
               (setf (plist-get owner :connection) nil)
               (hermes-chat--restart-failed owner message)))))))))

(defun hermes-chat--restart-replace-client (client records)
  "Attach restart RECORDS to a replacement of CLIENT, releasing its lease."
  (let ((replacement
         (hermes-dashboard-transport-acquire
          :host (hermes-dashboard-transport-client-host client)
          :port (hermes-dashboard-transport-client-port client)
          :start-mode 'spawn :callback #'ignore)))
    (unwind-protect
        (dolist (record records)
          (hermes-chat--restart-attach record replacement))
      (hermes-dashboard-transport-release replacement))))

;;;###autoload
(defun hermes-dashboard-restart ()
  "Confirm restarting this chat's shared Emacs-owned dashboard.
Stop in-flight work across all clients of that dashboard, preserve live chat
buffers and drafts, and asynchronously resume their original durable sessions.
Copy queued and uncertain input to editable recovery buffers; never resend it.
The backend may automatically recover interrupted turns under its own policy.
Blank chats remain blank.  Failed sessions remain available for manual retry.
Remote dashboards are unsupported; use `hermes-dashboard-reconnect' instead."
  (interactive nil hermes-chat-mode)
  (let* ((client hermes-chat--dashboard-client)
         (generation (and (hermes-dashboard-transport-client-p client)
                          (hermes-dashboard-transport-client-generation client)))
         (buffers (hermes-chat--dashboard-buffers client)))
    (unless (and client (eq (hermes-dashboard-transport--client-start-mode client)
                           'spawn))
      (user-error "Restart requires an Emacs-owned dashboard; use hermes-dashboard-reconnect for remote sockets"))
    (when (or hermes-chat--dashboard-restarting
              (seq-some (lambda (buffer)
                          (with-current-buffer buffer
                            (eq (plist-get hermes-chat--session-bootstrap :kind)
                                'restart))) buffers))
      (user-error "Dashboard restart is already in progress"))
    (when (yes-or-no-p
           (format "Restart shared dashboard (%d chats), stopping in-flight work for ALL its clients? "
                   (length buffers)))
      (unless (and (eq client hermes-chat--dashboard-client)
                   (= generation (hermes-dashboard-transport-client-generation client)))
        (user-error "Dashboard changed while confirming; try again"))
      (let* ((hermes-chat--dashboard-restarting t)
             (prepared nil)
             (records
              (mapcar (lambda (buffer)
                        (with-current-buffer buffer
                          (cons buffer
                                (list :client client :buffer buffer
                                      :generation hermes-chat--lifecycle-generation
                                      :stored hermes-chat--session-id
                                      :session-id hermes-chat--dashboard-active-session-id))))
                      (hermes-chat--dashboard-buffers client))))
        ;; Snapshot everyone before the first hook; preserve all input before
        ;; the first destructive operation.  Failed preparation is retryable.
        (condition-case err
            (progn
              (dolist (record records)
                (hermes-chat--in-buffer (car record)
                  (when (hermes-chat--restart-context-current-p (cdr record))
                    (hermes-chat--capture-recovery))))
              (mapc #'hermes-chat--restart-prepare records)
              (setq prepared t)
              (hermes-dashboard-transport-stop client "Dashboard explicitly restarted")
              (hermes-chat--restart-replace-client client records))
          ((error quit)
           (dolist (record records)
             (condition-case nil
                 (hermes-chat--in-buffer (car record)
                   (hermes-chat--restart-failed (cdr record)
                                               (error-message-string err)))
               ((error quit) nil)))
           (unless prepared (signal (car err) (cdr err)))))))))

(defun hermes-chat-stop-processes ()
  "Confirm stopping all processes in the connected Hermes instance.
This affects background/tool processes across all chats, not just this chat.
It does not interrupt the current model turn; use `hermes-chat-interrupt'
for that."
  (interactive nil hermes-chat-mode)
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support stopping processes"))
  (let* ((buffer (current-buffer))
         (client hermes-chat--dashboard-client)
         (context (hermes-chat--command-context client))
         (connection (hermes-dashboard-transport-client-generation client))
         (current-p
          (lambda ()
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (hermes-chat--command-context-current-p context)
                        (hermes-chat--dashboard-session-attached-p)
                        (= connection
                           (hermes-dashboard-transport-client-generation
                            client))))))))
    (when (and (yes-or-no-p
                "Stop all background/tool processes in the connected Hermes instance (all chats)? ")
               (funcall current-p))
      (hermes-dashboard-transport-process-stop
       client
       :resolve (lambda (result)
                  (when (funcall current-p)
                    (with-current-buffer buffer
                      (hermes-chat--insert-local-status
                       (format "Stopped %s background process(es) across all chats"
                               (or (hermes-transport--get result 'killed) 0))
                       'done))))
       :reject (lambda (message)
                 (when (funcall current-p)
                   (with-current-buffer buffer
                     (hermes-chat--command-error message))))))))

(defun hermes-chat--reset-transcript ()
  "Tear down the live session and re-initialize this chat buffer empty.
Stops any live dashboard client, clears the EWOC transcript and header, and
forgets both the live and durable session ids so the next send starts fresh."
  (let* ((active-sink
          (and (eq (car hermes-chat--reset-clarify-owner-sink)
                   (current-buffer))
               hermes-chat--reset-clarify-owner-sink))
         (outermost (null active-sink))
         (hermes-chat--reset-clarify-owner-sink
          (or active-sink (list (current-buffer) nil))))
    (hermes-chat-draft--cancel)
    (run-hooks 'hermes-chat-cleanup-functions)
    (hermes-chat--invalidate-transport-state)
    (hermes-chat--stop-dashboard-client)
    (hermes-chat--setup-buffer)
    (hermes-chat--prompt-indicator-sync)
    (hermes-chat-draft--activate)
    (hermes-chat--restore-draft-runtime)
    (hermes-chat--placeholder-update)
    (when outermost
      (hermes-chat--drain-reset-clarify-owners
       hermes-chat--reset-clarify-owner-sink))))

(defun hermes-chat-clear ()
  "Clear this chat's transcript and start a fresh Hermes session in place."
  (interactive nil hermes-chat-mode)
  (when (y-or-n-p "Clear this Hermes conversation and transcript? ")
    (hermes-chat--reset-transcript)
    (hermes-chat--insert-local-status "Session cleared" 'done)))

(defun hermes-chat-btw (&optional question)
  "Ask QUESTION about the live conversation without changing main history.
With no QUESTION, use the composer.  Failure retains the question for manual
recovery; it never falls back to an independent tool-capable background task."
  (interactive nil hermes-chat-mode)
  (hermes-chat--ensure-submit-allowed)
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "A side question needs an attached conversation"))
  (let ((content (or question (hermes-chat-input-string))))
    (when (string-empty-p (string-trim content))
      (user-error "No Hermes side question given"))
    (when (or (null question)
              (equal (hermes-chat--parse-slash (hermes-chat-input-string))
                     (cons "btw" question)))
      (hermes-chat--delete-input-tail))
    (hermes-chat--background-submit content (current-buffer) t)))

(defun hermes-chat--branch-receipt-p (result parent)
  "Return non-nil for a complete child RESULT belonging to PARENT."
  (let ((live (hermes-transport--get result 'session_id))
        (stored (hermes-transport--get result 'stored_session_id))
        (messages (hermes-transport--get result 'messages)))
    (and (stringp live) (not (string-empty-p live))
         (stringp stored) (not (string-empty-p stored))
         (not (equal stored parent))
         (equal (hermes-transport--get result 'parent) parent)
         (listp messages) messages
         (seq-every-p (lambda (message)
                        (pcase (hermes-transport--get message 'role)
                          ((or "user" "assistant")
                           (stringp (hermes-transport--get message 'text)))
                          ("tool" (stringp (hermes-transport--get message 'name)))
                          (_ nil)))
                      messages))))

(defun hermes-chat--adopt-branch (client result current-p)
  "Open CLIENT's child RESULT while CURRENT-P retains parent authority.
Hydrate in a fresh buffer; failed initialization never rewrites the parent
or deletes the child already persisted by the backend."
  (let ((instance hermes-instance)
        (profile hermes-chat--profile)
        (mode hermes-chat--resolved-start-mode)
        (parent (current-buffer))
        (child (generate-new-buffer hermes-chat-buffer-name))
        claim lifetime accepted)
    (cl-labels ((child-current-p ()
                  (and (buffer-live-p child)
                       (with-current-buffer child
                         (and (eq claim hermes-buffer--owner)
                              (hermes-buffer--owned-p 'hermes-chat-mode)
                              (eql lifetime hermes-chat--lifecycle-generation)))))
		(check-current ()
                  (unless (and (child-current-p) (buffer-live-p parent)
                               (with-current-buffer parent (funcall current-p)))
                    (error "Branch view was replaced during adoption"))))
      (unwind-protect
          (progn
            (with-current-buffer child
              (hermes-chat-mode)
              (hermes-buffer--claim 'hermes-chat-mode)
              (setq claim hermes-buffer--owner
                    lifetime hermes-chat--lifecycle-generation)
              (setq hermes-instance instance hermes-chat--profile profile
                    hermes-chat--resolved-start-mode mode)
              (unless (and (with-current-buffer parent (funcall current-p))
                           (eq client (hermes-chat--dashboard-start
                                       (hermes-chat--transport-callback
					child nil t (hermes-chat--next-transport-generation)))))
		(error "Branch connection was replaced"))
              (check-current)
              (hermes-chat--render-history (hermes-transport--get result 'messages))
              (check-current)
              (hermes-chat--dashboard-record-session client result)
              (check-current)
              (setq hermes-chat--title (hermes-transport--get result 'title))
              (hermes-chat--insert-local-status
               (format "Branch %s of %s" hermes-chat--session-id
                       (hermes-transport--get result 'parent)) 'done)
              (check-current)
              (hermes-chat--refresh-buffer-name))
            (check-current)
            (setq accepted t)
            child)
        (unless accepted
          (when (child-current-p) (kill-buffer child)))))))

(defun hermes-chat-branch (&optional name)
  "Branch the attached conversation, optionally assigning NAME to the child.
Open the returned child with authoritative history; retain the parent and its
draft.  An uncertain failure may have created a backend child: inspect sessions
before retrying.  Never invoke a slash worker or automatically replay a branch."
  (interactive nil hermes-chat-mode)
  (hermes-chat--ensure-submit-allowed)
  (when (hermes-chat--active-turn-p)
    (user-error "Wait for the active turn before branching"))
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "No attached conversation to branch"))
  (let* ((buffer (current-buffer))
         (client hermes-chat--dashboard-client)
         (connection (hermes-dashboard-transport-client-generation client))
         (parent hermes-chat--session-id)
         (input (hermes-chat-input-string))
         (tick (buffer-chars-modified-tick))
         (owner (hermes-chat--command-start))
         (context (hermes-chat--command-context client owner)))
    (cl-labels
        ((current-p ()
           (and (hermes-chat--command-context-current-p context)
                (= connection (hermes-dashboard-transport-client-generation client))))
         (failed (message)
           (hermes-chat--in-buffer buffer
             (hermes-chat--command-finish
              context (lambda ()
                        (hermes-chat--command-error
                         (concat message "; child may exist; inspect sessions before retrying")))))))
      (condition-case err
          (hermes-dashboard-transport-session-branch
           client :session-id (plist-get context :session-id)
           :name (hermes-transport--non-empty-string name)
           :reject #'failed
           :resolve
           (lambda (result)
             (hermes-chat--in-buffer buffer
               (if (not (current-p))
                   (hermes-chat--command-stop owner)
                 (condition-case err
                     (progn
                       (unless (and (hermes-chat--branch-receipt-p result parent)
                                    (not (equal (hermes-transport--get result 'session_id)
                                                (plist-get context :session-id))))
                         (error "Malformed branch receipt"))
                       (let ((child (hermes-chat--adopt-branch client result #'current-p)))
                         (hermes-chat--command-stop owner)
                         (when (and (= tick (buffer-chars-modified-tick))
                                    (equal (car (hermes-chat--parse-slash input)) "branch"))
                           (hermes-chat--delete-input-tail))
                         (pop-to-buffer-same-window child)))
                   ((error quit) (failed (error-message-string err))))))))
        ((error quit) (failed (error-message-string err)))))))

(defun hermes-chat--project-workspace (root instance start-mode)
  "Return project ROOT as a gateway cwd when INSTANCE shares this filesystem.
Spawned START-MODE and loopback gateways can resolve local roots; others
keep their backend default."
  (and root
       (not (file-remote-p root))
       (or (eq start-mode 'spawn)
           (hermes-dashboard-transport--loopback-host-p
            (url-host (url-generic-parse-url (hermes-instance-url instance)))))
       (directory-file-name root)))

(defun hermes-chat--new-buffer (&optional profile title instance pinned-url)
  "Create, display, and return a fresh chat buffer.
PROFILE selects the agent profile, TITLE pins a manual title, and INSTANCE is
the owning Hermes instance.  A nil INSTANCE is resolved from the current
context.  Non-nil PINNED-URL retains an explicit backend across reconnect.
PROFILE nil means the dashboard default; a non-empty TITLE pins a manual title.
Buffer names identify the instance, profile, and project; TITLE stays
session metadata.  This is the single side-effecting constructor every new-chat
entry point funnels through."
  (let* ((directory default-directory)
         (project-root hermes-chat--project-chat-root)
         (instance (or instance (hermes-instance-resolve)))
         (instance (if pinned-url
                       (cons (copy-sequence (car instance))
                             (copy-sequence (cdr instance)))
                     instance))
         (start-mode (hermes-chat--instance-start-mode instance))
         (profile (hermes-chat--clean-profile profile))
         (profile (and profile (copy-sequence profile)))
         (pinned-url (and pinned-url (copy-sequence pinned-url)))
         (title (hermes-transport--non-empty-string
                 (and title (string-trim title))))
         (buffer (generate-new-buffer hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory directory)
      ;; Publish captured authority before native hooks can acquire a client.
      (if pinned-url (delay-mode-hooks (hermes-chat-mode)) (hermes-chat-mode))
      (hermes-buffer--claim 'hermes-chat-mode)
      (setq hermes-instance instance
            hermes-chat--launch-project-root project-root
            ;; Project and local launches deliberately place the chat; an
            ;; unselected remote cwd stays backend-owned.
            hermes-chat--cwd-explicit-p (or (and project-root t)
                                          (eq start-mode 'spawn))
            hermes-chat--pinned-url (and pinned-url (copy-sequence pinned-url))
            hermes-chat--resolved-start-mode start-mode
            hermes-chat--working-directory
            (or (hermes-chat--project-workspace project-root instance start-mode)
                (and (eq start-mode 'spawn) directory))
            hermes-chat--profile profile)
      (when pinned-url (run-mode-hooks))
      (hermes-chat--restore-draft-runtime)
      (when title
        (setq hermes-chat--title title
              hermes-chat--title-manual-p t))
      (rename-buffer
       (hermes-chat--buffer-name profile instance) t))
    (pop-to-buffer-same-window buffer)
    (goto-char (or (hermes-chat--input-position) (point-max)))
    buffer))

(defun hermes-chat--profile-name (profile)
  "Return PROFILE's non-empty profile name, or nil."
  (and-let* ((name (hermes-transport--scalar-string
                    (hermes-transport--get profile 'name)))
             (trimmed (string-trim name))
             ((not (string-empty-p trimmed))))
    trimmed))

(defun hermes-chat--profile-default-p (profile)
  "Return non-nil when PROFILE denotes the dashboard default profile."
  (or (hermes-transport--get profile 'is_default)
      (equal (hermes-chat--profile-name profile) "default")))

(defun hermes-chat--profile-model-label (profile)
  "Return provider/model label for PROFILE, or nil."
  (let ((provider (hermes-transport--scalar-string
                   (hermes-transport--get profile 'provider)))
        (model (hermes-transport--scalar-string
                (hermes-transport--get profile 'model))))
    (cond
     ((and provider model) (format "%s/%s" provider model))
     (model model))))

(defun hermes-chat--draft-profile-row (payload)
  "Return this draft's selected profile row from dashboard PAYLOAD."
  (let ((name (or hermes-chat--profile "default")))
    (cl-find-if
     (lambda (profile)
       (or (equal (hermes-chat--profile-name profile) name)
           (and (equal name "default")
                (hermes-chat--profile-default-p profile))))
     (hermes-transport--get payload 'profiles))))

(defun hermes-chat--restore-draft-runtime ()
  "Restore pending or profile runtime state in this fresh draft's header."
  (let* ((instance (if hermes-chat--pinned-url hermes-instance
                     (hermes-instance-resolve)))
         (hermes-dashboard-transport-url
          (or hermes-chat--pinned-url (hermes-instance-url instance)))
         (profile-model
          (unless hermes-chat--dashboard-create-model
            (when-let* ((payload
                         (hermes-dashboard-transport-cached-profile-list))
                        (profile (hermes-chat--draft-profile-row payload)))
              (hermes-transport--scalar-string
               (hermes-transport--get profile 'model))))))
    (setq hermes-chat--model
          (or hermes-chat--dashboard-create-model profile-model))
    (when hermes-chat--dashboard-create-reasoning-effort
      (setq hermes-chat--runtime-flags
            (plist-put hermes-chat--runtime-flags :reasoning-effort
                       hermes-chat--dashboard-create-reasoning-effort)))
    (when hermes-chat--dashboard-create-fast-p
      (setq hermes-chat--runtime-flags
            (plist-put hermes-chat--runtime-flags :fast t)))
    (force-mode-line-update)))

(defun hermes-chat--profile-less-p (left right)
  "Return non-nil when LEFT dashboard profile should sort before RIGHT."
  (let ((left-default (hermes-chat--profile-default-p left))
        (right-default (hermes-chat--profile-default-p right)))
    (cond
     ((and left-default (not right-default)) t)
     ((and right-default (not left-default)) nil)
     (t (string-lessp (downcase (hermes-chat--profile-name left))
                      (downcase (hermes-chat--profile-name right)))))))

(defun hermes-chat--profile-candidates (payload)
  "Return sorted (NAME . MODEL-LABEL) candidates from dashboard PAYLOAD.
MODEL-LABEL is the profile's provider/model string, or nil when unknown."
  (mapcar (lambda (profile)
            (cons (hermes-chat--profile-name profile)
                  (hermes-chat--profile-model-label profile)))
          (sort (cl-remove-if-not
                 #'hermes-chat--profile-name
                 (or (hermes-transport--get payload 'profiles) '()))
                #'hermes-chat--profile-less-p)))

(defun hermes-chat--profile-annotation-function (candidates)
  "Return a completion `:annotation-function' over CANDIDATES.
CANDIDATES is a (NAME . MODEL-LABEL) alist; the annotation shows the model."
  (lambda (name)
    (when-let* ((model (cdr (assoc name candidates))))
      (concat "  " (propertize model 'face 'shadow)))))

(defun hermes-chat--existing-dashboard-client ()
  "Return a live dashboard client for the current Hermes instance, or nil."
  (when-let* ((instance (hermes-instance-context)))
    (cl-some (lambda (buffer)
               (with-current-buffer buffer
                 (and (derived-mode-p 'hermes-chat-mode)
                      (equal hermes-instance instance)
                      (hermes-chat--dashboard-client-live-p
                       hermes-chat--dashboard-client)
                      hermes-chat--dashboard-client)))
             (buffer-list))))

(defun hermes-chat--profile-list-payload ()
  "Return cached dashboard profiles and revalidate them asynchronously.
`hermes' warms a per-URL profile cache on launch (see
`hermes-dashboard-transport-profile-list-async').  When an existing client is
available, dispatch a best-effort refresh before returning the current cache, so
`hermes-chat--read-profile' can open completion immediately and the next call
sees fresh candidates.  A cold cache still returns nil without blocking."
  (let ((cached (hermes-dashboard-transport-cached-profile-list)))
    (when-let* ((client (hermes-chat--existing-dashboard-client)))
      (ignore-errors
        (hermes--promise-catch
         (hermes-dashboard-transport-profile-list-async client)
         #'ignore)))
    cached))

(defun hermes-chat--read-raw-profile (&optional notice)
  "Read a raw Hermes profile name with the default-profile prompt.
When NOTICE is non-nil, include it in the prompt so fallback context remains
visible while reading."
  (read-string (if notice
                   (format "%s; profile (blank for default): " notice)
                 "Profile (blank for default): ")))

(defun hermes-chat--read-profile ()
  "Read a Hermes profile name, using dashboard metadata when available."
  (condition-case err
      (let ((candidates (hermes-chat--profile-candidates
                         (hermes-chat--profile-list-payload))))
        (if candidates
            (let ((completion-extra-properties
                   (list :annotation-function
                         (hermes-chat--profile-annotation-function candidates))))
              (hermes-chat--clean-profile
               (completing-read "Profile (blank for default): "
                                (mapcar #'car candidates) nil nil)))
          (let ((notice "No dashboard profiles available"))
            (message "Hermes: %s; enter a profile name manually" notice)
            (hermes-chat--read-raw-profile notice))))
    (error
     (let ((notice (format "Profile list unavailable: %s"
                           (error-message-string err))))
       (message "Hermes: %s" notice)
       (hermes-chat--read-raw-profile notice)))))

(defun hermes-chat--history-entry (message)
  "Return a chat entry for a resumed history MESSAGE, or nil to skip it.
Use the backend's display projection; hidden rows return nil."
  (when-let* ((projection (hermes-transport-history-projection message)))
    (let ((role (hermes-transport--scalar-string
                 (hermes-transport--get message 'role)))
          (text (hermes-transport--non-empty-string
                 (hermes-transport-history-text projection))))
      (pcase role
        ("user"
         (and text
              (let ((entry (hermes-chat--make-entry 'user text 'done)))
                (when-let* ((row (hermes-transport-row-id
                                  (hermes-transport--get message 'row_id))))
                  (setq entry (plist-put entry :turn-row row)))
                (if-let* ((row-id (hermes-transport-history-row-id message)))
                    (plist-put entry :row-id row-id)
                  entry))))
        ("assistant" (and text (hermes-chat--make-entry 'assistant text 'done)))
        ("tool"
         (hermes-chat--make-entry
          'tool
          (hermes-chat--tool-head
           (or (hermes-transport--scalar-string
                (hermes-transport--get message 'name))
               "tool")
           (hermes-transport--scalar-string
            (hermes-transport--get message 'context)))
          'done))))))

(defun hermes-chat--history-entries (message)
  "Return the chat entries for a resumed history MESSAGE, in order.
An assistant row's reasoning precedes its reply as a collapsed entry."
  (let ((reasoning (and (equal (hermes-transport--get message 'role) "assistant")
                        (hermes-transport--non-blank-string
                         (plist-get (hermes-transport-history-projection message)
                                    :reasoning)))))
    (delq nil (list (and reasoning
                         (hermes-chat--make-entry 'commentary reasoning 'done))
                    (hermes-chat--history-entry message)))))

(defun hermes-chat--render-history (messages)
  "Insert prior MESSAGES (from `session.resume') into the transcript."
  (hermes-chat--render-history-entries
   (mapcan #'hermes-chat--history-entries messages)))

(defun hermes-chat--render-retained-failure (result)
  "Rebuild the failed turn that resume RESULT retained, if any.
Only an idle session's snapshot is a settled failure; a live one belongs
to the running turn."
  (when-let* (((not (hermes-chat--dashboard-result-live-turn-p result)))
              (retained (hermes-transport-retained-turn-failure
                         (hermes-transport--get result 'inflight))))
    (hermes-chat--render-history-entries
     (hermes-chat-failure-retained-entries
      retained
      (cl-loop for node = (ewoc-nth hermes-chat--ewoc -1)
               then (ewoc-prev hermes-chat--ewoc node)
               while node
               for entry = (ewoc-data node)
               when (eq (plist-get entry :role) 'user)
               return (plist-get entry :content))))))

(defun hermes-chat--render-history-entries (entries)
  "Insert resumed history ENTRIES into the transcript."
  (let ((owner hermes-chat--session-bootstrap))
    (dolist (entry entries)
      ;; Retain exact entries before insertion, which can run rendering hooks.
      (when owner (push entry (plist-get owner :history-entries)))
      ;; Start the change group inside the undo-disabled transcript boundary.
      ;; EWOC links before printing; retry retires a failed, now empty node.
      (hermes-chat--preserve-input-point
       (let ((inhibit-read-only t)
             (buffer-undo-list t))
         (hermes-chat--register-node
          entry (atomic-change-group (ewoc-enter-last hermes-chat--ewoc entry)))))
      (hermes-chat--notify-state-change))))

(defun hermes-chat--clear-partial-history (owner)
  "Remove only history entries retained by the current bootstrap OWNER."
  (when (hermes-chat--dashboard-bootstrap-current-p owner)
    (let ((entries (plist-get owner :history-entries)))
      ;; A printer can fail after EWOC links a node but before ID registration.
      ;; Filter by entry identity rather than relying on the node table.
      (hermes-chat--preserve-input-point
       (let ((inhibit-read-only t)
             (buffer-undo-list t))
         (hermes-chat--preserve-readers
           (ewoc-filter hermes-chat--ewoc
                        (lambda (entry) (not (memq entry entries)))))))
      (dolist (entry entries) (remhash (plist-get entry :id) hermes-chat--nodes))
      (setf (plist-get owner :history-entries) nil))))

(defun hermes-chat--restore-session-history (client owner result)
  "Restore CLIENT's history RESULT for OWNER before binding live output."
  (hermes-chat--dashboard-record-session client result)
  (hermes-chat--clear-partial-history owner)
  (hermes-chat--render-history (hermes-transport--get result 'messages))
  (hermes-chat--render-retained-failure result)
  (when (hermes-chat--dashboard-result-live-turn-p result)
    (hermes-chat--dashboard-restore-inflight-turn client))
  (hermes-chat--dashboard-restore-pending-clarify result)
  (when (hermes-chat--dashboard-result-live-turn-p result)
    (hermes-chat--dashboard-bind-stream-callback
     client hermes-chat--pending-assistant-id))
  (setq hermes-chat--restored-history
        (cons (copy-sequence hermes-chat--session-id) (copy-tree result t))))

(defun hermes-chat--load-session-history (buffer)
  "Resume BUFFER's session and hydrate history before draining queued input."
  (with-current-buffer buffer
    (let* ((session hermes-chat--session-id)
           recorded-session
           (lifetime hermes-chat--lifecycle-generation)
           (retry hermes-chat--session-bootstrap)
           (previous-client hermes-chat--dashboard-client)
           (generation (hermes-chat--next-transport-generation))
           (client
            (condition-case err
                (hermes-chat--dashboard-start
                 (hermes-chat--transport-callback buffer nil t generation))
              ((error quit)
               (hermes-chat--in-lifetime buffer lifetime
                 ;; Replacement releases the old client and bootstrap before
                 ;; acquisition.  Retain only this exact failed history demand.
                 (when (and (eq (plist-get retry :kind) 'history)
                            (eq (plist-get retry :phase) 'failed)
                            (equal session hermes-chat--session-id)
                            (hermes-chat--current-transport-generation-p generation)
                            (or (null hermes-chat--session-bootstrap)
                                (eq retry hermes-chat--session-bootstrap))
                            (or (null hermes-chat--dashboard-client)
                                (eq previous-client hermes-chat--dashboard-client)))
                   (setq hermes-chat--session-bootstrap retry)))
               (signal (car err) (cdr err)))))
           (owner
            (setq hermes-chat--session-bootstrap
                  (append (list :history-entries (plist-get retry :history-entries))
                          (hermes-chat--dashboard-begin-bootstrap client 'history nil)))))
      (cl-labels
          ((owned-p ()
             (and (hermes-chat--dashboard-bootstrap-current-p owner)
                  (eq (plist-get owner :phase) 'preflight)
                  (or (equal session hermes-chat--session-id)
                      (and recorded-session
                           (equal recorded-session hermes-chat--session-id)))
                  (hermes-chat--current-transport-generation-p generation)))
           (current-p ()
             (and (owned-p)
                  (= (plist-get owner :queue-connection)
                     (hermes-dashboard-transport-client-generation client))))
           (failed (message)
             (hermes-chat--in-buffer buffer
               ;; Stop/reconnect retires the connection before rejecting reads.
               ;; Only this exact owner may settle; success still needs its socket.
               (when (owned-p)
                 (setf (plist-get owner :phase) 'failed)
                 (hermes-chat--insert-local-status
                  (format "Could not load Hermes session history: %s; input retained; Send retries"
                          message)
                  'error)))))
        (condition-case err
            (hermes-dashboard-transport-session-resume
             client session :cols (hermes-chat--dashboard-cols)
             :profile hermes-chat--profile
             :resolve
             (lambda (result)
               (hermes-chat--in-buffer buffer
                 (when (current-p)
                   (let (restored)
                     (unwind-protect
                         (progn
                           ;; Recording can commit the canonical durable ID
                           ;; before a later projection or rendering error.
                           (setq recorded-session
                                 (hermes-chat--dashboard-stored-id-from-result
                                  client result
                                  (hermes-chat--dashboard-active-id-from-result client result)))
                           (hermes-chat--restore-session-history client owner result)
                           (setq restored t))
                       ;; The transport contains callback errors and quits after
                       ;; taking the RPC; only this callback can settle its read.
                       (unless restored (failed "Restoration interrupted")))
                     (when (eq owner hermes-chat--session-bootstrap)
                       (setq hermes-chat--session-bootstrap nil)
                       (hermes-chat--drain-queued-message))))))
             :reject #'failed)
          (error (failed (error-message-string err))))))))

;;; Authoritative transcript reconciliation

(defvar-local hermes-chat--history-reconcile nil
  "Owner of a pending transcript reconciliation, or nil.
A plist with the :client, :session and :lifetime that requested it, the
:known node table at that time, and a :phase that is `wait' until the chat
is idle and `read' while `session.history' is in flight.  :read-nodes is
the node table when that read started, to tell whether a turn raced it.")

(defun hermes-chat--bind-row-id (entry-id row-id &optional address-p)
  "Record durable ROW-ID as the turn identity of user entry ENTRY-ID.
With ADDRESS-P, ROW-ID also addresses the turn for editing.  Do nothing
unless both exist."
  (when-let* ((row-id (hermes-transport-row-id row-id))
              (node (and hermes-chat--nodes
                         (gethash entry-id hermes-chat--nodes)))
              (entry (ewoc-data node)))
    (when (eq (plist-get entry :role) 'user)
      (setq entry (plist-put entry :turn-row row-id))
      (ewoc-set-data node (if address-p (plist-put entry :row-id row-id) entry)))))

(defun hermes-chat--request-history-reconcile ()
  "Repaint the transcript from backend history once this chat is idle.
Use after the backend rewinds the session, so the transcript never keeps
turns the session no longer has nor drops any it still has."
  (when (and hermes-chat--dashboard-client hermes-chat--dashboard-active-session-id)
    (setq hermes-chat--history-reconcile
          (list :phase 'wait
                :client hermes-chat--dashboard-client
                :session hermes-chat--dashboard-active-session-id
                :lifetime hermes-chat--lifecycle-generation
                :known (copy-hash-table hermes-chat--nodes)
                :read-nodes nil))
    (hermes-chat--history-reconcile-maybe)))

(defun hermes-chat--history-reconcile-current-p (owner)
  "Return non-nil when OWNER still belongs to this chat's live session."
  (and owner
       (eq owner hermes-chat--history-reconcile)
       (hermes-chat--current-lifetime-p (plist-get owner :lifetime))
       (eq (plist-get owner :client) hermes-chat--dashboard-client)
       (equal (plist-get owner :session) hermes-chat--dashboard-active-session-id)))

(defun hermes-chat--history-reconcile-maybe ()
  "Read history for a waiting reconciliation once no turn is active."
  (let ((owner hermes-chat--history-reconcile))
    (cond
     ((null owner))
     ((not (hermes-chat--history-reconcile-current-p owner))
      (setq hermes-chat--history-reconcile nil))
     ((and (eq (plist-get owner :phase) 'wait)
           (not (hermes-chat--active-turn-p)))
      (setf (plist-get owner :phase) 'read
            (plist-get owner :read-nodes) (copy-hash-table hermes-chat--nodes))
      (let ((buffer (current-buffer)))
        (condition-case err
            (hermes-dashboard-transport-session-history
             (plist-get owner :client) (plist-get owner :session)
             :resolve (lambda (result)
                        (hermes-chat--in-buffer buffer
                          (hermes-chat--history-reconciled owner result)))
             :reject (lambda (message)
                       (hermes-chat--in-buffer buffer
                         (hermes-chat--history-reconcile-failed owner message))))
          (error (hermes-chat--history-reconcile-failed
                  owner (error-message-string err)))))))))

(defun hermes-chat--history-reconcile-failed (owner message)
  "Report MESSAGE for reconciliation OWNER, leaving the transcript as is."
  (when (hermes-chat--history-reconcile-current-p owner)
    (setq hermes-chat--history-reconcile nil)
    (hermes-chat--insert-local-status
     (format "Could not refresh the transcript: %s" message) 'error)))

(defun hermes-chat--history-read-raced-p (owner)
  "Return non-nil when a turn ran since OWNER's history read started.
Every turn adds transcript entries; local status lines do not count."
  (let ((before (plist-get owner :read-nodes)))
    (catch 'raced
      (maphash (lambda (id node)
                 (unless (or (gethash id before)
                             (eq (plist-get (ewoc-data node) :role) 'status))
                   (throw 'raced t)))
               hermes-chat--nodes)
      nil)))

(defun hermes-chat--history-reconciled (owner result)
  "Repaint the transcript from history RESULT read for OWNER.
A turn that ran during the read, even one already finished, makes RESULT
stale: read again once the chat is idle."
  (when (hermes-chat--history-reconcile-current-p owner)
    (if (or (hermes-chat--active-turn-p)
            (hermes-chat--history-read-raced-p owner))
        (progn (setf (plist-get owner :phase) 'wait)
               (hermes-chat--history-reconcile-maybe))
      (setq hermes-chat--history-reconcile nil)
      (hermes-chat--repaint-history (hermes-transport--get result 'messages)
                                    (plist-get owner :known)))))

(defun hermes-chat--split-turns (items entry-of)
  "Split ITEMS into one (ROW . ITEMS) per turn, in transcript order.
ENTRY-OF maps an item to its chat entry.  A user entry starts a turn whose
ROW is the entry's durable :turn-row, or nil without one; items before the
first user entry form the turn `prefix'.  Pure."
  (let (turns)
    (dolist (item items)
      (let ((entry (funcall entry-of item)))
        (cond ((eq (plist-get entry :role) 'user)
               (push (list (plist-get entry :turn-row) item) turns))
              (turns (push item (cdar turns)))
              (t (push (list 'prefix item) turns)))))
    (mapcar (lambda (turn) (cons (car turn) (reverse (cdr turn))))
            (nreverse turns))))

(defun hermes-chat--ewoc-nodes ()
  "Return the transcript's EWOC nodes in order."
  (let ((node (ewoc-nth hermes-chat--ewoc 0)) nodes)
    (while node
      (push node nodes)
      (setq node (ewoc-next hermes-chat--ewoc node)))
    (nreverse nodes)))

(defun hermes-chat--repaint-history (messages known)
  "Reconcile the transcript with history MESSAGES by durable turn identity.
Turns the history still has keep their entries, with live worker cards and
task nodes; turns it no longer has are removed; turns shown here without
an identity, or not at all, are inserted from the history in its order.
Local status lines added since KNOWN, a node table snapshot, stay, so the
notice that caused the repaint remains visible."
  (let ((wanted (hermes-chat--split-turns
                 (mapcan #'hermes-chat--history-entries messages) #'identity))
        (kept (make-hash-table :test #'eql))
        successor)
    (hermes-chat--preserve-input-point
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (hermes-chat--preserve-readers
          (dolist (turn (hermes-chat--split-turns (hermes-chat--ewoc-nodes)
                                                  #'ewoc-data))
            (if (and (car turn)
                     (not (gethash (car turn) kept))
                     (or (eq (car turn) 'prefix)
                         (assoc (car turn) wanted)))
                (puthash (car turn) (cadr turn) kept)
              (dolist (node (cdr turn))
                (let ((entry (ewoc-data node)))
                  (unless (and (eq (plist-get entry :role) 'status)
                               (not (gethash (plist-get entry :id) known)))
                    (hermes-chat--stream-format-retire (plist-get entry :id))
                    (remhash (plist-get entry :id) hermes-chat--nodes)
                    (ewoc-delete hermes-chat--ewoc node))))))
          ;; Walk backwards so each missing turn lands before its successor.
          (dolist (turn (reverse wanted))
            (setq successor
                  (or (and (car turn) (gethash (car turn) kept))
                      (let (first)
                        (dolist (entry (cdr turn) first)
                          (let ((node (if successor
                                          (ewoc-enter-before hermes-chat--ewoc
                                                             successor entry)
                                        (ewoc-enter-last hermes-chat--ewoc entry))))
                            (hermes-chat--register-node entry node)
                            (setq first (or first node)))))
                      successor))))))
    (hermes-chat--notify-state-change)))

;;; Editing earlier turns

(defvar-local hermes-chat--edit-turn nil
  "The earlier user turn being edited in the composer, or nil.
A plist with the turn's durable :row-id, the composer :draft it replaced,
and the :client, :session and :lifetime that own the edit.")

(defun hermes-chat--user-turn-at (position)
  "Return the user entry at POSITION, or signal a `user-error'."
  (let* ((node (and (not (hermes-chat--point-in-input-p))
                    (hermes-chat--entry-node-at position)))
         (entry (and node (ewoc-data node))))
    (unless (eq (plist-get entry :role) 'user)
      (user-error "Point is not on one of your messages"))
    entry))

(defun hermes-chat--rewind-target (verb)
  "Return the user entry at point that may be rewound to for VERB."
  (let ((entry (hermes-chat--user-turn-at (point))))
    (when (hermes-chat--active-turn-p)
      (user-error "Hermes is working; wait for this turn to end before %s" verb))
    (unless (hermes-chat--dashboard-session-attached-p)
      (user-error "No live Hermes session to rewind"))
    (unless (plist-get entry :row-id)
      (user-error "This message has no saved address; reload the session to %s it"
                  (if (equal verb "editing") "edit" "rerun")))
    entry))

(defun hermes-chat--rewind-scope (row-id)
  "Return (FIRST-P . LATER) for the user turn ROW-ID, or nil when absent.
FIRST-P is non-nil when no user turn precedes it; LATER counts user turns
after it."
  (let ((users (seq-filter (lambda (entry) (eq (plist-get entry :role) 'user))
                           (ewoc-collect hermes-chat--ewoc #'always))))
    (when-let* ((tail (seq-drop-while
                       (lambda (entry) (not (eql (plist-get entry :row-id) row-id)))
                       users)))
      (cons (eq tail users) (1- (length tail))))))

(defun hermes-chat--confirm-rewind (prompt scope)
  "Ask with PROMPT whether to rewind SCOPE, a `hermes-chat--rewind-scope'."
  (y-or-n-p
   (format "%s%s? " prompt
           (pcase (cdr scope)
             (0 "")
             (1 ", removing 1 later message")
             (n (format ", removing %d later messages" n))))))

(defun hermes-chat--submit-rewind (content row-id scope)
  "Submit CONTENT in place of user turn ROW-ID covering SCOPE."
  (let ((hermes-chat--submit-truncation
         (list :row-id row-id
               :deep (and (> (cdr scope) 0) t)
               :empty (car scope))))
    (hermes-chat--submit-content content)))

(defun hermes-chat-edit-turn ()
  "Edit the message at point in the composer and resend it from there.
Sending rewinds the conversation to just before this message.  Cancel with
\\[hermes-chat-edit-cancel] to restore the previous draft."
  (interactive nil hermes-chat-mode)
  (let ((entry (hermes-chat--rewind-target "editing")))
    (setq hermes-chat--edit-turn
          (append (list :draft (if hermes-chat--edit-turn
                                   (plist-get hermes-chat--edit-turn :draft)
                                 (hermes-chat-input-string)))
                  (hermes-chat--rewind-owner (plist-get entry :row-id))))
    (hermes-chat--replace-input-tail (plist-get entry :content))
    (force-mode-line-update)
    (message "%s" (substitute-command-keys
                   "Editing an earlier message: \\[hermes-chat-send] resends, \\[hermes-chat-edit-cancel] cancels"))))

(defun hermes-chat-edit-cancel ()
  "Stop editing an earlier message and restore the previous draft."
  (interactive nil hermes-chat-mode)
  (let ((edit hermes-chat--edit-turn))
    (unless edit (user-error "Not editing an earlier message"))
    (setq hermes-chat--edit-turn nil)
    (hermes-chat--replace-input-tail (plist-get edit :draft))
    (force-mode-line-update)))

(defun hermes-chat--editing-p ()
  "Return non-nil while the composer edits an earlier message."
  (and hermes-chat--edit-turn t))

(defun hermes-chat--send-edit (content)
  "Resend edited CONTENT in place of the turn being edited.
Return non-nil when the submission starts."
  (let* ((edit hermes-chat--edit-turn)
         (row-id (plist-get edit :row-id))
         (scope (hermes-chat--rewind-scope row-id)))
    (unless (and (hermes-chat--current-lifetime-p (plist-get edit :lifetime))
                 (eq (plist-get edit :client) hermes-chat--dashboard-client)
                 (equal (plist-get edit :session)
                        hermes-chat--dashboard-active-session-id)
                 scope)
      (setq hermes-chat--edit-turn nil)
      (force-mode-line-update)
      (user-error "The edited message is gone from this session; text kept as a draft"))
    (when (hermes-chat--active-turn-p)
      (user-error "Hermes is working; wait for this turn to end before resending"))
    (when (hermes-chat--confirm-rewind "Replace this message" scope)
      ;; Anything can happen while the user answers: recheck before
      ;; touching the composer, so a refusal keeps the edited text.
      (unless (eq edit hermes-chat--edit-turn)
        (user-error "The edit was cancelled; text kept as a draft"))
      (hermes-chat--rewind-revalidate edit scope)
      (hermes-chat--delete-input-tail)
      (setq hermes-chat--edit-turn nil)
      (force-mode-line-update)
      (hermes-chat--submit-rewind content row-id scope))))

(defun hermes-chat--rewind-owner (row-id)
  "Return the ownership plist for rewinding to the user turn ROW-ID now."
  (list :row-id row-id
        :client hermes-chat--dashboard-client
        :session hermes-chat--dashboard-active-session-id
        :lifetime hermes-chat--lifecycle-generation))

(defun hermes-chat--rewind-revalidate (owner scope)
  "Signal a `user-error' unless OWNER may still rewind over SCOPE.
OWNER is a `hermes-chat--rewind-owner' captured before asking the user and
SCOPE the `hermes-chat--rewind-scope' they confirmed."
  (unless (and (hermes-chat--current-lifetime-p (plist-get owner :lifetime))
               (eq (plist-get owner :client) hermes-chat--dashboard-client)
               (equal (plist-get owner :session)
                      hermes-chat--dashboard-active-session-id))
    (user-error "The session changed while asking; nothing was sent"))
  (when (hermes-chat--active-turn-p)
    (user-error "Hermes started a turn while asking; nothing was sent"))
  (unless (equal (hermes-chat--rewind-scope (plist-get owner :row-id)) scope)
    (user-error "The conversation changed while asking; nothing was sent")))

(defun hermes-chat-restore-turn ()
  "Rerun the conversation from the message at point.
The message is sent again unchanged after the conversation is rewound to
just before it; later messages are removed."
  (interactive nil hermes-chat-mode)
  (let* ((entry (hermes-chat--rewind-target "rerunning"))
         (owner (hermes-chat--rewind-owner (plist-get entry :row-id)))
         (scope (hermes-chat--rewind-scope (plist-get owner :row-id))))
    (when (hermes-chat--confirm-rewind "Rerun from this message" scope)
      (hermes-chat--rewind-revalidate owner scope)
      (hermes-chat--submit-rewind (plist-get entry :content)
                                  (plist-get owner :row-id) scope))))

(defun hermes-chat--rewind-unavailable-p ()
  "Return non-nil when point is not on a user turn that may be rewound."
  (or (hermes-chat--active-turn-p)
      (not (ignore-errors
             (plist-get (hermes-chat--user-turn-at (point)) :row-id)))))

(defun hermes-chat--message-actions-p ()
  "Return non-nil when point is on a user turn or an edit is open."
  (or hermes-chat--edit-turn
      (and (ignore-errors (hermes-chat--user-turn-at (point))) t)))

(defun hermes-chat--send-during-history ()
  "Queue composer input behind history, retrying a failed read on Send."
  (let ((content (hermes-chat--trimmed-input))
        (retry-p (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed)))
    (when (hermes-chat--parse-slash content)
      (user-error "Wait for session history before sending a command"))
    (cond
     (hermes-chat--draft-images (hermes-chat--queue-image-draft))
     ((not (string-empty-p content))
      (hermes-chat--queue-content content)
      (hermes-chat--delete-input-tail)
      (hermes-chat--record-input-history content))
     ((not (and retry-p hermes-chat--queued-messages))
      (user-error "No Hermes input to send")))
    (when retry-p (hermes-chat--load-session-history (current-buffer)))))

(defun hermes-chat--resume-buffer (session-id &optional title profile instance bot-root pinned-url)
  "Return a new chat buffer for SESSION-ID that is not yet attached.
TITLE, PROFILE, INSTANCE, BOT-ROOT and PINNED-URL are as for
`hermes-chat-resume-session'.  The buffer acquires no client until its
history is loaded."
  (when (or (null session-id) (string-empty-p session-id))
    (user-error "No Hermes session id to resume"))
  (let* ((directory default-directory)
         (instance (or instance (hermes-instance-resolve)))
         (instance (if (or pinned-url bot-root)
                       (cons (copy-sequence (car instance))
                             (copy-sequence (cdr instance)))
                     instance))
         (pinned-url (or pinned-url (and bot-root (hermes-instance-url instance))))
         (pinned-url (and pinned-url (copy-sequence pinned-url)))
         (profile (and profile (copy-sequence profile)))
         (session-id (copy-sequence session-id))
         (bot-root (and bot-root (copy-sequence bot-root)))
         (start-mode (hermes-chat--instance-start-mode instance))
         (title (hermes-transport--non-empty-string
                 (and title (string-trim title))))
         (buffer (generate-new-buffer hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory directory)
      ;; Publish captured authority before native hooks can acquire a client.
      (if pinned-url (delay-mode-hooks (hermes-chat-mode)) (hermes-chat-mode))
      (hermes-buffer--claim 'hermes-chat-mode)
      (setq hermes-instance instance
            hermes-chat--launch-project-root nil
            hermes-chat--resolved-start-mode start-mode
            hermes-chat--working-directory
            (and (eq start-mode 'spawn) directory)
            hermes-chat--session-id session-id
            hermes-chat--bot-chat-root bot-root
            hermes-chat--pinned-url pinned-url
            hermes-chat--profile profile
            hermes-chat--title title)
      (when pinned-url (run-mode-hooks))
      (rename-buffer (hermes-chat--buffer-name profile instance) t))
    buffer))

(defun hermes-chat-resume-session (session-id &optional title profile instance bot-root pinned-url)
  "Open a Hermes chat buffer that resumes dashboard SESSION-ID.
TITLE, when given, records its server title metadata.  PROFILE selects its
owning profile, and INSTANCE selects its owning Hermes instance.  A nil
INSTANCE is resolved from the current context.  BOT-ROOT, when non-nil, is
the backend-confirmed canonical Bot Chat root and enables its /new policy.
PINNED-URL explicitly retains a verified backend through hooks and reconnect;
when omitted, BOT-ROOT uses INSTANCE's endpoint.  Ordinary chats stay unpinned.
Over the dashboard transport the prior messages are fetched and rendered; the
durable session continues on send."
  (interactive (list (read-string "Resume Hermes session id: ")))
  (let ((buffer (hermes-chat--resume-buffer
                 session-id title profile instance bot-root pinned-url)))
    (pop-to-buffer-same-window buffer)
    (when (hermes-chat--dashboard-default-transport-p)
      (hermes-chat--load-session-history buffer))
    (with-current-buffer buffer
      (goto-char (or (hermes-chat--input-position) (point-max))))
    buffer))

;;;; Session restore (desktop.el and warm-restart)

;; A restored chat carries its durable identity and unsent draft, never
;; the transcript: the backend owns history and replays it on resume.

(defvar warm-restart-passive)
(defvar warm-restart-activate-functions)

(defun hermes-chat--desktop-save (_dirname)
  "Return this chat's durable identity and draft as desktop.el data."
  (let ((input (hermes-chat--input-position))
        (string (lambda (value) (and value (substring-no-properties value)))))
    (list :version 1
          :session-id (funcall string hermes-chat--session-id)
          :title (funcall string hermes-chat--title)
          :title-manual-p hermes-chat--title-manual-p
          :profile (funcall string hermes-chat--profile)
          :instance (copy-tree hermes-instance)
          :pinned-url (funcall string hermes-chat--pinned-url)
          :bot-root (funcall string hermes-chat--bot-chat-root)
          :draft (hermes-chat-input-string)
          :point-offset (and input (>= (point) input) (- (point) input)))))

(defun hermes-chat--restore-point (buffer offset)
  "Move point in BUFFER, and its windows, OFFSET characters into the composer."
  (with-current-buffer buffer
    (let ((position (min (point-max)
                         (+ (or (hermes-chat--input-position) (point-max))
                            (or offset 0)))))
      (goto-char position)
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-point window position)))))

(defun hermes-chat-desktop-restore (_file name misc)
  "Recreate chat buffer NAME from desktop data MISC and return it.
The draft is restored as unsent composer text.  Attaching to the backend
waits for `warm-restart' activation, when the previous editor has gone, or
otherwise for the next command loop."
  (let* ((session (plist-get misc :session-id))
         (title (plist-get misc :title))
         (buffer
          (save-window-excursion
            (if session
                (hermes-chat--resume-buffer
                 session title (plist-get misc :profile) (plist-get misc :instance)
                 (plist-get misc :bot-root) (plist-get misc :pinned-url))
              (hermes-chat--new-buffer
               (plist-get misc :profile)
               (and (plist-get misc :title-manual-p) title)
               (plist-get misc :instance) (plist-get misc :pinned-url)))))
         (attach
          (lambda ()
            (when (buffer-live-p buffer)
              (hermes-chat--restore-point buffer (plist-get misc :point-offset))
              (with-current-buffer buffer
                (when (and session (hermes-chat--dashboard-default-transport-p))
                  (hermes-chat--load-session-history buffer)))))))
    (with-current-buffer buffer
      (when (plist-get misc :title-manual-p)
        (setq hermes-chat--title-manual-p t))
      (let ((draft (plist-get misc :draft)))
        (when (and draft (not (string-empty-p draft)))
          (goto-char (point-max))
          (insert draft)))
      (unless (string-equal (buffer-name) name)
        (rename-buffer name t)))
    (if (bound-and-true-p warm-restart-passive)
        (add-hook 'warm-restart-activate-functions attach t)
      (run-at-time 0 nil attach))
    buffer))

(defun hermes-chat--warm-restart-blocker ()
  "Return why this chat cannot be handed to another editor now, or nil."
  (cond ((hermes-chat--active-turn-p) "Hermes turn in progress")
        (hermes-chat--session-bootstrap "Hermes session still loading")
        (hermes-chat--queued-messages "queued Hermes messages not yet sent")
        ((> (hermes-chat--pending-prompt-count) 0)
         "Hermes prompt awaiting an answer")
        (hermes-chat--draft-images "image draft cannot be carried")))

(add-to-list 'desktop-buffer-mode-handlers
             '(hermes-chat-mode . hermes-chat-desktop-restore))

(defun hermes-chat-send ()
  "Send the current Hermes chat input.
Answer a pending clarification instead of starting a new turn.  For a batch,
answer only the next unanswered question; use `hermes-chat-respond-to-prompt'
to answer all remaining questions in the minibuffer.  During an explicit
interrupt or an observed application turn, queue ordinary input until that
turn settles instead of steering its response.
During initial resume, queue input until history loads; Send retries a failed
history read, including with empty input when a queued message is retained."
  (interactive nil hermes-chat-mode)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (unless (hermes-chat--point-in-input-p)
    (user-error "Point is not in the Hermes chat input area"))
  (hermes-chat--ensure-submit-allowed)
  (cond
   ((eq (plist-get hermes-chat--session-bootstrap :kind) 'history)
    (hermes-chat--send-during-history))
   ((and hermes-chat--edit-turn hermes-chat--draft-images)
    ;; An image draft is its own submission, so it would bypass the rewind.
    (user-error "An edited message cannot carry images; remove them first"))
   (hermes-chat--draft-images (hermes-chat--queue-image-draft))
   (t
    (let ((content (hermes-chat--trimmed-input))
          (clarify-key (hermes-chat--pending-clarify-key))
          sent-p)
      (when (string-empty-p content)
	(user-error "No Hermes input to send"))
      (setq sent-p
            (cond
             (clarify-key
              (hermes-chat--send-clarify-input clarify-key content)
              t)
             ((hermes-chat--parse-slash content)
              (hermes-chat--handle-slash-content content)
              t)
             (hermes-chat--edit-turn
              (hermes-chat--send-edit content))
             ((and (hermes-chat--active-turn-p)
                   (null hermes-chat--session-bootstrap)
                   (null hermes-chat--application-context)
                   (null hermes-chat--interrupted-assistant-id)
                   (hermes-chat--dashboard-session-attached-p)
                   (null hermes-chat--queued-messages))
              (when hermes-chat--busy-submit-context
		(user-error "Hermes is accepting the previous message"))
              (hermes-chat--delete-input-tail)
              (hermes-chat--submit-busy-dashboard-content content))
             ((or (hermes-chat--active-turn-p) hermes-chat--queued-messages)
              (hermes-chat--delete-input-tail)
              (hermes-chat--queue-content content)
              (hermes-chat--drain-queued-message)
              t)
             (t
              (hermes-chat--delete-input-tail)
              (hermes-chat--submit-content content))))
      (when sent-p
	(hermes-chat--record-input-history content))))))

;;; Attachments view

(defun hermes-chat--text-urls (text)
  "Return the URLs found in TEXT, in order of appearance."
  (let ((case-fold-search t) (start 0) urls)
    (while (and text (string-match goto-address-url-regexp text start))
      (push (match-string 0 text) urls)
      (setq start (match-end 0)))
    (nreverse urls)))

(defun hermes-chat--collect-urls (entries)
  "Return ordered, de-duplicated URLs from ENTRIES' content."
  (seq-uniq
   (mapcan (lambda (entry) (hermes-chat--text-urls (plist-get entry :content)))
           entries)))

(defvar-local hermes-chat-attachments--source nil
  "Chat buffer whose links populate this attachments buffer.")

(defun hermes-chat-attachments--follow (button)
  "Open BUTTON's URL in a browser."
  (browse-url (button-label button)))

(defun hermes-chat--attachments-revert (&rest _)
  "Re-collect links from the source chat buffer."
  (let ((source hermes-chat-attachments--source))
    (unless (buffer-live-p source)
      (user-error "Source chat buffer is gone"))
    (hermes-chat--render-attachments
     (with-current-buffer source
       (hermes-chat--collect-urls (hermes-chat--entries)))
     source)))

(define-derived-mode hermes-chat-attachments-mode special-mode "Hermes Attachments"
  "Major mode listing links collected from a Hermes chat transcript."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-chat--attachments-revert))

(defun hermes-chat--render-attachments (urls source)
  "Render URLS gathered from the SOURCE chat buffer, returning the buffer."
  (with-current-buffer (hermes-buffer--get "*Hermes Attachments*"
                                           #'hermes-chat-attachments-mode t)
    (setq hermes-chat-attachments--source source)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Links from %s\n\n" (buffer-name source)))
      (if (null urls)
          (insert "No links found.\n")
        (dolist (url urls)
          (insert-text-button url
                              'action #'hermes-chat-attachments--follow
                              'help-echo "Open link in browser"
                              'follow-link t)
          (insert "\n"))))
    (goto-char (point-min))
    (current-buffer)))

(defun hermes-chat-view-attachments ()
  "Display a buffer listing every link from the current chat transcript."
  (interactive nil hermes-chat-mode)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (pop-to-buffer
   (hermes-chat--render-attachments
    (hermes-chat--collect-urls (hermes-chat--entries))
    (current-buffer))))

;;; Renaming and switching chat buffers

(defun hermes-chat--clean-profile (profile)
  "Return PROFILE trimmed to a non-empty string, or nil for the default."
  (and profile
       (let ((trimmed (string-trim profile)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun hermes-chat--live-buffers ()
  "Return all live Hermes chat buffers in `buffer-list' order."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer (derived-mode-p 'hermes-chat-mode)))
   (buffer-list)))

(defun hermes-chat--project-root (&optional directory)
  "Return the project root for DIRECTORY, or its normalized directory.
When DIRECTORY is nil, use the current buffer's `default-directory'."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (project (project-current nil directory)))
    (file-name-as-directory
     (expand-file-name (if project (project-root project) directory)))))

(defun hermes-chat--project-buffers (root buffers)
  "Return members of BUFFERS whose launching project is ROOT."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (equal (if (local-variable-p 'hermes-chat--launch-project-root)
                  hermes-chat--launch-project-root
                (hermes-chat--project-root))
              root)))
   buffers))

(defun hermes-chat--adopt-project-root (buffer root)
  "Return BUFFER after adopting ROOT as its launching project."
  (with-current-buffer buffer
    (unless (local-variable-p 'hermes-chat--launch-project-root)
      (setq hermes-chat--launch-project-root root))
    (hermes-chat--refresh-buffer-name))
  buffer)

(defun hermes-chat--read-project-buffer (buffers)
  "Read one chat from project-local BUFFERS and return the exact buffer.
Names are mapped back through the buffers captured before prompting, and
the choice is checked like `hermes-chat--read-captured-chat'."
  (let* ((candidates (mapcar (lambda (buffer)
                               (cons (buffer-name buffer) buffer))
                             buffers))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name)
                  (hermes-chat--switch-annotation
                   name (cdr (assoc name candidates)))))))
    (hermes-chat--read-captured-chat
     "Project chat: " candidates (mapcar #'car candidates))))

;;;###autoload
(defun hermes-project-chat (&optional new)
  "Switch to a live chat for the current project, or create one.
With prefix argument NEW, always create another project chat."
  (interactive "P")
  (let* ((root (or (and (derived-mode-p 'hermes-chat-mode)
                        hermes-chat--launch-project-root)
                   (hermes-chat--project-root)))
         (hermes-chat--project-chat-root root)
         (buffers (and (not new)
                       (hermes-chat--project-buffers
                        root (hermes-chat--live-buffers))))
         (buffers (mapcar (lambda (buffer)
                            (hermes-chat--adopt-project-root buffer root))
                          buffers)))
    (cond
     ((null buffers)
      (let ((default-directory root))
        (call-interactively #'hermes-chat)))
     ((null (cdr buffers))
      (pop-to-buffer-same-window (car buffers)))
     (t
      (pop-to-buffer-same-window
       (hermes-chat--read-project-buffer buffers))))))

(defun hermes-chat--switch-project (buffer-or-name)
  "Return the project naming chat buffer BUFFER-OR-NAME, or nil."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (with-current-buffer buffer
      (cdr (hermes-chat--project-identity (hermes-chat--naming-directory))))))

(defun hermes-chat--switch-group (name transform &optional buffer)
  "Group chat buffer NAME by project; with TRANSFORM, return NAME unchanged.
Optional BUFFER is the chat captured for NAME; it wins over a name lookup."
  (if transform
      name
    (or (hermes-chat--switch-project (or buffer name)) "Other")))

(defun hermes-chat--switch-annotation (name &optional buffer)
  "Return a switcher annotation for chat buffer NAME.
Optional BUFFER is the chat captured for NAME; it wins over a name lookup.
Show the session title, the attention state, connection and activity, and
the gateway cwd while it differs from the chat's project root."
  (when-let* ((buffer (if (buffer-live-p buffer) buffer (get-buffer name))))
    (with-current-buffer buffer
      (let* ((title (and hermes-chat--title
                         (hermes-transport--non-empty-string
                          (hermes-session-title-chat-display hermes-chat--title))))
             (state (plist-get hermes-chat--status-state :status))
             (label (hermes-chat--header-status-label state))
             (attention (and (member label '("Approval requested"
                                             "Input requested" "Error"))
                             (propertize label 'face
                                         (hermes-chat--header-status-face state))))
             (detail (delq nil
                           (list (hermes-chat--dashboard-connection-label)
                                 (hermes-transport--non-empty-string
                                  (plist-get hermes-chat--status-state :activity))
                                 (hermes-chat--divergent-directory-label))))
             (parts (delq nil (list title attention
                                    (and detail
                                         (propertize (string-join detail " · ")
                                                     'face 'shadow))))))
        (and parts
             (concat "  " (string-join parts (propertize " · " 'face 'shadow))))))))

(defun hermes-chat--switch-candidates (buffers current)
  "Return (NAME . BUFFER) for chat BUFFERS in completion order, CURRENT last.
BUFFERS arrive in `buffer-list' order, most recently selected first."
  (mapcar (lambda (buffer) (cons (buffer-name buffer) buffer))
          (append (remq current buffers)
                  (and (memq current buffers) (list current)))))

(defun hermes-chat--switch-table (candidates)
  "Return a completion table of (NAME . BUFFER) CANDIDATES grouped by project.
Keep the given order instead of sorting alphabetically.  Group and
annotate through the captured buffers, not their current names."
  (let* ((names (mapcar #'car candidates))
         (metadata
          `(metadata
            (category . hermes-chat)
            (group-function
             . ,(lambda (name transform)
                  (hermes-chat--switch-group
                   name transform (cdr (assoc name candidates)))))
            (annotation-function
             . ,(lambda (name)
                  (hermes-chat--switch-annotation
                   name (cdr (assoc name candidates)))))
            (display-sort-function . identity)
            (cycle-sort-function . identity))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action names string predicate)))))

(defun hermes-chat--live-chat-p (buffer)
  "Return non-nil if BUFFER is a live chat that retains its constructor claim."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'hermes-chat-mode)
              (hermes-buffer--owned-p 'hermes-chat-mode)))))

(defun hermes-chat--read-captured-chat (prompt candidates collection)
  "Read a chat with PROMPT among (NAME . BUFFER) CANDIDATES and return BUFFER.
COLLECTION is the completion table offering the candidate names.  Names
are mapped back through CANDIDATES, captured before prompting, so a rename
or a foreign buffer taking a name while the reader is open never redirects
the choice.  Signal `user-error' if the chosen chat is no longer a live
chat after input."
  (let* ((name (completing-read prompt collection nil t))
         (buffer (cdr (assoc name candidates))))
    (unless (hermes-chat--live-chat-p buffer)
      (user-error "Hermes chat %s is no longer available" name))
    buffer))

(defun hermes-chat--read-switch-buffer ()
  "Read a chat buffer with completion and return the exact buffer object.
See `hermes-chat--read-captured-chat' for how the choice is checked."
  (let ((buffers (hermes-chat--live-buffers)))
    (unless buffers
      (user-error "No Hermes chat buffers"))
    (let ((candidates (hermes-chat--switch-candidates
                       buffers (current-buffer))))
      (hermes-chat--read-captured-chat
       "Hermes chat: " candidates (hermes-chat--switch-table candidates)))))

(defun hermes-switch-to-chat (buffer)
  "Switch to a Hermes chat BUFFER chosen with completion.
Candidates are exact buffer names grouped by project, most recently used
first and the current chat last, annotated with each chat's title and state."
  (interactive (list (hermes-chat--read-switch-buffer)))
  (unless (hermes-chat--live-chat-p buffer)
    (user-error "Not a live Hermes chat"))
  (pop-to-buffer-same-window buffer))

;; `hermes-sessions' is downstream of this file; its autoloaded browser
;; command is the one sanctioned upward reference.
(declare-function hermes-list-sessions "hermes-sessions" t t)
;; The package hub binds the optional unified palette into chat buffers.
(declare-function hermes-command-palette "hermes-command-palette")

(defun hermes-chat--usage-content (result)
  "Return display text for a `session.usage' RESULT."
  (let ((line (format "Usage: %s calls — input %s, output %s, total %s tokens"
                      (or (hermes-transport--get result 'calls) 0)
                      (or (hermes-transport--get result 'input) 0)
                      (or (hermes-transport--get result 'output) 0)
                      (or (hermes-transport--get result 'total) 0)))
        (credits (delq nil (mapcar #'hermes-chat--scalar-string
                                   (hermes-chat--listify
                                    (hermes-transport--get
                                     result 'credits_lines))))))
    (string-join (cons line credits) "\n")))

(defun hermes-chat--show-session-panel (fetch render)
  "Call RPC wrapper FETCH for this session and insert RENDER of its result.
FETCH takes CLIENT plus :session-id/:resolve/:reject; RENDER turns the
result into the transient status text shown in the transcript."
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "This Hermes chat has no live session"))
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-control-client))
        (generation hermes-chat--lifecycle-generation)
        (session-id hermes-chat--dashboard-active-session-id))
    (funcall fetch client
             :session-id session-id
             :resolve (lambda (result)
                        (hermes-chat--in-buffer buffer
                          (when (hermes-chat--dashboard-context-current-p
                                 client generation session-id)
                            (hermes-chat--insert-local-status
                             (funcall render result) 'done))))
             :reject (lambda (message)
                       (hermes-chat--in-buffer buffer
                         (when (hermes-chat--dashboard-context-current-p
                                client generation session-id)
                           (hermes-chat--command-error message)))))))

(defun hermes-chat-show-usage ()
  "Show this session's token usage via `session.usage'."
  (interactive nil hermes-chat-mode)
  (hermes-chat--show-session-panel
   #'hermes-dashboard-transport-session-usage
   #'hermes-chat--usage-content))

(defun hermes-chat-show-status ()
  "Show the gateway's rendered `session.status' panel for this session."
  (interactive nil hermes-chat-mode)
  (hermes-chat--show-session-panel
   #'hermes-dashboard-transport-session-status
   (lambda (result)
     (or (hermes-transport--scalar-string
          (hermes-transport--get result 'output))
         "No status available"))))

(defun hermes-chat-quote-region ()
  "Append the active transcript region as a Markdown quote to the draft.
Preserve existing input and move below the quote for editing.  This only
edits the draft, even during a running turn; it never sends or queues it.
Reject an empty region or one extending into the composer."
  (interactive nil hermes-chat-mode)
  (unless (and (derived-mode-p 'hermes-chat-mode)
               (markerp hermes-chat--input-marker)
               (eq (marker-buffer hermes-chat--input-marker) (current-buffer))
               (use-region-p)
               (< (region-beginning) (region-end))
               (<= (point-min) (region-beginning))
               (<= (region-end) (min (point-max) hermes-chat--input-marker)))
    (user-error "Select text entirely within the chat transcript"))
  (let ((quote (mapconcat (lambda (line) (concat "> " line))
                          (split-string (buffer-substring-no-properties
                                         (region-beginning) (region-end))
                                        "\n" nil)
                          "\n")))
    (widen)
    (goto-char (point-max))
    (insert (cond
             ((= (point) hermes-chat--input-marker) "")
             ((and (>= (- (point) hermes-chat--input-marker) 2)
                   (equal (buffer-substring-no-properties (- (point) 2) (point))
                          "\n\n")) "")
             ((eq (char-before) ?\n) "\n")
             (t "\n\n"))
            quote "\n\n")
    (deactivate-mark)))

(defun hermes-chat-go-to-composer ()
  "Move to the end of the writable composer without changing its text.
The latest output sits just above it, so this also clears the mode line's
unseen-output indicator."
  (interactive nil hermes-chat-mode)
  (hermes-chat--jump-to-latest))

(defun hermes-chat-next-button (&optional backward)
  "Move to the next transcript button, or previous when BACKWARD is non-nil.
Do not wrap into the composer or modify its draft."
  (interactive nil hermes-chat-mode)
  (let ((button (if backward (previous-button (point)) (next-button (point)))))
    (unless (and button (< (button-start button) hermes-chat--input-marker))
      (user-error "No further transcript button"))
    (goto-char (button-start button))))

(defun hermes-chat-previous-button ()
  "Move to the previous transcript button without changing input."
  (interactive nil hermes-chat-mode)
  (hermes-chat-next-button t))

(defun hermes-chat-tab ()
  "Complete in the composer, or visit the next transcript button."
  (interactive nil hermes-chat-mode)
  (if (>= (point) hermes-chat--input-marker)
      (let ((hermes-chat--explicit-completion t))
        (completion-at-point))
    (hermes-chat-next-button)))

;;;; Transcript keys, turn navigation and detail levels

(defun hermes-chat--composer-empty-p ()
  "Return non-nil when the chat composer is empty."
  (when-let* ((pos (hermes-chat--input-position)))
    (save-restriction
      (widen)
      (= pos (point-max)))))

(defun hermes-chat--transcript-key (command &optional empty-composer native)
  "Return a filtered key binding running COMMAND outside the composer.
In the composer the key inserts itself, unless EMPTY-COMPOSER is non-nil
and the composer is empty.  With NATIVE non-nil the binding instead steps
aside in the composer, so the key keeps its ordinary global meaning, such
as deletion for DEL."
  `(menu-item "" ,command
              :filter ,(lambda (command)
                         (cond
                          ((or (not (hermes-chat--point-in-input-p))
                               (and empty-composer
                                    (hermes-chat--composer-empty-p)))
                           command)
                          ((not native) #'self-insert-command)))))

(defun hermes-chat--entry-node-at (position)
  "Return the transcript EWOC node containing POSITION, or nil."
  (when-let* ((ewoc hermes-chat--ewoc)
              (node (ewoc-locate ewoc position)))
    (and (<= (ewoc-location node) position) node)))

(defun hermes-chat--entry-visible-text (node)
  "Return NODE's rendered transcript text without properties."
  (let* ((next (ewoc-next hermes-chat--ewoc node))
         (end (if next (ewoc-location next) (hermes-chat--input-position))))
    (string-trim (buffer-substring-no-properties (ewoc-location node) end))))

(defun hermes-chat--copy-target (position)
  "Return (TEXT . MESSAGE) for the transcript content at POSITION, or nil.
Prefer the exact body of a fenced code block, then the Markdown source
of a user or assistant entry, then the entry's visible text."
  (if-let* ((code (get-text-property position 'hermes-chat-code)))
      (cons code (format "Copied code block (%d lines)"
                         (length (split-string (string-remove-suffix "\n" code)
                                               "\n"))))
    (when-let* ((node (hermes-chat--entry-node-at position)))
      (let* ((entry (ewoc-data node))
             (role (plist-get entry :role))
             (source (plist-get entry :content)))
        (if (and (memq role '(user assistant))
                 (stringp source) (not (string-empty-p source)))
            (cons source (if (eq role 'user) "Copied message" "Copied reply"))
          (cons (hermes-chat--entry-visible-text node) "Copied entry text"))))))

(defun hermes-chat-copy ()
  "Copy the transcript content at point to the kill ring.
With an active region, behave exactly like `kill-ring-save'.  Otherwise
copy the code of the fenced block at point without its fences, or else
the Markdown source of the message at point."
  (interactive nil hermes-chat-mode)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (pcase-let ((`(,text . ,label)
                 (or (hermes-chat--copy-target (point))
                     (user-error "Nothing to copy here"))))
      (kill-new text)
      (message "%s" label))))

(defun hermes-chat--user-turn-positions ()
  "Return the transcript start of each submitted user turn, in order."
  (let (positions)
    (when hermes-chat--ewoc
      (let ((node (ewoc-nth hermes-chat--ewoc 0)))
        (while node
          (when (eq (plist-get (ewoc-data node) :role) 'user)
            (push (marker-position (ewoc-location node)) positions))
          (setq node (ewoc-next hermes-chat--ewoc node)))))
    (nreverse positions)))

(defun hermes-chat-next-turn (&optional n)
  "Move to the start of the Nth next user turn in the transcript.
With negative N, move backward."
  (interactive "p" hermes-chat-mode)
  (let* ((n (or n 1))
         (here (line-beginning-position))
         (positions (if (> n 0)
                        (seq-filter (lambda (pos) (> pos here))
                                    (hermes-chat--user-turn-positions))
                      (reverse (seq-filter (lambda (pos) (< pos here))
                                           (hermes-chat--user-turn-positions)))))
         (target (nth (1- (abs n)) positions)))
    (unless target
      (user-error (if (> n 0) "No later turn" "No earlier turn")))
    (goto-char target)))

(defun hermes-chat-previous-turn (&optional n)
  "Move to the start of the Nth previous user turn in the transcript."
  (interactive "p" hermes-chat-mode)
  (hermes-chat-next-turn (- (or n 1))))

(defun hermes-chat--imenu-index ()
  "Return an `imenu' index naming each user turn by its first line."
  (let (index)
    (dolist (pos (hermes-chat--user-turn-positions))
      (let* ((text (buffer-substring-no-properties
                    (min (+ pos 2) (point-max))
                    (save-excursion (goto-char pos) (line-end-position))))
             (name (truncate-string-to-width (string-trim text) 60 nil nil "…")))
        (push (cons (format "%d. %s" (1+ (length index))
                            (if (string-empty-p name) "(empty)" name))
                    pos)
              index)))
    (nreverse index)))

(defun hermes-chat--detail-label (name kind)
  "Return popup label NAME with KIND's current shown/hidden value."
  (concat name ": "
          (propertize (if (hermes-chat--details-hidden-p kind) "hidden" "shown")
                      'face 'keymap-popup-value)))

(defun hermes-chat--toggle-details (kind name)
  "Toggle global visibility of KIND bodies and report it under NAME."
  (hermes-chat--set-details-hidden kind (not (hermes-chat--details-hidden-p kind)))
  (message "%s %s" name
           (if (hermes-chat--details-hidden-p kind) "hidden" "shown")))

(defun hermes-chat-toggle-tool-output ()
  "Show or hide every expanded tool-output body in this chat.
Hiding wins over per-entry expansion and keeps point; expanding a tool
entry by hand shows tool output again."
  (interactive nil hermes-chat-mode)
  (hermes-chat--toggle-details 'hermes-tools "Tool output"))

(defun hermes-chat-toggle-reasoning ()
  "Show or hide every expanded reasoning body in this chat.
Hiding wins over per-entry expansion and keeps point; expanding a
reasoning entry by hand shows reasoning again.  Showing it says so
when the backend profile does not capture reasoning; see
`hermes-chat-enable-backend-reasoning'."
  (interactive nil hermes-chat-mode)
  (hermes-chat--toggle-details 'hermes-reasoning "Reasoning")
  (unless (hermes-chat--details-hidden-p 'hermes-reasoning)
    (hermes-chat--check-backend-reasoning)))

(defun hermes-chat--expandable-entry-p (entry)
  "Return non-nil when ENTRY has a collapsible body."
  (let ((role (plist-get entry :role)))
    (cond
     ((eq role 'commentary) t)
     ((eq role 'tasks) (hermes-chat-todos--entry-collapsible-p entry))
     ((memq role hermes-chat--transient-entry-roles)
      (let ((content (or (plist-get entry :content) "")))
        (and (not (string-empty-p content))
             (hermes-chat--transient-disclosure
              entry content (hermes-chat--diff-blocks content))
             t))))))

(defun hermes-chat--set-all-expanded (expanded)
  "Set every collapsible transcript entry to EXPANDED; return the count."
  (let ((count 0) ids)
    (maphash (lambda (id node)
               (let ((entry (ewoc-data node)))
                 (when (and (hermes-chat--expandable-entry-p entry)
                            (not (eq (and (hermes-chat--entry-expanded-p entry) t)
                                     (and expanded t))))
                   (push id ids))))
             hermes-chat--nodes)
    (dolist (id ids)
      (when (hermes-chat--update-entry
             id (lambda (entry)
                  (hermes-chat--entry-with
                   entry :metadata
                   (plist-put (copy-sequence (plist-get entry :metadata))
                              :expanded expanded)))
             t)
        (cl-incf count)))
    (when ids (hermes-chat--notify-state-change))
    count))

(defun hermes-chat-expand-all-details ()
  "Expand every collapsible entry, tasks included, and show tools and reasoning."
  (interactive nil hermes-chat-mode)
  (hermes-chat--set-details-hidden 'hermes-tools nil)
  (hermes-chat--set-details-hidden 'hermes-reasoning nil)
  (message "Expanded %d entries" (hermes-chat--set-all-expanded t)))

(defun hermes-chat-collapse-all-details ()
  "Collapse every collapsible entry, tasks included, to its summary line."
  (interactive nil hermes-chat-mode)
  (message "Collapsed %d entries" (hermes-chat--set-all-expanded nil)))

;;;; Composer placeholder

(defcustom hermes-chat-composer-placeholder t
  "Non-nil shows key hints in an empty chat composer.
The hint is display-only: it is never buffer text, so it cannot be
edited, copied, saved as a draft or sent, and it disappears as soon as
the composer holds any text."
  :type 'boolean
  :group 'hermes)

(defvar-local hermes-chat--placeholder-overlay nil
  "Overlay displaying the empty-composer hint.")

(defvar hermes-chat-mode-map)

(defun hermes-chat--command-key-hint (command)
  "Return COMMAND's chat key as a `help-key-binding' string.
Prefer a binding that also works mid-draft over a self-inserting key."
  (let* ((keys (where-is-internal command hermes-chat-mode-map))
         (key (or (seq-find (lambda (key)
                              (not (and (= (length key) 1)
                                        (characterp (aref key 0))
                                        (aref printable-chars (aref key 0)))))
                            keys)
                  (car keys))))
    (propertize (if key (key-description key)
                  (concat "M-x " (symbol-name command)))
                'face 'help-key-binding)))

(defun hermes-chat--placeholder-string ()
  "Return the empty-composer hint, with keys from the chat keymap."
  (let ((hint (concat "Message… · "
                      (hermes-chat--command-key-hint
                       #'hermes-chat-actions-map-popup)
                      " actions · / commands · "
                      (hermes-chat--command-key-hint
                       #'hermes-chat-input-history-previous)
                      " history")))
    (add-face-text-property 0 (length hint) 'shadow t hint)
    ;; Draw the cursor before the hint rather than after it.
    (put-text-property 0 1 'cursor t hint)
    hint))

(defun hermes-chat--placeholder-update (&rest _)
  "Show the composer hint exactly when the composer is empty."
  (when-let* ((overlay hermes-chat--placeholder-overlay)
              ((overlay-buffer overlay))
              (pos (hermes-chat--input-position)))
    (if (and hermes-chat-composer-placeholder (hermes-chat--composer-empty-p))
        (progn
          (move-overlay overlay pos pos)
          (unless (overlay-get overlay 'after-string)
            (overlay-put overlay 'after-string
                         (hermes-chat--placeholder-string))))
      (overlay-put overlay 'after-string nil))))

(defun hermes-chat--placeholder-retire ()
  "Delete the composer hint overlay this chat lifetime owns."
  (when (overlayp hermes-chat--placeholder-overlay)
    (delete-overlay hermes-chat--placeholder-overlay))
  (setq hermes-chat--placeholder-overlay nil)
  (remove-hook 'after-change-functions #'hermes-chat--placeholder-update t))

(defun hermes-chat--placeholder-activate ()
  "Install the empty-composer hint in the current chat.
The overlay is retired when the major mode changes, including re-entry
into `hermes-chat-mode', so each activation owns exactly one hint."
  (hermes-chat--placeholder-retire)
  (setq hermes-chat--placeholder-overlay (make-overlay (point-max) (point-max)))
  (overlay-put hermes-chat--placeholder-overlay 'hermes-chat-placeholder t)
  (add-hook 'change-major-mode-hook #'hermes-chat--placeholder-retire nil t)
  (add-hook 'after-change-functions #'hermes-chat--placeholder-update t t)
  (hermes-chat--placeholder-update))

(autoload 'hermes-chat-work "hermes-subagents" nil
  '(hermes-chat-mode hermes-work-mode))
(autoload 'hermes-chat-workers-label "hermes-subagents")

(defun hermes-chat--popup-title ()
  "Identify the current chat using cached owner-local state."
  (concat "Chat: " (propertize (buffer-name) 'face 'font-lock-type-face)))

(defun hermes-chat--interrupt-unavailable-p ()
  "Return non-nil unless a run or local image preparation can be interrupted."
  (let ((phase (plist-get
                (plist-get (plist-get hermes-chat--unsettled-submit-context
                                      :queue-entry) :image-record) :state)))
    (not (or (memq phase '(uploading attaching))
             (and (eq phase 'local) hermes-chat--session-bootstrap)
             (and (not hermes-chat--busy-submit-context)
                  hermes-chat--pending-assistant-id
                  (hermes-chat--dashboard-session-attached-p))))))

(defun hermes-chat--interrupt-send-unavailable-p ()
  "Return non-nil when interrupt-and-send cannot target an attached run."
  (or (hermes-chat--interrupt-unavailable-p)
      (not hermes-chat--pending-assistant-id)
      (not (hermes-chat--dashboard-session-attached-p))))

(defun hermes-chat--queue-count-suffix ()
  "Return \" N\" with the queued count as a value, or \"\" when empty."
  (let ((count (length hermes-chat--queued-messages)))
    (if (> count 0)
        (concat " " (propertize (number-to-string count) 'face 'font-lock-type-face))
      "")))

(defun hermes-chat--queue-label ()
  "Describe whether the queue command sends now or waits for a turn."
  (concat
   (if (or (hermes-chat--active-turn-p) hermes-chat--queued-messages
           (eq (plist-get hermes-chat--session-bootstrap :kind) 'history))
       "Queue message"
     "Queue / send now")
   (hermes-chat--queue-count-suffix)))

(defun hermes-chat--queue-panel-label ()
  "Describe the queue panel action with the queued count, hidden when zero."
  (concat "Queue side panel" (hermes-chat--queue-count-suffix)))

(defun hermes-chat--steer-label ()
  "Describe the cached steering target, including the idle send fallback."
  (cond (hermes-chat--pending-assistant-id "Steer / queue fallback")
        ((or (hermes-chat--active-turn-p) hermes-chat--queued-messages
             (eq (plist-get hermes-chat--session-bootstrap :kind) 'history))
         "Steer / queue")
        (t "Steer / send now")))

;; Audio is lazy and absent-safe; ordinary chat has no device dependency.
(autoload 'hermes-audio-record "hermes-audio" nil t)
(autoload 'hermes-audio-stop "hermes-audio" nil t)
(autoload 'hermes-audio-cancel "hermes-audio" nil t)
(autoload 'hermes-audio-read-aloud "hermes-audio" nil t)

(keymap-popup-define hermes-chat-audio-map
  "Use optional Emacs-side audio devices through the owning backend."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Local audio"
  "r" ("Record (consent)" hermes-audio-record)
  "s" ("Stop / transcribe" hermes-audio-stop)
  "c" ("Cancel local audio" hermes-audio-cancel)
  "a" ("Read reply aloud" hermes-audio-read-aloud))

(put 'hermes-chat-audio-map-popup 'command-modes '(hermes-chat-mode))

(keymap-popup-define hermes-chat-images-map
  "Manage images in the current draft."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Images"
  "f" ("Attach image" hermes-chat-attach-image-file)
  "v" ("Paste image" hermes-chat-paste-image)
  "V" ("Preview / recover" hermes-chat-preview-images)
  "D" ("Remove draft image" hermes-chat-remove-image))

(put 'hermes-chat-images-map-popup 'command-modes '(hermes-chat-mode))

(keymap-popup-define hermes-chat-files-map
  "Attach local text/source through the gateway workspace."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Attachments"
  "f" ("Upload text/source" hermes-chat-attach-file)
  "r" ("Reopen retained recovery" hermes-chat-attachment-recovery)
  "c" ("Cancel local work" hermes-chat-attachment-cancel))

(put 'hermes-chat-files-map-popup 'command-modes '(hermes-chat-mode))

(defvar-keymap hermes-chat-images-mode-line-map
  :doc "Mouse access to the owning composer's image actions."
  "<mode-line> <mouse-1>"
  (lambda (event)
    (interactive "e")
    (select-window (posn-window (event-start event)))
    (hermes-chat-images-map-popup)))

(defun hermes-chat--images-mode-line ()
  "Return a compact action label for this composer's retained draft images."
  (when hermes-chat--draft-images
    (let ((count (length hermes-chat--draft-images)))
      (propertize (format " [%d image%s]" count (if (= count 1) "" "s"))
                  'mouse-face 'mode-line-highlight
                  'local-map hermes-chat-images-mode-line-map
                  'help-echo "mouse-1 or C-c C-o I: Preview, remove or recover images"))))

(defun hermes-chat--edit-mode-line ()
  "Return the compact editing-an-earlier-message segment, or nil."
  (when hermes-chat--edit-turn
    (propertize " ✎ editing" 'face 'mode-line-emphasis
                'help-echo (substitute-command-keys
                            "\\[hermes-chat-send] resends from that message; \
\\[hermes-chat-edit-cancel] restores the previous draft"))))

(defun hermes-chat--mode-line-process ()
  "Return this chat's mode-line process segments, or nil when there are none."
  (when-let* ((parts (delq nil (list (hermes-chat--edit-mode-line)
                                     (hermes-chat--images-mode-line)
                                     (hermes-chat--new-output-mode-line)))))
    (apply #'concat parts)))

(keymap-popup-define hermes-chat-sess-map
  "Manage the current chat session."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Session"
  "n" ("New chat" hermes-chat)
  "R" ("Rename session" hermes-chat-rename)
  "S" ("List / resume sessions" hermes-list-sessions)
  "g" ("Goal / loop controls" hermes-session-control)
  :group "Fork"
  "b" ("Branch" hermes-chat-branch
       :inapt-if #'hermes-chat--active-turn-p)
  "H" ("Hand off" hermes-chat-handoff)
  "z" ("Compress" hermes-chat-compress
       :inapt-if #'hermes-chat--active-turn-p))

(put 'hermes-chat-sess-map-popup 'command-modes '(hermes-chat-mode))

(keymap-popup-define hermes-chat-model-map
  "Configure the chat model and provider."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Model"
  "m" ((lambda () (hermes-chat--model-setting-value "Model"))
       hermes-chat-switch-model
       :inapt-if #'hermes-chat--active-turn-p)
  "e" ((lambda () (hermes-chat--reasoning-setting-value "Reasoning"))
       hermes-chat-set-reasoning
       :inapt-if #'hermes-chat--active-turn-p)
  "K" ("Connect provider" hermes-chat-connect-provider))

(put 'hermes-chat-model-map-popup 'command-modes '(hermes-chat-mode))

(keymap-popup-define hermes-chat-work-map
  "Choose the chat directory and related buffers."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Directory"
  "w" ((lambda () (hermes-chat--setting-value
                   (hermes-chat--current-working-directory) nil "Directory"))
       hermes-chat-set-directory
       :inapt-if #'hermes-chat--active-turn-p)
  "b" ("Switch chat buffer" hermes-switch-to-chat))

(put 'hermes-chat-work-map-popup 'command-modes '(hermes-chat-mode))

(keymap-popup-define hermes-chat-jobs-map
  "Inspect queued messages, workers and their output."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Activity"
  "P" (#'hermes-chat--queue-panel-label hermes-chat-queue-panel)
  "W" (#'hermes-chat-workers-label hermes-chat-work)
  "T" ("Live tasks" hermes-chat-show-todos)
  "o" ("Preview output" hermes-chat-preview-output))

(put 'hermes-chat-jobs-map-popup 'command-modes '(hermes-chat-mode))

;; Retain the original child shortcuts without advertising them twice.
(dolist (key '("P" "o" "T"))
  (unless (keymap-lookup hermes-chat-work-map key)
    (keymap-set hermes-chat-work-map key
                (keymap-lookup hermes-chat-jobs-map key))))

(autoload 'hermes-chat-context "hermes-context" nil t)

(keymap-popup-define hermes-chat-info-map
  "Inspect chat activity and connection state."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Inspect"
  "h" ("Session details" hermes-chat-session-details)
  "u" ("Token usage" hermes-chat-show-usage)
  "b" ("Context budget" hermes-chat-context)
  "t" ("Session status" hermes-chat-show-status)
  :row
  :group "Connection"
  "x" ("Reconnect socket" hermes-dashboard-reconnect)
  "r" ("Refresh commands" hermes-chat-refresh-commands :stay-open t))

(put 'hermes-chat-info-map-popup 'command-modes '(hermes-chat-mode))

(unless (keymap-lookup hermes-chat-info-map "W")
  (keymap-set hermes-chat-info-map "W" #'hermes-chat-work))

(keymap-popup-define hermes-chat-actions-map
  "In-chat action menu for `hermes-chat-mode'."
  :description #'hermes-chat--popup-title
  :popup-key "?"
  ;; Keep q available for queueing; children use the native q/C-g back key.
  :exit-key "C-g"
  :group "Turn"
  "s" (#'hermes-chat--steer-label hermes-chat-steer-message
       :inapt-if (lambda () hermes-chat--draft-images))
  "i" ("Interrupt" hermes-chat-interrupt
       :inapt-if #'hermes-chat--interrupt-unavailable-p)
  "k" ("Interrupt + send" hermes-chat-interrupt-and-send
       :inapt-if #'hermes-chat--interrupt-send-unavailable-p)
  "q" (#'hermes-chat--queue-label hermes-chat-queue-message)
  :group "Compose"
  "RET" ("Send" hermes-chat-send)
  "Q" ("Quote region" hermes-chat-quote-region)
  "I" ("Images" :keymap hermes-chat-images-map)
  "F" ("Attachments" :keymap hermes-chat-files-map)
  :row
  :group "Configure"
  "S" ("Session" :keymap hermes-chat-sess-map)
  "M" ("Model" :keymap hermes-chat-model-map)
  "w" ("Directory" :keymap hermes-chat-work-map)
  "A" ("Local audio" :keymap hermes-chat-audio-map)
  :group "Browse"
  "B" ("Activity" :keymap hermes-chat-jobs-map)
  "X" ("Inspect" :keymap hermes-chat-info-map)
  "c" ("Show commands" hermes-chat-show-commands)
  "W" (#'hermes-chat-workers-label hermes-chat-work)
  :row
  :group "Show"
  "O" ((lambda () (hermes-chat--detail-label "Tool output" 'hermes-tools))
       hermes-chat-toggle-tool-output :stay-open t)
  "E" ((lambda () (hermes-chat--detail-label "Reasoning" 'hermes-reasoning))
       hermes-chat-toggle-reasoning :stay-open t)
  "+" ("Expand all" hermes-chat-expand-all-details)
  "-" ("Collapse all" hermes-chat-collapse-all-details)
  :group "Aside"
  "y" ("Side question (btw)" hermes-chat-btw)
  "G" ("Background task" hermes-chat-background)
  "!" ("Stop processes" hermes-chat-stop-processes)
  :group ("Message" :if #'hermes-chat--message-actions-p)
  "C" ("Edit and resend" hermes-chat-edit-turn
       :inapt-if #'hermes-chat--rewind-unavailable-p)
  "U" ("Rerun from here" hermes-chat-restore-turn
       :inapt-if #'hermes-chat--rewind-unavailable-p)
  "Z" ("Cancel edit" hermes-chat-edit-cancel
       :if #'hermes-chat--editing-p)
  :group ("Prompt" :if #'hermes-chat--pending-prompt-p)
  "a" ("Answer prompt" hermes-chat-respond-to-prompt)
  "d" ("Cancel prompt" hermes-chat-cancel-prompt))

;; `C-c C-j' covers the composer and Inspect now offers Refresh commands;
;; keep both former menu keys as quiet aliases.
(keymap-set hermes-chat-actions-map "j" #'hermes-chat-go-to-composer)
(keymap-set hermes-chat-actions-map "r" #'hermes-chat-refresh-commands)

(dolist (command '(hermes-chat-actions-map-popup
                  hermes-chat-actions-map--enter-hermes-chat-audio-map
                  hermes-chat-actions-map--enter-hermes-chat-images-map
                  hermes-chat-actions-map--enter-hermes-chat-sess-map
                  hermes-chat-actions-map--enter-hermes-chat-model-map
                  hermes-chat-actions-map--enter-hermes-chat-work-map
                  hermes-chat-actions-map--enter-hermes-chat-jobs-map
                  hermes-chat-actions-map--enter-hermes-chat-info-map))
  (put command 'command-modes '(hermes-chat-mode)))

(defun hermes-chat--submenu-root-key ()
  "Refuse ancestor menu keys that cannot safely dispatch from a child."
  (interactive nil hermes-chat-mode)
  (user-error "Go back before choosing another menu"))

;; Retain unclaimed suffix shortcuts without advertising a second menu or
;; maintaining another command table.  The submenu maps own these bindings.
;; The Directory map precedes Session so root `b' keeps Switch chat buffer
;; rather than Fork's Branch.
(dolist (map (list hermes-chat-images-map hermes-chat-work-map
                   hermes-chat-sess-map hermes-chat-model-map
                   hermes-chat-info-map hermes-chat-jobs-map))
  (map-keymap
   (lambda (event binding)
     (let ((key (vector event)))
       (when (and (symbolp binding)
                  (not (lookup-key hermes-chat-actions-map key)))
         (define-key hermes-chat-actions-map key binding))))
   map))

;; Native parent wrappers capture the popup buffer.  An unclaimed ancestor
;; launcher exits the child before dispatch and kills that buffer.  Shadow
;; only these keys with an ordinary refusal, leaving native q/C-g navigation
;; and child actions intact.  Install aliases above before adding refusals.
(let* ((groups (apply #'append
                      (keymap-popup--meta hermes-chat-actions-map 'descriptions)))
       (menus (cl-remove-if-not
               (lambda (entry) (eq (plist-get entry :type) 'keymap))
               (apply #'append
                      (mapcar (lambda (group) (plist-get group :entries))
                              groups)))))
  (dolist (menu menus)
    (let* ((target (plist-get menu :target))
           (map (if (symbolp target) (symbol-value target) target)))
      (dolist (target menus)
        (let ((key (plist-get target :key)))
          (unless (keymap-lookup map key)
            (keymap-set map key #'hermes-chat--submenu-root-key)))))))

(defvar-keymap hermes-chat-mode-map
  :doc "Keymap for `hermes-chat-mode'."
  "RET" #'hermes-chat-send
  "C-j" #'hermes-chat-newline
  "S-<return>" #'hermes-chat-newline
  "TAB" #'hermes-chat-tab
  "r" '(menu-item "Quote region" hermes-chat-quote-region
         :filter (lambda (command)
                   (if (hermes-chat--point-in-input-p)
                       #'self-insert-command
                     command)))
  "?" (hermes-chat--transcript-key #'hermes-chat-actions-map-popup t)
  "n" (hermes-chat--transcript-key #'hermes-chat-next-turn)
  "p" (hermes-chat--transcript-key #'hermes-chat-previous-turn)
  "w" (hermes-chat--transcript-key #'hermes-chat-copy)
  "e" (hermes-chat--transcript-key #'hermes-chat-edit-turn)
  ;; Escape cancels an edit; otherwise it keeps its native meaning.
  "<escape>" '(menu-item "" hermes-chat-edit-cancel
                         :filter (lambda (command)
                                   (and hermes-chat--edit-turn command)))
  "SPC" (hermes-chat--transcript-key #'scroll-up-command)
  "S-SPC" (hermes-chat--transcript-key #'scroll-down-command nil t)
  "DEL" (hermes-chat--transcript-key #'scroll-down-command nil t)
  "<backtab>" #'hermes-chat-previous-button
  "<remap> <forward-button>" #'hermes-chat-next-button
  "<remap> <backward-button>" #'hermes-chat-previous-button
  "C-c C-w" #'hermes-chat-work
  "C-c C-j" #'hermes-chat-go-to-composer
  "M-p" #'hermes-chat-input-history-previous
  "M-n" #'hermes-chat-input-history-next
  "C-c C-c" #'hermes-chat-interrupt-turn
  "C-c C-i" #'hermes-chat-interrupt
  "C-c C-k" #'hermes-chat-interrupt-and-send
  "C-c C-q" #'hermes-chat-queue-message
  "C-c C-s" #'hermes-chat-steer-message
  "C-c C-a" #'hermes-chat-respond-to-prompt
  "C-c C-d" #'hermes-chat-cancel-prompt
  "C-c C-o" #'hermes-chat-actions-map-popup
  "C-c C-p" #'hermes-command-palette
  "C-c C-/" #'hermes-chat-show-commands
  "C-c C-l" #'hermes-chat-view-attachments
  "C-c C-g" #'hermes-session-control
  "C-c C-n" #'hermes-chat
  "C-c C-r" #'hermes-chat-rename
  "C-c C-b" #'hermes-switch-to-chat)

(defun hermes-chat--disable-linters ()
  "Turn off `flycheck-mode' and `flymake-mode' in the current chat buffer.
The transcript is generated, not authored, so linting it only wastes CPU on
every streamed delta.  Called from `after-change-major-mode-hook' at a late
depth so a globalized linter re-enabled after the mode body is overridden."
  (dolist (mode '(flycheck-mode flymake-mode))
    (when (and (fboundp mode) (boundp mode) (symbol-value mode))
      (funcall mode -1))))

(define-derived-mode hermes-chat-mode fundamental-mode "Hermes Chat"
  "Major mode for Hermes chat buffers."
  :keymap hermes-chat-mode-map
  :interactive nil
  (visual-line-mode 1)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 5)
  (setq-local display-line-numbers nil)
  (setq-local mode-line-process '(:eval (hermes-chat--mode-line-process)))
  (add-hook 'kill-buffer-hook #'hermes-chat--cleanup-buffer nil t)
  (add-hook 'change-major-mode-hook #'hermes-chat--cleanup-buffer nil t)
  (add-hook 'completion-at-point-functions #'hermes-chat--slash-capf nil t)
  (add-hook 'completion-at-point-functions #'hermes-chat--model-capf t t)
  (add-hook 'completion-at-point-functions #'hermes-chat--file-ref-capf t t)
  (add-hook 'after-change-major-mode-hook #'hermes-chat--disable-linters 90 t)
  (add-hook 'hermes-chat-lifecycle-invalidation-hook
            #'hermes-chat--images-invalidate nil t)
  (add-hook 'hermes-chat-lifecycle-invalidation-hook
            #'hermes-chat-todos--clear nil t)
  (add-hook 'hermes-chat-submit-inhibit-functions
            #'hermes-chat--images-inhibit nil t)
  (add-hook 'hermes-chat-state-change-hook
            #'hermes-chat--history-reconcile-maybe nil t)
  (setq-local desktop-save-buffer #'hermes-chat--desktop-save)
  (setq-local imenu-create-index-function #'hermes-chat--imenu-index)
  (add-hook 'warm-restart-blocker-functions
            #'hermes-chat--warm-restart-blocker nil t)
  (hermes-chat--setup-buffer)
  (hermes-chat-draft--activate)
  (hermes-chat--placeholder-activate))

;;;###autoload
(defun hermes-chat (&optional profile instance)
  "Open a new Hermes chat buffer under agent PROFILE.
INSTANCE selects the owning Hermes instance.  Interactively resolve the
instance first, then prompt for PROFILE (blank uses the dashboard default).
Each call opens a distinct buffer named after the profile -- and, once the
session is titled, after that title -- so chats stay filterable with
`hermes-switch-to-chat'."
  (interactive
   (let ((instance (hermes-instance-resolve)))
     (let ((hermes-instance instance)
           (hermes-dashboard-transport-url (hermes-instance-url instance)))
       (list (hermes-chat--read-profile) instance))))
  (hermes-chat--new-buffer profile nil instance))

(defun hermes-chat--new-command (title)
  "Handle /new with TITLE, preserving a canonical Bot Chat relationship."
  (if (not hermes-chat--bot-chat-root)
      (hermes-chat--new-buffer nil title)
    (let* ((buffer (current-buffer))
           (root hermes-chat--bot-chat-root)
           (lifetime hermes-chat--lifecycle-generation)
           (profile hermes-chat--profile)
           (instance hermes-instance)
           (url (or hermes-chat--pinned-url (hermes-instance-url instance)))
           (choice (completing-read
                    "Bot Chat: " '("Compress conversation" "Open scratch chat") nil t)))
      (unless (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (and (equal root hermes-chat--bot-chat-root)
                          (eql lifetime hermes-chat--lifecycle-generation)
                          (equal profile hermes-chat--profile)
                          (equal instance hermes-instance))))
        (user-error "Bot Chat changed during input"))
      (with-current-buffer buffer
        (pcase choice
          ("Compress conversation" (hermes-chat--dashboard-compress "compact" ""))
          ("Open scratch chat" (hermes-chat--new-buffer profile title instance url)))))))


;; Registries keep lower chat layers free of upward references.
(defun hermes-chat--install-terminal-owner-registry ()
  "Install capture/take functions in deterministic effect order."
  (setq hermes-chat--terminal-owner-functions
        '((hermes-chat--capture-terminal-prompts
           . hermes-chat--take-terminal-prompts)
          (hermes-chat--capture-command-terminal-owner
           . hermes-chat--take-command-terminal-owner)
          (hermes-chat--capture-handoff-terminal-owner
           . hermes-chat--take-handoff-terminal-owner))))

(defun hermes-chat--install-registries ()
  "Install chat-owned callbacks into lower-layer registries."
  (setq hermes-chat--submit-function #'hermes-chat--submit-content
        hermes-chat--history-reconcile-function
        #'hermes-chat--request-history-reconcile
        hermes-chat--queue-drain-ready-function
        #'hermes-chat--dashboard-queue-drain-ready-p
        hermes-chat--turn-event-function #'hermes-chat--run-turn-reducer
        hermes-chat--busy-submit-event-function
        #'hermes-chat--hold-busy-submit-event
        hermes-chat--busy-submit-abandon-function
        #'hermes-chat--abandon-busy-submit
        hermes-chat--native-slash-commands
        (list
         (cons '("commands") (lambda (_arg) (hermes-chat-show-commands)))
         (cons '("queue" "q")
               (lambda (arg)
                 (hermes-chat--dashboard-dispatch-command "queue" arg)))
         (cons '("background" "bg")
               (lambda (arg) (hermes-chat-background arg)))
         (cons '("btw") (lambda (arg) (hermes-chat-btw arg)))
         (cons '("branch") (lambda (arg) (hermes-chat-branch arg)))
         (cons '("steer") (lambda (arg) (hermes-chat-steer-message arg)))
         (cons '("stop") (lambda (_arg) (hermes-chat-stop-processes)))
         (cons '("interrupt" "int")
               (lambda (_arg) (hermes-chat-interrupt)))
         (cons '("clear" "reset") (lambda (_arg) (hermes-chat-clear)))
         (cons '("new") #'hermes-chat--new-command)
         (cons '("model")
               (lambda (arg)
                 (if (string-empty-p arg)
                     (hermes-chat-switch-model)
                   (hermes-chat--dashboard-set-model arg))))
         (cons '("title" "rename")
               (lambda (arg)
                 (if (string-empty-p arg)
                     (call-interactively #'hermes-chat-rename)
                   (hermes-chat-rename arg))))
         (cons '("handoff")
               (lambda (arg)
                 (if (string-empty-p arg)
                     (call-interactively #'hermes-chat-handoff)
                   (hermes-chat-handoff arg))))
         (cons '("compact")
               (lambda (arg) (hermes-chat--dashboard-compress "compact" arg)))
         (cons '("compress")
               (lambda (arg) (hermes-chat--dashboard-compress "compress" arg)))
         (cons '("sessions") (lambda (_arg) (hermes-list-sessions))))))

(hermes-chat--install-terminal-owner-registry)
(hermes-chat--install-registries)

(provide 'hermes-chat)
;;; hermes-chat.el ends here
