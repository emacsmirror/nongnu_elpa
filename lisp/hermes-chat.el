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
;; before a writable input tail -- lives in `hermes-chat-buffer'; see the
;; AGENTS.md module map for the full split.

;;; Code:

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
  "Name of the Hermes chat buffer."
  :type 'string
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
(defvar hermes-chat--working-directory)
(defvar hermes-chat--active-tools)
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
  (dolist (key '(:reasoning-effort :fast :yolo))
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
A non-empty kawaii spinner sets the `thinking' status with its verb.  An empty
`thinking.delta' is the gateway's clear signal once the model starts answering
or runs a tool, so it reverts to the running state instead of inventing a
literal \"Thinking\" label."
  (let ((content (or (plist-get event :content) "")))
    (if (string-empty-p (string-trim content))
        '(:status running :activity nil)
      (list :status 'thinking
            :activity (hermes-chat--thinking-activity content)))))

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
                         (or (hermes-chat--event-string
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

(defun hermes-chat--turn-done-effects (event status)
  "Return the ordered effect list for a `done' EVENT with header STATUS.
`refresh-header' precedes the lifecycle so the header settles before `drain'
re-submits any queued turn."
  (delq nil
        (list '(clear-tools)
              (cons 'refresh-header status)
              (cons 'clear-prompts event)
              (cons (if (plist-get event :response-previewed)
                        'mark-previewed
                      'mark-done)
                    (plist-get event :content))
              (and-let* ((warning (plist-get event :warning)))
                (cons 'warning warning))
              '(drop-thinking)
              '(settle . done)
              '(finish)
              '(clear-pending)
              '(set-dashboard-running)
              '(drain))))

(defun hermes-chat--turn-suppressed-effects (event status)
  "Return the ordered effect list for a `suppressed-terminal' EVENT.
STATUS is the merged header state.  Mirrors `hermes-chat--turn-done-effects'
minus content copying: the turn was resumed in flight without a local
assistant entry, so the reply placeholder keeps its text."
  (list '(clear-tools)
        (cons 'refresh-header status)
        (cons 'clear-prompts (plist-get event :original))
        (cons 'mark-status (plist-get event :settle-status))
        '(drop-thinking)
        (cons 'settle (plist-get event :settle-status))
        '(finish)
        '(clear-pending)
        '(set-dashboard-running)
        '(drain)))

(defun hermes-chat--turn-error-effects (event status)
  "Return the ordered effect list for an `error' EVENT with header STATUS."
  (let ((estatus (hermes-chat--error-status event)))
    (if (hermes-chat--interrupted-status-p estatus)
        (list '(clear-tools)
              (cons 'refresh-header status)
              (cons 'clear-prompts event)
              (cons 'mark-status estatus)
              (cons 'settle estatus)
              '(finish)
              '(clear-pending)
              '(set-dashboard-running)
              '(drain))
      (let ((content (let ((value (or (plist-get event :content) "")))
                       (if (string-empty-p value) "Transport error" value))))
        (list '(clear-tools)
              (cons 'refresh-header status)
              (cons 'clear-prompts event)
              (cons 'append-error (cons content estatus))
              (cons 'settle estatus)
              '(finish)
              '(clear-pending)
              '(set-dashboard-running)
              '(drain))))))

(defun hermes-chat--turn-reduce (state event now)
  "Return (NEW-STATE . EFFECTS) for domain EVENT applied to STATE at time NOW.
Pure: no buffer, EWOC, process, header, or message side effects.  EFFECTS is an
ordered list the boundary replays: a header change leads with `refresh-header',
`done'/`error' append the turn lifecycle, and tool/transcript events emit deltas
and `upsert-entry'.  Other types return (STATE)."
  (pcase (plist-get event :type)
    ('status
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
                   (hermes-chat--turn-state-put state :goal
                                                (plist-get event :goal))
                 state))
              (status (hermes-chat--turn-status-state next-state event now)))
         (cons (hermes-chat--turn-state-put next-state :status-state status)
               (append
                (delq nil (list (cons 'refresh-header status)
                                (hermes-chat--turn-entry-effect event)))
                (hermes-chat--turn-session-info-effects event)))))))
    ('goal
     (cons (hermes-chat--turn-state-put state :goal (plist-get event :goal))
           '((refresh-header))))
    ((or 'commentary 'thinking 'diff)
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
     (cons state (delq nil (list (hermes-chat--turn-tool-effect event)
                                 (hermes-chat--turn-entry-effect event)))))
    ('delta
     (cons state (list (cons 'append-delta (or (plist-get event :content) "")))))
    ('interim
     (cons state (list (cons 'seal-interim
                             (or (plist-get event :content) "")))))
    (_ (cons state nil))))

(defun hermes-chat--seal-interim-assistant (assistant-id content)
  "Seal ASSISTANT-ID with interim CONTENT and rotate the live stream entry."
  (hermes-chat--mark-assistant assistant-id 'done content t)
  (let* ((entry (hermes-chat--make-entry 'assistant "" 'streaming))
         (next-id (plist-get entry :id)))
    (hermes-chat--insert-entry entry)
    (setq hermes-chat--dashboard-interim-assistant-id assistant-id
          hermes-chat--pending-assistant-id next-id
          hermes-chat--dashboard-stream-assistant-id next-id)))

(defun hermes-chat--mark-previewed-assistant (assistant-id content)
  "Settle previewed CONTENT on its interim entry, or ASSISTANT-ID as fallback."
  (let* ((interim-id hermes-chat--dashboard-interim-assistant-id)
         (interim-node (and interim-id hermes-chat--nodes
                            (gethash interim-id hermes-chat--nodes)))
         (interim-content (and interim-node
                               (plist-get (ewoc-data interim-node) :content))))
    (if (and interim-content (equal interim-content content))
        (progn
          (hermes-chat--remove-entry assistant-id)
          (hermes-chat--mark-assistant interim-id 'done content t))
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
    (`(upsert-entry . ,event)
     (hermes-chat--upsert-transport-entry assistant-id event))
    (`(message . ,text) (message "%s" text))
    (`(warning . ,text)
     (hermes-chat--insert-local-status (format "warning: %s" text) 'done))
    (`(clear-prompts . ,event) (hermes-chat--clear-terminal-prompts event))
    (`(mark-done . ,content)
     (hermes-chat--mark-assistant
      assistant-id 'done
      (hermes-chat--assistant-done-content assistant-id content) t))
    (`(mark-previewed . ,content)
     (hermes-chat--mark-previewed-assistant assistant-id content))
    (`(append-error ,content . ,status)
     (hermes-chat--append-assistant-content assistant-id content status))
    (`(mark-status . ,status)
     (hermes-chat--mark-assistant assistant-id status nil t))
    ('(drop-thinking) (hermes-chat--drop-duplicate-thinking assistant-id))
    (`(settle . ,status)
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
Captures session identity first.  ASSISTANT-ID scopes the transcript effects.
The boundary persists NEW-STATE and replays the effects; it makes no decisions
of its own."
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

;; These files are sibling areas of one logical chat module.  They are
;; required here, after the reducer/effect helpers above, so the require
;; order documents the module seam: everything below this point may call
;; sibling functions directly, and the siblings' own upward wiring goes
;; through the registry variables they define (never `declare-function').
(require 'hermes-chat-buffer)
(require 'hermes-chat-prompts)
(require 'hermes-chat-dashboard)
(require 'hermes-chat-models)
(require 'hermes-chat-handoff)
(require 'hermes-chat-slash)


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

(defun hermes-chat--busy-submit-resolved (content result &optional before-node)
  "Apply backend busy-input RESULT for CONTENT before BEFORE-NODE."
  (pcase (hermes-chat--status-name
          (hermes-chat--result-string result 'status))
    ("queued" (hermes-chat--record-server-queued-content content))
    ((and status (or "steered" "redirected"))
     (hermes-chat--insert-entry
      (hermes-chat--make-entry
       'status (format "%s: %s" (capitalize status)
                       (hermes-chat--preview content)) 'done)
      (or before-node (hermes-chat--pending-assistant-node))))
    (_ (hermes-chat--activate-backend-turn content))))

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
    (hermes-chat--replay-busy-submit-events context after)))

(defun hermes-chat--settle-busy-submit (context result)
  "Settle busy submission CONTEXT from backend RESULT and replay held events."
  (let ((content (plist-get context :content))
        (events (hermes-chat--busy-submit-events context))
        (assistant-node
         (and hermes-chat--nodes
              (gethash (plist-get context :assistant-id)
                       hermes-chat--nodes)))
        (status (hermes-chat--status-name
                 (hermes-chat--result-string result 'status))))
    (setq hermes-chat--busy-submit-context nil)
    (pcase status
      ("queued"
       (hermes-chat--record-server-queued-content content)
       (hermes-chat--replay-busy-submit-events context events))
      ((or "steered" "redirected")
       (hermes-chat--replay-busy-submit-events context events)
       (hermes-chat--busy-submit-resolved content result assistant-node))
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
                :assistant-id assistant-id :events nil)))
    (setq hermes-chat--busy-submit-context context)
    (condition-case err
        (progn
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
  (interactive)
  (unless (hermes-chat--point-in-input-p)
    (goto-char (point-max)))
  (insert "\n"))

(defun hermes-chat--busy-submit-steered (context)
  "Convert CONTEXT's optimistic turn into the backend's active steered turn."
  (hermes-chat--remove-entry (plist-get context :user-id))
  (when (equal hermes-chat--pending-assistant-id
               (plist-get context :assistant-id))
    (hermes-chat--mark-assistant (plist-get context :assistant-id) 'streaming))
  (hermes-chat--insert-local-status
   (format "Steered: %s"
           (hermes-chat--preview
            (or (plist-get context :display)
                (plist-get context :content))))
   'done)
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
          hermes-chat--server-queued-prior-terminal-p nil)
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
    (when (and current-p (not terminal-p))
      (hermes-chat--dashboard-activate-server-queued-turn
       (plist-get context :assistant-id)))))

(defun hermes-chat--submit-resolved (context result)
  "Settle CONTEXT from dashboard prompt RESULT."
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
         (hermes-chat--current-lifetime-p (plist-get context :lifetime))
         (hermes-chat--current-transport-generation-p
          (plist-get context :generation))
         (eq (plist-get context :client) hermes-chat--dashboard-client)
         (equal (plist-get context :session-id)
                hermes-chat--dashboard-active-session-id)
         (equal (plist-get context :assistant-id)
                hermes-chat--pending-assistant-id)
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
          (hermes-chat--submit-resolved context result)
          (hermes-chat--clear-submit-context context))))))

(defun hermes-chat--queue-reject-callback (buffer context)
  "Return BUFFER callback rejecting the queued turn described by CONTEXT."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--submit-context-current-p context)
        (hermes-chat--clear-submit-context context)
        (hermes-chat--queue-submit-rejected
         (plist-get context :queue-id)
         (plist-get context :user-id)
         (plist-get context :assistant-id)
         message)))))

(defun hermes-chat--submit-reject-callback (buffer context)
  "Return BUFFER callback rejecting the turn described by CONTEXT."
  (lambda (message)
    (hermes-chat--in-buffer buffer
      (when (hermes-chat--submit-context-current-p context)
        (hermes-chat--clear-submit-context context)
        (setq hermes-chat--dashboard-running-p nil)
        (hermes-chat--handle-transport-event
         (plist-get context :assistant-id)
         (list :type 'error :content message))))))

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
  (let* ((buffer (plist-get context :buffer))
         (assistant-id (plist-get context :assistant-id))
         (dashboard-p (plist-get context :dashboard-p))
         (generation (plist-get context :generation))
         (queue-id (plist-get context :queue-id))
         (transport
          (hermes-chat--send-prompt
           content
           (hermes-chat--transport-callback
            buffer assistant-id dashboard-p generation)
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
    (hermes-chat--clear-submit-context context)
    (when (plist-get context :dashboard-p)
      (setq hermes-chat--dashboard-running-p nil))
    (if queue-id
        (hermes-chat--queue-submit-rejected
         queue-id user-id assistant-id message)
      (hermes-chat--handle-transport-event
       assistant-id (list :type 'error :content message)))
    (message "Hermes transport failed: %s" message)))

(defun hermes-chat--make-submit-context (content display queue-entry user assistant)
  "Return transport context for CONTENT, DISPLAY, QUEUE-ENTRY, USER, and ASSISTANT."
  (let ((dashboard-p (hermes-chat--dashboard-default-transport-p)))
    (list :buffer (current-buffer)
          :lifetime hermes-chat--lifecycle-generation
          :client nil
          :session-id nil
          :user-id (plist-get user :id)
          :assistant-id (plist-get assistant :id)
          :dashboard-p dashboard-p
          :generation (hermes-chat--next-transport-generation)
          :idle-count hermes-chat--dashboard-idle-count
          :post-start-terminal-p nil
          :queue-id (plist-get queue-entry :id)
          :queue-entry queue-entry
          :content content
          :display display)))

(defun hermes-chat--submit-callbacks (context)
  "Return the dashboard acceptance callbacks for CONTEXT."
  (let ((buffer (plist-get context :buffer)))
    (cons (hermes-chat--submit-resolve-callback buffer context)
          (if (plist-get context :queue-id)
              (hermes-chat--queue-reject-callback buffer context)
            (hermes-chat--submit-reject-callback buffer context)))))

(defun hermes-chat--submit-content (content &optional display queue-entry)
  "Submit CONTENT as a new user turn, echoing DISPLAY when non-nil.
DISPLAY lets a slash skill send its full payload while showing a compact line.
QUEUE-ENTRY identifies a queued message retained until transport acceptance.
Return non-nil when the transport request starts."
  (hermes-chat--ensure-submit-allowed)
  (when (and (hermes-chat--active-turn-p) (null queue-entry))
    (user-error "%s" (hermes-chat--busy-message)))
  (let* ((user-entry (hermes-chat--make-entry 'user (or display content) 'done))
         (assistant-entry (hermes-chat--make-entry 'assistant "" 'pending))
         (context (hermes-chat--make-submit-context
                   content display queue-entry user-entry assistant-entry))
         (callbacks (and (plist-get context :dashboard-p)
                         (hermes-chat--submit-callbacks context))))
    (hermes-chat--begin-pending-turn user-entry assistant-entry context)
    (condition-case err
        (progn
          (hermes-chat--submit-through-transport
           content context (car callbacks) (cdr callbacks))
          t)
      (error
       (hermes-chat--submit-signal-error context err)
       nil))))

;; The registry installation near the end of this file wires this submit
;; pipeline into lower chat layers without upward references.


(defun hermes-chat-queue-message (&optional message)
  "Queue MESSAGE to send after the active Hermes turn, or send now if idle."
  (interactive)
  (hermes-chat--ensure-submit-allowed)
  (let ((content (string-trim (or message (hermes-chat-input-string)))))
    (when (string-empty-p content)
      (user-error "No Hermes input to queue"))
    (unless message
      (hermes-chat--delete-input-tail))
    (hermes-chat--dashboard-queue-or-submit content (current-buffer))))

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
          (id (hermes-chat--steer-pending-status content)))
      (hermes-dashboard-transport-session-steer
       client content
       :session-id session-id
       :resolve (lambda (result)
                  (hermes-chat--in-buffer buffer
                    (when (hermes-chat--dashboard-context-current-p
                           client generation session-id)
                      (if (equal (hermes-chat--status-name
                                  (hermes-chat--result-string result 'status))
                                 "rejected")
                          (hermes-chat--steer-failed id content "rejected")
                        (hermes-chat--steer-acknowledged id content)))))
       :reject (lambda (err)
                 (hermes-chat--in-buffer buffer
                   (when (hermes-chat--dashboard-context-current-p
                          client generation session-id)
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
  (interactive)
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
              (hermes-chat--handle-transport-event assistant-id event))
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
  "Request interruption of the active dashboard run."
  (interactive)
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
  (interactive)
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
session key is preserved, so the conversation can still be resumed."
  (interactive)
  (unless (or hermes-chat--dashboard-client
              hermes-chat--process
              hermes-chat--dashboard-active-session-id)
    (user-error "This Hermes chat has no session to disconnect"))
  (when-let* ((assistant-id hermes-chat--pending-assistant-id))
    (hermes-chat--mark-assistant assistant-id 'disconnected nil t))
  (run-hooks 'hermes-chat-cleanup-functions)
  (hermes-chat--invalidate-transport-state)
  (hermes-chat--stop-dashboard-client)
  (hermes-chat--insert-local-status "Session disconnected" 'disconnected)
  (hermes-chat--set-header-state :status 'disconnected :activity "Disconnected"))

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
  (interactive)
  (unless (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (user-error "This chat has no live dashboard client to reconnect"))
  (when (hermes-chat--dashboard-client-active-turn-p
         hermes-chat--dashboard-client)
    (user-error "Interrupt every active turn sharing this dashboard first"))
  (hermes-dashboard-transport-reconnect hermes-chat--dashboard-client))

;;;###autoload
(defalias 'hermes-reconnect #'hermes-dashboard-reconnect)

(defun hermes-chat-stop-processes ()
  "Stop background/tool processes for this chat via `process.stop'.
This does not interrupt the current model turn -- use `hermes-chat-interrupt'
for that."
  (interactive)
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support stopping processes"))
  (let ((buffer (current-buffer))
        (lifetime hermes-chat--lifecycle-generation))
    (hermes-dashboard-transport-process-stop
     hermes-chat--dashboard-client
     :resolve (lambda (result)
                (hermes-chat--in-lifetime buffer lifetime
                  (hermes-chat--insert-local-status
                   (format "Stopped %s background process(es)"
                           (or (hermes-transport--get result 'killed) 0))
                   'done)))
     :reject (lambda (message)
               (hermes-chat--in-lifetime buffer lifetime
                 (hermes-chat--command-error message))))))

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
    (run-hooks 'hermes-chat-cleanup-functions)
    (hermes-chat--invalidate-transport-state)
    (hermes-chat--stop-dashboard-client)
    (hermes-chat--setup-buffer)
    (hermes-chat--restore-draft-runtime)
    (when outermost
      (hermes-chat--drain-reset-clarify-owners
       hermes-chat--reset-clarify-owner-sink))))

(defun hermes-chat-clear ()
  "Clear this chat's transcript and start a fresh Hermes session in place."
  (interactive)
  (when (y-or-n-p "Clear this Hermes conversation and transcript? ")
    (hermes-chat--reset-transcript)
    (hermes-chat--insert-local-status "Session cleared" 'done)))

(defun hermes-chat--new-buffer (&optional profile title instance)
  "Create, display, and return a fresh chat buffer.
PROFILE selects the agent profile, TITLE pins a manual title, and INSTANCE is
the owning Hermes instance.  A nil INSTANCE is resolved from the current
context.
PROFILE nil means the dashboard default; a non-empty TITLE pins a manual title.
Buffer names identify the instance, profile, and launching project; TITLE stays
session metadata.  This is the single side-effecting constructor every new-chat
entry point funnels through."
  (let ((directory default-directory)
        (instance (or instance (hermes-instance-resolve)))
        (profile (hermes-chat--clean-profile profile))
        (title (hermes-transport--non-empty-string
                (and title (string-trim title))))
        (buffer (generate-new-buffer hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory directory)
      (hermes-chat-mode)
      (setq hermes-chat--working-directory directory
            hermes-instance instance
            hermes-chat--profile profile)
      (hermes-chat--restore-draft-runtime)
      (when title
        (setq hermes-chat--title title
              hermes-chat--title-manual-p t))
      (rename-buffer
       (hermes-chat--buffer-name profile instance directory) t))
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
  (let* ((instance (hermes-instance-resolve))
         (hermes-dashboard-transport-url (hermes-instance-url instance))
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
  "Return a chat entry for a resumed history MESSAGE, or nil to skip it."
  (let ((role (hermes-transport--scalar-string
               (hermes-transport--get message 'role)))
        (text (hermes-transport--scalar-string
               (hermes-transport--get message 'text))))
    (pcase role
      ("user" (and text (hermes-chat--make-entry 'user text 'done)))
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
        'done)))))

(defun hermes-chat--render-history (messages)
  "Insert prior MESSAGES (from `session.resume') into the transcript."
  (dolist (message messages)
    (when-let* ((entry (hermes-chat--history-entry message)))
      (hermes-chat--insert-entry entry))))

(defun hermes-chat--load-session-history (buffer)
  "Resume BUFFER's session over the dashboard and render its prior messages."
  (with-current-buffer buffer
    (let ((lifetime hermes-chat--lifecycle-generation)
          (client
           (hermes-chat--dashboard-start
            (hermes-chat--transport-callback
             buffer nil t (hermes-chat--next-transport-generation)))))
      (hermes-dashboard-transport-session-resume
       client hermes-chat--session-id
       :cols (hermes-chat--dashboard-cols)
       :profile hermes-chat--profile
       :resolve (lambda (result)
                  (hermes-chat--in-lifetime buffer lifetime
                    (hermes-chat--dashboard-record-session client result)
                    (hermes-chat--render-history
                     (hermes-transport--get result 'messages))
                    (hermes-chat--dashboard-restore-pending-clarify result)))
       :reject (lambda (message)
                 (hermes-chat--in-lifetime buffer lifetime
                   (hermes-chat--insert-local-status
                    (format "Could not load Hermes session history: %s" message)
                    'error)))))))

(defun hermes-chat-resume-session (session-id &optional title profile instance)
  "Open a Hermes chat buffer that resumes dashboard SESSION-ID.
TITLE, when given, records its server title metadata.  PROFILE selects its
owning profile, and INSTANCE selects its owning Hermes instance.  A nil
INSTANCE is resolved from the current context.
Over the dashboard transport the prior messages are fetched and rendered; the
durable session continues on send."
  (interactive (list (read-string "Resume Hermes session id: ")))
  (when (or (null session-id) (string-empty-p session-id))
    (user-error "No Hermes session id to resume"))
  (let ((directory default-directory)
        (instance (or instance (hermes-instance-resolve)))
        (title (hermes-transport--non-empty-string
                (and title (string-trim title))))
        (buffer (generate-new-buffer
                 (hermes-chat--buffer-name profile instance))))
    (with-current-buffer buffer
      (setq default-directory directory)
      (hermes-chat-mode)
      (setq hermes-chat--working-directory directory
            hermes-instance instance
            hermes-chat--session-id session-id
            hermes-chat--profile profile
            hermes-chat--title title))
    (pop-to-buffer-same-window buffer)
    (when (hermes-chat--dashboard-default-transport-p)
      (hermes-chat--load-session-history buffer))
    (with-current-buffer buffer
      (goto-char (or (hermes-chat--input-position) (point-max))))
    buffer))

(defun hermes-chat-send ()
  "Send the current Hermes chat input."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (unless (hermes-chat--point-in-input-p)
    (user-error "Point is not in the Hermes chat input area"))
  (hermes-chat--ensure-submit-allowed)
  (let ((content (hermes-chat--trimmed-input))
        (clarify-key (hermes-chat--pending-clarify-key))
        sent-p)
    (when (string-empty-p content)
      (user-error "No Hermes input to send"))
    (setq sent-p
          (cond
           (clarify-key
            (when (hermes-chat--batch-clarify-p
                   (gethash clarify-key hermes-chat--pending-prompts))
              (user-error
               "Use C-c C-a to answer the batched Hermes clarification"))
            (when (hermes-chat--prompt-response-in-flight-p clarify-key)
              (user-error "Hermes is accepting the previous prompt response"))
            (hermes-chat--delete-input-tail)
            (hermes-chat-respond-to-prompt clarify-key content nil t)
            t)
           ((hermes-chat--parse-slash content)
            (hermes-chat--handle-slash-content content)
            t)
           ((and (hermes-chat--active-turn-p)
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
      (hermes-chat--record-input-history content))))

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
  (with-current-buffer (get-buffer-create "*Hermes Attachments*")
    (hermes-chat-attachments-mode)
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
  (interactive)
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
  "Return members of BUFFERS whose local project identity is ROOT."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (equal (hermes-chat--project-root) root)))
   buffers))

(defun hermes-chat--read-project-buffer (buffers)
  "Read one chat from project-local BUFFERS with completion."
  (let ((completion-extra-properties
         (list :annotation-function #'hermes-chat--switch-annotation)))
    (get-buffer
     (completing-read "Project chat: "
                      (mapcar #'buffer-name buffers) nil t))))

;;;###autoload
(defun hermes-project-chat (&optional new)
  "Switch to a live chat for the current project, or create one.
With prefix argument NEW, always create another project chat."
  (interactive "P")
  (let* ((root (hermes-chat--project-root))
         (buffers (and (not new)
                       (hermes-chat--project-buffers
                        root (hermes-chat--live-buffers)))))
    (cond
     ((null buffers)
      (let ((default-directory root))
        (call-interactively #'hermes-chat)))
     ((null (cdr buffers))
      (pop-to-buffer-same-window (car buffers)))
     (t
      (pop-to-buffer-same-window
       (hermes-chat--read-project-buffer buffers))))))

(defun hermes-chat--switch-annotation (name)
  "Return a shadowed status annotation for chat buffer NAME in the switcher."
  (when-let* ((buffer (get-buffer name)))
    (with-current-buffer buffer
      (let ((detail (string-join
                     (delq nil
                           (list (hermes-chat--dashboard-connection-label)
                                 (hermes-transport--non-empty-string
                                  (plist-get hermes-chat--status-state :activity))))
                     " · ")))
        (and (not (string-empty-p detail))
             (concat "  " (propertize detail 'face 'shadow)))))))

(defun hermes-switch-to-chat (buffer)
  "Switch to a Hermes chat BUFFER chosen with completion."
  (interactive
   (let ((buffers (hermes-chat--live-buffers)))
     (unless buffers
       (user-error "No Hermes chat buffers"))
     (let ((completion-extra-properties
            (list :annotation-function #'hermes-chat--switch-annotation)))
       (list (get-buffer
              (completing-read "Hermes chat: "
                               (mapcar #'buffer-name buffers) nil t))))))
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
  (interactive)
  (hermes-chat--show-session-panel
   #'hermes-dashboard-transport-session-usage
   #'hermes-chat--usage-content))

(defun hermes-chat-show-status ()
  "Show the gateway's rendered `session.status' panel for this session."
  (interactive)
  (hermes-chat--show-session-panel
   #'hermes-dashboard-transport-session-status
   (lambda (result)
     (or (hermes-transport--scalar-string
          (hermes-transport--get result 'output))
         "No status available"))))

(defvar hermes-chat-actions-map)

(keymap-popup-define hermes-chat-actions-map
  "In-chat action menu for `hermes-chat-mode'."
  :description "Hermes Chat Actions"
  :group "Turn"
  "s" ("Steer" hermes-chat-steer-message)
  "i" ("Interrupt" hermes-chat-interrupt)
  "k" ("Interrupt + send" hermes-chat-interrupt-and-send)
  "q" ("Queue message" hermes-chat-queue-message)
  :group "Input"
  "a" ("Answer prompt" hermes-chat-respond-to-prompt)
  "d" ("Cancel prompt" hermes-chat-cancel-prompt)
  "c" ("Show commands" hermes-chat-show-commands)
  "r" ("Refresh commands" hermes-chat-refresh-commands)
  :group "Session"
  "n" ("New chat" hermes-chat)
  "R" ("Rename session" hermes-chat-rename)
  "H" ("Hand off session" hermes-chat-handoff)
  :group "Runtime"
  "m" ("Switch model" hermes-chat-switch-model)
  "e" ("Set reasoning" hermes-chat-set-reasoning)
  "K" ("Connect provider" hermes-chat-connect-provider)
  :group "Workspace"
  "w" ("Set directory" hermes-chat-set-directory
       :inapt-if #'hermes-chat--active-turn-p)
  "b" ("Switch chat buffer" hermes-switch-to-chat)
  "S" ("Sessions" hermes-list-sessions)
  "P" ("Queue side panel" hermes-chat-queue-panel)
  :group "System"
  "x" ("Reconnect socket" hermes-dashboard-reconnect)
  "u" ("Token usage" hermes-chat-show-usage)
  "t" ("Session status" hermes-chat-show-status))

(defvar-keymap hermes-chat-mode-map
  :doc "Keymap for `hermes-chat-mode'."
  "RET" #'hermes-chat-send
  "C-j" #'hermes-chat-newline
  "S-<return>" #'hermes-chat-newline
  "TAB" #'completion-at-point
  "M-p" #'hermes-chat-input-history-previous
  "M-n" #'hermes-chat-input-history-next
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
  (add-hook 'kill-buffer-hook #'hermes-chat--cleanup-buffer nil t)
  (add-hook 'change-major-mode-hook #'hermes-chat--cleanup-buffer nil t)
  (add-hook 'completion-at-point-functions #'hermes-chat--slash-capf nil t)
  (add-hook 'completion-at-point-functions #'hermes-chat--model-capf t t)
  (add-hook 'completion-at-point-functions #'hermes-chat--file-ref-capf t t)
  (add-hook 'after-change-major-mode-hook #'hermes-chat--disable-linters 90 t)
  (hermes-chat--setup-buffer))

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
         (cons '("background" "bg" "btw")
               (lambda (arg) (hermes-chat-background arg)))
         (cons '("steer") (lambda (arg) (hermes-chat-steer-message arg)))
         (cons '("stop") (lambda (_arg) (hermes-chat-stop-processes)))
         (cons '("interrupt" "int")
               (lambda (_arg) (hermes-chat-interrupt)))
         (cons '("clear" "reset") (lambda (_arg) (hermes-chat-clear)))
         (cons '("new") (lambda (arg) (hermes-chat--new-buffer nil arg)))
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
