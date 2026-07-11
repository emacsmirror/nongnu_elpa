;;; hermes-chat.el --- EWOC chat buffer for Hermes  -*- lexical-binding: t; -*-

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

;; ERC/emacs-jabber-shaped chat buffer for hermes-el.  Transcript entries are
;; EWOC nodes before a read-only prompt; the input tail remains writable.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'diff-mode)
(require 'ewoc)
(require 'goto-addr)
(require 'keymap-popup)
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
  "Title sent when creating a dashboard transport chat session."
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

(defvar-local hermes-chat--background-counter 0
  "Number of background (`/btw') tasks launched from this chat buffer.")

(defvar-local hermes-chat--background-tasks nil
  "Alist mapping a background task id to its (:number :preview) plist.
Populated when `prompt.background' accepts a task and consumed when the matching
`background.complete' event arrives, so the result entry can show the task's
number and the prompt that launched it.")

;; Connection state owned by `hermes-chat-buffer'; re-declared here for the
;; byte-compiler.  See that file for the authoritative defvar-locals and docs.
(defvar hermes-chat--process)
(defvar hermes-chat--status-state)
(defvar hermes-chat--active-tools)
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-session-ready-p)
(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--session-id)

(defvar-local hermes-chat--profile nil
  "Profile name for this chat's dashboard session, or nil for the default.")

(defvar-local hermes-chat--model nil
  "Model id reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--agent-name nil
  "Agent/profile name reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--context nil
  "Context-window usage plist (:used :max :percent) for the header.")

(defvar-local hermes-chat--runtime-flags nil
  "Runtime flag plist (:reasoning-effort :fast :yolo) from `session.info'.
Shown as annotations after the model in the chat header.")

;; Owned by `hermes-chat-buffer'; declared here for the byte-compiler.
(defvar hermes-chat--pending-assistant-id)

;; Owned by `hermes-chat-buffer'; declared here for the byte-compiler.
(defvar hermes-chat--transport-generation)

(defvar hermes-chat--dashboard-detached-assistant-id)
(defvar hermes-chat--dashboard-stream-assistant-id)
(defvar hermes-chat--dashboard-suppress-stream-p)

(defvar-local hermes-chat--title nil
  "Human title for this chat session.
Set by `hermes-chat-rename'.  Shown in the buffer name and reported to the
dashboard; nil falls back to the buffer name.")

(defvar-local hermes-chat--title-manual-p nil
  "Non-nil when the user set this chat's title via `hermes-chat-rename'.
A manual title is preserved against the automatic session-title refresh.")

;; Queue and stream state owned by `hermes-chat-buffer'; re-declared here for
;; the byte-compiler.
(defvar hermes-chat--queued-message)
(defvar hermes-chat--queued-display)
(defvar hermes-chat--draining-queued-message-p)

(defconst hermes-chat--transient-entry-roles '(status progress tool)
  "Entry roles used for compact transport status/progress lines.")

(defconst hermes-chat--unknown-event-raw-preview-width 180
  "Maximum width for raw unknown transport event previews.")

(defun hermes-chat--unknown-event-content (event)
  "Return visible diagnostic text for unknown transport EVENT."
  (let* ((name (or (hermes-chat--event-string event '(:event)) "unnamed"))
         (raw (plist-get event :raw))
         (preview (and raw
                       (truncate-string-to-width
                        (format "%S" raw)
                        hermes-chat--unknown-event-raw-preview-width
                        nil nil "…"))))
    (string-join
     (delq nil (list (format "Unknown Hermes transport event: %s" name)
                     (and preview (format "raw: %s" preview))))
     "\n")))

(defun hermes-chat--clear-active-tools ()
  "Forget currently active tools in the chat header."
  (when (hash-table-p hermes-chat--active-tools)
    (clrhash hermes-chat--active-tools)))

(defun hermes-chat--active-tool-summaries ()
  "Return active tool summaries for the chat header."
  (let (summaries)
    (when (hash-table-p hermes-chat--active-tools)
      (maphash (lambda (_key summary) (push summary summaries))
               hermes-chat--active-tools))
    (nreverse summaries)))

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

(defun hermes-chat--active-tools-table ()
  "Return the active-tools hash for this buffer, creating it when absent.
This feeds the dashboard's per-session tool list via
`hermes-chat--dashboard-snapshot'; the chat header itself never shows tools."
  (unless (hash-table-p hermes-chat--active-tools)
    (setq hermes-chat--active-tools (make-hash-table :test 'equal)))
  hermes-chat--active-tools)

(defun hermes-chat--header-activity-for-event (event)
  "Return a compact activity string for transport EVENT."
  (hermes-transport--non-empty-string
   (or (hermes-chat--transport-entry-content event)
       (hermes-chat--event-string event '(:content :text :preview :event)))))

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
      "Ready"
    (or (hermes-chat--header-activity-for-event event) "Working")))

(defun hermes-chat--session-info-event-p (event)
  "Return non-nil when EVENT is a `session.info' status event."
  (equal (hermes-chat--event-string event '(:event)) "session.info"))

(defun hermes-chat--message-start-status-event-p (event)
  "Return non-nil when EVENT is low-value message-start status noise."
  (and (eq (plist-get event :type) 'status)
       (and-let* ((name (hermes-chat--event-string event '(:event))))
         (member (downcase name)
                 '("message.start" "message_start"
                   "message.started" "message_started")))
       (equal (hermes-chat--status-name (plist-get event :status)) "started")
       (hermes-chat--message-start-noise-content-p
        (hermes-chat--event-string event '(:content :text :preview)))))

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

(defun hermes-chat--turn-header-props (event)
  "Return header props for any header-affecting EVENT, or nil for none."
  (pcase (plist-get event :type)
    ('status (hermes-chat--status-header-props event))
    ('commentary '(:status running :activity "Reasoning"))
    ('thinking (hermes-chat--thinking-header-props event))
    ('diff '(:status running :activity "Reviewing diff"))
    ('done (list :status 'ready :activity "Ready"
                 :usage (plist-get event :usage)))
    ('error (list :status (hermes-chat--error-status event)
                  :activity (or (hermes-chat--event-string event '(:content :error))
                                "Transport error")))
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

(defun hermes-chat--transcript-event-p (event)
  "Return non-nil when EVENT should render a compact transcript entry.
`session.info' feeds the header only, and `notification.clear' retracts a
keyed notice without carrying text, so neither becomes an entry."
  (pcase (plist-get event :type)
    ('status (not (or (hermes-chat--session-info-event-p event)
                      (equal (hermes-chat--event-string event '(:event))
                             "notification.clear"))))
    ((or 'progress 'tool 'commentary 'diff 'unknown) t)))

(defun hermes-chat--turn-entry-effect (event)
  "Return an (upsert-entry . EVENT) transcript effect for EVENT, or nil.  Pure."
  (and (hermes-chat--transcript-event-p event)
       (cons 'upsert-entry event)))

(defun hermes-chat--turn-done-effects (event status)
  "Return the ordered effect list for a `done' EVENT with header STATUS.
`refresh-header' precedes the lifecycle so the header settles before `drain'
re-submits any queued turn."
  (delq nil
        (list '(clear-tools)
              (cons 'refresh-header status)
              (cons 'clear-prompts event)
              (cons 'mark-done (plist-get event :content))
              (and-let* ((warning (plist-get event :warning)))
                (cons 'warning warning))
              '(drop-thinking)
              '(settle . done)
              '(finish)
              '(clear-pending)
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
        '(drain)))

(defun hermes-chat--turn-error-effects (event status)
  "Return the ordered effect list for an `error' EVENT with header STATUS."
  (let ((estatus (hermes-chat--error-status event))
        (content (let ((c (or (plist-get event :content) "")))
                   (if (string-empty-p c) "Transport error" c))))
    (list '(clear-tools)
          (cons 'refresh-header status)
          (cons 'clear-prompts event)
          (cons 'append-error (cons content estatus))
          (cons 'settle estatus)
          '(finish)
          '(clear-pending)
          '(drain))))

(defun hermes-chat--turn-reduce (state event now)
  "Return (NEW-STATE . EFFECTS) for domain EVENT applied to STATE at time NOW.
Pure: no buffer, EWOC, process, header, or message side effects.  EFFECTS is an
ordered list the boundary replays: a header change leads with `refresh-header',
`done'/`error' append the turn lifecycle, and tool/transcript events emit deltas
and `upsert-entry'.  Other types return (STATE)."
  (pcase (plist-get event :type)
    ((or 'status 'commentary 'thinking 'diff)
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
    (_ (cons state nil))))

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
           hermes-chat--process nil))
    ('(drain) (hermes-chat--drain-queued-message))
    (`(append-delta . ,content)
     (unless (hermes-chat--thinking-echo-delta-p assistant-id content)
       (hermes-chat--append-assistant-content assistant-id content 'streaming)))))

(defun hermes-chat--run-turn-reducer (assistant-id event)
  "Reduce EVENT, persist the new turn state, and apply its effects in order.
Captures session identity first.  ASSISTANT-ID scopes the transcript effects.
The boundary persists NEW-STATE and replays the effects; it makes no decisions
of its own."
  (hermes-chat--capture-session-identity event)
  (pcase-let ((`(,new-state . ,effects)
               (hermes-chat--turn-reduce
                (hermes-chat--turn-state :status-state hermes-chat--status-state)
                event (current-time))))
    (setq hermes-chat--status-state
          (hermes-chat--turn-state-get new-state :status-state))
    (dolist (effect effects)
      (hermes-chat--apply-turn-effect assistant-id effect))))

(defun hermes-chat--update-header-for-event (event)
  "Update only header and tool state from transport EVENT, leaving the transcript.
Used where a synthesized header event must not insert a transcript entry."
  (hermes-chat--run-turn-reducer nil event))

(defun hermes-chat--render-turn-event (assistant-id event)
  "Render transport EVENT for ASSISTANT-ID: header, tools, and transcript entry."
  (hermes-chat--run-turn-reducer assistant-id event))

(defun hermes-chat--header-agent-name ()
  "Return the agent/profile name shown in the chat header."
  (or (hermes-transport--non-empty-string hermes-chat--agent-name)
      (hermes-transport--non-empty-string hermes-chat--profile)
      "Hermes"))

(defun hermes-chat--header-detail (label)
  "Return the live detail to append after LABEL in the header, or nil.
The activity is used, with a leading copy of LABEL stripped so a label-prefixed
activity is not shown twice.  Tool commands are deliberately not surfaced here:
the header keeps the kawaii thinking status as its only live detail, while the
transcript carries the full tool detail."
  (when-let* ((activity (hermes-transport--non-empty-string
                         (plist-get hermes-chat--status-state :activity))))
    (if (string-prefix-p (downcase label) (downcase activity))
        (hermes-transport--non-empty-string
         (string-trim (substring activity (length label)) "[-: ]+"))
      activity)))

(defun hermes-chat--header-status-segment ()
  "Return the propertized status segment: icon, label, and live detail.
The label carries the high-level state (Ready/Running/...) and an active tool
or distinct activity is appended so the live detail is not lost.  The `thinking'
state is shown bare (kawaii face plus verb, no icon or label) since the face is
self-explanatory."
  (let ((status (plist-get hermes-chat--status-state :status)))
    (if (eq status 'thinking)
        (propertize (or (hermes-transport--non-empty-string
                         (plist-get hermes-chat--status-state :activity))
                        "Thinking")
                    'face (hermes-chat--header-status-face 'running))
      (let* ((label (hermes-chat--header-status-label status))
             (detail (hermes-chat--header-detail label)))
        (propertize
         (format "%s %s" (hermes-chat--status-icon status)
                 (if detail (format "%s: %s" label detail) label))
         'face (hermes-chat--header-status-face status))))))

(defun hermes-chat--header-model-segment ()
  "Return the header model segment with runtime flag annotations, or nil.
The flags come from `session.info': reasoning effort, fast/priority tier,
and approval bypass (YOLO)."
  (and-let* ((model (hermes-transport--non-empty-string hermes-chat--model)))
    (let ((flags (delq nil
                       (list (plist-get hermes-chat--runtime-flags
                                        :reasoning-effort)
                             (and (plist-get hermes-chat--runtime-flags :fast)
                                  "fast")
                             (and (plist-get hermes-chat--runtime-flags :yolo)
                                  "YOLO")))))
      (if flags
          (format "%s (%s)" model (string-join flags ", "))
        model))))

(defun hermes-chat--header-line ()
  "Return the chat buffer header line: agent, status, model, and context."
  (let* ((parts (delq nil
                      (list (propertize (hermes-chat--header-agent-name)
                                        'face 'mode-line-emphasis)
                            (hermes-chat--header-status-segment)
                            (hermes-chat--header-model-segment)
                            (hermes-chat--format-context hermes-chat--context))))
         (text (concat " " (string-join parts "  |  ") " "))
         (width (max 20 (window-total-width))))
    ;; Double % so the context percentage is not read as a mode-line %-spec.
    (string-replace "%" "%%" (truncate-string-to-width text width nil nil "…"))))


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

(defun hermes-chat--trimmed-input ()
  "Return the current input tail trimmed for sending."
  (string-trim (hermes-chat-input-string)))

(defun hermes-chat--replace-input-tail (content)
  "Replace the current writable input tail with CONTENT."
  (hermes-chat--delete-input-tail)
  (goto-char (hermes-chat--input-position))
  (insert content))

(defun hermes-chat--append-input-tail (content)
  "Append CONTENT to the writable input tail, preserving existing draft text."
  (goto-char (point-max))
  (unless (string-suffix-p "\n" (hermes-chat-input-string))
    (insert "\n"))
  (insert content))

(defun hermes-chat--preserve-control-content (content)
  "Keep busy-control CONTENT recoverable after a dashboard bootstrap error."
  (when-let* ((text (hermes-transport--non-empty-string content)))
    (if (string-empty-p (string-trim (hermes-chat-input-string)))
        (hermes-chat--replace-input-tail text)
      (if (not hermes-chat--queued-message)
          (hermes-chat--queue-content
           text "Preserved busy-control text after dashboard error")
        (hermes-chat--append-input-tail text)
        (hermes-chat--insert-local-status
         "Restored busy-control text in input tail after dashboard error"
         'error)))))

(defun hermes-chat--queue-or-submit-content (content &optional display)
  "Queue CONTENT during an active turn, otherwise submit it now.
DISPLAY is the compact user-turn text to show instead of CONTENT."
  (if (hermes-chat--active-turn-p)
      (hermes-chat--queue-content content nil display)
    (hermes-chat--submit-content content display)))

(defun hermes-chat-input-string ()
  "Return the current input tail as a plain string."
  (let ((pos (hermes-chat--input-position)))
    (unless pos
      (user-error "No Hermes chat input marker in this buffer"))
    (buffer-substring-no-properties pos (point-max))))

(defun hermes-chat-newline ()
  "Insert a literal newline in the Hermes chat input tail.
Outside the tail, move to the end of the draft first so the newline
extends the input instead of prepending a blank line to it."
  (interactive)
  (unless (hermes-chat--point-in-input-p)
    (goto-char (point-max)))
  (insert "\n"))

(defun hermes-chat--drain-queued-message ()
  "Submit one queued message after the active turn settles."
  (when (and hermes-chat--queued-message
             (not hermes-chat--pending-assistant-id)
             (not hermes-chat--draining-queued-message-p))
    (let ((content hermes-chat--queued-message)
          (display hermes-chat--queued-display))
      (setq hermes-chat--queued-message nil
            hermes-chat--queued-display nil
            hermes-chat--draining-queued-message-p t)
      (unwind-protect
          (hermes-chat--submit-content content display)
        (setq hermes-chat--draining-queued-message-p nil)))))

(defun hermes-chat--submit-content (content &optional display)
  "Submit CONTENT as a new user turn, echoing DISPLAY when non-nil.
DISPLAY lets a slash skill send its full payload while showing a compact line."
  (when (hermes-chat--active-turn-p)
    (user-error "%s" (hermes-chat--busy-message)))
  (let* ((user-entry (hermes-chat--make-entry 'user (or display content) 'done))
         (assistant-entry (hermes-chat--make-entry 'assistant "" 'pending))
         (assistant-id (plist-get assistant-entry :id))
         (buffer (current-buffer))
         (dashboard-p (hermes-chat--dashboard-default-transport-p))
         (generation (hermes-chat--next-transport-generation)))
    (hermes-chat--insert-entry user-entry)
    (hermes-chat--insert-entry assistant-entry)
    (hermes-chat--clear-active-tools)
    (hermes-chat--set-header-state
     :status 'pending :activity "Waiting for Hermes"
     :assistant-id assistant-id :last-tool nil :started (current-time))
    (setq hermes-chat--pending-assistant-id assistant-id
          hermes-chat--dashboard-stream-assistant-id (and dashboard-p assistant-id)
          hermes-chat--dashboard-suppress-stream-p nil)
    (condition-case err
        (setq hermes-chat--process
              (hermes-chat--send-prompt
               content
               (hermes-chat--transport-callback
                buffer assistant-id dashboard-p generation)))
      (error
       (hermes-chat--handle-transport-event
        assistant-id (list :type 'error :content (error-message-string err)))
       (message "Hermes transport failed: %s" (error-message-string err))))))


(defun hermes-chat-queue-message (&optional message)
  "Queue MESSAGE to send after the active Hermes turn, or send now if idle."
  (interactive)
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
  (hermes-chat--queue-content content "Queued next message after steer fallback"))

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
  (hermes-chat--remove-entry id)
  (hermes-chat--steer-rejected content message))

(defun hermes-chat--steer-active-turn (content buffer)
  "Steer active dashboard turn with CONTENT in BUFFER, or queue when unsupported."
  (if (not (hermes-chat--dashboard-session-attached-p))
      (hermes-chat--queue-content content "Steer unavailable; queued next message")
    (let ((id (hermes-chat--steer-pending-status content)))
      (hermes-dashboard-transport-session-steer
       hermes-chat--dashboard-client content
       :session-id hermes-chat--dashboard-active-session-id
       :resolve (lambda (result)
                  (hermes-chat--in-buffer buffer
                    (if (equal (hermes-chat--status-name
                                (hermes-chat--result-string result 'status))
                               "rejected")
                        (hermes-chat--steer-failed id content "rejected")
                      (hermes-chat--steer-acknowledged id content))))
       :reject (lambda (err)
                 (hermes-chat--in-buffer buffer
                   (hermes-chat--steer-failed id content err)))))))

(defun hermes-chat--steer-or-submit (content buffer)
  "Steer active turn with CONTENT in BUFFER, or submit CONTENT when idle."
  (if (hermes-chat--active-turn-p)
      (hermes-chat--steer-active-turn content buffer)
    (hermes-chat--submit-content content)))

(defun hermes-chat--dashboard-steer-or-submit (content buffer)
  "Resume stored dashboard session in BUFFER before steering or submitting CONTENT."
  (if (hermes-chat--dashboard-stored-session-needs-resume-p)
      (hermes-chat--with-dashboard-session
       content buffer
       (lambda (_live-client)
         (hermes-chat--steer-or-submit content buffer)))
    (hermes-chat--steer-or-submit content buffer)))

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
     (hermes-chat--pending-assistant-node))))

(defun hermes-chat-steer-message (&optional message)
  "Steer the active dashboard run with MESSAGE, falling back to queue."
  (interactive)
  (let ((content (string-trim (or message (hermes-chat-input-string))))
        (buffer (current-buffer)))
    (when (string-empty-p content)
      (user-error "No Hermes input to steer"))
    (unless message
      (hermes-chat--delete-input-tail))
    (hermes-chat--dashboard-steer-or-submit content buffer)))

(defun hermes-chat-interrupt ()
  "Request interruption of the active dashboard run."
  (interactive)
  (unless (hermes-chat--active-turn-p)
    (user-error "No active Hermes run to interrupt"))
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support interrupt"))
  (let ((buffer (current-buffer)))
    (hermes-dashboard-transport-session-interrupt
     hermes-chat--dashboard-client
     :session-id hermes-chat--dashboard-active-session-id
     :resolve (lambda (_result)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--insert-local-status
                   "Interrupt requested" 'interrupted)
                  (hermes-chat--set-header-state
                   :status 'interrupted :activity "Interrupt requested")))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (hermes-chat--command-error message))))))

(defun hermes-chat-interrupt-and-send (&optional message)
  "Interrupt the active run, then queue MESSAGE for the next turn when non-empty.
MESSAGE defaults to the input tail.  The interrupt fires first and
unconditionally, so an empty input still stops the run instead of erroring."
  (interactive)
  (unless (hermes-chat--active-turn-p)
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
  (hermes-chat--stop-dashboard-client)
  (hermes-chat--insert-local-status "Session disconnected" 'disconnected)
  (hermes-chat--set-header-state :status 'disconnected :activity "Disconnected"))

;;;###autoload
(defun hermes-dashboard-reconnect ()
  "Restart this idle chat's shared Hermes dashboard WebSocket.
The durable session id is preserved.  After the replacement socket reports
ready, the chat resumes that durable session over a fresh live session id."
  (interactive)
  (unless (hermes-chat--dashboard-default-transport-p)
    (user-error "Hermes dashboard transport is not enabled for this chat"))
  (unless (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (user-error "This Hermes chat has no live dashboard socket to reconnect"))
  (when (hermes-chat--active-turn-p)
    (user-error "Cannot reconnect while a turn is active; interrupt or wait first"))
  (hermes-dashboard-transport-reconnect
   hermes-chat--dashboard-client "Hermes dashboard socket reconnecting"))

;;;###autoload
(defalias 'hermes-reconnect #'hermes-dashboard-reconnect)

(defun hermes-chat-stop-processes ()
  "Stop background/tool processes for this chat via `process.stop'.
This does not interrupt the current model turn -- use `hermes-chat-interrupt'
for that."
  (interactive)
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support stopping processes"))
  (let ((buffer (current-buffer)))
    (hermes-dashboard-transport-process-stop
     hermes-chat--dashboard-client
     :resolve (lambda (result)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--insert-local-status
                   (format "Stopped %s background process(es)"
                           (or (hermes-transport--get result 'killed) 0))
                   'done)))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (hermes-chat--command-error message))))))

(defun hermes-chat--reset-transcript ()
  "Tear down the live session and re-initialize this chat buffer empty.
Stops any live dashboard client, clears the EWOC transcript and header, and
forgets both the live and durable session ids so the next send starts fresh."
  (hermes-chat--stop-dashboard-client)
  (hermes-chat--setup-buffer))

(defun hermes-chat-clear ()
  "Clear this chat's transcript and start a fresh Hermes session in place."
  (interactive)
  (when (y-or-n-p "Clear this Hermes conversation and transcript? ")
    (hermes-chat--reset-transcript)
    (hermes-chat--insert-local-status "Session cleared" 'done)))

(defun hermes-chat--new-buffer (&optional profile title)
  "Create, display, and return a fresh chat buffer for PROFILE and TITLE.
PROFILE nil means the dashboard default; a non-empty TITLE pins a manual title.
The buffer is named `*Hermes: PROFILE*' (or `*Hermes: PROFILE: TITLE*') so chats
stay distinct before a server title arrives.  This is the single side-effecting
constructor every new-chat entry point funnels through."
  (let ((profile (hermes-chat--clean-profile profile))
        (title (hermes-transport--non-empty-string
                (and title (string-trim title))))
        (buffer (generate-new-buffer hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (hermes-chat-mode)
      (setq hermes-chat--profile profile)
      (when title
        (setq hermes-chat--title title
              hermes-chat--title-manual-p t))
      (rename-buffer (hermes-chat--buffer-name-for-title profile title) t))
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
  "Return a live dashboard client from any Hermes chat buffer, or nil."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'hermes-chat-mode)
                    (hermes-chat--dashboard-client-live-p
                     hermes-chat--dashboard-client)
                    hermes-chat--dashboard-client)))
           (buffer-list)))

(defun hermes-chat--profile-list-payload ()
  "Return dashboard profile metadata, preferring the warmed cache.
`hermes' warms a per-URL profile cache on launch (see
`hermes-dashboard-transport-profile-list-async'), so cold-start completion has
candidates without blocking.  On a cache miss this falls back to a synchronous
fetch only when a live chat client already supplies a cheap session token; with
neither it returns nil so the caller prompts for a profile manually and never
spawns a transient dashboard."
  (or (hermes-dashboard-transport-cached-profile-list)
      (when-let* ((client (hermes-chat--existing-dashboard-client)))
        (hermes-dashboard-transport-profile-list client))))

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
    (let ((client (hermes-chat--dashboard-start #'ignore)))
      (hermes-dashboard-transport-session-resume
       client hermes-chat--session-id
       :cols (hermes-chat--dashboard-cols)
       :resolve (lambda (result)
                  (hermes-chat--in-buffer buffer
                    (hermes-chat--dashboard-record-session client result)
                    (hermes-chat--render-history
                     (hermes-transport--get result 'messages))))
       :reject (lambda (message)
                 (hermes-chat--in-buffer buffer
                   (hermes-chat--insert-local-status
                    (format "Could not load Hermes session history: %s" message)
                    'error)))))))

(defun hermes-chat-resume-session (session-id &optional title)
  "Open a Hermes chat buffer that resumes dashboard SESSION-ID.
TITLE, when given, names the buffer.  Over the dashboard transport the prior
messages are fetched and rendered; the durable session continues on send."
  (interactive (list (read-string "Resume Hermes session id: ")))
  (when (or (null session-id) (string-empty-p session-id))
    (user-error "No Hermes session id to resume"))
  (let ((buffer (generate-new-buffer
                 (if (and title (not (string-empty-p title)))
                     (format "*Hermes: %s*" title)
                   hermes-chat-buffer-name))))
    (with-current-buffer buffer
      (hermes-chat-mode)
      (setq hermes-chat--session-id session-id))
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
  (let ((content (hermes-chat--trimmed-input)))
    (when (string-empty-p content)
      (user-error "No Hermes input to send"))
    (if (hermes-chat--parse-slash content)
        (hermes-chat--handle-slash-content content)
      (cond
       ((and (hermes-chat--active-turn-p) hermes-chat--queued-message)
        (user-error "A Hermes message is already queued"))
       ((hermes-chat--active-turn-p)
        (hermes-chat--delete-input-tail)
        (hermes-chat--queue-content content))
       (t
        (hermes-chat--delete-input-tail)
        (hermes-chat--submit-content content))))))

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

(defun hermes-chat--buffer-name-for-title (profile title)
  "Return a chat buffer name from PROFILE and TITLE.
PROFILE nil means the default profile.  A nil or empty TITLE yields a name with
just the profile, so buffers stay distinct before a session title arrives."
  (let ((profile (or profile "default")))
    (if (and title (not (string-empty-p title)))
        (format "*Hermes: %s: %s*" profile title)
      (format "*Hermes: %s*" profile))))

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

(defun hermes-chat--live-buffers ()
  "Return all live Hermes chat buffers in `buffer-list' order."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer (derived-mode-p 'hermes-chat-mode)))
   (buffer-list)))

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
(declare-function hermes-list-sessions "hermes-sessions")

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
  (let ((buffer (current-buffer)))
    (funcall fetch (hermes-chat--dashboard-control-client)
             :session-id hermes-chat--dashboard-active-session-id
             :resolve (lambda (result)
                        (hermes-chat--in-buffer buffer
                          (hermes-chat--insert-local-status
                           (funcall render result) 'done)))
             :reject (lambda (message)
                       (hermes-chat--in-buffer buffer
                         (hermes-chat--command-error message))))))

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
  :group "Prompt"
  "a" ("Answer prompt" hermes-chat-respond-to-prompt)
  "d" ("Cancel prompt" hermes-chat-cancel-prompt)
  :group "Session"
  "n" ("New chat" hermes-chat)
  "m" ("Switch model" hermes-chat-switch-model)
  "K" ("Connect provider" hermes-chat-connect-provider)
  "R" ("Rename session" hermes-chat-rename)
  "H" ("Hand off session" hermes-chat-handoff)
  "x" ("Reconnect socket" hermes-dashboard-reconnect)
  "b" ("Switch chat buffer" hermes-switch-to-chat)
  "S" ("Sessions" hermes-list-sessions)
  "u" ("Token usage" hermes-chat-show-usage)
  "t" ("Session status" hermes-chat-show-status)
  :group "Commands"
  "c" ("Show commands" hermes-chat-show-commands)
  "r" ("Refresh commands" hermes-chat-refresh-commands))

(defvar-keymap hermes-chat-mode-map
  :doc "Keymap for `hermes-chat-mode'."
  "RET" #'hermes-chat-send
  "C-j" #'hermes-chat-newline
  "S-<return>" #'hermes-chat-newline
  "TAB" #'completion-at-point
  "C-c C-i" #'hermes-chat-interrupt
  "C-c C-k" #'hermes-chat-interrupt-and-send
  "C-c C-q" #'hermes-chat-queue-message
  "C-c C-s" #'hermes-chat-steer-message
  "C-c C-a" #'hermes-chat-respond-to-prompt
  "C-c C-d" #'hermes-chat-cancel-prompt
  "C-c C-o" #'hermes-chat-actions-map-popup
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
  (add-hook 'completion-at-point-functions #'hermes-chat--slash-capf nil t)
  (add-hook 'after-change-major-mode-hook #'hermes-chat--disable-linters 90 t)
  (hermes-chat--setup-buffer))

;;;###autoload
(defun hermes-chat (&optional profile)
  "Open a new Hermes chat buffer under agent PROFILE.
Interactively prompt for PROFILE (blank uses the dashboard default).  Each call
opens a distinct buffer named after the profile -- and, once the session is
titled, after that title -- so chats stay filterable with
`hermes-switch-to-chat'."
  (interactive (list (hermes-chat--read-profile)))
  (hermes-chat--new-buffer profile))


;; Native slash commands are wired here, where the commands live; the
;; registry variable and dispatch stay in `hermes-chat-slash' so the
;; lower layer needs no declare-function back into this file.
(setq hermes-chat--native-slash-commands
  (list
   (cons '("commands") (lambda (_arg) (hermes-chat-show-commands)))
   (cons '("queue" "q")
         (lambda (arg) (hermes-chat--dashboard-dispatch-command "queue" arg)))
   (cons '("background" "bg" "btw")
         (lambda (arg) (hermes-chat-background arg)))
   (cons '("steer") (lambda (arg) (hermes-chat-steer-message arg)))
   (cons '("stop") (lambda (_arg) (hermes-chat-stop-processes)))
   (cons '("interrupt" "int") (lambda (_arg) (hermes-chat-interrupt)))
   (cons '("clear" "reset") (lambda (_arg) (hermes-chat-clear)))
   (cons '("new") (lambda (arg) (hermes-chat--new-buffer nil arg)))
   (cons '("model") (lambda (_arg) (hermes-chat-switch-model)))
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
   (cons '("sessions") (lambda (_arg) (hermes-list-sessions)))))

(provide 'hermes-chat)
;;; hermes-chat.el ends here
