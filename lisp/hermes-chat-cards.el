;;; hermes-chat-cards.el --- Live worker cards in the chat transcript -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The transcript row that spawned work grows into a live card in place.  A
;; `delegate_task' row shows the tree of its workers, each with its latest
;; activity, and folds to one line once the batch settles.  A background
;; `terminal' row shows its process's last output line, then the exit
;; verdict.  Cards are ordinary transcript text above the composer.
;;
;; Everything shown derives from the chat's work roster
;; (`hermes-chat--work-roster').  Each card keeps the rows it last saw on its
;; entry, so a settled card stays the durable record after the roster forgets
;; its workers.  `hermes-chat--card-text' is a pure function of the entry;
;; the refresh repaints one card's node only when that text changes.
;;
;; A global mode-line segment lists the chats that are not displayed and
;; have live or unseen failed work.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'hermes-chat-format)
(require 'hermes-chat-render)
(require 'hermes-chat-buffer)

(declare-function hermes-chat--switch-project "hermes-chat" (buffer-or-name))
(declare-function hermes-switch-to-chat "hermes-chat" (buffer))
(declare-function hermes-chat--prompt-indicator-install "hermes-chat-prompts"
                  (&optional segment))
(declare-function hermes-chat--prompt-indicator-uninstall "hermes-chat-prompts"
                  (&optional segment))
(defvar hermes-chat-work-changed-hook)

;;;; Faces

(defface hermes-chat-card-queued '((t :inherit shadow))
  "Face for a queued worker in a transcript card."
  :group 'hermes)

(defface hermes-chat-card-done '((t :inherit success))
  "Face for a finished worker or a clean process exit."
  :group 'hermes)

(defface hermes-chat-card-failed '((t :inherit error))
  "Face for a failed worker or process."
  :group 'hermes)

(defface hermes-chat-card-interrupted '((t :inherit warning))
  "Face for an interrupted worker or a killed process."
  :group 'hermes)

;;;; Pure presentation

(defconst hermes-chat--card-goal-width 56
  "Columns a worker goal may take before it is truncated.")

(defconst hermes-chat--card-activity-width 72
  "Columns an activity or output line may take before it is truncated.")

(defconst hermes-chat--card-states
  '((running "●" hermes-work-running)
    (queued "◌" hermes-chat-card-queued)
    (done "✓" hermes-chat-card-done)
    (failed "✗" hermes-chat-card-failed)
    (interrupted "⊘" hermes-chat-card-interrupted)
    (unknown "?" shadow))
  "Display state to glyph and face, in summary count order.
Glyphs carry the state without faces; faces only reinforce.")

(defun hermes-chat--card-state (row)
  "Return the display state of roster ROW."
  (pcase (plist-get row :state)
    ((and state (or 'running 'queued)) state)
    ((or 'completed 'done 'complete 'success 'succeeded) 'done)
    ((or 'interrupted 'cancelled 'canceled 'killed) 'interrupted)
    ((or 'failed 'error 'failure 'timeout 'lost 'failed_start) 'failed)
    (_ 'unknown)))

(defun hermes-chat--card-active-state-p (state)
  "Return non-nil when display STATE is still live."
  (memq state '(running queued)))

(defun hermes-chat--card-glyph (state)
  "Return the propertized glyph for display STATE."
  (pcase-let ((`(,glyph ,face) (alist-get state hermes-chat--card-states)))
    (propertize glyph 'face face)))

(defun hermes-chat--card-face (state)
  "Return the face for display STATE."
  (cadr (alist-get state hermes-chat--card-states)))

(defun hermes-chat--card-dim (text)
  "Return TEXT in `shadow'."
  (propertize text 'face 'shadow))

(defun hermes-chat--card-fit (text width)
  "Return TEXT on one line, truncated to WIDTH columns with an ellipsis."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[\t\n\r]+" " " (or text "")))
   width nil nil "…"))

(defun hermes-chat--card-right (text &optional face)
  "Return TEXT in FACE, right-aligned to the window edge."
  (if (string-empty-p text)
      ""
    (concat (propertize " " 'display
                        `(space :align-to (- right ,(1+ (string-width text)))))
            (propertize text 'face (or face 'shadow)))))

(defun hermes-chat--card-clock (seconds)
  "Return SECONDS as M:SS, or H:MM:SS from one hour."
  (let ((s (max 0 (floor seconds))))
    (if (>= s 3600)
        (format "%d:%02d:%02d" (/ s 3600) (% (/ s 60) 60) (% s 60))
      (format "%d:%02d" (/ s 60) (% s 60)))))

(defun hermes-chat--card-plural (count noun)
  "Return COUNT followed by NOUN, pluralized."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun hermes-chat--card-row-time (row state now)
  "Return the right-hand time for worker ROW in display STATE at NOW."
  (let ((started (plist-get row :started))
        (finished (plist-get row :finished)))
    (cond ((eq state 'queued) "queued")
          ((not (numberp started)) "")
          ((eq state 'running) (hermes-chat--card-clock (- now started)))
          ((numberp finished) (hermes-chat--card-clock (- finished started)))
          (t ""))))

(defun hermes-chat--card-tool-text (tool target)
  "Return TOOL with its emoji and optional TARGET."
  (concat (hermes-chat--tool-emoji tool) " " tool
          (if target (concat " " target) "")))

(defun hermes-chat--card-activity (row state)
  "Return worker ROW's activity line text in display STATE, or nil.
A running worker shows its last tool and tool count; an ended one the first
line of its summary or error."
  (let ((text
         (pcase state
           ('running
            (let ((tool (hermes-chat--work-last-tool row))
                  (count (plist-get row :tool-count)))
              (string-join
               (delq nil (list (and tool (hermes-chat--card-tool-text
                                          (car tool) (cdr tool)))
                               (and (natnump count) (> count 0)
                                    (hermes-chat--card-plural count "tool"))))
               " · ")))
           ((or 'done 'failed 'interrupted)
            (or (car (split-string (or (plist-get row :summary) (plist-get row :error) "")
                                   "\n" t "[ \t]+"))
                (and (eq state 'interrupted) "interrupted"))))))
    (and text (not (string-empty-p text)) text)))

(defun hermes-chat--card-counts (states)
  "Return the summary count parts for display STATES, zeros omitted."
  (delq nil
        (mapcar (lambda (spec)
                  (let ((count (seq-count (lambda (state) (eq state (car spec))) states)))
                    (when (> count 0)
                      (propertize (format "%d %s" count (car spec))
                                  'face (if (memq (car spec) '(failed interrupted))
                                            (nth 2 spec)
                                          'shadow)))))
                hermes-chat--card-states)))

(defun hermes-chat--card-settled-p (status states)
  "Return non-nil when a delegation row in STATUS with worker STATES settled."
  (and (not (hermes-chat--active-status-p status))
       (not (seq-some #'hermes-chat--card-active-state-p states))))

(defun hermes-chat--card-verdict-state (states)
  "Return the settled glyph state for a batch whose workers ended in STATES."
  (cond ((memq 'failed states) 'failed)
        ((memq 'interrupted states) 'interrupted)
        ((memq 'unknown states) 'unknown)
        (t 'done)))

(defun hermes-chat--card-elapsed (rows settled now)
  "Return the batch elapsed text for worker ROWS at NOW, or the empty string.
A SETTLED batch ends at its last finish."
  (let ((starts (seq-filter #'numberp (mapcar (lambda (row) (plist-get row :started)) rows)))
        (ends (seq-filter #'numberp (mapcar (lambda (row) (plist-get row :finished)) rows))))
    (if (null starts)
        ""
      (hermes-chat--card-clock
       (- (if (and settled ends) (apply #'max ends) now) (apply #'min starts))))))

(defun hermes-chat--card-tree-lines (rows now)
  "Return tree lines for worker ROWS, in tree order, at time NOW."
  (let ((ids (mapcar (lambda (row) (plist-get row :id)) rows)))
    (named-let walk ((parent nil) (prefix "  "))
      (let* ((kids (seq-filter
                    (lambda (row)
                      (let ((up (plist-get row :parent)))
                        (if parent (equal up parent)
                          (not (member up ids)))))
                    rows))
             (last (car (last kids))))
        (mapconcat
         (lambda (row)
           (let* ((state (hermes-chat--card-state row))
                  (lastp (eq row last))
                  (rail (concat prefix (hermes-chat--card-dim (if lastp "  " "│ "))))
                  (goal (hermes-chat--card-fit (plist-get row :goal)
                                               hermes-chat--card-goal-width))
                  (activity (hermes-chat--card-activity row state))
                  (help (string-join
                         (delq nil (list (plist-get row :goal)
                                         (let ((model (plist-get row :model)))
                                           (and model (not (equal model "—")) model))
                                         (plist-get row :id)))
                         "\n")))
             (concat
              (propertize
               (concat prefix (hermes-chat--card-dim (if lastp "└ " "├ "))
                       (hermes-chat--card-glyph state) " "
                       (if (eq state 'queued)
                           (propertize goal 'face 'hermes-chat-card-queued)
                         goal)
                       (hermes-chat--card-right
                        (hermes-chat--card-row-time row state now))
                       "\n")
               'help-echo help)
              (when activity
                (concat rail "  "
                        (propertize (concat "↳ " (hermes-chat--card-fit
                                                  activity
                                                  hermes-chat--card-activity-width))
                                    'face (if (eq state 'failed)
                                              'hermes-chat-card-failed
                                            'shadow))
                        "\n"))
              (walk (plist-get row :id)
                    (concat prefix (hermes-chat--card-dim (if lastp "  " "│ ")) "  ")))))
         kids "")))))

(defun hermes-chat--card-tool-label (entry)
  "Return ENTRY's tool label: emoji, name and command or goal."
  (let* ((event (plist-get (plist-get entry :metadata) :event))
         (name (or (hermes-chat--tool-name event) "tool")))
    (hermes-chat--card-fit (hermes-chat--tool-head name (hermes-chat--tool-detail event name))
                           hermes-chat--card-activity-width)))

(defun hermes-chat--card-fold-button (text entry)
  "Return TEXT as the fold button of ENTRY's card, keeping TEXT's faces.
Every character of TEXT has its own face, so the button face never shows."
  (make-text-button (copy-sequence text) nil
                    'follow-link t 'mouse-face 'highlight
                    'action #'hermes-chat--toggle-entry-button
                    'hermes-chat-entry-id (plist-get entry :id)
                    'help-echo (if (hermes-chat--entry-expanded-p entry)
                                   "Fold the workers"
                                 "Show each worker's result")))

(defun hermes-chat--card-delegate-text (entry card)
  "Return the lines of delegation ENTRY's worker CARD."
  (let* ((rows (plist-get card :rows))
         (now (plist-get card :now))
         (states (mapcar #'hermes-chat--card-state rows))
         (settled (hermes-chat--card-settled-p (plist-get entry :status) states))
         (expanded (or (not settled) (hermes-chat--entry-expanded-p entry)))
         (tasks (max (or (plist-get card :tasks) 0)
                     (seq-count (lambda (row) (not (plist-get row :parent))) rows)
                     1))
         (label (concat (hermes-chat--tool-emoji "delegate_task") " delegate_task"))
         (summary (concat (hermes-chat--card-dim
                           (format " · %s" (hermes-chat--card-plural tasks "task")))
                          (mapconcat (lambda (part) (concat (hermes-chat--card-dim " · ") part))
                                     (hermes-chat--card-counts states) "")))
         (head (concat "  "
                       (hermes-chat--card-glyph
                        (if settled (hermes-chat--card-verdict-state states) 'running))
                       " "
                       (if settled
                           (hermes-chat--card-fold-button
                            (concat (propertize label 'face 'shadow) summary
                                    (unless expanded (hermes-chat--card-dim " …")))
                            entry)
                         (concat label summary))
                       (hermes-chat--card-right
                        (hermes-chat--card-elapsed rows settled now))
                       "\n"))
         (tree (and expanded (hermes-chat--card-tree-lines rows now))))
    (concat head
            (if (and settled tree)
                ;; A settled batch's results are tool detail, like other bodies.
                (propertize tree 'invisible (hermes-chat--entry-detail-kind entry))
              tree))))

(defun hermes-chat--card-ago (seconds)
  "Return SECONDS as a short age."
  (let ((s (max 0 (floor seconds))))
    (if (< s 60) (format "%ds ago" s)
      (format "%s ago" (hermes-chat--card-clock s)))))

(defun hermes-chat--card-verdict (row now)
  "Return the exit verdict for ended process ROW at NOW."
  (let ((code (plist-get row :exit-code))
        (exited (plist-get row :exited-at)))
    (concat (pcase (plist-get row :state)
              ((or 'killed 'lost) (symbol-name (plist-get row :state)))
              (_ (if (integerp code) (format "exit %d" code) "exited")))
            (if (hermes-chat--work-recent-p exited now)
                (concat " · " (hermes-chat--card-ago (- now exited)))
              ""))))

(defun hermes-chat--card-process-text (entry card)
  "Return the lines of background terminal ENTRY's process CARD."
  (let* ((row (plist-get card :row))
         (now (plist-get card :now))
         (state (hermes-chat--card-state row))
         (running (eq state 'running))
         (started (plist-get card :started))
         (line (plist-get row :last-line)))
    (concat
     (propertize
      (concat "  " (hermes-chat--card-glyph state) " "
              (if running
                  (hermes-chat--card-tool-label entry)
                (propertize (hermes-chat--card-tool-label entry) 'face 'shadow))
              (hermes-chat--card-dim " (background)")
              (if running
                  (hermes-chat--card-right
                   (if (numberp started) (hermes-chat--card-clock (- now started)) ""))
                (hermes-chat--card-right (hermes-chat--card-verdict row now)
                                         (hermes-chat--card-face state)))
              "\n")
      'help-echo (string-join (delq nil (list (plist-get row :command)
                                              (plist-get row :cwd)
                                              (plist-get row :id)))
                              "\n"))
     (when (and running line)
       (concat "    " (hermes-chat--card-dim
                       (concat "↳ " (hermes-chat--card-fit
                                     line hermes-chat--card-activity-width)))
               "\n")))))

(defun hermes-chat--card-text (entry)
  "Return the propertized lines for ENTRY's work card.  Pure.
The text depends only on ENTRY: its card, status and fold state."
  (let ((card (plist-get entry :card)))
    (if (eq (plist-get card :kind) 'process)
        (hermes-chat--card-process-text entry card)
      (hermes-chat--card-delegate-text entry card))))

;;;; Pure derivation

(defun hermes-chat--card-delegate-rows (agents roots old now settled)
  "Return a delegation card's rows from roster AGENTS at time NOW.
A worker belongs to the card when its id is in ROOTS or in OLD, the card's
previous rows, or when its parent belongs.  OLD rows the roster no longer
lists are kept; once the spawning row SETTLED, any of them still live is
shown as unknown.  Rows are in tree order.  Pure."
  (let ((known (append roots (mapcar (lambda (row) (plist-get row :id)) old)))
        fresh)
    (dolist (row agents)
      (let ((id (plist-get row :id)))
        (when (or (member id known) (member (plist-get row :parent) known))
          (push row fresh)
          (push id known))))
    (setq fresh (nreverse fresh))
    (hermes-chat--work-tree
     (append
      (mapcar (lambda (row)
                (or (hermes-chat--work-row-id (plist-get row :id) fresh)
                    (if (and settled (hermes-chat--work-active-p row))
                        (append (list :state 'unknown :finished now) row)
                      row)))
              old)
      (seq-remove (lambda (row) (hermes-chat--work-row-id (plist-get row :id) old))
                  fresh)))))

(defun hermes-chat--card-task-count (event)
  "Return the number of tasks delegate tool EVENT's arguments request."
  (let* ((args (plist-get event :args))
         (tasks (and (or (consp args) (hash-table-p args))
                     (hermes-transport--get args 'tasks))))
    (cond ((and (sequencep tasks) (not (stringp tasks)) (> (length tasks) 0))
           (length tasks))
          ((and args (hermes-transport--get args 'goal)) 1)
          (t 0))))

(defun hermes-chat--card-kind (event)
  "Return the card kind tool EVENT can own, `delegate' or `process', or nil."
  (let ((name (hermes-chat--tool-name event))
        (args (plist-get event :args)))
    (cond ((equal name "delegate_task") 'delegate)
          ((and (equal name "terminal")
                (or (consp args) (hash-table-p args))
                (eq (hermes-transport--get args 'background) t))
           'process))))

(defun hermes-chat--card-process-key (event)
  "Return (SESSION-ID . COMMAND) identifying background tool EVENT's process."
  (let* ((result (plist-get event :result))
         (session (cond ((or (consp result) (hash-table-p result))
                         (hermes-transport--get result 'session_id))
                        ((and (stringp result)
                              (string-match "\"session_id\"[ \t]*:[ \t]*\"\\([^\"]+\\)\""
                                            result))
                         (match-string 1 result))))
         (args (plist-get event :args)))
    (cons (and (stringp session) session)
          (when-let* ((command (and (or (consp args) (hash-table-p args))
                                    (hermes-transport--get args 'command))))
            (and (stringp command) (string-trim command))))))

(defun hermes-chat--card-match-process (event processes taken recent)
  "Return the row in PROCESSES that background tool EVENT started, or nil.
Match the result's session id; when the result has none yet and the row is
RECENT, match the command.  Rows whose id is in TAKEN belong to other
cards."
  (pcase-let ((`(,session . ,command) (hermes-chat--card-process-key event)))
    (seq-find (lambda (row)
                (and (not (member (plist-get row :id) taken))
                     (if session
                         (equal (plist-get row :id) session)
                       (and recent command
                            (equal (string-trim (or (plist-get row :command) ""))
                                   command)))))
              processes)))

(defun hermes-chat--card-new-roots (agents taken)
  "Return ids of live root workers in AGENTS that no card has TAKEN.  Pure."
  (let ((ids (mapcar (lambda (row) (plist-get row :id)) agents)))
    (delq nil
          (mapcar (lambda (row)
                    (let ((id (plist-get row :id))
                          (up (plist-get row :parent)))
                      (and (hermes-chat--work-active-p row)
                           (not (member id taken))
                           (not (and up (or (member up ids) (member up taken))))
                           id)))
                  agents))))

;;;; Buffer state and repaint

(defvar-local hermes-chat--card-nodes nil
  "EWOC nodes of tool rows that may own live work, oldest first.")

(defvar-local hermes-chat--card-timer nil
  "Timer advancing elapsed times while a card is live.")

(defconst hermes-chat--card-claim-window 10
  "Seconds after a spawning row settles during which it still claims work.")

(defun hermes-chat--card-node-live-p (node)
  "Return non-nil when NODE is still a transcript entry of this chat."
  (and hermes-chat--nodes
       (eq (gethash (plist-get (ewoc-data node) :id) hermes-chat--nodes) node)))

(defun hermes-chat--card-taken (nodes)
  "Return every worker and process id the cards of NODES own."
  (mapcan (lambda (node)
            (let ((card (plist-get (ewoc-data node) :card)))
              (append (copy-sequence (plist-get card :roots))
                      (mapcar (lambda (row) (plist-get row :id)) (plist-get card :rows))
                      (and (plist-get card :id) (list (plist-get card :id))))))
          nodes))

(defun hermes-chat--card-claimable-p (entry now)
  "Return non-nil when spawning row ENTRY may claim new work at NOW."
  (or (hermes-chat--active-status-p (plist-get entry :status))
      (when-let* ((updated (or (plist-get entry :updated) (plist-get entry :created))))
        (<= (- now (float-time updated)) hermes-chat--card-claim-window))))

(defun hermes-chat--card-next (entry roster taken claimer now)
  "Return ENTRY's next card from ROSTER at NOW, or nil when it owns no work.
TAKEN lists ids other cards own; CLAIMER non-nil lets ENTRY claim new
root workers."
  (let* ((event (plist-get (plist-get entry :metadata) :event))
         (card (plist-get entry :card)))
    (pcase (hermes-chat--card-kind event)
      ('delegate
       (let* ((agents (plist-get roster :agents))
              (roots (append (plist-get card :roots)
                             (and claimer (hermes-chat--card-new-roots agents taken))))
              (settled (not (hermes-chat--active-status-p (plist-get entry :status))))
              (rows (hermes-chat--card-delegate-rows
                     agents roots (plist-get card :rows) now settled)))
         (when rows
           (list :kind 'delegate :roots roots :rows rows
                 :tasks (hermes-chat--card-task-count event) :now now))))
      ('process
       (let* ((id (plist-get card :id))
              (row (if id
                       (or (hermes-chat--work-row-id id (plist-get roster :processes))
                           (plist-get card :row))
                     (hermes-chat--card-match-process
                      event (plist-get roster :processes) taken
                      (hermes-chat--card-claimable-p entry now))))
              (uptime (plist-get row :uptime)))
         (when row
           (list :kind 'process :id (plist-get row :id) :row row :now now
                 :started (or (plist-get card :started)
                              (and (numberp uptime) (- now uptime))))))))))

(defun hermes-chat--card-ticking-p (entry)
  "Return non-nil when the time on ENTRY's card still advances."
  (let ((card (plist-get entry :card)))
    (pcase (plist-get card :kind)
      ('process
       (let ((row (plist-get card :row)))
         (or (eq (plist-get row :state) 'running)
             (hermes-chat--work-recent-p (plist-get row :exited-at)
                                         (- (plist-get card :now) 1)))))
      ('delegate
       (not (hermes-chat--card-settled-p
             (plist-get entry :status)
             (mapcar #'hermes-chat--card-state (plist-get card :rows))))))))

(defun hermes-chat--card-repaint (node entry)
  "Store ENTRY in NODE and reprint that node alone.
Preserve the composer draft, its undo, point and every window's start."
  (hermes-chat--preserve-input-point
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (hermes-chat--preserve-readers
        (ewoc-set-data node entry)
        (ewoc-invalidate hermes-chat--ewoc node)))))

(defun hermes-chat--card-store (node entry card)
  "Give NODE's ENTRY its next CARD; reprint only when its text differs."
  (let ((next (hermes-chat--entry-with entry :card card)))
    (if (and (plist-get entry :card)
             (equal (hermes-chat--card-text entry) (hermes-chat--card-text next)))
        (ewoc-set-data node next)
      (hermes-chat--card-repaint node next))))

(defun hermes-chat--card-roster (now)
  "Return this chat's work roster at NOW, or nil without an attached owner."
  (when-let* ((owner hermes-chat--work-owner)
              ((funcall (plist-get owner :current-p) owner)))
    (hermes-chat--work-roster owner now)))

(defun hermes-chat--cards-refresh (&optional now)
  "Bring every live card of this chat up to date at NOW.
Only the newest spawning row may claim new workers."
  (let ((now (or now (float-time))))
    (setq hermes-chat--card-nodes
          (seq-filter #'hermes-chat--card-node-live-p hermes-chat--card-nodes))
    (when hermes-chat--card-nodes
      (let* ((roster (hermes-chat--card-roster now))
             (claimer (seq-find (lambda (node)
                                  (let ((entry (ewoc-data node)))
                                    (and (eq (hermes-chat--card-kind
                                              (plist-get (plist-get entry :metadata) :event))
                                             'delegate)
                                         (hermes-chat--card-claimable-p entry now))))
                                (reverse hermes-chat--card-nodes))))
        (dolist (node hermes-chat--card-nodes)
          (let* ((entry (ewoc-data node))
                 (card (hermes-chat--card-next
                        entry roster
                        (hermes-chat--card-taken (remq node hermes-chat--card-nodes))
                        (eq node claimer) now)))
            (when card
              (hermes-chat--card-store node entry card))))))
    (hermes-chat--cards-arm)
    (hermes-chat--card-indicator-sync now)))

(defun hermes-chat--cards-note-tool (assistant-id event)
  "Track ASSISTANT-ID's tool row for EVENT when it can own live work."
  (when-let* (((eq (plist-get event :type) 'tool))
              ((hermes-chat--card-kind event))
              (key (hermes-chat--transport-entry-id event))
              (node (and hermes-chat--nodes
                         (gethash (format "%s:%s" assistant-id key) hermes-chat--nodes))))
    (unless (memq node hermes-chat--card-nodes)
      (setq hermes-chat--card-nodes (append hermes-chat--card-nodes (list node))))
    (hermes-chat--cards-refresh)))

(defun hermes-chat--cards-tick (buffer)
  "Advance BUFFER's live cards while a window displays it."
  (if (not (buffer-live-p buffer))
      nil
    (with-current-buffer buffer
      (when (get-buffer-window buffer t)
        (hermes-chat--cards-refresh)))))

(defun hermes-chat--cards-cancel ()
  "Cancel this chat's card timer."
  (when (timerp hermes-chat--card-timer)
    (cancel-timer hermes-chat--card-timer))
  (setq hermes-chat--card-timer nil))

(defun hermes-chat--cards-arm ()
  "Run the one-second card timer exactly while a card's time advances."
  (if (seq-some (lambda (node) (hermes-chat--card-ticking-p (ewoc-data node)))
                hermes-chat--card-nodes)
      (unless hermes-chat--card-timer
        (add-hook 'kill-buffer-hook #'hermes-chat--cards-cancel nil t)
        (setq hermes-chat--card-timer
              (run-at-time 1 1 #'hermes-chat--cards-tick (current-buffer))))
    (hermes-chat--cards-cancel)))

(defun hermes-chat--cards-work-changed ()
  "Refresh this chat's cards after its observed work changed."
  (when (derived-mode-p 'hermes-chat-mode)
    (hermes-chat--cards-refresh)))

(add-hook 'hermes-chat-work-changed-hook #'hermes-chat--cards-work-changed)

;;;; Cross-chat mode-line segment

(defvar hermes-chat--card-indicator-buffers nil
  "Chat buffers with live or unseen failed work, oldest first.")

(defvar-local hermes-chat--card-indicator nil
  "This chat's (:live N :failed IDS :label LABEL) mode-line state.")

(defvar-local hermes-chat--card-seen-failures nil
  "Ids of failed work this chat already showed while displayed.")

(defvar hermes-chat-work-indicator-mode-line
  '(:eval (hermes-chat--work-indicator-string))
  "Mode-line construct listing other chats with live or failed work.")
(put 'hermes-chat-work-indicator-mode-line 'risky-local-variable t)

(defun hermes-chat--set-work-indicator (symbol value)
  "Set SYMBOL to VALUE and add or remove the global work segment."
  (set-default symbol value)
  (if value
      (hermes-chat--prompt-indicator-install 'hermes-chat-work-indicator-mode-line)
    (hermes-chat--prompt-indicator-uninstall 'hermes-chat-work-indicator-mode-line))
  (force-mode-line-update t))

(defcustom hermes-chat-work-indicator t
  "Whether other chats' live work is shown in the global mode line.
When non-nil, `global-mode-string' lists each chat that no window displays
and that has running work or failures you have not seen, such as
\"[Hermes 🤖 claude-code 2 · gnosis ✗1]\".  Clicking a chat switches to it.
The segment is absent when there is nothing to show."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set #'hermes-chat--set-work-indicator
  :group 'hermes)

(defun hermes-chat--card-displayed-p (buffer)
  "Return non-nil when a window on a visible frame displays BUFFER."
  (seq-some (lambda (window) (eq (frame-visible-p (window-frame window)) t))
            (get-buffer-window-list buffer nil t)))

(defun hermes-chat--card-failed-ids (roster)
  "Return the ids of failed workers and processes in ROSTER."
  (delq nil (mapcar (lambda (row)
                      (and (eq (hermes-chat--card-state row) 'failed)
                           (plist-get row :id)))
                    (append (plist-get roster :agents) (plist-get roster :processes)))))

(defun hermes-chat--card-indicator-forget ()
  "Remove the current buffer and dead buffers from the work segment."
  (setq hermes-chat--card-indicator-buffers
        (seq-filter (lambda (buffer)
                      (and (buffer-live-p buffer) (not (eq buffer (current-buffer)))))
                    hermes-chat--card-indicator-buffers))
  (force-mode-line-update t))

(defun hermes-chat--card-indicator-sync (&optional now)
  "Update this chat's entry in the cross-chat work segment at NOW.
Failures count until a window has displayed the chat."
  (let* ((roster (hermes-chat--card-roster (or now (float-time))))
         (counts (and roster (hermes-chat--work-counts roster)))
         (live (if counts (+ (car counts) (cdr counts)) 0))
         (failed (and roster (hermes-chat--card-failed-ids roster)))
         (shown (hermes-chat--card-displayed-p (current-buffer)))
         (unseen (unless shown
                   (seq-uniq (append (plist-get hermes-chat--card-indicator :failed)
                                     (seq-difference failed
                                                     hermes-chat--card-seen-failures)))))
         (state (and (or (> live 0) unseen)
                     (list :live live :failed unseen
                           :label (or (plist-get hermes-chat--card-indicator :label)
                                      (ignore-errors (hermes-chat--switch-project
                                                      (current-buffer)))
                                      (buffer-name))))))
    (setq hermes-chat--card-seen-failures
          (if shown failed (seq-intersection hermes-chat--card-seen-failures failed)))
    (unless (equal state hermes-chat--card-indicator)
      (setq hermes-chat--card-indicator state)
      (if (not state)
          (hermes-chat--card-indicator-forget)
        (unless (memq (current-buffer) hermes-chat--card-indicator-buffers)
          (setq hermes-chat--card-indicator-buffers
                (append hermes-chat--card-indicator-buffers (list (current-buffer))))
          (add-hook 'kill-buffer-hook #'hermes-chat--card-indicator-forget nil t)
          (add-hook 'change-major-mode-hook #'hermes-chat--card-indicator-forget nil t)
          (add-hook 'window-buffer-change-functions
                    #'hermes-chat--card-indicator-shown nil t)
          (when hermes-chat-work-indicator
            (hermes-chat--prompt-indicator-install
             'hermes-chat-work-indicator-mode-line)))
        (force-mode-line-update t)))))

(defun hermes-chat--card-indicator-shown (window)
  "Acknowledge failures once WINDOW displays this chat."
  (with-current-buffer (window-buffer window)
    (hermes-chat--card-indicator-sync)))

(defun hermes-chat--card-indicator-chats ()
  "Return chats with live or unseen failed work that no window displays."
  (seq-filter (lambda (buffer)
                (and (buffer-live-p buffer)
                     (buffer-local-value 'hermes-chat--card-indicator buffer)
                     (not (hermes-chat--card-displayed-p buffer))))
              hermes-chat--card-indicator-buffers))

(defun hermes-chat--card-visit-command (buffer)
  "Return a mode-line command switching to chat BUFFER."
  (lambda (event)
    (interactive "e")
    (let ((window (posn-window (event-start event))))
      (when (window-live-p window) (select-window window)))
    (hermes-switch-to-chat buffer)))

(defun hermes-chat-work-indicator-switch ()
  "Switch to a chat listed in the global work segment."
  (interactive)
  (let ((chats (hermes-chat--card-indicator-chats)))
    (unless chats (user-error "No other Hermes chat has live work"))
    (hermes-switch-to-chat
     (if (cdr chats)
         (get-buffer (completing-read "Chat with work: " (mapcar #'buffer-name chats)
                                      nil t))
       (car chats)))))

(defun hermes-chat--card-indicator-item (buffer)
  "Return BUFFER's clickable label, live count and unseen failures."
  (let* ((state (buffer-local-value 'hermes-chat--card-indicator buffer))
         (live (plist-get state :live))
         (failed (length (plist-get state :failed)))
         (map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] (hermes-chat--card-visit-command buffer))
    (propertize
     (concat (plist-get state :label)
             (if (> live 0) (propertize (format " %d" live) 'face 'hermes-work-running) "")
             (if (> failed 0) (propertize (format " ✗%d" failed) 'face 'error) ""))
     'mouse-face 'mode-line-highlight
     'local-map map
     'help-echo (format "mouse-1: switch to %s" (buffer-name buffer)))))

(defun hermes-chat--work-indicator-string ()
  "Return the cross-chat work segment, or nil when no hidden chat has work."
  (when-let* ((hermes-chat-work-indicator)
              (chats (hermes-chat--card-indicator-chats)))
    (concat
     " [Hermes "
     (if (char-displayable-p ?🤖) "🤖 " "Work: ")
     (if (cdr (cdr (cdr chats)))
         (let ((failed (apply #'+ (mapcar (lambda (buffer)
                                            (length (plist-get (buffer-local-value
                                                                'hermes-chat--card-indicator
                                                                buffer)
                                                               :failed)))
                                          chats)))
               (map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] #'hermes-chat-work-indicator-switch)
           (propertize (concat (format "%d chats" (length chats))
                               (if (> failed 0)
                                   (propertize (format " · ✗%d" failed) 'face 'error)
                                 ""))
                       'mouse-face 'mode-line-highlight 'local-map map
                       'help-echo "mouse-1: switch to a chat with work"))
       (mapconcat #'hermes-chat--card-indicator-item chats " · "))
     "]")))

(provide 'hermes-chat-cards)
;;; hermes-chat-cards.el ends here
