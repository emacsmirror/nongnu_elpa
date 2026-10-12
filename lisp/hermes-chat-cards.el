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
(declare-function hermes-chat--work-visibility "hermes-chat-dashboard"
                  (&optional departing))
(declare-function hermes-chat--prompt-indicator-install "hermes-chat-prompts"
                  (&optional segment))
(declare-function hermes-chat--prompt-indicator-uninstall "hermes-chat-prompts"
                  (&optional segment))
(defvar hermes-chat-work-changed-hook)

;;;; Pure presentation

(defconst hermes-chat--card-default-width 78
  "Columns a card uses before any window has displayed it.")

(defconst hermes-chat--card-time-min-width 40
  "Narrower cards drop their right-aligned times.")

(defun hermes-chat--card-dim (text)
  "Return TEXT in `shadow'."
  (propertize text 'face 'shadow))

(defun hermes-chat--card-fit (text width)
  "Return TEXT on one line, truncated to WIDTH columns with an ellipsis."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[\t\n\r]+" " " (or text "")))
   (max 1 width) nil nil "…"))

(defun hermes-chat--card-right (text &optional face)
  "Return TEXT in FACE, right-aligned to the window edge."
  (if (string-empty-p text)
      ""
    (concat (propertize " " 'display
                        `(space :align-to (- right ,(1+ (string-width text)))))
            (propertize text 'face (or face 'shadow)))))

(defun hermes-chat--card-line (left right width &optional face)
  "Return LEFT with RIGHT right-aligned in FACE, fitting WIDTH columns.
LEFT is truncated to leave room for RIGHT.  Below
`hermes-chat--card-time-min-width' columns RIGHT is dropped."
  (let ((right (if (< width hermes-chat--card-time-min-width) "" right)))
    (concat (truncate-string-to-width
             left (max 1 (- width (if (string-empty-p right) 0
                                    (+ (string-width right) 2))))
             nil nil "…")
            (hermes-chat--card-right right face)
            "\n")))

(defun hermes-chat--card-plural (count noun)
  "Return COUNT followed by NOUN, pluralized."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun hermes-chat--card-row-time (row state now)
  "Return the right-hand time for worker ROW in display STATE at NOW."
  (let ((started (plist-get row :started))
        (finished (plist-get row :finished)))
    (cond ((eq state 'queued) "queued")
          ((not (numberp started)) "")
          ((eq state 'running) (hermes-work-duration (- now started)))
          ((numberp finished) (hermes-work-duration (- finished started)))
          (t ""))))

(defun hermes-chat--card-tool-text (tool target)
  "Return TOOL with its emoji and optional TARGET."
  (concat (hermes-chat--tool-emoji tool) " " tool
          (if target (concat " " target) "")))

(defun hermes-chat--card-arrow (text face)
  "Return result line TEXT in FACE behind a quiet `↳'."
  (concat (hermes-chat--card-dim "↳ ") (propertize text 'face face)))

(defun hermes-chat--card-first-line (text)
  "Return the first nonblank line of TEXT as plain text, or nil."
  (seq-find (lambda (line) (not (string-empty-p line)))
            (mapcar #'hermes-work-plain-line (split-string (or text "") "\n" t))))

(defun hermes-chat--card-activity (row state)
  "Return worker ROW's activity line text in display STATE, or nil.
A running worker shows its last tool and tool count; a failed one the first
line of its error, else its summary; another ended one its summary."
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
           ('failed
            (or (hermes-chat--card-first-line (plist-get row :error))
                (hermes-chat--card-first-line (plist-get row :summary))))
           ((or 'done 'interrupted)
            (or (hermes-chat--card-first-line
                 (or (plist-get row :summary) (plist-get row :error)))
                (and (eq state 'interrupted) "interrupted"))))))
    (and text (not (string-empty-p text)) text)))

(defun hermes-chat--card-counts (states)
  "Return the summary count parts for display STATES, zeros omitted."
  (delq nil
        (mapcar (lambda (spec)
                  (let* ((state (car spec))
                         (count (seq-count (lambda (s) (eq s state)) states)))
                    (when (> count 0)
                      (propertize (format "%d %s" count (hermes-work-state-label state))
                                  'face (if (memq state '(failed interrupted))
                                            (hermes-work-state-face state)
                                          'shadow)))))
                hermes-work-states)))

(defun hermes-chat--card-live-p (states)
  "Return non-nil when any of worker STATES is still running or queued."
  (seq-some (lambda (state) (memq state '(running queued))) states))

(defun hermes-chat--card-settled-p (status states)
  "Return non-nil when a delegation row in STATUS with worker STATES settled."
  (and (not (hermes-chat--active-status-p status))
       (not (hermes-chat--card-live-p states))))

(defun hermes-chat--card-verdict-state (states)
  "Return the settled glyph state for a batch whose workers ended in STATES."
  (cond ((memq 'failed states) 'failed)
        ((memq 'interrupted states) 'interrupted)
        ((memq 'unknown states) 'unknown)
        (t 'done)))

(defun hermes-chat--card-elapsed (rows)
  "Return the settled batch's elapsed text for worker ROWS, or the empty string."
  (let ((starts (seq-filter #'numberp (mapcar (lambda (row) (plist-get row :started)) rows)))
        (ends (seq-filter #'numberp (mapcar (lambda (row) (plist-get row :finished)) rows))))
    (if (and starts ends)
        (hermes-work-duration (- (apply #'max ends) (apply #'min starts)))
      "")))

(defun hermes-chat--card-root-p (row rows)
  "Return non-nil when worker ROW has no parent among ROWS."
  (let ((up (plist-get row :parent)))
    (not (and up (hermes-chat--work-row-id up rows)))))

(defun hermes-chat--card-tree-lines (rows now width)
  "Return tree lines for worker ROWS, in tree order, at NOW in WIDTH columns."
  (named-let walk ((parent nil) (prefix "  "))
    (let* ((kids (seq-filter
                  (lambda (row)
                    (if parent (equal (plist-get row :parent) parent)
                      (hermes-chat--card-root-p row rows)))
                  rows))
           (last (car (last kids))))
      (mapconcat
       (lambda (row)
         (let* ((state (hermes-work-display-state row))
                (lastp (eq row last))
                (inner (concat prefix (hermes-chat--card-dim (if lastp "  " "│ "))))
                (lead (concat inner "  "))
                (activity (hermes-chat--card-activity row state))
                (goal (or (plist-get row :goal) "")))
           (concat
            (propertize
             (hermes-chat--card-line
              (concat prefix (hermes-chat--card-dim (if lastp "└ " "├ "))
                      (hermes-work-state-glyph state) " "
                      (if (eq state 'queued)
                          (propertize goal 'face 'hermes-work-queued)
                        goal))
              (hermes-chat--card-row-time row state now) width)
             'help-echo (string-join
                         (delq nil (list (plist-get row :goal)
                                         (let ((model (plist-get row :model)))
                                           (and model (not (equal model "—")) model))
                                         (plist-get row :id)))
                         "\n"))
            (when activity
              (concat lead
                      (hermes-chat--card-arrow
                       (hermes-chat--card-fit activity (- width (string-width lead) 2))
                       (if (eq state 'failed) 'hermes-work-failed 'shadow))
                      "\n"))
            (walk (plist-get row :id) (concat inner "  ")))))
       kids ""))))

(defun hermes-chat--card-tool-label (entry width)
  "Return ENTRY's tool label in WIDTH columns: emoji, name, command or goal."
  (let* ((event (plist-get (plist-get entry :metadata) :event))
         (name (or (hermes-chat--tool-name event) "tool")))
    (hermes-chat--card-fit (hermes-chat--tool-head name (hermes-chat--tool-detail event name))
                           width)))

(defun hermes-chat--card-fold-button (text entry)
  "Return a `▸'/`▾' fold toggle labeled TEXT for ENTRY's settled card.
Every character of TEXT keeps its own face."
  (let ((expanded (hermes-chat--entry-expanded-p entry)))
    (make-text-button (concat (hermes-chat--card-dim (if expanded "▾ " "▸ ")) text) nil
                      'follow-link t 'mouse-face 'highlight
                      'action #'hermes-chat--toggle-entry-button
                      'hermes-chat-entry-id (plist-get entry :id)
                      'help-echo (if expanded
                                     "Fold the workers"
                                   "Show each worker's result"))))

(defun hermes-chat--card-summary (tasks counts nested width)
  "Return the \" · \"-joined head summary of TASKS, COUNTS and NESTED.
Fit WIDTH columns by dropping TASKS, which the counts already add up to,
then NESTED, before truncating, so the state counts are the last to go."
  (let ((join (lambda (parts)
                (mapconcat (lambda (part) (concat (hermes-chat--card-dim " · ") part))
                           (delq nil parts) "")))
        (nested (and nested (hermes-chat--card-dim nested))))
    (seq-find (lambda (text) (<= (string-width text) width))
              (list (funcall join (append (list (hermes-chat--card-dim tasks)) counts
                                          (list nested)))
                    (funcall join (append counts (list nested)))
                    (funcall join counts))
              (funcall join counts))))

(defun hermes-chat--card-delegate-text (entry card)
  "Return the lines of delegation ENTRY's worker CARD.
Counts cover the root workers, so they add up to the task count; tasks no
worker has picked up yet count as queued.  Nested workers only add a quiet
total.  While live only the rows' times tick; a settled head shows the
batch's elapsed time and folds like other tool rows."
  (let* ((rows (plist-get card :rows))
         (now (plist-get card :now))
         (width (or (plist-get card :width) hermes-chat--card-default-width))
         (roots (seq-filter (lambda (row) (hermes-chat--card-root-p row rows)) rows))
         (states (mapcar #'hermes-work-display-state rows))
         (root-states (mapcar #'hermes-work-display-state roots))
         (settled (hermes-chat--card-settled-p (plist-get entry :status) states))
         (expanded (or (not settled) (hermes-chat--entry-expanded-p entry)))
         (tasks (max (or (plist-get card :tasks) 0) (length roots) 1))
         (pending (if settled 0 (- tasks (length roots))))
         (nested (- (length rows) (length roots)))
         (label (concat (hermes-chat--tool-emoji "delegate_task") " delegate_task"))
         (elapsed (if (and settled (>= width hermes-chat--card-time-min-width))
                      (hermes-chat--card-elapsed rows)
                    ""))
         (summary (hermes-chat--card-summary
                   (hermes-chat--card-plural tasks "task")
                   (hermes-chat--card-counts (append root-states (make-list pending 'queued)))
                   (and (> nested 0) (format "+%d nested" nested))
                   (- width (string-width label) 4 (if settled 2 0)
                      (if (string-empty-p elapsed) 0 (+ (string-width elapsed) 2)))))
         (head (concat "  "
                       (hermes-work-state-glyph
                        (if settled (hermes-chat--card-verdict-state root-states) 'running))
                       " "
                       (if settled
                           (hermes-chat--card-fold-button
                            (concat (propertize label 'face 'shadow) summary) entry)
                         (concat label summary))))
         (tree (and expanded rows (hermes-chat--card-tree-lines rows now width))))
    (concat (hermes-chat--card-line head elapsed width)
            (if (and settled tree)
                ;; A settled batch's results are tool detail, like other bodies.
                (propertize tree 'invisible (hermes-chat--entry-detail-kind entry))
              tree))))

(defun hermes-chat--card-verdict (row now)
  "Return the exit verdict for ended process ROW at NOW."
  (let ((code (plist-get row :exit-code))
        (exited (plist-get row :exited-at)))
    (concat (pcase (plist-get row :state)
              ((or 'killed 'lost) (symbol-name (plist-get row :state)))
              (_ (if (integerp code) (format "exit %d" code) "exited")))
            (if (hermes-chat--work-recent-p exited now)
                (concat " · " (hermes-work-ago (- now exited)))
              ""))))

(defun hermes-chat--card-process-text (entry card)
  "Return the lines of background terminal ENTRY's process CARD.
A running or failed process keeps its last output line below the head."
  (let* ((row (plist-get card :row))
         (now (plist-get card :now))
         (width (or (plist-get card :width) hermes-chat--card-default-width))
         (state (hermes-work-display-state row))
         (running (eq state 'running))
         (started (plist-get card :started))
         (right (if running
                    (if (numberp started) (hermes-work-duration (- now started)) "")
                  (hermes-chat--card-verdict row now)))
         (room (- width 4 (if (string-empty-p right) 0 (+ (string-width right) 2))))
         (label (hermes-chat--card-tool-label entry room))
         (marker " (background)")
         ;; The command matters more than the marker: drop it rather than cut.
         (suffix (if (<= (+ (string-width label) (string-width marker)) room) marker ""))
         (line (plist-get row :last-line)))
    (concat
     (propertize
      (hermes-chat--card-line
       (concat "  " (hermes-work-state-glyph state t) " "
               (if (memq state '(running failed)) label (propertize label 'face 'shadow))
               (hermes-chat--card-dim suffix))
       right width (if running 'shadow (hermes-work-state-face state)))
      'help-echo (string-join (delq nil (list (plist-get row :command)
                                              (plist-get row :cwd)
                                              (plist-get row :id)))
                              "\n"))
     (when (and line (memq state '(running failed)))
       (concat "    "
               (hermes-chat--card-arrow (hermes-chat--card-fit line (- width 6))
                                        (if running 'shadow 'hermes-work-failed))
               "\n")))))

(defun hermes-chat--card-text (entry)
  "Return the propertized lines for ENTRY's work card.  Pure.
The text depends only on ENTRY: its card, status and fold state."
  (let ((card (plist-get entry :card)))
    (if (eq (plist-get card :kind) 'process)
        (hermes-chat--card-process-text entry card)
      (hermes-chat--card-delegate-text entry card))))

;;;; Pure derivation

(defun hermes-chat--card-args (event)
  "Return tool EVENT's arguments when they are an object, else nil."
  (let ((args (plist-get event :args)))
    (and (or (consp args) (hash-table-p args)) args)))

(defun hermes-chat--card-result (event)
  "Return tool EVENT's result as an object, or nil.
A JSON string result is parsed; anything else that is no object is nil."
  (let ((result (plist-get event :result)))
    (cond ((or (consp result) (hash-table-p result)) result)
          ((and (stringp result) (string-prefix-p "{" (string-trim-left result)))
           (ignore-errors (json-parse-string result :object-type 'alist
                                             :array-type 'array))))))

(defun hermes-chat--card-string (value)
  "Return VALUE when it is a nonempty string, else nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun hermes-chat--card-delegation (event)
  "Return (DELEGATION-ID . SUBAGENT-IDS) from delegate tool EVENT's result.
A background dispatch names both; a finished batch names its live
transcripts, whose directory is the delegation id."
  (let* ((result (hermes-chat--card-result event))
         (get (lambda (key) (and result (hermes-transport--get result key))))
         (live (seq-find
                #'stringp
                (append (let ((paths (funcall get 'live_transcripts)))
                          (and (sequencep paths) (not (stringp paths)) paths))
                        (let ((results (funcall get 'results)))
                          (and (sequencep results) (not (stringp results))
                               (mapcar (lambda (row)
                                         (and (or (consp row) (hash-table-p row))
                                              (hermes-transport--get row 'live_transcript)))
                                       results))))))
         (ids (funcall get 'subagent_ids)))
    (cons (or (hermes-chat--card-string (funcall get 'delegation_id))
              (and live (string-match "/live/\\([^/]+\\)/task-[0-9]+\\.log\\'" live)
                   (match-string 1 live)))
          (and (sequencep ids) (not (stringp ids))
               (seq-uniq (delq nil (mapcar #'hermes-chat--card-string ids)))))))

(defun hermes-chat--card-goals (event)
  "Return the trimmed goals delegate tool EVENT's arguments request."
  (let* ((args (hermes-chat--card-args event))
         (tasks (and args (hermes-transport--get args 'tasks)))
         (goal (lambda (value)
                 (and (stringp value)
                      (hermes-chat--card-string (string-trim value))))))
    (delq nil
          (if (and (sequencep tasks) (not (stringp tasks)) (> (length tasks) 0))
              (mapcar (lambda (task)
                        (and (or (consp task) (hash-table-p task))
                             (funcall goal (hermes-transport--get task 'goal))))
                      tasks)
            (list (and args (funcall goal (hermes-transport--get args 'goal))))))))

(defun hermes-chat--card-task-count (event)
  "Return the number of tasks delegate tool EVENT's arguments request."
  (let* ((args (hermes-chat--card-args event))
         (tasks (and args (hermes-transport--get args 'tasks))))
    (cond ((and (sequencep tasks) (not (stringp tasks)) (> (length tasks) 0))
           (length tasks))
          ((and args (hermes-transport--get args 'goal)) 1)
          (t 0))))

(defun hermes-chat--card-kind (event)
  "Return the card kind tool EVENT can own, `delegate' or `process', or nil.
Only a spawning `delegate_task' call owns workers; list, steer and stop
calls stay plain tool rows."
  (let ((name (hermes-chat--tool-name event))
        (args (hermes-chat--card-args event)))
    (cond ((equal name "delegate_task")
           (and (member (or (and args (hermes-transport--get args 'action)) "spawn")
                        '("spawn" ""))
                'delegate))
          ((and (equal name "terminal") args
                (eq (hermes-transport--get args 'background) t))
           'process))))

(defun hermes-chat--card-batches (agents)
  "Return (DELEGATION-ID . GOALS) for the root workers in roster AGENTS.
Workers that no event or read attributed to a delegation are left out."
  (let (batches)
    (dolist (row agents)
      (when-let* ((id (plist-get row :delegation-id))
                  ((hermes-chat--card-root-p row agents)))
        (push (string-trim (or (plist-get row :goal) "")) (alist-get id batches nil nil #'equal))))
    (nreverse batches)))

(defun hermes-chat--card-goals-cover-p (goals batch)
  "Return non-nil when goal list GOALS includes every goal of BATCH.
Both are multisets; BATCH may be part of a batch still spawning."
  (let ((left (copy-sequence goals)))
    (and batch
         (seq-every-p (lambda (goal)
                        (when (member goal left)
                          (setq left (cl-remove goal left :count 1 :test #'equal))
                          t))
                      batch))))

(defun hermes-chat--card-bindings (claims batches)
  "Pair spawning rows with worker batches by their goals.  Pure.
CLAIMS lists (KEY . GOALS) of rows whose delegation is still unknown;
BATCHES lists (DELEGATION-ID . GOALS) no row has bound.  Return (KEY .
DELEGATION-ID) only for pairs that match each other alone: two batches
with the same goals stay unbound until their results name them."
  (let ((fits (lambda (claim batch)
                (hermes-chat--card-goals-cover-p (cdr claim) (cdr batch)))))
    (delq nil
          (mapcar (lambda (claim)
                    (let ((found (seq-filter (lambda (batch) (funcall fits claim batch))
                                             batches)))
                      (when (and found (null (cdr found))
                                 (= 1 (seq-count (lambda (other)
                                                   (funcall fits other (car found)))
                                                 claims)))
                        (cons (car claim) (car (car found))))))
                  claims))))

(defun hermes-chat--card-delegate-rows (agents delegation roots old now settled)
  "Return a delegation card's rows from roster AGENTS at time NOW.
A worker belongs to the card when it is a root worker of DELEGATION, when
its id is in ROOTS or in OLD, the card's previous rows, or when its parent
belongs.  OLD rows the roster no longer lists are kept; once the spawning
row SETTLED, any of them still live is shown as unknown.  Rows are in tree
order.  Pure."
  (let ((known (append roots (mapcar (lambda (row) (plist-get row :id)) old)))
        fresh)
    (dolist (row agents)
      (let ((id (plist-get row :id)))
        (when (or (member id known)
                  (member (plist-get row :parent) known)
                  (and delegation
                       (equal (plist-get row :delegation-id) delegation)
                       (hermes-chat--card-root-p row agents)))
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

(defun hermes-chat--card-confirmed-rows (rows delegation roots)
  "Return the ROWS a delegation result naming DELEGATION and ROOTS confirms.
A root worker stays when ROOTS lists it or it belongs to DELEGATION; a
nested worker stays with its parent.  ROWS are in tree order.  Pure."
  (let (kept)
    (dolist (row rows)
      (when (if (hermes-chat--card-root-p row rows)
                (or (member (plist-get row :id) roots)
                    (and delegation (equal (plist-get row :delegation-id) delegation)))
              (hermes-chat--work-row-id (plist-get row :parent) kept))
        (push row kept)))
    (nreverse kept)))

(defun hermes-chat--card-process-key (event)
  "Return (SESSION-ID . COMMAND) identifying background tool EVENT's process."
  (let* ((result (plist-get event :result))
         (object (hermes-chat--card-result event))
         (session (or (and object (hermes-transport--get object 'session_id))
                      (and (stringp result)
                           (string-match "\"session_id\"[ \t]*:[ \t]*\"\\([^\"]+\\)\""
                                         result)
                           (match-string 1 result))))
         (args (hermes-chat--card-args event)))
    (cons (hermes-chat--card-string session)
          (when-let* ((command (and args (hermes-transport--get args 'command))))
            (and (stringp command) (string-trim command))))))

(defun hermes-chat--card-match-process (command processes taken)
  "Return the row in PROCESSES running COMMAND that no id in TAKEN owns."
  (and command
       (seq-find (lambda (row)
                   (and (not (member (plist-get row :id) taken))
                        (equal (string-trim (or (plist-get row :command) "")) command)))
                 processes)))

(defun hermes-chat--card-process (event card processes taken recent)
  "Return the row in PROCESSES that background tool EVENT owns, or nil.
The result's session id is authoritative and rebinds a provisional match.
Before the result, CARD's earlier row is kept unless another card's result
claims it, and a RECENT row may match by command.  TAKEN lists ids other
cards own."
  (pcase-let ((`(,session . ,command) (hermes-chat--card-process-key event))
              (id (plist-get card :id)))
    (cond
     (session (or (hermes-chat--work-row-id session processes)
                  (and (equal id session) (plist-get card :row))))
     ((and id (not (member id taken)))
      (or (hermes-chat--work-row-id id processes) (plist-get card :row)))
     (recent (hermes-chat--card-match-process command processes taken)))))

(defun hermes-chat--card-next (entry roster context now)
  "Return ENTRY's next card from ROSTER at NOW, or nil when it owns no work.
CONTEXT is a plist: :taken, ids other cards own; :binding, the delegation
id goals matched to ENTRY; :claimable, whether ENTRY may match new work;
:width, the narrowest displaying window's columns."
  (let* ((event (plist-get (plist-get entry :metadata) :event))
         (card (plist-get entry :card))
         (width (plist-get context :width))
         (active (hermes-chat--active-status-p (plist-get entry :status))))
    (pcase (hermes-chat--card-kind event)
      ('delegate
       ;; A result naming the delegation or its workers is authoritative: it
       ;; replaces a delegation the goals matched and drops what that brought.
       (pcase-let* ((`(,named . ,ids) (hermes-chat--card-delegation event))
                    (result (or named ids))
                    (delegation (if result named
                                  (or (plist-get card :delegation)
                                      (plist-get context :binding))))
                    (roots (seq-uniq (append (plist-get card :roots) ids)))
                    (old (if result
                             (hermes-chat--card-confirmed-rows
                              (plist-get card :rows) delegation roots)
                           (plist-get card :rows)))
                    (rows (hermes-chat--card-delegate-rows
                           (plist-get roster :agents) delegation roots old now
                           (not active))))
         (let ((tasks (hermes-chat--card-task-count event)))
           ;; Before any worker is attributed, a live call shows its task count.
           (when (or rows (and active (> tasks 0)))
             (list :kind 'delegate :delegation delegation :roots roots :rows rows
                   :tasks tasks :now now :width width)))))
      ('process
       (let* ((row (hermes-chat--card-process
                    event card (plist-get roster :processes) (plist-get context :taken)
                    (plist-get context :claimable)))
              (id (plist-get row :id))
              (uptime (plist-get row :uptime)))
         (when row
           (list :kind 'process :id id :row row :now now :width width
                 :started (or (and (equal id (plist-get card :id)) (plist-get card :started))
                              (and (numberp uptime) (- now uptime))))))))))

;;;; Buffer state and repaint

(defvar-local hermes-chat--card-nodes nil
  "EWOC nodes of tool rows that may own live work, oldest first.")

(defvar-local hermes-chat--card-timer nil
  "Timer advancing elapsed times while a displayed card is live.")

(defconst hermes-chat--card-claim-window 10
  "Seconds after a spawning row settles during which it still claims work.")

(defun hermes-chat--card-node-live-p (node)
  "Return non-nil when NODE is still a transcript entry of this chat."
  (and hermes-chat--nodes
       (eq (gethash (plist-get (ewoc-data node) :id) hermes-chat--nodes) node)))

(defun hermes-chat--card-event (node)
  "Return the tool event of NODE's entry."
  (plist-get (plist-get (ewoc-data node) :metadata) :event))

(defun hermes-chat--card-owned (node)
  "Return the worker, process and delegation ids NODE's row owns or names."
  (let* ((card (plist-get (ewoc-data node) :card))
         (event (hermes-chat--card-event node)))
    (delq nil
          (append (copy-sequence (plist-get card :roots))
                  (mapcar (lambda (row) (plist-get row :id)) (plist-get card :rows))
                  (list (plist-get card :id) (plist-get card :delegation))
                  (pcase (hermes-chat--card-kind event)
                    ('process (list (car (hermes-chat--card-process-key event))))
                    ('delegate (let ((named (hermes-chat--card-delegation event)))
                                 (cons (car named) (cdr named)))))))))

(defun hermes-chat--card-claimable-p (entry now)
  "Return non-nil when spawning row ENTRY may claim new work at NOW."
  (or (hermes-chat--active-status-p (plist-get entry :status))
      (when-let* ((updated (or (plist-get entry :updated) (plist-get entry :created))))
        (<= (- now (float-time updated)) hermes-chat--card-claim-window))))

(defun hermes-chat--card-ticking-p (entry)
  "Return non-nil when a time on ENTRY's card still advances."
  (let ((card (plist-get entry :card)))
    (pcase (plist-get card :kind)
      ('process
       (let ((row (plist-get card :row)))
         (or (eq (plist-get row :state) 'running)
             (hermes-chat--work-recent-p (plist-get row :exited-at)
                                         (- (plist-get card :now) 1)))))
      ('delegate
       (seq-some (lambda (row) (eq (hermes-work-display-state row) 'running))
                 (plist-get card :rows))))))

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
  "Give NODE's ENTRY its next CARD; reprint only when its text differs.
A nil CARD returns the row to a plain tool row."
  (let ((next (hermes-chat--entry-with entry :card card)))
    (cond ((and (null card) (null (plist-get entry :card))))
          ((and card (plist-get entry :card)
                (equal (hermes-chat--card-text entry) (hermes-chat--card-text next)))
           (ewoc-set-data node next))
          (t (hermes-chat--card-repaint node next)))))

(defun hermes-chat--card-roster (now)
  "Return this chat's work roster at NOW, or nil without an attached owner."
  (when-let* ((owner hermes-chat--work-owner)
              ((funcall (plist-get owner :current-p) owner)))
    (hermes-chat--work-roster owner now)))

(defun hermes-chat--card-width ()
  "Return the columns cards use: the narrowest displaying window's."
  (if (get-buffer-window (current-buffer) t)
      (hermes-chat--table-window-width)
    hermes-chat--card-default-width))

(defun hermes-chat--card-goal-bindings (nodes roster now)
  "Return (NODE . DELEGATION-ID) for unbound spawning NODES matched by goals.
ROSTER supplies the batches; a batch any card already owns is excluded.
Only rows still claimable at NOW take part."
  (let* ((owned (mapcan #'hermes-chat--card-owned nodes))
         (claims (delq nil
                       (mapcar (lambda (node)
                                 (let* ((entry (ewoc-data node))
                                        (event (hermes-chat--card-event node)))
                                   (and (eq (hermes-chat--card-kind event) 'delegate)
                                        (not (plist-get (plist-get entry :card) :delegation))
                                        (equal (hermes-chat--card-delegation event) '(nil))
                                        (hermes-chat--card-claimable-p entry now)
                                        (cons node (hermes-chat--card-goals event)))))
                               nodes)))
         (batches (seq-remove (lambda (batch) (member (car batch) owned))
                              (hermes-chat--card-batches (plist-get roster :agents)))))
    (and claims batches (hermes-chat--card-bindings claims batches))))

(defun hermes-chat--cards-refresh (&optional now)
  "Bring every live card of this chat up to date at NOW."
  (let ((now (or now (float-time))))
    (setq hermes-chat--card-nodes
          (seq-filter #'hermes-chat--card-node-live-p hermes-chat--card-nodes))
    (when hermes-chat--card-nodes
      (let* ((roster (hermes-chat--card-roster now))
             (width (hermes-chat--card-width))
             (bindings (hermes-chat--card-goal-bindings hermes-chat--card-nodes roster now)))
        (dolist (node hermes-chat--card-nodes)
          (let ((entry (ewoc-data node)))
            (hermes-chat--card-store
             node entry
             (hermes-chat--card-next
              entry roster
              (list :taken (mapcan #'hermes-chat--card-owned
                                   (remq node hermes-chat--card-nodes))
                    :binding (cdr (assq node bindings))
                    :claimable (hermes-chat--card-claimable-p entry now)
                    :width width)
              now))))))
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
      (setq hermes-chat--card-nodes (append hermes-chat--card-nodes (list node)))
      (add-hook 'kill-buffer-hook #'hermes-chat--cards-cancel nil t)
      (add-hook 'change-major-mode-hook #'hermes-chat--cards-retire nil t)
      (add-hook 'window-configuration-change-hook #'hermes-chat--cards-reflow nil t))
    (hermes-chat--cards-refresh)))

(defun hermes-chat--cards-tick (buffer)
  "Advance BUFFER's live cards while a window displays it, else stop ticking."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (hermes-chat--card-displayed-p buffer)
          (hermes-chat--cards-refresh)
        (hermes-chat--cards-cancel)))))

(defun hermes-chat--cards-cancel ()
  "Cancel this chat's card timer."
  (when (timerp hermes-chat--card-timer)
    (cancel-timer hermes-chat--card-timer))
  (setq hermes-chat--card-timer nil))

(defun hermes-chat--cards-retire ()
  "Cancel this chat's card clock and forget its cards on a mode change."
  (hermes-chat--cards-cancel)
  (setq hermes-chat--card-nodes nil))

(defun hermes-chat--cards-reflow ()
  "Refit this chat's cards to its windows, resuming their clock when shown."
  (when (and (derived-mode-p 'hermes-chat-mode) hermes-chat--card-nodes)
    (if (hermes-chat--card-displayed-p (current-buffer))
        (hermes-chat--cards-refresh)
      (hermes-chat--cards-cancel))))

(defun hermes-chat--cards-arm ()
  "Run the one-second card timer exactly while a displayed card's time advances."
  (if (and (hermes-chat--card-displayed-p (current-buffer))
           (seq-some (lambda (node) (hermes-chat--card-ticking-p (ewoc-data node)))
                     hermes-chat--card-nodes))
      (unless hermes-chat--card-timer
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
  ;; Hidden chats poll only while the segment wants their counts.
  (when (fboundp 'hermes-chat--work-visibility)
    (hermes-chat--work-visibility))
  (force-mode-line-update t))

(defcustom hermes-chat-work-indicator t
  "Whether other chats' live work is shown in the global mode line.
When non-nil, `global-mode-string' lists each chat that no window displays
and that has running work or failures you have not seen, such as
\"[Hermes 🤖 claude-code 2 · gnosis ✗ 1]\".  Clicking a chat switches to it.
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
                      (and (eq (hermes-work-display-state row) 'failed)
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
             (if (> failed 0) (propertize (format " ✗ %d" failed) 'face 'hermes-work-failed) ""))
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
                                   (propertize (format " · ✗ %d" failed) 'face 'hermes-work-failed)
                                 ""))
                       'mouse-face 'mode-line-highlight 'local-map map
                       'help-echo "mouse-1: switch to a chat with work"))
       (mapconcat #'hermes-chat--card-indicator-item chats " · "))
     "]")))

(provide 'hermes-chat-cards)
;;; hermes-chat-cards.el ends here
