;;; hermes-subagents.el --- Active subagent browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' view of active Hermes subagents from `delegation.status',
;; indented by spawn depth to show the delegation tree.  Instance inventory
;; is read-only.
;;
;; The chat Workers view is an outline of one chat's agents and background
;; processes.  `hermes-work--outline' is a pure function from the roster to
;; lines; the view only inserts them, keeping point, folds, cards and each
;; window's start across live repaints.  It owns session-qualified worker
;; actions; RET opens one reused detail buffer per row.

;;; Code:

(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-browser)

(defun hermes-subagents--rows (result)
  "Return `tabulated-list' entries for a `delegation.status' RESULT.
Each active subagent's goal is indented by its spawn depth."
  (mapcar
   (lambda (subagent)
     (let ((id (hermes-transport--scalar-string
                (hermes-transport--get subagent 'subagent_id)))
           (depth (or (hermes-transport--get subagent 'depth) 0)))
       (list id
             (vector (hermes-browser--face-cell
                      (concat (make-string (* 2 (max 0 depth)) ?\s)
                              (or (hermes-transport--scalar-string
                                   (hermes-transport--get subagent 'goal)) ""))
                      'hermes-browser-goal)
                     (hermes-browser--status-cell
                      (or (hermes-transport--scalar-string
                           (hermes-transport--get subagent 'status)) "")
                      'hermes-browser-status)
                     (hermes-browser--face-cell
                      (or (hermes-transport--scalar-string
                           (hermes-transport--get subagent 'model)) "")
                      'hermes-browser-model)
                     (hermes-browser--face-cell
                      (or (hermes-transport--get subagent 'tool_count) 0)
                      'hermes-browser-tool-count)))))
   (hermes-transport--get result 'active)))

(defun hermes-subagents-interrupt ()
  "Refuse control from the read-only instance inventory.
Open Workers from the owning chat to request a session-qualified interrupt."
  (interactive nil hermes-subagents-mode)
  (user-error "Instance inventory is read-only; open Workers from its chat"))

;;;###autoload (autoload 'hermes-list-subagents "hermes-subagents" nil t)
(hermes-define-list-browser subagents
  :title "Hermes Subagents"
  :buffer "*Hermes Subagents*"
  :doc "Major mode listing active Hermes subagents."
  :command-doc "Browse active Hermes subagents as a delegation tree."
  :columns [("Subagent" 44 t) ("Status" 12 t) ("Model" 18 t) ("Tools" 6 t)]
  :fetch (lambda (client)
           (hermes-dashboard-transport-call-fn
            #'hermes-dashboard-transport-delegation-status client))
  :rows #'hermes-subagents--rows)

;;; Exact-session observations

(require 'hermes-chat-buffer)

(defface hermes-work-failed '((t :inherit error))
  "Face for failed, killed or lost work." :group 'hermes)
(defface hermes-work-done '((t :inherit success))
  "Face for completed work." :group 'hermes)
(defface hermes-work-queued '((t :inherit shadow))
  "Face for queued workers." :group 'hermes)
(defface hermes-work-interrupted '((t :inherit warning))
  "Face for interrupted, timed out or killed work." :group 'hermes)
(defface hermes-work-unknown '((t :inherit shadow))
  "Face for work in an unrecognized state." :group 'hermes)
(defface hermes-work-section '((t :inherit bold))
  "Face for section titles in the work view." :group 'hermes)
(defface hermes-work-meta '((t :inherit shadow))
  "Face for quiet row metadata: model, tools, elapsed." :group 'hermes)
(defface hermes-work-activity '((t :inherit shadow))
  "Face for a row's latest activity text." :group 'hermes)
(defface hermes-work-tool '((t :inherit font-lock-function-name-face))
  "Face for tool names." :group 'hermes)
(defface hermes-work-label '((t :inherit shadow))
  "Face for field labels in cards and details." :group 'hermes)
(defface hermes-work-fading '((t :inherit shadow))
  "Face for exited processes about to leave the view." :group 'hermes)

(defvar-local hermes-work--owner nil
  "Exact chat attachment displayed by this view, never a buffer name.")

(defun hermes-work--view-p (owner)
  "Return non-nil when OWNER still owns its exact list buffer."
  (let ((view (plist-get owner :view)))
    (and (buffer-live-p view)
         (with-current-buffer view (hermes-buffer--owned-p 'hermes-work-mode))
         (eq owner (buffer-local-value 'hermes-work--owner view)))))

(defun hermes-work--current-p (owner)
  "Return non-nil when OWNER is a current chat attachment."
  (when-let* ((current (plist-get owner :current-p)))
    (funcall current owner)))

(defun hermes-work--observations (owner)
  "Return OWNER's displayed workers followed by its displayed processes."
  (let ((roster (hermes-chat--work-roster owner (float-time))))
    (append (plist-get roster :agents) (plist-get roster :processes))))

(defun hermes-work--find (key rows)
  "Return the row in ROWS whose key is KEY."
  (seq-find (lambda (row) (equal key (plist-get row :key))) rows))

;;;; Pure outline model

(defun hermes-work--glyph (unicode ascii)
  "Return UNICODE when displayable, else ASCII."
  (if (char-displayable-p (aref unicode 0)) unicode ascii))

(defun hermes-work--state (row)
  "Return (GLYPH FACE LABEL) for ROW's state."
  (let ((state (plist-get row :state)) (process (eq (plist-get row :kind) 'process)))
    (pcase state
      ('running (list (if process (hermes-work--glyph "⚙" "%") (hermes-work--glyph "●" "*"))
                      'hermes-work-running "running"))
      ('queued (list (hermes-work--glyph "○" "o") 'hermes-work-queued "queued"))
      ((or 'done 'completed) (list (hermes-work--glyph "✓" "+") 'hermes-work-done "done"))
      ((or 'failed 'error) (list (hermes-work--glyph "✗" "x") 'hermes-work-failed "failed"))
      ((or 'interrupted 'cancelled 'timeout 'killed)
       (list (hermes-work--glyph "■" "!") 'hermes-work-interrupted (symbol-name state)))
      ('lost (list "?" 'hermes-work-unknown "gone, exit unseen"))
      (_ (list (hermes-work--glyph "·" ".") 'hermes-work-unknown
               (if state (format "%s" state) "unknown"))))))

(defun hermes-work--duration (seconds)
  "Return SECONDS as a compact duration such as 48s, 3m 12s or 1h 05m."
  (let ((s (max 0 (round seconds))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm %02ds" (/ s 60) (% s 60)))
          (t (format "%dh %02dm" (/ s 3600) (/ (% s 3600) 60))))))

(defun hermes-work--model (model)
  "Return MODEL without its vendor prefix and version suffix, or nil."
  (and (stringp model) (not (member model '("" "—")))
       (replace-regexp-in-string "\\`claude-\\|\\(-[0-9][0-9.-]*\\)+\\'" "" model)))

(defun hermes-work--agent (row activity)
  "Return worker ROW with its ACTIVITY entry merged in.
Event tool counts and tools are newer than the snapshot's."
  (let* ((act (cdr (assoc (plist-get row :id) activity)))
         (tools (plist-get act :tools)))
    (append (list :tools tools
                  :tool-count (if (and (plist-get act :tool-count) (plist-get row :tool-count))
                                  (max (plist-get act :tool-count) (plist-get row :tool-count))
                                (or (plist-get act :tool-count) (plist-get row :tool-count)
                                    (and tools (length tools))))
                  :last-tool (or (car (last tools))
                                 (and (plist-get row :last-tool)
                                      (list (plist-get row :last-tool))))
                  :started (or (plist-get row :started) (plist-get act :seen))
                  :finished (plist-get act :finished)
                  :summary (plist-get act :summary) :error (plist-get act :error))
            row)))

(defun hermes-work--elapsed (row now)
  "Return ROW's elapsed seconds at NOW, or nil when unknown."
  (or (plist-get row :uptime)
      (when-let* ((started (plist-get row :started)))
        (max 0 (- (if (hermes-chat--work-active-p row) now
                    (or (plist-get row :finished) now))
                  started)))))

(defun hermes-work--depths (rows)
  "Return an alist of ROWS' IDs to tree depth; unlisted parents make roots."
  (let ((ids (mapcar (lambda (row) (plist-get row :id)) rows)) depths)
    (dolist (row rows (nreverse depths))
      (let ((parent (plist-get row :parent)))
        (push (cons (plist-get row :id)
                    (if (member parent ids) (1+ (or (cdr (assoc parent depths)) 0)) 0))
              depths)))))

(defun hermes-work--fit (text width)
  "Return TEXT truncated to WIDTH columns with an ellipsis."
  (truncate-string-to-width (or text "") (max 1 width) nil nil "…"))

(defun hermes-work--align (meta)
  "Return META right-aligned against the window edge."
  (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width meta)))))
          meta))

(defun hermes-work--line (key kind depth text &optional n)
  "Return an outline line plist for KEY of KIND at DEPTH showing TEXT.
N numbers repeated lines of one KIND, such as card fields."
  (list :key key :kind kind :depth depth :text text :n (or n 0)))

(defun hermes-work--heading (key depth state title meta width &optional extra)
  "Return a heading line for KEY at DEPTH: STATE glyph, TITLE, then META.
Truncate TITLE to fit WIDTH; EXTRA follows it unaligned."
  (let* ((face (and meta (> (length meta) 0) (get-text-property 0 'face meta)))
         (fixed (+ (* 2 depth) 2 2 (string-width (or extra ""))))
         (parts (split-string (or meta "") " · "))
         ;; Narrow windows keep the goal and drop leading metadata first.
         (meta (progn
                 (while (and (cdr parts)
                             (< (- width fixed (string-width (string-join parts " · ")))
                                (max 8 (/ width 3))))
                   (pop parts))
                 (propertize (string-join parts " · ") 'face face)))
         (room (- width fixed (string-width meta))))
    (hermes-work--line
     key 'heading depth
     (concat (propertize (nth 0 state) 'face (nth 1 state)) " "
             (propertize (hermes-work--fit title room)
                         'face (if (eq (nth 1 state) 'hermes-work-done) 'shadow 'default)
                         'help-echo title)
             (or extra "")
             (hermes-work--align meta)))))

(defun hermes-work--activity (key depth tool preview width)
  "Return KEY's activity line at DEPTH naming TOOL and PREVIEW within WIDTH."
  (let ((room (- width (* 2 depth) 2)))
    (hermes-work--line
     key 'activity depth
     (concat (propertize "↳ " 'face 'shadow)
             (if tool
                 (concat (propertize (hermes-work--fit tool room) 'face 'hermes-work-tool)
                         (if (and preview (> (- room (string-width tool) 1) 0))
                             (concat " " (propertize (hermes-work--fit
                                                      preview (- room (string-width tool) 1))
                                                     'face 'hermes-work-activity))
                           ""))
               (propertize (hermes-work--fit preview room) 'face 'hermes-work-activity))))))

(defun hermes-work--wrap (text width)
  "Return TEXT's lines filled to WIDTH, at most four."
  (let* ((lines (split-string (string-fill (string-trim (or text "")) (max 20 width)) "\n"))
         (shown (seq-take lines 4)))
    (if (> (length lines) 4)
        (append (butlast shown) (list (concat (car (last shown)) " …")))
      shown)))

(defun hermes-work--card (key depth fields width)
  "Return card lines for KEY at DEPTH from FIELDS, a list of (LABEL TEXT FACE).
Fields with no text are omitted; text is wrapped to WIDTH."
  (let ((n 0) lines)
    (dolist (field fields (nreverse lines))
      (when-let* ((text (nth 1 field))
                  ((not (string-empty-p text))))
        (let ((label (propertize (format "%-9s" (nth 0 field)) 'face 'hermes-work-label)))
          (dolist (part (hermes-work--wrap text (- width (* 2 depth) 9)))
            (push (hermes-work--line key 'card depth
                                     (concat label (propertize part 'face (nth 2 field)))
                                     (cl-incf n))
                  lines)
            (setq label (make-string 9 ?\s))))))))

(defun hermes-work--agent-meta (row now)
  "Return worker ROW's quiet metadata at NOW: model, tools, elapsed, verdict."
  (let ((count (plist-get row :tool-count))
        (elapsed (hermes-work--elapsed row now))
        (state (plist-get row :state)))
    (string-join
     (delq nil (list (hermes-work--model (plist-get row :model))
                     (and count (format "%d tool%s" count (if (= count 1) "" "s")))
                     (and elapsed (hermes-work--duration elapsed))
                     (unless (memq state '(running queued done completed))
                       (nth 2 (hermes-work--state row)))))
     " · ")))

(defun hermes-work--agent-lines (row depth view now width hidden)
  "Return the lines for worker ROW at DEPTH under VIEW at NOW within WIDTH.
HIDDEN counts descendants folded under it."
  (let* ((key (plist-get row :key))
         (state (hermes-work--state row))
         (meta (hermes-work--agent-meta row now))
         (tool (plist-get row :last-tool))
         (card (member key (plist-get view :cards))))
    (append
     (list (hermes-work--heading
            key depth state (plist-get row :goal)
            (propertize meta 'face (if (memq (plist-get row :state) '(failed error))
                                       'hermes-work-failed 'hermes-work-meta))
            width (and hidden (> hidden 0)
                       (propertize (format " +%d" hidden) 'face 'hermes-work-meta))))
     (pcase (plist-get row :state)
       ('running (and tool (list (hermes-work--activity key (1+ depth) (car tool) (cdr tool) width))))
       ('queued (list (hermes-work--activity key (1+ depth) nil "waiting for a slot" width))))
     (and card
          (hermes-work--card
           key (+ 2 depth)
           (list (list "Goal" (plist-get row :goal) 'default)
                 (list "Error" (plist-get row :error) 'hermes-work-failed)
                 (list "Summary" (plist-get row :summary) 'default)
                 (list "Recent"
                       (and (plist-get row :tools)
                            (format "%s  (%s)"
                                    (mapconcat #'car (last (plist-get row :tools) 4) " · ")
                                    (or (plist-get row :tool-count) (length (plist-get row :tools)))))
                       'hermes-work-tool))
           width)))))

(defun hermes-work--process-meta (row now)
  "Return process ROW's elapsed time or exit verdict at NOW."
  (let ((ago (and (plist-get row :exited-at)
                  (format " · %s ago" (hermes-work--duration (- now (plist-get row :exited-at))))))
        (elapsed (hermes-work--elapsed row now)))
    (pcase (plist-get row :state)
      ('running (if elapsed (hermes-work--duration elapsed) ""))
      ((or 'done 'failed)
       (concat (if (equal (plist-get row :reason) "failed_start") "failed to start"
                 (format "exit %s" (or (plist-get row :exit-code) "?")))
               ago))
      ('killed (concat "killed" ago))
      ('lost "gone, exit unseen")
      (_ (or ago "")))))

(defun hermes-work--process-lines (row view now width)
  "Return the lines for process ROW under VIEW at NOW within WIDTH."
  (let* ((key (plist-get row :key))
         (running (eq (plist-get row :state) 'running))
         (state (hermes-work--state row))
         (line (hermes-work--heading
                key 0 state (plist-get row :command)
                (propertize (hermes-work--process-meta row now) 'face 'hermes-work-meta)
                width)))
    (unless running
      (let ((text (plist-get line :text)))
        (alter-text-property 2 (length text) 'face
                             (lambda (face) (if (eq face 'default) 'hermes-work-fading face))
                             text)))
    (append
     (list line)
     (and running (plist-get row :last-line)
          (list (hermes-work--activity key 1 nil (plist-get row :last-line) width)))
     (and (member key (plist-get view :cards))
          (hermes-work--card
           key 2
           (list (list "Command" (plist-get row :command) 'default)
                 (list "Cwd" (plist-get row :cwd) 'hermes-work-activity)
                 (list "Output" (unless running (plist-get row :last-line)) 'hermes-work-activity)
                 (list "Id" (plist-get row :id) 'hermes-work-meta))
           width)))))

(defun hermes-work--counts-text (rows)
  "Return a quiet count summary of ROWS by state label."
  (let (counts)
    (dolist (row rows)
      (let ((label (pcase (plist-get row :state)
                     ('running "running") ('queued "queued")
                     ((or 'done 'completed) (if (eq (plist-get row :kind) 'process) "exited" "done"))
                     ((or 'failed 'error) "failed")
                     (_ "stopped"))))
        (setf (alist-get label counts nil nil #'equal)
              (1+ (alist-get label counts 0 nil #'equal)))))
    (mapconcat (lambda (label)
                 (format "%d %s" (cdr (assoc label counts)) label))
               (seq-filter (lambda (label) (assoc label counts))
                           '("running" "queued" "done" "exited" "failed" "stopped"))
               " · ")))

(defun hermes-work--section (key title rows view)
  "Return the heading line of section KEY titled TITLE over ROWS under VIEW."
  (hermes-work--line
   key 'section 0
   (concat (propertize (if (member key (plist-get view :folded)) "▸ " "▾ ") 'face 'shadow)
           (propertize title 'face 'hermes-work-section) "  "
           (propertize (hermes-work--counts-text rows) 'face 'hermes-work-meta))))

(defun hermes-work--outline (roster activity view now width)
  "Return the outline lines for ROSTER with ACTIVITY under VIEW at NOW.
VIEW is a plist of :cards, open card keys, :folded, folded section and
subtree keys, and :observed, non-nil once any roster was read.  Lines fit
WIDTH columns.  The view only inserts these lines."
  (let* ((agents (mapcar (lambda (row) (hermes-work--agent row activity))
                         (plist-get roster :agents)))
         (processes (plist-get roster :processes))
         (depths (hermes-work--depths agents))
         (folded (plist-get view :folded))
         lines)
    (if (not (or agents processes))
        (list (hermes-work--line
               '(section . empty) 'empty 0
               (propertize (if (plist-get view :observed) "No workers in this chat yet."
                             "Loading workers…")
                           'face 'shadow)))
      (when agents
        (push (list (hermes-work--section '(section . agents) "Agents" agents view)) lines)
        (unless (member '(section . agents) folded)
          (let (skip-below)
            (dolist (row agents)
              (let ((depth (cdr (assoc (plist-get row :id) depths))))
                (if (and skip-below (> depth skip-below)) nil
                  (setq skip-below nil)
                  (let ((hidden (and (member (plist-get row :key) folded)
                                     (seq-count (lambda (other)
                                                  (hermes-work--descendant-p
                                                   other row agents))
                                                agents))))
                    (when (and hidden (> hidden 0)) (setq skip-below depth))
                    (push (hermes-work--agent-lines row (min depth 4) view now width hidden)
                          lines))))))))
      (when processes
        (when agents (push (list (hermes-work--line nil 'blank 0 "")) lines))
        (push (list (hermes-work--section '(section . processes) "Processes" processes view))
              lines)
        (unless (member '(section . processes) folded)
          (dolist (row processes)
            (push (hermes-work--process-lines row view now width) lines))))
      (apply #'append (nreverse lines)))))

(defun hermes-work--descendant-p (row ancestor rows)
  "Return non-nil when ROW descends from ANCESTOR among ROWS."
  (let ((parent (plist-get row :parent)) (seen nil))
    (while (and parent (not (equal parent (plist-get ancestor :id)))
                (not (member parent seen)))
      (push parent seen)
      (setq parent (plist-get (hermes-chat--work-row-id parent rows) :parent)))
    (and parent (equal parent (plist-get ancestor :id)))))

;;;; Rendering

(defvar-local hermes-work--cards nil
  "Keys whose inline card is open.")
(defvar-local hermes-work--folded nil
  "Section and worker keys whose contents are folded.")
(defvar-local hermes-work--order nil
  "Row keys in the order last rendered, to place point after a row vanishes.")
(defvar-local hermes-work--detail-key nil
  "Row key this detail buffer shows; fixed at creation.")

(defvar-local hermes-work--width nil
  "Window width the outline was last fitted to.")

(defun hermes-work--context-owner ()
  "Return this chat or work view's exact owner, without following replacements."
  (if (derived-mode-p 'hermes-work-mode 'hermes-work-detail-mode) hermes-work--owner
    (and (derived-mode-p 'hermes-chat-mode) hermes-chat--work-owner)))

;;;###autoload
(defun hermes-chat-workers-label ()
  "Return a Workers count for the invoking chat or its exact work view."
  (let* ((owner (hermes-work--context-owner))
         (count (if (hermes-work--current-p owner)
                    (car (hermes-chat--work-counts
                          (hermes-chat--work-roster owner (float-time))))
                  0)))
    (concat "Workers " (propertize (number-to-string count) 'face 'keymap-popup-value))))

(defun hermes-work--scope-text (owner)
  "Return local scope details for OWNER, even when detached."
  (concat
   (format "Scope: runtime %s\n\n" (plist-get owner :runtime))
   "Roster visibility includes resumed lineage, not control authority.\nThe backend may refuse interrupt, steering or tail for a visible worker.\n\n"
   (if (buffer-live-p (plist-get owner :buffer))
       (with-current-buffer (plist-get owner :buffer)
         (let ((hermes-chat--work-owner owner)) (hermes-chat--work-details)))
     "Owner destroyed.")))

(defun hermes-work-scope-details ()
  "Display scope and limitations for this exact work view."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (let ((text (hermes-work--scope-text hermes-work--owner))
        (buffer (hermes-buffer--get "*Hermes Work Scope*" #'help-mode)))
    (with-help-window buffer (princ text))))

(defun hermes-work--key (face text)
  "Return key TEXT in FACE for header lines."
  (propertize text 'face face))

(defun hermes-work--header (owner roster)
  "Return the header line for OWNER's ROSTER."
  (if (not (hermes-work--current-p owner))
      (concat " " (propertize "last known · chat detached · reopen from the chat" 'face 'warning)
              "  " (hermes-work--key 'help-key-binding "?")
              (propertize " actions" 'face 'shadow))
    (let* ((counts (hermes-chat--work-counts roster))
           (count (lambda (states)
                    (seq-count (lambda (row) (memq (plist-get row :state) states))
                               (plist-get roster :agents))))
           (failed (funcall count '(failed error)))
           (item (lambda (glyph face count label)
                   (concat "  " (propertize glyph 'face face) " "
                           (propertize (number-to-string count) 'face 'font-lock-constant-face)
                           (if label (propertize label 'face 'shadow) "")))))
      (concat
       (funcall item (hermes-work--glyph "●" "*") 'hermes-work-running
                (funcall count '(running)) " running")
       (if (> failed 0) (funcall item (hermes-work--glyph "✗" "x") 'hermes-work-failed failed nil) "")
       (if (> (cdr counts) 0)
           (funcall item (hermes-work--glyph "⚙" "%") 'hermes-work-running (cdr counts) nil) "")
       "   " (hermes-work--key 'help-key-binding "?") (propertize " actions" 'face 'shadow)))))

(defun hermes-work--line-position (pos)
  "Return the logical position of POS: (KEY KIND N COLUMN LINE)."
  (save-excursion
    (goto-char pos)
    (let ((bol (line-beginning-position)))
      (list (get-text-property bol 'hermes-work-key)
            (get-text-property bol 'hermes-work-line)
            (get-text-property bol 'hermes-work-n)
            (- pos bol) (line-number-at-pos nil t)))))

(defun hermes-work--find-line (key &optional kind n)
  "Return the start of KEY's line of KIND numbered N, or nil.
Without KIND, return KEY's first line."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (when (and (equal key (get-text-property (point) 'hermes-work-key))
                   (or (not kind)
                       (and (eq kind (get-text-property (point) 'hermes-work-line))
                            (eql (or n 0) (get-text-property (point) 'hermes-work-n)))))
          (throw 'found (point)))
        (forward-line 1)))))

(defun hermes-work--restore (position order)
  "Return the buffer point for logical POSITION after a repaint.
A vanished key falls back to the next surviving key in previous ORDER, then
the previous one, then the same line number."
  (pcase-let ((`(,key ,kind ,n ,column ,line) position))
    (let* ((tail (member key order))
           (start (or (and key (or (hermes-work--find-line key kind n)
                                   (hermes-work--find-line key)))
                      (seq-some #'hermes-work--find-line (cdr tail))
                      (seq-some #'hermes-work--find-line
                                (cdr (member key (reverse order)))))))
      (save-excursion
        (if start (goto-char start)
          (goto-char (point-min))
          (forward-line (1- line)))
        (min (+ (point) (if (equal key (get-text-property (point) 'hermes-work-key)) column 0))
             (line-end-position))))))

(defun hermes-work--insert (lines)
  "Insert outline LINES, marking each with its key, kind, number and depth."
  (dolist (line lines)
    (let ((start (point)))
      (insert (make-string (* 2 (plist-get line :depth)) ?\s) (plist-get line :text) "\n")
      (add-text-properties start (point)
                           (list 'hermes-work-key (plist-get line :key)
                                 'hermes-work-line (plist-get line :kind)
                                 'hermes-work-n (plist-get line :n)
                                 'hermes-work-depth (plist-get line :depth))))))

(defun hermes-work--repaint (lines)
  "Replace this buffer with LINES, keeping point and every window's place."
  (let* ((buffer (current-buffer))
         (order hermes-work--order)
         (point (hermes-work--line-position (point)))
         (windows (mapcar (lambda (window)
                            (list window (hermes-work--line-position (window-start window))
                                  (hermes-work--line-position (window-point window))))
                          (get-buffer-window-list buffer nil t))))
    (let ((inhibit-read-only t) (inhibit-modification-hooks t))
      (erase-buffer)
      (hermes-work--insert lines))
    (setq hermes-work--order
          (delete-dups (delq nil (mapcar (lambda (line) (plist-get line :key)) lines))))
    (goto-char (hermes-work--restore point order))
    (dolist (entry windows)
      (let ((window (car entry)))
        (set-window-start window (hermes-work--restore (nth 1 entry) order) t)
        (set-window-point window (hermes-work--restore (nth 2 entry) order))))))

(defun hermes-work--fit-width ()
  "Return the width of the widest window showing this buffer, or 80."
  (let ((widths (mapcar #'window-body-width (get-buffer-window-list nil nil t))))
    (if widths (apply #'max widths) 80)))

(defun hermes-work--render (owner)
  "Repaint OWNER's exact view and details from snapshots, without selecting them."
  (when (hermes-work--view-p owner)
    (let* ((now (float-time))
           (roster (hermes-chat--work-roster owner now)))
      (with-current-buffer (plist-get owner :view)
        (setq hermes-work--width (hermes-work--fit-width)
              header-line-format (hermes-work--header owner roster))
        ;; This read-only projection must not call modification hooks mid-print:
        ;; they can replace the view or unwind an unfinished chat settlement.
        (hermes-work--repaint
         (append
          (unless (hermes-work--current-p owner)
            (list (hermes-work--line nil 'blank 0
                                     (propertize "Detached: last known workers; reopen from the chat."
                                                 'face 'warning))))
          (hermes-work--outline
           roster (plist-get owner :activity)
           (list :cards hermes-work--cards :folded hermes-work--folded
                 :observed (or (plist-get owner :delegates) (plist-get owner :processes)))
           now hermes-work--width))))
      (dolist (buffer (buffer-list))
        (when (and (eq owner (buffer-local-value 'hermes-work--owner buffer))
                   (hermes-browser--buffer-mode-p buffer 'hermes-work-detail-mode))
          (with-current-buffer buffer
            (when (hermes-buffer--owned-p 'hermes-work-detail-mode)
              (hermes-work--detail-render owner roster now))))))))

(defun hermes-work--changed ()
  "Repaint the current chat's observation list after a local state change."
  (when hermes-chat--work-owner (hermes-work--render hermes-chat--work-owner)))

(defun hermes-work--detach ()
  "Release only this exact view's cross-link on kill or mode change."
  (when (eq (plist-get hermes-work--owner :view) (current-buffer))
    (setf (plist-get hermes-work--owner :view) nil)
    (hermes-work--settle-owner hermes-work--owner)))

(defun hermes-work--resize (_window)
  "Refit the outline after a width change of _WINDOW or its siblings."
  (when (and (derived-mode-p 'hermes-work-mode)
             (not (eql hermes-work--width (hermes-work--fit-width))))
    (hermes-work--render hermes-work--owner)))

(defun hermes-work-refresh ()
  "Request a refresh from this view's exact owner, if still attached."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (unless (and (hermes-work--view-p hermes-work--owner)
               (hermes-work--current-p hermes-work--owner))
    (user-error "Work owner detached; reopen from the attached chat"))
  (funcall (plist-get hermes-work--owner :refresh)))

(defun hermes-work-instance-subagents ()
  "Browse Instance subagents explicitly, outside this session's observations."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (unless (hermes-work--current-p hermes-work--owner)
    (user-error "Work owner detached"))
  (let ((hermes-instance (plist-get hermes-work--owner :instance)))
    (hermes-list-subagents)))

;;;; Outline navigation and folding

(defun hermes-work--key-at-point ()
  "Return the key of the row a command at point acts on, or nil."
  (if (derived-mode-p 'hermes-work-detail-mode) hermes-work--detail-key
    (get-text-property (line-beginning-position) 'hermes-work-key)))

(defun hermes-work--heading-p ()
  "Return non-nil when this line is a row or section heading."
  (memq (get-text-property (line-beginning-position) 'hermes-work-line) '(heading section)))

(defun hermes-work-next (&optional n)
  "Move to the Nth next row or section heading."
  (interactive "p" hermes-work-mode)
  (let ((step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (let ((found (save-excursion
                     (catch 'found
                       (while (and (zerop (forward-line step)) (not (eobp)))
                         (when (hermes-work--heading-p) (throw 'found (point))))))))
        (if found (goto-char found) (message "No more rows"))))))

(defun hermes-work-previous (&optional n)
  "Move to the Nth previous row or section heading."
  (interactive "p" hermes-work-mode)
  (hermes-work-next (- (or n 1))))

(defun hermes-work--heading-start ()
  "Return the start of the heading owning this line, or nil."
  (when-let* ((key (get-text-property (line-beginning-position) 'hermes-work-key)))
    (hermes-work--find-line key 'heading 0)))

(defun hermes-work--relative (step predicate)
  "Return the first heading STEP lines away satisfying PREDICATE on its depth.
PREDICATE returns `stop' to end the search."
  (let* ((start (or (hermes-work--heading-start) (user-error "Not on a worker")))
         (depth (get-text-property start 'hermes-work-depth)))
    (save-excursion
      (goto-char start)
      (catch 'found
        (while (and (zerop (forward-line step)) (not (eobp)))
          (pcase (get-text-property (point) 'hermes-work-line)
            ((or 'section 'blank) (throw 'found nil))
            ('heading
             (pcase (funcall predicate (get-text-property (point) 'hermes-work-depth) depth)
               ('stop (throw 'found nil))
               ('nil nil)
               (_ (throw 'found (point)))))))))))

(defun hermes-work-forward-sibling ()
  "Move to the next row with the same parent."
  (interactive nil hermes-work-mode)
  (if-let* ((found (hermes-work--relative
                    1 (lambda (d depth) (cond ((< d depth) 'stop) ((= d depth) t))))))
      (goto-char found)
    (message "No next sibling")))

(defun hermes-work-backward-sibling ()
  "Move to the previous row with the same parent."
  (interactive nil hermes-work-mode)
  (if-let* ((found (hermes-work--relative
                    -1 (lambda (d depth) (cond ((< d depth) 'stop) ((= d depth) t))))))
      (goto-char found)
    (message "No previous sibling")))

(defun hermes-work-up ()
  "Move to this worker's parent."
  (interactive nil hermes-work-mode)
  (if-let* ((found (hermes-work--relative -1 (lambda (d depth) (< d depth)))))
      (goto-char found)
    (message "Top-level row; its parent is the chat")))

(defun hermes-work--toggle-key (key list)
  "Return LIST with KEY added, or removed when present."
  (if (member key list) (remove key list) (cons key list)))

(defun hermes-work-toggle ()
  "Fold the section at point, or cycle the row's card and subtree.
A worker with children cycles normal, card, folded subtree; other rows toggle
their card."
  (interactive nil hermes-work-mode)
  (let* ((bol (line-beginning-position))
         (key (get-text-property bol 'hermes-work-key))
         (kind (get-text-property bol 'hermes-work-line))
         (start (hermes-work--heading-start))
         (parent (and start (eq (car-safe key) 'delegate)
                      (save-excursion
                        (goto-char start)
                        (let ((depth (get-text-property (point) 'hermes-work-depth)))
                          (catch 'found
                            (while (and (zerop (forward-line 1)) (not (eobp)))
                              (when (eq (get-text-property (point) 'hermes-work-line) 'heading)
                                (throw 'found
                                       (> (get-text-property (point) 'hermes-work-depth)
                                          depth)))))))))
         (folded (member key hermes-work--folded)))
    (cond
     ((null key) (user-error "Nothing to fold here"))
     ((eq kind 'section)
      (setq hermes-work--folded (hermes-work--toggle-key key hermes-work--folded)))
     (folded (setq hermes-work--folded (remove key hermes-work--folded)))
     ((and parent (member key hermes-work--cards))
      (setq hermes-work--cards (remove key hermes-work--cards))
      (push key hermes-work--folded))
     (t (setq hermes-work--cards (hermes-work--toggle-key key hermes-work--cards))))
    (when start (goto-char start))
    (hermes-work--render hermes-work--owner)))

(defun hermes-work-toggle-all ()
  "Open every row's card, or close them all."
  (interactive nil hermes-work-mode)
  (setq hermes-work--cards
        (unless hermes-work--cards
          (seq-filter (lambda (key) (memq (car-safe key) '(delegate process)))
                      hermes-work--order)))
  (hermes-work--render hermes-work--owner))

;;;; Worker details

(defun hermes-work--detail-field (label text &optional face)
  "Return a detail line labelled LABEL showing TEXT in FACE, or empty."
  (if (and text (not (equal text "")))
      (concat (propertize (format "%-9s " label) 'face 'hermes-work-label)
              (propertize (format "%s" text) 'face (or face 'default)) "\n")
    ""))

(defun hermes-work--detail-text (row agents now)
  "Return the detail text for ROW at NOW, with AGENTS for parent and children."
  (let* ((state (hermes-work--state row))
         (process (eq (plist-get row :kind) 'process))
         (id (plist-get row :id))
         (children (seq-filter (lambda (other) (equal (plist-get other :parent) id)) agents))
         (parent (hermes-chat--work-row-id (plist-get row :parent) agents))
         (tools (plist-get row :tools))
         (title (lambda (text) (concat "\n" (propertize text 'face 'hermes-work-section) "\n"))))
    (concat
     (propertize (nth 0 state) 'face (nth 1 state)) " "
     (propertize (or (if process (plist-get row :command) (plist-get row :goal)) "—")
                 'face 'bold)
     "\n"
     (propertize
      (string-join
       (delq nil
             (if process
                 (list (nth 2 state) (hermes-work--process-meta row now))
               (list (nth 2 state)
                     (and (plist-get row :depth) (format "depth %d" (plist-get row :depth)))
                     (hermes-work--model (plist-get row :model))
                     (and (hermes-work--elapsed row now)
                          (hermes-work--duration (hermes-work--elapsed row now)))
                     (and (plist-get row :tool-count)
                          (format "%d tools" (plist-get row :tool-count)))
                     (and children (format "%d children" (length children))))))
       " · ")
      'face 'hermes-work-meta)
     "\n\n"
     (hermes-work--detail-field "Id" id 'hermes-work-meta)
     (if process
         (concat (hermes-work--detail-field "Remote cwd (inert)" (plist-get row :cwd)
                                            'hermes-work-activity)
                 (hermes-work--detail-field "Reason" (plist-get row :reason))
                 (funcall title "Output tail")
                 (or (plist-get row :output-tail) "—") "\n")
       (concat
        (hermes-work--detail-field "Parent" (if parent (plist-get parent :goal) "the chat"))
        (if (plist-get row :accepting-steer) "" (hermes-work--detail-field "Steering" "closed"))
        (when-let* ((tool (and (eq (plist-get row :state) 'running) (plist-get row :last-tool))))
          (concat (funcall title "Now")
                  (propertize (car tool) 'face 'hermes-work-tool) " "
                  (propertize (or (cdr tool) "") 'face 'hermes-work-activity) "\n"))
        (when-let* ((text (plist-get row :error)))
          (concat (funcall title "Error") (propertize text 'face 'hermes-work-failed) "\n"))
        (when-let* ((text (plist-get row :summary)))
          (concat (funcall title "Summary") text "\n"))
        (when tools
          (concat (funcall title (format "Tools (%s)" (or (plist-get row :tool-count)
                                                          (length tools))))
                  (mapconcat (lambda (tool)
                               (concat (propertize "· " 'face 'shadow)
                                       (propertize (car tool) 'face 'hermes-work-tool) " "
                                       (propertize (or (cdr tool) "") 'face 'hermes-work-activity)))
                             (reverse tools) "\n")
                  "\n"
                  (let ((earlier (- (or (plist-get row :tool-count) 0) (length tools))))
                    (if (> earlier 0)
                        (propertize (format "… %d earlier\n" earlier) 'face 'shadow) ""))))
        (when children
          (concat (funcall title (format "Children (%d)" (length children)))
                  (mapconcat (lambda (child)
                               (let ((s (hermes-work--state child)))
                                 (concat (propertize (nth 0 s) 'face (nth 1 s)) " "
                                         (plist-get child :goal))))
                             children "\n")
                  "\n")))))))

(defun hermes-work--detail-header (row listed)
  "Return the detail header for ROW; LISTED is nil once it left the roster."
  (let ((state (hermes-work--state row)) (process (eq (plist-get row :kind) 'process)))
    (concat " " (propertize (if process "Process" "Worker") 'face 'shadow) " "
            (propertize (hermes-buffer--mode-line-literal (plist-get row :id))
                        'face 'font-lock-constant-face)
            (propertize " · " 'face 'shadow)
            (propertize (if listed (nth 2 state) "no longer listed") 'face
                        (if listed (nth 1 state) 'warning))
            "   "
            (mapconcat (lambda (pair)
                         (concat (hermes-work--key 'help-key-binding (car pair))
                                 (propertize (concat " " (cdr pair)) 'face 'shadow)))
                       (if process '(("k" . "kill") ("?" . "more"))
                         '(("s" . "steer") ("k" . "interrupt") ("t" . "tail") ("?" . "more")))
                       "  "))))

(defun hermes-work--detail-render (owner roster now)
  "Repaint this detail buffer from OWNER's ROSTER at NOW, keeping place."
  (let* ((agents (mapcar (lambda (row) (hermes-work--agent row (plist-get owner :activity)))
                         (plist-get roster :agents)))
         (row (hermes-work--find hermes-work--detail-key
                                 (append agents (plist-get roster :processes)))))
    (if (not row)
        (setq header-line-format
              (concat (car (split-string (or (format "%s" header-line-format) "") " · "))
                      (propertize " · no longer listed; last known below" 'face 'warning)))
      (setq header-line-format (hermes-work--detail-header row t))
      (hermes-browser--preserve-reading-position
       (lambda ()
         (let ((inhibit-read-only t) (inhibit-modification-hooks t))
           (erase-buffer)
           (insert (hermes-work--detail-text row agents now))))))))

(defun hermes-work--detail-buffer (owner key)
  "Return OWNER's one detail buffer for KEY, creating it once."
  (or (seq-find (lambda (buffer)
                  (and (hermes-browser--buffer-mode-p buffer 'hermes-work-detail-mode)
                       (with-current-buffer buffer
                         (and (hermes-buffer--owned-p 'hermes-work-detail-mode)
                              (eq owner hermes-work--owner)
                              (equal key hermes-work--detail-key)))))
                (buffer-list))
      (let ((buffer (generate-new-buffer
                     (format "*Hermes Worker: %s*"
                             (truncate-string-to-width (cdr key) 12)))))
        (with-current-buffer buffer
          (hermes-work-detail-mode)
          (hermes-buffer--claim 'hermes-work-detail-mode)
          (setq hermes-work--owner owner
                hermes-work--detail-key key))
        buffer)))

(defun hermes-work-open ()
  "Open the row at point in its own reused detail buffer; fold a section."
  (interactive nil hermes-work-mode)
  (let* ((owner hermes-work--owner)
         (key (hermes-work--key-at-point)))
    (cond
     ((memq (car-safe key) '(delegate process))
      (let ((buffer (hermes-work--detail-buffer owner key))
            (now (float-time)))
        (with-current-buffer buffer
          (hermes-work--detail-render owner (hermes-chat--work-roster owner now) now))
        (pop-to-buffer buffer)))
     ((eq (get-text-property (line-beginning-position) 'hermes-work-line) 'section)
      (hermes-work-toggle))
     (t (user-error "No observed work on this line")))))

;;; Session-qualified worker actions

(defun hermes-work--target (owner key)
  "Return OWNER's exact running snapshot row for KEY while it may be controlled.
Live events may have finished a listed row; then there is no target."
  (let* ((source (if (eq (car-safe key) 'process) :processes :delegates))
         (row (hermes-work--find key (plist-get (plist-get owner source) :rows))))
    (and row (hermes-work--view-p owner) (hermes-work--current-p owner)
         (stringp (plist-get owner :runtime))
         (not (string-empty-p (plist-get owner :runtime)))
         (eq (plist-get (hermes-work--find key (hermes-work--observations owner)) :state)
             'running)
         row)))

(defun hermes-work--selected-worker ()
  "Return the exact running worker row at point, or refuse the action.
The row must be in the backend roster; live events may have finished it."
  (let ((key (hermes-work--key-at-point)))
    (or (and (eq (car-safe key) 'delegate) (hermes-work--target hermes-work--owner key))
        (user-error "No current running worker; refresh or reopen from its chat"))))

(defun hermes-work--action-current-p (owner row)
  "Return non-nil if OWNER and exact ROW remain actionable after input."
  (eq row (hermes-work--target owner (plist-get row :key))))

(defun hermes-work--row-current-p (owner row)
  "Return non-nil if exact ROW is in attached OWNER's roster."
  (and (hermes-work--current-p owner)
       (memq row (plist-get (plist-get owner (if (eq (plist-get row :kind) 'process)
                                                 :processes :delegates))
                            :rows))))

(defun hermes-work--control-current-p (operation)
  "Return non-nil if OPERATION retains its original presentation authority."
  (let ((owner (plist-get operation :owner)))
    (and (eq (plist-get operation :view) (plist-get owner :view))
         (hermes-work--view-p owner)
         (hermes-work--row-current-p owner (plist-get operation :row)))))

(defun hermes-work--control-finish (operation text)
  "Settle exact OPERATION once and report TEXT without touching any view."
  (unless (plist-get operation :settled)
    (let ((owner (plist-get operation :owner)))
      (setf (plist-get operation :settled) t
            (plist-get owner :controls) (delq operation (plist-get owner :controls)))
      (hermes-dashboard-transport-cancel-owner-requests
       (plist-get owner :client) operation)
      (message "%s" text))))

(defun hermes-work--control-retire (operation)
  "Retire OPERATION, distinguishing an attempted send from an unsent request."
  (hermes-work--control-finish
   operation
   (format "Hermes: %s %s %s in session %s: %s"
           (plist-get operation :action)
           (if (eq (plist-get (plist-get operation :row) :kind) 'process) "process" "worker")
           (plist-get operation :id) (plist-get operation :session)
           (if (plist-get operation :sent)
               "outcome unknown after retirement; check before retrying"
             "not sent; owner retired"))))

(defun hermes-work--settle-owner (owner)
  "Settle OWNER's retired controls and logs independently of their views."
  (dolist (operation (copy-sequence (plist-get owner :controls)))
    (unless (hermes-work--control-current-p operation)
      (hermes-work--control-retire operation)))
  (hermes-work-log--settle-owner owner))

(defun hermes-work--receipt (action result)
  "Return the message for ACTION's RESULT, claiming no more than it reports."
  (pcase action
    ('steer (if (equal (hermes-transport--get result 'status) "queued")
                "Hermes: steering queued (delivery not confirmed)"
              "Hermes: steering rejected; worker unavailable to this session"))
    ('interrupt (if (eq (hermes-transport--get result 'found) t)
                    "Hermes: worker interrupt accepted"
                  "Hermes: worker not found or unavailable to this session"))
    ('kill (pcase (hermes-transport--get result 'status)
             ("killed" "Hermes: process killed")
             ("already_exited" "Hermes: process had already exited")
             ("not_found" "Hermes: process not found")
             (_ (format "Hermes: process kill failed: %s"
                        (or (hermes-transport--get result 'error) "unknown error")))))))

(defun hermes-work--control (owner row action &optional text)
  "Request ACTION, `interrupt', `steer' or `kill', for OWNER's exact ROW.
Steering sends TEXT.  The backend checks control generation and process
ownership; roster visibility never grants them."
  (let* ((operation (list :owner owner :row row :view (plist-get owner :view)
                          :id (copy-sequence (plist-get row :id))
                          :session (copy-sequence (plist-get owner :runtime))
                          :action (symbol-name action) :sent nil :settled nil))
         (current (lambda ()
                    (and (not (plist-get operation :settled))
                         (hermes-work--action-current-p owner row)
                         (or (not (eq action 'steer)) (plist-get row :accepting-steer)))))
         ;; This predicate runs only at the transport's final send boundary,
         ;; after authentication.  A send attempt can have an uncertain outcome.
         (hermes-dashboard-transport-dispatch-guard
          (lambda () (when (funcall current) (setf (plist-get operation :sent) t))))
         (hermes-dashboard-transport-request-owner operation)
         (hermes-dashboard-transport-request-timeout 10))
    (when (funcall current)
      (push operation (plist-get owner :controls))
      (hermes--promise-catch
       (hermes--promise-then
        (apply #'hermes-dashboard-transport-call-fn
               (pcase action
                 ('steer #'hermes-dashboard-transport-subagent-steer)
                 ('interrupt #'hermes-dashboard-transport-subagent-interrupt)
                 ('kill #'hermes-dashboard-transport-process-kill))
               (plist-get owner :client) (plist-get operation :id)
               (append (and text (list text))
                       (list :session-id (plist-get operation :session))))
        (lambda (result)
          (if (not (hermes-work--control-current-p operation))
              (hermes-work--control-retire operation)
            (hermes-work--control-finish operation (hermes-work--receipt action result)))))
       (lambda (reason)
         (if (not (hermes-work--control-current-p operation))
             (hermes-work--control-retire operation)
           (hermes-work--control-finish
            operation
            (if (and (eq action 'kill) (stringp reason) (> (length reason) 0)
                     (eql (get-text-property 0 'hermes-rpc-code reason) 4044))
                "Hermes: process not owned by this chat"
              (format "Hermes: %s" reason)))))))))

(defun hermes-work--short (text)
  "Return TEXT shortened for a prompt."
  (hermes-work--fit (car (split-string (or text "") "\n")) 40))

(defun hermes-work-interrupt ()
  "Interrupt the worker at point, or kill the process at point, after confirming."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (let ((owner hermes-work--owner) (key (hermes-work--key-at-point)))
    (if (eq (car-safe key) 'process)
        (let ((row (or (hermes-work--target owner key)
                       (user-error "No current running process; refresh or reopen from its chat"))))
          (when (yes-or-no-p (format "Kill process `%s'? "
                                     (hermes-work--short (plist-get row :command))))
            (hermes-work--control owner row 'kill)))
      (let ((row (hermes-work--selected-worker)))
        (when (yes-or-no-p (format "Interrupt worker %s in this session? "
                                   (plist-get row :id)))
          (hermes-work--control owner row 'interrupt))))))

(defun hermes-work-steer ()
  "Queue steering text for the selected worker, without claiming delivery."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (let ((owner hermes-work--owner) (row (hermes-work--selected-worker)))
    (unless (plist-get row :accepting-steer)
      (user-error "Worker is not accepting steering"))
    (let ((text (read-string (format "Steer “%s”: " (hermes-work--short (plist-get row :goal))))))
      (unless (string-empty-p (string-trim text))
        (hermes-work--control owner row 'steer text)))))

(defun hermes-work-tail ()
  "Display the selected worker's bounded live tail, without inferring a path.
Resumed lineage may be visible but unavailable to this control generation."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (let ((owner hermes-work--owner) (row (hermes-work--selected-worker)))
    (hermes-work-log--open owner (plist-get row :id) nil row)))

;;; Worker logs

(require 'hermes-kanban-log)

(defconst hermes-work-log--max-bytes (* 2 1024 1024)
  "Maximum decoded worker log size accepted for rendering.")

(defvar-local hermes-work-log--binding nil
  "Exact owner, worker and remote path for this log buffer.")
(defvar-local hermes-work-log--request nil
  "Unique pending request token, or nil.")

(defun hermes-work--event-log-path (event id)
  "Return ID's exact log path from a structured delegate EVENT.
Reject malformed parallel arrays and ambiguous worker identities."
  (when (and (equal (plist-get event :name) "delegate_task")
             (equal (plist-get event :event) "tool.complete")
             (not (plist-get event :subagent-id)))
    (let* ((raw (or (plist-get event :result) (plist-get event :result-text)))
           (result (if (stringp raw) (cdr (hermes-transport--json-read raw)) raw))
           (ids (hermes-transport--get result 'subagent_ids))
           (paths (hermes-transport--get result 'live_transcripts)))
      (when (and (or (vectorp ids) (proper-list-p ids))
                 (or (vectorp paths) (proper-list-p paths))
                 (= (length ids) (length paths))
                 (= (seq-count (lambda (value) (equal value id)) ids) 1))
        (let ((path (elt paths (seq-position ids id #'equal))))
          (and (stringp path) (not (string-empty-p path)) path))))))

(defun hermes-work--log-path (owner id)
  "Return the unambiguous remote log path for ID in OWNER's chat entries.
Only structured parent tool results supply paths, never text or filenames."
  (when (hermes-work--current-p owner)
    (with-current-buffer (plist-get owner :buffer)
      (let ((paths (delete-dups
                    (delq nil
                          (mapcar
                           (lambda (entry)
                             (hermes-work--event-log-path
                              (plist-get (plist-get entry :metadata) :event) id))
                           (hermes-chat--entries))))))
        (and (= (length paths) 1) (car paths))))))

(defun hermes-work-log--owner-current-p (binding)
  "Return non-nil if BINDING retains its attachment and optional tail row."
  (let ((owner (plist-get binding :owner)) (row (plist-get binding :row)))
    (and (not (plist-get binding :retired)) (hermes-work--current-p owner)
         (or (plist-get binding :path)
             ;; A published log path stays readable while the owner is attached.
             (and row
                  (hermes-work--row-current-p owner row))))))

(defun hermes-work-log--retire (buffer binding)
  "Settle BUFFER's exact BINDING without repainting a retired view."
  (setf (plist-get binding :retired) t)
  (hermes-dashboard-transport-cancel-owner-requests
   (plist-get (plist-get binding :owner) :client) binding)
  (when (and (buffer-live-p buffer)
             (eq binding (buffer-local-value 'hermes-work-log--binding buffer)))
    (with-current-buffer buffer
      (setq hermes-work-log--request nil)
      (when (hermes-buffer--owned-p 'hermes-work-log-mode)
        (setq header-line-format
              "Worker log · Retired · reopen from Work in the attached chat")))))

(defun hermes-work-log--settle-owner (owner)
  "Retire OWNER's obsolete log bindings, independently of visible windows."
  (dolist (buffer (buffer-list))
    (when-let* ((binding (buffer-local-value 'hermes-work-log--binding buffer))
                ((eq owner (plist-get binding :owner)))
                ((not (hermes-work-log--owner-current-p binding))))
      (hermes-work-log--retire buffer binding))))

(defun hermes-work-log--detach ()
  "Release this log's exact read on native buffer retirement."
  (when hermes-work-log--binding
    (hermes-work-log--retire (current-buffer) hermes-work-log--binding)))

(defun hermes-work-log--fetch (buffer binding token)
  "Fetch BUFFER's BINDING under exact request TOKEN through authentication."
  (let* ((owner (plist-get binding :owner))
         (client (plist-get owner :client))
         (guard (lambda () (hermes-work-log--current-p buffer binding token)))
         (hermes-dashboard-transport-dispatch-guard guard)
         (hermes-dashboard-transport-request-owner binding)
         (hermes-dashboard-transport-request-timeout 10))
    (if-let* ((path (plist-get binding :path)))
        (hermes-dashboard-transport-api-request-async
         "GET" "/api/files/read" :client client :query (list (cons 'path path))
         :timeout 30 :current-p guard)
      (hermes-dashboard-transport-call-fn
       #'hermes-dashboard-transport-subagent-tail client (plist-get binding :id)
       :session-id (plist-get owner :runtime)))))

(defun hermes-work-log--accept-tail (buffer binding token result)
  "Render bounded tail RESULT for BUFFER's exact BINDING and TOKEN."
  (when (hermes-work-log--current-p buffer binding token)
    (let ((available (eq (hermes-transport--get result 'available) t))
          (truncated (eq (hermes-transport--get result 'truncated) t))
          (text (hermes-transport--get result 'text)))
      (unless (and (equal (hermes-transport--get result 'subagent_id)
                          (plist-get binding :id)) (stringp text))
        (error "Invalid worker tail"))
      (with-current-buffer buffer
        (hermes-work-log--render
         (if available (if (string-empty-p text) "Empty tail snapshot.\n" text)
           "Tail unavailable: no log or no control authority in this session.\nUse the Work view's Published log action for historical logs.\n"))
        (setq hermes-work-log--request nil
              header-line-format
              (format "Worker tail · available: %s · truncated: %s · g Refresh"
                      (if available "yes" "no") (if truncated "yes" "no")))))))

(defun hermes-work-log--decode (result)
  "Return log text from managed-file RESULT, or signal invalid data.
The endpoint returns whole files, not a tail or a paginated transcript."
  (decode-coding-string
   (hermes-transport-file-bytes result hermes-work-log--max-bytes) 'utf-8))

(defun hermes-work-log--current-p (buffer binding token)
  "Return non-nil if BUFFER still owns BINDING and request TOKEN."
  (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
       (not (with-current-buffer buffer (hermes-buffer--retired-p)))
       (eq binding (buffer-local-value 'hermes-work-log--binding buffer))
       (eq token (buffer-local-value 'hermes-work-log--request buffer))
       (hermes-work-log--owner-current-p binding)))

(defun hermes-work-log--render (text)
  "Replace this log with rendered TEXT, preserving point and windows."
  (hermes-browser--preserve-reading-position
   (lambda ()
     (let ((inhibit-read-only t)
           (inhibit-modification-hooks t))
       (erase-buffer)
       (insert text)))))

(defun hermes-work-log--accept (buffer binding token result)
  "Render RESULT while BUFFER retains BINDING and request TOKEN."
  (when (hermes-work-log--current-p buffer binding token)
    (let ((text (hermes-kanban--render-log-content
                 (hermes-work-log--decode result))))
      ;; Rendering invokes mode hooks in temporary buffers.  Revalidate.
      (when (hermes-work-log--current-p buffer binding token)
        (with-current-buffer buffer
          (hermes-work-log--render
           (if (string-empty-p text) "No log content yet.\n" text))
          (setq hermes-work-log--request nil
                header-line-format
                (if (string-empty-p text) "Worker log · Empty snapshot · ? Help"
                  "Worker log · Snapshot; entries may be truncated · ? Help")))))))

(defun hermes-work-log-refresh ()
  "Fetch this worker's remote log or bounded tail, preserving point.
Keep the last snapshot on failure.  Only one request may be pending per view.
A changed roster row requires reopening a tail from the Work view."
  (interactive nil hermes-work-log-mode)
  (let* ((buffer (current-buffer))
         (binding hermes-work-log--binding))
    (unless (and (derived-mode-p 'hermes-work-log-mode)
                 (not (hermes-buffer--retired-p))
                 (hermes-work-log--owner-current-p binding))
      (user-error "Worker log owner detached; reopen from the attached chat"))
    (when hermes-work-log--request (user-error "Worker log refresh already pending"))
    (let ((token (list 'request)))
      (setq hermes-work-log--request token
            header-line-format "Worker log · Loading · previous snapshot retained")
      (hermes--promise-catch
       (hermes--promise-then
        (condition-case err
            (hermes-work-log--fetch buffer binding token)
          (error (hermes--promise-rejected (error-message-string err))))
        (lambda (result)
          (unless (hermes-work-log--owner-current-p binding)
            (hermes-work-log--retire buffer binding))
          (if (plist-get binding :path)
              (hermes-work-log--accept buffer binding token result)
            (hermes-work-log--accept-tail buffer binding token result))))
       (lambda (reason)
         (unless (hermes-work-log--owner-current-p binding)
           (hermes-work-log--retire buffer binding))
         (when (hermes-work-log--current-p buffer binding token)
           (with-current-buffer buffer
             (let ((reason (hermes-dashboard-transport--redact-secret
                            (format "%s" reason))))
               (setq hermes-work-log--request nil
                     header-line-format
                     (propertize "Worker log · Failed · g Retry (hover for reason)"
                                 'help-echo reason))
               (when (= (buffer-size) 0)
                 (hermes-work-log--render (concat "Worker log unavailable: " reason "\n\ng Retry\n")))
               (message "Hermes worker log: %s" reason)))))))))

(defun hermes-work-log--open (owner id path &optional row)
  "Display OWNER's worker ID using exact remote PATH, or live tail for ROW."
  (let ((existing
         (seq-find
          (lambda (buffer)
            (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                 (not (with-current-buffer buffer (hermes-buffer--retired-p)))
                 (let ((binding (buffer-local-value 'hermes-work-log--binding buffer)))
                   (and (eq owner (plist-get binding :owner))
                        (equal id (plist-get binding :id))
                        (equal path (plist-get binding :path))
                        (eq row (plist-get binding :row))))))
          (buffer-list))))
    (if existing (pop-to-buffer existing)
      (let ((buffer (generate-new-buffer (format "*Hermes Worker Log: %s*" id))))
        (with-current-buffer buffer
          (hermes-work-log-mode)
          (when (and (eq (current-buffer) buffer)
                     (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                     (not (hermes-buffer--retired-p))
                     (hermes-work--current-p owner))
            (hermes-buffer--claim 'hermes-work-log-mode)
            (setf (plist-get owner :settle) #'hermes-work--settle-owner)
            (setq hermes-work-log--binding
                  (list :owner owner :id id :path path :row row :retired nil)
                  header-line-format "Worker log · Not fetched")
            (hermes-work-log-refresh)))
        (when (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                   (not (with-current-buffer buffer (hermes-buffer--retired-p)))
                   (eq owner (plist-get (buffer-local-value 'hermes-work-log--binding buffer)
                                       :owner))
                   (hermes-work--current-p owner))
          (pop-to-buffer buffer))))))

(defun hermes-work-log ()
  "Open the selected worker's read-only remote log, or process details.
Use only the exact path published in this parent's structured tool result."
  (interactive nil hermes-work-mode hermes-work-detail-mode)
  (let* ((owner hermes-work--owner)
         (row (hermes-work--find (hermes-work--key-at-point)
                                 (hermes-work--observations owner))))
    (unless row (user-error "No observed work on this line"))
    (unless (hermes-work--current-p owner) (user-error "Work owner detached"))
    (if (not (eq (plist-get row :kind) 'delegate))
        (hermes-work-open)
      (let* ((id (plist-get row :id))
             (path (hermes-work--log-path owner id)))
        (unless path
          (user-error "No authoritative worker log path; RET shows observed details"))
        (hermes-work-log--open owner id path)))))

(defvar-keymap hermes-work-log-mode-map
  :parent special-mode-map
  "g" #'hermes-work-log-refresh
  "n" #'hermes-kanban-log-next-hunk
  "p" #'hermes-kanban-log-previous-hunk)

(keymap-popup-annotate hermes-work-log-mode-map
  :popup-key "?" :exit-key "C-g" :description "Worker Log Snapshot"
  :group "Diff"
  hermes-kanban-log-next-hunk "Next hunk"
  hermes-kanban-log-previous-hunk "Previous hunk"
  :group "View"
  hermes-work-log-refresh "Refresh"
  quit-window "Quit view")

(put 'hermes-work-log-mode-map-popup 'command-modes '(hermes-work-log-mode))

(define-derived-mode hermes-work-log-mode special-mode "Worker Log"
  "Read a remote worker log or live tail without visiting a local file.
The backend may truncate individual entries or expire logs.  No full-session
history guarantee is implied.  Published-path reads time out after 30 seconds;
files over 2 MiB are not rendered, although the API transfers the whole file.
Live tails time out after 10 seconds and the backend bounds them to 16 KiB."
  (setq-local truncate-lines nil)
  (add-hook 'after-set-visited-file-name-hook #'hermes-buffer--retire nil t)
  (add-hook 'after-set-visited-file-name-hook #'hermes-work-log--detach t t)
  (add-hook 'kill-buffer-hook #'hermes-work-log--detach nil t)
  (add-hook 'change-major-mode-hook #'hermes-work-log--detach nil t)
  (visual-line-mode 1)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-log-refresh))))

(defun hermes-work--popup-title ()
  "Return the popup title naming the row at point."
  (let ((row (and hermes-work--owner
                  (hermes-work--find (hermes-work--key-at-point)
                                     (ignore-errors (hermes-work--observations hermes-work--owner))))))
    (if (not row) "Chat Workers"
      (let ((state (hermes-work--state row)))
        (concat (propertize (nth 0 state) 'face (nth 1 state)) " "
                (hermes-work--short (or (plist-get row :goal) (plist-get row :command)))
                " " (propertize (nth 2 state) 'face (nth 1 state)))))))

(defun hermes-work--process-at-point-p ()
  "Return non-nil when the row at point is a process."
  (eq (car-safe (hermes-work--key-at-point)) 'process))

(defun hermes-work--worker-at-point-p ()
  "Return non-nil when the row at point is a worker."
  (eq (car-safe (hermes-work--key-at-point)) 'delegate))

(defun hermes-work--stop-label ()
  "Return the label of `k' for the row at point."
  (if (hermes-work--process-at-point-p) "Kill process" "Interrupt"))

(defvar-keymap hermes-work-mode-map
  :parent special-mode-map
  "n" #'hermes-work-next
  "p" #'hermes-work-previous
  "f" #'hermes-work-forward-sibling
  "b" #'hermes-work-backward-sibling
  "^" #'hermes-work-up
  "TAB" #'hermes-work-toggle
  "<backtab>" #'hermes-work-toggle-all
  "RET" #'hermes-work-open
  "d" #'hermes-work-open
  "L" #'hermes-work-log
  "g" #'hermes-work-refresh
  "k" #'hermes-work-interrupt
  "s" #'hermes-work-steer
  "t" #'hermes-work-tail
  "q" #'quit-window
  "i" #'hermes-work-instance-subagents
  "h" #'hermes-work-scope-details)

(keymap-popup-annotate hermes-work-mode-map
  :popup-key "?" :exit-key "C-g" :description #'hermes-work--popup-title
  :group "Act"
  hermes-work-steer ("Steer" :if #'hermes-work--worker-at-point-p)
  hermes-work-interrupt #'hermes-work--stop-label
  hermes-work-tail ("Live tail" :if #'hermes-work--worker-at-point-p)
  :group "Read"
  hermes-work-open "Open details"
  hermes-work-log ("Published log" :if #'hermes-work--worker-at-point-p)
  hermes-work-scope-details "Scope"
  hermes-work-instance-subagents "Instance workers"
  :row
  :group "Move"
  hermes-work-up "Parent"
  hermes-work-forward-sibling "Next sibling"
  hermes-work-backward-sibling "Previous sibling"
  :group "View"
  hermes-work-toggle "Card / fold"
  hermes-work-toggle-all "All cards"
  hermes-work-refresh "Refresh"
  quit-window "Quit view")

(put 'hermes-work-mode-map-popup 'command-modes '(hermes-work-mode))

(define-derived-mode hermes-work-mode special-mode "Workers"
  "Browse one chat attachment's workers and processes as an outline.
\\<hermes-work-mode-map>\\[hermes-work-toggle] folds a section or opens a row's card, \\[hermes-work-open] opens
details, and \\[hermes-work-interrupt] interrupts a worker or kills a process."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-refresh)))
  (add-hook 'window-size-change-functions #'hermes-work--resize nil t)
  (add-hook 'after-set-visited-file-name-hook #'hermes-work--detach t t)
  (add-hook 'kill-buffer-hook #'hermes-work--detach nil t)
  (add-hook 'change-major-mode-hook #'hermes-work--detach nil t))

(defvar-keymap hermes-work-detail-mode-map
  :parent special-mode-map
  "g" #'hermes-work-refresh
  "k" #'hermes-work-interrupt
  "s" #'hermes-work-steer
  "t" #'hermes-work-tail
  "L" #'hermes-work-log
  "h" #'hermes-work-scope-details)

(keymap-popup-annotate hermes-work-detail-mode-map
  :popup-key "?" :exit-key "C-g" :description #'hermes-work--popup-title
  :group "Act"
  hermes-work-steer ("Steer" :if #'hermes-work--worker-at-point-p)
  hermes-work-interrupt #'hermes-work--stop-label
  hermes-work-tail ("Live tail" :if #'hermes-work--worker-at-point-p)
  :group "Read"
  hermes-work-log ("Published log" :if #'hermes-work--worker-at-point-p)
  hermes-work-scope-details "Scope"
  hermes-work-refresh "Refresh"
  quit-window "Quit view")

(put 'hermes-work-detail-mode-map-popup 'command-modes '(hermes-work-detail-mode))

(define-derived-mode hermes-work-detail-mode special-mode "Worker"
  "Read one observed worker or process; the roster refreshes it live."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-refresh))))

;;;###autoload
(defun hermes-chat-work ()
  "Browse this chat's observed work without acquiring another client."
  (interactive nil hermes-chat-mode hermes-work-mode)
  (let ((owner (hermes-work--context-owner)))
    (unless (and (hermes-work--current-p owner)
                 (or (derived-mode-p 'hermes-chat-mode) (hermes-work--view-p owner)))
      (user-error "No attached session work owner"))
    (unless (hermes-work--view-p owner)
      (let ((buffer (generate-new-buffer "*Hermes Workers*")))
        (with-current-buffer buffer
          (hermes-work-mode)
          (hermes-buffer--claim 'hermes-work-mode)
          (setq hermes-work--owner owner)
          (setq-local hermes-instance (plist-get owner :instance)))
        (setf (plist-get owner :view) buffer
              (plist-get owner :view-valid-p) #'hermes-work--view-p
              (plist-get owner :render) #'hermes-work--render
              (plist-get owner :settle) #'hermes-work--settle-owner)))
    (with-current-buffer (plist-get owner :buffer)
      (add-hook 'hermes-chat-state-change-hook #'hermes-work--changed nil t))
    (pop-to-buffer (plist-get owner :view))
    (hermes-work--render owner)))

(provide 'hermes-subagents)
;;; hermes-subagents.el ends here
