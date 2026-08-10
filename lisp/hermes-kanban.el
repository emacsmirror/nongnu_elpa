;;; hermes-kanban.el --- Kanban board browser for Hermes  -*- lexical-binding: t; -*-

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

;; Two `tabulated-list' browsers over the dashboard kanban HTTP plugin
;; (`/api/plugins/kanban/...'), mirroring how the web dashboard renders the
;; board.  Everything goes through the dashboard at
;; `hermes-dashboard-transport-url' -- never the local `hermes' CLI, which
;; reads local SQLite and ignores the configured remote.
;;
;; `hermes-list-kanban' opens the boards overview: one row per board with a
;; per-status count summary.  RET drills into a board's tasks, where RET shows
;; a task and the popup exposes editing, triage, recovery, and view actions.

;;; Code:

(require 'tabulated-list)
(require 'json)
(require 'outline)
(require 'subr-x)
(require 'url-util)
(require 'keymap-popup)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-browser)
(require 'hermes-kanban-log)
(require 'hermes-kanban-events)
(require 'cl-lib)

(declare-function markdown-mode "markdown-mode")
(declare-function read-string-from-buffer "string-edit")
(declare-function hermes-kanban-task-mode "hermes-kanban")

;;; HTTP against the dashboard kanban plugin

(defconst hermes-kanban-log-tail-bytes 100000
  "Number of worker-log bytes fetched for `hermes-kanban-show-log'.")

(defcustom hermes-kanban-triage-action-timeout 300
  "Seconds before a triage specify or decompose request gives up."
  :type 'number
  :group 'hermes)

(defun hermes-kanban--api (method path &optional body query timeout)
  "Return a promise of the kanban plugin response for METHOD PATH.
BODY, QUERY, and TIMEOUT extend the request.  Authentication and a single retry
on a failed GET come from the shared dashboard transport, which talks only to
`hermes-dashboard-transport-url'."
  (hermes-dashboard-transport-api-request-async
   method (concat "/api/plugins/kanban" path)
   :body body :query query :timeout timeout))

(defun hermes-kanban--then (promise on-ok)
  "Run ON-OK on PROMISE's resolved value, reporting any rejection."
  (hermes--promise-then
   promise on-ok
   (lambda (reason) (message "Hermes: %s" reason))))

(defvar hermes-kanban--board-request-id 0)
(defvar hermes-kanban--boards-request-id 0)
(defvar hermes-kanban--diagnostics-request-id 0)
(defvar hermes-kanban--task-request-id 0)
(defvar hermes-kanban--log-request-id 0)

(defun hermes-kanban--begin-request (slot)
  "Advance request identity SLOT and return its new value."
  (let ((request-id (1+ (symbol-value slot))))
    (set slot request-id)
    request-id))

(defun hermes-kanban--request-current-p (slot request-id)
  "Return non-nil when REQUEST-ID is the latest value of SLOT."
  (= request-id (symbol-value slot)))

;;; Field helpers

(defun hermes-kanban--count (counts status)
  "Return COUNTS' tally for STATUS as a string."
  (let ((n (hermes-transport--get counts (intern status))))
    (if (numberp n) (number-to-string n) "0")))

(defun hermes-kanban--format-time (value)
  "Return VALUE (a Unix timestamp) formatted, or an empty string."
  (if (numberp value)
      (format-time-string "%F %R" value)
    ""))

;;; Shared status display helpers

(defface hermes-kanban-triage-face
  '((t :inherit warning))
  "Face for triage tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-todo-face
  '((t :inherit font-lock-variable-name-face))
  "Face for todo tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-scheduled-face
  '((t :inherit font-lock-constant-face))
  "Face for scheduled tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-ready-face
  '((t :inherit font-lock-type-face))
  "Face for ready tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-running-face
  '((t :inherit font-lock-keyword-face))
  "Face for running tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-blocked-face
  '((t :inherit error))
  "Face for blocked tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-review-face
  '((t :inherit font-lock-function-name-face))
  "Face for review tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-done-face
  '((t :inherit success))
  "Face for done tasks and board counts."
  :group 'hermes)

(defface hermes-kanban-archived-face
  '((t :inherit shadow))
  "Face for archived tasks and board counts."
  :group 'hermes)

(defconst hermes-kanban--current-board-marker "📍"
  "Marker used for the current Hermes Kanban board.")

(defconst hermes-kanban--protected-board-slugs '("default")
  "Board slugs protected from archive/delete by the Hermes backend.")

(defconst hermes-kanban--status-display
  '(("triage" :icon "💡" :label "triage" :face hermes-kanban-triage-face)
    ("todo" :icon "📝" :label "todo" :face hermes-kanban-todo-face)
    ("scheduled" :icon "⏰" :label "scheduled"
     :face hermes-kanban-scheduled-face)
    ("ready" :icon "✅" :label "ready" :face hermes-kanban-ready-face)
    ("running" :icon "⚙️" :label "running" :face hermes-kanban-running-face)
    ("blocked" :icon "⛔" :label "blocked" :face hermes-kanban-blocked-face)
    ("review" :icon "👀" :label "review" :face hermes-kanban-review-face)
    ("done" :icon "🏁" :label "done" :face hermes-kanban-done-face)
    ("archived" :icon "🗄️" :label "archived"
     :face hermes-kanban-archived-face))
  "User-facing display metadata for Kanban task statuses.
Each entry maps a status string to :icon, :label, and optional :face.")

(defconst hermes-kanban--board-count-statuses
  '("triage" "todo" "scheduled" "ready" "running" "blocked" "review"
    "done" "archived")
  "Statuses displayed as count columns in the boards overview.")

(defconst hermes-kanban--task-title-column-max-width 64
  "Maximum width used for task titles in `hermes-kanban-mode'.")

(defun hermes-kanban--status-info (status)
  "Return display metadata for STATUS, or nil when STATUS is unknown."
  (alist-get (hermes-transport--scalar-string status)
             hermes-kanban--status-display nil nil #'equal))

(defun hermes-kanban--status-icon (status)
  "Return the shared icon for STATUS, or an empty string."
  (or (plist-get (hermes-kanban--status-info status) :icon) ""))

(defun hermes-kanban--status-label (status)
  "Return the shared label for STATUS, falling back to STATUS itself."
  (let ((raw (or (hermes-transport--scalar-string status) "")))
    (or (plist-get (hermes-kanban--status-info raw) :label) raw)))

(defun hermes-kanban--format-status (status)
  "Return STATUS as a user-facing icon plus label.
The returned string carries the raw status as the `hermes-kanban-status'
text property, so commands can keep using backend status values."
  (let* ((raw (or (hermes-transport--scalar-string status) ""))
         (info (hermes-kanban--status-info raw))
         (icon (plist-get info :icon))
         (label (or (plist-get info :label) raw))
         (face (or (plist-get info :face) 'hermes-browser-status))
         (text (copy-sequence
                (if (and icon (not (string-empty-p icon)))
                    (format "%s %s" icon label)
                  label))))
    (when (and face (not (string-empty-p text)))
      (setq text (propertize text 'face face)))
    (when (not (string-empty-p text))
      (add-text-properties 0 (length text) `(hermes-kanban-status ,raw) text))
    text))

(defun hermes-kanban-format-status (status)
  "Return STATUS with the standard Kanban icon and semantic face."
  (hermes-kanban--format-status status))

(defun hermes-kanban--format-status-indicator (status)
  "Return STATUS as a compact task-table indicator.
Known statuses use only their icon.  Unknown statuses fall back to their
raw label.  The returned string carries the raw status as the
`hermes-kanban-status' text property, so commands can keep using backend
status values."
  (let* ((raw (or (hermes-transport--scalar-string status) ""))
         (info (hermes-kanban--status-info raw))
         (icon (plist-get info :icon))
         (face (or (plist-get info :face) 'hermes-browser-status))
         (text (copy-sequence
                (or (hermes-transport--non-empty-string icon) raw))))
    (when (and face (not (string-empty-p text)))
      (setq text (propertize text 'face face)))
    (when (not (string-empty-p text))
      (add-text-properties 0 (length text) `(hermes-kanban-status ,raw) text))
    text))

(defun hermes-kanban--format-status-count (counts status)
  "Return COUNTS' tally for STATUS."
  (hermes-browser--face-cell
   (hermes-kanban--count counts status)
   (or (plist-get (hermes-kanban--status-info status) :face)
       'hermes-browser-count)))

(defun hermes-kanban--status-column-heading (status)
  "Return the boards-overview heading for STATUS."
  (or (hermes-transport--non-empty-string (hermes-kanban--status-icon status))
      (capitalize (hermes-kanban--status-label status))))

(defun hermes-kanban--display-status-value (display)
  "Return the backend status represented by DISPLAY."
  (let* ((plain (substring-no-properties display))
         (parts (split-string plain " " t)))
    (if parts (car (last parts)) plain)))

(defun hermes-kanban--entry-status (entry)
  "Return ENTRY's raw task status from its display cell."
  (let ((status (and (vectorp entry)
                     (> (length entry) 0)
                     (aref entry 0))))
    (or (and (stringp status)
             (> (length status) 0)
             (get-text-property 0 'hermes-kanban-status status))
        (and (stringp status) (hermes-kanban--display-status-value status))
        "")))

(defun hermes-kanban--boards-tabulated-list-format (&optional width)
  "Return the dynamic boards `tabulated-list-format' for WIDTH."
  (let* ((widths (hermes-browser--allocate-column-widths
                  width
                  (append '((2 0) (12 7) (4 1))
                          (mapcar (lambda (_) '(4 1))
                                  hermes-kanban--board-count-statuses))))
         (fixed-columns (vector (list "" (nth 0 widths) t)
                                (list "📋" (nth 1 widths) t)
                                (list "Σ" (nth 2 widths) t))))
    (vconcat fixed-columns
             (cl-mapcar (lambda (status column-width)
                          (list (hermes-kanban--status-column-heading status)
                                column-width t))
                        hermes-kanban--board-count-statuses
                        (nthcdr 3 widths)))))

(defun hermes-kanban--tasks-tabulated-list-format (&optional width)
  "Return the dynamic task `tabulated-list-format' for WIDTH."
  (let* ((widths (hermes-browser--allocate-column-widths
                  width '((6 0) (4 0) (10 2) (20 6))))
         (title-width (min (nth 3 widths)
                           hermes-kanban--task-title-column-max-width)))
    `[("Status" ,(nth 0 widths) t) ("Pri" ,(nth 1 widths) t)
      ("Assignee" ,(nth 2 widths) t) ("Title" ,title-width t)]))

;;; Boards overview buffer

(defun hermes-kanban--board-rows (boards)
  "Return `tabulated-list' entries for BOARDS, the dashboard board list."
  (mapcar
   (lambda (board)
     (let ((slug (hermes-transport--display-field board 'slug))
           (counts (hermes-transport--get board 'counts))
           (total (hermes-transport--get board 'total)))
       (list (cons slug (hermes-transport--non-empty-string (hermes-transport--display-field board 'name)))
             (vconcat
              (vector (hermes-browser--face-cell
                       (if (eq (hermes-transport--get board 'is_current) t)
                           hermes-kanban--current-board-marker
                         "")
                       'hermes-browser-default)
                      (hermes-browser--face-cell
                       (or (hermes-transport--non-empty-string
                            (hermes-transport--display-field board 'name))
                           slug)
                       'hermes-browser-name)
                      (hermes-browser--face-cell
                       (if (numberp total) total 0) 'hermes-browser-total))
              (mapcar (lambda (status)
                        (hermes-kanban--format-status-count counts status))
                      hermes-kanban--board-count-statuses)))))
   boards))

(defvar hermes-kanban-boards-mode-map)

(keymap-popup-define hermes-kanban-boards-mode-map
  "Keymap for `hermes-kanban-boards-mode'."
  :parent tabulated-list-mode-map
  :description "Hermes Kanban Boards"
  :group "Navigate"
  "RET" ("Open board" hermes-kanban-open-board)
  :group "Board"
  "+" ("New board" hermes-kanban-create-board)
  "s" ("Switch current board" hermes-kanban-switch-board)
  "r" ("Rename board" hermes-kanban-rename-board)
  "D" ("Archive non-default board" hermes-kanban-archive-board)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-kanban-boards-mode-map-popup))

(defun hermes-kanban--init-boards-header (&optional width)
  "Refresh the boards buffer `tabulated-list' header for WIDTH."
  (setq tabulated-list-format (hermes-kanban--boards-tabulated-list-format width))
  (tabulated-list-init-header))

(defun hermes-kanban--window-size-change (window)
  "Refresh Kanban tabulated-list columns for resized WINDOW."
  (when (derived-mode-p 'hermes-kanban-boards-mode 'hermes-kanban-mode)
    (let ((old-format tabulated-list-format)
          (width (window-body-width window)))
      (if (derived-mode-p 'hermes-kanban-boards-mode)
          (hermes-kanban--init-boards-header width)
        (hermes-kanban--init-board-header width))
      (unless (equal old-format tabulated-list-format)
        (tabulated-list-print t)))))

(define-derived-mode hermes-kanban-boards-mode tabulated-list-mode "Hermes Boards"
  "Major mode for the Hermes Kanban boards overview."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-kanban--boards-revert)
  (add-hook 'window-size-change-functions
            #'hermes-kanban--window-size-change nil t)
  (hermes-kanban--init-boards-header))

(defun hermes-kanban--render-boards (&optional in-place)
  "Fetch and render the dashboard boards overview asynchronously.
With IN-PLACE non-nil, refresh without re-displaying the buffer (used by revert,
which already runs in the displayed window)."
  (let ((request-id (hermes-kanban--begin-request
                     'hermes-kanban--boards-request-id)))
    (hermes--promise-then
     (hermes-kanban--api "GET" "/boards")
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--boards-request-id request-id)
         (let ((boards (hermes-transport--get payload 'boards)))
           (with-current-buffer (get-buffer-create "*Hermes Kanban Boards*")
             (unless (derived-mode-p 'hermes-kanban-boards-mode)
               (hermes-kanban-boards-mode))
             (setq tabulated-list-entries (hermes-kanban--board-rows boards))
             ;; Display first so column widths use the live window.
             (unless in-place (pop-to-buffer (current-buffer)))
             (hermes-kanban--init-boards-header
              (hermes-browser--visible-window-width))
             (tabulated-list-print t)))))
     (lambda (reason)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--boards-request-id request-id)
         (message "Hermes: %s" reason))))))

(defun hermes-kanban--boards-revert (&rest _)
  "Refresh the boards overview in place."
  (hermes-kanban--render-boards t))

(defun hermes-kanban--board-at-point ()
  "Return the selected board as (SLUG . NAME), or signal a `user-error'."
  (or (tabulated-list-get-id) (user-error "No board on this line")))

(defun hermes-kanban--board-path (slug &rest segments)
  "Return the kanban boards path for SLUG extended by SEGMENTS."
  (concat "/boards/" (url-hexify-string slug) (apply #'concat segments)))

(defun hermes-kanban--protected-board-p (slug)
  "Return non-nil when SLUG names a backend-protected board."
  (member slug hermes-kanban--protected-board-slugs))

(defun hermes-kanban--current-board-row-p ()
  "Return non-nil when the selected board row is marked current."
  (and-let* ((entry (tabulated-list-get-entry)))
    (equal (aref entry 0) hermes-kanban--current-board-marker)))

(defun hermes-kanban-open-board ()
  "Open the board at point in the detail buffer."
  (interactive)
  (let ((id (hermes-kanban--board-at-point)))
    (hermes-kanban--render-board (car id) (cdr id))))

(defun hermes-kanban-create-board ()
  "Create a new board from the boards overview."
  (interactive)
  (let ((slug (read-string "New board slug: "))
        (name (read-string "Display name: ")))
    (when (string-empty-p slug)
      (user-error "Board slug is required"))
    (hermes-kanban--then
     (hermes-kanban--api "POST" "/boards"
                         `((slug . ,slug)
                           (name . ,(if (string-empty-p name) slug name))
                           (switch . :false)))
     (lambda (_) (hermes-kanban--render-boards)))))

(defun hermes-kanban-switch-board ()
  "Make the selected board the current Hermes Kanban board."
  (interactive)
  (let* ((board (hermes-kanban--board-at-point))
         (slug (car board)))
    (hermes-kanban--then
     (hermes-kanban--api "POST" (hermes-kanban--board-path slug "/switch"))
     (lambda (_)
       (hermes-kanban--render-boards)
       (message "Current Hermes Kanban board: %s" slug)))))

(defun hermes-kanban-rename-board (name)
  "Rename the selected board's display NAME."
  (interactive
   (let* ((board (hermes-kanban--board-at-point))
          (current-name (or (cdr board) (car board))))
     (list (read-string "New board display name: " current-name))))
  (let* ((board (hermes-kanban--board-at-point))
         (slug (car board))
         (trimmed (string-trim name)))
    (when (string-empty-p trimmed)
      (user-error "Board name cannot be empty"))
    (hermes-kanban--then
     (hermes-kanban--api "PATCH" (hermes-kanban--board-path slug)
                         `((name . ,trimmed)))
     (lambda (_)
       (hermes-kanban--render-boards)
       (message "Renamed board %s to %s" slug trimmed)))))

(defun hermes-kanban-archive-board ()
  "Archive the selected board after confirmation.
This uses the dashboard's recoverable archive endpoint and never hard-deletes."
  (interactive)
  (let* ((board (hermes-kanban--board-at-point))
         (slug (car board))
         (name (or (cdr board) slug))
         (current-p (hermes-kanban--current-board-row-p)))
    (when (hermes-kanban--protected-board-p slug)
      (user-error "Board %s is protected and cannot be archived" slug))
    (when (and current-p
               (not (yes-or-no-p
                     (format "Archive current board %s and fall back to default?"
                             slug))))
      (user-error "Archive cancelled"))
    (when (yes-or-no-p
           (format "Archive board %s (%s) recoverably, without hard delete?"
                   slug name))
      (hermes-kanban--then
       (hermes-kanban--api "DELETE" (hermes-kanban--board-path slug))
       (lambda (_)
         (hermes-kanban--render-boards)
         (message "Archived board %s" slug))))))

;;; Board detail buffer

(defvar-local hermes-kanban--slug nil
  "Slug of the board shown in this detail buffer.")

(defvar-local hermes-kanban--name nil
  "Display name of the board shown in this detail buffer.")

(defvar-local hermes-kanban--assignees nil
  "Known assignees on the current board, for completion.")

(defvar-local hermes-kanban--latest-event-id nil
  "Most recent task-event id from the last board render, for live seeding.")

(defvar-local hermes-kanban--orchestration-mode 'unknown
  "Current triage orchestration mode: `auto', `manual', or `unknown'.")

(defun hermes-kanban--triage-mode-indicator ()
  "Return the current triage orchestration mode for the mode line."
  (pcase hermes-kanban--orchestration-mode
    ('auto (propertize " [Triage: auto]" 'face 'success))
    ('manual (propertize " [Triage: manual]" 'face 'warning))
    (_ (propertize " [Triage: …]" 'face 'shadow))))

(defun hermes-kanban--task-created-desc-p (left right)
  "Return non-nil when LEFT is newer (created_at) than RIGHT.
LEFT and RIGHT are task plists/alists as produced by the dashboard.
A missing or non-numeric `created_at' sorts oldest (treated as 0), so
tasks without a reliable timestamp never jump above dated ones.  This is
a strict comparator for `sort'; ties keep their input order, since
`sort' is stable."
  (let ((a (hermes-transport--get left 'created_at))
        (b (hermes-transport--get right 'created_at)))
    (> (if (numberp a) a 0)
       (if (numberp b) b 0))))

(defun hermes-kanban--task-rows (columns)
  "Flatten dashboard COLUMNS into `tabulated-list' entries, newest first.
Tasks are sorted by `created_at' descending across all status columns, so
the board detail buffer shows the most recently created tasks at the top."
  (let* ((tasks (mapcan (lambda (column)
                          (append (hermes-transport--get column 'tasks) nil))
                        columns))
         (sorted (sort tasks #'hermes-kanban--task-created-desc-p)))
    (mapcar
     (lambda (task)
       (list (hermes-transport--display-field task 'id)
             (vector (hermes-kanban--format-status-indicator
                      (hermes-transport--display-field task 'status))
                     (hermes-browser--face-cell
                      (hermes-transport--display-field task 'priority)
                      'hermes-browser-priority)
                     (hermes-browser--face-cell
                      (or (hermes-transport--non-empty-string
                           (hermes-transport--display-field task 'assignee))
                          "-")
                      'hermes-browser-assignee)
                     (hermes-browser--face-cell
                      (hermes-transport--display-field task 'title)
                      'hermes-browser-title))))
     sorted)))

(defvar hermes-kanban-mode-map)

(keymap-popup-define hermes-kanban-mode-map
  "Keymap for `hermes-kanban-mode'."
  :parent tabulated-list-mode-map
  :description "Hermes Kanban Board"
  :group "Navigate"
  "RET" ("Show task" hermes-kanban-show)
  "b" ("Back to boards" hermes-kanban-boards)
  :group "Edit task"
  "e" ("Edit title/priority" hermes-kanban-edit)
  "a" ("Assign / reassign" hermes-kanban-change-assignee)
  "s" ("Set status" hermes-kanban-set-status)
  "c" ("Comment" hermes-kanban-comment)
  :group "Triage"
  "i" ("New rough idea" hermes-kanban-create-triage-task)
  "S" ("Specify as one task" hermes-kanban-specify-triage-task)
  "x" ("Decompose now" hermes-kanban-decompose-triage-task)
  :group "Board"
  "+" ("New task" hermes-kanban-create-task)
  "D" ("Delete task" hermes-kanban-delete)
  "N" ("Nudge dispatcher" hermes-kanban-nudge-dispatch)
  :group "Recovery"
  "R" ("Reclaim task" hermes-kanban-reclaim)
  "K" ("Terminate run" hermes-kanban-terminate-run)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "t" ("Toggle live updates" hermes-kanban-toggle-live)
  "l" ("View selected task log" hermes-kanban-show-log)
  "d" ("Diagnostics overview" hermes-kanban-diagnostics)
  "?" ("Help" hermes-kanban-mode-map-popup))

(defun hermes-kanban--init-board-header (&optional width)
  "Refresh the board detail `tabulated-list' header for WIDTH."
  (setq tabulated-list-format (hermes-kanban--tasks-tabulated-list-format width))
  (tabulated-list-init-header))

(define-derived-mode hermes-kanban-mode tabulated-list-mode "Hermes Kanban"
  "Major mode for browsing a single Hermes Kanban board."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-kanban--revert)
  (add-hook 'window-size-change-functions
            #'hermes-kanban--window-size-change nil t)
  (hermes-kanban--init-board-header))

(defun hermes-kanban--display-board (payload slug name in-place)
  "Display board PAYLOAD for SLUG and NAME, optionally IN-PLACE."
  (let ((assignees
         (delq nil (mapcar #'hermes-transport--scalar-string
                           (hermes-transport--get payload 'assignees)))))
    (with-current-buffer (get-buffer-create "*Hermes Kanban*")
      (unless (derived-mode-p 'hermes-kanban-mode)
        (hermes-kanban-mode))
      (setq hermes-kanban--slug slug
            hermes-kanban--name name
            hermes-kanban--assignees assignees
            hermes-kanban--latest-event-id
            (hermes-transport--get payload 'latest_event_id)
            mode-line-process
            (list (format " [%s]" (or name slug "board"))
                  '(:eval (hermes-kanban--triage-mode-indicator))
                  '(:eval (hermes-kanban--live-indicator)))
            tabulated-list-sort-key nil
            tabulated-list-entries
            (hermes-kanban--task-rows
             (hermes-transport--get payload 'columns)))
      (hermes-kanban--events-retarget slug hermes-kanban--latest-event-id)
      ;; Display first so header sizing can use the live window width.
      (unless in-place (pop-to-buffer (current-buffer)))
      (hermes-kanban--init-board-header
       (hermes-browser--visible-window-width))
      (tabulated-list-print t))))

(defun hermes-kanban--refresh-orchestration-mode (request-id slug)
  "Refresh orchestration mode for REQUEST-ID and board SLUG."
  (hermes--promise-then
   (hermes-kanban--api "GET" "/orchestration")
   (lambda (settings)
     (when-let* ((buffer (get-buffer "*Hermes Kanban*")))
       (with-current-buffer buffer
         (when (and (derived-mode-p 'hermes-kanban-mode)
                    (equal hermes-kanban--slug slug)
                    (hermes-kanban--request-current-p
                     'hermes-kanban--board-request-id request-id))
           (setq hermes-kanban--orchestration-mode
                 (if (hermes-kanban--truthy-p
                      (hermes-transport--get settings 'auto_decompose))
                     'auto
                   'manual))
           (force-mode-line-update)))))
   (lambda (_) nil)))

(defun hermes-kanban--goto-task-row (task-id)
  "Move point to TASK-ID in the current Kanban board."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'tabulated-list-id task-id #'equal)))
    (goto-char (prop-match-beginning match))
    (beginning-of-line)))

(defun hermes-kanban--render-board (slug name &optional in-place task-id)
  "Fetch and render board SLUG (display NAME) in the detail buffer.
With IN-PLACE non-nil, refresh without re-displaying the buffer (used by revert,
which already runs in the displayed window).  When TASK-ID is non-nil, select
that task after rendering."
  (let ((request-id (hermes-kanban--begin-request
                     'hermes-kanban--board-request-id)))
    (hermes-kanban--then
     (hermes-kanban--api "GET" "/board" nil (and slug `((board . ,slug))))
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--board-request-id request-id)
         (hermes-kanban--display-board payload slug name in-place)
         (hermes-kanban--refresh-orchestration-mode request-id slug)
         (when task-id
           (with-current-buffer "*Hermes Kanban*"
             (hermes-kanban--goto-task-row task-id))))))))

(defun hermes-kanban-open-board-task (board-slug task-id)
  "Open BOARD-SLUG, select TASK-ID, and return the request promise."
  (hermes-kanban--render-board board-slug board-slug nil task-id))

(defun hermes-kanban--revert (&rest _)
  "Refresh the current board detail buffer in place."
  (hermes-kanban--render-board hermes-kanban--slug hermes-kanban--name t))

(defun hermes-kanban-boards ()
  "Return to the boards overview."
  (interactive)
  (hermes-kanban--render-boards))

(defun hermes-kanban--board-query ()
  "Return a board query alist for the current detail buffer, or nil."
  (and hermes-kanban--slug `((board . ,hermes-kanban--slug))))

(defun hermes-kanban--id-at-point ()
  "Return the task id on the current line or signal a `user-error'."
  (or (tabulated-list-get-id) (user-error "No task on this line")))

(defun hermes-kanban--task-path (id &rest segments)
  "Return the kanban tasks path for ID extended by SEGMENTS."
  (concat "/tasks/" (url-hexify-string id) (apply #'concat segments)))

;;; Task detail and log views

(defvar-local hermes-kanban-task--task-id nil
  "Task id displayed in the current task detail buffer.")

(defvar-local hermes-kanban-task--board-slug nil
  "Board slug displayed in the current task detail buffer.")

(defvar-local hermes-kanban-task--status nil
  "Raw task status displayed in the current task detail buffer.")

(defvar-local hermes-kanban-task--assignees nil
  "Assignee names known when the current task detail buffer was opened.")

(defvar hermes-kanban-task-detail-functions nil
  "Functions run after rendering a Kanban task detail.
Each function receives the task payload and board slug.  The current buffer is
the writable task detail buffer.")

(defvar-local hermes-kanban-log--task-id nil
  "Task id displayed in the current worker-log buffer.")

(defvar-local hermes-kanban-log--board-slug nil
  "Board slug displayed in the current worker-log buffer.")

(defun hermes-kanban--items (value)
  "Return VALUE as a list of response items."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)))

(defun hermes-kanban--truthy-p (value)
  "Return non-nil when VALUE is a JSON true-ish value."
  (and value (not (memq value '(false :false :json-false)))))

(defun hermes-kanban--object-string (object)
  "Return OBJECT as a compact display string."
  (when object
    (string-trim
     (condition-case nil
         (json-serialize object)
       (error (pp-to-string object))))))

(defun hermes-kanban--fontify-markdown-string (text)
  "Return TEXT fontified with `markdown-mode', or TEXT on failure."
  (condition-case nil
      (if (not (require 'markdown-mode nil t))
          text
        (with-temp-buffer
          (insert text)
          (delay-mode-hooks (markdown-mode))
          (font-lock-mode 1)
          (font-lock-ensure (point-min) (point-max))
          (remove-text-properties (point-min) (point-max) '(invisible nil))
          (buffer-string)))
    (error text)))

(defun hermes-kanban--outline-level ()
  "Return the Markdown heading level for `outline-minor-mode'."
  (length (match-string 1)))

(defun hermes-kanban--format-size (bytes)
  "Return BYTES as a small human-readable size string."
  (cond
   ((not (numberp bytes)) "")
   ((< bytes 1024) (format "%d B" bytes))
   ((< bytes (* 1024 1024)) (format "%.1f KiB" (/ bytes 1024.0)))
   (t (format "%.1f MiB" (/ bytes 1048576.0)))))

(defun hermes-kanban--format-section (title items empty-name formatter)
  "Return a Markdown section TITLE for ITEMS using FORMATTER.
EMPTY-NAME is inserted in the explicit empty-state line."
  (let ((rows (hermes-kanban--items items)))
    (concat "\n## " title " (" (number-to-string (length rows)) ")\n\n"
            (if rows
                (string-join (mapcar formatter rows) "\n\n")
              (format "— no %s —" empty-name))
            "\n")))

(defun hermes-kanban--format-failure-fields (task)
  "Return TASK's branch, run, and failure metadata as header lines.
Healthy tasks add nothing: a zero failure count and an empty error are dropped,
and an absent branch or run id is omitted."
  (let* ((branch (hermes-transport--non-empty-string
                  (hermes-transport--display-field task 'branch_name)))
         (run (hermes-transport--get task 'current_run_id))
         (failures (hermes-transport--get task 'consecutive_failures))
         (error (hermes-transport--non-empty-string
                 (hermes-transport--display-field task 'last_failure_error)))
         (lines (delq nil
                      (list (and branch (format "- Branch: `%s`" branch))
                            (and (numberp run) (format "- Run: `#%s`" run))
                            (and (numberp failures) (> failures 0)
                                 (format "- Failures: %d" failures))
                            (and error (format "- Last error: %s" error))))))
    (if lines (concat (string-join lines "\n") "\n") "")))

(defun hermes-kanban--format-task (task)
  "Return TASK's header and body as a display string."
  (let ((latest-summary (hermes-transport--non-empty-string
                         (hermes-transport--display-field task 'latest_summary)))
        (body (hermes-transport--non-empty-string (hermes-transport--display-field task 'body))))
    (concat
     (format "# %s\n\n- ID: `%s`\n- Status: `%s`\n- Priority: `%s`\n- Assignee: `%s`\n- Created: %s\n"
             (hermes-transport--display-field task 'title)
             (hermes-transport--display-field task 'id)
             (hermes-kanban--format-status (hermes-transport--display-field task 'status))
             (hermes-transport--display-field task 'priority)
             (or (hermes-transport--non-empty-string (hermes-transport--display-field task 'assignee)) "-")
             (hermes-kanban--format-time (hermes-transport--get task 'created_at)))
     (when-let* ((workspace (hermes-transport--non-empty-string
                             (hermes-transport--display-field task 'workspace_kind))))
       (format "- Workspace: %s%s\n" workspace
               (if-let* ((path (hermes-transport--non-empty-string
                                (hermes-transport--display-field task 'workspace_path))))
                   (concat ": " path)
                 "")))
     (when latest-summary
       (format "- Summary: %s\n" latest-summary))
     (hermes-kanban--format-failure-fields task)
     "\n## Description\n\n"
     (or body "— no description —")
     "\n")))

(defun hermes-kanban--format-comment-row (comment)
  "Return COMMENT as one Markdown row."
  (format "### %s — %s\n\n%s"
          (hermes-kanban--format-time (hermes-transport--get comment 'created_at))
          (or (hermes-transport--non-empty-string (hermes-transport--display-field comment 'author))
              "anon")
          (hermes-transport--display-field comment 'body)))

(defun hermes-kanban--format-event-row (event)
  "Return EVENT as one Markdown row."
  (let ((payload (hermes-transport--get event 'payload)))
    (concat
     (format "### %s — %s"
             (hermes-kanban--format-time (hermes-transport--get event 'created_at))
             (hermes-transport--display-field event 'kind))
     (and payload
          (format "\n\n- Payload: %s" (hermes-kanban--object-string payload))))))

(defun hermes-kanban--format-attachment (attachment)
  "Return ATTACHMENT as one Markdown row."
  (let ((size (hermes-kanban--format-size
               (hermes-transport--get attachment 'size)))
        (content-type (hermes-transport--non-empty-string
                       (hermes-transport--display-field attachment 'content_type)))
        (uploaded-by (hermes-transport--non-empty-string
                      (hermes-transport--display-field attachment 'uploaded_by)))
        (path (hermes-transport--non-empty-string
               (hermes-transport--display-field attachment 'stored_path))))
    (concat
     (format "### %s (#%s)%s"
             (hermes-transport--display-field attachment 'filename)
             (hermes-transport--display-field attachment 'id)
             (if (string-empty-p size) "" (format " (%s)" size)))
     (and content-type (format "\n\n- Type: %s" content-type))
     (and uploaded-by (format "\n- Uploaded by: %s" uploaded-by))
     (and path (format "\n- Path: %s" path)))))

(defun hermes-kanban--format-diagnostic-action (action)
  "Return ACTION as a short diagnostic action label."
  (let ((label (or (hermes-transport--non-empty-string (hermes-transport--display-field action 'label))
                   (hermes-transport--display-field action 'kind))))
    (if (hermes-kanban--truthy-p (hermes-transport--get action 'suggested))
        (concat label " (suggested)")
      label)))

(defun hermes-kanban--format-diagnostic (diagnostic)
  "Return DIAGNOSTIC as one Markdown row."
  (let ((actions (hermes-kanban--items
                  (hermes-transport--get diagnostic 'actions)))
        (data (hermes-transport--get diagnostic 'data)))
    (concat
     (format "### [%s] %s: %s"
             (hermes-transport--display-field diagnostic 'severity)
             (hermes-transport--display-field diagnostic 'kind)
             (hermes-transport--display-field diagnostic 'title))
     (when-let* ((detail (hermes-transport--non-empty-string
                          (hermes-transport--display-field diagnostic 'detail))))
       (format "\n\n%s" detail))
     (when (or (hermes-transport--get diagnostic 'run_id)
               (hermes-transport--get diagnostic 'count))
       (format "\n\n- Run: %s\n- Count: %s"
               (or (hermes-transport--display-field diagnostic 'run_id) "-")
               (or (hermes-transport--display-field diagnostic 'count) "-")))
     (when data
       (format "\n- Data: %s" (hermes-kanban--object-string data)))
     (when actions
       (format "\n- Actions: %s"
               (string-join (mapcar #'hermes-kanban--format-diagnostic-action
                                    actions)
                            ", "))))))

(defun hermes-kanban--format-run (run)
  "Return RUN as one Markdown row."
  (let* ((outcome (hermes-transport--non-empty-string
                   (hermes-transport--display-field run 'outcome)))
         (status (hermes-transport--non-empty-string
                  (hermes-transport--display-field run 'status)))
         (state (or outcome status "-"))
         (profile (or (hermes-transport--non-empty-string
                       (hermes-transport--display-field run 'profile))
                      "-"))
         (started (hermes-transport--get run 'started_at))
         (ended (hermes-transport--get run 'ended_at))
         (metadata (hermes-transport--get run 'metadata)))
    (concat
     (format "### Run #%s — %s @%s"
             (hermes-transport--display-field run 'id) state profile)
     (when (and (numberp started) (numberp ended))
       (format " (%ss)" (max 0 (- ended started))))
     (when (numberp started)
       (format "\n\n- Started: %s" (hermes-kanban--format-time started)))
     (when (numberp ended)
       (format "\n- Ended: %s" (hermes-kanban--format-time ended)))
     (when-let* ((pid (hermes-transport--non-empty-string
                       (hermes-transport--display-field run 'worker_pid))))
       (format "\n- PID: %s" pid))
     (when-let* ((summary (hermes-transport--non-empty-string
                           (hermes-transport--display-field run 'summary))))
       (format "\n- Summary: %s" summary))
     (when-let* ((error-text (hermes-transport--non-empty-string
                              (hermes-transport--display-field run 'error))))
       (format "\n- Error: %s" error-text))
     (when metadata
       (format "\n- Metadata: %s" (hermes-kanban--object-string metadata))))))

(defun hermes-kanban--format-task-detail (payload)
  "Return rich task detail text for PAYLOAD from GET /tasks/:id."
  (let* ((task (hermes-transport--get payload 'task))
         (diagnostics (or (hermes-transport--get task 'diagnostics)
                          (hermes-transport--get payload 'diagnostics))))
    (concat
     (hermes-kanban--format-task task)
     (hermes-kanban--format-section
      "Diagnostics" diagnostics "diagnostics"
      #'hermes-kanban--format-diagnostic)
     (hermes-kanban--format-section
      "Attachments" (hermes-transport--get payload 'attachments) "attachments"
      #'hermes-kanban--format-attachment)
     (hermes-kanban--format-section
      "Comments" (hermes-transport--get payload 'comments) "comments"
      #'hermes-kanban--format-comment-row)
     (hermes-kanban--format-section
      "Events" (hermes-transport--get payload 'events) "events"
      #'hermes-kanban--format-event-row)
     (hermes-kanban--format-section
      "Run history" (hermes-transport--get payload 'runs) "runs"
      #'hermes-kanban--format-run))))

(defvar hermes-kanban-task-mode-map)

(keymap-popup-define hermes-kanban-task-mode-map
  "Keymap for `hermes-kanban-task-mode'."
  :parent special-mode-map
  :description "Hermes Kanban Task"
  :group "Task"
  "c" ("Comment" hermes-kanban-comment)
  "a" ("Change assignee" hermes-kanban-change-assignee)
  :group "Triage"
  "S" ("Specify as one task" hermes-kanban-specify-triage-task)
  "x" ("Decompose now" hermes-kanban-decompose-triage-task)
  :group "Recovery"
  "R" ("Reclaim task" hermes-kanban-reclaim)
  "K" ("Terminate run" hermes-kanban-terminate-run)
  "N" ("Nudge dispatcher" hermes-kanban-nudge-dispatch)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "l" ("View worker log" hermes-kanban-show-log)
  "d" ("Diagnostics overview" hermes-kanban-diagnostics)
  "?" ("Help" hermes-kanban-task-mode-map-popup))

(defun hermes-kanban--task-mode-setup ()
  "Set up buffer-local state for `hermes-kanban-task-mode'."
  (setq-local revert-buffer-function #'hermes-kanban--task-revert)
  (setq-local outline-regexp "^\\(#+\\) ")
  (setq-local outline-level #'hermes-kanban--outline-level)
  (outline-minor-mode 1)
  (read-only-mode 1))

(if (require 'markdown-mode nil t)
    (define-derived-mode hermes-kanban-task-mode markdown-mode "Hermes Task"
      "Major mode for a Hermes Kanban task detail buffer."
      :interactive nil
      (hermes-kanban--task-mode-setup))
  (define-derived-mode hermes-kanban-task-mode special-mode "Hermes Task"
    "Major mode for a Hermes Kanban task detail buffer."
    :interactive nil
    (hermes-kanban--task-mode-setup)))

(defun hermes-kanban--query-for-board (slug)
  "Return a board query alist for SLUG, or nil."
  (and slug `((board . ,slug))))

(defun hermes-kanban--task-revert (&rest _)
  "Refresh the current task detail buffer in place."
  (unless hermes-kanban-task--task-id
    (user-error "No task id for this detail buffer"))
  (let ((task-id hermes-kanban-task--task-id)
        (board-slug hermes-kanban-task--board-slug)
        (assignees hermes-kanban-task--assignees)
        (request-id (hermes-kanban--begin-request
                     'hermes-kanban--task-request-id)))
    (hermes-kanban--then
     (hermes-kanban--api "GET" (hermes-kanban--task-path task-id)
                         nil (hermes-kanban--query-for-board board-slug))
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--task-request-id request-id)
         (hermes-kanban--display-task payload board-slug t assignees))))))

(defun hermes-kanban--display-task (payload &optional board-slug in-place assignees)
  "Render task PAYLOAD in a read-only detail buffer.
BOARD-SLUG is remembered for refreshes and log requests.  With IN-PLACE non-nil,
refresh without re-displaying the buffer (used by revert).  ASSIGNEES carries
the board-known assignee names for cold profile-cache completion fallback."
  (hermes-kanban--begin-request 'hermes-kanban--task-request-id)
  (let* ((task (hermes-transport--get payload 'task))
         (task-id (hermes-transport--display-field task 'id))
         (task-status (hermes-transport--display-field task 'status)))
    (with-current-buffer (get-buffer-create "*Hermes Kanban Task*")
      (unless (derived-mode-p 'hermes-kanban-task-mode)
        (hermes-kanban-task-mode))
      (setq hermes-kanban-task--task-id task-id
            hermes-kanban-task--board-slug board-slug
            hermes-kanban-task--status task-status
            hermes-kanban-task--assignees assignees
            mode-line-process (format " [%s]" (or task-id "task")))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (hermes-kanban--fontify-markdown-string
                 (hermes-kanban--format-task-detail payload)))
        (run-hook-with-args
         'hermes-kanban-task-detail-functions payload board-slug)
        (read-only-mode 1))
      (goto-char (point-min))
      (unless in-place (pop-to-buffer (current-buffer))))))

(defun hermes-kanban--open-task (task-id board-slug assignees)
  "Display TASK-ID from BOARD-SLUG with ASSIGNEES and return its promise."
  (let ((request-id (hermes-kanban--begin-request
                     'hermes-kanban--task-request-id)))
    (hermes-kanban--then
     (hermes-kanban--api "GET" (hermes-kanban--task-path task-id)
                         nil (hermes-kanban--query-for-board board-slug))
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--task-request-id request-id)
         (hermes-kanban--display-task payload board-slug nil assignees))))))

(defun hermes-kanban-open-task (task-id &optional board-slug)
  "Display TASK-ID from optional BOARD-SLUG and return its promise."
  (hermes-kanban--open-task task-id board-slug nil))

(defun hermes-kanban-show ()
  "Show the kanban task at point."
  (interactive)
  (hermes-kanban--open-task
   (hermes-kanban--id-at-point)
   hermes-kanban--slug
   hermes-kanban--assignees))

(defun hermes-kanban--task-id-for-command ()
  "Return the current task id for a board or task-detail command."
  (if (derived-mode-p 'hermes-kanban-task-mode)
      (or hermes-kanban-task--task-id
          (user-error "No task for this detail buffer"))
    (hermes-kanban--id-at-point)))

(defun hermes-kanban--board-slug-for-command ()
  "Return the current board slug for a board or task-detail command."
  (if (derived-mode-p 'hermes-kanban-task-mode)
      hermes-kanban-task--board-slug
    hermes-kanban--slug))

(defun hermes-kanban--task-status-for-command ()
  "Return the current task status for a board or task-detail command."
  (if (derived-mode-p 'hermes-kanban-task-mode)
      (or hermes-kanban-task--status "")
    (hermes-kanban--entry-status (tabulated-list-get-entry))))

(defun hermes-kanban--assignees-for-command ()
  "Return board-known assignees for the current board or task detail buffer."
  (if (derived-mode-p 'hermes-kanban-task-mode)
      hermes-kanban-task--assignees
    hermes-kanban--assignees))

(defun hermes-kanban--log-query (board-slug)
  "Return the query alist for fetching a task log on BOARD-SLUG."
  (append (hermes-kanban--query-for-board board-slug)
          `((tail . ,hermes-kanban-log-tail-bytes))))

(defun hermes-kanban--fetch-log (id board-slug)
  "Return a promise of the worker log payload for task ID on BOARD-SLUG.
A request failure resolves to an error payload so the log buffer shows it
instead of surfacing a transport error."
  (hermes--promise-catch
   (hermes-kanban--api "GET" (hermes-kanban--task-path id "/log")
                       nil (hermes-kanban--log-query board-slug))
   (lambda (reason) `((task_id . ,id) (error . ,reason)))))

(defun hermes-kanban--format-log (payload)
  "Return worker-log text for PAYLOAD from GET /tasks/:id/log."
  (let ((task-id (hermes-transport--display-field payload 'task_id))
        (path (hermes-transport--display-field payload 'path))
        (size (hermes-transport--get payload 'size_bytes))
        (content (hermes-transport--display-field payload 'content))
        (error (hermes-transport--non-empty-string (hermes-transport--display-field payload 'error))))
    (concat
     (format "Worker log for %s\n" (or (hermes-transport--non-empty-string task-id) "task"))
     (unless (string-empty-p path)
       (format "Path: %s\n" path))
     (when (numberp size)
       (format "Size: %s\n" (hermes-kanban--format-size size)))
     "\n"
     (cond
      (error (format "failed to load worker log: %s\n" error))
      ((not (hermes-kanban--truthy-p (hermes-transport--get payload 'exists)))
       "— no worker log yet (task has not spawned or the log was rotated away) —\n")
      ((string-empty-p content) "(empty)\n")
      (t (hermes-kanban--render-log-content content)))
     (when (hermes-kanban--truthy-p (hermes-transport--get payload 'truncated))
       (format "\n\n(showing last %s; full log path above)\n"
               (hermes-kanban--format-size hermes-kanban-log-tail-bytes))))))

(defvar hermes-kanban-log-mode-map)

(keymap-popup-define hermes-kanban-log-mode-map
  "Keymap for `hermes-kanban-log-mode'."
  :parent special-mode-map
  :description "Hermes Kanban Log"
  :group "Diff"
  "n" ("Next hunk" hermes-kanban-log-next-hunk)
  "p" ("Previous hunk" hermes-kanban-log-previous-hunk)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-kanban-log-mode-map-popup))

(defun hermes-kanban--log-revert (&rest _)
  "Refresh the current worker-log buffer in place."
  (unless hermes-kanban-log--task-id
    (user-error "No task id for this log buffer"))
  (let ((id hermes-kanban-log--task-id)
        (board-slug hermes-kanban-log--board-slug)
        (request-id (hermes-kanban--begin-request
                     'hermes-kanban--log-request-id)))
    (hermes-kanban--then
     (hermes-kanban--fetch-log id board-slug)
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--log-request-id request-id)
         (hermes-kanban--display-log payload board-slug t))))))

(define-derived-mode hermes-kanban-log-mode special-mode "Hermes Log"
  "Major mode for a Hermes Kanban worker log buffer."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-kanban--log-revert)
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defun hermes-kanban--display-log (payload &optional board-slug in-place)
  "Render worker log PAYLOAD for BOARD-SLUG in a read-only buffer.
With IN-PLACE non-nil, refresh without re-displaying (used by revert)."
  (hermes-kanban--begin-request 'hermes-kanban--log-request-id)
  (let ((task-id (hermes-transport--non-empty-string
                  (hermes-transport--display-field payload 'task_id))))
    (with-current-buffer (get-buffer-create "*Hermes Kanban Log*")
      (unless (derived-mode-p 'hermes-kanban-log-mode)
        (hermes-kanban-log-mode))
      (setq hermes-kanban-log--task-id task-id
            hermes-kanban-log--board-slug board-slug
            mode-line-process (format " [%s]" (or (hermes-transport--non-empty-string task-id)
                                                  "task")))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (hermes-kanban--format-log payload)))
      (goto-char (point-min))
      (unless in-place (pop-to-buffer (current-buffer))))))

(defun hermes-kanban-show-log ()
  "Fetch and display the worker log for the task at point or current detail."
  (interactive)
  (let ((id (hermes-kanban--task-id-for-command))
        (board-slug (hermes-kanban--board-slug-for-command))
        (request-id (hermes-kanban--begin-request
                     'hermes-kanban--log-request-id)))
    (hermes-kanban--then
     (hermes-kanban--fetch-log id board-slug)
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--log-request-id request-id)
         (hermes-kanban--display-log payload board-slug))))))

;;; Task mutations

(defun hermes-kanban-edit ()
  "Edit the title and priority of the task at point."
  (interactive)
  (let* ((id (hermes-kanban--id-at-point))
         (entry (tabulated-list-get-entry))
         (title (read-string "Title: " (aref entry 3)))
         (priority (read-number "Priority: " (string-to-number (aref entry 1))))
         (slug hermes-kanban--slug)
         (name hermes-kanban--name))
    (when (string-empty-p (string-trim title))
      (user-error "Title cannot be empty"))
    (hermes-kanban--then
     (hermes-kanban--api "PATCH" (hermes-kanban--task-path id)
                         `((title . ,title) (priority . ,priority))
                         (hermes-kanban--board-query))
     (lambda (_) (hermes-kanban--render-board slug name)))))

(defconst hermes-kanban--statuses
  '("triage" "todo" "scheduled" "ready" "running" "blocked" "review"
    "done" "archived")
  "Statuses settable through the dashboard PATCH endpoint.")

(defun hermes-kanban--profile-name (profile)
  "Return PROFILE's non-empty name string, or nil."
  (and-let* ((name (hermes-transport--scalar-string
                    (hermes-transport--get profile 'name)))
             (trimmed (string-trim name))
             ((not (string-empty-p trimmed))))
    trimmed))

(defun hermes-kanban--profile-default-p (profile)
  "Return non-nil when PROFILE denotes the dashboard default profile."
  (or (eq (hermes-transport--get profile 'is_default) t)
      (equal (hermes-kanban--profile-name profile) "default")))

(defun hermes-kanban--profile-less-p (left right)
  "Return non-nil when LEFT should sort before RIGHT in the assignee picker."
  (let ((left-default (hermes-kanban--profile-default-p left))
        (right-default (hermes-kanban--profile-default-p right)))
    (cond
     ((and left-default (not right-default)) t)
     ((and right-default (not left-default)) nil)
     (t (string-lessp (downcase (or (hermes-kanban--profile-name left) ""))
                      (downcase (or (hermes-kanban--profile-name right) "")))))))

(defun hermes-kanban--profile-candidates ()
  "Return assignee completion candidates for the current buffer.
Use the warmed dashboard `/api/profiles' cache from
`hermes-dashboard-transport-cached-profile-list' and merge it with the
buffer-local `hermes-kanban--assignees' known to the current board, so
completion stays useful when the profile cache is cold.  The default profile
sorts first; the rest are case-insensitive.  Returns a list of name strings."
  (let* ((cached (hermes-transport--get
                  (or (hermes-dashboard-transport-cached-profile-list) '())
                  'profiles))
         (from-cache (delq nil (mapcar #'hermes-kanban--profile-name cached)))
         (from-board (delq nil (mapcar #'hermes-kanban--profile-name
                                       (mapcar (lambda (name) `((name . ,name)))
                                               (hermes-kanban--assignees-for-command)))))
         (profiles (mapcar (lambda (name) `((name . ,name)))
                           (cl-remove-duplicates (append from-cache from-board)
                                                 :test #'equal))))
    (mapcar #'hermes-kanban--profile-name
            (sort profiles #'hermes-kanban--profile-less-p))))

(defun hermes-kanban-change-assignee ()
  "Change the assignee of the current task.
Reads the new assignee with completion over current Hermes profiles (and
board-known assignees when the profile cache is cold); empty input unassigns.
Works from `hermes-kanban-task-mode' and `hermes-kanban-mode'.  Running tasks
use the dashboard reassign endpoint with reclaim; other tasks use
`PATCH /tasks/:id' with the assignee body.  Refreshes the buffer in place on
success."
  (interactive)
  (let* ((id (hermes-kanban--task-id-for-command))
         (status (hermes-kanban--task-status-for-command))
         (query (hermes-kanban--query-for-board
                 (hermes-kanban--board-slug-for-command)))
         (who (completing-read "Assignee (empty to unassign): "
                               (hermes-kanban--profile-candidates) nil nil))
         (refresh (hermes-kanban--context-refresher)))
    (hermes-kanban--then
     (if (equal status "running")
         (hermes-kanban--api "POST" (hermes-kanban--task-path id "/reassign")
                             `((profile . ,who) (reclaim_first . t)) query)
       (hermes-kanban--api "PATCH" (hermes-kanban--task-path id)
                           `((assignee . ,who)) query))
     (lambda (_)
       (message "Assignee for %s set to %s"
                id (if (string-empty-p who) "-" who))
       (funcall refresh)))))

(defun hermes-kanban-set-status ()
  "Set the status of the task at point."
  (interactive)
  (let* ((id (hermes-kanban--id-at-point))
         (status (completing-read "Status: " hermes-kanban--statuses nil t))
         (slug hermes-kanban--slug)
         (name hermes-kanban--name))
    (hermes-kanban--then
     (hermes-kanban--api "PATCH" (hermes-kanban--task-path id)
                         `((status . ,status)) (hermes-kanban--board-query))
     (lambda (_) (hermes-kanban--render-board slug name)))))

(defun hermes-kanban-comment ()
  "Append a comment to the current task, then refresh the buffer.
Works from the board list and the task detail view; maps to the dashboard
`POST /tasks/:id/comments'.  The refresh surfaces the new comment in the detail
view."
  (interactive)
  (let ((id (hermes-kanban--task-id-for-command))
        (query (hermes-kanban--query-for-board
                (hermes-kanban--board-slug-for-command)))
        (refresh (hermes-kanban--context-refresher))
        (body (read-string-from-buffer "Comment: " "")))
    (when (string-empty-p (string-trim body))
      (user-error "Comment cannot be empty"))
    (hermes-kanban--then
     (hermes-kanban--api "POST" (hermes-kanban--task-path id "/comments")
                         `((body . ,body)) query)
     (lambda (_) (message "Comment added to task %s" id) (funcall refresh)))))

(defun hermes-kanban--create-task-body
    (title description priority assignee triage)
  "Return a task body from TITLE, DESCRIPTION, PRIORITY, ASSIGNEE, and TRIAGE."
  (append `((title . ,title) (priority . ,priority))
          (and (not (string-empty-p (string-trim description)))
               `((body . ,description)))
          (and (not (string-empty-p assignee)) `((assignee . ,assignee)))
          (and triage '((triage . t)))))

(defun hermes-kanban--created-task-summary (result triage orchestration-mode)
  "Return a creation summary for RESULT, TRIAGE, and ORCHESTRATION-MODE."
  (let ((id (or (hermes-transport--non-empty-string
                 (hermes-transport--display-field
                  (hermes-transport--get result 'task) 'id))
                "task")))
    (cond
     ((not triage) (format "Created task %s" id))
     ((eq orchestration-mode 'auto)
      (format "Created triage task %s; queued for automatic decomposition" id))
     ((eq orchestration-mode 'manual)
      (format "Created triage task %s; use x to decompose" id))
     (t (format "Created triage task %s" id)))))

(defun hermes-kanban--create-task (triage)
  "Create a task on the current board.
When TRIAGE is non-nil, create it in the triage column."
  (let ((title (read-string (if triage "Rough idea title: " "Title: "))))
    (when (string-empty-p (string-trim title))
      (user-error "Title is required"))
    (let ((description (read-string-from-buffer "Description: " ""))
          (assignee (if triage
                        ""
                      (completing-read "Assignee (optional): "
                                       (hermes-kanban--profile-candidates)
                                       nil nil)))
          (priority (read-number "Priority: " 0))
          (slug hermes-kanban--slug)
          (name hermes-kanban--name)
          (buffer (current-buffer))
          (board-request-id hermes-kanban--board-request-id)
          (orchestration-mode hermes-kanban--orchestration-mode))
      (hermes-kanban--then
       (hermes-kanban--api
        "POST" "/tasks"
        (hermes-kanban--create-task-body
         title description priority assignee triage)
        (hermes-kanban--board-query))
       (lambda (result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (derived-mode-p 'hermes-kanban-mode)
                        (equal hermes-kanban--slug slug)
                        (hermes-kanban--request-current-p
                         'hermes-kanban--board-request-id board-request-id))
               (hermes-kanban--render-board slug name))))
         (message "%s" (hermes-kanban--created-task-summary
                        result triage orchestration-mode)))))))

(defun hermes-kanban-create-task ()
  "Create a normal task on the current board."
  (interactive)
  (hermes-kanban--create-task nil))

(defun hermes-kanban-create-triage-task ()
  "Create a rough idea in the current board's triage column."
  (interactive)
  (hermes-kanban--create-task t))

(defun hermes-kanban--specify-summary (result)
  "Return a user-facing summary of a triage specifier RESULT."
  (if (hermes-kanban--truthy-p (hermes-transport--get result 'ok))
      (if-let* ((title (hermes-transport--non-empty-string
                        (hermes-transport--display-field result 'new_title))))
          (format "Specified task: %s" title)
        "Specified task")
    (format "Specify failed: %s"
            (or (hermes-transport--non-empty-string
                 (hermes-transport--display-field result 'reason))
                "unknown error"))))

(defun hermes-kanban--run-triage-action (endpoint summary-function)
  "POST ENDPOINT for the current triage task and call SUMMARY-FUNCTION."
  (let ((id (hermes-kanban--task-id-for-command))
        (status (hermes-kanban--task-status-for-command))
        (query (hermes-kanban--query-for-board
                (hermes-kanban--board-slug-for-command)))
        (refresh (hermes-kanban--context-refresher)))
    (unless (equal status "triage")
      (user-error "Task %s is not in triage" id))
    (hermes-kanban--then
     (hermes-kanban--api "POST" (hermes-kanban--task-path id endpoint)
                         '((author . :null)) query
                         hermes-kanban-triage-action-timeout)
     (lambda (result)
       (message "%s" (funcall summary-function result))
       (funcall refresh)))))

(defun hermes-kanban-specify-triage-task ()
  "Flesh out the current triage task and promote it to todo."
  (interactive)
  (hermes-kanban--run-triage-action "/specify"
                                    #'hermes-kanban--specify-summary))

(defun hermes-kanban--decompose-summary (result)
  "Return a user-facing summary of a triage decomposer RESULT."
  (let ((children (delq nil (mapcar #'hermes-transport--scalar-string
                                    (hermes-kanban--items
                                     (hermes-transport--get result 'child_ids))))))
    (cond
     ((not (hermes-kanban--truthy-p (hermes-transport--get result 'ok)))
      (format "Decompose failed: %s"
              (or (hermes-transport--non-empty-string
                   (hermes-transport--display-field result 'reason))
                  "unknown error")))
     (children
      (format "Decomposed task into %d children: %s"
              (length children) (string-join children ", ")))
     ((hermes-transport--non-empty-string
       (hermes-transport--display-field result 'new_title))
      (format "Kept as one task: %s"
              (hermes-transport--display-field result 'new_title)))
     (t "Kept as one task"))))

(defun hermes-kanban-decompose-triage-task ()
  "Decompose the current triage task into a dependency graph."
  (interactive)
  (hermes-kanban--run-triage-action "/decompose"
                                    #'hermes-kanban--decompose-summary))

(defun hermes-kanban-delete ()
  "Delete the task at point after confirmation."
  (interactive)
  (let ((id (hermes-kanban--id-at-point))
        (slug hermes-kanban--slug)
        (name hermes-kanban--name))
    (when (yes-or-no-p (format "Delete task %s? " id))
      (hermes-kanban--then
       (hermes-kanban--api "DELETE" (hermes-kanban--task-path id)
                           nil (hermes-kanban--board-query))
       (lambda (_) (hermes-kanban--render-board slug name))))))

;;; Diagnostics overview

(defconst hermes-kanban--diagnostics-format
  [("Sev" 10 t) ("Task" 30 t) ("Assignee" 14 t) ("Diagnostic" 50 t)]
  "Column format for the Hermes Kanban diagnostics overview.")

(defun hermes-kanban--diagnostic-summary (top count)
  "Return TOP diagnostic's title, suffixed with COUNT when more than one exists."
  (let ((title (hermes-transport--display-field top 'title)))
    (if (> count 1) (format "%s (+%d more)" title (1- count)) title)))

(defun hermes-kanban--diagnostic-row (group)
  "Return a tabulated-list entry (TASK-ID . [cells]) for one task GROUP.
The cells are the top diagnostic's severity, the task title, its assignee, and a
summary of the top diagnostic; absent fields fall back to placeholders."
  (let* ((task-id (hermes-transport--display-field group 'task_id))
         (diagnostics (hermes-kanban--items
                       (hermes-transport--get group 'diagnostics)))
         (top (car diagnostics)))
    (list task-id
          (vector
           (hermes-browser--status-cell
            (hermes-transport--display-field top 'severity)
            'hermes-browser-severity)
           (hermes-browser--face-cell
            (or (hermes-transport--non-empty-string
                 (hermes-transport--display-field group 'task_title))
                task-id)
            'hermes-browser-title)
           (hermes-browser--face-cell
            (or (hermes-transport--non-empty-string
                 (hermes-transport--display-field group 'task_assignee))
                "-")
            'hermes-browser-assignee)
           (hermes-browser--face-cell
            (hermes-kanban--diagnostic-summary top (length diagnostics))
            'hermes-browser-diagnostic)))))

(defun hermes-kanban--diagnostic-rows (groups)
  "Return tabulated-list entries for diagnostic GROUPS from GET /diagnostics."
  (mapcar #'hermes-kanban--diagnostic-row (hermes-kanban--items groups)))

(defvar hermes-kanban-diagnostics-mode-map)

(keymap-popup-define hermes-kanban-diagnostics-mode-map
  "Keymap for `hermes-kanban-diagnostics-mode'."
  :parent tabulated-list-mode-map
  :description "Hermes Kanban Diagnostics"
  :group "Navigate"
  "RET" ("Show task" hermes-kanban-show)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-kanban-diagnostics-mode-map-popup))

(define-derived-mode hermes-kanban-diagnostics-mode tabulated-list-mode
  "Hermes Diagnostics"
  "Major mode for the Hermes Kanban diagnostics overview."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-kanban--diagnostics-revert)
  (setq tabulated-list-format hermes-kanban--diagnostics-format)
  (tabulated-list-init-header))

(defun hermes-kanban--diagnostics-revert (&rest _)
  "Refresh the diagnostics overview in place."
  (hermes-kanban--render-diagnostics hermes-kanban--slug hermes-kanban--name t))

(defun hermes-kanban--render-diagnostics (slug name &optional in-place)
  "Fetch and render board SLUG's diagnostics overview asynchronously.
NAME is remembered for refreshes.  With IN-PLACE non-nil, refresh without
re-displaying the buffer (used by revert)."
  (let ((request-id (hermes-kanban--begin-request
                     'hermes-kanban--diagnostics-request-id)))
    (hermes--promise-then
     (hermes-kanban--api "GET" "/diagnostics"
                         nil (hermes-kanban--query-for-board slug))
     (lambda (payload)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--diagnostics-request-id request-id)
         (let ((groups (hermes-transport--get payload 'diagnostics)))
           (with-current-buffer (get-buffer-create "*Hermes Kanban Diagnostics*")
             (unless (derived-mode-p 'hermes-kanban-diagnostics-mode)
               (hermes-kanban-diagnostics-mode))
             (setq hermes-kanban--slug slug
                   hermes-kanban--name name
                   mode-line-process (and slug (format " [%s]" slug))
                   tabulated-list-entries (hermes-kanban--diagnostic-rows groups))
             (tabulated-list-print t)
             (unless in-place (pop-to-buffer (current-buffer)))
             (unless tabulated-list-entries
               (message "No active diagnostics on this board"))))))
     (lambda (reason)
       (when (hermes-kanban--request-current-p
              'hermes-kanban--diagnostics-request-id request-id)
         (message "Hermes: %s" reason))))))

;;;###autoload
(defun hermes-kanban-diagnostics ()
  "Show the dashboard diagnostics overview for the current board.
Lists every task with an active diagnostic, highest severity first; RET opens
the task and `g' refreshes."
  (interactive)
  (hermes-kanban--render-diagnostics
   (hermes-kanban--board-slug-for-command) hermes-kanban--name))

;;; Recovery actions

(defun hermes-kanban--run-id-for-task (task)
  "Return TASK's current run id as a number, or nil when there is no live run."
  (let ((run (hermes-transport--get task 'current_run_id)))
    (and (numberp run) run)))

(defun hermes-kanban--read-reason (prompt)
  "Read an optional reason string with PROMPT; return nil when blank."
  (let ((reason (string-trim (read-string prompt))))
    (and (not (string-empty-p reason)) reason)))

(defun hermes-kanban--reason-body (reason)
  "Return a request-body alist carrying REASON, or nil when REASON is nil."
  (and reason `((reason . ,reason))))

(defun hermes-kanban--context-refresher ()
  "Return a thunk that re-renders the current Kanban buffer in place.
The buffer is captured now so the thunk is safe to call from an async callback
whose then-current buffer may differ."
  (let ((buffer (current-buffer)))
    (lambda ()
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (revert-buffer nil t))))))

(defun hermes-kanban--dispatch-summary (result)
  "Return a one-line dispatcher summary for a `/dispatch' RESULT."
  (let* ((spawned (length (hermes-transport--get result 'spawned)))
         (auto (length (hermes-transport--get result 'auto_assigned_default)))
         (promoted (or (hermes-transport--get result 'promoted) 0))
         (reclaimed (or (hermes-transport--get result 'reclaimed) 0))
         (unassigned (length (hermes-transport--get result 'skipped_unassigned)))
         (parts (delq nil
                      (list (and (> spawned 0)
                                 (if (> auto 0)
                                     (format "%d spawned (%d auto-assigned)"
                                             spawned auto)
                                   (format "%d spawned" spawned)))
                            (and (> promoted 0) (format "%d promoted" promoted))
                            (and (> reclaimed 0)
                                 (format "%d reclaimed" reclaimed))
                            (and (> unassigned 0)
                                 (format "%d skipped unassigned" unassigned))))))
    (if parts
        (concat "Dispatcher: " (string-join parts ", "))
      "Dispatcher: nothing ready to dispatch")))

(defun hermes-kanban-nudge-dispatch (&optional dry-run)
  "Wake the kanban dispatcher to claim ready tasks now.
With prefix argument DRY-RUN, report what would spawn without spawning
anything.  Maps to the dashboard `POST /dispatch', the same quick-path
behind the web UI's Nudge dispatcher button; without it, ready tasks wait
for the dispatcher's next tick."
  (interactive "P")
  (let ((query (append (hermes-kanban--query-for-board
                        (hermes-kanban--board-slug-for-command))
                       (and dry-run '((dry_run . "true")))))
        (refresh (hermes-kanban--context-refresher)))
    (hermes-kanban--then
     (hermes-kanban--api "POST" "/dispatch" nil query)
     (lambda (result)
       (message "%s%s" (hermes-kanban--dispatch-summary result)
                (if dry-run " (dry run)" ""))
       (unless dry-run (funcall refresh))))))

(defun hermes-kanban-reclaim ()
  "Release the worker claim on the task at point after confirmation.
Reads an optional reason and refreshes the buffer on success.  Maps to the
dashboard `POST /tasks/:id/reclaim'; a 409 (task no longer claimable) is
reported as-is."
  (interactive)
  (let ((id (hermes-kanban--task-id-for-command))
        (query (hermes-kanban--query-for-board
                (hermes-kanban--board-slug-for-command)))
        (refresh (hermes-kanban--context-refresher)))
    (when (yes-or-no-p (format "Reclaim task %s? " id))
      (let ((reason (hermes-kanban--read-reason "Reclaim reason (optional): ")))
        (hermes-kanban--then
         (hermes-kanban--api "POST" (hermes-kanban--task-path id "/reclaim")
                             (hermes-kanban--reason-body reason) query)
         (lambda (_) (message "Reclaimed task %s" id) (funcall refresh)))))))

(defun hermes-kanban--terminate-run-for-task (task task-id query refresh)
  "Confirm and terminate TASK's current run, then call REFRESH.
TASK-ID labels the prompts and QUERY pins the board for the terminate call.
A task with no active run is reported and left untouched."
  (let ((run (hermes-kanban--run-id-for-task task)))
    (cond
     ((not run) (message "Task %s has no active run to terminate" task-id))
     ((yes-or-no-p (format "Terminate run #%d of task %s? " run task-id))
      (let ((reason (hermes-kanban--read-reason
                     "Terminate reason (optional): ")))
        (hermes-kanban--then
         (hermes-kanban--api "POST" (format "/runs/%d/terminate" run)
                             (hermes-kanban--reason-body reason) query)
         (lambda (_) (message "Terminated run #%d" run) (funcall refresh))))))))

(defun hermes-kanban-terminate-run ()
  "Terminate the worker process backing the task at point's current run.
Fetches the task to resolve its run id, confirms, then POSTs the terminate.
A task with no active run is reported; a 404/409 surfaces as a message."
  (interactive)
  (let ((id (hermes-kanban--task-id-for-command))
        (query (hermes-kanban--query-for-board
                (hermes-kanban--board-slug-for-command)))
        (refresh (hermes-kanban--context-refresher)))
    (hermes-kanban--then
     (hermes-kanban--api "GET" (hermes-kanban--task-path id) nil query)
     (lambda (payload)
       (hermes-kanban--terminate-run-for-task
        (hermes-transport--get payload 'task) id query refresh)))))

;;;###autoload
(defun hermes-list-kanban ()
  "Browse Hermes Kanban boards via the dashboard API."
  (interactive)
  (hermes-kanban--render-boards))

(provide 'hermes-kanban)
;;; hermes-kanban.el ends here
