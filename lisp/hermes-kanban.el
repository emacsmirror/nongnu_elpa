;;; hermes-kanban.el --- Kanban board browser for Hermes  -*- lexical-binding: t; -*-

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

;; Two `tabulated-list' browsers over the dashboard kanban HTTP plugin
;; (`/api/plugins/kanban/...'), mirroring how the web dashboard renders the
;; board.  Everything goes through the dashboard at
;; `hermes-dashboard-transport-url' -- never the local `hermes' CLI, which
;; reads local SQLite and ignores the configured remote.
;;
;; `hermes-list-kanban' opens the boards overview: one row per board with a
;; per-status count summary.  RET drills into a board's tasks, where RET shows
;; a task and e/a/s/c/+/D edit, assign, set-status, comment, create, delete.

;;; Code:

(require 'ansi-color)
(require 'diff-mode)
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
(require 'cl-lib)

(declare-function markdown-mode "markdown-mode")
(declare-function read-string-from-buffer "string-edit")
(declare-function hermes-kanban-task-mode "hermes-kanban")
(declare-function websocket-close "ext:websocket")

;;; HTTP against the dashboard kanban plugin

(defconst hermes-kanban-log-tail-bytes 100000
  "Number of worker-log bytes fetched for `hermes-kanban-show-log'.")

(defun hermes-kanban--api (method path &optional body query)
  "Return a promise of the kanban plugin response for METHOD PATH.
BODY and QUERY extend the request.  Authentication and a single retry on a
failed GET come from the shared dashboard transport, which talks only to
`hermes-dashboard-transport-url'."
  (hermes-dashboard-transport-api-request-async
   method (concat "/api/plugins/kanban" path)
   :body body :query query))

(defun hermes-kanban--then (promise on-ok)
  "Run ON-OK on PROMISE's resolved value, reporting any rejection."
  (hermes--promise-then
   promise on-ok
   (lambda (reason) (message "Hermes: %s" reason))))

;;; Field helpers

(defun hermes-kanban--count (counts status)
  "Return COUNTS' tally for STATUS as a string."
  (let ((n (hermes-transport--get counts (intern status))))
    (if (numberp n) (number-to-string n) "0")))

(defun hermes-kanban--format-time (value)
  "Return VALUE (a Unix timestamp) formatted, or an empty string."
  (if (numberp value)
      (format-time-string "%Y-%m-%d %H:%M" value)
    ""))

;;; Shared status display helpers

(defconst hermes-kanban--current-board-marker "📍"
  "Marker used for the current Hermes Kanban board.")

(defconst hermes-kanban--protected-board-slugs '("default")
  "Board slugs protected from archive/delete by the Hermes backend.")

(defconst hermes-kanban--status-display
  '(("todo" :icon "📝" :label "todo" :face nil)
    ("ready" :icon "✅" :label "ready" :face nil)
    ("running" :icon "⚙️" :label "running" :face nil)
    ("blocked" :icon "⛔" :label "blocked" :face nil)
    ("done" :icon "🏁" :label "done" :face nil)
    ("archived" :icon "🗄️" :label "archived" :face nil))
  "User-facing display metadata for Kanban task statuses.
Each entry maps a status string to :icon, :label, and optional :face.")

(defconst hermes-kanban--board-count-statuses
  '("todo" "ready" "running" "blocked" "done" "archived")
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
         (face (plist-get info :face))
         (text (copy-sequence
                (if (and icon (not (string-empty-p icon)))
                    (format "%s %s" icon label)
                  label))))
    (when (and face (not (string-empty-p text)))
      (setq text (propertize text 'face face)))
    (when (not (string-empty-p text))
      (add-text-properties 0 (length text) `(hermes-kanban-status ,raw) text))
    text))

(defun hermes-kanban--format-status-indicator (status)
  "Return STATUS as a compact task-table indicator.
Known statuses use only their icon.  Unknown statuses fall back to their
raw label.  The returned string carries the raw status as the
`hermes-kanban-status' text property, so commands can keep using backend
status values."
  (let* ((raw (or (hermes-transport--scalar-string status) ""))
         (info (hermes-kanban--status-info raw))
         (icon (plist-get info :icon))
         (face (plist-get info :face))
         (text (copy-sequence
                (or (hermes-transport--non-empty-string icon) raw))))
    (when (and face (not (string-empty-p text)))
      (setq text (propertize text 'face face)))
    (when (not (string-empty-p text))
      (add-text-properties 0 (length text) `(hermes-kanban-status ,raw) text))
    text))

(defun hermes-kanban--format-status-count (counts status)
  "Return COUNTS' tally for STATUS."
  (hermes-kanban--count counts status))

(defun hermes-kanban--status-column-heading (status)
  "Return the boards-overview heading for STATUS."
  (or (hermes-transport--non-empty-string (hermes-kanban--status-icon status))
      (capitalize (hermes-kanban--status-label status))))

(defun hermes-kanban--status-count-column (status)
  "Return one `tabulated-list-format' count column for STATUS."
  (let ((heading (hermes-kanban--status-column-heading status)))
    (list heading 5 t)))

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
             (let ((statuses hermes-kanban--board-count-statuses)
                   (status-widths (nthcdr 3 widths))
                   columns)
               (while statuses
                 (let ((column (hermes-kanban--status-count-column (car statuses))))
                   (setcar (cdr column) (car status-widths))
                   (push column columns))
                 (setq statuses (cdr statuses)
                       status-widths (cdr status-widths)))
               (nreverse columns)))))

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
              (vector (if (eq (hermes-transport--get board 'is_current) t)
                          hermes-kanban--current-board-marker
                        "")
                      (or (hermes-transport--non-empty-string
                           (hermes-transport--display-field board 'name))
                          slug)
                      (if (numberp total) (number-to-string total) "0"))
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
  (hermes-kanban--then
   (hermes-kanban--api "GET" "/boards")
   (lambda (payload)
     (let ((boards (hermes-transport--get payload 'boards)))
       (with-current-buffer (get-buffer-create "*Hermes Kanban Boards*")
         (unless (derived-mode-p 'hermes-kanban-boards-mode)
           (hermes-kanban-boards-mode))
         (setq tabulated-list-entries (hermes-kanban--board-rows boards))
         ;; Pop before --init-boards-header: it sizes columns from the live
         ;; window width, so the buffer must be displayed first.  Revert skips
         ;; the pop -- it already runs in the displayed window.
         (unless in-place (pop-to-buffer (current-buffer)))
         (hermes-kanban--init-boards-header
          (hermes-browser--visible-window-width))
         (tabulated-list-print t))))))

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

(defvar-local hermes-kanban--events-tail nil
  "Live-events tail for this board buffer, or nil when live updates are off.")

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
                     (hermes-transport--display-field task 'priority)
                     (or (hermes-transport--non-empty-string
                          (hermes-transport--display-field task 'assignee))
                         "-")
                     (hermes-transport--display-field task 'title))))
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
  :group "Board"
  "+" ("New task" hermes-kanban-create-task)
  "D" ("Delete task" hermes-kanban-delete)
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

(defun hermes-kanban--render-board (slug name &optional in-place)
  "Fetch and render board SLUG (display NAME) in the detail buffer.
With IN-PLACE non-nil, refresh without re-displaying the buffer (used by revert,
which already runs in the displayed window)."
  (hermes-kanban--then
   (hermes-kanban--api "GET" "/board" nil (and slug `((board . ,slug))))
   (lambda (payload)
     (let ((assignees (delq nil (mapcar #'hermes-transport--scalar-string
                                        (hermes-transport--get payload
                                                               'assignees)))))
       (with-current-buffer (get-buffer-create "*Hermes Kanban*")
         (unless (derived-mode-p 'hermes-kanban-mode)
           (hermes-kanban-mode))
         (setq hermes-kanban--slug slug
               hermes-kanban--name name
               hermes-kanban--assignees assignees
               hermes-kanban--latest-event-id (hermes-transport--get
                                               payload 'latest_event_id)
               mode-line-process (list (format " [%s]" (or name slug "board"))
                                       '(:eval (hermes-kanban--live-indicator)))
               tabulated-list-sort-key nil
               tabulated-list-entries (hermes-kanban--task-rows
                                       (hermes-transport--get payload 'columns)))
         ;; Pop before --init-board-header: it sizes columns from the live
         ;; window width, so the buffer must be displayed first.  Revert skips
         ;; the pop -- it already runs in the displayed window.
         (unless in-place (pop-to-buffer (current-buffer)))
         (hermes-kanban--init-board-header
          (hermes-browser--visible-window-width))
         (tabulated-list-print t))))))

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
     (when-let* ((error (hermes-transport--non-empty-string
                         (hermes-transport--display-field run 'error))))
       (format "\n- Error: %s" error))
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
  :group "Recovery"
  "R" ("Reclaim task" hermes-kanban-reclaim)
  "K" ("Terminate run" hermes-kanban-terminate-run)
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
        (assignees hermes-kanban-task--assignees))
    (hermes-kanban--then
     (hermes-kanban--api "GET" (hermes-kanban--task-path task-id)
                         nil (hermes-kanban--query-for-board board-slug))
     (lambda (payload)
       (hermes-kanban--display-task payload board-slug t assignees)))))

(defun hermes-kanban--display-task (payload &optional board-slug in-place assignees)
  "Render task PAYLOAD in a read-only detail buffer.
BOARD-SLUG is remembered for refreshes and log requests.  With IN-PLACE non-nil,
refresh without re-displaying the buffer (used by revert).  ASSIGNEES carries
the board-known assignee names for cold profile-cache completion fallback."
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
        (read-only-mode 1))
      (goto-char (point-min))
      (unless in-place (pop-to-buffer (current-buffer))))))

(defun hermes-kanban-show ()
  "Show the kanban task at point."
  (interactive)
  (let ((board-slug hermes-kanban--slug)
        (assignees hermes-kanban--assignees)
        (id (hermes-kanban--id-at-point)))
    (hermes-kanban--then
     (hermes-kanban--api "GET" (hermes-kanban--task-path id)
                         nil (hermes-kanban--query-for-board board-slug))
     (lambda (payload) (hermes-kanban--display-task payload board-slug nil assignees)))))

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

(defun hermes-kanban--sanitize-log-content (content)
  "Return CONTENT normalized for human-readable log display."
  (replace-regexp-in-string "\r\n?" "\n" (or content "") t t))

(defun hermes-kanban--diff-hunk-counts ()
  "Return old/new line counts for a unified diff hunk at point."
  (when (looking-at diff-hunk-header-re-unified)
    (cons (if-let* ((count (match-string 2)))
              (string-to-number count)
            1)
          (if-let* ((count (match-string 4)))
              (string-to-number count)
            1))))

(defun hermes-kanban--diff-hunk-header-p ()
  "Return non-nil when point is at a unified diff hunk header."
  (hermes-kanban--diff-hunk-counts))

(defun hermes-kanban--diff-header-line-p ()
  "Return non-nil when point is at unified diff file metadata."
  (or (looking-at
       (concat "^\\(?:diff --git \\|index \\|old mode \\|new mode \\|"
               "new file mode \\|deleted file mode \\|similarity index \\|"
               "dissimilarity index \\|rename from \\|rename to \\|"
               "copy from \\|copy to \\|--- \\|\\+\\+\\+ \\)"))
      (looking-at "^.+ → .+$")))

(defun hermes-kanban--diff-body-line-counts ()
  "Return old/new line counts for the current unified diff body line."
  (cond
   ((looking-at "^\\\\ No newline at end of file") '(0 . 0))
   ((looking-at "^\\+") '(0 . 1))
   ((looking-at "^-") '(1 . 0))
   ((looking-at "^ ") '(1 . 1))
   ((looking-at "^$") '(1 . 1))))

(defun hermes-kanban--consume-diff-hunk ()
  "Move over a valid unified diff hunk at point.
Return non-nil when the consumed hunk contains an added or removed line."
  (let ((start (point)))
    (when-let* ((counts (hermes-kanban--diff-hunk-counts)))
      (let ((old-left (car counts))
            (new-left (cdr counts))
            saw-change valid)
        (forward-line 1)
        (setq valid t)
        (while (and valid
                    (not (and (<= old-left 0) (<= new-left 0)))
                    (not (eobp)))
          (if-let* ((line-counts (hermes-kanban--diff-body-line-counts)))
              (let ((old-count (car line-counts))
                    (new-count (cdr line-counts)))
                (if (or (> old-count old-left)
                        (> new-count new-left))
                    (setq valid nil)
                  (when (or (and (= old-count 1) (= new-count 0))
                            (and (= old-count 0) (= new-count 1)))
                    (setq saw-change t))
                  (setq old-left (- old-left old-count)
                        new-left (- new-left new-count))
                  (forward-line 1)))
            (setq valid nil)))
        (when (or (> old-left 0) (> new-left 0))
          (setq valid nil))
        (while (and valid
                    (not (eobp))
                    (looking-at "^\\\\ No newline at end of file"))
          (forward-line 1))
        (if (and valid saw-change)
            t
          (goto-char start)
          nil)))))

(defun hermes-kanban--diff-range-at-point ()
  "Return embedded unified diff range at point as zero-based offsets, or nil."
  (let ((start (point))
        saw-hunk keep-scanning)
    (when (or (hermes-kanban--diff-header-line-p)
              (hermes-kanban--diff-hunk-header-p))
      (while (hermes-kanban--diff-header-line-p)
        (forward-line 1))
      (setq keep-scanning t)
      (while (and keep-scanning
                  (hermes-kanban--diff-hunk-header-p))
        (if (hermes-kanban--consume-diff-hunk)
            (setq saw-hunk t)
          (setq keep-scanning nil)))
      (if (and saw-hunk (< start (point)))
          (cons (1- start) (1- (point)))
        (goto-char start)
        nil))))

(defun hermes-kanban--diff-blocks (content)
  "Return embedded unified diff ranges in CONTENT as zero-based conses."
  (with-temp-buffer
    (insert (substring-no-properties content))
    (goto-char (point-min))
    (cl-loop until (eobp)
             for range = (hermes-kanban--diff-range-at-point)
             if range
             collect range
             and do (goto-char (1+ (cdr range)))
             else do (forward-line 1))))

(defun hermes-kanban--fontify-diff-string (text)
  "Return TEXT fontified with `diff-mode', or TEXT on failure."
  (condition-case nil
      (with-temp-buffer
        (insert (substring-no-properties text))
        (delay-mode-hooks (diff-mode))
        (font-lock-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (buffer-string))
    (error text)))

(defun hermes-kanban--fontify-log-diffs (text)
  "Return TEXT with embedded unified diff blocks fontified."
  (let ((blocks (hermes-kanban--diff-blocks text)))
    (if (null blocks)
        text
      (with-temp-buffer
        (let ((pos 0))
          (dolist (block blocks)
            (insert (substring text pos (car block)))
            (insert (hermes-kanban--fontify-diff-string
                     (substring text (car block) (cdr block))))
            (setq pos (cdr block)))
          (insert (substring text pos)))
        (buffer-string)))))

(defun hermes-kanban--render-log-content (content)
  "Return CONTENT normalized, ANSI-colored, and diff-fontified for display."
  (hermes-kanban--fontify-log-diffs
   (ansi-color-apply (hermes-kanban--sanitize-log-content content))))

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

(defun hermes-kanban-log--valid-hunk-header-p ()
  "Return non-nil when point is at a validated embedded diff hunk header.
A header is validated by consuming it like `hermes-kanban--diff-range-at-point'
does, so header-shaped log text that the fontifier rejected is skipped."
  (save-excursion (hermes-kanban--consume-diff-hunk)))

(defun hermes-kanban-log-next-hunk (&optional arg)
  "Move to the next validated embedded unified diff hunk.
ARG is a positive repeat count, as in `diff-hunk-next'.  Only hunks that
pass `hermes-kanban-log--valid-hunk-header-p' are visited, so incomplete
header-shaped blocks are skipped.  Point is left unchanged when no valid
hunk follows."
  (interactive "p")
  (let ((count (prefix-numeric-value arg)))
    (when (> count 0)
      (dotimes (_ count)
        ;; ORIGIN excludes the hunk point already sits on, so a second
        ;; `n' from a hunk header advances past it instead of re-matching.
        (let ((origin (point))
              done)
          (while (and (not done)
                      (re-search-forward diff-hunk-header-re-unified nil t))
            (let ((header (match-beginning 0)))
              (cond
               ((<= header origin)
                (goto-char (match-end 0)))
               ((save-excursion
                  (goto-char header)
                  (hermes-kanban--consume-diff-hunk))
                (goto-char header)
                (setq done t))
               (t (goto-char (match-end 0))))))
          (unless done (goto-char origin)))))))

(defun hermes-kanban-log-previous-hunk (&optional arg)
  "Move to the previous validated embedded unified diff hunk.
ARG is a positive repeat count, as in `diff-hunk-prev'.  Only hunks that
pass `hermes-kanban-log--valid-hunk-header-p' are visited, so incomplete
header-shaped blocks are skipped.  Point is left unchanged when no valid
hunk precedes point."
  (interactive "p")
  (let ((count (prefix-numeric-value arg)))
    (when (> count 0)
      (dotimes (_ count)
        ;; re-search-backward lands at match-beginning, so a candidate is
        ;; validated in place; an invalid header is naturally left behind
        ;; by the next backward search.
        (let ((origin (point))
              done)
          (while (and (not done)
                      (re-search-backward diff-hunk-header-re-unified nil t))
            (cond
             ((>= (point) origin))
             ((hermes-kanban-log--valid-hunk-header-p)
              (setq done t))
             (t)))
          (unless done (goto-char origin)))))))

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
        (board-slug hermes-kanban-log--board-slug))
    (hermes-kanban--then
     (hermes-kanban--fetch-log id board-slug)
     (lambda (payload) (hermes-kanban--display-log payload board-slug t)))))

(define-derived-mode hermes-kanban-log-mode special-mode "Hermes Log"
  "Major mode for a Hermes Kanban worker log buffer."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-kanban--log-revert)
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defun hermes-kanban--display-log (payload &optional board-slug in-place)
  "Render worker log PAYLOAD for BOARD-SLUG in a read-only buffer.
With IN-PLACE non-nil, refresh without re-displaying (used by revert)."
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
        (board-slug (hermes-kanban--board-slug-for-command)))
    (hermes-kanban--then
     (hermes-kanban--fetch-log id board-slug)
     (lambda (payload) (hermes-kanban--display-log payload board-slug)))))

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
  '("todo" "ready" "blocked" "scheduled" "done" "archived" "triage")
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

(defun hermes-kanban-create-task ()
  "Create a task on the current board."
  (interactive)
  (let ((title (read-string "Title: "))
        (who (completing-read "Assignee (optional): "
                              hermes-kanban--assignees nil nil))
        (priority (read-number "Priority: " 0))
        (slug hermes-kanban--slug)
        (name hermes-kanban--name))
    (when (string-empty-p (string-trim title))
      (user-error "Title is required"))
    (let ((body `((title . ,title) (priority . ,priority))))
      (unless (string-empty-p who)
        (setq body (append body `((assignee . ,who)))))
      (hermes-kanban--then
       (hermes-kanban--api "POST" "/tasks" body (hermes-kanban--board-query))
       (lambda (_) (hermes-kanban--render-board slug name))))))

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
           (hermes-transport--display-field top 'severity)
           (or (hermes-transport--non-empty-string
                (hermes-transport--display-field group 'task_title))
               task-id)
           (or (hermes-transport--non-empty-string
                (hermes-transport--display-field group 'task_assignee))
               "-")
           (hermes-kanban--diagnostic-summary top (length diagnostics))))))

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
  (hermes-kanban--then
   (hermes-kanban--api "GET" "/diagnostics"
                       nil (hermes-kanban--query-for-board slug))
   (lambda (payload)
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
           (message "No active diagnostics on this board")))))))

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

;;; Live events tail

(cl-defstruct (hermes-kanban--events-tail
               (:constructor hermes-kanban--events-tail-create))
  "State for one board buffer's live-events WebSocket."
  socket buffer slug (cursor 0) refresh-timer (backoff 1) reconnect-timer
  (active t))

(defconst hermes-kanban--events-debounce 0.4
  "Seconds to debounce an in-place board refresh from live events.")

(defconst hermes-kanban--events-backoff-max 30
  "Maximum reconnect backoff in seconds for the live-events tail.")

(defun hermes-kanban--live-indicator ()
  "Return the board mode-line live-status indicator.
Live is keyed on the socket, not the tail struct, so a tail waiting in the
reconnect backoff shows as retrying rather than falsely live."
  (cond
   ((null hermes-kanban--events-tail)
    (propertize " ○" 'face 'shadow))
   ((hermes-kanban--events-tail-socket hermes-kanban--events-tail)
    (propertize " ●live" 'face 'success))
   (t (propertize " ◌retry" 'face 'warning))))

(defun hermes-kanban--events-refresh (tail)
  "Refresh TAIL's board buffer in place when it is still live."
  (setf (hermes-kanban--events-tail-refresh-timer tail) nil)
  (let ((buffer (hermes-kanban--events-tail-buffer tail)))
    (when (and (hermes-kanban--events-tail-active tail) (buffer-live-p buffer))
      (with-current-buffer buffer (revert-buffer nil t)))))

(defun hermes-kanban--events-schedule-refresh (tail)
  "Debounce an in-place board refresh for TAIL."
  (when-let* ((timer (hermes-kanban--events-tail-refresh-timer tail)))
    (cancel-timer timer))
  (setf (hermes-kanban--events-tail-refresh-timer tail)
        (run-at-time hermes-kanban--events-debounce nil
                     #'hermes-kanban--events-refresh tail)))

(defun hermes-kanban--events-handle-frame (tail text)
  "Advance TAIL's cursor from the JSON frame TEXT and schedule a refresh.
TEXT is a plain `{events,cursor}' frame, parsed on this socket alone -- never
through the chat client's JSON-RPC handler."
  (when (hermes-kanban--events-tail-active tail)
    (setf (hermes-kanban--events-tail-backoff tail) 1)
    (when-let* ((frame (ignore-errors
                         (json-parse-string text :object-type 'alist
                                            :array-type 'list
                                            :null-object nil :false-object nil))))
      (let ((cursor (hermes-transport--get frame 'cursor)))
        (when (numberp cursor)
          (setf (hermes-kanban--events-tail-cursor tail) cursor)))
      (hermes-kanban--events-schedule-refresh tail))))

(defun hermes-kanban--events-reconnect (tail)
  "Schedule a bounded-backoff reconnect for TAIL, stopping when its buffer dies."
  (when (and (hermes-kanban--events-tail-active tail)
             (buffer-live-p (hermes-kanban--events-tail-buffer tail))
             (not (hermes-kanban--events-tail-reconnect-timer tail)))
    (let ((delay (hermes-kanban--events-tail-backoff tail)))
      (setf (hermes-kanban--events-tail-backoff tail)
            (min hermes-kanban--events-backoff-max (* 2 delay))
            (hermes-kanban--events-tail-reconnect-timer tail)
            (run-at-time delay nil #'hermes-kanban--events-do-reconnect tail)))))

(defun hermes-kanban--events-do-reconnect (tail)
  "Clear TAIL's reconnect timer and reconnect when still active."
  (setf (hermes-kanban--events-tail-reconnect-timer tail) nil)
  (when (and (hermes-kanban--events-tail-active tail)
             (buffer-live-p (hermes-kanban--events-tail-buffer tail)))
    (hermes-kanban--events-connect tail)))

(defun hermes-kanban--events-on-down (tail &optional message)
  "Drop TAIL's socket, report optional MESSAGE, and reconnect with backoff."
  (when message (message "Hermes kanban live: %s" message))
  (setf (hermes-kanban--events-tail-socket tail) nil)
  (hermes-kanban--events-reconnect tail))

(defun hermes-kanban--events-connect (tail)
  "Resolve the events URL for TAIL and open its socket.
A failed URL resolve or socket open re-enters the bounded backoff like a
dropped connection, instead of permanently killing the tail."
  (hermes--promise-then
   (hermes-dashboard-transport-kanban-events-url-async
    :since (hermes-kanban--events-tail-cursor tail)
    :board (hermes-kanban--events-tail-slug tail))
   (lambda (url)
     (when (hermes-kanban--events-tail-active tail)
       (condition-case err
           (setf (hermes-kanban--events-tail-socket tail)
                 (hermes-dashboard-transport-open-websocket
                  (plist-get url :url) (plist-get url :redacted-url)
                  (plist-get url :secrets)
                  :on-message (lambda (text)
                                (hermes-kanban--events-handle-frame tail text))
                  :on-close (lambda () (hermes-kanban--events-on-down tail))
                  :on-error (lambda (msg)
                              (hermes-kanban--events-on-down tail msg))))
         (error (hermes-kanban--events-on-down
                 tail (error-message-string err))))))
   (lambda (reason)
     (hermes-kanban--events-on-down tail (format "%s" reason)))))

(defun hermes-kanban--events-disconnect (tail)
  "Tear down TAIL: stop reconnecting, cancel timers, and close the socket."
  (setf (hermes-kanban--events-tail-active tail) nil)
  (when-let* ((timer (hermes-kanban--events-tail-refresh-timer tail)))
    (cancel-timer timer))
  (when-let* ((timer (hermes-kanban--events-tail-reconnect-timer tail)))
    (cancel-timer timer))
  (setf (hermes-kanban--events-tail-refresh-timer tail) nil
        (hermes-kanban--events-tail-reconnect-timer tail) nil)
  (when-let* ((socket (hermes-kanban--events-tail-socket tail)))
    (when (fboundp 'websocket-close) (ignore-errors (websocket-close socket))))
  (setf (hermes-kanban--events-tail-socket tail) nil))

(defun hermes-kanban--events-teardown ()
  "Disconnect the board buffer's events tail when the buffer is killed."
  (when hermes-kanban--events-tail
    (hermes-kanban--events-disconnect hermes-kanban--events-tail)
    (setq hermes-kanban--events-tail nil)))

(defun hermes-kanban-toggle-live ()
  "Toggle the live-events tail for the current board buffer.
When on, a dedicated WebSocket streams task events and the board refreshes in
place; the mode line shows a live indicator."
  (interactive)
  (unless (derived-mode-p 'hermes-kanban-mode)
    (user-error "Live updates are only available on a board buffer"))
  (if hermes-kanban--events-tail
      (progn
        (hermes-kanban--events-disconnect hermes-kanban--events-tail)
        (setq hermes-kanban--events-tail nil)
        (force-mode-line-update)
        (message "Hermes kanban live updates off"))
    (let ((tail (hermes-kanban--events-tail-create
                 :buffer (current-buffer) :slug hermes-kanban--slug
                 :cursor (or hermes-kanban--latest-event-id 0))))
      (setq hermes-kanban--events-tail tail)
      (add-hook 'kill-buffer-hook #'hermes-kanban--events-teardown nil t)
      (force-mode-line-update)
      (hermes-kanban--events-connect tail)
      (message "Hermes kanban live updates on"))))

;;;###autoload
(defun hermes-list-kanban ()
  "Browse Hermes Kanban boards via the dashboard API."
  (interactive)
  (hermes-kanban--render-boards))

(provide 'hermes-kanban)
;;; hermes-kanban.el ends here
