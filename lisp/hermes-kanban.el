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

(require 'tabulated-list)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'keymap-popup)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)

;;; HTTP + auth against the dashboard kanban plugin

(defvar hermes-kanban--auth nil
  "Cached auth plist: (:base-url URL :headers HEADERS :secrets SECRETS).
Invalidated and re-resolved when a GET fails, covering expired cookies.")

(defconst hermes-kanban-log-tail-bytes 100000
  "Number of worker-log bytes fetched for `hermes-kanban-show-log'.")

(defun hermes-kanban--base-url ()
  "Return the normalized dashboard base URL, or signal a `user-error'."
  (or (hermes-dashboard-transport--normalize-base-url
       hermes-dashboard-transport-url)
      (user-error "Set `hermes-dashboard-transport-url' to your Hermes dashboard URL")))

(defun hermes-kanban--token-auth (base-url)
  "Return legacy session-token auth headers for BASE-URL."
  (let ((token (hermes-dashboard-transport--remote-token-secret base-url)))
    (list :headers (list (cons "X-Hermes-Session-Token" token))
          :secrets (list token))))

(defun hermes-kanban--basic-auth (base-url status)
  "Return cookie auth headers for gated BASE-URL described by STATUS."
  (let ((provider (hermes-dashboard-transport--status-basic-provider status)))
    (unless provider
      (hermes-dashboard-transport--unsupported-remote-auth base-url))
    (let* ((creds (hermes-dashboard-transport--remote-basic-credentials base-url))
           (password (plist-get creds :password))
           (response (hermes-dashboard-transport--http-json
                      (hermes-dashboard-transport--api-url
                       base-url "/auth/password-login")
                      :method "POST"
                      :headers '(("Content-Type" . "application/json"))
                      :body `((provider . ,provider)
                              (username . ,(plist-get creds :username))
                              (password . ,password)
                              (next . ""))
                      :secrets (list password)))
           (cookies (hermes-dashboard-transport--response-cookie-header response)))
      (unless cookies
        (user-error "Hermes dashboard basic login returned no session cookies"))
      (list :headers (list (cons "Cookie" cookies))
            :secrets (list password cookies)))))

(defun hermes-kanban--authenticate ()
  "Resolve dashboard auth per `hermes-dashboard-transport-remote-auth-method'."
  (let ((base-url (hermes-kanban--base-url)))
    (append
     (list :base-url base-url)
     (pcase hermes-dashboard-transport-remote-auth-method
       ('token (hermes-kanban--token-auth base-url))
       ('basic (hermes-kanban--basic-auth
                base-url (hermes-dashboard-transport--remote-status base-url)))
       (_ (let ((status (hermes-dashboard-transport--remote-status base-url)))
            (if (hermes-dashboard-transport--status-auth-required-p status)
                (hermes-kanban--basic-auth base-url status)
              (hermes-kanban--token-auth base-url))))))))

(defun hermes-kanban--auth ()
  "Return cached auth, resolving it on first use."
  (or hermes-kanban--auth
      (setq hermes-kanban--auth (hermes-kanban--authenticate))))

(defun hermes-kanban--query-string (query)
  "Return a URL query string for QUERY, an alist of (KEY . VALUE)."
  (if query
      (concat "?" (string-join
                   (mapcar (lambda (kv)
                             (format "%s=%s" (car kv)
                                     (url-hexify-string (format "%s" (cdr kv)))))
                           query)
                   "&"))
    ""))

(defun hermes-kanban--api-1 (method path body query retry)
  "Call kanban plugin METHOD PATH with BODY and QUERY; retry once when RETRY."
  (let* ((auth (hermes-kanban--auth))
         (url (concat (hermes-dashboard-transport--api-url
                       (plist-get auth :base-url)
                       (concat "/api/plugins/kanban" path))
                      (hermes-kanban--query-string query)))
         (headers (append (plist-get auth :headers)
                          (and body '(("Content-Type" . "application/json"))))))
    (condition-case err
        (plist-get
         (hermes-dashboard-transport--http-json
          url :method method :headers headers :body body
          :secrets (plist-get auth :secrets))
         :body)
      (error
       (if retry
           (progn (setq hermes-kanban--auth nil)
                  (hermes-kanban--api-1 method path body query nil))
         (signal (car err) (cdr err)))))))

(defun hermes-kanban--api (method path &optional body query)
  "Call the kanban plugin: METHOD PATH with optional BODY and QUERY alist.
Return the parsed JSON body.  GET requests retry once after re-authenticating."
  (hermes-kanban--api-1 method path body query (equal method "GET")))

;;; Field helpers

(defun hermes-kanban--field (object key)
  "Return OBJECT's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get object key)) ""))

(defun hermes-kanban--non-empty (string)
  "Return STRING when it is a non-empty string."
  (and (stringp string) (not (string-empty-p string)) string))

(defun hermes-kanban--count (counts status)
  "Return COUNTS' tally for STATUS as a string."
  (let ((n (hermes-transport--get counts (intern status))))
    (if (numberp n) (number-to-string n) "0")))

(defun hermes-kanban--format-time (value)
  "Return VALUE (a Unix timestamp) formatted, or an empty string."
  (if (numberp value)
      (format-time-string "%Y-%m-%d %H:%M" value)
    ""))

;;; Boards overview buffer

(defun hermes-kanban--board-rows (boards)
  "Return `tabulated-list' entries for BOARDS, the dashboard board list."
  (mapcar
   (lambda (board)
     (let ((slug (hermes-kanban--field board 'slug))
           (counts (hermes-transport--get board 'counts))
           (total (hermes-transport--get board 'total)))
       (list (cons slug (hermes-kanban--non-empty (hermes-kanban--field board 'name)))
             (vector (if (eq (hermes-transport--get board 'is_current) t) "●" "")
                     (or (hermes-kanban--non-empty (hermes-kanban--field board 'name)) slug)
                     (if (numberp total) (number-to-string total) "0")
                     (hermes-kanban--count counts "todo")
                     (hermes-kanban--count counts "ready")
                     (hermes-kanban--count counts "running")
                     (hermes-kanban--count counts "review")
                     (hermes-kanban--count counts "blocked")
                     (hermes-kanban--count counts "done")))))
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
  :group "View"
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-kanban-boards-mode-map-popup))

(define-derived-mode hermes-kanban-boards-mode tabulated-list-mode "Hermes Boards"
  "Major mode for the Hermes Kanban boards overview."
  :interactive nil
  (setq tabulated-list-format
        [("Cur" 3 t) ("Board" 22 t) ("Total" 6 t) ("Todo" 6 t) ("Ready" 6 t)
         ("Run" 5 t) ("Review" 7 t) ("Block" 6 t) ("Done" 6 t)])
  (setq-local revert-buffer-function #'hermes-kanban--boards-revert)
  (tabulated-list-init-header))

(defun hermes-kanban--render-boards ()
  "Fetch and render the dashboard boards overview."
  (let ((boards (hermes-transport--get (hermes-kanban--api "GET" "/boards") 'boards)))
    (with-current-buffer (get-buffer-create "*Hermes Kanban Boards*")
      (unless (derived-mode-p 'hermes-kanban-boards-mode)
        (hermes-kanban-boards-mode))
      (setq tabulated-list-entries (hermes-kanban--board-rows boards))
      (tabulated-list-print t)
      (pop-to-buffer (current-buffer)))))

(defun hermes-kanban--boards-revert (&rest _)
  "Refresh the boards overview."
  (hermes-kanban--render-boards))

(defun hermes-kanban-open-board ()
  "Open the board at point in the detail buffer."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No board on this line"))
    (hermes-kanban--render-board (car id) (cdr id))))

(defun hermes-kanban-create-board ()
  "Create a new board from the boards overview."
  (interactive)
  (let ((slug (read-string "New board slug: "))
        (name (read-string "Display name: ")))
    (when (string-empty-p slug)
      (user-error "Board slug is required"))
    (hermes-kanban--api "POST" "/boards"
                        `((slug . ,slug)
                          (name . ,(if (string-empty-p name) slug name))
                          (switch . :false)))
    (hermes-kanban--render-boards)))

;;; Board detail buffer

(defvar-local hermes-kanban--slug nil
  "Slug of the board shown in this detail buffer.")

(defvar-local hermes-kanban--name nil
  "Display name of the board shown in this detail buffer.")

(defvar-local hermes-kanban--assignees nil
  "Known assignees on the current board, for completion.")

(defun hermes-kanban--task-rows (columns)
  "Flatten dashboard COLUMNS (status-grouped) into `tabulated-list' entries."
  (let (rows)
    (dolist (column columns (nreverse rows))
      (dolist (task (hermes-transport--get column 'tasks))
        (push (list (hermes-kanban--field task 'id)
                    (vector (hermes-kanban--field task 'status)
                            (hermes-kanban--field task 'priority)
                            (or (hermes-kanban--non-empty
                                 (hermes-kanban--field task 'assignee))
                                "-")
                            (hermes-kanban--field task 'title)))
              rows)))))

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
  "a" ("Assign / reassign" hermes-kanban-assign)
  "s" ("Set status" hermes-kanban-set-status)
  "c" ("Comment" hermes-kanban-comment)
  :group "Board"
  "+" ("New task" hermes-kanban-create-task)
  "D" ("Delete task" hermes-kanban-delete)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "l" ("View selected task log" hermes-kanban-show-log)
  "?" ("Help" hermes-kanban-mode-map-popup))

(define-derived-mode hermes-kanban-mode tabulated-list-mode "Hermes Kanban"
  "Major mode for browsing a single Hermes Kanban board."
  :interactive nil
  (setq tabulated-list-format
        [("Status" 10 t) ("Pri" 4 t) ("Assignee" 16 t) ("Title" 60 t)])
  (setq-local revert-buffer-function #'hermes-kanban--revert)
  (tabulated-list-init-header))

(defun hermes-kanban--render-board (slug name)
  "Fetch and render board SLUG (display NAME) in the detail buffer."
  (let* ((payload (hermes-kanban--api "GET" "/board" nil
                                      (and slug `((board . ,slug)))))
         (assignees (delq nil (mapcar #'hermes-transport--scalar-string
                                      (hermes-transport--get payload 'assignees)))))
    (with-current-buffer (get-buffer-create "*Hermes Kanban*")
      (unless (derived-mode-p 'hermes-kanban-mode)
        (hermes-kanban-mode))
      (setq hermes-kanban--slug slug
            hermes-kanban--name name
            hermes-kanban--assignees assignees
            mode-line-process (format " [%s]" (or name slug "board"))
            tabulated-list-sort-key nil
            tabulated-list-entries (hermes-kanban--task-rows
                                    (hermes-transport--get payload 'columns)))
      (tabulated-list-print t)
      (pop-to-buffer (current-buffer)))))

(defun hermes-kanban--revert (&rest _)
  "Refresh the current board detail buffer in place."
  (hermes-kanban--render-board hermes-kanban--slug hermes-kanban--name))

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

(defun hermes-kanban--format-size (bytes)
  "Return BYTES as a small human-readable size string."
  (cond
   ((not (numberp bytes)) "")
   ((< bytes 1024) (format "%d B" bytes))
   ((< bytes (* 1024 1024)) (format "%.1f KiB" (/ bytes 1024.0)))
   (t (format "%.1f MiB" (/ bytes 1048576.0)))))

(defun hermes-kanban--format-section (title items empty-name formatter)
  "Return a drawer section TITLE for ITEMS using FORMATTER.
EMPTY-NAME is inserted in the explicit empty-state line."
  (let ((rows (hermes-kanban--items items)))
    (concat "\n" title " (" (number-to-string (length rows)) "):\n"
            (if rows
                (string-join (mapcar formatter rows) "\n")
              (format "  — no %s —" empty-name))
            "\n")))

(defun hermes-kanban--format-task (task)
  "Return TASK's header and body as a display string."
  (let ((latest-summary (hermes-kanban--non-empty
                         (hermes-kanban--field task 'latest_summary))))
    (concat
     (format "Title:    %s\nID:       %s\nStatus:   %s   Priority: %s   Assignee: %s\nCreated:  %s\n"
             (hermes-kanban--field task 'title)
             (hermes-kanban--field task 'id)
             (hermes-kanban--field task 'status)
             (hermes-kanban--field task 'priority)
             (or (hermes-kanban--non-empty (hermes-kanban--field task 'assignee)) "-")
             (hermes-kanban--format-time (hermes-transport--get task 'created_at)))
     (when-let* ((workspace (hermes-kanban--non-empty
                             (hermes-kanban--field task 'workspace_kind))))
       (format "Workspace: %s%s\n" workspace
               (if-let* ((path (hermes-kanban--non-empty
                                (hermes-kanban--field task 'workspace_path))))
                   (concat ": " path)
                 "")))
     (when latest-summary
       (format "Summary:  %s\n" latest-summary))
     "\n"
     (hermes-kanban--field task 'body)
     "\n")))

(defun hermes-kanban--format-comments (comments)
  "Return COMMENTS as a display string, or an empty string."
  (if (null comments) ""
    (concat "\nComments:\n"
            (mapconcat
             (lambda (comment)
               (format "  [%s] %s: %s"
                       (hermes-kanban--format-time
                        (hermes-transport--get comment 'created_at))
                       (hermes-kanban--field comment 'author)
                       (hermes-kanban--field comment 'body)))
             comments "\n")
            "\n")))

(defun hermes-kanban--format-events (events)
  "Return EVENTS as a display string, or an empty string."
  (if (null events) ""
    (concat "\nEvents:\n"
            (mapconcat
             (lambda (event)
               (format "  [%s] %s"
                       (hermes-kanban--format-time
                        (hermes-transport--get event 'created_at))
                       (hermes-kanban--field event 'kind)))
             events "\n")
            "\n")))

(defun hermes-kanban--format-comment-row (comment)
  "Return COMMENT as one drawer row."
  (format "  [%s] %s: %s"
          (hermes-kanban--format-time (hermes-transport--get comment 'created_at))
          (or (hermes-kanban--non-empty (hermes-kanban--field comment 'author))
              "anon")
          (hermes-kanban--field comment 'body)))

(defun hermes-kanban--format-event-row (event)
  "Return EVENT as one drawer row."
  (let ((payload (hermes-transport--get event 'payload)))
    (concat
     (format "  [%s] %s"
             (hermes-kanban--format-time (hermes-transport--get event 'created_at))
             (hermes-kanban--field event 'kind))
     (when payload
       (format "\n    Payload: %s" (hermes-kanban--object-string payload))))))

(defun hermes-kanban--format-attachment (attachment)
  "Return ATTACHMENT as one drawer row."
  (let ((size (hermes-kanban--format-size
               (hermes-transport--get attachment 'size)))
        (content-type (hermes-kanban--non-empty
                       (hermes-kanban--field attachment 'content_type)))
        (uploaded-by (hermes-kanban--non-empty
                      (hermes-kanban--field attachment 'uploaded_by)))
        (path (hermes-kanban--non-empty
               (hermes-kanban--field attachment 'stored_path))))
    (concat
     (format "  #%s %s%s"
             (hermes-kanban--field attachment 'id)
             (hermes-kanban--field attachment 'filename)
             (if (string-empty-p size) "" (format " (%s)" size)))
     (when content-type (format "\n    Type: %s" content-type))
     (when uploaded-by (format "\n    Uploaded by: %s" uploaded-by))
     (when path (format "\n    Path: %s" path)))))

(defun hermes-kanban--format-diagnostic-action (action)
  "Return ACTION as a short diagnostic action label."
  (let ((label (or (hermes-kanban--non-empty (hermes-kanban--field action 'label))
                   (hermes-kanban--field action 'kind))))
    (if (hermes-kanban--truthy-p (hermes-transport--get action 'suggested))
        (concat label " (suggested)")
      label)))

(defun hermes-kanban--format-diagnostic (diagnostic)
  "Return DIAGNOSTIC as one drawer row."
  (let ((actions (hermes-kanban--items
                  (hermes-transport--get diagnostic 'actions)))
        (data (hermes-transport--get diagnostic 'data)))
    (concat
     (format "  [%s] %s: %s"
             (hermes-kanban--field diagnostic 'severity)
             (hermes-kanban--field diagnostic 'kind)
             (hermes-kanban--field diagnostic 'title))
     (when-let* ((detail (hermes-kanban--non-empty
                          (hermes-kanban--field diagnostic 'detail))))
       (format "\n    %s" detail))
     (when (or (hermes-transport--get diagnostic 'run_id)
               (hermes-transport--get diagnostic 'count))
       (format "\n    Run: %s   Count: %s"
               (or (hermes-kanban--field diagnostic 'run_id) "-")
               (or (hermes-kanban--field diagnostic 'count) "-")))
     (when data
       (format "\n    Data: %s" (hermes-kanban--object-string data)))
     (when actions
       (format "\n    Actions: %s"
               (string-join (mapcar #'hermes-kanban--format-diagnostic-action
                                    actions)
                            ", "))))))

(defun hermes-kanban--format-run (run)
  "Return RUN as one drawer row."
  (let* ((outcome (hermes-kanban--non-empty
                   (hermes-kanban--field run 'outcome)))
         (status (hermes-kanban--non-empty
                  (hermes-kanban--field run 'status)))
         (state (or outcome status "-"))
         (profile (or (hermes-kanban--non-empty
                       (hermes-kanban--field run 'profile))
                      "-"))
         (started (hermes-transport--get run 'started_at))
         (ended (hermes-transport--get run 'ended_at))
         (metadata (hermes-transport--get run 'metadata)))
    (concat
     (format "  #%s %s @%s"
             (hermes-kanban--field run 'id) state profile)
     (when (and (numberp started) (numberp ended))
       (format " (%ss)" (max 0 (- ended started))))
     (when (numberp started)
       (format "\n    Started: %s" (hermes-kanban--format-time started)))
     (when (numberp ended)
       (format "   Ended: %s" (hermes-kanban--format-time ended)))
     (when-let* ((pid (hermes-kanban--non-empty
                       (hermes-kanban--field run 'worker_pid))))
       (format "\n    PID: %s" pid))
     (when-let* ((summary (hermes-kanban--non-empty
                           (hermes-kanban--field run 'summary))))
       (format "\n    Summary: %s" summary))
     (when-let* ((error (hermes-kanban--non-empty
                         (hermes-kanban--field run 'error))))
       (format "\n    Error: %s" error))
     (when metadata
       (format "\n    Metadata: %s" (hermes-kanban--object-string metadata))))))

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
  :group "View"
  "g" ("Refresh" revert-buffer)
  "l" ("View worker log" hermes-kanban-show-log)
  "?" ("Help" hermes-kanban-task-mode-map-popup))

(define-derived-mode hermes-kanban-task-mode special-mode "Hermes Task"
  "Major mode for a Hermes Kanban task detail drawer."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-kanban--task-revert))

(defun hermes-kanban--query-for-board (slug)
  "Return a board query alist for SLUG, or nil."
  (and slug `((board . ,slug))))

(defun hermes-kanban--task-revert (&rest _)
  "Refresh the current task detail buffer in place."
  (unless hermes-kanban-task--task-id
    (user-error "No task id for this detail buffer"))
  (hermes-kanban--display-task
   (hermes-kanban--api "GET" (hermes-kanban--task-path hermes-kanban-task--task-id)
                       nil (hermes-kanban--query-for-board
                            hermes-kanban-task--board-slug))
   hermes-kanban-task--board-slug))

(defun hermes-kanban--display-task (payload &optional board-slug)
  "Render task PAYLOAD in a read-only detail buffer.
BOARD-SLUG is remembered for refreshes and log requests."
  (let* ((task (hermes-transport--get payload 'task))
         (task-id (hermes-kanban--field task 'id)))
    (with-current-buffer (get-buffer-create "*Hermes Kanban Task*")
      (unless (derived-mode-p 'hermes-kanban-task-mode)
        (hermes-kanban-task-mode))
      (setq hermes-kanban-task--task-id task-id
            hermes-kanban-task--board-slug board-slug
            mode-line-process (format " [%s]" (or task-id "task")))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (hermes-kanban--format-task-detail payload)))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun hermes-kanban-show ()
  "Show the kanban task at point."
  (interactive)
  (let ((board-slug hermes-kanban--slug))
    (hermes-kanban--display-task
     (hermes-kanban--api "GET" (hermes-kanban--task-path (hermes-kanban--id-at-point))
                         nil (hermes-kanban--query-for-board board-slug))
     board-slug)))

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

(defun hermes-kanban--log-query (board-slug)
  "Return the query alist for fetching a task log on BOARD-SLUG."
  (append (hermes-kanban--query-for-board board-slug)
          `((tail . ,hermes-kanban-log-tail-bytes))))

(defun hermes-kanban--format-log (payload)
  "Return worker-log text for PAYLOAD from GET /tasks/:id/log."
  (let ((task-id (hermes-kanban--field payload 'task_id))
        (path (hermes-kanban--field payload 'path))
        (size (hermes-transport--get payload 'size_bytes))
        (content (hermes-kanban--field payload 'content))
        (error (hermes-kanban--non-empty (hermes-kanban--field payload 'error))))
    (concat
     (format "Worker log for %s\n" (or (hermes-kanban--non-empty task-id) "task"))
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
      (t content))
     (when (hermes-kanban--truthy-p (hermes-transport--get payload 'truncated))
       (format "\n\n(showing last %s; full log path above)\n"
               (hermes-kanban--format-size hermes-kanban-log-tail-bytes))))))

(defun hermes-kanban--display-log (payload)
  "Render worker log PAYLOAD in a read-only buffer."
  (with-current-buffer (get-buffer-create "*Hermes Kanban Log*")
    (unless (derived-mode-p 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (hermes-kanban--format-log payload)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun hermes-kanban-show-log ()
  "Fetch and display the worker log for the task at point or current detail."
  (interactive)
  (let* ((id (hermes-kanban--task-id-for-command))
         (board-slug (hermes-kanban--board-slug-for-command))
         (query (hermes-kanban--log-query board-slug)))
    (hermes-kanban--display-log
     (condition-case err
         (hermes-kanban--api "GET" (hermes-kanban--task-path id "/log")
                             nil query)
       (error `((task_id . ,id)
                (error . ,(error-message-string err))))))))

;;; Task mutations

(defun hermes-kanban-edit ()
  "Edit the title and priority of the task at point."
  (interactive)
  (let* ((id (hermes-kanban--id-at-point))
         (entry (tabulated-list-get-entry))
         (title (read-string "Title: " (aref entry 3)))
         (priority (read-number "Priority: " (string-to-number (aref entry 1)))))
    (when (string-empty-p (string-trim title))
      (user-error "Title cannot be empty"))
    (hermes-kanban--api "PATCH" (hermes-kanban--task-path id)
                        `((title . ,title) (priority . ,priority))
                        (hermes-kanban--board-query))
    (hermes-kanban--revert)))

(defun hermes-kanban-assign ()
  "Assign or reassign the task at point.
Running tasks are reassigned with a reclaim; others are assigned directly."
  (interactive)
  (let* ((id (hermes-kanban--id-at-point))
         (status (aref (tabulated-list-get-entry) 0))
         (who (completing-read "Assignee (empty to unassign): "
                               hermes-kanban--assignees nil nil)))
    (if (equal status "running")
        (hermes-kanban--api "POST" (hermes-kanban--task-path id "/reassign")
                            `((profile . ,who) (reclaim_first . t))
                            (hermes-kanban--board-query))
      (hermes-kanban--api "PATCH" (hermes-kanban--task-path id)
                          `((assignee . ,who)) (hermes-kanban--board-query)))
    (hermes-kanban--revert)))

(defconst hermes-kanban--statuses
  '("todo" "ready" "blocked" "scheduled" "done" "archived" "triage")
  "Statuses settable through the dashboard PATCH endpoint.")

(defun hermes-kanban-set-status ()
  "Set the status of the task at point."
  (interactive)
  (let* ((id (hermes-kanban--id-at-point))
         (status (completing-read "Status: " hermes-kanban--statuses nil t)))
    (hermes-kanban--api "PATCH" (hermes-kanban--task-path id)
                        `((status . ,status)) (hermes-kanban--board-query))
    (hermes-kanban--revert)))

(defun hermes-kanban-comment ()
  "Append a comment to the task at point."
  (interactive)
  (let ((id (hermes-kanban--id-at-point))
        (body (read-string "Comment: ")))
    (when (string-empty-p (string-trim body))
      (user-error "Comment cannot be empty"))
    (hermes-kanban--api "POST" (hermes-kanban--task-path id "/comments")
                        `((body . ,body)) (hermes-kanban--board-query))
    (message "Comment added")))

(defun hermes-kanban-create-task ()
  "Create a task on the current board."
  (interactive)
  (let ((title (read-string "Title: "))
        (who (completing-read "Assignee (optional): "
                              hermes-kanban--assignees nil nil))
        (priority (read-number "Priority: " 0)))
    (when (string-empty-p (string-trim title))
      (user-error "Title is required"))
    (let ((body `((title . ,title) (priority . ,priority))))
      (unless (string-empty-p who)
        (setq body (append body `((assignee . ,who)))))
      (hermes-kanban--api "POST" "/tasks" body (hermes-kanban--board-query)))
    (hermes-kanban--revert)))

(defun hermes-kanban-delete ()
  "Delete the task at point after confirmation."
  (interactive)
  (let ((id (hermes-kanban--id-at-point)))
    (when (yes-or-no-p (format "Delete task %s? " id))
      (hermes-kanban--api "DELETE" (hermes-kanban--task-path id)
                          nil (hermes-kanban--board-query))
      (hermes-kanban--revert))))

;;;###autoload
(defun hermes-list-kanban ()
  "Browse Hermes Kanban boards via the dashboard API."
  (interactive)
  (hermes-kanban--render-boards))

(provide 'hermes-kanban)
;;; hermes-kanban.el ends here
