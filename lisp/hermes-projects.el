;;; hermes-projects.el --- Named workspaces for Hermes  -*- lexical-binding: t; -*-

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

;; Profile-scoped named multi-folder workspaces.  The backend owns membership,
;; paths, active selection, and session grouping; no local filesystem probing.

;;; Code:

(require 'hermes-browser)
(require 'hermes-dashboard-rpc)
(require 'hermes-sessions)

(defvar-local hermes-projects--profile nil
  "Exact backend profile, or nil for the owning dashboard's launch scope.")
(defvar-local hermes-projects--snapshot nil
  "Last authoritative projects.list result.")
(defvar-local hermes-projects--detail-id nil
  "Exact stored project ID in a detail buffer.")
(defvar-local hermes-projects--unsupported nil
  "Methods explicitly reported missing by this backend.")

(defun hermes-projects--true-p (value)
  "Return non-nil for a backend true flag VALUE."
  (memq value '(t 1)))

(defun hermes-projects--field (object key)
  "Return OBJECT's KEY as display text."
  (hermes-transport--display-field object key))

(defun hermes-projects--missing-p (reason method)
  "Return non-nil if REASON explicitly reports missing METHOD."
  (or (equal (hermes-transport--get reason 'code) -32601)
      (equal reason (concat "unknown method: projects." method))))

(defun hermes-projects--rpc (client method &rest args)
  "Call typed projects METHOD on CLIENT with keyword ARGS.
Validate explicit profiles against this backend before each dispatch.  This
point-in-time check cannot prevent backend profile deletion during dispatch."
  (let* ((profile (plist-get args :profile))
         (guard hermes-dashboard-transport-dispatch-guard)
         (token hermes-dashboard-transport-request-owner)
         (dispatch
          (lambda ()
            (when (and guard (not (funcall guard)))
              (error "Retired project operation"))
            (let ((hermes-dashboard-transport-dispatch-guard guard)
                  (hermes-dashboard-transport-request-owner token))
              (apply #'hermes-dashboard-transport-call-fn
                     (intern (concat "hermes-dashboard-transport-projects-"
                                     (replace-regexp-in-string "_" "-" method)))
                     client args)))))
    ;; Omitting the override is unambiguously the captured client's launch
    ;; scope.  Never substitute "default" for a named dashboard's launch scope.
    (if (null profile) (funcall dispatch)
      (hermes--promise-then
       (hermes-dashboard-transport-api-request-async
        "GET" "/api/profiles" :client client :current-p guard)
       (lambda (catalogue)
         (unless (seq-some (lambda (row)
                            (equal profile (hermes-transport--get row 'name)))
                          (hermes-transport--get catalogue 'profiles))
           (error "Profile %S is not in this backend's catalogue" profile))
         (funcall dispatch))))))

(defun hermes-projects--rows (result)
  "Project RESULT into profile-qualified native list rows."
  (mapcar
   (lambda (project)
     (let ((id (hermes-projects--field project 'id)))
       (list (cons hermes-projects--profile id)
             (vector
              (hermes-browser--face-cell (hermes-projects--field project 'name)
                                         'hermes-browser-title)
              (hermes-browser--status-cell
               (if (equal id (hermes-transport--get result 'active_id)) "active" ""))
              (hermes-browser--status-cell
               (if (hermes-projects--true-p (hermes-transport--get project 'archived))
                   "archived" "open"))
              (number-to-string (length (hermes-transport--get project 'folders)))
              (hermes-projects--field project 'primary_path)))))
   (hermes-transport--get result 'projects)))

(defun hermes-projects--description ()
  "Return this project view's profile and active selection."
  (let* ((active (hermes-transport--get hermes-projects--snapshot 'active_id))
         (project (seq-find
                   (lambda (row) (equal active (hermes-transport--get row 'id)))
                   (hermes-transport--get hermes-projects--snapshot 'projects))))
    (format "Projects · %s · Active: %s"
            (or hermes-projects--profile "dashboard launch profile")
            (cond (project (hermes-projects--field project 'name))
                  (active "unlisted project")
                  (hermes-projects--snapshot "none") (t "unknown")))))


(defun hermes-projects--primary-label ()
  "Return the primary-folder action with its bounded current value."
  (let ((path (condition-case nil
                  (hermes-projects--field (hermes-projects--selected) 'primary_path)
                (user-error "Unknown"))))
    (concat "Set primary: "
            (propertize (truncate-string-to-width path 24 nil nil t)
                        'face 'keymap-popup-value 'help-echo path))))

(defun hermes-projects--fetch (client)
  "Read this profile's projects from CLIENT."
  (hermes-projects--rpc client "list" :profile hermes-projects--profile))

;;;###autoload (autoload 'hermes-list-projects "hermes-projects" nil t)
(hermes-define-list-browser projects
			    :title "Hermes Projects"
			    :buffer "*Hermes Projects*"
			    :columns [("Project" 26 t) ("Active" 8 t) ("State" 10 t)
				      ("Folders" 8 t) ("Primary backend folder" 40 t)]
			    :description #'hermes-projects--description
			    :fetch #'hermes-projects--fetch
			    :refresh #'hermes-projects-refresh
			    :rows #'hermes-projects--rows
			    :on-result (lambda (result) (setq hermes-projects--snapshot result))
			    :on-mode (lambda ()
				       (setq-local hermes-browser--snapshot-variables
						   '(hermes-projects--snapshot hermes-projects--unsupported)))
			    :help (:group "Open"
					  hermes-projects-view "Details"
					  hermes-projects-sessions "Grouped sessions"
					  hermes-projects-choose "Choose project"
					  :group "Organise"
					  hermes-projects-create "Create"
					  hermes-projects-rename "Rename"
					  hermes-projects-set-active "Set active (no session move)"
					  hermes-projects-clear-active "Clear active"
					  :row
					  :group "Folders"
					  hermes-projects-add-folder "Add backend path"
					  hermes-projects-remove-folder "Remove backend path"
					  hermes-projects-set-primary #'hermes-projects--primary-label
					  :group "Lifecycle"
					  hermes-projects-archive "Archive"
					  hermes-projects-restore "Restore"
					  hermes-projects-delete "Delete metadata"
					  hermes-projects-profile "Change profile")
			    :keys ("RET" #'hermes-projects-view "f" #'hermes-projects-view
				   "b" #'quit-window "c" #'hermes-projects-create
				   "r" #'hermes-projects-rename "a" #'hermes-projects-archive
				   "u" #'hermes-projects-restore "d" #'hermes-projects-delete
				   "+" #'hermes-projects-add-folder "-" #'hermes-projects-remove-folder
				   "!" #'hermes-projects-set-primary "s" #'hermes-projects-sessions
				   "*" #'hermes-projects-set-active "0" #'hermes-projects-clear-active
				   "j" #'hermes-projects-choose "P" #'hermes-projects-profile))

(defvar-keymap hermes-project-detail-mode-map
  :parent hermes-projects-mode-map
  "g" #'hermes-projects-refresh
  "RET" #'push-button
  "n" #'forward-line "p" #'previous-line
  "f" #'forward-char "b" #'quit-window)


(keymap-popup-annotate hermes-project-detail-mode-map
		       :popup-key "?" :exit-key "C-g" :description #'hermes-projects--description
		       :group "Open"
		       hermes-projects-sessions "Grouped sessions"
		       hermes-projects-choose "Choose project"
		       :group "Organise"
		       hermes-projects-create "Create"
		       hermes-projects-rename "Rename"
		       hermes-projects-set-active "Set active (no session move)"
		       hermes-projects-clear-active "Clear active"
		       :row
		       :group "Folders"
		       hermes-projects-add-folder "Add backend path"
		       hermes-projects-remove-folder "Remove backend path"
		       hermes-projects-set-primary #'hermes-projects--primary-label
		       :group "Lifecycle"
		       hermes-projects-archive "Archive"
		       hermes-projects-restore "Restore"
		       hermes-projects-delete "Delete metadata"
		       :group "View"
		       hermes-projects-profile "Change profile"
		       hermes-projects-refresh "Refresh"
		       quit-window "Quit view")

(define-derived-mode hermes-project-detail-mode special-mode "Hermes Project"
  "Inspect backend project metadata and folders."
  (hermes-browser--setup-status))

(defun hermes-projects--selected ()
  "Return the selected authoritative stored project, or signal an error."
  (let* ((id (or hermes-projects--detail-id (cdr (tabulated-list-get-id))))
         (project (seq-find
                   (lambda (row) (equal id (hermes-transport--get row 'id)))
                   (hermes-transport--get hermes-projects--snapshot 'projects))))
    (or project (user-error "No stored project selected; refresh or create one"))))

(defun hermes-projects--context (&optional selected)
  "Capture an ownership predicate, optionally for SELECTED project."
  (let ((buffer (current-buffer))
        (profile (copy-sequence hermes-projects--profile))
        (owner (hermes-browser--dispatch-guard nil))
        (detail-id hermes-projects--detail-id)
        (id (and selected (hermes-projects--field selected 'id))))
    (lambda ()
      (and (funcall owner)
           (with-current-buffer buffer
             (and (equal profile hermes-projects--profile)
                  (equal detail-id hermes-projects--detail-id)
                  (or (null id)
                      (equal id (condition-case nil
                                    (hermes-projects--field
                                     (hermes-projects--selected) 'id)
                                  (user-error nil))))))))))

(defun hermes-projects--error (reason method)
  "Render failure REASON for METHOD without misclassifying transient errors."
  (if (hermes-projects--missing-p reason method)
      (progn
        (cl-pushnew method hermes-projects--unsupported :test #'equal)
        (setq hermes-browser--status
              (format "Unsupported: projects.%s; g retry, b back" method))
        (message "Hermes backend does not support projects.%s" method))
    (hermes-browser--read-error reason)))

(defun hermes-projects--render-detail ()
  "Render this detail's authoritative project, retaining the reading position."
  (hermes-browser--preserve-reading-position
   (lambda ()
     (let ((inhibit-read-only t)
           (project (condition-case nil (hermes-projects--selected) (user-error nil))))
       (erase-buffer)
       (insert (propertize (hermes-projects--description) 'face 'hermes-browser-title) "\n\n")
       (if (not project)
           (insert "Project metadata is absent. Directories and sessions are not deleted.\n")
         (insert (propertize (hermes-projects--field project 'name) 'face 'bold) "\n"
                 (hermes-projects--field project 'description) "\n"
                 "State: " (if (hermes-projects--true-p
				(hermes-transport--get project 'archived)) "archived" "open")
                 "\nPrimary backend folder: " (hermes-projects--field project 'primary_path)
                 "\n\nBackend folders (metadata; existence not verified):\n")
         (dolist (folder (hermes-transport--get project 'folders))
           (insert (if (hermes-projects--true-p (hermes-transport--get folder 'is_primary))
                       "  * " "    ")
                   (hermes-projects--field folder 'path) "  "
                   (hermes-projects--field folder 'label) "\n"))
         (insert "\n")
         (insert-text-button "Grouped sessions (bounded subset)"
                             'action (lambda (_) (hermes-projects-sessions)))
         (insert "\nActive project selection does not move sessions.\n"
                 "Change an attached chat's directory with its Set directory command;\n"
                 "stored-session moves are unavailable (cross-profile live-ID ambiguity).\n"))))))

(defun hermes-projects--accept (result)
  "Install authoritative project RESULT in this view."
  (setq hermes-projects--snapshot result)
  (if (derived-mode-p 'hermes-project-detail-mode)
      (hermes-projects--render-detail)
    (hermes-projects--render result))
  (setq hermes-browser--status "Ready"))

(defun hermes-projects-refresh ()
  "Refresh this profile's authoritative projects, including detail metadata."
  (interactive)
  (hermes-browser--next-request-generation)
  (let ((profile hermes-projects--profile)
        (id hermes-projects--detail-id))
    (setq hermes-browser--status "Loading")
    (hermes-browser--run-owned
     (lambda (client _guard)
       (if (not id) (hermes-projects--rpc client "list" :profile profile)
         (hermes--promise-then
          (hermes--promise-all
           (list (hermes-projects--rpc client "list" :profile profile)
                 (hermes-projects--rpc client "get" :id id :profile profile)))
          (lambda (results)
            (let ((project (hermes-transport--get (cadr results) 'project)))
              (unless (equal id (hermes-transport--get project 'id))
                (error "Project readback returned another project"))
              `((active_id . ,(hermes-transport--get (car results) 'active_id))
                (projects . ,(cons project
                                   (seq-remove
                                    (lambda (row) (equal id (hermes-transport--get row 'id)))
                                    (hermes-transport--get (car results) 'projects))))))))))
     (hermes-projects--context)
     (lambda (result)
       (setq hermes-projects--unsupported nil)
       (hermes-projects--accept result))
     (lambda (reason) (hermes-projects--error reason (if id "get" "list"))))))

(defun hermes-projects-view ()
  "Open the selected project's native detail buffer without changing sessions."
  (interactive)
  (hermes-projects--view-project (hermes-projects--selected)))

(defun hermes-projects--view-project (project)
  "Open authoritative PROJECT details, retaining this view's profile scope."
  (let ((profile hermes-projects--profile)
        (instance hermes-instance)
        (snapshot hermes-projects--snapshot)
        (target (generate-new-buffer "*Hermes Project*")))
    (with-current-buffer target
      (hermes-project-detail-mode)
      (hermes-browser--own-instance instance)
      (setq hermes-projects--profile profile
            hermes-projects--detail-id (hermes-projects--field project 'id)
            hermes-projects--snapshot snapshot)
      (hermes-projects--render-detail))
    (pop-to-buffer target)
    (hermes-projects-refresh)))

(defun hermes-projects--choices (projects)
  "Return unique human-title completion labels paired with exact PROJECTS."
  (let ((names (mapcar (lambda (row) (hermes-projects--field row 'name)) projects))
        (used (make-hash-table :test #'equal)))
    (mapcar
     (lambda (project)
       (let* ((name (hermes-projects--field project 'name))
              (base (if (> (cl-count name names :test #'equal) 1)
                        (format "%s [%s]" name (hermes-projects--field project 'id)) name))
              (label base))
         (while (or (gethash label used)
                    (and (not (equal label name)) (member label names)))
           (setq label (concat label " ·")))
         (puthash label t used)
         (cons label project))) projects)))

(defun hermes-projects-choose ()
  "Choose a project by human title and open its details."
  (interactive)
  (let* ((buffer (current-buffer))
         (current (hermes-projects--context))
         (choices (hermes-projects--choices
                   (hermes-transport--get hermes-projects--snapshot 'projects)))
         (choice (cdr (assoc (completing-read "Project: " choices nil t) choices))))
    (when (and choice (funcall current))
      (with-current-buffer buffer
        (hermes-projects--view-project choice)))))

(defun hermes-projects-profile ()
  "Choose an exact backend profile without consulting local directories."
  (interactive)
  (let* ((buffer (current-buffer))
         (current (hermes-projects--context))
         (profile (read-string "Backend profile: " hermes-projects--profile)))
    (when (and (funcall current) (not (string-empty-p profile)))
      (with-current-buffer buffer
	(hermes-browser--next-request-generation)
	(when (derived-mode-p 'hermes-project-detail-mode)
          (let ((instance hermes-instance))
            (hermes-projects-mode)
            (hermes-browser--own-instance instance)))
	(setq hermes-projects--profile profile hermes-projects--detail-id nil
              hermes-projects--snapshot nil hermes-projects--unsupported nil)
	(when (derived-mode-p 'hermes-projects-mode)
          (hermes-projects--render nil))
	(hermes-projects-refresh)))))

(defun hermes-projects--mutate (method prompt &optional unselected)
  "Run METHOD using PROMPT's keyword arguments, optionally UNSELECTED.
PROMPT receives the selected project, or nil.  Capture ownership before input;
read back projects.list after any write outcome, without retrying the write."
  (when hermes-browser--owned-cleanup
    (user-error "Project request pending; refresh to reconcile before another change"))
  (when (member method hermes-projects--unsupported)
    (user-error "Backend does not support projects.%s; refresh or change profile" method))
  (let* ((project (unless unselected (hermes-projects--selected)))
         (profile hermes-projects--profile)
         (buffer (current-buffer))
         (current (hermes-projects--context project))
         (args (funcall prompt project)))
    (when (and (not (eq args 'cancel)) (funcall current))
      (with-current-buffer buffer
	(hermes-browser--next-request-generation)
	(let ((current (hermes-projects--context project))
              problem)
          (setq hermes-browser--status "Saving")
          (hermes-browser--run-owned
           (lambda (client guard)
             (let ((token hermes-dashboard-transport-request-owner))
               (hermes--promise-then
		(hermes--promise-catch
		 (apply #'hermes-projects--rpc client method
			:profile profile (append (and project
                                                      (list :id (hermes-projects--field project 'id)))
						 args))
		 (lambda (reason) (setq problem reason) nil))
		(lambda (_receipt)
		  (if (funcall guard)
                      (let ((hermes-dashboard-transport-dispatch-guard guard)
                            (hermes-dashboard-transport-request-owner token))
			(hermes-projects--rpc client "list" :profile profile))
                    (hermes--promise-rejected "Retired project readback"))))))
           current
           (lambda (result)
             (hermes-projects--accept result)
             (when problem (hermes-projects--error problem method)))
           (lambda (reason) (hermes-projects--error reason method))))))))

(defun hermes-projects-create ()
  "Create named workspace metadata with an optional backend folder."
  (interactive)
  (hermes-projects--mutate
   "create" (lambda (_)
              (let ((name (read-string "Project name: "))
                    (path (read-string "Backend folder (blank for none): ")))
                (when (string-empty-p name) (user-error "Project name required"))
                (list :name name :folders (if (string-empty-p path) [] (vector path))
                      :use :false))) t))

(defun hermes-projects-rename ()
  "Rename selected workspace metadata without changing directories or sessions."
  (interactive)
  (hermes-projects--mutate
   "update" (lambda (project)
              (let ((name (read-string "Project name: " (hermes-projects--field project 'name))))
                (when (string-empty-p name) (user-error "Project name required"))
                (list :name name)))))

(defun hermes-projects--folder-prompt (project)
  "Read an exact backend folder path for PROJECT, without local completion."
  (let ((path (read-string "Backend folder: " (hermes-projects--field project 'primary_path))))
    (when (string-empty-p path) (user-error "Backend path required"))
    (list :path path)))

(defun hermes-projects-add-folder ()
  "Add backend folder metadata; this does not create a directory."
  (interactive)
  (hermes-projects--mutate "add_folder" #'hermes-projects--folder-prompt))

(defun hermes-projects-remove-folder ()
  "Remove folder metadata without deleting the directory or sessions."
  (interactive)
  (hermes-projects--mutate "remove_folder" #'hermes-projects--folder-prompt))

(defun hermes-projects-set-primary ()
  "Set a primary backend folder without moving existing sessions."
  (interactive)
  (hermes-projects--mutate "set_primary" #'hermes-projects--folder-prompt))

(defun hermes-projects-archive ()
  "Archive selected workspace metadata."
  (interactive)
  (hermes-projects--mutate "archive" (lambda (_) '(:restore :false))))

(defun hermes-projects-restore ()
  "Restore selected archived workspace metadata."
  (interactive)
  (hermes-projects--mutate "archive" (lambda (_) '(:restore t))))

(defun hermes-projects-delete ()
  "Delete workspace metadata, never its directories or stored sessions."
  (interactive)
  (hermes-projects--mutate
   "delete" (lambda (project)
              (unless (yes-or-no-p
                       (format "Delete project %s metadata only (keep directories and sessions)? "
                               (hermes-projects--field project 'name)))
                'cancel))))

(defun hermes-projects-set-active ()
  "Select the active workspace; do not move or attach any session."
  (interactive)
  (hermes-projects--mutate "set_active" (lambda (_) nil)))

(defun hermes-projects-clear-active ()
  "Clear active project metadata without changing session directories."
  (interactive)
  (hermes-projects--mutate "set_active" (lambda (_) nil) t))


;;; Bounded backend session groups

(defvar-local hermes-project-sessions--project-id nil
  "Stored project ID requested from backend session grouping.")
(defvar-local hermes-project-sessions--map nil
  "Profile-qualified session records in the grouped view.")

(defun hermes-project-sessions--records (result)
  "Flatten RESULT's backend groups without inferring membership locally."
  (cl-loop for repo in (hermes-transport--get
			(hermes-transport--get result 'project) 'repos)
           append
           (cl-loop for group in (hermes-transport--get repo 'groups)
                    append
                    (mapcar
                     (lambda (session)
                       (hermes-sessions--with-field
                        session 'workspace_group
                        (format "%s / %s"
                                (hermes-projects--field repo 'label)
                                (hermes-projects--field group 'label))))
                     (hermes-transport--get group 'sessions)))))

(defun hermes-project-sessions--fetch (client)
  "Read the backend's bounded session grouping from CLIENT."
  (hermes-projects--rpc
   client "project_sessions" :profile hermes-projects--profile
   :project-id hermes-project-sessions--project-id :session-limit 5000))

(defun hermes-project-sessions--rows (result)
  "Return native rows for backend grouped RESULT."
  (mapcar
   (lambda (session)
     (list (hermes-sessions--identity session)
           (vector (hermes-projects--field session 'title)
                   (hermes-projects--field session 'workspace_group)
                   (hermes-projects--field session 'cwd)
                   (hermes-projects--field session 'profile))))
   (hermes-project-sessions--records result)))

(hermes-define-list-browser project-sessions
			    :title "Project Sessions"
			    :description "Project sessions · bounded subset"
			    :buffer "*Hermes Project Sessions*"
			    :columns [("Session" 28 t) ("Backend group" 30 t)
				      ("Backend directory" 35 t) ("Profile" 14 t)]
			    :fetch #'hermes-project-sessions--fetch
			    :refresh #'hermes-project-sessions-refresh
			    :rows #'hermes-project-sessions--rows
			    :on-result (lambda (result)
					 (setq hermes-project-sessions--map
					       (hermes-sessions--sessions-by-id
						(hermes-project-sessions--records result)))
					 (setq hermes-browser--status
					       (if (hermes-transport--get result 'project)
						   "Bounded subset: up to 5000 recent sessions; exclusions apply"
						 "No matching group in the bounded backend subset")))
			    :help (:group "Session"
					  hermes-project-sessions-open "Resume chat")
			    :keys ("RET" #'hermes-project-sessions-open "f" #'hermes-project-sessions-open
				   "b" #'quit-window))


(defun hermes-project-sessions-refresh ()
  "Refresh this project's bounded backend grouping under its exact owner."
  (interactive)
  (unless hermes-project-sessions--project-id
    (user-error "Open a project's Grouped sessions action first"))
  (hermes-browser--next-request-generation)
  (let ((profile hermes-projects--profile)
        (id hermes-project-sessions--project-id)
        (buffer (current-buffer))
        (owner (hermes-browser--dispatch-guard nil)))
    (setq hermes-browser--status "Loading")
    (hermes-browser--run-owned
     (lambda (client _guard)
       (hermes-projects--rpc client "project_sessions" :profile profile
                             :project-id id :session-limit 5000))
     (lambda ()
       (and (funcall owner)
            (equal profile (buffer-local-value 'hermes-projects--profile buffer))
            (equal id (buffer-local-value 'hermes-project-sessions--project-id buffer))))
     #'hermes-project-sessions--render
     (lambda (reason) (hermes-projects--error reason "project_sessions")))))

(defun hermes-project-sessions-open ()
  "Resume the exact profile-qualified session at point."
  (interactive)
  (let ((session (and hermes-project-sessions--map
                      (gethash (tabulated-list-get-id) hermes-project-sessions--map))))
    (unless session (user-error "No session selected"))
    (hermes-chat-resume-session
     (hermes-sessions--id session) (hermes-projects--field session 'title)
     (hermes-sessions--profile session) hermes-instance)))

(defun hermes-projects-sessions ()
  "Show backend-grouped sessions as a bounded subset, not a complete archive."
  (interactive)
  (let ((project (hermes-projects--selected))
        (profile hermes-projects--profile)
        (instance hermes-instance)
        (target (generate-new-buffer "*Hermes Project Sessions*")))
    (with-current-buffer target
      (hermes-project-sessions-mode)
      (hermes-browser--own-instance instance)
      (setq hermes-projects--profile profile
            hermes-project-sessions--project-id (hermes-projects--field project 'id)))
    (pop-to-buffer target)
    (hermes-project-sessions--revert)))

(provide 'hermes-projects)
;;; hermes-projects.el ends here
