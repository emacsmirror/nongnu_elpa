;;; hermes-tracker.el --- Optional Tracker cockpit for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

;;; Commentary:

;; Optional, lazy Emacs-native client for the separate hermes-tracker service.
;; Tracker credentials come from auth-source and never share dashboard auth.
;; Repository/TODO browsing and safe mutations are asynchronous.  This module
;; also owns the canonical Tracker reference embedded in Hermes Kanban cards.

;;; Code:

(require 'auth-source)
(require 'button)
(require 'cl-lib)
(require 'json)
(require 'outline)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-parse)
(require 'url-util)
(require 'keymap-popup)
(require 'hermes-browser)
(require 'hermes-dashboard-api)
(require 'hermes-promise)
(require 'hermes-transport)

(defgroup hermes-tracker nil
  "Optional integration with the Hermes Tracker service."
  :group 'hermes)

(defcustom hermes-tracker-url nil
  "Base URL of the Hermes Tracker service, or nil to disable it."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'hermes-tracker)

(defcustom hermes-tracker-auth-source-user "hermes-tracker-token"
  "Auth-source user used to find the Tracker bearer credential."
  :type 'string
  :group 'hermes-tracker)

(defcustom hermes-tracker-auth-source-port "hermes-tracker"
  "Auth-source port used to find the Tracker bearer credential."
  :type 'string
  :group 'hermes-tracker)

(defcustom hermes-tracker-actor user-login-name
  "Actor sent with Tracker mutations."
  :type 'string
  :group 'hermes-tracker)

(defconst hermes-tracker--reference-regexp
  "```tracker-ref[[:space:]]*\n\\(.*?\\)\n```"
  "Regexp matching a fenced Tracker reference block.")

(defvar-local hermes-tracker--repo-slug nil)
(defvar-local hermes-tracker--repo-name nil)
(defvar-local hermes-tracker--status-filter nil)
(defvar-local hermes-tracker--todo-number nil)
(defvar-local hermes-tracker--todo nil)
(defvar-local hermes-tracker--linked-cards nil)

(defun hermes-tracker--normalize-base-url (value)
  "Return validated Tracker base URL from VALUE, or nil when empty."
  (and-let* ((text (and (stringp value) (string-trim value)))
             ((not (string-empty-p text))))
    (let ((parsed (url-generic-parse-url text)))
      (unless (member (url-type parsed) '("http" "https"))
        (user-error "Hermes Tracker URL must start with http:// or https://"))
      (unless (url-host parsed)
        (user-error "Hermes Tracker URL must include a host"))
      (when (or (url-user parsed) (url-password parsed))
        (user-error "Hermes Tracker URL must not include credentials"))
      (when (or (cdr (url-path-and-query parsed)) (url-target parsed))
        (user-error "Hermes Tracker URL must not include query or fragment")))
    (replace-regexp-in-string "/+\\'" "" text)))

(defun hermes-tracker--base-url ()
  "Return configured Tracker base URL or signal an actionable error."
  (or (hermes-tracker--normalize-base-url hermes-tracker-url)
      (user-error "Set `hermes-tracker-url' to enable Hermes Tracker")))

(defun hermes-tracker--auth-hosts (base-url)
  "Return auth-source host candidates for BASE-URL."
  (let* ((parsed (url-generic-parse-url base-url))
         (host (url-host parsed))
         (port (url-port parsed)))
    (cl-remove-duplicates
     (delq nil (list base-url (and host port (format "%s:%d" host port)) host))
     :test #'equal)))

(defun hermes-tracker--token (base-url)
  "Return Tracker bearer token for BASE-URL from auth-source."
  (or (catch 'token
        (dolist (host (hermes-tracker--auth-hosts base-url))
          (when-let* ((entry (car (auth-source-search
                                   :host host
                                   :user hermes-tracker-auth-source-user
                                   :port hermes-tracker-auth-source-port
                                   :require '(:secret) :max 1)))
                      (secret (plist-get entry :secret))
                      (value (if (functionp secret) (funcall secret) secret))
                      (token (hermes-transport--non-empty-string value)))
            (throw 'token token))))
      (user-error
       "No Hermes Tracker token found in auth-source for %s/%s"
       hermes-tracker-auth-source-user hermes-tracker-auth-source-port)))

(defun hermes-tracker--idempotency-key (method path actor body invocation)
  "Return stable mutation key for METHOD PATH ACTOR BODY and INVOCATION."
  (concat "emacs-hermes-"
          (secure-hash 'sha256
                       (mapconcat #'identity
                                  (list invocation method path actor
                                        (json-serialize (or body '())))
                                  "\n"))))

(defun hermes-tracker--request (method path &optional body invocation)
  "Return authenticated request plist for METHOD PATH BODY and INVOCATION."
  (let* ((base-url (hermes-tracker--base-url))
         (token (hermes-tracker--token base-url))
         (actor (or (hermes-transport--non-empty-string hermes-tracker-actor)
                    user-login-name))
         (mutation-p (not (equal method "GET")))
         (headers (append
                   `(("Authorization" . ,(concat "Bearer " token)))
                   (and body '(("Content-Type" . "application/json")))
                   (and mutation-p `(("X-Actor" . ,actor)))
                   (and invocation
                        `(("Idempotency-Key"
                           . ,(hermes-tracker--idempotency-key
                               method path actor body invocation)))))))
    (list :url (concat base-url path) :method method :headers headers
          :body body :secrets (list token))))

(defun hermes-tracker--safe-request-description (request)
  "Return safe description of REQUEST without headers or secrets."
  (format "%s %s" (plist-get request :method) (plist-get request :url)))

(defun hermes-tracker--request-async (method path &optional body invocation)
  "Return promise of Tracker response body for METHOD PATH BODY and INVOCATION."
  (hermes--promise-map
   (hermes-dashboard-transport--http-json-request-async
    (hermes-tracker--request method path body invocation))
   (lambda (response) (plist-get response :body))))

(defun hermes-tracker--api-path (&rest segments)
  "Return Tracker API path from percent-encoded SEGMENTS."
  (concat "/api/v1/"
          (mapconcat (lambda (segment) (url-hexify-string (format "%s" segment)))
                     segments "/")))

(defun hermes-tracker--items (value)
  "Return VALUE as a list."
  (cond ((vectorp value) (append value nil)) ((listp value) value)))

(defun hermes-tracker--display (object field &optional fallback)
  "Return OBJECT FIELD as a display string, or FALLBACK."
  (or (hermes-transport--non-empty-string
       (hermes-transport--display-field object field))
      fallback ""))

(defun hermes-tracker--repository-rows (repositories)
  "Return tabulated rows for REPOSITORIES."
  (mapcar (lambda (repo)
            (let ((slug (hermes-tracker--display repo 'slug)))
              (list slug (vector (hermes-tracker--display repo 'name slug)
                                 slug
                                 (hermes-tracker--display repo 'description)))))
          (hermes-tracker--items repositories)))

(defun hermes-tracker--todo-rows (payload)
  "Return tabulated TODO rows from PAYLOAD."
  (mapcar (lambda (todo)
            (let ((number (hermes-transport--get todo 'number)))
              (list number
                    (vector (format "%s" number)
                            (hermes-tracker--display todo 'status)
                            (hermes-tracker--display todo 'priority "0")
                            (hermes-tracker--display todo 'assignee "-")
                            (hermes-tracker--display todo 'title)))))
          (hermes-tracker--items (hermes-transport--get payload 'todos))))

(defun hermes-tracker--format-list (title values empty)
  "Return Markdown TITLE section for VALUES, or EMPTY text."
  (format "\n## %s (%d)\n\n%s\n" title (length (hermes-tracker--items values))
          (if-let* ((items (hermes-tracker--items values)))
              (string-join (mapcar (lambda (item) (format "- %s" item)) items) "\n")
            (format "— no %s —" empty))))

(defun hermes-tracker--format-link (link)
  "Return one TODO LINK row."
  (format "- %s %s#%s — %s"
          (hermes-tracker--display link 'link_type)
          (hermes-tracker--display link 'target_repo_slug)
          (hermes-tracker--display link 'target_number)
          (hermes-tracker--display link 'target_title)))

(defun hermes-tracker--format-comment (comment)
  "Return one TODO COMMENT section."
  (format "### %s — %s\n\n%s"
          (hermes-tracker--display comment 'created_at)
          (hermes-tracker--display comment 'author "anon")
          (hermes-tracker--display comment 'body)))

(defun hermes-tracker--format-todo-detail (todo)
  "Return rich Markdown text for TODO."
  (let ((links (hermes-tracker--items (hermes-transport--get todo 'links)))
        (comments (hermes-tracker--items (hermes-transport--get todo 'comments))))
    (concat
     (format "# %s\n\n- TODO: `%s#%s`\n- Status: `%s`\n- Priority: `%s`\n- Assignee: `%s`\n"
             (hermes-tracker--display todo 'title)
             (hermes-tracker--display todo 'repo_slug)
             (hermes-tracker--display todo 'number)
             (hermes-tracker--display todo 'status)
             (hermes-tracker--display todo 'priority "0")
             (hermes-tracker--display todo 'assignee "-"))
     (when-let* ((commit (hermes-tracker--display todo 'closing_commit nil)))
       (format "- Closing commit: `%s`\n" commit))
     (when-let* ((closed-by (hermes-tracker--display todo 'closed_by nil)))
       (format "- Closed by: %s\n" closed-by))
     (when-let* ((closed-at (hermes-tracker--display todo 'closed_at nil)))
       (format "- Closed at: %s\n" closed-at))
     "\n## Description\n\n"
     (hermes-tracker--display todo 'description "— no description —") "\n"
     (hermes-tracker--format-list
      "Acceptance criteria" (hermes-transport--get todo 'acceptance_criteria) "criteria")
     (hermes-tracker--format-list
      "Verification commands" (hermes-transport--get todo 'verification_commands) "commands")
     "\n## Verification evidence\n\n"
     (hermes-tracker--display todo 'verification_output "— no evidence —") "\n"
     (format "\n## Links (%d)\n\n%s\n" (length links)
             (if links (string-join (mapcar #'hermes-tracker--format-link links) "\n")
               "— no links —"))
     (format "\n## Comments (%d)\n\n%s\n" (length comments)
             (if comments
                 (string-join (mapcar #'hermes-tracker--format-comment comments) "\n\n")
               "— no comments —")))))

(defun hermes-tracker-parse-reference (body)
  "Return canonical Tracker reference plist parsed from Kanban BODY."
  (when (stringp body)
    (let ((start 0) payloads)
      (while (string-match hermes-tracker--reference-regexp body start)
        (push (match-string 1 body) payloads)
        (setq start (match-end 0)))
      (when (= (length payloads) 1)
        (condition-case nil
            (let* ((object (json-parse-string (car payloads) :object-type 'alist))
                   (keys (mapcar #'car object))
                   (slug (cdr (assq 'repo_slug object)))
                   (number (cdr (assq 'number object))))
              (and (equal (sort keys (lambda (a b) (string-lessp (symbol-name a)
                                                                    (symbol-name b))))
                          '(number repo_slug))
                   (stringp slug) (not (string-empty-p (string-trim slug)))
                   (integerp number) (> number 0)
                   (list :repo-slug (string-trim slug) :number number)))
          (error nil))))))

(defun hermes-tracker-render-reference (repo-slug number)
  "Return canonical fenced reference for REPO-SLUG TODO NUMBER."
  (unless (and (stringp repo-slug) (not (string-empty-p (string-trim repo-slug)))
               (integerp number) (> number 0))
    (user-error "Tracker reference requires repository slug and positive TODO number"))
  (format "```tracker-ref\n%s\n```"
          (json-serialize `((number . ,number) (repo_slug . ,(string-trim repo-slug))))))

(defun hermes-tracker--body-with-reference (body repo-slug number)
  "Return BODY with canonical REPO-SLUG NUMBER reference appended."
  (let ((body (or body "")))
    (when (string-match-p "```tracker-ref" body)
      (user-error "Kanban card already contains a Tracker reference"))
    (concat (string-trim-right body)
            (unless (string-empty-p (string-trim body)) "\n\n")
            (hermes-tracker-render-reference repo-slug number))))

(defun hermes-tracker--kanban-references (todo)
  "Return TODO's canonical Hermes Kanban external references."
  (seq-filter
   (lambda (reference)
     (and (equal (hermes-tracker--display reference 'system) "hermes-kanban")
          (string-match-p "\\`t_[0-9a-f]\\{8\\}\\'"
                          (hermes-tracker--display reference 'external_id))))
   (hermes-tracker--items (hermes-transport--get todo 'external_references))))

(defun hermes-tracker--supersession (todo task-id)
  "Return TASK-ID supersession record from TODO."
  (seq-find (lambda (item)
              (equal (hermes-tracker--display item 'task_id) task-id))
            (hermes-tracker--items
             (hermes-transport--get todo 'superseded_kanban_executions))))

(defun hermes-tracker--linked-card (todo reference payload)
  "Return normalized linked card from TODO REFERENCE and Kanban PAYLOAD."
  (let* ((task-id (hermes-tracker--display reference 'external_id))
         (metadata (hermes-transport--get reference 'metadata))
         (task (and payload (hermes-transport--get payload 'task)))
         (card-ref (and task (hermes-tracker-parse-reference
                              (hermes-tracker--display task 'body))))
         (expected (list :repo-slug (hermes-tracker--display todo 'repo_slug)
                         :number (hermes-transport--get todo 'number)))
         (supersession (hermes-tracker--supersession todo task-id))
         (drift (cond ((null task) 'missing-card)
                      ((null card-ref) 'missing-card-backlink)
                      ((not (equal card-ref expected)) 'mismatched-card-backlink)
                      (t 'none))))
    (list :task-id task-id
          :board (hermes-tracker--display metadata 'board "default")
          :profile (hermes-tracker--display metadata 'profile "-")
          :title (and task (hermes-tracker--display task 'title task-id))
          :status (and task (hermes-tracker--display task 'status))
          :assignee (and task (hermes-tracker--display task 'assignee "-"))
          :payload payload :drift drift
          :execution-state (and supersession 'superseded)
          :replacement-id (and supersession
                               (hermes-tracker--display supersession
                                                        'replacement_kanban_id nil)))))

(defun hermes-tracker--linked-card-line (card)
  "Return display line for linked CARD."
  (format "- `%s` [%s] %s — %s @%s%s%s"
          (plist-get card :task-id) (plist-get card :board)
          (or (plist-get card :title) "unavailable")
          (or (plist-get card :status) "missing")
          (or (plist-get card :assignee) (plist-get card :profile))
          (if (eq (plist-get card :drift) 'none) ""
            (format " — drift: %s" (plist-get card :drift)))
          (if (plist-get card :execution-state)
              (format " — superseded%s"
                      (if-let* ((replacement (plist-get card :replacement-id)))
                          (format " by %s" replacement) ""))
            "")))

(defun hermes-tracker--fetch-linked-card (todo reference)
  "Return promise resolving TODO REFERENCE to a linked-card record."
  (let* ((id (hermes-tracker--display reference 'external_id))
         (metadata (hermes-transport--get reference 'metadata))
         (board (hermes-tracker--display metadata 'board "default")))
    (hermes--promise-map
     (hermes--promise-catch
      (hermes-dashboard-transport-api-request-async
       "GET" (concat "/api/plugins/kanban/tasks/" (url-hexify-string id))
       :query `((board . ,board)))
      (lambda (_) nil))
     (lambda (payload) (hermes-tracker--linked-card todo reference payload)))))

(defun hermes-tracker--fetch-todo-with-cards (repo number)
  "Return promise of (TODO . LINKED-CARDS) for REPO NUMBER."
  (hermes--promise-then
   (hermes-tracker--request-async "GET"
                                  (hermes-tracker--api-path "repos" repo "todos" number))
   (lambda (todo)
     (hermes--promise-map
      (hermes--promise-all
       (mapcar (lambda (reference)
                 (hermes-tracker--fetch-linked-card todo reference))
               (hermes-tracker--kanban-references todo)))
      (lambda (cards) (cons todo cards))))))

(defun hermes-tracker--report-error (reason)
  "Report Tracker rejection REASON safely."
  (message "Hermes Tracker: %s" reason))

(defvar hermes-tracker-repositories-mode-map)
(keymap-popup-define hermes-tracker-repositories-mode-map
  "Keymap for Tracker repositories."
  :parent tabulated-list-mode-map :description "Hermes Tracker Repositories"
  :group "Navigate" "RET" ("Open repository" hermes-tracker-open-repository)
  :group "View" "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-tracker-repositories-mode-map-popup))

(define-derived-mode hermes-tracker-repositories-mode tabulated-list-mode
  "Tracker Repositories" "Major mode for Tracker repositories."
  :interactive nil
  (setq tabulated-list-format [("Repository" 28 t) ("Slug" 24 t) ("Description" 50 t)]
        revert-buffer-function #'hermes-tracker--repositories-revert)
  (tabulated-list-init-header))

(defun hermes-tracker--render-repositories (repositories &optional in-place)
  "Render REPOSITORIES, displaying unless IN-PLACE."
  (with-current-buffer (get-buffer-create "*Hermes Tracker Repositories*")
    (unless (derived-mode-p 'hermes-tracker-repositories-mode)
      (hermes-tracker-repositories-mode))
    (setq tabulated-list-entries (hermes-tracker--repository-rows repositories))
    (tabulated-list-print t)
    (unless in-place (pop-to-buffer (current-buffer)))))

(defun hermes-tracker--repositories-revert (&rest _)
  "Refresh Tracker repositories in place."
  (hermes--promise-then
   (hermes-tracker--request-async "GET" (hermes-tracker--api-path "repos"))
   (lambda (repos) (hermes-tracker--render-repositories repos t))
   #'hermes-tracker--report-error))

;;;###autoload
(defun hermes-list-tracker-repositories ()
  "Browse configured Hermes Tracker repositories."
  (interactive)
  (hermes--promise-then
   (hermes-tracker--request-async "GET" (hermes-tracker--api-path "repos"))
   #'hermes-tracker--render-repositories #'hermes-tracker--report-error))

(defun hermes-tracker-open-repository ()
  "Open selected Tracker repository's TODO list."
  (interactive)
  (let ((slug (or (tabulated-list-get-id) (user-error "No repository here")))
        (entry (tabulated-list-get-entry)))
    (hermes-tracker--render-todos slug (aref entry 0))))

(defvar hermes-tracker-todos-mode-map)
(keymap-popup-define hermes-tracker-todos-mode-map
  "Keymap for Tracker TODOs."
  :parent tabulated-list-mode-map :description "Hermes Tracker TODOs"
  :group "Navigate" "RET" ("Show TODO" hermes-tracker-show-todo)
  "b" ("Repositories" hermes-list-tracker-repositories)
  :group "TODO" "+" ("Create TODO" hermes-tracker-create-todo)
  "u" ("Update TODO" hermes-tracker-update-todo)
  "a" ("Claim TODO" hermes-tracker-claim-todo)
  "c" ("Comment" hermes-tracker-comment)
  :group "View" "f" ("Filter status" hermes-tracker-filter-status)
  "g" ("Refresh" revert-buffer) "?" ("Help" hermes-tracker-todos-mode-map-popup))

(define-derived-mode hermes-tracker-todos-mode tabulated-list-mode
  "Tracker TODOs" "Major mode for one Tracker repository's TODOs."
  :interactive nil
  (setq tabulated-list-format [("#" 5 t) ("Status" 10 t) ("Pri" 5 t)
                               ("Assignee" 18 t) ("Title" 60 t)]
        revert-buffer-function #'hermes-tracker--todos-revert)
  (tabulated-list-init-header))

(defun hermes-tracker--todo-list-path (slug status)
  "Return TODO-list path for SLUG and optional STATUS."
  (concat (hermes-tracker--api-path "repos" slug "todos")
          (if status (concat "?status_filter=" (url-hexify-string status)) "")))

(defun hermes-tracker--display-todos (payload slug name &optional in-place)
  "Display TODO PAYLOAD for repository SLUG NAME unless IN-PLACE."
  (with-current-buffer (get-buffer-create "*Hermes Tracker TODOs*")
    (unless (derived-mode-p 'hermes-tracker-todos-mode) (hermes-tracker-todos-mode))
    (setq hermes-tracker--repo-slug slug hermes-tracker--repo-name name
          mode-line-process (format " [%s%s]" slug
                                    (if hermes-tracker--status-filter
                                        (concat ":" hermes-tracker--status-filter) ""))
          tabulated-list-entries (hermes-tracker--todo-rows payload))
    (tabulated-list-print t)
    (unless in-place (pop-to-buffer (current-buffer)))))

(defun hermes-tracker--render-todos (slug name &optional in-place)
  "Fetch and render repository SLUG NAME TODOs unless IN-PLACE."
  (let ((status (and (derived-mode-p 'hermes-tracker-todos-mode)
                     hermes-tracker--status-filter)))
    (hermes--promise-then
     (hermes-tracker--request-async "GET" (hermes-tracker--todo-list-path slug status))
     (lambda (payload) (hermes-tracker--display-todos payload slug name in-place))
     #'hermes-tracker--report-error)))

(defun hermes-tracker--todos-revert (&rest _)
  "Refresh current Tracker TODO list."
  (hermes-tracker--render-todos hermes-tracker--repo-slug hermes-tracker--repo-name t))

(defun hermes-tracker-filter-status (status)
  "Filter current TODO list by STATUS; empty input clears it."
  (interactive (list (completing-read "Status (empty for all): "
                                      '("open" "claimed" "in-progress" "blocked" "done")
                                      nil nil)))
  (setq hermes-tracker--status-filter
        (hermes-transport--non-empty-string (string-trim status)))
  (revert-buffer nil t))

(defun hermes-tracker--number-for-command ()
  "Return selected/current TODO number."
  (or (and (derived-mode-p 'hermes-tracker-todo-mode) hermes-tracker--todo-number)
      (tabulated-list-get-id) (user-error "No Tracker TODO here")))

(defun hermes-tracker--context ()
  "Return current (REPO NUMBER REFRESH) command context."
  (let ((buffer (current-buffer)))
    (list (or hermes-tracker--repo-slug (user-error "No Tracker repository here"))
          (hermes-tracker--number-for-command)
          (lambda () (when (buffer-live-p buffer)
                       (with-current-buffer buffer (revert-buffer nil t)))))))

(defun hermes-tracker-show-todo ()
  "Show selected Tracker TODO with linked Kanban state."
  (interactive)
  (hermes-tracker-open-todo hermes-tracker--repo-slug
                            (hermes-tracker--number-for-command)))

(defun hermes-tracker-open-todo (repo number)
  "Open Tracker TODO NUMBER in REPO."
  (hermes--promise-then
   (hermes-tracker--fetch-todo-with-cards repo number)
   #'hermes-tracker--display-todo #'hermes-tracker--report-error))

(defvar hermes-tracker-todo-mode-map)
(keymap-popup-define hermes-tracker-todo-mode-map
  "Keymap for Tracker TODO detail."
  :parent special-mode-map :description "Hermes Tracker TODO"
  :group "Navigate" "K" ("Open linked Kanban card" hermes-tracker-open-kanban)
  "b" ("TODO list" hermes-tracker-back-to-todos)
  :group "TODO" "u" ("Update" hermes-tracker-update-todo)
  "a" ("Claim" hermes-tracker-claim-todo) "c" ("Comment" hermes-tracker-comment)
  "L" ("Link Kanban card" hermes-tracker-link-kanban)
  :group "View" "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-tracker-todo-mode-map-popup))

(define-derived-mode hermes-tracker-todo-mode special-mode "Tracker TODO"
  "Major mode for Tracker TODO details."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-tracker--todo-revert)
  (setq-local outline-regexp "^\\(#+\\) ")
  (outline-minor-mode 1))

(defun hermes-tracker--display-todo (pair)
  "Render TODO and linked cards from PAIR."
  (let ((todo (car pair)) (cards (cdr pair)))
    (with-current-buffer (get-buffer-create "*Hermes Tracker TODO*")
      (unless (derived-mode-p 'hermes-tracker-todo-mode) (hermes-tracker-todo-mode))
      (setq hermes-tracker--repo-slug (hermes-tracker--display todo 'repo_slug)
            hermes-tracker--todo-number (hermes-transport--get todo 'number)
            hermes-tracker--todo todo hermes-tracker--linked-cards cards
            mode-line-process (format " [%s#%s]" hermes-tracker--repo-slug
                                      hermes-tracker--todo-number))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (hermes-tracker--format-todo-detail todo)
                (format "\n## Linked Kanban cards (%d)\n\n" (length cards)))
        (if cards
            (dolist (card cards)
              (insert-text-button
               (hermes-tracker--linked-card-line card)
               'follow-link t 'help-echo "Open Hermes Kanban task"
               'action (lambda (_) (hermes-tracker--open-card card)))
              (insert "\n"))
          (insert "— no linked Kanban cards —\n")))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun hermes-tracker--todo-revert (&rest _)
  "Refresh current Tracker TODO."
  (hermes-tracker-open-todo hermes-tracker--repo-slug hermes-tracker--todo-number))

(defun hermes-tracker-back-to-todos ()
  "Return to current repository's TODO list."
  (interactive)
  (hermes-tracker--render-todos hermes-tracker--repo-slug hermes-tracker--repo-slug))

(defun hermes-tracker--open-card (card)
  "Open linked Kanban CARD through existing task detail renderer."
  (unless (plist-get card :payload) (user-error "Linked Kanban card is unavailable"))
  (require 'hermes-kanban)
  (funcall (intern "hermes-kanban--display-task")
           (plist-get card :payload) (plist-get card :board)))

(defun hermes-tracker-open-kanban ()
  "Choose and open a linked Kanban card."
  (interactive)
  (unless hermes-tracker--linked-cards (user-error "TODO has no linked Kanban cards"))
  (let* ((choices (mapcar (lambda (card) (cons (hermes-tracker--linked-card-line card) card))
                          hermes-tracker--linked-cards))
         (card (cdr (assoc (completing-read "Kanban card: " choices nil t) choices))))
    (hermes-tracker--open-card card)))

(defun hermes-tracker--invocation (operation repo number)
  "Return unique invocation identity for OPERATION REPO NUMBER."
  (format "%s:%s:%s:%s" operation repo number (float-time)))

(defun hermes-tracker-create-todo ()
  "Create a TODO in the current Tracker repository."
  (interactive)
  (let* ((repo hermes-tracker--repo-slug)
         (title (string-trim (read-string "Title: ")))
         (description (read-string "Description (optional): "))
         (priority (read-number "Priority: " 0)))
    (when (string-empty-p title) (user-error "Title is required"))
    (hermes--promise-then
     (hermes-tracker--request-async
      "POST" (hermes-tracker--api-path "repos" repo "todos")
      `((title . ,title) (description . ,description) (priority . ,priority))
      (hermes-tracker--invocation "create" repo 0))
     (lambda (_) (revert-buffer nil t)) #'hermes-tracker--report-error)))

(defun hermes-tracker-update-todo ()
  "Update title, priority, and assignee of current Tracker TODO."
  (interactive)
  (pcase-let ((`(,repo ,number ,refresh) (hermes-tracker--context)))
    (let* ((todo hermes-tracker--todo)
           (title (read-string "Title: " (hermes-tracker--display todo 'title)))
           (priority (read-number "Priority: "
                                  (or (hermes-transport--get todo 'priority) 0)))
           (assignee (read-string "Assignee (empty to clear): "
                                  (hermes-tracker--display todo 'assignee))))
      (hermes--promise-then
       (hermes-tracker--request-async
        "PATCH" (hermes-tracker--api-path "repos" repo "todos" number)
        `((title . ,title) (priority . ,priority)
          (assignee . ,(if (string-empty-p assignee) :null assignee))))
       (lambda (_) (funcall refresh)) #'hermes-tracker--report-error))))

(defun hermes-tracker-claim-todo ()
  "Claim current Tracker TODO for an assignee."
  (interactive)
  (pcase-let ((`(,repo ,number ,refresh) (hermes-tracker--context)))
    (let ((assignee (string-trim (read-string "Assignee: " hermes-tracker-actor))))
      (when (string-empty-p assignee) (user-error "Assignee is required"))
      (hermes--promise-then
       (hermes-tracker--request-async
        "POST" (hermes-tracker--api-path "repos" repo "todos" number "claim")
        `((assignee . ,assignee)) (hermes-tracker--invocation "claim" repo number))
       (lambda (_) (funcall refresh)) #'hermes-tracker--report-error))))

(defun hermes-tracker-comment ()
  "Append a public Tracker comment to current TODO."
  (interactive)
  (pcase-let ((`(,repo ,number ,refresh) (hermes-tracker--context)))
    (let ((body (string-trim (read-string "Comment: "))))
      (when (string-empty-p body) (user-error "Comment is required"))
      (hermes--promise-then
       (hermes-tracker--request-async
        "POST" (hermes-tracker--api-path "repos" repo "todos" number "comments")
        `((author . ,hermes-tracker-actor) (body . ,body))
        (hermes-tracker--invocation "comment" repo number))
       (lambda (_) (funcall refresh)) #'hermes-tracker--report-error))))

(defun hermes-tracker--external-reference-body (task-id board profile)
  "Return canonical external reference body for TASK-ID, BOARD, and PROFILE."
  `((system . "hermes-kanban") (external_id . ,task-id) (url . :null)
    (metadata . ((board . ,board) (profile . ,profile)))))

(defun hermes-tracker--ensure-external-reference (repo number task-id board profile)
  "Ensure Tracker backlink for REPO NUMBER, TASK-ID, BOARD, and PROFILE."
  (hermes--promise-catch
   (hermes-tracker--request-async
    "POST" (hermes-tracker--api-path "repos" repo "todos" number
                                      "external-references")
    (hermes-tracker--external-reference-body task-id board profile))
   (lambda (reason)
     (hermes--promise-then
      (hermes-tracker--request-async
       "GET" (hermes-tracker--api-path "repos" repo "todos" number))
      (lambda (todo)
        (or (seq-find (lambda (ref)
                        (and (equal (hermes-tracker--display ref 'system) "hermes-kanban")
                             (equal (hermes-tracker--display ref 'external_id) task-id)))
                      (hermes-tracker--items
                       (hermes-transport--get todo 'external_references)))
            (hermes--promise-rejected reason)))))))

(defun hermes-tracker--link-card (repo number board task-id payload)
  "Link REPO NUMBER to BOARD TASK-ID using loaded Kanban PAYLOAD."
  (let* ((task (hermes-transport--get payload 'task))
         (body (hermes-tracker--display task 'body))
         (profile (hermes-tracker--display task 'assignee hermes-tracker-actor))
         (existing (hermes-tracker-parse-reference body)))
    (when (and existing
               (not (equal existing (list :repo-slug repo :number number))))
      (user-error "Kanban card already links to %s#%s"
                  (plist-get existing :repo-slug) (plist-get existing :number)))
    (hermes--promise-then
     (hermes-tracker--ensure-external-reference repo number task-id board profile)
     (lambda (_)
       (if existing
           payload
         (hermes-dashboard-transport-api-request-async
          "PATCH" (concat "/api/plugins/kanban/tasks/" (url-hexify-string task-id))
          :body `((body . ,(hermes-tracker--body-with-reference body repo number)))
          :query `((board . ,board))))))))

(defun hermes-tracker-link-kanban ()
  "Explicitly link current Tracker TODO to a selected Kanban card."
  (interactive)
  (let* ((repo hermes-tracker--repo-slug) (number hermes-tracker--todo-number)
         (board (read-string "Kanban board: " "default"))
         (task-id (read-string "Kanban task id: ")))
    (unless (string-match-p "\\`t_[0-9a-f]\\{8\\}\\'" task-id)
      (user-error "Kanban task id must match t_<8 lowercase hex>"))
    (hermes--promise-then
     (hermes-dashboard-transport-api-request-async
      "GET" (concat "/api/plugins/kanban/tasks/" (url-hexify-string task-id))
      :query `((board . ,board)))
     (lambda (payload) (hermes-tracker--link-card repo number board task-id payload))
     #'hermes-tracker--report-error)
    (message "Hermes Tracker: linking %s#%s and %s" repo number task-id)))

(provide 'hermes-tracker)
;;; hermes-tracker.el ends here
