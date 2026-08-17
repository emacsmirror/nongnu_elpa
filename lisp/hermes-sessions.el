;;; hermes-sessions.el --- Session browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard `session.list' method.  RET
;; resumes the selected session in a fresh chat buffer; auxiliary commands view
;; history and manage stored sessions through dashboard RPCs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-promise)
(require 'hermes-browser)
(require 'hermes-chat)
(require 'hermes-session-title)

(defvar-local hermes-sessions--session-map nil
  "Hash table mapping session ids to session alists in a browser buffer.")

(defvar-local hermes-sessions--detail-session nil
  "Session alist rendered in the current detail buffer.")

(defvar-local hermes-sessions--detail-messages nil
  "History messages rendered in the current detail buffer.")

(defvar-local hermes-sessions--detail-count nil
  "Total history message count reported for the current detail buffer.")

(defvar-local hermes-sessions--all-profiles nil
  "Non-nil when the browser lists sessions from every profile.")

(defvar-local hermes-sessions--archived-filter "exclude"
  "Archived-session filter used by the REST list endpoint.")

(defvar-local hermes-sessions--search-query nil
  "Current session search query, or nil for normal listing.")

(defvar-local hermes-sessions--search-profile nil
  "Profile scope for session search, or nil for the dashboard default.")

(defun hermes-sessions--non-empty-field (session key)
  "Return SESSION's KEY as a non-empty display string, or nil."
  (hermes-transport--non-empty-string
   (hermes-transport--display-field session key)))

(defun hermes-sessions--id (session)
  "Return SESSION's durable id."
  (hermes-transport--display-field session 'id))

(defun hermes-sessions--profile (session)
  "Return SESSION's owning profile, or nil for the dashboard default."
  (hermes-sessions--non-empty-field session 'profile))

(defun hermes-sessions--identity (session)
  "Return SESSION's profile-qualified row identity."
  (cons (or (hermes-sessions--profile session) "")
        (hermes-sessions--id session)))

(defun hermes-sessions--live-id (session)
  "Return SESSION's live dashboard id, or nil."
  (let ((id (hermes-transport--display-field session 'live_session_id)))
    (and (not (string-empty-p id)) id)))

(defun hermes-sessions--message-count (session)
  "Return SESSION's message count as a display string."
  (format "%s" (or (hermes-transport--get session 'message_count) 0)))

(defun hermes-sessions--sessions-by-id (sessions)
  "Return a hash table mapping SESSIONS by profile-qualified identity."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (session sessions table)
      (let ((id (hermes-sessions--id session)))
        (unless (string-empty-p id)
          (puthash (hermes-sessions--identity session) session table))))))

(defun hermes-sessions--rows (sessions)
  "Return `tabulated-list' entries for SESSIONS, a list of session alists."
  (mapcar
   (lambda (session)
     (let ((id (hermes-sessions--id session)))
       (list (hermes-sessions--identity session)
             (vector (hermes-browser--face-cell id 'hermes-browser-identifier)
                     (hermes-browser--face-cell
                      (hermes-session-title-browser-display
                       (hermes-transport--display-field session 'title))
                      'hermes-browser-title)
                     (hermes-browser--face-cell
                      (hermes-sessions--message-count session)
                      'hermes-browser-message-count)
                     (hermes-browser--face-cell
                      (hermes-transport--display-field session 'source)
                      'hermes-browser-source)
                     (hermes-browser--face-cell
                      (hermes-transport--display-field session 'profile)
                      'hermes-browser-profile)))))
   sessions))

(defun hermes-sessions--result-sessions (result)
  "Return session records from list or search RESULT."
  (or (hermes-transport--get result 'sessions)
      (mapcar
       (lambda (hit)
         (if (hermes-transport--get hit 'id)
             hit
           (append `((id . ,(hermes-transport--get hit 'session_id))
                     (title . ,(hermes-transport--get hit 'snippet))
                     ,@(and hermes-sessions--search-profile
                            `((profile . ,hermes-sessions--search-profile))))
                   hit)))
       (or (hermes-transport--get result 'results) '()))))

(defun hermes-sessions--result-rows (result)
  "Return `tabulated-list' entries for a session list or search RESULT."
  (hermes-sessions--rows (hermes-sessions--result-sessions result)))

(defun hermes-sessions--record-result (result)
  "Cache RESULT's sessions by durable id for the row commands."
  (setq hermes-sessions--session-map
        (hermes-sessions--sessions-by-id
         (hermes-sessions--result-sessions result))))

(defun hermes-sessions--rest (client method path &optional body query)
  "Return a dashboard REST METHOD PATH promise through CLIENT.
BODY and QUERY extend the request."
  (hermes-dashboard-transport-api-request-async
   method path :body body :query query :client client))

(defun hermes-sessions--fetch (client)
  "Return current session browser result through CLIENT."
  (cond
   (hermes-sessions--search-query
    (hermes-sessions--rest
     client "GET" "/api/sessions/search" nil
     (append `((q . ,hermes-sessions--search-query) (limit . 100))
             (and hermes-sessions--search-profile
                  `((profile . ,hermes-sessions--search-profile))))))
   ((or hermes-sessions--all-profiles
        (not (equal hermes-sessions--archived-filter "exclude")))
    (hermes-sessions--rest
     client "GET" "/api/profiles/sessions" nil
     `((profile . "all") (limit . 100)
       (archived . ,hermes-sessions--archived-filter))))
   (t
    (hermes-dashboard-transport-call-fn
     #'hermes-dashboard-transport-session-list client))))

;;;###autoload (autoload 'hermes-list-sessions "hermes-sessions" nil t)
(hermes-define-list-browser sessions
  :title "Hermes Sessions"
  :buffer "*Hermes Sessions*"
  :command hermes-list-sessions
  :doc "Major mode listing resumable Hermes dashboard sessions."
  :command-doc "List resumable Hermes dashboard sessions in a browser buffer.
Reuses a live chat connection when one exists; otherwise connects a transient
client just for the listing."
  :columns [("Session" 22 t) ("Title" 36 t) ("Msgs" 6 t) ("Source" 12 t)
            ("Profile" 14 t)]
  :fetch #'hermes-sessions--fetch
  :rows #'hermes-sessions--result-rows
  :on-result #'hermes-sessions--record-result
  :keys ("RET" #'hermes-sessions-open
         "v" #'hermes-sessions-view
         "r" #'hermes-sessions-rename
         "d" #'hermes-sessions-delete
         "a" #'hermes-sessions-archive
         "u" #'hermes-sessions-unarchive
         "A" #'hermes-sessions-toggle-archived
         "s" #'hermes-sessions-search
         "P" #'hermes-sessions-list-all-profiles
         "w" #'hermes-sessions-export))

(defvar-keymap hermes-session-detail-mode-map
  :doc "Keymap for `hermes-session-detail-mode'."
  :parent special-mode-map
  "RET" #'hermes-sessions-open
  "g" #'hermes-sessions-view
  "r" #'hermes-sessions-rename
  "d" #'hermes-sessions-delete
  "a" #'hermes-sessions-archive
  "u" #'hermes-sessions-unarchive
  "w" #'hermes-sessions-export)

(define-derived-mode hermes-session-detail-mode special-mode "Hermes Session"
  "Major mode showing one Hermes session's history."
  :interactive nil)

(defun hermes-sessions--session-from-entry (identity entry)
  "Return a session alist from row IDENTITY and tabulated ENTRY."
  `((id . ,(cdr identity))
    (title . ,(or (and entry (> (length entry) 1) (aref entry 1)) ""))
    (message_count . ,(or (and entry (> (length entry) 2) (aref entry 2)) 0))
    (source . ,(or (and entry (> (length entry) 3) (aref entry 3)) ""))
    (profile . ,(or (car identity)
                    (and entry (> (length entry) 4) (aref entry 4)) ""))))

(defun hermes-sessions--selected-session ()
  "Return the current browser or detail session, or signal `user-error'."
  (cond
   ((derived-mode-p 'hermes-sessions-mode)
    (let ((identity (tabulated-list-get-id))
          (entry (tabulated-list-get-entry)))
      (unless identity
        (user-error "No Hermes session on this line"))
      (or (and hermes-sessions--session-map
               (gethash identity hermes-sessions--session-map))
          (hermes-sessions--session-from-entry identity entry))))
   ((derived-mode-p 'hermes-session-detail-mode)
    (or hermes-sessions--detail-session
        (user-error "No Hermes session in this buffer")))
   (t
    (user-error "Not in a Hermes sessions buffer"))))

(defun hermes-sessions-search (query &optional profile)
  "Search dashboard sessions for QUERY scoped to optional PROFILE."
  (interactive (list (read-string "Search sessions: "
                                  hermes-sessions--search-query)
                     (read-string "Profile (blank for dashboard default): "
                                  hermes-sessions--search-profile)))
  (setq hermes-sessions--search-query
        (hermes-transport--non-blank-string query)
        hermes-sessions--search-profile
        (hermes-transport--non-blank-string profile))
  (hermes-sessions--revert))

(defun hermes-sessions-list-all-profiles ()
  "List sessions aggregated across every Hermes profile."
  (interactive)
  (setq hermes-sessions--all-profiles t
        hermes-sessions--search-query nil)
  (hermes-sessions--revert))

(defun hermes-sessions-toggle-archived ()
  "Toggle the browser between active and archived sessions."
  (interactive)
  (setq hermes-sessions--archived-filter
        (if (equal hermes-sessions--archived-filter "only") "exclude" "only")
        hermes-sessions--search-query nil)
  (hermes-sessions--revert))

(defun hermes-sessions--set-archived (archived)
  "Set the selected session's ARCHIVED state through dashboard REST."
  (let* ((session (hermes-sessions--selected-session))
         (id (hermes-sessions--id session))
         (profile (hermes-sessions--non-empty-field session 'profile))
         (origin (current-buffer))
         (origin-mode (if (derived-mode-p 'hermes-session-detail-mode)
                          'hermes-session-detail-mode
                        'hermes-sessions-mode)))
    (when (string-empty-p id)
      (user-error "No Hermes session id to update"))
    (let ((generation (hermes-browser--next-request-generation)))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-sessions--rest
          client "PATCH" (concat "/api/sessions/" (url-hexify-string id))
          (append `((archived . ,(if archived t :false)))
                  (and profile `((profile . ,profile))))))
       (lambda (_result)
         (when (hermes-browser--request-current-mode-p
                origin generation origin-mode)
           (message "Hermes: %s session %s"
                    (if archived "archived" "unarchived") id)
           (when (eq origin-mode 'hermes-sessions-mode)
             (with-current-buffer origin (hermes-sessions--revert)))))))))

(defun hermes-sessions-archive ()
  "Archive the selected Hermes session."
  (interactive)
  (hermes-sessions--set-archived t))

(defun hermes-sessions-unarchive ()
  "Restore the selected archived Hermes session."
  (interactive)
  (hermes-sessions--set-archived nil))

(defun hermes-sessions--display-string (value)
  "Return a readable display string for VALUE."
  (cond
   ((null value) nil)
   ((hermes-transport--scalar-string value))
   ((vectorp value)
    (hermes-sessions--display-string (append value nil)))
   ((and (listp value) (not (hermes-transport--object-p value)))
    (string-join
     (delq nil
           (mapcar (lambda (item)
                     (let ((text (hermes-sessions--display-string item)))
                       (and text (not (string-empty-p text)) text)))
                   value))
     "\n"))
   ((hermes-transport--object-p value)
    (or (hermes-sessions--display-string
         (hermes-transport--get-any value '(text content output preview summary)))
        (let ((kind (hermes-transport--display-field value 'type)))
          (and (not (string-empty-p kind))
               (format "[%s]" kind)))
        (prin1-to-string value)))
   (value (format "%s" value))))

(defun hermes-sessions--message-role (message)
  "Return MESSAGE's role as a lowercase string."
  (downcase (or (hermes-transport--scalar-string
                 (hermes-transport--get message 'role))
                "system")))

(defun hermes-sessions--message-text (message)
  "Return MESSAGE's readable text body."
  (or (hermes-sessions--display-string
       (hermes-transport--get-any message '(text content context output)))
      ""))

(defun hermes-sessions--markdown (session messages)
  "Return SESSION and MESSAGES as Markdown."
  (string-join
   (cons (format "# %s\n\nSession: `%s`"
                 (or (hermes-sessions--non-empty-field session 'title)
                     "Hermes session")
                 (hermes-sessions--id session))
         (mapcar (lambda (message)
                   (format "## %s\n\n%s"
                           (hermes-sessions--message-label message)
                           (hermes-sessions--message-text message)))
                 messages))
   "\n\n"))

(defun hermes-sessions--write-export (session messages file)
  "Write SESSION MESSAGES as Markdown to FILE."
  (with-temp-file file
    (insert (hermes-sessions--markdown session messages) "\n"))
  (message "Hermes: exported session %s" (hermes-sessions--id session)))

(defun hermes-sessions-export (file)
  "Export selected session history as Markdown to FILE."
  (interactive
   (let* ((session (hermes-sessions--selected-session))
          (id (hermes-sessions--id session)))
     (list (read-file-name "Export session to: " nil
                           (format "hermes-session-%s.md" id)))))
  (let ((session (hermes-sessions--selected-session)))
    (if (derived-mode-p 'hermes-session-detail-mode)
        (hermes-sessions--write-export session hermes-sessions--detail-messages file)
      (let ((history-id (hermes-sessions--history-id session))
            (origin (current-buffer))
            (generation (hermes-browser--next-request-generation)))
        (hermes-browser--run-on-client
         (lambda (client)
           (hermes-sessions--history-promise
            client history-id (hermes-sessions--id session)
            (hermes-sessions--profile session)))
         (lambda (result)
           (when (hermes-browser--request-current-mode-p
                  origin generation 'hermes-sessions-mode)
             (hermes-sessions--write-export
              session (hermes-transport--get result 'messages) file))))))))

(defun hermes-sessions--tool-name (message)
  "Return MESSAGE's tool name, or nil."
  (or (hermes-sessions--non-empty-field message 'name)
      (hermes-sessions--non-empty-field message 'tool_name)))

(defun hermes-sessions--message-label (message)
  "Return a readable label for MESSAGE."
  (let ((role (hermes-sessions--message-role message)))
    (if (string= role "tool")
        (if-let* ((name (hermes-sessions--tool-name message)))
            (format "tool: %s" name)
          "tool")
      role)))

(defun hermes-sessions--message-tool-calls (message)
  "Return a list of tool invocation records from MESSAGE."
  (let ((calls (hermes-transport--get message 'tool_calls)))
    (cond
     ((null calls) nil)
     ((vectorp calls) (append calls nil))
     ((hermes-transport--object-p calls) (list calls))
     ((listp calls) calls))))

(defun hermes-sessions--insert-tool-call (tool-call)
  "Insert TOOL-CALL details at point."
  (let* ((function (hermes-transport--get tool-call 'function))
         (name (or (hermes-sessions--non-empty-field function 'name)
                   (hermes-sessions--non-empty-field tool-call 'name)
                   "tool"))
         (id (hermes-transport--display-field tool-call 'id))
         (args (hermes-sessions--display-string
                (or (hermes-transport--get function 'arguments)
                    (hermes-transport--get tool-call 'arguments)))))
    (insert (propertize (format "  tool-call: %s" name)
                        'face 'font-lock-function-name-face))
    (unless (string-empty-p id)
      (insert (format " <%s>" id)))
    (insert "\n")
    (when (and args (not (string-empty-p args)))
      (insert args "\n"))))

(defun hermes-sessions--insert-message (message)
  "Insert one history MESSAGE at point."
  (let ((text (hermes-sessions--message-text message)))
    (insert (propertize (format "[%s]\n" (hermes-sessions--message-label message))
                        'face 'font-lock-keyword-face))
    (insert (if (string-empty-p text) "[empty]" text) "\n")
    (dolist (tool-call (hermes-sessions--message-tool-calls message))
      (hermes-sessions--insert-tool-call tool-call))
    (insert "\n")))

(defun hermes-sessions--detail-buffer-name (session)
  "Return the detail buffer name for SESSION."
  (let ((id (hermes-sessions--id session))
        (profile (hermes-sessions--profile session)))
    (if (string-empty-p id)
        "*Hermes Session*"
      (format "*Hermes Session: %s%s*"
              (if profile (concat profile "/") "") id))))

(defun hermes-sessions--render-detail-contents (session messages count)
  "Render SESSION's MESSAGES in the current detail buffer.
COUNT, when non-nil, is the total history count reported by the gateway."
  (unless (derived-mode-p 'hermes-session-detail-mode)
    (hermes-session-detail-mode))
  (setq hermes-sessions--detail-session session
        hermes-sessions--detail-messages messages
        hermes-sessions--detail-count count)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((title (hermes-transport--display-field session 'title))
          (id (hermes-sessions--id session))
          (source (hermes-transport--display-field session 'source)))
      (insert (format "Session: %s\n" (if (string-empty-p title) id title)))
      (insert (format "ID: %s\n" id))
      (unless (string-empty-p source)
        (insert (format "Source: %s\n" source)))
      (insert (format "Messages: %s\n\n" (or count (length messages)))))
    (if messages
        (dolist (message messages)
          (hermes-sessions--insert-message message))
      (insert "No messages.\n"))
    (goto-char (point-min))))

(defun hermes-sessions--render-detail
    (session messages &optional count display instance)
  "Display SESSION's MESSAGES in a native detail buffer.
COUNT, when non-nil, is the total history count reported by the gateway.
DISPLAY pops the buffer when non-nil; a `g' refresh from within it omits that.
INSTANCE is inherited from the session browser when provided."
  (let ((buffer (get-buffer-create (hermes-sessions--detail-buffer-name session))))
    (with-current-buffer buffer
      (hermes-sessions--render-detail-contents session messages count)
      (when instance (hermes-browser--own-instance instance)))
    (when display (pop-to-buffer buffer))
    buffer))

(defun hermes-sessions--session-not-found-message-p (message)
  "Return non-nil when MESSAGE is the gateway's missing-session error."
  (and (stringp message)
       (string-match-p "session not found" (downcase message))))

(defun hermes-sessions--session-with-result (session result)
  "Return SESSION annotated with live ids from RESULT."
  (let ((live-id (hermes-transport--display-field result 'session_id)))
    (if (string-empty-p live-id)
        session
      (append `((live_session_id . ,live-id)) session))))

(defun hermes-sessions--history-id (session)
  "Return the best id for requesting SESSION history."
  (or (hermes-sessions--live-id session)
      (let ((id (hermes-sessions--id session)))
        (and (not (string-empty-p id)) id))))

(defun hermes-sessions--history-promise (client history-id resume-id profile)
  "Return a promise of HISTORY-ID history on CLIENT.
PROFILE selects another profile's stored session.  When older gateways cannot
find a default-profile live session, read RESUME-ID through non-attaching REST."
  (if profile
      (hermes-sessions--rest
       client "GET"
       (concat "/api/sessions/" (url-hexify-string resume-id) "/messages")
       nil `((profile . ,profile)))
    (hermes--promise-catch
     (hermes-dashboard-transport-call-fn
      #'hermes-dashboard-transport-session-history client history-id)
     (lambda (message)
       (if (and (hermes-sessions--session-not-found-message-p message)
                (not (string-empty-p resume-id)))
           (hermes-sessions--rest
            client "GET"
            (concat "/api/sessions/" (url-hexify-string resume-id) "/messages"))
         (hermes--promise-rejected message))))))

(defun hermes-sessions-view ()
  "Show a native detail/history buffer for the selected Hermes session."
  (interactive)
  (let* ((session (hermes-sessions--selected-session))
         (history-id (hermes-sessions--history-id session))
         (resume-id (hermes-sessions--id session))
         (profile (hermes-sessions--profile session))
         (detail-p (derived-mode-p 'hermes-session-detail-mode))
         (display (not detail-p))
         (instance (hermes-instance-resolve))
         (origin (current-buffer))
         (generation (hermes-browser--next-request-generation)))
    (unless history-id
      (user-error "No Hermes session id to view"))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-sessions--history-promise client history-id resume-id profile))
     (lambda (result)
       (when (hermes-browser--request-current-mode-p
              origin generation
              (if detail-p 'hermes-session-detail-mode 'hermes-sessions-mode))
         (let ((session (hermes-sessions--session-with-result session result))
               (messages (hermes-transport--get result 'messages))
               (count (hermes-transport--get result 'count)))
           (if detail-p
               (with-current-buffer origin
                 (when (derived-mode-p 'hermes-session-detail-mode)
                   (hermes-sessions--render-detail-contents
                    session messages count)))
             (hermes-sessions--render-detail
              session messages count display instance))))))))

(defun hermes-sessions-open ()
  "Resume the selected Hermes session in a chat buffer."
  (interactive)
  (let* ((instance (hermes-instance-resolve))
         (session (hermes-sessions--selected-session))
         (id (hermes-sessions--id session))
         (title (hermes-transport--display-field session 'title))
         (profile (hermes-sessions--profile session)))
    (when (string-empty-p id)
      (user-error "No Hermes session id to resume"))
    (hermes-chat-resume-session id title profile instance)))

(defun hermes-sessions--title-empty-p (title)
  "Return non-nil when TITLE is blank."
  (string-empty-p (string-trim (or title ""))))

(defun hermes-sessions--set-title-promise (client session-id title profile)
  "Return a promise setting SESSION-ID's TITLE on CLIENT.
PROFILE targets another profile through REST.  When older gateways cannot find
a default-profile live session, update its durable record without attaching it."
  (if profile
      (hermes-sessions--rest
       client "PATCH" (concat "/api/sessions/" (url-hexify-string session-id))
       `((title . ,title) (profile . ,profile)))
    (hermes--promise-catch
     (hermes-dashboard-transport-call-fn
      #'hermes-dashboard-transport-session-title
      client :session-id session-id :title title)
     (lambda (message)
       (if (hermes-sessions--session-not-found-message-p message)
           (hermes-sessions--rest
            client "PATCH"
            (concat "/api/sessions/" (url-hexify-string session-id))
            `((title . ,title)))
         (hermes--promise-rejected message))))))

(defun hermes-sessions--session-with-title (session title)
  "Return SESSION with TITLE shadowing its previous title field."
  (cons (cons 'title title) session))

(defun hermes-sessions--replace-browser-row-title (identity title)
  "Replace browser row IDENTITY's title with TITLE in the current buffer."
  (when-let* ((entry (assoc identity tabulated-list-entries)))
    (aset (cadr entry) 1
          (hermes-browser--face-cell title 'hermes-browser-title))
    (when hermes-sessions--session-map
      (let ((session (gethash identity hermes-sessions--session-map)))
        (when session
          (puthash identity (hermes-sessions--session-with-title session title)
                   hermes-sessions--session-map))))
    (tabulated-list-print t)))

(defun hermes-sessions--detail-buffer-for-id (identity)
  "Return the detail buffer for session IDENTITY, or nil."
  (get-buffer
   (hermes-sessions--detail-buffer-name
    `((id . ,(cdr identity)) (profile . ,(car identity))))))

(defun hermes-sessions--owned-buffer-p (buffer instance mode)
  "Return non-nil when BUFFER is live in MODE and still owns INSTANCE."
  (and (hermes-browser--buffer-mode-p buffer mode)
       (equal instance (buffer-local-value 'hermes-instance buffer))))

(defun hermes-sessions--after-rename (_buffer instance identity title)
  "Update session buffers owned by INSTANCE after renaming IDENTITY to TITLE."
  (when-let* ((browser (get-buffer "*Hermes Sessions*")))
    (with-current-buffer browser
      (when (hermes-sessions--owned-buffer-p
             browser instance 'hermes-sessions-mode)
        (hermes-sessions--replace-browser-row-title identity title))))
  (when-let* ((detail (hermes-sessions--detail-buffer-for-id identity)))
    (with-current-buffer detail
      (when (hermes-sessions--owned-buffer-p
             detail instance 'hermes-session-detail-mode)
        (setq hermes-sessions--detail-session
              (hermes-sessions--session-with-title
               hermes-sessions--detail-session title))
        (hermes-sessions--render-detail-contents
         hermes-sessions--detail-session
         hermes-sessions--detail-messages
         hermes-sessions--detail-count)))))

(defun hermes-sessions-rename ()
  "Rename the selected Hermes session after prompting for a title."
  (interactive)
  (let* ((session (hermes-sessions--selected-session))
         (id (hermes-sessions--id session)))
    (when (string-empty-p id)
      (user-error "No Hermes session id to rename"))
    (let* ((current-title (hermes-transport--display-field session 'title))
           (title (read-string (format "Rename Hermes session %s to: " id)
                               current-title))
           (origin (current-buffer))
           (origin-mode major-mode)
           (instance (hermes-instance-resolve)))
      (when (hermes-sessions--title-empty-p title)
        (user-error "Session title required"))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-sessions--set-title-promise
          client id (string-trim title) (hermes-sessions--profile session)))
       (lambda (_result)
         (when (hermes-sessions--owned-buffer-p origin instance origin-mode)
           (message "Hermes: renamed session %s" id)
           (hermes-sessions--after-rename
            origin instance (hermes-sessions--identity session)
            (string-trim title))))))))

(defun hermes-sessions--remove-browser-row (identity)
  "Remove browser row IDENTITY from the current buffer."
  (setq tabulated-list-entries
        (cl-remove identity tabulated-list-entries :key #'car :test #'equal))
  (when hermes-sessions--session-map
    (remhash identity hermes-sessions--session-map))
  (tabulated-list-print t))

(defun hermes-sessions--after-delete (_buffer instance identity)
  "Update session buffers owned by INSTANCE after deleting IDENTITY."
  (when-let* ((browser (get-buffer "*Hermes Sessions*")))
    (with-current-buffer browser
      (when (hermes-sessions--owned-buffer-p
             browser instance 'hermes-sessions-mode)
        (hermes-sessions--remove-browser-row identity))))
  (when-let* ((detail (hermes-sessions--detail-buffer-for-id identity)))
    (when (hermes-sessions--owned-buffer-p
           detail instance 'hermes-session-detail-mode)
      (kill-buffer detail))))

(defun hermes-sessions-delete ()
  "Delete the selected Hermes session after an explicit confirmation prompt."
  (interactive)
  (let* ((session (hermes-sessions--selected-session))
         (id (hermes-sessions--id session))
         (title (hermes-transport--display-field session 'title))
         (origin (current-buffer))
         (origin-mode major-mode)
         (instance (hermes-instance-resolve)))
    (when (string-empty-p id)
      (user-error "No Hermes session id to delete"))
    (if (yes-or-no-p
         (format "Delete Hermes session %s%s? "
                 id
                 (if (string-empty-p title) "" (format " (%s)" title))))
        (hermes-browser--run-on-client
         (lambda (client)
           (hermes-sessions--rest
            client "DELETE" (concat "/api/sessions/" (url-hexify-string id))
            nil (and (hermes-sessions--profile session)
                     `((profile . ,(hermes-sessions--profile session))))))
         (lambda (_result)
           (when (hermes-sessions--owned-buffer-p origin instance origin-mode)
             (message "Hermes: deleted session %s" id)
             (hermes-sessions--after-delete
              origin instance (hermes-sessions--identity session)))))
      (message "Hermes: delete cancelled"))))

(provide 'hermes-sessions)
;;; hermes-sessions.el ends here
