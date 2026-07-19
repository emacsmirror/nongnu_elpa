;;; hermes-sessions.el --- Session browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard `session.list' method.  RET
;; resumes the selected session in a fresh chat buffer; auxiliary commands view
;; history and manage stored sessions through dashboard RPCs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-promise)
(require 'hermes-browser)
(require 'hermes-chat)

(defvar-local hermes-sessions--session-map nil
  "Hash table mapping session ids to session alists in a browser buffer.")

(defvar-local hermes-sessions--detail-session nil
  "Session alist rendered in the current detail buffer.")

(defvar-local hermes-sessions--detail-messages nil
  "History messages rendered in the current detail buffer.")

(defvar-local hermes-sessions--detail-count nil
  "Total history message count reported for the current detail buffer.")

(defun hermes-sessions--non-empty-field (session key)
  "Return SESSION's KEY as a non-empty display string, or nil."
  (hermes-transport--non-empty-string
   (hermes-transport--display-field session key)))

(defun hermes-sessions--id (session)
  "Return SESSION's durable id."
  (hermes-transport--display-field session 'id))

(defun hermes-sessions--live-id (session)
  "Return SESSION's live dashboard id, or nil."
  (let ((id (hermes-transport--display-field session 'live_session_id)))
    (and (not (string-empty-p id)) id)))

(defun hermes-sessions--message-count (session)
  "Return SESSION's message count as a display string."
  (format "%s" (or (hermes-transport--get session 'message_count) 0)))

(defun hermes-sessions--sessions-by-id (sessions)
  "Return a hash table mapping SESSIONS by durable id."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (session sessions table)
      (let ((id (hermes-sessions--id session)))
        (unless (string-empty-p id)
          (puthash id session table))))))

(defun hermes-sessions--rows (sessions)
  "Return `tabulated-list' entries for SESSIONS, a list of session alists."
  (mapcar
   (lambda (session)
     (let ((id (hermes-sessions--id session)))
       (list id
             (vector (hermes-browser--face-cell id 'hermes-browser-identifier)
                     (hermes-browser--face-cell
                      (hermes-transport--display-field session 'title)
                      'hermes-browser-title)
                     (hermes-browser--face-cell
                      (hermes-sessions--message-count session)
                      'hermes-browser-message-count)
                     (hermes-browser--face-cell
                      (hermes-transport--display-field session 'source)
                      'hermes-browser-source)))))
   sessions))

(defun hermes-sessions--result-rows (result)
  "Return `tabulated-list' entries for a `session.list' RESULT."
  (hermes-sessions--rows (hermes-transport--get result 'sessions)))

(defun hermes-sessions--record-result (result)
  "Cache RESULT's sessions by durable id for the row commands."
  (setq hermes-sessions--session-map
        (hermes-sessions--sessions-by-id
         (hermes-transport--get result 'sessions))))

;;;###autoload (autoload 'hermes-list-sessions "hermes-sessions" nil t)
(hermes-define-list-browser sessions
  :title "Hermes Sessions"
  :buffer "*Hermes Sessions*"
  :command hermes-list-sessions
  :doc "Major mode listing resumable Hermes dashboard sessions."
  :command-doc "List resumable Hermes dashboard sessions in a browser buffer.
Reuses a live chat connection when one exists; otherwise connects a transient
client just for the listing."
  :columns [("Session" 22 t) ("Title" 40 t) ("Msgs" 6 t) ("Source" 12 t)]
  :fetch (lambda (client)
           (hermes-dashboard-transport-call-fn
            #'hermes-dashboard-transport-session-list client))
  :rows #'hermes-sessions--result-rows
  :on-result #'hermes-sessions--record-result
  :keys ("RET" #'hermes-sessions-open
         "v" #'hermes-sessions-view
         "r" #'hermes-sessions-rename
         "d" #'hermes-sessions-delete))

(defvar-keymap hermes-session-detail-mode-map
  :doc "Keymap for `hermes-session-detail-mode'."
  :parent special-mode-map
  "RET" #'hermes-sessions-open
  "g" #'hermes-sessions-view
  "r" #'hermes-sessions-rename
  "d" #'hermes-sessions-delete)

(define-derived-mode hermes-session-detail-mode special-mode "Hermes Session"
  "Major mode showing one Hermes session's history."
  :interactive nil)

(defun hermes-sessions--session-from-entry (id entry)
  "Return a session alist from row ID and tabulated ENTRY."
  `((id . ,id)
    (title . ,(or (and entry (> (length entry) 1) (aref entry 1)) ""))
    (message_count . ,(or (and entry (> (length entry) 2) (aref entry 2)) 0))
    (source . ,(or (and entry (> (length entry) 3) (aref entry 3)) ""))))

(defun hermes-sessions--selected-session ()
  "Return the current browser or detail session, or signal `user-error'."
  (cond
   ((derived-mode-p 'hermes-sessions-mode)
    (let ((id (tabulated-list-get-id))
          (entry (tabulated-list-get-entry)))
      (unless id
        (user-error "No Hermes session on this line"))
      (or (and hermes-sessions--session-map
               (gethash id hermes-sessions--session-map))
          (hermes-sessions--session-from-entry id entry))))
   ((derived-mode-p 'hermes-session-detail-mode)
    (or hermes-sessions--detail-session
        (user-error "No Hermes session in this buffer")))
   (t
    (user-error "Not in a Hermes sessions buffer"))))

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
  (let ((id (hermes-sessions--id session)))
    (if (string-empty-p id)
        "*Hermes Session*"
      (format "*Hermes Session: %s*" id))))

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

(defun hermes-sessions--render-detail (session messages &optional count display)
  "Display SESSION's MESSAGES in a native detail buffer.
COUNT, when non-nil, is the total history count reported by the gateway.
DISPLAY pops the buffer when non-nil; a `g' refresh from within it omits that."
  (let ((buffer (get-buffer-create (hermes-sessions--detail-buffer-name session))))
    (with-current-buffer buffer
      (hermes-sessions--render-detail-contents session messages count))
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

(defun hermes-sessions--history-promise (client history-id resume-id)
  "Return a promise of HISTORY-ID history on CLIENT.
The dashboard `session.history' RPC is live-session scoped in older gateways;
when it reports a missing live session, resume RESUME-ID and resolve with the
returned messages instead."
  (hermes--promise-catch
   (hermes-dashboard-transport-call-fn #'hermes-dashboard-transport-session-history
				       client history-id)
   (lambda (message)
     (if (and (hermes-sessions--session-not-found-message-p message)
              (not (string-empty-p resume-id)))
         (hermes-dashboard-transport-call-fn #'hermes-dashboard-transport-session-resume
					     client resume-id)
       (hermes--promise-rejected message)))))

(defun hermes-sessions-view ()
  "Show a native detail/history buffer for the selected Hermes session."
  (interactive)
  (let* ((session (hermes-sessions--selected-session))
         (history-id (hermes-sessions--history-id session))
         (resume-id (hermes-sessions--id session))
         (display (not (derived-mode-p 'hermes-session-detail-mode))))
    (unless history-id
      (user-error "No Hermes session id to view"))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-sessions--history-promise client history-id resume-id))
     (lambda (result)
       (hermes-sessions--render-detail
        (hermes-sessions--session-with-result session result)
        (hermes-transport--get result 'messages)
        (hermes-transport--get result 'count)
        display)))))

(defun hermes-sessions-open ()
  "Resume the selected Hermes session in a chat buffer."
  (interactive)
  (let* ((session (hermes-sessions--selected-session))
         (id (hermes-sessions--id session))
         (title (hermes-transport--display-field session 'title)))
    (when (string-empty-p id)
      (user-error "No Hermes session id to resume"))
    (hermes-chat-resume-session id title)))

(defun hermes-sessions--title-empty-p (title)
  "Return non-nil when TITLE is blank."
  (string-empty-p (string-trim (or title ""))))

(defun hermes-sessions--set-title-promise (client session-id title)
  "Return a promise setting SESSION-ID's TITLE on CLIENT.
On a missing-session error, resume SESSION-ID and retry the title on the live
id it returns."
  (hermes--promise-catch
   (hermes-dashboard-transport-call-fn #'hermes-dashboard-transport-session-title
				       client :session-id session-id :title title)
   (lambda (message)
     (if (hermes-sessions--session-not-found-message-p message)
         (hermes--promise-then
          (hermes-dashboard-transport-call-fn #'hermes-dashboard-transport-session-resume
					      client session-id)
          (lambda (result)
            (let ((live-id (hermes-transport--display-field result 'session_id)))
              (hermes-dashboard-transport-call-fn
               #'hermes-dashboard-transport-session-title
               client
               :session-id (if (string-empty-p live-id) session-id live-id)
               :title title))))
       (hermes--promise-rejected message)))))

(defun hermes-sessions--session-with-title (session title)
  "Return SESSION with TITLE shadowing its previous title field."
  (cons (cons 'title title) session))

(defun hermes-sessions--replace-browser-row-title (id title)
  "Replace browser row ID's title with TITLE in the current buffer."
  (when-let* ((entry (assoc id tabulated-list-entries)))
    (aset (cadr entry) 1 title)
    (when hermes-sessions--session-map
      (let ((session (gethash id hermes-sessions--session-map)))
        (when session
          (puthash id (hermes-sessions--session-with-title session title)
                   hermes-sessions--session-map))))
    (tabulated-list-print t)))

(defun hermes-sessions--detail-buffer-for-id (id)
  "Return the detail buffer for session ID, or nil."
  (get-buffer (format "*Hermes Session: %s*" id)))

(defun hermes-sessions--after-rename (_buffer id title)
  "Update open session buffers after session ID was renamed to TITLE."
  (when-let* ((browser (get-buffer "*Hermes Sessions*")))
    (with-current-buffer browser
      (when (derived-mode-p 'hermes-sessions-mode)
        (hermes-sessions--replace-browser-row-title id title))))
  (when-let* ((detail (hermes-sessions--detail-buffer-for-id id)))
    (with-current-buffer detail
      (when (derived-mode-p 'hermes-session-detail-mode)
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
           (origin (current-buffer)))
      (when (hermes-sessions--title-empty-p title)
        (user-error "Session title required"))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-sessions--set-title-promise client id (string-trim title)))
       (lambda (_result)
         (message "Hermes: renamed session %s" id)
         (hermes-sessions--after-rename origin id (string-trim title)))))))

(defun hermes-sessions--remove-browser-row (id)
  "Remove browser row ID from the current buffer."
  (setq tabulated-list-entries
        (cl-remove id tabulated-list-entries :key #'car :test #'equal))
  (when hermes-sessions--session-map
    (remhash id hermes-sessions--session-map))
  (tabulated-list-print t))

(defun hermes-sessions--after-delete (_buffer id)
  "Update open session buffers after session ID was deleted."
  (when-let* ((browser (get-buffer "*Hermes Sessions*")))
    (with-current-buffer browser
      (when (derived-mode-p 'hermes-sessions-mode)
        (hermes-sessions--remove-browser-row id))))
  (when-let* ((detail (hermes-sessions--detail-buffer-for-id id)))
    (kill-buffer detail)))

(defun hermes-sessions-delete ()
  "Delete the selected Hermes session after an explicit confirmation prompt."
  (interactive)
  (let* ((session (hermes-sessions--selected-session))
         (id (hermes-sessions--id session))
         (title (hermes-transport--display-field session 'title))
         (origin (current-buffer)))
    (when (string-empty-p id)
      (user-error "No Hermes session id to delete"))
    (if (yes-or-no-p
         (format "Delete Hermes session %s%s? "
                 id
                 (if (string-empty-p title) "" (format " (%s)" title))))
        (hermes-browser--run-on-client
         (lambda (client)
           (hermes-dashboard-transport-call-fn
            #'hermes-dashboard-transport-session-delete client id))
         (lambda (_result)
           (message "Hermes: deleted session %s" id)
           (hermes-sessions--after-delete origin id)))
      (message "Hermes: delete cancelled"))))

(provide 'hermes-sessions)
;;; hermes-sessions.el ends here
