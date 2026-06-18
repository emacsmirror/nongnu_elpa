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
;; resumes the selected session in a fresh chat buffer.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-chat)

(defun hermes-sessions--existing-client ()
  "Return a live dashboard client from any Hermes chat buffer, or nil."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'hermes-chat-mode)
                    (hermes-chat--dashboard-client-live-p
                     hermes-chat--dashboard-client)
                    hermes-chat--dashboard-client)))
           (buffer-list)))

(defun hermes-sessions--with-client (fn)
  "Call FN with a connected CLIENT and a DONE cleanup thunk.
Reuses a live chat connection when one exists; otherwise connects a transient
client that DONE stops.  Shared by the dashboard browser commands."
  (let* ((existing (hermes-sessions--existing-client))
         (client (or existing
                     (hermes-dashboard-transport-start :callback #'ignore)))
         (done (lambda ()
                 (unless existing
                   (hermes-dashboard-transport-stop client)))))
    (funcall fn client done)))

(defun hermes-sessions--field (session key)
  "Return SESSION's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get session key)) ""))

(defun hermes-sessions--rows (sessions)
  "Return `tabulated-list' entries for SESSIONS, a list of session alists."
  (mapcar
   (lambda (session)
     (let ((id (hermes-sessions--field session 'id)))
       (list id
             (vector id
                     (hermes-sessions--field session 'title)
                     (format "%s" (or (hermes-transport--get session 'message_count) 0))
                     (hermes-sessions--field session 'source)))))
   sessions))

(defun hermes-sessions--revert (&rest _)
  "Refresh the Hermes session list."
  (hermes-list-sessions))

(defvar-keymap hermes-sessions-mode-map
  :doc "Keymap for `hermes-sessions-mode'."
  :parent tabulated-list-mode-map
  "RET" #'hermes-sessions-open)

(define-derived-mode hermes-sessions-mode tabulated-list-mode "Hermes Sessions"
  "Major mode listing resumable Hermes dashboard sessions."
  :interactive nil
  (setq tabulated-list-format
        [("Session" 22 t) ("Title" 40 t) ("Msgs" 6 t) ("Source" 12 t)])
  (setq-local revert-buffer-function #'hermes-sessions--revert)
  (tabulated-list-init-header))

(defun hermes-sessions--render (sessions)
  "Display SESSIONS in the Hermes sessions buffer."
  (with-current-buffer (get-buffer-create "*Hermes Sessions*")
    (unless (derived-mode-p 'hermes-sessions-mode)
      (hermes-sessions-mode))
    (setq tabulated-list-entries (hermes-sessions--rows sessions))
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun hermes-sessions-open ()
  "Resume the Hermes session on the current row in a chat buffer."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (unless id
      (user-error "No Hermes session on this line"))
    (hermes-chat-resume-session id (and entry (aref entry 1)))))

;;;###autoload
(defun hermes-list-sessions ()
  "List resumable Hermes dashboard sessions in a browser buffer.
Reuses a live chat connection when one exists; otherwise connects a transient
client just for the listing."
  (interactive)
  (hermes-sessions--with-client
   (lambda (client done)
     (hermes-dashboard-transport-session-list
      client
      :resolve (lambda (result)
                 (funcall done)
                 (hermes-sessions--render (hermes-transport--get result 'sessions)))
      :reject (lambda (message)
                (funcall done)
                (message "Hermes: %s" message))))))

(provide 'hermes-sessions)
;;; hermes-sessions.el ends here
