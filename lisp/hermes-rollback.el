;;; hermes-rollback.el --- Checkpoint rollback browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard checkpoint methods
;; (`rollback.list'/`rollback.diff'/`rollback.restore').  RET shows a
;; checkpoint diff rendered through `diff-mode'; `x' restores the working tree.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-promise)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-browser)
(require 'hermes-chat)

(defun hermes-rollback--short (hash)
  "Return an abbreviated form of checkpoint HASH."
  (if (and hash (> (length hash) 8)) (substring hash 0 8) (or hash "")))

(defun hermes-rollback--live-session-id ()
  "Return a live dashboard session id from any Hermes chat buffer, or nil.
The rollback methods are session-scoped server-side: the gateway resolves
checkpoints against the session's working directory and rejects a request
whose session id is missing or unknown."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'hermes-chat-mode)
                    hermes-chat--dashboard-session-ready-p
                    hermes-chat--dashboard-active-session-id)))
           (buffer-list)))

(defun hermes-rollback--require-session-id ()
  "Return a live dashboard session id, or signal a `user-error'."
  (or (hermes-rollback--live-session-id)
      (user-error "Hermes rollback needs a live chat session; open a chat first")))

(defun hermes-rollback--fetch (client)
  "Return a promise of `rollback.list' for the live chat session on CLIENT.
When no live session exists the promise rejects instead of signaling, so the
browser reports the error and still releases a transient client."
  (let ((session-id (hermes-rollback--live-session-id)))
    (if session-id
        (hermes-dashboard-transport-call-fn
         #'hermes-dashboard-transport-rollback-list client
         :session-id session-id)
      (hermes--promise-rejected
       "rollback needs a live chat session; open a Hermes chat first"))))

(defun hermes-rollback--rows (result)
  "Return `tabulated-list' entries for a `rollback.list' RESULT."
  (mapcar
   (lambda (checkpoint)
     (let ((hash (hermes-transport--scalar-string
                  (hermes-transport--get checkpoint 'hash))))
       (list hash
             (vector (hermes-rollback--short hash)
                     (or (hermes-transport--scalar-string
                          (hermes-transport--get checkpoint 'timestamp)) "")
                     (or (hermes-transport--scalar-string
                          (hermes-transport--get checkpoint 'message)) "")))))
   (hermes-transport--get result 'checkpoints)))

(defun hermes-rollback--display-diff (hash result)
  "Render the diff for checkpoint HASH from RESULT through `diff-mode'."
  (let ((diff (hermes-transport--scalar-string
               (hermes-transport--get result 'diff))))
    (if (or (null diff) (string-empty-p diff))
        (message "Hermes: no diff for %s" (hermes-rollback--short hash))
      (hermes-chat--show-diff diff "*Hermes Rollback Diff*"))))

(defun hermes-rollback-show-diff ()
  "Show the diff for the checkpoint at point."
  (interactive)
  (let ((hash (tabulated-list-get-id))
        (session-id (hermes-rollback--require-session-id)))
    (unless hash (user-error "No checkpoint on this line"))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-dashboard-transport-call-fn
        #'hermes-dashboard-transport-rollback-diff client hash
        :session-id session-id))
     (lambda (result) (hermes-rollback--display-diff hash result)))))

(defun hermes-rollback-restore ()
  "Restore the working tree to the checkpoint at point."
  (interactive)
  (let ((hash (tabulated-list-get-id))
        (session-id (hermes-rollback--require-session-id)))
    (unless hash (user-error "No checkpoint on this line"))
    (when (yes-or-no-p
           (format "Restore working tree to checkpoint %s? "
                   (hermes-rollback--short hash)))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-rollback-restore client hash
          :session-id session-id))
       (lambda (_result)
         (message "Hermes: restored %s" (hermes-rollback--short hash)))))))

;;;###autoload (autoload 'hermes-list-rollbacks "hermes-rollback" nil t)
(hermes-define-list-browser rollback
  :title "Hermes Rollbacks"
  :buffer "*Hermes Rollbacks*"
  :command hermes-list-rollbacks
  :doc "Major mode listing Hermes session checkpoints."
  :command-doc "Browse Hermes checkpoint history for the active session."
  :columns [("Checkpoint" 10 t) ("When" 22 t) ("Message" 50 nil)]
  :fetch #'hermes-rollback--fetch
  :rows #'hermes-rollback--rows
  :keys ("RET" #'hermes-rollback-show-diff
         "d" #'hermes-rollback-show-diff
         "x" #'hermes-rollback-restore))

(provide 'hermes-rollback)
;;; hermes-rollback.el ends here
