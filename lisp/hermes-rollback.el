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

(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-sessions)
(require 'hermes-chat)

(defun hermes-rollback--short (hash)
  "Return an abbreviated form of checkpoint HASH."
  (if (and hash (> (length hash) 8)) (substring hash 0 8) (or hash "")))

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

(defun hermes-rollback--revert (&rest _)
  "Refresh the checkpoint list."
  (hermes-list-rollbacks))

(defvar-keymap hermes-rollback-mode-map
  :doc "Keymap for `hermes-rollback-mode'."
  :parent tabulated-list-mode-map
  "RET" #'hermes-rollback-show-diff
  "d" #'hermes-rollback-show-diff
  "x" #'hermes-rollback-restore)

(define-derived-mode hermes-rollback-mode tabulated-list-mode "Hermes Rollbacks"
  "Major mode listing Hermes session checkpoints."
  :interactive nil
  (setq tabulated-list-format
        [("Checkpoint" 10 t) ("When" 22 t) ("Message" 50 nil)])
  (setq-local revert-buffer-function #'hermes-rollback--revert)
  (tabulated-list-init-header))

(defun hermes-rollback--render (result)
  "Display checkpoints from RESULT in the rollbacks buffer."
  (with-current-buffer (get-buffer-create "*Hermes Rollbacks*")
    (unless (derived-mode-p 'hermes-rollback-mode)
      (hermes-rollback-mode))
    (setq tabulated-list-entries (hermes-rollback--rows result))
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

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
  (let ((hash (tabulated-list-get-id)))
    (unless hash (user-error "No checkpoint on this line"))
    (hermes-sessions--with-client
     (lambda (client done)
       (hermes--promise-catch
        (hermes--promise-then
         (hermes--promise-finally
          (hermes-dashboard-transport-call-fn
           #'hermes-dashboard-transport-rollback-diff client hash)
          done)
         (lambda (result) (hermes-rollback--display-diff hash result)))
        (lambda (message) (message "Hermes: %s" message)))))))

(defun hermes-rollback-restore ()
  "Restore the working tree to the checkpoint at point."
  (interactive)
  (let ((hash (tabulated-list-get-id)))
    (unless hash (user-error "No checkpoint on this line"))
    (when (yes-or-no-p
           (format "Restore working tree to checkpoint %s? "
                   (hermes-rollback--short hash)))
      (hermes-sessions--with-client
       (lambda (client done)
         (hermes--promise-catch
          (hermes--promise-then
           (hermes--promise-finally
            (hermes-dashboard-transport-call-fn
             #'hermes-dashboard-transport-rollback-restore client hash)
            done)
           (lambda (_result)
             (message "Hermes: restored %s" (hermes-rollback--short hash))))
          (lambda (message) (message "Hermes: %s" message))))))))

;;;###autoload
(defun hermes-list-rollbacks ()
  "Browse Hermes checkpoint history for the active session."
  (interactive)
  (hermes-sessions--with-client
   (lambda (client done)
     (hermes--promise-catch
      (hermes--promise-then
       (hermes--promise-finally
        (hermes-dashboard-transport-call-fn
         #'hermes-dashboard-transport-rollback-list client)
        done)
       (lambda (result) (hermes-rollback--render result)))
      (lambda (message) (message "Hermes: %s" message))))))

(provide 'hermes-rollback)
;;; hermes-rollback.el ends here
