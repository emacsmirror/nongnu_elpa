;;; hermes-rollback.el --- Checkpoint rollback browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard checkpoint methods
;; (`rollback.list'/`rollback.diff'/`rollback.restore').  RET shows a
;; checkpoint diff rendered through `diff-mode'; `x' restores the working tree
;; and rewinds the owning session's conversation history.

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

(defvar-local hermes-rollback--owner nil
  "Exact chat attachment selected for this browser.
The value is (BUFFER INSTANCE CLIENT SESSION TRANSPORT LIFETIME).")

(defvar-local hermes-rollback--snapshot nil
  "Unique identity of the currently actionable checkpoint read.")

(defun hermes-rollback--chat-owner (buffer instance)
  "Return BUFFER's live attachment for INSTANCE, or nil."
  (when (hermes-browser--buffer-mode-p buffer 'hermes-chat-mode)
    (with-current-buffer buffer
      (when (and (equal hermes-instance instance)
                 hermes-chat--dashboard-session-ready-p
                 hermes-chat--dashboard-active-session-id
                 (hermes-chat--dashboard-client-live-p
                  hermes-chat--dashboard-client))
        (list buffer (copy-tree instance) hermes-chat--dashboard-client
              (copy-sequence hermes-chat--dashboard-active-session-id)
              hermes-chat--transport-generation hermes-chat--lifecycle-generation)))))

(defun hermes-rollback--choose-owner (instance)
  "Choose a live chat attachment for INSTANCE, preferring the current chat."
  (or (hermes-rollback--chat-owner (current-buffer) instance)
      (let* ((owners (delq nil (mapcar
                               (lambda (buffer)
                                 (hermes-rollback--chat-owner buffer instance))
                               (buffer-list))))
             (choices (mapcar (lambda (owner)
                                (cons (format "%s [%s]" (buffer-name (car owner))
                                              (nth 3 owner)) owner))
                              owners)))
        (pcase owners
          ('nil (user-error "Hermes rollback needs a live chat session on this instance"))
          (`(,owner) owner)
          (_ (cdr (assoc (completing-read "Checkpoint session: " choices nil t)
                         choices)))))))

(defun hermes-rollback--owner-current-p (owner)
  "Return non-nil if OWNER still names the exact live chat attachment."
  (and owner
       (let ((current (hermes-rollback--chat-owner (car owner) (nth 1 owner))))
         (and current (eq (nth 2 owner) (nth 2 current))
              (equal owner current)))))

(defun hermes-rollback--current-p (origin owner snapshot)
  "Return non-nil if ORIGIN still owns OWNER and SNAPSHOT."
  (and (hermes-browser--buffer-mode-p origin 'hermes-rollback-mode)
       (eq owner (buffer-local-value 'hermes-rollback--owner origin))
       (eq snapshot (buffer-local-value 'hermes-rollback--snapshot origin))
       (equal (nth 1 owner) (buffer-local-value 'hermes-instance origin))
       (hermes-rollback--owner-current-p owner)))

(defun hermes-rollback--require-snapshot ()
  "Return the current snapshot or reject a stale checkpoint selection."
  (unless (and hermes-rollback--snapshot
               (eq (car hermes-rollback--snapshot) hermes-rollback--owner)
               (hermes-rollback--current-p
                (current-buffer) hermes-rollback--owner hermes-rollback--snapshot))
    (user-error "Checkpoint attachment changed; reopen the rollback browser"))
  hermes-rollback--snapshot)

(defun hermes-rollback--fetch (_client)
  "Fetch checkpoints on the browser's exact selected chat attachment.
Ignore _CLIENT: the shared browser may have acquired a different chat client."
  (setq hermes-rollback--snapshot nil)
  (let ((origin (current-buffer))
        (owner hermes-rollback--owner)
        (generation hermes-browser--request-generation))
    (if (not (hermes-rollback--current-p origin owner nil))
        (hermes--promise-rejected "Checkpoint attachment changed; reopen the rollback browser")
      (hermes--promise-map
       (hermes-dashboard-transport-call-fn
        #'hermes-dashboard-transport-rollback-list (nth 2 owner)
        :session-id (nth 3 owner))
       (lambda (result)
         (unless (and (hermes-rollback--current-p origin owner nil)
                      (hermes-browser--request-current-p origin generation))
           (error "Checkpoint attachment changed during refresh"))
         (with-current-buffer origin
           (setq hermes-rollback--snapshot (list owner)))
         result)))))

(defun hermes-rollback--rows (result)
  "Return `tabulated-list' entries for a `rollback.list' RESULT."
  (mapcar
   (lambda (checkpoint)
     (let ((hash (hermes-transport--scalar-string
                  (hermes-transport--get checkpoint 'hash))))
       (list hash
             (vector (hermes-browser--face-cell
                      (hermes-rollback--short hash) 'hermes-browser-identifier)
                     (hermes-browser--face-cell
                     (or (hermes-transport--scalar-string
                           (hermes-transport--get checkpoint 'timestamp)) "")
                      'hermes-browser-timestamp)
                     (hermes-browser--face-cell
                      (or (hermes-transport--scalar-string
                           (hermes-transport--get checkpoint 'message)) "")
                      'hermes-browser-message)))))
   (hermes-transport--get result 'checkpoints)))

(defun hermes-rollback--display-diff (hash result)
  "Render the diff for checkpoint HASH from RESULT through `diff-mode'."
  (let ((diff (hermes-transport--scalar-string
               (hermes-transport--get result 'diff))))
    (if (or (null diff) (string-empty-p diff))
        (message "Hermes: no diff for %s" (hermes-rollback--short hash))
      (hermes-chat--show-diff diff "*Hermes Rollback Diff*"))))

(defun hermes-rollback--run (make-promise on-success on-error)
  "Run MAKE-PROMISE with ON-SUCCESS and ON-ERROR without acquiring a client."
  (hermes--promise-catch
   (hermes--promise-then
    (condition-case err
        (funcall make-promise nil)
      ((error quit) (hermes--promise-rejected (error-message-string err))))
    on-success)
   on-error))

(defun hermes-rollback-show-diff ()
  "Show the diff for the checkpoint at point on its original attachment."
  (interactive)
  (let ((hash (tabulated-list-get-id))
        (snapshot (hermes-rollback--require-snapshot))
        (owner hermes-rollback--owner)
        (origin (current-buffer))
        (generation (hermes-browser--next-request-generation)))
    (unless hash (user-error "No checkpoint on this line"))
    (hermes-rollback--run
     (lambda (_client)
       (unless (hermes-rollback--current-p origin owner snapshot)
         (user-error "Checkpoint attachment changed"))
       (hermes-dashboard-transport-call-fn
        #'hermes-dashboard-transport-rollback-diff (nth 2 owner) hash
        :session-id (nth 3 owner)))
     (lambda (result)
       (when (and (hermes-rollback--current-p origin owner snapshot)
                  (hermes-browser--request-current-mode-p
                   origin generation 'hermes-rollback-mode))
         (hermes-rollback--display-diff hash result)))
     (lambda (reason)
       (when (and (hermes-rollback--current-p origin owner snapshot)
                  (hermes-browser--request-current-p origin generation))
         (message "Hermes: %s" reason))))))

(defun hermes-rollback--checked-restore (result)
  "Return restore RESULT, or signal when it declares failure."
  (if (and (hermes-transport--field-present-p result 'success)
           (not (eq (hermes-transport--get result 'success) t)))
      (error "%s" (or (hermes-transport--non-blank-string
                        (hermes-transport--display-field result 'error))
                       "Rollback restore failed"))
    result))

(defun hermes-rollback-restore ()
  "Restore the working tree and rewind conversation history to the checkpoint."
  (interactive)
  (let ((hash (tabulated-list-get-id))
        (snapshot (hermes-rollback--require-snapshot))
        (origin (current-buffer))
        (owner hermes-rollback--owner)
        (operation (list 'restore)))
    (unless hash (user-error "No checkpoint on this line"))
    (when (yes-or-no-p
           (format "Restore working tree and rewind conversation history for %s to checkpoint %s? "
                   (nth 3 owner) (hermes-rollback--short hash)))
      (unless (hermes-rollback--current-p origin owner snapshot)
        (user-error "Checkpoint attachment changed during confirmation"))
      (hermes-rollback--run
       (lambda (_client)
         (unless (hermes-rollback--current-p origin owner snapshot)
           (user-error "Checkpoint attachment changed"))
         ;; Once dispatched, this snapshot must not admit another mutation.
         (with-current-buffer origin (setq hermes-rollback--snapshot operation))
         (hermes--promise-map
          (hermes-dashboard-transport-call-fn
           #'hermes-dashboard-transport-rollback-restore (nth 2 owner) hash
           :session-id (nth 3 owner))
          #'hermes-rollback--checked-restore))
       (lambda (_result)
         (when (hermes-rollback--current-p origin owner operation)
           (message "Hermes: restored %s" (hermes-rollback--short hash))
           (with-current-buffer origin
             (setq hermes-rollback--snapshot nil)
             (hermes-rollback--revert))))
       (lambda (reason)
         (when (hermes-rollback--current-p origin owner operation)
           (with-current-buffer origin (setq hermes-rollback--snapshot nil))
           (message "Hermes: %s; refresh checkpoints before retrying" reason)))))))

(defun hermes-rollback--setup ()
  "Register attachment and snapshot state for instance invalidation."
  (setq hermes-browser--snapshot-variables
        '(hermes-rollback--owner hermes-rollback--snapshot)))

(hermes-define-list-browser rollback
  :title "Hermes Rollbacks"
  :buffer "*Hermes Rollbacks*"
  :command hermes-rollback--list
  :doc "Major mode listing Hermes session checkpoints."
  :command-doc "Browse Hermes checkpoint history for the active session."
  :columns [("Checkpoint" 10 t) ("When" 22 t) ("Message" 50 nil)]
  :on-mode #'hermes-rollback--setup
  :fetch #'hermes-rollback--fetch
  :rows #'hermes-rollback--rows
  :help (:group "Checkpoint"
         hermes-rollback-show-diff "View diff"
         hermes-rollback-restore "Restore")
  :keys ("RET" #'hermes-rollback-show-diff
         "d" #'hermes-rollback-show-diff
         "x" #'hermes-rollback-restore))

;;;###autoload
(defun hermes-list-rollbacks ()
  "Browse checkpoints for the current chat, or choose a same-instance chat."
  (interactive)
  (let* ((instance (hermes-instance-resolve))
         (owner (hermes-rollback--choose-owner instance)))
    (unless (hermes-rollback--owner-current-p owner)
      (user-error "Checkpoint attachment changed during selection"))
    (with-current-buffer (get-buffer-create "*Hermes Rollbacks*")
      (unless (derived-mode-p 'hermes-rollback-mode) (hermes-rollback-mode))
      (hermes-browser--own-instance instance)
      (setq hermes-rollback--owner owner
            hermes-rollback--snapshot nil
            tabulated-list-entries nil)
      (tabulated-list-print)
      (hermes-rollback--list))))

(provide 'hermes-rollback)
;;; hermes-rollback.el ends here
