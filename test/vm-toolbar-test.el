;;; vm-toolbar-test.el --- Tests for vm-toolbar.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The suite runs with `vm-use-toolbar' nil, because installing the toolbar
;; defines tool-bar keys in `vm-mode-map' -- a keymap every folder buffer shares
;; -- and records that it has done so in a global, neither of which a fixture
;; puts back (issue #559).  See the comment in vm-test-init.el.
;;
;; So this is the cover the installation has.  It binds what it touches.

;;; Code:

(require 'vm-test-init)
(require 'vm-toolbar)

(ert-deftest vm-toolbar-test-installing-defines-the-buttons ()
  "Installing the toolbar defines tool-bar keys and says it has.
`vm-mode-internal' does this for each folder, once per session, guarded by
`vm-fsfemacs-toolbar-installed-p'."
  (skip-unless (and (not (featurep 'xemacs)) (vm-toolbar-support-possible-p)))
  (let ((vm-use-toolbar vm-test-vm-use-toolbar)
        (vm-mode-map (copy-keymap vm-mode-map))
        (vm-fsfemacs-toolbar-installed-p nil))
    (should-not (lookup-key vm-mode-map [tool-bar]))
    (vm-toolbar-install-toolbar)
    (should vm-fsfemacs-toolbar-installed-p)
    ;; the buttons named in `vm-use-toolbar' are there, under those names
    (let ((toolbar (lookup-key vm-mode-map [tool-bar])))
      (should (keymapp toolbar))
      (dolist (button '(getmail next previous delete undelete reply quit))
        (should (lookup-key toolbar (vector button)))))))

(ert-deftest vm-toolbar-test-installing-twice-is-a-no-op ()
  "The flag is what stops a second install, so it is worth pinning.
Without it every folder visit would define the keys again."
  (skip-unless (and (not (featurep 'xemacs)) (vm-toolbar-support-possible-p)))
  (let ((vm-use-toolbar vm-test-vm-use-toolbar)
        (vm-mode-map (copy-keymap vm-mode-map))
        (vm-fsfemacs-toolbar-installed-p t)
        (installs 0))
    (cl-letf* ((real (symbol-function 'vm-toolbar-fsfemacs-install-toolbar))
               ((symbol-function 'vm-toolbar-fsfemacs-install-toolbar)
                (lambda (&rest args) (setq installs (1+ installs)) (apply real args))))
      (vm-toolbar-install-toolbar)
      (should (= 0 installs)))))

(ert-deftest vm-toolbar-test-update-answers-for-the-current-folder ()
  "The toolbar's state follows the folder: what is deleted, and what it can do.
`vm-toolbar-update-toolbar' picks the delete or undelete icon from the current
message and a helper command from `vm-toolbar-can-recover-p',
`vm-toolbar-can-decode-mime-p' and `vm-toolbar-mail-waiting-p'.  With the toolbar
off for the suite nothing else calls these, and they are the only logic it has
beyond the wiring above."
  (vm-test-with-real-folder (2)
    (let ((vm-toolbar-delete/undelete-icon nil)
          (vm-toolbar-helper-command nil)
          ;; Deleting queues the message for a summary update, and the queue is
          ;; global; nothing here redraws a summary, so it would be left on it.
          (vm-messages-needing-summary-update vm-messages-needing-summary-update))
      ;; nothing deleted: the button offers to delete
      (vm-toolbar-update-toolbar)
      (should (eq vm-toolbar-delete/undelete-icon vm-toolbar-delete-icon))
      ;; and with the message deleted it offers to undelete
      (vm-set-deleted-flag (car vm-message-pointer) t)
      (vm-toolbar-update-toolbar)
      (should (eq vm-toolbar-delete/undelete-icon vm-toolbar-undelete-icon))
      ;; the folder is not read-only and has no autosave newer than itself
      (should-not (vm-toolbar-can-recover-p))
      ;; each predicate answers rather than signalling, which is all their
      ;; `condition-case' promises
      (should (memq (and (vm-toolbar-can-decode-mime-p) t) '(nil t)))
      (should (memq (and (vm-toolbar-mail-waiting-p) t) '(nil t))))))

(provide 'vm-toolbar-test)

;;; vm-toolbar-test.el ends here
