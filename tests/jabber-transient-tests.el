;;; jabber-transient-tests.el --- Tests for transient menu integration  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber)
(require 'transient)

;;; Helpers

(defun jabber-test--extract-suffix-commands (layout)
  "Recursively extract command symbols from transient LAYOUT."
  (let (commands)
    (cond
     ((null layout) nil)
     ((and (listp layout) (symbolp (car layout))
           (memq (car layout)
                 '(transient-suffix transient-switch transient-option)))
      ;; Suffix entry: (:key ... :command CMD ...)
      (let ((cmd (plist-get (cdr layout) :command)))
        (when (and cmd (symbolp cmd))
          (push cmd commands))))
     ((vectorp layout)
      (dotimes (i (length layout))
        (setq commands (nconc commands
                              (jabber-test--extract-suffix-commands
                               (aref layout i))))))
     ((listp layout)
      (dolist (elt layout)
        (setq commands (nconc commands
                              (jabber-test--extract-suffix-commands elt))))))
    commands))

;;; Tests

(ert-deftest jabber-test-transient-suffixes-defined ()
  "Every command in a jabber transient menu must be fboundp.
Catches the bug where a transient suffix references a command
whose module is neither required nor autoloaded."
  (let ((prefixes '(jabber-chat-operations-menu
                    jabber-chat-encryption-menu
                    jabber-chat-menu
                    jabber-roster-context-menu
                    jabber-info-menu
                    jabber-muc-menu
                    jabber-service-menu
                    jabber-roster-menu))
        (missing nil))
    (dolist (prefix prefixes)
      (let ((layout (get prefix 'transient--layout)))
        (dolist (cmd (jabber-test--extract-suffix-commands layout))
          (unless (fboundp cmd)
            (push (format "%s (in %s)" cmd prefix) missing)))))
    (should (null missing))))

(provide 'jabber-transient-tests)
;;; jabber-transient-tests.el ends here
