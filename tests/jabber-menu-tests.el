;;; jabber-menu-tests.el --- Tests for menu integration  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber)
(require 'jabber-autoloads)
(require 'keymap-popup)

;;; Helpers

(defun jabber-test--extract-popup-commands (keymap)
  "Extract jabber command symbols bound in KEYMAP.
Only returns symbols with a `jabber-' prefix, skipping
inherited bindings from parent mode keymaps."
  (let (commands)
    (map-keymap
     (lambda (_key binding)
       (when (and (symbolp binding)
                  (string-prefix-p "jabber-" (symbol-name binding)))
         (push binding commands)))
     keymap)
    commands))

;;; Tests

(ert-deftest jabber-test-popup-commands-defined ()
  "Every command in a jabber popup keymap must be fboundp."
  (let ((maps (list jabber-chat-operations-menu-map
                    jabber-chat-encryption-menu-map
                    jabber-chat-menu-map
                    jabber-roster-context-menu-map
                    jabber-info-menu-map
                    jabber-muc-menu-map
                    jabber-service-menu-map
                    jabber-roster-mode-map
                    jabber-bookmarks-mode-map
                    jabber-bookmarks-edit-map
                    jabber-omemo-trust-mode-map))
        (missing nil))
    (dolist (map maps)
      (dolist (cmd (jabber-test--extract-popup-commands map))
        (unless (fboundp cmd)
          (push (format "%s" cmd) missing))))
    (should (null missing))))

(provide 'jabber-menu-tests)
;;; jabber-menu-tests.el ends here
