;;; jabber-test-menu.el --- Tests for jabber-menu  -*- lexical-binding: t; -*-

;;; Commentary:

;; Keymap popup menu structure.

;;; Code:

(require 'ert)
(require 'jabber-bookmarks)
(require 'jabber-chat-commands)
(require 'jabber-disco-menu)
(require 'jabber-keymap)
(require 'jabber-muc-menu)
(require 'jabber-omemo-trust)
(require 'jabber-roster-menu)

;;; Helpers

(defun jabber-test-menu--extract-popup-commands (keymap)
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

(ert-deftest jabber-test-menu-popup-commands-defined ()
  "Every command in a jabber popup keymap must be fboundp."
  (let ((maps (list jabber-common-keymap
                    jabber-global-keymap
                    jabber-chat-operations-menu-map
                    jabber-chat-encryption-menu-map
                    jabber-roster-popup-map
                    jabber-roster-presence-map
                    jabber-roster-discovery-map
                    jabber-roster-contact-action-map
                    jabber-info-menu-map
                    jabber-muc-menu-map
                    jabber-service-menu-map
                    jabber-bookmarks-mode-map
                    jabber-bookmarks-edit-map
                    jabber-omemo-trust-mode-map))
        (missing nil))
    (dolist (map maps)
      (dolist (cmd (jabber-test-menu--extract-popup-commands map))
        (unless (fboundp cmd)
          (push (format "%s" cmd) missing))))
    (should (null missing))))

(ert-deftest jabber-test-menu-thread-commands ()
  "Expose thread roots in the menu and keep normal thread sending on RET."
  (should
   (eq (keymap-lookup jabber-chat-operations-menu-map "t")
       'jabber-message-thread-open))
  (should
   (eq (keymap-lookup jabber-chat-operations-menu-map "T")
       'jabber-message-thread-start))
  (should
   (eq (keymap-lookup jabber-chat-operations-menu-map "l")
       'jabber-message-thread-browse))
  (let ((jabber-message-thread-id nil))
    (should-not
     (keymap-lookup jabber-chat-operations-menu-map "L")))
  (let ((jabber-message-thread-id "thread-id"))
    (should
     (eq (keymap-lookup jabber-chat-operations-menu-map "L")
         'jabber-message-thread-set-title)))
  (should
   (eq (keymap-lookup jabber-chat-mode-map "C-c C-t")
       'jabber-message-thread-open))
  (should
   (eq (keymap-lookup jabber-chat-mode-map "RET")
       'jabber-chat-goto-reply-target-or-send)))

(provide 'jabber-test-menu)
;;; jabber-test-menu.el ends here
