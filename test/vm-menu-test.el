;;; vm-menu-test.el --- Tests for vm-menu.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM menu functions in vm-menu.el

;;; Code:

(require 'vm-test-init)
(require 'vm-menu)

;;; Menu constant existence tests

(ert-deftest vm-menu-test-menu-constants-exist ()
  "Test that menu constant definitions exist."
  (should (boundp 'vm-menu-folders-menu))
  (should (boundp 'vm-menu-folder-menu))
  (should (boundp 'vm-menu-dispose-menu)))

;;; Function existence tests

(ert-deftest vm-menu-test-popup-functions-exist ()
  "Test that popup menu functions exist."
  (should (fboundp 'vm-menu-popup-mode-menu))
  (should (fboundp 'vm-menu-popup-context-menu))
  (should (fboundp 'vm-menu-popup-url-browser-menu))
  (should (fboundp 'vm-menu-popup-mime-dispose-menu))
  (should (fboundp 'vm-menu-popup-fsfemacs-menu)))

(ert-deftest vm-menu-test-install-functions-exist ()
  "Test that menu installation functions exist."
  (should (fboundp 'vm-menu-install-menus))
  (should (fboundp 'vm-menu-install-mail-mode-menu))
  (should (fboundp 'vm-menu-install-visited-folders-menu))
  (should (fboundp 'vm-menu-install-known-virtual-folders-menu)))

(ert-deftest vm-menu-test-helper-functions-exist ()
  "Test that helper functions exist."
  (should (fboundp 'vm-menu-can-get-new-mail-p))
  (should (fboundp 'vm-menu-can-save-p))
  (should (fboundp 'vm-menu-can-revert-p))
  (should (fboundp 'vm-menu-can-recover-p))
  (should (fboundp 'vm-menu-can-expunge-pop-messages-p))
  (should (fboundp 'vm-menu-can-expunge-imap-messages-p)))

(ert-deftest vm-menu-test-folder-functions-exist ()
  "Test that folder menu functions exist."
  (should (fboundp 'vm-menu-hm-make-folder-menu))
  (should (fboundp 'vm-menu-hm-tree-make-menu)))

;;; Variable existence tests

(ert-deftest vm-menu-test-variables-exist ()
  "Test that menu-related variables exist."
  (should (boundp 'vm-use-menus))
  (should (boundp 'vm-popup-menu-on-mouse-3)))

;;; Menu structure tests

(ert-deftest vm-menu-test-folder-menu-is-list ()
  "Test that vm-menu-folder-menu is a list."
  (should (listp vm-menu-folder-menu)))

(ert-deftest vm-menu-test-folder-menu-has-name ()
  "Test that vm-menu-folder-menu has a name."
  (should (stringp (car vm-menu-folder-menu))))

(ert-deftest vm-menu-test-dispose-menu-is-list ()
  "Test that vm-menu-dispose-menu is a list."
  (should (listp vm-menu-dispose-menu)))

(ert-deftest vm-menu-test-dispose-menu-has-name ()
  "Test that vm-menu-dispose-menu has a name."
  (should (stringp (car vm-menu-dispose-menu))))

;;; Predicate function tests

(ert-deftest vm-menu-test-can-save-p-without-folder ()
  "Test vm-menu-can-save-p returns nil outside folder context."
  (with-temp-buffer
    (should (null (vm-menu-can-save-p)))))

(ert-deftest vm-menu-test-can-revert-p-without-folder ()
  "Test vm-menu-can-revert-p returns nil outside folder context."
  (with-temp-buffer
    (should (null (vm-menu-can-revert-p)))))

(ert-deftest vm-menu-test-can-recover-p-without-folder ()
  "Test vm-menu-can-recover-p returns nil outside folder context."
  (with-temp-buffer
    (should (null (vm-menu-can-recover-p)))))

;;; Installing the menus (issue #559)

;; The suite runs with `vm-use-menus' nil, because installing the menus splices
;; the visited folders into `vm-menu-folder-menu' and rebuilds
;; `vm-menu-fsfemacs-folder-menu' from it, which no fixture puts back.  See the
;; comment in vm-test-init.el.  This is the one test that switches them on, so it
;; is the only cover the installation has: everything it touches is bound here,
;; the two menu structures deeply, since the splice is by `setcdr'.

(ert-deftest vm-menu-test-visiting-a-folder-fills-in-the-folder-menu ()
  "Visiting a folder adds it to the Folder menu and rebuilds the keymap.
`vm-menu-install-visited-folders-menu' splices `vm-folder-history' into
`vm-menu-folder-menu' after its \"---\" marker and defines
`vm-menu-fsfemacs-folder-menu' from the result."
  (require 'vm)
  (let ((vm-use-menus vm-test-vm-use-menus)
        (vm-menu-folder-menu (copy-tree vm-menu-folder-menu))
        (vm-menu-virtual-menu (copy-tree vm-menu-virtual-menu))
        (vm-menu-fsfemacs-folder-menu (bound-and-true-p
                                       vm-menu-fsfemacs-folder-menu))
        (vm-menu-fsfemacs-virtual-menu (bound-and-true-p
                                        vm-menu-fsfemacs-virtual-menu)))
    (vm-test-with-real-folder (2)
      (let ((file (buffer-file-name)))
        (should file)
        ;; the folder is in the menu, as an item whose suffix is its name
        (should (seq-find (lambda (item)
                            (and (vectorp item)
                                 (equal (plist-get (append item nil) :suffix)
                                        file)))
                          vm-menu-folder-menu))
        ;; and the keymap was rebuilt, so it is a keymap naming the same folder
        (should (keymapp vm-menu-fsfemacs-folder-menu))
        (should (string-match-p (regexp-quote file)
                                (format "%S" vm-menu-fsfemacs-folder-menu)))))))

(ert-deftest vm-menu-test-mail-mode-menu-goes-on-the-mail-map ()
  "Installing the composition menu puts it on `mail-mode-map'.
`vm-reply' calls this as a composition begins, under `vm-use-menus', which the
suite has off, so this is the cover it has.  Both keymaps are bound to copies:
the function defines keys in shared maps, which is state no fixture restores."
  (require 'vm)
  (let ((mail-mode-map (copy-keymap mail-mode-map))
        (vm-mail-mode-map (copy-keymap vm-mail-mode-map))
        (vm-use-menus vm-test-vm-use-menus))
    ;; Emacs has its own Mail menu there, so this has to replace it
    (should-not (eq (lookup-key mail-mode-map [menu-bar mail])
                    vm-menu-fsfemacs-mail-menu))
    (vm-menu-install-mail-mode-menu)
    (let ((entry (lookup-key mail-mode-map [menu-bar mail])))
      (should (keymapp entry))
      (should (eq entry vm-menu-fsfemacs-mail-menu)))))

(ert-deftest vm-menu-test-menubar-buttons-are-not-possible-without-a-window-system ()
  "`vm-menubar-buttons-possible-p' answers for the current windowing system.
A menubar button is a menu entry that acts when clicked, which GTK and NeXTstep
do not allow.  In batch there is no window system, so they are possible, which
is the branch that decides whether `vm-menu-fsfemacs-add-vm-menu' makes a button
or a menu."
  (should (null window-system))
  (should (vm-menubar-buttons-possible-p)))

(provide 'vm-menu-test)

;;; vm-menu-test.el ends here