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

(provide 'vm-menu-test)

;;; vm-menu-test.el ends here