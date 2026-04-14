;;; vm-user-test.el --- Tests for vm-user.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM user interface functions in vm-user.el

;;; Code:

(require 'vm-test-init)
(require 'vm-user)

;;; Function existence tests

(ert-deftest vm-user-test-functions-exist ()
  "Test that user interface functions exist."
  (should (fboundp 'vm-user-composition-folder-buffer))
  (should (fboundp 'vm-user-composition-real-folder-buffers)))

;;; vm-user-composition-folder-buffer tests

(ert-deftest vm-user-test-composition-folder-buffer-not-mail-mode ()
  "Test vm-user-composition-folder-buffer returns nil when not in mail-mode."
  (with-temp-buffer
    (should (null (vm-user-composition-folder-buffer)))))

(ert-deftest vm-user-test-composition-folder-buffer-mail-mode-no-buffer ()
  "Test vm-user-composition-folder-buffer with mail-mode but no vm-mail-buffer."
  (with-temp-buffer
    (let ((major-mode 'mail-mode)
          (vm-mail-buffer nil))
      (should (null (vm-user-composition-folder-buffer))))))

(ert-deftest vm-user-test-composition-folder-buffer-mail-mode-with-buffer ()
  "Test vm-user-composition-folder-buffer returns vm-mail-buffer in mail-mode."
  (with-temp-buffer
    (let ((folder-buf (current-buffer)))
      (with-temp-buffer
        (let ((major-mode 'mail-mode)
              (vm-mail-buffer folder-buf))
          (should (eq folder-buf (vm-user-composition-folder-buffer))))))))

;;; vm-user-composition-real-folder-buffers tests

(ert-deftest vm-user-test-composition-real-folder-buffers-not-mail-mode ()
  "Test vm-user-composition-real-folder-buffers returns nil when not in mail-mode."
  (with-temp-buffer
    (should (null (vm-user-composition-real-folder-buffers)))))

(ert-deftest vm-user-test-composition-real-folder-buffers-mail-mode-no-state ()
  "Test vm-user-composition-real-folder-buffers with mail-mode but no state."
  (with-temp-buffer
    (let ((major-mode 'mail-mode)
          (vm-system-state nil)
          (vm-reply-list nil)
          (vm-forward-list nil)
          (vm-redistribute-list nil))
      (should (null (vm-user-composition-real-folder-buffers))))))

(provide 'vm-user-test)

;;; vm-user-test.el ends here