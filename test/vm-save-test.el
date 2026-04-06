;;; vm-save-test.el --- Tests for vm-save.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM save functions in vm-save.el

;;; Code:

(require 'vm-test-init)
(require 'vm-save)

;;; Save function existence tests

(ert-deftest vm-save-test-functions-exist ()
  "Test that save functions exist."
  (should (fboundp 'vm-save-message))
  (should (fboundp 'vm-save-message-sans-headers))
  (should (fboundp 'vm-save-message-to-local-folder))
  (should (fboundp 'vm-auto-archive-messages))
  (should (fboundp 'vm-auto-select-folder)))

;;; Pipe function existence tests

(ert-deftest vm-save-test-pipe-functions-exist ()
  "Test that pipe functions exist."
  (should (fboundp 'vm-pipe-message-to-command))
  (should (fboundp 'vm-pipe-message-to-command-to-string))
  (should (fboundp 'vm-pipe-message-to-command-discard-output))
  (should (fboundp 'vm-pipe-messages-to-command))
  (should (fboundp 'vm-pipe-messages-to-command-to-string))
  (should (fboundp 'vm-pipe-messages-to-command-discard-output))
  (should (fboundp 'vm-pipe-message-part)))

;;; IMAP folder check

(ert-deftest vm-save-test-imap-folder-p-exists ()
  "Test that vm-imap-folder-p exists."
  (should (fboundp 'vm-imap-folder-p)))

;;; Print function

(ert-deftest vm-save-test-print-message-exists ()
  "Test that vm-print-message exists."
  (should (fboundp 'vm-print-message)))

(provide 'vm-save-test)

;;; vm-save-test.el ends here
