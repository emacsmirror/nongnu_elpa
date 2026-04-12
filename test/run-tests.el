;;; run-tests.el --- Batch test runner for VM -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Batch test runner for VM test suite.
;; Run with: emacs -batch -Q -L lisp -l test/run-tests.el

;;; Code:

(setq load-prefer-newer t)

;; Determine test directory
(defvar vm-test-runner-dir
  (file-name-directory (or load-file-name buffer-file-name)))

;; Load test infrastructure
(load (expand-file-name "vm-test-init.el" vm-test-runner-dir))

;; Load all test files
(dolist (test-file '("vm-misc-test.el"
                     "vm-mime-test.el"
                     "vm-folder-test.el"
                     "vm-imap-test.el"
                     "vm-pop-test.el"
                     "vm-sort-test.el"
                     "vm-thread-test.el"
                     "vm-virtual-test.el"
                     "vm-delete-test.el"
                     "vm-mark-test.el"
                     "vm-save-test.el"
                     "vm-undo-test.el"
                     "vm-reply-test.el"
                     "vm-summary-test.el"
                     "vm-accessors-test.el"
                     "vm-integration-test.el"
                     "vm-window-test.el"
                     "vm-pcrisis-test.el"
                     "vm-postpone-test.el"))
  (let ((full-path (expand-file-name test-file vm-test-runner-dir)))
    (when (file-exists-p full-path)
      (load full-path))))

;; Run tests in batch mode
(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; run-tests.el ends here
