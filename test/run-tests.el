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

;; Load all test files (discovered dynamically)
(vm-test-load-all-test-files)

;; Run tests in batch mode
(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; run-tests.el ends here
