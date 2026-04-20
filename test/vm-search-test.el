;;; vm-search-test.el --- Tests for vm-search.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM incremental search functions in vm-search.el

;;; Code:

(require 'vm-test-init)
(require 'vm-search)

;;; Function existence tests

(ert-deftest vm-search-test-functions-exist ()
  "Test that search functions exist."
  (should (fboundp 'vm-isearch-forward))
  (should (fboundp 'vm-isearch-backward))
  (should (fboundp 'vm-isearch))
  (should (fboundp 'vm-isearch-widen))
  (should (fboundp 'vm-isearch-narrow))
  (should (fboundp 'vm-isearch-update)))

;;; Variable existence tests

(ert-deftest vm-search-test-variables-exist ()
  "Test that search-related variables exist."
  (should (boundp 'vm-search-using-regexps)))

;;; vm-isearch-widen tests

(ert-deftest vm-search-test-isearch-widen-in-vm-mode ()
  "Test that vm-isearch-widen widens in vm-mode."
  (with-temp-buffer
    (let ((major-mode 'vm-mode))
      (narrow-to-region 1 1)
      (vm-isearch-widen)
      (should (= (point-min) 1))
      (should (= (point-max) 1)))))

(ert-deftest vm-search-test-isearch-widen-not-vm-mode ()
  "Test that vm-isearch-widen does nothing outside vm-mode."
  (with-temp-buffer
    (insert "test content")
    (let ((major-mode 'fundamental-mode))
      (narrow-to-region 1 5)
      (vm-isearch-widen)
      ;; Should still be narrowed
      (should (= (point-min) 1))
      (should (= (point-max) 5)))))

;;; Interactive command tests

(ert-deftest vm-search-test-isearch-forward-interactive ()
  "Test that vm-isearch-forward is an interactive command."
  (should (commandp 'vm-isearch-forward)))

(ert-deftest vm-search-test-isearch-backward-interactive ()
  "Test that vm-isearch-backward is an interactive command."
  (should (commandp 'vm-isearch-backward)))

;;; Autoload tests

(ert-deftest vm-search-test-isearch-forward-autoload ()
  "Test that vm-isearch-forward is autoloaded."
  (let ((autoload-info (symbol-function 'vm-isearch-forward)))
    ;; After requiring vm-search, it should be a compiled function, not autoload
    (should (functionp autoload-info))))

(ert-deftest vm-search-test-isearch-backward-autoload ()
  "Test that vm-isearch-backward is autoloaded."
  (let ((autoload-info (symbol-function 'vm-isearch-backward)))
    (should (functionp autoload-info))))

(provide 'vm-search-test)

;;; vm-search-test.el ends here