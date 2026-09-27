;;; vm-search-test.el --- Tests for vm-search.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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

;;; vm-isearch-narrow and vm-isearch-update: what the search lands you in

;; `vm-isearch' widens the folder to search it, then narrows back to the
;; message the search stopped in and makes that the current message.  Those
;; two steps had no test: the widening did, so a search that left you looking
;; at the whole folder, or reading one message while the summary pointed at
;; another, would not have been noticed.

(defconst vm-search-test--folder
  (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
          "From: alice@example.com\nSubject: first\n\nBody of the first.\n\n"
          "From bob@example.com Mon Jan  1 00:00:00 2024\n"
          "From: bob@example.com\nSubject: second\n\nBody of the second.\n\n"
          "From carol@example.com Mon Jan  1 00:00:00 2024\n"
          "From: carol@example.com\nSubject: third\n\nBody of the third.\n\n")
  "Three messages, so that a search can stop in the middle one.")

(ert-deftest vm-search-test-isearch-narrow-shows-one-message ()
  "Narrowing after a search leaves the message the search stopped in visible.
From the headers down: the accessible region runs from the visible headers to
the end of the text, so the folder above and below is out of the way."
  (vm-test-with-folder vm-search-test--folder
    (setq major-mode 'vm-mode)
    (let ((m (nth 1 vm-message-list)))
      (setq vm-message-pointer (nthcdr 1 vm-message-list))
      (goto-char (vm-text-of m))
      (vm-isearch-narrow)
      (should (= (point-min) (vm-vheaders-of m)))
      (should (= (point-max) (vm-text-end-of m)))
      (should (string-match-p "second" (buffer-string)))
      (should-not (string-match-p "first" (buffer-string))))))

(ert-deftest vm-search-test-isearch-narrow-from-above-the-headers ()
  "With point above the visible headers, the message separator is included.
Otherwise narrowing would put point outside the accessible region, and the
search would appear to have jumped."
  (vm-test-with-folder vm-search-test--folder
    (setq major-mode 'vm-mode)
    (let ((m (nth 1 vm-message-list)))
      (setq vm-message-pointer (nthcdr 1 vm-message-list))
      (goto-char (vm-start-of m))
      (vm-isearch-narrow)
      (should (= (point-min) (vm-start-of m)))
      (should (<= (point-min) (point)))
      (should (<= (point) (point-max))))))

(ert-deftest vm-search-test-isearch-narrow-does-nothing-outside-vm-mode ()
  "A composition buffer is not a folder and must not be narrowed."
  (with-temp-buffer
    (insert "not a folder\n")
    (let ((major-mode 'mail-mode))
      (vm-isearch-narrow)
      (should (= (point-min) 1))
      (should (= (point-max) (1+ (buffer-size)))))))

(ert-deftest vm-search-test-isearch-update-follows-the-search ()
  "The message the search stopped in becomes the current message.
Reading one message while the summary arrow points at another is the bug this
holds shut."
  (vm-test-with-folder vm-search-test--folder
    (setq major-mode 'vm-mode)
    (setq vm-message-pointer vm-message-list)
    (cl-letf (((symbol-function 'vm-update-summary-and-mode-line) #'ignore))
      ;; the search has stopped in the third message
      (goto-char (vm-text-of (nth 2 vm-message-list)))
      (vm-isearch-update)
      (should (eq (car vm-message-pointer) (nth 2 vm-message-list)))
      (should vm-need-summary-pointer-update))))

(ert-deftest vm-search-test-isearch-update-leaves-the-pointer-alone ()
  "A search that stayed inside the current message changes nothing."
  (vm-test-with-folder vm-search-test--folder
    (setq major-mode 'vm-mode)
    (setq vm-message-pointer (nthcdr 1 vm-message-list))
    (let ((was vm-message-pointer)
          (updated nil))
      (cl-letf (((symbol-function 'vm-update-summary-and-mode-line)
                 (lambda (&rest _) (setq updated t))))
        (goto-char (vm-text-of (nth 1 vm-message-list)))
        (vm-isearch-update)
        (should (eq was vm-message-pointer))
        (should-not updated)))))

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