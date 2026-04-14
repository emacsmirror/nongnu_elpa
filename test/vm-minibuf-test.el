;;; vm-minibuf-test.el --- Tests for vm-minibuf.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM minibuffer functions in vm-minibuf.el

;;; Code:

(require 'vm-test-init)
(require 'vm-minibuf)

;;; Minibuffer function existence tests

(ert-deftest vm-minibuf-test-functions-exist ()
  "Test that minibuffer functions exist."
  (should (fboundp 'vm-minibuffer-complete-word))
  (should (fboundp 'vm-minibuffer-complete-word-and-exit))
  (should (fboundp 'vm-minibuffer-completion-message))
  (should (fboundp 'vm-minibuffer-replace-word))
  (should (fboundp 'vm-minibuffer-show-completions))
  (should (fboundp 'vm-show-list))
  (should (fboundp 'vm-minibuffer-completion-help))
  (should (fboundp 'vm-keyboard-read-string))
  (should (fboundp 'vm-read-string))
  (should (fboundp 'vm-read-number))
  (should (fboundp 'vm-keyboard-read-file-name))
  (should (fboundp 'vm-read-file-name))
  (should (fboundp 'vm-read-folder-name)))

;;; vm-show-list tests

(ert-deftest vm-minibuf-test-show-list-simple ()
  "Test vm-show-list with simple list."
  (let ((result (with-temp-buffer
                  (display-buffer (current-buffer))
                  (vm-show-list '("apple" "banana" "cherry"))
                  (buffer-string))))
    ;; Should contain all items
    (should (string-match "apple" result))
    (should (string-match "banana" result))
    (should (string-match "cherry" result))))

(ert-deftest vm-minibuf-test-show-list-empty ()
  "Test vm-show-list with empty list."
  (let ((result (with-temp-buffer
                  (display-buffer (current-buffer))
                  (vm-show-list nil)
                  (buffer-string))))
    ;; Should handle empty list gracefully
    (should (stringp result))))

(ert-deftest vm-minibuf-test-show-list-single-item ()
  "Test vm-show-list with single item."
  (let ((result (with-temp-buffer
                  (display-buffer (current-buffer))
                  (vm-show-list '("item"))
                  (buffer-string))))
    ;; Should contain the item
    (should (string-match "item" result))))

;;; vm-minibuffer-replace-word tests
;; Note: vm-minibuffer-replace-word always replaces the last word in buffer

(ert-deftest vm-minibuf-test-replace-word-basic ()
  "Test vm-minibuffer-replace-word replaces last word in buffer."
  (with-temp-buffer
    (insert "hello world")
    (vm-minibuffer-replace-word "universe")
    (should (string-match "hello universe" (buffer-string)))))

(ert-deftest vm-minibuf-test-replace-word-at-end ()
  "Test vm-minibuffer-replace-word replaces last word."
  (with-temp-buffer
    (insert "hello")
    (vm-minibuffer-replace-word "goodbye")
    (should (string-match "goodbye" (buffer-string)))))

;;; vm-read-number tests
;; Note: vm-read-number calls read-from-minibuffer which we can't easily
;; test interactively, so we test the parsing logic

(ert-deftest vm-minibuf-test-read-number-function-arity ()
  "Test vm-read-number function signature."
  (let ((arity (func-arity 'vm-read-number)))
    ;; Should accept at least 1 argument (prompt)
    (should (>= (cdr arity) 1))))

;;; vm-keyboard-read-string tests

(ert-deftest vm-minibuf-test-keyboard-read-string-exists ()
  "Test vm-keyboard-read-string is callable."
  (should (fboundp 'vm-keyboard-read-string)))

;;; vm-read-file-name tests

(ert-deftest vm-minibuf-test-read-file-name-function-arity ()
  "Test vm-read-file-name accepts proper arguments."
  (let ((arity (func-arity 'vm-read-file-name)))
    ;; Should accept arguments like read-file-name
    (should (>= (cdr arity) 1))))

;;; vm-read-folder-name tests

(ert-deftest vm-minibuf-test-read-folder-name-function-arity ()
  "Test vm-read-folder-name function signature."
  (let ((arity (func-arity 'vm-read-folder-name)))
    ;; Should be callable
    (should (>= (cdr arity) 0))))

;;; Completion variables

(ert-deftest vm-minibuf-test-completion-variables-exist ()
  "Test that completion-related variables exist."
  (should (boundp 'vm-completion-auto-correct))
  (should (boundp 'vm-completion-auto-space)))

(provide 'vm-minibuf-test)

;;; vm-minibuf-test.el ends here
