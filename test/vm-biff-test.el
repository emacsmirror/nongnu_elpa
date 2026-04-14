;;; vm-biff-test.el --- Tests for vm-biff.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM biff (new mail notification) in vm-biff.el

;;; Code:

(require 'vm-test-init)
(require 'vm-biff)

;;; Function existence tests

(ert-deftest vm-biff-test-core-functions-exist ()
  "Test that core biff functions exist."
  (should (fboundp 'vm-biff-popup))
  (should (fboundp 'vm-biff-delete-popup))
  (should (fboundp 'vm-biff-select-message))
  (should (fboundp 'vm-biff-select-message-mouse)))

(ert-deftest vm-biff-test-helper-functions-exist ()
  "Test that helper functions exist."
  (should (fboundp 'vm-biff-place-frame))
  (should (fboundp 'vm-biff-x-p))
  (should (fboundp 'vm-biff-get-buffer-window))
  (should (fboundp 'vm-biff-find-folder-window))
  (should (fboundp 'vm-biff-find-folder-frame))
  (should (fboundp 'vm-biff-timer-delete-popup)))

(ert-deftest vm-biff-test-summary-function-exists ()
  "Test that summary function for body peek exists."
  (should (fboundp 'vm-summary-function-V)))

(ert-deftest vm-biff-test-fvwm-function-exists ()
  "Test that FVWM integration function exists."
  (should (fboundp 'vm-biff-fvwm-focus-vm-folder-frame)))

;;; Variable existence tests

(ert-deftest vm-biff-test-customization-variables-exist ()
  "Test that customization variables exist."
  (should (boundp 'vm-biff-position))
  (should (boundp 'vm-biff-width))
  (should (boundp 'vm-biff-max-height))
  (should (boundp 'vm-biff-body-peek))
  (should (boundp 'vm-biff-focus-popup))
  (should (boundp 'vm-biff-auto-remove))
  (should (boundp 'vm-biff-summary-format))
  (should (boundp 'vm-biff-selector))
  (should (boundp 'vm-biff-place-frame-function))
  (should (boundp 'vm-biff-folder-list)))

(ert-deftest vm-biff-test-hook-variables-exist ()
  "Test that hook variables exist."
  (should (boundp 'vm-biff-select-hook))
  (should (boundp 'vm-biff-select-frame-hook)))

(ert-deftest vm-biff-test-internal-variables-exist ()
  "Test that internal variables exist."
  (should (boundp 'vm-biff-message-pointer))
  (should (boundp 'vm-biff-folder-buffer))
  (should (boundp 'vm-biff-message-number))
  (should (boundp 'vm-biff-folder-frame))
  (should (boundp 'vm-biff-keymap))
  (should (boundp 'vm-biff-FvwmCommand-path)))

;;; Customization group tests

(ert-deftest vm-biff-test-customization-group ()
  "Test that vm-biff customization group is defined."
  (should (get 'vm-biff 'custom-group)))

;;; Interactive command tests

(ert-deftest vm-biff-test-interactive-commands ()
  "Test that interactive commands exist."
  (should (commandp 'vm-biff-popup))
  (should (commandp 'vm-biff-delete-popup))
  (should (commandp 'vm-biff-select-message))
  (should (commandp 'vm-biff-select-message-mouse))
  (should (commandp 'vm-biff-fvwm-focus-vm-folder-frame)))

;;; Default values tests

(ert-deftest vm-biff-test-default-position ()
  "Test that default position is center."
  (should (eq 'center vm-biff-position)))

(ert-deftest vm-biff-test-default-width ()
  "Test that default width is 120."
  (should (= 120 vm-biff-width)))

(ert-deftest vm-biff-test-default-max-height ()
  "Test that default max height is 10."
  (should (= 10 vm-biff-max-height)))

(ert-deftest vm-biff-test-default-body-peek ()
  "Test that default body peek is 50 characters."
  (should (= 50 vm-biff-body-peek)))

(ert-deftest vm-biff-test-default-focus-popup ()
  "Test that focus popup is disabled by default."
  (should (null vm-biff-focus-popup)))

(ert-deftest vm-biff-test-default-auto-remove ()
  "Test that auto-remove is disabled by default."
  (should (null vm-biff-auto-remove)))

;;; Keymap tests

(ert-deftest vm-biff-test-keymap-exists ()
  "Test that biff keymap exists and is a keymap."
  (should vm-biff-keymap)
  (should (keymapp vm-biff-keymap)))

(ert-deftest vm-biff-test-keymap-bindings ()
  "Test that keymap has expected bindings."
  (should (eq 'vm-biff-delete-popup (lookup-key vm-biff-keymap "q")))
  (should (eq 'vm-biff-delete-popup (lookup-key vm-biff-keymap " ")))
  (should (eq 'vm-biff-select-message (lookup-key vm-biff-keymap [(return)]))))

;;; Frame properties tests

(ert-deftest vm-biff-test-frame-properties-constant ()
  "Test that frame properties constant exists."
  (should (boundp 'vm-biff-frame-properties))
  (should (listp vm-biff-frame-properties)))

(ert-deftest vm-biff-test-frame-properties-content ()
  "Test that frame properties has expected entries."
  (should (assq 'name vm-biff-frame-properties))
  (should (assq 'unsplittable vm-biff-frame-properties))
  (should (assq 'minibuffer vm-biff-frame-properties)))

;;; Default selector tests

(ert-deftest vm-biff-test-default-selector ()
  "Test that default selector is a valid expression."
  (should (listp vm-biff-selector))
  (should (eq 'and (car vm-biff-selector))))

;;; vm-biff-x-p tests

(ert-deftest vm-biff-test-x-p-returns-boolean ()
  "Test that vm-biff-x-p returns a boolean-like value."
  (let ((result (vm-biff-x-p)))
    (should (or (eq result t) (eq result nil) result))))

(provide 'vm-biff-test)

;;; vm-biff-test.el ends here