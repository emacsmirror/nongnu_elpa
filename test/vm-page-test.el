;;; vm-page-test.el --- Tests for vm-page.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM page functions in vm-page.el

;;; Code:

(require 'vm-test-init)
(require 'vm-page)

;;; Page function existence tests

(ert-deftest vm-page-test-functions-exist ()
  "Test that page navigation functions exist."
  (should (fboundp 'vm-scroll-forward))
  (should (fboundp 'vm-scroll-backward))
  (should (fboundp 'vm-scroll-forward-one-line))
  (should (fboundp 'vm-scroll-backward-one-line))
  (should (fboundp 'vm-beginning-of-message))
  (should (fboundp 'vm-end-of-message))
  (should (fboundp 'vm-widen-page))
  (should (fboundp 'vm-narrow-to-page)))

;;; Button navigation functions

(ert-deftest vm-page-test-button-functions-exist ()
  "Test that button navigation functions exist."
  (should (fboundp 'vm-next-button))
  (should (fboundp 'vm-previous-button))
  (should (fboundp 'vm-move-to-next-button))
  (should (fboundp 'vm-move-to-previous-button)))

;;; Header highlighting functions

(ert-deftest vm-page-test-highlight-functions-exist ()
  "Test that header highlighting functions exist."
  (should (fboundp 'vm-highlight-headers))
  (should (fboundp 'vm-highlight-headers-maybe))
  (should (fboundp 'vm-energize-urls))
  (should (fboundp 'vm-energize-headers))
  (should (fboundp 'vm-energize-urls-in-message-region)))

;;; Presentation functions

(ert-deftest vm-page-test-presentation-functions-exist ()
  "Test that presentation functions exist."
  (should (fboundp 'vm-present-current-message))
  (should (fboundp 'vm-preview-current-message))
  (should (fboundp 'vm-show-current-message))
  (should (fboundp 'vm-expose-hidden-headers)))

;;; X-Face functions

(ert-deftest vm-page-test-xface-functions-exist ()
  "Test that X-Face display functions exist."
  (should (fboundp 'vm-display-xface)))

;;; vm-emit-eom-blurb tests

(ert-deftest vm-page-test-emit-eom-blurb-exists ()
  "Test vm-emit-eom-blurb exists."
  (should (fboundp 'vm-emit-eom-blurb)))

;;; vm-howl-if-eom tests

(ert-deftest vm-page-test-howl-if-eom-exists ()
  "Test vm-howl-if-eom exists."
  (should (fboundp 'vm-howl-if-eom)))

;;; URL handling variables

(ert-deftest vm-page-test-url-variables-exist ()
  "Test that URL handling variables exist."
  (should (boundp 'vm-highlight-url-face))
  (should (boundp 'vm-url-browser)))

;;; Header visibility variables

(ert-deftest vm-page-test-header-variables-exist ()
  "Test that header display variables exist."
  (should (boundp 'vm-visible-headers))
  (should (boundp 'vm-invisible-header-regexp)))

;;; vm-narrow-for-preview tests

(ert-deftest vm-page-test-narrow-for-preview-exists ()
  "Test vm-narrow-for-preview exists."
  (should (fboundp 'vm-narrow-for-preview)))

;;; Scrolling variables

(ert-deftest vm-page-test-scroll-variables-exist ()
  "Test that scroll-related variables exist."
  (should (boundp 'vm-auto-next-message))
  (should (boundp 'vm-honor-page-delimiters)))

;;; vm-url-help tests

(ert-deftest vm-page-test-url-help-exists ()
  "Test vm-url-help function exists."
  (should (fboundp 'vm-url-help)))

(provide 'vm-page-test)

;;; vm-page-test.el ends here
