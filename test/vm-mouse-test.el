;;; vm-mouse-test.el --- Tests for vm-mouse.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM mouse functions in vm-mouse.el

;;; Code:

(require 'vm-test-init)
(require 'vm-mouse)

;;; vm-mouse-set-mouse-track-highlight tests

(ert-deftest vm-mouse-test-set-highlight-creates-overlay ()
  "Test that vm-mouse-set-mouse-track-highlight creates an overlay."
  (with-temp-buffer
    (insert "test text here")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 1 5)))
      (should overlay)
      (should (overlayp overlay))
      (delete-overlay overlay))))

(ert-deftest vm-mouse-test-set-highlight-mouse-face ()
  "Test that overlay has mouse-face property set to highlight."
  (with-temp-buffer
    (insert "test text here")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 1 10)))
      (should (eq 'highlight (overlay-get overlay 'mouse-face)))
      (delete-overlay overlay))))

(ert-deftest vm-mouse-test-set-highlight-region ()
  "Test that overlay covers the specified region."
  (with-temp-buffer
    (insert "test text here")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 6 10)))
      (should (= 6 (overlay-start overlay)))
      (should (= 10 (overlay-end overlay)))
      (delete-overlay overlay))))

(ert-deftest vm-mouse-test-set-highlight-move-existing ()
  "Test that existing overlay is moved when passed as argument."
  (with-temp-buffer
    (insert "test text here for testing")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 1 5)))
      ;; Move the overlay to a new region
      (vm-mouse-set-mouse-track-highlight 10 20 overlay)
      (should (= 10 (overlay-start overlay)))
      (should (= 20 (overlay-end overlay)))
      (delete-overlay overlay))))

(ert-deftest vm-mouse-test-set-highlight-returns-same-overlay ()
  "Test that moving existing overlay returns the same overlay."
  (with-temp-buffer
    (insert "test text here for testing")
    (let* ((overlay1 (vm-mouse-set-mouse-track-highlight 1 5))
           (overlay2 (vm-mouse-set-mouse-track-highlight 10 20 overlay1)))
      (should (eq overlay1 overlay2))
      (delete-overlay overlay1))))

(ert-deftest vm-mouse-test-set-highlight-zero-width ()
  "Test creating zero-width overlay."
  (with-temp-buffer
    (insert "test")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 2 2)))
      (should overlay)
      (should (= (overlay-start overlay) (overlay-end overlay)))
      (delete-overlay overlay))))

(ert-deftest vm-mouse-test-set-highlight-whole-buffer ()
  "Test creating overlay spanning whole buffer."
  (with-temp-buffer
    (insert "test text")
    (let ((overlay (vm-mouse-set-mouse-track-highlight (point-min) (point-max))))
      (should (= (point-min) (overlay-start overlay)))
      (should (= (point-max) (overlay-end overlay)))
      (delete-overlay overlay))))

;;; vm-mouse-support-possible-p tests

(ert-deftest vm-mouse-test-support-possible-returns-boolean ()
  "Test that vm-mouse-support-possible-p returns a boolean."
  (let ((result (vm-mouse-support-possible-p)))
    (should (or (eq result t) (eq result nil)))))

;;; vm-mouse-3-help tests

(ert-deftest vm-mouse-test-3-help-returns-string ()
  "Test that vm-mouse-3-help returns a help string."
  (let ((result (vm-mouse-3-help nil)))
    (should (or (null result) (stringp result)))))

;;; Interactive command tests

(ert-deftest vm-mouse-test-button-2-interactive ()
  "Test that vm-mouse-button-2 is an interactive command."
  (should (commandp 'vm-mouse-button-2)))

(ert-deftest vm-mouse-test-button-3-interactive ()
  "Test that vm-mouse-button-3 is an interactive command."
  (should (commandp 'vm-mouse-button-3)))

(ert-deftest vm-mouse-test-popup-or-select-interactive ()
  "Test that vm-mouse-popup-or-select is an interactive command."
  (should (commandp 'vm-mouse-popup-or-select)))

(ert-deftest vm-mouse-test-send-url-at-event-interactive ()
  "Test that vm-mouse-send-url-at-event is an interactive command."
  (should (commandp 'vm-mouse-send-url-at-event)))

;;; Variable existence tests

(ert-deftest vm-mouse-test-track-summary-variable ()
  "Test that vm-mouse-track-summary exists and is boolean."
  (should (boundp 'vm-mouse-track-summary))
  (should (or (eq vm-mouse-track-summary t) (eq vm-mouse-track-summary nil))))

(ert-deftest vm-mouse-test-url-browser-variable ()
  "Test that vm-url-browser variable exists."
  (should (boundp 'vm-url-browser)))

;;; vm-mouse-send-url tests

(ert-deftest vm-mouse-test-send-url-function-exists ()
  "Test that vm-mouse-send-url function exists."
  (should (fboundp 'vm-mouse-send-url)))

;;; Overlay text retrieval tests

(ert-deftest vm-mouse-test-get-mouse-track-string-function-exists ()
  "Test that vm-mouse-get-mouse-track-string function exists."
  (should (fboundp 'vm-mouse-get-mouse-track-string)))

;;; Multiple overlays tests

(ert-deftest vm-mouse-test-multiple-overlays ()
  "Test creating multiple non-overlapping overlays."
  (with-temp-buffer
    (insert "first second third")
    (let ((o1 (vm-mouse-set-mouse-track-highlight 1 5))
          (o2 (vm-mouse-set-mouse-track-highlight 7 12))
          (o3 (vm-mouse-set-mouse-track-highlight 14 18)))
      (should (not (eq o1 o2)))
      (should (not (eq o2 o3)))
      (should (not (eq o1 o3)))
      ;; All should have mouse-face
      (should (eq 'highlight (overlay-get o1 'mouse-face)))
      (should (eq 'highlight (overlay-get o2 'mouse-face)))
      (should (eq 'highlight (overlay-get o3 'mouse-face)))
      (delete-overlay o1)
      (delete-overlay o2)
      (delete-overlay o3))))

;;; Overlay persistence tests

(ert-deftest vm-mouse-test-overlay-survives-insert ()
  "Test that overlay adjusts when text is inserted before it."
  (with-temp-buffer
    (insert "test text")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 6 9)))
      ;; Insert at beginning
      (goto-char (point-min))
      (insert "xxx ")
      ;; Overlay should have moved
      (should (= 10 (overlay-start overlay)))
      (should (= 13 (overlay-end overlay)))
      (delete-overlay overlay))))

(ert-deftest vm-mouse-test-overlay-survives-delete ()
  "Test that overlay persists when text is deleted elsewhere."
  (with-temp-buffer
    (insert "prefix test text")
    (let ((overlay (vm-mouse-set-mouse-track-highlight 8 12)))
      ;; Delete from beginning
      (goto-char (point-min))
      (delete-char 7)
      ;; Overlay should have moved
      (should (= 1 (overlay-start overlay)))
      (should (= 5 (overlay-end overlay)))
      (delete-overlay overlay))))

(provide 'vm-mouse-test)

;;; vm-mouse-test.el ends here