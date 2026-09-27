;;; vm-mouse-test.el --- Tests for vm-mouse.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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
;;; What these two do, in place of a test each that they were bound.

(ert-deftest vm-mouse-test-send-url-mails-a-bare-address ()
  "A URL that is only an address is sent as mail, not to the browser.
This is what makes clicking a From line compose to it."
  (let ((mailed nil))
    (cl-letf (((symbol-function 'vm-mail-to-mailto-url)
               (lambda (url) (setq mailed url))))
      (vm-mouse-send-url "someone@example.com")
      (should (equal mailed "mailto:someone@example.com"))
      (setq mailed nil)
      (vm-mouse-send-url "mailto:someone@example.com")
      (should (equal mailed "mailto:someone@example.com")))))

(ert-deftest vm-mouse-test-send-url-uses-the-browser-it-is-given ()
  "A function browser is called with the URL; a program is run with switches.
The argument wins over `vm-url-browser', which is how the button-3 menu
offers a choice of browser."
  (let ((got nil) (ran nil))
    (cl-letf (((symbol-function 'vm-run-background-command)
               (lambda (&rest args) (setq ran args)))
              ((symbol-function 'vm-inform) #'ignore))
      (let ((vm-url-browser (lambda (url) (setq got (cons 'default url)))))
        (vm-mouse-send-url "http://example.com/")
        (should (equal got '(default . "http://example.com/"))))
      (vm-mouse-send-url "http://example.com/"
                         (lambda (url) (setq got (cons 'given url))))
      (should (equal got '(given . "http://example.com/")))
      (let ((vm-url-browser-switches '("--new-window")))
        (vm-mouse-send-url "http://example.com/" "/usr/bin/firefox")
        (should (equal ran '("/usr/bin/firefox" "--new-window"
                             "http://example.com/"))))
      (vm-mouse-send-url "http://example.com/" "/usr/bin/firefox" '("-P"))
      (should (equal ran '("/usr/bin/firefox" "-P" "http://example.com/"))))))

(ert-deftest vm-mouse-test-send-url-with-no-browser-does-nothing ()
  "A nil `vm-url-browser' means URL passing is off, as its docstring says.
It used to reach `(funcall nil url)'."
  (let ((vm-url-browser nil))
    (should-not (vm-mouse-send-url "http://example.com/"))))

(ert-deftest vm-mouse-test-get-mouse-track-string-reads-the-highlighted-text ()
  "The text under the mouse is the text of the overlay that highlights it.
An overlay without a `mouse-face' is not one of VM's buttons, so it is
ignored -- font-lock's overlays would otherwise answer for it."
  (with-temp-buffer
    (insert "see http://example.com/ for more")
    (let* ((end (progn (goto-char (point-min))
                       (search-forward "http://example.com/") (point)))
           (start (match-beginning 0))
           (o (make-overlay start end))
           (window (selected-window)))
      (set-window-buffer window (current-buffer))
      (overlay-put o 'mouse-face 'highlight)
      (should (equal (vm-mouse-get-mouse-track-string
                      (list 'mouse-1 (list window (+ start 2) '(0 . 0) 0)))
                     "http://example.com/"))
      ;; and not an overlay that is not a button
      (overlay-put o 'mouse-face nil)
      (should-not (vm-mouse-get-mouse-track-string
                   (list 'mouse-1 (list window (+ start 2) '(0 . 0) 0)))))))


;;; Overlay text retrieval tests

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

;;; Running an external program.  `vm-run-command' and its two relatives live
;;; in this file, and had a test each in vm-misc-test.el asserting only that
;;; they were bound; the behaviour tests are here, with the code.

(ert-deftest vm-mouse-test-run-command-runs-it-and-keeps-the-output ()
  "The program runs with the arguments given and its output is kept in a
buffer named after it, which is where a failed MIME viewer's complaint ends
up."
  (let ((buffer (get-buffer " */bin/echo*")))
    (when buffer (kill-buffer buffer)))
  (unwind-protect
      (cl-letf (((symbol-function 'vm-inform) #'ignore))
        (should (= (vm-run-command "/bin/echo" "hello" "world") 0))
        (with-current-buffer " */bin/echo*"
          (should (equal (buffer-string) "hello world\n")))
        ;; and a program that fails says so in its exit status
        (should (/= (vm-run-command "/bin/sh" "-c" "exit 3") 0)))
    (let ((buffer (get-buffer " */bin/echo*")))
      (when buffer (kill-buffer buffer)))
    (let ((buffer (get-buffer " */bin/sh*")))
      (when buffer (kill-buffer buffer)))))

(ert-deftest vm-mouse-test-run-command-on-region-passes-the-region-through ()
  "The region is the program's input and the output buffer gets its output.
This is how VM decodes and encodes with external programs."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((output (generate-new-buffer " *vm-mouse-test-output*")))
      (unwind-protect
          (progn
            (should (eq (vm-run-command-on-region
                         (point-min) (point-max) output "/bin/cat")
                        t))
            (with-current-buffer output
              (should (equal (buffer-string) "one\ntwo\n"))))
        (kill-buffer output)))))

(ert-deftest vm-mouse-test-run-command-on-region-forgives-a-silent-failure ()
  "A non-zero exit with nothing on stderr is taken as success.
Users complained when the exit status alone was believed, so the comment in
the code says; `vm-report-subprocess-errors' is how to get the status anyway."
  (with-temp-buffer
    (insert "input\n")
    (let ((output (generate-new-buffer " *vm-mouse-test-output*")))
      (unwind-protect
          (cl-letf (((symbol-function 'vm-warn) #'ignore))
            (let ((vm-report-subprocess-errors nil))
              (should (eq (vm-run-command-on-region
                           (point-min) (point-max) output
                           "/bin/sh" "-c" "exit 3")
                          t)))
            (let ((vm-report-subprocess-errors t))
              (should (equal (vm-run-command-on-region
                              (point-min) (point-max) output
                              "/bin/sh" "-c" "exit 3")
                             '(3 . "")))))
        (kill-buffer output)))))

(ert-deftest vm-mouse-test-run-command-on-region-reports-what-went-wrong ()
  "A program that says something on stderr has that returned with its status,
whatever `vm-report-subprocess-errors' says: there is something to report."
  (with-temp-buffer
    (insert "input\n")
    (let ((output (generate-new-buffer " *vm-mouse-test-output*")))
      (unwind-protect
          (cl-letf (((symbol-function 'vm-warn) #'ignore))
            (let* ((vm-report-subprocess-errors nil)
                   (result (vm-run-command-on-region
                            (point-min) (point-max) output
                            "/bin/sh" "-c" "echo it went wrong >&2; exit 4")))
              (should (consp result))
              (should (= (car result) 4))
              (should (string-match-p "it went wrong" (cdr result)))))
        (kill-buffer output)))))

(provide 'vm-mouse-test)

;;; vm-mouse-test.el ends here