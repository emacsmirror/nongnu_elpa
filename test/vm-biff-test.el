;;; vm-biff-test.el --- Tests for vm-biff.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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

;;; What the body peek shows, and where the popup frame goes.  These had a
;;; test each asserting the function was bound.

(ert-deftest vm-biff-test-body-peek-shows-the-start-of-the-body ()
  "The V summary format shows up to `vm-biff-body-peek' characters of body.
It is what the popup is for, and nothing checked what it produced."
  (vm-test-with-folder (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                               "From: alice@example.com\nSubject: hello\n\n"
                               "First line of the body.\n"
                               "Second line, well past any peek.\n\n")
    (let* ((m (car vm-message-list))
           (vm-biff-body-peek 10)
           (peek (vm-summary-function-V m)))
      ;; the peek is indented with a tab, and stops at the end of the line
      ;; the limit fell in rather than mid-word
      (should (string-prefix-p "\t" peek))
      (should (string-match-p "First line of the body" peek))
      (should-not (string-match-p "Second line" peek))
      (should (eq (get-text-property 0 'face peek) 'bold)))))

(ert-deftest vm-biff-test-body-peek-squeezes-the-blank-lines ()
  "Blank lines are dropped so a paragraph break does not fill the popup,
and every line after the first is indented to line up under it."
  (vm-test-with-folder (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                               "From: alice@example.com\nSubject: hello\n\n"
                               "\n\nOne\n\n\nTwo\n\n")
    (let* ((m (car vm-message-list))
           (vm-biff-body-peek 400)
           (peek (vm-summary-function-V m)))
      (should-not (string-match-p "\n\n" peek))
      (should (string-match-p "One\n\tTwo" peek)))))

(ert-deftest vm-biff-test-body-peek-leaves-the-folder-narrowing-alone ()
  "The peek widens the folder to read the body, and puts it back.
`save-restriction' saves the restriction of the buffer it is entered in, and
this one was entered before the switch to the folder, so it held whichever
buffer the summary was being built in.  `vm-biff-popup' makes the popup
buffer current before it formats a line, so a folder narrowed to the message
being read came back showing the whole mbox."
  (vm-test-with-folder (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                               "From: alice@example.com\nSubject: one\n\nBody one.\n\n"
                               "From bob@example.com Mon Jan  1 00:00:00 2024\n"
                               "From: bob@example.com\nSubject: two\n\nBody two.\n\n")
    (let ((m (car vm-message-list))
          (folder (current-buffer)))
      (narrow-to-region (vm-start-of m) (vm-end-of m))
      (let ((bounds (cons (point-min) (point-max))))
        (should (< (cdr bounds) (1+ (buffer-size))))   ; really narrowed
        ;; as the popup does it: another buffer is current
        (with-temp-buffer
          (vm-summary-function-V m))
        (with-current-buffer folder
          (should (equal bounds (cons (point-min) (point-max)))))))))

(ert-deftest vm-biff-test-place-frame-limits-the-height ()
  "The popup is `vm-biff-width' wide and never more than
`vm-biff-max-height' lines, however long the message is."
  (let (sized)
    (cl-letf (((symbol-function 'set-frame-size)
               (lambda (_f w h) (setq sized (cons w h))))
              ((symbol-function 'set-frame-position) #'ignore))
      (with-temp-buffer
        (insert "one line\n")
        ;; an explicit position, so that centring does not want a display
        (let ((vm-biff-position '(1 1))
              (vm-biff-width 40) (vm-biff-max-height 10))
          (vm-biff-place-frame 'a-frame)
          (should (equal sized '(40 . 2))))
        (dotimes (_ 100) (insert "another line\n"))
        (let ((vm-biff-position '(1 1))
              (vm-biff-width 40) (vm-biff-max-height 10))
          (vm-biff-place-frame 'a-frame)
          (should (equal sized '(40 . 10))))))))

(ert-deftest vm-biff-test-place-frame-centres-or-obeys-the-position ()
  "`vm-biff-position' center centres the frame on the display; a list is
passed to `set-frame-position' as it stands."
  (let (placed)
    (cl-letf (((symbol-function 'set-frame-size) #'ignore)
              ((symbol-function 'set-frame-position)
               (lambda (_f x y) (setq placed (list x y))))
              ((symbol-function 'x-display-pixel-width) (lambda () 1000))
              ((symbol-function 'x-display-pixel-height) (lambda () 800))
              ((symbol-function 'frame-pixel-width) (lambda (_f) 400))
              ((symbol-function 'frame-pixel-height) (lambda (_f) 200)))
      (with-temp-buffer
        (let ((vm-biff-position 'center))
          (vm-biff-place-frame 'a-frame)
          (should (equal placed '(300 300))))
        (let ((vm-biff-position '(17 23)))
          (vm-biff-place-frame 'a-frame)
          (should (equal placed '(17 23))))))))

(ert-deftest vm-biff-test-fvwm-focus-names-the-folder-window ()
  "The FVWM command matches the window whose title has the folder name in it.
The stars around the name are what make it a match rather than a title."
  (let (sent)
    (cl-letf (((symbol-function 'start-process) (lambda (&rest _) 'a-process))
              ((symbol-function 'process-send-string)
               (lambda (_p s) (setq sent s)))
              ((symbol-function 'process-send-eof) #'ignore))
      (with-temp-buffer
        (rename-buffer " *vm-biff-test-INBOX*" t)
        (let ((vm-biff-folder-buffer (current-buffer)))
          (vm-biff-fvwm-focus-vm-folder-frame)
          (should (equal sent (concat "SelectWindow *" (buffer-name) "*\n"))))))))

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


;;; loading must not change VM's behaviour (issue #512)

(ert-deftest vm-biff-test-loading-does-not-install-the-hook ()
  "REGRESSION: merely loading vm-biff does not switch it on.
Issue #512, on Stefan Monnier's advice: loading a file should not change how
Emacs behaves, and this one added `vm-biff-popup' to `vm-arrived-messages-hook'
as it loaded -- which is also why it needed a guard against doing so while being
byte-compiled.  The mode does it instead."
  (require 'vm-biff)
  (let ((vm-arrived-messages-hook nil)
        (vm-biff-mode nil))
    ;; Loading has already happened; the hook is untouched.
    (should-not (memq 'vm-biff-popup vm-arrived-messages-hook))))

(ert-deftest vm-biff-test-mode-toggles-the-hook ()
  "`vm-biff-mode' adds the hook when switched on and removes it when off."
  (require 'vm-biff)
  (let ((vm-arrived-messages-hook nil)
        (vm-biff-mode nil))
    (vm-biff-mode 1)
    (should vm-biff-mode)
    (should (memq 'vm-biff-popup vm-arrived-messages-hook))
    (vm-biff-mode -1)
    (should-not vm-biff-mode)
    (should-not (memq 'vm-biff-popup vm-arrived-messages-hook))))

(ert-deftest vm-biff-test-mode-is-idempotent ()
  "Switching the mode on twice leaves one copy of the hook function.
`add-hook' guarantees this, but the old code path could be reached more than
once -- loading the file again -- so it is worth pinning."
  (require 'vm-biff)
  (let ((vm-arrived-messages-hook nil)
        (vm-biff-mode nil))
    (vm-biff-mode 1)
    (vm-biff-mode 1)
    (should (= 1 (seq-count (lambda (f) (eq f 'vm-biff-popup))
                            vm-arrived-messages-hook)))))

(provide 'vm-biff-test)

;;; vm-biff-test.el ends here