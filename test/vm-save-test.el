;;; vm-save-test.el --- Tests for vm-save.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM save functions in vm-save.el

;;; Code:

(require 'vm-test-init)
(require 'vm-save)

;;; Save function existence tests

(ert-deftest vm-save-test-functions-exist ()
  "Test that save functions exist."
  (should (fboundp 'vm-save-message))
  (should (fboundp 'vm-save-message-sans-headers))
  (should (fboundp 'vm-save-message-to-local-folder))
  (should (fboundp 'vm-auto-archive-messages))
  (should (fboundp 'vm-auto-select-folder)))

;;; Pipe function existence tests

(ert-deftest vm-save-test-pipe-functions-exist ()
  "Test that pipe functions exist."
  (should (fboundp 'vm-pipe-message-to-command))
  (should (fboundp 'vm-pipe-message-to-command-to-string))
  (should (fboundp 'vm-pipe-message-to-command-discard-output))
  (should (fboundp 'vm-pipe-messages-to-command))
  (should (fboundp 'vm-pipe-messages-to-command-to-string))
  (should (fboundp 'vm-pipe-messages-to-command-discard-output))
  (should (fboundp 'vm-pipe-message-part)))

;;; IMAP folder check

(ert-deftest vm-save-test-imap-folder-p-exists ()
  "Test that vm-imap-folder-p exists."
  (should (fboundp 'vm-imap-folder-p)))

;;; Print function

(ert-deftest vm-save-test-print-message-exists ()
  "Test that vm-print-message exists."
  (should (fboundp 'vm-print-message)))

;;; vm-auto-select-folder tests

(ert-deftest vm-save-test-auto-select-folder-matches-from ()
  "Test vm-auto-select-folder matches From header."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: newsletter@lists.example.org
Subject: Weekly digest
Message-ID: <test@example.com>

Newsletter content
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters")
                     ("admin@" . "admin"))))
          (vm-save-using-auto-folders t))
      (should (equal "newsletters"
                     (vm-auto-select-folder vm-message-pointer))))))

(ert-deftest vm-save-test-auto-select-folder-matches-subject ()
  "Test vm-auto-select-folder matches Subject header."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: anyone@example.com
Subject: [BUG] Something is broken
Message-ID: <test@example.com>

Bug report
"
    (let ((vm-auto-folder-alist
           '(("Subject" ("\\[BUG\\]" . "bugs")
                        ("\\[FEATURE\\]" . "features"))))
          (vm-save-using-auto-folders t))
      (should (equal "bugs"
                     (vm-auto-select-folder vm-message-pointer))))))

(ert-deftest vm-save-test-auto-select-folder-no-match ()
  "Test vm-auto-select-folder returns nil when no match."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: random@example.com
Subject: Hello
Message-ID: <test@example.com>

Body
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters"))))
          (vm-save-using-auto-folders t))
      (should-not (vm-auto-select-folder vm-message-pointer)))))

(ert-deftest vm-save-test-auto-select-folder-disabled ()
  "Test vm-auto-select-folder returns nil when disabled."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: newsletter@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters"))))
          (vm-save-using-auto-folders nil))  ; disabled
      (should-not (vm-auto-select-folder vm-message-pointer)))))

(ert-deftest vm-save-test-auto-select-folder-case-fold ()
  "Test vm-auto-select-folder respects case-fold setting."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: NEWSLETTER@EXAMPLE.COM
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters"))))
          (vm-save-using-auto-folders t)
          (vm-auto-folder-case-fold-search t))
      (should (equal "newsletters"
                     (vm-auto-select-folder vm-message-pointer))))))

;;; vm-pipe-message-part tests

(ert-deftest vm-save-test-pipe-message-part-whole-message ()
  "Test vm-pipe-message-part returns whole message with no prefix arg."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body text here
"
    (let* ((msg (vm-test-first-message))
           (prefix-arg nil)
           (region (vm-pipe-message-part msg prefix-arg)))
      ;; Should return headers-of to text-end-of
      (should (= (vm-headers-of msg) (nth 0 region)))
      (should (= (vm-text-end-of msg) (nth 1 region))))))

(ert-deftest vm-save-test-pipe-message-part-body-only ()
  "Test vm-pipe-message-part returns body with single prefix arg."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body text here
"
    (let* ((msg (vm-test-first-message))
           (prefix-arg '(4))  ; C-u
           (region (vm-pipe-message-part msg prefix-arg)))
      ;; Should return text-of to text-end-of (body only)
      (should (= (vm-text-of msg) (nth 0 region)))
      (should (= (vm-text-end-of msg) (nth 1 region))))))

(ert-deftest vm-save-test-pipe-message-part-headers-only ()
  "Test vm-pipe-message-part returns headers with double prefix arg."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body text here
"
    (let* ((msg (vm-test-first-message))
           (prefix-arg '(16))  ; C-u C-u
           (region (vm-pipe-message-part msg prefix-arg)))
      ;; Should return headers-of to text-of (headers only)
      (should (= (vm-headers-of msg) (nth 0 region)))
      (should (= (vm-text-of msg) (nth 1 region))))))

;;; vm-switch-to-command-output-buffer tests

(ert-deftest vm-save-test-switch-output-buffer-empty ()
  "Test vm-switch-to-command-output-buffer with empty output."
  (let ((buffer (get-buffer-create " *test-output*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (erase-buffer))
          ;; Should not error with empty buffer
          (vm-switch-to-command-output-buffer "test-cmd" buffer nil))
      (kill-buffer buffer))))

(ert-deftest vm-save-test-switch-output-buffer-with-content ()
  "Test vm-switch-to-command-output-buffer with output content."
  (let ((buffer (get-buffer-create " *test-output*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Some output"))
          ;; Should not error with content
          (vm-switch-to-command-output-buffer "test-cmd" buffer nil))
      (kill-buffer buffer))))

(ert-deftest vm-save-test-switch-output-buffer-discard ()
  "Test vm-switch-to-command-output-buffer with discard flag."
  (let ((buffer (get-buffer-create " *test-output*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Some output"))
          ;; With discard-output=t, should not display buffer
          (vm-switch-to-command-output-buffer "test-cmd" buffer t))
      (kill-buffer buffer))))

;;; Filed flag tests

(ert-deftest vm-save-test-filed-flag ()
  "Test that vm-set-filed-flag sets the filed attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not filed
      (should-not (vm-filed-flag msg))
      ;; Set filed flag
      (vm-set-filed-flag msg t)
      (should (vm-filed-flag msg))
      ;; Unset filed flag
      (vm-set-filed-flag msg nil)
      (should-not (vm-filed-flag msg)))))

;;; Written flag tests

(ert-deftest vm-save-test-written-flag ()
  "Test that vm-set-written-flag sets the written attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not written
      (should-not (vm-written-flag msg))
      ;; Set written flag
      (vm-set-written-flag msg t)
      (should (vm-written-flag msg)))))

(provide 'vm-save-test)

;;; vm-save-test.el ends here
