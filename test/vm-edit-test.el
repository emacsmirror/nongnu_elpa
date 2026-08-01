;;; vm-edit-test.el --- Tests for vm-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM edit functions in vm-edit.el

;;; Code:

(require 'vm-test-init)
(require 'vm-edit)

;;; Edit function existence tests

(ert-deftest vm-edit-test-functions-exist ()
  "Test that edit functions exist."
  (should (fboundp 'vm-edit-message))
  (should (fboundp 'vm-edit-message-other-frame))
  (should (fboundp 'vm-discard-cached-data))
  (should (fboundp 'vm-discard-cached-data-internal))
  (should (fboundp 'vm-edit-message-end))
  (should (fboundp 'vm-edit-message-abort)))

;;; vm-discard-cached-data-internal tests
;; Note: vm-discard-cached-data-internal requires full folder context.
;; These tests verify the underlying fillarray behavior on cached-data.

(ert-deftest vm-edit-test-fillarray-clears-cached-data ()
  "Test that fillarray clears cached data vector."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    ;; Set some cached values
    (vm-set-subject-of msg "Cached Subject")
    (vm-set-from-of msg "Cached From")
    (should (equal "Cached Subject" (vm-subject-of msg)))
    (should (equal "Cached From" (vm-from-of msg)))
    ;; Use fillarray like vm-discard-cached-data-internal does
    (fillarray (vm-cached-data-of msg) nil)
    ;; After fillarray, should be nil
    (should (null (vm-subject-of msg)))
    (should (null (vm-from-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-message-id ()
  "Test that fillarray clears cached message-id."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-message-id-of msg "<cached@example.com>")
    (should (equal "<cached@example.com>" (vm-message-id-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-message-id-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-byte-count ()
  "Test that fillarray clears cached byte count."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-byte-count-of msg "12345")
    (should (equal "12345" (vm-byte-count-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-byte-count-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-line-count ()
  "Test that fillarray clears cached line count."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-line-count-of msg "100")
    (should (equal "100" (vm-line-count-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-line-count-of msg)))))

;;; Edit state tests

(ert-deftest vm-edit-test-edited-flag ()
  "Test that the edited flag can be set and read."
  (let* ((msg (vm-make-message))
         (attrs (make-vector vm-attributes-vector-length nil)))
    (vm-set-attributes-of msg attrs)
    ;; Initially not edited
    (should-not (vm-edited-flag msg))
    ;; Set edited flag directly in attributes vector
    (aset attrs 7 t)
    (should (vm-edited-flag msg))
    ;; Clear edited flag
    (aset attrs 7 nil)
    (should-not (vm-edited-flag msg))))

;;; Edit buffer tracking

(ert-deftest vm-edit-test-edit-buffer-of ()
  "Test vm-edit-buffer-of accessor."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message))
          (edit-buf (generate-new-buffer " *test-edit*")))
      (unwind-protect
          (progn
            ;; Initially no edit buffer
            (should (null (vm-edit-buffer-of msg)))
            ;; Set edit buffer
            (vm-set-edit-buffer-of msg edit-buf)
            (should (eq edit-buf (vm-edit-buffer-of msg)))
            ;; Clear edit buffer
            (vm-set-edit-buffer-of msg nil)
            (should (null (vm-edit-buffer-of msg))))
        (kill-buffer edit-buf)))))

;;; editing an external (headers-only) message

(defvar vm-edit-test-folder
  (concat "From sender@example.com Sat Aug  1 12:00:00 2026\n"
          "From: sender@example.com\n"
          "To: me@example.com\n"
          "Subject: an external message\n"
          "\n"
          "the body as fetched from the server\n"
          "\n")
  "A one-message folder, used as an IMAP message held in external mode.")

(defun vm-edit-test-make-external (m)
  "Register M as a fetched IMAP message, as `vm-register-fetched-message' does.
Its body is present but marked for discarding once the fetched-message
limit evicts it, or when the folder discards fetched bodies wholesale."
  (vm-set-message-access-method-of m 'imap)
  (vm-set-body-to-be-retrieved-flag m nil t)
  (setq vm-fetched-messages (list m)
        vm-fetched-message-count 1)
  (vm-set-body-to-be-discarded-of m t))

(ert-deftest vm-edit-test-end-keeps-edited-external-body ()
  "Test that editing an external message stops its body being discarded.
Regression test for issue #376.  An external message keeps a
body-to-be-discarded flag; after an edit the edited text exists only in
the folder, so discarding it throws the edit away and the server's copy
comes back in its place."
  (vm-test-with-folder vm-edit-test-folder
    (let* ((m (car vm-message-list))
           (vm-enable-external-messages '(imap))
           (edit-buf (generate-new-buffer " *vm-edit-test*")))
      (unwind-protect
          (progn
            (vm-edit-test-make-external m)
            (should (vm-body-to-be-discarded-of m))
            ;; stand in for the user's edit session
            (setq vm-message-pointer vm-message-list)
            (with-current-buffer edit-buf
              (insert-buffer-substring
               (vm-buffer-of m) (vm-headers-of m) (vm-text-end-of m))
              (goto-char (point-min))
              (should (search-forward "as fetched from the server" nil t))
              (replace-match "as edited by hand")
              (setq vm-message-pointer (list m)
                    vm-mail-buffer (vm-buffer-of m))
              (set-buffer-modified-p t)
              (cl-letf (((symbol-function 'vm-present-current-message) #'ignore)
                        ((symbol-function 'vm-update-summary-and-mode-line)
                         #'ignore)
                        ((symbol-function 'vm-display)
                         (lambda (&rest _) nil)))
                (vm-edit-message-end)))
            ;; the edit landed, and the message is no longer registered
            ;; as a fetched one whose body may be thrown away
            (should (vm-edited-flag m))
            (should-not (vm-body-to-be-discarded-of m))
            (should-not (memq m vm-fetched-messages))
            ;; so discarding fetched bodies leaves the edited one alone
            (vm-discard-fetched-messages)
            (should (string-match "as edited by hand"
                                  (vm-test-message-body m)))
            (should-not (vm-body-to-be-retrieved-of m)))
        (when (buffer-live-p edit-buf) (kill-buffer edit-buf))))))

(provide 'vm-edit-test)

;;; vm-edit-test.el ends here
