;;; vm-test-init.el --- Test infrastructure for VM -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Test infrastructure for VM including:
;; - Load path setup
;; - Fixture loading helpers
;; - Temporary directory/buffer macros
;; - Common test utilities

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Path setup

(defvar vm-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing test files.")

(defvar vm-test-lisp-dir
  (expand-file-name "../lisp" vm-test-dir)
  "Directory containing VM lisp source files.")

(defvar vm-test-fixtures-dir
  (expand-file-name "fixtures" vm-test-dir)
  "Directory containing test fixtures.")

;; Add VM lisp directory to load path
(add-to-list 'load-path vm-test-lisp-dir)

;;; Load VM modules

(require 'vm-macro)
(require 'vm-vars)

;;; Fixture helpers

(defun vm-test-fixture-path (category filename)
  "Return full path to fixture file FILENAME in CATEGORY subdirectory."
  (expand-file-name filename (expand-file-name category vm-test-fixtures-dir)))

(defun vm-test-read-fixture (category filename)
  "Read fixture file FILENAME from CATEGORY as a string."
  (with-temp-buffer
    (insert-file-contents (vm-test-fixture-path category filename))
    (buffer-string)))

(defun vm-test-fixture-exists-p (category filename)
  "Return non-nil if fixture file FILENAME exists in CATEGORY."
  (file-exists-p (vm-test-fixture-path category filename)))

;;; Temporary directory macro

(defmacro vm-test-with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a temporary directory.
The directory is created before BODY and deleted after, even on error."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "vm-test-" t))
          (default-directory (file-name-as-directory temp-dir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

;;; Temporary buffer macro

(defmacro vm-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.
Point is positioned at the beginning of the buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Mock helpers for IMAP testing

(defvar vm-test-mock-responses nil
  "Queue of mock responses to return from mocked network functions.")

(defvar vm-test-mock-commands nil
  "List of commands sent to mocked network functions.")

(defun vm-test-mock-process-send-string (_process string)
  "Mock `process-send-string' that records STRING to `vm-test-mock-commands'."
  (push string vm-test-mock-commands))

(defun vm-test-mock-accept-process-output (process &optional _timeout _millisec _just-this-one)
  "Mock `accept-process-output' that injects responses from `vm-test-mock-responses'."
  (when (and process vm-test-mock-responses)
    (let ((response (pop vm-test-mock-responses)))
      (when (buffer-live-p (process-buffer process))
        (with-current-buffer (process-buffer process)
          (goto-char (point-max))
          (insert response)))))
  t)

(defmacro vm-test-with-mock-network (responses &rest body)
  "Execute BODY with mocked network layer returning RESPONSES.
RESPONSES is a list of strings to return from accept-process-output.
Commands sent are recorded in `vm-test-mock-commands'."
  (declare (indent 1) (debug t))
  `(let ((vm-test-mock-responses (copy-sequence ,responses))
         (vm-test-mock-commands nil))
     (cl-letf (((symbol-function 'process-send-string)
                #'vm-test-mock-process-send-string)
               ((symbol-function 'accept-process-output)
                #'vm-test-mock-accept-process-output))
       ,@body)))

;;; Test skip helpers

(defmacro vm-test-skip-unless (condition &optional message)
  "Skip the current test unless CONDITION is true.
Optional MESSAGE explains why the test was skipped."
  `(unless ,condition
     (ert-skip (or ,message "Precondition not met"))))

;;; Folder setup helpers

;; Load additional modules needed for folder operations
(require 'vm-folder)
(require 'vm-message)
(require 'vm-mime)

(defvar vm-test-folder-default-variables
  '((vm-folder-type . nil)
    (vm-folder-access-method . nil)
    (vm-message-list . nil)
    (vm-message-pointer . nil)
    (vm-summary-buffer . nil)
    (vm-presentation-buffer . nil)
    (vm-system-state . nil)
    (vm-undo-record-list . nil)
    (vm-undo-record-pointer . nil)
    (vm-message-order-changed . nil)
    (vm-message-order-header-present . nil)
    (vm-numbering-redo-start-point . nil)
    (vm-numbering-redo-end-point . nil)
    (vm-summary-redo-start-point . nil)
    (vm-folder-read-only . nil)
    (vm-modification-counter . 0)
    (vm-messages-not-on-disk . 0)
    (vm-totals . nil)
    (vm-thread-obarray . nil)
    (vm-thread-subject-obarray . nil)
    (vm-buffers-needing-display-update . nil)
    (vm-default-From_-folder-type . From_)
    (vm-trust-From_-with-Content-Length . nil)
    (vm-display-using-mime . t)
    (vm-auto-decode-mime-messages . t)
    (vm-mime-charset-converter-alist . nil)
    (vm-mime-default-face-charsets . nil))
  "Default variable bindings for test folder buffers.")

(defun vm-test-init-folder-variables ()
  "Initialize VM folder variables in current buffer."
  (dolist (var-val vm-test-folder-default-variables)
    (set (make-local-variable (car var-val)) (cdr var-val)))
  ;; Initialize hash table for buffers needing update
  (setq vm-buffers-needing-display-update (make-vector 29 0)))

(defmacro vm-test-with-folder (content &rest body)
  "Execute BODY in a temp buffer set up as a VM folder with CONTENT.
The buffer is initialized with VM folder variables and messages are parsed.
`vm-message-list' will contain the parsed messages.

CONTENT should be a string containing raw email(s) in mbox format.

Example:
  (vm-test-with-folder
    \"From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body text
\"
    (should (= (length vm-message-list) 1)))"
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (vm-test-init-folder-variables)
     (insert ,content)
     (goto-char (point-min))
     ;; Parse messages
     (vm-build-message-list)
     ;; Set message pointer to first message
     (setq vm-message-pointer vm-message-list)
     ,@body))

(defmacro vm-test-with-folder-fixture (category filename &rest body)
  "Execute BODY with a VM folder loaded from fixture file.
CATEGORY and FILENAME specify the fixture to load."
  (declare (indent 2) (debug t))
  `(vm-test-with-folder (vm-test-read-fixture ,category ,filename)
     ,@body))

(defun vm-test-message-count ()
  "Return the number of messages in current folder."
  (length vm-message-list))

(defun vm-test-first-message ()
  "Return the first message in current folder."
  (car vm-message-list))

(defun vm-test-nth-message (n)
  "Return the Nth message (0-indexed) in current folder."
  (nth n vm-message-list))

(defun vm-test-message-body (m)
  "Return the body text of message M as a string."
  (save-excursion
    (vm-find-and-set-text-of m)
    (buffer-substring-no-properties
     (vm-text-of m)
     (vm-text-end-of m))))

(defun vm-test-message-header (m header-name)
  "Return the value of HEADER-NAME from message M."
  (save-excursion
    (goto-char (vm-headers-of m))
    (let ((case-fold-search t)
          (limit (or (vm-text-of m) (vm-text-end-of m))))
      (when (re-search-forward
             (concat "^" (regexp-quote header-name) ":[ \t]*")
             limit t)
        (let ((start (point)))
          ;; Handle multi-line headers
          (while (progn
                   (forward-line 1)
                   (and (< (point) limit)
                        (looking-at "[ \t]"))))
          (buffer-substring-no-properties start (1- (point))))))))

(provide 'vm-test-init)

;;; vm-test-init.el ends here
