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

;;; Mock process infrastructure for POP/IMAP testing

(defvar vm-test-mock-process nil
  "The current mock process object.")

(defvar vm-test-mock-buffer nil
  "Buffer associated with the mock process.")

(defvar vm-test-mock-responses nil
  "Queue of mock responses to return from mocked network functions.")

(defvar vm-test-mock-commands nil
  "List of commands sent to mocked network functions (most recent first).")

(defvar vm-test-mock-process-status 'open
  "Status to return for mock process.")

(defun vm-test-mock-process-p (obj)
  "Return t if OBJ is our mock process."
  (and (consp obj) (eq (car obj) 'vm-mock-process)))

(defun vm-test-mock-open-network-stream (name buffer host service &rest params)
  "Mock `open-network-stream' that returns a fake process."
  (ignore name host service params)
  ;; Use provided buffer or create one
  (setq vm-test-mock-buffer (or buffer (generate-new-buffer " *mock-net*")))
  (setq vm-test-mock-process (cons 'vm-mock-process vm-test-mock-buffer))
  vm-test-mock-process)

(defun vm-test-mock-process-status (process)
  "Mock `process-status' for our mock process."
  (cond ((null process) nil)
        ((vm-test-mock-process-p process) vm-test-mock-process-status)
        ;; For any other process in test context, assume closed
        (t nil)))

(defun vm-test-mock-process-buffer (process)
  "Mock `process-buffer' for our mock process."
  (cond ((null process) nil)
        ((vm-test-mock-process-p process) (cdr process))
        ;; For any other process in test context, return nil
        (t nil)))

(defun vm-test-mock-buffer-live-p (buffer)
  "Check if BUFFER is live (works with mock process buffers)."
  (buffer-live-p buffer))

(defun vm-test-mock-process-send-string (process string)
  "Mock `process-send-string' that records STRING and injects response."
  (push string vm-test-mock-commands)
  ;; After sending a command, inject the next response into the buffer
  (when vm-test-mock-responses
    (let ((response (pop vm-test-mock-responses))
          (buf (cond ((null process) vm-test-mock-buffer)
                     ((vm-test-mock-process-p process) (cdr process))
                     (t vm-test-mock-buffer))))
      (when (and response buf (buffer-live-p buf))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert response))))))

(defun vm-test-mock-accept-process-output (process &optional _seconds _millisec _just-this-one)
  "Mock `accept-process-output' - data already injected by send-string."
  (ignore process)
  ;; If there are still responses queued, inject one
  ;; This handles cases where accept-process-output is called without send-string
  (when (and vm-test-mock-responses
             vm-test-mock-buffer
             (buffer-live-p vm-test-mock-buffer))
    (let ((response (pop vm-test-mock-responses)))
      (when response
        (with-current-buffer vm-test-mock-buffer
          (goto-char (point-max))
          (insert response)))))
  t)

(defun vm-test-mock-delete-process (process)
  "Mock `delete-process' for mock processes."
  (when (vm-test-mock-process-p process)
    (setq vm-test-mock-process-status 'closed)))

(defun vm-test-mock-set-process-sentinel (process sentinel)
  "Mock `set-process-sentinel' - no-op for all processes in test context."
  (ignore process sentinel))

(defun vm-test-mock-process-sentinel (process)
  "Mock `process-sentinel' - returns nil for all processes in test context."
  (ignore process)
  nil)

(defun vm-test-mock-processp (obj)
  "Mock `processp' that returns t for mock processes."
  (vm-test-mock-process-p obj))

(defmacro vm-test-with-mock-process (responses &rest body)
  "Execute BODY with a fully mocked network process layer.
RESPONSES is a list of strings that will be injected as server responses.
Commands sent are recorded in `vm-test-mock-commands' (most recent first).

The mock process passes all standard process checks:
- `process-status' returns 'open
- `process-buffer' returns the associated buffer
- `process-send-string' records commands and injects responses
- `accept-process-output' returns t (data already injected)

Example:
  (vm-test-with-mock-process
      \\='(\"+OK POP3 ready\\r\\n\"
        \"+OK\\r\\n\"
        \"+OK 5 12345\\r\\n\")
    ;; Test code that makes POP calls
    (should (member \"STAT\\r\\n\" vm-test-mock-commands)))"
  (declare (indent 1) (debug t))
  `(let ((vm-test-mock-responses (copy-sequence ,responses))
         (vm-test-mock-commands nil)
         (vm-test-mock-process nil)
         (vm-test-mock-buffer nil)
         (vm-test-mock-process-status 'open))
     (cl-letf (((symbol-function 'open-network-stream)
                #'vm-test-mock-open-network-stream)
               ((symbol-function 'processp)
                #'vm-test-mock-processp)
               ((symbol-function 'process-status)
                #'vm-test-mock-process-status)
               ((symbol-function 'process-buffer)
                #'vm-test-mock-process-buffer)
               ((symbol-function 'process-send-string)
                #'vm-test-mock-process-send-string)
               ((symbol-function 'accept-process-output)
                #'vm-test-mock-accept-process-output)
               ((symbol-function 'delete-process)
                #'vm-test-mock-delete-process)
               ((symbol-function 'set-process-sentinel)
                #'vm-test-mock-set-process-sentinel)
               ((symbol-function 'process-sentinel)
                #'vm-test-mock-process-sentinel))
       (unwind-protect
           (progn ,@body)
         (when (and vm-test-mock-buffer (buffer-live-p vm-test-mock-buffer))
           (kill-buffer vm-test-mock-buffer))))))

;; Alias for backward compatibility
(defalias 'vm-test-with-mock-network 'vm-test-with-mock-process)

;;; POP-specific mock helpers

(defmacro vm-test-with-pop-session (responses &rest body)
  "Execute BODY with a mocked POP session buffer already set up.
RESPONSES are injected as POP server responses.
The buffer has `vm-pop-read-point' initialized.
`vm-test-mock-process' is set to a valid mock process."
  (declare (indent 1) (debug t))
  `(vm-test-with-mock-process ,responses
     (let ((pop-buffer (generate-new-buffer " *mock-pop*")))
       (unwind-protect
           (with-current-buffer pop-buffer
             ;; Create mock process connected to this buffer
             (setq vm-test-mock-buffer pop-buffer)
             (setq vm-test-mock-process (cons 'vm-mock-process pop-buffer))
             (make-local-variable 'vm-pop-read-point)
             (setq vm-pop-read-point (point-min-marker))
             ;; Inject greeting
             (when vm-test-mock-responses
               (insert (pop vm-test-mock-responses)))
             (goto-char (point-min))
             ,@body)
         (kill-buffer pop-buffer)))))

;;; IMAP-specific mock helpers

(defmacro vm-test-with-imap-session (responses &rest body)
  "Execute BODY with a mocked IMAP session buffer already set up.
RESPONSES are injected as IMAP server responses.
The buffer has IMAP-related variables initialized.
`vm-test-mock-process' is set to a valid mock process."
  (declare (indent 1) (debug t))
  `(vm-test-with-mock-process ,responses
     (let ((imap-buffer (generate-new-buffer " *mock-imap*")))
       (unwind-protect
           (with-current-buffer imap-buffer
             ;; Create mock process connected to this buffer
             (setq vm-test-mock-buffer imap-buffer)
             (setq vm-test-mock-process (cons 'vm-mock-process imap-buffer))
             (make-local-variable 'vm-imap-read-point)
             (setq vm-imap-read-point (point-min-marker))
             (make-local-variable 'vm-imap-session-done)
             (setq vm-imap-session-done nil)
             ;; Inject greeting
             (when vm-test-mock-responses
               (insert (pop vm-test-mock-responses)))
             (goto-char (point-min))
             ,@body)
         (kill-buffer imap-buffer)))))

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

(defun vm-test-init-message-data (message)
  "Initialize attributes and cached-data arrays for MESSAGE.
This is needed because vm-make-message doesn't create these arrays."
  ;; Initialize attributes array (element 2) if not present
  (unless (vm-attributes-of message)
    (vm-set-attributes-of message (make-vector vm-attributes-vector-length nil)))
  ;; Initialize cached-data array (element 3) if not present
  (unless (vm-cached-data-of message)
    (vm-set-cached-data-of message (make-vector vm-cached-data-vector-length nil))))

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
     ;; Initialize attributes and cached-data for each message
     (dolist (msg vm-message-list)
       (vm-test-init-message-data msg))
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

;;; Test file discovery

(defun vm-test-discover-test-files ()
  "Return list of test files in `vm-test-dir'.
Files match pattern vm-*-test.el, excluding vm-test-init.el."
  (sort (directory-files vm-test-dir nil "^vm-.*-test\\.el$")
        #'string<))

(defun vm-test-load-all-test-files ()
  "Load all test files from `vm-test-dir'."
  (let ((test-files (vm-test-discover-test-files)))
    (message "Loading %d test files..." (length test-files))
    (dolist (f test-files)
      (load (expand-file-name f vm-test-dir)))))

(provide 'vm-test-init)

;;; vm-test-init.el ends here
