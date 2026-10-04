;;; vm-dired-test.el --- Tests for attaching files from dired -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Attaching the file at point, and the marked files, to a VM composition.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired)

(eval-when-compile (require 'vm-test-init))

;;; Attaching from dired (emacs-vm/vm#632)
;;
;; `vm-dired-attach-file' attaches the file at point in a dired buffer to a
;; composition, and `vm-dired-do-attach-files' attaches every marked file.
;; Neither had a test.

(defmacro vm-dired-test--with-files (spec &rest body)
  "Make a directory of two files, a composition, and a dired buffer on it.
SPEC is (DIRECTORY-VAR COMPOSITION-VAR DIRED-VAR).  BODY runs with the dired
buffer current."
  (declare (indent 1) (debug t))
  `(let ((,(car spec) (file-name-as-directory (make-temp-file "vm-dired" t)))
         (before (buffer-list))
         ,(nth 1 spec) ,(nth 2 spec))
     (unwind-protect
         (let ((vm-frame-per-composition nil)
               (vm-mutable-frame-configuration nil)
               (vm-mail-mode-hook nil)
               (mail-signature nil)
               (vm-send-using-mime t))
           (write-region "The first file.\n" nil
                         (expand-file-name "one.txt" ,(car spec)) nil 'quiet)
           (write-region "The second file.\n" nil
                         (expand-file-name "two.txt" ,(car spec)) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-mail)
             (setq ,(nth 1 spec) (current-buffer))
             (setq ,(nth 2 spec) (dired-noselect ,(car spec)))
             (set-buffer ,(nth 2 spec))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory ,(car spec) t))))

(defun vm-dired-test--tags (composition)
  "The file names of the attachment tags in COMPOSITION, in order."
  (with-current-buffer composition
    (let (names (start (point-min)))
      (while (string-match "\\[ATTACHMENT \\([^,]+\\)," (buffer-string) start)
        (push (match-string 1 (buffer-string)) names)
        (setq start (match-end 1)))
      (nreverse names))))

(ert-deftest vm-dired-test-attaching-the-file-at-point ()
  "`vm-dired-attach-file' attaches the file point is on, and gives it a type
worked out from the name rather than calling everything binary."
  (vm-dired-test--with-files (dir composition dired)
    (dired-goto-file (expand-file-name "one.txt" dir))
    (vm-dired-attach-file composition)
    (should (equal (vm-dired-test--tags composition) '("one.txt")))
    (with-current-buffer composition
      (should (string-match-p "text/plain" (buffer-string))))))

(ert-deftest vm-dired-test-attaching-every-marked-file ()
  "`vm-dired-do-attach-files' attaches all the marked files, which is its
whole difference from the one at point."
  (vm-dired-test--with-files (_dir composition dired)
    (goto-char (point-min))
    (dired-mark-files-regexp "\\.txt\\'")
    (vm-dired-do-attach-files composition)
    (should (equal (sort (vm-dired-test--tags composition) #'string<)
                   '("one.txt" "two.txt")))))

(ert-deftest vm-dired-test-attaching-needs-mime-sending-enabled ()
  "Both commands refuse when `vm-send-using-mime' is off, and say what to
set: an attachment cannot be sent without MIME."
  (vm-dired-test--with-files (dir composition dired)
    (dired-goto-file (expand-file-name "one.txt" dir))
    (let ((vm-send-using-mime nil)
          (text-quoting-style 'grave))
      (dolist (command '(vm-dired-attach-file vm-dired-do-attach-files))
        (should (string-match-p
                 "set vm-send-using-mime non-nil"
                 (cadr (should-error (funcall command composition))))))
      (should (null (vm-dired-test--tags composition))))))

(ert-deftest vm-dired-test-attached-files-survive-encoding ()
  "What the tags stand for reaches the message: encoding the composition
carries both files' contents."
  (vm-dired-test--with-files (_dir composition dired)
    (goto-char (point-min))
    (dired-mark-files-regexp "\\.txt\\'")
    (vm-dired-do-attach-files composition)
    (with-current-buffer composition
      (goto-char (point-max))
      (insert "Here are the files.\n")
      (vm-mime-encode-composition)
      (let ((encoded (buffer-string)))
        (should (string-match-p "The first file" encoded))
        (should (string-match-p "The second file" encoded))
        (should (string-match-p "multipart/mixed" encoded)))
      (set-buffer-modified-p nil))))

(provide 'vm-dired-test)

;;; vm-dired-test.el ends here
