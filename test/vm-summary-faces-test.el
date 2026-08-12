;;; vm-summary-faces-test.el --- Tests for VM's summary faces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Decorating the summary lines, and hiding the ones with a given face.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-when-compile (require 'vm-test-init))

;;; Summary faces (emacs-vm/vm#632)
;;
;; `vm-summary-faces-mode' turns the decoration of summary lines on and off,
;; and `vm-summary-faces-hide' hides the lines carrying one of those faces --
;; hiding the deleted messages, by default.  Neither had a test.

(defconst vm-summary-faces-test--folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nSubject: first\n\nOne.\n\n"
          "From bob@example.com Sun Aug  9 16:00:00 2026\n"
          "From: bob@example.com\nSubject: second\n\nTwo.\n\n")
  "Two messages, so one can be deleted and the other not.")

(defmacro vm-summary-faces-test--with-summary (spec &rest body)
  "Visit a folder with a summary and run BODY in the folder buffer.
SPEC is (SUMMARY-VAR), bound to the summary buffer.  It has to be captured
here: `vm-summary-faces-hide' leaves the summary buffer current, and
`vm-summary-buffer' is nil there -- it is the folder that knows its summary."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-summary-faces" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-summary-enable-faces nil)
               (vm-summary-show-threads nil))
           (write-region vm-summary-faces-test--folder nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             (vm-summarize)
             (let ((,(car spec) vm-summary-buffer))
               ,@body)))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-summary-faces-test--faces (summary)
  "Every face named on an overlay of SUMMARY, flattened."
  (let (faces)
    (with-current-buffer summary
      (dolist (o (overlays-in (point-min) (point-max)) faces)
        (let ((face (overlay-get o 'face)))
          (dolist (one (if (listp face) face (list face)))
            (when one (push one faces))))))))

(defun vm-summary-faces-test--invisible-count (summary)
  "How many overlays of SUMMARY are invisible."
  (let ((n 0))
    (with-current-buffer summary
      (dolist (o (overlays-in (point-min) (point-max)) n)
        (when (overlay-get o 'invisible)
          (setq n (1+ n)))))))

(ert-deftest vm-summary-faces-test-mode-turns-decoration-on-and-off ()
  "`vm-summary-faces-mode' toggles `vm-summary-enable-faces', and an argument
sets it rather than toggling: 0 for off, a positive number for on.

With it on the summary lines carry the faces `vm-summary-faces-alist' asks
for, so a deleted message's line is marked deleted."
  (vm-summary-faces-test--with-summary (summary)
    (should-not vm-summary-enable-faces)
    (vm-set-deleted-flag (car vm-message-list) t)
    (vm-summary-faces-mode)
    (should vm-summary-enable-faces)
    (vm-update-summary-and-mode-line)
    (should (memq 'vm-summary-deleted
                  (vm-summary-faces-test--faces summary)))
    ;; an argument sets it outright
    (vm-summary-faces-mode 0)
    (should-not vm-summary-enable-faces)
    (vm-summary-faces-mode 1)
    (should vm-summary-enable-faces)))

(ert-deftest vm-summary-faces-test-hiding-the-lines-with-a-face ()
  "`vm-summary-faces-hide' hides the summary lines carrying the face named,
and shows them again.  Only those lines: the message that is not deleted stays
visible while the deleted one is hidden."
  (vm-summary-faces-test--with-summary (summary)
    (vm-set-deleted-flag (car vm-message-list) t)
    (vm-summary-faces-mode 1)
    (vm-update-summary-and-mode-line)
    (should (equal (vm-summary-faces-test--invisible-count summary) 0))
    (vm-summary-faces-hide "vm-summary-deleted")
    (should (equal (vm-summary-faces-test--invisible-count summary) 1))
    (vm-summary-faces-hide "vm-summary-deleted")
    (should (equal (vm-summary-faces-test--invisible-count summary) 0))))

(ert-deftest vm-summary-faces-test-hiding-a-face-no-line-carries ()
  "Hiding a face nothing carries hides nothing, rather than everything."
  (vm-summary-faces-test--with-summary (summary)
    (vm-summary-faces-mode 1)
    (vm-update-summary-and-mode-line)
    (vm-summary-faces-hide "vm-summary-flagged")
    (should (equal (vm-summary-faces-test--invisible-count summary) 0))))

(provide 'vm-summary-faces-test)

;;; vm-summary-faces-test.el ends here
