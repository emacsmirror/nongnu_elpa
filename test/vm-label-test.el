;;; vm-label-test.el --- Tests for VM message labels -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Attaching, removing and keeping the user-defined labels of a message.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-when-compile (require 'vm-test-init))

;;; Labels (emacs-vm/vm#632)
;;
;; The label commands were called by no test.  A label is user-defined text
;; kept with the message and written into the folder, so what matters is that
;; it goes on, comes off, and is still there after the folder is saved and
;; read again.

(defconst vm-label-test--folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nSubject: one\n\nThe first.\n\n"
          "From bob@example.com Sun Aug  9 16:00:00 2026\n"
          "From: bob@example.com\nSubject: two\n\nThe second.\n\n"
          "From carol@example.com Mon Aug 10 16:00:00 2026\n"
          "From: carol@example.com\nSubject: three\n\nThe third.\n\n")
  "Three messages to hang labels on.")

(defmacro vm-label-test--with-folder (spec &rest body)
  "Visit a folder of three messages, select the first, and run BODY.
SPEC is (FILE-VAR), bound to the folder's file name."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-labels" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-summary-show-threads nil))
           (write-region vm-label-test--folder nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-label-test--labels (n)
  "The labels of message N, counting from 1, sorted."
  (sort (copy-sequence (vm-labels-of (nth (1- n) vm-message-list))) #'string<))

(ert-deftest vm-label-test-adding-and-deleting-labels ()
  "`vm-add-message-labels' attaches a space-separated list of labels and
`vm-delete-message-labels' takes the named ones off, leaving the rest."
  (vm-label-test--with-folder (_file)
    (vm-add-message-labels "urgent later" 1)
    (should (equal (vm-label-test--labels 1) '("later" "urgent")))
    (vm-delete-message-labels "urgent" 1)
    (should (equal (vm-label-test--labels 1) '("later")))
    ;; deleting one that is not there changes nothing
    (vm-delete-message-labels "absent" 1)
    (should (equal (vm-label-test--labels 1) '("later")))))

(ert-deftest vm-label-test-a-count-labels-several-messages ()
  "The count argument labels that many messages from the current one."
  (vm-label-test--with-folder (_file)
    (vm-add-message-labels "batch" 2)
    (should (equal (vm-label-test--labels 1) '("batch")))
    (should (equal (vm-label-test--labels 2) '("batch")))
    (should (null (vm-label-test--labels 3)))))

(ert-deftest vm-label-test-existing-labels-only ()
  "`vm-add-existing-message-labels' adds only labels the folder has seen,
and reports the rest rather than adding them.  That is its whole difference
from `vm-add-message-labels': it is for applying a label you already use, not
for inventing one by typo.

The labels are read back in the folder buffer on purpose: a label it declined
is listed in a *Ignored Labels* buffer, which the command makes current and
does not put back, so a caller that reads `vm-message-list' afterwards reads
it from there and finds nothing."
  (vm-label-test--with-folder (_file)
    (let ((folder (current-buffer)))
      (vm-add-message-labels "known" 1)
      (setq vm-message-pointer (cdr vm-message-list))
      (vm-add-existing-message-labels "known unheardof" 1)
      (should (get-buffer "*Ignored Labels*"))
      (should (string-match-p "unheardof"
                              (with-current-buffer "*Ignored Labels*"
                                (buffer-string))))
      (with-current-buffer folder
        (should (equal (vm-label-test--labels 2) '("known")))))))

(ert-deftest vm-label-test-labels-survive-saving-and-reading ()
  "A label is written into the folder and is still there next time.
Labels live in an X-VM-Labels header, so this is the test that says they are
kept rather than merely held in memory until the folder is closed."
  (vm-label-test--with-folder (file)
    (vm-add-message-labels "keepme" 1)
    (vm-save-folder)
    (let ((on-disk (with-temp-buffer (insert-file-contents file)
                                     (buffer-string))))
      (should (string-match-p "X-VM-Labels:.*keepme" on-disk)))
    ;; read it again, in a buffer of its own
    (let ((again (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer again
            (vm-mode)
            (should (member "keepme" (vm-labels-of (car vm-message-list)))))
        (with-current-buffer again (set-buffer-modified-p nil))
        (kill-buffer again)))))

(ert-deftest vm-label-test-a-label-search-folder-selects-by-label ()
  "`vm-create-label-virtual-folder' collects the messages carrying a label."
  (vm-label-test--with-folder (_file)
    (vm-add-message-labels "wanted" 1)
    (setq vm-message-pointer (nthcdr 2 vm-message-list))
    (vm-add-message-labels "wanted" 1)
    (setq vm-message-pointer vm-message-list)
    (vm-create-label-virtual-folder "wanted")
    (should (equal (mapcar #'vm-su-subject vm-message-list) '("one" "three")))))

(ert-deftest vm-label-test-labels-are-compared-without-case ()
  "Label names are compared case-insensitively, as the docstrings say, so
deleting URGENT takes off urgent."
  (vm-label-test--with-folder (_file)
    (vm-add-message-labels "urgent" 1)
    (vm-delete-message-labels "URGENT" 1)
    (should (null (vm-label-test--labels 1)))))

(ert-deftest vm-label-test-a-comma-separates-labels-too ()
  "A comma separates label names, as spaces and tabs do.
The docstrings said \"a space separated list\" and nothing said otherwise, so
a reader typing `work,home' would expect one label of that name and get two.
`vm-add-or-delete-message-labels' splits on any of the control characters,
space, comma, and the bytes above DEL, so the docstrings now say spaces or
commas."
  (vm-label-test--with-folder (_file)
    (vm-add-message-labels "work,home" 1)
    (should (equal (sort (copy-sequence (vm-label-test--labels 1)) #'string<)
                   '("home" "work")))
    ;; and the same the other way: deleting by comma-separated names
    (vm-delete-message-labels "home,work" 1)
    (should (null (vm-label-test--labels 1)))))

(ert-deftest vm-label-test-a-repeated-label-is-kept-once ()
  "Naming a label twice attaches it once."
  (vm-label-test--with-folder (_file)
    (vm-add-message-labels "urgent urgent" 1)
    (should (equal (vm-label-test--labels 1) '("urgent")))))

(provide 'vm-label-test)

;;; vm-label-test.el ends here
