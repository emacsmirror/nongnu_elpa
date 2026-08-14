;;; vm-bug-report-test.el --- Tests for VM's bug-report commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Arming the IMAP and POP trace keeping, and what the report carries.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-when-compile (require 'vm-test-init))

;;; The bug-report commands (emacs-vm/vm#632)
;;
;; Four commands, two per protocol, that no test called.  The start command
;; arms the trace keeping; the submit command hands the kept traces to
;; `vm-submit-bug-report' as a hook that writes them into the report.  Nothing
;; here sends mail: the submit is stubbed and what it was given is inspected.

(defconst vm-bug-report-test--folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nSubject: one\n\nA body.\n\n")
  "One message: these commands only need a folder to be called in.")

(defmacro vm-bug-report-test--in-a-folder (&rest body)
  "Visit a folder and run BODY in it."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-bug-report" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-imap-keep-trace-buffer nil)
               (vm-pop-keep-trace-buffer nil)
               (vm-kept-imap-buffers nil)
               (vm-kept-pop-buffers nil))
           (write-region vm-bug-report-test--folder nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-bug-report-test-starting-arms-the-trace-keeping ()
  "`vm-imap-start-bug-report' and its POP twin arm the trace keeping and
throw away whatever traces were already kept.

Keeping traces is off by default -- a session's trace buffer is killed when
the session ends -- so without this the report has nothing to carry."
  (vm-bug-report-test--in-a-folder
    (should-not vm-imap-keep-trace-buffer)
    (setq vm-kept-imap-buffers (list (current-buffer)))
    (vm-imap-start-bug-report)
    (should (equal vm-imap-keep-trace-buffer 20))
    (should-not vm-kept-imap-buffers)
    (should-not vm-pop-keep-trace-buffer)
    (setq vm-kept-pop-buffers (list (current-buffer)))
    (vm-pop-start-bug-report)
    (should (equal vm-pop-keep-trace-buffer 20))
    (should-not vm-kept-pop-buffers)))

(ert-deftest vm-bug-report-test-submitting-asks-when-nothing-was-armed ()
  "The submit command asks whether the start command was run, and only when
the trace keeping is off.  With it on there is nothing to ask about."
  (vm-bug-report-test--in-a-folder
    (let ((asked 0))
      (cl-letf (((symbol-function 'vm-submit-bug-report) #'ignore)
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked (1+ asked)) t)))
        (vm-imap-submit-bug-report)
        (should (equal asked 1))
        (vm-imap-start-bug-report)
        (vm-imap-submit-bug-report)
        (should (equal asked 1))))))

(ert-deftest vm-bug-report-test-the-report-carries-the-kept-traces ()
  "The submit command hands `vm-submit-bug-report' a hook, and running that
hook writes the kept trace buffers into the report.

That hook is the whole point of these commands: the report is useless without
the conversation that went wrong."
  (vm-bug-report-test--in-a-folder
    (let ((trace (get-buffer-create "vm-bug-report-test-trace"))
          (hooks nil))
      (unwind-protect
          (progn
            (with-current-buffer trace
              (insert "VM IMAP 1 LOGIN <omitted>\nVM OK LOGIN completed\n"))
            (vm-imap-start-bug-report)
            (setq vm-kept-imap-buffers (list trace))
            (cl-letf (((symbol-function 'vm-submit-bug-report)
                       (lambda (&optional _pre post) (setq hooks post)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-imap-submit-bug-report))
            (should hooks)
            (with-temp-buffer
              (dolist (hook hooks) (funcall hook))
              (let ((report (buffer-string)))
                (should (string-match-p "IMAP Trace buffers" report))
                (should (string-match-p "VM OK LOGIN completed" report)))))
        (when (buffer-live-p trace)
          (with-current-buffer trace (set-buffer-modified-p nil))
          (kill-buffer trace))))))

(ert-deftest vm-bug-report-test-the-pop-report-carries-its-traces ()
  "The same for POP, whose traces are kept in a list of its own."
  (vm-bug-report-test--in-a-folder
    (let ((trace (get-buffer-create "vm-bug-report-test-pop-trace"))
          (hooks nil))
      (unwind-protect
          (progn
            (with-current-buffer trace
              (insert "+OK POP3 ready\nUSER vmtest\n+OK user accepted\n"))
            (vm-pop-start-bug-report)
            (setq vm-kept-pop-buffers (list trace))
            (cl-letf (((symbol-function 'vm-submit-bug-report)
                       (lambda (&optional _pre post) (setq hooks post)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-pop-submit-bug-report))
            (should hooks)
            (with-temp-buffer
              (dolist (hook hooks) (funcall hook))
              (let ((report (buffer-string)))
                (should (string-match-p "POP Trace buffers" report))
                (should (string-match-p "\\+OK user accepted" report)))))
        (when (buffer-live-p trace)
          (with-current-buffer trace (set-buffer-modified-p nil))
          (kill-buffer trace))))))

;;; The configuration the report carries (emacs-vm/vm#644)

(defun vm-bug-report-test--submit-and-read ()
  "Return the text of the report `vm-submit-bug-report' composes.
Kills the composition it leaves behind."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (vm-submit-bug-report)
          (setq buffer (current-buffer))
          (buffer-string))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      ;; reporter.el builds the report in a scratch buffer of its own and
      ;; leaves it behind for the next call to reuse
      (let ((scratch (get-buffer " *tmp-reporter-buffer*")))
        (when scratch (kill-buffer scratch))))))

(ert-deftest vm-bug-report-test-spool-files-as-a-list-of-lists-are-reported ()
  "REGRESSION: the report must carry `vm-spool-files' in its list-of-lists form.

The mapping over that form was called with the function and no list, and
`vm-mapcar' loops over `(car lists)' -- so it returned nil without signalling,
the `condition-case' fallback never ran, and the report told the maintainer
the user had no spool files at all.  Spool configuration is the first thing a
mail-fetching bug report needs."
  (let ((vm-spool-files
         (list (list "/tmp/inbox"
                     "pop:mail.example.invalid:110:pass:alice:s3cret"
                     "/tmp/inbox.crash"))))
    (let ((report (vm-bug-report-test--submit-and-read)))
      (should (string-match-p "vm-bug-spool-files" report))
      (should (string-match-p "mail\\.example\\.invalid" report))
      (should (string-match-p "/tmp/inbox\\.crash" report)))))

(ert-deftest vm-bug-report-test-spool-file-passwords-are-not-reported ()
  "The report is mailed or pasted into a public issue, so the password and
the login in a spool maildrop are replaced by stars.  Both forms of
`vm-spool-files' are checked: the list-of-lists form went unredacted as well
as unreported, since the mapping that redacts it was the one that did
nothing."
  (dolist (spool (list (list (list "/tmp/inbox"
                                   "pop:mail.example.invalid:110:pass:alice:s3cret"
                                   "/tmp/inbox.crash"))
                       (list "pop:mail.example.invalid:110:pass:alice:s3cret")))
    (let* ((vm-spool-files spool)
           (report (vm-bug-report-test--submit-and-read)))
      (should-not (string-match-p "s3cret" report))
      (should-not (string-match-p "alice" report))
      ;; what is left still identifies the server, which is the useful part
      (should (string-match-p "pop:mail\\.example\\.invalid:110:pass:\\*:\\*"
                              report)))))

(ert-deftest vm-bug-report-test-account-alist-passwords-are-not-reported ()
  "The IMAP and POP account and expunge alists are redacted the same way.
They hold maildrop strings with passwords in them, and go into the same
report."
  (let* ((drop "imap:mail.example.invalid:143:inbox:login:alice:s3cret")
         (vm-imap-account-alist (list (list drop "work")))
         (vm-pop-folder-alist (list (list "pop:mail.example.invalid:110:pass:alice:s3cret"
                                          "home")))
         (report (vm-bug-report-test--submit-and-read)))
    (should-not (string-match-p "s3cret" report))
    (should (string-match-p "mail\\.example\\.invalid" report))))

(provide 'vm-bug-report-test)

;;; vm-bug-report-test.el ends here
