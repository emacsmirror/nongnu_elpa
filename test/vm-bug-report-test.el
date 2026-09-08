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

;;; The trace of the session still running (emacs-vm/vm#822)

(defun vm-bug-report-test--running-session (buffer)
  "A live session whose process buffer is BUFFER.
A process of its own, so `vm-net-session-live-p' answers t: the folder's
session is what the report has to reach into, and a struct with no process
is not one."
  (let ((process (start-process "vm-bug-report-test" buffer "cat")))
    (set-process-query-on-exit-flag process nil)
    (vm-net-session :process process :name "imap" :timeout 5)))

(ert-deftest vm-bug-report-test-the-report-carries-the-running-session ()
  "The trace of the session still running is in the report.

It is not in `vm-kept-imap-buffers', which a session joins only when it ends,
and it is the one a reader is most likely reporting about.  The command used
to end the folder's session to flush its trace into the ring; on the driver
that would abort a fetch in flight."
  (vm-bug-report-test--in-a-folder
    (let* ((buffer (get-buffer-create "vm-bug-report-test-live"))
           (session (vm-bug-report-test--running-session buffer))
           (hooks nil))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (insert "VM IMAP 3 UID FETCH 1:* (UID RFC822.SIZE FLAGS)\n"))
            (setq vm-imap-net-session session)
            (cl-letf (((symbol-function 'vm-submit-bug-report)
                       (lambda (&optional _pre post) (setq hooks post)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-imap-submit-bug-report))
            (with-temp-buffer
              (dolist (hook hooks) (funcall hook))
              (should (string-match-p "UID FETCH 1:\\*" (buffer-string))))
            ;; and it was not ended to get there
            (should (vm-net-session-live-p session)))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest vm-bug-report-test-a-running-session-is-not-reported-twice ()
  "A session already in the ring is not written into the report twice."
  (vm-bug-report-test--in-a-folder
    (let* ((buffer (get-buffer-create "vm-bug-report-test-both"))
           (session (vm-bug-report-test--running-session buffer)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "VM IMAP 1 NOOP\n"))
            (setq vm-imap-net-session session)
            (setq vm-kept-imap-buffers (list buffer))
            (should (equal (vm-imap-net-trace-buffers) (list buffer))))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest vm-bug-report-test-a-session-that-has-gone-is-named-not-skipped ()
  "A buffer killed since it was kept is named in the report, not left out.
A report short of a session should say so rather than look complete."
  (with-temp-buffer
    (let ((dead (get-buffer-create "vm-bug-report-test-dead")))
      (kill-buffer dead)
      (vm-insert-session-traces "IMAP" (list dead))
      (let ((report (buffer-string)))
        (should (string-match-p "IMAP Trace buffers" report))
        (should (string-match-p "this buffer is gone" report))))))

(ert-deftest vm-bug-report-test-no-session-at-all-still-makes-a-report ()
  "With nothing kept and nothing running, the report has its heading and no
sessions.  A reader who forgot to arm the trace keeping still gets a report."
  (vm-bug-report-test--in-a-folder
    (should (equal (vm-imap-net-trace-buffers) nil))
    (should (equal (vm-pop-net-trace-buffers) nil))
    (with-temp-buffer
      (vm-insert-session-traces "IMAP" nil)
      (should (string-match-p "IMAP Trace buffers" (buffer-string))))))

(ert-deftest vm-bug-report-test-the-pop-report-carries-its-running-session ()
  "The same for POP: the session still running is in the report."
  (vm-bug-report-test--in-a-folder
    (let* ((buffer (get-buffer-create "vm-bug-report-test-pop-live"))
           (session (vm-bug-report-test--running-session buffer))
           (hooks nil))
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "VM POP 1 RETR 4\n"))
            (setq vm-pop-net-session session)
            (cl-letf (((symbol-function 'vm-submit-bug-report)
                       (lambda (&optional _pre post) (setq hooks post)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-pop-submit-bug-report))
            (with-temp-buffer
              (dolist (hook hooks) (funcall hook))
              (let ((report (buffer-string)))
                (should (string-match-p "POP Trace buffers" report))
                (should (string-match-p "RETR 4" report))))
            (should (vm-net-session-live-p session)))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

;;; A trace too long to send

(defmacro vm-bug-report-test--with-a-long-trace (var &rest body)
  "Bind VAR to a buffer holding a long trace, and run BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (generate-new-buffer "vm-bug-report-test-long")))
     (unwind-protect
         (progn
           (with-current-buffer ,var
             (insert "VM IMAP 1 OK [CAPABILITY IMAP4rev1] the start\n")
             (dotimes (n 400)
               (insert (format "* %d FETCH (UID %d RFC822.SIZE 1000 FLAGS ())\n"
                               (1+ n) (1+ n))))
             (insert "VM IMAP 9 BAD the end\n"))
           ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var (set-buffer-modified-p nil))
         (kill-buffer ,var)))))

(ert-deftest vm-bug-report-test-a-long-trace-keeps-both-ends ()
  "A trace over the limit keeps its start and its end and says what went.

A synchronise of a large mailbox leaves most of a megabyte of identical FETCH
lines, and a report that size cannot be sent.  What matters is at the ends:
the start says what the server is and what was asked, the end says where it
went wrong."
  (vm-bug-report-test--with-a-long-trace trace
    (let ((vm-session-trace-max-size 500))
      (with-temp-buffer
        (vm-insert-session-traces "IMAP" (list trace))
        (let ((report (buffer-string)))
          (should (string-match-p "OK \\[CAPABILITY IMAP4rev1\\] the start" report))
          (should (string-match-p "BAD the end" report))
          (should (string-match-p "left out of the middle" report))
          ;; and it really is shorter than the trace it came from
          (should (< (length report) (buffer-size trace))))))))

(ert-deftest vm-bug-report-test-a-long-trace-is-not-itself-altered ()
  "The trace buffer is not written into while it is being reported.
`insert-buffer-substring' inserts into the buffer that is current, so reading
the positions by making the trace current copies it into itself."
  (vm-bug-report-test--with-a-long-trace trace
    (let ((before (with-current-buffer trace (buffer-string)))
          (vm-session-trace-max-size 500))
      (with-temp-buffer
        (vm-insert-session-traces "IMAP" (list trace)))
      (should (equal (with-current-buffer trace (buffer-string)) before)))))

(ert-deftest vm-bug-report-test-a-short-trace-is-carried-whole ()
  "Under the limit nothing is left out, and nothing is said about leaving
anything out."
  (vm-bug-report-test--with-a-long-trace trace
    (let ((vm-session-trace-max-size 1000000))
      (with-temp-buffer
        (vm-insert-session-traces "IMAP" (list trace))
        (let ((report (buffer-string)))
          (should-not (string-match-p "left out of the middle" report))
          (should (string-match-p "\\* 200 FETCH" report)))))))

(ert-deftest vm-bug-report-test-no-limit-carries-every-trace-whole ()
  "Nil for the limit carries the whole of it, which is the way to get a trace
the elision has cut something out of."
  (vm-bug-report-test--with-a-long-trace trace
    (let ((vm-session-trace-max-size nil))
      (with-temp-buffer
        (vm-insert-session-traces "IMAP" (list trace))
        (should-not (string-match-p "left out of the middle" (buffer-string)))
        (should (string-match-p "\\* 200 FETCH" (buffer-string)))))))

(provide 'vm-bug-report-test)

;;; vm-bug-report-test.el ends here
