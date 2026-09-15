;;; vm-configuration-test.el --- Tests for vm-check-configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; What `vm-check-configuration' reports, and what it stays quiet about
;; (emacs-vm/vm#816).

;;; Code:

(require 'ert)
(require 'vm)
(require 'seq)
(require 'cl-lib)

(eval-when-compile (require 'vm-test-init))

(defmacro vm-configuration-test--with-a-working-setup (&rest body)
  "Run BODY with everything the checker looks at set to something sound.
Each test then breaks one thing, so a report can only come from that."
  (declare (indent 0))
  `(let ((mail-user-agent 'vm-user-agent)
         (user-mail-address "becky@example.com")
         (mail-host-address nil)
         (send-mail-function 'smtpmail-send-it)
         (vm-folder-directory temporary-file-directory)
         (vm-spool-files (list "/var/mail/becky"))
         (vm-imap-account-alist nil)
         (vm-pop-folder-alist nil))
     ,@body))

(ert-deftest vm-configuration-test-a-working-setup-is-quiet ()
  "Nothing is reported when everything the checker looks at is set.
The fixture the other tests break, so a false positive here would make all
of them meaningless."
  (vm-configuration-test--with-a-working-setup
    (should-not (vm-configuration-problems))))

(ert-deftest vm-configuration-test-the-mail-agent-is-checked ()
  "`mail-user-agent' left at Emacs's default is reported.
VM can be loaded and configured and still not be what C-x m composes with,
which looks like VM being ignored rather than like a setting."
  (vm-configuration-test--with-a-working-setup
    (let ((mail-user-agent 'message-user-agent))
      (let ((problems (vm-configuration-problems)))
        (should (= 1 (length problems)))
        (should (string-match-p "mail-user-agent" (car problems)))
        (should (string-match-p "vm-user-agent" (car problems)))
        (should (string-match-p "Mail agent in the VM manual" (car problems)))))))

(ert-deftest vm-configuration-test-a-machine-made-address-is-reported ()
  "An address Emacs invented from the host name is reported.
Emacs sets `user-mail-address' to the login name at the system name where
nothing tells it otherwise, so mail goes out from an address that cannot be
replied to and nothing says so."
  (vm-configuration-test--with-a-working-setup
    (let ((user-mail-address (concat "becky@" (system-name))))
      (should (vm-address-looks-machine-made-p user-mail-address))
      (should (string-match-p "user-mail-address"
                              (car (vm-configuration-problems))))))
  ;; and the same through mail-host-address, which is the other way Emacs
  ;; builds one
  (vm-configuration-test--with-a-working-setup
    (let ((mail-host-address "laptop.local")
          (user-mail-address "becky@laptop.local"))
      (should (vm-address-looks-machine-made-p user-mail-address))))
  ;; a domain with no dot cannot be resolved, so it is machine-made too
  (should (vm-address-looks-machine-made-p "becky@laptop"))
  ;; a real address is left alone
  (should-not (vm-address-looks-machine-made-p "becky@example.com")))

(ert-deftest vm-configuration-test-an-unset-sender-is-reported ()
  "`sendmail-query-once' means Emacs has not been told how to send.
It asks once and remembers, so the answer is given in a hurry and never
looked at again."
  (vm-configuration-test--with-a-working-setup
    (let ((send-mail-function 'sendmail-query-once))
      (should (string-match-p "smtpmail-send-it"
                              (car (vm-configuration-problems))))))
  (vm-configuration-test--with-a-working-setup
    (let ((send-mail-function nil))
      (should (string-match-p "nil" (car (vm-configuration-problems))))))
  ;; a sender that has been chosen is not commented on
  (vm-configuration-test--with-a-working-setup
    (let ((send-mail-function 'sendmail-send-it))
      (should-not (vm-configuration-problems)))))

(ert-deftest vm-configuration-test-the-folder-directory-is-checked ()
  "An unset or non-existent `vm-folder-directory' is reported."
  (vm-configuration-test--with-a-working-setup
    (let ((vm-folder-directory nil))
      (should (string-match-p "vm-folder-directory"
                              (car (vm-configuration-problems))))))
  (vm-configuration-test--with-a-working-setup
    (let ((vm-folder-directory "/no/such/directory/anywhere"))
      (should (string-match-p "not a\n?\\s-*directory"
                              (car (vm-configuration-problems)))))))

(ert-deftest vm-configuration-test-having-no-mail-source-is-reported ()
  "Nothing to get mail from is reported, and any one source is enough.
`vm-spool-files' is consulted through its function, which falls back to
MAILPATH and MAIL, so the test clears those as well."
  (let ((process-environment (append '("MAILPATH=" "MAIL=")
                                     process-environment)))
    (vm-configuration-test--with-a-working-setup
      (let ((vm-spool-files nil))
        (should (string-match-p "Nothing says where your mail comes from"
                                (car (vm-configuration-problems))))))
    ;; an IMAP account on its own is a mail source
    (vm-configuration-test--with-a-working-setup
      (let ((vm-spool-files nil)
            (vm-imap-account-alist
             '(("imap-ssl:mail.example.com:993:inbox:login:becky:*" "work"))))
        (should-not (vm-configuration-problems))))))

;;; Maildrop specifications
;;
;; `vm-imap-parse-spec-to-list' and `vm-pop-parse-spec-to-list' take whatever
;; they are given: any leading word is a type and any number of fields is a
;; spec.  So a typo is not reported where it was made, and the session fails
;; later saying something about the server.

(ert-deftest vm-configuration-test-an-unknown-maildrop-type-is-reported ()
  "A misspelt maildrop type is named, with the types that exist."
  (vm-configuration-test--with-a-working-setup
    (let ((vm-imap-account-alist
           '(("imapssl:mail.example.com:993:inbox:login:becky:*" "work"))))
      (let ((problems (vm-configuration-problems)))
        (should (= 1 (length problems)))
        (should (string-match-p "imapssl" (car problems)))
        (should (string-match-p "imap-ssl" (car problems)))))))

(ert-deftest vm-configuration-test-a-short-maildrop-is-reported ()
  "A maildrop with the wrong number of fields is reported with both counts."
  (vm-configuration-test--with-a-working-setup
    (let ((vm-imap-account-alist
           '(("imap-ssl:mail.example.com:993:inbox:login:becky" "work"))))
      (let ((problems (vm-configuration-problems)))
        (should (= 1 (length problems)))
        (should (string-match-p "6 colon-separated" (car problems)))
        (should (string-match-p "takes 7" (car problems)))))))

(ert-deftest vm-configuration-test-a-good-maildrop-is-quiet ()
  "Each type VM knows, written out in full, is accepted."
  (vm-configuration-test--with-a-working-setup
    (dolist (spec '("imap:mail.example.com:143:inbox:login:becky:*"
                    "imap-ssl:mail.example.com:993:inbox:login:becky:*"
                    "imap-ssh:mail.example.com:22:inbox:login:becky:*"
                    ;; the user field is an address on a good many servers,
                    ;; so the at sign has to be ordinary here
                    "imap-ssl:imap.gmail.com:993:INBOX:login:becky@example.com:*"
                    ;; and a mailbox name that is not the inbox
                    "imap-ssl:mail.example.com:993:some-project:login:becky:*"))
      (let ((vm-imap-account-alist (list (list spec "work"))))
        (should-not (vm-configuration-problems))))))

(ert-deftest vm-configuration-test-a-local-spool-file-is-not-a-maildrop ()
  "A plain file name in `vm-spool-files' is not read as a maildrop spec.
A path has no leading type word, and reporting one as an unknown type would
make the command useless to everybody reading local mail."
  (vm-configuration-test--with-a-working-setup
    (let ((vm-spool-files (list "/var/mail/becky" "~/mail/incoming")))
      (should-not (vm-configuration-problems))))
  ;; a POP spec in vm-spool-files, which is where they are written, is checked
  (vm-configuration-test--with-a-working-setup
    (let ((vm-spool-files (list "pop:mail.example.com:110:pass:becky")))
      (should (string-match-p "5 colon-separated"
                              (car (vm-configuration-problems)))))))

(ert-deftest vm-configuration-test-the-command-counts-what-it-found ()
  "The command answers how many problems it reported, and says so plainly.
It is called for its buffer, but the count is what a test and a hook can
use."
  (vm-configuration-test--with-a-working-setup
    (should (equal 0 (vm-check-configuration))))
  (vm-configuration-test--with-a-working-setup
    (let ((mail-user-agent 'message-user-agent)
          (send-mail-function nil))
      (should (equal 2 (vm-check-configuration)))))
  ;; the buffer names the manual chapter that works through all of it
  (vm-configuration-test--with-a-working-setup
    (let ((mail-user-agent 'message-user-agent))
      (vm-check-configuration)
      (with-current-buffer "*VM Configuration*"
        (should (string-match-p "Setting Up" (buffer-string)))))))

;;; vm-setup: asking everything before writing anything (emacs-vm/vm#816)
;;
;; The property worth testing is negative: no file appears until the last
;; question has an answer, and a file that exists is not replaced without
;; being asked about.  So most of these count files in an empty directory.

(defvar vm-setup-test--answers nil
  "What the stubbed readers hand back, in the order they are asked for.")

(defvar vm-setup-test--yes nil
  "What the stubbed `y-or-n-p' hands back, in order.")

(defvar vm-setup-test--questions nil
  "Every prompt the stubs were given, most recent last.")

(define-error 'vm-setup-test-no-answer
  "The stubbed reader has run out of answers")

(defun vm-setup-test--answer (prompt)
  "The next answer, recording PROMPT.
Signals when they run out, which stands for C-g at that question: a test
that returned nil instead would go on to the next question with an answer
no reader could have given.

The signal is `vm-setup-test-no-answer' and not `quit', which ert takes
for an interruption of the whole run rather than a result.  Either unwinds
out of `vm-setup' at the same point, which is what these tests are
about."
  (push prompt vm-setup-test--questions)
  (unless vm-setup-test--answers (signal 'vm-setup-test-no-answer nil))
  (pop vm-setup-test--answers))

(defmacro vm-setup-test--answering (answers yeses &rest body)
  "Run BODY with the reading functions answering ANSWERS and YESES."
  (declare (indent 2))
  `(let ((vm-setup-test--answers ,answers)
         (vm-setup-test--yes ,yeses)
         (vm-setup-test--questions nil))
     (cl-letf (((symbol-function 'read-string)
                (lambda (prompt &rest _) (vm-setup-test--answer prompt)))
               ((symbol-function 'read-directory-name)
                (lambda (prompt &rest _) (vm-setup-test--answer prompt)))
               ((symbol-function 'read-file-name)
                (lambda (prompt &rest _) (vm-setup-test--answer prompt)))
               ((symbol-function 'completing-read)
                (lambda (prompt &rest _) (vm-setup-test--answer prompt)))
               ((symbol-function 'read-number) (lambda (&rest _) 587))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt &rest _)
                  (push prompt vm-setup-test--questions)
                  (unless vm-setup-test--yes
                    (signal 'vm-setup-test-no-answer nil))
                  (pop vm-setup-test--yes)))
               ((symbol-function 'switch-to-buffer) (lambda (&rest _) nil)))
       ,@body)))

(defmacro vm-setup-test--in-an-empty-directory (var &rest body)
  "Run BODY with VAR bound to a new empty directory, removed afterwards."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "vm-setup-" t)))
          ;; and run there: a name the wizard is given without a directory
          ;; is expanded against `default-directory', so a test that left
          ;; that pointing at the source tree could write into it and still
          ;; find the temporary directory empty.  Four files did.
          (default-directory ,var))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun vm-setup-test--files (directory)
  "The files in DIRECTORY, ignoring the dot entries and any backup."
  (seq-remove (lambda (f) (string-suffix-p "~" f))
              (directory-files directory nil "\\`[^.]")))

(defconst vm-setup-test--imap-answers
  '("Becky Bee" "becky@example.com" "MAILDIR" "INBOX"
    "IMAP server" "mail.example.com" "INBOX" "becky"
    "smtp.example.com" "becky@example.com")
  "Answers for an IMAP setup; MAILDIR is replaced with a real directory.")

(defun vm-setup-test--answers-for (maildir destination)
  "The answer list for MAILDIR, with DESTINATION asked for last.
The destination is read with `read-file-name', which is also what asks for
a spool file, so it has to come after every other answer."
  (append (mapcar (lambda (a) (if (equal a "MAILDIR") maildir a))
                  vm-setup-test--imap-answers)
          (list destination)))

(ert-deftest vm-setup-test-writes-the-file-it-was-asked-for ()
  "The settings go where the reader said, and load."
  (vm-setup-test--in-an-empty-directory dir
    (let ((destination (expand-file-name "prefs.el" dir))
          (maildir (expand-file-name "mail/" dir)))
      (vm-setup-test--answering
          (vm-setup-test--answers-for maildir destination)
          ;; create the directory, TLS, an SMTP server, the mail agent
          (list t t t t)
        (let ((vm-preferences-file destination))
          (should (equal destination (vm-setup)))))
      (should (file-exists-p destination))
      (with-temp-buffer
        (insert-file-contents destination)
        (let ((written (buffer-string)))
          (should (string-match-p "becky@example.com" written))
          (should (string-match-p "imap-ssl:mail\\.example\\.com:993" written))
          (should (string-match-p "smtpmail-send-it" written))
          (should (string-match-p "vm-user-agent" written))))
      ;; asking to create it is what created it, and only in the write phase
      (should (file-directory-p maildir)))))

(ert-deftest vm-setup-test-what-it-writes-satisfies-the-checker ()
  "REGRESSION: after `vm-setup', `vm-check-configuration' has nothing to say.
The two would otherwise be free to disagree, which is worse than either on
its own: a reader who has just answered every question would be told the
answers are wrong."
  (vm-setup-test--in-an-empty-directory dir
    (let ((destination (expand-file-name "prefs.el" dir))
          (maildir (expand-file-name "mail/" dir)))
      (vm-setup-test--answering
          (vm-setup-test--answers-for maildir destination)
          (list t t t t)
        (let ((vm-preferences-file destination)) (vm-setup)))
      ;; the file is loaded by vm-setup, so the variables are already set
      (should-not (vm-configuration-problems)))))

(ert-deftest vm-setup-test-nothing-is-written-until-every-answer-is-in ()
  "REGRESSION: an abort part way through leaves the disk alone.
Asked for on emacs-vm/vm#816.  Each question is a place a reader can change
their mind, and a half-written settings file is worse than none: VM loads it
and behaves in a way nothing accounts for."
  ;; stop at every question in turn, and at every yes-or-no in turn
  (dotimes (stop (length vm-setup-test--imap-answers))
    (vm-setup-test--in-an-empty-directory dir
      (let ((destination (expand-file-name "prefs.el" dir))
            (maildir (expand-file-name "mail/" dir)))
        (vm-setup-test--answering
            (seq-take (vm-setup-test--answers-for maildir destination) stop)
            (list t t t t)
          (let ((vm-preferences-file destination))
            (should-error (vm-setup) :type 'vm-setup-test-no-answer)))
        ;; nothing written, and the mail directory not created either: both
        ;; belong to the phase after the last question
        (should-not (vm-setup-test--files dir))
        (should-not (file-exists-p maildir)))))
  (dotimes (stop 4)
    (vm-setup-test--in-an-empty-directory dir
      (let ((destination (expand-file-name "prefs.el" dir))
            (maildir (expand-file-name "mail/" dir)))
        (vm-setup-test--answering
            (vm-setup-test--answers-for maildir destination)
            (seq-take (list t t t t) stop)
          (let ((vm-preferences-file destination))
            (should-error (vm-setup) :type 'vm-setup-test-no-answer)))
        (should-not (vm-setup-test--files dir))
        (should-not (file-exists-p maildir))))))

(ert-deftest vm-setup-test-an-existing-file-is-not-replaced-unasked ()
  "REGRESSION: replacing a file that exists is confirmed, and declining stops.
Asked for on emacs-vm/vm#816.  The destination defaults to a file VM owns,
but a reader may name `~/.vm', which is hand-written."
  (vm-setup-test--in-an-empty-directory dir
    (let ((destination (expand-file-name "prefs.el" dir))
          (maildir (expand-file-name "mail/" dir)))
      (with-temp-file destination (insert ";; mine\n"))
      ;; the last y-or-n-p is the overwrite question; say no to it
      (vm-setup-test--answering
          (vm-setup-test--answers-for maildir destination)
          (list t t t t nil)
        (let ((vm-preferences-file destination))
          (should-error (vm-setup)))
        ;; inside the macro: it binds the record of the prompts
        (should (seq-find (lambda (q)
                            (string-match-p "Replace what is in it" q))
                          vm-setup-test--questions)))
      (with-temp-buffer
        (insert-file-contents destination)
        (should (equal ";; mine\n" (buffer-string)))))))

(ert-deftest vm-setup-test-declining-says-what-to-do-instead ()
  "The error after declining names the way forward, not just the refusal."
  (vm-setup-test--in-an-empty-directory dir
    (let ((destination (expand-file-name "prefs.el" dir))
          (maildir (expand-file-name "mail/" dir))
          (text-quoting-style 'grave))
      (with-temp-file destination (insert ";; mine\n"))
      (vm-setup-test--answering
          (vm-setup-test--answers-for maildir destination)
          (list t t t t nil)
        (let* ((vm-preferences-file destination)
               (message (cadr (should-error (vm-setup)))))
          (should (string-match-p "Nothing was written" message))
          (should (string-match-p "does not exist" message)))))))

(ert-deftest vm-setup-test-a-directory-is-not-a-destination ()
  "Naming a directory is reported rather than written into."
  (vm-setup-test--in-an-empty-directory dir
    (let ((maildir (expand-file-name "mail/" dir))
          (text-quoting-style 'grave))
      (vm-setup-test--answering
          (vm-setup-test--answers-for maildir dir)
          (list t t t t)
        (let* ((vm-preferences-file (expand-file-name "prefs.el" dir))
               (message (cadr (should-error (vm-setup)))))
          (should (string-match-p "is a directory" message))))
      (should-not (vm-setup-test--files dir)))))

(ert-deftest vm-setup-test-a-later-answer-is-what-gets-written ()
  "Deciding against something asked about earlier is what the file shows.
Nothing is written while questions remain, so the answers are only read
after the last of them."
  (vm-setup-test--in-an-empty-directory dir
    (let ((destination (expand-file-name "prefs.el" dir))
          (maildir (expand-file-name "mail/" dir)))
      (vm-setup-test--answering
          (append (mapcar (lambda (a) (if (equal a "MAILDIR") maildir a))
                          '("Becky Bee" "becky@example.com" "MAILDIR" "INBOX"
                            "decide later"))
                  (list destination))
          ;; create the directory, no SMTP server, not the mail agent
          (list t nil nil)
        (let ((vm-preferences-file destination)) (vm-setup)))
      (with-temp-buffer
        (insert-file-contents destination)
        (let ((written (buffer-string)))
          ;; each declined thing is a comment saying what is unset, not a setq
          (should (string-match-p "Nothing says where new mail comes from"
                                  written))
          (should (string-match-p "`send-mail-function' is left alone"
                                  written))
          (should-not (string-match-p "(setq mail-user-agent" written))
          (should-not (string-match-p "smtpmail-smtp-server" written)))))))

(ert-deftest vm-setup-test-a-pop-account-is-written-as-a-maildrop ()
  "The POP answers make a spec with the fields POP takes."
  (vm-setup-test--in-an-empty-directory dir
    (let ((destination (expand-file-name "prefs.el" dir))
          (maildir (expand-file-name "mail/" dir)))
      (vm-setup-test--answering
          (append (mapcar (lambda (a) (if (equal a "MAILDIR") maildir a))
                          '("Becky Bee" "becky@example.com" "MAILDIR" "INBOX"
                            "POP server" "mail.example.com" "becky"))
                  (list destination))
          (list t t nil nil)            ; create dir, TLS, no smtp, no agent
        (let ((vm-preferences-file destination)) (vm-setup)))
      (with-temp-buffer
        (insert-file-contents destination)
        (should (string-match-p "pop-ssl:mail\\.example\\.com:995:pass:becky:\\*"
                                (buffer-string))))
      ;; and the spec it wrote is one the checker accepts
      (should-not (vm-maildrop-problem
                   "pop-ssl:mail.example.com:995:pass:becky:*" "test")))))

(ert-deftest vm-setup-test-the-file-it-writes-is-readable-lisp ()
  "Every form in the written file reads, whichever answers made it.
It is loaded by `load', so a form that does not read leaves VM failing at
startup with nothing pointing here."
  ;; the y-or-n-p answers differ per case: creating the directory always,
  ;; then TLS where the source is a server, then the SMTP server, then the
  ;; mail agent.
  (dolist (case '((("IMAP server" "mail.example.com" "INBOX" "becky"
                    "smtp.example.com" "becky@example.com") (t t t t))
                  (("POP server" "mail.example.com" "becky") (t t nil nil))
                  (("local spool file" "/var/mail/becky"
                    "smtp.example.com" "becky") (t t t))
                  (("decide later") (t nil nil))))
    (vm-setup-test--in-an-empty-directory dir
      (let ((destination (expand-file-name "prefs.el" dir))
            (maildir (expand-file-name "mail/" dir)))
        (vm-setup-test--answering
            (append (list "Becky Bee" "becky@example.com" maildir "INBOX")
                    (car case) (list destination))
            (cadr case)
          (let ((vm-preferences-file destination)) (vm-setup)))
        (with-temp-buffer
          (insert-file-contents destination)
          (goto-char (point-min))
          ;; read every form; a bad one signals here
          (condition-case err
              (while t (read (current-buffer)))
            (end-of-file nil)
            (error (ert-fail (format "%S in the file for %S"
                                     err (car case))))))))))

(ert-deftest vm-setup-test-an-empty-answer-is-asked-for-again ()
  "An answer that would be written as an empty string is asked for again.
An empty host makes a maildrop that fails from inside a session saying
nothing about where it came from, and an empty address sends mail from
nobody."
  (let ((asked 0))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _)
                 (setq asked (1+ asked))
                 (if (< asked 3) "" "becky@example.com"))))
      (should (equal "becky@example.com"
                     (vm-setup--read-required "Address: ")))
      ;; two empties, then the answer
      (should (equal 3 asked))))
  ;; whitespace is empty too
  (let ((answers (list " " "\t" "becky")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (pop answers))))
      (should (equal "becky" (vm-setup--read-required "Name: "))))))

(ert-deftest vm-setup-test-an-empty-smtp-login-is-left-out ()
  "No `smtpmail-smtp-user' is written where none was given.
Absent, smtpmail asks; written as an empty string, it would try to
authenticate as nobody."
  (let ((with-user '((smtp-server . "smtp.example.com") (smtp-service . 587)
                     (smtp-user . "becky")))
        (without '((smtp-server . "smtp.example.com") (smtp-service . 587)
                   (smtp-user . nil))))
    (should (string-match-p "smtpmail-smtp-user \"becky\""
                            (vm-setup--sending-form with-user)))
    (should-not (string-match-p "smtpmail-smtp-user"
                                (vm-setup--sending-form without)))
    ;; and what is left still reads
    (with-temp-buffer
      (insert (vm-setup--sending-form without))
      (goto-char (point-min))
      (should (equal 'setq (car (read (current-buffer))))))))

;;; Suggesting the check, never running it (emacs-vm/vm#816)
;;
;; The maintainer's decision: VM may say that the check would have something
;; to report, and may not report it.  So what is tested is that a line is
;; said once, that it names the commands, and that nothing else happens --
;; no buffer, and nothing at all where there is nothing to say.

(defmacro vm-configuration-test--recording-warnings (var &rest body)
  "Run BODY with VAR bound to a list that `vm-warn' pushes its text onto."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'vm-warn)
                (lambda (_level _secs &rest args)
                  (push (apply #'format args) ,var))))
       ,@body)))

(ert-deftest vm-configuration-test-the-suggestion-is-made-once ()
  "One line per session, not one per folder visited."
  (vm-configuration-test--recording-warnings said
    (vm-configuration-test--with-a-working-setup
      (let ((mail-user-agent 'message-user-agent)
            (vm-suggest-checking-configuration t)
            (vm-suggested-checking-configuration nil))
        (vm-suggest-checking-configuration-maybe)
        (vm-suggest-checking-configuration-maybe)
        (vm-suggest-checking-configuration-maybe)
        (should (= 1 (length said)))
        (should (string-match-p "vm-check-configuration" (car said)))
        (should (string-match-p "vm-setup" (car said)))
        ;; and it says how to stop it
        (should (string-match-p "vm-suggest-checking-configuration"
                                (car said)))))))

(ert-deftest vm-configuration-test-the-suggestion-counts-in-words ()
  "One problem reads as one, several as several."
  (vm-configuration-test--recording-warnings said
    (vm-configuration-test--with-a-working-setup
      (let ((mail-user-agent 'message-user-agent)
            (vm-suggest-checking-configuration t)
            (vm-suggested-checking-configuration nil))
        (vm-suggest-checking-configuration-maybe)
        (should (string-match-p "1 thing is not set up" (car said))))))
  (vm-configuration-test--recording-warnings said
    (vm-configuration-test--with-a-working-setup
      (let ((mail-user-agent 'message-user-agent)
            (send-mail-function nil)
            (vm-suggest-checking-configuration t)
            (vm-suggested-checking-configuration nil))
        (vm-suggest-checking-configuration-maybe)
        (should (string-match-p "2 things are not set up" (car said)))))))

(ert-deftest vm-configuration-test-nothing-is-said-on-a-working-setup ()
  "REGRESSION: silence where the settings VM checks are in place.
The suggestion is for someone who has not finished; anyone who has must
never see it, or it becomes a line to learn to ignore."
  (vm-configuration-test--recording-warnings said
    (vm-configuration-test--with-a-working-setup
      (let ((vm-suggest-checking-configuration t)
            (vm-suggested-checking-configuration nil))
        (vm-suggest-checking-configuration-maybe)
        (should-not said)))))

(ert-deftest vm-configuration-test-the-suggestion-can-be-turned-off ()
  "Nil says nothing at all."
  (vm-configuration-test--recording-warnings said
    (vm-configuration-test--with-a-working-setup
      (let ((mail-user-agent 'message-user-agent)
            (vm-suggest-checking-configuration nil)
            (vm-suggested-checking-configuration nil))
        (vm-suggest-checking-configuration-maybe)
        (should-not said)))))

(ert-deftest vm-configuration-test-the-suggestion-shows-no-buffer ()
  "REGRESSION: suggesting is not reporting.
Asked for on emacs-vm/vm#816: the check is not to run unasked, so the
suggestion must not be `vm-check-configuration' by another name."
  (let ((buffer (get-buffer "*VM Configuration*")))
    (when buffer (kill-buffer buffer)))
  (vm-configuration-test--recording-warnings _said
    (vm-configuration-test--with-a-working-setup
      (let ((mail-user-agent 'message-user-agent)
            (vm-suggest-checking-configuration t)
            (vm-suggested-checking-configuration nil))
        (vm-suggest-checking-configuration-maybe)
        (should-not (get-buffer "*VM Configuration*"))))))

(provide 'vm-configuration-test)

;;; vm-configuration-test.el ends here
