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
