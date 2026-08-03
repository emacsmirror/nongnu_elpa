;;; vm-pop-live-test.el --- Live POP tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Issue #554, the POP half.  vm-pop.el against a real Dovecot, seeded over
;; IMAP because POP3 cannot put a message into a maildrop.  See
;; vm-pop-live-init.el for the harness and
;; dev/docs/design/imap-live-tests.org for how to configure a server.
;;
;; Skipped unless test/vm-imap-config.el sets `vm-pop-test-servers'.  The mock
;; tests in vm-pop-mock-test.el cover the protocol and the failure paths
;; without any server; what these add is the part a mock cannot vouch for --
;; that a real server, with its own idea of octet counts, line endings and
;; UIDLs, is understood.

;;; Code:

(require 'cl-lib)
(require 'vm-test-init)
(require 'vm-pop-live-init)
(require 'vm-pop)

(defmacro vm-pop-live-test--with-fixture (spec &rest body)
  "Seed the INBOX with one fixture message, run BODY, then clean up.
SPEC is (ID-VAR SERVER-VAR ACCOUNT-VAR), bound to the fixture id, the POP
server plist and the account.  BODY may also use IMAP-SERVER.  The fixture is
removed over IMAP afterwards even if BODY fails, since a maildrop is shared
state rather than a throwaway mailbox."
  (declare (indent 1) (debug t))
  `(let* ((imap-server (vm-imap-live-server "plain"))
	  (,(nth 1 spec) (vm-pop-live-server "pop"))
	  (,(nth 2 spec) (car (plist-get ,(nth 1 spec) :accounts)))
	  (,(car spec) (vm-pop-live-fixture-id)))
     (unwind-protect
	 (progn
	   (vm-pop-live-seed-inbox imap-server ,(nth 2 spec)
				   (vm-pop-live-message ,(car spec)))
	   ,@body)
       (ignore-errors
	 (vm-pop-live-purge-inbox imap-server ,(nth 2 spec) ,(car spec))))))

;;; The harness itself

(ert-deftest vm-pop-live-test-greeting-and-login ()
  "The server greets us and accepts the configured account."
  (vm-pop-live-skip-unless-server "pop" "plain")
  (let* ((server (vm-pop-live-server "pop"))
	 (account (car (plist-get server :accounts))))
    (vm-pop-live-with-conn (conn server account)
      ;; STAT is the first thing that needs an authenticated session.
      (should (integerp (car (vm-pop-live-stat conn)))))))

(ert-deftest vm-pop-live-test-bad-password-is-refused ()
  "A wrong password is refused rather than accepted.
Dovecot's auth_failure_delay, 2s by default, is charged to this test."
  (vm-pop-live-skip-unless-server "pop" "plain")
  (let* ((server (vm-pop-live-server "pop"))
	 (user (car (car (plist-get server :accounts))))
	 (conn (vm-pop-live--open server)))
    (unwind-protect
	(progn
	  (vm-pop-live-cmd-ok conn "USER %s" user)
	  (should (string-prefix-p "-ERR"
				   (vm-pop-live-cmd conn "PASS definitely-wrong"))))
      (vm-pop-live-close conn))))

(ert-deftest vm-pop-live-test-fixture-round-trip ()
  "A message put in over IMAP is visible over POP, and is the one we sent.
Also the harness's own check: if seeding or purging were broken, everything
below would be testing an empty maildrop."
  (vm-pop-live-skip-unless-server "pop" "plain")
  (vm-pop-live-test--with-fixture (id server account)
    (vm-pop-live-with-conn (conn server account)
      (let* ((count (car (vm-pop-live-stat conn)))
	     (uidls (vm-pop-live-multiline conn "UIDL"))
	     (found nil))
	(should (> count 0))
	(should (= count (length uidls)))
	;; Find our own message rather than assuming the maildrop holds only
	;; it: this is a real INBOX.
	(dolist (n (number-sequence 1 count))
	  (let ((lines (vm-pop-live-multiline conn "TOP %d 0" n)))
	    (when (cl-find-if (lambda (line)
				(string-match-p (regexp-quote id) line))
			      lines)
	      (setq found n))))
	(should found)))))

;;; VM retrieving mail

(ert-deftest vm-pop-live-test-retrieves-and-deletes ()
  "VM retrieves the fixture into a folder and removes it from the maildrop.
The end-to-end case: `vm-pop-move-mail' against a server VM did not write."
  (vm-pop-live-skip-unless-server "pop" "plain")
  (vm-pop-live-test--with-fixture (id server account)
    (let ((dest (make-temp-file "vm-pop-live-dest")))
      (unwind-protect
	  (let ((vm-pop-server-timeout vm-pop-live-timeout)
		(vm-pop-ok-to-ask nil)
		(vm-pop-expunge-after-retrieving t)
		(vm-pop-retrieved-messages nil)
		(vm-pop-auto-expunge-alist nil)
		(vm-pop-max-message-size nil)
		(vm-pop-messages-per-session nil)
		(vm-pop-bytes-per-session nil)
		(vm-folder-type 'From_))
	    (should (vm-pop-move-mail (vm-pop-live-spec server account) dest))
	    (with-temp-buffer
	      (insert-file-contents dest)
	      (should (string-match-p (regexp-quote id) (buffer-string)))
	      (should (string-match-p (format "Body of %s" id)
				      (buffer-string)))
	      ;; A From_ separator per message, so the folder is well formed.
	      (should (string-prefix-p "From " (buffer-string))))
	    ;; And it is gone from the maildrop, not merely marked.
	    (vm-pop-live-with-conn (conn server account)
	      (let ((count (car (vm-pop-live-stat conn))))
		(dolist (n (number-sequence 1 count))
		  (let ((lines (vm-pop-live-multiline conn "TOP %d 0" n)))
		    (should-not
		     (cl-find-if (lambda (line)
				   (string-match-p (regexp-quote id) line))
				 lines)))))))
	(when (file-exists-p dest) (delete-file dest))))))

(ert-deftest vm-pop-live-test-leaves-mail-when-not-expunging ()
  "Without auto-expunge the message is retrieved and left on the server.
That is the arrangement a user keeps when POP is not their only client, and
the message must then still be there afterwards."
  (vm-pop-live-skip-unless-server "pop" "plain")
  (vm-pop-live-test--with-fixture (id server account)
    (let ((dest (make-temp-file "vm-pop-live-dest")))
      (unwind-protect
	  (let ((vm-pop-server-timeout vm-pop-live-timeout)
		(vm-pop-ok-to-ask nil)
		(vm-pop-expunge-after-retrieving nil)
		(vm-pop-auto-expunge-alist nil)
		(vm-pop-auto-expunge-warned nil)
		(vm-pop-retrieved-messages nil)
		(vm-pop-max-message-size nil)
		(vm-pop-messages-per-session nil)
		(vm-pop-bytes-per-session nil)
		(vm-folder-type 'From_))
	    (should (vm-pop-move-mail (vm-pop-live-spec server account) dest))
	    (with-temp-buffer
	      (insert-file-contents dest)
	      (should (string-match-p (regexp-quote id) (buffer-string))))
	    ;; Still on the server, and remembered as retrieved so that the
	    ;; next session does not fetch it again.
	    (should (cl-find-if
		     (lambda (entry)
		       (and (consp entry)
			    (cl-find-if
			     (lambda (part)
			       (and (stringp part)
				    (string-match-p "pop:" part)))
			     entry)))
		     vm-pop-retrieved-messages))
	    (vm-pop-live-with-conn (conn server account)
	      (let ((count (car (vm-pop-live-stat conn)))
		    (found nil))
		(dolist (n (number-sequence 1 count))
		  (let ((lines (vm-pop-live-multiline conn "TOP %d 0" n)))
		    (when (cl-find-if (lambda (line)
					(string-match-p (regexp-quote id) line))
				      lines)
		      (setq found n))))
		(should found))))
	(when (file-exists-p dest) (delete-file dest))))))

(ert-deftest vm-pop-live-test-check-mail-sees-the-fixture ()
  "`vm-pop-check-mail' reports mail waiting for a maildrop that has some."
  (vm-pop-live-skip-unless-server "pop" "plain")
  (vm-pop-live-test--with-fixture (id server account)
    (ignore id)
    (let ((vm-pop-server-timeout vm-pop-live-timeout)
	  (vm-pop-retrieved-messages nil))
      (should (vm-pop-check-mail (vm-pop-live-spec server account))))))

;;; TLS

(ert-deftest vm-pop-live-test-tls-round-trip ()
  "POP3S works: implicit TLS on 995, and a fixture read back over it.
VM has no STARTTLS for POP either, so pop-ssl is the only encrypted form."
  (vm-pop-live-skip-unless-server "pop-tls" "plain")
  (let* ((server (vm-pop-live-server "pop-tls"))
	 (account (car (plist-get server :accounts)))
	 (imap-server (vm-imap-live-server "plain"))
	 (id (vm-pop-live-fixture-id)))
    (unwind-protect
	(progn
	  (vm-pop-live-seed-inbox imap-server account
				  (vm-pop-live-message id))
	  (should (string-prefix-p "pop-ssl:"
				   (vm-pop-live-spec server account)))
	  (vm-pop-live-with-conn (conn server account)
	    (let ((count (car (vm-pop-live-stat conn)))
		  (found nil))
	      (dolist (n (number-sequence 1 count))
		(let ((lines (vm-pop-live-multiline conn "TOP %d 0" n)))
		  (when (cl-find-if (lambda (line)
				      (string-match-p (regexp-quote id) line))
				    lines)
		    (setq found n))))
	      (should found))))
      (ignore-errors (vm-pop-live-purge-inbox imap-server account id)))))

(provide 'vm-pop-live-test)

;;; vm-pop-live-test.el ends here
