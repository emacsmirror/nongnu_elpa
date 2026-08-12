;;; vm-pop-mock-test.el --- vm-pop.el against a mock POP3 server -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Issue #554.  These drive vm-pop.el end to end -- `vm-pop-make-session',
;; `vm-pop-check-mail', `vm-pop-move-mail' and everything below them -- against
;; the server in vm-pop-mock.el.  No configuration and no real server: the mock
;; listens on a local port that the operating system picks, so these run
;; wherever the suite runs.
;;
;; The point is the protocol behaviour and, more, the failure behaviour: a
;; download cut off half way, a server that has no UIDL, a DELE that comes back
;; -ERR.  None of that could be reached before, and mail integrity is exactly
;; what it decides.

;;; Code:

(require 'cl-lib)
(require 'vm-test-init)
(require 'vm-pop)
(require 'vm-pop-mock)

(defconst vm-pop-mock-test--message-1
  "From: alice@example.com
To: vmtest@example.com
Subject: the first message
Message-ID: <pop-1@example.com>

Body of the first message.
"
  "A whole message, as it sits in a maildrop.")

(defconst vm-pop-mock-test--message-2
  "From: bob@example.com
To: vmtest@example.com
Subject: the second message
Message-ID: <pop-2@example.com>

Body of the second message.
"
  "A second one, so that tests can tell \"stopped early\" from \"finished\".")

(defmacro vm-pop-mock-test--retrieving (spec &rest body)
  "Run BODY with a mock POP server and a destination file to retrieve into.
SPEC is (MOCK-VAR DEST-VAR &rest ARGS); ARGS go to `vm-pop-mock-start'.  The
POP variables are bound so that nothing prompts and nothing is remembered
between tests."
  (declare (indent 1) (debug t))
  `(let ((,(cadr spec) (make-temp-file "vm-pop-mock-dest")))
     (unwind-protect
	 (vm-pop-mock-with (,(car spec) ,@(cddr spec))
	   (let ((vm-pop-server-timeout 10)
		 (vm-pop-ok-to-ask nil)
		 (vm-pop-expunge-after-retrieving t)
		 (vm-pop-retrieved-messages nil)
		 (vm-pop-auto-expunge-alist nil)
		 (vm-pop-max-message-size nil)
		 (vm-pop-messages-per-session nil)
		 (vm-pop-bytes-per-session nil)
		 (vm-folder-type 'From_))
	     ,@body))
       (when (file-exists-p ,(cadr spec)) (delete-file ,(cadr spec))))))

(defun vm-pop-mock-test--contents (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; Sessions and authentication

(ert-deftest vm-pop-mock-test-session-opens ()
  "A session can be opened, and it authenticates with USER and PASS."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1))
    (let* ((vm-pop-server-timeout 10)
	   (process (vm-pop-make-session (vm-pop-mock-spec mock) nil)))
      (should process)
      (should (process-live-p process))
      (vm-pop-end-session process)
      (should (equal '("USER vmtest" "PASS secret")
		     (seq-take (vm-pop-mock-commands mock) 2)))
      (should (vm-pop-mock-received-p mock "\\`QUIT")))))

(ert-deftest vm-pop-mock-test-session-uses-apop ()
  "With apop in the maildrop, VM authenticates with APOP and not with PASS.
The mock checks the digest against the timestamp it greeted with, so this
fails rather than passes if VM sends something that only looks like APOP."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1))
    (let* ((vm-pop-server-timeout 10)
	   (process (vm-pop-make-session (vm-pop-mock-spec mock "apop") nil)))
      (should process)
      (should (process-live-p process))
      (vm-pop-end-session process)
      (should (vm-pop-mock-received-p mock "\\`APOP "))
      (should-not (vm-pop-mock-received-p mock "\\`PASS ")))))

(ert-deftest vm-pop-mock-test-bad-password-is-refused ()
  "A wrong password does not yield a usable session."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1))
    (let* ((vm-pop-server-timeout 10)
	   (vm-pop-ok-to-ask nil)
	   (spec (format "pop:127.0.0.1:%d:pass:vmtest:wrong"
			 (vm-pop-mock-port mock)))
	   (process (condition-case nil (vm-pop-make-session spec nil)
		      (error nil))))
      (when (and process (process-live-p process))
	(vm-pop-end-session process))
      (should (vm-pop-mock-received-p mock "\\`PASS wrong"))
      ;; Whatever it does with the session, it must not have got as far as
      ;; looking at the maildrop.
      (should-not (vm-pop-mock-received-p mock "\\`STAT")))))

;;; Checking for mail

(ert-deftest vm-pop-mock-test-check-mail-sees-messages ()
  "`vm-pop-check-mail' reports mail waiting when there is some."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1))
    (let ((vm-pop-server-timeout 10)
	  (vm-pop-retrieved-messages nil))
      (should (vm-pop-check-mail (vm-pop-mock-spec mock))))))

(ert-deftest vm-pop-mock-test-check-mail-on-empty-maildrop ()
  "An empty maildrop is reported as no mail waiting."
  (vm-pop-mock-with (mock :messages nil)
    (let ((vm-pop-server-timeout 10)
	  (vm-pop-retrieved-messages nil))
      (should-not (vm-pop-check-mail (vm-pop-mock-spec mock))))))

;;; Retrieval

(ert-deftest vm-pop-mock-test-retrieves-the-whole-maildrop ()
  "Both messages arrive in the destination, and both are deleted afterwards."
  (vm-pop-mock-test--retrieving (mock dest
				 :messages (list vm-pop-mock-test--message-1
						 vm-pop-mock-test--message-2))
    (should (vm-pop-move-mail (vm-pop-mock-spec mock) dest))
    (let ((text (vm-pop-mock-test--contents dest)))
      (should (string-match-p "Body of the first message" text))
      (should (string-match-p "Body of the second message" text))
      ;; In order, and each introduced by a From_ separator, since that is the
      ;; folder type these are being appended to.
      (should (< (string-match "pop-1@example.com" text)
		 (string-match "pop-2@example.com" text)))
      (should (= 2 (cl-count-if (lambda (line) (string-prefix-p "From " line))
			       (split-string text "\n")))))
    (should (vm-pop-mock-received-p mock "\\`RETR 1"))
    (should (vm-pop-mock-received-p mock "\\`RETR 2"))
    (should (vm-pop-mock-received-p mock "\\`DELE 1"))
    (should (vm-pop-mock-received-p mock "\\`DELE 2"))
    (should (null (vm-pop-mock-live-messages mock)))))

(ert-deftest vm-pop-mock-test-retrieval-without-uidl ()
  "A server with no UIDL is coped with, and everything still arrives.
VM asks for a unique id first and falls back when the server refuses; that
fallback has never been exercised."
  (vm-pop-mock-test--retrieving (mock dest
				 :no-uidl t
				 :messages (list vm-pop-mock-test--message-1
						 vm-pop-mock-test--message-2))
    (should (vm-pop-move-mail (vm-pop-mock-spec mock) dest))
    (let ((text (vm-pop-mock-test--contents dest)))
      (should (string-match-p "Body of the first message" text))
      (should (string-match-p "Body of the second message" text)))))

(ert-deftest vm-pop-mock-test-truncated-download-stores-nothing ()
  "A download cut off half way leaves no partial message behind.
This is the one that matters: half a message appended to a folder is
corruption, and it would look like a short message rather than an error."
  (vm-pop-mock-test--retrieving (mock dest
				 :truncate-retr t
				 :messages (list vm-pop-mock-test--message-1
						 vm-pop-mock-test--message-2))
    (should-error (vm-pop-move-mail (vm-pop-mock-spec mock) dest))
    (should (equal "" (vm-pop-mock-test--contents dest)))
    ;; And nothing was deleted from the server, so the mail is not lost.
    (should-not (vm-pop-mock-received-p mock "\\`DELE"))
    (should (equal '(1 2) (vm-pop-mock-live-messages mock)))))

(ert-deftest vm-pop-mock-test-dropped-connection-loses-nothing ()
  "A connection that vanishes mid-session is an error, not a silent success."
  (vm-pop-mock-test--retrieving (mock dest
				 :drop-on "\\`STAT"
				 :messages (list vm-pop-mock-test--message-1))
    (let ((result (condition-case nil (vm-pop-move-mail
				       (vm-pop-mock-spec mock) dest)
		    (error 'signalled))))
      (should (memq result '(nil signalled))))
    (should (equal "" (vm-pop-mock-test--contents dest)))
    (should (equal '(1) (vm-pop-mock-live-messages mock)))))

(ert-deftest vm-pop-mock-test-refused-delete-keeps-the-message ()
  "A refused DELE does not lose the message it could not delete.
The message is still on the server, so a later session will fetch it: what
must not happen is VM treating it as gone."
  (vm-pop-mock-test--retrieving (mock dest
				 :refuse "\\`DELE"
				 :messages (list vm-pop-mock-test--message-1
						 vm-pop-mock-test--message-2))
    (vm-pop-move-mail (vm-pop-mock-spec mock) dest)
    (should (vm-pop-mock-received-p mock "\\`DELE 1"))
    ;; Nothing was actually deleted, whatever VM was told.
    (should (equal '(1 2) (vm-pop-mock-live-messages mock)))))

(ert-deftest vm-pop-mock-test-refused-delete-is-reported ()
  "REGRESSION: a refused DELE is reported rather than passed over in silence.
Issue #555.  DELE failing makes `vm-pop-move-mail' abandon the rest of the
maildrop and return success -- message 2 here is never even fetched -- and it
used to do that with nothing said, under a comment reading \"DELE can't
fail\".  Nothing is lost, since the message is still on the server, but a fetch
that stops after one of two messages and reports success has to say why."
  (vm-pop-mock-test--retrieving (mock dest
				 :refuse "\\`DELE"
				 :messages (list vm-pop-mock-test--message-1
						 vm-pop-mock-test--message-2))
    (let ((warnings nil))
      (cl-letf (((symbol-function 'vm-warn)
		 (lambda (_level _delay format &rest args)
		   (push (apply #'format format args) warnings))))
	(vm-pop-move-mail (vm-pop-mock-spec mock) dest))
      (should (cl-find-if (lambda (w) (string-match-p "DELE 1 failed" w))
			  warnings))
      ;; And the thing the warning is about: it did stop early.
      (should-not (vm-pop-mock-received-p mock "\\`RETR 2")))))

(ert-deftest vm-pop-mock-test-wrong-octet-count-is-survivable ()
  "A server that reports the wrong size still gets its message stored whole.
A POP body ends at a dot on a line of its own; the octet count in LIST is for
the progress report and the size threshold, so getting it wrong must not
truncate anything."
  (vm-pop-mock-test--retrieving (mock dest
				 :lie-about-size t
				 :messages (list vm-pop-mock-test--message-1))
    (should (vm-pop-move-mail (vm-pop-mock-spec mock) dest))
    (let ((text (vm-pop-mock-test--contents dest)))
      (should (string-match-p "Body of the first message" text))
      (should (string-match-p "Message-ID: <pop-1@example.com>" text)))))

(ert-deftest vm-pop-mock-test-too-large-message-is-left-alone ()
  "A message over `vm-pop-max-message-size' is neither retrieved nor deleted."
  (vm-pop-mock-test--retrieving (mock dest
				 :messages (list vm-pop-mock-test--message-1))
    (let ((vm-pop-max-message-size 10))
      (vm-pop-move-mail (vm-pop-mock-spec mock) dest))
    (should-not (vm-pop-mock-received-p mock "\\`RETR"))
    (should (equal "" (vm-pop-mock-test--contents dest)))
    (should (equal '(1) (vm-pop-mock-live-messages mock)))))

;;; The mock itself
;;
;; It is a server the tests trust, so its own behaviour is worth pinning.

(defun vm-pop-mock-test--complete-response-p (text multiline)
  "Whether TEXT is a whole POP3 response, MULTILINE saying which kind.
A single-line response ends at its first CRLF.  A multi-line one -- what UIDL,
LIST and RETR answer when they succeed -- ends at a dot on a line of its own;
an error reply to the same command is a single line, so the status decides."
  (let ((first (and (string-match "\\`\\([^\r\n]*\\)\r\n" text)
                    (match-string 1 text))))
    (and first
         (if (and multiline (string-prefix-p "+OK" first))
             (string-suffix-p "\r\n.\r\n" text)
           t))))

(ert-deftest vm-pop-mock-test-mock-serves-a-plain-conversation ()
  "The mock speaks POP3 to a client that is not VM.
Keeps the tests above honest: if the mock stopped answering STAT or dot-stuffing
its bodies, they would all still pass by agreeing with a broken server.

Each response is read to its end before it is matched, and only then is the
next command sent.  Matching a pattern as soon as it appeared left the rest of
the response unread, so it arrived during the next command and the next
pattern -- three of them anchored at the start of what had arrived -- was
matched against the wrong reply.  That never fired on an idle machine, where a
small response arrives in one piece (emacs-vm/vm#626)."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1
					  vm-pop-mock-test--message-2))
    (let ((process (make-network-process
		    :name "vm-pop-mock-test-client"
		    :host "127.0.0.1" :service (vm-pop-mock-port mock)
		    :coding 'binary :noquery t))
	  (received ""))
      (unwind-protect
	  (progn
	    (set-process-filter process
				(lambda (_p text) (setq received
							(concat received text))))
	    (cl-flet* ((await
			 (done what)
			 ;; Wait for DONE, a predicate on what has arrived.
			 ;; The deadline is a watchdog, not a measurement: the
			 ;; test asserts nothing about how long a local socket
			 ;; takes, and gives up early the moment the
			 ;; connection dies, since nothing more is coming
			 ;; then.  A failure says what the server thought it
			 ;; was doing -- what it received, and any error it
			 ;; hit answering, which used to be lost to the
			 ;; messages buffer.
			 (let ((deadline (+ 30 (float-time))))
			   (while (and (not (funcall done))
				       (process-live-p process)
				       (< (float-time) deadline))
			     (accept-process-output process 0 100)))
			 (unless (funcall done)
			   (ert-fail
			    (list what
				  :received received
				  :connection (process-status process)
				  :server-saw (vm-pop-mock-commands mock)
				  :server-errors (vm-pop-mock-errors mock)))))
		       (converse
			 (command pattern &optional multiline)
			 (setq received "")
			 (process-send-string process (concat command "\r\n"))
			 (await (lambda ()
				  (vm-pop-mock-test--complete-response-p
				   received multiline))
				(format "no complete response to %s" command))
			 (should (string-match-p pattern received))))
	      ;; The greeting arrives unprompted.
	      (await (lambda ()
		       (vm-pop-mock-test--complete-response-p received nil))
		     "no greeting")
	      (should (string-prefix-p "+OK" received))
	      (converse "USER vmtest" "\\`\\+OK")
	      (converse "PASS secret" "\\`\\+OK")
	      (converse "STAT" "\\`\\+OK 2 ")
	      (converse "UIDL" "uid1" t)
	      (converse "RETR 1" "Body of the first message" t)
	      ;; The body is terminated by a dot on a line of its own.
	      (converse "RETR 2" "\r\n\\.\r\n\\'" t)
	      (converse "DELE 1" "\\`\\+OK")
	      (converse "STAT" "\\`\\+OK 1 ")
	      (converse "RSET" "\\`\\+OK")
	      (converse "STAT" "\\`\\+OK 2 ")))
	(when (process-live-p process) (delete-process process))))))

(ert-deftest vm-pop-mock-test-mock-doubles-a-leading-dot ()
  "A line of the message that begins with a dot is sent doubled.
Without that the message would end early at its own text, which is the classic
POP3 mistake in both directions."
  (vm-pop-mock-test--retrieving (mock dest
				 :messages (list (concat
						  "From: alice@example.com\n"
						  "Subject: dotted\n\n"
						  "before\n"
						  ".hidden line\n"
						  "after\n")))
    (should (vm-pop-move-mail (vm-pop-mock-spec mock) dest))
    (let ((text (vm-pop-mock-test--contents dest)))
      ;; The whole message arrived, dot line included and undoubled again.
      (should (string-match-p "^\\.hidden line$" text))
      (should (string-match-p "^after$" text)))))

(provide 'vm-pop-mock-test)

;;; vm-pop-mock-test.el ends here
