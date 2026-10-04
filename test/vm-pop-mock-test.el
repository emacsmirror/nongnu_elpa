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
		 (vm-folder-type 'From_))
	     ,@body))
       (when (file-exists-p ,(cadr spec)) (delete-file ,(cadr spec))))))

(defun vm-pop-mock-test--contents (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; Sessions and authentication

;;; Checking for mail

;;; Retrieval

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

(defun vm-pop-mock-test--connect (mock)
  "Open a client connection to MOCK and return the process.
The filter goes in through `make-network-process' rather than being set
afterwards.  A process with neither a filter nor a buffer discards what
arrives, and the greeting is written by the server the moment it accepts the
connection -- so anything that let the event loop run between creating the
process and setting its filter dropped the greeting for good, and the test
then waited out its deadline for a line that no longer existed.  That is what
emacs-vm/vm#626 was.

What has arrived is kept on the process, readable with
`vm-pop-mock-test--received'."
  (make-network-process
   :name "vm-pop-mock-test-client"
   :host "127.0.0.1" :service (vm-pop-mock-port mock)
   :coding 'binary :noquery t
   :filter (lambda (process text)
	     (process-put process 'vm-pop-mock-test-received
			  (concat (process-get process
					       'vm-pop-mock-test-received)
				  text)))))

(defun vm-pop-mock-test--received (process)
  "Everything PROCESS has been sent since it was last forgotten."
  (or (process-get process 'vm-pop-mock-test-received) ""))

(defun vm-pop-mock-test--forget (process)
  "Forget what PROCESS has been sent, before sending the next command."
  (process-put process 'vm-pop-mock-test-received ""))

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
    (let ((process (vm-pop-mock-test--connect mock)))
      (unwind-protect
	  (progn
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
				  :received (vm-pop-mock-test--received process)
				  :connection (process-status process)
				  :server-saw (vm-pop-mock-commands mock)
				  :server-errors (vm-pop-mock-errors mock)))))
		       (converse
			 (command pattern &optional multiline)
			 (vm-pop-mock-test--forget process)
			 (process-send-string process (concat command "\r\n"))
			 (await (lambda ()
				  (vm-pop-mock-test--complete-response-p
				   (vm-pop-mock-test--received process)
				   multiline))
				(format "no complete response to %s" command))
			 (should (string-match-p
				  pattern
				  (vm-pop-mock-test--received process)))))
	      ;; The greeting arrives unprompted.
	      (await (lambda ()
		       (vm-pop-mock-test--complete-response-p
			(vm-pop-mock-test--received process) nil))
		     "no greeting")
	      (should (string-prefix-p "+OK"
				       (vm-pop-mock-test--received process)))
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

(ert-deftest vm-pop-mock-test-a-client-without-a-filter-loses-what-arrives ()
  "The trap behind issue #626, demonstrated on two clients side by side.

A process with neither a filter nor a buffer discards what it is sent.  The
mock writes its greeting the moment it accepts a connection, so a client
created bare and given its filter afterwards loses that greeting to anything
that lets the event loop run in between -- and there is then nothing to wait
for but the deadline.  A client whose filter goes in through
`make-network-process' has no such window, which is why
`vm-pop-mock-test--connect' does it that way.

This does not guard the helper: the window it closes is a race, and a test
cannot force the event loop inside a helper it is calling.  The conversation
test is the regression -- it is the one that failed about once in ten full
runs.  This pins the behaviour that explains it, and would notice if Emacs
ever started buffering for a filterless process."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1))
    ;; bare: no filter, no buffer, and the event loop runs before the filter
    ;; is installed
    (let ((bare (make-network-process
                 :name "vm-pop-mock-test-bare" :host "127.0.0.1"
                 :service (vm-pop-mock-port mock)
                 :coding 'binary :noquery t)))
      (unwind-protect
          (progn
            (accept-process-output nil 0.1)
            (set-process-filter
             bare (lambda (process text)
                    (process-put process 'vm-pop-mock-test-received
                                 (concat (process-get
                                          process 'vm-pop-mock-test-received)
                                         text))))
            (accept-process-output bare 0 100)
            (should (equal (vm-pop-mock-test--received bare) "")))
        (when (process-live-p bare) (delete-process bare))))
    ;; and the way the tests connect: the greeting is there whenever it is
    ;; delivered, because the filter was in place before the connection was
    (let ((process (vm-pop-mock-test--connect mock)))
      (unwind-protect
          (progn
            (accept-process-output nil 0.1)
            (let ((deadline (+ 30 (float-time))))
              (while (and (string= (vm-pop-mock-test--received process) "")
                          (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output process 0 50)))
            (should (string-prefix-p "+OK"
                                     (vm-pop-mock-test--received process))))
        (when (process-live-p process) (delete-process process))))))

;;; A server that goes quiet (emacs-vm/vm#639)

;;; Visiting a POP folder (emacs-vm/vm#632)
;;
;; `vm-visit-pop-folder' opens a maildrop as a folder rather than fetching
;; from it into one, which is the other half of VM's POP support and had no
;; test.  The mock serves it.

(defmacro vm-pop-mock-test--visiting (spec &rest body)
  "Visit a mock POP maildrop as a folder and run BODY in it.
SPEC is (MOCK-VAR &rest ARGS), ARGS going to `vm-pop-mock-start'.  The name in
`vm-pop-folder-alist' is \"mockdrop\"."
  (declare (indent 1) (debug t))
  `(vm-pop-mock-with (,(car spec) ,@(cdr spec))
     (let* ((cache (make-temp-file "vm-pop-visit-cache" t))
            (vm-pop-folder-alist
             (list (list (vm-pop-mock-spec ,(car spec)) "mockdrop")))
            (vm-pop-folder-cache-directory cache)
            (vm-pop-server-timeout 10)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (before (buffer-list)))
       (unwind-protect
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-pop-folder "mockdrop")
             ;; visiting starts the fetch and returns without waiting for it,
             ;; so what waits for the mail is whoever wants the mail
             (vm-pop-net-wait nil 10)
             ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory cache t)))))

(ert-deftest vm-pop-mock-test-visiting-a-maildrop-as-a-folder ()
  "`vm-visit-pop-folder' opens the maildrop named in `vm-pop-folder-alist'
and the messages arrive whole.

The folder knows it is POP afterwards, which is what tells the rest of VM to
talk to the server rather than to a file."
  (vm-pop-mock-test--visiting
      (mock :messages (list vm-pop-mock-test--message-1
                            vm-pop-mock-test--message-2))
    (should (equal (length vm-message-list) 2))
    (should (eq vm-folder-access-method 'pop))
    (should (string-match-p
             "Body of the first message"
             (with-current-buffer (vm-buffer-of (car vm-message-list))
               (save-restriction
                 ;; the folder buffer is narrowed to the message on show
                 (widen)
                 (buffer-substring (vm-text-of (car vm-message-list))
                                   (vm-text-end-of (car vm-message-list)))))))
    ;; it asked for the messages by number after listing them
    (should (vm-pop-mock-received-p mock "\\`RETR 1"))
    (should (vm-pop-mock-received-p mock "\\`RETR 2"))))

(ert-deftest vm-pop-mock-test-visiting-an-unknown-maildrop ()
  "A name that is in no `vm-pop-folder-alist' entry is refused, and the name
is in the message: it is the thing the user got wrong."
  (vm-pop-mock-with (mock :messages (list vm-pop-mock-test--message-1))
    (let ((vm-pop-folder-alist nil)
          (text-quoting-style 'grave))
      (should (equal (cadr (should-error (vm-visit-pop-folder "nowhere")))
                     "No such POP folder: nowhere")))))

(ert-deftest vm-pop-mock-test-visiting-leaves-the-mail-on-the-server ()
  "Visiting is not fetching: the maildrop still holds its messages
afterwards, since the folder is a view of the server rather than a copy."
  (vm-pop-mock-test--visiting
      (mock :messages (list vm-pop-mock-test--message-1
                            vm-pop-mock-test--message-2))
    (should (equal (length vm-message-list) 2))
    (should (equal (vm-pop-mock-live-messages mock) '(1 2)))
    (should-not (vm-pop-mock-received-p mock "\\`DELE"))))


(ert-deftest vm-pop-mock-test-a-folder-check-does-not-wait ()
  "The check on a POP folder starts and returns, as the one on a maildrop
does.  It was the last check still opening a session with Emacs stopped, and
`vm-spooled-mail-waiting' -- what the mode line reads -- is set when the
answer arrives rather than before the question is asked."
  (vm-pop-mock-test--visiting
      (mock :messages (list vm-pop-mock-test--message-1))
    (setq vm-spooled-mail-waiting nil)
    (let ((started (float-time)))
      (should (vm-check-for-spooled-mail nil t))
      (should (< (- (float-time) started) 0.5)))
    (should (vm-pop-net-wait nil 20))
    ;; nothing new: the folder holds what the maildrop holds
    (should-not vm-spooled-mail-waiting)))

(defmacro vm-pop-mock-test--with-a-local-expunge (mock &rest body)
  "Run BODY in a visited mock folder that has expunged \"uid1\" locally.
That leaves the UIDL queued for the maildrop and recorded as one this folder
has had, which is what `vm-expunge-queue-pop-deletion' and
`vm-expunge-record-pop-uidl' do, and it is the state a save then owes the
server."
  (declare (indent 1) (debug t))
  `(let* ((spec (vm-popdrop-sans-password (vm-pop-mock-spec ,mock)))
          (retrieved (list (list "uid1" spec 'uidl))))
     (setq vm-pop-retrieved-messages (copy-tree retrieved))
     (setq vm-pop-messages-to-expunge (list "uid1"))
     ,@body))

;;; Arriving mail whose body looks like a folder separator

;; Everything above retrieves messages whose bodies are ordinary prose.  What
;; a folder cannot survive is a body the folder type reads as a separator: a
;; `From ' line going into a From_ folder, \001\001\001\001 into an mmdf one.
;; Retrieval is the path every received message takes, so it is the one that
;; must quote them, and nothing checked that it does.

(defconst vm-pop-mock-test--separator-bodies
  '(("plain"              . "an ordinary body line.")
    ("a From_ line"       . "text\nFrom nobody@example.com Mon Jan  1 00:00:00 2024")
    ("a From_ line first" . "From nobody@example.com Mon Jan  1 00:00:00 2024\nrest")
    ("an mmdf separator"  . "text\n\001\001\001\001\nmore")
    ("a babyl separator"  . "text\n\037\014\nmore")
    ("8-bit"              . "Gr\303\274\303\237e"))
  "Bodies that a folder of some type would otherwise read as a separator.")

(defun vm-pop-mock-test--message-with (n body)
  "A message numbered N carrying BODY."
  (concat "From: alice@example.com\nTo: vmtest@example.com\n"
          (format "Subject: message %d\nMessage-ID: <pop-%d@example.com>\n\n" n n)
          body "\n"))

(defun vm-pop-mock-test--messages-in (file type)
  "Read FILE as a folder of TYPE; answer (TYPE-READ . COUNT).
`vm-build-message-list' asks `vm-get-folder-type' rather than trusting the
caller, so the buffer is given a name that states the type, as a folder VM
visits has."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert-file-contents file)
    (setq-local buffer-file-name (vm-folder-name-for-type file type))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (vm-build-message-list)
    (cons vm-folder-type (length vm-message-list))))

(defun vm-pop-mock-test--retrieve-into (type body)
  "Retrieve two messages, the first carrying BODY, into a folder of TYPE.
Answers a complaint, or nil when the folder reads back as the two messages
that were sent."
  (let ((dest (make-temp-file "vm-pop-mock-dest")))
    (unwind-protect
        (condition-case err
            (vm-pop-mock-with (mock :messages
                                    (list (vm-pop-mock-test--message-with 1 body)
                                          (vm-pop-mock-test--message-with 2 "second body")))
              (let ((vm-pop-server-timeout 10)
                    (vm-pop-ok-to-ask nil)
                    (vm-pop-expunge-after-retrieving t)
                    (vm-pop-retrieved-messages nil)
                    (vm-pop-auto-expunge-alist nil)
                    (vm-pop-max-message-size nil)
                    (vm-folder-type type))
                (vm-pop-move-mail (vm-pop-mock-spec mock) dest)
                (let ((read (vm-pop-mock-test--messages-in dest type)))
                  (cond ((not (eq (car read) type))
                         (format "%s: read back as %s" type (car read)))
                        ((/= 2 (cdr read))
                         (format "%s: %d messages, not 2" type (cdr read)))
                        (t nil)))))
          (error (format "%s: %s" type (error-message-string err))))
      (when (file-exists-p dest) (delete-file dest)))))

(provide 'vm-pop-mock-test)

;;; vm-pop-mock-test.el ends here
