;;; vm-pop-live-init.el --- Harness for live POP tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Issue #554, the POP half.  Support for testing vm-pop.el against a real POP3
;; server -- the same Dovecot the live IMAP tests use, with pop3 added to its
;; protocols.  See dev/docs/design/imap-live-tests.org.
;;
;; The opt-in is the same gitignored test/vm-live-config.el, which sets
;; `vm-pop-test-servers' alongside `vm-imap-test-servers'.  Without POP entries
;; every test here skips.
;;
;; Seeding is the awkward part, and the reason these tests want an IMAP server
;; too.  POP3 has no APPEND: a client cannot put a message into a maildrop, it
;; can only take messages out.  So a fixture is created over IMAP, into the
;; INBOX of the same account, and then read over POP.  The two protocols have
;; to be served for the same user, which Dovecot does without being asked
;; twice.
;;
;; That makes the INBOX shared state, which the tests are careful about: every
;; fixture message carries an X-VMTest-Id header naming the test run, and
;; anything left over is removed over IMAP afterwards even if the test failed.
;; A maildrop is not a throwaway mailbox, so nothing here may assume it starts
;; empty either -- the assertions are about the messages the test itself put
;; there.
;;
;; As with the IMAP harness, verification uses its own minimal POP client
;; rather than vm-pop.el: a fixture checked with the code under test can hide
;; that code's own bugs.

;;; Code:

(require 'vm-test-init)
(require 'cl-lib)
(require 'vm-imap-live-init)

;; As in the IMAP harness: load these before anything binds their variables,
;; or a `let' under lexical binding creates a lexical binding and the
;; library's own defvar then fails.
(require 'nsm)
(require 'gnutls nil t)
(defvar network-security-level)
(defvar gnutls-trustfiles)

;;; ------------------------------------------------------------------
;;; Opt-in
;;; ------------------------------------------------------------------

(defvar vm-pop-live-enabled (vm-test-live-wanted-p)
  "Whether the live POP tests may run at all.
As with `vm-imap-live-enabled', the config file is the real opt-in; bind this
to nil to keep a configured checkout off the network, and the default answers
VM_TEST_LIVE.")

(defvar vm-pop-test-servers nil
  "List of POP server plists, set by `vm-live-config-file'.
Each is (:name NAME :host HOST :port PORT :tls BOOL :auth AUTH :accounts
ALIST), where AUTH is \"pass\" or \"apop\" and ALIST maps user to password --
the same structure as `vm-imap-test-servers', so one config file describes both.")

(defvar vm-pop-live-timeout 10
  "Seconds any single POP interaction may take.
`vm-pop-server-timeout' defaults to nil, meaning never time out.")

(defun vm-pop-live-available-p ()
  "Return non-nil if the POP harness is enabled and a server is configured."
  (and vm-pop-live-enabled vm-pop-test-servers t))

(defun vm-pop-live-server (name)
  "Return the configured POP server plist called NAME, or nil."
  (seq-find (lambda (s) (equal (plist-get s :name) name))
	    vm-pop-test-servers))

(defmacro vm-pop-live-skip-unless-server (name imap-name)
  "Skip the running test unless POP server NAME and IMAP server IMAP-NAME exist.
Both are needed: the fixture goes in over IMAP, because POP cannot put a
message into a maildrop, and comes back over POP."
  `(progn
     (skip-unless (vm-pop-live-available-p))
     (skip-unless (vm-pop-live-server ,name))
     (skip-unless (vm-imap-live-available-p))
     (skip-unless (vm-imap-live-server ,imap-name))))

(defun vm-pop-live-spec (server account)
  "Return a VM POP maildrop specification for ACCOUNT on SERVER."
  (format "%s:%s:%s:%s:%s:%s"
	  (if (plist-get server :tls) "pop-ssl" "pop")
	  (plist-get server :host)
	  (plist-get server :port)
	  (or (plist-get server :auth) "pass")
	  (car account)
	  (cdr account)))

;;; ------------------------------------------------------------------
;;; Independent minimal POP client
;;; ------------------------------------------------------------------

(cl-defstruct (vm-pop-live-conn (:constructor vm-pop-live--make-conn))
  process buffer)

(defun vm-pop-live--read-line (conn)
  "Return the next line from CONN, without its line ending."
  (with-current-buffer (vm-pop-live-conn-buffer conn)
    (let ((deadline (+ vm-pop-live-timeout (float-time))))
      (while (and (not (save-excursion
			 (goto-char (point-min))
			 (re-search-forward "\r?\n" nil t)))
		  (< (float-time) deadline))
	(accept-process-output (vm-pop-live-conn-process conn) 0 100))
      (goto-char (point-min))
      (unless (re-search-forward "\r?\n" nil t)
	(error "POP server said nothing in %s seconds" vm-pop-live-timeout))
      (let ((line (buffer-substring-no-properties (point-min)
						  (match-beginning 0))))
	(delete-region (point-min) (match-end 0))
	line))))

(defun vm-pop-live--open (server)
  "Open a connection to SERVER and read its greeting.  Return a conn."
  (let* ((tls (plist-get server :tls))
	 (buffer (generate-new-buffer " *vm-pop-live*"))
	 (gnutls-trustfiles (if (plist-get server :trustfile)
				(cons (plist-get server :trustfile)
				      (bound-and-true-p gnutls-trustfiles))
			      (bound-and-true-p gnutls-trustfiles)))
	 (network-security-level (if tls 'low
				   (bound-and-true-p network-security-level)))
	 (process (open-network-stream
		   "vm-pop-live" buffer
		   (plist-get server :host) (plist-get server :port)
		   :type (if tls 'tls 'plain)
		   :nowait nil)))
    (set-process-coding-system process 'binary 'binary)
    (let ((conn (vm-pop-live--make-conn :process process :buffer buffer)))
      (let ((greeting (vm-pop-live--read-line conn)))
	(unless (string-prefix-p "+OK" greeting)
	  (error "POP server refused us: %s" greeting)))
      conn)))

(defun vm-pop-live-close (conn)
  "Send QUIT to CONN and shut it down, ignoring errors."
  (ignore-errors (vm-pop-live-cmd conn "QUIT"))
  (let ((process (vm-pop-live-conn-process conn)))
    (when (process-live-p process) (ignore-errors (delete-process process))))
  (when (buffer-live-p (vm-pop-live-conn-buffer conn))
    (kill-buffer (vm-pop-live-conn-buffer conn))))

(defun vm-pop-live-cmd (conn format &rest args)
  "Send a command to CONN and return its status line."
  (let ((command (apply #'format format args)))
    (process-send-string (vm-pop-live-conn-process conn)
			 (concat command "\r\n"))
    (vm-pop-live--read-line conn)))

(defun vm-pop-live-cmd-ok (conn format &rest args)
  "Send a command to CONN and return its status line, insisting on +OK."
  (let ((line (apply #'vm-pop-live-cmd conn format args)))
    (unless (string-prefix-p "+OK" line)
      (error "POP command failed: %s" line))
    line))

(defun vm-pop-live-multiline (conn format &rest args)
  "Send a command to CONN and return its multi-line body as a list of lines."
  (apply #'vm-pop-live-cmd-ok conn format args)
  (let ((lines nil) (line nil) (done nil))
    (while (not done)
      (setq line (vm-pop-live--read-line conn))
      (if (equal line ".")
	  (setq done t)
	;; Undo dot-stuffing, so callers see the text the sender wrote.
	(push (if (string-prefix-p ".." line) (substring line 1) line) lines)))
    (nreverse lines)))

(defun vm-pop-live-login (conn server account)
  "Authenticate as ACCOUNT on CONN, using SERVER's auth method."
  (vm-pop-live-cmd-ok conn "USER %s" (car account))
  (vm-pop-live-cmd-ok conn "PASS %s" (cdr account))
  (ignore server))

(defun vm-pop-live-stat (conn)
  "Return (COUNT . OCTETS) from STAT on CONN."
  (let ((line (vm-pop-live-cmd-ok conn "STAT")))
    (if (string-match "\\`\\+OK +\\([0-9]+\\) +\\([0-9]+\\)" line)
	(cons (string-to-number (match-string 1 line))
	      (string-to-number (match-string 2 line)))
      (error "Cannot read STAT response: %s" line))))

(defmacro vm-pop-live-with-conn (spec &rest body)
  "Run BODY with an authenticated POP connection bound to CONN-VAR.
SPEC is (CONN-VAR SERVER ACCOUNT)."
  (declare (indent 1) (debug t))
  `(let ((,(car spec) (vm-pop-live--open ,(nth 1 spec))))
     (unwind-protect
	 (progn
	   (vm-pop-live-login ,(car spec) ,(nth 1 spec) ,(nth 2 spec))
	   ,@body)
       (vm-pop-live-close ,(car spec)))))

;;; ------------------------------------------------------------------
;;; Fixtures in the INBOX, put there over IMAP
;;; ------------------------------------------------------------------

(defvar vm-pop-live--id-counter 0
  "Counter making each fixture message's X-VMTest-Id unique in this run.")

(defun vm-pop-live-fixture-id ()
  "Return an id no other fixture message in this run will use."
  (setq vm-pop-live--id-counter (1+ vm-pop-live--id-counter))
  (format "vmtest-pop-%d-%d" (emacs-pid) vm-pop-live--id-counter))

(defun vm-pop-live-message (id &optional body)
  "Return a whole message carrying fixture id ID."
  (concat "From: alice@example.com\r\n"
	  "To: vmtest@example.com\r\n"
	  (format "Subject: live pop test %s\r\n" id)
	  "Date: Mon, 01 Jan 2024 00:00:00 +0000\r\n"
	  (format "Message-ID: <%s@example.com>\r\n" id)
	  (format "X-VMTest-Id: %s\r\n" id)
	  "\r\n"
	  (or body (format "Body of %s.\r\n" id))))

(defun vm-pop-live-seed-inbox (imap-server account message)
  "APPEND MESSAGE to ACCOUNT's INBOX on IMAP-SERVER."
  (let ((conn (vm-imap-live--open imap-server)))
    (unwind-protect
	(progn
	  (vm-imap-live-login conn imap-server account)
	  (vm-imap-live-append conn "INBOX" message))
      (vm-imap-live-close conn))))

(defun vm-pop-live-purge-inbox (imap-server account id)
  "Delete and expunge any INBOX message on IMAP-SERVER carrying fixture id ID.
Used to clean up after a test, whether or not it got as far as retrieving
what it put there."
  (let ((conn (vm-imap-live--open imap-server)))
    (unwind-protect
	(progn
	  (vm-imap-live-login conn imap-server account)
	  (vm-imap-live-cmd-ok conn "SELECT \"INBOX\"")
	  (let ((text (vm-imap-live-cmd-ok
		       conn "SEARCH HEADER X-VMTest-Id \"%s\"" id)))
	    (when (string-match "\\* SEARCH\\([ 0-9]*\\)" text)
	      (dolist (n (split-string (match-string 1 text) " " t))
		(ignore-errors
		  (vm-imap-live-cmd-ok conn "STORE %s +FLAGS.SILENT (\\Deleted)"
				       n))))
	    (ignore-errors (vm-imap-live-cmd-ok conn "EXPUNGE"))))
      (vm-imap-live-close conn))))

(provide 'vm-pop-live-init)

;;; vm-pop-live-init.el ends here
