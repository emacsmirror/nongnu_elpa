;;; vm-pop-mock.el --- A POP3 server for tests, faults and all -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Issue #554 asks for a POP mock layer alongside the live IMAP rig.  Nothing
;; in vm-pop.el has ever been exercised end to end: the unit tests reach the
;; spec parsing and the file naming, and stop where the protocol starts.
;;
;; POP3 is small enough to serve honestly rather than stub, so this is a real
;; server on a local port -- greeting, USER/PASS or APOP, STAT, LIST, UIDL,
;; RETR, TOP, DELE, RSET, NOOP, QUIT -- and VM talks to it through its ordinary
;; code path, `vm-pop-make-session' and everything below it.  A stub of
;; `vm-pop-send-command' would prove far less: the interesting behaviour is in
;; how VM reads multi-line responses, counts octets and decides a message
;; arrived intact.
;;
;; It also misbehaves on request, which is the point.  Several of the things
;; worth knowing about a POP client are what it does when the server does not
;; play along:
;;
;;   :no-uidl        answer UIDL with -ERR, as pre-RFC1939 servers do; VM has a
;;                   whole fallback path for this and it has never been run
;;   :refuse         regexp; answer a matching command with -ERR
;;   :drop-on        regexp; close the connection when a command matches
;;   :silent-on      regexp; read a matching command and answer nothing, which
;;                   is the server that accepts and then says nothing -- the
;;                   case a client with no read timeout waits out for ever
;;   :truncate-retr  cut a RETR response off mid-message and close, which is
;;                   what a real interrupted download looks like
;;   :lie-about-size report a wrong octet count in LIST and STAT
;;   :slow-greeting  wait before greeting, for timeout tests
;;
;; The server records every command it received, so a test can assert on what
;; VM actually sent -- that a DELE really was withheld, say -- rather than on
;; what it was supposed to send.

;;; Code:

(require 'cl-lib)

(cl-defstruct (vm-pop-mock (:constructor vm-pop-mock--make))
  server port
  user password
  messages				; list of strings, 1-based by index
  deleted				; list of 1-based numbers marked DELE
  (log nil)
  ;; faults
  no-uidl refuse drop-on silent-on truncate-retr lie-about-size slow-greeting
  ;; per-connection state, this server serves one client at a time
  authenticated)

(defun vm-pop-mock--timestamp (mock)
  "Return the APOP timestamp this MOCK greets with."
  (format "<%d.%d@vm-pop-mock>" (vm-pop-mock-port mock) 1))

(defun vm-pop-mock--log (mock line)
  "Record LINE as received by MOCK."
  (setf (vm-pop-mock-log mock) (append (vm-pop-mock-log mock) (list line))))

(defun vm-pop-mock-commands (mock)
  "Return the commands MOCK has been sent, in order, as a list of strings."
  (vm-pop-mock-log mock))

(defun vm-pop-mock-received-p (mock regexp)
  "Return non-nil if MOCK was ever sent a command matching REGEXP."
  (cl-find-if (lambda (line) (string-match-p regexp line))
	      (vm-pop-mock-log mock)))

(defun vm-pop-mock-live-messages (mock)
  "Return the numbers of MOCK's messages that are not marked deleted."
  (let ((n 0) (live nil))
    (dolist (_ (vm-pop-mock-messages mock))
      (setq n (1+ n))
      (unless (memq n (vm-pop-mock-deleted mock))
	(setq live (append live (list n)))))
    live))

(defun vm-pop-mock--message (mock n)
  "Return MOCK's message N, or nil if there is no such live message."
  (and (integerp n) (> n 0)
       (<= n (length (vm-pop-mock-messages mock)))
       (not (memq n (vm-pop-mock-deleted mock)))
       (nth (1- n) (vm-pop-mock-messages mock))))

(defun vm-pop-mock--size (mock n)
  "Return the octet count MOCK reports for message N."
  (let ((size (length (vm-pop-mock--message mock n))))
    (if (vm-pop-mock-lie-about-size mock)
	(+ size 100)
      size)))

(defun vm-pop-mock--send (process string)
  "Send STRING to PROCESS if it is still alive.
A client can go away between the check and the send, and the signal from that
would take down the filter this runs in -- see `vm-pop-mock--filter'."
  (when (process-live-p process)
    (ignore-errors (process-send-string process string))))

(defun vm-pop-mock--send-multiline (process text)
  "Send TEXT to PROCESS as a POP3 multi-line response body.
Lines are CRLF-terminated and a leading dot is doubled, per RFC 1939, then
the terminating dot is sent."
  (dolist (line (split-string text "\n"))
    (vm-pop-mock--send process
		       (concat (if (string-prefix-p "." line) "." "")
			       line "\r\n")))
  (vm-pop-mock--send process ".\r\n"))

(defun vm-pop-mock--handle (mock process line)
  "Answer the single command LINE from PROCESS against MOCK."
  (vm-pop-mock--log mock line)
  (let* ((words (split-string line "[ \t]+" t))
	 (verb (upcase (or (car words) "")))
	 (arg (nth 1 words))
	 (n (and arg (string-match-p "\\`[0-9]+\\'" arg)
		 (string-to-number arg))))
    (cond
     ;; Faults first: they are about the server, not the protocol.
     ((and (vm-pop-mock-drop-on mock)
	   (string-match-p (vm-pop-mock-drop-on mock) line))
      (delete-process process))
     ((and (vm-pop-mock-silent-on mock)
	   (string-match-p (vm-pop-mock-silent-on mock) line))
      ;; heard, and deliberately unanswered
      nil)
     ((and (vm-pop-mock-refuse mock)
	   (string-match-p (vm-pop-mock-refuse mock) line))
      (vm-pop-mock--send process "-ERR the server declines\r\n"))
     ((equal verb "USER")
      (if (equal arg (vm-pop-mock-user mock))
	  (vm-pop-mock--send process "+OK user accepted\r\n")
	(vm-pop-mock--send process "-ERR no such user\r\n")))
     ((equal verb "PASS")
      (if (equal arg (vm-pop-mock-password mock))
	  (progn (setf (vm-pop-mock-authenticated mock) t)
		 (vm-pop-mock--send process "+OK logged in\r\n"))
	(vm-pop-mock--send process "-ERR bad password\r\n")))
     ((equal verb "APOP")
      ;; The digest is over the greeting timestamp and the password; check it
      ;; rather than wave it through, so a test can tell APOP was really done.
      (if (equal (nth 2 words)
		 (md5 (concat (vm-pop-mock--timestamp mock)
			      (vm-pop-mock-password mock))))
	  (progn (setf (vm-pop-mock-authenticated mock) t)
		 (vm-pop-mock--send process "+OK logged in\r\n"))
	(vm-pop-mock--send process "-ERR bad digest\r\n")))
     ((not (vm-pop-mock-authenticated mock))
      (vm-pop-mock--send process "-ERR not authenticated\r\n"))
     ((equal verb "STAT")
      (let ((count 0) (octets 0))
	(dolist (i (vm-pop-mock-live-messages mock))
	  (setq count (1+ count)
		octets (+ octets (vm-pop-mock--size mock i))))
	(vm-pop-mock--send process (format "+OK %d %d\r\n" count octets))))
     ((equal verb "LIST")
      (if n
	  (if (vm-pop-mock--message mock n)
	      (vm-pop-mock--send process
				 (format "+OK %d %d\r\n"
					 n (vm-pop-mock--size mock n)))
	    (vm-pop-mock--send process "-ERR no such message\r\n"))
	(vm-pop-mock--send process "+OK scan listing follows\r\n")
	(vm-pop-mock--send-multiline
	 process
	 (mapconcat (lambda (i) (format "%d %d" i (vm-pop-mock--size mock i)))
		    (vm-pop-mock-live-messages mock) "\n"))))
     ((equal verb "UIDL")
      (cond
       ((vm-pop-mock-no-uidl mock)
	(vm-pop-mock--send process "-ERR UIDL not supported\r\n"))
       (n
	(if (vm-pop-mock--message mock n)
	    (vm-pop-mock--send process (format "+OK %d uid%d\r\n" n n))
	  (vm-pop-mock--send process "-ERR no such message\r\n")))
       (t
	(vm-pop-mock--send process "+OK unique-id listing follows\r\n")
	(vm-pop-mock--send-multiline
	 process
	 (mapconcat (lambda (i) (format "%d uid%d" i i))
		    (vm-pop-mock-live-messages mock) "\n")))))
     ((equal verb "RETR")
      (let ((text (vm-pop-mock--message mock n)))
	(cond
	 ((null text)
	  (vm-pop-mock--send process "-ERR no such message\r\n"))
	 ((vm-pop-mock-truncate-retr mock)
	  ;; Announce the message, send part of it, then vanish.  This is what
	  ;; an interrupted download looks like from the client's side.
	  (vm-pop-mock--send process
			     (format "+OK %d octets\r\n" (length text)))
	  (vm-pop-mock--send process (substring text 0
						(/ (length text) 2)))
	  (delete-process process))
	 (t
	  (vm-pop-mock--send process
			     (format "+OK %d octets\r\n" (length text)))
	  (vm-pop-mock--send-multiline process text)))))
     ((equal verb "TOP")
      (let ((text (vm-pop-mock--message mock n))
	    (lines (string-to-number (or (nth 2 words) "0"))))
	(if (null text)
	    (vm-pop-mock--send process "-ERR no such message\r\n")
	  (vm-pop-mock--send process "+OK top of message follows\r\n")
	  (let* ((all (split-string text "\n"))
		 (headers nil))
	    ;; Headers, then LINES lines of the body.
	    (while (and all (not (equal (car all) "")))
	      (setq headers (append headers (list (car all)))
		    all (cdr all)))
	    (vm-pop-mock--send-multiline
	     process
	     (mapconcat #'identity
			(append headers '("")
				(butlast all (max 0 (- (length all) 1 lines))))
			"\n"))))))
     ((equal verb "DELE")
      (if (vm-pop-mock--message mock n)
	  (progn
	    (setf (vm-pop-mock-deleted mock)
		  (cons n (vm-pop-mock-deleted mock)))
	    (vm-pop-mock--send process (format "+OK message %d deleted\r\n" n)))
	(vm-pop-mock--send process "-ERR no such message\r\n")))
     ((equal verb "RSET")
      (setf (vm-pop-mock-deleted mock) nil)
      (vm-pop-mock--send process "+OK deletions undone\r\n"))
     ((equal verb "NOOP")
      (vm-pop-mock--send process "+OK\r\n"))
     ((equal verb "CAPA")
      (vm-pop-mock--send process "+OK capability list follows\r\n")
      (vm-pop-mock--send-multiline
       process (if (vm-pop-mock-no-uidl mock) "TOP\nUSER" "TOP\nUSER\nUIDL")))
     ((equal verb "QUIT")
      (vm-pop-mock--send process "+OK signing off\r\n")
      (delete-process process))
     (t
      (vm-pop-mock--send process "-ERR unknown command\r\n")))))

(defun vm-pop-mock--filter (process text)
  "Split TEXT from PROCESS into commands and answer each.
An error while answering is caught, recorded and answered -ERR.  Emacs prints
an error in a process filter to the messages and carries on, so an error here
was invisible to the test and cost it the rest of PENDING as well -- the
command still in there was never answered, and the client sat waiting for a
reply that was not coming until its deadline ran out.  A silent timeout is
the least debuggable thing a mock can do; failing loudly is the point."
  (let ((mock (process-get process 'vm-pop-mock))
	(pending (concat (or (process-get process 'vm-pop-mock-pending) "")
			 text))
	line)
    (unwind-protect
	(while (and (process-live-p process)
		    (string-match "\\`\\([^\r\n]*\\)\r?\n" pending))
	  (setq line (match-string 1 pending)
		pending (substring pending (match-end 0)))
	  (condition-case error
	      (vm-pop-mock--handle mock process line)
	    (error
	     (vm-pop-mock--log mock (format "!! error answering %s: %s"
					    line (error-message-string error)))
	     (vm-pop-mock--send process "-ERR internal mock error\r\n"))))
      ;; whatever happened, what has not been consumed is still owed an answer
      (process-put process 'vm-pop-mock-pending pending))))

(defun vm-pop-mock-errors (mock)
  "The errors MOCK hit while answering, as strings, newest last."
  (let (errors)
    (dolist (line (vm-pop-mock-log mock) (nreverse errors))
      (when (string-prefix-p "!! " line)
	(push line errors)))))

(defun vm-pop-mock--connection-buffer-away (client)
  "Detach and kill the buffer Emacs gave CLIENT.
An accepted connection gets a buffer named after its process, and this server
never reads it: what the client sends goes to `vm-pop-mock--filter' and what is
pending sits in a process property.  Left alone the buffer outlives the test,
one per connection.  Detached before it is killed, so killing it does not ask
about the live process."
  (let ((buffer (process-buffer client)))
    (set-process-buffer client nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun vm-pop-mock--on-connect (server client _message)
  "Greet CLIENT, which SERVER has just accepted."
  (let ((mock (process-get server 'vm-pop-mock)))
    (process-put client 'vm-pop-mock mock)
    (process-put client 'vm-pop-mock-pending "")
    (setf (vm-pop-mock-authenticated mock) nil)
    (set-process-coding-system client 'binary 'binary)
    (set-process-filter client #'vm-pop-mock--filter)
    (vm-pop-mock--connection-buffer-away client)
    (when (vm-pop-mock-slow-greeting mock)
      (sleep-for (vm-pop-mock-slow-greeting mock)))
    (vm-pop-mock--send client (format "+OK vm-pop-mock ready %s\r\n"
				      (vm-pop-mock--timestamp mock)))))

(cl-defun vm-pop-mock-start (&key (user "vmtest") (password "secret")
				  messages no-uidl refuse drop-on silent-on
				  truncate-retr lie-about-size slow-greeting)
  "Start a mock POP3 server on a local port and return it.
MESSAGES is the maildrop: a list of strings, each a whole RFC 5322 message.
The keywords after it are the faults described in the commentary above.
`vm-pop-mock-port' gives the port to point VM at."
  (let* ((mock (vm-pop-mock--make
		:user user :password password
		:messages messages :deleted nil
		:no-uidl no-uidl :refuse refuse :drop-on drop-on
		:silent-on silent-on
		:truncate-retr truncate-retr
		:lie-about-size lie-about-size
		:slow-greeting slow-greeting))
	 (server (make-network-process
		  :name "vm-pop-mock" :server t :service t
		  :host 'local :family 'ipv4 :coding 'binary :noquery t
		  :log #'vm-pop-mock--on-connect)))
    (process-put server 'vm-pop-mock mock)
    (setf (vm-pop-mock-server mock) server
	  (vm-pop-mock-port mock) (process-contact server :service))
    mock))

(defun vm-pop-mock-stop (mock)
  "Shut MOCK down, along with any connection it is serving.
Its own connections, found by the property `vm-pop-mock--on-connect' puts on
each: this used to kill every process whose name began with \"vm-pop-mock\",
which is two mocks killing each other's connections and, in the test that
speaks POP itself, the client -- it is called vm-pop-mock-test-client."
  (let ((server (vm-pop-mock-server mock)))
    (when (process-live-p server)
      (ignore-errors (delete-process server))))
  (dolist (process (process-list))
    (when (eq (process-get process 'vm-pop-mock) mock)
      (ignore-errors (delete-process process)))))

(defun vm-pop-mock-spec (mock &optional auth)
  "Return a VM POP maildrop specification pointing at MOCK.
AUTH defaults to \"pass\"; \"apop\" is the other one worth testing."
  (format "pop:127.0.0.1:%d:%s:%s:%s"
	  (vm-pop-mock-port mock)
	  (or auth "pass")
	  (vm-pop-mock-user mock)
	  (vm-pop-mock-password mock)))

(defmacro vm-pop-mock-with (spec &rest body)
  "Run BODY with a mock POP server bound to MOCK-VAR, then stop it.
SPEC is (MOCK-VAR &rest ARGS), where ARGS go to `vm-pop-mock-start'."
  (declare (indent 1) (debug t))
  `(let ((,(car spec) (vm-pop-mock-start ,@(cdr spec)))
         ;; A session remembers its password and keeps its buffer for reuse.
         ;; Bound, so the mock's credentials and buffer do not outlive the test
         ;; that invented them.
         (vm-pop-passwords vm-pop-passwords)
         (vm-kept-pop-buffers vm-kept-pop-buffers)
         ;; No session trace buffer: VM keeps one per session for debugging, and
         ;; the harness kills every new buffer the moment the test ends, so the
         ;; trace is unreachable anyway.  A session that errors sets this back
         ;; buffer-locally, so a failure still has its trace while it matters.
         (vm-pop-keep-trace-buffer nil)
         ;; `vm-warn' remembers its last warning so as not to repeat it, and
         ;; these tests produce warnings on purpose.
         (vm-current-warning vm-current-warning))
     (unwind-protect (progn ,@body)
       (vm-pop-mock-stop ,(car spec)))))

(provide 'vm-pop-mock)

;;; vm-pop-mock.el ends here
