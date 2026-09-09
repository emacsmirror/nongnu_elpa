;;; vm-pop-net.el --- POP over the non-blocking driver  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 The VM Developers
;;
;; This file is part of VM.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; POP3 written as generators, so that a session runs in the process filter
;; and Emacs is never inside a wait.  The blocking implementation in vm-pop.el
;; is still what the commands use; this is the protocol layer they will move
;; to, converted first because POP is the smaller and simpler of the two --
;; five waits against IMAP's four in a recursive parser, and no session state
;; machine.  See dev/docs/design/async-imap.org.
;;
;; Everything here is an `iter-defun' and so can only be run by the driver in
;; vm-net.el:
;;
;;   (vm-net-start session (vm-pop-net-session user password))
;;
;; Each of these yields exactly where vm-pop.el waits, and for the same
;; reason: POP responses end in a terminator, so a read asks to be resumed
;; when the terminator is in the buffer and not before.
;;
;; The read point is a marker in the process buffer, as it is in vm-pop.el:
;; the generator reads from where the last read stopped, and the filter
;; appends at the process mark.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'vm-net)
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(defvar vm-pop-net-read-point nil
  "Where the next read of this POP session starts.
Buffer-local to the process buffer, as `vm-pop-read-point\\=' is for the
blocking implementation.")
(make-variable-buffer-local 'vm-pop-net-read-point)

(defvar vm-pop-net-auth nil
  "The authentication method this session's maildrop asked for.
A string: pass or apop.  Buffer-local to the session's process buffer, so
that vm-pop-net-authenticate can choose without every caller having to hand
it on.

It was not read at all until 2026, so an apop maildrop was served with USER
and PASS and its password went over the wire in clear (emacs-vm/vm#823).")
(make-variable-buffer-local 'vm-pop-net-auth)

(define-error 'vm-pop-net-error "POP error")

(defun vm-pop-net-init ()
  "Prepare the current buffer to be a POP session's process buffer."
  (setq vm-pop-net-read-point (point-min-marker))
  (goto-char (point-min)))

(iter-defun vm-pop-net-read-line ()
  "Read one line of response, and answer with it, terminator and all.
Yields until there is a complete line: a partial one is not an answer, and
reading it as one is how a client gets out of step with a server."
  ;; a position and not the marker itself: `set-marker' below moves the
  ;; marker, and the text wanted is the text before it moved
  (let ((start (marker-position (or vm-pop-net-read-point
				    (setq vm-pop-net-read-point
					  (point-min-marker))))))
    (while (not (save-excursion
		  (goto-char start)
		  (re-search-forward "\r\n" nil t)))
      (iter-yield (vm-net-request-match "\r\n" start)))
    (let ((end (save-excursion (goto-char start)
			       (re-search-forward "\r\n" nil t))))
      (set-marker vm-pop-net-read-point end)
      (buffer-substring-no-properties start end))))

(iter-defun vm-pop-net-read-response ()
  "Read one response line and answer with it, signalling on -ERR.
The blocking code answers nil for an error and leaves the caller to work out
what happened; a signal says what the server said, which is what a user is
shown when a session fails."
  (let ((line (iter-yield-from (vm-pop-net-read-line))))
    (cond ((string-prefix-p "+OK" line) line)
	  ((string-prefix-p "-ERR" line)
	   (signal 'vm-pop-net-error (list (string-trim line))))
	  (t (signal 'vm-pop-net-error
		     (list (format "unrecognized response: %s"
				   (string-trim line))))))))

(iter-defun vm-pop-net-read-multiline ()
  "Read a multi-line response and answer with its lines, the dot removed.
The first line has been read already; this is the body after it."
  (let ((start (marker-position vm-pop-net-read-point)))
    (while (not (save-excursion
		  (goto-char start)
		  (re-search-forward "^\\.\r\n" nil t)))
      (iter-yield (vm-net-request-match "^\\.\r\n" start)))
    (let ((end (save-excursion (goto-char start)
			       (re-search-forward "^\\.\r\n" nil t))))
      (set-marker vm-pop-net-read-point end)
      (let* ((text (buffer-substring-no-properties
		    start (save-excursion (goto-char end) (forward-line -1)
					  (point))))
	     ;; the terminating CRLF of the last line, and only that one: a
	     ;; body may end in blank lines and they are part of it
	     (body (if (string-suffix-p "\r\n" text)
		       (substring text 0 -2)
		     text)))
	;; RFC 1939 §3: a line of the body beginning with a dot was sent with
	;; another in front of it, and the client takes it off again.
	;;
	;; Empty lines are kept.  Dropping them takes the blank line between
	;; the headers and the body with them, and a message whose headers run
	;; straight into its text has no body at all.
	(mapcar (lambda (line)
		  (if (string-prefix-p ".." line) (substring line 1) line))
		(split-string body "\r\n"))))))

(defun vm-pop-net-send (command)
  "Send COMMAND to this session's server, and note where its answer starts."
  (let ((process (get-buffer-process (current-buffer))))
    (goto-char (point-max))
    (set-marker vm-pop-net-read-point (point))
    (process-send-string process (concat command "\r\n"))))

(iter-defun vm-pop-net-command (command)
  "Send COMMAND and answer with its one-line response."
  (vm-pop-net-send command)
  (iter-yield-from (vm-pop-net-read-response)))

(iter-defun vm-pop-net-command-multiline (command)
  "Send COMMAND and answer with the lines of its multi-line response."
  (vm-pop-net-send command)
  (iter-yield-from (vm-pop-net-read-response))
  (iter-yield-from (vm-pop-net-read-multiline)))

;;; The session

(iter-defun vm-pop-net-greeting ()
  "Read the greeting, and answer with it.  A server that says -ERR here has
refused the connection, which the read reports as an error."
  (vm-pop-net-init)
  (iter-yield-from (vm-pop-net-read-response)))

(declare-function vm-pop-md5 "vm-crypto" (string))
(declare-function vm-parse "vm-misc"
		  (string regexp &optional matchn matches))

(defun vm-pop-net-timestamp (greeting)
  "The APOP timestamp in GREETING, or nil.
RFC 1939 section 7: a server offering APOP puts a message id in angle
brackets at the end of its greeting, and that is what the digest is taken
over."
  (car (vm-parse greeting "[^<]+\\(<[^>]+>\\)")))

(iter-defun vm-pop-net-authenticate (user password &optional greeting)
  "Log in as USER with PASSWORD.

APOP where the maildrop asked for it, USER and PASS otherwise.  GREETING is
what the server said, which is where the APOP timestamp comes from.

A maildrop asking for APOP against a server that offers no timestamp is an
error rather than a quiet fall back to PASS: the password would go over the
wire in clear, which is the thing APOP was asked for to avoid."
  (if (equal vm-pop-net-auth "apop")
      (let ((timestamp (vm-pop-net-timestamp (or greeting ""))))
	(unless timestamp
	  (signal 'vm-pop-net-error
		  (list "server offers no APOP timestamp")))
	(iter-yield-from
	 (vm-pop-net-command
	  (format "APOP %s %s" user
		  (vm-pop-md5 (concat timestamp password))))))
    (iter-yield-from (vm-pop-net-command (format "USER %s" user)))
    (iter-yield-from (vm-pop-net-command (format "PASS %s" password))))
  t)

(iter-defun vm-pop-net-stat ()
  "Answer with (COUNT . OCTETS), what STAT says the maildrop holds."
  (let* ((line (iter-yield-from (vm-pop-net-command "STAT")))
	 (fields (split-string line "[ \r\n]+" t)))
    (cons (string-to-number (or (nth 1 fields) "0"))
	  (string-to-number (or (nth 2 fields) "0")))))

(iter-defun vm-pop-net-uidl ()
  "Answer with the UIDs of the maildrop, as (NUMBER . UID) in server order.
Answers nil when the server has no UIDL: a maildrop VM cannot identify
messages in is one it must not delete from, and that is the caller's
decision to make."
  ;; a named variable and a handler body that is not nil: inside an
  ;; iter-defun, (condition-case nil FORM (err nil)) answers with the error
  ;; object rather than nil, which is generator.el's CPS transform and not
  ;; what the same code means outside one
  (condition-case _err
      (let ((lines (iter-yield-from (vm-pop-net-command-multiline "UIDL"))))
	(delq nil
	      (mapcar (lambda (line)
			(let ((fields (split-string line "[ \t]+" t)))
			  (when (cdr fields)
			    (cons (string-to-number (car fields))
				  (cadr fields)))))
		      lines)))
    (vm-pop-net-error nil)))

(iter-defun vm-pop-net-retrieve (n)
  "Answer with message N as it arrived, headers and body."
  (let ((lines (iter-yield-from
		(vm-pop-net-command-multiline (format "RETR %d" n)))))
    (concat (string-join lines "\n") "\n")))

(iter-defun vm-pop-net-delete (n)
  "Mark message N deleted on the server."
  (iter-yield-from (vm-pop-net-command (format "DELE %d" n)))
  t)

(defvar vm-pop-net-said-goodbye nil
  "Whether this session has said QUIT and heard the answer.

Bound per session by the generators that delete, so the `unwind-protect' that
covers an abandoned session does not say it twice.")

(iter-defun vm-pop-net-quit ()
  "Say QUIT and read what the server says to it.

QUIT is what makes a POP server act on the session's deletions, and RFC 1939
3.5 lets it answer -ERR -- \"some deleted messages not removed\" -- when it
could not.  Written blind, as the unwinding does for a session that was
abandoned, that answer is never seen and VM records deletions the maildrop
still has.  Read here, an -ERR signals, and what the folder owes the server
stays on its list."
  (iter-yield-from (vm-pop-net-command "QUIT"))
  (setq vm-pop-net-said-goodbye t)
  t)

(iter-defun vm-pop-net-session (user password)
  "A whole session: greet, log in, ask what is there, and say goodbye.
Answers (COUNT . OCTETS) from STAT.  The QUIT is in an `unwind-protect', so
it is said whether the session ran to the end or was abandoned -- a POP
server that is not told QUIT rolls back the deletions of the session, and
under generators that form runs only because `vm-net-abandon\\=' closes the
generator rather than dropping it."
  (unwind-protect
      (progn
	(let ((greeting (iter-yield-from (vm-pop-net-greeting))))
	  (iter-yield-from (vm-pop-net-authenticate user password greeting)))
	(iter-yield-from (vm-pop-net-stat)))
    (let ((process (get-buffer-process (current-buffer))))
      (when (process-live-p process)
	(process-send-string process "QUIT\r\n")))))


;;; Connecting

(declare-function vm-pop-parse-spec-to-list "vm-pop" (spec))
(declare-function vm-pop-find-name-for-spec "vm-pop" (spec))
(declare-function vm-popdrop-sans-password "vm-misc" (source))
(declare-function vm-binary-coding-system "vm-misc" ())
(declare-function vm-folder-type-to-write "vm-folder" (&optional file))

(defvar vm-pop-server-timeout)
(defvar vm-pop-retrieved-messages)

(define-error 'vm-pop-net-no-password
  "VM has no password for this POP maildrop")

(defvar vm-stunnel-program)
(defvar vm-stunnel-program-switches)
(defvar vm-ssh-program)
(defvar vm-ssh-program-switches)
(defvar vm-ssh-remote-command)

(declare-function vm-setup-stunnel-random-data-if-needed "vm-crypto" ())
(declare-function vm-stunnel-configuration-args "vm-crypto" (host port))

(defvar vm-pop-keep-trace-buffer)
(defvar vm-kept-pop-buffers)
(declare-function vm-keep-some-buffers "vm-misc"
		  (buffer ring-variable number-to-keep &optional rename-prefix))

(defun vm-pop-net-done-with-buffer (buffer)
  "Finish with BUFFER, a session's process buffer.
Kept as a trace when `vm-pop-keep-trace-buffer' says to; see
`vm-imap-net-done-with-buffer'."
  (when (buffer-live-p buffer)
    ;; nothing to keep when nothing was said: a connection that was never made
    ;; leaves an empty buffer, and keeping those is how a session that failed
    ;; early leaves one behind for every attempt
    (if (or (null vm-pop-keep-trace-buffer)
	    (zerop (buffer-size buffer)))
	(kill-buffer buffer)
      (vm-keep-some-buffers buffer 'vm-kept-pop-buffers
			    vm-pop-keep-trace-buffer "saved "))))

(defun vm-pop-net-connect (name host port buffer &optional tls)
  "A connection to HOST at PORT, made without waiting for it to come up.
The process is not open when this returns; the session's sentinel hears
whether it ever will be, and its timeout covers a connect that never
completes.

TLS goes through `open-network-stream', as for IMAP: `make-network-process'
has no TLS and answers `:type \\='tls' with \"Unsupported connection type\"."
  (let ((process
	 (if tls
	     (open-network-stream name buffer host port
				  :type 'tls :nowait t :coding 'binary)
	   (make-network-process :name name :host host :service port
				 :buffer buffer :noquery t :coding 'binary
				 :nowait t))))
    (set-process-query-on-exit-flag process nil)
    process))

(defvar vm-pop-passwords)
(defvar vm-pop-ok-to-ask)

(declare-function vm-auth-source-password "vm-misc" (hosts port user))
(declare-function vm-pop-get-password "vm-pop"
		  (popdrop source user host port ask-password))

(defun vm-pop-net-known-password (source user host port)
  "The password VM already holds for SOURCE, or nil.
Its own cache first, then auth-source.  Nothing is written back, and only a
non-empty string counts: see `vm-imap-net-known-password'."
  (let* ((spec (vm-popdrop-sans-password source))
	 (known (car (cdr (assoc spec vm-pop-passwords))))
	 (password (or known
		       (vm-auth-source-password
			(list (vm-pop-find-name-for-spec source) host)
			port user))))
    (and (stringp password)
	 (not (equal password ""))
	 (not (equal password "*"))
	 password)))

(defun vm-pop-net-open (source name &optional may-ask)
  "Open a connection for the POP maildrop SOURCE and answer with a session.

MAY-ASK says the caller is a command and the reader is there to be asked for
a password.  A timer passes nil: a question from a timer arrives while
somebody is typing something else.

NAME goes in messages.  The session has a buffer of its own and is ready for
`vm-net-start'; the answer is (SESSION USER PASSWORD).

Plain, TLS, over ssh, and through stunnel where the user has one and would
rather use it than Emacs's own TLS.  An ssh session has no process yet when
this returns: ssh has to be listening on its forwarded port before there is
anything to connect to, and `vm-net-attach' gives the session its connection
when it is.  stunnel is the connection itself, over its standard input and
output.  A maildrop whose password VM has not been told signals
`vm-pop-net-no-password', there being nobody to ask from inside a filter."
  (let* ((parts (vm-pop-parse-spec-to-list source))
	 (protocol (car parts))
	 (host (nth 1 parts))
	 (port (nth 2 parts))
	 (auth (nth 3 parts))
	 (user (nth 4 parts))
	 (password (nth 5 parts)))
    (unless (member protocol '("pop" "pop-ssl" "pop-ssh"))
      (error (concat "%s is not a POP maildrop type VM knows.  The types"
		     " are pop, pop-ssl and pop-ssh; M-x"
		     " vm-check-configuration checks every maildrop")
	     protocol))
    ;; Ignoring this field is what made an `apop' maildrop authenticate with
    ;; USER and PASS, sending the password in clear (emacs-vm/vm#823).
    (unless (member auth '("pass" "apop"))
      (if (equal auth "rpop")
	  (error (concat "rpop is no longer supported: it relied on a"
			 " privileged source port and sent the password"
			 " under another verb.  Write pass or apop in the"
			 " maildrop instead; see Spool Files in the VM"
			 " manual"))
	(error (concat "%s is not a POP authentication VM knows."
		       "  Write pass, or apop where the server offers a"
		       " timestamp")
	       (or auth "no authentication"))))
    (when (and (stringp port) (string-match "\\`[0-9]+\\'" port))
      (setq port (string-to-number port)))
    (when (equal password "*")
      ;; "*" means VM is to find the password rather than read it out of the
      ;; maildrop.  VM may already know it; failing that, a command may ask
      ;; the reader.
      (setq password (or (vm-pop-net-known-password source user host port)
			 (and may-ask
			      ;; as for IMAP: nothing binds `vm-pop-ok-to-ask'
			      ;; on the way here, so requiring it meant the
			      ;; question was never put
			      (let ((vm-pop-ok-to-ask t))
				(condition-case nil
				    (vm-pop-get-password
				     (or (vm-pop-find-name-for-spec source)
					 (vm-safe-popdrop-string source))
				     (vm-popdrop-sans-password source)
				     user host port t)
				  (error nil))))))
      (unless (and (stringp password) (not (equal password "")))
	;; the keys and not the passwords, as for IMAP: what this distinguishes
	;; is a password never remembered from one remembered under a key the
	;; check does not look under
	(vm-inform 10 "%s: no password held under %s; VM holds %s" name
		   (vm-popdrop-sans-password source)
		   (if vm-pop-passwords
		       (mapconcat #'car vm-pop-passwords ", ")
		     "none"))
	(signal 'vm-pop-net-no-password
		(list "password not remembered" source))))
    (let* ((buffer (generate-new-buffer (format " *%s*" name)))
	   (session (vm-net-session :name name :timeout vm-pop-server-timeout))
	   (opened nil))
      (with-current-buffer buffer
	(buffer-disable-undo)
	(vm-pop-net-init)
	(setq vm-pop-net-auth auth))
      (setf (vm-net-session-buffer session) buffer)
      ;; as for IMAP: the buffer goes with a connection that was never made
      (unwind-protect
	  (progn
	    (cond
	     ((equal protocol "pop-ssh")
	(let ((local (vm-net-free-port)))
	  (vm-net-tunnel
	   session vm-ssh-program
	   (nconc (list "-L" (format "%d:%s:%s" local host port))
		  (copy-sequence vm-ssh-program-switches)
		  (list host vm-ssh-remote-command))
	   local (or vm-pop-server-timeout 30)
	   (lambda (tunnel)
	     (when tunnel
	       (vm-net-attach session (vm-pop-net-connect name "127.0.0.1"
							  local buffer)))))))
       ((and (equal protocol "pop-ssl") vm-stunnel-program)
	(vm-setup-stunnel-random-data-if-needed)
	;; as for IMAP: stunnel relays its own standard input and output, and
	;; is the connection rather than something to connect through
	(setf (vm-net-session-process session)
	      (vm-net-pipe session name buffer vm-stunnel-program
			   (nconc (vm-stunnel-configuration-args host port)
				  (copy-sequence vm-stunnel-program-switches)))))
	     (t
	      (setf (vm-net-session-process session)
		    (vm-pop-net-connect name host port buffer
					(equal protocol "pop-ssl")))))
	    (setq opened t))
	(unless opened
	  (vm-pop-net-done-with-buffer buffer)))
      (list session user password))))

;;; Checking for mail, which is the first thing a command wanted

(iter-defun vm-pop-net-unretrieved (user password source retrieved)
  "Answer with how many messages of the maildrop have not been retrieved.
RETRIEVED is `vm-pop-retrieved-messages' and SOURCE the maildrop without
its password, which is how an entry there names the maildrop it came from.

Answers nil when the server has no UIDL: without UIDs VM cannot tell what it
has already seen, and saying \"no mail\" would be a guess."
  (unwind-protect
      (progn
	(let ((greeting (iter-yield-from (vm-pop-net-greeting))))
	  (iter-yield-from (vm-pop-net-authenticate user password greeting)))
	(let ((uids (iter-yield-from (vm-pop-net-uidl))))
	  (when uids
	    (let ((count 0))
	      (dolist (pair uids)
		(let ((seen (assoc (cdr pair) retrieved)))
		  (unless (and seen
			       (equal (nth 1 seen) source)
			       (eq (nth 2 seen) 'uidl))
		    (setq count (1+ count)))))
	      count))))
    (let ((process (get-buffer-process (current-buffer))))
      (when (process-live-p process)
	(process-send-string process "QUIT\r\n")))))

(defun vm-pop-net-checkable-p (source)
  "Whether SOURCE can be checked for mail without waiting.

  POP or POP over TLS, with a password VM holds.  A pop-ssh maildrop starts a
tunnel program inside the connect, and a maildrop whose password is `*'
would ask for one -- neither of which a timer should do behind the reader."
  (condition-case nil
      (let ((parts (vm-pop-parse-spec-to-list source)))
	(and (member (car parts) '("pop" "pop-ssl"))
	     (nth 5 parts)
	     (not (equal (nth 5 parts) "*"))
	     t))
    (error nil)))

(defun vm-pop-net-check-mail (source callback &optional retrieved)
  "Ask SOURCE whether it has mail VM has not retrieved, and tell CALLBACK.

RETRIEVED is what counts as already had, `vm-pop-retrieved-messages' by
default.  A POP folder passes its own messages instead: they are what it
holds, and the list remembers only what was fetched into a folder somewhere
else.

CALLBACK is called with t, nil, or the error that stopped the session.  It
is called from the process filter, so the folder buffer it wants is the one
it remembers, not the one that happens to be current.

Nothing waits: this returns as soon as the connection is made."
  (let* ((retrieved (or retrieved vm-pop-retrieved-messages))
	 (popdrop (vm-popdrop-sans-password source))
	 (opened (vm-pop-net-open source "POP check"))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-pop-net-done-with-buffer buffer)
	    (funcall callback
		     (if (vm-net-session-error finished)
			 (vm-net-session-error finished)
		       (let ((count (vm-net-session-value finished)))
			 (and count (> count 0)))))))
    (vm-net-start session
		  (vm-pop-net-unretrieved (nth 1 opened) (nth 2 opened)
					  popdrop retrieved))
    session))


;;; Fetching what has not been fetched

(defvar vm-pop-max-message-size)
(defvar vm-pop-messages-per-session)

(defun vm-pop-net-messages-to-fetch (uids sizes retrieved source)
  "Which of UIDS are to be fetched, as (NUMBER . UID) in server order.

Left out: what RETRIEVED already has from SOURCE, and what is larger than
`vm-pop-max-message-size'.  What is left for its size is named, since a
message nobody is told about is one nobody knows to raise the limit for; it
stays on the server, so raising the limit is all it takes.  Cut at
`vm-pop-messages-per-session' if that is set, so a maildrop with a thousand
messages in it is not one session."
  (let ((wanted nil)
	(too-large nil))
    (dolist (pair uids)
      (let* ((number (car pair))
	     (uid (cdr pair))
	     (seen (assoc uid retrieved))
	     (size (cdr (assq number sizes)))
	     (had (and seen
		       (equal (nth 1 seen) source)
		       (eq (nth 2 seen) 'uidl))))
	(cond
	 (had nil)
	 ((and vm-pop-max-message-size size
	       (> size vm-pop-max-message-size))
	  (push size too-large))
	 (t (push pair wanted)))))
    (when too-large
      (vm-net-warn 0 (concat "%s: %d message%s left on the server, over"
			     " vm-pop-max-message-size (%d): %s")
		   (vm-safe-popdrop-string source)
		   (length too-large) (if (cdr too-large) "s" "")
		   vm-pop-max-message-size
		   (mapconcat (lambda (size) (format "%d bytes" size))
			      (nreverse too-large) ", ")))
    (setq wanted (nreverse wanted))
    (if vm-pop-messages-per-session
	(seq-take wanted vm-pop-messages-per-session)
      wanted)))

(iter-defun vm-pop-net-fetch-new (folder user password source retrieved)
  "Fetch the messages of this maildrop that are not in RETRIEVED.

RETRIEVED is `vm-pop-retrieved-messages' and SOURCE the maildrop without
its password, which is how an entry there names where it came from.

Answers a list of (UID . TEXT), oldest first: the caller puts them in the
folder, which is folder work and does not belong in a process filter.

Nothing is deleted here, whatever the maildrop's auto-expunge setting says.
A DELE sent in this session takes effect at the QUIT that ends it, and the
QUIT is in an `unwind-protect', so an error part way through would commit the
deletion of messages whose text was thrown away with the session: fetched,
deleted on the server, never written anywhere.  The caller deletes them once
the crash box is on disk, in a session of its own and by UID.

Stops at `vm-pop-messages-per-session' if that is set, and passes over a
message bigger than `vm-pop-max-message-size' -- the same two limits the
blocking implementation honours, and for the same reason: a maildrop with a
thousand messages in it should not be one command."
  (unwind-protect
      (progn
	(let ((greeting (iter-yield-from (vm-pop-net-greeting))))
	  (iter-yield-from (vm-pop-net-authenticate user password greeting)))
	(let* ((uids (iter-yield-from (vm-pop-net-uidl)))
	       (sizes (and uids (iter-yield-from (vm-pop-net-sizes))))
	       (wanted (vm-pop-net-messages-to-fetch uids sizes retrieved source))
	       (total (length wanted))
	       (fetched nil)
	       (count 0))
	  (unless uids
	    ;; UIDL is what tells one message from another between sessions.
	    ;; Without it nothing here can say which of these has been fetched
	    ;; before, so nothing is fetched -- and a maildrop that quietly
	    ;; never arrives is worse than one that says why.  The blocking
	    ;; path deletes each message as it takes it instead, which is the
	    ;; other way to keep count and not one to start from a filter.
	    (signal 'vm-pop-net-error
		    (list (format (concat "%s: the server has no UIDL, so VM"
					  " cannot tell what it has already"
					  " fetched; no mail was retrieved")
				  (vm-safe-popdrop-string source)))))
	  (vm-pop-net-note-progress folder 0 total)
	  ;; the start, said once: a fetch nobody is frozen out of looks like
	  ;; nothing happening unless VM says it began
	  (unless (zerop total)
	    (vm-net-inform 5 "%s: retrieving %d message%s..."
			   (vm-safe-popdrop-string source) total
			   (if (= total 1) "" "s")))
	  (dolist (pair wanted)
	    (push (cons (cdr pair)
			(iter-yield-from (vm-pop-net-retrieve (car pair))))
		  fetched)
	    (setq count (1+ count))
	    ;; level 6, so it is logged and not shown: the mode line carries the
	    ;; count live, and a line per bunch in the echo area is in the way of
	    ;; whoever is using Emacs while the fetch runs -- which is the point of
	    ;; the fetch not freezing them out.  The start and the end are said.
	    (vm-pop-net-note-progress folder count total)
	    (vm-net-inform 6 "%s: %d of %d messages retrieved"
		       (vm-safe-popdrop-string source) count total))
	  (nreverse fetched)))
    (let ((process (get-buffer-process (current-buffer))))
      (when (process-live-p process)
	(process-send-string process "QUIT\r\n")))))

(iter-defun vm-pop-net-sizes ()
  "Answer with the sizes of the maildrop, as (NUMBER . OCTETS).
LIST rather than a RETR that turns out to be enormous: the size decides
whether a message is fetched at all."
  (condition-case _err
      (let ((lines (iter-yield-from (vm-pop-net-command-multiline "LIST"))))
	(delq nil
	      (mapcar (lambda (line)
			(let ((fields (split-string line "[ \t]+" t)))
			  (when (cdr fields)
			    (cons (string-to-number (car fields))
				  (string-to-number (cadr fields))))))
		      lines)))
    (vm-pop-net-error nil)))

(defun vm-pop-net-fetch (source retrieved callback &optional folder)
  "Fetch what SOURCE holds that RETRIEVED does not, and tell CALLBACK.

CALLBACK is given a list of (UID . TEXT), oldest first, or the error that
stopped the session.  Nothing is deleted from the server here; see
`vm-pop-net-fetch-new' for why, and `vm-pop-net-expunge-maildrop' for what
does it afterwards.

Nothing waits.  The caller does the folder work when the callback comes:
appending to the folder and remembering the UIDs is done where a folder
buffer is, not in a process filter."
  (let* ((popdrop (vm-popdrop-sans-password source))
	 (opened (vm-pop-net-open source "POP fetch" 'may-ask))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-pop-net-done-with-buffer buffer)
	    (funcall callback (or (vm-net-session-error finished)
				  (vm-net-session-value finished)))))
    ;; taken by the folder before it is started, so that a second fetch of the
    ;; same folder is refused rather than found out about afterwards
    (with-current-buffer (or folder (current-buffer))
      (vm-pop-net-take-session
       session
       (vm-pop-net-fetch-new (or folder (current-buffer))
			     (nth 1 opened) (nth 2 opened)
			     popdrop retrieved)))
    session))


;;; Putting what was fetched into a folder

(declare-function vm-get-folder-type "vm-folder"
		  (&optional file start end ignore-visited))
(declare-function vm-munge-message-separators "vm-folder"
		  (folder-type start end))
(declare-function vm-pop-cleanup-region "vm-pop" (start end))
(declare-function vm-leading-message-separator "vm-folder"
		  (&optional folder-type message for-other-folder))
(declare-function vm-trailing-message-separator "vm-folder"
		  (&optional folder-type))
(declare-function vm-convert-folder-type-headers "vm-folder"
		  (old-type new-type))
(declare-function vm-safe-popdrop-string "vm-misc" (string))

(defvar vm-folder-type)
(defvar vm-default-folder-type)
(defvar vm-pop-auto-expunge-alist)
(defvar vm-pop-expunge-after-retrieving)

(defvar vm-pop-net-session nil
  "The session this folder has running, if it has one.
A folder runs one at a time: two writing into it would interleave what they
put there.")
(make-variable-buffer-local 'vm-pop-net-session)

(defun vm-pop-net-auto-expunge-p (source)
  "Whether messages fetched from SOURCE are to be deleted from the server.
`vm-pop-auto-expunge-alist' first, by the maildrop with its password and
then without, and `vm-pop-expunge-after-retrieving' failing those."
  (let ((entry (or (assoc source vm-pop-auto-expunge-alist)
		   (assoc (vm-popdrop-sans-password source)
			  vm-pop-auto-expunge-alist))))
    (if entry (cdr entry) vm-pop-expunge-after-retrieving)))

(defun vm-pop-net-write-crash-box (messages crash-box folder-type)
  "Write MESSAGES to CRASH-BOX in FOLDER-TYPE, and answer with how many.

MESSAGES is what `vm-pop-net-fetch' answers with.  A crash box rather than
the folder itself, because that is what VM recovers from when Emacs dies
between the fetch and the folder being written: `vm-gobble-crash-box' is
what reads it, here and after a crash alike.

The messages arrive in CRLF and with the separators the server chose, or
with none: the same cleaning up the blocking path does, in the same order."
  (let ((count 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; set here rather than let-bound outside: vm-folder-type is
      ;; buffer-local, so a binding made in the folder buffer is not what
      ;; this buffer sees, and the separators would come out empty
      (setq-local vm-folder-type folder-type)
      (dolist (message messages)
	(let ((start (point))
	      (end nil))
	  (insert (cdr message))
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (setq end (point-marker))
	  ;; No cleaning up here: `vm-pop-net-read-multiline' has already made
	  ;; the CRLFs LFs and taken the stuffed dots off, and a second pass
	  ;; over the same text takes a real leading dot with it -- a body line
	  ;; of ".hidden" arrived as "hidden" (emacs-vm/vm#822).
	  ;;
	  ;; Some servers send the separators and some do not, which is what
	  ;; the type of what arrived says.  Without them the message is a
	  ;; bare one and is given the folder's own, the same way and in the
	  ;; same order as vm-pop-retrieve-to-target does it.
	  (when (eq (vm-get-folder-type nil start end) 'unknown)
	    (vm-munge-message-separators folder-type start end)
	    (goto-char start)
	    (insert (vm-leading-message-separator folder-type))
	    (save-restriction
	      (narrow-to-region (point) end)
	      (vm-convert-folder-type-headers 'baremessage folder-type))
	    (goto-char end)
	    (insert-before-markers (vm-trailing-message-separator folder-type)))
	  (goto-char (point-max))
	  (setq count (1+ count))))
      (let ((coding-system-for-write 'binary)
	    (selective-display nil))
	(write-region (point-min) (point-max) crash-box nil 'quiet)))
    count))

(defun vm-pop-net-note-retrieved (messages source)
  "Remember the UIDs of MESSAGES as fetched from SOURCE.
This is `vm-pop-retrieved-messages', the list that stops a message being
fetched a second time, and it is buffer-local to the folder."
  (let ((popdrop (vm-popdrop-sans-password source)))
    (dolist (message messages)
      (setq vm-pop-retrieved-messages
	    (cons (list (car message) popdrop 'uidl)
		  vm-pop-retrieved-messages)))))

(defun vm-pop-net-get-mail (source crash-box callback)
  "Fetch new mail from SOURCE into CRASH-BOX, and tell CALLBACK.

CALLBACK is called in the folder buffer this was started from, with the
number of messages written, or with the error that stopped the fetch.  It
is for the caller to gobble the crash box: this writes it and remembers the
UIDs, and what to do with a folder is the folder's business.

Nothing waits.  Whether the messages are deleted from the server is
`vm-pop-net-auto-expunge-p', as it is for the blocking path -- and the
deletion is a session of its own, run once the crash box is written: what
the server still has is what VM has not saved yet."
  (let ((folder (current-buffer))
	;; an empty folder has no type of its own yet, and a crash box has
	;; to be written in some type or nothing can read it back
	(folder-type (vm-folder-type-to-write)))
    (vm-pop-net-when-free
     (format "fetching from %s" (vm-safe-popdrop-string source))
     (lambda ()
     (vm-pop-net-fetch
     source vm-pop-retrieved-messages
     (lambda (result)
       (when (buffer-live-p folder)
	 (with-current-buffer folder
	   (funcall callback
		    (if (and (consp result) (symbolp (car result))
			     (get (car result) 'error-conditions))
			result
		      (let ((count (vm-pop-net-write-crash-box
				    result crash-box folder-type)))
			(vm-pop-net-note-retrieved result source)
			(when (and result (vm-pop-net-auto-expunge-p source))
			  (vm-pop-net-delete-fetched folder source
						     (mapcar #'car result)))
			count))))))
     folder)))))

(defun vm-pop-net-delete-fetched (folder source uidls)
  "Delete UIDLS from SOURCE, now that they are written, and say how it went.
FOLDER is where to report to.  A failure loses nothing: the messages are on
the server still, and `vm-pop-retrieved-messages' stops them being fetched
again."
  (let ((name (buffer-name folder)))
    (unless (vm-pop-net-expunge-maildrop
	     source uidls
	     (lambda (result)
	       (if (and (consp result) (symbolp (car result))
			(get (car result) 'error-conditions))
		   (vm-net-warn 0 "%s: deleting on the server failed: %s" name
			    (error-message-string result))
		 (vm-net-inform 5 "%s: %d message%s deleted on the server"
			    name (length result)
			    (if (= (length result) 1) "" "s")))))
      (vm-net-warn 0 "%s: fetched mail is still on the server: %s" name
	       "VM has no password for the maildrop"))))


;;; A POP folder, which is a maildrop VM keeps a copy of

(declare-function vm-folder-pop-maildrop-spec "vm-folder" ())
(declare-function vm-mark-folder-modified-p "vm-folder" (&optional buffer))

(defvar vm-pop-messages-to-expunge)
(declare-function vm-pop-uidl-of "vm-message" (m))
(declare-function vm-set-pop-uidl-of "vm-message" (m uidl))
(declare-function vm-set-stuff-flag-of "vm-message" (m flag))
(declare-function vm-assimilate-new-messages "vm-folder" (&rest keys))
(declare-function vm-update-summary-and-mode-line "vm-summary" ())
(declare-function vm-thoughtfully-select-message "vm-folder" ())
(declare-function vm-present-current-message "vm-page" ())
(declare-function vm-arrival-blurb "vm-folder" (count))
(declare-function vm-inform "vm-misc" (level &rest args))
(declare-function vm-warn "vm-misc" (l secs &rest args))
(declare-function vm-get-folder-type "vm-folder"
		  (&optional file start end ignore-visited))

(defvar vm-message-list)
(defvar vm-spooled-mail-waiting)
(defvar vm-buffers-needing-display-update)
(defvar vm-modification-counter)

(defvar vm-mail-buffer)

(defun vm-pop-net-folder-buffer (&optional folder)
  "FOLDER, or the folder buffer the current buffer belongs to."
  (or folder
      (and (boundp 'vm-mail-buffer) vm-mail-buffer
	   (buffer-live-p vm-mail-buffer) vm-mail-buffer)
      (current-buffer)))

(defun vm-pop-net-trace-buffers (&optional folder)
  "The session buffers a bug report about FOLDER should carry, newest first.
`vm-kept-pop-buffers' and the buffer of the session still running, which is
not in the ring yet.  See `vm-imap-net-trace-buffers'."
  (let* ((session (with-current-buffer (vm-pop-net-folder-buffer folder)
		    vm-pop-net-session))
	 (live (and session (vm-net-session-live-p session)
		    (vm-net-session-buffer session))))
    (seq-filter #'buffer-live-p
		(if (and live (not (memq live vm-kept-pop-buffers)))
		    (cons live vm-kept-pop-buffers)
		  vm-kept-pop-buffers))))

(defvar vm-ml-session)
(declare-function vm-update-summary-and-mode-line "vm-folder" ())

(defvar vm-pop-net-waiting nil
  "What this folder is to do when the session running now has finished.
A list of (NAME . FUNCTION), oldest first.  A folder runs one session at a
time, so work asked for while one runs waits here rather than opening a second
connection that would write the same folder.")
(make-variable-buffer-local 'vm-pop-net-waiting)

(defun vm-pop-net-when-free (name function)
  "Run FUNCTION now, or when this folder's session ends.  Answers non-nil.
NAME says what it is, for the log.  Answers `later' when it was queued: the
work has not happened yet, and it happens on the one session this folder has
rather than as the second writer this is avoiding."
  (cond
   ((vm-pop-net-busy-p)
    (setq vm-pop-net-waiting
	  (append vm-pop-net-waiting (list (cons name function))))
    (vm-pop-net-show-session)
    (vm-net-inform 6 "%s: %s when the session running now has finished"
		   (buffer-name) name)
    'later)
   (t
    (funcall function))))

(defun vm-pop-net-run-next ()
  "Start the next thing this folder was waiting to do, if any.
One at a time: what it starts becomes the folder's session, and whatever is
still queued waits for that."
  (let ((next (car vm-pop-net-waiting)))
    (setq vm-pop-net-waiting (cdr vm-pop-net-waiting))
    (when next
      (condition-case reason
	  (funcall (cdr next))
	(error (vm-net-warn 0 "%s: %s: %s" (buffer-name) (car next)
			    (error-message-string reason)))))))

(defun vm-pop-net-take-session (session &optional iterator)
  "Record SESSION as this folder's, start ITERATOR as its work, and answer it.

Where the folder refuses a second session, as the IMAP side does: two of them
writing one folder is two sets of messages in one buffer and one cache file.
ITERATOR is started after the refusal rather than before, so a second session
is prevented instead of reported -- started first, the error arrived with a
session already talking to a server and the folder holding no record of it."
  (let ((folder (current-buffer)))
    (when (and vm-pop-net-session
	       (not (eq vm-pop-net-session session))
	       (vm-net-session-live-p vm-pop-net-session))
      (error "%s: a second session was started while %s was running"
	     (buffer-name folder)
	     (or (vm-net-session-name vm-pop-net-session) "one")))
    (setq vm-pop-net-session session)
    (vm-pop-net-show-session)
    (vm-net-at-end session
		   (lambda ()
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (vm-pop-net-run-next)
			 (vm-pop-net-show-session)))))
    (when iterator
      (vm-net-start session iterator))
    session))

(defvar vm-pop-net-progress nil
  "How far the session running in this folder has got, as (DONE . TOTAL).
A folder runs one session at a time, so one pair says it.  What the fetch says
in the echo area is gone by the next message; this stays in the mode line until
the next message moves it on.")
(make-variable-buffer-local 'vm-pop-net-progress)

(defun vm-pop-net-note-progress (folder done total)
  "Say that FOLDER's session has done DONE of TOTAL, and show it."
  (when (buffer-live-p folder)
    (with-current-buffer folder
      (setq vm-pop-net-progress (and total (> total 0) (cons done total)))
      (vm-pop-net-show-session))))

(defun vm-pop-net-show-session ()
  "Say in the mode line what this folder is doing with its server.
The folder buffer, its summary and its presentation all show it: a reader
looking at the summary is looking at a folder that is being written into.
How far it has got is there once it knows: \" fetching 24/340\"."
  (let* ((session vm-pop-net-session)
	 (running (and session (vm-net-session-live-p session)
		       (vm-net-session-doing (vm-net-session-name session))))
	 (progress (and running vm-pop-net-progress)))
    (unless running (setq vm-pop-net-progress nil))
    (setq vm-ml-session
	  (and running (propertize (concat " " running
					   (if progress
					       (format " %d/%d" (car progress)
						       (cdr progress))
					     "")
					   " ")
				   'face 'vm-net-session-face)))
    (vm-update-summary-and-mode-line)))

(defun vm-pop-net-stop ()
  "Stop what this folder is doing with its server.
For a folder that is going away; see `vm-imap-net-stop'."
  (let ((session vm-pop-net-session))
    (when (and session (vm-net-session-live-p session))
      (vm-net-inform 5 "%s: stopping %s" (buffer-name)
		 (or (vm-net-session-name session) "the session"))
      (vm-net-abandon session))
    (setq vm-pop-net-session nil)
    (setq vm-ml-session nil)))

(defun vm-pop-net-busy-p (&optional folder)
  "Whether FOLDER, or the current buffer's folder, has a session running."
  (with-current-buffer (vm-pop-net-folder-buffer folder)
    (and vm-pop-net-session
	 (vm-net-session-live-p vm-pop-net-session))))

(defun vm-pop-net-wait (&optional folder seconds)
  "Wait for FOLDER's session to finish, up to SECONDS.
For a caller that has to have the mail before it goes on -- a test, or a
command asked to do something with what arrives."
  (let ((folder (vm-pop-net-folder-buffer folder))
	(deadline (+ (float-time) (or seconds 30))))
    (save-current-buffer
      (while (and (vm-pop-net-busy-p folder) (< (float-time) deadline))
	(accept-process-output nil 0.05)))
    (not (vm-pop-net-busy-p folder))))

(defun vm-pop-net-folder-retrieved ()
  "What this POP folder already has, structured as `vm-pop-retrieved-messages'.
Its own messages as well as the list, since a message in the folder is one
that must not be fetched again whether the list remembers it or not."
  (let ((popdrop (vm-popdrop-sans-password (vm-folder-pop-maildrop-spec)))
	(retrieved (copy-sequence vm-pop-retrieved-messages)))
    (dolist (message vm-message-list)
      (let ((uidl (vm-pop-uidl-of message)))
	(when uidl
	  (push (list uidl popdrop 'uidl) retrieved))))
    retrieved))

(defun vm-pop-net-store-in-folder (folder folder-type messages)
  "Put MESSAGES, as `vm-pop-net-fetch' answers with them, into FOLDER.
Answers with the UIDLs stored, oldest first.  The same cleaning up the crash
box gets: CRLF to LF, and the folder's own separators where the server sent
none."
  (with-current-buffer folder
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(let ((buffer-read-only nil)	; a folder buffer is read-only
	      (uidls nil))
	  (dolist (message messages)
	    (let ((start (point))
		  (end nil))
	      (insert (cdr message))
	      (goto-char (point-max))
	      (unless (bolp) (insert "\n"))
	      (setq end (point-marker))
	      ;; Already cleaned by the reader; see the note above.
	      (when (eq (vm-get-folder-type nil start end) 'unknown)
		(vm-munge-message-separators folder-type start end)
		(goto-char start)
		(insert (vm-leading-message-separator folder-type))
		(save-restriction
		  (narrow-to-region (point) end)
		  (vm-convert-folder-type-headers 'baremessage folder-type))
		(goto-char end)
		(insert-before-markers (vm-trailing-message-separator
					folder-type)))
	      (set-marker end nil)
	      (goto-char (point-max))
	      (push (car message) uidls)))
	  (nreverse uidls))))))

(defun vm-pop-net-folder-arrived (folder uidls)
  "Take the messages just written into FOLDER into its message list.
Each is given the UIDL it was fetched under, which is what stops it being
fetched again."
  (with-current-buffer folder
    (setq vm-spooled-mail-waiting nil)
    (intern (buffer-name) vm-buffers-needing-display-update)
    (let ((new (vm-assimilate-new-messages :read-attributes nil))
	  (rest uidls))
      (when new
	(setq vm-modification-counter (1+ vm-modification-counter)))
      (dolist (message new)
	(vm-set-pop-uidl-of message (car rest))
	(vm-set-stuff-flag-of message t)
	(setq rest (cdr rest)))
      ;; Built before the selection below, which reads a message and alters
      ;; the new count.
      (let ((blurb (vm-arrival-blurb (length new))))
	(if (vm-thoughtfully-select-message)
	    (vm-present-current-message)
	  (vm-update-summary-and-mode-line))
	(vm-net-inform 5 "%s" blurb))
      (length new))))

(defun vm-pop-net-get-folder-mail ()
  "Start fetching this POP folder's new mail, and answer with whether it did.
Nil means nothing was started: VM has no password for the maildrop yet, or a
session is already running on the folder."
  (let* ((folder (current-buffer))
	 (source (vm-folder-pop-maildrop-spec))
	 (folder-type (vm-folder-type-to-write)))
    (cond
     ((vm-pop-net-busy-p) nil)
     (t
      (condition-case reason
	  (progn
	    (vm-pop-net-take-session
	     (vm-pop-net-fetch
		   source (vm-pop-net-folder-retrieved)
		   (lambda (result)
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (cond
			  ((and (consp result) (symbolp (car result))
				(get (car result) 'error-conditions))
			   (vm-net-warn 0 "%s: %s" (buffer-name folder)
				    (error-message-string result)))
			  ((null result)
			   (vm-net-inform 5 "%s: no new mail" (buffer-name folder)))
			  (t
			   (vm-pop-net-folder-arrived
			    folder
			    (vm-pop-net-store-in-folder folder folder-type
							result)))))))
	     folder))
	    (vm-net-inform 6 "%s: fetching new mail without waiting"
		       (buffer-name folder))
	    t)
	(vm-pop-net-no-password
	 (vm-net-inform 6 (concat "%s: not started, VM has no password for"
				  " the maildrop yet (%s)")
		    (buffer-name folder) (or (car (cdr reason)) "no password"))
	 nil))))))


(iter-defun vm-pop-net-expunge-session (user password uidls)
  "Log in and delete the messages whose UIDs are UIDLS.

Answers (:deleted UIDLS :gone UIDLS): the ones this session deleted, and the
ones the maildrop does not list at all.  Both are settled and the caller can
forget them.  A UID the maildrop no longer has is a deletion that has already
happened, by an earlier session whose answer was lost or by another client,
and asking for it again is a session per save for ever: a request for a
message that had gone was still on the list after two goes at it.

By UID: a POP message number means something different after every session,
and the folder remembers what it deleted by UID.  The deletions take effect
when the server is told QUIT, which `vm-pop-net-session' style unwinding
does whether this runs to the end or is abandoned."
  (let ((vm-pop-net-said-goodbye nil))
    (unwind-protect
	(progn
	  (let ((greeting (iter-yield-from (vm-pop-net-greeting))))
	    (iter-yield-from (vm-pop-net-authenticate user password greeting)))
	  (let ((numbers (iter-yield-from (vm-pop-net-uidl)))
		(deleted nil))
	    (unless numbers
	      ;; without UIDL there is no telling which message is which, and
	      ;; deleting the wrong one is worse than deleting none
	      (signal 'vm-pop-net-error
		      (list "server has no UIDL; nothing deleted")))
	    (dolist (pair numbers)
	      (when (member (cdr pair) uidls)
		(iter-yield-from (vm-pop-net-delete (car pair)))
		(push (cdr pair) deleted)))
	    ;; and QUIT here rather than on the way out, so that the answer to
	    ;; it is read: that is where a server says it could not remove them
	    (iter-yield-from (vm-pop-net-quit))
	    (list :deleted (nreverse deleted)
		  :gone (seq-remove (lambda (uidl) (rassoc uidl numbers))
				    uidls))))
      (unless vm-pop-net-said-goodbye
	(let ((process (get-buffer-process (current-buffer))))
	  (when (process-live-p process)
	    (process-send-string process "QUIT\r\n")))))))

(defun vm-pop-net-send-changes ()
  "Start deleting on the server what this POP folder has expunged locally.

Answers with whether it did: nil means nothing was started, VM having no
password for the maildrop yet; `later' that a session is already running and
these deletions go up next time -- they are in
`vm-pop-messages-to-expunge', which is written into the folder file, so
nothing is lost by waiting.

A save owes the server the deletions and nothing else.  Working out what the
server no longer has means downloading the maildrop's UIDs, which is the
next fetch's business."
  (let* ((folder (current-buffer))
	 (uidls (copy-sequence vm-pop-messages-to-expunge)))
    (cond
     ((null uidls) nil)
     ((vm-pop-net-busy-p)
      (vm-net-inform 6 "%s: a session is running; these deletions go up next time"
		 (buffer-name folder))
      'later)
     (t
      (condition-case reason
	  (let* ((source (vm-folder-pop-maildrop-spec))
		 (opened (vm-pop-net-open source "POP expunge" 'may-ask))
		 (session (car opened))
		 (buffer (vm-net-session-buffer session))
		 (name (buffer-name folder)))
	    (setf (vm-net-session-finished session)
		  (lambda (finished)
		    (let ((process (vm-net-session-process finished)))
		      (when (process-live-p process) (delete-process process)))
		    (vm-pop-net-done-with-buffer buffer)
		    (cond
		     ((vm-net-session-error finished)
		      (vm-net-warn 0 "%s: %s" name
			       (error-message-string
				(vm-net-session-error finished))))
		     (t
		      (let* ((answer (vm-net-session-value finished))
			     (deleted (plist-get answer :deleted))
			     (gone (plist-get answer :gone))
			     (settled (append deleted gone)))
			;; what the server still has and did not delete stays on
			;; the list, so the next save offers it again
			(when (buffer-live-p folder)
			  (with-current-buffer folder
			    (setq vm-pop-messages-to-expunge
				  (seq-remove (lambda (uidl)
						(member uidl settled))
					      vm-pop-messages-to-expunge))
			    (vm-mark-folder-modified-p)))
			(vm-net-inform 5 "%s: %d message%s deleted on the server%s"
				   name (length deleted)
				   (if (= (length deleted) 1) "" "s")
				   (if gone
				       (format ", %d already gone" (length gone))
				     "")))))))
	    (vm-pop-net-take-session session
				     (vm-pop-net-expunge-session
				      (nth 1 opened) (nth 2 opened) uidls))
	    (vm-net-inform 6 "%s: deleting %d message%s on the server without waiting"
		       name (length uidls) (if (= (length uidls) 1) "" "s"))
	    t)
	(vm-pop-net-no-password
	 (vm-net-inform 6 (concat "%s: not started, VM has no password for"
				  " the maildrop yet (%s)")
		    (buffer-name folder) (or (car (cdr reason)) "no password"))
	 nil))))))

(defun vm-pop-net-expunge-maildrop (source uidls callback)
  "Delete the messages with UIDLS from the maildrop SOURCE, without waiting.
CALLBACK is called with the UIDLs deleted, or with the error.  Answers
whether it started."
  (condition-case nil
      (let* ((opened (vm-pop-net-open source "POP maildrop expunge" 'may-ask))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session)))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-pop-net-done-with-buffer buffer)
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))
	(vm-net-start session
		      (vm-pop-net-expunge-session (nth 1 opened) (nth 2 opened)
						  uidls))
	t)
    (vm-pop-net-no-password nil)))

(defun vm-pop-net-expunge-retrieved ()
  "Delete on their servers the messages this folder has retrieved by POP.

Answers whether it started; nil means nothing was started, VM having no
password for the first maildrop yet, and none after it is answered for
either.  One maildrop at a time: a POP server serves one session anyway, and
they all write the same folder.

The folder forgets each maildrop's messages as that maildrop answers for
them, so an expunge that fails half way leaves the rest to be offered again."
  (let ((folder (current-buffer))
	(groups nil)
	step)
    (dolist (entry vm-pop-retrieved-messages)
      (let* ((source (nth 1 entry))
	     (group (assoc source groups)))
	(if group
	    (setcdr group (cons (car entry) (cdr group)))
	  (push (list source (car entry)) groups))))
    (setq groups (nreverse groups))
    (setq step
	  (lambda (rest trouble first)
	    (cond
	     ((null rest)
	      (when (buffer-live-p folder)
		(with-current-buffer folder
		  (if trouble
		      (vm-net-warn 1 "Expunged what could be; trouble with %s"
			       (mapconcat #'identity (reverse trouble) ", "))
		    (vm-net-inform 5 "Retrieved messages deleted on the server"))))
	      t)
	     (t
	      (let* ((group (car rest))
		     (source (car group))
		     (name (or (vm-pop-find-name-for-spec source)
			       (vm-safe-popdrop-string source))))
		(vm-net-inform 6 "Deleting messages in %s..." name)
		(cond
		 ((vm-pop-net-expunge-maildrop
		   source (cdr group)
		   (lambda (result)
		     (cond
		      ((vm-net-error-p result)
		       (vm-net-warn 0 "%s: %s" name (error-message-string result))
		       (funcall step (cdr rest) (cons name trouble) nil))
		      (t
		       (let* ((deleted (plist-get result :deleted))
			      (settled (append deleted (plist-get result :gone))))
			 (when (buffer-live-p folder)
			   (with-current-buffer folder
			     (setq vm-pop-retrieved-messages
				   (seq-remove
				    (lambda (entry)
				      (and (equal (nth 1 entry) source)
					   (member (car entry) settled)))
				    vm-pop-retrieved-messages))
			     (when settled (vm-mark-folder-modified-p folder))
			     (vm-net-inform 6 "%s: %d message%s deleted" name
					(length deleted)
					(if (= (length deleted) 1) "" "s")))))
		       (funcall step (cdr rest) trouble nil)))))
		  t)
		 (first nil)
		 (t
		  (vm-net-warn 0 "%s: not deleted from, VM has no password for it"
			   name)
		  (funcall step (cdr rest) (cons name trouble) nil))))))))
    (and groups (funcall step groups nil t))))

(defvar vm-pop-retrieved-messages)
(declare-function vm-pop-find-name-for-spec "vm-pop" (spec))

(defun vm-pop-net-folder-check-mail ()
  "Start asking whether this POP folder has new mail, and answer with whether
it did.  The answer itself arrives later, in `vm-spooled-mail-waiting',
which is what the mode line reads.

Nil means nothing was started: VM has no password for the maildrop yet, or a
session is already running -- and one already running will say what arrived
anyway."
  (let ((folder (current-buffer))
	(source (vm-folder-pop-maildrop-spec)))
    (cond
     ((vm-pop-net-busy-p) nil)
     (t
      (condition-case reason
	  (progn
	    (vm-pop-net-take-session
	     (vm-pop-net-check-mail
		   source
		   (lambda (answer)
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (if (and (consp answer) (symbolp (car answer))
				  (get (car answer) 'error-conditions))
			     (vm-net-inform 6 "%s: could not check for new mail: %s"
					(buffer-name folder)
					(error-message-string answer))
			   (setq vm-spooled-mail-waiting answer)
			   (intern (buffer-name folder)
				   vm-buffers-needing-display-update)
			   (vm-update-summary-and-mode-line)
			   (vm-net-inform 6 "%s: %s" (buffer-name folder)
				      (if answer "new mail" "no new mail"))))))
		   ;; what this folder holds, not what was fetched into some
		   ;; other one
		   (vm-pop-net-folder-retrieved)))
	    (vm-net-inform 6 "%s: checking the server without waiting"
		       (buffer-name folder))
	    t)
	(vm-pop-net-no-password
	 (vm-net-inform 6 (concat "%s: not started, VM has no password for"
				  " the maildrop yet (%s)")
		    (buffer-name folder) (or (car (cdr reason)) "no password"))
	 nil))))))

(provide 'vm-pop-net)
;;; vm-pop-net.el ends here
