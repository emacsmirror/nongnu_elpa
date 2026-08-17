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

(defvar vm-pop-net-read-point nil
  "Where the next read of this POP session starts.
Buffer-local to the process buffer, as `vm-pop-read-point\\=' is for the
blocking implementation.")
(make-variable-buffer-local 'vm-pop-net-read-point)

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

(iter-defun vm-pop-net-authenticate (user password)
  "Log in as USER with PASSWORD, using USER and PASS."
  (iter-yield-from (vm-pop-net-command (format "USER %s" user)))
  (iter-yield-from (vm-pop-net-command (format "PASS %s" password)))
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

(iter-defun vm-pop-net-quit ()
  "Say QUIT, which is what makes the server act on the deletions."
  (iter-yield-from (vm-pop-net-command "QUIT"))
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
	(iter-yield-from (vm-pop-net-greeting))
	(iter-yield-from (vm-pop-net-authenticate user password))
	(iter-yield-from (vm-pop-net-stat)))
    (let ((process (get-buffer-process (current-buffer))))
      (when (process-live-p process)
	(process-send-string process "QUIT\r\n")))))


;;; Connecting

(declare-function vm-pop-parse-spec-to-list "vm-pop" (spec))
(declare-function vm-pop-get-password "vm-pop"
		  (popdrop source user host port ask-password))
(declare-function vm-pop-find-name-for-spec "vm-pop" (spec))
(declare-function vm-popdrop-sans-password "vm-misc" (source))
(declare-function vm-binary-coding-system "vm-misc" ())

(defvar vm-pop-server-timeout)
(defvar vm-pop-retrieved-messages)

(define-error 'vm-pop-net-unsupported "POP maildrop VM cannot open without waiting")

(defvar vm-stunnel-program)
(defvar vm-stunnel-program-switches)
(defvar vm-ssh-program)
(defvar vm-ssh-program-switches)
(defvar vm-ssh-remote-command)

(declare-function vm-setup-stunnel-random-data-if-needed "vm-crypto" ())
(declare-function vm-stunnel-configuration-args "vm-crypto" (host port))

(defun vm-pop-net-connect (name host port buffer &optional tls)
  "A connection to HOST at PORT, made without waiting for it to come up.
The process is not open when this returns; the session's sentinel hears
whether it ever will be, and its timeout covers a connect that never
completes.  TLS is negotiated the same way, Emacs doing the handshake as the
connection comes up."
  (make-network-process :name name :host host :service port :buffer buffer
			:noquery t :coding 'binary :nowait t
			:type (if tls 'tls nil)))

(defun vm-pop-net-open (source name)
  "Open a connection for the POP maildrop SOURCE and answer with a session.

NAME goes in messages.  The session has a buffer of its own and is ready for
`vm-net-start\='; the answer is (SESSION USER PASSWORD).

Plain, TLS, over ssh, and through stunnel where the user has one and would
rather use it than Emacs\='s own TLS.  A tunnelled session has no process yet
when this returns: the program has to be listening before there is anything
to connect to, and `vm-net-attach\=' gives the session its connection when
it is.  A maildrop whose password VM has not been told signals
`vm-pop-net-unsupported\=', there being nobody to ask from inside a filter."
  (let* ((parts (vm-pop-parse-spec-to-list source))
	 (protocol (car parts))
	 (host (nth 1 parts))
	 (port (nth 2 parts))
	 (user (nth 4 parts))
	 (password (nth 5 parts)))
    (unless (member protocol '("pop" "pop-ssl" "pop-ssh"))
      (signal 'vm-pop-net-unsupported (list protocol source)))
    (when (and (stringp port) (string-match "\\`[0-9]+\\'" port))
      (setq port (string-to-number port)))
    (when (equal password "*")
      ;; "*" means VM is to find the password rather than read it out of the
      ;; maildrop.  It may already know it -- from a session earlier in this
      ;; Emacs, or from auth-source -- and only asking the user is out of the
      ;; question here, there being nobody to ask from inside a filter.
      (setq password
	    (condition-case nil
		(vm-pop-get-password
		 (or (vm-pop-find-name-for-spec source)
		     (vm-safe-popdrop-string source))
		 (vm-popdrop-sans-password source)
		 user host port nil)
	      (error nil)))
      (unless password
	(signal 'vm-pop-net-unsupported
		(list "password not remembered" source))))
    (let* ((buffer (generate-new-buffer (format " *%s*" name)))
	   (session (vm-net-session :name name :timeout vm-pop-server-timeout)))
      (with-current-buffer buffer
	(buffer-disable-undo)
	(vm-pop-net-init))
      (setf (vm-net-session-buffer session) buffer)
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
	(let ((local (vm-net-free-port)))
	  (vm-net-tunnel
	   session vm-stunnel-program
	   (nconc (list "-d" (format "127.0.0.1:%d" local))
		  (vm-stunnel-configuration-args host port)
		  (copy-sequence vm-stunnel-program-switches))
	   local (or vm-pop-server-timeout 30)
	   (lambda (tunnel)
	     (when tunnel
	       (vm-net-attach session (vm-pop-net-connect name "127.0.0.1"
							  local buffer)))))))
       (t
	(setf (vm-net-session-process session)
	      (vm-pop-net-connect name host port buffer
				  (equal protocol "pop-ssl")))))
      (list session user password))))

;;; Checking for mail, which is the first thing a command wanted

(iter-defun vm-pop-net-unretrieved (user password source retrieved)
  "Answer with how many messages of the maildrop have not been retrieved.
RETRIEVED is `vm-pop-retrieved-messages\=' and SOURCE the maildrop without
its password, which is how an entry there names the maildrop it came from.

Answers nil when the server has no UIDL: without UIDs VM cannot tell what it
has already seen, and saying \"no mail\" would be a guess."
  (unwind-protect
      (progn
	(iter-yield-from (vm-pop-net-greeting))
	(iter-yield-from (vm-pop-net-authenticate user password))
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
tunnel program inside the connect, and a maildrop whose password is `*\='
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

RETRIEVED is what counts as already had, `vm-pop-retrieved-messages\=' by
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
	    (when (buffer-live-p buffer) (kill-buffer buffer))
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

(iter-defun vm-pop-net-fetch-new (user password source retrieved
				       &optional delete)
  "Fetch the messages of this maildrop that are not in RETRIEVED.

RETRIEVED is `vm-pop-retrieved-messages\=' and SOURCE the maildrop without
its password, which is how an entry there names where it came from.  DELETE
non-nil says to mark each fetched message deleted on the server.

Answers a list of (UID . TEXT), oldest first: the caller puts them in the
folder, which is folder work and does not belong in a process filter.

Stops at `vm-pop-messages-per-session\=' if that is set, and passes over a
message bigger than `vm-pop-max-message-size\=' -- the same two limits the
blocking implementation honours, and for the same reason: a maildrop with a
thousand messages in it should not be one command."
  (unwind-protect
      (progn
	(iter-yield-from (vm-pop-net-greeting))
	(iter-yield-from (vm-pop-net-authenticate user password))
	(let ((uids (iter-yield-from (vm-pop-net-uidl)))
	      (sizes nil)
	      (fetched nil)
	      (count 0))
	  (when uids
	    (setq sizes (iter-yield-from (vm-pop-net-sizes)))
	    (dolist (pair uids)
	      (let* ((number (car pair))
		     (uid (cdr pair))
		     (seen (assoc uid retrieved))
		     (size (cdr (assq number sizes))))
		(when (and (not (and seen
				     (equal (nth 1 seen) source)
				     (eq (nth 2 seen) 'uidl)))
			   (or (null vm-pop-messages-per-session)
			       (< count vm-pop-messages-per-session))
			   (or (null vm-pop-max-message-size)
			       (null size)
			       (<= size vm-pop-max-message-size)))
		  (push (cons uid (iter-yield-from
				   (vm-pop-net-retrieve number)))
			fetched)
		  (setq count (1+ count))
		  (when delete
		    (iter-yield-from (vm-pop-net-delete number)))))))
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

(defun vm-pop-net-fetch (source retrieved callback &optional delete)
  "Fetch what SOURCE holds that RETRIEVED does not, and tell CALLBACK.

CALLBACK is given a list of (UID . TEXT), oldest first, or the error that
stopped the session.  DELETE non-nil marks each fetched message deleted on
the server, which takes effect when the session says QUIT.

Nothing waits.  The caller does the folder work when the callback comes:
appending to the folder and remembering the UIDs is done where a folder
buffer is, not in a process filter."
  (let* ((popdrop (vm-popdrop-sans-password source))
	 (opened (vm-pop-net-open source "POP fetch"))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (when (buffer-live-p buffer) (kill-buffer buffer))
	    (funcall callback (or (vm-net-session-error finished)
				  (vm-net-session-value finished)))))
    (vm-net-start session
		  (vm-pop-net-fetch-new (nth 1 opened) (nth 2 opened)
					popdrop retrieved delete))
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
`vm-pop-auto-expunge-alist\=' first, by the maildrop with its password and
then without, and `vm-pop-expunge-after-retrieving\=' failing those."
  (let ((entry (or (assoc source vm-pop-auto-expunge-alist)
		   (assoc (vm-popdrop-sans-password source)
			  vm-pop-auto-expunge-alist))))
    (if entry (cdr entry) vm-pop-expunge-after-retrieving)))

(defun vm-pop-net-write-crash-box (messages crash-box folder-type)
  "Write MESSAGES to CRASH-BOX in FOLDER-TYPE, and answer with how many.

MESSAGES is what `vm-pop-net-fetch\=' answers with.  A crash box rather than
the folder itself, because that is what VM recovers from when Emacs dies
between the fetch and the folder being written: `vm-gobble-crash-box\=' is
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
	  (vm-pop-cleanup-region start end)
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
This is `vm-pop-retrieved-messages\=', the list that stops a message being
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
`vm-pop-net-auto-expunge-p\=', as it is for the blocking path."
  (let ((folder (current-buffer))
	;; an empty folder has no type of its own yet, and a crash box has
	;; to be written in some type or nothing can read it back
	(folder-type (or vm-folder-type vm-default-folder-type)))
    (setq vm-pop-net-session
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
			count))))))
     (vm-pop-net-auto-expunge-p source)))))


;;; A POP folder, which is a maildrop VM keeps a copy of

(declare-function vm-folder-pop-maildrop-spec "vm-folder" ())
(declare-function vm-pop-uidl-of "vm-message" (m))
(declare-function vm-set-pop-uidl-of "vm-message" (m uidl))
(declare-function vm-set-stuff-flag-of "vm-message" (m flag))
(declare-function vm-assimilate-new-messages "vm-folder" (&rest keys))
(declare-function vm-update-summary-and-mode-line "vm-summary" ())
(declare-function vm-thoughtfully-select-message "vm-folder" ())
(declare-function vm-present-current-message "vm-page" ())
(declare-function vm-emit-totals-blurb "vm-folder" ())
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
  "What this POP folder already has, in `vm-pop-retrieved-messages\=' shape.
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
  "Put MESSAGES, as `vm-pop-net-fetch\=' answers with them, into FOLDER.
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
	      (vm-pop-cleanup-region start end)
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
      (if (vm-thoughtfully-select-message)
	  (vm-present-current-message)
	(vm-update-summary-and-mode-line))
      (vm-inform 5 "%s: %d new message%s.  %s" (buffer-name folder)
		 (length new) (if (= (length new) 1) "" "s")
		 (vm-emit-totals-blurb))
      (length new))))

(defun vm-pop-net-get-folder-mail ()
  "Start fetching this POP folder's new mail, and answer with whether it did.
Nil means this maildrop is one that cannot be opened without waiting, or a
session is already running, and the caller is to use the blocking path."
  (let* ((folder (current-buffer))
	 (source (vm-folder-pop-maildrop-spec))
	 (folder-type (or vm-folder-type vm-default-folder-type)))
    (cond
     ((vm-pop-net-busy-p) nil)
     (t
      (condition-case nil
	  (progn
	    (setq vm-pop-net-session
		  (vm-pop-net-fetch
		   source (vm-pop-net-folder-retrieved)
		   (lambda (result)
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (cond
			  ((and (consp result) (symbolp (car result))
				(get (car result) 'error-conditions))
			   (vm-warn 0 2 "%s: %s" (buffer-name folder)
				    (error-message-string result)))
			  ((null result)
			   (vm-inform 5 "%s: no new mail" (buffer-name folder)))
			  (t
			   (vm-pop-net-folder-arrived
			    folder
			    (vm-pop-net-store-in-folder folder folder-type
							result)))))))))
	    t)
	(vm-pop-net-unsupported nil))))))


(defun vm-pop-net-folder-check-mail ()
  "Start asking whether this POP folder has new mail, and answer with whether
it did.  The answer itself arrives later, in `vm-spooled-mail-waiting\=',
which is what the mode line reads.

Nil means the maildrop cannot be opened without waiting, or a session is
already running -- and one already running will say what arrived anyway."
  (let ((folder (current-buffer))
	(source (vm-folder-pop-maildrop-spec)))
    (cond
     ((vm-pop-net-busy-p) nil)
     (t
      (condition-case nil
	  (progn
	    (vm-inform 6 "%s: checking the server without waiting"
		       (buffer-name folder))
	    (setq vm-pop-net-session
		  (vm-pop-net-check-mail
		   source
		   (lambda (answer)
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (if (and (consp answer) (symbolp (car answer))
				  (get (car answer) 'error-conditions))
			     (vm-inform 6 "%s: could not check for new mail: %s"
					(buffer-name folder)
					(error-message-string answer))
			   (setq vm-spooled-mail-waiting answer)
			   (intern (buffer-name folder)
				   vm-buffers-needing-display-update)
			   (vm-update-summary-and-mode-line)
			   (vm-inform 6 "%s: %s" (buffer-name folder)
				      (if answer "new mail" "no new mail"))))))
		   ;; what this folder holds, not what was fetched into some
		   ;; other one
		   (vm-pop-net-folder-retrieved)))
	    t)
	(vm-pop-net-unsupported nil))))))

(provide 'vm-pop-net)
;;; vm-pop-net.el ends here
