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
      (let ((text (buffer-substring-no-properties
		   start (save-excursion (goto-char end) (forward-line -1)
					 (point)))))
	;; RFC 1939 §3: a line of the body beginning with a dot was sent with
	;; another in front of it, and the client takes it off again.
	(mapcar (lambda (line)
		  (if (string-prefix-p ".." line) (substring line 1) line))
		(split-string (string-trim-right text "\r\n") "\r\n" t))))))

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
(declare-function vm-popdrop-sans-password "vm-misc" (source))
(declare-function vm-binary-coding-system "vm-misc" ())

(defvar vm-pop-server-timeout)
(defvar vm-pop-retrieved-messages)

(define-error 'vm-pop-net-unsupported "POP maildrop VM cannot open without waiting")

(defun vm-pop-net-open (source name)
  "Open a connection for the POP maildrop SOURCE and answer with a session.

NAME goes in messages.  The session has a process and a buffer of its own
and is ready for `vm-net-start\='; nothing has been read from it yet.

Plain connections only for now.  A pop-ssl maildrop is opened through
`open-network-stream\=' with :type tls, which negotiates before it returns,
and a pop-ssh one runs a tunnel program: both are still waits, and both are
converted with the connect path rather than here.  Such a maildrop signals
`vm-pop-net-unsupported\=', which is a caller\='s cue to use the blocking
implementation."
  (let* ((parts (vm-pop-parse-spec-to-list source))
	 (protocol (car parts))
	 (host (nth 1 parts))
	 (port (nth 2 parts))
	 (user (nth 4 parts))
	 (password (nth 5 parts)))
    (unless (member protocol '("pop" "pop-ssl"))
      (signal 'vm-pop-net-unsupported (list protocol source)))
    (when (and (stringp port) (string-match "\\`[0-9]+\\'" port))
      (setq port (string-to-number port)))
    (when (equal password "*")
      (signal 'vm-pop-net-unsupported (list "password not remembered" source)))
    (let* ((buffer (generate-new-buffer (format " *%s*" name)))
	   ;; :nowait, so the connect itself does not wait either.  The
	   ;; process is not open when this returns; the session's sentinel
	   ;; hears whether it ever will be, and its timeout covers a connect
	   ;; that never completes.  TLS is negotiated the same way, Emacs
	   ;; doing the handshake as the connection comes up.
	   (process (make-network-process
		     :name name :host host :service port :buffer buffer
		     :noquery t :coding 'binary :nowait t
		     :type (if (equal protocol "pop-ssl") 'tls nil))))
      (with-current-buffer buffer
	(buffer-disable-undo)
	(vm-pop-net-init))
      (list (vm-net-session :process process :name name
			    :timeout vm-pop-server-timeout)
	    user password))))

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

(defun vm-pop-net-check-mail (source callback)
  "Ask SOURCE whether it has mail VM has not retrieved, and tell CALLBACK.

CALLBACK is called with t, nil, or the error that stopped the session.  It
is called from the process filter, so the folder buffer it wants is the one
it remembers, not the one that happens to be current.

Nothing waits: this returns as soon as the connection is made."
  (let* ((retrieved vm-pop-retrieved-messages)
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

(provide 'vm-pop-net)
;;; vm-pop-net.el ends here
