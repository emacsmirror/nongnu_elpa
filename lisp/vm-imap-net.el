;;; vm-imap-net.el --- IMAP over the non-blocking driver  -*- lexical-binding: t; -*-
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

;; IMAP's reader as generators, on the driver in vm-net.el.  What
;; `vm-imap-read-object' does in vm-imap.el, waiting inside
;; `accept-process-output' whenever it wants more than the buffer holds --
;; four such waits, and 51 call sites above them that block by transitivity.
;;
;; The tokens are the ones vm-imap.el's parser already produces, positions in
;; the process buffer and all:
;;
;;   (end-of-line)  (atom START END)  (string START END)
;;   (vector TOKEN...)  (list TOKEN...)
;;   (close-bracket)  (close-paren)  (close-brace)
;;
;; So `vm-imap-response-matches' and everything built on it work unchanged on
;; what this returns.  Only the waiting is different.
;;
;; The recursion is `iter-yield-from': a generator resuming a generator, which
;; is what lets a bracketed list wait for its own contents without a stack of
;; callbacks.  See dev/docs/design/async-imap.org.

;;; Code:

(require 'vm-net)
(require 'generator)

(eval-when-compile (require 'vm-misc))

(declare-function vm-imap-protocol-error "vm-imap" (&rest args))
(declare-function vm-imap-normal-error "vm-imap" (&rest args))
(declare-function vm-imap-response-matches "vm-imap" (response &rest expr))
(declare-function vm-warn "vm-misc" (l secs &rest args))

(defvar vm-imap-tolerant-of-bad-imap)
(defvar vm-imap-current-tag)

(defvar vm-imap-net-read-point nil
  "Where in the process buffer the next response begins.
Buffer-local to a session's process buffer, as `vm-imap-read-point\\=' is for
the blocking implementation.")
(make-variable-buffer-local 'vm-imap-net-read-point)

(defvar vm-imap-net-tag 0
  "The number of the last tag this session sent.")
(make-variable-buffer-local 'vm-imap-net-tag)

(defun vm-imap-net-init ()
  "Prepare the current buffer to be a session's process buffer."
  (setq vm-imap-net-read-point (point-min))
  (setq vm-imap-net-tag 0)
  ;; what `VM' in a response pattern is matched against
  (set (make-local-variable 'vm-imap-current-tag) nil))

;;; Reading

(iter-defun vm-imap-net-read-object (&optional skip-eol)
  "Read one token and answer with it, yielding until it is all here.

SKIP-EOL means an end-of-line is a token like any other rather than the end
of the read, which is what the bracketed and parenthesised lists want.

The waits are where `vm-imap-read-object' calls `accept-process-output': too
little in the buffer to tell what is coming, the octets of a literal, the
closing quote of a quoted string, and the terminator of an atom."
  (let ((done nil)
	(token nil))
    (unwind-protect
	(while (not done)
	  (skip-chars-forward " \t")
	  (cond
	   ((< (- (point-max) (point)) 2)
	    (let ((opoint (point)))
	      (iter-yield (vm-net-request-growth))
	      (goto-char opoint)))
	   ((looking-at "\r\n")
	    (forward-char 2)
	    (setq token '(end-of-line) done (not skip-eol)))
	   ((looking-at "\n")
	    (vm-warn 0 2
		     "missing CR before LF - IMAP connection may have a problem")
	    (forward-char 1)
	    (setq token '(end-of-line) done (not skip-eol)))
	   ((looking-at "\\[")
	    (forward-char 1)
	    (setq token (iter-yield-from (vm-imap-net-read-group 'vector))
		  done t))
	   ((looking-at "\\]")
	    (forward-char 1)
	    (setq token '(close-bracket) done t))
	   ((looking-at "(")
	    (forward-char 1)
	    (setq token (iter-yield-from (vm-imap-net-read-group 'list))
		  done t))
	   ((looking-at ")")
	    (forward-char 1)
	    (setq token '(close-paren) done t))
	   ((looking-at "{")
	    (forward-char 1)
	    (setq token (iter-yield-from (vm-imap-net-read-literal))
		  done t))
	   ((looking-at "}")
	    (forward-char 1)
	    (setq token '(close-brace) done t))
	   ((looking-at "\042")
	    (forward-char 1)
	    (setq token (iter-yield-from (vm-imap-net-read-quoted))
		  done t))
	   ;; should be "[\000-\040\177-\377]", but Microsoft Exchange emits
	   ;; 8-bit characters despite the RFC 2060 prohibition
	   ((and (looking-at "[\000-\040\177]")
		 (= vm-imap-tolerant-of-bad-imap 0))
	    (vm-imap-protocol-error "illegal char (%d)" (char-after (point))))
	   (t
	    (setq token (iter-yield-from (vm-imap-net-read-atom))
		  done t))))
      (setq vm-imap-net-read-point (point)))
    token))

(iter-defun vm-imap-net-read-group (kind)
  "Read tokens until this group's closing bracket, and answer with the group.
KIND is `vector' for one opened with [ and `list' for one opened with (."
  (let* ((closer (if (eq kind 'vector) 'close-bracket 'close-paren))
	 (wrong (if (eq kind 'vector) 'close-paren 'close-bracket))
	 (group (list kind))
	 (tail group)
	 (object (iter-yield-from (vm-imap-net-read-object t))))
    (while (not (eq (car object) closer))
      (when (eq (car object) wrong)
	(vm-imap-protocol-error "unexpected %s"
				(if (eq wrong 'close-paren) ")" "]")))
      (setcdr tail (list object))
      (setq tail (cdr tail))
      (setq object (iter-yield-from (vm-imap-net-read-object t))))
    group))

(iter-defun vm-imap-net-read-literal ()
  "Read a {n} literal, the { having been read, and answer with its string.
Waits for the whole of it in one go: the request is the position the octets
end at, so a body arriving in a thousand chunks resumes this once."
  (let ((object (iter-yield-from (vm-imap-net-read-object)))
	(octets nil)
	(start nil))
    (unless (and (eq (car object) 'atom)
		 (string-match "\\`[0-9]+\\'"
			       (buffer-substring (nth 1 object) (nth 2 object))))
      ;; gmail sometimes puts random strings in braces, which cannot be taken
      ;; for a count
      (vm-imap-protocol-error "number expected after {"))
    (setq octets (string-to-number
		  (buffer-substring (nth 1 object) (nth 2 object))))
    (setq object (iter-yield-from (vm-imap-net-read-object)))
    (unless (eq (car object) 'close-brace)
      (vm-imap-protocol-error "} expected"))
    (setq object (iter-yield-from (vm-imap-net-read-object)))
    (unless (eq (car object) 'end-of-line)
      (vm-imap-protocol-error "CRLF expected"))
    (setq start (point))
    (while (< (- (point-max) start) octets)
      (iter-yield (vm-net-request-position (+ start octets))))
    (goto-char (+ start octets))
    (list 'string start (point))))

(iter-defun vm-imap-net-read-quoted ()
  "Read a quoted string, the opening quote having been read."
  (let ((start (point))
	(done nil)
	(end nil))
    (while (not done)
      (skip-chars-forward "^\042")
      (setq end (point))
      (if (looking-at "\042")
	  (progn (setq done t)
		 (forward-char 1))
	(iter-yield (vm-net-request-growth))
	(goto-char end)))
    (list 'string start end)))

(iter-defun vm-imap-net-read-atom ()
  "Read an atom, up to the first character that cannot be part of one."
  ;; 8-bit characters should be non-word characters here, but Microsoft
  ;; Exchange puts them in atoms
  (let ((start (point))
	(not-word-chars "^\000-\040\177()[]{}")
	(not-word-regexp "[][\000-\040\177(){}]")
	(done nil)
	(end nil))
    (while (not done)
      (skip-chars-forward not-word-chars)
      (setq end (point))
      (if (looking-at not-word-regexp)
	  (setq done t)
	(iter-yield (vm-net-request-growth))
	(goto-char end)))
    (list 'atom start end)))

(iter-defun vm-imap-net-read-response ()
  "Read one line of response and answer with its tokens.
An ill-formed line answers with an empty list, as the blocking reader does."
  (let ((tokens nil)
	(tail nil)
	(object nil)
	(done nil))
    (goto-char vm-imap-net-read-point)
    (while (not done)
      (setq object (iter-yield-from (vm-imap-net-read-object)))
      (if (eq (car object) 'end-of-line)
	  (setq done t)
	(if (null tokens)
	    (setq tokens (list object)
		  tail tokens)
	  (setcdr tail (list object))
	  (setq tail (cdr tail)))))
    tokens))

(defun vm-imap-net-error-message (position)
  "The server's error text in the process buffer, starting at POSITION."
  (buffer-substring position
		    (save-excursion
		      (goto-char position)
		      (if (search-forward "\r\n" (point-max) t)
			  (- (point) 2)
			(point-max)))))

(iter-defun vm-imap-net-read-response-and-verify (&optional description)
  "Read one response and answer with it, signalling on NO, BAD or BYE.
DESCRIPTION names the command, for the error message."
  (let ((response (iter-yield-from (vm-imap-net-read-response))))
    (when response
      (when (or (vm-imap-response-matches response 'VM 'NO)
		(vm-imap-response-matches response 'VM 'BAD))
	(vm-imap-normal-error
	 "server says - %s"
	 (vm-imap-net-error-message (cadr (cadr response)))))
      (when (vm-imap-response-matches response '* 'BYE)
	(vm-imap-normal-error "server disconnected%s"
			      (if description
				  (format " during %s" description) ""))))
    response))

(iter-defun vm-imap-net-read-ok-response ()
  "Read responses until the tagged one, and answer with whether it was OK."
  (let ((done nil)
	(answer nil)
	response)
    (while (not done)
      (setq response (iter-yield-from (vm-imap-net-read-response)))
      (cond ((vm-imap-response-matches response '*)
	     nil)
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq answer t done t))
	    ((vm-imap-response-matches response 'VM 'NO)
	     (setq answer nil done t))
	    ((vm-imap-response-matches response 'VM 'BAD)
	     (vm-imap-normal-error
	      "server says - %s"
	      (vm-imap-net-error-message (cadr (cadr response)))))
	    (t
	     (vm-imap-protocol-error "Did not receive OK response"))))
    answer))

;;; Sending

(defun vm-imap-net-next-tag ()
  "The tag for the next command of this session, and remember it.
Remembered because `VM' in a response pattern means the tag of the command
being waited for, which is what `vm-imap-response-matches\\=' reads out of
`vm-imap-current-tag\\='."
  (setq vm-imap-current-tag
	(format "vm%d" (setq vm-imap-net-tag (1+ vm-imap-net-tag)))))

(defun vm-imap-net-send (command &optional tag)
  "Send COMMAND, tagged, and note where its answer begins.
Answers with the tag it used.  The command is echoed into the process buffer
the way the blocking implementation echoes it, so a session's buffer reads as
a transcript -- with a LOGIN's arguments left out of it."
  (let ((process (get-buffer-process (current-buffer)))
	(tag (or tag (vm-imap-net-next-tag))))
    (goto-char (point-max))
    (insert-before-markers
     tag " "
     (if (let ((case-fold-search t)) (string-match "\\`LOGIN" command))
	 "LOGIN <parameters omitted>"
       command)
     "\r\n")
    (setq vm-imap-net-read-point (point))
    (process-send-string process (format "%s %s\r\n" tag command))
    tag))

(iter-defun vm-imap-net-command (command &optional description)
  "Send COMMAND and answer with every response line up to its tagged one.
The tagged line is the last of them, so a caller that wants only whether it
worked can look at that, and one that wants the untagged data has it in
order."
  (vm-imap-net-send command)
  (let ((lines nil)
	(done nil)
	response)
    (while (not done)
      (setq response
	    (iter-yield-from (vm-imap-net-read-response-and-verify
			      (or description command))))
      (push response lines)
      (when (vm-imap-response-matches response 'VM 'OK)
	(setq done t)))
    (nreverse lines)))

;;; The start of a session

(iter-defun vm-imap-net-greeting ()
  "Read the server's greeting.
Answers t for OK, `preauth' for PREAUTH, and nil for anything else, which is
what `vm-imap-read-greeting' answers."
  (let ((response (iter-yield-from (vm-imap-net-read-response))))
    (cond ((vm-imap-response-matches response '* 'OK) t)
	  ((vm-imap-response-matches response '* 'PREAUTH) 'preauth)
	  (t nil))))

(iter-defun vm-imap-net-capabilities ()
  "Ask what the server can do.
Answers (CAPABILITIES AUTHENTICATIONS), both lists of symbols, as
`vm-imap-read-capability-response' does."
  (let ((lines (iter-yield-from (vm-imap-net-command "CAPABILITY")))
	(capabilities nil)
	(authentications nil))
    (dolist (response lines)
      (when (vm-imap-response-matches response '* 'CAPABILITY)
	(dolist (token (cddr response))
	  (when (eq (car token) 'atom)
	    (let ((text (buffer-substring (nth 1 token) (nth 2 token))))
	      (if (let ((case-fold-search t)) (string-match "\\`AUTH=." text))
		  (push (intern (upcase (substring text 5))) authentications)
		(push (intern (upcase text)) capabilities)))))))
    (list (nreverse capabilities) (nreverse authentications))))

(defun vm-imap-net-quote (string)
  "STRING as an IMAP quoted string."
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" string) "\""))

(iter-defun vm-imap-net-login (user password)
  "Log in as USER, and answer with what the server can do afterwards.
The capabilities are asked for again: a server may advertise more once the
connection is authenticated, and several advertise fewer before it."
  (iter-yield-from (vm-imap-net-command
		    (format "LOGIN %s %s"
			    (vm-imap-net-quote user)
			    (vm-imap-net-quote password))
		    "LOGIN"))
  (iter-yield-from (vm-imap-net-capabilities)))

;;; A mailbox

(declare-function vm-imap-quote-mailbox-name "vm-imap" (mailbox))
(declare-function vm-imap-scan-list-for-flag "vm-imap" (list flag))
(declare-function vm-inform "vm-misc" (level &rest args))

(defun vm-imap-net-number (token)
  "The number TOKEN is, read from the process buffer."
  (string-to-number (buffer-substring (nth 1 token) (nth 2 token))))

(iter-defun vm-imap-net-select (mailbox &optional examine)
  "Select MAILBOX, or EXAMINE it, and answer with what the server said of it.
The answer is (COUNT RECENT UID-VALIDITY READ-WRITE CAN-DELETE
PERMANENT-FLAGS), which is `vm-imap-select-mailbox\\='s."
  (let* ((command (if examine "EXAMINE" "SELECT"))
	 (lines (iter-yield-from
		 (vm-imap-net-command
		  (format "%s %s" command (vm-imap-quote-mailbox-name mailbox))
		  command)))
	 (count nil) (recent nil) (uid-validity nil)
	 (read-write (not examine)) (flags nil) (permanent-flags nil))
    (dolist (response lines)
      (cond ((vm-imap-response-matches response '* 'OK 'vector)
	     (let ((contents (cdr (nth 2 response))))
	       (cond ((vm-imap-response-matches contents 'UIDVALIDITY 'atom)
		      (let ((token (nth 1 contents)))
			(setq uid-validity
			      (buffer-substring (nth 1 token) (nth 2 token)))))
		     ((vm-imap-response-matches contents 'PERMANENTFLAGS 'list)
		      (setq permanent-flags (nth 1 contents))))))
	    ((vm-imap-response-matches response '* 'FLAGS 'list)
	     (setq flags (nth 2 response)))
	    ((vm-imap-response-matches response '* 'atom 'EXISTS)
	     (setq count (vm-imap-net-number (nth 1 response))))
	    ((vm-imap-response-matches response '* 'atom 'RECENT)
	     (setq recent (vm-imap-net-number (nth 1 response))))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-WRITE))
	     (setq read-write t))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-ONLY))
	     (setq read-write nil))))
    (unless flags
      (vm-imap-protocol-error "FLAGS missing from %s responses" command))
    (unless count
      (vm-imap-protocol-error "EXISTS missing from %s responses" command))
    (unless uid-validity
      (vm-imap-protocol-error "UIDVALIDITY missing from %s responses" command))
    (list count recent uid-validity read-write
	  (and (vm-imap-scan-list-for-flag flags "\\Deleted") t)
	  permanent-flags)))

;;; What is in it

(iter-defun vm-imap-net-message-data (first last)
  "Ask for the UID, size and flags of the messages FIRST to LAST.
Answers an alist of (SEQUENCE-NUMBER UID SIZE . FLAGS), which is what
`vm-imap-get-message-data-list\\=' answers, newest first."
  (let ((lines (iter-yield-from
		(vm-imap-net-command
		 (format "FETCH %s:%s (UID RFC822.SIZE FLAGS)" first last)
		 "FETCH")))
	(data nil))
    (dolist (response lines)
      (when (vm-imap-response-matches response '* 'atom 'FETCH 'list)
	(let ((number (vm-imap-net-number (nth 1 response)))
	      (contents (cdr (nth 3 response)))
	      (uid nil) (size nil) (flags nil))
	  (while contents
	    (cond
	     ((vm-imap-response-matches contents 'UID 'atom)
	      (let ((token (nth 1 contents)))
		(setq uid (buffer-substring (nth 1 token) (nth 2 token))))
	      (setq contents (nthcdr 2 contents)))
	     ((vm-imap-response-matches contents 'RFC822\.SIZE 'atom)
	      (let ((token (nth 1 contents)))
		(setq size (buffer-substring (nth 1 token) (nth 2 token))))
	      (setq contents (nthcdr 2 contents)))
	     ((vm-imap-response-matches contents 'FLAGS 'list)
	      (dolist (token (cdr (nth 1 contents)))
		(unless (eq (car token) 'atom)
		  (vm-imap-protocol-error
		   "expected atom in FLAGS list in FETCH response"))
		(push (downcase (buffer-substring (nth 1 token) (nth 2 token)))
		      flags))
	      (setq contents (nthcdr 2 contents)))
	     (t
	      (vm-imap-protocol-error
	       "expected UID, RFC822.SIZE and (FLAGS list) in FETCH response"))))
	  (push (cons number (cons uid (cons size (nreverse flags)))) data))))
    data))

(defun vm-imap-net-fetch-items (body-peek headers-only)
  "What to ask a FETCH for, as `vm-imap-fetch-messages\\=' asks for it.
The UID comes back with the body because a server may answer for a range in
any order, and the responses have to be told apart (issue #185)."
  (cond ((and headers-only body-peek) "(UID BODY.PEEK[HEADER])")
	(headers-only "(UID RFC822.HEADER)")
	(body-peek "(UID BODY.PEEK[])")
	(t "(UID RFC822.PEEK)")))

(defun vm-imap-net-fetch-message-text (response)
  "Where in the process buffer RESPONSE's message is: (UID START END).
Signals unless RESPONSE is a FETCH carrying a UID and one string."
  (let ((contents (cdr (nth 3 response)))
	(uid nil) (text nil))
    (while contents
      (cond ((vm-imap-response-matches contents 'UID 'atom)
	     (let ((token (nth 1 contents)))
	       (setq uid (buffer-substring (nth 1 token) (nth 2 token))))
	     (setq contents (nthcdr 2 contents)))
	    ((vm-imap-response-matches contents 'atom 'string)
	     (setq text (nth 1 contents))
	     (setq contents (nthcdr 2 contents)))
	    ((vm-imap-response-matches contents 'atom '(vector) 'string)
	     (setq text (nth 2 contents))
	     (setq contents (nthcdr 3 contents)))
	    (t
	     (vm-imap-protocol-error "unexpected FETCH response contents"))))
    (unless (and uid text)
      (vm-imap-protocol-error "expected a UID and a message in FETCH response"))
    (list uid (nth 1 text) (nth 2 text))))

(iter-defun vm-imap-net-fetch (first last body-peek headers-only store)
  "Fetch messages FIRST to LAST, handing each to STORE as it arrives.
STORE is called in the process buffer with the message's UID and the
positions its text lies between, so it can copy the message out without
another one being made of it first.  It is called before the next message is
read, which is what keeps a mailbox of any size out of memory."
  (vm-imap-net-send (format "FETCH %s:%s %s" first last
			    (vm-imap-net-fetch-items body-peek headers-only)))
  (let ((done nil)
	(count 0)
	response)
    (while (not done)
      (setq response (iter-yield-from
		      (vm-imap-net-read-response-and-verify "FETCH")))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (let ((message (vm-imap-net-fetch-message-text response)))
	       (apply store message)
	       (setq count (1+ count))))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq done t))))
    count))

;;; Connecting

(declare-function vm-parse "vm-misc" (string regexp &optional matchn matches))
(declare-function vm-binary-coding-system "vm-misc" ())

(defvar vm-imap-server-timeout)

(define-error 'vm-imap-net-unsupported "IMAP maildrop VM cannot open without waiting")

(defun vm-imap-net-open (source name)
  "Open a connection for the IMAP maildrop SOURCE and answer with a session.

NAME goes in messages.  The session has a process and a buffer of its own
and is ready for `vm-net-start\\='; nothing has been read from it yet.  The
answer is (SESSION MAILBOX USER PASSWORD).

An imap-ssh maildrop runs a tunnel program and a preauth one runs a hook,
both of which wait; either signals `vm-imap-net-unsupported\\=', as does a
maildrop whose password VM has not been told, since there is nobody to ask
from inside a filter.  imap-ssl connects with :type tls, Emacs doing the
handshake as the connection comes up."
  (let* ((parts (vm-parse source "\\([^:]*\\):?" 1 7))
	 (protocol (car parts))
	 (host (nth 1 parts))
	 (port (nth 2 parts))
	 (mailbox (nth 3 parts))
	 (auth (nth 4 parts))
	 (user (nth 5 parts))
	 (password (nth 6 parts)))
    (unless (member protocol '("imap" "imap-ssl"))
      (signal 'vm-imap-net-unsupported (list protocol source)))
    (unless (equal auth "login")
      (signal 'vm-imap-net-unsupported (list (or auth "no authentication") source)))
    (when (equal password "*")
      (signal 'vm-imap-net-unsupported (list "password not remembered" source)))
    (when (and (stringp port) (string-match "\\`[0-9]+\\'" port))
      (setq port (string-to-number port)))
    (let* ((buffer (generate-new-buffer (format " *%s*" name)))
	   (process (make-network-process
		     :name name :host host :service port :buffer buffer
		     :noquery t :coding 'binary :nowait t
		     :type (if (equal protocol "imap-ssl") 'tls nil))))
      (with-current-buffer buffer
	(buffer-disable-undo)
	(vm-imap-net-init))
      (list (vm-net-session :process process :name name
			    :timeout vm-imap-server-timeout)
	    mailbox user password))))

(iter-defun vm-imap-net-open-session (user password)
  "Greet, log in, and answer with what the server says it can do.
Answers (CAPABILITIES AUTHENTICATIONS).  A greeting that is neither OK nor
PREAUTH signals: there is no session to be had, and the caller has nothing
to decide."
  (let ((greeting (iter-yield-from (vm-imap-net-greeting))))
    (cond ((null greeting)
	   (vm-imap-normal-error "server did not greet the connection"))
	  ((eq greeting 'preauth)
	   (iter-yield-from (vm-imap-net-capabilities)))
	  (t
	   (iter-yield-from (vm-imap-net-capabilities))
	   (iter-yield-from (vm-imap-net-login user password))))))

;;; Getting new mail into a folder

(declare-function vm-imap-cleanup-region "vm-imap" (start end))
(declare-function vm-imap-bunch-retrieve-list "vm-imap" (retrieve-list))
(declare-function vm-imap-get-synchronization-data "vm-imap" (&optional do-retrieves))
(declare-function vm-imap-update-message-flags "vm-imap" (m flags &optional norecord))
(declare-function vm-folder-imap-uid-message-size "vm-imap" (uid))
(declare-function vm-folder-imap-uid-message-flags "vm-imap" (uid))
(declare-function vm-folder-imap-maildrop-spec "vm-folder" ())
(declare-function vm-folder-imap-uid-validity "vm-folder" ())
(declare-function vm-set-folder-imap-uid-validity "vm-folder" (value))
(declare-function vm-set-folder-imap-mailbox-count "vm-folder" (value))
(declare-function vm-folder-imap-retrieved-count "vm-folder" ())
(declare-function vm-set-folder-imap-retrieved-count "vm-folder" (value))
(declare-function vm-set-folder-imap-recent-count "vm-folder" (value))
(declare-function vm-set-folder-imap-read-write "vm-folder" (value))
(declare-function vm-set-folder-imap-can-delete "vm-folder" (value))
(declare-function vm-set-folder-imap-body-peek "vm-folder" (value))
(declare-function vm-set-folder-imap-permanent-flags "vm-folder" (value))
(declare-function vm-set-folder-imap-uid-list "vm-folder" (value))
(declare-function vm-set-folder-imap-uid-obarray "vm-folder" (value))
(declare-function vm-set-folder-imap-flags-obarray "vm-folder" (value))
(declare-function vm-folder-imap-mailbox-count "vm-folder" ())
(declare-function vm-munge-message-separators "vm-folder" (folder-type start end))
(declare-function vm-leading-message-separator "vm-folder" (&optional folder-type message for-other-folder))
(declare-function vm-trailing-message-separator "vm-folder" (&optional folder-type))
(declare-function vm-convert-folder-type-headers "vm-folder" (old new))
(declare-function vm-assimilate-new-messages "vm-folder" (&rest keys))
(declare-function vm-update-summary-and-mode-line "vm-summary" ())
(declare-function vm-mark-for-summary-update "vm-summary" (m &optional dont-kill-cache))
(declare-function vm-set-imap-uid-of "vm-message" (m uid))
(declare-function vm-set-imap-uid-validity-of "vm-message" (m validity))
(declare-function vm-set-byte-count-of "vm-message" (m count))
(declare-function vm-set-stuff-flag-of "vm-message" (m flag))
(declare-function vm-set-body-to-be-retrieved-of "vm-message" (m flag))
(declare-function vm-set-body-to-be-discarded-of "vm-message" (m flag))
(declare-function vm-run-hook-on-message "vm-misc" (hook message))

(defvar vm-folder-type)
(defvar vm-default-folder-type)
(defvar vm-message-list)
(defvar vm-spooled-mail-waiting)
(defvar vm-buffers-needing-display-update)
(defvar vm-modification-counter)
(defvar vm-arrived-message-hook)
(defvar vm-imap-max-message-size)
(defvar vm-enable-external-messages)

(defun vm-imap-net-install-message-data (data count)
  "Put DATA, from `vm-imap-net-message-data\\=', into the folder's own tables.
The current buffer is the folder.  What
`vm-imap-retrieve-uid-and-flags-data\\=' installs after its own blocking
fetch -- and having installed it, that function finds the list already there
and asks for nothing, which is what lets the synchronisation code below run
unchanged.

The obarrays are sized for the mailbox rather than fixed at 67 buckets: a
mailbox of 100,000 messages put 1,500 symbols in each of those buckets, and
every lookup walked one."
  (let* ((buckets (max 67 (/ count 4)))
	 (uids (make-vector buckets 0))
	 (flags (make-vector buckets 0)))
    (dolist (tuple data)
      (set (intern (cadr tuple) uids) (car tuple))
      (set (intern (cadr tuple) flags) (nthcdr 2 tuple)))
    (vm-set-folder-imap-uid-list data)
    (vm-set-folder-imap-uid-obarray uids)
    (vm-set-folder-imap-flags-obarray flags)))

(defun vm-imap-net-plan (data count)
  "Work out what has to be fetched, and answer with (RETRIEVE-LIST BUNCHES).
The current buffer is the folder.  RETRIEVE-LIST is (UID SEQUENCE-NUMBER
HEADERS-ONLY) per message, in the order the messages will arrive; BUNCHES is
what to ask the server for, `vm-imap-message-bunch-size\\=' at a time."
  (vm-imap-net-install-message-data data count)
  (let* ((sync (vm-imap-get-synchronization-data t))
	 (headers-only (or (eq vm-enable-external-messages t)
			   (memq 'imap vm-enable-external-messages)))
	 (limit (or vm-imap-max-message-size most-positive-fixnum))
	 (retrieve-list
	  (mapcar (lambda (pair)
		    (let ((size (string-to-number
				 (or (vm-folder-imap-uid-message-size (car pair))
				     "0"))))
		      (list (car pair) (cdr pair)
			    (and (> size limit) headers-only))))
		  (nth 0 sync))))
    (list retrieve-list
	  (vm-imap-bunch-retrieve-list (mapcar #'cdr retrieve-list)))))

(defun vm-imap-net-store (folder folder-type source start end)
  "Copy the message between START and END of SOURCE into FOLDER.
The same cleaning up the blocking path does, in the same order: CRLF to LF,
the separators the folder's own type wants, and the headers that go with
them."
  (with-current-buffer folder
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(let ((buffer-read-only nil)	; a folder buffer is read-only
	      (start-of-message (point))
	      (end-of-message nil))
	  (insert-buffer-substring source start end)
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (setq end-of-message (point-marker))
	  (vm-imap-cleanup-region start-of-message end-of-message)
	  (vm-munge-message-separators folder-type start-of-message
				       end-of-message)
	  (goto-char start-of-message)
	  (insert (vm-leading-message-separator folder-type))
	  (save-restriction
	    (narrow-to-region (point) end-of-message)
	    (vm-convert-folder-type-headers 'baremessage folder-type))
	  (goto-char end-of-message)
	  (insert-before-markers (vm-trailing-message-separator folder-type))
	  (set-marker end-of-message nil))))))

(defun vm-imap-net-assimilate (retrieve-list uid-validity)
  "Take the messages just written into the folder into the message list.
The current buffer is the folder.  Answers with the new messages.  Each is
given the UID it was fetched under and the flags the server reported for it,
which is what `vm-imap-retrieve-messages\\=' does at the end of its own
loop."
  (setq vm-spooled-mail-waiting nil)
  (vm-set-folder-imap-retrieved-count (vm-folder-imap-mailbox-count))
  (intern (buffer-name) vm-buffers-needing-display-update)
  (let* ((new-messages (vm-assimilate-new-messages :read-attributes nil))
	 (messages new-messages)
	 (entries retrieve-list))
    (when new-messages
      (setq vm-modification-counter (1+ vm-modification-counter)))
    (while messages
      (let* ((message (car messages))
	     (entry (car entries))
	     (uid (car entry)))
	(when (nth 2 entry)
	  (vm-set-body-to-be-retrieved-of message t)
	  (vm-set-body-to-be-discarded-of message nil))
	(vm-set-imap-uid-of message uid)
	(vm-set-imap-uid-validity-of message uid-validity)
	(vm-set-byte-count-of message (vm-folder-imap-uid-message-size uid))
	(vm-imap-update-message-flags
	 message (vm-folder-imap-uid-message-flags uid) t)
	(vm-mark-for-summary-update message)
	(vm-set-stuff-flag-of message t))
      (setq messages (cdr messages)
	    entries (cdr entries)))
    (vm-update-summary-and-mode-line)
    (when vm-arrived-message-hook
      (dolist (message new-messages)
	(vm-run-hook-on-message 'vm-arrived-message-hook message)))
    (run-hooks 'vm-arrived-messages-hook)
    new-messages))

(iter-defun vm-imap-net-get-new-mail (folder mailbox user password)
  "Fetch what FOLDER has not got from MAILBOX, and answer with how many.
The messages are written into FOLDER as they arrive, a bunch at a time; the
folder takes them into its message list once they are all there, as the
blocking path does."
  (let* ((capabilities (iter-yield-from (vm-imap-net-open-session user password)))
	 (body-peek (and (memq 'IMAP4REV1 (car capabilities)) t))
	 (select (iter-yield-from (vm-imap-net-select mailbox)))
	 (count (nth 0 select))
	 (uid-validity (nth 2 select))
	 (source (current-buffer))
	 (folder-type nil)
	 (data nil)
	 (plan nil)
	 (retrieved 0))
    (with-current-buffer folder
      (let ((known (vm-folder-imap-uid-validity)))
	(when (and known uid-validity (not (equal known uid-validity)))
	  ;; The blocking path asks whether to refresh the cache.  There is
	  ;; nobody to ask from inside a filter, and going on regardless would
	  ;; fetch every message again under UIDs that mean something else.
	  (vm-imap-normal-error
	   "UID VALIDITY of %s has changed on the server; refresh it with vm-imap-synchronize"
	   mailbox)))
      (setq folder-type (or vm-folder-type vm-default-folder-type))
      (vm-set-folder-imap-uid-validity uid-validity)
      (vm-set-folder-imap-mailbox-count count)
      (unless (vm-folder-imap-retrieved-count)
	(vm-set-folder-imap-retrieved-count count))
      (vm-set-folder-imap-recent-count (nth 1 select))
      (vm-set-folder-imap-read-write (nth 3 select))
      (vm-set-folder-imap-can-delete (nth 4 select))
      (vm-set-folder-imap-body-peek body-peek)
      (vm-set-folder-imap-permanent-flags (nth 5 select)))
    (setq data (if (zerop count)
		   nil
		 (iter-yield-from (vm-imap-net-message-data 1 count))))
    (setq plan (with-current-buffer folder (vm-imap-net-plan data count)))
    (let ((retrieve-list (nth 0 plan))
	  (bunches (nth 1 plan)))
      (dolist (bunch bunches)
	(let* ((range (car bunch))
	       (headers-only (cadr bunch))
	       (store (lambda (_uid start end)
			(vm-imap-net-store folder folder-type source start end))))
	  (iter-yield-from
	   (vm-imap-net-fetch (car range) (cdr range) body-peek headers-only
			      store))
	  (setq retrieved (+ retrieved (1+ (- (cdr range) (car range)))))
	  (vm-inform 6 "%s: %d of %d messages"
		     (buffer-name folder) retrieved (length retrieve-list))))
      (with-current-buffer folder
	(vm-imap-net-assimilate retrieve-list uid-validity))
      retrieved)))

(defun vm-imap-net-get-mail (source callback)
  "Fetch into the current folder what SOURCE has that it has not, and
tell CALLBACK.

CALLBACK is called in the folder buffer with the number of messages
fetched, or with the error that stopped the session.  Nothing waits: the
whole of it happens in the process filter, and the folder is left usable
while it does.

Signals `vm-imap-net-unsupported\\=' for a maildrop this cannot open, which
is a caller\\='s cue to use the blocking implementation."
  (let* ((folder (current-buffer))
	 (opened (vm-imap-net-open source "IMAP fetch"))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (when (buffer-live-p buffer) (kill-buffer buffer))
	    (when (buffer-live-p folder)
	      (with-current-buffer folder
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))))
    (vm-net-start session
		  (vm-imap-net-get-new-mail folder (nth 1 opened) (nth 2 opened)
					    (nth 3 opened)))
    session))

(provide 'vm-imap-net)
;;; vm-imap-net.el ends here
