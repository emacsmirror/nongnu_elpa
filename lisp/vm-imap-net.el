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
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-imap-protocol-error "vm-imap" (&rest args))
(declare-function vm-imap-normal-error "vm-imap" (&rest args))
(declare-function vm-imap-response-matches "vm-imap" (response &rest expr))
(declare-function vm-imap-skip-fetch-item "vm-imap" (contents))
(declare-function vm-imap-fetch-response-parts "vm-imap" (contents))
(declare-function vm-warn "vm-misc" (l secs &rest args))

(defvar vm-imap-tolerant-of-bad-imap)
(defvar vm-imap-current-tag)
(defvar vm-imap-ok-to-ask)

(defvar vm-imap-net-read-point nil
  "Where in the process buffer the next response begins.
Buffer-local to a session's process buffer.")
(make-variable-buffer-local 'vm-imap-net-read-point)

(defvar vm-imap-net-tag 0
  "The number of the last tag this session sent.")
(make-variable-buffer-local 'vm-imap-net-tag)

(defvar vm-imap-net-password-key nil
  "The maildrop this session's password belongs to, without its password.
What `vm-imap-passwords' is keyed by, kept so that a password the server
has accepted can be remembered under it.")
(make-variable-buffer-local 'vm-imap-net-password-key)

(defvar vm-imap-net-auth nil
  "The authentication method this session's maildrop asked for.
A string: login, cram-md5 or preauth.  Buffer-local to the session's process
buffer, so that vm-imap-net-open-session can choose without every caller
having to hand it on.")
(make-variable-buffer-local 'vm-imap-net-auth)

(defun vm-imap-net-init ()
  "Prepare the current buffer to be a session's process buffer."
  (setq vm-imap-net-read-point (point-min))
  (setq vm-imap-net-tag 0)
  ;; what `VM' in a response pattern is matched against
  (set (make-local-variable 'vm-imap-current-tag) nil))

;;; Reading

(defun vm-imap-net-parse-token ()
  "Parse the token at point, whatever it is, and answer with it.
Throws `vm-imap-net-need\\=' with a request when the buffer does not hold the
whole of it -- too little in it to tell what is coming, the octets of a
literal, the closing quote of a quoted string, the terminator of an atom:
the four places `vm-imap-read-object\\=' calls `accept-process-output\\='."
  (cond
   ((< (- (point-max) (point)) 2)
    (throw 'vm-imap-net-need (vm-net-request-growth)))
   ((looking-at "\r\n")
    (forward-char 2)
    '(end-of-line))
   ((looking-at "\n")
    (vm-net-warn 0
	     "missing CR before LF - IMAP connection may have a problem")
    (forward-char 1)
    '(end-of-line))
   ((looking-at "\\[")
    (forward-char 1)
    (vm-imap-net-parse-group 'vector))
   ((looking-at "\\]")
    (forward-char 1)
    '(close-bracket))
   ((looking-at "(")
    (forward-char 1)
    (vm-imap-net-parse-group 'list))
   ((looking-at ")")
    (forward-char 1)
    '(close-paren))
   ((looking-at "{")
    (forward-char 1)
    (vm-imap-net-parse-literal))
   ((looking-at "}")
    (forward-char 1)
    '(close-brace))
   ((looking-at "\042")
    (forward-char 1)
    (vm-imap-net-parse-quoted))
   ;; should be "[\000-\040\177-\377]", but Microsoft Exchange emits
   ;; 8-bit characters despite the RFC 2060 prohibition
   ((and (looking-at "[\000-\040\177]")
	 (= vm-imap-tolerant-of-bad-imap 0))
    (vm-imap-protocol-error "illegal char (%d)" (char-after (point))))
   (t
    (vm-imap-net-parse-atom))))

(defun vm-imap-net-parse-object (&optional skip-eol)
  "Parse one token and answer with it.
SKIP-EOL means a line ending inside this read is nothing rather than the end
of it, which is what the bracketed and parenthesised lists want: a literal in
one of them puts its octets on the next line."
  (let ((token nil))
    (while (null token)
      (skip-chars-forward " \t")
      (setq token (vm-imap-net-parse-token))
      (when (and skip-eol (eq (car token) 'end-of-line))
	(setq token nil)))
    token))

(defun vm-imap-net-parse-group (kind)
  "Parse tokens until this group's closing bracket, and answer with the group.
KIND is `vector\\=' for one opened with [ and `list\\=' for one opened with (."
  (let* ((closer (if (eq kind 'vector) 'close-bracket 'close-paren))
	 (wrong (if (eq kind 'vector) 'close-paren 'close-bracket))
	 (group (list kind))
	 (tail group)
	 (object (vm-imap-net-parse-object t)))
    (while (not (eq (car object) closer))
      (when (eq (car object) wrong)
	(vm-imap-protocol-error "unexpected %s"
				(if (eq wrong 'close-paren) ")" "]")))
      (setcdr tail (list object))
      (setq tail (cdr tail))
      (setq object (vm-imap-net-parse-object t)))
    group))

(defun vm-imap-net-parse-literal ()
  "Parse a {n} literal, the { having been read, and answer with its string.
Asks for the whole of it in one go: the request is the position the octets end
at, so a body arriving in a thousand chunks is waited for once and parsed
once."
  (let ((object (vm-imap-net-parse-object))
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
    (unless (eq (car (vm-imap-net-parse-object)) 'close-brace)
      (vm-imap-protocol-error "} expected"))
    (unless (eq (car (vm-imap-net-parse-object)) 'end-of-line)
      (vm-imap-protocol-error "CRLF expected"))
    (setq start (point))
    (when (< (- (point-max) start) octets)
      (throw 'vm-imap-net-need (vm-net-request-position (+ start octets))))
    (goto-char (+ start octets))
    (list 'string start (point))))

(defun vm-imap-net-parse-quoted ()
  "Parse a quoted string, the opening quote having been read."
  (let ((start (point))
	(end nil))
    (skip-chars-forward "^\042")
    (setq end (point))
    (unless (looking-at "\042")
      (throw 'vm-imap-net-need (vm-net-request-growth)))
    (forward-char 1)
    (list 'string start end)))

(defun vm-imap-net-parse-atom ()
  "Parse an atom, up to the first character that cannot be part of one."
  ;; 8-bit characters should be non-word characters here, but Microsoft
  ;; Exchange puts them in atoms
  (let ((start (point))
	(end nil))
    (skip-chars-forward "^\000-\040\177()[]{}")
    (setq end (point))
    (unless (looking-at "[][\000-\040\177(){}]")
      (throw 'vm-imap-net-need (vm-net-request-growth)))
    (list 'atom start end)))

(defun vm-imap-net-parse-response ()
  "One whole response line as its tokens, or the request the parse wants.
An ill-formed line answers with an empty list, as the blocking reader does.

A list of tokens means the line was all here and the read point is past it.
A request -- a function, which tokens never are -- means it was not: nothing
has moved, and parsing again once the driver has satisfied the request starts
the line from its beginning."
  (catch 'vm-imap-net-need
    (goto-char vm-imap-net-read-point)
    (let ((tokens nil)
	  (tail nil)
	  (object (vm-imap-net-parse-object)))
      (while (not (eq (car object) 'end-of-line))
	(if (null tokens)
	    (setq tokens (list object)
		  tail tokens)
	  (setcdr tail (list object))
	  (setq tail (cdr tail)))
	(setq object (vm-imap-net-parse-object)))
      (setq vm-imap-net-read-point (point))
      tokens)))

(defmacro vm-imap-net-read-a-response ()
  "The tokens of the next response line, waited for.
Goes inside a generator and nowhere else: `iter-yield\\=' is what waits, and
a plain function cannot yield, which is the whole reason this is a macro.

One generator then reads a command\\='s worth of responses.  A generator per
token, which is what this replaces, cost 3775 conses and 23,279 vector words
a response line where parsing the same tokens costs 84 and 112: 340,000
generators to fetch 6500 messages, of which only 2,000 reads ever waited for
anything, and 323M vector words of allocation to bring in 13MB of mail.  That
allocation was what the garbage collections in a fetch were (issue #742)."
  '(let ((parsed (vm-imap-net-parse-response)))
     (while (functionp parsed)
       (iter-yield parsed)
       (setq parsed (vm-imap-net-parse-response)))
     parsed))

(defun vm-imap-net-verify-response (response &optional description)
  "Answer with RESPONSE, signalling if the server said NO, BAD or BYE.
DESCRIPTION names the command, for the error message."
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
  response)

(defun vm-imap-net-error-message (position)
  "The server's error text in the process buffer, starting at POSITION."
  (buffer-substring position
		    (save-excursion
		      (goto-char position)
		      (if (search-forward "\r\n" (point-max) t)
			  (- (point) 2)
			(point-max)))))

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

(defun vm-imap-net-send-line (line)
  "Send LINE with no tag in front of it, and note where the answer begins.
What a continuation asks for: the answer to an AUTHENTICATE challenge is a
line of its own.  The line is not echoed into the process buffer, holding a
credential."
  (let ((process (get-buffer-process (current-buffer))))
    (goto-char (point-max))
    (insert-before-markers "<authentication response omitted>\r\n")
    (setq vm-imap-net-read-point (point))
    (process-send-string process (format "%s\r\n" line))))

(defvar vm-imap-net-counting nil
  "Where `vm-imap-net-command' is to report its progress, or nil.
A list (FOLDER PHASE TOTAL): the folder whose mode line says so, the word for
what is being done, and how many responses are expected.  Bound around a
command that answers one line per message, which is the only kind worth
counting.")

(iter-defun vm-imap-net-command (command &optional description)
  "Send COMMAND and answer with every response line up to its tagged one.
The tagged line is the last of them, so a caller that wants only whether it
worked can look at that, and one that wants the untagged data has it in
order."
  (vm-imap-net-send command)
  (let ((lines nil)
	(done nil)
	(counted 0)
	(worked (float-time))
	response)
    (while (not done)
      (setq response (vm-imap-net-verify-response
		      (vm-imap-net-read-a-response)
		      (or description command)))
      (push response lines)
      (setq counted (1+ counted))
      ;; Yield with nothing to wait for, so that this is interruptible.  A
      ;; server answering with a response per message sends faster than they
      ;; are read, so the reads above never wait and the whole of a six
      ;; thousand response FETCH was parsed inside one `iter-next' -- 0.68
      ;; seconds of Emacs stopped, which the slice in `vm-net--resume' cannot
      ;; help with because it sits between steps and there was only ever one.
      ;; Measured: 78 steps for a synchronisation, 15 of them over 50ms.
      (when (> (- (float-time) worked) vm-net--slice)
	(setq worked (float-time))
	(iter-yield (vm-net-request-now)))
      ;; A command that answers one line per message -- the FETCH of every
      ;; UID and flag, which is the long silence at the start of a fetch of a
      ;; big mailbox -- says how far it has got.  Every hundredth, because the
      ;; mode line is redrawn for each one and six thousand redraws are worth
      ;; nothing to anybody.
      (when (and vm-imap-net-counting (zerop (% counted 100)))
	(vm-imap-net-note-progress (nth 0 vm-imap-net-counting)
				   counted
				   (nth 2 vm-imap-net-counting)
				   (nth 1 vm-imap-net-counting)))
      (when (vm-imap-response-matches response 'VM 'OK)
	(setq done t)))
    (nreverse lines)))

(defun vm-imap-net-logout ()
  "Say LOGOUT, without waiting to be answered.
A server counts its connections -- dovecot's `mail_max_userip_connections'
-- and a client that drops them without a word leaves it to time them out.
Nothing waits for the answer: the session is over either way, and this runs
where a session is being unwound.

In an `unwind-protect', so it is said whether the session ran to the end
or was abandoned, which is the reason the driver closes a generator rather
than dropping it."
  (let ((process (get-buffer-process (current-buffer))))
    (when (process-live-p process)
      (ignore-errors
	(process-send-string process
			     (format "%s LOGOUT\r\n" (vm-imap-net-next-tag)))))))

;;; The start of a session

(iter-defun vm-imap-net-greeting ()
  "Read the server's greeting.
Answers t for OK, `preauth' for PREAUTH, and nil for anything else."
  (let ((response (vm-imap-net-read-a-response)))
    (cond ((vm-imap-response-matches response '* 'OK) t)
	  ((vm-imap-response-matches response '* 'PREAUTH) 'preauth)
	  (t nil))))

(iter-defun vm-imap-net-capabilities ()
  "Ask what the server can do.
Answers (CAPABILITIES AUTHENTICATIONS), both lists of symbols."
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

(defun vm-imap-net-remember-password (password)
  "Remember PASSWORD for this session's maildrop, the server having taken it.

After the login and not before: an entry in `vm-imap-passwords' is what the
next session looks the password up in, and a wrong one there is a login that
fails without anybody being asked anything."
  (let ((key vm-imap-net-password-key))
    (when (and key (stringp password) (not (equal password ""))
	       (not (equal password "*"))
	       (not (assoc key vm-imap-passwords)))
      (setq vm-imap-passwords (cons (list key password) vm-imap-passwords)))))

(declare-function vm-hmac-md5 "vm-crypto" (key data))
(declare-function vm-mime-base64-decode-string "vm-mime" (string))
(declare-function vm-mime-base64-encode-string "vm-mime" (string))
(declare-function vm-imap-protocol-error "vm-imap" (&rest args))

(iter-defun vm-imap-net-authenticate-cram-md5 (user password)
  "Log in as USER with CRAM-MD5, and answer what the server can do afterwards.

RFC 2195: the server answers the AUTHENTICATE with a continuation line
holding a base64 challenge, and the client sends back the user name and the
HMAC-MD5 of that challenge under the password, base64 again, as a line of
its own with no tag.

`vm-hmac-md5' rather than the pads and xors spelled out: it takes the
password as octets, where doing it by hand sent the wrong digest for an
accented one (emacs-vm/vm#772)."
  (let ((tag (vm-imap-net-send "AUTHENTICATE CRAM-MD5"))
	(challenge nil)
	response)
    ;; The server answers with a continuation line, not a tagged one, so this
    ;; reads a single response rather than going through
    ;; `vm-imap-net-command', which reads until a tag that cannot come until
    ;; the answer has been sent.
    (setq response (vm-imap-net-verify-response
		    (vm-imap-net-read-a-response)
		    "AUTHENTICATE CRAM-MD5"))
    (unless (vm-imap-response-matches response '+ 'atom)
      (vm-imap-protocol-error "Don't understand AUTHENTICATE response"))
    (let ((token (nth 1 response)))
      (setq challenge (vm-mime-base64-decode-string
		       (buffer-substring (nth 1 token) (nth 2 token)))))
    (vm-imap-net-send-line
     (vm-mime-base64-encode-string
      (concat user " " (vm-hmac-md5 password challenge))))
    ;; and now the tagged answer to the AUTHENTICATE
    (let ((done nil))
      (while (not done)
	(setq response (vm-imap-net-verify-response
			(vm-imap-net-read-a-response)
			"AUTHENTICATE CRAM-MD5"))
	(when (vm-imap-response-matches response 'VM 'OK)
	  (setq done t))))
    (ignore tag))
  (vm-imap-net-remember-password password)
  (iter-yield-from (vm-imap-net-capabilities)))

(iter-defun vm-imap-net-login (user password)
  "Log in as USER, and answer with what the server can do afterwards.
The capabilities are asked for again: a server may advertise more once the
connection is authenticated, and several advertise fewer before it."
  (iter-yield-from (vm-imap-net-command
		    (format "LOGIN %s %s"
			    (vm-imap-net-quote user)
			    (vm-imap-net-quote password))
		    "LOGIN"))
  (vm-imap-net-remember-password password)
  (iter-yield-from (vm-imap-net-capabilities)))

;;; A mailbox

(declare-function vm-imap-quote-mailbox-name "vm-imap" (mailbox))
(declare-function vm-imap-scan-list-for-flag "vm-imap" (list flag))
(declare-function vm-inform "vm-misc" (level &rest args))

(defun vm-imap-net-number (token)
  "The number TOKEN is, read from the process buffer."
  (string-to-number (buffer-substring (nth 1 token) (nth 2 token))))

(defun vm-imap-net-flag-names (list)
  "The flags of a parsed FLAGS or PERMANENTFLAGS response, as strings.
Must be called with the process buffer current, the tokens holding positions
in it."
  (delq nil (mapcar (lambda (token)
                      (and (eq (car token) 'atom)
                           (buffer-substring-no-properties (nth 1 token)
                                                           (nth 2 token))))
                    (cdr list))))

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
		      ;; As strings, here, while the tokens' buffer positions
		      ;; are still good: they point into the process buffer,
		      ;; so a folder that kept the tokens could not read them
		      ;; again, and the folder does keep these (#601).
		      (setq permanent-flags
			    (vm-imap-net-flag-names (nth 1 contents)))))))
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
`vm-imap-get-message-data-list\\=' answers, newest first.

A response with no UID is not an answer to this: a server sends a message\\='s
flags on its own account when somebody else changes them (RFC 3501 7.4.1), and
taking that for message data put an entry with no UID in the folder\\='s tables
-- \"Wrong type argument: stringp, nil\", and no mail.  VM reads the flags it
acts on from the server at the next synchronisation anyway."
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
	     ;; an item VM did not ask for: stepped over, not called a broken
	     ;; response
	     (t (setq contents (vm-imap-skip-fetch-item contents)))))
	  (when uid
	    (push (cons number (cons uid (cons size (nreverse flags)))) data)))))
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
Answers nil for a FETCH that carries no message: a server sends one of its own
accord to report a message's flags, and that is not an answer to a fetch of
message text.  Signals for one that carries a message but no UID, there being
no saying which message it would be."
  (let* ((parts (vm-imap-fetch-response-parts (cdr (nth 3 response))))
	 (uid (car parts))
	 (text (cdr parts)))
    (cond ((null text) nil)
	  ((null uid)
	   (vm-imap-protocol-error "expected a UID in FETCH response"))
	  (t (list uid (nth 1 text) (nth 2 text))))))

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
      (setq response (vm-imap-net-verify-response
		      (vm-imap-net-read-a-response) "FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (let ((message (vm-imap-net-fetch-message-text response)))
	       ;; nil for a FETCH the server sent to report a message's flags
	       ;; rather than to answer this one
	       (when message
		 (apply store message)
		 (setq count (1+ count)))))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq done t))))
    count))

;;; Connecting

(declare-function vm-parse "vm-misc" (string regexp &optional matchn matches))
(declare-function vm-binary-coding-system "vm-misc" ())
(declare-function vm-folder-type-to-write "vm-folder" (&optional file))

(defvar vm-imap-server-timeout)
(defvar vm-imap-passwords)

(declare-function vm-imapdrop-sans-password-and-mailbox "vm-misc" (source))
(declare-function vm-auth-source-password "vm-misc" (hosts port user))
(declare-function vm-imap-account-name-for-spec "vm-imap" (spec))
(declare-function vm-imap-get-password "vm-imap"
		  (folder source user host port ask-password purpose))

(define-error 'vm-imap-net-no-password
  "VM has no password for this IMAP maildrop")

(declare-function vm-setup-stunnel-random-data-if-needed "vm-crypto" ())
(declare-function vm-stunnel-configuration-args "vm-crypto" (host port))

(defvar vm-stunnel-program)
(defvar vm-stunnel-program-switches)
(defvar vm-ssh-program)
(defvar vm-ssh-program-switches)
(defvar vm-ssh-remote-command)
(defvar vm-imap-session-preauth-hook)

(defvar vm-imap-keep-trace-buffer)
(defvar vm-kept-imap-buffers)
(declare-function vm-keep-some-buffers "vm-misc"
		  (buffer ring-variable number-to-keep &optional rename-prefix))

(defun vm-imap-net-done-with-buffer (buffer)
  "Finish with BUFFER, a session's process buffer.

Kept as a trace when `vm-imap-keep-trace-buffer' says to, as the blocking
path keeps its own: the driver killed every session buffer the moment the
session ended, so there was nothing left to look at afterwards -- and \"did VM
send that delete?\" is answered by the traffic and by nothing else."
  (when (buffer-live-p buffer)
    ;; nothing to keep when nothing was said: a connection that was never made
    ;; leaves an empty buffer, and keeping those is how a session that failed
    ;; early leaves one behind for every attempt
    (if (or (null vm-imap-keep-trace-buffer)
	    (zerop (buffer-size buffer)))
	(kill-buffer buffer)
      (vm-keep-some-buffers buffer 'vm-kept-imap-buffers
			    vm-imap-keep-trace-buffer "saved "))))

(defun vm-imap-net-session-buffer (name)
  "A process buffer for a session called NAME, ready to be read from."
  (let ((buffer (generate-new-buffer (format " *%s*" name))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (vm-imap-net-init))
    buffer))

(defun vm-imap-net-connect (name host port buffer &optional tls)
  "A connection to HOST at PORT, made without waiting for it to come up.
The process is not open when this returns; the session's sentinel hears
whether it ever will be, and its timeout covers a connect that never
completes.

TLS goes through `open-network-stream', which is the only thing that does
it: `make-network-process' has no TLS of its own and answers `:type
\\='tls' with \"Unsupported connection type\", so every imap-ssl maildrop
without an stunnel failed there before it had sent a byte.  It takes
`:nowait' too, and negotiates as the connection comes up."
  (let ((process
	 (if tls
	     (open-network-stream name buffer host port
				  :type 'tls :nowait t :coding 'binary)
	   (make-network-process :name name :host host :service port
				 :buffer buffer :noquery t :coding 'binary
				 :nowait t))))
    (set-process-query-on-exit-flag process nil)
    process))

(defun vm-imap-net-tunnelled (session name port buffer program arguments)
  "Run PROGRAM and attach SESSION to PORT once it is listening.
For a maildrop reached over ssh, which is told to forward a local port.  The
program is started, a timer looks until the port answers, and the session
starts then -- so a tunnel that takes two seconds to come up costs those two
seconds to the mail, not to Emacs."
  (vm-net-tunnel
   session program arguments port
   (or vm-imap-server-timeout 30)
   (lambda (tunnel)
     (when tunnel
       (vm-net-attach session
		      (vm-imap-net-connect name "127.0.0.1" port buffer))))))

(defun vm-imap-net-preauth-process (host port mailbox user password)
  "What `vm-imap-session-preauth-hook' answers with, or nil.
The hook is the user's own function and makes the connection itself; VM's
half of a preauthenticated session is everything after that, which is what
the driver runs."
  (run-hook-with-args-until-success 'vm-imap-session-preauth-hook
				    host port mailbox user password))

(defun vm-imap-net-known-password (source user host port)
  "The password VM already holds for SOURCE, or nil.

Its own cache first, then auth-source.  Nothing is written back: what the
blocking path remembers is its own business, and a wrong or empty entry
written there is a login that fails without asking anybody anything.  Only a
non-empty string counts -- auth-source hands back a function for some
backends, and the cache can hold a `*' that means nothing yet."
  (let* ((spec (vm-imapdrop-sans-password-and-mailbox source))
	 (known (car (cdr (assoc spec vm-imap-passwords))))
	 (password (or known
		       (vm-auth-source-password
			(list (vm-imap-account-name-for-spec source) host)
			port user))))
    (and (stringp password)
	 (not (equal password ""))
	 (not (equal password "*"))
	 password)))

(defvar vm-imap-net-said-it-is-uncompiled nil
  "Whether the notice about running from source has been given.")

(defun vm-imap-net-check-compiled ()
  "Say once that this file is not compiled, if it is not.

The parse is called once per token -- 190,000 times to fetch 6500 messages --
and the generators it runs inside rebuild their closures on every call from
source, through `cconv-make-interpreted-closure'.  Measured on a mock
server, fetching a thousand messages takes 0.8 seconds compiled and three
minutes from source, with Emacs held for tens of seconds at a time -- which
looks exactly like the blocking implementation VM used to have.  A reader
testing an uncompiled tree would draw the wrong conclusion, so VM says so
rather than being slow silently."
  (unless (or vm-imap-net-said-it-is-uncompiled
	      (let ((reader (symbol-function 'vm-imap-net-parse-object)))
		(or (byte-code-function-p reader)
		    ;; `native-comp-function-p' arrived in Emacs 30.1 and
		    ;; `subr-native-elisp-p' is obsolete from the same
		    ;; release, so the new name is asked for first and the
		    ;; old one serves the Emacsen that have only it
		    ;; (emacs-vm/vm#818).
		    (if (fboundp 'native-comp-function-p)
			(native-comp-function-p reader)
		      (and (fboundp 'subr-native-elisp-p)
			   (subr-native-elisp-p reader))))))
    (setq vm-imap-net-said-it-is-uncompiled t)
    (vm-net-warn 1 (concat "VM is running from source: asynchronous mail will be"
			 " very slow until lisp/ is byte-compiled"))))

(defun vm-imap-net-open (source name &optional may-ask)
  "Open a connection for the IMAP maildrop SOURCE and answer with a session.

MAY-ASK says the caller is a command and the reader is there to be asked for
a password.  A timer passes nil: a question from a timer arrives while
somebody is typing something else, and the check has nothing to do with the
answer anyway.

NAME goes in messages.  The session has a buffer of its own and is ready for
`vm-net-start'; nothing has been read from it yet.  The answer is
(SESSION MAILBOX USER PASSWORD).

Plain, TLS, over ssh, through stunnel, and preauthenticated.  An ssh session
has no process yet when this returns: ssh has to be listening on its
forwarded port before there is anything to connect to, and `vm-net-attach'
gives the session its connection when it is.  stunnel is the connection
itself, over its standard input and output.  A maildrop whose password VM has
not been told signals `vm-imap-net-no-password', there being nobody to ask
from inside a filter."
  (let* ((parts (vm-parse source "\\([^:]*\\):?" 1 7))
	 (protocol (car parts))
	 (host (nth 1 parts))
	 (port (nth 2 parts))
	 (mailbox (nth 3 parts))
	 (auth (nth 4 parts))
	 (user (nth 5 parts))
	 (password (nth 6 parts))
	 (preauth (equal auth "preauth")))
    (vm-imap-net-check-compiled)
    (unless (member protocol '("imap" "imap-ssl" "imap-ssh"))
      (error (concat "%s is not an IMAP maildrop type VM knows."
		     "  The types are imap, imap-ssl and imap-ssh;"
		     " M-x vm-check-configuration checks every maildrop")
	     protocol))
    (unless (member auth '("login" "cram-md5" "preauth"))
      (error (concat "%s is not an IMAP authentication VM knows."
		     "  Write login, cram-md5 or preauth in the maildrop")
	     (or auth "no authentication")))
    (when (and (stringp port) (string-match "\\`[0-9]+\\'" port))
      (setq port (string-to-number port)))
    (when (and (equal password "*") (not preauth))
      ;; "*" means VM is to find the password rather than read it out of the
      ;; maildrop.  VM may already know it; failing that, a command may ask
      ;; the reader, which is what the blocking path did and the only reason
      ;; it was reached at all.
      (setq password
	    (or (vm-imap-net-known-password source user host port)
		(and may-ask
		     ;; `vm-imap-ok-to-ask' is nil unless something has bound
		     ;; it, and nothing binds it on the way here: requiring it
		     ;; meant the question was never put and the blocking path
		     ;; asked instead
		     (let ((vm-imap-ok-to-ask t))
		       (vm-net-inform 6 "%s: asking for a password, VM has none"
				  (or (vm-imap-account-name-for-spec source)
				      (vm-safe-imapdrop-string source)))
		       (condition-case nil
			   (vm-imap-get-password
			    (or (vm-imap-folder-for-spec source)
				(vm-safe-imapdrop-string source))
			    (vm-imapdrop-sans-password-and-mailbox source)
			    user host port t "reading mail")
			 (error nil))))))
      (unless (and (stringp password) (not (equal password "")))
	;; the keys and not the passwords: a maildrop that VM will not check
	;; because it has no password is either one that was never remembered
	;; or one remembered under a key the check does not look under, and
	;; the log is the only place that difference shows
	(vm-inform 10 "%s: no password held under %s; VM holds %s" name
		   (vm-imapdrop-sans-password-and-mailbox source)
		   (if vm-imap-passwords
		       (mapconcat #'car vm-imap-passwords ", ")
		     "none"))
	(signal 'vm-imap-net-no-password (list source))))
    (let* ((buffer (vm-imap-net-session-buffer name))
	   (session (vm-net-session :name name
				    :timeout vm-imap-server-timeout))
	   (local (and (member protocol '("imap-ssh"))
		       (vm-net-free-port)))
	   (opened nil))
      (setf (vm-net-session-buffer session) buffer)
      (with-current-buffer buffer
	(setq vm-imap-net-password-key
	      (vm-imapdrop-sans-password-and-mailbox source))
	(setq vm-imap-net-auth auth))
      ;; The buffer goes with a connection that was never made: a host that
      ;; does not resolve, an stunnel that is not installed, a preauth hook
      ;; that answers with nothing.  Left behind, one accumulated per attempt.
      (unwind-protect
	  (progn
	    (cond
	     (preauth
	      (let ((process (vm-imap-net-preauth-process host port mailbox
							  user password)))
		(unless (processp process)
		  (kill-buffer buffer)
		  (error (concat "vm-imap-session-preauth-hook gave no"
				 " process for %s, so there is no"
				 " connection to use")
			 source))
		(set-process-buffer process buffer)
		(setf (vm-net-session-process session) process)
		;; nothing to log in with, and nothing to log in to: the hook did it
		(setq password nil)))
	     ((equal protocol "imap-ssh")
	      (vm-imap-net-tunnelled
	       session name local buffer vm-ssh-program
	       (nconc (list "-L" (format "%d:%s:%s" local host port))
		      (copy-sequence vm-ssh-program-switches)
		      (list host vm-ssh-remote-command))))
	     ((and (equal protocol "imap-ssl") vm-stunnel-program)
	      (vm-setup-stunnel-random-data-if-needed)
	      ;; stunnel is the connection, over its own standard input and output,
	      ;; which is what the blocking path does with it.  Telling it to listen
	      ;; on a local port instead asked for an option stunnel does not have,
	      ;; so nothing ever came up on that port and the session failed after
	      ;; the whole tunnel timeout.
	      (setf (vm-net-session-process session)
		    (vm-net-pipe session name buffer vm-stunnel-program
				 (nconc (vm-stunnel-configuration-args host port)
					(copy-sequence vm-stunnel-program-switches)))))
	     (t
	      (setf (vm-net-session-process session)
		    (vm-imap-net-connect name host port buffer
					 (equal protocol "imap-ssl")))))
	    (setq opened t))
	(unless opened
	  (vm-imap-net-done-with-buffer buffer)))
      (list session mailbox user password))))

(iter-defun vm-imap-net-open-session (user password)
  "Greet, log in, and answer with what the server says it can do.
Answers (CAPABILITIES AUTHENTICATIONS).  A greeting that is neither OK nor
PREAUTH signals: there is no session to be had, and the caller has nothing
to decide.

A nil PASSWORD is a preauthenticated session -- greeted with PREAUTH, or
made by `vm-imap-session-preauth-hook' -- and there is nothing to log in
with: the connection arrived authenticated."
  (let ((greeting (iter-yield-from (vm-imap-net-greeting))))
    (cond ((null greeting)
	   (vm-imap-normal-error "server did not greet the connection"))
	  ((or (eq greeting 'preauth) (null password))
	   (iter-yield-from (vm-imap-net-capabilities)))
	  ((equal vm-imap-net-auth "cram-md5")
	   (iter-yield-from (vm-imap-net-capabilities))
	   (iter-yield-from (vm-imap-net-authenticate-cram-md5 user password)))
	  (t
	   (iter-yield-from (vm-imap-net-capabilities))
	   (iter-yield-from (vm-imap-net-login user password))))))

;;; Getting new mail into a folder

(declare-function vm-imap-cleanup-region "vm-imap" (start end))
(declare-function vm-imap-bunch-retrieve-list "vm-imap" (retrieve-list))
(declare-function vm-imap-get-synchronization-data "vm-imap" (&optional do-retrieves))
(declare-function vm-imap-update-message-flags "vm-imap" (m flags &optional norecord))
(declare-function vm-decoded-labels-of "vm-message" (m))
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
(declare-function vm-folder-imap-permanent-flags "vm-folder" ())
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
(declare-function vm-folder-imap-uid-msn "vm-folder" (uid))
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
(defvar vm-message-list-generation)
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

(defun vm-imap-net-plan (data count &optional full-retrieve)
  "Work out what has to be fetched, and answer with (RETRIEVE-LIST BUNCHES).
The current buffer is the folder.  RETRIEVE-LIST is (UID SEQUENCE-NUMBER
HEADERS-ONLY) per message, in the order the messages will arrive; BUNCHES is
what to ask the server for, `vm-imap-message-bunch-size\\=' at a time.

FULL-RETRIEVE asks for the messages `vm-imap-retrieved-messages\\=' records as
fetched once already and the folder no longer holds, which are passed over
otherwise.  Two prefix arguments to `vm-get-new-mail\\=' ask for it.

A full synchronise used to delete those on the server instead of leaving them
alone.  A cache that had been truncated or read as the wrong type says the
same thing as a reader who expunged, so that destroyed mail nobody asked it
to; server deletions come only from `vm-imap-messages-to-expunge\\=' now,
which is what the reader expunged (emacs-vm/vm#752)."
  (vm-imap-net-install-message-data data count)
  (if (null data)
      ;; Nothing on the server, so nothing to fetch -- and everything here
      ;; that came from it is gone.  Not a case for
      ;; `vm-imap-get-synchronization-data': that calls
      ;; `vm-imap-retrieve-uid-and-flags-data', which asks the server itself
      ;; unless the UID list is non-empty, and an empty mailbox's is empty.
      ;; It asked through `vm-folder-imap-process', which a session on the
      ;; driver does not set, and the session died of it.
      (list nil nil (vm-imap-net-messages-not-on-the-server)
	    (vm-imap-net-stale-messages))
    (vm-imap-net-plan-1 data count full-retrieve)))

(defun vm-imap-net-messages-not-on-the-server ()
  "The folder's messages that came from this mailbox, none of them being there.
The current buffer is the folder."
  (seq-filter (lambda (message)
		(and (vm-imap-uid-of message)
		     (equal (vm-imap-uid-validity-of message)
			    (vm-folder-imap-uid-validity))
		     (not (member "stale" (vm-decoded-labels-of message)))))
	      vm-message-list))

(defun vm-imap-net-stale-messages ()
  "The folder's messages whose UIDVALIDITY is not the mailbox's.
The current buffer is the folder."
  (seq-filter (lambda (message)
		(not (equal (vm-imap-uid-validity-of message)
			    (vm-folder-imap-uid-validity))))
	      vm-message-list))

(defmacro vm-imap-net-as-folder (&rest body)
  "Run BODY saying that the current buffer is a folder.

`vm-buffer-types' is VM's stack of what kind of buffer it is working in, and
the blocking code pushes and pops it around every change of buffer so that
`vm-buffer-type:assert' can catch a folder being written where a connection
was meant.  The driver does not push and pop -- a generator that suspended
between an enter and its exit would leave the stack pushed for whatever ran
next -- so it binds it instead, which cannot be left unbalanced.

Without this, code the driver calls into asserted that it was in a folder
while the stack said nothing at all: with `vm-assertion-checking-off' set to
nil, which is what `test-runner --assert' and anyone debugging VM does,
visiting an IMAP folder brought in no messages."
  (declare (indent 0) (debug t))
  `(let ((vm-buffer-types (cons 'folder vm-buffer-types)))
     ,@body))

(defun vm-imap-net-plan-1 (data count &optional full-retrieve)
  "The plan for a mailbox that holds something.  See `vm-imap-net-plan'."
  (ignore data count)
  (let* ((sync (vm-imap-net-as-folder
		(vm-imap-get-synchronization-data (if full-retrieve 'full t))))
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
    (setq retrieve-list (vm-imap-net-only-new-uids retrieve-list))
    (list retrieve-list
	  (vm-imap-bunch-retrieve-list (mapcar #'cdr retrieve-list))
	  (nth 1 sync)
	  (nth 2 sync))))

(defvar vm-imap-net-held-uids nil
  "The table `vm-imap-net-uids-held\\=' answers with, or nil for none yet.
Buffer-local to the folder.")
(make-variable-buffer-local 'vm-imap-net-held-uids)

(defvar vm-imap-net-held-validity nil
  "The UIDVALIDITY `vm-imap-net-held-uids\\=' was filled for.
A UID means nothing without it, so a mailbox that has changed its validity
has none of the UIDs in that table.")
(make-variable-buffer-local 'vm-imap-net-held-validity)

(defvar vm-imap-net-held-generation nil
  "The `vm-message-list-generation\\=' `vm-imap-net-held-uids\\=' was filled at.")
(make-variable-buffer-local 'vm-imap-net-held-generation)

(defvar vm-imap-net-held-tail nil
  "The last cons of `vm-message-list\\=' `vm-imap-net-held-uids\\=' has read.
What follows it is what has arrived since, and is all that has to be read to
bring the table up to date.")
(make-variable-buffer-local 'vm-imap-net-held-tail)

(defun vm-imap-net-uids-held ()
  "The UIDs this folder holds for the mailbox it is looking at.
The current buffer is the folder.

Kept from one call to the next rather than built again, because a fetch asks
once for every message that arrives: on the 6505-message mailbox of issue #742
building it each time was 6501 tables of up to 6505 entries, 21 million
`puthash\\=' calls and 0.95s of a 6.8s fetch.

What it costs instead is one walk of the messages appended since the last
call, which over a whole fetch is one walk of the mailbox.  A folder that has
had a message taken out of it, or its list rewritten, moves
`vm-message-list-generation\\=' and the table is filled again from nothing --
so the answer follows a message put into the folder by anyone, which is what
the fetch needs: it does not block, so the reader can save a message into the
folder from elsewhere while its own fetch for that message is in flight."
  (let ((validity (vm-folder-imap-uid-validity)))
    (unless (and vm-imap-net-held-uids
		 (eq vm-imap-net-held-generation vm-message-list-generation)
		 (equal vm-imap-net-held-validity validity))
      (setq vm-imap-net-held-uids (make-hash-table :test 'equal)
	    vm-imap-net-held-generation vm-message-list-generation
	    vm-imap-net-held-validity validity
	    vm-imap-net-held-tail nil))
    (vm-imap-net-read-held-uids)
    vm-imap-net-held-uids))

(defun vm-imap-net-read-held-uids ()
  "Read into `vm-imap-net-held-uids\\=' the messages it has not read yet.
Those are the ones after `vm-imap-net-held-tail\\=', which the table's
generation says are appended and not a list rewritten underneath it."
  (let ((mp (if vm-imap-net-held-tail
		(cdr vm-imap-net-held-tail)
	      vm-message-list)))
    (while mp
      (vm-imap-net-note-held-uid (car mp))
      (setq vm-imap-net-held-tail mp
	    mp (cdr mp)))))

(defun vm-imap-net-note-held-uid (message)
  "Put MESSAGE's UID into `vm-imap-net-held-uids\\=', if it has one to put.
A message with no UID, or one from a mailbox of another validity, is not a
message this folder holds for the mailbox being read."
  (let ((uid (vm-imap-uid-of message)))
    (when (and uid (equal (vm-imap-uid-validity-of message)
			  vm-imap-net-held-validity))
      (puthash uid t vm-imap-net-held-uids))))

(defun vm-imap-net-note-held-message (message)
  "Tell the held-UID table, if there is one, that this folder holds MESSAGE.
The table is filled by reading `vm-message-list\\=', so a message put into the
list before it was given its UID is invisible to it -- and a UID the table has
not got is one the folder fetches and writes a second time.  A caller that
gives a message its UID after putting it in the list says so here, or forgets
the table with `vm-imap-net-forget-held-uids\\='."
  (when vm-imap-net-held-uids
    (vm-imap-net-note-held-uid message)))

(defun vm-imap-net-forget-held-uids ()
  "Forget the held-UID table, so that the next question fills it again.
For a caller that changes what the folder holds and is not the one asking per
arriving message, where filling it again is one walk of the folder and costs
nothing."
  (setq vm-imap-net-held-uids nil
	vm-imap-net-held-tail nil))

(defun vm-imap-net-uid-held-p (uid)
  "Whether this folder already holds UID, for the mailbox it is looking at.
The current buffer is the folder."
  (and (gethash uid (vm-imap-net-uids-held)) t))

(defun vm-imap-net-only-new-uids (retrieve-list)
  "RETRIEVE-LIST without the entries this folder already holds, warning of them.
The current buffer is the folder.

A UID names one message for as long as the UIDVALIDITY holds, so a folder
holding two of them holds the same message twice: two summary lines, two
copies in the file, one server message for both, and every later operation
that looks one up by UID reaching whichever comes first.

Dropped and reported rather than fetched, and rather than the whole fetch
failing over it: the rest of the mailbox is new mail the reader wants, and a
message that is here already is one there is nothing left to do about."
  (let ((held (vm-imap-net-uids-held))
	(asked (make-hash-table :test 'equal))
	(wanted nil)
	(again nil)
	(twice nil))
    (dolist (entry retrieve-list)
      (let ((uid (car entry)))
	(cond ((gethash uid held) (push uid again))
	      ((gethash uid asked) (push uid twice))
	      (t (puthash uid t asked)
		 (push entry wanted)))))
    (when again
      (vm-net-warn 0 "%s: not fetching %d message%s the folder has already: UID%s %s"
	       (buffer-name) (length again) (if (= (length again) 1) "" "s")
	       (if (= (length again) 1) "" "s")
	       (string-join (nreverse again) ", ")))
    (when twice
      (vm-net-warn 0 "%s: the server listed UID%s %s twice; fetching %s once"
	       (buffer-name) (if (= (length twice) 1) "" "s")
	       (string-join (nreverse twice) ", ")
	       (if (= (length twice) 1) "it" "each")))
    (nreverse wanted)))

(defun vm-imap-net-require-folder (folder)
  "Signal unless FOLDER is still there to be written into.
A folder can be quit while a session of its own is running, and a session
that goes on writing into a dead buffer errors somewhere further in, where
what went wrong is no longer visible."
  (unless (buffer-live-p folder)
    (vm-imap-normal-error "the folder was closed while its session ran")))

(defun vm-imap-net-hold (holding folder-type source start end)
  "Copy the message between START and END of SOURCE into HOLDING.

HOLDING is a buffer of its own, not the folder: a bunch is written there and
put into the folder in one piece when it is taken in.  The folder therefore
never holds a message the message list does not know about, which is what a
save landing in the middle of a bunch used to write to the cache file -- and
after a crash there, the next fetch had no UID for it and brought it again, so
the reader had it twice.

The cleaning up a crash box wants, in the order the blocking path did it:
CRLF to LF, the separators the folder's own type wants, and the headers that
go with them."
  (with-current-buffer holding
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(let ((buffer-read-only nil)
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

(defun vm-imap-net-put-in-folder (folder holding)
  "Put what HOLDING has collected into FOLDER, in one piece, and empty it.
Answers whether there was anything.  One insert, so that no save can see the
folder part way through a bunch."
  (vm-imap-net-require-folder folder)
  (when (> (buffer-size holding) 0)
    (with-current-buffer folder
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-max))
	  (let ((buffer-read-only nil))	; a folder buffer is read-only
	    (insert-buffer-substring holding)))))
    (with-current-buffer holding (erase-buffer))
    t))

(defun vm-imap-net-entries-written (uids entries)
  "The ENTRIES for UIDS, in the order the messages were written.

ENTRIES is (UID SEQUENCE-NUMBER HEADERS-ONLY) per message, as the plan made
them; UIDS is what the server actually answered for.  Signals for a UID that
was not asked for: a message in the folder is about to be given that UID, and
one that came from nowhere the plan knows about is not a message this folder
can account for."
  (mapcar (lambda (uid)
	    (or (assoc uid entries)
		(vm-imap-protocol-error
		 "server answered with UID %s, which was not asked for" uid)))
	  uids))

(defvar vm-imap-net-provisional-message nil
  "The message `vm-imap-net-assimilate' chose to keep the folder usable.

A folder being fetched into for the first time has no current message until
one is chosen, and a command typed before then fails on nil.  So one is chosen
partway, from the messages that have arrived so far.  Where the folder holds
nothing new or unread, that choice is the last message of the first bunch, and
it used to stand: a fully read folder of three hundred and ninety opened at
message ten, and the number followed `vm-imap-message-bunch-size'.  Worse, it
was written to the cache as `X-VM-Bookmark', so every later visit opened there
too (emacs-vm/vm#799).

Held so that the end of the fetch can tell that choice from a message the
reader went to themselves, and take it back if they did not.")
(make-variable-buffer-local 'vm-imap-net-provisional-message)

(defun vm-imap-net-assimilate (retrieve-list uid-validity)
  "Take the messages just written into the folder into the message list.
The current buffer is the folder.  RETRIEVE-LIST is the entries for the
messages this call is taking in, in the order they were written.  Answers
with the new messages.

Signals rather than pairing them up wrongly if the folder holds a different
number of new messages from the number written: each message here is about to
be given the UID of the entry beside it, and a folder that gained a message
from somewhere else in between would give every message after it the UID of
another.  That is the corruption this pairing can cause, so it is checked and
not assumed.

Called once per bunch as the fetch runs, not once at the end.  The work is
the same either way but the pieces are small: taking two thousand messages in
at once, threading them and rebuilding the summary, is several seconds in
which Emacs answers nothing -- which is the freeze the conversion was
supposed to remove, arriving from the other side."
  (setq vm-spooled-mail-waiting nil)
  (vm-set-folder-imap-retrieved-count (vm-folder-imap-mailbox-count))
  (intern (buffer-name) vm-buffers-needing-display-update)
  (unless (equal uid-validity (vm-folder-imap-uid-validity))
    ;; the mailbox this folder holds is not the one these messages came from
    (vm-imap-protocol-error
     "UID VALIDITY changed while the folder was being written"))
  (let* ((new-messages (vm-assimilate-new-messages :read-attributes nil))
	 (messages new-messages)
	 (entries retrieve-list))
    (unless (= (length new-messages) (length retrieve-list))
      (vm-imap-protocol-error
       "%d message%s arrived for %d written: the folder changed underneath"
       (length new-messages) (if (= (length new-messages) 1) "" "s")
       (length retrieve-list)))
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
	;; the message was appended before it had a UID, so the table cannot
	;; have read one from it
	(vm-imap-net-note-held-message message)
	(vm-set-byte-count-of message (vm-folder-imap-uid-message-size uid))
	(vm-imap-update-message-flags
	 message (vm-folder-imap-uid-message-flags uid) t)
	(vm-mark-for-summary-update message)
	(vm-set-stuff-flag-of message t))
      (setq messages (cdr messages)
	    entries (cdr entries)))
    ;; A folder that was empty has no current message, and until one is chosen
    ;; every command that works on it takes `(car vm-message-pointer)' and gets
    ;; nil: typing a space during the first fetch into an empty folder was
    ;; "vm-scroll-forward: Wrong type argument: arrayp, nil".  The folder is
    ;; usable while the rest of the fetch runs, so this cannot wait for the end
    ;; of the fetch.  Shown as well as selected, which is what the arrival
    ;; would have done; it happens once, since after this the folder has a
    ;; current message and a reader reading it is not to be moved.
    (when (and new-messages (null vm-message-pointer)
	       (vm-thoughtfully-select-message))
      ;; Remembered as a guess.  With no new or unread message to go to,
      ;; `vm-thoughtfully-select-message' falls back to the last message there
      ;; is, and at this point that is the last of the first bunch rather than
      ;; the last of the folder.  `vm-imap-net-show-arrival' undoes it at the
      ;; end of the fetch if the reader has not moved (emacs-vm/vm#799).
      (setq vm-imap-net-provisional-message (car vm-message-pointer))
      (vm-present-current-message))
    (vm-update-summary-and-mode-line)
    (when vm-arrived-message-hook
      (dolist (message new-messages)
	(vm-run-hook-on-message 'vm-arrived-message-hook message)))
    new-messages))

(defun vm-imap-net-take-server-flags (uid-validity)
  "Give the folder's messages the flags the server says they have.

The current buffer is the folder, and its UID tables have been installed by
`vm-imap-net-install-message-data' -- the same tables the blocking
`retrieve-attributes' step reads.  Answers with how many messages were
touched.

A message whose own changes have not reached the server is left alone: its
modification flag is still set, so the server's flags are the stale copy, and
applying them would overwrite the reader's own attributes and leave nothing
for the next synchronisation to retry."
  (let ((touched 0))
    (dolist (message vm-message-list)
      (let ((uid (vm-imap-uid-of message)))
	(when (and uid
		   (equal (vm-imap-uid-validity-of message) uid-validity)
		   (vm-folder-imap-uid-msn uid)
		   (not (vm-attribute-modflag-of message)))
	  (vm-imap-update-message-flags
	   message (vm-folder-imap-uid-message-flags uid) t)
	  (vm-mark-for-summary-update message)
	  (setq touched (1+ touched)))))
    (vm-update-summary-and-mode-line)
    touched))

(defun vm-imap-net-arrived (folder)
  "Say that a fetch into FOLDER has finished putting messages in it.
`vm-arrived-messages-hook' is for the arrival and not for each bunch of
it, so it runs here rather than in `vm-imap-net-assimilate'."
  (with-current-buffer folder
    (run-hooks 'vm-arrived-messages-hook)))

(declare-function vm-expunge-folder "vm-folder" (&rest keys))
(declare-function vm-add-or-delete-message-labels "vm-undo" (string mlist action))

(defun vm-imap-net-expunge-locally (local-expunge-list stale-list)
  "Take out of the folder what the server no longer has.
The current buffer is the folder.  A message whose UIDVALIDITY is stale is
labelled rather than removed: the blocking path asks whether to expunge
those, and there is nobody to ask from inside a filter, so the safe half of
the choice is taken and the label says which messages it was taken for."
  (when local-expunge-list
    ;; gone from the server, so nothing to tell the server about
    (vm-expunge-folder :quiet t :just-these-messages local-expunge-list
		       :not-on-the-server t))
  (dolist (message stale-list)
    (vm-add-or-delete-message-labels "stale" (list message) 'all)))

(iter-defun vm-imap-net-get-new-mail (folder mailbox user password
					     &optional attributes all-flags
					     full-retrieve)
  "Fetch what FOLDER has not got from MAILBOX, and answer with how many.
The messages arrive a bunch at a time, are collected in a buffer of their own,
and go into FOLDER in one piece as each bunch is taken into the message list.
The folder therefore never holds a message the list does not know about: a save
landing in the middle of a bunch wrote one to the cache file, and after a crash
there the next fetch had no UID for it and brought it again.

The two options are what a synchronisation asks for on top of a fetch:
ATTRIBUTES gives the folder's own messages the flags the server has for them,
and ALL-FLAGS sends every message's flags rather than only those that changed.
Without them this is `vm-get-new-mail\\=': what has arrived, and what the folder
has asked to be expunged.

There was a third, FULL, which deleted on the server what the folder no longer
holds.  It is gone: what the reader expunged is in
`vm-imap-messages-to-expunge\\=' and goes up either way, and the difference
between mailbox and cache is as often a damaged cache as an expunge
(emacs-vm/vm#752).

FULL-RETRIEVE is the other direction, and no part of a synchronisation: fetch
what the folder was given once and no longer holds, rather than passing it
over.  Two prefix arguments to `vm-get-new-mail' ask for it."
  ;; The bunch buffer is made here and killed in the cleanup below, not at the
  ;; end of the body: an abandoned session or any error on the way -- the
  ;; refused UIDVALIDITY a few lines down is the first of them -- never reaches
  ;; the end of the body, and left one behind every time.
  (let ((holding (generate-new-buffer " *vm-imap-bunch*")))
    (unwind-protect
      (progn
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
	    (setq folder-type (vm-folder-type-to-write))
	    (vm-set-folder-imap-uid-validity uid-validity)
	    (vm-set-folder-imap-mailbox-count count)
	    (unless (vm-folder-imap-retrieved-count)
    	      (vm-set-folder-imap-retrieved-count count))
	    (vm-set-folder-imap-recent-count (nth 1 select))
	    (vm-set-folder-imap-read-write (nth 3 select))
	    (vm-set-folder-imap-can-delete (nth 4 select))
	    (vm-set-folder-imap-body-peek body-peek)
	    (vm-set-folder-imap-permanent-flags (nth 5 select)))
	  ;; the folder's own changes go up before its picture of the server is
	  ;; taken, or the flags just fetched would be written back over them
	  (iter-yield-from (vm-imap-net-save-flags folder all-flags))
	  ;; level 5, unlike the blocking path's 6: there Emacs is frozen and
	  ;; the freeze is the progress report.  Here nothing looks as if it is
	  ;; happening unless VM says so, which is what the reader who waited
	  ;; through a first fetch of a large mailbox was left doing.
	  (unless (zerop count)
	    (vm-net-inform 5 "%s: reading the list of %d message%s on the server..."
    		       (buffer-name folder) count (if (= count 1) "" "s"))
	    ;; The mode line says "listing 1200/6438" while this runs.  It is one
	    ;; response per message and on a mailbox of thousands it is the long
	    ;; wait before anything arrives; the word alone said "fetching" and
	    ;; nothing was being fetched yet.
	    (vm-imap-net-note-progress folder 0 count "listing"))
	  (setq data (if (zerop count)
    			 nil
    		       (let ((vm-imap-net-counting (list folder "listing" count)))
    			 (iter-yield-from (vm-imap-net-message-data 1 count)))))
	  (setq plan (with-current-buffer folder
		       (vm-imap-net-plan data count full-retrieve)))
	  (let ((retrieve-list (nth 0 plan))
    		(bunches (nth 1 plan)))
	    (when retrieve-list
	      (vm-imap-net-note-progress folder 0 (length retrieve-list))
	      (vm-net-inform 5 "%s: retrieving %d message%s..." (buffer-name folder)
    			 (length retrieve-list)
    			 (if (= (length retrieve-list) 1) "" "s")))
	    (with-current-buffer folder
    	      (vm-imap-net-expunge-locally (nth 2 plan) (nth 3 plan)))
	    (when attributes
	      ;; after the folder's own flags went up, and after the local
	      ;; expunges: a message that is gone has no flags to be given
	      (let ((touched (with-current-buffer folder
			       (vm-imap-net-take-server-flags uid-validity))))
		(vm-net-inform 6 "%s: %d message%s took the server's flags"
			   (buffer-name folder) touched
			   (if (= touched 1) "" "s"))))
	    (dolist (bunch bunches)
    	      (let* ((range (car bunch))
    		     (headers-only (cadr bunch))
    		     (count (1+ (- (cdr range) (car range))))
    		     (entries (seq-take (nthcdr retrieved retrieve-list) count))
    		     (written nil)
    		     (store (lambda (uid start end)
			      ;; asked for as new, and here by the time it
			      ;; arrived: written twice, the folder would hold
			      ;; the one message twice over.  Said out loud and
			      ;; left out, the rest of the fetch going on -- the
			      ;; other messages are new mail the reader wants
			      (if (with-current-buffer folder
				    (vm-imap-net-uid-held-p uid))
				  (vm-net-warn 0 (concat "%s: UID %s arrived while"
						       " the folder was gaining"
						       " it; not written twice")
					   (buffer-name folder) uid)
				(push uid written)
				(vm-imap-net-hold holding folder-type source
						  start end)))))
    		(iter-yield-from
    		 (vm-imap-net-fetch (car range) (cdr range) body-peek headers-only
    				    store))
		;; the bunch goes into the folder in one piece, so that a save
		;; -- the reader's, or the one a queued save does when this
		;; session ends -- never writes the cache file with a message
		;; the message list does not know about
		(vm-imap-net-put-in-folder folder holding)
		;; taken in a bunch at a time: the folder shows what has
		;; arrived while the rest is still coming, and no single
		;; slice of the work is long enough to be felt.
		;;
		;; What was written, in the order it was written, and not what
		;; was asked for: a server may answer for fewer messages than
		;; the range names, and pairing the messages in the buffer with
		;; the entries of the ones that were asked for would then give
		;; each message the UID of another.
    		(with-current-buffer folder
    		  (vm-imap-net-assimilate
		   (vm-imap-net-entries-written (nreverse written) entries)
		   uid-validity))
    		(setq retrieved (+ retrieved count))
		(vm-imap-net-note-progress folder retrieved (length retrieve-list))
		;; level 6, so it is logged and not shown: the mode line carries the
		;; count live, and a line per bunch in the echo area is in the way of
		;; whoever is using Emacs while the fetch runs -- which is the point
		;; of the fetch not freezing them out.  The start and the end are
		;; said.
		(vm-net-inform 6 "%s: %d of %d messages retrieved"
			       (buffer-name folder) retrieved (length retrieve-list))))
	    (vm-imap-net-arrived folder)
	    ;; and what the folder has expunged locally goes on the server, in the
	    ;; same session: by UID, since a sequence number means something
	    ;; different after every expunge
	    (let ((uids (with-current-buffer folder
    			  (vm-imap-net-uids-to-expunge uid-validity))))
	      (when uids
    		(iter-yield-from (vm-imap-net-expunge uids))
    		(with-current-buffer folder
    		  (vm-imap-net-note-expunged uids))))
	    retrieved)))
      (vm-imap-net-logout)
      (when (buffer-live-p holding) (kill-buffer holding)))))

;;; Flags, and what the server would not take

(declare-function vm-imap-message-flag-changes "vm-imap" (m))
(declare-function vm-imap-flag-list-string "vm-imap" (flags))
(declare-function vm-attribute-modflag-of "vm-message" (m))
(declare-function vm-set-attribute-modflag-of "vm-message" (m flag))
(declare-function vm-imap-uid-validity-of "vm-message" (m))

(defvar vm-imap-refused-flags)
(defvar vm-imap-dropped-flags)
(declare-function vm-imap-keyword-p "vm-imap" (flag))
(declare-function vm-imap-fetch-response-flags "vm-imap" (response))

(iter-defun vm-imap-net-store-flags-1 (sign uid flags)
  "Send one UID STORE of FLAGS, and read its answer.
SIGN is \"+\" or \"-\" and UID the message's UID.  Signals
`vm-imap-normal-error\\=' if the server refuses the command.

Answers with (t . FLAGS), the flags the server reported the message to have
afterwards, or nil where it reported nothing.  The two differ: a message left
with no flags reports an empty list, which is the case this is here to catch,
so \"none\" and \"did not say\" cannot share a value.  The answer costs a
response line per message, so it is asked for only where there is something to
check.  A store of nothing but the protocol's own flags uses `.SILENT\\=' and
asks nothing; one carrying a keyword does not, a keyword being what a server
may take and discard (emacs-vm/vm#601).

By UID and not by sequence number.  The numbers VM holds are the ones the
mailbox had when it last read it, and every expunge by anybody else shifts
them down: a folder that marked its own second message read after another
client had deleted the first sent `STORE 2\\=', which by then was the third
message, and the server marked that one read instead.  A UID means one
message for as long as the UIDVALIDITY holds, and a UID the mailbox no longer
has matches nothing rather than matching a stranger."
  (let* ((checking (seq-some #'vm-imap-keyword-p flags))
	 (suffix (if checking "FLAGS" "FLAGS.SILENT"))
	 (lines (iter-yield-from
		 (vm-imap-net-command
		  (format "UID STORE %s %s%s %s" uid sign suffix
			  (vm-imap-flag-list-string flags))
		  (format "UID STORE %s%s" sign suffix))))
	 (reported nil))
    (when checking
      (dolist (response lines)
	(when (vm-imap-response-matches response '* 'atom 'FETCH 'list)
	  (setq reported (cons t (vm-imap-fetch-response-flags response))))))
    reported))

(defun vm-imap-net-note-dropped-flags (wanted reported)
  "Complain about each of WANTED that REPORTED does not have, once per session.
The server said OK and did not keep it.  REPORTED is what
`vm-imap-net-store-flags-1\\=' answered: nil where the server said nothing,
which is a server that did not answer the question rather than one that
dropped anything.

The flag is not remembered as refused and is offered again: unlike a refusal,
which is an error the server means, this is a difference of opinion about what
a mailbox can hold, and a mailbox that gains the ability keeps working."
  (when reported
    (let* ((have (cdr reported))
	   (lost (seq-filter (lambda (flag)
			       (and (vm-imap-keyword-p flag)
				    (not (member (downcase flag) have))
				    (not (member flag vm-imap-dropped-flags))))
			     wanted)))
      (when lost
	(setq vm-imap-dropped-flags (append lost vm-imap-dropped-flags))
	(vm-net-warn 0 (concat "IMAP server accepted and discarded the label%s"
			       " %s: set here, absent there.  Gmail does this"
			       " with every label; see the manual under Gmail")
		     (if (cdr lost) "s" "")
		     (mapconcat #'identity lost ", "))))))

(iter-defun vm-imap-net-store-flags (sign uid flags)
  "Store FLAGS, one command if the server will take them, singly if not.
Answers with the flags it accepted.

A server need not accept every keyword, and Exchange refuses the whole STORE
when it meets one it does not know, so a single unknown keyword would
otherwise stop \\Deleted and everything else in the same command from being
stored (issue #391).  What is refused on its own is remembered in
`vm-imap-refused-flags\\=' and not offered again this session; a refusal of
every flag is re-signalled, which leaves the message pending for a later try
(issue #270)."
  (let ((wanted (seq-remove (lambda (flag) (member flag vm-imap-refused-flags))
			    flags))
	(accepted nil)
	(refused nil)
	(failure nil))
    (when wanted
      (let ((error-data nil))
	(condition-case caught
	    (let ((reported (iter-yield-from
			     (vm-imap-net-store-flags-1 sign uid wanted))))
	      (setq accepted wanted)
	      ;; Only for the adding direction: a keyword still there after a
	      ;; removal is a different fault, and not one Gmail has.
	      (when (equal sign "+")
		(vm-imap-net-note-dropped-flags wanted reported)))
	  (vm-imap-normal-error (setq error-data caught)))
	(when error-data
	  ;; the server refused the lot; find out what it will take, unless
	  ;; there was only one, which has just been refused on its own
	  (dolist (flag (if (cdr wanted) wanted nil))
	    (let ((one-failed nil))
	      (condition-case caught
		  (let ((reported (iter-yield-from
				   (vm-imap-net-store-flags-1
				    sign uid (list flag)))))
		    (when (equal sign "+")
		      (vm-imap-net-note-dropped-flags (list flag) reported)))
		(vm-imap-normal-error (setq one-failed caught)))
	      (if one-failed
		  (progn (push flag refused)
			 (push flag vm-imap-refused-flags))
		(push flag accepted))))
	  (when (and refused (cdr wanted))
	    (vm-net-warn 1 "IMAP server refuses the flag%s %s; not sending %s again"
		     (if (cdr refused) "s" "")
		     (mapconcat #'identity (reverse refused) ", ")
		     (if (cdr refused) "them" "it")))
	  (unless (cdr wanted)
	    ;; the single flag that was refused, remembered without a second ask
	    (setq refused wanted)
	    (setq vm-imap-refused-flags (append wanted vm-imap-refused-flags))
	    (vm-net-warn 1 "IMAP server refuses the flag %s; not sending it again"
		     (car wanted)))
	  (unless accepted
	    (setq failure error-data)))))
    (when failure
      (signal (car failure) (cdr failure)))
    accepted))

(defvar vm-imap-net-told-about-keywords nil
  "Whether this folder has already said its server will not keep keywords.
Buffer-local to the folder, and said once a session rather than once a label:
a folder of four hundred messages would otherwise say it four hundred times.")
(make-variable-buffer-local 'vm-imap-net-told-about-keywords)

(defun vm-imap-net-keywords-in (flags)
  "The members of FLAGS that are keywords rather than system flags.
A system flag begins with a backslash; anything else is a keyword of the
server's own, which is what a VM label becomes."
  (seq-remove (lambda (flag) (string-prefix-p "\\" flag)) flags))

(defun vm-imap-net-keeps-keywords-p ()
  "Whether this folder's server said it keeps keywords of its own.
That is the `\\*' of PERMANENTFLAGS, RFC 3501 6.3.1.  Answers t when the
folder has no PERMANENTFLAGS recorded, so nothing is claimed about a server
that did not say."
  (let ((permanent (vm-folder-imap-permanent-flags)))
    (or (null permanent)
        (and (member "\\*" permanent) t))))

(defun vm-imap-net-tell-about-keywords (folder flags)
  "Say once that FOLDER's server will not keep the keywords in FLAGS.

A server that does not advertise `\\*' in PERMANENTFLAGS is saying it keeps
no keywords of its own.  It takes the STORE all the same and answers OK, so
nothing here fails and nothing is refused: the label is simply not there the
next time the mailbox is read.  Gmail is such a server, which is
emacs-vm/vm#601.

Worded as what the server said rather than as a certainty, because
PERMANENTFLAGS can be wrong both ways: a server may leave a keyword out of it
and store the keyword anyway, or advertise `\\*' and keep nothing.  That is
why it warns and changes nothing.

`vm-imap-net-note-dropped-flags' says the same thing after the fact, having seen
a keyword come back missing.  This says it at the moment the label is sent,
which is where the reader still has the label in front of them.

Said where a label is actually at risk rather than at every visit, so a
reader who sets none is not told about a limit that does not touch them."
  (when (buffer-live-p folder)
    (with-current-buffer folder
      (let ((keywords (vm-imap-net-keywords-in flags)))
        (when (and keywords
                   (not vm-imap-net-told-about-keywords)
                   (not (vm-imap-net-keeps-keywords-p)))
          (setq vm-imap-net-told-about-keywords t)
          (vm-net-warn 1 (concat "%s: this server does not offer \\* in"
                                 " PERMANENTFLAGS, which is how it says it"
                                 " keeps no labels of its own, so %s may be"
                                 " gone when the folder is read again")
                       (buffer-name folder)
                       (mapconcat #'identity keywords ", ")))))))

(iter-defun vm-imap-net-save-message-flags (folder message)
  "Send MESSAGE's flags to the server, and note what it took.
Answers t when something was sent.  The change itself is worked out in the
folder by `vm-imap-message-flag-changes\\=', the same function the blocking
path uses; only the sending of it is here."
  (let* ((changes (and (buffer-live-p folder)
		       (with-current-buffer folder
			 (vm-imap-message-flag-changes message))))
	 (uid (with-current-buffer (or (and (buffer-live-p folder) folder)
				       (current-buffer))
		(vm-imap-uid-of message)))
	 (number (nth 0 changes))
	 (cached-flags (nth 1 changes))
	 (flags+ (nth 2 changes))
	 (flags- (nth 3 changes)))
    (when number
      (when flags+
	;; A server that does not advertise \* takes a keyword and does not
	;; keep it, so nothing fails here and the label is gone by the next
	;; read.  Said before the STORE, since the STORE will answer OK (#601).
	(vm-imap-net-tell-about-keywords folder flags+)
	;; only what the server took goes in the cache, or the next sync would
	;; think a refused flag was already there
	(nconc cached-flags
	       (iter-yield-from (vm-imap-net-store-flags "+" uid flags+))))
      (when flags-
	(dolist (flag (iter-yield-from
		       (vm-imap-net-store-flags "-" uid flags-)))
	  (delete flag cached-flags)))
      ;; the folder may have been quit while this session ran: the file is
      ;; written by then and still says the flags are unsent, which costs one
      ;; STORE of flags the server already has the next time it is visited
      (when (buffer-live-p folder)
	(with-current-buffer folder
	  (vm-set-attribute-modflag-of message nil)))
      t)))

(iter-defun vm-imap-net-save-flags (folder &optional all)
  "Send the flags of every message in FOLDER whose own have changed.
Answers with how many were sent.  A message the server refuses is counted as
an error and left with its modification flag set, so the next synchronisation
tries it again, and the rest are still sent.

ALL sends every message's flags, changed or not, which is what a full
synchronisation asks for: `vm-imap-net-save-attributes' with `:all-flags'."
  (let ((messages (and (buffer-live-p folder)
		       (with-current-buffer folder
			 (seq-filter
			  (lambda (message)
			    (and (or all (vm-attribute-modflag-of message))
				 (equal (vm-imap-uid-validity-of message)
					(vm-folder-imap-uid-validity))))
			  vm-message-list))))
	(saved 0)
	(errors 0))
    (dolist (message messages)
      (let ((failed nil))
	(condition-case caught
	    (when (iter-yield-from (vm-imap-net-save-message-flags folder message))
	      (setq saved (1+ saved)))
	  (vm-imap-normal-error (setq failed caught)))
	(when failed
	  (setq errors (1+ errors)))))
    (when (> errors 0)
      (vm-net-warn 1 "%s: %d message%s whose flags the server would not take"
	       (if (buffer-live-p folder) (buffer-name folder) "folder")
	       errors (if (= errors 1) "" "s")))
    saved))

(declare-function vm-thoughtfully-select-message "vm-folder" ())
(declare-function vm-present-current-message "vm-page" ())
(declare-function vm-arrival-blurb "vm-folder" (count))


(defun vm-imap-net-show-arrival (folder count)
  "Say that COUNT messages arrived in FOLDER, and show one of them.
What `vm-get-new-mail' does when mail arrives, done when it arrives rather
than when the command was typed.  A folder that was empty has no current
message until this runs, and every command that works on the current message
would have nothing to work on."
  (with-current-buffer folder
    ;; Built before the selection below, which reads a message and alters the
    ;; new count, as the synchronous path builds its blurb first for the same
    ;; reason.
    (let ((blurb (vm-arrival-blurb count)))
      ;; If the reader is still sitting where the fetch put them to keep the
      ;; folder usable, that was a guess made from part of a folder and this
      ;; is the moment to make it again with all of it (#799).  If they have
      ;; moved, the guess is theirs to keep and nothing here disturbs it.
      (when (and vm-imap-net-provisional-message
		 (eq vm-imap-net-provisional-message (car vm-message-pointer)))
	(setq vm-message-pointer nil))
      (setq vm-imap-net-provisional-message nil)
      (if (vm-thoughtfully-select-message)
	  (vm-present-current-message)
	(vm-update-summary-and-mode-line))
      (vm-net-inform 5 "%s" blurb))))

(defvar vm-imap-net-session nil
  "The session this folder has running, if it has one.
A folder runs one at a time: two writing into it would interleave what they
put there.")
(make-variable-buffer-local 'vm-imap-net-session)

(defvar vm-imap-net-waiting nil
  "What this folder is to do when the session it has running ends.
A list of (NAME . FUNCTION), oldest first.  A folder writes what a session
tells it to -- messages, flags, expunges -- and two sessions doing that at
once would interleave their writes into one buffer and one cache file.  So
the second asks to be run afterwards rather than opening a connection of its
own.")
(make-variable-buffer-local 'vm-imap-net-waiting)

(defun vm-imap-net-take-session (session &optional iterator)
  "Record SESSION as this folder's, start ITERATOR as its work, and answer it.

ITERATOR is started after the folder has refused a second session rather than
before, so a second one is prevented instead of reported: the sites here called
`vm-net-start' first, and the error then arrived with a session already
talking to a server and the folder holding no record of it -- unowned, so
`vm-imap-net-busy-p' could not see it, the mode line did not show it and
`vm-imap-net-stop' could not stop it.  Two IMAP maildrops as spool sources for
one folder did exactly that.
Every start goes through here: the slot is what `vm-imap-net-busy-p' reads,
what is queued behind it would otherwise never run, and the mode line of every
buffer showing this folder says what it is doing."
  (let ((folder (current-buffer)))
    (when (and vm-imap-net-session
	       (not (eq vm-imap-net-session session))
	       (vm-net-session-live-p vm-imap-net-session))
      ;; The queue is what keeps this from happening; this is what says so if
      ;; a path is ever added that does not go through it.  Two sessions
      ;; writing one folder is how a folder gets two sets of messages, flags
      ;; and expunges in one buffer and one cache file.
      (error "%s: a second session was started while %s was running"
	     (buffer-name folder)
	     (or (vm-net-session-name vm-imap-net-session) "one")))
    (setq vm-imap-net-session session)
    (vm-imap-net-show-session)
    (vm-net-at-end session
		   (lambda ()
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (vm-imap-net-run-next)
			 (vm-imap-net-show-session)))))
    (when iterator
      (vm-net-start session iterator))
    session))

(defvar vm-ml-session)

(defvar vm-imap-net-phase nil
  "What the session running in this folder is doing, as a word, or nil.
Shown in place of the word the session name gives.  A fetch is several things
in a row and only one of them is fetching: the connection, then the list of
what the server holds, then the messages themselves.")
(make-variable-buffer-local 'vm-imap-net-phase)

(defvar vm-imap-net-progress nil
  "How far the session running in this folder has got, as (DONE . TOTAL).

A folder runs one session at a time, so one pair says it.  What the fetch says
in the echo area is gone by the next message; this is in the mode line beside
what the folder is doing, where it stays until the next bunch moves it on:
the word alone, on a mailbox of six thousand, says nothing about whether it
is getting anywhere.")
(make-variable-buffer-local 'vm-imap-net-progress)

(defun vm-imap-net-note-progress (folder done total &optional phase)
  "Say that FOLDER's session has done DONE of TOTAL, and show it.
PHASE is a word for what it is doing, shown in place of the word the session
name gives -- \"listing\" while the server is being asked what it holds, which
on a mailbox of thousands is the long wait before anything arrives.  Nil,
which is what the fetch itself passes, puts the session's own word back."
  (when (buffer-live-p folder)
    (with-current-buffer folder
      (setq vm-imap-net-phase phase)
      (setq vm-imap-net-progress (and total (> total 0) (cons done total)))
      (vm-imap-net-show-session))))

(defun vm-imap-net-show-session ()
  "Say in the mode line what this folder is doing with its server.

The folder buffer, its summary and its presentation all show it: a reader
looking at the summary is looking at a folder that is being written into, and
the folder buffer may not be on screen at all.  What is queued is counted, so
\" IMAP fetch +2\" is a fetch running with two things waiting for it, and how
far it has got is there once it knows: \" fetching 24/340\"."
  (let* ((session vm-imap-net-session)
	 (running (and session (vm-net-session-live-p session)
		       (vm-net-session-doing (vm-net-session-name session))))
	 (progress (and running vm-imap-net-progress))
	 (waiting (length vm-imap-net-waiting)))
    (unless running
      (setq vm-imap-net-progress nil)
      (setq vm-imap-net-phase nil))
    (setq vm-ml-session
	  (and running
	       (propertize (concat " " (or (and running vm-imap-net-phase)
					   running)
				   (if progress
				       (format " %d/%d" (car progress)
					       (cdr progress))
				     "")
				   (if (> waiting 0)
				       (format " +%d" waiting)
				     "")
				   " ")
			   'face 'vm-net-session-face)))
    ;; registered before the update, because the update copies the mode line
    ;; into the summary and presentation only for a folder that asked for one:
    ;; without this the summary kept whatever it was told last, so a folder
    ;; said " fetching " while its summary still said " fetching 0/1 "
    (intern (buffer-name) vm-buffers-needing-display-update)
    (vm-update-summary-and-mode-line)))

(defun vm-imap-net-stop ()
  "Stop what this folder is doing with its server, and forget what is queued.

For a folder that is going away: quitting writes the file and kills the
buffer, and a session that went on writing into it would be writing into
nothing.  Nothing is lost that is not still on the server -- messages fetched
but not saved are fetched again next time, and flags that did not go up keep
their modification flag in the file.

The session is abandoned rather than dropped, so its `unwind-protect' forms
run: the LOGOUT is said, and a server told nothing is a server that keeps the
connection until it times out."
  (let ((session vm-imap-net-session)
	(waiting (length vm-imap-net-waiting)))
    (setq vm-imap-net-waiting nil)
    (when (and session (vm-net-session-live-p session))
      (vm-net-inform 5 "%s: stopping %s%s" (buffer-name)
		 (or (vm-net-session-name session) "the session")
		 (if (> waiting 0)
		     (format " and %d more" waiting)
		   ""))
      (vm-net-abandon session))
    (setq vm-imap-net-session nil)
    (setq vm-ml-session nil)))

(defun vm-imap-net-run-next ()
  "Start the next thing this folder was waiting to do, if any.
Called when a session ends.  One at a time: what it starts becomes the
folder's session and the rest keep waiting behind it."
  (let ((next (car vm-imap-net-waiting)))
    (when next
      (setq vm-imap-net-waiting (cdr vm-imap-net-waiting))
      (vm-imap-net-show-session)
      (vm-net-inform 6 "%s: %s now that the folder is free"
		 (buffer-name) (car next))
      (condition-case reason
	  (funcall (cdr next))
	(vm-imap-net-no-password
	 (vm-imap-net-say-no-password (current-buffer) reason))
	(error
	 (vm-net-warn 0 "%s: %s: %s" (buffer-name) (car next)
		  (error-message-string reason)))))))

(defun vm-imap-net-when-free (name function)
  "Run FUNCTION now, or when this folder's session ends.  Answers non-nil.
NAME says what it is, for the log.  Answers `later' when it was queued: the
work has not happened yet, and it happens on the one connection this folder
has rather than on a second one."
  (cond
   ((vm-imap-net-busy-p)
    (setq vm-imap-net-waiting
	  (append vm-imap-net-waiting (list (cons name function))))
    (vm-imap-net-show-session)
    (vm-net-inform 6 "%s: %s when the session running now has finished"
	       (buffer-name) name)
    'later)
   (t
    (funcall function))))

;;; Bodies kept on the server

(declare-function vm-make-room-for-message-body "vm-folder" (mm))
(declare-function vm-settle-message-body "vm-folder" (mm modified))
(declare-function vm-headers-of "vm-message" (m))
(declare-function vm-text-end-of "vm-message" (m))
(declare-function vm-imap-uid-of "vm-message" (m))
(declare-function vm-text-of "vm-message" (m))
(declare-function vm-buffer-of "vm-message" (m))
(declare-function vm-mark-folder-modified-p "vm-folder" (&optional buffer))
(declare-function vm-preview-current-message "vm-page" ())

(iter-defun vm-imap-net-fetch-bodies (folder uids body-peek)
  "UID FETCH the bodies of UIDS, putting each where its own message is.
Answers with the UIDs the server answered for.  One command for all of them,
and the UID in each response says which message it is: a server may answer
in any order (issue #185)."
  (let ((source (current-buffer))
	(fetched nil))
    (vm-imap-net-send
     (format "UID FETCH %s %s" (mapconcat #'identity uids ",")
	     (if body-peek "(UID BODY.PEEK[])" "(UID RFC822.PEEK)")))
    (let ((done nil)
	  response)
      (while (not done)
	(setq response (vm-imap-net-verify-response
			(vm-imap-net-read-a-response) "UID FETCH"))
	(cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	       (let ((message (vm-imap-net-fetch-message-text response)))
		 ;; nil for a FETCH the server sent to report a message's
		 ;; flags rather than to answer this one, as `vm-imap-net-fetch'
		 ;; passes over too: handing its nil UID on raised "FETCH
		 ;; response for a UID that was not asked for" and lost the
		 ;; rest of the fetch (emacs-vm/vm#890)
		 (when message
		   (let ((uid (nth 0 message)))
		     (vm-imap-net-store-body folder source uid
					     (nth 1 message) (nth 2 message))
		     (push uid fetched)))))
	      ((vm-imap-response-matches response 'VM 'OK)
	       (setq done t)))))
    (nreverse fetched)))

(defun vm-imap-net-message-by-uid (folder uid)
  "The message in FOLDER whose IMAP UID is UID, or nil."
  (with-current-buffer folder
    (seq-find (lambda (message) (equal (vm-imap-uid-of message) uid))
	      vm-message-list)))

(defun vm-imap-net-store-body (folder source uid start end)
  "Put the body between START and END of SOURCE into its message in FOLDER."
  (vm-imap-net-require-folder folder)
  (let ((message (vm-imap-net-message-by-uid folder uid)))
    (unless message
      (vm-imap-protocol-error "FETCH response for a UID that was not asked for"))
    (with-current-buffer folder
      (let ((inhibit-read-only t)
	    (buffer-undo-list t)
	    (modified (buffer-modified-p)))
	(save-excursion
	  (save-restriction
	    (widen)
	    (narrow-to-region (marker-position (vm-headers-of message))
			      (marker-position (vm-text-end-of message)))
	    (vm-make-room-for-message-body message)
	    (insert-buffer-substring source start end)
	    (vm-imap-cleanup-region (vm-text-of message) (point-max))
	    (vm-settle-message-body message modified)))))))

(iter-defun vm-imap-net-load (folder mailbox user password uids)
  "Log in, select MAILBOX, and fetch the bodies of UIDS into FOLDER."
  (unwind-protect
      (progn
	(let* ((capabilities (iter-yield-from (vm-imap-net-open-session user password)))
    	       (body-peek (and (memq 'IMAP4REV1 (car capabilities)) t)))
	  (iter-yield-from (vm-imap-net-select mailbox))
	  (iter-yield-from (vm-imap-net-fetch-bodies folder uids body-peek))))
    (vm-imap-net-logout)))

(defun vm-imap-net-load-bodies (messages callback)
  "Fetch the bodies of MESSAGES, which are the current folder's, and tell
CALLBACK how many arrived.  Nothing waits.

Signals `vm-imap-net-no-password' where VM has no password yet, which
means the work does not start: a command asks for a password, and a
timer has nobody to ask."
  (let* ((folder (current-buffer))
	 (validity (vm-folder-imap-uid-validity))
	 ;; What is still here: this can have waited behind a session that
	 ;; expunged locally, and a body fetched for a message the folder no
	 ;; longer has has nowhere to go
	 (messages (seq-filter (lambda (message)
				 (and (memq message vm-message-list)
				      (not (eq (vm-deleted-flag message)
					       'expunged))))
			       messages))
	 (uids (mapcar (lambda (message)
			 (unless (equal (vm-imap-uid-validity-of message)
					validity)
			   (error "Message has an invalid UID"))
			 (vm-imap-uid-of message))
		       messages))
	 (opened (vm-imap-net-open (vm-folder-imap-maildrop-spec) "IMAP fetch"
				   'may-ask))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-imap-net-done-with-buffer buffer)
	    (when (buffer-live-p folder)
	      (with-current-buffer folder
		(funcall callback (or (vm-net-session-error finished)
				      (length (vm-net-session-value finished))))))))
    ;; the folder's one session slot: what `vm-imap-net-busy-p' reports and
    ;; what a caller that has to have the bodies waits on
    (vm-imap-net-take-session session
		  (vm-imap-net-load folder (nth 1 opened) (nth 2 opened)
				    (nth 3 opened) uids))
    session))

(defun vm-imap-net-load-message-bodies (messages)
  "Start fetching the bodies of MESSAGES, and answer with whether it did.
They must all be in one folder.  Nil means nothing was started, VM having no
password for the maildrop yet.

With a session already running on the folder this waits for it rather than
opening a second connection into the same buffer, and answers `later\\=': the
bodies arrive when the fetch in front of them is done.

Each message is marked as no longer needing its body only when the body is
there, so a message the server did not answer for is asked for again rather
than left empty."
  (let ((folder (vm-buffer-of (car messages))))
    (with-current-buffer folder
      (condition-case reason
	  (vm-imap-net-when-free
	   (format "fetching %d message bod%s"
		   (length messages) (if (cdr messages) "ies" "y"))
	   (lambda ()
	     (vm-imap-net-load-bodies
	      messages
	      (lambda (result)
		(cond ((vm-net-error-p result)
		       (vm-net-warn 0 "%s: %s" (buffer-name folder)
				(error-message-string result)))
		      (t
		       (vm-mark-folder-modified-p folder)
		       (vm-update-summary-and-mode-line)
		       (vm-preview-current-message)
		       (vm-net-inform 5 "%s: %d message bod%s loaded"
				  (buffer-name folder) result
				  (if (= result 1) "y" "ies"))))))
	     (vm-net-inform 6 "%s: fetching %d message bod%s without waiting"
			(buffer-name folder)
			(length messages) (if (cdr messages) "ies" "y"))
	     t))
	(vm-imap-net-no-password
	 (vm-imap-net-say-no-password folder reason)
	 nil)))))

;;; Saving a message to a mailbox

(declare-function vm-imap-subst-CRLF-for-LF "vm-imap" (string))
(declare-function vm-replied-flag "vm-message" (m))
(declare-function vm-unread-flag "vm-message" (m))

(defun vm-imap-net-message-text (message)
  "MESSAGE as it goes on the wire: headers and body, CRLF for LF.

Unibyte, so that its length is the octet count the APPEND literal announces.
A folder buffer holds bytes, but `vm-imap-subst-CRLF-for-LF' works in a
multibyte buffer of its own, where every byte over 0x7F comes back as a
character `string-bytes' counts as two: VM promised the server more octets
than it sent, the server waited for a literal that had finished and read the
next command line as message data, and the session was lost.  Every save of
a message with an eight-bit header or body went that way (#887)."
  (with-current-buffer (vm-buffer-of message)
    (save-restriction
      (widen)
      (encode-coding-string
       (vm-imap-subst-CRLF-for-LF
	(buffer-substring (vm-headers-of message) (vm-text-end-of message)))
       'binary))))

(defun vm-imap-net-message-flags (message)
  "The flags to store MESSAGE under, as a list of IMAP flag names.

The system flags, and the labels and the attributes that travel as keywords:
`filed', `written', `forwarded' and `redistributed' are keywords on the
sync path too, so a saved copy carries what a synchronised message carries
(emacs-vm/vm#828).

Which of the keywords are sent is the destination mailbox's to say, in its
PERMANENTFLAGS; `vm-imap-net-flags-a-mailbox-takes' decides it.  An APPEND
naming a flag the server does not support can be answered NO, and that loses
the copy rather than the flag.

Not \\Deleted: a message is not saved into a mailbox in order to be deleted
from it."
  (let ((flags nil))
    (when (vm-replied-flag message) (push "\\Answered" flags))
    (when (vm-flagged-flag message) (push "\\Flagged" flags))
    (unless (vm-unread-flag message) (push "\\Seen" flags))
    (when (vm-filed-flag message) (push "filed" flags))
    (when (vm-written-flag message) (push "written" flags))
    (when (vm-forwarded-flag message) (push "forwarded" flags))
    (when (vm-redistributed-flag message) (push "redistributed" flags))
    (append (nreverse flags) (copy-sequence (vm-decoded-labels-of message)))))

(defun vm-imap-net-flags-a-mailbox-takes (flags permanent)
  "Those of FLAGS that a mailbox whose PERMANENTFLAGS are PERMANENT will keep.

The system flags always: every server has them.  A keyword only where the
mailbox says it takes one, which is `\\*' in PERMANENTFLAGS or the keyword
named there itself.  A refused APPEND costs the copy and not the flag, which
is why this is decided before sending rather than after.

A mailbox that said nothing about its permanent flags is taken the other
way, as `vm-imap-net-keeps-keywords-p' takes it: nothing is claimed about a
server that did not say, so everything goes."
  (if (or (null permanent) (member "\\*" permanent))
      flags
    (seq-filter (lambda (flag)
                  (or (not (vm-imap-keyword-p flag))
                      (member-ignore-case flag permanent)))
                flags)))

(iter-defun vm-imap-net-append (mailbox text flags)
  "APPEND TEXT to MAILBOX with FLAGS, as a literal.
The server answers the command line with a `+' before the octets are sent,
which is the one place IMAP asks the client to wait for permission to
speak."
  (vm-imap-net-send (format "APPEND %s %s {%d}"
			    (vm-imap-quote-mailbox-name mailbox)
			    flags (string-bytes text)))
  (let ((ready nil)
	response)
    (while (not ready)
      (setq response (vm-imap-net-verify-response
		      (vm-imap-net-read-a-response) "APPEND"))
      (when (vm-imap-response-matches response '+)
	(setq ready t))))
  (let ((process (get-buffer-process (current-buffer))))
    (goto-char (point-max))
    (insert-before-markers "<message omitted>\r\n")
    (setq vm-imap-net-read-point (point))
    (process-send-string process (concat text "\r\n")))
  (let ((done nil)
	response)
    (while (not done)
      (setq response (vm-imap-net-verify-response
		      (vm-imap-net-read-a-response) "APPEND data"))
      (when (vm-imap-response-matches response 'VM 'OK)
	(setq done t))))
  t)

(iter-defun vm-imap-net-mailbox-permanent-flags (mailbox)
  "What MAILBOX says it keeps, or nil where it would not say.
EXAMINE rather than SELECT: nothing here writes to the mailbox, and this
session is the save's own, so selecting in it takes no folder's mailbox
away.  A mailbox that cannot be examined answers nil, which sends the flags
a server is obliged to keep and no others.

A named variable, and not `(condition-case nil ...)': inside an `iter-defun'
whose protected form yields, that answers with the error object rather than
with the handler's value, which is generator.el's CPS transform and not what
the same code means outside one.  A mailbox that could not be
examined therefore looked like one that keeps no keywords, and every label
was dropped from the APPEND (emacs-vm/vm#889)."
  (condition-case _err
      (nth 5 (iter-yield-from (vm-imap-net-select mailbox 'examine)))
    (vm-imap-normal-error nil)))

(iter-defun vm-imap-net-save (user password mailbox messages)
  "Log in and APPEND each of MESSAGES to MAILBOX, and answer with how many.
The mailbox is created if the server does not have it, its refusal to create
one it already has being no reason to stop.

Each message carries the flags it was read with, less the keywords the
mailbox says it will not keep: a refused APPEND loses the copy rather than
the flag, so what the destination takes is asked before anything is sent
(emacs-vm/vm#828)."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let ((error-data nil))
	  (condition-case caught
    	      (iter-yield-from (vm-imap-net-command
    				(format "CREATE %s"
    					(vm-imap-quote-mailbox-name mailbox))
    				"CREATE"))
	    (vm-imap-normal-error (setq error-data caught)))
	  (ignore error-data))
	(let ((permanent (iter-yield-from
			  (vm-imap-net-mailbox-permanent-flags mailbox)))
	      (dropped nil)
	      (saved 0))
	  (dolist (message messages)
	    (let* ((wanted (cdr message))
		   (sending (vm-imap-net-flags-a-mailbox-takes wanted
							       permanent)))
	      (dolist (flag wanted)
		(unless (member flag sending) (push flag dropped)))
	      (iter-yield-from
	       (vm-imap-net-append mailbox (car message)
				   (vm-imap-flag-list-string sending))))
	    (setq saved (1+ saved)))
	  (when dropped
	    (vm-net-warn 1 (concat "Saved into %s without %s, which its"
				   " PERMANENTFLAGS says it does not keep")
			 mailbox
			 (mapconcat #'identity
				    (delete-dups (nreverse dropped)) ", ")))
	  saved))
    (vm-imap-net-logout)))

(defun vm-imap-net-save-messages (source mailbox messages callback)
  "Save MESSAGES into MAILBOX on SOURCE, and tell CALLBACK how many went.

The text and flags of each message are taken now, in the folder they are in;
what the session sends is that copy, so the folder is free to change while
it goes.  Signals `vm-imap-net-no-password' where VM has no password
open."
  (let* ((folder (current-buffer))
	 (copies (mapcar (lambda (message)
			   (cons (vm-imap-net-message-text message)
				 (vm-imap-net-message-flags message)))
			 messages))
	 (opened (vm-imap-net-open source "IMAP save" 'may-ask))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-imap-net-done-with-buffer buffer)
	    (when (buffer-live-p folder)
	      (with-current-buffer folder
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))))
    (vm-imap-net-take-session session
		  (vm-imap-net-save (nth 2 opened) (nth 3 opened)
				    mailbox copies))
    session))

(defun vm-imap-net-append-text (spec mailbox text &optional flags may-ask)
  "APPEND TEXT to MAILBOX on SPEC without waiting, and answer whether it did.
FLAGS is a list of flag names, of which the mailbox is sent what it says it
will keep.

For filing a composition as it is sent: there is no folder here whose session
could be borrowed and none whose buffer could be written, so this session
belongs to nobody and is not the folder queue's business.  TEXT is taken as it
is; the composition buffer is free to go.

Nil means nothing was started, VM having no password for the maildrop yet.  A
failure afterwards is a warning: the message has been sent by then, and the
copy is what did not arrive."
  (condition-case nil
      (let* ((opened (vm-imap-net-open spec "IMAP FCC" may-ask))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session))
	     (name (or (vm-imap-account-name-for-spec spec)
		       (vm-safe-imapdrop-string spec))))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-imap-net-done-with-buffer buffer)
		(if (vm-net-session-error finished)
		    (vm-net-warn 0 "Not filed in %s on %s: %s" mailbox name
			     (error-message-string
			      (vm-net-session-error finished)))
		  (vm-net-inform 6 "Filed in %s on %s" mailbox name))))
	(vm-net-start session
		      (vm-imap-net-save (nth 2 opened) (nth 3 opened) mailbox
					(list (cons text flags))))
	(vm-net-inform 6 "Filing in %s on %s without waiting" mailbox name)
	t)
    (vm-imap-net-no-password nil)))

(declare-function vm-imap-decode-mailbox-name "vm-imap" (name))
(declare-function vm-imap-scan-list-for-flag "vm-imap" (list flag))

(iter-defun vm-imap-net-mailbox-list (&optional selectable-only)
  "Ask what mailboxes the account has, and answer with their names.
SELECTABLE-ONLY leaves out the ones the server marks \\Noselect, which are
the directories of a hierarchy and not mailboxes to be read."
  (let ((lines (iter-yield-from (vm-imap-net-command "LIST \"\" \"*\"" "LIST")))
	(names nil))
    (dolist (response lines)
      (when (vm-imap-response-matches response '* 'LIST 'list)
	(let ((flags (nth 2 response))
	      (name (nth 4 response)))
	  (when (and (memq (car name) '(atom string))
		     (not (and selectable-only
			       (vm-imap-scan-list-for-flag flags "\\Noselect"))))
	    (push (vm-imap-decode-mailbox-name
		   (buffer-substring (nth 1 name) (nth 2 name)))
		  names)))))
    (nreverse names)))

(iter-defun vm-imap-net-mailbox-status (mailbox)
  "Answer with (MESSAGES RECENT) for MAILBOX, or nil if the server will not say.
A mailbox that cannot be asked about is not an error worth stopping a listing
for: a server refuses STATUS on a name it has just listed often enough."
  (let ((lines nil)
	(counts nil)
	(refused nil))
    (condition-case caught
	(setq lines (iter-yield-from
		     (vm-imap-net-command
		      (format "STATUS %s (MESSAGES RECENT)"
			      (vm-imap-quote-mailbox-name mailbox))
		      "STATUS")))
      (vm-imap-normal-error (setq refused caught)))
    (unless refused
      (dolist (response lines)
	(when (or (vm-imap-response-matches response '* 'STATUS 'string 'list)
		  (vm-imap-response-matches response '* 'STATUS 'atom 'list))
	  (let ((items (cdr (nth 3 response)))
		(messages nil)
		(recent nil))
	    (while items
	      (cond ((vm-imap-response-matches items 'MESSAGES 'atom)
		     (setq messages (vm-imap-net-number (nth 1 items))
			   items (nthcdr 2 items)))
		    ((vm-imap-response-matches items 'RECENT 'atom)
		     (setq recent (vm-imap-net-number (nth 1 items))
			   items (nthcdr 2 items)))
		    (t (setq items (nthcdr 2 items)))))
	    (setq counts (list (or messages 0) (or recent 0)))))))
    counts))

(iter-defun vm-imap-net-list-session (user password)
  "Log in and answer with (MAILBOX MESSAGES RECENT) for every mailbox."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let ((names (iter-yield-from (vm-imap-net-mailbox-list)))
	      (listed nil)
	      (done 0))
	  (dolist (name names)
	    (let ((counts (iter-yield-from (vm-imap-net-mailbox-status name))))
	      (push (cons name (or counts (list 0 0))) listed)
	      (setq done (1+ done))
	      (vm-net-inform 6 "%d of %d mailboxes asked about" done (length names))))
	  (nreverse listed)))
    (vm-imap-net-logout)))

(defun vm-imap-net-list-folders (spec callback)
  "Ask SPEC's server what mailboxes it has, and tell CALLBACK.

CALLBACK is called with a list of (MAILBOX MESSAGES RECENT), or with the
error.  Answers whether the asking started; nil means nothing was started, VM
having no password for the maildrop yet.  A listing is a command per mailbox,
so it is the slowest thing VM asks a server for and the one worst spent
frozen."
  (condition-case nil
      (let* ((opened (vm-imap-net-open spec "IMAP folders" t))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session)))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-imap-net-done-with-buffer buffer)
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))
	(vm-net-start session
		      (vm-imap-net-list-session (nth 2 opened) (nth 3 opened)))
	t)
    (vm-imap-net-no-password nil)))

(iter-defun vm-imap-net-uids-session (user password mailbox)
  "Log in, EXAMINE MAILBOX, and answer with (UID-VALIDITY UIDS).
Examined and not selected: asking what a mailbox holds is not a reason to
mark anything seen."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let* ((select (iter-yield-from (vm-imap-net-select mailbox t)))
	       (count (nth 0 select))
	       (validity (nth 2 select))
	       (data (if (zerop count)
			 nil
		       (iter-yield-from (vm-imap-net-message-data 1 count)))))
	  (list validity (mapcar #'cadr data))))
    (vm-imap-net-logout)))

(defun vm-imap-net-mailbox-uids (source callback)
  "Ask SOURCE which messages it holds, and tell CALLBACK (UID-VALIDITY UIDS).

Answers whether the asking started; nil means nothing was started, VM having
no password for the maildrop yet.  CALLBACK is called with the error instead
when the session failed.  For the commands that compare what a folder
remembers against what the mailbox still has."
  (condition-case nil
      (let* ((opened (vm-imap-net-open source "IMAP uids" t))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session)))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-imap-net-done-with-buffer buffer)
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))
	(vm-net-start session
		      (vm-imap-net-uids-session (nth 2 opened) (nth 3 opened)
						(nth 1 opened)))
	t)
    (vm-imap-net-no-password nil)))

(iter-defun vm-imap-net-maildrop-expunge-session (user password mailbox uids)
  "Log in, select MAILBOX and delete the messages with UIDS.

Answers (UID-VALIDITY DELETED GONE): the UIDs this session expunged, and the
ones the mailbox does not have at all.  Both are settled, and the caller can
forget them: a UID the mailbox no longer holds is a deletion that has already
happened, and asking for it again is a session per expunge for ever.  This is
what `vm-imap-net-note-expunged' says of the folder's own list -- a UID that
no longer exists on the server is not a message anything need be told not to
fetch again.

A mailbox that cannot be deleted from signals rather than reporting that
nothing was there to delete."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let* ((select (iter-yield-from (vm-imap-net-select mailbox)))
	       (count (nth 0 select))
	       (validity (nth 2 select))
	       (writable (nth 3 select))
	       (can-delete (nth 4 select)))
	  (unless writable
	    (vm-imap-normal-error "mailbox %s is read-only" mailbox))
	  (unless can-delete
	    (vm-imap-normal-error "messages cannot be deleted in %s" mailbox))
	  (if (zerop count)
	      ;; an empty mailbox has none of them, so all of them are done
	      (list validity nil uids)
	    (let* ((data (iter-yield-from (vm-imap-net-message-data 1 count)))
		   (there (mapcar #'cadr data))
		   (wanted (seq-filter (lambda (uid) (member uid there)) uids))
		   (gone (seq-remove (lambda (uid) (member uid there)) uids)))
	      (when wanted
		(iter-yield-from (vm-imap-net-expunge wanted)))
	      (list validity wanted gone)))))
    (vm-imap-net-logout)))

(defun vm-imap-net-expunge-maildrop (source uids callback)
  "Delete the messages with UIDS from the maildrop SOURCE, without waiting.
CALLBACK is called with (UID-VALIDITY DELETED), or with the error.  Answers
whether it started."
  (condition-case nil
      (let* ((opened (vm-imap-net-open source "IMAP maildrop expunge" t))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session)))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-imap-net-done-with-buffer buffer)
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))
	(vm-imap-net-take-session session
		      (vm-imap-net-maildrop-expunge-session
		       (nth 2 opened) (nth 3 opened) (nth 1 opened) uids))
	t)
    (vm-imap-net-no-password nil)))

(defun vm-imap-net-expunge-maildrops (groups folder each done)
  "Work through GROUPS, one maildrop at a time, deleting what each names.

GROUPS is (SOURCE . UIDS) per maildrop.  EACH is called in FOLDER with the
source, the UID validity, the UIDs deleted and the UIDs the mailbox did not
have, as each maildrop answers, so that the folder forgets them then rather
than at the end; DONE is called with the maildrops that gave trouble, newest
first, when there are no more.

Answers whether the first maildrop started.  Nil means nothing was started, VM
having no password for that one yet, and no maildrop after it is answered for
either: half an expunge is worse than none.

One maildrop at a time and not all at once: they are separate servers as often
as not, but they all write the same folder, and what it remembers of one is not
to be rewritten while another is being answered for."
  (let (step)
    (setq step
	  (lambda (rest trouble first)
	    (cond
	     ((null rest)
	      (when (buffer-live-p folder)
		(with-current-buffer folder (funcall done trouble)))
	      t)
	     (t
	      (let* ((group (car rest))
		     (source (car group))
		     (name (or (vm-imap-folder-for-spec source)
			       (vm-safe-imapdrop-string source))))
		(vm-net-inform 6 "Expunging messages in %s..." name)
		(cond
		 ((vm-imap-net-expunge-maildrop
		   source (cdr group)
		   (lambda (result)
		     (cond
		      ((vm-net-error-p result)
		       (vm-net-warn 0 "%s: %s" name (error-message-string result))
		       (funcall step (cdr rest) (cons name trouble) nil))
		      (t
		       (when (buffer-live-p folder)
			 (with-current-buffer folder
			   (funcall each source (car result) (cadr result)
				    (nth 2 result))))
		       (funcall step (cdr rest) trouble nil)))))
		  t)
		 (first
		  ;; nothing has been started yet, so the caller is told that
		  ;; none of it was
		  nil)
		 (t
		  (vm-net-warn 0 "%s: not expunged, VM has no password for it" name)
		  (funcall step (cdr rest) (cons name trouble) nil))))))))
    (and groups (funcall step groups nil t))))

(iter-defun vm-imap-net-names-session (user password selectable-only)
  "Log in and answer with the account's mailbox names."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(iter-yield-from (vm-imap-net-mailbox-list selectable-only)))
    (vm-imap-net-logout)))

(defun vm-imap-net-mailbox-names (spec &optional selectable-only seconds)
  "The mailbox names of SPEC's account, or nil if the driver cannot ask.

This one waits, up to SECONDS, and says so: it is what completion is built
on, and completion has to answer with the names it has.  What it does not do
is open a second connection -- the session is the folder's own, and the wait
is `accept-process-output', so C-g still works.

SELECTABLE-ONLY leaves out the names the server marks \\Noselect."
  (let ((names nil)
	(answered nil))
    (when (vm-imap-net-list-names spec selectable-only
				  (lambda (result)
				    (setq answered t)
				    (unless (vm-net-error-p result)
				      (setq names result))))
      (let ((deadline (+ (float-time) (or seconds 30))))
	(while (and (not answered) (< (float-time) deadline))
	  (accept-process-output nil 0.05))))
    names))

(defun vm-imap-net-list-names (spec selectable-only callback)
  "Ask SPEC's server for its mailbox names and tell CALLBACK, without waiting.
Answers whether the asking started."
  (condition-case nil
      (let* ((opened (vm-imap-net-open spec "IMAP names" t))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session)))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-imap-net-done-with-buffer buffer)
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))
	(vm-net-start session
		      (vm-imap-net-names-session (nth 2 opened) (nth 3 opened)
						 selectable-only))
	t)
    (vm-imap-net-no-password nil)))

(iter-defun vm-imap-net-one-command-session (user password command purpose)
  "Log in, send COMMAND, and answer with what the server said.
PURPOSE names the command in an error message, as elsewhere here."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(iter-yield-from (vm-imap-net-command command purpose))
	t)
    (vm-imap-net-logout)))

(defun vm-imap-net-run-command (spec command purpose &optional may-ask done)
  "Send COMMAND to SPEC's server without waiting, and answer whether it did.

For the mailbox commands -- CREATE, DELETE, RENAME -- which are one command
each and belong to no folder: nothing is written into a buffer here, so there
is no folder session to queue behind.  DONE is called with t, or with the
error, when the server has answered.

Nil means nothing was sent, VM having no password for the maildrop yet."
  (condition-case nil
      (let* ((opened (vm-imap-net-open spec (format "IMAP %s" purpose) may-ask))
	     (session (car opened))
	     (buffer (vm-net-session-buffer session)))
	(setf (vm-net-session-finished session)
	      (lambda (finished)
		(let ((process (vm-net-session-process finished)))
		  (when (process-live-p process) (delete-process process)))
		(vm-imap-net-done-with-buffer buffer)
		(when done
		  (funcall done (or (vm-net-session-error finished) t)))))
	(vm-net-start session
		      (vm-imap-net-one-command-session
		       (nth 2 opened) (nth 3 opened) command purpose))
	t)
    (vm-imap-net-no-password nil)))

(defvar vm-imap-account-folder-cache)
(declare-function vm-delete "vm-misc" (predicate list &optional reverse))

(defun vm-imap-net-mailbox-command (spec command purpose said)
  "Send COMMAND to SPEC without waiting, and say SAID when it lands.

What the mailbox commands share: the account's folder cache is forgotten when
the server has done it, since it is that answer and not the asking that makes
the cache wrong.  Answers whether the command is on its way."
  (let ((account (vm-imap-account-name-for-spec spec)))
    (vm-imap-net-run-command
     spec command purpose t
     (lambda (result)
       (if (vm-net-error-p result)
	   (vm-net-warn 0 "%s failed: %s" purpose (error-message-string result))
	 (setq vm-imap-account-folder-cache
	       (vm-delete (lambda (entry) (equal (car entry) account))
			  vm-imap-account-folder-cache))
	 (vm-net-inform 5 "%s" said))))))

;;; Expunging on the server


(iter-defun vm-imap-net-expunge (uids)
  "Delete the messages with UIDS on the server, and expunge them.
Answers with how many were expunged.  Marked by UID and expunged in one
command each: a sequence number means something different after every
expunge, and a UID does not."
  (if (null uids)
      0
    (iter-yield-from
     (vm-imap-net-command
      (format "UID STORE %s +FLAGS.SILENT (\\Deleted)"
	      (mapconcat #'identity uids ","))
      "UID STORE"))
    (iter-yield-from (vm-imap-net-command "EXPUNGE" "EXPUNGE"))
    (length uids)))

(defvar vm-imap-messages-to-expunge)

(defun vm-imap-net-uids-to-expunge (uid-validity)
  "The UIDs the folder has expunged locally and the server still has.
The current buffer is the folder.  An entry whose UIDVALIDITY is not
UID-VALIDITY names a message on a mailbox that no longer exists as it was,
and is left alone."
  (let ((uids nil))
    (dolist (entry vm-imap-messages-to-expunge)
      (when (equal (cdr entry) uid-validity)
	(push (car entry) uids)))
    (nreverse uids)))

(defun vm-imap-net-note-expunged (uids)
  "Forget UIDS, the server having expunged them.
The current buffer is the folder.

Both lists the folder keeps them in: the requests, which are done, and what
the folder remembers having retrieved, which the blocking path clears here too
-- a UID that no longer exists on the server is not a message anything should
be told not to fetch again.  Leaving one of the two behind is how the same
folder ends up in a different state depending on which path did the work."
  (let ((validity (vm-folder-imap-uid-validity)))
    (setq vm-imap-retrieved-messages
	  (seq-remove (lambda (entry)
			(and (member (car entry) uids)
			     (equal (cadr entry) validity)))
		      vm-imap-retrieved-messages)))
  (setq vm-imap-messages-to-expunge
	(seq-remove (lambda (entry) (member (car entry) uids))
		    vm-imap-messages-to-expunge))
  (vm-set-folder-imap-mailbox-count
   (max 0 (- (or (vm-folder-imap-mailbox-count) 0) (length uids))))
  (vm-mark-folder-modified-p))

(iter-defun vm-imap-net-save-attributes-session (folder user password mailbox)
  "Log in, select MAILBOX, and send FOLDER's changed flags."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(iter-yield-from (vm-imap-net-select mailbox))
	(iter-yield-from (vm-imap-net-save-flags folder)))
    (vm-imap-net-logout)))

(defun vm-imap-net-save-attributes ()
  "Start sending this folder's changed flags to the server.
Answers with whether it did; nil means nothing was started, VM having no
password for the maildrop yet.  With a session already
running it is done when that one ends, and the answer is `later': a second
connection writing this folder's flags while the first is writing its
messages is what the queue exists to prevent."
  (let ((folder (current-buffer)))
    (vm-imap-net-when-free
     "sending the changed flags"
     (lambda ()
       (condition-case reason
	   (let* ((opened (vm-imap-net-open (vm-folder-imap-maildrop-spec)
					    "IMAP flags" 'may-ask))
		  (session (car opened))
		  (buffer (vm-net-session-buffer session)))
	     (setf (vm-net-session-finished session)
		   (lambda (finished)
		     (let ((process (vm-net-session-process finished)))
		       (when (process-live-p process) (delete-process process)))
		     (vm-imap-net-done-with-buffer buffer)
		     (when (buffer-live-p folder)
		       (with-current-buffer folder
			 (if (vm-net-session-error finished)
			     (vm-net-warn 0 "%s: %s" (buffer-name folder)
				      (error-message-string
				       (vm-net-session-error finished)))
			   (vm-net-inform 6 "%s: attributes updated on the server"
				      (buffer-name folder)))))))
	     (vm-imap-net-take-session session
			   (vm-imap-net-save-attributes-session
			    folder (nth 2 opened) (nth 3 opened) (nth 1 opened)))
	     t)
	 (vm-imap-net-no-password
	  (vm-imap-net-say-no-password folder reason)
	  nil))))))

(iter-defun vm-imap-net-send-changes-session (folder user password mailbox uids)
  "Log in, select MAILBOX, send FOLDER's changed flags and expunge UIDS.
The two in one session, and in that order: a message whose flags have changed
and which is also being deleted should go up with the flags it had."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(iter-yield-from (vm-imap-net-select mailbox))
	(iter-yield-from (vm-imap-net-save-flags folder))
	(when uids
	  (iter-yield-from (vm-imap-net-expunge uids))))
    (vm-imap-net-logout)))

(defun vm-imap-net-send-changes ()
  "Start sending what this folder owes its server, and answer with whether it
did.  Nil means nothing was sent, VM having no password for the maildrop yet.

What a save owes the server is what the reader changed: the flags of the
messages whose attributes moved, and the deletions the folder has been asked
to make.  Not the other direction -- what the server has expunged is worked
out by downloading the flags of every message in the mailbox, which on a
folder of six thousand took nineteen seconds with Emacs held still, and the
next fetch or `vm-imap-synchronize' works it out anyway.

The session outlives the folder buffer, which a quit kills as soon as the file
is written.  Nothing is lost by that: a message whose flags did not reach the
server still has its modification flag in the file, so the next session sends
them again.

Answers `later' when a session is already running.  That is not a nil: two
sessions writing to one mailbox would interleave, and the blocking path the
caller would fall back to is the wait this is here to remove.  The changes
keep their modification flags and go up next time."
  (let ((folder (current-buffer)))
    (vm-imap-net-when-free
     "sending this folder's changes"
     (lambda ()
       (condition-case reason
	   (let* ((uids (vm-imap-net-uids-to-expunge
			 (vm-folder-imap-uid-validity)))
		  (opened (vm-imap-net-open (vm-folder-imap-maildrop-spec)
					    "IMAP save" 'may-ask))
		  (session (car opened))
		  (buffer (vm-net-session-buffer session))
		  (name (buffer-name folder)))
	     (setf (vm-net-session-finished session)
		   (lambda (finished)
		     (let ((process (vm-net-session-process finished)))
		       (when (process-live-p process) (delete-process process)))
		     (vm-imap-net-done-with-buffer buffer)
		     (cond ((vm-net-session-error finished)
			    (vm-net-warn 0 "%s: %s" name
				     (error-message-string
				      (vm-net-session-error finished))))
			   (t
			    (when (and uids (buffer-live-p folder))
			      (with-current-buffer folder
				(vm-imap-net-note-expunged uids)))
			    (vm-net-inform 6 "%s: changes sent to the server" name)))))
	     (vm-imap-net-take-session session
			   (vm-imap-net-send-changes-session
			    folder (nth 2 opened) (nth 3 opened) (nth 1 opened)
			    uids))
	     (vm-net-inform 6 "%s: sending this folder's changes without waiting"
			(buffer-name folder))
	     t)
	 (vm-imap-net-no-password
	  (vm-imap-net-say-no-password folder reason)
	  nil))))))

(iter-defun vm-imap-net-expunge-session (user password mailbox uids)
  "Log in, select MAILBOX, and expunge UIDS from it."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(iter-yield-from (vm-imap-net-select mailbox))
	(iter-yield-from (vm-imap-net-expunge uids)))
    (vm-imap-net-logout)))

(defun vm-imap-net-expunge-remote-messages ()
  "Start expunging on the server what this folder has expunged locally.
Answers with whether it did; nil means nothing was started, VM having no
password for the maildrop yet.  With a session already running the expunge is
done when that one ends and the answer is `later'."
  (let* ((folder (current-buffer))
	 (pending (vm-imap-net-uids-to-expunge (vm-folder-imap-uid-validity))))
    (cond
     ((null pending) nil)
     (t
      (vm-imap-net-when-free
       (format "expunging %d message%s on the server"
	       (length pending) (if (= (length pending) 1) "" "s"))
       (lambda ()
	 (condition-case reason
	     ;; read again here, not from what was pending when this was asked
	     ;; for: a session that ran in between may have expunged some of
	     ;; them already
	     (let* ((uids (vm-imap-net-uids-to-expunge
			   (vm-folder-imap-uid-validity)))
		    (opened (vm-imap-net-open (vm-folder-imap-maildrop-spec)
					      "IMAP expunge" 'may-ask))
		    (session (car opened))
		    (buffer (vm-net-session-buffer session)))
	       (setf (vm-net-session-finished session)
		     (lambda (finished)
		       (let ((process (vm-net-session-process finished)))
			 (when (process-live-p process) (delete-process process)))
		       (vm-imap-net-done-with-buffer buffer)
		       (when (buffer-live-p folder)
			 (with-current-buffer folder
			   (if (vm-net-session-error finished)
			       (vm-net-warn 0 "%s: %s" (buffer-name folder)
					(error-message-string
					 (vm-net-session-error finished)))
			     (vm-imap-net-note-expunged uids)
			     (vm-net-inform 5 "%s: %d message%s expunged on the server"
					(buffer-name folder) (length uids)
					(if (= (length uids) 1) "" "s")))))))
	       (vm-imap-net-take-session session
			     (vm-imap-net-expunge-session
			      (nth 2 opened) (nth 3 opened) (nth 1 opened) uids))
	       t)
	   (vm-imap-net-no-password
	    (vm-imap-net-say-no-password folder reason)
	    nil))))))))

(defun vm-imap-net-get-mail (source callback &optional may-ask full-retrieve)
  "Fetch into the current folder what SOURCE has that it has not, and
tell CALLBACK.

CALLBACK is called in the folder buffer with the number of messages
fetched, or with the error that stopped the session.  Nothing waits: the
whole of it happens in the process filter, and the folder is left usable
while it does.

FULL-RETRIEVE asks for the messages the folder was given once and no longer
holds; see `vm-imap-net-plan\\='.

Signals `vm-imap-net-no-password' where VM has no password yet, and there is
nothing else to try: nobody can be asked for one from inside a filter."
  (let* ((folder (current-buffer))
	 (opened (vm-imap-net-open source "IMAP fetch" may-ask))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-imap-net-done-with-buffer buffer)
	    (when (buffer-live-p folder)
	      (with-current-buffer folder
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))))
    ;; the folder's session, so that what is queued behind it runs and nothing
    ;; else opens a second connection while this one writes -- taken before it
    ;; is started, so a second one is refused rather than reported
    (vm-imap-net-take-session session
			      (vm-imap-net-get-new-mail folder (nth 1 opened)
							(nth 2 opened)
							(nth 3 opened)
							nil nil
							full-retrieve))))

(declare-function vm-folder-imap-maildrop-spec "vm-folder" ())
(declare-function vm-inform "vm-misc" (level &rest args))

(defvar vm-mail-buffer)

(defun vm-imap-net-folder-buffer (&optional folder)
  "FOLDER, or the folder buffer the current buffer belongs to.
A summary or presentation buffer is not where the session is: the session is
the folder's, and those buffers name it in `vm-mail-buffer'."
  (or folder
      (and (boundp 'vm-mail-buffer) vm-mail-buffer
	   (buffer-live-p vm-mail-buffer) vm-mail-buffer)
      (current-buffer)))

(defun vm-imap-net-trace-buffers (&optional folder)
  "The session buffers a bug report about FOLDER should carry, newest first.

`vm-kept-imap-buffers', which holds the sessions that have ended, and the
buffer of the session still running if there is one.  A running session is not
in the ring yet, its buffer going there only when it ends, and it is the one a
reader is most likely reporting about: the report used to end the folder's
session to flush its trace, which on the driver would abort a fetch in flight
rather than tidily close an idle connection."
  (let* ((session (with-current-buffer (vm-imap-net-folder-buffer folder)
		    vm-imap-net-session))
	 (live (and session (vm-net-session-live-p session)
		    (vm-net-session-buffer session))))
    (seq-filter #'buffer-live-p
		(if (and live (not (memq live vm-kept-imap-buffers)))
		    (cons live vm-kept-imap-buffers)
		  vm-kept-imap-buffers))))

(defun vm-imap-net-busy-p (&optional folder)
  "Whether FOLDER, or the current buffer's folder, has a session running."
  (with-current-buffer (vm-imap-net-folder-buffer folder)
    (and vm-imap-net-session
	 (vm-net-session-live-p vm-imap-net-session))))

(defun vm-imap-net-say-no-password (folder _reason)
  "Record that FOLDER had no work started, VM having no password for it.
Nothing else can be done here: the password would have to be asked for, and
there is nobody to ask from inside a process filter.  A command run by the
reader asks and then this does not arise."
  (vm-net-inform 6 (concat "%s: not started, VM has no password for the"
			   " maildrop yet")
		 (if (bufferp folder) (buffer-name folder) folder)))

(defun vm-imap-net-synchronize (&optional full interactive)
  "Start synchronising this folder with its mailbox, and answer whether it did.

Everything `vm-imap-synchronize' does, on the driver: the folder's flags go
up, the mailbox's come down, what has arrived is fetched, what the server no
longer has is expunged here, and what the folder has expunged is expunged
there -- whether FULL is given or not, VM having recorded those expunges as
the reader made them.  FULL sends every message's flags rather than only those
that changed.

It used to delete on the server what the folder no longer holds, which a
damaged cache turned into losing mail (emacs-vm/vm#752).

Nil means nothing was started, VM having no password for the maildrop yet;
`later' that a session is running and this one goes when it ends.  INTERACTIVE
says a reader is there to be asked for a password."
  (let ((folder (current-buffer)))
    (vm-imap-net-when-free
     (if full "synchronising fully" "synchronising")
     (lambda ()
       (condition-case reason
	   (let* ((opened (vm-imap-net-open (vm-folder-imap-maildrop-spec)
					    "IMAP synchronize"
					    (eq interactive t)))
		  (session (car opened))
		  (buffer (vm-net-session-buffer session))
		  (name (buffer-name folder)))
	     (setf (vm-net-session-finished session)
		   (lambda (finished)
		     (let ((process (vm-net-session-process finished)))
		       (when (process-live-p process) (delete-process process)))
		     (vm-imap-net-done-with-buffer buffer)
		     (cond
		      ((vm-net-session-error finished)
		       (vm-net-warn 0 "%s: %s" name
				(error-message-string
				 (vm-net-session-error finished))))
		      ((buffer-live-p folder)
		       (with-current-buffer folder
			 (let ((arrived (or (vm-net-session-value finished) 0)))
			   (if (> arrived 0)
			       (vm-imap-net-show-arrival folder arrived)
			     (vm-update-summary-and-mode-line)
			     (vm-net-inform 5 "%s: synchronised" name))))))))
	     (vm-imap-net-take-session session
			   (vm-imap-net-get-new-mail
			    folder (nth 1 opened) (nth 2 opened) (nth 3 opened)
			    'attributes full))
	     (vm-net-inform 6 "%s: synchronising without waiting" name)
	     t)
	 (vm-imap-net-no-password
	  (vm-imap-net-say-no-password folder reason)
	  nil))))))

(defun vm-imap-net-get-spooled-mail (&optional interactive full)
  "Start fetching this IMAP folder's new mail, and answer with whether it did.

INTERACTIVE says a reader is there, and is what allows a password to be
asked for; it is `vm-get-spooled-mail's own argument.  So is FULL: fetch
what the folder was given once and no longer holds, which is what two prefix
arguments to `vm-get-new-mail' ask for.

Nil means nothing was started: VM has no password for the maildrop and
nobody can be asked for one from here.  `started' means the fetch is under
way and has not happened yet: this returns before it does, so the folder is
usable while it runs and says what arrived when it lands.

`started' and not t, because the caller says what the folder holds when this
answers and would otherwise say what it held before the fetch -- a visit that
reported the cached count as though the fetch were done, which reads as
nothing having happened (emacs-vm/vm#825).

A folder already fetching is left to finish.  The mail check runs from a
timer, and two fetches writing into one folder would interleave their
messages."
  (let ((folder (current-buffer)))
    (cond
     ((vm-imap-net-busy-p)
      (vm-net-inform 6 "%s: already fetching" (buffer-name folder))
      'started)
     (t
      (condition-case reason
	  (progn
	    (setq vm-imap-net-session
		  (vm-imap-net-get-mail
		   (vm-folder-imap-maildrop-spec)
		   (lambda (result)
		     (cond ((vm-net-error-p result)
			    (vm-net-warn 0 "%s: %s" (buffer-name folder)
				     (error-message-string result)))
			   ((and (numberp result) (> result 0))
			    (vm-imap-net-show-arrival folder result))
			   (t
			    (vm-net-inform 5 "%s: no new mail"
				       (buffer-name folder)))))
		   (eq interactive t)
		   full))
	    (vm-net-inform 6 "%s: fetching new mail without waiting"
		       (buffer-name folder))
	    'started)
	(vm-imap-net-no-password
	 (vm-imap-net-say-no-password folder reason)
	 nil))))))

(defun vm-imap-net-unfinished-p (&optional folder)
  "Whether FOLDER has work with the server outstanding.
A session running, or something waiting to run when that one ends.

A folder that has been killed has none: its session may still be finishing,
but nothing is waiting for it here, and `with-current-buffer' on a dead
buffer would signal in the middle of a wait."
  (let ((buffer (vm-imap-net-folder-buffer folder)))
    (and (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (or (and vm-imap-net-session
		    (vm-net-session-live-p vm-imap-net-session))
	       (and vm-imap-net-waiting t))))))

(defun vm-imap-net-wait (&optional folder seconds)
  "Wait for FOLDER's work with the server to finish, up to SECONDS.
For a caller that has to have the mail before it goes on: a body a save or a
copy must have in hand (`vm-load-bodies-through-the-driver'), folder-name
completion, or a test.  Those are the only places VM waits on purpose, and
the wait is `accept-process-output', so C-g works.

What is queued behind the running session counts as unfinished: a body asked
for during a fetch runs when the fetch ends, and a caller waiting for the
folder means that too."
  (let ((folder (vm-imap-net-folder-buffer folder))
	(deadline (+ (float-time) (or seconds 30))))
    ;; the session's own callback selects a message and shows it, which
    ;; changes what buffer is current; a caller that waited here did not ask
    ;; to be moved somewhere else
    (save-current-buffer
      (while (and (vm-imap-net-unfinished-p folder) (< (float-time) deadline))
	(accept-process-output nil 0.05)))
    (not (vm-imap-net-unfinished-p folder))))


;;; Copying on the server

(iter-defun vm-imap-net-copy (mailbox uids)
  "UID COPY UIDS into MAILBOX, making it if the server has not got it.
The session's own mailbox is the one they are copied from, so this is the
same server: what the server holds is copied where it stands, and nothing
travels to Emacs and back."
  (let ((error-data nil))
    (condition-case caught
	(iter-yield-from (vm-imap-net-command
			  (format "CREATE %s"
				  (vm-imap-quote-mailbox-name mailbox))
			  "CREATE"))
      ;; CREATE of a mailbox that exists answers NO, which is not an error
      ;; here (issue #690)
      (vm-imap-normal-error (setq error-data caught)))
    (ignore error-data))
  (iter-yield-from (vm-imap-net-command
		    (format "UID COPY %s %s" (mapconcat #'identity uids ",")
			    (vm-imap-quote-mailbox-name mailbox))
		    "UID COPY"))
  (length uids))

(defun vm-imap-net-warn-about-stale-flags (folder messages)
  "Warn if any of MESSAGES still has changes the server would not take.
UID COPY copies what the server holds, so a change that did not go up is not
in the copy, and a save that says nothing about that is issue #38."
  (when (with-current-buffer folder
	  (seq-find #'vm-attribute-modflag-of messages))
    (vm-net-warn 0 (concat "Saved copy has the flags the server holds:"
			 " attribute changes the server would not take"
			 " are not in it.  Save again once"
			 " `vm-imap-synchronize' stores them."))))

(iter-defun vm-imap-net-copy-session (folder user password mailbox target
					     uids messages)
  "Log in, select MAILBOX, send FOLDER's pending flags, and copy UIDS to TARGET.
The flags go first because UID COPY copies what the server holds: a change
made here and not yet stored would not be in the copy (issue #38), and
MESSAGES is what to check that against afterwards."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(iter-yield-from (vm-imap-net-select mailbox))
	(let ((error-data nil))
	  (condition-case caught
	      (iter-yield-from (vm-imap-net-save-flags folder))
	    (vm-imap-normal-error (setq error-data caught)))
	  (ignore error-data))
	(vm-imap-net-warn-about-stale-flags folder messages)
	(iter-yield-from (vm-imap-net-copy target uids)))
    (vm-imap-net-logout)))

(defun vm-imap-net-copy-messages (target messages callback)
  "Copy MESSAGES, which are the current folder's, into the TARGET mailbox.
On the folder's own server, so the messages themselves do not travel.  Tells
CALLBACK how many were copied, or the error that stopped it."
  (let* ((folder (current-buffer))
	 (validity (vm-folder-imap-uid-validity))
	 (uids (mapcar (lambda (message)
			 (unless (equal (vm-imap-uid-validity-of message)
					validity)
			   (error "Message does not have a valid UID"))
			 (vm-imap-uid-of message))
		       messages))
	 (opened (vm-imap-net-open (vm-folder-imap-maildrop-spec) "IMAP copy"
				   'may-ask))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-imap-net-done-with-buffer buffer)
	    (when (buffer-live-p folder)
	      (with-current-buffer folder
		(funcall callback (or (vm-net-session-error finished)
				      (vm-net-session-value finished)))))))
    (vm-imap-net-take-session session
		  (vm-imap-net-copy-session folder (nth 2 opened) (nth 3 opened)
					    (nth 1 opened) target uids messages))
    session))

;;; Saving to an IMAP folder, whichever way round

(declare-function vm-imap-parse-spec-to-list "vm-imap" (spec))
(declare-function vm-imap-folder-p "vm-folder" ())
(declare-function vm-body-to-be-retrieved-of "vm-message" (m))
(declare-function vm-real-message-of "vm-message" (m))

(defun vm-imap-net-same-server-p (target)
  "Whether TARGET is a mailbox on the server the current folder is on."
  (and (vm-imap-folder-p)
       (let ((here (vm-imap-parse-spec-to-list (vm-folder-imap-maildrop-spec)))
	     (there (vm-imap-parse-spec-to-list target)))
	 (and (equal (nth 1 here) (nth 1 there))
	      (equal (nth 5 here) (nth 5 there))))))

(defun vm-imap-net-save-to-folder (target messages callback)
  "Save MESSAGES into the IMAP maildrop TARGET, and tell CALLBACK how many.

Copied on the server where it is the same server, and appended where it is
not -- and where it is not, a message whose body is still on the server is
fetched first, since a message cannot be saved without its body.  Answers
with whether it started; nil means the maildrop is one that cannot be opened
without waiting."
  (let ((folder (current-buffer))
	(external (seq-filter (lambda (m)
				(vm-body-to-be-retrieved-of
				 (vm-real-message-of m)))
			      messages)))
    (condition-case nil
	(cond
	 ((vm-imap-net-same-server-p target)
	  (vm-imap-net-copy-messages (nth 3 (vm-imap-parse-spec-to-list target))
				     messages callback)
	  t)
	 (external
	  ;; fetch what is not here, then save: two servers, so two sessions
	  (vm-imap-net-load-bodies
	   (mapcar #'vm-real-message-of external)
	   (lambda (result)
	     (if (vm-net-error-p result)
		 (funcall callback result)
	       (with-current-buffer folder
		 (vm-imap-net-save-messages
		  target (nth 3 (vm-imap-parse-spec-to-list target))
		  messages callback)))))
	  t)
	 (t
	  (vm-imap-net-save-messages
	   target (nth 3 (vm-imap-parse-spec-to-list target)) messages callback)
	  t))
      (vm-imap-net-no-password nil))))

(declare-function vm-set-filed-flag "vm-message" (m flag))
(declare-function vm-set-deleted-flag "vm-message" (m flag))
(declare-function vm-deleted-flag "vm-message" (m))
(declare-function vm-run-hook-on-message-with-args "vm-misc" (hook message &rest args))
(declare-function vm-imap-folder-for-spec "vm-imap" (spec))
(declare-function vm-safe-imapdrop-string "vm-misc" (string))
(declare-function vm-delete-message "vm-delete" (count &optional mlist))

(defvar vm-delete-after-saving)
(defvar vm-folder-read-only)
(defvar vm-last-save-imap-folder)

(defun vm-imap-net-save-messages-to-folder (target messages count)
  "Save MESSAGES into the IMAP maildrop TARGET without waiting.
Answers with whether it started; nil means nothing was saved, VM having no
password for the target yet.
COUNT is what the command was given, for the deletion afterwards.

The messages are flagged filed when the server has taken them, not when the
command was typed: a save that the server refuses must not leave a folder
saying it was saved."
  (let ((folder (current-buffer)))
    (and (vm-imap-net-save-to-folder
	  target messages
	  (lambda (result)
	    (cond
	     ((vm-net-error-p result)
	      (vm-net-warn 0 "%s: nothing was saved to %s: %s"
		       (buffer-name folder)
		       (or (vm-imap-folder-for-spec target)
			   (vm-safe-imapdrop-string target))
		       (error-message-string result)))
	     (t
	      (dolist (message messages)
		(vm-run-hook-on-message-with-args 'vm-save-message-hook
						  message target)
		(vm-set-filed-flag message t)
		(when (and vm-delete-after-saving (not (vm-deleted-flag message)))
		  (vm-set-deleted-flag message t)))
	      (when (and vm-delete-after-saving (not vm-folder-read-only))
		(vm-delete-message count messages))
	      (setq vm-last-save-imap-folder target)
	      (vm-update-summary-and-mode-line)
	      (vm-net-inform 5 "%d message%s saved to %s" result
			 (if (= result 1) "" "s")
			 (or (vm-imap-folder-for-spec target)
			     (vm-safe-imapdrop-string target)))))))
	 t)))


;;; A maildrop used as a spool source

(declare-function vm-imapdrop-sans-password "vm-misc" (source))
(declare-function vm-get-folder-type "vm-folder"
		  (&optional file start end ignore-visited))

(defvar vm-imap-retrieved-messages)

(defun vm-imap-net-unretrieved (data source retrieved)
  "The UIDs in DATA that RETRIEVED does not say were fetched from SOURCE.
DATA is what `vm-imap-net-message-data' answers with; the answer is a list
of (SEQUENCE-NUMBER . UID), oldest first."
  (let ((maildrop (vm-imapdrop-sans-password source))
	(wanted nil))
    (dolist (tuple (reverse data))
      (let ((number (car tuple))
	    (uid (cadr tuple)))
	(unless (seq-find (lambda (entry)
			    (and (equal (nth 0 entry) uid)
				 (equal (nth 2 entry) maildrop)))
			  retrieved)
	  (push (cons number uid) wanted))))
    (nreverse wanted)))

(defun vm-imap-net-too-large (data wanted)
  "Those of WANTED whose message DATA says is over `vm-imap-max-message-size'.
Answers (KEEP SKIPPED SIZES): the entries to fetch, the entries not to, and
the size of each skipped one in the same order.  Nil for the limit keeps
everything.

A maildrop is fetched into a local folder, which cannot go back to the server
for a body later, so an oversize message is left where it is rather than
fetched as its headers: that is what `vm-imap-max-message-size' says happens
in a local folder.  The blocking path asked the reader about each one instead,
where a reader was there to ask; nothing can be asked from inside a process
filter, and a question per message on a maildrop of any size is not what a
fetch should be."
  (if (not (integerp vm-imap-max-message-size))
      (list wanted nil nil)
    (let ((keep nil) (skipped nil) (sizes nil))
      (dolist (entry wanted)
	(let ((size (string-to-number
		     (or (nth 2 (assq (car entry) data)) "0"))))
	  (if (> size vm-imap-max-message-size)
	      (progn (push entry skipped) (push size sizes))
	    (push entry keep))))
      (list (nreverse keep) (nreverse skipped) (nreverse sizes)))))

(defun vm-imap-net-say-what-was-too-large (source skipped sizes)
  "Say that SKIPPED were left on SOURCE's server for being SIZES bytes.
Named, since a message nobody is told about is one nobody knows to go and
fetch by hand or to raise the limit for."
  (when skipped
    (vm-net-warn 0 (concat "%s: %d message%s left on the server, over"
			   " vm-imap-max-message-size (%d): %s")
		 (vm-safe-imapdrop-string source)
		 (length skipped) (if (cdr skipped) "s" "")
		 vm-imap-max-message-size
		 (mapconcat (lambda (size) (format "%d bytes" size))
			    sizes ", "))))

(defun vm-imap-net-write-message (source start end folder-type)
  "Put the message between START and END of SOURCE into the current buffer.
The same cleaning up the crash box wants: CRLF to LF, and the separators of
FOLDER-TYPE where the server sent none of its own."
  (let ((from (point)))
    (insert-buffer-substring source start end)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((to (point-marker)))
      (vm-imap-cleanup-region from to)
      (vm-munge-message-separators folder-type from to)
      (goto-char from)
      (insert (vm-leading-message-separator folder-type))
      (save-restriction
	(narrow-to-region (point) to)
	(vm-convert-folder-type-headers 'baremessage folder-type))
      (goto-char to)
      (insert-before-markers (vm-trailing-message-separator folder-type))
      (set-marker to nil))
    (goto-char (point-max))))

(declare-function vm-convert-folder-header "vm-folder" (old new))

(defun vm-imap-net-append-crash-box (work crash-box)
  "Append what is in WORK to CRASH-BOX, and empty WORK.

A babyl crash box wants the folder header at the start of it, and only when
it is empty: babyl is the one type VM writes that has one, and a file without
it reads back as no type at all."
  (with-current-buffer work
    (let ((coding-system-for-write 'binary)
	  (selective-display nil))
      (when (and (eq vm-folder-type 'babyl)
		 (let ((attributes (file-attributes crash-box)))
		   (or (null attributes) (equal 0 (nth 7 attributes)))))
	(save-excursion
	  (goto-char (point-min))
	  (vm-convert-folder-header nil vm-folder-type)))
      (write-region (point-min) (point-max) crash-box t 'quiet))
    (erase-buffer)))

(iter-defun vm-imap-net-move (folder source mailbox user password crash-box
				     folder-type retrieved delete)
  "Fetch what RETRIEVED does not have from MAILBOX into CRASH-BOX.
Answers with how many messages were written.  DELETE says to delete them from
the server afterwards, which is `vm-imap-auto-expunge-alist' for this
maildrop.

A bunch at a time: appended to the crash box, and then remembered in FOLDER as
fetched, before the next bunch is asked for.  The order is what keeps a
message from arriving twice.  A session that fails after some of the mail is
on disk leaves that mail in the crash box, which the folder gobbles when it
next looks; if the folder did not know it had those UIDs it would fetch them
again, and both copies would land -- refusing the EXPUNGE of a two-message
maildrop put four messages in the folder.

`vm-imap-net-note-retrieved' only writes the folder's own variable, so this
is not the folder work the session leaves to the callback: nothing is parsed
and nothing is displayed."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let* ((select (iter-yield-from (vm-imap-net-select mailbox)))
	       (count (nth 0 select))
	       (uid-validity (nth 2 select))
	       (body-peek t)
	       (process-buffer (current-buffer))
	       (data (if (zerop count)
			 nil
		       (iter-yield-from (vm-imap-net-message-data 1 count))))
	       (sifted (vm-imap-net-too-large
			data (vm-imap-net-unretrieved data source retrieved)))
	       (wanted (nth 0 sifted))
	       (work (generate-new-buffer " *vm-imap-crash*"))
	       (written 0)
	       (uids nil))
	  (unwind-protect
	      (progn
		(with-current-buffer work
		  (set-buffer-multibyte nil)
		  (setq-local vm-folder-type folder-type))
		(vm-imap-net-say-what-was-too-large
		 source (nth 1 sifted) (nth 2 sifted))
		(dolist (bunch (vm-imap-bunch-messages (mapcar #'car wanted)))
		  (let ((arrived nil))
		    (iter-yield-from
		     (vm-imap-net-fetch
		      (car bunch) (cdr bunch) body-peek nil
		      (lambda (uid start end)
			(with-current-buffer work
			  (vm-imap-net-write-message process-buffer start end
						     folder-type))
			(push uid arrived)
			(setq written (1+ written)))))
		    (when arrived
		      (setq arrived (nreverse arrived))
		      (vm-imap-net-append-crash-box work crash-box)
		      (vm-imap-net-require-folder folder)
		      (with-current-buffer folder
			(vm-imap-net-note-retrieved arrived uid-validity source)
			(vm-mark-folder-modified-p folder))
		      (setq uids (append uids arrived)))))
		(when (and delete uids)
		  (iter-yield-from (vm-imap-net-expunge uids))))
	    (when (buffer-live-p work) (kill-buffer work)))
	  written))
    (vm-imap-net-logout)))

(defun vm-imap-net-note-retrieved (uids uid-validity source)
  "Remember UIDS, valid under UID-VALIDITY, as fetched from SOURCE.
So they are not fetched again.  The current buffer is the folder; this is
`vm-imap-retrieved-messages', which is buffer-local to it.  The
UIDVALIDITY is part of the entry because a UID means nothing without it:
a mailbox recreated on the server hands the same numbers to other messages."
  (let ((maildrop (vm-imapdrop-sans-password source)))
    (dolist (uid uids)
      (setq vm-imap-retrieved-messages
	    (cons (list uid uid-validity maildrop 'uid)
		  vm-imap-retrieved-messages)))))

(declare-function vm-imap-bunch-messages "vm-imap" (seq-nums))

(defvar vm-imap-auto-expunge-alist)
(defvar vm-imap-expunge-after-retrieving)
(defvar vm-imap-auto-expunge-warned)

(defun vm-imap-net-auto-expunge-p (source)
  "Whether messages fetched from SOURCE are to be deleted from the server.
`vm-imap-auto-expunge-alist' first, by the maildrop with its password and
then without, and `vm-imap-expunge-after-retrieving' failing those.  A
maildrop that neither names is left alone, with one warning per maildrop
that mail is being left on the server."
  (let ((entry (or (assoc source vm-imap-auto-expunge-alist)
		   (assoc (vm-imapdrop-sans-password source)
			  vm-imap-auto-expunge-alist))))
    (cond (entry (cdr entry))
	  (vm-imap-expunge-after-retrieving t)
	  ((member source vm-imap-auto-expunge-warned) nil)
	  (t
	   (vm-net-warn 1 "Warning: IMAP folder is not set to auto-expunge")
	   (setq vm-imap-auto-expunge-warned
		 (cons source vm-imap-auto-expunge-warned))
	   nil))))

(defun vm-imap-net-move-mail (source crash-box callback)
  "Fetch new mail from the maildrop SOURCE into CRASH-BOX, and tell CALLBACK.

CALLBACK is called in the folder buffer with the number of messages written,
or with the error that stopped the session.  It is for the caller to gobble
the crash box: this writes it and remembers the UIDs.

Queued when the folder is already busy, and the answer is then `later': a
folder with two maildrops among its spool files is asked for both, one after
the other, and the second must not open a second session writing the same
folder and the same cache file.  It happened -- \"a second session was started
while IMAP movemail was running\" -- and the caller is not to fall back to the
blocking path either, which would be the same two writers.

Signals `vm-imap-net-no-password' where VM has no password yet."
  (if (vm-imap-net-busy-p)
      (vm-imap-net-when-free
       (format "fetching from %s" (vm-safe-imapdrop-string source))
       (lambda () (vm-imap-net-move-mail source crash-box callback)))
    (vm-imap-net-move-mail-1 source crash-box callback)))

(defun vm-imap-net-move-mail-1 (source crash-box callback)
  "Fetch from SOURCE into CRASH-BOX now.  See `vm-imap-net-move-mail'."
  (let* ((folder (current-buffer))
	 (folder-type (vm-folder-type-to-write))
	 (retrieved vm-imap-retrieved-messages)
	 (delete (vm-imap-net-auto-expunge-p source))
	 (opened (vm-imap-net-open source "IMAP movemail" 'may-ask))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-imap-net-done-with-buffer buffer)
	    (when (buffer-live-p folder)
	      (with-current-buffer folder
		(let ((error-data (vm-net-session-error finished)))
		  ;; the UIDs were remembered as each bunch reached the crash
		  ;; box, so there is nothing to record here: what is on disk
		  ;; the folder already knows it has
		  (funcall callback (or error-data
					(vm-net-session-value finished))))))))
    (vm-imap-net-take-session session
		  (vm-imap-net-move folder source (nth 1 opened) (nth 2 opened)
				    (nth 3 opened) crash-box folder-type
				    retrieved delete))
    session))


;;; The check that runs on a timer

(declare-function vm-folder-imap-recent-count "vm-folder" ())
(defvar vm-spooled-mail-waiting)

(iter-defun vm-imap-net-check (folder mailbox user password)
  "Say whether MAILBOX holds mail FOLDER has not got.
Answers the number of messages to be fetched.  The same comparison the fetch
itself makes -- the UIDs the server has against the UIDs the folder has --
since a count of what is there says nothing about what is new."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let* ((select (iter-yield-from (vm-imap-net-select mailbox t)))
	       (count (nth 0 select))
	       (data (if (zerop count)
			 nil
		       (iter-yield-from (vm-imap-net-message-data 1 count)))))
	  (with-current-buffer folder
	    (length (nth 0 (vm-imap-net-plan data count))))))
    (vm-imap-net-logout)))

(defun vm-imap-net-folder-check-mail ()
  "Start asking whether this IMAP folder has new mail, and answer with
whether it did.  The answer to the question itself arrives later, in
`vm-spooled-mail-waiting', which is what the mode line reads.

Nil means nothing was started: VM has no password for the maildrop yet, or a
session is already running -- and a session already running is one that will
say what arrived anyway."
  (let ((folder (current-buffer)))
    (cond
     ((vm-imap-net-busy-p) nil)
     (t
      (condition-case reason
	  (let* ((opened (vm-imap-net-open (vm-folder-imap-maildrop-spec)
					   "IMAP checkmail"))
		 (session (car opened))
		 (buffer (vm-net-session-buffer session)))
	    (setf (vm-net-session-finished session)
		  (lambda (finished)
		    (let ((process (vm-net-session-process finished)))
		      (when (process-live-p process) (delete-process process)))
		    (vm-imap-net-done-with-buffer buffer)
		    (when (buffer-live-p folder)
		      (with-current-buffer folder
			(if (vm-net-session-error finished)
			    (vm-net-inform 6 "%s: could not check for new mail: %s"
				       (buffer-name folder)
				       (error-message-string
					(vm-net-session-error finished)))
			  (let ((waiting (> (or (vm-net-session-value finished) 0)
					    0)))
			    (setq vm-spooled-mail-waiting waiting)
			    (intern (buffer-name folder)
				    vm-buffers-needing-display-update)
			    (vm-update-summary-and-mode-line)
			    (vm-net-inform 6 "%s: %s" (buffer-name folder)
				       (if waiting "new mail" "no new mail"))))))))
	    (vm-imap-net-take-session session
			  (vm-imap-net-check folder (nth 1 opened) (nth 2 opened)
					     (nth 3 opened)))
	    (vm-net-inform 6 "%s: checking the server without waiting"
		       (buffer-name folder))
	    t)
	(vm-imap-net-no-password
	 (vm-imap-net-say-no-password folder reason)
	 nil))))))


;;; The check on a maildrop, which is what the timer asks

(defun vm-imap-net-checkable-p (source)
  "Whether SOURCE can be checked for mail without waiting.
An imap or imap-ssl maildrop whose password VM holds -- in the maildrop, in
its own cache, or in auth-source.  Over ssh a tunnel program has to be
started, and preauth runs a hook; neither belongs in a timer behind the
reader, and both would want the blocking path to ask a question."
  (condition-case nil
      (let* ((parts (vm-parse source "\\([^:]*\\):?" 1 7))
	     (protocol (car parts))
	     (password (nth 6 parts)))
	(and (member protocol '("imap" "imap-ssl"))
	     (equal (nth 4 parts) "login")
	     (or (and (stringp password) (not (equal password "*"))
		      (not (equal password "")))
		 (vm-imap-net-known-password source (nth 5 parts) (nth 1 parts)
					     (nth 2 parts)))
	     t))
    (error nil)))

(iter-defun vm-imap-net-unretrieved-count (mailbox user password source retrieved)
  "How many messages MAILBOX holds that RETRIEVED does not have.
The maildrop is examined rather than selected: a check is not a reason to
mark anything seen."
  (unwind-protect
      (progn
	(iter-yield-from (vm-imap-net-open-session user password))
	(let* ((select (iter-yield-from (vm-imap-net-select mailbox t)))
	       (count (nth 0 select))
	       (data (if (zerop count)
			 nil
		       (iter-yield-from (vm-imap-net-message-data 1 count)))))
	  (length (vm-imap-net-unretrieved data source retrieved))))
    (vm-imap-net-logout)))

(defun vm-imap-net-check-mail (source callback)
  "Ask SOURCE whether it has mail VM has not retrieved, and tell CALLBACK.

CALLBACK is called with t, nil, or the error that stopped the session.  It is
called from the process filter, so the folder buffer it wants is the one it
remembers, not the one that happens to be current.

Nothing waits: this returns as soon as the connection is started."
  (let* ((retrieved vm-imap-retrieved-messages)
	 (opened (vm-imap-net-open source "IMAP check"))
	 (session (car opened))
	 (buffer (vm-net-session-buffer session)))
    (setf (vm-net-session-finished session)
	  (lambda (finished)
	    (let ((process (vm-net-session-process finished)))
	      (when (process-live-p process) (delete-process process)))
	    (vm-imap-net-done-with-buffer buffer)
	    (funcall callback
		     (if (vm-net-session-error finished)
			 (vm-net-session-error finished)
		       (let ((count (vm-net-session-value finished)))
			 (and count (> count 0)))))))
    (vm-net-start session
		  (vm-imap-net-unretrieved-count
		   (nth 1 opened) (nth 2 opened) (nth 3 opened)
		   source retrieved))
    session))

(provide 'vm-imap-net)
;;; vm-imap-net.el ends here
