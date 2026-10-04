;;; vm-imap-mock.el --- An IMAP server for tests, faults and all -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; VM's IMAP client had no test that spoke IMAP.  vm-imap-test.el reaches the
;; spec parsing, the response parser and the file naming and stops where the
;; protocol starts; vm-imap-live-test.el drives the whole thing, but wants a
;; server in test/vm-live-config.el and skips without one -- so on a stock
;; checkout nothing between `vm-imap-make-session' and the wire ran at all.
;;
;; This is the IMAP counterpart of vm-pop-mock.el and follows it: a real
;; server on a local port, spoken to through VM's ordinary code path, rather
;; than a stub of `vm-imap-send-command'.  What is interesting about an IMAP
;; client is in how it reads literals, counts octets, tracks UIDs across an
;; expunge and decides a message arrived intact, and a stub proves none of it.
;;
;; It serves enough of IMAP4rev1 for VM: CAPABILITY, LOGIN, LIST, SELECT and
;; EXAMINE, STATUS, FETCH and UID FETCH (RFC822, RFC822.HEADER, RFC822.SIZE,
;; FLAGS, UID, BODY.PEEK), STORE and UID STORE, EXPUNGE, CLOSE, CREATE,
;; DELETE, RENAME, APPEND, UID COPY, NOOP and LOGOUT.
;;
;; It also misbehaves on request, which is the point.  Several of the things
;; worth knowing about an IMAP client are what it does when the server does
;; not play along:
;;
;;   :refuse          regexp; answer a matching command NO
;;   :bad             regexp; answer a matching command BAD, which is the
;;                    server calling the client's syntax wrong
;;   :drop-on         regexp; close the connection when a command matches
;;   :truncate-fetch  cut a FETCH literal off mid-message and close, which is
;;                    what a real interrupted download looks like
;;   :lie-about-size  report a wrong octet count in RFC822.SIZE
;;   :slow-greeting   wait before greeting, for timeout tests
;;   :reorder-fetch   answer a FETCH of several messages backwards, which a
;;                    server may do: the responses carry UIDs and the client
;;                    is expected to tell them apart by those, not by order
;;   :drop-after-fetch  N; close the connection after N FETCH responses, which
;;                    is a download interrupted between messages
;;   :preauth         greet with PREAUTH: the connection arrives authenticated,
;;                    which is what a session over ssh or through a helper
;;                    program looks like
;;   :extra-fetch-items  add items to every FETCH response that VM did not ask
;;                    for -- MODSEQ and INTERNALDATE, which RFC 3501 7.4.2
;;                    allows a server to send unasked
;;   :unsolicited-flags  send a FETCH of one message's flags before the OK of
;;                    another command, which a server does when somebody else
;;                    changes them
;;   :no-uidplus      leave UIDPLUS out of CAPABILITY
;;   :drops-keywords  take a STORE of a keyword, answer OK, and keep only the
;;                    protocol's own flags, which is what Gmail does
;;   :capabilities    replace the advertised capability list outright
;;   :cram-md5        advertise AUTH=CRAM-MD5 and serve AUTHENTICATE
;;
;; The server records every command it received, so a test can assert on what
;; VM actually sent -- that an expunge really was withheld, say -- rather than
;; on what it was supposed to send.

;;; Code:

(require 'cl-lib)
(require 'vm-crypto)          ; vm-hmac-md5, for CRAM-MD5

(cl-defstruct (vm-imap-mock (:constructor vm-imap-mock--make))
  server port
  user password
  mailboxes				; alist of name -> list of vm-imap-mock-message
  selected				; name of the selected mailbox
  read-only				; whether it was selected with EXAMINE
  (uid-next 1)
  (log nil)
  ;; faults
  refuse bad drop-on truncate-fetch lie-about-size slow-greeting
  no-uidplus capabilities preauth reorder-fetch drop-after-fetch
  extra-fetch-items unsolicited-flags drops-keywords
  authenticated
  ;; CRAM-MD5: the challenge sent, kept so that the response can be checked
  cram-md5 challenge)

(cl-defstruct (vm-imap-mock-message (:constructor vm-imap-mock--message-make))
  uid text (flags nil) (expunged nil))

(defconst vm-imap-mock-default-capabilities
  '("IMAP4REV1" "UIDPLUS" "NAMESPACE" "LITERAL+" "AUTH=LOGIN")
  "What the mock says it can do unless a test says otherwise.")

;;; Building a maildrop

(defun vm-imap-mock--make-message (mock text flags)
  "Return a new message holding TEXT with FLAGS, taking MOCK's next UID."
  (prog1 (vm-imap-mock--message-make
	  :uid (vm-imap-mock-uid-next mock) :text text :flags flags)
    (cl-incf (vm-imap-mock-uid-next mock))))

(defun vm-imap-mock-add-message (mock mailbox text &optional flags)
  "Append TEXT to MAILBOX on MOCK and return its UID.
FLAGS is a list of strings such as (\"\\\\Seen\")."
  (let* ((message (vm-imap-mock--make-message mock text flags))
	 (cell (assoc mailbox (vm-imap-mock-mailboxes mock))))
    (if cell
	(setcdr cell (append (cdr cell) (list message)))
      (push (cons mailbox (list message)) (vm-imap-mock-mailboxes mock)))
    (vm-imap-mock-message-uid message)))

(defun vm-imap-mock-add-mailbox (mock mailbox)
  "Give MOCK an empty MAILBOX, as CREATE would.
For a test that needs somewhere to copy to: a mailbox with a message in it
would be indistinguishable from the copy having gone somewhere it should not."
  (unless (assoc mailbox (vm-imap-mock-mailboxes mock))
    (push (cons mailbox nil) (vm-imap-mock-mailboxes mock)))
  mailbox)

(defun vm-imap-mock-messages (mock mailbox)
  "The messages MAILBOX holds on MOCK, expunged ones left out."
  (cl-remove-if #'vm-imap-mock-message-expunged
		(cdr (assoc mailbox (vm-imap-mock-mailboxes mock)))))

(defun vm-imap-mock-mailbox-names (mock)
  "The mailboxes MOCK has, in the order they were created."
  (reverse (mapcar #'car (vm-imap-mock-mailboxes mock))))

(defun vm-imap-mock--selected-messages (mock)
  "The messages of the mailbox MOCK has selected."
  (vm-imap-mock-messages mock (vm-imap-mock-selected mock)))

(defun vm-imap-mock--nth-message (mock n)
  "Message number N, 1-based, of the selected mailbox on MOCK."
  (nth (1- n) (vm-imap-mock--selected-messages mock)))

(defun vm-imap-mock--message-by-uid (mock uid)
  "The message with UID in the selected mailbox on MOCK."
  (cl-find uid (vm-imap-mock--selected-messages mock)
	   :key #'vm-imap-mock-message-uid))

;;; What the test asks afterwards

(defun vm-imap-mock--log (mock line)
  "Record LINE as received by MOCK."
  (setf (vm-imap-mock-log mock) (append (vm-imap-mock-log mock) (list line))))

(defun vm-imap-mock-commands (mock)
  "Every command line MOCK received, tags and all, in order."
  (vm-imap-mock-log mock))

(defun vm-imap-mock-forget-commands (mock)
  "Forget what MOCK has received, so what follows is asked about on its own.
For a test that has already done something over the wire -- a visit, a fetch
-- and asks what the next operation sent."
  (setf (vm-imap-mock-log mock) nil))

(defun vm-imap-mock-received-p (mock regexp)
  "Whether MOCK received a command matching REGEXP."
  (cl-some (lambda (line) (string-match-p regexp line))
	   (vm-imap-mock-log mock)))

(defun vm-imap-mock-set-flags (mock mailbox uid flags)
  "Give the message with UID in MAILBOX the FLAGS, as another client would.
For the synchronisation tests: what VM is to notice is a change the server
knows about and VM does not."
  (let ((message (cl-find uid (vm-imap-mock-messages mock mailbox)
			  :key #'vm-imap-mock-message-uid)))
    (unless message
      (error "No message with UID %s in %s" uid mailbox))
    (setf (vm-imap-mock-message-flags message) flags)))

(defun vm-imap-mock-flags (mock mailbox uid)
  "The flags of the message with UID in MAILBOX on MOCK."
  (let ((message (cl-find uid (vm-imap-mock-messages mock mailbox)
			  :key #'vm-imap-mock-message-uid)))
    (and message (vm-imap-mock-message-flags message))))

;;; Talking

(defun vm-imap-mock--send (process string)
  "Send STRING to PROCESS, if it is still there to send to."
  (when (process-live-p process)
    (ignore-errors (process-send-string process string))))

(defun vm-imap-mock--literal (text)
  "TEXT as an IMAP literal, its octet count in braces first."
  (format "{%d}\r\n%s" (string-bytes text) text))

(defun vm-imap-mock--quote (string)
  "STRING as an IMAP quoted string."
  (format "\"%s\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" string)))

(defun vm-imap-mock--capabilities (mock)
  "The capability list MOCK advertises."
  (let ((capabilities
	 (or (vm-imap-mock-capabilities mock)
	     (if (vm-imap-mock-no-uidplus mock)
		 (remove "UIDPLUS" vm-imap-mock-default-capabilities)
	       vm-imap-mock-default-capabilities))))
    (if (vm-imap-mock-cram-md5 mock)
	(append capabilities '("AUTH=CRAM-MD5"))
      capabilities)))

;;; Answering

(defun vm-imap-mock--flags-string (message)
  "The FLAGS item for MESSAGE."
  (format "FLAGS (%s)" (mapconcat #'identity
				  (vm-imap-mock-message-flags message) " ")))

(defun vm-imap-mock--headers (text)
  "The header part of TEXT, its closing blank line included."
  (if (string-match "\n\r?\n" text)
      (substring text 0 (match-end 0))
    text))

(defun vm-imap-mock--size (mock text)
  "The octet count MOCK reports for TEXT, which it may be lying about."
  (if (vm-imap-mock-lie-about-size mock)
      (+ (string-bytes text) (vm-imap-mock-lie-about-size mock))
    (string-bytes text)))

(defun vm-imap-mock--body (text)
  "The body part of TEXT, everything after the blank line."
  (if (string-match "\n\r?\n" text)
      (substring text (match-end 0))
    ""))

(defun vm-imap-mock--fetch-item (mock message item)
  "Render ITEM of MESSAGE for a FETCH response from MOCK.
The name in the response is the name asked for with any .PEEK dropped, which
is what a server does and what VM looks for: asked BODY.PEEK[] it must hear
back BODY[], and answering RFC822 to that leaves VM saying it expected
\"(BODY[] string)\".  Returns nil for an item the mock does not serve, which
the caller drops."
  (let* ((text (vm-imap-mock-message-text message))
	 (name (replace-regexp-in-string "\\.PEEK" "" item))
	 (upper (upcase name)))
    (cond ((equal upper "UID")
	   (format "UID %d" (vm-imap-mock-message-uid message)))
	  ((equal upper "FLAGS")
	   (vm-imap-mock--flags-string message))
	  ((equal upper "RFC822.SIZE")
	   (format "RFC822.SIZE %d" (vm-imap-mock--size mock text)))
	  ((equal upper "INTERNALDATE")
	   "INTERNALDATE \"01-Jan-2026 00:00:00 +0000\"")
	  ((member upper '("RFC822.HEADER" "BODY[HEADER]"))
	   (format "%s %s" name
		   (vm-imap-mock--literal (vm-imap-mock--headers text))))
	  ((string-prefix-p "BODY[HEADER.FIELDS" upper)
	   (format "%s %s" name
		   (vm-imap-mock--literal (vm-imap-mock--headers text))))
	  ((member upper '("RFC822.TEXT" "BODY[TEXT]"))
	   (format "%s %s" name (vm-imap-mock--literal (vm-imap-mock--body text))))
	  ((member upper '("RFC822" "BODY[]"))
	   (format "%s %s" name (vm-imap-mock--literal text)))
	  (t nil))))

(defun vm-imap-mock--fetch-items (spec)
  "Split a FETCH item SPEC such as \"(UID RFC822.SIZE FLAGS)\" into items."
  (let ((body (if (string-match "\\`(\\(.*\\))\\'" spec)
		  (match-string 1 spec)
		spec)))
    (split-string body "[ \t]+" t)))

(defun vm-imap-mock--fetch-one (mock process n message spec)
  "Send the FETCH response for MESSAGE, number N, to PROCESS."
  (let* ((items (delq nil (mapcar (lambda (item)
				    (vm-imap-mock--fetch-item mock message item))
				  (vm-imap-mock--fetch-items spec))))
	 (items (if (vm-imap-mock-extra-fetch-items mock)
		    ;; unasked for, and allowed: RFC 3501 7.4.2 says a server
		    ;; may send items the client did not ask about, and a
		    ;; CONDSTORE server sends MODSEQ with everything
		    (append items (list "MODSEQ (23)"
					"INTERNALDATE \"01-Jan-2026 00:00:00 +0000\""))
		  items))
	 (line (format "* %d FETCH (%s)\r\n" n (mapconcat #'identity items " "))))
    (if (vm-imap-mock-truncate-fetch mock)
	(progn (vm-imap-mock--send process (substring line 0 (/ (length line) 2)))
	       (delete-process process))
      (vm-imap-mock--send process line))))

(defun vm-imap-mock--number-range (spec count)
  "The message numbers SPEC covers, given COUNT messages.
SPEC is an IMAP sequence set: 1, 1:4, 1:*, or a comma-separated list of them."
  (let (numbers)
    (dolist (part (split-string spec "," t))
      (cond ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\|\\*\\)\\'" part)
	     (let* ((from (string-to-number (match-string 1 part)))
		    (to-text (match-string 2 part))
		    (to (if (equal to-text "*") count (string-to-number to-text))))
	       (setq numbers (append numbers (number-sequence (min from to)
							     (max from to))))))
	    ((equal part "*") (setq numbers (append numbers (list count))))
	    ((string-match-p "\\`[0-9]+\\'" part)
	     (setq numbers (append numbers (list (string-to-number part)))))))
    (cl-remove-if (lambda (n) (or (< n 1) (> n count))) numbers)))

(defun vm-imap-mock--uid-range (mock spec)
  "The messages of the selected mailbox on MOCK whose UIDs SPEC covers."
  (let ((messages (vm-imap-mock--selected-messages mock))
	found)
    (dolist (part (split-string spec "," t))
      (let (from to)
	(cond ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\|\\*\\)\\'" part)
	       (setq from (string-to-number (match-string 1 part))
		     to (if (equal (match-string 2 part) "*")
			    most-positive-fixnum
			  (string-to-number (match-string 2 part)))))
	      ((string-match-p "\\`[0-9]+\\'" part)
	       (setq from (string-to-number part) to from)))
	(when from
	  (dolist (message messages)
	    (let ((uid (vm-imap-mock-message-uid message)))
	      (when (and (>= uid (min from to)) (<= uid (max from to)))
		(cl-pushnew message found)))))))
    (cl-sort found #'< :key #'vm-imap-mock-message-uid)))

(defun vm-imap-mock--set-flags (message sign flags)
  "Apply FLAGS to MESSAGE, SIGN being \"+\", \"-\" or \"\" for a replacement."
  (setf (vm-imap-mock-message-flags message)
	(cond ((equal sign "+")
	       (cl-union (vm-imap-mock-message-flags message) flags :test #'equal))
	      ((equal sign "-")
	       (cl-set-difference (vm-imap-mock-message-flags message) flags
				  :test #'equal))
	      (t flags))))

;;; The commands

(defun vm-imap-mock--select (mock process tag mailbox examine)
  "Answer SELECT or EXAMINE of MAILBOX for PROCESS."
  (if (null (assoc mailbox (vm-imap-mock-mailboxes mock)))
      (vm-imap-mock--send process
			  (format "%s NO [TRYCREATE] no such mailbox\r\n" tag))
    (setf (vm-imap-mock-selected mock) mailbox
	  (vm-imap-mock-read-only mock) examine)
    (let ((messages (vm-imap-mock-messages mock mailbox)))
      (vm-imap-mock--send
       process
       (concat (format "* %d EXISTS\r\n" (length messages))
	       "* 0 RECENT\r\n"
	       "* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n"
	       (format "* OK [PERMANENTFLAGS (%s)]\r\n"
		       vm-imap-mock-permanent-flags)
	       "* OK [UIDVALIDITY 1000]\r\n"
	       (format "* OK [UIDNEXT %d]\r\n" (vm-imap-mock-uid-next mock))
	       (format "%s OK [%s] %s completed\r\n" tag
		       (if examine "READ-ONLY" "READ-WRITE")
		       (if examine "EXAMINE" "SELECT")))))))

(defvar vm-imap-mock-permanent-flags
  "\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*"
  "What the mock answers for PERMANENTFLAGS at SELECT.
The `\\*\\=' at the end is the server saying it keeps keywords of its own.
Bind this without it for a server that does not, which is what Gmail
answers and what makes a label set in VM disappear (emacs-vm/vm#601).")

(defun vm-imap-mock--fetch (mock process tag spec items by-uid)
  "Answer a FETCH or UID FETCH of SPEC for ITEMS."
  (if (null (vm-imap-mock-selected mock))
      (vm-imap-mock--send process (format "%s BAD no mailbox selected\r\n" tag))
    (let* ((messages (vm-imap-mock--selected-messages mock))
	   (wanted (if by-uid
		       (vm-imap-mock--uid-range mock spec)
		     (mapcar (lambda (n) (nth (1- n) messages))
			     (vm-imap-mock--number-range spec (length messages))))))
      (when (vm-imap-mock-reorder-fetch mock)
	(setq wanted (reverse wanted)))
      (let ((sent 0)
	    (limit (vm-imap-mock-drop-after-fetch mock)))
	(catch 'dropped
	  (dolist (message wanted)
	    (when (process-live-p process)
	      (vm-imap-mock--fetch-one mock process
				       (1+ (cl-position message messages))
				       message items)
	      (setq sent (1+ sent))
	      (when (and limit (>= sent limit))
		(delete-process process)
		(throw 'dropped t))))
	  (when (process-live-p process)
	    ;; somebody else changed a message's flags while this ran, which a
	    ;; server reports whenever it next has the chance: RFC 3501 7.4.1
	    (when (and (vm-imap-mock-unsolicited-flags mock) messages)
	      (vm-imap-mock--send process "* 1 FETCH (FLAGS (\\Seen))\r\n"))
	    (vm-imap-mock--send process
				(format "%s OK FETCH completed\r\n" tag))))))))

(defun vm-imap-mock--store (mock process tag spec sign flags by-uid silent)
  "Answer a STORE or UID STORE, setting FLAGS on the messages SPEC covers."
  (let* ((messages (vm-imap-mock--selected-messages mock))
	 (wanted (if by-uid
		     (vm-imap-mock--uid-range mock spec)
		   (mapcar (lambda (n) (nth (1- n) messages))
			   (vm-imap-mock--number-range spec (length messages))))))
    (dolist (message wanted)
      ;; Gmail accepts a keyword and does not keep it, so the flags that go on
      ;; the message are not always the flags that were asked for.
      (vm-imap-mock--set-flags
       message sign
       (if (vm-imap-mock-drops-keywords mock)
	   (seq-filter (lambda (flag) (string-prefix-p "\\" flag)) flags)
	 flags))
      (unless silent
	(vm-imap-mock--send process
			    (format "* %d FETCH (%s)\r\n"
				    (1+ (cl-position message messages))
				    (vm-imap-mock--flags-string message)))))
    (vm-imap-mock--send process (format "%s OK STORE completed\r\n" tag))))

(defun vm-imap-mock--expunge (mock process tag)
  "Expunge the deleted messages of the selected mailbox, announcing each."
  (let (numbers)
    ;; the numbers shift as each one goes, so they are announced highest first
    (let ((n 0))
      (dolist (message (vm-imap-mock--selected-messages mock))
	(setq n (1+ n))
	(when (member "\\Deleted" (vm-imap-mock-message-flags message))
	  (push (cons n message) numbers))))
    (dolist (cell numbers)			; already highest first
      (setf (vm-imap-mock-message-expunged (cdr cell)) t)
      (vm-imap-mock--send process (format "* %d EXPUNGE\r\n" (car cell))))
    (vm-imap-mock--send process (format "%s OK EXPUNGE completed\r\n" tag))))

(defun vm-imap-mock--pattern-regexp (pattern)
  "The regexp an IMAP LIST PATTERN means.
* matches anything, % anything but the delimiter, and everything else is
literal.  Quoting the pattern and then substituting for the wildcards does
not work: `regexp-quote' turns * into \\*, and replacing the * of that
leaves the backslash behind."
  (concat "\\`"
	  (mapconcat (lambda (character)
		       (cond ((eq character ?*) ".*")
			     ((eq character ?%) "[^/]*")
			     (t (regexp-quote (char-to-string character)))))
		     pattern "")
	  "\\'"))

(defun vm-imap-mock--list (mock process tag pattern)
  "Answer LIST, naming the mailboxes PATTERN covers."
  (let ((regexp (vm-imap-mock--pattern-regexp pattern)))
    (dolist (name (vm-imap-mock-mailbox-names mock))
      (when (or (equal pattern "") (string-match-p regexp name))
	(vm-imap-mock--send process
			    (format "* LIST () \"/\" %s\r\n"
				    (vm-imap-mock--quote name)))))
    (vm-imap-mock--send process (format "%s OK LIST completed\r\n" tag))))

(defun vm-imap-mock--append-finish (mock process)
  "Store the literal PROCESS has finished sending as a new message."
  (let ((mailbox (process-get process 'vm-imap-mock-append-mailbox))
	(flags (process-get process 'vm-imap-mock-append-flags))
	(tag (process-get process 'vm-imap-mock-append-tag))
	(text (process-get process 'vm-imap-mock-append-text)))
    (unless (assoc mailbox (vm-imap-mock-mailboxes mock))
      (push (cons mailbox nil) (vm-imap-mock-mailboxes mock)))
    (let ((uid (vm-imap-mock-add-message mock mailbox text flags)))
      (process-put process 'vm-imap-mock-append-mailbox nil)
      (vm-imap-mock--send
       process (format "%s OK [APPENDUID 1000 %d] APPEND completed\r\n"
		       tag uid)))))

(defun vm-imap-mock--fault (mock process tag line)
  "Act on any fault MOCK is configured with for LINE.
Returns non-nil when the fault answered the command, so the caller stops."
  (cond ((and (vm-imap-mock-drop-on mock)
	      (string-match-p (vm-imap-mock-drop-on mock) line))
	 (delete-process process) t)
	((and (vm-imap-mock-refuse mock)
	      (string-match-p (vm-imap-mock-refuse mock) line))
	 (vm-imap-mock--send process (format "%s NO refused by the mock\r\n" tag))
	 t)
	((and (vm-imap-mock-bad mock)
	      (string-match-p (vm-imap-mock-bad mock) line))
	 (vm-imap-mock--send process (format "%s BAD rejected by the mock\r\n" tag))
	 t)
	(t nil)))

(defun vm-imap-mock--handle-authenticated (mock process tag rest)
  "Answer REST, a command from an authenticated client, tagged TAG."
  (cond
   ((string-match "\\`\\(SELECT\\|EXAMINE\\) +\\(.*\\)\\'" rest)
    (let ((command (match-string 1 rest))
	  (name (match-string 2 rest)))
      (vm-imap-mock--select mock process tag (vm-imap-mock--unquote name)
			    (equal (upcase command) "EXAMINE"))))
   ((string-match "\\`UID +FETCH +\\([0-9:,*]+\\) +\\(.*\\)\\'" rest)
    (vm-imap-mock--fetch mock process tag (match-string 1 rest)
			 (match-string 2 rest) t))
   ((string-match "\\`FETCH +\\([0-9:,*]+\\) +\\(.*\\)\\'" rest)
    (vm-imap-mock--fetch mock process tag (match-string 1 rest)
			 (match-string 2 rest) nil))
   ((string-match "\\`\\(UID +\\)?STORE +\\([0-9:,*]+\\) +\\([-+]?\\)FLAGS\\(\\.SILENT\\)? +(\\(.*\\))\\'" rest)
    (vm-imap-mock--store mock process tag (match-string 2 rest)
			 (match-string 3 rest)
			 (split-string (match-string 5 rest) " " t)
			 (and (match-string 1 rest) t)
			 (and (match-string 4 rest) t)))
   ((string-match-p "\\`EXPUNGE\\'" rest)
    (vm-imap-mock--expunge mock process tag))
   ((string-match-p "\\`CLOSE\\'" rest)
    (vm-imap-mock--expunge mock process tag)
    (setf (vm-imap-mock-selected mock) nil))
   ((string-match "\\`LIST +\\(\"[^\"]*\"\\|[^ ]+\\) +\\(.*\\)\\'" rest)
    (vm-imap-mock--list mock process tag
			(vm-imap-mock--unquote (match-string 2 rest))))
   ((string-match "\\`STATUS +\\(\"[^\"]*\"\\|[^ ]+\\) +(\\(.*\\))\\'" rest)
    (let* ((name (vm-imap-mock--unquote (match-string 1 rest)))
	   (messages (vm-imap-mock-messages mock name)))
      (vm-imap-mock--send
       process (format "* STATUS %s (MESSAGES %d UIDNEXT %d UIDVALIDITY 1000)\r\n%s OK STATUS completed\r\n"
		       (vm-imap-mock--quote name) (length messages)
		       (vm-imap-mock-uid-next mock) tag))))
   ((string-match "\\`CREATE +\\(.*\\)\\'" rest)
    (let ((name (vm-imap-mock--unquote (match-string 1 rest))))
      (if (assoc name (vm-imap-mock-mailboxes mock))
	  (vm-imap-mock--send process (format "%s NO mailbox exists\r\n" tag))
	(push (cons name nil) (vm-imap-mock-mailboxes mock))
	(vm-imap-mock--send process (format "%s OK CREATE completed\r\n" tag)))))
   ((string-match "\\`DELETE +\\(.*\\)\\'" rest)
    (let ((name (vm-imap-mock--unquote (match-string 1 rest))))
      (setf (vm-imap-mock-mailboxes mock)
	    (cl-remove name (vm-imap-mock-mailboxes mock) :key #'car :test #'equal))
      (vm-imap-mock--send process (format "%s OK DELETE completed\r\n" tag))))
   ((string-match "\\`RENAME +\\(\"[^\"]*\"\\|[^ ]+\\) +\\(.*\\)\\'" rest)
    (let* ((raw-from (match-string 1 rest))	; both out before unquoting,
	   (raw-to (match-string 2 rest))	; which matches again
	   (from (vm-imap-mock--unquote raw-from))
	   (to (vm-imap-mock--unquote raw-to)))
      (let ((cell (assoc from (vm-imap-mock-mailboxes mock))))
	(if (null cell)
	    (vm-imap-mock--send process (format "%s NO no such mailbox\r\n" tag))
	  (setcar cell to)
	  (vm-imap-mock--send process (format "%s OK RENAME completed\r\n" tag))))))
   ((string-match "\\`UID +COPY +\\([0-9:,*]+\\) +\\(.*\\)\\'" rest)
    (let* ((raw-set (match-string 1 rest))
	   (name (vm-imap-mock--unquote (match-string 2 rest)))
	   (messages (vm-imap-mock--uid-range mock raw-set)))
      (if (null (assoc name (vm-imap-mock-mailboxes mock)))
	  (vm-imap-mock--send process
			      (format "%s NO [TRYCREATE] no such mailbox\r\n" tag))
	(dolist (message messages)
	  (vm-imap-mock-add-message mock name (vm-imap-mock-message-text message)
				    (vm-imap-mock-message-flags message)))
	(vm-imap-mock--send process (format "%s OK COPY completed\r\n" tag)))))
   ((string-match "\\`APPEND +\\(\"[^\"]*\"\\|[^ ]+\\)\\(.*\\){\\([0-9]+\\)\\+?}\\'" rest)
    (let ((raw-name (match-string 1 rest))
	  (middle (match-string 2 rest))
	  (octets (match-string 3 rest)))
      (process-put process 'vm-imap-mock-append-mailbox
		   (vm-imap-mock--unquote raw-name))
      (process-put process 'vm-imap-mock-append-flags
		   (and (string-match "(\\([^)]*\\))" middle)
			(split-string (match-string 1 middle) " " t)))
      (process-put process 'vm-imap-mock-append-tag tag)
      (process-put process 'vm-imap-mock-append-text "")
      (process-put process 'vm-imap-mock-append-wanted (string-to-number octets)))
    (vm-imap-mock--send process "+ ready for the literal\r\n"))
   ((string-match "\\`SEARCH +UID +\\([0-9]+\\)\\'" rest)
    (let* ((uid (string-to-number (match-string 1 rest)))
	   (message (vm-imap-mock--message-by-uid mock uid))
	   (n (and message (1+ (cl-position message (vm-imap-mock--selected-messages mock))))))
      (vm-imap-mock--send process (format "* SEARCH%s\r\n%s OK SEARCH completed\r\n"
					  (if n (format " %d" n) "") tag))))
   ((string-match-p "\\`NOOP\\'" rest)
    (vm-imap-mock--send process (format "%s OK NOOP completed\r\n" tag)))
   (t
    (vm-imap-mock--send process (format "%s BAD unknown command\r\n" tag)))))

(defun vm-imap-mock--unquote (string)
  "STRING without its surrounding quotes, if it has any."
  (let ((trimmed (string-trim string)))
    (if (string-match "\\`\"\\(.*\\)\"\\'" trimmed)
	(replace-regexp-in-string "\\\\\\(.\\)" "\\1" (match-string 1 trimmed))
      trimmed)))

(defun vm-imap-mock--cram-md5-answer (mock process line)
  "Check LINE, the answer to a CRAM-MD5 challenge, and answer the client.
RFC 2195: base64 of the user name, a space, and the HMAC-MD5 of the
challenge keyed by the password, in lower case hexadecimal."
  (let* ((tag (car (vm-imap-mock-challenge mock)))
	 (challenge (cdr (vm-imap-mock-challenge mock)))
	 (given (ignore-errors (base64-decode-string line)))
	 (want (concat (vm-imap-mock-user mock) " "
		       (vm-hmac-md5 (vm-imap-mock-password mock) challenge))))
    (setf (vm-imap-mock-challenge mock) nil)
    (if (equal given want)
	(progn (setf (vm-imap-mock-authenticated mock) t)
	       (vm-imap-mock--send
		process (format "%s OK AUTHENTICATE completed\r\n" tag)))
      (vm-imap-mock--log mock (format "!! CRAM-MD5 wanted %S, got %S" want given))
      (vm-imap-mock--send
       process (format "%s NO authentication failed\r\n" tag)))))

(defun vm-imap-mock--handle (mock process line)
  "Answer LINE, one command from PROCESS."
  (vm-imap-mock--log mock line)
  (cond
   ;; the answer to a continuation carries no tag, so it is taken before
   ;; anything tries to read one off the front of it
   ((vm-imap-mock-challenge mock)
    (vm-imap-mock--cram-md5-answer mock process line))
   ((string-match "\\`\\([^ ]+\\) +\\(.*\\)\\'" line)
      (let ((tag (match-string 1 line))
	    (rest (match-string 2 line)))
	(unless (vm-imap-mock--fault mock process tag line)
	  (cond
	   ((string-match-p "\\`CAPABILITY\\'" rest)
	    (vm-imap-mock--send
	     process (format "* CAPABILITY %s\r\n%s OK CAPABILITY completed\r\n"
			     (mapconcat #'identity (vm-imap-mock--capabilities mock) " ")
			     tag)))
	   ((string-match-p "\\`AUTHENTICATE +CRAM-MD5\\'" rest)
	    (if (not (vm-imap-mock-cram-md5 mock))
		(vm-imap-mock--send
		 process (format "%s NO AUTHENTICATE not supported\r\n" tag))
	      ;; RFC 2195: any challenge will do, so long as the digest is
	      ;; taken over the one that was sent.
	      (let ((challenge (format "<%d.%d@vm-imap-mock>"
				       (random 100000) (random 100000))))
		(setf (vm-imap-mock-challenge mock) (cons tag challenge))
		(vm-imap-mock--send
		 process (format "+ %s\r\n"
				 (base64-encode-string challenge t))))))
	   ((string-match "\\`LOGIN +\\(.*\\)\\'" rest)
	    (let* ((args (match-string 1 rest))
		   ;; both groups out before anything that matches again:
		   ;; `vm-imap-mock--unquote' clobbers the match data
		   (raw (when (string-match "\\`\\(\"[^\"]*\"\\|[^ ]+\\) +\\(.*\\)\\'" args)
			  (list (match-string 1 args) (match-string 2 args))))
		   (parts (and raw (mapcar #'vm-imap-mock--unquote raw))))
	      (if (and parts
		       (equal (nth 0 parts) (vm-imap-mock-user mock))
		       (equal (nth 1 parts) (vm-imap-mock-password mock)))
		  (progn (setf (vm-imap-mock-authenticated mock) t)
			 (vm-imap-mock--send
			  process (format "%s OK LOGIN completed\r\n" tag)))
		(vm-imap-mock--send
		 process (format "%s NO login failed\r\n" tag)))))
	   ((string-match-p "\\`LOGOUT\\'" rest)
	    (vm-imap-mock--send
	     process (format "* BYE vm-imap-mock signing off\r\n%s OK LOGOUT completed\r\n" tag))
	    (setf (vm-imap-mock-selected mock) nil))
	   ((null (vm-imap-mock-authenticated mock))
	    (vm-imap-mock--send process (format "%s NO not authenticated\r\n" tag)))
	   (t (vm-imap-mock--handle-authenticated mock process tag rest))))))
   (t (vm-imap-mock--send process "* BAD not a command\r\n"))))

;;; The connection

(defun vm-imap-mock--filter (process text)
  "Split TEXT from PROCESS into commands and answer each.
A pending APPEND is taking a literal, and those octets are the message rather
than commands, so they are counted off first."
  (let ((mock (process-get process 'vm-imap-mock))
	(pending (concat (or (process-get process 'vm-imap-mock-pending) "") text))
	line)
    (unwind-protect
	(while (and (process-live-p process)
		    (if (process-get process 'vm-imap-mock-append-mailbox)
			(setq pending
			      (vm-imap-mock--take-literal mock process pending))
		      (when (string-match "\\`\\([^\r\n]*\\)\r?\n" pending)
			(setq line (match-string 1 pending)
			      pending (substring pending (match-end 0)))
			(condition-case error
			    (vm-imap-mock--handle mock process line)
			  (error
			   (vm-imap-mock--log
			    mock (format "!! error answering %s: %s"
					 line (error-message-string error)))
			   (vm-imap-mock--send
			    process
			    (format "%s NO internal mock error\r\n"
				    (car (split-string line " " t))))))
			t))))
      ;; an error must not cost the rest of PENDING: the command still in
      ;; there would never be answered, and the client would wait for a reply
      ;; that is not coming.  Emacs prints an error in a process filter and
      ;; carries on, so the test would see only a timeout.
      (process-put process 'vm-imap-mock-pending pending))))

(defun vm-imap-mock-errors (mock)
  "The errors MOCK hit while answering, as strings, newest last."
  (let (errors)
    (dolist (line (vm-imap-mock-log mock) (nreverse errors))
      (when (string-prefix-p "!! " line)
	(push line errors)))))

(defun vm-imap-mock--take-literal (mock process pending)
  "Take the APPEND literal out of PENDING, returning what is left.
Returns nil when the literal is not all here yet, which stops the caller."
  (let* ((wanted (process-get process 'vm-imap-mock-append-wanted))
	 (have (process-get process 'vm-imap-mock-append-text))
	 (need (- wanted (string-bytes have))))
    (if (< (string-bytes pending) need)
	(progn (process-put process 'vm-imap-mock-append-text (concat have pending))
	       nil)
      (process-put process 'vm-imap-mock-append-text
		   (concat have (substring pending 0 need)))
      (vm-imap-mock--append-finish mock process)
      ;; the client ends the literal with a newline of its own
      (replace-regexp-in-string "\\`\r?\n" "" (substring pending need)))))

(defun vm-imap-mock--connection-buffer-away (client)
  "Detach and kill the buffer Emacs gave CLIENT.
An accepted connection gets a buffer named after its process, and this server
never reads it: what the client sends goes to `vm-imap-mock--filter' and what
is pending sits in a process property.  Left alone the buffer outlives the
test, one per connection.  Detached before it is killed, so killing it does
not ask about the live process."
  (let ((buffer (process-buffer client)))
    (set-process-buffer client nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun vm-imap-mock--on-connect (server client _message)
  "Greet CLIENT, which SERVER has just accepted."
  (let ((mock (process-get server 'vm-imap-mock)))
    (process-put client 'vm-imap-mock mock)
    (process-put client 'vm-imap-mock-pending "")
    (setf (vm-imap-mock-authenticated mock) (and (vm-imap-mock-preauth mock) t))
    (set-process-coding-system client 'binary 'binary)
    (set-process-filter client #'vm-imap-mock--filter)
    (vm-imap-mock--connection-buffer-away client)
    (when (vm-imap-mock-slow-greeting mock)
      (sleep-for (vm-imap-mock-slow-greeting mock)))
    (vm-imap-mock--send client
			(if (vm-imap-mock-preauth mock)
			    "* PREAUTH vm-imap-mock ready\r\n"
			  "* OK vm-imap-mock ready\r\n"))))

(cl-defun vm-imap-mock-start (&key (user "vmtest") (password "secret")
				   (mailbox "INBOX") messages
				   refuse bad drop-on truncate-fetch
				   lie-about-size slow-greeting no-uidplus
				   capabilities preauth reorder-fetch
				   drop-after-fetch extra-fetch-items
				   unsolicited-flags drops-keywords cram-md5)
  "Start a mock IMAP server on a local port and return it.
MESSAGES is what MAILBOX holds: a list of strings, each a whole RFC 5322
message, or of (TEXT . FLAGS).  The keywords after it are the faults
described in the commentary above.  `vm-imap-mock-port' gives the port to
point VM at, and `vm-imap-mock-spec' builds the maildrop."
  (let* ((mock (vm-imap-mock--make
		:user user :password password
		:mailboxes (list (cons mailbox nil))
		:refuse refuse :bad bad :drop-on drop-on
		:truncate-fetch truncate-fetch
		:lie-about-size lie-about-size
		:slow-greeting slow-greeting
		:no-uidplus no-uidplus
		:capabilities capabilities
		:preauth preauth
		:reorder-fetch reorder-fetch
		:drop-after-fetch drop-after-fetch
		:extra-fetch-items extra-fetch-items
		:unsolicited-flags unsolicited-flags
		:drops-keywords drops-keywords
		:cram-md5 cram-md5))
	 (server (make-network-process
		  :name "vm-imap-mock" :server t :service t
		  :host 'local :family 'ipv4 :coding 'binary :noquery t
		  :log #'vm-imap-mock--on-connect)))
    (dolist (message messages)
      (if (consp message)
	  (vm-imap-mock-add-message mock mailbox (car message) (cdr message))
	(vm-imap-mock-add-message mock mailbox message)))
    (process-put server 'vm-imap-mock mock)
    (setf (vm-imap-mock-server mock) server
	  (vm-imap-mock-port mock) (process-contact server :service))
    mock))

(defun vm-imap-mock-stop (mock)
  "Shut MOCK down, along with any connection it is serving."
  (let ((server (vm-imap-mock-server mock)))
    (when (process-live-p server)
      (ignore-errors (delete-process server))))
  (dolist (process (process-list))
    (when (eq (process-get process 'vm-imap-mock) mock)
      (ignore-errors (delete-process process)))))

(defun vm-imap-mock-spec (mock &optional mailbox auth)
  "Return a VM IMAP maildrop specification pointing at MOCK.
MAILBOX defaults to INBOX, AUTH to login."
  (format "imap:127.0.0.1:%d:%s:%s:%s:%s"
	  (vm-imap-mock-port mock)
	  (or mailbox "INBOX")
	  (or auth "login")
	  (vm-imap-mock-user mock)
	  (vm-imap-mock-password mock)))

(defmacro vm-imap-mock-with (spec &rest body)
  "Run BODY with a mock IMAP server bound to MOCK-VAR, then stop it.
SPEC is (MOCK-VAR &rest ARGS), where ARGS go to `vm-imap-mock-start'."
  (declare (indent 1) (debug t))
  `(let ((,(car spec) (vm-imap-mock-start ,@(cdr spec)))
         ;; A session remembers its password and keeps its buffer for reuse.
         ;; Bound, so the mock's credentials and buffer do not outlive the test
         ;; that invented them.
         (vm-imap-passwords vm-imap-passwords)
         (vm-kept-imap-buffers vm-kept-imap-buffers)
         (vm-imap-keep-trace-buffer nil)
         (vm-imap-ok-to-ask nil)
         ;; `vm-warn' remembers its last warning so as not to repeat it, and
         ;; these tests produce warnings on purpose.
         (vm-current-warning vm-current-warning))
     (unwind-protect (progn ,@body)
       (vm-imap-mock-stop ,(car spec)))))

(provide 'vm-imap-mock)

;;; vm-imap-mock.el ends here
