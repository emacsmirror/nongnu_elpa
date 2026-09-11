;;; vm-imap.el --- IMAP folder-side support for VM  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1998, 2001, 2003 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2006 Robert P. Goldman
;; Copyright (C) 2008-2011 Uday S. Reddy
;; Copyright (C) 2024-2025 The VM Developers
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
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Code:

(require 'vm-macro)
(require 'vm-misc)
(require 'vm-motion)
(require 'vm-reply)                     ;vm-mail-mode-remove-header
(require 'sendmail)
(require 'utf7)
(declare-function vm-net-error-p "vm-net" (value))
(declare-function vm-imap-net-forget-held-uids "vm-imap-net" ())
(eval-when-compile (require 'cl-lib))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-session-initialization 
		  "vm.el" ())
(declare-function vm-submit-bug-report 
		  "vm.el" (&optional pre-hooks post-hooks))
(declare-function open-network-stream 
		  "subr.el" (name buffer host service &rest parameters))

(defvar selectable-only) ;; FIXME: Add `vm-' prefix!
(defvar auth-sources)  ;; from auth-source.el, used for dynamic binding

;;; To-Do  (USR)
;; - Need to ensure that new imap sessions get created as and when needed.

;; ------------------------------------------------------------------------
;; The IMAP session protocol
;; ------------------------------------------------------------------------

;; movemail: Folder-specific IMAP sessions are created and destroyed
;; for each get-new-mail.  (Same as in VM 7.19)
;;
;; expunge: expunge-imap-messages creates and destroys sessions.
;; checkmail: check-for-mail also creates and destroys sessions.

;; checkmail: check-for-mail also creates and destroys sessions.

;; IMAP-FCC: Rob F's save-composition creates and destroys its own sessions.

;; folders: imap-folder-completion-list creates and destroys (?) sessions.

;; create, delete folder, rename folder, folders: They are also
;; created and destroyed at a global level for operations like
;; create-mailbox.  (VM 7.19 didn't destroy them in the end, but we
;; do.)

;; general operation: synchronize-folder creates an IMAP session but
;; leaves it active.  Since session is linked to the folder buffer,
;; the folder can use it for other operations like fetch-imap-message
;; and copy-message.  The next time a synchronize-folder is done, this
;; session is killed and a fresh session is created.

;; ------------------------------------------------------------------------
;;; Utilities
;; ------------------------------------------------------------------------


;;
;; vm-folder-access-data
;;
;; See the info manual section on "Folder Internals" for the structure
;; of the data stored here.
;;
;; The following functions are based on cached folder-access-data.
;; They will only function when the IMAP process is "valid" and the
;; server message data is non-nil.

(defun vm-folder-imap-cached (uid table)
  "The value UID has in TABLE, one of the folder's obarrays.
Answers nil when the folder has no table: the server data is dropped at the
end of a session (`vm-imap-dump-uid-seq-num-data'), and `intern' with a nil
obarray reads the global one, so a folder with no data of its own would be
answered out of Emacs's own symbols and out of whatever another folder had
interned there."
  (and table
       (let ((key (intern-soft uid table)))
	 (and key (boundp key) (symbol-value key)))))

(defun vm-folder-imap-uid-msn (uid)
  "Returns the message sequence number of message with UID on the IMAP
server, using cached data."
  (vm-folder-imap-cached uid (vm-folder-imap-uid-obarray)))

(defun vm-folder-imap-uid-message-size (uid)
  "Returns the size of the message with UID on the IMAP server (as a
string), using cached data."
  (car (vm-folder-imap-cached uid (vm-folder-imap-flags-obarray))))

(defun vm-folder-imap-uid-message-flags (uid)
  "Returns the flags of the message with UID on the IMAP server,
using cached data."
  (cdr (vm-folder-imap-cached uid (vm-folder-imap-flags-obarray))))

;; Status indicator vector
;; timer
;; whether the current status has been reported already
;; mailbox specification
;; message number (count) of the message currently being retrieved
;; total number of mesasges that need to be retrieved in this round
;; amount of the current message that has been retrieved
;; size of the current message
;; Data for the message last reported
;; For logging IMAP sessions

(defvar vm-imap-log-sessions nil
  "* Boolean flag to turn on or off logging of IMAP sessions.  Meant
  for debugging IMAP server interactions.")

(defvar vm-imap-tokens nil
  "Internal variable used to store a trail of the lexical and parsing
activity carried out on the IMAP process output.  Used for debugging
purposes.")

  
;; For verification of session protocol
;; Possible values are 
;; 'active - active session present
;; 'valid - message sequence numbers are valid 
;;	validity is preserved by FETCH, STORE and SEARCH operations
;; 'inactive - session is inactive


;; Handling mailbox names and maildrop specs

(defsubst vm-imap-quote-string (string)
  (vm-with-string-as-temp-buffer string 'vm-imap-quote-buffer))

(defun vm-imap-quote-buffer ()
  (goto-char (point-min))
  (insert "\"")
  (while (re-search-forward "[\"\\]" nil t)
    (forward-char -1)
    (insert "\\")
    (forward-char 1))
  (goto-char (point-max))
  (insert "\""))

(defsubst vm-imap-quote-mailbox-name (name)
  (vm-imap-quote-string (utf7-encode name t)))

(defsubst vm-imap-encode-mailbox-name (name)
  (utf7-encode name t))

(defsubst vm-imap-decode-mailbox-name (name)
  (utf7-decode name t))

;;;###autoload
(defun vm-imap-make-filename-for-spec (spec)
  "Returns the cache file in use for the IMAP maildrop specification SPEC.
The name is built from the MD5 of the specification; `vm-cache-file-in-use'
decides between an existing cache and the name a new one gets."
  (let (md5)
    (setq spec (vm-imap-normalize-spec spec))
    (setq md5 (vm-md5-string spec))
    (vm-cache-file-in-use
     (expand-file-name (concat "imap-cache-" md5)
		       (or vm-imap-folder-cache-directory
			   vm-folder-directory
			   (getenv "HOME"))))))

;;;###autoload
(defun vm-imap-normalize-spec (spec)
  (let (comps)
    (setq comps (vm-imap-parse-spec-to-list spec))
    (setcar (vm-last comps) "*")		; scrub password
    (setcar comps "imap")		; standardise protocol name
    (setcar (nthcdr 2 comps) "*")	; scrub portnumber
    (setcar (nthcdr 4 comps) "*")	; scrub authentication method
    (setq spec (mapconcat (function identity) comps ":"))
    spec ))

;;;###autoload
(defun vm-imap-account-name-for-spec (spec)
  "Returns the IMAP account name for maildrop specification SPEC, by
looking up `vm-imap-account-alist' or nil if there is no such account."
  (let ((alist vm-imap-account-alist)
	comps account-comps)
    (setq comps (vm-imap-parse-spec-to-list spec))
    (catch 'return
    (while alist
      (setq account-comps (vm-imap-parse-spec-to-list (car (car alist))))
      (if (and (equal (nth 1 comps) (nth 1 account-comps)) ; host
	       (equal (nth 5 comps) (nth 5 account-comps))) ; login
	  (throw 'return (cadr (car alist)))
	(setq alist (cdr alist))))
    nil)))

;;;###autoload
(defun vm-imap-folder-name-for-spec (spec)
  "Returns the IMAP folder name for maildrop specification SPEC, by
looking up `vm-imap-account-alist' or nil if there is no such account."
  (let ((alist vm-imap-account-alist)
	comps account-comps)
    (setq comps (vm-imap-parse-spec-to-list spec))
    (catch 'return
    (while alist
      (setq account-comps (vm-imap-parse-spec-to-list (car (car alist))))
      (if (and (equal (nth 1 comps) (nth 1 account-comps)) ; host
	       (equal (nth 5 comps) (nth 5 account-comps))) ; login
	  (throw 'return (nth 3 comps))
	(setq alist (cdr alist))))
    nil)))

;;;###autoload
(defun vm-imap-folder-for-spec (spec)
  "Returns the IMAP folder for maildrop specification SPEC in the
format account:mailbox."
  (let (comps account-comps (alist vm-imap-account-alist))
    (setq comps (vm-imap-parse-spec-to-list spec))
    (catch 'return
    (while alist
      (setq account-comps (vm-imap-parse-spec-to-list (car (car alist))))
      (if (and (equal (nth 1 comps) (nth 1 account-comps)) ; host
	       (equal (nth 5 comps) (nth 5 account-comps))) ; login
	  (throw 'return (concat (cadr (car alist)) ":" (nth 3 comps)))
	(setq alist (cdr alist))))
    nil)))

;;;###autoload
;;;###autoload
(defun vm-imap-spec-for-account (account)
  "Returns the IMAP maildrop spec for ACCOUNT, by looking up
`vm-imap-account-alist' or nil if there is no such account.

VM is initialised first, since that is what reads the init file where
`vm-imap-account-alist' is set: a command of one's own that names an account
--- (vm-visit-imap-folder (vm-imap-spec-for-account \"work\")) --- is
otherwise answered with nil the first time it is run in an Emacs session and
with the spec every time after, VM having been initialised by the failed
attempt (emacs-vm/vm#826)."
  (vm-session-initialization)
  (car (rassoc (list account) vm-imap-account-alist)))

;;;###autoload
(defun vm-imap-cache-file-for-folder-name (name)
  "Return the cache file of the IMAP folder NAME, or nil.
NAME is ACCOUNT:MAILBOX, the form `vm-imap-folder-for-spec' produces and the
one a user sees, and ACCOUNT must appear in `vm-imap-account-alist'.  Returns
nil for anything else, so a caller can fall back to treating NAME as a file.

A cache file is named after the MD5 of the maildrop specification, which is
why one cannot be recognised or typed by hand -- see `vm-recover-folder'."
  (when (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" name)
    (let* ((account (match-string 1 name))
	   (mailbox (match-string 2 name))
	   (spec (vm-imap-spec-for-account account)))
      (when spec
	(let ((comps (vm-imap-parse-spec-to-list spec)))
	  ;; The mailbox is the fourth component.  Only the scheme, host,
	  ;; mailbox and login survive `vm-imap-normalize-spec', so the file
	  ;; this yields is the one the folder is really cached in.
	  (setcar (nthcdr 3 comps) mailbox)
	  (vm-imap-make-filename-for-spec
	   (vm-imap-encode-list-to-spec comps)))))))

;;;###autoload
(defun vm-imap-spec-fields (spec)
  "The colon-separated fields of SPEC, empty ones included.

`vm-imap-parse-spec-to-list' drops an empty field, so a maildrop with no
mailbox in it -- imap:host:143::login:user:* -- parses as six fields and its
login is read as the mailbox.  Nothing reported that: the session then
failed for an unrelated reason and the error looked like the right one
(emacs-vm/vm#822)."
  (split-string spec ":"))

(defun vm-imap-parse-spec-to-list (spec)
  "Parses the IMAP maildrop specification SPEC and returns a list of
its components."
  (let ((list (vm-parse spec "\\([^:]+\\):?" 1 6)))
    ;; The mailbox name is left as it stands.  The modified UTF-7 of RFC
    ;; 3501 is applied where the name goes on the wire, by
    ;; `vm-imap-encode-mailbox-name', not in the maildrop spec, which is
    ;; what the user typed and what the folder history shows back.
    list
    ))

;;;###autoload
(defun vm-imap-encode-list-to-spec (list)
  "Convert a LIST of components into a maildrop specification."
    (mapconcat 'identity list ":")
  )

;;;###autoload
(defun vm-imap-spec-for-mailbox (spec mailbox)
  "Return a modified version of the maildrop specification SPEC
for accessing MAILBOX."
  (let ((list (vm-parse spec "\\([^:]+\\):?" 1 6)))
    (mapconcat 'identity 
	       (append (vm-elems 3 list) (cons mailbox (nthcdr 4 list)))
	       ":")))

(defun vm-imap-spec-list-to-host-alist (spec-list)
  (let (host-alist spec) ;;  host
    (while spec-list
      (setq spec (vm-imapdrop-sans-password-and-mailbox (car spec-list)))
      (setq host-alist (cons
			(list
			 (nth 1 (vm-imap-parse-spec-to-list spec))
			 spec)
			host-alist)
	    spec-list (cdr spec-list)))
    host-alist ))

;; Simple macros

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-imap-protocol-error "IMAP protocol error")
      (define-error 'vm-imap-normal-error "IMAP error" 'vm-imap-protocol-error)
      )
  (put 'vm-imap-protocol-error 'error-conditions
       '(vm-imap-protocol-error error))
  (put 'vm-imap-protocol-error 'error-message "IMAP protocol error")
  (put 'vm-imap-normal-error 'error-conditions
       '(vm-imap-protocol-error vm-imap-normal-error error))
  (put 'vm-imap-normal-error 'error-message "IMAP error")
  )

(defsubst vm-imap-protocol-error (&rest args)
  (let ((local (make-local-variable 'vm-imap-keep-trace-buffer)))
    (unless (symbol-value local) (set local 1)))
  (signal 'vm-imap-protocol-error (list (apply 'format args))))

(defsubst vm-imap-normal-error (&rest args)
  (let ((local (make-local-variable 'vm-imap-keep-trace-buffer)))
    (unless (symbol-value local) (set local 1)))
  (signal 'vm-imap-normal-error (list (apply 'format args))))


;; -----------------------------------------------------------------------
;;; IMAP Spool
;; 
;; -- Functions that treat IMAP mailboxes as spools to get mail
;; -- into local buffers and subsequently expunge on the server.
;; -- USR thinks this is obsolete functionality that should not be
;; -- used. Use 'IMAP folders' instead.
;;
;; handler methods:
;; vm-imap-move-mail: (string & string) -> bool
;; vm-imap-check-mail: string -> void
;;
;; interactive commands:
;; vm-expunge-imap-messages: () -> void
;;
;; vm-imap-prune-retrieval-entries: (string & list &
;;				     (retrieval-entry -> bool) -> list
;; vm-imap-clear-invalid-retrieval-entries: (string & list & string) -> list
;; ------------------------------------------------------------------------


;; Our goal is to drag the mail from the IMAP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.

;;;###autoload
(defun vm-expunge-imap-messages ()
  "Deletes all messages from IMAP mailbox that have already been retrieved
into the current folder.  VM sets the \\Deleted flag on all such messages
on all the relevant IMAP servers and then immediately expunges."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-virtual-folder)
  ;; On the driver: this is a session and an expunge per maildrop, and it
  ;; held Emacs for all of them.
  (let ((vm-global-block-new-mail t)
	(vm-imap-ok-to-ask t))
    ;; Said rather than discarded: with no password nothing is expunged, and a
    ;; command that answers a keystroke with silence looks as though it worked.
    (unless (vm-imap-net-expunge-retrieved)
      (vm-inform 5 (concat "Nothing expunged: VM has no password for the"
			   " maildrop yet")))))
(defun vm-imap-net-expunge-retrieved ()
  "Delete on their servers the messages this folder has retrieved, without
waiting for any of it.  Answers whether it started.

The folder forgets each maildrop's messages as that maildrop answers for
them, so an expunge that fails half way through leaves the rest to be offered
again rather than forgetting what was never deleted."
  (let ((folder (current-buffer))
	(groups nil))
    (dolist (entry vm-imap-retrieved-messages)
      (let* ((source (nth 2 entry))
	     (group (assoc source groups)))
	(if group
	    (setcdr group (cons (car entry) (cdr group)))
	  (push (list source (car entry)) groups))))
    (setq groups (nreverse groups))
    (and groups
	 (vm-imap-net-expunge-maildrops
	  groups folder
	  (lambda (source validity deleted gone)
	    ;; and the ones the mailbox no longer has: those are deletions that
	    ;; have already happened, and an entry kept for one is a maildrop
	    ;; asked again at every expunge for ever
	    (let ((settled (append deleted gone)))
	      (setq vm-imap-retrieved-messages
		    (seq-remove (lambda (entry)
				  (and (equal (nth 2 entry) source)
				       (equal (nth 1 entry) validity)
				       (member (car entry) settled)))
				vm-imap-retrieved-messages))
	      (when settled
		(vm-mark-folder-modified-p folder)))
	    (vm-inform 6 "%s: %d message%s expunged"
		       (or (vm-imap-folder-for-spec source)
			   (vm-safe-imapdrop-string source))
		       (length deleted) (if (= (length deleted) 1) "" "s")))
	  (lambda (trouble)
	    (if trouble
		(vm-warn 1 2 "Expunged what could be; trouble with %s"
			 (mapconcat #'identity (reverse trouble) ", "))
	      (vm-inform 5 "Retrieved messages expunged on the server")))))))

;;;###autoload
(defun vm-prune-imap-retrieved-list (source)
  "Prune the X-VM-IMAP-Retrieved header of the current folder by
examining which messages are still present in SOURCE.  SOURCE
should be a maildrop folder on an IMAP server.         USR, 2011-04-06

Nothing waits: the mailbox is asked for the UID of every message it holds and
the header is pruned when the answer comes.  That is the same round trip a
synchronisation makes, and it used to be just as frozen."
  (interactive
   (let ((this-command this-command)
	 (last-command last-command))
     (vm-follow-summary-cursor)
     (save-current-buffer
       (vm-session-initialization)
       (vm-select-folder-buffer)
       (vm-error-if-folder-empty)
       (list (vm-read-imap-folder-name 
	      "Prune messages from IMAP folder: " t nil nil)))))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-display nil nil '(vm-prune-imap-retrieved-list) 
	      '(vm-prune-imap-retrieved-list))
  ;;--------------------------
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (let* ((imapdrop (vm-imapdrop-sans-password source))
	 (folder (current-buffer))
	 ;; The name is taken now: what the callback closes over is the
	 ;; variable and not its value, and a callback that read `source'
	 ;; after this function had finished with it was handed a nil to
	 ;; print.
	 (name (vm-safe-imapdrop-string source)))
    (if (vm-imap-net-mailbox-uids
	 source
	 (lambda (result)
	   (if (vm-net-error-p result)
	       (vm-warn 0 2 "Could not prune %s: %s" name
			(error-message-string result))
	     (when (buffer-live-p folder)
	       (with-current-buffer folder
		 (vm-imap-prune-retrieved-with
		  (car result) (cadr result) imapdrop))))))
	(vm-inform 5 "Asking %s which messages it still has..." name)
      (vm-inform 5 "Cannot prune %s: VM has no password for it yet" name))))

(defun vm-imap-prune-retrieved-with (uid-validity uids imapdrop)
  "Prune this folder's retrieval list to the UIDS the mailbox still has.

UID-VALIDITY is the mailbox's, IMAPDROP names it without its password.  Split
out of `vm-prune-imap-retrieved-list' so that the pruning can be done when
the answer arrives rather than only when it was waited for."
  (let ((there (make-vector 67 0))
	(retrieved-count (length vm-imap-retrieved-messages))
	pruned-count)
    (dolist (uid uids)
      (set (intern uid there) t))
    (setq vm-imap-retrieved-messages
	  (vm-imap-prune-retrieval-entries
	   imapdrop vm-imap-retrieved-messages
	   (lambda (tuple)
	     (and (equal (nth 1 tuple) uid-validity)
		  (intern-soft (car tuple) there)))))
    (setq pruned-count (- retrieved-count (length vm-imap-retrieved-messages)))
    (if (= pruned-count 0)
	(vm-inform 5 "No messages to be pruned")
      (vm-mark-folder-modified-p)
      (vm-update-summary-and-mode-line)
      (vm-inform 5 "%d message%s pruned"
		 pruned-count (if (= pruned-count 1) "" "s")))
    pruned-count))

(defun vm-imap-prune-retrieval-entries (source retrieved pred)
  "Prune RETRIEVED (a copy of `vm-imap-retrieved-messages') by
keeping only those messages from SOURCE that satisfy PRED.
SOURCE must be an IMAP maildrop spec without password info.  
                                                   USR, 2011-04-06"
  (let ((list retrieved)
	(prev nil))
    (setq source (vm-imap-normalize-spec source))
    (while list
      (if (and (equal source (vm-imap-normalize-spec (nth 2 (car list))))
	       (not (apply pred (car list) nil)))
	  (if prev
	      (setcdr prev (cdr list))
	    (setq retrieved (cdr retrieved)))
	(setq prev list))
      (setq list (cdr list)))
    retrieved ))


(defun vm-imap-recorded-uid-validity ()
  "Return the UID-VALIDITY value recorded in the X-IMAP-Retrieved header
of the current folder, or nil if none has been recorded."
  (let ((pos (vm-find vm-imap-retrieved-messages
		      (lambda (record) (nth 1 record)))))
    (and pos
	 (nth 1 (nth pos vm-imap-retrieved-messages)))))



;; --------------------------------------------------------------------
;;; Talking to the server
;;
;; Nothing here does.  The sessions are in vm-imap-net.el, on the driver in
;; vm-net.el, and what is left in this file is the folder-side work they
;; call: maildrop specs, cache file names, passwords, the flag arithmetic
;; and the response parsing.
;;
;; There was an index of the blocking session functions here.  It went with
;; them (emacs-vm/vm#822).
;; --------------------------------------------------------------------


;; The IMAP sessions work as follows:

;; Generally, sessions are created for get-new-mail, save-folder and
;; vm-imap-synchronize operations.  All these operations read the
;; uid-and-flags-data and cache it internally.  At this stage, the
;; IMAP session is said to be "valid", i.e., message numbers stored in
;; the cache are valid.  As long as FETCH and STORE operations are
;; performed, the session remains valid.

;; When other IMAP operations are performed, the server can send
;; EXPUNGE responses and invalidate the cached message sequence
;; numbers.  In this state, the IMAP session is "active", but not
;; "valid".  Only UID-based commands can be issued in this state.


(defun vm-imap-get-password (folder source user host port ask-password purpose)
  "Get the password for the IMAP FOLDER at the server SOURCE.  The
additional arguments USER, HOST and PORT are also passed in for
convenience.  The password is obtained from VM's internal password
cache, the auth-source package or by interactively querying the user.  
The argument ASK-PASSWORD says whether the interactive querying should
be done.  The argument PURPOSE is a string displayed to the user in
case of errors."
  (let ((pass (car (cdr (assoc source vm-imap-passwords)))))
    (when (null pass)
      (setq pass (vm-auth-source-password
		  (list (vm-imap-account-name-for-spec source) host)
		  port user)))
    (while (and (null pass) ask-password)
      (setq pass
	    (read-passwd (format "IMAP password for %s: " folder)))
      (when (equal pass "")
	(vm-warn 0 2 "Password cannot be empty")
	(setq pass nil)))
    (when (null pass)
      (error "Need password for %s for %s" folder purpose))
    pass))

;; This function is not recommended, but is available to use when
;; caching uid-and-flags data might be too expensive.

	

(defun vm-imap-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t)))
  (set-marker end nil))

(defun vm-imap-skip-fetch-item (contents)
  "CONTENTS past the FETCH data item it starts with.

A server may answer with items the client did not ask about (RFC 3501 7.4.2),
and one with CONDSTORE on sends MODSEQ with everything.  Refusing to read past
them cost the whole session and the mail with it: against a server that adds
INTERNALDATE, no mailbox arrived at all.

An item is a name, then a section in brackets if it has one, then one value:
an atom, a string, or a parenthesised list.  That is enough to step over
anything the grammar allows without knowing what it means."
  (let ((rest (cdr contents)))
    (while (and rest (eq (car (car rest)) 'vector))
      (setq rest (cdr rest)))
    (if (and rest (memq (car (car rest)) '(atom string list)))
	(cdr rest)
      rest)))

(defun vm-imap-message-item-p (token)
  "Whether TOKEN names the item that carries a message's text.
The names a fetch of message text asks under, as the server answers them:
BODY.PEEK comes back as BODY, and the section follows it in brackets."
  (and (eq (car token) 'atom)
       (member (upcase (buffer-substring (nth 1 token) (nth 2 token)))
	       '("RFC822" "RFC822.HEADER" "RFC822.TEXT" "BODY"))))

(defun vm-imap-fetch-response-parts (contents)
  "The UID and the message of a FETCH response's CONTENTS, as (UID . TOKEN).
Either may be nil: a response the server sent to report flags carries neither.

The items are walked by name rather than matched in a fixed order, because a
server may answer them in any order and may add items the client did not ask
about (RFC 3501 7.4.2).  The message is taken by name too: an INTERNALDATE the
server added is a string as well, and taking the first string for the message
left every subject empty."
  (let ((uid nil) (text nil))
    (while contents
      (cond ((vm-imap-response-matches contents 'UID 'atom)
	     (let ((token (nth 1 contents)))
	       (setq uid (buffer-substring (nth 1 token) (nth 2 token))))
	     (setq contents (nthcdr 2 contents)))
	    ((vm-imap-message-item-p (car contents))
	     (let ((rest (cdr contents)))
	       (while (and rest (eq (car (car rest)) 'vector))
		 (setq rest (cdr rest)))
	       (unless (eq (car (car rest)) 'string)
		 (vm-imap-protocol-error
		  "expected a message in the FETCH response"))
	       (setq text (car rest)
		     contents (cdr rest))))
	    (t (setq contents (vm-imap-skip-fetch-item contents)))))
    (cons uid text)))

(defun vm-imap-response-matches (response &rest expr)
  "Checks if a REPSONSE from the IMAP server matches the pattern
EXPR.  The syntax of patterns is:

  EXPR ::= QUOTED-SYMBOL | atom | string | (vector EXPR*) | (list EXPR*)

Numbers are included among atoms."
  (let ((case-fold-search t) e r)
    (catch 'done
      (if (null response)
	  (throw 'done nil))
      (while (and expr response)
	(setq e (car expr)
	      r (car response))
	(cond ((stringp e)
	       (if (or (not (eq (car r) 'string))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward e (nth 2 r) t) (nth 2 r)))))
		   (throw 'done nil)))
	      ((numberp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (int-to-string e)
						  (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil)))
	      ((consp e)
	       (if (not (eq (car e) (car r)))
		   (throw 'done nil))
	       ;; the contents have to match as well, and this used to throw
	       ;; the result of that away -- so (vector READ-WRITE) matched
	       ;; [READ-ONLY] and every EXAMINE was reported writable.  A
	       ;; pattern of (vector) says nothing about the contents and
	       ;; still matches any vector, empty ones included: that is what
	       ;; BODY[] is read with, and its token has nothing inside, on
	       ;; which the recursive call would report no match.
	       (if (and (cdr e)
			(not (apply 'vm-imap-response-matches (cdr r) (cdr e))))
		   (throw 'done nil)))
	      ((eq e 'atom)
	       (if (not (eq (car r) 'atom))
		   (throw 'done nil)))
	      ((eq e 'vector)
	       (if (not (eq (car r) 'vector))
		   (throw 'done nil)))
	      ((eq e 'list)
	       (if (not (eq (car r) 'list))
		   (throw 'done nil)))
	      ((eq e 'string)
	       (if (not (eq (car r) 'string))
		   (throw 'done nil)))
	      ;; this must to come after all the comparisons for
	      ;; specific symbols.
	      ((symbolp e)
	       ;; `VM' in a pattern is the tag of the command being waited
	       ;; for, whatever this session numbered it.  It was the literal
	       ;; tag of every command until commands were given tags of their
	       ;; own (emacs-vm/vm#473), and the thirty-odd patterns that say
	       ;; it read the same either way.
	       (let ((name (if (eq e 'VM)
			       (or vm-imap-current-tag "VM")
			     (symbol-name e))))
		 (if (or (not (eq (car r) 'atom))
			 (save-excursion
			   (goto-char (nth 1 r))
			   (not (eq (search-forward name (nth 2 r) t)
				    (nth 2 r)))))
		     (throw 'done nil)))))
	(setq response (cdr response)
	      expr (cdr expr)))
      t )))

(defun vm-imap-scan-list-for-flag (list flag)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward flag (nth 2 e) t) (nth 2 e))
	      (throw 'done t)))
	(setq list (cdr list)))
      nil )))

;; like Lisp get but for IMAP property lists like those returned by FETCH.
(defun vm-imap-plist-get (list name)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward name (nth 2 e) t) (nth 2 e))
	      (throw 'done (car (cdr list)))))
	(setq list (cdr (cdr list))))
      nil )))

(defun vm-imap-retrieve-uid-and-flags-data ()
  "Check that the folder has the UID and flags of every message in its mailbox.

The tables are `vm-folder-access-data's imap-uid-list, imap-uid-obarray and
imap-flags-obarray, and `vm-imap-net-install-message-data' fills them from the
FETCH the driver has already made: a synchronisation reads the mailbox once,
and this is the step that used to read it a second time.

So this no longer talks to a server.  It signals where the tables are empty,
which means a caller reached `vm-imap-get-synchronization-data' without the
driver having filled them, and the answer would silently be that the mailbox
holds nothing."
  (unless (vm-folder-imap-uid-list)
    (error (concat "This folder has not read its mailbox yet;"
		   " use vm-imap-synchronize"))))

;; This function is now obsolete.  It is faster to get flags of
;; several messages at once, using vm-imap-get-message-data-list

(defun vm-imap-mailbox-holds-a-keyword-p (flags-obarray)
  "Whether any message in FLAGS-OBARRAY carries an IMAP keyword.
FLAGS-OBARRAY is the folder's flags obarray, whose value for each UID is the
message's size followed by its flags."
  (catch 'found
    (mapatoms (lambda (symbol)
		(when (seq-some #'vm-imap-keyword-p (cdr (symbol-value symbol)))
		  (throw 'found t)))
	      flags-obarray)
    nil))

(defun vm-imap-mailbox-carries-keywords-p ()
  "Whether this mailbox holds IMAP keywords at all, as far as VM has looked.

A VM label is an IMAP keyword on the wire, and the read-back takes the
server's flag list for the whole truth: a keyword it does not report is a
label the reader has removed somewhere else.  Gmail keeps no keyword at all
(emacs-vm/vm#601), so the read-back that follows a save erased every label the
save had just failed to store, leaving the reader's labels nowhere.

The answer is whether any message in the mailbox carries a keyword.  Nothing
in the protocol answers it: a server may leave a keyword out of PERMANENTFLAGS
and store it anyway, or advertise `\\*' and keep nothing, which is what Gmail
does.  A mailbox where no message has one is a mailbox whose flag list says
nothing about labels, and the labels are left as the folder has them.

Being wrong here costs a label that lingers one sync too long, on a mailbox
where no message carries a keyword and another client removed the last one.
Being wrong the other way costs the reader the labels they set.

The answer is worked out once per look at the mailbox and kept in
`vm-imap-keywords-carried'; the current buffer is the folder."
  (let ((flags-obarray (vm-folder-imap-flags-obarray)))
    (cond
     ;; nothing has been fetched, so there is nothing to go on: answer as VM
     ;; did before this existed
     ((null flags-obarray) t)
     ((eq (car-safe vm-imap-keywords-carried) flags-obarray)
      (cdr vm-imap-keywords-carried))
     (t
      (let ((carried (vm-imap-mailbox-holds-a-keyword-p flags-obarray)))
	(setq vm-imap-keywords-carried (cons flags-obarray carried))
	carried)))))

(defun vm-imap-update-message-flags (m flags &optional norecord)
  "Update the flags of the message M in the folder to imap flags FLAGS.
Optional argument NORECORD says whether this fact should not be
recorded in the undo stack."
  (let (flag saw-Seen saw-Deleted saw-Flagged seen-labels labels)
    (while flags
      (setq flag (car flags))
      (cond ((string= flag "\\answered")
	     (when (null (vm-replied-flag m))
	       (vm-set-replied-flag m t norecord)
	       (vm-set-stuff-flag-of m t)))

	    ((string= flag "\\deleted")
	     (when (null (vm-deleted-flag m))
	       (vm-set-deleted-flag m t norecord)
	       (vm-set-stuff-flag-of m t))
	     (setq saw-Deleted t))

	    ((string= flag "\\flagged")
	     (when (null (vm-flagged-flag m))
	       (vm-set-flagged-flag m t norecord)
	       (vm-set-stuff-flag-of m t))
	     (setq saw-Flagged t))

	    ((string= flag "\\seen")
	     (when (vm-unread-flag m)
	       (vm-set-unread-flag m nil norecord)
	       (vm-set-stuff-flag-of m t))
	     (when (vm-new-flag m)
	       (vm-set-new-flag m nil norecord)
	       (vm-set-stuff-flag-of m t))
	     (setq saw-Seen t))

	    ((string= flag "\\recent")
	     (when (null (vm-new-flag m))
	       (vm-set-new-flag m t norecord)
	       (vm-set-stuff-flag-of m t)))

	    ((string= flag "forwarded")
	     (when (null (vm-forwarded-flag m))
	       (vm-set-forwarded-flag m t norecord)
	       (vm-set-stuff-flag-of m t)))

	    ((string= flag "redistributed")
	     (when (null (vm-redistributed-flag m))
	       (vm-set-redistributed-flag m t norecord)
	       (vm-set-stuff-flag-of m t)))

	    ((string= flag "filed")
	     (when (null (vm-filed-flag m))
	       (vm-set-filed-flag m t norecord)
	       (vm-set-stuff-flag-of m t)))

	    ((string= flag "written")
	     (when (null (vm-written-flag m))
	       (vm-set-written-flag m t norecord)
	       (vm-set-stuff-flag-of m t)))

	    (t			  ; all other flags including \flagged
	     (setq seen-labels (cons flag seen-labels)))
	    )
      (setq flags (cdr flags)))

    (if (not saw-Seen)			; unread if the server says so
	(if (null (vm-unread-flag m))
	    (vm-set-unread-flag m t norecord)))
    (if (not saw-Deleted)		; undelete if the server says so
	(if (vm-deleted-flag m)
	    (vm-set-deleted-flag m nil norecord)))
    (if (not saw-Flagged)		; unflag if the server says so
	(if (vm-flagged-flag m)
	    (vm-set-flagged-flag m nil norecord)))
    (setq labels (sort (vm-decoded-labels-of m) 'string-lessp))
    (setq seen-labels (sort seen-labels 'string-lessp))
    (cond
     ((equal labels seen-labels) t)
     ;; A mailbox that carries no keyword anywhere has said nothing about this
     ;; message's labels, so they stay as the folder has them.  Gmail carries
     ;; none: taking its flag list for the whole truth is what erased a label
     ;; a moment after it was set (emacs-vm/vm#601).
     ((and (null seen-labels) (not (vm-imap-mailbox-carries-keywords-p))) t)
     (t
      (vm-set-decoded-labels-of m seen-labels)
      (vm-set-decoded-label-string-of m nil)
      (vm-mark-for-summary-update m)
      (vm-set-stuff-flag-of m t)))
    ))

(defun vm-imap-flag-list-string (flags)
  "Return FLAGS as an IMAP parenthesised flag list."
  (concat "(" (mapconcat #'identity flags " ") ")"))

(defun vm-imap-keyword-p (flag)
  "Whether FLAG is a keyword rather than one of the protocol's own flags.
A keyword is what a VM label is on the wire, and what a server may decline to
keep; a flag beginning with a backslash is the protocol's and every server
has to know it."
  (not (string-prefix-p "\\" flag)))

(defun vm-imap-fetch-response-flags (response)
  "The flag names in an untagged FETCH RESPONSE, downcased.
Nil when the response carries no FLAGS item.  Reads the process buffer, so it
is called there."
  (let ((tokens (cdr (vm-imap-plist-get (car (nthcdr 3 response)) "FLAGS")))
	(flags nil))
    (dolist (token tokens (nreverse flags))
      (when (eq (car token) 'atom)
	(push (downcase (buffer-substring (nth 1 token) (nth 2 token)))
	      flags)))))

(defun vm-imap-message-flag-changes (m)
  "What M's flags on the server would have to be to match VM's.
Answers (MESSAGE-NUM CACHED-FLAGS FLAGS+ FLAGS-): the sequence number the
server knows M by, the flags VM last saw it with, and the flags to add and
to remove.  MESSAGE-NUM nil means the server does not have the message and
there is nothing to do.

Folder-side and no I/O, so both the blocking and the non-blocking paths
compute the change the same way and only the sending of it differs."
  (let* ((uid (vm-imap-uid-of m))
	 (uid-key1 (intern uid (vm-folder-imap-uid-obarray)))
	 (uid-key2 (intern-soft uid (vm-folder-imap-flags-obarray)))
	 (message-num (and (boundp uid-key1) (symbol-value uid-key1)))
	 (cached-flags (and (boundp uid-key2) (symbol-value uid-key2)))
					; leave uid as the dummy header
	 (labels (vm-decoded-labels-of m))
	 copied-flags flags+ flags-)
    (when message-num
      ;; Reversible flags are treated the same as labels
      (if (not (vm-unread-flag m))
	  (setq labels (cons "\\seen" labels)))
      (if (vm-deleted-flag m)
	  (setq labels (cons "\\deleted" labels)))
      (if (vm-flagged-flag m)
	  (setq labels (cons "\\flagged" labels)))
      ;; Irreversible flags
      (if (and (vm-replied-flag m) 
	       (not (member "\\answered" cached-flags)))
	  (setq flags+ (cons "\\Answered" flags+)))
      (if (and (vm-filed-flag m) (not (member "filed" cached-flags)))
	  (setq flags+ (cons "filed" flags+)))
      (if (and (vm-written-flag m) 
	       (not (member "written" cached-flags)))
	  (setq flags+ (cons "written" flags+)))
      (if (and (vm-forwarded-flag m)
	       (not (member "forwarded" cached-flags)))
	  (setq flags+ (cons "forwarded" flags+)))
      (if (and (vm-redistributed-flag m)
	       (not (member "redistributed" cached-flags)))
	  (setq flags+ (cons "redistributed" flags+)))
      (mapc (lambda (flag) (delete flag cached-flags))
	    '("\\answered" "filed" "written" "forwarded" "redistributed"))
      ;; make copies for side effects
      (setq copied-flags (copy-sequence cached-flags))
      (setq labels (cons nil (copy-sequence labels)))
      ;; Ignore labels that are both in vm and the server
      (vm-delete-common-elements labels copied-flags 'string<)
      ;; Ignore reversible flags that we have locally reversed -- Why?
      ;; Flags to be added to the server
      (setq flags+ (append (cdr labels) flags+))
      ;; Flags to be deleted from the server
      (setq flags- (append (cdr copied-flags) flags-))
      ;; Not \Recent, ever: RFC 3501 says a client cannot set or clear it, so
      ;; asking is a protocol error every time.  It arrives in the cached flags
      ;; from FETCH, and VM was sending "-FLAGS.SILENT (\recent)" for every
      ;; message it synced -- ignored by servers that are polite about it and
      ;; another BAD from the ones that are not (issue #389).
      (setq flags+ (delete "\\recent" (delete "\\Recent" flags+)))
      (setq flags- (delete "\\recent" (delete "\\Recent" flags-))))
    (list message-num cached-flags flags+ flags-)))

(defvar vm-imap-subst-char-in-string-buffer
  (get-buffer-create " *subst-char-in-string*"))

(defun vm-imap-subst-CRLF-for-LF (string)
  (with-current-buffer vm-imap-subst-char-in-string-buffer
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n" nil t))
    (buffer-substring-no-properties (point-min) (point-max))))

;; Incomplete -- Yet to be finished.  USR
;; creation of new mailboxes has to be straightened out


;; ------------------------------------------------------------------------
;; 
;;; interactive commands:
;;
;; vm-create-imap-folder: string -> void
;; vm-delete-imap-folder: string -> void
;; vm-rename-imap-folder: string & string -> void
;; 
;; top-level operations
;; vm-fetch-imap-message: (vm-message) -> void
;; vm-imap-synchronize-folder:
;;	(&optional :interactive interactive & 
;;                 :do-remote-expunges bool & 
;;                 :do-local-expunges bool & 
;;                 :do-retrieves bool &
;;                 :save-attributes nil|t|'all & 
;;                 :retrieve-attributes bool) -> void
;; vm-imap-save-attributes: (:all-flags bool) -> void
;; vm-imap-folder-check-mail: (&optional interactive) -> ?
;;
;; vm-imap-get-synchronization-data: (&optional bool) -> 
;;		(retrieve-list: (uid . int) list &
;;		 local-expunge-list: vm-message list & 
;;		 stale-list: vm-message list)
;;
;; ------------------------------------------------------------------------



(defun vm-imap-get-synchronization-data (&optional do-retrieves)
  "Compares the UID's of messages in the local cache and the IMAP
server.  Returns a list containing:
RETRIEVE-LIST: A list of pairs consisting of UID's and message
  sequence numbers of the messages that are not present in the
  local cache and not retrieved previously, and, hence, need to be
  retrieved now.
LOCAL-EXPUNGE-LIST: A list of message descriptors for messages in the
  local cache which are not present on the server and, hence, need
  to expunged locally.
STALE-LIST: A list of message descriptors for messages in the
  local cache whose UIDVALIDITY values are stale.
If the argument DO-RETRIEVES is `full', then all the messages that
are not presently in cache are retrieved.  Otherwise, the
messages previously retrieved are ignored.

A message the server has, that the cache does not, and that was fetched once
is neither retrieved nor deleted unless DO-RETRIEVES says `full'.  There used
to be a fourth component naming those for deletion on the server, and a full
synchronise deleted them: what put a message on that list was as often a
damaged cache as a reader expunging it, and a truncated cache put the whole
mailbox on it (emacs-vm/vm#752).  Server deletions come from
`vm-imap-messages-to-expunge', which `vm-expunge-folder' fills as the reader
expunges and which the folder carries in its `X-VM-IMAP-To-Expunge' header."

  ;; Comments by USR
  ;; - Originally, messages with stale UIDVALIDITY values were
  ;; ignored.  So, they would never get expunged from the cache.  The
  ;; STALE-LIST component was added to fix this.
  
  ;;-----------------------------
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'synchronization-data
				       vm-buffer-type-trail)))
  (vm-buffer-type:assert 'folder)
  ;;-----------------------------
  (let ((here (make-vector 67 0))	; OBARRAY(uid, vm-message)
	there ;; flags
	(uid-validity (vm-folder-imap-uid-validity))
	(do-full-retrieve (eq do-retrieves 'full))
	retrieve-list local-expunge-list stale-list uid
	mp retrieved-entry)
    (vm-imap-retrieve-uid-and-flags-data)
    (setq there (vm-folder-imap-uid-obarray))
    ;; Figure out stale uidvalidity values and messages to be expunged
    ;; in the cache.
    (setq mp vm-message-list)
    (while mp
      (cond ((not (equal (vm-imap-uid-validity-of (car mp)) uid-validity))
	     (setq stale-list (cons (car mp) stale-list)))
	    ((member "stale" (vm-decoded-labels-of (car mp)))
	     nil)
	    (t
	     (setq uid (vm-imap-uid-of (car mp)))
	     (set (intern uid here) (car mp))
	     (if (not (boundp (intern uid there)))
		 (setq local-expunge-list (cons (car mp) local-expunge-list)))))
      (setq mp (cdr mp)))
    ;; Figure out messages that need to be retrieved
    (mapatoms 
     (lambda (sym)
       (let ((uid (symbol-name sym)))
	 (unless  (boundp (intern uid here))
	   ;; message not in cache.  if it has been retrieved
	   ;; previously, it needs to be expunged on the server.
	   ;; otherwise, it needs to be retrieved.
	   (setq retrieved-entry
		 (vm-find vm-imap-retrieved-messages
			  (lambda (entry)
			    (and (equal (car entry) uid)
				 (equal (cadr entry) uid-validity)))))
	   ;; A message fetched once and no longer in the cache is left alone:
	   ;; fetched again when asked for a full retrieve, and otherwise
	   ;; neither fetched nor deleted.  It used to be queued for deletion
	   ;; on the server, which is what emacs-vm/vm#752 was.
	   (when (or do-full-retrieve (null retrieved-entry))
	     (setq retrieve-list
		   (cons (cons uid (symbol-value sym)) retrieve-list))))))
     there)
    (setq retrieve-list 
	  (sort retrieve-list 
		(lambda (**pair1 **pair2)
		  (< (cdr **pair1) (cdr **pair2)))))	  
    (list retrieve-list local-expunge-list stale-list)))

(defun vm-imap-bunch-retrieve-list (retrieve-list)
  "RETRIEVE-LIST consists of pairs (message-sequence-number boolean)
  where the boolean flag says whether the message is headers-only.
  Bunch the message sequence numbers to produce a list of pairs
  ((begin-num . end-num) boolean).  Each message in a bunch has the
  same headers-only flag."
  (let ((ranges nil)
	pair headers-only
	beg last last-headers-only next) ;; diff
    (when retrieve-list
      (setq pair (car retrieve-list)
	    beg (car pair)
	    headers-only (cadr pair))
      (setq last beg
	    last-headers-only headers-only)
      (setq retrieve-list (cdr retrieve-list))
      (while retrieve-list
	(setq pair (car retrieve-list)
	      next (car pair)
	      headers-only (cadr pair))
	(if (and (= (- next last) 1)
		 (eq last-headers-only headers-only)
		 (< (- next beg) vm-imap-message-bunch-size))
	    (setq last next)
	  (setq ranges (cons (list (cons beg last) last-headers-only) ranges))
	  (setq beg next)
	  (setq last next)
	  (setq last-headers-only headers-only))
	(setq retrieve-list (cdr retrieve-list)))
      (setq ranges (cons (list (cons beg last) last-headers-only) ranges)))
    (nreverse ranges)))

(defun vm-imap-bunch-messages (seq-nums)
  "Given a sorted list of message sequence numbers, creates a
  list of bunched message sequences, each of the form 
  (begin-num . end-num)."
  (let ((seqs nil)
	beg last next) ;; diff
    (when seq-nums
      (setq beg (car seq-nums))
      (setq last beg)
      (setq seq-nums (cdr seq-nums))
      (while seq-nums
	(setq next (car seq-nums))
	(if (and (= (- next last) 1)
		 (< (- next beg) vm-imap-message-bunch-size))
	    (setq last next)
	  (setq seqs (cons (cons beg last) seqs))
	  (setq beg next)
	  (setq last next))
	(setq seq-nums (cdr seq-nums)))
      (setq seqs (cons (cons beg last) seqs)))
    (nreverse seqs)))


	 

(defun vm-fetch-imap-message-size (m)
  "Given an IMAP message M, return its message size by looking up the
cached tables.  If there is no cached data, return nil.  USR, 2012-10-19"
  (with-current-buffer (vm-buffer-of m)
    (condition-case _error
	(let ((uid-sym (intern-soft (vm-imap-uid-of m)
				    (vm-folder-imap-flags-obarray))))
	  (car (symbol-value uid-sym)))
      (error nil))))

;;;###autoload
(defun vm-imap-synchronize (&optional full)
  "Synchronize the current folder with the IMAP mailbox.
Changes made to the buffer are uploaded to the server first before
downloading the server data.
Deleted messages are not expunged.

Prefix argument FULL says to write every message's attributes to the server,
rather than only those of the messages whose attributes changed in this
session, and to fetch a message the cache no longer holds rather than leaving
it alone.  This is useful for saving offline work on the cache folder, whose
expunges are sent whether FULL is given or not: VM records them as the reader
makes them.

FULL used to delete on the server every message the mailbox had and the cache
did not.  A damaged cache says the same thing as a reader who expunged, so
that destroyed mail nobody asked it to (emacs-vm/vm#752)."
  (interactive "P")
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  ;;--------------------------
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (vm-display nil nil '(vm-imap-synchronize) '(vm-imap-synchronize))
  (if (not (eq vm-folder-access-method 'imap))
      (vm-inform 0 "%s: This is not an IMAP folder" (buffer-name))
    ;; On the driver, which is the only way this is done: the work here is
    ;; the expensive half -- the flags of every message in the mailbox come
    ;; down it -- and on a folder of six thousand it was half a minute of
    ;; frozen Emacs.
    (vm-imap-net-synchronize full t)))
  


;; ---------------------------------------------------------------------------
;;; Utilities for maildrop specs  (this should be moved up top)
;;
;; A maildrop spec is of the form
;;      protocol:hostname:port:mailbox:auth:loginid:password 
;;             0        1    2       3    4       5        6
;; vm-imap-find-spec-for-buffer: (buffer) -> maildrop-spec
;; vm-imap-make-filename-for-spec: (maildrop-spec) -> string
;; vm-imap-normalize-spec: (maildrop-spec) -> maildrop-spec
;; vm-imap-account-name-for-spec: (maildrop-spec) -> string
;; vm-imap-spec-for-account: (string) -> maildrop-spec
;; vm-imap-parse-spec-to-list: (maildrop-spec) -> string list
;; vm-imap-spec-list-to-host-alist: 
;;	(maildrop-spec list) -> (string, maildrop-spec) alist
;; ---------------------------------------------------------------------------

;; ----------- missing functions-----------
;;-----------------------------------------

;;;###autoload
(defun vm-imap-find-spec-for-buffer (buffer)
  "Find the IMAP maildrop spec for the folder BUFFER."
  (with-current-buffer buffer
    (vm-folder-imap-maildrop-spec)))

(defvar vm-imap-account-folder-cache nil
  "Caches the list of all folders on an IMAP account.")

(defun vm-imap-folder-completion-list (string predicate method)
  "Find completions for STRING as an IMAP folder name, satisfying
  PREDICATE.  The third argument METHOD is one of:

`nil' - try-completion, returns string if there are mult possibilities,
`t' - all-completions, returns a list of all completions,
`lambda' - test-completion, test if the string is an exact match for a
           possibility , and
a pair (boundaries. SUFFIX) - completion-boundaries.

See Info node `(elisp)Programmed Completion'."
  ;; selectable-only is used via dynamic binding

  (let ((account-list (mapcar (lambda (a) (list (concat (cadr a) ":")))
			      vm-imap-account-alist))
	completion-list folder account spec mailbox-list)

    ;; handle SPC completion (remove last " " from string)
    (when (and (> (length string) 0)
	       (string= " " (substring string -1)))
      (setq string (substring string 0 -1)))

    ;; check if account-name is present
    (setq folder (try-completion (or string "") account-list predicate))
    (setq account (car (vm-parse (if (stringp folder) folder string)
				 "\\([^:]+\\):" 1)))
    
    ;; if yes, get folders of the account into completion-list
    (when account
      (setq mailbox-list (cdr (assoc account vm-imap-account-folder-cache)))
      (setq spec (vm-imap-spec-for-account account))
      (when (and (null mailbox-list) spec)
	;; Completion has to answer with the names it has, so this is the one
	;; place that waits on purpose.  It waits on the driver rather than the
	;; blocking implementation: one connection, made and read the way every
	;; other path here now does it, and `accept-process-output' leaves C-g
	;; working.  What is asked for once is cached, so the next TAB is
	;; instant.
	(vm-inform 6 "Asking %s what folders it has..." account)
	(setq mailbox-list (vm-imap-net-mailbox-names spec selectable-only))
	(when mailbox-list
	  (add-to-list 'vm-imap-account-folder-cache
		       (cons account mailbox-list))))
      (setq completion-list 
	    (mapcar (lambda (m) (list (format "%s:%s" account m)))
		    mailbox-list))
      (setq folder (try-completion (or string "") completion-list predicate)))
    
    ;; process the requested method
    (setq folder (if (eq folder t)
		     string
		   (or folder string)))

    (cond ((null method)		; try-completion
	   folder)
	  ((eq method t)		; all-completions
	   (mapcar 'car
		   (vm-delete (lambda (c)
				(string-prefix-p folder (car c)))
			      (or completion-list account-list) t))
	   )
	  ((eq method 'lambda)		; test-completion
	   (try-completion folder completion-list predicate)))))

;;;###autoload
(defun vm-read-imap-folder-name (prompt &optional selectable--only
					_newone default) 
  "Read an IMAP folder name in the format account:mailbox, return an
IMAP mailbox spec." 
  (let* ((selectable-only selectable--only)
	 folder-input spec list ;; completion-list process
	 default-account default-folder
	 (vm-imap-ok-to-ask t)
	 (account-list (mapcar 'cadr vm-imap-account-alist))
	 account-and-folder account folder) ;; mailbox-list
    (if (null account-list)
	(error "No known IMAP accounts.  Please set vm-imap-account-alist."))
    (if default 
	(setq list (vm-imap-parse-spec-to-list default)
	      default-account 
	      (cadr (assoc (vm-imapdrop-sans-password-and-mailbox default)
			   vm-imap-account-alist))
	      default-folder (nth 3 list))
      (setq default-account 
	    (or vm-last-visit-imap-account vm-imap-default-account)))
    (setq folder-input
	  (completing-read
	   ;; prompt
	   (format			
	    ;; "IMAP folder:%s " 
	    "%s%s" prompt
	    (if (and default-account default-folder)
		(format "(default %s:%s) " default-account default-folder)
	      ""))
	   ;; collection
	   'vm-imap-folder-completion-list 
	   ;; predicate, require-match
	   nil nil
	   ;; initial-input
	   (if default-account		
	       (format "%s:" default-account)
	     "")))
    (if (or (equal folder-input "")  
	    (equal folder-input (format "%s:" default-account)))
	(if (and default-account default-folder)
	    (setq folder-input (format "%s:%s" default-account default-folder))
	  (error 
	   "IMAP folder required in the format account-name:folder-name"))) 
    (setq account-and-folder (vm-parse folder-input "\\([^:]+\\):?" 1 2)
	  account (car account-and-folder)
	  folder (cadr account-and-folder)
	  spec (vm-imap-spec-for-account account))
    (if (null folder)
	(error 
	 "IMAP folder required in the format account-name:folder-name"))
    (if (null spec)
	(error "Unknown IMAP account %s" account))
    (setq list (vm-imap-parse-spec-to-list spec))
    (setcar (nthcdr 3 list) folder)
    (setq vm-last-visit-imap-account account)
    (vm-imap-encode-list-to-spec list)
    ))

;; This is unfinished
;;;###autoload
(defun vm-create-imap-folder (folder)
  "Create a folder on an IMAP server.
First argument FOLDER is read from the minibuffer if called
interactively.  Non-interactive callers must provide an IMAP
maildrop specification for the folder as described in the
documentation for `vm-spool-files'."
  ;; Creates a self-contained IMAP session and destroys it at the end.
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (let ((this-command this-command)
	   (last-command last-command)
	   (folder (vm-read-imap-folder-name "Create IMAP folder: " nil t)))
       ;;-------------------
       (vm-buffer-type:exit)
       ;;-------------------
       (list folder))
     ))
  (let* ((vm-imap-ok-to-ask t)
	 (account (vm-imap-account-name-for-spec folder))
	 (mailbox (nth 3 (vm-imap-parse-spec-to-list folder)))
	 (folder-display (or (vm-imap-folder-for-spec folder)
			     (vm-safe-imapdrop-string folder)))
	 )
    (ignore account)
    ;; On the driver: one command to a server has no more business freezing
    ;; Emacs than a fetch has.
    (vm-imap-net-mailbox-command
     folder (format "CREATE %s" (vm-imap-quote-mailbox-name mailbox))
     "CREATE" (format "Folder %s created" folder-display))))
;;;###autoload (autoload 'vm-imap-create-folder "vm-imap" nil t)
(defalias 'vm-imap-create-folder 'vm-create-imap-folder)

;;;###autoload
(defun vm-delete-imap-folder (folder)
  "Delete a folder on an IMAP server.
First argument FOLDER is read from the minibuffer if called
interactively.  Non-interactive callers must provide an IMAP
maildrop specification for the folder as described in the
documentation for `vm-spool-files'."
;; Creates a self-contained IMAP session and destroys it at the end.
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name "Delete IMAP folder: " nil nil)))))
  (let* ((vm-imap-ok-to-ask t)
	 (mailbox (nth 3 (vm-imap-parse-spec-to-list folder)))
	 (folder-display (or (vm-imap-folder-for-spec folder)
			     (vm-safe-imapdrop-string folder))))
    ;; On the driver: one command to a server has no more business freezing
    ;; Emacs than a fetch has.
    (vm-imap-net-mailbox-command
     folder (format "DELETE %s" (vm-imap-quote-mailbox-name mailbox))
     "DELETE" (format "Folder %s deleted" folder-display))))
;;;###autoload (autoload 'vm-imap-delete-folder "vm-imap" nil t)
(defalias 'vm-imap-delete-folder 'vm-delete-imap-folder)

;;;###autoload
(defun vm-rename-imap-folder (source dest)
  "Rename a folder on an IMAP server.
Argument SOURCE and DEST are read from the minibuffer if called
interactively.  Non-interactive callers must provide full IMAP
maildrop specifications for SOURCE and DEST as described in the
documentation for `vm-spool-files'."
;; Creates a self-contained IMAP session and destroys it at the end.
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (let ((this-command this-command)
	   (last-command last-command)
	   source dest)
       (setq source (vm-read-imap-folder-name "Rename IMAP folder: " t nil))
       (setq dest (vm-read-imap-folder-name
		   (format "Rename %s to: " 
			   (or (vm-imap-folder-for-spec source)
			       (vm-safe-imapdrop-string source)))
		   nil t))
       (list source dest))))
  (let* ((vm-imap-ok-to-ask t)
	 (mailbox-source (nth 3 (vm-imap-parse-spec-to-list source)))
	 (mailbox-dest (nth 3 (vm-imap-parse-spec-to-list dest))))
    ;; On the driver, as the other mailbox commands are.
    (vm-imap-net-mailbox-command
     source (format "RENAME %s %s"
		    (vm-imap-quote-mailbox-name mailbox-source)
		    (vm-imap-quote-mailbox-name mailbox-dest))
     "RENAME"
     (format "Folder %s renamed to %s"
	     (or (vm-imap-folder-for-spec source)
		 (vm-safe-imapdrop-string source))
	     (or (vm-imap-folder-for-spec dest)
		 (vm-safe-imapdrop-string dest))))))
;;;###autoload (autoload 'vm-imap-rename-folder "vm-imap" nil t)
(defalias 'vm-imap-rename-folder 'vm-rename-imap-folder)

;;;###autoload
(defun vm-list-imap-folders (account &optional filter-new)
  "List all folders on an IMAP account ACCOUNT, along with the
counts of messages in them.  The account must be one declared in
`vm-imap-account-alist'.

With a prefix argument, it lists only the folders with new messages in
them."
;; Creates a self-contained IMAP session and destroys it at the end.
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (let ((this-command this-command)
	   (last-command last-command)
	   (completion-list (mapcar (function cadr) vm-imap-account-alist)))
       (list (completing-read 
	      ;; prompt
	      "IMAP account: " 
	      ;; collection
	      completion-list 
	      ;; predicate, require-match
	      nil t
	      ;; initial-input
	      (if vm-last-visit-imap-account		
		  (format "%s" vm-last-visit-imap-account)
		"")
	      )
	     current-prefix-arg))))
  (require 'ehelp)
  (setq vm-last-visit-imap-account account)
  (let ((vm-imap-ok-to-ask t)
	(spec (vm-imap-spec-for-account account)))
    (unless spec
      (error (concat "No IMAP account named %S in `vm-imap-account-alist',"
		     " so there is nothing to list")
	     account))
    ;; A listing is a command per mailbox, so it is the slowest thing VM asks
    ;; a server for and the one worst spent frozen.  On the driver: the list
    ;; is shown when it arrives.
    (vm-imap-net-list-folders
     spec
     (lambda (result)
       (if (vm-net-error-p result)
	   (vm-warn 0 2 "Could not list %s: %s" account
		    (error-message-string result))
	 (vm-imap-show-folder-list account result filter-new))))
    (vm-inform 5 "Asking %s what folders it has..." account)))
(defun vm-imap-show-folder-list (account mailbox-status-list filter-new)
  "Show what ACCOUNT holds: MAILBOX-STATUS-LIST is (MAILBOX MESSAGES RECENT).
FILTER-NEW leaves out the mailboxes with nothing new in them.  Split out of
`vm-list-imap-folders' so that the listing can be shown when it arrives
rather than only when it was waited for."
  (require 'ehelp)
  (let ((sorted (sort (copy-sequence mailbox-status-list)
		      (lambda (one other)
			(string-lessp (car one) (car other)))))
	(buffer (get-buffer-create (format "*%s folders*" account))))
    (with-electric-help
     (lambda ()
       (dolist (mbstat sorted)
	 (if (or (null filter-new) (> (nth 2 mbstat) 0))
	     (princ (format "%s: %s messages, %s new \n"
			    (car mbstat) (nth 1 mbstat) (nth 2 mbstat))))))
     buffer)))

;;;###autoload (autoload 'vm-imap-list-folders "vm-imap" nil t)
(defalias 'vm-imap-list-folders 'vm-list-imap-folders)

;;; Robert Fenk's draft function for saving messages to IMAP folders.

(defun vm-imap-fcc-mailbox ()
  "The mailbox the IMAP-FCC header of this composition names, or nil for none.
Trimmed: the whitespace around a header value is not part of the value, and
a mailbox name carrying it is a different mailbox, which the server creates
and files the copy in without anyone asking for it.  Signals when the header
is there and names nothing."
  (let ((value (vm-mail-get-header-contents "IMAP-FCC:")))
    (when value
      (let ((mailbox (string-trim value)))
	(when (string-empty-p mailbox)
	  (error "The IMAP-FCC header names no mailbox; remove it or name one"))
	mailbox))))

;;;###autoload
(defun vm-imap-save-composition ()
  "Saves the current composition in the IMAP folder given by the
IMAP-FCC header.

VM calls this itself as it sends, so there is nothing to add to
`mail-send-hook' any more (issue #68).  A configuration that still has it
there does no harm: the header is removed by the time the hook runs, so
this finds nothing to file.

An `FCC:' header naming an IMAP maildrop is not this function\'s business:
VM files those itself as it sends (`vm-do-fcc\'), so doing it here as well
would put two copies on the server (issue #605).

May throw exceptions." 
  ;; FIXME This function should not be throwing exceptions.
  ;; Creates a self-contained IMAP session and destroys it at the end.
  (let ((mailbox (vm-imap-fcc-mailbox))
	(mailboxes nil)
	maildrop
	(flags nil) string m ;; response
	(vm-imap-ok-to-ask t))
    (if (null mailbox)
	(setq mailboxes nil)
      ;; IMAP-FCC header present
      (when vm-mail-buffer		; has parent folder
	(with-current-buffer vm-mail-buffer
	  ;;----------------------------
	  (vm-buffer-type:enter 'folder)
	  ;;----------------------------
	  (setq m (car vm-message-pointer))
	  (when m 
	    (set-buffer (vm-buffer-of (vm-real-message-of m))))
	  (if (eq vm-folder-access-method 'imap)
	      (setq maildrop (vm-folder-imap-maildrop-spec)))
	  ;;-------------------
	  (vm-buffer-type:exit)
	  ;;-------------------
	  ))
      (when (null maildrop)
	;; No parent IMAP folder to inherit the account from, so fall
	;; back on the default account.
	(when (null vm-imap-default-account)
	  (error "Set `vm-imap-default-account' to use IMAP-FCC"))
	(setq maildrop (vm-imap-spec-for-account vm-imap-default-account))
	(when (null maildrop)
	  (error "No IMAP account named \"%s\" in `vm-imap-account-alist'"
		 vm-imap-default-account)))
      ;; the maildrop, not a session: the copy goes through the driver where
      ;; the maildrop allows it, and only what is left opens a connection and
      ;; waits for it
      (setq mailboxes (list (cons mailbox maildrop)))
      (vm-mail-mode-remove-header "IMAP-FCC:"))

    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (setq string (concat (buffer-substring (point-min) (match-beginning 0))
			 (buffer-substring
			  (match-end 0) (point-max))))
    (setq string (vm-imap-subst-CRLF-for-LF string))
    
    (while mailboxes
      (setq mailbox (car (car mailboxes)))
      (setq maildrop (cdr (car mailboxes)))
      (vm-imap-net-append-text maildrop mailbox string
			       (vm-imap-flag-list-string flags) t)
      (setq mailboxes (cdr mailboxes)))
    ))

;;;###autoload
(defun vm-imap-start-bug-report ()
  "Begin to compose a bug report for IMAP support functionality."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (setq vm-kept-imap-buffers nil)
  (setq vm-imap-keep-trace-buffer 20))

;;;###autoload
(defun vm-imap-submit-bug-report ()
  "Submit a bug report for VM's IMAP support functionality.  
It is necessary to run `vm-imap-start-bug-report' before the problem
occurrence and this command after the problem occurrence, in
order to capture the trace of IMAP sessions during the occurrence.

The session still running is included, so a report can be made about a fetch
while it is happening; nothing is closed to collect it."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if (or vm-imap-keep-trace-buffer
	  (y-or-n-p "Did you run vm-imap-start-bug-report earlier? "))
      (vm-inform 5 "Thank you. Preparing the bug report... ")
    (vm-inform 1 (concat "Consider running vm-imap-start-bug-report "
			 "before the problem occurrence")))
  (let ((buffers (vm-imap-net-trace-buffers)))
    (vm-submit-bug-report
     nil (list (lambda () (vm-insert-session-traces "IMAP" buffers))))))


;;;###autoload
(defun vm-imap-set-default-attributes (m)
  (vm-set-headers-to-be-retrieved-of m nil)
  (vm-set-body-to-be-retrieved-of m nil)
  (vm-set-body-to-be-discarded-of m nil))

(provide 'vm-imap)
;;; vm-imap.el ends here
