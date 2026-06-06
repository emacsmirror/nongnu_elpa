;;; jabber-chat.el --- one-to-one chats  -*- lexical-binding: t; -*-

;; Copyright (C) 2005, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:
;;

;;; Code:

(require 'jabber-core)
(require 'jabber-alert)
(require 'jabber-chatbuffer)
(require 'jabber-reactions)
(require 'ewoc)
(require 'goto-addr)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-queue)
(require 'hex-util)

(eval-when-compile (require 'cl-lib))

(defgroup jabber-chat nil
  "Chat display options."
  :group 'jabber)

(defcustom jabber-chat-buffer-format "*%j-%a*"
  "The format specification for the name of chat buffers.

These fields are available (all are about the person you are chatting
with):

%n   Nickname, or JID if no nickname set
%j   Bare JID (without resource)
%r   Resource

These fields are about your account:

%a   Your bare JID (account)
%u   Your username
%s   Your server"
  :type 'string)

(defvar jabber-chat-header-line-format
  '("" (jabber-chat-buffer-show-avatar
	(:eval
	 (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
	   (propertize " "
		       'display (get buddy 'avatar)))))
    (:eval (jabber-jid-displayname jabber-chatting-with))
    " " (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
		 (propertize
		  (or
		   (cdr (assoc (get buddy 'show) jabber-presence-strings))
		   (get buddy 'show))
		  'face
		  (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
		      'jabber-roster-user-online))))
    " " (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with) 'status)))
    " " (:eval jabber-chat-encryption-message)	;see jabber-chatbuffer.el
    (:eval jabber-chat-receipt-message)	;see jabber-receipts.el
    (:eval (when jabber-chat-mam-syncing
	     (propertize " [syncing]" 'face 'shadow))))
  "The specification for the header line of chat buffers.
The format is that of `mode-line-format' and `header-line-format'.")

(defcustom jabber-chat-buffer-show-avatar nil
  "Show avatars in header line of chat buffer?
This variable might not take effect if you have changed
`jabber-chat-header-line-format'."
  :type 'boolean)

(defcustom jabber-chat-time-format "%H:%M"
  "The format specification for instant messages in the chat buffer.
See also `jabber-chat-delayed-time-format'.

See `format-time-string' for valid values."
  :type 'string)

(defcustom jabber-chat-delayed-time-format "%H:%M"
  "The format specification for delayed messages in the chat buffer.
See also `jabber-chat-time-format'.

See `format-time-string' for valid values."
  :type 'string)

(defcustom jabber-print-rare-time t
  "Non-nil means to print \"rare time\" indications in chat buffers.
The default settings tell every new hour."
  :type 'boolean)

(defcustom jabber-rare-time-format "%a %e %b %Y %H:00"
  "The format specification for the rare time information.
Rare time information will be printed whenever the current time,
formatted according to this string, is different to the last
rare time printed."
  :type 'string)

(defcustom jabber-chat-display-images t
  "When non-nil, fetch and display image URLs inline in chat buffers."
  :type 'boolean)

(defface jabber-rare-time-face
  '((t :inherit font-lock-comment-face :underline t))
  "Face for displaying rare time information.")

(defcustom jabber-chat-encrypted-indicator
  (propertize (if (char-displayable-p ?🔒) "🔒" "[E]") 'face 'shadow)
  "String prepended to the timestamp of encrypted messages."
  :type 'string)

(defface jabber-chat-nick-encrypted
  '((t :inherit font-lock-constant-face))
  "Face for own nick on encrypted messages.")

(defface jabber-chat-nick-foreign-encrypted
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for foreign nick on encrypted messages.")

(defface jabber-chat-nick-plaintext
  '((t :inherit font-lock-warning-face :slant italic))
  "Face for own nick on plaintext messages.")

(defface jabber-chat-nick-foreign-plaintext
  '((t :inherit font-lock-keyword-face :slant italic))
  "Face for foreign nick on plaintext messages.")

(defface jabber-chat-nick-system
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for system and special messages.")

(defface jabber-chat-text-local '((t ()))
  "Face used for text you write.")

(defface jabber-chat-text-foreign '((t ()))
  "Face used for text others write.")

(defface jabber-chat-error
  '((t :inherit error))
  "Face used for error messages.")

;;;###autoload
(defvar jabber-chatting-with nil
  "JID of the person you are chatting with.")

(defvar jabber-chat-printers '(jabber-chat-print-subject
			       jabber-chat-print-body
			       jabber-chat-print-url
			       jabber-chat-goto-address
			       jabber-chat-mark-oob-attachment
			       jabber-chat-mark-aesgcm-url
			       jabber-chat--schedule-image-scan)
  "List of functions that may be able to print part of a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local or :foreign, for sent or received stanza, respectively
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.")

(defvar jabber-chat--body-start nil
  "Buffer position where the current message body starts.
Bound dynamically during ewoc rendering so that printer-chain
functions can style or fontify only the body region.")

(defvar jabber-body-printers '(jabber-chat-normal-body)
  "List of functions that may be able to print a body for a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local, :foreign or :error
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.

These functions are called in order, until one of them returns
non-nil.

Add a function to the beginning of this list if the tag it handles
replaces the contents of the <body/> tag.")

(defvar jabber-chat-send-hooks nil
  "List of functions called when a chat message is sent.
The arguments are the text to send, and the id attribute of the
message.

The functions should return a list of XML nodes they want to be
added to the outgoing message.")

(defun jabber-chat--run-send-hooks (stanza body id)
  "Run `jabber-chat-send-hooks' and nconc results onto STANZA.
BODY and ID are passed to each hook function."
  (dolist (hook jabber-chat-send-hooks)
    (if (eq hook t)
        (when (local-variable-p 'jabber-chat-send-hooks)
          (dolist (global-hook (default-value 'jabber-chat-send-hooks))
            (nconc stanza (funcall global-hook body id))))
      (nconc stanza (funcall hook body id)))))

;; Global reference declarations

(declare-function jabber-compose "jabber-compose.el" (jc &optional recipient))
(declare-function jabber-chat-buffer-recenter-input "jabber-chatbuffer.el" ())
(declare-function jabber-omemo--send-chat "jabber-omemo" (jc body &optional extra-elements))
(declare-function jabber-openpgp--send-chat "jabber-openpgp" (jc body &optional extra-elements))
(declare-function jabber-openpgp-legacy--send-chat "jabber-openpgp-legacy" (jc body &optional extra-elements))
(declare-function jabber-muc-private-create-buffer "jabber-muc.el"
                  (jc group nickname))
(declare-function jabber-muc-print-prompt "jabber-muc.el"
                  (msg &optional local dont-print-nick-p))
(declare-function jabber-muc-private-print-prompt "jabber-muc.el" (msg))
(declare-function jabber-muc-system-prompt "jabber-muc.el" (&rest _ignore))
(declare-function jabber-muc-message-p "jabber-muc.el"(message))
(declare-function jabber-muc-sender-p "jabber-muc.el" (jid))
(declare-function jabber-muc-private-message-p "jabber-muc.el" (message))
(declare-function jabber-muc-nickname "jabber-muc.el" (group &optional jc))
(declare-function jabber-muc-our-nick-p "jabber-muc.el" (group nick))
(defvar jabber-muc-xmlns-user)
(declare-function jabber-image-fetch "jabber-image" (url callback &rest cbargs))
(declare-function jabber-omemo-aesgcm-decrypt "jabber-omemo"
                  (key iv ciphertext-with-tag))
(defvar jabber-backlog-days)
(defvar jabber-backlog-number)
(declare-function jabber-db-backlog "jabber-db.el"
                  (account peer &optional count start-time resource msg-type))
(declare-function jabber-db--store-outgoing "jabber-db.el"
                  (jc to body type))
(declare-function jabber-db-store-message "jabber-db.el"
                  (account peer direction type body timestamp
                           &optional resource stanza-id
                           server-id occupant-id oob-entries
                           encrypted))
(declare-function jabber-db--extract-occupant-id "jabber-db.el" (xml-data))
(declare-function jabber-message-correct--replace-id "jabber-message-correct"
                  (xml-data))
(declare-function jabber-message-correct--apply "jabber-message-correct"
                  (replace-id new-body new-from muc-p buffer
                              &optional new-occupant-id))
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-muc-printers)            ; jabber-muc.el
(declare-function jabber-mam-chat-opened "jabber-mam" (jc peer))
(declare-function jabber-chatstates--clear-typing "jabber-chatstates" ())
(defvar jabber-oob-xmlns)              ; jabber-xml.el
(defvar jabber-image-max-width)        ; jabber-image.el
(defvar jabber-image-max-height)       ; jabber-image.el

;;

(defconst jabber-chat--forward-xmlns "urn:xmpp:forward:0"
  "XEP-0297 forwarded stanza namespace.")

(defconst jabber-chat--carbons-xmlns "urn:xmpp:carbons:2"
  "XEP-0280 Message Carbons namespace.")

(defvar jabber-chat-earliest-backlog nil
  "Float-time of earliest backlog entry inserted into buffer.
nil if no backlog has been inserted.")

(defvar jabber-chat-muc-presence-patterns-history nil
  "History values selected for `jabber-muc-decorate-presence-patterns'.")

(defface jabber-muc-presence-dim
  '((t :inherit shadow :slant italic))
  "Face for diminished presence notifications.")

(defcustom jabber-muc-decorate-presence-patterns-alist
  '(("Show enter/leave diminished"
     ("." . jabber-muc-presence-dim))
    ("Show all"
     ("." . jabber-chat-text-foreign))
    ("Hide all"
     ("."))
    ("Hide enter/leave"
     ("\\( enters the room ([^)]+)\\| has left the chatroom\\)$")
     ("." . jabber-muc-presence-dim)))
  "List presence treatment specifications.
Each specification consists of a label (string) and a list of
pattern/face pairs which are suitable values for
`jabber-muc-decorate-presence-patterns'.  These pairs describe
how to highlight presence events in MUC chat logs."
  :type '(alist
          :key-type string
          :value-type (repeat
                       :tag "Patterns"
                       (cons :format "%v"
                             (regexp :tag "Regexp")
                             (choice
                              (const :tag "Ignore" nil)
                              (face :tag "Face" :value jabber-muc-presence-dim)))))
  :group 'jabber-alerts)

(defcustom jabber-muc-decorate-presence-patterns (cdar jabber-muc-decorate-presence-patterns-alist)
  "List of regular expressions and face pairs.
When a presence notification matches a pattern, display it with
associated face.  Ignore notification if face is nil."
  :type '(repeat
          :tag "Patterns"
          (cons :format "%v"
		(regexp :tag "Regexp")
		(choice
		 (const :tag "Ignore" nil)
		 (face :tag "Face" :value jabber-muc-presence-dim))))
  :group 'jabber-alerts)

;;;###autoload
(defun jabber-chat-get-buffer (chat-with &optional jc)
  "Return the chat buffer name for chatting with CHAT-WITH (bare or full JID).
When JC is provided, account-specific format specs (%a, %u, %s) are
expanded.  Either a string or a buffer is returned, so use `get-buffer'
or `get-buffer-create'."
  (format-spec jabber-chat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname chat-with))
		(cons ?j (jabber-jid-user chat-with))
		(cons ?r (or (jabber-jid-resource chat-with) ""))
		(cons ?a (if jc (jabber-connection-bare-jid jc) ""))
		(cons ?u (if jc (plist-get (fsm-get-state-data jc) :username) ""))
		(cons ?s (if jc (plist-get (fsm-get-state-data jc) :server) "")))))

(defun jabber-chat-find-buffer (chat-with)
  "Find an existing 1:1 chat buffer for CHAT-WITH, or nil."
  (jabber-chatbuffer--registry-get 'chat (jabber-jid-user chat-with)))

(defun jabber-chat-create-buffer (jc chat-with)
  "Prepare a buffer for chatting with CHAT-WITH.
This function is idempotent.
JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-chat-get-buffer chat-with jc))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode)

      (setq-local jabber-chatting-with chat-with)
      (jabber-chatbuffer--registry-put 'chat (jabber-jid-user chat-with))

      (jabber-chat-mode-setup jc #'jabber-chat-pp)
      (setq jabber-send-function #'jabber-chat-send)
      (setq header-line-format jabber-chat-header-line-format)

      (setq-local jabber-chat-earliest-backlog nil)

      ;; insert backlog
      (when (null jabber-chat-earliest-backlog)
	(let ((backlog-entries (jabber-db-backlog
				(jabber-connection-bare-jid jc)
				(jabber-jid-user chat-with))))
	  (if (null backlog-entries)
	      (setq jabber-chat-earliest-backlog (float-time))
	    ;; backlog-entries is DESC; last element is oldest.
	    (setq jabber-chat-earliest-backlog
		  (float-time (plist-get (car (last backlog-entries)) :timestamp)))
	    ;; ewoc-enter-first with DESC input produces ascending display.
	    ;; Insert in chunks to keep the UI responsive.
	    (cl-incf jabber-chat--backlog-generation)
	    (jabber-chat--insert-backlog-chunked
	     (current-buffer) backlog-entries
	     #'jabber-chat-display-buffer-images
	     jabber-chat--backlog-generation))))

      (jabber-chat-buffer-recenter-input))

    ;; Catch up missed 1:1 messages from MAM.  Run this after the setup
    ;; branch so initial creation and reopening an existing buffer both
    ;; request one catch-up.
    (jabber-mam-chat-opened jc (jabber-jid-user chat-with))

    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)

    (current-buffer)))

(defconst jabber-chat-backlog-chunk-size 100
  "Number of backlog entries to insert per timer tick.")

(defun jabber-chat-insert-backlog-entry (msg-plist)
  "Insert backlog MSG-PLIST at beginning of buffer."
  ;; Rare timestamps are especially important in backlog.  We risk
  ;; having superfluous timestamps if we just add before each backlog
  ;; entry.
  (let* ((message-time (plist-get msg-plist :timestamp))
	 (direction (plist-get msg-plist :direction))
	 (msg-type (plist-get msg-plist :msg-type))
	 (node-type (cond
		     ((string= msg-type "groupchat")
		      (let ((nick (jabber-jid-resource (plist-get msg-plist :from))))
			(if (or (and nick
				     (jabber-muc-our-nick-p jabber-group nick))
				(string= direction "out"))
			    :muc-local
			  :muc-foreign)))
		     ((string= direction "out") :local)
		     (t :foreign)))
	 (node-data (list node-type msg-plist)))

    ;; Insert after existing rare timestamp?
    (let ((node
           (if (and jabber-print-rare-time
                    (ewoc-nth jabber-chat-ewoc 0)
                    (eq (car (ewoc-data (ewoc-nth jabber-chat-ewoc 0))) :rare-time)
                    (not (jabber-rare-time-needed message-time (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc 0))))))
               (ewoc-enter-after jabber-chat-ewoc (ewoc-nth jabber-chat-ewoc 0) node-data)
             (let ((n (ewoc-enter-first jabber-chat-ewoc node-data)))
               (when jabber-print-rare-time
                 (ewoc-enter-first jabber-chat-ewoc (list :rare-time message-time)))
               n))))
      (when-let* ((id (plist-get msg-plist :id)))
        (puthash id node jabber-chat--msg-nodes)))))

(defun jabber-chat--insert-backlog-chunked (buffer entries callback
                                                   &optional generation)
  "Insert ENTRIES into BUFFER's ewoc in chunks to avoid blocking.
Inserts `jabber-chat-backlog-chunk-size' entries per timer tick.
Call CALLBACK with no arguments when all entries are inserted.
GENERATION, when non-nil, is checked against the buffer's
`jabber-chat--backlog-generation'; a mismatch means a newer
refresh has started and this insert sequence should abort."
  (if (or (null entries) (not (buffer-live-p buffer))
          (and generation
               (not (eql generation
                         (buffer-local-value
                          'jabber-chat--backlog-generation buffer)))))
      (when (and callback (buffer-live-p buffer)
                 (or (null generation)
                     (eql generation
                          (buffer-local-value
                           'jabber-chat--backlog-generation buffer))))
        (with-current-buffer buffer
          (funcall callback)))
    (with-current-buffer buffer
      (let* ((buffer-undo-list t)
             (inhibit-read-only t)
	     (chunk (cl-subseq entries 0
			       (min jabber-chat-backlog-chunk-size
				    (length entries))))
	     (rest (nthcdr (length chunk) entries)))
	(mapc #'jabber-chat-insert-backlog-entry chunk)
	(if rest
	    (run-with-timer 0.1 nil
			    #'jabber-chat--insert-backlog-chunked
			    buffer rest callback generation)
	  (when callback (funcall callback)))))))

(defun jabber-chat-display-more-backlog (how-many)
  "Display more messages from local history.
HOW-MANY is the number of additional messages to show.
When nil or 0, display all messages."
  (interactive
   (let ((input (read-string "How many more messages (empty for all)? ")))
     (list (if (string-empty-p input) nil
	     (string-to-number input)))))
  (let* ((current-count (length (ewoc-collect
				 jabber-chat-ewoc
				 (lambda (data) (not (eq (car data) :rare-time))))))
	 (target-count (if (or (null how-many) (zerop how-many)) t
			 (+ current-count how-many))))
    (setq jabber-chat-buffer-msg-count target-count)
    (jabber-chat-buffer-refresh)
    (let ((new-count (length (ewoc-collect
			      jabber-chat-ewoc
			      (lambda (data) (not (eq (car data) :rare-time)))))))
      (if (> new-count current-count)
	  (message "Loaded %d messages from local history" new-count)
	(message "No older messages in local history")))))

(jabber-chain-add 'jabber-message-chain #'jabber-process-chat)

(defun jabber-get-forwarded-message (xml-data)
  "Extract the inner message from a carbon-forwarded stanza.
Returns the inner message element, or nil if XML-DATA is not a carbon."
  (cdr (jabber-chat--extract-carbon xml-data)))

(defun jabber-chat--extract-carbon (xml-data)
  "Extract carbon type and inner message from XML-DATA.
Returns (TYPE . MESSAGE) where TYPE is `sent' or `received',
or nil if XML-DATA is not a carbon."
  (let ((wrapper (seq-find
                  (lambda (child)
                    (and (memq (jabber-xml-node-name child) '(sent received))
                         (string= (jabber-xml-get-xmlns child)
                                  jabber-chat--carbons-xmlns)))
                  (jabber-xml-node-children xml-data))))
    (when wrapper
      (let* ((type (jabber-xml-node-name wrapper))
             (fwd (jabber-xml-child-with-xmlns
                   wrapper jabber-chat--forward-xmlns))
             (msg (car (jabber-xml-get-children fwd 'message))))
        (when msg (cons type msg))))))

(defun jabber-chat--unwrap-carbon (jc xml-data)
  "If XML-DATA is a carbon-forwarded message, unwrap it.
Return (EFFECTIVE-XML-DATA . CHAT-BUFFER-OR-NIL).
JC is the Jabber connection.

Validates that the outer stanza's `from' matches our bare JID to
prevent forged carbons (CVE-2017-5589)."
  (let ((carbon (jabber-chat--extract-carbon xml-data)))
    (if (not carbon)
        (cons xml-data nil)
      (let ((outer-from (jabber-jid-user
                         (jabber-xml-get-attribute xml-data 'from))))
        (if (not (string= outer-from (jabber-connection-bare-jid jc)))
            (progn
              (warn "Jabber: dropping forged carbon from %s" outer-from)
              (cons xml-data nil))
          (let* ((type (car carbon))
                 (inner-msg (cdr carbon)))
            (pcase type
              ('sent
               (let ((to (jabber-xml-get-attribute inner-msg 'to)))
                 (cons inner-msg
                       (when to (jabber-chat-create-buffer jc to)))))
              ('received
               (cons inner-msg nil)))))))))

(defun jabber-chat--reaction-only-p (xml-data)
  "Return non-nil when XML-DATA is a reaction-only stanza."
  (jabber-reactions--reaction-only-p xml-data))

(defun jabber-chat--store-carbon (jc xml-data)
  "Store a carbon-forwarded message in the database.
JC is the Jabber connection.  XML-DATA is the inner (unwrapped,
possibly decrypted) message stanza.
Direction is determined by comparing the sender to our bare JID.
Correction stanzas (XEP-0308) are skipped; the correction handler
updates the original row instead."
  (unless (jabber-message-correct--replace-id xml-data)
    (let* ((from (jabber-xml-get-attribute xml-data 'from))
           (to (jabber-xml-get-attribute xml-data 'to))
           (body (car (jabber-xml-node-children
                       (car (jabber-xml-get-children xml-data 'body)))))
           (stanza-id (jabber-xml-get-attribute xml-data 'id))
           (timestamp (jabber-message-timestamp xml-data))
           (our-jid (jabber-connection-bare-jid jc))
           (sent-p (string= (jabber-jid-user from) our-jid))
           (direction (if sent-p "out" "in"))
           (peer-jid (if sent-p to from))
           (peer (when peer-jid (jabber-jid-user peer-jid)))
           (encrypted (or (jabber-xml-child-with-xmlns
                           xml-data "eu.siacs.conversations.axolotl")
                          (jabber-xml-child-with-xmlns
                           xml-data "jabber:x:encrypted")
                          (jabber-xml-child-with-xmlns
                           xml-data "urn:xmpp:openpgp:0"))))
      (when (and peer body)
        (jabber-db-store-message
         our-jid peer direction "chat" body
         (floor (float-time (or timestamp (current-time))))
         (when from (jabber-jid-resource from))
         stanza-id
         nil (jabber-db--extract-occupant-id xml-data) nil
         encrypted)))))

(defun jabber-chat--select-buffer (jc from &optional carbon-buffer)
  "Return the chat buffer for an incoming message from FROM.
CARBON-BUFFER, if non-nil, is a buffer already created for a
carbon-forwarded message.  JC is the Jabber connection."
  (if (jabber-muc-sender-p from)
      (jabber-muc-private-create-buffer
       jc (jabber-jid-user from) (jabber-jid-resource from))
    (or carbon-buffer
        (jabber-chat-create-buffer jc from))))

(defun jabber-chat--set-body (xml-data text)
  "Replace or create the <body> child of XML-DATA with TEXT.
Mutates XML-DATA in place and returns it."
  (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
    (if body-el
        (setcar (cddr body-el) text)
      (nconc xml-data (list `(body () ,text)))))
  xml-data)

(defvar jabber-chat-decrypt-handlers nil
  "Alist of registered decryption handlers.
Each entry is (ID . PLIST) where PLIST has keys:
  :detect    - function (XML-DATA) -> parsed-data or nil
  :decrypt   - function (JC XML-DATA PARSED) -> XML-DATA (modified)
  :priority  - integer, lower runs first (default 50)
  :error-label - string for error body, e.g. \"OMEMO\"
Handlers are tried in :priority order (ascending).
The first whose :detect returns non-nil wins.")

(defvar jabber-chat--sorted-decrypt-handlers-cache nil
  "Cached sorted handler list.  Invalidated on register/unregister.")

(defun jabber-chat-register-decrypt-handler (id &rest props)
  "Register decryption handler ID with properties PROPS.
ID is a symbol (e.g. `omemo', `openpgp', `openpgp-legacy').
PROPS is a plist with keys :detect, :decrypt, :priority, :error-label.
If ID is already registered, replace it."
  (setq jabber-chat-decrypt-handlers
        (assq-delete-all id jabber-chat-decrypt-handlers))
  (push (cons id props) jabber-chat-decrypt-handlers)
  (setq jabber-chat--sorted-decrypt-handlers-cache nil))

(defun jabber-chat-unregister-decrypt-handler (id)
  "Remove decryption handler ID."
  (setq jabber-chat-decrypt-handlers
        (assq-delete-all id jabber-chat-decrypt-handlers))
  (setq jabber-chat--sorted-decrypt-handlers-cache nil))

(defun jabber-chat--sorted-decrypt-handlers ()
  "Return `jabber-chat-decrypt-handlers' sorted by :priority."
  (or jabber-chat--sorted-decrypt-handlers-cache
      (setq jabber-chat--sorted-decrypt-handlers-cache
            (sort (copy-sequence jabber-chat-decrypt-handlers)
                  (lambda (a b)
                    (< (or (plist-get (cdr a) :priority) 50)
                       (or (plist-get (cdr b) :priority) 50)))))))

(defun jabber-chat--try-decrypt (jc xml-data parsed handler-props)
  "Call the :decrypt function from HANDLER-PROPS on XML-DATA with error handling.
JC is the Jabber connection.  PARSED is the parsed message plist.
On success, return the (mutated) XML-DATA.  On error, replace the
body with \"[LABEL: could not decrypt]\" and return XML-DATA."
  (condition-case err
      (funcall (plist-get handler-props :decrypt) jc xml-data parsed)
    (error
     (message "%s decrypt failed: %s"
              (plist-get handler-props :error-label)
              (error-message-string err))
     (jabber-chat--set-body xml-data
			    (format "[%s: could not decrypt]"
				    (plist-get handler-props :error-label)))
     xml-data)))

(defvar jabber-chat--crypto-loaded nil
  "Non-nil after crypto modules have been loaded.")

(defun jabber-chat--decrypt-if-needed (jc xml-data)
  "Dispatch XML-DATA to the first matching decrypt handler.
On first call, loads crypto modules so their handlers are registered.
Tries handlers in :priority order.  Returns XML-DATA, possibly
with its body replaced by decrypted plaintext (or an error
placeholder).  Skips dispatch when XML-DATA has no `from' attribute.
JC is the Jabber connection."
  (unless jabber-chat--crypto-loaded
    (condition-case nil (require 'jabber-omemo nil t) (error nil))
    (condition-case nil (require 'jabber-openpgp nil t) (error nil))
    (condition-case nil (require 'jabber-openpgp-legacy nil t) (error nil))
    (setq jabber-chat--crypto-loaded t))
  ;; First-match-wins: the dispatcher stops at the first handler whose
  ;; :detect returns non-nil, so re-entrancy guards are unnecessary.
  (if (null (jabber-xml-get-attribute xml-data 'from))
      xml-data
    (cl-loop for (_id . props) in (jabber-chat--sorted-decrypt-handlers)
             for parsed = (funcall (plist-get props :detect) xml-data)
             when parsed
             return (jabber-chat--try-decrypt jc xml-data parsed props)
             finally return xml-data)))

(defun jabber-chat--display-message (jc _xml-data chat-buffer
					error-p from msg-plist)
  "Display an incoming message and run alert hooks.
Insert an EWOC entry into CHAT-BUFFER for the message described by
MSG-PLIST, then run `jabber-message-hooks' and
`jabber-alert-message-hooks'.  ERROR-P is non-nil when the stanza
contains an error element.  FROM is the sender JID.  JC is the
Jabber connection, used to detect self-authored carbons.
_XML-DATA is reserved for future use by OMEMO."
  (let ((body-text (plist-get msg-plist :body))
        (self-p (string= (jabber-jid-user from)
                         (jabber-connection-bare-jid jc))))
    (with-current-buffer chat-buffer
      (jabber-chat-buffer-with-scrolltobottom
        (jabber-chatstates--clear-typing)
        (jabber-maybe-print-rare-time
         (jabber-chat-ewoc-enter
          (list (if error-p :error :foreign) msg-plist))))
      (when (and (not error-p) (not self-p))
        (let ((inhibit-message jabber-chat-mam-syncing))
          (dolist (hook '(jabber-message-hooks jabber-alert-message-hooks))
            (run-hook-with-args hook
                                from (current-buffer) body-text
                                (funcall jabber-alert-message-function
                                         from (current-buffer) body-text))))))))

(defun jabber-process-chat (jc xml-data)
  "If XML-DATA is a one-to-one chat message, handle it as such.
JC is the Jabber connection."
  (when (and (not (jabber-muc-message-p xml-data))
             (jabber-xml-get-attribute xml-data 'from))
    (let* ((unwrapped (jabber-chat--unwrap-carbon jc xml-data))
           (is-carbon (not (eq xml-data (car unwrapped))))
           (xml-data (jabber-chat--decrypt-if-needed jc (car unwrapped)))
           (carbon-buffer (cdr unwrapped))
           (from (jabber-xml-get-attribute xml-data 'from))
           (error-p (jabber-xml-get-children xml-data 'error))
           (msg-plist (jabber-chat--msg-plist-from-stanza xml-data)))
      (unless (jabber-chat--reaction-only-p xml-data)
        (when is-carbon
          (jabber-chat--store-carbon jc xml-data))
        (let ((replace-id (jabber-message-correct--replace-id xml-data)))
          (if (and replace-id (not jabber-chat-mam-syncing))
              (jabber-message-correct--apply
               replace-id
               (plist-get msg-plist :body)
               from
               nil
               (jabber-chat-find-buffer from)
               (jabber-db--extract-occupant-id xml-data))
            (when (or error-p
                      (run-hook-with-args-until-success 'jabber-chat-printers
                                                        msg-plist :foreign :printp))
              (jabber-chat--display-message
               jc xml-data
               (jabber-chat--select-buffer jc from carbon-buffer)
               error-p from msg-plist))))))))

(defun jabber-chat-send (jc body &optional extra-elements)
  "Send BODY through connection JC, and display it in chat buffer.
JC is the Jabber connection.
EXTRA-ELEMENTS, when non-nil, is a list of XML sexp elements to
splice into the stanza after the body (e.g. OOB, hints)."
  (pcase jabber-chat-encryption
    ('omemo (jabber-omemo--send-chat jc body extra-elements))
    ('openpgp (require 'jabber-openpgp)
              (jabber-openpgp--send-chat jc body extra-elements))
    ('openpgp-legacy (require 'jabber-openpgp-legacy)
                     (jabber-openpgp-legacy--send-chat jc body extra-elements))
    (_
     ;; Build the stanza...
     (let* ((id (format "emacs-msg-%.6f" (float-time)))
	    (stanza-to-send `(message
			      ((to . ,jabber-chatting-with)
			       (type . "chat")
			       (id . ,id))
			      (body () ,body)
			      ,@extra-elements)))
       ;; ...add additional elements...
       (jabber-chat--run-send-hooks stanza-to-send body id)
       ;; ...display it (skip for corrections, caller handles display).
       (unless (assq 'replace extra-elements)
         (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza-to-send)))
           (plist-put msg-plist :status :sent)
	   (when (run-hook-with-args-until-success 'jabber-chat-printers msg-plist :local :printp)
             (jabber-maybe-print-rare-time
              (jabber-chat-ewoc-enter (list :local msg-plist))))))
       ;; ...and send it...
       (jabber-send-sexp jc stanza-to-send)))))

(defun jabber-find-previous-visible-node (node)
  "Return first visible EWOC node preceding NODE.
Step backward over hidden nodes, like MUC presence join/leave
messages."
  (let* ((node-location (ewoc-location node))
         (prev (ewoc-prev jabber-chat-ewoc node))
         (prev-location (and prev (ewoc-location prev))))
    (while (and
            prev
            (not (equal (ewoc-data node) (ewoc-data prev)))
            (equal (marker-position node-location) (marker-position prev-location)))
      (setq prev (ewoc-prev jabber-chat-ewoc prev)
            prev-location (and prev (ewoc-location prev))))
    prev))

(defun jabber-chat-muc-presence-patterns-select (global)
  "Select a MUC presence treatment.
When GLOBAL is non-nil (prefix arg), set the default value instead
of the buffer-local one.
Prompts user to select a presence treatment by name, where the
name is the `car' of an entry in
`jabber-muc-decorate-presence-patterns-alist'.  The variable
`jabber-muc-decorate-presence-patterns' is set to the `cdr' of
the selected treatment.

By default, when `jabber-muc-decorate-presence-patterns' is
updated, it is made buffer local.  With a prefix argument, the
buffer-local state of the variable is not changed.

The chat buffer is redisplayed using the new value of
`jabber-muc-decorate-presence-patterns'.  Redisplaying the buffer
may take a few second, especially in MUCs with a large number of
participants connected through intermittent networks (like mobile
clients)."
  (interactive "P")
  (when-let* ((patterns (cdr
                         (assoc-string
                          (completing-read
                           "MUC presence treatment: "
                           (mapcar #'car jabber-muc-decorate-presence-patterns-alist)
                           nil t nil
                           'jabber-chat-muc-presence-patterns-history)
                          jabber-muc-decorate-presence-patterns-alist))))
    (unless (equal patterns jabber-muc-decorate-presence-patterns)
      (if global
          (setq jabber-muc-decorate-presence-patterns patterns)
        (setq-local jabber-muc-decorate-presence-patterns patterns))
      (jabber-chat-redisplay))))

(defun jabber-chat-muc-presence-highlight (message)
  "Return non-nil to control MUC presence notification display for MESSAGE.
This matches MESSAGE's :muc-notification text against
`jabber-muc-decorate-presence-patterns' and returns the pattern
entry when a match is found, or nil if no matching pattern is
found."
  (seq-find
   (lambda (pair)
     (string-match (car pair) message nil 'inhibit-modify))
   jabber-muc-decorate-presence-patterns))

(defun jabber-chat--oob-field (oob-node child)
  "Return text content of CHILD element inside OOB-NODE, or nil."
  (when oob-node
    (car (jabber-xml-node-children
          (car (jabber-xml-get-children oob-node child))))))

(defun jabber-chat--extract-oob-entries (xml-data)
  "Extract all jabber:x:oob entries from XML-DATA.
Returns a list of (URL . DESC) cons cells, or nil."
  (let (entries)
    (dolist (child (jabber-xml-node-children xml-data))
      (when (and (listp child)
                 (string= (jabber-xml-get-attribute child 'xmlns)
                          jabber-oob-xmlns))
        (let ((url (jabber-chat--oob-field child 'url))
              (desc (jabber-chat--oob-field child 'desc)))
          (when url
            (push (cons url desc) entries)))))
    (nreverse entries)))

(defun jabber-chat--has-muc-invite-p (xml-data)
  "Return non-nil when XML-DATA carries a MUC invitation."
  (let ((muc-x (jabber-xml-child-with-xmlns
                xml-data jabber-muc-xmlns-user)))
    (and muc-x (jabber-xml-get-children muc-x 'invite))))

(defconst jabber-chat--reply-xmlns "urn:xmpp:reply:0"
  "XEP-0461 message replies namespace.")

(defconst jabber-chat--fallback-xmlns "urn:xmpp:fallback:0"
  "XEP-0428 fallback indication namespace.")

(defun jabber-chat--fallback-offset (value)
  "Return VALUE as a non-negative integer, or nil."
  (when (and (stringp value)
             (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value)))

(defun jabber-chat--reply-fallback-range (xml-data)
  "Return the XEP-0461 fallback body range in XML-DATA, or nil."
  (when-let* ((fallback (jabber-xml-child-with-xmlns
                         xml-data jabber-chat--fallback-xmlns))
              ((eq (jabber-xml-node-name fallback) 'fallback))
              ((string= (jabber-xml-get-attribute fallback 'for)
                        jabber-chat--reply-xmlns))
              (body (car (jabber-xml-get-children fallback 'body)))
              (start (jabber-xml-get-attribute body 'start))
              (end (jabber-xml-get-attribute body 'end)))
    (when-let* ((from (jabber-chat--fallback-offset start))
                (to (jabber-chat--fallback-offset end)))
      (list from to))))

(defun jabber-chat--strip-reply-fallback (body xml-data)
  "Return BODY with a valid XEP-0461 fallback range from XML-DATA removed.
Malformed fallback ranges leave BODY unchanged."
  (if-let* (((stringp body))
            (range (jabber-chat--reply-fallback-range xml-data))
            (start (car range))
            (end (cadr range))
            ((<= 0 start end (length body))))
      (concat (substring body 0 start)
              (substring body end))
    body))

(defun jabber-chat--build-msg-plist (xml-data delayed)
  "Build a message plist from the fields in XML-DATA.
DELAYED marks the message as delayed unconditionally."
  (let* ((msg-timestamp (jabber-message-timestamp xml-data))
         (oob-entries (jabber-chat--extract-oob-entries xml-data))
         (error-node (car (jabber-xml-get-children xml-data 'error)))
         (sid-el (jabber-xml-child-with-xmlns xml-data "urn:xmpp:sid:0"))
         (reply-el (jabber-xml-child-with-xmlns xml-data
                                                jabber-chat--reply-xmlns))
         (unstyled-el (jabber-xml-child-with-xmlns xml-data "urn:xmpp:styling:0"))
         (raw-body (car (jabber-xml-node-children
                         (car (jabber-xml-get-children xml-data 'body)))))
         (body (if reply-el
                   (jabber-chat--strip-reply-fallback raw-body xml-data)
                 raw-body)))
    (list
     :id (jabber-xml-get-attribute xml-data 'id)
     :server-id (when sid-el (jabber-xml-get-attribute sid-el 'id))
     :from (jabber-xml-get-attribute xml-data 'from)
     :body body
     :subject (car (jabber-xml-node-children
                    (car (jabber-xml-get-children xml-data 'subject))))
     :timestamp (or msg-timestamp (current-time))
     :delayed (or delayed (and msg-timestamp t))
     :encrypted (and (jabber-xml-child-with-xmlns
                      xml-data "eu.siacs.conversations.axolotl")
                     t)
     :oob-entries oob-entries
     :oob-url (caar oob-entries)
     :oob-desc (cdar oob-entries)
     :error-text (when error-node
                   (jabber-parse-error error-node))
     :reply-to-id (when reply-el
                    (jabber-xml-get-attribute reply-el 'id))
     :reply-to-jid (when reply-el
                     (jabber-xml-get-attribute reply-el 'to))
     :unstyled (and unstyled-el t))))

(defun jabber-chat--msg-plist-from-stanza (xml-data &optional delayed)
  "Extract display fields from XML-DATA into a message plist.
If DELAYED is non-nil, mark the message as delayed regardless of
whether a delay element is present."
  (let ((plist (jabber-chat--build-msg-plist xml-data delayed)))
    (when (jabber-chat--has-muc-invite-p xml-data)
      (plist-put plist :xml-data xml-data))
    plist))

(defun jabber-chat--insert-status-indicator (msg)
  "Insert a receipt status indicator for outgoing MSG.
Shows a dot for sent, check for delivered, green check for seen,
or X for undelivered."
  (when-let* ((status (plist-get msg :status)))
    (let ((indicator
           (pcase status
             (:sending (propertize " \u00b7" 'face 'warning))
             (:sent (propertize " \u00b7" 'face 'shadow))
             (:delivered (propertize " \u2713" 'face 'shadow))
             (:displayed (propertize " \u2713" 'face 'success))
             (:undelivered (propertize " \u2717" 'face 'error)))))
      (when indicator
        (insert indicator)))))

(defun jabber-chat--reaction-sender ()
  "Return the local sender key for reaction display."
  (jabber-reactions--local-sender))

(defun jabber-chat--reaction-entry-string (entry)
  "Return propertized reaction summary text for ENTRY."
  (propertize (format "%d%s"
                      (plist-get entry :count)
                      (plist-get entry :reaction))
              'face (if (plist-get entry :chosen)
                        'jabber-reaction-chosen
                      'jabber-reaction)))

(defun jabber-chat--insert-reactions (msg)
  "Insert compact reaction summaries for MSG."
  (unless (plist-get msg :retracted)
    (when-let* ((entries (jabber-reactions--display-entries
                          (plist-get msg :reactions)
                          (jabber-chat--reaction-sender))))
      (insert "\n"
              (string-join
               (mapcar #'jabber-chat--reaction-entry-string entries)
               "  ")))))

(defun jabber-chat--reply-context-label (msg)
  "Return a compact reply context label for MSG, or nil."
  (when-let* ((reply-id (plist-get msg :reply-to-id)))
    (let* ((jid (plist-get msg :reply-to-jid))
           (who (and jid
                     (or (jabber-jid-resource jid)
                         (jabber-jid-displayname jid)))))
      (if (and who (not (string-empty-p who)))
          (format "reply to %s (%s)" who reply-id)
        (format "reply to %s" reply-id)))))

(defun jabber-chat--insert-reply-context (msg)
  "Insert reply context line for MSG when reply metadata is present."
  (when-let* ((label (jabber-chat--reply-context-label msg)))
    (insert (propertize (concat label "\n") 'face 'shadow))))

(defun jabber-chat-pp--local (data)
  "Render a locally sent message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-chat-self-prompt msg (plist-get msg :timestamp)
                             (plist-get msg :delayed) /me-p)
    (jabber-chat--insert-reply-context msg)
    (let ((jabber-chat--body-start (point)))
      (run-hook-with-args 'jabber-chat-printers msg :local :insert))
    (when (plist-get msg :edited)
      (insert (propertize " (edited)" 'face 'shadow)))
    (jabber-chat--insert-status-indicator msg)
    (jabber-chat--insert-reactions msg)
    (insert "\n")))

(defun jabber-chat-pp--foreign (data)
  "Render a received message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-chat-print-prompt msg (plist-get msg :timestamp)
                              (plist-get msg :delayed) /me-p)
    (jabber-chat--insert-reply-context msg)
    (let ((jabber-chat--body-start (point)))
      (run-hook-with-args 'jabber-chat-printers msg :foreign :insert))
    (when (plist-get msg :edited)
      (insert (propertize " (edited)" 'face 'shadow)))
    (jabber-chat--insert-reactions msg)
    (insert "\n")))

(defun jabber-chat--insert-tombstone (msg)
  "Insert a retraction tombstone for MSG."
  (let ((moderator (plist-get msg :retracted-by))
        (reason (plist-get msg :retraction-reason)))
    (when moderator
      (setq moderator (or (jabber-jid-resource moderator) moderator)))
    (insert (propertize
             (concat "[Message retracted"
                     (when moderator (concat " by: " moderator))
                     (when reason (concat " reason: " reason))
                     "]")
             'face 'shadow))))

(defun jabber-chat-pp--muc-local (data)
  "Render a locally sent MUC message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-muc-print-prompt msg t /me-p)
    (if (plist-get msg :retracted)
        (jabber-chat--insert-tombstone msg)
      (jabber-chat--insert-reply-context msg)
      (let ((jabber-chat--body-start (point)))
        (mapc (lambda (f) (funcall f msg :muc-local :insert))
              (append jabber-muc-printers jabber-chat-printers)))
      (when (plist-get msg :edited)
        (insert (propertize " (edited)" 'face 'shadow)))
      (jabber-chat--insert-status-indicator msg)
      (jabber-chat--insert-reactions msg))
    (insert "\n")))

(defun jabber-chat-pp--muc-foreign (data)
  "Render a received MUC message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-muc-print-prompt msg nil /me-p)
    (if (plist-get msg :retracted)
        (jabber-chat--insert-tombstone msg)
      (jabber-chat--insert-reply-context msg)
      (let ((jabber-chat--body-start (point)))
        (mapc (lambda (f) (funcall f msg :muc-foreign :insert))
              (append jabber-muc-printers jabber-chat-printers)))
      (when (plist-get msg :edited)
        (insert (propertize " (edited)" 'face 'shadow)))
      (jabber-chat--insert-reactions msg))
    (insert "\n")))

(defun jabber-chat-pp--error (data)
  "Render an error message from DATA."
  (let* ((msg (cadr data))
         (timestamp (when (listp msg) (plist-get msg :timestamp))))
    (jabber-chat-system-prompt (or timestamp (current-time)))
    (if (stringp msg)
        (insert (propertize msg 'face 'jabber-chat-error) "\n")
      (jabber-chat-print-error msg))))

(defun jabber-chat-pp--muc-error (data)
  "Render a MUC error message from DATA."
  (let ((msg (cadr data)))
    (jabber-muc-system-prompt)
    (if (stringp msg)
        (insert (propertize msg 'face 'jabber-chat-error) "\n")
      (jabber-chat-print-error msg))))

(defun jabber-chat-pp--notice (data)
  "Render a system notice from DATA."
  (let* ((msg (cadr data))
         (timestamp (when (listp msg) (plist-get msg :timestamp))))
    (jabber-chat-system-prompt (or timestamp (current-time)))
    (insert msg "\n")))

(defun jabber-chat-pp--typing (data)
  "Render a typing indicator from DATA."
  (insert (propertize (cadr data) 'face 'shadow) "\n"))

(defun jabber-chat-pp--muc-notice (data)
  "Render a MUC presence notice from DATA.
Respects `jabber-muc-decorate-presence-patterns' for
highlight/hide behavior."
  (let* ((msg (cadr data))
         (match (jabber-chat-muc-presence-highlight msg))
         (face (cdr-safe match)))
    (cond
     ;; Matched with face: show prompt and body with that face
     (face
      (let ((prompt-start (point)))
        (jabber-muc-system-prompt)
        (put-text-property prompt-start (point) 'face face))
      (insert (propertize msg 'face face) "\n"))
     ;; Matched with no face: hide entirely
     (match)
     ;; No match: show normally
     (t
      (jabber-muc-system-prompt)
      (insert msg "\n")))))

(defun jabber-chat-pp--rare-time (data)
  "Insert rare-time separator from DATA.
When the previous visible node is also a :rare-time entry,
remove its text to suppress consecutive duplicates."
  (let* ((msg (cadr data))
         (node (jabber-chat-find-node data))
         (prev (jabber-find-previous-visible-node node)))
    (when (and prev (eq (car (ewoc-data prev)) :rare-time))
      (delete-region (marker-position (ewoc-location prev)) (point)))
    (insert (propertize (format-time-string jabber-rare-time-format msg)
                        'face 'jabber-rare-time-face)
            "\n")))

(defun jabber-chat-pp--subscription-request (data)
  "Render a subscription request from DATA."
  (let* ((msg (cadr data))
         (timestamp (when (listp msg) (plist-get msg :timestamp))))
    (jabber-chat-system-prompt (or timestamp (current-time)))
    (insert "This user requests subscription to your presence.\n")
    (when (and (stringp msg) (not (zerop (length msg))))
      (insert "Message: " msg "\n"))
    (insert "Accept?\n\n")
    (insert-button "Mutual" 'action 'jabber-subscription-accept-mutual)
    (insert "\t")
    (insert-button "One-way" 'action 'jabber-subscription-accept-one-way)
    (insert "\t")
    (insert-button "Decline" 'action 'jabber-subscription-decline)
    (insert "\n")))

(defconst jabber-chat-pp-dispatch
  '((:local                . jabber-chat-pp--local)
    (:foreign              . jabber-chat-pp--foreign)
    (:muc-local            . jabber-chat-pp--muc-local)
    (:muc-foreign          . jabber-chat-pp--muc-foreign)
    (:error                . jabber-chat-pp--error)
    (:muc-error            . jabber-chat-pp--muc-error)
    (:notice               . jabber-chat-pp--notice)
    (:muc-notice           . jabber-chat-pp--muc-notice)
    (:rare-time            . jabber-chat-pp--rare-time)
    (:subscription-request . jabber-chat-pp--subscription-request)
    (:typing               . jabber-chat-pp--typing))
  "Alist mapping message types to their render functions.")

(defun jabber-chat-pp (data)
  "Pretty-print a chat message DATA for EWOC display.
Dispatches to a type-specific render function via
`jabber-chat-pp-dispatch', then marks the region read-only."
  (let ((beg (point-marker))
        (type (car data)))
    (funcall (alist-get type jabber-chat-pp-dispatch) data)
    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)
    (set-marker beg nil)))

(defun jabber-rare-time-needed (time1 time2)
  "Return non-nil if a timestamp should be printed between TIME1 and TIME2."
  (not (string= (format-time-string jabber-rare-time-format time1)
		(format-time-string jabber-rare-time-format time2))))

(defun jabber-chat-entry-time (entry)
  "Return timestamp from EWOC node ENTRY.
Handles both msg-plist entries (with :timestamp) and legacy
string entries like :notice/:muc-notice (with :time in cddr)."
  (pcase (car entry)
    (:rare-time (cadr entry))
    (_ (if (stringp (cadr entry))
           (plist-get (cddr entry) :time)
         (plist-get (cadr entry) :timestamp)))))

(defun jabber-chat-find-node (data)
  "Find EWOC node whose data element equals DATA."
  (let* ((node (ewoc-locate jabber-chat-ewoc (point)))
         (node-time (jabber-chat-entry-time (ewoc-data node)))
         (data-time (jabber-chat-entry-time data))
         (node-iter (if (time-less-p data-time node-time)
                        #'ewoc-next
                      #'ewoc-prev)))
    (cl-macrolet ((search ()))
      (while (and
              node
              (not (equal data (ewoc-data node))))
        (setq node (funcall node-iter jabber-chat-ewoc node)))
      (search)
      ;; In the off chance we searched the wrong direction, switch
      ;; directions and re-search.
      (unless node
        (setq node (ewoc-locate jabber-chat-ewoc (point))
              node-iter (if (equal node-iter #'ewoc-prev)
                            #'ewoc-next
                          #'ewoc-prev))
        (search)))
    node))

(defun jabber-maybe-print-rare-time (node)
  "Print rare time before NODE, if appropriate.
NODE may be nil (e.g. when a duplicate was suppressed)."
  (when node
    (let* ((prev (ewoc-prev jabber-chat-ewoc node))
	   (data (ewoc-data node))
	   (prev-data (when prev (ewoc-data prev))))
      (cl-flet ((entry-time (entry)
		  (pcase (car entry)
		    (:rare-time (cadr entry))
		    (_ (plist-get (cadr entry) :timestamp)))))
        (when (and jabber-print-rare-time
		   (or (null prev)
		       (jabber-rare-time-needed (entry-time prev-data)
						(entry-time data))))
          ;; When jabber-parse-time supports fraction seconds (optional
          ;; with XEP-0082), jabber-chat-pp chokes on :rate-time ewoc
          ;; elements.  Ensure that the timestamp is in lisp form,
          ;; rather than (cons bignum . bignum).
	  (let ((buffer-undo-list t))
            (ewoc-enter-before jabber-chat-ewoc node
                               (list :rare-time (time-convert
                                                 (entry-time data)
                                                 'list)))))))))

(defun jabber-chat--format-time (timestamp delayed)
  "Format TIMESTAMP for prompt display.
Use short format normally, long format when DELAYED."
  (format-time-string (if delayed
                          jabber-chat-delayed-time-format
                        jabber-chat-time-format)
                      timestamp))

(defun jabber-chat--insert-prompt (timestamp nick face &optional plaintext-face encrypted)
  "Insert a chat prompt: TIMESTAMP <NICK> .
NICK gets FACE when ENCRYPTED, PLAINTEXT-FACE otherwise.
When ENCRYPTED, `jabber-chat-encrypted-indicator' is prepended."
  (when encrypted
    (insert jabber-chat-encrypted-indicator))
  (insert (propertize timestamp 'face 'shadow) " ")
  (when (> (length nick) 0)
    (insert (propertize (format "<%s> " nick)
                        'face (if encrypted face (or plaintext-face face))
                        'rear-nonsticky t))))

(defun jabber-chat-print-prompt (msg timestamp delayed dont-print-nick-p)
  "Print prompt for received message MSG.
TIMESTAMP overrides MSG's :timestamp when non-nil.
DELAYED selects the delayed-message face.
When DONT-PRINT-NICK-P is non-nil, omit the nickname."
  (let* ((from (plist-get msg :from))
         (timestamp (or timestamp (plist-get msg :timestamp)))
         (nick (if dont-print-nick-p ""
                 (if (jabber-muc-sender-p from)
                     (jabber-jid-resource from)
                   (jabber-jid-displayname from)))))
    (jabber-chat--insert-prompt
     (jabber-chat--format-time timestamp delayed)
     nick
     'jabber-chat-nick-foreign-encrypted
     'jabber-chat-nick-foreign-plaintext
     (plist-get msg :encrypted))))

(defun jabber-chat-system-prompt (timestamp)
  "Print system prompt at TIMESTAMP."
  (jabber-chat--insert-prompt
   (jabber-chat--format-time timestamp nil)
   ""
   'jabber-chat-nick-system))

(defun jabber-chat-self-prompt (msg timestamp delayed dont-print-nick-p)
  "Print prompt for sent message MSG.
TIMESTAMP overrides MSG's :timestamp when non-nil.
DELAYED selects the delayed-message face.
When DONT-PRINT-NICK-P is non-nil, omit the nickname."
  (let* ((state-data (fsm-get-state-data jabber-buffer-connection))
         (username (plist-get state-data :username)))
    (jabber-chat--insert-prompt
     (jabber-chat--format-time timestamp delayed)
     (if dont-print-nick-p "" username)
     'jabber-chat-nick-encrypted
     'jabber-chat-nick-plaintext
     (plist-get msg :encrypted))))

(defun jabber-chat-print-error (msg)
  "Print error from message plist MSG in a readable way."
  (let ((error-text (plist-get msg :error-text)))
    (insert
     (propertize
      (concat "Error: " (or error-text "Unknown error"))
      'face 'jabber-chat-error)
     "\n")))

(defun jabber-chat-print-subject (msg _who mode)
  "Print subject from message plist MSG, if any, in display MODE."
  (let ((subject (plist-get msg :subject)))
    (when (not (zerop (length subject)))
      (pcase mode
	(:printp
	 t)
	(:insert
	 (insert (propertize
		  "Subject: " 'face 'jabber-chat-nick-system)
		 (propertize
		  subject
		  'face 'jabber-chat-text-foreign)
		 "\n"))))))

(defun jabber-chat-print-body (msg who mode)
  "Dispatch MSG/WHO/MODE through `jabber-body-printers' until one succeeds."
  (run-hook-with-args-until-success 'jabber-body-printers msg who mode))

(defun jabber-chat-normal-body (msg who mode)
  "Print body from message plist MSG.
WHO and MODE follow the `jabber-body-printers' contract."
  (let ((body (plist-get msg :body)))
    (when body

      (when (eql mode :insert)
	(if (and (> (length body) 4)
		 (string= (substring body 0 4) "/me "))
	    (let ((action (substring body 4))
		  (nick (cond
			 ((eq who :local)
			  (plist-get (fsm-get-state-data jabber-buffer-connection) :username))
			 ((memq who '(:muc-local :muc-foreign))
			  (jabber-jid-resource (plist-get msg :from)))
			 (t
			  (jabber-jid-displayname (plist-get msg :from))))))
	      (insert (propertize
		       (concat nick
			       " "
			       action)
		       'face 'jabber-chat-nick-system)))
	  (let ((face (pcase who
			((or :foreign :muc-foreign) 'jabber-chat-text-foreign)
			((or :local :muc-local) 'jabber-chat-text-local))))
	    (insert (propertize body 'face face)))))
      t)))

(defun jabber-chat-print-url (msg _who mode)
  "Print OOB URLs from message plist MSG in display MODE.
Skip printing when the body already contains the URL to avoid
duplication (e.g. HTTP Upload messages)."
  (let ((entries (or (plist-get msg :oob-entries)
                     (when-let* ((url (plist-get msg :oob-url)))
                       (list (cons url (plist-get msg :oob-desc))))))
        (body (plist-get msg :body))
        (printed nil))
    (dolist (entry entries)
      (let ((url (car entry))
            (desc (cdr entry)))
        (when (and url (not (equal body url)))
          (when (eql mode :insert)
            (insert (format "\n%s%s<%s>"
                            (propertize
                             "URL: " 'face 'jabber-chat-nick-system)
                            (if (stringp desc) (concat desc " ") "")
                            url)))
          (setq printed t))))
    printed))

(defun jabber-chat--parse-aesgcm-url (url)
  "Parse an aesgcm:// URL into a plist.
Returns (:https-url URL :iv BYTES :key BYTES) or nil if URL is
not a valid aesgcm:// URL.  The fragment must be 88 hex characters
\(12-byte IV + 32-byte key) or 96 hex characters (16-byte IV +
32-byte key, used by some older clients)."
  (when (string-match
         "\\`aesgcm://\\([^#]*\\)#\\([[:xdigit:]]\\{88\\}\\|[[:xdigit:]]\\{96\\}\\)\\'"
         url)
    (let* ((path (match-string 1 url))
           (hex (match-string 2 url))
           (bytes (decode-hex-string hex))
           (key-len 32)
           (iv (substring bytes 0 (- (length bytes) key-len)))
           (key (substring bytes (- (length bytes) key-len))))
      (list :https-url (concat "https://" path)
            :iv iv
            :key key))))

(defun jabber-chat--fetch-aesgcm-image (url callback &rest cbargs)
  "Fetch and decrypt an aesgcm:// image URL.
Downloads via HTTPS, decrypts with AES-256-GCM, calls CALLBACK
with the created image (or nil) followed by CBARGS."
  (let ((parsed (jabber-chat--parse-aesgcm-url url)))
    (if (null parsed)
        (apply callback nil cbargs)
      (url-queue-retrieve
       (plist-get parsed :https-url)
       (lambda (status key iv cb args)
         (let ((url-buffer (current-buffer))
               (image
                (unless (plist-get status :error)
                  (set-buffer-multibyte nil)
                  (goto-char (point-min))
                  (when (re-search-forward "\r?\n\r?\n" nil t)
                    (let* ((encrypted (buffer-substring-no-properties
                                       (point) (point-max)))
                           (plaintext (condition-case nil
                                          (jabber-omemo-aesgcm-decrypt
                                           key iv encrypted)
					(error nil))))
                      (when plaintext
                        (let ((img (create-image plaintext nil t)))
                          (if (null img)
                              (progn
                                (message "aesgcm: failed to create image (%d bytes decrypted)"
                                         (length plaintext))
                                nil)
                            (setf (image-property img :max-width)
                                  jabber-image-max-width)
                            (setf (image-property img :max-height)
                                  jabber-image-max-height)
                            img))))))))
           (kill-buffer url-buffer)
           (apply cb image args)))
       (list (plist-get parsed :key) (plist-get parsed :iv)
             callback cbargs)
       'silent
       'inhibit-cookies))))

(defconst jabber-chat--image-extension-types
  '(("png"  . png)
    ("jpg"  . jpeg)
    ("jpeg" . jpeg)
    ("gif"  . gif)
    ("webp" . webp)
    ("svg"  . svg)
    ("avif" . avif)
    ("tiff" . tiff))
  "Alist mapping file extensions to Emacs image type symbols.")

(defun jabber-chat--supported-image-extensions ()
  "Return file extensions whose image types Emacs can render."
  (cl-loop for (ext . type) in jabber-chat--image-extension-types
           when (image-type-available-p type)
           collect ext))

(defun jabber-chat--image-ext-regexp ()
  "Return a regexp alternation matching supported image extensions."
  (regexp-opt (jabber-chat--supported-image-extensions) t))

(defun jabber-chat--image-url-p (url)
  "Return non-nil when URL has an image-like file extension."
  (string-match-p (concat "\\." (jabber-chat--image-ext-regexp)
                          "\\(?:[?#].*\\)?$")
                  (downcase url)))

(defvar jabber-chat-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jabber-chat-url-action-at-point)
    (define-key map "w" #'jabber-chat-copy-url)
    map)
  "Keymap active on inline images and downloadable URLs in chat buffers.")

(defvar jabber-chat--image-cache (make-hash-table :test 'equal)
  "Session-local cache mapping image URLs to Emacs image objects.")

(defvar-local jabber-chat--image-recenter-timer nil
  "Timer for recentering after inline image display changes.")

(defun jabber-chat--schedule-image-recenter ()
  "Schedule one recenter after newly displaying inline images."
  (when jabber-chat--image-recenter-timer
    (cancel-timer jabber-chat--image-recenter-timer))
  (let ((buffer (current-buffer)))
    (setq jabber-chat--image-recenter-timer
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq jabber-chat--image-recenter-timer nil)
                 (jabber-chat-buffer-recenter-input))))))))

(defun jabber-chat-copy-url ()
  "Copy the URL at point to the kill ring and display it."
  (interactive)
  (if-let* ((url (get-text-property (point) 'jabber-chat-image-url)))
      (progn (kill-new url) (message "%s" url))
    (user-error "No URL at point")))

(defcustom jabber-chat-download-directory nil
  "Default directory for file downloads.
When nil, use the last download directory from this session or
`default-directory'."
  :type '(choice (const :tag "Last used or default-directory" nil)
                 (directory :tag "Fixed directory")))

(defvar jabber-chat-last-download-directory nil
  "Last directory used for file downloads this session.")

(defun jabber-chat--download-default-directory ()
  "Return the default directory for file downloads."
  (or jabber-chat-download-directory
      jabber-chat-last-download-directory
      default-directory))

(defun jabber-chat--download-destination (url)
  "Prompt for a save path for URL, returning the chosen filename."
  (let* ((filename (file-name-nondirectory
                    (url-filename (url-generic-parse-url url)))))
    (read-file-name (format "Save %s to: " filename)
                    (jabber-chat--download-default-directory)
                    nil nil filename)))

(defun jabber-chat--record-download-directory (dest)
  "Remember the directory of DEST for future downloads."
  (setq jabber-chat-last-download-directory
        (file-name-directory dest)))

(defun jabber-chat-url-action-at-point ()
  "Download the file or image URL at point.
Handles aesgcm:// URLs by decrypting after download."
  (interactive)
  (let ((url (or (get-text-property (point) 'jabber-chat-file-url)
                 (get-text-property (point) 'jabber-chat-image-url))))
    (unless url
      (user-error "No downloadable URL at point"))
    (jabber-chat-download-url url)))

(defun jabber-chat-download-url (url)
  "Prompt to download URL to a local file.
For aesgcm:// URLs, fetches via HTTPS and decrypts with AES-256-GCM."
  (let* ((parsed (and (string-prefix-p "aesgcm://" url)
                      (jabber-chat--parse-aesgcm-url url)))
         (fetch-url (if parsed (plist-get parsed :https-url) url))
         (dest (jabber-chat--download-destination fetch-url)))
    (jabber-chat--record-download-directory dest)
    (if parsed
        (jabber-chat--download-aesgcm fetch-url dest
                                      (plist-get parsed :key)
                                      (plist-get parsed :iv))
      (url-copy-file fetch-url dest t)
      (message "Downloaded %s" dest))))

(defun jabber-chat--download-aesgcm (url dest key iv)
  "Fetch URL, decrypt with KEY and IV, write to DEST."
  (url-queue-retrieve
   url
   (lambda (status dest-file key iv)
     (let ((url-buffer (current-buffer)))
       (if (plist-get status :error)
           (progn
             (kill-buffer url-buffer)
             (message "Download failed: %s"
                      (plist-get status :error)))
         (set-buffer-multibyte nil)
         (goto-char (point-min))
         (re-search-forward "\r?\n\r?\n" nil t)
         (let* ((encrypted (buffer-substring-no-properties
                            (point) (point-max)))
                (plaintext (condition-case err
                               (jabber-omemo-aesgcm-decrypt
                                key iv encrypted)
                             (error
                              (message "Decryption failed: %s"
                                       (error-message-string err))
                              nil))))
           (kill-buffer url-buffer)
           (when plaintext
             (with-temp-file dest-file
               (set-buffer-multibyte nil)
               (insert plaintext))
             (message "Downloaded and decrypted %s" dest-file))))))
   (list dest key iv)
   'silent
   'inhibit-cookies))

(defun jabber-chat--apply-image-display (image beg end url)
  "Display IMAGE over URL text from BEG to END.
Preserve the underlying URL text so refresh/redraw can redisplay it."
  (add-text-properties
   beg end
   (list 'display image
         'jabber-chat-image-url url
         'keymap jabber-chat-url-keymap
         'jabber-chat-image-fetching nil
         'rear-nonsticky t))
  (jabber-chat--schedule-image-recenter)
  image)

(defun jabber-chat--cache-image (url image)
  "Cache IMAGE for URL and return IMAGE."
  (puthash url image jabber-chat--image-cache)
  image)

(defun jabber-chat--restore-cached-image (url beg end)
  "Apply cached image for URL to text from BEG to END.
Return non-nil when a cached image was applied."
  (when-let* ((image (gethash url jabber-chat--image-cache)))
    (jabber-chat--apply-image-display image beg end url)))

(defun jabber-chat--image-displayed-p (beg url)
  "Return non-nil when BEG already displays URL image."
  (and (get-text-property beg 'display)
       (equal (get-text-property beg 'jabber-chat-image-url) url)))

(defun jabber-chat--image-fetching-p (beg url)
  "Return non-nil when BEG is already marked as fetching URL."
  (equal (get-text-property beg 'jabber-chat-image-fetching) url))

(defun jabber-chat--mark-image-fetching (beg end url)
  "Mark URL text from BEG to END as an image fetch attempt."
  (add-text-properties
   beg end
   (list 'jabber-chat-image-url url
         'jabber-chat-image-fetching url
         'keymap jabber-chat-url-keymap
         'rear-nonsticky t)))

(defun jabber-chat--replace-url-with-image (image url beg end buffer)
  "Display fetched IMAGE over URL text between markers BEG and END.
Cache IMAGE by URL.  Leave URL text visible when IMAGE is nil,
BUFFER is dead, or the marker range no longer contains URL."
  (when image
    (jabber-chat--cache-image url image)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (markerp beg) (markerp end)
                   (eq (marker-buffer beg) buffer)
                   (eq (marker-buffer end) buffer)
                   (marker-position beg) (marker-position end)
                   (<= beg end)
                   (equal (buffer-substring-no-properties beg end) url))
          (let ((inhibit-read-only t))
            (jabber-chat--apply-image-display image beg end url)))))))

(defvar-local jabber-chat--image-scan-timer nil
  "Idle timer for scanning image URLs in this buffer.")

(defun jabber-chat--schedule-image-scan (_msg _who mode)
  "Schedule an async image scan after message insertion (MODE = :insert).
Added to `jabber-chat-printers' to trigger after each message."
  (when (eql mode :insert)
    (let ((buf (current-buffer)))
      (when jabber-chat--image-scan-timer
        (cancel-timer jabber-chat--image-scan-timer))
      (setq jabber-chat--image-scan-timer
            (run-with-idle-timer
             0.3 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (jabber-chat-display-buffer-images)))))))))

(defconst jabber-chat--image-url-re
  (concat "\\(?:https?\\|aesgcm\\)://[^ \t\n<>\"]+"
          "\\." (jabber-chat--image-ext-regexp)
          "\\(?:[?#][^ \t\n<>\"]*\\)?")
  "Regexp matching HTTP(S) and aesgcm:// image URLs.")

(defun jabber-chat-display-buffer-images ()
  "Scan buffer for image URLs and display them inline.
Preserve URL text and restore cached images without refetching."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (limit (and (markerp jabber-point-insert) jabber-point-insert)))
      (when (and jabber-chat-display-images (display-graphic-p))
        (goto-char (point-min))
        (while (re-search-forward jabber-chat--image-url-re limit t)
          (let ((url (match-string-no-properties 0))
                (url-beg (match-beginning 0))
                (url-end (match-end 0)))
            ;; Insert newline before URL so images appear on their
            ;; own line.
            (when (and (> url-beg (point-min))
                       (not (eq (char-before url-beg) ?\n)))
              (save-excursion
                (goto-char url-beg)
                (insert "\n"))
              (setq url-beg (1+ url-beg)
                    url-end (1+ url-end)))
            (unless (jabber-chat--image-displayed-p url-beg url)
              (unless (jabber-chat--restore-cached-image url url-beg url-end)
                (unless (jabber-chat--image-fetching-p url-beg url)
                  (jabber-chat--mark-image-fetching url-beg url-end url)
                  (let ((beg (copy-marker url-beg))
                        (end (copy-marker url-end))
                        (buf (current-buffer)))
                    (if (string-prefix-p "aesgcm://" url)
                        (jabber-chat--fetch-aesgcm-image
                         url
                         #'jabber-chat--replace-url-with-image
                         url beg end buf)
                      (jabber-image-fetch
                       url
                       #'jabber-chat--replace-url-with-image
                       url beg end buf))))))))))))

(defun jabber-chat-goto-address (_msg _who mode)
  "Call function `goto-address' on the newly written text (MODE = :insert)."
  (when (eq mode :insert)
    (condition-case err
        (let ((end (point))
              (start (or jabber-chat--body-start (point-min))))
          (when (< start end)
            (goto-address-fontify start end)
            ;; Clip overlays that leaked past end.  During ewoc
            ;; invalidation the body text abuts the next node with no
            ;; separator, so bounds-of-thing-at-point can extend into
            ;; the next message.
            (dolist (ov (overlays-in start end))
              (when (and (overlay-get ov 'goto-address)
                         (> (overlay-end ov) end))
                (move-overlay ov (overlay-start ov) end)))))
      (error (message "jabber-chat: goto-address-fontify failed: %s" err)))))

(defun jabber-chat-mark-oob-attachment (msg _who mode)
  "Mark non-image OOB attachment URLs in MSG (MODE = :insert) for download.
Runs after `jabber-chat-goto-address' so the `goto-address' overlay
exists when we set our keymap as its parent."
  (when (eql mode :insert)
    (let ((entries (or (plist-get msg :oob-entries)
                       (when-let* ((url (plist-get msg :oob-url)))
                         (list (cons url nil))))))
      (dolist (entry entries)
        (let ((oob-url (car entry)))
          (when (and oob-url (not (jabber-chat--image-url-p oob-url)))
            (save-excursion
              (when (search-backward oob-url nil t)
                (let ((beg (match-beginning 0))
                      (end (match-end 0))
                      (inhibit-read-only t))
                  (put-text-property beg end 'jabber-chat-file-url oob-url)
                  (let ((ov (seq-find (lambda (o) (overlay-get o 'keymap))
                                      (overlays-in beg end))))
                    (if ov
                        (set-keymap-parent (overlay-get ov 'keymap)
                                           jabber-chat-url-keymap)
                      (put-text-property beg end 'keymap
                                         jabber-chat-url-keymap))))))))))))

(defconst jabber-chat--aesgcm-url-re
  "aesgcm://[^ \t\n<>\"#]+#\\(?:[[:xdigit:]]\\{88\\}\\|[[:xdigit:]]\\{96\\}\\)\\b"
  "Regexp matching aesgcm:// URLs with 88 or 96-hex-char fragment.")

(defun jabber-chat-mark-aesgcm-url (_msg _who mode)
  "Mark non-image aesgcm:// URLs with download keymap and link face.
Runs only when MODE is :insert.
Skips URLs already handled by the image scanner."
  (when (eql mode :insert)
    (save-excursion
      (let ((end (point))
            (limit (max (- (point) 1000) (1+ (point-min))))
            (inhibit-read-only t))
        (goto-char limit)
        (while (re-search-forward jabber-chat--aesgcm-url-re end t)
          (let ((beg (match-beginning 0))
                (url-end (match-end 0))
                (url (match-string-no-properties 0)))
            (unless (or (get-text-property beg 'jabber-chat-file-url)
                        (jabber-chat--image-url-p url))
              (put-text-property beg url-end 'jabber-chat-file-url url)
              (put-text-property beg url-end 'keymap jabber-chat-url-keymap)
              (add-face-text-property beg url-end 'link t))))))))

;; jabber-compose is autoloaded in jabber.el
(defun jabber-send-message (jc to subject body type)
  "Send a message stanza to TO with SUBJECT, BODY and TYPE.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "subject: ")
		     (jabber-read-with-input-method "body: ")
		     (read-string "type: ")))
  (jabber-send-sexp jc
		    `(message ((to . ,to)
                               ,(if (> (length type) 0)
                                    `(type . ,type)))
                              ,(if (> (length subject) 0)
                                   `(subject () ,subject))
                              ,(if (> (length body) 0)
                                   `(body () ,body))))
  (jabber-db--store-outgoing jc to body type))

(defun jabber-chat-with (jc jid &optional other-window)
  "Open an empty chat window for chatting with JID.
With a prefix argument, open buffer in other window.
Returns the chat buffer.
JC is the Jabber connection."
  (interactive (let* ((jid
		       (jabber-read-jid-completing "chat with:"))
		      (account
		       (jabber-read-account nil jid)))
		 (list
		  account jid current-prefix-arg)))
  (let ((buffer (jabber-chat-create-buffer jc jid)))
    (if other-window
	(switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun jabber-chat-with-jid-at-point (&optional other-window)
  "Start chat with JID at point.
Signal an error if there is no JID at point.
With a prefix argument OTHER-WINDOW, open buffer in other window."
  (interactive "P")
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid))
	(account (get-text-property (point)
				    'jabber-account)))
    (if (and jid-at-point account)
	(jabber-chat-with account jid-at-point other-window)
      (error "No contact at point"))))

(provide 'jabber-chat)
;;; jabber-chat.el ends here
