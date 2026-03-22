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

;;; Code:

(require 'jabber-core)
(require 'jabber-alert)
(require 'jabber-chatbuffer)
(require 'ewoc)
(require 'goto-addr)
(require 'url-parse)
(require 'url-queue)
(require 'hex-util)

(eval-when-compile (require 'cl-lib))

(defgroup jabber-chat nil "chat display options"
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
    (:eval jabber-chatstates-message)		;see jabber-chatstates.el
    (:eval jabber-chat-receipt-message))	;see jabber-receipts.el
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
  :type 'boolean
  :group 'jabber-chat)

(defface jabber-rare-time-face
  '((t :inherit font-lock-comment-face :underline t))
  "Face for displaying rare time information."
  :group 'jabber-chat)

(defcustom jabber-chat-encrypted-indicator (propertize "[E]" 'face 'shadow)
  "String prepended to the timestamp of encrypted messages."
  :type 'string
  :group 'jabber-chat)

(defface jabber-chat-nick-encrypted
  '((t :inherit font-lock-constant-face))
  "Face for own nick on encrypted messages."
  :group 'jabber-chat)

(defface jabber-chat-nick-foreign-encrypted
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for foreign nick on encrypted messages."
  :group 'jabber-chat)

(defface jabber-chat-nick-plaintext
  '((t :inherit font-lock-warning-face :slant italic))
  "Face for own nick on plaintext messages."
  :group 'jabber-chat)

(defface jabber-chat-nick-foreign-plaintext
  '((t :inherit font-lock-keyword-face :slant italic))
  "Face for foreign nick on plaintext messages."
  :group 'jabber-chat)

(defface jabber-chat-nick-system
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for system and special messages."
  :group 'jabber-chat)

(defface jabber-chat-text-local '((t ()))
  "Face used for text you write."
  :group 'jabber-chat)

(defface jabber-chat-text-foreign '((t ()))
  "Face used for text others write."
  :group 'jabber-chat)

(defface jabber-chat-error
  '((t :inherit error))
  "Face used for error messages."
  :group 'jabber-chat)

;;;###autoload
(defvar jabber-chatting-with nil
  "JID of the person you are chatting with.")

(defvar jabber-chat-printers '(jabber-chat-print-subject
			 jabber-chat-print-body
			 jabber-chat-print-url
			 jabber-chat-goto-address
			 jabber-chat-mark-oob-attachment
			 jabber-chat--schedule-image-scan)
  "List of functions that may be able to print part of a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local or :foreign, for sent or received stanza, respectively
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.")

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

;; Global reference declarations

(declare-function jabber-compose "jabber-compose.el" (jc &optional recipient))
(declare-function jabber-omemo--send-chat "jabber-omemo" (jc body))
(declare-function jabber-openpgp--send-chat "jabber-openpgp" (jc body))
(declare-function jabber-openpgp-legacy--send-chat "jabber-openpgp-legacy" (jc body))
(declare-function jabber-muc-private-create-buffer "jabber-muc.el"
                  (jc group nickname))
(declare-function jabber-muc-print-prompt "jabber-muc.el"
                  (msg &optional local dont-print-nick-p))
(declare-function jabber-muc-private-print-prompt "jabber-muc.el" (msg))
(declare-function jabber-muc-system-prompt "jabber-muc.el" (&rest _ignore))
(declare-function jabber-muc-message-p "jabber-muc.el"(message))
(declare-function jabber-muc-sender-p "jabber-muc.el" (jid))
(declare-function jabber-muc-private-message-p "jabber-muc.el" (message))
(declare-function jabber-muc-nickname "jabber-muc.el" (group))
(defvar jabber-muc-xmlns-user)
(declare-function jabber-image-fetch "jabber-image" (url callback &rest cbargs))
(declare-function jabber-omemo-aesgcm-decrypt "jabber-omemo"
                  (key iv ciphertext-with-tag))
(defvar jabber-backlog-days)
(defvar jabber-backlog-number)
(declare-function jabber-db-backlog "jabber-db.el"
                  (account peer &optional count start-time))
(declare-function jabber-db--store-outgoing "jabber-db.el"
                  (jc to body type))
(declare-function jabber-db-store-message "jabber-db.el"
                  (account peer direction type body timestamp
                           &optional resource stanza-id
                           server-id raw-xml oob-url oob-desc
                           encrypted))
(declare-function jabber-mam-fetch-peer-history "jabber-mam"
                  (jc peer &optional muc-p callback))
(declare-function jabber-message-correct--replace-id "jabber-message-correct"
                  (xml-data))
(declare-function jabber-message-correct--apply "jabber-message-correct"
                  (replace-id new-body new-from muc-p buffer))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-muc-printers)            ; jabber-muc.el
(declare-function jabber-mam-syncing-p "jabber-mam" ())
(defvar jabber-mam--dirty-buffers)      ; jabber-mam.el
(defvar jabber-oob-xmlns)              ; jabber-xml.el
(defvar jabber-carbons-xmlns)          ; jabber-carbons.el
(defvar jabber-image-max-width)        ; jabber-image.el
(defvar jabber-image-max-height)       ; jabber-image.el

;;

(defvar jabber-chat-earliest-backlog nil
  "Float-time of earliest backlog entry inserted into buffer.
nil if no backlog has been inserted.")

(defvar jaber-chat-much-presence-patterns-history nil
  "History values selected for `jabber-muc-decorate-presence-patterns'")

(defface jabber-muc-presence-dim
  '((t :inherit shadow :slant italic))
  "Face for diminished presence notifications."
  :group 'jabber-chat)

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
associated face.  Ignore notification if face is `nil'."
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

      (make-local-variable 'jabber-chatting-with)
      (setq jabber-chatting-with chat-with)
      (jabber-chatbuffer--registry-put 'chat (jabber-jid-user chat-with))

      (jabber-chat-mode-setup jc #'jabber-chat-pp)
      (setq jabber-send-function #'jabber-chat-send)
      (setq header-line-format jabber-chat-header-line-format)

      (make-local-variable 'jabber-chat-earliest-backlog)

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
	    (jabber-chat--insert-backlog-chunked
	     (current-buffer) backlog-entries
	     #'jabber-chat-display-buffer-images))))

      (when-let* ((win (get-buffer-window (current-buffer))))
        (with-selected-window win
          (goto-char jabber-point-insert)
          (recenter -1))))

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
		     (let* ((nick (jabber-jid-resource (plist-get msg-plist :from)))
			    (my-nick (jabber-muc-nickname jabber-group)))
		       (if (and nick my-nick (string= nick my-nick))
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

(defun jabber-chat--insert-backlog-chunked (buffer entries callback)
  "Insert ENTRIES into BUFFER's ewoc in chunks to avoid blocking.
Inserts `jabber-chat-backlog-chunk-size' entries per timer tick.
Call CALLBACK with no arguments when all entries are inserted."
  (if (or (null entries) (not (buffer-live-p buffer)))
      (when callback
	(when (buffer-live-p buffer)
	  (with-current-buffer buffer
	    (funcall callback))))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
	     (chunk (cl-subseq entries 0
			       (min jabber-chat-backlog-chunk-size
				    (length entries))))
	     (rest (nthcdr (length chunk) entries)))
	(mapc #'jabber-chat-insert-backlog-entry chunk)
	(if rest
	    (run-with-timer 0.1 nil
			    #'jabber-chat--insert-backlog-chunked
			    buffer rest callback)
	  (when callback (funcall callback)))))))

(defun jabber-chat-display-more-backlog (how-many)
  "Display more context by fetching history from the server.
Queries MAM for the peer's full archive, stores results in the
database, then recreates the buffer.  MAM fetches in chunks so
the UI stays responsive.
HOW-MANY is the number of additional messages to show.
When nil or 0, display all messages."
  (interactive
   (let ((input (read-string "How many more messages (empty for all)? ")))
     (list (if (string-empty-p input) nil
	     (string-to-number input)))))
  (when (jabber-mam-syncing-p)
    (user-error "MAM sync already in progress"))
  (let* ((jc jabber-buffer-connection)
	 (group (bound-and-true-p jabber-group))
	 (chat-with (bound-and-true-p jabber-chatting-with))
	 (peer (or group chat-with))
	 (current-count (length (ewoc-collect
				jabber-chat-ewoc
				(lambda (data) (not (eq (car data) :rare-time))))))
	 (target-count (if (or (null how-many) (zerop how-many)) t
			 (+ current-count how-many))))
    (message "Fetching history for %s..." peer)
    (jabber-mam-fetch-peer-history
     jc peer group
     (lambda ()
       (let ((jabber-backlog-days nil)
	     (jabber-backlog-number target-count))
	 (when-let* ((buf (if group
			      (jabber-muc-find-buffer peer)
			    (jabber-chat-find-buffer peer))))
	   (kill-buffer buf))
	 (switch-to-buffer
	  (if group
	      (jabber-muc-create-buffer jc peer)
	    (jabber-chat-create-buffer jc chat-with)))
	 (message "History loaded for %s" peer))))))

(add-to-list 'jabber-message-chain #'jabber-process-chat)

(defun jabber-get-forwarded-message (xml-data)
  "Extract the inner message from a carbon-forwarded stanza.
Returns the inner message element, or nil if XML-DATA is not a carbon."
  (cdr (jabber-chat--extract-carbon xml-data)))

(defun jabber-chat--extract-carbon (xml-data)
  "Extract carbon type and inner message from XML-DATA.
Returns (TYPE . MESSAGE) where TYPE is `sent' or `received',
or nil if XML-DATA is not a carbon."
  (let ((wrapper (or (car (jabber-xml-get-children xml-data 'sent))
                     (car (jabber-xml-get-children xml-data 'received)))))
    (when wrapper
      (let* ((type (jabber-xml-node-name wrapper))
             (fwd (car (jabber-xml-get-children wrapper 'forwarded)))
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
              (warn "jabber: dropping forged carbon from %s" outer-from)
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

(defun jabber-chat--store-carbon (jc xml-data)
  "Store a carbon-forwarded message in the database.
JC is the Jabber connection.  XML-DATA is the inner (unwrapped,
possibly decrypted) message stanza.
Direction is determined by comparing the sender to our bare JID."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (to (jabber-xml-get-attribute xml-data 'to))
         (body (car (jabber-xml-node-children
                     (car (jabber-xml-get-children xml-data 'body)))))
         (stanza-id (jabber-xml-get-attribute xml-data 'id))
         (timestamp (jabber-message-timestamp xml-data))
         (our-jid (jabber-connection-bare-jid jc))
         (sent-p (string= (jabber-jid-user from) our-jid))
         (direction (if sent-p "out" "in"))
         (peer (jabber-jid-user (if sent-p to from)))
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
       nil nil nil nil
       encrypted))))

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
  "Call the :decrypt function from HANDLER-PROPS with error handling.
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

(defun jabber-chat--display-message (_jc _xml-data chat-buffer
                                    error-p from msg-plist)
  "Display an incoming message and run alert hooks.
Insert an EWOC entry into CHAT-BUFFER for the message described by
MSG-PLIST, then run `jabber-message-hooks' and
`jabber-alert-message-hooks'.  ERROR-P is non-nil when the stanza
contains an error element.  FROM is the sender JID.
_JC and _XML-DATA are reserved for future use by OMEMO."
  (let ((body-text (plist-get msg-plist :body)))
    (with-current-buffer chat-buffer
      (jabber-maybe-print-rare-time
       (jabber-chat-ewoc-enter
        (list (if error-p :error :foreign) msg-plist)))
      (let ((inhibit-message (jabber-mam-syncing-p)))
        (dolist (hook '(jabber-message-hooks jabber-alert-message-hooks))
          (run-hook-with-args hook
                              from (current-buffer) body-text
                              (funcall jabber-alert-message-function
                                       from (current-buffer) body-text)))))))

(defun jabber-process-chat (jc xml-data)
  "If XML-DATA is a one-to-one chat message, handle it as such.
JC is the Jabber connection."
  (when (not (jabber-muc-message-p xml-data))
    (let* ((unwrapped (jabber-chat--unwrap-carbon jc xml-data))
           (is-carbon (not (eq xml-data (car unwrapped))))
           (xml-data (jabber-chat--decrypt-if-needed jc (car unwrapped)))
           (carbon-buffer (cdr unwrapped))
           (from (jabber-xml-get-attribute xml-data 'from))
           (error-p (jabber-xml-get-children xml-data 'error))
           (msg-plist (jabber-chat--msg-plist-from-stanza xml-data)))
      (when is-carbon
        (jabber-chat--store-carbon jc xml-data))
      (let ((replace-id (jabber-message-correct--replace-id xml-data)))
        (if (and replace-id (not (jabber-mam-syncing-p)))
            (jabber-message-correct--apply
             replace-id
             (plist-get msg-plist :body)
             from
             nil
             (jabber-chat-find-buffer from))
          (when (or error-p
                    (run-hook-with-args-until-success 'jabber-chat-printers
                                                      msg-plist :foreign :printp))
            (jabber-chat--display-message
             jc xml-data
             (jabber-chat--select-buffer jc from carbon-buffer)
             error-p from msg-plist)))))))

(defun jabber-chat-send (jc body &optional extra-elements)
  "Send BODY through connection JC, and display it in chat buffer.
JC is the Jabber connection.
EXTRA-ELEMENTS, when non-nil, is a list of XML sexp elements to
splice into the stanza after the body (e.g. OOB, hints)."
  (pcase jabber-chat-encryption
    ('omemo (jabber-omemo--send-chat jc body))
    ('openpgp (require 'jabber-openpgp)
              (jabber-openpgp--send-chat jc body))
    ('openpgp-legacy (require 'jabber-openpgp-legacy)
                     (jabber-openpgp-legacy--send-chat jc body))
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
       (dolist (hook jabber-chat-send-hooks)
	 (if (eq hook t)
	     ;; Local hook referring to global...
	     (when (local-variable-p 'jabber-chat-send-hooks)
	       (dolist (global-hook (default-value 'jabber-chat-send-hooks))
		 (nconc stanza-to-send (funcall global-hook body id))))
	   (nconc stanza-to-send (funcall hook body id))))
       ;; ...display it, if it would be displayed.
       (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza-to-send)))
         (plist-put msg-plist :status :sent)
	 (when (run-hook-with-args-until-success 'jabber-chat-printers msg-plist :local :printp)
           (jabber-maybe-print-rare-time
            (jabber-chat-ewoc-enter (list :local msg-plist)))))
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
                          'jaber-chat-much-presence-patterns-history)
                         jabber-muc-decorate-presence-patterns-alist))))
    (unless (equal patterns jabber-muc-decorate-presence-patterns)
      (set (if global
               'jabber-muc-decorate-presence-patterns
             (make-local-variable 'jabber-muc-decorate-presence-patterns))
           patterns)
      (jabber-chat-redisplay))))

(defun jabber-chat-muc-presence-highlight (message)
  "Return non-`nil' to control MUC presence notification display.
This matches :muc-notification message text with the list
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

(defun jabber-chat--has-muc-invite-p (xml-data)
  "Return non-nil if XML-DATA contains a MUC invitation."
  (let ((muc-x (jabber-xml-child-with-xmlns
                xml-data jabber-muc-xmlns-user)))
    (and muc-x (jabber-xml-get-children muc-x 'invite))))

(defun jabber-chat--build-msg-plist (xml-data delayed)
  "Build a message plist from the fields in XML-DATA.
DELAYED marks the message as delayed unconditionally."
  (let* ((msg-timestamp (jabber-message-timestamp xml-data))
         (oob-x (jabber-xml-child-with-xmlns xml-data jabber-oob-xmlns))
         (error-node (car (jabber-xml-get-children xml-data 'error)))
         (sid-el (jabber-xml-child-with-xmlns xml-data "urn:xmpp:sid:0")))
    (list
     :id (jabber-xml-get-attribute xml-data 'id)
     :server-id (when sid-el (jabber-xml-get-attribute sid-el 'id))
     :from (jabber-xml-get-attribute xml-data 'from)
     :body (car (jabber-xml-node-children
                 (car (jabber-xml-get-children xml-data 'body))))
     :subject (car (jabber-xml-node-children
                    (car (jabber-xml-get-children xml-data 'subject))))
     :timestamp (or msg-timestamp (current-time))
     :delayed (or delayed (and msg-timestamp t))
     :encrypted (and (jabber-xml-child-with-xmlns
                      xml-data "eu.siacs.conversations.axolotl")
                     t)
     :oob-url (jabber-chat--oob-field oob-x 'url)
     :oob-desc (jabber-chat--oob-field oob-x 'desc)
     :error-text (when error-node
                   (jabber-parse-error error-node)))))

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

(defun jabber-chat-pp--local (data)
  "Render a locally sent message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-chat-self-prompt msg (plist-get msg :timestamp)
                             (plist-get msg :delayed) /me-p)
    (run-hook-with-args 'jabber-chat-printers msg :local :insert)
    (when (plist-get msg :edited)
      (insert (propertize " (edited)" 'face 'shadow)))
    (jabber-chat--insert-status-indicator msg)
    (insert "\n")))

(defun jabber-chat-pp--foreign (data)
  "Render a received message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-chat-print-prompt msg (plist-get msg :timestamp)
                              (plist-get msg :delayed) /me-p)
    (run-hook-with-args 'jabber-chat-printers msg :foreign :insert)
    (when (plist-get msg :edited)
      (insert (propertize " (edited)" 'face 'shadow)))
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
      (mapc (lambda (f) (funcall f msg :muc-local :insert))
            (append jabber-muc-printers jabber-chat-printers))
      (when (plist-get msg :edited)
        (insert (propertize " (edited)" 'face 'shadow)))
      (jabber-chat--insert-status-indicator msg))
    (insert "\n")))

(defun jabber-chat-pp--muc-foreign (data)
  "Render a received MUC message from DATA."
  (let* ((msg (cadr data))
         (body (plist-get msg :body))
         (/me-p (and (stringp body) (string-prefix-p "/me " body))))
    (jabber-muc-print-prompt msg nil /me-p)
    (if (plist-get msg :retracted)
        (jabber-chat--insert-tombstone msg)
      (mapc (lambda (f) (funcall f msg :muc-foreign :insert))
            (append jabber-muc-printers jabber-chat-printers))
      (when (plist-get msg :edited)
        (insert (propertize " (edited)" 'face 'shadow))))
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
    (:subscription-request . jabber-chat-pp--subscription-request))
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
  "Print rare time before NODE, if appropriate."
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
	(ewoc-enter-before jabber-chat-ewoc node
			   (list :rare-time (time-convert
                                             (entry-time data)
                                             'list)))))))

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
  "Print prompt for received message MSG."
  (let* ((from (plist-get msg :from))
         (timestamp (or timestamp (plist-get msg :timestamp)))
         (nick (if dont-print-nick-p "" (jabber-jid-displayname from))))
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
  "Print prompt for sent message MSG."
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
  "Print subject from message plist MSG, if any."
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
  (run-hook-with-args-until-success 'jabber-body-printers msg who mode))

(defun jabber-chat-normal-body (msg who mode)
  "Print body from message plist MSG."
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
	  (insert (propertize
		   body
		   'face (pcase who
			   ((or :foreign :muc-foreign) 'jabber-chat-text-foreign)
			   ((or :local :muc-local) 'jabber-chat-text-local))))))
      t)))

(defun jabber-chat-print-url (msg _who mode)
  "Print OOB URL from message plist MSG.
Skips printing when the body already contains the URL to avoid
duplication (e.g. HTTP Upload messages)."
  (let ((url (plist-get msg :oob-url))
        (body (plist-get msg :body)))
    (when (and url (not (equal body url)))
      (when (eql mode :insert)
        (let ((desc (plist-get msg :oob-desc)))
          (insert (format "\n%s%s<%s>"
                          (propertize
                           "URL: " 'face 'jabber-chat-nick-system)
                          (if (stringp desc) (concat desc " ") "")
                          url))))
      t)))

(defun jabber-chat--parse-aesgcm-url (url)
  "Parse an aesgcm:// URL into a plist.
Returns (:https-url URL :iv BYTES :key BYTES) or nil if URL is
not a valid aesgcm:// URL.  The fragment must be 88 hex characters
\(12-byte IV + 32-byte key)."
  (when (string-match
         "\\`aesgcm://\\([^#]*\\)#\\([[:xdigit:]]\\{88\\}\\)\\'"
         url)
    (let* ((path (match-string 1 url))
           (hex (match-string 2 url))
           (bytes (decode-hex-string hex))
           (iv (substring bytes 0 12))
           (key (substring bytes 12 44)))
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
                              (message "aesgcm: failed to create image (%d bytes decrypted)"
                                       (length plaintext))
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

(defun jabber-chat--image-url-p (url)
  "Return non-nil if URL looks like an image based on file extension."
  (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\)\\(?:[?#].*\\)?$"
                  (downcase url)))

(defvar jabber-chat-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jabber-chat-url-action-at-point)
    map)
  "Keymap active on inline images and downloadable URLs in chat buffers.")

(defun jabber-chat-url-action-at-point ()
  "Act on the URL at point: download files, open images in EWW.
Handles aesgcm:// URLs by decrypting after download."
  (interactive)
  (let ((file-url (get-text-property (point) 'jabber-chat-file-url))
        (image-url (get-text-property (point) 'jabber-chat-image-url)))
    (cond
     (file-url (jabber-chat-download-url file-url))
     (image-url (jabber-chat-download-url image-url)))))

(defun jabber-chat-download-url (url)
  "Prompt to download URL to a local file.
For aesgcm:// URLs, fetches via HTTPS and decrypts with AES-256-GCM."
  (let* ((parsed (and (string-prefix-p "aesgcm://" url)
                      (jabber-chat--parse-aesgcm-url url)))
         (fetch-url (if parsed (plist-get parsed :https-url) url))
         (filename (file-name-nondirectory
                    (url-filename (url-generic-parse-url fetch-url))))
         (dest (read-file-name (format "Save %s to: " filename)
                               nil nil nil filename)))
    (if (null parsed)
        (progn
          (url-copy-file url dest t)
          (message "Downloaded %s" dest))
      (url-queue-retrieve
       fetch-url
       (lambda (status dest-file key iv)
         (let ((url-buffer (current-buffer)))
           (if (plist-get status :error)
               (progn
                 (kill-buffer url-buffer)
                 (message "Download failed: %s"
                          (plist-get status :error)))
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
       (list dest (plist-get parsed :key) (plist-get parsed :iv))
       'silent
       'inhibit-cookies))))

(defun jabber-chat--replace-url-with-image (image beg end buffer)
  "Delete URL text between markers BEG and END, insert IMAGE.
Uses `insert-image' so the URL serves as alt-text."
  (when (and image (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (url (buffer-substring-no-properties beg end)))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (let ((start (point)))
            (insert-image image url)
            (put-text-property start (point) 'jabber-chat-image-url url)
            (put-text-property start (point) 'keymap jabber-chat-url-keymap)))))))

(defvar-local jabber-chat--image-scan-timer nil
  "Idle timer for scanning image URLs in this buffer.")

(defun jabber-chat--schedule-image-scan (_msg _who mode)
  "Schedule an async image scan after message insertion.
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
  "\\(?:https?\\|aesgcm\\)://[^ \t\n<>\"]+\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\)\\(?:[?#][^ \t\n<>\"]*\\)?"
  "Regexp matching HTTP(S) and aesgcm:// image URLs.")

(defun jabber-chat-display-buffer-images ()
  "Scan buffer for image URLs and replace them with inline images.
Skips URLs already processed (marked with `jabber-chat-image-url').
When the image arrives the URL text is deleted and the image inserted."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (limit (and (markerp jabber-point-insert) jabber-point-insert)))
      (when (and jabber-chat-display-images (display-graphic-p))
        (goto-char (point-min))
        (while (re-search-forward jabber-chat--image-url-re limit t)
          (unless (get-text-property (match-beginning 0) 'jabber-chat-image-url)
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
              (put-text-property url-beg url-end
                                 'jabber-chat-image-url url)
              (let ((beg (copy-marker url-beg))
                    (end (copy-marker url-end))
                    (buf (current-buffer)))
                (if (string-prefix-p "aesgcm://" url)
                    (jabber-chat--fetch-aesgcm-image
                     url
                     #'jabber-chat--replace-url-with-image
                     beg end buf)
                  (jabber-image-fetch
                   url
                   #'jabber-chat--replace-url-with-image
                   beg end buf))))))))))

(defun jabber-chat-goto-address (_msg _who mode)
  "Call `goto-address' on the newly written text."
  (when (eq mode :insert)
    (ignore-errors
      (let ((end (point))
	    (limit (max (- (point) 1000) (1+ (point-min)))))
	;; We only need to fontify the text written since the last
	;; prompt.  The prompt has a field property, so we can find it
	;; using `field-beginning'.
	(goto-address-fontify (field-beginning nil nil limit) end)))))

(defun jabber-chat-mark-oob-attachment (msg _who mode)
  "Mark non-image OOB attachment URLs with download keymap.
Runs after `jabber-chat-goto-address' so the goto-address overlay
exists when we set our keymap as its parent."
  (when (eql mode :insert)
    (let ((oob-url (plist-get msg :oob-url)))
      (when (and oob-url (not (jabber-chat--image-url-p oob-url)))
        (save-excursion
          (when (search-backward oob-url nil t)
            (let ((beg (match-beginning 0))
                  (end (match-end 0))
                  (inhibit-read-only t))
              (put-text-property beg end 'jabber-chat-file-url oob-url)
              ;; goto-address places an overlay with its own keymap that
              ;; shadows text-property keymaps.  Set ours as parent so
              ;; RET falls through to jabber-chat-url-action-at-point.
              (let ((ov (seq-find (lambda (o) (overlay-get o 'keymap))
                                  (overlays-in beg end))))
                (if ov
                    (set-keymap-parent (overlay-get ov 'keymap)
                                      jabber-chat-url-keymap)
                  (put-text-property beg end 'keymap
                                     jabber-chat-url-keymap))))))))))

;; jabber-compose is autoloaded in jabber.el
(defun jabber-send-message (jc to subject body type)
  "Send a message tag to the server.
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
With a prefix argument, open buffer in other window."
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
