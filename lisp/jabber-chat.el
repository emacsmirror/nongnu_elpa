;;; jabber-chat.el --- one-to-one chats  -*- lexical-binding: t; -*-

;; Copyright (C) 2005, 2007, 2008 - Magnus Henoch - mange@freemail.hu

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
(require 'jabber-history)
(require 'jabber-menu)                  ;we need jabber-jid-chat-menu
(require 'ewoc)
(require 'goto-addr)

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

(defcustom jabber-chat-header-line-format
  '("" (jabber-chat-buffer-show-avatar
	(:eval
	 (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
	   (jabber-propertize " "
			      'display (get buddy 'avatar)))))
    (:eval (jabber-jid-displayname jabber-chatting-with))
    "\t" (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
		  (propertize
		   (or
		    (cdr (assoc (get buddy 'show) jabber-presence-strings))
		    (get buddy 'show))
		   'face
		   (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
		       'jabber-roster-user-online))))
    "\t" (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with) 'status)))
    "\t" jabber-events-message		;see jabber-events.el
    "\t" jabber-chatstates-message)		;see jabber-chatstates.el
  "The specification for the header line of chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp)

(defcustom jabber-chat-buffer-show-avatar t
  "Show avatars in header line of chat buffer?
This variable might not take effect if you have changed
`jabber-chat-header-line-format'."
  :type 'boolean)

(defcustom jabber-chat-time-format "%H:%M"
  "The format specification for instant messages in the chat buffer.
See also `jabber-chat-delayed-time-format'.

See `format-time-string' for valid values."
  :type 'string)

(defcustom jabber-chat-delayed-time-format "%Y-%m-%d %H:%M"
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

(defface jabber-rare-time-face
  '((t :inherit font-lock-comment-face :underline t))
  "Face for displaying rare time information."
  :group 'jabber-chat)

(defcustom jabber-chat-local-prompt-format "[%t] %n> "
  "The format specification for lines you type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%u   Username
%n   Nickname (obsolete, same as username)
%r   Resource
%j   Bare JID (without resource)"
  :type 'string)

(defcustom jabber-chat-foreign-prompt-format "[%t] %n> "
  "The format specification for lines others type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%n   Nickname, or JID if no nickname set
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string)

(defface jabber-chat-prompt-local
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for displaying the chat prompt for what you type in."
  :group 'jabber-chat)

(defface jabber-chat-prompt-foreign
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for displaying the chat prompt for what they send."
  :group 'jabber-chat)

(defface jabber-chat-prompt-system
  '((t :inherit success))
  "Face used for system and special messages."
  :group 'jabber-chat)

(defface jabber-chat-text-local
  '((t :inherit jabber-chat-prompt-local))
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
			 jabber-chat-goto-address)
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
(defvar jabber-backlog-days)
(defvar jabber-backlog-number)
(declare-function jabber-db-backlog "jabber-db.el"
                  (account peer &optional count start-time))
(declare-function jabber-db--store-outgoing "jabber-db.el"
                  (jc to body type))
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-muc-printers)            ; jabber-muc.el

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
  "Find an existing chat buffer for CHAT-WITH, or nil.
Searches by buffer-local `jabber-chatting-with' variable."
  (let ((bare (jabber-jid-user chat-with)))
    (cl-find bare (buffer-list)
             :test #'string=
             :key (lambda (buf)
                    (when-let ((jid (buffer-local-value 'jabber-chatting-with buf)))
                      (jabber-jid-user jid))))))

(defun jabber-chat-create-buffer (jc chat-with)
  "Prepare a buffer for chatting with CHAT-WITH.
This function is idempotent.
JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-chat-get-buffer chat-with jc))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode)
      (jabber-chat-mode-setup jc #'jabber-chat-pp)

      (make-local-variable 'jabber-chatting-with)
      (setq jabber-chatting-with chat-with)
      (setq jabber-send-function #'jabber-chat-send)
      (setq header-line-format jabber-chat-header-line-format)

      (make-local-variable 'jabber-chat-earliest-backlog)

      ;; insert backlog
      (when (null jabber-chat-earliest-backlog)
	(let ((backlog-entries (jabber-db-backlog
				(jabber-connection-bare-jid jc)
				(jabber-jid-user chat-with))))
	  (if (null backlog-entries)
	      (setq jabber-chat-earliest-backlog (jabber-float-time))
	    ;; backlog-entries is DESC; last element is oldest.
	    (setq jabber-chat-earliest-backlog
		  (float-time (plist-get (car (last backlog-entries)) :timestamp)))
	    ;; ewoc-enter-first with DESC input produces ascending display.
	    (mapc #'jabber-chat-insert-backlog-entry backlog-entries)))))

    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)

    (current-buffer)))

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
    (if (and jabber-print-rare-time
	     (ewoc-nth jabber-chat-ewoc 0)
	     (eq (car (ewoc-data (ewoc-nth jabber-chat-ewoc 0))) :rare-time)
	     (not (jabber-rare-time-needed message-time (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc 0))))))
	(ewoc-enter-after jabber-chat-ewoc (ewoc-nth jabber-chat-ewoc 0) node-data)
      ;; Insert first.
      (ewoc-enter-first jabber-chat-ewoc node-data)
      (when jabber-print-rare-time
	(ewoc-enter-first jabber-chat-ewoc (list :rare-time message-time))))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Display more context" 'jabber-chat-display-more-backlog))

(defun jabber-chat-display-more-backlog (how-many)
  "Display more context.
The HOW-MANY argument is number of messages.
Specify 0 to display all messages."
  (interactive "nHow many more messages (Specify 0 to display all)? ")
  (let* ((inhibit-read-only t)
	 (jabber-backlog-days nil)
	 (jabber-backlog-number (if (= how-many 0) t how-many))
	 (backlog-entries (jabber-db-backlog
			   (jabber-connection-bare-jid jabber-buffer-connection)
			   (jabber-jid-user (or jabber-chatting-with jabber-group))
			   jabber-backlog-number
			   jabber-chat-earliest-backlog)))
    (when backlog-entries
      (setq jabber-chat-earliest-backlog
	    (float-time (plist-get (car (last backlog-entries)) :timestamp)))
      (save-excursion
	(goto-char (point-min))
	(mapc #'jabber-chat-insert-backlog-entry backlog-entries)))))

(add-to-list 'jabber-message-chain #'jabber-process-chat)

(defun jabber-get-forwarded-message (xml-data)
  (let* ((sent (car (jabber-xml-get-children xml-data 'sent)))
         (forwarded (car (jabber-xml-get-children sent 'forwarded)))
         (forwarded-message (car (jabber-xml-get-children forwarded 'message))))
    (when forwarded-message
      forwarded-message)))

(defun jabber-process-chat (jc xml-data)
  "If XML-DATA is a one-to-one chat message, handle it as such.
JC is the Jabber connection."
  ;; For now, everything that is not a public MUC message is
  ;; potentially a 1to1 chat message.
  (when (not (jabber-muc-message-p xml-data))
    ;; Note that we handle private MUC messages here.
    (cl-destructuring-bind (xml-data chat-buffer)
        (if (car (jabber-xml-get-children xml-data 'sent))
            (let* ((fwd-msg (jabber-get-forwarded-message xml-data))
                   (to (jabber-xml-get-attribute fwd-msg 'to)))
              (list fwd-msg
                    (and
                     to
                     (jabber-chat-create-buffer
                      jc
                      ;; issue-106: cheogram telephony gateway silently drops
                      ;; message that include "/sip:..." resource part.
                      (if (string-equal (jabber-jid-server to) "cheogram.com")
                          (jabber-jid-user to)
                        to)))))
          (list xml-data nil))
      (let ((from (jabber-xml-get-attribute xml-data 'from))
	    (error-p (jabber-xml-get-children xml-data 'error))
	    (body-text (car (jabber-xml-node-children
			     (car (jabber-xml-get-children
				   xml-data 'body)))))
	    (msg-plist (jabber-chat--msg-plist-from-stanza xml-data)))
        ;; First check if we would output anything for this stanza.
        (when (or error-p
		  (run-hook-with-args-until-success 'jabber-chat-printers
                                                    msg-plist
                                                    :foreign :printp))
          ;; If so, create chat buffer, if necessary...
	  (with-current-buffer (if (jabber-muc-sender-p from)
				   (jabber-muc-private-create-buffer
				    jc
				    (jabber-jid-user from)
				    (jabber-jid-resource from))
			         (or chat-buffer
                                     (jabber-chat-create-buffer jc from)))
            ;; ...add the message to the ewoc...
	    (let ((node (ewoc-enter-last jabber-chat-ewoc
                                         (list (if error-p :error :foreign)
                                               msg-plist))))
	      (jabber-maybe-print-rare-time node))

            ;; ...and call alert hooks.
	    (dolist (hook '(jabber-message-hooks jabber-alert-message-hooks))
	      (run-hook-with-args hook
				  from (current-buffer) body-text
				  (funcall jabber-alert-message-function
					   from (current-buffer) body-text)))))))))

(defun jabber-chat-send (jc body)
  "Send BODY through connection JC, and display it in chat buffer.
JC is the Jabber connection."
  ;; Build the stanza...
  (let* ((id (apply #'format "emacs-msg-%d.%d.%d" (current-time)))
	 (stanza-to-send `(message
			   ((to . ,jabber-chatting-with)
			    (type . "chat")
			    (id . ,id))
			   (body () ,body))))
    ;; ...add additional elements...
    ;; TODO: Once we require Emacs 24.1, use `run-hook-wrapped' instead.
    ;; That way we don't need to eliminate the "local hook" functionality
    ;; here.
    (dolist (hook jabber-chat-send-hooks)
      (if (eq hook t)
	  ;; Local hook referring to global...
	  (when (local-variable-p 'jabber-chat-send-hooks)
	    (dolist (global-hook (default-value 'jabber-chat-send-hooks))
	      (nconc stanza-to-send (funcall global-hook body id))))
      (nconc stanza-to-send (funcall hook body id))))
    ;; ...display it, if it would be displayed.
    (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza-to-send)))
      (when (run-hook-with-args-until-success 'jabber-chat-printers msg-plist :local :printp)
        (jabber-maybe-print-rare-time
         (ewoc-enter-last jabber-chat-ewoc (list :local msg-plist)))))
    ;; ...and send it...
    (jabber-send-sexp jc stanza-to-send)))

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

(defun jabber-chat--msg-plist-from-stanza (xml-data &optional delayed)
  "Extract display fields from XML-DATA into a message plist.
If DELAYED is non-nil, mark the message as delayed regardless of
whether a delay element is present."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (body (car (jabber-xml-node-children
                     (car (jabber-xml-get-children xml-data 'body)))))
         (subject (car (jabber-xml-node-children
                        (car (jabber-xml-get-children xml-data 'subject)))))
         (timestamp (or (jabber-message-timestamp xml-data) (current-time)))
         (oob-x (cl-find-if
                 (lambda (x)
                   (and (listp x)
                        (string= (jabber-xml-get-attribute x 'xmlns)
                                 "jabber:x:oob")))
                 (jabber-xml-node-children xml-data)))
         (error-node (car (jabber-xml-get-children xml-data 'error)))
         ;; Detect MUC invitations - these need raw XML for accept/decline
         (has-invite (cl-find-if
                      (lambda (x)
                        (and (listp x)
                             (string= (jabber-xml-get-attribute x 'xmlns)
                                      "http://jabber.org/protocol/muc#user")
                             (jabber-xml-get-children x 'invite)))
                      (jabber-xml-get-children xml-data 'x)))
         (plist (list :from from
                      :body body
                      :subject subject
                      :timestamp timestamp
                      :delayed (or delayed
                                   (and (jabber-message-timestamp xml-data) t))
                      :oob-url (when oob-x
                                 (car (jabber-xml-node-children
                                       (car (jabber-xml-get-children
                                             oob-x 'url)))))
                      :oob-desc (when oob-x
                                  (car (jabber-xml-node-children
                                        (car (jabber-xml-get-children
                                              oob-x 'desc)))))
                      :error-text (when error-node
                                    (jabber-parse-error error-node)))))
    (when has-invite
      (plist-put plist :xml-data xml-data))
    plist))

(defun jabber-chat-pp (data)
  "Pretty-print a message node.
\(car data) is the node type: :local, :foreign, :muc-local,
:muc-foreign, :error, :muc-error, :notice, :muc-notice,
:rare-time, or :subscription-request.
For message types, (cadr data) is a message plist with keys
:from, :body, :subject, :timestamp, :delayed, :oob-url,
:oob-desc, :error-text.
This function is used as an ewoc prettyprinter."
  (let* ((beg (point))
         (type (car data))
         (msg (cadr data))
         (body (when (and (listp msg) (plist-member msg :body))
                 (plist-get msg :body)))
         (timestamp (when (and (listp msg) (plist-member msg :timestamp))
                      (plist-get msg :timestamp)))
         (delayed (when (and (listp msg) (plist-member msg :delayed))
                    (plist-get msg :delayed)))
         (/me-p
          (and (stringp body)
               (> (length body) 4)
               (string= (substring body 0 4) "/me ")))
         (muc-highlight (when (eq type :muc-notice)
                          (jabber-chat-muc-presence-highlight msg)))
         (muc-highlight-face (cdr-safe muc-highlight)))

    ;; Print prompt...
    (let ((prompt-start (point)))
      (pcase type
	(:local
	 (jabber-chat-self-prompt timestamp delayed /me-p))
	(:foreign
	 (jabber-chat-print-prompt msg timestamp delayed /me-p))
	((or :error :notice :subscription-request)
	 (jabber-chat-system-prompt (or timestamp (current-time))))
	(:muc-local
	 (jabber-muc-print-prompt msg t /me-p))
        (:muc-foreign
         (jabber-muc-print-prompt msg nil /me-p))
	(:muc-notice
         (cond
          (muc-highlight-face
           (jabber-muc-system-prompt)
           (put-text-property prompt-start (point) 'face muc-highlight-face))
          (muc-highlight)  ; matched but no face = hide entirely
          (t (jabber-muc-system-prompt))))
        (:muc-error
	 (jabber-muc-system-prompt)))
      (put-text-property prompt-start (point) 'field 'jabber-prompt))

    ;; ...and body
    (pcase type
      ((or :local :foreign)
       (run-hook-with-args 'jabber-chat-printers msg type :insert)
       (insert "\n"))
      ((or :muc-local :muc-foreign)
       (let ((args (list msg type :insert)))
	 (mapc (lambda (f) (apply f args))
	       (append jabber-muc-printers jabber-chat-printers))
         (insert "\n")))
      ((or :error :muc-error)
       (if (stringp msg)
	    (insert (jabber-propertize msg 'face 'jabber-chat-error) "\n")
	 (jabber-chat-print-error msg)))
      (:muc-notice
       (cond
        (muc-highlight-face
         (insert (jabber-propertize msg 'face muc-highlight-face) "\n"))
        (muc-highlight)  ; matched but no face = hide entirely
        (t (insert msg "\n"))))
      (:notice
       (insert msg "\n"))
      (:rare-time
       ;; When MUC presence announcements are hidden, lightly
       ;; trafficked chat rooms fill with superfluous :rare-time
       ;; entries.  To suppress these, search backward from the node
       ;; containing DATA for the previous visible node.  If that node
       ;; is also a :rare-time entry, remove its text.  This seems a
       ;; bit skeevy; we await a better implementation.
       (let* ((node (jabber-chat-find-node data))
              (prev-visible (jabber-find-previous-visible-node node)))
         (when (and
		prev-visible
		(eq (car (ewoc-data prev-visible)) :rare-time))
           (delete-region
            (setq beg (marker-position (ewoc-location prev-visible)))
            (point))
           (goto-char beg)))
       (insert (jabber-propertize (format-time-string jabber-rare-time-format msg)
                                  'face 'jabber-rare-time-face)
               "\n"))
      (:subscription-request
       (insert "This user requests subscription to your presence.\n")
       (when (and (stringp msg) (not (zerop (length msg))))
         (insert "Message: " msg "\n"))
       (insert "Accept?\n\n")
       (cl-flet ((button
	           (text action)
	           (if (fboundp 'insert-button)
		       (insert-button text 'action action)
		     ;; simple button replacement
		     (let ((keymap (make-keymap)))
		       (define-key keymap "\r" action)
		       (insert (jabber-propertize text 'keymap keymap 'face 'highlight))))
		   (insert "\t")))
	 (button "Mutual" 'jabber-subscription-accept-mutual)
	 (button "One-way" 'jabber-subscription-accept-one-way)
	 (button "Decline" 'jabber-subscription-decline)
         (insert "\n"))))

    (when jabber-chat-fill-long-lines
      (save-restriction
        (narrow-to-region beg (point))
        (jabber-chat-buffer-fill-long-lines)))

    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)))

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

(defun jabber-chat-print-prompt (msg timestamp delayed dont-print-nick-p)
  "Print prompt for received message.
MSG is a message plist.  TIMESTAMP is the time to display.
If DELAYED is non-nil, print long timestamp
\(`jabber-chat-delayed-time-format' as opposed to
`jabber-chat-time-format').
If DONT-PRINT-NICK-P is non-nil, don't include nickname."
  (let ((from (plist-get msg :from))
	(timestamp (or timestamp (plist-get msg :timestamp))))
    (insert (jabber-propertize
	     (format-spec jabber-chat-foreign-prompt-format
			  (list
			   (cons ?t (format-time-string
				     (if delayed
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (if dont-print-nick-p "" (jabber-jid-displayname from)))
			   (cons ?u (or (jabber-jid-username from) from))
			   (cons ?r (or (jabber-jid-resource from) ""))
			   (cons ?j (jabber-jid-user from))))
	     'face 'jabber-chat-prompt-foreign
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " from)))))

(defun jabber-chat-system-prompt (timestamp)
  (insert (jabber-propertize
	   (format-spec jabber-chat-foreign-prompt-format
			(list
			 (cons ?t (format-time-string jabber-chat-time-format
						      timestamp))
			 (cons ?n "")
			 (cons ?u "")
			 (cons ?r "")
			 (cons ?j "")))
	   'face 'jabber-chat-prompt-system
	   'help-echo
	   (concat (format-time-string "System message on %Y-%m-%d %H:%M:%S" timestamp)))))

(defun jabber-chat-self-prompt (timestamp delayed dont-print-nick-p)
  "Print prompt for sent message.
TIMESTAMP is the timestamp to print, or nil for now.
If DELAYED is non-nil, print long timestamp
\(`jabber-chat-delayed-time-format' as opposed to
`jabber-chat-time-format').
If DONT-PRINT-NICK-P is non-nil, don't include nickname."
  (let* ((state-data (fsm-get-state-data jabber-buffer-connection))
	 (username (plist-get state-data :username))
	 (server (plist-get state-data :server))
	 (resource (plist-get state-data :resource))
	 (nickname username))
    (insert (jabber-propertize
	     (format-spec jabber-chat-local-prompt-format
			  (list
			   (cons ?t (format-time-string
				     (if delayed
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (if dont-print-nick-p "" nickname))
			   (cons ?u username)
			   (cons ?r resource)
			   (cons ?j (concat username "@" server))))
	     'face 'jabber-chat-prompt-local
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from you")))))

(defun jabber-chat-print-error (msg)
  "Print error from message plist MSG in a readable way."
  (let ((error-text (plist-get msg :error-text)))
    (insert
     (jabber-propertize
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
	 (insert (jabber-propertize
		  "Subject: " 'face 'jabber-chat-prompt-system)
		 (jabber-propertize
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
	      (insert (jabber-propertize
		       (concat nick
			       " "
			       action)
		       'face 'jabber-chat-prompt-system)))
	  (insert (jabber-propertize
		   body
		   'face (pcase who
			   ((or :foreign :muc-foreign) 'jabber-chat-text-foreign)
			   ((or :local :muc-local) 'jabber-chat-text-local))))))
      t)))

(defun jabber-chat-print-url (msg _who mode)
  "Print URLs from message plist MSG."
  (let ((url (plist-get msg :oob-url)))
    (when url
      (when (eql mode :insert)
        (let ((desc (plist-get msg :oob-desc)))
          (insert (format "\n%s%s<%s>"
                          (jabber-propertize
                           "URL: " 'face 'jabber-chat-prompt-system)
                          (if (stringp desc) (concat desc " ") "")
                          url))))
      t)))

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

;; jabber-compose is autoloaded in jabber.el
(add-to-list 'jabber-jid-chat-menu
	     (cons "Compose message" 'jabber-compose))

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

(add-to-list 'jabber-jid-chat-menu
	     (cons "Start chat" 'jabber-chat-with))

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
