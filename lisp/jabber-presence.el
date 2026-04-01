;; jabber-presence.el - roster and presence bookkeeping  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
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

(require 'jabber-core)
(require 'jabber-iq)
(require 'jabber-alert)
(require 'jabber-util)
(require 'jabber-menu)
(require 'ewoc)

(defconst jabber-presence-show-alist
  '(("Online" . "")
    ("Away" . "away")
    ("Extended Away" . "xa")
    ("Do Not Disturb" . "dnd")
    ("Free to Chat" . "chat"))
  "Alist mapping human-readable labels to XMPP presence show values.")

(defvar jabber-presence-element-functions nil
  "List of functions returning extra elements for <presence/> stanzas.
Each function takes one argument, the connection, and returns a
possibly empty list of extra child element of the <presence/>
stanza.")

(defvar jabber-presence-history ()
  "Keeps track of previously used presence status types.")

(defvar jabber-presence-sent-hooks nil
  "List of functions called after presence messages are sent.")

;; Global reference declarations

(declare-function jabber-roster--refresh "jabber-roster.el" ())
(declare-function jabber-roster-update "jabber-roster.el"
                  (jc new-items changed-items deleted-items))
(declare-function jabber-chat-create-buffer "jabber-chat.el" (jc chat-with))
(declare-function jabber-chat-ewoc-enter "jabber-chatbuffer.el" (data))
(declare-function jabber-chat-ewoc-delete "jabber-chatbuffer" (node))
(declare-function jabber-chat-get-buffer "jabber-chat.el" (chat-with &optional jc))
(declare-function jabber-muc-get-buffer "jabber-muc.el" (group &optional jc))
(declare-function jabber-muc-process-presence "jabber-muc.el" (jc presence))
(declare-function jabber-muc-presence-p "jabber-muc.el" (presence))
(declare-function jabber-muc-active-rooms "jabber-muc.el" ())
(declare-function jabber-muc-connection "jabber-muc.el" (group))
(declare-function jabber-muc-nickname "jabber-muc.el" (group &optional jc))
(declare-function jabber-muc-room-entries "jabber-muc.el" (group))
(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar *jabber-current-show*)          ; jabber.el
(defvar *jabber-current-status*)        ; jabber.el
(defvar *jabber-current-priority*)      ; jabber.el
(defvar jabber-default-show)            ; jabber.el
(defvar jabber-default-status)          ; jabber.el
(defvar jabber-default-priority)        ; jabber.el
(defvar jabber-silent-mode)             ; jabber.el
(defvar jabber-roster-xmlns)           ; jabber-xml.el

;;

(defun jabber--roster-valid-push-p (from state-data)
  "Return non-nil if FROM is a valid roster push sender.
Valid senders are: nil (absent), the bare server, or our own full/bare JID."
  (let ((username (plist-get state-data :username))
        (server (plist-get state-data :server))
        (resource (plist-get state-data :resource)))
    (or (null from)
        (string= from server)
        (string= from (concat username "@" server))
        (string= from (concat username "@" server "/" resource)))))

(defun jabber--roster-process-item (item roster initialp)
  "Process a single roster ITEM element.
ROSTER is the current roster list.  INITIALP non-nil means initial fetch.
Return (CATEGORY . JID-SYMBOL) where CATEGORY is `new', `changed', or `deleted'."
  (let* ((jid (jabber-jid-symbol (jabber-xml-get-attribute item 'jid)))
         (existing (car (memq jid roster))))
    (if (string= (jabber-xml-get-attribute item 'subscription) "remove")
        (progn
          (if (jabber-jid-rostername jid)
              (message "%s (%s) removed from roster" (jabber-jid-rostername jid) jid)
            (message "%s removed from roster" jid))
          (cons 'deleted jid))
      (let ((roster-item (or existing jid)))
        (when (and (not existing) (not initialp))
          (if (jabber-xml-get-attribute item 'name)
              (message "%s (%s) added to roster"
                       (jabber-xml-get-attribute item 'name) jid)
            (message "%s added to roster" jid)))
        (when initialp
          (setplist roster-item nil))
        (put roster-item 'name (jabber-xml-get-attribute item 'name))
        (put roster-item 'subscription (jabber-xml-get-attribute item 'subscription))
        (put roster-item 'ask (jabber-xml-get-attribute item 'ask))
        (put roster-item 'xml item)
        (put roster-item 'groups
             (mapcar (lambda (g) (nth 2 g))
                     (jabber-xml-get-children item 'group)))
        (cons (if existing 'changed 'new) roster-item)))))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons jabber-roster-xmlns (function (lambda (jc x) (jabber-process-roster jc x nil)))))
(defun jabber-process-roster (jc xml-data closure-data)
  "Process an incoming roster infoquery result.
CLOSURE-DATA should be `initial' if initial roster push, nil otherwise.
JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((state-data (fsm-get-state-data jc))
         (roster (plist-get state-data :roster))
         (from (jabber-xml-get-attribute xml-data 'from))
         (type (jabber-xml-get-attribute xml-data 'type))
         (id (jabber-xml-get-attribute xml-data 'id))
         (initialp (eq closure-data 'initial))
         new-items changed-items deleted-items)
    (if (not (jabber--roster-valid-push-p from state-data))
        (message "Roster push with invalid \"from\": \"%s\"" from)
      (dolist (item (jabber-xml-get-children
                     (car (jabber-xml-get-children xml-data 'query)) 'item))
        (pcase (jabber--roster-process-item item roster initialp)
          (`(new . ,sym)     (push sym new-items))
          (`(changed . ,sym) (push sym changed-items))
          (`(deleted . ,sym) (push sym deleted-items))))
      (jabber-roster-update jc new-items changed-items deleted-items)
      (when (and id (string= type "set"))
        (jabber-send-iq jc nil "result" nil nil nil nil nil id)))
    (when initialp
      (run-hook-with-args 'jabber-post-connect-hooks jc))))

(defun jabber-initial-roster-failure (jc xml-data _closure-data)
  "Report the initial roster failure.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  ;; If the initial roster request fails, let's report it, but run
  ;; `jabber-post-connect-hooks' anyway. According to the spec, there is
  ;; nothing exceptional about the server not returning a roster.
  (jabber-report-success jc xml-data "Initial roster retrieval")
  (run-hook-with-args 'jabber-post-connect-hooks jc))

(defun jabber-presence--extract-metadata (xml-data)
  "Parse presence metadata from XML-DATA.
Return a plist (:show :status :priority :error)."
  (list :show (car (jabber-xml-node-children
                    (car (jabber-xml-get-children xml-data 'show))))
        :status (car (jabber-xml-node-children
                      (car (jabber-xml-get-children xml-data 'status))))
        :priority (string-to-number
                   (or (car (jabber-xml-node-children
                             (car (jabber-xml-get-children xml-data 'priority))))
                       "0"))
        :error (car (jabber-xml-get-children xml-data 'error))))

(defun jabber-presence--update-resource (buddy type resource metadata)
  "Update BUDDY presence for RESOURCE given TYPE and METADATA.
METADATA is a plist from `jabber-presence--extract-metadata'.
Modifies BUDDY symbol properties as a side effect.
Return (NEWSTATUS . RESOURCE-PLIST)."
  (let ((resource-plist (cdr (assoc resource (get buddy 'resources))))
        (presence-show (plist-get metadata :show))
        (presence-status (plist-get metadata :status))
        (error-xml (plist-get metadata :error))
        (priority (plist-get metadata :priority))
        newstatus)
    (cond
     ((and (string= resource "") (member type '("unavailable" "error")))
      ;; 'unavailable' or 'error' from bare JID means that all resources
      ;; are offline.
      (setq resource-plist nil)
      (setq newstatus (if (string= type "error") "error" nil))
      (let ((new-message (if error-xml
                             (jabber-parse-error error-xml)
                           presence-status)))
        ;; erase any previous information
        (put buddy 'resources nil)
        (put buddy 'connected nil)
        (put buddy 'show newstatus)
        (put buddy 'status new-message)))

     ((string= type "unavailable")
      (setq resource-plist
            (plist-put resource-plist 'connected nil))
      (setq resource-plist
            (plist-put resource-plist 'show nil))
      (setq resource-plist
            (plist-put resource-plist 'status
                       presence-status)))

     ((string= type "error")
      (setq newstatus "error")
      (setq resource-plist
            (plist-put resource-plist 'connected nil))
      (setq resource-plist
            (plist-put resource-plist 'show "error"))
      (setq resource-plist
            (plist-put resource-plist 'status
                       (if error-xml
                           (jabber-parse-error error-xml)
                         presence-status))))
     ((or
       (string= type "unsubscribe")
       (string= type "subscribed")
       (string= type "unsubscribed"))
      ;; Do nothing, except letting the user know.  The Jabber protocol
      ;; places all this complexity on the server.
      (setq newstatus type))
     (t
      (setq resource-plist
            (plist-put resource-plist 'connected t))
      (setq resource-plist
            (plist-put resource-plist 'show (or presence-show "")))
      (setq resource-plist
            (plist-put resource-plist 'status
                       presence-status))
      (setq resource-plist
            (plist-put resource-plist 'priority priority))
      (setq newstatus (or presence-show ""))))
    (cons newstatus resource-plist)))

(defun jabber-presence--run-hooks (buddy oldstatus newstatus status-message)
  "Fire presence hooks for BUDDY with OLDSTATUS, NEWSTATUS, and STATUS-MESSAGE.
Runs `jabber-presence-hooks' and `jabber-alert-presence-hooks'."
  (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
    (run-hook-with-args hook
                        buddy
                        oldstatus
                        newstatus
                        status-message
                        (funcall jabber-alert-presence-message-function
                                 buddy
                                 oldstatus
                                 newstatus
                                 status-message))))

(jabber-chain-add 'jabber-presence-chain #'jabber-process-presence)
(defun jabber-process-presence (jc xml-data)
  "Process incoming presence tags.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  ;; XXX: use JC argument
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
         (from (jabber-xml-get-attribute xml-data 'from))
         (type (jabber-xml-get-attribute xml-data 'type))
         (metadata (jabber-presence--extract-metadata xml-data)))
    (cond
     ((string= type "subscribe")
      (run-with-idle-timer 0.01 nil #'jabber-process-subscription-request
                           jc from (plist-get metadata :status)))

     ((jabber-muc-presence-p xml-data)
      (jabber-muc-process-presence jc xml-data))

     (t
      ;; Clean up any stale subscription request prompts for this JID.
      (jabber-subscription--remove-stale jc from)
      ;; XXX: Think about what to do about out-of-roster presences.
      (let ((buddy (jabber-jid-symbol from)))
        (when (memq buddy roster)
          (let* ((oldstatus (get buddy 'show))
                 (resource (or (jabber-jid-resource from) ""))
                 (result (jabber-presence--update-resource
                          buddy type resource metadata))
                 (newstatus (car result))
                 (resource-plist (cdr result)))

            (when resource-plist
              ;; this is for `assoc-set!' in guile
              (if (assoc resource (get buddy 'resources))
                  (setcdr (assoc resource (get buddy 'resources))
                          resource-plist)
                (put buddy 'resources
                     (cons (cons resource resource-plist)
                           (get buddy 'resources))))
              (jabber-prioritize-resources buddy))

            (fsm-send jc (cons :roster-update buddy))

            (jabber-presence--run-hooks
             buddy oldstatus newstatus
             (plist-get resource-plist 'status)))))))))

(defun jabber-process-subscription-request (jc from presence-status)
  "Process an incoming subscription request.
JC is the Jabber connection."
  (with-current-buffer (jabber-chat-create-buffer jc from)
    (jabber-chat-ewoc-enter (list :subscription-request presence-status :time (current-time)))

    (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
      (run-hook-with-args hook (jabber-jid-symbol from) nil "subscribe" presence-status (funcall jabber-alert-presence-message-function (jabber-jid-symbol from) nil "subscribe" presence-status)))))

(defun jabber-subscription-accept-mutual (&rest _ignored)
  (message "Subscription accepted; reciprocal subscription request sent")
  (jabber-subscription-reply "subscribed" "subscribe"))

(defun jabber-subscription-accept-one-way (&rest _ignored)
  (message "Subscription accepted")
  (jabber-subscription-reply "subscribed"))

(defun jabber-subscription-decline (&rest _ignored)
  (message "Subscription declined")
  (jabber-subscription-reply "unsubscribed"))

(defun jabber-subscription--remove-prompt ()
  "Remove the subscription request EWOC node at point."
  (when (bound-and-true-p jabber-chat-ewoc)
    (let ((node (ewoc-locate jabber-chat-ewoc)))
      (when (and node (eq :subscription-request (car (ewoc-data node))))
        (jabber-chat-ewoc-delete node)))))

(defun jabber-subscription--remove-stale (jc from)
  "Remove all subscription request nodes from FROM's chat buffer.
JC is the Jabber connection."
  (when-let* ((buf (get-buffer (jabber-chat-get-buffer from jc))))
    (with-current-buffer buf
      (when (bound-and-true-p jabber-chat-ewoc)
        (let ((node (ewoc-nth jabber-chat-ewoc 0))
              to-delete)
          (while node
            (when (eq :subscription-request (car (ewoc-data node)))
              (push node to-delete))
            (setq node (ewoc-next jabber-chat-ewoc node)))
          (dolist (n to-delete)
            (jabber-chat-ewoc-delete n)))))))

(defun jabber-subscription-reply (&rest types)
  (let ((to (jabber-jid-user jabber-chatting-with)))
    (dolist (type types)
      (jabber-send-sexp jabber-buffer-connection `(presence ((to . ,to) (type . ,type))))))
  (jabber-subscription--remove-prompt))

(defun jabber-prioritize-resources (buddy)
  "Set connected, show and status properties for BUDDY.
Show status properties from highest-priority resource."
  (let ((resource-alist (get buddy 'resources))
	(highest-priority nil))
    ;; Reset to nil at first, for cases (a) resource-alist is nil
    ;; and (b) all resources are disconnected.
    (put buddy 'connected nil)
    (put buddy 'show nil)
    (put buddy 'status nil)
    (mapc #'(lambda (resource)
	      (let* ((resource-plist (cdr resource))
		     (priority (plist-get resource-plist 'priority)))
		(if (plist-get resource-plist 'connected)
		    (when (or (null highest-priority)
			      (and priority
				   (> priority highest-priority)))
		      ;; if no priority specified, interpret as zero
		      (setq highest-priority (or priority 0))
		      (put buddy 'connected (plist-get resource-plist 'connected))
		      (put buddy 'show (plist-get resource-plist 'show))
		      (put buddy 'status (plist-get resource-plist 'status))
		      (put buddy 'resource (car resource)))

		  ;; if we have not found a connected resource yet, but this
		  ;; disconnected resource has a status message, display it.
		  (when (not (get buddy 'connected))
		    (if (plist-get resource-plist 'status)
			(put buddy 'status (plist-get resource-plist 'status)))
		    (if (plist-get resource-plist 'show)
			(put buddy 'show (plist-get resource-plist 'show)))))))
	  resource-alist)))

(defun jabber-count-connected-resources (buddy)
  "Return the number of connected resources for BUDDY."
  (let ((resource-alist (get buddy 'resources))
	(count 0))
    (dolist (resource resource-alist)
      (if (plist-get (cdr resource) 'connected)
	  (setq count (1+ count))))
    count))

;;;###autoload
(defun jabber-send-presence (show status priority &optional jc)
  "Set presence.
When called interactively, prompt for which account to use.
With prefix argument, send to all accounts.
When JC is non-nil, send only for that connection.
When JC is nil, send for all connections."
  (interactive
   (let* ((jc (unless current-prefix-arg (jabber-read-account)))
          (label (completing-read "Status: "
                                  (mapcar #'car jabber-presence-show-alist)
                                  nil t nil 'jabber-presence-history))
          (show (cdr (assoc label jabber-presence-show-alist))))
     (list show
           (jabber-read-with-input-method "Status message: " *jabber-current-status*
                                          '*jabber-status-history*)
           (read-string "Priority: " (int-to-string (if *jabber-current-priority*
                                                         *jabber-current-priority*
                                                       jabber-default-priority)))
           jc)))

  (setq *jabber-current-show* show *jabber-current-status* status)
  (setq *jabber-current-priority*
	(if (numberp priority) priority (string-to-number priority)))

  (let ((connections (if jc (list jc) jabber-connections))
        subelements-map)
    ;; For each connection, we use a different set of subelements.  We
    ;; cache them, to only generate them once.

    ;; Ordinary presence, with no specified recipient
    (dolist (c connections)
      (let ((subelements (jabber-presence-children c)))
        (push (cons c subelements) subelements-map)
	(jabber-send-sexp-if-connected c `(presence () ,@subelements))))

    ;; Then send presence to groupchats.  A room may have entries for
    ;; multiple accounts, so iterate all (JC . NICK) pairs.
    (dolist (room (jabber-muc-active-rooms))
      (dolist (entry (jabber-muc-room-entries room))
	(let* ((room-jc (car entry))
	       (nick (cdr entry))
	       (subelements (cdr (assq room-jc subelements-map))))
	  (when (and room-jc (or (null jc) (eq room-jc jc)))
	    (jabber-send-sexp-if-connected
	     room-jc `(presence ((to . ,(concat room "/" nick)))
				,@subelements)))))))

  (jabber-roster--refresh)
  (run-hooks 'jabber-presence-sent-hooks))

(defun jabber-presence-children (jc)
  "Return the children for a <presence/> stanza.
JC is the Jabber connection."
  `(,(when (> (length *jabber-current-status*) 0)
       `(status () ,*jabber-current-status*))
    ,(when (> (length *jabber-current-show*) 0)
	 `(show () ,*jabber-current-show*))
    ,(when *jabber-current-priority*
       `(priority () ,(number-to-string *jabber-current-priority*)))
    ,@(apply #'append (mapcar (lambda (f)
			        (funcall f jc))
			      jabber-presence-element-functions))))

(defun jabber-send-directed-presence (jc jid type)
  "Send a directed presence stanza to JID.
TYPE is one of:
\"online\", \"away\", \"xa\", \"dnd\", \"chatty\":
  Appear as present with the given status.
\"unavailable\":
  Appear as offline.
\"probe\":
  Ask the contact's server for updated presence.
\"subscribe\":
  Ask for subscription to contact's presence.
  (see also `jabber-send-subscription-request')
\"unsubscribe\":
  Cancel your subscription to contact's presence.
\"subscribed\":
  Accept contact's request for presence subscription.
  (this is usually done within a chat buffer)
\"unsubscribed\":
  Cancel contact's subscription to your presence.

JC is the Jabber connection."
  (interactive
   (list (jabber-read-account)
	 (jabber-read-jid-completing "Send directed presence to: ")
	 (completing-read "Type (default is online): "
			  '(("online")
			    ("away")
			    ("xa")
			    ("dnd")
			    ("chatty")
			    ("probe")
			    ("unavailable")
			    ("subscribe")
			    ("unsubscribe")
			    ("subscribed")
			    ("unsubscribed"))
			  nil t nil 'jabber-presence-history "online")))
  (cond
   ((member type '("probe" "unavailable"
		   "subscribe" "unsubscribe"
		   "subscribed" "unsubscribed"))
    (jabber-send-sexp jc `(presence ((to . ,jid)
				     (type . ,type)))))

   (t
    (let ((*jabber-current-show*
	   (if (string= type "online")
	       ""
	     type))
	  (*jabber-current-status* nil))
      (jabber-send-sexp jc `(presence ((to . ,jid))
				      ,@(jabber-presence-children jc)))))))

(defun jabber-send-away-presence (&optional status jc)
  "Set status to away.
With prefix argument, ask for status message.
If JC is non-nil, send only for that connection."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " *jabber-current-status* '*jabber-status-history*))))
  (jabber-send-presence "away" (if status status *jabber-current-status*)
			*jabber-current-priority* jc))

;; XXX code duplication!
(defun jabber-send-xa-presence (&optional status jc)
  "Send extended away presence.
With prefix argument, ask for status message.
If JC is non-nil, send only for that connection."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " *jabber-current-status* '*jabber-status-history*))))
  (jabber-send-presence "xa" (if status status *jabber-current-status*)
			*jabber-current-priority* jc))

;;;###autoload
(defun jabber-send-default-presence (&optional jc)
  "Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'.
If JC is non-nil, send only for that connection."
  (interactive)
  (jabber-send-presence
   jabber-default-show jabber-default-status jabber-default-priority jc))

(defun jabber-send-current-presence (&optional jc)
  "(Re-)send current presence.
That is, if presence has already been sent, use current settings,
otherwise send defaults (see `jabber-send-default-presence').
If JC is non-nil, send only for that connection."
  (interactive)
  (if *jabber-current-show*
      (jabber-send-presence *jabber-current-show* *jabber-current-status*
			    *jabber-current-priority* jc)
    (jabber-send-default-presence jc)))

(defun jabber-send-subscription-request (jc to &optional request)
  "Send a subscription request to jid.
Show him your request text, if specified.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "request: ")))
  (jabber-send-sexp jc
		    `(presence
		      ((to . ,to)
		       (type . "subscribe"))
		      ,@(when (and request (> (length request) 0))
			  (list `(status () ,request))))))

(defvar jabber-roster-group-history nil
  "History of entered roster groups.")

(defun jabber-roster-change (jc jid name groups)
  "Add or change a roster item.
JC is the Jabber connection."
  (interactive (let* ((jid (jabber-jid-symbol
			    (jabber-read-jid-completing "Add/change JID: ")))
		      (account (jabber-read-account))
		      (name (get jid 'name))
		      (groups (get jid 'groups))
		      (all-groups
		       (apply #'append
			      (mapcar
			       (lambda (j) (get j 'groups))
			       (plist-get (fsm-get-state-data account) :roster)))))
		 (list account
		       jid (jabber-read-with-input-method (format "Name: (default `%s') " name) nil nil name)
		       (delete ""
			       (completing-read-multiple
				(format
				 "Groups, comma-separated: (default %s) "
				 (if groups
				     (mapconcat #'identity groups ",")
				   "none"))
				all-groups
				nil nil nil
				'jabber-roster-group-history
				(mapconcat #'identity groups ",")
				t)))))
  ;; If new fields are added to the roster XML structure in a future standard,
  ;; they will be clobbered by this function.
  ;; XXX: specify account
  (jabber-send-iq jc nil "set"
		  (list 'query (list (cons 'xmlns jabber-roster-xmlns))
				(append
				 (list 'item (append
				     (list (cons 'jid (symbol-name jid)))
				     (if (and name (> (length name) 0))
					 (list (cons 'name name)))))
				 (mapcar #'(lambda (x) `(group () ,x))
				      groups)))
		  #'jabber-report-success "Roster item change"
		  #'jabber-report-success "Roster item change"))

(defun jabber-roster-delete (jc jid)
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Delete from roster: ")))
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . ,jabber-roster-xmlns))
			  (item ((jid . ,jid)
				 (subscription . "remove"))))
		  #'jabber-report-success "Roster item removal"
		  #'jabber-report-success "Roster item removal"))

(defun jabber-roster-delete-jid-at-point ()
  "Delete JID at point from roster.
Signal an error if there is no JID at point."
  (interactive)
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid))
	(account (get-text-property (point) 'jabber-account)))
    (if (and jid-at-point account
	     (or jabber-silent-mode (yes-or-no-p (format "Really delete %s from roster? " jid-at-point))))
	(jabber-roster-delete account jid-at-point)
      (error "No contact at point"))))

(defun jabber-roster-delete-group-from-jids (jc jids group)
  "Delete group `group' from all JIDs.
JC is the Jabber connection."
  (interactive)
  (dolist (jid jids)
    (jabber-roster-change
     jc jid (get jid 'name)
     (cl-remove-if-not (lambda (g) (not (string= g group)))
		       (get jid 'groups)))))

(defun jabber-roster-edit-group-from-jids (jc jids group)
  "Edit group `group' from all JIDs.
JC is the Jabber connection."
  (interactive)
  (let ((new-group
	 (jabber-read-with-input-method
	  (format "New group: (default `%s') " group) nil nil group)))
    (dolist (jid jids)
      (jabber-roster-change
       jc jid (get jid 'name)
       (cl-remove-duplicates
	(mapcar
	 (lambda (g) (if (string= g group)
			 new-group
		       g))
	 (get jid 'groups))
	:test #'string=)))))


(provide 'jabber-presence)

;;; jabber-presence.el ends here