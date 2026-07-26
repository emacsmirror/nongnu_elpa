;;; jabber-presence.el --- Roster and presence bookkeeping  -*- lexical-binding: t; -*-

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

;;; Commentary:
;;

;;; Code:

(require 'jabber-core)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-lifecycle)
(require 'jabber-alert)
(require 'jabber-util)
(require 'jabber-muc-protocol)
(require 'jabber-muc-state)
(require 'jabber-presence-events)

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

(defvar jabber-current-show)          ; jabber.el
(defvar jabber-current-status)        ; jabber.el
(defvar jabber-current-priority)      ; jabber.el
(defvar jabber-default-show)            ; jabber.el
(defvar jabber-default-status)          ; jabber.el
(defvar jabber-default-priority)        ; jabber.el
(defvar jabber-silent-mode)             ; jabber.el
(defvar jabber-roster-xmlns)           ; jabber-xml.el

;;

(defun jabber--roster-valid-push-p (from state-data)
  "Return non-nil if FROM is a valid roster push sender for STATE-DATA.
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
      (jabber-presence-events-dispatch-roster-update
       jc new-items changed-items deleted-items)
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

(defun jabber-presence--request-initial-roster (jc)
  "Request the initial roster for JC."
  (jabber-send-iq jc nil
                  "get"
                  `(query ((xmlns . ,jabber-roster-xmlns)))
                  #'jabber-process-roster 'initial
                  #'jabber-initial-roster-failure nil))

(add-hook 'jabber-lifecycle-session-bootstrap-functions
          #'jabber-presence--request-initial-roster)

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

(defun jabber-presence--resource-plist (resource &rest properties)
  "Return a copy of RESOURCE updated with PROPERTIES."
  (let ((result (copy-sequence resource)))
    (while properties
      (setq result (plist-put result (pop properties) (pop properties))))
    result))

(defun jabber-presence--resource-transition
    (type resource current metadata)
  "Return the presence transition for RESOURCE given TYPE and METADATA.
CURRENT is the existing resource plist.  The result contains
`:newstatus' and `:resource', plus `:clear-all' and `:status' when
a bare unavailable or error presence resets the buddy."
  (let ((show (plist-get metadata :show))
        (status (plist-get metadata :status))
        (error-xml (plist-get metadata :error))
        (priority (plist-get metadata :priority)))
    (cond
     ((and (string= resource "") (member type '("unavailable" "error")))
      (list :newstatus (and (string= type "error") "error")
            :resource nil
            :clear-all t
            :status (if error-xml
                        (jabber-parse-error error-xml)
                      status)))
     ((string= type "unavailable")
      (list :newstatus nil
            :resource (jabber-presence--resource-plist
                       current 'connected nil 'show nil 'status status)))
     ((string= type "error")
      (list :newstatus "error"
            :resource
            (jabber-presence--resource-plist
             current 'connected nil 'show "error" 'status
             (if error-xml (jabber-parse-error error-xml) status))))
     ((member type '("unsubscribe" "subscribed" "unsubscribed"))
      (list :newstatus type :resource current))
     (t
      (list :newstatus (or show "")
            :resource
            (jabber-presence--resource-plist
             current 'connected t 'show (or show "")
             'status status 'priority priority))))))

(defun jabber-presence--update-resource (buddy type resource metadata)
  "Update BUDDY presence for RESOURCE given TYPE and METADATA.
METADATA is a plist from `jabber-presence--extract-metadata'.
Modifies BUDDY symbol properties as a side effect.
Return (NEWSTATUS . RESOURCE-PLIST)."
  (let* ((current (cdr (assoc resource (get buddy 'resources))))
         (transition
          (jabber-presence--resource-transition
           type resource current metadata))
         (newstatus (plist-get transition :newstatus))
         (resource-plist (plist-get transition :resource)))
    (when (plist-get transition :clear-all)
      ;; A bare unavailable or error presence takes every resource offline.
      (put buddy 'resources nil)
      (put buddy 'connected nil)
      (put buddy 'show newstatus)
      (put buddy 'status (plist-get transition :status)))
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
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
         (from (jabber-xml-get-attribute xml-data 'from))
         (type (jabber-xml-get-attribute xml-data 'type))
         (metadata (jabber-presence--extract-metadata xml-data)))
    (cond
     ((string= type "subscribe")
      (run-with-idle-timer
       0.01 nil #'jabber-presence-events-dispatch-subscription-request
       jc from (plist-get metadata :status)))

     ((jabber-muc-presence-p xml-data)
      (jabber-presence-events-dispatch-muc jc xml-data))

     (t
      (jabber-presence-events-dispatch-contact jc from)
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

;;;###autoload
(defun jabber-send-presence (show status priority &optional jc)
  "Set presence to SHOW with STATUS message and PRIORITY.
SHOW is one of \"\", \"away\", \"chat\", \"dnd\", \"xa\".
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
           (jabber-read-with-input-method "Status message: " jabber-current-status
                                          'jabber-status-history)
           (read-string "Priority: " (int-to-string (if jabber-current-priority
                                                        jabber-current-priority
                                                      jabber-default-priority)))
           jc)))

  (setq jabber-current-show show jabber-current-status status)
  (setq jabber-current-priority
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

  (run-hooks 'jabber-presence-sent-hooks))

(defun jabber-presence-children (jc)
  "Return the children for a <presence/> stanza.
JC is the Jabber connection."
  (append
   (delq nil
         (list (when (and jabber-current-status
                          (> (length jabber-current-status) 0))
                 `(status () ,jabber-current-status))
               (when (and jabber-current-show
                          (> (length jabber-current-show) 0))
                 `(show () ,jabber-current-show))
               (when jabber-current-priority
                 `(priority () ,(number-to-string jabber-current-priority)))))
   (apply #'append (mapcar (lambda (f)
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
    (let ((jabber-current-show
	   (if (string= type "online")
	       ""
	     type))
	  (jabber-current-status nil))
      (jabber-send-sexp jc `(presence ((to . ,jid))
				      ,@(jabber-presence-children jc)))))))

(defun jabber-send-away-presence (&optional status jc)
  "Set presence to away with the given STATUS message.
With prefix argument, ask for STATUS message.
If JC is non-nil, send only for that connection."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " jabber-current-status 'jabber-status-history))))
  (jabber-send-presence "away" (if status status jabber-current-status)
			jabber-current-priority jc))

;; XXX code duplication!
(defun jabber-send-xa-presence (&optional status jc)
  "Send extended-away presence with the given STATUS message.
With prefix argument, ask for STATUS message.
If JC is non-nil, send only for that connection."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " jabber-current-status 'jabber-status-history))))
  (jabber-send-presence "xa" (if status status jabber-current-status)
			jabber-current-priority jc))

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
  (if jabber-current-show
      (jabber-send-presence jabber-current-show jabber-current-status
			    jabber-current-priority jc)
    (jabber-send-default-presence jc)))

(defun jabber-presence--refresh-advertised-features ()
  "Resend presence with the current advertised feature set."
  (mapc #'jabber-send-current-presence jabber-connections))

(add-hook 'jabber-disco-features-changed-hook
          #'jabber-presence--refresh-advertised-features)

(defun jabber-send-subscription-request (jc to &optional request)
  "Send a subscription request to TO.
REQUEST, if non-empty, is included as the status text.

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
  "Add or change roster item JID with NAME and GROUPS.
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
  "Remove JID from the roster on connection JC."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Delete from roster: ")))
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . ,jabber-roster-xmlns))
			  (item ((jid . ,jid)
				 (subscription . "remove"))))
		  #'jabber-report-success "Roster item removal"
		  #'jabber-report-success "Roster item removal"))



(provide 'jabber-presence)

;;; jabber-presence.el ends here
