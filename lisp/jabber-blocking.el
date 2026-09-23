;;; jabber-blocking.el --- XEP-0191: Blocking Command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 - Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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
;; XEP-0191 Blocking Command support.  Allows users to block and
;; unblock JIDs, and retrieve their server-side blocklist.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-iq)
(require 'jabber-disco)

(defconst jabber-blocking-xmlns "urn:xmpp:blocking"
  "XML namespace for XEP-0191 Blocking Command.")

(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el

(defvar jabber-post-connect-hooks)

(defvar jabber-blocking-ready-hook nil
  "Hook called with a connection after blocking discovery settles.
Run after a successful blocklist fetch or discovery of an unsupported server.")

(defun jabber-blocking-ready-p (jc jid)
  "Return non-nil when JC's blocking discovery permits work for JID.
Own-account work proceeds while peer work waits for a successful snapshot.
Connections not using startup discovery retain their previous behavior."
  (let ((state (fsm-get-state-data jc)))
    (or (not (memq (plist-get state :blocking-status) '(pending failed)))
        (equal (jabber-jid-user jid)
               (concat (plist-get state :username) "@"
                       (plist-get state :server))))))

(defun jabber-blocking--ready (jc status)
  "Mark blocking discovery on JC as STATUS and release background work."
  (plist-put (fsm-get-state-data jc) :blocking-status status)
  (run-hook-with-args 'jabber-blocking-ready-hook jc))

(defun jabber-blocking-blocked-p (jc jid)
  "Return non-nil if JID is known to be blocked on connection JC.
Match full JIDs, bare JIDs, domains and domain/resource entries per
XEP-0191.  Never block communication with the account's own resources.
Unknown blocklists are not treated as evidence of a block."
  (when (stringp jid)
    (let* ((state (fsm-get-state-data jc))
           (bare (downcase (jabber-jid-user jid)))
           (domain (car (last (split-string bare "@"))))
           (resource (jabber-jid-resource jid))
           (own (downcase (concat (plist-get state :username) "@"
                                  (plist-get state :server)))))
      (and (not (equal bare own))
           (cl-some
            (lambda (entry)
              (let ((blocked (downcase (jabber-jid-user entry)))
                    (blocked-resource (jabber-jid-resource entry)))
                (and (or (equal blocked bare) (equal blocked domain))
                     (or (null blocked-resource)
                         (equal blocked-resource resource)))))
            (plist-get state :blocking-list))))))

(defun jabber-blocking--valid-items-p (query)
  "Return non-nil if every item in QUERY has a nonempty JID."
  (cl-every (lambda (item)
              (let ((jid (jabber-xml-get-attribute item 'jid)))
                (and (stringp jid) (not (string-empty-p jid)))))
            (jabber-xml-get-children query 'item)))

(defun jabber-blocking--set-list (state jids)
  "Store blocked JIDS in connection STATE and invalidate older snapshots."
  (plist-put state :blocking-list jids)
  (plist-put state :blocking-revision (list nil)))

(defun jabber-blocking--session-predicate (jc)
  "Return a predicate checking JC's captured connection and session.
FSM state plists may be replaced during ordinary Stream Management work.
Capture lifecycle values instead, and always consult the current plist."
  (let* ((state (fsm-get-state-data jc))
         (connection (plist-get state :connection))
         (stream (plist-get state :session-id))
         (session (plist-get state :blocking-session))
         (username (plist-get state :username))
         (server (plist-get state :server)))
    (lambda ()
      (let ((current (fsm-get-state-data jc)))
        (and (eq connection (plist-get current :connection))
             (equal stream (plist-get current :session-id))
             (eq session (plist-get current :blocking-session))
             (equal username (plist-get current :username))
             (equal server (plist-get current :server)))))))

(defun jabber-blocking--response-predicate (jc kind)
  "Return an IQ admission predicate for JC's blocking request of KIND.
KIND is `disco', `snapshot' or `command'.  Reject foreign senders,
connections and retired sessions before they can consume a pending IQ."
  (let* ((state (fsm-get-state-data jc))
         (current-p (jabber-blocking--session-predicate jc))
         (server (plist-get state :server))
         (bare (concat (plist-get state :username) "@" server)))
    (lambda (received-jc xml)
      (let ((from (jabber-xml-get-attribute xml 'from))
            (query (jabber-iq-query xml)))
        (and (eq received-jc jc)
             (funcall current-p)
             (or (null from) (equal from server)
                 (and (not (eq kind 'disco)) (equal from bare)))
             (if (equal (jabber-xml-get-attribute xml 'type) "error")
                 (jabber-iq-error xml)
               (pcase kind
                 ('disco
                  (and (eq (car-safe query) 'query)
                       (equal (jabber-xml-get-xmlns query) jabber-disco-xmlns-info)
                       (null (jabber-xml-get-attribute query 'node))))
                 ('snapshot
                  (and (eq (car-safe query) 'blocklist)
                       (equal (jabber-xml-get-xmlns query) jabber-blocking-xmlns)
                       (jabber-blocking--valid-items-p query)))
                 ('command (null query)))))))))

(defun jabber-blocking--fetch (jc &optional callback)
  "Synchronize JC's blocklist, then call CALLBACK with the result stanza.
Retain known state on failure.  Retry snapshots overtaken by a push."
  (let* ((state (fsm-get-state-data jc))
         (revision (plist-get state :blocking-revision))
         (current-p (jabber-blocking--session-predicate jc)))
    (jabber-send-iq
     jc nil "get" `(blocklist ((xmlns . ,jabber-blocking-xmlns)))
     (lambda (_jc xml _ctx)
       (when (funcall current-p)
         (let ((state (fsm-get-state-data jc)))
           (if (not (eq revision (plist-get state :blocking-revision)))
               (jabber-blocking--fetch jc callback)
             (let ((query (car (jabber-xml-get-children xml 'blocklist))))
               (when (and (equal (jabber-xml-get-xmlns query) jabber-blocking-xmlns)
                          (jabber-blocking--valid-items-p query))
		 (jabber-blocking--set-list
                  state (jabber-blocking--item-jids query))
		 (jabber-blocking--ready jc 'ready)
		 (when callback (funcall callback jc xml nil))))))))
     nil
     (lambda (_jc xml _ctx)
       (when (funcall current-p)
         (let ((state (fsm-get-state-data jc)))
           (when (and (eq revision (plist-get state :blocking-revision))
                      (not (eq (plist-get state :blocking-status) 'ready)))
             (plist-put state :blocking-status 'failed))
           (message "Failed to retrieve blocklist: %s"
                    (jabber-parse-error (jabber-iq-error xml))))))
     nil nil (jabber-blocking--response-predicate jc 'snapshot))))

(defun jabber-blocking--on-connect (jc)
  "Discover blocking support and synchronize the blocklist on JC."
  (let* ((state (fsm-get-state-data jc))
         (session (list nil)))
    (plist-put state :blocking-session session)
    (plist-put state :blocking-status 'pending)
    (let ((current-p (jabber-blocking--session-predicate jc)))
      (jabber-disco-get-info
       jc (plist-get state :server) nil
       (lambda (_jc _ctx info)
	 (when (funcall current-p)
           (let ((state (fsm-get-state-data jc)))
             (cond
              ((eq (car info) 'error)
               (plist-put state :blocking-status 'failed))
              ((member jabber-blocking-xmlns (cadr info))
               (jabber-blocking--fetch jc))
              (t
               (jabber-blocking--set-list state nil)
               (jabber-blocking--ready jc 'unsupported))))))
       nil t (jabber-blocking--response-predicate jc 'disco)))))

(defun jabber-blocking--item-jids (query)
  "Return the JIDs in blocking QUERY."
  (delq nil
        (mapcar (lambda (item)
                  (jabber-xml-get-attribute item 'jid))
                (jabber-xml-get-children query 'item))))

(defun jabber-blocking--apply-push (current query)
  "Apply blocking push QUERY to CURRENT and return the new list."
  (pcase (jabber-xml-node-name query)
    ('block
     (delete-dups (append current (jabber-blocking--item-jids query))))
    ('unblock
     (let ((jids (jabber-blocking--item-jids query)))
       (if (jabber-xml-get-children query 'item)
           (cl-remove-if (lambda (jid) (member jid jids)) current)
         nil)))
    (_ current)))

(defun jabber-blocking--valid-push-p (from state-data)
  "Return non-nil when FROM can push blocking state for STATE-DATA."
  (let ((bare (concat (plist-get state-data :username) "@"
                      (plist-get state-data :server))))
    (or (null from)
        (string= from bare))))

(defun jabber-blocking--process-push (jc xml-data)
  "Process an XEP-0191 blocking push in XML-DATA on JC."
  (let* ((state-data (fsm-get-state-data jc))
         (from (jabber-xml-get-attribute xml-data 'from))
         (id (jabber-xml-get-attribute xml-data 'id))
         (query (jabber-iq-query xml-data)))
    (when (and (equal (jabber-xml-get-attribute xml-data 'type) "set")
               (equal (jabber-xml-get-xmlns query) jabber-blocking-xmlns)
               (jabber-blocking--valid-items-p query)
               (jabber-blocking--valid-push-p from state-data)
               (memq (jabber-xml-node-name query) '(block unblock)))
      (jabber-blocking--set-list
       state-data (jabber-blocking--apply-push
                   (plist-get state-data :blocking-list) query))
      (jabber-send-iq jc from "result" nil nil nil nil nil id))))

(defun jabber-blocking--change (jc jid action)
  "Ask JC's server to apply blocking ACTION to JID.
Refresh the authoritative snapshot after success instead of replaying a
possibly stale command over newer pushes."
  (let ((current-p (jabber-blocking--session-predicate jc)))
    (jabber-send-iq
     jc nil "set"
     `(,action ((xmlns . ,jabber-blocking-xmlns)) (item ((jid . ,jid))))
     (lambda (_jc _xml _ctx)
       (when (funcall current-p)
         (jabber-blocking--fetch jc)
         (message "%s %s" (if (eq action 'block) "Blocked" "Unblocked") jid)))
     nil
     (lambda (_jc xml _ctx)
       (message "Failed to %s %s: %s" action jid
                (jabber-parse-error (jabber-iq-error xml))))
     nil nil (jabber-blocking--response-predicate jc 'command))))

;;;###autoload
(defun jabber-blocking-block-jid (jc jid)
  "Block JID on connection JC.

JC is the Jabber connection.  JID is the bare JID to block."
  (interactive
   (let ((jc (jabber-read-account)))
     (list jc (jabber-read-jid-completing "Block JID: "))))
  (jabber-blocking--change jc jid 'block))

;;;###autoload
(defun jabber-blocking-unblock-jid (jc jid)
  "Unblock JID on connection JC.

JC is the Jabber connection.  JID is the bare JID to unblock."
  (interactive
   (let ((jc (jabber-read-account)))
     (list jc (jabber-read-jid-completing "Unblock JID: "))))
  (jabber-blocking--change jc jid 'unblock))

;;;###autoload
(defun jabber-blocking-list (jc)
  "Retrieve and display the blocklist for connection JC.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-blocking--fetch jc #'jabber-blocking--display-list))

(defun jabber-blocking--display-list (jc xml-data _closure)
  "Display the blocklist from XML-DATA.
JC is the Jabber connection."
  (let* ((blocklist (jabber-xml-get-children
                     (car (jabber-xml-get-children xml-data 'blocklist))
                     'item))
         (jids (mapcar (lambda (item)
                         (jabber-xml-get-attribute item 'jid))
                       blocklist)))
    (if (null jids)
        (message "Blocklist for %s is empty"
                 (jabber-connection-bare-jid jc))
      (with-output-to-temp-buffer "*jabber-blocklist*"
        (princ (format "Blocklist for %s:\n\n"
                       (jabber-connection-bare-jid jc)))
        (dolist (jid jids)
          (princ (format "  %s\n" jid)))))))

;;;###autoload
(defun jabber-blocking-block-chat-peer (jc)
  "Block the JID of the current chat buffer.
JC is the Jabber connection."
  (interactive (list jabber-buffer-connection))
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (let ((jid (jabber-jid-user jabber-chatting-with)))
    (when (yes-or-no-p (format "Block %s? " jid))
      (jabber-blocking-block-jid jc jid))))

;;;###autoload
(defun jabber-blocking-toggle-chat-peer (jc)
  "Toggle block state of the JID in the current chat buffer.
Fetches the blocklist from the server, then blocks or unblocks
accordingly.  JC is the Jabber connection."
  (interactive (list jabber-buffer-connection))
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (let ((jid (jabber-jid-user jabber-chatting-with)))
    (jabber-blocking--fetch
     jc (lambda (jc xml-data _ctx)
          (jabber-blocking--toggle jc xml-data jid)))))

(defun jabber-blocking--toggle (jc xml-data jid)
  "Block or unblock JID based on current blocklist in XML-DATA.
JC is the Jabber connection."
  (let* ((blocklist (jabber-xml-get-children
                     (car (jabber-xml-get-children xml-data 'blocklist))
                     'item))
         (blocked-jids (mapcar (lambda (item)
                                 (jabber-xml-get-attribute item 'jid))
                               blocklist))
         (blocked-p (member jid blocked-jids)))
    (if blocked-p
        (when (yes-or-no-p (format "Unblock %s? " jid))
          (jabber-blocking-unblock-jid jc jid))
      (when (yes-or-no-p (format "Block %s? " jid))
        (jabber-blocking-block-jid jc jid)))))

(add-to-list 'jabber-iq-set-xmlns-alist
             (cons jabber-blocking-xmlns #'jabber-blocking--process-push))
(jabber-disco-advertise-feature jabber-blocking-xmlns)

(add-hook 'jabber-post-connect-hooks #'jabber-blocking--on-connect -90)

(provide 'jabber-blocking)

;;; jabber-blocking.el ends here
