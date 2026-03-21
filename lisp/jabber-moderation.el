;;; jabber-moderation.el --- XEP-0425: Moderated Message Retraction  -*- lexical-binding: t; -*-

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
;; Implements XEP-0425 (Moderated Message Retraction) send/receive and
;; uses the XEP-0424 <retract> element on incoming stanzas.  When a MUC
;; moderator retracts a message, the original is replaced with a tombstone
;; in the chat buffer.

;;; Code:

(require 'ewoc)
(require 'jabber-xml)
(require 'jabber-disco)
(require 'jabber-iq)

(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-jid-resource "jabber-util" (jid))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-chat-ewoc-find-by-id "jabber-chatbuffer" (stanza-id))
(declare-function jabber-send-iq "jabber-iq"
                  (jc to type query success-callback success-closure-data
                   error-callback error-closure-data))
(declare-function jabber-report-success "jabber-util" (_jc xml-data context))
(declare-function jabber-db-retract-message "jabber-db" (server-id retracted-by))

(defvar jabber-message-chain)           ; jabber-core.el
(defvar jabber-chat-ewoc)              ; jabber-chatbuffer.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

(defconst jabber-moderation-xmlns "urn:xmpp:message-moderate:1"
  "XML namespace for XEP-0425 Message Moderation.")

(defconst jabber-moderation-retract-xmlns "urn:xmpp:message-retract:1"
  "XML namespace for XEP-0424 Message Retraction.")

(defun jabber-moderation--handle-message (_jc xml-data)
  "Handle moderated message retraction in XML-DATA.
If the stanza contains a <retract> with a <moderated> child, look up
the original message in the MUC buffer and replace it with a tombstone."
  (when-let* ((type (jabber-xml-get-attribute xml-data 'type))
              ((string= type "groupchat"))
              (retract (jabber-xml-child-with-xmlns
                        xml-data jabber-moderation-retract-xmlns))
              (moderated (car (jabber-xml-get-children retract 'moderated)))
              (stanza-id (jabber-xml-get-attribute retract 'id))
              (from (jabber-xml-get-attribute xml-data 'from))
              (room (jabber-jid-user from))
              ;; Only accept retractions from the MUC service itself
              ;; (bare room JID, no resource).
              ((not (jabber-jid-resource from))))
    (let* ((moderator (jabber-xml-get-attribute moderated 'by))
           (reason-el (car (jabber-xml-get-children retract 'reason)))
           (reason (car (jabber-xml-node-children reason-el)))
           (buf (jabber-muc-find-buffer room)))
      (jabber-db-retract-message stanza-id moderator)
      (when buf
        (with-current-buffer buf
          (when-let* ((node (jabber-chat-ewoc-find-by-id stanza-id))
                      (data (ewoc-data node))
                      (msg (cadr data))
                      ;; XEP-0425 §5 MUST: only process if the ID is the
                      ;; server-assigned stanza-id (MUC-assigned via XEP-0359),
                      ;; not a client-generated message id.
                      ((equal stanza-id (plist-get msg :server-id))))
            (setq msg (plist-put msg :retracted t))
            (setq msg (plist-put msg :retracted-by moderator))
            (setq msg (plist-put msg :retraction-reason reason))
            (setcar (cdr data) msg)
            (let ((inhibit-read-only t))
              (ewoc-invalidate jabber-chat-ewoc node))))))
    t))

(add-to-list 'jabber-message-chain #'jabber-moderation--handle-message)

;; XEP-0424: clients SHOULD advertise retract support so senders know we
;; handle tombstones.  The moderate namespace is a MUC-service feature
;; and MUST NOT be advertised by clients.
(jabber-disco-advertise-feature jabber-moderation-retract-xmlns)

(defun jabber-moderation-retract ()
  "Retract the MUC message at point via XEP-0425 moderation.
Sends a moderation IQ to the room requesting retraction of the
message under point.  Requires moderator privileges."
  (interactive)
  (unless (bound-and-true-p jabber-group)
    (user-error "Not in a MUC buffer"))
  (let* ((node (ewoc-locate jabber-chat-ewoc (point)))
         (data (and node (ewoc-data node)))
         (msg (and data (listp (cadr data)) (cadr data)))
         (server-id (and msg (plist-get msg :server-id))))
    (unless server-id
      (user-error "No server-assigned stanza ID on this message"))
    (let ((reason (read-string "Reason (empty for none): ")))
      (jabber-send-iq
       jabber-buffer-connection jabber-group "set"
       `(moderate ((id . ,server-id)
                   (xmlns . ,jabber-moderation-xmlns))
                  (retract ((xmlns . ,jabber-moderation-retract-xmlns)))
                  ,@(when (not (string-empty-p reason))
                      `((reason () ,reason))))
       #'jabber-report-success "Message retraction"
       #'jabber-report-success "Message retraction"))))

(provide 'jabber-moderation)
;;; jabber-moderation.el ends here
