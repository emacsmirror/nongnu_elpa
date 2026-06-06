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
(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-muc)
(require 'jabber-db)
(require 'cl-lib)

(defvar jabber-chat-ewoc)              ; jabber-chatbuffer.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

(defconst jabber-moderation-xmlns "urn:xmpp:message-moderate:1"
  "XML namespace for XEP-0425 Message Moderation.")

(defconst jabber-moderation-retract-xmlns "urn:xmpp:message-retract:1"
  "XML namespace for XEP-0424 Message Retraction.")

(defun jabber-moderation--child-with-name-and-xmlns (xml-data name xmlns)
  "Return the first child of XML-DATA with NAME and XMLNS."
  (cl-find-if (lambda (child)
                (and (listp child)
                     (eq (jabber-xml-node-name child) name)
                     (string= (jabber-xml-get-xmlns child) xmlns)))
              (jabber-xml-node-children xml-data)))

(defun jabber-moderation--retraction-element (xml-data)
  "Return (ELEMENT . TOMBSTONE-P) for a moderated retraction in XML-DATA."
  (or (when-let* ((retract (jabber-moderation--child-with-name-and-xmlns
                            xml-data 'retract
                            jabber-moderation-retract-xmlns)))
        (cons retract nil))
      (when-let* (((jabber-xml-get-attribute xml-data 'jabber-mam--origin))
                  (retracted (jabber-moderation--child-with-name-and-xmlns
                              xml-data 'retracted
                              jabber-moderation-retract-xmlns)))
        (cons retracted t))))

(defun jabber-moderation--moderator (xml-data moderated)
  "Return the moderator JID from MODERATED in XML-DATA, or nil."
  (or (jabber-xml-get-attribute moderated 'by)
      ;; Prosody sends the v0 <apply-to>/<moderated by="..."> with the
      ;; moderator JID but omits it from the v1 element.  Fall back to v0.
      (when-let* ((apply-to (jabber-xml-child-with-xmlns
                             xml-data "urn:xmpp:fasten:0"))
                  (mod-v0 (car (jabber-xml-get-children apply-to 'moderated))))
        (jabber-xml-get-attribute mod-v0 'by))))

(defun jabber-moderation--target-id (xml-data retraction tombstone-p)
  "Return the server id targeted by RETRACTION in XML-DATA.
TOMBSTONE-P non-nil means RETRACTION is an archived <retracted/> element."
  (if tombstone-p
      (jabber-xml-get-attribute xml-data 'jabber-mam--archive-id)
    (jabber-xml-get-attribute retraction 'id)))

(defun jabber-moderation--valid-source-p (from tombstone-p)
  "Return non-nil if FROM may send this moderation stanza.
TOMBSTONE-P allows archived tombstones from the original occupant.  Live
moderation action stanzas must come from the bare MUC service."
  (or tombstone-p
      (not (jabber-jid-resource from))))

(defun jabber-moderation--handle-message (jc xml-data)
  "Handle moderated message retraction in XML-DATA.
Live <retract/> action stanzas update an existing message.  Archived
<retracted/> tombstones use the preserved MAM archive id as the original
server id.  JC is the connection the stanza arrived on."
  (when-let* ((type (jabber-xml-get-attribute xml-data 'type))
              ((string= type "groupchat"))
              (entry (jabber-moderation--retraction-element xml-data))
              (retraction (car entry))
              (moderated (car (jabber-xml-get-children retraction 'moderated)))
              (stanza-id (jabber-moderation--target-id
                          xml-data retraction (cdr entry)))
              (from (jabber-xml-get-attribute xml-data 'from))
              ((jabber-moderation--valid-source-p from (cdr entry)))
              (room (jabber-jid-user from)))
    (let* ((moderator (jabber-moderation--moderator xml-data moderated))
           (reason-el (car (jabber-xml-get-children retraction 'reason)))
           (reason (car (jabber-xml-node-children reason-el)))
           (buf (jabber-muc-find-buffer room)))
      (when moderator
        (jabber-db-retract-message-in-peer
         (jabber-connection-bare-jid jc) room stanza-id moderator reason))
      (when buf
        (with-current-buffer buf
          (jabber-moderation--mark-ewoc-retracted
           stanza-id moderator reason))))
    t))

(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-message-chain #'jabber-moderation--handle-message))

;; XEP-0424: clients SHOULD advertise retract support so senders know we
;; handle tombstones.  The moderate namespace is a MUC-service feature
;; and MUST NOT be advertised by clients.
(jabber-disco-advertise-feature jabber-moderation-retract-xmlns)

(defun jabber-moderation--mark-ewoc-retracted (server-id retracted-by reason)
  "Mark the ewoc node with SERVER-ID as retracted in the current buffer.
RETRACTED-BY and REASON are stored on the message plist."
  (when-let* ((node (jabber-chat-ewoc-find-by-id server-id))
              (data (ewoc-data node))
              (msg (cadr data))
              ((equal server-id (plist-get msg :server-id))))
    (setq msg (plist-put msg :retracted t))
    (setq msg (plist-put msg :retracted-by retracted-by))
    (setq msg (plist-put msg :retraction-reason reason))
    (setcar (cdr data) msg)
    (jabber-chat-ewoc-invalidate node)))

(defun jabber-moderation--mark-local-retracted (_jc _xml-data data)
  "Mark the moderated message in DATA as retracted locally."
  (pcase-let ((`(,room ,server-id ,moderator ,reason) data))
    (jabber-db-retract-message server-id moderator reason)
    (when-let* ((buf (jabber-muc-find-buffer room)))
      (with-current-buffer buf
        (jabber-moderation--mark-ewoc-retracted server-id moderator reason)))))

(defun jabber-moderation--send-retract (jc room server-id &optional reason)
  "Send a moderation IQ to retract SERVER-ID in ROOM on JC.
Marks the message as retracted locally after the MUC accepts the IQ.
Optional REASON is a human-readable string."
  (let ((moderator (concat room "/" (jabber-muc-nickname room jc))))
    (jabber-send-iq
     jc room "set"
     `(moderate ((id . ,server-id)
                 (xmlns . ,jabber-moderation-xmlns))
                (retract ((xmlns . ,jabber-moderation-retract-xmlns)))
                ,@(when (and reason (not (string-empty-p reason)))
                    `((reason () ,reason))))
     #'jabber-moderation--mark-local-retracted
     (list room server-id moderator reason)
     #'jabber-report-success "Message retraction")))

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
      (jabber-moderation--send-retract
       jabber-buffer-connection jabber-group server-id reason))))

(defun jabber-moderation-retract-by-occupant ()
  "Retract all MUC messages from the occupant at point.
Uses XEP-0421 occupant-id to find all messages, sends
individual moderation IQs for each."
  (interactive)
  (unless (bound-and-true-p jabber-group)
    (user-error "Not in a MUC buffer"))
  (let* ((node (ewoc-locate jabber-chat-ewoc (point)))
         (data (and node (ewoc-data node)))
         (msg (and data (listp (cadr data)) (cadr data)))
         (server-id (and msg (plist-get msg :server-id))))
    (unless server-id
      (user-error "No server-assigned stanza ID on this message"))
    (let ((occupant-id (jabber-db-occupant-id-by-server-id server-id)))
      (unless occupant-id
        (user-error "No occupant-id for this message"))
      (let* ((account (jabber-connection-bare-jid jabber-buffer-connection))
             (ids (jabber-db-server-ids-by-occupant-id
                   account jabber-group occupant-id))
             (count (length ids)))
        (unless ids
          (user-error "No retractable messages for this occupant"))
        (when (y-or-n-p (format "Retract %d message%s from this occupant? "
                                count (if (= count 1) "" "s")))
          (let ((reason (read-string "Reason (empty for none): ")))
            (dolist (id ids)
              (jabber-moderation--send-retract
               jabber-buffer-connection jabber-group id reason))
            (message "Sent %d retraction request%s"
                     count (if (= count 1) "" "s"))))))))

(provide 'jabber-moderation)
;;; jabber-moderation.el ends here
