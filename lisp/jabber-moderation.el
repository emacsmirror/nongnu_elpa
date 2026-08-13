;;; jabber-moderation.el --- XEP-0424/0425 Message Retraction  -*- lexical-binding: t; -*-

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
;; Implements incoming MUC author retractions from XEP-0424 in rooms with
;; trusted XEP-0421 occupant identifiers, plus moderated retractions from
;; XEP-0425.  Accepted retractions replace the original message with a
;; tombstone in storage and live chat buffers.

;;; Code:

(require 'ewoc)
(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-muc)
(require 'jabber-db)
(require 'jabber-message-thread)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar jabber-chat-ewoc)              ; jabber-chatbuffer.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

(defconst jabber-moderation-xmlns "urn:xmpp:message-moderate:1"
  "XML namespace for XEP-0425 Message Moderation.")

(defconst jabber-moderation-retract-xmlns "urn:xmpp:message-retract:1"
  "XML namespace for XEP-0424 Message Retraction.")

(defconst jabber-moderation-occupant-id-xmlns "urn:xmpp:occupant-id:0"
  "XML namespace for XEP-0421 MUC occupant identifiers.")

(defun jabber-moderation--child-with-name-and-xmlns (xml-data name xmlns)
  "Return the first child of XML-DATA with NAME and XMLNS."
  (cl-find-if (lambda (child)
                (and (listp child)
                     (eq (jabber-xml-node-name child) name)
                     (string= (jabber-xml-get-xmlns child) xmlns)))
              (jabber-xml-node-children xml-data)))

(defun jabber-moderation--children-with-name-and-xmlns (xml-data name xmlns)
  "Return child elements of XML-DATA with NAME and XMLNS."
  (cl-remove-if-not
   (lambda (child)
     (and (listp child)
          (eq (jabber-xml-node-name child) name)
          (string= (jabber-xml-get-xmlns child) xmlns)))
   (jabber-xml-node-children xml-data)))

(defun jabber-moderation--single-child-with-name-and-xmlns
    (xml-data name xmlns)
  "Return XML-DATA's sole child with NAME and XMLNS, or nil."
  (let ((children
         (jabber-moderation--children-with-name-and-xmlns
          xml-data name xmlns)))
    (and (null (cdr children)) (car children))))

(defun jabber-moderation--muc-retraction-message-p (xml-data)
  "Return non-nil when XML-DATA contains a MUC XEP-0424 retraction.
This intentionally does not require a valid target.  XEP-0424 forbids
displaying sender-controlled fallback bodies even for unknown targets."
  (and (string= (or (jabber-xml-get-attribute xml-data 'type) "")
                "groupchat")
       (jabber-moderation--child-with-name-and-xmlns
        xml-data 'retract jabber-moderation-retract-xmlns)))

(defun jabber-moderation--history-inhibit-p (_jc xml-data)
  "Return non-nil when XML-DATA is a MUC retraction action stanza."
  (jabber-moderation--muc-retraction-message-p xml-data))

(defun jabber-moderation--room-supports-occupant-id-p (room)
  "Return non-nil when cached disco info says ROOM supports XEP-0421."
  (member jabber-moderation-occupant-id-xmlns
          (nth 1 (jabber-disco-get-info-immediately room nil))))

(defun jabber-moderation--single-occupant-id (xml-data)
  "Return XML-DATA's one valid XEP-0421 occupant ID, or nil."
  (when-let* ((element
               (jabber-moderation--single-child-with-name-and-xmlns
                xml-data 'occupant-id jabber-moderation-occupant-id-xmlns))
              (occupant-id (jabber-xml-get-attribute element 'id))
              ((not (string-empty-p occupant-id)))
              ((<= (length occupant-id) 128)))
    occupant-id))

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

(defun jabber-moderation--target-buffers (jc room server-id)
  "Return live buffers containing SERVER-ID in ROOM on JC."
  (let ((thread-targets
         (jabber-message-thread-update-targets
          jc room "groupchat" server-id t)))
    (cond
     ((eq thread-targets 'closed) nil)
     (thread-targets thread-targets)
     (t (delq nil (list (jabber-muc-find-buffer room jc)))))))

(defun jabber-moderation--target-buffers-for-row (jc room row-id)
  "Return live buffers containing exact message ROW-ID in ROOM on JC."
  (let ((thread-targets
         (jabber-message-thread-update-targets-for-row
          jc room "groupchat" row-id)))
    (cond
     ((eq thread-targets 'closed) nil)
     (thread-targets thread-targets)
     (t (delq nil (list (jabber-muc-find-buffer room jc)))))))

(defun jabber-moderation--author-candidate (candidates occupant-id)
  "Return the sole occupant-matched retraction target, or nil.
CANDIDATES all refer to one conversation-scoped ID.  OCCUPANT-ID
authorizes the sender."
  (let ((candidate (and (null (cdr candidates)) (car candidates))))
    (when (and candidate
               (jabber-jid-resource (plist-get candidate :from))
               (equal occupant-id (plist-get candidate :occupant-id)))
      candidate)))

(defun jabber-moderation--live-author-candidates (jc room server-id)
  "Return live message candidates for SERVER-ID in ROOM on JC."
  (let (candidates)
    (dolist (buffer (buffer-list) (nreverse candidates))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (eq jabber-buffer-connection jc)
                     (equal (bound-and-true-p jabber-group) room)
                     jabber-chat-ewoc)
            (let ((node (ewoc-nth jabber-chat-ewoc 0)))
              (while node
                (let* ((data (ewoc-data node))
                       (msg (and (memq (car-safe data)
                                       '(:muc-local :muc-foreign))
                                 (cadr data))))
                  (when (and (listp msg)
                             (equal server-id (plist-get msg :server-id)))
                    (push (list :buffer buffer :node node
                                :thread-id
                                (bound-and-true-p jabber-message-thread-id)
                                :from (plist-get msg :from)
                                :occupant-id (plist-get msg :occupant-id)
                                :retracted (plist-get msg :retracted)
                                :retracted-by (plist-get msg :retracted-by)
                                :retraction-reason
                                (plist-get msg :retraction-reason))
                          candidates)))
                (setq node (ewoc-next jabber-chat-ewoc node))))))))))

(defun jabber-moderation--live-projection-set-p (candidates)
  "Return non-nil when CANDIDATES form at most one logical projection set."
  (and (<= (length candidates) 2)
       (<= (seq-count (lambda (candidate)
                        (null (plist-get candidate :thread-id)))
                      candidates)
           1)
       (<= (seq-count (lambda (candidate)
                        (plist-get candidate :thread-id))
                      candidates)
           1)))

(defun jabber-moderation--live-original (candidates occupant-id)
  "Return CANDIDATES' one logical target authorized by OCCUPANT-ID."
  (when (and candidates
             (jabber-moderation--live-projection-set-p candidates)
             (seq-every-p
              (lambda (candidate)
                (and (jabber-jid-resource (plist-get candidate :from))
                     (equal occupant-id
                            (plist-get candidate :occupant-id))
                     (not (plist-get candidate :retracted))
                     (not (plist-get candidate :retracted-by))))
              candidates))
    candidates))

(defun jabber-moderation--apply-live-author-retraction
    (candidates server-id from)
  "Retract live CANDIDATES identified by SERVER-ID and FROM."
  (dolist (candidate candidates)
    (with-current-buffer (plist-get candidate :buffer)
      (jabber-message-thread--mark-root-retracted server-id)
      (jabber-moderation--mark-node-retracted
       (plist-get candidate :node) from nil)))
  t)

(defun jabber-moderation--live-candidates-match-p
    (candidates original occupant-id)
  "Return non-nil when CANDIDATES agree with ORIGINAL and OCCUPANT-ID."
  (and (or (null candidates)
           (and (jabber-moderation--live-projection-set-p candidates)
                (seq-every-p
                 (lambda (candidate)
                   (equal occupant-id
                          (plist-get candidate :occupant-id)))
                 candidates)))
       (seq-every-p
        (lambda (candidate)
          (equal (plist-get candidate :from)
                 (plist-get original :from)))
        candidates)))

(defun jabber-moderation--handle-author-retraction (jc xml-data retraction)
  "Apply an XEP-0424 MUC author RETRACTION from XML-DATA on JC."
  (when-let* ((from (jabber-xml-get-attribute xml-data 'from))
              ((jabber-jid-resource from))
              (room (jabber-jid-user from))
              ((jabber-moderation--room-supports-occupant-id-p room))
              (occupant-id (jabber-moderation--single-occupant-id xml-data))
              (server-id (jabber-xml-get-attribute retraction 'id))
              ((not (string-empty-p server-id)))
              (account (jabber-connection-bare-jid jc)))
    (let ((stored-candidates
           (jabber-db-message-retraction-candidates
            account room server-id))
          (live-candidates
           (jabber-moderation--live-author-candidates
            jc room server-id)))
      (if stored-candidates
          (when-let* ((candidate
                       (jabber-moderation--author-candidate
                        stored-candidates occupant-id))
                      ((jabber-moderation--live-candidates-match-p
                        live-candidates candidate occupant-id)))
            (if (plist-get candidate :retracted-by)
                (progn
                  (dolist (buffer
                           (jabber-moderation--target-buffers-for-row
                            jc room (plist-get candidate :row-id)))
                    (with-current-buffer buffer
                      (jabber-moderation--mark-ewoc-retracted
                       server-id
                       (plist-get candidate :retracted-by)
                       (plist-get candidate :retraction-reason)
                       (plist-get candidate :from))))
                  t)
              (when (jabber-db-retract-message-row
                     (plist-get candidate :row-id) from)
                (dolist (buffer
                         (jabber-moderation--target-buffers-for-row
                          jc room (plist-get candidate :row-id)))
                  (with-current-buffer buffer
                    (jabber-moderation--mark-ewoc-retracted
                     server-id from nil (plist-get candidate :from))))
                t)))
        (when-let* ((candidates
                     (jabber-moderation--live-original
                      live-candidates occupant-id)))
          (jabber-moderation--apply-live-author-retraction
           candidates server-id from))))))

(defun jabber-moderation--handle-message (jc xml-data)
  "Handle MUC message retraction in XML-DATA.
Live <retract/> action stanzas update an existing message.  Archived
<retracted/> tombstones use the preserved MAM archive id as the original
server id.  JC is the connection the stanza arrived on."
  (when-let* ((type (jabber-xml-get-attribute xml-data 'type))
              ((string= type "groupchat"))
              (entry (jabber-moderation--retraction-element xml-data))
              (retraction (car entry)))
    (if-let* ((moderated
               (car (jabber-xml-get-children retraction 'moderated))))
        (when-let* ((stanza-id (jabber-moderation--target-id
                                xml-data retraction (cdr entry)))
                    (from (jabber-xml-get-attribute xml-data 'from))
                    ((jabber-moderation--valid-source-p from (cdr entry)))
                    (room (jabber-jid-user from)))
          (let* ((moderator (jabber-moderation--moderator xml-data moderated))
                 (reason-el (car (jabber-xml-get-children retraction 'reason)))
                 (reason (car (jabber-xml-node-children reason-el)))
                 (buffers
                  (jabber-moderation--target-buffers jc room stanza-id)))
            (when moderator
              (jabber-db-retract-message-in-peer
               (jabber-connection-bare-jid jc) room stanza-id moderator reason))
            (dolist (buffer buffers)
              (with-current-buffer buffer
                (jabber-moderation--mark-ewoc-retracted
                 stanza-id moderator reason)))
            t))
      (unless (cdr entry)
        (when-let* ((single-retraction
                     (jabber-moderation--single-child-with-name-and-xmlns
                      xml-data 'retract jabber-moderation-retract-xmlns)))
          (jabber-moderation--handle-author-retraction
           jc xml-data single-retraction))))))

(jabber-chain-add 'jabber-message-chain #'jabber-moderation--handle-message)
(add-to-list 'jabber-history-inhibit-received-message-functions
             #'jabber-moderation--history-inhibit-p)

;; XEP-0424: clients SHOULD advertise retract support so senders know we
;; handle tombstones.  The moderate namespace is a MUC-service feature
;; and MUST NOT be advertised by clients.
(jabber-disco-advertise-feature jabber-moderation-retract-xmlns)

(defun jabber-moderation--mark-node-retracted (node retracted-by reason)
  "Mark EWOC NODE retracted by RETRACTED-BY for REASON."
  (when-let* ((data (ewoc-data node))
              (msg (cadr data)))
    (setq msg (plist-put msg :retracted t))
    (setq msg (plist-put msg :retracted-by retracted-by))
    (setq msg (plist-put msg :retraction-reason reason))
    (setcar (cdr data) msg)
    (jabber-chat-ewoc-invalidate node)))

(defun jabber-moderation--mark-ewoc-retracted
    (server-id retracted-by reason &optional original-from)
  "Mark the ewoc node with SERVER-ID as retracted in the current buffer.
RETRACTED-BY and REASON are stored on the message plist.  When
ORIGINAL-FROM is non-nil, require that full JID to match."
  (jabber-message-thread--mark-root-retracted server-id)
  (when-let* ((node
               (if original-from
                   (jabber-chat-ewoc-find-by-id-and-sender
                    server-id original-from)
                 (jabber-chat-ewoc-find-by-id server-id)))
              (data (ewoc-data node))
              (msg (cadr data))
              ((equal server-id (plist-get msg :server-id))))
    (jabber-moderation--mark-node-retracted node retracted-by reason)))

(defun jabber-moderation--mark-local-retracted (jc _xml-data data)
  "Mark the moderated message in DATA as retracted locally on JC."
  (pcase-let ((`(,room ,server-id ,moderator ,reason) data))
    (jabber-db-retract-message-in-peer
     (jabber-connection-bare-jid jc) room server-id moderator reason)
    (dolist (buffer (jabber-moderation--target-buffers jc room server-id))
      (with-current-buffer buffer
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
    (let* ((account
            (jabber-connection-bare-jid jabber-buffer-connection))
           (occupant-id
            (jabber-db-occupant-id-by-server-id-in-peer
             account jabber-group server-id)))
      (unless occupant-id
        (user-error "No occupant-id for this message"))
      (let* ((ids (jabber-db-server-ids-by-occupant-id
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
