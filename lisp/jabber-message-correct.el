;;; jabber-message-correct.el --- XEP-0308 Last Message Correction  -*- lexical-binding: t; -*-

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

;; Implements XEP-0308 Last Message Correction.
;;
;; A correction stanza is a normal <message> carrying a
;; <replace id='ORIGINAL-ID' xmlns='urn:xmpp:message-correct:0'/> child.
;; The receiver finds the original message by that id, replaces its body
;; in-place and marks it as edited.
;;
;; Send: `jabber-correct-last-message' (C-c C-e) re-sends your last
;; message with a <replace> child and updates the local buffer entry.

;;; Code:

(require 'jabber-xml)
(require 'jabber-chatbuffer)
(require 'jabber-db)

(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-chat-send "jabber-chat" (jc body &optional extra-elements))
(declare-function jabber-muc-send "jabber-muc" (jc body &optional extra-elements))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-connection-bare-jid "jabber-util" (jc))
(declare-function jabber-disco-advertise-feature "jabber-disco" (feature))
(declare-function jabber-db-message-sender-by-stanza-id "jabber-db" (stanza-id))

(defconst jabber-message-correct-xmlns "urn:xmpp:message-correct:0"
  "XML namespace for XEP-0308 Last Message Correction.")

;;; Parsing

(defun jabber-message-correct--replace-id (xml-data)
  "Return the id from the <replace> child of XML-DATA, or nil."
  (when-let* ((el (jabber-xml-child-with-xmlns xml-data
                                               jabber-message-correct-xmlns)))
    (jabber-xml-get-attribute el 'id)))

;;; Sender validation

(defun jabber-message-correct--valid-sender-p (original-from new-from muc-p)
  "Return non-nil if NEW-FROM may correct a message from ORIGINAL-FROM.
MUC-P non-nil means full-JID comparison; otherwise bare-JID comparison."
  (if muc-p
      (string= original-from new-from)
    (string= (jabber-jid-user original-from)
             (jabber-jid-user new-from))))

;;; Apply correction

(defun jabber-message-correct--apply (replace-id new-body new-from muc-p buffer)
  "Apply correction REPLACE-ID with NEW-BODY sent by NEW-FROM.
MUC-P non-nil for groupchat.  BUFFER is the chat buffer or nil.
Validates sender against the stored original message (via DB lookup)
before writing.  If the original is not in the DB the correction is
dropped.  Returns non-nil when the correction was accepted."
  (let ((original-from (jabber-db-message-sender-by-stanza-id replace-id)))
    (cond
     ((null original-from)
      (message "XEP-0308: correction for unknown message %s dropped" replace-id)
      nil)
     ((not (jabber-message-correct--valid-sender-p original-from new-from muc-p))
      (message "XEP-0308: rejected correction from %s for message by %s"
               new-from original-from)
      nil)
     (t
      (jabber-db-correct-message replace-id new-body)
      (when buffer
        (with-current-buffer buffer
          (when-let* ((node (jabber-chat-ewoc-find-by-id replace-id))
                      (data (ewoc-data node))
                      (msg  (cadr data)))
            (setq msg (plist-put msg :body new-body))
            (setq msg (plist-put msg :edited t))
            (setcar (cdr data) msg)
            (let ((inhibit-read-only t))
              (ewoc-invalidate jabber-chat-ewoc node)))))
      t))))

;;; Inhibit DB storage of correction stanzas

(defun jabber-message-correct--inhibit (_jc xml-data)
  "Return non-nil to prevent logging XML-DATA as a new message."
  (not (null (jabber-message-correct--replace-id xml-data))))

(add-to-list 'jabber-history-inhibit-received-message-functions
             #'jabber-message-correct--inhibit)

;;; Disco feature advertisement

(jabber-disco-advertise-feature jabber-message-correct-xmlns)

;;; Find last sent message (pure)

(defun jabber-message-correct--find-last-sent (ewoc)
  "Return (NODE ID BODY) for the last sent message in EWOC, or nil."
  (let (result (node (ewoc-nth ewoc -1)))
    (while (and node (not result))
      (pcase-let ((`(,type ,msg) (ewoc-data node)))
        (when (and (memq type '(:local :muc-local))
                   (listp msg)
                   (plist-get msg :id))
          (setq result (list node
                             (plist-get msg :id)
                             (or (plist-get msg :body) "")))))
      (setq node (ewoc-prev ewoc node)))
    result))

;;; Build replace element (pure)

(defun jabber-message-correct--replace-element (stanza-id)
  "Return a <replace> XML element referencing STANZA-ID."
  `(replace ((id . ,stanza-id)
             (xmlns . ,jabber-message-correct-xmlns))))

;;; Update ewoc entry in-place

(defun jabber-message-correct--update-ewoc (ewoc node new-body)
  "Update NODE in EWOC with NEW-BODY and mark as edited."
  (let* ((data (ewoc-data node))
         (msg  (cadr data)))
    (setq msg (plist-put msg :body new-body))
    (setq msg (plist-put msg :edited t))
    (setcar (cdr data) msg)
    (let ((inhibit-read-only t))
      (ewoc-invalidate ewoc node))))

;;; Interactive command

(defun jabber-correct-last-message ()
  "Correct the last sent message in this chat buffer.
Prompts with the existing body pre-filled."
  (interactive)
  (pcase (jabber-message-correct--find-last-sent jabber-chat-ewoc)
    ('nil (user-error "No sent message found to correct"))
    (`(,node ,id ,body)
     (let ((new-body (read-string "Correction: " body)))
       (when (string= new-body body)
         (user-error "No change"))
       (jabber-message-correct--update-ewoc jabber-chat-ewoc node new-body)
       (jabber-db-correct-message id new-body)
       (let ((replace-el (jabber-message-correct--replace-element id))
             (muc-p (bound-and-true-p jabber-group)))
         (if muc-p
             (jabber-muc-send jabber-buffer-connection new-body
                              (list replace-el))
           (jabber-chat-send jabber-buffer-connection new-body
                             (list replace-el))))))))

(provide 'jabber-message-correct)
;;; jabber-message-correct.el ends here
