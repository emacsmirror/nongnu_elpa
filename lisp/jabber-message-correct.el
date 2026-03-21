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
(declare-function jabber-send-sexp "jabber-core" (jc sexp))
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
silently dropped.  Returns non-nil when the correction was accepted."
  (when-let* ((original-from (jabber-db-message-sender-by-stanza-id replace-id))
              ((jabber-message-correct--valid-sender-p original-from new-from muc-p)))
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
    t))

;;; Inhibit DB storage of correction stanzas

(defun jabber-message-correct--inhibit (_jc xml-data)
  "Return non-nil to prevent logging XML-DATA as a new message."
  (not (null (jabber-message-correct--replace-id xml-data))))

(add-to-list 'jabber-history-inhibit-received-message-functions
             #'jabber-message-correct--inhibit)

;;; Disco feature advertisement

(jabber-disco-advertise-feature jabber-message-correct-xmlns)

;;; Interactive command

(defun jabber-correct-last-message ()
  "Correct the last sent message in this chat buffer.
Prompts with the existing body pre-filled."
  (interactive)
  (let (last-node last-id last-body node)
    (setq node (ewoc-nth jabber-chat-ewoc -1))
    (while (and node (not last-node))
      (let* ((data (ewoc-data node))
             (type (car data))
             (msg  (cadr data)))
        (when (and (memq type '(:local :muc-local))
                   (listp msg)
                   (plist-get msg :id))
          (setq last-node node
                last-id   (plist-get msg :id)
                last-body (or (plist-get msg :body) ""))))
      (setq node (ewoc-prev jabber-chat-ewoc node)))
    (unless last-id
      (user-error "No sent message found to correct"))
    (let ((new-body (read-string "Correction: " last-body)))
      (when (string= new-body last-body)
        (user-error "No change"))
      (let* ((id    (format "emacs-msg-%.6f" (float-time)))
             (muc-p (bound-and-true-p jabber-group))
             (type  (if muc-p "groupchat" "chat"))
             (to    (if muc-p jabber-group jabber-chatting-with)))
        (jabber-send-sexp jabber-buffer-connection
                          `(message ((to . ,to) (type . ,type) (id . ,id))
                                    (body () ,new-body)
                                    (replace ((id . ,last-id)
                                              (xmlns . ,jabber-message-correct-xmlns)))))
        (let* ((data (ewoc-data last-node))
               (msg  (cadr data)))
          (setq msg (plist-put msg :body new-body))
          (setq msg (plist-put msg :edited t))
          (setcar (cdr data) msg)
          (let ((inhibit-read-only t))
            (ewoc-invalidate jabber-chat-ewoc last-node)))
        (jabber-db-correct-message last-id new-body)))))

(provide 'jabber-message-correct)
;;; jabber-message-correct.el ends here
