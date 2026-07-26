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
(require 'jabber-util)
(require 'jabber-chatbuffer)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-message-reply)

(defconst jabber-message-correct-xmlns "urn:xmpp:message-correct:0"
  "XML namespace for XEP-0308 Last Message Correction.")

(defvar jabber-message-correct--muc-presence-sessions
  (make-hash-table :test #'equal)
  "Active legacy MUC participant lifetimes keyed by connection and sender.")

(defvar jabber-message-correct--muc-last-message-ids
  (make-hash-table :test #'equal)
  "Latest accepted legacy MUC message IDs by connection and sender.")

(defvar-local jabber-message-correct--pending-outgoing nil
  "Token for an OMEMO correction awaiting transport handoff.")

;;; Parsing

(defun jabber-message-correct--replace-id (xml-data)
  "Return the id from the <replace> child of XML-DATA, or nil."
  (when-let* ((el (jabber-xml-child-with-xmlns xml-data
                                               jabber-message-correct-xmlns))
              (replace-id (jabber-xml-get-attribute el 'id))
              ((not (string= replace-id
                             (or (jabber-xml-get-attribute xml-data 'id)
                                 "")))))
    replace-id))

;;; Sender validation

(defun jabber-message-correct--valid-sender-p
    (original-from new-from muc-p &optional original-occupant-id new-occupant-id)
  "Return non-nil if NEW-FROM may correct a message from ORIGINAL-FROM.
MUC-P non-nil means full-JID comparison unless ORIGINAL-OCCUPANT-ID
and NEW-OCCUPANT-ID are both available."
  (if muc-p
      (if (and original-occupant-id new-occupant-id)
          (string= original-occupant-id new-occupant-id)
        (string= original-from new-from))
    (string= (jabber-jid-user original-from)
             (jabber-jid-user new-from))))

(defun jabber-message-correct--muc-key (jc from)
  "Return the legacy MUC continuity key for JC and full JID FROM."
  (list jc from))

(defun jabber-message-correct--muc-presence-enter (jc from)
  "Start a participant lifetime for FROM observed on JC."
  (let ((key (jabber-message-correct--muc-key jc from)))
    (unless (gethash key jabber-message-correct--muc-presence-sessions)
      (remhash key jabber-message-correct--muc-last-message-ids)
      (puthash key t jabber-message-correct--muc-presence-sessions))))

(defun jabber-message-correct--muc-presence-leave (jc from)
  "End the participant lifetime for FROM observed on JC."
  (let ((key (jabber-message-correct--muc-key jc from)))
    (remhash key jabber-message-correct--muc-presence-sessions)
    (remhash key jabber-message-correct--muc-last-message-ids)))

(defun jabber-message-correct--record-muc-original (jc from stanza-id)
  "Record accepted STANZA-ID from legacy MUC sender FROM on JC."
  (let ((key (jabber-message-correct--muc-key jc from)))
    (when (gethash key jabber-message-correct--muc-presence-sessions)
      (puthash key stanza-id jabber-message-correct--muc-last-message-ids))))

(defun jabber-message-correct--muc-current-target-p (jc from stanza-id)
  "Return non-nil when STANZA-ID is FROM's latest live message on JC."
  (let ((key (jabber-message-correct--muc-key jc from)))
    (and (gethash key jabber-message-correct--muc-presence-sessions)
         (equal stanza-id
                (gethash key
                         jabber-message-correct--muc-last-message-ids)))))

(defun jabber-message-correct--muc-session-reset (jc)
  "Forget legacy MUC correction continuity belonging to JC."
  (dolist (table (list jabber-message-correct--muc-presence-sessions
                       jabber-message-correct--muc-last-message-ids))
    (maphash (lambda (key _value)
               (when (eq (car key) jc)
                 (remhash key table)))
             table)))

(defun jabber-message-correct--muc-room-leave (jc group)
  "Forget legacy MUC correction continuity for GROUP on JC."
  (dolist (table (list jabber-message-correct--muc-presence-sessions
                       jabber-message-correct--muc-last-message-ids))
    (maphash
     (lambda (key _value)
       (when (and (eq (car key) jc)
                  (equal group (jabber-jid-user (cadr key))))
         (remhash key table)))
     table)))

(add-hook 'jabber-lifecycle-session-reset-functions
          #'jabber-message-correct--muc-session-reset)

;;; Apply correction

(defun jabber-message-correct--matching-candidates
    (candidates new-from muc-p new-occupant-id legacy-authorized-p)
  "Return CANDIDATES that NEW-FROM may correct.
MUC-P, NEW-OCCUPANT-ID, and LEGACY-AUTHORIZED-P describe the
incoming correction."
  (seq-filter
   (lambda (candidate)
     (let ((original-from (plist-get candidate :from))
           (original-occupant-id (plist-get candidate :occupant-id)))
       (and (jabber-message-correct--valid-sender-p
             original-from new-from muc-p
             original-occupant-id new-occupant-id)
            (or (not muc-p)
                (and original-occupant-id new-occupant-id)
                (and (null original-occupant-id)
                     (null new-occupant-id)
                     legacy-authorized-p)))))
   candidates))

(defun jabber-message-correct--update-buffer
    (buffer muc-p replace-id original-from new-body)
  "Apply NEW-BODY to the matching message in BUFFER.
MUC-P selects lookup by REPLACE-ID and ORIGINAL-FROM."
  (when buffer
    (with-current-buffer buffer
      (when-let* ((node
                   (if muc-p
                       (jabber-chat-ewoc-find-by-id-and-sender
                        replace-id original-from)
                     (jabber-chat-ewoc-find-by-id replace-id)))
                  (data (ewoc-data node))
                  (msg (cadr data)))
        (setq msg (plist-put msg :body new-body))
        (setq msg (plist-put msg :edited t))
        (setcar (cdr data) msg)
        (jabber-chat-ewoc-invalidate node)))))

(defun jabber-message-correct--apply
    (replace-id new-body new-from muc-p buffer &optional new-occupant-id
                account peer legacy-authorized-p)
  "Apply correction REPLACE-ID with NEW-BODY sent by NEW-FROM.
MUC-P non-nil for groupchat.  BUFFER is the chat buffer or nil.
NEW-OCCUPANT-ID is the correction stanza's XEP-0421 occupant-id.
ACCOUNT and PEER scope persistence.  LEGACY-AUTHORIZED-P permits
the current-presence MUC fallback when occupant-id is unavailable.
Validates sender against the stored original message (via DB lookup)
before writing.  If the original is not in the DB the correction is
dropped.  Returns non-nil when the correction was accepted."
  (let* ((scoped-p (and account peer))
         (candidates
          (and scoped-p
               (jabber-db-message-correction-candidates
                account peer replace-id)))
         (matches
          (and scoped-p
               (jabber-message-correct--matching-candidates
                candidates new-from muc-p new-occupant-id
                legacy-authorized-p)))
         (original (and (= (length matches) 1) (car matches)))
         (original-from
          (if scoped-p
              (plist-get original :from)
            (jabber-db-message-sender-by-stanza-id replace-id)))
         (original-occupant-id
          (if scoped-p
              (plist-get original :occupant-id)
            (and muc-p
                 (jabber-db-occupant-id-by-stanza-id replace-id)))))
    (cond
     ;; A correction that failed to decrypt must never overwrite the
     ;; original body with the placeholder (issue #134).
     ((jabber--decrypt-failure-body-p new-body)
      (message "XEP-0308: dropped correction %s with undecryptable body"
               replace-id)
      nil)
     ((and scoped-p (/= (length matches) 1))
      (message "XEP-0308: correction target %s is missing or ambiguous"
               replace-id)
      nil)
     ((null original-from)
      (message "XEP-0308: correction for unknown message %s dropped" replace-id)
      nil)
     ((not (jabber-message-correct--valid-sender-p
            original-from new-from muc-p original-occupant-id new-occupant-id))
     (message "XEP-0308: rejected correction from %s for message by %s"
               new-from original-from)
      nil)
     (t
      (if scoped-p
          (jabber-db-correct-message-row
           (plist-get original :row-id) new-body)
        (jabber-db-correct-message replace-id new-body))
      (jabber-message-correct--update-buffer
       buffer muc-p replace-id original-from new-body)
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
  "Return (NODE ID BODY MSG) for the last sent message in EWOC, or nil."
  (let (result (node (ewoc-nth ewoc -1)))
    (while (and node (not result))
      (pcase-let ((`(,type ,msg) (ewoc-data node)))
        (when (and (memq type '(:local :muc-local))
                   (listp msg)
                   (plist-get msg :id))
          (setq result (list node
                             (plist-get msg :id)
                             (or (plist-get msg :body) "")
                             msg))))
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
    (let ((buffer-undo-list t))
      (ewoc-invalidate ewoc node))))

(defun jabber-message-correct--outgoing-candidates
    (account peer id stored-from)
  "Return stored correction candidates matching STORED-FROM.
ACCOUNT, PEER, and ID scope the database lookup."
  (seq-filter
   (lambda (candidate)
     (equal stored-from (plist-get candidate :from)))
   (jabber-db-message-correction-candidates account peer id)))

(defun jabber-message-correct--update-outgoing-buffer
    (buffer group id from new-body fallback-length)
  "Update outgoing message ID in BUFFER with NEW-BODY.
GROUP and FROM select the MUC lookup.  FALLBACK-LENGTH updates
the stored fallback range."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((node
                   (if group
                       (jabber-chat-ewoc-find-by-id-and-sender id from)
                     (jabber-chat-ewoc-find-by-id id))))
        (plist-put (cadr (ewoc-data node))
                   :fallback-range
                   (and fallback-length (list 0 fallback-length)))
        (jabber-message-correct--update-ewoc
         jabber-chat-ewoc node new-body))
      (setq jabber-message-correct--pending-outgoing nil))))

(defun jabber-message-correct--send
    (jc group body extra &optional success failure)
  "Send correction BODY and EXTRA on JC.
Use the MUC transport when GROUP is non-nil.  SUCCESS and FAILURE
are optional transport callbacks."
  (if group
      (if success
          (jabber-muc-send jc body extra success failure)
        (jabber-muc-send jc body extra))
    (if success
        (jabber-chat-send jc body extra success failure)
      (jabber-chat-send jc body extra))))

;;; Interactive command

(defun jabber-correct-last-message ()
  "Correct the last sent message in this chat buffer.
Prompts with the existing body pre-filled.  When the corrected
message is a reply, re-attach its <reply> element: per XEP-0308 the
correction replaces the whole message, XEP-0461 linkage included."
  (interactive)
  (when jabber-message-correct--pending-outgoing
    (user-error "A correction is still waiting to be sent"))
  (pcase (jabber-message-correct--find-last-sent jabber-chat-ewoc)
    ('nil (user-error "No sent message found to correct"))
    (`(,_node ,id ,body ,msg)
     (let ((new-body (read-string "Correction: " body)))
       (when (string= new-body body)
         (user-error "No change"))
       (let* ((fb-len (jabber-message-reply--correction-fallback-length
                       msg new-body))
              (reply-els (and-let* ((reply-id (plist-get msg :reply-to-id)))
                           (jabber-message-reply--elements
                            reply-id (plist-get msg :reply-to-jid) fb-len))))
         (let* ((buffer (current-buffer))
                (group (bound-and-true-p jabber-group))
                (account
                 (jabber-connection-bare-jid jabber-buffer-connection))
                (peer (jabber-jid-user
                       (or group jabber-chatting-with)))
                (stored-from (if group (plist-get msg :from) account))
                (db-matches (jabber-message-correct--outgoing-candidates
                             account peer id stored-from))
                (row-id (and (= (length db-matches) 1)
                             (plist-get (car db-matches) :row-id)))
                (extra (cons (jabber-message-correct--replace-element id)
                             reply-els))
                (omemo-p (eq jabber-chat-encryption 'omemo))
                (token (list t id))
                (commit
                 (lambda ()
                   (when (and (car token)
                              (or (not omemo-p)
                                  (and (buffer-live-p buffer)
                                       (with-current-buffer buffer
                                         (eq token
                                             jabber-message-correct--pending-outgoing)))))
                     (setcar token nil)
                     (when row-id
                       (jabber-db-correct-message-row row-id new-body))
                     (jabber-message-correct--update-outgoing-buffer
                      buffer group id (plist-get msg :from)
                      new-body fb-len))))
                (failure
                 (lambda (_reason)
                   (when (car token)
                     (setcar token nil)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (eq token
                                   jabber-message-correct--pending-outgoing)
                           (setq jabber-message-correct--pending-outgoing
                                 nil))))))))
           (when (and jabber-db-path (/= (length db-matches) 1))
             (user-error "Stored correction target is missing or ambiguous"))
           (if omemo-p
               (progn
                 (setq jabber-message-correct--pending-outgoing token)
                 (condition-case err
                     (jabber-message-correct--send
                      jabber-buffer-connection group new-body extra
                      commit failure)
                   (error
                    (funcall failure (error-message-string err))
                    (signal (car err) (cdr err)))))
             (funcall commit)
             (jabber-message-correct--send
              jabber-buffer-connection group new-body extra))))))))

(provide 'jabber-message-correct)
;;; jabber-message-correct.el ends here
