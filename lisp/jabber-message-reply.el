;;; jabber-message-reply.el --- XEP-0461 Message Replies  -*- lexical-binding: t; -*-

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

;; XEP-0461 Message Replies with XEP-0428 Fallback Indication.
;; Adds reply-to-message support in chat buffers.  The user positions
;; point on a message, invokes `jabber-chat-reply', a quoted fallback
;; is inserted into the composition area, and on send the <reply> and
;; <fallback> elements are added to the stanza.  The fallback text is
;; kept in the displayed body as-is.

;;; Code:

(require 'ewoc)
(require 'jabber-disco)
(require 'jabber-xml)

(declare-function jabber-jid-displayname "jabber-util" (jid))
(declare-function jabber-jid-username "jabber-util" (jid))
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-jid-resource "jabber-util" (jid))

(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chat-send-hooks)         ; jabber-chat.el
(defvar jabber-point-insert)            ; jabber-chatbuffer.el
(defvar jabber-group)                   ; jabber-muc.el

(defconst jabber-message-reply-xmlns "urn:xmpp:reply:0"
  "XEP-0461 Message Replies namespace.")

(defconst jabber-message-reply-fallback-xmlns "urn:xmpp:fallback:0"
  "XEP-0428 Fallback Indication namespace.")

;;; Buffer-local reply state

(defvar-local jabber-message-reply--id nil
  "Stanza ID of the message being replied to.")

(defvar-local jabber-message-reply--jid nil
  "JID of the original message author.")

(defvar-local jabber-message-reply--fallback-length nil
  "Character count of the fallback prefix in the composition area.")

;;; Pure functions

(defun jabber-message-reply--build-fallback-text (author body)
  "Build a fallback quote string from AUTHOR and BODY.
Returns \"> Author:\\n> line1\\n> line2\\n\"."
  (let ((lines (if (or (null body) (string-empty-p body))
                   nil
                 (split-string body "\n"))))
    (concat "> " author ":\n"
            (mapconcat (lambda (line) (concat "> " line))
                       lines
                       "\n")
            "\n")))

(defun jabber-message-reply--select-id (msg muc-p)
  "Select the appropriate message ID from MSG for a reply.
In MUC (when MUC-P is non-nil), prefer :server-id.
In 1:1 chat, use :id.  Returns nil if unavailable."
  (if muc-p
      (or (plist-get msg :server-id) (plist-get msg :id))
    (plist-get msg :id)))

;;; Send hook

(defun jabber-message-reply--send-hook (body _id)
  "Add <reply> and <fallback> elements when replying to a message.
BODY is the message text.  Clears reply state after producing elements."
  (when jabber-message-reply--id
    (let ((reply-id jabber-message-reply--id)
          (reply-jid jabber-message-reply--jid)
          (fb-len jabber-message-reply--fallback-length))
      (setq jabber-message-reply--id nil
            jabber-message-reply--jid nil
            jabber-message-reply--fallback-length nil)
      (let ((elements
             (list
              `(reply ((xmlns . ,jabber-message-reply-xmlns)
                       (to . ,reply-jid)
                       (id . ,reply-id))))))
        (when (and fb-len (> fb-len 0) (<= fb-len (length body)))
          (push `(fallback ((xmlns . ,jabber-message-reply-fallback-xmlns)
                            (for . ,jabber-message-reply-xmlns))
                           (body ((start . "0")
                                  (end . ,(number-to-string fb-len)))))
                elements))
        elements))))

(with-eval-after-load "jabber-chat"
  (add-hook 'jabber-chat-send-hooks #'jabber-message-reply--send-hook))

;;; Helpers

(defun jabber-message-reply--author-name (jid)
  "Return a short display name for JID.
In MUC buffers the resource is the nickname.
In 1:1 chat, use the username part of the JID."
  (if (bound-and-true-p jabber-group)
      (or (jabber-jid-resource jid)
          (jabber-jid-displayname jid))
    (or (jabber-jid-username jid)
        (jabber-jid-user jid))))

;;; Interactive commands

;;;###autoload
(defun jabber-chat-reply ()
  "Reply to the message at point in the ewoc.
Stores reply state and inserts fallback quote text at point-max."
  (interactive)
  (unless (bound-and-true-p jabber-chat-ewoc)
    (user-error "Not in a chat buffer"))
  (let* ((ewoc-node (ewoc-locate jabber-chat-ewoc (point)))
         (data (and ewoc-node (ewoc-data ewoc-node)))
         (msg (and data (cadr data)))
         (muc-p (bound-and-true-p jabber-group))
         (id (and msg (jabber-message-reply--select-id msg muc-p))))
    (unless id
      (user-error "No message ID at point"))
    (let* ((from (plist-get msg :from))
           (author (if from
                       (jabber-message-reply--author-name from)
                     "me"))
           (body (or (plist-get msg :body) ""))
           (jid (or from ""))
           (fallback (jabber-message-reply--build-fallback-text author body)))
      (setq jabber-message-reply--id id
            jabber-message-reply--jid (if (stringp jid) jid (format "%s" jid))
            jabber-message-reply--fallback-length (length fallback))
      (goto-char (point-max))
      (insert fallback)
      (message "Replying to %s (C-c C-k to cancel)" author))))

;;;###autoload
(defun jabber-chat-cancel-reply ()
  "Cancel the pending reply and remove fallback text."
  (interactive)
  (when jabber-message-reply--id
    (let ((fb-len jabber-message-reply--fallback-length))
      (setq jabber-message-reply--id nil
            jabber-message-reply--jid nil
            jabber-message-reply--fallback-length nil)
      (when (and fb-len (> fb-len 0))
        (save-excursion
          (goto-char jabber-point-insert)
          (delete-char (min fb-len (- (point-max) (point)))))))
    (message "Reply cancelled")))

;;; Disco

(jabber-disco-advertise-feature jabber-message-reply-xmlns)

(provide 'jabber-message-reply)
;;; jabber-message-reply.el ends here
