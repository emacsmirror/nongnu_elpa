;;; jabber-receipts.el --- Delivery receipts and chat markers  -*- lexical-binding: t; -*-

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

;; XEP-0184 Message Delivery Receipts and XEP-0333 Chat Markers for
;; 1:1 chats.  Outgoing messages get <request/> and <markable/>
;; elements.  Incoming receipts update the DB and header-line.
;; Optionally sends <received/> and <displayed/> back.

;;; Code:

(require 'jabber-xml)
(require 'jabber-core)
(require 'jabber-chat)
(require 'jabber-db)
(require 'jabber-disco)

(defgroup jabber-receipts nil
  "Message delivery receipts (XEP-0184) and chat markers (XEP-0333)."
  :group 'jabber-chat)

(defconst jabber-receipts-xmlns "urn:xmpp:receipts"
  "XML namespace for XEP-0184 Message Delivery Receipts.")

(defconst jabber-chat-markers-xmlns "urn:xmpp:chat-markers:0"
  "XML namespace for XEP-0333 Chat Markers.")

(defcustom jabber-chat-send-receipts t
  "Send delivery receipts and read markers to peers.
When non-nil, send <received/> on message delivery and
<displayed/> when a chat buffer becomes visible.
Incoming receipts are always processed regardless of this setting."
  :type 'boolean
  :group 'jabber-receipts)

(defface jabber-chat-delivered
  '((t :inherit shadow))
  "Face for delivery receipt status in header-line."
  :group 'jabber-receipts)

(defface jabber-chat-seen
  '((t :inherit success))
  "Face for seen/displayed status in header-line."
  :group 'jabber-receipts)

(defvar-local jabber-chat-receipt-message ""
  "Header-line string showing receipt status for current chat.")

(defvar-local jabber-receipts--pending-displayed-id nil
  "Stanza ID of latest unread markable message in this buffer.")

;;; Send hook

(defun jabber-receipts--send-hook (_body _id)
  "Add receipt request and markable elements to outgoing messages.
Added to `jabber-chat-send-hooks'."
  `((request ((xmlns . ,jabber-receipts-xmlns)))
    (markable ((xmlns . ,jabber-chat-markers-xmlns)))))

(add-hook 'jabber-chat-send-hooks #'jabber-receipts--send-hook)

(provide 'jabber-receipts)

;;; jabber-receipts.el ends here
