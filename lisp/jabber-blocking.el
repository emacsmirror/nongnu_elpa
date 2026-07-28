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
(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-iq)
(require 'jabber-disco)

(defconst jabber-blocking-xmlns "urn:xmpp:blocking"
  "XML namespace for XEP-0191 Blocking Command.")

(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el

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
       (and jids
            (cl-remove-if (lambda (jid) (member jid jids)) current))))
    (_ current)))

(defun jabber-blocking--valid-push-p (from state-data)
  "Return non-nil when FROM can push blocking state for STATE-DATA."
  (let ((bare (concat (plist-get state-data :username) "@"
                      (plist-get state-data :server))))
    (or (null from)
        (string= from (plist-get state-data :server))
        (string= from bare)
        (string= from (concat bare "/" (plist-get state-data :resource))))))

(defun jabber-blocking--process-push (jc xml-data)
  "Process an XEP-0191 blocking push in XML-DATA on JC."
  (let* ((state-data (fsm-get-state-data jc))
         (from (jabber-xml-get-attribute xml-data 'from))
         (id (jabber-xml-get-attribute xml-data 'id))
         (query (jabber-iq-query xml-data)))
    (when (and (jabber-blocking--valid-push-p from state-data)
               (memq (jabber-xml-node-name query) '(block unblock)))
      (plist-put state-data :blocking-list
                 (jabber-blocking--apply-push
                  (plist-get state-data :blocking-list) query))
      (jabber-send-iq jc from "result" nil nil nil nil nil id))))

;;;###autoload
(defun jabber-blocking-block-jid (jc jid)
  "Block JID on connection JC.

JC is the Jabber connection.  JID is the bare JID to block."
  (interactive
   (let ((jc (jabber-read-account)))
     (list jc (jabber-read-jid-completing "Block JID: "))))
  (jabber-send-iq jc nil "set"
                  `(block ((xmlns . ,jabber-blocking-xmlns))
                          (item ((jid . ,jid))))
                  (lambda (_jc _xml _ctx)
                    (message "Blocked %s" jid))
                  nil
                  (lambda (_jc xml _ctx)
                    (message "Failed to block %s: %S" jid xml))
                  nil))

;;;###autoload
(defun jabber-blocking-unblock-jid (jc jid)
  "Unblock JID on connection JC.

JC is the Jabber connection.  JID is the bare JID to unblock."
  (interactive
   (let ((jc (jabber-read-account)))
     (list jc (jabber-read-jid-completing "Unblock JID: "))))
  (jabber-send-iq jc nil "set"
                  `(unblock ((xmlns . ,jabber-blocking-xmlns))
                            (item ((jid . ,jid))))
                  (lambda (_jc _xml _ctx)
                    (message "Unblocked %s" jid))
                  nil
                  (lambda (_jc xml _ctx)
                    (message "Failed to unblock %s: %S" jid xml))
                  nil))

;;;###autoload
(defun jabber-blocking-list (jc)
  "Retrieve and display the blocklist for connection JC.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc nil "get"
                  `(blocklist ((xmlns . ,jabber-blocking-xmlns)))
                  #'jabber-blocking--display-list nil
                  (lambda (_jc xml _ctx)
                    (message "Failed to retrieve blocklist: %S" xml))
                  nil))

(defun jabber-blocking--display-list (jc xml-data _closure)
  "Display the blocklist from XML-DATA.
JC is the Jabber connection."
  (let* ((blocklist (jabber-xml-get-children
                     (car (jabber-xml-get-children xml-data 'blocklist))
                     'item))
         (jids (mapcar (lambda (item)
                         (jabber-xml-get-attribute item 'jid))
                       blocklist)))
    (plist-put (fsm-get-state-data jc) :blocking-list jids)
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
    (jabber-send-iq jc nil "get"
                    `(blocklist ((xmlns . ,jabber-blocking-xmlns)))
                    (lambda (jc xml-data _ctx)
                      (jabber-blocking--toggle jc xml-data jid))
                    nil
                    (lambda (_jc xml _ctx)
                      (message "Failed to retrieve blocklist: %S" xml))
                    nil)))

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

(provide 'jabber-blocking)

;;; jabber-blocking.el ends here
