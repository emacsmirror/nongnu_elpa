;;; jabber-message-thread-protocol.el --- XEP-0201 protocol data  -*- lexical-binding: t; -*-

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

;; Pure parsing and construction of XEP-0201 thread elements.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'jabber-xml)

(defun jabber-message-thread-protocol--core-element-p (element)
  "Return non-nil when ELEMENT is a core XMPP thread element."
  (and (consp element)
       (eq (jabber-xml-node-name element) 'thread)
       (member (jabber-xml-get-xmlns element) '(nil "jabber:client"))))

(defun jabber-message-thread-protocol-fields (xml-data)
  "Return valid XEP-0201 thread metadata from XML-DATA, or nil."
  (let ((threads
         (seq-filter #'jabber-message-thread-protocol--core-element-p
                     (jabber-xml-node-children xml-data))))
    (when (= (length threads) 1)
      (let* ((thread (car threads))
             (content (jabber-xml-node-children thread))
             (id (and content
                      (seq-every-p #'stringp content)
                      (apply #'concat content)))
             (parent (jabber-xml-get-attribute thread 'parent)))
        (when (and id
                   (not (string-empty-p id))
                   (or (null parent)
                       (and (not (string-empty-p parent))
                            (not (equal parent id)))))
          (list :thread-id id :thread-parent-id parent))))))

(defun jabber-message-thread-protocol-has-core-p (xml-data)
  "Return non-nil when XML-DATA contains a core thread element."
  (seq-some #'jabber-message-thread-protocol--core-element-p
            (jabber-xml-node-children xml-data)))

(defun jabber-message-thread-protocol-elements (thread-id parent-id)
  "Return a thread element for THREAD-ID and optional PARENT-ID."
  (when (and (stringp thread-id)
             (not (string-empty-p thread-id))
             (or (null parent-id)
                 (and (stringp parent-id)
                      (not (string-empty-p parent-id))
                      (not (equal thread-id parent-id)))))
    (list `(thread (,@(when parent-id
                        (list (cons 'parent parent-id))))
                   ,thread-id))))

(provide 'jabber-message-thread-protocol)
;;; jabber-message-thread-protocol.el ends here
