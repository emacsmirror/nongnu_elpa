;;; jabber-browse.el --- jabber browsing by XEP-0011  -*- lexical-binding: t; -*-

;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2003, 2004 - Magnus Henoch - mange@freemail.hu
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

;; Legacy entity browsing via jabber:iq:browse (XEP-0011, deprecated).
;; Sends browse queries and renders results (users, services,
;; conferences) into a buffer.  Read-only: we answer no browse
;; requests.  Modern servers use Service Discovery (XEP-0030) instead;
;; see `jabber-disco.el'.

;;; Code:

(require 'jabber-iq)
(require 'jabber-xml)
(require 'jabber-util)
(require 'jabber-menu)

(defconst jabber-browse-xmlns "jabber:iq:browse"
  "XEP-0011 Jabber Browsing namespace.")

(add-to-list 'jabber-jid-info-menu (cons "Send browse query" 'jabber-get-browse))

(defun jabber-get-browse (jc to)
  "Send a browse infoquery request to someone.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "browse: " nil nil nil nil t)))
  (jabber-send-iq jc to
                  "get"
                  `(query ((xmlns . ,jabber-browse-xmlns)))
                  #'jabber-process-data #'jabber-process-browse
		  #'jabber-process-data "Browse failed"))

(defconst jabber-browse--category-labels
  '((user       . "$ USER")
    (service    . "* SERVICE")
    (conference . "@ CONFERENCE"))
  "Alist mapping browse category symbols to display labels.")

(defun jabber-browse--category-heading (item)
  "Return a heading string for browse ITEM."
  (let* ((name (jabber-xml-node-name item))
         (category (jabber-xml-get-attribute item 'category))
         (label (or (cdr (assq name jabber-browse--category-labels))
                    (cdr (assq (intern-soft (or category ""))
                               jabber-browse--category-labels))
                    (format "! OTHER: %s"
                            (if (and category (> (length category) 0))
                                category
                              name)))))
    (propertize label 'face 'jabber-title-medium)))

(defun jabber-browse--insert-item (jc item)
  "Insert a single browse result ITEM into the current buffer.
JC is the Jabber connection."
  (let ((jid (jabber-xml-get-attribute item 'jid))
        (beginning (point)))
    (insert (jabber-browse--category-heading item) "\n\n")
    (dolist (attr '((type . "Type:\t\t")
                    (jid . "JID:\t\t")
                    (name . "Name:\t\t")
                    (version . "Version:\t")))
      (let ((data (jabber-xml-get-attribute item (car attr))))
        (when (> (length data) 0)
          (insert (cdr attr) data "\n"))))
    (dolist (ns (jabber-xml-get-children item 'ns))
      (let ((text (car (jabber-xml-node-children ns))))
        (when (stringp text)
          (insert "Namespace:\t" text "\n"))))
    (insert "\n")
    (put-text-property beginning (point) 'jabber-jid jid)
    (put-text-property beginning (point) 'jabber-account jc)
    (when (listp (car (jabber-xml-node-children item)))
      (jabber-process-browse jc item))))

(defun jabber-process-browse (jc xml-data)
  "Handle results from jabber:iq:browse requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (dolist (item (jabber-xml-node-children xml-data))
    (when (and (listp item)
               (not (eq (jabber-xml-node-name item) 'ns)))
      (jabber-browse--insert-item jc item))))

(provide 'jabber-browse)
;;; jabber-browse.el ends here.
