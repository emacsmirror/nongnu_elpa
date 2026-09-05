;;; jabber-jid.el --- Basic JID parsing  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2008, 2010 - Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru
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

;; Dependency-free JID parsing shared by stream management and utilities.

;;; Code:

(defun jabber-jid-user (jid)
  "Return the user portion (username@server) of JID.
JID must be a string."
  ;;transports don't have @, so don't require it
  ;;(string-match ".*@[^/]*" jid)
  (string-match "[^/]*" jid)
  (match-string 0 jid))

(defun jabber-jid-resource (jid)
  "Return the resource portion of a JID, or nil if there is none.
JID must be a string."
  (when (string-match "^\\(\\([^/]*@\\)?[^/]*\\)/\\(.*\\)" jid)
    (match-string 3 jid)))

(provide 'jabber-jid)
;;; jabber-jid.el ends here
