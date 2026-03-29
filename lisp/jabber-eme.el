;;; jabber-eme.el --- XEP-0380 Explicit Message Encryption  -*- lexical-binding: t; -*-

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

;; XML element builder for XEP-0380 Explicit Message Encryption.
;; Callers splice the returned element into outgoing encrypted stanzas
;; so non-supporting clients can display a meaningful fallback.

;;; Code:

(defconst jabber-eme-xmlns "urn:xmpp:eme:0"
  "XML namespace for XEP-0380 Explicit Message Encryption.")

(defun jabber-eme-encryption (namespace &optional name)
  "Return an <encryption> element for NAMESPACE.
Optional NAME is a human-readable encryption name."
  `(encryption ((xmlns . ,jabber-eme-xmlns)
                (namespace . ,namespace)
                ,@(when name `((name . ,name))))))

(provide 'jabber-eme)

;;; jabber-eme.el ends here
