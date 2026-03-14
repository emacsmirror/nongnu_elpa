;;; jabber-hints.el --- XEP-0334 message processing hints  -*- lexical-binding: t; -*-

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

;; XML element builders for XEP-0334 Message Processing Hints.
;; Callers splice these into outgoing stanzas to advise servers
;; and other entities on how to handle messages.

;;; Code:

(defconst jabber-hints-xmlns "urn:xmpp:hints"
  "XML namespace for XEP-0334 Message Processing Hints.")

(defun jabber-hints-store ()
  "Return a <store/> hint element."
  `(store ((xmlns . ,jabber-hints-xmlns))))

(defun jabber-hints-no-store ()
  "Return a <no-store/> hint element."
  `(no-store ((xmlns . ,jabber-hints-xmlns))))

(defun jabber-hints-no-copy ()
  "Return a <no-copy/> hint element."
  `(no-copy ((xmlns . ,jabber-hints-xmlns))))

(defun jabber-hints-no-permanent-store ()
  "Return a <no-permanent-store/> hint element."
  `(no-permanent-store ((xmlns . ,jabber-hints-xmlns))))

(provide 'jabber-hints)

;;; jabber-hints.el ends here
