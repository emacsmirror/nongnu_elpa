;;; jabber-carbons.el --- Support for XEP-0280: Message Carbons  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-menu)
(require 'jabber-iq)
(require 'jabber-disco)

(defconst jabber-carbons-xmlns "urn:xmpp:carbons:2"
  "XML namespace for XEP-0280 Message Carbons.")

(defcustom jabber-carbons-enable t
  "When non-nil, enable XEP-0280 Message Carbons on connect.
Carbons copy outbound messages to all your other connected
devices, keeping conversations in sync across clients."
  :type 'boolean
  :group 'jabber)

(defun jabber-carbon-success (jc xml-data _context)
  (when (equal "result" (jabber-xml-get-attribute xml-data 'type))
    (message "Carbons feature successfully enabled for %s"
             (jabber-connection-jid jc))))

(defun jabber-carbon-failure (_jc xml-data _context)
  (message "Carbons feature could not be enabled: %S" xml-data))

(add-to-list 'jabber-jid-service-menu
             (cons "Enable Carbons" 'jabber-enable-carbons))

;;;###autoload
(defun jabber-enable-carbons (jc)
  "Send request to enable XEP-0280 Message Carbons.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc
                  nil
                  "set"
                  `(enable ((xmlns . ,jabber-carbons-xmlns)))
                  #'jabber-carbon-success nil
                  #'jabber-carbon-failure nil))

;;;###autoload
(defun jabber-disable-carbons (jc)
  "Send request to disable XEP-0280 Message Carbons.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc
                  nil
                  "set"
                  `(disable ((xmlns . ,jabber-carbons-xmlns)))
                  (lambda (_jc _xml _ctx) (message "Carbons disabled"))
                  nil
                  (lambda (_jc xml _ctx)
                    (message "Failed to disable carbons: %S" xml))
                  nil))

;;;###autoload
(defun jabber-carbons-maybe-enable (jc)
  "Enable carbons for JC if `jabber-carbons-enable' is non-nil."
  (when jabber-carbons-enable
    (jabber-enable-carbons jc)))

(jabber-disco-advertise-feature jabber-carbons-xmlns)

(provide 'jabber-carbons)

;;; jabber-carbons.el ends here
