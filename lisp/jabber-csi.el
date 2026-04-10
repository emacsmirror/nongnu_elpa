;;; jabber-csi.el --- XEP-0352 Client State Indication  -*- lexical-binding: t; -*-

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

;; Send <active/> and <inactive/> top-level elements per XEP-0352
;; based on whether any Emacs frame currently has input focus.

;;; Code:

(require 'jabber-core)
(require 'jabber-disco)

(declare-function jabber-send-sexp-if-connected "jabber-core" (jc sexp))

(defvar jabber-pre-disconnect-hook)
(defvar jabber-lost-connection-hooks)
(defvar jabber-post-resume-hooks)

(defconst jabber-csi-xmlns "urn:xmpp:csi:0"
  "XML namespace for XEP-0352 Client State Indication.")

(defgroup jabber-csi nil
  "Client State Indication."
  :group 'jabber)

(defcustom jabber-csi-enable t
  "Send CSI active/inactive notifications to the server."
  :type 'boolean
  :group 'jabber-csi)

(defvar jabber-csi--last-state nil
  "Last CSI state sent: `active', `inactive', or nil.")

(defvar jabber-csi--timer nil
  "Pending debounce timer for focus changes, or nil.")

(defconst jabber-csi--debounce-delay 0.5
  "Seconds to wait before sending a CSI state change.
Coalesces rapid focus oscillations into one stanza.")

(defun jabber-csi--stop-timer ()
  "Cancel any pending CSI debounce timer."
  (when (timerp jabber-csi--timer)
    (cancel-timer jabber-csi--timer)
    (setq jabber-csi--timer nil)))

(defun jabber-csi--focused-p ()
  "Return non-nil if any Emacs frame has input focus."
  (cl-some #'frame-focus-state (frame-list)))

(defun jabber-csi--send-state ()
  "Send CSI active or inactive to all connections."
  (setq jabber-csi--timer nil)
  (when jabber-csi-enable
    (let ((state (if (jabber-csi--focused-p) 'active 'inactive)))
      (unless (eq state jabber-csi--last-state)
        (setq jabber-csi--last-state state)
        (dolist (jc jabber-connections)
          (jabber-send-sexp-if-connected
           jc `(,state ((xmlns . ,jabber-csi-xmlns)))))))))

(defun jabber-csi--focus-changed ()
  "Hook for `after-focus-change-function'.
Debounces rapid focus changes into a single CSI stanza."
  (jabber-csi--stop-timer)
  (setq jabber-csi--timer
	(run-with-timer jabber-csi--debounce-delay nil
			#'jabber-csi--send-state)))

(defun jabber-csi--on-connect (_jc)
  "Send current CSI state after connection.
Added to `jabber-post-connect-hooks'."
  (jabber-csi--stop-timer)
  (setq jabber-csi--last-state nil)
  (jabber-csi--send-state))

(defun jabber-csi--on-disconnect (&optional _jc)
  "Cancel pending CSI timer and reset state on disconnect."
  (jabber-csi--stop-timer)
  (setq jabber-csi--last-state nil))

(add-hook 'jabber-post-connect-hooks #'jabber-csi--on-connect)
(add-hook 'jabber-post-resume-hooks #'jabber-csi--on-connect)
(add-hook 'jabber-pre-disconnect-hook #'jabber-csi--on-disconnect)
(add-hook 'jabber-lost-connection-hooks #'jabber-csi--on-disconnect)
(add-function :after after-focus-change-function #'jabber-csi--focus-changed)

(jabber-disco-advertise-feature jabber-csi-xmlns)

(provide 'jabber-csi)

;;; jabber-csi.el ends here
