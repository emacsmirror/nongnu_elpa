;;; jabber-chatstate.el --- Chat state notification (XEP-0085) implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Ami Fischman <ami@fischman.org>
;; (based entirely on jabber-events.el by Magnus Henoch <mange@freemail.hu>)
;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; TODO
;; - Currently only active/composing notifications are /sent/ though all 5
;;   notifications are handled on receipt.

;;; Code:

(require 'cl-lib)
(require 'jabber-core)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-disco)
(require 'jabber-xml)

(defgroup jabber-chatstates nil
  "Chat state notifications."
  :group 'jabber)

(defconst jabber-chatstates-xmlns "http://jabber.org/protocol/chatstates"
  "XML namespace for the chatstates feature.")

(defcustom jabber-chatstates-confirm t
  "Send notifications about chat states?"
  :type 'boolean)

(defvar-local jabber-chatstates-requested t
  "Whether chat state notifications should be sent.
Non-nil means send states, nil means don't.")

(defvar-local jabber-chatstates-last-state nil
  "The last seen chat state.")

(defvar-local jabber-chatstates-message ""
  "Human-readable presentation of chat state information.")

(defvar-local jabber-chatstates-composing-sent nil
  "Has composing notification been sent?
It can be sent and cancelled several times.")

;;; INCOMING
;; Code for requesting chat state notifications from others and handling
;; them.

(defun jabber-chatstates-update-message ()
  (setq jabber-chatstates-message
        (if (and jabber-chatstates-last-state
                 (not (eq 'active jabber-chatstates-last-state)))
            (format " (%s)" (symbol-name jabber-chatstates-last-state))
          "")))

(add-hook 'jabber-chat-send-hooks #'jabber-chatstates-when-sending)
(defun jabber-chatstates-when-sending (_text _id)
  (jabber-chatstates-update-message)
  (jabber-chatstates-stop-timer)
  (when jabber-chatstates-confirm
    (setq jabber-chatstates-composing-sent nil)
    `((active ((xmlns . ,jabber-chatstates-xmlns))))))

;;; OUTGOING
;; Code for handling requests for chat state notifications and providing
;; them, modulo user preferences.

(defvar-local jabber-chatstates-paused-timer nil
  "Timer that counts down from `composing' state to `paused'.")

(defun jabber-chatstates-stop-timer ()
  "Stop the `paused' timer."
  (when jabber-chatstates-paused-timer
    (cancel-timer jabber-chatstates-paused-timer)))

(defun jabber-chatstates-kick-timer ()
  "Start (or restart) the `paused' timer as approriate."
  (jabber-chatstates-stop-timer)
  (setq jabber-chatstates-paused-timer
        (run-with-timer 5 nil #'jabber-chatstates-send-paused)))

(defun jabber-chatstates-send-paused ()
  "Send a `paused' state notification."
  (when (and jabber-chatstates-confirm jabber-chatting-with)
    (setq jabber-chatstates-composing-sent nil)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection
     `(message
       ((to . ,jabber-chatting-with)
        (type . "chat"))
       (paused ((xmlns . ,jabber-chatstates-xmlns)))))))

(defun jabber-chatstates-after-change ()
  (let* ((composing-now (not (= (point-max) jabber-point-insert)))
         (state (if composing-now 'composing 'active)))
    (when (and jabber-chatstates-confirm
               jabber-chatting-with
               (not (eq composing-now jabber-chatstates-composing-sent)))
      (jabber-send-sexp-if-connected
       jabber-buffer-connection
       `(message
         ((to . ,jabber-chatting-with)
          (type . "chat"))
         (,state ((xmlns . ,jabber-chatstates-xmlns)))))
      (when (setq jabber-chatstates-composing-sent composing-now)
        (jabber-chatstates-kick-timer)))))

;;; COMMON

(defun jabber-handle-incoming-message-chatstates (jc xml-data)
  (when-let* ((buffer (get-buffer (jabber-chat-get-buffer
                                  (jabber-xml-get-attribute xml-data 'from) jc))))
    (with-current-buffer buffer
      (cond
       ;; If we get an error message, we shouldn't report any
       ;; events, as the requests are mirrored from us.
       ((string= (jabber-xml-get-attribute xml-data 'type) "error")
        (remove-hook 'post-command-hook #'jabber-chatstates-after-change t)
        (setq jabber-chatstates-requested nil))

       (t
	(let ((state
	       (jabber-xml-node-name
		(cl-find jabber-chatstates-xmlns
			 (jabber-xml-node-children xml-data)
			 :key (lambda (x) (jabber-xml-get-attribute x 'xmlns))
			 :test #'string=))))
	  ;; Set up hooks for composition notification
	  (when (and jabber-chatstates-confirm state)
	    (setq jabber-chatstates-requested t)
	    (add-hook 'post-command-hook #'jabber-chatstates-after-change nil t))

	  (setq jabber-chatstates-last-state state)
	  (jabber-chatstates-update-message)))))))

;; Add function last in chain, so a chat buffer is already created.
(add-to-list 'jabber-message-chain #'jabber-handle-incoming-message-chatstates t)

(jabber-disco-advertise-feature jabber-chatstates-xmlns)

(provide 'jabber-chatstates)

;;; jabber-chatstates.el ends here