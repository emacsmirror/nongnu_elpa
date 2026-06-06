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

;;; Commentary:
;;
;; All five chat states (active, composing, paused, inactive, gone) are
;; sent and received per XEP-0085.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jabber-util)
(require 'ewoc)
(require 'jabber-core)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-disco)
(require 'jabber-xml)


(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el

(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-muc-nickname "jabber-muc" (group &optional jc))
(declare-function jabber-reactions--reaction-only-p "jabber-reactions" (xml-data))

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

(defvar-local jabber-chatstates--ewoc-node nil
  "Ewoc node for the typing indicator, or nil.")

(defvar-local jabber-chatstates--muc-composers nil
  "Ordered list of MUC occupants currently composing in this buffer.")

(defvar-local jabber-chatstates-composing-sent nil
  "Has composing notification been sent?
It can be sent and cancelled several times.")

;;; INCOMING
;; Code for requesting chat state notifications from others and handling
;; them.

(defun jabber-chatstates-update-message ()
  "Refresh the mode-line indicator for the peer's last chat state."
  (setq jabber-chatstates-message
        (if (and jabber-chatstates-last-state
                 (not (eq 'active jabber-chatstates-last-state)))
            (format " (%s)" (symbol-name jabber-chatstates-last-state))
          "")))

(defun jabber-chatstates--update-ewoc (state)
  "Show or remove the typing indicator ewoc node for STATE."
  (let ((inhibit-read-only t))
    (if (eq state 'composing)
        (progn
          (when (and jabber-chatstates--ewoc-node
                     (not (jabber-chatstates--live-ewoc-node-p
                           jabber-chatstates--ewoc-node)))
            (setq jabber-chatstates--ewoc-node nil))
          (unless jabber-chatstates--ewoc-node
            (setq jabber-chatstates--ewoc-node
                  (jabber-chat-ewoc-enter
                   (list :typing
                         (format "%s is typing..."
                                 (jabber-jid-displayname jabber-chatting-with)))))))
      (jabber-chatstates--delete-typing-node))))

(defun jabber-chatstates--composing-state-p (state)
  "Return non-nil when STATE is the XEP-0085 composing state."
  (eq state 'composing))

(defun jabber-chatstates--muc-add-composer (composers nick)
  "Return COMPOSERS with NICK appended once, preserving order."
  (if (member nick composers)
      composers
    (append composers (list nick))))

(defun jabber-chatstates--muc-remove-composer (composers nick)
  "Return COMPOSERS without NICK, preserving order."
  (remove nick composers))

(defun jabber-chatstates--muc-composers-for-state (composers nick state)
  "Return COMPOSERS updated for NICK's chat STATE."
  (if (jabber-chatstates--composing-state-p state)
      (jabber-chatstates--muc-add-composer composers nick)
    (jabber-chatstates--muc-remove-composer composers nick)))

(defun jabber-chatstates--format-muc-composers (composers)
  "Return typing text for COMPOSERS, or nil when no one is composing."
  (pcase composers
    ('nil nil)
    (`(,nick) (format "%s is typing..." nick))
    (_ (format "%s are typing..." (string-join composers ", ")))))

(defun jabber-chatstates--live-ewoc-node-p (node)
  "Return non-nil when NODE still has a live EWOC marker."
  (condition-case err
      (and-let* ((marker (ewoc-location node)))
        (marker-buffer marker))
    (error
     (message "Jabber: stale chat state ewoc node: %s"
              (error-message-string err))
     nil)))

(defun jabber-chatstates--delete-typing-node ()
  "Remove the current typing indicator node without changing state."
  (when jabber-chatstates--ewoc-node
    (when (jabber-chatstates--live-ewoc-node-p jabber-chatstates--ewoc-node)
      (jabber-chat-ewoc-delete jabber-chatstates--ewoc-node))
    (setq jabber-chatstates--ewoc-node nil)))

(defun jabber-chatstates--muc-reinsert-typing ()
  "Reinsert the current buffer's MUC typing indicator at the bottom."
  (jabber-chatstates--delete-typing-node)
  (when-let* ((message (jabber-chatstates--format-muc-composers
                        jabber-chatstates--muc-composers)))
    (setq jabber-chatstates--ewoc-node
          (jabber-chat-ewoc-enter (list :typing message)))))

(defun jabber-chatstates--update-muc-ewoc ()
  "Refresh the current buffer's MUC typing indicator at the bottom."
  (let ((inhibit-read-only t))
    (jabber-chatstates--muc-reinsert-typing)))

(defun jabber-chatstates--muc-remove-nick (nick)
  "Remove MUC NICK from the current buffer's composer state."
  (setq jabber-chatstates--muc-composers
        (jabber-chatstates--muc-remove-composer
         jabber-chatstates--muc-composers nick)))

(defun jabber-chatstates--muc-clear-nick (nick)
  "Remove MUC NICK from the current buffer's typing indicator."
  (jabber-chatstates--muc-remove-nick nick)
  (jabber-chatstates--update-muc-ewoc))

(defun jabber-chatstates--clear-typing ()
  "Remove the typing indicator ewoc node if present."
  (jabber-chatstates--delete-typing-node))

(defun jabber-chatstates--clear-send-typing ()
  "Remove direct-chat typing state while preserving active MUC composers."
  (unless jabber-chatstates--muc-composers
    (jabber-chatstates--clear-typing)))

(defun jabber-chatstates--message-state (xml-data)
  "Return the chat state symbol from XML-DATA, or nil."
  (jabber-xml-node-name
   (cl-find jabber-chatstates-xmlns
            (jabber-xml-node-children xml-data)
            :key (lambda (x) (jabber-xml-get-attribute x 'xmlns))
            :test #'string=)))

(defun jabber-chatstates--muc-self-nick-p (group nick jc)
  "Return non-nil when NICK is our nickname in GROUP on JC."
  (and-let* ((self-nick (jabber-muc-nickname group jc)))
    (string= nick self-nick)))

(defun jabber-chatstates--handle-muc-state (jc from state)
  "Apply incoming MUC chat STATE from FROM on JC."
  (when-let* ((group (jabber-jid-user from))
              (nick (jabber-jid-resource from))
              (buffer (jabber-muc-find-buffer group)))
    (with-current-buffer buffer
      (unless (jabber-chatstates--muc-self-nick-p group nick jc)
        (setq jabber-chatstates--muc-composers
              (jabber-chatstates--muc-composers-for-state
               jabber-chatstates--muc-composers nick state)))
      (jabber-chatstates--update-muc-ewoc))))

(add-hook 'jabber-chat-send-hooks #'jabber-chatstates-when-sending)
(defun jabber-chatstates-when-sending (_text _id)
  "Chat-send hook: cancel state timers and attach an `active' element."
  (jabber-chatstates--clear-send-typing)
  (jabber-chatstates-stop-timer)
  (when (and jabber-chatstates-confirm jabber-chatstates-requested)
    (setq jabber-chatstates-composing-sent nil)
    `((active ((xmlns . ,jabber-chatstates-xmlns))))))

;;; OUTGOING
;; Code for handling requests for chat state notifications and providing
;; them, modulo user preferences.

(defvar-local jabber-chatstates-paused-timer nil
  "Timer that counts down from `composing' state to `paused'.")

(defvar-local jabber-chatstates-inactive-timer nil
  "Timer that counts down from `paused' state to `inactive'.")

(defun jabber-chatstates-stop-timer ()
  "Stop the `paused' and `inactive' timers."
  (when jabber-chatstates-paused-timer
    (cancel-timer jabber-chatstates-paused-timer))
  (when jabber-chatstates-inactive-timer
    (cancel-timer jabber-chatstates-inactive-timer)))

(defun jabber-chatstates-kick-timer ()
  "Start (or restart) the `paused' timer as approriate."
  (jabber-chatstates-stop-timer)
  (setq jabber-chatstates-paused-timer
        (run-with-timer 5 nil #'jabber-chatstates-send-paused)))

(defun jabber-chatstates-send-paused ()
  "Send a `paused' state notification, then start the inactive timer."
  (when (and jabber-chatstates-confirm jabber-chatting-with)
    (setq jabber-chatstates-composing-sent nil)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection
     `(message
       ((to . ,jabber-chatting-with)
        (type . "chat"))
       (paused ((xmlns . ,jabber-chatstates-xmlns)))))
    (setq jabber-chatstates-inactive-timer
          (run-with-timer 30 nil #'jabber-chatstates-send-inactive))))

(defun jabber-chatstates-send-inactive ()
  "Send an `inactive' state notification."
  (when (and jabber-chatstates-confirm jabber-chatting-with)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection
     `(message
       ((to . ,jabber-chatting-with)
        (type . "chat"))
       (inactive ((xmlns . ,jabber-chatstates-xmlns)))))))

(defun jabber-chatstates-send-gone ()
  "Send a `gone' state notification and cancel timers.
Added to `kill-buffer-hook' in chat buffers."
  (when (and jabber-chatstates-confirm jabber-chatting-with)
    (jabber-chatstates-stop-timer)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection
     `(message
       ((to . ,jabber-chatting-with)
        (type . "chat"))
       (gone ((xmlns . ,jabber-chatstates-xmlns)))))))

(defun jabber-chatstates-after-change ()
  "Post-command-hook: emit `composing'/`active' when typing state flips."
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

(defun jabber-chatstates--real-body-message-p (xml-data)
  "Return non-nil when XML-DATA has a body that should clear chatstates."
  (and (jabber-xml-get-children xml-data 'body)
       (not (and (fboundp 'jabber-reactions--reaction-only-p)
                 (jabber-reactions--reaction-only-p xml-data)))))

(defun jabber-chatstates--handle-direct-state (jc xml-data from)
  "Update the direct chat buffer from XML-DATA sent by FROM on JC."
  (when-let* ((buffer (get-buffer (jabber-chat-get-buffer from jc))))
    (with-current-buffer buffer
      (cond
       ;; If we get an error message, we shouldn't report any
       ;; events, as the requests are mirrored from us.
       ((string= (jabber-xml-get-attribute xml-data 'type) "error")
        (remove-hook 'post-command-hook #'jabber-chatstates-after-change t)
        (setq jabber-chatstates-requested nil))

       (t
        (let ((state (jabber-chatstates--message-state xml-data)))
          ;; Set up hooks for composition notification
          (when (and jabber-chatstates-confirm state)
            (setq jabber-chatstates-requested t)
            (add-hook 'post-command-hook #'jabber-chatstates-after-change nil t)
            (add-hook 'kill-buffer-hook #'jabber-chatstates-send-gone nil t))

          (when (or state (jabber-chatstates--real-body-message-p xml-data))
            (setq jabber-chatstates-last-state state)
            (jabber-chatstates--update-ewoc state))))))))

(defun jabber-handle-incoming-message-chatstates (jc xml-data)
  "Update the chat buffer's typing indicator from XML-DATA on JC."
  (when-let* ((from (jabber-xml-get-attribute xml-data 'from)))
    (if (string= (jabber-xml-get-attribute xml-data 'type) "groupchat")
        (let ((state (jabber-chatstates--message-state xml-data)))
          (when (and (not (string= (jabber-xml-get-attribute xml-data 'type) "error"))
                     (or state (jabber-chatstates--real-body-message-p xml-data)))
            (jabber-chatstates--handle-muc-state jc from state)))
      (jabber-chatstates--handle-direct-state jc xml-data from))))

(jabber-chain-add 'jabber-message-chain #'jabber-handle-incoming-message-chatstates 50)

(jabber-disco-advertise-feature jabber-chatstates-xmlns)

(provide 'jabber-chatstates)

;;; jabber-chatstates.el ends here
