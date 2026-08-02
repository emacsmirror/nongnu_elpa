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
(require 'jabber-buffer-registry)
(require 'jabber-util)
(require 'ewoc)
(require 'jabber-core)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-disco)
(require 'jabber-message-thread)
(require 'jabber-muc)
(require 'jabber-muc-state)
(require 'jabber-reactions)
(require 'jabber-xml)


(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el

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

(defun jabber-chatstates--enable-send-hooks (groupchat-p)
  "Enable local chat-state hooks, excluding gone for GROUPCHAT-P."
  (setq jabber-chatstates-requested t)
  (add-hook 'post-command-hook #'jabber-chatstates-after-change nil t)
  (add-hook 'kill-buffer-hook #'jabber-chatstates-stop-timer nil t)
  (unless groupchat-p
    (add-hook 'kill-buffer-hook #'jabber-chatstates-send-gone nil t)))

(defun jabber-chatstates--direct-parent-sends-p (parent-buffer)
  "Return non-nil when PARENT-BUFFER negotiated chat-state sending."
  (and (buffer-live-p parent-buffer)
       (buffer-local-value 'jabber-chatstates-requested parent-buffer)
       (memq #'jabber-chatstates-after-change
             (buffer-local-value 'post-command-hook parent-buffer))))

(defun jabber-chatstates--thread-buffer-setup (parent-buffer)
  "Set up chat-state sending using PARENT-BUFFER's conversation."
  (cond
   ((bound-and-true-p jabber-group)
    (when jabber-chatstates-confirm
      (jabber-chatstates--enable-send-hooks t)))
   (t
    (setq jabber-chatstates-requested
          (and (buffer-live-p parent-buffer)
               (buffer-local-value
                'jabber-chatstates-requested parent-buffer)))
    (when (and jabber-chatstates-confirm
               (jabber-chatstates--direct-parent-sends-p parent-buffer))
      (jabber-chatstates--enable-send-hooks nil)))))

(add-hook 'jabber-message-thread-buffer-created-functions
          #'jabber-chatstates--thread-buffer-setup)

(defun jabber-chatstates--handle-muc-state (buffer jc from state)
  "Apply incoming MUC chat STATE from FROM on JC in BUFFER."
  (let ((group (jabber-jid-user from))
        (nick (jabber-jid-resource from)))
    (when (and group nick)
      (with-current-buffer buffer
        (unless (eq state 'gone)
          (unless (jabber-chatstates--muc-self-nick-p group nick jc)
            (when (and state
                       jabber-chatstates-confirm
                       (bound-and-true-p jabber-message-thread-id))
              (jabber-chatstates--enable-send-hooks t))
            (setq jabber-chatstates--muc-composers
                  (jabber-chatstates--muc-composers-for-state
                   jabber-chatstates--muc-composers nick state)))
          (jabber-chatstates--update-muc-ewoc))))))

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

(defun jabber-chatstates--conversation ()
  "Return the current chat target and message type, or nil."
  (cond
   ((bound-and-true-p jabber-group)
    (list jabber-group "groupchat"))
   ((bound-and-true-p jabber-chatting-with)
    (list jabber-chatting-with "chat"))))

(defun jabber-chatstates--stanza (state)
  "Return a standalone chat STATE stanza for the current buffer."
  (when-let* ((conversation (jabber-chatstates--conversation)))
    `(message
      ((to . ,(car conversation))
       (type . ,(cadr conversation)))
      ,@(jabber-message-thread--elements
         (bound-and-true-p jabber-message-thread-id)
         (bound-and-true-p jabber-message-thread-parent-id))
      (,state ((xmlns . ,jabber-chatstates-xmlns))))))

(defun jabber-chatstates-stop-timer ()
  "Stop the `paused' and `inactive' timers."
  (when jabber-chatstates-paused-timer
    (cancel-timer jabber-chatstates-paused-timer))
  (when jabber-chatstates-inactive-timer
    (cancel-timer jabber-chatstates-inactive-timer)))

(defun jabber-chatstates--call-in-buffer (buffer function)
  "Call FUNCTION in BUFFER when BUFFER is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (funcall function))))

(defun jabber-chatstates--run-with-buffer-timer (seconds function)
  "Call FUNCTION after SECONDS in the current live buffer."
  (run-with-timer seconds nil #'jabber-chatstates--call-in-buffer
                  (current-buffer) function))

(defun jabber-chatstates-kick-timer ()
  "Start (or restart) the `paused' timer as approriate."
  (jabber-chatstates-stop-timer)
  (setq jabber-chatstates-paused-timer
        (jabber-chatstates--run-with-buffer-timer
         5 #'jabber-chatstates-send-paused)))

(defun jabber-chatstates-send-paused ()
  "Send a `paused' state notification, then start the inactive timer."
  (when-let* (((and jabber-chatstates-confirm
                    jabber-chatstates-requested))
              (stanza (jabber-chatstates--stanza 'paused)))
    (setq jabber-chatstates-composing-sent nil)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection stanza)
    (setq jabber-chatstates-inactive-timer
          (jabber-chatstates--run-with-buffer-timer
           30 #'jabber-chatstates-send-inactive))))

(defun jabber-chatstates-send-inactive ()
  "Send an `inactive' state notification."
  (when-let* (((and jabber-chatstates-confirm
                    jabber-chatstates-requested))
              (stanza (jabber-chatstates--stanza 'inactive)))
    (jabber-send-sexp-if-connected
     jabber-buffer-connection stanza)))

(defun jabber-chatstates-send-gone ()
  "Send a `gone' state notification and cancel timers.
Added to `kill-buffer-hook' in chat buffers."
  (when-let* (((and jabber-chatstates-confirm
                    jabber-chatstates-requested
                    (not (bound-and-true-p jabber-group))))
              (stanza (jabber-chatstates--stanza 'gone)))
    (jabber-chatstates-stop-timer)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection stanza)))

(defun jabber-chatstates-after-change ()
  "Post-command-hook: emit `composing'/`active' when typing state flips."
  (let* ((composing-now (not (= (point-max) jabber-point-insert)))
         (state (if composing-now 'composing 'active)))
    (when-let* (((and jabber-chatstates-confirm
                      jabber-chatstates-requested
                      (not (eq composing-now
                               jabber-chatstates-composing-sent))))
                (stanza (jabber-chatstates--stanza state)))
      (jabber-send-sexp-if-connected
       jabber-buffer-connection stanza)
      (when (setq jabber-chatstates-composing-sent composing-now)
        (jabber-chatstates-kick-timer)))))

;;; COMMON

(defun jabber-chatstates--real-body-message-p (xml-data)
  "Return non-nil when XML-DATA has a body that should clear chatstates."
  (and (jabber-xml-get-children xml-data 'body)
       (not (jabber-reactions--reaction-only-p xml-data))))

(defun jabber-chatstates--handle-direct-state (buffer xml-data)
  "Update direct chat BUFFER from XML-DATA."
  (with-current-buffer buffer
      (cond
       ;; If we get an error message, we shouldn't report any
       ;; events, as the requests are mirrored from us.
       ((string= (jabber-xml-get-attribute xml-data 'type) "error")
        (remove-hook 'post-command-hook #'jabber-chatstates-after-change t)
        (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t)
        (setq jabber-chatstates-requested nil))

       (t
        (let ((state (jabber-chatstates--message-state xml-data))
              (body-message-p (jabber-chatstates--real-body-message-p
                               xml-data)))
          (when (and (eq state 'gone)
                     (bound-and-true-p jabber-message-thread-id))
            (jabber-chatstates-stop-timer)
            (setq jabber-chatstates-composing-sent nil)
            (jabber-message-thread--renew-id))
          ;; Set up hooks for composition notification
          (when (and jabber-chatstates-confirm state)
            (jabber-chatstates--enable-send-hooks nil))
          (when (and body-message-p (not state))
            (remove-hook 'post-command-hook #'jabber-chatstates-after-change t)
            (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t)
            (setq jabber-chatstates-requested nil))

          (when (or state body-message-p)
            (setq jabber-chatstates-last-state state)
            (jabber-chatstates--update-ewoc state)))))))

(defun jabber-chatstates--parent-buffer (jc from type)
  "Return FROM's ordinary chat buffer on JC for message TYPE."
  (if (equal type "groupchat")
      (jabber-muc-find-buffer (jabber-jid-user from) jc)
    (get-buffer (jabber-chat-get-buffer from jc))))

(defun jabber-chatstates--thread-buffer (jc from type thread-id)
  "Return THREAD-ID's open buffer for FROM and TYPE on JC."
  (let ((account (jabber-connection-bare-jid jc))
        (peer (jabber-jid-user from)))
    (if (equal type "error")
        (let ((chat (jabber-message-thread-find-buffer
                     account peer "chat" thread-id))
              (groupchat (jabber-message-thread-find-buffer
                          account peer "groupchat" thread-id)))
          (cond
           ((and chat groupchat) nil)
           (chat chat)
           (groupchat groupchat)))
      (jabber-message-thread-find-buffer
       account peer type thread-id))))

(defun jabber-chatstates--thread-state-buffer (jc from type thread-id)
  "Return THREAD-ID's chat-state buffer for FROM and TYPE on JC."
  (let* ((parent (jabber-chatstates--parent-buffer jc from type))
         (target
          (jabber-message-thread-chat-state-target
           jc (jabber-jid-user from) type thread-id parent)))
    (cond
     ((eq target 'parent) parent)
     ((buffer-live-p target) target))))

(defun jabber-chatstates--thread-content-buffer (jc xml-data from type)
  "Return threaded content XML-DATA's display buffer on JC."
  (let ((target
         (jabber-message-thread-display-target
          jc (jabber-jid-user from) type
          (jabber-chat--msg-plist-from-stanza xml-data))))
    (cond
     ((eq target 'parent)
      (jabber-chatstates--parent-buffer jc from type))
     ((buffer-live-p target) target))))

(defun jabber-chatstates--target-buffer (jc xml-data from type)
  "Return XML-DATA's chat-state buffer for FROM and TYPE on JC."
  (let ((fields (jabber-message-thread--fields xml-data))
        (body-message-p (jabber-chatstates--real-body-message-p xml-data)))
    (cond
     ((or (not jabber-message-thread-use-buffers)
          (and (equal type "chat") (jabber-muc-sender-p from)))
      (jabber-chatstates--parent-buffer jc from type))
     ((not (jabber-message-thread-protocol-has-core-p xml-data))
      (jabber-chatstates--parent-buffer jc from type))
     ((not fields)
      (and body-message-p
           (jabber-chatstates--parent-buffer jc from type)))
     ((equal type "error")
      (jabber-chatstates--thread-buffer
       jc from type (plist-get fields :thread-id)))
     (body-message-p
      (jabber-chatstates--thread-content-buffer jc xml-data from type))
     (t
      (jabber-chatstates--thread-state-buffer
       jc from type (plist-get fields :thread-id))))))

(defun jabber-handle-incoming-message-chatstates (jc xml-data)
  "Update the chat buffer's typing indicator from XML-DATA on JC."
  (when-let* ((from (jabber-xml-get-attribute xml-data 'from))
              (type (or (jabber-xml-get-attribute xml-data 'type) "chat"))
              (buffer (jabber-chatstates--target-buffer
                       jc xml-data from type)))
    (if (string= type "groupchat")
        (let ((state (jabber-chatstates--message-state xml-data)))
          (when (and (not (string= (jabber-xml-get-attribute xml-data 'type) "error"))
                     (or state (jabber-chatstates--real-body-message-p xml-data)))
            (jabber-chatstates--handle-muc-state buffer jc from state)))
      (jabber-chatstates--handle-direct-state buffer xml-data))))

(jabber-chain-add 'jabber-message-chain #'jabber-handle-incoming-message-chatstates 50)

(jabber-disco-advertise-feature jabber-chatstates-xmlns)

(provide 'jabber-chatstates)

;;; jabber-chatstates.el ends here
