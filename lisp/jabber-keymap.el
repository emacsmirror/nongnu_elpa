;;; jabber-keymap.el --- common keymap for many modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

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

;;; Code:

(require 'jabber-menu)
(require 'button t nil)

;; Global reference declarations

(declare-function jabber-send-presence "jabber-presence.el" (show status priority))
(declare-function jabber-send-xa-presence "jabber-presence.el" (&optional status))
(declare-function jabber-send-default-presence "jabber-presence.el" (&optional _ignore))
(declare-function jabber-send-away-presence "jabber-presence.el" (&optional status))
(declare-function jabber-activity-switch-to "lisp/jabber-activity.el" (&optional jid-param))
(declare-function jabber-chat-with "jabber-chat.el" (jc jid &optional other-window))
(declare-function jabber-chat-muc-presence-patterns-select "jabber-chat.el" ())
(declare-function jabber-roster "jabber-roster.el" ())
(declare-function jabber-disconnect "jabber-core.el" (&optional arg interactivep))
(declare-function jabber-connect-all "jabber-core.el" (&optional arg))
(declare-function jabber-chat-buffer-switch "jabber-chat.el")
(declare-function jabber-muc-switch "jabber-muc.el" (group))
(declare-function jabber-chat-menu "jabber-menu.el")
(declare-function jabber-roster-context-menu "jabber-menu.el")
(declare-function jabber-info-menu "jabber-menu.el")
(declare-function jabber-muc-menu "jabber-menu.el")
(declare-function jabber-service-menu "jabber-menu.el")
;;

(defvar jabber-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'jabber-chat-menu)
    (define-key map "\C-c\C-r" #'jabber-roster-context-menu)
    (define-key map "\C-c\C-i" #'jabber-info-menu)
    (define-key map "\C-c\C-m" #'jabber-muc-menu)
    (define-key map "\C-c\C-s" #'jabber-service-menu)
    ;; note that {forward,backward}-button are not autoloaded.
    ;; thus the `require' above.
    (when (fboundp 'forward-button)
      (define-key map [?\t] #'forward-button)
      (define-key map [backtab] #'backward-button))
    map))

;;;###autoload
(defvar jabber-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" #'jabber-connect-all)
    (define-key map "\C-d" #'jabber-disconnect)
    (define-key map "\C-r" #'jabber-roster)
    (define-key map "\C-j" #'jabber-chat-with)
    (define-key map "\C-l" #'jabber-activity-switch-to)
    (define-key map "\C-a" #'jabber-send-away-presence)
    (define-key map "\C-o" #'jabber-send-default-presence)
    (define-key map "\C-y" #'jabber-chat-muc-presence-patterns-select)
    (define-key map "\C-x" #'jabber-send-xa-presence)
    (define-key map "\C-p" #'jabber-send-presence)
    (define-key map "\C-b" #'jabber-chat-buffer-switch)
    (define-key map "\C-m" #'jabber-muc-switch)
    map)
  "Global Jabber keymap (usually under C-x C-j).")

;;;###autoload
(define-key ctl-x-map "\C-j" jabber-global-keymap)

(provide 'jabber-keymap)
;;; jabber-keymap.el ends here
