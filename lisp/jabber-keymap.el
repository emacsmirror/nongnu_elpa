;;; jabber-keymap.el --- Shared Jabber keymaps  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Shared keymaps are assembled from command symbols without loading their
;; owning feature modules.  The main Jabber entry point establishes the full
;; feature load order.

;;; Code:

(require 'keymap-popup)

;; Feature modules depend on these shared maps, so keep their loading lazy.
(declare-function jabber-info-menu "jabber-disco-menu" ())
(declare-function jabber-service-menu "jabber-disco-menu" ())
(declare-function jabber-muc-menu "jabber-muc-menu" ())
(declare-function jabber-connect-all "jabber-core" (&optional arg))
(declare-function jabber-disconnect "jabber-core" (&optional arg interactivep))
(declare-function jabber-roster-popup "jabber-roster-menu" ())
(declare-function jabber-chat-with "jabber-chat" (jc jid &optional other-window))
(declare-function jabber-activity-switch-to "jabber-activity" (&optional jid-param))
(declare-function jabber-send-away-presence "jabber-presence" (&optional status jc))
(declare-function jabber-send-default-presence "jabber-presence" (&optional jc))
(declare-function jabber-send-xa-presence "jabber-presence" (&optional status jc))
(declare-function jabber-send-presence "jabber-presence" (show status priority &optional jc))
(declare-function jabber-chat-buffer-switch "jabber-chatbuffer" ())
(declare-function jabber-muc-join "jabber-muc" (jc group nickname &optional popup))

(defconst jabber-keymap--global-bindings
  '(("C-c" "Connect" jabber-connect-all)
    ("C-d" "Disconnect" jabber-disconnect)
    ("C-r" "Roster" jabber-roster-popup)
    ("C-j" "Chat with" jabber-chat-with)
    ("C-l" "Next unread" jabber-activity-switch-to)
    ("C-a" "Away" jabber-send-away-presence)
    ("C-o" "Online" jabber-send-default-presence)
    ("C-x" "Extended away" jabber-send-xa-presence)
    ("C-p" "Set presence" jabber-send-presence)
    ("C-b" "Switch buffer" jabber-chat-buffer-switch)
    ("C-m" "Join MUC" jabber-muc-join))
  "Bindings exposed through `jabber-global-keymap'.")

(defvar jabber-common-keymap)
(defvar jabber-global-keymap)

(defun jabber-common-menu ()
  "Show common Jabber commands."
  (interactive)
  (keymap-popup jabber-common-keymap))

(defun jabber-global-menu ()
  "Show global Jabber commands."
  (interactive)
  (keymap-popup jabber-global-keymap))

(unless (boundp 'jabber-common-keymap)
  (defvar-keymap jabber-common-keymap
    :doc "Common Jabber commands."
    :parent special-mode-map
    "h" #'jabber-common-menu
    "TAB" #'forward-button
    "<backtab>" #'backward-button
    "C-c C-i" #'jabber-info-menu
    "C-c C-m" #'jabber-muc-menu
    "C-c C-s" #'jabber-service-menu)

  (keymap-popup-annotate jabber-common-keymap
    forward-button "Next button"
    backward-button "Previous button"
    jabber-info-menu "Info/Discovery"
    jabber-muc-menu "MUC"
    jabber-service-menu "Services"))

(unless (boundp 'jabber-global-keymap)
  (defvar jabber-global-keymap
    (let ((map (make-sparse-keymap)))
      (keymap-set map "h" #'jabber-global-menu)
      (dolist (binding jabber-keymap--global-bindings)
        (keymap-set map (car binding) (caddr binding)))
      (define-key ctl-x-map "\C-j" map)
      map)
    "Global Jabber commands.")

  (keymap-popup-annotate jabber-global-keymap
    jabber-connect-all "Connect"
    jabber-disconnect "Disconnect"
    jabber-roster-popup "Roster"
    jabber-chat-with "Chat with"
    jabber-activity-switch-to "Next unread"
    jabber-send-away-presence "Away"
    jabber-send-default-presence "Online"
    jabber-send-xa-presence "Extended away"
    jabber-send-presence "Set presence"
    jabber-chat-buffer-switch "Switch buffer"
    jabber-muc-join "Join MUC"))

(provide 'jabber-keymap)

;;; jabber-keymap.el ends here
