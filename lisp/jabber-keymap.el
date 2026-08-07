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

(defconst jabber-keymap--common-bindings
  '(("C-c C-i" "Info/Discovery" jabber-info-menu)
    ("C-c C-m" "MUC" jabber-muc-menu)
    ("C-c C-s" "Services" jabber-service-menu))
  "Bindings shared by Jabber `special-mode' maps.")

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

(defun jabber-keymap--add-bindings (keymap bindings)
  "Install BINDINGS and their popup descriptions in KEYMAP."
  (dolist (binding bindings)
    (pcase-let ((`(,key ,description ,command) binding))
      (keymap-popup-add-entry keymap key description command))))

(keymap-popup-define jabber-common-keymap
  "Common Jabber commands."
  :parent special-mode-map
  "TAB" ("Next button" forward-button)
  "<backtab>" ("Previous button" backward-button))

(jabber-keymap--add-bindings jabber-common-keymap
                            jabber-keymap--common-bindings)

(keymap-popup-define jabber-global-keymap
  "Global Jabber commands."
  "C-g" ("Quit" keyboard-quit))

;; keymap-popup 0.3 cannot add entries before a map has popup metadata.
(jabber-keymap--add-bindings jabber-global-keymap
                            jabber-keymap--global-bindings)
(keymap-popup-remove-entry jabber-global-keymap "C-g")
(define-key ctl-x-map "\C-j" jabber-global-keymap)

(provide 'jabber-keymap)

;;; jabber-keymap.el ends here
