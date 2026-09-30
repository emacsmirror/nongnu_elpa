;;; jabber-muc-menu.el --- Multi-user chat command menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Assemble the MUC popup after its protocol and information providers load.

;;; Code:

(require 'keymap-popup)
(require 'jabber-info)
(require 'jabber-muc)

(defvar jabber-muc-menu-map)

;; Newer popup revisions generate these launchers at first initialization.
(declare-function jabber-muc-menu-map-popup "jabber-muc-menu" () t)

(unless (boundp 'jabber-muc-menu-map)
  (keymap-popup-define jabber-muc-menu-map
    "Jabber MUC commands."
    :description (lambda ()
                   (if (bound-and-true-p jabber-group)
                       (format "MUC actions for %s"
                               (propertize jabber-group 'face
                                           'font-lock-constant-face))
                     "MUC actions"))
    :group "Room"
    "j" ("Join" jabber-muc-join)
    "J" ("Create room" jabber-muc-create)
    "l" ("Leave" jabber-muc-leave)
    "t" ("Set topic" jabber-muc-set-topic)
    "c" ("Configure" jabber-muc-get-config)
    :group "Participants"
    "n" ("Change nick" jabber-muc-nick)
    "I" ("Get info" jabber-muc-get-info)
    "i" ("Invite" jabber-muc-invite)
    "w" ("List participants" jabber-muc-names)
    "p" ("Private chat" jabber-muc-private)
    "v" ("Request vcard" jabber-muc-vcard-get)
    :group "Admin"
    "r" ("Set role" jabber-muc-set-role)
    "a" ("Set affiliation" jabber-muc-set-affiliation)))

(defun jabber-muc-menu ()
  "Show the Jabber MUC command menu."
  (interactive)
  (keymap-popup jabber-muc-menu-map))

(provide 'jabber-muc-menu)

;;; jabber-muc-menu.el ends here
