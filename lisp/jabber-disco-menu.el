;;; jabber-disco-menu.el --- Service discovery command menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Collect discovery-related commands after their feature modules load.

;;; Code:

(require 'keymap-popup)
(require 'jabber-ahc)
(require 'jabber-browse)
(require 'jabber-carbons)
(require 'jabber-disco)
(require 'jabber-info)
(require 'jabber-ping)
(require 'jabber-register)
(require 'jabber-search)
(require 'jabber-time)
(require 'jabber-vcard)
(require 'jabber-version)

(defvar jabber-info-menu-map)
(defvar jabber-service-menu-map)

;; Newer popup revisions generate these launchers at first initialization.
(declare-function jabber-info-menu-map-popup "jabber-disco-menu" () t)
(declare-function jabber-service-menu-map-popup "jabber-disco-menu" () t)

(unless (boundp 'jabber-info-menu-map)
  (keymap-popup-define jabber-info-menu-map
    "Jabber info/discovery commands."
    :group "Discovery"
    "I" ("Get info" jabber-get-info)
    "i" ("Disco items" jabber-get-disco-items)
    "d" ("Disco info" jabber-get-disco-info)
    "b" ("Browse" jabber-get-browse)
    "v" ("Client version" jabber-get-version)
    "p" ("Ping" jabber-ping)
    "t" ("Request time" jabber-get-time)
    "V" ("View vCard" jabber-vcard-get)))

(defun jabber-info-menu ()
  "Jabber info/discovery commands."
  (interactive)
  (keymap-popup jabber-info-menu-map))

(unless (boundp 'jabber-service-menu-map)
  (keymap-popup-define jabber-service-menu-map
    "Jabber service commands."
    :group "Services"
    "r" ("Register" jabber-get-register)
    "s" ("Search directory" jabber-get-search)
    "c" ("Execute command" jabber-ahc-execute-command)
    "l" ("Command list" jabber-ahc-get-list)
    "C" ("Enable carbons" jabber-enable-carbons)))

(defun jabber-service-menu ()
  "Jabber service commands."
  (interactive)
  (keymap-popup jabber-service-menu-map))

(provide 'jabber-disco-menu)

;;; jabber-disco-menu.el ends here
