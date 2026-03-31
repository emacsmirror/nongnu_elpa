;;; jabber-info.el --- aggregate info queries  -*- lexical-binding: t; -*-

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

(require 'jabber-util)
(eval-when-compile (require 'jabber-muc)) ; jabber-muc-argument-list macro

;; Global reference declarations

(declare-function jabber-get-version "jabber-version.el" (jc to))
(declare-function jabber-get-disco-info "jabber-disco.el" (jc to &optional node))
(declare-function jabber-ping-send "jabber-ping.el"
                  (jc to process-func on-success on-error))
(declare-function jabber-browse--buffer "jabber-iq.el" (jid))
(declare-function jabber-muc-read-nickname "jabber-muc.el" (group prompt))

(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-buffer-connection)        ; jabber-chatbuffer.el

;;;###autoload
(defun jabber-get-info (jc to)
  "Query JC for version, disco info and ping of TO in parallel.
Results appear in the browse buffer for TO."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing
                      "Get info for: " nil nil nil 'full t)))
  (with-current-buffer (jabber-browse--buffer to)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize to 'face 'jabber-title) "\n\n")))
  (jabber-get-version jc to)
  (jabber-get-disco-info jc to)
  (jabber-ping-send jc to
                    #'jabber-process-data #'jabber-process-ping
                    "Ping is unsupported"))

;;;###autoload
(defun jabber-muc-get-info (jc group nickname)
  "Query version, disco info and ping for NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (jabber-get-info jc (format "%s/%s" group nickname)))

(provide 'jabber-info)
;;; jabber-info.el ends here
