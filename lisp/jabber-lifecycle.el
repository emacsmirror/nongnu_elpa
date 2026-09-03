;;; jabber-lifecycle.el --- Connection lifecycle effect dispatch  -*- lexical-binding: t; -*-

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

;; Keeps the connection FSM independent from feature lifecycle effects.

;;; Code:

(defvar jabber-lifecycle-session-reset-functions nil
  "Functions called with a connection when its logical session is lost.")

(defvar jabber-lifecycle-session-bootstrap-functions nil
  "Functions called with a connection after a new session is established.")

(defvar jabber-lifecycle-registration-functions nil
  "Functions called with a connection entering account registration.")

(defvar jabber-lifecycle-connection-list-changed-functions nil
  "Functions called after the live connection list changes.")

(defun jabber-lifecycle--call-contained (function &rest arguments)
  "Call FUNCTION with ARGUMENTS, containing callback failures."
  (condition-case err
      (apply function arguments)
    ((error quit)
     (message "Jabber lifecycle callback failed: %s"
              (error-message-string err))))
  nil)

(defun jabber-lifecycle--dispatch-contained (hook &rest arguments)
  "Run HOOK with ARGUMENTS, containing each callback failure."
  (apply #'run-hook-wrapped
         hook #'jabber-lifecycle--call-contained arguments))

(defun jabber-lifecycle-dispatch-session-reset (jc)
  "Dispatch logical session reset effects for JC."
  (jabber-lifecycle--dispatch-contained
   'jabber-lifecycle-session-reset-functions jc))

(defun jabber-lifecycle-dispatch-session-bootstrap (jc)
  "Dispatch new session bootstrap effects for JC."
  (jabber-lifecycle--dispatch-contained
   'jabber-lifecycle-session-bootstrap-functions jc))

(defun jabber-lifecycle-dispatch-registration (jc)
  "Dispatch account registration startup for JC."
  (run-hook-with-args 'jabber-lifecycle-registration-functions jc))

(defun jabber-lifecycle-dispatch-connection-list-changed ()
  "Dispatch effects of a change to the live connection list."
  (jabber-lifecycle--dispatch-contained
   'jabber-lifecycle-connection-list-changed-functions))

(provide 'jabber-lifecycle)
;;; jabber-lifecycle.el ends here
