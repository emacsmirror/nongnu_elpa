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

(declare-function jabber-ping-send "jabber-ping.el"
                  (jc to process-func on-success on-error))
(declare-function jabber-browse--buffer "jabber-iq.el" (jid))
(declare-function jabber-browse--insert "jabber-iq.el"
                  (jc xml-data closure-data))
(declare-function jabber-send-iq "jabber-iq.el"
                  (jc to type query success-callback success-closure-data
                      error-callback error-closure-data &optional result-id))
(declare-function jabber-muc-read-nickname "jabber-muc.el"
                  (group prompt))
(declare-function jabber-process-version "jabber-version.el"
                  (jc xml-data))
(declare-function jabber-process-disco-info "jabber-disco.el"
                  (jc xml-data))
(declare-function jabber-process-ping "jabber-ping.el"
                  (jc xml-data))
(declare-function jabber-process-time "jabber-time.el"
                  (jc xml-data))
(declare-function jabber-process-last "jabber-time.el"
                  (jc xml-data))

(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-buffer-connection)        ; jabber-chatbuffer.el

(defun jabber-info--connected-resources (bare-jid)
  "Return list of full JIDs for connected resources of BARE-JID."
  (let* ((sym (jabber-jid-symbol bare-jid))
         (resources (get sym 'resources))
         (full-jids nil))
    (dolist (entry resources)
      (when (plist-get (cdr entry) 'connected)
        (push (concat bare-jid "/" (car entry)) full-jids)))
    (nreverse full-jids)))

(defun jabber-info--make-marker ()
  "Return an advancing marker at point.
The marker has insertion-type t so it advances past text
inserted at its position."
  (let ((m (point-marker)))
    (set-marker-insertion-type m t)
    m))

(defun jabber-info--make-callback (marker)
  "Return a callback that inserts results at MARKER position."
  (lambda (jc xml-data closure-data)
    (when (buffer-live-p (marker-buffer marker))
      (with-current-buffer (marker-buffer marker)
        (let ((inhibit-read-only t))
          (goto-char marker)
          (save-excursion
            (cond
             ((functionp closure-data)
              (let ((result (funcall closure-data jc xml-data)))
                (when (stringp result)
                  (insert result "\n\n"))))
             ((stringp closure-data)
              (insert closure-data ": "
                      (jabber-parse-error (jabber-iq-error xml-data)) "\n\n"))
             (t
              (insert (format "%S\n\n" xml-data))))))))))

(defun jabber-info--query-bare (jc bare-jid marker)
  "Fire queries appropriate for a bare JID.
Results are inserted at MARKER in its buffer."
  (let ((cb (jabber-info--make-callback marker)))
    (jabber-send-iq jc bare-jid "get"
                    '(query ((xmlns . "http://jabber.org/protocol/disco#info")))
                    cb #'jabber-process-disco-info
                    cb "Disco info request failed")
    (jabber-send-iq jc bare-jid "get"
                    '(query ((xmlns . "jabber:iq:last")))
                    cb #'jabber-process-last
                    cb "Last online request failed")))

(defun jabber-info--query-resource (jc full-jid marker)
  "Fire queries appropriate for FULL-JID.
Results are inserted at MARKER in its buffer."
  (let ((cb (jabber-info--make-callback marker)))
    (jabber-send-iq jc full-jid "get"
                    '(query ((xmlns . "jabber:iq:version")))
                    cb #'jabber-process-version
                    cb "Version request failed")
    (jabber-send-iq jc full-jid "get"
                    '(query ((xmlns . "http://jabber.org/protocol/disco#info")))
                    cb #'jabber-process-disco-info
                    cb "Disco info request failed")
    (jabber-ping-send jc full-jid
                      cb #'jabber-process-ping "Ping is unsupported")
    (jabber-send-iq jc full-jid "get"
                    '(time ((xmlns . "urn:xmpp:time")))
                    cb #'jabber-process-time
                    cb "Time request failed")
    (jabber-send-iq jc full-jid "get"
                    '(query ((xmlns . "jabber:iq:last")))
                    cb #'jabber-process-last
                    cb "Idle time request failed")))

;;;###autoload
(defun jabber-get-info (jc to)
  "Query JC for all available info about TO.
For a bare JID, queries disco info, last activity, and also
queries each connected resource for version, disco, ping, time
and idle time.  For a full JID, queries the resource directly.
Results appear in the browse buffer for TO."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing
                      "Get info for: " nil nil nil 'full t)))
  (let* ((bare (jabber-jid-user to))
         (resource (jabber-jid-resource to))
         (full-jids (if resource
                        (list to)
                      (jabber-info--connected-resources bare))))
    (let ((buf (jabber-browse--buffer bare)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize bare 'face 'jabber-title) "\n\n")
          (if resource
              (jabber-info--query-resource
               jc to (jabber-info--make-marker))
            ;; Build the full outline skeleton, then create markers.
            (insert (propertize "* Account:" 'face 'jabber-title) "\n\n")
            (let ((account-pos (point)))
              (if full-jids
                  (let ((resource-entries nil))
                    (insert (propertize "* Clients:" 'face 'jabber-title)
                            "\n\n")
                    (dolist (full-jid full-jids)
                      (insert (propertize
                               (concat "** " (jabber-jid-resource full-jid)
                                       ":")
                               'face 'jabber-title)
                              "\n\n")
                      (push (cons full-jid (point)) resource-entries))
                    ;; Create markers and fire queries.
                    (goto-char account-pos)
                    (jabber-info--query-bare
                     jc bare (jabber-info--make-marker))
                    (dolist (entry (nreverse resource-entries))
                      (goto-char (cdr entry))
                      (jabber-info--query-resource
                       jc (car entry) (jabber-info--make-marker))))
                (jabber-info--query-bare
                 jc bare (jabber-info--make-marker))
                (goto-char (point-max))
                (insert "No connected resources found.\n\n"))))))
      (display-buffer buf))))

;;;###autoload
(defun jabber-muc-get-info (jc group nickname)
  "Query version, disco info and ping for NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (let* ((full-jid (format "%s/%s" group nickname))
         (buf (jabber-browse--buffer full-jid)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize full-jid 'face 'jabber-title) "\n\n")
        (jabber-info--query-resource
         jc full-jid (jabber-info--make-marker))))
    (display-buffer buf)))

(provide 'jabber-info)
;;; jabber-info.el ends here
