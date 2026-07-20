;;; hermes-notifications.el --- Desktop notifications for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared notification policy and the optional notifications.el boundary.
;; Feature modules decide which domain event occurred; this module applies the
;; user policy, focus suppression, safe previews, and click-to-open behavior.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar notifications-on-action-map nil)
(defvar notifications-on-action-object nil)

(declare-function dbus-unregister-object "dbusbind.c")

(defcustom hermes-notifications-events
  '(chat-reply chat-error prompt background kanban-attention cron-failure)
  "Hermes events that raise desktop notifications.
The default covers completed chat replies, terminal chat errors, input
requests, background-task results, and Kanban states that need attention.
Cron failures are included when cron failure monitoring is enabled.
Add `kanban-done' to notify for ordinary successful task completion.  An empty
set disables automatic Hermes notifications."
  :type '(set
          (const :tag "Completed chat replies" chat-reply)
          (const :tag "Terminal chat errors" chat-error)
          (const :tag "Approval and input requests" prompt)
          (const :tag "Completed background tasks" background)
          (const :tag "Kanban review, blocked, and failed tasks"
                 kanban-attention)
          (const :tag "Failed cron jobs" cron-failure)
          (const :tag "Completed Kanban tasks" kanban-done))
  :group 'hermes)

(defun hermes-notifications-enabled-p (event)
  "Return non-nil when notification EVENT is enabled.
Nil EVENT denotes an unconditional compatibility notification."
  (or (null event) (memq event hermes-notifications-events)))

(defun hermes-notifications-preview (text &optional width)
  "Return plain one-line TEXT truncated to WIDTH columns.
WIDTH defaults to 160 columns."
  (truncate-string-to-width
   (string-trim
    (replace-regexp-in-string
     "[[:space:]\n\r]+" " " (substring-no-properties (or text ""))))
   (or width 160) nil nil "…"))

(defun hermes-notifications--buffer-attended-p (buffer)
  "Return whether BUFFER is visible on the focused selected frame."
  (and (buffer-live-p buffer)
       (frame-focus-state)
       (get-buffer-window buffer (selected-frame))))

(defun hermes-notifications--remove-action-callback (callback)
  "Remove CALLBACK from pending desktop notification actions."
  (setq notifications-on-action-map
        (cl-delete callback notifications-on-action-map
                   :key #'cadr :test #'eq))
  (when (and (null notifications-on-action-map)
             notifications-on-action-object
             (fboundp 'dbus-unregister-object))
    (dbus-unregister-object notifications-on-action-object)
    (setq notifications-on-action-object nil)))

(defun hermes-notifications--fallback (title body)
  "Show notification TITLE and BODY in the echo area and return nil."
  (message "%s: %s" title body)
  nil)

(cl-defun hermes-notifications-notify
    (event title body &key buffer open urgency category)
  "Notify for EVENT with TITLE and BODY.
Skip disabled events and a BUFFER already visible on the focused frame.  A
click calls OPEN when supplied, otherwise it displays BUFFER.  URGENCY and
CATEGORY are passed to `notifications-notify'.  Nil EVENT bypasses the event
policy for compatibility callers."
  (when (and (hermes-notifications-enabled-p event)
             (not (and buffer
                       (hermes-notifications--buffer-attended-p buffer))))
    (letrec ((action
              (and (or buffer open)
                   (lambda (&rest _)
                     (when (or (null buffer) (buffer-live-p buffer))
                       (if open (funcall open) (pop-to-buffer buffer))))))
             (closed
              (and action
                   (lambda (&rest _)
                     (hermes-notifications--remove-action-callback action))))
             (arguments
              (append
               (list :title title :body body :app-name "Hermes")
               (and urgency (list :urgency urgency))
               (and category (list :category category))
               (and action
                    (list :actions '("default" "Open in Emacs")
                          :on-action action :on-close closed))))
             (id
              (and (require 'notifications nil t)
                   (fboundp 'notifications-notify)
                   (condition-case nil
                       (apply #'notifications-notify arguments)
                     (error nil)))))
      (or id (hermes-notifications--fallback title body)))))

(provide 'hermes-notifications)
;;; hermes-notifications.el ends here
