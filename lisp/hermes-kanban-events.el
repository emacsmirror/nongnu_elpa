;;; hermes-kanban-events.el --- Live-events tail for Hermes Kanban  -*- lexical-binding: t; -*-

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

;; The live-events tail for a Hermes Kanban board buffer: a dedicated raw
;; WebSocket to `/api/plugins/kanban/events' whose plain `{events,cursor}'
;; JSON frames debounce an in-place board revert, with bounded-backoff
;; reconnect that stops when the buffer dies.  This stream is never routed
;; through the chat JSON-RPC client.  `hermes-kanban' requires this file and
;; keeps the board buffer, mode, and commands.

;;; Code:

(require 'cl-lib)
(require 'hermes-promise)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)

(declare-function websocket-close "ext:websocket")

;; Owned by hermes-kanban.el, which loads after this file.
(defvar hermes-kanban--slug)
(defvar hermes-kanban--latest-event-id)

(defvar-local hermes-kanban--events-tail nil
  "Live-events tail for this board buffer, or nil when live updates are off.")

(cl-defstruct (hermes-kanban--events-tail
               (:constructor hermes-kanban--events-tail-create))
  "State for one board buffer's live-events WebSocket."
  socket buffer slug (cursor 0) refresh-timer (backoff 1) reconnect-timer
  (active t))

(defconst hermes-kanban--events-debounce 0.4
  "Seconds to debounce an in-place board refresh from live events.")

(defconst hermes-kanban--events-backoff-max 30
  "Maximum reconnect backoff in seconds for the live-events tail.")

(defun hermes-kanban--live-indicator ()
  "Return the board mode-line live-status indicator.
Live is keyed on the socket, not the tail struct, so a tail waiting in the
reconnect backoff shows as retrying rather than falsely live."
  (cond
   ((null hermes-kanban--events-tail)
    (propertize " ○" 'face 'shadow))
   ((and (equal hermes-kanban--slug
                (hermes-kanban--events-tail-slug hermes-kanban--events-tail))
         (hermes-kanban--events-tail-socket hermes-kanban--events-tail))
    (propertize " ●live" 'face 'success))
   (t (propertize " ◌retry" 'face 'warning))))

(defun hermes-kanban--events-refresh (tail)
  "Refresh TAIL's board buffer in place when it is still live."
  (setf (hermes-kanban--events-tail-refresh-timer tail) nil)
  (let ((buffer (hermes-kanban--events-tail-buffer tail)))
    (when (and (hermes-kanban--events-tail-active tail) (buffer-live-p buffer))
      (with-current-buffer buffer (revert-buffer nil t)))))

(defun hermes-kanban--events-schedule-refresh (tail)
  "Debounce an in-place board refresh for TAIL."
  (when-let* ((timer (hermes-kanban--events-tail-refresh-timer tail)))
    (cancel-timer timer))
  (setf (hermes-kanban--events-tail-refresh-timer tail)
        (run-at-time hermes-kanban--events-debounce nil
                     #'hermes-kanban--events-refresh tail)))

(defun hermes-kanban--events-handle-frame (tail text &optional socket)
  "Advance TAIL's cursor from the JSON frame TEXT and schedule a refresh.
TEXT is a plain `{events,cursor}' frame, parsed on this socket alone -- never
through the chat client's JSON-RPC handler.  Optional SOCKET identifies the
connection that delivered TEXT so stale callbacks are ignored."
  (when (and (hermes-kanban--events-tail-active tail)
             (or (null socket)
                 (eq socket (hermes-kanban--events-tail-socket tail))))
    (setf (hermes-kanban--events-tail-backoff tail) 1)
    (when-let* ((frame (ignore-errors
                         (json-parse-string text :object-type 'alist
                                            :array-type 'list
                                            :null-object nil :false-object nil))))
      (let ((cursor (hermes-transport--get frame 'cursor)))
        (when (numberp cursor)
          (setf (hermes-kanban--events-tail-cursor tail) cursor)))
      (hermes-kanban--events-schedule-refresh tail))))

(defun hermes-kanban--events-reconnect (tail)
  "Schedule a bounded-backoff reconnect for TAIL, stopping when its buffer dies."
  (when (and (hermes-kanban--events-tail-active tail)
             (buffer-live-p (hermes-kanban--events-tail-buffer tail))
             (not (hermes-kanban--events-tail-reconnect-timer tail)))
    (let ((delay (hermes-kanban--events-tail-backoff tail)))
      (setf (hermes-kanban--events-tail-backoff tail)
            (min hermes-kanban--events-backoff-max (* 2 delay))
            (hermes-kanban--events-tail-reconnect-timer tail)
            (run-at-time delay nil #'hermes-kanban--events-do-reconnect tail)))))

(defun hermes-kanban--events-do-reconnect (tail)
  "Clear TAIL's reconnect timer and reconnect when still active."
  (setf (hermes-kanban--events-tail-reconnect-timer tail) nil)
  (when (and (hermes-kanban--events-tail-active tail)
             (buffer-live-p (hermes-kanban--events-tail-buffer tail)))
    (hermes-kanban--events-connect tail)))

(defun hermes-kanban--events-on-down (tail socket &optional message)
  "Drop TAIL's SOCKET when it is current, then reconnect with backoff.
Report optional MESSAGE only for the current connection."
  (when (or (null socket)
            (eq socket (hermes-kanban--events-tail-socket tail)))
    (when message (message "Hermes kanban live: %s" message))
    (setf (hermes-kanban--events-tail-socket tail) nil)
    (hermes-kanban--events-reconnect tail)))

(defun hermes-kanban--events-connect (tail)
  "Resolve the events URL for TAIL and open its socket.
A failed URL resolve or socket open re-enters the bounded backoff like a
dropped connection, instead of permanently killing the tail."
  (hermes--promise-then
   (hermes-dashboard-transport-kanban-events-url-async
    :since (hermes-kanban--events-tail-cursor tail)
    :board (hermes-kanban--events-tail-slug tail))
   (lambda (url)
     (when (hermes-kanban--events-tail-active tail)
       (condition-case err
           (let (socket)
             (setq socket
                   (hermes-dashboard-transport-open-websocket
                    (plist-get url :url) (plist-get url :redacted-url)
                    (plist-get url :secrets)
                    :on-message
                    (lambda (text)
                      (hermes-kanban--events-handle-frame tail text socket))
                    :on-close
                    (lambda () (hermes-kanban--events-on-down tail socket))
                    :on-error
                    (lambda (msg)
                      (hermes-kanban--events-on-down tail socket msg))))
             (setf (hermes-kanban--events-tail-socket tail) socket))
         (error (hermes-kanban--events-on-down
                 tail nil (error-message-string err))))))
   (lambda (reason)
     (hermes-kanban--events-on-down tail nil (format "%s" reason)))))

(defun hermes-kanban--events-disconnect (tail)
  "Tear down TAIL: stop reconnecting, cancel timers, and close the socket."
  (setf (hermes-kanban--events-tail-active tail) nil)
  (when-let* ((timer (hermes-kanban--events-tail-refresh-timer tail)))
    (cancel-timer timer))
  (when-let* ((timer (hermes-kanban--events-tail-reconnect-timer tail)))
    (cancel-timer timer))
  (setf (hermes-kanban--events-tail-refresh-timer tail) nil
        (hermes-kanban--events-tail-reconnect-timer tail) nil)
  (when-let* ((socket (hermes-kanban--events-tail-socket tail)))
    (when (fboundp 'websocket-close) (ignore-errors (websocket-close socket))))
  (setf (hermes-kanban--events-tail-socket tail) nil))

(defun hermes-kanban--events-teardown ()
  "Disconnect the board buffer's events tail when the buffer is killed."
  (when hermes-kanban--events-tail
    (hermes-kanban--events-disconnect hermes-kanban--events-tail)
    (setq hermes-kanban--events-tail nil)))

(defun hermes-kanban--events-retarget (slug cursor)
  "Retarget an enabled live tail to SLUG, seeding it from CURSOR."
  (when (and hermes-kanban--events-tail
             (not (equal slug
                         (hermes-kanban--events-tail-slug
                          hermes-kanban--events-tail))))
    (hermes-kanban--events-disconnect hermes-kanban--events-tail)
    (setq hermes-kanban--events-tail
          (hermes-kanban--events-tail-create
           :buffer (current-buffer) :slug slug :cursor (or cursor 0)))
    (hermes-kanban--events-connect hermes-kanban--events-tail)
    (force-mode-line-update)))

(defun hermes-kanban-toggle-live ()
  "Toggle the live-events tail for the current board buffer.
When on, a dedicated WebSocket streams task events and the board refreshes in
place; the mode line shows a live indicator."
  (interactive)
  (unless (derived-mode-p 'hermes-kanban-mode)
    (user-error "Live updates are only available on a board buffer"))
  (if hermes-kanban--events-tail
      (progn
        (hermes-kanban--events-disconnect hermes-kanban--events-tail)
        (setq hermes-kanban--events-tail nil)
        (force-mode-line-update)
        (message "Hermes kanban live updates off"))
    (let ((tail (hermes-kanban--events-tail-create
                 :buffer (current-buffer) :slug hermes-kanban--slug
                 :cursor (or hermes-kanban--latest-event-id 0))))
      (setq hermes-kanban--events-tail tail)
      (add-hook 'kill-buffer-hook #'hermes-kanban--events-teardown nil t)
      (force-mode-line-update)
      (hermes-kanban--events-connect tail)
      (message "Hermes kanban live updates on"))))

(provide 'hermes-kanban-events)
;;; hermes-kanban-events.el ends here
