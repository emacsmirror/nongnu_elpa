;;; hermes-transport-cli.el --- CLI fallback transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
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

;; The sanctioned `hermes chat -Q -q' one-shot subprocess fallback and the
;; `hermes-transport-send-function' seam.  This is smoke-test transport only;
;; the real frontend drives the dashboard WebSocket in
;; `hermes-dashboard-transport'.  Kept out of `hermes-transport', which stays
;; pure event/model normalization with no I/O.

;;; Code:

(require 'subr-x)
(require 'hermes-transport)

(defcustom hermes-command "hermes"
  "Hermes Agent command used by the fallback CLI transport.
Resolved at call time by `hermes-transport--resolve-command': a bare name
is searched on variable `exec-path', with ~/.local/bin/hermes as a fallback."
  :type 'string
  :group 'hermes)

(defun hermes-transport--resolve-command ()
  "Return the Hermes Agent executable to run for `hermes-command'."
  (or (executable-find hermes-command)
      (let ((local (expand-file-name "~/.local/bin/hermes")))
        (and (file-executable-p local) local))
      hermes-command))

(defun hermes-transport--command (prompt)
  "Return the Hermes CLI command for PROMPT."
  (list (hermes-transport--resolve-command) "chat" "-Q" "-q" prompt))

(defun hermes-transport--process-output (process)
  "Return PROCESS buffer contents, or an empty string."
  (let ((buffer (process-buffer process)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      "")))

(defun hermes-transport--start-event ()
  "Return fallback status event emitted before Hermes process startup."
  (list :type 'status
        :event "run.started"
        :status "running"
        :content "Starting Hermes"))

(defun hermes-transport-send (prompt callback)
  "Send PROMPT to Hermes asynchronously and report events to CALLBACK.

CALLBACK receives plist events:

  (:type delta :content STRING)  output chunk arrived
  (:type done)                   process exited successfully
  (:type error :content STRING)  process failed

Structured transports may additionally emit `status', `progress', `tool',
`commentary', and optional `diff' events through
`hermes-transport-normalize-event'.

Return the process object created by `make-process'."
  (hermes-transport--emit callback (hermes-transport--start-event))
  (let ((buffer (generate-new-buffer " *hermes-transport*")))
    (condition-case err
        (make-process
         :name "hermes-chat"
         :buffer buffer
         :command (hermes-transport--command prompt)
         :connection-type 'pipe
         :noquery t
         :filter (lambda (process chunk)
                   (when (buffer-live-p (process-buffer process))
                     (with-current-buffer (process-buffer process)
                       (goto-char (point-max))
                       (insert chunk)))
                   (unless (string-empty-p chunk)
                     (hermes-transport--emit
                      callback (list :type 'delta :content chunk))))
         :sentinel (lambda (process event)
                     (when (memq (process-status process) '(exit signal))
                       (unwind-protect
                           (if (zerop (process-exit-status process))
                               (hermes-transport--emit callback '(:type done))
                             (let ((message (string-trim
                                             (or (hermes-transport--process-output
                                                  process)
                                                 event))))
                               (hermes-transport--emit
                                callback
                                (list :type 'error
                                      :content (if (string-empty-p message)
                                                   event
                                                 message)))))
                         (when (buffer-live-p (process-buffer process))
                           (kill-buffer (process-buffer process)))))))
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal (car err) (cdr err))))))

(defcustom hermes-transport-send-function #'hermes-transport-send
  "Function used to send a prompt to Hermes.
The function is called with PROMPT and CALLBACK arguments.  CALLBACK receives
transport event plists as documented by `hermes-transport-send'."
  :type 'function
  :group 'hermes)

(provide 'hermes-transport-cli)
;;; hermes-transport-cli.el ends here
