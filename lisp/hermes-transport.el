;;; hermes-transport.el --- Async Hermes transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

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

;; Small asynchronous transport boundary for hermes-el.  UI code calls
;; `hermes-transport-send-function', which may be rebound in tests or by users.

;;; Code:

(require 'subr-x)

(defgroup hermes nil
  "Emacs frontend for Hermes Agent."
  :group 'applications)

(defun hermes-transport-default-command ()
  "Return the preferred Hermes Agent executable path."
  (or (executable-find "hermes")
      (let ((local (expand-file-name "~/.local/bin/hermes")))
        (and (file-executable-p local) local))
      "hermes"))

(defcustom hermes-command (hermes-transport-default-command)
  "Hermes Agent command used by the default CLI transport."
  :type 'string
  :group 'hermes)

(defun hermes-transport--command (prompt)
  "Return the Hermes CLI command for PROMPT."
  (list hermes-command "chat" "-Q" "-q" prompt))

(defun hermes-transport--process-output (process)
  "Return PROCESS buffer contents, or an empty string."
  (let ((buffer (process-buffer process)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      "")))

(defun hermes-transport-send (prompt callback)
  "Send PROMPT to Hermes asynchronously and report events to CALLBACK.

CALLBACK receives plist events:

  (:type delta :content STRING)  output chunk arrived
  (:type done)                   process exited successfully
  (:type error :content STRING)  process failed

Return the process object created by `make-process'."
  (let ((buffer (generate-new-buffer " *hermes-transport*")))
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
                 (funcall callback (list :type 'delta :content chunk))))
     :sentinel (lambda (process event)
                 (when (memq (process-status process) '(exit signal))
                   (unwind-protect
                       (if (zerop (process-exit-status process))
                           (funcall callback '(:type done))
                         (let ((message (string-trim
                                         (or (hermes-transport--process-output
                                              process)
                                             event))))
                           (funcall callback
                                    (list :type 'error
                                          :content (if (string-empty-p message)
                                                       event
                                                     message)))))
                     (when (buffer-live-p (process-buffer process))
                       (kill-buffer (process-buffer process)))))))))

(defcustom hermes-transport-send-function #'hermes-transport-send
  "Function used to send a prompt to Hermes.
The function is called with PROMPT and CALLBACK arguments.  CALLBACK receives
transport event plists as documented by `hermes-transport-send'."
  :type 'function
  :group 'hermes)

(provide 'hermes-transport)
;;; hermes-transport.el ends here
