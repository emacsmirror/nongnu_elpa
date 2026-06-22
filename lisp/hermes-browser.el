;;; hermes-browser.el --- Shared foundation for Hermes dashboard browsers  -*- lexical-binding: t; -*-

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

;; Foundation shared by the dashboard browser views (sessions, rollback,
;; cron, subagents, inventory, ...).  It provides dashboard client
;; provisioning -- reusing a live chat connection when one exists, else a
;; transient client released when its work settles -- so each browser
;; composes its RPC over a promise without re-implementing the plumbing.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-chat)

(defun hermes-browser--existing-client ()
  "Return a live dashboard client from any Hermes chat buffer, or nil."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'hermes-chat-mode)
                    (hermes-chat--dashboard-client-live-p
                     hermes-chat--dashboard-client)
                    hermes-chat--dashboard-client)))
           (buffer-list)))

(defun hermes-browser--with-client (fn)
  "Call FN with a connected CLIENT and a DONE cleanup thunk.
Reuses a live chat connection when one exists; otherwise connects a transient
client that DONE stops.  Shared by the dashboard browser commands."
  (let* ((existing (hermes-browser--existing-client))
         (client (or existing
                     (hermes-dashboard-transport-start :callback #'ignore)))
         (done (lambda ()
                 (unless existing
                   (hermes-dashboard-transport-stop client)))))
    (funcall fn client done)))

(defun hermes-browser--run-on-client (make-promise &optional on-success)
  "Run MAKE-PROMISE on a dashboard client, releasing it when its promise settles.
MAKE-PROMISE receives the CLIENT and returns a promise.  ON-SUCCESS, when given,
receives the resolved result; rejections are reported with a `Hermes:' message.
Shared by the dashboard browser commands."
  (hermes-browser--with-client
   (lambda (client done)
     (hermes--promise-catch
      (hermes--promise-then
       (hermes--promise-finally (funcall make-promise client) done)
       on-success)
      (lambda (message) (message "Hermes: %s" message))))))

(defmacro hermes-define-list-browser (name &rest body)
  "Define a `tabulated-list' browser NAME backed by a dashboard RPC.
NAME is the short browser name; the macro defines `hermes-NAME-mode',
`hermes-NAME-mode-map', `hermes-NAME--render', `hermes-NAME--revert', and the
command `hermes-list-NAME'.  BODY is a plist:

  :title        display/mode-line name (string)
  :buffer       browser buffer name (string)
  :columns      `tabulated-list-format' vector
  :command      list-command symbol when it differs from `hermes-list-NAME';
                it must match the caller's `(autoload ...)' cookie
  :fetch        function (CLIENT -> promise) issuing the dashboard RPC
  :rows         pure function (RESULT -> list of `tabulated-list' entries)
  :keys         extra bindings, spliced into `defvar-keymap'
  :doc          major-mode docstring, optional
  :command-doc  list-command docstring, optional

`:fetch' and `:rows' must be pure: this macro owns the only side effects -- the
buffer render and the dashboard client plumbing."
  (declare (indent 1))
  (let ((mode (intern (format "hermes-%s-mode" name)))
        (map (intern (format "hermes-%s-mode-map" name)))
        (render (intern (format "hermes-%s--render" name)))
        (revert (intern (format "hermes-%s--revert" name)))
        (command (or (plist-get body :command)
                     (intern (format "hermes-list-%s" name))))
        (title (plist-get body :title))
        (buffer (plist-get body :buffer))
        (columns (plist-get body :columns))
        (fetch (plist-get body :fetch))
        (rows (plist-get body :rows))
        (keys (plist-get body :keys))
        (doc (plist-get body :doc))
        (command-doc (plist-get body :command-doc)))
    `(progn
       (defvar-keymap ,map
         :doc ,(format "Keymap for `%s'." mode)
         :parent tabulated-list-mode-map
         ,@keys)
       (define-derived-mode ,mode tabulated-list-mode ,title
         ,(or doc (format "Major mode for the %s browser." title))
         :interactive nil
         (setq tabulated-list-format ,columns)
         (setq-local revert-buffer-function #',revert)
         (tabulated-list-init-header))
       (defun ,render (result)
         ,(format "Render dashboard RESULT in the %s buffer." title)
         (with-current-buffer (get-buffer-create ,buffer)
           (unless (derived-mode-p ',mode)
             (,mode))
           (setq tabulated-list-entries (funcall ,rows result))
           (tabulated-list-print t)
           (pop-to-buffer (current-buffer))))
       (defun ,revert (&rest _)
         ,(format "Refresh the %s browser." title)
         (,command))
       (defun ,command ()
         ,(or command-doc (format "Browse %s from the Hermes dashboard." title))
         (interactive)
         (hermes-browser--run-on-client ,fetch #',render)))))

(provide 'hermes-browser)
;;; hermes-browser.el ends here
