;;; hermes-cron.el --- Scheduled-job browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard `cron.manage' method.  `t'
;; pauses or resumes the job at point, `D' removes it, and `c' creates one.

;;; Code:

(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-sessions)

(defun hermes-cron--field (job key)
  "Return JOB's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get job key)) ""))

(defun hermes-cron--rows (result)
  "Return `tabulated-list' entries for a `cron.manage' list RESULT."
  (mapcar
   (lambda (job)
     (list (hermes-cron--field job 'job_id)
           (vector (hermes-cron--field job 'name)
                   (hermes-cron--field job 'schedule)
                   (hermes-cron--field job 'state)
                   (hermes-cron--field job 'next_run_at)
                   (hermes-cron--field job 'prompt_preview))))
   (hermes-transport--get result 'jobs)))

(defun hermes-cron--with-client (fn)
  "Call FN with a connected CLIENT and a DONE cleanup thunk.
Reuses a live chat connection when one exists; otherwise connects a transient
client that DONE stops."
  (let* ((existing (hermes-sessions--existing-client))
         (client (or existing
                     (hermes-dashboard-transport-start :callback #'ignore)))
         (done (lambda ()
                 (unless existing
                   (hermes-dashboard-transport-stop client)))))
    (funcall fn client done)))

(defun hermes-cron--revert (&rest _)
  "Refresh the cron job list."
  (hermes-list-crons))

(defvar-keymap hermes-cron-mode-map
  :doc "Keymap for `hermes-cron-mode'."
  :parent tabulated-list-mode-map
  "t" #'hermes-cron-toggle
  "D" #'hermes-cron-remove
  "c" #'hermes-cron-create)

(define-derived-mode hermes-cron-mode tabulated-list-mode "Hermes Cron"
  "Major mode listing Hermes scheduled jobs."
  :interactive nil
  (setq tabulated-list-format
        [("Name" 22 t) ("Schedule" 16 t) ("State" 9 t)
         ("Next run" 18 t) ("Prompt" 40 nil)])
  (setq-local revert-buffer-function #'hermes-cron--revert)
  (tabulated-list-init-header))

(defun hermes-cron--render (result)
  "Display cron jobs from RESULT in the cron buffer."
  (with-current-buffer (get-buffer-create "*Hermes Cron*")
    (unless (derived-mode-p 'hermes-cron-mode)
      (hermes-cron-mode))
    (setq tabulated-list-entries (hermes-cron--rows result))
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun hermes-cron--act (action name done-message)
  "Run cron ACTION on job NAME, report DONE-MESSAGE, then refresh the list."
  (hermes-cron--with-client
   (lambda (client done)
     (hermes-dashboard-transport-cron-manage
      client :action action :name name
      :resolve (lambda (_result)
                 (funcall done)
                 (message "Hermes: %s" done-message)
                 (hermes-list-crons))
      :reject (lambda (message)
                (funcall done)
                (message "Hermes: %s" message))))))

(defun hermes-cron-toggle ()
  "Pause or resume the cron job at point."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (unless id (user-error "No cron job on this line"))
    (let ((action (if (equal (and entry (aref entry 2)) "paused")
                      "resume" "pause")))
      (hermes-cron--act action id (format "%sd %s" action id)))))

(defun hermes-cron-remove ()
  "Remove the cron job at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No cron job on this line"))
    (when (yes-or-no-p (format "Remove cron job %s? " id))
      (hermes-cron--act "remove" id (format "removed %s" id)))))

(defun hermes-cron-create (name schedule prompt)
  "Create a cron job NAME running PROMPT on SCHEDULE."
  (interactive (list (read-string "Cron job name: ")
                     (read-string "Schedule (cron expression): ")
                     (read-string "Prompt: ")))
  (when (or (string-empty-p name)
            (string-empty-p schedule)
            (string-empty-p prompt))
    (user-error "Name, schedule and prompt are required"))
  (hermes-cron--with-client
   (lambda (client done)
     (hermes-dashboard-transport-cron-manage
      client :action "add" :name name :schedule schedule :prompt prompt
      :resolve (lambda (_result)
                 (funcall done)
                 (message "Hermes: created cron job %s" name))
      :reject (lambda (message)
                (funcall done)
                (message "Hermes: %s" message))))))

;;;###autoload
(defun hermes-list-crons ()
  "Browse Hermes scheduled (cron) jobs."
  (interactive)
  (hermes-cron--with-client
   (lambda (client done)
     (hermes-dashboard-transport-cron-manage
      client :action "list"
      :resolve (lambda (result)
                 (funcall done)
                 (hermes-cron--render result))
      :reject (lambda (message)
                (funcall done)
                (message "Hermes: %s" message))))))

(provide 'hermes-cron)
;;; hermes-cron.el ends here
