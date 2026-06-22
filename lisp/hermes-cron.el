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

;; A `tabulated-list' browser over Hermes dashboard cron APIs.  `t' pauses or
;; resumes the job at point, `D' removes it, `c' creates one, `e' edits it, `!'
;; triggers it immediately, and RET opens job details with recent run history.

;;; Code:

(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-browser)

;;; Fields

(defun hermes-cron--field (job key)
  "Return JOB's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get job key)) ""))

(defun hermes-cron--non-empty (string)
  "Return STRING when it is non-empty after trimming."
  (and (stringp string)
       (let ((trimmed (string-trim string)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun hermes-cron--has-key-p (object key)
  "Return non-nil when OBJECT has KEY."
  (cond
   ((hash-table-p object)
    (let ((missing (make-symbol "missing")))
      (or (not (eq (gethash key object missing) missing))
          (not (eq (gethash (symbol-name key) object missing) missing)))))
   ((listp object)
    (or (assq key object)
        (assoc (symbol-name key) object)))))

(defun hermes-cron--job-id (job)
  "Return JOB's stable identifier."
  (or (hermes-cron--non-empty (hermes-cron--field job 'id))
      (hermes-cron--non-empty (hermes-cron--field job 'job_id))
      (hermes-cron--non-empty (hermes-cron--field job 'name))
      ""))

(defun hermes-cron--profile (job)
  "Return JOB's profile name, or an empty string."
  (or (hermes-cron--non-empty (hermes-cron--field job 'profile))
      (hermes-cron--non-empty (hermes-cron--field job 'profile_name))
      ""))

(defun hermes-cron--schedule (job)
  "Return JOB's schedule display string."
  (let ((schedule (hermes-transport--get job 'schedule)))
    (or (hermes-cron--non-empty (hermes-cron--field job 'schedule_display))
        (hermes-cron--non-empty (hermes-cron--field schedule 'display))
        (hermes-cron--non-empty (hermes-cron--field schedule 'expr))
        (hermes-cron--non-empty (hermes-cron--field job 'schedule))
        "")))

(defun hermes-cron--schedule-expr (job)
  "Return JOB's raw schedule expression for editing."
  (let ((schedule (hermes-transport--get job 'schedule)))
    (or (hermes-cron--non-empty (hermes-cron--field schedule 'expr))
        (hermes-cron--non-empty (hermes-cron--field job 'schedule))
        (hermes-cron--non-empty (hermes-cron--field job 'schedule_display))
        "")))

(defun hermes-cron--state (job)
  "Return JOB's state string."
  (or (hermes-cron--non-empty (hermes-cron--field job 'state))
      (and (hermes-cron--has-key-p job 'enabled)
           (not (hermes-transport--get job 'enabled))
           "disabled")
      "scheduled"))

(defun hermes-cron--prompt (job)
  "Return JOB's prompt or prompt preview."
  (or (hermes-cron--non-empty (hermes-cron--field job 'prompt))
      (hermes-cron--non-empty (hermes-cron--field job 'prompt_preview))
      (hermes-cron--non-empty (hermes-cron--field job 'script))
      ""))

(defun hermes-cron--skills (job)
  "Return JOB's skill names as a list of strings."
  (delq nil
        (mapcar #'hermes-transport--scalar-string
                (or (hermes-transport--get job 'skills) '()))))

(defun hermes-cron--skills-string (job)
  "Return JOB's skills as a comma-separated display string."
  (string-join (hermes-cron--skills job) ", "))

(defun hermes-cron--rows (result)
  "Return `tabulated-list' entries for a cron list RESULT."
  (mapcar
   (lambda (job)
     (list (hermes-cron--job-id job)
           (vector (hermes-cron--field job 'name)
                   (hermes-cron--schedule job)
                   (hermes-cron--state job)
                   (hermes-cron--profile job)
                   (hermes-cron--field job 'deliver)
                   (hermes-cron--field job 'last_run_at)
                   (hermes-cron--field job 'next_run_at)
                   (hermes-cron--prompt job))))
   (hermes-transport--get result 'jobs)))

;;; Dashboard REST API

(defun hermes-cron--client-base-url (client)
  "Return CLIENT's dashboard HTTP base URL."
  (or (and (hermes-dashboard-transport-client-p client)
           (hermes-dashboard-transport-client-base-url client))
      (and (hermes-dashboard-transport-client-p client)
           (hermes-dashboard-transport--base-url
            (hermes-dashboard-transport-client-host client)
            (hermes-dashboard-transport-client-port client)))
      (hermes-dashboard-transport--normalize-base-url
       hermes-dashboard-transport-url)))

(defun hermes-cron--client-token (client)
  "Return CLIENT's session token, if any."
  (and (hermes-dashboard-transport-client-p client)
       (hermes-cron--non-empty
        (hermes-dashboard-transport-client-token client))))

(defun hermes-cron--client-api (client method path &optional body query)
  "Call cron REST METHOD PATH with BODY and QUERY using CLIENT when possible."
  (if-let* ((token (hermes-cron--client-token client)))
      (let* ((base-url (hermes-cron--client-base-url client))
             (url (concat (hermes-dashboard-transport--api-url
                           base-url (concat "/api/cron" path))
                          (hermes-dashboard-transport--query-string query)))
             (headers (append (list (cons "X-Hermes-Session-Token" token))
                              (and body
                                   '(("Content-Type" . "application/json"))))))
        (plist-get (hermes-dashboard-transport--http-json
                    url :method method :headers headers :body body
                    :secrets (list token))
                   :body))
    (hermes-dashboard-transport-api-request
     method (concat "/api/cron" path) :body body :query query)))

(defun hermes-cron--job-path (id &rest segments)
  "Return the cron jobs path for ID extended by SEGMENTS."
  (concat "/jobs/" (url-hexify-string id) (apply #'concat segments)))

(defun hermes-cron--query (profile &optional extra)
  "Return a REST query for PROFILE plus EXTRA query entries."
  (append (and (hermes-cron--non-empty profile)
               `((profile . ,profile)))
          extra))

(defun hermes-cron--entry-profile ()
  "Return the profile shown on the current tabulated-list row, or nil."
  (and-let* ((entry (tabulated-list-get-entry))
             ((> (length entry) 3)))
    (hermes-cron--non-empty (aref entry 3))))

(defun hermes-cron--id-at-point ()
  "Return the cron job id at point, or signal a `user-error'."
  (or (tabulated-list-get-id) (user-error "No cron job on this line")))

(defun hermes-cron--with-client (fn)
  "Call FN with a dashboard client, reporting REST errors as messages.
When FN returns a function, call it after the transient client cleanup thunk."
  (hermes-browser--with-client
   (lambda (client done)
     (let ((cleaned nil))
       (condition-case err
           (let ((after (funcall fn client)))
             (unless cleaned
               (setq cleaned t)
               (funcall done))
             (when (functionp after)
               (funcall after)))
         (error
          (unless cleaned
            (setq cleaned t)
            (funcall done))
          (message "Hermes: %s" (error-message-string err))))))))

(defun hermes-cron--fetch-job (client id profile)
  "Fetch cron job ID for PROFILE through CLIENT."
  (hermes-cron--client-api client "GET" (hermes-cron--job-path id)
                           nil (hermes-cron--query profile)))

(defun hermes-cron--fetch-runs (client id profile)
  "Fetch recent run history for cron job ID and PROFILE through CLIENT."
  (hermes-cron--client-api client "GET" (hermes-cron--job-path id "/runs")
                           nil (hermes-cron--query profile '((limit . 20)))))

(defun hermes-cron--update-job (client id profile updates)
  "Update cron job ID for PROFILE through CLIENT.
UPDATES is the payload sent to the dashboard."
  (hermes-cron--client-api client "PUT" (hermes-cron--job-path id)
                           `((updates . ,updates))
                           (hermes-cron--query profile)))

;;; Job detail view

(defun hermes-cron--time (value)
  "Return VALUE as a display timestamp."
  (cond
   ((numberp value) (format-time-string "%Y-%m-%d %H:%M" value))
   (t (or (hermes-transport--scalar-string value) ""))))

(defun hermes-cron--format-job (job)
  "Return detail text for cron JOB."
  (string-join
   (list (format "Name:     %s" (hermes-cron--field job 'name))
         (format "ID:       %s" (hermes-cron--job-id job))
         (format "Profile:  %s" (or (hermes-cron--non-empty
                                      (hermes-cron--profile job))
                                     "default"))
         (format "State:    %s" (hermes-cron--state job))
         (format "Schedule: %s" (hermes-cron--schedule job))
         (format "Deliver:  %s" (or (hermes-cron--non-empty
                                      (hermes-cron--field job 'deliver))
                                     "local"))
         (format "Skills:   %s" (or (hermes-cron--non-empty
                                      (hermes-cron--skills-string job))
                                     "-"))
         (format "Last:     %s" (hermes-cron--field job 'last_run_at))
         (format "Next:     %s" (hermes-cron--field job 'next_run_at))
         (format "Error:    %s" (or (hermes-cron--non-empty
                                      (hermes-cron--field job 'last_error))
                                     "-"))
         ""
         "Prompt:"
         (or (hermes-cron--non-empty (hermes-cron--prompt job)) "-"))
   "\n"))

(defun hermes-cron--format-run (run)
  "Return one display line for cron RUN."
  (format "  %s  %s  %s msg%s  %s%s"
          (hermes-cron--time (or (hermes-transport--get run 'started_at)
                                 (hermes-transport--get run 'created_at)))
          (or (hermes-cron--non-empty (hermes-cron--field run 'title))
              (hermes-cron--field run 'id))
          (or (hermes-cron--non-empty (hermes-cron--field run 'message_count))
              "0")
          (if (equal (hermes-cron--field run 'message_count) "1") "" "s")
          (hermes-cron--field run 'source)
          (if (eq (hermes-transport--get run 'is_active) t) " active" "")))

(defun hermes-cron--format-runs (runs)
  "Return detail text for recent cron run history.
RUNS is the run list from the dashboard."
  (concat "\n\nRuns:\n"
          (if runs
              (string-join (mapcar #'hermes-cron--format-run runs) "\n")
            "  No recorded runs.")))

(defun hermes-cron--display-detail (job runs)
  "Display cron JOB with run history in a detail buffer.
RUNS is the detail run list."
  (with-current-buffer (get-buffer-create "*Hermes Cron Job*")
    (unless (derived-mode-p 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (hermes-cron--format-job job))
      (insert (hermes-cron--format-runs runs)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun hermes-cron-show ()
  "Show details and recent run history for the cron job at point."
  (interactive)
  (let ((id (hermes-cron--id-at-point))
        (profile (hermes-cron--entry-profile)))
    (hermes-cron--with-client
     (lambda (client)
       (let* ((job (hermes-cron--fetch-job client id profile))
              (job-profile (or (hermes-cron--non-empty
                                (hermes-cron--profile job))
                               profile))
              (runs (hermes-transport--get
                     (hermes-cron--fetch-runs client id job-profile) 'runs)))
         (hermes-cron--display-detail job runs))))))

;;; Job mutations

(defun hermes-cron--act (action name done-message)
  "Run cron ACTION on job NAME, report DONE-MESSAGE, then refresh the list."
  (hermes-browser--run-on-client
   (lambda (client)
     (hermes-dashboard-transport-call-fn
      #'hermes-dashboard-transport-cron-manage
      client :action action :name name))
   (lambda (_result)
     (message "Hermes: %s" done-message)
     (hermes-list-crons))))

(defun hermes-cron-toggle ()
  "Pause or resume the cron job at point."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (unless id (user-error "No cron job on this line"))
    (let ((action (if (member (and entry (aref entry 2)) '("paused" "disabled"))
                      "resume" "pause")))
      (hermes-cron--act action id (format "%sd %s" action id)))))

(defun hermes-cron-remove ()
  "Remove the cron job at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No cron job on this line"))
    (when (yes-or-no-p (format "Remove cron job %s? " id))
      (hermes-cron--act "remove" id (format "removed %s" id)))))

(defun hermes-cron--split-skills (text)
  "Return comma-separated skill names from TEXT."
  (delq nil
        (mapcar #'hermes-cron--non-empty
                (split-string (or text "") ","))))

(defun hermes-cron--read-updates (job)
  "Read and return update fields for JOB."
  (let* ((name (read-string "Name: " (hermes-cron--field job 'name)))
         (schedule (read-string "Schedule: " (hermes-cron--schedule-expr job)))
         (prompt (read-string "Prompt: " (hermes-cron--prompt job)))
         (deliver (read-string "Deliver: " (or (hermes-cron--non-empty
                                                (hermes-cron--field job 'deliver))
                                               "local")))
         (skills (hermes-cron--split-skills
                  (read-string "Skills (comma-separated): "
                               (hermes-cron--skills-string job)))))
    (when (or (string-empty-p (string-trim schedule))
              (string-empty-p (string-trim prompt)))
      (user-error "Schedule and prompt are required"))
    `((name . ,(string-trim name))
      (schedule . ,(string-trim schedule))
      (prompt . ,(string-trim prompt))
      (deliver . ,(or (hermes-cron--non-empty deliver) "local"))
      (skills . ,skills))))

(defun hermes-cron-edit ()
  "Edit the cron job at point."
  (interactive)
  (let ((id (hermes-cron--id-at-point))
        (profile (hermes-cron--entry-profile)))
    (hermes-cron--with-client
     (lambda (client)
       (let* ((job (hermes-cron--fetch-job client id profile))
              (job-profile (or (hermes-cron--non-empty
                                (hermes-cron--profile job))
                               profile))
              (updates (hermes-cron--read-updates job)))
         (hermes-cron--update-job client id job-profile updates)
         (lambda ()
           (message "Hermes: updated %s" id)
           (hermes-list-crons)))))))

(defun hermes-cron-trigger ()
  "Trigger the cron job at point immediately."
  (interactive)
  (let ((id (hermes-cron--id-at-point))
        (profile (hermes-cron--entry-profile)))
    (hermes-cron--with-client
     (lambda (client)
       (hermes-cron--client-api client "POST" (hermes-cron--job-path id "/trigger")
                                nil (hermes-cron--query profile))
       (lambda ()
         (message "Hermes: triggered %s" id)
         (hermes-list-crons))))))

(defun hermes-cron-create (name schedule prompt)
  "Create a cron job NAME running PROMPT on SCHEDULE."
  (interactive (list (read-string "Cron job name: ")
                     (read-string "Schedule (cron expression): ")
                     (read-string "Prompt: ")))
  (when (or (string-empty-p name)
            (string-empty-p schedule)
            (string-empty-p prompt))
    (user-error "Name, schedule and prompt are required"))
  (hermes-browser--run-on-client
   (lambda (client)
     (hermes-dashboard-transport-call-fn
      #'hermes-dashboard-transport-cron-manage
      client :action "add" :name name :schedule schedule :prompt prompt))
   (lambda (_result) (message "Hermes: created cron job %s" name))))

;;;###autoload (autoload 'hermes-list-crons "hermes-cron" nil t)
(hermes-define-list-browser cron
  :title "Hermes Cron"
  :command hermes-list-crons
  :buffer "*Hermes Cron*"
  :columns [("Name" 22 t) ("Schedule" 18 t) ("State" 10 t) ("Profile" 12 t)
            ("Deliver" 12 t) ("Last run" 18 t) ("Next run" 18 t)
            ("Prompt" 40 nil)]
  :fetch (lambda (client)
           (hermes-dashboard-transport-call-fn
            #'hermes-dashboard-transport-cron-manage client :action "list"))
  :rows #'hermes-cron--rows
  :keys ("RET" #'hermes-cron-show
         "e" #'hermes-cron-edit
         "!" #'hermes-cron-trigger
         "t" #'hermes-cron-toggle
         "D" #'hermes-cron-remove
         "c" #'hermes-cron-create))

(provide 'hermes-cron)
;;; hermes-cron.el ends here
