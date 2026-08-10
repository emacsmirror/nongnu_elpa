;;; hermes-cron.el --- Scheduled-job browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over Hermes dashboard cron APIs.  The t key pauses
;; or resumes the job at point, D removes it, c creates one, e edits it, !
;; triggers it immediately, and RET opens job details with recent run history.

;;; Code:

(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-promise)
(require 'hermes-browser)

(declare-function read-string-from-buffer "string-edit")

;;; Customization

(defcustom hermes-cron-notify-on-failure nil
  "When non-nil, raise a desktop notification when a cron job newly fails.
Failures are only detected when the cron list refreshes; pair this with
`hermes-cron-auto-refresh-interval' to be alerted without refreshing by hand."
  :type 'boolean
  :group 'hermes)

(defcustom hermes-cron-auto-refresh-interval nil
  "Seconds between automatic refreshes of a cron browser buffer.
A positive number refreshes the list on that interval; nil or zero disables
auto-refresh.  Used to detect cron failures for `hermes-cron-notify-on-failure'."
  :type '(choice (const :tag "Disabled" nil) (natnum :tag "Seconds"))
  :group 'hermes)

;;; Fields

(defun hermes-cron--job-id (job)
  "Return JOB's stable identifier."
  (or (hermes-transport--non-blank-string (hermes-transport--display-field job 'id))
      (hermes-transport--non-blank-string (hermes-transport--display-field job 'job_id))
      (hermes-transport--non-blank-string (hermes-transport--display-field job 'name))
      ""))

(defun hermes-cron--profile (job)
  "Return JOB's profile name, or an empty string."
  (or (hermes-transport--non-blank-string (hermes-transport--display-field job 'profile))
      (hermes-transport--non-blank-string (hermes-transport--display-field job 'profile_name))
      ""))

(defun hermes-cron--schedule (job)
  "Return JOB's schedule display string."
  (let ((schedule (hermes-transport--get job 'schedule)))
    (or (hermes-transport--non-blank-string (hermes-transport--display-field job 'schedule_display))
        (hermes-transport--non-blank-string (hermes-transport--display-field schedule 'display))
        (hermes-transport--non-blank-string (hermes-transport--display-field schedule 'expr))
        (hermes-transport--non-blank-string (hermes-transport--display-field job 'schedule))
        "")))

(defun hermes-cron--schedule-expr (job)
  "Return JOB's raw schedule expression for editing."
  (let ((schedule (hermes-transport--get job 'schedule)))
    (or (hermes-transport--non-blank-string (hermes-transport--display-field schedule 'expr))
        (hermes-transport--non-blank-string (hermes-transport--display-field job 'schedule))
        (hermes-transport--non-blank-string (hermes-transport--display-field job 'schedule_display))
        "")))

(defun hermes-cron--state (job)
  "Return JOB's state string."
  (or (hermes-transport--non-blank-string (hermes-transport--display-field job 'state))
      (and (hermes-transport--field-present-p job 'enabled)
           (not (hermes-transport--get job 'enabled))
           "disabled")
      "scheduled"))

(defun hermes-cron--prompt (job)
  "Return JOB's prompt or prompt preview."
  (or (hermes-transport--non-blank-string (hermes-transport--display-field job 'prompt))
      (hermes-transport--non-blank-string (hermes-transport--display-field job 'prompt_preview))
      (hermes-transport--non-blank-string (hermes-transport--display-field job 'script))
      ""))

(defun hermes-cron--skills (job)
  "Return JOB's skill names as a list of strings."
  (delq nil
        (mapcar #'hermes-transport--scalar-string
                (or (hermes-transport--get job 'skills) '()))))

(defun hermes-cron--skills-string (job)
  "Return JOB's skills as a comma-separated display string."
  (string-join (hermes-cron--skills job) ", "))

(defun hermes-cron--last-status (job)
  "Return JOB's last-run outcome symbol: `error', `ok', or nil when unknown."
  (pcase (downcase (or (hermes-transport--scalar-string
                        (hermes-transport--get job 'last_status))
                       ""))
    ("error" 'error)
    ("ok" 'ok)
    (_ nil)))

(defun hermes-cron--state-cell (job)
  "Return JOB's state cell with its semantic and column faces.
The cell text is unchanged so commands reading it by `equal' still match."
  (hermes-browser--status-cell (hermes-cron--state job)
                               'hermes-browser-state))

(defun hermes-cron--deliver-cell (job)
  "Return JOB's deliver cell, faced as an error after delivery failure."
  (hermes-browser--face-cell
   (hermes-transport--display-field job 'deliver)
   (if (hermes-transport--non-blank-string
        (hermes-transport--display-field job 'last_delivery_error))
       'hermes-browser-error
     'hermes-browser-delivery)))

(defun hermes-cron--last-run-cell (job)
  "Return JOB's last-run cell, faced by the most recent run outcome."
  (hermes-browser--face-cell
   (hermes-transport--display-field job 'last_run_at)
   (pcase (hermes-cron--last-status job)
     ('error 'hermes-browser-error)
     ('ok 'hermes-browser-success)
     (_ 'hermes-browser-timestamp))))

(defun hermes-cron--rows (result)
  "Return `tabulated-list' entries for a cron list RESULT."
  (mapcar
   (lambda (job)
     (list (hermes-cron--job-id job)
           (vector (hermes-browser--face-cell
                    (hermes-transport--display-field job 'name)
                    'hermes-browser-name)
                   (hermes-browser--face-cell
                    (hermes-cron--schedule job) 'hermes-browser-schedule)
                   (hermes-cron--state-cell job)
                   (hermes-browser--face-cell
                    (hermes-cron--profile job) 'hermes-browser-profile)
                   (hermes-cron--deliver-cell job)
                   (hermes-cron--last-run-cell job)
                   (hermes-browser--face-cell
                    (hermes-transport--display-field job 'next_run_at)
                    'hermes-browser-timestamp)
                   (hermes-browser--face-cell
                    (hermes-cron--prompt job) 'hermes-browser-prompt))))
   (hermes-transport--get result 'jobs)))

(defun hermes-cron--jobs-result (payload)
  "Return cron list PAYLOAD normalized to a `jobs' result object."
  (if (hermes-transport--field-present-p payload 'jobs)
      payload
    `((jobs . ,payload))))

;;; Dashboard REST API

(defun hermes-cron--api (client method path &optional body query)
  "Return a promise of the cron REST METHOD PATH through CLIENT.
BODY and QUERY extend the request; authentication comes from CLIENT's session
token when present, otherwise the configured dashboard URL."
  (hermes-dashboard-transport-api-request-async
   method (concat "/api/cron" path) :body body :query query :client client))

(defun hermes-cron--job-path (id &rest segments)
  "Return the cron jobs path for ID extended by SEGMENTS."
  (concat "/jobs/" (url-hexify-string id) (apply #'concat segments)))

(defun hermes-cron--query (profile &optional extra)
  "Return a REST query for PROFILE plus EXTRA query entries."
  (append (and (hermes-transport--non-blank-string profile)
               `((profile . ,profile)))
          extra))

(defun hermes-cron--entry-profile ()
  "Return the profile shown on the current tabulated-list row, or nil."
  (and-let* ((entry (tabulated-list-get-entry))
             ((> (length entry) 3)))
    (hermes-transport--non-blank-string (aref entry 3))))

(defun hermes-cron--id-at-point ()
  "Return the cron job id at point, or signal a `user-error'."
  (or (tabulated-list-get-id) (user-error "No cron job on this line")))

(defun hermes-cron--fetch-job (client id profile)
  "Return a promise of cron job ID for PROFILE through CLIENT."
  (hermes-cron--api client "GET" (hermes-cron--job-path id)
                    nil (hermes-cron--query profile)))

(defun hermes-cron--fetch-runs (client id profile)
  "Return a promise of recent run history for cron job ID and PROFILE via CLIENT."
  (hermes-cron--api client "GET" (hermes-cron--job-path id "/runs")
                    nil (hermes-cron--query profile '((limit . 20)))))

(defun hermes-cron--update-job (client id profile payload)
  "Return a promise that sends PAYLOAD for cron job ID and PROFILE via CLIENT."
  (hermes-cron--api client "PUT" (hermes-cron--job-path id)
                    `((updates . ,payload))
                    (hermes-cron--query profile)))

;;; Job detail view

(defun hermes-cron--time (value)
  "Return VALUE as a display timestamp."
  (cond
   ((numberp value) (format-time-string "%F %R" value))
   (t (or (hermes-transport--scalar-string value) ""))))

(defun hermes-cron--format-job (job)
  "Return detail text for cron JOB."
  (string-join
   (list (format "Name:     %s" (hermes-transport--display-field job 'name))
         (format "ID:       %s" (hermes-cron--job-id job))
         (format "Profile:  %s" (or (hermes-transport--non-blank-string
                                      (hermes-cron--profile job))
                                     "default"))
         (format "State:    %s" (hermes-cron--state job))
         (format "Schedule: %s" (hermes-cron--schedule job))
         (format "Deliver:  %s" (or (hermes-transport--non-blank-string
                                      (hermes-transport--display-field job 'deliver))
                                     "local"))
         (format "Skills:   %s" (or (hermes-transport--non-blank-string
                                      (hermes-cron--skills-string job))
                                     "-"))
         (format "Last:     %s" (hermes-transport--display-field job 'last_run_at))
         (format "Next:     %s" (hermes-transport--display-field job 'next_run_at))
         (format "Result:   %s" (or (hermes-transport--non-blank-string
                                      (hermes-transport--display-field job 'last_status))
                                     "-"))
         (format "Error:    %s" (or (hermes-transport--non-blank-string
                                      (hermes-transport--display-field job 'last_error))
                                     "-"))
         (format "Delivery: %s" (or (hermes-transport--non-blank-string
                                      (hermes-transport--display-field job 'last_delivery_error))
                                     "-"))
         ""
         "Prompt:"
         (or (hermes-transport--non-blank-string (hermes-cron--prompt job)) "-"))
   "\n"))

(defvar-keymap hermes-cron--run-line-map
  :doc "Keymap active on cron run lines in the job detail buffer."
  "RET" #'hermes-cron-show-run-log
  "<mouse-1>" #'hermes-cron-show-run-log)

(defun hermes-cron--format-run (run)
  "Return one display line for cron RUN, navigable to its transcript."
  (let ((id (hermes-transport--display-field run 'id))
        (line (format "  %s  %s  %s msg%s  %s%s"
                      (hermes-cron--time (or (hermes-transport--get run 'started_at)
                                             (hermes-transport--get run 'created_at)))
                      (or (hermes-transport--non-blank-string
                           (hermes-transport--display-field run 'title))
                          (hermes-transport--display-field run 'id))
                      (or (hermes-transport--non-blank-string
                           (hermes-transport--display-field run 'message_count))
                          "0")
                      (if (equal (hermes-transport--display-field run 'message_count) "1") "" "s")
                      (hermes-transport--display-field run 'source)
                      (if (eq (hermes-transport--get run 'is_active) t) " active" ""))))
    (if (hermes-transport--non-blank-string id)
        (propertize line
                    'hermes-cron-run-id id
                    'keymap hermes-cron--run-line-map
                    'mouse-face 'highlight
                    'help-echo "RET: show this run's transcript")
      line)))

(defun hermes-cron--format-runs (runs)
  "Return detail text for recent cron run history.
RUNS is the run list from the dashboard."
  (concat "\n\nRuns (RET on a line for its transcript):\n"
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
        (profile (hermes-cron--entry-profile))
        (origin (current-buffer))
        (generation (hermes-browser--next-request-generation)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-then
        (hermes-cron--fetch-job client id profile)
        (lambda (job)
          (let ((job-profile (or (hermes-transport--non-blank-string
                                  (hermes-cron--profile job))
                                 profile)))
            (hermes--promise-map
             (hermes-cron--fetch-runs client id job-profile)
             (lambda (runs-result)
               (list job (hermes-transport--get runs-result 'runs))))))))
     (lambda (detail)
       (when (hermes-browser--request-current-mode-p
              origin generation 'hermes-cron-mode)
         (hermes-cron--display-detail (car detail) (cadr detail)))))))

;;; Run transcript (log)

(defun hermes-cron--fetch-run-messages (client session-id)
  "Return a promise of run SESSION-ID's transcript messages via CLIENT."
  (hermes-dashboard-transport-api-request-async
   "GET" (concat "/api/sessions/" (url-hexify-string session-id) "/messages")
   :client client))

(defun hermes-cron--message-text (message)
  "Return MESSAGE's textual content as a string."
  (let ((content (hermes-transport--get message 'content)))
    (or (hermes-transport--scalar-string content)
        (and (or (listp content) (vectorp content))
             (string-join
              (delq nil
                    (mapcar (lambda (part)
                              (or (hermes-transport--scalar-string part)
                                  (hermes-transport--non-blank-string
                                   (hermes-transport--display-field part 'text))))
                            (if (vectorp content) (append content nil) content)))
              "\n"))
        "")))

(defun hermes-cron--format-message (message)
  "Return display text for a run transcript MESSAGE."
  (let ((role (or (hermes-transport--non-blank-string
                   (hermes-transport--display-field message 'role))
                  "message"))
        (text (hermes-cron--message-text message)))
    (concat "## " role "\n\n" (if (string-empty-p text) "(no content)" text) "\n")))

(defun hermes-cron--display-run (session-id messages)
  "Display run SESSION-ID's transcript MESSAGES in a log buffer."
  (let ((entries (if (vectorp messages) (append messages nil) messages)))
    (with-current-buffer (get-buffer-create "*Hermes Cron Run*")
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Run: %s\n\n" session-id))
        (insert (if entries
                    (string-join (mapcar #'hermes-cron--format-message entries) "\n")
                  "No transcript recorded.")))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun hermes-cron-show-run-log ()
  "Show the transcript of the cron run on the current detail line."
  (interactive)
  (let ((id (get-text-property (point) 'hermes-cron-run-id))
        (origin (current-buffer))
        (generation (hermes-browser--next-request-generation)))
    (unless id (user-error "No cron run on this line"))
    (hermes-browser--run-on-client
     (lambda (client) (hermes-cron--fetch-run-messages client id))
     (lambda (result)
       (when (hermes-browser--request-current-p origin generation)
         (hermes-cron--display-run
          id (hermes-transport--get result 'messages)))))))

;;; Job mutations

(defun hermes-cron--checked-result (result)
  "Return RESULT, or signal when it declares an unsuccessful operation."
  (if (and (hermes-transport--field-present-p result 'ok)
           (not (eq (hermes-transport--get result 'ok) t)))
      (error "%s" (or (hermes-transport--non-blank-string
                        (hermes-transport--display-field result 'error))
                       (hermes-transport--non-blank-string
                        (hermes-transport--display-field result 'detail))
                       "Cron request failed"))
    result))

(defun hermes-cron--refresh-origin (buffer)
  "Start a fresh read of live cron BUFFER."
  (when (hermes-browser--buffer-mode-p buffer 'hermes-cron-mode)
    (with-current-buffer buffer
      (hermes-cron--revert))))

(defun hermes-cron--act (action id profile done-message)
  "Run cron ACTION on job ID for PROFILE, then report DONE-MESSAGE."
  (let ((origin (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-map
        (if (equal action "remove")
            (hermes-cron--api client "DELETE" (hermes-cron--job-path id)
                              nil (hermes-cron--query profile))
          (hermes-cron--api client "POST"
                            (hermes-cron--job-path id (concat "/" action))
                            nil (hermes-cron--query profile)))
        #'hermes-cron--checked-result))
     (lambda (_result)
       (message "Hermes: %s" done-message)
       (hermes-cron--refresh-origin origin)))))

(defun hermes-cron-toggle ()
  "Pause or resume the cron job at point."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (unless id (user-error "No cron job on this line"))
    (let ((action (if (member (and entry (aref entry 2)) '("paused" "disabled"))
                      "resume" "pause")))
      (hermes-cron--act action id (hermes-cron--entry-profile)
                        (format "%sd %s" action id)))))

(defun hermes-cron-remove ()
  "Remove the cron job at point."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (profile (hermes-cron--entry-profile)))
    (unless id (user-error "No cron job on this line"))
    (when (yes-or-no-p (format "Remove cron job %s? " id))
      (hermes-cron--act "remove" id profile (format "removed %s" id)))))

(defun hermes-cron--split-skills (text)
  "Return comma-separated skill names from TEXT."
  (delq nil
        (mapcar #'hermes-transport--non-blank-string
                (split-string (or text "") ","))))

(defun hermes-cron--read-updates (job)
  "Read and return update fields for JOB."
  (let* ((name (read-string "Name: " (hermes-transport--display-field job 'name)))
         (schedule (read-string "Schedule: " (hermes-cron--schedule-expr job)))
         (prompt (read-string-from-buffer "Prompt: " (hermes-cron--prompt job)))
         (deliver (read-string "Deliver: " (or (hermes-transport--non-blank-string
                                                (hermes-transport--display-field job 'deliver))
                                               "local")))
         (skills (hermes-cron--split-skills
                  (read-string "Skills (comma-separated): "
                               (hermes-cron--skills-string job)))))
    (when (or (string-empty-p (string-trim name))
              (string-empty-p (string-trim schedule))
              (string-empty-p (string-trim prompt)))
      (user-error "Name, schedule and prompt are required"))
    `((name . ,(string-trim name))
      (schedule . ,(string-trim schedule))
      (prompt . ,(string-trim prompt))
      (deliver . ,(or (hermes-transport--non-blank-string deliver) "local"))
      (skills . ,skills))))

(defun hermes-cron-edit ()
  "Edit the cron job at point."
  (interactive)
  (let ((id (hermes-cron--id-at-point))
        (profile (hermes-cron--entry-profile))
        (origin (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-then
        (hermes-cron--fetch-job client id profile)
        (lambda (job)
          (let* ((job-profile (or (hermes-transport--non-blank-string
                                   (hermes-cron--profile job))
                                  profile))
                 (updates (hermes-cron--read-updates job)))
            (hermes--promise-map
             (hermes-cron--update-job client id job-profile updates)
             #'hermes-cron--checked-result)))))
     (lambda (_result)
       (message "Hermes: updated %s" id)
       (hermes-cron--refresh-origin origin)))))

(defun hermes-cron-trigger ()
  "Trigger the cron job at point immediately."
  (interactive)
  (let ((id (hermes-cron--id-at-point))
        (profile (hermes-cron--entry-profile))
        (origin (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-map
        (hermes-cron--api client "POST" (hermes-cron--job-path id "/trigger")
                          nil (hermes-cron--query profile))
        #'hermes-cron--checked-result))
     (lambda (_result)
       (message "Hermes: triggered %s" id)
       (hermes-cron--refresh-origin origin)))))

(defun hermes-cron-create (name schedule prompt &optional profile deliver skills)
  "Create cron job NAME running PROMPT on SCHEDULE for PROFILE.
When non-nil, DELIVER names the delivery target and SKILLS is a list of skill
names.  Interactive creation defaults DELIVER to local delivery."
  (interactive (list (read-string "Cron job name: ")
                     (read-string "Schedule (cron expression): ")
                     (read-string-from-buffer "Prompt: " "")
                     (read-string "Profile: " "default")
                     (read-string "Deliver: " "local")
                     (hermes-cron--split-skills
                      (read-string "Skills (comma-separated): "))))
  (let* ((name (string-trim name))
         (schedule (string-trim schedule))
         (prompt (string-trim prompt))
         (profile (or (hermes-transport--non-blank-string profile) "default"))
         (origin (current-buffer)))
    (when (or (string-empty-p name)
              (string-empty-p schedule)
              (string-empty-p prompt))
      (user-error "Name, schedule and prompt are required"))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-map
        (hermes-cron--api client "POST" "/jobs"
                          (append `((name . ,name) (schedule . ,schedule)
                                    (prompt . ,prompt))
                                  (and deliver `((deliver . ,deliver)))
                                  (and skills `((skills . ,(vconcat skills)))))
                          (hermes-cron--query profile))
        #'hermes-cron--checked-result))
     (lambda (_result)
       (message "Hermes: created cron job %s" name)
       (hermes-cron--refresh-origin origin)))))

;;; Failure notifications and auto-refresh

(defvar-local hermes-cron--seen-runs nil
  "Hash of cron job id to last seen (LAST-RUN-AT . OUTCOME).
Nil until the first render records a baseline.")

(defun hermes-cron--note-failures (result)
  "Notify about newly failed cron jobs in RESULT when enabled.
The first render only records a baseline so pre-existing failures do not alert."
  (let ((seen hermes-cron--seen-runs)
        (next (make-hash-table :test 'equal)))
    (dolist (job (hermes-transport--get result 'jobs))
      (let* ((id (hermes-cron--job-id job))
             (stamp (hermes-transport--display-field job 'last_run_at))
             (outcome (hermes-cron--last-status job))
             (entry (cons stamp outcome)))
        (puthash id entry next)
        (when (and seen hermes-cron-notify-on-failure
                   (eq outcome 'error)
                   (not (equal (gethash id seen) entry)))
          (hermes-browser--notify
           "Hermes cron failed"
           (format "%s failed (%s)"
                   (hermes-transport--display-field job 'name)
                   (or (hermes-transport--non-blank-string stamp) "just now"))
           'cron-failure (current-buffer)))))
    (setq hermes-cron--seen-runs next)))

(defvar-local hermes-cron--auto-refresh-timer nil
  "Per-buffer repeat timer refreshing the cron list, or nil.")

(defun hermes-cron--auto-refresh-tick (buffer)
  "Refresh BUFFER in place when it is still a live cron browser."
  (when (hermes-browser--buffer-mode-p buffer 'hermes-cron-mode)
    (with-current-buffer buffer
      (hermes-cron--revert))))

(defun hermes-cron--stop-auto-refresh ()
  "Cancel this buffer's cron auto-refresh timer."
  (when hermes-cron--auto-refresh-timer
    (cancel-timer hermes-cron--auto-refresh-timer)
    (setq hermes-cron--auto-refresh-timer nil)))

(defun hermes-cron--maybe-start-auto-refresh ()
  "Start a per-buffer cron auto-refresh timer when one is configured."
  (when (and (natnump hermes-cron-auto-refresh-interval)
             (> hermes-cron-auto-refresh-interval 0)
             (not hermes-cron--auto-refresh-timer))
    (setq hermes-cron--auto-refresh-timer
          (run-at-time hermes-cron-auto-refresh-interval
                       hermes-cron-auto-refresh-interval
                       #'hermes-cron--auto-refresh-tick (current-buffer)))
    (add-hook 'kill-buffer-hook #'hermes-cron--stop-auto-refresh nil t)
    (add-hook 'change-major-mode-hook
              #'hermes-cron--stop-auto-refresh nil t)))

;;;###autoload (autoload 'hermes-list-crons "hermes-cron" nil t)
(hermes-define-list-browser cron
  :title "Hermes Cron"
  :command hermes-list-crons
  :buffer "*Hermes Cron*"
  :doc "Major mode listing Hermes scheduled jobs."
  :command-doc "Browse Hermes scheduled (cron) jobs."
  :dynamic-columns (("Name" 14 3 t) ("Schedule" 12 1 t) ("State" 8 0 t)
                    ("Profile" 10 1 t) ("Deliver" 9 0 t) ("Last run" 16 0 t)
                    ("Next run" 16 0 t) ("Prompt" 24 5 nil))
  :fetch (lambda (client)
           (hermes--promise-map
            (hermes-cron--api client "GET" "/jobs" nil '((profile . "all")))
            #'hermes-cron--jobs-result))
  :rows #'hermes-cron--rows
  :on-result #'hermes-cron--note-failures
  :on-mode #'hermes-cron--maybe-start-auto-refresh
  :keys ("RET" #'hermes-cron-show
         "e" #'hermes-cron-edit
         "!" #'hermes-cron-trigger
         "t" #'hermes-cron-toggle
         "D" #'hermes-cron-remove
         "c" #'hermes-cron-create))

(provide 'hermes-cron)
;;; hermes-cron.el ends here
