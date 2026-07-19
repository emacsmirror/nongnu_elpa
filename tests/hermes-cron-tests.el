;;; hermes-cron-tests.el --- cron tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-cron-rows-from-list ()
  "Cron rows map rich job fields into the tabulated list."
  (let ((rows (hermes-cron--rows
               '((jobs . (((id . "j1") (name . "nightly")
                           (schedule . ((expr . "0 0 * * *") (display . "daily")))
                           (state . "scheduled") (profile . "work")
                           (deliver . "telegram") (last_run_at . "2026-01-01")
                           (next_run_at . "2026-01-02") (prompt . "do it"))))))))
    (should (equal (caar rows) "j1"))
    (should (equal (aref (cadr (car rows)) 0) "nightly"))
    (should (equal (aref (cadr (car rows)) 1) "daily"))
    (should (equal (aref (cadr (car rows)) 2) "scheduled"))
    (should (equal (aref (cadr (car rows)) 3) "work"))
    (should (equal (aref (cadr (car rows)) 4) "telegram"))
    (should (equal (aref (cadr (car rows)) 5) "2026-01-01"))
    (should (equal (aref (cadr (car rows)) 6) "2026-01-02"))
    (should (equal (aref (cadr (car rows)) 7) "do it"))
    (should (equal (aref (cadr (car (hermes-cron--rows '((jobs . (((id . "j2")))))))) 2)
                   "scheduled"))
    (should (equal (aref (cadr (car (hermes-cron--rows
                                     '((jobs . (((id . "j3") (enabled . nil)))))))) 2)
                   "disabled"))))

(ert-deftest hermes-cron-api-sends-session-token ()
  "Cron REST calls authenticate with a live dashboard client's session token."
  (let ((client (make-hermes-dashboard-transport-client
                 :host "127.0.0.1" :port 9119 :token "[REDACTED]"))
        request result)
    (let ((hermes-dashboard-transport-http-request-async-function
           (lambda (url &rest args)
             (setq request (list :url url :headers (plist-get args :headers)
                                 :method (plist-get args :method)
                                 :data (plist-get args :data)))
             (hermes--promise-resolved
              '(:status 200 :headers nil :body ((ok . t)))))))
      (hermes--promise-then
       (hermes-cron--api client "POST" "/jobs/j1/trigger" nil '((profile . "work")))
       (lambda (body) (setq result body)))
      (should (equal result '((ok . t))))
      (should (equal (plist-get request :url)
                     "http://127.0.0.1:9119/api/cron/jobs/j1/trigger?profile=work"))
      (should (equal (alist-get "X-Hermes-Session-Token"
                                (plist-get request :headers) nil nil #'equal)
                     "[REDACTED]"))
      (should (equal (plist-get request :method) "POST")))))

(ert-deftest hermes-cron-list-fetches-and-renders ()
  "Listing fetches cron.manage list and renders the jobs."
  (let (action)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-cron-manage)
               (lambda (_client &rest args)
                 (setq action (plist-get args :action))
                 (funcall (plist-get args :resolve)
                          '((jobs . (((job_id . "j1") (name . "nightly")))))))))
      (unwind-protect
          (progn
            (hermes-list-crons)
            (should (equal action "list"))
            (with-current-buffer "*Hermes Cron*"
              (should (derived-mode-p 'hermes-cron-mode))
              (should (equal (caar tabulated-list-entries) "j1"))))
        (when (get-buffer "*Hermes Cron*") (kill-buffer "*Hermes Cron*"))))))

(ert-deftest hermes-cron-toggle-resumes-paused-job ()
  "Toggling a paused or disabled job sends the resume action."
  (let (actions)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-cron-manage)
               (lambda (_client &rest args)
                 (push (plist-get args :action) actions)
                 (funcall (plist-get args :resolve)
                          (if (equal (plist-get args :action) "list")
                              '((jobs . (((job_id . "j1") (name . "n") (state . "paused")))))
                            '((ok . t)))))))
      (unwind-protect
          (progn
            (hermes-list-crons)
            (with-current-buffer "*Hermes Cron*"
              (goto-char (point-min))
              (hermes-cron-toggle))
            (should (member "resume" actions)))
        (when (get-buffer "*Hermes Cron*") (kill-buffer "*Hermes Cron*")))))
  (let (actions)
    (cl-letf (((symbol-function 'hermes-cron--act)
               (lambda (action _id _message)
                 (push action actions))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry "disabled"))
        (hermes-cron-toggle))
      (should (equal actions '("resume"))))))

(ert-deftest hermes-cron-edit-updates-job-at-point ()
  "Editing sends the update payload for the selected cron job."
  (let (calls refreshed messages)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (_client method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved (cond
					    ((and (equal method "GET") (equal path "/jobs/j1"))
					     '((id . "j1") (profile . "work") (name . "nightly")
					       (schedule . ((expr . "0 0 * * *"))) (prompt . "old")
					       (deliver . "local") (skills . ("old-skill"))))
					    ((equal method "PUT") '((id . "j1")))))))
              ((symbol-function 'read-string)
               (lambda (prompt &optional _initial &rest _)
                 (cond
                  ((string-prefix-p "Name" prompt) "edited")
                  ((string-prefix-p "Schedule" prompt) "*/5 * * * *")
                  ((string-prefix-p "Prompt" prompt) "new prompt")
                  ((string-prefix-p "Deliver" prompt) "telegram")
                  ((string-prefix-p "Skills" prompt) "emacs, cron")
                  (t ""))))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-edit))
      (let* ((put (car (last (nreverse calls))))
             (updates (hermes-transport--get (nth 2 put) 'updates)))
        (should (equal (nth 0 put) "PUT"))
        (should (equal (nth 1 put) "/jobs/j1"))
        (should (equal (cdr (assq 'profile (nth 3 put))) "work"))
        (should (equal (hermes-transport--get updates 'name) "edited"))
        (should (equal (hermes-transport--get updates 'schedule) "*/5 * * * *"))
        (should (equal (hermes-transport--get updates 'prompt) "new prompt"))
        (should (equal (hermes-transport--get updates 'deliver) "telegram"))
        (should (equal (hermes-transport--get updates 'skills) '("emacs" "cron")))
        (should refreshed)
        (should (member "Hermes: updated j1" messages))))))

(ert-deftest hermes-cron-trigger-posts-job-at-point ()
  "Trigger-now posts to the selected cron job endpoint."
  (let (call refreshed)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (_client method path &optional body query)
                 (setq call (list method path body query))
                 (hermes--promise-resolved '((id . "j1")))))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (equal (nth 0 call) "POST"))
      (should (equal (nth 1 call) "/jobs/j1/trigger"))
      (should-not (nth 2 call))
      (should (equal (cdr (assq 'profile (nth 3 call))) "work"))
      (should refreshed))))

(ert-deftest hermes-cron-trigger-refreshes-after-transient-client-cleanup ()
  "Trigger-now cleans up a transient client before refreshing the list."
  (let (events)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client
                          (lambda ()
                            (setq events (append events '(done)))))))
              ((symbol-function 'hermes-cron--api)
               (lambda (&rest _)
                 (setq events (append events '(trigger)))
                 (hermes--promise-resolved '((id . "j1")))))
              ((symbol-function 'hermes-list-crons)
               (lambda ()
                 (setq events (append events '(refresh)))))
              ((symbol-function 'message) #'ignore))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (equal events '(trigger done refresh))))))

(ert-deftest hermes-cron-create-refreshes-the-list-on-success ()
  "Creating a cron job refreshes the list so the new row appears."
  (let (refreshed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (_make-promise &optional on-success)
                 (when on-success (funcall on-success '((id . "j9"))))))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message) #'ignore))
      (hermes-cron-create "nightly" "0 0 * * *" "do it"))
    (should refreshed)))

(ert-deftest hermes-cron-show-fetches-job-and-run-history ()
  "Detail view fetches the job and run history, then renders both."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (_client method path &optional _body query)
                 (push (list method path query) calls)
                 (hermes--promise-resolved (cond
					    ((equal path "/jobs/j1")
					     '((id . "j1") (profile . "work") (name . "nightly")
					       (schedule . ((display . "Daily at midnight")))
					       (state . "scheduled") (deliver . "telegram")
					       (prompt . "do it")))
					    ((equal path "/jobs/j1/runs")
					     '((runs . (((id . "cron_j1_1") (title . "Run one")
							 (message_count . 3) (source . "cron")
							 (started_at . 1780000000) (ended_at . 1780000030)))))))))))
      (unwind-protect
          (progn
            (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
              (hermes-cron-show))
            (with-current-buffer "*Hermes Cron Job*"
              (let ((text (buffer-string)))
                (should (string-match-p "nightly" text))
                (should (string-match-p "Daily at midnight" text))
                (should (string-match-p "Runs (RET" text))
                (should (string-match-p "Run one" text))))
            (should (member '("GET" "/jobs/j1" ((profile . "work"))) calls))
            (should (member '("GET" "/jobs/j1/runs" ((profile . "work") (limit . 20)))
                            calls)))
        (when (get-buffer "*Hermes Cron Job*")
          (kill-buffer "*Hermes Cron Job*"))))))

(ert-deftest hermes-cron-trigger-reports-api-errors ()
  "Trigger-now reports REST failures without refreshing the list."
  (let (messages refreshed)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (&rest _) (hermes--promise-rejected "boom")))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (cl-some (lambda (message) (string-match-p "boom" message)) messages))
      (should-not refreshed))))

;;; Group: failure surfacing and run logs

(ert-deftest hermes-cron-rows-face-failed-runs ()
  "A failed last run faces the last-run cell error; an ok run faces it success."
  (let ((failed (car (hermes-cron--rows
                      '((jobs . (((id . "j") (name . "n") (state . "scheduled")
                                  (last_status . "error") (last_run_at . "t1"))))))))
        (ok (car (hermes-cron--rows
                  '((jobs . (((id . "k") (name . "m") (state . "scheduled")
                              (last_status . "ok") (last_run_at . "t2")))))))))
    (should (eq (get-text-property 0 'face (aref (cadr failed) 5))
                'hermes-browser-error))
    (should (eq (get-text-property 0 'face (aref (cadr ok) 5))
                'hermes-browser-success))))

(ert-deftest hermes-cron-state-cell-faces-error-but-keeps-text ()
  "An error state is faced yet still equals the bare string for commands."
  (let ((cell (hermes-cron--state-cell '((state . "error")))))
    (should (eq (get-text-property 0 'face cell) 'hermes-browser-error))
    (should (equal cell "error"))
    (should (member cell '("error")))))

(ert-deftest hermes-cron-deliver-cell-faces-delivery-failure ()
  "A recorded delivery error faces the deliver cell without changing its text."
  (let ((cell (hermes-cron--deliver-cell
               '((deliver . "telegram") (last_delivery_error . "boom")))))
    (should (eq (get-text-property 0 'face cell) 'hermes-browser-error))
    (should (equal cell "telegram"))))

(ert-deftest hermes-cron-rows-face-profile-and-timestamps ()
  "Cron rows distinguish profile names and secondary timestamps."
  (let* ((row (car (hermes-cron--rows
                    '((jobs . (((id . "j") (profile . "work")
                                (last_run_at . "old")
                                (next_run_at . "next"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-profile))
    (should (eq (get-text-property 0 'face (aref entry 5))
                'hermes-browser-muted))
    (should (eq (get-text-property 0 'face (aref entry 6))
                'hermes-browser-muted))))

(ert-deftest hermes-cron-format-run-is-navigable ()
  "A run line carries its session id and a RET keymap."
  (let ((line (hermes-cron--format-run '((id . "cron_j_1") (message_count . "3")))))
    (should (equal (get-text-property 0 'hermes-cron-run-id line) "cron_j_1"))
    (should (keymapp (get-text-property 0 'keymap line)))))

(ert-deftest hermes-cron-show-run-log-requires-run-at-point ()
  "Opening a run log errors when point is not on a run line."
  (with-temp-buffer
    (insert "no run here")
    (goto-char (point-min))
    (should-error (hermes-cron-show-run-log) :type 'user-error)))

(ert-deftest hermes-cron-message-text-handles-string-and-parts ()
  "Run message content is read from a plain string or structured text parts."
  (should (equal (hermes-cron--message-text '((role . "user") (content . "hi")))
                 "hi"))
  (should (equal (hermes-cron--message-text
                  '((content . [((text . "a")) ((text . "b"))])))
                 "a\nb"))
  (should (equal (hermes-cron--message-text
                  '((content . (((text . "x")) ((text . "y"))))))
                 "x\ny"))
  (should (equal (hermes-cron--message-text '((role . "user"))) "")))

(ert-deftest hermes-cron-format-message-includes-role-and-text ()
  "A formatted run message shows its role header and content."
  (let ((out (hermes-cron--format-message '((role . "assistant") (content . "done")))))
    (should (string-match-p "## assistant" out))
    (should (string-match-p "done" out))))

;;; Group: failure notifications and auto-refresh

(ert-deftest hermes-cron-note-failures-baselines-then-alerts ()
  "The first render baselines silently; a newer failure then notifies once."
  (with-temp-buffer
    (let ((hermes-cron-notify-on-failure t)
          notes)
      (cl-letf (((symbol-function 'hermes-browser--notify)
                 (lambda (title body) (push (cons title body) notes))))
        (hermes-cron--note-failures
         '((jobs . (((id . "j") (name . "n") (last_status . "error")
                     (last_run_at . "t1"))))))
        (should-not notes)
        (hermes-cron--note-failures
         '((jobs . (((id . "j") (name . "n") (last_status . "error")
                     (last_run_at . "t1"))))))
        (should-not notes)
        (hermes-cron--note-failures
         '((jobs . (((id . "j") (name . "n") (last_status . "error")
                     (last_run_at . "t2"))))))
        (should (= (length notes) 1))))))

(ert-deftest hermes-cron-note-failures-silent-when-disabled ()
  "No notification fires while `hermes-cron-notify-on-failure' is nil."
  (with-temp-buffer
    (let ((hermes-cron-notify-on-failure nil)
          notes)
      (cl-letf (((symbol-function 'hermes-browser--notify)
                 (lambda (&rest _) (push t notes))))
        (hermes-cron--note-failures
         '((jobs . (((id . "j") (last_status . "error") (last_run_at . "t1"))))))
        (hermes-cron--note-failures
         '((jobs . (((id . "j") (last_status . "error") (last_run_at . "t2"))))))
        (should-not notes)))))

(ert-deftest hermes-cron-auto-refresh-starts-and-stops ()
  "A configured interval starts a per-buffer timer that stop cancels."
  (with-temp-buffer
    (let ((hermes-cron-auto-refresh-interval 5))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'timer)))
        (hermes-cron--maybe-start-auto-refresh)
        (should (eq hermes-cron--auto-refresh-timer 'timer)))
      (cl-letf (((symbol-function 'cancel-timer) #'ignore))
        (hermes-cron--stop-auto-refresh)
        (should-not hermes-cron--auto-refresh-timer)))))

(ert-deftest hermes-cron-auto-refresh-disabled-when-unset ()
  "No timer starts when the interval is nil."
  (with-temp-buffer
    (let ((hermes-cron-auto-refresh-interval nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (error "should not start a timer"))))
        (hermes-cron--maybe-start-auto-refresh)
        (should-not hermes-cron--auto-refresh-timer)))))

(provide 'hermes-cron-tests)
;;; hermes-cron-tests.el ends here
