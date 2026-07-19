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

(ert-deftest hermes-cron-toggle-resumes-paused-job ()
  "Toggling a disabled job selects the resume action."
  (let (actions)
    (cl-letf (((symbol-function 'hermes-cron--act)
               (lambda (action _id _profile _message)
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
              ((symbol-function 'hermes-cron--revert)
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
              ((symbol-function 'hermes-cron--revert)
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
              ((symbol-function 'hermes-cron--revert)
               (lambda ()
                 (setq events (append events '(refresh)))))
              ((symbol-function 'message) #'ignore))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (equal events '(trigger done refresh))))))

(ert-deftest hermes-cron-mutation-refreshes-after-newer-read ()
  "A completed cron mutation starts a fresh read after an intervening refresh."
  (let ((promise (hermes--promise-make)) refreshed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-cron--api)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-cron--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (let ((origin (current-buffer)))
          (hermes-cron-trigger)
          (hermes-browser--next-request-generation)
          (hermes--promise-resolve promise '((ok . t)))
          (should (eq refreshed origin)))))))

(ert-deftest hermes-cron-create-refreshes-the-list-on-success ()
  "Creating a cron job refreshes the list so the new row appears."
  (let (refreshed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (_make-promise &optional on-success)
                 (when on-success (funcall on-success '((id . "j9"))))))
              ((symbol-function 'hermes-cron--revert)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message) #'ignore))
      (hermes-test-with-cron-buffer nil
        (hermes-cron-create "nightly" "0 0 * * *" "do it")))
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

(ert-deftest hermes-cron-show-keeps-newest-detail ()
  "An older cron detail request cannot replace a newer result."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (job-requests 0)
        displayed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-cron--api)
               (lambda (_client _method path &optional _body _query)
                 (if (string-suffix-p "/runs" path)
                     (hermes--promise-resolved '((runs . nil)))
                   (setq job-requests (1+ job-requests))
                   (if (= job-requests 1) first second))))
              ((symbol-function 'hermes-cron--display-detail)
               (lambda (job _runs)
                 (push (hermes-transport--get job 'name) displayed))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-show)
        (hermes-cron-show)
        (hermes--promise-resolve second '((id . "j1") (name . "new")))
        (hermes--promise-resolve first '((id . "j1") (name . "old"))))
      (should (equal displayed '("new"))))))

(ert-deftest hermes-cron-show-ignores-result-after-origin-killed ()
  "A cron detail response is ignored after its originating list buffer dies."
  (let ((promise (hermes--promise-make)) displayed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-cron--api)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-cron--display-detail)
               (lambda (&rest _) (setq displayed t))))
      (let ((origin (generate-new-buffer " *Hermes cron detail origin*")))
        (with-current-buffer origin
          (hermes-cron-mode)
          (setq tabulated-list-entries (list (hermes-test--cron-entry)))
          (tabulated-list-print)
          (goto-char (point-min))
          (hermes-cron-show))
        (kill-buffer origin)
        (hermes--promise-resolve promise '((id . "j1") (name . "late")))
        (should-not displayed)))))

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

(ert-deftest hermes-cron-rest-list-keeps-paused-profile-job ()
  "REST listing requests all profiles and keeps paused jobs visible."
  (let (request)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-cron--api)
               (lambda (client method path &optional body query)
                 (setq request (list client method path body query))
                 (hermes--promise-resolved
                  '(((id . "j1") (name . "nightly") (state . "paused")
                     (profile . "work")))))))
      (unwind-protect
          (progn
            (hermes-list-crons)
            (should (equal (nth 1 request) "GET"))
            (should (equal (nth 2 request) "/jobs"))
            (should (equal (cdr (assq 'profile (nth 4 request))) "all"))
            (with-current-buffer "*Hermes Cron*"
              (should (equal (caar tabulated-list-entries) "j1"))
              (should (equal (aref (cadar tabulated-list-entries) 2) "paused"))))
        (when (get-buffer "*Hermes Cron*") (kill-buffer "*Hermes Cron*"))))))

(ert-deftest hermes-cron-rest-toggle-propagates-profile ()
  "Toggling a paused job resumes it through its profile-aware REST route."
  (let (request refreshed)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (client method path &optional body query)
                 (setq request (list client method path body query))
                 (hermes--promise-resolved
                  '((id . "j1") (state . "scheduled")))))
              ((symbol-function 'hermes-cron--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry "paused"))
        (hermes-cron-toggle)
        (should (eq refreshed (current-buffer))))
      (should (equal (nth 1 request) "POST"))
      (should (equal (nth 2 request) "/jobs/j1/resume"))
      (should (equal (cdr (assq 'profile (nth 4 request))) "work")))))

(ert-deftest hermes-cron-remove-does-not-report-declared-failure ()
  "A backend `ok' false result is reported as failure, never success."
  (let (messages refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (&rest _)
                 (hermes--promise-resolved '((ok . :false) (error . "busy")))))
              ((symbol-function 'hermes-cron--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-remove))
      (should-not refreshed)
      (should-not (cl-some (lambda (text) (string-match-p "removed" text))
                           messages))
      (should (cl-some (lambda (text) (string-match-p "busy" text)) messages)))))

(ert-deftest hermes-cron-create-posts-default-profile-over-rest ()
  "Creating a job posts its fields through the profile-aware REST API."
  (let (request)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--api)
               (lambda (client method path &optional body query)
                 (setq request (list client method path body query))
                 (hermes--promise-resolved '((id . "j9")))))
              ((symbol-function 'message) #'ignore))
      (hermes-cron-create "nightly" "0 0 * * *" "do it")
      (should (equal (nth 1 request) "POST"))
      (should (equal (nth 2 request) "/jobs"))
      (should (equal (nth 3 request)
                     '((name . "nightly") (schedule . "0 0 * * *")
                       (prompt . "do it"))))
      (should (equal (cdr (assq 'profile (nth 4 request))) "default")))))

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
    (should (equal (get-text-property 0 'face cell)
                   '(hermes-browser-error hermes-browser-state)))
    (should (equal cell "error"))
    (should (member cell '("error")))))

(ert-deftest hermes-cron-deliver-cell-faces-delivery-failure ()
  "A recorded delivery error faces the deliver cell without changing its text."
  (let ((cell (hermes-cron--deliver-cell
               '((deliver . "telegram") (last_delivery_error . "boom")))))
    (should (eq (get-text-property 0 'face cell) 'hermes-browser-error))
    (should (equal cell "telegram"))))

(ert-deftest hermes-cron-rows-face-profile-and-timestamps ()
  "Cron rows give every column its own face."
  (let* ((row (car (hermes-cron--rows
                    '((jobs . (((id . "j") (name . "nightly")
                                (schedule . "daily") (state . "scheduled")
                                (profile . "work") (deliver . "telegram")
                                (last_run_at . "old")
                                (next_run_at . "next")
                                (prompt . "do it"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-name))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-schedule))
    (should (equal (get-text-property 0 'face (aref entry 2))
                   '(hermes-browser-pending hermes-browser-state)))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-profile))
    (should (eq (get-text-property 0 'face (aref entry 4))
                'hermes-browser-delivery))
    (should (eq (get-text-property 0 'face (aref entry 5))
                'hermes-browser-timestamp))
    (should (eq (get-text-property 0 'face (aref entry 6))
                'hermes-browser-timestamp))
    (should (eq (get-text-property 0 'face (aref entry 7))
                'hermes-browser-prompt))))

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

(ert-deftest hermes-cron-show-run-log-keeps-newest-result ()
  "An older run transcript cannot replace a newer request."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        displayed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-cron--fetch-run-messages)
               (lambda (&rest _)
                 (setq requests (1+ requests))
                 (if (= requests 1) first second)))
              ((symbol-function 'hermes-cron--display-run)
               (lambda (_id messages)
                 (push (hermes-cron--message-text (car messages)) displayed))))
      (with-temp-buffer
        (insert (propertize "run" 'hermes-cron-run-id "r1"))
        (special-mode)
        (goto-char (point-min))
        (hermes-cron-show-run-log)
        (hermes-cron-show-run-log)
        (hermes--promise-resolve second
                                 '((messages . (((content . "new"))))))
        (hermes--promise-resolve first
                                 '((messages . (((content . "old")))))))
      (should (equal displayed '("new"))))))

(ert-deftest hermes-cron-show-run-log-ignores-killed-origin ()
  "A run transcript response is ignored after its detail buffer dies."
  (let ((promise (hermes--promise-make)) displayed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-cron--fetch-run-messages)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-cron--display-run)
               (lambda (&rest _) (setq displayed t))))
      (let ((origin (generate-new-buffer " *Hermes cron run origin*")))
        (with-current-buffer origin
          (insert (propertize "run" 'hermes-cron-run-id "r1"))
          (special-mode)
          (goto-char (point-min))
          (hermes-cron-show-run-log))
        (kill-buffer origin)
        (hermes--promise-resolve promise '((messages . nil)))
        (should-not displayed)))))

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

(ert-deftest hermes-cron-auto-refresh-stops-on-mode-change ()
  "Leaving cron mode cancels its buffer-local refresh timer."
  (let ((hermes-cron-auto-refresh-interval 5) cancelled)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer))))
      (with-temp-buffer
        (hermes-cron-mode)
        (should (eq hermes-cron--auto-refresh-timer 'timer))
        (fundamental-mode)
        (should (eq cancelled 'timer))))))

(ert-deftest hermes-cron-auto-refresh-tick-ignores-wrong-mode ()
  "A queued timer tick cannot refresh a buffer that left cron mode."
  (let (refreshed)
    (cl-letf (((symbol-function 'hermes-cron--revert)
               (lambda (&rest _) (setq refreshed t))))
      (with-temp-buffer
        (fundamental-mode)
        (hermes-cron--auto-refresh-tick (current-buffer))
        (should-not refreshed)))))

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
