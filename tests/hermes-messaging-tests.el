;;; hermes-messaging-tests.el --- Messaging browser tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-messaging-selects-instance-before-profile ()
  "Interactive messaging selection reads profiles under the chosen instance."
  (let ((instance '("remote" . "https://hermes.example.test"))
        profile-instance fetched)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-messaging--read-profile)
               (lambda ()
                 (setq profile-instance hermes-instance)
                 "work"))
              ((symbol-function 'hermes-messaging--fetch)
               (lambda (profile &optional display _target _generation selected)
                 (setq fetched (list profile display selected)))))
      (call-interactively #'hermes-list-messaging-platforms)
      (should (equal profile-instance instance))
      (should (equal fetched (list "work" t instance))))))

(ert-deftest hermes-messaging-profile-selection-keeps-instance ()
  "Selecting another profile keeps the catalog's owning instance."
  (let ((instance '("remote" . "https://hermes.example.test")) seen)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-messaging--read-profile)
               (lambda () "work"))
              ((symbol-function 'hermes-list-messaging-platforms)
               (lambda (&rest args) (setq seen args))))
      (with-temp-buffer
        (setq hermes-instance instance)
        (hermes-messaging-select-profile))
      (should (equal seen (list "work" instance))))))

(require 'hermes-messaging)

(defun hermes-messaging-test--platform (&optional enabled)
  "Return one representative platform payload with ENABLED state."
  `((id . "telegram")
    (name . "Telegram")
    (description . "Bot messaging")
    (enabled . ,(if enabled t :false))
    (configured . t)
    (state . "connected")
    (env_vars . (((key . "TELEGRAM_BOT_TOKEN")
                  (required . t) (is_set . t)
                  (redacted_value . "...SECRET-SUFFIX")
                  (description . "Bot token") (is_password . t))
                 ((key . "TELEGRAM_ALLOWED_USERS")
                  (required . :false) (is_set . :false)
                  (description . "Allowed users") (is_password . :false))))))

(ert-deftest hermes-messaging-rows-and-detail-never-display-env-values ()
  "Catalog rows/details expose state and schema, never redacted secret values."
  (let* ((platform (hermes-messaging-test--platform t))
         (row (car (hermes-messaging--rows `((platforms . (,platform))))))
         (detail (hermes-messaging--detail-text platform "work")))
    (should (equal (car row) "telegram"))
    (should (equal (append (cadr row) nil)
                   '("Telegram" "on" "yes" "connected" "Bot messaging")))
    (should (string-match-p "TELEGRAM_BOT_TOKEN.*set" detail))
    (should (string-match-p "TELEGRAM_ALLOWED_USERS.*unset" detail))
    (should-not (string-match-p "SECRET-SUFFIX" detail))))

(ert-deftest hermes-messaging-projects-current-backend-lifecycle-states ()
  "Catalog rows and details preserve every current backend lifecycle state."
  (dolist (state '("disabled" "not_configured" "pending_restart"
                   "gateway_stopped" "startup_failed" "connecting"
                   "connected" "retrying" "disconnected" "paused" "fatal"))
    (let* ((platform (copy-tree (hermes-messaging-test--platform t)))
           (_ (setf (alist-get 'state platform) state))
           (row (car (hermes-messaging--rows `((platforms . (,platform))))))
           (detail (hermes-messaging--detail-text platform "work")))
      (should (equal (aref (cadr row) 3) state))
      (should (string-match-p
               (format "State:      %s" (regexp-quote state)) detail)))))

(ert-deftest hermes-messaging-list-profile-scopes-get-and-ignores-stale-result ()
  "A catalog GET carries profile and cannot render after buffer ownership changes."
  (let ((request (hermes--promise-make)) seen target)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (setq seen (list method path (plist-get args :query)))
                     request))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (hermes-list-messaging-platforms "work")
          (setq target (get-buffer "*Hermes Messaging: work*"))
          (should (equal seen
                         '("GET" "/api/messaging/platforms"
                           ((profile . "work")))))
          (with-current-buffer target
            (setq hermes-messaging-profile "other"))
          (hermes--promise-resolve
           request `((platforms . (,(hermes-messaging-test--platform t)))))
          (with-current-buffer target
            (should-not tabulated-list-entries)))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest hermes-messaging-newest-profile-selection-owns-display ()
  "An older displayed profile request cannot render or pop after a newer one."
  (let ((old-request (hermes--promise-make))
        (new-request (hermes--promise-make))
        renders pops old-target new-target)
    (unwind-protect
        (let ((hermes-messaging--display-generation 0))
          (cl-letf (((symbol-function 'hermes-browser--existing-client)
                     (lambda () 'fake-client))
                    ((symbol-function 'hermes-dashboard-transport-api-request-async)
                     (lambda (_method _path &rest args)
                       (if (equal (cdr (assq 'profile (plist-get args :query)))
                                  "old")
                           old-request
                         new-request)))
                    ((symbol-function 'hermes-messaging--render)
                     (lambda (result buffer)
                       (push (list result buffer) renders)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) (push buffer pops))))
            (hermes-list-messaging-platforms "old")
            (hermes-list-messaging-platforms "new")
            (setq old-target (get-buffer "*Hermes Messaging: old*")
                  new-target (get-buffer "*Hermes Messaging: new*"))
            (hermes--promise-resolve new-request 'new)
            (hermes--promise-resolve old-request 'old)))
      (when (buffer-live-p old-target) (kill-buffer old-target))
      (when (buffer-live-p new-target) (kill-buffer new-target)))
    (should (equal renders `((new ,new-target))))
    (should (equal pops (list new-target)))))

(ert-deftest hermes-messaging-toggle-uses-put-and-refreshes-same-profile ()
  "Toggle PUTs enabled state and refreshes the same profile-owned catalog."
  (let (requests)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :body)
                             (plist-get args :query))
                       requests)
                 (hermes--promise-resolved
                  (if (equal method "GET")
                      `((platforms . (,(hermes-messaging-test--platform t))))
                    '((ok . t))))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform nil))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-toggle)))
    (should (member
             '("PUT" "/api/messaging/platforms/telegram"
               ((enabled . t)) ((profile . "work")))
             requests))
    (should (member
             '("GET" "/api/messaging/platforms" nil ((profile . "work")))
             requests))))

(ert-deftest hermes-messaging-mutations-serialize-through-refreshed-state ()
  "Only one mutation runs; a later toggle derives from refreshed state."
  (let ((first-put (hermes--promise-make)) requests)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :body)) requests)
                 (cond
                  ((and (equal method "PUT") (= (length requests) 1)) first-put)
                  ((equal method "GET")
                   (hermes--promise-resolved
                    `((platforms . (,(hermes-messaging-test--platform t))))))
                  (t (hermes--promise-resolved '((ok . t)))))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform nil))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-toggle)
        (should-error (hermes-messaging-toggle) :type 'user-error)
        (should (= (length requests) 1))
        (hermes--promise-resolve first-put '((ok . t)))
        (goto-char (point-min))
        (hermes-messaging-toggle)))
    (should (equal (mapcar (lambda (request) (nth 2 request))
                           (seq-filter (lambda (request)
                                         (equal (car request) "PUT"))
                                       (reverse requests)))
                   '(((enabled . t)) ((enabled . :false)))))))

(ert-deftest hermes-messaging-concurrent-env-write-stops-before-prompt ()
  "A second env write neither prompts nor races an unsettled first write."
  (let ((put (hermes--promise-make)) (password-reads 0) requests)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _)
                 (setq password-reads (1+ password-reads))
                 "one-secret"))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest _)
                 (push (list method path) requests)
                 (if (equal method "PUT")
                     put
                   (hermes--promise-resolved
                    `((platforms . (,(hermes-messaging-test--platform t))))))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform t))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-set-env)
        (should-error (hermes-messaging-set-env) :type 'user-error)
        (should (= password-reads 1))
        (should (= (length requests) 1))
        (hermes--promise-resolve put '((ok . t)))))))

(ert-deftest hermes-messaging-client-acquisition-failure-clears-mutation-lock ()
  "Synchronous client acquisition failures cannot strand the mutation lock."
  (cl-letf (((symbol-function 'hermes-browser--with-client)
             (lambda (_fn) (error "start failed"))))
    (with-temp-buffer
      (hermes-messaging-mode)
      (setq hermes-messaging-profile "work")
      (hermes-messaging--render
       `((platforms . (,(hermes-messaging-test--platform nil))))
       (current-buffer))
      (goto-char (point-min))
      (should-error (hermes-messaging-toggle) :type 'error)
      (should-not hermes-messaging--mutation-in-flight)
      (let ((error (should-error (hermes-messaging--revert) :type 'error)))
        (should (equal (error-message-string error) "start failed"))))))

(ert-deftest hermes-messaging-stale-mutation-skips-refresh-and-cleans-up ()
  "A superseded mutation issues no GET and releases its client and lock once."
  (let ((put (hermes--promise-make)) requests done-calls target)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client
                          (lambda () (setq done-calls (1+ (or done-calls 0)))))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest _)
                 (push (list method path) requests)
                 put))
              ((symbol-function 'message) #'ignore))
      (setq target (generate-new-buffer " *Hermes stale mutation*"))
      (unwind-protect
          (with-current-buffer target
            (hermes-messaging-mode)
            (setq hermes-messaging-profile "work")
            (hermes-messaging--render
             `((platforms . (,(hermes-messaging-test--platform nil))))
             target)
            (goto-char (point-min))
            (hermes-messaging-toggle)
            (hermes-browser--next-request-generation)
            (hermes--promise-resolve put '((ok . t)))
            (should-not hermes-messaging--mutation-in-flight))
        (when (buffer-live-p target) (kill-buffer target))))
    (should (equal requests
                   '(("PUT" "/api/messaging/platforms/telegram"))))
    (should (= done-calls 1))))

(ert-deftest hermes-messaging-set-and-clear-only-catalog-env-keys ()
  "Set/clear derive keys from catalog schema and keep secret input redacted."
  (let (requests read-string-called)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) "top-secret-value"))
              ((symbol-function 'read-string)
               (lambda (&rest _) (setq read-string-called t) "wrong"))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :body)
                             (plist-get args :query)
                             (plist-get args :secrets))
                       requests)
                 (hermes--promise-resolved
                  (if (equal method "GET")
                      `((platforms . (,(hermes-messaging-test--platform t))))
                    '((ok . t))))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform t))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-set-env)
        (goto-char (point-min))
        (hermes-messaging-clear-env)))
    (should-not read-string-called)
    (should (member
             '("PUT" "/api/messaging/platforms/telegram"
               ((env ("TELEGRAM_BOT_TOKEN" . "top-secret-value")))
               ((profile . "work")) ("top-secret-value"))
             requests))
    (should (member
             '("PUT" "/api/messaging/platforms/telegram"
               ((clear_env "TELEGRAM_BOT_TOKEN"))
               ((profile . "work")) nil)
             requests))))

(ert-deftest hermes-messaging-test-reports-backend-message-only-while-current ()
  "Late platform-test results neither report nor trigger a catalog refresh."
  (let ((test (hermes--promise-make)) requests messages)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :query)) requests)
                 test))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform t))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-test)
        (hermes-browser--next-request-generation)
        (hermes--promise-resolve
         test '((ok . :false) (state . "error")
                (message . "bad token SECRET-SUFFIX")))))
    (should (equal requests
                   '(("POST" "/api/messaging/platforms/telegram/test"
                      ((profile . "work"))))))
    (should-not messages)))

(ert-deftest hermes-messaging-test-reports-safe-backend-message ()
  "A current platform test reports recognized backend text."
  (let (messages)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((ok . t) (state . "connected")
                    (message . "Telegram is connected.")))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform t))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-test)))
    (should (equal messages '("Hermes: Telegram is connected.")))))

(ert-deftest hermes-messaging-test-fails-closed-for-runtime-errors ()
  "Untrusted runtime error text is replaced rather than pattern-redacted."
  (let ((platform (hermes-messaging-test--platform t)))
    (dolist (message '("authentication failed for abc123"
                       "authentication failed for 'abc123'"
                       "authentication failed: abc123!"
                       "authentication failed for a-very-long-unlabelled-credential"))
      (let ((safe (hermes-messaging--safe-test-message
                   `((ok . :false) (state . "error") (message . ,message))
                   platform)))
        (should (equal safe "Platform test failed."))
        (should-not (string-match-p (regexp-quote message) safe))))))

(ert-deftest hermes-messaging-stale-rejections-are-silent ()
  "Killed, superseded, and profile-switched GET/PUT/POST failures stay silent."
  (let ((get-request (hermes--promise-make))
        (put-request (hermes--promise-make))
        (post-request (hermes--promise-make))
        messages get-target)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) get-request))
                ((symbol-function 'pop-to-buffer) #'ignore))
        (hermes-list-messaging-platforms "gone")
        (setq get-target (get-buffer "*Hermes Messaging: gone*"))
        (kill-buffer get-target)
        (hermes--promise-reject get-request "late GET failure"))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) put-request)))
        (with-temp-buffer
          (hermes-messaging-mode)
          (setq hermes-messaging-profile "work")
          (hermes-messaging--render
           `((platforms . (,(hermes-messaging-test--platform nil))))
           (current-buffer))
          (goto-char (point-min))
          (hermes-messaging-toggle)
          (hermes-browser--next-request-generation)
          (hermes--promise-reject put-request "late PUT failure")))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) post-request)))
        (with-temp-buffer
          (hermes-messaging-mode)
          (setq hermes-messaging-profile "work")
          (hermes-messaging--render
           `((platforms . (,(hermes-messaging-test--platform t))))
           (current-buffer))
          (goto-char (point-min))
          (hermes-messaging-test)
          (setq hermes-messaging-profile "other")
          (hermes--promise-reject post-request "late POST failure"))))
    (should-not messages)))

(ert-deftest hermes-messaging-current-rejection-redacts-submitted-secret ()
  "A current request failure reports safely without echoing its submitted secret."
  (let (messages)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection)))
              ((symbol-function 'read-passwd) (lambda (&rest _) "abc123"))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-rejected "failed: abc123")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-messaging-mode)
        (setq hermes-messaging-profile "work")
        (hermes-messaging--render
         `((platforms . (,(hermes-messaging-test--platform t))))
         (current-buffer))
        (goto-char (point-min))
        (hermes-messaging-set-env)))
    (should (equal messages '("Hermes: failed: <redacted>")))
    (should-not (string-match-p "abc123" (car messages)))))

(provide 'hermes-messaging-tests)
;;; hermes-messaging-tests.el ends here
