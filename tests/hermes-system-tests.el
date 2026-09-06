;;; hermes-system-tests.el --- Gateway status and log tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-system-api-uses-status-and-log-routes ()
  "System requests preserve status path and log tail query."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :query)) calls)
                 (hermes--promise-resolved '((ok . t))))))
      (hermes-system--api 'client "/api/status")
      (hermes-system--api 'client "/api/logs" '((lines . 25))))
    (should (member '("GET" "/api/status" nil) calls))
    (should (member '("GET" "/api/logs" ((lines . 25))) calls))))

(ert-deftest hermes-system-redacts-secret-shaped-log-values ()
  "Management log rendering removes token, API-key, and bearer values."
  (let ((safe (hermes-system--redact-text
               "token=abc api_key: xyz Authorization: Bearer credential-value")))
    (should-not (string-match-p "abc\\|xyz\\|credential-value" safe))
    (should (string-match-p "<redacted>" safe))))

(ert-deftest hermes-system-logs-bounds-and-preserves-tail-on-refresh ()
  "Log requests clamp their tail and refresh with the same query."
  (let (queries)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success on-error)
                 (hermes--promise-catch
                  (hermes--promise-then (funcall make-promise 'client) on-success)
                  on-error)))
              ((symbol-function 'hermes-system--api)
               (lambda (_client _path &optional query)
                 (push query queries)
                 (hermes--promise-resolved '((lines . ("one")))))))
      (unwind-protect
          (progn
            (hermes-system-logs 900)
            (with-current-buffer "*Hermes Logs*"
              (funcall revert-buffer-function nil t)))
        (when-let* ((buffer (get-buffer "*Hermes Logs*")))
          (kill-buffer buffer))))
    (should (equal queries
                   '(((file . "agent") (lines . 500))
                     ((file . "agent") (lines . 500)))))))

(ert-deftest hermes-system-renders-request-errors-in-owning-buffer ()
  "A failed system request replaces the view with a visible error."
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
            ((symbol-function 'hermes-browser--run-on-client)
             (lambda (make-promise &optional on-success on-error)
               (hermes--promise-catch
                (hermes--promise-then (funcall make-promise 'client) on-success)
                on-error)))
            ((symbol-function 'hermes-system--api)
             (lambda (&rest _)
               (hermes--promise-rejected "HTTP 503 unavailable"))))
    (unwind-protect
        (progn
          (hermes-system-status)
          (with-current-buffer "*Hermes Status*"
            (should (derived-mode-p 'hermes-system-mode))
            (should (string-match-p "Error: HTTP 503 unavailable"
                                    (buffer-string)))))
      (when-let* ((buffer (get-buffer "*Hermes Status*")))
        (kill-buffer buffer)))))

(ert-deftest hermes-system-ignores-stale-refresh-results ()
  "An older status response cannot replace a reopened status view."
  (let (callbacks)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (_make-promise &optional on-success _on-error)
                 (push on-success callbacks))))
      (unwind-protect
          (progn
            (hermes-system-status)
            (hermes-system-status)
            (funcall (car callbacks) '((gateway_state . "new")))
            (funcall (cadr callbacks) '((gateway_state . "old")))
            (with-current-buffer "*Hermes Status*"
              (should (string-match-p "new" (buffer-string)))
              (should-not (string-match-p "old" (buffer-string)))))
        (when-let* ((buffer (get-buffer "*Hermes Status*")))
          (kill-buffer buffer))))))

(ert-deftest hermes-system-log-filters-use-backend-query ()
  "Native controls send the server's minimum-level and component filters."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--path "/api/logs"
          hermes-system--query '((file . "agent") (lines . 100)))
    (let (queries)
      (cl-letf (((symbol-function 'hermes-system--fetch)
                 (lambda (_) (push hermes-system--query queries))))
        (hermes-system-log-source "errors")
        (hermes-system-log-level "WARNING")
        (hermes-system-log-component "cron")
        (hermes-system-log-lines 900))
      (should (equal (car queries)
                     '((component . "cron") (level . "WARNING")
                       (file . "errors") (lines . 500))))
      (should (string-match-p "errors.*WARNING.*cron.*500"
                              (hermes-system--header-line)))
      (should-error (hermes-system-log-source "../secret") :type 'user-error)
      (should-error (hermes-system-log-level "BOGUS") :type 'user-error))
    (should (eq (key-binding "s") #'hermes-system-log-source))
    (should (eq (key-binding "a") #'hermes-system-log-auto-refresh))))

(ert-deftest hermes-system-log-poll-is-bounded-and-retired ()
  "Polling waits for settlement and old timers cannot restart hidden views."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (hermes-system-mode)
      (setq hermes-system--heading "Logs"
            hermes-system--path "/api/logs"
            hermes-system--query '((file . "agent") (lines . 100)))
      (let (resolve tick timer-calls canceled)
        (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                   (lambda (_make &optional success _error)
                     (setq resolve success)))
                  ((symbol-function 'run-at-time)
                   (lambda (seconds repeat function &rest args)
                     (should (= seconds 5))
                     (should-not repeat)
                     (cl-incf timer-calls)
                     (setq tick (lambda () (apply function args)))
                     'timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (_) (setq canceled t))))
          (setq timer-calls 0)
          (hermes-system-log-auto-refresh)
          (should (= timer-calls 0))
          (funcall resolve '((lines . ("first"))))
          (should (= timer-calls 1))
          (let ((old-tick tick))
            (hermes-system--fetch (current-buffer))
            (setq canceled nil)
            (funcall resolve '((lines . ("second"))))
            (should (= timer-calls 2))
            (funcall old-tick)
            (should-not canceled)
            (should (= timer-calls 2)))
          (switch-to-buffer (get-buffer-create " *system hidden*"))
          (funcall tick)
          (should canceled)
          (should (= timer-calls 2)))
        (kill-buffer " *system hidden*")))))

(ert-deftest hermes-system-log-mode-change-retires-late-results ()
  "Mode replacement invalidates both success and timer authority."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--heading "Logs" hermes-system--path "/api/logs")
    (let (resolve)
      (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                 (lambda (_make &optional success _error) (setq resolve success))))
        (hermes-system--fetch (current-buffer))
        (fundamental-mode)
        (let ((inhibit-read-only t)) (insert "replacement"))
        (funcall resolve '((lines . ("late"))))
        (should (equal (buffer-string) "replacement"))))))

(ert-deftest hermes-system-empty-log-tail-is-not-a-printed-payload ()
  "Empty tails remain a readable empty state, not a raw JSON/Lisp object."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--heading "Logs" hermes-system--path "/api/logs")
    (hermes-system--render (current-buffer) '((file . "agent") (lines . nil)))
    (should (string-match-p "No log lines" (buffer-string)))))

(ert-deftest hermes-system-requests-pin-owner-and-redact-captured-credentials ()
  "Settlements use their exact endpoint lifetime and release shared clients."
  (dolist (ending '(success error stale-error refresh instance reconnect endpoint mode kill))
    (with-temp-buffer
      (hermes-system-mode)
      (setq hermes-system--heading "Logs" hermes-system--path "/api/logs"
            hermes-instance '(:name "test" :url "http://example.test"))
      (let ((buffer (current-buffer))
            (client (make-hermes-dashboard-transport-client
                     :base-url "http://example.test" :token "fixture-credential"))
            (promise (hermes--promise-make))
            (released 0))
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn)
                     (funcall fn client (lambda () (cl-incf released)))))
                  ((symbol-function 'hermes-system--api)
                   (lambda (owner path &optional _query)
                     (should (eq owner client))
                     (should (equal path "/api/logs"))
                     promise)))
          (hermes-system--fetch (current-buffer))
          (should (= released 0))
          (pcase ending
            ((or 'refresh 'stale-error) (hermes-browser--next-request-generation))
            ('instance (setq hermes-instance '(:name "other" :url "http://else.test")))
            ('reconnect (cl-incf (hermes-dashboard-transport-client-generation client)))
            ('endpoint (setf (hermes-dashboard-transport-client-base-url client)
                             "http://else.test"))
            ('mode (fundamental-mode))
            ('kill (kill-buffer (current-buffer))))
          ;; Token rotation must not remove the old request's redaction material.
          (setf (hermes-dashboard-transport-client-token client) "replacement-token")
          (if (memq ending '(error stale-error))
              (hermes--promise-reject promise "bare fixture-credential failure")
            (hermes--promise-resolve promise '((lines . ("bare fixture-credential")))))
          (should (= released 1))
          (when (buffer-live-p buffer)
            (if (memq ending '(success error))
                (progn
                  (should (string-match-p "<redacted>" (buffer-string)))
                  (should-not (string-match-p "fixture-credential" (buffer-string))))
              (should (string-empty-p (buffer-string))))))))))

(ert-deftest hermes-system-log-render-preserves-reading-position ()
  "Refreshing a bounded tail preserves the reader's line and column."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--heading "Logs" hermes-system--path "/api/logs")
    (hermes-system--render (current-buffer) '((lines . ("first line" "second line"))))
    (goto-char (point-min))
    (forward-line 3)
    (move-to-column 4)
    (hermes-system--render (current-buffer)
                          '((lines . ("next line" "later line" "extra line"))))
    (should (= (line-number-at-pos) 4))
    (should (= (current-column) 4))))

(ert-deftest hermes-system-log-cleanup-cancels-once ()
  "Hide, kill, and mode replacement retire the buffer's timer quietly."
  (dolist (exit '(hide kill mode))
    (let ((buffer (generate-new-buffer " *system cleanup*")))
      (unwind-protect
          (with-current-buffer buffer
            (hermes-system-mode)
            (setq hermes-system--auto-refresh t hermes-system--timer 'timer)
            (let ((canceled 0))
              (cl-letf (((symbol-function 'cancel-timer)
                         (lambda (timer)
                           (should (eq timer 'timer))
                           (cl-incf canceled))))
                (pcase exit
                  ('hide (hermes-system--visibility-change))
                  ('kill (kill-buffer buffer))
                  ('mode (fundamental-mode)))
                (ert-info ((format "Exit: %s" exit))
                  (should (= canceled 1)))
                (when (derived-mode-p 'hermes-system-mode)
                  (hermes-system--stop)
                  (should-not hermes-system--auto-refresh)
                  (should-not hermes-system--timer)
                  (should (= canceled 1))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-system-last-release-settles-and-polls ()
  "Real last release preserves logs/status results and the next log poll."
  (dolist (path '("/api/logs" "/api/status"))
    (dolist (outcome '(success error sync-error))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (hermes-system-mode)
          (setq hermes-system--heading "System" hermes-system--path path
                hermes-instance '(:id "test" :name "test" :url "http://example.test")
                hermes-system--auto-refresh (equal path "/api/logs"))
          (let ((hermes-instances (list hermes-instance))
                (hermes-dashboard-transport-idle-close-delay nil)
                (requests 0) client promise tick)
            (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                      ((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest _)
                         (setq client (make-hermes-dashboard-transport-client
                                       :base-url "http://example.test" :refcount 1))))
                      ((symbol-function 'hermes-system--api)
                       (lambda (&rest _)
                         (cl-incf requests)
                         (when (eq outcome 'sync-error) (error "expected-error"))
                         (setq promise (hermes--promise-make))))
                      ((symbol-function 'run-at-time)
                       (lambda (_seconds _repeat fn &rest args)
                         (setq tick (lambda () (apply fn args))) 'timer))
                      ((symbol-function 'cancel-timer) #'ignore))
              (hermes-system--fetch (current-buffer))
              (dotimes (iteration (if (equal path "/api/logs") 2 1))
                (should (= requests (1+ iteration)))
                (pcase outcome
                  ('success
                   (hermes--promise-resolve promise '((lines . ("expected-result")))))
                  ('error (hermes--promise-reject promise "expected-error")))
                (should (hermes-dashboard-transport-client-stopping-p client))
                (should (= (hermes-dashboard-transport-client-generation client) 1))
                (ert-info ((format "%s %s iteration %s" path outcome iteration))
                  (should (string-match-p (if (eq outcome 'success)
                                             "expected-result" "Error: expected-error")
                                         (buffer-string))))
                (when (equal path "/api/logs")
                  (should hermes-system--auto-refresh)
                  (should tick)
                  (when (zerop iteration) (funcall tick))))
              (when (equal path "/api/logs")
                (if (eq outcome 'success)
                    (setf (hermes-dashboard-transport-client-base-url client)
                          "http://else.test")
                  (setq hermes-instance '(:name "other" :url "http://else.test")))
                (funcall tick)
                (should (= requests 2))
                (should-not hermes-system--auto-refresh))
              (hermes-system--stop))))))))

(ert-deftest hermes-system-render-preserves-two-window-viewports ()
  "Identical and updated tails retain both windows' line/column positions."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (hermes-system-mode)
      (setq hermes-system--heading "Logs" hermes-system--path "/api/logs")
      (let* ((first (selected-window))
             (second (split-window-below))
             (payload (list (cons 'lines
                                  (cl-loop for i from 1 to 100
                                           collect (format "line %d" i))))))
        (set-window-buffer second (current-buffer))
        (hermes-system--render (current-buffer) payload)
        (cl-loop for window in (list first second)
                 for start in '(40 60) do
                 (goto-char (point-min)) (forward-line start)
                 (move-to-column 2)
                 (set-window-start window (point))
                 (forward-line 5) (move-to-column 4)
                 (set-window-point window (point)))
        ;; Buffer point is the selected window's point; the second setup
        ;; above moved it too, so restore the first reader explicitly.
        (goto-char (point-min)) (forward-line 45) (move-to-column 4)
        (dolist (result (list payload
                              (list (cons 'lines
                                          (make-list 100 "updated longer line")))))
          (hermes-system--render (current-buffer) result)
          (cl-loop for window in (list first second)
                   for start in '(41 61) do
                   (should (= (line-number-at-pos (window-start window)) start))
                   (let ((position (window-point window)))
                     (save-excursion
                       (goto-char (window-start window))
                       (should (= (current-column) 2))
                       (goto-char position)
                       (should (= (line-number-at-pos) (+ start 5)))
                       (should (= (current-column) 4))))))
        (hermes-system--render (current-buffer) '((lines . ("x"))))
        (dolist (window (list first second))
          (should (<= (point-min) (window-start window) (point-max)))
          (should (<= (point-min) (window-point window) (point-max))))))))

(ert-deftest hermes-system-popup-describes-existing-actions ()
  "Popup groups expose the real bindings without splicing shared metadata."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--path "/api/logs")
    (let* ((rows (keymap-popup--meta hermes-system-mode-map 'descriptions))
           (groups (apply #'append rows))
           (entries (apply #'append
                           (mapcar (lambda (group) (plist-get group :entries))
                                   groups))))
      (should (equal (mapcar (lambda (group) (plist-get group :name)) groups)
                     '("Filter" "View")))
      (should (equal (mapcar (lambda (group) (length (plist-get group :entries)))
                            groups)
                     '(4 4)))
      (pcase-dolist (`(,key . ,command)
                    '(("s" . hermes-system-log-source)
                      ("l" . hermes-system-log-level)
                      ("c" . hermes-system-log-component)
                      ("n" . hermes-system-log-lines)
                      ("a" . hermes-system-log-auto-refresh)
                      ("g" . revert-buffer)
                      ("q" . quit-window)
                      ("?" . hermes-system-mode-map-popup)))
        (should (eq (key-binding key) command))
        (should (eq (plist-get (cl-find key entries
                                        :key (lambda (entry) (plist-get entry :key))
                                        :test #'equal)
                               :command)
                    command)))
      (let (opened)
        (cl-letf (((symbol-function 'keymap-popup)
                   (lambda (map) (setq opened map))))
          (call-interactively (key-binding "?")))
        (should (eq opened hermes-system-mode-map)))
      (should (plist-get (cl-find "a" entries
                                 :key (lambda (entry) (plist-get entry :key))
                                 :test #'equal)
                         :stay-open)))))

(ert-deftest hermes-system-header-shows-faced-state-and-help ()
  "The log header reflects filters and polling, not a key legend."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--path "/api/logs"
          hermes-system--query '((file . "agent") (lines . 100)))
    (cl-letf (((symbol-function 'hermes-system--fetch) #'ignore))
      (let ((header (hermes-system--header-line)))
        (should (string-match-p "Source agent.*Min ALL.*Component all.*100 lines.*Auto off.*? Help"
                                header))
        (should (eq (get-text-property (string-match "agent" header) 'face header)
                    'font-lock-type-face))
        (should (eq (get-text-property (string-match "off" header) 'face header)
                    'shadow))
        (should (eq (get-text-property (string-match "? Help" header) 'face header)
                    'help-key-binding)))
      (call-interactively (key-binding "a"))
      (let ((header (hermes-system--header-line)))
        (should (string-match "5s" header))
        (should (eq (get-text-property (match-beginning 0) 'face header) 'success)))
      (call-interactively (key-binding "a"))
      (should-not hermes-system--auto-refresh)
      (setq hermes-system--path "/api/status")
      (should (equal (hermes-system--header-line)
                     (concat (hermes-browser--instance-header-line))))
      (should (eq (key-binding "g") #'revert-buffer))
      (should (eq (key-binding "q") #'quit-window)))))

(ert-deftest hermes-system-popup-dispatches-and-refreshes-state ()
  "The real popup dispatches filters and reflects polling without reopening."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (hermes-system-mode)
      (setq hermes-system--path "/api/logs"
            hermes-system--heading "Hermes Logs"
            hermes-system--query '((file . "agent") (lines . 100)))
      (let ((keymap-popup--buffer-name " *hermes system popup test*")
            (keymap-popup-backend #'keymap-popup-backend-side-window)
            (keymap-popup-persistent nil)
            (fetches 0))
        (cl-letf (((symbol-function 'hermes-system--fetch)
                   (lambda (_) (cl-incf fetches)))
                  ((symbol-function 'completing-read)
                   (lambda (prompt &rest _)
                     (pcase prompt
                       ("Log source: " "errors")
                       ("Minimum log level: " "WARNING")
                       ("Log component: " "cron"))))
                  ((symbol-function 'read-number) (lambda (&rest _) 250)))
          (unwind-protect
              (progn
                (call-interactively (key-binding "?"))
                (let* ((popup (get-buffer keymap-popup--buffer-name))
                       (active (keymap-popup--session-get popup :active))
                       (map (plist-get active :wrapper-map)))
                  (should (equal (plist-get active :exit-key) "C-g"))
                  (should (eq (keymap-lookup map "q") #'quit-window))
                  (dolist (key '("s" "l" "c" "n" "a" "g"))
                    (call-interactively (keymap-lookup map key)))
                  (should (= fetches 6))
                  (should (string-match-p "errors.*WARNING.*cron.*250 lines.*5s"
                                          (hermes-system--header-line)))
                  (should (string-match-p "Auto-refresh: 5s"
                                          (with-current-buffer popup (buffer-string))))
                  (call-interactively (keymap-lookup map "a"))
                  (should (= fetches 6))
                  (should-not hermes-system--auto-refresh)
                  (should (eq popup (get-buffer keymap-popup--buffer-name)))
                  (should (string-match-p "Auto-refresh: off"
                                          (with-current-buffer popup (buffer-string))))
                  (setq hermes-system--path "/api/status")
                  (call-interactively (keymap-lookup map "a"))
                  (call-interactively (keymap-lookup map "s"))
                  (should (= fetches 6))
                  (should-not hermes-system--auto-refresh)))
            (keymap-popup-dismiss)))))))

(provide 'hermes-system-tests)
;;; hermes-system-tests.el ends here
