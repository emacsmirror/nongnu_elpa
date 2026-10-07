;;; hermes-system-tests.el --- Gateway status and log tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defun hermes-system-test--with-client (function)
  "Call FUNCTION with a real disposable client, without opening a socket."
  (funcall function (make-hermes-dashboard-transport-client
                     :base-url "http://example.invalid") #'ignore))

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
              ((symbol-function 'hermes-browser--with-client)
               #'hermes-system-test--with-client)
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
            ((symbol-function 'hermes-browser--with-client)
               #'hermes-system-test--with-client)
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
              ((symbol-function 'hermes-browser--with-client)
               #'hermes-system-test--with-client)
              ((symbol-function 'hermes-system--api)
               (lambda (&rest _)
                 (let ((promise (hermes--promise-make)))
                   (push (lambda (result) (hermes--promise-resolve promise result)) callbacks)
                   promise))))
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

(ert-deftest hermes-system-reopen-clears-previous-instance-content ()
  "Status and log views do not relabel an old snapshot as a new instance."
  (dolist (spec '((hermes-system-status . "*Hermes Status*")
                  (hermes-system-logs . "*Hermes Logs*")))
    (let ((instance '("Alpha" . "http://alpha.invalid")) callbacks)
      (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
                ((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'hermes-browser--with-client)
                 #'hermes-system-test--with-client)
                ((symbol-function 'hermes-system--api)
                 (lambda (&rest _)
                   (let ((promise (hermes--promise-make)))
                     (push (lambda (result) (hermes--promise-resolve promise result)) callbacks)
                     promise))))
        (unwind-protect
            (progn
              (funcall (car spec))
              (funcall (car callbacks) '((lines . ("ALPHA-ONLY PROCESS"))))
              (with-current-buffer (cdr spec)
                (should (string-match-p "ALPHA-ONLY" (buffer-string))))
              (setq instance '("Beta" . "http://beta.invalid"))
              (funcall (car spec))
              (with-current-buffer (cdr spec)
                (should (equal hermes-instance instance))
                (should (string-match-p "Loading" (buffer-string)))
                (should-not (string-match-p "ALPHA-ONLY" (buffer-string))))
              (funcall (cadr callbacks) '((lines . ("ALPHA-LATE PROCESS"))))
              (with-current-buffer (cdr spec)
                (should-not (string-match-p "ALPHA" (buffer-string))))
              (funcall (car callbacks) '((lines . ("BETA-ONLY PROCESS"))))
              (with-current-buffer (cdr spec)
                (should (string-match-p "BETA-ONLY" (buffer-string)))))
          (when-let* ((buffer (get-buffer (cdr spec))))
            (kill-buffer buffer)))))))

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
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   #'hermes-system-test--with-client)
                  ((symbol-function 'hermes-system--api)
                   (lambda (&rest _)
                     (let ((promise (hermes--promise-make)))
                       (setq resolve (lambda (result) (hermes--promise-resolve promise result)))
                       promise)))
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
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 #'hermes-system-test--with-client)
                ((symbol-function 'hermes-system--api)
                 (lambda (&rest _)
                   (let ((promise (hermes--promise-make)))
                     (setq resolve (lambda (result) (hermes--promise-resolve promise result)))
                     promise))))
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
                     '("Filter" "View" "Gateway")))
      (should (equal (mapcar (lambda (group) (length (plist-get group :entries)))
                            groups)
                     '(4 4 1)))
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
      (should-not (hermes-system--header-line))
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
                       ((pred (string-prefix-p "Log source:")) "errors")
                       ((pred (string-prefix-p "Minimum log level:")) "WARNING")
                       ((pred (string-prefix-p "Log component:")) "cron"))))
                  ((symbol-function 'read-number) (lambda (&rest _) 250)))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "?"))
                (let ((popup (get-buffer keymap-popup--buffer-name))
                      (owner (current-buffer)))
                  (should (get-buffer-window popup))
                  (should (eq (key-binding "q") #'quit-window))
                  (dolist (key '("s" "l" "c" "n" "a" "g"))
                    (execute-kbd-macro (kbd key))
                    (should (eq popup (get-buffer keymap-popup--buffer-name))))
                  (should (= fetches 6))
                  (should (string-match-p "errors.*WARNING.*cron.*250 lines.*5s"
                                          (hermes-system--header-line)))
                  (should (string-match-p "Auto-refresh: 5s"
                                          (with-current-buffer popup (buffer-string))))
                  (execute-kbd-macro (kbd "a"))
                  (should (= fetches 6))
                  (should-not hermes-system--auto-refresh)
                  (should (eq popup (get-buffer keymap-popup--buffer-name)))
                  (should (string-match-p "Auto-refresh: off"
                                          (with-current-buffer popup (buffer-string))))
                  (setq hermes-system--path "/api/status")
                  (execute-kbd-macro (kbd "a s"))
                  (should (= fetches 6))
                  (should-not hermes-system--auto-refresh)
                  (condition-case nil
                      (execute-kbd-macro (kbd "C-g"))
                    (quit nil))
                  (should-not (buffer-live-p popup))
                  (should (eq (current-buffer) owner))
                  (should (eq major-mode 'hermes-system-mode))
                  (should (= fetches 6))
                  (should-not hermes-system--auto-refresh)))
            (keymap-popup-dismiss)))))))


(ert-deftest hermes-system-popup-shows-each-owners-current-filters ()
  "Rendered descriptions show current filter values, not replacement defaults."
  (save-window-excursion
    (dolist (source '("agent" "gateway"))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (hermes-system-mode)
        (setq hermes-system--path "/api/logs"
              hermes-system--query `((file . ,source) (level . "ERROR")
                                     (component . "cron") (lines . 37)))
        (let ((keymap-popup-backend #'keymap-popup-backend-side-window)
              (keymap-popup--buffer-name " *system labels test*"))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "?"))
                (with-current-buffer keymap-popup--buffer-name
                  (should (string-match-p (concat "Source: " source) (buffer-string)))
                  (should (string-match-p "Min level: ERROR" (buffer-string)))
                  (should (string-match-p "Component: cron" (buffer-string)))
                  (should (string-match-p "Tail lines: 37" (buffer-string)))))
            (keymap-popup-dismiss)))))))


(ert-deftest hermes-system-real-filter-prompt-cancel-preserves-two-owners ()
  "A real popup prompt shows its owner value and cancellation changes neither."
  (save-window-excursion
    (let ((one (generate-new-buffer " *log owner one*"))
          (two (generate-new-buffer " *log owner two*"))
          (keymap-popup-backend #'keymap-popup-backend-side-window)
          (keymap-popup--buffer-name " *log prompt test*"))
      (unwind-protect
          (progn
            (dolist (buffer (list one two))
              (with-current-buffer buffer
                (hermes-system-mode)
                (setq hermes-system--path "/api/logs"
                      hermes-system--query `((file . ,(if (eq buffer one) "agent" "gateway"))))))
            (switch-to-buffer one)
            (let ((noninteractive nil) prompt)
              (minibuffer-with-setup-hook
                  (lambda () (setq prompt (minibuffer-prompt)))
                (condition-case nil
                    (execute-kbd-macro (kbd "? s C-g"))
                  (quit nil)))
              (should (string-match-p "Log source: agent" prompt))
              (should (eq (current-buffer) one)))
            (should (equal (buffer-local-value 'hermes-system--query one) '((file . "agent"))))
            (should (equal (buffer-local-value 'hermes-system--query two) '((file . "gateway")))))
        (keymap-popup-dismiss)
        (kill-buffer one) (kill-buffer two)))))

(ert-deftest hermes-system-filter-prompt-refuses-replaced-owner ()
  "Minibuffer input cannot replace a newer query or status view."
  (with-temp-buffer
    (hermes-system-mode)
    (setq hermes-system--path "/api/logs" hermes-system--query '((file . "agent")))
    (let (fetched)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq hermes-system--path "/api/status") "gateway"))
                ((symbol-function 'hermes-system--fetch) (lambda (&rest _) (setq fetched t))))
        (should-error (call-interactively #'hermes-system-log-source) :type 'user-error)
        (should-not fetched)
        (should (equal hermes-system--query '((file . "agent"))))))))

(ert-deftest hermes-system-acquisition-failure-cannot-start-polling ()
  "A public log retry displays failure but nil/nil never grants a poll owner."
  (save-window-excursion
    (let ((hermes-instances '(("test" . "http://example.invalid")))
          fail buffer (polls 0))
      (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                ((symbol-function 'hermes-dashboard-transport-acquire)
                 (lambda (&rest _)
                   (when fail (error "cold acquisition unavailable"))
                   (make-hermes-dashboard-transport-client
                    :base-url "http://example.invalid")))
                ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                ((symbol-function 'hermes-system--api)
                 (lambda (&rest _) (hermes--promise-resolved '((lines . ("live"))))))
                ((symbol-function 'run-at-time)
                 (lambda (seconds repeat &rest _)
                   (when (and (equal seconds 5) (null repeat)) (cl-incf polls))
                   'test-timer))
                ((symbol-function 'cancel-timer) #'ignore))
        (unwind-protect
            (progn
              (hermes-system-logs)
              (setq buffer (current-buffer) fail t)
              (should (string-match-p "live" (buffer-string)))
              (should-error (call-interactively #'hermes-system-log-auto-refresh))
              (should (= polls 0))
              (should-not hermes-system--auto-refresh)
              (should-not hermes-system--timer)
              (should (string-match-p "Error: cold acquisition unavailable" (buffer-string)))
              (setq fail nil)
              (call-interactively #'hermes-system-log-auto-refresh)
              (should (= polls 1))
              (should hermes-system--auto-refresh)
              (should (string-match-p "live" (buffer-string))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-system-handoff-is-discoverable-without-mutation ()
  "Native management and system keys expose a truthful, inert handoff."
  (save-window-excursion
    (let ((hermes-instances '(("Named backend" . "http://named.invalid")))
          (native-comp-enable-subr-trampolines nil)
          (requests 0) (processes 0) buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) (cl-incf requests)))
                ((symbol-function 'hermes-dashboard-transport-acquire)
                 (lambda (&rest _) (cl-incf processes)))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (cl-incf processes)))
                ((symbol-function 'start-process)
                 (lambda (&rest _) (cl-incf processes)))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (cl-incf processes))))
        (unwind-protect
            (progn
              (dolist (map (list hermes-dash-sys-map hermes-config-mode-map
                                 hermes-plugins-mode-map hermes-messaging-mode-map
                                 hermes-system-mode-map))
                (should (eq (lookup-key map (kbd "H"))
                            'hermes-system-restart-handoff)))
              (with-temp-buffer
                (switch-to-buffer (current-buffer))
                (hermes-system-mode)
                (execute-kbd-macro (kbd "H"))
                (setq buffer (current-buffer))
                (should (eq major-mode 'hermes-system-handoff-mode))
                (should (string-match-p "Named backend" (buffer-string)))
                (should (string-match-p "http://named.invalid" (buffer-string)))
                (should (string-match-p "Runtime adoption: unverified" (buffer-string)))
                (should (string-match-p "shared multiplexer" (buffer-string)))
                (should (string-match-p "separately" (buffer-string)))
                (should (= requests 0))
                (should (= processes 0))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun hermes-system-test--http-response (text)
  "Decode a disposable HTTP response containing JSON TEXT."
  (let ((promise (hermes--promise-make)))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n" text)
      (hermes-dashboard-transport--settle-http-response
       promise nil (current-buffer) "https://named.invalid" nil))
    promise))

(ert-deftest hermes-system-handoff-public-config-status-logs-journey ()
  "Configuration, handoff, Status/Logs and refresh use only the captured GETs."
  (save-window-excursion
    (let* ((hermes-instances '(("Named" . "https://named.invalid")))
           (client (make-hermes-dashboard-transport-client
                    :base-url "https://named.invalid" :token "fixture"))
           (before (buffer-list)) requests (releases 0)
           (hermes-dashboard-transport-http-request-async-function
            (lambda (url &rest args)
              (push (cons (plist-get args :method) url) requests)
              (hermes-system-test--http-response
               (cond ((string-suffix-p "/api/status" url)
                      "{\"ok\":true,\"pid\":123}")
                     ((string-match-p "/api/logs" url)
                      "{\"lines\":[\"captured log\"]}")
                     (t "{}"))))))
      (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                ((symbol-function 'hermes-dashboard-transport-acquire)
                 (lambda (&rest _)
                   (should (equal hermes-dashboard-transport-url "https://named.invalid"))
                   client))
                ((symbol-function 'hermes-dashboard-transport-release)
                 (lambda (_) (cl-incf releases))))
        (unwind-protect
            (progn
              (hermes-config)
              (should (= (length requests) 3))
              (execute-kbd-macro (kbd "H"))
              (should (= (length requests) 3))
              (let ((handoff (current-buffer))
                    (keymap-popup-backend #'keymap-popup-backend-side-window))
                (execute-kbd-macro (kbd "? s"))
                (should (eq major-mode 'hermes-system-mode))
                (should (string-match-p "123" (buffer-string)))
                (should (string-match-p "Runtime adoption: unverified" (buffer-string)))
                (execute-kbd-macro (kbd "g"))
                (switch-to-buffer handoff)
                (execute-kbd-macro (kbd "l"))
                (should (string-match-p "captured log" (buffer-string)))
                (should (string-match-p "Runtime adoption: unverified" (buffer-string)))
                (execute-kbd-macro (kbd "g")))
              (should (= (length requests) 7))
              (should (= releases 5))
              (should (seq-every-p
                       (lambda (request)
                         (and (equal (car request) "GET")
                              (string-prefix-p "https://named.invalid/api/" (cdr request))))
                       requests)))
          (keymap-popup-dismiss)
          (dolist (buffer (seq-difference (buffer-list) before))
            (kill-buffer buffer)))))))

(ert-deftest hermes-system-handoff-fences-navigation-refresh-and-auth ()
  "Retired origin, handoff, target and transport never dispatch a late GET."
  (dolist (boundary '(current error cancel origin-retarget handoff-retarget
                     target-retarget origin-mode origin-kill handoff-file transport))
    (ert-info ((format "Boundary: %s" boundary))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (let* ((origin (current-buffer))
                 (hermes-instances '(("Named" . "https://named.invalid")))
                 (client (make-hermes-dashboard-transport-client
                          :base-url "https://named.invalid"))
                 (auth (hermes--promise-make))
                 (before (buffer-list)) (releases 0) requests
                 (hermes-dashboard-transport-http-request-async-function
                  (lambda (url &rest args)
                    (push (cons (plist-get args :method) url) requests)
                    (hermes-system-test--http-response "{\"pid\":123}"))))
            (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                      ((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest _) client))
                      ((symbol-function 'hermes-dashboard-transport-release)
                       (lambda (_) (cl-incf releases)))
                      ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                       (lambda () auth)))
              (unwind-protect
                  (progn
                    (hermes-system-restart-handoff)
                    (let ((handoff (current-buffer)))
                      (execute-kbd-macro (kbd "s"))
                      (let ((target (current-buffer)))
                        (should-not requests)
                        (pcase boundary
                          ('origin-retarget
                           (with-current-buffer origin
                             (setq-local hermes-instance '("Other" . "https://other.invalid"))))
                          ('handoff-retarget
                           (with-current-buffer handoff
                             (setq hermes-instance '("Other" . "https://other.invalid"))))
                          ('target-retarget
                           (setq hermes-instance '("Other" . "https://other.invalid")))
                          ('origin-mode
                           (with-current-buffer origin
                             (text-mode) (fundamental-mode)))
                          ('origin-kill (kill-buffer origin))
                          ('handoff-file
                           (with-current-buffer handoff
                             (set-visited-file-name
                              (expand-file-name "handoff-notes" temporary-file-directory) t)
                             (set-visited-file-name nil t)))
                          ('transport (hermes-dashboard-transport-stop client)))
                        (if (memq boundary '(error cancel))
                            (hermes--promise-reject auth (if (eq boundary 'error) "HTTP 503" "Login cancelled"))
                          (hermes--promise-resolve auth '(:base-url "https://named.invalid")))
                        (should (= releases 1))
                        (if (eq boundary 'current)
                            (progn
                              (should (equal requests '(("GET" . "https://named.invalid/api/status"))))
                              (should (string-match-p "123" (buffer-string))))
                          (should-not requests)
                          (should-not (string-match-p "123" (buffer-string))))
                        (when (memq boundary '(error cancel))
                          (should (string-match-p "Error:" (buffer-string))))
                        (when (memq boundary '(origin-retarget handoff-retarget target-retarget
                                              origin-mode origin-kill handoff-file))
                          (should-error (with-current-buffer target
                                          (call-interactively #'revert-buffer))
                                        :type 'user-error)
                          (unless (eq boundary 'target-retarget)
                            (should-error (with-current-buffer handoff
                                            (call-interactively #'hermes-system-handoff-logs))
                                          :type 'user-error))
                          (should-not requests)))))
                (dolist (buffer (seq-difference (buffer-list) before))
                  (with-current-buffer buffer (set-buffer-modified-p nil))
                  (kill-buffer buffer))))))))))

(ert-deftest hermes-system-handoff-prompt-cancel-and-retarget-are-inert ()
  "Instance selection cancellation and recursive retarget cannot open a handoff."
  (dolist (outcome '(cancel retarget))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (let ((hermes-instances '(("One" . "https://one.invalid")
                                  ("Two" . "https://two.invalid")))
              (before (buffer-list)) (requests 0) (entered 0))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (cl-incf entered)
                       (if (eq outcome 'cancel) (signal 'quit nil)
                         (setq-local hermes-instance '("Two" . "https://two.invalid"))
                         "One")))
                    ((symbol-function 'hermes-dashboard-transport-acquire)
                     (lambda (&rest _) (cl-incf requests))))
            (if (eq outcome 'cancel)
                (condition-case nil
                    (progn (hermes-system-restart-handoff) (ert-fail "No quit"))
                  (quit nil))
              (should-error (hermes-system-restart-handoff) :type 'user-error))
            (should (= entered 1))
            (should (= requests 0))
            (should-not (seq-difference (buffer-list) before))))))))

(ert-deftest hermes-system-handoff-local-never-spawns ()
  "Loopback observations attach remotely despite a configured spawn default."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((hermes-instances nil)
            (hermes-dashboard-transport-url "http://127.0.0.1:9999")
            (hermes-dashboard-transport-start-mode 'spawn)
            (starts 0) handoff)
        (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                  ((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _)
                     (unless (eq hermes-dashboard-transport-start-mode 'remote)
                       (cl-incf starts))
                     (error "Offline fixture"))))
          (unwind-protect
              (progn
                (hermes-system-restart-handoff)
                (setq handoff (current-buffer))
                (should (string-match-p "127.0.0.1:9999" (buffer-string)))
                (should-error (call-interactively #'hermes-system-handoff-status))
                (kill-buffer (current-buffer))
                (switch-to-buffer handoff)
                (should-error (call-interactively #'hermes-system-handoff-logs))
                (kill-buffer (current-buffer))
                (should (= starts 0)))
            (when (buffer-live-p handoff) (kill-buffer handoff))))))))

(ert-deftest hermes-system-handoff-real-http-is-read-only ()
  "Native handoff keys cross real HTTP with GETs only and no restart receipt inference."
  (save-window-excursion
    (let (requests)
      (hermes-test--with-http-server
       (lambda (peer request)
         (push (car (split-string request "\r\n")) requests)
         (hermes-test--http-reply peer 200
                                 (if (string-match-p "/api/logs" request)
                                     "{\"lines\":[\"literal fixture log\"]}"
                                   "{\"ok\":true,\"pid\":123}")))
       (lambda (url)
         (with-temp-buffer
           (switch-to-buffer (current-buffer))
           (let* ((hermes-instances (list (cons "Fixture" url)))
                  (client (make-hermes-dashboard-transport-client
                           :base-url url :token "synthetic"))
                  (hermes-dashboard-transport-http-request-function
                   #'hermes-dashboard-transport--default-http-request)
                  (hermes-dashboard-transport-http-request-async-function
                   #'hermes-dashboard-transport--default-http-request-async)
                  (url-proxy-services nil)
                  (before (buffer-list)) handoff)
             (cl-letf (((symbol-function 'hermes-browser--existing-client)
                        (lambda () client)))
               (unwind-protect
                   (progn
                     (hermes-system-restart-handoff)
                     (setq handoff (current-buffer))
                     (should-not requests)
                     (dolist (spec '(("s" . "123") ("l" . "literal fixture log")))
                       (switch-to-buffer handoff)
                       (execute-kbd-macro (kbd (car spec)))
                       (let ((view (current-buffer)))
                         (hermes-test--http-wait
                          (lambda () (with-current-buffer view
                                       (string-match-p (cdr spec) (buffer-string)))))
                         (should (string-match-p "Runtime adoption: unverified" (buffer-string)))))
                     (should (equal (reverse requests)
                                    '("GET /api/status HTTP/1.1"
                                      "GET /api/logs?file=agent&lines=100 HTTP/1.1"))))
                 (dolist (buffer (seq-difference (buffer-list) before))
                   (kill-buffer buffer)))))))))))

(ert-deftest hermes-system-handoff-late-response-preserves-successor ()
  "An issued observation cannot render after native file reassociation."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((hermes-instances '(("Named" . "https://named.invalid")))
             (client (make-hermes-dashboard-transport-client
                      :base-url "https://named.invalid" :token "synthetic"))
             (reply (hermes--promise-make))
             (before (buffer-list)) (requests 0)
             (hermes-dashboard-transport-http-request-async-function
              (lambda (&rest _) (cl-incf requests) reply)))
        (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () client)))
          (unwind-protect
              (progn
                (hermes-system-restart-handoff)
                (execute-kbd-macro (kbd "s"))
                (should (= requests 1))
                (set-visited-file-name (expand-file-name "notes" temporary-file-directory) t)
                (set-visited-file-name nil t)
                (let ((inhibit-read-only t)) (erase-buffer) (insert "Successor notes"))
                (hermes--promise-resolve reply '(:status 200 :body ((pid . 123))))
                (should (equal (buffer-string) "Successor notes"))
                (should-error (call-interactively #'revert-buffer))
                (should (= requests 1)))
            (dolist (buffer (seq-difference (buffer-list) before))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest hermes-system-handoff-native-instance-prompt-cancel ()
  "Cancel the real instance minibuffer without a handoff or acquisition."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (hermes-system-mode)
      (let ((hermes-instances '(("One" . "https://one.invalid")
                                ("Two" . "https://two.invalid")))
            (before (buffer-list)) (acquired 0) entered)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _) (cl-incf acquired))))
          (let ((noninteractive nil))
            (minibuffer-with-setup-hook
                (lambda () (setq entered (minibuffer-prompt)))
              (condition-case nil
                  (execute-kbd-macro (kbd "H C-g"))
                (quit nil))))
          (should (equal entered "Hermes instance: "))
          (should (= acquired 0))
          (should-not
           (seq-some (lambda (buffer)
                       (with-current-buffer buffer
                         (derived-mode-p 'hermes-system-handoff-mode)))
                     (seq-difference (buffer-list) before))))))))

(provide 'hermes-system-tests)
;;; hermes-system-tests.el ends here
