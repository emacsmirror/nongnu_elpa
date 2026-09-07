;;; hermes-plugins-tests.el --- Agent plugin journeys -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'hermes-plugins)

(ert-deftest hermes-plugins-inventory-preserves-unknowns ()
  (let ((rows (hermes-plugins--entries
               '(:plugins [(:name "novel" :runtime_status "future" :source "other")
                            (:name "off" :runtime_status "disabled")]
                 :orphan_dashboard_plugins [(:name "web-only")]))))
    (should (equal (mapcar #'car rows) '("novel" "off")))
    (should (equal (aref (cadar rows) 1) "future"))))

(ert-deftest hermes-plugins-install-confirmation-and-wire ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (setq hermes-plugins--snapshot '(:plugins []))
    (let (calls)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "vendor/plugin"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'hermes-plugins--request)
                 (lambda (&rest args) (push args calls))))
        (hermes-plugins-install)
        (should-not calls)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (hermes-plugins-install))
        (should (equal (car calls)
                       '("POST" "/api/dashboard/agent-plugins/install"
                         ((identifier . "vendor/plugin") (force . :false)
                          (enable . :false)) t)))))))

(ert-deftest hermes-plugins-context-uses-backend-options ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (setq hermes-plugins--snapshot
          '(:providers (:context_engine "old" :context_options
                        [(:name "future-engine")])) )
    (let (call)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_ options &rest _) (should (equal options '("compressor" "future-engine"))) "future-engine"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'hermes-plugins--request)
                 (lambda (&rest args) (setq call args))))
        (hermes-plugins-select-context-engine)
        (should (equal call '("PUT" "/api/dashboard/plugin-providers"
                              ((context_engine . "future-engine")) t)))))))

(ert-deftest hermes-plugins-public-mutations-read-back-before-unlocking ()
  (dolist (command '(hermes-plugins-enable hermes-plugins-disable
                     hermes-plugins-update hermes-plugins-remove))
    (with-temp-buffer
      (hermes-plugins-mode)
      (let* ((row '(:name "future" :path "/virtual/plugins/future" :source "git"
                    :runtime_status "inactive" :can_remove t :can_update_git t))
             (hub `(:plugins [,row] :providers (:context_engine "future")
                    :hermes_home "/virtual"))
             (write (hermes--promise-make))
             (readback (hermes--promise-make))
             (client (make-hermes-dashboard-transport-client :base-url "http://example.test"))
             calls (released 0))
        (hermes-plugins--render hub "Ready")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn client (lambda () (cl-incf released)))))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (should (eq (plist-get args :client) client))
                     (push (list method path) calls)
                     (cond ((equal path "/api/status")
                            (hermes--promise-resolved '(:hermes_home "/virtual")))
                           ((equal method "GET") readback)
                           (t write)))))
          (funcall command)
          (should hermes-plugins--busy)
          (should-error (hermes-plugins-refresh) :type 'user-error)
          (should (string-match-p "/future" (cadar calls)))
          (hermes--promise-resolve write '(:ok t))
          (should (= (length calls) 2))
          (should (equal (car calls) '("GET" "/api/dashboard/plugins/hub")))
          (should hermes-plugins--busy)
          (should-not hermes-plugins--snapshot)
          (should (= released 0))
          (hermes--promise-resolve readback hub)
          (should-not hermes-plugins--busy)
          (should (equal hermes-plugins--snapshot hub))
          (should (string-match-p "unverified" hermes-plugins--status))
          (should (= released 1)))))))

(ert-deftest hermes-plugins-stale-callbacks-and-secret-safe-failure ()
  (dolist (ending '(success failure semantic refresh instance reconnect endpoint mode kill))
    (dolist (reject '(nil t))
      (with-temp-buffer
        (hermes-plugins-mode)
        (setq hermes-instance (list :name "test" :url "http://example.test"))
        (let ((buffer (current-buffer))
              (client (make-hermes-dashboard-transport-client :base-url "http://example.test"))
              (pending (hermes--promise-make))
              (released 0) (calls 0))
          (cl-letf (((symbol-function 'hermes-browser--with-client)
                     (lambda (fn) (funcall fn client (lambda () (cl-incf released)))))
                    ((symbol-function 'hermes-plugins--api)
                     (lambda (&rest _) (cl-incf calls) pending)))
            (hermes-plugins--request "POST" "/test" nil t)
            (pcase ending
              ('refresh (hermes-browser--next-request-generation))
              ('instance (setf (plist-get hermes-instance :url) "http://else.test"))
              ('reconnect (cl-incf (hermes-dashboard-transport-client-generation client)))
              ('endpoint (setf (hermes-dashboard-transport-client-base-url client) "http://else.test"))
              ('mode (fundamental-mode))
              ('kill (kill-buffer buffer)))
            (if (or reject (eq ending 'failure))
                (hermes--promise-reject pending "secret-unlabelled-credential")
              (hermes--promise-resolve pending '(:ok :false :error "secret-unlabelled-credential")))
            (should (= calls 1))
            (should (= released 1))
            (when (buffer-live-p buffer)
              (should-not (string-match-p "secret-unlabelled" (buffer-string)))
              (when (derived-mode-p 'hermes-plugins-mode)
                (should-not (string-match-p "secret-unlabelled" hermes-plugins--status))
                (if (memq ending '(success failure semantic))
                    (progn (should-not hermes-plugins--busy)
                           (should (string-match-p "failed" hermes-plugins--status)))
                  (should (equal hermes-plugins--status "Updating; awaiting readback")))))))))))

(ert-deftest hermes-plugins-stale-install-does-not-read-back ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (let ((pending (hermes--promise-make)) (calls 0))
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-plugins--api)
                 (lambda (&rest _) (cl-incf calls) pending)))
        (hermes-plugins--request "POST" "/test" nil t)
        (hermes-browser--next-request-generation)
        (hermes--promise-resolve pending '(:ok t))
        (should (= calls 1))))))

(ert-deftest hermes-plugins-prompt-replacement-and-permissions ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (hermes-plugins--render
     '(:hermes_home "/virtual" :plugins
       [(:name "locked" :path "/virtual/plugins/locked" :source "user"
         :can_remove :false)]) "Ready")
    (goto-char (point-min))
    (let (called)
      (cl-letf (((symbol-function 'hermes-plugins--request)
                 (lambda (&rest _) (setq called t)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (hermes-plugins-mode) t)))
        (should-error (hermes-plugins-remove) :type 'user-error)
        (hermes-plugins-enable)
        (should-not called)))))

(ert-deftest hermes-plugins-keymap-and-config-instance ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (setq hermes-instance '(:name "other" :url "http://other.test"))
    (let ((owner hermes-instance) called)
      (cl-letf (((symbol-function 'hermes-config)
                 (lambda () (setq called hermes-instance))))
        (call-interactively (keymap-lookup hermes-plugins-mode-map "c")))
      (should (eq owner called)))
    (dolist (key '("g" "i" "e" "d" "u" "D" "x" "?" "q"))
      (should (commandp (keymap-lookup hermes-plugins-mode-map key))))))

(ert-deftest hermes-plugins-open-and-install-use-owning-http-client ()
  (let* ((instance '(:name "other" :url "http://other.test"))
         (client (make-hermes-dashboard-transport-client :base-url "http://other.test"))
         (hub '(:plugins [(:name "future" :runtime_status "enabled")]
                :providers (:context_engine "future") :hermes_home nil))
         calls browser)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq browser buffer)))
                  ((symbol-function 'hermes-browser--with-client)
                   (lambda (fn)
                     (should (equal hermes-instance instance))
                     (funcall fn client #'ignore)))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (should (eq client (plist-get args :client)))
                     (push (list method path (plist-get args :body)) calls)
                     (hermes--promise-resolved
                      (if (equal method "POST") '(:ok t :enabled :false) hub))))
                  ((symbol-function 'read-string) (lambda (&rest _) "vendor/future"))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (hermes-list-plugins)
          (with-current-buffer browser
            (should (equal hermes-plugins--snapshot hub))
            (hermes-plugins-install)
            (should-not hermes-plugins--busy)
            ;; The receipt's enabled:false is an option echo, not actual state.
            (should (equal (aref (cadar tabulated-list-entries) 1) "enabled")))
          (should (equal (mapcar #'car (reverse calls)) '("GET" "GET" "POST" "GET" "GET")))
          (should (equal (json-serialize (nth 2 (nth 2 calls)) :false-object :false)
                         "{\"identifier\":\"vendor/future\",\"force\":false,\"enable\":false}")))
      (when (buffer-live-p browser) (kill-buffer browser)))))

(ert-deftest hermes-plugins-refresh-preserves-point-and-requires-fresh-state ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (let* ((hub '(:plugins [(:name "first") (:name "second")]))
           (pending (hermes--promise-make)))
      (hermes-plugins--render hub "Ready")
      (goto-char (point-min))
      (forward-line 1)
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-plugins--api) (lambda (&rest _) pending)))
        (hermes-plugins-refresh)
        (should-error (hermes-plugins-enable) :type 'user-error)
        (should-error (hermes-plugins-install) :type 'user-error)
        (hermes--promise-resolve pending hub)
        (should (equal (tabulated-list-get-id) "second"))))))

(ert-deftest hermes-plugins-ambiguous-server-name-refuses-mutation ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (hermes-plugins--render '(:plugins [(:name "same") (:name "same")]) "Ready")
    (goto-char (point-min))
    (should-error (hermes-plugins-enable) :type 'user-error)))

(ert-deftest hermes-plugins-install-hints-never-display-raw-warnings ()
  (let ((note (hermes-plugins--setup-note
               '(:missing_env ["PLUGIN_KEY" "not an env key: secret-value"]
                 :warnings ["clone https://credential@example.test"]))))
    (should (string-match-p "PLUGIN_KEY" note))
    (should (string-match-p "warnings" note))
    (should-not (string-match-p "secret-value\\|credential" note))))

(ert-deftest hermes-plugins-filesystem-target-is-not-manifest-name ()
  (dolist (command '(hermes-plugins-update hermes-plugins-remove))
    (with-temp-buffer
      (hermes-plugins-mode)
      (hermes-plugins--render
       '(:hermes_home "/virtual" :plugins
         [(:name "manifest" :path "/virtual/plugins/directory" :source "git"
           :can_remove t :can_update_git t)]) "Ready")
      (goto-char (point-min))
      (let (call)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'hermes-plugins--request)
                   (lambda (&rest args) (setq call args))))
          (funcall command)
          (should (string-match-p "/directory\\(?:/update\\)?\\'" (cadr call))))))))

(ert-deftest hermes-plugins-filesystem-target-rejects-unproven-and-collisions ()
  (dolist (hub
           '((:plugins [(:name "leaf" :path "/virtual/plugins/leaf")])
             (:hermes_home "/virtual" :plugins
              [(:name "nested-name" :path "/virtual/plugins/category/leaf")
               (:name "other-name" :path "/virtual/plugins/nested-name")])
             (:hermes_home "/virtual" :plugins
              [(:name "manifest" :path "/virtual/plugins/directory")
               (:name "directory" :path "/virtual/plugins/other")])
             (:hermes_home "/virtual" :plugins
              [(:name "outside" :path "/else/plugins/outside")])
             (:hermes_home "/virtual" :plugins
              [(:name "traversal" :path "/virtual/plugins/../other")])
             (:hermes_home "/virtual" :plugins
              [(:name "duplicate" :path "/virtual/plugins/same")
               (:name "other" :path "/virtual/plugins/same")])))
    (with-temp-buffer
      (hermes-plugins-mode)
      (let* ((copy (copy-tree hub t))
             (rows (hermes-transport--get copy 'plugins)))
        (setf (plist-get (aref rows 0) :source) "git"
              (plist-get (aref rows 0) :can_remove) t
              (plist-get (aref rows 0) :can_update_git) t)
        (hermes-plugins--render copy "Ready"))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (ert-fail "Must reject before prompting"))))
        (should-error (hermes-plugins-update) :type 'user-error)
        (should-error (hermes-plugins-remove) :type 'user-error)))))

(ert-deftest hermes-plugins-context-builtin-empty-and-deduplicated ()
  (dolist (options '([] [(:name "compressor") (:name "compressor")]))
    (with-temp-buffer
      (hermes-plugins-mode)
      (setq hermes-plugins--snapshot `(:providers (:context_options ,options)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_ choices &rest _)
                   (should (equal choices '("compressor"))) "compressor"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (hermes-plugins-select-context-engine)))))

(ert-deftest hermes-plugins-install-warns-existing-enablement-may-persist ()
  (with-temp-buffer
    (hermes-plugins-mode)
    (setq hermes-plugins--snapshot '(:plugins []))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "vendor/plugin"))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (should (string-match-p "without automatically enabling" prompt))
                 (should (string-match-p "existing enablement may persist" prompt))
                 nil)))
      (hermes-plugins-install))))

(ert-deftest hermes-plugins-process-home-read-is-owned-and-fails-closed ()
  (dolist (ending '(available withheld rejected stale))
    (with-temp-buffer
      (hermes-plugins-mode)
      (let* ((row '(:name "manifest" :path "/virtual/plugins/directory"
                    :source "git" :can_remove t))
             (hub `(:plugins [,row]))
             (status (hermes--promise-make))
             (client (make-hermes-dashboard-transport-client :base-url "http://example.test"))
             calls)
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn client #'ignore)))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (should (equal method "GET"))
                     (should (eq (plist-get args :client) client))
                     (push path calls)
                     (if (equal path "/api/status") status
                       (hermes--promise-resolved hub)))))
          (hermes-plugins-refresh)
          (should (equal (reverse calls) '("/api/dashboard/plugins/hub" "/api/status")))
          (should-not hermes-plugins--snapshot)
          (when (eq ending 'stale)
            (hermes-browser--next-request-generation))
          (if (eq ending 'rejected)
              (hermes--promise-reject status "private server details")
            (hermes--promise-resolve status
                                    (if (eq ending 'withheld) '(:auth_required t)
                                      '(:hermes_home "/virtual"))))
          (pcase ending
            ('stale (should-not hermes-plugins--snapshot))
            ('available (should (equal (hermes-plugins--directory-target row) "directory")))
            (_ (should (equal (hermes-transport--get hermes-plugins--snapshot 'plugins)
                              (vector row)))
               (should-error (hermes-plugins--directory-target row) :type 'user-error))))))))

(ert-deftest hermes-plugins-process-home-never-normalized-or-local ()
  (dolist (home '(nil "relative" "/virtual/.." "/virtual/./other" "/virtual//other"
                     "/virtual/" "/virtual\\other"))
    (let* ((hermes-plugins--snapshot `(:hermes_home ,home))
           (row `(:name "manifest" :path ,(concat home "/plugins/directory") :source "git")))
      (should-error (hermes-plugins--directory-target row) :type 'user-error))))

(ert-deftest hermes-plugins-enable-disable-reject-backend-name-rewriting ()
  (dolist (command '(hermes-plugins-enable hermes-plugins-disable))
    (dolist (name '("/victim/" "/victim" "victim/" "" "bad..name" "bad\\name"))
      (with-temp-buffer
        (hermes-plugins-mode)
        (hermes-plugins--render `(:plugins [(:name ,name) (:name "victim")]) "Ready")
        (goto-char (point-min))
        (let ((prompts 0) (writes 0))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (cl-incf prompts) t))
                    ((symbol-function 'hermes-plugins--request)
                     (lambda (&rest _) (cl-incf writes))))
            (should-error (funcall command) :type 'user-error)
            (should (zerop prompts))
            (should (zerop writes))))))))

(ert-deftest hermes-plugins-directory-confirmation-identifies-wire-and-owner ()
  (dolist (command '(hermes-plugins-update hermes-plugins-remove))
    (dolist (retire '(nil t))
      (with-temp-buffer
        (hermes-plugins-mode)
        (setq hermes-instance (list :name "chosen-server" :url "http://example.test"))
        (hermes-plugins--render
         '(:hermes_home "/virtual" :plugins
           [(:name "/manifest/" :path "/virtual/plugins/directory" :source "git"
             :can_remove t :can_update_git t)]) "Ready")
        (goto-char (point-min))
        (let (prompt call)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (text)
                       (setq prompt text)
                       (when retire (setf (plist-get hermes-instance :url) "http://other.test"))
                       t))
                    ((symbol-function 'hermes-plugins--request)
                     (lambda (&rest args) (setq call args))))
            (funcall command)
            (dolist (text '("/manifest/" "/virtual/plugins/directory" "chosen-server"
                            "resolves filesystem links"))
              (should (string-match-p (regexp-quote text) prompt)))
            (if retire (should-not call)
              (should (equal (cadr call)
                             (concat "/api/dashboard/agent-plugins/directory"
                                     (when (eq command 'hermes-plugins-update) "/update")))))))))))

(provide 'hermes-plugins-tests)
;;; hermes-plugins-tests.el ends here
