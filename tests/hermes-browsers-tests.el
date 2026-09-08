;;; hermes-browsers-tests.el --- browsers tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-work-log-structured-mapping ()
  "Only exact structured parent delegate results authorize log reads."
  (let* ((result '((subagent_ids . ["one" "two"])
                   (live_transcripts . ["/remote/one.log" "/remote/two.log"])))
         (event (list :event "tool.complete" :name "delegate_task" :result result)))
    (should (equal (hermes-work--event-log-path event "two") "/remote/two.log"))
    (should-not (hermes-work--event-log-path event "other"))
    (should-not (hermes-work--event-log-path
                 (plist-put (copy-sequence event) :subagent-id "child") "one"))
    (should (equal (hermes-work--event-log-path
                    (list :event "tool.complete" :name "delegate_task"
                          :result-text (json-serialize result)) "one") "/remote/one.log"))
    (dolist (bad '(((subagent_ids "one" "one") (live_transcripts "/a" "/b"))
                   ((subagent_ids "one" "two") (live_transcripts "/a"))
                   ((subagent_ids . "one") (live_transcripts "/a"))))
      (should-not (hermes-work--event-log-path
                   (plist-put (copy-sequence event) :result bad) "one")))
    (should-not (hermes-work--event-log-path
                 '(:event "tool.complete" :name "terminal"
                   :result "{\"subagent_ids\":[\"one\"],\"live_transcripts\":[\"/a\"]}") "one"))))

(defun hermes-test--log-response (text)
  "Return a managed-file fixture for UTF-8 TEXT."
  (let ((bytes (encode-coding-string text 'utf-8)))
    (list :size (string-bytes bytes)
          :data_url (concat "data:application/octet-stream;base64,"
                            (base64-encode-string bytes t)))))

(ert-deftest hermes-work-log-decode-and-diff ()
  "Decode Unicode, reject corrupt/oversize payloads and reuse real diff faces."
  (let ((text "Assistant: λ\n--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+new\n"))
    (should (equal (hermes-work-log--decode (hermes-test--log-response text)) text))
    (should (text-property-not-all
             0 (length text) 'face nil (hermes-kanban--render-log-content text))))
  (should (equal (hermes-work-log--decode (hermes-test--log-response "")) ""))
  (dolist (bad '((:size 3 :data_url "data:text/plain;base64,eA==")
                 (:size 3000000 :data_url "data:text/plain;base64,")
                 (:size 0 :data_url "https://other.example/log")
                 (:size 1 :data_url "data:text/plain;base64,!!!")))
    (should-error (hermes-work-log--decode bad))))

(ert-deftest hermes-work-log-refresh-owner-and-point ()
  "Fetch through the captured client once, preserve point, and retain failures."
  (let* ((client (list 'exact-client))
         (owner (list :client client :current-p (lambda (_) t)))
         (promise (hermes--promise-make)) calls)
    (with-temp-buffer
      (hermes-work-log-mode)
      (setq hermes-work-log--binding (list :owner owner :id "one" :path "/remote/log"))
      (hermes-work-log--render "old snapshot\n")
      (goto-char 5)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest args) (push args calls) promise)))
        (hermes-work-log-refresh)
        (should-error (hermes-work-log-refresh) :type 'user-error)
        (should (= (length calls) 1))
        (should (equal (car calls)
                       (list "GET" "/api/files/read" :client client
                             :query '((path . "/remote/log")) :timeout 30)))
        (hermes--promise-resolve promise (hermes-test--log-response "new snapshot\nmore\n"))
        (should-not hermes-work-log--request)
        (should (= (point) 5))
        (should buffer-read-only)
        (should (string-match-p "may be truncated" header-line-format))
        (setq promise (hermes--promise-make))
        (hermes-work-log-refresh)
        (hermes--promise-reject promise "timeout")
        (should-not hermes-work-log--request)
        (should (equal (buffer-string) "new snapshot\nmore\n"))
        (should (string-match-p "Failed" header-line-format))
        (should (equal (get-text-property 0 'help-echo header-line-format) "timeout"))))))

(ert-deftest hermes-work-log-stale-completions ()
  "Owner replacement, view replacement and killed buffers ignore late replies."
  (dolist (change '(owner binding mode kill))
    (let* ((current t)
           (owner (list :current-p (lambda (_) current)))
           (promise (hermes--promise-make))
           (buffer (generate-new-buffer " *worker-test*")))
      (unwind-protect
          (with-current-buffer buffer
            (hermes-work-log-mode)
            (setq hermes-work-log--binding (list :owner owner :path "/log"))
            (hermes-work-log--render "retained")
            (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                       (lambda (&rest _) promise)))
              (hermes-work-log-refresh))
            (pcase change
              ('owner (setq current nil))
              ('binding (setq hermes-work-log--binding (copy-sequence hermes-work-log--binding)))
              ('mode (fundamental-mode))
              ('kill (kill-buffer buffer)))
            (hermes--promise-resolve promise (hermes-test--log-response "wrong"))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (should (equal (buffer-string) "retained")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-work-log-keys-and-absence ()
  "RET opens logs, metadata remains separate, and absence never fetches."
  (should (eq (lookup-key hermes-work-mode-map (kbd "RET")) 'hermes-work-log))
  (should (eq (lookup-key hermes-work-mode-map (kbd "d")) 'hermes-work-details))
  (should (eq (lookup-key hermes-work-log-mode-map (kbd "g")) 'hermes-work-log-refresh))
  (with-temp-buffer
    (hermes-work-mode)
    (setq hermes-work--owner (list :current-p (lambda (_) t)))
    (let ((kind 'process) details)
      (setq tabulated-list-format [("Worker" 20 t)]
            tabulated-list-entries '(((process . "one") ["one"])))
      (tabulated-list-print)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'hermes-work--observations)
                 (lambda (_) (list (list :key (cons kind "one") :kind kind :id "one"))))
                ((symbol-function 'hermes-work--log-path) (lambda (&rest _) nil))
                ((symbol-function 'hermes-work-details) (lambda () (setq details t)))
                ((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) (ert-fail "Unexpected network access"))))
        (hermes-work-log)
        (should details)
        (setq kind 'delegate
              tabulated-list-entries '(((delegate . "one") ["one"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-work-log) :type 'user-error)))))

(ert-deftest hermes-work-log-ewoc-open-reuse-and-disappearance ()
  "Bind from real EWOC metadata and keep an open log after the worker vanishes."
  (let ((chat (generate-new-buffer " *parent-log-test*"))
        (view (generate-new-buffer " *work-log-test*"))
        (promise (hermes--promise-make)) log calls)
    (unwind-protect
        (with-current-buffer chat
          (setq-local hermes-chat--ewoc (ewoc-create #'ignore))
          (ewoc-enter-last hermes-chat--ewoc
                           '(:role tool :metadata
                             (:event (:event "tool.complete" :name "delegate_task"
                                      :result ((subagent_ids "one")
                                               (live_transcripts "/remote/log"))))))
          (let ((owner (list :buffer chat :current-p (lambda (_) t)
                             :delegates '(:coverage current :rows
                                          ((:key (delegate . "one") :id "one" :kind delegate))))))
            (should (equal (hermes-work--log-path owner "one") "/remote/log"))
            (with-current-buffer view
              (hermes-work-mode)
              (setq hermes-work--owner owner
                    tabulated-list-format [("Worker" 20 t)]
                    tabulated-list-entries '(((delegate . "one") ["one"])))
              (tabulated-list-print)
              (goto-char (point-min)))
            (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq log buffer)))
                      ((symbol-function 'hermes-dashboard-transport-api-request-async)
                       (lambda (&rest args) (push args calls) promise)))
              (with-current-buffer view (hermes-work-log) (hermes-work-log))
              (should (= (length calls) 1))
              (should (equal (plist-get (cddar calls) :query) '((path . "/remote/log"))))
              (setf (plist-get owner :delegates) nil)
              ;; The selected worker need not remain in active status or EWOC.
              (ewoc-delete hermes-chat--ewoc (ewoc-nth hermes-chat--ewoc 0))
              (hermes--promise-resolve promise (hermes-test--log-response ""))
              (with-current-buffer log
                (should visual-line-mode)
                (should-not truncate-lines)
                (should (string-match-p "Empty snapshot" header-line-format))
                (should (string-match-p "No log content yet" (buffer-string)))
                (setq promise (hermes--promise-make))
                (hermes-work-log-refresh)
                (hermes--promise-resolve promise (hermes-test--log-response "later output"))
                (should (equal (buffer-string) "later output"))))))
      (kill-buffer chat)
      (kill-buffer view)
      (when (buffer-live-p log) (kill-buffer log)))))

(ert-deftest hermes-work-log-constructor-hook-invalidates ()
  "Mode hooks cannot redirect log requests or display invalidated views."
  (dolist (action '(kill retarget))
    (let ((owner (list :current-p (lambda (_) t))) created)
      (unwind-protect
          (let ((hermes-work-log-mode-hook
                 (list (lambda ()
                         (setq created (current-buffer))
                         (if (eq action 'kill) (kill-buffer) (fundamental-mode))))))
            (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                       (lambda (&rest _) (ert-fail "Hook redirected a request")))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (&rest _) (ert-fail "Displayed invalid log owner"))))
              (hermes-work-log--open owner "hook-test" "/remote/log")))
        (when (buffer-live-p created) (kill-buffer created))))))

(defvar hermes-browser-test--fetch-function nil)

(hermes-define-list-browser browseridentity
  :title "Hermes Browser Identity"
  :buffer "*Hermes Browser Identity*"
  :columns [("Name" 20 t)]
  :fetch (lambda (_client) (funcall hermes-browser-test--fetch-function))
  :rows (lambda (result)
          (mapcar (lambda (name) (list name (vector name))) result)))

(ert-deftest hermes-browser-refresh-renamed-owner ()
  "Delayed refresh writes only its renamed owner, never a name replacement."
  (dolist (rename-before '(nil t))
    (let ((owner (generate-new-buffer "*Hermes Browser Identity*"))
          replacement callback)
      (unwind-protect
          (with-current-buffer owner
            (hermes-browseridentity-mode)
            (when rename-before (rename-buffer "*Renamed browser*" t))
            (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                       (lambda (_fetch done &optional _error) (setq callback done))))
              (hermes-browseridentity--revert))
            (unless rename-before (rename-buffer "*Renamed browser*" t))
            (setq replacement (get-buffer-create "*Hermes Browser Identity*"))
            (funcall callback '("fresh"))
            (should (equal (caar tabulated-list-entries) "fresh"))
            (with-current-buffer replacement (should (= (buffer-size) 0)))
            (fundamental-mode)
            (funcall callback '("stale"))
            (should-not (string-match-p "stale" (buffer-string))))
        (kill-buffer owner)
        (when replacement (kill-buffer replacement))))))

(ert-deftest hermes-browser-retarget-removes-actionable-rows ()
  "Every retained list rejects A's rows as soon as it is retargeted to B."
  (dolist (case '((hermes-profiles-mode hermes-profiles-delete)
                  (hermes-sessions-mode hermes-sessions-delete)
                  (hermes-cron-mode hermes-cron-remove)
                  (hermes-rollback-mode hermes-rollback-restore)
                  (hermes-subagents-mode hermes-subagents-interrupt)
                  (hermes-inventory-mode hermes-inventory-disable)
                  (hermes-mcp-mode hermes-mcp-test)
                  (hermes-messaging-mode hermes-messaging-toggle)
                  (hermes-provider-accounts-mode
                   hermes-onboarding-provider-account-act)
                  (hermes-kanban-boards-mode hermes-kanban-archive-board)
                  (hermes-kanban-mode hermes-kanban-delete)
                  (hermes-kanban-diagnostics-mode hermes-kanban-show)))
    (with-temp-buffer
      (funcall (car case))
      (hermes-browser--own-instance '("A" . "https://a.example.test"))
      (setq tabulated-list-format [("Name" 20 t)]
            tabulated-list-entries '(("same-name" ["same-name"])))
      (tabulated-list-print)
      (goto-char (point-min))
      (should (equal (tabulated-list-get-id) "same-name"))
      (hermes-browser--own-instance '("B" . "https://b.example.test"))
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (&rest _) (ert-fail "Stale row acquired B client")))
                ((symbol-function 'hermes-kanban--api)
                 (lambda (&rest _) (ert-fail "Stale row reached B API")))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (should-error (funcall (cadr case)) :type 'user-error))
      ;; Sorting/resizing must not resurrect A's entries either.
      (should-not tabulated-list-entries)
      (tabulated-list-print)
      (should-not (tabulated-list-get-id)))))

(ert-deftest hermes-browser-retarget-pending-and-failed-fetch ()
  "A delayed or rejected B fetch leaves no A rows, even after late A replies."
  (let* ((instance '("A" . "https://a.example.test"))
         (pending (hermes--promise-make))
         (hermes-browser-test--fetch-function
          (lambda () (hermes--promise-resolved '("same-name")))))
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client)))
      (unwind-protect
          (progn
            (hermes-list-browseridentity)
            (setq hermes-browser-test--fetch-function (lambda () pending))
            (hermes-list-browseridentity)
            (let ((old pending))
              (setq instance '("B" . "https://b.example.test")
                    pending (hermes--promise-make))
              (hermes-list-browseridentity)
              (with-current-buffer "*Hermes Browser Identity*"
                (should-not tabulated-list-entries))
              (hermes--promise-resolve old '("late-A"))
              (hermes--promise-reject pending "B unavailable")
              (with-current-buffer "*Hermes Browser Identity*"
                (should-not tabulated-list-entries))
              (setq hermes-browser-test--fetch-function
                    (lambda () (hermes--promise-resolved '("same-name"))))
              (hermes-list-browseridentity)
              (with-current-buffer "*Hermes Browser Identity*"
                (should (equal (caar tabulated-list-entries) "same-name"))
                (should (equal hermes-instance instance)))))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-retarget-clears-registered-snapshots ()
  "Only a changed instance clears each mode's cached result data."
  (dolist (mode '(hermes-sessions-mode hermes-mcp-mode hermes-messaging-mode
                 hermes-provider-accounts-mode hermes-cron-mode
                 hermes-kanban-mode hermes-memory-status-mode))
    (with-temp-buffer
      (funcall mode)
      (hermes-browser--own-instance '("A" . "https://a.example.test"))
      (should hermes-browser--snapshot-variables)
      (dolist (variable hermes-browser--snapshot-variables)
        (set variable 'old-snapshot))
      (hermes-browser--own-instance '("A" . "https://a.example.test"))
      (dolist (variable hermes-browser--snapshot-variables)
        (should (eq (symbol-value variable) 'old-snapshot)))
      (hermes-browser--own-instance '("B" . "https://b.example.test"))
      (dolist (variable hermes-browser--snapshot-variables)
        (should-not (symbol-value variable))))))

(ert-deftest hermes-browser-retarget-mcp-same-name-authority ()
  "MCP actions wait for B rows and do not inherit A's same-name test cache."
  (let* ((instance '("A" . "https://a.example.test"))
         (pending (hermes--promise-make))
         (response (hermes--promise-resolved
                    '((servers . (((name . "same") (enabled . t)))))))
         mutations)
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'hermes-mcp--api)
               (lambda (method path &optional body _query &rest _)
                 (if (equal method "GET") response
                   (push (list hermes-instance method path body) mutations)
                   (hermes--promise-resolved nil)))))
      (unwind-protect
          (progn
            (hermes-list-mcp)
            (with-current-buffer hermes-mcp-buffer-name
              (puthash "same" '((ok . t) (tool_count . 99))
                       hermes-mcp--test-results))
            (setq instance '("B" . "https://b.example.test") response pending)
            (hermes-list-mcp)
            (with-current-buffer hermes-mcp-buffer-name
              (should-error (hermes-mcp-toggle) :type 'user-error)
              (should-error (hermes-mcp-test) :type 'user-error))
            (hermes--promise-reject pending "B unavailable")
            (with-current-buffer hermes-mcp-buffer-name
              (should-error (hermes-mcp-toggle) :type 'user-error))
            (should-not mutations)
            (setq response (hermes--promise-resolved
                            '((servers . (((name . "same")
                                           (enabled . :false)))))))
            (hermes-list-mcp)
            (with-current-buffer hermes-mcp-buffer-name
              (goto-char (point-min))
              (should-not (gethash "same" hermes-mcp--test-results))
              (hermes-mcp-toggle))
            (should (equal mutations
                           (list (list instance "PUT" "/servers/same/enabled"
                                       '((enabled . t)))))))
        (when (get-buffer hermes-mcp-buffer-name)
          (kill-buffer hermes-mcp-buffer-name))))))

(ert-deftest hermes-browser-retarget-kanban-pending-context ()
  "A pending board switch drops A's tail and uses B's requested board context."
  (let ((instance '("B" . "https://b.example.test"))
        (pending (hermes--promise-make))
        tail queries)
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
              ((symbol-function 'hermes-kanban--api)
               (lambda (_method _path &optional _body query &rest _)
                 (push query queries)
                 pending)))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create "*Hermes Kanban*")
              (hermes-kanban-mode)
              (hermes-browser--own-instance '("A" . "https://a.example.test"))
              (setq hermes-kanban--slug "old-board"
                    hermes-kanban--name "Old board"
                    tail (hermes-kanban--events-tail-create
                          :buffer (current-buffer) :slug "old-board"
                          :instance hermes-instance)
                    hermes-kanban--events-tail tail))
            (hermes-kanban--render-board "new-board" "New board" t)
            (with-current-buffer "*Hermes Kanban*"
              (should-not (hermes-kanban--events-tail-active tail))
              (should (eq hermes-kanban--events-tail tail))
              (should (equal hermes-kanban--slug "new-board"))
              (should (equal hermes-kanban--name "New board"))
              (hermes-kanban--revert))
            (should (equal queries '(((board . "new-board"))
                                     ((board . "new-board")))))
            (hermes--promise-reject pending "B unavailable")
            ;; Returning to A after B failed must restart the suspended tail,
            ;; even though its stored instance and slug already match A.
            (let (connected)
              (cl-letf (((symbol-function 'hermes-kanban--events-connect)
                         (lambda (new-tail) (setq connected new-tail))))
                (hermes-kanban--display-board
                 nil "old-board" "Old board" t
                 '("A" . "https://a.example.test")))
              (should connected)
              (should-not (eq connected tail))
              (should (hermes-kanban--events-tail-active connected)))
            (setq instance '("A" . "https://a.example.test")
                  pending (hermes--promise-make))
            (hermes-kanban--render-board "another-board" "Another" t)
            (with-current-buffer "*Hermes Kanban*"
              ;; A different board is a replacement even on the same instance.
              (should (equal hermes-kanban--slug "another-board"))
              (should-not tabulated-list-entries)
              (should-not (hermes-kanban--events-tail-active
                           hermes-kanban--events-tail))))
        (when (get-buffer "*Hermes Kanban*")
          (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-browser-command-pins-resolved-instance ()
  "A browser command resolves once and uses that instance for its client."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test")))
        (hermes-browser-test--fetch-function
         (lambda () (hermes--promise-resolved '("remote item"))))
        started-url)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _)
                 (setq started-url hermes-dashboard-transport-url)
                 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-release) #'ignore))
      (unwind-protect
          (progn
            (hermes-list-browseridentity)
            (with-current-buffer "*Hermes Browser Identity*"
              (should (equal hermes-instance instance))
              (should (string-match-p
                       "remote" (hermes-browser--instance-header-line)))
              (should (equal started-url (hermes-instance-url instance)))))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-existing-client-matches-buffer-instance ()
  "Passive client reuse is limited to the current buffer's instance."
  (let ((local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        (local-client (hermes-test--dashboard-client))
        (remote-client (hermes-test--dashboard-client))
        buffers)
    (unwind-protect
        (progn
          (dolist (pair (list (cons local local-client)
                              (cons remote remote-client)))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance (car pair)
                      hermes-chat--dashboard-client (cdr pair)))))
          (with-temp-buffer
            (setq hermes-instance remote)
            (should (eq (hermes-browser--existing-client) remote-client))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest hermes-browser-existing-client-does-not-prompt-without-context ()
  "Passive client lookup returns nil when several instances are ambiguous."
  (let ((hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test")))
        (hermes-instance nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected instance prompt"))))
      (should-not (hermes-browser--existing-client)))))

(ert-deftest hermes-browser-with-client-registers-created-owner-once ()
  "Browser-created clients acquire and release the shared registry exactly once."
  (let ((instance '("local" . "http://127.0.0.1:9119"))
        (acquired 0) released seen)
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
              ((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _) (cl-incf acquired) 'registry-client))
              ((symbol-function 'hermes-dashboard-transport-release)
               (lambda (client) (setq released client))))
      (hermes-browser--with-client
       (lambda (client done)
         (setq seen client)
         (funcall done)
         (funcall done))))
    (should (and (eq seen 'registry-client) (= acquired 1)
                 (eq released 'registry-client)))
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'borrowed-client))
              ((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _) (ert-fail "borrowed client was acquired")))
              ((symbol-function 'hermes-dashboard-transport-release)
               (lambda (&rest _) (ert-fail "borrowed client was released"))))
      (hermes-browser--with-client
       (lambda (client done) (setq seen client) (funcall done))))
    (should (eq seen 'borrowed-client))))

(ert-deftest hermes-browser-semantic-faces-are-customizable ()
  "Every semantic browser role has its own customizable face."
  (dolist (face '(hermes-browser-name hermes-browser-title
                  hermes-browser-description hermes-browser-identifier
                  hermes-browser-profile hermes-browser-count
                  hermes-browser-message-count hermes-browser-tool-count
                  hermes-browser-total hermes-browser-priority
                  hermes-browser-assignee hermes-browser-model
                  hermes-browser-provider hermes-browser-type
                  hermes-browser-timestamp hermes-browser-schedule
                  hermes-browser-delivery hermes-browser-prompt
                  hermes-browser-command hermes-browser-category
                  hermes-browser-version hermes-browser-source
                  hermes-browser-message hermes-browser-default
                  hermes-browser-reasoning hermes-browser-diagnostic
                  hermes-browser-uptime hermes-browser-goal
                  hermes-browser-enabled hermes-browser-state
                  hermes-browser-status hermes-browser-severity
                  hermes-browser-active hermes-browser-success
                  hermes-browser-pending hermes-browser-error
                  hermes-browser-muted))
    (should (facep face))))

(ert-deftest hermes-browser-semantic-face-cell-preserves-visible-text ()
  "Semantic cells add only the requested face to their visible text."
  (let ((cell (hermes-browser--face-cell 42 'hermes-browser-count)))
    (should (equal cell "42"))
    (should (eq (get-text-property 0 'face cell) 'hermes-browser-count)))
  (should (equal (hermes-browser--face-cell "" 'hermes-browser-count) ""))
  (should-not (get-text-property
               0 'face (hermes-browser--face-cell "plain" nil))))

(ert-deftest hermes-browser-semantic-status-face-classifies-known-states ()
  "Known backend states map to the shared semantic face vocabulary."
  (should (eq (hermes-browser--status-face "running")
              'hermes-browser-active))
  (should (eq (hermes-browser--status-face "ready")
              'hermes-browser-success))
  (should (eq (hermes-browser--status-face "on")
              'hermes-browser-success))
  (should (eq (hermes-browser--status-face "triage")
              'hermes-browser-pending))
  (should (eq (hermes-browser--status-face "connecting")
              'hermes-browser-pending))
  (should (eq (hermes-browser--status-face "blocked")
              'hermes-browser-error))
  (should (eq (hermes-browser--status-face "archived")
              'hermes-browser-muted))
  (should (eq (hermes-browser--status-face "backend-specific")
              'hermes-browser-status)))

(ert-deftest hermes-browser-semantic-status-cell-faces-unknown-states ()
  "Status cells give known and unknown states explicit faces."
  (let ((known (hermes-browser--status-cell "RUNNING"))
        (unknown (hermes-browser--status-cell "custom"))
        (column-unknown
         (hermes-browser--status-cell "custom" 'hermes-browser-status)))
    (should (equal known "RUNNING"))
    (should (eq (get-text-property 0 'face known) 'hermes-browser-active))
    (should (equal unknown "custom"))
    (should (eq (get-text-property 0 'face unknown)
                'hermes-browser-status))
    (should (eq (get-text-property 0 'face column-unknown)
                'hermes-browser-status))))

(ert-deftest hermes-rollback-rows-from-list ()
  "Rollback rows abbreviate the hash and map timestamp/message."
  (let ((rows (hermes-rollback--rows
               '((checkpoints . (((hash . "abcdef1234567890")
                                  (timestamp . "2026-01-01") (message . "edit foo"))))))))
    (should (equal (caar rows) "abcdef1234567890"))
    (should (equal (aref (cadr (car rows)) 0) "abcdef12"))
    (should (equal (aref (cadr (car rows)) 1) "2026-01-01"))
    (should (equal (aref (cadr (car rows)) 2) "edit foo"))))

(ert-deftest hermes-rollback-rows-face-every-column ()
  "Rollback rows give every column its own face."
  (let* ((row (car (hermes-rollback--rows
                    '((checkpoints . (((hash . "abcdef1234567890")
                                       (timestamp . "2026-01-01")
                                       (message . "edit foo"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-identifier))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-timestamp))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-message))))

(ert-deftest hermes-rollback-list-fetches-and-renders ()
  "Listing fetches rollback.list with the live session id and renders it."
  (let (stopped seen-session)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "sid-live"))
              ((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-release)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-rollback-list)
               (lambda (_client &rest args)
                 (setq seen-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve)
                          '((checkpoints . (((hash . "h1") (message . "m1")))))))))
      (unwind-protect
          (progn
            (hermes-list-rollbacks)
            (should (eq stopped 'fake-client))
            (should (equal seen-session "sid-live"))
            (with-current-buffer "*Hermes Rollbacks*"
              (should (derived-mode-p 'hermes-rollback-mode))
              (should (equal (caar tabulated-list-entries) "h1"))))
        (when (get-buffer "*Hermes Rollbacks*") (kill-buffer "*Hermes Rollbacks*"))))))

(ert-deftest hermes-rollback-list-without-live-session-rejects ()
  "Without a live chat session the fetch rejects and the client is released."
  (let (stopped reported)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-rollback--live-session-id) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-release)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-rollback-list)
               (lambda (&rest _) (error "Must not reach the RPC")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) reported))))
      (hermes-list-rollbacks)
      (should (eq stopped 'fake-client))
      (should (cl-some (lambda (m) (string-match-p "live chat session" m))
                       reported)))))

(ert-deftest hermes-rollback-diff-passes-session-id ()
  "The diff command threads the live session id into rollback.diff."
  (let (seen-session seen-hash)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "sid-live"))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-rollback-diff)
               (lambda (_client hash &rest args)
                 (setq seen-hash hash
                       seen-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((diff . "")))))
              ((symbol-function 'hermes-rollback--display-diff)
               (lambda (&rest _))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("hash-1" ["hash-1" "" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-show-diff))
      (should (equal seen-hash "hash-1"))
      (should (equal seen-session "sid-live")))))

(ert-deftest hermes-rollback-display-diff-fontifies ()
  "The diff view renders the unified diff through diff-mode."
  (unwind-protect
      (progn
        (hermes-rollback--display-diff
         "abc1234567"
         '((diff . "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n")))
        (with-current-buffer "*Hermes Rollback Diff*"
          (should (derived-mode-p 'diff-mode))
          (should (string-match-p "\\+new" (buffer-string)))))
    (when (get-buffer "*Hermes Rollback Diff*")
      (kill-buffer "*Hermes Rollback Diff*"))))

(ert-deftest hermes-subagents-rows-indents-by-depth ()
  "Subagent rows indent the goal by spawn depth."
  (let ((rows (hermes-subagents--rows
               '((active . (((subagent_id . "s0") (depth . 0) (goal . "root")
                             (status . "running") (model . "m") (tool_count . 2))
                            ((subagent_id . "s1") (depth . 2) (goal . "child")
                             (status . "running") (model . "m") (tool_count . 0))))))))
    (should (equal (caar rows) "s0"))
    (should (equal (aref (cadr (car rows)) 0) "root"))
    (should (equal (aref (cadr (nth 1 rows)) 0) "    child"))
    (should (equal (aref (cadr (car rows)) 3) "2"))))

(ert-deftest hermes-subagents-rows-face-every-column ()
  "Subagent rows give every column its own face."
  (let* ((row (car (hermes-subagents--rows
                    '((active . (((subagent_id . "s0") (goal . "root")
                                  (status . "running") (model . "m")
                                  (tool_count . 2))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-goal))
    (should (equal (get-text-property 0 'face (aref entry 1))
                   '(hermes-browser-active hermes-browser-status)))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-model))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-tool-count))))

(ert-deftest hermes-subagents-list-fetches-and-renders ()
  "Listing fetches delegation.status and renders active subagents."
  (let (stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-release)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-delegation-status)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((active . (((subagent_id . "s0") (depth . 0)
                                        (goal . "root")))))))))
      (unwind-protect
          (progn
            (hermes-list-subagents)
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Subagents*"
              (should (derived-mode-p 'hermes-subagents-mode))
              (should (equal (caar tabulated-list-entries) "s0"))))
        (when (get-buffer "*Hermes Subagents*")
          (kill-buffer "*Hermes Subagents*"))))))

(ert-deftest hermes-subagents-interrupt-reports-finished-result ()
  "An interrupt result with `found' false does not report success."
  (let ((promise (hermes--promise-make)) messages refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-subagents--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (hermes-subagents-mode)
        (setq tabulated-list-entries '(("s1" ["goal" "running" "m" "0"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-subagents-interrupt)
        (hermes--promise-resolve promise '((found . :false)))
        (should (eq refreshed (current-buffer))))
      (should-not (cl-some (lambda (text) (string-match-p "interrupted" text))
                           messages))
      (should (cl-some (lambda (text)
                         (string-match-p "already finished\\|not found" text))
                       messages)))))

(ert-deftest hermes-browser-revert-does-not-resurrect-killed-buffer ()
  "A late revert result does not recreate its killed browser buffer."
  (let ((promise (hermes--promise-make))
        (hermes-browser-test--fetch-function nil))
    (setq hermes-browser-test--fetch-function (lambda () promise))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore))))
      (with-current-buffer (get-buffer-create "*Hermes Browser Identity*")
              (hermes-browseridentity-mode)
              (hermes-browseridentity--render '("initial")))
      (with-current-buffer "*Hermes Browser Identity*"
        (hermes-browseridentity--revert))
      (kill-buffer "*Hermes Browser Identity*")
      (hermes--promise-resolve promise '("late"))
      (should-not (get-buffer "*Hermes Browser Identity*")))))

(ert-deftest hermes-browser-revert-keeps-newest-result ()
  "An older refresh cannot overwrite rows from a newer refresh."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        (hermes-browser-test--fetch-function nil))
    (setq hermes-browser-test--fetch-function
          (lambda ()
            (setq requests (1+ requests))
            (if (= requests 1) first second)))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore))))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create "*Hermes Browser Identity*")
              (hermes-browseridentity-mode)
              (hermes-browseridentity--render '("initial")))
            (with-current-buffer "*Hermes Browser Identity*"
              (hermes-browseridentity--revert)
              (hermes-browseridentity--revert))
            (hermes--promise-resolve second '("new"))
            (hermes--promise-resolve first '("old"))
            (with-current-buffer "*Hermes Browser Identity*"
              (should (equal (mapcar #'car tabulated-list-entries) '("new")))))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-late-rejections-respect-request-ownership ()
  "Replaced and killed list-browser requests cannot report late failures."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        messages
        (hermes-browser-test--fetch-function nil))
    (setq hermes-browser-test--fetch-function
          (lambda ()
            (setq requests (1+ requests))
            (if (= requests 1) first second)))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create "*Hermes Browser Identity*")
              (hermes-browseridentity-mode)
              (hermes-browseridentity--render '("initial")))
            (with-current-buffer "*Hermes Browser Identity*"
              (hermes-browseridentity--revert)
              (hermes-browseridentity--revert))
            (hermes--promise-reject first "superseded failure")
            (kill-buffer "*Hermes Browser Identity*")
            (hermes--promise-reject second "orphaned failure")
            (should-not messages))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-request-token-survives-mode-reset ()
  "A request token cannot become current again after changing modes twice."
  (with-temp-buffer
    (hermes-browseridentity-mode)
    (let ((old (hermes-browser--next-request-generation)))
      (fundamental-mode)
      (hermes-browseridentity-mode)
      (hermes-browser--next-request-generation)
      (should-not (hermes-browser--request-current-p (current-buffer) old)))))

(ert-deftest hermes-browser-retarget-invalidates-pending-request ()
  "Changing browser instance ownership invalidates pending requests."
  (let ((local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test")))
    (with-temp-buffer
      (setq-local hermes-instance local)
      (let ((generation (hermes-browser--next-request-generation)))
        (hermes-browser--own-instance local)
        (should (hermes-browser--request-current-p
                 (current-buffer) generation))
        (hermes-browser--own-instance remote)
        (should-not (hermes-browser--request-current-p
                     (current-buffer) generation))))))

(ert-deftest hermes-browser-run-on-client-cleans-signalling-setup ()
  "A synchronous fetch setup error releases its transient client once."
  (let ((stops 0) reported)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq stops (1+ stops))))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq reported (apply #'format fmt args)))))
      (hermes-browser--run-on-client (lambda (_client) (error "setup failed")))
      (should (= stops 1))
      (should (equal reported "Hermes: setup failed")))))

(ert-deftest hermes-browser-list-browser-macro-defines-working-browser ()
  "`hermes-define-list-browser' defines a mode, keymap, render, and command."
  (hermes-define-list-browser browsertest
    :title "Hermes Browser Test"
    :buffer "*Hermes Browser Test*"
    :columns [("Name" 20 t)]
    :fetch (lambda (_client) (hermes--promise-resolved '("a" "b")))
    :rows (lambda (result)
            (mapcar (lambda (name) (list name (vector name))) result))
    :keys ("g" #'ignore))
  (unwind-protect
      (progn
        (should (fboundp 'hermes-browsertest-mode))
        (should (fboundp 'hermes-list-browsertest))
        (should (eq (keymap-lookup hermes-browsertest-mode-map "g") #'ignore))
        (with-current-buffer (get-buffer-create "*Hermes Browser Test*")
          (hermes-browsertest-mode)
          (hermes-browsertest--render '("x" "y"))
          (should (derived-mode-p 'hermes-browsertest-mode))
          (should (equal tabulated-list-format [("Name" 20 t)]))
          (should (equal (mapcar #'car tabulated-list-entries) '("x" "y")))))
    (when (get-buffer "*Hermes Browser Test*")
      (kill-buffer "*Hermes Browser Test*"))))

(ert-deftest hermes-browser-list-browser-revert-refreshes-without-display ()
  "Revert refreshes rows in place; only the command displays the buffer."
  (hermes-define-list-browser browserrevert
    :title "Hermes Browser Revert"
    :buffer "*Hermes Browser Revert*"
    :columns [("Name" 20 t)]
    :fetch (lambda (_client) (hermes--promise-resolved '("a" "b")))
    :rows (lambda (result)
            (mapcar (lambda (name) (list name (vector name))) result)))
  (let (displayed)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (setq displayed t))))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create "*Hermes Browser Revert*")
              (hermes-browserrevert-mode)
              (hermes-browserrevert--revert))
            (should-not displayed)
            (with-current-buffer "*Hermes Browser Revert*"
              (should (equal (mapcar #'car tabulated-list-entries) '("a" "b"))))
            (hermes-list-browserrevert)
            (should displayed))
        (when (get-buffer "*Hermes Browser Revert*")
          (kill-buffer "*Hermes Browser Revert*"))))))

;;; Group: dynamic column widths

(ert-deftest hermes-browser-dynamic-format-fits-and-flexes ()
  "Dynamic column format fits the width and grows weighted columns."
  (let ((specs '(("A" 6 0 t) ("B" 8 0 t) ("C" 10 3 nil))))
    (dolist (width '(30 40 80 120))
      (let ((format (hermes-browser--dynamic-format width specs)))
        (should (= (hermes-test--tabulated-list-format-total-width format)
                   width))))
    (let ((narrow (hermes-browser--dynamic-format 40 specs))
          (wide (hermes-browser--dynamic-format 120 specs)))
      (should (> (cadr (aref wide 2)) (cadr (aref narrow 2))))
      (should (= (cadr (aref wide 0)) 6)))))

(ert-deftest hermes-browser-dynamic-format-preserves-sort-and-name ()
  "Dynamic format keeps each spec's header and sort predicate."
  (let ((format (hermes-browser--dynamic-format
                 80 '(("A" 6 0 t) ("B" 10 5 nil)))))
    (should (equal (car (aref format 0)) "A"))
    (should (eq (caddr (aref format 0)) t))
    (should (eq (caddr (aref format 1)) nil))))

(ert-deftest hermes-browser-dynamic-format-honors-max-cap ()
  "A column MAX caps its computed width even on a wide window."
  (let ((format (hermes-browser--dynamic-format
                 200 '(("A" 6 0 t) ("Wide" 10 5 t 20)))))
    (should (= (cadr (aref format 1)) 20))))

(ert-deftest hermes-browser-shrink-widths-fits-narrow-target ()
  "Shrinking trims the widest column until the total fits the target."
  (let ((widths (hermes-browser--shrink-widths '(10 20 30) 30)))
    (should (<= (apply #'+ widths) 30))
    (should (seq-every-p (lambda (w) (> w 0)) widths))))

(ert-deftest hermes-cron-columns-scale-with-width ()
  "Cron dynamic columns fit the display width and keep index-sensitive order."
  (dolist (width '(40 80 120))
    (let ((format (hermes-cron--format width)))
      (should (= (hermes-test--tabulated-list-format-total-width format) width))
      (should (equal (car (aref format 2)) "State"))
      (should (equal (car (aref format 3)) "Profile")))))

;;; Group: desktop notifications

(ert-deftest hermes-browser-notify-uses-notifications-when-available ()
  "When D-Bus notifications exist, the helper forwards title and body."
  (let (got)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (setq got args) 1)))
      (should (hermes-browser--notify "T" "B"))
      (should (equal (plist-get got :title) "T"))
      (should (equal (plist-get got :body) "B")))))

(ert-deftest hermes-browser-notify-falls-back-to-message ()
  "Without notifications the helper degrades to a `message' and returns nil."
  (let (msg)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (not (eq feature 'notifications))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (should-not (hermes-browser--notify "T" "B"))
      (should (string-match-p "T: B" msg)))))

(ert-deftest hermes-profiles-rows-map-fields ()
  "Profile rows carry name, default marker, model, provider, and description."
  (let ((rows (hermes-profiles--rows
               '((profiles . (((name . "default") (is_default . t)
                               (model . "gpt-5.5") (provider . "openai")
                               (description . "main"))
                              ((name . "planner") (is_default . :false))))))))
    (should (equal (caar rows) "default"))
    (should (equal (aref (cadr (car rows)) 1) "*"))
    (should (equal (aref (cadr (car rows)) 2) "gpt-5.5"))
    (should (equal (aref (cadr (car rows)) 3) "openai"))
    (should (equal (aref (cadr (car rows)) 5) "main"))
    (should (equal (aref (cadr (cadr rows)) 1) ""))))

(ert-deftest hermes-profiles-rows-face-every-column ()
  "Profile rows give every column its own face."
  (let* ((row (car (hermes-profiles--rows
                    '((profiles . (((name . "default") (is_default . t)
                                    (model . "gpt-5.5")
                                    (provider . "openai")
                                    (description . "main"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-profile))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-default))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-model))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-provider))
    (should (eq (get-text-property 0 'face (aref entry 4))
                'hermes-browser-reasoning))
    (should (eq (get-text-property 0 'face (aref entry 5))
                'hermes-browser-description))))

(ert-deftest hermes-profiles-set-model-puts-provider-and-model ()
  "Setting a profile model PUTs provider+model to the profile route."
  (let (seen-method seen-path seen-body reverted)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((providers . (((slug . "openai") (name . "openai")
                                           (authenticated . t)
                                           (models . ("gpt-5.5")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (setq seen-method method
                       seen-path path
                       seen-body (plist-get args :body))
                 (hermes--promise-resolved
                  '((ok . t) (model . "gpt-5.5") (provider . "openai")))))
              ((symbol-function 'hermes-profiles--revert)
               (lambda (&rest _) (setq reverted t))))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "\u2014" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-set-model))
      (should (equal seen-method "PUT"))
      (should (equal seen-path "/api/profiles/planner/model"))
      (should (equal (cdr (assq 'provider seen-body)) "openai"))
      (should (equal (cdr (assq 'model seen-body)) "gpt-5.5"))
      (should reverted))))

(ert-deftest hermes-profiles-set-model-refreshes-origin-after-newer-read ()
  "A completed model update starts a fresh read in its originating profile buffer."
  (let ((put (hermes--promise-make)) refreshed)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((providers . (((slug . "openai") (name . "openai")
                                           (authenticated . t)
                                           (models . ("gpt-5.5")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) put))
              ((symbol-function 'hermes-profiles--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (let ((origin (current-buffer)))
          (hermes-profiles-set-model)
          (hermes-browser--next-request-generation)
          (with-temp-buffer
            (hermes--promise-resolve
             put '((ok . t) (model . "gpt-5.5") (provider . "openai"))))
          (should (eq refreshed origin)))))))

(ert-deftest hermes-profiles-stale-model-catalog-cannot-prompt-or-put ()
  "A model catalog from instance A cannot act after retargeting to B."
  (let ((catalog (hermes--promise-make)) prompted put)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () (or hermes-instance '("default" . "http://default"))))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client-a))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) catalog))
              ((symbol-function 'hermes-profiles--read-model-candidate)
               (lambda (&rest _) (setq prompted t) '("p" . "m")))
              ((symbol-function 'hermes-profiles--put-model)
               (lambda (&rest _) (setq put t) (hermes--promise-resolved nil))))
      (with-temp-buffer
        (hermes-profiles-mode)
        (hermes-browser--own-instance '("a" . "http://a"))
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-set-model)
        (hermes-browser--own-instance '("b" . "http://b"))
        (hermes--promise-resolve catalog '((providers . nil)))))
    (should-not prompted)
    (should-not put)))

(ert-deftest hermes-profiles-lifecycle-uses-exact-rest-and-refreshes ()
  "Profile lifecycle commands use exact REST contracts and refresh on success."
  (let (requests (refreshes 0))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :body)) requests)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-profiles--revert)
               (lambda (&rest _) (setq refreshes (1+ refreshes))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (hermes-profiles-create " worker ")
        (setq tabulated-list-entries
              '(("old/name" ["old/name" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-rename " new ")
        (hermes-profiles-delete)))
    (should (member '("POST" "/api/profiles" ((name . "worker"))) requests))
    (should (member '("PATCH" "/api/profiles/old%2Fname"
                      ((new_name . "new")))
                    requests))
    (should (member '("DELETE" "/api/profiles/old%2Fname" nil) requests))
    (should (= refreshes 3))))

(ert-deftest hermes-profiles-lifecycle-refuses-default-profile ()
  "Profile lifecycle commands refuse to mutate the built-in default profile."
  (let (requested)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq requested t)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (hermes-profiles-mode)
        (should-error (hermes-profiles-create " Default ") :type 'user-error)
        (setq tabulated-list-entries
              '(("default" ["default" "*" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-profiles-rename "renamed") :type 'user-error)
        (should-error (hermes-profiles-delete) :type 'user-error)
        (setq tabulated-list-entries
              '(("worker" ["worker" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-profiles-rename " DEFAULT ") :type 'user-error)))
    (should-not requested)))

(ert-deftest hermes-profiles-create-can-clone-existing-profile ()
  "Profile creation sends the backend's optional clone_from field."
  (let (seen-body)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (_method _path &rest args)
                 (setq seen-body (plist-get args :body))
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-profiles--revert) #'ignore)
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (hermes-profiles-create "worker" "planner")))
    (should (equal seen-body
                   '((name . "worker") (clone_from . "planner"))))))

(ert-deftest hermes-profiles-soul-destination-survives-singleton-replacement ()
  "Keep drafts and delayed replies on their exact owner, not a display name."
  (dolist (identity '(distinct same-name typed))
    (let* ((a (if (eq identity 'typed)
                  '(:id "stable" :name "A" :url "https://a.example.test")
                '("A" . "https://a.example.test")))
           (b (if (eq identity 'typed)
                  '(:id "stable" :name "A" :url "https://b.example.test")
                (cons (if (eq identity 'same-name) "A" "B")
                      "https://b.example.test")))
           (hermes-instances (list a))
           requests buffers shown)
      (unwind-protect
          (cl-letf (((symbol-function 'hermes-browser--existing-client)
                     (lambda () 'fake-client))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) (setq shown buffer)
                       (cl-pushnew buffer buffers)))
                    ((symbol-function 'hermes-dashboard-transport-api-request-async)
                     (lambda (method path &rest args)
                       (let ((pending (hermes--promise-make)))
                         (push (list method hermes-instance path
                                     (plist-get args :body) pending) requests)
                         pending))))
            (cl-labels ((open-editor ()
                          (with-temp-buffer
                            (hermes-profiles-mode)
                            (setq tabulated-list-entries
                                  '(("planner" ["planner" "" "" "" "—" ""])))
                            (tabulated-list-print)
                            (goto-char (point-min))
                            (hermes-profiles-edit-soul))
                          shown))
              (let* ((editor-a (open-editor))
                     (get-a (car requests)))
                (with-current-buffer editor-a (insert "A draft\nexact"))
                (setq hermes-instances (list b))
                (let* ((editor-b (open-editor))
                       (get-b (car requests)))
                  (should-not (eq editor-a editor-b))
                  (should (equal (cl-subseq get-b 0 3)
                                 (list "GET" b "/api/profiles/planner/soul")))
                  (hermes--promise-resolve (nth 4 get-b) '((content . "B soul")))
                  (hermes--promise-resolve (nth 4 get-a) '((content . "late A")))
                  (with-current-buffer editor-b
                    (should (equal (buffer-string) "B soul"))
                    (should-not (buffer-modified-p)))
                  (with-current-buffer editor-a
                    (should (equal (buffer-string) "A draft\nexact"))
                    (should-error (hermes-profiles-soul-save) :type 'user-error)
                    (should (= (length requests) 2))
                    (should (buffer-modified-p))
                    (rename-buffer "renamed SOUL draft" t))
                  (setq hermes-instances (list a))
                  (should (eq (open-editor) editor-a))
                  (should (= (length requests) 2))
                  (with-current-buffer editor-a (hermes-profiles-soul-save))
                  (let ((put-a (car requests)))
                    (should (equal (cl-subseq put-a 0 4)
                                   (list "PUT" a "/api/profiles/planner/soul"
                                         '((content . "A draft\nexact")))))
                    (setq hermes-instances (list b))
                    (should (eq (open-editor) editor-b))
                    (with-current-buffer editor-a (insert " newer"))
                    (hermes--promise-resolve (nth 4 put-a) '((ok . t)))
                    (with-current-buffer editor-a
                      (should (buffer-modified-p))
                      (should (equal (buffer-string) "A draft\nexact newer"))))))))
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer))) buffers)))))

(ert-deftest hermes-profiles-soul-save-without-owner-never-falls-back ()
  "An ownerless draft cannot use even the sole configured instance."
  (let ((hermes-instances '(("B" . "https://b.example.test"))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _) (ert-fail "Unexpected client acquisition"))))
      (with-temp-buffer
        (hermes-profiles-soul-mode)
        (setq hermes-profiles-soul-profile "planner")
        (insert "unowned draft")
        (should-error (hermes-profiles-soul-save) :type 'user-error)
        (should (buffer-modified-p))
        (should (equal (buffer-string) "unowned draft"))
        (should-not hermes-profiles--soul-save-pending)))))

(ert-deftest hermes-profiles-soul-get-and-put-use-exact-profile-route ()
  "SOUL editing loads and saves the selected non-default profile."
  (let (requests soul-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (push (list method path (plist-get args :body)) requests)
                     (hermes--promise-resolved
                      (if (equal method "GET")
                          '((content . "You are precise.\n") (exists . t))
                        '((ok . t))))))
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'message) #'ignore))
          (with-temp-buffer
            (hermes-profiles-mode)
            (setq tabulated-list-entries
                  '(("planner" ["planner" "" "" "" "—" ""])))
            (tabulated-list-print)
            (goto-char (point-min))
            (hermes-profiles-edit-soul))
          (setq soul-buffer (get-buffer "*Hermes Profile SOUL: planner*"))
          (should (buffer-live-p soul-buffer))
          (with-current-buffer soul-buffer
            (should (equal (buffer-string) "You are precise.\n"))
            (goto-char (point-max))
            (insert "Stay brief.\n")
            (hermes-profiles-soul-save))
          (should (member '("GET" "/api/profiles/planner/soul" nil) requests))
          (should (member
                   '("PUT" "/api/profiles/planner/soul"
                     ((content . "You are precise.\nStay brief.\n")))
                   requests)))
      (when (buffer-live-p soul-buffer) (kill-buffer soul-buffer)))))

(ert-deftest hermes-profiles-soul-save-orders-real-dispatch-and-preserves-edits ()
  "Reject overlapping dispatch, then save the still-modified latest draft."
  (let (requests)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (let ((promise (hermes--promise-make)))
                   (push (list method path (plist-get args :body) promise) requests)
                   promise))))
      (with-temp-buffer
        (hermes-profiles-soul-mode)
        (setq hermes-instance (hermes-instance-resolve)
              hermes-profiles-soul-profile "planner")
        (insert "first")
        (hermes-profiles-soul-save)
        (insert " latest")
        (should-error (hermes-profiles-soul-save) :type 'user-error)
        ;; Count actual HTTP dispatches, not just ignored callbacks.
        (should (= (length requests) 1))
        (should (equal (caddar requests) '((content . "first"))))
        (should (buffer-modified-p))
        (hermes--promise-resolve (nth 3 (car requests)) '((ok . t)))
        (should (buffer-modified-p))
        (should (equal (buffer-string) "first latest"))
        (hermes-profiles-soul-save)
        (should (= (length requests) 2))
        (should (equal (caddar requests) '((content . "first latest"))))
        (hermes--promise-resolve (nth 3 (car requests)) '((ok . t)))
        (should-not (buffer-modified-p))))))

(ert-deftest hermes-profiles-soul-save-edit-and-undo-stays-modified ()
  "Even edits returning to the submitted text require a new save."
  (let ((pending (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) pending)))
      (with-temp-buffer
        (hermes-profiles-soul-mode)
        (setq hermes-instance (hermes-instance-resolve)
              hermes-profiles-soul-profile "planner")
        (insert "draft")
        (hermes-profiles-soul-save)
        (insert "x")
        (delete-char -1)
        (hermes--promise-resolve pending '((ok . t)))
        (should (equal (buffer-string) "draft"))
        (should (buffer-modified-p))))))

(ert-deftest hermes-profiles-soul-save-failure-allows-retry ()
  "Rejected and synchronously failed saves retain the draft and allow retry."
  (dolist (failure '(reject signal acquire))
    (let ((pending (hermes--promise-make)) (calls 0))
      (cl-letf (((symbol-function 'hermes-browser--existing-client)
                 (lambda ()
                   (when (eq failure 'acquire)
                     (setq failure nil)
                     (error "acquire failed"))
                   'fake-client))
                ((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _)
                   (cl-incf calls)
                   (if (eq failure 'signal)
                       (progn (setq failure nil) (error "dispatch failed"))
                     pending))))
        (with-temp-buffer
          (hermes-profiles-soul-mode)
          (setq hermes-instance (hermes-instance-resolve)
                hermes-profiles-soul-profile "planner")
          (insert "retry me")
          (condition-case nil (hermes-profiles-soul-save) (error nil))
          (hermes--promise-reject pending "save rejected")
          (should (buffer-modified-p))
          (should (equal (buffer-string) "retry me"))
          (setq pending (hermes--promise-make))
          (let ((before calls))
            (hermes-profiles-soul-save)
            (should (= calls (1+ before))))
          (hermes--promise-resolve pending '((ok . t)))
          (should-not (buffer-modified-p)))))))

(ert-deftest hermes-profiles-soul-save-stale-owners-stay-modified ()
  "Success and rejection after owner replacement cannot affect the new draft."
  (dolist (replacement '(profile instance generation mode kill))
    (dolist (settle '(hermes--promise-resolve hermes--promise-reject))
      (let ((pending (hermes--promise-make))
            (hermes-instances '(("first" . "https://first.example.test")))
            messages)
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _) pending))
                  ((symbol-function 'message)
                   (lambda (&rest args) (push args messages))))
          (with-temp-buffer
            (hermes-profiles-soul-mode)
            (setq hermes-instance (hermes-instance-resolve)
                  hermes-profiles-soul-profile "planner")
            (insert "draft")
            (hermes-profiles-soul-save)
            (pcase replacement
              ('profile (setq hermes-profiles-soul-profile "other"))
              ('instance (setq-local hermes-instance
                                     '("other" . "https://other.example.test")))
              ('generation (hermes-browser--next-request-generation))
              ('mode (fundamental-mode))
              ('kill (kill-buffer (current-buffer))))
            (setq messages nil)
            (funcall settle pending "late result")
            (should-not messages)
            (unless (eq replacement 'kill)
              (should (buffer-modified-p))
              (should (equal (buffer-string) "draft")))))))))

(ert-deftest hermes-profiles-soul-save-old-settlement-cannot-release-successor ()
  "An old callback cannot clean or release a replacement editor's save."
  (let (requests)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (let ((pending (hermes--promise-make)))
                   (push pending requests)
                   pending))))
      (with-temp-buffer
        (hermes-profiles-soul-mode)
        (setq hermes-instance (hermes-instance-resolve)
              hermes-profiles-soul-profile "planner")
        (insert "old")
        (hermes-profiles-soul-save)
        (hermes-profiles-soul-mode)
        (setq hermes-instance (hermes-instance-resolve)
              hermes-profiles-soul-profile "other")
        (erase-buffer)
        (insert "successor")
        (hermes-profiles-soul-save)
        (hermes--promise-resolve (cadr requests) '((ok . t)))
        (should (buffer-modified-p))
        (should-error (hermes-profiles-soul-save) :type 'user-error)
        (should (= (length requests) 2))
        (hermes--promise-resolve (car requests) '((ok . t)))
        (should-not (buffer-modified-p))
        (should (equal (buffer-string) "successor"))))))

(ert-deftest hermes-profiles-soul-ignores-stale-and-killed-buffer-results ()
  "Late SOUL reads cannot overwrite a repurposed or killed editor buffer."
  (let ((read (hermes--promise-make)) target)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) read))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-edit-soul))
      (setq target (get-buffer "*Hermes Profile SOUL: planner*"))
      (with-current-buffer target
        (setq hermes-profiles-soul-profile "other")
        (insert "new owner"))
      (hermes--promise-resolve read '((content . "stale") (exists . t)))
      (with-current-buffer target
        (should (equal (buffer-string) "new owner")))
      (kill-buffer target)
      (should-not (buffer-live-p target)))))

(ert-deftest hermes-profiles-soul-read-preserves-input-typed-while-loading ()
  "A late SOUL read does not overwrite user input typed after dispatch."
  (let ((read (hermes--promise-make)) target)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _) read))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (with-temp-buffer
            (hermes-profiles-mode)
            (setq tabulated-list-entries
                  '(("planner" ["planner" "" "" "" "—" ""])))
            (tabulated-list-print)
            (goto-char (point-min))
            (hermes-profiles-edit-soul))
          (setq target (get-buffer "*Hermes Profile SOUL: planner*"))
          (with-current-buffer target (insert "typed while loading"))
          (hermes--promise-resolve read '((content . "stale")))
          (with-current-buffer target
            (should (equal (buffer-string) "typed while loading"))))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest hermes-profiles-soul-buffers-are-instance-specific ()
  "The same profile on two instances uses two independently owned editors."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         buffers saved-instance saved-content)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method _path &rest args)
                     (when (equal method "PUT")
                       (setq saved-instance hermes-instance
                             saved-content
                             (alist-get 'content (plist-get args :body))))
                     (hermes--promise-resolved '((content . "SOUL\n")))))
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
          (dolist (instance (list local remote))
            (with-temp-buffer
              (hermes-profiles-mode)
              (setq hermes-instance instance
                    tabulated-list-entries
                    '(("planner" ["planner" "" "" "" "—" ""])))
              (tabulated-list-print)
              (goto-char (point-min))
              (hermes-profiles-edit-soul)))
          (setq buffers
                (list (get-buffer "*Hermes Profile SOUL@local: planner*")
                      (get-buffer "*Hermes Profile SOUL@remote: planner*")))
          (should (cl-every #'buffer-live-p buffers))
          (should (equal (mapcar (lambda (buffer)
                                   (buffer-local-value 'hermes-instance buffer))
                                 buffers)
                         (list local remote)))
          (with-current-buffer (car buffers)
            (goto-char (point-max))
            (insert "local draft\n")
            (should (buffer-modified-p))
            (hermes-profiles-soul-save))
          (should (equal saved-instance local))
          (should (equal saved-content "SOUL\nlocal draft\n"))
          (let ((header (with-current-buffer (cadr buffers)
                          (hermes-profiles--soul-header-line))))
            (should (string-match-p "Hermes instance: remote" header))
            (should (string-match-p "Profile: planner" header))
            (should (string-match-p "C-c C-c save" header))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest hermes-profiles-soul-refuses-default-profile ()
  "The built-in default profile has no editable SOUL surface."
  (let (requested)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq requested t)
                 (hermes--promise-resolved nil))))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("default" ["default" "*" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-profiles-edit-soul) :type 'user-error)))
    (should-not requested)))

(ert-deftest hermes-dashboard-keymap-reaches-profiles-browser ()
  "The main dashboard exposes profile management directly."
  (should (eq (lookup-key hermes-dashboard-mode-map (kbd "F"))
              #'hermes-list-profiles)))

(ert-deftest hermes-rollback-diff-ignores-stale-result ()
  "An older rollback diff cannot replace the result of a newer request."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (calls 0)
        displayed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (if (= calls 1) first second)))
              ((symbol-function 'hermes-rollback--display-diff)
               (lambda (_hash result)
                 (push (hermes-transport--get result 'diff) displayed))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-show-diff)
        (hermes-rollback-show-diff)
        (hermes--promise-resolve second '((diff . "new")))
        (hermes--promise-resolve first '((diff . "old"))))
      (should (equal displayed '("new"))))))

(ert-deftest hermes-rollback-diff-ignores-killed-origin ()
  "A rollback diff response is ignored after its list buffer dies."
  (let ((promise (hermes--promise-make)) displayed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-rollback--display-diff)
               (lambda (&rest _) (setq displayed t))))
      (let ((origin (generate-new-buffer " *Hermes rollback origin*")))
        (with-current-buffer origin
          (hermes-rollback-mode)
          (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
          (tabulated-list-print)
          (goto-char (point-min))
          (hermes-rollback-show-diff))
        (kill-buffer origin)
        (hermes--promise-resolve promise '((diff . "late")))
        (should-not displayed)))))

(ert-deftest hermes-rollback-restore-rejects-false-success ()
  "A rollback response declaring failure does not report success or refresh."
  (let (messages refreshed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-catch
                  (hermes--promise-then (funcall make-promise 'client) on-success)
                  (lambda (reason) (push reason messages)))))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) (hermes--promise-resolved
                                   '((success . :false) (error . "denied")))))
              ((symbol-function 'hermes-rollback--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-restore))
      (should-not refreshed)
      (should (cl-some (lambda (text) (string-match-p "denied" text)) messages))
      (should-not (cl-some (lambda (text) (string-match-p "restored" text)) messages)))))

(ert-deftest hermes-rollback-restore-refreshes-origin-on-success ()
  "A successful rollback starts a fresh read in its originating buffer."
  (let (refreshed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) (hermes--promise-resolved '((success . t)))))
              ((symbol-function 'hermes-rollback--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (let ((origin (current-buffer)))
          (hermes-rollback-restore)
          (should (eq refreshed origin)))))))

(ert-deftest hermes-rollback-late-restore-does-not-report-or-refresh ()
  "An instance A restore cannot report or refresh after retargeting to B."
  (let ((promise (hermes--promise-make)) messages refreshed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-rollback--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (hermes-browser--own-instance '("a" . "http://a"))
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-restore)
        (hermes-browser--own-instance '("b" . "http://b"))
        (hermes--promise-resolve promise '((success . t)))
        (should-not refreshed)
        (should-not messages)))))

(ert-deftest hermes-subagents-interrupt-refreshes-after-newer-read ()
  "A completed interrupt starts a fresh read despite an intervening refresh."
  (let ((promise (hermes--promise-make)) refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-subagents--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-subagents-mode)
        (setq tabulated-list-entries '(("s1" ["goal" "running" "m" "0"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (let ((origin (current-buffer)))
          (hermes-subagents-interrupt)
          (hermes-browser--next-request-generation)
          (hermes--promise-resolve promise '((found . t)))
          (should (eq refreshed origin)))))))

(ert-deftest hermes-subagents-late-interrupt-does-not-report-or-refresh ()
  "An instance A interrupt cannot report or refresh after retargeting to B."
  (let ((promise (hermes--promise-make)) messages refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-subagents--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (hermes-subagents-mode)
        (hermes-browser--own-instance '("a" . "http://a"))
        (setq tabulated-list-entries '(("s1" ["goal" "running" "m" "0"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-subagents-interrupt)
        (hermes-browser--own-instance '("b" . "http://b"))
        (hermes--promise-resolve promise '((found . t)))
        (should-not refreshed)
        (should-not messages)))))

(ert-deftest hermes-browser-entry-displays-before-deferred-result ()
  "Show the pending list now; late replies cannot redirect subsequent typing."
  (save-window-excursion
    (let ((draft (generate-new-buffer " *browser draft*")) callback)
      (unwind-protect
          (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                     (lambda (_fetch success &optional _error)
                       (setq callback success))))
            (call-interactively #'hermes-list-sessions)
            (should (eq (window-buffer) (get-buffer "*Hermes Sessions*")))
            (with-current-buffer "*Hermes Sessions*"
              (should (equal hermes-browser--status "Loading")))
            (switch-to-buffer draft)
            (buffer-enable-undo)
            (execute-kbd-macro "draft")
            (let ((point (point)) (undo (copy-tree buffer-undo-list)))
              (funcall callback '((sessions . (((id . "fixture")
                                                (title . "Visible result"))))))
              (should (eq (window-buffer) draft))
              (should (= (point) point))
              (should (equal buffer-undo-list undo))
              (should (equal (buffer-string) "draft")))
            (with-current-buffer "*Hermes Sessions*"
              (should (string-match-p "Visible result" (buffer-string)))))
        (kill-buffer draft)
        (when (get-buffer "*Hermes Sessions*") (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-browser-simple-help-command-loop ()
  "Native popup dispatch, dismissal, and direct help preserve ordinary maps."
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *browser help*")) (refreshes 0))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (hermes-sessions-mode)
            (setq hermes-sessions--archived-filter "only"
                  hermes-sessions--all-profiles t)
            (cl-letf (((symbol-function 'hermes-sessions--revert)
                       (lambda (&rest _) (cl-incf refreshes))))
              (execute-kbd-macro (kbd "?"))
              (should (get-buffer-window keymap-popup--buffer-name))
              (with-current-buffer keymap-popup--buffer-name
                (should (string-match-p
                         "Archived: only"
                         (plist-get (plist-get keymap-popup--session :active)
                                    :resolved-docstring))))
              (execute-kbd-macro (kbd "C-g"))
              (should-not (get-buffer-window keymap-popup--buffer-name))
              (should (eq (window-buffer) buffer))
              (execute-kbd-macro (kbd "? g"))
              (should (= refreshes 1))
              (execute-kbd-macro (kbd "g"))
              (should (= refreshes 2))
              (should (eq (key-binding (kbd "RET")) #'hermes-sessions-open))
              (should (keymapp (key-binding (kbd "C-h"))))
              (execute-kbd-macro (kbd "? q"))
              (should-not (eq (window-buffer) buffer))))
        (keymap-popup-dismiss)
        (kill-buffer buffer)))))

(defconst hermes-browser-test--kanban-buffers
  '("*Hermes Kanban Boards*" "*Hermes Kanban*" "*Hermes Kanban Task*"
    "*Hermes Kanban Log*" "*Hermes Kanban Diagnostics*"))

(ert-deftest hermes-browser-kanban-entry-focus-and-pending-context ()
  "Every public Kanban navigation displays an initialized pending owner."
  (save-window-excursion
    (dolist (kind '(boards board task log diagnostics))
      (let ((pending (hermes--promise-make))
            (draft (generate-new-buffer " *kanban draft*")))
        (unwind-protect
            (cl-letf (((symbol-function 'hermes-kanban--api)
                       (lambda (method path &rest _)
                         (should (equal method "GET"))
                         (if (equal path "/orchestration")
                             (hermes--promise-resolved nil) pending))))
              (switch-to-buffer draft)
              (hermes-kanban-mode)
              (setq hermes-kanban--slug "alpha"
                    tabulated-list-entries '(("one" ["todo" "1" "worker" "One"])))
              (tabulated-list-print)
              (goto-char (point-min))
              (let* ((name (pcase kind
                             ('boards (call-interactively #'hermes-list-kanban)
                                      "*Hermes Kanban Boards*")
                             ('board (hermes-kanban-open-board-task "alpha" "one")
                                     "*Hermes Kanban*")
                             ('task (call-interactively #'hermes-kanban-show)
                                    "*Hermes Kanban Task*")
                             ('log (call-interactively #'hermes-kanban-show-log)
                                   "*Hermes Kanban Log*")
                             ('diagnostics (call-interactively #'hermes-kanban-diagnostics)
                                           "*Hermes Kanban Diagnostics*")))
                     (target (get-buffer name)))
                (should (eq (window-buffer) target))
                (with-current-buffer target
                  (should (equal hermes-browser--status "Loading"))
                  (when (eq kind 'task) (should (equal hermes-kanban-task--task-id "one")))
                  (when (eq kind 'log) (should (equal hermes-kanban-log--task-id "one"))))
                (switch-to-buffer draft)
                (fundamental-mode)
                (read-only-mode -1)
                (erase-buffer)
                (buffer-enable-undo)
                (execute-kbd-macro "still typing")
                (let ((point (point)) (undo (copy-tree buffer-undo-list)))
                  (hermes--promise-resolve
                   pending '((task . ((id . "one") (title . "One")))
                             (task_id . "one") (exists . t) (content . "log")))
                  (should (eq (window-buffer) draft))
                  (should (= (point) point))
                  (should (equal buffer-undo-list undo))
                  (should (equal (buffer-string) "still typing")))))
          (kill-buffer draft)
          (dolist (name hermes-browser-test--kanban-buffers)
            (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest hermes-browser-kanban-refresh-preserves-every-window ()
  "Task and log refresh retain buffer point and each independent viewport."
  (save-window-excursion
    (dolist (kind '(task log))
      (let* ((text (mapconcat (lambda (n) (format "Line %d: content" n))
                             (number-sequence 1 100) "\n"))
             (payload `((task . ((id . "one") (title . "One") (body . ,text)))
                        (task_id . "one") (exists . t) (content . ,text)))
             (pending (hermes--promise-make)))
        (unwind-protect
            (cl-letf (((symbol-function 'hermes-kanban--api)
                       (lambda (method &rest _) (should (equal method "GET")) pending)))
              (delete-other-windows)
              (if (eq kind 'task)
                  (hermes-kanban--display-task payload "alpha" nil nil (hermes-instance-resolve))
                (hermes-kanban--display-log payload "alpha" nil (hermes-instance-resolve)))
              (set-buffer (window-buffer))
              (let* ((target (window-buffer))
                     (first (selected-window))
                     (second (split-window-below))
                     (start-a (save-excursion (goto-char (point-min)) (forward-line 10) (point)))
                     (start-b (save-excursion (goto-char (point-min)) (forward-line 40) (point)))
                     (point-a (+ start-a 4))
                     (point-b (+ start-b 7)))
                (set-buffer target)
                (set-window-buffer second target)
                (set-window-start first start-a t)
                (set-window-start second start-b t)
                (set-window-point first point-a)
                (set-window-point second point-b)
                (execute-kbd-macro (kbd "g"))
                (hermes--promise-resolve pending payload)
                (should (= (with-current-buffer target (point)) point-a))
                (should (= (window-start first) start-a))
                (should (= (window-start second) start-b))
                (should (= (window-point first) point-a))
                (should (= (window-point second) point-b))))
          (dolist (name hermes-browser-test--kanban-buffers)
            (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest hermes-browser-list-status-failure-and-retry ()
  "Initial acquisition failure is visible, and g distinguishes empty from loading."
  (save-window-excursion
    (let ((pending (hermes--promise-make))
          (hermes-browser-test--fetch-function nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'hermes-browser--with-client)
                       (lambda (_) (error "Unavailable"))))
              (hermes-list-browseridentity))
            (should (eq (window-buffer) (get-buffer "*Hermes Browser Identity*")))
            (set-buffer (window-buffer))
            (should (string-prefix-p "Failed" hermes-browser--status))
            (setq hermes-browser-test--fetch-function (lambda () pending))
            (cl-letf (((symbol-function 'hermes-browser--with-client)
                       (lambda (fn) (funcall fn 'test-client #'ignore))))
              (execute-kbd-macro (kbd "g")))
            (should (equal hermes-browser--status "Loading"))
            (hermes--promise-resolve pending nil)
            (should (equal hermes-browser--status "Empty")))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-kanban-board-refresh-sort-and-row ()
  "Public board refresh retains the selected sort and logical task row."
  (save-window-excursion
    (let* ((tasks '(((id . "a") (title . "Zulu") (created_at . 2))
                    ((id . "b") (title . "Alpha") (created_at . 1))))
           (payload `((columns . (((tasks . ,tasks))))))
           (pending (hermes--promise-make)))
      (unwind-protect
          (cl-letf (((symbol-function 'hermes-kanban--api)
                     (lambda (method path &rest _)
                       (should (equal method "GET"))
                       (if (equal path "/orchestration")
                           (hermes--promise-resolved nil) pending))))
            (hermes-kanban-open-board-task "alpha" "a")
            (hermes--promise-resolve pending payload)
            (set-buffer (window-buffer))
            (tabulated-list-sort 3)
            (hermes-kanban--goto-task-row "a")
            (let ((sort tabulated-list-sort-key))
              (setq pending (hermes--promise-make))
              (execute-kbd-macro (kbd "g"))
              (hermes--promise-resolve pending payload)
              (should (equal tabulated-list-sort-key sort))
              (should (equal (tabulated-list-get-id) "a")))
            (setq pending (hermes--promise-make))
            (hermes-kanban-open-board-task "beta" nil)
            (should (equal hermes-kanban--slug "beta"))
            (should-not tabulated-list-entries)
            (should-not tabulated-list-sort-key)
            (hermes--promise-resolve pending payload)
            (should (= (point) (point-min))))
        (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-browser-kanban-detached-reading-and-replacement ()
  "Refresh preserves hidden/unselected readers; replacing a task starts at top."
  (save-window-excursion
    (dolist (kind '(task log))
      (dolist (visibility '(hidden unselected))
        (let* ((text (mapconcat (lambda (n) (format "Line %d" n))
                               (number-sequence 1 80) "\n"))
               (payload `((task . ((id . "one") (body . ,text)))
                          (task_id . "one") (exists . t) (content . ,text)))
               (pending (hermes--promise-make))
               (draft (generate-new-buffer " *detached draft*")))
          (unwind-protect
              (cl-letf (((symbol-function 'hermes-kanban--api)
                         (lambda (method &rest _) (should (equal method "GET")) pending)))
                (delete-other-windows)
                (if (eq kind 'task) (hermes-kanban-open-task "one" "alpha")
                  (hermes-kanban--open-log "one" "alpha"))
                (hermes--promise-resolve pending payload)
                (set-buffer (window-buffer))
                (let* ((target (current-buffer))
                       (reader (selected-window)))
                  (goto-char (point-min)) (forward-line 20)
                  (set-window-start reader (point) t)
                  (forward-line 3) (forward-char 2)
                  (let ((position (point)) (start (window-start reader)))
                    (when (eq visibility 'unselected) (select-window (split-window-below)))
                    (switch-to-buffer draft)
                    (execute-kbd-macro "new draft")
                    (setq pending (hermes--promise-make))
                    (with-current-buffer target (revert-buffer nil t))
                    (hermes--promise-resolve pending payload)
                    (should (eq (window-buffer) draft))
                    (should (equal (buffer-string) "new draft"))
                    (with-current-buffer target (should (= (point) position)))
                    (when (eq visibility 'unselected)
                      (should (= (window-point reader) position))
                      (should (= (window-start reader) start))))
                  (setq pending (hermes--promise-make))
                  (if (eq kind 'task) (hermes-kanban-open-task "two" "alpha")
                    (hermes-kanban--open-log "two" "alpha"))
                  (with-current-buffer target
                    (should (= (buffer-size) 0))
                    (should (equal hermes-browser--status "Loading")))
                  (hermes--promise-resolve
                   pending '((task . ((id . "two") (body . "Short")))
                             (task_id . "two") (exists . t) (content . "Short")))
                  (with-current-buffer target (should (= (point) (point-min))))))
            (kill-buffer draft)
            (dolist (name hermes-browser-test--kanban-buffers)
              (when (get-buffer name) (kill-buffer name)))))))))

(ert-deftest hermes-browser-kanban-results-own-buffer-not-name ()
  "Renamed targets retain results; killed or repurposed owners ignore both settlements."
  (dolist (kind '(boards board task log diagnostics))
    (dolist (change '(rename mode kill instance))
      (dolist (reject '(nil t))
        (save-window-excursion
          (let ((pending (hermes--promise-make)) target replacement messages)
            (unwind-protect
                (cl-letf (((symbol-function 'hermes-kanban--api)
                           (lambda (method path &rest _)
                             (should (equal method "GET"))
                             (if (equal path "/orchestration")
                                 (hermes--promise-resolved nil) pending)))
                          ((symbol-function 'message)
                           (lambda (&rest args) (push args messages))))
                  (pcase kind
                    ('boards (hermes-list-kanban))
                    ('board (hermes-kanban-open-board-task "alpha" nil))
                    ('task (hermes-kanban-open-task "one" "alpha"))
                    ('log (hermes-kanban--open-log "one" "alpha"))
                    ('diagnostics (hermes-kanban--render-diagnostics "alpha" "Alpha")))
                  (setq target (window-buffer))
                  (with-current-buffer target
                    (pcase change
                      ('rename
                       (let ((name (buffer-name)))
                         (rename-buffer " *renamed browser*" t)
                         (setq replacement (get-buffer-create name))))
                      ('mode (fundamental-mode))
                      ('kill (kill-buffer))
                      ('instance (setq hermes-instance '("other" . "https://other.invalid")))))
                  (setq messages nil)
                  (if reject (hermes--promise-reject pending "Read failed")
                    (hermes--promise-resolve pending '((task . ((id . "one"))) (task_id . "one"))))
                  (when (buffer-live-p replacement)
                    (with-current-buffer replacement (should (= (buffer-size) 0))))
                  (if (eq change 'rename)
                      (with-current-buffer target
                        (should-not (equal hermes-browser--status "Loading")))
                    (should-not messages)
                    (when (buffer-live-p target)
                      (with-current-buffer target (should (= (buffer-size) 0))))))
              (when (buffer-live-p target) (kill-buffer target))
              (when (buffer-live-p replacement) (kill-buffer replacement)))))))))

(ert-deftest hermes-browser-help-shared-modes-and-editable-soul ()
  "Every simple list and observed-work view has native contextual help."
  (save-window-excursion
    (dolist (mode '(hermes-sessions-mode hermes-session-detail-mode hermes-cron-mode
                    hermes-profiles-mode hermes-rollback-mode hermes-subagents-mode
                    hermes-provider-accounts-mode hermes-work-mode hermes-work-log-mode))
      (let ((buffer (generate-new-buffer " *native browser help*")))
        (unwind-protect
            (progn
              (switch-to-buffer buffer) (funcall mode)
              (let ((map (current-local-map))
                    (refresh (key-binding (kbd "g")))
                    (quit (key-binding (kbd "q"))))
                (execute-kbd-macro (kbd "?"))
                (should (get-buffer-window keymap-popup--buffer-name))
                (with-current-buffer keymap-popup--buffer-name
                  (should (string-match-p "Refresh" (buffer-string))))
                (execute-kbd-macro (kbd "C-g"))
                (should (eq (window-buffer) buffer))
                (should (eq map (current-local-map)))
                (should (eq refresh (key-binding (kbd "g"))))
                (should (eq quit (key-binding (kbd "q"))))
                (should (keymapp (key-binding (kbd "C-h"))))))
          (keymap-popup-dismiss) (kill-buffer buffer))))
    (let ((buffer (generate-new-buffer " *SOUL typing*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer) (hermes-profiles-soul-mode)
            (execute-kbd-macro "? q g ordinary text")
            (should (equal (buffer-string) "? q g ordinary text")))
        (kill-buffer buffer)))))

(ert-deftest hermes-browser-kanban-initial-failure-and-retry ()
  "A new view settles failure, and its direct refresh can load usable data."
  (save-window-excursion
    (dolist (kind '(boards board task log diagnostics))
      (let ((pending (hermes--promise-make)))
        (unwind-protect
            (cl-letf (((symbol-function 'hermes-kanban--api)
                       (lambda (method path &rest _)
                         (should (equal method "GET"))
                         (if (equal path "/orchestration")
                             (hermes--promise-resolved nil) pending))))
              (dolist (name hermes-browser-test--kanban-buffers)
                (should-not (get-buffer name)))
              (pcase kind
                ('boards (call-interactively #'hermes-list-kanban))
                ('board (hermes-kanban-open-board-task "alpha" nil))
                ('task (hermes-kanban-open-task "one" "alpha"))
                ('log (hermes-kanban--open-log "one" "alpha"))
                ('diagnostics (hermes-kanban--render-diagnostics "alpha" "Alpha")))
              (set-buffer (window-buffer))
              (should (equal hermes-browser--status "Loading"))
              (hermes--promise-reject pending "Read unavailable")
              (should (string-prefix-p "Failed" hermes-browser--status))
              (setq pending (hermes--promise-make))
              (execute-kbd-macro (kbd "g"))
              (should (equal hermes-browser--status "Loading"))
              (hermes--promise-resolve
               pending '((task . ((id . "one") (title . "One")))
                         (task_id . "one") (exists . t) (content . "Log ready")))
              (should (member hermes-browser--status '("Ready" "Empty")))
              (when (memq kind '(task log))
                (should (string-match-p (if (eq kind 'task) "One" "Log ready")
                                        (buffer-string)))))
          (dolist (name hermes-browser-test--kanban-buffers)
            (when (get-buffer name) (kill-buffer name))))))))

(provide 'hermes-browsers-tests)
;;; hermes-browsers-tests.el ends here
