;;; hermes-inventory-tests.el --- inventory tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-inventory-toolset-rows ()
  "Toolset rows map name/enabled/count/description."
  (let ((rows (hermes-inventory--toolset-rows
               '((toolsets . (((name . "files") (enabled . t) (tool_count . 5)
                               (description . "File ops"))))))))
    (should (equal (caar rows) "files"))
    (should (equal (aref (cadr (car rows)) 1) "on"))
    (should (equal (aref (cadr (car rows)) 2) "5"))
    (should (equal (aref (cadr (car rows)) 3) "File ops"))))

(ert-deftest hermes-inventory-skill-rows-flattens-categories ()
  "Skill rows flatten the category->names map into per-skill rows."
  (let ((rows (hermes-inventory--skill-rows
               '((skills . ((coding . ("refactor" "review")) (writing . ("draft"))))))))
    (should (equal (mapcar (lambda (r) (aref (cadr r) 1)) rows)
                   '("refactor" "review" "draft")))
    (should (equal (aref (cadr (car rows)) 0) "coding"))))

(ert-deftest hermes-inventory-agent-and-plugin-rows ()
  "Agent and plugin rows map their fields."
  (let ((agents (hermes-inventory--agent-rows
                 '((processes . (((session_id . "a1") (status . "running")
                                  (uptime . 42) (command . "do x")))))))
        (plugins (hermes-inventory--plugin-rows
                  '((plugins . (((name . "p1") (version . "1.2") (enabled . nil))))))))
    (should (equal (aref (cadr (car agents)) 0) "a1"))
    (should (equal (aref (cadr (car agents)) 2) "42"))
    (should (equal (aref (cadr (car plugins)) 1) "1.2"))
    (should (equal (aref (cadr (car plugins)) 2) "off"))))

(ert-deftest hermes-inventory-list-fetches-and-renders ()
  "Choosing a category fetches its method and renders the rows."
  (let (requested-method stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'completing-read) (lambda (&rest _) "Toolsets"))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client method _params resolve _reject)
                 (setq requested-method method)
                 (funcall resolve '((toolsets . (((name . "files") (enabled . t)
                                                  (tool_count . 5)))))))))
      (unwind-protect
          (progn
            (hermes-list-inventory)
            (should (equal requested-method "tools.list"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Toolsets*"
              (should (derived-mode-p 'hermes-inventory-mode))
              (should (equal (caar tabulated-list-entries) "files"))))
        (when (get-buffer "*Hermes Toolsets*") (kill-buffer "*Hermes Toolsets*"))))))

(ert-deftest hermes-inventory-skill-rows-map-dashboard-skill-list ()
  "Skill rows map dashboard REST skill metadata, including enabled state."
  (let ((rows (hermes-inventory--skill-rows
               '((skills . (((name . "review") (category . "coding")
                              (description . "Review code") (enabled . t))
                             ((name . "draft") (category . "writing")
                              (description . "Draft text") (enabled . nil))))))))
    (should (equal (mapcar #'car rows) '("review" "draft")))
    (should (equal (aref (cadr (car rows)) 0) "coding"))
    (should (equal (aref (cadr (car rows)) 2) "on"))
    (should (equal (aref (cadr (nth 1 rows)) 2) "off"))
    (should (equal (aref (cadr (car rows)) 3) "Review code"))))

(ert-deftest hermes-inventory-skills-result-normalizes-rest-shapes ()
  "Skill REST payloads normalize raw-list and object response shapes."
  (let ((raw '(((name . "review") (enabled . t))))
        (wrapped '((skills . (((name . "draft") (enabled . nil)))))))
    (should (equal (hermes-inventory--skills-result raw)
                   `((skills . ,raw))))
    (should (equal (hermes-inventory--skills-result wrapped) wrapped))))

(ert-deftest hermes-inventory-fetch-skills-prefers-rest ()
  "Skill inventory fetch uses dashboard REST when `/api/skills' is available."
  (let (method path requested-client result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       requested-client (plist-get args :client))
                 (hermes--promise-resolved
                  '(((name . "review") (enabled . t)))))))
      (hermes--promise-then
       (hermes-inventory--skills-promise
        'fake-client (assoc "Skills" hermes-inventory--specs))
       (lambda (r) (setq result r)))
      (should (equal method "GET"))
      (should (equal path "/api/skills"))
      (should (eq requested-client 'fake-client))
      (should (equal result '((skills . (((name . "review") (enabled . t))))))))))

(ert-deftest hermes-inventory-fetch-skills-falls-back-to-jsonrpc ()
  "Skill inventory fetch falls back to read-only JSON-RPC when REST fails."
  (let (fallback-method message-text result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-rejected "missing endpoint")))
              ((symbol-function 'hermes-dashboard-transport-call)
               (lambda (_client method _params)
                 (setq fallback-method method)
                 (hermes--promise-resolved '((skills . nil)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (let ((spec (assoc "Skills" hermes-inventory--specs)))
        (hermes--promise-then
         (hermes-inventory--skills-promise 'fake-client spec)
         (lambda (r) (setq result r)))
        (should (equal fallback-method (nth 1 spec)))
        (should (string-match-p "using read-only list" message-text))
        (should (equal result '((skills . nil))))))))

(ert-deftest hermes-inventory-toolset-toggle-sends-tools-configure ()
  "Inventory toolset actions go through `tools.configure' with safe actions.
Toolset toggles are global configuration: no `:session-id' is sent."
  :tags '(shared-socket-isolation)
  (let (names action session done-called reverted)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (let ((client (hermes-test--dashboard-client)))
                   (funcall fn client (lambda () (setq done-called t))))))
              ((symbol-function 'hermes-dashboard-transport-tools-configure)
               (lambda (_client ns act &rest args)
                 (setq names ns action act session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((reset . t)))))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) #'ignore))
      (hermes-inventory--set-toolset-enabled "terminal" nil)
      (should done-called)
      (should reverted)
      (should (equal names '("terminal")))
      (should (equal action "disable"))
      (should-not session))))

(ert-deftest hermes-inventory-skill-toggle-posts-rest-json-boolean ()
  "Inventory skill actions use the dashboard REST toggle endpoint, no CLI shellout."
  (let (method path body requested-client done-called reverted)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       body (plist-get args :body)
                       requested-client (plist-get args :client))
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) #'ignore))
      (hermes-inventory--set-skill-enabled "review" nil)
      (should done-called)
      (should reverted)
      (should (equal method "PUT"))
      (should (equal path "/api/skills/toggle"))
      (should (eq requested-client 'fake-client))
      (should (equal body '((name . "review") (enabled . :false)))))))

(ert-deftest hermes-inventory-skill-toggle-cleans-up-on-rest-error ()
  "Skill toggle stops transient clients when REST toggle fails."
  (let (done-called message-text reverted)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-rejected "token missing")))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (hermes-inventory--set-skill-enabled "review" t)
      (should done-called)
      (should-not reverted)
      (should (equal message-text "Hermes: token missing")))))

(ert-deftest hermes-inventory-reload-skills-dispatches-rpc-and-refreshes ()
  "Skill reload uses dashboard RPC and refreshes skill inventory buffers."
  (let (done-called reloaded-client message-text reverted)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-skills-reload)
               (lambda (client &rest args)
                 (setq reloaded-client client)
                 (funcall (plist-get args :resolve) '((output . "Reloaded skills")))))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (let ((hermes-inventory--spec (assoc "Skills" hermes-inventory--specs)))
        (hermes-inventory-reload-skills))
      (should done-called)
      (should (eq reloaded-client 'fake-client))
      (should (equal message-text "Hermes: Reloaded skills"))
      (should reverted))))

(ert-deftest hermes-inventory-memory-status-redacts-secrets-and-contents ()
  "Memory status displays only provider names/sizes, never contents or secrets."
  (let* ((secret "token-secret-value-that-is-long-enough-to-redact-1234567890")
         (text (hermes-inventory--memory-status-text
                `((active . ,secret)
                  (providers . (((name . "built-in")
                                 (description . "contains-private-detail")
                                 (configured . t))
                                ((name . ,secret)
                                 (description . "contains-private-token")
                                 (configured . nil))))
                  (builtin_files . ((memory . 12) (user . 34)))
                  (memory_contents . "do not show this")))))
    (should (string-match-p "Active provider: <redacted>" text))
    (should (string-match-p "MEMORY.md: 12 bytes" text))
    (should (string-match-p "USER.md: 34 bytes" text))
    (should (string-match-p "<redacted>" text))
    (should-not (string-match-p (regexp-quote secret) text))
    (should-not (string-match-p "External providers" text))
    (should-not (string-match-p "built-in (configured)" text))
    (should-not (string-match-p "contains-private" text))
    (should-not (string-match-p "do not show this" text))))

(ert-deftest hermes-memory-status-fetches-rest-with-client ()
  "Memory status passes the live dashboard client to REST."
  (let (method path requested-client rendered done-called)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       requested-client (plist-get args :client))
                 (hermes--promise-resolved
                  '((active . "built-in") (builtin_files . ((memory . 1)))))))
              ((symbol-function 'hermes-inventory--render-memory-status)
               (lambda (status &optional _display) (setq rendered status))))
      (hermes-memory-status)
      (should done-called)
      (should (equal method "GET"))
      (should (equal path "/api/memory"))
      (should (eq requested-client 'fake-client))
      (should (equal rendered '((active . "built-in")
                                (builtin_files . ((memory . 1)))))))))

(ert-deftest hermes-memory-reset-confirms-and-posts-target ()
  "Memory reset is gated by yes-or-no-p and posts the chosen target to REST."
  (let (prompt method path body requested-client done-called refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       body (plist-get args :body)
                       requested-client (plist-get args :client))
                 (hermes--promise-resolved '((ok . t) (deleted . ("USER.md"))))))
              ((symbol-function 'hermes-memory-status)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message) #'ignore))
      (hermes-memory-reset "user")
      (should (string-match-p "Erase built-in Hermes user memory" prompt))
      (should (equal method "POST"))
      (should (equal path "/api/memory/reset"))
      (should (equal body '((target . "user"))))
      (should (eq requested-client 'fake-client))
      (should done-called)
      (should refreshed))))

(ert-deftest hermes-memory-reset-cancel-skips-client-and-rest ()
  "Declining memory reset stops before client startup or REST calls."
  (let (with-client-called request-called)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (&rest _)
                 (setq with-client-called t)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq request-called t)
                 (hermes--promise-resolved nil))))
      (hermes-memory-reset "all")
      (should-not with-client-called)
      (should-not request-called))))

(ert-deftest hermes-memory-status-reports-rest-errors ()
  "Memory status reports REST errors."
  (let (message-text requested-client done-called)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest args)
                 (setq requested-client (plist-get args :client))
                 (hermes--promise-rejected "backend unavailable")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (hermes-memory-status)
      (should done-called)
      (should (eq requested-client 'fake-client))
      (should (equal message-text "Hermes: backend unavailable")))))

(provide 'hermes-inventory-tests)
;;; hermes-inventory-tests.el ends here
