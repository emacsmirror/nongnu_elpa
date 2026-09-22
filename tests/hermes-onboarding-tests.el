;;; hermes-onboarding-tests.el --- Tests for hermes-onboarding  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(require 'hermes-onboarding)

;;; Group 1: provider model (pure)

(defun hermes-onboarding-test--api-key-provider ()
  "Return a connectable API-key skeleton provider row."
  '((slug . "deepseek") (name . "DeepSeek")
    (auth_type . "api_key") (key_env . "DEEPSEEK_API_KEY")))

(ert-deftest hermes-onboarding-disconnecting-hides-stale-instructions ()
  "Disconnect hides retained sign-in URLs and codes, unlike a pending flow."
  (let ((details '((verification_url . "https://fixture.invalid/sign-in")
                   (user_code . "fixture-code"))))
    (let ((text (hermes-onboarding--oauth-status-text
                 (cons '(status . "disconnecting") details))))
      (should-not (string-match-p (rx (or "fixture.invalid" "fixture-code" "Sign in:")) text)))
    (let ((text (hermes-onboarding--oauth-status-text
                 (cons '(status . "pending") details))))
      (should (string-match-p "fixture.invalid" text)))))

(ert-deftest hermes-onboarding-unauthed-p-accepts-unauthed-rejects-authed ()
  (should (hermes-onboarding--unauthed-p
           (hermes-onboarding-test--api-key-provider)))
  (should-not (hermes-onboarding--unauthed-p
               '((slug . "openai") (name . "OpenAI") (authenticated . t)))))

(ert-deftest hermes-onboarding-unauthed-providers-offers-only-api-key-rows ()
  "Only unauthenticated rows classified as API-key providers are offered."
  (let ((result '((providers . (((slug . "openai") (authenticated . t))
                                ((slug . "deepseek") (auth_type . "api_key")
                                 (key_env . "DEEPSEEK_API_KEY"))
                                ((slug . "nous") (auth_type . "oauth_device_code")
                                 (key_env . "")))))))
    (should (equal (mapcar (lambda (p) (hermes-transport--get p 'slug))
                           (hermes-onboarding--unauthed-providers result))
                   '("deepseek")))))

;;; Group 2: interaction

(ert-deftest hermes-onboarding-choose-provider-errors-when-none-connectable ()
  (should-error (hermes-onboarding--choose-provider
                 '((providers . (((slug . "openai") (authenticated . t))))))
                :type 'user-error))

(ert-deftest hermes-onboarding-read-key-rejects-empty ()
  (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "")))
    (should-error (hermes-onboarding--read-key
                   (hermes-onboarding-test--api-key-provider))
                  :type 'user-error)))

;;; Group 3: connect flow

(ert-deftest hermes-onboarding-connect-provider-saves-the-chosen-key ()
  "The command fetches options, picks a provider, reads its key, and saves it."
  (let* (saved-slug saved-key connected
         (hermes-onboarding-auth-changed-function
          (lambda () (setq connected t)))
         (hermes-dashboard-transport--model-options-cache nil))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          (hermes-onboarding-test--api-key-provider-result))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (caar collection)))
              ((symbol-function 'read-passwd) (lambda (&rest _) "sk-secret"))
              ((symbol-function 'hermes-dashboard-transport-model-save-key)
               (lambda (_client slug key &rest args)
                 (setq saved-slug slug saved-key key)
                 (funcall (plist-get args :resolve)
                          '((provider . ((slug . "deepseek") (name . "DeepSeek")))))))
              ((symbol-function 'message) #'ignore))
      (hermes-onboarding-connect-provider)
      (should (equal saved-slug "deepseek"))
      (should (equal saved-key "sk-secret"))
      (should connected))))

(ert-deftest hermes-onboarding-oauth-rest-actions-use-profile-scoped-routes ()
  "OAuth actions use canonical routes, one profile, and secret code marking."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path
                             (plist-get args :body)
                             (plist-get args :secrets)
                             (plist-get args :query))
                       calls)
                 (hermes--promise-resolved '((ok . t))))))
      (with-temp-buffer
        (hermes-provider-accounts-mode)
        (setq hermes-onboarding--provider-account-profile "profile-b")
        (hermes-onboarding--provider-accounts-fetch 'client))
      (hermes-onboarding--oauth-start 'client "nous" "profile-b")
      (hermes-onboarding--oauth-poll 'client "nous" "sid" "profile-b")
      (hermes-onboarding--oauth-submit
       'client "anthropic" "sid" "code-secret" "profile-b")
      (hermes-onboarding--oauth-cancel 'client "sid" "profile-b")
      (hermes-onboarding--oauth-disconnect 'client "nous" "profile-b")
      (should (equal (nreverse calls)
                     '(("GET" "/api/providers/oauth" nil nil
                        ((profile . "profile-b")))
                       ("POST" "/api/providers/oauth/nous/start" nil nil
                        ((profile . "profile-b")))
                       ("GET" "/api/providers/oauth/nous/poll/sid" nil nil
                        ((profile . "profile-b")))
                       ("POST" "/api/providers/oauth/anthropic/submit"
                        ((session_id . "sid") (code . "code-secret"))
                        ("code-secret") ((profile . "profile-b")))
                       ("DELETE" "/api/providers/oauth/sessions/sid" nil nil
                        ((profile . "profile-b")))
                       ("DELETE" "/api/providers/oauth/nous" nil nil
                        ((profile . "profile-b")))))))))

(ert-deftest hermes-onboarding-oauth-success-routes-reject-ok-false ()
  "Start, poll, and submit cannot promote a semantic API failure."
  (let* ((provider '((id . "nous") (name . "Nous")))
         (changed 0)
         (invalidated 0)
         (refreshed 0)
         opened messages
         (hermes-onboarding-auth-changed-function
          (lambda () (setq changed (1+ changed)))))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((ok . nil) (status . "approved")
                    (message . "backend rejected operation")
                    (auth_url . "https://example.org/should-not-open")))))
              ((symbol-function 'hermes-dashboard-transport-invalidate-model-options)
               (lambda () (setq invalidated (1+ invalidated))))
              ((symbol-function 'hermes-provider-accounts--revert)
               (lambda () (setq refreshed (1+ refreshed))))
              ((symbol-function 'browse-url) (lambda (url) (setq opened url)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'read-passwd) (lambda (&rest _) "code"))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (hermes-provider-accounts-mode)
              (hermes-onboarding--oauth-start-provider provider))
            (dolist (command '(hermes-onboarding-oauth-poll
                               hermes-onboarding-oauth-submit))
              (with-temp-buffer
                (hermes-onboarding-oauth-mode)
                (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
                (setq hermes-onboarding-oauth--provider "nous"
                      hermes-onboarding-oauth--provider-name "Nous"
                      hermes-onboarding-oauth--session-id "sid")
                (funcall command)))
            (should (= changed 0))
            (should (= invalidated 0))
            (should (= refreshed 0))
            (should-not opened)
            (should (= (cl-count-if
                        (lambda (text)
                          (string-match-p "backend rejected operation" text))
                        messages)
                       3))
            (should-not (cl-some
                         (lambda (text) (string-match-p "connected" text))
                         messages)))
        (when (get-buffer "*Hermes OAuth*")
          (kill-buffer "*Hermes OAuth*"))))))

(ert-deftest hermes-onboarding-oauth-catalog-rejects-ok-false ()
  "A failed provider catalog cannot render or start a disconnect workflow."
  (let ((payload '((ok . nil) (message . "catalog unavailable")
                   (providers . (((id . "nous") (name . "Nous")
                                  (disconnectable . t)
                                  (status . ((logged_in . t))))))))
        (prompts 0)
        (deletes 0)
        (rendered 0)
        (shown 0)
        (applied 0)
        (changed 0)
        (refreshed 0)
        (revert (symbol-function 'hermes-provider-accounts--revert))
        opened messages)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method _path &rest _)
                 (if (string= method "GET")
                     (hermes--promise-resolved payload)
                   (setq deletes (1+ deletes))
                   (hermes--promise-resolved '((ok . t))))))
              ((symbol-function 'hermes-onboarding--provider-account-rows)
               (lambda (&rest _) (setq rendered (1+ rendered)) nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (setq prompts (1+ prompts)) "Nous"))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (setq prompts (1+ prompts)) t))
              ((symbol-function 'hermes-onboarding--show-oauth)
               (lambda (&rest _) (setq shown (1+ shown)) 'context))
              ((symbol-function 'hermes-onboarding--oauth-apply-result)
               (lambda (&rest _) (setq applied (1+ applied)) t))
              ((symbol-function 'hermes-onboarding--auth-changed)
               (lambda () (setq changed (1+ changed))))
              ((symbol-function 'hermes-provider-accounts--revert)
               (lambda (&rest args)
                 (setq refreshed (1+ refreshed))
                 (apply revert args)))
              ((symbol-function 'browse-url) (lambda (url) (setq opened url)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (with-temp-buffer
            (hermes-chat-mode)
            (setq hermes-chat--profile "profile-b")
            (hermes-onboarding-oauth-connect)
            (hermes-onboarding-oauth-disconnect-provider)
            (should (= prompts 0))
            (should (= deletes 0))
            (should (= rendered 0))
            (should (= shown 0))
            (should (= applied 0))
            (should (= changed 0))
            ;; Initial catalog acquisition uses the normal refresh route.
            ;; A rejected catalog must not trigger another auth refresh.
            (should (= refreshed 1))
            (should-not opened)
            (should (= (cl-count-if
                        (lambda (text)
                          (string-match-p "catalog unavailable" text))
                        messages)
                       2))
            (should-not (cl-some
                         (lambda (text) (string-match-p "disconnected" text))
                         messages)))
        (dolist (buffer '("*Hermes Provider Accounts*" "*Hermes OAuth*"))
          (when (get-buffer buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-onboarding-oauth-disconnect-keeps-origin-instance ()
  "A delayed provider lookup opens OAuth on its originating instance."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         (providers (hermes--promise-make))
         (origin (generate-new-buffer " *Hermes OAuth origin*"))
         disconnected-client oauth-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn 'remote-client #'ignore)))
                  ((symbol-function 'hermes-onboarding--oauth-providers)
                   (lambda (&rest _) providers))
                  ((symbol-function 'hermes-onboarding--oauth-disconnect)
                   (lambda (client &rest _)
                     (setq disconnected-client client)
                     (hermes--promise-resolved '((ok . t)))))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (when (string-prefix-p "Hermes instance" prompt)
                       (ert-fail "Unexpected instance prompt"))
                     (caar collection)))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer origin
            (hermes-chat-mode)
            (setq hermes-instance remote)
            (hermes-onboarding-oauth-disconnect-provider))
          (with-temp-buffer
            (hermes--promise-resolve
             providers
             '((providers . (((id . "nous") (name . "Nous")
                              (disconnectable . t)
                              (status . ((logged_in . t)))))))))
          (setq oauth-buffer (get-buffer "*Hermes OAuth@remote*"))
          (should (buffer-live-p oauth-buffer))
          (should (equal (buffer-local-value 'hermes-instance oauth-buffer)
                         remote))
          (should (eq disconnected-client 'remote-client)))
      (when (buffer-live-p origin) (kill-buffer origin))
      (when (buffer-live-p oauth-buffer) (kill-buffer oauth-buffer)))))

(ert-deftest hermes-onboarding-account-browser-captures-chat-profile ()
  "Account command carries its invoking chat's profile and instance into GET."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        query seen-instance)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (setq seen-instance hermes-instance)
                 (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (_method _path &rest args)
                 (setq query (plist-get args :query))
                 (hermes--promise-resolved '((providers . nil)))))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (hermes-chat-mode)
            (setq hermes-instance instance
                  hermes-chat--profile "profile-b")
            (hermes-onboarding-oauth-connect)
            (should (equal query '((profile . "profile-b"))))
            (should (equal seen-instance instance))
            (with-current-buffer "*Hermes Provider Accounts*"
              (should (equal hermes-instance instance))
              (should (equal hermes-onboarding--provider-account-profile
                             "profile-b"))))
        (when (get-buffer "*Hermes Provider Accounts*")
          (kill-buffer "*Hermes Provider Accounts*"))))))

(ert-deftest hermes-onboarding-oauth-native-flow-carries-profile-context ()
  "Native start and status actions retain the provider browser's profile."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-onboarding--show-oauth)
               (lambda (_provider _result &optional profile)
                 (push (list 'show profile) calls)
                 (hermes-onboarding-oauth-mode)
                 (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
                 (hermes-onboarding--oauth-context)))
              ((symbol-function 'hermes-onboarding--oauth-start)
               (lambda (client provider &optional profile)
                 (push (list 'start client provider profile) calls)
                 (hermes--promise-make)))
              ((symbol-function 'hermes-onboarding--oauth-poll)
               (lambda (client provider session &optional profile)
                 (push (list 'poll client provider session profile) calls)
                 (hermes--promise-make)))
              ((symbol-function 'hermes-onboarding--oauth-submit)
               (lambda (client provider session code &optional profile)
                 (push (list 'submit client provider session code profile) calls)
                 (hermes--promise-make)))
              ((symbol-function 'hermes-onboarding--oauth-cancel)
               (lambda (client session &optional profile)
                 (push (list 'cancel client session profile) calls)
                 (hermes--promise-make)))
              ((symbol-function 'hermes-onboarding--oauth-disconnect)
               (lambda (client provider &optional profile)
                 (push (list 'disconnect client provider profile) calls)
                 (hermes--promise-make)))
              ((symbol-function 'read-passwd) (lambda (&rest _) "code"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (hermes-provider-accounts-mode)
        (setq hermes-onboarding--provider-account-profile "profile-b")
        (hermes-onboarding--oauth-start-provider
         '((id . "anthropic") (name . "Anthropic"))))
      (with-temp-buffer
        (hermes-onboarding-oauth-mode)
        (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
        (setq hermes-onboarding-oauth--provider "anthropic"
              hermes-onboarding-oauth--provider-name "Anthropic"
              hermes-onboarding-oauth--session-id "sid"
              hermes-onboarding-oauth--profile "profile-b")
        (hermes-onboarding-oauth-poll)
        (hermes-onboarding-oauth-submit)
        (hermes-onboarding-oauth-cancel)
        (hermes-onboarding-oauth-disconnect))
      (should (equal (nreverse calls)
                     '((show "profile-b")
                       (start client "anthropic" "profile-b")
                       (poll client "anthropic" "sid" "profile-b")
                       (submit client "anthropic" "sid" "code" "profile-b")
                       (cancel client "sid" "profile-b")
                       (disconnect client "anthropic" "profile-b")))))))

(ert-deftest hermes-onboarding-provider-rows-show-every-api-provider ()
  "Account rows preserve API membership, order, names, status, and flow."
  (let* ((result '((providers . (((id . "already-set")
                                  (name . "Backend Supplied A")
                                  (flow . "external")
                                  (status . ((logged_in . t)
                                             (source_label . "Remote CLI"))))
                                 ((id . "future")
                                  (name . "Backend Supplied B")
                                  (flow . "future_flow")
                                  (status . ((logged_in . nil)
                                             (source . "future-source"))))))))
         (rows (hermes-onboarding--provider-account-rows result)))
    (should (equal (mapcar #'car rows) '("already-set" "future")))
    (should (equal (append (cadr (car rows)) nil)
                   '("Backend Supplied A" "Connected" "external" "Remote CLI")))
    (should (equal (append (cadr (cadr rows)) nil)
                   '("Backend Supplied B" "Available" "future_flow"
                     "future-source")))))

(ert-deftest hermes-onboarding-provider-external-action-uses-api-command ()
  "External account action copies the backend command without starting OAuth."
  (let ((provider '((id . "future") (name . "Backend Supplied")
                    (flow . "external") (cli_command . "backend auth future")
                    (status . ((logged_in . nil)))))
        copied started executed)
    (cl-letf (((symbol-function 'kill-new) (lambda (text) (setq copied text)))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'hermes-onboarding--oauth-start-provider)
               (lambda (&rest _) (setq started t)))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq executed t)))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq executed t)))
              ((symbol-function 'shell-command)
               (lambda (&rest _) (setq executed t)))
              ((symbol-function 'async-shell-command)
               (lambda (&rest _) (setq executed t))))
      (hermes-onboarding--provider-account-act provider)
      (should (equal copied "backend auth future"))
      (should-not started)
      (should-not executed))))

(ert-deftest hermes-onboarding-provider-error-status-is-not-connectable ()
  "A backend status error is visible and blocks account connection."
  (let* ((provider '((id . "broken") (name . "Broken Provider")
                     (flow . "device_code")
                     (status . ((logged_in . nil)
                                (error . "credential lookup failed")))))
         (entry (cadr (hermes-onboarding--provider-account-row provider))))
    (should (equal (append entry nil)
                   '("Broken Provider" "Error" "device_code"
                     "credential lookup failed")))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-error))
    (should-error (hermes-onboarding--provider-account-act provider)
                  :type 'user-error)))

(ert-deftest hermes-onboarding-provider-actions-follow-api-flow-metadata ()
  "Native flows start OAuth; future flows may fall back to API docs."
  (let ((native '((id . "native") (flow . "pkce")
                  (status . ((logged_in . nil)))))
        (future '((id . "future") (flow . "future_flow")
                  (docs_url . "https://example.org/future")
                  (status . ((logged_in . nil)))))
        started opened)
    (cl-letf (((symbol-function 'hermes-onboarding--oauth-start-provider)
               (lambda (provider) (setq started provider)))
              ((symbol-function 'browse-url) (lambda (url) (setq opened url))))
      (hermes-onboarding--provider-account-act native)
      (hermes-onboarding--provider-account-act future)
      (should (eq started native))
      (should (equal opened "https://example.org/future")))))

(ert-deftest hermes-onboarding-provider-external-disconnect-copies-command ()
  "External disconnect commands remain inert API-supplied text."
  (let ((provider '((id . "future") (name . "Backend Supplied")
                    (disconnectable . nil)
                    (disconnect_command . "backend auth logout")
                    (status . ((logged_in . t)))))
        copied native-disconnect executed)
    (cl-letf (((symbol-function 'hermes-onboarding--provider-account-at-point)
               (lambda () provider))
              ((symbol-function 'kill-new) (lambda (text) (setq copied text)))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (&rest _) (setq native-disconnect t)))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq executed t)))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq executed t)))
              ((symbol-function 'shell-command)
               (lambda (&rest _) (setq executed t)))
              ((symbol-function 'async-shell-command)
               (lambda (&rest _) (setq executed t))))
      (hermes-onboarding-provider-account-disconnect)
      (should (equal copied "backend auth logout"))
      (should-not native-disconnect)
      (should-not executed))))

(ert-deftest hermes-onboarding-provider-native-disconnect-refreshes-accounts ()
  "Confirmed native disconnect uses the API; declining has no effect."
  (let ((provider '((id . "native") (name . "Native")
                    (disconnectable . t)
                    (status . ((logged_in . t)))))
        confirmed requested changed refreshed)
    (cl-letf (((symbol-function 'hermes-onboarding--provider-account-at-point)
               (lambda () provider))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) confirmed))
              ((symbol-function 'hermes-onboarding--oauth-disconnect)
               (lambda (client id &optional profile)
                 (setq requested (list client id profile))))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise on-success &optional _on-error)
                 (funcall make-promise 'client)
                 (funcall on-success '((ok . t)))))
              ((symbol-function 'hermes-onboarding--auth-changed)
               (lambda () (setq changed t)))
              ((symbol-function 'hermes-provider-accounts--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-provider-accounts-mode)
        (setq hermes-onboarding--provider-account-profile "profile-b")
        (hermes-onboarding-provider-account-disconnect)
        (should-not requested)
        (should-not changed)
        (should-not refreshed)
        (setq confirmed t)
        (hermes-onboarding-provider-account-disconnect))
      (should (equal requested '(client "native" "profile-b")))
      (should changed)
      (should refreshed))))

(ert-deftest hermes-onboarding-provider-disconnect-rejects-ok-false ()
  "A 2xx response that cleared nothing cannot trigger success effects."
  (let ((provider '((id . "native") (name . "Native")
                    (disconnectable . t)
                    (status . ((logged_in . t)))))
        changed refreshed messages)
    (cl-letf (((symbol-function 'hermes-onboarding--provider-account-at-point)
               (lambda () provider))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((ok . nil) (message . "nothing was cleared")))))
              ((symbol-function 'hermes-onboarding--auth-changed)
               (lambda () (setq changed t)))
              ((symbol-function 'hermes-provider-accounts--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-provider-accounts-mode)
        (hermes-onboarding-provider-account-disconnect))
      (should-not changed)
      (should-not refreshed)
      (should (cl-some (lambda (text)
                         (string-match-p "nothing was cleared" text))
                       messages))
      (should-not (cl-some (lambda (text)
                             (string-match-p "disconnected provider" text))
                           messages)))))

(ert-deftest hermes-onboarding-standalone-disconnect-respects-owner ()
  "Standalone disconnect is silent after its invoking buffer loses ownership."
  (dolist (scenario '((current . resolve)
                      (kill . resolve) (kill . reject)
                      (supersede . resolve) (supersede . reject)))
    (let ((request (hermes--promise-make))
          (buffer (generate-new-buffer " *Hermes disconnect owner*"))
          (owner-state (car scenario))
          (settlement (cdr scenario))
          (prompts 0)
          (shown 0)
          (applied 0)
          (disconnects 0)
          (changed 0)
          (show (symbol-function 'hermes-onboarding--show-oauth))
          (apply-result (symbol-function 'hermes-onboarding--oauth-apply-result))
          view messages)
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (method _path &rest _)
                   (if (string= method "GET")
                       request
                     (setq disconnects (1+ disconnects))
                     (hermes--promise-resolved '((ok . t))))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq prompts (1+ prompts))
                   (caar collection)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq prompts (1+ prompts)) t))
                ((symbol-function 'hermes-onboarding--show-oauth)
                 (lambda (&rest args)
                   (setq shown (1+ shown))
                   (let ((context (apply show args)))
                     (setq view (plist-get context :buffer))
                     context)))
                ((symbol-function 'hermes-onboarding--oauth-apply-result)
                 (lambda (&rest args)
                   (setq applied (1+ applied))
                   (apply apply-result args)))
                ((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'hermes-onboarding--auth-changed)
                 (lambda () (setq changed (1+ changed))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-chat--profile "profile-b")
                (hermes-onboarding-oauth-disconnect-provider))
              (pcase owner-state
                ('kill (kill-buffer buffer))
                ('supersede
                 (with-current-buffer buffer
                   (hermes-browser--next-request-generation))))
              (setq messages nil)
              (if (eq settlement 'resolve)
                  (hermes--promise-resolve
                   request
                   '((providers . (((id . "nous") (name . "Nous")
                                    (disconnectable . t)
                                    (status . ((logged_in . t))))))))
                (hermes--promise-reject request "late list failure"))
              (if (eq owner-state 'current)
                  (progn
                    (should (= prompts 2))
                    (should (= shown 1))
                    (should (= applied 1))
                    (should (= disconnects 1))
                    (should (= changed 1))
                    (should (cl-some
                             (lambda (text)
                               (string-match-p "disconnected OAuth provider" text))
                             messages)))
                (should (= prompts 0))
                (should (= shown 0))
                (should (= applied 0))
                (should (= disconnects 0))
                (should (= changed 0))
                (should-not messages)))
          (when (buffer-live-p view) (kill-buffer view))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-onboarding-row-disconnect-respects-owner ()
  "Provider-row disconnect effects require the exact invoking generation."
  (dolist (scenario '((current . resolve)
                      (kill . resolve) (kill . reject)
                      (supersede . resolve) (supersede . reject)))
    (let ((request (hermes--promise-make))
          (buffer (generate-new-buffer " *Hermes row disconnect owner*"))
          (owner-state (car scenario))
          (settlement (cdr scenario))
          (provider '((id . "nous") (name . "Nous")
                      (disconnectable . t)
                      (status . ((logged_in . t)))))
          (changed 0)
          (refreshed 0)
          messages)
      (cl-letf (((symbol-function 'hermes-onboarding--provider-account-at-point)
                 (lambda () provider))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-onboarding--oauth-disconnect)
                 (lambda (&rest _) request))
                ((symbol-function 'hermes-onboarding--auth-changed)
                 (lambda () (setq changed (1+ changed))))
                ((symbol-function 'hermes-provider-accounts--revert)
                 (lambda () (setq refreshed (1+ refreshed))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (hermes-provider-accounts-mode)
                (hermes-onboarding-provider-account-disconnect))
              (pcase owner-state
                ('kill (kill-buffer buffer))
                ('supersede
                 (with-current-buffer buffer
                   (hermes-browser--next-request-generation))))
              (setq messages nil)
              (if (eq settlement 'resolve)
                  (hermes--promise-resolve request '((ok . t)))
                (hermes--promise-reject request "late disconnect failure"))
              (if (eq owner-state 'current)
                  (progn
                    (should (= changed 1))
                    (should (= refreshed 1))
                    (should (cl-some
                             (lambda (text)
                               (string-match-p "disconnected provider" text))
                             messages)))
                (should (= changed 0))
                (should (= refreshed 0))
                (should-not messages)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-onboarding-row-disconnect-prompt-respects-owner ()
  "A provider-row disconnect cannot outlive its confirmation prompt owner."
  (let ((provider '((id . "nous") (name . "Nous")
                    (disconnectable . t)
                    (status . ((logged_in . t)))))
        (requests 0)
        (changed 0)
        (refreshed 0)
        messages)
    (cl-letf (((symbol-function 'hermes-onboarding--provider-account-at-point)
               (lambda () provider))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (hermes-browser--next-request-generation)
                 t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (&rest _) (setq requests (1+ requests))))
              ((symbol-function 'hermes-onboarding--auth-changed)
               (lambda () (setq changed (1+ changed))))
              ((symbol-function 'hermes-provider-accounts--revert)
               (lambda () (setq refreshed (1+ refreshed))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-provider-accounts-mode)
        (hermes-onboarding-provider-account-disconnect))
      (should (= requests 0))
      (should (= changed 0))
      (should (= refreshed 0))
      (should-not messages))))

(ert-deftest hermes-onboarding-oauth-mode-exposes-session-actions ()
  "OAuth status buffers expose poll, submit, cancel, and disconnect commands."
  (dolist (binding '(("g" . hermes-onboarding-oauth-poll)
                     ("s" . hermes-onboarding-oauth-submit)
                     ("c" . hermes-onboarding-oauth-cancel)
                     ("d" . hermes-onboarding-oauth-disconnect)))
    (should (eq (keymap-lookup hermes-onboarding-oauth-mode-map (car binding))
                (cdr binding)))))

(defun hermes-onboarding-test--without-startup (fn)
  "Call FN with process and socket startup denied, even if errors are caught."
  (let* (attempted
         (deny (lambda (&rest _)
                 (setq attempted t)
                 (error "Unexpected process or socket startup"))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start) deny)
              ((symbol-function 'make-process) deny)
              ((symbol-function 'start-process) deny)
              ((symbol-function 'make-network-process) deny))
      (unwind-protect (funcall fn)
        (should-not attempted)))))

(ert-deftest hermes-onboarding-oauth-prompts-respect-captured-owner ()
  "Only a current prompt owner reaches acquisition and the real REST builder."
  (hermes-onboarding-test--without-startup
   (lambda ()
     (dolist (command '(hermes-onboarding-oauth-submit
                        hermes-onboarding-oauth-disconnect))
       (dolist (retire '(nil t))
         (with-temp-buffer
           (hermes-onboarding-oauth-mode)
           (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
           (setq hermes-onboarding-oauth--provider "old"
                 hermes-onboarding-oauth--provider-name "Old"
                 hermes-onboarding-oauth--session-id "old-session"
                 hermes-onboarding-oauth--profile "profile-b"
                 hermes-onboarding-oauth--result '((status . "old")))
           (let* ((hermes-instances nil)
                  (hermes-dashboard-transport-url "https://prompt.example")
                  (hermes-dashboard-transport-idle-close-delay nil)
                  (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
                  (prompts 0) (acquisitions 0) (changed 0) (rendered 0)
                  requests clients successor opened messages
                  (prompt
                   (lambda (&rest _)
                     (cl-incf prompts)
                     (when retire
                       (setq hermes-onboarding-oauth--provider "successor"
                             hermes-onboarding-oauth--provider-name "Successor"
                             hermes-onboarding-oauth--session-id "new-session"
                             hermes-onboarding-oauth--profile "profile-c"
                             hermes-onboarding-oauth--result '((status . "successor")))
                       (setq successor (hermes-onboarding--oauth-context)))
                     "secret-code")))
             (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                       ((symbol-function 'hermes-dashboard-transport-acquire)
                        (lambda (&rest _)
                          (cl-incf acquisitions)
                          (car (push (make-hermes-dashboard-transport-client
                                      :base-url "https://prompt.example"
                                      :token "test-token" :generation 1 :refcount 1)
                                     clients))))
                       ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                        (lambda (request)
                          (push request requests)
                          (hermes--promise-resolved
                           '(:status 200 :body ((status . "approved"))))))
                       ((symbol-function 'read-passwd) prompt)
                       ((symbol-function 'yes-or-no-p) prompt)
                       ((symbol-function 'hermes-onboarding--auth-changed)
                        (lambda () (cl-incf changed)))
                       ((symbol-function 'hermes-onboarding-oauth--render)
                        (lambda () (cl-incf rendered)))
                       ((symbol-function 'browse-url) (lambda (url) (setq opened url)))
                       ((symbol-function 'message)
                        (lambda (format-string &rest args)
                          (push (apply #'format format-string args) messages))))
               (call-interactively command)
               (should (= prompts 1))
               (should (= acquisitions (if retire 0 1)))
               (should (= (length requests) (if retire 0 1)))
               (should (= changed (if retire 0 1)))
               (should (= rendered (if retire 0 1)))
               (if retire
                   (progn
                     (should (hermes-onboarding--oauth-context-current-p successor))
                     (should (equal hermes-onboarding-oauth--provider "successor"))
                     (should (equal hermes-onboarding-oauth--session-id "new-session"))
                     (should (equal hermes-onboarding-oauth--profile "profile-c"))
                     (should (equal hermes-onboarding-oauth--result
                                    '((status . "successor")))))
                 (let ((submit (eq command 'hermes-onboarding-oauth-submit))
                       (request (car requests)))
                   (should (equal (plist-get request :method)
                                  (if submit "POST" "DELETE")))
                   (should (equal (plist-get request :url)
                                  (concat "https://prompt.example/api/providers/oauth/old"
                                          (if submit "/submit" "") "?profile=profile-b")))
                   (when submit
                     (should (equal (plist-get request :body)
                                    '((session_id . "old-session") (code . "secret-code"))))))
                 (should (= (hermes-dashboard-transport-client-refcount (car clients)) 0))
                 (should (hermes-dashboard-transport-client-stopping-p (car clients))))
               (should-not opened)
               (should-not messages)))))))))

(ert-deftest hermes-onboarding-oauth-mode-reset-does-not-reuse-owner ()
  "A mode reset cannot let an old OAuth resolution or rejection win an ABA race."
  (dolist (settlement '(resolve reject))
    (let ((first (hermes--promise-make))
          (second (hermes--promise-make))
          (requests 0)
          (changed 0)
          opened messages successor-result successor-text retired successor
          (provider '((id . "nous") (name . "Nous"))))
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-onboarding--oauth-start)
                 (lambda (&rest _)
                   (setq requests (1+ requests))
                   (if (= requests 1) first second)))
                ((symbol-function 'hermes-onboarding--auth-changed)
                 (lambda () (setq changed (1+ changed))))
                ((symbol-function 'pop-to-buffer) (lambda (buffer) (setq successor buffer)))
                ((symbol-function 'browse-url) (lambda (url) (setq opened url)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (unwind-protect
            (progn
              (with-temp-buffer
                (hermes-provider-accounts-mode)
                (setq hermes-onboarding--provider-account-profile "profile-b")
                (hermes-onboarding--oauth-start-provider provider))
              (setq retired successor)
              (with-current-buffer retired
                (fundamental-mode)
                (hermes-onboarding-oauth-mode)
                (setq hermes-onboarding-oauth--profile "profile-b")
                (hermes-onboarding--oauth-start-provider provider))
              (should-not (eq retired successor))
              (with-current-buffer successor
                (setq successor-result
                      (copy-tree hermes-onboarding-oauth--result)
                      successor-text (buffer-string)))
              (setq messages nil)
              (if (eq settlement 'resolve)
                  (hermes--promise-resolve
                   first '((status . "approved")
                           (auth_url . "https://example.org/stale")))
                (hermes--promise-reject first "stale OAuth failure"))
              (with-current-buffer successor
                (should (equal hermes-onboarding-oauth--result successor-result))
                (should (equal (buffer-string) successor-text)))
              (should (= changed 0))
              (should-not opened)
              (should-not messages)
              (hermes--promise-resolve second '((status . "approved")))
              (with-current-buffer successor
                (should (equal
                         (hermes-transport--display-field
                          hermes-onboarding-oauth--result 'status)
                         "approved")))
              (should (= changed 1)))
          (dolist (buffer (list retired successor))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-onboarding-oauth-status-omits-secret-fields ()
  "OAuth status text renders useful state without arbitrary secret fields."
  (let ((text (hermes-onboarding--oauth-status-text
               '((status . "pending") (user_code . "ABCD-EFGH")
                 (code . "secret-auth-code") (access_token . "secret-token")))))
    (should (string-match-p "Status: pending" text))
    (should (string-match-p "User code: ABCD-EFGH" text))
    (should-not (string-match-p "secret" text))))

(ert-deftest hermes-onboarding-oauth-error-message-precedence ()
  "Only explicit errors or failed results expose the fallback message."
  (dolist (case '((nil nil)
                  (((message . "fallback")) nil)
                  (((ok . t) (message . "fallback")) nil)
                  (((ok . nil) (message . "fallback")) "fallback")
                  (((ok . :false) (message . "fallback")) "fallback")
                  (((status . "error") (message . "fallback")) "fallback")
                  (((ok . nil) (error_message . " \t") (message . "fallback"))
                   "fallback")
                  (((ok . t) (error_message . "explicit") (message . "fallback"))
                   "explicit")
                  (((ok . nil) (message . " \t")) nil)
                  (((access_token . "secret") (code . "secret")) nil)))
    (let* ((result (car case))
           (before (copy-tree result))
           (expected (cadr case)))
      (should (equal (hermes-onboarding--oauth-error-message result) expected))
      (should (equal result before)))))

(ert-deftest hermes-onboarding-oauth-status-shows-backend-error-message ()
  "A failed PKCE response renders the backend's actionable message."
  (let ((text (hermes-onboarding--oauth-status-text
               '((ok . nil) (status . "error")
                 (message . "authorization code expired")))))
    (should (string-match-p "Error: authorization code expired" text))))

(ert-deftest hermes-onboarding-oauth-stale-rejections-are-silent ()
  "Replaced or killed OAuth owners cannot report late failures."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        messages
        (buffer (generate-new-buffer " *Hermes OAuth stale test*")))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-onboarding--oauth-poll)
               (lambda (&rest _)
                 (setq requests (1+ requests))
                 (if (= requests 1) first second)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (hermes-onboarding-oauth-mode)
              (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
              (setq hermes-onboarding-oauth--provider "nous"
                    hermes-onboarding-oauth--provider-name "Nous"
                    hermes-onboarding-oauth--session-id "sid"
                    hermes-onboarding-oauth--profile "profile-b")
              (hermes-onboarding-oauth-poll)
              (hermes-onboarding-oauth-poll))
            (hermes--promise-reject first "superseded failure")
            (kill-buffer buffer)
            (hermes--promise-reject second "orphaned failure")
            (should-not messages))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-onboarding-oauth-stale-result-cannot-replace-new-flow ()
  "An older callback for the same provider cannot overwrite newer state."
  (let ((provider '((id . "nous") (name . "Nous"))) first second)
    (unwind-protect
        (save-window-excursion
          (setq first
                (hermes-onboarding--show-oauth
                 provider '((status . "first-starting"))))
          (setq second
                (hermes-onboarding--show-oauth
                 provider '((status . "second-starting"))))
          (should-not
           (hermes-onboarding--oauth-apply-result
            first '((status . "stale-approved"))))
          (with-current-buffer "*Hermes OAuth*"
            (should (string-match-p "second-starting" (buffer-string)))
            (should-not (string-match-p "stale-approved" (buffer-string))))
          (should
           (hermes-onboarding--oauth-apply-result
            second '((status . "current-approved"))))
          (with-current-buffer "*Hermes OAuth*"
            (should (string-match-p "current-approved" (buffer-string)))))
      (when (get-buffer "*Hermes OAuth*")
        (kill-buffer "*Hermes OAuth*")))))

(ert-deftest hermes-onboarding-oauth-auth-changes-refresh-provider-state ()
  "OAuth approval and disconnect report both changes without real startup."
  (hermes-onboarding-test--without-startup
   (lambda ()
     (let ((hermes-instances nil)
           (hermes-dashboard-transport-url "https://refresh.example")
           (hermes-dashboard-transport-idle-close-delay nil)
           (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
           (changed 0) clients requests)
       (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                 ((symbol-function 'hermes-dashboard-transport-acquire)
                  (lambda (&rest _)
                    (car (push (make-hermes-dashboard-transport-client
                                :base-url "https://refresh.example"
                                :token "test-token" :generation 1 :refcount 1)
                               clients))))
                 ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                  (lambda (request)
                    (push request requests)
                    (hermes--promise-resolved
                     '(:status 200 :body ((status . "approved") (ok . t))))))
                 ((symbol-function 'hermes-onboarding--auth-changed)
                  (lambda () (cl-incf changed)))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (with-temp-buffer
           (hermes-onboarding-oauth-mode)
           (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
           (setq hermes-onboarding-oauth--provider "nous"
                 hermes-onboarding-oauth--provider-name "Nous"
                 hermes-onboarding-oauth--session-id "sid")
           (hermes-onboarding-oauth-poll)
           (hermes-onboarding-oauth-disconnect)
           (should (= changed 2))
           (should (= (length clients) 2))
           (should (equal (mapcar (lambda (request)
                                   (list (plist-get request :method)
                                         (plist-get request :url)))
                                 (reverse requests))
                          '(("GET" "https://refresh.example/api/providers/oauth/nous/poll/sid")
                            ("DELETE" "https://refresh.example/api/providers/oauth/nous"))))
           (dolist (client clients)
             (should (= (hermes-dashboard-transport-client-refcount client) 0))
             (should (hermes-dashboard-transport-client-stopping-p client)))))))))

(defun hermes-onboarding-test--api-key-provider-result ()
  "Return a `model.options' result carrying one connectable provider."
  `((providers . (,(hermes-onboarding-test--api-key-provider)))))


(ert-deftest hermes-onboarding-oauth-poll-preserves-device-code-fields ()
  "Poll results keep the device user code from the start payload."
  (let* ((start '((session_id . "s1")
                  (status . "pending")
                  (user_code . "ABCD-EFGH")
                  (verification_url . "https://example.org/device")
                  (flow . "device")))
         (poll '((session_id . "s1") (status . "pending")))
         (merged (hermes-onboarding--oauth-merge-result start poll)))
    (should (equal (hermes-transport--display-field merged 'user_code)
                   "ABCD-EFGH"))
    (should (equal (hermes-transport--display-field merged 'verification_url)
                   "https://example.org/device"))
    (should (equal (hermes-transport--display-field merged 'status) "pending"))))

;;; Owned command and recovery regressions

(ert-deftest hermes-onboarding-key-delayed-lookup-retires-owner ()
  "Late catalogue replies cannot prompt or save for a retired origin."
  (dolist (retirement '(current kill mode replace instance client rejection))
    (let* ((origin (generate-new-buffer " *key owner*"))
           (request (hermes--promise-make))
           (client (make-hermes-dashboard-transport-client :generation 1))
           (prompts 0) (changed 0) saved messages)
      (unwind-protect
          (cl-letf (((symbol-function 'hermes-browser--with-client)
                     (lambda (fn) (funcall fn client #'ignore)))
                    ((symbol-function 'hermes-dashboard-transport-model-options-cached)
                     (lambda (_client &rest args)
                       (hermes--promise-subscribe
                        request (plist-get args :resolve) (plist-get args :reject))))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (cl-incf prompts) (caar collection)))
                    ((symbol-function 'read-passwd)
                     (lambda (&rest _) (cl-incf prompts) " literal-key "))
                    ((symbol-function 'hermes-dashboard-transport-model-save-key)
                     (lambda (owner slug key &rest args)
                       (setq saved (list owner slug key))
                       (funcall (plist-get args :resolve)
                                '((provider . ((name . "DeepSeek")))))))
                    ((symbol-function 'hermes-onboarding--auth-changed)
                     (lambda () (cl-incf changed)))
                    ((symbol-function 'message)
                     (lambda (&rest args) (push args messages))))
            (with-current-buffer origin
              (hermes-onboarding-connect-provider)
              (pcase retirement
                ((or 'kill 'rejection) (kill-buffer origin))
                ('mode (text-mode))
                ('replace (hermes-browser--next-request-generation))
                ('instance (setq-local hermes-instance '("other" . "https://other.test")))
                ('client (cl-incf (hermes-dashboard-transport-client-generation client)))))
            (with-temp-buffer
              (if (eq retirement 'rejection)
                  (hermes--promise-reject request "Late catalogue failure")
                (hermes--promise-resolve
                 request (hermes-onboarding-test--api-key-provider-result))))
            (if (eq retirement 'current)
                (progn
                  (should (= prompts 2))
                  (should (equal saved (list client "deepseek" " literal-key ")))
                  (should (= changed 1)))
              (should (= prompts 0))
              (should-not saved)
              (should (= changed 0))
              (should-not messages)))
        (when (buffer-live-p origin) (kill-buffer origin))))))

(ert-deftest hermes-onboarding-key-recursive-prompts-retire-owner ()
  "Provider and secret prompts must not lend consent to a replaced view."
  (dolist (stage '(provider secret))
    (with-temp-buffer
      (let ((origin (current-buffer)) (prompts 0)
            (lookup (hermes--promise-make)) saved)
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn 'client #'ignore)))
                  ((symbol-function 'hermes-dashboard-transport-model-options-cached)
                   (lambda (_client &rest args)
                     (hermes--promise-subscribe
                      lookup (plist-get args :resolve) (plist-get args :reject))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (when (eq stage 'provider)
                       (with-current-buffer origin
                         (hermes-browser--next-request-generation)))
                     (caar collection)))
                  ((symbol-function 'read-passwd)
                   (lambda (&rest _)
                     (cl-incf prompts)
                     (with-current-buffer origin
                       (hermes-browser--next-request-generation))
                     "synthetic-key"))
                  ((symbol-function 'hermes-dashboard-transport-model-save-key)
                   (lambda (&rest _) (setq saved t))))
          (hermes-onboarding-connect-provider)
          (with-temp-buffer
            (hermes--promise-resolve
             lookup (hermes-onboarding-test--api-key-provider-result)))
          (should (= prompts (if (eq stage 'secret) 1 0)))
          (should-not saved))))))

(ert-deftest hermes-onboarding-key-save-fences-readiness ()
  "The real save RPC cannot send after its delayed readiness owner retires."
  (dolist (retire '(nil t))
    (with-temp-buffer
      (let* ((ready (hermes--promise-make))
             (lookup (hermes--promise-make))
             (client (make-hermes-dashboard-transport-client
                      :generation 1 :ready-promise ready))
             (hermes-dashboard-transport-request-timeout nil)
             frames
             (hermes-dashboard-transport-websocket-send-function
              (lambda (_socket frame) (push frame frames))))
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn client #'ignore)))
                  ((symbol-function 'hermes-dashboard-transport-model-options-cached)
                   (lambda (_client &rest args)
                     (hermes--promise-subscribe
                      lookup (plist-get args :resolve) (plist-get args :reject))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _) (caar collection)))
                  ((symbol-function 'read-passwd) (lambda (&rest _) "literal-key")))
          (hermes-onboarding-connect-provider)
          (hermes--promise-resolve lookup
                                   (hermes-onboarding-test--api-key-provider-result))
          (should-not frames)
          (when retire (hermes-browser--next-request-generation))
          (hermes--promise-resolve ready t)
          (if retire (should-not frames)
            (should (= (length frames) 1))
            (let* ((frame (json-parse-string (car frames) :object-type 'alist))
                   (params (alist-get 'params frame)))
              (should (equal (alist-get 'method frame) "model.save_key"))
              (should (equal (alist-get 'slug params) "deepseek"))
              (should (equal (alist-get 'api_key params) "literal-key")))))))))

(ert-deftest hermes-onboarding-oauth-start-failure-has-working-retry ()
  "A public account action visibly fails, then its retry key starts anew."
  (let ((provider '((id . "nous") (name . "Nous") (flow . "device_code")))
        (requests 0) opened)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn 'client #'ignore)))
                  ((symbol-function 'hermes-onboarding--provider-account-at-point)
                   (lambda () provider))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (_method path &rest _)
                     (should (equal path "/api/providers/oauth/nous/start"))
                     (if (= (cl-incf requests) 1)
                         (hermes--promise-rejected "Start unavailable")
                       (hermes--promise-resolved
                        '((status . "pending") (session_id . "new-flow")
                          (user_code . "ABCD")
                          (verification_url . "https://example.test/device"))))))
                  ((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'browse-url) (lambda (url) (push url opened)))
                  ((symbol-function 'message) #'ignore))
          (with-temp-buffer
            (hermes-provider-accounts-mode)
            (call-interactively #'hermes-onboarding-provider-account-act))
          (with-current-buffer "*Hermes OAuth*"
            (should (string-match-p "Status: error" (buffer-string)))
            (should (string-match-p "Start unavailable" (buffer-string)))
            (should-not (string-match-p "g poll" (buffer-string)))
            (should (string-match-p "r retry start" (buffer-string)))
            (call-interactively (key-binding (kbd "r")))
            (should (equal hermes-onboarding-oauth--session-id "new-flow"))
            (should (string-match-p "Status: pending" (buffer-string)))
            (should (string-match-p "User code: ABCD" (buffer-string))))
          (should (= requests 2))
          (should (equal opened '("https://example.test/device"))))
      (when (get-buffer "*Hermes OAuth*") (kill-buffer "*Hermes OAuth*")))))

(ert-deftest hermes-onboarding-oauth-cancel-failure-and-retry-settle-visibly ()
  "Cancel failure retains the handle; retry clears obsolete instructions."
  (let ((calls 0))
    (with-temp-buffer
      (hermes-onboarding-oauth-mode)
      (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
      (setq hermes-onboarding-oauth--provider "nous"
            hermes-onboarding-oauth--session-id "flow"
            hermes-onboarding-oauth--result
            '((status . "pending") (user_code . "ABCD")
              (verification_url . "https://example.test/device")))
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (method path &rest _)
                   (should (equal method "DELETE"))
                   (should (equal path "/api/providers/oauth/sessions/flow"))
                   (if (= (cl-incf calls) 1)
                       (hermes--promise-rejected "Cancel unavailable")
                     (hermes--promise-resolved '((ok . t) (session_id . "flow"))))))
                ((symbol-function 'message) #'ignore))
        (call-interactively (key-binding (kbd "c")))
        (should (string-match-p "Status: error" (buffer-string)))
        (should (string-match-p "c cancel" (buffer-string)))
        (should (equal hermes-onboarding-oauth--session-id "flow"))
        (call-interactively (key-binding (kbd "c")))
        (should-not hermes-onboarding-oauth--session-id)
        (should (string-match-p "Status: cancelled" (buffer-string)))
        (should-not (string-match-p "Sign in:\\|User code:\\|g poll\\|c cancel" (buffer-string)))
        (should (string-match-p "r retry start" (buffer-string)))
        (should (= calls 2))))))

(ert-deftest hermes-onboarding-oauth-cancel-fences-authentication ()
  "Deferred REST authentication cannot cancel a replaced OAuth flow."
  (dolist (replace '(t nil))
    (with-temp-buffer
      (hermes-onboarding-oauth-mode)
      (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
      (setq hermes-onboarding-oauth--provider "nous"
            hermes-onboarding-oauth--session-id "old-flow"
            hermes-onboarding-oauth--profile "test-profile")
      (let ((auth (hermes--promise-make)) requests)
        (cl-letf (((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn 'client #'ignore)))
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _) auth))
                  ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                   (lambda (request)
                     (push request requests)
                     (hermes--promise-resolved
                      '(:status 200 :body ((ok . t) (session_id . "old-flow")))))))
          (call-interactively #'hermes-onboarding-oauth-cancel)
          (should-not requests)
          (when replace
            (setq hermes-onboarding-oauth--session-id "new-flow")
            (hermes-onboarding--oauth-context))
          (hermes--promise-resolve auth '(:base-url "https://example.test"))
          (if replace
              (progn
                (should-not requests)
                (should (equal hermes-onboarding-oauth--session-id "new-flow")))
            (progn
            (should (= (length requests) 1))
            (should (equal (plist-get (car requests) :url)
                           "https://example.test/api/providers/oauth/sessions/old-flow?profile=test-profile"))
            (should (string-match-p "Status: cancelled" (buffer-string))))))))))

(ert-deftest hermes-onboarding-oauth-approval-and-disconnect-show-terminal-state ()
  "Public poll and disconnect remove stale invitations and expose recovery."
  (with-temp-buffer
    (hermes-onboarding-oauth-mode)
    (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
    (setq hermes-onboarding-oauth--provider "nous"
          hermes-onboarding-oauth--session-id "flow"
          hermes-onboarding-oauth--result
          '((status . "pending") (user_code . "ABCD")
            (verification_url . "https://example.test/device")))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method _path &rest _)
                 (hermes--promise-resolved
                  (if (equal method "GET") '((status . "approved"))
                    '((ok . t))))))
              ((symbol-function 'hermes-onboarding--auth-changed) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (call-interactively (key-binding (kbd "g")))
      (should (string-match-p "Status: approved" (buffer-string)))
      (should (string-match-p "d disconnect" (buffer-string)))
      (should-not (string-match-p "Sign in:\\|User code:\\|g poll" (buffer-string)))
      (call-interactively (key-binding (kbd "d")))
      (should (string-match-p "Status: disconnected" (buffer-string)))
      (should (string-match-p "a accounts" (buffer-string)))
      (should-not hermes-onboarding-oauth--session-id))))

;;; Real acquisition and wire boundaries

(ert-deftest hermes-onboarding-key-public-acquisition-keeps-source-owner ()
  "Resolve endpoints without shadowing owners across prompts and RPCs."
  (dolist (setup '(legacy default owned named))
    (dolist (retire (if (eq setup 'named)
                        '(nil instance provider secret)
                      '(nil provider secret)))
      (with-temp-buffer
        (let* ((origin (current-buffer))
               (endpoint '("chosen" . "https://chosen.example"))
               (hermes-instances
                (pcase setup
                  ('legacy nil)
                  ('named (list '("other" . "https://other.example") endpoint))
                  (_ (list endpoint))))
               (hermes-dashboard-transport-url "https://chosen.example")
               (client (make-hermes-dashboard-transport-client
                        :generation 1 :base-url "https://chosen.example"
                        :ready-promise (hermes--promise-resolved t)))
               (hermes-dashboard-transport--model-options-cache nil)
               (hermes-dashboard-transport-request-timeout nil)
               frames
               (hermes-dashboard-transport-websocket-send-function
                (lambda (_socket text)
                  (push (json-parse-string text :object-type 'alist) frames)))
               (completing-read-function
                (lambda (prompt _collection &rest _)
                  (let ((selection (equal prompt "Hermes instance: ")))
                    (when (eq retire (if selection 'instance 'provider))
                      (with-current-buffer origin
                        (setq-local hermes-instance
                                    '("replacement" . "https://other.example"))))
                    (if selection "chosen" "DeepSeek"))))
               (releases 0) (secrets 0) acquired)
          (when (eq setup 'owned) (setq-local hermes-instance endpoint))
          (let ((source hermes-instance))
            (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                      ((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest _)
                         (setq acquired hermes-dashboard-transport-url)
                         client))
                      ((symbol-function 'hermes-dashboard-transport-release)
                       (lambda (owner)
                         (should (eq owner client))
                         (cl-incf releases)))
                      ((symbol-function 'read-passwd)
                       (lambda (&rest _)
                         (cl-incf secrets)
                         (when (eq retire 'secret)
                           (with-current-buffer origin
                             (setq-local hermes-instance
                                         '("replacement" . "https://other.example"))))
                         " literal-key ")))
              (call-interactively #'hermes-onboarding-connect-provider)
              (should (equal acquired "https://chosen.example"))
              (if (and (eq setup 'named) (eq retire 'instance))
                  (should-not frames)
                (should (equal (alist-get 'method (car frames)) "model.options"))
                (hermes-dashboard-transport--handle-frame
                 client `((jsonrpc . "2.0") (id . ,(alist-get 'id (car frames)))
                          (result . ,(hermes-onboarding-test--api-key-provider-result))))
                (if (memq retire '(provider secret))
                    (progn
                      (should (= (length frames) 1))
                      (should (= secrets (if (eq retire 'secret) 1 0))))
                  (should (= (length frames) 2))
                  (should (= secrets 1))
                  (should (eq hermes-instance source))
                  (should (equal (alist-get 'method (car frames)) "model.save_key"))
                  (should (equal (alist-get 'params (car frames))
                                 '((slug . "deepseek") (api_key . " literal-key "))))
                  (hermes-dashboard-transport--handle-frame
                   client `((jsonrpc . "2.0") (id . ,(alist-get 'id (car frames)))
                            (result . ((provider . ((name . "DeepSeek")))))))))
              (should (= releases 1)))))))))

(ert-deftest hermes-onboarding-oauth-public-acquisition-retries-legacy-and-named ()
  "Public RET and retry retain source identity and use the acquired REST URL."
  (dolist (setup '(legacy default owned named))
    (let* ((endpoint '("chosen" . "https://chosen.example"))
           (hermes-instances
            (pcase setup
              ('legacy nil)
              ('named (list '("other" . "https://other.example") endpoint))
              (_ (list endpoint))))
           (hermes-dashboard-transport-url "https://chosen.example")
           (completing-read-function (lambda (&rest _) "chosen"))
           (hermes-dashboard-transport-idle-close-delay nil)
           (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
           (release (symbol-function 'hermes-dashboard-transport-release))
           (provider '((id . "nous") (name . "Nous") (flow . "device_code")))
           (releases 0) requests opened buffer)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                      ((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest _)
                         (should (equal hermes-dashboard-transport-url
                                        "https://chosen.example"))
                         (make-hermes-dashboard-transport-client
                          :generation 1 :refcount 1
                          :base-url "https://chosen.example"
                          :token "synthetic-session-token")))
                      ((symbol-function 'hermes-dashboard-transport-release)
                       (lambda (client)
                         (cl-incf releases)
                         (funcall release client)
                         (should (hermes-dashboard-transport-client-stopping-p client))))
                      ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                       (lambda (request)
                         (push request requests)
                         (if (= (length requests) 1)
                             (hermes--promise-rejected "Start unavailable")
                           (hermes--promise-resolved
                            '(:status 200 :body
                              ((status . "pending") (session_id . "new-flow")
                               (verification_url . "https://chosen.example/verify")))))))
                      ((symbol-function 'browse-url) (lambda (url) (push url opened))))
              (with-temp-buffer
                (hermes-provider-accounts-mode)
                (when (eq setup 'owned) (setq-local hermes-instance endpoint))
                (setq hermes-onboarding--provider-account-profile "profile-a"
                      hermes-onboarding--provider-account-result
                      (list (cons 'providers (list provider)))
                      tabulated-list-entries
                      (hermes-onboarding--provider-account-rows
                       hermes-onboarding--provider-account-result))
                (tabulated-list-print)
                (goto-char (point-min))
                (call-interactively (key-binding (kbd "RET"))))
              (setq buffer (get-buffer (hermes-onboarding--oauth-buffer-name endpoint)))
              (with-current-buffer buffer
                (should (string-match-p "Status: error" (buffer-string)))
                (should (string-match-p "Start unavailable" (buffer-string)))
                (should (string-match-p "r retry start" (buffer-string)))
                (call-interactively (key-binding (kbd "r")))
                (should (equal hermes-onboarding-oauth--session-id "new-flow"))
                (should (equal hermes-onboarding-oauth--profile "profile-a"))
                (should (string-match-p "Status: pending" (buffer-string))))
              (should (= releases 2))
              (should (= (length requests) 2))
              (dolist (request requests)
                (should (equal (plist-get request :method) "POST"))
                (should (equal (plist-get request :url)
                               "https://chosen.example/api/providers/oauth/nous/start?profile=profile-a")))
              (should (equal opened '("https://chosen.example/verify")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))


(defun hermes-onboarding-test--lease (key references rejection &optional retire fault)
  "Exercise KEY with REFERENCES and REJECTION, optionally RETIRE or FAULT.
Keep acquisition wrapping, REST construction, release and stop real.  Only
external acquisition, HTTP delivery, input and browser launch are controlled."
  (let* ((hermes-instances nil)
         (hermes-dashboard-transport-url "https://lease.example")
         (hermes-dashboard-transport-idle-close-delay nil)
         (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (client (make-hermes-dashboard-transport-client
                  :base-url "https://lease.example" :token "test-token"
                  :refcount references :generation 1))
         (response (hermes--promise-make))
         (release (symbol-function 'hermes-dashboard-transport-release))
         (report (symbol-function 'hermes-onboarding--oauth-report-error))
         (releases 0) (changed 0) requests opened buffer before)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                    ((symbol-function 'hermes-dashboard-transport-acquire)
                     (lambda (&rest _) client))
                    ((symbol-function 'hermes-dashboard-transport-release)
                     (lambda (owner)
                       (should (eq owner client))
                       (cl-incf releases)
                       (funcall release owner)))
                    ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                     (lambda (request)
                       (push request requests)
                       (if (eq retire 'dispatch-fault) (signal fault nil)
                         response)))
                    ((symbol-function 'browse-url)
                     (lambda (url)
                       (should (= releases 0))
                       (should (= (hermes-dashboard-transport-client-generation client) 1))
                       (when fault (signal fault nil))
                       (push url opened)))
                    ((symbol-function 'hermes-onboarding--auth-changed)
                     (lambda () (cl-incf changed)))
                    ((symbol-function 'hermes-onboarding--oauth-report-error)
                     (lambda (context reason)
                       (should (= releases 0))
                       (when (eq retire 'report-fault) (signal fault nil))
                       (funcall report context reason)))
                    ((symbol-function 'read-passwd) (lambda (&rest _) " literal-code "))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (setq buffer (hermes-buffer--get " *OAuth lease*" #'hermes-onboarding-oauth-mode))
            (with-current-buffer buffer
              (setq hermes-onboarding-oauth--provider "nous"
                    hermes-onboarding-oauth--provider-name "Nous"
                    hermes-onboarding-oauth--profile "profile-a"
                    hermes-onboarding-oauth--session-id "exact-flow"
                    hermes-onboarding-oauth--result '((status . "pending")))
              ;; Retry exercises the public start path without a named-buffer collision.
              (cl-letf (((symbol-function 'hermes-onboarding--oauth-buffer-name)
                         (lambda (_) (buffer-name buffer))))
                (when (equal key "r")
                  (setq hermes-onboarding-oauth--session-id nil
                        hermes-onboarding-oauth--result '((status . "error"))))
                (call-interactively (key-binding (kbd key))))
              (should (= (length requests) 1))
              (let* ((request (car requests))
                     (route (pcase key
                              ("r" '("POST" . "nous/start"))
                              ("g" '("GET" . "nous/poll/exact-flow"))
                              ("s" '("POST" . "nous/submit"))
                              ("c" '("DELETE" . "sessions/exact-flow"))
                              ("d" '("DELETE" . "nous")))))
                (should (equal (plist-get request :method) (car route)))
                (should (equal (plist-get request :url)
                               (concat "https://lease.example/api/providers/oauth/"
                                       (cdr route) "?profile=profile-a")))
                (when (equal key "s")
                  (should (equal (plist-get request :body)
                                 '((session_id . "exact-flow")
                                   (code . " literal-code "))))))
              (pcase retire
                ('stop (hermes-dashboard-transport-stop client))
                ('mode (fundamental-mode) (setq buffer-read-only nil)
                       (insert "successor draft"))
                ('kill (kill-buffer buffer)))
              (when (buffer-live-p buffer) (setq before (buffer-string)))
              (if rejection
                  (hermes--promise-reject response "Lease request failed")
                (hermes--promise-resolve
                 response '(:status 200 :body
                            ((status . "pending") (session_id . "exact-flow")
                             (verification_url . "https://lease.example/verify")))))
              (should (= releases 1))
              ;; A duplicate response must not release somebody else's lease.
              (hermes--promise-reject response "Duplicate")
              (should (= releases 1))
              (if (memq retire '(stop mode kill))
                  (progn
                    (when (buffer-live-p buffer) (should (equal before (buffer-string))))
                    (should-not opened)
                    (should (= changed 0)))
                (unless (eq retire 'report-fault)
                  (should (equal (hermes-transport--get hermes-onboarding-oauth--result 'status)
                                 (cond ((or rejection (eq retire 'dispatch-fault)) "error")
                                       ((equal key "c") "cancelled")
                                       ((equal key "d") "disconnected")
                                       (t "pending"))))
                  (when (equal key "r")
                    (if (or rejection (eq retire 'dispatch-fault))
                        (should (string-match-p "r retry start" (buffer-string)))
                      (should (equal opened (unless fault '("https://lease.example/verify"))))
                      (should (equal hermes-onboarding-oauth--session-id "exact-flow")))))
                (when (and (not rejection) (not fault) (member key '("c" "d")))
                  (should-not hermes-onboarding-oauth--session-id))
                (should (= changed (if (and (equal key "d") (not rejection)) 1 0))))
            (unless (eq retire 'stop)
              (should (= (hermes-dashboard-transport-client-refcount client)
                         (1- references)))
              (should (eq (not (null (hermes-dashboard-transport-client-stopping-p client)))
                          (= references 1)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (hermes-dashboard-transport-stop client))))

(ert-deftest hermes-onboarding-oauth-lease-start ()
  "Standalone and shared start success/failure settle before exact release."
  (dolist (references '(1 2))
    (dolist (rejection '(nil t))
      (hermes-onboarding-test--lease "r" references rejection))))

(ert-deftest hermes-onboarding-oauth-lease-session-actions ()
  "All public session keys settle on standalone and shared clients."
  (dolist (key '("g" "s" "c" "d"))
    (dolist (references '(1 2))
      (dolist (rejection '(nil t))
        (hermes-onboarding-test--lease key references rejection)))))

(ert-deftest hermes-onboarding-oauth-lease-retired-owner ()
  "Real external stop, mode change and kill still suppress late effects."
  (dolist (retire '(stop mode kill))
    (dolist (rejection '(nil t))
      (hermes-onboarding-test--lease "r" 1 rejection retire))))

(ert-deftest hermes-onboarding-oauth-lease-exception-cleanup ()
  "Dispatch and settlement errors and quits release the exact lease once."
  (dolist (fault '(error quit))
    (dolist (retire '(nil dispatch-fault report-fault))
      (hermes-onboarding-test--lease "r" 1 (eq retire 'report-fault) retire fault))))

(ert-deftest hermes-onboarding-oauth-lease-synchronous-faults ()
  "Acquisition and promise-construction errors and quits cannot leak a lease."
  (dolist (acquisition '(nil t))
    (dolist (fault '(error quit))
      (with-temp-buffer
        (hermes-onboarding-oauth-mode)
        (hermes-buffer--claim 'hermes-onboarding-oauth-mode)
        (setq hermes-onboarding-oauth--provider "nous"
              hermes-onboarding-oauth--result '((status . "starting")))
        (let* ((hermes-instances nil)
               (hermes-dashboard-transport-url "https://lease.example")
               (hermes-dashboard-transport-idle-close-delay nil)
               (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
               (client (make-hermes-dashboard-transport-client
                        :base-url "https://lease.example" :generation 1 :refcount 1))
               (context (hermes-onboarding--oauth-context))
               caught)
          (unwind-protect
              (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                        ((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest _)
                           (if acquisition (signal fault nil) client))))
                (condition-case err
                    (hermes-onboarding--oauth-run
                     context (lambda (_) (signal fault nil))
                     (lambda (_) (ert-fail "Unexpected success")))
                  ((error quit) (setq caught (car err))))
                (should (eq caught (and acquisition fault)))
                (should (equal (hermes-transport--get
                                hermes-onboarding-oauth--result 'status) "error"))
                (should (string-match-p "r retry start" (buffer-string)))
                (should (= (hermes-dashboard-transport-client-refcount client)
                           (if acquisition 1 0)))
                (unless acquisition
                  (should (hermes-dashboard-transport-client-stopping-p client))))
            (hermes-dashboard-transport-stop client)))))))

(defun hermes-onboarding-test--retire-oauth (retirement)
  "Retire this OAuth view using RETIREMENT, retaining its other state."
  (pcase retirement
    ((or 'associated 'detached)
     (set-visited-file-name
      (expand-file-name "oauth-notes" temporary-file-directory) t)
     (auto-save-mode -1)
     (when (eq retirement 'detached) (set-visited-file-name nil t)))
    ('claim (hermes-buffer--claim 'hermes-onboarding-oauth-mode))
    ('unclaimed (setq hermes-buffer--owner nil)))
  (when retirement
    (read-only-mode -1)
    (erase-buffer)
    (insert "Literal OAuth notes λ\n")
    (setq header-line-format "Local notes")))

(defun hermes-onboarding-test--oauth-editing-state ()
  "Return the current OAuth view's editing and operation state."
  (list (buffer-string) (point) buffer-file-name buffer-read-only
        (buffer-modified-p) header-line-format hermes-buffer--owner
        hermes-onboarding-oauth--generation hermes-onboarding-oauth--result
        hermes-onboarding-oauth--session-id))

(defun hermes-onboarding-test--oauth-retirement (key boundary retirement rejection)
  "Exercise KEY at BOUNDARY with RETIREMENT and response REJECTION.
Keep the public constructor, acquisition wrapper, REST builder and release."
  (hermes-onboarding-test--without-startup
   (lambda ()
     (let* ((instance '("Owned" . "https://owned.example"))
            (hermes-instances (list instance))
            (hermes-dashboard-transport-idle-close-delay nil)
            (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
            (client (make-hermes-dashboard-transport-client
                     :base-url "https://owned.example"
                     :refcount 1 :generation 1))
            (auth (hermes--promise-make))
            (response (hermes--promise-make))
            (release (symbol-function 'hermes-dashboard-transport-release))
            (acquisitions 0) (releases 0) (prompts 0) (changed 0)
            requests opened view before
            (prompt (lambda (&rest _)
                      (cl-incf prompts)
                      (when (eq boundary 'input)
                        (hermes-onboarding-test--retire-oauth retirement)
                        (setq before (hermes-onboarding-test--oauth-editing-state)))
                      "literal-code")))
       (unwind-protect
           (save-window-excursion
             (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                       ((symbol-function 'hermes-dashboard-transport-acquire)
                        (lambda (&rest _) (cl-incf acquisitions) client))
                       ((symbol-function 'hermes-dashboard-transport-release)
                        (lambda (owner)
                          (should (eq owner client))
                          (cl-incf releases)
                          (funcall release owner)))
                       ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                        (lambda (&rest _) auth))
                       ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                        (lambda (request) (push request requests) response))
                       ((symbol-function 'read-passwd) prompt)
                       ((symbol-function 'yes-or-no-p) prompt)
                       ((symbol-function 'browse-url) (lambda (url) (push url opened)))
                       ((symbol-function 'hermes-onboarding--auth-changed)
                        (lambda () (cl-incf changed))))
               (hermes-onboarding--show-oauth
                '((id . "nous") (name . "Nous"))
                (if (equal key "r") '((status . "error"))
                  '((status . "pending") (session_id . "exact-flow")))
                nil instance)
               (setq view (current-buffer))
               (when (eq boundary 'admission)
                 (hermes-onboarding-test--retire-oauth retirement)
                 (setq before (hermes-onboarding-test--oauth-editing-state)))
               (call-interactively (key-binding (kbd key)))
               (when (eq boundary 'auth)
                 (hermes-onboarding-test--retire-oauth retirement)
                 (setq before (hermes-onboarding-test--oauth-editing-state)))
               (should-not requests)
               (hermes--promise-resolve auth '(:base-url "https://owned.example"))
               (when (eq boundary 'response)
                 (hermes-onboarding-test--retire-oauth retirement)
                 (setq before (hermes-onboarding-test--oauth-editing-state)))
               (if rejection
                   (hermes--promise-reject response "Synthetic OAuth failure")
                 (hermes--promise-resolve
                  response '(:status 200 :body
                             ((status . "approved") (ok . t)))))
               (let ((admitted (not (and retirement
                                        (memq boundary '(admission input)))))
                     (sent (or (not retirement) (eq boundary 'response))))
                 (should (= acquisitions (if admitted 1 0)))
                 (should (= releases (if admitted 1 0)))
                 (should (= (length requests) (if sent 1 0)))
                 (should (= prompts (if (and (member key '("s" "d"))
                                            (not (and retirement
                                                      (eq boundary 'admission))))
                                       1 0)))
                 (when sent
                   (should (equal (plist-get (car requests) :method)
                                  (pcase key ("g" "GET")
                                         ((or "r" "s") "POST") (_ "DELETE"))))
                   (should (equal (plist-get (car requests) :url)
                                  (concat "https://owned.example/api/providers/oauth/"
                                          (pcase key
                                            ("r" "nous/start")
                                            ("g" "nous/poll/exact-flow")
                                            ("s" "nous/submit")
                                            ("c" "sessions/exact-flow")
                                            ("d" "nous"))))))
                 (if retirement
                     (progn
                       (should (equal before (hermes-onboarding-test--oauth-editing-state)))
                       (should (= changed 0)))
                   (should (equal (hermes-transport--get hermes-onboarding-oauth--result 'status)
                                  (cond (rejection "error")
                                        ((equal key "c") "cancelled")
                                        ((equal key "d") "disconnected")
                                        (t "approved")))))
                 (should-not opened)
                 (hermes--promise-reject response "Duplicate")
                 (should (= releases (if admitted 1 0)))
                 (when admitted
                   (should (= (hermes-dashboard-transport-client-refcount client) 0))
                   (should (hermes-dashboard-transport-client-stopping-p client))))))
         (when (buffer-live-p view)
           (with-current-buffer view (set-buffer-modified-p nil))
           (kill-buffer view))
         (hermes-dashboard-transport-stop client))))))

(ert-deftest hermes-onboarding-oauth-retirement-public-admission ()
  "Fresh public commands never acquire, prompt or repaint retired notes."
  (dolist (key '("g" "s" "c" "d" "r"))
    (dolist (retirement '(nil associated detached unclaimed))
      (hermes-onboarding-test--oauth-retirement key 'admission retirement nil))))

(ert-deftest hermes-onboarding-oauth-retirement-input-continuations ()
  "Code and consent cannot transfer to a replacement constructor claim."
  (dolist (key '("s" "d"))
    (dolist (retirement '(nil associated detached claim))
      (hermes-onboarding-test--oauth-retirement key 'input retirement nil))))

(ert-deftest hermes-onboarding-oauth-retirement-authentication ()
  "Even polling GET retains exact view authority until actual HTTP dispatch."
  (dolist (key '("g" "s" "c" "d" "r"))
    (dolist (retirement '(nil associated detached claim))
      (hermes-onboarding-test--oauth-retirement key 'auth retirement nil))))

(ert-deftest hermes-onboarding-oauth-retirement-pending-publication ()
  "Late success and error settle once without publishing over retired views."
  (dolist (key '("g" "s" "c" "d" "r"))
    (dolist (retirement '(nil associated detached claim))
      (dolist (rejection '(nil t))
        (hermes-onboarding-test--oauth-retirement key 'response retirement rejection)))))

(ert-deftest hermes-onboarding-oauth-retirement-reopen ()
  "Reopening creates a current OAuth view without reclaiming detached notes."
  (dolist (retirement '(associated detached))
    (let* ((instance '("Owned" . "https://owned.example"))
           (hermes-instances (list instance))
           (provider '((id . "nous") (name . "Nous")))
           (result '((status . "pending") (session_id . "exact-flow")))
           (client (make-hermes-dashboard-transport-client
                    :base-url "https://owned.example" :token "synthetic" :generation 1))
           first second requests before)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () client))
                      ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                       (lambda (request)
                         (push request requests)
                         (hermes--promise-resolved '(:status 200 :body ((ok . t)))))))
              (setq first (plist-get (hermes-onboarding--show-oauth provider result nil instance)
                                     :buffer))
              (hermes-onboarding-test--retire-oauth retirement)
              (setq before (hermes-onboarding-test--oauth-editing-state))
              (setq second (plist-get (hermes-onboarding--show-oauth provider result nil instance)
                                      :buffer))
              (should-not (eq first second))
              (call-interactively (key-binding (kbd "c")))
              (should (= (length requests) 1))
              (should (string-match-p "Status: cancelled" (buffer-string)))
              (with-current-buffer first
                (should (equal before (hermes-onboarding-test--oauth-editing-state))))))
        (dolist (buffer (list first second))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))


(ert-deftest hermes-onboarding-oauth-retirement-before-acquisition ()
  "A constructor retired during display cannot reach client acquisition."
  (dolist (retirement '(associated detached claim kill))
    (let ((instance '("Owned" . "https://owned.example")) buffer context)
      (unwind-protect
          (save-window-excursion
            (setq context
                  (hermes-onboarding--show-oauth
                   '((id . "nous")) '((status . "starting")) nil instance)
                  buffer (plist-get context :buffer))
            (if (eq retirement 'kill) (kill-buffer buffer)
              (hermes-onboarding-test--retire-oauth retirement))
            (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest _) (ert-fail "Retired acquisition")))
                      ((symbol-function 'hermes-browser--existing-client)
                       (lambda () (ert-fail "Retired lookup"))))
              (hermes-onboarding--oauth-run
               context (lambda (_) (ert-fail "Retired dispatch"))
               (lambda (_) (ert-fail "Retired result")))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(defun hermes-onboarding-test--global-disconnect
    (delayed entry boundary effect &optional shared)
  "Exercise DELAYED catalogue from ENTRY, with EFFECT at BOUNDARY.
Keep real acquisition, REST building, constructor and release.  SHARED
retains another transport lease; otherwise settlement stops the client."
  (hermes-onboarding-test--without-startup
   (lambda ()
     (let* ((instance '("Owned" . "https://owned.example"))
            (hermes-instances (unless (eq entry 'legacy) (list instance)))
            (hermes-dashboard-transport-url (cdr instance))
            (hermes-dashboard-transport-idle-close-delay nil)
            (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
            (client (make-hermes-dashboard-transport-client
                     :base-url (cdr instance) :generation 1
                     :refcount (if shared 2 1)))
            (catalogue (hermes--promise-make))
            (auth (hermes--promise-make))
            (response (hermes--promise-make))
            (catalogue-result
             '(:status 200 :body
               ((providers . (((id . "nous") (name . "Nous")
                               (disconnectable . t)
                               (status . ((logged_in . t)))))))))
            (release (symbol-function 'hermes-dashboard-transport-release))
            (acquisitions 0) (releases 0) (authentications 0)
            (prompts 0) (changed 0)
            source view retired before requests statuses caught auth-guards)
       (cl-labels
           ((status ()
              (when (buffer-live-p view)
                (with-current-buffer view
                  (hermes-transport--get hermes-onboarding-oauth--result 'status))))
            (interleave (phase)
              (when (eq boundary phase)
                (pcase effect
                  ((or 'error 'quit) (signal effect '("Synthetic disconnect failure")))
                  ('transport (hermes-dashboard-transport-stop client))
                  ((or 'source 'view 'constructor)
                   (setq retired (if (eq effect 'source) source view))
                   (with-current-buffer retired
                     (if (eq effect 'constructor)
                         (should (eq retired (hermes-buffer--get
                                              (buffer-name) #'hermes-onboarding-oauth-mode t)))
                       (if (eq major-mode 'hermes-onboarding-oauth-mode)
                           (hermes-onboarding-test--retire-oauth 'detached)
                         (fundamental-mode)
                         (insert "Literal source notes λ\n")))
                     (setq before (hermes-onboarding-test--oauth-editing-state))))))))
         (unwind-protect
             (save-window-excursion
               (cl-letf (((symbol-function 'hermes-browser--existing-client) #'ignore)
                         ((symbol-function 'hermes-dashboard-transport-acquire)
                          (lambda (&rest _)
                            (cl-incf acquisitions)
                            (interleave 'acquire)
                            client))
                         ((symbol-function 'hermes-dashboard-transport-release)
                          (lambda (owner)
                            (should (eq owner client))
                            (push (status) statuses)
                            (cl-incf releases)
                            (funcall release owner)))
                         ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                          (lambda (&rest _)
                            (if (= (cl-incf authentications) 1)
                                (hermes--promise-resolved (list :base-url (cdr instance)))
                              ;; Observe outside the promise's error handler.
                              (push hermes-dashboard-transport--api-auth-current-p auth-guards)
                              (interleave 'auth-start)
                              auth)))
                         ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                          (lambda (request)
                            (push request requests)
                            (if (equal (plist-get request :method) "GET")
                                (progn
                                  (interleave 'catalogue-request)
                                  (if delayed catalogue
                                    (hermes--promise-resolved catalogue-result)))
                              response)))
                         ((symbol-function 'completing-read)
                          (lambda (&rest _)
                            (cl-incf prompts) (interleave 'choice) "Nous"))
                         ((symbol-function 'yes-or-no-p)
                          (lambda (&rest _)
                            (cl-incf prompts) (interleave 'consent)
                            (not (eq effect 'cancel))))
                         ((symbol-function 'hermes-onboarding--auth-changed)
                          (lambda () (cl-incf changed))))
                 (setq source
                       (if (eq entry 'oauth)
                           (plist-get (hermes-onboarding--show-oauth
                                       '((id . "nous") (name . "Nous"))
                                       '((status . "approved")) nil instance) :buffer)
                         (generate-new-buffer " *OAuth global origin*")))
                 (with-current-buffer source
                   (unless (eq entry 'oauth) (text-mode))
                   (condition-case err
                       (call-interactively #'hermes-onboarding-oauth-disconnect-provider)
                     ((error quit) (setq caught (car err)))))
                 (setq view (hermes-buffer--find "*Hermes OAuth*" 'hermes-onboarding-oauth-mode))
                 (interleave 'catalogue)
                 ;; Deliver outside the origin and acquisition dynamic extent.
                 (with-temp-buffer
                   (hermes--promise-resolve catalogue catalogue-result))
                 (setq view (hermes-buffer--find "*Hermes OAuth*" 'hermes-onboarding-oauth-mode))
                 (interleave 'auth)
                 (hermes--promise-resolve auth (list :base-url (cdr instance)))
                 (interleave 'response)
                 (if (eq effect 'reject)
                     (hermes--promise-reject response "Synthetic DELETE failure")
                   (hermes--promise-resolve response '(:status 200 :body ((ok . t)))))
                 (let ((sent (or (null effect) (eq boundary 'response)))
                       (acquired (not (eq boundary 'acquire))))
                   (should (= acquisitions 1))
                   (should (seq-every-p #'functionp auth-guards))
                   (should (= prompts
                              (pcase boundary
                                ((or 'acquire 'catalogue-request 'catalogue) 0)
                                ('choice 1)
                                (_ 2))))
                   (should (= releases (if acquired 1 0)))
                   (should (eq caught (and (not acquired) effect)))
                   (should (= (cl-count "DELETE" requests :key (lambda (r) (plist-get r :method))
                                        :test #'equal)
                              (if sent 1 0)))
                   (when sent
                     (should (equal (plist-get (car requests) :url)
                                    "https://owned.example/api/providers/oauth/nous")))
                   (when retired
                     (with-current-buffer retired
                       (should (equal before (hermes-onboarding-test--oauth-editing-state)))))
                   (should (= changed (if effect 0 1)))
                   (unless effect
                     (should (= prompts 2))
                     (should (equal (status) "disconnected"))
                     (should (equal statuses '("disconnected"))))
                   (when (eq effect 'reject)
                     (should (equal (status) "error"))
                     (should (equal statuses '("error"))))
                   (when acquired
                     (should (= (hermes-dashboard-transport-client-refcount client)
                                (if shared 1 0))))
                   ;; Duplicate late deliveries cannot release or publish again.
                   (hermes--promise-reject catalogue "Duplicate")
                   (hermes--promise-reject auth "Duplicate")
                   (hermes--promise-reject response "Duplicate")
                   (should (= releases (if acquired 1 0))))))
           (dolist (buffer (delete-dups (list source view retired)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer)))
           (hermes-dashboard-transport-stop client)))))))

(ert-deftest hermes-onboarding-global-disconnect-current-owners ()
  "Immediate and delayed catalogues work from owned and global origins."
  (dolist (delayed '(nil t))
    (dolist (entry '(oauth global legacy))
      (dolist (shared '(nil t))
        (hermes-onboarding-test--global-disconnect delayed entry nil nil shared)))))

(ert-deftest hermes-onboarding-global-disconnect-retired-origins ()
  "Catalogue, choice, consent and authentication retain origin authority."
  (dolist (boundary '(catalogue choice consent auth response))
    (hermes-onboarding-test--global-disconnect t 'oauth boundary 'source))
  (hermes-onboarding-test--global-disconnect nil 'oauth 'auth 'source)
  (hermes-onboarding-test--global-disconnect t 'global 'auth 'source))

(ert-deftest hermes-onboarding-global-disconnect-retired-destinations ()
  "A global entry also retains the newly shown view and transport."
  (dolist (effect '(view constructor transport))
    (dolist (boundary '(auth response))
      (hermes-onboarding-test--global-disconnect t 'global boundary effect)))
  (hermes-onboarding-test--global-disconnect t 'oauth 'catalogue 'constructor))

(ert-deftest hermes-onboarding-global-disconnect-errors-and-quits ()
  "Errors, quits, cancellation and rejection release only the acquired lease."
  (dolist (effect '(error quit))
    (dolist (boundary '(acquire catalogue-request choice consent auth-start))
      (hermes-onboarding-test--global-disconnect t 'global boundary effect)))
  (hermes-onboarding-test--global-disconnect t 'global 'consent 'cancel)
  (dolist (shared '(nil t))
    (hermes-onboarding-test--global-disconnect t 'global 'response 'reject shared)))


(provide 'hermes-onboarding-tests)
;;; hermes-onboarding-tests.el ends here
