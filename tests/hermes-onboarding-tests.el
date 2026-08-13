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
               (lambda () (setq refreshed (1+ refreshed))))
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
            (should (= refreshed 0))
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

(ert-deftest hermes-onboarding-account-browser-captures-chat-profile ()
  "The public account command carries its invoking chat's profile into GET."
  (let (query)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (_method _path &rest args)
                 (setq query (plist-get args :query))
                 (hermes--promise-resolved '((providers . nil)))))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (hermes-chat-mode)
            (setq hermes-chat--profile "profile-b")
            (hermes-onboarding-oauth-connect)
            (should (equal query '((profile . "profile-b"))))
            (with-current-buffer "*Hermes Provider Accounts*"
              (should (equal hermes-onboarding--provider-account-profile
                             "profile-b"))))
        (when (get-buffer "*Hermes Provider Accounts*")
          (kill-buffer "*Hermes Provider Accounts*"))))))

(ert-deftest hermes-onboarding-oauth-native-flow-carries-profile-context ()
  "Native start and status actions retain the provider browser's profile."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional _on-success _on-error)
                 (funcall make-promise 'client)))
              ((symbol-function 'hermes-onboarding--show-oauth)
               (lambda (_provider _result &optional profile)
                 (push (list 'show profile) calls)
                 'context))
              ((symbol-function 'hermes-onboarding--oauth-start)
               (lambda (client provider &optional profile)
                 (push (list 'start client provider profile) calls)))
              ((symbol-function 'hermes-onboarding--oauth-poll)
               (lambda (client provider session &optional profile)
                 (push (list 'poll client provider session profile) calls)))
              ((symbol-function 'hermes-onboarding--oauth-submit)
               (lambda (client provider session code &optional profile)
                 (push (list 'submit client provider session code profile) calls)))
              ((symbol-function 'hermes-onboarding--oauth-cancel)
               (lambda (client session &optional profile)
                 (push (list 'cancel client session profile) calls)))
              ((symbol-function 'hermes-onboarding--oauth-disconnect)
               (lambda (client provider &optional profile)
                 (push (list 'disconnect client provider profile) calls)))
              ((symbol-function 'read-passwd) (lambda (&rest _) "code"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (hermes-provider-accounts-mode)
        (setq hermes-onboarding--provider-account-profile "profile-b")
        (hermes-onboarding--oauth-start-provider
         '((id . "anthropic") (name . "Anthropic"))))
      (with-temp-buffer
        (hermes-onboarding-oauth-mode)
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
          messages)
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
                 (lambda (&rest _) (setq shown (1+ shown)) 'context))
                ((symbol-function 'hermes-onboarding--oauth-apply-result)
                 (lambda (&rest _) (setq applied (1+ applied)) t))
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

(ert-deftest hermes-onboarding-oauth-prompts-respect-captured-owner ()
  "Submit and disconnect do nothing when their prompt loses OAuth ownership."
  (dolist (command '(hermes-onboarding-oauth-submit
                     hermes-onboarding-oauth-disconnect))
    (let ((buffer (generate-new-buffer " *Hermes OAuth prompt owner*"))
          successor
          (requests 0)
          (changed 0)
          (rendered 0)
          opened messages)
      (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                 (lambda (&rest _) (setq requests (1+ requests))))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _)
                   (with-current-buffer buffer
                     (setq hermes-onboarding-oauth--provider "successor"
                           hermes-onboarding-oauth--provider-name "Successor"
                           hermes-onboarding-oauth--session-id "new-session"
                           hermes-onboarding-oauth--profile "profile-c"
                           hermes-onboarding-oauth--result
                           '((status . "successor")))
                     (setq successor (hermes-onboarding--oauth-context)))
                   "secret-code"))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (with-current-buffer buffer
                     (setq hermes-onboarding-oauth--provider "successor"
                           hermes-onboarding-oauth--provider-name "Successor"
                           hermes-onboarding-oauth--session-id "new-session"
                           hermes-onboarding-oauth--profile "profile-c"
                           hermes-onboarding-oauth--result
                           '((status . "successor")))
                     (setq successor (hermes-onboarding--oauth-context)))
                   t))
                ((symbol-function 'hermes-onboarding--auth-changed)
                 (lambda () (setq changed (1+ changed))))
                ((symbol-function 'hermes-onboarding-oauth--render)
                 (lambda () (setq rendered (1+ rendered))))
                ((symbol-function 'browse-url) (lambda (url) (setq opened url)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (unwind-protect
            (with-current-buffer buffer
              (hermes-onboarding-oauth-mode)
              (setq hermes-onboarding-oauth--provider "old"
                    hermes-onboarding-oauth--provider-name "Old"
                    hermes-onboarding-oauth--session-id "old-session"
                    hermes-onboarding-oauth--profile "profile-b"
                    hermes-onboarding-oauth--result '((status . "old")))
              (funcall command)
              (should (hermes-onboarding--oauth-context-current-p successor))
              (should (equal hermes-onboarding-oauth--provider "successor"))
              (should (equal hermes-onboarding-oauth--session-id "new-session"))
              (should (equal hermes-onboarding-oauth--profile "profile-c"))
              (should (= requests 0))
              (should (= changed 0))
              (should (= rendered 0))
              (should-not opened)
              (should-not messages))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-onboarding-oauth-mode-reset-does-not-reuse-owner ()
  "A mode reset cannot let an old OAuth resolution or rejection win an ABA race."
  (dolist (settlement '(resolve reject))
    (let ((first (hermes--promise-make))
          (second (hermes--promise-make))
          (requests 0)
          (changed 0)
          opened messages successor-result successor-text
          (provider '((id . "nous") (name . "Nous"))))
      (cl-letf (((symbol-function 'hermes-browser--with-client)
                 (lambda (fn) (funcall fn 'client #'ignore)))
                ((symbol-function 'hermes-onboarding--oauth-start)
                 (lambda (&rest _)
                   (setq requests (1+ requests))
                   (if (= requests 1) first second)))
                ((symbol-function 'hermes-onboarding--auth-changed)
                 (lambda () (setq changed (1+ changed))))
                ((symbol-function 'pop-to-buffer) #'ignore)
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
              (with-current-buffer "*Hermes OAuth*"
                (fundamental-mode)
                (hermes-onboarding-oauth-mode)
                (setq hermes-onboarding-oauth--profile "profile-b")
                (hermes-onboarding--oauth-start-provider provider)
                (setq successor-result
                      (copy-tree hermes-onboarding-oauth--result)
                      successor-text (buffer-string)))
              (setq messages nil)
              (if (eq settlement 'resolve)
                  (hermes--promise-resolve
                   first '((status . "approved")
                           (auth_url . "https://example.org/stale")))
                (hermes--promise-reject first "stale OAuth failure"))
              (with-current-buffer "*Hermes OAuth*"
                (should (equal hermes-onboarding-oauth--result successor-result))
                (should (equal (buffer-string) successor-text)))
              (should (= changed 0))
              (should-not opened)
              (should-not messages)
              (hermes--promise-resolve second '((status . "approved")))
              (with-current-buffer "*Hermes OAuth*"
                (should (equal
                         (hermes-transport--display-field
                          hermes-onboarding-oauth--result 'status)
                         "approved")))
              (should (= changed 1)))
          (when (get-buffer "*Hermes OAuth*")
            (kill-buffer "*Hermes OAuth*")))))))

(ert-deftest hermes-onboarding-oauth-status-omits-secret-fields ()
  "OAuth status text renders useful state without arbitrary secret fields."
  (let ((text (hermes-onboarding--oauth-status-text
               '((status . "pending") (user_code . "ABCD-EFGH")
                 (code . "secret-auth-code") (access_token . "secret-token")))))
    (should (string-match-p "Status: pending" text))
    (should (string-match-p "User code: ABCD-EFGH" text))
    (should-not (string-match-p "secret" text))))

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
  "OAuth approval and disconnect report both authentication changes."
  (let ((changed 0))
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success _on-error)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-onboarding--oauth-poll)
               (lambda (&rest _)
                 (hermes--promise-resolved '((status . "approved")))))
              ((symbol-function 'hermes-onboarding--oauth-disconnect)
               (lambda (&rest _)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-onboarding--auth-changed)
               (lambda () (setq changed (1+ changed))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (hermes-onboarding-oauth-mode)
        (setq hermes-onboarding-oauth--provider "nous"
              hermes-onboarding-oauth--provider-name "Nous"
              hermes-onboarding-oauth--session-id "sid")
        (hermes-onboarding-oauth-poll)
        (hermes-onboarding-oauth-disconnect)
        (should (= changed 2))))))

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

(provide 'hermes-onboarding-tests)
;;; hermes-onboarding-tests.el ends here
