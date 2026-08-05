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

(ert-deftest hermes-onboarding-unauthed-providers-offers-every-unconnected-one ()
  "Every unauthenticated provider is offered; authenticated ones are dropped.
The client does not classify by auth type -- Nous (registry-tagged OAuth) is
listed like any other, and the gateway decides on save."
  (let ((result '((providers . (((slug . "openai") (authenticated . t))
                                ((slug . "deepseek") (auth_type . "api_key")
                                 (key_env . "DEEPSEEK_API_KEY"))
                                ((slug . "nous") (auth_type . "oauth_device_code")
                                 (key_env . "")))))))
    (should (equal (mapcar (lambda (p) (hermes-transport--get p 'slug))
                           (hermes-onboarding--unauthed-providers result))
                   '("deepseek" "nous")))))

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

(ert-deftest hermes-onboarding-oauth-rest-actions-use-canonical-routes ()
  "OAuth actions use provider/session routes and mark submitted code secret."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path
                             (plist-get args :body)
                             (plist-get args :secrets))
                       calls)
                 (hermes--promise-resolved '((ok . t))))))
      (hermes-onboarding--oauth-start 'client "nous")
      (hermes-onboarding--oauth-poll 'client "nous" "sid")
      (hermes-onboarding--oauth-submit 'client "anthropic" "sid" "code-secret")
      (hermes-onboarding--oauth-cancel 'client "sid")
      (hermes-onboarding--oauth-disconnect 'client "nous")
      (should (equal (nreverse calls)
                     '(("POST" "/api/providers/oauth/nous/start" nil nil)
                       ("GET" "/api/providers/oauth/nous/poll/sid" nil nil)
                       ("POST" "/api/providers/oauth/anthropic/submit"
                        ((session_id . "sid") (code . "code-secret"))
                        ("code-secret"))
                       ("DELETE" "/api/providers/oauth/sessions/sid" nil nil)
                       ("DELETE" "/api/providers/oauth/nous" nil nil)))))))

(ert-deftest hermes-onboarding-oauth-filters-native-and-connected-providers ()
  "Connect excludes external/already-connected flows; disconnect is explicit."
  (let ((native '((flow . "device_code") (status . ((logged_in . nil)))))
        (external '((flow . "external") (status . ((logged_in . nil)))))
        (connected '((flow . "pkce") (disconnectable . t)
                     (status . ((logged_in . t))))))
    (should (hermes-onboarding--oauth-provider-connectable-p native))
    (should-not (hermes-onboarding--oauth-provider-connectable-p external))
    (should-not (hermes-onboarding--oauth-provider-connectable-p connected))
    (should (hermes-onboarding--oauth-provider-disconnectable-p connected))))

(ert-deftest hermes-onboarding-oauth-mode-exposes-session-actions ()
  "OAuth status buffers expose poll, submit, cancel, and disconnect commands."
  (dolist (binding '(("g" . hermes-onboarding-oauth-poll)
                     ("s" . hermes-onboarding-oauth-submit)
                     ("c" . hermes-onboarding-oauth-cancel)
                     ("d" . hermes-onboarding-oauth-disconnect)))
    (should (eq (keymap-lookup hermes-onboarding-oauth-mode-map (car binding))
                (cdr binding)))))

(ert-deftest hermes-onboarding-oauth-status-omits-secret-fields ()
  "OAuth status text renders useful state without arbitrary secret fields."
  (let ((text (hermes-onboarding--oauth-status-text
               '((status . "pending") (user_code . "ABCD-EFGH")
                 (code . "secret-auth-code") (access_token . "secret-token")))))
    (should (string-match-p "Status: pending" text))
    (should (string-match-p "User code: ABCD-EFGH" text))
    (should-not (string-match-p "secret" text))))

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
               (lambda (make-promise &optional on-success)
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
