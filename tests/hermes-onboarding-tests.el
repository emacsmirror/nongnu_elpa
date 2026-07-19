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
         (hermes-onboarding-connected-function
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

(defun hermes-onboarding-test--api-key-provider-result ()
  "Return a `model.options' result carrying one connectable provider."
  `((providers . (,(hermes-onboarding-test--api-key-provider)))))

(provide 'hermes-onboarding-tests)
;;; hermes-onboarding-tests.el ends here
