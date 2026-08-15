;;; hermes-chat-models-tests.el --- model selection tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-models': `model.options' completion candidates,
;; the `config.set' model switch with its expensive-model confirmation
;; loop, and API-key provider connect from the chat picker.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)
(ert-deftest hermes-chat-model-candidates-auth-first-dedup ()
  "Model candidates list authenticated providers first and keep provider identity."
  (let* ((cands (hermes-chat--model-candidates
                 '((providers
                    . (((slug . "openai") (name . "OpenAI")
                        (authenticated . nil) (models . ("gpt")))
                       ((slug . "anthropic") (name . "Anthropic")
                        (authenticated . t)
                        (models . ("claude"))
                        (pricing . ((claude . ((input . "$3") (output . "$15")))))
                        (capabilities . ((claude . ((reasoning . t)
                                                    (fast . t)
                                                    (context_window . 200000))))))
                       ((slug . "openrouter") (name . "OpenRouter")
                        (authenticated . t) (models . ("claude" ((id . "gemini"))))))))))
         (labels (mapcar #'car cands))
         (providers (mapcar (lambda (cand)
                              (plist-get (cdr cand) :provider))
                            cands)))
    (should (equal providers '("anthropic" "openrouter" "openrouter" "openai")))
    (should (string-match-p "Anthropic" (car labels)))
    (should (string-match-p "(anthropic)" (car labels)))
    (should (string-match-p "claude" (car labels)))
    (should (string-match-p "\\$3" (car labels)))
    (should-not (string-match-p "reasoning" (car labels)))
    (should-not (string-match-p "fast" (car labels)))
    (should-not (string-match-p "ctx" (car labels)))
    (should (equal (hermes-chat--model-config-value (cdar cands))
                   "claude --provider anthropic"))))

(ert-deftest hermes-chat-model-capf-completes-cached-authenticated-models ()
  "`/model' arguments complete from cached authenticated provider models."
  (let ((payload
         '((providers
            . (((slug . "openai-codex") (name . "OpenAI Codex")
                (authenticated . t) (models . ("gpt-5.6")))
               ((slug . "anthropic") (name . "Anthropic")
                (authenticated . t) (models . ("claude-opus")))
               ((slug . "openrouter") (name . "OpenRouter")
                (authenticated . nil) (models . ("gpt-5.6"))))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-model-options)
               (lambda (_client) payload))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (&rest _args)
                 (ert-fail "Warm completion must not issue an RPC"))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client))
       (goto-char (point-max))
       (insert "/model gp")
       (let* ((capf (hermes-chat--model-capf))
              (candidates (nth 2 capf))
              (annotation (plist-get (nthcdr 3 capf) :annotation-function)))
         (should (= (nth 0 capf) (- (point) 2)))
         (should (= (nth 1 capf) (point)))
         (should (equal candidates
                        '("gpt-5.6 --provider openai-codex"
                          "claude-opus --provider anthropic")))
         (should (string-match-p
                  "OpenAI Codex"
                  (funcall annotation "gpt-5.6 --provider openai-codex")))
         (should-not (member "gpt-5.6 --provider openrouter" candidates)))))))

(ert-deftest hermes-chat-model-capf-completes-at-point-in-chat-mode ()
  "TAB-style completion inserts the unique cached `/model' argument."
  (let ((payload
         '((providers
            . (((slug . "openai-codex") (name . "OpenAI Codex")
                (authenticated . t) (models . ("gpt-5.6"))))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-model-options)
               (lambda (_client) payload)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client))
       (goto-char (point-max))
       (insert "/model gp")
       (completion-at-point)
       (should (equal (hermes-chat-input-string)
                      "/model gpt-5.6 --provider openai-codex"))))))

(ert-deftest hermes-chat-model-capf-warms-cold-cache-asynchronously ()
  "A cold `/model' completion starts one asynchronous catalog warmup."
  (let ((requests 0) requested)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-model-options)
               (lambda (_client) nil))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (cl-incf requests)
                 (setq requested args))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client))
       (goto-char (point-max))
       (insert "/model gp")
       (should-not (hermes-chat--model-capf))
       (should-not (hermes-chat--model-capf))
       (should (= requests 1))
       (should (equal (plist-get requested :session-id)
                      hermes-chat--dashboard-active-session-id))))))

(ert-deftest hermes-chat-switch-model-sets-chosen-model ()
  "Switching prompts from model.options and applies the choice via config.set."
  (let (set-key set-value set-session set-confirm)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("alpha" "beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (let ((choice (cl-find "beta" coll :test #'string-match-p)))
                   (should choice)
                   choice)))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq set-key key set-value value
                       set-session (plist-get args :session-id)
                       set-confirm (plist-get args :confirm-expensive-model))
                 (funcall (plist-get args :resolve)
                          '((key . "model") (value . "beta --provider p1"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal set-key "model"))
       (should (equal set-value "beta --provider p1"))
       (should (equal set-session "sid-1"))
       (should-not set-confirm)
       (should (string-match-p "Model set to beta" (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-confirms-expensive-choice ()
  "Expensive model confirmation retries config.set with confirmation enabled."
  (let ((calls 0)
        confirms prompt)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (cl-find "beta" coll :test #'string-match-p)))
              ((symbol-function 'yes-or-no-p)
               (lambda (text)
                 (setq prompt text)
                 t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key value &rest args)
                 (should (equal value "beta --provider p1"))
                 (setq calls (1+ calls))
                 (push (plist-get args :confirm-expensive-model) confirms)
                 (funcall (plist-get args :resolve)
                          (if (= calls 1)
                              '((confirm_required . t)
                                (confirm_message . "This model may be expensive"))
                            '((key . "model")
                              (value . "beta --provider p1")))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal calls 2))
       (should (equal (nreverse confirms) '(nil t)))
       (should (equal prompt "This model may be expensive"))
       (should (string-match-p "Model set to beta" (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-stops-repeated-expensive-confirmation ()
  "A repeated confirmation request after consent reports an error instead of looping."
  (let ((calls 0)
        confirms)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (cl-find "beta" coll :test #'string-match-p)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key value &rest args)
                 (should (equal value "beta --provider p1"))
                 (setq calls (1+ calls))
                 (push (plist-get args :confirm-expensive-model) confirms)
                 (funcall (plist-get args :resolve)
                          '((confirm_required . t)
                            (confirm_message . "Still expensive"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal calls 2))
       (should (equal (nreverse confirms) '(nil t)))
       (should (string-match-p "still requires confirmation"
                               (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-cancelled-expensive-choice-stops ()
  "Declining an expensive-model confirmation does not retry config.set."
  (let ((calls 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (cl-find "beta" coll :test #'string-match-p)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key value &rest args)
                 (should (equal value "beta --provider p1"))
                 (setq calls (1+ calls))
                 (funcall (plist-get args :resolve)
                          '((confirm_required . t)
                            (confirm_message . "This model may be expensive"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal calls 1))
       (should (string-match-p "Model switch cancelled" (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-renders-config-set-rejection ()
  "A config.set rejection from the dashboard is rendered in the chat buffer."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve)
                        '((model . "old-model")
                          (providers
                           . (((slug . "p1") (authenticated . t)
                               (name . "Provider One")
                               (models . ("beta")))))))))
            ((symbol-function 'completing-read)
             (lambda (_prompt coll &rest _)
               (cl-find "beta" coll :test #'string-match-p)))
            ((symbol-function 'hermes-dashboard-transport-config-set)
             (lambda (_client _key _value &rest args)
               (funcall (plist-get args :reject) "backend denied"))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
           hermes-chat--dashboard-active-session-id "sid-1"
           hermes-chat--dashboard-session-ready-p t)
     (hermes-chat-switch-model)
     (should (string-match-p "backend denied" (buffer-string))))))

(ert-deftest hermes-chat-switch-model-ignores-catalog-after-turn-starts ()
  "A delayed catalog cannot prompt once the target chat becomes busy."
  (let (resolve prompted applied)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) (setq prompted t) "model"))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (&rest _args) (setq applied t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (setq hermes-chat--pending-assistant-id "assistant")
       (funcall resolve
                '((providers . (((slug . "p") (authenticated . t)
                                  (models . ("model")))))))
       (should-not prompted)
       (should-not applied)))))

(ert-deftest hermes-chat-switch-model-allows-locally-finished-turn ()
  "A terminal event releases the model picker without waiting for session.info."
  (let (requested)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest _args) (setq requested t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-running-p t)
       (hermes-chat--run-turn-reducer nil '(:type done))
       (hermes-chat-switch-model)
       (should requested)
       (should-not hermes-chat--dashboard-running-p)))))

(ert-deftest hermes-chat-switch-model-ignores-catalog-after-reset ()
  "A delayed catalog cannot prompt after the chat lifetime changes."
  (let (resolve prompted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) (setq prompted t) "model")))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (cl-incf hermes-chat--lifecycle-generation)
       (funcall resolve
                '((providers . (((slug . "p") (authenticated . t)
                                  (models . ("model")))))))
       (should-not prompted)))))

(ert-deftest hermes-chat-switch-model-ignores-catalog-after-client-change ()
  "A delayed catalog cannot prompt after the chat changes transport clients."
  (let (resolve prompted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) (setq prompted t) "model")))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client))
       (funcall resolve
                '((providers . (((slug . "p") (authenticated . t)
                                  (models . ("model")))))))
       (should-not prompted)))))

;;; Group: provider onboarding from chat

(ert-deftest hermes-chat-find-provider-matches-slug ()
  "`hermes-chat--find-provider' returns the provider row for a slug, or nil."
  (let ((result '((providers . (((slug . "openai"))
                                ((slug . "deepseek") (name . "DeepSeek")))))))
    (should (equal (hermes-transport--get
                    (hermes-chat--find-provider result "deepseek") 'name)
                   "DeepSeek"))
    (should-not (hermes-chat--find-provider result "nope"))))

(ert-deftest hermes-chat-connect-provider-candidate-saves-then-runs-on-connected ()
  "Connecting reads a key and saves it scoped to the session, then continues."
  (let (saved on-ran)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "sk-secret"))
              ((symbol-function 'hermes-dashboard-transport-model-save-key)
               (lambda (_client slug key &rest args)
                 (setq saved (list slug key (plist-get args :session-id)))
                 (funcall (plist-get args :resolve)
                          '((provider . ((slug . "deepseek"))))))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-active-session-id "sid-1"
              hermes-chat--dashboard-session-ready-p t)
        (hermes-chat--connect-provider-candidate
         (current-buffer) 'fake-client
         '((slug . "deepseek") (name . "DeepSeek")
           (auth_type . "api_key") (key_env . "DEEPSEEK_API_KEY"))
         (lambda () (setq on-ran t)))))
    (should (equal saved '("deepseek" "sk-secret" "sid-1")))
    (should on-ran)))

(ert-deftest hermes-chat-model-picker-connects-unauthed-then-applies ()
  "Picking an unauthenticated provider's model connects it, then applies it."
  (let (saved applied)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt labels &rest _) (car labels)))
              ((symbol-function 'read-passwd) (lambda (&rest _) "sk-secret"))
              ((symbol-function 'hermes-dashboard-transport-model-save-key)
               (lambda (_client slug _key &rest args)
                 (setq saved slug)
                 (funcall (plist-get args :resolve)
                          '((provider . ((slug . "deepseek")))))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq applied (cons key value))
                 (funcall (plist-get args :resolve) '((ok . t))))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-active-session-id "sid-1"
              hermes-chat--dashboard-session-ready-p t)
        (hermes-chat--prompt-and-set-model
         (current-buffer) 'fake-client
         '((providers . (((slug . "deepseek") (name . "DeepSeek")
                          (auth_type . "api_key") (key_env . "DEEPSEEK_API_KEY")
                          (models . ("deepseek-chat")))))))))
    (should (equal saved "deepseek"))
    (should (equal (car applied) "model"))))

(ert-deftest hermes-chat-model-picker-does-not-key-connect-oauth-provider ()
  "Picking an unauthenticated OAuth model never enters the API-key flow."
  (let (read-key saved applied notice)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt labels &rest _) (car labels)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) (setq read-key t) "oauth-secret"))
              ((symbol-function 'hermes-dashboard-transport-model-save-key)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (&rest _) (setq applied t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq notice (apply #'format format-string args)))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-active-session-id "sid-oauth"
              hermes-chat--dashboard-session-ready-p t)
        (hermes-chat--prompt-and-set-model
         (current-buffer) 'fake-client
         '((providers . (((slug . "openai-codex") (name . "OpenAI Codex")
                          (auth_type . "oauth") (authenticated . nil)
                          (models . ("gpt-5.6")))))))))
    (should-not read-key)
    (should-not saved)
    (should-not applied)
    (should (string-match-p "OAuth" notice))
    (should (string-match-p "OpenAI Codex" notice))))

(provide 'hermes-chat-models-tests)
;;; hermes-chat-models-tests.el ends here
