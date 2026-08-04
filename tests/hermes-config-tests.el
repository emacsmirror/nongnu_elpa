;;; hermes-config-tests.el --- config management tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-config-renders-schema-and-redacted-environment ()
  "Config render shows schema values and only dashboard-redacted env values."
  (let ((buffer (generate-new-buffer " *Hermes config test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (hermes-config-mode))
          (hermes-config--render
           buffer
           '((fields . ((model . ((type . "string")))
                        (agent.max_turns . ((type . "integer"))))))
           '((model . "gpt") (agent . ((max_turns . 9))))
           '((OPENAI_API_KEY . ((is_set . t)
                                (redacted_value . "sk-...last")))))
          (with-current-buffer buffer
            (should (string-match-p "model.*gpt" (buffer-string)))
            (should (string-match-p "agent.max_turns.*9" (buffer-string)))
            (should (string-match-p "OPENAI_API_KEY.*sk-...last"
                                    (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest hermes-config-fetches-schema-config-and-env-routes ()
  "Config refresh fetches the three authoritative dashboard resources."
  (let (calls result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest _)
                 (push (list method path) calls)
                 (hermes--promise-resolved `((path . ,path))))))
      (hermes--promise-then
       (hermes-config--fetch 'client)
       (lambda (value) (setq result value))))
    (should (equal (nreverse calls)
                   '(("GET" "/api/config/schema")
                     ("GET" "/api/config")
                     ("GET" "/api/env"))))
    (should (equal (mapcar (lambda (value) (cdr (assq 'path value))) result)
                   '("/api/config/schema" "/api/config" "/api/env")))))

(ert-deftest hermes-config-set-path-preserves-neighboring-config ()
  "Nested schema edits preserve unrelated configuration keys."
  (should
   (equal (hermes-config--set-path
           '((agent . ((max_turns . 9) (verbose . t))) (model . "gpt"))
           '("agent" "max_turns") 12)
          '((agent . ((max_turns . 12) (verbose . t))) (model . "gpt")))))

(ert-deftest hermes-config-coerces-values-from-schema ()
  "Config coercion follows schema type instead of current runtime value."
  (should (= (hermes-config--coerce "12" '((type . "number"))) 12))
  (should (eq (hermes-config--coerce "false" '((type . "boolean"))) :false))
  (should (eq (hermes-config--coerce "true" '((type . "bool"))) t))
  (should (equal (hermes-config--coerce "one, two" '((type . "list")))
                 '("one" "two")))
  (should-error (hermes-config--coerce "twelve" '((type . "number")))
                :type 'user-error))

(ert-deftest hermes-config-edit-uses-field-schema-and-config-rest-contract ()
  "Config edit derives type from schema and PUTs the full config envelope."
  (let (request)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "12"))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional _on-success)
                 (funcall make-promise 'client)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (setq request (list method path (plist-get args :body)))
                 (hermes--promise-resolved '((ok . t))))))
      (with-temp-buffer
        (hermes-config-mode)
        (setq hermes-config--schema
              '((fields . ((agent.max_turns . ((type . "number"))))))
              hermes-config--config
              '((agent . ((max_turns . "9") (verbose . t)))))
        (let ((inhibit-read-only t))
          (insert (propertize "agent.max_turns" 'hermes-config-key
                             "agent.max_turns")))
        (goto-char (point-min))
        (hermes-config-edit)))
    (should
     (equal request
            '("PUT" "/api/config"
              ((config . ((agent . ((max_turns . 12) (verbose . t)))))))))))

(ert-deftest hermes-config-env-actions-use-exact-redacted-rest-contracts ()
  "Env set, reveal, and delete use exact bodies without displaying a secret."
  (let (requests copied messages)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "live-secret"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'kill-new) (lambda (value) (setq copied value)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (let ((promise (funcall make-promise 'client)))
                   (when (and on-success
                              (equal (cadar requests) "/api/env/reveal"))
                     (funcall on-success
                              '((key . "OPENAI_API_KEY")
                                (value . "live-secret"))))
                   promise)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :body)
                             (plist-get args :secrets))
                       requests)
                 (hermes--promise-resolved '((ok . t))))))
      (with-temp-buffer
        (hermes-config-mode)
        (setq hermes-config--env
              '((OPENAI_API_KEY . ((is_password . t)
                                   (redacted_value . "sk-...last")))))
        (let ((inhibit-read-only t))
          (insert (propertize "OPENAI_API_KEY" 'hermes-env-key
                             "OPENAI_API_KEY")))
        (goto-char (point-min))
        (hermes-config-set-env)
        (hermes-config-reveal-env)
        (hermes-config-delete-env)))
    (should (member '("PUT" "/api/env"
                      ((key . "OPENAI_API_KEY") (value . "live-secret"))
                      ("live-secret"))
                    requests))
    (should (member '("POST" "/api/env/reveal"
                      ((key . "OPENAI_API_KEY")) nil)
                    requests))
    (should (member '("DELETE" "/api/env"
                      ((key . "OPENAI_API_KEY")) nil)
                    requests))
    (should (equal copied "live-secret"))
    (should-not (string-match-p "live-secret" (string-join messages " ")))))

(ert-deftest hermes-config-reveal-ignores-result-after-buffer-kill ()
  "Late reveal response never copies a secret after its owner is killed."
  (let ((pending (hermes--promise-make)) copied buffer)
    (cl-letf (((symbol-function 'kill-new) (lambda (value) (setq copied value)))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) pending)))
      (setq buffer (generate-new-buffer " *Hermes reveal owner*"))
      (with-current-buffer buffer
        (hermes-config-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "OPENAI_API_KEY" 'hermes-env-key
                             "OPENAI_API_KEY")))
        (goto-char (point-min))
        (hermes-config-reveal-env))
      (kill-buffer buffer)
      (hermes--promise-resolve
       pending '((key . "OPENAI_API_KEY") (value . "live-secret"))))
    (should-not copied)))

(provide 'hermes-config-tests)
;;; hermes-config-tests.el ends here
