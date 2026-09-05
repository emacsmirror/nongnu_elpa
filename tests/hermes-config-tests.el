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
  (should (equal (hermes-config--coerce "[\"one\", \"two\"]" '((type . "list")))
                 ["one" "two"]))
  (should-error (hermes-config--coerce "twelve" '((type . "number")))
                :type 'user-error))

(ert-deftest hermes-config-list-edit-roundtrips-through-json-wire ()
  "Accepting list input unchanged preserves elements in the actual PUT JSON."
  (dolist (items '(nil ("") ("" "") ("one" "two")
                  ("a,b" " leading" "trailing " "  " "")
                  ("quote\"" "back\\slash" "line\nbreak" "tab\t" "λ")))
    (dolist (current (list items (vconcat items)))
      (ert-info ((format "Current value: %S" current))
        (let (wire)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (_prompt initial &rest _) initial))
                    ((symbol-function 'hermes-browser--with-client)
                     (lambda (fn) (funcall fn 'client #'ignore)))
                    ((symbol-function 'hermes-dashboard-transport-api-request-async)
                     (lambda (_method _path &rest args)
                       (setq wire (json-parse-string
                                   (json-serialize (plist-get args :body))
                                   :object-type 'alist))
                       (hermes--promise-resolved '((ok . t)))))
                    ((symbol-function 'hermes-config--fetch)
                     (lambda (_) (hermes--promise-resolved '(nil nil nil)))))
            (with-temp-buffer
              (hermes-config-mode)
              (hermes-config--render
               (current-buffer) '((fields . ((tools . ((type . "list"))))))
               `((tools . ,current) (model . "unchanged")) nil)
              (search-forward "tools")
              (hermes-config-edit)))
          (should (equal (hermes-config--path-value wire "config.tools")
                         (vconcat items)))
          (should (equal (hermes-config--path-value wire "config.model")
                         "unchanged")))))))

(ert-deftest hermes-config-list-coercion-validates-json-array-shape ()
  "Only complete JSON arrays are accepted, without discarding element types."
  (dolist (text '("" "one,two" "[\"a\",]" "[\"a\"" "[] trailing"
                 "[] []" "{}" "null" "false" "42" "\"text\""))
    (ert-info ((format "Invalid list: %S" text))
      (should-error (hermes-config--coerce text '((type . "list")))
                    :type 'user-error)))
  (dolist (case '((" [] " . [])
                  ("[\"\",\"a,b\",\" x \"]" . ["" "a,b" " x "])
                  ("[1,true,false,null,[]]" . [1 t :false :null []])))
    (should (equal (hermes-config--coerce (car case) '((type . "list")))
                   (cdr case)))))

(ert-deftest hermes-config-list-invalid-input-never-starts-mutation ()
  "Malformed input cannot acquire a mutation or send arbitrary text."
  (let (requested)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bad,input"))
              ((symbol-function 'hermes-config--run-mutation)
               (lambda (&rest _) (setq requested t))))
      (with-temp-buffer
        (hermes-config-mode)
        (hermes-config--render
         (current-buffer) '((fields . ((tools . ((type . "list"))))))
         '((tools . ["old"])) nil)
        (search-forward "tools")
        (should-error (hermes-config-edit) :type 'user-error)
        (should (equal hermes-config--config '((tools . ["old"]))))))
    (should-not requested)))

(ert-deftest hermes-config-list-rejects-non-list-current-values ()
  "A schema mismatch must not turn a scalar or improper list into an array."
  (dolist (current '("text" 42 t :null ("one" . "two")))
    (let (prompted)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompted t) "[]")))
        (should-error (hermes-config--read-value
                       "tools" '((type . "list")) current)
                      :type 'user-error))
      (should-not prompted))))

(ert-deftest hermes-config-scalar-input-remains-unchanged ()
  "String and number editors keep their existing initial text and coercion."
  (dolist (case '(("string" " a,b " " a,b ") ("number" 12 12)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt initial &rest _)
                 (should (equal prompt "field: "))
                 initial)))
      (should (equal (hermes-config--read-value
                      "field" `((type . ,(car case))) (cadr case))
                     (caddr case))))))

(ert-deftest hermes-config-edit-uses-field-schema-and-config-rest-contract ()
  "Config edit derives type from schema and PUTs the full config envelope."
  (let (request)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "12"))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (setq request (list method path (plist-get args :body)))
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-config--fetch)
               (lambda (_client) (hermes--promise-resolved '(nil nil nil)))))
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
               (lambda (make-promise &optional on-success _on-error)
                 (let ((promise (funcall make-promise 'client)))
                   (when (and on-success
                              (equal (cadar requests) "/api/env/reveal"))
                     (funcall on-success
                              '((key . "OPENAI_API_KEY")
                                (value . "live-secret"))))
                   promise)))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
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
        (cl-labels ((select-key ()
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert (propertize "OPENAI_API_KEY" 'hermes-env-key
                                           "OPENAI_API_KEY")))
                      (goto-char (point-min))))
          (select-key)
          (hermes-config-set-env)
          (select-key)
          (hermes-config-reveal-env)
          (select-key)
          (hermes-config-delete-env))))
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

(ert-deftest hermes-config-refresh-ignores-stale-rejection ()
  "An older refresh rejection cannot report over a newer refresh."
  (let ((old-request (hermes--promise-make))
        (new-request (hermes--promise-make))
        requests messages)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'hermes-config--fetch)
               (lambda (_client)
                 (let ((request (if requests new-request old-request)))
                   (push request requests)
                   request)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-config-mode)
        (hermes-config-refresh)
        (hermes-config-refresh)
        (hermes--promise-resolve new-request '(nil nil nil))
        (hermes--promise-reject old-request "obsolete failure")))
    (should-not messages)))

(ert-deftest hermes-config-reveal-keeps-newest-value ()
  "An older reveal response cannot overwrite the newer clipboard value."
  (let ((old-request (hermes--promise-make))
        (new-request (hermes--promise-make))
        requests copied)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (let ((request (if requests new-request old-request)))
                   (push request requests)
                   request)))
              ((symbol-function 'kill-new)
               (lambda (value) (setq copied value)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-config-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "OPENAI_API_KEY" 'hermes-env-key
                             "OPENAI_API_KEY")))
        (goto-char (point-min))
        (hermes-config-reveal-env)
        (hermes-config-reveal-env)
        (hermes--promise-resolve
         new-request '((key . "OPENAI_API_KEY") (value . "new-secret")))
        (hermes--promise-resolve
         old-request '((key . "OPENAI_API_KEY") (value . "old-secret")))))
    (should (equal copied "new-secret"))))

(ert-deftest hermes-config-reveal-requires-authoritative-state ()
  "Reveal cannot use a stale environment snapshot."
  (let (requested)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (setq requested t))))
      (with-temp-buffer
        (hermes-config-mode)
        (setq hermes-config--refresh-required t)
        (let ((inhibit-read-only t))
          (insert (propertize "KEY" 'hermes-env-key "KEY")))
        (goto-char (point-min))
        (should-error (hermes-config-reveal-env) :type 'user-error)))
    (should-not requested)))

(ert-deftest hermes-config-retarget-invalidates-displayed-authority ()
  "Retargeting blocks actions until the new instance refresh renders."
  (let ((local '("local" . "http://local"))
        (remote '("remote" . "http://remote"))
        (refresh (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () remote))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-config--fetch) (lambda (_client) refresh))
              ((symbol-function 'hermes-browser--existing-client) (lambda () 'client)))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create "*Hermes Config*")
              (hermes-config-mode)
              (hermes-browser--own-instance local)
              (setq hermes-config--refresh-required nil))
            (hermes-config)
            (with-current-buffer "*Hermes Config*"
              (should hermes-config--refresh-required)))
        (when (get-buffer "*Hermes Config*")
          (kill-buffer "*Hermes Config*"))))))

(ert-deftest hermes-config-edits-serialize-through-authoritative-refresh ()
  "A later full-config edit derives from the first edit's refreshed state."
  (let ((first-put (hermes--promise-make))
        (schema '((fields . ((agent.max_turns . ((type . "number")))
                            (model . ((type . "string")))))))
        (refreshed-config
         '((agent . ((max_turns . 12) (verbose . t))) (model . "gpt")))
        put-bodies
        (reads '("12" "claude")))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'read-string)
               (lambda (&rest _)
                 (prog1 (car reads) (setq reads (cdr reads)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (cond
                  ((equal method "PUT")
                   (push (plist-get args :body) put-bodies)
                   (if (= (length put-bodies) 1)
                       first-put
                     (hermes--promise-resolved '((ok . t)))))
                  ((equal path "/api/config/schema")
                   (hermes--promise-resolved schema))
                  ((equal path "/api/config")
                   (hermes--promise-resolved refreshed-config))
                  ((equal path "/api/env")
                   (hermes--promise-resolved nil))
                  (t (ert-fail (format "Unexpected request: %s %s" method path))))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-config-mode)
        (setq hermes-config--schema schema
              hermes-config--config
              '((agent . ((max_turns . 9) (verbose . t))) (model . "gpt")))
        (let ((inhibit-read-only t))
          (insert (propertize "agent.max_turns" 'hermes-config-key
                             "agent.max_turns")
                  "\n"
                  (propertize "model" 'hermes-config-key "model")))
        (goto-char (point-min))
        (hermes-config-edit)
        (forward-line 1)
        (should-error (hermes-config-edit) :type 'user-error)
        (should (equal reads '("claude")))
        (should (= (length put-bodies) 1))
        (hermes--promise-resolve first-put '((ok . t)))
        (goto-char (point-min))
        (search-forward "model")
        (hermes-config-edit)))
    (let* ((bodies (nreverse put-bodies))
           (first (cdr (assq 'config (nth 0 bodies))))
           (second (cdr (assq 'config (nth 1 bodies)))))
      (should (= (hermes-config--path-value first "agent.max_turns") 12))
      (should (equal (hermes-config--path-value first "model") "gpt"))
      (should (= (hermes-config--path-value second "agent.max_turns") 12))
      (should (equal (hermes-config--path-value second "model") "claude")))))

(ert-deftest hermes-config-failed-authoritative-refresh-blocks-next-mutation ()
  "A successful write cannot be overwritten from a stale local snapshot."
  (let ((write (hermes--promise-make))
        (refresh (hermes--promise-make))
        (fetches 0)
        (reads 0)
        (puts 0))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'read-string)
               (lambda (&rest _) (cl-incf reads) "new"))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method _path &rest _)
                 (if (equal method "PUT")
                     (progn (cl-incf puts) write)
                   (ert-fail (format "Unexpected request: %s" method)))))
              ((symbol-function 'hermes-config--fetch)
               (lambda (_client)
                 (cl-incf fetches)
                 (if (= fetches 1)
                     refresh
                   (hermes--promise-resolved
                    (list '((fields . ((model . ((type . "string"))))))
                          '((model . "authoritative")) nil)))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-config-mode)
        (setq hermes-config--schema
              '((fields . ((model . ((type . "string"))))))
              hermes-config--config '((model . "old")))
        (let ((inhibit-read-only t))
          (insert (propertize "model" 'hermes-config-key "model")))
        (goto-char (point-min))
        (hermes-config-edit)
        (hermes--promise-resolve write '((ok . t)))
        (hermes--promise-reject refresh "refresh failed")
        (should-not hermes-config--mutation-in-flight)
        (should hermes-config--refresh-required)
        (should-error (hermes-config-edit) :type 'user-error)
        (should (= reads 1))
        (should (= puts 1))
        (hermes-config-refresh)
        (should-not hermes-config--refresh-required)
        (should (equal hermes-config--config
                       '((model . "authoritative"))))))))

(ert-deftest hermes-config-mutation-invalidates-older-refresh ()
  "An older read cannot overwrite a mutation's authoritative refresh."
  (let ((old-read (hermes--promise-make))
        (write (hermes--promise-make))
        (fetches 0)
        rendered)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'hermes-config--fetch)
               (lambda (_client)
                 (cl-incf fetches)
                 (if (= fetches 1)
                     old-read
                   (hermes--promise-resolved
                    '(nil ((model . "authoritative")) nil)))))
              ((symbol-function 'hermes-config--render)
               (lambda (_buffer _schema config _env)
                 (setq rendered config)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-config-mode)
        (hermes-config-refresh)
        (hermes-config--run-mutation (current-buffer) (lambda (_client) write))
        (hermes--promise-resolve write '((ok . t)))
        (should (equal rendered '((model . "authoritative"))))
        (hermes--promise-resolve old-read '(nil ((model . "stale")) nil))
        (should (equal rendered '((model . "authoritative"))))))))

(ert-deftest hermes-config-reopen-preserves-pending-mutation ()
  "Reopening the config view preserves its write and authoritative refresh."
  (let ((write (hermes--promise-make))
        (refresh (hermes--promise-make))
        (done-calls 0)
        (fetch-calls 0)
        buffer token)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "new"))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-instance-resolve)
               (lambda () '("local" . "http://127.0.0.1:9119")))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'client (lambda () (cl-incf done-calls)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) write))
              ((symbol-function 'hermes-config--fetch)
               (lambda (_client)
                 (cl-incf fetch-calls)
                 refresh)))
      (setq buffer (get-buffer-create "*Hermes Config*"))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (hermes-config-mode)
              (hermes-browser--own-instance
               '("local" . "http://127.0.0.1:9119"))
              (setq hermes-config--schema
                    '((fields . ((model . ((type . "string"))))))
                    hermes-config--config '((model . "old")))
              (let ((inhibit-read-only t))
                (insert (propertize "model" 'hermes-config-key "model")))
              (goto-char (point-min))
              (hermes-config-edit)
              (setq token hermes-config--mutation-in-flight))
            (hermes-config)
            (with-current-buffer buffer
              (should (eq hermes-config--mutation-in-flight token)))
            (should (= done-calls 0))
            (should (= fetch-calls 0))
            (hermes--promise-resolve write '((ok . t)))
            (should (= fetch-calls 1))
            (hermes--promise-resolve
             refresh
             (list '((fields . ((model . ((type . "string"))))))
                   '((model . "authoritative"))
                   nil))
            (with-current-buffer buffer
              (should (equal hermes-config--config
                             '((model . "authoritative"))))
              (should-not hermes-config--mutation-in-flight))
            (should (= done-calls 1)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-config-reopen-cannot-retarget-pending-mutation ()
  "Reopening for another instance cannot steal an unsettled mutation."
  (let ((write (hermes--promise-make))
        (local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test"))
        buffer token)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "new"))
              ((symbol-function 'hermes-instance-resolve) (lambda () remote))
              ((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'client #'ignore)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) write)))
      (setq buffer (get-buffer-create "*Hermes Config*"))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (hermes-config-mode)
              (hermes-browser--own-instance local)
              (setq hermes-config--schema
                    '((fields . ((model . ((type . "string"))))))
                    hermes-config--config '((model . "old")))
              (let ((inhibit-read-only t))
                (insert (propertize "model" 'hermes-config-key "model")))
              (goto-char (point-min))
              (hermes-config-edit)
              (setq token hermes-config--mutation-in-flight))
            (should-error (hermes-config) :type 'user-error)
            (with-current-buffer buffer
              (should (equal hermes-instance local))
              (should (eq hermes-config--mutation-in-flight token))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-config-mutation-rejection-releases-lock ()
  "A rejected config edit reports once and permits a later edit."
  (let ((first-put (hermes--promise-make)) requests messages)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client))
              ((symbol-function 'read-string) (lambda (&rest _) "12"))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method _path &rest _)
                 (push method requests)
                 (if (= (length requests) 1)
                     first-put
                   (hermes--promise-resolved '((ok . t))))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-config-mode)
        (setq hermes-config--schema
              '((fields . ((agent.max_turns . ((type . "number"))))))
              hermes-config--config '((agent . ((max_turns . 9)))))
        (let ((inhibit-read-only t))
          (insert (propertize "agent.max_turns" 'hermes-config-key
                             "agent.max_turns")))
        (goto-char (point-min))
        (hermes-config-edit)
        (hermes--promise-reject first-put "write failed")
        (should-not hermes-config--mutation-in-flight)
        (hermes-config-edit)))
    (should (= (cl-count "PUT" requests :test #'equal) 2))
    (should (equal messages '("Hermes: write failed")))))

(defun hermes-config-tests--assert-teardown-releases-client (boundary)
  "Assert BOUNDARY releases pending config transactions immediately."
  (dolist (stage '(write refresh))
    (dolist (late-settlement '(resolve reject))
      (let ((put (hermes--promise-make))
            (refresh (hermes--promise-make))
            (done-calls 0)
            (fetch-calls 0)
            (render-calls 0)
            messages
            buffer)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "12"))
                  ((symbol-function 'hermes-browser--with-client)
                   (lambda (fn)
                     (funcall fn 'client
                              (lambda () (cl-incf done-calls)))))
                  ((symbol-function
                    'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _) put))
                  ((symbol-function 'hermes-config--fetch)
                   (lambda (_client)
                     (cl-incf fetch-calls)
                     refresh))
                  ((symbol-function 'hermes-config--render)
                   (lambda (&rest _) (cl-incf render-calls)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (setq buffer (generate-new-buffer " *Hermes config teardown*"))
          (unwind-protect
              (progn
                (with-current-buffer buffer
                  (hermes-config-mode)
                  (setq hermes-config--schema
                        '((fields . ((agent.max_turns . ((type . "number"))))))
                        hermes-config--config
                        '((agent . ((max_turns . 9)))))
                  (let ((inhibit-read-only t))
                    (insert
                     (propertize "agent.max_turns" 'hermes-config-key
                                "agent.max_turns")))
                  (goto-char (point-min))
                  (hermes-config-edit))
                (when (eq stage 'refresh)
                  (hermes--promise-resolve put '((ok . t)))
                  (should (= fetch-calls 1)))
                (pcase boundary
                  ('mode (with-current-buffer buffer (fundamental-mode)))
                  ('kill (kill-buffer buffer)))
                (should (= done-calls 1))
                (let ((pending (if (eq stage 'write) put refresh)))
                  (pcase late-settlement
                    ('resolve
                     (hermes--promise-resolve
                      pending '(nil ((model . "late")) nil)))
                    ('reject (hermes--promise-reject pending "late failure"))))
                (should (= done-calls 1))
                (should (= fetch-calls (if (eq stage 'refresh) 1 0)))
                (should (= render-calls 0))
                (should-not messages)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (hermes-config-mode)
                    (should-not hermes-config--mutation-in-flight))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-config-mode-change-releases-pending-client ()
  "Mode teardown releases writes and authoritative reads before settlement."
  (hermes-config-tests--assert-teardown-releases-client 'mode))

(ert-deftest hermes-config-buffer-kill-releases-pending-client ()
  "Buffer kill releases writes and authoritative reads before settlement."
  (hermes-config-tests--assert-teardown-releases-client 'kill))

(defun hermes-config-test--response (text)
  "Settle a real HTTP response containing TEXT without network access."
  (let ((promise (hermes--promise-make)))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n" text)
      (hermes-dashboard-transport--settle-http-response
       promise nil (current-buffer) "https://example.test/api/config" nil))
    promise))

(defun hermes-config-test--json-value (value)
  "Return order-independent VALUE while keeping JSON container types distinct."
  (cond
   ((hash-table-p value)
    (cons 'object
          (mapcar (lambda (key)
                    (cons key (hermes-config-test--json-value (gethash key value))))
                  (sort (hash-table-keys value) #'string<))))
   ((vectorp value)
    (cons 'array (mapcar #'hermes-config-test--json-value value)))
   (t value)))

(ert-deftest hermes-config-lossless-http-edit-wire ()
  "Both auth paths preserve every untouched JSON type in serialized PUTs."
  (dolist (token '(nil "test-token"))
    (dolist (edit '(("model" "string" "new" "\"new\"")
                    ("enabled" "boolean" nil "false")
                    ("off" "boolean" t "true")
                    ("tools" "list" "[]" "[]")
                    ("tools" "list" "[false,null,[],{},[\"x\"]]"
                     "[false,null,[],{},[\"x\"]]")
                    ("nested.neighbors" "list" unchanged
                     "[false,null,[],{},[\"x\"]]")
                    ("nested.value" "number" "7" "7")))
      (let* ((text "{\"model\":\"old\",\"enabled\":true,\"off\":false,\"optional\":null,\"empty\":[],\"tools\":[\"one\",\"two\"],\"object\":{},\"nested\":{\"value\":1,\"neighbors\":[false,null,[],{},[\"x\"]]}}")
             (client (make-hermes-dashboard-transport-client
                      :token token :base-url "https://example.test"))
             wire fetched
             (hermes-dashboard-transport-http-request-async-function
              (lambda (_url &rest args)
                (if (equal (plist-get args :method) "PUT")
                    (progn (setq wire (plist-get args :data))
                           (hermes-config-test--response "{\"ok\":true}"))
                  (hermes-config-test--response text)))))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _)
                     (hermes--promise-resolved
                      '(:base-url "https://example.test" :headers nil))))
                  ((symbol-function 'hermes-browser--with-client)
                   (lambda (fn) (funcall fn client #'ignore)))
                  ((symbol-function 'read-string)
                   (lambda (_prompt initial &rest _)
                     (if (eq (nth 2 edit) 'unchanged) initial (nth 2 edit))))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (nth 2 edit)))
                  ((symbol-function 'hermes-config--fetch)
                   (lambda (_) (hermes--promise-rejected "refresh failed"))))
          (hermes--promise-then
           (hermes-dashboard-transport-api-request-async
            "GET" "/api/config" :client client)
           (lambda (value) (setq fetched value)))
          (should fetched)
          (with-temp-buffer
            (hermes-config-mode)
            (hermes-config--render
             (current-buffer)
             `((fields . ((,(intern (car edit)) . ((type . ,(nth 1 edit)))))))
             fetched nil)
            (search-forward (car edit))
            (hermes-config-edit)
            (should wire)
            (should hermes-config--refresh-required)
            (should-not hermes-config--mutation-in-flight)
            (should-error (hermes-config-edit) :type 'user-error)))
        ;; Compare independently parsed JSON structurally, ignoring object order.
        (let* ((expected (json-parse-string text))
               (parts (split-string (car edit) "\\."))
               (parent (if (cdr parts) (gethash (car parts) expected) expected)))
          (puthash (car (last parts)) (json-parse-string (nth 3 edit)) parent)
          (should (equal (hermes-config-test--json-value
                          (gethash "config" (json-parse-string wire)))
                         (hermes-config-test--json-value expected))))))))

(ert-deftest hermes-config-lossless-rejects-unproven-response ()
  "Missing raw JSON and malformed or non-object payloads never enable edits."
  (dolist (token '(nil "test-token"))
    (dolist (text '(nil "{" "[]" "null" "false" "{} trailing"))
      (let* ((client (make-hermes-dashboard-transport-client
                      :token token :base-url "https://example.test"))
             written accepted rejected
             (hermes-dashboard-transport-http-request-async-function
              (lambda (_url &rest args)
                (when (equal (plist-get args :method) "PUT") (setq written t))
                (hermes--promise-resolved
                 (list :status 200 :body '((model . "lossy")) :body-text text)))))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _)
                     (hermes--promise-resolved '(:base-url "https://example.test")))))
          (with-temp-buffer
            (hermes-config-mode)
            (hermes--promise-then
             (hermes-dashboard-transport-api-request-async "GET" "/api/config"
                                                            :client client)
             (lambda (config)
               (setq accepted t)
               (hermes-config--render
                (current-buffer) '((fields . ((model . ((type . "string"))))))
                config nil))
             (lambda (_) (setq rejected t)))
            (should rejected)
            (should-not accepted)
            (should-error (hermes-config-edit) :type 'user-error)
            (should-not written)))))))

(provide 'hermes-config-tests)
;;; hermes-config-tests.el ends here
