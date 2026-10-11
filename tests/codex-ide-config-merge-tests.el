;;; codex-ide-config-merge-tests.el --- Installed Codex config checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Opt-in installed CLI proof.  All configuration belongs to disposable
;; fixtures; no editor endpoint is contacted and no real config is modified.
;; Set CODEX_IDE_TEST_CODEX to the installed CLI executable to run this lane.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide)

(defmacro codex-ide-config-merge-tests--with-config (text &rest body)
  "Run BODY with TEXT as disposable Codex config and an installed CLI."
  (declare (indent 1) (debug (form body)))
  `(let ((cli (getenv "CODEX_IDE_TEST_CODEX")))
     (unless cli (ert-skip "Set CODEX_IDE_TEST_CODEX for installed CLI proof"))
     (let* ((root (make-temp-file "codex-ide-config-merge-" t))
            (config (expand-file-name "config.toml" root))
            (process-environment (copy-sequence process-environment))
            (default-directory (file-name-as-directory root))
            (codex-ide-cli-path cli)
            (codex-ide-neomacs-mcp-command cli))
       (unwind-protect
           (progn
             (setenv "CODEX_HOME" root)
             (setenv "HOME" root)
             (setenv "XDG_CACHE_HOME" (expand-file-name "cache" root))
             (with-temp-file config (insert ,text))
             (let ((before (with-temp-buffer
                             (insert-file-contents-literally config)
                             (secure-hash 'sha256 (current-buffer)))))
               ,@body
               (should (equal before (with-temp-buffer
                                       (insert-file-contents-literally config)
                                       (secure-hash 'sha256 (current-buffer)))))))
         (delete-directory root t)))))

(defun codex-ide-config-merge-tests--get (overrides name)
  "Return real Codex server NAME with transient OVERRIDES."
  (with-temp-buffer
    (should (= 0 (apply #'call-process codex-ide-cli-path nil
                        (list (current-buffer) nil) nil
                        (append (mapcan (lambda (pair)
                                          (list "-c" (format "%s=%s" (car pair) (cdr pair))))
                                        overrides)
                                (list "mcp" "get" name "--json")))))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(ert-deftest codex-ide-config-merge-empty-does-not-create-invalid-gnu-entry ()
  (codex-ide-config-merge-tests--with-config ""
    (let* ((overrides (codex-ide-mcp-neo--config-overrides "/fixture/α socket"))
           (entry (codex-ide-config-merge-tests--get overrides "neomacs")))
      (should-not (assoc "mcp_servers.emacs_tools.enabled" overrides))
      (should (equal (alist-get 'type (alist-get 'transport entry)) "stdio"))
      (should (equal (alist-get 'args (alist-get 'transport entry))
                     '("--socket" "/fixture/α socket"))))))

(ert-deftest codex-ide-config-merge-inherited-gnu-is-disabled-transiently ()
  (dolist (transport '("url = \"http://127.0.0.1:43210/mcp\""
                       "command = \"/fixture/gnu-relay\""))
    (codex-ide-config-merge-tests--with-config
        (concat "[mcp_servers.emacs_tools]\n" transport "\n"
                "[mcp_servers.unrelated]\ncommand = \"/fixture/unrelated\"\n")
      (let ((overrides (codex-ide-mcp-neo--config-overrides "/fixture/socket")))
        (should (eq (alist-get 'enabled
                               (codex-ide-config-merge-tests--get overrides "emacs_tools"))
                    :false))
        (should (eq (alist-get 'enabled
                               (codex-ide-config-merge-tests--get overrides "unrelated"))
                    t))))))

(ert-deftest codex-ide-config-merge-native-http-conflict-is-actionable ()
  (codex-ide-config-merge-tests--with-config
      "[mcp_servers.neomacs]\nurl = \"http://127.0.0.1:43210/mcp\"\nenabled = false\n"
    (let ((err (should-error (codex-ide-mcp-neo--config-overrides "/fixture/socket")
                             :type 'user-error)))
      (should (string-match-p "neomacs.*HTTP" (error-message-string err)))
      (should (string-match-p "no config was changed" (error-message-string err))))))

(ert-deftest codex-ide-config-merge-native-policy-is-retained ()
  (codex-ide-config-merge-tests--with-config
      (concat "[mcp_servers.neomacs]\ncommand = \"/fixture/old-relay\"\n"
              "args = [\"--socket\", \"/fixture/old-socket\"]\nenabled = false\n"
              "enabled_tools = [\"neomacs_identity\"]\ndisabled_tools = [\"neomacs_eval\"]\n")
    (let* ((overrides (codex-ide-mcp-neo--config-overrides "/fixture/α socket"))
           (entry (codex-ide-config-merge-tests--get overrides "neomacs")))
      (should (eq (alist-get 'enabled entry) :false))
      (should (equal (alist-get 'enabled_tools entry) '("neomacs_identity")))
      (should (equal (alist-get 'disabled_tools entry) '("neomacs_eval")))
      (should (equal (alist-get 'command (alist-get 'transport entry)) codex-ide-cli-path))
      (should (equal (alist-get 'args (alist-get 'transport entry))
                     '("--socket" "/fixture/α socket"))))))

(defun codex-ide-config-merge-tests--session-command ()
  "Capture the actual session command at the terminal spawn boundary."
  (let ((codex-ide-mcp-enabled t)
        (codex-ide-no-alt-screen nil)
        (codex-ide-yolo nil)
        (codex-ide-ask-for-approval nil)
        (native-feature-p (symbol-function 'featurep))
        command)
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (or (eq feature 'neomacs)
                     (funcall native-feature-p feature subfeature))))
              ((symbol-function 'codex-ide--get-working-directory)
               (lambda () default-directory))
              ((symbol-function 'codex-ide-mcp-ensure-server)
               (lambda () "/fixture/α socket"))
              ((symbol-function 'codex-ide-term--prepare-buffer)
               (lambda (&rest _) (generate-new-buffer " *config fixture*")))
              ((symbol-function 'codex-ide--display-buffer) (lambda (_) t))
              ((symbol-function 'codex-ide-term--make-process)
               (lambda (_buffer program args _env)
                 (setq command (cons program args))
                 (error "Fixture spawn boundary"))))
      (condition-case err
          (codex-ide--create-session 1)
        (error (unless (equal (error-message-string err) "Fixture spawn boundary")
                 (signal (car err) (cdr err))))))
    (should command)
    command))

(ert-deftest codex-ide-config-merge-full-session-argv-preserves-owner-gnu-policy ()
  (dolist (value '(nil "true" "false"))
    (codex-ide-config-merge-tests--with-config
        (concat "[mcp_servers.emacs_tools]\ncommand = \"/fixture/gnu-relay\"\n"
                "[mcp_servers.neomacs]\ncommand = \"/fixture/old-relay\"\nenabled = false\n"
                "[mcp_servers.unrelated]\ncommand = \"/fixture/unrelated\"\n")
      (let* ((codex-ide-cli-extra-args nil)
             (codex-ide-config-overrides
              (when value (list (cons "mcp_servers.emacs_tools.enabled" value))))
             (command (codex-ide-config-merge-tests--session-command)))
        (should (= 1 (cl-count-if
                      (lambda (arg) (string-prefix-p "mcp_servers.emacs_tools.enabled=" arg))
                      (cdr command))))
        (should (member (concat "mcp_servers.emacs_tools.enabled=" (or value "false"))
                        (cdr command)))
        (with-temp-buffer
          (should (= 0 (apply #'call-process (car command) nil
                              (list (current-buffer) nil) nil
                              (append (cdr command) '("mcp" "list" "--json")))))
          (goto-char (point-min))
          (let* ((servers (json-parse-buffer :object-type 'alist :array-type 'list))
                 (gnu (seq-find (lambda (entry) (equal (alist-get 'name entry) "emacs_tools")) servers))
                 (native (seq-find (lambda (entry) (equal (alist-get 'name entry) "neomacs")) servers))
                 (other (seq-find (lambda (entry) (equal (alist-get 'name entry) "unrelated")) servers)))
            (should (eq (alist-get 'enabled gnu) (if (equal value "true") t :false)))
            (should (eq (alist-get 'enabled other) t))
            (should (eq (alist-get 'enabled native) :false))
            (should (equal (alist-get 'args (alist-get 'transport native))
                           '("--socket" "/fixture/α socket")))))))))

(ert-deftest codex-ide-config-merge-full-argv-profile-and-late-config-refuse ()
  (dolist (fixture '(("[mcp_servers.emacs_tools]\ncommand = \"/fixture/gnu-relay\"\n" "-p" "review")
                      ("[mcp_servers.neomacs]\nurl = \"http://127.0.0.1:43210/mcp\"\n" "--profile=review")
                      ("" "-c" "mcp_servers.neomacs.url=\"http://127.0.0.1:43210/mcp\"")))
    (codex-ide-config-merge-tests--with-config ""
      (let* ((profile (expand-file-name "review.config.toml" root))
             (codex-ide-cli-extra-args (cdr fixture))
             (codex-ide-config-overrides
              '(("mcp_servers.neomacs.command" . "\"/fixture/relay\"")
                ("mcp_servers.neomacs.args" . "[\"--socket\", \"/fixture/socket\"]")))
             (codex-ide-no-alt-screen nil)
             (codex-ide-yolo nil)
             (codex-ide-ask-for-approval nil))
        (with-temp-file profile (insert (car fixture)))
        (let ((before (with-temp-buffer
                        (insert-file-contents-literally profile)
                        (secure-hash 'sha256 (current-buffer))))
              (command (codex-ide--build-command)))
          ;; Establish the actual CLI hazard at full argv precedence, not
          ;; merely the config-only helper: GNU survives, or HTTP won't merge.
          (with-temp-buffer
            (let ((status (apply #'call-process (car command) nil
                                 (list (current-buffer) nil) nil
                                 (append (cdr command) '("mcp" "list" "--json")))))
              (if (string-match-p "emacs_tools" (car fixture))
                  (progn
                    (should (= status 0))
                    (goto-char (point-min))
                    (let* ((servers (json-parse-buffer :object-type 'alist :array-type 'list))
                           (gnu (seq-find (lambda (entry) (equal (alist-get 'name entry) "emacs_tools")) servers))
                           (native (seq-find (lambda (entry) (equal (alist-get 'name entry) "neomacs")) servers)))
                      (should (eq (alist-get 'enabled gnu) t))
                      (should (eq (alist-get 'enabled native) t))))
                (should-not (= status 0)))))
          (let ((err (should-error (codex-ide-config-merge-tests--session-command)
                                   :type 'user-error)))
            (should (string-match-p "codex-ide-config-overrides" (error-message-string err))))
          (should (equal before (with-temp-buffer
                                  (insert-file-contents-literally profile)
                                  (secure-hash 'sha256 (current-buffer))))))))))


(ert-deftest codex-ide-config-merge-gnu-bearer-env-key ()
  "Installed Codex accepts the transient bearer environment variable key."
  (codex-ide-config-merge-tests--with-config ""
    (let* ((overrides (codex-ide-mcp-config-overrides "http://127.0.0.1:43210/mcp"))
           (entry (codex-ide-config-merge-tests--get overrides "emacs_tools")))
      (should (equal (alist-get 'bearer_token_env_var (alist-get 'transport entry))
                     "CODEX_IDE_MCP_TOKEN")))))

(ert-deftest codex-ide-config-merge-reloads-ancestor-layer ()
  "Starts in a nested trusted project see new ancestor config layers."
  (codex-ide-config-merge-tests--with-config
      (format "project_root_markers = [\"WORKSPACE\"]\n[projects.%s]\ntrust_level = \"trusted\"\n"
              (json-serialize (expand-file-name "project" root)))
    (let* ((project (expand-file-name "project" root))
           (default-directory (file-name-as-directory
                               (expand-file-name "sub/nested" project)))
           (ancestor (expand-file-name ".codex/config.toml" project)))
      (make-directory default-directory t)
      (with-temp-file (expand-file-name "WORKSPACE" project))
      (make-directory (file-name-directory ancestor))
      (should-not (assoc "mcp_servers.emacs_tools.enabled"
                         (codex-ide-mcp-neo--config-overrides "/fixture/socket")))
      (with-temp-file ancestor
        (insert "[mcp_servers.emacs_tools]\ncommand = \"/fixture/gnu\"\n"))
      (should (assoc "mcp_servers.emacs_tools.enabled"
                     (codex-ide-mcp-neo--config-overrides "/fixture/socket")))
      (with-temp-file ancestor
        (insert "[mcp_servers.neomacs]\nurl = \"http://127.0.0.1:43210/mcp\"\n"))
      (should-error (codex-ide-mcp-neo--config-overrides "/fixture/socket")
                    :type 'user-error))))

(provide 'codex-ide-config-merge-tests)
;;; codex-ide-config-merge-tests.el ends here
