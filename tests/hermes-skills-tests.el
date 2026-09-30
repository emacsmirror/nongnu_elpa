;;; hermes-skills-tests.el --- Skill content and Hub tests -*- lexical-binding: t; -*-

(require 'hermes-test-helpers)
(require 'hermes-inventory)
(require 'hermes-skills)

(defconst hermes-skills-tests--source
  "---\nname: sample\ndescription: Example\n---\nΚαλημέρα 世界\n\nLocal Variables:\neval: (error \"must remain inert\")\nEnd:\n")

(defun hermes-skills-tests--backend (respond run &optional url)
  "Call RUN with isolated acquisition and RESPOND at the HTTP boundary.
Optional URL selects a real disposable server.  Real REST serialization,
authentication guards, browser acquisition and ownership remain in use."
  (let* ((url (or url "http://skills.example.test"))
         (hermes-instances `((:id "skills" :name "Skills" :url ,url)))
         (hermes-dashboard-transport-url url)
         (client (make-hermes-dashboard-transport-client :base-url url))
         (before (buffer-list))
         (acquired 0) (released 0))
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                  ((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _)
                     (should (equal (hermes-instance-url hermes-instance) url))
                     (cl-incf acquired) client))
                  ((symbol-function 'hermes-dashboard-transport-release)
                   (lambda (value) (should (eq value client)) (cl-incf released)))
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda () (hermes--promise-resolved `(:base-url ,url)))))
          (let ((hermes-dashboard-transport-http-request-async-function
                 (or respond hermes-dashboard-transport-http-request-async-function)))
            (save-window-excursion (funcall run client))
            (should (> acquired 0))
            (should (= acquired released))))
      (dolist (buffer (seq-difference (buffer-list) before))
        (when (buffer-live-p buffer)
          (when-let* ((process (get-buffer-process buffer)))
            (delete-process process))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(defun hermes-skills-tests--content-response (&optional content)
  "Return a native HTTP content response containing CONTENT."
  (hermes--promise-resolved
   `(:body ((name . "sample") (content . ,(or content hermes-skills-tests--source))
            (path . "/remote/never-visit/SKILL.md")))))

(ert-deftest hermes-skills-native-entry-contract ()
  "Installed inventory exposes content and a separate passive Hub."
  (should (eq (keymap-lookup hermes-inventory-mode-map "RET")
              'hermes-inventory-skill-content))
  (should (eq (keymap-lookup hermes-inventory-mode-map "H") 'hermes-skills-hub)))

(ert-deftest hermes-skills-installed-native-edit-save-readback-discard ()
  "Selected inventory opens inert Unicode text; native edits save exact bytes."
  (let ((stored hermes-skills-tests--source) requests)
    (hermes-skills-tests--backend
     (lambda (url &rest args)
       (push (cons url args) requests)
       (if (equal (plist-get args :method) "PUT")
           (let ((body (json-parse-string (decode-coding-string (plist-get args :data) 'utf-8)
                                          :object-type 'alist)))
             (should (equal (alist-get 'name body) "sample"))
             (should-not (assq 'profile body))
             (setq stored (alist-get 'content body))
             (hermes--promise-resolved '(:body ((success . t)))))
         (hermes-skills-tests--content-response stored)))
     (lambda (_client)
       (let ((inventory (hermes-buffer--get " *skills inventory*" #'hermes-inventory-mode)))
         (switch-to-buffer inventory)
         (hermes-inventory--render (assoc "Skills" hermes-inventory--specs)
                                   '(("sample" ["test" "sample" "on" "metadata, not content"])) inventory)
         (goto-char (point-min))
         (execute-kbd-macro (kbd "RET"))
         (should (eq major-mode 'hermes-skill-content-mode))
         (should buffer-read-only)
         (should-not buffer-file-name)
         (should-not buffer-auto-save-file-name)
         (should (equal (buffer-string) stored))
         (execute-kbd-macro (kbd "C-c C-e M->"))
         (execute-kbd-macro "additional text\n")
         (let ((draft (buffer-string)))
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (prompt)
                        (should (string-match-p "Concurrent edits may be overwritten" prompt)) t)))
             (execute-kbd-macro (kbd "C-c C-c")))
           (should (equal stored draft))
           (should-not (buffer-modified-p))
           (should (string-match-p "Saved and read back" hermes-browser--status))
           (should (equal (mapcar (lambda (r) (plist-get (cdr r) :method))
                                  (reverse requests)) '("GET" "PUT" "GET")))
           (insert "not saved")
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (execute-kbd-macro (kbd "C-c C-k")))
           (should (equal (buffer-string) draft))
           (should buffer-read-only)))))))

(ert-deftest hermes-skills-save-failures-retain-draft-without-retry ()
  "Validation, missing skill, guard refusal and uncertain write preserve drafts."
  (dolist (failure '(400 404 500 readback mismatch))
    (let ((writes 0) (reads 0))
      (hermes-skills-tests--backend
       (lambda (_url &rest args)
         (if (equal (plist-get args :method) "PUT")
             (progn (cl-incf writes)
                    (if (numberp failure)
                        (hermes--promise-rejected (format "HTTP %s backend refusal" failure))
                      (hermes--promise-resolved '(:body ((success . t))))))
           (cl-incf reads)
           (cond ((and (> reads 1) (eq failure 'readback))
                  (hermes--promise-rejected "connection lost"))
                 ((and (> reads 1) (eq failure 'mismatch))
                  (hermes-skills-tests--content-response "concurrent edit"))
                 (t (hermes-skills-tests--content-response)))))
       (lambda (_client)
         (hermes-skill-content "sample" "Research")
         (hermes-skill-content-edit)
         (goto-char (point-max)) (insert "draft\n")
         (let ((draft (buffer-string)))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (hermes-skill-content-save))
           (should (= writes 1))
           (should (equal draft (buffer-string)))
           (should (buffer-modified-p))
           (should-not hermes-skills--busy)
           (should (string-match-p "retained" hermes-browser--status))))))))

(defun hermes-skills-tests--replace-owner (kind)
  "Replace the current skill owner according to KIND."
  (pcase kind
    ('instance (setq hermes-instance '("Other" . "http://other.example.test")))
    ('profile (setq hermes-skills--profile "Other"))
    ('name (setq hermes-skills--name "other"))
    ('claim (setq hermes-buffer--owner (cons t major-mode)))
    ('file (let ((enable-local-variables nil) (enable-local-eval nil))
             (set-visited-file-name (expand-file-name "owned-test.txt" temporary-file-directory))
             (set-visited-file-name nil)))
    ('generation (hermes-browser--next-request-generation))))

(ert-deftest hermes-skills-save-consent-and-delayed-auth-fence-owner ()
  "No write reaches HTTP after recursive consent or delayed auth changes owner."
  (dolist (stage '(consent auth))
    (dolist (kind '(instance profile name claim file generation))
      (let ((writes 0) (auth (hermes--promise-make)))
        (hermes-skills-tests--backend
         (lambda (_url &rest args)
           (when (equal (plist-get args :method) "PUT") (cl-incf writes))
           (hermes-skills-tests--content-response))
         (lambda (_client)
           (hermes-skill-content "sample" "Research")
           (hermes-skill-content-edit)
           (goto-char (point-max)) (insert "draft")
           (let ((draft (buffer-string)))
             (cl-letf (((symbol-function 'yes-or-no-p)
                        (lambda (&rest _)
                          (when (eq stage 'consent) (hermes-skills-tests--replace-owner kind)) t))
                       ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                        (lambda () auth)))
               (hermes-skill-content-save)
               (when (eq stage 'auth) (hermes-skills-tests--replace-owner kind))
               (hermes--promise-resolve auth '(:base-url "http://skills.example.test")))
             (should (= writes 0))
             (should (equal draft (buffer-string)))
             (should (buffer-modified-p))
             (should-not hermes-skills--busy))))))))

(ert-deftest hermes-skills-save-auth-cancel-and-same-owner-control ()
  "Authentication cancellation retains text; an unchanged owner sends exact scope."
  (dolist (cancel '(t nil))
    (let ((auth (hermes--promise-make)) (writes 0) body)
      (hermes-skills-tests--backend
       (lambda (url &rest args)
         (should (string-match-p "profile=Research" url))
         (when (equal (plist-get args :method) "PUT")
           (cl-incf writes)
           (setq body (json-parse-string (plist-get args :data) :object-type 'alist)))
         (hermes-skills-tests--content-response))
       (lambda (_client)
         (hermes-skill-content "sample" "Research")
         (hermes-skill-content-edit)
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                   ((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth)))
           (hermes-skill-content-save)
           (if cancel (hermes--promise-reject auth "Cancelled")
             (hermes--promise-resolve auth '(:base-url "http://skills.example.test"))))
         (should (= writes (if cancel 0 1)))
         (unless cancel (should (equal (alist-get 'profile body) "Research")))
         (should-not hermes-skills--busy))))))

(ert-deftest hermes-skills-save-readback-never-erases-newer-draft ()
  "Typing during a pending write/readback cannot be overwritten by its receipt."
  (let ((write (hermes--promise-make)))
    (hermes-skills-tests--backend
     (lambda (_url &rest args)
       (if (equal (plist-get args :method) "PUT") write
         (hermes-skills-tests--content-response)))
     (lambda (_client)
       (hermes-skill-content "sample") (hermes-skill-content-edit)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (hermes-skill-content-save))
       (goto-char (point-max)) (insert "newer draft")
       (hermes--promise-resolve write '(:body ((success . t))))
       (should (string-suffix-p "newer draft" (buffer-string)))
       (should (buffer-modified-p))
       (should (string-match-p "draft retained" hermes-browser--status))))))

(defconst hermes-skills-tests--hub-rows
  '(((name . "Identical") (source . "official") (identifier . "official/a/same")
     (trust_level . "builtin") (repo . "Nous/skills"))
    ((name . "Identical") (source . "github") (identifier . "github:other/repo/same")
     (trust_level . "community") (repo . "other/repo"))))

(ert-deftest hermes-skills-hub-real-http-collisions-partial-preview-explicit-scan ()
  "Real HTTP rows keep source identity; only explicit informed scan runs it."
  (let (requests)
    (hermes-test--with-http-server
     (lambda (peer request)
       (push request requests)
       (let ((body
              (cond
               ((string-match-p "/sources" request)
                '((sources . (((id . "github") (label . "GitHub") (searchable . t))))))
               ((string-match-p "/official" request) `((skills . ,hermes-skills-tests--hub-rows)))
               ((string-match-p "/search" request)
                `((results . ,hermes-skills-tests--hub-rows) (timed_out . ("clawhub"))))
               ((string-match-p "/preview" request)
                (append (cadr hermes-skills-tests--hub-rows)
                        `((skill_md . ,hermes-skills-tests--source) (files . ("SKILL.md" "scripts/unreviewed.sh")))))
               ((string-match-p "/scan" request)
                '((identifier . "github:other/repo/same") (policy . "ask") (tier1 . nil)))
               (t (ert-fail request)))))
         (hermes-test--http-reply peer 200 (json-encode body))))
     (lambda (url)
       (hermes-skills-tests--backend
        nil
        (lambda (_client)
          (hermes-skills-hub "Research")
          (hermes-test--http-wait (lambda () (equal (length tabulated-list-entries) 2)))
          (should-not (equal (caar tabulated-list-entries) (caadr tabulated-list-entries)))
          (should (equal (aref (cadar tabulated-list-entries) 0)
                         (aref (cadadr tabulated-list-entries) 0)))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "same")))
            (hermes-skills-hub-search))
          (hermes-test--http-wait (lambda () (string-match-p "Partial" hermes-browser--status)))
          (should (string-match-p "clawhub" hermes-browser--status))
          (goto-char (point-min)) (forward-line 1)
          (execute-kbd-macro (kbd "f"))
          (hermes-test--http-wait (lambda () (string-match-p "unreviewed.sh" (buffer-string))))
          (should (equal hermes-skills--candidate '("github" "github:other/repo/same")))
          (should buffer-read-only)
          (should (string-match-p "contents NOT reviewed" (buffer-string)))
          (should (string-match-p "must remain inert" (buffer-string)))
          (should-not (seq-some (lambda (r) (string-match-p "/scan" r)) requests))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (should (string-match-p "external scanner" prompt)) t)))
            (execute-kbd-macro (kbd "S")))
          (hermes-test--http-wait (lambda () (string-match-p "Policy: ask" (buffer-string))))
          (should (string-match-p "Absent/unavailable/failed" (buffer-string)))
          (should (string-match-p "not a security guarantee" (buffer-string)))
          (should-not (seq-some (lambda (r) (string-match-p "install\\|PUT\\|POST" r)) requests))
          (should (seq-every-p (lambda (r) (string-match-p "profile=Research" r)) requests)))
        url)))))

(ert-deftest hermes-skills-hub-pending-preview-fences-selection-profile-claim ()
  "Pending candidate replies cannot repaint after source/owner replacement."
  (dolist (change '(selection profile claim instance))
    (let ((preview (hermes--promise-make)) hub target)
      (hermes-skills-tests--backend
       (lambda (url &rest _)
         (cond ((string-match-p "/sources" url) (hermes--promise-resolved '(:body ((sources . nil)))))
               ((string-match-p "/official" url)
                (hermes--promise-resolved `(:body ((skills . ,hermes-skills-tests--hub-rows)))))
               (t preview)))
       (lambda (_client)
         (setq hub (hermes-skills-hub))
         (goto-char (point-min))
         (setq target (hermes-skills-hub-preview))
         (if (eq change 'selection)
             (with-current-buffer hub (goto-char (point-min)) (forward-line 1))
           (hermes-skills-tests--replace-owner change))
         (hermes--promise-resolve preview
                                 `(:body ,(append (car hermes-skills-tests--hub-rows)
                                                  '((skill_md . "must not render") (files . nil)))))
         (with-current-buffer target (should-not (string-match-p "must not render" (buffer-string)))))))))

(ert-deftest hermes-skills-hub-scan-policy-and-advisory-labels ()
  "Allow, ask and block remain distinct; missing advisory is not a pass."
  (dolist (policy '("allow" "ask" "block"))
    (let ((text (hermes-skills-hub--scan-text `((policy . ,policy) (tier1 . nil)))))
      (should (string-match-p (concat "Policy: " policy) text))
      (should (string-match-p "not a security guarantee" text))
      (should (string-match-p "Absent/unavailable/failed" text)))))

(ert-deftest hermes-skills-hub-unsupported-and-empty-are-usable ()
  "Unavailable REST never falls back to a local command or ambiguous RPC."
  (dolist (empty '(t nil))
    (hermes-skills-tests--backend
     (lambda (_url &rest _)
       (if empty (hermes--promise-resolved '(:body ((sources . nil) (skills . nil))))
         (hermes--promise-rejected "HTTP 404 unsupported")))
     (lambda (_client)
       (hermes-skills-hub)
       (should (string-match-p (if empty "No candidates" "Hub unavailable") hermes-browser--status))
       (should (commandp (key-binding (kbd "s"))))
       (should (commandp (key-binding (kbd "P"))))))))

(ert-deftest hermes-skills-native-save-cancel-keeps-draft ()
  "Native C-g dismisses save confirmation without an HTTP mutation."
  (let ((writes 0) prompt)
    (hermes-skills-tests--backend
     (lambda (_url &rest args)
       (when (equal (plist-get args :method) "PUT") (cl-incf writes))
       (hermes-skills-tests--content-response))
     (lambda (_client)
       (hermes-skill-content "sample") (hermes-skill-content-edit)
       (goto-char (point-max)) (insert "retained draft")
       (let ((draft (buffer-string)) (noninteractive nil))
         (minibuffer-with-setup-hook
             (lambda () (setq prompt (minibuffer-prompt)))
           (condition-case nil (execute-kbd-macro (kbd "C-c C-c C-g"))
             (quit nil)))
         (should prompt)
         (should (= writes 0))
         (should (equal draft (buffer-string)))
         (should-not hermes-skills--busy))))))

(ert-deftest hermes-skills-readback-and-scan-auth-retirement ()
  "Delayed readback and scan authentication do not escape profile ownership."
  (dolist (action '(readback scan))
    (let ((auth (hermes--promise-make)) (requests 0) (auth-count 0))
      (hermes-skills-tests--backend
       (lambda (_url &rest _)
         (cl-incf requests)
         (hermes-skills-tests--content-response))
       (lambda (_client)
         (hermes-skill-content "sample" "Research")
         (if (eq action 'readback)
             (hermes-skill-content-edit)
           (hermes-skill-preview-mode)
           (hermes-buffer--claim 'hermes-skill-preview-mode)
           (setq hermes-skills--profile "Research"
                 hermes-skills--candidate '("github" "github:owner/repo/skill")))
         (let ((before requests))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                      (lambda ()
                        (if (and (eq action 'readback) (= (cl-incf auth-count) 1))
                            (hermes--promise-resolved '(:base-url "http://skills.example.test"))
                          auth))))
             (if (eq action 'readback) (hermes-skill-content-save)
               (hermes-skills-hub-scan))
             (setq hermes-skills--profile "Other")
             (hermes--promise-resolve auth '(:base-url "http://skills.example.test")))
           (should (= requests (+ before (if (eq action 'readback) 1 0))))))))))

(ert-deftest hermes-skills-save-consent-rejects-changed-text ()
  "Recursive input cannot silently save an obsolete draft snapshot."
  (let ((writes 0))
    (hermes-skills-tests--backend
     (lambda (_url &rest args)
       (when (equal (plist-get args :method) "PUT") (cl-incf writes))
       (hermes-skills-tests--content-response))
     (lambda (_client)
       (hermes-skill-content "sample") (hermes-skill-content-edit)
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (&rest _) (goto-char (point-max)) (insert "new draft") t)))
         (should-error (hermes-skill-content-save) :type 'user-error))
       (should (string-suffix-p "new draft" (buffer-string)))
       (should (= writes 0))))))

(defun hermes-skills-tests--publication (kind run)
  "Hold KIND's real publication, then call RUN with its delivery thunk."
  (let ((pending (hermes--promise-make)) (held nil))
    (hermes-skills-tests--backend
     (lambda (url &rest _)
       (cond (held pending)
             ((string-match-p "/sources" url)
              (hermes--promise-resolved '(:body ((sources . nil)))))
             ((string-match-p "/official" url)
              (hermes--promise-resolved `(:body ((skills . ,hermes-skills-tests--hub-rows)))))
             ((string-match-p "/preview" url)
              (hermes--promise-resolved `(:body ,(car hermes-skills-tests--hub-rows))))
             (t (hermes-skills-tests--content-response))))
     (lambda (_client)
       (pcase kind
         ((or 'read 'discard) (hermes-skill-content "sample"))
         ('catalog (hermes-skills-hub))
         (_ (hermes-skills-hub) (goto-char (point-min)) (hermes-skills-hub-preview)))
       (setq held t)
       (pcase kind
         ('read (hermes-skill-content-refresh))
         ('catalog (hermes-skills-hub-refresh))
         ('preview (hermes-skills-hub-preview-refresh))
         ('scan (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                  (hermes-skills-hub-scan)))
         ('discard (hermes-skill-content-edit) (insert "draft")))
       (funcall run
                (lambda ()
                  (if (eq kind 'discard)
                      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                        (hermes-skill-content-discard))
                    (hermes--promise-resolve
                     pending
                     `(:body ,(pcase kind
                                ('read '((name . "sample") (content . "remote instructions")))
                                ('catalog '((sources . nil)))
                                ('preview (append (car hermes-skills-tests--hub-rows)
                                                  '((skill_md . "remote instructions"))))
                                ('scan '((identifier . "official/a/same") (policy . "ask")))))))))))))

(ert-deftest hermes-skills-publication-native-retirement-preserves-successor ()
  "Before/after-change retirement cannot overwrite a foreign draft or flags."
  (dolist (kind '(read discard preview scan catalog))
    (dolist (phase '(before-change-functions after-change-functions))
      (dolist (exit '(nil error quit))
        (hermes-skills-tests--publication
         kind
         (lambda (deliver)
           (let (called)
             (add-hook
              phase
              (lambda (&rest _)
                (setq called t)
                (fundamental-mode)
                (let ((inhibit-modification-hooks t) (inhibit-read-only t))
                  (erase-buffer) (insert "foreign successor draft"))
                (setq buffer-read-only nil)
                (setq-local hermes-browser--status "successor status")
                (when exit (signal exit '("hook exit"))))
              nil t)
             (condition-case nil (funcall deliver) ((error quit) nil))
             (should called)
             (should (eq major-mode 'fundamental-mode))
             (should (equal (buffer-string) "foreign successor draft"))
             (should (buffer-modified-p))
             (should-not buffer-read-only)
             (should (equal hermes-browser--status "successor status")))))))))

(ert-deftest hermes-skills-publication-same-owner-errors-rollback ()
  "Native hook error and quit retain accepted text, flags and undo history."
  (dolist (kind '(read discard preview scan catalog))
    (dolist (phase '(before-change-functions after-change-functions))
      (dolist (exit '(error quit))
        (hermes-skills-tests--publication
         kind
         (lambda (deliver)
           (buffer-enable-undo)
           (let ((text (buffer-string)) (modified (buffer-modified-p))
                 (readonly buffer-read-only) (undo buffer-undo-list) called)
             (add-hook phase
                       (lambda (&rest _)
                         (setq called t)
                         (signal exit '("hook exit"))) nil t)
             (condition-case nil (funcall deliver) ((error quit) nil))
             (should called)
             (should (equal (buffer-string) text))
             (should (eq (buffer-modified-p) modified))
             (should (eq buffer-read-only readonly))
             (should (equal buffer-undo-list undo)))))))))

(ert-deftest hermes-skills-hub-source-flags-real-decoder ()
  "Native REST JSON decoding preserves present false versus absent flags."
  (hermes-skills-tests--backend
   (lambda (url &rest _)
     (hermes--promise-resolved
      (list :body
            (hermes-dashboard-transport--json-body
             (if (string-match-p "/sources" url)
                 "{\"sources\":[{\"id\":\"false\",\"label\":\"False\",\"available\":false,\"searchable\":false,\"rate_limited\":false},{\"id\":\"true\",\"label\":\"True\",\"available\":true,\"searchable\":true,\"rate_limited\":true},{\"id\":\"missing\",\"label\":\"Missing\"}]}"
               "{\"skills\":[]}")))))
   (lambda (_client)
     (hermes-skills-hub) (hermes-skills-hub-sources)
     (dolist (value '("no" "yes" "unknown"))
       (should (string-match-p
                (format "available: %s; searchable: %s; rate limited: %s" value value value)
                (buffer-string)))))))

(ert-deftest hermes-skills-hub-search-budget-survives-auth ()
  "Search carries a bounded route budget across deferred native authentication."
  (dolist (retire '(nil t))
    (let ((auth (hermes--promise-make)) (auth-count 0) requests)
      (hermes-skills-tests--backend
       (lambda (url &rest args)
         (push (cons url args) requests)
         (hermes--promise-resolved '(:body ((skills . nil) (sources . nil) (results . nil)))))
       (lambda (_client)
         (hermes-skills-hub)
         (setq requests nil)
         (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "test"))
                   ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                    (lambda ()
                      (if (= (cl-incf auth-count) 1)
                          (hermes--promise-resolved '(:base-url "http://skills.example.test"))
                        auth))))
           (hermes-skills-hub-search))
         (should (= auth-count 2))
         (when retire (fundamental-mode))
         (hermes--promise-resolve auth '(:base-url "http://skills.example.test"))
         (let ((search (seq-find (lambda (r) (string-match-p "/search" (car r))) requests)))
           (if retire (should-not search)
             (should search)
             (should (equal (plist-get (cdr search) :timeout) 35))))
         (should (= hermes-dashboard-transport-http-timeout 30)))))))

(ert-deftest hermes-skills-initial-publication-native-before-change ()
  "An initial empty editor cannot append remote text to its successor draft."
  (let ((pending (hermes--promise-make)))
    (hermes-skills-tests--backend
     (lambda (&rest _) pending)
     (lambda (_client)
       (hermes-skill-content "sample")
       (add-hook 'before-change-functions
                 (lambda (&rest _)
                   (fundamental-mode)
                   (insert "foreign successor draft")
                   (setq buffer-read-only nil)
                   (setq-local hermes-browser--status "successor")) nil t)
       (hermes--promise-resolve pending '(:body ((name . "sample") (content . "remote instructions"))))
       (should (equal (buffer-string) "foreign successor draft"))
       (should (buffer-modified-p))
       (should-not buffer-read-only)
       (should (equal hermes-browser--status "successor"))))))

(ert-deftest hermes-skills-publication-reentry-and-local-edit ()
  "A new request or native edit during either hook phase supersedes publication."
  (dolist (phase '(before-change-functions after-change-functions))
    (dolist (replace '(nil generation operation))
      (hermes-skills-tests--publication
       'read
       (lambda (deliver)
         (let (called)
           (add-hook
            phase
            (lambda (&rest _)
              (unless called
                (setq called t)
                (pcase replace
                  ('generation (hermes-browser--next-request-generation))
                  ('operation (hermes-browser--retire-owned)))
                (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                  (erase-buffer) (insert "newer local text"))
                (setq buffer-read-only nil hermes-browser--status "newer status"))) nil t)
           (funcall deliver)
           (should called)
           (should (equal (buffer-string) "newer local text"))
           (should (buffer-modified-p))
           (should-not buffer-read-only)
           (should (equal hermes-browser--status "newer status"))))))))

(ert-deftest hermes-skills-sources-publication-native-retirement ()
  "Configured source rendering respects native modification-hook retirement."
  (hermes-skills-tests--backend
   (lambda (&rest _) (hermes--promise-resolved '(:body ((sources . nil) (skills . nil)))))
   (lambda (_client)
     (hermes-skills-hub)
     (let ((hermes-skill-preview-mode-hook
            (list (lambda ()
                    (add-hook 'before-change-functions
                              (lambda (&rest _)
                                (fundamental-mode) (insert "source successor")
                                (setq buffer-read-only nil)
                                (setq-local hermes-browser--status "source status")) nil t)))))
       (hermes-skills-hub-sources))
     (should (equal (buffer-string) "source successor"))
     (should (buffer-modified-p))
     (should-not buffer-read-only)
     (should (equal hermes-browser--status "source status")))))

(ert-deftest hermes-skills-hub-search-real-deadline-partial ()
  "A released 30-second fanout plus response overhead still renders partial rows."
  (let (reply-timer (searches 0))
    (unwind-protect
        (hermes-test--with-http-server
         (lambda (peer request)
           (cond ((string-match-p "/search" request)
                  (cl-incf searches)
                  (setq reply-timer
                        (run-at-time 30.1 nil
                                     (lambda ()
                                       (when (process-live-p peer)
                                         (hermes-test--http-reply
                                          peer 200 (json-encode
                                                    `((results . ,hermes-skills-tests--hub-rows)
                                                      (timed_out . ("clawhub"))))))))))
                 (t (hermes-test--http-reply peer 200 "{\"sources\":[],\"skills\":[]}"))))
         (lambda (url)
           (hermes-skills-tests--backend
            nil
            (lambda (_client)
              (hermes-skills-hub)
              (hermes-test--http-wait (lambda () (not (equal hermes-browser--status "Loading"))))
              (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "slow")))
                (hermes-skills-hub-search))
              (let ((deadline (+ (float-time) 38)))
                (while (and (equal hermes-browser--status "Loading") (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should (string-match-p "Partial results; timed out: clawhub" hermes-browser--status))
              (should (= (length tabulated-list-entries) 2))
              (should (= searches 1))) url)))
      (when reply-timer (cancel-timer reply-timer)))))

(ert-deftest hermes-skills-hub-search-over-budget-and-retirement ()
  "The actual HTTP deadline rejects once; retirement prevents late publication."
  (dolist (retire '(nil t))
    (let (timeout-callback timeout-args timeout-timer peer (searches 0)
          (native-run-at-time (symbol-function 'run-at-time)))
      (hermes-test--with-http-server
       (lambda (client request)
         (if (string-match-p "/search" request)
             (setq peer client searches (1+ searches))
           (hermes-test--http-reply client 200 "{\"sources\":[],\"skills\":[]}")))
       (lambda (url)
         (hermes-skills-tests--backend
          nil
          (lambda (_client)
            (hermes-skills-hub)
            (hermes-test--http-wait (lambda () (not (equal hermes-browser--status "Loading"))))
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "slow"))
                      ((symbol-function 'run-at-time)
                       (lambda (time repeat function &rest args)
                         (let ((timer (apply native-run-at-time time repeat function args)))
                           (when (equal time 35)
                             (setq timeout-callback function timeout-args args timeout-timer timer))
                           timer))))
              (hermes-skills-hub-search)
              (hermes-test--http-wait (lambda () peer)))
            (should timeout-callback)
            (when retire
              (fundamental-mode) (setq buffer-read-only nil)
              (insert "search successor")
              (setq-local hermes-browser--status "successor"))
            ;; Advance the actual url.el request deadline deterministically.
            (cancel-timer timeout-timer)
            (apply timeout-callback timeout-args)
            (when (process-live-p peer)
              (hermes-test--http-reply peer 200
                                      (json-encode `((results . ,hermes-skills-tests--hub-rows))))
              (accept-process-output nil 0.05))
            (if retire
                (progn (should (equal (buffer-string) "search successor"))
                       (should (equal hermes-browser--status "successor"))
                       (should (buffer-modified-p)))
              (should (string-match-p "Hub unavailable" hermes-browser--status))
              (should-not tabulated-list-entries))
            (should (= searches 1))) url))))))

(provide 'hermes-skills-tests)
;;; hermes-skills-tests.el ends here
