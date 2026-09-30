;;; hermes-foreign-context-tests.el --- Foreign history and context journeys -*- lexical-binding: t; -*-

;;; Code:
(require 'hermes-test-helpers)
(require 'hermes-sessions)
(require 'hermes-foreign)
(require 'hermes-context)

(ert-deftest hermes-foreign-context-public-entry ()
  (with-temp-buffer
    (hermes-sessions-mode)
    (should (eq (key-binding (kbd "F")) 'hermes-list-foreign-sessions))
    (should (commandp (key-binding (kbd "F")))))
  (should (commandp 'hermes-chat-context))
  (should (eq (lookup-key hermes-chat-info-map (kbd "b")) 'hermes-chat-context)))

(ert-deftest hermes-context-empty-is-unavailable ()
  (should (fboundp 'hermes-context--text))
  (let ((text (hermes-context--text
               '((categories . nil) (estimated_total . 0)
                 (context_used . 123) (context_max . 1000)
                 (context_source . "provider_usage") (context_estimated . nil)))))
    (should (string-match-p "Unavailable / not built" text))
    (should (string-match-p "Context files: Unknown" text))
    (should (string-match-p "123" text))
    (should (string-match-p "provider_usage" text))))

(defmacro hermes-foreign-test--fixture (&rest body)
  "Run BODY through real acquisition, ownership, RPC and serialization."
  (declare (indent 0) (debug t))
  `(let* ((before (buffer-list))
          (client (make-hermes-dashboard-transport-client
                   :websocket 'fixture :base-url "http://fixture.invalid"))
          (hermes-instances (copy-tree '(("fixture" . "http://fixture.invalid"))))
          (hermes-dashboard-transport-url "http://fixture.invalid")
          (hermes-dashboard-transport-request-timeout nil)
          (frames nil) (released 0)
          (hermes-dashboard-transport-websocket-send-function
           (lambda (_socket text)
             (push (hermes-dashboard-transport--decode-frame text) frames))))
     (unwind-protect
         (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                   ((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                   ((symbol-function 'hermes-dashboard-transport-release) (lambda (_) (cl-incf released)))
                   ((symbol-function 'hermes-dashboard-transport-api-request-async)
                    (lambda (_method path &rest _args)
                      (should (equal path "/api/profiles"))
                      (hermes--promise-resolved '((profiles . (((name . "work")) ((name . "play")))))))))
           (save-window-excursion ,@body))
       (dolist (buffer (seq-difference (buffer-list) before))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun hermes-foreign-test--reply (client frame result)
  "Deliver RESULT for FRAME through CLIENT's native response correlation."
  (hermes-dashboard-transport--resolve-response
   client `((jsonrpc . "2.0") (id . ,(hermes-transport--get frame 'id)) (result . ,result))))

(defun hermes-foreign-test--open ()
  "Open a foreign browser through the public Sessions key."
  (with-current-buffer (hermes-buffer--get " *foreign source*" #'hermes-sessions-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "work"))
              ((symbol-function 'completing-read) (lambda (&rest _) "all")))
      (call-interactively (key-binding (kbd "F")))))
  (window-buffer (selected-window)))

(defun hermes-foreign-test--preview (client frame)
  "Accept FRAME on CLIENT and open its selected preview."
  (hermes-foreign-test--reply
   client frame '((sessions . (((id . "opaque/do-not-decode") (source . "codex")
                                (title . "Synthetic conversation") (turn_count . 60))))
                 (next_offset . nil) (host . "backend-fixture") (unreadable . 0)))
  (goto-char (point-min))
  (execute-kbd-macro (kbd "RET"))
  (window-buffer (selected-window)))

(ert-deftest hermes-foreign-native-empty-page-preview-import-readback-resume ()
  (hermes-foreign-test--fixture
   (let ((listing (hermes-foreign-test--open)))
     (with-current-buffer listing
       (hermes-foreign-test--reply client (car frames)
                                   '((sessions . nil) (next_offset . 25)
                                     (host . "backend-fixture") (unreadable . 25)))
       (should (string-match-p "Unreadable 25" hermes-browser--status))
       (should (string-match-p "Destination work" hermes-browser--status))
       (execute-kbd-macro (kbd ">"))
       (should (= 25 (hermes-transport--get (hermes-transport--get (car frames) 'params) 'offset)))
       (let ((preview (hermes-foreign-test--preview client (car frames))))
         (with-current-buffer preview
           (should (equal (hermes-transport--get (car frames) 'method) "session.foreign.preview"))
           (should (equal (hermes-transport--get (car frames) 'params)
                          '((id . "opaque/do-not-decode") (profile . "work"))))
           (hermes-foreign-test--reply
            client (car frames)
            '((messages . (((role . "user") (content . "Local Variables:\neval: (error \"NO\")\nEnd:"))))
              (total . 60) (truncated . t) (already_imported . "existing") (cwd . "/remote/label")))
           (should buffer-read-only)
           (should-not buffer-file-name)
           (should (string-match-p "Bounded tail preview" (buffer-string)))
           (should (string-match-p "Already imported: existing" (buffer-string)))
           (should (= 3 (length frames)))
           (let ((readback (hermes--promise-make)) calls resumed
                 (resume (symbol-function 'hermes-chat-resume-session)))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'hermes-dashboard-transport-api-request-async)
                        (lambda (method path &rest args)
                          (push (list method path args) calls)
                          (if (equal path "/api/profiles")
                              (hermes--promise-resolved '((profiles . (((name . "work"))))))
                            readback)))
                       ((symbol-function 'hermes-chat-resume-session)
                        (lambda (&rest args) (setq resumed args) (apply resume args))))
               (execute-kbd-macro (kbd "i"))
               (should (equal (hermes-transport--get (car frames) 'method) "session.foreign.import"))
               (hermes-foreign-test--reply client (car frames)
                                           '((session_id . "stored/result") (already_imported . t)))
               (should (equal (cadar calls) "/api/sessions/stored%2Fresult"))
               (should (equal (plist-get (caddar calls) :query) '((profile . "work"))))
               (should-not hermes-foreign--session)
               (hermes--promise-resolve readback '((id . "stored/result") (profile . "work") (title . "Read back")))
               (execute-kbd-macro (kbd "RET"))
               (should (equal (hermes-transport--get (car frames) 'method) "session.resume"))
               (should (equal (hermes-transport--get (hermes-transport--get (car frames) 'params) 'session_id)
                              "stored/result"))
               (should (equal (hermes-transport--get (hermes-transport--get (car frames) 'params) 'profile) "work"))
               (should (equal resumed '("stored/result" "Read back" "work" ("fixture" . "http://fixture.invalid")
                                       nil "http://fixture.invalid")))
               (with-current-buffer (window-buffer)
                 (should-not hermes-chat--bot-chat-root)))
             (should-not hermes-browser--owned-cleanup))))))))

(ert-deftest hermes-foreign-malformed-page-retains-cursor-and-retries ()
  (hermes-foreign-test--fixture
   (with-current-buffer (hermes-foreign-test--open)
     (hermes-foreign-test--reply client (car frames)
                                 '((sessions . nil) (next_offset . 25) (host . "host") (unreadable . 1)))
     (hermes-foreign-next-page)
     (hermes-foreign-test--reply client (car frames)
                                 '((sessions . nil) (next_offset . 25) (host . "host") (unreadable . 0)))
     (should (string-match-p "Malformed" hermes-browser--status))
     (should (= hermes-foreign--next 25))
     (hermes-foreign-next-page)
     (should (= 25 (hermes-transport--get (hermes-transport--get (car frames) 'params) 'offset))))))

(ert-deftest hermes-foreign-import-confirmation-owner-and-cancel ()
  (dolist (action '(cancel retire quit))
    (hermes-foreign-test--fixture
     (with-current-buffer (hermes-foreign-test--open)
       (with-current-buffer (hermes-foreign-test--preview client (car frames))
         (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0) (truncated . nil)))
         (let ((count (length frames)))
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (&rest _)
                        (pcase action
                          ('cancel nil)
                          ('quit (signal 'quit nil))
                          ('retire (setq hermes-foreign--profile "play") t)))))
             (pcase action
               ('cancel (hermes-foreign-import))
               ('quit (should (condition-case nil
                                  (progn (hermes-foreign-import) nil)
                                (quit t))))
               ('retire (should-error (hermes-foreign-import) :type 'user-error))))
           (should (= count (length frames)))))))))

(ert-deftest hermes-foreign-retired-preview-and-auth-never-dispatch-import ()
  (hermes-foreign-test--fixture
   (with-current-buffer (hermes-foreign-test--open)
     (with-current-buffer (hermes-foreign-test--preview client (car frames))
       (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0) (truncated . nil)))
       (let ((catalogue (hermes--promise-make)) (count (length frames)))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                   ((symbol-function 'hermes-dashboard-transport-api-request-async) (lambda (&rest _) catalogue)))
           (hermes-foreign-import)
           (set-visited-file-name (expand-file-name "foreign-notes" temporary-file-directory) t)
           (set-visited-file-name nil t)
           (let ((inhibit-read-only t)) (erase-buffer) (insert "Retired notes"))
           (hermes--promise-resolve catalogue '((profiles . (((name . "work"))))))
           (should (= count (length frames)))
           (should (equal (buffer-string) "Retired notes"))
           (should-error (hermes-foreign-import) :type 'user-error)))))))

(ert-deftest hermes-foreign-lost-receipt-no-replay ()
  (hermes-foreign-test--fixture
   (with-current-buffer (hermes-foreign-test--open)
     (with-current-buffer (hermes-foreign-test--preview client (car frames))
       (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0) (truncated . nil)))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (hermes-foreign-import))
       (let ((count (length frames)))
         (hermes-dashboard-transport--reject-response
          client `((id . ,(hermes-transport--get (car frames) 'id))
                   (error . ((code . -32000) (message . "Connection lost")))))
         (should (string-match-p "uncertain" hermes-browser--status))
         (should (= count (length frames)))
         (should-error (hermes-foreign-resume) :type 'user-error))))))

(ert-deftest hermes-context-native-on-demand-and-retired-chat ()
  (hermes-foreign-test--fixture
   (hermes-test-with-chat-buffer
    (setq hermes-chat--dashboard-client client
          hermes-chat--dashboard-session-ready-p t
          hermes-chat--dashboard-active-session-id "runtime-original")
    (let ((chat (current-buffer)))
      (hermes-chat-context)
      (with-current-buffer (window-buffer (selected-window))
        (should (equal (hermes-transport--get (car frames) 'method) "session.context_breakdown"))
        (should (equal (hermes-transport--get (car frames) 'params) '((session_id . "runtime-original"))))
        (hermes-foreign-test--reply
         client (car frames)
         '((categories . (((id . "system") (label . "System") (color . "red") (tokens . 51))))
           (estimated_total . 51) (context_used . 97) (context_max . 1000)
           (context_source . "provider_usage") (context_estimated . nil)
           (context_files . (((label . "Instructions") (path . "/ssh:other:/secret")
                             (loaded . nil) (status . "ignored") (chars . 23) (est_tokens . 9))))))
        (should (string-match-p "Used: 97" (buffer-string)))
        (should (string-match-p "Estimated total: 51" (buffer-string)))
        (should (string-match-p "Loaded: no" (buffer-string)))
        (should-not (button-at (point-min)))
        (should (= 1 (length frames)))
        (execute-kbd-macro (kbd "g"))
        (let ((old (buffer-string)))
          (with-current-buffer chat (setq hermes-chat--dashboard-active-session-id "successor"))
          (hermes-foreign-test--reply client (car frames) '((categories . nil) (context_used . 999)))
          (should (equal old (buffer-string)))
          (should (string-match-p "retired" hermes-browser--status))
          (should-error (hermes-context-refresh) :type 'user-error)))))))

(ert-deftest hermes-context-error-is-not-zero-or-pure ()
  (hermes-foreign-test--fixture
   (hermes-test-with-chat-buffer
    (setq hermes-chat--dashboard-client client
          hermes-chat--dashboard-session-ready-p t
          hermes-chat--dashboard-active-session-id "runtime")
    (hermes-chat-context)
    (with-current-buffer (window-buffer (selected-window))
      (hermes-dashboard-transport--reject-response
       client `((id . ,(hermes-transport--get (car frames) 'id))
                (error . ((code . 5000) (message . "Could not compute context breakdown")))))
      (should (string-match-p "prompt hooks may have run" hermes-browser--status))
      (should-not hermes-browser--owned-cleanup)
      (should (= (length frames) 1))))))

(ert-deftest hermes-foreign-authentication-dispatch-fence-with-positive-control ()
  (let ((real-api (symbol-function 'hermes-dashboard-transport-api-request-async)))
    (dolist (retire '(nil t))
      (hermes-foreign-test--fixture
       (with-current-buffer (hermes-foreign-test--open)
         (with-current-buffer (hermes-foreign-test--preview client (car frames))
           (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
           (let ((auth (hermes--promise-make)) (count (length frames)) http)
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'hermes-dashboard-transport-api-request-async) real-api)
                       ((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth))
                       ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                        (lambda (spec)
                          (push spec http)
                          (hermes--promise-resolved
                           '(:status 200 :body ((profiles . (((name . "work"))))))))))
               (hermes-foreign-import)
               (should-not http)
               (when retire (hermes-browser--next-request-generation))
               (hermes--promise-resolve auth '(:session-token "synthetic-fixture"))
               (if retire
                   (progn (should-not http) (should (= count (length frames))))
                 (should (= (length http) 1))
                 (should (equal (hermes-transport--get (car frames) 'method) "session.foreign.import")))))))))))

(ert-deftest hermes-foreign-missing-method-and-disappeared-preview-recovery ()
  (hermes-foreign-test--fixture
   (with-current-buffer (hermes-foreign-test--open)
     (hermes-dashboard-transport--reject-response
      client `((id . ,(hermes-transport--get (car frames) 'id))
               (error . ((code . -32601) (message . "unknown method: session.foreign.list")))))
     (should (string-match-p "compatible backend" hermes-browser--status))
     (should (string-match-p "Destination work" (hermes-foreign--header)))
     (execute-kbd-macro (kbd "g"))
     (with-current-buffer (hermes-foreign-test--preview client (car frames))
       (hermes-dashboard-transport--reject-response
        client `((id . ,(hermes-transport--get (car frames) 'id))
                 (error . ((code . -32602) (message . "Session no longer available. Refresh the list and try again")))))
       (should (string-match-p "no longer available" hermes-browser--status))
       (should-not hermes-browser--owned-cleanup)))))

(ert-deftest hermes-foreign-cold-acquisition-error-retry ()
  (hermes-foreign-test--fixture
   (let ((buffer (hermes-buffer--get " *cold foreign*" #'hermes-foreign-mode)))
     (with-current-buffer buffer
       (hermes-browser--own-instance '("fixture" . "http://fixture.invalid"))
       (setq hermes-foreign--profile "work"
             hermes-foreign--endpoint (hermes-browser--copy-identity hermes-instance))
       (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                  (lambda (&rest _) (error "No dashboard executable"))))
         (should-error (hermes-foreign-refresh)))
       (should (string-match-p "Failed" hermes-browser--status))
       (pop-to-buffer buffer)
       (execute-kbd-macro (kbd "g"))
       (hermes-foreign-test--reply client (car frames)
                                   '((sessions . nil) (next_offset . nil) (host . "host") (unreadable . 0)))
       (should (string-match-p "End" hermes-browser--status))))))

(ert-deftest hermes-context-replaced-claim-and-retired-file-do-not-render ()
  (dolist (retirement '(claim file stop))
    (hermes-foreign-test--fixture
     (hermes-test-with-chat-buffer
      (setq hermes-chat--dashboard-client client
            hermes-chat--dashboard-session-ready-p t
            hermes-chat--dashboard-active-session-id "runtime")
      (let ((chat (current-buffer)))
        (hermes-chat-context)
        (with-current-buffer (window-buffer (selected-window))
          (pcase retirement
            ('claim (with-current-buffer chat
                      (setq hermes-buffer--owner (copy-tree hermes-buffer--owner))))
            ('file (set-visited-file-name (expand-file-name "context-notes" temporary-file-directory) t)
                   (set-visited-file-name nil t))
            ('stop (setf (hermes-dashboard-transport-client-websocket client) nil)
                   (hermes-dashboard-transport-stop client)))
          (let ((inhibit-read-only t)) (erase-buffer) (insert "Keep this text"))
          (hermes-foreign-test--reply client (car frames) '((categories . nil) (context_used . 999)))
          (should (equal (buffer-string) "Keep this text"))
          (should-error (hermes-context-refresh) :type 'user-error)))))))

(ert-deftest hermes-foreign-import-rejects-unverified-identity ()
  (dolist (failure '(missing-profile missing-receipt wrong-session wrong-profile))
    (hermes-foreign-test--fixture
     (with-current-buffer (hermes-foreign-test--open)
       (with-current-buffer (hermes-foreign-test--preview client (car frames))
         (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
         (let ((count (length frames)) reads)
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'hermes-dashboard-transport-api-request-async)
                      (lambda (_method path &rest args)
                        (should (eq client (plist-get args :client)))
                        (push path reads)
                        (hermes--promise-resolved
                         (if (equal path "/api/profiles")
                             (unless (eq failure 'missing-profile)
                               '((profiles . (((name . "work"))))))
                           `((id . ,(if (eq failure 'wrong-session) "different" "returned"))
                             (profile . ,(if (eq failure 'wrong-profile) "play" "work"))))))))
             (execute-kbd-macro (kbd "i"))
             (if (eq failure 'missing-profile)
                 (should (= count (length frames)))
               (hermes-foreign-test--reply
                client (car frames)
                (unless (eq failure 'missing-receipt) '((session_id . "returned")))))
             (should (string-match-p "uncertain / readback failed" hermes-browser--status))
             (should-not hermes-foreign--session)
             (should-error (hermes-foreign-resume) :type 'user-error)
             (should (= (length reads) (if (memq failure '(wrong-profile wrong-session)) 2 1))))))))))

(ert-deftest hermes-context-partial-file-metadata-remains-unknown ()
  (let ((text (hermes-context--text
               '((categories . (((label . "System") (tokens . 6))))
                 (estimated_total . 6) (context_files . (((path . "/backend/AGENTS.md"))))))))
    (should (string-match-p "manifest completeness unknown" text))
    (should (string-match-p "Loaded: Unknown · Status: Unknown · Chars: Unknown" text))
    (should (string-match-p "Used: Unknown / Maximum: Unknown" text))
    (should (string-match-p "Source: Unknown · Estimated: Unknown" text))))

(defmacro hermes-foreign-test--verified (&rest body)
  "Run BODY in a preview imported through its public key and detail readback."
  (declare (indent 0) (debug t))
  `(hermes-foreign-test--fixture
    (with-current-buffer (hermes-foreign-test--open)
      (with-current-buffer (hermes-foreign-test--preview client (car frames))
        (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
        (let ((readback (hermes--promise-make)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'hermes-dashboard-transport-api-request-async)
                     (lambda (_method path &rest args)
                       (should (eq client (plist-get args :client)))
                       (if (equal path "/api/profiles")
                           (hermes--promise-resolved '((profiles . (((name . "work"))))))
                         (should (equal path "/api/sessions/stored%2Fresult"))
                         (should (equal (plist-get args :query) '((profile . "work"))))
                         readback))))
            (execute-kbd-macro (kbd "i"))
            (hermes-foreign-test--reply client (car frames) '((session_id . "stored/result")))
            (hermes--promise-resolve
             readback '((id . "stored/result") (profile . "work") (title . "Verified")))))
        ,@body))))

(defun hermes-foreign-test--resume-after (action)
  "Prove verified native resume after the public preview ACTION."
  (hermes-foreign-test--verified
   (let ((preview (current-buffer)) slow)
     (pcase action
       ((or 'refresh 'slow-refresh 'overlapping-refresh)
        (execute-kbd-macro (kbd "g"))
        (setq slow (car frames))
        (when (eq action 'overlapping-refresh)
          (execute-kbd-macro (kbd "g")))
        (unless (eq action 'slow-refresh)
          (hermes-foreign-test--reply
           client (car frames) '((messages . nil) (total . 0) (already_imported . "stored/result")))
          (should (string-match-p "RET resume" hermes-browser--status))))
       ((or 'decline 'quit)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (if (eq action 'quit) (signal 'quit nil) nil))))
          (condition-case nil (execute-kbd-macro (kbd "i")) (quit nil)))
        (should (string-match-p "RET resume" hermes-browser--status))))
     (execute-kbd-macro (kbd "RET"))
     (should (equal (hermes-transport--get (car frames) 'method) "session.resume"))
     (should (equal (hermes-transport--get (hermes-transport--get (car frames) 'params) 'session_id)
                    "stored/result"))
     (should (equal (hermes-transport--get (hermes-transport--get (car frames) 'params) 'profile) "work"))
     (when slow
       (hermes-foreign-test--reply client slow '((messages . nil) (total . 0)))
       (with-current-buffer preview
         (should (string-match-p "RET resume" hermes-browser--status))))
     (should (= 1 (seq-count (lambda (frame)
                               (equal (hermes-transport--get frame 'method) "session.foreign.import"))
                             frames))))))

(ert-deftest hermes-foreign-verified-resume-immediate ()
  (hermes-foreign-test--resume-after 'immediate))

(ert-deftest hermes-foreign-verified-resume-refresh ()
  (hermes-foreign-test--resume-after 'refresh))

(ert-deftest hermes-foreign-verified-resume-decline ()
  (hermes-foreign-test--resume-after 'decline))

(ert-deftest hermes-foreign-verified-resume-quit ()
  (hermes-foreign-test--resume-after 'quit))

(ert-deftest hermes-foreign-verified-resume-slow-and-overlapping-reads ()
  (hermes-foreign-test--resume-after 'slow-refresh)
  (hermes-foreign-test--resume-after 'overlapping-refresh))

(ert-deftest hermes-foreign-verified-resume-rejects-retired-authority ()
  (dolist (change '(claim file instance endpoint profile handle session mode))
    (hermes-foreign-test--verified
     (let ((count (length frames)))
       (pcase change
         ('claim (setq hermes-buffer--owner (copy-tree hermes-buffer--owner)))
         ('file (set-visited-file-name (expand-file-name "retired-preview" temporary-file-directory) t)
                (set-visited-file-name nil t))
         ('instance (setq hermes-instance (copy-tree hermes-instance)))
         ('endpoint (setcdr hermes-instance "http://different.invalid"))
         ('profile (setq hermes-foreign--profile "play"))
         ('handle (setq hermes-foreign--row (copy-tree hermes-foreign--row))
                  (setcdr (assq 'id hermes-foreign--row) "another-handle"))
         ('session (setq hermes-foreign--session (copy-tree hermes-foreign--session))
                   (setcdr (assq 'id hermes-foreign--session) "another-session"))
         ('mode (fundamental-mode) (hermes-foreign-preview-mode)))
       (should-error (call-interactively #'hermes-foreign-resume) :type 'user-error)
       (should (= count (length frames)))))))

(ert-deftest hermes-foreign-superseded-import-readback-cannot-verify ()
  (hermes-foreign-test--fixture
   (with-current-buffer (hermes-foreign-test--open)
     (with-current-buffer (hermes-foreign-test--preview client (car frames))
       (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
       (let ((readback (hermes--promise-make)))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                   ((symbol-function 'hermes-dashboard-transport-api-request-async)
                    (lambda (_method path &rest _args)
                      (if (equal path "/api/profiles")
                          (hermes--promise-resolved '((profiles . (((name . "work"))))))
                        readback))))
           (execute-kbd-macro (kbd "i"))
           (hermes-foreign-test--reply client (car frames) '((session_id . "stored/result")))
           (execute-kbd-macro (kbd "g"))
           (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
           (hermes--promise-resolve readback '((id . "stored/result") (profile . "work")))
           (should-not hermes-foreign--session)
           (should-error (hermes-foreign-resume) :type 'user-error)))))))

(ert-deftest hermes-context-public-header-retains-runtime-and-caveat ()
  (hermes-foreign-test--fixture
   (let (views)
     (dolist (runtime '("runtime-one" "runtime-two"))
       (let ((hermes-chat-buffer-name (hermes-test--chat-buffer-name)))
         (with-current-buffer (hermes-chat)
           (setq hermes-chat--dashboard-client client
		 hermes-chat--dashboard-session-ready-p t
		 hermes-chat--dashboard-active-session-id runtime)
           (hermes-chat-context)
           (let ((view (window-buffer (selected-window))))
             (push view views)
             (with-current-buffer view
               (dolist (phase '(pending settled))
		 (when (eq phase 'settled)
                   (if (equal runtime "runtime-one")
                       (hermes-foreign-test--reply client (car frames) '((categories . nil)))
                     (hermes-dashboard-transport--reject-response
                      client `((id . ,(hermes-transport--get (car frames) 'id))
                               (error . ((code . 5000) (message . "Compute failed")))))))
		 (let ((header (if (eq (car-safe header-line-format) :eval)
                                   (eval (cadr header-line-format) t)
				 header-line-format)))
                   (should (stringp header))
                   (should (string-match-p (regexp-quote runtime) header))
                   (unless noninteractive
                     (should (string-match-p
                              (regexp-quote runtime)
                              (format-mode-line header-line-format))))
                   (should (string-match-p "may invoke backend memory hooks" header))
                   (should (eq (get-text-property (string-match (regexp-quote runtime) header) 'face header)
                               'font-lock-constant-face)))))))))
     (should (= 2 (length (delete-dups views)))))))

(defun hermes-foreign-test--retarget-consent (named phase)
  "Exercise public import at NAMED or legacy backend with retarget PHASE."
  (hermes-foreign-test--fixture
   (let ((hermes-instances (and named hermes-instances))
         (other (make-hermes-dashboard-transport-client
                 :websocket 'other :base-url "http://other.invalid"))
         acquired sockets prompt)
     (let ((hermes-dashboard-transport-websocket-send-function
            (lambda (socket text)
              (push socket sockets)
              (push (hermes-dashboard-transport--decode-frame text) frames))))
       (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                  (lambda (&rest _)
                    (push hermes-dashboard-transport-url acquired)
                    (if (equal hermes-dashboard-transport-url "http://other.invalid")
                        other client))))
         (with-current-buffer (hermes-foreign-test--open)
           (with-current-buffer (hermes-foreign-test--preview client (car frames))
             (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
             (let ((count (length frames)) refused)
               (when (eq phase 'before)
                 (setq hermes-dashboard-transport-url "http://other.invalid"))
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (text)
                            (setq prompt text)
                            (when (eq phase 'consent)
                              (setq hermes-dashboard-transport-url "http://other.invalid"))
                            t)))
                 (condition-case nil
                     (call-interactively (key-binding (kbd "i")))
                   (user-error (setq refused t))))
               (if (and phase (not named))
                   (progn
                     (should refused)
                     (should (= count (length frames)))
                     (when (eq phase 'before) (should-not prompt)))
                 (should-not refused)
                 (should (equal "session.foreign.import"
                                (hermes-transport--get (car frames) 'method))))
               (when prompt (should (string-match-p "http://fixture.invalid" prompt)))
               (should-not (member "http://other.invalid" acquired))
               (should-not (memq 'other sockets))))))))))

(ert-deftest hermes-foreign-legacy-retarget-before-import ()
  (hermes-foreign-test--retarget-consent nil 'before))

(ert-deftest hermes-foreign-legacy-retarget-during-consent ()
  (hermes-foreign-test--retarget-consent nil 'consent))

(ert-deftest hermes-foreign-retarget-explicit-and-unchanged-controls ()
  (hermes-foreign-test--retarget-consent nil nil)
  (dolist (phase '(nil before consent))
    (hermes-foreign-test--retarget-consent t phase)))

(ert-deftest hermes-foreign-retarget-during-authentication ()
  (let ((real-api (symbol-function 'hermes-dashboard-transport-api-request-async)))
    (dolist (named '(nil t))
      (hermes-foreign-test--fixture
       (let ((hermes-instances (and named hermes-instances)))
         (with-current-buffer (hermes-foreign-test--open)
           (with-current-buffer (hermes-foreign-test--preview client (car frames))
             (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
             (let ((auth (hermes--promise-make)) (count (length frames)) http)
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                         ((symbol-function 'hermes-dashboard-transport-api-request-async) real-api)
                         ((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth))
                         ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                          (lambda (spec)
                            (push spec http)
                            (hermes--promise-resolved
                             '(:status 200 :body ((profiles . (((name . "work"))))))))))
                 (call-interactively (key-binding (kbd "i")))
                 (should-not http)
                 (setq hermes-dashboard-transport-url "http://other.invalid")
                 (hermes--promise-resolve auth '(:session-token "synthetic"))
                 (if named
                     (progn
                       (should (= 1 (length http)))
                       (should (equal "session.foreign.import"
                                      (hermes-transport--get (car frames) 'method))))
                   (should-not http)
                   (should (= count (length frames)))
                   (should-not hermes-foreign--session))
                 (should-not (and (not named) hermes-browser--owned-cleanup)))))))))))

(ert-deftest hermes-foreign-retarget-readback-never-adopts-old-session ()
  (hermes-foreign-test--fixture
   (let ((hermes-instances nil))
     (with-current-buffer (hermes-foreign-test--open)
       (with-current-buffer (hermes-foreign-test--preview client (car frames))
         (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
         (let ((readback (hermes--promise-make)))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'hermes-dashboard-transport-api-request-async)
                      (lambda (_method path &rest args)
                        (should (eq client (plist-get args :client)))
                        (if (equal path "/api/profiles")
                            (hermes--promise-resolved '((profiles . (((name . "work"))))))
                          readback))))
             (call-interactively (key-binding (kbd "i")))
             (hermes-foreign-test--reply client (car frames) '((session_id . "stored/result")))
             (setq hermes-dashboard-transport-url "http://other.invalid")
             (hermes--promise-resolve readback '((id . "stored/result") (profile . "work")))
             (should-not hermes-foreign--session)
             (should-not hermes-browser--owned-cleanup)
             (should-error (hermes-foreign-resume) :type 'user-error))))))))

(defun hermes-foreign-test--resume-route (named boundary)
  "Prove NAMED or legacy resume retains its backend across BOUNDARY."
  (hermes-foreign-test--fixture
   (let ((hermes-instances (and named hermes-instances)))
     (with-current-buffer (hermes-foreign-test--open)
       (with-current-buffer (hermes-foreign-test--preview client (car frames))
         (hermes-foreign-test--reply client (car frames) '((messages . nil) (total . 0)))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                   ((symbol-function 'hermes-dashboard-transport-api-request-async)
                    (lambda (_method path &rest _)
                      (hermes--promise-resolved
                       (if (equal path "/api/profiles")
                           '((profiles . (((name . "work")))))
                         '((id . "verified-A") (profile . "work") (title . "Synthetic")))))))
           (execute-kbd-macro (kbd "i"))
           (hermes-foreign-test--reply client (car frames) '((session_id . "verified-A"))))
         (should (hermes-foreign--verified-p))
         (let* ((other (make-hermes-dashboard-transport-client
                        :websocket 'other :base-url "http://other.invalid"))
                (ready (hermes--promise-make))
                (retarget (lambda (&rest _)
                            (setq hermes-dashboard-transport-url "http://other.invalid")))
                (hermes-chat-mode-hook
                 (and (eq boundary 'mode)
                      (list (lambda ()
                              (funcall retarget)
                              (should-not hermes-chat--bot-chat-root)
                              (should (equal hermes-chat--profile "work"))
                              (should (equal hermes-chat--session-id "verified-A"))
                              (hermes-chat--dashboard-ensure-client)))))
                (buffer-list-update-hook (and (eq boundary 'display) (list retarget)))
                routed acquired
                (hermes-dashboard-transport-websocket-send-function
                 (lambda (socket text)
                   (push (cons socket (hermes-dashboard-transport--decode-frame text)) routed))))
           (when (eq boundary 'ready)
             (setf (hermes-dashboard-transport-client-ready-promise client) ready
                   (hermes-dashboard-transport-client-ready-p client) nil))
           (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                      (lambda (&rest _)
                        (push hermes-dashboard-transport-url acquired)
                        (if (equal hermes-dashboard-transport-url "http://other.invalid") other
                          (setf (hermes-dashboard-transport-client-websocket client) 'fixture)
                          client))))
             (execute-kbd-macro (kbd "RET"))
             (with-current-buffer (window-buffer (selected-window))
               (when (eq boundary 'ready)
                 (should-not routed)
                 (funcall retarget)
                 (hermes--promise-resolve ready t))
               (should (eq hermes-chat--dashboard-client client))
               (should (equal acquired '("http://fixture.invalid")))
               (should (equal (caar routed) 'fixture))
               (should (equal (hermes-transport--get (cdar routed) 'method) "session.resume"))
               (should (equal (hermes-transport--get (hermes-transport--get (cdar routed) 'params) 'session_id)
                              "verified-A"))
               ;; A failed history read followed by cold acquisition must use A,
               ;; even though the global default remains B.
               (hermes-dashboard-transport--reject-response
                client `((id . ,(hermes-transport--get (cdar routed) 'id))
                         (error . ((code . -32000) (message . "Synthetic failure")))))
               (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed))
               (setf (hermes-dashboard-transport-client-websocket client) nil)
               (funcall retarget)
               (hermes-chat--load-session-history (current-buffer))
               (setf (hermes-dashboard-transport-client-websocket client) 'fixture)
               (should (equal acquired '("http://fixture.invalid" "http://fixture.invalid")))
               (should-not (assq 'other routed))))))))))

(ert-deftest hermes-foreign-resume-retarget-mode-hook ()
  (hermes-foreign-test--resume-route nil 'mode))

(ert-deftest hermes-foreign-resume-retarget-display-hook ()
  (hermes-foreign-test--resume-route nil 'display))

(ert-deftest hermes-foreign-resume-retarget-deferred-readiness ()
  (hermes-foreign-test--resume-route nil 'ready))

(ert-deftest hermes-foreign-resume-retarget-unchanged-and-named ()
  (hermes-foreign-test--resume-route nil nil)
  (hermes-foreign-test--resume-route t 'mode))

(ert-deftest hermes-foreign-resume-acquisition-ordinary-chat-controls ()
  "Keep ordinary unpinned chats on their existing instance resolution policy."
  (hermes-foreign-test--fixture
   (let ((hermes-instances nil) acquired)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                (lambda (&rest _)
                  (push hermes-dashboard-transport-url acquired)
                  client)))
       (with-current-buffer (hermes-chat--new-buffer)
         (setq hermes-dashboard-transport-url "http://other.invalid")
         (should (eq client (hermes-chat--dashboard-ensure-client)))
         (should (eq client (hermes-chat--dashboard-ensure-client)))
         (should-not hermes-chat--pinned-url)
         (should (equal acquired '("http://other.invalid"))))
       (with-temp-buffer
         (hermes-chat-mode)
         (should-not hermes-instance)
         (hermes-chat--dashboard-ensure-client)
         (should (equal acquired '("http://other.invalid" "http://other.invalid"))))))))

(ert-deftest hermes-foreign-resume-refuses-wrong-live-client ()
  "A mode hook's unrelated live client cannot override the captured backend."
  (hermes-foreign-test--verified
   (let* ((other (make-hermes-dashboard-transport-client
                  :websocket 'other :base-url "http://other.invalid"))
          (hermes-chat-mode-hook
           (list (lambda ()
                   (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                              (lambda (&rest _) other)))
                     (hermes-chat--dashboard-ensure-client)))))
          (count (length frames)))
     (should (equal (should-error (call-interactively (key-binding (kbd "RET")))
                                 :type 'user-error)
                    '(user-error "Chat backend changed; reconnect to its original backend")))
     (should (= count (length frames))))))

(ert-deftest hermes-foreign-resume-reuses-effective-spawn-endpoint ()
  "A pinned chat reuses its spawn client, whose raw base URL is nil."
  (hermes-foreign-test--fixture
   (setf (hermes-dashboard-transport-client-base-url client) nil
         (hermes-dashboard-transport-client-host client) "127.0.0.1"
         (hermes-dashboard-transport-client-port client) 8765
         (hermes-dashboard-transport-client-endpoint-key client)
         '(spawn "127.0.0.1" 8765))
   (with-current-buffer (hermes-chat--new-buffer)
     (setq hermes-chat--pinned-url "http://127.0.0.1:8765/"
           hermes-chat--resolved-start-mode nil
           hermes-chat--dashboard-client client)
     (should (eq client (hermes-chat--dashboard-ensure-client)))
     (should (eq client (hermes-chat--dashboard-start #'ignore)))
     (should (eq hermes-chat--resolved-start-mode 'spawn)))))

(ert-deftest hermes-foreign-resume-refuses-wrong-spawn-endpoint ()
  "A nil raw base URL never exempts an unrelated spawned backend."
  (hermes-foreign-test--verified
   (let* ((other (make-hermes-dashboard-transport-client
                  :websocket 'other :host "127.0.0.1" :port 8766
                  :endpoint-key '(spawn "127.0.0.1" 8766)))
          (hermes-chat-mode-hook
           (list (lambda () (setq hermes-chat--dashboard-client other))))
          (count (length frames)))
     (should-error (call-interactively (key-binding (kbd "RET"))) :type 'user-error)
     (should (= count (length frames))))))

(provide 'hermes-foreign-context-tests)
;;; hermes-foreign-context-tests.el ends here
