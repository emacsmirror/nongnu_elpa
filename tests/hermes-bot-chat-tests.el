;;; hermes-bot-chat-tests.el --- Canonical profile conversation tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'hermes-test-helpers)
(require 'hermes-profiles)
(require 'hermes-cron)

(ert-deftest hermes-bot-chat-native-profile-entry ()
  "Profiles expose the canonical conversation separately from model settings."
  (with-temp-buffer
    (hermes-profiles-mode)
    (should (eq (key-binding (kbd "RET")) #'hermes-profiles-open-bot-chat))
    (should (eq (key-binding (kbd "m")) #'hermes-profiles-set-model))
    (should (eq (key-binding (kbd "R")) #'hermes-profiles-routines))))

(defvar hermes-bot-test--wire nil)
(defvar hermes-bot-test--client nil)

(defmacro hermes-bot-test--with-profile (profile &rest body)
  "Run BODY in a real Profiles view selecting PROFILE with a synthetic wire."
  (declare (indent 1))
  `(let* ((hermes-instances '(("fixture" . "http://fixture.invalid")))
          (hermes-dashboard-transport-start-mode 'remote)
          (hermes-dashboard-transport-request-timeout nil)
          (hermes-cron-auto-refresh-interval 0)
          (hermes-bot-test--wire nil)
          (before (buffer-list))
          (hermes-bot-test--client
           (make-hermes-dashboard-transport-client
            :base-url "http://fixture.invalid" :token "synthetic" :ready-p t :websocket 'fixture)))
     (cl-letf (((symbol-function 'hermes-browser--existing-client)
                (lambda () hermes-bot-test--client))
               ((symbol-function 'hermes-dashboard-transport-acquire)
                (lambda (&rest _) hermes-bot-test--client))
               ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
               ((symbol-value 'hermes-dashboard-transport-websocket-send-function)
                (lambda (_socket text)
                  (push (hermes-transport-json-parse text) hermes-bot-test--wire))))
       (unwind-protect
           (with-temp-buffer
             (hermes-profiles-mode)
             (hermes-buffer--claim 'hermes-profiles-mode)
             (hermes-browser--own-instance (car hermes-instances))
             (hermes-profiles--render `((profiles . (((name . ,,profile))))))
             (goto-char (point-min))
             ,@body)
         (dolist (buffer (seq-difference (buffer-list) before))
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun hermes-bot-test--reply (method result &optional code)
  "Reply to latest serialized METHOD with RESULT or error CODE."
  (let ((frame (car hermes-bot-test--wire)))
    (should (equal method (hermes-transport--get frame 'method)))
    (hermes-dashboard-transport--handle-frame
     hermes-bot-test--client
     (json-serialize
      (append `((jsonrpc . "2.0") (id . ,(hermes-transport--get frame 'id)))
              (if code `((error . ((code . ,code) (message . ,result))))
                `((result . ,result))))))))

(defun hermes-bot-test--registry (profile &optional root tip)
  "Return registry fixture for PROFILE with optional ROOT and TIP."
  `((bot_mode_protocol . t)
    (profiles . ,(vector `((name . ,profile)
                          (canonical_session . ,(if root `((id . ,root)
                                                           (resolved_id . ,(or tip root))) :null)))))))

(defun hermes-bot-test--rows (root &optional tip)
  "Return exact-title lookup fixture with optional ROOT and TIP."
  `((sessions . ,(if root (vector `((id . ,root) (resolved_id . ,(or tip root))
                                    (title . "Bot Chat"))) []))))

(defun hermes-bot-test--methods ()
  "Return chronological serialized method names."
  (mapcar (lambda (frame) (hermes-transport--get frame 'method))
          (reverse hermes-bot-test--wire)))

(ert-deftest hermes-bot-chat-failed-and-contradictory-lookup ()
  "Failed lookup, missing results and contradictory positives never mint."
  (dolist (outcome '(error contradictory malformed multiple null false))
    (hermes-bot-test--with-profile "alpha"
      (call-interactively #'hermes-profiles-open-bot-chat)
      (hermes-bot-test--reply
       "profiles.list" (hermes-bot-test--registry
                        "alpha" (and (eq outcome 'contradictory) "root")))
      (if (eq outcome 'error)
          (hermes-bot-test--reply "session.list" "storage unavailable" 5006)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (hermes-bot-test--reply
           "session.list" (pcase outcome
                          ('malformed '((other . [])))
                          ('null '((sessions . :null)))
                          ('false '((sessions . :false)))
                          ('multiple '((sessions . [((id . "one")) ((id . "two"))])))
                            (_ (hermes-bot-test--rows nil))))))
      (should (equal (hermes-bot-test--methods) '("profiles.list" "session.list")))
      (should (equal hermes-browser--status "Failed; g retry")))))

(ert-deftest hermes-bot-chat-create-persist-readback-and-next-send ()
  "Two profiles adopt only after title/readback, hydrate, then send to the winner."
  (dolist (profile '("alpha" "beta"))
    (hermes-bot-test--with-profile profile
      (let ((origin (current-buffer)) (count 0))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (cl-incf count) t)))
          (call-interactively #'hermes-profiles-open-bot-chat)
          (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry profile))
          (hermes-bot-test--reply "session.list" (hermes-bot-test--rows nil)))
        (should (= count 1))
        (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'params)
                       `((profile . ,profile) (hidden . t) (follow_profile_config . t)
                         (source . "emacs"))))
        (hermes-bot-test--reply
         "session.create" `((session_id . "created") (stored_session_id . "root")
                            (info . ((profile_name . ,profile)))))
        (should-not (member "session.resume" (hermes-bot-test--methods)))
        (hermes-bot-test--reply "session.title" '((pending . :false) (title . "Bot Chat")))
        (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root" "tip"))
        (should-not (member "session.resume" (hermes-bot-test--methods)))
        (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry profile "root" "tip"))
        (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method) "session.resume"))
        (should (equal (hermes-transport--get
                        (hermes-transport--get (car hermes-bot-test--wire) 'params) 'session_id) "tip"))
        (let ((chat (window-buffer)))
          (should-not (eq origin chat))
          (should (equal (buffer-local-value 'hermes-chat--bot-chat-root chat) "root"))
          (hermes-bot-test--reply
           "session.resume" '((session_id . "winner") (stored_session_id . "tip")
                              (running . :false)
                              (messages . [((role . "user") (text . "Earlier question"))
                                           ((role . "assistant") (text . "Retained answer"))])))
          (should-not (member "prompt.submit" (hermes-bot-test--methods)))
          (with-current-buffer chat
            (should (string-match-p "Retained answer" (buffer-string)))
            (should (equal hermes-chat--profile profile))
            (should (equal hermes-chat--session-id "tip"))
            (goto-char (point-max))
            (insert "Next deliberate message")
            (call-interactively #'hermes-chat-send))
          (let* ((frame (car hermes-bot-test--wire))
                 (params (hermes-transport--get frame 'params)))
            (should (equal (hermes-transport--get frame 'method) "prompt.submit"))
            (should (equal (hermes-transport--get params 'session_id) "winner"))
            (should (equal (hermes-transport--get params 'text) "Next deliberate message"))))))))

(ert-deftest hermes-bot-chat-title-pending-failure-and-conflict ()
  "Only persisted titles or a freshly read conflict winner permit adoption."
  (dolist (outcome '(pending malformed null failure conflict))
    (hermes-bot-test--with-profile "alpha"
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (hermes-profiles-open-bot-chat)
        (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha"))
        (hermes-bot-test--reply "session.list" (hermes-bot-test--rows nil)))
      (hermes-bot-test--reply "session.create"
                            '((session_id . "loser") (info . ((profile_name . "alpha")))))
      (pcase outcome
        ('pending (hermes-bot-test--reply "session.title" '((pending . t) (title . "Bot Chat"))))
        ('malformed (hermes-bot-test--reply "session.title" '((title . "Bot Chat"))))
        ('null (hermes-bot-test--reply "session.title" '((pending . :null) (title . "Bot Chat"))))
        ('failure (hermes-bot-test--reply "session.title" "Uncertain" 5000))
        ('conflict
         (hermes-bot-test--reply "session.title" "opaque conflict diagnostic" 4022)
         (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "winner"))
         (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "winner"))))
      (should (= 1 (seq-count (lambda (m) (equal m "session.create")) (hermes-bot-test--methods))))
      (should (eq (not (null (member "session.resume" (hermes-bot-test--methods))))
                  (eq outcome 'conflict)))
      (unless (eq outcome 'conflict)
        (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method)
                       "session.title")))
      (should-not (member "prompt.submit" (hermes-bot-test--methods))))))

(ert-deftest hermes-bot-chat-delayed-retirement ()
  "Retirement at each boundary prevents further mutation and adoption."
  (dolist (boundary '(registry lookup consent create title readback))
    (hermes-bot-test--with-profile "alpha"
      (let ((origin (current-buffer)) late)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (when (eq boundary 'consent) (hermes-browser--next-request-generation)) t)))
          (hermes-profiles-open-bot-chat)
          (unless (eq boundary 'registry)
            (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha")))
          (unless (memq boundary '(registry lookup))
            (hermes-bot-test--reply "session.list" (hermes-bot-test--rows nil)))
          (unless (memq boundary '(registry lookup consent create))
            (hermes-bot-test--reply "session.create"
                                  '((session_id . "new") (info . ((profile_name . "alpha"))))))
          (when (eq boundary 'readback)
            (hermes-bot-test--reply "session.title" '((pending . :false) (title . "Bot Chat")))))
        (setq late (car hermes-bot-test--wire))
        (let ((before (length hermes-bot-test--wire)))
          (with-current-buffer origin (hermes-browser--next-request-generation))
          (hermes-dashboard-transport--handle-frame
           hermes-bot-test--client
           (json-serialize `((id . ,(hermes-transport--get late 'id))
                             (result . ((sessions . []))))))
          (should (= before (length hermes-bot-test--wire))))
        (should-not (member "session.resume" (hermes-bot-test--methods)))
        (should-not (member "prompt.submit" (hermes-bot-test--methods)))))))

(ert-deftest hermes-bot-chat-new-offers-compression-or-explicit-scratch ()
  "Canonical /new keeps its relationship; scratch and ordinary chats stay separate."
  (dolist (choice '("Compress conversation" "Open scratch chat"))
    (hermes-test-with-chat-buffer
      (setq hermes-chat--bot-chat-root "root" hermes-chat--profile "alpha"
            hermes-instance '("fixture" . "http://fixture.invalid"))
      (let (compressed scratch)
        (cl-letf (((symbol-value 'completing-read-function) (lambda (&rest _) choice))
                  ((symbol-function 'hermes-chat--dashboard-compress)
                   (lambda (&rest args) (setq compressed args)))
                  ((symbol-function 'hermes-chat--new-buffer)
                   (lambda (&rest args) (setq scratch args))))
          (hermes-chat--handle-slash-content "/new"))
        (should (equal hermes-chat--bot-chat-root "root"))
        (if (equal choice "Compress conversation")
            (progn (should compressed) (should-not scratch))
          (should-not compressed)
          (should (equal scratch '("alpha" "" ("fixture" . "http://fixture.invalid")
                                  "http://fixture.invalid"))))))))

(ert-deftest hermes-bot-chat-routines-are-read-only-scoped-cron ()
  "Routines use the existing cron browser with exact profile/backend GETs only."
  (dolist (profile '("alpha" "beta"))
    (hermes-bot-test--with-profile profile
      (let (requests target)
        (cl-letf (((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                   (lambda (request &rest _)
                     (push request requests)
                     (hermes--promise-resolved '(:body nil)))))
          (call-interactively #'hermes-profiles-routines)
          (setq target (window-buffer))
          (with-current-buffer target
            (should (derived-mode-p 'hermes-cron-mode))
            (should (equal hermes-cron--scope-profile profile))
            (should (equal hermes-instance (car hermes-instances)))
            (call-interactively #'revert-buffer)
            (should-error (hermes-cron-create "wrong" "0 * * * *" "test" "other") :type 'user-error)))
        (should (= 2 (length requests)))
        (dolist (request requests)
          (should (equal (plist-get request :method) "GET"))
          (should (equal (plist-get request :url)
                         (concat "http://fixture.invalid/api/cron/jobs?profile=" profile))))))))

(ert-deftest hermes-bot-chat-native-ret-dispatch ()
  "The actual Profiles RET key dispatches canonical registry lookup."
  (hermes-bot-test--with-profile "alpha"
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "RET")))
    (should (equal (hermes-bot-test--methods) '("profiles.list")))))

(ert-deftest hermes-bot-chat-two-clients-race-adopts-one-winner ()
  "Independent clients with confirmed absence converge after title conflict."
  (hermes-bot-test--with-profile "alpha"
    (let* ((one hermes-bot-test--client)
           (first (current-buffer))
           (two (make-hermes-dashboard-transport-client
                 :base-url "http://fixture.invalid" :token "synthetic"
                 :ready-p t :websocket 'second))
           (second (generate-new-buffer " *second profiles*"))
           first-wire second-wire)
      (dolist (pair (list (cons first one) (cons second two)))
        (let ((hermes-bot-test--client (cdr pair)) (hermes-bot-test--wire nil))
          (with-current-buffer (car pair)
            (unless (eq (car pair) first)
              (hermes-profiles-mode)
              (hermes-buffer--claim 'hermes-profiles-mode)
              (hermes-browser--own-instance (car hermes-instances))
              (hermes-profiles--render '((profiles . (((name . "alpha"))))))
              (goto-char (point-min)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (hermes-profiles-open-bot-chat)
              (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha"))
              (hermes-bot-test--reply "session.list" (hermes-bot-test--rows nil)))
            (hermes-bot-test--reply
             "session.create"
             `((session_id . ,(if (eq one (cdr pair)) "one" "two"))
               (info . ((profile_name . "alpha")))))
            (if (eq one (cdr pair)) (setq first-wire hermes-bot-test--wire)
              (setq second-wire hermes-bot-test--wire)))))
      (dolist (pair (list (cons one first-wire) (cons two second-wire)))
        (let ((hermes-bot-test--client (car pair)) (hermes-bot-test--wire (cdr pair)))
          (if (eq (car pair) one)
              (hermes-bot-test--reply "session.title" '((pending . :false) (title . "Bot Chat")))
            (hermes-bot-test--reply "session.title" "conflict" 4022))
          (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "winner" "tip"))
          (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "winner" "tip"))
          (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method) "session.resume"))
          (should (equal (hermes-transport--get
                          (hermes-transport--get (car hermes-bot-test--wire) 'params) 'session_id) "tip"))
          (should-not (member "prompt.submit" (hermes-bot-test--methods)))
          (should (= 1 (seq-count (lambda (m) (equal m "session.create"))
                                 (hermes-bot-test--methods)))))))))

(ert-deftest hermes-bot-chat-existing-compressed-and-reopen ()
  "Reopening always looks up the backend root/tip, never a cached local pointer."
  (hermes-bot-test--with-profile "alpha"
    (dotimes (round 2)
      (let ((tip (format "tip-%d" round)))
        (hermes-profiles-open-bot-chat)
        (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "root" tip))
        (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root" tip))
        (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root" tip))
        (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "root" tip))
        (should (equal (hermes-transport--get
                        (hermes-transport--get (car hermes-bot-test--wire) 'params) 'session_id) tip))))
    (should-not (member "session.create" (hermes-bot-test--methods)))
    (should-not (member "prompt.submit" (hermes-bot-test--methods)))))

(defvar hermes-bot-test--requests nil)
(defvar hermes-bot-test--destinations nil)

(defmacro hermes-bot-test--with-backends (named &rest body)
  "Run BODY with NAMED or legacy instances and native acquisition of A and B."
  (declare (indent 1))
  `(let* ((hermes-instances
           (and ,named (list (cons "A" "http://authority-a.invalid")
                             (cons "B" "http://authority-b.invalid"))))
          (hermes-dashboard-transport-url "http://authority-a.invalid")
          (hermes-dashboard-transport-start-mode 'remote)
          (hermes-dashboard-transport-request-timeout nil)
          (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
          (hermes-notifications-events nil)
          (hermes-cron-auto-refresh-interval nil)
          (hermes-bot-test--wire nil)
          (hermes-bot-test--requests nil)
          (hermes-bot-test--destinations nil)
          (before (buffer-list))
          (hermes-bot-test--client nil))
     (dolist (url '("http://authority-a.invalid" "http://authority-b.invalid"))
       ;; The fixture owns one lease; UI acquisition/release stays native.
       (puthash url (make-hermes-dashboard-transport-client
                     :base-url url :endpoint-key url :refcount 1
                     :token "synthetic" :ready-p t :websocket url)
                hermes-dashboard-transport--clients))
     (setq hermes-bot-test--client
           (gethash "http://authority-a.invalid" hermes-dashboard-transport--clients))
     (cl-letf (((symbol-value 'hermes-dashboard-transport-websocket-send-function)
                (lambda (socket text)
                  (push socket hermes-bot-test--destinations)
                  (push (hermes-transport-json-parse text) hermes-bot-test--wire)))
               ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                (lambda (request &rest _)
                  (push request hermes-bot-test--requests)
                  (hermes--promise-resolved
                   (list :body
                         (cond
                          ((string-suffix-p "/api/profiles" (plist-get request :url))
                           '((profiles . (((name . "alpha"))))))
                          ((string-match-p (regexp-quote "/jobs?profile=") (plist-get request :url))
                           '((jobs . (((id . "job-a") (name . "Routine A")
                                       (profile . "alpha") (enabled . t))))))
                          ((string-match-p (regexp-quote "/runs?") (plist-get request :url))
                           '((runs . (((id . "run-a") (source . "cron") (profile . "alpha"))))))
                          ((string-match-p (regexp-quote "/messages?") (plist-get request :url))
                           '((session_id . "run-a") (messages . [])
                             (pagination . ((limit . 500) (offset . 0)
                                            (order . "oldest") (returned . 0)))))
                          (t '((id . "job-a") (name . "Routine A") (profile . "alpha")
                               (schedule . "0 * * * *") (prompt . "Original") (ok . t)))))))))
       (unwind-protect
           (with-temp-buffer
             (hermes-profiles-mode)
             (hermes-buffer--claim 'hermes-profiles-mode)
             (hermes-browser--own-instance
              (or (car hermes-instances) '("default" . "http://authority-a.invalid")))
             (hermes-profiles--render '((profiles . (((name . "alpha"))))))
             (goto-char (point-min))
             ,@body)
         (dolist (buffer (seq-difference (buffer-list) before))
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun hermes-bot-test--open-canonical ()
  "Open and hydrate A's canonical conversation through public Profiles."
  (call-interactively #'hermes-profiles-open-bot-chat)
  (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "root-a" "tip-a"))
  (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root-a" "tip-a"))
  (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root-a" "tip-a"))
  (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "root-a" "tip-a"))
  (hermes-bot-test--reply
   "session.resume" '((session_id . "runtime-a") (stored_session_id . "tip-a")
                      (running . :false) (messages . [])))
  (window-buffer))

(ert-deftest hermes-bot-chat-constructor-hook-and-history-retry-keep-backend ()
  "Constructor hooks and failed-history Send retry keep captured authority."
  (dolist (named '(nil t))
    (hermes-bot-test--with-backends named
      (let ((hermes-chat-mode-hook
             (list (lambda ()
                     (setq hermes-dashboard-transport-url "http://authority-b.invalid")))))
        (call-interactively #'hermes-profiles-open-bot-chat)
        (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "root-a" "tip-a"))
        (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root-a" "tip-a"))
        (hermes-bot-test--reply "session.list" (hermes-bot-test--rows "root-a" "tip-a"))
        (hermes-bot-test--reply "profiles.list" (hermes-bot-test--registry "alpha" "root-a" "tip-a"))
        (should (equal hermes-dashboard-transport-url "http://authority-b.invalid"))
        (with-current-buffer (window-buffer)
          (should (eq hermes-chat--dashboard-client hermes-bot-test--client))
          (hermes-bot-test--reply "session.resume" "temporary history failure" 5006)
          (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed))
          (goto-char (point-max))
          (insert "Retry my draft")
          (call-interactively #'hermes-chat-send)
          (should (equal hermes-chat--bot-chat-root "root-a"))
          (should (equal hermes-chat--profile "alpha"))
          (hermes-bot-test--reply
           "session.resume" '((session_id . "retry-a") (stored_session_id . "tip-a")
                              (running . :false) (messages . [])))
          (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method)
                         "prompt.submit"))
          (should (equal (hermes-transport--get
                          (hermes-transport--get (car hermes-bot-test--wire) 'params) 'text)
                         "Retry my draft")))
        (should (seq-every-p (lambda (url) (equal url "http://authority-a.invalid"))
                            hermes-bot-test--destinations))))))

(ert-deftest hermes-bot-chat-native-acquire-reconnect-backend ()
  "Canonical reconnect and Send never lend A's tip or draft to B."
  (dolist (named '(nil t))
    (dolist (changed '(nil t))
      (hermes-bot-test--with-backends named
        (let ((chat (hermes-bot-test--open-canonical)))
          (with-current-buffer chat
            (hermes-chat--stop-dashboard-client)
            (when changed
              (setq hermes-dashboard-transport-url "http://authority-b.invalid"))
            (goto-char (point-max))
            (insert "Deliberate draft")
            (call-interactively #'hermes-chat-send)
            (should (equal hermes-chat--bot-chat-root "root-a"))
            (should (equal hermes-chat--profile "alpha"))
            (should (eq hermes-chat--dashboard-client hermes-bot-test--client))
            (should (equal (hermes-transport--get
                            (hermes-transport--get (car hermes-bot-test--wire) 'params)
                            'session_id) "tip-a"))
            (hermes-bot-test--reply
             "session.resume" '((session_id . "runtime-a-again") (stored_session_id . "tip-a")
                                (running . :false) (messages . [])))
            (let ((params (hermes-transport--get (car hermes-bot-test--wire) 'params)))
              (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method)
                             "prompt.submit"))
              (should (equal (hermes-transport--get params 'text) "Deliberate draft"))
              (should (equal (hermes-transport--get params 'session_id) "runtime-a-again"))))
          (should (seq-every-p (lambda (url) (equal url "http://authority-a.invalid"))
                              hermes-bot-test--destinations)))))))

(ert-deftest hermes-bot-chat-routines-native-acquire-retarget ()
  "Real Routines rows keep refresh, toggle and edit on their original backend."
  (dolist (named '(nil t))
    (hermes-bot-test--with-backends named
      (call-interactively #'hermes-profiles-routines)
      (with-current-buffer (window-buffer)
        (should (equal (tabulated-list-get-id) "job-a"))
        (setq hermes-dashboard-transport-url "http://authority-b.invalid")
        (call-interactively #'revert-buffer)
        (should (equal (tabulated-list-get-id) "job-a"))
        (should (seq-every-p (lambda (r) (equal (plist-get r :method) "GET"))
                            hermes-bot-test--requests))
        (call-interactively #'hermes-cron-toggle)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (if (equal prompt "Name: ") "Renamed" initial)))
                  ((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Original")))
          (call-interactively #'hermes-cron-edit))
        (should (seq-some (lambda (r) (equal (plist-get r :method) "PUT"))
                          hermes-bot-test--requests))
        (should (equal hermes-cron--scope-profile "alpha")))
      (should (seq-every-p
               (lambda (r) (string-prefix-p "http://authority-a.invalid/" (plist-get r :url)))
               hermes-bot-test--requests)))))

(ert-deftest hermes-bot-chat-new-continuations-retain-backend ()
  "Native /new keeps compression and explicit scratch creation on A."
  (dolist (choice '("Compress conversation" "Open scratch chat"))
    (hermes-bot-test--with-backends nil
      (let ((chat (hermes-bot-test--open-canonical)))
        (with-current-buffer chat
          (hermes-chat--stop-dashboard-client)
          (goto-char (point-max))
          (insert "/new")
          (cl-letf (((symbol-value 'completing-read-function)
                     (lambda (&rest _)
                       (setq hermes-dashboard-transport-url "http://authority-b.invalid")
                       choice)))
            (call-interactively #'hermes-chat-send)))
        (if (equal choice "Compress conversation")
            (progn
              (hermes-bot-test--reply
               "session.resume" '((session_id . "runtime-a-again") (stored_session_id . "tip-a")
                                  (running . :false) (messages . [])))
              (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method)
                             "session.compress"))
              (should (equal (buffer-local-value 'hermes-chat--bot-chat-root chat) "root-a")))
          (let ((scratch (window-buffer)))
            (should-not (eq scratch chat))
            (with-current-buffer scratch
              (should-not hermes-chat--bot-chat-root)
              (should (equal hermes-chat--profile "alpha"))
              (goto-char (point-max))
              (insert "Explicit scratch input")
              (call-interactively #'hermes-chat-send))
            (hermes-bot-test--reply
             "session.create" '((session_id . "scratch-a") (info . ((profile_name . "alpha")))))
            (should (equal (hermes-transport--get (car hermes-bot-test--wire) 'method)
                           "prompt.submit"))))
        (should (seq-every-p (lambda (url) (equal url "http://authority-a.invalid"))
                            hermes-bot-test--destinations))))))

(ert-deftest hermes-bot-chat-routines-retarget-during-input-and-auth ()
  "Confirmation, editing and delayed REST authentication retain scoped A."
  (dolist (boundary '(confirmation reader auth))
    (hermes-bot-test--with-backends nil
      (call-interactively #'hermes-profiles-routines)
      (with-current-buffer (window-buffer)
        (pcase boundary
          ('confirmation
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (&rest _)
                        (setq hermes-dashboard-transport-url "http://authority-b.invalid") t)))
             (call-interactively #'hermes-cron-remove))
           (should (seq-some (lambda (r) (equal (plist-get r :method) "DELETE"))
                             hermes-bot-test--requests)))
          ('reader
           (cl-letf (((symbol-function 'read-string)
                      (lambda (prompt &optional initial &rest _)
                        (setq hermes-dashboard-transport-url "http://authority-b.invalid")
                        (if (equal prompt "Name: ") "Renamed" initial)))
                     ((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Original")))
             (call-interactively #'hermes-cron-edit))
           (should (seq-some (lambda (r) (equal (plist-get r :method) "PUT"))
                             hermes-bot-test--requests)))
          ('auth
           (let ((pending (hermes--promise-make))
                 (hermes-dashboard-transport--api-auth nil)
                 captured)
             (setf (hermes-dashboard-transport-client-token hermes-bot-test--client) nil)
             (cl-letf (((symbol-function 'hermes-dashboard-transport--api-authenticate-async)
                        (lambda ()
                          (setq captured hermes-dashboard-transport--api-auth-base-url)
                          pending)))
               (call-interactively #'hermes-cron-toggle))
             (should (equal captured "http://authority-a.invalid"))
             (should (= 1 (length hermes-bot-test--requests)))
             (setq hermes-dashboard-transport-url "http://authority-b.invalid")
             (with-temp-buffer
               (hermes--promise-resolve pending (list :base-url captured :session-token "synthetic")))
             (should (seq-some (lambda (r) (equal (plist-get r :method) "POST"))
                               hermes-bot-test--requests)))))
        (should (equal hermes-cron--scope-profile "alpha")))
      (should (seq-every-p
               (lambda (r) (string-prefix-p "http://authority-a.invalid/" (plist-get r :url)))
               hermes-bot-test--requests)))))

(ert-deftest hermes-bot-chat-routines-pin-before-display-and-reject-wrong-reuse ()
  "Display hooks and a misrouted ordinary chat cannot retarget a scoped view."
  (hermes-bot-test--with-backends nil
    (let* ((origin (current-buffer))
           (ordinary (hermes-chat--new-buffer "alpha" nil hermes-instance)))
      (with-current-buffer ordinary
        ;; Exercise the ordinary policy: it can resolve the changed default.
        (setq hermes-dashboard-transport-url "http://authority-b.invalid")
        (should-not hermes-chat--pinned-url)
        (should (eq (hermes-chat--dashboard-ensure-client)
                    (gethash "http://authority-b.invalid" hermes-dashboard-transport--clients))))
      (setq hermes-dashboard-transport-url "http://authority-a.invalid")
      (let ((count (length hermes-bot-test--wire)))
        (with-current-buffer origin
          (call-interactively #'hermes-profiles-open-bot-chat)
          (should (equal hermes-browser--status "Failed; g retry")))
        (should (= count (length hermes-bot-test--wire))))
      (let ((display-buffer-alist
             '(("\\*Hermes Routines" (lambda (buffer _alist)
                                        (setq hermes-dashboard-transport-url "http://authority-b.invalid")
                                        (display-buffer-same-window buffer nil))))))
        (with-current-buffer origin (call-interactively #'hermes-profiles-routines)))
      (with-current-buffer (window-buffer)
        (should (equal (tabulated-list-get-id) "job-a"))
        (should (equal (hermes-instance-url hermes-instance) "http://authority-a.invalid"))
        (call-interactively #'hermes-cron-toggle))
      (should (seq-every-p
               (lambda (r) (string-prefix-p "http://authority-a.invalid/" (plist-get r :url)))
               hermes-bot-test--requests)))))

(ert-deftest hermes-bot-chat-routines-named-snapshot-and-ordinary-control ()
  "Named configuration edits do not mutate scoped identity; ordinary g can retarget."
  (hermes-bot-test--with-backends t
    (call-interactively #'hermes-profiles-routines)
    (setcdr (car hermes-instances) "http://authority-b.invalid")
    (with-current-buffer (window-buffer)
      (call-interactively #'revert-buffer)
      (call-interactively #'hermes-cron-toggle))
    (should (seq-every-p
             (lambda (r) (string-prefix-p "http://authority-a.invalid/" (plist-get r :url)))
             hermes-bot-test--requests)))
  (hermes-bot-test--with-backends nil
    (call-interactively #'hermes-list-crons)
    (with-current-buffer (window-buffer)
      (should-not hermes-browser--pinned-instance)
      (should-not hermes-cron--scope-profile)
      (setq hermes-dashboard-transport-url "http://authority-b.invalid")
      (call-interactively #'revert-buffer))
    (should (equal (plist-get (car hermes-bot-test--requests) :url)
                   "http://authority-b.invalid/api/cron/jobs?profile=all"))))

(ert-deftest hermes-bot-chat-routines-inherited-actions-keep-scope ()
  "Creation, execution preferences, trigger and detail/log actions use pinned A."
  (dolist (action '(hermes-cron-create hermes-cron-edit-preferences
                   hermes-cron-trigger hermes-cron-show))
    (hermes-bot-test--with-backends nil
      (call-interactively #'hermes-profiles-routines)
      (with-current-buffer (window-buffer)
        (setq hermes-dashboard-transport-url "http://authority-b.invalid")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (cond ((equal prompt "Cron job name: ") "New routine")
                           ((equal prompt "Schedule (cron expression): ") "0 * * * *")
                           (t (or initial "")))))
                  ((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Authored prompt"))
                  ((symbol-value 'completing-read-function)
                   (lambda (prompt &rest _)
                     (if (equal prompt "Initial state: ") "Create paused" "Inherit"))))
          (call-interactively action)))
      (pcase action
        ('hermes-cron-show
         (with-current-buffer (window-buffer)
           (should (eq major-mode 'hermes-cron-detail-mode))
           (goto-char (point-min))
           (search-forward "run-a")
           (call-interactively #'hermes-cron-show-run-log))
         (should (string-match-p "/sessions/run-a/messages"
                                 (plist-get (car hermes-bot-test--requests) :url)))
         (should (eq (buffer-local-value 'major-mode (window-buffer)) 'hermes-cron-run-mode)))
        (_ (should (seq-some (lambda (r) (member (plist-get r :method) '("PUT" "POST")))
                            hermes-bot-test--requests))))
      (should (seq-every-p
               (lambda (r) (and (string-prefix-p "http://authority-a.invalid/" (plist-get r :url))
                                (or (string-suffix-p "/api/profiles" (plist-get r :url))
                                    (string-match-p "profile=alpha" (plist-get r :url)))))
               hermes-bot-test--requests)))))

(ert-deftest hermes-bot-chat-reconnect-readiness-keeps-draft-authority ()
  "Delayed connection readiness cannot redirect a retained input to B."
  (hermes-bot-test--with-backends nil
    (let ((chat (hermes-bot-test--open-canonical))
          (ready (hermes--promise-make)))
      (with-current-buffer chat
        (hermes-chat--stop-dashboard-client)
        (setf (hermes-dashboard-transport-client-ready-p hermes-bot-test--client) nil
              (hermes-dashboard-transport-client-ready-promise hermes-bot-test--client) ready)
        (goto-char (point-max))
        (insert "Retained through readiness")
        (call-interactively #'hermes-chat-send)
        (setq hermes-dashboard-transport-url "http://authority-b.invalid")
        (should (string-match-p "Retained through readiness" (buffer-string)))
        (should (equal hermes-chat--bot-chat-root "root-a"))
        (should (equal hermes-chat--profile "alpha")))
      (with-temp-buffer
        (setf (hermes-dashboard-transport-client-ready-p hermes-bot-test--client) t)
        (hermes--promise-resolve ready t))
      (hermes-bot-test--reply
       "session.resume" '((session_id . "runtime-a-again") (stored_session_id . "tip-a")
                          (running . :false) (messages . [])))
      (should (equal (hermes-transport--get
                      (hermes-transport--get (car hermes-bot-test--wire) 'params) 'text)
                     "Retained through readiness"))
      (should (seq-every-p (lambda (url) (equal url "http://authority-a.invalid"))
                          hermes-bot-test--destinations)))))

(defun hermes-bot-test--constructor-prewarm (scratch)
  "Exercise native hook acquisition for canonical or SCRATCH construction."
  (dolist (drift '(nil t))
      (hermes-bot-test--with-backends nil
        (let* ((calls 0)
               (hermes-chat-mode-hook
               (list (lambda ()
                       (cl-incf calls)
                       (when drift
                         (setq hermes-dashboard-transport-url "http://authority-b.invalid"))
                       (hermes-chat--dashboard-ensure-client)))))
          (let ((chat (if scratch
                          (hermes-chat--new-buffer
                           "alpha" nil '("A" . "http://authority-a.invalid")
                           "http://authority-a.invalid")
                        (hermes-bot-test--open-canonical))))
            (should (= calls 1))
            (with-current-buffer chat
              (should (equal hermes-chat--pinned-url "http://authority-a.invalid"))
              (should (eq hermes-chat--dashboard-client hermes-bot-test--client))
              (should (equal hermes-chat--profile "alpha"))
              (should (equal hermes-chat--bot-chat-root (unless scratch "root-a"))))
            (should (= 2 (hermes-dashboard-transport-client-refcount hermes-bot-test--client)))
            (should-not (member "http://authority-b.invalid" hermes-bot-test--destinations))
            (kill-buffer chat)
            (should (= 1 (hermes-dashboard-transport-client-refcount hermes-bot-test--client)))
            (should (= 1 (hermes-dashboard-transport-client-refcount
                          (gethash "http://authority-b.invalid"
                                   hermes-dashboard-transport--clients)))))))))

(ert-deftest hermes-bot-chat-constructor-prewarm-canonical ()
  "Canonical lookup and readback keep A despite hook-driven acquisition."
  (hermes-bot-test--constructor-prewarm nil))

(ert-deftest hermes-bot-chat-constructor-prewarm-scratch ()
  "Explicitly pinned scratch construction also keeps A through native hooks."
  (hermes-bot-test--constructor-prewarm t))

(ert-deftest hermes-bot-chat-live-client-effective-authority ()
  "Reject an owned wrong live lease; accept a spawn client's effective URL."
  (dolist (spawn '(nil t))
    (dolist (matching '(nil t))
      (hermes-bot-test--with-backends nil
        (let* ((url (if spawn "http://127.0.0.1:8642" "http://authority-a.invalid"))
               (client (gethash (if matching "http://authority-a.invalid"
                                 "http://authority-b.invalid")
                               hermes-dashboard-transport--clients))
               (chat (hermes-chat--new-buffer
                      "alpha" nil (cons "A" url) url)))
          (when spawn
            (setf (hermes-dashboard-transport-client-base-url client) nil
                  (hermes-dashboard-transport-client-host client) "127.0.0.1"
                  (hermes-dashboard-transport-client-port client) (if matching 8642 8643)))
          (with-current-buffer chat
            ;; Acquire the lease natively, modelling an already-owned client.
            (let ((hermes-dashboard-transport-url
                   (hermes-dashboard-transport-client-endpoint-key client)))
              (setq hermes-chat--dashboard-client
                    (hermes-dashboard-transport-acquire :start-mode 'remote)))
            (setq hermes-chat--bot-chat-root "root-a")
            (goto-char (point-max))
            (insert "Retain this draft")
            (if matching
                (should (eq client (hermes-chat--dashboard-ensure-client)))
              (should-error (hermes-chat--dashboard-ensure-client) :type 'user-error)
              (should-not hermes-chat--dashboard-client)
              (should (= 1 (hermes-dashboard-transport-client-refcount client))))
            (should (equal hermes-chat--bot-chat-root "root-a"))
            (should (equal hermes-chat--profile "alpha"))
            (should (string-match-p "Retain this draft" (buffer-string))))
          (kill-buffer chat)
          (should (= 1 (hermes-dashboard-transport-client-refcount client)))
          (should-not hermes-bot-test--wire))))))

(ert-deftest hermes-bot-chat-unpinned-constructor-prewarm-control ()
  "Ordinary new chats retain native unpinned hook acquisition policy."
  (hermes-bot-test--with-backends nil
    (let ((hermes-chat-mode-hook
           (list (lambda ()
                   (setq hermes-dashboard-transport-url "http://authority-b.invalid")
                   (hermes-chat--dashboard-ensure-client)))))
      (let* ((chat (hermes-chat--new-buffer "alpha"))
             (client (buffer-local-value 'hermes-chat--dashboard-client chat)))
        (should-not (buffer-local-value 'hermes-chat--pinned-url chat))
        (should (equal (hermes-dashboard-transport--api-client-base-url client)
                       "http://authority-b.invalid"))
        (kill-buffer chat)
        (should (= 1 (hermes-dashboard-transport-client-refcount client)))))))

(ert-deftest hermes-bot-chat-routines-copy-before-mode-hook ()
  "Public Profiles selection snapshots mutable backend and profile before hooks."
  (dolist (drift '(nil t))
    (hermes-bot-test--with-backends t
      (setcdr (car hermes-instances) (copy-sequence (cdar hermes-instances)))
      (with-temp-buffer
        (let ((completing-read-function (lambda (&rest _) "A")))
          (call-interactively #'hermes-list-profiles)))
      (with-current-buffer (window-buffer)
        (let* ((profile (copy-sequence (tabulated-list-get-id)))
               (hermes-cron-mode-hook
                (list (lambda ()
                        (when drift
                          (let ((url (cdar hermes-instances)))
                            (aset url (string-match "a\\.invalid" url) ?b))
                          (aset profile 0 ?o))))))
          ;; Native rows expose mutable profile IDs just like backend strings.
          (hermes-profiles--render `((profiles . (((name . ,profile))))))
          (goto-char (point-min))
          (call-interactively #'hermes-profiles-routines)))
      (with-current-buffer (window-buffer)
        (should (equal hermes-browser--pinned-instance '("A" . "http://authority-a.invalid")))
        (should (equal hermes-cron--scope-profile "alpha"))
        (call-interactively #'revert-buffer)
        (call-interactively #'hermes-cron-toggle))
      (should (seq-some (lambda (r) (equal (plist-get r :method) "POST"))
                        hermes-bot-test--requests))
      (should (seq-every-p
               (lambda (r)
                 (and (string-prefix-p "http://authority-a.invalid/" (plist-get r :url))
                      (or (string-suffix-p "/api/profiles" (plist-get r :url))
                          (string-match-p "profile=alpha" (plist-get r :url)))))
               hermes-bot-test--requests)))))

(defun hermes-bot-test--routines-hook-refresh (generic drift)
  "Refresh from a native mode hook, optionally GENERIC, with optional URL DRIFT."
  (hermes-bot-test--with-backends nil
    (setq hermes-dashboard-transport-url
          (copy-sequence hermes-dashboard-transport-url))
    (let* ((specific-calls 0)
           (generic-calls 0)
           (refresh (lambda ()
                      (when drift
                        (aset hermes-dashboard-transport-url
                              (string-match (regexp-quote "a.invalid")
                                            hermes-dashboard-transport-url) ?b))
                      (call-interactively #'revert-buffer)))
           (hermes-cron-mode-hook
            (list (lambda ()
                    (cl-incf specific-calls)
                    (unless generic (funcall refresh)))))
           (after-change-major-mode-hook
            (list (lambda ()
                    (when (eq major-mode 'hermes-cron-mode)
                      (cl-incf generic-calls)
                      (when generic (funcall refresh)))))))
      (call-interactively #'hermes-profiles-routines)
      (should (= specific-calls 1))
      (should (= generic-calls 1))
      ;; Check every request, including the one dispatched before return.
      (should (= (length hermes-bot-test--requests) 2))
      (dolist (request hermes-bot-test--requests)
        (should (equal (plist-get request :method) "GET"))
        (should (equal (plist-get request :url)
                       "http://authority-a.invalid/api/cron/jobs?profile=alpha")))
      (with-current-buffer (window-buffer)
        (should (hermes-buffer--owned-p 'hermes-cron-mode))
        (should (equal hermes-instance hermes-browser--pinned-instance))
        (should (equal hermes-cron--scope-profile "alpha"))
        (call-interactively #'hermes-cron-toggle)
        (should (seq-some (lambda (request)
                            (and (equal (plist-get request :method) "POST")
                                 (equal (plist-get request :url)
                                        "http://authority-a.invalid/api/cron/jobs/job-a/pause?profile=alpha")))
                          hermes-bot-test--requests))
        (kill-buffer))
      (maphash (lambda (_ client)
                 (should (= 1 (hermes-dashboard-transport-client-refcount client))))
               hermes-dashboard-transport--clients))))

(ert-deftest hermes-bot-chat-routines-native-hook-refresh-same-owner ()
  "Specific mode hooks refresh the selected A/alpha, never all jobs."
  (hermes-bot-test--routines-hook-refresh nil nil))

(ert-deftest hermes-bot-chat-routines-native-hook-refresh-url-drift ()
  "Specific mode hooks cannot redirect refresh through a mutable URL."
  (hermes-bot-test--routines-hook-refresh nil t))

(ert-deftest hermes-bot-chat-routines-generic-hook-refresh-same-owner ()
  "Generic mode hooks also see selected authority before native refresh."
  (hermes-bot-test--routines-hook-refresh t nil))

(ert-deftest hermes-bot-chat-routines-generic-hook-refresh-url-drift ()
  "Generic mode-hook refresh retains A/alpha after an in-place URL edit."
  (hermes-bot-test--routines-hook-refresh t t))

(ert-deftest hermes-bot-chat-routines-hook-retirement ()
  "Do not reclaim or dispatch after native hooks retire the scoped view."
  (dolist (transition '(kill mode file))
    (hermes-bot-test--with-backends nil
      (let* (target
             (hermes-cron-mode-hook
              (list (lambda ()
                      (setq target (current-buffer))
                      (pcase transition
                        ('kill (kill-buffer))
                        ('mode
                         (fundamental-mode)
                         (setq buffer-read-only nil)
                         (insert "Retained notes"))
                        ('file
                         (set-visited-file-name
                          (expand-file-name "routine-hook-notes" temporary-file-directory) t)
                         (set-visited-file-name nil t)
                         (let ((inhibit-read-only t)) (insert "Retained notes"))))))))
        (should-error (call-interactively #'hermes-profiles-routines) :type 'user-error)
        (should-not hermes-bot-test--requests)
        (when (buffer-live-p target)
          (with-current-buffer target
            (should (string-match-p "Retained notes" (buffer-string)))
            (should-not (hermes-buffer--owned-p 'hermes-cron-mode))
            (set-buffer-modified-p nil)))
        (should (= 1 (hermes-dashboard-transport-client-refcount hermes-bot-test--client)))))))

(ert-deftest hermes-bot-chat-routines-hook-reentry-and-action ()
  "Nested Routines construction keeps separate owners and inherited actions."
  (hermes-bot-test--with-backends nil
    (let* ((calls 0) nested
           (hermes-cron-mode-hook
            (list (lambda ()
                    (cl-incf calls)
                    (when (= calls 1)
                      (save-current-buffer
                        (setq nested
                              (hermes-cron-for-profile
                               "alpha" '("B" . "http://authority-b.invalid")))))
                    (call-interactively #'revert-buffer)
                    (call-interactively #'hermes-cron-toggle)))))
      (call-interactively #'hermes-profiles-routines)
      (should (= calls 2))
      (should-not (eq nested (window-buffer)))
      (should (= 2 (seq-count (lambda (request)
                               (equal (plist-get request :method) "POST"))
                             hermes-bot-test--requests)))
      (dolist (request hermes-bot-test--requests)
        (let ((url (plist-get request :url)))
          (should (or (and (string-prefix-p "http://authority-a.invalid/" url)
                           (string-suffix-p "profile=alpha" url))
                      (and (string-prefix-p "http://authority-b.invalid/" url)
                           (string-suffix-p "profile=alpha" url))))))
      (kill-buffer nested)
      (kill-buffer (window-buffer))
      (maphash (lambda (_ client)
                 (should (= 1 (hermes-dashboard-transport-client-refcount client))))
               hermes-dashboard-transport--clients))))

(ert-deftest hermes-bot-chat-integration-constructor-argument-snapshots ()
  "Bot fifth and foreign sixth arguments retain copied pre-hook authority."
  (dolist (kind '(bot foreign explicit scratch))
    (hermes-bot-test--with-backends nil
      (let* ((instance (cons (copy-sequence "A")
                             (copy-sequence "http://authority-a.invalid")))
             (profile (copy-sequence "alpha"))
             (session (copy-sequence "stored-a"))
             (root (and (memq kind '(bot explicit)) (copy-sequence "root-a")))
             (pin (and (not (eq kind 'bot))
                       (copy-sequence "http://authority-a.invalid")))
             (hermes-chat-mode-hook
              (list (lambda ()
                      (aset (car instance) 0 ?B)
                      (aset (cdr instance) 17 ?b)
                      (aset profile 0 ?o)
                      (aset session 0 ?x)
                      (when root (aset root 0 ?x))
                      (when pin (aset pin 17 ?b))
                      (hermes-chat--dashboard-ensure-client)))))
        (let ((chat (if (eq kind 'scratch)
                        (hermes-chat--new-buffer profile nil instance pin)
                      (hermes-chat-resume-session session "Bot or foreign" profile
                                                  instance root pin))))
          (with-current-buffer chat
            (should (equal hermes-instance '("A" . "http://authority-a.invalid")))
            (should (equal hermes-chat--profile "alpha"))
            (should (equal hermes-chat--pinned-url "http://authority-a.invalid"))
            (should (equal hermes-chat--bot-chat-root
                           (and (memq kind '(bot explicit)) "root-a")))
            (unless (eq kind 'scratch)
              (should (equal hermes-chat--session-id "stored-a")))
            (should (eq hermes-chat--dashboard-client hermes-bot-test--client)))
          (kill-buffer chat)
          (should (= 1 (hermes-dashboard-transport-client-refcount
                        hermes-bot-test--client))))))))

(ert-deftest hermes-bot-chat-integration-reject-cold-wrong-lease ()
  "Cold acquisition rejects the wrong endpoint before adoption or warming."
  (hermes-bot-test--with-backends nil
    (let* ((chat (hermes-chat--new-buffer
                  "alpha" nil '("A" . "http://authority-a.invalid")
                  "http://authority-a.invalid"))
           (other (gethash "http://authority-b.invalid"
                           hermes-dashboard-transport--clients))
           (acquire (symbol-function 'hermes-dashboard-transport-acquire))
           warmed)
      (with-current-buffer chat
        (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest args)
                     (let ((hermes-dashboard-transport-url "http://authority-b.invalid"))
                       (apply acquire args))))
                  ((symbol-function 'hermes-chat--warm-model-options)
                   (lambda (client) (push client warmed))))
          (should-error (hermes-chat--dashboard-ensure-client) :type 'user-error)
          (should-not hermes-chat--dashboard-client)
          (should-not warmed)
          (should (= 1 (hermes-dashboard-transport-client-refcount other)))))
      (kill-buffer chat))))

(provide 'hermes-bot-chat-tests)
;;; hermes-bot-chat-tests.el ends here
