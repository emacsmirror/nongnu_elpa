;;; hermes-chat-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'delsel)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-send-uses-transport-and-creates-pending-assistant ()
  (let (sent callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt cb)
              (setq sent prompt
                    callback cb)
              'fake-process)))
       (insert "hello Hermes")
       (hermes-chat-send)
       (should (equal sent "hello Hermes"))
       (should (functionp callback))
       (should (equal (hermes-chat-input-string) ""))
       (pcase-let ((`(,user ,assistant) (hermes-chat--entries)))
         (should (equal (plist-get user :role) 'user))
         (should (equal (plist-get user :content) "hello Hermes"))
         (should (equal (plist-get assistant :role) 'assistant))
         (should (equal (plist-get assistant :status) 'pending))
         (should (equal (plist-get assistant :content) "")))))))

(ert-deftest hermes-chat-transport-updates-preserve-draft-input ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback '(:type delta :content "hello"))
       (insert "draft survives")
       (funcall callback '(:type delta :content " there"))
       (should (equal (hermes-chat-input-string) "draft survives"))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :status) 'streaming))
         (should (equal (plist-get assistant :content) "hello there")))
       (funcall callback '(:type done))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :status) 'done))
         (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-notification-follows-completed-reply ()
  "A completed turn notifies with the settled assistant text and owning buffer."
  (let (callback notice)
    (cl-letf (((symbol-function 'hermes-notifications-notify)
               (lambda (&rest arguments) (setq notice arguments))))
      (hermes-test-with-chat-buffer
       (let ((buffer (current-buffer))
             (hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "notify me")
         (hermes-chat-send)
         (funcall callback '(:type delta :content "Finished\ncleanly"))
         (funcall callback '(:type done))
         (should (eq (car notice) 'chat-reply))
         (should (equal (nth 1 notice)
                        (format "%s: Finished cleanly" (buffer-name buffer))))
         (should (equal (nth 2 notice) "Finished cleanly"))
         (should (eq (plist-get (nthcdr 3 notice) :buffer) buffer)))))))

(ert-deftest hermes-chat-notification-reports-terminal-error-not-interrupt ()
  "A real terminal error notifies, while an intentional interrupt does not."
  (let (callback notices)
    (cl-letf (((symbol-function 'hermes-notifications-notify)
               (lambda (&rest arguments) (push arguments notices))))
      (hermes-test-with-chat-buffer
       (let ((hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "fail")
         (hermes-chat-send)
         (funcall callback '(:type error :content "backend failed"))
         (should (equal (mapcar #'car notices) '(chat-error)))
         (should (equal (nth 2 (car notices)) "backend failed"))))
      (setq notices nil)
      (hermes-test-with-chat-buffer
       (let ((hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "interrupt")
         (hermes-chat-send)
         (funcall callback '(:type error :status interrupted))
         (should-not notices))))))

(ert-deftest hermes-chat-rename-updates-title-not-project-identity ()
  "Renaming stores a canonical title without changing project identity."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time 45 30 18 7 8 2026 t))))
    (hermes-test-with-chat-buffer
     (let ((project-name (buffer-name)))
       (hermes-chat-rename "  My Project  ")
       (should (equal hermes-chat--title
                      "My Project--20260807T183045.000000Z--emacs"))
       (should hermes-chat--title-manual-p)
       (should (equal (buffer-name) project-name))))))

(ert-deftest hermes-chat-rename-rejects-empty-title ()
  (hermes-test-with-chat-buffer
   (should-error (hermes-chat-rename "   ") :type 'user-error)))

(ert-deftest hermes-chat-rename-pushes-server-title-when-attached ()
  "An attached session pushes `session.title' with the live session id."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time 45 30 18 7 8 2026 t))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-active-session-id "sid-1")
     (let (sent)
       (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                  (lambda () t))
                 ((symbol-function 'hermes-dashboard-transport-session-title)
                  (lambda (_client &rest args) (setq sent args))))
         (hermes-chat-rename "Renamed"))
       (should (equal (plist-get sent :session-id) "sid-1"))
       (should (equal (plist-get sent :title)
                      "Renamed--20260807T183045.000000Z--emacs"))))))

(ert-deftest hermes-chat-rename-prompt-rejects-replaced-owner ()
  "A real rename answer cannot mutate any replacement owner."
  (dolist (changed '(lifetime transport session fresh-session client mode killed))
    (hermes-test-with-chat-buffer
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (insert "Exact draft")
        (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
              hermes-chat--dashboard-session-ready-p t
              hermes-chat--dashboard-active-session-id
              (unless (eq changed 'fresh-session) "session-original"))
        (let* ((owner (current-buffer)) (noninteractive nil) sent
               (minibuffer-setup-hook
                (cons (lambda ()
                        (with-current-buffer owner
                          (pcase changed
                            ('lifetime
                             (setq hermes-chat--lifecycle-generation
                                   (hermes-chat--next-lifetime-token)))
                            ('transport (cl-incf hermes-chat--transport-generation))
                            ((or 'session 'fresh-session)
                             (setq hermes-chat--dashboard-active-session-id
                                   "session-successor"))
                            ('client
                             (setq hermes-chat--dashboard-client
                                   (hermes-test--dashboard-client)))
                            ('mode (fundamental-mode))
                            ('killed (kill-buffer owner)))
                          (when (buffer-live-p owner)
                            (setq hermes-chat--title "Successor"
                                  hermes-chat--title-manual-p nil))))
                      minibuffer-setup-hook)))
          (cl-letf (((symbol-function 'hermes-chat--dashboard-client-live-p)
                     (lambda (_client) t))
                    ((symbol-function 'hermes-dashboard-transport-session-title)
                     (lambda (_client &rest args) (setq sent args))))
            (unwind-protect
                (progn
                  (should (equal
                           (should-error
                            (execute-kbd-macro
                             (kbd "C-c C-o S R r e n a m e d RET"))
                            :type 'user-error)
                           '(user-error "Hermes rename prompt is no longer current")))
                  (should-not sent)
                  (when (buffer-live-p owner)
                    (with-current-buffer owner
                      (should (equal hermes-chat--title "Successor"))
                      (should-not hermes-chat--title-manual-p)
                      (when (derived-mode-p 'hermes-chat-mode)
                        (should (equal (hermes-chat-input-string) "Exact draft"))))))
              (keymap-popup-dismiss)
              (when (buffer-live-p owner)
                (with-current-buffer owner
                  (setq hermes-chat--dashboard-client nil))))))))))

(ert-deftest hermes-chat-rename-prompt-uses-original-session ()
  "An unchanged real prompt renames its original session, preserving the draft."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (buffer-enable-undo)
      (insert "Exact draft")
      (undo-boundary)
      (setq hermes-chat--dashboard-active-session-id "session-original")
      (let ((owner (current-buffer)) (noninteractive nil)
            (before (buffer-string)) (position (point))
            (undo (copy-tree buffer-undo-list)) sent)
        (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                   (lambda () t))
                  ((symbol-function 'hermes-dashboard-transport-session-title)
                   (lambda (_client &rest args) (setq sent args))))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "C-c C-o S R r e n a m e d RET"))
                (should (equal (plist-get sent :session-id) "session-original"))
                (should (equal (plist-get sent :title) hermes-chat--title))
                (should (equal (hermes-session-title-chat-display hermes-chat--title)
                               "renamed"))
                (should hermes-chat--title-manual-p)
                (should (equal before (buffer-string)))
                (should (= position (point)))
                (should (equal undo buffer-undo-list))
                (should (eq owner (current-buffer))))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-rename-prompt-preserves-reader-error ()
  "An unrelated reader failure is not laundered into stale-owner refusal."
  (hermes-test-with-chat-buffer
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (signal 'file-error '("Reader failure")))))
      (should (equal (should-error (call-interactively #'hermes-chat-rename)
                                   :type 'file-error)
                     '(file-error "Reader failure"))))))

(ert-deftest hermes-chat-rename-prompt-cancel-preserves-editor ()
  "Cancelling a real rename preserves the title, draft, undo, point and focus."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (buffer-enable-undo)
      (insert "Exact draft")
      (undo-boundary)
      (setq hermes-chat--title "Original")
      (let ((before (buffer-string)) (position (point))
            (undo (copy-tree buffer-undo-list)) (owner (current-buffer))
            (noninteractive nil) sent)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-session-title)
                   (lambda (&rest _) (setq sent t))))
          (unwind-protect
              (progn
                (condition-case nil
                    (execute-kbd-macro (kbd "C-c C-o S R C-g"))
                  (quit nil))
                (should-not sent)
                (should (equal hermes-chat--title "Original"))
                (should-not hermes-chat--title-manual-p)
                (should (equal before (buffer-string)))
                (should (= position (point)))
                (should (equal undo buffer-undo-list))
                (should (eq owner (current-buffer)))
                (should (eq owner (window-buffer (selected-window)))))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-rename-preserves-canonical-session-timestamp ()
  "Renaming an identified session changes its label, not its timestamp."
  (hermes-test-with-chat-buffer
   (let ((project-name (buffer-name)))
     (setq hermes-chat--title "Old--20260102T030405.123456Z--emacs")
     (hermes-chat-rename "New")
     (should (equal hermes-chat--title
                    "New--20260102T030405.123456Z--emacs"))
     (should (equal (buffer-name) project-name)))))

(ert-deftest hermes-chat-buffer-name-formats-project-identity ()
  "Buffer names carry profile and gateway directory, never session title."
  (should (equal (hermes-chat--buffer-name
                  "coder" nil "/tmp/emacs-hermes/")
                 "*Hermes@coder: [emacs-hermes]*"))
  (should (equal (hermes-chat--buffer-name
                  nil nil "C:\\Users\\Thanos\\project\\")
                 "*Hermes@default: [project]*")))

(ert-deftest hermes-chat-buffer-name-identifies-instance-when-multiple ()
  "Named-instance chat buffers use the instance before the profile."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote)))
    (should (equal (hermes-chat--buffer-name
                    nil local "/tmp/emacs-hermes/")
                   "*local@default: [emacs-hermes]*"))
    (should (equal
             (hermes-chat--buffer-name
              "coder" remote "/tmp/emacs-hermes/")
             "*remote@coder: [emacs-hermes]*"))))

(ert-deftest hermes-chat-buffer-name-uses-named-single-instance ()
  "A named single instance remains part of the project identity."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (hermes-instances (list local)))
    (should (equal (hermes-chat--buffer-name
                    "coder" local "/tmp/emacs-hermes/")
                   "*local@coder: [emacs-hermes]*"))))

(ert-deftest hermes-chat-create-uses-project-canonical-title ()
  "Fresh dashboard sessions use a canonical title from the current project."
  (let ((client (hermes-test--dashboard-client)) created-title)
    (cl-letf (((symbol-function 'current-time)
               (lambda () (encode-time 45 30 18 7 8 2026 t)))
              ((symbol-function 'project-current) (lambda (&rest _) 'project))
              ((symbol-function 'project-root)
               (lambda (_project) "/tmp/emacs-hermes/"))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq created-title (plist-get args :title))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) nil)))
      (hermes-test-with-chat-buffer
       (insert "hello")
       (hermes-chat-send)
       (should (equal created-title
                      "emacs-hermes--20260807T183045.000000Z--emacs"))))))

(ert-deftest hermes-chat-prompts-profile-and-names-buffer ()
  "M-x hermes-chat reads a profile and names the buffer after it."
  (cl-letf (((symbol-function 'hermes-chat--read-profile)
             (lambda () "coder")))
    (let ((buffer (call-interactively #'hermes-chat)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal hermes-chat--profile "coder"))
            (should (string-prefix-p "*Hermes@coder" (buffer-name))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-uses-current-default-directory ()
  "A new chat keeps the directory of the buffer that launched it."
  (let ((origin (generate-new-buffer " *hermes-chat-origin*")) chat)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer origin
            (setq default-directory
                  (file-name-as-directory temporary-file-directory))
            (setq chat (hermes-chat nil)))
          (with-current-buffer chat
            (should (equal default-directory
                           (file-name-as-directory temporary-file-directory)))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (kill-buffer origin))))

(ert-deftest hermes-chat-configured-instance-uses-resolved-filesystem-mode ()
  "A named instance derives filesystem ownership from its transport target."
  (dolist (spec '(("http://127.0.0.1:9119" auto t)
                  ("http://127.0.0.1:9119" spawn t)
                  ("http://127.0.0.1:9119" remote nil)
                  ("https://hermes.example.test" auto nil)))
    (let* ((instance (cons "named" (nth 0 spec)))
           (hermes-instances (list instance))
           (hermes-dashboard-transport-start-mode (nth 1 spec))
           (launch-directory (file-name-as-directory temporary-file-directory))
           buffer)
      (unwind-protect
          (let ((default-directory launch-directory))
            (setq buffer (hermes-chat nil instance))
            (with-current-buffer buffer
              (should (eq hermes-chat--resolved-start-mode
                          (if (nth 2 spec) 'spawn 'remote)))
              (should (equal default-directory launch-directory))
              (if (nth 2 spec)
                  (should (equal (hermes-chat--current-working-directory)
                                 launch-directory))
                (should-not (hermes-chat--current-working-directory)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-configured-instance-pins-resolved-start-mode ()
  "A named chat keeps one transport mode for cwd locality and acquisition."
  (dolist (spec '((auto remote spawn t)
                  (remote spawn remote nil)))
    (let* ((instance '(:id "named" :name "Named"
                       :url "http://127.0.0.1:9119"))
           (hermes-instances (list instance))
           (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
           (launch-directory (file-name-as-directory temporary-file-directory))
           (client (hermes-test--dashboard-client))
           buffer acquired-mode)
      (unwind-protect
          (progn
            (set-default 'hermes-dashboard-transport-start-mode (nth 0 spec))
            (let ((default-directory launch-directory))
              (setq buffer (hermes-chat nil instance)))
            (set-default 'hermes-dashboard-transport-start-mode (nth 1 spec))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest args)
                           (setq acquired-mode (plist-get args :start-mode))
                           client)))
                (hermes-chat--dashboard-ensure-client))
              (should (eq acquired-mode (nth 2 spec)))
              (should (eq hermes-chat--resolved-start-mode (nth 2 spec)))
              (should-not
               (local-variable-p 'hermes-dashboard-transport-start-mode))
              (if (nth 3 spec)
                  (should (equal (hermes-chat--current-working-directory)
                                 launch-directory))
                (should-not (hermes-chat--current-working-directory)))))
        (set-default 'hermes-dashboard-transport-start-mode saved-mode)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-legacy-buffer-adopts-attached-client-start-mode ()
  "A live legacy client wins over changed configuration before reacquisition."
  (dolist (spec '((remote "https://hermes.example.test" spawn
                          ("named" . "http://127.0.0.1:9119"))
                  (spawn (spawn "127.0.0.1" 9119) remote
                         ("named" . "https://hermes.example.test"))))
    (let* ((expected (nth 0 spec))
           (client (make-hermes-dashboard-transport-client
                    :websocket 'attached-websocket
                    :endpoint-key (nth 1 spec)))
           (replacement (hermes-test--dashboard-client))
           (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
           (buffer (generate-new-buffer " *hermes-legacy-mode*"))
           acquired-mode)
      (unwind-protect
          (progn
            (set-default 'hermes-dashboard-transport-start-mode (nth 2 spec))
            (with-current-buffer buffer
              (hermes-chat-mode)
              (setq hermes-instance (nth 3 spec)
                    hermes-chat--dashboard-client client)
              (kill-local-variable 'hermes-dashboard-transport-start-mode)
              (should (eq (hermes-chat--dashboard-ensure-client) client))
              (should (eq hermes-chat--resolved-start-mode expected))
              (setq hermes-chat--dashboard-client nil)
              (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest args)
                           (setq acquired-mode (plist-get args :start-mode))
                           replacement)))
                (hermes-chat--dashboard-ensure-client))
              (should (eq acquired-mode expected))))
        (set-default 'hermes-dashboard-transport-start-mode saved-mode)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-legacy-buffer-adopts-old-local-start-mode ()
  "A legacy concrete pin wins over changed configuration during acquisition."
  (dolist (spec '((spawn remote) (remote spawn)))
    (let ((saved-mode (default-value 'hermes-dashboard-transport-start-mode))
          (client (hermes-test--dashboard-client))
          (buffer (generate-new-buffer " *hermes-legacy-local-mode*"))
          acquired-mode)
      (unwind-protect
          (progn
            (set-default 'hermes-dashboard-transport-start-mode (nth 1 spec))
            (with-current-buffer buffer
              (hermes-chat-mode)
              (setq hermes-instance
                    '("named" . "http://127.0.0.1:9119"))
              (setq-local hermes-dashboard-transport-start-mode (nth 0 spec))
              (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest args)
                           (setq acquired-mode (plist-get args :start-mode))
                           client)))
                (hermes-chat--dashboard-ensure-client))
              (should (eq hermes-chat--resolved-start-mode (nth 0 spec)))
              (should (eq acquired-mode (nth 0 spec)))))
        (set-default 'hermes-dashboard-transport-start-mode saved-mode)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-legacy-buffer-resolves-instance-once-for-acquisition ()
  "One selected instance supplies both fallback mode and acquisition URL."
  (let* ((remote '(:id "remote" :name "Remote"
                   :url "https://hermes.example.test"))
         (local '("local" . "http://127.0.0.1:9119"))
         (choices (list remote local))
         (client (hermes-test--dashboard-client))
         (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
         (buffer (generate-new-buffer " *hermes-legacy-instance*"))
         prompts acquired-mode acquired-url)
    (unwind-protect
        (progn
          (set-default 'hermes-dashboard-transport-start-mode 'auto)
          (with-current-buffer buffer
            (hermes-chat-mode)
            (kill-local-variable 'hermes-dashboard-transport-start-mode)
            (cl-letf (((symbol-function 'hermes-instance-resolve)
                       (lambda ()
                         (setq prompts (1+ (or prompts 0)))
                         (pop choices)))
                      ((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest args)
                         (setq acquired-mode (plist-get args :start-mode)
                               acquired-url hermes-dashboard-transport-url)
                         client)))
              (hermes-chat--dashboard-ensure-client))
            (should (= prompts 1))
            (should (equal hermes-instance remote))
            (should (eq acquired-mode 'remote))
            (should (equal acquired-url "https://hermes.example.test"))))
      (set-default 'hermes-dashboard-transport-start-mode saved-mode)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-lifetime-reset-adopts-attached-client-start-mode ()
  "Disconnect and reset preserve an unpinned attached client's endpoint mode."
  (dolist (action '(disconnect reset))
    (dolist (spec '((remote "https://hermes.example.test" spawn
                            ("named" . "https://hermes.example.test"))
                    (spawn (spawn "127.0.0.1" 9119) remote
                           ("named" . "http://127.0.0.1:9119"))))
      (let* ((expected (nth 0 spec))
             (client (make-hermes-dashboard-transport-client
                      :websocket 'attached-websocket
                      :endpoint-key (nth 1 spec)))
             (replacement (hermes-test--dashboard-client))
             (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
             (buffer (generate-new-buffer " *hermes-lifetime-mode*"))
             acquired-mode)
        (unwind-protect
            (progn
              (set-default 'hermes-dashboard-transport-start-mode (nth 2 spec))
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance (nth 3 spec)
                      hermes-chat--dashboard-client client
                      hermes-chat--dashboard-active-session-id "sid")
                (kill-local-variable 'hermes-dashboard-transport-start-mode)
                (let ((hermes-chat-cleanup-functions nil))
                  (pcase action
                    ('disconnect (hermes-chat-disconnect))
                    ('reset (hermes-chat--reset-transcript))))
                (should (eq hermes-chat--resolved-start-mode expected))
                (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                           (lambda (&rest args)
                             (setq acquired-mode (plist-get args :start-mode))
                             replacement)))
                  (hermes-chat--dashboard-ensure-client))
                (should (eq acquired-mode expected))))
          (set-default 'hermes-dashboard-transport-start-mode saved-mode)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-new-instance-does-not-inherit-source-start-mode ()
  "A new chat resolves its instance without inheriting another chat's mode."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
         (launch-directory (file-name-as-directory temporary-file-directory))
         local-buffer remote-buffer)
    (unwind-protect
        (progn
          (set-default 'hermes-dashboard-transport-start-mode 'auto)
          (let ((default-directory launch-directory))
            (setq local-buffer (hermes-chat nil local)))
          (with-current-buffer local-buffer
            (setq remote-buffer (hermes-chat nil remote)))
          (with-current-buffer remote-buffer
            (should (eq hermes-chat--resolved-start-mode 'remote))
            (should-not
             (local-variable-p 'hermes-dashboard-transport-start-mode))
            (should-not (hermes-chat--current-working-directory))))
      (set-default 'hermes-dashboard-transport-start-mode saved-mode)
      (when (buffer-live-p local-buffer) (kill-buffer local-buffer))
      (when (buffer-live-p remote-buffer) (kill-buffer remote-buffer)))))

(ert-deftest hermes-chat-legacy-instance-uses-resolved-filesystem-mode ()
  "The unnamed singleton seeds cwd only when its resolved mode is spawn."
  (dolist (spec '(("http://127.0.0.1:9119" auto t)
                  ("http://127.0.0.1:9119" spawn t)
                  ("http://127.0.0.1:9119" remote nil)
                  ("https://hermes.example.test" auto nil)))
    (let ((hermes-instances nil)
          (hermes-dashboard-transport-url (nth 0 spec))
          (hermes-dashboard-transport-start-mode (nth 1 spec))
          (launch-directory (file-name-as-directory temporary-file-directory))
          buffer)
      (unwind-protect
          (let ((default-directory launch-directory))
            (setq buffer (hermes-chat nil))
            (with-current-buffer buffer
              (should (eq hermes-chat--resolved-start-mode
                          (if (nth 2 spec) 'spawn 'remote)))
              (should (equal default-directory launch-directory))
              (if (nth 2 spec)
                  (should (equal (hermes-chat--current-working-directory)
                                 launch-directory))
                (should-not (hermes-chat--current-working-directory))
                (should (string-match-p
                         (format "\\[%s\\]"
                                 (regexp-quote
                                  (hermes-chat--directory-basename
                                   launch-directory)))
                         (buffer-name)))
                (should (string-match-p
                         "detached" (hermes-test--header-line-string))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-remote-resume-hydrates-only-gateway-cwd ()
  "A remote resume uses editor identity while gateway execution stays detached."
  (dolist (outcome '(resolve reject))
    (let* ((instance '("remote" . "https://hermes.example.test"))
           (hermes-instances (list instance))
           (launch-directory (file-name-as-directory temporary-file-directory))
           resolve reject buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) (hermes-test--dashboard-client)))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _session-id &rest args)
                   (setq resolve (plist-get args :resolve)
                         reject (plist-get args :reject)))))
        (unwind-protect
            (let ((default-directory launch-directory))
              (setq buffer
                    (hermes-chat-resume-session "stored" nil nil instance))
              (with-current-buffer buffer
                (should-not hermes-chat--working-directory)
                (should (string-match-p
                         (format "\\[%s\\]"
                                 (regexp-quote
                                  (hermes-chat--directory-basename
                                   launch-directory)))
                         (buffer-name)))
                (should (string-match-p "detached"
                                        (hermes-test--header-line-string)))
                (pcase outcome
                  ('resolve
                   (funcall resolve
                            '((session_id . "live")
                              (info . ((cwd . "/srv/repo")))))
                   (should (equal hermes-chat--working-directory "/srv/repo"))
                   (should (string-match-p "\\[repo\\]" (buffer-name))))
                  ('reject
                   (funcall reject "resume failed")
                   (should (string-match-p
                            (format "\\[%s\\]"
                                    (regexp-quote
                                     (hermes-chat--directory-basename
                                      launch-directory)))
                            (buffer-name)))
                   (should-not hermes-chat--working-directory)))
                (should (equal default-directory launch-directory))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-resume-is-explicitly-unowned-by-launch-project ()
  "A resumed chat cannot be mistaken for a legacy project chat."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-resume-project-" t)))
         (instance '("remote" . "https://hermes.example.test"))
         (hermes-instances (list instance))
         resolve buffer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (hermes-test--dashboard-client)))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resolve (plist-get args :resolve)))))
      (unwind-protect
          (let ((default-directory root))
            (setq buffer
                  (hermes-chat-resume-session "stored" nil nil instance))
            (with-current-buffer buffer
              (should (local-variable-p 'hermes-chat--launch-project-root))
              (should-not hermes-chat--launch-project-root)
              (funcall resolve
                       '((session_id . "live")
                         (info . ((cwd . "/srv/resumed")))))
              (should (string-match-p "\\[resumed\\]" (buffer-name)))
              (should-not
               (hermes-chat--project-buffers root (list buffer)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))
    (delete-directory root t)))

(ert-deftest hermes-chat-fresh-override-failure-settles-prompt ()
  "A post-attachment override failure settles the creating prompt exactly once."
  (let ((client (hermes-test--dashboard-client)) config-reject prompt-submits)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved '((cwd . "/srv/default")))))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-reject (plist-get args :reject))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (setq prompt-submits (1+ (or prompt-submits 0)))))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq hermes-chat--dashboard-create-fast-p t)
         (insert "hello")
         (hermes-chat-send)
         (should (bound-and-true-p hermes-chat--session-bootstrap))
         (should hermes-chat--pending-assistant-id)
         (should hermes-chat--unsettled-submit-context)
         (funcall config-reject "config boom")
         (should-not prompt-submits)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--unsettled-submit-context)
         (should-not hermes-chat--dashboard-running-p)
         (should-not (bound-and-true-p hermes-chat--session-bootstrap))
         (should (string-match-p "Pre-session override failed: config boom"
                                 (buffer-string))))))))

(ert-deftest hermes-chat-sync-create-failure-retains-queued-entry ()
  "A queued origin remains queued when fresh session creation signals."
  (let ((client (hermes-test--dashboard-client)) api-calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq api-calls (1+ (or api-calls 0)))
                 (hermes--promise-resolved '((cwd . "/srv/default")))))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (&rest _) (error "sync create boom"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq hermes-chat--working-directory nil)
         (hermes-chat--queue-content "queued")
         (let ((entry (car hermes-chat--queued-messages)))
           (setq hermes-chat--queued-submit-id (plist-get entry :id))
           (hermes-chat--submit-content "queued" nil entry))
         (should (= api-calls 1))
         (should (equal (hermes-test--queued-contents) '("queued")))
         (should-not hermes-chat--queued-submit-id)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--unsettled-submit-context)
         (should-not (bound-and-true-p hermes-chat--session-bootstrap))
         (should (string-match-p "Queued message retained: sync create boom"
                                 (buffer-string))))))))

(ert-deftest hermes-chat-selects-instance-before-profile ()
  "Interactive chat selection pins the instance used to read its profile."
  (let ((instance '("remote" . "https://hermes.example.test"))
        profile-instance)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-chat--read-profile)
               (lambda ()
                 (setq profile-instance hermes-instance)
                 "coder")))
      (let ((buffer (call-interactively #'hermes-chat)))
        (unwind-protect
            (with-current-buffer buffer
              (should (equal profile-instance instance))
              (should (equal hermes-instance instance))
              (should (equal hermes-chat--profile "coder")))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-blank-profile-names-buffer-default ()
  "A blank profile yields the default profile name and no stored profile."
  (let ((buffer (hermes-chat "")))
    (unwind-protect
        (with-current-buffer buffer
          (should-not hermes-chat--profile)
          (should (string-prefix-p "*Hermes@default" (buffer-name))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-should-apply-title-p-rules ()
  "A fetched title applies only when non-empty, changed, and not manual."
  (should (hermes-chat--should-apply-title-p "New" "Old" nil))
  (should-not (hermes-chat--should-apply-title-p "New" "Old" t))
  (should-not (hermes-chat--should-apply-title-p "" "Old" nil))
  (should-not (hermes-chat--should-apply-title-p "Same" "Same" nil))
  (should-not (hermes-chat--should-apply-title-p nil "Old" nil)))

(ert-deftest hermes-chat-done-refreshes-session-title ()
  "A completed turn fetches title metadata without renaming or pushing."
  (let ((client (hermes-test--dashboard-client))
        callback (pushes 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _a) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_c _t &rest _a) nil))
              ((symbol-function 'hermes-dashboard-transport-session-title-fetch)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve) '((title . "Auto Title")))))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (&rest _a) (setq pushes (1+ pushes)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (let ((project-name (buffer-name)))
           (insert "hi")
           (hermes-chat-send)
           (funcall callback '(:type done))
           ;; The title fetch is deferred off the event handler; let it run.
           (sit-for 0.05)
           (should (equal hermes-chat--title "Auto Title"))
           (should (equal (buffer-name) project-name))
           (should (= pushes 0))))))))

(ert-deftest hermes-chat-manual-title-survives-refresh ()
  "A manually set title is not overwritten by the automatic refresh."
  (let ((fetches 0))
    (cl-letf (((symbol-function 'current-time)
               (lambda () (encode-time 45 30 18 7 8 2026 t)))
              ((symbol-function 'hermes-chat--dashboard-session-attached-p)
               (lambda () t))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (&rest _a) nil))
              ((symbol-function 'hermes-dashboard-transport-session-title-fetch)
               (lambda (&rest _a) (setq fetches (1+ fetches)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-active-session-id "sid")
       (hermes-chat-rename "Pinned")
       (should hermes-chat--title-manual-p)
       (hermes-chat--maybe-refresh-session-title)
       (should (= fetches 0))
       (should (equal hermes-chat--title
                      "Pinned--20260807T183045.000000Z--emacs"))))))

(ert-deftest hermes-chat-snapshot-prefers-title ()
  "The dashboard snapshot uses the chat title over the buffer name."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--title "Pinned--20260807T183045.123456Z--emacs")
   (should (equal (plist-get (hermes-chat--dashboard-snapshot) :title)
                  "Pinned"))))

(ert-deftest hermes-chat-switch-offers-and-selects-live-buffer ()
  "The switcher lists live chat buffers and switches to the chosen one."
  (hermes-test-with-chat-buffer
   (let ((target (current-buffer)))
     (should (memq target (hermes-chat--live-buffers)))
     (with-temp-buffer
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) (buffer-name target))))
         (call-interactively #'hermes-switch-to-chat))
       (should (eq (current-buffer) target))))))

(ert-deftest hermes-chat-project-root-prefers-project-and-falls-back-to-directory ()
  "Project identity uses the project root or normalized directory fallback."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-root-" t)))
         (nested (expand-file-name "src/lib/" root))
         (outside (file-name-as-directory
                   (make-temp-file "hermes-project-fallback-" t))))
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (should (equal (file-truename (hermes-chat--project-root nested))
                         (file-truename root)))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
            (should (equal (hermes-chat--project-root outside) outside))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest hermes-project-chat-switches-single-exact-root-match ()
  "Project chat switches directly to the sole exact-root chat."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-chat-" t)))
         (other-root (file-name-as-directory
                      (make-temp-file "hermes-project-other-" t)))
         (nested (expand-file-name "src/" root))
         (target (generate-new-buffer " *hermes-project-target*"))
         (other (generate-new-buffer " *hermes-project-other*"))
         shown)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (dolist (entry `((,target . ,root) (,other . ,other-root)))
            (with-current-buffer (car entry)
              (setq default-directory (cdr entry))
              (hermes-chat-mode)))
          (with-temp-buffer
            (setq default-directory nested)
            (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                       (lambda (buffer &rest _) (setq shown buffer))))
              (hermes-project-chat)))
          (should (eq shown target)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list target other))
      (delete-directory root t)
      (delete-directory other-root t))))

(ert-deftest hermes-project-chat-completes-among-same-root-siblings ()
  "Multiple project chats offer only exact-root siblings."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-siblings-" t)))
         (other-root (file-name-as-directory
                      (make-temp-file "hermes-project-excluded-" t)))
         (first (generate-new-buffer " *hermes-project-first*"))
         (second (generate-new-buffer " *hermes-project-second*"))
         (other (generate-new-buffer " *hermes-project-excluded*"))
         offered shown)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (dolist (entry `((,first . ,root) (,second . ,root)
                           (,other . ,other-root)))
            (with-current-buffer (car entry)
              (setq default-directory (cdr entry))
              (hermes-chat-mode)))
          (with-temp-buffer
            (setq default-directory root)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt candidates &rest _)
                         (setq offered candidates)
                         (buffer-name second)))
                      ((symbol-function 'pop-to-buffer-same-window)
                       (lambda (buffer &rest _) (setq shown buffer))))
              (hermes-project-chat)))
          (should (equal (sort (copy-sequence offered) #'string<)
                         (sort (mapcar #'buffer-name (list first second))
                               #'string<)))
          (should (eq shown second)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list first second other))
      (delete-directory root t)
      (delete-directory other-root t))))

(ert-deftest hermes-project-chat-adopts-legacy-buffer-with-cwd-name ()
  "Project switching adopts a legacy chat without masking its known cwd."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-adopt-" t)))
         (target (generate-new-buffer "*local@default: [emacs-hermes]*"))
         shown)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (with-current-buffer target
            (setq default-directory root)
            (hermes-chat-mode)
            (setq hermes-chat--resolved-start-mode 'remote
                  hermes-chat--working-directory "/srv/emacs-hermes"))
          (with-temp-buffer
            (setq default-directory root)
            (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                       (lambda (buffer &rest _) (setq shown buffer))))
              (hermes-project-chat)))
          (should (eq shown target))
          (with-current-buffer target
            (should (equal (file-truename hermes-chat--launch-project-root)
                           (file-truename root)))
            (should (string-match-p "\\[emacs-hermes\\]" (buffer-name)))))
      (when (buffer-live-p target) (kill-buffer target))
      (delete-directory root t))))

(ert-deftest hermes-chat-direct-new-does-not-inherit-project-identity ()
  "A direct new chat from a project chat remains cwd-named and unowned."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-direct-from-project-" t)))
         (instance '("local" . "http://127.0.0.1:9119"))
         (hermes-instances (list instance))
         (source (generate-new-buffer " *hermes-project-source*"))
         direct-a direct-b)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer source
            (setq default-directory root)
            (hermes-chat-mode)
            (setq hermes-instance instance
                  hermes-chat--launch-project-root root
                  hermes-chat--resolved-start-mode 'remote
                  hermes-chat--working-directory "/srv/source")
            (setq direct-a (hermes-chat nil instance)
                  direct-b (hermes-chat nil instance)))
          (dolist (buffer (list direct-a direct-b))
            (with-current-buffer buffer
              (hermes-chat--record-working-directory "/srv/second")
              (should-not hermes-chat--launch-project-root)
              (should (string-match-p "\\[second\\]" (buffer-name)))))
          (should-not (memq direct-a
                            (hermes-chat--project-buffers
                             root (list direct-a direct-b))))
          (should (string-suffix-p "<2>" (buffer-name direct-b))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list source direct-a direct-b))
      (delete-directory root t))))

(ert-deftest hermes-chat-buffer-name-function-is-customizable ()
  "A custom naming function owns the complete chat buffer name."
  (let ((hermes-chat-buffer-name-function
         (lambda (profile instance directory)
           (format "*Custom: %s/%s/%s*"
                   (hermes-instance-name instance) profile
                   (file-name-nondirectory (directory-file-name directory))))))
    (dolist (instance '(("local" . "http://127.0.0.1:9119")
                        (:id "local-id" :name "local"
                         :url "http://127.0.0.1:9119")))
      (should (equal (hermes-chat--buffer-name
                      "coder" instance "/tmp/nema/")
                     "*Custom: local/coder/nema*")))))

(ert-deftest hermes-chat-buffer-name-function-receives-display-directory ()
  "A custom name receives explicit cwd, gateway cwd, then local fallbacks."
  (let* (captured
         (hermes-chat-buffer-name-function
          (lambda (_profile _instance directory)
            (setq captured directory)
            "*Captured Hermes*")))
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--launch-project-root "/tmp/launch-project/"
           hermes-chat--working-directory nil)
     (hermes-chat--refresh-buffer-name)
     (should (equal captured "/tmp/launch-project/"))
     (hermes-chat--record-working-directory "/srv/project")
     (should (equal captured "/srv/project"))
     (hermes-chat--buffer-name nil nil "/srv/explicit")
     (should (equal captured "/srv/explicit"))
     (setq hermes-chat--working-directory nil
           hermes-chat--launch-project-root nil)
     (hermes-chat--refresh-buffer-name)
     (should (equal captured "/tmp/local-editor/")))))

(ert-deftest hermes-project-chat-buffer-name-follows-working-directory ()
  "A project chat name follows cwd without changing its launch association."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-name-" t)))
         (nested (expand-file-name "src/" root))
         (instance '("local" . "http://127.0.0.1:9119"))
         (hermes-instances (list instance))
         buffer)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (save-window-excursion
            (with-temp-buffer
              (setq default-directory nested)
              (cl-letf (((symbol-function 'hermes-chat--read-profile)
                         (lambda () nil)))
                (setq buffer (hermes-project-chat t)))))
          (with-current-buffer buffer
            (hermes-chat--record-working-directory
             "/srv/emacs-hermes")
            (should (equal (file-truename hermes-chat--launch-project-root)
                           (file-truename root)))
            (should (string-match-p "\\[emacs-hermes\\]" (buffer-name)))
            (should (string-match-p "emacs-hermes"
                                    (hermes-test--header-line-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest hermes-project-chat-routes-from-adopted-project-buffer ()
  "A project chat routes from its launch root after explicit cwd adoption."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-route-" t)))
         (gateway (file-name-as-directory
                   (make-temp-file "hermes-gateway-route-" t)))
         (buffer (generate-new-buffer " *Hermes project route*"))
         selected normal-created prefix-created prefix-directory prefix-phase)
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory root)
          (hermes-chat-mode)
          (setq hermes-chat--launch-project-root root
                hermes-chat--resolved-start-mode 'remote)
          (hermes-chat--apply-directory gateway)
          (cl-letf (((symbol-function 'hermes-chat--live-buffers)
                     (lambda () (list buffer)))
                    ((symbol-function 'pop-to-buffer-same-window)
                     (lambda (target &rest _) (setq selected target)))
                    ((symbol-function 'call-interactively)
                     (lambda (command &rest _)
                       (if prefix-phase
                           (setq prefix-created command
                                 prefix-directory default-directory)
                         (setq normal-created command)))))
            (hermes-project-chat)
            (should (eq selected buffer))
            (should-not normal-created)
            (setq prefix-phase t)
            (hermes-project-chat t)
            (should (eq prefix-created #'hermes-chat))
            (should (equal (file-truename prefix-directory)
                           (file-truename root)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)
      (delete-directory gateway t))))

(ert-deftest hermes-project-chat-prefix-creates-at-project-root ()
  "Prefix always creates a sibling chat rooted at the current project."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-new-" t)))
         (nested (expand-file-name "src/" root))
         called directory)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (with-temp-buffer
            (setq default-directory nested)
            (cl-letf (((symbol-function 'call-interactively)
                       (lambda (command &rest _)
                         (setq called command
                               directory default-directory))))
              (hermes-project-chat t)))
          (should (eq called #'hermes-chat))
          (should (equal (file-truename directory) (file-truename root))))
      (delete-directory root t))))

(ert-deftest hermes-chat-actions-popup-bound ()
  "Chat actions use shallow native menus with two columns per row."
  (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-o")
              #'hermes-chat-actions-map-popup))
  (let ((rows (keymap-popup--meta hermes-chat-actions-map 'descriptions)))
    (should (equal (mapcar (lambda (row)
                            (mapcar (lambda (group) (plist-get group :name)) row))
                          rows)
                   '(("Turn" "Compose") ("Configure" "Browse") ("Prompt"))))))

(ert-deftest hermes-chat-actions-popup-paths ()
  "Actual popup wrappers dispatch every advertised path in the owner buffer."
  (hermes-test-with-chat-buffer
    (let ((owner (current-buffer)))
      (dolist (path '(("RET" hermes-chat-send)
                      ("I f" hermes-chat-attach-image-file)
                      ("I v" hermes-chat-paste-image)
                      ("B P" hermes-chat-queue-panel)
                      ("B W" hermes-chat-work)
                      ("B T" hermes-chat-show-todos)
                      ("B o" hermes-chat-preview-output)
                      ("s" hermes-chat-steer-message)
                      ("i" hermes-chat-interrupt)
                      ("k" hermes-chat-interrupt-and-send)
                      ("q" hermes-chat-queue-message)
                      ("a" hermes-chat-respond-to-prompt)
                      ("d" hermes-chat-cancel-prompt)
                      ("j" hermes-chat-go-to-composer)
                      ("f" hermes-chat-attach-image-file)
                      ("v" hermes-chat-paste-image)
                      ("I V" hermes-chat-preview-images)
                      ("I D" hermes-chat-remove-image)
                      ("c" hermes-chat-show-commands)
                      ("r" hermes-chat-refresh-commands)
                      ("S n" hermes-chat)
                      ("S R" hermes-chat-rename)
                      ("S H" hermes-chat-handoff)
                      ("S S" hermes-list-sessions)
                      ("M m" hermes-chat-switch-model)
                      ("M e" hermes-chat-set-reasoning)
                      ("M K" hermes-chat-connect-provider)
                      ("w w" hermes-chat-set-directory)
                      ("w b" hermes-switch-to-chat)
                      ("w P" hermes-chat-queue-panel)
                      ("W" hermes-chat-work)
                      ("B W" hermes-chat-work)
                      ("X W" hermes-chat-work)
                      ("X h" hermes-chat-session-details)
                      ("X x" hermes-dashboard-reconnect)
                      ("X u" hermes-chat-show-usage)
                      ("X t" hermes-chat-show-status)))
        (let (called)
          (cl-letf (((symbol-function (cadr path))
                     (lambda () (interactive) (setq called (current-buffer))))
                    ((symbol-function 'hermes-chat--interrupt-unavailable-p) #'ignore)
                    ((symbol-function 'hermes-chat--interrupt-send-unavailable-p) #'ignore)
                    ((symbol-function 'hermes-chat--pending-prompt-p) (lambda () t)))
            (unwind-protect
                (save-window-excursion
                  (switch-to-buffer owner)
                  (execute-kbd-macro (kbd (concat "C-c C-o " (car path))))
                  (should (eq called owner)))
              (keymap-popup-dismiss))))))))

(ert-deftest hermes-chat-actions-popup-shortcut-aliases ()
  "Unclaimed old suffix keys remain hidden aliases, not a second menu."
  (dolist (map (list hermes-chat-images-map hermes-chat-sess-map
                     hermes-chat-model-map hermes-chat-work-map
                     hermes-chat-info-map hermes-chat-jobs-map))
    (map-keymap
     (lambda (event binding)
       (when (and (commandp binding)
                  (not (eq binding #'hermes-chat--submenu-root-key))
                  (not (memq event '(?? ?S ?w ?B))))
         (should (eq (lookup-key hermes-chat-actions-map (vector event))
                     binding))))
     map)))

(ert-deftest hermes-chat-actions-popup-back-and-cancel ()
  "Native q/C-g back navigation and dismissal preserve the chat draft."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (insert "Unsent draft")
      (let ((owner (current-buffer)) (before (buffer-string)))
        (unwind-protect
            (progn
              (hermes-chat-actions-map-popup)
              (execute-kbd-macro (kbd "S q"))
              (let ((popup (get-buffer keymap-popup--buffer-name)))
                (should popup)
                (with-current-buffer popup
                   (should (string-match-p "Compose" (buffer-string)))))
              (execute-kbd-macro (kbd "M C-g"))
              (should (get-buffer keymap-popup--buffer-name))
              (execute-kbd-macro (kbd "C-g"))
              (should-not (get-buffer keymap-popup--buffer-name))
              (should (eq (current-buffer) owner))
              (should (equal (buffer-string) before)))
          (keymap-popup-dismiss))))))

(ert-deftest hermes-chat-actions-popup-directory-guard ()
  "The nested directory action refuses busy turns in the actual wrapper."
  (hermes-test-with-chat-buffer
    (let (called)
      (cl-letf (((symbol-function 'hermes-chat--active-turn-p) (lambda () t))
                ((symbol-function 'hermes-chat-set-directory)
                 (lambda () (interactive) (setq called t))))
        (unwind-protect
            (save-window-excursion
              (hermes-chat-actions-map-popup)
              (call-interactively (key-binding (kbd "w")))
              (call-interactively (key-binding (kbd "w")))
              (should-not called)
              (with-current-buffer keymap-popup--buffer-name
                (should (string-match-p "Directory:" (buffer-string)))))
          (keymap-popup-dismiss))))))

(ert-deftest hermes-chat-actions-popup-minibuffer-owner ()
  "A nested prompting suffix reads text without stealing the draft owner."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (insert "Keep this draft")
      (let ((owner (current-buffer)) (before (buffer-string)) answer called)
        (cl-letf (((symbol-function 'hermes-chat-rename)
                   (lambda (name)
                     (interactive (list (read-string "Session name: ")))
                     (setq answer name called (current-buffer)))))
          (unwind-protect
              (let ((noninteractive nil))
                (hermes-chat-actions-map-popup)
                (execute-kbd-macro (kbd "S R n e w SPC n a m e RET"))
                (should (equal answer "new name"))
                (should (eq called owner))
                (should (equal (buffer-string) before))
                (should-not (get-buffer keymap-popup--buffer-name)))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-actions-popup-real-setting-cancel-two-owners ()
  "Cancel native completion without changing either chat's settings or draft."
  (hermes-test-with-chat-buffer
   (let ((first (current-buffer)))
     (setq hermes-chat--model "first-model"
           hermes-chat--runtime-flags '(:reasoning-effort "low"))
     (hermes-test-with-chat-buffer
      (setq hermes-chat--model "second-model"
            hermes-chat--runtime-flags '(:reasoning-effort "high"))
      (let ((second (current-buffer)))
        (dolist (owner (list first second))
          (save-window-excursion
            (switch-to-buffer owner)
            (buffer-enable-undo)
            (insert "Keep this draft")
            (undo-boundary)
            (let ((before (buffer-string)) (position (point))
                  (undo (copy-tree buffer-undo-list))
                  (flags (copy-tree hermes-chat--runtime-flags)) entered
                  (noninteractive nil))
              (unwind-protect
                  (progn
                    (minibuffer-with-setup-hook
                        (lambda () (setq entered (minibufferp)))
                      (condition-case nil
                          (execute-kbd-macro (kbd "C-c C-o M e C-g"))
                        (quit nil)))
                    (should entered)
                    (should-not (active-minibuffer-window))
                    (should (equal flags hermes-chat--runtime-flags))
                    (should-not hermes-chat--dashboard-create-reasoning-effort)
                    (should (equal before (buffer-string)))
                    (should (= position (point)))
                    (should (equal undo buffer-undo-list))
                    (should (eq owner (current-buffer)))
                    (should-not (get-buffer keymap-popup--buffer-name))
                    (execute-kbd-macro "x")
                    (should (equal (buffer-string) (concat before "x"))))
                (keymap-popup-dismiss)))))
        (should (equal (buffer-local-value 'hermes-chat--model first) "first-model"))
        (should (equal (buffer-local-value 'hermes-chat--model second) "second-model")))))))

(ert-deftest hermes-chat-actions-popup-availability-follows-local-state ()
  "Idle, running, prompt and image preparation expose honest action state."
  (hermes-test-with-chat-buffer
   (let ((render (lambda ()
                   (keymap-popup--render
                    (keymap-popup--meta hermes-chat-actions-map 'descriptions)))))
     (should (hermes-chat--interrupt-unavailable-p))
     (should (string-match-p "Queue / send now" (funcall render)))
     (should (string-match-p "Steer / send now" (funcall render)))
     (should (string-match-p "Send" (funcall render)))
     (should-not (string-match-p "Answer prompt" (funcall render)))
     (setq hermes-chat--pending-assistant-id "assistant"
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "session")
     (should-not (hermes-chat--interrupt-unavailable-p))
     (should-not (hermes-chat--interrupt-send-unavailable-p))
     (should (string-match-p "Steer / queue fallback" (funcall render)))
     (should (string-match-p "Queue message" (funcall render)))
     (puthash "prompt" '(:type "clarify") (hermes-chat--ensure-pending-prompts))
     (should (string-match-p "Answer prompt" (funcall render)))
     (setq hermes-chat--busy-submit-context '(:pending t))
     (should (hermes-chat--interrupt-unavailable-p))
     (setq hermes-chat--pending-assistant-id nil
           hermes-chat--dashboard-session-ready-p nil)
     (dolist (phase '(uploading attaching local))
       (setq hermes-chat--session-bootstrap (and (eq phase 'local) '(:kind create))
             hermes-chat--unsettled-submit-context
             (list :queue-entry (list :image-record (list :state phase))))
       (should-not (hermes-chat--interrupt-unavailable-p))
       (should (hermes-chat--interrupt-send-unavailable-p)))
     ;; Do not leave synthetic submit ownership for real cleanup to settle.
     (setq hermes-chat--busy-submit-context nil
           hermes-chat--unsettled-submit-context nil
           hermes-chat--session-bootstrap nil))))

(ert-deftest hermes-chat-actions-popup-inherited-launchers-safe ()
  "Root launchers inside children refuse safely through the command loop."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (buffer-enable-undo)
      (insert "Exact draft")
      (undo-boundary)
      (let ((before (buffer-string)) (position (point))
            (undo (copy-tree buffer-undo-list)) (owner (current-buffer))
            (children '("S" "M" "w" "I" "X" "B")) paths)
        (dolist (child children)
          (dolist (target children)
            ;; S S and w w are child actions, not ancestor launchers.
            (unless (member (list child target) '(("S" "S") ("w" "w")))
              (let ((path (concat "C-c C-o " child " " target)))
                (unwind-protect
                    (progn
                      (should (equal
                               (should-error (execute-kbd-macro (kbd path))
                                             :type 'user-error)
                               '(user-error
                                 "Go back before choosing another menu")))
                      (push path paths)
                      (should-not (get-buffer keymap-popup--buffer-name))
                      ;; The supported child -> root -> child route still works.
                      (execute-kbd-macro
                       (kbd (concat "C-c C-o " child " q " target " q C-g")))
                      (should-not (get-buffer keymap-popup--buffer-name))
                      (should (eq owner (current-buffer)))
                      (should (eq owner (window-buffer (selected-window))))
                      (should (equal before (buffer-string)))
                      (should (= position (point)))
                      (should (equal undo buffer-undo-list)))
                  (keymap-popup-dismiss))))))
        (should (= (length paths) 34))))))

(ert-deftest hermes-chat-actions-popup-model-busy-availability ()
  "Busy model and reasoning actions remain visible but cannot prompt."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (setq hermes-chat--dashboard-running-p t
            hermes-chat--model "Current model"
            hermes-chat--runtime-flags '(:reasoning-effort "low"))
      (let (prompted)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (setq prompted t) "high")))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "C-c C-o M"))
                (with-current-buffer (get-buffer keymap-popup--buffer-name)
                  (dolist (text '("Model: Current model"
                                  "Reasoning: low"))
                    (goto-char (point-min))
                    (search-forward text)
                    (should (eq (get-text-property (line-beginning-position) 'face)
                                'keymap-popup-inapt))))
                (execute-kbd-macro (kbd "m e"))
                (should-not prompted)
                (should-not hermes-chat--dashboard-create-reasoning-effort)
                (with-current-buffer keymap-popup--buffer-name
                  (should (string-match-p "Reasoning: low" (buffer-string)))))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-set-reasoning-busy-before-prompt ()
  "Direct reasoning invocation refuses an already-busy chat before asking."
  (hermes-test-with-chat-buffer
    (setq hermes-chat--dashboard-running-p t)
    (let (prompted)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq prompted t) "high")))
        (should-error (call-interactively #'hermes-chat-set-reasoning)
                      :type 'user-error)
        (should-not prompted)
        (should-not hermes-chat--dashboard-create-reasoning-effort)))))

(ert-deftest hermes-chat-actions-popup-attach-image-label ()
  "The Images submenu advertises every image action together."
  (hermes-test-with-chat-buffer
    (unwind-protect
        (progn
          (hermes-chat-actions-map-popup)
          (call-interactively (key-binding (kbd "I")))
          (with-current-buffer (get-buffer keymap-popup--buffer-name)
            (dolist (label '("Attach image" "Paste image" "Preview / recover"
                             "Remove draft image"))
              (should (string-match-p label (buffer-string))))
            (should-not (string-match-p "Attach file" (buffer-string)))))
      (keymap-popup-dismiss))))

(ert-deftest hermes-chat-set-reasoning-before-session-stores-override ()
  "A fresh buffer stores reasoning effort without opening a session."
  (hermes-test-with-chat-buffer
   (hermes-chat-set-reasoning "high")
   (should (equal hermes-chat--dashboard-create-reasoning-effort "high"))
   (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                  "high"))
   (should-not hermes-chat--dashboard-client)
   (should (string-match-p "applies to next session" (buffer-string)))))

(ert-deftest hermes-chat-loads-cached-profile-model-for-fresh-draft ()
  "A fresh draft projects the selected profile's configured model."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "default") (is_default . t)
                                (model . "gpt-default"))
                               ((name . "coder") (model . "gpt-coder"))))))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--profile "coder")
     (hermes-chat--restore-draft-runtime)
     (should (equal hermes-chat--model "gpt-coder")))))

(ert-deftest hermes-chat-draft-model-cache-does-not-clobber-selection ()
  "Cached profile metadata cannot replace an explicit draft model choice."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "coder") (model . "default"))))))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--profile "coder"
           hermes-chat--dashboard-create-model "chosen"
           hermes-chat--model "chosen")
     (hermes-chat--restore-draft-runtime)
     (should (equal hermes-chat--model "chosen")))))

(ert-deftest hermes-chat-new-and-cleared-buffers-load-profile-model ()
  "New buffers and `/clear' restore the selected profile's configured model."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "coder") (model . "gpt-coder"))))))))
    (let ((buffer (hermes-chat--new-buffer "coder")))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal hermes-chat--model "gpt-coder"))
            (setq hermes-chat--model "stale")
            (hermes-chat--reset-transcript)
            (should (equal hermes-chat--model "gpt-coder")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-clear-reprojects-pending-draft-runtime ()
  "`/clear' keeps surviving create overrides visible and ready for creation."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "default")
                                (model . "profile-default"))))))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-create-model "grok-4.6"
           hermes-chat--dashboard-create-provider "xai-oauth"
           hermes-chat--dashboard-create-reasoning-effort "high"
           hermes-chat--dashboard-create-fast-p t
           hermes-chat--model "grok-4.6"
           hermes-chat--runtime-flags '(:reasoning-effort "high" :fast t))
     (hermes-chat--reset-transcript)
     (should (equal hermes-chat--model "grok-4.6"))
     (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                    "high"))
     (should (eq (plist-get hermes-chat--runtime-flags :fast) t))
     (let ((params (hermes-chat--dashboard-create-params)))
       (should (equal (plist-get params :model) "grok-4.6"))
       (should (equal (plist-get params :provider) "xai-oauth"))
       (should (equal (plist-get params :reasoning-effort) "high"))
       (should (eq (plist-get params :fast) t))))))

(ert-deftest hermes-chat-new-buffer-while-pending ()
  (let (original new)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
       (setq original (current-buffer))
       (insert "first")
       (hermes-chat-send)
       (setq new (hermes-chat--new-buffer))
       (unwind-protect
           (progn
             (should (buffer-live-p new))
             (with-current-buffer original
               (should hermes-chat--pending-assistant-id))
             (with-current-buffer new
               (should (derived-mode-p 'hermes-chat-mode))
               (should-not hermes-chat--pending-assistant-id)
               (should-not hermes-chat--session-id)
               (should (equal (hermes-chat-input-string) ""))))
         (when (buffer-live-p new)
           (kill-buffer new)))))))

(ert-deftest hermes-chat-dashboard-creates-session ()
  (let ((client (hermes-test--dashboard-client))
        start-callback create-resolve submit-client submit-text submit-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq start-callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (sent-client &rest args)
                 (should (eq sent-client client))
                 (setq create-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (sent-client text &rest args)
                 (setq submit-client sent-client
                       submit-text text
                       submit-args args))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (let ((chat-buffer (current-buffer)))
           (insert "hello dashboard")
           (hermes-chat-send)
           (should (functionp start-callback))
           (should (functionp create-resolve))
           (should-not submit-client)
           (with-temp-buffer
             (funcall create-resolve
                      '((session_id . "sid-live")
                        (stored_session_id . "sid-stored"))))
           (with-current-buffer chat-buffer
             (should (equal hermes-chat--session-id "sid-stored"))
             (should (equal (bound-and-true-p
                             hermes-chat--dashboard-active-session-id)
                            "sid-live"))
             (should hermes-chat--dashboard-session-ready-p)
             (should-not (hermes-dashboard-transport-client-session-id
                          client))
             (should-not (hermes-dashboard-transport-client-stored-session-id
                          client))
             (should (eq hermes-chat--process client))
             (should (eq submit-client client))
             (should (equal submit-text "hello dashboard"))
             (should (equal (plist-get submit-args :session-id)
                            "sid-live")))))))))

(ert-deftest hermes-chat-dashboard-submits-prompt ()
  (let ((client (hermes-test--dashboard-client))
        resumed-session resume-resolve submit-text submit-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (&rest _args)
                 (error "session.create should not run for resumed chat")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (sent-client session-id &rest args)
                 (should (eq sent-client client))
                 (setq resumed-session session-id
                       resume-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (setq submit-text text
                       submit-args args))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (let ((chat-buffer (current-buffer)))
           (setq hermes-chat--session-id "sid-stored")
           (insert "resume me")
           (hermes-chat-send)
           (should (equal resumed-session "sid-stored"))
           (should (functionp resume-resolve))
           (should-not submit-text)
           (with-temp-buffer
             (funcall resume-resolve
                      '((session_id . "sid-live")
                        (resumed . "sid-stored"))))
           (with-current-buffer chat-buffer
             (should (equal hermes-chat--session-id "sid-stored"))
             (should (equal (bound-and-true-p
                             hermes-chat--dashboard-active-session-id)
                            "sid-live"))
             (should hermes-chat--dashboard-session-ready-p)
             (should-not (hermes-dashboard-transport-client-session-id
                          client))
             (should-not (hermes-dashboard-transport-client-stored-session-id
                          client))
             (should (equal submit-text "resume me"))
             (should (equal (plist-get submit-args :session-id)
                            "sid-live")))))))))

(ert-deftest hermes-chat-dashboard-reset-ignores-stale-resume-result ()
  "A pre-reset session callback must not repopulate the cleared chat."
  (let ((client (hermes-test--dashboard-client)) resume-resolve submitted)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resume-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (setq submitted text))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored")
         (insert "stale prompt")
         (hermes-chat-send)
         (should (functionp resume-resolve))
         (hermes-chat--reset-transcript)
         (funcall resume-resolve
                  '((session_id . "old-live")
                    (resumed . "sid-stored")
                    (running . :false)))
         (should-not submitted)
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-running-p)
         (should-not (hermes-chat--entries)))))))

(ert-deftest hermes-chat-dashboard-submit-signal-clears-running-state ()
  "A synchronous prompt failure must not leave the session locally busy."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args)
                 (user-error "submit failed"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "fail synchronously")
         (hermes-chat-send)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--dashboard-running-p)
         (should (string-match-p "submit failed" (buffer-string))))))))

(ert-deftest hermes-chat-dashboard-streams-events-into-ewoc ()
  (let ((client (hermes-test--dashboard-client))
        callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-stream")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "stream please")
         (hermes-chat-send)
         (funcall callback
                  '(:type delta
                    :session-id "sid-other"
                    :content "ignored"))
         (funcall callback
                  '(:type status
                    :session-id "sid-other"
                    :status-key "run"
                    :status "running"
                    :content "Ignore me"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-other"
                    :tool-call-id "tool-other"
                    :name "terminal"
                    :status "running"
                    :preview "ignored"))
         (funcall callback
                  '(:type delta
                    :session-id "sid-stream"
                    :content "hello"))
         (funcall callback
                  '(:type status
                    :session-id "sid-stream"
                    :status-key "run"
                    :status "running"
                    :content "Thinking"))
         (funcall callback
                  '(:type status
                    :session-id "sid-stream"
                    :status-key "run"
                    :status "running"
                    :content "Still thinking"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-stream"
                    :tool-call-id "tool-1"
                    :name "terminal"
                    :status "running"
                    :preview "make test"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-stream"
                    :tool-call-id "tool-1"
                    :name "terminal"
                    :status "completed"
                    :duration 0.5))
         (funcall callback
                  '(:type delta
                    :session-id "sid-stream"
                    :content " world"))
         (funcall callback '(:type done :session-id "sid-stream"))
         (let* ((entries (hermes-chat--entries))
                (roles (mapcar (lambda (entry) (plist-get entry :role))
                               entries))
                (assistant (nth 3 entries))
                (status (nth 1 entries))
                (tool (nth 2 entries)))
           (should (equal roles '(user status tool assistant)))
           (should (equal (plist-get assistant :content) "hello world"))
           (should (equal (plist-get assistant :status) 'done))
           (should (equal (plist-get status :content) "Still thinking"))
           (should (equal (plist-get tool :status) "completed"))
           (should-not hermes-chat--pending-assistant-id)))))))

(ert-deftest hermes-chat-dashboard-drops-late-settled-turn-events ()
  "Late fallback events must not appear after a newer turn's final reply."
  (let ((client (hermes-test--dashboard-client))
        callback interrupt-resolve)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-interrupt)
         (funcall callback
                  '(:type done :session-id "sid-active" :status "interrupted"))
         (funcall interrupt-resolve '((status . "ok")))
         (hermes-chat--activate-backend-turn "second")
         (funcall callback
                  '(:type done :session-id "sid-active" :content "second reply"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-active"
                    :tool-call-id "late-tool"
                    :name "terminal"
                    :status "completed"
                    :preview "late output"))
         (let ((entries (hermes-chat--entries)))
           (should (equal
                    (mapcar (lambda (entry) (plist-get entry :role)) entries)
                    '(user status assistant user assistant)))
           (should (equal
                    (plist-get (hermes-test--last-assistant-entry) :content)
                    "second reply"))))))))

(ert-deftest hermes-chat-dashboard-handles-close-after-settled-turn ()
  "A current transport close must clear session state after reply settlement."
  (let ((client (hermes-test--dashboard-client))
        callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (funcall callback
                  '(:type done :session-id "sid-active" :content "reply"))
         (should (equal hermes-chat--dashboard-active-session-id "sid-active"))
         (funcall callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (should-not hermes-chat--dashboard-active-session-id)
         (should (equal
                  (hermes-chat--status-name
                   (plist-get hermes-chat--status-state :status))
                  "closed")))))))

(ert-deftest hermes-chat-dashboard-collapses-reasoning-into-toggle ()
  (let ((client (hermes-test--dashboard-client))
        callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-thinking")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "think first")
         (hermes-chat-send)
         (funcall callback
                  '(:type commentary
                    :session-id "sid-other"
                    :event "reasoning.delta"
                    :content "ignore this"))
         (dolist (chunk '("I\\n" " need\\n" " to inspect^J" " repo"))
           (funcall callback
                    (list :type 'commentary
                          :session-id "sid-thinking"
                          :event "reasoning.delta"
                          :content chunk)))
         (let ((collapsed (buffer-string)))
           (should (string-match-p "▸ Reasoning" collapsed))
           (should-not (string-match-p "inspect repo" collapsed)))
         (hermes-test--push-button-labeled "Reasoning")
         (let ((expanded (buffer-string)))
           (should (string-match-p "▾ Reasoning" expanded))
           (should (string-match-p "I need to inspect repo" expanded))
           (should-not (string-match-p "\\\\n\|\\^J" expanded)))
         (funcall callback
                  '(:type commentary
                    :session-id "sid-thinking"
                    :event "reasoning.delta"
                    :content " and cite files"))
         (let ((expanded (buffer-string)))
           (should (string-match-p "▾ Reasoning" expanded))
           (should (string-match-p "I need to inspect repo and cite files"
                                   expanded)))
         (funcall callback
                  '(:type delta
                    :session-id "sid-thinking"
                    :content "Clean answer"))
         (funcall callback '(:type done :session-id "sid-thinking"))
         (let* ((entries (hermes-chat--entries))
                (roles (mapcar (lambda (entry) (plist-get entry :role))
                               entries))
                (assistant (nth 2 entries))
                (commentary (nth 1 entries)))
           (should (equal roles '(user commentary assistant)))
           (should (= (cl-count 'commentary roles) 1))
           (should (equal (plist-get assistant :content) "Clean answer"))
           (should-not (string-match-p "inspect repo" (plist-get assistant :content)))
           (should (equal (plist-get commentary :content)
                          "I\\n need\\n to inspect^J repo and cite files"))))))))

(ert-deftest hermes-chat-dashboard-close-clears-pending-for-retry ()
  (let* ((client-1 (hermes-test--dashboard-client))
         (client-2 (hermes-test--dashboard-client))
         (clients (list client-1 client-2))
         callback first-callback second-callback
         resumed-session submit-sessions second-assistant-id)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 (pop clients)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live-1")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (setq resumed-session session-id)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live-2")
                            (resumed . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (push (plist-get args :session-id) submit-sessions))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (setq first-callback callback)
         (should (equal submit-sessions '("sid-live-1")))
         (setf (hermes-dashboard-transport-client-websocket client-1) nil
               (hermes-dashboard-transport-client-ready-p client-1) nil)
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :status) 'error))
           (should (string-match-p "WebSocket closed"
                                   (plist-get assistant :content))))
         (should-not hermes-chat--pending-assistant-id)
         (insert "second")
         (hermes-chat-send)
         (setq second-callback callback
               second-assistant-id hermes-chat--pending-assistant-id)
         (should (equal resumed-session "sid-stored"))
         (should (equal submit-sessions '("sid-live-2" "sid-live-1")))
         (funcall first-callback '(:type error :content "late old error"))
         (should (equal hermes-chat--pending-assistant-id second-assistant-id))
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "late old close"))
         (should (equal hermes-chat--dashboard-active-session-id "sid-live-2"))
         (funcall second-callback
                  '(:type delta
                    :session-id "sid-live-2"
                    :content "retry ok"))
         (funcall second-callback '(:type done :session-id "sid-live-2"))
         (let ((assistant (nth 3 (hermes-chat--entries))))
           (should (equal (plist-get assistant :content) "retry ok"))
           (should (equal (plist-get assistant :status) 'done)))
         (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-idle-reconciliation-settles-missing-finished-session ()
  "A missing durable row cannot keep a locally finished chat busy forever."
  (let ((client (hermes-test--dashboard-client)) idle rescheduled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) (setq rescheduled t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-live"
             hermes-chat--session-id "sid-missing"
             hermes-chat--dashboard-running-p t)
       (setq rescheduled nil)
       (hermes-chat--dashboard-reconcile-idle
        (hermes-chat--dashboard-idle-context (lambda () (setq idle t))))
       (should idle)
       (should-not rescheduled)
       (should-not hermes-chat--dashboard-running-p)))))

(ert-deftest hermes-chat-collect-urls-extracts-in-order ()
  "URLs are returned in transcript order across entries."
  (should (equal '("https://a.example" "https://b.example")
                 (hermes-chat--collect-urls
                  (list '(:content "see https://a.example now")
                        '(:content "then https://b.example end"))))))

(ert-deftest hermes-chat-collect-urls-dedupes ()
  "Repeated URLs collapse to a single entry."
  (should (equal '("https://a.example")
                 (hermes-chat--collect-urls
                  (list '(:content "https://a.example")
                        '(:content "again https://a.example"))))))

(ert-deftest hermes-chat-collect-urls-handles-empty-and-nil-content ()
  "Entries without links or with nil content yield no URLs and no error."
  (should-not (hermes-chat--collect-urls
               (list '(:content "no links here") '(:content nil)))))

(ert-deftest hermes-chat-done-event-records-usage ()
  "A done event records usage in header state; the compact header omits the gauge."
  (hermes-test-with-chat-buffer
   (hermes-chat--run-turn-reducer nil
    '(:type done :usage (:input 1200 :output 340)))
   (should (equal (plist-get hermes-chat--status-state :usage) '(:input 1200 :output 340)))
   (should-not (string-match-p "1200↑ 340↓ tok" (hermes-chat--header-line)))))

(ert-deftest hermes-chat-warm-model-options-fetches-after-ready ()
  "Warming defers the fetch until the client's readiness promise resolves."
  (let* ((hermes-dashboard-transport--model-options-cache nil)
         (ready (hermes--promise-make))
         (client (make-hermes-dashboard-transport-client :ready-promise ready))
         fetched)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (setq fetched t)
                 (funcall (plist-get args :resolve) '((providers . nil))))))
      (hermes-chat--warm-model-options client)
      (should-not fetched)
      (hermes--promise-resolve ready t)
      (should fetched)
      (should (hermes-dashboard-transport-cached-model-options)))))

(ert-deftest hermes-chat-new-buffer-sets-profile ()
  "A profile chat records the profile; a blank one stays nil."
  (let ((buffer (hermes-chat--new-buffer "work")))
    (unwind-protect
        (with-current-buffer buffer (should (equal hermes-chat--profile "work")))
      (kill-buffer buffer)))
  (let ((buffer (hermes-chat--new-buffer "")))
    (unwind-protect
        (with-current-buffer buffer (should-not hermes-chat--profile))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-new-buffer-pins-instance ()
  "A new chat owns the resolved Hermes instance for its lifetime."
  (let* ((instance '("remote" . "https://hermes.example.test"))
         (buffer (hermes-chat--new-buffer "work" nil instance)))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal hermes-instance instance)))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-legacy-url-change-before-connect-is-honored ()
  "Unconfigured chats keep following the legacy dashboard URL until connect."
  (let ((hermes-instances nil)
        (hermes-dashboard-transport-url "http://127.0.0.1:9119")
        acquired-url buffer)
    (unwind-protect
        (progn
          (setq buffer (hermes-chat--new-buffer "work"))
          (setq hermes-dashboard-transport-url "https://hermes.example.test")
          (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                     (lambda (&rest _)
                       (setq acquired-url hermes-dashboard-transport-url)
                       (hermes-test--dashboard-client))))
            (with-current-buffer buffer
              (hermes-chat--dashboard-ensure-client))
            (should (equal acquired-url "https://hermes.example.test"))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-existing-dashboard-client-matches-instance ()
  "The profile picker reuses only a client for its selected instance."
  (let ((local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        (local-client (hermes-test--dashboard-client))
        (remote-client (hermes-test--dashboard-client))
        buffers)
    (unwind-protect
        (progn
          (dolist (pair (list (cons local local-client)
                              (cons remote remote-client)))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance (car pair)
                      hermes-chat--dashboard-client (cdr pair)))))
          (with-temp-buffer
            (setq hermes-instance local)
            (should (eq (hermes-chat--existing-dashboard-client)
                        local-client))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest hermes-chat-new-buffer-uses-project-identity ()
  "Fresh buffer names reflect instance, profile, and launching project."
  (let* ((default-directory "/tmp/emacs-hermes/")
         (buffer (hermes-chat--new-buffer nil nil)))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (buffer-name) "*Hermes@default: [emacs-hermes]*")))
      (kill-buffer buffer)))
  (let* ((default-directory "/tmp/emacs-hermes/")
         (buffer (hermes-chat--new-buffer "work" "deploy")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (buffer-name)
                         "*Hermes@work: [emacs-hermes]*"))
          (should hermes-chat--title-manual-p))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-profile-candidates-describe-dashboard-profiles ()
  "Profile candidates parse and sort to (NAME . MODEL-LABEL) pairs."
  (let ((cands (hermes-chat--profile-candidates
                '((profiles
                   . (((name . "zeta"))
                      ((name . "") (description . "ignored"))
                      ((name . "elisp-dev") (is_default . nil)
                       (provider . "anthropic") (model . "claude-sonnet")
                       (description . "Emacs Lisp work"))
                      ((name . "default") (is_default . t)
                       (provider . "openai") (model . "gpt-5.5")
                       (description . "Main profile")
                       (gateway_running . t))
                      ((name . "alpha") (has_alias . t))))))))
    (should (equal (mapcar #'car cands)
                   '("default" "alpha" "elisp-dev" "zeta")))
    (should (equal (cdr (assoc "default" cands)) "openai/gpt-5.5"))
    (should (equal (cdr (assoc "elisp-dev" cands)) "anthropic/claude-sonnet"))
    (should-not (cdr (assoc "alpha" cands)))))

(ert-deftest hermes-chat-profile-annotation-shows-model ()
  "The profile annotation shows the model, and nothing when none is known."
  (let* ((cands '(("default" . "openai/gpt-5.5") ("alpha" . nil)))
         (annotate (hermes-chat--profile-annotation-function cands)))
    (should (string-match-p "openai/gpt-5.5" (funcall annotate "default")))
    (should-not (funcall annotate "alpha"))
    (should-not (funcall annotate "unknown"))))

(ert-deftest hermes-chat-read-profile-falls-back-when-dashboard-unavailable ()
  "A cold profile chooser falls back while its asynchronous warmup fails."
  (let (prompt messages)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (_client) (hermes--promise-rejected "404 not found")))
              ((symbol-function 'read-string)
               (lambda (text &rest _)
                 (setq prompt text)
                 "manual-profile"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (should (equal (hermes-chat--read-profile) "manual-profile"))
      (should (string-match-p "blank for default" prompt))
      (should (string-match-p "No dashboard profiles available" prompt))
      (should (string-match-p "No dashboard profiles available"
                              (car messages))))))

(ert-deftest hermes-chat-read-profile-falls-back-when-list-empty ()
  "An empty dashboard profile list falls back with a helpful message."
  (let ((hermes-dashboard-transport--profile-cache nil)
        prompt messages fetched)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (client)
                 (setq fetched client)
                 (hermes--promise-resolved '((profiles . nil)))))
              ((symbol-function 'read-string)
               (lambda (text &rest _)
                 (setq prompt text)
                 "manual-profile"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (should (equal (hermes-chat--read-profile) "manual-profile"))
      (should (eq fetched 'fake-client))
      (should (string-match-p "blank for default" prompt))
      (should (string-match-p "No dashboard profiles available" prompt))
      (should (string-match-p "No dashboard profiles available" (car messages))))))

(ert-deftest hermes-chat-read-profile-skips-spawn-without-client ()
  "With no live chat client the profile chooser prompts raw, never spawning."
  (let (prompt spawned)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq spawned t) 'transient-client))
              ((symbol-function 'read-string)
               (lambda (text &rest _) (setq prompt text) "manual-profile"))
              ((symbol-function 'message) #'ignore))
      (should (equal (hermes-chat--read-profile) "manual-profile"))
      (should-not spawned)
      (should (string-match-p "blank for default" prompt)))))

(ert-deftest hermes-chat-profile-list-payload-serves-cache-and-revalidates ()
  "A warm profile cache is returned while an existing client refreshes it async."
  (let ((hermes-dashboard-transport--profile-cache nil)
        (client (hermes-test--dashboard-client))
        refreshed)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (value)
                 (setq refreshed value)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (&rest _) (error "synchronous profile fetch"))))
      (let ((cached
             '((profiles . (((name . "default") (is_default . t))
                            ((name . "elisp-dev")))))))
        (hermes-dashboard-transport--store-profile-cache cached)
        (should (equal (hermes-chat--profile-list-payload) cached))
        (should (eq refreshed client))))))

(ert-deftest hermes-chat-profile-list-cache-miss-warms-asynchronously ()
  "A cold profile picker starts a warmup but never calls synchronous HTTP."
  (let ((hermes-dashboard-transport--profile-cache nil)
        (client (hermes-test--dashboard-client)) warmed)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (value)
                 (setq warmed value)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (&rest _) (error "synchronous profile fetch")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "synchronous HTTP"))))
      (should-not (hermes-chat--profile-list-payload))
      (should (eq warmed client)))))

(ert-deftest hermes-chat-read-profile-completes-from-cache-without-client ()
  "With a warm cache and no live client the picker completes, never spawning."
  (let ((hermes-dashboard-transport--profile-cache nil)
        spawned)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq spawned t) 'transient-client))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (cl-find "elisp-dev" collection :test #'string-match-p))))
      (hermes-dashboard-transport--store-profile-cache
       '((profiles . (((name . "default") (is_default . t))
                      ((name . "elisp-dev"))))))
      (should (equal (hermes-chat--read-profile) "elisp-dev"))
      (should-not spawned))))

(ert-deftest hermes-chat-completes-dashboard-profile ()
  "Interactively creating a chat chooses from the warmed profile cache."
  (let ((hermes-dashboard-transport--profile-cache nil) choices)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 (cl-find "elisp-dev" collection :test #'string-match-p))))
      (hermes-dashboard-transport--store-profile-cache
       '((profiles . (((name . "default") (is_default . t))
                      ((name . "elisp-dev")
                       (description . "Emacs Lisp work"))))))
      (let ((buffer (call-interactively #'hermes-chat)))
        (unwind-protect
            (progn
              (should (cl-find "default" choices :test #'string-match-p))
              (with-current-buffer buffer
                (should (equal hermes-chat--profile "elisp-dev"))))
          (kill-buffer buffer))))))

(ert-deftest hermes-chat-send-passes-profile-to-session-create ()
  "The buffer's profile is threaded into session.create."
  (let (create-profile)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (hermes-test--dashboard-client)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-profile (plist-get args :profile))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid") (stored_session_id . "stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) nil)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--profile "work")
         (insert "hello")
         (hermes-chat-send)
         (should (equal create-profile "work")))))))

(ert-deftest hermes-chat-blank-profile-fallback-uses-default-session-profile ()
  "Blank raw fallback input leaves session.create profile omitted."
  (let ((hermes-dashboard-transport--profile-cache nil)
        create-profile fetched)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (client)
                 (setq fetched client)
                 (hermes--promise-rejected "404 not found")))
              ((symbol-function 'read-string)
               (lambda (&rest _) "   "))
              ((symbol-function 'message)
               (lambda (&rest _) nil))
              ((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (hermes-test--dashboard-client)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-profile (plist-get args :profile))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid") (stored_session_id . "stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) nil)))
      (let ((hermes-transport-send-function #'hermes-transport-send)
            (buffer (call-interactively #'hermes-chat)))
        (unwind-protect
            (with-current-buffer buffer
              (should (eq fetched 'fake-client))
              (should-not hermes-chat--profile)
              (insert "hello")
              (hermes-chat-send)
              (should-not create-profile))
          (kill-buffer buffer))))))

(ert-deftest hermes-chat-usage-content-formats-counts-and-credits ()
  "Usage text carries the four counters and appends credit lines."
  (should (equal (hermes-chat--usage-content
                  '((calls . 3) (input . 100) (output . 50) (total . 150)
                    (credits_lines . ("Nous: 1.2 credits left"))))
                 "Usage: 3 calls — input 100, output 50, total 150 tokens\nNous: 1.2 credits left"))
  (should (equal (hermes-chat--usage-content '())
                 "Usage: 0 calls — input 0, output 0, total 0 tokens")))

(ert-deftest hermes-chat-show-usage-inserts-panel-for-session ()
  "The usage command fetches `session.usage' for the attached session."
  (let (seen-session)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-usage)
               (lambda (_client &rest args)
                 (setq seen-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve)
                          '((calls . 2) (input . 10) (output . 5) (total . 15))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client
             (hermes-test--dashboard-client)
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-usage")
       (cl-letf (((symbol-function 'hermes-chat--dashboard-control-client)
                  (lambda () hermes-chat--dashboard-client)))
         (hermes-chat-show-usage))
       (should (equal seen-session "sid-usage"))
       (should (string-match-p "Usage: 2 calls" (buffer-string)))))))

(ert-deftest hermes-chat-show-usage-ignores-stale-session ()
  "A late usage result cannot render into a replacement session."
  (let (resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-usage)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-chat--dashboard-control-client)
               (lambda () hermes-chat--dashboard-client)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old")
       (hermes-chat-show-usage)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((calls . 2) (input . 10) (output . 5) (total . 15)))
       (should-not (string-match-p "Usage:" (buffer-string)))))))

(ert-deftest hermes-chat-notification-clear-adds-no-transcript-entry ()
  "notification.clear retracts a keyed notice; it must not render an entry."
  (should-not (hermes-chat--transcript-event-p
               '(:type status :event "notification.clear"
                       :notification-key "credits")))
  (should (hermes-chat--transcript-event-p
           '(:type status :event "notification.show"
                   :content "[warning] credits low"))))

(ert-deftest hermes-chat-load-populates-registry-functions ()
  "Loading `hermes-chat' wires the buffer/dashboard registry variables."
  (should (eq hermes-chat--submit-function #'hermes-chat--submit-content))
  (should (eq hermes-chat--turn-event-function #'hermes-chat--run-turn-reducer))
  (should (memq #'hermes-chat--handoff-stop hermes-chat-cleanup-functions)))

(provide 'hermes-chat-tests)
;;; hermes-chat-tests.el ends here
