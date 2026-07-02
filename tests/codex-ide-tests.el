;;; codex-ide-tests.el --- ERT tests for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; ERT tests for `codex-ide'.  These run under `emacs -Q --batch' with no
;; network and no live Codex process.

;;; Code:

(require 'ert)
(require 'codex-ide)
(require 'codex-ide-term)
(require 'codex-ide-menu)

(defun codex-ide-test--with-vars (body)
  "Run BODY with controlled `codex-ide' variables, then restore them."
  (let ((codex-ide-cli-path "codex")
        (codex-ide-config-overrides nil)
        (codex-ide-ask-for-approval nil)
        (codex-ide-no-alt-screen nil)
        (codex-ide-display-buffer-function #'pop-to-buffer-same-window)
        (codex-ide-cli-extra-args nil))
    (funcall body)))

(defun codex-ide-test--make-process (name)
  "Return a live test process named NAME."
  (unless (executable-find "sleep")
    (ert-skip "sleep executable not found"))
  (start-process name nil "sleep" "60"))

(defun codex-ide-test--make-buffer-process (buffer name)
  "Return a live test process named NAME attached to BUFFER."
  (unless (executable-find "sleep")
    (ert-skip "sleep executable not found"))
  (start-process name buffer "sleep" "60"))

(defun codex-ide-test--make-recoverable-buffer (root name process-name)
  "Return (BUFFER PROCESS) for a live orphan Codex buffer."
  (let ((buffer (get-buffer-create name))
        process)
    (with-current-buffer buffer
      (setq default-directory root)
      (setq-local codex-ide--session-root nil)
      (setq-local codex-ide--session-id nil))
    (setq process
          (codex-ide-test--make-buffer-process buffer process-name))
    (set-process-query-on-exit-flag process nil)
    (list buffer process)))

(defun codex-ide-test--kill-buffer-process (buffer process)
  "Kill BUFFER and PROCESS when they are live."
  (when (and process (process-live-p process))
    (delete-process process))
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun codex-ide-test--call-with-project (body)
  "Call BODY with a temporary project root."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-project-" t)))
         (default-directory root))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (funcall body root))
      (delete-directory root t))))

(defun codex-ide-test--call-with-buffer-process (body)
  "Call BODY with a temp buffer and live process."
  (unless (executable-find "sleep")
    (ert-skip "sleep executable not found"))
  (let ((buffer (generate-new-buffer " *codex-ide-process-test*"))
        process)
    (unwind-protect
        (progn
          (setq process (start-process "codex-ide-test-process"
                                       buffer "sleep" "60"))
          (funcall body buffer process))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun codex-ide-test--make-session (root id)
  "Return a live test session for ROOT and ID."
  (let* ((buffer (get-buffer-create (codex-ide--get-buffer-name root id)))
         (process (codex-ide-test--make-buffer-process
                   buffer (format "codex-ide-test-%d" id)))
         (session (codex-ide--make-session root id buffer process)))
    (set-process-query-on-exit-flag process nil)
    (with-current-buffer buffer
      (setq-local codex-ide--session-root root)
      (setq-local codex-ide--session-id id))
    session))

(defun codex-ide-test--store-session (session)
  "Store SESSION in `codex-ide--sessions'."
  (let ((root (plist-get session :root)))
    (puthash root (cons session (gethash root codex-ide--sessions))
             codex-ide--sessions)
    (codex-ide--activate-session session)))

(defun codex-ide-test--kill-session (session)
  "Kill SESSION's process and buffer."
  (let ((process (plist-get session :process))
        (buffer (plist-get session :buffer)))
    (when (and process (process-live-p process))
      (delete-process process))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun codex-ide-test--session-by-id (sessions id)
  "Return the session with ID from SESSIONS."
  (cl-find id sessions
           :key (lambda (session) (plist-get session :id))
           :test #'=))

(defun codex-ide-test--session-by-root (sessions root)
  "Return the session for ROOT from SESSIONS."
  (cl-find root sessions
           :key (lambda (session) (plist-get session :root))
           :test #'equal))

(defun codex-ide-test--session-visible-p (session)
  "Return non-nil when SESSION's buffer is visible."
  (get-buffer-window (plist-get session :buffer) t))

(defun codex-ide-test--call-with-toggle-stubs (body)
  "Call BODY with noisy toggle collaborators muted."
  (cl-letf (((symbol-function 'codex-ide-context-record-source-buffer)
             (lambda (&rest _args) nil))
            ((symbol-function 'codex-ide-term--sync-dimensions)
             (lambda (&rest _args) nil))
            ((symbol-function 'codex-ide-debug)
             (lambda (&rest _args) nil)))
    (funcall body)))

(defun codex-ide-test--toggle-in-root (root)
  "Call `codex-ide-toggle' for ROOT with same-window display."
  (codex-ide-test--call-with-toggle-stubs
   (lambda ()
     (let ((default-directory root)
           (codex-ide-display-buffer-function #'pop-to-buffer-same-window))
       (codex-ide-toggle)))))

(defun codex-ide-test--call-with-sessions (root-ids body)
  "Call BODY with live sessions for ROOT-IDS.
ROOT-IDS is a list of (ROOT ID) pairs.  BODY receives the session records."
  (let ((codex-ide--sessions (make-hash-table :test 'equal))
        (codex-ide--active-session-ids (make-hash-table :test 'equal))
        sessions)
    (unwind-protect
        (progn
          (dolist (root (delete-dups (mapcar #'car root-ids)))
            (let ((git-dir (expand-file-name ".git" root)))
              (unless (file-exists-p git-dir)
                (make-directory git-dir))))
          (setq sessions
                (mapcar (lambda (root-id)
                          (pcase-let ((`(,root ,id) root-id))
                            (let ((session
                                   (codex-ide-test--make-session root id)))
                              (codex-ide-test--store-session session)
                              session)))
                        root-ids))
          (funcall body sessions))
      (mapc #'codex-ide-test--kill-session sessions))))

(ert-deftest codex-ide-build-command-default ()
  "Default command is just the program with no args."
  (codex-ide-test--with-vars
   (lambda ()
     (should (equal (codex-ide--build-command)
                    (cons "codex" nil))))))

(ert-deftest codex-ide-build-command-resume-last ()
  "RESUME-LAST adds \"resume --last\"."
  (codex-ide-test--with-vars
   (lambda ()
     (should (equal (codex-ide--build-command t)
                    (cons "codex" '("resume" "--last")))))))

(ert-deftest codex-ide-build-command-session-id ()
  "SESSION-ID adds \"resume <id>\" as positional args."
  (codex-ide-test--with-vars
   (lambda ()
     (should (equal (codex-ide--build-command nil "abc-123")
                    (cons "codex" '("resume" "abc-123")))))))

(ert-deftest codex-ide-build-command-session-id-wins ()
  "SESSION-ID takes precedence over RESUME-LAST."
  (codex-ide-test--with-vars
   (lambda ()
     (should (equal (codex-ide--build-command t "abc-123")
                    (cons "codex" '("resume" "abc-123")))))))

(ert-deftest codex-ide-build-command-config-overrides ()
  "Config overrides fold as alternating \"-c\" \"key=value\" pairs."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-config-overrides '(("model" . "o3"))))
       (should (equal (codex-ide--build-command)
                      (cons "codex" '("-c" "model=o3"))))))))

(ert-deftest codex-ide-build-command-config-overrides-multiple ()
  "Multiple overrides each emit their own \"-c\" pair."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-config-overrides
            '(("model" . "o3")
              ("sandbox_permissions" . "[\"disk-full-read-access\"]"))))
       (should (equal (codex-ide--build-command)
                      (cons
                       "codex"
                       '("-c" "model=o3"
                         "-c" "sandbox_permissions=[\"disk-full-read-access\"]"))))))))

(ert-deftest codex-ide-build-command-ask-for-approval-nil ()
  "Nil approval omits the flag."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-ask-for-approval nil))
       (should (equal (codex-ide--build-command)
                      (cons "codex" nil)))))))

(ert-deftest codex-ide-build-command-ask-for-approval-on-request ()
  "Non-nil approval includes the flag and value."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-ask-for-approval 'on-request))
       (should (equal (codex-ide--build-command)
                      (cons "codex" '("--ask-for-approval" "on-request"))))))))

(ert-deftest codex-ide-build-command-no-alt-screen ()
  "`codex-ide-no-alt-screen' adds the flag."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-no-alt-screen t))
       (should (equal (codex-ide--build-command)
                      (cons "codex" '("--no-alt-screen"))))))))

(ert-deftest codex-ide-build-command-extra-args ()
  "Extra args are appended verbatim."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-cli-extra-args '("--search")))
       (should (equal (codex-ide--build-command)
                      (cons "codex" '("--search"))))))))

(ert-deftest codex-ide-build-command-combined ()
  "All modeled flags combine in the documented order."
  (codex-ide-test--with-vars
   (lambda ()
     (let ((codex-ide-config-overrides '(("model" . "o3")))
           (codex-ide-ask-for-approval 'never)
           (codex-ide-no-alt-screen t)
           (codex-ide-cli-extra-args '("--search")))
       (should (equal (codex-ide--build-command t)
                      (cons
                       "codex"
                       '("-c" "model=o3"
                         "resume" "--last"
                         "--ask-for-approval" "never"
                         "--no-alt-screen"
                         "--search"))))))))

(ert-deftest codex-ide-term-send-string-delegates-to-vterm ()
  "String input goes directly to vterm."
  (let (sent)
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (string)
                 (setq sent string))))
      (codex-ide-term--send-string "hello"))
    (should (equal sent "hello"))))

(ert-deftest codex-ide-term-send-return-delegates-to-vterm ()
  "Return input goes directly to vterm."
  (let (called)
    (cl-letf (((symbol-function 'vterm-send-return)
               (lambda ()
                 (setq called t))))
      (codex-ide-term--send-return))
    (should called)))

(ert-deftest codex-ide-term-send-escape-delegates-to-vterm ()
  "Escape input goes directly to vterm."
  (let (called)
    (cl-letf (((symbol-function 'vterm-send-escape)
               (lambda ()
                 (setq called t))))
      (codex-ide-term--send-escape))
    (should called)))

(ert-deftest codex-ide-term-osc-color-query-no-reply ()
  "Non-query output produces no OSC color replies."
  (should-not (codex-ide-term--osc-color-query-types "plain output"))
  (should-not (codex-ide-term--osc-color-query-replies
               "plain output" '(1 2 3) '(4 5 6))))

(ert-deftest codex-ide-term-osc-color-query-foreground-reply ()
  "OSC 10 query returns the foreground color reply."
  (should (equal (codex-ide-term--osc-color-query-types "\e]10;?\e\\")
                 '(foreground)))
  (should (equal (codex-ide-term--osc-color-query-replies
                  "\e]10;?\e\\" '(1 2 3) '(4 5 6))
                 "\e]10;rgb:0001/0002/0003\e\\")))

(ert-deftest codex-ide-term-osc-color-query-background-reply ()
  "OSC 11 query returns the background color reply."
  (should (equal (codex-ide-term--osc-color-query-types "\e]11;?\e\\")
                 '(background)))
  (should (equal (codex-ide-term--osc-color-query-replies
                  "\e]11;?\e\\" '(1 2 3) '(4 5 6))
                 "\e]11;rgb:0004/0005/0006\e\\")))

(ert-deftest codex-ide-term-osc-color-query-combined-replies ()
  "Combined OSC 10/11 queries return replies in input order."
  (should (equal (codex-ide-term--osc-color-query-replies
                  "\e]10;?\e\\\e]11;?\e\\" '(1 2 3) '(4 5 6))
                 "\e]10;rgb:0001/0002/0003\e\\\e]11;rgb:0004/0005/0006\e\\")))

(ert-deftest codex-ide-term-osc-color-query-face-fallbacks ()
  "Nil face colors fall back to default terminal colors."
  (cl-letf (((symbol-function 'face-foreground)
             (lambda (&rest _args) nil))
            ((symbol-function 'face-background)
             (lambda (&rest _args) nil)))
    (should (equal (codex-ide-term--default-face-color-values 'foreground)
                   '(255 255 255)))
    (should (equal (codex-ide-term--default-face-color-values 'background)
                   '(0 0 0)))
    (should (equal (codex-ide-term--vterm-osc-color-replies
                    "\e]10;?\e\\\e]11;?\e\\")
                   "\e]10;rgb:00ff/00ff/00ff\e\\\e]11;rgb:0000/0000/0000\e\\"))))

(ert-deftest codex-ide-term-vterm-configure-installs-filter-once ()
  "Codex vterm setup installs the output filter once."
  (codex-ide-test--call-with-buffer-process
   (lambda (buffer process)
     (let ((original (lambda (_process _input) nil)))
       (set-process-filter process original)
       (with-current-buffer buffer
         (setq truncate-lines nil)
         (codex-ide-term--vterm-configure-buffer process)
         (codex-ide-term--vterm-configure-buffer process)
         (should truncate-lines)
         (should-not (local-variable-p
                      'vterm-scroll-to-bottom-on-output buffer)))
       (should (eq (process-filter process)
                   #'codex-ide-term--vterm-output-filter))
       (should (eq (process-get
                    process 'codex-ide-term--vterm-original-filter)
                   original))))))

(ert-deftest codex-ide-term-vterm-output-filter-preserves-existing-filter ()
  "The vterm output wrapper still delegates to the previous process filter."
  (codex-ide-test--call-with-buffer-process
   (lambda (_buffer process)
     (let (received sent)
       (set-process-filter
        process
        (lambda (_process input)
          (setq received input)))
       (codex-ide-term--vterm-install-output-filter process)
       (cl-letf (((symbol-function 'process-send-string)
                  (lambda (_process string)
                    (setq sent string))))
         (funcall (process-filter process) process "plain output"))
       (should (equal received "plain output"))
       (should-not sent)))))

(ert-deftest codex-ide-term-vterm-output-filter-sends-osc-replies ()
  "The vterm output wrapper sends OSC replies back to the process."
  (codex-ide-test--call-with-buffer-process
   (lambda (_buffer process)
     (let (received sent)
       (set-process-filter
        process
        (lambda (_process input)
          (setq received input)))
       (codex-ide-term--vterm-install-output-filter process)
       (cl-letf (((symbol-function
                   'codex-ide-term--default-face-color-values)
                  (lambda (type)
                    (pcase type
                      ('foreground '(1 2 3))
                      ('background '(4 5 6)))))
                 ((symbol-function 'process-send-string)
                  (lambda (_process string)
                    (setq sent string))))
         (funcall (process-filter process)
                  process "\e]10;?\e\\\e]11;?\e\\"))
       (should (equal received "\e]10;?\e\\\e]11;?\e\\"))
       (should (equal sent
                      "\e]10;rgb:0001/0002/0003\e\\\e]11;rgb:0004/0005/0006\e\\"))))))

(ert-deftest codex-ide-term-vterm-sync-dimensions-delegates ()
  "vterm display sync delegates to vterm's adjustment function."
  (codex-ide-test--call-with-buffer-process
   (lambda (buffer process)
     (let (called)
       (process-put
        process 'adjust-window-size-function
        (lambda (sync-process windows)
          (setq called (list sync-process windows))))
       (save-window-excursion
         (switch-to-buffer buffer)
         (let ((window (selected-window)))
           (codex-ide-term--sync-dimensions buffer window)
           (should (equal called (list process (list window))))))))))

(ert-deftest codex-ide-term-sync-dimensions-does-not-resize-process ()
  "Display sync does not manually resize terminal processes."
  (codex-ide-test--call-with-buffer-process
   (lambda (buffer process)
     (let (resized)
       (process-put process 'adjust-window-size-function
                    (lambda (&rest _args) nil))
       (save-window-excursion
         (switch-to-buffer buffer)
         (cl-letf (((symbol-function 'set-process-window-size)
                    (lambda (&rest _args)
                      (setq resized t))))
           (codex-ide-term--sync-dimensions buffer (selected-window))
           (should-not resized)))))))

(ert-deftest codex-ide-default-buffer-name ()
  "Buffer name follows the `*codex[<basename>]*' shape."
  (should (equal (codex-ide--default-buffer-name "/tmp/foo")
                 "*codex[foo]*"))
  (should (equal (codex-ide--default-buffer-name "/tmp/foo/")
                 "*codex[foo]*")))

(ert-deftest codex-ide-indexed-buffer-name ()
  "Additional same-project sessions get indexed buffer names."
  (should (equal (codex-ide--get-buffer-name "/tmp/foo" 1)
                 "*codex[foo]*"))
  (should (equal (codex-ide--get-buffer-name "/tmp/foo" 2)
                 "*codex[foo]<2>*")))

(ert-deftest codex-ide-display-buffer-function-default ()
  "Codex displays buffers in the selected window by default."
  (should (eq codex-ide-display-buffer-function
              #'pop-to-buffer-same-window)))

(ert-deftest codex-ide-display-buffer-calls-custom-function ()
  "Display helper delegates buffer placement to the configured function."
  (let ((buffer (get-buffer-create " *codex-ide-display-test*"))
        (main (get-buffer-create " *codex-ide-main-test*"))
        called synced)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (cl-letf (((symbol-function 'codex-ide-term--sync-dimensions)
                     (lambda (sync-buffer sync-window)
                       (setq synced (list sync-buffer sync-window)))))
            (let ((codex-ide-display-buffer-function
                   (lambda (buf)
                     (setq called buf)
                     (pop-to-buffer-same-window buf)
                     (selected-window))))
              (let ((window (codex-ide--display-buffer buffer)))
                (should (eq called buffer))
                (should (eq window (selected-window)))
                (should (eq (window-buffer window) buffer))
                (should (equal synced (list buffer window)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main)))))

(ert-deftest codex-ide-display-buffer-uses-buffer-result-window ()
  "A display function may return the displayed buffer instead of a window."
  (let ((buffer (get-buffer-create " *codex-ide-buffer-result-test*"))
        (main (get-buffer-create " *codex-ide-buffer-result-main-test*"))
        synced)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (cl-letf (((symbol-function 'codex-ide-term--sync-dimensions)
                     (lambda (sync-buffer sync-window)
                       (setq synced (list sync-buffer sync-window)))))
            (let ((codex-ide-display-buffer-function
                   (lambda (buf)
                     (switch-to-buffer buf)
                     buf)))
              (let ((window (codex-ide--display-buffer buffer)))
                (should (window-live-p window))
                (should (eq (window-buffer window) buffer))
                (should (equal synced (list buffer window)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main)))))

(ert-deftest codex-ide-display-buffer-selects-existing-window ()
  "Displaying an already visible Codex buffer selects its window."
  (let ((buffer (get-buffer-create " *codex-ide-existing-test*"))
        (main (get-buffer-create " *codex-ide-existing-main-test*"))
        (other (get-buffer-create " *codex-ide-existing-other-test*"))
        called synced)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (split-window-right)
          (other-window 1)
          (switch-to-buffer buffer)
          (let ((codex-window (selected-window)))
            (other-window 1)
            (switch-to-buffer other)
            (cl-letf (((symbol-function 'codex-ide-term--sync-dimensions)
                       (lambda (sync-buffer sync-window)
                         (setq synced (list sync-buffer sync-window)))))
              (let ((codex-ide-display-buffer-function
                     (lambda (_buffer)
                       (setq called t)
                       (selected-window))))
                (let ((window (codex-ide--display-buffer buffer)))
                  (should-not called)
                  (should (eq window codex-window))
                  (should (eq (selected-window) codex-window))
                  (should (equal synced (list buffer codex-window)))
                  (should (= (length (get-buffer-window-list buffer nil t))
                             1)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest codex-ide-display-buffer-syncs-visible-buffer-on-nil-result ()
  "Display sync still runs when a nil result leaves the buffer visible."
  (let ((buffer (get-buffer-create " *codex-ide-nil-result-test*"))
        (main (get-buffer-create " *codex-ide-nil-result-main-test*"))
        synced)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (cl-letf (((symbol-function 'codex-ide-term--sync-dimensions)
                     (lambda (sync-buffer sync-window)
                       (setq synced (list sync-buffer sync-window)))))
            (let ((codex-ide-display-buffer-function
                   (lambda (buf)
                     (switch-to-buffer buf)
                     nil)))
              (let ((window (codex-ide--display-buffer buffer)))
                (should (window-live-p window))
                (should (equal synced (list buffer window)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main)))))

(ert-deftest codex-ide-display-buffer-skips-sync-when-not-visible ()
  "Display sync is skipped when the configured function shows no window."
  (let ((buffer (get-buffer-create " *codex-ide-no-window-test*"))
        called)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'codex-ide-term--sync-dimensions)
                     (lambda (&rest _args)
                       (setq called t))))
            (let ((codex-ide-display-buffer-function
                   (lambda (_buffer) nil)))
              (should-not (codex-ide--display-buffer buffer))
              (should-not called))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-toggle-hides-window-without-killing-buffer ()
  "Toggling a visible Codex window hides it and leaves the buffer alive."
  (let ((buffer (get-buffer-create " *codex-ide-toggle-window-test*"))
        (main (get-buffer-create " *codex-ide-toggle-main-test*"))
        (codex-ide-display-buffer-function #'pop-to-buffer-same-window))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (should (window-live-p (codex-ide--display-buffer buffer)))
          (should (get-buffer-window buffer t))
          (codex-ide--toggle-existing-window buffer)
          (should (buffer-live-p buffer))
          (should-not (get-buffer-window buffer t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main)))))

(ert-deftest codex-ide-display-buffer-updates-last-accessed-buffer ()
  "Display helper records the displayed Codex buffer."
  (let ((buffer (get-buffer-create " *codex-ide-last-accessed-test*"))
        (codex-ide--last-accessed-buffer nil))
    (unwind-protect
        (save-window-excursion
          (codex-ide--display-buffer buffer)
          (should (eq codex-ide--last-accessed-buffer buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-get-working-directory-default ()
  "Without a project, working directory falls back to `default-directory'."
  (let ((default-directory "/tmp/"))
    (should (equal (codex-ide--get-working-directory)
                   (expand-file-name "/tmp/")))))

(ert-deftest codex-ide-get-working-directory-project-root ()
  "Inside a project, working directory is the project root."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-project-" t)))
         (subdir (expand-file-name "sub/" root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (make-directory subdir)
          (let ((default-directory subdir))
            (should (equal (file-truename (codex-ide--get-working-directory))
                           (file-truename root)))))
      (delete-directory root t))))

(ert-deftest codex-ide-session-candidates-empty ()
  "Empty session table produces no session candidates."
  (let ((codex-ide--sessions (make-hash-table :test 'equal))
        (codex-ide--active-session-ids (make-hash-table :test 'equal)))
    (should-not (codex-ide--session-candidates))))

(ert-deftest codex-ide-session-candidates-live ()
  "Live session table entries produce buffer-name candidates."
  (let* ((dir-a (file-name-as-directory
                 (make-temp-file "codex-ide-alpha-" t)))
         (dir-b (file-name-as-directory
                 (make-temp-file "codex-ide-beta-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,dir-a 1) (,dir-b 1))
         (lambda (_sessions)
           (let ((candidates (codex-ide--session-candidates)))
             (should (equal (mapcar #'car candidates)
                            (sort (list (codex-ide--get-buffer-name dir-a)
                                        (codex-ide--get-buffer-name dir-b))
                                  #'string<)))
             (should (equal (sort (mapcar (lambda (candidate)
                                             (plist-get (cdr candidate) :root))
                                           candidates)
                                  #'string<)
                            (sort (list dir-a dir-b) #'string<))))))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest codex-ide-session-candidates-filter-dead-and-missing-buffers ()
  "Only live session entries with live buffers become candidates."
  (let ((live-dir (file-name-as-directory
                   (make-temp-file "codex-ide-live-" t)))
        (dead-dir (file-name-as-directory
                   (make-temp-file "codex-ide-dead-" t)))
        (missing-dir (file-name-as-directory
                      (make-temp-file "codex-ide-missing-" t)))
        (codex-ide--sessions (make-hash-table :test 'equal))
        (codex-ide--active-session-ids (make-hash-table :test 'equal))
        live-process
        dead-process
        missing-process
        dead-buffer
        live-buffer
        missing-buffer)
    (unwind-protect
        (progn
          (setq live-buffer (get-buffer-create
                             (codex-ide--get-buffer-name live-dir)))
          (setq dead-buffer (get-buffer-create
                             (codex-ide--get-buffer-name dead-dir)))
          (setq missing-buffer (get-buffer-create
                                (codex-ide--get-buffer-name missing-dir)))
          (setq live-process (codex-ide-test--make-buffer-process
                              live-buffer "codex-ide-live"))
          (setq dead-process (codex-ide-test--make-buffer-process
                              dead-buffer "codex-ide-dead"))
          (setq missing-process (codex-ide-test--make-buffer-process
                                 missing-buffer "codex-ide-missing"))
          (set-process-query-on-exit-flag live-process nil)
          (set-process-query-on-exit-flag dead-process nil)
          (set-process-query-on-exit-flag missing-process nil)
          (puthash live-dir
                   (list (codex-ide--make-session live-dir 1
                                                   live-buffer live-process))
                   codex-ide--sessions)
          (puthash dead-dir
                   (list (codex-ide--make-session
                          dead-dir 1 dead-buffer dead-process))
                   codex-ide--sessions)
          (puthash missing-dir
                   (list (codex-ide--make-session missing-dir 1
                                                   missing-buffer
                                                   missing-process))
                   codex-ide--sessions)
          (delete-process dead-process)
          (kill-buffer missing-buffer)
          (let ((candidates (codex-ide--session-candidates)))
            (should (equal candidates
                           (list (cons (buffer-name live-buffer)
                                       (codex-ide--make-session
                                        live-dir 1 live-buffer
                                        live-process)))))
            (should-not (gethash dead-dir codex-ide--sessions))
            (should-not (gethash missing-dir codex-ide--sessions))))
      (when (and live-process (process-live-p live-process))
        (delete-process live-process))
      (when (and dead-process (process-live-p dead-process))
        (delete-process dead-process))
      (when (and missing-process (process-live-p missing-process))
        (delete-process missing-process))
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (when (buffer-live-p dead-buffer)
        (kill-buffer dead-buffer))
      (when (buffer-live-p missing-buffer)
        (kill-buffer missing-buffer))
      (delete-directory live-dir t)
      (delete-directory dead-dir t)
      (delete-directory missing-dir t))))

(ert-deftest codex-ide-session-annotation-shows-directory ()
  "Session completion annotations show the abbreviated directory."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "codex-ide-annotation-" t)))
         (session (codex-ide--make-session directory 1 nil nil))
         (candidates `(("buffer" . ,session)))
         (annotation (funcall (codex-ide--session-annotation-function
                               candidates)
                              "buffer")))
    (unwind-protect
        (should (equal (substring-no-properties annotation)
                       (concat "  " (abbreviate-file-name directory))))
      (delete-directory directory t))))

(ert-deftest codex-ide-read-session-uses-annotated-completion ()
  "Session reader uses completing-read with an annotation function."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "codex-ide-read-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         session)
    (unwind-protect
        (progn
          (setq session (codex-ide-test--make-session directory 1))
          (codex-ide-test--store-session session)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     &rest _args)
                       (should (equal prompt "Codex session: "))
                       (should (equal collection
                                      (list (cons
                                             (buffer-name
                                              (plist-get session :buffer))
                                             session))))
                       (should require-match)
                       (should (functionp
                                (plist-get completion-extra-properties
                                           :annotation-function)))
                       (buffer-name (plist-get session :buffer)))))
            (should (eq (codex-ide--read-session) session))))
      (when session
        (codex-ide-test--kill-session session))
      (delete-directory directory t))))

(ert-deftest codex-ide-read-session-uses-default-session ()
  "Session reader supplies DEFAULT-SESSION as completion default."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-read-default-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((default (cl-find 2 sessions
                                   :key (lambda (session)
                                          (plist-get session :id))))
                 seen-default)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt _collection _predicate _require-match
                                         _initial-input _hist def
                                         &rest _args)
                          (setq seen-default def)
                          def)))
               (should (eq (codex-ide--read-session root default)
                           default)))
             (should (equal seen-default
                            (buffer-name (plist-get default :buffer)))))))
      (delete-directory root t))))

(ert-deftest codex-ide-target-session-single-session-does-not-prompt ()
  "Single project session is selected without completion."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-target-single-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (sessions)
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (&rest _args)
                        (error "Unexpected session prompt"))))
             (let ((session (let ((default-directory root))
                              (codex-ide--target-session))))
               (should (eq session (car sessions)))
               (should (= (gethash root codex-ide--active-session-ids)
                          1))))))
      (delete-directory root t))))

(ert-deftest codex-ide-target-session-multiple-sessions-prompts ()
  "Multiple project sessions are selected with project-filtered completion."
  (let ((root-a (file-name-as-directory
                 (make-temp-file "codex-ide-target-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "codex-ide-target-b-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root-a 1) (,root-a 2) (,root-b 1))
         (lambda (_sessions)
           (let (seen)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt collection _predicate _require-match
                                         &rest _args)
                          (setq seen collection)
                          (car (cl-find 1 collection
                                        :key (lambda (candidate)
                                               (plist-get (cdr candidate)
                                                          :id))
                                        :test #'=)))))
               (let ((session (let ((default-directory root-a))
                                (codex-ide--target-session))))
                 (should (= (plist-get session :id) 1))))
             (should (equal (delete-dups
                             (mapcar (lambda (candidate)
                                       (plist-get (cdr candidate) :root))
                                     seen))
                            (list root-a)))
             (should (= (gethash root-a codex-ide--active-session-ids)
                        1)))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest codex-ide-create-session-uses-process-buffer ()
  "Session creation records the actual process buffer."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let* ((requested-buffer (get-buffer-create
                               (codex-ide--get-buffer-name root)))
            (actual-buffer (generate-new-buffer
                            (format "%s<2>"
                                    (codex-ide--get-buffer-name root))))
            requested-name
            process)
       (unwind-protect
           (cl-letf (((symbol-function 'codex-ide-term--make-process)
                      (lambda (buffer-name _program _args _env _working-dir)
                        (setq requested-name buffer-name)
                        (setq process
                              (codex-ide-test--make-buffer-process
                               actual-buffer
                               "codex-ide-process-buffer"))
                        process)))
             (let ((default-directory root))
               (let ((session (codex-ide--create-session 1)))
                 (should (equal requested-name
                                (codex-ide--get-buffer-name root)))
                 (should (eq (plist-get session :buffer) actual-buffer))
                 (should-not
                  (eq (plist-get session :buffer) requested-buffer)))))
         (codex-ide-test--kill-buffer-process actual-buffer process)
         (when (buffer-live-p requested-buffer)
           (kill-buffer requested-buffer)))))))

(ert-deftest codex-ide-recover-live-session-registers-orphan-buffer ()
  "Recovery registers an orphan live Codex buffer without renaming it."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-recover-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "sleep")
         (buffer-name (format "%s<2>" (codex-ide--get-buffer-name root)))
         pair buffer process)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq pair
                (codex-ide-test--make-recoverable-buffer
                 root buffer-name "codex-ide-recover"))
          (setq buffer (car pair))
          (setq process (cadr pair))
          (codex-ide--recover-live-sessions)
          (codex-ide--recover-live-sessions)
          (let* ((sessions (codex-ide--project-sessions root))
                 (session (car sessions)))
            (should (= (length sessions) 1))
            (should (eq (plist-get session :buffer) buffer))
            (should (eq (plist-get session :process) process))
            (should (= (plist-get session :id) 2))
            (should (equal (buffer-name buffer) buffer-name))
            (should-not (process-query-on-exit-flag process))
            (should (functionp (process-sentinel process)))
            (with-current-buffer buffer
              (should (equal codex-ide--session-root root))
              (should (= codex-ide--session-id 2))
              (should (eq (local-key-binding (kbd "S-<return>"))
                          #'codex-ide-insert-newline))
              (should (eq (local-key-binding (kbd "C-<escape>"))
                          #'codex-ide-send-escape)))
            (should (assoc buffer-name
                           (codex-ide--session-candidates root)))
            (should (assoc buffer-name
                           (codex-ide--session-candidates)))))
      (when pair
        (codex-ide-test--kill-buffer-process (car pair) (cadr pair)))
      (delete-directory root t))))

(ert-deftest codex-ide-recovery-ignores-noncodex-processes ()
  "Recovery ignores Codex-named buffers whose command is not Codex."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-ignore-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "codex")
         pair)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq pair
                (codex-ide-test--make-recoverable-buffer
                 root (codex-ide--get-buffer-name root)
                 "codex-ide-ignore"))
          (codex-ide--recover-live-sessions)
          (should-not (codex-ide--project-sessions root)))
      (when pair
        (codex-ide-test--kill-buffer-process (car pair) (cadr pair)))
      (delete-directory root t))))

(ert-deftest codex-ide-recovered-session-commands-target-buffer ()
  "Active-session commands target a recovered Codex buffer."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-recovered-commands-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "sleep")
         pair buffer process displayed sent returned escaped)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq pair
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-recovered-commands"))
          (setq buffer (car pair))
          (setq process (cadr pair))
          (codex-ide--recover-live-sessions)
          (cl-letf (((symbol-function 'codex-ide--display-buffer)
                     (lambda (display-buffer)
                       (setq displayed display-buffer)))
                    ((symbol-function 'codex-ide-context-record-source-buffer)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'codex-ide-term--send-string)
                     (lambda (string)
                       (setq sent
                             (append sent
                                     (list (list string
                                                 (current-buffer)))))))
                    ((symbol-function 'codex-ide-term--send-return)
                     (lambda ()
                       (setq returned
                             (append returned (list (current-buffer))))))
                    ((symbol-function 'codex-ide-term--send-escape)
                     (lambda ()
                       (setq escaped (current-buffer))))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'codex-ide-debug)
                     (lambda (&rest _args) nil)))
            (let ((default-directory root))
              (codex-ide-switch-to-buffer)
              (codex-ide-send-prompt "hello")
              (codex-ide-send-escape)
              (codex-ide-insert-newline)))
          (should (eq displayed buffer))
          (should (equal sent (list (list "hello" buffer)
                                    (list "\\" buffer))))
          (should (equal returned (list buffer buffer)))
          (should (eq escaped buffer)))
      (codex-ide-test--kill-buffer-process buffer process)
      (delete-directory root t))))

(ert-deftest codex-ide-stop-targets-recovered-session ()
  "`codex-ide-stop' kills the active recovered session only."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-recovered-stop-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "sleep")
         first second)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq first
                (codex-ide-test--make-recoverable-buffer
                 root (codex-ide--get-buffer-name root)
                 "codex-ide-stop-1"))
          (setq second
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-stop-2"))
          (codex-ide--recover-live-sessions)
          (codex-ide--activate-session
           (codex-ide--session-by-id root 2))
          (cl-letf (((symbol-function 'codex-ide-log)
                     (lambda (&rest _args) nil)))
            (let ((default-directory root))
              (codex-ide-stop)))
          (should (buffer-live-p (car first)))
          (should-not (buffer-live-p (car second)))
          (should (equal (mapcar (lambda (session)
                                   (plist-get session :id))
                                 (codex-ide--project-sessions root))
                         '(1))))
      (when first
        (codex-ide-test--kill-buffer-process (car first) (cadr first)))
      (when second
        (codex-ide-test--kill-buffer-process (car second) (cadr second)))
      (delete-directory root t))))

(ert-deftest codex-ide-cleanup-recovered-session-preserves-siblings ()
  "Cleanup removes one recovered session and keeps live siblings."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-recovered-cleanup-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "sleep")
         first second)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq first
                (codex-ide-test--make-recoverable-buffer
                 root (codex-ide--get-buffer-name root)
                 "codex-ide-cleanup-1"))
          (setq second
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-cleanup-2"))
          (codex-ide--recover-live-sessions)
          (codex-ide--cleanup-on-exit root 2)
          (should (buffer-live-p (car first)))
          (should-not (buffer-live-p (car second)))
          (should (equal (mapcar (lambda (session)
                                   (plist-get session :id))
                                 (codex-ide--project-sessions root))
                         '(1))))
      (when first
        (codex-ide-test--kill-buffer-process (car first) (cadr first)))
      (when second
        (codex-ide-test--kill-buffer-process (car second) (cadr second)))
      (delete-directory root t))))

(ert-deftest codex-ide-next-session-id-avoids-recovered-ids ()
  "New live session ids skip ids assigned during recovery."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-recovered-next-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "sleep")
         first second)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq first
                (codex-ide-test--make-recoverable-buffer
                 root (codex-ide--get-buffer-name root)
                 "codex-ide-next-1"))
          (setq second
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-next-2"))
          (codex-ide--recover-live-sessions)
          (should (equal (sort (mapcar (lambda (session)
                                          (plist-get session :id))
                                        (codex-ide--project-sessions root))
                               #'<)
                         '(1 2)))
          (should (= (codex-ide--next-session-id root) 3)))
      (when first
        (codex-ide-test--kill-buffer-process (car first) (cadr first)))
      (when second
        (codex-ide-test--kill-buffer-process (car second) (cadr second)))
      (delete-directory root t))))

(ert-deftest codex-ide-prefix-and-new-session-create-indexed-sessions ()
  "`C-u M-x codex-ide' and `codex-ide-new-session' create siblings."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let ((codex-ide--sessions (make-hash-table :test 'equal))
           (codex-ide--active-session-ids (make-hash-table :test 'equal))
           (codex-ide-context-auto-start nil)
           created)
       (unwind-protect
           (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                      (lambda () t))
                     ((symbol-function 'codex-ide--create-session)
                      (lambda (emacs-session-id &rest _args)
                        (let ((session
                               (codex-ide-test--make-session
                                root emacs-session-id)))
                          (push session created)
                          session)))
                     ((symbol-function 'codex-ide--display-buffer)
                      (lambda (_buffer) nil))
                     ((symbol-function 'codex-ide-context-record-source-buffer)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'codex-ide-log)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'codex-ide-debug)
                      (lambda (&rest _args) nil)))
             (let ((default-directory root))
               (codex-ide)
               (codex-ide '(4))
               (codex-ide-new-session))
             (should (equal (sort (mapcar (lambda (session)
                                             (buffer-name
                                              (plist-get session :buffer)))
                                           (codex-ide--project-sessions root))
                                  #'string<)
                            (sort (list (codex-ide--get-buffer-name root 1)
                                        (codex-ide--get-buffer-name root 2)
                                        (codex-ide--get-buffer-name root 3))
                                  #'string<)))
             (should (= (gethash root codex-ide--active-session-ids) 3)))
         (mapc #'codex-ide-test--kill-session created))))))

(ert-deftest codex-ide-without-prefix-toggles-active-session ()
  "`codex-ide' without prefix toggles the active project session."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-toggle-active-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((active (cl-find 2 sessions
                                  :key (lambda (session)
                                         (plist-get session :id))))
                 created toggled)
             (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                        (lambda () t))
                       ((symbol-function 'codex-ide--create-session)
                        (lambda (&rest _args)
                          (setq created t)))
                       ((symbol-function 'codex-ide--toggle-existing-window)
                        (lambda (buffer)
                          (setq toggled buffer)))
                       ((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide)))
             (should-not created)
             (should (eq toggled (plist-get active :buffer))))))
      (delete-directory root t))))

(ert-deftest codex-ide-toggle-shows-active-session-when-hidden ()
  "`codex-ide-toggle' shows the active project session when none is visible."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cycle-hidden-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((main (generate-new-buffer " *codex-ide-cycle-main*"))
                 (active (codex-ide-test--session-by-id sessions 2)))
             (unwind-protect
                 (save-window-excursion
                   (delete-other-windows)
                   (switch-to-buffer main)
                   (codex-ide-test--toggle-in-root root)
                   (should (eq (window-buffer) (plist-get active :buffer)))
                   (should (= (gethash root codex-ide--active-session-ids) 2)))
               (when (buffer-live-p main)
                 (kill-buffer main))))))
      (delete-directory root t))))

(ert-deftest codex-ide-toggle-hidden-state-falls-back-to-first-session ()
  "`codex-ide-toggle' uses the first sorted session when no active id exists."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cycle-fallback-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((main (generate-new-buffer " *codex-ide-cycle-fallback*"))
                 (first (codex-ide-test--session-by-id sessions 1)))
             (remhash root codex-ide--active-session-ids)
             (unwind-protect
                 (save-window-excursion
                   (delete-other-windows)
                   (switch-to-buffer main)
                   (codex-ide-test--toggle-in-root root)
                   (should (eq (window-buffer) (plist-get first :buffer)))
                   (should (= (gethash root codex-ide--active-session-ids) 1)))
               (when (buffer-live-p main)
                 (kill-buffer main))))))
      (delete-directory root t))))

(ert-deftest codex-ide-toggle-cycles-visible-session-to-next ()
  "`codex-ide-toggle' cycles a visible project session to the next id."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cycle-next-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((first (codex-ide-test--session-by-id sessions 1))
                 (second (codex-ide-test--session-by-id sessions 2)))
             (save-window-excursion
               (delete-other-windows)
               (switch-to-buffer (plist-get first :buffer))
               (codex-ide-test--toggle-in-root root)
               (should (eq (window-buffer) (plist-get second :buffer)))
               (should-not (codex-ide-test--session-visible-p first))
               (should (= (gethash root codex-ide--active-session-ids) 2))))))
      (delete-directory root t))))

(ert-deftest codex-ide-toggle-hides-last-session-then-shows-first ()
  "`codex-ide-toggle' hides after the last session and next shows the first."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cycle-wrap-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((first (codex-ide-test--session-by-id sessions 1))
                 (second (codex-ide-test--session-by-id sessions 2)))
             (save-window-excursion
               (delete-other-windows)
               (switch-to-buffer (plist-get second :buffer))
               (codex-ide-test--call-with-toggle-stubs
                (lambda ()
                  (let ((default-directory root)
                        (codex-ide-display-buffer-function
                         #'pop-to-buffer-same-window))
                    (codex-ide-toggle)
                    (should-not (cl-some #'codex-ide-test--session-visible-p
                                         sessions))
                    (dolist (session sessions)
                      (should (buffer-live-p (plist-get session :buffer)))
                      (should (process-live-p (plist-get session :process))))
                    (should (= (gethash root codex-ide--active-session-ids) 1))
                    (codex-ide-toggle))))
               (should (eq (window-buffer) (plist-get first :buffer)))
               (should (= (gethash root codex-ide--active-session-ids) 1))))))
      (delete-directory root t))))

(ert-deftest codex-ide-toggle-ignores-visible-sessions-from-other-roots ()
  "`codex-ide-toggle' ignores visible Codex windows from other projects."
  (let ((root-a (file-name-as-directory
                 (make-temp-file "codex-ide-cycle-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "codex-ide-cycle-b-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root-a 1) (,root-b 1))
         (lambda (sessions)
           (let ((main (generate-new-buffer " *codex-ide-cycle-other*"))
                 (session-a (codex-ide-test--session-by-root sessions root-a))
                 (session-b (codex-ide-test--session-by-root sessions root-b)))
             (unwind-protect
                 (save-window-excursion
                   (delete-other-windows)
                   (switch-to-buffer main)
                   (split-window-right)
                   (other-window 1)
                   (switch-to-buffer (plist-get session-b :buffer))
                   (let ((other-window (selected-window)))
                     (other-window -1)
                     (codex-ide-test--toggle-in-root root-a)
                     (should (eq (window-buffer)
                                 (plist-get session-a :buffer)))
                     (should (eq (window-buffer other-window)
                                 (plist-get session-b :buffer)))))
               (when (buffer-live-p main)
                 (kill-buffer main))))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest codex-ide-toggle-without-project-sessions-errors ()
  "`codex-ide-toggle' preserves the no-session user error."
  (let ((root-a (file-name-as-directory
                 (make-temp-file "codex-ide-cycle-empty-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "codex-ide-cycle-empty-b-" t))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root-a))
          (codex-ide-test--call-with-sessions
           `((,root-b 1))
           (lambda (_sessions)
             (condition-case err
                 (progn
                   (codex-ide-test--toggle-in-root root-a)
                   (ert-fail "Expected user-error"))
               (user-error
                (should (equal (cadr err)
                               "No Codex session for this project")))))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest codex-ide-list-project-sessions-filters-current-root ()
  "Project session listing offers only sessions for the current root."
  (let ((root-a (file-name-as-directory
                 (make-temp-file "codex-ide-project-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "codex-ide-project-b-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root-a 1) (,root-a 2) (,root-b 1))
         (lambda (_sessions)
           (let (seen)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt collection _predicate _require-match
                                         &rest _args)
                          (setq seen collection)
                          (car (cl-find 2 collection
                                        :key (lambda (candidate)
                                               (plist-get (cdr candidate)
                                                          :id))
                                        :test #'=))))
                       ((symbol-function 'codex-ide--display-buffer)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root-a))
                 (codex-ide-list-project-sessions)))
             (should (equal (delete-dups
                             (mapcar (lambda (candidate)
                                       (plist-get (cdr candidate) :root))
                                     seen))
                            (list root-a)))
             (should (= (gethash root-a codex-ide--active-session-ids) 2)))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest codex-ide-list-sessions-offers-all-roots ()
  "All-session listing offers live sessions from every root."
  (let ((root-a (file-name-as-directory
                 (make-temp-file "codex-ide-all-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "codex-ide-all-b-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root-a 1) (,root-b 1))
         (lambda (_sessions)
           (let (seen)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt collection _predicate _require-match
                                         &rest _args)
                          (setq seen collection)
                          (car (cl-find root-b collection
                                        :key (lambda (candidate)
                                               (plist-get (cdr candidate)
                                                          :root))
                                        :test #'equal))))
                       ((symbol-function 'codex-ide--display-buffer)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root-a))
                 (codex-ide-list-sessions)))
             (should (equal (sort (mapcar (lambda (candidate)
                                             (plist-get (cdr candidate)
                                                        :root))
                                           seen)
                                  #'string<)
                            (sort (list root-a root-b) #'string<)))
             (should (= (gethash root-b codex-ide--active-session-ids) 1)))))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest codex-ide-send-prompt-prompts-for-multiple-sessions ()
  "Prompt sending writes to the selected project session."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-prompt-target-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((target (cl-find 1 sessions
                                  :key (lambda (session)
                                         (plist-get session :id))))
                 sent returned)
             (cl-letf (((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'completing-read)
                        (lambda (_prompt collection _predicate _require-match
                                         &rest _args)
                          (car (cl-find 1 collection
                                        :key (lambda (candidate)
                                               (plist-get (cdr candidate)
                                                          :id))
                                        :test #'=))))
                       ((symbol-function 'codex-ide-term--send-string)
                        (lambda (string)
                          (setq sent (list string (current-buffer)))))
                       ((symbol-function 'codex-ide-term--send-return)
                        (lambda ()
                          (setq returned (current-buffer))))
                       ((symbol-function 'sit-for)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'codex-ide-debug)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide-send-prompt "active")))
             (should (equal sent (list "active"
                                       (plist-get target :buffer))))
             (should (eq returned (plist-get target :buffer)))
             (should (= (gethash root codex-ide--active-session-ids)
                        1)))))
      (delete-directory root t))))

(ert-deftest codex-ide-terminal-input-prompts-for-multiple-sessions ()
  "Escape and newline commands write to the selected project session."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-input-target-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((target (cl-find 1 sessions
                                  :key (lambda (session)
                                         (plist-get session :id))))
                 sent returned escaped)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt collection _predicate _require-match
                                         &rest _args)
                          (car (cl-find 1 collection
                                        :key (lambda (candidate)
                                               (plist-get (cdr candidate)
                                                          :id))
                                        :test #'=))))
                       ((symbol-function 'codex-ide-term--send-string)
                        (lambda (string)
                          (setq sent
                                (append sent
                                        (list (list string
                                                    (current-buffer)))))))
                       ((symbol-function 'codex-ide-term--send-return)
                        (lambda ()
                          (setq returned
                                (append returned
                                        (list (current-buffer))))))
                       ((symbol-function 'codex-ide-term--send-escape)
                        (lambda ()
                          (setq escaped (current-buffer))))
                       ((symbol-function 'sit-for)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide-send-escape)
                 (codex-ide-insert-newline)))
             (should (eq escaped (plist-get target :buffer)))
             (should (equal sent
                            (list (list "\\" (plist-get target :buffer)))))
             (should (equal returned
                            (list (plist-get target :buffer))))
             (should (= (gethash root codex-ide--active-session-ids)
                        1)))))
      (delete-directory root t))))

(ert-deftest codex-ide-terminal-input-targets-own-session ()
  "Escape from a session buffer targets it without prompting."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-input-own-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((by-id (lambda (id)
                          (cl-find id sessions
                                   :key (lambda (session)
                                          (plist-get session :id))
                                   :test #'=)))
                 escaped)
             (codex-ide--activate-session (funcall by-id 1))
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _args)
                          (error "Unexpected session prompt")))
                       ((symbol-function 'codex-ide-term--send-escape)
                        (lambda ()
                          (setq escaped (current-buffer)))))
               (with-current-buffer (plist-get (funcall by-id 2) :buffer)
                 (let ((default-directory root))
                   (codex-ide-send-escape))))
             (should (eq escaped (plist-get (funcall by-id 2) :buffer)))
             (should (= (gethash root codex-ide--active-session-ids)
                        2)))))
      (delete-directory root t))))

(ert-deftest codex-ide-stop-targets-active-session ()
  "`codex-ide-stop' kills only the active project session buffer."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-stop-active-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((inactive (cl-find 1 sessions
                                    :key (lambda (session)
                                           (plist-get session :id))))
                 (active (cl-find 2 sessions
                                  :key (lambda (session)
                                         (plist-get session :id)))))
             (cl-letf (((symbol-function 'codex-ide-log)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide-stop)))
             (should (buffer-live-p (plist-get inactive :buffer)))
             (should-not (buffer-live-p (plist-get active :buffer))))))
      (delete-directory root t))))

(ert-deftest codex-ide-stop-leaves-context-provider-running ()
  "`codex-ide-stop' does not stop the user-wide context provider."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-stop-context-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (_sessions)
           (let ((codex-ide-context-mode t)
                 stopped-provider)
             (cl-letf (((symbol-function 'codex-ide-context-stop)
                        (lambda ()
                          (setq stopped-provider t)))
                       ((symbol-function 'codex-ide-log)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide-stop)))
             (should codex-ide-context-mode)
             (should-not stopped-provider))))
      (delete-directory root t))))

(ert-deftest codex-ide-cleanup-removes-only-dead-session ()
  "Cleanup removes one dead session and preserves live siblings."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cleanup-active-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let ((kept (cl-find 1 sessions
                                :key (lambda (session)
                                       (plist-get session :id)))))
             (codex-ide--cleanup-on-exit root 2)
             (should (equal (mapcar (lambda (session)
                                      (plist-get session :id))
                                    (codex-ide--project-sessions root))
                            '(1)))
             (should (buffer-live-p (plist-get kept :buffer)))
             (should (= (gethash root codex-ide--active-session-ids) 1)))))
      (delete-directory root t))))

(ert-deftest codex-ide-cleanup-accepts-buffer-argument ()
  "Cleanup tolerates hook-style calls with the session buffer."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cleanup-buffer-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (sessions)
           (let ((session (car sessions)))
             (codex-ide--cleanup-on-exit (plist-get session :buffer))
             (should-not (buffer-live-p (plist-get session :buffer)))
             (should-not (codex-ide--project-sessions root)))))
      (delete-directory root t))))

(ert-deftest codex-ide-cleanup-accepts-process-argument ()
  "Cleanup tolerates sentinel-style calls with only the process."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cleanup-process-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (sessions)
           (let ((session (car sessions)))
             (codex-ide--cleanup-on-exit (plist-get session :process))
             (should-not (buffer-live-p (plist-get session :buffer)))
             (should-not (codex-ide--project-sessions root)))))
      (delete-directory root t))))

(ert-deftest codex-ide-setup-session-removes-stale-cleanup-hook ()
  "Session setup removes obsolete cleanup hook function objects."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-cleanup-stale-hook-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (sessions)
           (let* ((session (car sessions))
                  (buffer (plist-get session :buffer))
                  (stale (symbol-function 'codex-ide--cleanup-on-exit)))
             (with-current-buffer buffer
               (add-hook 'kill-buffer-hook stale nil t))
             (codex-ide--setup-session session)
             (with-current-buffer buffer
               (should-not (memq stale kill-buffer-hook))
               (should (memq #'codex-ide--cleanup-current-buffer-session
                             kill-buffer-hook))))))
      (delete-directory root t))))

;;; Native IDE context

(ert-deftest codex-ide-start-session-enables-context-provider-for-new-session ()
  "New sessions enable the IDE context provider when auto-start is enabled."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let* ((codex-ide--sessions (make-hash-table :test 'equal))
            (codex-ide--active-session-ids (make-hash-table :test 'equal))
            (codex-ide-context-auto-start t)
            (buffer (get-buffer-create (codex-ide--get-buffer-name root)))
            (process nil)
            (enabled 0))
       (unwind-protect
           (progn
             (setq process
                   (codex-ide-test--make-buffer-process
                    buffer "codex-ide-context-start"))
             (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                        (lambda () t))
                       ((symbol-function 'codex-ide-context-mode)
                        (lambda (arg)
                          (when (> (prefix-numeric-value arg) 0)
                            (setq enabled (1+ enabled)))))
                       ((symbol-function 'codex-ide--create-session)
                        (lambda (emacs-session-id &rest _args)
                          (codex-ide--make-session
                           root emacs-session-id buffer process)))
                       ((symbol-function 'codex-ide--display-buffer)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'codex-ide-log)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'codex-ide-debug)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide--start-session)))
             (should (= enabled 1)))
         (when (and process (process-live-p process))
           (delete-process process))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest codex-ide-start-session-enables-context-provider-once-for-active-session ()
  "Auto-start does not re-enable context when toggling an existing session."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let* ((codex-ide--sessions (make-hash-table :test 'equal))
            (codex-ide--active-session-ids (make-hash-table :test 'equal))
            (codex-ide-context-auto-start t)
            (buffer (get-buffer-create (codex-ide--get-buffer-name root)))
            (process nil)
            (enabled 0)
            (created 0))
       (unwind-protect
           (progn
             (setq process
                   (codex-ide-test--make-buffer-process
                    buffer "codex-ide-context-existing"))
             (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                        (lambda () t))
                       ((symbol-function 'codex-ide-context-mode)
                        (lambda (arg)
                          (when (> (prefix-numeric-value arg) 0)
                            (setq enabled (1+ enabled)))))
                       ((symbol-function 'codex-ide--create-session)
                        (lambda (emacs-session-id &rest _args)
                          (setq created (1+ created))
                          (codex-ide--make-session
                           root emacs-session-id buffer process)))
                       ((symbol-function 'codex-ide--display-buffer)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide--toggle-existing-window)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'codex-ide-log)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'codex-ide-debug)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide--start-session)
                 (codex-ide--start-session)))
             (should (= enabled 1))
             (should (= created 1)))
         (when (and process (process-live-p process))
           (delete-process process))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest codex-ide-start-session-does-not-send-ide-command ()
  "New sessions do not schedule `/ide on'; users enable it in Codex."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let* ((codex-ide--sessions (make-hash-table :test 'equal))
            (codex-ide--active-session-ids (make-hash-table :test 'equal))
            (codex-ide-context-auto-start nil)
            (buffer (get-buffer-create (codex-ide--get-buffer-name root)))
            (process nil)
            (scheduled nil)
            (sent nil)
            (created 0))
       (unwind-protect
           (progn
             (setq process
                   (codex-ide-test--make-buffer-process
                    buffer "codex-ide-context-schedule"))
             (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                        (lambda () t))
                       ((symbol-function 'codex-ide--create-session)
                        (lambda (emacs-session-id &rest _args)
                          (setq created (1+ created))
                          (codex-ide--make-session
                           root emacs-session-id buffer process)))
                       ((symbol-function 'codex-ide--display-buffer)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide--toggle-existing-window)
                        (lambda (_buffer) nil))
                       ((symbol-function 'codex-ide-context-record-source-buffer)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'run-at-time)
                        (lambda (&rest args)
                          (push args scheduled)
                          'timer))
                       ((symbol-function 'codex-ide-term--send-string)
                        (lambda (string)
                          (push string sent)))
                       ((symbol-function 'codex-ide-term--send-return)
                        (lambda ()
                          (push :return sent)))
                       ((symbol-function 'codex-ide-log)
                        (lambda (&rest _args) nil))
                       ((symbol-function 'codex-ide-debug)
                        (lambda (&rest _args) nil)))
               (let ((default-directory root))
                 (codex-ide--start-session)
                 (codex-ide--start-session)))
             (should-not scheduled)
             (should-not sent)
             (should (= created 1)))
         (when (and process (process-live-p process))
           (delete-process process))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest codex-ide-send-prompt-records-caller-source-buffer ()
  "`codex-ide-send-prompt' records the source before terminal input."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let* ((source-file (expand-file-name "source.el" root))
            (codex-ide--sessions (make-hash-table :test 'equal))
            (codex-ide--active-session-ids (make-hash-table :test 'equal))
            (source-buffer nil)
            (codex-buffer (get-buffer-create
                           (codex-ide--get-buffer-name root)))
            (process nil)
            (session nil)
            recorded sent returned)
       (write-region "(message \"hi\")\n" nil source-file)
       (unwind-protect
           (progn
             (setq process
                   (codex-ide-test--make-buffer-process
                    codex-buffer "codex-ide-prompt"))
             (setq session (codex-ide--make-session
                            root 1 codex-buffer process))
             (codex-ide-test--store-session session)
             (setq source-buffer (find-file-noselect source-file))
             (with-current-buffer source-buffer
               (let ((default-directory root))
                 (cl-letf (((symbol-function 'codex-ide-context-record-source-buffer)
                            (lambda (directory buffer)
                              (setq recorded (list directory buffer))))
                           ((symbol-function 'codex-ide-term--send-string)
                            (lambda (string)
                              (setq sent (list string (current-buffer)))))
                           ((symbol-function 'codex-ide-term--send-return)
                            (lambda ()
                              (setq returned (current-buffer))))
                           ((symbol-function 'sit-for)
                            (lambda (&rest _args) nil))
                           ((symbol-function 'codex-ide-debug)
                            (lambda (&rest _args) nil)))
                   (codex-ide-send-prompt "hello"))))
             (should (equal recorded (list root source-buffer)))
             (should (equal sent (list "hello" codex-buffer)))
             (should (eq returned codex-buffer)))
         (when (buffer-live-p source-buffer)
           (kill-buffer source-buffer))
         (when (and process (process-live-p process))
           (delete-process process))
         (when (buffer-live-p codex-buffer)
           (kill-buffer codex-buffer))
         (delete-file source-file))))))

;;; Menu

(defun codex-ide-test--popup-entries (keymap)
  "Return popup metadata entries for KEYMAP."
  (let ((rows (keymap-popup--meta keymap 'descriptions)))
    (cl-loop for row in rows
             append (cl-loop for group in row
                             append (plist-get group :entries)))))

(defun codex-ide-test--popup-entry-by-command (keymap command)
  "Return the popup entry for COMMAND in KEYMAP."
  (cl-find command (codex-ide-test--popup-entries keymap)
           :key (lambda (entry) (plist-get entry :command))
           :test #'eq))

(defun codex-ide-test--command-bound-p (keymap command)
  "Return non-nil when COMMAND is reachable in KEYMAP."
  (where-is-internal command (list keymap) t))

(ert-deftest codex-ide-menu-main-commands ()
  "Main menu exposes the core session/navigation/interaction commands."
  (dolist (command '(codex-ide
                     codex-ide-resume-last
                     codex-ide-resume
                     codex-ide-stop
                     codex-ide-switch-to-buffer
                     codex-ide-list-project-sessions
                     codex-ide-list-sessions
                     codex-ide-toggle
                     codex-ide-send-prompt
                     codex-ide-send-escape
                     codex-ide-insert-newline))
    (should (codex-ide-test--command-bound-p codex-ide-map command))))

(ert-deftest codex-ide-menu-config-commands ()
  "Config menu exposes package configuration commands."
  (dolist (command '(codex-ide-menu--set-cli-path
                     codex-ide-menu--set-approval
                     codex-ide-menu--toggle-no-alt-screen
                     codex-ide-menu--save-config))
    (should (codex-ide-test--command-bound-p codex-ide-config-map command))))

(ert-deftest codex-ide-menu-save-config-saves-current-symbols ()
  "Save config persists current configuration."
  (let (saved)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol _value)
                 (push symbol saved)))
              ((symbol-function 'codex-ide-log)
               (lambda (&rest _args) nil)))
      (codex-ide-menu--save-config))
    (should (equal (reverse saved)
                   '(codex-ide-cli-path
                     codex-ide-display-buffer-function
                     codex-ide-ask-for-approval
                     codex-ide-no-alt-screen)))))

(ert-deftest codex-ide-menu-debug-commands ()
  "Debug menu exposes status, toggle, and log commands."
  (dolist (command '(codex-ide-check-status
                     codex-ide-menu--toggle-debug-mode
                     codex-ide-show-debug
                     codex-ide-clear-debug))
    (should (codex-ide-test--command-bound-p codex-ide-debug-map command))))

(ert-deftest codex-ide-menu-submenus-exist ()
  "Config and debug submenus are defined keymaps."
  (should (keymapp codex-ide-config-map))
  (should (keymapp codex-ide-debug-map)))

(ert-deftest codex-ide-menu-no-alt-screen-description-is-dynamic ()
  "Toggle entries resolve their description from current variable state."
  (let* ((entry (codex-ide-test--popup-entry-by-command
                 codex-ide-config-map
                 #'codex-ide-menu--toggle-no-alt-screen))
         (desc-fn (plist-get entry :description)))
    (should (functionp desc-fn))
    (let ((codex-ide-no-alt-screen t))
      (should (string-match-p "ON" (funcall desc-fn))))
    (let ((codex-ide-no-alt-screen nil))
      (should (string-match-p "OFF" (funcall desc-fn))))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
