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
        (codex-ide-cli-extra-args nil))
    (funcall body)))

(defun codex-ide-test--make-process (name)
  "Return a live test process named NAME."
  (unless (executable-find "sleep")
    (ert-skip "sleep executable not found"))
  (start-process name nil "sleep" "60"))

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

(defun codex-ide-test--call-with-process-table (directories body)
  "Call BODY with `codex-ide--processes' populated for DIRECTORIES.
BODY is called with the live processes in the same order as DIRECTORIES."
  (let ((codex-ide--processes (make-hash-table :test 'equal))
        (processes nil))
    (unwind-protect
        (progn
          (dolist (directory directories)
            (let ((process (codex-ide-test--make-process "codex-ide-test")))
              (push process processes)
              (puthash directory process codex-ide--processes)))
          (funcall body (reverse processes)))
      (mapc (lambda (process)
              (when (process-live-p process)
                (delete-process process)))
            processes))))

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

(ert-deftest codex-ide-window-option-defaults ()
  "Window customization defaults match the documented side-window setup."
  (should (eq codex-ide-window-side 'right))
  (should (= codex-ide-window-width 100))
  (should (= codex-ide-window-height 20))
  (should codex-ide-use-side-window)
  (should codex-ide-focus-on-open))

(ert-deftest codex-ide-display-buffer-side-window-focuses ()
  "Side-window display selects and sizes Codex when focus is enabled."
  (let ((buffer (get-buffer-create " *codex-ide-display-test*"))
        (main (get-buffer-create " *codex-ide-main-test*"))
        (codex-ide-window-side 'right)
        (codex-ide-window-width 24)
        (codex-ide-use-side-window t)
        (codex-ide-focus-on-open t))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (let ((window (codex-ide--display-buffer buffer)))
            (should (window-live-p window))
            (should (eq (selected-window) window))
            (should (eq (window-buffer window) buffer))
            (should (eq (window-parameter window 'window-side) 'right))
            (should (eq (window-dedicated-p window) 'side))
            (should (= (window-body-width window)
                       codex-ide-window-width))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main)))))

(ert-deftest codex-ide-display-buffer-selects-existing-window ()
  "Displaying an already visible Codex buffer selects its window."
  (let ((buffer (get-buffer-create " *codex-ide-existing-test*"))
        (main (get-buffer-create " *codex-ide-existing-main-test*"))
        (codex-ide-window-side 'right)
        (codex-ide-window-width 24)
        (codex-ide-use-side-window t)
        (codex-ide-focus-on-open nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (let ((window (codex-ide--display-buffer buffer)))
            (should-not (eq (selected-window) window))
            (codex-ide--display-buffer buffer)
            (should (eq (selected-window) window))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p main)
        (kill-buffer main)))))

(ert-deftest codex-ide-display-buffer-top-bottom-height ()
  "Top and bottom side windows use `codex-ide-window-height'."
  (dolist (side '(top bottom))
    (let ((buffer (get-buffer-create
                   (format " *codex-ide-%s-height-test*" side)))
          (main (get-buffer-create " *codex-ide-main-height-test*"))
          (codex-ide-window-side side)
          (codex-ide-window-height 6)
          (codex-ide-use-side-window t)
          (codex-ide-focus-on-open nil))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer main)
            (let ((window (codex-ide--display-buffer buffer)))
              (should (window-live-p window))
              (should (eq (window-parameter window 'window-side) side))
              (should (eq (window-dedicated-p window) 'side))
              (should (= (window-body-height window)
                         codex-ide-window-height))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (buffer-live-p main)
          (kill-buffer main))))))

(ert-deftest codex-ide-toggle-hides-window-without-killing-buffer ()
  "Toggling a visible Codex window hides it and leaves the buffer alive."
  (let ((buffer (get-buffer-create " *codex-ide-toggle-window-test*"))
        (main (get-buffer-create " *codex-ide-toggle-main-test*"))
        (codex-ide-window-side 'right)
        (codex-ide-window-width 24)
        (codex-ide-use-side-window t)
        (codex-ide-focus-on-open nil))
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
        (codex-ide--last-accessed-buffer nil)
        (codex-ide-use-side-window nil))
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
  "Empty process table produces no session candidates."
  (let ((codex-ide--processes (make-hash-table :test 'equal)))
    (should-not (codex-ide--session-candidates))))

(ert-deftest codex-ide-session-candidates-live ()
  "Live process table entries produce buffer-name candidates."
  (let* ((dir-a (file-name-as-directory
                 (make-temp-file "codex-ide-alpha-" t)))
         (dir-b (file-name-as-directory
                 (make-temp-file "codex-ide-beta-" t))))
    (unwind-protect
        (codex-ide-test--call-with-process-table
         (list dir-a dir-b)
         (lambda (_processes)
           (let ((buffer-a (get-buffer-create (codex-ide--get-buffer-name dir-a)))
                 (buffer-b (get-buffer-create (codex-ide--get-buffer-name dir-b))))
             (unwind-protect
                 (should (equal (codex-ide--session-candidates)
                                (list (cons (buffer-name buffer-a) dir-a)
                                      (cons (buffer-name buffer-b) dir-b))))
               (kill-buffer buffer-a)
               (kill-buffer buffer-b)))))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest codex-ide-session-candidates-filter-dead-and-missing-buffers ()
  "Only live process entries with live buffers become candidates."
  (let ((live-dir (file-name-as-directory
                   (make-temp-file "codex-ide-live-" t)))
        (dead-dir (file-name-as-directory
                   (make-temp-file "codex-ide-dead-" t)))
        (missing-dir (file-name-as-directory
                      (make-temp-file "codex-ide-missing-" t)))
        (codex-ide--processes (make-hash-table :test 'equal))
        live-process
        dead-process
        missing-process
        live-buffer)
    (unwind-protect
        (progn
          (setq live-process (codex-ide-test--make-process "codex-ide-live"))
          (setq dead-process (codex-ide-test--make-process "codex-ide-dead"))
          (setq missing-process (codex-ide-test--make-process "codex-ide-missing"))
          (setq live-buffer (get-buffer-create
                             (codex-ide--get-buffer-name live-dir)))
          (puthash live-dir live-process codex-ide--processes)
          (puthash dead-dir dead-process codex-ide--processes)
          (puthash missing-dir missing-process codex-ide--processes)
          (delete-process dead-process)
          (let ((candidates (codex-ide--session-candidates)))
            (should (equal candidates
                           (list (cons (buffer-name live-buffer) live-dir))))
            (should-not (gethash dead-dir codex-ide--processes))))
      (when (and live-process (process-live-p live-process))
        (delete-process live-process))
      (when (and dead-process (process-live-p dead-process))
        (delete-process dead-process))
      (when (and missing-process (process-live-p missing-process))
        (delete-process missing-process))
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (delete-directory live-dir t)
      (delete-directory dead-dir t)
      (delete-directory missing-dir t))))

(ert-deftest codex-ide-session-annotation-shows-directory ()
  "Session completion annotations show the abbreviated directory."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "codex-ide-annotation-" t)))
         (candidates `(("buffer" . ,directory)))
         (annotation (funcall (codex-ide--session-annotation-function
                               candidates)
                              "buffer")))
    (unwind-protect
        (should (equal (substring-no-properties annotation)
                       (concat "  " (abbreviate-file-name directory))))
      (delete-directory directory t))))

(ert-deftest codex-ide-read-session-directory-uses-annotated-completion ()
  "Session reader uses completing-read with an annotation function."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "codex-ide-read-" t)))
         (buffer (get-buffer-create (codex-ide--get-buffer-name directory)))
         (codex-ide--processes (make-hash-table :test 'equal))
         process)
    (unwind-protect
        (progn
          (setq process (codex-ide-test--make-process "codex-ide-read"))
          (puthash directory process codex-ide--processes)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection _predicate require-match
                                     &rest _args)
                       (should (equal prompt "Codex session: "))
                       (should (equal collection
                                      (list (cons (buffer-name buffer)
                                                  directory))))
                       (should require-match)
                       (should (functionp
                                (plist-get completion-extra-properties
                                           :annotation-function)))
                       (buffer-name buffer))))
            (should (equal (codex-ide--read-session-directory)
                           directory))))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

;;; Menu

(defun codex-ide-test--popup-keys (keymap)
  "Return the list of binding keys in KEYMAP's popup descriptions."
  (let ((rows (keymap-popup--meta keymap 'descriptions)))
    (cl-loop for row in rows
             append (cl-loop for group in row
                             append (mapcar (lambda (entry)
                                              (plist-get entry :key))
                                            (plist-get group :entries))))))

(ert-deftest codex-ide-menu-main-bindings ()
  "Main menu binds the core session/navigation/interaction commands."
  (should (eq (keymap-lookup codex-ide-map "s") #'codex-ide))
  (should (eq (keymap-lookup codex-ide-map "q") #'codex-ide-stop))
  (should (eq (keymap-lookup codex-ide-map "b") #'codex-ide-switch-to-buffer))
  (should (eq (keymap-lookup codex-ide-map "l") #'codex-ide-list-sessions))
  (should (eq (keymap-lookup codex-ide-map "p") #'codex-ide-send-prompt))
  (dolist (key '("s" "r" "R" "q" "b" "l" "w" "p" "e" "n" "C" "d"))
    (should (member key (codex-ide-test--popup-keys codex-ide-map)))))

(ert-deftest codex-ide-menu-config-bindings ()
  "Config menu binds set/toggle suffixes and the save command."
  (should (eq (keymap-lookup codex-ide-config-map "S")
              #'codex-ide-menu--save-config))
  (should (eq (keymap-lookup codex-ide-config-map "u")
              #'codex-ide-menu--toggle-use-side-window))
  (dolist (key '("s" "w" "h" "u" "f" "p" "a" "A" "S"))
    (should (member key (codex-ide-test--popup-keys codex-ide-config-map)))))

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
                     codex-ide-window-side
                     codex-ide-window-width
                     codex-ide-window-height
                     codex-ide-use-side-window
                     codex-ide-focus-on-open
                     codex-ide-ask-for-approval
                     codex-ide-no-alt-screen)))))

(ert-deftest codex-ide-menu-debug-bindings ()
  "Debug menu binds status, toggle, and log commands."
  (should (eq (keymap-lookup codex-ide-debug-map "S")
              #'codex-ide-check-status))
  (should (eq (keymap-lookup codex-ide-debug-map "d")
              #'codex-ide-menu--toggle-debug-mode))
  (dolist (key '("S" "d" "l" "c"))
    (should (member key (codex-ide-test--popup-keys codex-ide-debug-map)))))

(ert-deftest codex-ide-menu-submenu-navigation ()
  "Main menu enters config/debug submenus, not their commands directly."
  (should (eq (keymap-lookup codex-ide-map "C")
              #'codex-ide-map--enter-codex-ide-config-map))
  (should (eq (keymap-lookup codex-ide-map "d")
              #'codex-ide-map--enter-codex-ide-debug-map)))

(ert-deftest codex-ide-menu-use-side-window-description-is-dynamic ()
  "Toggle entries resolve their description from current variable state."
  (let* ((rows (keymap-popup--meta codex-ide-config-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows))
         (entry (cl-find "u" entries
                         :key (lambda (e) (plist-get e :key))
                         :test #'equal))
         (desc-fn (plist-get entry :description)))
    (should (functionp desc-fn))
    (let ((codex-ide-use-side-window t))
      (should (string-match-p "ON" (funcall desc-fn))))
    (let ((codex-ide-use-side-window nil))
      (should (string-match-p "OFF" (funcall desc-fn))))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
