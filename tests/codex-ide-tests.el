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
;; network, no live Codex process, and no eat/vterm loaded.  Backend
;; resolution tests exercise the dispatch table, not real terminal backends.

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

(ert-deftest codex-ide-term-resolve-backend-vterm ()
  "vterm resolves to a descriptor implementing the four ops."
  (let ((desc (codex-ide-term--resolve-backend 'vterm)))
    (should (plist-get desc :ensure))
    (should (plist-get desc :make-process))
    (should (plist-get desc :send-string))
    (should (plist-get desc :send-return))
    (should (plist-get desc :send-escape))))

(ert-deftest codex-ide-term-resolve-backend-eat ()
  "eat resolves to a descriptor implementing the four ops."
  (let ((desc (codex-ide-term--resolve-backend 'eat)))
    (should (plist-get desc :ensure))
    (should (plist-get desc :make-process))
    (should (plist-get desc :send-string))
    (should (plist-get desc :send-return))
    (should (plist-get desc :send-escape))))

(ert-deftest codex-ide-term-resolve-backend-unknown-errors ()
  "Unknown backend signals `user-error'."
  (should-error (codex-ide-term--resolve-backend 'nosuch)
                :type 'user-error))

(ert-deftest codex-ide-term-send-string-uses-buffer-local-backend ()
  "Send operations use the session backend, not the mutable global default."
  (let (sent)
    (with-temp-buffer
      (setq-local codex-ide-term--backend
                  (list :send-string (lambda (string)
                                       (setq sent string))))
      (let ((codex-ide-terminal-backend 'nosuch))
        (codex-ide-term--send-string "hello"))
      (should (equal sent "hello")))))

(ert-deftest codex-ide-default-buffer-name ()
  "Buffer name follows the `*codex[<basename>]*' shape."
  (should (equal (codex-ide--default-buffer-name "/tmp/foo")
                 "*codex[foo]*"))
  (should (equal (codex-ide--default-buffer-name "/tmp/foo/")
                 "*codex[foo]*")))

(ert-deftest codex-ide-display-buffer-default-function ()
  "Default display uses same-window buffer switching."
  (should (eq codex-ide-display-buffer-function
              #'pop-to-buffer-same-window)))

(ert-deftest codex-ide-display-buffer-calls-custom-function ()
  "Display helper delegates to `codex-ide-display-buffer-function'."
  (let ((buffer (get-buffer-create " *codex-ide-display-test*"))
        called)
    (unwind-protect
        (save-window-excursion
          (let ((codex-ide-display-buffer-function
                 (lambda (buf)
                   (setq called buf)
                   (pop-to-buffer-same-window buf))))
            (let ((window (codex-ide--display-buffer buffer)))
              (should (eq called buffer))
              (should (window-live-p window))
              (should (eq (window-buffer window) buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-display-buffer-updates-last-accessed-buffer ()
  "Display helper records the displayed Codex buffer."
  (let ((buffer (get-buffer-create " *codex-ide-last-accessed-test*"))
        (codex-ide--last-accessed-buffer nil)
        (codex-ide-display-buffer-function #'pop-to-buffer-same-window))
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
  "Config menu binds CLI suffixes and the save command."
  (should (eq (keymap-lookup codex-ide-config-map "S")
              #'codex-ide-menu--save-config))
  (dolist (key '("p" "b" "a" "A" "S"))
    (should (member key (codex-ide-test--popup-keys codex-ide-config-map)))))

(ert-deftest codex-ide-menu-config-omits-side-window-controls ()
  "Config menu does not expose package-owned side-window layout controls."
  (dolist (key '("s" "w" "h" "u" "f"))
    (should-not (keymap-lookup codex-ide-config-map key))
    (should-not (member key (codex-ide-test--popup-keys
                             codex-ide-config-map)))))

(ert-deftest codex-ide-menu-save-config-saves-current-symbols ()
  "Save config persists current non-layout configuration."
  (let (saved)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol _value)
                 (push symbol saved)))
              ((symbol-function 'codex-ide-log)
               (lambda (&rest _args) nil)))
      (codex-ide-menu--save-config))
    (should (equal (reverse saved)
                   '(codex-ide-cli-path
                     codex-ide-terminal-backend
                     codex-ide-display-buffer-function
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

(ert-deftest codex-ide-menu-no-alt-screen-description-is-dynamic ()
  "Toggle entries resolve their description from current variable state."
  (let* ((rows (keymap-popup--meta codex-ide-config-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows))
         (entry (cl-find "A" entries
                         :key (lambda (e) (plist-get e :key))
                         :test #'equal))
         (desc-fn (plist-get entry :description)))
    (should (functionp desc-fn))
    (let ((codex-ide-no-alt-screen t))
      (should (string-match-p "ON" (funcall desc-fn))))
    (let ((codex-ide-no-alt-screen nil))
      (should (string-match-p "OFF" (funcall desc-fn))))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
