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
        (codex-ide-terminal-backend 'eat)
        (codex-ide-config-overrides nil)
        (codex-ide-ask-for-approval nil)
        (codex-ide-no-alt-screen nil)
        (codex-ide-display-buffer-function #'pop-to-buffer-same-window)
        (codex-ide-cli-extra-args nil))
    (funcall body)))

(defun codex-ide-test--make-buffer-process (buffer name)
  "Return a live test process named NAME attached to BUFFER."
  (unless (executable-find "sleep")
    (ert-skip "sleep executable not found"))
  (start-process name buffer "sleep" "60"))

(defun codex-ide-test--make-recoverable-buffer (root name process-name
                                                     session-id)
  "Return (BUFFER PROCESS) for a live orphan Codex buffer.
The buffer carries ROOT and SESSION-ID as buffer-locals with
`codex-ide-mode' enabled, simulating a session whose record was lost."
  (let ((buffer (get-buffer-create name))
        process)
    (with-current-buffer buffer
      (setq default-directory root)
      (setq-local codex-ide--session-root root)
      (setq-local codex-ide--session-id session-id)
      (codex-ide-mode 1))
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

(defun codex-ide-test--wait-for (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass.
Returns the final PREDICATE value.  TIMEOUT defaults to 5 seconds."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (sit-for 0.05))
    (funcall predicate)))

(defun codex-ide-test--call-with-eat-process (script body &optional env)
  "Run SCRIPT through sh in a Codex eat buffer and call BODY.
BODY receives the eat buffer and its process.  ENV is passed through
to the session.  The session is torn down afterwards."
  (when (getenv "CODEX_IDE_SKIP_PTY_TESTS")
    (ert-skip "PTY unavailable in the Nix build sandbox"))
  (unless (executable-find "sh")
    (ert-skip "sh executable not found"))
  (let ((buffer nil)
        (process nil))
    (unwind-protect
        (save-window-excursion
          (setq buffer
                (codex-ide-term--prepare-buffer
                 (generate-new-buffer-name " *codex-ide-eat-test*")
                 temporary-file-directory))
          (switch-to-buffer buffer)
          (setq process (codex-ide-term--make-process
                         buffer "sh" (list "-c" script) env))
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

(ert-deftest codex-ide-term-send-string-delegates-to-eat ()
  "String input goes directly to the eat terminal."
  (with-temp-buffer
    (setq-local eat-terminal 'dummy)
    (let (sent)
      (cl-letf (((symbol-function 'eat-term-send-string)
                 (lambda (terminal string)
                   (setq sent (list terminal string)))))
        (codex-ide-term--send-string "hello"))
      (should (equal sent '(dummy "hello"))))))

(ert-deftest codex-ide-term-send-return-sends-cr ()
  "Return input reaches the eat terminal as a carriage return."
  (with-temp-buffer
    (setq-local eat-terminal 'dummy)
    (let (sent)
      (cl-letf (((symbol-function 'eat-term-send-string)
                 (lambda (_terminal string)
                   (setq sent string))))
        (codex-ide-term--send-return))
      (should (equal sent "\r")))))

(ert-deftest codex-ide-term-send-escape-sends-esc ()
  "Escape input reaches the eat terminal as an ESC character."
  (with-temp-buffer
    (setq-local eat-terminal 'dummy)
    (let (sent)
      (cl-letf (((symbol-function 'eat-term-send-string)
                 (lambda (_terminal string)
                   (setq sent string))))
        (codex-ide-term--send-escape))
      (should (equal sent "\e")))))

(ert-deftest codex-ide-term-send-ops-require-terminal ()
  "Send operations do nothing without a live eat terminal."
  (with-temp-buffer
    (setq-local eat-terminal nil)
    (let (sent)
      (cl-letf (((symbol-function 'eat-term-send-string)
                 (lambda (_terminal string)
                   (setq sent string))))
        (codex-ide-term--send-string "hello")
        (codex-ide-term--send-return)
        (codex-ide-term--send-escape))
      (should-not sent))))

(ert-deftest codex-ide-term-normalize-cursor-state ()
  "Cursor normalization steadies blinking states and preserves others."
  (dolist (case '((:blinking-block . :block)
                  (:blinking-bar . :bar)
                  (:blinking-underline . :underline)
                  (:block . :block)
                  (:bar . :bar)
                  (:underline . :underline)
                  (:invisible . :invisible)
                  (:unknown . :unknown)))
    (should (eq (codex-ide-term-eat--normalize-cursor-state (car case) nil)
                (cdr case)))
    (should (eq (codex-ide-term-eat--normalize-cursor-state (car case) t)
                (car case)))))

(ert-deftest codex-ide-term-configure-buffer-installs-sync-and-hook ()
  "Buffer setup installs point and window synchronization."
  (with-temp-buffer
    (eat-mode)
    (codex-ide-term--configure-buffer)
    (should (eq eat--synchronize-scroll-function
                #'codex-ide-term-eat--synchronize-scroll))
    (should (local-variable-p 'eat--synchronize-scroll-function))
    (should (memq #'codex-ide-term-eat--synchronize-window
                  (buffer-local-value 'window-buffer-change-functions
                                      (current-buffer))))))

(ert-deftest codex-ide-term-cursor-adapter-installs-idempotently ()
  "Cursor setup retains Eat's callback, applies state, and avoids wrapping."
  (with-temp-buffer
    (eat-mode)
    (setq eat-terminal (eat-term-make (current-buffer) (point)))
    (let* ((states nil)
           (original (lambda (_terminal state) (push state states))))
      (setf (eat-term-parameter eat-terminal 'set-cursor-function) original)
      (cl-letf (((symbol-function 'eat-term-cursor-type)
                 (lambda (_terminal) :blinking-bar)))
        (codex-ide-term--configure-buffer)
        (codex-ide-term--configure-buffer))
      (should (eq (eat-term-parameter eat-terminal 'set-cursor-function)
                  #'codex-ide-term-eat--set-cursor))
      (should (eq (eat-term-parameter
                   eat-terminal
                   'codex-ide-term-eat--original-set-cursor-function)
                  original))
      (should (equal states '(:bar :bar))))))

(ert-deftest codex-ide-term-cursor-adapter-honors-blink-option ()
  "The cursor adapter passes blinking states through when enabled."
  (with-temp-buffer
    (eat-mode)
    (setq eat-terminal (eat-term-make (current-buffer) (point)))
    (let ((codex-ide-term-blink-cursor t)
          state)
      (setf (eat-term-parameter eat-terminal 'set-cursor-function)
            (lambda (_terminal cursor-state) (setq state cursor-state)))
      (cl-letf (((symbol-function 'eat-term-cursor-type)
                 (lambda (_terminal) :blinking-underline)))
        (codex-ide-term--configure-buffer))
      (should (eq state :blinking-underline)))))

(defun codex-ide-test--sync-scroll (park snapshot-p &optional emacs-mode-p)
  "Run the scroll sync override with point parked at PARK.
The stubbed terminal display region begins at buffer position 5.  When
SNAPSHOT-P, pass eat's would-be pre-output snapshot (buffer plus the
selected window); otherwise pass nil.  EMACS-MODE-P selects Eat's
Emacs input mode instead of semi-char mode.  Return (SYNCED . WINDOW),
the list forwarded to `eat--synchronize-scroll' and the selected window."
  (let ((buffer (generate-new-buffer " *codex-ide-sync-test*"))
        synced window)
    (unwind-protect
        (cl-letf (((symbol-function 'eat--synchronize-scroll)
                   (lambda (windows)
                     (setq synced windows)))
                  ((symbol-function 'eat-term-display-beginning)
                   (lambda (_terminal) 5)))
          (save-window-excursion
            (switch-to-buffer buffer)
            (insert "scrollback and display text")
            (setq-local eat-terminal 'dummy)
            (if emacs-mode-p
                (eat-emacs-mode)
              (eat-semi-char-mode))
            (goto-char park)
            (setq window (selected-window))
            (codex-ide-term-eat--synchronize-scroll
             (and snapshot-p (list 'buffer window)))))
      (kill-buffer buffer))
    (cons synced window)))

(ert-deftest codex-ide-term-synchronize-scroll-rescues-display-point ()
  "Off-snapshot points inside the display region snap to the cursor."
  (pcase-let ((`(,synced . ,window) (codex-ide-test--sync-scroll 7 nil)))
    (should (equal synced (list 'buffer window)))))

(ert-deftest codex-ide-term-synchronize-scroll-leaves-scrollback-alone ()
  "Points parked above the display region are not yanked to the cursor."
  (pcase-let ((`(,synced . ,_window) (codex-ide-test--sync-scroll 2 nil)))
    (should-not synced)))

(ert-deftest codex-ide-term-synchronize-scroll-rescues-collapsed-point ()
  "Points collapsed to `point-min' by a scrollback purge re-sync."
  (pcase-let ((`(,synced . ,window) (codex-ide-test--sync-scroll 1 nil)))
    (should (equal synced (list 'buffer window)))))

(ert-deftest codex-ide-term-synchronize-scroll-honors-eat-snapshot ()
  "Positions eat saw on the cursor before output always sync."
  (pcase-let ((`(,synced . ,window) (codex-ide-test--sync-scroll 2 t)))
    (should (equal synced (list 'buffer window)))))

(ert-deftest codex-ide-term-synchronize-scroll-leaves-emacs-mode-free ()
  "Off-snapshot display positions stay free in Eat Emacs mode."
  (pcase-let ((`(,synced . ,_window)
               (codex-ide-test--sync-scroll 7 nil t)))
    (should-not synced)))

(ert-deftest codex-ide-term-synchronize-scroll-honors-emacs-snapshot ()
  "Eat snapshots remain authoritative in Eat Emacs mode."
  (pcase-let ((`(,synced . ,window)
               (codex-ide-test--sync-scroll 7 t t)))
    (should (equal synced (list 'buffer window)))))

(ert-deftest codex-ide-return-live-restores-terminal-following ()
  "Returning live restores input, point, window, and recentering."
  (with-temp-buffer
    (eat-mode)
    (let ((inhibit-read-only t))
      (insert "0123456789abcdef"))
    (setq-local eat-terminal 'dummy)
    (goto-char 2)
    (let ((buffer (current-buffer))
          recentered)
      (save-window-excursion
        (switch-to-buffer buffer)
        (cl-letf (((symbol-function 'eat-term-display-cursor)
                   (lambda (_terminal) 10))
                  ((symbol-function 'eat-term-display-beginning)
                   (lambda (_terminal) 5))
                  ((symbol-function 'eat-term-size)
                   (lambda (_terminal) '(80 . 24)))
                  ((symbol-function 'recenter)
                   (lambda (&rest args)
                     (setq recentered args))))
          (codex-ide-return-live))
        (should eat--semi-char-mode)
        (should-not eat--char-mode)
        (should-not eat--line-mode)
        (should-not buffer-read-only)
        (should (= (point) 10))
        (should (= (window-point (selected-window)) 10))
        (should recentered)))))

(ert-deftest codex-ide-term-synchronize-window-syncs-window ()
  "The window hook delegates the window to Eat's scroll sync."
  (let ((buffer (generate-new-buffer " *codex-ide-snap-test*"))
        synced)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local eat-terminal 'dummy)
          (save-window-excursion
            (switch-to-buffer buffer)
            (cl-letf (((symbol-function 'eat--synchronize-scroll)
                       (lambda (windows)
                         (setq synced windows))))
              (codex-ide-term-eat--synchronize-window (selected-window)))
            (should (equal synced (list (selected-window))))))
      (kill-buffer buffer))))

(ert-deftest codex-ide-term-synchronize-window-ignores-non-eat-buffers ()
  "The window hook leaves windows on non-Eat buffers alone."
  (let ((buffer (generate-new-buffer " *codex-ide-snap-plain*"))
        synced)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (cl-letf (((symbol-function 'eat--synchronize-scroll)
                     (lambda (windows)
                       (setq synced windows))))
            (codex-ide-term-eat--synchronize-window (selected-window)))
          (should-not synced))
      (kill-buffer buffer))))

(ert-deftest codex-ide-term-sync-dimensions-delegates ()
  "Display sync delegates to eat's window size adjustment function."
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

(ert-deftest codex-ide-term-make-process-starts-eat-session ()
  "Session creation yields a live eat process with argv and env intact."
  (codex-ide-test--call-with-eat-process
   "printf '%s' \"$CODEX_IDE_TEST_VAR\"; sleep 60"
   (lambda (buffer process)
     (should (process-live-p process))
     (with-current-buffer buffer
       (should (eq major-mode 'eat-mode))
       ;; Session recovery matches the program inside `process-command';
       ;; eat's stty wrapper must keep the real argv visible.
       (should (member "sh" (process-command process)))
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "codex-env-ok" (buffer-string)))))))
   '("CODEX_IDE_TEST_VAR=codex-env-ok")))

(ert-deftest codex-ide-term-process-starts-at-visible-window-size ()
  "The child process sees the displayed window size on its first output."
  (save-window-excursion
    (delete-other-windows)
    (codex-ide-test--call-with-eat-process
     "stty size; sleep 60"
     (lambda (buffer _process)
       (with-current-buffer buffer
         (pcase-let ((`(,columns . ,rows) (eat-term-size eat-terminal)))
           (should
            (codex-ide-test--wait-for
             (lambda ()
               (string-match-p
                (format "\\b%d %d\\b" rows columns)
                (buffer-string)))))))))))

(ert-deftest codex-ide-term-point-survives-erase-display ()
  "Point tracks the cursor across full-screen redraws.
Regression test for the bug that forced the switch to vterm: an
erase-display sequence collapsed off-cursor points to `point-min' and
eat never restored them."
  (codex-ide-test--call-with-eat-process
   "printf 'one\\r\\ntwo\\r\\nthree\\r\\n'; read x; printf '\\033[H\\033[2Jredrawn'; read y"
   (lambda (buffer process)
     (with-current-buffer buffer
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "three" (buffer-string)))))
       ;; Park point away from the cursor, as a stale window switch or a
       ;; user click would, then let the TUI redraw everything.
       (goto-char (point-min))
       (process-send-string process "\n")
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "redrawn" (buffer-string)))))
       (should (= (point) (eat-term-display-cursor eat-terminal)))))))

(ert-deftest codex-ide-term-scrollback-point-survives-output ()
  "A point parked in the scrollback stays put while output streams."
  (codex-ide-test--call-with-eat-process
   "i=1; while [ $i -le 80 ]; do printf 'line-%s\\r\\n' $i; i=$((i+1)); done; read x; printf 'tail\\r\\n'; read y"
   (lambda (buffer process)
     (with-current-buffer buffer
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "line-80" (buffer-string)))))
       ;; Park point above the display region, as scrolling back does.
       ;; Not at `point-min': that position re-syncs by design, since
       ;; scrollback purges collapse dragged points there.
       (goto-char (+ (point-min) 10))
       (should (< (point) (eat-term-display-beginning eat-terminal)))
       (process-send-string process "\n")
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "tail" (buffer-string)))))
       (should (= (point) (+ (point-min) 10)))))))

(ert-deftest codex-ide-term-point-survives-scrollback-purge ()
  "Point re-syncs when a resize-style redraw purges the scrollback.
Codex resize reflows emit ESC [2J and ESC [3J, deleting the whole
buffer and collapsing every point and marker to `point-min'.  Once the
transcript is re-emitted the collapsed point sits below the display
region, where the scrollback-browsing rule alone would strand it."
  (codex-ide-test--call-with-eat-process
   (concat
    "i=1; while [ $i -le 80 ]; do printf 'line-%s\\r\\n' $i; i=$((i+1)); done;"
    " read x; printf '\\033[H\\033[2J\\033[3J';"
    " i=1; while [ $i -le 80 ]; do printf 'again-%s\\r\\n' $i; i=$((i+1)); done;"
    " printf 'redrawn\\r\\n'; read y")
   (lambda (buffer process)
     (with-current-buffer buffer
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "line-80" (buffer-string)))))
       ;; Park point mid-scrollback, then let the purge and reflow run.
       (goto-char (+ (point-min) 40))
       (process-send-string process "\n")
       (should (codex-ide-test--wait-for
                (lambda ()
                  (string-match-p "redrawn" (buffer-string)))))
       (should (= (point) (eat-term-display-cursor eat-terminal)))))))

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
  "Codex pops the terminal buffer to another window by default."
  (should (eq codex-ide-display-buffer-function #'pop-to-buffer)))

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
           (cl-letf (((symbol-function 'codex-ide--display-buffer)
                      (lambda (_buffer) (selected-window)))
                     ((symbol-function 'codex-ide-term--make-process)
                      (lambda (buffer _program _args _env)
                        (setq requested-name (buffer-name buffer))
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

(ert-deftest codex-ide-create-session-displays-before-process-start ()
  "Session creation prepares and displays the buffer before process start."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let (events process buffer)
       (unwind-protect
           (cl-letf (((symbol-function 'codex-ide-term--prepare-buffer)
                      (lambda (name directory)
                        (push (list 'prepare name directory) events)
                        (setq buffer (get-buffer-create name))))
                     ((symbol-function 'codex-ide--display-buffer)
                      (lambda (display-buffer)
                        (push (list 'display display-buffer) events)
                        (selected-window)))
                     ((symbol-function 'codex-ide-term--make-process)
                      (lambda (process-buffer _program _args _env)
                        (push (list 'process process-buffer) events)
                        (setq process
                              (codex-ide-test--make-buffer-process
                               process-buffer "codex-ide-order")))))
             (let ((default-directory root))
               (codex-ide--create-session 1))
             (should (equal (mapcar #'car (nreverse events))
                            '(prepare display process))))
         (codex-ide-test--kill-buffer-process buffer process))))))

(ert-deftest codex-ide-create-session-cleans-up-display-failure ()
  "A display failure kills the prepared buffer before registration."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let (buffer process-started)
       (cl-letf (((symbol-function 'codex-ide-term--prepare-buffer)
                  (lambda (name _directory)
                    (setq buffer (get-buffer-create name))))
                 ((symbol-function 'codex-ide--display-buffer)
                  (lambda (_buffer) nil))
                 ((symbol-function 'codex-ide-term--make-process)
                  (lambda (&rest _args) (setq process-started t))))
         (let ((default-directory root))
           (should-error (codex-ide--create-session 1) :type 'error))
         (should-not process-started)
         (should-not (buffer-live-p buffer)))))))

(ert-deftest codex-ide-create-session-cleans-up-process-failure ()
  "A process failure kills the displayed but incomplete buffer."
  (codex-ide-test--call-with-project
   (lambda (root)
     (let (buffer)
       (cl-letf (((symbol-function 'codex-ide-term--prepare-buffer)
                  (lambda (name _directory)
                    (setq buffer (get-buffer-create name))))
                 ((symbol-function 'codex-ide--display-buffer)
                  (lambda (_buffer) (selected-window)))
                 ((symbol-function 'codex-ide-term--make-process)
                  (lambda (&rest _args) (error "Process failed"))))
         (let ((default-directory root))
           (should-error (codex-ide--create-session 1) :type 'error))
         (should-not (buffer-live-p buffer)))))))

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
                 root buffer-name "codex-ide-recover" 2))
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
              (should codex-ide-mode)
              (should (equal codex-ide--session-root root))
              (should (= codex-ide--session-id 2))
              (should (eq (cdr (assq 'codex-ide-mode
                                     (minor-mode-key-binding
                                      (kbd "S-<return>"))))
                          #'codex-ide-insert-newline))
              (should (eq (cdr (assq 'codex-ide-mode
                                     (minor-mode-key-binding
                                      (kbd "C-c C-k"))))
                          #'codex-ide-send-escape)))
            (should (assoc buffer-name
                           (codex-ide--session-candidates root)))
            (should (assoc buffer-name
                           (codex-ide--session-candidates)))))
      (when pair
        (codex-ide-test--kill-buffer-process (car pair) (cadr pair)))
      (delete-directory root t))))

(ert-deftest codex-ide-mode-map-leaves-eat-maps-alone ()
  "Codex bindings live in the minor mode map, not in eat's keymaps."
  (should (eq (lookup-key codex-ide-mode-map (kbd "S-<return>"))
              #'codex-ide-insert-newline))
  (should (eq (lookup-key codex-ide-mode-map (kbd "C-c C-k"))
              #'codex-ide-send-escape))
  (should (eq (lookup-key codex-ide-mode-map (kbd "C-c C-j"))
              #'codex-ide-return-live))
  (should (eq (lookup-key eat-mode-map (kbd "C-c C-j"))
              #'eat-semi-char-mode))
  (dolist (map (list eat-mode-map eat-semi-char-mode-map))
    (should-not (eq (lookup-key map (kbd "S-<return>"))
                    #'codex-ide-insert-newline))
    (should-not (eq (lookup-key map (kbd "C-c C-k"))
                    #'codex-ide-send-escape))
    (should-not (eq (lookup-key map (kbd "C-c C-j"))
                    #'codex-ide-return-live)))
  (with-temp-buffer
    (eat-mode)
    (should (eq (key-binding (kbd "C-c C-j"))
                #'eat-semi-char-mode))
    (codex-ide-mode 1)
    (should (eq (key-binding (kbd "C-c C-j"))
                #'codex-ide-return-live))
    (should (eq (cdr (assq 'codex-ide-mode
                           (minor-mode-key-binding (kbd "S-<return>"))))
                #'codex-ide-insert-newline))
    (should (eq (cdr (assq 'codex-ide-mode
                           (minor-mode-key-binding (kbd "C-c C-j"))))
                #'codex-ide-return-live))))

(ert-deftest codex-ide-mode-manages-cleanup-hook ()
  "Enabling the mode installs the cleanup hook once; disabling removes it."
  (with-temp-buffer
    (codex-ide-mode 1)
    (codex-ide-mode 1)
    (should (= (cl-count #'codex-ide--cleanup-current-buffer-session
                         kill-buffer-hook)
               1))
    (codex-ide-mode -1)
    (should-not (memq #'codex-ide--cleanup-current-buffer-session
                      kill-buffer-hook))))

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
                 "codex-ide-ignore" 1))
          (codex-ide--recover-live-sessions)
          (should-not (codex-ide--project-sessions root)))
      (when pair
        (codex-ide-test--kill-buffer-process (car pair) (cadr pair)))
      (delete-directory root t))))

(ert-deftest codex-ide-recovery-ignores-mode-off-buffers ()
  "Recovery ignores Codex-named buffers without `codex-ide-mode'."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-mode-off-" t)))
         (codex-ide--sessions (make-hash-table :test 'equal))
         (codex-ide--active-session-ids (make-hash-table :test 'equal))
         (codex-ide-cli-path "sleep")
         buffer process)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq buffer (get-buffer-create (codex-ide--get-buffer-name root)))
          (with-current-buffer buffer
            (setq default-directory root))
          (setq process (codex-ide-test--make-buffer-process
                         buffer "codex-ide-mode-off"))
          (set-process-query-on-exit-flag process nil)
          (codex-ide--recover-live-sessions)
          (should-not (codex-ide--project-sessions root)))
      (codex-ide-test--kill-buffer-process buffer process)
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
                 "codex-ide-recovered-commands" 2))
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
                 "codex-ide-stop-1" 1))
          (setq second
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-stop-2" 2))
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
                 "codex-ide-cleanup-1" 1))
          (setq second
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-cleanup-2" 2))
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
                 "codex-ide-next-1" 1))
          (setq second
                (codex-ide-test--make-recoverable-buffer
                 root (format "%s<2>" (codex-ide--get-buffer-name root))
                 "codex-ide-next-2" 2))
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

(ert-deftest codex-ide-make-process-sentinel-chains-original ()
  "The Codex sentinel runs the replaced sentinel before cleaning up."
  (let (chained cleaned)
    (cl-letf (((symbol-function 'codex-ide--cleanup-on-exit)
               (lambda (&rest args)
                 (setq cleaned args))))
      (funcall (codex-ide--make-process-sentinel
                "/tmp/root" 1
                (lambda (proc event)
                  (setq chained (list proc event))))
               'fake-proc "finished\n"))
    (should (equal chained '(fake-proc "finished\n")))
    (should (equal cleaned '("/tmp/root" 1)))))

(ert-deftest codex-ide-make-process-sentinel-chains-on-non-exit-events ()
  "Non-exit events reach the chained sentinel without triggering cleanup."
  (let (chained cleaned)
    (cl-letf (((symbol-function 'codex-ide--cleanup-on-exit)
               (lambda (&rest args)
                 (setq cleaned args))))
      (funcall (codex-ide--make-process-sentinel
                "/tmp/root" 1
                (lambda (proc event)
                  (setq chained (list proc event))))
               'fake-proc "open\n"))
    (should (equal chained '(fake-proc "open\n")))
    (should-not cleaned)))

(ert-deftest codex-ide-make-process-sentinel-cleans-up-when-original-errors ()
  "A failing chained sentinel cannot block Codex session cleanup."
  (let (cleaned)
    (cl-letf (((symbol-function 'codex-ide--cleanup-on-exit)
               (lambda (&rest args)
                 (setq cleaned args))))
      (funcall (codex-ide--make-process-sentinel
                "/tmp/root" 1
                (lambda (_proc _event)
                  (error "Boom")))
               'fake-proc "killed\n"))
    (should (equal cleaned '("/tmp/root" 1)))))

(ert-deftest codex-ide-setup-session-enables-mode ()
  "Session setup enables `codex-ide-mode' with the cleanup hook."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-setup-mode-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (sessions)
           (let* ((session (car sessions))
                  (buffer (plist-get session :buffer))
                  (configured 0))
             (cl-letf (((symbol-function 'codex-ide-term--configure-buffer)
                        (lambda () (cl-incf configured))))
               (codex-ide--setup-session session)
               (codex-ide--setup-session session))
             (with-current-buffer buffer
               (should codex-ide-mode)
               (should (= configured 2))
               (should (= (cl-count
                           #'codex-ide--cleanup-current-buffer-session
                           kill-buffer-hook)
                          1))))))
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
            (created 0)
            (recoveries 0))
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
                       ((symbol-function 'codex-ide--recover-live-sessions)
                        (lambda ()
                          (setq recoveries (1+ recoveries))))
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
             (should (= created 1))
             (should (= recoveries 2)))
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
                     codex-ide-menu--set-terminal-backend
                     codex-ide-menu--set-approval
                     codex-ide-menu--toggle-no-alt-screen
                     codex-ide-menu--save-config))
    (should (codex-ide-test--command-bound-p codex-ide-config-map command))))

(ert-deftest codex-ide-menu-mcp-commands ()
  "MCP menu exposes start/stop/status/install commands."
  (dolist (command '(codex-ide-mcp-start
                     codex-ide-mcp-stop
                     codex-ide-mcp-status
                     codex-ide-mcp-install-codex-config))
    (should (codex-ide-test--command-bound-p codex-ide-mcp-map command))))

(ert-deftest codex-ide-menu-save-config-saves-current-symbols ()
  "Save config persists the documented configuration set."
  (let (saved)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol _value)
                 (push symbol saved)))
              ((symbol-function 'codex-ide-log)
               (lambda (&rest _args) nil)))
      (codex-ide-menu--save-config))
    (should (equal (reverse saved)
                   codex-ide-menu--saved-config-symbols))))

(ert-deftest codex-ide-menu-debug-commands ()
  "Debug menu exposes status, toggle, and log commands."
  (dolist (command '(codex-ide-check-status
                     codex-ide-menu--toggle-debug-mode
                     codex-ide-show-debug
                     codex-ide-clear-debug))
    (should (codex-ide-test--command-bound-p codex-ide-debug-map command))))

(ert-deftest codex-ide-menu-submenus-exist ()
  "Config, MCP, and debug submenus are defined keymaps."
  (should (keymapp codex-ide-config-map))
  (should (keymapp codex-ide-mcp-map))
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

(ert-deftest codex-ide-setup-session-sentinel-idempotent ()
  "Repeated setup installs one process sentinel cleanup path."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-setup-sentinel-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1))
         (lambda (sessions)
           (let* ((session (car sessions))
                  (process (plist-get session :process))
                  (cleanup-calls 0)
                  (original (lambda (&rest _) nil)))
             (set-process-sentinel process original)
             (cl-letf (((symbol-function 'codex-ide--cleanup-on-exit)
                        (lambda (&rest _)
                          (setq cleanup-calls (1+ cleanup-calls))))
                       ((symbol-function 'codex-ide-term--configure-buffer)
                        (lambda () nil)))
               (codex-ide--setup-session session)
               (codex-ide--setup-session session)
               (codex-ide--setup-session session)
               (should (process-get process 'codex-ide--sentinel-installed))
               (funcall (process-sentinel process) process "finished\n")
               (should (= cleanup-calls 1))))))
      (delete-directory root t))))

(ert-deftest codex-ide-ensure-cli-redetects-after-path-change ()
  "CLI availability cache is invalidated when the path changes."
  (let ((codex-ide--cli-available t)
        (codex-ide-cli-path "true")
        (detected 0))
    (cl-letf (((symbol-function 'codex-ide--detect-cli)
               (lambda ()
                 (setq detected (1+ detected)
                       codex-ide--cli-available nil))))
      (codex-ide-menu--set-cli-path "/no/such/codex-binary")
      (should-not codex-ide--cli-available)
      (should-not (codex-ide--ensure-cli))
      (should (= detected 1))
      (should (equal codex-ide-cli-path "/no/such/codex-binary")))))

(ert-deftest codex-ide-saved-session-candidates-from-rollout ()
  "Saved-session scanner reads rollout session_meta ids."
  (let* ((dir (make-temp-file "codex-ide-sessions-" t))
         (project (file-name-as-directory
                   (make-temp-file "codex-ide-project-" t)))
         (other (file-name-as-directory
                 (make-temp-file "codex-ide-other-" t)))
         (file (expand-file-name "rollout.jsonl" dir))
         (id "019fc273-afc7-7543-85ee-9f7725df777f"))
    (unwind-protect
        (let ((codex-ide-sessions-directory dir)
              (codex-ide-resume-session-scan-limit 50))
          (with-temp-file file
            (insert (format
                     "{\"type\":\"session_meta\",\"payload\":{\"session_id\":%s,\"cwd\":%s}}\n"
                     (json-encode id)
                     (json-encode project))))
          (should (equal (mapcar #'car
                                 (codex-ide--saved-session-candidates project))
                         (list id)))
          (should-not (codex-ide--saved-session-candidates other))
          (should (equal (mapcar #'car
                                 (codex-ide--saved-session-candidates nil))
                         (list id)))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (format "%s  %s" id project))))
            (should (equal (codex-ide--read-saved-session-id project) id)))
          (should (equal (codex-ide--build-command nil id)
                         (cons "codex" (list "resume" id)))))
      (delete-directory dir t)
      (delete-directory project t)
      (delete-directory other t))))

(ert-deftest codex-ide-stop-active-only-contract ()
  "Stop kills only the active session buffer for a project root."
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-stop-" t))))
    (unwind-protect
        (codex-ide-test--call-with-sessions
         `((,root 1) (,root 2))
         (lambda (sessions)
           (let* ((first (car sessions))
                  (second (cadr sessions))
                  (logs nil))
             (codex-ide--activate-session first)
             (cl-letf (((symbol-function 'codex-ide-log)
                        (lambda (fmt &rest args)
                          (push (apply #'format fmt args) logs)))
                       ((symbol-function 'codex-ide--get-working-directory)
                        (lambda () root)))
               (codex-ide-stop))
             (should-not (buffer-live-p (plist-get first :buffer)))
             (should (buffer-live-p (plist-get second :buffer)))
             (should (cl-some (lambda (line)
                                (string-match-p "active Codex session" line))
                              logs)))))
      (delete-directory root t))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
