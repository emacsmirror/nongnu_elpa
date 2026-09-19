;;; codex-ide-term-tests.el --- Terminal backend tests  -*- lexical-binding: t; -*-

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

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT tests for terminal backend selection and the optional vterm adapter.

;;; Code:

(require 'ert)
(require 'codex-ide)
(require 'codex-ide-term)
(require 'codex-ide-term-vterm)
(require 'seq)

(declare-function vterm-copy-mode "vterm" (&optional arg))

(defvar vterm-copy-mode)
(defvar vterm-environment)
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-shell)

(ert-deftest codex-ide-term-default-backend-is-eat ()
  "Eat remains the default terminal backend."
  (should (eq codex-ide-terminal-backend 'eat)))

(ert-deftest codex-ide-term-prepare-records-selected-backend ()
  "A new terminal buffer records the backend that prepared it."
  (let ((codex-ide-terminal-backend 'eat)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-term-eat--prepare-buffer)
                   (lambda (name _directory)
                     (setq buffer (get-buffer-create name))))
                  ((symbol-function 'codex-ide-term-eat--available-p)
                   (lambda () t)))
          (setq buffer
                (codex-ide-term--prepare-buffer
                 " *codex-ide-backend-test*" temporary-file-directory))
          (with-current-buffer buffer
            (should (eq codex-ide-term--backend 'eat))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-term-unavailable-backend-creates-no-buffer ()
  "An unavailable optional backend fails before buffer creation."
  (let ((codex-ide-terminal-backend 'vterm)
        (name " *codex-ide-missing-vterm-test*"))
    (cl-letf (((symbol-function 'codex-ide-term-vterm--available-p)
               (lambda () nil)))
      (should-error
       (codex-ide-term--prepare-buffer name temporary-file-directory)
       :type 'user-error)
      (should-not (get-buffer name)))))

(ert-deftest codex-ide-term-vterm-command-preserves-argv ()
  "The vterm shell command preserves argument boundaries."
  (should
   (equal (codex-ide-term-vterm--command
           "/tmp/codex cli" '("resume" "id with space" "$HOME"))
          (mapconcat #'shell-quote-argument
                     '("/tmp/codex cli" "resume" "id with space" "$HOME")
                     " "))))

(ert-deftest codex-ide-term-existing-buffer-keeps-backend ()
  "Changing the option does not reroute an existing terminal buffer."
  (with-temp-buffer
    (setq-local codex-ide-term--backend 'vterm)
    (let ((codex-ide-terminal-backend 'eat)
          sent)
      (cl-letf (((symbol-function 'codex-ide-term-vterm--available-p)
                 (lambda () t))
                ((symbol-function 'codex-ide-term-vterm--send-string)
                 (lambda (string)
                   (setq sent string)))
                ((symbol-function 'codex-ide-term-eat--send-string)
                 (lambda (_string)
                   (ert-fail "Existing buffer was rerouted to Eat"))))
        (codex-ide-term--send-string "hello"))
      (should (equal sent "hello")))))

(ert-deftest codex-ide-term-recovered-vterm-records-backend ()
  "Configuration infers and records vterm for a recovered buffer."
  (with-temp-buffer
    (setq major-mode 'vterm-mode)
    (let ((codex-ide-terminal-backend 'eat)
          configured)
      (cl-letf (((symbol-function 'codex-ide-term-vterm--available-p)
                 (lambda () t))
                ((symbol-function 'codex-ide-term-vterm--configure-buffer)
                 (lambda ()
                   (setq configured t))))
        (codex-ide-term--configure-buffer))
      (should configured)
      (should (eq codex-ide-term--backend 'vterm)))))

(ert-deftest codex-ide-term-vterm-make-process-binds-startup-data ()
  "vterm receives the complete command and environment at startup."
  (unless (executable-find "sleep")
    (ert-skip "sleep executable not found"))
  (let ((vterm-environment '("USER_VTERM=value"))
        (buffer (generate-new-buffer " *codex-ide-vterm-start-test*"))
        process startup)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-mode)
                   (lambda ()
                     (setq major-mode 'vterm-mode)
                     (setq startup
                           (list vterm-shell vterm-environment
                                 vterm-kill-buffer-on-exit))
                     (setq process
                           (start-process "codex-ide-vterm-start-test"
                                          (current-buffer) "sleep" "60")))))
          (should
           (eq (codex-ide-term-vterm--make-process
                buffer "/tmp/codex cli" '("resume" "id with space")
                '("CODEX_TEST=value"))
               process))
          (should
           (equal startup
                  (list
                   (codex-ide-term-vterm--command
                    "/tmp/codex cli" '("resume" "id with space"))
                   '("CODEX_TEST=value" "USER_VTERM=value") nil)))
          (with-current-buffer buffer
            (should-not vterm-kill-buffer-on-exit)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-term-vterm-input-delegates ()
  "vterm input operations use its public send functions."
  (let (calls)
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (string &optional _paste-p)
                 (push (list 'string string) calls)))
              ((symbol-function 'vterm-send-return)
               (lambda ()
                 (push '(return) calls)))
              ((symbol-function 'vterm-send-escape)
               (lambda ()
                 (push '(escape) calls))))
      (codex-ide-term-vterm--send-string "hello")
      (codex-ide-term-vterm--send-return)
      (codex-ide-term-vterm--send-escape))
    (should (equal (nreverse calls)
                   '((string "hello") (return) (escape))))))

(ert-deftest codex-ide-term-vterm-return-live-leaves-copy-mode ()
  "Returning live disables copy mode before resetting point."
  (let ((vterm-copy-mode t)
        calls)
    (cl-letf (((symbol-function 'vterm-copy-mode)
               (lambda (arg)
                 (push (list 'copy arg) calls)
                 (setq vterm-copy-mode nil)))
              ((symbol-function 'vterm-reset-cursor-point)
               (lambda ()
                 (push '(reset) calls))))
      (codex-ide-term-vterm--return-live))
    (should-not vterm-copy-mode)
    (should (equal (nreverse calls) '((copy -1) (reset))))))

(defun codex-ide-term-test--wait-for (predicate)
  "Wait up to five seconds for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (sit-for 0.05))
    (funcall predicate)))

(ert-deftest codex-ide-term-vterm-live-session ()
  "A vterm session preserves env, input, ownership, and resize support."
  (when (getenv "CODEX_IDE_SKIP_PTY_TESTS")
    (ert-skip "PTY unavailable in the Nix build sandbox"))
  (unless (and (executable-find "sh")
               (codex-ide-term-vterm--available-p))
    (ert-skip "vterm or sh is unavailable"))
  (let ((codex-ide-terminal-backend 'vterm)
        (vterm-environment '("CODEX_VTERM_USER=kept"))
        buffer process)
    (unwind-protect
        (save-window-excursion
          (setq buffer
                (codex-ide-term--prepare-buffer
                 (generate-new-buffer-name " *codex-ide-vterm-live-test*")
                 temporary-file-directory))
          (switch-to-buffer buffer)
          (setq process
                (codex-ide-term--make-process
                 buffer "sh"
                 '("-c" "printf '%s:%s\\n' \"$CODEX_VTERM_TEST\" \"$CODEX_VTERM_USER\"; read line; printf 'reply:%s\\n' \"$line\"; sleep 60")
                 '("CODEX_VTERM_TEST=ready")))
          (should (process-live-p process))
          (should (eq major-mode 'vterm-mode))
          (should (eq codex-ide-term--backend 'vterm))
          (should-not vterm-kill-buffer-on-exit)
          (should (functionp
                   (process-get process 'adjust-window-size-function)))
          (should
           (seq-some (lambda (fragment)
                       (string-match-p "CODEX_VTERM_TEST" fragment))
                     (process-command process)))
          (should
           (codex-ide-term-test--wait-for
            (lambda ()
              (string-match-p "ready:kept" (buffer-string)))))
          (codex-ide-term--send-string "hello")
          (codex-ide-term--send-return)
          (should
           (codex-ide-term-test--wait-for
            (lambda ()
              (string-match-p "reply:hello" (buffer-string)))))
          (vterm-copy-mode 1)
          (codex-ide-term--return-live)
          (should-not vterm-copy-mode))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun codex-ide-term-test--paste-pty (backend)
  "Verify BACKEND delivers an exact Unicode bracketed paste to a raw PTY."
  (when (getenv "CODEX_IDE_SKIP_PTY_TESTS")
    (ert-skip "PTY unavailable in the Nix build sandbox"))
  (unless (executable-find "python3")
    (ert-skip "python3 is unavailable for the raw PTY fixture"))
  (when (and (eq backend 'vterm)
             (not (codex-ide-term-vterm--available-p)))
    (ert-skip "Optional vterm backend is unavailable"))
  (let* ((codex-ide-terminal-backend backend)
         (text "λ\ttext\nnext")
         (wire (encode-coding-string (concat "\e[200~" text "\e[201~") 'utf-8))
         (expected (mapconcat (lambda (byte) (format "%02x" byte)) wire ""))
         buffer process)
    (unwind-protect
        (save-window-excursion
          (setq buffer (codex-ide-term--prepare-buffer
                        (generate-new-buffer-name " *codex-paste-pty*")
                        temporary-file-directory))
          (switch-to-buffer buffer)
          (setq process
                (codex-ide-term--make-process
                 buffer (executable-find "python3")
                 (list "-c"
                       (concat "import os,tty\ntty.setraw(0)\n"
                               "os.write(1,b'READY')\ndata=b''\n"
                               "while not data.endswith(b'\\x1b[201~'):\n"
                               " data+=os.read(0,4096)\n"
                               "os.write(1,b'HEX:'+data.hex().encode()+b':END')\n"
                               "os.read(0,1)\n")) nil))
          (should (codex-ide-term-test--wait-for
                   (lambda () (string-match-p "READY" (buffer-string)))))
          (codex-ide-term--paste-draft process text)
          (should (codex-ide-term-test--wait-for
                   (lambda ()
                     (string-match-p (concat "HEX:" expected ":END")
                                     (buffer-string)))))
          (should (process-live-p process)))
      (when (and process (process-live-p process)) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest codex-ide-term-paste-draft-eat-pty ()
  "Eat preserves paste framing, LF, TAB, and UTF-8 without submitting."
  (codex-ide-term-test--paste-pty 'eat))

(ert-deftest codex-ide-term-paste-draft-vterm-pty ()
  "Optional vterm preserves the same literal paste wire contract."
  (codex-ide-term-test--paste-pty 'vterm))

(ert-deftest codex-ide-term-vterm-load-error-keeps-diagnostic ()
  "A broken module reports its original diagnostic before buffer creation."
  (let ((original (symbol-function 'require))
        (codex-ide-terminal-backend 'vterm)
        (name " *codex-broken-module*"))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (if (eq feature 'vterm)
                     (error "Module library could not load: test ABI mismatch")
                   (apply original feature args)))))
      (should (string-match-p
               "test ABI mismatch"
               (error-message-string
                (should-error
                 (codex-ide-term--prepare-buffer name temporary-file-directory)))))
      (should-not (get-buffer name)))))

(defvar vterm-exit-functions)
(defvar vterm-min-window-width)

(defun codex-ide-term-test--with-owners (backend body)
  "Call BODY with a factory for disposable real BACKEND session owners."
  (when (getenv "CODEX_IDE_SKIP_PTY_TESTS")
    (ert-skip "PTY unavailable in the Nix build sandbox"))
  (unless (executable-find "python3")
    (ert-skip "python3 is unavailable"))
  (when (and (eq backend 'vterm)
             (not (codex-ide-term-vterm--available-p)))
    (ert-skip "Optional vterm is unavailable"))
  (let ((codex-ide-terminal-backend backend)
        (codex-ide-cli-path (executable-find "python3"))
        (codex-ide--sessions (make-hash-table :test 'equal))
        (codex-ide--active-session-ids (make-hash-table :test 'equal))
        (root (file-name-as-directory (make-temp-file "codex-owners-" t)))
        buffers)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (funcall
           body
           (lambda (id script)
             (let ((buffer (codex-ide-term--prepare-buffer
                            (generate-new-buffer-name " *codex-owner*") root)))
               (push buffer buffers)
               (switch-to-buffer buffer)
               (let* ((process (codex-ide-term--make-process
                                buffer codex-ide-cli-path (list "-c" script) nil))
                      (session (codex-ide--make-session root id buffer process)))
                 (puthash root (cons session (gethash root codex-ide--sessions))
                          codex-ide--sessions)
                 (codex-ide--setup-session session)
                 (codex-ide--activate-session session)
                 session)))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (when-let* ((process (get-buffer-process buffer)))
            (delete-process process))
          (when (buffer-live-p buffer) (kill-buffer buffer))))
      (delete-directory root t))))

(defconst codex-ide-term-test--owner-script
  (concat "import os,tty,signal\ntty.setraw(0)\nos.write(1,b'READY\\r\\n')\n"
          "while True:\n c=os.read(0,1)\n"
          " if c==b'x': break\n"
          " if c==b'h': os.kill(os.getpid(),signal.SIGHUP)\n"
          " s=os.get_terminal_size(0)\n"
          " os.write(1,('ACK%s:%dx%d;\\r\\n'%(c.decode(),s.columns,s.lines)).encode())\n")
  "Raw PTY child reporting dimensions and supporting explicit exit/hangup.")

(defun codex-ide-term-test--ready (session)
  "Wait for SESSION's child to enable raw input."
  (with-current-buffer (plist-get session :buffer)
    (should (codex-ide-term-test--wait-for
             (lambda () (string-match-p "READY" (buffer-string)))))))

(ert-deftest codex-ide-term-vterm-owner-lifecycle ()
  "Real vterm cleanup, recovery and mode rejection preserve sibling owners."
  (let (exits)
    (let ((vterm-exit-functions
           (list (lambda (buffer event) (push (list buffer event) exits)))))
      (codex-ide-term-test--with-owners
       'vterm
       (lambda (start)
         (let* ((sibling (funcall start 2 codex-ide-term-test--owner-script))
                (root (plist-get sibling :root)))
           (codex-ide-term-test--ready sibling)
           (dolist (ending '(normal hangup stop kill))
             (let* ((owner (funcall start 1 codex-ide-term-test--owner-script))
                    (buffer (plist-get owner :buffer))
                    (process (plist-get owner :process))
                    (sentinel (process-sentinel process)))
               (codex-ide-term-test--ready owner)
               (dotimes (_ 3) (codex-ide--setup-session owner))
               (should (eq sentinel (process-sentinel process)))
               (with-current-buffer buffer
                 (should-error (fundamental-mode) :type 'user-error)
                 (should (eq major-mode 'vterm-mode))
                 (should codex-ide-mode))
               (should (eq owner (codex-ide--session-by-id root 1)))
               (should (process-live-p process))
               (signal-process process 'SIGSTOP)
               (should (codex-ide-term-test--wait-for
                        (lambda () (eq (process-status process) 'stop))))
               (should (eq owner (codex-ide--session-by-id root 1)))
               ;; The status flips before its notification is dispatched.
               ;; Observe the actual continue event, not a pending stop event.
               (setq exits nil)
               (continue-process process)
               (should (codex-ide-term-test--wait-for
                        (lambda () (member (list buffer "run") exits))))
               (should (eq owner (codex-ide--session-by-id root 1)))
               ;; Registry recovery reuses the process and never nests sentinels.
               (remhash root codex-ide--sessions)
               (dotimes (_ 3) (codex-ide--recover-live-sessions))
               (should (= (length (gethash root codex-ide--sessions)) 2))
               (should (eq process (plist-get (codex-ide--session-by-id root 1)
                                               :process)))
               (should (eq sentinel (process-sentinel process)))
               (setq exits nil)
               (pcase ending
                 ('normal (process-send-string process "x"))
                 ('hangup (process-send-string process "h"))
                 ('stop (with-current-buffer buffer (codex-ide-stop)))
                 ('kill (kill-buffer buffer)))
               (should (codex-ide-term-test--wait-for
                        (lambda () (not (process-live-p process)))))
               (should (codex-ide-term-test--wait-for
                        (lambda () (not (codex-ide--session-by-id root 1)))))
               ;; vterm forwards state notifications as well as terminal exits.
               ;; Continuing can produce a second queued "run" notification.
               (let* ((event (pcase ending
                               ('normal "finished\n")
                               ('hangup "hangup\n")
                               (_ "killed\n")))
                      (terminal-exits
                       (seq-filter (lambda (entry) (equal (cadr entry) event))
                                   exits)))
                 (ert-info ((format "Ending %s, backend hooks %S" ending exits))
                   (should (equal terminal-exits (list (list buffer event)))))
                 (should (seq-every-p (lambda (entry) (eq (car entry) buffer))
                                      exits)))
               ;; Replaying the old callback must not retire a new same-id owner.
               (let* ((replacement (funcall start 1 codex-ide-term-test--owner-script))
                      (replacement-buffer (plist-get replacement :buffer)))
                 (codex-ide-term-test--ready replacement)
                 (funcall sentinel process "hangup\n")
                 (should (eq replacement (codex-ide--session-by-id root 1)))
                 (should (codex-ide--session-live-p replacement))
                 (with-current-buffer replacement-buffer (codex-ide-stop)))
               (should (codex-ide--session-live-p sibling))
               (with-current-buffer (plist-get sibling :buffer)
                 (let ((token (format "%s" (cl-position ending '(normal hangup stop kill)))))
                   (codex-ide-term--send-string token)
                   (should (codex-ide-term-test--wait-for
                            (lambda () (string-match-p (concat "ACK" token ":")
                                                       (buffer-string)))))))))))))))

(defun codex-ide-term-test--check-size (session token)
  "Assert SESSION's child PTY size matches visible windows using TOKEN."
  ;; Batch Emacs has no redisplay loop.  Exercise the core resize dispatcher,
  ;; not a private vterm resize workaround or just the callback's return value.
  (window--adjust-process-windows)
  (with-current-buffer (plist-get session :buffer)
    (let* ((process (plist-get session :process))
           (size (funcall window-adjust-process-window-size-function
                          process (get-buffer-window-list (current-buffer) nil t)))
           (expected (format "ACK%s:%dx%d;" token
                             (max vterm-min-window-width (car size)) (cdr size))))
      (codex-ide-term--send-string token)
      (should (codex-ide-term-test--wait-for
               (lambda () (string-match-p expected (buffer-string))))))))

(ert-deftest codex-ide-term-vterm-resize-and-copy-mode ()
  "Core/vterm own PTY resize across split, clamp, hide and copy navigation."
  (codex-ide-term-test--with-owners
   'vterm
   (lambda (start)
     (let* ((vterm-min-window-width 1)
            (owner (funcall start 1 codex-ide-term-test--owner-script))
            (buffer (plist-get owner :buffer)))
       (codex-ide-term-test--ready owner)
       (codex-ide-term-test--check-size owner "a")
       (let ((other (split-window-right)))
         (codex-ide-term-test--check-size owner "b")
         (window-resize other -5 t)
         (codex-ide-term-test--check-size owner "c")
         (let ((vterm-min-window-width 80))
           (codex-ide-term-test--check-size owner "d"))
         (let ((hidden (generate-new-buffer " *codex-hidden*")))
           (unwind-protect
               (progn
                 (set-window-buffer other hidden)
                 (set-window-buffer (selected-window) hidden)
                 (window--adjust-process-windows)
                 (set-window-buffer (selected-window) buffer)
                 (codex-ide-term-test--check-size owner "e"))
             (kill-buffer hidden)))
         (delete-window other))
       (with-current-buffer buffer
         (vterm-copy-mode 1)
         (goto-char (point-min))
         (let ((point (point))
               (before (buffer-string)))
           (process-send-string (plist-get owner :process) "f")
           (accept-process-output (plist-get owner :process) 0.1)
           (sit-for 0.1)
           (split-window-below)
           (window--adjust-process-windows)
           (should (= point (point)))
           (should (equal before (buffer-string))))
         (codex-ide-return-live)
         (should-not vterm-copy-mode)
         (codex-ide-term-test--check-size owner "g")
         (should (string-match-p "ACKf:" (buffer-string))))))))

(defun codex-ide-term-test--prompt-pty (backend)
  "Prove public prompt submission writes exact framed bytes on BACKEND."
  (codex-ide-term-test--with-owners
   backend
   (lambda (start)
     (let* ((text "λ\ttext\nnext")
            (wire (encode-coding-string (concat "\e[200~" text "\e[201~\r") 'utf-8))
            (expected (mapconcat (lambda (byte) (format "%02x" byte)) wire ""))
            (owner
             (funcall start 1
                      (concat "import os,tty,select\ntty.setraw(0)\n"
                              "os.write(1,b'READY\\r\\n')\ndata=b''\n"
                              "while not data.endswith(b'\\r'):\n data+=os.read(0,4096)\n"
                              "while select.select([0],[],[],0.2)[0]:\n data+=os.read(0,4096)\n"
                              "os.write(1,b'HEX:'+data.hex().encode()+b':END')\n"
                              "os.read(0,1)\n"))))
       (codex-ide-term-test--ready owner)
       (with-current-buffer (plist-get owner :buffer)
         (codex-ide-send-prompt text)
         (should (codex-ide-term-test--wait-for
                  (lambda ()
                    (string-match-p (concat "HEX:" expected ":END")
                                    (buffer-string)))))
         (should (process-live-p (plist-get owner :process))))))))

(ert-deftest codex-ide-term-send-prompt-eat-pty ()
  "Eat receives one full literal prompt and one final Return."
  (codex-ide-term-test--prompt-pty 'eat))

(ert-deftest codex-ide-term-send-prompt-vterm-pty ()
  "vterm receives one full literal prompt and one final Return."
  (codex-ide-term-test--prompt-pty 'vterm))

(ert-deftest codex-ide-term-vterm-startup-error-cleans-only-candidate ()
  "An error after native startup leaves no orphan and preserves siblings."
  (codex-ide-term-test--with-owners
   'vterm
   (lambda (start)
     (let* ((sibling (funcall start 2 codex-ide-term-test--owner-script))
            (default-directory (plist-get sibling :root))
            (original (symbol-function 'codex-ide-term--make-process))
            candidate process)
       (codex-ide-term-test--ready sibling)
       (cl-letf (((symbol-function 'codex-ide--display-buffer)
                  (lambda (buffer) (switch-to-buffer buffer) (selected-window)))
                 ((symbol-function 'codex-ide--build-command)
                  (lambda (&rest _)
                    (list codex-ide-cli-path "-c" codex-ide-term-test--owner-script)))
                 ((symbol-function 'codex-ide-term--make-process)
                  (lambda (buffer &rest args)
                    (setq candidate buffer
                          process (apply original buffer args))
                    (error "Injected failure after native process startup"))))
         (should (string-match-p
                  "Injected failure after native process startup"
                  (error-message-string
                   (should-error (codex-ide--create-session 1))))))
       (should process)
       (should-not (process-live-p process))
       (should-not (buffer-live-p candidate))
       (should-not (codex-ide--session-by-id default-directory 1))
       (should (codex-ide--session-live-p sibling))))))

(provide 'codex-ide-term-tests)

;;; codex-ide-term-tests.el ends here
