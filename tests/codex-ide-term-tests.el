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

(provide 'codex-ide-term-tests)

;;; codex-ide-term-tests.el ends here
