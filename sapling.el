;;; sapling.el --- Fast interface for Sapling  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Swithin Chan <swithinchan@yahoo.com.hk>
;; Assisted-by: Deepseek:deepseek-v4-pro default
;; Version: 0.3.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, vc
;; URL: https://github.com/swithinchan/sapling/

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; A fast interface for the Sapling SCM (`sl').
;;
;; The status buffer is the main entry point:
;;
;;   M-x sapling-status
;;
;; It intentionally uses the same command names as Sapling where possible
;; (`commit', `amend', `absorb', `rebase', `shelve', `smartlog', ...).
;;
;; The separate `vc-sapling' package adds a Sapling backend for Emacs's
;; generic VC commands (`C-x v =', `C-x v d', `C-x v l', ...).
;;
;; On Windows the package avoids shell wrappers whenever possible, runs `sl'
;; through `make-process' with pipe connections, forces UTF-8 decoding, and
;; lowers `w32-pipe-read-delay' for faster output processing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'ansi-color)
(require 'diff-mode)

(defgroup sapling nil
  "Sapling SCM interface."
  :group 'tools
  :group 'vc)

(defcustom sapling-program "sl"
  "Name of or path to the Sapling executable."
  :type 'string)

(defcustom sapling-status-buffer-name "*sapling*"
  "Name of the Sapling status buffer."
  :type 'string)

(defcustom sapling-smartlog-buffer-name "*sapling-smartlog*"
  "Name of the Sapling smartlog buffer."
  :type 'string)

(defcustom sapling-log-buffer-name "*sapling-log*"
  "Name of the Sapling graph log buffer."
  :type 'string)

(defcustom sapling-diff-buffer-name "*sapling-diff*"
  "Name of the Sapling diff buffer."
  :type 'string)

(defcustom sapling-output-buffer-name "*sapling-output*"
  "Name of the generic Sapling output buffer."
  :type 'string)

(defcustom sapling-log-limit 100
  "Number of commits to show in `sapling-log'."
  :type 'natnum)

(defcustom sapling-w32-pipe-read-delay
  (or (bound-and-true-p w32-pipe-read-delay) 0)
  "Value to assign to `w32-pipe-read-delay' while reading `sl' output.

`w32-pipe-read-delay' is a global variable and cannot be safely
buffer-bound around the reads that happen in process filters, so
this package assigns it directly while `sl' processes are running.
Set this to nil to leave the global value unchanged."
  :type '(choice integer
                 (const :tag "Leave unchanged" nil)))

(defcustom sapling-use-color t
  "When non-nil, colorize Sapling command output.
Sapling commands that display text are run with ANSI color enabled and
`ansi-color' translates the SGR sequences into Emacs faces.  This
works on Windows as well as on Unix."
  :type 'boolean)

(defcustom sapling-diff-use-diff-mode t
  "When non-nil, show `sapling-diff' output in `diff-mode'.
This gives Emacs-native diff coloring (removed lines in red, added
lines in green) without parsing Sapling's terminal color codes."
  :type 'boolean)

(defcustom sapling-diff-ignore-space-at-eol
  (memq system-type '(ms-dos windows-nt))
  "When non-nil, pass `--ignore-space-at-eol' to `sl diff'.

Windows text files commonly differ from Sapling's recorded content
only by a carriage return at end of line.  This is enabled by default
on Windows to keep those differences from flooding `sapling-diff'."
  :type 'boolean)

(defcustom sapling-debug nil
  "When non-nil, log Sapling command invocations and output.

The log is written to `sapling-debug-buffer-name'.  Use
`sapling-toggle-debug' to toggle it interactively."
  :type 'boolean)

(defcustom sapling-debug-buffer-name "*sapling-debug*"
  "Name of the Sapling debug log buffer."
  :type 'string)

(defcustom sapling-debug-output-limit 20000
  "Maximum number of output characters stored per process log entry."
  :type 'integer)

(defvar sapling-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `sapling-debug-mode'.")

(define-derived-mode sapling-debug-mode special-mode "Sapling-Debug"
  "Major mode for the Sapling debug log.
\\{sapling-debug-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t))

(defun sapling--debug-log (format-string &rest args)
  "Append a formatted debug entry to `sapling-debug-buffer-name'."
  (when sapling-debug
    (let ((buffer (get-buffer-create sapling-debug-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'sapling-debug-mode)
          (sapling-debug-mode))
        (let ((inhibit-read-only t)
              (text (apply #'format format-string args)))
          (goto-char (point-max))
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
          (insert text)
          (unless (bolp) (insert "
")))))))

(defun sapling--debug-log-output (label output)
  "Log OUTPUT under LABEL, truncating to `sapling-debug-output-limit'."
  (when sapling-debug
    (let ((text (or output "")))
      (when (> (length text) sapling-debug-output-limit)
        (setq text (concat (substring text 0 sapling-debug-output-limit)
                           "\n...truncated...")))
      (sapling--debug-log "%s:\n%s" label text))))

(defun sapling-debug ()
  "Open the Sapling debug log buffer."
  (interactive)
  (let ((buffer (get-buffer-create sapling-debug-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'sapling-debug-mode)
        (sapling-debug-mode)))
    (pop-to-buffer buffer)
    (goto-char (point-max))))

(defun sapling-toggle-debug ()
  "Toggle Sapling command logging."
  (interactive)
  (setq sapling-debug (not sapling-debug))
  (sapling--debug-log "Debug mode %s"
                      (if sapling-debug "enabled" "disabled"))
  (message "Sapling debug %s" (if sapling-debug "enabled" "disabled")))

(defface sapling-header-face
  '((t :inherit bold))
  "Face for Sapling section headers.")

(defface sapling-status-modified-face
  '((t :foreground "orange"))
  "Face for modified files.")

(defface sapling-status-added-face
  '((t :foreground "green"))
  "Face for added files.")

(defface sapling-status-removed-face
  '((t :foreground "red"))
  "Face for removed files.")

(defface sapling-status-missing-face
  '((t :foreground "red" :weight bold))
  "Face for missing files.")

(defface sapling-status-unknown-face
  '((t :foreground "magenta"))
  "Face for untracked files.")

(defface sapling-status-ignored-face
  '((t :foreground "gray"))
  "Face for ignored files.")

(defface sapling-status-clean-face
  '((t :foreground "gray"))
  "Face for clean files.")

(defface sapling-marked-face
  '((t :weight bold :box (:line-width 1)))
  "Face for marked files in the status buffer.")

(defface sapling-log-changeset-face
  '((t :foreground "yellow"))
  "Face for changeset identifiers in Sapling smartlog output.")

;;; Buffer-local state

(defvar-local sapling--repo-root nil
  "Root directory of the Sapling repository for the current buffer.")

(defvar-local sapling--files nil
  "Working copy file list as (STATUS . FILE) entries.")

(defvar-local sapling--marked nil
  "List of marked files.")

(defvar-local sapling--commit-info nil
  "List (HASH BOOKMARK PHASE) for the current commit.")

(defvar-local sapling--smartlog nil
  "Most recent smartlog text for the status buffer.")

(defvar-local sapling--output-command nil
  "Command arguments used to populate the current output buffer.")

(defvar-local sapling--output-title nil
  "Title used by the current output buffer.")

(defvar-local sapling--output-directory nil
  "Directory in which the current output command runs.")

(defvar-local sapling--output-color nil
  "Non-nil when the current output command runs with ANSI colors.")

;;; Process helpers

(defun sapling--sl-program ()
  "Return the absolute path to the Sapling executable."
  (or (executable-find sapling-program)
      (error "Sapling executable `%s' not found; customize `sapling-program'"
             sapling-program)))

(defun sapling--process-command (args)
  "Return a process command list for running `sl' with ARGS.
On Windows, `.bat'/`.cmd' wrappers need to be run through the shell."
  (let* ((program (sapling--sl-program))
         (extension (file-name-extension program)))
    (if (member-ignore-case (or extension "") '("bat" "cmd" "com"))
        (list shell-file-name shell-command-switch
              (mapconcat #'shell-quote-argument (cons program args) " "))
      (cons program args))))

(defun sapling--color-args (args)
  "Return ARGS with ANSI color flags prepended.
`color.mode=ansi' forces ANSI escape sequences even on Windows,
where Sapling may otherwise use the native console color API."
  (append '("--config" "color.mode=ansi" "--color=always") args))

(defun sapling--process-environment ()
  "Return `process-environment' with Sapling automation variables.
When `sapling-use-color' is non-nil, allow colored output under Sapling's
automation mode."
  (let ((env (copy-sequence process-environment)))
    ;; HGPLAIN keeps output stable for scripts, and SL_AUTOMATION
    ;; avoids pagination and interactive prompts.  The latter normally
    ;; also disables color, so request color as an explicit exception.
    (dolist (var '("HGPLAIN=1" "SL_AUTOMATION=1"))
      (unless (member var env)
        (push var env)))
    (when sapling-use-color
      (unless (member "SL_AUTOMATION_EXCEPT=color" env)
        (push "SL_AUTOMATION_EXCEPT=color" env)))
    env))

(cl-defun sapling--run-async (args &key name callback directory color)
  "Run `sl' with ARGS asynchronously.

NAME is used to identify the process buffer.  When CALLBACK is
non-nil, it is called with the process output string and the exit
code (or nil if the process did not exit normally).  DIRECTORY, when
non-nil, is used as the process working directory.  When COLOR is
non-nil, run `sl' with ANSI colors enabled."
  (let* ((default-directory (or directory sapling--repo-root default-directory))
         (buffer (generate-new-buffer (format " *sapling-%s*" (or name "process"))))
         (process-environment (sapling--process-environment))
         (command (sapling--process-command (if color (sapling--color-args args) args))))
    ;; `w32-pipe-read-delay' is a global variable, so bindings around
    ;; `make-process' do not affect later reads.  Set it directly here.
    ;; This intentionally affects other Windows subprocess reads while
    ;; Sapling commands are running; see `sapling-w32-pipe-read-delay'.
    (when (and (boundp 'w32-pipe-read-delay)
               sapling-w32-pipe-read-delay)
      (setq w32-pipe-read-delay sapling-w32-pipe-read-delay))
    (sapling--debug-log "RUN: cd %S; %s" default-directory
                        (mapconcat #'shell-quote-argument command " "))
    (make-process
     :name (concat "sapling-" (or name "process"))
     :buffer buffer
     :command command
     ;; Decode process output with `utf-8-auto' (handles CRLF on
     ;; Windows) but encode command-line arguments with plain `utf-8'.
     ;; `utf-8-auto' on the encoding side would prepend a BOM to every
     ;; multibyte argument, which makes `sl -I FILE' patterns fail.
     :coding '(utf-8-auto . utf-8)
     :connection-type 'pipe
     :stderr buffer
     :noquery t
     :sentinel
     (lambda (proc _event)
       (let ((out (and (buffer-live-p buffer)
                       (with-current-buffer buffer (buffer-string))))
             (code (if (eq (process-status proc) 'exit)
                       (process-exit-status proc)
                     nil)))
         (sapling--debug-log "EXIT %s (%s)" (or name "process")
                             (or code "signal"))
         (sapling--debug-log-output "OUTPUT" out)
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (when callback
           (funcall callback out code)))))))

(defun sapling--call-output (directory &rest args)
  "Run `sl' synchronously in DIRECTORY with ARGS and return its output.
Signal an error if the command exits unsuccessfully."
  (let* ((default-directory (or directory sapling--repo-root default-directory))
         (process-environment (sapling--process-environment))
         (command (sapling--process-command args)))
    (sapling--debug-log "RUN: cd %S; %s" default-directory
                        (mapconcat #'shell-quote-argument command " "))
    (with-temp-buffer
      (let ((status (apply #'call-process (car command) nil (current-buffer) nil
                           (cdr command))))
        (sapling--debug-log "EXIT sync (%s)" (or status "signal"))
        (sapling--debug-log-output "OUTPUT" (buffer-string))
        (unless (eq status 0)
          (error "Sapling %s failed:\n%s"
                 (mapconcat #'identity args " ")
                 (buffer-string)))
        (buffer-string)))))

(defun sapling--find-root (directory)
  "Return the Sapling repository root containing DIRECTORY, or nil."
  (or (locate-dominating-file
       directory
       (lambda (dir) (file-directory-p (expand-file-name ".sl" dir))))
      (let ((out (ignore-errors
                   (sapling--call-output directory "root"))))
        (and out
             (let ((trimmed (string-trim out)))
               (unless (equal trimmed "")
                 (file-name-as-directory trimmed)))))))

;;; Status buffer

(defvar sapling-menu-table
  '((?s "status" "Show working copy status" sapling-status nil)
    (?l "smartlog" "Show smartlog" sapling-smartlog nil)
    (?L "log" "Show graph log" sapling-log nil)
    (?d "diff" "Show diff" sapling-diff nil)
    (?c "commit" "Commit changes" sapling-commit nil)
    (?a "amend" "Amend current commit" sapling-amend nil)
    (?x "absorb" "Absorb changes into stack" sapling-absorb nil)
    (?r "rebase" "Rebase onto revision" sapling-rebase t)
    (?f "fold" "Fold commits" sapling-fold t)
    (?g "graft" "Graft a commit" sapling-graft t)
    (?h "hide" "Hide a commit" sapling-hide t)
    (?H "unhide" "Unhide a commit" sapling-unhide t)
    (?z "shelve" "Shelve changes" sapling-shelve nil)
    (?Z "unshelve" "Unshelve changes" sapling-unshelve nil)
    (?n "next" "Check out next commit" sapling-next nil)
    (?p "previous" "Check out previous commit" sapling-previous nil)
    (?u "undo" "Undo local command" sapling-undo nil)
    (?R "redo" "Redo local command" sapling-redo nil)
    (?A "add" "Add files" sapling-add nil)
    (?D "remove" "Remove files" sapling-remove nil)
    (?K "forget" "Forget files" sapling-forget nil)
    (?V "revert" "Revert files" sapling-revert nil)
    (?e "metaedit" "Edit commit message" sapling-metaedit t)
    (?o "show" "Show current commit" sapling-show nil)
    (?J "journal" "Show journal" sapling-journal t)
    (?B "bookmark" "Create bookmark" sapling-bookmark-create t)
    (?F "pull" "Pull changes" sapling-pull nil)
    (?P "push" "Push changes" sapling-push nil)
    (?q "quit" "Quit" nil nil))
  "Menu entries shared by `sapling-menu' and `sapling-mode'.")

(defun sapling--header-key (key)
  "Return a display key for menu KEY."
  (cl-case key
    (?g "C-c g")
    (?u "C-c u")
    (?R "C-c R")
    (t (char-to-string key))))

(defun sapling--header-line ()
  "Return the status buffer header line, truncated to fit the window."
  (let* ((prefix '(("g" "refresh" sapling-refresh)
                   ("c" "commit" sapling-commit)
                   ("a" "amend" sapling-amend)
                   ("=" "diff" sapling-diff)
                   ("m" "mark" sapling-mark)))
         (seen (mapcar #'caddr prefix))
         (menu-items nil))
    (dolist (entry sapling-menu-table)
      (let ((cmd (cadddr entry)))
        (when (and cmd (not (memq cmd seen)))
          (push (list (sapling--header-key (car entry))
                      (cadr entry)
                      cmd)
                menu-items)
          (push cmd seen))))
    (let ((full (mapconcat (lambda (item)
                             (format "%s %s" (car item) (cadr item)))
                           (append prefix (nreverse menu-items))
                           " ")))
      (truncate-string-to-width
       full
       (max 10 (window-text-width))
       nil nil "..."))))


(defvar sapling-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g")   #'sapling-refresh)
    (define-key map (kbd "s")   #'sapling-status)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'describe-mode)
    (define-key map (kbd "RET") #'sapling-visit-file-at-point)
    (define-key map (kbd "SPC") #'sapling-diff-file-at-point)
    (define-key map (kbd "m")   #'sapling-mark)
    (define-key map (kbd "M")   #'sapling-mark-all)
    (define-key map (kbd "u")   #'sapling-unmark)
    (define-key map (kbd "U")   #'sapling-unmark-all)
    (define-key map (kbd "c")   #'sapling-commit)
    (define-key map (kbd "a")   #'sapling-amend)
    (define-key map (kbd "d")   #'sapling-diff)
    (define-key map (kbd "=")   #'sapling-diff)
    (define-key map (kbd "C-x v =") #'sapling-diff)
    (define-key map (kbd "l")   #'sapling-smartlog)
    (define-key map (kbd "L")   #'sapling-log)
    (define-key map (kbd "b")   #'sapling-smartlog)
    (define-key map (kbd "x")   #'sapling-absorb)
    (define-key map (kbd "r")   #'sapling-rebase)
    (define-key map (kbd "R")   #'sapling-rebase-continue)
    (define-key map (kbd "z")   #'sapling-shelve)
    (define-key map (kbd "Z")   #'sapling-unshelve)
    (define-key map (kbd "G")   #'sapling-goto)
    (define-key map (kbd "B")   #'sapling-bookmark-create)
    (define-key map (kbd "F")   #'sapling-pull)
    (define-key map (kbd "P")   #'sapling-push)
    (define-key map (kbd "n")   #'sapling-next)
    (define-key map (kbd "p")   #'sapling-previous)
    (define-key map (kbd "A")   #'sapling-add)
    (define-key map (kbd "D")   #'sapling-remove)
    (define-key map (kbd "K")   #'sapling-forget)
    (define-key map (kbd "V")   #'sapling-revert)
    (define-key map (kbd "e")   #'sapling-metaedit)
    (define-key map (kbd "o")   #'sapling-show)
    (define-key map (kbd "J")   #'sapling-journal)
    (define-key map (kbd "f")   #'sapling-fold)
    (define-key map (kbd "h")   #'sapling-hide)
    (define-key map (kbd "H")   #'sapling-unhide)
    (define-key map (kbd "C-c g") #'sapling-graft)
    (define-key map (kbd "C-c u") #'sapling-undo)
    (define-key map (kbd "C-c R") #'sapling-redo)
    (define-key map (kbd "C-c c") #'sapling-command)
    (define-key map (kbd "C-c d") #'sapling-toggle-debug)
    map)
  "Keymap for `sapling-mode'.")

(define-derived-mode sapling-mode special-mode "Sapling"
  "Major mode for Sapling status output.

\\{sapling-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sapling-refresh)
  (setq-local header-line-format '(:eval (sapling--header-line))))

;;;###autoload
(defun sapling-status ()
  "Open the Sapling status buffer for the current repository."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (pop-to-buffer (get-buffer-create sapling-status-buffer-name))
    (unless (derived-mode-p 'sapling-mode)
      (sapling-mode))
    (setq default-directory root
          sapling--repo-root root
          sapling--files nil
          sapling--marked nil
          sapling--commit-info nil
          sapling--smartlog nil)
    (sapling-refresh)))

(defun sapling-refresh ()
  "Refresh the current Sapling status buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (root (or (sapling--find-root default-directory) sapling--repo-root))
         (remaining 3)
         (smartlog nil)
         (files nil)
         (info nil))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (setq sapling--repo-root root)
    (sapling--render-loading)
    (let ((default-directory root))
      (sapling--run-async
       '("smartlog" "-T" "{node|short} {desc|firstline}\n")
       :name "smartlog" :directory root
       :callback
       (lambda (out code)
         (setq smartlog (if (and code (zerop code)) out ""))
         (setq remaining (1- remaining))
         (when (<= remaining 0)
           (sapling--finish-refresh buffer root smartlog files info))))
      (sapling--run-async
       '("status")
       :name "status" :directory root
       :callback
       (lambda (out code)
         (setq files (if (and code (zerop code))
                         (sapling--parse-status out)
                       nil))
         (setq remaining (1- remaining))
         (when (<= remaining 0)
           (sapling--finish-refresh buffer root smartlog files info))))
      (sapling--run-async
       '("log" "-r" "." "-T" "{node|short}\t{bookmarks}\t{phase}\n")
       :name "commit-info" :directory root
       :callback
       (lambda (out code)
         (setq info (if (and code (zerop code))
                        (sapling--parse-commit-info out)
                      nil))
         (setq remaining (1- remaining))
         (when (<= remaining 0)
           (sapling--finish-refresh buffer root smartlog files info)))))))

(defun sapling--finish-refresh (buffer root smartlog files info)
  "Populate BUFFER with refreshed Sapling data.
ROOT is the repository root, SMARTLOG is the smartlog text, FILES is
the status file list, and INFO is the current commit metadata."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq sapling--repo-root root
            sapling--smartlog (or smartlog "")
            sapling--files (or files nil)
            sapling--commit-info info)
      (sapling--render-status))))

(defun sapling--render-loading ()
  "Render a loading placeholder in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Refreshing Sapling status...\n" 'face 'italic)))
  (set-buffer-modified-p nil))

(defun sapling--strip-cr (string)
  "Return STRING with any trailing carriage return removed."
  (string-trim-right string "\r"))

(defun sapling--parse-status (text)
  "Parse `sl status' output TEXT into a list of (STATUS FILE)."
  (let (files)
    (dolist (line (split-string text "\n" t))
      (setq line (sapling--strip-cr line))
      (when (string-match "^\\([MARC!?I]\\) \\(.+\\)$" line)
        (push (list (match-string 1 line) (match-string 2 line)) files)))
    (nreverse files)))

(defun sapling--parse-commit-info (text)
  "Parse commit info line TEXT into (HASH BOOKMARK PHASE)."
  (let ((line (sapling--strip-cr (car (split-string text "\n" t)))))
    (when line
      (seq-take (split-string line "\t") 3))))

(defun sapling--status-face (status)
  "Return the face for STATUS character."
  (cl-case (aref status 0)
    (?M 'sapling-status-modified-face)
    (?A 'sapling-status-added-face)
    (?R 'sapling-status-removed-face)
    (?! 'sapling-status-missing-face)
    (?? 'sapling-status-unknown-face)
    (?I 'sapling-status-ignored-face)
    (?C 'sapling-status-clean-face)
    (t nil)))

(defun sapling--insert-smartlog (text)
  "Insert smartlog TEXT with changeset hashes colored."
  (let ((beg (point)))
    (insert text)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              "^\\(?:[^[:space:]]+[[:space:]]+\\)\\([0-9a-f]+\\)[[:space:]]"
              nil t)
        (add-face-text-property (match-beginning 1) (match-end 1)
                                'sapling-log-changeset-face)))))

(defun sapling--render-status ()
  "Render the Sapling status buffer from cached data."
  (let ((inhibit-read-only t)
        (info sapling--commit-info)
        (smartlog sapling--smartlog)
        (files sapling--files))
    (erase-buffer)
    (insert (propertize "Sapling status\n" 'face 'sapling-header-face))
    (insert (format "Repository: %s\n" (or sapling--repo-root "")))
    (when info
      (insert (format "Commit:     %s" (nth 0 info)))
      (when (and (nth 1 info) (not (equal (nth 1 info) "")))
        (insert (format "  [%s]" (nth 1 info))))
      (when (nth 2 info)
        (insert (format "  (%s)" (nth 2 info))))
      (insert "\n"))
    (insert "\n")
    (insert (propertize "Smartlog\n" 'face 'sapling-header-face))
    (if (and smartlog (not (equal smartlog "")))
        (sapling--insert-smartlog smartlog)
      (insert "  (no smartlog output)\n"))
    (insert "\n")
    (insert (propertize "Changes\n" 'face 'sapling-header-face))
    (if files
        (dolist (entry files)
          (sapling--insert-file-line (nth 0 entry) (nth 1 entry)))
      (insert "  (no changes)\n"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun sapling--insert-file-line (status file)
  "Insert a status line for STATUS and FILE with text properties."
  (let ((beg (point))
        (marked (member file sapling--marked)))
    (insert (format "  %s %s" status file))
    (let ((face (sapling--status-face status)))
      (when face
        (add-face-text-property beg (point) face)))
    (when marked
      (add-face-text-property beg (point) 'sapling-marked-face))
    (put-text-property beg (point) 'sapling-file file)
    (put-text-property beg (point) 'sapling-status status)
    (put-text-property beg (point) 'sapling-file-line t)
    (insert "\n")))

(defun sapling-file-at-point ()
  "Return the file represented by the line at point, or nil."
  (or (get-text-property (point) 'sapling-file)
      (save-excursion
        (beginning-of-line)
        (get-text-property (point) 'sapling-file))))

(defun sapling--goto-file (file)
  "Move point to FILE in the current status buffer."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal (get-text-property (point) 'sapling-file) file)))
    (forward-line 1)))

(defun sapling-mark ()
  "Toggle the mark on the file at point."
  (interactive)
  (let ((file (sapling-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (if (member file sapling--marked)
        (setq sapling--marked (delete file sapling--marked))
      (push file sapling--marked))
    (sapling--render-status)
    (sapling--goto-file file)))

(defun sapling-unmark ()
  "Remove the mark from the file at point."
  (interactive)
  (let ((file (sapling-file-at-point)))
    (when file
      (setq sapling--marked (delete file sapling--marked))
      (sapling--render-status)
      (sapling--goto-file file))))

(defun sapling-mark-all ()
  "Mark all files in the status buffer."
  (interactive)
  (setq sapling--marked (mapcar (lambda (entry) (nth 1 entry)) sapling--files))
  (sapling--render-status))

(defun sapling-unmark-all ()
  "Unmark all files in the status buffer."
  (interactive)
  (setq sapling--marked nil)
  (sapling--render-status))

(defun sapling-visit-file-at-point ()
  "Visit the file at point."
  (interactive)
  (let ((file (sapling-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (find-file-other-window (expand-file-name file sapling--repo-root))))

(defun sapling-diff-file-at-point ()
  "Diff the file at point."
  (interactive)
  (let ((file (sapling-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (sapling-diff (list file))))

;;; Output buffers

(defvar sapling-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `sapling-output-mode'.")

(defvar-local sapling--commit-amend nil
  "Non-nil when the commit buffer amends the current commit.")

(defvar-local sapling--commit-files nil
  "Files included by the commit buffer.")

(defvar-local sapling--commit-status-buffer nil
  "Status buffer to refresh after the commit finishes.")

(define-derived-mode sapling-output-mode special-mode "Sapling-Output"
  "Major mode for Sapling command output.

\\{sapling-output-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sapling-output-refresh))

(defvar sapling-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    ;; `diff-mode-map' does not inherit from `special-mode-map', so
    ;; provide the refresh and quit bindings explicitly.
    (define-key map (kbd "g") #'sapling-output-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `sapling-diff-mode'.")

(define-derived-mode sapling-diff-mode diff-mode "Sapling-Diff"
  "Major mode for Sapling diff output.

\\{sapling-diff-mode-map}"
  ;; This exists to make diff output read-only and refreshable; the
  ;; actual diff coloring is provided by `diff-mode' itself.
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sapling-output-refresh))

(defun sapling--render-output-loading ()
  "Render a loading placeholder in the current output buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize
             (format "Running sl %s...\n"
                     (mapconcat #'identity (or sapling--output-command '("...")) " "))
             'face 'italic)))
  (set-buffer-modified-p nil))

(defun sapling--colorize-region (beg end)
  "Translate ANSI color sequences between BEG and END into faces."
  (setq ansi-color-context-region nil)
  (ansi-color-apply-on-region beg end))

(defun sapling--render-output (title text code)
  "Render output TEXT with TITLE and process exit CODE."
  (let ((inhibit-read-only t)
        (text-beg nil))
    (erase-buffer)
    (insert (propertize title 'face 'sapling-header-face) "\n")
    (when text
      (setq text-beg (point))
      (insert text)
      (unless (bolp) (insert "\n"))
      (when (and sapling-use-color sapling--output-color)
        (sapling--colorize-region text-beg (point))))
    (unless (and code (zerop code))
      (insert (propertize
               (format "[sl exited with code %s]\n" (or code "unknown"))
               'face 'error)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun sapling-output-refresh ()
  "Refresh the current output buffer by rerunning its command."
  (interactive)
  (unless sapling--output-command
    (user-error "No Sapling command associated with this buffer"))
  (sapling--render-output-loading)
  (let ((buffer (current-buffer))
        (args sapling--output-command)
        (title sapling--output-title)
        (directory sapling--output-directory))
    (sapling--run-async
     args :name (or (car args) "output") :directory directory
     :color sapling--output-color
     :callback
     (lambda (out code)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (sapling--render-output title out code)))))))

(cl-defun sapling--show-output (buffer-name args title directory &key mode (color sapling-use-color))
  "Show output of `sl ARGS' in BUFFER-NAME with TITLE.
When MODE is non-nil, use it as the major mode for BUFFER-NAME
instead of `sapling-output-mode'.  When COLOR is non-nil, run `sl' with
ANSI colors enabled."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p (or mode 'sapling-output-mode))
        (funcall (or mode #'sapling-output-mode)))
      (setq default-directory directory
            sapling--output-command args
            sapling--output-title title
            sapling--output-directory directory
            sapling--output-color color)
      (sapling--render-output-loading))
    (pop-to-buffer buffer)
    (sapling--run-async
     args :name (or (car args) "output") :directory directory
     :color color
     :callback
     (lambda (out code)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (sapling--render-output title out code)))))))

(defun sapling--refresh-status-buffer ()
  "Refresh the Sapling status buffer if it exists."
  (let ((buffer (get-buffer sapling-status-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (when (derived-mode-p 'sapling-mode)
          (sapling-refresh))))))

(defun sapling--run-and-show (args title)
  "Run `sl' with ARGS and show its output under TITLE.
Refresh the status buffer after a successful run."
  (let* ((root (sapling--find-root default-directory))
         (buffer (get-buffer-create sapling-output-buffer-name)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'sapling-output-mode)
        (sapling-output-mode))
      (setq default-directory root
            sapling--output-command args
            sapling--output-title title
            sapling--output-directory root
            sapling--output-color sapling-use-color)
      (sapling--render-output-loading))
    (pop-to-buffer buffer)
    (sapling--run-async
     args :name (or (car args) "output") :directory root
     :color sapling-use-color
     :callback
     (lambda (out code)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (sapling--render-output title out code)))
       (when (and code (zerop code))
         (sapling--refresh-status-buffer))))))

;;; Command helpers

(defun sapling--marked-or-point-files ()
  "Return files marked or at point in the `sapling-mode' buffer.
Returns nil when the current buffer is not a status buffer."
  (when (derived-mode-p 'sapling-mode)
    (or sapling--marked
        (let ((file (sapling-file-at-point)))
          (and file (list file))))))

(defun sapling--read-files (prompt)
  "Read one or more file names using PROMPT."
  (let (files file)
    (while (progn
             (setq file (read-file-name
                         (format "%s (empty to finish): " prompt)
                         nil nil t nil
                         (lambda (name)
                           (not (file-directory-p name)))))
             (not (string-empty-p file)))
      (push file files))
    (nreverse files)))

;;;; Commands

;;;###autoload
(defun sapling-smartlog ()
  "Show the Sapling smartlog."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--show-output sapling-smartlog-buffer-name
                          '("smartlog")
                          "Sapling Smartlog"
                          root)))

;;;###autoload
(defun sapling-log ()
  "Show the Sapling graph log."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--show-output sapling-log-buffer-name
                          (list "log" "-G" "-l" (number-to-string sapling-log-limit))
                          "Sapling Log"
                          root)))

;;;###autoload
(defun sapling-show ()
  "Show the current Sapling commit."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--show-output sapling-output-buffer-name
                          '("show")
                          "Sapling Show"
                          root)))

;;;###autoload
(defun sapling-diff (&optional files)
  "Show the Sapling diff for FILES.
When FILES are marked in the status buffer, diff only those files;
otherwise diff the file at point, or the whole working copy when
called outside the status buffer."
  (interactive
   (list (and (derived-mode-p 'sapling-mode)
              (or sapling--marked
                  (let ((file (sapling-file-at-point)))
                    (and file (list file)))))))
  (let ((root (or (sapling--find-root default-directory) sapling--repo-root)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--show-output sapling-diff-buffer-name
                     (append '("diff")
                             (when sapling-diff-ignore-space-at-eol
                               '("--ignore-space-at-eol"))
                             files)
                     "Sapling Diff"
                     root
                     :mode (if sapling-diff-use-diff-mode
                               #'sapling-diff-mode
                             #'sapling-output-mode)
                     :color (and sapling-use-color
                                 (not sapling-diff-use-diff-mode)))))

;;;###autoload
(defun sapling-commit ()
  "Create a Sapling commit with an editable commit message."
  (interactive)
  (sapling--start-commit nil))

;;;###autoload
(defun sapling-amend ()
  "Amend the current Sapling commit with an editable commit message."
  (interactive)
  (sapling--start-commit t))

(defconst sapling-commit-cut-line
  "# ------------------------ >8 ------------------------"
  "Line separating the commit message from the comment block.")

(defun sapling--start-commit (amend)
  "Open a commit message buffer.
If AMEND is non-nil, amend the current commit instead of creating a
new one."
  (let* ((root (sapling--find-root default-directory))
         (status-buffer (and (derived-mode-p 'sapling-mode) (current-buffer)))
         (files (and status-buffer sapling--marked))
         (previous-message (when amend
                             (ignore-errors
                               (sapling--call-output
                                root "log" "-r" "." "-T" "{desc}"))))
         (buffer-name (if amend "*sapling-amend*" "*sapling-commit*")))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'sapling-commit-mode)
          (sapling-commit-mode))
        (setq default-directory root)
        (setq sapling--commit-amend amend
              sapling--commit-files files
              sapling--commit-status-buffer status-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when previous-message
            (insert previous-message)
            (unless (bolp) (insert "\n")))
          (insert "\n" sapling-commit-cut-line "\n")
          (insert (sapling--commit-comment root files amend)))
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (message "Describe your changes, then press C-c C-c to finish"))))

(defun sapling--commit-comment (root files amend)
  "Build the comment block for ROOT, FILES, and AMEND."
  (with-temp-buffer
    (insert (format "# Repository: %s\n" root))
    (if amend
        (insert "# Amending the current commit.\n")
      (insert "# Please enter the commit message for your changes.\n"))
    (insert "#\n# Changes:\n")
    (if files
        (dolist (file files)
          (insert (format "#   %s\n" file)))
      (insert "#   (all working copy changes)\n"))
    (buffer-string)))

(defun sapling--commit-message ()
  "Return the message portion of the current commit buffer."
  (let ((cut (save-excursion
               (goto-char (point-min))
               (re-search-forward
                (concat "^" (regexp-quote sapling-commit-cut-line) "$") nil t))))
    (string-trim
     (if cut
         (buffer-substring-no-properties (point-min) (match-beginning 0))
       (buffer-string)))))

(defun sapling-commit-finish ()
  "Finish the commit or amend in the current message buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (message-text (sapling--commit-message))
         (status-buffer sapling--commit-status-buffer)
         (root (or (sapling--find-root default-directory)
                   (when (buffer-live-p status-buffer)
                     (with-current-buffer status-buffer
                       sapling--repo-root))))
         (files sapling--commit-files)
         (amend sapling--commit-amend)
         (logfile (make-temp-file "sapling-message" nil ".txt")))
    (when (equal message-text "")
      (user-error "Empty commit message"))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (let ((coding-system-for-write 'utf-8))
      (write-region message-text nil logfile nil 'silent))
    (let ((args (append (if amend '("amend") '("commit"))
                        (list "-l" logfile)
                        (cl-loop for file in files
                                 append (list "-I" file)))))
      (message "Running sl %s..." (mapconcat #'identity args " "))
      (sapling--run-async
       args :name (if amend "amend" "commit") :directory root
       :callback
       (lambda (out code)
         (when (file-exists-p logfile)
           (delete-file logfile))
         (if (and code (zerop code))
             (progn
               (message (if amend "Amended" "Committed"))
               (when (buffer-live-p buffer)
                 (kill-buffer buffer))
               (when (buffer-live-p status-buffer)
                 (with-current-buffer status-buffer
                   (sapling-refresh))))
           (let ((action (if amend "amend" "commit"))
                 (error-buffer (get-buffer-create "*sapling-error*")))
             (with-current-buffer error-buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert (or out ""))))
             (pop-to-buffer error-buffer)
             (message "sl %s failed:\n%s" action (or out "")))))))))

(defun sapling-commit-cancel ()
  "Kill the commit message buffer, canceling the commit or amend."
  (interactive)
  (kill-buffer))

(defvar sapling-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'sapling-commit-finish)
    (define-key map (kbd "C-c C-k") #'sapling-commit-cancel)
    map)
  "Keymap for `sapling-commit-mode'.")

(define-derived-mode sapling-commit-mode text-mode "Sapling-Commit"
  "Major mode for editing a Sapling commit message.

\\{sapling-commit-mode-map}"
  (setq-local header-line-format
              (substitute-command-keys
               "Sapling commit: \\[sapling-commit-finish] finish, \
\\[sapling-commit-cancel] cancel")))

;;;###autoload
(defun sapling-absorb (&optional dry-run)
  "Absorb the working copy into the current stack.
When DRY-RUN is non-nil, show what would be absorbed instead of
applying it."
  (interactive "P")
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show
     (if dry-run '("absorb" "-n") '("absorb" "-a"))
     (if dry-run "Sapling Absorb (dry run)" "Sapling Absorb"))))

;;;###autoload
(defun sapling-rebase (destination)
  "Rebase the current stack onto DESTINATION."
  (interactive "sRebase onto revision: ")
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show (list "rebase" "-d" destination) "Sapling Rebase")))

;;;###autoload
(defun sapling-rebase-continue ()
  "Continue an interrupted Sapling rebase."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show '("rebase" "--continue") "Sapling Rebase Continue")))

;;;###autoload
(defun sapling-rebase-abort ()
  "Abort an interrupted Sapling rebase."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show '("rebase" "--abort") "Sapling Rebase Abort")))

;;;###autoload
(defun sapling-goto (revision)
  "Go to REVISION in the current Sapling repository."
  (interactive "sGoto revision: ")
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show (list "goto" revision) "Sapling Goto")))

;;;###autoload
(defun sapling-bookmark-create (name)
  "Create a Sapling bookmark named NAME at the current commit."
  (interactive "sBookmark name: ")
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show (list "bookmark" name) "Sapling Bookmark")))

;;;###autoload
(defun sapling-shelve ()
  "Shelve all pending modifications."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show '("shelve") "Sapling Shelve")))

;;;###autoload
(defun sapling-unshelve ()
  "Unshelve the most recent shelved change."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show '("unshelve") "Sapling Unshelve")))

;;;###autoload
(defun sapling-pull ()
  "Pull from the configured remote repository."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show '("pull") "Sapling Pull")))

;;;###autoload
(defun sapling-push ()
  "Push to the configured remote repository."
  (interactive)
  (let ((root (sapling--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--run-and-show '("push") "Sapling Push")))

;;;###autoload
(defun sapling-add (&optional files)
  "Add FILES to Sapling tracking.
With no files, add all untracked files.  In the status buffer,
marked files or the file at point are used when available."
  (interactive (list (sapling--marked-or-point-files)))
  (sapling--run-and-show (append '("add") files) "Sapling Add"))

;;;###autoload
(defun sapling-remove (&optional files)
  "Remove FILES from Sapling and delete them from disk."
  (interactive
   (list (or (sapling--marked-or-point-files)
             (sapling--read-files "Remove file"))))
  (sapling--run-and-show (append '("remove") files) "Sapling Remove"))

;;;###autoload
(defun sapling-forget (&optional files)
  "Stop tracking FILES without deleting them from disk."
  (interactive
   (list (or (sapling--marked-or-point-files)
             (sapling--read-files "Forget file"))))
  (sapling--run-and-show (append '("forget") files) "Sapling Forget"))

;;;###autoload
(defun sapling-revert (&optional files)
  "Revert FILES to their state in the current commit.
With no FILES, revert all pending changes.  A prefix argument skips
the confirmation prompt."
  (interactive (list (sapling--marked-or-point-files)))
  (when (and (null files)
             (not current-prefix-arg)
             (not (yes-or-no-p "Revert all pending changes? ")))
    (user-error "Aborted"))
  (sapling--run-and-show (append '("revert") files) "Sapling Revert"))

;;;###autoload
(defun sapling-clean (&optional dry-run)
  "Delete untracked files from the working copy.
When DRY-RUN is non-nil, print what would be deleted instead."
  (interactive "P")
  (unless dry-run
    (unless (yes-or-no-p "Delete all untracked files? ")
      (user-error "Aborted")))
  (sapling--run-and-show
   (if dry-run '("clean" "--print") '("clean"))
   (if dry-run "Sapling Clean (dry run)" "Sapling Clean")))

;;;###autoload
(defun sapling-uncommit (&optional files)
  "Uncommit the current commit, optionally only FILES."
  (interactive (list (sapling--marked-or-point-files)))
  (sapling--run-and-show (append '("uncommit") files) "Sapling Uncommit"))

;;;###autoload
(defun sapling-fold (revision)
  "Fold commits linearly from REVISION into the current commit."
  (interactive "sFold from revision: ")
  (sapling--run-and-show (list "fold" "--from" revision) "Sapling Fold"))

;;;###autoload
(defun sapling-hide (revision)
  "Hide REVISION and its descendants."
  (interactive "sHide revision: ")
  (sapling--run-and-show (list "hide" revision) "Sapling Hide"))

;;;###autoload
(defun sapling-unhide (revision)
  "Unhide REVISION and its ancestors."
  (interactive "sUnhide revision: ")
  (sapling--run-and-show (list "unhide" revision) "Sapling Unhide"))

;;;###autoload
(defun sapling-graft (revision)
  "Graft REVISION onto the current commit."
  (interactive "sGraft revision: ")
  (sapling--run-and-show (list "graft" revision) "Sapling Graft"))

;;;###autoload
(defun sapling-previous ()
  "Check out the previous commit in the current stack."
  (interactive)
  (sapling--run-and-show '("previous") "Sapling Previous"))

;;;###autoload
(defun sapling-next ()
  "Check out the next commit in the current stack."
  (interactive)
  (sapling--run-and-show '("next") "Sapling Next"))

;;;###autoload
(defun sapling-unamend ()
  "Undo the last amend operation on the current commit."
  (interactive)
  (sapling--run-and-show '("unamend") "Sapling Unamend"))

;;;###autoload
(defun sapling-undo (&optional arg)
  "Undo the last local Sapling command.
When ARG is non-nil, undo that many local commands."
  (interactive "P")
  (let ((args (cons "undo"
                    (and arg
                         (list (number-to-string
                                (prefix-numeric-value arg)))))))
    (sapling--run-and-show args "Sapling Undo")))

;;;###autoload
(defun sapling-redo (&optional arg)
  "Redo the last undone Sapling command.
When ARG is non-nil, redo that many local commands."
  (interactive "P")
  (let ((args (cons "redo"
                    (and arg
                         (list (number-to-string
                                (prefix-numeric-value arg)))))))
    (sapling--run-and-show args "Sapling Redo")))

;;;###autoload
(defun sapling-metaedit (message)
  "Edit the current commit message using `sl metaedit -m MESSAGE'."
  (interactive "sCommit message: ")
  (when (string-empty-p message)
    (user-error "Empty commit message"))
  (sapling--run-and-show (list "metaedit" "-m" message) "Sapling Metaedit"))

;;;###autoload
(defun sapling-grep (pattern)
  "Search tracked files for PATTERN using `sl grep -n'."
  (interactive "sSapling grep pattern: ")
  (sapling--run-and-show (list "grep" "-n" pattern) "Sapling Grep"))

;;;###autoload
(defun sapling-journal (&optional bookmark)
  "Show the Sapling journal, optionally for BOOKMARK."
  (interactive "sBookmark (empty for current): ")
  (sapling--run-and-show
   (if (or (null bookmark) (string-empty-p bookmark))
       '("journal")
     (list "journal" bookmark))
   "Sapling Journal"))

;;;###autoload
(defun sapling-web ()
  "Launch the Sapling Web GUI."
  (interactive)
  (sapling--run-and-show '("web") "Sapling Web"))

;;;###autoload
(defun sapling-annotate (revision file)
  "Annotate FILE at REVISION.
REVISION may be empty for the current commit.  In the status buffer,
the file at point is used as the initial FILE."
  (interactive
   (let ((file (or (and (derived-mode-p 'sapling-mode) (sapling-file-at-point))
                   (read-file-name "Annotate file: "))))
     (list (read-string "Revision (empty for current): ") file)))
  (let* ((root (or (sapling--find-root default-directory) sapling--repo-root))
         (args (append '("annotate")
                       (and (not (string-empty-p revision))
                            (list "-r" revision))
                       (list file))))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sapling--show-output sapling-output-buffer-name args "Sapling Annotate" root)))

;;;###autoload
(defun sapling-backout (revision)
  "Back out REVISION in the current repository."
  (interactive "sBackout revision: ")
  (sapling--run-and-show (list "backout" revision) "Sapling Backout"))

;;;###autoload
(defun sapling-bookmark-list ()
  "List Sapling bookmarks."
  (interactive)
  (sapling--run-and-show '("bookmark") "Sapling Bookmarks"))

;;;###autoload
(defun sapling-bookmark-delete (name)
  "Delete Sapling bookmark NAME."
  (interactive "sDelete bookmark: ")
  (sapling--run-and-show (list "bookmark" "--delete" name) "Sapling Bookmark Delete"))

;;;###autoload
(defun sapling-bookmark-rename (old-name new-name)
  "Rename Sapling bookmark OLD-NAME to NEW-NAME."
  (interactive "sRename bookmark: \nsNew name: ")
  (sapling--run-and-show
   (list "bookmark" "--rename" old-name new-name)
   "Sapling Bookmark Rename"))

;;;###autoload
(defun sapling-pr-list ()
  "List GitHub pull requests associated with this repository."
  (interactive)
  (sapling--run-and-show '("pr" "list") "Sapling PR List"))

;;;###autoload
(defun sapling-pr-submit ()
  "Submit GitHub pull requests for the current stack."
  (interactive)
  (sapling--run-and-show '("pr" "submit") "Sapling PR Submit"))

;;;###autoload
(defun sapling-pr-pull ()
  "Pull GitHub pull request data into the current repository."
  (interactive)
  (sapling--run-and-show '("pr" "pull") "Sapling PR Pull"))

;;;###autoload
(defun sapling-config ()
  "Show Sapling configuration."
  (interactive)
  (sapling--run-and-show '("config") "Sapling Config"))

;;;###autoload
(defun sapling-doctor ()
  "Run Sapling's doctor diagnostics."
  (interactive)
  (sapling--run-and-show '("doctor") "Sapling Doctor"))

;;;###autoload
(defun sapling-clone (source destination)
  "Clone SOURCE into DESTINATION using Sapling."
  (interactive "sClone source: \nsDestination: ")
  (sapling--show-output
   sapling-output-buffer-name
   (if (string-empty-p destination)
       (list "clone" source)
     (list "clone" source destination))
   "Sapling Clone"
   default-directory))

;;;###autoload
(defun sapling-init (directory)
  "Initialize a new Sapling repository in DIRECTORY.
An empty DIRECTORY initializes the current directory."
  (interactive "GInitialize repository in directory: ")
  (sapling--show-output
   sapling-output-buffer-name
   (if (string-empty-p directory)
       '("init")
     (list "init" directory))
   "Sapling Init"
   default-directory))

(defvar sapling-command-history nil
  "History for `sapling-command'.")

;;;###autoload
(defun sapling-command (&optional command)
  "Run a Sapling command selected from its documented command-line options.

This builds a lightweight, `completing-read'-based command menu from
`sl help commands' and `sl help COMMAND'.  It intentionally avoids
the transient UI so command selection stays fast on Windows.  The
actual Sapling process is still launched asynchronously by
`sapling--run-async'."
  (interactive)
  (let* ((commands (sapling--command-entries))
         (command (or command
                      (completing-read "Sapling command: "
                                       (mapcar #'car commands)
                                       nil t nil 'sapling-command-history)))
         (subcommands (sapling--command-subcommands command))
         (subcommand (when subcommands
                       (completing-read "Subcommand: " subcommands nil t)))
         (options (sapling--command-options command))
         (choices (mapcar (lambda (opt)
                            (cons (sapling--option-label opt) opt))
                          options))
         (selected (when choices
                     (completing-read-multiple
                      "Options (comma/space-separated): "
                      (mapcar #'car choices) nil t)))
         (args nil))
    (when subcommand
      (push subcommand args))
    (dolist (choice selected)
      (let* ((cell (assoc choice choices))
             (opt (cdr cell))
             (short (nth 0 opt))
             (long (nth 1 opt))
             (value (nth 2 opt)))
        (when opt
          (push (or short long) args)
          (when value
            (push (read-string (format "%s value: " value)) args)))))
    (sapling--run-and-show
     (cons command (nreverse args))
     (format "Sapling %s%s" command
             (if subcommand (format " %s" subcommand) "")))))

(defun sapling--command-entries ()
  "Return an alist of Sapling command names and descriptions."
  (let ((output (sapling--call-output default-directory "help" "commands"))
        (in-commands nil)
        entries)
    (dolist (line (split-string output "\n" t))
      (when (string-match-p "^Commands:$" line)
        (setq in-commands t))
      (when (and in-commands
                 (string-match "^ \\([a-zA-Z][a-zA-Z0-9-]*\\)[[:space:]]\\{2,\\}\\(.*\\)$" line))
        (push (cons (match-string 1 line)
                    (string-trim (match-string 2 line)))
              entries)))
    (nreverse entries)))

(defun sapling--command-subcommands (command)
  "Return subcommand names for COMMAND, if its usage exposes them."
  (let ((output (ignore-errors
                  (sapling--call-output default-directory "help" command))))
    (when (string-match "^sl [^ ]+[^\n]*<\\([^>]+\\)>" output)
      (let ((items (split-string (match-string 1 output) "|" t)))
        (delq nil (mapcar (lambda (item)
                            (unless (member item '("..." ".."))
                              item))
                          items))))))

(defun sapling--command-options (command)
  "Return parsed options for COMMAND."
  (let ((output (ignore-errors
                  (sapling--call-output default-directory "help" command)))
        (in-options nil)
        options)
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-match-p "^Options\\(?:[^:]*\\)?:" line)
        (setq in-options t))
       ((and in-options (string-match-p "^\\((some details hidden\\|(use 'sl help\\)" line))
        (setq in-options nil)))
      (when in-options
        (let ((option (sapling--parse-option-line line)))
          (when option
            (push option options)))))
    (nreverse options)))

(defun sapling--parse-option-line (line)
  "Parse one Sapling option help LINE.
Return (SHORT LONG VALUE REPEAT DESCRIPTION), or nil."
  (let ((case-fold-search nil)
        short long value repeat description)
    (cond
     ((string-match
       "^[[:space:]]+-\\([^-[:space:]]\\)[[:space:]]+--\\([^[:space:]]+\\)[[:space:]]+\\([A-Z][A-Z0-9_-]*\\)?[[:space:]]*\\(\\[+\\]\\)?[[:space:]]*\\(.*\\)$"
       line)
      (setq short (concat "-" (match-string 1 line))
            long (concat "--" (match-string 2 line))
            value (match-string 3 line)
            repeat (stringp (match-string 4 line))
            description (string-trim (match-string 5 line))))
     ((string-match
       "^[[:space:]]+--\\([^[:space:]]+\\)[[:space:]]+\\([A-Z][A-Z0-9_-]*\\)?[[:space:]]*\\(\\[+\\]\\)?[[:space:]]*\\(.*\\)$"
       line)
      (setq long (concat "--" (match-string 1 line))
            value (match-string 2 line)
            repeat (stringp (match-string 3 line))
            description (string-trim (match-string 4 line))))
     ((string-match
       "^[[:space:]]+-\\([^-[:space:]]\\)[[:space:]]+\\([A-Z][A-Z0-9_-]*\\)?[[:space:]]*\\(\\[+\\]\\)?[[:space:]]*\\(.*\\)$"
       line)
      (setq short (concat "-" (match-string 1 line))
            value (match-string 2 line)
            repeat (stringp (match-string 3 line))
            description (string-trim (match-string 4 line)))))
    (when (or short long)
      (list short long value repeat description))))

(defun sapling--option-label (option)
  "Return a `completing-read' label for OPTION."
  (let ((short (nth 0 option))
        (long (nth 1 option))
        (value (nth 2 option))
        (repeat (nth 3 option))
        (description (nth 4 option)))
    (concat
     (mapconcat #'identity (delq nil (list short long)) ", ")
     (when value (concat " " value))
     (when repeat " [+]")
     (when (and description (not (string-empty-p description)))
       (concat "  " description)))))

;;;###autoload
(defun sapling-menu ()
  "Display a Magit-style dispatch menu for Sapling commands."
  (interactive)
  (let* ((choices (mapcar (lambda (item)
                            (list (car item) (cadr item) (caddr item)))
                          sapling-menu-table))
         (entry (read-multiple-choice "Sapling" choices))
         (choice (car entry))
         (selected (and choice (assq choice sapling-menu-table))))
    (pcase selected
      (`(,_ ,_ ,_ ,cmd ,interactivep)
       (when cmd
         (if interactivep
             (funcall-interactively cmd)
           (funcall cmd))))
      (_ nil))))

(provide 'sapling)

;;; sapling.el ends here
