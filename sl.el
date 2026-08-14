;;; sl.el --- Magit-like interface for Sapling  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Swithin Chan <swithinchan@yahoo.com.hk>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, vc
;; URL: https://github.com/swithinchan/sl/

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

;; A fast, Magit-inspired interface for the Sapling SCM (`sl').
;;
;; The status buffer is the main entry point:
;;
;;   M-x sl-status
;;
;; It intentionally uses the same command names as Sapling where possible
;; (`commit', `amend', `absorb', `rebase', `shelve', `smartlog', ...).
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

(defgroup sl nil
  "Sapling SCM interface."
  :group 'tools
  :group 'vc)

(defcustom sl-program "sl"
  "Name of or path to the Sapling executable."
  :type 'string
  :group 'sl)

(defcustom sl-status-buffer-name "*sl*"
  "Name of the Sapling status buffer."
  :type 'string
  :group 'sl)

(defcustom sl-smartlog-buffer-name "*sl-smartlog*"
  "Name of the Sapling smartlog buffer."
  :type 'string
  :group 'sl)

(defcustom sl-log-buffer-name "*sl-log*"
  "Name of the Sapling graph log buffer."
  :type 'string
  :group 'sl)

(defcustom sl-diff-buffer-name "*sl-diff*"
  "Name of the Sapling diff buffer."
  :type 'string
  :group 'sl)

(defcustom sl-output-buffer-name "*sl-output*"
  "Name of the generic Sapling output buffer."
  :type 'string
  :group 'sl)

(defcustom sl-log-limit 100
  "Number of commits to show in `sl-log'."
  :type 'integer
  :group 'sl)

(defcustom sl-w32-pipe-read-delay 0
  "Value for `w32-pipe-read-delay' while reading `sl' output.
Lower values make process output significantly faster on Windows."
  :type 'integer
  :group 'sl)

(defcustom sl-use-color t
  "When non-nil, colorize Sapling command output.
Sl commands that display text are run with ANSI color enabled and
`ansi-color' translates the SGR sequences into Emacs faces.  This
works on Windows as well as on Unix."
  :type 'boolean
  :group 'sl)

(defcustom sl-diff-use-diff-mode t
  "When non-nil, show `sl-diff' output in `diff-mode'.
This gives Emacs-native diff coloring (removed lines in red, added
lines in green) without parsing Sapling's terminal color codes."
  :type 'boolean
  :group 'sl)

(defface sl-header-face
  '((t :inherit bold))
  "Face for Sapling section headers."
  :group 'sl)

(defface sl-status-modified-face
  '((t :foreground "orange"))
  "Face for modified files."
  :group 'sl)

(defface sl-status-added-face
  '((t :foreground "green"))
  "Face for added files."
  :group 'sl)

(defface sl-status-removed-face
  '((t :foreground "red"))
  "Face for removed files."
  :group 'sl)

(defface sl-status-missing-face
  '((t :foreground "red" :weight bold))
  "Face for missing files."
  :group 'sl)

(defface sl-status-unknown-face
  '((t :foreground "magenta"))
  "Face for untracked files."
  :group 'sl)

(defface sl-status-ignored-face
  '((t :foreground "gray"))
  "Face for ignored files."
  :group 'sl)

(defface sl-status-clean-face
  '((t :foreground "gray"))
  "Face for clean files."
  :group 'sl)

(defface sl-marked-face
  '((t :weight bold :box (:line-width 1)))
  "Face for marked files in the status buffer."
  :group 'sl)

(defface sl-log-changeset-face
  '((t :foreground "yellow"))
  "Face for changeset identifiers in Sapling smartlog output."
  :group 'sl)

;;; Buffer-local state

(defvar-local sl--repo-root nil
  "Root directory of the Sapling repository for the current buffer.")

(defvar-local sl--files nil
  "Working copy file list as (STATUS . FILE) entries.")

(defvar-local sl--marked nil
  "List of marked files.")

(defvar-local sl--commit-info nil
  "List (HASH BOOKMARK PHASE) for the current commit.")

(defvar-local sl--smartlog nil
  "Most recent smartlog text for the status buffer.")

(defvar-local sl--output-command nil
  "Command arguments used to populate the current output buffer.")

(defvar-local sl--output-title nil
  "Title used by the current output buffer.")

(defvar-local sl--output-directory nil
  "Directory in which the current output command runs.")

(defvar-local sl--output-color nil
  "Non-nil when the current output command runs with ANSI colors.")

;;; Process helpers

(defun sl--windows-p ()
  "Return non-nil when running on a Windows system."
  (memq system-type '(ms-dos windows-nt cygwin)))

(defun sl--sl-program ()
  "Return the absolute path to the Sapling executable."
  (or (executable-find sl-program)
      (error "Sapling executable `%s' not found; customize `sl-program'"
             sl-program)))

(defun sl--process-command (args)
  "Return a process command list for running `sl' with ARGS.
On Windows, `.bat'/`.cmd' wrappers need to be run through the shell."
  (let* ((program (sl--sl-program))
         (extension (downcase (or (file-name-extension program) ""))))
    (if (member extension '("bat" "cmd" "com"))
        (list shell-file-name shell-command-switch
              (mapconcat #'shell-quote-argument (cons program args) " "))
      (cons program args))))

(defun sl--color-args (args)
  "Return ARGS with ANSI color flags prepended.
`color.mode=ansi' forces ANSI escape sequences even on Windows,
where Sapling may otherwise use the native console color API."
  (append '("--config" "color.mode=ansi" "--color=always") args))

(defun sl--process-environment ()
  "Return `process-environment' with Sapling automation variables.
When `sl-use-color' is non-nil, allow colored output under Sapling's
automation mode."
  (let ((env (copy-sequence process-environment)))
    (dolist (var '("HGPLAIN=1" "SL_AUTOMATION=1"))
      (unless (member var env)
        (push var env)))
    (when sl-use-color
      (unless (member "SL_AUTOMATION_EXCEPT=color" env)
        (push "SL_AUTOMATION_EXCEPT=color" env)))
    env))

(cl-defun sl--run-async (args &key name callback directory color)
  "Run `sl' with ARGS asynchronously.

When CALLBACK is non-nil it is called with two arguments: the
process output string and the exit code (or nil if the process did
not exit normally).  DIRECTORY, when non-nil, is used as the process
working directory.  When COLOR is non-nil, run `sl' with ANSI colors
enabled."
  (let* ((default-directory (or directory sl--repo-root default-directory))
         (buffer (generate-new-buffer (format " *sl-%s*" (or name "process"))))
         (process-environment (sl--process-environment)))
    ;; `w32-pipe-read-delay' is a global variable, so bindings around
    ;; `make-process' do not affect later reads.  Set it directly here.
    (when (and (boundp 'w32-pipe-read-delay)
               sl-w32-pipe-read-delay)
      (setq w32-pipe-read-delay sl-w32-pipe-read-delay))
    (make-process
     :name (concat "sl-" (or name "process"))
     :buffer buffer
     :command (sl--process-command (if color (sl--color-args args) args))
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
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (when callback
           (funcall callback out code)))))))

(defun sl--call-output (args &optional directory)
  "Run `sl' with ARGS synchronously in DIRECTORY and return its output.
Signal an error if the command exits unsuccessfully."
  (let* ((default-directory (or directory sl--repo-root default-directory))
         (process-environment (sl--process-environment))
         (command (sl--process-command args)))
    (with-temp-buffer
      (let ((status (apply #'call-process (car command) nil (current-buffer) nil
                           (cdr command))))
        (unless (eq status 0)
          (error "sl %s failed:\n%s"
                 (mapconcat #'identity args " ")
                 (buffer-string)))
        (buffer-string)))))

(defun sl--find-root (directory)
  "Return the Sapling repository root containing DIRECTORY, or nil."
  (or (locate-dominating-file
       directory
       (lambda (dir) (file-directory-p (expand-file-name ".sl" dir))))
      (let ((out (ignore-errors
                   (sl--call-output '("root") directory))))
        (and out
             (let ((trimmed (string-trim out)))
               (unless (equal trimmed "")
                 (file-name-as-directory trimmed)))))))

;;; Status buffer

(defvar sl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g")   #'sl-refresh)
    (define-key map (kbd "s")   #'sl-status)
    (define-key map (kbd "q")   #'sl-quit)
    (define-key map (kbd "?")   #'sl-help)
    (define-key map (kbd "RET") #'sl-visit-file-at-point)
    (define-key map (kbd "SPC") #'sl-diff-file-at-point)
    (define-key map (kbd "m")   #'sl-mark)
    (define-key map (kbd "M")   #'sl-mark-all)
    (define-key map (kbd "u")   #'sl-unmark)
    (define-key map (kbd "U")   #'sl-unmark-all)
    (define-key map (kbd "c")   #'sl-commit)
    (define-key map (kbd "a")   #'sl-amend)
    (define-key map (kbd "d")   #'sl-diff)
    (define-key map (kbd "=")   #'sl-diff)
    (define-key map (kbd "C-x v =") #'sl-diff)
    (define-key map (kbd "l")   #'sl-log)
    (define-key map (kbd "b")   #'sl-smartlog)
    (define-key map (kbd "x")   #'sl-absorb)
    (define-key map (kbd "r")   #'sl-rebase)
    (define-key map (kbd "R")   #'sl-rebase-continue)
    (define-key map (kbd "z")   #'sl-shelve)
    (define-key map (kbd "Z")   #'sl-unshelve)
    (define-key map (kbd "G")   #'sl-goto)
    (define-key map (kbd "B")   #'sl-bookmark-create)
    (define-key map (kbd "F")   #'sl-pull)
    (define-key map (kbd "P")   #'sl-push)
    (define-key map (kbd "n")   #'sl-next)
    (define-key map (kbd "p")   #'sl-previous)
    (define-key map (kbd "A")   #'sl-add)
    (define-key map (kbd "D")   #'sl-remove)
    (define-key map (kbd "K")   #'sl-forget)
    (define-key map (kbd "V")   #'sl-revert)
    (define-key map (kbd "e")   #'sl-metaedit)
    (define-key map (kbd "o")   #'sl-show)
    (define-key map (kbd "J")   #'sl-journal)
    map)
  "Keymap for `sl-mode'.")

(define-derived-mode sl-mode special-mode "Sapling"
  "Major mode for Sapling status output.

\\{sl-mode-map}"
  :group 'sl
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sl-refresh)
  (setq-local header-line-format
              (substitute-command-keys
               "Sapling: \\[sl-refresh] refresh, \\[sl-commit] commit, \
\\[sl-amend] amend, \\[sl-diff] diff, \\[sl-mark] mark")))

;;;###autoload
(defun sl-status ()
  "Open the Sapling status buffer for the current repository."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (pop-to-buffer (get-buffer-create sl-status-buffer-name))
    (unless (derived-mode-p 'sl-mode)
      (sl-mode))
    (setq sl--repo-root root
          sl--files nil
          sl--marked nil
          sl--commit-info nil
          sl--smartlog nil)
    (sl-refresh)))

(defun sl-quit ()
  "Quit the current Sapling buffer."
  (interactive)
  (quit-window))

(defun sl-help ()
  "Show help for the current Sapling mode."
  (interactive)
  (describe-mode))

(defun sl-refresh ()
  "Refresh the current Sapling status buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (root (or sl--repo-root (sl--find-root default-directory)))
         (remaining 3)
         (smartlog nil)
         (files nil)
         (info nil))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (setq sl--repo-root root)
    (sl--render-loading)
    (let ((default-directory root))
      (sl--run-async
       '("smartlog" "-T" "{node|short} {desc|firstline}\n")
       :name "smartlog" :directory root
       :callback
       (lambda (out code)
         (setq smartlog (if (and code (zerop code)) out ""))
         (setq remaining (1- remaining))
         (when (<= remaining 0)
           (sl--finish-refresh buffer root smartlog files info))))
      (sl--run-async
       '("status")
       :name "status" :directory root
       :callback
       (lambda (out code)
         (setq files (if (and code (zerop code))
                         (sl--parse-status out)
                       nil))
         (setq remaining (1- remaining))
         (when (<= remaining 0)
           (sl--finish-refresh buffer root smartlog files info))))
      (sl--run-async
       '("log" "-r" "." "-T" "{node|short}\t{bookmarks}\t{phase}\n")
       :name "commit-info" :directory root
       :callback
       (lambda (out code)
         (setq info (if (and code (zerop code))
                        (sl--parse-commit-info out)
                      nil))
         (setq remaining (1- remaining))
         (when (<= remaining 0)
           (sl--finish-refresh buffer root smartlog files info)))))))

(defun sl--finish-refresh (buffer root smartlog files info)
  "Populate BUFFER with refreshed Sapling data."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq sl--repo-root root
            sl--smartlog (or smartlog "")
            sl--files (or files nil)
            sl--commit-info info)
      (sl--render-status))))

(defun sl--render-loading ()
  "Render a loading placeholder in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Refreshing Sapling status...\n" 'face 'italic)))
  (set-buffer-modified-p nil))

(defun sl--strip-cr (string)
  "Return STRING without a trailing carriage return, if any."
  (if (string-suffix-p "\r" string)
      (substring string 0 -1)
    string))

(defun sl--parse-status (text)
  "Parse `sl status' output TEXT into a list of (STATUS FILE)."
  (let (files)
    (dolist (line (split-string text "\n" t))
      (setq line (sl--strip-cr line))
      (when (string-match "^\\([MARC!?I]\\) \\(.+\\)$" line)
        (push (list (match-string 1 line) (match-string 2 line)) files)))
    (nreverse files)))

(defun sl--parse-commit-info (text)
  "Parse commit info line TEXT into (HASH BOOKMARK PHASE)."
  (let ((line (sl--strip-cr (car (split-string text "\n" t)))))
    (when line
      (let ((parts (split-string line "\t")))
        (list (nth 0 parts) (nth 1 parts) (nth 2 parts))))))

(defun sl--status-face (status)
  "Return the face for STATUS character."
  (cl-case (aref status 0)
    (?M 'sl-status-modified-face)
    (?A 'sl-status-added-face)
    (?R 'sl-status-removed-face)
    (?! 'sl-status-missing-face)
    (?? 'sl-status-unknown-face)
    (?I 'sl-status-ignored-face)
    (?C 'sl-status-clean-face)
    (t nil)))

(defun sl--insert-smartlog (text)
  "Insert smartlog TEXT with changeset hashes colored."
  (let ((beg (point)))
    (insert text)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              "^\\(?:[^[:space:]]+[[:space:]]+\\)\\([0-9a-f]+\\)[[:space:]]"
              nil t)
        (add-face-text-property (match-beginning 1) (match-end 1)
                                'sl-log-changeset-face)))))

(defun sl--render-status ()
  "Render the Sapling status buffer from cached data."
  (let ((inhibit-read-only t)
        (info sl--commit-info)
        (smartlog sl--smartlog)
        (files sl--files))
    (erase-buffer)
    (insert (propertize "Sapling status\n" 'face 'sl-header-face))
    (insert (format "Repository: %s\n" (or sl--repo-root "")))
    (when info
      (insert (format "Commit:     %s" (nth 0 info)))
      (when (and (nth 1 info) (not (equal (nth 1 info) "")))
        (insert (format "  [%s]" (nth 1 info))))
      (when (nth 2 info)
        (insert (format "  (%s)" (nth 2 info))))
      (insert "\n"))
    (insert "\n")
    (insert (propertize "Smartlog\n" 'face 'sl-header-face))
    (if (and smartlog (not (equal smartlog "")))
        (sl--insert-smartlog smartlog)
      (insert "  (no smartlog output)\n"))
    (insert "\n")
    (insert (propertize "Changes\n" 'face 'sl-header-face))
    (if files
        (dolist (entry files)
          (sl--insert-file-line (nth 0 entry) (nth 1 entry)))
      (insert "  (no changes)\n"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun sl--insert-file-line (status file)
  "Insert a status FILE line with text properties."
  (let ((beg (point))
        (marked (member file sl--marked)))
    (insert (format "  %s %s" status file))
    (let ((face (sl--status-face status)))
      (when face
        (add-face-text-property beg (point) face)))
    (when marked
      (add-face-text-property beg (point) 'sl-marked-face))
    (put-text-property beg (point) 'sl-file file)
    (put-text-property beg (point) 'sl-status status)
    (put-text-property beg (point) 'sl-file-line t)
    (insert "\n")))

(defun sl-file-at-point ()
  "Return the file represented by the line at point, or nil."
  (or (get-text-property (point) 'sl-file)
      (save-excursion
        (beginning-of-line)
        (get-text-property (point) 'sl-file))))

(defun sl--goto-file (file)
  "Move point to FILE in the current status buffer."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal (get-text-property (point) 'sl-file) file)))
    (forward-line 1)))

(defun sl-mark ()
  "Toggle the mark on the file at point."
  (interactive)
  (let ((file (sl-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (if (member file sl--marked)
        (setq sl--marked (delete file sl--marked))
      (push file sl--marked))
    (sl--render-status)
    (sl--goto-file file)))

(defun sl-unmark ()
  "Remove the mark from the file at point."
  (interactive)
  (let ((file (sl-file-at-point)))
    (when file
      (setq sl--marked (delete file sl--marked))
      (sl--render-status)
      (sl--goto-file file))))

(defun sl-mark-all ()
  "Mark all files in the status buffer."
  (interactive)
  (setq sl--marked (mapcar (lambda (entry) (nth 1 entry)) sl--files))
  (sl--render-status))

(defun sl-unmark-all ()
  "Remove all marks in the status buffer."
  (interactive)
  (setq sl--marked nil)
  (sl--render-status))

(defun sl-visit-file-at-point ()
  "Visit the file at point."
  (interactive)
  (let ((file (sl-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (find-file-other-window (expand-file-name file sl--repo-root))))

(defun sl-diff-file-at-point ()
  "Diff the file at point."
  (interactive)
  (let ((file (sl-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (sl-diff (list file))))

;;; Output buffers

(defvar sl-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'sl-output-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `sl-output-mode'.")

(defvar-local sl--commit-amend nil
  "Non-nil when the commit buffer amends the current commit.")

(defvar-local sl--commit-files nil
  "Files included by the commit buffer.")

(defvar-local sl--commit-status-buffer nil
  "Status buffer to refresh after the commit finishes.")

(define-derived-mode sl-output-mode special-mode "Sapling-Output"
  "Major mode for Sapling command output.

\\{sl-output-mode-map}"
  :group 'sl
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sl-output-refresh))

(defvar sl-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "g") #'sl-output-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `sl-diff-mode'.")

(define-derived-mode sl-diff-mode diff-mode "Sapling-Diff"
  "Major mode for Sapling diff output.

\\{sl-diff-mode-map}"
  :group 'sl
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sl-output-refresh))

(defun sl--render-output-loading ()
  "Render a loading placeholder in the current output buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize
             (format "Running sl %s...\n"
                     (mapconcat #'identity (or sl--output-command '("...")) " "))
             'face 'italic)))
  (set-buffer-modified-p nil))

(defun sl--colorize-region (beg end)
  "Translate ANSI color sequences between BEG and END into faces."
  (setq ansi-color-context-region nil)
  (ansi-color-apply-on-region beg end))

(defun sl--render-output (title text code)
  "Render output TEXT with TITLE and process exit CODE."
  (let ((inhibit-read-only t)
        (text-beg nil))
    (erase-buffer)
    (insert (propertize (format "%s\n" title) 'face 'sl-header-face))
    (when text
      (setq text-beg (point))
      (insert text)
      (unless (bolp) (insert "\n"))
      (when (and sl-use-color sl--output-color)
        (sl--colorize-region text-beg (point))))
    (unless (and code (zerop code))
      (insert (propertize
               (format "[sl exited with code %s]\n" (or code "unknown"))
               'face 'error)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun sl-output-refresh ()
  "Refresh the current output buffer by rerunning its command."
  (interactive)
  (unless sl--output-command
    (user-error "No Sapling command associated with this buffer"))
  (sl--render-output-loading)
  (let ((buffer (current-buffer))
        (args sl--output-command)
        (title sl--output-title)
        (directory sl--output-directory))
    (sl--run-async
     args :name (or (car args) "output") :directory directory
     :color sl--output-color
     :callback
     (lambda (out code)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (sl--render-output title out code)))))))

(cl-defun sl--show-output (buffer-name args title directory &key mode (color sl-use-color))
  "Show output of `sl ARGS' in BUFFER-NAME with TITLE.
When MODE is non-nil, use it as the major mode for BUFFER-NAME
instead of `sl-output-mode'.  When COLOR is non-nil, run `sl' with
ANSI colors enabled."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p (or mode 'sl-output-mode))
        (funcall (or mode #'sl-output-mode)))
      (setq default-directory directory
            sl--output-command args
            sl--output-title title
            sl--output-directory directory
            sl--output-color color)
      (sl--render-output-loading))
    (pop-to-buffer buffer)
    (sl--run-async
     args :name (or (car args) "output") :directory directory
     :color color
     :callback
     (lambda (out code)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (sl--render-output title out code)))))))

(defun sl--refresh-status-buffer ()
  "Refresh the Sapling status buffer if it exists."
  (let ((buffer (get-buffer sl-status-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (when (derived-mode-p 'sl-mode)
          (sl-refresh))))))

(defun sl--run-and-show (args title)
  "Run `sl ARGS' and show its output, refreshing the status on success."
  (let* ((root (sl--find-root default-directory))
         (buffer (get-buffer-create sl-output-buffer-name)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'sl-output-mode)
        (sl-output-mode))
      (setq default-directory root
            sl--output-command args
            sl--output-title title
            sl--output-directory root
            sl--output-color sl-use-color)
      (sl--render-output-loading))
    (pop-to-buffer buffer)
    (sl--run-async
     args :name (or (car args) "output") :directory root
     :color sl-use-color
     :callback
     (lambda (out code)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (sl--render-output title out code)))
       (when (and code (zerop code))
         (sl--refresh-status-buffer))))))

;;; Command helpers

(defun sl--marked-or-point-files ()
  "Return files marked or at point in the `sl-mode' buffer.
Returns nil when the current buffer is not a status buffer."
  (when (derived-mode-p 'sl-mode)
    (or sl--marked
        (let ((file (sl-file-at-point)))
          (and file (list file))))))

(defun sl--read-files (prompt)
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

;;; Commands

;;;###autoload
(defun sl-smartlog ()
  "Show the Sapling smartlog."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--show-output sl-smartlog-buffer-name
                          '("smartlog")
                          "Sapling Smartlog"
                          root)))

;;;###autoload
(defun sl-log ()
  "Show the Sapling graph log."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--show-output sl-log-buffer-name
                          (list "log" "-G" "-l" (number-to-string sl-log-limit))
                          "Sapling Log"
                          root)))

;;;###autoload
(defun sl-show ()
  "Show the current Sapling commit."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--show-output sl-output-buffer-name
                          '("show")
                          "Sapling Show"
                          root)))

;;;###autoload
(defun sl-diff (&optional files)
  "Show the Sapling diff.
When files are marked in the status buffer, diff only those files;
otherwise diff the file at point, or the whole working copy when
called outside the status buffer."
  (interactive
   (list (and (derived-mode-p 'sl-mode)
              (or sl--marked
                  (let ((file (sl-file-at-point)))
                    (and file (list file)))))))
  (let ((root (or sl--repo-root (sl--find-root default-directory))))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--show-output sl-diff-buffer-name
                     (append '("diff") files)
                     "Sapling Diff"
                     root
                     :mode (if sl-diff-use-diff-mode
                               #'sl-diff-mode
                             #'sl-output-mode)
                     :color (and sl-use-color
                                 (not sl-diff-use-diff-mode)))))

;;;###autoload
(defun sl-commit ()
  "Create a Sapling commit with an editable commit message."
  (interactive)
  (sl--start-commit nil))

;;;###autoload
(defun sl-amend ()
  "Amend the current Sapling commit with an editable commit message."
  (interactive)
  (sl--start-commit t))

(defun sl--start-commit (amend)
  "Open a commit message buffer.  If AMEND is non-nil, amend instead."
  (let* ((root (sl--find-root default-directory))
         (status-buffer (and (derived-mode-p 'sl-mode) (current-buffer)))
         (files (and status-buffer sl--marked))
         (previous-message (when amend
                             (ignore-errors
                               (sl--call-output
                                '("log" "-r" "." "-T" "{desc}") root))))
         (buffer-name (if amend "*sl-amend*" "*sl-commit*")))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'sl-commit-mode)
          (sl-commit-mode))
        (setq default-directory root)
        (setq sl--commit-amend amend
              sl--commit-files files
              sl--commit-status-buffer status-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when previous-message
            (insert previous-message)
            (unless (bolp) (insert "\n")))
          (insert "\n# ------------------------ >8 ------------------------\n")
          (insert (sl--commit-comment root files amend)))
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (message "Describe your changes, then press C-c C-c to finish"))))

(defun sl--commit-comment (root files amend)
  "Build the comment block for a commit message buffer."
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

(defun sl--commit-message ()
  "Return the message portion of the current commit buffer."
  (let ((cut (save-excursion
               (goto-char (point-min))
               (re-search-forward
                "^# ------------------------ >8 ------------------------$" nil t))))
    (string-trim
     (if cut
         (buffer-substring-no-properties (point-min) (match-beginning 0))
       (buffer-string)))))

(defun sl-commit-finish ()
  "Finish the commit or amend in the current message buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (message-text (sl--commit-message))
         (root default-directory)
         (files sl--commit-files)
         (amend sl--commit-amend)
         (status-buffer sl--commit-status-buffer)
         (logfile (make-temp-file "sl-message" nil ".txt")))
    (when (equal message-text "")
      (user-error "Empty commit message"))
    (let ((coding-system-for-write 'utf-8))
      (write-region message-text nil logfile nil 'silent))
    (let ((args (append (if amend '("amend") '("commit"))
                        (list "-l" logfile)
                        (cl-loop for file in files
                                 append (list "-I" file)))))
      (message "Running sl %s..." (mapconcat #'identity args " "))
      (sl--run-async
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
                   (sl-refresh))))
           (progn
             (message "sl %s failed" (if amend "amend" "commit"))
             (when (and out (not (equal out "")))
               (display-buffer
                (with-current-buffer (get-buffer-create "*sl-error*")
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert out))
                  (current-buffer)))))))))))

(defun sl-commit-cancel ()
  "Cancel the commit or amend in the current message buffer."
  (interactive)
  (kill-buffer))

(defvar sl-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'sl-commit-finish)
    (define-key map (kbd "C-c C-k") #'sl-commit-cancel)
    map)
  "Keymap for `sl-commit-mode'.")

(define-derived-mode sl-commit-mode text-mode "Sapling-Commit"
  "Major mode for editing a Sapling commit message.

\\{sl-commit-mode-map}"
  :group 'sl
  (setq-local header-line-format
              (substitute-command-keys
               "Sapling commit: \\[sl-commit-finish] finish, \
\\[sl-commit-cancel] cancel")))

;;;###autoload
(defun sl-absorb (&optional dry-run)
  "Absorb working copy changes into the current stack.
With a prefix argument, perform a dry run instead of applying."
  (interactive "P")
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show
     (if dry-run '("absorb" "-n") '("absorb" "-a"))
     (if dry-run "Sapling Absorb (dry run)" "Sapling Absorb"))))

;;;###autoload
(defun sl-rebase (destination)
  "Rebase the current stack onto DESTINATION."
  (interactive "sRebase onto revision: ")
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show (list "rebase" "-d" destination) "Sapling Rebase")))

;;;###autoload
(defun sl-rebase-continue ()
  "Continue an interrupted Sapling rebase."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show '("rebase" "--continue") "Sapling Rebase Continue")))

;;;###autoload
(defun sl-rebase-abort ()
  "Abort an interrupted Sapling rebase."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show '("rebase" "--abort") "Sapling Rebase Abort")))

;;;###autoload
(defun sl-goto (revision)
  "Go to REVISION in the current Sapling repository."
  (interactive "sGoto revision: ")
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show (list "goto" revision) "Sapling Goto")))

;;;###autoload
(defun sl-bookmark-create (name)
  "Create a Sapling bookmark named NAME at the current commit."
  (interactive "sBookmark name: ")
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show (list "bookmark" name) "Sapling Bookmark")))

;;;###autoload
(defun sl-shelve ()
  "Shelve working copy changes."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show '("shelve") "Sapling Shelve")))

;;;###autoload
(defun sl-unshelve ()
  "Unshelve the most recent shelved change."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show '("unshelve") "Sapling Unshelve")))

;;;###autoload
(defun sl-pull ()
  "Pull changes into the current Sapling repository."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show '("pull") "Sapling Pull")))

;;;###autoload
(defun sl-push ()
  "Push changes from the current Sapling repository."
  (interactive)
  (let ((root (sl--find-root default-directory)))
    (unless root
      (user-error "Not inside a Sapling repository"))
    (sl--run-and-show '("push") "Sapling Push")))

;;;###autoload
(defun sl-add (&optional files)
  "Add FILES to Sapling tracking.
With no files, add all untracked files.  In the status buffer,
marked files or the file at point are used when available."
  (interactive (list (sl--marked-or-point-files)))
  (sl--run-and-show (append '("add") files) "Sl Add"))

;;;###autoload
(defun sl-remove (&optional files)
  "Remove FILES from Sapling and delete them from disk."
  (interactive
   (list (or (sl--marked-or-point-files)
             (sl--read-files "Remove file"))))
  (sl--run-and-show (append '("remove") files) "Sl Remove"))

;;;###autoload
(defun sl-forget (&optional files)
  "Stop tracking FILES without deleting them from disk."
  (interactive
   (list (or (sl--marked-or-point-files)
             (sl--read-files "Forget file"))))
  (sl--run-and-show (append '("forget") files) "Sl Forget"))

;;;###autoload
(defun sl-revert (&optional files)
  "Revert FILES to their state in the current commit.
With no FILES, revert all pending changes.  A prefix argument skips
the confirmation prompt."
  (interactive (list (sl--marked-or-point-files)))
  (when (and (null files)
             (not current-prefix-arg)
             (not (y-or-n-p "Revert all pending changes? ")))
    (user-error "Aborted"))
  (sl--run-and-show (append '("revert") files) "Sl Revert"))

;;;###autoload
(defun sl-clean (&optional dry-run)
  "Delete untracked files from the working copy.
With a prefix argument, print what would be deleted instead."
  (interactive "P")
  (unless dry-run
    (unless (y-or-n-p "Delete all untracked files? ")
      (user-error "Aborted")))
  (sl--run-and-show
   (if dry-run '("clean" "--print") '("clean"))
   (if dry-run "Sl Clean (dry run)" "Sl Clean")))

;;;###autoload
(defun sl-uncommit (&optional files)
  "Uncommit the current commit, optionally only FILES."
  (interactive (list (sl--marked-or-point-files)))
  (sl--run-and-show (append '("uncommit") files) "Sl Uncommit"))

;;;###autoload
(defun sl-fold (revision)
  "Fold commits linearly from REVISION into the current commit."
  (interactive "sFold from revision: ")
  (sl--run-and-show (list "fold" "--from" revision) "Sl Fold"))

;;;###autoload
(defun sl-hide (revision)
  "Hide REVISION and its descendants."
  (interactive "sHide revision: ")
  (sl--run-and-show (list "hide" revision) "Sl Hide"))

;;;###autoload
(defun sl-unhide (revision)
  "Unhide REVISION and its ancestors."
  (interactive "sUnhide revision: ")
  (sl--run-and-show (list "unhide" revision) "Sl Unhide"))

;;;###autoload
(defun sl-graft (revision)
  "Graft REVISION onto the current commit."
  (interactive "sGraft revision: ")
  (sl--run-and-show (list "graft" revision) "Sl Graft"))

;;;###autoload
(defun sl-previous ()
  "Check out the previous commit in the current stack."
  (interactive)
  (sl--run-and-show '("previous") "Sl Previous"))

;;;###autoload
(defun sl-next ()
  "Check out the next commit in the current stack."
  (interactive)
  (sl--run-and-show '("next") "Sl Next"))

;;;###autoload
(defun sl-unamend ()
  "Undo the last amend operation on the current commit."
  (interactive)
  (sl--run-and-show '("unamend") "Sl Unamend"))

;;;###autoload
(defun sl-undo (&optional arg)
  "Undo the last local Sapling command.
With a prefix argument, undo that many local commands."
  (interactive "P")
  (let ((args (if arg
                  (list "undo" (number-to-string (prefix-numeric-value arg)))
                '("undo"))))
    (sl--run-and-show args "Sl Undo")))

;;;###autoload
(defun sl-redo (&optional arg)
  "Redo the last undone Sapling command.
With a prefix argument, redo that many local commands."
  (interactive "P")
  (let ((args (if arg
                  (list "redo" (number-to-string (prefix-numeric-value arg)))
                '("redo"))))
    (sl--run-and-show args "Sl Redo")))

;;;###autoload
(defun sl-metaedit (message)
  "Edit the current commit message using `sl metaedit -m MESSAGE'."
  (interactive "sCommit message: ")
  (when (string-empty-p message)
    (user-error "Empty commit message"))
  (sl--run-and-show (list "metaedit" "-m" message) "Sl Metaedit"))

;;;###autoload
(defun sl-grep (pattern)
  "Search tracked files for PATTERN using `sl grep -n'."
  (interactive "sSl grep pattern: ")
  (sl--run-and-show (list "grep" "-n" pattern) "Sl Grep"))

;;;###autoload
(defun sl-journal (&optional bookmark)
  "Show the Sapling journal, optionally for BOOKMARK."
  (interactive "sBookmark (empty for current): ")
  (sl--run-and-show
   (if (or (null bookmark) (string-empty-p bookmark))
       '("journal")
     (list "journal" bookmark))
   "Sl Journal"))

;;;###autoload
(defun sl-web ()
  "Launch the Sapling Web GUI."
  (interactive)
  (sl--run-and-show '("web") "Sl Web"))

;;;###autoload
(defun sl-annotate (revision file)
  "Annotate FILE at REVISION.
REVISION may be empty for the current commit.  In the status buffer,
the file at point is used as the initial FILE."
  (interactive
   (let ((file (or (and (derived-mode-p 'sl-mode) (sl-file-at-point))
                   (read-file-name "Annotate file: "))))
     (list (read-string "Revision (empty for current): ") file)))
  (let* ((root (or sl--repo-root (sl--find-root default-directory)))
         (args (append '("annotate")
                       (unless (string-empty-p revision)
                         (list "-r" revision))
                       (list file))))
    (unless root
      (user-error "Not inside a Sl repository"))
    (sl--show-output sl-output-buffer-name args "Sl Annotate" root)))

;;;###autoload
(defun sl-backout (revision)
  "Back out REVISION in the current repository."
  (interactive "sBackout revision: ")
  (sl--run-and-show (list "backout" revision) "Sl Backout"))

;;;###autoload
(defun sl-bookmark-list ()
  "List Sapling bookmarks."
  (interactive)
  (sl--run-and-show '("bookmark") "Sl Bookmarks"))

;;;###autoload
(defun sl-bookmark-delete (name)
  "Delete Sapling bookmark NAME."
  (interactive "sDelete bookmark: ")
  (sl--run-and-show (list "bookmark" "--delete" name) "Sl Bookmark Delete"))

;;;###autoload
(defun sl-bookmark-rename (old-name new-name)
  "Rename Sapling bookmark OLD-NAME to NEW-NAME."
  (interactive "sRename bookmark: \nsNew name: ")
  (sl--run-and-show
   (list "bookmark" "--rename" old-name new-name)
   "Sl Bookmark Rename"))

;;;###autoload
(defun sl-pr-list ()
  "List GitHub pull requests associated with this repository."
  (interactive)
  (sl--run-and-show '("pr" "list") "Sl PR List"))

;;;###autoload
(defun sl-pr-submit ()
  "Submit GitHub pull requests for the current stack."
  (interactive)
  (sl--run-and-show '("pr" "submit") "Sl PR Submit"))

;;;###autoload
(defun sl-pr-pull ()
  "Pull GitHub pull request data into the current repository."
  (interactive)
  (sl--run-and-show '("pr" "pull") "Sl PR Pull"))

;;;###autoload
(defun sl-config ()
  "Show Sapling configuration."
  (interactive)
  (sl--run-and-show '("config") "Sl Config"))

;;;###autoload
(defun sl-doctor ()
  "Run Sapling's doctor diagnostics."
  (interactive)
  (sl--run-and-show '("doctor") "Sl Doctor"))

;;;###autoload
(defun sl-clone (source destination)
  "Clone SOURCE into DESTINATION using Sapling."
  (interactive "sClone source: \nsDestination: ")
  (sl--show-output
   sl-output-buffer-name
   (if (string-empty-p destination)
       (list "clone" source)
     (list "clone" source destination))
   "Sl Clone"
   default-directory))

;;;###autoload
(defun sl-init (directory)
  "Initialize a new Sapling repository in DIRECTORY.
An empty DIRECTORY initializes the current directory."
  (interactive "GInitialize repository in directory: ")
  (sl--show-output
   sl-output-buffer-name
   (if (string-empty-p directory)
       '("init")
     (list "init" directory))
   "Sl Init"
   default-directory))

;;;###autoload
(defun sl-menu ()
  "Display a Magit-style dispatch menu for Sapling commands."
  (interactive)
  (let ((choice (read-multiple-choice
                 "Sl"
                 '((?s "status" "Show working copy status")
                   (?l "smartlog" "Show smartlog")
                   (?L "log" "Show graph log")
                   (?d "diff" "Show diff")
                   (?c "commit" "Commit changes")
                   (?a "amend" "Amend current commit")
                   (?x "absorb" "Absorb changes into stack")
                   (?r "rebase" "Rebase onto revision")
                   (?f "fold" "Fold commits")
                   (?g "graft" "Graft a commit")
                   (?h "hide" "Hide a commit")
                   (?H "unhide" "Unhide a commit")
                   (?z "shelve" "Shelve changes")
                   (?Z "unshelve" "Unshelve changes")
                   (?n "next" "Check out next commit")
                   (?p "previous" "Check out previous commit")
                   (?u "undo" "Undo local command")
                   (?R "redo" "Redo local command")
                   (?A "add" "Add files")
                   (?D "remove" "Remove files")
                   (?K "forget" "Forget files")
                   (?V "revert" "Revert files")
                   (?e "metaedit" "Edit commit message")
                   (?o "show" "Show current commit")
                   (?J "journal" "Show journal")
                   (?B "bookmark" "Create bookmark")
                   (?F "pull" "Pull changes")
                   (?P "push" "Push changes")
                   (?q "quit" "Quit")))))
    (cl-case choice
      (?s (sl-status))
      (?l (sl-smartlog))
      (?L (sl-log))
      (?d (sl-diff))
      (?c (sl-commit))
      (?a (sl-amend))
      (?x (sl-absorb))
      (?r (call-interactively #'sl-rebase))
      (?f (call-interactively #'sl-fold))
      (?g (call-interactively #'sl-graft))
      (?h (call-interactively #'sl-hide))
      (?H (call-interactively #'sl-unhide))
      (?z (sl-shelve))
      (?Z (sl-unshelve))
      (?n (sl-next))
      (?p (sl-previous))
      (?u (sl-undo))
      (?R (sl-redo))
      (?A (sl-add))
      (?D (sl-remove))
      (?K (sl-forget))
      (?V (sl-revert))
      (?e (call-interactively #'sl-metaedit))
      (?o (sl-show))
      (?J (call-interactively #'sl-journal))
      (?B (call-interactively #'sl-bookmark-create))
      (?F (sl-pull))
      (?P (sl-push))
      (?q nil))))

(provide 'sl)

;;; sl.el ends here
