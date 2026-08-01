;;; org-history.el --- Show Dates for Org headers from vcs + auto-commit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, outline, vc
;; URL: https://codeberg.org/Anoncheg/emacs-org-history
;; Version: 0.5
;; Created: 30 may 2026
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:

;; Track and display modification dates automatically in `org-mode`
;; buffers with this minor mode.

;; It uses Git version control to make automatic commits whenever you
;; save a buffer (per-day with --amend).

;; A special feature allows auto-enabling the mode for current opened
;; file in current directory by using `.dir-locals.el`, removing the
;; need to manually list tracked files.

;;;; Features:

;; - Automatically commits buffer changes to a per-file Git repository
;; in the background (using `--amend` to group daily changes)
;; - Prompts for confirmation only once per file
;; - Efficient performance even with large files, thanks to caching
;; and asynchronous Git operations

;;;; Configuration:

;; (add-to-list 'load-path "/path-to/emacs-org-history")
;; (require 'org-history)

;; If you dont like using .dir-locals.el, you may disable this feature
;;  in ~/.emacs:
;; (setopt org-history-dir-locals-flag nil)

;;;; Activation: M-x org-history

;;;; Customization: M-x customize-group RET org-history

;; Hint: You may use "C-h ." at the end of header to get hint without
;;  using “mouse over” to see it.

;; Built-in Emacs alternative: M-x vc-annotate

;;;; How this works:

;; For every visible header, we get a range of line numbers like
;; 21-34; from .git/, we get the last modification in this range.
;; We put a read-only overlay on the last character of the Org header
;; with the date.
;; We accurately do "git commit --amend" if the current day is the day
;; of the last commit with the "org-history" message, or just add a new
;; commit.

;; When saving, we check .dir-locals.el to see if there is a record
;; for the current file and if .git exists.  If not, we ask the user and
;; add the line:
;; ("subfolder-maybe/current-file" (org-mode (mode . org-history)))
;; Which checks 1) the path of the file relative to the Git directory
;; 2) Org mode for the buffer.

;;; TODO:
;; - 1) Support TRUMP.
;; - make it work with outline mode.
;; - command to add current folder to list, remake org-history-directories
;; - create alternative to "git blame" for first (not last)
;;  modification: git log -S "your line of text" --format="%cd"
;;  --date=short --reverse
;; - New headers use date from next line, think how to move first part
;;  from `org-history-outline--process-tasks' to
;;  `org-history-outline--process-git-blame-output'. And process all
;;  headers maybe.
;; - When org-history-add-dates called at opening document with some
;;  headers opened, it add dates to top level headers only, maybe it
;;  trigger to early idk

;; Require 29.1 for `org-fold-folded-p'

;;;; Other packages:

;; - Modern navigation in major modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff no 3-th window	https://github.com/Anoncheg1/ediffnw
;; - Dired history		https://github.com/Anoncheg1/dired-hist
;; - Selected window highlighting	https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state	https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
;; - Dates for Org-mode headers	https://github.com/Anoncheg1/emacs-org-history
;; - Call LLMs and agents from Org-mode cui blocks https://github.com/Anoncheg1/emacs-cui

;; Donate, sponsor the author
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether TRX-TRON) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;; Code:

;; Touch: Blessed is he who will take his place in the beginning. Gos. Thom. 18

;;;; Useful code:

;; (vc-backend buffer-file-name) or (vc-root-dir)
;; - Check if file is tracked, .git should exist

;; (vc-git-root buffer-file-name)
;; - Check if there is .git, works for directory, file or not existing file

(require 'vc)
(require 'vc-git)
(require 'org)
(require 'org-history-outline)
(require 'org-history-dirl)
(require 'org-history-debug)


;; -=-= Variables

(defgroup org-history nil
  "Faces for OAI blocks."
  :tag "Org dates for outlines"
  :group 'org)

(defcustom org-history-git-init-commands
  '(("init") ; git init
    ("config" "user.name" "'(none)'") ; git config user.name '(none)'
    ("config" "user.email" "''")) ; git config user.email ''
  "List of argument lists passed to `vc-git-command` during initialization.
Also used to reinitialize if no commits was made."
  :type '(repeat (repeat string))
  :group 'org-history)

(defcustom org-history-gitignore-content '("*.elc"
                               "*~"
                               "#*#"
                               "/.emacs.desktop"
                               "/.emacs.desktop.lock"
                               "elpa/")
   "Default patterns written into a newly created .gitignore file.
These patterns ensure compiled Lisp files, autosaves, backups, and
lock files are not tracked by Git."
  :type '(repeat string)
  :group 'org-history)

(defcustom org-history-hide-dates-flag nil
  "Non-nil means hide dates two seconds after activating the mode.
A timer is used to allow file-local and directory-local variables to
 load completely before calculating the final state."
  :type 'boolean
  :group 'org-history)

(defcustom org-history-dir-locals-flag t
  "Non-nil means save command to autostart `org-history' in dir-locals.el.
dir-locals.el located in  the `.git' root folder of the current file."
  :type 'boolean
  :group 'org-history)

;; ;; NOT USED
;; (defcustom org-history-directories nil
;;   "List of directories that processed without questions.
;; If `org-history-hook-for-after-save' set as global hook.
;; TODO: testing and refining required."
;;   :type 'string
;;   :group 'org-history)

(defvar-local org-history-answer-was-given nil
  "Status of the user choice regarding tracking for the current file.
When non-nil, the asnwer was given and we wouldnt ask user again.

Possible symbol values are:
  nil              -- User was not asked yet.
  `dont-track-file' -- Do not track this file.
  `track-file'     -- Track this file.")

;; -=-= functions: VC-git

(defun org-history--vc-reset-cache (&optional file)
  "Flush all common VC cache properties for `default-directory' or FILE."
  (if file
      (vc-file-clearprops file)
    ;; else
    (when-let ((root (vc-root-dir))) ; nil is handled properly here
      (dolist (prop '(vc-backend vc-state vc-working-revision vc-name))
        (vc-file-setprop root prop nil)))))

(defun org-history--vc-git-get-last-commit-hash (&optional file)
  "Return string with the last commit or nil.
If FILE provided check commits only for file, otherwise any commit.
Uses variable `buffer-file-name'.
Note: `vc-git-working-revision' - accept directory as argument,
 `vc-working-revision' - only with file."
  (org-history-debug-print "org-history--vc-git-get-last-commit-hash N1 %s" file)
  (if file
      (when (vc-backend file)
        (vc-working-revision file)) ; require tracked file with commit
    ;; else
    (when (vc-git-root buffer-file-name) ; check that .git exist
      (when-let ((path (vc-git-root buffer-file-name)))
        (vc-git-working-revision path)))))

(defun org-history--vc-add-file (file &optional backend)
  "Track FILE.
Uses `default-derectory'.
Optional argument BACKEND is Git or may be other."
  ;; Clear Emacs VC cache so it realizes Git now exists
  (vc-file-clearprops file)

  (unless (vc-backend file) ; never added
    (vc-register (list (or backend 'Git) (list file))))

  ;; `vc-register' have a bug, rise error if file 'edited and was added before, but not now
  ;; 'register can cause duplicate registrations or errors depending
  ;;   on the Emacs version because vc-register already invokes the
  ;;   backend's register function.
  (unless (vc-backend file)
    (vc-call-backend (or backend 'Git) 'register (list file)))

  ;; Refresh the state to update vc-state immediately
  (vc-refresh-state))

;; NOT USED
(defun org-history--vc-git-unstage-file (file)
  "Unstage FILE using git restore --staged via vc-git primitives."
  (interactive "fFile to unstage: ")
  ;; Ensure the file actually belongs to a Git repository
  (if (eq (vc-backend file) 'Git)
      (progn
        (with-temp-buffer
          ;; vc-git--call takes: (buffer command &rest args)
          ;; file-local-name strips remote file prefixes (like TRAMP) if present
          (vc-git--call t "restore" "--staged" (file-local-name file)))

        ;; CRITICAL: Reset Emacs's internal VC cache so it notices the file is unstaged
        (vc-file-setprop file 'vc-state nil)
        (vc-file-setprop file 'vc-working-revision nil)

        (message "Successfully unstaged %s" (file-name-nondirectory file)))
    (error "This file is not in a Git repository")))

;; NOT USED
(defun org-history--vc-git-get-last-commit-date (&optional file)
  "Get commit date for FILE or for last commit.
Return string date YYYY-MM-DD or nil.
FILE should be tracked by git."
  (if file
      (when (vc-backend file)
        (when-let ((d (vc-git--run-command-string buffer-file-name "log" "-1" "--format=%as" file)))
          (string-trim d)))
    ;; else
    (when-let ((d (vc-git--run-command-string nil "show" "-s" "--format=%as")))
    (string-trim d))))

;; NOT USED
(defun org-history--vc-git-get-range-last-mod-date (file start end)
  "Return the last modification date (YYYY-MM-DD) for lines START to END in FILE.
Returns nil if FILE is not in Git, range is invalid, or no commits exist.
Uses `default-directory'."
  (when (and (file-exists-p file)
             (> start 0)
             (>= end start)
             default-directory
             (vc-git-responsible-p default-directory))
    (with-temp-buffer
      (condition-case nil
          (when (zerop (vc-git-command
                        t 0 nil
                        "log" "-1" "--pretty=format:%as" "-s"  ; Added "-s" to suppress the diff patch
                        (format "-L%d,%d:%s" start end file)))
            (let ((output (buffer-string)))
              (unless (string-empty-p output)
                (string-trim output))))
        (error nil)))))

;; -=-= function: add record to .dir-locals.el

(defun org-history--append-to-dirl ()
  "Wrap `org-history-dirl-append' function.
Check `org-history-dir-locals-flag' varianble and add .dir-locals.el to
 git.
Use `default-directory'."
  (when org-history-dir-locals-flag
    (when-let* ((dirl-path (org-history-dirl-append))
                (default-directory (vc-root-dir)))
      (org-history--vc-add-file dirl-path 'Git))))

;; -=-= functions: init .git

(defun org-history-git-init (&optional first-file)
  "Execute the custom Git initialization commands sequentially.
Optional argument FIRST-FILE is used for the first file to init git
 repostiry or .gitignore if not exist or .dir-locals.el
FIRST-FILE should be a name of file in `default-directory'.
Create .gitignore and .dir-locals.el.
Warning, `vc-root-dir' will return nil until first commit will be made.
Safe to call with existing .git.
Use `default-directory'."
  (interactive)
  (org-history-debug-print "org-history-git-init %s" first-file)
  (let ((vc-handled-backends '(Git)))
    ;;  Step 0 : clear vc catch
    (org-history--vc-reset-cache)
    (when first-file
      (org-history--vc-reset-cache first-file))
    ;; Step 1: Create the .gitignore file safely
    ;; (we need at leas one file to init initialize)
    (let ((first-f (expand-file-name
                    (if (and first-file (file-exists-p first-file))
                        first-file
                      ;; else
                      (if (not (file-exists-p ".gitignore"))
                          ".gitignore"
                        ;; else
                        ".dir-locals.el"
                        ;; else
                        ;; (user-error "Cant init git in for org-history: .gitignore and .elpaignore is exist")
                        ))
                    default-directory)))
      ;; Create  .dir-locals.el
      (org-history--append-to-dirl)

      ;; Create  .gitignore
      (when (not (file-exists-p ".gitignore"))
        (with-temp-file  ".gitignore"
          (insert (mapconcat #'identity org-history-gitignore-content "\n") "\n")
          ;; (write-file ".gitignore")
          ;; (basic-save-buffer)
          ))

      ;; Step 1: Initialize the Git repository
      (dolist (args org-history-git-init-commands)
        ;; 'apply' lets us unpack the 'args' list directly into the function call
        (apply #'vc-git-command nil 0 nil args))

      (message "Initialized Git repository...")

      ;; Step 3: add first file to finish initialization
      (org-history--vc-add-file first-f 'Git)

      ;; ;; Refresh the state to update vc-state immediately
      ;; (vc-refresh-state)
      ;; (org-history-debug-print "org-history-git-init" (org-history--vc-git-status))
      ;; (vc-responsible-backend default-directory) ; fix vc-root-dir to return without first commit
      (setq org-history-answer-was-given 'track-file)

      ;; Commit is not required, add is enough
      (message "Git initialized %s with file %s!" default-directory (file-relative-name first-f)))))


;; -=-= functions: check-hook-scope (OLD)

(defun org-history--check-hook-scope (hook func)
  "Check if FUNC is bound to HOOK as :global, :local, :both, or nil."
  (cond ((and (memq func (default-value hook))
              (local-variable-p hook)
              (memq t (memq func (and (boundp hook) (symbol-value hook)))))
         :both)
        ((and (local-variable-p hook)
              (memq t (memq func (and (boundp hook) (symbol-value hook)))))
         :local)
        ((memq func (default-value hook))
         :global)))

;; -=-= git: commit and get last commit message
(defun org-history--git-get-last-commit-message (&optional file)
  "Get any last Git commit message, or for FILE if provided.
Returns the trimmed message string, or nil if an error occurs (e.g., no
 commits exist)."
  (when (or (not file) (and file (file-exists-p file)))
    ;; The first element here must be the command string "log", NOT nil.
    (let ((args (if file
                    (list "log" "-1" "--pretty=%B" "--" (file-relative-name file))
                  (list "log" "-1" "--pretty=%B"))))
      (with-demoted-errors "org-history (git log error): %S"
        (when-let ((output (apply #'vc-git--run-command-string nil args)))
          (string-trim output))))))


(defun org-history--commit (&optional last-commit-message)
  "Execute the commit or amend routines based on LAST-COMMIT-MESSAGE.
If last-commit-message is nil we try to get it from git, otherwise we
 check it if it is `org-history' made and have current date.
Uses variable `buffer-file-name'.
Assumes tracking confirmation has already been validated and set."
  (org-history-debug-print "org-history--commit %s" last-commit-message)
  (let* ((current-date (format-time-string "%F")) ; Y-%m-%d
         ;; If last-commit-message wasn't provided, fetch it
         (msg (or last-commit-message
                  (org-history--git-get-last-commit-message))))

    ;; Check if the commit message starts with "org-history [current-date]"
    (if (and msg (string-prefix-p (concat "org-history " current-date) msg))
        ;; Sub-Case A: Amend day's existing transaction
        (progn
          (org-history-debug-print "org-history--commit N1")
          (org-history--vc-add-file buffer-file-name 'Git)
          (vc-git-command nil 0 nil "commit" "--amend" "--allow-empty" "--no-edit" "--date=now")
          (message "VC-Git: Amended existing commit for today."))

      ;; Sub-Case B: Stage file changes and commit with date-stamped message
      (progn
        (org-history-debug-print "org-history--commit N2")
        (org-history--vc-add-file buffer-file-name 'Git)
        (vc-git-command nil 0 nil "commit" "-m" (format "org-history %s" current-date))
        (message "VC-Git: Created new date-stamped commit.")))))

;; -=-= Attach-date

(defun org-history-add-dates (&optional page-beg page-end)
  "Collect line ranges for visible Org headings and apply dates separately.
Optional arguments PAGE-BEG PAGE-END are position in current buffer."
  (interactive)
  (org-history-debug-print "org-history-add-dates %s %s" page-beg page-end)
  ;; if not checked that commit exist error: gethash(4 nil) error in `org-history-outline--process-tasks'
    (let (head-markers)
      (save-excursion
        ;; go to the top of page or range
        (goto-char (or page-beg (point-min)))
        ;; go to the first heading
        (unless (org-at-heading-p)
          (outline-next-heading))

        ;; Loop over visible headers and store its begining positions
        (while (and (not (eobp))
                    (if page-end (< (point) page-end) t))
          (unless (org-fold-core-get-folding-spec 'headline (point))
            ;; (let* ((heading-pos (1+ (point)))
            ;;        ;; (start (save-excursion (forward-line 1) (line-number-at-pos)))
            ;;        ;; (end (save-excursion (org-end-of-subtree t t) (line-number-at-pos)))
            ;;        ;; (real-start (min start end))
            ;;        ;; (real-end (max start end))
            ;;        )
              ;; Push a task tuple: (heading-marker start-line end-line)
              ;; Using a marker ensures the position stays accurate even if the buffer shifts
            (push (copy-marker (1+ (point))) head-markers)) ; position of begining of heading +1
          (outline-next-heading)))

      ;; apply using git blame hash.
      (when head-markers
        (when-let ((commit-hash (org-history--vc-git-get-last-commit-hash buffer-file-name)))
          (org-history-outline--add-dates (nreverse head-markers) commit-hash (unless (or page-beg page-end) t))))))

;; -=-= hook: after-save

(defun org-history-hook-for-after-save ()
  "Hook for `org-mode' buffers run after saving a file."
  (org-history-debug-print "org-history-hook-for-after-save N1")
  (when (and (not (eq org-history-answer-was-given 'dont-track-file))
             buffer-file-name
             default-directory) ; this just in case. in case of nothing to save raise error at attempt to commit if no diff.
    (org-history-debug-print "org-history-hook-for-after-save N2")
    (let ((before-answer org-history-answer-was-given)
          (git-root (vc-git-root buffer-file-name)) ; cause problem with TRUMP, we check if TRUMP is active at enabling org-history minor mode
          (is-file-tracked (eq 'Git (vc-backend buffer-file-name)))
          (current-date (format-time-string "%F"))) ; Y-%m-%d
      (let ((default-directory (or git-root default-directory))) ; use git-root
        (vc-file-clearprops buffer-file-name)
        (let* ((file-last-commit-message
                (when is-file-tracked (org-history--git-get-last-commit-message buffer-file-name)))
               (rel-file-name (file-relative-name buffer-file-name default-directory))
               ;; Construct the matching target string for Case 2
               (expected-prefix (concat "org-history " current-date)))

          ;; --- CASES ---
          (cond
           ;; Case 1: No Git repository exists at all
           ((not git-root)
            (org-history-debug-print "org-history-hook-for-after-save Case 1")
            (if (y-or-n-p (format "org-history: Do git init and activate auto-commit for this file in\n%s? " default-directory))
                (progn
                  (org-history-git-init rel-file-name)
                  (if org-history-hide-dates-flag
                      (message "org-history: dates was not shown because of org-history-hide-dates-flag variable.")
                    ;; else
                    (org-history-add-dates))
                  (setq org-history-answer-was-given 'track-file))
              (setq org-history-answer-was-given 'dont-track-file)))

           ;; Case 2: Git repo exists + starts with "org-history [today's date]" -> Transparently Amend
           ((and file-last-commit-message
                 (string-prefix-p expected-prefix file-last-commit-message))
            (org-history-debug-print "org-history-hook-for-after-save Case 2")
            ;; 1. Ensure we have a tracking decision if we don't already
            (if (not (or org-history-answer-was-given ; 1
                         (org-history-dirl--dir-locals-p rel-file-name))) ; 2
              (let ((prompt (format "org-history: track this file and add record for this file in\n%s? "
                                    (expand-file-name ".dir-locals.el" default-directory))))
                (setq org-history-answer-was-given (if (y-or-n-p prompt) 'track-file 'dont-track-file)))
              ;; else - no need to ask
              (setq org-history-answer-was-given 'track-file))
            ;; Create  .dir-locals.el
            (when (and org-history-dir-locals-flag
                       (not before-answer)
                       (eq org-history-answer-was-given 'track-file))
              (org-history--append-to-dirl))
            ;; 2. Execute updated commit routine (passes the message forward) - always
            (org-history--commit file-last-commit-message)
            ;; (unless org-history-hide-dates-flag
            ;;       (org-history-add-dates)) ; too aggressive?
            )

           ;; Case 3: Git repo exists, but requires a new commit or initial tracking approval
           ;; if was not asked and dir-locals not exist do inits
           (t
            (org-history-debug-print "org-history-hook-for-after-save Case 3")
            ;; 1. Ensure we have a clear tracking decision
            (if (not (or org-history-answer-was-given ; 1
                         (org-history-dirl--dir-locals-p rel-file-name))) ; 2 - ask only if
                (let ((prompt-msg (format "org-history: enable auto-commit on save this file and in\n%s? "
                                          (expand-file-name ".dir-locals.el" default-directory))))
                  (setq org-history-answer-was-given (if (y-or-n-p prompt-msg) 'track-file 'dont-track-file)))
              ;; else - no need to ask
              (setq org-history-answer-was-given 'track-file))
            ;; 2. do init if was not asked before
            (when (and org-history-dir-locals-flag
                       (not before-answer)
                       (eq org-history-answer-was-given 'track-file))
              ;; Create  .dir-locals.el
              (org-history--append-to-dirl))

            ;; 3. if tracking do commit
            (when (eq org-history-answer-was-given 'track-file)
              ;; Ensure Git repo is initialized PROPERLY with ALL variables, double calling "git init" dont cause trouble
              (unless (vc-git--run-command-string nil "log" "-1") ; return nil if no commits
                (let ((inhibit-message t))
                  (dolist (args org-history-git-init-commands)
                    (apply #'vc-git-command nil 0 nil args))))
              ;; Passes the message file text (or nil) to handle new commit branches
              (org-history--commit file-last-commit-message)
              ;; Show dates
              (unless org-history-hide-dates-flag
                (org-history-add-dates))))) ; too agressive?
          ;; Synchronize cache once more post-execution for UI updates (e.g., modeline)
          (vc-file-clearprops buffer-file-name))))))

;; -=-= function: headers folding
(defun org-history--show-dates-at-unfold (orig-fun &rest args)
  "Add dates for subheaders at unfolding.
Checks if user interactively unfolded a heading.
Triggered after `org-cycle'.
Reliably check for interactive execution using :around advice.
Argument ORIG-FUN is `org-cycle' and its ARGS."
  ;; 1. Check interactivity FIRST while org-cycle is at the top of the stack
  (let ((interactive-call (called-interactively-p 'any)))

    ;; 2. Run the original org-cycle command so the heading actually changes state
    (apply orig-fun args)

    (when (bound-and-true-p org-history-mode)
      ;; 3. Now perform your post-execution visibility checks safely
      (org-history-debug-print "org-history--show-dates-at-unfold N1 %s %s"
                               (org-at-heading-p)
                               interactive-call)
      (when (and
             interactive-call			; 1. Only run if called interactively
             (org-at-heading-p)			; 2. Only run if cursor is on a heading
             (not (save-excursion			; 3. Ensure heading is currently open
                    (end-of-line)
                    (org-fold-folded-p (point) 'outline))) ; 4. ; Unfolding? 'headline

             org-history-outline--git-blame-cache)	; 5. Only run if git blame was retrieved (especilly with async call) or without mode active

        (org-history-debug-print "org-history--show-dates-at-unfold N2")
        (let ((vc-handled-backends '(Git)))
          (let ((start (line-beginning-position)) ;; (save-excursion (forward-line 1) (point))) ; with root header too.
                (end (save-excursion (org-end-of-subtree t t) (point))))
            (org-history-add-dates start end)))))))

(defun org-history--cycle-hook (state)
  "Triggered by `org-shifttab' from `org-cycle-internal-global' after cycling.
STATE may be `overview', `contents', or `all'."
  (when (eq state 'contents)
    (let ((vc-handled-backends '(Git)))
      (org-history-add-dates (point-min) (point-max)))))

;; -=-= interactive: hide dates
(defun org-history-hide ()
  "Hide dates only."
  (interactive)
  (advice-remove 'org-cycle #'org-history--show-dates-at-unfold)
  (remove-hook 'org-cycle-hook #'org-history--cycle-hook t)
  (org-history-outline-clear-all-date))

(defun org-history-show ()
  "Show dates only."
  (interactive)
  (when (or (bound-and-true-p org-history-mode)
            (memq 'org-history--cycle-hook org-cycle-hook)
            (if (not (and (vc-git-root buffer-file-name) (org-history--vc-git-get-last-commit-hash)))
                (progn (message "There is not .git with any dates to get.") nil)
              ;; else
              (y-or-n-p (format "You want to see dates while org-history is not active?"))))
    (advice-add 'org-cycle :around #'org-history--show-dates-at-unfold '((local . t)))
    (add-hook 'org-cycle-hook #'org-history--cycle-hook nil t)
    (let ((vc-handled-backends '(Git)))
      (org-history-add-dates))))

;; -=-= minor mode
;;;###autoload
(define-minor-mode org-history-mode
  "Minor mode for `org-mode' to showing date of last modified per outline."
  :init-value nil
  ;; :keymap oai-mode-map
  :group 'org-history
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-history minor mode failed to activate in buffer %s, not Org mode" (buffer-name (current-buffer))))
  (when (file-remote-p buffer-file-name)
    (user-error "Org-history dont support TRUMP for now. File %s is remote" buffer-file-name))
  (if org-history-mode
      (progn
        (org-history-debug-print "org-history-mode")
        (let ((vc-handled-backends '(Git)))
          ;; no last commit - ask user to init .git
          (when (or (not (vc-git-root buffer-file-name)) (not (org-history--vc-git-get-last-commit-hash))) ; any commits for file?
            (if (y-or-n-p (format "org-history: Do git init in %s? " default-directory))
                (progn
                  (org-history-git-init (file-relative-name buffer-file-name default-directory)) ; (setq org-history-answer-was-given 'track-file))
                  (org-history--commit "")) ; create new commit.
              ;; else
              (setq org-history-answer-was-given 'dont-track-file))))
        (add-hook 'after-save-hook #'org-history-hook-for-after-save nil t)
        (advice-add 'org-cycle :around #'org-history--show-dates-at-unfold) ; cycle one header, set global,  we check org-history mode inside advice.
        (add-hook 'org-cycle-hook #'org-history--cycle-hook nil t) ; global cycling whole buffer
        (let ((orig-buffer (current-buffer))) ; lexical binding
          ;; we need delay, because dir-locals and file-locals run before
          (run-with-timer
           0.5 nil
           (lambda ()
             (if (buffer-live-p orig-buffer)
                 (with-current-buffer orig-buffer
                   (org-history-add-dates)))))
          ;; we use timer to to be able to see File-Local variables
          (run-with-timer
           2.5 nil
           (lambda ()
             ;; Because of lexical binding, orig-buffer is automatically captured
             (if (buffer-live-p orig-buffer)
                 (with-current-buffer orig-buffer
                   (when org-history-hide-dates-flag
                     (org-history-hide))))))))

    ;; else - off
    (advice-remove 'org-cycle #'org-history--show-dates-at-unfold)
    (remove-hook 'after-save-hook #'org-history-hook-for-after-save t)
    (remove-hook 'org-cycle-hook #'org-history--cycle-hook t)
    (org-history-outline-clear-all-date)

    (kill-local-variable 'org-history-answer-was-given)
    (kill-local-variable 'org-history-outline--git-blame-cache)
    (kill-local-variable 'org-history-outline--git-last-commit)
    (message "org-history is disabled.")))

(defalias 'org-history #'org-history-mode)


;; ;; NOT USED
;; (defun org-history--check-hook-directories () ; OLD TODO: replace with minor global mode
;;   (or (eq hook-placed :local)
;;       (eq hook-placed :both)
;;       (and (eq hook-placed :global)
;;            (catch 'found
;;              (dolist (path org-history-directories)
;;                (when (file-equal-p dir path)
;;                  (throw 'found t)))
;;              nil))))

;;; provide

(provide 'org-history)

;;; org-history.el ends here
