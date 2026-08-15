;;; vc-sl.el --- VC backend for Sapling  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Swithin Chan <swithinchan@yahoo.com.hk>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (sl "0.2.0"))
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

;; This library adds Sapling (`sl') as a VC backend.  It intentionally
;; complements `sl.el' rather than replacing it: the Sapling-specific
;; stack/smartlog interface lives in `sl.el', while the generic VC
;; commands (`C-x v =', `C-x v d', `C-x v l', ...) are served by this
;; backend.
;;
;; To enable it, add Sapling to `vc-handled-backends':
;;
;;   (add-to-list 'vc-handled-backends 'Sl)
;;
;; This backend recognizes native Sapling repositories (identified by
;; their `.sl' directory).  Git-backed Sapling working copies are
;; intentionally left to `vc-git', which is already a good fit for them.
;; Registration is deliberately conservative: if `sl' cannot be found,
;; or a file is not managed by Sapling, VC falls through to the next
;; backend.

;;; Code:

(require 'cl-lib)
(require 'sl)

(eval-when-compile
  (require 'vc)
  (require 'vc-dir)
  (require 'log-view)
  (require 'log-edit))

(declare-function vc-find-root "vc-hooks" (file witness))
(declare-function vc-file-setprop "vc-hooks" (file property value))
(declare-function vc-state "vc-hooks" (file &optional backend))
(declare-function vc-working-revision "vc-hooks" (file &optional backend))
(declare-function vc-switches "vc" (backend op))
(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))
(declare-function log-edit-extract-headers "log-edit" (headers string))
(declare-function completion-table-dynamic "minibuffer" (fun))

(defvar vc-log-view-type nil)
(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(autoload 'vc-setup-buffer "vc-dispatcher")
(autoload 'vc-switches "vc")
(autoload 'vc-annotate-convert-time "vc-annotate")
(autoload 'log-edit-extract-headers "log-edit")

(defgroup vc-sl nil
  "VC Sapling (`sl') backend."
  :group 'vc
  :group 'tools)

(defcustom vc-sl-global-switches nil
  "Global switches to pass to any Sapling command run by VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-sl-diff-switches t
  "String or list of strings specifying switches for Sapling diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-sl-log-short-format "{node|short}\t{date|shortdate}\t{desc|firstline}\n"
  "Sapling log template for the short VC log format."
  :type 'string)

(defcustom vc-sl-log-format
  (concat "changeset:   {node|short}\n"
          "{bookmarks % 'bookmark:    {bookmark}\n'}"
          "user:        {author}\n"
          "date:        {date|rfc3339date}\n"
          "summary:     {desc|firstline}\n\n")
  "Sapling log template for the long VC log format."
  :type 'string)

(defconst vc-sl-log-short-regexp
  "^\\(?1:[0-9a-f]+\\)\t\\(?2:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\t\\(?3:.*\\)"
  "Regexp matching one line produced by `vc-sl-log-short-format'.")

(defconst vc-sl-log-long-regexp
  "^changeset:[ \t]*\\(?1:[0-9a-f]+\\)"
  "Regexp matching the start of a long Sapling VC log entry.")

(defconst vc-sl-annotate-re
  (concat "^\\(?:[^ ]+ +\\)?\\([0-9a-f]+\\) "
          "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\):")
  "Regexp matching one line of `sl annotate' output.")

;; Clear the cached backend function table when this file is reloaded.
(put 'Sl 'vc-functions nil)

;;; Process helpers

(defun vc-sl--command-list (args)
  "Return a command list for running Sapling with ARGS.
Return nil if the Sapling executable cannot be found."
  (condition-case nil
      (sl--process-command
       (append (if (stringp vc-sl-global-switches)
                   (list vc-sl-global-switches)
                 vc-sl-global-switches)
               args))
    (error nil)))

(defun vc-sl--process-environment ()
  "Return `process-environment' suitable for non-interactive Sapling use.
VC parses Sapling output, so colors and pagers are always disabled here."
  (let ((sl-use-color nil))
    (sl--process-environment)))

(defun vc-sl--run (destination args &optional directory coding)
  "Run Sapling with ARGS, sending output to DESTINATION.
DESTINATION is passed to `process-file' and may be nil, t, a buffer,
or `(:file FILE)'.  DIRECTORY, when non-nil, is the working directory.
CODING, when non-nil, overrides the default UTF-8 process coding.
Return the exit status, or nil if Sapling could not be run."
  (let* ((default-directory (or directory default-directory))
         (process-environment (vc-sl--process-environment))
         (command (vc-sl--command-list args))
         (coding-system-for-read (or coding 'utf-8-auto))
         (coding-system-for-write (or coding 'utf-8)))
    (when command
      (apply #'process-file (car command) nil destination nil (cdr command)))))

(defun vc-sl--run-to-string (args &optional directory)
  "Run Sapling with ARGS in DIRECTORY and return (STATUS . OUTPUT).
Return nil if Sapling could not be run."
  (with-temp-buffer
    (let ((status (vc-sl--run (current-buffer) args directory)))
      (and status (cons status (buffer-string))))))

(defun vc-sl--run-async (buffer args &optional directory sentinel)
  "Run Sapling with ARGS asynchronously, sending output to BUFFER.
DIRECTORY, when non-nil, is the working directory.  SENTINEL, when
non-nil, is installed as the process sentinel.
Return the process object, or nil if Sapling could not be run."
  (let* ((default-directory (or directory default-directory))
         (process-environment (vc-sl--process-environment))
         (command (vc-sl--command-list args)))
    (when command
      ;; Match `sl.el' and keep process output responsive on Windows.
      (when (and (boundp 'w32-pipe-read-delay)
                 sl-w32-pipe-read-delay)
        (setq w32-pipe-read-delay sl-w32-pipe-read-delay))
      (let ((process-connection-type nil))
        (prog1 (apply #'start-file-process
                      "sl-vc" buffer (car command) (cdr command))
          (set-process-coding-system
           (get-buffer-process buffer) 'utf-8-auto 'utf-8)
          ;; Avoid the default sentinel message ("Process sl-vc finished")
          ;; when the caller does not need a custom sentinel.
          (set-process-sentinel (get-buffer-process buffer)
                                (or sentinel #'ignore)))))))

;;; Backend properties

(defun vc-sl-revision-granularity ()
  "Return the revision granularity of the Sapling backend."
  'repository)

(defun vc-sl-checkout-model (_files)
  "Return the checkout model used by Sapling."
  'implicit)

(defun vc-sl-update-on-retrieve-tag ()
  "Return whether `vc-retrieve-tag' should update buffers."
  nil)

;;; State querying

;;;###autoload (defun vc-sl-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with Sapling."
;;;###autoload   (if (vc-find-root file ".sl")
;;;###autoload       (progn
;;;###autoload         (load "vc-sl" nil t)
;;;###autoload         (vc-sl-registered file))))

(defun vc-sl-registered (file)
  "Return non-nil if FILE is registered with Sapling."
  (when (vc-sl-root file)
    (let ((state (vc-state file 'Sl)))
      (if (memq state '(ignored unregistered nil))
          (progn
            (vc-file-setprop file 'vc-state nil)
            nil)
        t))))

(defun vc-sl-root (file)
  "Return the Sapling repository root containing FILE, or nil."
  (vc-find-root file ".sl"))

(defalias 'vc-sl-responsible-p #'vc-sl-root)

(defun vc-sl-state (file)
  "Sapling-specific version of `vc-state' for FILE."
  (unless (file-directory-p file)
    (let* ((file (expand-file-name file))
           (root (vc-sl-root file))
           (out (and root
                     (vc-sl--run-to-string
                      (list "status" "-A" "--"
                            (file-relative-name file root))
                      root))))
      (if (or (null out) (equal (cdr out) ""))
          'unregistered
        (pcase (aref (cdr out) 0)
          (?M 'edited)
          (?A 'added)
          (?R 'removed)
          (?! 'missing)
          (?? 'unregistered)
          (?I 'ignored)
          (?C 'up-to-date)
          (_ 'unregistered))))))

(defun vc-sl-working-revision (file)
  "Return the current Sapling working revision for FILE."
  (let* ((root (vc-sl-root file))
         (out (and root
                   (vc-sl--run-to-string
                    '("log" "-r" "." "-T" "{node|short}\n")
                    root))))
    (and out
         (zerop (car out))
         (string-trim (cdr out)))))

;;; State changing

(defun vc-sl-create-repo ()
  "Create a new Sapling repository in the current directory."
  (vc-sl--run nil '("init")))

(defun vc-sl-register (files &optional _comment)
  "Register FILES with Sapling.  COMMENT is ignored."
  (let ((root (vc-sl-root (or (car files) default-directory))))
    (vc-sl--run nil (append '("add") files) root)))

(defun vc-sl-unregister (file)
  "Stop tracking FILE in Sapling without deleting it."
  (vc-sl--run nil (list "forget" file) (vc-sl-root file)))

(defun vc-sl--extract-headers (comment)
  "Extract VC `log-edit' headers from COMMENT for `sl commit'.
The `log-edit' package supplies the header extraction function."
  (log-edit-extract-headers
   '(("Author" . "--user")
     ("Date" . "--date"))
   comment))

(defun vc-sl-checkin (files comment &optional _rev)
  "Commit FILES to Sapling using COMMENT.  REV is ignored."
  (let* ((root (vc-sl-root (or (car files) default-directory)))
         (args (append '("commit" "-m")
                       (vc-sl--extract-headers comment)
                       (and files
                            (cl-loop for file in files
                                     append (list "-I" file))))))
    (vc-sl--run nil args root)))

(defun vc-sl-find-revision (file rev buffer)
  "Insert revision REV of FILE into BUFFER.
When REV is nil, insert the working revision."
  (let* ((root (vc-sl-root file))
         (file (file-relative-name file root))
         (args (if rev
                   (list "cat" "-r" rev file)
                 (list "cat" file))))
    (vc-sl--run buffer args root 'binary)))

(defun vc-sl-checkout (file &optional rev)
  "Check out revision REV of FILE into the working area."
  (let* ((root (vc-sl-root file))
         (file (file-relative-name file root))
         (args (if rev
                   (list "cat" "-r" rev file)
                 (list "cat" file))))
    (vc-sl--run (list :file file) args root 'binary)))

(defun vc-sl-revert (file &optional contents-done)
  "Revert FILE back to the working revision.
When CONTENTS-DONE is non-nil, only update VC's view of FILE."
  (unless contents-done
    (vc-sl--run nil (list "revert" file) (vc-sl-root file))))

(defun vc-sl-delete-file (file)
  "Delete FILE and mark it as deleted in the Sapling repository."
  (let ((root (vc-sl-root file)))
    (condition-case nil
        (delete-file file)
      (file-error nil))
    (vc-sl--run nil (list "remove" "--mark" "--force" file) root)))

(defun vc-sl-rename-file (old new)
  "Rename OLD to NEW in the working area and the Sapling repository."
  (let ((root (vc-sl-root (or old new))))
    (vc-sl--run nil
                (list "rename" (expand-file-name old) (expand-file-name new))
                root)))

(defun vc-sl-find-ignore-file (file)
  "Return the ignore file that controls FILE."
  (expand-file-name ".gitignore" (vc-sl-root file)))

;;; History

(defun vc-sl-print-log (files buffer &optional shortlog start-revision limit)
  "Print the Sapling log for FILES into BUFFER.
When SHORTLOG is non-nil, use one-line log entries.  When
START-REVISION is non-nil, start from that revision.  When LIMIT is
non-nil, show at most LIMIT entries."
  (vc-setup-buffer buffer)
  (let* ((root (vc-sl-root (or (car files) default-directory)))
         (with-diff (with-current-buffer buffer
                      (eq vc-log-view-type 'with-diff)))
         (args (append
                '("log")
                (when start-revision
                  (list "-r" (format "%s::0" start-revision)))
                (when limit (list "-l" (number-to-string limit)))
                (when with-diff '("-p"))
                (list "-T"
                      (if shortlog
                          vc-sl-log-short-format
                        vc-sl-log-format))
                files)))
    (with-current-buffer buffer
      (setq default-directory (or root default-directory)))
    (vc-sl--run-async buffer args root)))

(defun vc-sl-expanded-log-entry (revision)
  "Return a detailed description of REVISION for the VC log buffer."
  (with-temp-buffer
    (vc-sl--run (current-buffer) (list "show" revision)
                (vc-sl-root default-directory))
    (goto-char (point-min))
    (unless (eobp)
      (indent-region (point-min) (point-max) 2)
      (goto-char (point-max))
      (buffer-string))))

(define-derived-mode vc-sl-log-view-mode log-view-mode "Sl-Log-View"
  "Major mode for viewing Sapling logs under VC."
  (require 'add-log)
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  (setq-local log-view-message-re
              (if (eq vc-log-view-type 'short)
                  vc-sl-log-short-regexp
                vc-sl-log-long-regexp))
  (setq-local tab-width 2)
  (when (eq vc-log-view-type 'short)
    (setq truncate-lines t)
    (setq-local log-view-expanded-log-entry-function
                #'vc-sl-expanded-log-entry))
  (setq-local log-view-font-lock-keywords
              (if (eq vc-log-view-type 'short)
                  (list (cons vc-sl-log-short-regexp
                              '((1 'log-view-message)
                                (2 'change-log-date)
                                (3 'change-log-name))))
                (append
                 log-view-font-lock-keywords
                 '(("^user:[ \t]+\\(.*\\)" (1 'change-log-name))
                   ("^date:[ \t]+\\(.*\\)" (1 'change-log-date))
                   ("^summary:[ \t]+\\(.*\\)" (1 'log-view-message)))))))

(defun vc-sl-diff (files &optional rev1 rev2 buffer async)
  "Get a Sapling difference report for FILES.
REV1 and REV2 specify the revisions to compare, and BUFFER is the
output buffer.  When ASYNC is non-nil, run the command asynchronously."
  (let* ((firstfile (car files))
         (working (and firstfile (vc-working-revision firstfile)))
         (root (vc-sl-root (or firstfile default-directory)))
         (args (append '("diff")
                       (vc-switches 'sl 'diff)
                       (when rev1
                         (if rev2
                             (list "-r" rev1 "-r" rev2)
                           (list "-r" rev1)))
                       files)))
    (when (and (equal rev1 working) (not rev2))
      (setq args (append '("diff")
                         (vc-switches 'sl 'diff)
                         files)))
    (when (and (not rev1) rev2)
      (setq args (append '("diff")
                         (vc-switches 'sl 'diff)
                         (list "-r" working "-r" rev2)
                         files)))
    (if async
        (progn
          (vc-sl--run-async (or buffer "*vc-diff*") args root)
          t)
      (let ((buf (or buffer "*vc-diff*")))
        (ignore (vc-sl--run buf args root))
        (and (buffer-live-p (get-buffer buf))
             (> (buffer-size (get-buffer buf)) 0))))))

(defun vc-sl-annotate-command (file buffer &optional revision)
  "Execute `sl annotate' on FILE, inserting the output into BUFFER.
Optional REVISION is the revision to annotate from."
  (let* ((root (vc-sl-root file))
         (file (file-relative-name file root))
         (args (append '("annotate" "-c")
                       (when revision (list "-r" revision))
                       (list file))))
    (vc-sl--run buffer args root)))

(defun vc-sl-annotate-time ()
  "Return the time of the next Sapling annotation line."
  (when (looking-at vc-sl-annotate-re)
    (goto-char (match-end 0))
    (let ((str (match-string-no-properties 2)))
      (vc-annotate-convert-time
       (encode-time 0 0 0
                    (string-to-number (substring str 8 10))
                    (string-to-number (substring str 5 7))
                    (string-to-number (substring str 0 4)))))))

(defun vc-sl-annotate-extract-revision-at-line ()
  "Return the Sapling revision corresponding to the current annotation line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-sl-annotate-re)
      (match-string-no-properties 1))))

(defun vc-sl-previous-revision (_file rev)
  "Return the revision preceding REV, or nil if there is none."
  (let ((out (vc-sl--run-to-string
              (list "log" "-r" (format "%s^" rev)
                    "-T" "{node|short}\n"))))
    (and out
         (zerop (car out))
         (string-trim (cdr out)))))

(defun vc-sl-next-revision (_file rev)
  "Return the revision following REV, or nil if there is none."
  (let ((out (vc-sl--run-to-string
              (list "log" "-r" (format "children(%s)" rev)
                    "-l" "1" "-T" "{node|short}\n"))))
    (and out
         (zerop (car out))
         (string-trim (cdr out)))))

(defun vc-sl-revision-table (_files)
  "Return a list of Sapling bookmarks for revision completion."
  (let ((out (vc-sl--run-to-string '("bookmark"))))
    (and out
         (with-temp-buffer
           (insert (cdr out))
           (goto-char (point-min))
           (let (bookmarks)
             (while (re-search-forward "^[ *]+\\([^ \n]+\\)" nil t)
               (push (match-string 1) bookmarks))
             (nreverse bookmarks))))))

(defun vc-sl-revision-completion-table (files)
  "Return a completion table for existing Sapling revisions of FILES."
  (completion-table-dynamic
   (lambda (_string) (vc-sl-revision-table files))))

;;; Tag/branch system

(defun vc-sl-create-tag (dir name _branchp)
  "Create a Sapling bookmark NAME in DIR.
Sapling bookmarks serve as both tags and lightweight branches."
  (let ((default-directory dir))
    (vc-sl--run nil (list "bookmark" name))))

(defun vc-sl-retrieve-tag (dir name _update)
  "Check out Sapling bookmark NAME in DIR."
  (let ((default-directory dir))
    (vc-sl--run nil (if (equal name "")
                        '("goto")
                      (list "goto" name)))))

;;; Directory status

(defun vc-sl-dir-status-files (dir files update-function)
  "Produce the Sapling directory status for FILES in DIR.
Call UPDATE-FUNCTION with the resulting list when it is ready."
  (let ((args (if files
                  (append '("status" "-A" "-C" "--") files)
                '("status" "-mardu" "-C"))))
    (vc-sl--run-async
     (current-buffer) args dir
     (lambda (proc _event)
       (when (buffer-live-p (process-buffer proc))
         (with-current-buffer (process-buffer proc)
           (funcall update-function (vc-sl--parse-dir-status (buffer-string)))))))))

(defun vc-sl--parse-dir-status (text)
  "Parse `sl status' output TEXT into a list of (FILE STATE EXTRA)."
  (let ((result nil)
        (translation '((?M . edited)
                       (?A . added)
                       (?R . removed)
                       (?! . missing)
                       (?? . unregistered)
                       (?I . ignored)
                       (?C . up-to-date))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((status (char-after))
               (state (cdr (assq status translation)))
               (file (and state
                          (buffer-substring-no-properties
                           (+ (point) 2) (line-end-position)))))
          (when file
            (push (list file state nil) result))
          (forward-line))))
    (nreverse result)))

(defun vc-sl-dir-extra-headers (dir)
  "Return extra status headers for a Sapling repository in DIR."
  (let ((out (vc-sl--run-to-string '("summary") dir)))
    (if (and out (zerop (car out)))
        (cdr out)
      "")))

;;; Miscellaneous

(defun vc-sl-repository-url (file-or-dir &optional remote-name)
  "Return the URL of the Sapling repository containing FILE-OR-DIR.
REMOTE-NAME is the configured remote path to query (default \"default\")."
  (let ((default-directory (vc-sl-root file-or-dir)))
    (let ((out (vc-sl--run-to-string
                (list "config"
                      (format "paths.%s" (or remote-name "default"))))))
      (and out
           (zerop (car out))
           (string-trim (cdr out))))))

(defun vc-sl-clone (remote directory rev)
  "Clone REMOTE into DIRECTORY, optionally checking out REV."
  (vc-sl--run nil
              (append '("clone")
                      (when rev (list "-u" rev))
                      (list remote directory))
              default-directory)
  directory)

(provide 'vc-sl)

;;; vc-sl.el ends here
