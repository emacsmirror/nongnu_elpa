;;; eldoc-diffstat.el --- Make VCS diffstat available via eldoc  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Johann Klähn

;; Author: Johann Klähn <johann@jklaehn.de>
;; Keywords: vc, docs
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

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

;; This package provides a way to display VCS diffstat information via eldoc.
;; It supports Git and Mercurial repositories.
;;
;; To use, call `eldoc-diffstat-setup' in the desired buffer or mode hook.
;; You might also want to add the following to your config:
;;
;;   (eldoc-add-command
;;    'magit-next-line 'magit-previous-line
;;    'magit-section-forward 'magit-section-backward
;;    'magit-section-forward-sibling 'magit-section-backward-sibling)
;;
;; Adapted from Tassilo Horn's blog post:
;; https://www.tsdh.org/posts/2022-07-20-using-eldoc-with-magit-async.html

;;; Code:

(require 'ansi-color)
(require 'eieio)

(declare-function log-view-current-tag "log-view")
(declare-function magit-commit-at-point "magit-git")
(declare-function magit-stash-at-point "magit-git")
;; These slots are used in git-rebase-action (see git-rebase.el).
(eieio-declare-slots action-type target)

(defgroup eldoc-diffstat nil
  "Show VCS diffstat information in echo area."
  :group 'eldoc
  :group 'vc)

(defcustom eldoc-diffstat-lines nil
  "If non-nil, truncate echo area output after this many lines.

If the value is a negative number, force output to exactly the absolute
value number of lines, either by truncating or padding with empty lines
as needed.

See also `eldoc-echo-area-use-multiline-p'."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum number of lines (positive values)" 5)
                 (integer :tag "Fixed number of lines (negative values)" -5)))

(defvar eldoc-diffstat--process nil
  "The latest async process used for fetching diffstat information.
Only one active process at a time; new requests terminate previous ones.
After completion, a cached version of the diffstat output is attached as
a property to the process object.")

(defconst eldoc-diffstat--commands
  '((Git "git" "--no-pager" "show" "--color=always"
         "--format=format:%an <%ae>, %aD:%n%s" "--stat=80")
    (Hg "hg" "--pager=never" "log" "--color=always"
        "--template" "{author}, {date|rfc822date}:\n{desc|firstline}\n"
        "--stat" "--rev"))
  "Alist mapping VCS backend to the command to use for computing the diffstat.")

(defconst eldoc-diffstat--output-regex
  (rx buffer-start
      ;; 1: commit author and date
      ;; 2: newline between author info and subject (will be removed)
      (group-n 1 (+ not-newline)) (group-n 2 ?\n)
      ;; subject / first line of commit message
      (+ not-newline) ?\n
      ;; 3: point after subject / before diffstat (shortstat will be moved here)
      (group-n 3)
      ;; diffstat for individual files
      (+? anything)
      ;; 4: shortstat, i.e., total # of modified files + added/deleted lines
      (group-n 4 (+ not-newline) ?\n)
      (* space) buffer-end)
  "Regular expression used to rewrite and apply faces to diffstat output.
Has to match `eldoc-diffstat--commands'.")

;;;###autoload
(define-obsolete-function-alias 'eldoc-diffstat-setup 'eldoc-diffstat-mode
  "1.0" "Configure eldoc buffer-locally to display diffstat for revision at point.")

;;;###autoload
(define-minor-mode eldoc-diffstat-mode
  "Toggle echo area display of VCS diffstat information in the local buffer.

When enabled, diffstat information is shown in supported major modes if
point is on a revision."
  :group 'eldoc-diffstat
  :lighter nil
  (if eldoc-diffstat-mode
      (progn
        (add-hook 'eldoc-documentation-functions
                  #'eldoc-diffstat--docstring nil 'local)
        (eldoc-mode))
    (remove-hook 'eldoc-documentation-functions
                 #'eldoc-diffstat--docstring 'local)
    ;; Disable eldoc if diffstat was the only information source.
    (unless (eldoc--supported-p)
      (eldoc-mode -1))))

;;;###autoload
(define-globalized-minor-mode global-eldoc-diffstat-mode
  eldoc-diffstat-mode eldoc-diffstat-mode
  :group 'eldoc-diffstat
  :predicate
  '(git-rebase-mode
    log-view-mode
    magit-log-mode
    magit-status-mode
    vc-annotate-mode))

(defun eldoc-diffstat--docstring (callback &rest _ignored)
  "Display diffstat for revision at point by calling CALLBACK.
Intended for `eldoc-documentation-functions'."
  (when-let* ((info (eldoc-diffstat--get-revision-info))
              (backend (car info))
              (revision (cdr info))
              (command (alist-get backend eldoc-diffstat--commands)))
    (if-let* ((result (eldoc-diffstat--get-cache info)))
        (funcall callback result)
      (eldoc-diffstat--docstring-1 (append command (list revision)) callback info))))

(defun eldoc-diffstat--get-revision-info ()
  "Get revision info for the current buffer context.
The returned record should be a cons cell of the form (BACKEND . REVISION) where
BACKEND is a symbol representing the version control system and REVISION is
a string identifying the specific revision."
  (cond
   ((when-let* (((fboundp 'magit-stash-at-point))
                (revision (magit-stash-at-point)))
      (cons 'Git revision)))
   ((when-let* (((fboundp 'magit-commit-at-point))
                (revision (magit-commit-at-point)))
      (cons 'Git revision)))
   ((and (derived-mode-p 'git-rebase-mode)
         (fboundp 'git-rebase-current-line)
         (with-slots (action-type target)
             (git-rebase-current-line)
           (and (eq action-type 'commit)
                (cons 'Git target)))))
   ((and (derived-mode-p 'vc-annotate-mode)
         (boundp 'vc-annotate-backend)
         (fboundp 'vc-annotate-extract-revision-at-line))
    (cons vc-annotate-backend
          (car (vc-annotate-extract-revision-at-line))))
   ((and (derived-mode-p 'log-view-mode)
         (boundp 'log-view-vc-backend))
    (cons log-view-vc-backend
          (log-view-current-tag)))))

(defun eldoc-diffstat--get-cache (revision-info)
  "Retrieve cached diffstat result for REVISION-INFO if available."
  (when-let* ((proc eldoc-diffstat--process)
              ((processp proc))
              (cached-result (process-get proc :cached-result))
              ((equal revision-info (car cached-result))))
    (cdr cached-result)))

(defun eldoc-diffstat--docstring-1 (command callback revision-info)
  "Asynchronously compute diffstat using COMMAND and pass it to CALLBACK.
This function sets up a new asynchronous process to compute the diffstat,
killing any existing process.  REVISION-INFO is a unique identifier used for
caching the result, see `eldoc-diffstat--get-cache' for details."
  ;; Clean up old process and its buffer.
  (when (processp eldoc-diffstat--process)
    (when (process-live-p eldoc-diffstat--process)
      (let (confirm-kill-processes)
        (kill-process eldoc-diffstat--process)))
    (kill-buffer (process-buffer eldoc-diffstat--process)))

  (let ((proc (make-process
               :name "eldoc-diffstat"
               :buffer (generate-new-buffer " *eldoc-diffstat*")
               :noquery t
               :command command
               :sentinel
               (apply-partially #'eldoc-diffstat--sentinel callback))))
    (process-put proc :revision-info revision-info)
    (setq eldoc-diffstat--process proc))

  ;; Signal that the doc string is computed asynchronously.
  t)

(defun eldoc-diffstat--sentinel (callback proc _event)
  "Display output of PROC after its exit by calling CALLBACK."
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (eldoc-diffstat--format-output-buffer)
      (let ((result (buffer-string))
            (revision-info (process-get proc :revision-info)))
        (process-put proc :cached-result
                     (cons revision-info result))
        (funcall callback result)))))

(defun eldoc-diffstat--format-output-buffer ()
  "Format the diffstat output in the current buffer."
  ;; Translate color control sequences into text properties.
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (put-text-property beg end 'face face))))
    (ansi-color-apply-on-region (point-min) (point-max)))

  (goto-char (point-min))
  (when (looking-at eldoc-diffstat--output-regex)
    ;; Make commit author and date bold.
    (put-text-property
     (match-beginning 1) (match-end 1) 'face 'bold)

    ;; Join the first two lines.
    (replace-match " " nil nil nil 2)

    ;; Move summary to the second line and make it italic.
    (let ((summary (delete-and-extract-region
                    (match-beginning 4) (match-end 4))))
      (replace-match
       (propertize summary 'face 'italic) nil nil nil 3)))

  ;; Optionally truncate / pad output.
  (when eldoc-diffstat-lines
    (goto-char (point-min))
    (let ((remaining (forward-line (abs eldoc-diffstat-lines))))
      (delete-region (point) (point-max))
      (when (and (< eldoc-diffstat-lines 0) (> remaining 0))
        (newline remaining)
        ;; eldoc skips backwards over spaces, tabs, and newlines, so insert any
        ;; unobstrusive character to work around that, e.g., a no-break space.
        (backward-char) (insert ?\xA0)))))

(provide 'eldoc-diffstat)
;;; eldoc-diffstat.el ends here
