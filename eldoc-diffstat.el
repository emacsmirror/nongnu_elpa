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

;;;###autoload
(defun eldoc-diffstat-setup ()
  "Configure eldoc buffer-locally to display diffstat for revision at point."
  (interactive)
  (add-hook 'eldoc-documentation-functions
            #'eldoc-diffstat--docstring nil 'local)
  (unless (bound-and-true-p eldoc-mode)
    (eldoc-mode)))

(defun eldoc-diffstat--docstring (callback &rest _ignored)
  "Display diffstat for revision at point by calling CALLBACK.
Intended for `eldoc-documentation-functions'."
  (when-let* ((info (eldoc-diffstat--get-revision-info))
              (backend (car info))
              (revision (cdr info))
              (command (alist-get backend eldoc-diffstat--commands)))
    (if-let ((result (eldoc-diffstat--get-cache info)))
        (funcall callback result)
      (eldoc-diffstat--docstring-1 (append command (list revision)) callback info))))

(defun eldoc-diffstat--get-revision-info ()
  "Get revision info for the current buffer context.
The returned record should be a cons cell of the form (BACKEND . REVISION) where
BACKEND is a symbol representing the version control system and REVISION is
a string identifying the specific revision."
  (cond
   ((when-let (((fboundp 'magit-stash-at-point))
               (revision (magit-stash-at-point)))
      (cons 'Git revision)))
   ((when-let (((fboundp 'magit-commit-at-point))
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
  "Format the diffstat output."
  ;; Translate color control sequences into text properties.
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (put-text-property beg end 'face face))))
    (ansi-color-apply-on-region (point-min) (point-max)))

  ;; Delete trailing blank lines.
  (goto-char (point-max))
  (delete-blank-lines)

  ;; Make first line bold.
  (goto-char (point-min))
  (put-text-property (point)
                     (line-end-position)
                     'face 'bold)

  ;; Join second line.
  (forward-line)
  (join-line)

  ;; Move summary to the top and make it italic.
  (forward-line)
  (reverse-region (point) (point-max))
  (put-text-property (point)
                     (line-end-position)
                     'face 'italic)
  (forward-line)
  (reverse-region (point) (point-max)))

(provide 'eldoc-diffstat)
;;; eldoc-diffstat.el ends here
