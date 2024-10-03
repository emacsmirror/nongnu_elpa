;;; eldoc-diffstat.el --- Make VCS diffstat available via eldoc  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Johann Klähn

;; Author: Johann Klähn <johann@jklaehn.de>
;; Keywords: vc, docs

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

;; Adapted from https://www.tsdh.org/posts/2022-07-20-using-eldoc-with-magit-async.html

;;; Code:

(require 'ansi-color)

(defvar eldoc-diffstat--process nil)
(defconst eldoc-diffstat--commands
  '((Git "git" "--no-pager" "show" "--color=always"
         "--format=format:%an <%ae>, %aD:%n%s" "--stat=80")
    (Hg "hg" "--pager=never" "log" "--color=always"
        "--template" "{author}, {date|rfc822date}:\n{desc|firstline}\n"
        "--stat" "--rev")))

;;;###autoload
(defun eldoc-diffstat-setup ()
  "Configure eldoc buffer-locally to display diffstat for revision at point."
  (interactive)
  (unless (bound-and-true-p eldoc-mode)
    (eldoc-mode))
  (add-hook 'eldoc-documentation-functions
            #'eldoc-diffstat--docstring nil 'local))

(defun eldoc-diffstat--docstring (callback &rest _ignored)
  "Display diffstat for revision at point by calling CALLBACK.
Intended for `eldoc-documentation-functions'."
  (when-let* ((info (or (when-let (((fboundp 'magit-commit-at-point))
                                   (revision (magit-commit-at-point)))
                          (cons 'Git revision))
                        (and (derived-mode-p 'vc-annotate-mode)
                             (boundp 'vc-annotate-backend)
                             (fboundp 'vc-annotate-extract-revision-at-line)
                             (cons vc-annotate-backend
                                   (car (vc-annotate-extract-revision-at-line))))
                        (and (derived-mode-p 'log-view-mode)
                             (boundp 'log-view-vc-backend)
                             (cons log-view-vc-backend
                                   (log-view-current-tag)))))
              (backend (car info))
              (revision (cdr info))
              (command (alist-get backend eldoc-diffstat--commands)))
    (if-let ((result (eldoc--diffstat--get-cache info)))
        (funcall callback result)
      (eldoc-diffstat--docstring-1 (append command (list revision)) callback info))))

(defun eldoc--diffstat--get-cache (cache-tag)
  "Retrieve cached diffstat result for CACHE-TAG if available.
CACHE-TAG is a cons cell of the form (BACKEND . REVISION) where BACKEND is
a symbol representing the version control system and REVISION is a string
identifying the specific revision.  Returns the cached result if available,
nil otherwise."
  (when-let* (((processp eldoc-diffstat--process))
              (cached-result (process-get eldoc-diffstat--process :cached-result))
              ((equal cache-tag (car cached-result))))
    (cdr cached-result)))

(defun eldoc-diffstat--docstring-1 (command callback cache-tag)
  "Asynchronously compute diffstat using COMMAND and pass it to CALLBACK.
This function sets up a new asynchronous process to compute the diffstat,
killing any existing process.  CACHE-TAG is a unique identifier used for
caching the result, see `eldoc-diffstat--get-cache' for details."
  ;; Clean up old process and its buffer.
  (when (processp eldoc-diffstat--process)
    (when (process-live-p eldoc-diffstat--process)
      (let (confirm-kill-processes)
        (kill-process eldoc-diffstat--process)))
    (kill-buffer (process-buffer eldoc-diffstat--process)))

  (setq
   eldoc-diffstat--process
   (make-process
    :name "eldoc-diffstat"
    :buffer (generate-new-buffer " *eldoc-diffstat*")
    :noquery t
    :command command
    :sentinel
    (apply-partially #'eldoc-diffstat--sentinel callback)))
  (process-put eldoc-diffstat--process :cache-tag cache-tag)

  ;; Signal that the doc string is computed asynchronously.
  t)

(defun eldoc-diffstat--sentinel (callback proc event)
  "Display output of PROC by calling CALLBACK if EVENT indicates success."
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
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
      (reverse-region (point) (point-max))
      (let ((result (buffer-string))
            (cache-tag (process-get eldoc-diffstat--process :cache-tag)))
        (process-put eldoc-diffstat--process :cached-result
                     (cons cache-tag result))
        (funcall callback result)))))

(provide 'eldoc-diffstat)
;;; eldoc-diffstat.el ends here
