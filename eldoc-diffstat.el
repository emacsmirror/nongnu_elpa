;;; eldoc-diffstat.el --- Make git diffstat available via eldoc  -*- lexical-binding: t; -*-

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
(defconst eldoc-diffstat--command
  (list "git" "--no-pager" "show" "--color=always"
        "--format=format:%an <%ae>, %aD:%n%s" "--stat=80"))

;;;###autoload
(defun eldoc-diffstat-setup ()
  "Configure eldoc buffer-locally to display diffstat for commit at point."
  (interactive)
  (add-hook 'eldoc-documentation-functions
            #'eldoc-diffstat--docstring nil 'local))

(defun eldoc-diffstat--docstring (callback &rest _ignored)
  "Display diffstat for commit at point by calling CALLBACK.
Intended for `eldoc-documentation-functions'."
  (when-let* ((commit (or (when (fboundp 'magit-commit-at-point)
                            (magit-commit-at-point))
                          (and (derived-mode-p 'vc-annotate-mode)
                               (boundp 'vc-annotate-backend)
                               (eq vc-annotate-backend 'Git)
                               (fboundp 'vc-annotate-extract-revision-at-line)
                               (car (vc-annotate-extract-revision-at-line)))
                          (and (derived-mode-p 'log-view-mode)
                               (boundp 'log-view-vc-backend)
                               (eq log-view-vc-backend 'Git)
                               (log-view-current-tag)))))
    (if-let* (((processp eldoc-diffstat--process))
              (result (process-get eldoc-diffstat--process :result))
              ((equal commit (car result))))
        (funcall callback (cdr result))
      (eldoc-diffstat--docstring-1 commit callback))))

(defun eldoc-diffstat--docstring-1 (commit callback &rest _ignored)
  "Display diffstat for COMMIT by calling CALLBACK."
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
    :command
    (append eldoc-diffstat--command (list commit))
    :sentinel
    (apply-partially #'eldoc-diffstat--sentinel callback)))
  (process-put eldoc-diffstat--process :commit commit)

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
            (commit (process-get eldoc-diffstat--process :commit)))
        (process-put eldoc-diffstat--process :result (cons commit result))
        (funcall callback result)))))

(provide 'eldoc-diffstat)
;;; eldoc-diffstat.el ends here
