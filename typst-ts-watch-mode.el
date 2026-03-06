;;; typst-ts-watch-mode.el --- Watch typst file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 The typst-ts-mode Project Contributors

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for watching(hot compile) current typst file.

;;; Code:

(require 'seq)
(require 'typst-ts-compile)
(require 'typst-ts-variables)

(defgroup typst-ts-watch nil
  "Typst TS Watch."
  :prefix "typst-ts-watch"
  :group 'typst-ts)

(defvar typst-ts-watch--sessions (make-hash-table :test #'equal)
  "Watch sessions keyed by normalized target file path.
Each value is a plist with keys :process and :process-buffer.")

(defvar-local typst-ts-watch--last-session-key nil
  "Last resolved watch session key in current buffer.")

(define-minor-mode typst-ts-watch-mode
  "Watch(hot compile) current typst file."
  :lighter " [Watch]"
  :group 'typst-ts-watch
  (if typst-ts-watch-mode
      (typst-ts-watch-start)
    (typst-ts-watch-stop)))

(defun typst-ts-watch--normalize-file (file)
  "Normalize FILE for using as session key."
  (let ((path (expand-file-name file)))
    (if (file-exists-p path)
        (file-truename path)
      path)))

(defun typst-ts-watch--session-key (&optional buffer)
  "Get normalized session key for BUFFER.
If BUFFER is nil, use current buffer."
  (and-let* ((target-file (typst-ts-compile-get-target-file-argument buffer)))
    (typst-ts-watch--normalize-file target-file)))

(defun typst-ts-watch--buffer-session-key (buffer)
  "Get normalized session key of BUFFER."
  (with-current-buffer buffer
    (when (and (derived-mode-p 'typst-ts-mode)
               (buffer-file-name))
      (typst-ts-watch--session-key buffer))))

(defun typst-ts-watch--buffers-for-session (session-key)
  "Get all live Typst buffers whose session key is SESSION-KEY."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (equal (typst-ts-watch--buffer-session-key buffer) session-key)))
   (buffer-list)))

(defun typst-ts-watch--set-buffer-watch-state (buffer state)
  "Set typst watch STATE for BUFFER without side effects."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'typst-ts-mode)
        (setq-local typst-ts-watch-mode state)
        (force-mode-line-update)))))

(defun typst-ts-watch--sync-session-state (session-key state)
  "Set watch STATE for all open Typst buffers of SESSION-KEY."
  (dolist (buffer (typst-ts-watch--buffers-for-session session-key))
    (typst-ts-watch--set-buffer-watch-state buffer state)))

(defun typst-ts-watch--cleanup-session (session-key)
  "Cleanup watch session for SESSION-KEY and sync mode state."
  (when-let* ((session (gethash session-key typst-ts-watch--sessions)))
    (remhash session-key typst-ts-watch--sessions)
    (let ((process (plist-get session :process))
          (buffer (plist-get session :process-buffer)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (let ((window (get-buffer-window buffer)))
          (kill-buffer buffer)
          (when window
            (delete-window window)))))
    (run-hooks typst-ts-watch-after-watch-hook)
    (typst-ts-watch--sync-session-state session-key nil)))

(defun typst-ts-watch--handle-target-change (&optional old-session-key)
  "Handle target/session key change in current buffer.
OLD-SESSION-KEY is previous resolved session key."
  (when (and (derived-mode-p 'typst-ts-mode)
             (buffer-file-name))
    (let* ((old-key (or old-session-key typst-ts-watch--last-session-key))
           (new-key (typst-ts-watch--session-key))
           (watch-enabled typst-ts-watch-mode))
      (setq-local typst-ts-watch--last-session-key new-key)
      ;; If old key has no buffers attached anymore, stop that session.
      (when (and old-key
                 (not (equal old-key new-key))
                 (gethash old-key typst-ts-watch--sessions)
                 (null (typst-ts-watch--buffers-for-session old-key)))
        (typst-ts-watch--cleanup-session old-key))
      ;; Keep current buffer's mode indicator/process behavior consistent.
      (if watch-enabled
          (if (and new-key (gethash new-key typst-ts-watch--sessions))
              (typst-ts-watch--sync-session-state new-key t)
            (typst-ts-watch-start))
        (typst-ts-watch--set-buffer-watch-state
         (current-buffer)
         (and new-key (not (null (gethash new-key typst-ts-watch--sessions)))))))))

(defun typst-ts-watch--main-file-variable-watcher (_symbol _newval operation where)
  "React to `typst-main-file' updates from buffer WHERE.
OPERATION follows `add-variable-watcher'."
  (when (and (memq operation '(set let make-local kill-local))
             (bufferp where)
             (buffer-live-p where))
    (with-current-buffer where
      (when (derived-mode-p 'typst-ts-mode)
        (let ((old-key typst-ts-watch--last-session-key))
          (typst-ts-watch--handle-target-change old-key))))))

(defun typst-ts-watch--process-sentinel (session-key)
  "Create process sentinel for watch process of SESSION-KEY."
  (lambda (process _event)
    (unless (process-live-p process)
      (typst-ts-watch--cleanup-session session-key))))

(defun typst-ts-watch--sync-current-buffer-from-session ()
  "Sync watch indicator in current buffer from existing watch session."
  (setq-local typst-ts-watch--last-session-key (typst-ts-watch--session-key))
  (typst-ts-watch--set-buffer-watch-state
   (current-buffer)
   (and typst-ts-watch--last-session-key
        (not (null (gethash typst-ts-watch--last-session-key
                            typst-ts-watch--sessions)))))
  ;; File-local variables may update `typst-main-file' after mode init.
  ;; Keep this buffer in sync even if variable watchers miss that transition.
  (add-hook 'hack-local-variables-hook #'typst-ts-watch--handle-target-change nil t))

(add-hook 'typst-ts-mode-hook #'typst-ts-watch--sync-current-buffer-from-session)
(when (fboundp 'remove-variable-watcher)
  (remove-variable-watcher 'typst-main-file
                           #'typst-ts-watch--main-file-variable-watcher))
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher 'typst-main-file
                        #'typst-ts-watch--main-file-variable-watcher))


(defun typst-ts-watch--process-filter (proc output)
  "Filter the `typst watch' process output.
Only error will be transported to the process buffer.
See `(info \"(elisp) Filter Functions\")'.
PROC: process; OUTPUT: new output from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (let ((window (get-buffer-window))
            (re (rx bol "error:" (+ not-newline) "\n" (+ blank) "┌─ "
                    (+ not-newline) ":"  ; file
                    (+ num) ":"  ; start-line
                    (+ num) "\n"  ; start-col
                    (+ (+ (or blank num)) (or "│" "=") (* not-newline) "\n")))
            (next-match-start-pos 0)
            res-output)
        (while (string-match re output next-match-start-pos)
          (setq res-output (concat
                            res-output
                            (when res-output "\n")
                            (substring output (match-beginning 0) (match-end 0)))
                next-match-start-pos (match-end 0)))
        ;; Insert the Error text
        (if (not res-output)
            (when (and typst-ts-watch-auto-display-compilation-error window)
              (delete-window window))
          (insert res-output)
          (goto-char (point-min))
          (when typst-ts-watch-auto-display-compilation-error
            (typst-ts-watch-display-buffer
             (process-get proc 'typst-ts-watch-session-key))))))))

;;;###autoload
(defun typst-ts-watch-display-buffer (&optional session-key)
  "Display typst watch process buffer."
  (interactive)
  (let* ((key (or session-key (typst-ts-watch--session-key)))
         (session (and key (gethash key typst-ts-watch--sessions)))
         (buf (and session (plist-get session :process-buffer))))
    (when (and (called-interactively-p 'interactive)
               (not (buffer-live-p buf)))
      (user-error "No typst watch process is alive for current target file"))
    (when (buffer-live-p buf)
      (display-buffer buf typst-ts-watch-display-buffer-parameters))))

;;;###autoload
(defun typst-ts-watch-start ()
  "Watch(hot compile) current typst file."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (and-let* ((target-file (typst-ts-compile-get-target-file-argument))
             (session-key (typst-ts-watch--normalize-file target-file))
             (result-file (typst-ts-compile-get-result-pdf-filename)))
    (if (gethash session-key typst-ts-watch--sessions)
        (message "Watch is already running for target file: %s" target-file)
      (run-hooks typst-ts-watch-before-watch-hook)
      (let* ((process-name (format "%s<%s>" typst-ts-watch-process-name target-file))
             (process-buffer-name (format "%s<%s>" typst-ts-watch-process-buffer-name target-file))
             (process-buffer (get-buffer-create process-buffer-name))
             process)
        (with-current-buffer process-buffer
          (erase-buffer)
          (unless (derived-mode-p 'typst-ts-compilation-mode)
            (typst-ts-compilation-mode)
            (read-only-mode -1)))
        (setq process
              (apply
               #'start-process
               process-name process-buffer-name
               typst-ts-compile-executable-location
               "watch"
               target-file
               result-file
               typst-ts-watch-options))
        (process-put process 'typst-ts-watch-session-key session-key)
        (set-process-filter process #'typst-ts-watch--process-filter)
        (set-process-sentinel process (typst-ts-watch--process-sentinel session-key))
        (puthash session-key
                 (list :process process
                       :process-buffer process-buffer)
                 typst-ts-watch--sessions)
        (message "Start Watch: %s" target-file)))
    (typst-ts-watch--sync-session-state session-key t)))

;;;###autoload
(defun typst-ts-watch-stop ()
  "Stop watch process."
  (interactive)
  (and-let* ((session-key (typst-ts-watch--session-key))
             (target-file (typst-ts-compile-get-target-file-argument)))
    (if (gethash session-key typst-ts-watch--sessions)
        (progn
          (typst-ts-watch--cleanup-session session-key)
          (message "Stop Watch: %s" target-file))
      (typst-ts-watch--sync-session-state session-key nil)
      (message "Watch is not running for target file: %s" target-file))))

(provide 'typst-ts-watch-mode)
;;; typst-ts-watch-mode.el ends here
