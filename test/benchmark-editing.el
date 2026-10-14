;;; benchmark-editing.el --- Measure real graphical editing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Launch a separate, clean graphical Emacs:
;;   emacs -Q --eval '(progn (require (quote package)) (package-initialize))' \
;;     -L . -l test/benchmark-editing.el
;; Modes: native, appear (default), math.  The process exits when finished.
;; Input uses the normal command loop, not keyboard macros.  Timings include
;; forced redisplay; call counts expose repeated work independently of hardware.
;; APPEAR_BENCH_OUTPUT optionally names a result file.  No notes are edited.

;;; Code:
(require 'cl-lib)
(require 'markdown-ts-appear)

(defvar appear-edit-bench--cases
  '((plain "hello ") (bold "**hi** ") (italic "*hi* ")
    (code "`hi` ") (link "[hi](url) ") (math "$x^2$ ")
    (backspace "**hi** " ("DEL") end)
    (delete "`hi` " ("C-d") start)
    (characters "words " ("C-f" "C-b") start)
    (words "`one` **two** " ("M-f" "M-f" "M-b" "M-b") start)
    (style-boundary "**hi** " ("C-f" "C-f" "C-b" "C-b") 5)
    (table "" ("C-n" "C-p" "C-f" "C-b") table)))
(defvar appear-edit-bench--mode (or (getenv "APPEAR_BENCH_MODE") "appear"))
(defvar appear-edit-bench--directory
  (expand-file-name "../examples/" (file-name-directory load-file-name)))
(defvar appear-edit-bench--buffer nil)
(defvar appear-edit-bench--case nil)
(defvar appear-edit-bench--timer nil)
(defvar appear-edit-bench--sent 0)
(defvar appear-edit-bench--done 0)
(defvar appear-edit-bench--steps 0)
(defvar appear-edit-bench--start nil)
(defvar appear-edit-bench--samples nil)
(defvar appear-edit-bench--gc-start nil)
(defvar appear-edit-bench--results nil)
(defvar appear-edit-bench--calls (make-hash-table))
(defvar appear-edit-bench--advice nil)

(defun appear-edit-bench--summary (values)
  "Return count and latency statistics for VALUES in milliseconds."
  (let* ((sorted (sort (copy-sequence values) #'<)) (n (length sorted)))
    (list :count n :median (nth (/ n 2) sorted)
          :p95 (nth (min (1- n) (floor (* n 0.95))) sorted)
          :max (car (last sorted)))))

(defun appear-edit-bench--finish (&optional error)
  "Print benchmark results and exit, recording ERROR when non-nil."
  (when appear-edit-bench--timer (cancel-timer appear-edit-bench--timer))
  (dolist (entry appear-edit-bench--advice) (advice-remove (car entry) (cdr entry)))
  (let ((report (list :error error :emacs emacs-version :init user-init-file
                      :gc-threshold gc-cons-threshold
                      :mode appear-edit-bench--mode
                      :library (symbol-file 'markdown-ts-appear-mode)
                      :results (nreverse appear-edit-bench--results))))
    (princ (pp-to-string report) #'external-debugging-output)
    (when-let* ((file (getenv "APPEAR_BENCH_OUTPUT")))
      (with-temp-file file (pp report (current-buffer)))))
  ;; The disposable process must not overwrite another session's saved history.
  (let ((kill-emacs-hook nil)) (kill-emacs (if error 1 0))))

(defun appear-edit-bench--send ()
  "Queue the next ordinary input event."
  (let ((text (cadr appear-edit-bench--case))
        (keys (nth 2 appear-edit-bench--case)))
    (when (< appear-edit-bench--sent appear-edit-bench--steps)
      (setq unread-command-events
            (append unread-command-events
                    (if keys
                        (listify-key-sequence
                         (kbd (nth (% appear-edit-bench--sent (length keys)) keys)))
                      (list (aref text (% appear-edit-bench--sent (length text)))))))
      (cl-incf appear-edit-bench--sent))))

(defun appear-edit-bench--pre-command ()
  "Start timing an input command in the benchmark buffer."
  (when (and (eq (current-buffer) appear-edit-bench--buffer)
             (< appear-edit-bench--done appear-edit-bench--sent))
    (when (zerop appear-edit-bench--done)
      (clrhash appear-edit-bench--calls)
      (setq appear-edit-bench--gc-start (cons gcs-done gc-elapsed)))
    (setq appear-edit-bench--start (float-time))))

(defun appear-edit-bench--post-command ()
  "Record command and redraw times, advancing when the scenario is complete."
  (when appear-edit-bench--start
    (let ((command (* 1000 (- (float-time) appear-edit-bench--start)))
          (draw (float-time)))
      (redisplay t)
      (push (cons command (* 1000 (- (float-time) draw))) appear-edit-bench--samples))
    (setq appear-edit-bench--start nil)
    (when (= (cl-incf appear-edit-bench--done) appear-edit-bench--steps)
      (cancel-timer appear-edit-bench--timer)
      (push (list :case (car appear-edit-bench--case)
                  :gc-count (- gcs-done (car appear-edit-bench--gc-start))
                  :gc-ms (* 1000 (- gc-elapsed (cdr appear-edit-bench--gc-start)))
                  :command-ms (appear-edit-bench--summary (mapcar #'car appear-edit-bench--samples))
                  :redisplay-ms (appear-edit-bench--summary (mapcar #'cdr appear-edit-bench--samples))
                  :total-ms (appear-edit-bench--summary
                             (mapcar (lambda (pair) (+ (car pair) (cdr pair))) appear-edit-bench--samples))
                  :calls (let (entries)
                           (maphash (lambda (fn count) (push (cons fn count) entries)) appear-edit-bench--calls)
                           entries))
            appear-edit-bench--results)
      (run-at-time 0.2 nil #'appear-edit-bench--next))))

(defun appear-edit-bench--next ()
  "Prepare the next input scenario in a fresh disposable Markdown buffer."
  (condition-case err
      (if (null appear-edit-bench--cases)
          (appear-edit-bench--finish)
        (unless (display-graphic-p) (error "Graphical Emacs required"))
        (unless (member appear-edit-bench--mode '("native" "appear" "math"))
          (error "APPEAR_BENCH_MODE must be native, appear or math"))
        (when (buffer-live-p appear-edit-bench--buffer) (kill-buffer appear-edit-bench--buffer))
        (setq appear-edit-bench--case (pop appear-edit-bench--cases)
              appear-edit-bench--buffer (generate-new-buffer "*appear-edit-benchmark*")
              appear-edit-bench--sent 0 appear-edit-bench--done 0 appear-edit-bench--samples nil)
        (switch-to-buffer appear-edit-bench--buffer)
        (setq default-directory appear-edit-bench--directory)
        (insert-file-contents "demo.md")
        (let ((markdown-ts-appear-enable-math-preview (equal appear-edit-bench--mode "math")))
          (markdown-ts-mode)
          (markdown-ts-appear-mode (if (equal appear-edit-bench--mode "native") -1 1)))
        (goto-char (point-min)) (forward-line 2)
        (when (bound-and-true-p evil-local-mode) (evil-insert-state))
        (let ((text (cadr appear-edit-bench--case))
              (keys (nth 2 appear-edit-bench--case))
              (start (nth 3 appear-edit-bench--case)))
          (setq appear-edit-bench--steps
                (if (and keys (not (memq (car appear-edit-bench--case) '(backspace delete))))
                    80 (* 10 (length text))))
          (if (eq start 'table)
              (progn
                (re-search-forward "^| Element")
                (beginning-of-line) (forward-line 2) (forward-char 2)
                (when markdown-ts-appear-mode (markdown-ts-appear-stop)))
            (insert "Typing: ")
            (when keys
              (let ((beg (point)))
                (insert (apply #'concat (make-list 10 text)))
                (unless (eq start 'end)
                  (goto-char (+ beg (if (integerp start) start 0))))))))
        (run-hooks 'post-command-hook)
        (font-lock-ensure)
        (redisplay t)
        (unless appear-edit-bench--advice
          (dolist (fn '(jit-lock-function treesit-font-lock-fontify-region
                        markdown-ts-appear-table--render latex-to-svg-frontend--scan))
            (when (fboundp fn)
              (let ((advice (lambda (original &rest args)
                              (puthash fn (1+ (gethash fn appear-edit-bench--calls 0)) appear-edit-bench--calls)
                              (apply original args))))
                (advice-add fn :around advice)
                (push (cons fn advice) appear-edit-bench--advice)))))
        (setq appear-edit-bench--timer (run-at-time 2 0.08 #'appear-edit-bench--send)))
    (error (appear-edit-bench--finish (error-message-string err)))))

(add-hook 'pre-command-hook #'appear-edit-bench--pre-command -100)
(add-hook 'post-command-hook #'appear-edit-bench--post-command 100)
(run-at-time 1 nil #'appear-edit-bench--next)
(run-at-time 180 nil (lambda () (appear-edit-bench--finish "Timed out")))
;;; benchmark-editing.el ends here
