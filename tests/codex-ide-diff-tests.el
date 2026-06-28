;;; codex-ide-diff-tests.el --- Tests for codex-ide-diff.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide-diff)

;;; Pure builder tests

(ert-deftest codex-ide-diff-buffer-name-path ()
  "Path policy names buffers from FILE-PATH and SIDE."
  (let ((codex-ide-diff-buffer-naming 'path))
    (should (equal "*codex-ide-diff:foo.el[old]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'old)))
    (should (equal "*codex-ide-diff:foo.el[new]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'new)))))

(ert-deftest codex-ide-diff-buffer-name-generic ()
  "Generic policy uses an incrementing counter for buffer names."
  (let ((codex-ide-diff-buffer-naming 'generic)
        (codex-ide-diff--counter 0))
    (should (equal "*codex-ide-diff-1[old]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'old)))
    (should (equal "*codex-ide-diff-2[new]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'new)))))

(ert-deftest codex-ide-diff-make-temp-buffer ()
  "Temp buffer creation inserts content and is read-only."
  (let ((buf (codex-ide-diff--make-temp-buffer
              " *test-diff-temp*" "hello world")))
    (unwind-protect
        (with-current-buffer buf
          (should (equal "hello world" (buffer-string)))
          (should buffer-read-only)
          (should-not (buffer-modified-p)))
      (kill-buffer buf))))

(ert-deftest codex-ide-diff-make-temp-buffer-empty ()
  "Temp buffer with nil content is empty, not errored."
  (let ((buf (codex-ide-diff--make-temp-buffer " *test-diff-empty*" nil)))
    (unwind-protect
        (with-current-buffer buf
          (should (string= "" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest codex-ide-diff-make-temp-buffer-pair ()
  "Buffer pair returns (A . B) with correct content, both live."
  (let ((pair (codex-ide-diff--make-temp-buffer-pair
               "old text" "new text" "/tmp/foo.el")))
    (unwind-protect
        (progn
          (should (consp pair))
          (should (buffer-live-p (car pair)))
          (should (buffer-live-p (cdr pair)))
          (with-current-buffer (car pair)
            (should (equal "old text" (buffer-string))))
          (with-current-buffer (cdr pair)
            (should (equal "new text" (buffer-string)))))
      (when (buffer-live-p (car pair)) (kill-buffer (car pair)))
      (when (buffer-live-p (cdr pair)) (kill-buffer (cdr pair))))))

(ert-deftest codex-ide-diff-build-state-shape ()
  "Build-state returns an alist with all six keys."
  (let* ((buf-a (get-buffer-create " *test-state-a*"))
         (buf-b (get-buffer-create " *test-state-b*"))
         (winconf (current-window-configuration))
         (state (codex-ide-diff--build-state
                 "test-tab" buf-a buf-b t winconf "old snapshot")))
    (unwind-protect
        (progn
          (should (equal "test-tab" (cdr (assq 'tab-name state))))
          (should (eq buf-a (cdr (assq 'buffer-A state))))
          (should (eq buf-b (cdr (assq 'buffer-B state))))
          (should (eq t (cdr (assq 'file-exists state))))
          (should (eq winconf (cdr (assq 'saved-winconf state))))
          (should (equal "old snapshot"
                         (cdr (assq 'old-content state)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest codex-ide-diff-build-state-no-old-content ()
  "Build-state omits old-content when not passed."
  (let* ((buf-a (get-buffer-create " *test-state-a2*"))
         (buf-b (get-buffer-create " *test-state-b2*"))
         (winconf (current-window-configuration))
         (state (codex-ide-diff--build-state
                 "test-tab" buf-a buf-b nil winconf)))
    (unwind-protect
        (progn
          (should (eq nil (cdr (assq 'file-exists state))))
          (should-not (assq 'old-content state)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;;; Cleanup tests

(ert-deftest codex-ide-diff-cleanup-kills-buffers ()
  "Cleanup kills both temp buffers."
  (let* ((pair (codex-ide-diff--make-temp-buffer-pair
                "old" "new" "/tmp/test.el"))
         (state (codex-ide-diff--build-state
                 "tab" (car pair) (cdr pair) t
                 (current-window-configuration))))
    (codex-ide-diff--cleanup state)
    (should-not (buffer-live-p (car pair)))
    (should-not (buffer-live-p (cdr pair)))))

(ert-deftest codex-ide-diff-cleanup-idempotent ()
  "Calling cleanup twice does not error."
  (let* ((pair (codex-ide-diff--make-temp-buffer-pair
                "old" "new" "/tmp/test2.el"))
         (state (codex-ide-diff--build-state
                 "tab" (car pair) (cdr pair) t
                 (current-window-configuration))))
    (codex-ide-diff--cleanup state)
    ;; Second call should be a no-op (no error).
    (codex-ide-diff--cleanup state)))

(ert-deftest codex-ide-diff-cleanup-already-dead ()
  "Cleanup is safe when a buffer is already dead."
  (let* ((pair (codex-ide-diff--make-temp-buffer-pair
                "old" "new" "/tmp/test3.el"))
         (state (codex-ide-diff--build-state
                 "tab" (car pair) (cdr pair) t
                 (current-window-configuration))))
    (kill-buffer (car pair))
    ;; Should not error.
    (codex-ide-diff--cleanup state)
    (should-not (buffer-live-p (cdr pair)))))

(ert-deftest codex-ide-diff-cleanup-restores-winconf ()
  "Cleanup calls set-window-configuration with the saved config."
  (let* ((pair (codex-ide-diff--make-temp-buffer-pair
                "old" "new" "/tmp/test4.el"))
         (captured-winconf (current-window-configuration))
         (state (codex-ide-diff--build-state
                 "tab" (car pair) (cdr pair) t captured-winconf))
         restored-winconf)
    (cl-letf (((symbol-function 'set-window-configuration)
               (lambda (winconf)
                 (setq restored-winconf winconf))))
      (codex-ide-diff--cleanup state))
    (should (eq captured-winconf restored-winconf))))

(ert-deftest codex-ide-diff-run-ediff-cancelled-accept-keeps-rejected ()
  "Canceled accept attempt leaves a later reject decision rejected."
  (require 'ediff)
  (let ((buf-a (generate-new-buffer " *test-run-ediff-a*"))
        (buf-b (generate-new-buffer " *test-run-ediff-b*"))
        accept-fn
        really-quit-called)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (_a _b startup-hooks)
                       (mapc #'funcall startup-hooks)))
                    ((symbol-function 'local-set-key)
                     (lambda (key fn)
                       (when (equal key codex-ide-diff-accept-key)
                         (setq accept-fn fn))))
                    ((symbol-function 'y-or-n-p)
                     (lambda (_prompt) nil))
                    ((symbol-function 'ediff-really-quit)
                     (lambda (&rest _args)
                       (setq really-quit-called t)))
                    ((symbol-function 'recursive-edit)
                     (lambda ()
                       (should accept-fn)
                       (funcall accept-fn))))
            (should (eq 'rejected
                        (codex-ide-diff--run-ediff buf-a buf-b "tab")))
            (should-not really-quit-called)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;;; Preview tests (ediff stubbed)

(defmacro codex-ide-diff--with-stubbed-ediff (decision &rest body)
  "Run BODY with `codex-ide-diff--run-ediff-function' returning DECISION.
Also ensures the stub is cleared afterward."
  (declare (indent 1))
  `(let ((codex-ide-diff--run-ediff-function
          (lambda (_a _b _name) ,decision)))
     (unwind-protect
         (progn ,@body)
       (setq codex-ide-diff--run-ediff-function nil))))

(ert-deftest codex-ide-diff-preview-accept ()
  "Stubbed ediff returning `accepted' yields t from --preview."
  (let* ((pair (codex-ide-diff--make-temp-buffer-pair
                "old" "new" "/tmp/p1.el"))
         (state (codex-ide-diff--build-state
                 "tab" (car pair) (cdr pair) t
                 (current-window-configuration))))
    (unwind-protect
        (codex-ide-diff--with-stubbed-ediff 'accepted
          (should (eq t (codex-ide-diff--preview state))))
      (codex-ide-diff--cleanup state))))

(ert-deftest codex-ide-diff-preview-reject ()
  "Stubbed ediff returning `rejected' yields nil from --preview."
  (let* ((pair (codex-ide-diff--make-temp-buffer-pair
                "old" "new" "/tmp/p2.el"))
         (state (codex-ide-diff--build-state
                 "tab" (car pair) (cdr pair) t
                 (current-window-configuration))))
    (unwind-protect
        (codex-ide-diff--with-stubbed-ediff 'rejected
          (should (eq nil (codex-ide-diff--preview state))))
      (codex-ide-diff--cleanup state))))

(ert-deftest codex-ide-diff-preview-cleans-up ()
  "After preview via public API, temp buffers are dead."
  (codex-ide-diff--with-stubbed-ediff 'accepted
    (codex-ide-diff-preview "old content" "new content" "/tmp/p3.el"))
  ;; No temp buffers should linger with the diff prefix.
  (dolist (buf (buffer-list))
    (should-not (string-prefix-p "*codex-ide-diff:" (buffer-name buf)))))

(ert-deftest codex-ide-diff-preview-cleans-on-error ()
  "Cleanup runs even if the stubbed ediff errors."
  (let ((codex-ide-diff--run-ediff-function
         (lambda (_a _b _name) (error "boom"))))
    (unwind-protect
        (should-error (codex-ide-diff-preview "a" "b" "/tmp/p4.el"))
      (setq codex-ide-diff--run-ediff-function nil)))
  (dolist (buf (buffer-list))
    (should-not (string-prefix-p "*codex-ide-diff:" (buffer-name buf)))))

;;; Public entry point test

(ert-deftest codex-ide-diff-preview-compose ()
  "End-to-end public API with stubbed ediff returns the decision."
  (codex-ide-diff--with-stubbed-ediff 'accepted
    (should (eq t (codex-ide-diff-preview "a" "b" "/tmp/c1.el"))))
  (codex-ide-diff--with-stubbed-ediff 'rejected
    (should (eq nil (codex-ide-diff-preview "a" "b" "/tmp/c2.el")))))

(provide 'codex-ide-diff-tests)

;;; codex-ide-diff-tests.el ends here
