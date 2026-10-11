;;; codex-ide-autoload-tests.el --- Autoload gate regression -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise the actual Nix autoload predicates in a fresh process.

;;; Code:

(require 'ert)
(require 'loaddefs-gen)

(defconst codex-ide-autoload-tests--source
  (expand-file-name "../lisp" (file-name-directory load-file-name)))

(ert-deftest codex-ide-autoload-gate ()
  "Generated autoloads satisfy every predicate from the Nix package gate."
  (let* ((source codex-ide-autoload-tests--source)
         (directory (make-temp-file "codex-autoloads-" t))
         (autoloads (expand-file-name "codex-ide-autoloads.el" directory))
         (forms (with-temp-buffer
                  (insert-file-contents (expand-file-name "../flake.nix" source))
                  (search-forward "codex-ide-autoload-check")
                  (let (result)
                    (while (re-search-forward "--eval \"" (save-excursion
                                                                  (search-forward "touch ")
                                                                  (point)) t)
                      (backward-char)
                      (push (read (current-buffer)) result))
                    (nreverse result)))))
    (unwind-protect
        (progn
          (should forms)
          (loaddefs-generate source autoloads)
          (with-temp-buffer
            (let ((status
                   (apply #'call-process
                          (expand-file-name invocation-name invocation-directory) nil t nil
                          (append (list "-Q" "--batch" "--eval"
                                        (prin1-to-string `(setq load-path ',load-path))
                                        "-L" source "-l" autoloads)
                                  (mapcan (lambda (form) (list "--eval" form)) forms)))))
              (ert-info ((buffer-string))
                (should (zerop status))))
            (should (equal (buffer-string) ""))))
      (delete-directory directory t))))

(provide 'codex-ide-autoload-tests)
;;; codex-ide-autoload-tests.el ends here
