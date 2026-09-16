;;; hermes-dependency-tests.el --- Dependency compatibility tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise the real popup library, including its load-time macros.  The
;; minimum-keymap-popup Nix check runs this suite with the release dependency,
;; separately from the development pin, and requires exact version equality.

;;; Code:

(require 'ert)
(require 'lisp-mnt)
(require 'package)
(require 'hermes)

(defvar hermes-test-keymap-popup-minimum nil
  "Non-nil means require the declared minimum, not a newer dependency.")

(ert-deftest hermes-dependency-keymap-popup-version ()
  "Check the loaded library against the package's declared requirement."
  (let* ((library (symbol-file 'keymap-popup 'defun))
         (source (concat (file-name-sans-extension library) ".el"))
         (actual (version-to-list (lm-version source)))
         (minimum
          (with-temp-buffer
            (insert-file-contents (locate-library "hermes.el"))
            (cadr (assq 'keymap-popup
                        (package-desc-reqs (package-buffer-info)))))))
    (message "keymap-popup loaded from %s; version %S; minimum %S"
             library actual minimum)
    (should minimum)
    (should (version-list-<= minimum actual))
    (when hermes-test-keymap-popup-minimum
      (should (equal minimum actual)))))

(ert-deftest hermes-dependency-public-popup-entry-and-dismissal ()
  "Open real popups through public keys without changing the owner buffer."
  (save-window-excursion
    (dolist (mode '(hermes-dashboard-mode hermes-system-mode))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (let ((hermes-dashboard-stale-refresh-interval nil)
              (keymap-popup-backend #'keymap-popup-backend-side-window)
              (keymap-popup--buffer-name " *Hermes dependency popup*"))
          (funcall mode)
          (let ((owner (current-buffer))
                (text (buffer-string))
                (position (point)))
            (unwind-protect
                (progn
                  (execute-kbd-macro (kbd "?"))
                  (should (get-buffer-window keymap-popup--buffer-name))
                  (with-current-buffer keymap-popup--buffer-name
                    (should (string-match-p "Refresh" (buffer-string)))
                    (should (string-match-p "Help" (buffer-string))))
                  (condition-case nil
                      (execute-kbd-macro (kbd "C-g"))
                    (quit nil))
                  (should-not (get-buffer keymap-popup--buffer-name))
                  (should (eq (current-buffer) owner))
                  (should (eq major-mode mode))
                  (should (equal (buffer-string) text))
                  (should (= (point) position)))
              (keymap-popup-dismiss))))))))

(provide 'hermes-dependency-tests)
;;; hermes-dependency-tests.el ends here
