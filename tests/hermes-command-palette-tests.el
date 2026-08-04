;;; hermes-command-palette-tests.el --- command palette tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-command-palette-dispatches-selected-command ()
  "Palette dispatches selected available command and hides unavailable entries."
  (let ((hermes-command-palette-commands
         '(("Available" . hermes-command-palette-test-command)
           ("Unavailable" . hermes-command-palette-test-missing)))
        called candidates)
    (cl-letf (((symbol-function 'hermes-command-palette-test-command)
               (lambda () (interactive) (setq called t)))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq candidates collection)
                 "Available")))
      (hermes-command-palette))
    (should called)
    (should (equal candidates
                   '(("Available" . hermes-command-palette-test-command))))))

(ert-deftest hermes-command-palette-is-bound-on-main-surfaces ()
  "Dashboard and chat expose the command palette through one shared key."
  (should (eq (keymap-lookup hermes-dashboard-mode-map "C-c C-p")
              #'hermes-command-palette))
  (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-p")
              #'hermes-command-palette)))

(ert-deftest hermes-command-palette-lists-runnable-commands ()
  "Every command listed after loading Hermes is interactive."
  (dolist (entry hermes-command-palette-commands)
    (should (commandp (cdr entry)))))

(provide 'hermes-command-palette-tests)
;;; hermes-command-palette-tests.el ends here
