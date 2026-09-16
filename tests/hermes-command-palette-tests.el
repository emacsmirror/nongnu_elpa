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

(ert-deftest hermes-command-palette-primary-entry-journeys ()
  "Chat discovery reaches the existing hub and provider command boundaries."
  (save-window-excursion
    (hermes-test-with-chat-buffer
     (buffer-enable-undo)
     (insert "  draft stays literal\n")
     (let ((owner (current-buffer))
           (point (point))
           (undo (copy-tree buffer-undo-list))
           (hermes-dashboard-buffer-name " *palette dashboard test*")
           (keymap-popup--buffer-name " *palette popup test*")
           (keymap-popup-backend #'keymap-popup-backend-side-window)
           calls accounts)
       (unwind-protect
           (cl-letf (((symbol-function 'hermes-dashboard--check-auth) #'ignore)
                     ((symbol-function 'hermes-dashboard--warm-profile-cache) #'ignore)
                     ((symbol-function 'hermes-browser--existing-client)
                      (lambda () (hermes-test--dashboard-client)))
                     ((symbol-function 'hermes-dashboard-transport-call-fn)
                      (lambda (command &rest _)
                        (push command calls)
                        (hermes--promise-make)))
                     ((symbol-function 'hermes-dashboard-transport-api-request-async)
                      (lambda (method path &rest _)
                        (push (list method path) calls)
                        (hermes--promise-make))))
             (dolist (choice '("Dashboard" "Provider accounts" "Connect provider (API key)"))
               (switch-to-buffer owner)
               (let ((completing-read-function
                      (lambda (_prompt collection &rest _)
                        (should (assoc choice collection))
                        choice)))
                 (call-interactively (key-binding (kbd "C-c C-p"))))
               (pcase choice
                 ("Dashboard"
                  (should (derived-mode-p 'hermes-dashboard-mode))
                  (keymap-popup-dismiss))
                 ("Provider accounts"
                  (setq accounts (window-buffer (selected-window)))
                  (with-current-buffer accounts
                    (should (derived-mode-p 'hermes-provider-accounts-mode)))
                  (should (member '("GET" "/api/providers/oauth") calls)))
                 (_ (should (memq #'hermes-dashboard-transport-model-options-cached calls)))))
             (with-current-buffer owner
               (should (equal (hermes-chat-input-string) "  draft stays literal\n"))
               (should (= (point) point))
               (should (equal buffer-undo-list undo))))
         (keymap-popup-dismiss)
         (dolist (buffer (list accounts (get-buffer hermes-dashboard-buffer-name)))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-command-palette-cancel-preserves-composer ()
  "Cancelling the completion reader leaves the chat's editing state alone."
  (hermes-test-with-chat-buffer
   (buffer-enable-undo)
   (insert "  unsent λ\n")
   (backward-char 2)
   (let ((point (point)) (undo (copy-tree buffer-undo-list)) cancelled
         (completing-read-function (lambda (&rest _) (signal 'quit nil))))
     (condition-case nil
         (call-interactively (key-binding (kbd "C-c C-p")))
       (quit (setq cancelled t)))
     (should cancelled)
     (should (equal (hermes-chat-input-string) "  unsent λ\n"))
     (should (= (point) point))
     (should (equal buffer-undo-list undo)))))

(provide 'hermes-command-palette-tests)
;;; hermes-command-palette-tests.el ends here
