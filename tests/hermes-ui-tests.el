;;; hermes-ui-tests.el --- dashboard UI tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `M-x hermes' EWOC dashboard in hermes.el: buffer/popup
;; setup, action and chat-list rendering, status classification, and the
;; provider-onboarding auth gate.  Transport tests live in
;; `hermes-dashboard-tests'.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-dashboard-opens-special-mode-buffer-and-popup ()
  (let (shown-map)
    (cl-letf (((symbol-function 'keymap-popup)
               (lambda (keymap) (setq shown-map keymap))))
      (unwind-protect
          (progn
            (hermes)
            (should (eq major-mode 'hermes-dashboard-mode))
            (should (eq shown-map hermes-dashboard-mode-map))
            (should hermes-dashboard--ewoc)
            (let ((text (buffer-string)))
              (should (string-match-p "Hermes" text))
              (should (string-match-p "Chat" text))
              (should (string-match-p "New session" text))))
        (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-chat-action-is-keymap-popup-binding ()
  (should (eq (keymap-lookup hermes-dashboard-mode-map "c") #'hermes-chat))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "m") #'hermes-dashboard-switch-model))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "X") #'hermes-list-mcp))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "g") #'hermes-dashboard-refresh))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "n") #'hermes-dashboard-next))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "p") #'hermes-dashboard-previous))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "RET") #'hermes-dashboard-open))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "i") #'hermes-dashboard-interrupt))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "s") #'hermes-dashboard-steer))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "a") #'hermes-dashboard-respond))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "?") #'hermes-dashboard-popup))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "h") #'hermes-dashboard-popup))
  (let* ((rows (keymap-popup--meta hermes-dashboard-mode-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows)))
    (dolist (key '("c" "m"))
      (should (cl-find key entries :key (lambda (entry)
                                         (plist-get entry :key))
                       :test #'equal)))))

(ert-deftest hermes-dashboard-previous-reports-at-the-top ()
  "`hermes-dashboard-previous' signals at the first card and moves otherwise."
  (with-temp-buffer
    (let ((hermes-dashboard--ewoc
           (ewoc-create (lambda (x) (insert (format "%s" x))))))
      (ewoc-enter-last hermes-dashboard--ewoc 'a)
      (ewoc-enter-last hermes-dashboard--ewoc 'b)
      (ewoc-goto-node hermes-dashboard--ewoc
                      (ewoc-nth hermes-dashboard--ewoc 0))
      (should-error (hermes-dashboard-previous) :type 'user-error)
      (ewoc-goto-node hermes-dashboard--ewoc
                      (ewoc-nth hermes-dashboard--ewoc 1))
      (hermes-dashboard-previous)
      (should (eq (ewoc-locate hermes-dashboard--ewoc)
                  (ewoc-nth hermes-dashboard--ewoc 0))))))

(ert-deftest hermes-dashboard-status-symbol-does-not-intern-unknown-strings ()
  (let* ((normalized "hermes-unknown-status-from-test")
         (status (replace-regexp-in-string "-" " " normalized)))
    (should-not (intern-soft normalized))
    (should-not (hermes-dashboard--status-symbol status))
    (should-not (intern-soft normalized))
    (should (eq (hermes-dashboard--status-symbol "input requested")
                'input-requested))
    (should (eq (hermes-dashboard--status-symbol "In_Progress")
                'in-progress))))

(ert-deftest hermes-dashboard-repeated-open-cleans-stale-refresh-timers ()
  (let ((hermes-dashboard-buffer-name (hermes-test--dashboard-buffer-name))
        (hermes-dashboard-stale-refresh-interval 3600)
        buffer)
    (cl-letf (((symbol-function 'keymap-popup)
               (lambda (&rest _args) nil)))
      (unwind-protect
          (progn
            (dotimes (_ 3)
              (hermes))
            (setq buffer (get-buffer hermes-dashboard-buffer-name))
            (should (buffer-live-p buffer))
            (should (= 1 (length (hermes-test--dashboard-stale-refresh-timers
                                  buffer))))
            (kill-buffer buffer)
            (should (= 0 (length (hermes-test--dashboard-stale-refresh-timers
                                  buffer)))))
        (when (and buffer (buffer-live-p buffer))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-renders-ewoc-actions-and-empty-state ()
  (hermes-test-with-dashboard-buffer
   (should (eq major-mode 'hermes-dashboard-mode))
   (should hermes-dashboard--ewoc)
   (should (equal (hermes-dashboard--current-ids)
                  '("action:chat" "empty:chats")))
   (let ((text (buffer-string)))
     (should (string-match-p "Chat" text))
     (should (string-match-p "No live Hermes chat buffers" text)))
   (should (eq (plist-get (hermes-test--dashboard-node-data "action:chat") :action)
               #'hermes-chat))
   (goto-char (point-min))
   (search-forward "Chat")
   (should (equal (get-text-property (point) 'hermes-dashboard-node-id)
                  "action:chat"))))

(ert-deftest hermes-dashboard-lists-open-chat-buffers-with-status ()
  (let (chat-buffer chat-name)
    (hermes-test-with-chat-buffer
     (setq chat-buffer (current-buffer)
           chat-name (buffer-name))
     (setq hermes-chat--session-id "sid-dashboard-test")
     (puthash "tool-1" "terminal: make check" hermes-chat--active-tools)
     (puthash "prompt-1" '(:prompt-type "approval") hermes-chat--pending-prompts)
     (hermes-chat--set-header-state
      :status 'running :activity "terminal: make check")
     (hermes-test-with-dashboard-buffer
      (let ((id (format "chat:%s" chat-name))
            (text (buffer-string)))
        (should (member id (hermes-dashboard--current-ids)))
        (should (string-match-p (regexp-quote chat-name) text))
        (should (string-match-p "Running" text))
        (should (string-match-p "terminal: make check" text))
        (should (string-match-p "1 pending prompt" text))
        (should (string-match-p "session sid-dashboard-test" text))
        (should (eq (plist-get (hermes-test--dashboard-node-data id) :buffer)
                    chat-buffer)))))))

(ert-deftest hermes-dashboard-refresh-updates-chat-node ()
  (let (chat-name)
    (hermes-test-with-chat-buffer
     (setq chat-name (buffer-name))
     (hermes-test-with-dashboard-buffer
      (should (string-match-p "Ready" (buffer-string)))
      (with-current-buffer chat-name
        (hermes-chat--set-header-state :status 'error :activity "boom"))
      (hermes-dashboard-refresh)
      (let ((text (buffer-string))
            (chat-id (format "chat:%s" chat-name)))
        (should (string-match-p "Error" text))
        (should (string-match-p "boom" text))
        (should (= 1 (cl-count chat-id (hermes-dashboard--current-ids)
                               :test #'equal))))))))

(ert-deftest hermes-dashboard-open-at-point-switches-to-chat-buffer ()
  (let (chat-buffer chat-name)
    (hermes-test-with-chat-buffer
     (setq chat-buffer (current-buffer)
           chat-name (buffer-name))
     (hermes-test-with-dashboard-buffer
      (search-forward chat-name)
      (hermes-dashboard-open)
      (should (eq (current-buffer) chat-buffer))
      (should (= (point) hermes-chat--input-marker))))))

(ert-deftest hermes-dashboard-selected-chat-actions-error-without-chat-node ()
  (hermes-test-with-dashboard-buffer
   (goto-char (point-min))
   (search-forward "Chat")
   (should-error (hermes-dashboard-interrupt) :type 'user-error)
   (should-error (hermes-dashboard-steer) :type 'user-error)
   (should-error (hermes-dashboard-respond) :type 'user-error)))

(ert-deftest hermes-dashboard-status-helpers-classify-parity-states ()
  (dolist (case '(("in_progress" "Running" hermes-dashboard-status-running)
                  ("busy" "Running" hermes-dashboard-status-running)
                  ("approval requested" "Approval requested"
                   hermes-dashboard-status-waiting)
                  ("input.requested" "Input requested"
                   hermes-dashboard-status-waiting)
                  ("succeeded" "Ready" hermes-dashboard-status-ready)
                  ("interrupted" "Interrupted" hermes-dashboard-status-error)
                  ("disconnected" "Disconnected"
                   hermes-dashboard-status-error)
                  ("backend paused" "Backend Paused" hermes-dashboard-muted)))
    (pcase-let ((`(,status ,label ,face) case))
      (should (equal (hermes-dashboard--status-label status) label))
      (should (eq (hermes-dashboard--status-face status) face)))))

;;; Group: provider-onboarding auth gate

(ert-deftest hermes-dashboard-onboarding-card-bound-to-e ()
  "The onboarding action and the `e' key both reach the connect command."
  (should (eq (keymap-lookup hermes-dashboard-mode-map "e")
              #'hermes-onboarding-connect-provider))
  (should (eq (plist-get (hermes-dashboard--onboarding-node) :action)
              #'hermes-onboarding-connect-provider)))

(ert-deftest hermes-dashboard-action-nodes-gate-on-onboarding-flag ()
  "The onboarding node appears only when the gateway lacks credentials."
  (with-temp-buffer
    (setq hermes-dashboard--needs-onboarding t)
    (should (cl-find "action:onboarding" (hermes-dashboard--action-nodes)
                     :key (lambda (n) (plist-get n :id)) :test #'equal))
    (setq hermes-dashboard--needs-onboarding nil)
    (should-not (cl-find "action:onboarding" (hermes-dashboard--action-nodes)
                         :key (lambda (n) (plist-get n :id)) :test #'equal))))

(ert-deftest hermes-dashboard-check-auth-surfaces-onboarding-when-unconfigured ()
  "An `ok' nil runtime check flags onboarding and adds the card."
  (cl-letf (((symbol-function 'hermes-browser--existing-client)
             (lambda () 'fake-client))
            ((symbol-function 'hermes-browser--with-client)
             (lambda (fn) (funcall fn 'fake-client #'ignore)))
            ((symbol-function 'hermes-dashboard-transport-setup-runtime-check)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve) '((error . "no provider"))))))
    (with-temp-buffer
      (hermes-dashboard-mode)
      (hermes-dashboard--check-auth)
      (should hermes-dashboard--needs-onboarding)
      (should (cl-find "action:onboarding" (hermes-dashboard--action-nodes)
                       :key (lambda (n) (plist-get n :id)) :test #'equal)))))

(ert-deftest hermes-dashboard-check-auth-skips-card-when-authed ()
  "An `ok' t runtime check leaves the onboarding card off."
  (cl-letf (((symbol-function 'hermes-browser--existing-client)
             (lambda () 'fake-client))
            ((symbol-function 'hermes-browser--with-client)
             (lambda (fn) (funcall fn 'fake-client #'ignore)))
            ((symbol-function 'hermes-dashboard-transport-setup-runtime-check)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve)
                        '((ok . t) (provider . "openai"))))))
    (with-temp-buffer
      (hermes-dashboard-mode)
      (hermes-dashboard--check-auth)
      (should-not hermes-dashboard--needs-onboarding))))

(provide 'hermes-ui-tests)
;;; hermes-ui-tests.el ends here
