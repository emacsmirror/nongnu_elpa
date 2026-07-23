;;; hermes-ui-tests.el --- dashboard UI tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `M-x hermes' EWOC dashboard in hermes.el: buffer/popup
;; setup, action and chat-list rendering, status classification, and the
;; provider-onboarding auth gate.  Transport tests live in
;; `hermes-dashboard-tests'.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-close-stops-local-services-and-kills-hermes-buffers ()
  "Closing Hermes tears down local state without killing unrelated buffers."
  (let ((chat (generate-new-buffer " *hermes-close-chat*"))
        (dashboard (generate-new-buffer " *hermes-close-dashboard*"))
        (kanban (generate-new-buffer " *hermes-close-kanban*"))
        (unrelated (generate-new-buffer " *hermes-close-unrelated*"))
        (hermes-dashboard-stale-refresh-interval nil)
        tail
        calls)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (hermes-chat-mode)
            (add-hook 'hermes-chat-cleanup-functions
                      (lambda () (push 'chat-cleanup calls)) nil t))
          (with-current-buffer dashboard
            (hermes-dashboard-mode))
          (cl-letf (((symbol-function 'hermes-kanban--events-connect) #'ignore)
                    ((symbol-function 'websocket-close)
                     (lambda (socket) (push socket calls))))
            (with-current-buffer kanban
              (hermes-kanban-mode)
              (setq hermes-kanban--slug "tests"
                    hermes-kanban--latest-event-id 7)
              (hermes-kanban-toggle-live)
              (setq tail hermes-kanban--events-tail)
              (setf (hermes-kanban--events-tail-socket tail) 'kanban-socket))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'hermes-capabilities-stop)
                     (lambda () (push 'capabilities calls)))
                    ((symbol-function 'hermes-exec-stop)
                     (lambda () (push 'exec calls)))
                    ((symbol-function 'hermes-dashboard-transport-stop-all)
                     (lambda (&optional _message)
                       (push 'transport calls)
                       2)))
              (hermes-close)))
          (should-not (buffer-live-p chat))
          (should-not (buffer-live-p dashboard))
          (should-not (buffer-live-p kanban))
          (should (buffer-live-p unrelated))
          (should-not (hermes-kanban--events-tail-active tail))
          (should-not (hermes-kanban--events-tail-socket tail))
          (should (equal (sort calls
                               (lambda (left right)
                                 (string< (symbol-name left)
                                          (symbol-name right))))
                         '(capabilities chat-cleanup exec kanban-socket
                           transport))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            (list chat dashboard kanban unrelated)))))

(ert-deftest hermes-close-cancel-preserves-local-state ()
  "Declining the close confirmation leaves buffers and transports alone."
  (let ((chat (generate-new-buffer " *hermes-close-cancel*"))
        stopped)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (hermes-chat-mode))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                    ((symbol-function 'hermes-dashboard-transport-stop-all)
                     (lambda (&rest _) (setq stopped t))))
            (hermes-close))
          (should (buffer-live-p chat))
          (should-not stopped))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest hermes-close-stops-pending-transient-browser-client ()
  "Closing Hermes stops a browser client whose request has not settled."
  (let* ((buffer (generate-new-buffer " *hermes-close-browser*"))
         (client (make-hermes-dashboard-transport-client
                  :process 'browser-process :websocket 'browser-socket))
         (pending (hermes--promise-make))
         (hermes-browser--transient-clients nil)
         (hermes-dashboard-stale-refresh-interval nil)
         closed
         deleted)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                   (lambda (&rest _) client))
                  ((symbol-function 'websocket-close)
                   (lambda (socket) (setq closed socket)))
                  ((symbol-function 'delete-process)
                   (lambda (process) (setq deleted process)))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'hermes-capabilities-stop) #'ignore)
                  ((symbol-function 'hermes-exec-stop) #'ignore)
                  ((symbol-function 'hermes-dashboard-transport-stop-all)
                   (lambda (&rest _) 0)))
          (with-current-buffer buffer
            (hermes-dashboard-mode)
            (hermes-browser--run-on-client (lambda (_client) pending)))
          (should (memq client hermes-browser--transient-clients))
          (hermes-close)
          (should-not (buffer-live-p buffer))
          (should (eq closed 'browser-socket))
          (should (eq deleted 'browser-process))
          (should-not (hermes-dashboard-transport-client-websocket client))
          (should-not (hermes-dashboard-transport-client-process client))
          (should-not hermes-browser--transient-clients))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
              (should (string-match-p "Press c to open Chat" text))
              (should-not (string-match-p "N for a new session" text))))
        (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-chat-action-is-keymap-popup-binding ()
  (should (eq (keymap-lookup hermes-dashboard-mode-map "c") #'hermes-chat))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "m") #'hermes-dashboard-switch-model))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "X") #'hermes-list-mcp))
  (should-not (keymap-lookup hermes-dashboard-mode-map "T"))
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
  "An `ok' t runtime check clears a stale onboarding card."
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
      (setq hermes-dashboard--needs-onboarding t)
      (hermes-dashboard--check-auth)
      (should-not hermes-dashboard--needs-onboarding))))

(ert-deftest hermes-dashboard-check-auth-keeps-newest-result ()
  "An older credential check cannot replace a newer result."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _)
                 (setq requests (1+ requests))
                 (if (= requests 1) first second)))
              ((symbol-function 'hermes-dashboard-refresh) #'ignore))
      (with-temp-buffer
        (hermes-dashboard-mode)
        (hermes-dashboard--check-auth)
        (hermes-dashboard--check-auth)
        (hermes--promise-resolve second '((ok . t)))
        (hermes--promise-resolve first '((ok . :false)))
        (should-not hermes-dashboard--needs-onboarding)))))

(ert-deftest hermes-dashboard-provider-connect-invalidates-auth-check ()
  "Saving credentials invalidates a credential check already in flight."
  (let ((promise (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-dashboard-refresh) #'ignore))
      (let ((buffer (get-buffer-create hermes-dashboard-buffer-name)))
        (unwind-protect
            (with-current-buffer buffer
              (hermes-dashboard-mode)
              (hermes-dashboard--check-auth)
              (hermes-dashboard--provider-connected)
              (hermes--promise-resolve promise '((ok . :false)))
              (should-not hermes-dashboard--needs-onboarding))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'hermes-ui-tests)
;;; hermes-ui-tests.el ends here
