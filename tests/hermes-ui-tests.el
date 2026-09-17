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
  (let ((chat (hermes-buffer--get " *hermes-close-chat*" #'hermes-chat-mode))
        (dashboard (let ((hermes-dashboard-stale-refresh-interval nil))
                     (hermes-buffer--get " *hermes-close-dashboard*" #'hermes-dashboard-mode)))
        (kanban (hermes-buffer--get " *hermes-close-kanban*" #'hermes-kanban-mode))
        (unrelated (generate-new-buffer " *hermes-close-unrelated*"))
        (hermes-dashboard-stale-refresh-interval nil)
        tail
        calls)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (add-hook 'hermes-chat-cleanup-functions
                      (lambda () (push 'chat-cleanup calls)) nil t))
          (cl-letf (((symbol-function 'hermes-kanban--events-connect) #'ignore)
                    ((symbol-function 'websocket-close)
                     (lambda (socket) (push socket calls))))
            (with-current-buffer kanban
              (setq hermes-kanban--slug "tests"
                    hermes-kanban--latest-event-id 7)
              (hermes-kanban-toggle-live)
              (setq tail hermes-kanban--events-tail)
              (setf (hermes-kanban--events-tail-socket tail) 'kanban-socket))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'hermes-browser-stop-all-transient-clients)
                     (lambda (&rest _)
                       (ert-fail "Hermes close used the retired transient catalog")))
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
  (let ((chat (hermes-buffer--get " *hermes-close-cancel*" #'hermes-chat-mode))
        stopped)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                    ((symbol-function 'hermes-dashboard-transport-stop-all)
                     (lambda (&rest _) (setq stopped t))))
            (hermes-close))
          (should (buffer-live-p chat))
          (should-not stopped))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest hermes-close-stops-pending-browser-client ()
  "Closing Hermes stops a browser client whose request has not settled."
  (let* ((buffer (let ((hermes-dashboard-stale-refresh-interval nil))
                   (hermes-buffer--get " *hermes-close-browser*" #'hermes-dashboard-mode)))
         (client (make-hermes-dashboard-transport-client
                  :process 'browser-process :websocket 'browser-socket))
         (pending (hermes--promise-make))
         (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
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
                  ((symbol-function 'hermes-exec-stop) #'ignore))
          (with-current-buffer buffer
            (hermes-browser--run-on-client (lambda (_client) pending)))
          (should (= (hash-table-count hermes-dashboard-transport--clients) 1))
          (hermes-close)
          (should-not (buffer-live-p buffer))
          (should (eq closed 'browser-socket))
          (should (eq deleted 'browser-process))
          (should-not (hermes-dashboard-transport-client-websocket client))
          (should-not (hermes-dashboard-transport-client-process client))
          (should (= (hash-table-count hermes-dashboard-transport--clients) 0)))
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
  (dolist (binding '(("B" . hermes-list-pairing)
                     ("W" . hermes-list-webhooks)
                     ("J" . hermes-list-plugins)))
    (should (eq (keymap-lookup hermes-dashboard-mode-map (car binding))
                (cdr binding)))
    (should (commandp (cdr binding))))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "T") #'hermes-list-projects))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "g") #'hermes-dashboard-refresh))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "n") #'hermes-dashboard-next))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "p") #'hermes-dashboard-previous))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "RET") #'hermes-dashboard-open))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "i") #'hermes-dashboard-interrupt))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "s") #'hermes-dashboard-steer))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "a") #'hermes-dashboard-respond))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "?") #'hermes-dashboard-popup))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "h")
              #'hermes-dashboard-mode-map-popup))
  (let* ((rows (keymap-popup--meta hermes-dashboard-mode-map 'descriptions))
         (groups (apply #'append rows))
         (group-names (mapcar (lambda (group) (plist-get group :name)) groups))
         (entries (apply #'append
                         (mapcar (lambda (group)
                                   (plist-get group :entries))
                                 groups))))
    (should (equal group-names
                   '("Navigate" "Chats" "Tools" "View")))
    (dolist (group groups)
      (should (<= (length (plist-get group :entries)) 6)))
    (dolist (key '("c" "v" "b" "z" "!" "g"))
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

(ert-deftest hermes-dashboard-status-entry-does-not-intern-unknown-strings ()
  (let* ((normalized "hermes-unknown-status-from-test")
         (status (replace-regexp-in-string "-" " " normalized)))
    (should-not (intern-soft normalized))
    (should-not (hermes-dashboard--status-entry status))
    (should-not (intern-soft normalized))
    (should (equal (car (hermes-dashboard--status-entry "input requested"))
                   "input-requested"))
    (should (equal (car (hermes-dashboard--status-entry "In_Progress"))
                   "in-progress"))))

(ert-deftest hermes-dashboard-chat-detail-identifies-instance-when-ambiguous ()
  "Aggregate chat cards identify the instance owning each chat."
  (let ((hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test"))))
    (should (member "instance remote"
                    (hermes-dashboard--format-chat-detail
                     '(:instance "remote" :status ready))))))

(ert-deftest hermes-dashboard-groups-typed-instance-by-stable-id ()
  "A display rename does not orphan a typed instance's existing chats."
  (let* ((hermes-instances
          '((:id "local" :name "Local" :url "http://127.0.0.1:9119")
            (:id "remote" :name "Remote renamed"
             :url "https://hermes.example.test")))
         (nodes '((:id "chat:remote" :instance-id "remote"
                   :instance "Old remote name")))
         (grouped (hermes-dashboard--group-chat-nodes nodes)))
    (should (equal (plist-get (car grouped) :instance-heading)
                   "Remote renamed"))))

(ert-deftest hermes-dashboard-groups-chats-by-configured-instance ()
  "Multiple instances render as stable dashboard sections."
  (let (local-buffer local-name remote-buffer remote-name)
    (hermes-test-with-chat-buffer
     (setq local-buffer (current-buffer)
           local-name (buffer-name)
           hermes-instance '("local" . "http://127.0.0.1:9119"))
     (hermes-test-with-chat-buffer
      (setq remote-buffer (current-buffer)
            remote-name (buffer-name)
            hermes-instance '("remote" . "https://hermes.example.test"))
      (let ((hermes-instances '(("local" . "http://127.0.0.1:9119")
                                ("remote" . "https://hermes.example.test"))))
        (hermes-test-with-dashboard-buffer
         (should (equal (hermes-dashboard--current-ids)
                        (list "action:chat"
                              (format "chat:%s" local-name)
                              (format "chat:%s" remote-name))))
         (let ((text (buffer-substring-no-properties (point-min) (point-max))))
           (should (< (string-match "local" text)
                      (string-match (regexp-quote local-name) text)))
           (should (< (string-match "remote" text)
                      (string-match (regexp-quote remote-name) text)))
           (should-not (string-match-p "instance local" text))
           (should-not (string-match-p "instance remote" text)))
         (should (eq (plist-get
                      (hermes-test--dashboard-node-data
                       (format "chat:%s" local-name))
                      :buffer)
                     local-buffer))
         (should (eq (plist-get
                      (hermes-test--dashboard-node-data
                       (format "chat:%s" remote-name))
                      :buffer)
                     remote-buffer))))))))

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
    (let ((buffer (hermes-buffer--get hermes-dashboard-buffer-name
                                      #'hermes-dashboard-mode)))
      (unwind-protect
          (with-current-buffer buffer
            (hermes-dashboard--check-auth)
            (should hermes-dashboard--needs-onboarding)
            (should (cl-find "action:onboarding" (hermes-dashboard--action-nodes)
                             :key (lambda (n) (plist-get n :id)) :test #'equal)))
        (kill-buffer buffer)))))

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
    (let ((buffer (hermes-buffer--get hermes-dashboard-buffer-name
                                      #'hermes-dashboard-mode)))
      (unwind-protect
          (with-current-buffer buffer
            (setq hermes-dashboard--needs-onboarding t)
            (hermes-dashboard--check-auth)
            (should-not hermes-dashboard--needs-onboarding))
        (kill-buffer buffer)))))

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
      (let ((buffer (hermes-buffer--get hermes-dashboard-buffer-name
                                        #'hermes-dashboard-mode)))
        (unwind-protect
            (with-current-buffer buffer
              (hermes-dashboard--check-auth)
              (hermes-dashboard--check-auth)
              (hermes--promise-resolve second '((ok . t)))
              (hermes--promise-resolve first '((ok . :false)))
              (should-not hermes-dashboard--needs-onboarding))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-warm-profile-cache-skips-ambiguous-instance ()
  "The aggregate dashboard does not choose an instance for passive warming."
  (let ((hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test")))
        (hermes-instance nil)
        fetched)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport-cached-profile-list)
               (lambda () nil))
              ((symbol-function
                'hermes-dashboard-transport-profile-list-async)
               (lambda (&rest _)
                 (setq fetched t)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected instance prompt"))))
      (hermes-dashboard--warm-profile-cache)
      (should-not fetched))))

(ert-deftest hermes-dashboard-header-line-uses-semantic-faces ()
  "Dashboard header title, count, and hints use dashboard faces."
  (hermes-test-with-dashboard-buffer
   (let ((header (hermes-dashboard--header-line)))
     (should (equal (substring-no-properties header)
                    " Hermes Dashboard  |  0 live chats  |  g refresh  ? help "))
     (dolist (case '(("Hermes Dashboard" . hermes-dashboard-heading)
                     ("0 live chats" . hermes-dashboard-title)
                     ("g refresh  ? help" . hermes-dashboard-muted)))
       (let ((position (string-match-p (regexp-quote (car case)) header)))
         (should position)
         (should (eq (get-text-property position 'face header)
                     (cdr case))))))))

(ert-deftest hermes-dashboard-provider-connect-invalidates-auth-check ()
  "Saving credentials forces a newest-owned credential check."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () nil))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _)
                 (setq requests (1+ requests))
                 (if (= requests 1) first second)))
              ((symbol-function 'hermes-dashboard-refresh) #'ignore))
      (let ((buffer (hermes-buffer--get hermes-dashboard-buffer-name #'hermes-dashboard-mode)))
        (unwind-protect
            (with-current-buffer buffer
              (setq hermes-dashboard--needs-onboarding t)
              (hermes-dashboard--check-auth t)
              (hermes-dashboard--provider-auth-changed)
              (hermes--promise-resolve second '((ok . t)))
              (hermes--promise-resolve first '((ok . :false)))
              (should (= requests 2))
              (should-not hermes-dashboard--needs-onboarding))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))


(ert-deftest hermes-dashboard-popup-uses-deliberate-two-column-rows ()
  "The real popup fits narrow and wide displays and preserves direct actions."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((hermes-dashboard-stale-refresh-interval nil)
            (keymap-popup-backend #'keymap-popup-backend-side-window)
            (keymap-popup--buffer-name " *dashboard layout test*"))
        (hermes-dashboard-mode)
        (unwind-protect
            ;; Side-window fringes leave body widths 52, 118, 48 and 47.
            (dolist (width '(54 120 50 49))
              (let ((old-width (frame-width))
                    (owner (current-buffer)))
                (unwind-protect
                    (progn
                      (set-frame-width (selected-frame) width)
                      (execute-kbd-macro (kbd "?"))
                      (should (get-buffer-window keymap-popup--buffer-name))
                      (with-current-buffer keymap-popup--buffer-name
                        (let ((text (buffer-string)))
                          (should (<= (apply #'max (mapcar #'string-width
                                                          (split-string text "\n")))
                                      (1- (window-body-width (get-buffer-window (current-buffer))))))
                          (dolist (pair '("Navigate.*Chats"
                                          "Tools.*View"))
                            (should (string-match-p pair text)))
                          (should-not (string-match-p "Navigate.*Selected chat" text))
                          (dolist (row (keymap-popup--meta
                                        hermes-dashboard-mode-map 'descriptions))
                            (dolist (group row)
                              (dolist (entry (plist-get group :entries))
                                (should (string-match-p
                                         (concat (regexp-quote (plist-get entry :key))
                                                 " +"
                                                 (regexp-quote
                                                  (plist-get entry :description)))
                                         text)))))))
                      (condition-case nil
                          (execute-kbd-macro (kbd "C-g"))
                        (quit nil))
                      (should-not (get-buffer keymap-popup--buffer-name))
                      (should (eq (current-buffer) owner))
                      (should (eq major-mode 'hermes-dashboard-mode)))
                  (keymap-popup-dismiss)
                  (set-frame-width (selected-frame) old-width))))
          (keymap-popup-dismiss))))))

(ert-deftest hermes-dashboard-cards-bound-activity-with-full-source-retained ()
  "Long tool arguments cannot dominate cards or replace dispatch identity."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((tool (concat "terminal: " (make-string 500 ?x) "\nlast argument"))
             (node (list :id "chat: exact bytes " :kind 'chat :title "Exact  title λ"
                         :status 'running :activity tool :active-tools (list tool tool)))
             (original (copy-tree node)))
        (hermes-dashboard--print-chat-node node)
        (should (< (buffer-size) 400))
        (should (string-match-p "Exact  title λ.*Running" (buffer-string)))
        (should (string-match-p "2 active tools" (buffer-string)))
        (should-not (string-match-p "last argument" (buffer-string)))
        (should (equal node original))
        (goto-char (point-min))
        (should (equal (get-text-property (point) 'hermes-dashboard-node-id)
                       "chat: exact bytes "))
        (should (string-match-p "last argument"
                                (get-text-property (point) 'help-echo)))))))

(ert-deftest hermes-management-plain-maps-have-contextual-help ()
  "Annotations render original bindings and keep dismissal non-destructive."
  (save-window-excursion
    (dolist (mode '(hermes-config-mode hermes-inventory-mode
                    hermes-memory-status-mode hermes-plugins-mode))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (funcall mode)
        (let ((keymap-popup-backend #'keymap-popup-backend-side-window)
              (keymap-popup--buffer-name " *management help test*"))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "?"))
                (should (get-buffer-window keymap-popup--buffer-name))
                (with-current-buffer keymap-popup--buffer-name
                  (should (string-match-p "Refresh" (buffer-string)))
                  (should (string-match-p "Quit view" (buffer-string))))
                (execute-kbd-macro (kbd "C-g"))
                (should-not (get-buffer keymap-popup--buffer-name))
                (should (eq major-mode mode))
                (let (quit-called)
                  (cl-letf (((symbol-function 'quit-window)
                             (lambda () (interactive) (setq quit-called t))))
                    (execute-kbd-macro (kbd "? q"))
                    (should quit-called)
                    (setq quit-called nil)
                    (execute-kbd-macro (kbd "q")))
                  (should quit-called)
                  (should-not (get-buffer keymap-popup--buffer-name))))
            (keymap-popup-dismiss)))))))


(ert-deftest hermes-management-annotation-keeps-remapped-keys-and-guards ()
  "Popup annotations follow existing command bindings and refuse busy edits."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (hermes-config-mode)
      (let ((map (copy-keymap hermes-config-mode-map))
            (keymap-popup-backend #'keymap-popup-backend-side-window)
            (keymap-popup--buffer-name " *config remap test*") invoked)
        (keymap-unset map "e")
        (keymap-unset map "RET")
        (keymap-set map "v" #'hermes-config-edit)
        (use-local-map map)
        (setq hermes-config--mutation-in-flight t)
        (cl-letf (((symbol-function 'hermes-config-edit)
                   (lambda () (interactive) (setq invoked t))))
          (unwind-protect
              (progn
                (keymap-popup map)
                (with-current-buffer keymap-popup--buffer-name
                  (should (string-match-p "v.*Edit value" (buffer-string))))
                (execute-kbd-macro (kbd "v"))
                (should-not invoked)
                (should (get-buffer keymap-popup--buffer-name))
                (setq hermes-config--mutation-in-flight nil)
                (execute-kbd-macro (kbd "v"))
                (should invoked))
            (keymap-popup-dismiss)))))))


(ert-deftest hermes-dashboard-popup-preserves-direct-and-child-actions ()
  "Existing direct shortcuts and the new goal menus reach the same commands."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((hermes-dashboard-stale-refresh-interval nil)
            (keymap-popup-backend #'keymap-popup-backend-side-window))
        (hermes-dashboard-mode)
        (dolist (case '(("v" "i" hermes-dashboard-interrupt)
                        ("v" "s" hermes-dashboard-steer)
                        ("v" "a" hermes-dashboard-respond)
                        ("v" "m" hermes-dashboard-switch-model)
                        ("v" "d" hermes-dashboard-disconnect)
                        ("b" "I" hermes-list-inventory)
                        ("b" "R" hermes-list-rollbacks)
                        ("b" "A" hermes-list-subagents)
                        ("b" "C" hermes-list-crons)
                        ("b" "O" hermes-files)
                        ("b" "K" hermes-list-kanban)
                        ("b" "X" hermes-list-mcp)
                        ("b" "T" hermes-list-projects)
                        ("z" "F" hermes-list-profiles)
                        ("z" "M" hermes-list-messaging-platforms)
                        ("z" "Z" hermes-config)
                        ("z" "J" hermes-list-plugins)
                        ("z" "e" hermes-onboarding-connect-provider)
                        ("z" "o" hermes-onboarding-oauth-connect)
                        ("z" "B" hermes-list-pairing)
                        ("z" "W" hermes-list-webhooks)
                        ("!" "G" hermes-system-status)
                        ("!" "L" hermes-system-logs)))
          (let ((command (nth 2 case)) called)
            (should (eq (key-binding (kbd (cadr case))) command))
            (cl-letf (((symbol-function command)
                       (lambda () (interactive) (setq called t)))
                      ((symbol-function 'hermes-dashboard--chat-unavailable-p)
                       (lambda (&optional _) nil)))
              (unwind-protect
                  (progn
                    (execute-kbd-macro (kbd (cadr case)))
                    (should called)
                    (setq called nil)
                    (execute-kbd-macro (kbd (concat "? " (car case) " " (cadr case))))
                    (should called))
                (keymap-popup-dismiss)))))))))

(ert-deftest hermes-dashboard-popup-back-repeat-and-rebinding ()
  "Navigation stays open; native back, dismissal and user rebindings survive."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((hermes-dashboard-stale-refresh-interval nil)
            (keymap-popup-backend #'keymap-popup-backend-side-window))
        (hermes-dashboard-mode)
        (let ((map (copy-keymap hermes-dashboard-mode-map)) (steps 0))
          (keymap-unset map "n")
          (keymap-set map "N" #'hermes-dashboard-next)
          (use-local-map map)
          (cl-letf (((symbol-function 'hermes-dashboard-next)
                     (lambda () (interactive) (cl-incf steps))))
            (unwind-protect
                (progn
                  (keymap-popup map)
                  (execute-kbd-macro (kbd "N N b q"))
                  (should (= steps 2))
                  (with-current-buffer keymap-popup--buffer-name
                    (should (string-match-p "Chats" (buffer-string)))
                    (should-not (string-match-p "mouse-1" (buffer-string))))
                  (condition-case nil (execute-kbd-macro (kbd "C-g")) (quit nil))
                  (should-not (get-buffer keymap-popup--buffer-name)))
              (keymap-popup-dismiss))))))))

(ert-deftest hermes-management-popup-layout-and-cached-selection ()
  "Board and MCP menus use two columns and identify their cached selection."
  (save-window-excursion
    (let ((width (frame-width))
          (keymap-popup-backend #'keymap-popup-backend-side-window))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 80)
            (dolist (case '((hermes-mcp-mode hermes-mcp-mode-map "server-one" 5)
                            (hermes-kanban-mode hermes-kanban-mode-map "task-one" 4)))
              (with-temp-buffer
                (switch-to-buffer (current-buffer))
                (funcall (car case))
                (setq hermes-kanban--name "Test board")
                (let ((map (symbol-value (cadr case))) (id (nth 2 case)))
                  (dolist (row (keymap-popup--meta map 'descriptions))
                    (should (= (length row) 2)))
                  (setq tabulated-list-entries
                        (list (list id (make-vector (nth 3 case) "cached"))))
                  (tabulated-list-print)
                  (goto-char (point-min))
                  (should (string-match-p
                           id (funcall (keymap-popup--meta map 'description))))
                  (unwind-protect
                      (progn
                        (keymap-popup map)
                        (with-current-buffer keymap-popup--buffer-name
                          (should (<= (apply #'max (mapcar #'string-width
                                                          (split-string (buffer-string) "\n")))
                                      (1- (window-body-width
                                           (get-buffer-window (current-buffer))))))))
                    (keymap-popup-dismiss))))))
        (set-frame-width (selected-frame) width)))))

(ert-deftest hermes-management-popup-selection-and-busy-guards ()
  "Missing selections and active MCP/plugin operations are visibly unavailable."
  (save-window-excursion
    (dolist (case '((hermes-plugins-mode hermes-plugins-mode-map
                    hermes-plugins-enable "e" hermes-plugins--busy)
                   (hermes-mcp-mode hermes-mcp-mode-map
                    hermes-mcp-toggle "e" hermes-mcp--operation)
                   (hermes-kanban-mode hermes-kanban-mode-map
                    hermes-kanban-edit "e" nil)))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (funcall (car case))
        (let ((map (symbol-value (cadr case))) (command (nth 2 case)) called)
          (cl-letf (((symbol-function command)
                     (lambda () (interactive) (setq called t))))
            (unwind-protect
                (progn
                  (keymap-popup map)
                  (execute-kbd-macro (kbd (nth 3 case)))
                  (should-not called)
                  (should (get-buffer keymap-popup--buffer-name))
                  (keymap-popup-dismiss)
                  (setq tabulated-list-entries
                        (list (list "selected" (make-vector (length tabulated-list-format)
                                                          "cached"))))
                  (tabulated-list-print)
                  (goto-char (point-min))
                  (when (nth 4 case) (set (nth 4 case) t))
                  (keymap-popup map)
                  (execute-kbd-macro (kbd (nth 3 case)))
                  (if (nth 4 case) (should-not called) (should called))
                  (when (nth 4 case)
                    (set (nth 4 case) nil)
                    ;; No description fetch or re-open is needed after settlement.
                    (execute-kbd-macro (kbd (nth 3 case)))
                    (should called)))
              (when (nth 4 case) (set (nth 4 case) nil))
              (keymap-popup-dismiss))))))))

(ert-deftest hermes-dashboard-popup-selected-owner-title-and-availability ()
  "A selected chat's cached identity and busy state never come from another chat."
  (hermes-test-with-chat-buffer
   (let ((first (current-buffer)))
     (hermes-test-with-chat-buffer
      (let ((second (current-buffer)))
        (setq hermes-chat--dashboard-running-p t)
        (hermes-test-with-dashboard-buffer
         (hermes-dashboard--sync-ewoc
          (list (list :id "first" :kind 'chat :title "First chat" :buffer first)
                (list :id "second" :kind 'chat :title "Second chat" :buffer second)))
         (dolist (case '(("first" "First chat" nil) ("second" "Second chat" t)))
           (goto-char (ewoc-location (gethash (car case) hermes-dashboard--nodes)))
           (should (string-match-p (cadr case) (hermes-dashboard--popup-title)))
           (should (eq (not (null (hermes-dashboard--chat-unavailable-p
                                  #'hermes-chat--active-turn-p)))
                       (nth 2 case))))))))))

(ert-deftest hermes-dashboard-reader-windows-survive-membership ()
  "Adding, removing and reordering cards preserves both window anchors."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((hermes-dashboard-stale-refresh-interval nil))
        (hermes-dashboard-mode))
      (hermes-dashboard--ensure-ewoc)
      (let ((nodes (mapcar (lambda (n)
                            (list :id (format "card-%02d" n) :kind 'empty
                                  :title (format "Card %02d" n)
                                  :subtitle "Second line"))
                          (number-sequence 1 60))))
        (hermes-dashboard--sync-ewoc nodes)
        (let* ((first (selected-window)) (second (split-window-right))
               (one (ewoc-location (gethash "card-20" hermes-dashboard--nodes)))
               (two (ewoc-location (gethash "card-40" hermes-dashboard--nodes)))
               (observe (lambda (window)
                          (mapcar (lambda (pos)
                                    (save-excursion
                                      (goto-char pos)
                                      (list (get-text-property pos 'hermes-dashboard-node-id)
                                            (buffer-substring (line-beginning-position) pos))))
                                  (list (window-start window) (window-point window))))))
          (set-window-start first one t) (set-window-point first (+ one 3))
          (set-window-start second two t) (set-window-point second (+ two 4))
          (redisplay t)
          (let ((before (mapcar observe (list first second))))
            (dolist (new (list (append nodes (list '(:id "added" :kind empty :title "Added")))
                              (cdr nodes) (reverse nodes)))
              (hermes-dashboard--sync-ewoc new)
              (redisplay t)
              (should (equal (mapcar observe (list first second)) before))
              (should (eq first (selected-window))))
            (hermes-dashboard--sync-ewoc
             (seq-remove (lambda (node) (equal (plist-get node :id) "card-20"))
                         (reverse nodes)))
            (redisplay t)
            (should (equal (funcall observe second) (nth 1 before)))
            (should (equal (get-text-property (window-point first)
                                              'hermes-dashboard-node-id)
                           "card-19"))))))))

(ert-deftest hermes-dashboard-stale-projection-has-one-semantic-status ()
  "Stale dot and label agree without changing the chat's running state."
  (with-temp-buffer
    (hermes-chat-mode)
    (setq hermes-chat--status-state
          (list :status 'running :updated (time-subtract (current-time) 120)))
    (dolist (stale '(t nil))
      (unless stale
        (setf (plist-get hermes-chat--status-state :updated) (current-time)))
      (let* ((node (hermes-dashboard--chat-node (current-buffer)))
             (label (if stale "Stale" "Running"))
             (face (if stale 'hermes-dashboard-status-stale
                     'hermes-dashboard-status-running)))
        (should (eq (plist-get hermes-chat--status-state :status) 'running))
        (should (= 1 (cl-loop for (key _value) on node by #'cddr
                             count (eq key :status))))
        (with-temp-buffer
          (hermes-dashboard--print-chat-node node)
          (should (eq (get-text-property (point-min) 'face) face))
          (goto-char (point-min))
          (search-forward label)
          (should (eq (get-text-property (1- (point)) 'face) face)))))))

;;; Native command completion

(ert-deftest hermes-ui-command-completion-mode-contexts ()
  "Native completion follows view modes, including derived and shared modes."
  (require 'hermes-preview)
  (require 'hermes-exec)
  (require 'hermes-tool-setup)
  (let ((cases
         '((hermes-chat-send hermes-chat-mode)
           (hermes-chat-set-reasoning hermes-chat-mode)
           (hermes-chat-attach-image-file hermes-chat-mode)
           (hermes-chat-show-todos hermes-chat-mode)
           (hermes-chat-queue-panel-edit hermes-chat-queue-panel-mode)
           (hermes-chat-image-recovery-restore hermes-chat-image-recovery-mode)
           (hermes-chat-todos-next hermes-chat-todos-mode)
           (hermes-reconnect hermes-chat-mode)
           (hermes-dashboard-restart hermes-chat-mode)
           (hermes-dashboard-next hermes-dashboard-mode)
           (hermes-exec-approve hermes-exec-approval-mode)
           (hermes-config-edit hermes-config-mode)
           (hermes-cron-edit hermes-cron-mode)
           (hermes-files-directory hermes-files-mode)
           (hermes-file-save hermes-file-view-mode hermes-preview-mode)
           (hermes-files-cancel hermes-files-mode hermes-file-view-mode hermes-preview-mode)
           (hermes-preview-retry hermes-preview-mode)
           (hermes-inventory-toggle hermes-inventory-mode)
           (hermes-memory-reset hermes-memory-status-mode)
           (hermes-kanban-switch-board hermes-kanban-boards-mode)
           (hermes-kanban-show hermes-kanban-mode hermes-kanban-diagnostics-mode)
           (hermes-kanban-set-status hermes-kanban-mode hermes-kanban-diagnostics-mode)
           (hermes-kanban-comment hermes-kanban-mode hermes-kanban-task-mode hermes-kanban-diagnostics-mode)
           (hermes-kanban-toggle-live hermes-kanban-mode)
           (hermes-kanban-log-next-hunk hermes-kanban-log-mode hermes-work-log-mode)
           (hermes-mcp-add hermes-mcp-mode)
           (hermes-messaging-set-env hermes-messaging-mode)
           (hermes-onboarding-oauth-poll hermes-onboarding-oauth-mode)
           (hermes-onboarding-provider-account-act hermes-provider-accounts-mode)
           (hermes-plugins-install hermes-plugins-mode)
           (hermes-profiles-set-model hermes-profiles-mode)
           (hermes-profiles-soul-save hermes-profiles-soul-mode)
           (hermes-projects-create hermes-projects-mode hermes-project-detail-mode)
           (hermes-project-sessions-open hermes-project-sessions-mode)
           (hermes-rollback-restore hermes-rollback-mode)
           (hermes-sessions-search hermes-sessions-mode)
           (hermes-sessions-open hermes-sessions-mode hermes-session-detail-mode)
           (hermes-subagents-interrupt hermes-subagents-mode)
           (hermes-chat-work hermes-chat-mode hermes-work-mode)
           (hermes-work-refresh hermes-work-mode)
           (hermes-work-log-refresh hermes-work-log-mode)
           (hermes-tool-setup-refresh hermes-tool-setup-mode)
           (hermes-admin-quit hermes-pairing-mode hermes-webhooks-mode)
           (hermes-webhooks-create hermes-webhooks-mode))))
    (with-temp-buffer
      (let ((origin (current-buffer)))
        ;; Check the explicit BUFFER argument rather than the selected buffer.
        (with-temp-buffer
          (dolist (case cases)
            (let ((command (car case)))
              (should (commandp command))
              (dolist (mode (append '(fundamental-mode text-mode) (cdr case)))
                (with-current-buffer origin (setq major-mode mode))
                (should (eq (not (null (command-completion-default-include-p command origin)))
                            (not (null (memq mode (cdr case)))))))))))))
  (with-temp-buffer
    (delay-mode-hooks (hermes-chat-mode))
    (should (command-completion-default-include-p 'hermes-chat-send (current-buffer)))
    (let ((parent (get 'hermes-ui-test-chat-mode 'derived-mode-parent)))
      (unwind-protect
          (progn
            (put 'hermes-ui-test-chat-mode 'derived-mode-parent 'hermes-chat-mode)
            (setq major-mode 'hermes-ui-test-chat-mode)
            (should (command-completion-default-include-p
                     'hermes-chat-send (current-buffer))))
        (put 'hermes-ui-test-chat-mode 'derived-mode-parent parent)))))

(ert-deftest hermes-ui-command-completion-kanban-diagnostics-status ()
  "Status changes remain discoverable and usable from real diagnostics rows."
  (with-temp-buffer
    (hermes-kanban-diagnostics-mode)
    (setq hermes-kanban--slug "test-board"
          hermes-kanban--name "Test"
          tabulated-list-entries
          (hermes-kanban--diagnostic-rows
           '(((task_id . "task-17") (task_title . "Task")
              (diagnostics . [((severity . "warning") (title . "Needs action"))])))))
    (tabulated-list-print)
    (goto-char (point-min))
    (let (request rendered)
      (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () nil))
                ((symbol-function 'completing-read) (lambda (&rest _) "blocked"))
                ((symbol-function 'hermes-kanban--api)
                 (lambda (&rest args)
                   (setq request args)
                   (hermes--promise-resolved nil)))
                ((symbol-function 'hermes-kanban--render-board)
                 (lambda (&rest args) (setq rendered args))))
        (call-interactively #'hermes-kanban-set-status))
      (should (equal request '("PATCH" "/tasks/task-17"
                              ((status . "blocked")) ((board . "test-board")))))
      (should (equal rendered '("test-board" "Test"))))
    (should (command-completion-default-include-p
             'hermes-kanban-set-status (current-buffer)))))

(ert-deftest hermes-ui-command-completion-global-entries ()
  "Opening, acquisition and setup commands remain globally discoverable."
  (require 'hermes-capabilities)
  (require 'hermes-exec)
  (require 'hermes-files)
  (require 'hermes-tool-setup)
  (with-temp-buffer
    (dolist (mode '(fundamental-mode text-mode hermes-chat-mode hermes-sessions-mode))
      (setq major-mode mode)
      (dolist (command '(hermes hermes-close hermes-chat hermes-project-chat
                        hermes-switch-to-chat hermes-chat-resume-session
                        hermes-command-palette hermes-list-sessions hermes-list-profiles
                        hermes-list-projects hermes-list-crons hermes-list-subagents
                        hermes-list-provider-accounts hermes-list-rollbacks
                        hermes-files hermes-config hermes-tool-setup
                        hermes-onboarding-connect-provider hermes-onboarding-oauth-connect
                        hermes-cron-create hermes-profiles-create
                        hermes-kanban-create-board hermes-kanban-create-task
                        hermes-kanban-create-triage-task hermes-kanban-diagnostics
                        hermes-inventory-reload-skills hermes-messaging-select-profile
                        hermes-system-status hermes-system-logs hermes-plugins-mode
                        hermes-chat-image-recovery-mode hermes-exec-approval-mode
                        hermes-project-detail-mode hermes-work-log-mode hermes-work-mode
                        hermes-exec-start hermes-exec-stop hermes-exec-trust
                        hermes-capabilities-start hermes-capabilities-stop))
        (should (commandp command))
        (should (command-completion-default-include-p command (current-buffer)))))))

(ert-deftest hermes-ui-command-completion-generated-commands ()
  "Generated popup launchers and scoped browsers carry native mode metadata."
  (with-temp-buffer
    (dolist (case '((hermes-chat-actions-map-popup hermes-chat-mode)
                    (hermes-chat-actions-map--enter-hermes-chat-images-map hermes-chat-mode)
                    (hermes-chat-jobs-map-popup hermes-chat-mode)
                    (hermes-dashboard-mode-map-popup hermes-dashboard-mode)
                    (hermes-dashboard-mode-map--enter-hermes-dash-chat-map hermes-dashboard-mode)
                    (hermes-dash-sys-map-popup hermes-dashboard-mode)
                    (hermes-sessions-mode-map-popup hermes-sessions-mode)
                    (hermes-session-detail-mode-map-popup hermes-session-detail-mode)
                    (hermes-work-mode-map-popup hermes-work-mode)
                    (hermes-rollback--list hermes-rollback-mode)
                    (hermes-list-project-sessions hermes-project-sessions-mode)))
      (should (commandp (car case)))
      (setq major-mode 'fundamental-mode)
      (should-not (command-completion-default-include-p (car case) (current-buffer)))
      (setq major-mode (cadr case))
      (should (command-completion-default-include-p (car case) (current-buffer))))))

(ert-deftest hermes-ui-command-completion-inert-context-predicates ()
  "Point and view-kind predicates read only their supplied buffer's state."
  (with-temp-buffer
    (insert "run\n")
    (goto-char (point-min))
    (let ((origin (current-buffer))
          (before (buffer-list)))
      (with-temp-buffer
        (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _) (ert-fail "Completion acquired a client")))
                  ((symbol-function 'get-buffer-create)
                   (lambda (&rest _) (ert-fail "Completion opened a buffer")))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (ert-fail "Completion prompted"))))
          (should-not (command-completion-default-include-p 'hermes-cron-show-run-log origin))
          (with-current-buffer origin
            (put-text-property (point-min) (1+ (point-min)) 'hermes-cron-run-id "run"))
          (should (command-completion-default-include-p 'hermes-cron-show-run-log origin))
          (with-current-buffer origin (forward-char))
          (should-not (command-completion-default-include-p 'hermes-cron-show-run-log origin))
          (dolist (command '(hermes-system-log-source hermes-system-log-level
                            hermes-system-log-component hermes-system-log-lines
                            hermes-system-log-auto-refresh))
            (with-current-buffer origin
              (setq major-mode 'hermes-system-mode hermes-system--path "/api/status"))
            (should-not (command-completion-default-include-p command origin))
            (with-current-buffer origin (setq hermes-system--path "/api/logs"))
            (should (command-completion-default-include-p command origin))
            (with-current-buffer origin (setq major-mode 'text-mode))
            (should-not (command-completion-default-include-p command origin)))))
      (should (equal before (buffer-list))))))

(ert-deftest hermes-ui-command-completion-native-reader-policy ()
  "The native M-x table respects origin context and the user's predicate."
  (with-temp-buffer
    (let ((origin (current-buffer)))
      (dolist (policy '(nil command-completion-default-include-p))
        (dolist (mode '(text-mode hermes-chat-mode))
          (setq major-mode mode)
          (let* ((read-extended-command-predicate policy)
                 (completing-read-function
                  (lambda (_prompt table predicate &rest _)
                    (with-temp-buffer
                      (setq major-mode 'special-mode)
                      (let ((commands (all-completions "hermes-" table predicate)))
                        (should (member "hermes-chat" commands))
                        (should (eq (not (null (member "hermes-chat-send" commands)))
                                    (or (null policy) (eq mode 'hermes-chat-mode))))))
                    "hermes-chat")))
            (should (equal (read-extended-command) "hermes-chat"))
            (should (eq (current-buffer) origin))
            (should (eq read-extended-command-predicate policy))))))))

(ert-deftest hermes-ui-command-completion-keeps-invocation-and-readers ()
  "Metadata leaves direct invocation, argument readers and guards unchanged."
  (with-temp-buffer
    (insert "draft")
    (goto-char (point-min))
    (should-not (command-completion-default-include-p 'hermes-chat-go-to-composer (current-buffer)))
    (call-interactively #'hermes-chat-go-to-composer)
    (should (= (point) (point-max)))
    (should-error (call-interactively #'hermes-chat-send) :type 'user-error)
    (let ((current-prefix-arg '(4))
          (hermes-dashboard--ewoc 'ewoc)
          received)
      (cl-letf (((symbol-function 'ewoc-goto-next)
                 (lambda (ewoc count) (setq received (list ewoc count)))))
        (call-interactively #'hermes-dashboard-next))
      (should (equal received '(ewoc 4))))
    (let (received)
      (cl-letf (((symbol-function 'hermes-chat--read-session-title) (lambda () "New title"))
                ((symbol-function 'hermes-chat--apply-session-title)
                 (lambda (title) (setq received title)))
                ((symbol-function 'hermes-chat--push-session-title) #'ignore))
        (call-interactively #'hermes-chat-rename))
      (should (string-prefix-p "New title--" received)))))

(ert-deftest hermes-ui-command-completion-autoload-metadata ()
  "Explicit and generated autoloads retain contexts without loading a feature."
  (require 'autoload)
  (dolist (case '(("hermes-chat" autoload hermes-chat-work
                  hermes-chat-mode hermes-work-mode)
                 ("hermes-chat-todos" defun hermes-chat-show-todos
                  hermes-chat-mode)
                 ("hermes-chat" defun hermes-dashboard-reconnect
                  hermes-chat-mode)))
    (let ((command (nth 2 case))
          (loaded features)
          form)
      (with-temp-buffer
        (insert-file-contents
         (concat (file-name-sans-extension (locate-library (car case))) ".el"))
        (goto-char (point-min))
        (while (not form)
          (let ((candidate (read (current-buffer))))
            (when (and (eq (car-safe candidate) (cadr case))
                       (equal (cadr candidate)
                              (if (eq (cadr case) 'autoload)
                                  (list 'quote command) command)))
              (setq form (if (eq (cadr case) 'autoload) candidate
                           (make-autoload candidate (car case))))))))
      (cl-letf (((symbol-function command) nil))
        (eval form t)
        (should (autoloadp (symbol-function command)))
        (should (equal (command-modes command) (nthcdr 3 case)))
        (when (eq command 'hermes-dashboard-reconnect)
          (should (equal (command-modes 'hermes-reconnect) '(hermes-chat-mode))))
        (with-temp-buffer
          (should-not (command-completion-default-include-p command (current-buffer)))
          (dolist (mode (nthcdr 3 case))
            (setq major-mode mode)
            (should (command-completion-default-include-p command (current-buffer))))))
      (should (equal features loaded)))))

(provide 'hermes-ui-tests)
;;; hermes-ui-tests.el ends here
