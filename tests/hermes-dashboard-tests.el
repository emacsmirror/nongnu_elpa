;;; hermes-dashboard-tests.el --- dashboard tests for hermes-el  -*- lexical-binding: t; -*-

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

(ert-deftest hermes-dashboard-transport-call-resolves-on-result ()
  "A call resolves its promise with the JSON-RPC result for the matching id."
  (let* ((client (hermes-test--dashboard-client))
         last-frame resolved
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text)
            (setq last-frame (hermes-dashboard-transport--decode-frame text)))))
    (hermes--promise-then
     (hermes-dashboard-transport-call client "session.list" '((cols . 80)))
     (lambda (result) (setq resolved result)))
    (should (equal (alist-get 'method last-frame) "session.list"))
    (hermes-dashboard-transport--handle-frame
     client
     `((jsonrpc . "2.0") (id . ,(alist-get 'id last-frame)) (result . "ok")))
    (should (equal resolved "ok"))))

(ert-deftest hermes-dashboard-transport-call-rejects-on-error ()
  "A call rejects its promise with the error message for the matching id."
  (let* ((client (hermes-test--dashboard-client))
         last-frame rejected
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text)
            (setq last-frame (hermes-dashboard-transport--decode-frame text)))))
    (hermes--promise-catch
     (hermes-dashboard-transport-call client "session.create" nil)
     (lambda (reason) (setq rejected reason)))
    (hermes-dashboard-transport--handle-frame
     client
     `((jsonrpc . "2.0") (id . ,(alist-get 'id last-frame))
       (error . ((message . "boom")))))
    (should (string-match-p "boom" rejected))))

(ert-deftest hermes-dashboard-transport-call-rejects-on-timeout ()
  "A call rejects its promise when the request times out."
  (let* ((client (hermes-test--dashboard-client))
         last-frame rejected
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text)
            (setq last-frame (hermes-dashboard-transport--decode-frame text)))))
    (hermes--promise-catch
     (hermes-dashboard-transport-call client "session.list" nil)
     (lambda (reason) (setq rejected reason)))
    (hermes-dashboard-transport--on-request-timeout
     client (alist-get 'id last-frame))
    (should (string-match-p "timed out" rejected))))

(ert-deftest hermes-dashboard-transport-tools-configure-sends-action-payload ()
  "The transport wrapper sends `tools.configure' names/action/session_id."
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((ok . t))))))
      (let ((client (hermes-test--dashboard-client)))
        (setf (hermes-dashboard-transport-client-session-id client) "sid-1")
        (hermes-dashboard-transport-tools-configure
         client '("terminal") "disable" :resolve #'ignore :reject #'ignore))
      (should (equal method "tools.configure"))
      (should (equal (cdr (assq 'names params)) '("terminal")))
      (should (equal (cdr (assq 'action params)) "disable"))
      (should (equal (cdr (assq 'session_id params)) "sid-1")))))

(ert-deftest hermes-dashboard-transport-skills-reload-sends-rpc ()
  "The transport wrapper sends `skills.reload' without shelling out."
  (let (method params resolved)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((output . "ok"))))))
      (hermes-dashboard-transport-skills-reload
       'fake-client
       :resolve (lambda (result) (setq resolved result))
       :reject #'ignore)
      (should (equal method "skills.reload"))
      (should-not params)
      (should (equal resolved '((output . "ok")))))))

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

;;; Group: kanban events WS-URL plumbing

(ert-deftest hermes-dashboard-websocket-endpoint-for-parameterizes-path ()
  "The path-parameterized endpoint builds ws/wss URLs; the legacy one is /api/ws."
  (should (equal "ws://127.0.0.1:8765/api/plugins/kanban/events"
                 (hermes-dashboard-transport--websocket-endpoint-for
                  "127.0.0.1" 8765 "/api/plugins/kanban/events")))
  (should (string-prefix-p
           "wss://example.com"
           (hermes-dashboard-transport--websocket-endpoint-for
            "example.com" nil "/api/plugins/kanban/events" "https://example.com")))
  (should (equal "ws://127.0.0.1:8765/api/ws"
                 (hermes-dashboard-transport--websocket-endpoint
                  "127.0.0.1" 8765))))

(ert-deftest hermes-dashboard-swap-websocket-path-preserves-query ()
  "Swapping the endpoint path leaves the credential query untouched."
  (should (equal "ws://h:1/api/plugins/kanban/events?token=ABC"
                 (hermes-dashboard-transport--swap-websocket-path
                  "ws://h:1/api/ws?token=ABC" "/api/plugins/kanban/events")))
  (should (equal "ws://h:1/api/plugins/kanban/events?ticket=XYZ"
                 (hermes-dashboard-transport--swap-websocket-path
                  "ws://h:1/api/ws?ticket=XYZ" "/api/plugins/kanban/events"))))

(ert-deftest hermes-dashboard-append-url-query-drops-nil-and-escapes ()
  "Nil-valued params are dropped; values are percent-encoded."
  (should (equal "u&since=5&board=emacs%20lisp"
                 (hermes-dashboard-transport--append-url-query
                  "u" '((since . 5) (board . "emacs lisp")))))
  (should (equal "u&since=0"
                 (hermes-dashboard-transport--append-url-query
                  "u" '((since . 0) (board . nil))))))

(ert-deftest hermes-dashboard-kanban-events-plist-swaps-and-redacts ()
  "The events plist swaps the path, appends since/board, and never leaks a secret."
  (let ((plist (hermes-dashboard-transport--kanban-events-plist
                '(:url "ws://h:1/api/ws?token=SEKRIT"
                  :redacted-url "ws://h:1/api/ws?token=<redacted>"
                  :secrets ("SEKRIT"))
                5 "emacs-lisp")))
    (should (equal (concat "ws://h:1/api/plugins/kanban/events?token=SEKRIT"
                           "&since=5&board=emacs-lisp")
                   (plist-get plist :url)))
    (should-not (string-match-p "SEKRIT" (plist-get plist :redacted-url)))
    (should (string-search "kanban/events?token=<redacted>"
                           (plist-get plist :redacted-url)))
    (should (equal '("SEKRIT") (plist-get plist :secrets)))))

(ert-deftest hermes-dashboard-kanban-events-url-async-reuses-client ()
  "A live client's resolved URL is reused without a fresh auth round-trip."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket-url "ws://127.0.0.1:8765/api/ws?token=SEKRIT"
                 :redacted-websocket-url "ws://127.0.0.1:8765/api/ws?token=<redacted>"
                 :secrets '("SEKRIT")))
        result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-auth-async)
               (lambda (&rest _) (error "must not resolve fresh auth"))))
      (hermes--promise-then
       (hermes-dashboard-transport-kanban-events-url-async
        :since 9 :board "emacs-lisp" :client client)
       (lambda (v) (setq result v))))
    (should (equal (concat "ws://127.0.0.1:8765/api/plugins/kanban/events"
                           "?token=SEKRIT&since=9&board=emacs-lisp")
                   (plist-get result :url)))
    (should (equal '("SEKRIT") (plist-get result :secrets)))))

(ert-deftest hermes-dashboard-kanban-events-url-async-resolves-fresh ()
  "Without a client, auth resolves against the configured URL and is path-swapped."
  (let ((hermes-dashboard-transport-url "http://127.0.0.1:8765")
        result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-auth-async)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '(:url "ws://127.0.0.1:8765/api/ws?token=SEKRIT"
                    :redacted-url "ws://127.0.0.1:8765/api/ws?token=<redacted>"
                    :secrets ("SEKRIT"))))))
      (hermes--promise-then
       (hermes-dashboard-transport-kanban-events-url-async :board "emacs-lisp")
       (lambda (v) (setq result v))))
    (should (string-search "/api/plugins/kanban/events?token=SEKRIT"
                           (plist-get result :url)))
    (should (string-match-p "board=emacs-lisp" (plist-get result :url)))
    (should-not (string-match-p "SEKRIT" (plist-get result :redacted-url)))))

(ert-deftest hermes-dashboard-open-websocket-delivers-text-and-redacts-errors ()
  "The opener hands frame text to :on-message and scrubs secrets from errors."
  (let (msg err)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-frame-text) (lambda (f) f))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (funcall (plist-get args :on-message) nil "{\"events\":[]}")
                 (funcall (plist-get args :on-error) nil 'on-error "boom SEKRIT")
                 'fake-socket)))
      (let ((socket (hermes-dashboard-transport-open-websocket
                     "ws://h/api/plugins/kanban/events?token=SEKRIT"
                     "ws://h/api/plugins/kanban/events?token=<redacted>"
                     '("SEKRIT")
                     :on-message (lambda (text) (setq msg text))
                     :on-error (lambda (m) (setq err m)))))
        (should (eq socket 'fake-socket))
        (should (equal msg "{\"events\":[]}"))
        (should (stringp err))
        (should-not (string-match-p "SEKRIT" err))))))

(provide 'hermes-dashboard-tests)
;;; hermes-dashboard-tests.el ends here
