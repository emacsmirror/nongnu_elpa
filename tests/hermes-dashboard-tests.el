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
  :tags '(shared-socket-isolation)
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((ok . t))))))
      (let ((client (hermes-test--dashboard-client)))
        (hermes-dashboard-transport-tools-configure
         client '("terminal") "disable"
         :session-id "sid-1" :resolve #'ignore :reject #'ignore))
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

(ert-deftest hermes-dashboard-transport-handoff-request-sends-platform ()
  "The transport wrapper sends `handoff.request' with platform and session_id."
  :tags '(shared-socket-isolation)
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((queued . t))))))
      (let ((client (hermes-test--dashboard-client)))
        (hermes-dashboard-transport-handoff-request
         client "telegram"
         :session-id "sid-1" :resolve #'ignore :reject #'ignore))
      (should (equal method "handoff.request"))
      (should (equal (cdr (assq 'platform params)) "telegram"))
      (should (equal (cdr (assq 'session_id params)) "sid-1")))))

(ert-deftest hermes-dashboard-transport-handoff-state-sends-session ()
  "The transport wrapper sends `handoff.state' scoped to the session."
  :tags '(shared-socket-isolation)
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((state . "pending"))))))
      (let ((client (hermes-test--dashboard-client)))
        (hermes-dashboard-transport-handoff-state
         client :session-id "sid-2" :resolve #'ignore :reject #'ignore))
      (should (equal method "handoff.state"))
      (should (equal (cdr (assq 'session_id params)) "sid-2")))))

(ert-deftest hermes-dashboard-transport-handoff-fail-sends-error ()
  "The transport wrapper sends `handoff.fail' with the error reason."
  :tags '(shared-socket-isolation)
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((failed . t))))))
      (let ((client (hermes-test--dashboard-client)))
        (hermes-dashboard-transport-handoff-fail
         client :error "poll timed out"
         :session-id "sid-3" :resolve #'ignore :reject #'ignore))
      (should (equal method "handoff.fail"))
      (should (equal (cdr (assq 'error params)) "poll timed out"))
      (should (equal (cdr (assq 'session_id params)) "sid-3")))))

(ert-deftest hermes-dashboard-transport-complete-slash-sends-text ()
  "The transport wrapper sends `complete.slash' with the partial command text."
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((items . []))))))
      (hermes-dashboard-transport-complete-slash
       'fake-client "/handoff " :resolve #'ignore :reject #'ignore)
      (should (equal method "complete.slash"))
      (should (equal (cdr (assq 'text params)) "/handoff ")))))

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

;;; Group: reconnect

(ert-deftest hermes-dashboard-transport-reconnect-backoff-doubles-and-caps ()
  "Reconnect backoff doubles per attempt and caps at the max delay."
  (let ((hermes-dashboard-transport-reconnect-base-delay 1)
        (hermes-dashboard-transport-reconnect-max-delay 8))
    (should (= (hermes-dashboard-transport--reconnect-backoff 0) 1))
    (should (= (hermes-dashboard-transport--reconnect-backoff 2) 4))
    (should (= (hermes-dashboard-transport--reconnect-backoff 5) 8))))

(ert-deftest hermes-dashboard-transport-unexpected-close-schedules-reconnect ()
  "An unexpected close on a referenced client schedules a reconnect, staying registered."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-reconnect-max-attempts 3)
         (hermes-dashboard-transport-reconnect-base-delay 1)
         events scheduled
         (hermes-dashboard-transport-schedule-function
          (lambda (delay _fn &rest _args) (setq scheduled delay) 'timer)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (make-hermes-dashboard-transport-client :websocket 'ws))))
      (let ((c (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-subscribe c (lambda (e) (push e events)))
        (hermes-dashboard-transport--handle-socket-down c "dropped")
        (should (hermes-dashboard-transport-client-reconnecting-p c))
        (should (eq (gethash 'local-spawn hermes-dashboard-transport--clients) c))
        (should (equal scheduled 1))
        (should (cl-find "closed" events
                         :key (lambda (e) (plist-get e :status)) :test #'equal))))))

(ert-deftest hermes-dashboard-transport-unreferenced-close-finalizes ()
  "An unexpected close on an unreferenced client finalizes instead of reconnecting."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-reconnect-max-attempts 3)
         scheduled
         (hermes-dashboard-transport-schedule-function
          (lambda (&rest _) (setq scheduled t) 'timer)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (make-hermes-dashboard-transport-client :websocket 'ws))))
      (let ((c (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (setf (hermes-dashboard-transport-client-refcount c) 0)
        (hermes-dashboard-transport--handle-socket-down c "dropped")
        (should-not scheduled)
        (should-not (gethash 'local-spawn hermes-dashboard-transport--clients))))))

(ert-deftest hermes-dashboard-transport-stop-suppresses-reconnect ()
  "An intentional stop marks the socket closed without scheduling a reconnect."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-reconnect-max-attempts 3)
         scheduled
         (hermes-dashboard-transport-schedule-function
          (lambda (&rest _) (setq scheduled t) 'timer)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (make-hermes-dashboard-transport-client :websocket 'ws)))
              ((symbol-function 'websocket-close) #'ignore))
      (let ((c (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-stop c "bye")
        (hermes-dashboard-transport--handle-socket-down c "dropped")
        (should-not scheduled)
        (should-not (hermes-dashboard-transport-client-reconnecting-p c))))))

(ert-deftest hermes-dashboard-transport-reconnect-reopens-socket ()
  "A reconnect attempt reopens the socket through the open function."
  (let* ((hermes-dashboard-transport-reconnect-max-attempts 3)
         opened
         (hermes-dashboard-transport-websocket-open-function
          (lambda (_url _client) (setq opened t) 'new-ws))
         (c (make-hermes-dashboard-transport-client
             :reconnecting-p t :refcount 1 :websocket-url "ws://x")))
    (hermes-dashboard-transport--reconnect-attempt c 0)
    (should opened)
    (should (eq (hermes-dashboard-transport-client-websocket c) 'new-ws))))

(ert-deftest hermes-dashboard-transport-reconnect-gives-up-after-max ()
  "Exhausting reconnect attempts finalizes the client and reports closed."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-reconnect-max-attempts 2)
         events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (make-hermes-dashboard-transport-client :websocket 'ws))))
      (let ((c (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-subscribe c (lambda (e) (push e events)))
        (setf (hermes-dashboard-transport-client-reconnecting-p c) t)
        (hermes-dashboard-transport--reconnect-attempt c 2)
        (should-not (gethash 'local-spawn hermes-dashboard-transport--clients))
        (should-not (hermes-dashboard-transport-client-reconnecting-p c))
        (should (cl-find "closed" events
                         :key (lambda (e) (plist-get e :status)) :test #'equal))))))

(ert-deftest hermes-dashboard-transport-reconnect-ready-emits-reconnected ()
  "After reconnect, `gateway.ready' clears reconnect state and broadcasts reconnected."
  (let* (events
         (c (make-hermes-dashboard-transport-client
             :reconnecting-p t :reconnect-attempts 2 :websocket 'ws)))
    (hermes-dashboard-transport-subscribe c (lambda (e) (push e events)))
    (hermes-dashboard-transport--handle-frame
     c '((jsonrpc . "2.0") (method . "event")
         (params . ((type . "gateway.ready")))))
    (should-not (hermes-dashboard-transport-client-reconnecting-p c))
    (should (= (hermes-dashboard-transport-client-reconnect-attempts c) 0))
    (should (cl-find "reconnected" events
                     :key (lambda (e) (plist-get e :status)) :test #'equal))))

(ert-deftest hermes-dashboard-transport-reconnect-defers-requests-until-new-gateway-ready ()
  "A request issued during reconnect sends no frame until the new `gateway.ready'."
  (let* ((hermes-dashboard-transport-reconnect-max-attempts 3)
         (hermes-dashboard-transport-reconnect-base-delay 1)
         (hermes-dashboard-transport-ready-timeout nil)
         (sent-frames nil)
         (reconnect-timer nil)
         (hermes-dashboard-transport-websocket-open-function
          (lambda (_url _client) 'new-ws))
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text) (push text sent-frames)))
         (hermes-dashboard-transport-schedule-function
          (lambda (_delay fn &rest args)
            (setq reconnect-timer (lambda () (apply fn args)))
            'fake-timer))
         (old-ready (hermes--promise-resolved 'first-ready))
         (c (make-hermes-dashboard-transport-client
             :ready-p t :ready-promise old-ready
             :websocket 'old-ws :refcount 1
             :websocket-url "ws://x"
             :pending (make-hash-table :test #'equal))))
    ;; Socket drops unexpectedly: reconnect begins, readiness resets.
    (hermes-dashboard-transport--handle-socket-down c "dropped")
    (should (hermes-dashboard-transport-client-reconnecting-p c))
    (should-not (hermes-dashboard-transport-client-ready-p c))
    (let ((promise (hermes-dashboard-transport-client-ready-promise c)))
      (should (hermes--promise-p promise))
      (should (eq (hermes--promise-state promise) 'pending))
      (should-not (eq promise old-ready)))
    ;; Request before gateway.ready: must not send.
    (hermes-dashboard-transport-request c "ping" nil #'ignore #'ignore)
    (should-not sent-frames)
    ;; Reconnect attempt installs the replacement socket; still no frame.
    (when (functionp reconnect-timer) (funcall reconnect-timer))
    (should (eq (hermes-dashboard-transport-client-websocket c) 'new-ws))
    (should-not sent-frames)
    ;; New gateway.ready resolves the fresh promise; the deferred frame sends.
    (hermes-dashboard-transport--handle-frame
     c '((jsonrpc . "2.0") (method . "event")
         (params . ((type . "gateway.ready")))))
    (should (hermes-dashboard-transport-client-ready-p c))
    (should (= (length sent-frames) 1))))

;;; Group: heartbeat keepalive

(ert-deftest hermes-dashboard-transport-heartbeat-arms-on-ready-and-pings ()
  "With an interval set, `gateway.ready' arms a heartbeat that sends pings."
  (let* ((hermes-dashboard-transport-heartbeat-interval 30)
         (schedules 0)
         pings captured
         (hermes-dashboard-transport-schedule-function
          (lambda (delay fn &rest args)
            (setq schedules (1+ schedules)
                  captured (list delay fn args))
            'fake-timer))
         (hermes-dashboard-transport-ping-function
          (lambda (ws) (push ws pings)))
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket)))
    (hermes-dashboard-transport--handle-frame
     client '((jsonrpc . "2.0") (method . "event")
              (params . ((type . "gateway.ready")))))
    (should (hermes-dashboard-transport-client-ready-p client))
    (should (equal (nth 0 captured) 30))
    (should (= schedules 1))
    (should (eq (hermes-dashboard-transport-client-heartbeat-timer client)
                'fake-timer))
    (apply (nth 1 captured) (nth 2 captured))
    (should (equal pings '(fake-websocket)))
    (should (= schedules 2))))

(ert-deftest hermes-dashboard-transport-heartbeat-disabled-when-nil ()
  "A nil heartbeat interval arms no timer."
  (let* ((hermes-dashboard-transport-heartbeat-interval nil)
         armed
         (hermes-dashboard-transport-schedule-function
          (lambda (&rest _) (setq armed t) 'fake-timer))
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket)))
    (hermes-dashboard-transport--arm-heartbeat client)
    (should-not armed)
    (should-not (hermes-dashboard-transport-client-heartbeat-timer client))))

(ert-deftest hermes-dashboard-transport-heartbeat-cleared-on-close ()
  "Marking the socket closed clears the heartbeat timer."
  (let* ((hermes-dashboard-transport-heartbeat-interval 30)
         (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-schedule-function
          (lambda (&rest _) 'fake-timer))
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket)))
    (hermes-dashboard-transport--arm-heartbeat client)
    (should (hermes-dashboard-transport-client-heartbeat-timer client))
    (hermes-dashboard-transport--mark-websocket-closed client)
    (should-not (hermes-dashboard-transport-client-heartbeat-timer client))))

(ert-deftest hermes-dashboard-transport-heartbeat-tick-stops-without-socket ()
  "A heartbeat tick on a closed socket sends nothing and does not re-arm."
  (let* ((hermes-dashboard-transport-heartbeat-interval 30)
         armed pings
         (hermes-dashboard-transport-schedule-function
          (lambda (&rest _) (setq armed t) 'fake-timer))
         (hermes-dashboard-transport-ping-function
          (lambda (ws) (push ws pings)))
         (client (make-hermes-dashboard-transport-client :websocket nil)))
    (hermes-dashboard-transport--heartbeat-tick client)
    (should-not pings)
    (should-not armed)))

;;; Group: subscriber registry + session demux

(ert-deftest hermes-dashboard-transport-subscribe-routes-tagged-event-to-owner ()
  "A tagged event reaches only the subscriber owning its session id."
  (let* ((client (hermes-test--dashboard-client))
         a-events b-events
         (a (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e a-events))))
         (b (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e b-events)))))
    (hermes-dashboard-transport-subscribe-session client a "sid-a")
    (hermes-dashboard-transport-subscribe-session client b "sid-b")
    (hermes-dashboard-transport--dispatch-event
     client (list :type 'delta :session-id "sid-a" :content "x"))
    (should (equal (length a-events) 1))
    (should-not b-events)))

(ert-deftest hermes-dashboard-transport-untagged-event-broadcasts ()
  "An untagged connection-level event reaches every subscriber."
  (let* ((client (hermes-test--dashboard-client))
         a-events b-events
         (a (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e a-events))))
         (b (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e b-events)))))
    (hermes-dashboard-transport-subscribe-session client a "sid-a")
    (hermes-dashboard-transport-subscribe-session client b "sid-b")
    (hermes-dashboard-transport--dispatch-event
     client (list :type 'status :status "closed"))
    (should (equal (length a-events) 1))
    (should (equal (length b-events) 1))))

(ert-deftest hermes-dashboard-transport-unowned-tagged-event-broadcasts ()
  "A tagged event with no registered owner falls back to a broadcast."
  (let* ((client (hermes-test--dashboard-client))
         events
         (token (hermes-dashboard-transport-subscribe
                 client (lambda (e) (push e events)))))
    (hermes-dashboard-transport-subscribe-session client token "sid-a")
    (hermes-dashboard-transport--dispatch-event
     client (list :type 'delta :session-id "other" :content "x"))
    (should (equal (length events) 1))))

(ert-deftest hermes-dashboard-transport-dispatch-falls-back-to-callback ()
  "With no subscribers, dispatch uses the legacy single callback."
  (let* (received
         (client (make-hermes-dashboard-transport-client
                  :callback (lambda (e) (setq received e)))))
    (hermes-dashboard-transport--dispatch-event client (list :type 'done))
    (should (equal received (list :type 'done)))))

(ert-deftest hermes-dashboard-transport-unsubscribe-stops-delivery ()
  "Unsubscribing a token stops delivery and clears the session index."
  (let* ((client (hermes-test--dashboard-client))
         events
         (token (hermes-dashboard-transport-subscribe
                 client (lambda (e) (push e events)))))
    (hermes-dashboard-transport-subscribe-session client token "sid-a")
    (hermes-dashboard-transport-unsubscribe client token)
    (hermes-dashboard-transport--dispatch-event
     client (list :type 'delta :session-id "sid-a"))
    (hermes-dashboard-transport--dispatch-event client (list :type 'status))
    (should-not events)
    (should-not (hermes-dashboard-transport--session-subscriber-fn
                 client "sid-a"))))

(ert-deftest hermes-dashboard-transport-subscribe-session-rebinds-index ()
  "Re-binding a token to a new session id moves it off the old id."
  (let* ((client (hermes-test--dashboard-client))
         events
         (token (hermes-dashboard-transport-subscribe
                 client (lambda (e) (push e events)))))
    (hermes-dashboard-transport-subscribe-session client token "old")
    (hermes-dashboard-transport-subscribe-session client token "new")
    (should-not (hermes-dashboard-transport--session-subscriber-fn client "old"))
    (should (hermes-dashboard-transport--session-subscriber-fn client "new"))
    (hermes-dashboard-transport--dispatch-event
     client (list :type 'delta :session-id "new"))
    (should (equal (length events) 1))))

(ert-deftest hermes-dashboard-transport-unsubscribe-preserves-other-owner ()
  "Unsubscribing a stale token does not evict another token's session ownership."
  (let* ((client (hermes-test--dashboard-client))
         a-events b-events
         (a (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e a-events))))
         (b (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e b-events)))))
    (hermes-dashboard-transport-subscribe-session client a "sid")
    (hermes-dashboard-transport-subscribe-session client b "sid")
    (hermes-dashboard-transport-unsubscribe client a)
    (hermes-dashboard-transport--dispatch-event
     client (list :type 'delta :session-id "sid"))
    (should-not a-events)
    (should (equal (length b-events) 1))))

(ert-deftest hermes-dashboard-transport-handle-frame-demuxes-by-session ()
  "An inbound event frame routes to the subscriber owning its session id."
  (let* ((client (hermes-test--dashboard-client))
         a-events b-events
         (a (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e a-events))))
         (b (hermes-dashboard-transport-subscribe
             client (lambda (e) (push e b-events)))))
    (hermes-dashboard-transport-subscribe-session client a "sid-a")
    (hermes-dashboard-transport-subscribe-session client b "sid-b")
    (hermes-dashboard-transport--handle-frame
     client
     '((jsonrpc . "2.0") (method . "event")
       (params . ((type . "message.delta") (session_id . "sid-a")
                  (payload . ((delta . "hi")))))))
    (should (equal (length a-events) 1))
    (should (equal (plist-get (car a-events) :session-id) "sid-a"))
    (should-not b-events)))

;;; Group: shared client registry

(ert-deftest hermes-dashboard-transport-endpoint-key-spawn-is-local ()
  "A spawn-mode target keys on the single `local-spawn' endpoint."
  (should (eq (hermes-dashboard-transport--endpoint-key
               :host "127.0.0.1" :port 8765 :start-mode 'spawn)
              'local-spawn)))

(ert-deftest hermes-dashboard-transport-endpoint-key-remote-is-base-url ()
  "A remote target keys on its normalized base URL."
  (should (equal (hermes-dashboard-transport--endpoint-key
                  :start-mode 'remote :remote-url "https://h.example/")
                 "https://h.example")))

(ert-deftest hermes-dashboard-transport-acquire-shares-client-by-endpoint ()
  "Acquiring the same endpoint twice reuses one client and counts references."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (made 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (cl-incf made)
                 (make-hermes-dashboard-transport-client))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn))
            (c2 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (should (eq c1 c2))
        (should (= made 1))
        (should (= (hermes-dashboard-transport-client-refcount c1) 2))))))

(ert-deftest hermes-dashboard-transport-acquire-distinct-endpoints ()
  "Different remote endpoints get distinct clients."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (make-hermes-dashboard-transport-client))))
      (let ((c1 (hermes-dashboard-transport-acquire
                 :start-mode 'remote :remote-url "https://a.example"))
            (c2 (hermes-dashboard-transport-acquire
                 :start-mode 'remote :remote-url "https://b.example")))
        (should-not (eq c1 c2))))))

(ert-deftest hermes-dashboard-transport-release-stops-and-unregisters-at-zero ()
  "Release decrements references and tears the client down only at zero."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (make-hermes-dashboard-transport-client))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-acquire :start-mode 'spawn)
        (should (= (hermes-dashboard-transport-release c1) 1))
        (should (eq (gethash 'local-spawn hermes-dashboard-transport--clients) c1))
        (should (= (hermes-dashboard-transport-release c1) 0))
        (should-not (gethash 'local-spawn hermes-dashboard-transport--clients))
        (should-not (eq c1 (hermes-dashboard-transport-acquire
                            :start-mode 'spawn)))))))

(ert-deftest hermes-dashboard-transport-release-schedules-idle-close ()
  "With an idle delay, the last release keeps the client warm and re-acquire reuses it."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-idle-close-delay 30)
         scheduled
         (hermes-dashboard-transport-schedule-function
          (lambda (delay _fn &rest _args) (setq scheduled delay) 'fake-timer)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (make-hermes-dashboard-transport-client))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-release c1)
        (should (equal scheduled 30))
        (should (eq (gethash 'local-spawn hermes-dashboard-transport--clients) c1))
        (should (eq c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (should (= (hermes-dashboard-transport-client-refcount c1) 1))
        (should-not (hermes-dashboard-transport-client-idle-timer c1))))))

(ert-deftest hermes-dashboard-transport-idle-close-stops-when-still-idle ()
  "Firing the idle-close timer tears down a client that stayed idle."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-idle-close-delay 30)
         captured
         (hermes-dashboard-transport-schedule-function
          (lambda (_delay fn &rest args) (setq captured (cons fn args)) 'fake-timer)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (make-hermes-dashboard-transport-client))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-release c1)
        (apply (car captured) (cdr captured))
        (should-not (gethash 'local-spawn
                             hermes-dashboard-transport--clients))))))

(ert-deftest hermes-dashboard-transport-idle-timer-after-rebuild-is-harmless ()
  "A stale idle timer firing after a drop and rebuild leaves the fresh client."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-idle-close-delay 30)
         captured
         (hermes-dashboard-transport-schedule-function
          (lambda (_delay fn &rest args) (setq captured (cons fn args)) 'fake-timer)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-release c1)
        (hermes-dashboard-transport--handle-socket-down c1 "dropped")
        (let ((c2 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
          (apply (car captured) (cdr captured))
          (should-not (eq c1 c2))
          (should (eq (gethash 'local-spawn hermes-dashboard-transport--clients)
                      c2)))))))

(ert-deftest hermes-dashboard-transport-acquire-rebuilds-after-close ()
  "With reconnect off, a dropped socket finalizes so the next acquire rebuilds."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (hermes-dashboard-transport-reconnect-max-attempts nil))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-acquire :start-mode 'spawn)
        (hermes-dashboard-transport--handle-socket-down c1 "dropped")
        (should-not (gethash 'local-spawn hermes-dashboard-transport--clients))
        (should-not (eq c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))))))

(ert-deftest hermes-dashboard-transport-unregister-keeps-replacement ()
  "Stopping a stale client does not evict a replacement under the same key."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (make-hermes-dashboard-transport-client))))
      (let ((c1 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
        (hermes-dashboard-transport-release c1)
        (let ((c2 (hermes-dashboard-transport-acquire :start-mode 'spawn)))
          (should-not (eq c1 c2))
          (hermes-dashboard-transport-stop c1)
          (should (eq (gethash 'local-spawn hermes-dashboard-transport--clients)
                      c2)))))))


;;; Shared-socket session param isolation

(ert-deftest hermes-dashboard-transport-session-param-is-explicit-only ()
  "`--session-param' returns the explicit id and never reads the client."
  :tags '(shared-socket-isolation)
  (let ((client (make-hermes-dashboard-transport-client
                 :session-id "stale-sid")))
    (should (equal (hermes-dashboard-transport--session-param client "live-sid")
                   "live-sid"))
    (should-not (hermes-dashboard-transport--session-param client nil))))

(ert-deftest hermes-dashboard-transport-session-response-does-not-store-client-session ()
  "A `session.create' response must not mutate the shared client."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-dashboard-transport--store-session-result
     client "session.create"
     '((session_id . "sid-live") (stored_session_id . "sid-stored")))
    (should-not (hermes-dashboard-transport-client-session-id client))
    (should-not (hermes-dashboard-transport-client-stored-session-id client))))

(ert-deftest hermes-dashboard-transport-session-rpc-omits-session-when-not-explicit ()
  "A `:session t' RPC that omits `:session-id' sends no session_id param."
  :tags '(shared-socket-isolation)
  (let (params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client _method p resolve _reject)
                 (setq params p)
                 (funcall resolve '((ok . t))))))
      (let ((client (make-hermes-dashboard-transport-client
                     :session-id "ambient-sid")))
        (hermes-dashboard-transport-tools-configure
         client '("terminal") "disable" :resolve #'ignore :reject #'ignore))
      (should-not (assq 'session_id params)))))

(ert-deftest hermes-dashboard-transport-session-rpc-forwards-explicit-session ()
  "A `:session t' RPC with explicit `:session-id' forwards it."
  :tags '(shared-socket-isolation)
  (let (params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client _method p resolve _reject)
                 (setq params p)
                 (funcall resolve '((ok . t))))))
      (let ((client (hermes-test--dashboard-client)))
        (hermes-dashboard-transport-tools-configure
         client '("terminal") "disable"
         :session-id "explicit-sid" :resolve #'ignore :reject #'ignore))
      (should (equal (cdr (assq 'session_id params)) "explicit-sid")))))

(provide 'hermes-dashboard-tests)
;;; hermes-dashboard-tests.el ends here
