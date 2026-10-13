;;; hermes-connectors-tests.el --- Connector card tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Frames are recorded from the released backend contract
;; (tui_gateway/contracts/connectors_operation.py); the connector service
;; itself is not needed to exercise the card.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defun hermes-connectors-test--wire (value)
  "Return VALUE as an RPC result after a JSON round trip."
  (hermes-transport-json-parse (json-serialize value)))

(defun hermes-connectors-test--target (&optional state &rest fields)
  "Return a connector target in STATE with extra FIELDS alist."
  (append fields
          `((name . "github") (kind . "connector") (action . "reconnect")
            (state . ,(or state "initiated")) (alias . "work")
            (instructions . "Approve access in the browser tab.")
            (connect_url . "https://connect.example/authorize"))))

(defun hermes-connectors-test--payload (&optional seq state &rest targets)
  "Return a `connection.request' payload at SEQ with a target in STATE.
TARGETS, when given, replace the default target."
  `((op_id . "op-1") (seq . ,(or seq 1))
    (tool_call_id . "call-1") (deadline_at . 2000000000)
    (timeout_seconds . 600)
    (targets . ,(vconcat (or targets
                             (list (hermes-connectors-test--target state)))))))

(defun hermes-connectors-test--update (client seq state &optional settled)
  "Emit a released full update through CLIENT at SEQ in STATE and SETTLED."
  (hermes-test--emit-dashboard-prompt
   client "connection.update"
   (append `((owner . ((type . "session") (session_id . "sid-prompt")))
             (settled . ,(if settled t :false)))
           (assq-delete-all 'tool_call_id
                            (hermes-connectors-test--payload seq state)))))

(defun hermes-connectors-test--buttons ()
  "Return the labels of the connector buttons in the buffer."
  (let (labels (button (next-button (point-min))))
    (while button
      (when (get-text-property (button-start button) 'hermes-connector)
        (push (button-label button) labels))
      (setq button (next-button (button-end button))))
    (nreverse labels)))

(defun hermes-connectors-test--goto (text)
  "Move point to the start of the first TEXT in the card."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defmacro hermes-connectors-test--capturing (requests &rest body)
  "Run BODY recording dashboard REQUESTS, answering each with ok."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'hermes-dashboard-transport-request)
              (lambda (_client method params resolve _reject)
                (push (cons method params) ,requests)
                (funcall resolve '((status . "ok"))))))
     ,@body))

(ert-deftest hermes-connectors-card-attaches-to-exact-tool ()
  "A request renders under its originating tool row, not the composer."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "tool.start" '((tool_id . "call-1") (name . "manage_connections")))
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (should (string-match-p "Reconnect github · work  waiting for browser"
                            (buffer-string)))
    (should (string-match-p "Approve access in the browser tab" (buffer-string)))
    (should-not (seq-find (lambda (e) (eq (plist-get e :role) 'connection))
                          (hermes-chat--entries)))
    (should (equal (hermes-chat-input-string) ""))))

(ert-deftest hermes-connectors-fallback-moves-when-tool-arrives ()
  "Without its tool row the card stands alone, then moves under the row."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (should (gethash "connection:op-1" hermes-chat--nodes))
    (hermes-test--emit-dashboard-prompt
     client "tool.start" '((tool_id . "call-other") (name . "terminal")))
    (should (gethash "connection:op-1" hermes-chat--nodes))
    (hermes-test--emit-dashboard-prompt
     client "tool.start" '((tool_id . "call-1") (name . "manage_connections")))
    (should-not (gethash "connection:op-1" hermes-chat--nodes))
    (should (= 1 (how-many "Reconnect github" (point-min) (point-max))))))

(ert-deftest hermes-connectors-sequence-and-settlement-are-authoritative ()
  "Older frames move nothing; a settled card offers no actions or link."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (let ((header (copy-tree hermes-chat--status-state)))
      (hermes-connectors-test--update client 3 "connected" t)
      (hermes-connectors-test--update client 2 "initiated")
      (should (string-match-p "Reconnect github · work  connected"
                              (buffer-string)))
      (should-not (hermes-connectors-test--buttons))
      (hermes-connectors-test--goto "Reconnect github")
      (should-not (hermes-connectors--available-p 'continue))
      (should (equal header hermes-chat--status-state))
      (should (equal "call-1"
                     (plist-get (plist-get (car hermes-connectors--operations)
                                           :snapshot)
                                :tool-call-id))))))

(ert-deftest hermes-connectors-foreign-frames-are-ignored ()
  "Another session's update cannot move this chat's card."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (let ((before (buffer-string)))
      (hermes-test--emit-dashboard-prompt
       client "connection.update"
       (append '((owner . ((type . "session") (session_id . "other"))))
               (hermes-connectors-test--payload 5 "connected")))
      (should (equal before (buffer-string))))))

(ert-deftest hermes-connectors-actions-use-exact-session-owner ()
  "Card actions send the released parameters with this chat's session."
  (hermes-test-with-dashboard-prompt-session (client)
    (let (requests opened)
      (hermes-connectors-test--capturing requests
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq opened url))))
          (hermes-test--emit-dashboard-prompt
           client "connection.request" (hermes-connectors-test--payload))
          (should (equal (hermes-connectors-test--buttons) '("Authorize")))
          (hermes-connectors-test--goto "Authorize")
          (button-activate (button-at (point)))
          (should (equal opened "https://connect.example/authorize"))
          (dolist (command '(hermes-connectors-wake hermes-connectors-decline
                             hermes-connectors-continue))
            (hermes-connectors-test--goto "Reconnect github")
            (funcall command))))
      (setq requests (nreverse requests))
      (should (equal (mapcar #'car requests)
                     '("connectors.operation.wake" "connection.respond"
                       "connection.respond")))
      (dolist (request requests)
        (should (equal (sort (mapcar #'car (cdr request)) #'string<)
                       (if (equal (car request) "connection.respond")
                           '(op_id owner result)
                         '(op_id owner))))
        (should (equal (alist-get 'owner (cdr request))
                       '((type . "session") (session_id . "sid-prompt"))))
        (should (equal (alist-get 'op_id (cdr request)) "op-1")))
      (should (equal (alist-get 'result (cdr (nth 1 requests)))
                     '((targets . [((name . "github") (status . "skipped"))]))))
      (should (equal (alist-get 'result (cdr (nth 2 requests)))
                     '((settled_by . "continue"))))
      ;; An accepted answer is not a fabricated settlement.
      (should-not (plist-get (plist-get (car hermes-connectors--operations)
                                        :snapshot)
                             :settled)))))

(ert-deftest hermes-connectors-retry-projects-the-reissued-link ()
  "Retry re-mints through `connectors.connect' and shows the new link."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload 1 "expired"))
    (should (string-match-p "link expired" (buffer-string)))
    (hermes-connectors-test--goto "Reconnect github")
    (should-not (hermes-connectors--available-p 'open))
    (let (sent)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                 (lambda (_client method params resolve _reject)
                   (setq sent (cons method params))
                   (funcall resolve
                            (hermes-connectors-test--wire
                             (hermes-connectors-test--payload 2 "initiated"))))))
        (hermes-connectors-retry))
      (should (equal (car sent) "connectors.connect"))
      (should (equal (sort (mapcar #'car (cdr sent)) #'string<)
                     '(connectors owner reconnect)))
      (should (equal (alist-get 'connectors (cdr sent)) ["github"]))
      (should (eq (alist-get 'reconnect (cdr sent)) t)))
    (hermes-connectors-test--goto "Reconnect github")
    (should (hermes-connectors--available-p 'open))
    (should (string-match-p "waiting for browser" (buffer-string)))))

(ert-deftest hermes-connectors-install-approves-and-shows-needs ()
  "A pending MCP install offers Approve; credential needs stay read-only."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request"
     (hermes-connectors-test--payload
      1 nil
      '((name . "linear") (kind . "mcp") (action . "install") (state . "pending"))
      '((name . "notion") (kind . "mcp") (action . "install") (state . "pending")
        (required_env . [((name . "NOTION_TOKEN") (required . t) (secret . t)
                          (default . "") (prompt . "Notion token"))]))))
    (should (string-match-p "Needs Notion token" (buffer-string)))
    (hermes-connectors-test--goto "Install notion")
    (should-not (hermes-connectors--available-p 'approve))
    (should (hermes-connectors--available-p 'decline))
    (hermes-connectors-test--goto "Install linear")
    (let (requests)
      (hermes-connectors-test--capturing requests
        (hermes-connectors-approve))
      (should (equal (alist-get 'result (cdar requests))
                     '((targets . [((name . "linear") (status . "approved"))])))))))

(ert-deftest hermes-connectors-stale-callbacks-cannot-touch-successors ()
  "A reply arriving after the attachment changed leaves the buffer alone."
  (dolist (change '(session profile connection lifetime claim))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "connection.request" (hermes-connectors-test--payload 1 "expired"))
      (hermes-connectors-test--goto "Reconnect github")
      (let (resolve reject guard)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                   (lambda (_client _method _params yes no)
                     (setq resolve yes reject no
                           guard hermes-dashboard-transport-dispatch-guard))))
          (hermes-connectors-retry))
        (pcase change
          ('session (setq hermes-chat--dashboard-active-session-id "successor"))
          ('profile (setq hermes-chat--profile "successor"))
          ('connection (cl-incf (hermes-dashboard-transport-client-generation
                                 client)))
          ('lifetime (setq hermes-chat--lifecycle-generation (list 'successor)))
          ('claim (setq hermes-buffer--owner 'retired)))
        (let ((before (buffer-string)))
          (should-not (funcall guard))
          (funcall resolve (hermes-connectors-test--wire
                            (hermes-connectors-test--payload 9 "connected")))
          (funcall reject "late failure")
          (should (equal before (buffer-string))))))))

(ert-deftest hermes-connectors-rejected-action-keeps-card-retryable ()
  "A refused request shows its reason and leaves the action available."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload 1 "failed"))
    (hermes-connectors-test--goto "Reconnect github")
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_c _m _p _ok reject)
                 (funcall reject "Connector request failed. Try again explicitly."))))
      (hermes-connectors-retry))
    (should (string-match-p "Try again explicitly" (buffer-string)))
    (hermes-connectors-test--goto "Reconnect github")
    (should (hermes-connectors--available-p 'retry))))

(ert-deftest hermes-connectors-disconnect-retires-live-actions ()
  "Socket loss removes links and actions but keeps the last known state."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (should (equal (hermes-connectors-test--buttons) '("Authorize")))
    (hermes-dashboard-transport--dispatch-event
     client '(:type status :status "closed"))
    (should-not (hermes-connectors-test--buttons))
    (should (string-match-p "Reconnect github · work  waiting for browser"
                            (buffer-string)))))

(ert-deftest hermes-connectors-resume-restores-open-operation ()
  "A reattach restores the backend's open operation and rebinds actions."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (let ((record (car hermes-connectors--operations)))
      (setf (plist-get record :pending) t)
      (cl-incf (hermes-dashboard-transport-client-generation client))
      (hermes-chat--dashboard-restore-pending-clarify
       (hermes-connectors-test--wire
        `((pending_connection . ,(hermes-connectors-test--payload 3)))))
      (hermes-connectors-test--goto "Reconnect github")
      (should (hermes-connectors--available-p 'open))
      (should-not (plist-get record :pending)))))

(ert-deftest hermes-connectors-resume-without-operation-closes-card ()
  "A reattach that reports no open operation retires the stale card."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (hermes-chat--dashboard-restore-pending-clarify '((running . t)))
    (should (string-match-p "No longer open in this session" (buffer-string)))
    (should-not (hermes-connectors-test--buttons))
    (hermes-connectors-test--goto "Reconnect github")
    (should-not (hermes-connectors--available-p 'continue))))

(ert-deftest hermes-connectors-directory-operation-continues ()
  "A connector directory request offers only Continue."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request"
     '((op_id . "op-d") (seq . 1) (deadline_at . 1) (timeout_seconds . 1)
       (targets . []) (display . ((suggested . ["gmail" "github"])))))
    (should (string-match-p "Connector directory · suggested gmail, github"
                            (buffer-string)))
    (hermes-connectors-test--goto "Connector directory")
    (should (hermes-connectors--available-p 'continue))
    (should-not (hermes-connectors--available-p 'decline))))

(ert-deftest hermes-connectors-reset-forgets-operations ()
  "Clearing the chat drops cards so later tool rows cannot revive them."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (should hermes-connectors--operations)
    (hermes-chat--reset-transcript)
    (should-not hermes-connectors--operations)))

(ert-deftest hermes-connectors-popup-lists-only-valid-actions ()
  "The card popup shows exactly the actions the current state allows."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "connection.request" (hermes-connectors-test--payload))
    (hermes-connectors-test--goto "Reconnect github")
    (should (eq (key-binding (kbd "o")) #'hermes-connectors-open))
    (should (equal (cl-remove-if-not
                    #'hermes-connectors--available-p
                    '(open wake approve retry decline continue))
                   '(open wake decline continue)))))

(provide 'hermes-connectors-tests)
;;; hermes-connectors-tests.el ends here
