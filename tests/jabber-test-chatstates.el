;;; jabber-test-chatstates.el --- Tests for jabber-chatstates  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0085 Chat State Notifications.

;;; Code:

(require 'ert)
(require 'jabber-chatstates)
(require 'jabber-reactions)

(defun jabber-test-chatstates--message (from type state)
  "Return a message sexp from FROM with TYPE and chat STATE."
  `(message ((from . ,from)
             (type . ,type))
            (,state ((xmlns . ,jabber-chatstates-xmlns)))))

(defun jabber-test-chatstates--plain-message (from type)
  "Return a message sexp from FROM with TYPE, body, and no chat state."
  `(message ((from . ,from)
             (type . ,type))
            (body nil "hello")))

(defun jabber-test-chatstates--reaction-message (from type)
  "Return a bodyless reaction message sexp from FROM with TYPE."
  `(message ((from . ,from)
             (type . ,type))
            (reactions ((xmlns . "urn:xmpp:reactions:0")
                        (id . "target-1"))
                       (reaction nil "👍"))))

(defun jabber-test-chatstates--reaction-fallback-message (from type)
  "Return a reaction message from FROM with TYPE and fallback text."
  `(message ((from . ,from)
             (type . ,type))
            (body nil "> quoted\n👍")
            (reactions ((xmlns . "urn:xmpp:reactions:0")
                        (id . "target-1"))
                       (reaction nil "👍"))
            (fallback ((xmlns . "urn:xmpp:fallback:0")
                       (for . "urn:xmpp:reactions:0"))
                      (body ((start . "0") (end . "10"))))))

(defun jabber-test-chatstates--ewoc-data ()
  "Return the current EWOC data in display order."
  (let (data)
    (ewoc-map (lambda (item) (push item data)) jabber-chat-ewoc)
    (nreverse data)))

;;; Group 1: Composing notification fix

(ert-deftest jabber-test-chatstates-composing-after-first-send ()
  "Composing notification works after the first message send.
The first-time gating used to set jabber-chatstates-requested to
nil after the first message, breaking subsequent composing detection."
  (let ((sent-states nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (push sexp sent-states))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-point-insert (point-min))
        (setq-local jabber-chatstates-composing-sent nil)
        (setq-local jabber-chatstates-paused-timer nil)
        ;; Simulate sending the first message (triggers when-sending)
        (jabber-chatstates-when-sending "hello" "id-1")
        ;; Now simulate typing a second message
        (setq sent-states nil)
        (goto-char (point-max))
        (insert "world")
        (jabber-chatstates-after-change)
        (should sent-states)))))

(ert-deftest jabber-test-chatstates-no-composing-when-disabled ()
  "Composing notification is not sent when jabber-chatstates-confirm is nil."
  (let ((sent-states nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (push sexp sent-states))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm nil)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-point-insert (point-min))
        (setq-local jabber-chatstates-composing-sent nil)
        (goto-char (point-max))
        (insert "hello")
        (jabber-chatstates-after-change)
        (should-not sent-states)))))

(ert-deftest jabber-test-chatstates-no-composing-when-not-requested ()
  "Composing notification is not sent after negotiation opt-out."
  (let ((sent-states nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (push sexp sent-states))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested nil)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-point-insert (point-min))
        (setq-local jabber-chatstates-composing-sent nil)
        (goto-char (point-max))
        (insert "hello")
        (jabber-chatstates-after-change)
        (should-not sent-states)))))

(ert-deftest jabber-test-chatstates-send-hook-returns-active ()
  "Send hook returns active element when chatstates-confirm is t."
  (with-temp-buffer
    (setq-local jabber-chatstates-confirm t)
    (setq-local jabber-chatstates-last-state nil)
    (setq-local jabber-chatstates-composing-sent nil)
    (setq-local jabber-chatstates-paused-timer nil)
    (let ((result (jabber-chatstates-when-sending "hello" "id-1")))
      (should result)
      (should (equal (caar result) 'active)))))

(ert-deftest jabber-test-chatstates-send-hook-nil-when-disabled ()
  "Send hook returns nil when chatstates-confirm is nil."
  (with-temp-buffer
    (setq-local jabber-chatstates-confirm nil)
    (setq-local jabber-chatstates-last-state nil)
    (setq-local jabber-chatstates-composing-sent nil)
    (setq-local jabber-chatstates-paused-timer nil)
    (let ((result (jabber-chatstates-when-sending "hello" "id-1")))
      (should-not result))))

(ert-deftest jabber-test-chatstates-send-hook-nil-when-not-requested ()
  "Send hook returns nil after chat state negotiation is disabled."
  (with-temp-buffer
    (setq-local jabber-chatstates-confirm t)
    (setq-local jabber-chatstates-requested nil)
    (setq-local jabber-chatstates-last-state nil)
    (setq-local jabber-chatstates-composing-sent nil)
    (setq-local jabber-chatstates-paused-timer nil)
    (let ((result (jabber-chatstates-when-sending "hello" "id-1")))
      (should-not result))))

;;; Group 2: Inactive and gone states

(ert-deftest jabber-test-chatstates-paused-starts-inactive-timer ()
  "Sending paused starts a 30s timer for inactive."
  (cl-letf (((symbol-function 'jabber-send-sexp-if-connected) #'ignore))
    (with-temp-buffer
      (setq-local jabber-chatstates-confirm t)
      (setq-local jabber-chatting-with "them@example.com")
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chatstates-composing-sent t)
      (setq-local jabber-chatstates-inactive-timer nil)
      (jabber-chatstates-send-paused)
      (should jabber-chatstates-inactive-timer)
      (cancel-timer jabber-chatstates-inactive-timer))))

(ert-deftest jabber-test-chatstates-paused-not-sent-when-not-requested ()
  "send-paused is a no-op after negotiation opt-out."
  (let ((sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested nil)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-chatstates-inactive-timer nil)
        (jabber-chatstates-send-paused)
        (should-not sent)
        (should-not jabber-chatstates-inactive-timer)))))

(ert-deftest jabber-test-chatstates-stop-timer-cancels-both ()
  "stop-timer cancels both paused and inactive timers."
  (with-temp-buffer
    (setq-local jabber-chatstates-paused-timer
                (run-with-timer 999 nil #'ignore))
    (setq-local jabber-chatstates-inactive-timer
                (run-with-timer 999 nil #'ignore))
    (jabber-chatstates-stop-timer)
    ;; Timers should be cancelled (not in timer-list)
    (should-not (memq jabber-chatstates-paused-timer timer-list))
    (should-not (memq jabber-chatstates-inactive-timer timer-list))))

(ert-deftest jabber-test-chatstates-send-inactive-sends-stanza ()
  "send-inactive sends an inactive chat state stanza."
  (let ((sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (jabber-chatstates-send-inactive)
        (should sent)
        (should (assq 'inactive (cddr sent)))))))

(ert-deftest jabber-test-chatstates-send-gone-sends-stanza ()
  "send-gone sends a gone chat state stanza."
  (let ((sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-chatstates-paused-timer nil)
        (setq-local jabber-chatstates-inactive-timer nil)
        (jabber-chatstates-send-gone)
        (should sent)
        (should (assq 'gone (cddr sent)))))))

(ert-deftest jabber-test-chatstates-send-gone-not-sent-when-not-requested ()
  "send-gone is a no-op after negotiation opt-out."
  (let ((sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested nil)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-chatstates-paused-timer nil)
        (setq-local jabber-chatstates-inactive-timer nil)
        (jabber-chatstates-send-gone)
        (should-not sent)))))

(ert-deftest jabber-test-chatstates-after-change-cancels-inactive-timer ()
  "Typing again cancels the inactive timer."
  (cl-letf (((symbol-function 'jabber-send-sexp-if-connected) #'ignore))
    (with-temp-buffer
      (setq-local jabber-chatstates-confirm t)
      (setq-local jabber-chatting-with "them@example.com")
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-point-insert (point-min))
      (setq-local jabber-chatstates-composing-sent nil)
      (setq-local jabber-chatstates-paused-timer nil)
      (setq-local jabber-chatstates-inactive-timer
                  (run-with-timer 999 nil #'ignore))
      (goto-char (point-max))
      (insert "hello")
      (jabber-chatstates-after-change)
      (should-not (memq jabber-chatstates-inactive-timer timer-list)))))

;;; Group 3: MUC typing helpers

(ert-deftest jabber-test-chatstates-format-no-muc-composers ()
  (should-not (jabber-chatstates--format-muc-composers nil)))

(ert-deftest jabber-test-chatstates-format-one-muc-composer ()
  (should (string= (jabber-chatstates--format-muc-composers '("alice"))
                   "alice is typing...")))

(ert-deftest jabber-test-chatstates-format-multiple-muc-composers ()
  (should (string= (jabber-chatstates--format-muc-composers '("alice" "bob"))
                   "alice, bob are typing...")))

(ert-deftest jabber-test-chatstates-muc-composers-adds-on-composing ()
  (should (equal (jabber-chatstates--muc-composers-for-state
                  '("alice") "bob" 'composing)
                 '("alice" "bob"))))

(ert-deftest jabber-test-chatstates-muc-composers-does-not-duplicate ()
  (should (equal (jabber-chatstates--muc-composers-for-state
                  '("alice" "bob") "alice" 'composing)
                 '("alice" "bob"))))

(ert-deftest jabber-test-chatstates-muc-composers-removes-on-non-composing ()
  (should (equal (jabber-chatstates--muc-composers-for-state
                  '("alice" "bob" "carol") "bob" 'paused)
                 '("alice" "carol"))))

(ert-deftest jabber-test-chatstates-muc-composers-removal-is-idempotent ()
  (should (equal (jabber-chatstates--muc-composers-for-state
                  '("alice" "bob") "carol" 'active)
                 '("alice" "bob"))))

(ert-deftest jabber-test-chatstates-direct-send-clears-typing-node ()
  "Local direct-chat send clears the peer typing node."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (setq-local jabber-chatstates-confirm t)
      (setq-local jabber-chatstates--ewoc-node node)
      (jabber-chatstates-when-sending "hello" "id-1")
      (should-not jabber-chatstates--ewoc-node)
      (should-not (jabber-test-chatstates--ewoc-data)))))

(ert-deftest jabber-test-chatstates-muc-send-preserves-remote-composers ()
  "Local MUC send preserves remote composers and their typing node."
  (let ((deleted nil))
    (with-temp-buffer
      (setq-local jabber-chatstates-confirm t)
      (setq-local jabber-chatstates--muc-composers '("alice"))
      (setq-local jabber-chatstates--ewoc-node 'node)
      (cl-letf (((symbol-function 'jabber-chat-ewoc-delete)
                 (lambda (node) (setq deleted node))))
        (jabber-chatstates-when-sending "hello" "id-1")
        (should-not deleted)
        (should (equal jabber-chatstates--muc-composers '("alice")))
        (should (eq jabber-chatstates--ewoc-node 'node))))))

(ert-deftest jabber-test-chatstates-clear-typing-forgets-stale-node ()
  "Clearing a stale typing node forgets it without deleting again."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (ewoc-delete jabber-chat-ewoc node)
      (setq-local jabber-chatstates--ewoc-node node)
      (should-not (jabber-chatstates--live-ewoc-node-p node))
      (jabber-chatstates--clear-typing)
      (should-not jabber-chatstates--ewoc-node))))

(ert-deftest jabber-test-chatstates-live-node-error-is-reported ()
  "EWOC lookup errors are reported and treated as stale nodes."
  (let (logged)
    (cl-letf (((symbol-function 'ewoc-location)
               (lambda (_node) (error "bad node")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq logged (apply #'format format-string args)))))
      (should-not (jabber-chatstates--live-ewoc-node-p 'bad-node)))
    (should (string-match-p "stale chat state ewoc node: bad node"
                            logged))))

(ert-deftest jabber-test-chatstates-direct-send-forgets-stale-typing-node ()
  "Local direct-chat send ignores stale typing nodes."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (ewoc-delete jabber-chat-ewoc node)
      (setq-local jabber-chatstates-confirm t)
      (setq-local jabber-chatstates--ewoc-node node)
      (jabber-chatstates-when-sending "hello" "id-1")
      (should-not jabber-chatstates--ewoc-node))))

(ert-deftest jabber-test-chatstates-muc-reinsert-after-stale-node ()
  "A stale MUC typing node does not block bottom reinsertion."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (ewoc-delete jabber-chat-ewoc node)
      (ewoc-enter-last jabber-chat-ewoc '(:muc-message "alice: hello"))
      (setq-local jabber-chatstates--muc-composers '("bob"))
      (setq-local jabber-chatstates--ewoc-node node)
      (jabber-chatstates--update-muc-ewoc)
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:muc-message "alice: hello")
                       (:typing "bob is typing..."))))
      (should-not (eq jabber-chatstates--ewoc-node node)))))

(ert-deftest jabber-test-chatstates-muc-ewoc-update-reinserts-node ()
  "Updating existing MUC typing text reinserts the node at bottom."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (first (ewoc-enter-last jabber-chat-ewoc '(:muc-notice "joined")))
           (old-node (ewoc-enter-last jabber-chat-ewoc
                                      '(:typing "alice is typing..."))))
      (setq-local jabber-chatstates--muc-composers '("alice" "bob"))
      (setq-local jabber-chatstates--ewoc-node old-node)
      (jabber-chatstates--update-muc-ewoc)
      (should (equal (ewoc-data first) '(:muc-notice "joined")))
      (should-not (eq jabber-chatstates--ewoc-node old-node))
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:muc-notice "joined")
                       (:typing "alice, bob are typing...")))))))

(ert-deftest jabber-test-chatstates-muc-clear-nick-deletes-typing-node ()
  "Clearing the last MUC composer deletes the typing node."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (setq-local jabber-chatstates--muc-composers '("alice"))
      (setq-local jabber-chatstates--ewoc-node node)
      (jabber-chatstates--muc-clear-nick "alice")
      (should-not jabber-chatstates--muc-composers)
      (should-not jabber-chatstates--ewoc-node)
      (should-not (jabber-test-chatstates--ewoc-data)))))

(ert-deftest jabber-test-chatstates-muc-clear-nick-reinserts-typing-node ()
  "Clearing one MUC composer reinserts the node for remaining composers."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (old-node (ewoc-enter-last jabber-chat-ewoc
                                      '(:typing "alice, bob are typing..."))))
      (setq-local jabber-chatstates--muc-composers '("alice" "bob"))
      (setq-local jabber-chatstates--ewoc-node old-node)
      (jabber-chatstates--muc-clear-nick "alice")
      (should (equal jabber-chatstates--muc-composers '("bob")))
      (should-not (eq jabber-chatstates--ewoc-node old-node))
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:typing "bob is typing...")))))))

(ert-deftest jabber-test-chatstates-groupchat-message-keeps-typing-at-bottom ()
  "Plain groupchat message cleanup moves remaining typing below the message."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (old-node (ewoc-enter-last jabber-chat-ewoc
                                      '(:typing "alice, bob are typing..."))))
      (setq-local jabber-chatstates--muc-composers '("alice" "bob"))
      (setq-local jabber-chatstates--ewoc-node old-node)
      (ewoc-enter-last jabber-chat-ewoc '(:muc-message "alice: hello"))
      (let ((muc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (_group) muc-buffer))
                  ((symbol-function 'jabber-muc-nickname) #'ignore))
          (jabber-handle-incoming-message-chatstates
           'fake-jc
           (jabber-test-chatstates--plain-message
            "room@conference.example/alice" "groupchat"))))
      (should (equal jabber-chatstates--muc-composers '("bob")))
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:muc-message "alice: hello")
                       (:typing "bob is typing...")))))))

(ert-deftest jabber-test-chatstates-muc-leave-cleanup-keeps-typing-at-bottom ()
  "Leave cleanup can remove, print notice, and reinsert typing at bottom."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (old-node (ewoc-enter-last jabber-chat-ewoc
                                      '(:typing "alice, bob are typing..."))))
      (setq-local jabber-chatstates--muc-composers '("alice" "bob"))
      (setq-local jabber-chatstates--ewoc-node old-node)
      (jabber-chatstates--muc-remove-nick "alice")
      (jabber-chatstates--delete-typing-node)
      (ewoc-enter-last jabber-chat-ewoc '(:muc-notice "alice has left"))
      (jabber-chatstates--muc-reinsert-typing)
      (should (equal jabber-chatstates--muc-composers '("bob")))
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:muc-notice "alice has left")
                       (:typing "bob is typing...")))))))

;;; Group 4: Incoming MUC routing

(ert-deftest jabber-test-chatstates-groupchat-composing-routes-to-muc-buffer ()
  "Incoming groupchat composing updates the room buffer by bare JID."
  (let ((entered nil)
        (seen-group nil))
    (with-temp-buffer
      (let ((muc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (group)
                     (setq seen-group group)
                     muc-buffer))
                  ((symbol-function 'jabber-muc-nickname) #'ignore)
                  ((symbol-function 'jabber-chat-ewoc-enter)
                   (lambda (data)
                     (setq entered data)
                     'node)))
          (jabber-handle-incoming-message-chatstates
           'fake-jc
           (jabber-test-chatstates--message
            "room@conference.example/alice" "groupchat" 'composing))
          (should (string= seen-group "room@conference.example"))
          (should (equal jabber-chatstates--muc-composers '("alice")))
          (should (equal entered '(:typing "alice is typing..."))))))))

(ert-deftest jabber-test-chatstates-groupchat-active-removes-from-muc-buffer ()
  "Incoming groupchat active removes the occupant from the room buffer."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (setq-local jabber-chatstates--muc-composers '("alice"))
      (setq-local jabber-chatstates--ewoc-node node)
      (let ((muc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (_group) muc-buffer))
                  ((symbol-function 'jabber-muc-nickname) #'ignore))
          (jabber-handle-incoming-message-chatstates
           'fake-jc
           (jabber-test-chatstates--message
            "room@conference.example/alice" "groupchat" 'active))
          (should-not jabber-chatstates--muc-composers)
          (should-not jabber-chatstates--ewoc-node)
          (should-not (jabber-test-chatstates--ewoc-data)))))))

(ert-deftest jabber-test-chatstates-groupchat-message-clears-composing ()
  "Incoming groupchat message without chatstate clears occupant typing."
  (with-temp-buffer
    (let ((muc-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group) muc-buffer))
                ((symbol-function 'jabber-muc-nickname) #'ignore))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "room@conference.example/alice" "groupchat" 'composing))
        (should (equal jabber-chatstates--muc-composers '("alice")))
        (should (equal (jabber-test-chatstates--ewoc-data)
                       '((:typing "alice is typing..."))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--plain-message
          "room@conference.example/alice" "groupchat"))
        (should-not jabber-chatstates--muc-composers)
        (should-not jabber-chatstates--ewoc-node)
        (should-not (jabber-test-chatstates--ewoc-data))))))

(ert-deftest jabber-test-chatstates-groupchat-reaction-preserves-composing ()
  "Incoming groupchat reaction-only stanza does not clear occupant typing."
  (with-temp-buffer
    (let ((muc-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group) muc-buffer))
                ((symbol-function 'jabber-muc-nickname) #'ignore))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "room@conference.example/alice" "groupchat" 'composing))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--reaction-message
          "room@conference.example/alice" "groupchat"))
        (should (equal jabber-chatstates--muc-composers '("alice")))
        (should (equal (jabber-test-chatstates--ewoc-data)
                       '((:typing "alice is typing..."))))))))

(ert-deftest jabber-test-chatstates-groupchat-reaction-fallback-preserves-composing ()
  "Incoming groupchat reaction fallback body does not clear occupant typing."
  (with-temp-buffer
    (let ((muc-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group) muc-buffer))
                ((symbol-function 'jabber-muc-nickname) #'ignore))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "room@conference.example/alice" "groupchat" 'composing))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--reaction-fallback-message
          "room@conference.example/alice" "groupchat"))
        (should (equal jabber-chatstates--muc-composers '("alice")))
        (should (equal (jabber-test-chatstates--ewoc-data)
                       '((:typing "alice is typing..."))))))))

(ert-deftest jabber-test-chatstates-groupchat-self-nick-is-ignored ()
  "Incoming groupchat state from our nick refreshes without mutating composers."
  (let ((find-called nil)
        (entered nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group)
                   (setq find-called t)
                   (current-buffer)))
                ((symbol-function 'jabber-muc-nickname)
                 (lambda (_group _jc) "alice"))
                ((symbol-function 'jabber-chat-ewoc-enter)
                 (lambda (data)
                   (setq entered data)
                   'node)))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "room@conference.example/alice" "groupchat" 'composing))
        (should find-called)
        (should-not jabber-chatstates--muc-composers)
        (should-not entered)))))

(ert-deftest jabber-test-chatstates-self-groupchat-message-keeps-typing-at-bottom ()
  "Self groupchat echo refreshes remote typing below the echoed message."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (old-node (ewoc-enter-last jabber-chat-ewoc
                                      '(:typing "bob is typing..."))))
      (setq-local jabber-chatstates--muc-composers '("bob"))
      (setq-local jabber-chatstates--ewoc-node old-node)
      (ewoc-enter-last jabber-chat-ewoc '(:muc-message "alice: hello"))
      (let ((muc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (_group) muc-buffer))
                  ((symbol-function 'jabber-muc-nickname)
                   (lambda (_group _jc) "alice")))
          (jabber-handle-incoming-message-chatstates
           'fake-jc
           (jabber-test-chatstates--plain-message
            "room@conference.example/alice" "groupchat"))))
      (should (equal jabber-chatstates--muc-composers '("bob")))
      (should-not (eq jabber-chatstates--ewoc-node old-node))
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:muc-message "alice: hello")
                       (:typing "bob is typing...")))))))

(ert-deftest jabber-test-chatstates-direct-chat-keeps-direct-routing ()
  "Incoming direct chat states keep using the direct chat buffer lookup."
  (let ((direct-called nil)
        (muc-called nil)
        (entered nil))
    (with-temp-buffer
      (rename-buffer " *jabber-direct-chatstates-test*" t)
      (setq-local jabber-chatting-with "alice@example.org/resource")
      (let ((chat-buffer (current-buffer)))
        (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                   (lambda (from jc)
                     (setq direct-called (list from jc))
                     (buffer-name chat-buffer)))
                  ((symbol-function 'jabber-muc-find-buffer)
                   (lambda (_group)
                     (setq muc-called t)
                     nil))
                  ((symbol-function 'jabber-chat-ewoc-enter)
                   (lambda (data)
                     (setq entered data)
                     'node)))
          (jabber-handle-incoming-message-chatstates
           'fake-jc
           (jabber-test-chatstates--message
            "alice@example.org/resource" "chat" 'composing))
          (should (equal direct-called '("alice@example.org/resource" fake-jc)))
          (should-not muc-called)
          (should (eq jabber-chatstates-last-state 'composing))
          (should (equal entered '(:typing "alice@example.org is typing..."))))))))

(ert-deftest jabber-test-chatstates-direct-reaction-fallback-preserves-composing ()
  "Incoming direct reaction fallback body does not clear peer typing."
  (with-temp-buffer
    (rename-buffer " *jabber-direct-chatstates-reaction-fallback-test*" t)
    (let ((chat-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatting-with "alice@example.org/resource")
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name chat-buffer))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "alice@example.org/resource" "chat" 'composing))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--reaction-fallback-message
          "alice@example.org/resource" "chat"))
        (should (eq jabber-chatstates-last-state 'composing))
        (should (equal (jabber-test-chatstates--ewoc-data)
                       '((:typing "alice@example.org is typing..."))))))))

(ert-deftest jabber-test-chatstates-direct-message-clears-composing ()
  "Incoming direct message without chatstate clears peer typing."
  (with-temp-buffer
    (rename-buffer " *jabber-direct-chatstates-message-clears-test*" t)
    (let ((chat-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatting-with "alice@example.org/resource")
      (setq-local jabber-chatstates-requested t)
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name chat-buffer))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "alice@example.org/resource" "chat" 'composing))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--plain-message
          "alice@example.org/resource" "chat"))
        (should-not jabber-chatstates-last-state)
        (should-not jabber-chatstates-requested)
        (should-not jabber-chatstates--ewoc-node)
        (should-not (jabber-test-chatstates--ewoc-data))))))

(ert-deftest jabber-test-chatstates-direct-message-removes-send-hooks ()
  "A direct body reply without chatstate removes local send hooks."
  (with-temp-buffer
    (rename-buffer " *jabber-direct-chatstates-hook-opt-out-test*" t)
    (let ((chat-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatstates-requested t)
      (add-hook 'post-command-hook #'jabber-chatstates-after-change nil t)
      (add-hook 'kill-buffer-hook #'jabber-chatstates-send-gone nil t)
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name chat-buffer))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--plain-message
          "alice@example.org/resource" "chat"))
        (should-not jabber-chatstates-requested)
        (should-not (memq #'jabber-chatstates-after-change post-command-hook))
        (should-not (memq #'jabber-chatstates-send-gone kill-buffer-hook))))))

(ert-deftest jabber-test-chatstates-direct-active-forgets-stale-node ()
  "Incoming direct active clears a stale typing node without error."
  (with-temp-buffer
    (rename-buffer " *jabber-direct-chatstates-active-stale-test*" t)
    (let* ((chat-buffer (current-buffer))
           (jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (ewoc-delete jabber-chat-ewoc node)
      (setq-local jabber-chatstates--ewoc-node node)
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name chat-buffer))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "alice@example.org/resource" "chat" 'active))
        (should-not jabber-chatstates--ewoc-node)
        (should-not (jabber-test-chatstates--ewoc-data))))))

(ert-deftest jabber-test-chatstates-direct-composing-replaces-stale-node ()
  "Incoming direct composing inserts a fresh node after stale node cleanup."
  (with-temp-buffer
    (rename-buffer " *jabber-direct-chatstates-composing-stale-test*" t)
    (let* ((chat-buffer (current-buffer))
           (jabber-chat-ewoc (ewoc-create #'ignore))
           (node (ewoc-enter-last jabber-chat-ewoc
                                  '(:typing "alice is typing..."))))
      (ewoc-delete jabber-chat-ewoc node)
      (setq-local jabber-chatting-with "alice@example.org/resource")
      (setq-local jabber-chatstates--ewoc-node node)
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name chat-buffer))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "alice@example.org/resource" "chat" 'composing))
        (should jabber-chatstates--ewoc-node)
        (should-not (eq jabber-chatstates--ewoc-node node))
        (should (equal (jabber-test-chatstates--ewoc-data)
                       '((:typing "alice@example.org is typing..."))))))))

(ert-deftest jabber-test-chatstates-direct-composing-keeps-live-node ()
  "Repeated incoming direct composing preserves the live typing node."
  (with-temp-buffer
    (rename-buffer " *jabber-direct-chatstates-composing-live-test*" t)
    (let ((chat-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatting-with "alice@example.org/resource")
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name chat-buffer))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "alice@example.org/resource" "chat" 'composing))
        (let ((node jabber-chatstates--ewoc-node))
          (jabber-handle-incoming-message-chatstates
           'fake-jc
           (jabber-test-chatstates--message
            "alice@example.org/resource" "chat" 'composing))
          (should (eq jabber-chatstates--ewoc-node node))
          (should (equal (jabber-test-chatstates--ewoc-data)
                         '((:typing "alice@example.org is typing...")))))))))

(provide 'jabber-test-chatstates)

;;; jabber-test-chatstates.el ends here
