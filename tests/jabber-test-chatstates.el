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

(defun jabber-test-chatstates--thread-message (from type state thread-id)
  "Return a threaded message from FROM with TYPE and chat STATE."
  `(message ((from . ,from)
             (type . ,type))
            (thread () ,thread-id)
            (,state ((xmlns . ,jabber-chatstates-xmlns)))))

(defun jabber-test-chatstates--thread-body-message (from type thread-id)
  "Return a threaded body message from FROM with TYPE."
  `(message ((from . ,from)
             (type . ,type))
            (thread () ,thread-id)
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

(defun jabber-test-chatstates--create-thread-buffer (parent type)
  "Create a test thread with PARENT and message TYPE."
  (cl-letf (((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.org"))
            ((symbol-function 'jabber-message-thread-find-buffer)
             (lambda (&rest _) nil))
            ((symbol-function 'jabber-chat-mode-setup) #'ignore)
            ((symbol-function 'jabber-buffer-registry-register) #'ignore)
            ((symbol-function 'jabber-db-thread-backlog) #'ignore))
    (jabber-message-thread-create-buffer
     'fake-jc
     (if (equal type "groupchat")
         "room@conference.example"
       "alice@example.org")
     type "thread-1" nil parent)))

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

(ert-deftest jabber-test-chatstates-direct-thread-composing-keeps-thread ()
  "A direct thread composing notification includes its ThreadID."
  (let (sent)
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc stanza) (setq sent stanza)))
              ((symbol-function 'jabber-chatstates-kick-timer) #'ignore))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested t)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-message-thread-id "thread-1")
        (setq-local jabber-message-thread-parent-id "parent-1")
        (setq-local jabber-point-insert (point-min))
        (setq-local jabber-chatstates-composing-sent nil)
        (insert "draft")
        (jabber-chatstates-after-change)))
    (should
     (equal sent
            `(message ((to . "them@example.com") (type . "chat"))
                      (thread ((parent . "parent-1")) "thread-1")
                      (composing ((xmlns . ,jabber-chatstates-xmlns))))))))

(ert-deftest jabber-test-chatstates-muc-thread-composing-keeps-thread ()
  "A MUC thread composing notification targets the room and ThreadID."
  (let (sent)
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc stanza) (setq sent stanza)))
              ((symbol-function 'jabber-chatstates-kick-timer) #'ignore))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested t)
        (setq-local jabber-group "room@conference.example")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-message-thread-id "thread-1")
        (setq-local jabber-message-thread-parent-id nil)
        (setq-local jabber-point-insert (point-min))
        (setq-local jabber-chatstates-composing-sent nil)
        (insert "draft")
        (jabber-chatstates-after-change)))
    (should
     (equal sent
            `(message ((to . "room@conference.example")
                       (type . "groupchat"))
                      (thread () "thread-1")
                      (composing ((xmlns . ,jabber-chatstates-xmlns))))))))

(ert-deftest jabber-test-chatstates-new-direct-thread-inherits-hooks ()
  "A new direct thread inherits negotiated chat-state sending."
  (let ((parent (generate-new-buffer " *jabber-direct-thread-parent*"))
        thread)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local jabber-chatstates-requested t)
            (add-hook 'post-command-hook
                      #'jabber-chatstates-after-change nil t))
          (setq thread
                (jabber-test-chatstates--create-thread-buffer parent "chat"))
          (with-current-buffer thread
            (should (memq #'jabber-chatstates-after-change post-command-hook))
            (should (memq #'jabber-chatstates-send-gone kill-buffer-hook))))
      (when (buffer-live-p thread)
        (with-current-buffer thread
          (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t))
        (kill-buffer thread))
      (kill-buffer parent))))

(ert-deftest jabber-test-chatstates-new-direct-thread-inherits-opt-out ()
  "A new direct thread inherits its parent's negotiated opt-out."
  (let ((parent (generate-new-buffer " *jabber-opt-out-parent*"))
        thread sent)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local jabber-chatstates-requested nil))
          (setq thread
                (jabber-test-chatstates--create-thread-buffer parent "chat"))
          (with-current-buffer thread
            (setq-local jabber-buffer-connection 'fake-jc)
            (setq-local jabber-point-insert (point-min))
            (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
                       (lambda (&rest _) (setq sent t))))
              (should-not jabber-chatstates-requested)
              (should-not
               (jabber-chatstates-when-sending "message" "message-1"))
              (insert "draft")
              (jabber-chatstates-after-change)
              (jabber-chatstates-send-gone)
              (should-not sent))
            (should-not
             (memq #'jabber-chatstates-after-change post-command-hook))
            (should-not
             (memq #'jabber-chatstates-send-gone kill-buffer-hook))))
      (when (buffer-live-p thread)
        (kill-buffer thread))
      (kill-buffer parent))))

(ert-deftest jabber-test-chatstates-new-muc-thread-enables-sending ()
  "A new MUC thread enables chat states without a gone hook."
  (let ((parent (generate-new-buffer " *jabber-muc-thread-parent*"))
        thread)
    (unwind-protect
        (progn
          (setq thread
                (jabber-test-chatstates--create-thread-buffer
                 parent "groupchat"))
          (with-current-buffer thread
            (should (memq #'jabber-chatstates-after-change post-command-hook))
            (should-not
             (memq #'jabber-chatstates-send-gone kill-buffer-hook))))
      (when (buffer-live-p thread)
        (kill-buffer thread))
      (kill-buffer parent))))

(ert-deftest jabber-test-chatstates-muc-thread-kill-cancels-timers ()
  "Killing a MUC thread cancels state timers without sending gone."
  (let ((parent (generate-new-buffer " *jabber-muc-timer-parent*"))
        thread timer sent)
    (unwind-protect
        (progn
          (setq thread
                (jabber-test-chatstates--create-thread-buffer
                 parent "groupchat"))
          (with-current-buffer thread
            (setq timer (run-with-timer 3600 nil #'ignore))
            (setq-local jabber-chatstates-paused-timer timer))
          (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
                     (lambda (&rest _) (setq sent t))))
            (kill-buffer thread))
          (should-not sent)
          (should-not (memq timer timer-list)))
      (when (and timer (memq timer timer-list))
        (cancel-timer timer))
      (when (buffer-live-p thread)
        (kill-buffer thread))
      (kill-buffer parent))))

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

(ert-deftest jabber-test-chatstates-timer-keeps-originating-thread ()
  "A paused timer sends from the thread buffer that started it."
  (let ((origin (generate-new-buffer " *jabber-chatstate-origin*"))
        callback
        callback-args
        sent)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest args)
                     (setq callback function
                           callback-args args)
                     'fake-timer))
                  ((symbol-function 'jabber-send-sexp-if-connected)
                   (lambda (_jc stanza) (setq sent stanza))))
          (with-current-buffer origin
            (setq-local jabber-chatstates-confirm t)
            (setq-local jabber-chatstates-requested t)
            (setq-local jabber-chatting-with "them@example.com")
            (setq-local jabber-buffer-connection 'fake-jc)
            (setq-local jabber-message-thread-id "thread-1")
            (setq-local jabber-message-thread-parent-id nil)
            (setq-local jabber-chatstates-paused-timer nil)
            (setq-local jabber-chatstates-inactive-timer nil)
            (jabber-chatstates-kick-timer))
          (with-temp-buffer
            (apply callback callback-args))
          (should
           (equal sent
                  `(message ((to . "them@example.com") (type . "chat"))
                            (thread () "thread-1")
                            (paused
                             ((xmlns . ,jabber-chatstates-xmlns)))))))
      (kill-buffer origin))))

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

(ert-deftest jabber-test-chatstates-direct-thread-gone-keeps-thread ()
  "A direct thread gone notification includes its ThreadID."
  (let (sent)
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc stanza) (setq sent stanza))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested t)
        (setq-local jabber-chatting-with "them@example.com")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-message-thread-id "thread-1")
        (setq-local jabber-message-thread-parent-id nil)
        (setq-local jabber-chatstates-paused-timer nil)
        (setq-local jabber-chatstates-inactive-timer nil)
        (jabber-chatstates-send-gone)))
    (should
     (equal sent
            `(message ((to . "them@example.com") (type . "chat"))
                      (thread () "thread-1")
                      (gone ((xmlns . ,jabber-chatstates-xmlns))))))))

(ert-deftest jabber-test-chatstates-muc-thread-does-not-send-gone ()
  "Closing a MUC thread does not send a gone notification."
  (let (sent)
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc stanza) (setq sent stanza))))
      (with-temp-buffer
        (setq-local jabber-chatstates-confirm t)
        (setq-local jabber-chatstates-requested t)
        (setq-local jabber-group "room@conference.example")
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-message-thread-id "thread-1")
        (setq-local jabber-chatstates-paused-timer nil)
        (setq-local jabber-chatstates-inactive-timer nil)
        (jabber-chatstates-send-gone)))
    (should-not sent)))

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
        (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                   (lambda (_kind _group) muc-buffer))
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

(ert-deftest jabber-test-chatstates-direct-thread-routes-exclusively ()
  "A direct threaded state updates and enables only its thread buffer."
  (let ((parent (generate-new-buffer " *jabber-chatstate-parent*"))
        (thread (generate-new-buffer " *jabber-chatstate-thread*")))
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local jabber-chatting-with "alice@example.org")
            (setq-local jabber-message-thread-session-id "thread-1")
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
          (with-current-buffer thread
            (setq-local jabber-chatting-with "alice@example.org")
            (setq-local jabber-message-thread-id "thread-1")
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
          (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                     (lambda (_from _jc) (buffer-name parent)))
                    ((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.org"))
                    ((symbol-function 'jabber-message-thread-find-buffer)
                     (lambda (_account _peer _type _thread-id) thread)))
            (jabber-handle-incoming-message-chatstates
             'fake-jc
             (jabber-test-chatstates--thread-message
              "alice@example.org/resource" "chat" 'composing "thread-1")))
          (with-current-buffer parent
            (should-not (jabber-test-chatstates--ewoc-data))
            (should-not
             (memq #'jabber-chatstates-after-change post-command-hook)))
          (with-current-buffer thread
            (should (eq jabber-chatstates-last-state 'composing))
            (should (equal (jabber-test-chatstates--ewoc-data)
                           '((:typing "alice@example.org is typing..."))))
            (should (memq #'jabber-chatstates-after-change post-command-hook))
            (should (memq #'jabber-chatstates-send-gone kill-buffer-hook))))
      (dolist (buffer (list parent thread))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t))
          (kill-buffer buffer))))))

(ert-deftest jabber-test-chatstates-parent-gone-retires-session-thread ()
  "Incoming gone forces the parent chat to use a new session thread."
  (with-temp-buffer
    (rename-buffer " *jabber-parent-gone-chatstate*" t)
    (let ((parent (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore))
          stored)
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chatting-with "alice@example.org")
      (setq-local jabber-message-thread-session-id "session-old")
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name parent)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.org"))
                ((symbol-function 'jabber-message-thread-find-buffer)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-message-thread--generate-id)
                 (lambda () "session-new"))
                ((symbol-function 'jabber-db-set-chat-thread)
                 (lambda (&rest args) (push args stored))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--thread-message
          "alice@example.org/resource" "chat" 'gone "session-old"))
        (should-not jabber-message-thread-session-id)
        (should
         (equal (jabber-chat--session-send-hook "reply" "message-1")
                '((thread nil "session-new")))))
      (should
       (equal (reverse stored)
              '(("me@example.org" "alice@example.org" nil)
                ("me@example.org" "alice@example.org" "session-new"))))
      (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t))))

(ert-deftest jabber-test-chatstates-disabled-thread-routes-to-parent ()
  "A state-only threaded stanza uses the parent when buffers are disabled."
  (let ((parent (generate-new-buffer " *jabber-disabled-state-parent*"))
        (stale-thread (generate-new-buffer " *jabber-disabled-state-thread*"))
        (jabber-message-thread-use-buffers nil))
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local jabber-chatting-with "alice@example.org")
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
          (with-current-buffer stale-thread
            (setq-local jabber-message-thread-id "thread-1")
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
          (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                     (lambda (&rest _) (buffer-name parent)))
                    ((symbol-function 'jabber-message-thread-find-buffer)
                     (lambda (&rest _)
                       (ert-fail "Looked up a stale thread buffer"))))
            (jabber-handle-incoming-message-chatstates
             'fake-jc
             (jabber-test-chatstates--thread-message
              "alice@example.org/resource" "chat" 'composing "thread-1")))
          (with-current-buffer parent
            (should (eq jabber-chatstates-last-state 'composing))
            (should (equal (jabber-test-chatstates--ewoc-data)
                           '((:typing "alice@example.org is typing...")))))
          (with-current-buffer stale-thread
            (should-not jabber-chatstates-last-state)
            (should-not (jabber-test-chatstates--ewoc-data))))
      (dolist (buffer (list parent stale-thread))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t))
          (kill-buffer buffer))))))

(ert-deftest jabber-test-chatstates-direct-thread-gone-renews-thread-id ()
  "Incoming gone retires the direct thread ID before further typing."
  (let ((parent (generate-new-buffer " *jabber-gone-parent*"))
        (thread (generate-new-buffer " *jabber-gone-thread*"))
        (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
        content-elements sent)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local jabber-chatting-with "alice@example.org"))
          (with-current-buffer thread
            (setq-local jabber-chatting-with "alice@example.org")
            (setq-local jabber-message-thread-id "thread-1")
            (setq-local jabber-message-thread-type "chat")
            (setq-local jabber-message-thread-peer "alice@example.org")
            (setq-local jabber-buffer-connection 'fake-jc)
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
            (setq-local jabber-point-insert (point-min))
            (jabber-buffer-registry-register
             'thread
             '("me@example.org" "alice@example.org" "chat" "thread-1")))
          (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                     (lambda (_from _jc) (buffer-name parent)))
                    ((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.org"))
                    ((symbol-function 'jabber-message-thread--generate-id)
                     (lambda () "thread-2"))
                    ((symbol-function 'jabber-send-sexp-if-connected)
                     (lambda (_jc stanza) (setq sent stanza))))
            (jabber-handle-incoming-message-chatstates
             'fake-jc
             (jabber-test-chatstates--thread-message
              "alice@example.org/resource" "chat" 'gone "thread-1"))
            (with-current-buffer thread
              (should (equal jabber-message-thread-id "thread-2"))
              (should (equal jabber-message-thread-parent-id "thread-1"))
              (goto-char (point-max))
              (insert "reply")
              (jabber-chatstates-after-change)
              (setq content-elements
                    (append
                     (jabber-message-thread--send-hook "reply" "message-1")
                     (jabber-chatstates-when-sending
                      "reply" "message-1")))))
          (should
           (equal sent
                  `(message
                    ((to . "alice@example.org") (type . "chat"))
                    (thread ((parent . "thread-1")) "thread-2")
                    (composing
                     ((xmlns . ,jabber-chatstates-xmlns))))))
          (should
           (equal content-elements
                  `((thread ((parent . "thread-1")) "thread-2")
                    (active ((xmlns . ,jabber-chatstates-xmlns))))))
          (should-not
           (jabber-message-thread-find-buffer
            "me@example.org" "alice@example.org" "chat" "thread-1"))
          (should
           (eq (jabber-message-thread-find-buffer
                "me@example.org" "alice@example.org" "chat" "thread-2")
               thread)))
      (when (buffer-live-p thread)
        (with-current-buffer thread
          (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t)))
      (dolist (buffer (list parent thread))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-test-chatstates-direct-state-without-type-still-routes ()
  "A direct state without a type attribute keeps legacy routing."
  (with-temp-buffer
    (let ((parent (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatting-with "alice@example.org")
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name parent))))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         `(message ((from . "alice@example.org/resource"))
                   (composing
                    ((xmlns . ,jabber-chatstates-xmlns))))))
      (should (eq jabber-chatstates-last-state 'composing))
      (should (memq #'jabber-chatstates-after-change post-command-hook))
      (remove-hook 'kill-buffer-hook #'jabber-chatstates-send-gone t))))

(ert-deftest jabber-test-chatstates-threaded-error-disables-owner-only ()
  "A threaded error disables its chat or groupchat owner, not its parent."
  (dolist (owner-type '("chat" "groupchat"))
    (let ((parent (generate-new-buffer " *jabber-error-parent*"))
          (thread (generate-new-buffer " *jabber-error-thread*")))
      (unwind-protect
          (progn
            (dolist (buffer (list parent thread))
              (with-current-buffer buffer
                (setq-local jabber-chatstates-requested t)
                (add-hook 'post-command-hook
                          #'jabber-chatstates-after-change nil t)
                (add-hook 'kill-buffer-hook
                          #'jabber-chatstates-send-gone nil t)))
            (with-current-buffer thread
              (setq-local jabber-message-thread-id "thread-1"))
            (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                       (lambda (_jc) "me@example.org"))
                      ((symbol-function 'jabber-message-thread-find-buffer)
                       (lambda (_account _peer type _thread-id)
                         (and (equal type owner-type) thread))))
              (jabber-handle-incoming-message-chatstates
               'fake-jc
               `(message
                 ((from . "peer@example.org/resource") (type . "error"))
                 (thread () "thread-1")
                 (error ((type . "cancel"))))))
            (with-current-buffer parent
              (should jabber-chatstates-requested)
              (should
               (memq #'jabber-chatstates-after-change post-command-hook)))
            (with-current-buffer thread
              (should-not jabber-chatstates-requested)
              (should-not
               (memq #'jabber-chatstates-after-change post-command-hook))
              (should-not
               (memq #'jabber-chatstates-send-gone kill-buffer-hook))))
        (dolist (buffer (list parent thread))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (remove-hook 'kill-buffer-hook
                           #'jabber-chatstates-send-gone t))
            (kill-buffer buffer)))))))

(ert-deftest jabber-test-chatstates-muc-thread-routes-exclusively ()
  "A MUC threaded state updates and enables only its thread buffer."
  (let ((parent (generate-new-buffer " *jabber-muc-chatstate-parent*"))
        (thread (generate-new-buffer " *jabber-muc-chatstate-thread*")))
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local jabber-group "room@conference.example")
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
          (with-current-buffer thread
            (setq-local jabber-group "room@conference.example")
            (setq-local jabber-message-thread-id "thread-1")
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
          (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                     (lambda (_kind _key) parent))
                    ((symbol-function 'jabber-muc-find-buffer)
                     (lambda (_group _jc) parent))
                    ((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.org"))
                    ((symbol-function 'jabber-message-thread-find-buffer)
                     (lambda (_account _peer _type _thread-id) thread))
                    ((symbol-function 'jabber-muc-nickname) #'ignore))
            (jabber-handle-incoming-message-chatstates
             'fake-jc
             (jabber-test-chatstates--thread-message
              "room@conference.example/alice"
              "groupchat" 'composing "thread-1")))
          (with-current-buffer parent
            (should-not jabber-chatstates--muc-composers)
            (should-not (jabber-test-chatstates--ewoc-data)))
          (with-current-buffer thread
            (should (equal jabber-chatstates--muc-composers '("alice")))
            (should (equal (jabber-test-chatstates--ewoc-data)
                           '((:typing "alice is typing..."))))
            (should (memq #'jabber-chatstates-after-change post-command-hook))
            (should-not
             (memq #'jabber-chatstates-send-gone kill-buffer-hook))))
      (dolist (buffer (list parent thread))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-test-chatstates-muc-thread-ignores-gone ()
  "A MUC gone state does not change thread composer or send state."
  (with-temp-buffer
    (let ((thread (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-group "room@conference.example")
      (setq-local jabber-message-thread-id "thread-1")
      (setq-local jabber-chatstates--muc-composers '("alice"))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.org"))
                ((symbol-function 'jabber-message-thread-find-buffer)
                 (lambda (&rest _) thread))
                ((symbol-function 'jabber-muc-nickname) #'ignore))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--thread-message
          "room@conference.example/alice"
          "groupchat" 'gone "thread-1")))
      (should (equal jabber-chatstates--muc-composers '("alice")))
      (should-not
       (memq #'jabber-chatstates-after-change post-command-hook)))))

(ert-deftest jabber-test-chatstates-muc-ignores-state-without-nick ()
  "A MUC state from the bare room JID does not add a composer."
  (with-temp-buffer
    (let ((muc-buffer (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-group "room@conference.example")
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group _jc) muc-buffer))
                ((symbol-function 'jabber-muc-nickname) #'ignore))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--message
          "room@conference.example" "groupchat" 'composing)))
      (should-not jabber-chatstates--muc-composers)
      (should-not (jabber-test-chatstates--ewoc-data)))))

(ert-deftest jabber-test-chatstates-session-thread-state-routes-to-parent ()
  "A state for the direct chat session updates its parent buffer."
  (with-temp-buffer
    (rename-buffer " *jabber-session-thread-chatstate*" t)
    (let ((parent (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatting-with "alice@example.org")
      (setq-local jabber-message-thread-session-id "session-42")
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name parent)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.org"))
                ((symbol-function 'jabber-message-thread-find-buffer)
                 (lambda (&rest _) nil)))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--thread-message
          "alice@example.org/resource" "chat" 'composing "session-42")))
      (should (eq jabber-chatstates-last-state 'composing))
      (should (equal (jabber-test-chatstates--ewoc-data)
                     '((:typing "alice@example.org is typing...")))))))

(ert-deftest jabber-test-chatstates-unknown-thread-state-stays-out-of-parent ()
  "A state-only unknown thread does not update the parent buffer."
  (with-temp-buffer
    (rename-buffer " *jabber-unknown-thread-chatstate*" t)
    (let ((parent (current-buffer))
          (jabber-chat-ewoc (ewoc-create #'ignore)))
      (setq-local jabber-chatting-with "alice@example.org")
      (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                 (lambda (_from _jc) (buffer-name parent)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.org"))
                ((symbol-function 'jabber-message-thread-find-buffer)
                 (lambda (&rest _) nil)))
        (jabber-handle-incoming-message-chatstates
         'fake-jc
         (jabber-test-chatstates--thread-message
          "alice@example.org/resource" "chat" 'composing "unknown")))
      (should-not jabber-chatstates-last-state)
      (should-not (jabber-test-chatstates--ewoc-data)))))

(ert-deftest jabber-test-chatstates-thread-body-clears-thread-state ()
  "A threaded body message clears composing in its displayed thread."
  (let ((parent (generate-new-buffer " *jabber-body-parent*"))
        (thread (generate-new-buffer " *jabber-body-thread*")))
    (unwind-protect
        (progn
          (with-current-buffer thread
            (setq-local jabber-chatting-with "alice@example.org")
            (setq-local jabber-message-thread-id "thread-1")
            (setq-local jabber-chatstates-last-state 'composing)
            (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
            (setq-local jabber-chatstates--ewoc-node
                        (ewoc-enter-last
                         jabber-chat-ewoc
                         '(:typing "alice@example.org is typing..."))))
          (cl-letf (((symbol-function 'jabber-chat-get-buffer)
                     (lambda (_from _jc) (buffer-name parent)))
                    ((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.org"))
                    ((symbol-function 'jabber-message-thread-display-target)
                     (lambda (&rest _) thread)))
            (jabber-handle-incoming-message-chatstates
             'fake-jc
             (jabber-test-chatstates--thread-body-message
              "alice@example.org/resource" "chat" "thread-1")))
          (with-current-buffer thread
            (should-not jabber-chatstates-last-state)
            (should-not jabber-chatstates--ewoc-node)
            (should-not (jabber-test-chatstates--ewoc-data))))
      (kill-buffer parent)
      (kill-buffer thread))))

(ert-deftest jabber-test-chatstates-groupchat-composing-routes-to-muc-buffer ()
  "Incoming groupchat composing updates the room buffer by bare JID."
  (let ((entered nil)
        (seen-context nil))
    (with-temp-buffer
      (let ((muc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (group jc)
                     (setq seen-context (list group jc))
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
          (should
           (equal seen-context
                  '("room@conference.example" fake-jc)))
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
        (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                   (lambda (_kind _group) muc-buffer))
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
      (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                 (lambda (_kind _group) muc-buffer))
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
      (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                 (lambda (_kind _group) muc-buffer))
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
      (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                 (lambda (_kind _group) muc-buffer))
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
      (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                 (lambda (_kind _group)
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
        (cl-letf (((symbol-function 'jabber-buffer-registry-find)
                   (lambda (_kind _group) muc-buffer))
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
                  ((symbol-function 'jabber-buffer-registry-find)
                   (lambda (_kind _group)
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
