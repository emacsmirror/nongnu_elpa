;;; jabber-chatstates-tests.el --- Tests for jabber-chatstates  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-chatstates)

;;; Group 1: Composing notification fix

(ert-deftest jabber-chatstates-test-composing-after-first-send ()
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

(ert-deftest jabber-chatstates-test-no-composing-when-disabled ()
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

(ert-deftest jabber-chatstates-test-send-hook-returns-active ()
  "Send hook returns active element when chatstates-confirm is t."
  (with-temp-buffer
    (setq-local jabber-chatstates-confirm t)
    (setq-local jabber-chatstates-last-state nil)
    (setq-local jabber-chatstates-composing-sent nil)
    (setq-local jabber-chatstates-paused-timer nil)
    (let ((result (jabber-chatstates-when-sending "hello" "id-1")))
      (should result)
      (should (equal (caar result) 'active)))))

(ert-deftest jabber-chatstates-test-send-hook-nil-when-disabled ()
  "Send hook returns nil when chatstates-confirm is nil."
  (with-temp-buffer
    (setq-local jabber-chatstates-confirm nil)
    (setq-local jabber-chatstates-last-state nil)
    (setq-local jabber-chatstates-composing-sent nil)
    (setq-local jabber-chatstates-paused-timer nil)
    (let ((result (jabber-chatstates-when-sending "hello" "id-1")))
      (should-not result))))

;;; Group 2: Inactive and gone states

(ert-deftest jabber-chatstates-test-paused-starts-inactive-timer ()
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

(ert-deftest jabber-chatstates-test-stop-timer-cancels-both ()
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

(ert-deftest jabber-chatstates-test-send-inactive-sends-stanza ()
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

(ert-deftest jabber-chatstates-test-send-gone-sends-stanza ()
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

(ert-deftest jabber-chatstates-test-after-change-cancels-inactive-timer ()
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

(provide 'jabber-chatstates-tests)

;;; jabber-chatstates-tests.el ends here
