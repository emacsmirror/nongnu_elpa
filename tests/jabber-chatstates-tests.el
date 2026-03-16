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

(provide 'jabber-chatstates-tests)

;;; jabber-chatstates-tests.el ends here
