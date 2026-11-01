;;; vm-send-live-init-test.el --- The send pass's fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The bookkeeping `vm-send-live-init.el' does around the sending tests, which
;; is testable without sending anything.  The tests that do send are in
;; vm-send-live-test.el and run only in the send pass.

;;; Code:

(require 'vm-test-init)
(require 'vm-send-live-init)

;;; Sending every message before awaiting any, issue #930

(ert-deftest vm-send-live-init-test-every-message-is-sent-once ()
  "`vm-send-live-sent' sends every message the first time it is asked.
Delivery takes a minute or two and the tests awaited one message each, so
sending them one at a time cost one delivery per test.  Sent together at the
first ask they cost one between them."
  (let* ((sent nil)
         (vm-send-live--sent nil)
         (vm-send-live-messages
          (list (cons 'one (lambda () (push "one" sent) (list :subject "s1")))
                (cons 'two (lambda () (push "two" sent) (list :subject "s2"))))))
    (unwind-protect
        (progn
          ;; asking for one of them sends both
          (should (equal (plist-get (vm-send-live-sent 'two) :subject) "s2"))
          (should (equal (sort (copy-sequence sent) #'string<) '("one" "two")))
          ;; and asking again sends nothing more
          (should (equal (plist-get (vm-send-live-sent 'one) :subject) "s1"))
          (should (equal (length sent) 2))
          ;; a tag nothing sent is an error, not a nil the test would go on
          ;; to await for ten minutes
          (should-error (vm-send-live-sent 'three)))
      (remove-hook 'kill-emacs-hook #'vm-send-live-sweep))))

(ert-deftest vm-send-live-init-test-the-sweep-is-armed-when-mail-goes-out ()
  "The sweep is on `kill-emacs-hook' from the moment anything has been sent.
These put real mail in a real mailbox, and it has to come out whether the
tests passed, failed or stopped.  It cannot run after each test any more:
every message is sent before the first one is awaited, so a sweep then would
take the others with it."
  (let ((vm-send-live--sent nil)
        (vm-send-live-messages
         (list (cons 'one (lambda () (list :subject "s1")))))
        (kill-emacs-hook nil))
    (should-not (memq #'vm-send-live-sweep kill-emacs-hook))
    (vm-send-live-sent 'one)
    (should (memq #'vm-send-live-sweep kill-emacs-hook))))

(ert-deftest vm-send-live-init-test-the-record-survives-a-test ()
  "`vm-send-live--sent' is exempt from the per-test isolation.
Every VM variable is wound back between tests.  Wound back, this one would
leave each test dispatching again, and the deliveries the batch exists to
save would all come back."
  (should (memq 'vm-send-live--sent vm-test-isolation-exceptions)))

(provide 'vm-send-live-init-test)

;;; vm-send-live-init-test.el ends here
