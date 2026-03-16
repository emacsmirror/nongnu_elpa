;;; jabber-receipts-tests.el --- Tests for jabber-receipts  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-receipts)

;;; Group 1: Send hook

(ert-deftest jabber-receipts-test-send-hook-adds-elements ()
  "Send hook appends request and markable elements."
  (let ((result (jabber-receipts--send-hook "hello" "msg-001")))
    (should (assq 'request result))
    (should (assq 'markable result))))

(ert-deftest jabber-receipts-test-send-hook-correct-xmlns ()
  "Send hook elements have correct xmlns attributes."
  (let* ((result (jabber-receipts--send-hook "test" "id-1"))
         (request (assq 'request result))
         (markable (assq 'markable result)))
    (should (string= "urn:xmpp:receipts"
                      (cdr (assq 'xmlns (cadr request)))))
    (should (string= "urn:xmpp:chat-markers:0"
                      (cdr (assq 'xmlns (cadr markable)))))))

(provide 'jabber-receipts-tests)

;;; jabber-receipts-tests.el ends here
