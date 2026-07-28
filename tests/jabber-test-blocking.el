;;; jabber-test-blocking.el --- Tests for XEP-0191 blocking  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-blocking)

(ert-deftest jabber-test-blocking-apply-block-push ()
  "A block push adds its JIDs without duplicates."
  (let ((query `(block ((xmlns . ,jabber-blocking-xmlns))
                       (item ((jid . "a@example.org")))
                       (item ((jid . "b@example.org"))))))
    (should (equal (jabber-blocking--apply-push
                    '("a@example.org" "old@example.org") query)
                   '("a@example.org" "old@example.org" "b@example.org")))))

(ert-deftest jabber-test-blocking-apply-unblock-push ()
  "An unblock push removes listed JIDs or clears the whole list."
  (let ((current '("a@example.org" "b@example.org")))
    (should
     (equal
      (jabber-blocking--apply-push
       current
       `(unblock ((xmlns . ,jabber-blocking-xmlns))
                 (item ((jid . "a@example.org")))))
      '("b@example.org")))
    (should-not
     (jabber-blocking--apply-push
      current `(unblock ((xmlns . ,jabber-blocking-xmlns)))))))

(ert-deftest jabber-test-blocking-processes-valid-push ()
  "A valid server push updates state and receives an IQ result."
  (let* ((state-data (list :username "me"
                           :server "example.org"
                           :resource "emacs"
                           :blocking-list '("old@example.org")))
         (xml `(iq ((type . "set") (id . "push-1"))
                   (block ((xmlns . ,jabber-blocking-xmlns))
                          (item ((jid . "new@example.org"))))))
         sent)
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) state-data))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq sent args))))
      (jabber-blocking--process-push 'fake-jc xml))
    (should (equal (plist-get state-data :blocking-list)
                   '("old@example.org" "new@example.org")))
    (should (equal (nth 2 sent) "result"))
    (should (equal (car (last sent)) "push-1"))))

(ert-deftest jabber-test-blocking-rejects-foreign-push ()
  "A foreign entity cannot change the local blocklist state."
  (let* ((state-data (list :username "me"
                           :server "example.org"
                           :resource "emacs"
                           :blocking-list '("old@example.org")))
         (xml `(iq ((type . "set") (id . "push-1")
                    (from . "attacker@example.net"))
                   (block ((xmlns . ,jabber-blocking-xmlns))
                          (item ((jid . "new@example.org"))))))
         sent)
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) state-data))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq sent args))))
      (jabber-blocking--process-push 'fake-jc xml))
    (should (equal (plist-get state-data :blocking-list)
                   '("old@example.org")))
    (should-not sent)))

(provide 'jabber-test-blocking)

;;; jabber-test-blocking.el ends here
