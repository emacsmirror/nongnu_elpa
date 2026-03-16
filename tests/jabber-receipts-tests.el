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

;;; Group 2: Incoming receipt handling

(ert-deftest jabber-receipts-test-handle-xep0184-received ()
  "Incoming XEP-0184 received stanza updates delivered_at in DB."
  (let ((updated-id nil)
        (updated-col nil))
    (cl-letf (((symbol-function 'jabber-db-update-receipt)
               (lambda (id col _ts)
                 (setq updated-id id updated-col col)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "me@example.com"))
              ((symbol-function 'jabber-chat-get-buffer)
               (lambda (_from &optional _jc) "*test-chat*")))
      (jabber-receipts--handle-message
       'fake-jc
       '(message ((from . "them@example.com") (type . "chat"))
                 (received ((xmlns . "urn:xmpp:receipts")
                            (id . "msg-001"))))))
    (should (equal updated-id "msg-001"))
    (should (equal updated-col "delivered_at"))))

(ert-deftest jabber-receipts-test-handle-xep0333-displayed ()
  "Incoming XEP-0333 displayed stanza updates displayed_at in DB."
  (let ((updated-id nil)
        (updated-col nil))
    (cl-letf (((symbol-function 'jabber-db-update-receipt)
               (lambda (id col _ts)
                 (setq updated-id id updated-col col)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "me@example.com"))
              ((symbol-function 'jabber-chat-get-buffer)
               (lambda (_from &optional _jc) "*test-chat*")))
      (jabber-receipts--handle-message
       'fake-jc
       '(message ((from . "them@example.com") (type . "chat"))
                 (displayed ((xmlns . "urn:xmpp:chat-markers:0")
                             (id . "msg-002"))))))
    (should (equal updated-id "msg-002"))
    (should (equal updated-col "displayed_at"))))

(ert-deftest jabber-receipts-test-handle-ignores-non-receipt ()
  "Messages without receipt elements are ignored."
  (let ((updated nil))
    (cl-letf (((symbol-function 'jabber-db-update-receipt)
               (lambda (&rest _) (setq updated t)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "me@example.com")))
      (jabber-receipts--handle-message
       'fake-jc
       '(message ((from . "them@example.com") (type . "chat"))
                 (body nil "hello"))))
    (should-not updated)))

;;; Group 3: Sending receipts back

(ert-deftest jabber-receipts-test-send-received-on-request ()
  "Incoming message with <request/> triggers <received/> response."
  (let ((sent-sexp nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent-sexp sexp)))
              ((symbol-function 'jabber-db-update-receipt) #'ignore)
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "me@example.com"))
              ((symbol-function 'jabber-chat-get-buffer)
               (lambda (_from &optional _jc) "*test-chat*")))
      (let ((jabber-chat-send-receipts t))
        (jabber-receipts--handle-message
         'fake-jc
         '(message ((from . "them@example.com") (id . "msg-100") (type . "chat"))
                   (body nil "hello")
                   (request ((xmlns . "urn:xmpp:receipts")))))))
    (should sent-sexp)
    ;; Verify it's a <received/> stanza
    (should (eq 'message (car sent-sexp)))
    (let ((children (cddr sent-sexp)))
      (should (assq 'received children)))))

(ert-deftest jabber-receipts-test-no-received-when-disabled ()
  "No <received/> sent when jabber-chat-send-receipts is nil."
  (let ((sent-sexp nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent-sexp sexp)))
              ((symbol-function 'jabber-db-update-receipt) #'ignore)
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "me@example.com"))
              ((symbol-function 'jabber-chat-get-buffer)
               (lambda (_from &optional _jc) "*test-chat*")))
      (let ((jabber-chat-send-receipts nil))
        (jabber-receipts--handle-message
         'fake-jc
         '(message ((from . "them@example.com") (id . "msg-100") (type . "chat"))
                   (body nil "hello")
                   (request ((xmlns . "urn:xmpp:receipts")))))))
    (should-not sent-sexp)))

(ert-deftest jabber-receipts-test-no-received-without-request ()
  "No <received/> sent for messages without <request/> element."
  (let ((sent-sexp nil))
    (cl-letf (((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent-sexp sexp)))
              ((symbol-function 'jabber-db-update-receipt) #'ignore)
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "me@example.com"))
              ((symbol-function 'jabber-chat-get-buffer)
               (lambda (_from &optional _jc) "*test-chat*")))
      (let ((jabber-chat-send-receipts t))
        (jabber-receipts--handle-message
         'fake-jc
         '(message ((from . "them@example.com") (id . "msg-100") (type . "chat"))
                   (body nil "hello")))))
    (should-not sent-sexp)))

(ert-deftest jabber-receipts-test-markable-sets-pending-id ()
  "Incoming markable message sets pending displayed ID."
  (cl-letf (((symbol-function 'jabber-db-update-receipt) #'ignore)
            ((symbol-function 'jabber-send-sexp-if-connected) #'ignore)
            ((symbol-function 'jabber-connection-bare-jid)
             (lambda (_j) "me@example.com"))
            ((symbol-function 'jabber-chat-get-buffer)
             (lambda (_from &optional _jc) (buffer-name))))
    (with-temp-buffer
      (setq-local jabber-receipts--pending-displayed-id nil)
      (let ((jabber-chat-send-receipts t))
        (jabber-receipts--handle-message
         'fake-jc
         `(message ((from . "them@example.com") (id . "msg-200") (type . "chat"))
                   (body nil "hello")
                   (markable ((xmlns . ,jabber-chat-markers-xmlns))))))
      (should (equal "msg-200" jabber-receipts--pending-displayed-id)))))

(provide 'jabber-receipts-tests)

;;; jabber-receipts-tests.el ends here
