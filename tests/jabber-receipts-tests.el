;;; jabber-receipts-tests.el --- Tests for jabber-receipts  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-receipts)
(require 'jabber-db)

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

;;; Group 4: Header-line update

(ert-deftest jabber-receipts-test-header-line-delivered ()
  "Header-line shows delivered with correct face."
  (with-temp-buffer
    (setq-local jabber-chat-receipt-message "")
    (jabber-receipts--update-header-line "delivered_at" 1700000000)
    (should (string-match-p "delivered" jabber-chat-receipt-message))
    (should (eq 'jabber-chat-delivered
                (get-text-property 0 'face jabber-chat-receipt-message)))))

(ert-deftest jabber-receipts-test-header-line-seen ()
  "Header-line shows seen with correct face."
  (with-temp-buffer
    (setq-local jabber-chat-receipt-message "")
    (jabber-receipts--update-header-line "displayed_at" 1700000000)
    (should (string-match-p "seen" jabber-chat-receipt-message))
    (should (eq 'jabber-chat-seen
                (get-text-property 0 'face jabber-chat-receipt-message)))))

;;; Group 5: EWOC cascade

(ert-deftest jabber-receipts-test-ewoc-cascade-promotes-delivered ()
  "Cascade promotes all prior :delivered nodes to :displayed."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (m1 (list :local (list :id "m1" :status :delivered :timestamp (encode-time 0 0 10 1 1 2026))))
           (m2 (list :local (list :id "m2" :status :delivered :timestamp (encode-time 0 1 10 1 1 2026))))
           (m3 (list :local (list :id "m3" :status :displayed :timestamp (encode-time 0 2 10 1 1 2026))))
           (_n1 (ewoc-enter-last jabber-chat-ewoc m1))
           (_n2 (ewoc-enter-last jabber-chat-ewoc m2))
           (n3 (ewoc-enter-last jabber-chat-ewoc m3)))
      (let ((inhibit-read-only t))
        (jabber-receipts--cascade-displayed n3))
      (should (eq :displayed (plist-get (cadr m1) :status)))
      (should (eq :displayed (plist-get (cadr m2) :status))))))

(ert-deftest jabber-receipts-test-ewoc-cascade-stops-at-displayed ()
  "Cascade stops walking when it hits an already-displayed node."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (m1 (list :local (list :id "m1" :status :delivered :timestamp (encode-time 0 0 10 1 1 2026))))
           (m2 (list :local (list :id "m2" :status :displayed :timestamp (encode-time 0 1 10 1 1 2026))))
           (m3 (list :local (list :id "m3" :status :delivered :timestamp (encode-time 0 2 10 1 1 2026))))
           (m4 (list :local (list :id "m4" :status :displayed :timestamp (encode-time 0 3 10 1 1 2026))))
           (_n1 (ewoc-enter-last jabber-chat-ewoc m1))
           (_n2 (ewoc-enter-last jabber-chat-ewoc m2))
           (_n3 (ewoc-enter-last jabber-chat-ewoc m3))
           (n4 (ewoc-enter-last jabber-chat-ewoc m4)))
      (let ((inhibit-read-only t))
        (jabber-receipts--cascade-displayed n4))
      ;; m3 promoted
      (should (eq :displayed (plist-get (cadr m3) :status)))
      ;; m2 was already displayed, stops there
      (should (eq :displayed (plist-get (cadr m2) :status)))
      ;; m1 NOT promoted (before the already-displayed m2)
      (should (eq :delivered (plist-get (cadr m1) :status))))))

(ert-deftest jabber-receipts-test-ewoc-cascade-skips-foreign ()
  "Cascade skips :foreign nodes and only promotes :local ones."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (m1 (list :local (list :id "m1" :status :delivered :timestamp (encode-time 0 0 10 1 1 2026))))
           (m2 (list :foreign (list :id "m2" :timestamp (encode-time 0 1 10 1 1 2026))))
           (m3 (list :local (list :id "m3" :status :displayed :timestamp (encode-time 0 2 10 1 1 2026))))
           (_n1 (ewoc-enter-last jabber-chat-ewoc m1))
           (_n2 (ewoc-enter-last jabber-chat-ewoc m2))
           (n3 (ewoc-enter-last jabber-chat-ewoc m3)))
      (let ((inhibit-read-only t))
        (jabber-receipts--cascade-displayed n3))
      (should (eq :displayed (plist-get (cadr m1) :status)))
      ;; foreign node untouched
      (should-not (plist-get (cadr m2) :status)))))

(ert-deftest jabber-receipts-test-ewoc-cascade-skips-sent ()
  "Cascade does not promote :sent nodes (not yet delivered)."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (m1 (list :local (list :id "m1" :status :sent :timestamp (encode-time 0 0 10 1 1 2026))))
           (m2 (list :local (list :id "m2" :status :delivered :timestamp (encode-time 0 1 10 1 1 2026))))
           (m3 (list :local (list :id "m3" :status :displayed :timestamp (encode-time 0 2 10 1 1 2026))))
           (_n1 (ewoc-enter-last jabber-chat-ewoc m1))
           (_n2 (ewoc-enter-last jabber-chat-ewoc m2))
           (n3 (ewoc-enter-last jabber-chat-ewoc m3)))
      (let ((inhibit-read-only t))
        (jabber-receipts--cascade-displayed n3))
      (should (eq :displayed (plist-get (cadr m2) :status)))
      ;; :sent stays :sent
      (should (eq :sent (plist-get (cadr m1) :status))))))

;;; Group 6: DB cascade

(ert-deftest jabber-receipts-test-db-cascade-marks-all-delivered ()
  "DB cascade marks all delivered outgoing messages as displayed."
  (let* ((dir (make-temp-file "jabber-receipts-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" dir))
         (jabber-db--connection nil)
         (jabber-backlog-days 3.0)
         (jabber-backlog-number 10))
    (unwind-protect
        (progn
          (jabber-db-ensure-open)
          ;; Store 3 outgoing messages
          (jabber-db-store-message "me@example.com" "them@example.com"
                                  "out" "chat" "msg1" 1000
                                  nil "id-1" nil nil nil nil nil)
          (jabber-db-store-message "me@example.com" "them@example.com"
                                  "out" "chat" "msg2" 1001
                                  nil "id-2" nil nil nil nil nil)
          (jabber-db-store-message "me@example.com" "them@example.com"
                                  "out" "chat" "msg3" 1002
                                  nil "id-3" nil nil nil nil nil)
          ;; Mark all as delivered
          (jabber-db-update-receipt "id-1" "delivered_at" 1010)
          (jabber-db-update-receipt "id-2" "delivered_at" 1011)
          (jabber-db-update-receipt "id-3" "delivered_at" 1012)
          ;; Cascade displayed from msg3
          (jabber-db-cascade-displayed "me@example.com" "them@example.com"
                                       2000 1002)
          ;; All 3 should have displayed_at
          (let ((rows (sqlite-select jabber-db--connection
                                     "SELECT stanza_id, displayed_at FROM message ORDER BY timestamp")))
            (should (= 3 (length rows)))
            (should (= 2000 (cadr (nth 0 rows))))
            (should (= 2000 (cadr (nth 1 rows))))
            (should (= 2000 (cadr (nth 2 rows))))))
      (jabber-db-close)
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest jabber-receipts-test-db-cascade-skips-undelivered ()
  "DB cascade skips messages without delivered_at."
  (let* ((dir (make-temp-file "jabber-receipts-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" dir))
         (jabber-db--connection nil)
         (jabber-backlog-days 3.0)
         (jabber-backlog-number 10))
    (unwind-protect
        (progn
          (jabber-db-ensure-open)
          (jabber-db-store-message "me@example.com" "them@example.com"
                                  "out" "chat" "msg1" 1000
                                  nil "id-1" nil nil nil nil nil)
          (jabber-db-store-message "me@example.com" "them@example.com"
                                  "out" "chat" "msg2" 1001
                                  nil "id-2" nil nil nil nil nil)
          (jabber-db-store-message "me@example.com" "them@example.com"
                                  "out" "chat" "msg3" 1002
                                  nil "id-3" nil nil nil nil nil)
          ;; Only mark msg1 and msg3 as delivered (msg2 undelivered)
          (jabber-db-update-receipt "id-1" "delivered_at" 1010)
          (jabber-db-update-receipt "id-3" "delivered_at" 1012)
          ;; Cascade displayed from msg3
          (jabber-db-cascade-displayed "me@example.com" "them@example.com"
                                       2000 1002)
          (let ((rows (sqlite-select jabber-db--connection
                                     "SELECT stanza_id, displayed_at FROM message ORDER BY timestamp")))
            ;; msg1 displayed (delivered + before ref-ts)
            (should (= 2000 (cadr (nth 0 rows))))
            ;; msg2 NOT displayed (not delivered)
            (should (null (cadr (nth 1 rows))))
            ;; msg3 displayed (delivered + at ref-ts)
            (should (= 2000 (cadr (nth 2 rows))))))
      (jabber-db-close)
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(provide 'jabber-receipts-tests)

;;; jabber-receipts-tests.el ends here
