;;; jabber-message-correct-tests.el --- Tests for XEP-0308  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ewoc)

(load (expand-file-name "../lisp/jabber-xml.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-db.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-chatbuffer.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-message-correct.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Test helpers

(defmacro jabber-message-correct-test-with-ewoc (&rest body)
  "Set up a temp buffer with a chat ewoc and hash table, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       ,@body)))

(defmacro jabber-message-correct-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database."
  (declare (indent 0) (debug t))
  `(let* ((jabber-mc-test--dir (make-temp-file "jabber-mc-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-mc-test--dir))
          (jabber-db--connection nil))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-mc-test--dir)
         (delete-directory jabber-mc-test--dir t)))))

;;; Group 1: jabber-message-correct--replace-id

(ert-deftest jabber-message-correct-test-replace-id-nil-for-plain ()
  "Plain message with no <replace> returns nil."
  (let ((stanza '(message ((from . "alice@example.com") (id . "msg-1"))
                          (body () "hello"))))
    (should-not (jabber-message-correct--replace-id stanza))))

(ert-deftest jabber-message-correct-test-replace-id-returns-id ()
  "Correction stanza returns the id from <replace>."
  (let ((stanza `(message ((from . "alice@example.com") (id . "msg-2"))
                          (body () "hello corrected")
                          (replace ((id . "msg-1")
                                    (xmlns . ,jabber-message-correct-xmlns))))))
    (should (equal "msg-1" (jabber-message-correct--replace-id stanza)))))

(ert-deftest jabber-message-correct-test-replace-id-wrong-xmlns ()
  "Element with wrong xmlns is not treated as a correction."
  (let ((stanza '(message ((from . "alice@example.com") (id . "msg-3"))
                          (body () "hello")
                          (replace ((id . "msg-0")
                                    (xmlns . "urn:xmpp:wrong:0"))))))
    (should-not (jabber-message-correct--replace-id stanza))))

;;; Group 2: jabber-message-correct--valid-sender-p

(ert-deftest jabber-message-correct-test-valid-sender-1to1-same ()
  "1:1: same bare JID allows correction."
  (should (jabber-message-correct--valid-sender-p
           "alice@example.com/laptop"
           "alice@example.com/phone"
           nil)))

(ert-deftest jabber-message-correct-test-valid-sender-1to1-different ()
  "1:1: different bare JID rejects correction."
  (should-not (jabber-message-correct--valid-sender-p
               "alice@example.com/laptop"
               "mallory@example.com/phone"
               nil)))

(ert-deftest jabber-message-correct-test-valid-sender-muc-same-full ()
  "MUC: same full JID (nick) allows correction."
  (should (jabber-message-correct--valid-sender-p
           "room@muc.example.com/alice"
           "room@muc.example.com/alice"
           t)))

(ert-deftest jabber-message-correct-test-valid-sender-muc-different-nick ()
  "MUC: different nick rejects correction."
  (should-not (jabber-message-correct--valid-sender-p
               "room@muc.example.com/alice"
               "room@muc.example.com/mallory"
               t)))

;;; Group 3: DB integration

(ert-deftest jabber-message-correct-test-db-correct-message ()
  "jabber-db-correct-message updates body and sets edited=1."
  (jabber-message-correct-test-with-db
    (jabber-db-store-message
     "me@example.com" "friend@example.com" "in" "chat"
     "Original body" (floor (float-time))
     nil "stanza-abc" nil nil nil nil nil)
    (jabber-db-correct-message "stanza-abc" "Corrected body")
    (let* ((rows (sqlite-select jabber-db--connection
                                "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-abc'"))
           (row (car rows)))
      (should (equal "Corrected body" (car row)))
      (should (= 1 (cadr row))))))

(ert-deftest jabber-message-correct-test-db-row-to-plist-edited ()
  "jabber-db--row-to-plist returns :edited t for edited messages."
  (jabber-message-correct-test-with-db
    (let* ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello" ts nil "stanza-edit-1" nil nil nil nil nil)
      (jabber-db-correct-message "stanza-edit-1" "Hello fixed")
      (let* ((rows (jabber-db-backlog "me@example.com" "friend@example.com"
                                      1 (- (float-time) 60)))
             (plist (car rows)))
        (should (plist-get plist :edited))
        (should (equal "Hello fixed" (plist-get plist :body)))))))

(ert-deftest jabber-message-correct-test-db-correct-unknown-id ()
  "jabber-db-correct-message is a no-op for unknown stanza-id."
  (jabber-message-correct-test-with-db
    (jabber-db-correct-message "nonexistent-id" "body")
    (let ((count (caar (sqlite-select jabber-db--connection
                                      "SELECT COUNT(*) FROM message"))))
      (should (= 0 count)))))

(ert-deftest jabber-message-correct-test-db-unedited-returns-nil ()
  "Unedited message returns :edited nil via jabber-db--row-to-plist."
  (jabber-message-correct-test-with-db
    (let* ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello" ts nil "stanza-plain-1" nil nil nil nil nil)
      (let* ((rows (jabber-db-backlog "me@example.com" "friend@example.com"
                                      1 (- (float-time) 60)))
             (plist (car rows)))
        (should-not (plist-get plist :edited))))))

;;; Group 4: ewoc apply correction

(ert-deftest jabber-message-correct-test-apply-updates-ewoc ()
  "jabber-message-correct--apply updates body and edited in the ewoc node."
  (jabber-message-correct-test-with-ewoc
    (let ((msg (list :id "orig-1"
                     :from "alice@example.com/phone"
                     :body "original"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :foreign msg)))
    (cl-letf (((symbol-function 'jabber-db-correct-message) #'ignore))
      (jabber-message-correct--apply
       "orig-1" "corrected" "alice@example.com/laptop" nil (current-buffer)))
    (let* ((node (jabber-chat-ewoc-find-by-id "orig-1"))
           (msg (cadr (ewoc-data node))))
      (should (equal "corrected" (plist-get msg :body)))
      (should (plist-get msg :edited)))))

(ert-deftest jabber-message-correct-test-apply-rejects-wrong-sender ()
  "jabber-message-correct--apply rejects correction from wrong sender."
  (jabber-message-correct-test-with-ewoc
    (let ((msg (list :id "orig-2"
                     :from "alice@example.com/phone"
                     :body "original"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :foreign msg)))
    (cl-letf (((symbol-function 'jabber-db-correct-message) #'ignore))
      (jabber-message-correct--apply
       "orig-2" "evil" "mallory@example.com/x" nil (current-buffer)))
    (let* ((node (jabber-chat-ewoc-find-by-id "orig-2"))
           (msg (cadr (ewoc-data node))))
      (should (equal "original" (plist-get msg :body)))
      (should-not (plist-get msg :edited)))))

(provide 'jabber-message-correct-tests)

;;; jabber-message-correct-tests.el ends here
