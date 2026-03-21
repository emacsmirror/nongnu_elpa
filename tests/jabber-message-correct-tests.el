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

;; jabber-chat and jabber-muc are needed for Groups 5 and 9.
;; jabber-muc requires jabber-chat, and both need this stub constant.
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")
(require 'jabber-chat)
(require 'jabber-muc)

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
  "jabber-message-correct--apply updates body and edited in the ewoc node, and writes DB."
  (jabber-message-correct-test-with-ewoc
    (let ((msg (list :id "orig-1"
                     :from "alice@example.com/phone"
                     :body "original"
                     :timestamp (current-time)))
          db-called)
      (jabber-chat-ewoc-enter (list :foreign msg))
      (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
                 (lambda (_id) "alice@example.com/phone"))
                ((symbol-function 'jabber-db-correct-message)
                 (lambda (_id _body) (setq db-called t))))
        (jabber-message-correct--apply
         "orig-1" "corrected" "alice@example.com/laptop" nil (current-buffer)))
      (let* ((node (jabber-chat-ewoc-find-by-id "orig-1"))
             (msg (cadr (ewoc-data node))))
        (should (equal "corrected" (plist-get msg :body)))
        (should (plist-get msg :edited))
        (should db-called)))))

(ert-deftest jabber-message-correct-test-apply-rejects-wrong-sender ()
  "jabber-message-correct--apply rejects correction from wrong sender."
  (jabber-message-correct-test-with-ewoc
    (let ((msg (list :id "orig-2"
                     :from "alice@example.com/phone"
                     :body "original"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :foreign msg)))
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message) #'ignore))
      (jabber-message-correct--apply
       "orig-2" "evil" "mallory@example.com/x" nil (current-buffer)))
    (let* ((node (jabber-chat-ewoc-find-by-id "orig-2"))
           (msg (cadr (ewoc-data node))))
      (should (equal "original" (plist-get msg :body)))
      (should-not (plist-get msg :edited)))))

(ert-deftest jabber-message-correct-test-apply-nil-buffer-db-update ()
  "With nil buffer and valid sender, jabber-db-correct-message is called."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "orig-3" "corrected" "alice@example.com/laptop" nil nil))
    (should db-called)))

(ert-deftest jabber-message-correct-test-apply-nil-buffer-wrong-sender-no-db ()
  "With nil buffer and wrong sender, jabber-db-correct-message is not called."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "orig-4" "evil" "mallory@example.com/x" nil nil))
    (should-not db-called)))

(ert-deftest jabber-message-correct-test-apply-unknown-id-no-db ()
  "When stanza-id not in DB, correction is dropped without DB write."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) nil))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "unknown-id" "body" "alice@example.com" nil nil))
    (should-not db-called)))

(ert-deftest jabber-message-correct-test-apply-muc-same-nick ()
  "MUC: correction from same nick is accepted."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "room@muc.example.com/alice"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "muc-orig-1" "corrected" "room@muc.example.com/alice" t nil))
    (should db-called)))

(ert-deftest jabber-message-correct-test-apply-muc-different-nick ()
  "MUC: correction from different nick is rejected."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "room@muc.example.com/alice"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "muc-orig-2" "evil" "room@muc.example.com/mallory" t nil))
    (should-not db-called)))

(ert-deftest jabber-message-correct-test-apply-outgoing-carbon ()
  "Outgoing message: carbon correction from same account is accepted."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "me@example.com"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "out-orig-1" "corrected" "me@example.com/other-device" nil nil))
    (should db-called)))

;;; Group 5: MAM / delayed stanza guard

(ert-deftest jabber-message-correct-test-replace-id-present-in-delayed-stanza ()
  "A delayed stanza that contains <replace> still exposes its replace-id.
This confirms that callers are responsible for the delayed guard,
not jabber-message-correct--replace-id itself."
  ;; The stanza is delayed but structurally valid as a correction.
  ;; --replace-id must return the id so the caller can choose to skip it.
  (let ((stanza `(message ((from . "room@muc.example.com/alice")
                           (id . "corr-1")
                           (type . "groupchat"))
                          (body () "fixed text")
                          (replace ((id . "orig-1")
                                    (xmlns . ,jabber-message-correct-xmlns)))
                          (delay ((xmlns . "urn:xmpp:delay")
                                  (stamp . "2025-01-15T10:30:00Z"))))))
    (should (equal "orig-1" (jabber-message-correct--replace-id stanza)))))

(ert-deftest jabber-message-correct-test-history-message-p-detects-delay ()
  "jabber-muc--history-message-p returns non-nil for stanzas with <delay>."
  (let ((stanza '(message ((from . "room@muc.example.com/alice")
                           (type . "groupchat"))
                          (body () "old message")
                          (delay ((xmlns . "urn:xmpp:delay")
                                  (stamp . "2025-01-15T10:30:00Z"))))))
    (should (jabber-muc--history-message-p stanza))))

(ert-deftest jabber-message-correct-test-history-message-p-nil-for-live ()
  "jabber-muc--history-message-p returns nil for live stanzas without <delay>."
  (let ((stanza '(message ((from . "room@muc.example.com/alice")
                           (type . "groupchat"))
                          (body () "live message"))))
    (should-not (jabber-muc--history-message-p stanza))))

(ert-deftest jabber-message-correct-test-delayed-stanza-skipped-by-muc-dispatch ()
  "jabber-muc-process-message must not apply corrections from delayed stanzas.
Regression guard: a delayed correction arriving in MUC history replay
must not mutate the DB or the ewoc.
This test documents an UNIMPLEMENTED guard and will fail until it is added."
  :expected-result :failed
  ;; Once the fix lands (guard on jabber-muc--history-message-p before calling
  ;; jabber-message-correct--apply in jabber-muc-process-message), this test
  ;; must be changed to :expected-result :passed.
  (let ((apply-called nil)
        (stanza `(message ((from . "room@muc.example.com/alice")
                           (id . "corr-2")
                           (type . "groupchat"))
                          (body () "corrected text")
                          (replace ((id . "orig-2")
                                    (xmlns . ,jabber-message-correct-xmlns)))
                          (delay ((xmlns . "urn:xmpp:delay")
                                  (stamp . "2025-01-15T10:30:00Z"))))))
    (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) t))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc xml) xml))
              ((symbol-function 'jabber-muc-find-buffer) (lambda (_) nil))
              ((symbol-function 'jabber-muc--display-message) #'ignore)
              ((symbol-function 'jabber-message-correct--apply)
               (lambda (&rest _) (setq apply-called t))))
      (jabber-muc-process-message nil stanza))
    (should-not apply-called)))

(ert-deftest jabber-message-correct-test-mam-syncing-skipped-by-chat-dispatch ()
  "jabber-process-chat must not apply corrections while jabber-mam--syncing is t.
Regression guard: a MAM catch-up stanza carrying <replace> must not
be treated as a live edit.
This test documents an UNIMPLEMENTED guard and will fail until it is added."
  :expected-result :failed
  ;; Once the fix lands (guard on jabber-mam--syncing before calling
  ;; jabber-message-correct--apply in jabber-process-chat), change to :passed.
  (let ((apply-called nil)
        (jabber-mam--syncing t)
        (stanza `(message ((from . "alice@example.com/phone")
                           (id . "corr-3")
                           (type . "chat"))
                          (body () "corrected")
                          (replace ((id . "orig-3")
                                    (xmlns . ,jabber-message-correct-xmlns))))))
    (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) nil))
              ((symbol-function 'jabber-chat--unwrap-carbon)
               (lambda (_jc xml) (cons xml nil)))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc xml) xml))
              ((symbol-function 'jabber-message-correct--apply)
               (lambda (&rest _) (setq apply-called t))))
      (jabber-process-chat nil stanza))
    (should-not apply-called)))

;;; Group 6: chained corrections

(ert-deftest jabber-message-correct-test-chained-correction-id-unchanged ()
  "jabber-correct-last-message re-uses the original :id after a first correction.
The ewoc node's :id must not be updated when a correction is applied
locally, so that a subsequent C-c C-e will reference the original id,
not the correction's stanza-id."
  (jabber-message-correct-test-with-ewoc
    ;; Insert a sent message with id \"orig-chain-1\"
    (let ((msg (list :id "orig-chain-1"
                     :from "me@example.com"
                     :body "orignal"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    ;; Simulate first correction arriving from DB (apply with valid sender)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message) #'ignore))
      (jabber-message-correct--apply
       "orig-chain-1" "original" "alice@example.com/phone" nil (current-buffer)))
    ;; After the apply the node's :id must still be the original id
    (let* ((node (jabber-chat-ewoc-find-by-id "orig-chain-1"))
           (msg (cadr (ewoc-data node))))
      (should (equal "orig-chain-1" (plist-get msg :id)))
      (should (plist-get msg :edited)))))

(ert-deftest jabber-message-correct-test-correct-last-uses-original-id ()
  "jabber-correct-last-message sends replace referencing original id after edit.
After --apply updates :body/:edited but leaves :id alone,
jabber-correct-last-message must pick up the original id."
  (jabber-message-correct-test-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    ;; Insert an already-edited sent message (simulating post-first-correction state)
    (let ((msg (list :id "orig-chain-2"
                     :from "me@example.com"
                     :body "first correction"
                     :edited t
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    (let (sent-replace-id)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc stanza)
                   (let* ((replace (car (jabber-xml-get-children stanza 'replace))))
                     (setq sent-replace-id
                           (jabber-xml-get-attribute replace 'id)))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "second correction"))
                ((symbol-function 'jabber-db-correct-message) #'ignore))
        (jabber-correct-last-message))
      (should (equal "orig-chain-2" sent-replace-id)))))

;;; Group 7: nil and empty body edge cases

(ert-deftest jabber-message-correct-test-nil-body-db-write ()
  "Correction with nil body writes nil to the DB (no crash)."
  ;; A correction stanza lacking <body> passes nil through --apply.
  ;; jabber-db-correct-message must not error on nil body.
  (jabber-message-correct-test-with-db
    (jabber-db-store-message
     "me@example.com" "friend@example.com" "in" "chat"
     "Original" (floor (float-time))
     nil "stanza-nil-body" nil nil nil nil nil)
    ;; Should not signal -- nil body is a valid (if odd) correction
    (jabber-db-correct-message "stanza-nil-body" nil)
    (let* ((rows (sqlite-select jabber-db--connection
                                "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-nil-body'"))
           (row (car rows)))
      ;; edited flag must be set even for nil body
      (should (= 1 (cadr row))))))

(ert-deftest jabber-message-correct-test-nil-body-ewoc-update ()
  "Correction with nil body updates ewoc :body to nil without error."
  (jabber-message-correct-test-with-ewoc
    (let ((msg (list :id "nil-body-orig"
                     :from "alice@example.com/phone"
                     :body "original"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :foreign msg)))
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message) #'ignore))
      (jabber-message-correct--apply
       "nil-body-orig" nil "alice@example.com/phone" nil (current-buffer)))
    (let* ((node (jabber-chat-ewoc-find-by-id "nil-body-orig"))
           (msg (cadr (ewoc-data node))))
      (should-not (plist-get msg :body))
      (should (plist-get msg :edited)))))

(ert-deftest jabber-message-correct-test-empty-body-accepted ()
  "Correction with empty string body is accepted and written."
  (jabber-message-correct-test-with-ewoc
    (let ((msg (list :id "empty-body-orig"
                     :from "alice@example.com/phone"
                     :body "original text"
                     :timestamp (current-time)))
          db-called)
      (jabber-chat-ewoc-enter (list :foreign msg))
      (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
                 (lambda (_id) "alice@example.com/phone"))
                ((symbol-function 'jabber-db-correct-message)
                 (lambda (_id _body) (setq db-called t))))
        (jabber-message-correct--apply
         "empty-body-orig" "" "alice@example.com/phone" nil (current-buffer)))
      (let* ((node (jabber-chat-ewoc-find-by-id "empty-body-orig"))
             (msg (cadr (ewoc-data node))))
        (should (equal "" (plist-get msg :body)))
        (should (plist-get msg :edited))
        (should db-called)))))

;;; Group 8: carbon path (outgoing direction in DB)

(ert-deftest jabber-message-correct-test-sender-lookup-outgoing-returns-account ()
  "jabber-db-message-sender-by-stanza-id returns the account JID for direction=out.
This enables carbon copies of our own corrections to be validated
against the account bare JID and accepted."
  (jabber-message-correct-test-with-db
    ;; Store the message as outgoing (direction = \"out\")
    (jabber-db-store-message
     "me@example.com" "alice@example.com" "out" "chat"
     "Sent by me" (floor (float-time))
     nil "stanza-outgoing" nil nil nil nil nil)
    ;; The lookup returns the account JID for outgoing messages
    (should (equal "me@example.com"
                   (jabber-db-message-sender-by-stanza-id "stanza-outgoing")))))

(ert-deftest jabber-message-correct-test-apply-accepts-outgoing-carbon-same-account ()
  "Correction of an outgoing stanza-id is accepted when the carbon is from our account.
The DB lookup returns the account bare JID; the sender in the carbon
arrives as account/resource; bare-JID comparison succeeds."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               ;; Simulates the outgoing-direction account-JID return
               (lambda (_id) "me@example.com"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      ;; Carbon arrives as me@example.com/other-device (1:1, muc-p=nil)
      ;; bare-JID check: "me@example.com" == "me@example.com" => accepted
      (jabber-message-correct--apply
       "stanza-outgoing" "edited" "me@example.com/other-device" nil nil))
    (should db-called)))

(ert-deftest jabber-message-correct-test-apply-rejects-outgoing-carbon-wrong-account ()
  "Correction of an outgoing stanza-id is rejected when the carbon is from a stranger."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "me@example.com"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "stanza-outgoing" "evil" "mallory@example.com/x" nil nil))
    (should-not db-called)))

;;; Group 9: (edited) indicator rendering

(defmacro jabber-message-correct-test-with-printer-buffer (&rest body)
  "Run BODY in a temp buffer with stubs for prompt-level dependencies.
Stubs out jabber-buffer-connection / fsm-get-state-data so that
jabber-chat-pp--local and related functions can run in batch."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-buffer-connection 'fake-jc))
       (cl-letf (((symbol-function 'fsm-get-state-data)
                  (lambda (_jc) '(:username "me")))
                 ((symbol-function 'jabber-jid-displayname)
                  (lambda (jid) jid))
                 ((symbol-function 'jabber-jid-resource)
                  (lambda (jid)
                    (when (string-match "/\\(.*\\)$" jid)
                      (match-string 1 jid)))))
         ,@body))))

(ert-deftest jabber-message-correct-test-edited-indicator-foreign ()
  "jabber-chat-pp--foreign inserts \" (edited)\" when :edited is t."
  (jabber-message-correct-test-with-printer-buffer
    (let ((data (list :foreign
                      (list :id "ind-1"
                            :from "alice@example.com/phone"
                            :body "corrected text"
                            :edited t
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--foreign data)
      (should (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-message-correct-test-no-edited-indicator-when-unedited ()
  "jabber-chat-pp--foreign does not insert \" (edited)\" when :edited is nil."
  (jabber-message-correct-test-with-printer-buffer
    (let ((data (list :foreign
                      (list :id "ind-2"
                            :from "alice@example.com/phone"
                            :body "original text"
                            :edited nil
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--foreign data)
      (should-not (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-message-correct-test-edited-indicator-local ()
  "jabber-chat-pp--local inserts \" (edited)\" when :edited is t."
  (jabber-message-correct-test-with-printer-buffer
    (let ((data (list :local
                      (list :id "ind-3"
                            :from "me@example.com/laptop"
                            :body "my corrected text"
                            :edited t
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--local data)
      (should (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-message-correct-test-edited-indicator-muc-foreign ()
  "jabber-chat-pp--muc-foreign inserts \" (edited)\" when :edited is t."
  (jabber-message-correct-test-with-printer-buffer
    (let ((jabber-muc-printers nil)
          (jabber-chat-printers (list (lambda (msg _who mode)
                                       (when (eq mode :insert)
                                         (insert (or (plist-get msg :body) "")))
                                       t)))
          (data (list :muc-foreign
                      (list :id "ind-4"
                            :from "room@muc.example.com/alice"
                            :body "muc corrected"
                            :edited t
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--muc-foreign data)
      (should (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-message-correct-test-edited-indicator-muc-local ()
  "jabber-chat-pp--muc-local inserts \" (edited)\" when :edited is t."
  (jabber-message-correct-test-with-printer-buffer
    (let ((jabber-muc-printers nil)
          (jabber-chat-printers (list (lambda (msg _who mode)
                                       (when (eq mode :insert)
                                         (insert (or (plist-get msg :body) "")))
                                       t)))
          (data (list :muc-local
                      (list :id "ind-5"
                            :from "room@muc.example.com/me"
                            :body "my muc corrected"
                            :edited t
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--muc-local data)
      (should (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-message-correct-test-edited-indicator-absent-for-retracted ()
  "A retracted MUC message shows tombstone, not (edited), even if :edited is t.
XEP-0425 retraction takes precedence over XEP-0308 edit display."
  (jabber-message-correct-test-with-printer-buffer
    (let ((jabber-muc-printers nil)
          (jabber-chat-printers nil)
          (data (list :muc-foreign
                      (list :id "ind-6"
                            :from "room@muc.example.com/alice"
                            :body "spam"
                            :edited t
                            :retracted t
                            :retracted-by "room@muc.example.com/admin"
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--muc-foreign data)
      (let ((text (buffer-string)))
        (should (string-match-p "retracted" text))
        (should-not (string-match-p "(edited)" text))))))

(provide 'jabber-message-correct-tests)

;;; jabber-message-correct-tests.el ends here
