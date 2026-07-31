;;; jabber-test-message-correct.el --- Tests for jabber-message-correct  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0308 Last Message Correction.

;;; Code:

(require 'ert)
(require 'ewoc)

(require 'jabber-xml)
(require 'jabber-db)
(require 'jabber-chatbuffer)
(require 'jabber-message-correct)

;; jabber-chat and jabber-muc are needed for Groups 5 and 9.
;; jabber-muc requires jabber-chat, and both need this stub constant.
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-mam)

;;; Test helpers

(defmacro jabber-test-message-correct-with-ewoc (&rest body)
  "Set up a temp buffer with a chat ewoc and hash table, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       ,@body)))

(defmacro jabber-test-message-correct-with-db (&rest body)
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

(ert-deftest jabber-test-message-correct-replace-id-nil-for-plain ()
  "Plain message with no <replace> returns nil."
  (let ((stanza '(message ((from . "alice@example.com") (id . "msg-1"))
                          (body () "hello"))))
    (should-not (jabber-message-correct--replace-id stanza))))

(ert-deftest jabber-test-message-correct-replace-id-returns-id ()
  "Correction stanza returns the id from <replace>."
  (let ((stanza `(message ((from . "alice@example.com") (id . "msg-2"))
                          (body () "hello corrected")
                          (replace ((id . "msg-1")
                                    (xmlns . ,jabber-message-correct-xmlns))))))
    (should (equal "msg-1" (jabber-message-correct--replace-id stanza)))))

(ert-deftest jabber-test-message-correct-replace-id-missing-id ()
  "Correction element without id is ignored."
  (let ((stanza `(message ((from . "alice@example.com") (id . "msg-2"))
                          (body () "hello corrected")
                          (replace ((xmlns . ,jabber-message-correct-xmlns))))))
    (should-not (jabber-message-correct--replace-id stanza))))

(ert-deftest jabber-test-message-correct-replace-id-self-reference ()
  "Correction element that references its own stanza id is ignored."
  (let ((stanza `(message ((from . "alice@example.com") (id . "msg-2"))
                          (body () "hello corrected")
                          (replace ((id . "msg-2")
                                    (xmlns . ,jabber-message-correct-xmlns))))))
    (should-not (jabber-message-correct--replace-id stanza))))

(ert-deftest jabber-test-message-correct-replace-id-wrong-xmlns ()
  "Element with wrong xmlns is not treated as a correction."
  (let ((stanza '(message ((from . "alice@example.com") (id . "msg-3"))
                          (body () "hello")
                          (replace ((id . "msg-0")
                                    (xmlns . "urn:xmpp:wrong:0"))))))
    (should-not (jabber-message-correct--replace-id stanza))))

;;; Group 2: jabber-message-correct--valid-sender-p

(ert-deftest jabber-test-message-correct-valid-sender-1to1-same ()
  "1:1: same bare JID allows correction."
  (should (jabber-message-correct--valid-sender-p
           "alice@example.com/laptop"
           "alice@example.com/phone"
           nil)))

(ert-deftest jabber-test-message-correct-valid-sender-1to1-different ()
  "1:1: different bare JID rejects correction."
  (should-not (jabber-message-correct--valid-sender-p
               "alice@example.com/laptop"
               "mallory@example.com/phone"
               nil)))

(ert-deftest jabber-test-message-correct-valid-sender-muc-same-full ()
  "MUC: same full JID (nick) allows correction."
  (should (jabber-message-correct--valid-sender-p
           "room@muc.example.com/alice"
           "room@muc.example.com/alice"
           t)))

(ert-deftest jabber-test-message-correct-valid-sender-muc-different-nick ()
  "MUC: different nick rejects correction."
  (should-not (jabber-message-correct--valid-sender-p
               "room@muc.example.com/alice"
               "room@muc.example.com/mallory"
               t)))

(ert-deftest jabber-test-message-correct-valid-sender-muc-same-occupant-id ()
  "MUC: same occupant-id allows correction even if resource changed."
  (should (jabber-message-correct--valid-sender-p
           "room@muc.example.com/alice"
           "room@muc.example.com/alice2"
           t "occ-1" "occ-1")))

(ert-deftest jabber-test-message-correct-valid-sender-muc-different-occupant-id ()
  "MUC: different occupant-id rejects correction even if resource matches."
  (should-not (jabber-message-correct--valid-sender-p
               "room@muc.example.com/alice"
               "room@muc.example.com/alice"
               t "occ-1" "occ-2")))

;;; Group 3: DB integration

(ert-deftest jabber-test-message-correct-db-correct-message ()
  "jabber-db-correct-message updates body and sets edited=1."
  (jabber-test-message-correct-with-db
    (jabber-db-store-message
     "me@example.com" "friend@example.com" "in" "chat"
     "Original body" (floor (float-time))
     nil "stanza-abc")
    (jabber-db-correct-message "stanza-abc" "Corrected body")
    (let* ((rows (sqlite-select jabber-db--connection
                                "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-abc'"))
           (row (car rows)))
      (should (equal "Corrected body" (car row)))
      (should (= 1 (cadr row))))))

(ert-deftest jabber-test-message-correct-db-row-to-plist-edited ()
  "jabber-db--row-to-plist returns :edited t for edited messages."
  (jabber-test-message-correct-with-db
    (let* ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello" ts nil "stanza-edit-1")
      (jabber-db-correct-message "stanza-edit-1" "Hello fixed")
      (let* ((rows (jabber-db-backlog "me@example.com" "friend@example.com"
                                      1 (- (float-time) 60)))
             (plist (car rows)))
        (should (plist-get plist :edited))
        (should (equal "Hello fixed" (plist-get plist :body)))))))

(ert-deftest jabber-test-message-correct-db-correct-unknown-id ()
  "jabber-db-correct-message is a no-op for unknown stanza-id."
  (jabber-test-message-correct-with-db
    (jabber-db-correct-message "nonexistent-id" "body")
    (let ((count (caar (sqlite-select jabber-db--connection
                                      "SELECT COUNT(*) FROM message"))))
      (should (= 0 count)))))

(ert-deftest jabber-test-message-correct-db-unedited-returns-nil ()
  "Unedited message returns :edited nil via jabber-db--row-to-plist."
  (jabber-test-message-correct-with-db
    (let* ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello" ts nil "stanza-plain-1")
      (let* ((rows (jabber-db-backlog "me@example.com" "friend@example.com"
                                      1 (- (float-time) 60)))
             (plist (car rows)))
        (should-not (plist-get plist :edited))))))

(ert-deftest jabber-test-message-correct-db-candidates-are-conversation-scoped ()
  "Correction candidates never cross account or peer boundaries."
  (jabber-test-message-correct-with-db
    (dolist (row '(("me-a@example.com" "friend@example.com" "one")
                   ("me-b@example.com" "friend@example.com" "two")
                   ("me-a@example.com" "other@example.com" "three")))
      (jabber-db-store-message
       (nth 0 row) (nth 1 row) "in" "chat" (nth 2 row)
       (floor (float-time)) nil "shared-id"))
    (let ((candidates
           (jabber-db-message-correction-candidates
            "me-a@example.com" "friend@example.com" "shared-id")))
      (should (= 1 (length candidates)))
      (should (equal "friend@example.com"
                     (plist-get (car candidates) :from))))))

(ert-deftest jabber-test-message-correct-db-update-targets-one-row ()
  "Correction updates the selected primary row only."
  (jabber-test-message-correct-with-db
    (dotimes (i 2)
      (jabber-db-store-message
       "me@example.com" "room@conference.example.com" "in" "groupchat"
       (format "body-%d" i) (+ (floor (float-time)) i)
       (if (zerop i) "alice" "bob") "shared-id"))
    (let* ((rows (jabber-db-message-correction-candidates
                  "me@example.com" "room@conference.example.com" "shared-id"))
           (alice (seq-find
                   (lambda (row)
                     (equal (plist-get row :from)
                            "room@conference.example.com/alice"))
                   rows)))
      (jabber-db-correct-message-row (plist-get alice :row-id) "fixed")
      (should
       (equal '(("fixed" 1) ("body-1" 0))
              (sqlite-select
               jabber-db--connection
               "SELECT body, edited FROM message ORDER BY id"))))))

(ert-deftest jabber-test-message-correct-db-unscoped-update-rejects-collision ()
  "The compatibility update refuses an ambiguous global stanza ID."
  (jabber-test-message-correct-with-db
    (dolist (account '("me-a@example.com" "me-b@example.com"))
      (jabber-db-store-message
       account "friend@example.com" "in" "chat" account
       (floor (float-time)) nil "shared-id"))
    (jabber-db-correct-message "shared-id" "unsafe")
    (should
     (equal '(("me-a@example.com" 0) ("me-b@example.com" 0))
            (sqlite-select
             jabber-db--connection
             "SELECT body, edited FROM message ORDER BY account")))))

;;; Group 4: ewoc apply correction

(ert-deftest jabber-test-message-correct-apply-updates-ewoc ()
  "jabber-message-correct--apply updates body and edited in the ewoc node, and writes DB."
  (jabber-test-message-correct-with-ewoc
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

(ert-deftest jabber-test-message-correct-apply-drops-undecryptable-body ()
  "A correction whose body failed to decrypt is dropped entirely (issue #134)."
  (jabber-test-message-correct-with-ewoc
    (let ((msg (list :id "orig-dec-1"
                     :from "alice@example.com/phone"
                     :body "original plaintext"
                     :timestamp (current-time)))
          db-called)
      (jabber-chat-ewoc-enter (list :foreign msg))
      (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
                 (lambda (_id) "alice@example.com/phone"))
                ((symbol-function 'jabber-db-correct-message)
                 (lambda (_id _body) (setq db-called t))))
        (should-not (jabber-message-correct--apply
                     "orig-dec-1" "[OMEMO: could not decrypt]"
                     "alice@example.com/phone" nil (current-buffer))))
      (should-not db-called)
      (let* ((node (jabber-chat-ewoc-find-by-id "orig-dec-1"))
             (msg (cadr (ewoc-data node))))
        (should (equal "original plaintext" (plist-get msg :body)))
        (should-not (plist-get msg :edited))))))

(ert-deftest jabber-test-message-correct-apply-rejects-wrong-sender ()
  "jabber-message-correct--apply rejects correction from wrong sender."
  (jabber-test-message-correct-with-ewoc
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

(ert-deftest jabber-test-message-correct-apply-nil-buffer-db-update ()
  "With nil buffer and valid sender, jabber-db-correct-message is called."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "orig-3" "corrected" "alice@example.com/laptop" nil nil))
    (should db-called)))

(ert-deftest jabber-test-message-correct-apply-nil-buffer-wrong-sender-no-db ()
  "With nil buffer and wrong sender, jabber-db-correct-message is not called."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "alice@example.com/phone"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "orig-4" "evil" "mallory@example.com/x" nil nil))
    (should-not db-called)))

(ert-deftest jabber-test-message-correct-apply-unknown-id-no-db ()
  "When stanza-id not in DB, correction is dropped without DB write."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) nil))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "unknown-id" "body" "alice@example.com" nil nil))
    (should-not db-called)))

(ert-deftest jabber-test-message-correct-apply-muc-same-nick ()
  "MUC: correction from same nick is accepted."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "room@muc.example.com/alice"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "muc-orig-1" "corrected" "room@muc.example.com/alice" t nil))
    (should db-called)))

(ert-deftest jabber-test-message-correct-apply-muc-different-nick ()
  "MUC: correction from different nick is rejected."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "room@muc.example.com/alice"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "muc-orig-2" "evil" "room@muc.example.com/mallory" t nil))
    (should-not db-called)))

(ert-deftest jabber-test-message-correct-apply-muc-same-occupant-id ()
  "MUC: correction with matching occupant-id is accepted."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "room@muc.example.com/alice"))
              ((symbol-function 'jabber-db-occupant-id-by-stanza-id)
               (lambda (_id) "occ-1"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "muc-orig-3" "corrected" "room@muc.example.com/alice2" t nil "occ-1"))
    (should db-called)))

(ert-deftest jabber-test-message-correct-apply-muc-different-occupant-id ()
  "MUC: correction with different occupant-id is rejected."
  (let (db-called)
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "room@muc.example.com/alice"))
              ((symbol-function 'jabber-db-occupant-id-by-stanza-id)
               (lambda (_id) "occ-1"))
              ((symbol-function 'jabber-db-correct-message)
               (lambda (_id _body) (setq db-called t))))
      (jabber-message-correct--apply
       "muc-orig-4" "evil" "room@muc.example.com/alice" t nil "occ-2"))
    (should-not db-called)))

(ert-deftest jabber-test-message-correct-apply-outgoing-carbon ()
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

(ert-deftest jabber-test-message-correct-replace-id-present-in-delayed-stanza ()
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

(ert-deftest jabber-test-message-correct-history-message-p-detects-delay ()
  "jabber-muc--history-message-p returns non-nil for stanzas with <delay>."
  (let ((stanza '(message ((from . "room@muc.example.com/alice")
                           (type . "groupchat"))
                          (body () "old message")
                          (delay ((xmlns . "urn:xmpp:delay")
                                  (from . "room@muc.example.com")
                                  (stamp . "2025-01-15T10:30:00Z"))))))
    (should (jabber-muc--history-message-p stanza))))

(ert-deftest jabber-test-message-correct-history-message-p-nil-for-live ()
  "jabber-muc--history-message-p returns nil for live stanzas without <delay>."
  (let ((stanza '(message ((from . "room@muc.example.com/alice")
                           (type . "groupchat"))
                          (body () "live message"))))
    (should-not (jabber-muc--history-message-p stanza))))

(ert-deftest jabber-test-message-correct-delayed-stanza-skipped-by-muc-dispatch ()
  "jabber-muc-process-message must not apply corrections from delayed stanzas.
Regression guard: a delayed correction arriving in MUC history replay
must not mutate the DB or the ewoc."
  (let ((apply-called nil)
        (stanza `(message ((from . "room@muc.example.com/alice")
                           (id . "corr-2")
                           (type . "groupchat"))
                          (body () "corrected text")
                          (replace ((id . "orig-2")
                                    (xmlns . ,jabber-message-correct-xmlns)))
                          (delay ((xmlns . "urn:xmpp:delay")
                                  (from . "room@muc.example.com")
                                  (stamp . "2025-01-15T10:30:00Z"))))))
    (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) t))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc xml) xml))
              ((symbol-function 'jabber-muc-find-buffer)
               (lambda (_group &optional _jc) nil))
              ((symbol-function 'jabber-muc--display-message) #'ignore)
              ((symbol-function 'jabber-message-correct--apply)
               (lambda (&rest _) (setq apply-called t))))
      (jabber-muc-process-message nil stanza))
    (should-not apply-called)))

(ert-deftest jabber-test-message-correct-mam-syncing-skipped-by-chat-dispatch ()
  "jabber-process-chat must not apply corrections while jabber-chat-mam-syncing is non-nil.
Regression guard: a MAM catch-up stanza carrying <replace> must not
be treated as a live edit."
  (let ((apply-called nil)
        (jabber-chat-mam-syncing t)
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
              ((symbol-function 'jabber-mam-chat-opened)
               #'ignore)
              ((symbol-function 'jabber-chat--display-message)
               #'ignore)
              ((symbol-function 'jabber-message-correct--apply)
               (lambda (&rest _) (setq apply-called t))))
      (jabber-process-chat nil stanza))
    (should-not apply-called)))

;;; Group 6: chained corrections

(ert-deftest jabber-test-message-correct-chained-correction-id-unchanged ()
  "jabber-correct-last-message re-uses the original :id after a first correction.
The ewoc node's :id must not be updated when a correction is applied
locally, so that a subsequent C-c C-e will reference the original id,
not the correction's stanza-id."
  (jabber-test-message-correct-with-ewoc
    ;; Insert a sent message with id \"orig-chain-1\"
    (let ((msg (list :id "orig-chain-1"
                     :from "me@example.com"
                     :body "orignal"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    ;; Simulate first correction arriving as a carbon from another own device.
    ;; DB returns our account JID (direction=out); corrector is same bare JID.
    (cl-letf (((symbol-function 'jabber-db-message-sender-by-stanza-id)
               (lambda (_id) "me@example.com"))
              ((symbol-function 'jabber-db-correct-message) #'ignore))
      (jabber-message-correct--apply
       "orig-chain-1" "original" "me@example.com/other-device" nil (current-buffer)))
    ;; After the apply the node's :id must still be the original id
    (let* ((node (jabber-chat-ewoc-find-by-id "orig-chain-1"))
           (msg (cadr (ewoc-data node))))
      (should (equal "orig-chain-1" (plist-get msg :id)))
      (should (plist-get msg :edited)))))

(ert-deftest jabber-test-message-correct-correct-last-uses-original-id ()
  "jabber-correct-last-message sends replace referencing original id after edit.
After --apply updates :body/:edited but leaves :id alone,
jabber-correct-last-message must pick up the original id."
  (jabber-test-message-correct-with-ewoc
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
    ;; jabber-correct-last-message runs the full send-hook chain;
    ;; disable storage so the DB outgoing handler cannot touch the
    ;; user's real database.
    (let ((jabber-db-path nil)
          (jabber-db--connection nil)
          sent-replace-id)
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

(ert-deftest jabber-test-message-correct-nil-body-db-write ()
  "Correction with nil body writes nil to the DB (no crash)."
  ;; A correction stanza lacking <body> passes nil through --apply.
  ;; jabber-db-correct-message must not error on nil body.
  (jabber-test-message-correct-with-db
    (jabber-db-store-message
     "me@example.com" "friend@example.com" "in" "chat"
     "Original" (floor (float-time))
     nil "stanza-nil-body")
    ;; Should not signal -- nil body is a valid (if odd) correction
    (jabber-db-correct-message "stanza-nil-body" nil)
    (let* ((rows (sqlite-select jabber-db--connection
                                "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-nil-body'"))
           (row (car rows)))
      ;; edited flag must be set even for nil body
      (should (= 1 (cadr row))))))

(ert-deftest jabber-test-message-correct-nil-body-ewoc-update ()
  "Correction with nil body updates ewoc :body to nil without error."
  (jabber-test-message-correct-with-ewoc
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

(ert-deftest jabber-test-message-correct-empty-body-accepted ()
  "Correction with empty string body is accepted and written."
  (jabber-test-message-correct-with-ewoc
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

(ert-deftest jabber-test-message-correct-sender-lookup-outgoing-returns-account ()
  "jabber-db-message-sender-by-stanza-id returns the account JID for direction=out.
This enables carbon copies of our own corrections to be validated
against the account bare JID and accepted."
  (jabber-test-message-correct-with-db
    ;; Store the message as outgoing (direction = \"out\")
    (jabber-db-store-message
     "me@example.com" "alice@example.com" "out" "chat"
     "Sent by me" (floor (float-time))
     nil "stanza-outgoing")
    ;; The lookup returns the account JID for outgoing messages
    (should (equal "me@example.com"
                   (jabber-db-message-sender-by-stanza-id "stanza-outgoing")))))

(ert-deftest jabber-test-message-correct-apply-accepts-outgoing-carbon-same-account ()
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

(ert-deftest jabber-test-message-correct-apply-rejects-outgoing-carbon-wrong-account ()
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

(defmacro jabber-test-message-correct-with-printer-buffer (&rest body)
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

(ert-deftest jabber-test-message-correct-edited-indicator-foreign ()
  "jabber-chat-pp--foreign inserts \" (edited)\" when :edited is t."
  (jabber-test-message-correct-with-printer-buffer
    (let ((data (list :foreign
                      (list :id "ind-1"
                            :from "alice@example.com/phone"
                            :body "corrected text"
                            :edited t
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--foreign data)
      (should (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-test-message-correct-no-edited-indicator-when-unedited ()
  "jabber-chat-pp--foreign does not insert \" (edited)\" when :edited is nil."
  (jabber-test-message-correct-with-printer-buffer
    (let ((data (list :foreign
                      (list :id "ind-2"
                            :from "alice@example.com/phone"
                            :body "original text"
                            :edited nil
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--foreign data)
      (should-not (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-test-message-correct-edited-indicator-local ()
  "jabber-chat-pp--local inserts \" (edited)\" when :edited is t."
  (jabber-test-message-correct-with-printer-buffer
    (let ((data (list :local
                      (list :id "ind-3"
                            :from "me@example.com/laptop"
                            :body "my corrected text"
                            :edited t
                            :timestamp (current-time)
                            :delayed nil))))
      (jabber-chat-pp--local data)
      (should (string-match-p "(edited)" (buffer-string))))))

(ert-deftest jabber-test-message-correct-edited-indicator-muc-foreign ()
  "jabber-chat-pp--muc-foreign inserts \" (edited)\" when :edited is t."
  (jabber-test-message-correct-with-printer-buffer
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

(ert-deftest jabber-test-message-correct-edited-indicator-muc-local ()
  "jabber-chat-pp--muc-local inserts \" (edited)\" when :edited is t."
  (jabber-test-message-correct-with-printer-buffer
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

(ert-deftest jabber-test-message-correct-edited-indicator-absent-for-retracted ()
  "A retracted MUC message shows tombstone, not (edited), even if :edited is t.
XEP-0425 retraction takes precedence over XEP-0308 edit display."
  (jabber-test-message-correct-with-printer-buffer
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

;;; Group 10: reply re-attachment on correction

(ert-deftest jabber-test-message-correct-find-last-sent-returns-msg ()
  "find-last-sent returns the msg plist as fourth element."
  (jabber-test-message-correct-with-ewoc
    (let ((msg (list :id "m-1" :body "hello" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg))
      (pcase-let ((`(,_node ,id ,body ,found)
                   (jabber-message-correct--find-last-sent jabber-chat-ewoc)))
        (should (equal "m-1" id))
        (should (equal "hello" body))
        (should (eq msg found))))))

(ert-deftest jabber-test-message-correct-reattaches-reply ()
  "Correcting a reply re-attaches the reply and fallback elements."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let ((msg (list :id "r-1"
                     :from "me@example.com"
                     :body "> alice:\n> hi\nanswer"
                     :reply-to-id "orig-1"
                     :reply-to-jid "alice@example.com/phone"
                     :fallback-range '(0 14)
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    (let ((jabber-db-path nil)
          sent)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc stanza) (setq sent stanza)))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "> alice:\n> hi\nbetter answer"))
                ((symbol-function 'jabber-db-correct-message) #'ignore))
        (jabber-correct-last-message))
      (should (jabber-xml-child-with-xmlns sent "urn:xmpp:message-correct:0"))
      (let ((reply-el (jabber-xml-child-with-xmlns sent "urn:xmpp:reply:0")))
        (should reply-el)
        (should (equal "orig-1" (jabber-xml-get-attribute reply-el 'id)))
        (should (equal "alice@example.com/phone"
                       (jabber-xml-get-attribute reply-el 'to))))
      (let ((fb-el (jabber-xml-child-with-xmlns sent "urn:xmpp:fallback:0")))
        (should fb-el)
        (should (equal "14" (jabber-xml-get-attribute
                             (car (jabber-xml-get-children fb-el 'body))
                             'end)))))))

(ert-deftest jabber-test-message-correct-threaded-root-keeps-thread ()
  "Direct and MUC root corrections retain one XEP-0201 element."
  (dolist (kind '(chat groupchat))
    (jabber-test-message-correct-with-ewoc
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chat-encryption 'plaintext)
      (if (eq kind 'groupchat)
          (setq-local jabber-group "room@example.com")
        (setq-local jabber-chatting-with "alice@example.com"))
      (jabber-chat-ewoc-enter
       (list (if (eq kind 'groupchat) :muc-local :local)
             (list :id "root-1"
                   :from (if (eq kind 'groupchat)
                             "room@example.com/me"
                           "me@example.com")
                   :body "root"
                   :thread-id "thread-1"
                   :thread-parent-id "parent-1"
                   :timestamp (current-time))))
      (let ((jabber-db-path nil)
            sent)
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "me@example.com"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "corrected root"))
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc stanza &rest _) (setq sent stanza))))
          (jabber-correct-last-message))
        (let ((threads (jabber-xml-get-children sent 'thread)))
          (should (= 1 (length threads)))
          (should (equal "thread-1"
                         (car (jabber-xml-node-children (car threads)))))
          (should (equal "parent-1"
                         (jabber-xml-get-attribute (car threads) 'parent))))))))

(ert-deftest jabber-test-message-correct-drops-stale-fallback ()
  "Editing the quote away keeps the reply element but drops the range."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let ((msg (list :id "r-2"
                     :from "me@example.com"
                     :body "> alice:\n> hi\nanswer"
                     :reply-to-id "orig-1"
                     :reply-to-jid "alice@example.com/phone"
                     :fallback-range '(0 14)
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    (let ((jabber-db-path nil)
          sent)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc stanza) (setq sent stanza)))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "answer without quote"))
                ((symbol-function 'jabber-db-correct-message) #'ignore))
        (jabber-correct-last-message))
      (should (jabber-xml-child-with-xmlns sent "urn:xmpp:reply:0"))
      (should-not (jabber-xml-child-with-xmlns sent "urn:xmpp:fallback:0"))
      (let* ((node (jabber-chat-ewoc-find-by-id "r-2"))
             (msg (cadr (ewoc-data node))))
        (should-not (plist-get msg :fallback-range))))))

(ert-deftest jabber-test-message-correct-non-reply-adds-no-reply-element ()
  "Correcting a plain message adds no reply or fallback elements."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let ((msg (list :id "p-1"
                     :from "me@example.com"
                     :body "plain"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    (let ((jabber-db-path nil)
          sent)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc stanza) (setq sent stanza)))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "plain fixed"))
                ((symbol-function 'jabber-db-correct-message) #'ignore))
        (jabber-correct-last-message))
      (should (jabber-xml-child-with-xmlns sent "urn:xmpp:message-correct:0"))
      (should-not (jabber-xml-child-with-xmlns sent "urn:xmpp:reply:0"))
      (should-not (jabber-xml-child-with-xmlns sent "urn:xmpp:fallback:0")))))

(ert-deftest jabber-test-message-correct-omemo-failure-preserves-local-state ()
  "An asynchronous OMEMO pre-send failure leaves local history unchanged."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          db-updated)
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "proposed correction"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (_jc _body _extra _success failure)
                   (funcall failure "no recipient sessions")))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (&rest _) (setq db-updated t))))
        (jabber-correct-last-message))
      (should (equal "original" (plist-get msg :body)))
      (should-not (plist-get msg :edited))
      (should-not db-updated))))

(ert-deftest jabber-test-message-correct-omemo-success-commits-once ()
  "An OMEMO correction commits only from its transport-success callback."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          (updates 0))
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (_jc _body _extra success _failure)
                   (funcall success)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (_row-id _body) (cl-incf updates))))
        (jabber-correct-last-message))
      (should (equal "corrected" (plist-get msg :body)))
      (should (plist-get msg :edited))
      (should (= 1 updates)))))

(ert-deftest jabber-test-message-correct-own-muc-echo-matches-stored-sender ()
  "A locally sent MUC echo is corrected using its full stored sender."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group "room@conference.example.com")
    (setq-local jabber-chatting-with nil)
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "room@conference.example.com/me"
                     :body "original"
                     :timestamp (current-time)))
          queried
          updates)
      (jabber-chat-ewoc-enter (list :muc-local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-muc-send)
                 (lambda (_jc _body _extra success _failure)
                   (funcall success)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (account peer id)
                   (setq queried (list account peer id))
                   (list (list :row-id 7
                               :from "room@conference.example.com/me"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (row-id body)
                   (setq updates (list row-id body)))))
        (jabber-correct-last-message))
      (should (equal '("me@example.com"
                       "room@conference.example.com"
                       "original-1")
                     queried))
      (should (equal '(7 "corrected") updates))
      (should (equal "corrected" (plist-get msg :body))))))

(ert-deftest jabber-test-message-correct-omemo-late-success-after-failure-is-inert ()
  "A stale success callback cannot commit after the send has failed."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          (updates 0)
          success
          failure)
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (_jc _body _extra on-success on-failure)
                   (setq success on-success
                         failure on-failure)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (&rest _) (cl-incf updates))))
        (jabber-correct-last-message)
        (funcall failure "connection reset")
        (funcall success))
      (should (equal "original" (plist-get msg :body)))
      (should-not (plist-get msg :edited))
      (should (= 0 updates)))))

(ert-deftest jabber-test-message-correct-omemo-synchronous-error-clears-pending ()
  "A synchronous OMEMO send error clears pending correction state."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          (updates 0))
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (&rest _) (error "send failed")))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (&rest _) (cl-incf updates))))
        (should-error (jabber-correct-last-message)))
      (should-not jabber-message-correct--pending-outgoing)
      (should (equal "original" (plist-get msg :body)))
      (should-not (plist-get msg :edited))
      (should (= 0 updates)))))

(ert-deftest jabber-test-message-correct-omemo-success-after-buffer-kill-is-inert ()
  "An OMEMO success callback cannot commit after its buffer is killed."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((buffer (current-buffer))
          (msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          (updates 0)
          success)
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (_jc _body _extra on-success _on-failure)
                   (setq success on-success)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (&rest _) (cl-incf updates))))
        (jabber-correct-last-message)
        (kill-buffer buffer)
        (funcall success))
      (should (equal "original" (plist-get msg :body)))
      (should-not (plist-get msg :edited))
      (should (= 0 updates)))))

(ert-deftest jabber-test-message-correct-omemo-duplicate-success-is-inert ()
  "A duplicate OMEMO success callback commits only once."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          (updates 0)
          success)
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (_jc _body _extra on-success _on-failure)
                   (setq success on-success)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (&rest _) (cl-incf updates))))
        (jabber-correct-last-message)
        (funcall success)
        (funcall success))
      (should (equal "corrected" (plist-get msg :body)))
      (should (plist-get msg :edited))
      (should-not jabber-message-correct--pending-outgoing)
      (should (= 1 updates)))))

(ert-deftest jabber-test-message-correct-omemo-failure-after-success-is-inert ()
  "A stale OMEMO failure callback cannot undo a committed correction."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-group nil)
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chat-encryption 'omemo)
    (let ((msg (list :id "original-1"
                     :from "me@example.com"
                     :body "original"
                     :timestamp (current-time)))
          (updates 0)
          success
          failure)
      (jabber-chat-ewoc-enter (list :local msg))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "corrected"))
                ((symbol-function 'jabber-chat-send)
                 (lambda (_jc _body _extra on-success on-failure)
                   (setq success on-success
                         failure on-failure)))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-correction-candidates)
                 (lambda (&rest _)
                   (list (list :row-id 1 :from "me@example.com"))))
                ((symbol-function 'jabber-db-correct-message-row)
                 (lambda (&rest _) (cl-incf updates))))
        (jabber-correct-last-message)
        (funcall success)
        (funcall failure "late failure"))
      (should (equal "corrected" (plist-get msg :body)))
      (should (plist-get msg :edited))
      (should-not jabber-message-correct--pending-outgoing)
      (should (= 1 updates)))))

(ert-deftest jabber-test-message-correct-blocks-overlapping-omemo-edits ()
  "A second edit cannot race an OMEMO correction awaiting transport."
  (jabber-test-message-correct-with-ewoc
    (setq-local jabber-message-correct--pending-outgoing '(pending))
    (let ((prompted nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompted t))))
        (should-error (jabber-correct-last-message) :type 'user-error))
      (should-not prompted))))

(ert-deftest jabber-test-message-correct-muc-continuity-rejects-rejoin ()
  "Legacy MUC correction continuity is reset across occupant presence lifetimes."
  (let ((jabber-message-correct--muc-presence-sessions
         (make-hash-table :test #'equal))
        (jabber-message-correct--muc-last-message-ids
         (make-hash-table :test #'equal))
        (from "room@conference.example.com/alice"))
    (jabber-message-correct--muc-presence-enter 'jc-a from)
    (jabber-message-correct--record-muc-original 'jc-a from "original-1")
    (should (jabber-message-correct--muc-current-target-p
             'jc-a from "original-1"))
    (jabber-message-correct--muc-presence-leave 'jc-a from)
    (jabber-message-correct--muc-presence-enter 'jc-a from)
    (should-not (jabber-message-correct--muc-current-target-p
                 'jc-a from "original-1"))))

(ert-deftest jabber-test-message-correct-muc-continuity-is-connection-scoped ()
  "Two accounts in one room cannot authorize each other's correction target."
  (let ((jabber-message-correct--muc-presence-sessions
         (make-hash-table :test #'equal))
        (jabber-message-correct--muc-last-message-ids
         (make-hash-table :test #'equal))
        (from "room@conference.example.com/alice"))
    (jabber-message-correct--muc-presence-enter 'jc-a from)
    (jabber-message-correct--muc-presence-enter 'jc-b from)
    (jabber-message-correct--record-muc-original 'jc-a from "account-a-id")
    (should (jabber-message-correct--muc-current-target-p
             'jc-a from "account-a-id"))
    (should-not (jabber-message-correct--muc-current-target-p
                 'jc-b from "account-a-id"))))

(provide 'jabber-test-message-correct)

;;; jabber-test-message-correct.el ends here
