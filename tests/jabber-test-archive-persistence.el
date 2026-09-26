;;; jabber-test-archive-persistence.el --- Durable archive proof -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-archive-test-helpers)
(require 'jabber-message-reply)
(require 'jabber-reactions)

(ert-deftest jabber-test-archive-origin-transport-reopen ()
  "Preserve distinct origin and transport IDs through real incoming storage."
  (jabber-test-archive--with-file
    (let ((xml '(message ((from . "friend@example.com/mobile")
                         (type . "chat") (id . "transport"))
                        (body () "original")
                        (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "origin"))))))
      (jabber-db--message-handler jc xml)
      (jabber-db-close)
      (let ((stored (car (jabber-db-backlog "me@example.com" "friend@example.com"))))
        (should (equal (plist-get stored :id) "transport"))
        (should (equal (jabber-message-reply--select-id stored nil) "origin"))
        (should (equal "original" (jabber-db-reply-target-body
                                   "me@example.com" "friend@example.com" "origin" nil)))
        (should (equal "original" (jabber-db-reply-target-body
                                   "me@example.com" "friend@example.com" "transport" nil)))))))

(ert-deftest jabber-test-archive-room-personal-replay ()
  "Keep trusted room action identity across two archive authorities."
  (jabber-test-archive--with-file
    (let ((inner '(message ((from . "room@example.com/nick")
                           (to . "me@example.com") (type . "groupchat")
                           (id . "transport"))
                          (body () "room message")
                          (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                      (by . "room@example.com") (id . "room-id"))))))
      (dolist (archive '(nil "room@example.com"))
        (let ((query (jabber-mam--query jc nil nil nil nil archive)))
          (jabber-mam--process-message
           jc (jabber-test-archive--result query (if archive "room-uid" "personal-uid") inner))
          (jabber-test-archive--fin jc query (if archive "room-uid" "personal-uid"))))
      (jabber-db-close)
      (let ((rows (jabber-db-backlog "me@example.com" "room@example.com" nil nil nil "groupchat")))
        (should (= 1 (length rows)))
        (should (equal "room-id" (jabber-message-reply--select-id (car rows) t)))))))

(ert-deftest jabber-test-archive-bodyless-fin-progress ()
  "Advance a bodyless page without borrowing a message row's identity."
  (jabber-test-archive--with-file
    (let ((query (jabber-mam--query jc)))
      (jabber-mam--process-message
       jc (jabber-test-archive--result query "bodyless"
            '(message ((from . "friend@example.com") (to . "me@example.com")))))
      (jabber-test-archive--fin jc query "bodyless"))
    (jabber-db-close)
    (let (request)
      (cl-letf (((symbol-function 'jabber-mam--query)
                 (lambda (&rest args) (setq request args))))
        (jabber-mam--catch-up jc))
      (should (equal (cadr request) "bodyless")))))

(ert-deftest jabber-test-archive-commit-is-the-receipt ()
  "Independent SQLite readers see both effects and progress only at commit."
  (jabber-test-archive--with-file
    (let* ((first (jabber-mam--query jc))
           (second (jabber-mam--query jc nil nil nil nil "room@example.com"))
           (reader (sqlite-open jabber-db-path)))
      (unwind-protect
          (progn
            (jabber-mam--process-message jc
             (jabber-test-archive--result first "uid"
              '(message ((from . "friend@example.com/phone") (type . "chat") (id . "one"))
                        (body () "visible at commit"))))
            (jabber-test-archive--fin jc first "uid")
            (should-not (sqlite-select reader "SELECT uid FROM mam_progress"))
            (should-not (sqlite-select reader "SELECT body FROM message"))
            (jabber-test-archive--fin jc second "room-uid")
            (should (equal '(("visible at commit"))
                           (sqlite-select reader "SELECT body FROM message")))
            (should (equal '(("uid") ("room-uid"))
                           (sqlite-select reader "SELECT uid FROM mam_progress ORDER BY archive"))))
        (sqlite-close reader)))))

(ert-deftest jabber-test-archive-predecessor-cas-and-backfill ()
  "Late same-scope completion and backward history cannot regress progress."
  (jabber-test-archive--with-file
    (let ((older (jabber-mam--query jc))
          (newer (jabber-mam--query jc)))
      (jabber-test-archive--fin jc newer "newer")
      (jabber-test-archive--fin jc older "older"))
    (should (equal '("newer" nil) (jabber-test-archive--progress)))
    (let ((backfill (jabber-mam--query jc nil nil nil nil nil t 10)))
      (jabber-mam--process-message jc
       (jabber-test-archive--result backfill "old-history"
        '(message ((from . "friend@example.com/phone") (type . "chat") (id . "old"))
                  (body () "older"))))
      (jabber-test-archive--fin jc backfill "old-history"))
    (jabber-db-store-message "me@example.com" "room@example.com" "in" "groupchat"
                             "live" 1 "someone" "live-id" "unrelated-room")
    ;; Arrival order and equal/archive timestamps have no cursor authority.
    (dolist (entry '((42 "tie-a") (42 "tie-b") (2 "late-old")))
      (jabber-db-store-message "me@example.com" "friend@example.com" "in" "chat"
                               (cadr entry) (car entry) "phone"
                               (cadr entry) (cadr entry)))
    (jabber-db-close)
    (should (equal '("newer" nil) (jabber-test-archive--progress)))))

(ert-deftest jabber-test-archive-scopes-and-wire-cursors ()
  "Keep two accounts, personal/two room archives and peer filters distinct."
  (jabber-test-archive--with-file
    (let ((other (jabber-test-mam--native-connection "other")))
      (dolist (connection (list jc other))
        (dolist (archive '(nil "room@example.com" "second@example.com"))
          (dolist (with '(nil "friend@example.com"))
            (let* ((account (jabber-connection-bare-jid connection))
                   (uid (format "%s/%s/%s" account archive with))
                   (query (jabber-mam--query connection nil nil with nil archive)))
              (jabber-mam--handle-fin connection
               `(iq ((from . ,(or archive account)))
                    (fin ((xmlns . "urn:xmpp:mam:2") (complete . "true"))
                         (set ((xmlns . "http://jabber.org/protocol/rsm")) (last () ,uid))))
               (cons query (plist-get query :page)))))))
      (jabber-db-close)
      (dolist (connection (list jc other))
        (dolist (kind '(global peer room second))
          (let* ((account (jabber-connection-bare-jid connection))
                 (archive (pcase kind ('room "room@example.com") ('second "second@example.com")))
                 (with (and (eq kind 'peer) "friend@example.com"))
                 sent)
            (cl-letf (((symbol-function 'jabber-send-sexp)
                       (lambda (_jc xml &rest _) (setq sent xml))))
              (pcase kind
                ('global (jabber-mam--catch-up connection))
                ('peer (jabber-mam--chat-catch-up connection with nil))
                (_ (jabber-mam--muc-catch-up connection archive))))
            (let* ((query (car (jabber-xml-get-children sent 'query)))
                   (set (car (jabber-xml-get-children query 'set)))
                   (form (car (jabber-xml-get-children query 'x)))
                   (field (seq-find (lambda (el) (equal "with" (jabber-xml-get-attribute el 'var)))
                                    (jabber-xml-get-children form 'field))))
              (should (equal archive (jabber-xml-get-attribute sent 'to)))
              (should (equal with (car (jabber-xml-node-children
                                       (car (jabber-xml-get-children field 'value))))))
              (should (equal (format "%s/%s/%s" account archive with)
                             (car (jabber-xml-node-children
                                   (car (jabber-xml-get-children set 'after)))))))
            (jabber-mam--cleanup-connection connection)))))))

(ert-deftest jabber-test-archive-correction-duplicate-and-empty-pages ()
  "Progress correction-only, duplicate-only and empty pages without new rows."
  (jabber-test-archive--with-file
    (jabber-db-store-message "me@example.com" "friend@example.com" "in" "chat"
                             "before" 1 "phone" "original" "initial")
    (let ((query (jabber-mam--query jc)))
      (jabber-mam--process-message jc
       (jabber-test-archive--result query "edit-uid"
        '(message ((from . "friend@example.com/phone") (type . "chat") (id . "edit"))
                  (body () "after")
                  (replace ((xmlns . "urn:xmpp:message-correct:0") (id . "original"))))))
      (jabber-test-archive--fin jc query "edit-uid"))
    (should (equal '(("after")) (sqlite-select jabber-db--connection "SELECT body FROM message")))
    (should (equal '("edit-uid" nil) (jabber-test-archive--progress)))
    (let ((query (jabber-mam--query jc "edit-uid")))
      (jabber-mam--process-message jc
       (jabber-test-archive--result query "initial"
        '(message ((from . "friend@example.com/phone") (type . "chat") (id . "original"))
                  (body () "before"))))
      (jabber-test-archive--fin jc query "duplicate-page"))
    (let ((query (jabber-mam--query jc "duplicate-page")))
      (jabber-test-archive--fin jc query nil))
    (jabber-db-close)
    (should (equal '("duplicate-page" nil) (jabber-test-archive--progress)))
    (should (equal '(("after")) (sqlite-select jabber-db--connection "SELECT body FROM message")))))

(ert-deftest jabber-test-archive-stale-uid-preserves-coverage ()
  "Stale UID recovery keeps the durable lower bound despite a long absence."
  (jabber-test-archive--with-file
    (let ((query (jabber-mam--query jc nil nil nil "2001-01-01T00:00:00Z")))
      (jabber-test-archive--fin jc query "gone"))
    (jabber-db-close)
    (let ((jabber-mam-catch-up-days 1) sent)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc xml &rest _) (push xml sent))))
        (let* ((query (jabber-mam--catch-up jc))
               (closure (cons query (plist-get query :page))))
          (jabber-mam--handle-error jc
           '(iq () (error () (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))) closure)
          (should-not (plist-get query :after))
          (should (equal "2001-01-01T00:00:00Z" (plist-get query :start)))
          (should (= 2 (length sent)))
          (jabber-test-archive--fin jc query "recovered"))))
    (should (equal '("recovered" "2001-01-01T00:00:00Z") (jabber-test-archive--progress)))
    ;; A different, unknown archive never borrows history or recent-days policy.
    (let ((query (jabber-mam--query jc nil nil nil nil "new-room@example.com")))
      (should-not (plist-get query :start)))))

(ert-deftest jabber-test-archive-commit-fault-rolls-back ()
  "A native COMMIT failure rolls back cursor and page effects, then retries."
  (jabber-test-archive--with-file
    (let ((query (jabber-mam--query jc)) outcome
          (execute (symbol-function 'sqlite-execute)))
      (plist-put query :result-callback (lambda (value) (setq outcome value)))
      (jabber-mam--process-message jc
       (jabber-test-archive--result query "failed"
        '(message ((from . "friend@example.com/phone") (type . "chat") (id . "one"))
                  (body () "uncommitted"))))
      (cl-letf (((symbol-function 'sqlite-execute)
                 (lambda (db sql &rest args)
                   (if (equal sql "COMMIT") (error "Commit fault")
                     (apply execute db sql args)))))
        (should-error (jabber-test-archive--fin jc query "failed")))
      (should (eq outcome 'failed)))
    (jabber-db-close)
    (should-not (jabber-test-archive--progress))
    (should-not (sqlite-select jabber-db--connection "SELECT id FROM message"))
    (let ((query (jabber-mam--query jc)))
      (jabber-test-archive--fin jc query "retry"))
    (should (equal '("retry" nil) (jabber-test-archive--progress)))))

(ert-deftest jabber-test-archive-failed-or-retired-pages-do-not-progress ()
  "Reject malformed fin, store/decrypt/downstream faults and retired owners."
  (dolist (failure '(cancel stale store decrypt downstream malformed))
    (jabber-test-archive--with-file
      (let* ((query (jabber-mam--query jc))
             (xml (jabber-test-archive--result query "bad"
                   '(message ((from . "friend@example.com/phone") (type . "chat") (id . "one"))
                             (body () "body")))))
        (pcase failure
          ('cancel (jabber-mam--cleanup-connection jc))
          ('stale (plist-put (fsm-get-state-data jc) :connection (list 'replacement)))
          ('store
           (cl-letf (((symbol-function 'jabber-db-store-message) (lambda (&rest _) (error "Store fault"))))
             (should-error (jabber-mam--process-message jc xml))))
          ('decrypt
           (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed) (lambda (&rest _) (error "Decrypt fault"))))
             (should-error (jabber-mam--process-message jc xml))))
          ('downstream
           (let ((called 0))
             (let ((debug-on-error nil)
                   (jabber-message-chain
                    (list #'jabber-mam--process-message
                          (cons 0 (lambda (&rest _)
                                    (cl-incf called)
                                    (error "Downstream fault"))))))
               (jabber-process-input jc
                (jabber-test-archive--result query "bad"
                 '(message ((from . "friend@example.com"))))))
             (should (= called 1))))
          ('malformed
           (jabber-mam--handle-fin jc '(iq () (wrong ((xmlns . "urn:xmpp:mam:2") (complete . "true"))))
                                   (cons query (plist-get query :page)))))
        (jabber-test-archive--fin jc query "bad")
        (jabber-mam--cleanup-connection jc))
      (jabber-db-close)
      (should-not (jabber-test-archive--progress)))))

(ert-deftest jabber-test-archive-completed-room-retirement-before-commit ()
  "Room leave revokes accepted but not yet durable room progress only."
  (jabber-test-archive--with-file
    (let ((room (jabber-mam--query jc nil nil nil nil "room@example.com"))
          (personal (jabber-mam--query jc)))
      (jabber-test-archive--fin jc room "retired")
      (jabber-mam--cancel-muc-query "room@example.com" jc)
      (jabber-test-archive--fin jc personal "personal"))
    (jabber-db-close)
    (should-not (jabber-test-archive--progress nil "room@example.com"))
    (should (equal '("personal" nil) (jabber-test-archive--progress)))))

(ert-deftest jabber-test-archive-live-room-actions-and-tombstone-replay ()
  "Retain room reply/reaction/retraction identity through live and two archives."
  (jabber-test-archive--with-file
    (let ((jabber-muc--rooms (make-hash-table :test #'equal))
          (jabber-muc--generation 0)
          (inner '(message ((from . "room@example.com/nick") (to . "me@example.com")
                            (type . "groupchat") (id . "transport"))
                           (body () "retained")
                           (thread () "thread")
                           (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "origin")))
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (by . "room@example.com") (id . "room-id"))))))
      (jabber-muc-join-set "room@example.com" jc "me")
      (jabber-db--message-handler jc inner)
      (dolist (archive '(nil "room@example.com"))
        (let ((query (jabber-mam--query jc nil nil nil nil archive t 10)))
          (jabber-mam--process-message jc
           (jabber-test-archive--result query (if archive "room-uid" "personal-uid") inner))
          (jabber-test-archive--fin jc query (if archive "room-uid" "personal-uid"))))
      (jabber-db-close)
      (let* ((rows (jabber-db-backlog "me@example.com" "room@example.com" nil nil nil "groupchat"))
             (stored (car rows)))
        (should (= 1 (length rows)))
        (should (equal "room-id" (jabber-message-reply--select-id stored t)))
        (should (equal "room-id" (jabber-reactions--target-id stored t)))
        (should (equal "origin" (plist-get stored :origin-id)))
        (should (equal "transport" (plist-get stored :id)))
        (should (= 1 (length (jabber-db-message-retraction-candidates
                             "me@example.com" "room@example.com" "room-id"))))
        (should-not (jabber-db-message-retraction-candidates
                     "me@example.com" "room@example.com" "personal-uid"))
        (should (equal '(("me@example.com" "personal-uid") ("room@example.com" "room-uid"))
                       (sqlite-select jabber-db--connection
                        "SELECT archive,uid FROM message_archive ORDER BY archive")))
        (jabber-db-retract-message-row (plist-get stored :db-id) nil)
        (let ((query (jabber-mam--query jc nil nil nil nil nil t 10)))
          (jabber-mam--process-message jc (jabber-test-archive--result query "personal-uid" inner))
          (jabber-test-archive--fin jc query "personal-uid"))
        (jabber-db-close)
        (let ((after (car (jabber-db-backlog "me@example.com" "room@example.com" nil nil nil "groupchat"))))
          (should (plist-get after :retracted))
          (should (equal "thread" (plist-get after :thread-id)))
          (should (equal "retained" (plist-get after :body))))))))

(ert-deftest jabber-test-archive-occurrence-conflicts-preserve-row ()
  "Never enrich a different sender, type or ID under an existing archive UID."
  (dolist (conflict '(sender type transport origin))
    (jabber-test-archive--with-file
      (let ((inner '(message ((from . "friend@example.com/phone") (to . "me@example.com")
                             (type . "chat") (id . "transport"))
                            (body () "retained")
                            (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "origin"))))))
        (let ((query (jabber-mam--query jc)))
          (jabber-mam--process-message jc (jabber-test-archive--result query "uid" inner))
          (jabber-test-archive--fin jc query "uid"))
        (let* ((changed (copy-tree inner))
               (query (jabber-mam--query jc nil nil nil nil nil t 10)))
          (pcase conflict
            ('sender (setf (alist-get 'from (cadr changed)) "friend@example.com/other"))
            ('type (setf (alist-get 'type (cadr changed)) "headline"))
            ('transport (setf (alist-get 'id (cadr changed)) "different"))
            ('origin (setf (alist-get 'id (cadr (car (jabber-xml-get-children changed 'origin-id)))) "different")))
          (should-error (jabber-mam--process-message jc (jabber-test-archive--result query "uid" changed)))
          (jabber-test-archive--fin jc query "uid"))
        (jabber-db-close)
        (should (equal '(("retained" "transport" "origin" "phone"))
                       (sqlite-select (jabber-db-ensure-open)
                        "SELECT body,stanza_id,origin_id,resource FROM message")))))))

(ert-deftest jabber-test-archive-conflicting-origin-does-not-merge ()
  "A reused transport ID cannot replace another origin ID."
  (jabber-test-archive--with-file
    (dolist (origin '("first" "second"))
      (jabber-db--message-handler jc
       `(message ((from . "friend@example.com/phone") (type . "chat") (id . "transport"))
                 (body () "equal body")
                 (origin-id ((xmlns . "urn:xmpp:sid:0") (id . ,origin))))))
    (jabber-db-close)
    (should (equal '(("first") ("second"))
                   (sqlite-select (jabber-db-ensure-open) "SELECT origin_id FROM message ORDER BY id")))))

(ert-deftest jabber-test-archive-legacy-unknown-evidence ()
  "Keep legacy server semantics and infer neither origins nor archive coverage."
  (jabber-test-archive--with-file
    (jabber-db-store-message "me@example.com" "friend@example.com" "in" "chat"
                             "legacy" 1 "phone" "transport" "unknown")
    (should-not (jabber-test-archive--progress))
    (jabber-db--message-handler jc
     '(message ((from . "friend@example.com/phone") (type . "chat") (id . "transport"))
               (body () "legacy")
               (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "proven")))))
    (jabber-db-close)
    (should (equal '(("unknown" "proven" nil))
                   (sqlite-select (jabber-db-ensure-open)
                                  "SELECT server_id,origin_id,room_id FROM message")))
    (should-not (sqlite-select jabber-db--connection "SELECT * FROM message_archive"))))

(ert-deftest jabber-test-archive-progress-sql-fault-and-retry ()
  "Roll back page effects when SQLite itself rejects cursor persistence."
  (jabber-test-archive--with-file
    (sqlite-execute jabber-db--connection
     "CREATE TRIGGER reject_progress BEFORE INSERT ON mam_progress
BEGIN SELECT RAISE(ABORT, 'cursor fault'); END")
    (let ((query (jabber-mam--query jc)))
      (jabber-mam--process-message jc
       (jabber-test-archive--result query "uid"
        '(message ((from . "friend@example.com/phone") (type . "chat") (id . "one"))
                  (body () "atomic"))))
      (should-error (jabber-test-archive--fin jc query "uid")))
    (jabber-db-close)
    (should-not (jabber-test-archive--progress))
    (should-not (sqlite-select jabber-db--connection "SELECT id FROM message"))
    (sqlite-execute jabber-db--connection "DROP TRIGGER reject_progress")
    (let ((query (jabber-mam--query jc)))
      (jabber-test-archive--fin jc query "retry"))
    (jabber-db-close)
    (should (equal '("retry" nil) (jabber-test-archive--progress)))))

(ert-deftest jabber-test-archive-pagination-send-failure ()
  "A failed next-page send retains only the already committed predecessor."
  (jabber-test-archive--with-file
    (let ((query (jabber-mam--query jc)))
      (jabber-mam--handle-fin jc
       '(iq ((from . "me@example.com"))
            (fin ((xmlns . "urn:xmpp:mam:2"))
                 (set ((xmlns . "http://jabber.org/protocol/rsm"))
                      (last () "accepted"))))
       (cons query (plist-get query :page)))
      (cancel-timer (plist-get query :timer))
      (plist-put query :timer nil)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (&rest _) (error "Send failed"))))
        (jabber-mam--send-page query))
      (should-not jabber-mam--syncing))
    (jabber-db-close)
    (should (equal '("accepted" nil) (jabber-test-archive--progress)))))

(provide 'jabber-test-archive-persistence)
;;; jabber-test-archive-persistence.el ends here
