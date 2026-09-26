;;; jabber-test-archive-recovery.el --- Native rollback and reply proof -*- lexical-binding: t; -*-

(require 'jabber-test-archive-persistence)
(require 'jabber-omemo-store)
(require 'jabber-chat)

(ert-deftest jabber-test-archive-recovery-native-store-full ()
  "Reject orphan receipts after a shared OMEMO write rolls SQLite back."
  (dolist (paginate '(nil t))
    (jabber-test-archive--with-file
      (let ((jabber-mam--tx-progress nil)
            (jabber-mam--peer-syncing nil))
	(jabber-test-archive--fin jc (jabber-mam--query jc) "predecessor")
	(sqlite-execute jabber-db--connection
			(format "PRAGMA max_page_count=%d"
				(+ 20 (caar (sqlite-select jabber-db--connection "PRAGMA page_count")))))
	(let* ((first (jabber-mam--catch-up jc))
               (second (jabber-mam--query jc nil nil "friend@example.com"))
               (inner '(message ((from . "friend@example.com/phone")
                                 (type . "chat") (id . "lost"))
				(body () "must survive with cursor")))
               outcome)
          (should (equal "predecessor" (plist-get first :after)))
          (plist-put second :result-callback (lambda (value) (setq outcome value)))
          (jabber-mam--process-message jc
				       (jabber-test-archive--result first "lost-message-uid" inner))
          (if paginate
              (jabber-mam--handle-fin
               jc '(iq () (fin ((xmlns . "urn:xmpp:mam:2"))
                               (set ((xmlns . "http://jabber.org/protocol/rsm"))
                                    (last () "lost-message-uid"))))
               (cons first (plist-get first :page)))
            (jabber-test-archive--fin jc first "lost-message-uid"))
          ;; A real application writer, not a mocked execute or trigger, causes
          ;; SQLITE_FULL to roll back the *whole* physical transaction.
          (let ((err (should-error
                      (jabber-omemo-store-save "me@example.com"
					       (make-string (* 1024 1024) ?x)) :type 'sqlite-error)))
            (should (string-match-p "full" (error-message-string err))))
          (should-not (sqlite-select jabber-db--connection "SELECT body FROM message"))
          (should-error (jabber-test-archive--fin jc second nil))
          (should (eq outcome 'failed))
          (jabber-db-close)
          (should (equal '("predecessor" nil) (jabber-test-archive--progress)))
          (should (plist-get first :failed))
          (should-not jabber-mam--syncing)
          (should-not jabber-mam--tx-progress)
          (should-not jabber-mam--dirty-peers)
          (should-not jabber-open-info-queries)
          (should-not (plist-get first :timer))
          (should (zerop jabber-mam--tx-depth))
          (jabber-db-close)
          (should (equal '("predecessor" nil) (jabber-test-archive--progress)))
          (should-not (jabber-test-archive--progress nil nil "friend@example.com"))
          (should-not (sqlite-select jabber-db--connection "SELECT body FROM message"))
          (let ((retry (jabber-mam--catch-up jc)))
            (should (equal "predecessor" (plist-get retry :after)))
            (jabber-mam--process-message jc
					 (jabber-test-archive--result retry "lost-message-uid" inner))
            (jabber-test-archive--fin jc retry "lost-message-uid"))
          (jabber-db-close)
          (should (equal '("lost-message-uid" nil) (jabber-test-archive--progress)))
          (should (equal '(("must survive with cursor"))
                         (sqlite-select jabber-db--connection "SELECT body FROM message"))))))))

(ert-deftest jabber-test-archive-recovery-statement-local-fault ()
  "Keep valid page effects when only another writer's statement aborts."
  (jabber-test-archive--with-file
    (sqlite-execute jabber-db--connection "CREATE TABLE unique_control (id INTEGER PRIMARY KEY)")
    (sqlite-execute jabber-db--connection "INSERT INTO unique_control VALUES (1)")
    (let ((first (jabber-mam--query jc))
          (second (jabber-mam--query jc nil nil "friend@example.com")))
      (jabber-mam--process-message jc
				   (jabber-test-archive--result first "retained"
								'(message ((from . "friend@example.com/phone") (type . "chat") (id . "one"))
									  (body () "statement survives"))))
      (jabber-test-archive--fin jc first "retained")
      (should-error (sqlite-execute jabber-db--connection "INSERT INTO unique_control VALUES (1)")
                    :type 'sqlite-error)
      (jabber-test-archive--fin jc second nil))
    (jabber-db-close)
    (should (equal '("retained" nil) (jabber-test-archive--progress)))
    (should (equal '(("statement survives"))
                   (sqlite-select jabber-db--connection "SELECT body FROM message")))))

(ert-deftest jabber-test-archive-recovery-statement-full ()
  "An FTS correction SQLITE_FULL can abort only its statement, not the page."
  (jabber-test-archive--with-file
    (sqlite-execute jabber-db--connection
		    (format "PRAGMA max_page_count=%d"
			    (+ 20 (caar (sqlite-select jabber-db--connection "PRAGMA page_count")))))
    (let ((first (jabber-mam--query jc))
          (second (jabber-mam--query jc nil nil "friend@example.com")))
      (jabber-mam--process-message jc
				   (jabber-test-archive--result first "retained"
								'(message ((from . "friend@example.com/phone") (type . "chat") (id . "original"))
									  (body () "retained body"))))
      (jabber-test-archive--fin jc first "retained")
      (let ((err
             (should-error
              (jabber-mam--process-message jc
					   (jabber-test-archive--result second "fault"
									`(message ((from . "friend@example.com/phone") (type . "chat") (id . "edit"))
										  (body () ,(make-string (* 1024 1024) ?x))
										  (replace ((xmlns . "urn:xmpp:message-correct:0") (id . "original"))))))
              :type 'sqlite-error)))
        (should (string-match-p "full" (error-message-string err))))
      (jabber-test-archive--fin jc second "fault"))
    (jabber-db-close)
    (should (equal '("retained" nil) (jabber-test-archive--progress)))
    (should-not (jabber-test-archive--progress nil nil "friend@example.com"))
    (should (equal '(("retained body"))
                   (sqlite-select jabber-db--connection "SELECT body FROM message")))))

(ert-deftest jabber-test-archive-recovery-replacement-transaction ()
  "A new empty physical transaction cannot bless old accepted receipts."
  (jabber-test-archive--with-file
    (let ((first (jabber-mam--query jc))
          (second (jabber-mam--query jc nil nil "friend@example.com")))
      (jabber-test-archive--fin jc first "lost")
      (sqlite-execute jabber-db--connection "ROLLBACK")
      (sqlite-execute jabber-db--connection "BEGIN")
      (should-error (jabber-test-archive--fin jc second nil) :type 'sqlite-error)
      (should-not jabber-mam--tx-progress)
      (should-not jabber-mam--syncing)
      (should (zerop jabber-mam--tx-depth)))
    (jabber-db-close)
    (should-not (jabber-test-archive--progress))))

(ert-deftest jabber-test-archive-recovery-native-commit-fault ()
  "A deferred foreign-key failure at native COMMIT invalidates all receipts."
  (jabber-test-archive--with-file
    (sqlite-execute jabber-db--connection "CREATE TABLE control_parent(id INTEGER PRIMARY KEY)")
    (sqlite-execute jabber-db--connection "CREATE TABLE control_child(parent INTEGER REFERENCES control_parent(id) DEFERRABLE INITIALLY DEFERRED)")
    (let ((query (jabber-mam--query jc)) outcome)
      (plist-put query :result-callback (lambda (value) (setq outcome value)))
      (jabber-mam--process-message jc
				   (jabber-test-archive--result query "uid"
								'(message ((from . "friend@example.com/phone") (type . "chat") (id . "one"))
									  (body () "atomic"))))
      (sqlite-execute jabber-db--connection "INSERT INTO control_child VALUES (999)")
      (should-error (jabber-test-archive--fin jc query "uid") :type 'sqlite-error)
      (should (eq 'failed outcome))
      (should (zerop jabber-mam--tx-depth))
      (should-not jabber-mam--tx-progress)
      (should-not jabber-mam--dirty-peers))
    (jabber-db-close)
    (should-not (jabber-test-archive--progress))
    (should-not (sqlite-select jabber-db--connection "SELECT * FROM message"))
    (should-not (sqlite-select jabber-db--connection "SELECT * FROM control_child"))
    (jabber-test-archive--fin jc (jabber-mam--query jc) "retry")
    (should (equal '("retry" nil) (jabber-test-archive--progress)))))

(ert-deftest jabber-test-archive-recovery-outgoing-reply-context ()
  "Resolve native outgoing replies before and after archive enrichment."
  (jabber-test-archive--with-file
    (with-temp-buffer
      (setq-local jabber-buffer-connection jc)
      (setq-local jabber-chatting-with "friend@example.com")
      (let ((jabber-chat-encryption nil)
            (jabber-chat--sending-correction nil)
            (jabber-chat--send-hook-stanza nil))
	(jabber-db--outgoing-handler "original outgoing body" "sent-id"))
      (dolist (enrich '(nil t))
	(when enrich
          (let ((query (jabber-mam--query jc)))
            (jabber-mam--process-message jc
					 (jabber-test-archive--result query "archive-uid"
								      '(message ((from . "me@example.com/desktop") (to . "friend@example.com")
										 (type . "chat") (id . "sent-id"))
										(body () "original outgoing body")
										(origin-id ((xmlns . "urn:xmpp:sid:0") (id . "origin-id"))))))
            (jabber-test-archive--fin jc query "archive-uid")))
	(dolist (sender '("me@example.com" "me@example.com/desktop"))
          (jabber-db--message-handler jc
				      `(message ((from . "friend@example.com/phone") (type . "chat")
						 (id . ,(concat "reply-" sender)))
						(body () "answer")
						(reply ((xmlns . "urn:xmpp:reply:0") (id . "sent-id") (to . ,sender)))))
          (jabber-db-close)
          (let* ((rows (jabber-db-backlog "me@example.com" "friend@example.com"))
		 (reply (seq-find (lambda (msg) (equal (plist-get msg :id)
                                                       (concat "reply-" sender))) rows)))
            (should (equal "original outgoing body" (jabber-chat--reply-context-snippet reply)))))
	(when enrich
          (should (equal "original outgoing body"
			 (jabber-db-reply-target-body "me@example.com" "friend@example.com"
                                                      "origin-id" nil "me@example.com/desktop")))))
      (should (= 1 (caar (sqlite-select jabber-db--connection
					"SELECT count(*) FROM message WHERE direction = 'out'")))))))

(ert-deftest jabber-test-archive-recovery-reply-sender-controls ()
  "Unknown outgoing resource must not weaken known conflicts or ambiguity."
  (jabber-test-archive--with-file
    (ignore jc)
    (jabber-db-store-message "me@example.com" "friend@example.com" "out" "chat"
                             "known" 1 "desktop" "known-id")
    (jabber-db-store-message "me@example.com" "friend@example.com" "out" "chat"
                             "unknown" 2 nil "unknown-id")
    (dolist (case '(("known-id" "me@example.com/phone")
                    ("unknown-id" "other@example.com/desktop")
                    ("unknown-id" "friend@example.com/phone")))
      (should-not (jabber-db-reply-target-body "me@example.com" "friend@example.com"
                                               (car case) nil (cadr case))))
    (should-not (jabber-db-reply-target-body "other@example.com" "friend@example.com"
                                             "unknown-id" nil "me@example.com/desktop"))
    (should-not (jabber-db-reply-target-body "me@example.com" "other@example.com"
                                             "unknown-id" nil "me@example.com/desktop"))
    (should-not (jabber-db-reply-target-body "me@example.com" "friend@example.com"
                                             "unknown-id" t "me@example.com/desktop"))
    ;; Unknown resources remain strict for inbound/room messages.
    (jabber-db-store-message "me@example.com" "friend@example.com" "in" "chat"
                             "incoming" 3 nil "incoming-id")
    (should-not (jabber-db-reply-target-body "me@example.com" "friend@example.com"
                                             "incoming-id" nil "friend@example.com/phone"))
    (jabber-db-store-message "me@example.com" "friend@example.com" "out" "chat"
                             "alias collision" 4 "desktop" "second-id" nil nil nil nil nil nil
                             '(:origin-id "unknown-id"))
    (jabber-db-close)
    (should-not (jabber-db-reply-target-body "me@example.com" "friend@example.com"
                                             "unknown-id" nil "me@example.com/desktop"))
    (let ((row (caar (sqlite-select jabber-db--connection
                                    "SELECT id FROM message WHERE stanza_id = 'known-id'"))))
      (jabber-db-retract-message-row row nil))
    (jabber-db-close)
    (should-not (jabber-db-reply-target-body "me@example.com" "friend@example.com"
                                             "known-id" nil "me@example.com/desktop"))))

(provide 'jabber-test-archive-recovery)
;;; jabber-test-archive-recovery.el ends here
