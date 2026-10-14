;;; jabber-test-history-cost.el --- Bounded history reads and replay writes -*- lexical-binding: t; -*-

;;; Commentary:
;; Archive replay must not rewrite unchanged rows, and thread views must
;; convert only the rows they display.  Parent backlog thread summaries
;; keep their existing selection among threads that share a root.

;;; Code:

(require 'jabber-archive-test-helpers)
(require 'jabber-message-reply)

;;; Replay writes

(defmacro jabber-test-history-cost--with-db (&rest body)
  "Run BODY with a disposable database."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "jabber-history-cost-" t))
          (jabber-db-path (expand-file-name "history.sqlite" directory))
          (jabber-db--connection nil)
          (jabber-backlog-days nil)
          (jabber-backlog-number 30))
     (unwind-protect
         (progn (jabber-db-ensure-open) ,@body)
       (jabber-db-close)
       (delete-directory directory t))))

(defun jabber-test-history-cost--updates (thunk)
  "Return the number of message row UPDATEs made by calling THUNK."
  (let ((db jabber-db--connection))
    (sqlite-execute db "CREATE TEMP TABLE IF NOT EXISTS history_cost_update (id INTEGER)")
    (sqlite-execute db "DELETE FROM history_cost_update")
    (sqlite-execute db "CREATE TEMP TRIGGER IF NOT EXISTS history_cost_update_message
AFTER UPDATE ON main.message BEGIN
INSERT INTO history_cost_update VALUES (new.id); END")
    (unwind-protect
        (progn (funcall thunk)
               (caar (sqlite-select db "SELECT count(*) FROM history_cost_update")))
      (sqlite-execute db "DROP TRIGGER history_cost_update_message"))))

(defun jabber-test-history-cost--replay (jc inner uid)
  "Replay INNER on JC with UID through a complete page after the cursor.
An overlapping server page redelivers UID after the durable cursor."
  (let ((query (jabber-mam--query jc (car (jabber-test-archive--progress)))))
    (jabber-mam--process-message
     jc (jabber-test-archive--result query uid (copy-tree inner)))
    (jabber-test-archive--fin jc query uid)))

(defun jabber-test-history-cost--direct (&optional origin encrypted)
  "Return a direct chat message with ORIGIN, ENCRYPTED when non-nil."
  `(message ((from . "friend@example.com/phone") (to . "me@example.com")
             (type . "chat") (id . "transport"))
            (body () ,(if encrypted "OMEMO fallback" "retained"))
            ,@(when origin
                `((origin-id ((xmlns . "urn:xmpp:sid:0") (id . ,origin)))))
            ,@(when encrypted
                '((encrypted ((xmlns . "eu.siacs.conversations.axolotl"))
                             (payload () "ciphertext"))))))

(defun jabber-test-history-cost--counts ()
  "Return message, archive alias and searchable row counts."
  (mapcar (lambda (sql) (caar (sqlite-select jabber-db--connection sql)))
          '("SELECT count(*) FROM message"
            "SELECT count(*) FROM message_archive"
            "SELECT count(*) FROM message_fts WHERE message_fts MATCH 'retained OR secret'")))

(defun jabber-test-history-cost--identity ()
  "Return the stored identity columns of the only message row."
  (car (sqlite-select jabber-db--connection
                      "SELECT stanza_id, server_id, origin_id, room_id, body FROM message")))

(ert-deftest jabber-test-history-cost-exact-replay-writes-nothing ()
  "Exact archive replay keeps the row, aliases and presentation signal."
  (jabber-test-archive--with-file
    (let ((xml (jabber-test-history-cost--direct "origin"))
          dirty)
      (jabber-test-history-cost--replay jc xml "uid-1")
      (setq jabber-mam-sync-complete-functions
            (list (lambda (peers) (setq dirty peers))))
      (should (= 0 (jabber-test-history-cost--updates
                    (lambda () (jabber-test-history-cost--replay jc xml "uid-1")))))
      ;; Replay remains a dirty-peer settlement; redraw policy is unchanged.
      (should (equal '(("me@example.com" "friend@example.com" "chat")) dirty))
      (should (equal '(1 1 1) (jabber-test-history-cost--counts)))
      (jabber-db-close)
      (jabber-db-ensure-open)
      (should (equal '("transport" "uid-1" "origin" nil "retained")
                     (jabber-test-history-cost--identity)))
      (should (equal "uid-1" (car (jabber-test-archive--progress))))
      (should (sqlite-execute jabber-db--connection
                              "INSERT INTO message_fts(message_fts) VALUES ('integrity-check')")))))

(ert-deftest jabber-test-history-cost-direct-store-replay-keeps-row-id ()
  "Repeated store calls return the canonical row without rewriting it."
  (jabber-test-history-cost--with-db
    (let* ((identity '(:origin-id "origin" :room-id nil
                       :archive "me@example.com" :archive-id "uid-1"))
           (store (lambda ()
                    (jabber-db-store-message
                     "me@example.com" "friend@example.com" "in" "chat"
                     "retained" 100 "phone" "transport" "uid-1"
                     nil nil nil nil nil identity)))
           (id (funcall store)))
      (should (= 0 (jabber-test-history-cost--updates
                    (lambda () (should (equal id (funcall store)))))))
      (should (equal '(1 1 1) (jabber-test-history-cost--counts))))))

(ert-deftest jabber-test-history-cost-encrypted-cache-replay-writes-nothing ()
  "A cached decryption is replayed without ratchet use or row rewrites."
  (jabber-test-archive--with-file
    (let ((jabber-chat-decrypt-handlers nil)
          (jabber-chat--sorted-decrypt-handlers-cache nil)
          (jabber-chat--crypto-loaded t)
          (jabber-chat--decrypt-cache (make-hash-table :test #'equal))
          (runs 0)
          (xml (jabber-test-history-cost--direct "origin" t)))
      (jabber-chat-register-decrypt-handler
       'history-cost-omemo
       :detect (lambda (stanza)
                 (jabber-xml-child-with-xmlns
                  stanza "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc stanza _parsed)
                  (setq runs (1+ runs))
                  (jabber-chat--set-body stanza "secret"))
       :priority 10
       :error-label "OMEMO")
      (jabber-test-history-cost--replay jc xml "uid-1")
      (should (= 0 (jabber-test-history-cost--updates
                    (lambda () (jabber-test-history-cost--replay jc xml "uid-1")))))
      (should (= 1 runs))
      (should (equal '(1 1 1) (jabber-test-history-cost--counts)))
      (should (equal '("transport" "uid-1" "origin" nil "secret")
                     (jabber-test-history-cost--identity))))))

(ert-deftest jabber-test-history-cost-replay-fills-missing-identity ()
  "Replay still fills NULL transport, archive and origin identity once."
  (jabber-test-archive--with-file
    (jabber-db--message-handler jc (jabber-test-history-cost--direct))
    (should (equal '("transport" nil nil nil "retained")
                   (jabber-test-history-cost--identity)))
    (let ((xml (jabber-test-history-cost--direct "origin")))
      (should (< 0 (jabber-test-history-cost--updates
                    (lambda () (jabber-test-history-cost--replay jc xml "uid-1")))))
      (should (equal '("transport" "uid-1" "origin" nil "retained")
                     (jabber-test-history-cost--identity)))
      (should (= 0 (jabber-test-history-cost--updates
                    (lambda () (jabber-test-history-cost--replay jc xml "uid-1")))))
      (should (equal '(1 1 1) (jabber-test-history-cost--counts))))))

(ert-deftest jabber-test-history-cost-room-identity-fills-only-null ()
  "Room identity and a missing server ID are each added exactly when absent."
  (jabber-test-history-cost--with-db
    (let ((store (lambda (server identity)
                   (jabber-db-store-message
                    "me@example.com" "room@conference.example.com" "in"
                    "groupchat" "room text" 100 "Alice" "client" server
                    "occupant" nil nil nil nil identity)))
          (room '(:origin-id nil :room-id "room-sid")))
      (let ((id (funcall store nil nil)))
        ;; One statement fills the server ID, another the room identity.
        (should (= 2 (jabber-test-history-cost--updates
                      (lambda () (should (equal id (funcall store "room-sid" room)))))))
        (should (= 0 (jabber-test-history-cost--updates
                      (lambda () (should (equal id (funcall store "room-sid" room)))))))
        (should (equal '("client" "room-sid" nil "room-sid" "room text")
                       (jabber-test-history-cost--identity)))))))

;;; Thread backlog

(defun jabber-test-history-cost--insert (&rest columns)
  "Insert one message from plist COLUMNS and return its row ID."
  (let ((columns (append columns '(:account "me@x" :peer "peer@x"
                                   :direction "in" :type "chat"))))
    (sqlite-execute
     jabber-db--connection
     "INSERT INTO message (id, account, peer, direction, type, body, timestamp,
stanza_id, server_id, room_id, thread_id)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (mapcar (lambda (key) (plist-get columns key))
             '(:id :account :peer :direction :type :body :timestamp
               :stanza-id :server-id :room-id :thread-id)))
    (caar (sqlite-select jabber-db--connection "SELECT last_insert_rowid()"))))

(defun jabber-test-history-cost--thread (thread-id &rest root)
  "Register dedicated THREAD-ID with ROOT plist identity."
  (sqlite-execute
   jabber-db--connection
   "INSERT INTO message_thread (account, peer, type, thread_id, root_message_id,
root_stanza_id, root_server_id, created_at, dedicated, title)
VALUES (?, ?, ?, ?, ?, ?, ?, 0, 1, ?)"
   (list (or (plist-get root :account) "me@x") "peer@x"
         (or (plist-get root :type) "chat") thread-id
         (plist-get root :id) (plist-get root :stanza-id)
         (plist-get root :server-id) (plist-get root :title))))

(defun jabber-test-history-cost--bodies (messages)
  "Return MESSAGES' bodies."
  (mapcar (lambda (message) (plist-get message :body)) messages))

(defun jabber-test-history-cost--thread-bodies (count &optional start-time type)
  "Return bodies of thread \"t\" for COUNT, START-TIME and TYPE."
  (jabber-test-history-cost--bodies
   (jabber-db-thread-backlog "me@x" "peer@x" (or type "chat") "t"
                             count start-time)))

(ert-deftest jabber-test-history-cost-thread-backlog-bounded-rows ()
  "A short thread view converts only its root and newest replies."
  (jabber-test-history-cost--with-db
    (jabber-test-history-cost--insert :id 1 :body "root" :timestamp 1)
    (jabber-test-history-cost--thread "t" :id 1)
    (dotimes (i 200)
      (jabber-test-history-cost--insert
       :body (format "reply %d" i) :timestamp (+ 10 i) :thread-id "t"))
    (let ((converted 0)
          (convert (symbol-function 'jabber-db--row-to-plist)))
      (cl-letf (((symbol-function 'jabber-db--row-to-plist)
                 (lambda (row) (setq converted (1+ converted))
                   (funcall convert row))))
        (should (equal '("reply 199" "reply 198" "reply 197" "reply 196" "root")
                       (jabber-test-history-cost--thread-bodies 5))))
      (should (= 5 converted)))))

(ert-deftest jabber-test-history-cost-thread-backlog-counts-and-cutoff ()
  "Counts keep the root plus newest replies; cutoff can exclude the root."
  (jabber-test-history-cost--with-db
    (jabber-test-history-cost--insert :id 1 :body "root" :timestamp 1)
    (jabber-test-history-cost--thread "t" :id 1)
    (dolist (spec '((2 "a" 10) (3 "b" 20) (4 "c" 20) (5 "d" 30)))
      (jabber-test-history-cost--insert
       :id (car spec) :body (cadr spec) :timestamp (nth 2 spec) :thread-id "t"))
    ;; Unrelated account, peer and type rows share the thread ID.
    (jabber-test-history-cost--insert :body "foreign" :timestamp 40
                                      :thread-id "t" :account "other@x")
    (jabber-test-history-cost--insert :body "muc" :timestamp 40
                                      :thread-id "t" :type "groupchat")
    (should (equal '("root") (jabber-test-history-cost--thread-bodies 0)))
    (should (equal '("root") (jabber-test-history-cost--thread-bodies 1)))
    (should (equal '("d" "c" "root") (jabber-test-history-cost--thread-bodies 3)))
    ;; Equal timestamps keep the newer row first.
    (should (equal '("d" "c" "b" "a" "root")
                   (jabber-test-history-cost--thread-bodies t)))
    (should (equal '("d" "c" "b" "a" "root")
                   (jabber-test-history-cost--thread-bodies nil)))
    (should (equal '("d" "c") (jabber-test-history-cost--thread-bodies 3 5)))
    (should-not (jabber-db-thread-backlog "me@x" "peer@x" "chat" "absent" t))))

(ert-deftest jabber-test-history-cost-thread-backlog-root-fallbacks ()
  "Historical root IDs follow row, room-server and stanza precedence."
  (jabber-test-history-cost--with-db
    ;; The newest row carrying a fallback stanza ID is the root; an older
    ;; copy remains an ordinary reply.
    (jabber-test-history-cost--insert :body "old copy" :timestamp 1
                                      :stanza-id "root-s" :thread-id "t")
    (jabber-test-history-cost--insert :body "root" :timestamp 2 :stanza-id "root-s")
    (jabber-test-history-cost--insert :body "reply" :timestamp 3 :thread-id "t")
    (jabber-test-history-cost--thread "t" :stanza-id "root-s")
    (should (equal '("reply" "root") (jabber-test-history-cost--thread-bodies 2)))
    (should (equal '("reply" "old copy" "root")
                   (jabber-test-history-cost--thread-bodies t)))
    ;; Groupchat roots match a server ID or a room ID in thread rows.
    (jabber-test-history-cost--insert :body "muc root" :timestamp 5 :type "groupchat"
                                      :server-id "legacy" :room-id "room-root"
                                      :thread-id "t")
    (jabber-test-history-cost--insert :body "muc reply" :timestamp 6 :type "groupchat"
                                      :thread-id "t" :stanza-id "root-s")
    (jabber-test-history-cost--thread "t" :type "groupchat" :server-id "room-root"
                                      :stanza-id "root-s")
    (should (equal '("muc reply" "muc root")
                   (jabber-test-history-cost--thread-bodies 2 nil "groupchat")))
    ;; Without root identity only replies remain.
    (jabber-test-history-cost--insert :body "loose" :timestamp 7 :thread-id "u")
    (jabber-test-history-cost--thread "u")
    (should (equal '("loose")
                   (jabber-test-history-cost--bodies
                    (jabber-db-thread-backlog "me@x" "peer@x" "chat" "u" 5))))))

(ert-deftest jabber-test-history-cost-thread-root-timestamp-tie ()
  "Equal-time copies of a stanza root keep the newer row as root."
  (jabber-test-history-cost--with-db
    (jabber-test-history-cost--insert :id 1 :body "first" :timestamp 5 :stanza-id "s")
    (jabber-test-history-cost--insert :id 2 :body "second" :timestamp 5 :stanza-id "s")
    (jabber-test-history-cost--insert :id 3 :body "reply" :timestamp 5 :thread-id "t")
    (jabber-test-history-cost--thread "t" :stanza-id "s")
    (should (equal '("reply" "first" "second")
                   (jabber-test-history-cost--thread-bodies t)))
    (should (equal '("reply" "second")
                   (jabber-test-history-cost--thread-bodies 2)))))

;;; Thread summaries in the parent backlog

(ert-deftest jabber-test-history-cost-summaries-match-root-identities ()
  "Row, room-server and stanza roots attach only within their own type."
  (jabber-test-history-cost--with-db
    (let ((row-root (jabber-test-history-cost--insert
                     :body "row root" :timestamp 1 :stanza-id "shared")))
      (jabber-test-history-cost--insert :body "server root" :timestamp 2
                                        :type "groupchat" :room-id "room-root"
                                        :server-id "legacy" :stanza-id "shared")
      (jabber-test-history-cost--insert :body "stanza root" :timestamp 3
                                        :stanza-id "chat-root")
      (jabber-test-history-cost--thread "by-row" :id row-root)
      (jabber-test-history-cost--thread "by-server" :type "groupchat"
                                        :server-id "room-root" :stanza-id "shared")
      (jabber-test-history-cost--thread "by-stanza" :stanza-id "chat-root")
      ;; A groupchat thread cannot claim a chat row by its row ID.
      (jabber-test-history-cost--thread "wrong-type" :type "groupchat" :id row-root)
      (jabber-test-history-cost--thread "other-account" :account "other@x" :id row-root)
      (let ((threads (mapcar (lambda (message)
                               (cons (plist-get message :body)
                                     (plist-get (plist-get message :thread-summary)
                                                :thread-id)))
                             (jabber-db-backlog "me@x" "peer@x" t 0))))
        (should (equal '(("stanza root" . "by-stanza")
                         ("server root" . "by-server")
                         ("row root" . "by-row"))
                       threads)))
      (should-not (jabber-db--attach-thread-summaries
                   jabber-db--connection "me@x" "peer@x" nil)))))

;; Several dedicated threads may claim one root.  The parent backlog
;; attaches the claimant that comes last in its unordered metadata read,
;; so the winner depends on the database's query plan, including SQLite
;; statistics persisted by an earlier ANALYZE.  Cost changes must keep
;; that selection until a deliberate contract replaces it.

(defun jabber-test-history-cost--last-claimant (claimants)
  "Return the last of CLAIMANTS in the parent backlog's thread read."
  (car (last (seq-filter
              (lambda (thread-id) (member thread-id claimants))
              (mapcar #'car (sqlite-select
                             jabber-db--connection
                             "SELECT thread_id FROM message_thread \
WHERE account = ? AND peer = ? AND dedicated = 1"
                             '("me@x" "peer@x")))))))

(defun jabber-test-history-cost--shared-root-view ()
  "Return the parent root's summary fields and consumed reply thread."
  (let* ((root (seq-find (lambda (message)
                           (equal "root" (plist-get message :body)))
                         (jabber-db-backlog "me@x" "peer@x" t 0)))
         (summary (plist-get root :thread-summary)))
    (list (plist-get summary :thread-id) (plist-get summary :title)
          (plist-get summary :reply-count) (plist-get summary :unread)
          (jabber-message-reply--thread-fields root))))

(defun jabber-test-history-cost--analyzed-p ()
  "Return non-nil when thread metadata has persisted SQLite statistics."
  (let ((db jabber-db--connection))
    (and (sqlite-select db "SELECT 1 FROM sqlite_master WHERE name = 'sqlite_stat1'")
         (sqlite-select db "SELECT 1 FROM sqlite_stat1 WHERE tbl = 'message_thread'")
         t)))

(ert-deftest jabber-test-history-cost-summaries-shared-root-selection ()
  "Shared-root summaries follow the metadata read fresh and after ANALYZE."
  (jabber-test-history-cost--with-db
    (let ((root (jabber-test-history-cost--insert :id 1 :body "root" :timestamp 1
                                                  :stanza-id "s"))
          (claimants '("z" "a" "m"))
          (expected '(("z" "Z topic" 0 nil) ("a" "A topic" 1 nil)
                      ("m" "M topic" 2 t))))
      ;; Insertion order differs from key order; each claimant is distinct.
      (jabber-db-register-message-thread "me@x" "peer@x" "chat" "z" nil "s" nil 1)
      (jabber-db-register-message-thread "me@x" "peer@x" "chat" "a" nil "s" nil 1 root)
      (jabber-db-register-message-thread "me@x" "peer@x" "chat" "m" nil "s" nil 1)
      (dolist (thread expected)
        (jabber-db-set-message-thread-title "me@x" "peer@x" "chat"
                                            (car thread) (cadr thread)))
      (jabber-test-history-cost--insert :id 2 :body "a1" :timestamp 2
                                        :thread-id "a" :stanza-id "a1")
      (jabber-db-mark-message-thread-read "me@x" "peer@x" "chat" "a")
      (jabber-test-history-cost--insert :id 3 :body "m1" :timestamp 3
                                        :thread-id "m" :stanza-id "m1")
      (jabber-test-history-cost--insert :id 4 :body "m2" :timestamp 4
                                        :thread-id "m" :stanza-id "m2")
      (dolist (state '(fresh analyzed reopened))
        (pcase state
          ('analyzed (sqlite-execute jabber-db--connection "ANALYZE"))
          ('reopened (jabber-db-close) (jabber-db-ensure-open)))
        (should (eq (not (eq state 'fresh))
                    (jabber-test-history-cost--analyzed-p)))
        (let ((winner (jabber-test-history-cost--last-claimant claimants)))
          (should (equal (list state
                               (append (assoc winner expected)
                                       (list (list :thread-id winner
                                                   :thread-parent-id nil))))
                         (list state
                               (jabber-test-history-cost--shared-root-view)))))))))

(provide 'jabber-test-history-cost)
;;; jabber-test-history-cost.el ends here
