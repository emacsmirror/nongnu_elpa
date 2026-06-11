;;; jabber-test-chatbuffer.el --- Tests for jabber-chatbuffer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared chat buffer infrastructure (ewoc, message nodes).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewoc)
(require 'jabber-chatbuffer)
(require 'jabber-chat)
(require 'jabber-db)
(require 'jabber-httpupload)

;; jabber-chat requires this via jabber-muc
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")
(defvar jabber-group nil)
(defvar jabber-muc-participants nil)
(defvar jabber-scrolltobottom-all nil)

(declare-function jabber-chat-buffer-recenter-input "jabber-chatbuffer" ())
(declare-function jabber-chat-buffer--recenter-input-window
                  "jabber-chatbuffer" (window))

;;; Test helpers

(defmacro jabber-test-chatbuffer-with-ewoc (&rest body)
  "Set up a temp buffer with a chat ewoc and hash table, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       ,@body)))

;;; Group 1: jabber-chat-ewoc-enter

(ert-deftest jabber-test-chatbuffer-ewoc-enter-registers-id ()
  "Inserting a message with :id registers it in the hash table."
  (jabber-test-chatbuffer-with-ewoc
    (let* ((msg (list :id "msg-001" :body "hello" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (should node)
      (should (eq node (gethash "msg-001" jabber-chat--msg-nodes))))))

(ert-deftest jabber-test-chatbuffer-ewoc-enter-skips-nil-id ()
  "Inserting a message without :id does not pollute the hash table."
  (jabber-test-chatbuffer-with-ewoc
    (let* ((msg (list :body "notice text" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :notice msg))))
      (should node)
      (should (zerop (hash-table-count jabber-chat--msg-nodes))))))

(ert-deftest jabber-test-chatbuffer-ewoc-enter-notice-string ()
  "Inserting a notice with string body does not error."
  (jabber-test-chatbuffer-with-ewoc
    (let ((node (jabber-chat-ewoc-enter (list :notice "Someone joined"
                                              :time (current-time)))))
      (should node)
      (should (zerop (hash-table-count jabber-chat--msg-nodes))))))

(ert-deftest jabber-test-chatbuffer-ewoc-enter-multiple-ids ()
  "Multiple messages with distinct IDs are all registered."
  (jabber-test-chatbuffer-with-ewoc
    (dotimes (i 5)
      (let ((msg (list :id (format "msg-%03d" i) :body "x"
                       :timestamp (current-time))))
        (jabber-chat-ewoc-enter (list :local msg))))
    (should (= 5 (hash-table-count jabber-chat--msg-nodes)))
    (should (gethash "msg-002" jabber-chat--msg-nodes))))

(ert-deftest jabber-test-chatbuffer-shift-undo-list-translates-positions ()
  "Undo entries that contain buffer positions are shifted together."
  (with-temp-buffer
    (let ((marker (point-marker))
          (buffer-undo-list
           (list 4
                 (cons 6 9)
                 (cons "abc" 7)
                 (cons "def" -8)
                 '(nil face bold 10 . 12)
                 nil
                 (cons t 0))))
      (push (cons marker 3) buffer-undo-list)
      (jabber-chat-buffer--shift-undo-list 5)
      (should (equal buffer-undo-list
                     (list (cons marker 3)
                           9
                           (cons 11 14)
                           (cons "abc" 12)
                           (cons "def" -13)
                           '(nil face bold 15 . 17)
                           nil
                           (cons t 0)))))))

(ert-deftest jabber-test-chatbuffer-ewoc-enter-shifts-input-undo ()
  "Inserting chat output keeps typed input undo entries aligned."
  (with-temp-buffer
    (setq buffer-undo-list nil)
    (let ((jabber-chat-ewoc
           (ewoc-create
            (lambda (data)
              (insert (plist-get (cadr data) :body)))
            nil (concat (jabber-separator) "\n") 'nosep))
          (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
      (goto-char (point-max))
      (setq-local jabber-point-insert (point-marker))
      (insert "draft")
      (let ((undo-entry (copy-tree (car buffer-undo-list)))
            (prompt (marker-position jabber-point-insert)))
        (jabber-chat-ewoc-enter
         (list :local (list :id "shift-input" :body "hello")))
        (let ((shift (- jabber-point-insert prompt)))
          (should (cl-plusp shift))
          (should (equal (car buffer-undo-list)
                         (cons (+ (car undo-entry) shift)
                               (+ (cdr undo-entry) shift)))))))))

;;; Group 2: jabber-chat-ewoc-find-by-id

(ert-deftest jabber-test-chatbuffer-find-by-id-returns-node ()
  "Looking up a registered ID returns the correct ewoc node."
  (jabber-test-chatbuffer-with-ewoc
    (let* ((msg (list :id "find-me" :body "test" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (should (eq node (jabber-chat-ewoc-find-by-id "find-me"))))))

(ert-deftest jabber-test-chatbuffer-find-by-id-scans-and-backfills-server-id ()
  "Looking up a stale missing :server-id scans EWOC and backfills it."
  (jabber-test-chatbuffer-with-ewoc
    (let* ((msg (list :id "local-id" :server-id "server-id"
                      :body "test" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (remhash "server-id" jabber-chat--msg-nodes)
      (should (gethash "local-id" jabber-chat--msg-nodes))
      (should-not (gethash "server-id" jabber-chat--msg-nodes))
      (should (eq node (jabber-chat-ewoc-find-by-id "server-id")))
      (should (eq node (gethash "local-id" jabber-chat--msg-nodes)))
      (should (eq node (gethash "server-id" jabber-chat--msg-nodes))))))

(ert-deftest jabber-test-chatbuffer-find-by-id-returns-nil-for-missing ()
  "Looking up a nonexistent ID returns nil."
  (jabber-test-chatbuffer-with-ewoc
    (should-not (jabber-chat-ewoc-find-by-id "no-such-id"))))

(ert-deftest jabber-test-chatbuffer-find-by-id-nil-safe ()
  "Looking up nil returns nil without error."
  (jabber-test-chatbuffer-with-ewoc
    (should-not (jabber-chat-ewoc-find-by-id nil))))

;;; Group 3: In-place status update

(ert-deftest jabber-test-chatbuffer-status-update-in-place ()
  "Mutating :status on the shared plist is visible through the ewoc node."
  (jabber-test-chatbuffer-with-ewoc
    (let* ((msg (list :id "msg-upd" :body "hi" :status :sent
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      ;; Simulate receipt arrival: mutate plist in place
      (plist-put msg :status :delivered)
      ;; The ewoc node shares the same plist object
      (should (eq :delivered (plist-get (cadr (ewoc-data node)) :status))))))

(ert-deftest jabber-test-chatbuffer-status-update-via-lookup ()
  "Status update via find-by-id + plist-put works end-to-end."
  (jabber-test-chatbuffer-with-ewoc
    (let ((msg (list :id "msg-e2e" :body "test" :status :sent
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg))
      ;; Look up and update
      (when-let* ((node (jabber-chat-ewoc-find-by-id "msg-e2e")))
        (plist-put (cadr (ewoc-data node)) :status :displayed))
      ;; Verify the original plist was mutated (shared object)
      (should (eq :displayed (plist-get msg :status))))))

;;; Group 4: Hash table cleanup

(ert-deftest jabber-test-chatbuffer-hash-cleanup-on-clear ()
  "Clearing the hash table via clrhash removes all entries."
  (jabber-test-chatbuffer-with-ewoc
    (dotimes (i 3)
      (let ((msg (list :id (format "clr-%d" i) :body "x"
                       :timestamp (current-time))))
        (jabber-chat-ewoc-enter (list :local msg))))
    (should (= 3 (hash-table-count jabber-chat--msg-nodes)))
    ;; Simulate what jabber-mam--reload-buffer does
    (ewoc-filter jabber-chat-ewoc #'ignore)
    (clrhash jabber-chat--msg-nodes)
    (should (zerop (hash-table-count jabber-chat--msg-nodes)))))

(ert-deftest jabber-test-chatbuffer-hash-remhash-on-delete ()
  "Removing an entry via remhash drops that ID from the table."
  (jabber-test-chatbuffer-with-ewoc
    (let ((msg (list :id "del-me" :body "x" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    (should (gethash "del-me" jabber-chat--msg-nodes))
    (remhash "del-me" jabber-chat--msg-nodes)
    (should-not (gethash "del-me" jabber-chat--msg-nodes))))

;;; Group 5: DB backlog includes stanza ID

(ert-deftest jabber-test-chatbuffer-backlog-includes-stanza-id ()
  "Backlog entries from DB include :id from stanza_id column."
  (skip-unless (fboundp 'sqlite-open))
  (let* ((jabber-db-test--dir (make-temp-file "jabber-db-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" jabber-db-test--dir))
         (jabber-db--connection nil)
         (jabber-backlog-days 3.0)
         (jabber-backlog-number 10))
    (unwind-protect
        (progn
          (jabber-db-ensure-open)
          ;; Insert a message with stanza_id
          (sqlite-execute jabber-db--connection
                          "INSERT INTO message (account, peer, direction, body, timestamp, stanza_id)
                           VALUES (?, ?, ?, ?, ?, ?)"
                          (list "me@example.com" "them@example.com" "out"
                                "Hello" (floor (float-time)) "emacs-msg-1234"))
          (let* ((entries (jabber-db-backlog "me@example.com" "them@example.com"))
                 (entry (car entries)))
            (should entry)
            (should (equal "emacs-msg-1234" (plist-get entry :id)))))
      (jabber-db-close)
      (when (file-directory-p jabber-db-test--dir)
        (delete-directory jabber-db-test--dir t)))))

(ert-deftest jabber-test-chatbuffer-backlog-status-from-receipts ()
  "Backlog entries derive :status from delivered_at/displayed_at."
  (skip-unless (fboundp 'sqlite-open))
  (let* ((jabber-db-test--dir (make-temp-file "jabber-db-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" jabber-db-test--dir))
         (jabber-db--connection nil)
         (jabber-backlog-days 3.0)
         (jabber-backlog-number 10)
         (now (floor (float-time))))
    (unwind-protect
        (progn
          (jabber-db-ensure-open)
          ;; Sent, no receipt
          (sqlite-execute jabber-db--connection
                          "INSERT INTO message (account, peer, direction, body, timestamp, stanza_id)
                           VALUES (?, ?, ?, ?, ?, ?)"
                          (list "me@x.com" "them@x.com" "out" "a" now "id-sent"))
          ;; Delivered
          (sqlite-execute jabber-db--connection
                          "INSERT INTO message (account, peer, direction, body, timestamp, stanza_id, delivered_at)
                           VALUES (?, ?, ?, ?, ?, ?, ?)"
                          (list "me@x.com" "them@x.com" "out" "b" now "id-del" now))
          ;; Displayed
          (sqlite-execute jabber-db--connection
                          "INSERT INTO message (account, peer, direction, body, timestamp, stanza_id, delivered_at, displayed_at)
                           VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                          (list "me@x.com" "them@x.com" "out" "c" now "id-disp" now now))
          (let ((entries (jabber-db-backlog "me@x.com" "them@x.com")))
            ;; Entries are DESC, reverse to get chronological
            (let ((by-id (make-hash-table :test 'equal)))
              (dolist (e entries)
                (puthash (plist-get e :id) e by-id))
              (should-not (plist-get (gethash "id-sent" by-id) :status))
              (should (eq :delivered (plist-get (gethash "id-del" by-id) :status)))
              (should (eq :displayed (plist-get (gethash "id-disp" by-id) :status))))))
      (jabber-db-close)
      (when (file-directory-p jabber-db-test--dir)
        (delete-directory jabber-db-test--dir t)))))

;;; Group 6: :id in message plist from stanza

(ert-deftest jabber-test-chatbuffer-build-msg-plist-includes-id ()
  "jabber-chat--build-msg-plist extracts the stanza id attribute."
  (let* ((stanza '(message ((from . "alice@example.com")
                            (id . "emacs-msg-42")
                            (type . "chat"))
                           (body () "Hello")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "emacs-msg-42" (plist-get plist :id)))))

(ert-deftest jabber-test-chatbuffer-build-msg-plist-nil-id ()
  "jabber-chat--build-msg-plist returns nil :id when stanza has none."
  (let* ((stanza '(message ((from . "alice@example.com")
                            (type . "chat"))
                           (body () "Hello")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :id))))

;;; Group 7: Carbon display suppression

(defun jabber-test-chatbuffer--sent-carbon (inner)
  "Wrap INNER in a sent carbon from the local account."
  `(message ((from . "me@example.com/resource")
             (type . "chat"))
            (sent ((xmlns . "urn:xmpp:carbons:2"))
                  (forwarded ((xmlns . "urn:xmpp:forward:0"))
                             ,inner))))

(defun jabber-test-chatbuffer--received-carbon (inner)
  "Wrap INNER in a received carbon from the local account."
  `(message ((from . "me@example.com/resource")
             (type . "chat"))
            (received ((xmlns . "urn:xmpp:carbons:2"))
                      (forwarded ((xmlns . "urn:xmpp:forward:0"))
                                 ,inner))))

(defmacro jabber-test-chatbuffer-with-process-chat-spies (&rest body)
  "Run BODY with `jabber-process-chat' storage and display spies."
  (declare (indent 0) (debug t))
  `(let ((stored nil)
         (displayed nil)
         (buffer (generate-new-buffer " *test-carbon-chat*"))
         (jabber-chat-printers (list (lambda (&rest _) t))))
     (unwind-protect
         (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                    (lambda (_jc) "me@example.com"))
                   ((symbol-function 'jabber-muc-message-p)
                    (lambda (&rest _) nil))
                   ((symbol-function 'jabber-muc-sender-p)
                    (lambda (&rest _) nil))
                   ((symbol-function 'jabber-chat--decrypt-if-needed)
                    (lambda (_jc xml-data) xml-data))
                   ((symbol-function 'jabber-message-correct--replace-id)
                    (lambda (&rest _) nil))
                   ((symbol-function 'jabber-chat-create-buffer)
                    (lambda (&rest _) buffer))
                   ((symbol-function 'jabber-chat--store-carbon)
                    (lambda (_jc xml-data) (push xml-data stored)))
                   ((symbol-function 'jabber-chat--display-message)
                    (lambda (&rest args) (push args displayed))))
           ,@body)
       (kill-buffer buffer))))

(ert-deftest jabber-test-chatbuffer-reaction-sent-carbon-not-stored-or-displayed ()
  "Reaction fallback sent carbons are not stored or displayed as chat text."
  (jabber-test-chatbuffer-with-process-chat-spies
    (let* ((inner `(message ((from . "me@example.com/phone")
                             (to . "friend@example.com")
                             (type . "chat")
                             (id . "reaction-carbon-1"))
                            (body nil "> hello\n👍")
                            (reactions ((xmlns . ,jabber-reactions-xmlns)
                                        (id . "target-1"))
                                       (reaction nil "👍"))
                            (fallback ((xmlns . "urn:xmpp:fallback:0")
                                       (for . ,jabber-reactions-xmlns)))))
           (carbon (jabber-test-chatbuffer--sent-carbon inner)))
      (jabber-process-chat 'fake-jc carbon)
      (should-not stored)
      (should-not displayed))))

(ert-deftest jabber-test-chatbuffer-reaction-received-carbon-not-stored-or-displayed ()
  "Reaction fallback received carbons are not stored or displayed as chat text."
  (jabber-test-chatbuffer-with-process-chat-spies
    (let* ((inner `(message ((from . "friend@example.com/phone")
                             (to . "me@example.com/resource")
                             (type . "chat")
                             (id . "reaction-carbon-2"))
                            (body nil "> hello\n👍")
                            (reactions ((xmlns . ,jabber-reactions-xmlns)
                                        (id . "target-1"))
                                       (reaction nil "👍"))
                            (fallback ((xmlns . "urn:xmpp:fallback:0")
                                       (for . ,jabber-reactions-xmlns)))))
           (carbon (jabber-test-chatbuffer--received-carbon inner)))
      (jabber-process-chat 'fake-jc carbon)
      (should-not stored)
      (should-not displayed))))

(ert-deftest jabber-test-chatbuffer-normal-sent-carbon-stores-and-displays ()
  "Normal sent carbons still store and display as before."
  (jabber-test-chatbuffer-with-process-chat-spies
    (let* ((inner '(message ((from . "me@example.com/phone")
                             (to . "friend@example.com")
                             (type . "chat")
                             (id . "normal-carbon-1"))
                            (body nil "hello from phone")))
           (carbon (jabber-test-chatbuffer--sent-carbon inner)))
      (jabber-process-chat 'fake-jc carbon)
      (should (= 1 (length stored)))
      (should (eq inner (car stored)))
      (should (= 1 (length displayed))))))

(ert-deftest jabber-test-chatbuffer-normal-received-carbon-stores-and-displays ()
  "Normal received carbons still store and display as before."
  (jabber-test-chatbuffer-with-process-chat-spies
    (let* ((inner '(message ((from . "friend@example.com/phone")
                             (to . "me@example.com/resource")
                             (type . "chat")
                             (id . "normal-carbon-2"))
                            (body nil "hello from phone")))
           (carbon (jabber-test-chatbuffer--received-carbon inner)))
      (jabber-process-chat 'fake-jc carbon)
      (should (= 1 (length stored)))
      (should (eq inner (car stored)))
      (should (= 1 (length displayed))))))

;;; Group 8: OMEMO anonymous-room warning

(require 'jabber-omemo)

(ert-deftest jabber-test-chatbuffer-omemo-warns-anonymous-room ()
  "Enabling OMEMO in a room with no visible JIDs emits a warning."
  (let ((messages nil)
        (jabber-muc-participants nil))
    (with-temp-buffer
      (setq-local jabber-group "room@conf.example.com")
      (setq-local jabber-buffer-connection nil)
      (cl-letf (((symbol-function 'jabber-chat-encryption--save) #'ignore)
                ((symbol-function 'jabber-chat-encryption--update-header) #'ignore)
                ((symbol-function 'require) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore)
                ((symbol-function 'jabber-omemo--muc-participant-jids)
                 (lambda (&rest _) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (jabber-chat-encryption-set-omemo)
        (should (cl-some (lambda (m) (string-match-p "anonymous" m)) messages))))))

(ert-deftest jabber-test-chatbuffer-omemo-no-warning-when-jids-visible ()
  "No warning when participant JIDs are available."
  (let ((messages nil)
        (jabber-muc-participants nil))
    (with-temp-buffer
      (setq-local jabber-group "room@conf.example.com")
      (setq-local jabber-buffer-connection nil)
      (cl-letf (((symbol-function 'jabber-chat-encryption--save) #'ignore)
                ((symbol-function 'jabber-chat-encryption--update-header) #'ignore)
                ((symbol-function 'require) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore)
                ((symbol-function 'jabber-omemo--muc-participant-jids)
                 (lambda (&rest _) (list "alice@example.com")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (jabber-chat-encryption-set-omemo)
        (should-not (cl-some (lambda (m) (string-match-p "anonymous" m)) messages))))))

;;; Group 8: Buffer lookup registry

(ert-deftest jabber-test-chatbuffer-registry-chat-find ()
  "Register a temp buffer as a chat buffer; registry-get returns it."
  (let ((jabber-chatbuffer--registry (make-hash-table :test #'equal)))
    (with-temp-buffer
      (setq-local jabber-chatting-with "alice@example.com")
      (jabber-chatbuffer--registry-put 'chat "alice@example.com")
      (should (eq (current-buffer)
                  (jabber-chatbuffer--registry-get 'chat "alice@example.com"))))))

(ert-deftest jabber-test-chatbuffer-registry-kill-removes-entry ()
  "Killing the buffer removes its registry entry."
  (let ((jabber-chatbuffer--registry (make-hash-table :test #'equal)))
    (let ((buf (generate-new-buffer " *test-chat-registry*")))
      (with-current-buffer buf
        (setq-local jabber-chatting-with "bob@example.com")
        (jabber-chatbuffer--registry-put 'chat "bob@example.com"))
      (should (eq buf (jabber-chatbuffer--registry-get 'chat "bob@example.com")))
      (kill-buffer buf)
      ;; The kill-buffer-hook removed it; get now returns nil.
      (should-not (jabber-chatbuffer--registry-get 'chat "bob@example.com")))))

(ert-deftest jabber-test-chatbuffer-registry-no-collision ()
  "MUC and chat buffers with the same bare JID do not collide."
  (let ((jabber-chatbuffer--registry (make-hash-table :test #'equal)))
    (let ((chat-buf (generate-new-buffer " *test-chat*"))
          (muc-buf  (generate-new-buffer " *test-muc*")))
      (unwind-protect
          (progn
            (with-current-buffer chat-buf
              (setq-local jabber-chatting-with "room@conf.example.com")
              (jabber-chatbuffer--registry-put 'chat "room@conf.example.com"))
            (with-current-buffer muc-buf
              (setq-local jabber-group "room@conf.example.com")
              (jabber-chatbuffer--registry-put 'muc "room@conf.example.com"))
            (should (eq chat-buf
                        (jabber-chatbuffer--registry-get 'chat "room@conf.example.com")))
            (should (eq muc-buf
                        (jabber-chatbuffer--registry-get 'muc "room@conf.example.com"))))
        (kill-buffer chat-buf)
        (kill-buffer muc-buf)))))

(ert-deftest jabber-test-chatbuffer-registry-muc-private ()
  "MUC-private lookup by group+nick returns correct buffer."
  (let ((jabber-chatbuffer--registry (make-hash-table :test #'equal)))
    (let ((buf (generate-new-buffer " *test-muc-private*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local jabber-chatting-with "room@conf.example.com/alice")
              (jabber-chatbuffer--registry-put
               'muc-private "room@conf.example.com/alice"))
            (should (eq buf
                        (jabber-chatbuffer--registry-get
                         'muc-private "room@conf.example.com/alice"))))
        (kill-buffer buf)))))

(ert-deftest jabber-test-chatbuffer-registry-kill-full-jid-removes-bare-key ()
  "kill-buffer-hook cleans up bare-JID key when chatting-with is a full JID."
  (let ((jabber-chatbuffer--registry (make-hash-table :test #'equal)))
    (let ((buf (generate-new-buffer " *test-chat-full-jid*")))
      (with-current-buffer buf
        ;; Registered with bare JID (as jabber-chat-create-buffer does).
        (setq-local jabber-chatting-with "carol@example.com/phone")
        (jabber-chatbuffer--registry-put 'chat "carol@example.com"))
      ;; At this point the registry holds bare key.
      (should (eq buf (jabber-chatbuffer--registry-get 'chat "carol@example.com")))
      ;; jabber-chatting-with has a resource — kill-hook should still remove
      ;; the bare-JID key because it normalises via jabber-jid-user.
      (kill-buffer buf)
      (should-not (jabber-chatbuffer--registry-get 'chat "carol@example.com")))))

;;; Group 9: OMEMO immediate display status transitions

(defvar jabber-muc-printers)

(defmacro jabber-test-chatbuffer-with-rendering-ewoc (&rest body)
  "Set up a temp buffer with a rendering chat ewoc, then run BODY.
Uses `jabber-chat-pp' so status indicators are actually rendered."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'jabber-chat-pp nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal))
           (jabber-chat-printers '(jabber-chat-print-body))
           (jabber-muc-printers nil)
           (jabber-chat-header-line-format nil)
           (inhibit-read-only t))
       (cl-letf (((symbol-function 'jabber-chat-self-prompt)
                  (lambda (_msg _ts _delayed _/me-p) (insert "me: ")))
                 ((symbol-function 'jabber-chat-print-prompt)
                  (lambda (_msg _ts _delayed _/me-p) (insert "them: ")))
                 ((symbol-function 'jabber-muc-print-prompt)
                  (lambda (_msg _local-p _/me-p) (insert "room: "))))
         ,@body))))

(ert-deftest jabber-test-chatbuffer-sending-status-renders-warning-dot ()
  "A message with :sending status renders a warning-face dot."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "omemo-001" :body "secret"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (should node)
      (goto-char (point-min))
      (should (search-forward "\u00b7" nil t))
      (should (eq 'warning (get-text-property (1- (point)) 'face))))))

(ert-deftest jabber-test-chatbuffer-reply-context-renders-1to1 ()
  "A 1:1 reply renders reply context before the body."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "reply-1" :body "answer"
                      :from "alice@example.com/phone"
                      :reply-to-id "orig-1"
                      :reply-to-jid "alice@example.com/phone"
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (should node)
      (let ((text (buffer-string)))
        (should (string-match-p "reply to phone (orig-1)" text))
        (should (string-match-p "reply to phone (orig-1)\nanswer" text))))))

(ert-deftest jabber-test-chatbuffer-reply-context-renders-muc ()
  "A MUC reply renders reply context before the body."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "reply-2" :body "answer"
                      :from "room@conf.example.com/bob"
                      :reply-to-id "server-orig-1"
                      :reply-to-jid "room@conf.example.com/alice"
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :muc-foreign msg))))
      (should node)
      (let ((text (buffer-string)))
        (should (string-match-p "reply to alice (server-orig-1)" text))
        (should (string-match-p "reply to alice (server-orig-1)\nanswer" text))))))

(ert-deftest jabber-test-chatbuffer-status-sending-to-sent ()
  "Status :sending -> :sent updates the indicator face."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "omemo-002" :body "hello"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (plist-put (cadr (ewoc-data node)) :status :sent)
      (ewoc-invalidate jabber-chat-ewoc node)
      (goto-char (point-min))
      (should (search-forward "\u00b7" nil t))
      (should (eq 'shadow (get-text-property (1- (point)) 'face))))))

(ert-deftest jabber-test-chatbuffer-status-sending-to-undelivered ()
  "Status :sending -> :undelivered shows error-face X."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "omemo-003" :body "fail"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (plist-put (cadr (ewoc-data node)) :status :undelivered)
      (ewoc-invalidate jabber-chat-ewoc node)
      (goto-char (point-min))
      (should (search-forward "\u2717" nil t))
      (should (eq 'error (get-text-property (1- (point)) 'face))))))

(ert-deftest jabber-test-chatbuffer-send-failed-restores-body ()
  "jabber-omemo--send-failed restores body text to buffer input area."
  (require 'jabber-omemo)
  (jabber-test-chatbuffer-with-ewoc
    (let* ((jabber-point-insert (point-marker))
           (msg (list :id "omemo-004" :body "restore me"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (jabber-omemo--send-failed (current-buffer) node "restore me"
                                 "OMEMO: test failure")
      (should (string= "restore me"
                        (buffer-substring jabber-point-insert (point-max))))
      (should (eq :undelivered
                  (plist-get (cadr (ewoc-data node)) :status))))))

;;; Group 10: jabber-chat-mode-setup ewoc idempotency

(ert-deftest jabber-test-chatbuffer-mode-setup-preserves-ewoc-on-repeat ()
  "Calling jabber-chat-mode-setup twice preserves the existing ewoc.
The `make-local-variable' pattern for jabber-chat-ewoc and
jabber-point-insert is critical: on reconnection the function is called
again, and the ewoc created on the first call must survive."
  (with-temp-buffer
    (let ((jabber-chat-ewoc nil)
          (jabber-chat--msg-nodes nil)
          (jabber-point-insert nil)
          (jabber-send-function nil)
          (jabber-chat-encryption nil)
          (jabber-chat-default-encryption 'plaintext)
          (jabber-buffer-connection nil)
          (jabber-chat-encryption-message ""))
      ;; Stub out DB and connection helpers called by jabber-chat-mode-setup
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-get-chat-encryption)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-muc-nick-completion-at-point)
                 #'ignore))
        ;; First call: creates the ewoc
        (jabber-chat-mode-setup 'fake-jc #'ignore)
        (let ((ewoc-1 jabber-chat-ewoc)
              (marker-1 jabber-point-insert))
          (should ewoc-1)
          (should marker-1)
          ;; Insert a message into the ewoc to verify identity later
          (jabber-chat-ewoc-enter
           (list :local (list :id "persist-me" :body "x"
                              :timestamp (current-time))))
          ;; Second call (simulates reconnection): ewoc must survive
          (jabber-chat-mode-setup 'fake-jc-2 #'ignore)
          (should (eq ewoc-1 jabber-chat-ewoc))
          (should (eq marker-1 jabber-point-insert))
          ;; The message inserted before the second call is still there
          (should (gethash "persist-me" jabber-chat--msg-nodes)))))))

(ert-deftest jabber-test-chatbuffer-mode-setup-creates-ewoc-on-first-call ()
  "First call to jabber-chat-mode-setup creates a new ewoc and marker."
  (with-temp-buffer
    (let ((jabber-chat-ewoc nil)
          (jabber-chat--msg-nodes nil)
          (jabber-point-insert nil)
          (jabber-send-function nil)
          (jabber-chat-encryption nil)
          (jabber-chat-default-encryption 'plaintext)
          (jabber-buffer-connection nil)
          (jabber-chat-encryption-message ""))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-get-chat-encryption)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-muc-nick-completion-at-point)
                 #'ignore))
        (jabber-chat-mode-setup 'fake-jc #'ignore)
        (should jabber-chat-ewoc)
        (should (markerp jabber-point-insert))
        (should (hash-table-p jabber-chat--msg-nodes))))))

(ert-deftest jabber-test-chatbuffer-mode-setup-updates-connection ()
  "Second call to jabber-chat-mode-setup updates jabber-buffer-connection."
  (with-temp-buffer
    (let ((jabber-chat-ewoc nil)
          (jabber-chat--msg-nodes nil)
          (jabber-point-insert nil)
          (jabber-send-function nil)
          (jabber-chat-encryption nil)
          (jabber-chat-default-encryption 'plaintext)
          (jabber-buffer-connection nil)
          (jabber-chat-encryption-message ""))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-get-chat-encryption)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-muc-nick-completion-at-point)
                 #'ignore))
        (jabber-chat-mode-setup 'jc-old #'ignore)
        (should (eq 'jc-old jabber-buffer-connection))
        (jabber-chat-mode-setup 'jc-new #'ignore)
        (should (eq 'jc-new jabber-buffer-connection))))))

;;; Group 11: Refresh completion

(ert-deftest jabber-test-chatbuffer-refresh-recenters-after-chunked-insert ()
  "Refresh recenters only from the chunked insertion completion callback."
  (jabber-test-chatbuffer-with-ewoc
    (let ((events nil)
          (callback nil)
          (insert-generation nil)
          (jabber-buffer-connection 'fake-jc)
          (jabber-chatting-with "friend@example.com")
          (jabber-group nil)
          (jabber-chat-buffer-msg-count nil)
          (jabber-backlog-number 10)
          (jabber-chat-earliest-backlog nil)
          (entries (list (list :timestamp (current-time) :body "hello"))))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-backlog)
                 (lambda (&rest _) entries))
                ((symbol-function 'jabber-muc-sender-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-chat--insert-backlog-chunked)
                 (lambda (_buffer _entries cb &optional generation)
                   (setq events (append events '(insert-start))
                         callback cb
                         insert-generation generation)))
                ((symbol-function 'jabber-chat-display-buffer-images)
                 (lambda ()
                   (setq events (append events '(images)))))
                ((symbol-function 'jabber-chat-buffer-recenter-input)
                 (lambda ()
                   (setq events (append events '(recenter))))))
        (jabber-chat-buffer-refresh)
        (should (equal '(insert-start) events))
        (should callback)
        (should (= insert-generation jabber-chat--backlog-generation))
        (funcall callback)
        (should (equal '(insert-start images recenter) events))))))

(ert-deftest jabber-test-chatbuffer-refresh-empty-skips-completion-callbacks ()
  "Empty refresh preserves behavior by skipping insert completion callbacks."
  (jabber-test-chatbuffer-with-ewoc
    (let ((events nil)
          (jabber-buffer-connection 'fake-jc)
          (jabber-chatting-with "friend@example.com")
          (jabber-group nil)
          (jabber-chat-buffer-msg-count nil)
          (jabber-backlog-number 10)
          (jabber-chat-earliest-backlog nil))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-backlog)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-muc-sender-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'jabber-chat--insert-backlog-chunked)
                 (lambda (&rest _)
                   (setq events (append events '(insert-start)))))
                ((symbol-function 'jabber-chat-display-buffer-images)
                 (lambda ()
                   (setq events (append events '(images)))))
                ((symbol-function 'jabber-chat-buffer-recenter-input)
                 (lambda ()
                   (setq events (append events '(recenter))))))
        (jabber-chat-buffer-refresh)
        (should-not events)))))

;;; Group 12: scroll-to-bottom window policy

(ert-deftest jabber-test-chatbuffer-recenter-input-default-uses-one-window ()
  "Default scroll-to-bottom behavior recenters only one visible window."
  (with-temp-buffer
    (let ((jabber-scrolltobottom-all nil)
          (seen-buffers nil)
          (checked nil)
          (recentered nil))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (buffer &optional _all-frames)
                   (push buffer seen-buffers)
                   'win-a))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _)
                   (error "get-buffer-window-list should not be called")))
                ((symbol-function 'jabber-chat-buffer--recenter-input-p)
                 (lambda (window)
                   (push window checked)
                   t))
                ((symbol-function 'window-live-p)
                 (lambda (_window) t))
                ((symbol-function 'jabber-chat-buffer--recenter-input-window)
                 (lambda (window)
                   (push window recentered))))
        (jabber-chat-buffer-recenter-input)
        (should (equal (list (current-buffer)) seen-buffers))
        (should (equal '(win-a) checked))
        (should (equal '(win-a) recentered))))))

(ert-deftest jabber-test-chatbuffer-recenter-input-all-uses-visible-windows ()
  "All-window scroll-to-bottom checks every visible chat buffer window."
  (with-temp-buffer
    (let ((jabber-scrolltobottom-all t)
          (seen-buffers nil)
          (checked nil)
          (recentered nil))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _)
                   (error "get-buffer-window should not be called")))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (buffer &optional _minibuf _all-frames)
                   (push buffer seen-buffers)
                   '(win-a win-b win-c)))
                ((symbol-function 'jabber-chat-buffer--recenter-input-p)
                 (lambda (window)
                   (push window checked)
                   (memq window '(win-a win-c))))
                ((symbol-function 'jabber-chat-buffer--recenter-input-window)
                 (lambda (window)
                   (push window recentered))))
        (jabber-chat-buffer-recenter-input)
        (should (equal (list (current-buffer)) seen-buffers))
        (should (equal '(win-c win-b win-a) checked))
        (should (equal '(win-c win-a) recentered))))))

(ert-deftest jabber-test-chatbuffer-with-scrolltobottom-is-no-op-wrapper ()
  "Scroll-to-bottom compatibility wrapper evaluates BODY only."
  (let ((events nil))
    (cl-letf (((symbol-function 'jabber-chat-buffer-recenter-input)
               (lambda ()
                 (push 'recenter events)))
              ((symbol-function 'jabber-chat-buffer--recenter-input-window)
               (lambda (_window)
                 (push 'recenter-window events))))
      (should (eq 'body-result
                  (jabber-chat-buffer-with-scrolltobottom
                    (push 'body events)
                    'body-result)))
      (should (equal '(body) events)))))

(ert-deftest jabber-test-chatbuffer-recenter-input-window-preserves-point ()
  "Recentering moves temporarily to the input marker and restores point."
  (let ((buffer (generate-new-buffer " *test-chat-recenter*"))
        (previous-buffer (window-buffer (selected-window)))
        (recenter-point nil)
        typed-point)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (insert "history\n")
          (setq-local jabber-point-insert (point-marker))
          (insert "typed input")
          (setq typed-point (point))
          (cl-letf (((symbol-function 'recenter)
                     (lambda (&rest _)
                       (setq recenter-point (point)))))
            (jabber-chat-buffer--recenter-input-window (selected-window)))
          (should (= recenter-point jabber-point-insert))
          (should (= (point) typed-point)))
      (set-window-buffer (selected-window) previous-buffer)
      (kill-buffer buffer))))

;;; Group 13: HTTP Upload callback

(ert-deftest jabber-test-chatbuffer-attach-file-inserts-url-when-buffer-live ()
  "Upload completion inserts the URL into the original live buffer."
  (with-temp-buffer
    (let ((jabber-buffer-connection 'jc)
          (callback nil)
          (messages nil))
      (cl-letf (((symbol-function 'jabber-httpupload--upload)
                 (lambda (_jc _filepath cb)
                   (setq callback cb)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (jabber-chat-attach-file "/tmp/file.txt")
        (funcall callback "https://upload.example.net/file.txt")
        (should (string= (buffer-string)
                         "https://upload.example.net/file.txt"))
        (should (string= jabber-httpupload--pending-url
                         "https://upload.example.net/file.txt"))
        (should (equal messages
                       '("Uploaded: https://upload.example.net/file.txt (send with RET)")))))))

(ert-deftest jabber-test-chatbuffer-attach-file-skips-dead-buffer ()
  "Upload completion skips insertion when the original buffer was killed."
  (let ((buffer (generate-new-buffer " *jabber-upload-dead*"))
        (callback nil))
    (with-current-buffer buffer
      (setq-local jabber-buffer-connection 'jc)
      (cl-letf (((symbol-function 'jabber-httpupload--upload)
                 (lambda (_jc _filepath cb)
                   (setq callback cb))))
        (jabber-chat-attach-file "/tmp/file.txt")))
    (kill-buffer buffer)
    (should-not (buffer-live-p buffer))
    (should-not (funcall callback "https://upload.example.net/file.txt"))))

(provide 'jabber-test-chatbuffer)

;;; jabber-test-chatbuffer.el ends here
