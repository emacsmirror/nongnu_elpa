;;; jabber-chatbuffer-tests.el --- Tests for ewoc hash table API  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ewoc)
(require 'jabber-chatbuffer)
(require 'jabber-chat)
(require 'jabber-db)

;; jabber-chat requires this via jabber-muc
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")
(defvar jabber-muc-participants nil)

;;; Test helpers

(defmacro jabber-chatbuffer-test-with-ewoc (&rest body)
  "Set up a temp buffer with a chat ewoc and hash table, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       ,@body)))

;;; Group 1: jabber-chat-ewoc-enter

(ert-deftest jabber-chat-test-ewoc-enter-registers-id ()
  "Inserting a message with :id registers it in the hash table."
  (jabber-chatbuffer-test-with-ewoc
    (let* ((msg (list :id "msg-001" :body "hello" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (should node)
      (should (eq node (gethash "msg-001" jabber-chat--msg-nodes))))))

(ert-deftest jabber-chat-test-ewoc-enter-skips-nil-id ()
  "Inserting a message without :id does not pollute the hash table."
  (jabber-chatbuffer-test-with-ewoc
    (let* ((msg (list :body "notice text" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :notice msg))))
      (should node)
      (should (zerop (hash-table-count jabber-chat--msg-nodes))))))

(ert-deftest jabber-chat-test-ewoc-enter-notice-string ()
  "Inserting a notice with string body does not error."
  (jabber-chatbuffer-test-with-ewoc
    (let ((node (jabber-chat-ewoc-enter (list :notice "Someone joined"
                                              :time (current-time)))))
      (should node)
      (should (zerop (hash-table-count jabber-chat--msg-nodes))))))

(ert-deftest jabber-chat-test-ewoc-enter-multiple-ids ()
  "Multiple messages with distinct IDs are all registered."
  (jabber-chatbuffer-test-with-ewoc
    (dotimes (i 5)
      (let ((msg (list :id (format "msg-%03d" i) :body "x"
                       :timestamp (current-time))))
        (jabber-chat-ewoc-enter (list :local msg))))
    (should (= 5 (hash-table-count jabber-chat--msg-nodes)))
    (should (gethash "msg-002" jabber-chat--msg-nodes))))

;;; Group 2: jabber-chat-ewoc-find-by-id

(ert-deftest jabber-chat-test-find-by-id-returns-node ()
  "Looking up a registered ID returns the correct ewoc node."
  (jabber-chatbuffer-test-with-ewoc
    (let* ((msg (list :id "find-me" :body "test" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (should (eq node (jabber-chat-ewoc-find-by-id "find-me"))))))

(ert-deftest jabber-chat-test-find-by-id-returns-nil-for-missing ()
  "Looking up a nonexistent ID returns nil."
  (jabber-chatbuffer-test-with-ewoc
    (should-not (jabber-chat-ewoc-find-by-id "no-such-id"))))

(ert-deftest jabber-chat-test-find-by-id-nil-safe ()
  "Looking up nil returns nil without error."
  (jabber-chatbuffer-test-with-ewoc
    (should-not (jabber-chat-ewoc-find-by-id nil))))

;;; Group 3: In-place status update

(ert-deftest jabber-chat-test-status-update-in-place ()
  "Mutating :status on the shared plist is visible through the ewoc node."
  (jabber-chatbuffer-test-with-ewoc
    (let* ((msg (list :id "msg-upd" :body "hi" :status :sent
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      ;; Simulate receipt arrival: mutate plist in place
      (plist-put msg :status :delivered)
      ;; The ewoc node shares the same plist object
      (should (eq :delivered (plist-get (cadr (ewoc-data node)) :status))))))

(ert-deftest jabber-chat-test-status-update-via-lookup ()
  "Status update via find-by-id + plist-put works end-to-end."
  (jabber-chatbuffer-test-with-ewoc
    (let ((msg (list :id "msg-e2e" :body "test" :status :sent
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg))
      ;; Look up and update
      (when-let* ((node (jabber-chat-ewoc-find-by-id "msg-e2e")))
        (plist-put (cadr (ewoc-data node)) :status :displayed))
      ;; Verify the original plist was mutated (shared object)
      (should (eq :displayed (plist-get msg :status))))))

;;; Group 4: Hash table cleanup

(ert-deftest jabber-chat-test-hash-cleanup-on-clear ()
  "Clearing the hash table via clrhash removes all entries."
  (jabber-chatbuffer-test-with-ewoc
    (dotimes (i 3)
      (let ((msg (list :id (format "clr-%d" i) :body "x"
                       :timestamp (current-time))))
        (jabber-chat-ewoc-enter (list :local msg))))
    (should (= 3 (hash-table-count jabber-chat--msg-nodes)))
    ;; Simulate what jabber-mam--reload-buffer does
    (ewoc-filter jabber-chat-ewoc #'ignore)
    (clrhash jabber-chat--msg-nodes)
    (should (zerop (hash-table-count jabber-chat--msg-nodes)))))

(ert-deftest jabber-chat-test-hash-remhash-on-delete ()
  "Removing an entry via remhash drops that ID from the table."
  (jabber-chatbuffer-test-with-ewoc
    (let ((msg (list :id "del-me" :body "x" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :local msg)))
    (should (gethash "del-me" jabber-chat--msg-nodes))
    (remhash "del-me" jabber-chat--msg-nodes)
    (should-not (gethash "del-me" jabber-chat--msg-nodes))))

;;; Group 5: DB backlog includes stanza ID

(ert-deftest jabber-db-test-backlog-includes-stanza-id ()
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

(ert-deftest jabber-db-test-backlog-status-from-receipts ()
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

(ert-deftest jabber-chat-test-build-msg-plist-includes-id ()
  "jabber-chat--build-msg-plist extracts the stanza id attribute."
  (let* ((stanza '(message ((from . "alice@example.com")
                            (id . "emacs-msg-42")
                            (type . "chat"))
                           (body () "Hello")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "emacs-msg-42" (plist-get plist :id)))))

(ert-deftest jabber-chat-test-build-msg-plist-nil-id ()
  "jabber-chat--build-msg-plist returns nil :id when stanza has none."
  (let* ((stanza '(message ((from . "alice@example.com")
                            (type . "chat"))
                           (body () "Hello")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :id))))

;;; Group 7: OMEMO anonymous-room warning

(ert-deftest jabber-chatbuffer-test-omemo-warns-anonymous-room ()
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

(ert-deftest jabber-chatbuffer-test-omemo-no-warning-when-jids-visible ()
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

(ert-deftest jabber-chatbuffer-test-registry-chat-find ()
  "Register a temp buffer as a chat buffer; registry-get returns it."
  (let ((jabber-chatbuffer--registry (make-hash-table :test #'equal)))
    (with-temp-buffer
      (setq-local jabber-chatting-with "alice@example.com")
      (jabber-chatbuffer--registry-put 'chat "alice@example.com")
      (should (eq (current-buffer)
                  (jabber-chatbuffer--registry-get 'chat "alice@example.com"))))))

(ert-deftest jabber-chatbuffer-test-registry-kill-removes-entry ()
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

(ert-deftest jabber-chatbuffer-test-registry-no-collision ()
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

(ert-deftest jabber-chatbuffer-test-registry-muc-private ()
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

(ert-deftest jabber-chatbuffer-test-registry-kill-full-jid-removes-bare-key ()
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

(defmacro jabber-chatbuffer-test-with-rendering-ewoc (&rest body)
  "Set up a temp buffer with a rendering chat ewoc, then run BODY.
Uses `jabber-chat-pp' so status indicators are actually rendered."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'jabber-chat-pp nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal))
           (jabber-chat-printers '(jabber-chat-print-body))
           (jabber-chat-header-line-format nil)
           (inhibit-read-only t))
       (cl-letf (((symbol-function 'jabber-chat-self-prompt)
                  (lambda (_msg _ts _delayed _/me-p) (insert "me: "))))
         ,@body))))

(ert-deftest jabber-chat-test-sending-status-renders-warning-dot ()
  "A message with :sending status renders a warning-face dot."
  (jabber-chatbuffer-test-with-rendering-ewoc
    (let* ((msg (list :id "omemo-001" :body "secret"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (should node)
      (goto-char (point-min))
      (should (search-forward "\u00b7" nil t))
      (should (eq 'warning (get-text-property (1- (point)) 'face))))))

(ert-deftest jabber-chat-test-status-sending-to-sent ()
  "Status :sending -> :sent updates the indicator face."
  (jabber-chatbuffer-test-with-rendering-ewoc
    (let* ((msg (list :id "omemo-002" :body "hello"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (plist-put (cadr (ewoc-data node)) :status :sent)
      (ewoc-invalidate jabber-chat-ewoc node)
      (goto-char (point-min))
      (should (search-forward "\u00b7" nil t))
      (should (eq 'shadow (get-text-property (1- (point)) 'face))))))

(ert-deftest jabber-chat-test-status-sending-to-undelivered ()
  "Status :sending -> :undelivered shows error-face X."
  (jabber-chatbuffer-test-with-rendering-ewoc
    (let* ((msg (list :id "omemo-003" :body "fail"
                      :status :sending :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :local msg))))
      (plist-put (cadr (ewoc-data node)) :status :undelivered)
      (ewoc-invalidate jabber-chat-ewoc node)
      (goto-char (point-min))
      (should (search-forward "\u2717" nil t))
      (should (eq 'error (get-text-property (1- (point)) 'face))))))

(ert-deftest jabber-chat-test-send-failed-restores-body ()
  "jabber-omemo--send-failed restores body text to buffer input area."
  (require 'jabber-omemo)
  (jabber-chatbuffer-test-with-ewoc
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

(provide 'jabber-chatbuffer-tests)

;;; jabber-chatbuffer-tests.el ends here
