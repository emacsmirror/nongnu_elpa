;;; jabber-test-chatbuffer.el --- Tests for jabber-chatbuffer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared chat buffer infrastructure (ewoc, message nodes).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewoc)
(require 'jabber-chatbuffer)
(require 'jabber-chat)
(require 'jabber-chat-commands)
(require 'jabber-db)
(require 'jabber-httpupload)
(require 'jabber-subscription)

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
                 ;; Freshly consed: `jabber-chat-buffer--shift-undo-list'
                 ;; shifts this entry in place, so a quoted literal would
                 ;; be mutated and corrupt a second run in the same process.
                 (copy-tree '(nil face bold 10 . 12))
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
  "Register a temp buffer as a chat buffer and find it."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
    (with-temp-buffer
      (jabber-buffer-registry-register 'chat "alice@example.com")
      (should (eq (current-buffer)
                  (jabber-buffer-registry-find 'chat "alice@example.com"))))))

(ert-deftest jabber-test-chatbuffer-registry-kill-removes-entry ()
  "Killing the buffer removes its registry entry."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
    (let ((buf (generate-new-buffer " *test-chat-registry*")))
      (with-current-buffer buf
        (jabber-buffer-registry-register 'chat "bob@example.com"))
      (should (eq buf (jabber-buffer-registry-find 'chat "bob@example.com")))
      (kill-buffer buf)
      (should-not (jabber-buffer-registry-find 'chat "bob@example.com")))))

(ert-deftest jabber-test-chatbuffer-registry-no-collision ()
  "MUC and chat buffers with the same bare JID do not collide."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
    (let ((chat-buf (generate-new-buffer " *test-chat*"))
          (muc-buf  (generate-new-buffer " *test-muc*")))
      (unwind-protect
          (progn
            (with-current-buffer chat-buf
              (jabber-buffer-registry-register 'chat "room@conf.example.com"))
            (with-current-buffer muc-buf
              (jabber-buffer-registry-register 'muc "room@conf.example.com"))
            (should (eq chat-buf
                        (jabber-buffer-registry-find
                         'chat "room@conf.example.com")))
            (should (eq muc-buf
                        (jabber-buffer-registry-find
                         'muc "room@conf.example.com"))))
        (kill-buffer chat-buf)
        (kill-buffer muc-buf)))))

(ert-deftest jabber-test-chatbuffer-registry-muc-private ()
  "MUC-private lookup by group+nick returns correct buffer."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
    (let ((buf (generate-new-buffer " *test-muc-private*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (jabber-buffer-registry-register
               'muc-private "room@conf.example.com/alice"))
            (should (eq buf
                        (jabber-buffer-registry-find
                         'muc-private "room@conf.example.com/alice"))))
        (kill-buffer buf)))))

(ert-deftest jabber-test-chatbuffer-registry-replacement-survives-old-kill ()
  "Killing a replaced buffer does not remove the current registration."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal))
        (old (generate-new-buffer " *test-chat-old*"))
        (new (generate-new-buffer " *test-chat-new*")))
    (unwind-protect
        (progn
          (jabber-buffer-registry-register 'chat "carol@example.com" old)
          (jabber-buffer-registry-register 'chat "carol@example.com" new)
          (kill-buffer old)
          (should (eq new
                      (jabber-buffer-registry-find
                       'chat "carol@example.com"))))
      (when (buffer-live-p old)
        (kill-buffer old))
      (when (buffer-live-p new)
        (kill-buffer new)))))

(ert-deftest jabber-test-subscription-removes-stale-prompts-from-chat ()
  "Ordinary presence removes old subscription prompts for its sender."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
    (jabber-test-chatbuffer-with-ewoc
      (jabber-buffer-registry-register 'chat "alice@example.com")
      (jabber-chat-ewoc-enter '(:subscription-request "hello"))
      (jabber-chat-ewoc-enter '(:notice "keep me"))
      (jabber-subscription--remove-stale nil "alice@example.com/phone")
      (should (equal (ewoc-collect jabber-chat-ewoc #'identity)
                     '((:notice "keep me")))))))

(ert-deftest jabber-test-subscription-request-enters-chat-buffer ()
  "A subscription request is rendered in the sender's chat buffer."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          entered)
      (cl-letf (((symbol-function 'jabber-chat-create-buffer)
                 (lambda (_jc _from) buffer))
                ((symbol-function 'jabber-chat-ewoc-enter)
                 (lambda (data) (setq entered data))))
        (let ((jabber-presence-hooks nil)
              (jabber-alert-presence-hooks nil))
          (jabber-process-subscription-request
           'fake-jc "alice@example.com" "please")))
      (should (equal (plist-get entered :subscription-request) "please"))
      (should (plist-get entered :time)))))

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

(ert-deftest jabber-test-chatbuffer-reply-inline-quote-renders-1to1 ()
  "A 1:1 reply renders the fallback quote inline and shows no id label."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "reply-1"
                      :body "> phone:\n> original\nanswer"
                      :from "alice@example.com/phone"
                      :reply-to-id "orig-1"
                      :reply-to-jid "alice@example.com/phone"
                      :fallback-range '(0 20)
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (should node)
      (let ((text (buffer-string)))
        (should (string-match-p "> phone:\n> original\nanswer" text))
        (should-not (string-match-p "reply to " text))))))

(ert-deftest jabber-test-chatbuffer-reply-inline-quote-renders-muc ()
  "A MUC reply renders the fallback quote inline and shows no id label."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "reply-2"
                      :body "> alice:\n> original\nanswer"
                      :from "room@conf.example.com/bob"
                      :reply-to-id "server-orig-1"
                      :reply-to-jid "room@conf.example.com/alice"
                      :fallback-range '(0 20)
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :muc-foreign msg))))
      (should node)
      (let ((text (buffer-string)))
        (should (string-match-p "> alice:\n> original\nanswer" text))
        (should-not (string-match-p "reply to " text))))))

(ert-deftest jabber-test-chatbuffer-reply-without-fallback-shows-label ()
  "A reply carrying no fallback quote renders a compact context label."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "reply-3"
                      :body "answer"
                      :from "alice@example.com/phone"
                      :reply-to-id "orig-1"
                      :reply-to-jid "alice@example.com/phone"
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (should node)
      (should (string-match-p "reply to alice@example.com" (buffer-string))))))

(ert-deftest jabber-test-chatbuffer-reply-without-fallback-muc-label-uses-nick ()
  "The MUC context label shows the occupant nick, not the room."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (cl-letf (((symbol-function 'jabber-muc-sender-p) (lambda (_jid) t)))
      (let* ((msg (list :id "reply-4"
                        :body "answer"
                        :from "room@conf.example.com/bob"
                        :reply-to-id "server-orig-1"
                        :reply-to-jid "room@conf.example.com/alice"
                        :timestamp (current-time)))
             (node (jabber-chat-ewoc-enter (list :muc-foreign msg))))
        (should node)
        (should (string-match-p "reply to alice\n" (buffer-string)))))))

(ert-deftest jabber-test-chatbuffer-reply-without-jid-shows-bare-label ()
  "A reply with no author JID still gets a bare context label."
  (jabber-test-chatbuffer-with-rendering-ewoc
    (let* ((msg (list :id "reply-5"
                      :body "answer"
                      :from "alice@example.com/phone"
                      :reply-to-id "orig-1"
                      :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :foreign msg))))
      (should node)
      (let ((text (buffer-string)))
        (should (string-match-p "reply\n" text))
        (should-not (string-match-p "reply to " text))))))

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
                ((symbol-function 'jabber-chat-buffer--restore-view)
                 (lambda (_anchors)
                   (setq events (append events '(restore))))))
        (jabber-chat-buffer-refresh)
        (should (equal '(insert-start) events))
        (should callback)
        (should (= insert-generation jabber-chat--backlog-generation))
        (funcall callback)
        (should (equal '(insert-start images restore) events))))))

(ert-deftest jabber-test-chatbuffer-disabled-threads-refresh-as-plain-chat ()
  "Include threaded messages in the original parent refresh path."
  (jabber-test-chatbuffer-with-ewoc
    (let ((jabber-message-thread-use-buffers nil)
          (jabber-buffer-connection 'fake-jc)
          (jabber-chatting-with "friend@example.com")
          (jabber-group nil)
          (jabber-chat-buffer-msg-count nil)
          backlog-args)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-backlog)
                 (lambda (&rest args)
                   (setq backlog-args args)
                   nil))
                ((symbol-function 'jabber-muc-sender-p)
                 (lambda (&rest _) nil)))
        (jabber-chat-buffer-refresh)
        (should (eq t (nth 6 backlog-args)))))))

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

;;; Group 11b: view preservation across refresh

(ert-deftest jabber-test-chatbuffer-node-stanza-id-prefers-id ()
  "A message node's anchor id is :id when present."
  (jabber-test-chatbuffer-with-ewoc
    (let ((node (jabber-chat-ewoc-enter
                 (list :foreign (list :id "a" :server-id "b" :body "x")))))
      (should (equal "a" (jabber-chat-buffer--node-stanza-id node))))))

(ert-deftest jabber-test-chatbuffer-node-stanza-id-falls-back-to-server-id ()
  "A message node with only :server-id anchors on the server id."
  (jabber-test-chatbuffer-with-ewoc
    (let ((node (jabber-chat-ewoc-enter
                 (list :foreign (list :server-id "only-server" :body "x")))))
      (should (equal "only-server"
                     (jabber-chat-buffer--node-stanza-id node))))))

(ert-deftest jabber-test-chatbuffer-node-stanza-id-nil-for-notice ()
  "Notice nodes carry no anchor id."
  (jabber-test-chatbuffer-with-ewoc
    (let ((node (jabber-chat-ewoc-enter (list :notice "joined"))))
      (should-not (jabber-chat-buffer--node-stanza-id node)))))

(ert-deftest jabber-test-chatbuffer-anchor-id-survives-refresh-rebuild ()
  "A captured anchor id still resolves after a full clear and rebuild.
This is the invariant the refresh view-restore depends on: a reader
scrolled up to a server-id-only message must be findable again once the
ewoc is rebuilt from the database."
  (jabber-test-chatbuffer-with-ewoc
    (let ((entries (list (list :foreign (list :id "m1" :body "one"))
                         (list :foreign (list :server-id "s2" :body "two"))
                         (list :local (list :id "m3" :body "three")))))
      (dolist (e entries) (jabber-chat-ewoc-enter e))
      (let ((anchor-id (jabber-chat-buffer--node-stanza-id
                        (jabber-chat-ewoc-find-by-id "s2"))))
        (should (equal "s2" anchor-id))
        ;; Mimic jabber-chat-buffer-refresh: drop every node, then rebuild.
        (let ((n (ewoc-nth jabber-chat-ewoc 0)))
          (while n
            (let ((next (ewoc-next jabber-chat-ewoc n)))
              (ewoc-delete jabber-chat-ewoc n)
              (setq n next))))
        (clrhash jabber-chat--msg-nodes)
        (dolist (e entries) (jabber-chat-ewoc-enter e))
        (let ((restored (jabber-chat-ewoc-find-by-id anchor-id)))
          (should restored)
          (should (equal "two"
                         (plist-get (cadr (ewoc-data restored)) :body))))))))

(ert-deftest jabber-test-chatbuffer-restore-view-dispatch ()
  "A following window is recentered without moving point; a history
window is scrolled back to its anchored message; a vanished anchor is
forced to the bottom (the only path that overwrites point)."
  (jabber-test-chatbuffer-with-ewoc
    (let ((recentered nil)
          (forced nil)
          (started nil))
      (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                ((symbol-function 'jabber-chat-buffer--recenter-input-window)
                 (lambda (w) (push w recentered)))
                ((symbol-function 'jabber-chat-buffer--restore-bottom)
                 (lambda (w) (push w forced)))
                ((symbol-function 'jabber-chat-ewoc-find-by-id)
                 (lambda (id) (when (equal id "present") 'fake-node)))
                ((symbol-function 'ewoc-location) (lambda (_n) 42))
                ((symbol-function 'set-window-start)
                 (lambda (w pos) (push (list w pos) started)))
                ((symbol-function 'set-window-point) (lambda (_w _pos) nil)))
        (jabber-chat-buffer--restore-view
         '((win-bottom . bottom)
           (win-present msg . "present")
           (win-missing msg . "gone")))
        ;; Following window recentered, point preserved (not forced).
        (should (equal '(win-bottom) recentered))
        ;; Only the vanished-anchor window is forced to the bottom.
        (should (equal '(win-missing) forced))
        ;; The live-anchor window is scrolled to its message.
        (should (equal '((win-present 42)) started))))))

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

(ert-deftest jabber-test-chatbuffer-with-scrolltobottom-is-callable ()
  "Scroll-to-bottom wrapper also works as an ordinary function."
  (should (eq 'body-result
              (funcall #'jabber-chat-buffer-with-scrolltobottom
                       'ignored
                       'body-result))))

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

;;; Group: help-at-point display

(ert-deftest jabber-test-chatbuffer-help-at-point-enabled ()
  "Enabling the option scopes help-at-pt to the chat buffer."
  (let ((jabber-chat-display-help-at-point t))
    (unwind-protect
        (with-temp-buffer
          (jabber-chat-mode)
          (should (local-variable-p 'help-at-pt-display-when-idle))
          (should (equal help-at-pt-display-when-idle '(help-echo))))
      (help-at-pt-cancel-timer))))

(ert-deftest jabber-test-chatbuffer-help-at-point-disabled ()
  "Disabling the option leaves help-at-pt untouched in the buffer."
  (let ((jabber-chat-display-help-at-point nil))
    (with-temp-buffer
      (jabber-chat-mode)
      (should-not (local-variable-p 'help-at-pt-display-when-idle)))))

;;; Group 14: shared input sending

(ert-deftest jabber-test-input-send-extracts-body ()
  "Send and remove input below the prompt marker."
  (with-temp-buffer
    (insert "Prompt: hello")
    (let ((jabber-connections '(connection))
          (sent nil))
      (setq-local jabber-buffer-connection 'connection)
      (setq-local jabber-point-insert (copy-marker 9))
      (setq-local jabber-send-function
                  (lambda (jc body) (setq sent (cons jc body))))
      (jabber-chat-buffer-send)
      (should (equal sent '(connection . "hello")))
      (should (string= (buffer-string) "Prompt: ")))))

(ert-deftest jabber-test-input-send-passes-extra-elements ()
  "Pass optional stanza elements through the shared input sender."
  (with-temp-buffer
    (insert "Prompt: hello")
    (let ((jabber-connections '(connection))
          sent)
      (setq-local jabber-buffer-connection 'connection)
      (setq-local jabber-point-insert (copy-marker 9))
      (setq-local jabber-send-function
                  (lambda (jc body &optional extra-elements)
                    (setq sent (list jc body extra-elements))))
      (jabber-chat-buffer-send '((thread () "thread-1")))
      (should
       (equal sent
              '(connection "hello" ((thread () "thread-1")))))
      (should (string= (buffer-string) "Prompt: ")))))

(ert-deftest jabber-test-input-send-reuses-active-connection ()
  "Replace a stale connection before sending input."
  (with-temp-buffer
    (insert "hello")
    (let ((jabber-connections '(new))
          (sent nil))
      (setq-local jabber-buffer-connection 'old)
      (setq-local jabber-point-insert (copy-marker (point-min)))
      (setq-local jabber-send-function
                  (lambda (jc body) (setq sent (cons jc body))))
      (cl-letf (((symbol-function 'jabber-find-active-connection)
                 (lambda (_jc) 'new))
                ((symbol-function 'jabber-read-account)
                 (lambda (&rest _) (ert-fail "Prompted for an account"))))
        (jabber-chat-buffer-send))
      (should (equal sent '(new . "hello")))
      (should (eq jabber-buffer-connection 'new)))))

;;; Group 15: MUC message identity

(ert-deftest jabber-test-chatbuffer-muc-client-id-is-sender-scoped ()
  "Two MUC occupants using one client ID both remain addressable."
  (with-temp-buffer
    (let ((jabber-group "room@conference.example.com")
          (jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
          (jabber-chat--msg-nodes (make-hash-table :test #'equal)))
      (let ((alice (jabber-chat-ewoc-enter
                    (list :muc-foreign
                          (list :id "same-id"
                                :server-id "server-a"
                                :from "room@conference.example.com/alice"
                                :body "alice"))))
            (bob (jabber-chat-ewoc-enter
                  (list :muc-foreign
                        (list :id "same-id"
                              :server-id "server-b"
                              :from "room@conference.example.com/bob"
                              :body "bob")))))
        (should alice)
        (should bob)
        (should (eq alice
                    (jabber-chat-ewoc-find-by-id-and-sender
                     "same-id" "room@conference.example.com/alice")))
        (should (eq bob
                    (jabber-chat-ewoc-find-by-id-and-sender
                     "same-id" "room@conference.example.com/bob")))
        (should (eq alice (jabber-chat-ewoc-find-by-id "server-a")))
        (should (eq bob (jabber-chat-ewoc-find-by-id "server-b")))))))

(ert-deftest jabber-test-chatbuffer-unregister-removes-composite-keys ()
  "Removing a MUC node drops its client and server index keys."
  (with-temp-buffer
    (let ((jabber-group "room@conference.example.com")
          (jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
          (jabber-chat--msg-nodes (make-hash-table :test #'equal)))
      (let ((node (jabber-chat-ewoc-enter
                   (list :muc-foreign
                         (list :id "client-id"
                               :server-id "server-id"
                               :from "room@conference.example.com/alice"
                               :body "hello")))))
        (jabber-chat-ewoc-unregister-node node)
        (should-not (gethash "server-id" jabber-chat--msg-nodes))
        (should-not
         (gethash '(:muc "room@conference.example.com/alice" "client-id")
                  jabber-chat--msg-nodes))))))

(ert-deftest jabber-test-chatbuffer-mam-refresh-is-account-scoped ()
  "A MAM completion refreshes only the account that produced it."
  (let ((first (generate-new-buffer " *jabber-mam-first*"))
        (second (generate-new-buffer " *jabber-mam-second*"))
        refreshed)
    (unwind-protect
        (progn
          (dolist (entry `((,first account-a) (,second account-b)))
            (with-current-buffer (car entry)
              (setq-local major-mode 'jabber-chat-mode)
              (setq-local jabber-buffer-connection (cadr entry))
              (setq-local jabber-chatting-with "friend@example.com")))
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (jc)
                       (if (eq jc 'account-a) "me-a@example.com"
                         "me-b@example.com")))
                    ((symbol-function 'jabber-chat-buffer-refresh)
                     (lambda () (push (current-buffer) refreshed))))
            (jabber-chat--handle-mam-sync-complete
             '(("me-a@example.com" "friend@example.com" "chat"))))
          (should (equal (list first) refreshed)))
      (kill-buffer first)
      (kill-buffer second))))

(provide 'jabber-test-chatbuffer)

;;; jabber-test-chatbuffer.el ends here
