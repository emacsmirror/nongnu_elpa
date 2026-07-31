;;; jabber-test-message-thread.el --- Tests for XEP-0201 threads  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0201 message thread parsing, persistence, routing, and buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-chat)
(require 'jabber-message-correct)
(require 'jabber-message-thread)
(require 'jabber-moderation)
(require 'jabber-muc)
(require 'jabber-reactions)

(defvar jabber-message-thread-buffer-created-functions)

;;; Protocol data

(ert-deftest jabber-test-message-thread-fields ()
  "Parse an opaque thread ID and its optional parent."
  (should
   (equal '(:thread-id "child-1" :thread-parent-id "parent-1")
          (jabber-message-thread--fields
           '(message ((type . "chat"))
                     (body () "hello")
                     (thread ((parent . "parent-1")) "child-1"))))))

(ert-deftest jabber-test-message-thread-fields-without-parent ()
  "Parse a thread that has no parent."
  (should
   (equal '(:thread-id "thread-1" :thread-parent-id nil)
          (jabber-message-thread--fields
           '(message ((type . "groupchat"))
                     (thread () "thread-1"))))))

(ert-deftest jabber-test-message-thread-fields-rejects-invalid-shapes ()
  "Ignore empty and ambiguous thread elements."
  (should-not
   (jabber-message-thread--fields
    '(message ((type . "chat")) (thread () ""))))
  (should-not
   (jabber-message-thread--fields
    '(message ((type . "chat"))
              (thread () "one")
              (thread () "two"))))
  (should-not
   (jabber-message-thread--fields
    '(message ((type . "chat"))
              (thread ((xmlns . "urn:example:foreign")) "thread-1"))))
  (should-not
   (jabber-db--extract-thread-fields
    '(message ((type . "chat"))
              (thread ((xmlns . "urn:example:foreign")) "thread-1")))))

(ert-deftest jabber-test-message-thread-fields-ignore-foreign-elements ()
  "A foreign thread element does not make one core thread ambiguous."
  (let ((stanza
         '(message ((type . "chat"))
                   (thread ((xmlns . "urn:example:foreign")) "foreign")
                   (thread ((xmlns . "jabber:client")) "thread-1"))))
    (dolist (parser '(jabber-message-thread--fields
                      jabber-db--extract-thread-fields))
      (should
       (equal '(:thread-id "thread-1" :thread-parent-id nil)
              (funcall parser stanza))))))

(ert-deftest jabber-test-message-thread-fields-reject-mixed-content ()
  "Thread content must contain character data only."
  (dolist (stanza
           '((message () (thread () (foreign () "x") "thread-1"))
             (message () (thread () "thread" (foreign () "x") "-1"))
             (message () (thread () "thread-1" (foreign () "x")))))
    (dolist (parser '(jabber-message-thread--fields
                      jabber-db--extract-thread-fields))
      (should-not (funcall parser stanza)))))

(ert-deftest jabber-test-message-thread-fields-join-character-data ()
  "All character data in a thread element forms the opaque ID."
  (let ((stanza '(message () (thread () "thread" "-" "1"))))
    (dolist (parser '(jabber-message-thread--fields
                      jabber-db--extract-thread-fields))
      (should
       (equal '(:thread-id "thread-1" :thread-parent-id nil)
              (funcall parser stanza))))))

(ert-deftest jabber-test-message-thread-element ()
  "Build one core thread element with an optional distinct parent."
  (should
   (equal '((thread ((parent . "parent-1")) "child-1"))
          (jabber-message-thread--elements "child-1" "parent-1")))
  (should
   (equal '((thread () "thread-1"))
          (jabber-message-thread--elements "thread-1" nil)))
  (should-not
   (jabber-message-thread--elements "same" "same"))
  (should-not
   (jabber-message-thread--elements "thread-1" "")))

(ert-deftest jabber-test-message-thread-generate-id ()
  "Generate non-empty opaque IDs that do not repeat."
  (let ((first (jabber-message-thread--generate-id))
        (second (jabber-message-thread--generate-id)))
    (should (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" first))
    (should-not (equal first second))))

(ert-deftest jabber-test-message-thread-send-hook ()
  "A thread buffer adds its thread element to outgoing messages."
  (with-temp-buffer
    (setq-local jabber-message-thread-id "thread-1")
    (setq-local jabber-message-thread-parent-id "parent-1")
    (should
     (equal '((thread ((parent . "parent-1")) "thread-1"))
            (jabber-message-thread--send-hook "hello" "msg-1")))))

(ert-deftest jabber-test-message-thread-create-runs-buffer-functions ()
  "Creating a thread runs setup functions in it with its parent."
  (let ((parent (generate-new-buffer " *jabber-thread-parent*"))
        thread
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-message-thread-find-buffer)
                   (lambda (&rest _) nil))
                  ((symbol-function 'jabber-chat-mode-setup) #'ignore)
                  ((symbol-function 'jabber-buffer-registry-register) #'ignore)
                  ((symbol-function 'jabber-db-thread-backlog) #'ignore))
          (let ((jabber-message-thread-buffer-created-functions
                 (list (lambda (parent-buffer)
                         (setq called
                               (list (current-buffer) parent-buffer))))))
            (setq thread
                  (jabber-message-thread-create-buffer
                   'fake-jc "alice@example.com" "chat"
                   "thread-1" nil parent)))
          (should (equal called (list thread parent))))
      (kill-buffer parent)
      (when (buffer-live-p thread)
        (kill-buffer thread)))))

(ert-deftest jabber-test-message-thread-first-send-links-root ()
  "The first local reply links to the root and later replies do not."
  (with-temp-buffer
    (setq-local jabber-message-thread-id "thread-1")
    (setq-local jabber-message-thread--root-reply-id "root-1")
    (setq-local jabber-message-thread--root-reply-jid "alice@example.com")
    (should
     (equal
      '((thread () "thread-1")
        (reply ((xmlns . "urn:xmpp:reply:0")
                (to . "alice@example.com")
                (id . "root-1"))))
      (jabber-message-thread--send-hook "hello" "msg-1")))
    (should
     (equal '((thread () "thread-1"))
            (jabber-message-thread--send-hook "again" "msg-2")))))

(ert-deftest jabber-test-message-thread-correction-keeps-thread-context ()
  "Corrections keep the thread element without consuming the root link."
  (with-temp-buffer
    (setq-local jabber-message-thread-id "thread-1")
    (setq-local jabber-message-thread--root-reply-id "root-1")
    (let ((jabber-chat--sending-correction t))
      (should
       (equal '((thread () "thread-1"))
              (jabber-message-thread--send-hook "corrected" "msg-2"))))
    (should (equal "root-1" jabber-message-thread--root-reply-id))))

(ert-deftest jabber-test-message-thread-captured-reply-is-single-owner ()
  "An explicit reply overrides and consumes the initial thread root link."
  (with-temp-buffer
    (setq-local jabber-message-thread-id "thread-1")
    (setq-local jabber-message-thread--root-reply-id "root-link")
    (setq-local jabber-message-thread--root-reply-jid "alice@example.com")
    (setq-local jabber-message-reply--id "explicit-target")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--thread '(:thread-id "thread-1"))
    (let* ((context (jabber-chat--capture-send-context "answer" nil))
           (extra (plist-get context :extra-elements))
           (stanza `(message () ,@extra))
           (replies (jabber-xml-get-children stanza 'reply)))
      (should (= 1 (length replies)))
      (should (equal "explicit-target"
                     (jabber-xml-get-attribute (car replies) 'id)))
      (should (= 1 (length (jabber-xml-get-children stanza 'thread))))
      (should-not jabber-message-reply--id)
      (should-not jabber-message-thread--root-reply-id)
      (jabber-chat--restore-send-context context)
      (should (equal "explicit-target" jabber-message-reply--id))
      (should (equal "root-link"
                     jabber-message-thread--root-reply-id)))))

(ert-deftest jabber-test-message-thread-correction-capture-is-inert ()
  "Capturing a correction does not consume the next composed reply."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "next-reply")
    (let* ((replace '(replace ((xmlns . "urn:xmpp:message-correct:0")
                               (id . "old"))))
           (context (jabber-chat--capture-send-context "fixed" (list replace))))
      (should (equal (list replace) (plist-get context :extra-elements)))
      (should-not (plist-get context :state))
      (should (equal "next-reply" jabber-message-reply--id)))))

(ert-deftest jabber-test-message-thread-message-at-input-is-nil ()
  "Do not treat the last rendered message as the input-area target."
  (with-temp-buffer
    (let ((jabber-chat-ewoc (ewoc-create #'ignore)))
      (ewoc-enter-last
       jabber-chat-ewoc
       '(:foreign (:id "message-1" :body "hello")))
      (insert "\nDraft")
      (setq-local jabber-point-insert (copy-marker (- (point-max) 5)))
      (goto-char (point-max))
      (should-not (jabber-message-thread--message-at-point)))))

(ert-deftest jabber-test-message-thread-start-sends-chat-draft ()
  "Send a chat draft with a fresh thread and open its sibling buffer."
  (let ((thread-buffer (generate-new-buffer " *jabber-thread-start*")))
    (unwind-protect
        (with-temp-buffer
          (insert "Prompt: root message")
          (let ((jabber-connections '(connection))
                sent created popped)
            (setq-local jabber-buffer-connection 'connection)
            (setq-local jabber-chatting-with "alice@example.com/resource")
            (setq-local jabber-point-insert (copy-marker 9))
            (setq-local jabber-send-function
                        (lambda (jc body &optional extra-elements)
                          (setq sent (list jc body extra-elements))))
            (cl-letf
                (((symbol-function 'jabber-db-ensure-open)
                  (lambda () t))
                 ((symbol-function 'jabber-message-thread--generate-id)
                  (lambda () "thread-1"))
                 ((symbol-function 'jabber-message-thread-create-buffer)
                  (lambda (&rest args)
                    (setq created args)
                    thread-buffer))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (buffer &rest _)
                    (setq popped buffer))))
              (let ((parent (current-buffer)))
                (jabber-message-thread-start)
                (should
                 (equal sent
                        '(connection "root message"
                                     ((thread () "thread-1")))))
                (should
                 (equal created
                        (list 'connection "alice@example.com" "chat"
                              "thread-1" nil parent nil)))
                (should (eq popped thread-buffer))
                (should (string= (buffer-string) "Prompt: "))))))
      (kill-buffer thread-buffer))))

(ert-deftest jabber-test-message-thread-renew-id-rekeys-buffer ()
  "Renewing a thread ID keeps lineage and replaces its registry key."
  (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
    (with-temp-buffer
      (setq-local jabber-message-thread-id "thread-1")
      (setq-local jabber-message-thread-type "chat")
      (setq-local jabber-message-thread-peer "alice@example.org")
      (setq-local jabber-buffer-connection 'fake-jc)
      (jabber-buffer-registry-register
       'thread
       '("me@example.org" "alice@example.org" "chat" "thread-1"))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.org"))
                ((symbol-function 'jabber-message-thread--generate-id)
                 (lambda () "thread-2")))
        (should (equal (jabber-message-thread--renew-id) "thread-2")))
      (should-not
       (jabber-message-thread-find-buffer
        "me@example.org" "alice@example.org" "chat" "thread-1"))
      (should
       (eq (jabber-message-thread-find-buffer
            "me@example.org" "alice@example.org" "chat" "thread-2")
           (current-buffer)))
      (should (equal jabber-message-thread-id "thread-2"))
      (should (equal jabber-message-thread-parent-id "thread-1")))))

(ert-deftest jabber-test-message-thread-start-sends-muc-draft ()
  "Send a MUC draft as a public thread root."
  (let ((thread-buffer (generate-new-buffer " *jabber-muc-thread-start*")))
    (unwind-protect
        (with-temp-buffer
          (insert "root message")
          (let ((jabber-connections '(connection))
                sent created)
            (setq-local jabber-buffer-connection 'connection)
            (setq-local jabber-group "room@example.com")
            (setq-local jabber-point-insert (copy-marker (point-min)))
            (setq-local jabber-send-function
                        (lambda (_jc body &optional extra-elements)
                          (setq sent (list body extra-elements))))
            (cl-letf
                (((symbol-function 'jabber-db-ensure-open)
                  (lambda () t))
                 ((symbol-function 'jabber-message-thread--generate-id)
                  (lambda () "thread-1"))
                 ((symbol-function 'jabber-message-thread-create-buffer)
                  (lambda (&rest args)
                    (setq created args)
                    thread-buffer))
                 ((symbol-function 'pop-to-buffer) #'ignore))
              (let ((parent (current-buffer)))
                (jabber-message-thread-start)
                (should
                 (equal sent
                        '("root message" ((thread () "thread-1")))))
                (should
                 (equal created
                        (list 'connection "room@example.com" "groupchat"
                              "thread-1" nil parent nil)))))))
      (kill-buffer thread-buffer))))

(ert-deftest jabber-test-message-thread-start-requires-a-draft ()
  "Reject starting a thread without input text."
  (with-temp-buffer
    (let ((jabber-connections '(connection)))
      (setq-local jabber-buffer-connection 'connection)
      (setq-local jabber-chatting-with "alice@example.com")
      (setq-local jabber-point-insert (copy-marker (point-max)))
      (setq-local jabber-send-function
                  (lambda (&rest _) (ert-fail "Sent an empty draft")))
      (should-error (jabber-message-thread-start) :type 'user-error))))

(ert-deftest jabber-test-message-thread-refreshes-missing-root ()
  "Reload an open thread buffer when its stored root is not rendered."
  (let ((thread-buffer (generate-new-buffer " *jabber-thread-refresh*"))
        refreshed)
    (unwind-protect
        (with-current-buffer thread-buffer
          (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
          (cl-letf
              (((symbol-function 'jabber-message-thread-find-buffer)
                (lambda (&rest _) thread-buffer))
               ((symbol-function 'jabber-db-message-thread-summary)
                (lambda (&rest _)
                  '(:thread-id "thread-1" :root-message-id 1)))
               ((symbol-function 'jabber-chat-buffer-refresh)
                (lambda () (setq refreshed t))))
            (jabber-message-thread--refresh-thread-root
             "me@example.com" "alice@example.com" "chat" "thread-1")
            (should refreshed)
            (setq refreshed nil)
            (ewoc-enter-last
             jabber-chat-ewoc
             '(:local (:db-id 1 :thread-id "thread-1")))
            (jabber-message-thread--refresh-thread-root
             "me@example.com" "alice@example.com" "chat" "thread-1")
            (should-not refreshed)))
      (kill-buffer thread-buffer))))

(ert-deftest jabber-test-message-thread-refreshes-idless-parent-root ()
  "Reload new 1:1 and MUC roots when only their rows identify them."
  (let ((parent-buffer (generate-new-buffer " *jabber-parent-refresh*"))
        (summary '(:thread-id "thread-1"
                   :thread-type "chat"
                   :root-message-id 7
                   :reply-count 0))
        (refresh-count 0))
    (unwind-protect
        (with-current-buffer parent-buffer
          (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
          (cl-letf
              (((symbol-function 'jabber-message-thread--parent-buffer)
                (lambda (&rest _) parent-buffer))
               ((symbol-function 'jabber-db-message-thread-summary)
                (lambda (&rest _) summary))
               ((symbol-function 'jabber-chat-buffer-refresh)
                (lambda () (setq refresh-count (1+ refresh-count)))))
            (jabber-message-thread--refresh-root
             "me@example.com" "alice@example.com" "chat" "thread-1")
            (setq summary
                  '(:thread-id "thread-2"
                    :thread-type "groupchat"
                    :root-message-id 8
                    :root-stanza-id "untrusted-client-id"
                    :reply-count 0))
            (jabber-message-thread--refresh-root
             "me@example.com" "room@example.com" "groupchat" "thread-2")
            (should (= refresh-count 2))))
      (kill-buffer parent-buffer))))

(ert-deftest jabber-test-message-thread-open-reuses-stored-thread-id ()
  "Open an ID-less stored root with its received opaque ThreadID."
  (let ((thread-buffer (generate-new-buffer " *jabber-thread-open*"))
        created)
    (unwind-protect
        (with-temp-buffer
          (setq-local jabber-buffer-connection 'connection)
          (setq-local jabber-chatting-with "alice@example.com")
          (cl-letf
              (((symbol-function 'jabber-db-ensure-open) (lambda () t))
               ((symbol-function 'jabber-connection-bare-jid)
                (lambda (_jc) "me@example.com"))
               ((symbol-function 'jabber-db-message-thread-summary)
                (lambda (_account _peer _type thread-id)
                  (and (equal thread-id "received-thread")
                       '(:thread-id "received-thread"
                         :thread-parent-id "parent-thread"
                         :local-reply-count 1))))
               ((symbol-function 'jabber-message-thread--generate-id)
                (lambda () (ert-fail "Generated a replacement ThreadID")))
               ((symbol-function 'jabber-db-register-message-thread)
                (lambda (&rest _) (ert-fail "Re-registered a stored thread")))
               ((symbol-function 'jabber-message-thread--refresh-root) #'ignore)
               ((symbol-function 'jabber-db-mark-message-thread-read) #'ignore)
               ((symbol-function 'jabber-message-thread-create-buffer)
                (lambda (&rest args)
                  (setq created args)
                  thread-buffer))
               ((symbol-function 'pop-to-buffer) #'ignore))
            (jabber-message-thread-open
             '(:body "root" :thread-id "received-thread"))
            (should
             (equal (seq-take created 5)
                    '(connection "alice@example.com" "chat"
                                 "received-thread" "parent-thread")))))
      (kill-buffer thread-buffer))))

(ert-deftest jabber-test-message-thread-completion-items ()
  "Format compact unique thread candidates with useful annotations."
  (let* ((latest (seconds-to-time 30))
         (threads
          `((:thread-id "thread-1" :thread-type "groupchat"
             :root-message (:from "room@example.com/Alice"
                            :body "  Same\nroot  ")
             :reply-count 1 :latest-at ,latest :unread t)
            (:thread-id "thread-2" :thread-type "groupchat"
             :root-message (:from "room@example.com/Alice"
                            :body "Same root")
             :reply-count 2 :latest-at ,latest :unread nil)
            (:thread-id "thread-3" :thread-type "chat"
             :root-message (:body "Alice: Same root (2)")
             :reply-count 0 :latest-at ,latest :unread nil)))
         (items (jabber-message-thread--completion-items threads)))
    (should (equal '("Alice: Same root" "Alice: Same root (2)"
                     "Alice: Same root (2) (2)")
                   (mapcar #'car items)))
    (should
     (equal
      (format "  1 reply · active %s · unread"
              (format-time-string "%Y-%m-%d %H:%M" latest))
      (jabber-message-thread--completion-annotation
       (cdar items))))
    (should
     (equal
      (format "  2 replies · active %s"
              (format-time-string "%Y-%m-%d %H:%M" latest))
      (jabber-message-thread--completion-annotation
       (cdadr items))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Alice: Same root (2) (2)")))
      (should (eq (nth 2 threads)
                  (jabber-message-thread--read-thread threads))))))

(ert-deftest jabber-test-message-thread-completion-handles-missing-muc-root ()
  "Represent a stored MUC thread even when its root row is unavailable."
  (should
   (equal "(no text)"
          (jabber-message-thread--completion-label
           '(:thread-type "groupchat" :root-message nil)))))

(ert-deftest jabber-test-message-thread-completion-hides-retracted-root ()
  "Never expose retained plaintext for a retracted root."
  (let ((label
         (jabber-message-thread--completion-label
          '(:thread-type "chat"
            :root-message (:body "Sensitive root" :retracted t)))))
    (should (equal "[Message retracted]" label))
    (should-not (string-match-p "Sensitive" label))))

(ert-deftest jabber-test-message-thread-read-preserves-activity-order ()
  "Keep database order and expose annotations to completion UIs."
  (let* ((first '(:thread-id "new" :root-message (:body "Newest")
                  :reply-count 0 :latest-at (0 30 0 0)))
         (second '(:thread-id "old" :root-message (:body "Older")
                   :reply-count 0 :latest-at (0 20 0 0)))
         observed)
    (cl-letf
        (((symbol-function 'completing-read)
          (lambda (_prompt table &rest _)
            (let* ((metadata (completion-metadata "" table nil))
                   (sorter
                    (completion-metadata-get
                     metadata 'display-sort-function))
                   (candidates (all-completions "" table)))
              (setq observed
                    (list (funcall sorter candidates)
                          (funcall
                           (plist-get completion-extra-properties
                                      :annotation-function)
                           "Newest")))
              "Newest"))))
      (should (eq first
                  (jabber-message-thread--read-thread
                   (list first second)))))
    (should (equal '("Newest" "Older") (car observed)))
    (should (string-prefix-p "  0 replies · active " (cadr observed)))))

(ert-deftest jabber-test-message-thread-browse-opens-selected-thread ()
  "Browse the current chat and open the selected stored thread."
  (let ((target-buffer (generate-new-buffer " *jabber-browse-target*"))
        (summary
         '(:thread-id "thread-1" :thread-parent-id nil
           :thread-type "chat" :local-reply-count 0
           :root-message (:id "root-1" :from "alice@example.com"
                          :body "Root")))
        created marked refreshed popped)
    (unwind-protect
        (with-temp-buffer
          (setq-local jabber-buffer-connection 'connection)
          (setq-local jabber-chatting-with "alice@example.com/resource")
          (cl-letf
              (((symbol-function 'jabber-db-ensure-open) (lambda () t))
               ((symbol-function 'jabber-connection-bare-jid)
                (lambda (_jc) "me@example.com"))
               ((symbol-function 'jabber-db-message-threads)
                (lambda (&rest _) (list summary)))
               ((symbol-function 'jabber-message-thread--read-thread)
                (lambda (threads)
                  (should (equal threads (list summary)))
                  summary))
               ((symbol-function 'jabber-message-thread-create-buffer)
                (lambda (&rest args)
                  (setq created args)
                  target-buffer))
               ((symbol-function 'jabber-db-mark-message-thread-read)
                (lambda (&rest args) (setq marked args)))
               ((symbol-function 'jabber-message-thread--refresh-root)
                (lambda (&rest args) (setq refreshed args)))
               ((symbol-function 'pop-to-buffer)
                (lambda (buffer &rest _) (setq popped buffer))))
            (let ((parent (current-buffer)))
              (jabber-message-thread-browse)
              (should
               (equal created
                      (list 'connection "alice@example.com" "chat"
                            "thread-1" nil parent
                            (plist-get summary :root-message)))))
            (should
             (equal marked
                    '("me@example.com" "alice@example.com" "chat"
                      "thread-1")))
            (should (equal refreshed marked))
            (should (eq popped target-buffer))))
      (kill-buffer target-buffer))))

(ert-deftest jabber-test-message-thread-browse-context-uses-parent-chat ()
  "Browsing from a thread uses its live parent conversation buffer."
  (let ((parent (generate-new-buffer " *jabber-browse-parent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local jabber-buffer-connection 'connection)
          (setq-local jabber-chatting-with "alice@example.com/resource")
          (setq-local jabber-message-thread-id "thread-1")
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.com"))
                    ((symbol-function 'jabber-message-thread--parent-buffer)
                     (lambda (&rest _) parent)))
            (should
             (equal (jabber-message-thread--browse-context)
                    (list "me@example.com" "alice@example.com" "chat"
                          parent)))))
      (kill-buffer parent))))

(ert-deftest jabber-test-message-thread-browse-opens-muc-without-root ()
  "Browse a stored MUC thread whose root row is unavailable."
  (let ((target (generate-new-buffer " *jabber-browse-muc-target*"))
        (summary '(:thread-id "thread-1" :thread-type "groupchat"
                   :thread-parent-id nil :local-reply-count 0
                   :root-message nil))
        created)
    (unwind-protect
        (with-temp-buffer
          (setq-local jabber-buffer-connection 'connection)
          (setq-local jabber-group "room@example.com")
          (cl-letf
              (((symbol-function 'jabber-db-ensure-open) (lambda () t))
               ((symbol-function 'jabber-connection-bare-jid)
                (lambda (_jc) "me@example.com"))
               ((symbol-function 'jabber-db-message-threads)
                (lambda (&rest _) (list summary)))
               ((symbol-function 'jabber-message-thread--read-thread)
                (lambda (_threads) summary))
               ((symbol-function 'jabber-message-thread-create-buffer)
                (lambda (&rest args) (setq created args) target))
               ((symbol-function 'jabber-db-mark-message-thread-read) #'ignore)
               ((symbol-function 'jabber-message-thread--refresh-root) #'ignore)
               ((symbol-function 'pop-to-buffer) #'ignore))
            (let ((parent (current-buffer)))
              (jabber-message-thread-browse)
              (should
               (equal created
                      (list 'connection "room@example.com" "groupchat"
                            "thread-1" nil parent nil))))))
      (kill-buffer target))))

(ert-deftest jabber-test-message-thread-summary-marker ()
  "Render the reply count as a compact link."
  (with-temp-buffer
    (jabber-chat--insert-thread-summary
     '(:thread-summary
       (:thread-id "thread-1" :reply-count 3 :unread t)))
    (should (equal "\n[3 Replies]" (buffer-string)))
    (should
     (equal '(:inherit link :weight bold)
            (get-text-property 2 'face)))))

(ert-deftest jabber-test-message-thread-summary-button-uses-its-message ()
  "Activating a marker opens its root even when point moved elsewhere."
  (with-temp-buffer
    (let ((msg '(:thread-summary
                 (:thread-id "thread-1" :reply-count 1 :unread nil)))
          opened
          button-start)
      (cl-letf (((symbol-function 'jabber-message-thread--message-at-point)
                 (lambda ()
                   (should (= (point) button-start))
                   msg))
                ((symbol-function 'jabber-message-thread-open)
                 (lambda (&optional root) (setq opened root))))
        (jabber-chat--insert-thread-summary msg)
        (let ((button (button-at 2)))
          (setq button-start (button-start button))
          (goto-char (point-max))
          (button-activate button))
        (should (eq msg opened))))))

(ert-deftest jabber-test-message-thread-disabled-hides-summary ()
  "Do not render thread UI when dedicated buffers are disabled."
  (with-temp-buffer
    (let ((jabber-message-thread-use-buffers nil))
      (jabber-chat--insert-thread-summary
       '(:thread-summary (:reply-count 2 :thread-id "thread-1")))
      (should (string-empty-p (buffer-string))))))

(ert-deftest jabber-test-message-thread-read-summary-is-shadowed-button ()
  "Keep a read thread actionable while rendering its count as shadowed."
  (with-temp-buffer
    (jabber-chat--insert-thread-summary
     '(:thread-summary (:reply-count 2 :thread-id "thread-1")))
    (let ((button (button-at 2)))
      (should button)
      (should (eq 'shadow (button-get button 'face))))))

(ert-deftest jabber-test-message-thread-root-node-does-not-fall-back-from-row ()
  "A stored root row never falls through to a recycled MUC client ID."
  (with-temp-buffer
    (let ((ewoc (ewoc-create #'ignore))
          (summary
           '(:thread-id "thread-1"
             :thread-type "groupchat"
             :root-message-id 7
             :root-stanza-id "same-id")))
      (ewoc-enter-last
       ewoc
       '(:muc-foreign
         (:from "room@example.com/Bob" :id "same-id")))
      (ewoc-enter-last
       ewoc
       '(:muc-foreign
         (:from "room@example.com/Alice" :id "same-id"
                :thread-id "thread-1")))
      (should-not
       (jabber-message-thread--node-for-root ewoc summary)))))

(ert-deftest jabber-test-message-thread-live-muc-root-uses-server-id ()
  "Match a row-less live MUC root by its trusted room server ID."
  (with-temp-buffer
    (let ((ewoc (ewoc-create #'ignore))
          (summary
           '(:thread-id "thread-1"
             :thread-type "groupchat"
             :root-message-id 7
             :root-stanza-id "same-id"
             :root-server-id "root-server-id")))
      (ewoc-enter-last
       ewoc
       '(:muc-local
         (:id "same-id" :server-id "reply-server-id"
              :thread-id "thread-1")))
      (let ((root
             (ewoc-enter-last
              ewoc
              '(:muc-local
                (:id "same-id" :server-id "root-server-id"
                     :thread-id "thread-1")))))
        (should
         (eq root
             (jabber-message-thread--node-for-root ewoc summary)))))))

(ert-deftest jabber-test-message-thread-root-node-id-precedence ()
  "Do not fall through from row or MUC server identity."
  (should-not
   (jabber-message-thread--root-node-p
    '(:db-id 8 :server-id "root-server-id")
    '(:thread-type "groupchat" :root-server-id "root-server-id")))
  (should-not
   (jabber-message-thread--root-node-p
    '(:id "root-id" :thread-id "thread-1")
    '(:thread-type "groupchat" :root-stanza-id "root-id"
      :thread-id "thread-1"))))

(ert-deftest jabber-test-message-thread-exclusive-display-target ()
  "Known replies use only their open thread buffer."
  (let ((thread-buffer (generate-new-buffer " *jabber-thread-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-db-message-thread-known-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'jabber-db-message-thread-root-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'jabber-message-thread-find-buffer)
                   (lambda (&rest _) thread-buffer)))
          (should
           (eq thread-buffer
               (jabber-message-thread-display-target
                'jc "alice@example.com" "chat"
                '(:thread-id "thread-1" :id "reply-1"))))
          (cl-letf (((symbol-function 'jabber-message-thread-find-buffer)
                     (lambda (&rest _) nil)))
            (should-not
             (jabber-message-thread-display-target
              'jc "alice@example.com" "chat"
              '(:thread-id "thread-1" :id "reply-1")))))
      (kill-buffer thread-buffer))))

(ert-deftest jabber-test-message-thread-disabled-display-target-is-parent ()
  "Route threaded messages through the original parent-buffer path."
  (let ((jabber-message-thread-use-buffers nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-db-message-thread-known-p)
               (lambda (&rest _) t))
              ((symbol-function 'jabber-db-message-thread-root-p)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-message-thread-find-buffer)
               (lambda (&rest _)
                 (ert-fail "Looked up a disabled thread buffer"))))
      (should
       (eq 'parent
           (jabber-message-thread-display-target
            'jc "alice@example.com" "chat"
            '(:thread-id "thread-1" :id "reply-1")))))))

(ert-deftest jabber-test-message-thread-muc-private-is-unavailable ()
  "Dedicated thread commands reject MUC private chat buffers."
  (with-temp-buffer
    (let ((jabber-message-thread-use-buffers t))
      (setq-local jabber-muc-private-p t)
      (should-not (jabber-message-thread-available-p))
      (should-error (jabber-message-thread--ensure-buffers)
                    :type 'user-error))))

(ert-deftest jabber-test-message-thread-muc-private-routes-through-parent ()
  "A threaded MUC private message keeps the ordinary occupant buffer path."
  (let ((parent (generate-new-buffer " *jabber-muc-private-thread-parent*"))
        displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) nil))
                  ((symbol-function 'jabber-muc-sender-p) (lambda (_) t))
                  ((symbol-function 'jabber-chat--unwrap-carbon)
                   (lambda (_jc xml) (cons xml nil)))
                  ((symbol-function 'jabber-chat--decrypt-if-needed)
                   (lambda (_jc xml) xml))
                  ((symbol-function 'jabber-chat--select-buffer)
                   (lambda (&rest _) parent))
                  ((symbol-function 'jabber-message-thread-display-target)
                   (lambda (&rest _)
                     (ert-fail "Selected a dedicated private thread")))
                  ((symbol-function 'jabber-chat--display-message)
                   (lambda (_jc _xml buffer &rest _)
                     (setq displayed buffer)))
                  ((symbol-function 'run-hook-with-args-until-success)
                   (lambda (&rest _) t))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "me@example.com")))
          (jabber-process-chat
           'fake-jc
           '(message ((from . "room@example.com/Alice") (type . "chat"))
                     (body () "reply")
                     (thread () "thread-1")))
          (should (eq displayed parent)))
      (kill-buffer parent))))

(ert-deftest jabber-test-message-thread-disabled-uses-original-update-routing ()
  "Leave buffer selection to the original non-thread update path."
  (let ((jabber-message-thread-use-buffers nil))
    (cl-letf (((symbol-function 'jabber-message-thread--parent-buffer)
               (lambda (&rest _)
                 (ert-fail "Looked up a disabled thread parent")))
              ((symbol-function 'jabber-message-thread-find-buffer)
               (lambda (&rest _)
                 (ert-fail "Looked up a disabled thread buffer"))))
      (should-not
       (jabber-message-thread--update-targets
        "me@example.com" "alice@example.com" "chat"
        '(:thread-id "thread-1" :root nil))))))

(ert-deftest jabber-test-message-thread-disabled-rejects-thread-send-hook ()
  "Prevent stale thread buffers from sending threaded messages."
  (with-temp-buffer
    (let ((jabber-message-thread-use-buffers nil))
      (setq-local jabber-message-thread-id "thread-1")
      (should-error
       (jabber-message-thread--send-hook "reply" "message-1")
       :type 'user-error))))

(ert-deftest jabber-test-message-thread-local-reply-uses-open-thread-buffer ()
  "A local threaded reply is echoed only in its open thread buffer."
  (let ((parent (generate-new-buffer " *jabber-local-parent*"))
        (thread (generate-new-buffer " *jabber-local-thread*")))
    (unwind-protect
        (progn
          (dolist (buffer (list parent thread))
            (with-current-buffer buffer
              (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
              (setq-local jabber-chat--msg-nodes
                          (make-hash-table :test #'equal))))
          (with-current-buffer parent
            (setq-local jabber-chatting-with "alice@example.com")
            (setq-local jabber-chat-encryption nil)
            (let ((jabber-chat-printers (list (lambda (&rest _) t)))
                  (jabber-chat-send-hooks
                   (list (lambda (&rest _)
                           '((thread () "thread-1"))))))
              (cl-letf
                  (((symbol-function 'jabber-connection-bare-jid)
                    (lambda (_jc) "me@example.com"))
                   ((symbol-function 'jabber-db-message-thread-location)
                    (lambda (&rest _)
                      '(:thread-id "thread-1" :root nil)))
                   ((symbol-function 'jabber-message-thread-find-buffer)
                    (lambda (&rest _) thread))
                   ((symbol-function 'jabber-send-sexp) #'ignore))
                (jabber-chat-send 'connection "reply"))))
          (should-not
           (with-current-buffer parent (ewoc-nth jabber-chat-ewoc 0)))
          (should
           (equal "thread-1"
                  (with-current-buffer thread
                    (plist-get
                     (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc -1)))
                     :thread-id)))))
      (kill-buffer parent)
      (kill-buffer thread))))

(ert-deftest jabber-test-message-thread-local-reply-stays-closed ()
  "A local reply to a closed thread is not echoed into the parent."
  (with-temp-buffer
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-chat-encryption nil)
    (let ((jabber-chat-printers (list (lambda (&rest _) t)))
          (jabber-chat-send-hooks
           (list (lambda (&rest _) '((thread () "thread-1"))))))
      (cl-letf
          (((symbol-function 'jabber-connection-bare-jid)
            (lambda (_jc) "me@example.com"))
           ((symbol-function 'jabber-db-message-thread-location)
            (lambda (&rest _) '(:thread-id "thread-1" :root nil)))
           ((symbol-function 'jabber-message-thread-find-buffer)
            (lambda (&rest _) nil))
           ((symbol-function 'jabber-send-sexp) #'ignore))
        (jabber-chat-send 'connection "reply")))
    (should-not (ewoc-nth jabber-chat-ewoc 0))))

(ert-deftest jabber-test-message-thread-local-root-stays-in-parent ()
  "A newly sent thread root keeps its canonical parent echo."
  (with-temp-buffer
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (setq-local jabber-chatting-with "alice@example.com")
    (setq-local jabber-chat-encryption nil)
    (let ((jabber-chat-printers (list (lambda (&rest _) t)))
          (jabber-chat-send-hooks nil))
      (cl-letf
          (((symbol-function 'jabber-connection-bare-jid)
            (lambda (_jc) "me@example.com"))
           ((symbol-function 'jabber-db-message-thread-location)
            (lambda (&rest _) '(:thread-id "thread-1" :root t)))
           ((symbol-function 'jabber-message-thread-find-buffer)
            (lambda (&rest _) (ert-fail "Looked up a root as a reply")))
           ((symbol-function 'jabber-send-sexp) #'ignore))
        (jabber-chat-send
         'connection "root" '((thread () "thread-1")))))
    (should
     (equal "thread-1"
            (plist-get
             (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc -1)))
             :thread-id)))))

(ert-deftest jabber-test-message-thread-unknown-message-is-local-root ()
  "The first message seen for an unknown thread stays in the parent."
  (cl-letf (((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.com"))
            ((symbol-function 'jabber-db-message-thread-known-p)
             (lambda (&rest _) nil)))
    (should
     (eq 'parent
         (jabber-message-thread-display-target
          'jc "alice@example.com" "chat"
          '(:thread-id "thread-1" :id "first-seen"))))))

(ert-deftest jabber-test-message-thread-known-idless-message-is-reply ()
  "An ID-less message in a known thread is not inferred to be its root."
  (cl-letf (((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.com"))
            ((symbol-function 'jabber-db-message-thread-known-p)
             (lambda (&rest _) t))
            ((symbol-function 'jabber-db-message-thread-root-p)
             (lambda (&rest _) nil))
            ((symbol-function 'jabber-message-thread-find-buffer)
             (lambda (&rest _) nil)))
    (should-not
     (jabber-message-thread-display-target
      'jc "alice@example.com" "chat"
      '(:thread-id "thread-1")))))

(ert-deftest jabber-test-message-thread-update-targets ()
  "Roots update both views and closed replies do not fall back to parent."
  (let ((parent (generate-new-buffer " *jabber-thread-parent*"))
        (thread (generate-new-buffer " *jabber-thread-child*")))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-message-thread--parent-buffer)
                   (lambda (&rest _) parent))
                  ((symbol-function 'jabber-message-thread-find-buffer)
                   (lambda (&rest _) thread))
                  ((symbol-function 'jabber-db-message-thread-location)
                   (lambda (&rest _)
                     '(:thread-id "thread-1" :root t))))
          (should
           (equal (list parent thread)
                  (jabber-message-thread-update-targets
                   'jc "alice@example.com" "chat" "root-1")))
          (cl-letf (((symbol-function 'jabber-db-message-thread-location)
                     (lambda (&rest _)
                       '(:thread-id "thread-1" :root nil)))
                    ((symbol-function 'jabber-message-thread-find-buffer)
                     (lambda (&rest _) nil)))
            (should
             (eq 'closed
                 (jabber-message-thread-update-targets
                  'jc "alice@example.com" "chat" "reply-1")))))
      (kill-buffer parent)
      (kill-buffer thread))))

(ert-deftest jabber-test-message-thread-chat-correction-uses-original-owner ()
  "A 1:1 correction follows the replaced message, not its own thread field."
  (let ((target (generate-new-buffer " *jabber-thread-correction*"))
        applied)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) nil))
                  ((symbol-function 'jabber-chat--unwrap-carbon)
                   (lambda (_jc xml) (cons xml nil)))
                  ((symbol-function 'jabber-chat--decrypt-if-needed)
                   (lambda (_jc xml) xml))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function
                    'jabber-message-thread-update-targets-for-row)
                   (lambda (_jc _peer _type row-id)
                     (should (= row-id 7))
                     (list target)))
                  ((symbol-function 'jabber-chat--select-buffer)
                   (lambda (&rest _) (ert-fail "selected parent buffer")))
                  ((symbol-function 'jabber-message-correct--apply)
                   (lambda (&rest args)
                     (setq applied (funcall (nth 4 args)
                                            '(:row-id 7))))))
          (jabber-process-chat
           'jc
           '(message ((from . "alice@example.com/phone")
                      (type . "chat") (id . "correction-1"))
                     (body () "corrected")
                     (replace ((xmlns . "urn:xmpp:message-correct:0")
                               (id . "reply-1")))
                     (thread () "wrong-thread")))
          (should (equal (list target) applied)))
      (kill-buffer target))))

(ert-deftest jabber-test-message-thread-unthreaded-correction-does-not-create ()
  "An unthreaded correction keeps the old find-only buffer behavior."
  (let (applied)
    (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) nil))
              ((symbol-function 'jabber-chat--unwrap-carbon)
               (lambda (_jc xml) (cons xml nil)))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc xml) xml))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function
                'jabber-message-thread-update-targets-for-row)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-chat-find-buffer)
               (lambda (_) nil))
              ((symbol-function 'jabber-chat--select-buffer)
               (lambda (&rest _) (ert-fail "created parent buffer")))
              ((symbol-function 'jabber-message-correct--apply)
               (lambda (&rest args)
                 (setq applied (funcall (nth 4 args)
                                        '(:row-id 7))))))
      (jabber-process-chat
       'jc
       '(message ((from . "alice@example.com/phone")
                  (type . "chat") (id . "correction-1"))
                 (body () "corrected")
                 (replace ((xmlns . "urn:xmpp:message-correct:0")
                           (id . "message-1")))))
      (should-not applied))))

(ert-deftest jabber-test-message-thread-muc-correction-uses-original-owner ()
  "A MUC correction follows the replaced message's thread."
  (let ((target (generate-new-buffer " *jabber-muc-thread-correction*"))
        applied)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) t))
                  ((symbol-function 'jabber-chat--decrypt-if-needed)
                   (lambda (_jc xml) xml))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-muc--classify-message)
                   (lambda (&rest _) :muc-foreign))
                  ((symbol-function
                    'jabber-message-thread-update-targets-for-row)
                   (lambda (_jc _peer _type row-id)
                     (should (= row-id 8))
                     (list target)))
                  ((symbol-function 'jabber-muc-find-buffer)
                   (lambda (&rest _) (ert-fail "selected parent buffer")))
                  ((symbol-function 'jabber-message-correct--apply)
                   (lambda (&rest args)
                     (setq applied (funcall (nth 4 args)
                                            '(:row-id 8)))))
                  ((symbol-function
                    'jabber-message-correct--muc-current-target-p)
                   (lambda (&rest _) t)))
          (jabber-muc-process-message
           'jc
           '(message ((from . "room@example.com/Alice")
                      (type . "groupchat") (id . "correction-1"))
                     (body () "corrected")
                     (replace ((xmlns . "urn:xmpp:message-correct:0")
                               (id . "reply-1")))
                     (thread () "wrong-thread")))
          (should (equal (list target) applied)))
      (kill-buffer target))))

(ert-deftest jabber-test-message-thread-muc-correction-resolves-authorized-row ()
  "A recycled MUC client ID routes through the sender-authorized row."
  (let (corrected-row resolved-row)
    (cl-letf (((symbol-function 'jabber-db-message-correction-candidates)
               (lambda (&rest _)
                 '((:row-id 7 :from "room@example.com/Alice")
                   (:row-id 8 :from "room@example.com/Bob"))))
              ((symbol-function 'jabber-db-correct-message-row)
               (lambda (row-id _body) (setq corrected-row row-id))))
      (should
       (jabber-message-correct--apply
        "same-id" "corrected" "room@example.com/Alice" t
        (lambda (original)
          (setq resolved-row (plist-get original :row-id))
          nil)
        nil "me@example.com" "room@example.com" t))
      (should (= 7 corrected-row))
      (should (= 7 resolved-row)))))

(ert-deftest jabber-test-message-thread-closed-chat-alert-has-parent-buffer ()
  "A closed reply alerts with the parent buffer without inserting there."
  (let ((parent (generate-new-buffer " *jabber-thread-alert-parent*"))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-chat--select-buffer)
                   (lambda (&rest _) parent)))
          (let ((jabber-message-hooks
                 (list (lambda (_from buffer _body _alert)
                         (setq seen buffer))))
                (jabber-alert-message-hooks nil)
                (jabber-alert-message-function
                 (lambda (&rest _) "alert")))
            (jabber-chat--display-message
             'jc nil nil nil "alice@example.com"
             '(:body "reply" :thread-id "thread-1"))
            (should (eq parent seen))
            (should (equal "" (with-current-buffer parent
                                (buffer-string))))))
      (kill-buffer parent))))

(ert-deftest jabber-test-message-thread-closed-muc-alert-has-parent-buffer ()
  "A closed MUC reply alerts with the room buffer without insertion."
  (let ((parent (generate-new-buffer " *jabber-thread-muc-alert-parent*"))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (&rest _) parent)))
          (let ((jabber-muc-hooks
                 (list (lambda (_nick _group buffer _body _alert)
                         (setq seen buffer))))
                (jabber-alert-muc-hooks nil)
                (jabber-alert-muc-function
                 (lambda (&rest _) "alert")))
            (jabber-muc--display-message
             'jc
             '(message ((from . "room@example.com/Alice")
                        (type . "groupchat")))
             "room@example.com" "Alice" :muc-foreign
             '(:body "reply" :thread-id "thread-1")
             'closed)
            (should (eq parent seen))
            (should (equal "" (with-current-buffer parent
                                (buffer-string))))))
      (kill-buffer parent))))

(ert-deftest jabber-test-message-thread-closed-muc-reply-stays-closed ()
  "MUC dispatch maps a closed known reply to the closed sentinel."
  (let (display-target)
    (cl-letf (((symbol-function 'jabber-muc-message-p) (lambda (_) t))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc xml) xml))
              ((symbol-function 'jabber-muc--classify-message)
               (lambda (&rest _) :muc-foreign))
              ((symbol-function 'jabber-message-thread-display-target)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc-find-buffer)
               (lambda (&rest _) (ert-fail "selected parent buffer")))
              ((symbol-function 'jabber-muc--display-message)
               (lambda (_jc _xml _group _nick _type _msg
                            &optional target)
                 (setq display-target target))))
      (jabber-muc-process-message
       'jc
       '(message ((from . "room@example.com/Alice")
                  (type . "groupchat"))
                 (body () "reply")
                 (thread () "thread-1")))
      (should (eq display-target 'closed)))))

(ert-deftest jabber-test-message-thread-root-reaction-updates-both-views ()
  "A reaction to a root updates the canonical and thread views."
  (let ((parent (generate-new-buffer " *jabber-reaction-parent*"))
        (thread (generate-new-buffer " *jabber-reaction-thread*"))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-reactions--unwrap-stanza)
                   (lambda (_jc xml) (cons xml nil)))
                  ((symbol-function 'jabber-reactions--single-element)
                   (lambda (_) 'reactions))
                  ((symbol-function 'jabber-reactions--parse-element)
                   (lambda (_) '("root-1" ("👍"))))
                  ((symbol-function 'jabber-reactions--persist-update)
                   (lambda (&rest _) :ok))
                  ((symbol-function 'jabber-reactions--storage-peer)
                   (lambda (&rest _) "alice@example.com"))
                  ((symbol-function 'jabber-message-thread-update-targets)
                   (lambda (&rest _) (list parent thread)))
                  ((symbol-function 'jabber-chat-ewoc-find-by-id)
                   (lambda (_) 'node))
                  ((symbol-function 'jabber-reactions--apply-incoming-update)
                   (lambda (&rest _) (push (current-buffer) seen))))
          (jabber-reactions--handle-message
           'jc
           '(message ((from . "alice@example.com")
                      (type . "chat"))))
          (should (equal (list thread parent) seen)))
      (kill-buffer parent)
      (kill-buffer thread))))

(ert-deftest jabber-test-message-thread-root-moderation-updates-both-views ()
  "Moderating a root updates the canonical and thread views."
  (let ((parent (generate-new-buffer " *jabber-moderation-parent*"))
        (thread (generate-new-buffer " *jabber-moderation-thread*"))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-moderation--retraction-element)
                   (lambda (_)
                     (cons '(retract ()
                                     (moderated ())
                                     (reason () "spam"))
                           nil)))
                  ((symbol-function 'jabber-moderation--target-id)
                   (lambda (&rest _) "root-1"))
                  ((symbol-function 'jabber-moderation--moderator)
                   (lambda (&rest _) "mod@example.com"))
                  ((symbol-function 'jabber-message-thread-update-targets)
                   (lambda (&rest _) (list parent thread)))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-db-retract-message-in-peer)
                   #'ignore)
                  ((symbol-function 'jabber-moderation--mark-ewoc-retracted)
                   (lambda (&rest _) (push (current-buffer) seen))))
          (jabber-moderation--handle-message
           'jc
           '(message ((from . "room@example.com")
                      (type . "groupchat"))))
          (should (equal (list thread parent) seen)))
      (kill-buffer parent)
      (kill-buffer thread))))

(ert-deftest jabber-test-message-thread-local-moderation-updates-owner ()
  "Successful local moderation updates a reply's owning thread view."
  (let ((thread (generate-new-buffer " *jabber-local-moderation-thread*"))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-db-retract-message-in-peer)
                   #'ignore)
                  ((symbol-function 'jabber-message-thread-update-targets)
                   (lambda (jc peer type id server-id-p)
                     (should (eq jc 'jc))
                     (should (equal peer "room@example.com"))
                     (should (equal type "groupchat"))
                     (should (equal id "reply-1"))
                     (should server-id-p)
                     (list thread)))
                  ((symbol-function 'jabber-muc-find-buffer)
                   (lambda (&rest _)
                     (ert-fail "Selected the parent MUC buffer")))
                  ((symbol-function 'jabber-moderation--mark-ewoc-retracted)
                   (lambda (&rest _) (push (current-buffer) seen))))
          (jabber-moderation--mark-local-retracted
           'jc nil
           '("room@example.com" "reply-1" "room@example.com/Mod" "spam"))
          (should (equal (list thread) seen)))
      (kill-buffer thread))))

(ert-deftest jabber-test-message-thread-display-plist ()
  "Chat display data retains thread identity."
  (let ((msg (jabber-chat--msg-plist-from-stanza
              '(message ((from . "alice@example.com") (type . "chat"))
                        (body () "hello")
                        (thread ((parent . "parent-1")) "child-1")))))
    (should (equal "child-1" (plist-get msg :thread-id)))
    (should (equal "parent-1" (plist-get msg :thread-parent-id)))))

(provide 'jabber-test-message-thread)
;;; jabber-test-message-thread.el ends here
