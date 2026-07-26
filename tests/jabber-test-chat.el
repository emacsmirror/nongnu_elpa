;;; jabber-test-chat.el --- Tests for jabber-chat  -*- lexical-binding: t; -*-

;;; Commentary:

;; One-to-one chat message parsing and display.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-chat-commands)

(defun jabber-test-chat--make-fake-jc (account)
  "Create a fake connection symbol for ACCOUNT."
  (let ((jc (gensym "jabber-test-chat-jc-"))
        (parts (split-string account "@")))
    (put jc :state-data (list :username (nth 0 parts)
                              :server (nth 1 parts)))
    jc))

;; jabber-chat uses this constant from jabber-muc, which has too many
;; dependencies to load in isolation.  Define it here for tests.
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")

;;; Group 1: jabber-chat--msg-plist-from-stanza

(ert-deftest jabber-test-chat-plist-from-stanza-basic ()
  "Basic chat message produces correct plist keys."
  (let* ((stanza '(message ((from . "alice@example.com/res")
                            (type . "chat"))
                           (body () "Hello!")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (string= "alice@example.com/res" (plist-get plist :from)))
    (should (string= "Hello!" (plist-get plist :body)))
    (should-not (plist-get plist :subject))
    (should-not (plist-get plist :delayed))
    (should-not (plist-get plist :oob-url))
    (should-not (plist-get plist :error-text))
    (should (plist-get plist :timestamp))))

(ert-deftest jabber-test-chat-plist-from-stanza-nil-body ()
  "Message with no body produces nil :body."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (subject () "Topic")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :body))
    (should (string= "Topic" (plist-get plist :subject)))))

(ert-deftest jabber-test-chat-plist-from-stanza-muc ()
  "MUC message has room JID with nick as resource."
  (let* ((stanza '(message ((from . "room@conf.example.com/Alice")
                            (type . "groupchat"))
                           (body () "Hi room")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (string= "room@conf.example.com/Alice" (plist-get plist :from)))
    (should (string= "Hi room" (plist-get plist :body)))))

(ert-deftest jabber-test-chat-plist-from-stanza-delay ()
  "Message with XEP-0203 delay element is marked delayed."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Old message")
                           (delay ((xmlns . "urn:xmpp:delay")
                                   (stamp . "2025-01-15T10:30:00Z")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (plist-get plist :delayed))
    (should (string= "Old message" (plist-get plist :body)))))

(ert-deftest jabber-test-chat-plist-from-stanza-forced-delay ()
  "Passing DELAYED arg forces :delayed to non-nil."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Backlog")))
         (plist (jabber-chat--msg-plist-from-stanza stanza t)))
    (should (plist-get plist :delayed))))

(ert-deftest jabber-test-chat-plist-from-stanza-oob ()
  "OOB URL and description are extracted."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Check this")
                           (x ((xmlns . "jabber:x:oob"))
                              (url () "https://example.com/file.png")
                              (desc () "A picture"))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (string= "https://example.com/file.png" (plist-get plist :oob-url)))
    (should (string= "A picture" (plist-get plist :oob-desc)))))

(ert-deftest jabber-test-chat-plist-from-stanza-error ()
  "Error node is parsed into :error-text."
  (let* ((stanza '(message ((from . "alice@example.com")
                            (type . "error"))
                           (body () "Bad request")
                           (error ((type . "modify") (code . "400"))
                                  (bad-request
                                   ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (stringp (plist-get plist :error-text)))))

(ert-deftest jabber-test-chat-plist-from-stanza-oob-no-url ()
  "OOB element with no url child yields nil :oob-url."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Check this")
                           (x ((xmlns . "jabber:x:oob")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :oob-url))))

(ert-deftest jabber-test-chat-plist-from-stanza-invite ()
  "MUC invitation preserves raw XML in :xml-data."
  (let* ((stanza '(message ((from . "room@conf.example.com"))
                           (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                              (invite ((from . "alice@example.com"))
                                      (reason () "Join us")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (plist-get plist :xml-data))
    (should (eq stanza (plist-get plist :xml-data)))))

(ert-deftest jabber-test-chat-plist-from-stanza-no-invite-no-xml ()
  "Non-invitation message does not include :xml-data."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Normal message")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :xml-data))))

(ert-deftest jabber-test-chat-plist-from-stanza-server-id ()
  "Valid stanza-id element sets :server-id."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "Hello")
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "server-1")
                                       (by . "room@muc.example.com")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal (plist-get plist :server-id) "server-1"))))

(ert-deftest jabber-test-chat-plist-from-stanza-skips-origin-id ()
  "Origin-id before stanza-id does not become :server-id."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "Hello")
                           (origin-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "origin-1")))
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "server-1")
                                       (by . "room@muc.example.com")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal (plist-get plist :server-id) "server-1"))))

(ert-deftest jabber-test-chat-plist-from-stanza-rejects-stanza-id-without-by ()
  "Stanza-id without by is not treated as a server id."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "Hello")
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "server-1")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :server-id))))

(ert-deftest jabber-test-chat-plist-groupchat-stanza-id-wrong-by-rejected ()
  "Groupchat stanza-id with a by not matching the room is rejected."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "Hello")
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "spoofed-1")
                                       (by . "attacker@evil.example")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :server-id))))

(ert-deftest jabber-test-chat-plist-groupchat-stanza-id-skips-spoofed-by ()
  "The room's stanza-id wins even when a spoofed one comes first."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "Hello")
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "spoofed-1")
                                       (by . "attacker@evil.example")))
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "server-1")
                                       (by . "room@muc.example.com")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal (plist-get plist :server-id) "server-1"))))

(ert-deftest jabber-test-chat-plist-chat-stanza-id-any-by-accepted ()
  "1:1 chat messages keep accepting stanza-id from any archive."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (type . "chat"))
                           (body () "Hello")
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "archive-1")
                                       (by . "me@example.com")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal (plist-get plist :server-id) "archive-1"))))

(ert-deftest jabber-test-chat-plist-parses-origin-id ()
  "Origin-id and stanza-id each land in their own plist key."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (id . "client-1")
                            (type . "chat"))
                           (body () "Hello")
                           (origin-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "origin-1")))
                           (stanza-id ((xmlns . "urn:xmpp:sid:0")
                                       (id . "archive-1")
                                       (by . "me@example.com")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "origin-1" (plist-get plist :origin-id)))
    (should (equal "archive-1" (plist-get plist :server-id)))
    (should (equal "client-1" (plist-get plist :id)))))

(ert-deftest jabber-test-chat-plist-reply-fallback-range-parsed ()
  "Reply fallback body offsets land in :fallback-range."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello\nanswer")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/phone")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "0")
                                            (end . "17"))))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal '(0 17) (plist-get plist :fallback-range)))))

(ert-deftest jabber-test-chat-plist-reply-fallback-range-all ()
  "Fallback without a body child covers the whole body."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/phone")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (eq 'all (plist-get plist :fallback-range)))))

(ert-deftest jabber-test-chat-plist-reply-fallback-range-bare-body-all ()
  "A fallback <body/> without offsets covers the whole body."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/phone")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ()))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (eq 'all (plist-get plist :fallback-range)))))

(ert-deftest jabber-test-chat-plist-reply-fallback-range-malformed-nil ()
  "Malformed fallback offsets yield a nil :fallback-range."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello\nanswer")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/phone")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "x")
                                            (end . "17"))))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :fallback-range))))

(defmacro jabber-test-chat--with-reply-ewoc (&rest body)
  "Run BODY in a temp buffer with an original and a reply node.
The original has id \"orig-1\"; the reply references it.  Point
starts on the reply node; `jabber-point-insert' marks the input
area after both messages."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create
                              (lambda (data)
                                (insert (plist-get (cadr data) :body) "\n"))
                              nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       (jabber-chat-ewoc-enter
        (list :foreign (list :id "orig-1" :from "alice@x.com"
                             :body "the original"
                             :timestamp (current-time))))
       (let ((reply-node
              (jabber-chat-ewoc-enter
               (list :foreign (list :id "r-1" :from "alice@x.com"
                                    :body "the reply"
                                    :reply-to-id "orig-1"
                                    :timestamp (current-time))))))
         (setq-local jabber-point-insert (point-max-marker))
         (goto-char (ewoc-location reply-node))
         ,@body))))

(ert-deftest jabber-test-chat-reply-target-at-point ()
  "The reply target is found on the reply node and nowhere else."
  (jabber-test-chat--with-reply-ewoc
    (should (equal "orig-1" (jabber-chat--reply-target-at-point)))
    (goto-char (point-min))
    (should-not (jabber-chat--reply-target-at-point))
    (goto-char (point-max))
    (should-not (jabber-chat--reply-target-at-point))))

(ert-deftest jabber-test-chat-goto-reply-target-jumps ()
  "RET on a reply moves point to the original message."
  (jabber-test-chat--with-reply-ewoc
    (cl-letf (((symbol-function 'pulse-momentary-highlight-region)
               #'ignore))
      (jabber-chat-goto-reply-target))
    (should (= (point) (point-min)))
    (should (looking-at "the original"))))

(ert-deftest jabber-test-chat-goto-reply-target-or-send-dispatch ()
  "RET sends from the input area and jumps from a reply."
  (jabber-test-chat--with-reply-ewoc
    (let ((sent nil))
      (cl-letf (((symbol-function 'jabber-chat-buffer-send)
                 (lambda () (setq sent t)))
                ((symbol-function 'pulse-momentary-highlight-region)
                 #'ignore))
        (goto-char (point-max))
        (jabber-chat-goto-reply-target-or-send)
        (should sent)
        (setq sent nil)
        (goto-char (point-min))
        (ewoc-goto-next jabber-chat-ewoc 1)
        (jabber-chat-goto-reply-target-or-send)
        (should-not sent)
        (should (= (point) (point-min)))))))

(ert-deftest jabber-test-chat-reply-context-synthesizes-quote ()
  "A fallback-less reply quotes the original body from the database."
  (with-temp-buffer
    (setq-local jabber-chatting-with "alice@x.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@x.com"))
              ((symbol-function 'jabber-muc-sender-p)
               (lambda (_jid) nil))
              ((symbol-function 'jabber-db-reply-target-body)
               (lambda (_account _peer reply-id _muc-p)
                 (and (equal reply-id "orig-1")
                      "original text\nsecond line"))))
      (jabber-chat--insert-reply-context
       '(:reply-to-id "orig-1" :reply-to-jid "alice@x.com"))
      (should (string-match-p "reply to alice@x.com: original text"
                              (buffer-string)))
      (should-not (string-match-p "second line" (buffer-string))))))

(ert-deftest jabber-test-chat-reply-context-label-without-db-hit ()
  "A fallback-less reply falls back to the bare label when unresolved."
  (with-temp-buffer
    (setq-local jabber-chatting-with "alice@x.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@x.com"))
              ((symbol-function 'jabber-muc-sender-p)
               (lambda (_jid) nil))
              ((symbol-function 'jabber-db-reply-target-body)
               (lambda (&rest _) nil)))
      (jabber-chat--insert-reply-context
       '(:reply-to-id "orig-2" :reply-to-jid "alice@x.com"))
      (should (equal "reply to alice@x.com\n" (buffer-string))))))

(ert-deftest jabber-test-chat-outgoing-handler-stores-reply-metadata ()
  "The DB outgoing handler reads reply elements off the final stanza."
  (require 'jabber-db)
  (with-temp-buffer
    (setq-local jabber-chatting-with "alice@x.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let (stored-reply)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@x.com"))
                ((symbol-function 'jabber-muc-sender-p)
                 (lambda (_jid) nil))
                ((symbol-function 'jabber-db-store-message)
                 (lambda (&rest args) (setq stored-reply (nth 12 args)))))
        (let ((stanza '(message ((to . "alice@x.com")
                                 (type . "chat")
                                 (id . "m-9"))
                                (body () "> q\nanswer")
                                (reply ((xmlns . "urn:xmpp:reply:0")
                                        (id . "orig-9")))))
              (jabber-chat-send-hooks (list #'jabber-db--outgoing-handler)))
          (jabber-chat--run-send-hooks stanza "> q\nanswer" "m-9")))
      (should (equal "orig-9" (plist-get stored-reply :reply-to-id))))))

(ert-deftest jabber-test-chat-outgoing-handler-skips-corrections ()
  "The outgoing DB hook does not store a correction as a new message."
  (let ((jabber-chat--sending-correction t)
        (jabber-chatting-with "friend@example.com")
        (jabber-buffer-connection 'fake-jc)
        stored)
    (cl-letf (((symbol-function 'jabber-db-store-message)
               (lambda (&rest _) (setq stored t))))
      (jabber-db--outgoing-handler "corrected" "correction-id"))
    (should-not stored)))

(ert-deftest jabber-test-chat-send-hooks-stamp-origin-id ()
  "The default send hooks stamp an XEP-0359 origin-id on outgoing stanzas."
  (with-temp-buffer
    (let ((stanza '(message ((to . "alice@example.com")
                             (type . "chat")
                             (id . "m-1"))
                            (body () "hi"))))
      (jabber-chat--run-send-hooks stanza "hi" "m-1")
      (let ((el (seq-find (lambda (child)
                            (and (consp child) (eq (car child) 'origin-id)))
                          (jabber-xml-node-children stanza))))
        (should el)
        (should (equal "m-1" (jabber-xml-get-attribute el 'id)))
        (should (equal "urn:xmpp:sid:0" (jabber-xml-get-xmlns el)))))))

(ert-deftest jabber-test-chat-origin-id-round-trip ()
  "A stanza stamped by the send hook parses back into :origin-id."
  (with-temp-buffer
    (let ((stanza '(message ((to . "alice@example.com")
                             (type . "chat")
                             (id . "m-2"))
                            (body () "hi"))))
      (jabber-chat--run-send-hooks stanza "hi" "m-2")
      (should (equal "m-2" (plist-get (jabber-chat--msg-plist-from-stanza stanza)
                                      :origin-id))))))

(ert-deftest jabber-test-chat-plist-reply-fallback-not-masked ()
  "A non-reply <fallback> before the reply one must not mask it."
  (let* ((stanza '(message ((from . "alice@example.com/phone")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello\nanswer")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/phone")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reactions:0")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "0")
                                            (end . "17"))))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal '(0 17) (plist-get plist :fallback-range)))))

;;; Group 2: jabber-chat--oob-field

(ert-deftest jabber-test-chat-oob-field-url ()
  "Extract URL from OOB node."
  (let ((oob '(x ((xmlns . "jabber:x:oob"))
                  (url () "https://example.com/file.png"))))
    (should (string= (jabber-chat--oob-field oob 'url)
                     "https://example.com/file.png"))))

(ert-deftest jabber-test-chat-oob-field-missing-child ()
  "Return nil when OOB child element is absent."
  (let ((oob '(x ((xmlns . "jabber:x:oob"))
                  (url () "https://example.com/file.png"))))
    (should-not (jabber-chat--oob-field oob 'desc))))

(ert-deftest jabber-test-chat-oob-field-nil-node ()
  "Return nil when OOB node is nil."
  (should-not (jabber-chat--oob-field nil 'url)))

;;; Group 3: jabber-chat--has-muc-invite-p

(ert-deftest jabber-test-chat-has-muc-invite-positive ()
  "Detect MUC invitation in stanza."
  (let ((stanza '(message ((from . "room@conf.example.com"))
                  (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (invite ((from . "alice@example.com")))))))
    (should (jabber-chat--has-muc-invite-p stanza))))

(ert-deftest jabber-test-chat-has-muc-invite-negative ()
  "Return nil for stanza without MUC invitation."
  (let ((stanza '(message ((from . "alice@example.com"))
                  (body () "Hello"))))
    (should-not (jabber-chat--has-muc-invite-p stanza))))

(ert-deftest jabber-test-chat-has-muc-invite-muc-user-no-invite ()
  "Return nil when muc#user element exists but has no invite child."
  (let ((stanza '(message ((from . "room@conf.example.com"))
                  (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (status ((code . "110")))))))
    (should-not (jabber-chat--has-muc-invite-p stanza))))

;;; Group 4: jabber-chat-entry-time

(ert-deftest jabber-test-chat-entry-time-plist ()
  "Entry time from a msg-plist entry."
  (let* ((ts (encode-time '(0 30 14 15 1 2025 nil nil 0)))
         (entry (list :foreign (list :from "alice" :timestamp ts))))
    (should (equal ts (jabber-chat-entry-time entry)))))

(ert-deftest jabber-test-chat-entry-time-rare-time ()
  "Entry time from a :rare-time entry."
  (let* ((ts (encode-time '(0 0 12 10 3 2025 nil nil 0)))
         (entry (list :rare-time ts)))
    (should (equal ts (jabber-chat-entry-time entry)))))

(ert-deftest jabber-test-chat-entry-time-string-notice ()
  "Entry time from a string :muc-notice with :time in cddr."
  (let* ((ts (current-time))
         (entry (list :muc-notice "user enters the room" :time ts)))
    (should (equal ts (jabber-chat-entry-time entry)))))

(ert-deftest jabber-test-chat-entry-time-string-no-time ()
  "String entry without :time returns nil."
  (let ((entry (list :notice "some notice")))
    (should-not (jabber-chat-entry-time entry))))

;;; Group 5: jabber-chat--decrypt-if-needed

(ert-deftest jabber-test-chat-decrypt-if-needed-returns-xml-unchanged ()
  "No-op decryption returns xml-data unchanged."
  (let ((xml '(message ((from . "alice@example.com") (type . "chat"))
                       (body () "Hello!"))))
    (should (eq xml (jabber-chat--decrypt-if-needed nil xml)))))

(ert-deftest jabber-test-chat-decrypt-if-needed-preserves-complex-stanza ()
  "No-op decryption preserves a stanza with nested elements."
  (let ((xml '(message ((from . "bob@example.com"))
                       (body () "Encrypted?")
                       (x ((xmlns . "jabber:x:oob"))
                          (url () "https://example.com/file.png")))))
    (should (eq xml (jabber-chat--decrypt-if-needed nil xml)))))

;;; Group 6: jabber-chat--set-body

(ert-deftest jabber-test-chat-set-body-replaces-existing ()
  "set-body replaces existing <body> text."
  (let ((xml '(message ((from . "alice@example.com"))
                       (body () "old text"))))
    (jabber-chat--set-body xml "new text")
    (should (string= "new text"
                      (car (jabber-xml-node-children
                            (car (jabber-xml-get-children xml 'body))))))))

(ert-deftest jabber-test-chat-set-body-creates-missing ()
  "set-body appends <body> when none exists."
  (let ((xml '(message ((from . "alice@example.com")))))
    (jabber-chat--set-body xml "created")
    (let ((body-el (car (jabber-xml-get-children xml 'body))))
      (should body-el)
      (should (string= "created"
                        (car (jabber-xml-node-children body-el)))))))

;;; Group 7: decrypt handler dispatch

(ert-deftest jabber-test-chat-register-decrypt-handler-adds-entry ()
  "Register a handler, assert it appears in the alist."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 10 :error-label "Test")
    (should (assq 'test-handler jabber-chat-decrypt-handlers))))

(ert-deftest jabber-test-chat-unregister-decrypt-handler-removes-entry ()
  "Register then unregister, assert the alist is empty."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 10 :error-label "Test")
    (jabber-chat-unregister-decrypt-handler 'test-handler)
    (should-not jabber-chat-decrypt-handlers)))

(ert-deftest jabber-test-chat-register-decrypt-handler-replaces-existing ()
  "Register a handler twice, assert only one entry with new priority."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 10 :error-label "Test")
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 20 :error-label "Test")
    (should (= 1 (length jabber-chat-decrypt-handlers)))
    (should (= 20 (plist-get (cdr (assq 'test-handler
                                         jabber-chat-decrypt-handlers))
                              :priority)))))

(ert-deftest jabber-test-chat-decrypt-dispatches-to-matching-handler ()
  "Handler whose :detect matches gets its :decrypt called."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t)
        (called nil))
    (jabber-chat-register-decrypt-handler
     'test-handler
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc xml _parsed) (setq called t) xml)
     :priority 10
     :error-label "Test")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (jabber-chat--decrypt-if-needed nil xml)
      (should called))))

(ert-deftest jabber-test-chat-decrypt-skips-non-matching-handler ()
  "Handler whose :detect returns nil leaves xml-data unchanged."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t))
    (jabber-chat-register-decrypt-handler
     'test-handler
     :detect (lambda (_xml) nil)
     :decrypt (lambda (_jc _xml _parsed) (error "Should not be called"))
     :priority 10
     :error-label "Test")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (should (eq xml (jabber-chat--decrypt-if-needed nil xml))))))

(ert-deftest jabber-test-chat-decrypt-priority-order ()
  "Lower-priority handler runs first when both match."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t)
        (winner nil))
    (jabber-chat-register-decrypt-handler
     'handler-20
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc xml _parsed) (setq winner 20) xml)
     :priority 20
     :error-label "H20")
    (jabber-chat-register-decrypt-handler
     'handler-10
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc xml _parsed) (setq winner 10) xml)
     :priority 10
     :error-label "H10")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (jabber-chat--decrypt-if-needed nil xml)
      (should (= 10 winner)))))

(ert-deftest jabber-test-chat-decrypt-error-replaces-body ()
  "Handler that signals error gets body replaced with error label."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t))
    (jabber-chat-register-decrypt-handler
     'test-handler
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc _xml _parsed) (error "Decrypt boom"))
     :priority 10
     :error-label "BOOM")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "fallback"))))
      (jabber-chat--decrypt-if-needed nil xml)
      (should (string= "[BOOM: could not decrypt]"
                        (car (jabber-xml-node-children
                              (car (jabber-xml-get-children xml 'body)))))))))

(ert-deftest jabber-test-chat-decrypt-no-handlers-returns-unchanged ()
  "With empty handler alist, xml-data passes through."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t))
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (should (eq xml (jabber-chat--decrypt-if-needed nil xml))))))

(ert-deftest jabber-test-chat-decrypt-skips-nil-from ()
  "Stanza with no from attribute bypasses decrypt dispatch entirely."
  (let ((jabber-chat--crypto-loaded t)
        (called nil))
    (jabber-chat-register-decrypt-handler
     'test-nil-from
     :detect  (lambda (_xml) (setq called t) nil)
     :decrypt (lambda (_jc _xml _det) nil)
     :priority 1
     :error-label "test")
    (unwind-protect
        (let ((xml '(message () (body () "no from"))))
          (should (eq xml (jabber-chat--decrypt-if-needed nil xml)))
          (should-not called))
      (jabber-chat-unregister-decrypt-handler 'test-nil-from))))

;;; Group: decrypt dedup cache

(defun jabber-test-chat--encrypted-stanza
    (from id &optional origin-id ciphertext)
  "Build a fresh OMEMO-shaped encrypted stanza from FROM with ID.
Optional ORIGIN-ID adds a XEP-0359 <origin-id/> child.
CIPHERTEXT defaults to ID."
  (append
   (list 'message (list (cons 'from from) (cons 'id id))
         (list 'encrypted
               (list (cons 'xmlns "eu.siacs.conversations.axolotl"))
               (list 'payload nil (or ciphertext id))))
   (and origin-id
        (list (list 'origin-id
                    (list (cons 'xmlns "urn:xmpp:sid:0")
                          (cons 'id origin-id)))))))

(defun jabber-test-chat--body-text (xml-data)
  "Return the body text of XML-DATA, or nil."
  (car (jabber-xml-node-children
        (car (jabber-xml-get-children xml-data 'body)))))

(defmacro jabber-test-chat--with-decrypt-cache (&rest body)
  "Run BODY with fresh decrypt handler and dedup cache state.
Stubs `jabber-connection-bare-jid' to a fixed account."
  (declare (indent 0) (debug t))
  `(let ((jabber-chat-decrypt-handlers nil)
         (jabber-chat--sorted-decrypt-handlers-cache nil)
         (jabber-chat--crypto-loaded t)
         (jabber-chat--decrypt-cache (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                (lambda (_jc) "me@x.com")))
       ,@body)))

(ert-deftest jabber-test-chat-decrypt-dedup-serves-repeat-from-cache ()
  "A second delivery of the same encrypted stanza skips the handler."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret text"))
       :priority 10
       :error-label "OMEMO")
      (let ((first (jabber-chat--decrypt-if-needed
                    nil (jabber-test-chat--encrypted-stanza
                         "alice@x.com/phone" "msg-1")))
            (second (jabber-chat--decrypt-if-needed
                     nil (jabber-test-chat--encrypted-stanza
                          "alice@x.com/phone" "msg-1"))))
        (should (= 1 runs))
        (should (string= "secret text" (jabber-test-chat--body-text first)))
        (should (string= "secret text" (jabber-test-chat--body-text second)))))))

(ert-deftest jabber-test-chat-decrypt-dedup-keeps-behavioral-message-id ()
  "Different ciphertexts decrypt even when their origin-id matches."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret text"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza
            "alice@x.com/phone" "id-a" "origin-1"))
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza
            "alice@x.com/phone" "id-b" "origin-1"))
      (should (= 2 runs)))))

(ert-deftest jabber-test-chat-decrypt-dedup-rejects-changed-message-id ()
  "A changed raw id cannot run or reuse the same ciphertext."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret text"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza
            "alice@x.com/phone" "id-a" nil "same-ciphertext"))
      (let ((second
             (jabber-chat--decrypt-if-needed
              nil (jabber-test-chat--encrypted-stanza
                   "alice@x.com/phone" "id-b" nil "same-ciphertext"))))
        (should (= 1 runs))
        (should (string= "[OMEMO: could not decrypt]"
                         (jabber-test-chat--body-text second)))))))

(ert-deftest jabber-test-chat-decrypt-dedup-binds-replace-target ()
  "Changed correction metadata cannot replay the same plaintext."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0)
          second)
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "corrected"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed
       nil '(message ((from . "alice@x.com/phone")
                      (id . "correction-1"))
                     (body () "fallback")
                     (encrypted
                      ((xmlns . "eu.siacs.conversations.axolotl"))
                      (payload () "same-ciphertext"))
                     (replace ((xmlns . "urn:xmpp:message-correct:0")
                               (id . "original-1")))))
      (setq second
            (jabber-chat--decrypt-if-needed
             nil '(message ((from . "alice@x.com/phone")
                            (id . "correction-1"))
                           (body () "fallback")
                           (encrypted
                            ((xmlns . "eu.siacs.conversations.axolotl"))
                            (payload () "same-ciphertext"))
                           (replace
                            ((xmlns . "urn:xmpp:message-correct:0")
                             (id . "original-2"))))))
      (should (= 1 runs))
      (should (string= "[OMEMO: could not decrypt]"
                       (jabber-test-chat--body-text second))))))

(ert-deftest jabber-test-chat-decrypt-dedup-binds-inner-delay ()
  "A changed delay wrapper cannot run the same ciphertext twice."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0)
          (base '(message ((from . "room@conference.x/alice")
                           (type . "groupchat")
                           (id . "correction-1"))
                          (body () "fallback")
                          (encrypted
                           ((xmlns . "eu.siacs.conversations.axolotl"))
                           (payload () "same-ciphertext"))
                          (replace ((xmlns . "urn:xmpp:message-correct:0")
                                    (id . "original-1"))))))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "corrected"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed nil (copy-tree base))
      (let ((second
             (jabber-chat--decrypt-if-needed
              nil (append (copy-tree base)
                          '((delay ((xmlns . "urn:xmpp:delay")
                                    (stamp . "2026-07-26T10:00:00Z"))))))))
        (should (= 1 runs))
        (should (string= "[OMEMO: could not decrypt]"
                         (jabber-test-chat--body-text second)))))))

(ert-deftest jabber-test-chat-decrypt-dedup-normalizes-muc-mam-item ()
  "MUC live and MAM forms differing only in archive item metadata dedup."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0)
          (live '(message ((from . "room@conference.x/alice")
                           (to . "me@x.com/resource")
                           (type . "groupchat")
                           (id . "message-1"))
                          (body () "fallback")
                          (encrypted
                           ((xmlns . "eu.siacs.conversations.axolotl"))
                           (payload () "ciphertext"))))
          (archived
           '(message ((from . "room@conference.x/alice")
                      (type . "groupchat")
                      (id . "message-1"))
                     (body () "fallback")
                     (encrypted
                      ((xmlns . "eu.siacs.conversations.axolotl"))
                      (payload () "ciphertext"))
                     (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                        (item ((jid . "alice@example.com")))))))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed nil (copy-tree live))
      (jabber-chat--decrypt-if-needed nil (copy-tree archived))
      (should (= 1 runs)))))

(ert-deftest jabber-test-chat-decrypt-dedup-retains-muc-invite ()
  "Changed MUC invitation metadata rejects repeated ciphertext."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret"))
       :priority 10
       :error-label "OMEMO")
      (dolist (reason '("first" "second"))
        (jabber-chat--decrypt-if-needed
         nil `(message ((from . "room@conference.x")
                        (type . "normal")
                        (id . "invite-1"))
                       (encrypted
                        ((xmlns . "eu.siacs.conversations.axolotl"))
                        (payload () "same-ciphertext"))
                       (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                          (invite ((from . "alice@example.com"))
                                  (reason () ,reason))))))
      (should (= 1 runs)))))

(ert-deftest jabber-test-chat-decrypt-dedup-normalizes-attribute-order ()
  "Attribute order does not change ciphertext or wrapper identity."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed
       nil '(message ((from . "alice@x.com/phone") (id . "one"))
                     (encrypted
                      ((xmlns . "eu.siacs.conversations.axolotl")
                       (test . "yes"))
                      (payload ((b . "2") (a . "1")) "cipher"))))
      (let ((second
             (jabber-chat--decrypt-if-needed
              nil '(message ((id . "one") (from . "alice@x.com/phone"))
                            (encrypted
                             ((test . "yes")
                              (xmlns . "eu.siacs.conversations.axolotl"))
                             (payload ((a . "1") (b . "2")) "cipher"))))))
        (should (= 1 runs))
        (should (string= "secret" (jabber-test-chat--body-text second)))))))

(ert-deftest jabber-test-chat-decrypt-dedup-no-cross-sender-collision ()
  "Two senders using the same stanza id are decrypted independently."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret text"))
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza "alice@x.com/phone" "1"))
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza "bob@x.com/laptop" "1"))
      (should (= 2 runs)))))

(ert-deftest jabber-test-chat-decrypt-dedup-caches-bodyless-outcome ()
  "A successful decrypt with no body (heartbeat) is not re-decrypted."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed) (cl-incf runs) xml)
       :priority 10
       :error-label "OMEMO")
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza "alice@x.com/phone" "hb-1"))
      (let ((second (jabber-chat--decrypt-if-needed
                     nil (jabber-test-chat--encrypted-stanza
                          "alice@x.com/phone" "hb-1"))))
        (should (= 1 runs))
        (should-not (jabber-test-chat--body-text second))))))

(ert-deftest jabber-test-chat-decrypt-dedup-does-not-cache-failures ()
  "A failed decrypt stays retryable on the next delivery."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (if (= runs 1)
                      (error "Ratchet failure")
                    (jabber-chat--set-body xml "recovered text")))
       :priority 10
       :error-label "OMEMO")
      (let ((first (jabber-chat--decrypt-if-needed
                    nil (jabber-test-chat--encrypted-stanza
                         "alice@x.com/phone" "msg-2")))
            (second (jabber-chat--decrypt-if-needed
                     nil (jabber-test-chat--encrypted-stanza
                          "alice@x.com/phone" "msg-2"))))
        (should (= 2 runs))
        (should (string= "[OMEMO: could not decrypt]"
                         (jabber-test-chat--body-text first)))
        (should (string= "recovered text"
                         (jabber-test-chat--body-text second)))))))

(ert-deftest jabber-test-chat-decrypt-dedup-caches-post-ratchet-failure ()
  "A corrupt payload cannot send consumed ciphertext through the ratchet twice."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc _xml _parsed)
                  (cl-incf runs)
                  (setq jabber-chat--decrypt-consumed-p t)
                  (error "Payload authentication failed"))
       :priority 10
       :error-label "OMEMO")
      (let ((first (jabber-chat--decrypt-if-needed
                    nil (jabber-test-chat--encrypted-stanza
                         "alice@x.com/phone" "corrupt")))
            (second (jabber-chat--decrypt-if-needed
                     nil (jabber-test-chat--encrypted-stanza
                          "alice@x.com/phone" "corrupt"))))
        (should (= 1 runs))
        (should (string= "[OMEMO: could not decrypt]"
                         (jabber-test-chat--body-text first)))
        (should (string= "[OMEMO: could not decrypt]"
                         (jabber-test-chat--body-text second)))))))

(ert-deftest jabber-test-chat-decrypt-dedup-retains-old-ciphertext ()
  "Old ciphertext remains blocked after many later messages."
  (jabber-test-chat--with-decrypt-cache
    (let ((runs 0))
      (jabber-chat-register-decrypt-handler
       'test-omemo
       :detect (lambda (xml) (jabber-xml-child-with-xmlns
                              xml "eu.siacs.conversations.axolotl"))
       :decrypt (lambda (_jc xml _parsed)
                  (cl-incf runs)
                  (jabber-chat--set-body xml "secret text"))
       :priority 10
       :error-label "OMEMO")
      (dotimes (i 513)
        (jabber-chat--decrypt-if-needed
         nil (jabber-test-chat--encrypted-stanza
              "alice@x.com/phone" (format "e-%d" i))))
      (jabber-chat--decrypt-if-needed
       nil (jabber-test-chat--encrypted-stanza "alice@x.com/phone" "e-0"))
      (should (= 513 runs)))))

;;; Group 8: jabber-chat-goto-address error handling

(ert-deftest jabber-test-chat-goto-address-logs-error-on-failure ()
  "goto-address error is logged via message, not silently swallowed."
  (let ((logged-messages nil))
    (cl-letf (((symbol-function 'goto-address-fontify)
               (lambda (&rest _) (error "Test fontify error")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) logged-messages))))
      (with-temp-buffer
        (insert "https://example.com some text")
        (jabber-chat-goto-address nil nil :insert)
        (should (cl-some
                 (lambda (m)
                   (string-match-p "goto-address-fontify failed" m))
                 logged-messages))))))

(ert-deftest jabber-test-chat-goto-address-succeeds-normally ()
  "goto-address runs without error when fontify succeeds."
  (with-temp-buffer
    (insert "Visit https://example.com today")
    ;; Should not signal an error
    (jabber-chat-goto-address nil nil :insert)))

(ert-deftest jabber-test-chat-goto-address-skips-non-insert-mode ()
  "goto-address does nothing when mode is not :insert."
  (let ((called nil))
    (cl-letf (((symbol-function 'goto-address-fontify)
               (lambda (&rest _) (setq called t))))
      (with-temp-buffer
        (insert "https://example.com")
        (jabber-chat-goto-address nil nil :printp)
        (should-not called)))))

;;; Group 9: jabber-chat-muc-presence-patterns-history variable

(ert-deftest jabber-test-chat-muc-presence-patterns-history-exists ()
  "The correctly-named history variable exists and is nil by default."
  (should (boundp 'jabber-chat-muc-presence-patterns-history))
  ;; The old typo should not exist
  (should-not (boundp 'jaber-chat-much-presence-patterns-history)))

;;; Group 10: inline image resizing

(defmacro jabber-test-chat-with-inline-image (&rest body)
  "Run BODY in a temp buffer containing one inline image URL."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((url "https://example.com/image.png")
            (image (list 'image :type 'png :max-width 300 :max-height 200)))
       (insert url)
       (cl-letf (((symbol-function 'jabber-chat--schedule-image-recenter)
                  #'ignore))
         (jabber-chat--apply-image-display image (point-min) (point-max) url)
         (put-text-property (point-min) (point-max) 'read-only t)
         (goto-char (point-min))
         ,@body))))

(ert-deftest jabber-test-chat-image-range-at-point-finds-display ()
  "Inline image range lookup returns URL, base image, and scale."
  (jabber-test-chat-with-inline-image
    (let ((range (jabber-chat--image-range-at-point)))
      (should (= (point-min) (plist-get range :beg)))
      (should (= (point-max) (plist-get range :end)))
      (should (equal url (plist-get range :url)))
      (should (eq image (plist-get range :image)))
      (should (= 1.0 (plist-get range :scale))))))

(ert-deftest jabber-test-chat-image-range-at-point-supports-loaded-image ()
  "Inline image range lookup supports images rendered before reload."
  (jabber-test-chat-with-inline-image
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max)
                              '(jabber-chat-image-base nil
                                jabber-chat-image-scale nil)))
    (let ((range (jabber-chat--image-range-at-point)))
      (should (equal url (plist-get range :url)))
      (should (eq (get-text-property (point) 'display)
                  (plist-get range :image)))
      (should (= 1.0 (plist-get range :scale))))))

(ert-deftest jabber-test-chat-image-enlarge-shrink-and-reset ()
  "Image resize commands update range-local scale."
  (jabber-test-chat-with-inline-image
    (cl-letf (((symbol-function 'message) #'ignore))
      (jabber-chat-image-enlarge)
      (should (= jabber-chat--image-scale-step
                 (get-text-property (point) 'jabber-chat-image-scale)))
      (jabber-chat-image-shrink)
      (should (= 1.0 (get-text-property (point) 'jabber-chat-image-scale)))
      (jabber-chat-image-shrink)
      (should (< (get-text-property (point) 'jabber-chat-image-scale) 1.0))
      (jabber-chat-image-reset-size)
      (should (= 1.0 (get-text-property (point) 'jabber-chat-image-scale))))))

(ert-deftest jabber-test-chat-inline-image-keys-active-at-point ()
  "Inline image resize keys are active through text properties."
  (jabber-test-chat-with-inline-image
    (should (eq (key-binding (kbd "+") nil nil (point))
                #'jabber-chat-image-enlarge))
    (should (eq (key-binding (kbd "=") nil nil (point))
                #'jabber-chat-image-enlarge))
    (should (eq (key-binding (kbd "-") nil nil (point))
                #'jabber-chat-image-shrink))
    (should (eq (key-binding (kbd "0") nil nil (point))
                #'jabber-chat-image-reset-size))))

(defun jabber-test-chat--dispatch-key (key)
  "Dispatch KEY using the active keymaps at point."
  (let ((command (key-binding (kbd key) nil nil (point)))
        (last-command-event (string-to-char key)))
    (call-interactively command)))

(ert-deftest jabber-test-chat-minus-key-shrinks-inline-image-via-command-loop ()
  "Pressing - in `jabber-chat-mode' shrinks images via normal dispatch."
  (jabber-test-chat-with-inline-image
    (jabber-chat-mode)
    (cl-letf (((symbol-function 'message) #'ignore))
      (jabber-test-chat--dispatch-key "-")
      (should (< (get-text-property (point) 'jabber-chat-image-scale) 1.0))
      (should (string= url (buffer-substring-no-properties
                            (point-min) (point-max)))))))

(ert-deftest jabber-test-chat-mode-map-enlarge-and-zero-resize-inline-image ()
  "The mode-map +, =, and 0 bindings dispatch to inline image resizing."
  (jabber-test-chat-with-inline-image
    (jabber-chat-mode)
    (cl-letf (((symbol-function 'message) #'ignore))
      (jabber-test-chat--dispatch-key "+")
      (should (= jabber-chat--image-scale-step
                 (get-text-property (point) 'jabber-chat-image-scale)))
      (jabber-test-chat--dispatch-key "0")
      (should (= 1.0 (get-text-property (point) 'jabber-chat-image-scale)))
      (jabber-test-chat--dispatch-key "=")
      (should (= jabber-chat--image-scale-step
                 (get-text-property (point) 'jabber-chat-image-scale))))))

(ert-deftest jabber-test-chat-mode-map-resize-keys-self-insert-off-image ()
  "The mode-map resize keys self-insert outside inline images."
  (with-temp-buffer
    (jabber-chat-mode)
    (dolist (key '("-" "+" "=" "0"))
      (jabber-test-chat--dispatch-key key))
    (should (string= "-+=0" (buffer-string)))
    (let ((current-prefix-arg 3))
      (jabber-test-chat--dispatch-key "-"))
    (should (string= "-+=0---" (buffer-string)))))

(ert-deftest jabber-test-chat-scaled-image-does-not-mutate-base ()
  "Scaling copies the image object instead of mutating the cached image."
  (let* ((image (list 'image :type 'png :max-width 300 :max-height 200))
         (scaled (jabber-chat--scaled-image image 2.0)))
    (should (not (eq image scaled)))
    (should (= 300 (image-property image :max-width)))
    (should (= 200 (image-property image :max-height)))
    (should (= 600 (image-property scaled :max-width)))
    (should (= 400 (image-property scaled :max-height)))))

;;; Group: Backlog message identity

(defmacro jabber-test-chat--with-backlog-ewoc (&rest body)
  "Run BODY with isolated backlog EWOC state."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test #'equal))
           (jabber-print-rare-time nil)
           (jabber-group "room@conference.example.com"))
       (cl-letf (((symbol-function 'jabber-muc-our-nick-p)
                  (lambda (&rest _) nil)))
         ,@body))))

(ert-deftest jabber-test-chat-backlog-scopes-colliding-muc-client-ids ()
  "Backlog messages from different occupants may share a client id."
  (jabber-test-chat--with-backlog-ewoc
    (dolist (from '("room@conference.example.com/alice"
                    "room@conference.example.com/bob"))
      (jabber-chat-insert-backlog-entry
       (list :id "same" :from from :direction "in"
             :msg-type "groupchat" :timestamp (current-time))))
    (should (ewoc-nth jabber-chat-ewoc 1))
    (should (gethash
             '(:muc "room@conference.example.com/alice" "same")
             jabber-chat--msg-nodes))
    (should (gethash
             '(:muc "room@conference.example.com/bob" "same")
             jabber-chat--msg-nodes))))

(ert-deftest jabber-test-chat-backlog-then-live-dedups-server-id ()
  "A live replay does not duplicate a backlog server identity."
  (jabber-test-chat--with-backlog-ewoc
    (let ((msg (list :id "client" :server-id "server"
                     :from "room@conference.example.com/alice"
                     :direction "in" :msg-type "groupchat"
                     :timestamp (current-time))))
      (jabber-chat-insert-backlog-entry msg)
      (should-not (jabber-chat-ewoc-enter (list :muc-foreign msg)))
      (should-not (ewoc-nth jabber-chat-ewoc 1)))))

(ert-deftest jabber-test-chat-live-then-backlog-dedups-server-id ()
  "A backlog replay does not duplicate a live server identity."
  (jabber-test-chat--with-backlog-ewoc
    (let ((msg (list :id "client" :server-id "server"
                     :from "room@conference.example.com/alice"
                     :direction "in" :msg-type "groupchat"
                     :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg))
      (jabber-chat-insert-backlog-entry msg)
      (should-not (ewoc-nth jabber-chat-ewoc 1)))))

;;; Group 11: jabber-chat-create-buffer

(ert-deftest jabber-test-chat-create-buffer-notifies-mam-on-create-and-reopen ()
  "Creating and reopening a chat buffer each notify MAM once."
  (let* ((jc1 (jabber-test-chat--make-fake-jc "me@example.com"))
         (jc2 (jabber-test-chat--make-fake-jc "me@example.com"))
         (peer "emma@example.com/laptop")
         (bare-peer "emma@example.com")
         (jabber-chat-buffer-format " *jabber-test-chat-%j-%a*")
         (calls nil)
         buf)
    (cl-letf (((symbol-function 'jabber-db-backlog)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-db-get-chat-encryption)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-mam-chat-opened)
               (lambda (jc peer)
                 (push (cons jc peer) calls))))
      (unwind-protect
          (progn
            (setq buf (jabber-chat-create-buffer jc1 peer))
            (should (= 1 (length calls)))
            (should (equal (list (cons jc1 bare-peer)) calls))
            (should (eq buf (jabber-chat-create-buffer jc2 peer)))
            (should (= 2 (length calls)))
            (should (equal (list (cons jc2 bare-peer)
                                 (cons jc1 bare-peer))
                           calls))
            (with-current-buffer buf
              (should (eq jc2 jabber-buffer-connection))))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;;; Group: reaction rendering

(ert-deftest jabber-test-chat-reaction-entry-string-carries-help-echo ()
  "Rendered reaction text exposes who reacted via `help-echo'."
  (let* ((entry (jabber-reactions--display-entry
                 "👍" '("alice@example.com" "bob@example.com") nil))
         (text (jabber-chat--reaction-entry-string entry)))
    (should (string= text "2👍"))
    (should (equal (get-text-property 0 'help-echo text)
                   "👍: alice@example.com, bob@example.com"))))

;;; Group 12: error stanza collapse

(defun jabber-test-chat--error-buffer (jc peer)
  "Create a real chat buffer for PEER with DB and MAM stubbed out.
JC is a fake connection from `jabber-test-chat--make-fake-jc'."
  (cl-letf (((symbol-function 'jabber-db-backlog) (lambda (&rest _) nil))
            ((symbol-function 'jabber-db-get-chat-encryption)
             (lambda (&rest _) nil))
            ((symbol-function 'jabber-mam-chat-opened) #'ignore))
    (jabber-chat-create-buffer jc peer)))

(defun jabber-test-chat--error-nodes (buffer)
  "Return the list of :error ewoc data entries in BUFFER."
  (with-current-buffer buffer
    (ewoc-collect jabber-chat-ewoc (lambda (data) (eq (car data) :error)))))

(defun jabber-test-chat--make-error (peer text id)
  "Build an error message plist from PEER, TEXT and ID."
  (list :from peer :error-text text :id id :timestamp (current-time)))

(ert-deftest jabber-test-chat-error-collapse-counts-repeats ()
  "Repeated identical errors collapse into one counted node."
  (let* ((jc (jabber-test-chat--make-fake-jc "me@example.com"))
         (peer "bridge@example.com/x")
         (jabber-chat-buffer-format " *jabber-test-chat-%j-%a*")
         buf)
    (unwind-protect
        (progn
          (setq buf (jabber-test-chat--error-buffer jc peer))
          (with-current-buffer buf
            (dolist (id '("e1" "e2" "e3"))
              (jabber-chat--enter-error-collapsed
               (jabber-test-chat--make-error peer "Recipient unavailable" id))))
          (let ((nodes (jabber-test-chat--error-nodes buf)))
            (should (= 1 (length nodes)))
            (should (= 3 (plist-get (cadr (car nodes)) :count))))
          (should (string-search
                   "Error: Recipient unavailable (×3)"
                   (with-current-buffer buf (buffer-string)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest jabber-test-chat-error-collapse-distinct-text-new-node ()
  "A different error text after the first produces a second node."
  (let* ((jc (jabber-test-chat--make-fake-jc "me@example.com"))
         (peer "bridge@example.com/x")
         (jabber-chat-buffer-format " *jabber-test-chat-%j-%a*")
         buf)
    (unwind-protect
        (progn
          (setq buf (jabber-test-chat--error-buffer jc peer))
          (with-current-buffer buf
            (jabber-chat--enter-error-collapsed
             (jabber-test-chat--make-error peer "Recipient unavailable" "e1"))
            (jabber-chat--enter-error-collapsed
             (jabber-test-chat--make-error peer "Service unavailable" "e2")))
          (should (= 2 (length (jabber-test-chat--error-nodes buf)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest jabber-test-chat-find-buffer-nil-when-absent ()
  "`jabber-chat--find-buffer' returns nil when no buffer exists."
  (cl-letf (((symbol-function 'jabber-muc-sender-p) #'ignore))
    (should-not (jabber-chat--find-buffer "nobody@example.com/x"))))

;;; Group 13: aesgcm image policy

(ert-deftest jabber-test-chat-aesgcm-image-size-cap-blocks-decrypt ()
  "Oversized ciphertext is rejected before decryption runs."
  (let ((decrypted nil))
    (cl-letf (((symbol-function 'jabber-omemo-aesgcm-decrypt)
               (lambda (&rest _) (setq decrypted t) "plain")))
      (let ((jabber-image-max-bytes 4))
        (should-not (jabber-chat--aesgcm-image-from-body
                     "too big ciphertext" "key" "iv" nil))
        (should-not decrypted)))))

(ert-deftest jabber-test-chat-aesgcm-image-threads-allowed-types ()
  (cl-letf (((symbol-function 'jabber-omemo-aesgcm-decrypt)
             (lambda (&rest _) "plaintext"))
            ((symbol-function 'jabber-image--result-from-data)
             (lambda (data types) (list :image (list data types)))))
    (let ((jabber-image-max-bytes nil))
      (should (equal (jabber-chat--aesgcm-image-from-body
                      "ct" "key" "iv" '(png))
                     '("plaintext" (png)))))))

(ert-deftest jabber-test-chat-aesgcm-decode-failure-retains-plaintext ()
  "An unsupported decrypted payload remains available for manual saving."
  (cl-letf (((symbol-function 'jabber-omemo-aesgcm-decrypt)
             (lambda (&rest _) "decrypted-heic"))
            ((symbol-function 'jabber-image--result-from-data)
             (lambda (data _types) (list :error 'decode :data data))))
    (let ((result (jabber-chat--aesgcm-image-result-from-body
                   "ciphertext" "key" "iv" nil)))
      (should (eq (plist-get result :error) 'decode))
      (should (equal (plist-get result :data) "decrypted-heic")))))

(ert-deftest jabber-test-chat-aesgcm-image-nil-body-returns-nil ()
  (should-not (jabber-chat--aesgcm-image-from-body nil "key" "iv" nil)))

;;; Group 14: image display policy

(defun jabber-test-chat--make-jc-with-roster (&rest jids)
  "Create a fake connection whose roster contains JIDS."
  (let ((jc (gensym "jabber-test-chat-jc-")))
    (put jc :state-data (list :roster (mapcar #'jabber-jid-symbol jids)))
    jc))

(defmacro jabber-test-chat--with-policy-buffer (peer &rest body)
  "Run BODY in a temp buffer chatting with PEER (nil for a MUC).
The fake connection has alice@example.com on its roster."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local jabber-buffer-connection
                 (jabber-test-chat--make-jc-with-roster "alice@example.com"))
     (let ((peer ,peer))
       (when peer
         (setq-local jabber-chatting-with peer)))
     ,@body))

(ert-deftest jabber-test-chat-auto-display-t-always ()
  (jabber-test-chat--with-policy-buffer nil
    (let ((jabber-chat-display-images t))
      (should (jabber-chat--auto-display-images-p)))))

(ert-deftest jabber-test-chat-auto-display-legacy-non-nil-value ()
  "Any non-nil value other than `roster' behaves like t."
  (jabber-test-chat--with-policy-buffer nil
    (let ((jabber-chat-display-images 'always))
      (should (jabber-chat--auto-display-images-p)))))

(ert-deftest jabber-test-chat-auto-display-nil-never ()
  (jabber-test-chat--with-policy-buffer "alice@example.com"
    (let ((jabber-chat-display-images nil))
      (should-not (jabber-chat--auto-display-images-p)))))

(ert-deftest jabber-test-chat-auto-display-roster-contact ()
  (jabber-test-chat--with-policy-buffer "alice@example.com"
    (let ((jabber-chat-display-images 'roster))
      (should (jabber-chat--auto-display-images-p)))))

(ert-deftest jabber-test-chat-auto-display-roster-full-jid ()
  (jabber-test-chat--with-policy-buffer "alice@example.com/laptop"
    (let ((jabber-chat-display-images 'roster))
      (should (jabber-chat--auto-display-images-p)))))

(ert-deftest jabber-test-chat-auto-display-roster-stranger ()
  (jabber-test-chat--with-policy-buffer "mallory@example.com"
    (let ((jabber-chat-display-images 'roster))
      (should-not (jabber-chat--auto-display-images-p)))))

(ert-deftest jabber-test-chat-auto-display-roster-muc ()
  "MUC buffers have no `jabber-chatting-with' and never auto-display."
  (jabber-test-chat--with-policy-buffer nil
    (let ((jabber-chat-display-images 'roster))
      (setq-local jabber-group "room@conf.example.com")
      (should-not (jabber-chat--auto-display-images-p)))))

;;; Group 15: image URL scan behavior

(defconst jabber-test-chat--scan-url "https://example.com/pic.png")

(defmacro jabber-test-chat--with-scan-buffer (&rest body)
  "Run BODY in a temp buffer containing one image URL.
Bind `fetches' to the recorded `jabber-chat--start-image-fetch'
calls and `url', `beg' and `end' to the URL and its bounds."
  `(with-temp-buffer
     (let ((fetches nil)
           (url jabber-test-chat--scan-url))
       (insert url)
       (let ((beg (point-min))
             (end (point-max)))
         (cl-letf (((symbol-function 'jabber-chat--start-image-fetch)
                    (lambda (&rest args) (push args fetches))))
           ,@body)))))

(ert-deftest jabber-test-chat-scan-auto-fetches-with-allowlist ()
  (jabber-test-chat--with-scan-buffer
   (jabber-chat--scan-image-url url beg end t)
   (should (equal fetches
                  (list (list url beg end jabber-chat-image-auto-types))))))

(ert-deftest jabber-test-chat-scan-no-auto-still-clickable ()
  "Without auto-display the URL is not fetched but stays actionable."
  (jabber-test-chat--with-scan-buffer
   (jabber-chat--scan-image-url url beg end nil)
   (should (null fetches))
   (should (equal (get-text-property beg 'jabber-chat-image-url) url))
   (should (eq (get-text-property beg 'keymap) jabber-chat-url-keymap))))

(ert-deftest jabber-test-chat-scan-skips-failed-fetch ()
  (jabber-test-chat--with-scan-buffer
   (put-text-property beg end 'jabber-chat-image-fetching 'failed)
   (jabber-chat--scan-image-url url beg end t)
   (should (null fetches))))

(ert-deftest jabber-test-chat-scan-skips-in-flight-fetch ()
  (jabber-test-chat--with-scan-buffer
   (put-text-property beg end 'jabber-chat-image-fetching url)
   (jabber-chat--scan-image-url url beg end t)
   (should (null fetches))))

(ert-deftest jabber-test-chat-scan-restores-cached-despite-policy ()
  "A cached image is displayed even when auto-display is off."
  (jabber-test-chat--with-scan-buffer
   (unwind-protect
       (progn
         (jabber-chat--cache-image url '(image :type png))
         (jabber-chat--scan-image-url url beg end nil)
         (should (null fetches))
         (should (get-text-property beg 'display)))
     (remhash url jabber-chat--image-cache))))

(ert-deftest jabber-test-chat-failed-fetch-marks-url ()
  "A nil image from the fetcher marks the URL range as failed."
  (with-temp-buffer
    (insert jabber-test-chat--scan-url)
    (let ((beg (copy-marker (point-min)))
          (end (copy-marker (point-max))))
      (jabber-chat--replace-url-with-image
       nil jabber-test-chat--scan-url beg end (current-buffer))
      (should (eq (jabber-chat--image-fetch-state (point-min)) 'failed)))))

(ert-deftest jabber-test-chat-isolate-image-url-inserts-newline ()
  (with-temp-buffer
    (insert "text https://example.com/pic.png")
    (let* ((end (point-max))
           (bounds (jabber-chat--isolate-image-url 6 end)))
      (should (equal (cons 7 (1+ end)) bounds))
      (should (eq (char-before (car bounds)) ?\n)))))

(ert-deftest jabber-test-chat-isolate-image-url-already-alone ()
  (with-temp-buffer
    (insert "https://example.com/pic.png")
    (should (equal (cons 1 (point-max))
                   (jabber-chat--isolate-image-url 1 (point-max))))))

;;; Group 16: manual image load with RET

(ert-deftest jabber-test-chat-image-url-bounds-at-point ()
  (with-temp-buffer
    (insert "x")
    (insert (propertize jabber-test-chat--scan-url
                        'jabber-chat-image-url jabber-test-chat--scan-url))
    (goto-char 3)
    (should (equal (jabber-chat--image-url-bounds)
                   (list 2 (point-max) jabber-test-chat--scan-url)))))

(ert-deftest jabber-test-chat-image-url-bounds-nil-without-property ()
  (with-temp-buffer
    (insert "no url here")
    (goto-char (point-min))
    (should-not (jabber-chat--image-url-bounds))))

(defmacro jabber-test-chat--with-manual-load-buffer (&rest body)
  "Run BODY in a temp buffer with point on an undisplayed image URL.
Bind `fetches' to recorded `jabber-chat--start-image-fetch' calls
and `url' to the URL; `display-graphic-p' is stubbed to t."
  `(with-temp-buffer
     (let ((fetches nil)
           (url jabber-test-chat--scan-url))
       (insert (propertize url 'jabber-chat-image-url url))
       (goto-char (point-min))
       (cl-letf (((symbol-function 'jabber-chat--start-image-fetch)
                  (lambda (&rest args) (push args fetches)))
                 ((symbol-function 'display-graphic-p)
                  (lambda (&optional _) t)))
         ,@body))))

(ert-deftest jabber-test-chat-manual-load-bypasses-allowlist ()
  "Manual load passes nil ALLOWED-TYPES to the fetch."
  (jabber-test-chat--with-manual-load-buffer
   (jabber-chat--load-image-at-point)
   (should (equal fetches (list (list url 1 (point-max) nil t))))))

(ert-deftest jabber-test-chat-manual-load-blocked-while-in-flight ()
  (jabber-test-chat--with-manual-load-buffer
   (put-text-property 1 (point-max) 'jabber-chat-image-fetching url)
   (jabber-chat--load-image-at-point)
   (should (null fetches))))

(ert-deftest jabber-test-chat-manual-load-retries-after-failure ()
  (jabber-test-chat--with-manual-load-buffer
   (put-text-property 1 (point-max) 'jabber-chat-image-fetching 'failed)
   (jabber-chat--load-image-at-point)
   (should (= 1 (length fetches)))))

(ert-deftest jabber-test-chat-manual-load-uses-cache ()
  (jabber-test-chat--with-manual-load-buffer
   (unwind-protect
       (progn
         (jabber-chat--cache-image url '(image :type png))
         (jabber-chat--load-image-at-point)
         (should (null fetches))
         (should (get-text-property 1 'display)))
     (remhash url jabber-chat--image-cache))))

(ert-deftest jabber-test-chat-ret-loads-undisplayed-image ()
  (jabber-test-chat--with-manual-load-buffer
   (cl-letf (((symbol-function 'jabber-chat-download-url)
              (lambda (_) (error "Should not download"))))
     (jabber-chat-url-action-at-point)
     (should (= 1 (length fetches))))))

(ert-deftest jabber-test-chat-ret-downloads-displayed-image ()
  (jabber-test-chat--with-manual-load-buffer
   (let ((downloads nil))
     (cl-letf (((symbol-function 'jabber-chat-download-url)
                (lambda (u) (push u downloads))))
       (put-text-property 1 (point-max) 'display '(image :type png))
       (jabber-chat-url-action-at-point)
       (should (equal downloads (list url)))
       (should (null fetches))))))

(ert-deftest jabber-test-chat-ret-prefix-downloads-undisplayed ()
  (jabber-test-chat--with-manual-load-buffer
   (let ((downloads nil))
     (cl-letf (((symbol-function 'jabber-chat-download-url)
                (lambda (u) (push u downloads))))
       (jabber-chat-url-action-at-point '(4))
       (should (equal downloads (list url)))
       (should (null fetches))))))

(ert-deftest jabber-test-chat-ret-prefers-file-url ()
  (jabber-test-chat--with-manual-load-buffer
   (let ((downloads nil))
     (cl-letf (((symbol-function 'jabber-chat-download-url)
                (lambda (u) (push u downloads))))
       (put-text-property 1 (point-max) 'jabber-chat-file-url
                          "https://example.com/doc.pdf")
       (jabber-chat-url-action-at-point)
       (should (equal downloads '("https://example.com/doc.pdf")))
       (should (null fetches))))))

(ert-deftest jabber-test-chat-manual-load-tty-errors ()
  "Batch Emacs is not graphical, so the tty branch errors."
  (with-temp-buffer
    (let ((url jabber-test-chat--scan-url))
      (insert (propertize url 'jabber-chat-image-url url))
      (goto-char (point-min))
      (should-error (jabber-chat--load-image-at-point)
                    :type 'user-error))))

(ert-deftest jabber-test-chat-ret-errors-without-url ()
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (should-error (jabber-chat-url-action-at-point) :type 'user-error)))

(ert-deftest jabber-test-chat-manual-decode-failure-offers-decrypted-save ()
  "Manual aesgcm preview failure offers its decrypted bytes for saving."
  (with-temp-buffer
    (let* ((url (concat "aesgcm://example.org/photo.jpg#"
                        (make-string 88 ?a)))
           (saved nil))
      (insert (propertize url 'jabber-chat-image-url url))
      (cl-letf (((symbol-function 'jabber-chat--offer-image-save)
                 (lambda (save-url data)
                   (setq saved (list save-url data)))))
        (jabber-chat--handle-image-result
         (list :error 'decode :data "decrypted-heic")
         url (copy-marker 1) (copy-marker (point-max))
         (current-buffer) t)
        (should (equal saved (list url "decrypted-heic")))
        (should (eq (get-text-property 1 'jabber-chat-image-fetching)
                    'failed))))))

(ert-deftest jabber-test-chat-aesgcm-save-fallback-writes-decrypted-bytes ()
  "The aesgcm save fallback writes retained plaintext, not ciphertext."
  (let* ((url (concat "aesgcm://example.org/photo.jpg#"
                      (make-string 88 ?a)))
         (plaintext (unibyte-string 0 1 2 255))
         (dest (make-temp-file "jabber-save-fallback-")))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'jabber-chat--download-destination)
                   (lambda (_) dest)))
          (jabber-chat--offer-image-save url plaintext)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally dest)
            (should (equal (buffer-string) plaintext))))
      (delete-file dest))))

(ert-deftest jabber-test-chat-automatic-decode-failure-does-not-offer-save ()
  "Background image fetching must never open an interactive save prompt."
  (with-temp-buffer
    (let ((url "https://example.org/photo.jpg")
          (offered nil))
      (insert (propertize url 'jabber-chat-image-url url))
      (cl-letf (((symbol-function 'jabber-chat--offer-image-save)
                 (lambda (&rest _) (setq offered t))))
        (jabber-chat--handle-image-result
         (list :error 'decode :data "unsupported-image")
         url (copy-marker 1) (copy-marker (point-max))
         (current-buffer) nil)
        (should-not offered)
        (should (eq (get-text-property 1 'jabber-chat-image-fetching)
                    'failed))))))

(provide 'jabber-test-chat)

;;; jabber-test-chat.el ends here
