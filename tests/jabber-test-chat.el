;;; jabber-test-chat.el --- Tests for jabber-chat  -*- lexical-binding: t; -*-

;;; Commentary:

;; One-to-one chat message parsing and display.

;;; Code:

(require 'ert)
(require 'xref)
(require 'jabber-chat)
(require 'jabber-chat-commands)

(defun jabber-test-chat--make-fake-jc (account)
  "Create a fake connection symbol for ACCOUNT."
  (let ((jc (gensym "jabber-test-chat-jc-"))
        (parts (split-string account "@")))
    (put jc :state-data (list :username (nth 0 parts)
                              :server (nth 1 parts)))
    jc))

(defmacro jabber-test-chat--with-db (&rest body)
  "Run BODY with a fresh temporary Jabber database."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "jabber-chat-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" dir))
          (jabber-db--connection nil))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

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
  `(save-window-excursion
     (with-temp-buffer
       (let* ((history (cons nil nil))
              (xref-history-storage (lambda (&optional _new-value) history))
              (jabber-chat-ewoc (ewoc-create
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
           ,@body)))))

(ert-deftest jabber-test-chat-goto-reply-target-history ()
  "Reply jumps preserve exact positions through nested back navigation."
  (jabber-test-chat--with-reply-ewoc
    (let* ((reply (point))
           (node (jabber-chat-ewoc-enter
                  '(:foreign (:id "r-2" :body "nested reply"
                              :reply-to-id "r-1"))))
           (origin (+ 3 (ewoc-location node))))
      (set-marker jabber-point-insert (point-max))
      (goto-char origin)
      (jabber-chat-goto-reply-target-or-send)
      (should (= (point) reply))
      (jabber-chat-goto-reply-target-or-send)
      (should (= (point) (point-min)))
      (xref-go-back)
      (should (= (point) reply))
      (xref-go-back)
      (should (= (point) origin)))))

(ert-deftest jabber-test-chat-goto-reply-target-failure-preserves-history ()
  "Missing replies and targets do not alter point or xref history."
  (jabber-test-chat--with-reply-ewoc
    (xref-push-marker-stack)
    (let ((before (copy-tree (funcall xref-history-storage)))
          (origin (point)))
      (setf (plist-get (cadr (ewoc-data (ewoc-locate jabber-chat-ewoc)))
                       :reply-to-id)
            "missing")
      (should-error (jabber-chat-goto-reply-target) :type 'user-error)
      (should (= (point) origin))
      (should (equal before (funcall xref-history-storage)))
      (goto-char (point-min))
      (should-error (jabber-chat-goto-reply-target) :type 'user-error)
      (should (= (point) (point-min)))
      (should (equal before (funcall xref-history-storage))))))

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
               (lambda (_account _peer reply-id _muc-p &optional _sender)
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

(ert-deftest jabber-test-chat-create-buffer-does-not-start-mam ()
  "Creating or reopening a chat buffer does not start MAM catch-up."
  (let* ((jc1 (jabber-test-chat--make-fake-jc "me@example.com"))
         (jc2 (jabber-test-chat--make-fake-jc "me@example.com"))
         (peer "emma@example.com/laptop")
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
            (should-not calls)
            (should (eq buf (jabber-chat-create-buffer jc2 peer)))
            (should-not calls)
            (with-current-buffer buf
              (should (eq jc2 jabber-buffer-connection))))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest jabber-test-chat-create-buffer-restores-parent-session ()
  "Creating a parent chat restores its current logical session."
  (let* ((jc (jabber-test-chat--make-fake-jc "me@example.com"))
         (peer "emma@example.com/laptop")
         (jabber-chat-buffer-format " *jabber-test-session-%j-%a*")
         buffer)
    (cl-letf (((symbol-function 'jabber-db-backlog)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-db-get-chat-encryption)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-db-get-chat-thread)
               (lambda (account bare-peer)
                 (should (equal account "me@example.com"))
                 (should (equal bare-peer "emma@example.com"))
                 "session-1")))
      (unwind-protect
          (progn
            (setq buffer (jabber-chat-create-buffer jc peer))
            (should
             (equal "session-1"
                    (buffer-local-value
                     'jabber-message-thread-session-id buffer))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-test-chat-capture-creates-parent-session ()
  "The first parent send creates, persists, and captures a session ID."
  (with-temp-buffer
    (setq-local jabber-buffer-connection
                (jabber-test-chat--make-fake-jc "me@example.com"))
    (setq-local jabber-chatting-with "emma@example.com/laptop")
    (let (stored)
      (cl-letf (((symbol-function 'jabber-message-thread--generate-id)
                 (lambda () "session-1"))
                ((symbol-function 'jabber-db-set-chat-thread)
                 (lambda (&rest args) (setq stored args))))
        (let ((context (jabber-chat--capture-send-context "hello" nil)))
          (should
           (equal '((thread nil "session-1"))
                  (plist-get context :extra-elements)))
          (should (equal "session-1" jabber-message-thread-session-id))
          (should
           (equal '("me@example.com" "emma@example.com" "session-1")
                  stored)))))))

(ert-deftest jabber-test-chat-parent-session-scope ()
  "Only ordinary one-to-one parent buffers own chat sessions."
  (with-temp-buffer
    (setq-local jabber-chatting-with "emma@example.com/laptop")
    (should (jabber-chat--parent-session-buffer-p))
    (setq-local jabber-group "room@example.com")
    (should-not (jabber-chat--parent-session-buffer-p))
    (setq-local jabber-group nil)
    (setq-local jabber-muc-private-p t)
    (should-not (jabber-chat--parent-session-buffer-p))
    (setq-local jabber-muc-private-p nil)
    (setq-local jabber-message-thread-id "dedicated-thread")
    (should-not (jabber-chat--parent-session-buffer-p))))

(ert-deftest jabber-test-chat-plaintext-reuses-parent-session ()
  "Plaintext sends generate one session ID and keep using it."
  (with-temp-buffer
    (setq-local jabber-buffer-connection
                (jabber-test-chat--make-fake-jc "me@example.com"))
    (setq-local jabber-chatting-with "emma@example.com/laptop")
    (setq-local jabber-chat-encryption 'plaintext)
    (let ((jabber-chat-send-hooks '(jabber-chat--session-send-hook))
          generated sent stored)
      (cl-letf (((symbol-function 'jabber-message-thread--generate-id)
                 (lambda ()
                   (setq generated (1+ (or generated 0)))
                   "session-1"))
                ((symbol-function 'jabber-db-set-chat-thread)
                 (lambda (&rest args) (push args stored)))
                ((symbol-function 'jabber-chat--display-local-message)
                 #'ignore)
                ((symbol-function 'jabber-send-sexp)
                 (lambda (_jc stanza &rest _)
                   (push stanza sent))))
        (jabber-chat-send jabber-buffer-connection "first")
        (jabber-chat-send jabber-buffer-connection "second"))
      (should (= generated 1))
      (should
       (equal '(("me@example.com" "emma@example.com" "session-1"))
              stored))
      (dolist (stanza sent)
        (should
         (equal '(:thread-id "session-1" :thread-parent-id nil)
                (jabber-message-thread-protocol-fields stanza)))))))

(ert-deftest jabber-test-chat-session-does-not-override-explicit-thread ()
  "Explicit reply or dedicated metadata takes precedence over a session."
  (with-temp-buffer
    (setq-local jabber-message-thread-session-id "session-1")
    (should
     (equal '(:thread-id "thread-1" :thread-parent-id nil)
            (jabber-chat--captured-thread
             '(message () (thread () "thread-1")) nil)))
    (setq-local jabber-message-reply--thread
                '(:thread-id "reply-thread" :thread-parent-id nil))
    (should
     (equal jabber-message-reply--thread
            (jabber-chat--captured-thread '(message ()) nil)))
    (setq-local jabber-message-reply--thread nil)
    (setq-local jabber-message-thread-id "dedicated-thread")
    (should
     (equal '(:thread-id "dedicated-thread" :thread-parent-id nil)
            (jabber-chat--captured-thread '(message ()) nil)))))

(ert-deftest jabber-test-chat-live-message-adopts-parent-session ()
  "A live parent message makes its thread the current chat session."
  (let ((buffer (generate-new-buffer " *jabber-test-live-session*"))
        stored)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local jabber-buffer-connection 'connection)
            (setq-local jabber-chatting-with "emma@example.com/laptop"))
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_) "me@example.com"))
                    ((symbol-function 'jabber-chat--decrypt-if-needed)
                     (lambda (_jc stanza) stanza))
                    ((symbol-function 'jabber-chat--select-buffer)
                     (lambda (&rest _) buffer))
                    ((symbol-function 'jabber-chat--display-message) #'ignore)
                    ((symbol-function 'jabber-db-set-chat-thread)
                     (lambda (&rest args) (setq stored args))))
            (let ((jabber-chat-printers (list (lambda (&rest _) t))))
              (jabber-process-chat
               'connection
               '(message ((from . "emma@example.com/laptop")
                          (to . "me@example.com") (type . "chat"))
                         (body () "hello")
                         (thread () "session-2")))))
          (should
           (equal "session-2"
                  (buffer-local-value
                   'jabber-message-thread-session-id buffer)))
          (should
           (equal '("me@example.com" "emma@example.com" "session-2")
                  stored)))
      (kill-buffer buffer))))

(ert-deftest jabber-test-chat-child-thread-preserves-parent-session ()
  "A live child thread cannot become the ordinary parent chat session."
  (jabber-test-chat--with-db
    (let* ((jc (jabber-test-chat--make-fake-jc "me@example.com"))
           (buffer (generate-new-buffer " *jabber-test-child-session*"))
           displayed)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local major-mode 'jabber-chat-mode)
              (setq-local jabber-buffer-connection jc)
              (setq-local jabber-chatting-with "friend@example.com/phone")
              (setq-local jabber-message-thread-session-id "session-S"))
            (jabber-db-set-chat-thread
             "me@example.com" "friend@example.com" "session-S")
            (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                       (lambda (_jc stanza) stanza))
                      ((symbol-function 'jabber-chat--select-buffer)
                       (lambda (&rest _) buffer))
                      ((symbol-function 'jabber-chat--display-message)
                       (lambda (_jc _xml target &rest _)
                         (push target displayed))))
              (let ((jabber-chat-printers (list (lambda (&rest _) t))))
                (jabber-process-chat
                 jc
                 '(message ((from . "friend@example.com/phone")
                            (to . "me@example.com") (type . "chat")
                            (id . "child-1"))
                           (body () "child root")
                           (thread ((parent . "session-S")) "thread-T"))))
              (should
               (equal "session-S"
                      (buffer-local-value
                       'jabber-message-thread-session-id buffer)))
              (should
               (equal "session-S"
                      (jabber-db-get-chat-thread
                       "me@example.com" "friend@example.com")))
              (let ((jabber-db-message-thread-stored-functions nil))
                (jabber-db-store-message
                 "me@example.com" "friend@example.com" "in" "chat"
                 "child root" 1 "phone" "child-1" nil nil nil nil nil
                 '(:thread-id "thread-T" :thread-parent-id "session-S")))
              (should
               (jabber-db-message-thread-known-p
                "me@example.com" "friend@example.com" "chat" "thread-T"))
              (let ((jabber-chat-printers (list (lambda (&rest _) t))))
                (jabber-process-chat
                 jc
                 '(message ((from . "friend@example.com/phone")
                            (to . "me@example.com") (type . "chat")
                            (id . "child-2"))
                           (body () "later child")
                           (thread ((parent . "session-S")) "thread-T"))))
              (should
               (equal "session-S"
                      (buffer-local-value
                       'jabber-message-thread-session-id buffer)))
              (with-current-buffer buffer
                (let ((context
                       (jabber-chat--capture-send-context "parent send" nil)))
                  (should
                   (equal '(:thread-id "session-S" :thread-parent-id nil)
                          (jabber-message-thread-protocol-fields
                           `(message ()
                                     ,@(plist-get context :extra-elements)))))
                  (should
                   (eq buffer
                       (jabber-chat--local-message-buffer
                        jc '(:thread-id "session-S" :id "local-1"))))))
              (should (equal (list nil buffer) displayed))))
        (kill-buffer buffer)))))

(ert-deftest jabber-test-chat-live-unthreaded-message-starts-session ()
  "A live unthreaded parent message starts a local logical session."
  (let ((buffer (generate-new-buffer " *jabber-test-new-session*"))
        stored)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local jabber-buffer-connection 'connection)
            (setq-local jabber-chatting-with "emma@example.com/laptop"))
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_) "me@example.com"))
                    ((symbol-function 'jabber-chat--decrypt-if-needed)
                     (lambda (_jc stanza) stanza))
                    ((symbol-function 'jabber-chat--select-buffer)
                     (lambda (&rest _) buffer))
                    ((symbol-function 'jabber-chat--display-message) #'ignore)
                    ((symbol-function 'jabber-message-thread--generate-id)
                     (lambda () "session-new"))
                    ((symbol-function 'jabber-db-set-chat-thread)
                     (lambda (&rest args) (setq stored args))))
            (let ((jabber-chat-printers (list (lambda (&rest _) t))))
              (jabber-process-chat
               'connection
               '(message ((from . "emma@example.com/laptop")
                          (to . "me@example.com") (type . "chat"))
                         (body () "hello")))))
          (should
           (equal "session-new"
                  (buffer-local-value
                   'jabber-message-thread-session-id buffer)))
          (should
           (equal '("me@example.com" "emma@example.com" "session-new")
                  stored)))
      (kill-buffer buffer))))

(ert-deftest jabber-test-chat-mam-does-not-change-parent-session ()
  "MAM replay cannot replace a live parent session."
  (let ((buffer (generate-new-buffer " *jabber-test-mam-session*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local jabber-buffer-connection 'connection)
            (setq-local jabber-chatting-with "emma@example.com/laptop")
            (setq-local jabber-message-thread-session-id "session-live"))
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_) "me@example.com"))
                    ((symbol-function 'jabber-chat--decrypt-if-needed)
                     (lambda (_jc stanza) stanza))
                    ((symbol-function 'jabber-chat--find-buffer)
                     (lambda (&rest _) buffer))
                    ((symbol-function 'jabber-chat--display-message) #'ignore)
                    ((symbol-function 'jabber-db-set-chat-thread)
                     (lambda (&rest _)
                       (ert-fail "Persisted a MAM session"))))
            (let ((jabber-chat-printers (list (lambda (&rest _) t))))
              (jabber-process-chat
               'connection
               '(message ((from . "emma@example.com/laptop")
                          (to . "me@example.com") (type . "chat")
                          (jabber-mam--origin . "t"))
                         (body () "archived")
                         (thread () "session-old")))))
          (should
           (equal "session-live"
                  (buffer-local-value
                   'jabber-message-thread-session-id buffer))))
      (kill-buffer buffer))))

(ert-deftest jabber-test-chat-with-starts-mam-after-buffer-creation ()
  "Explicitly opening a chat starts MAM after creating its buffer."
  (let ((buffer (generate-new-buffer " *jabber-test-chat-with*"))
        events)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-chat-create-buffer)
                   (lambda (jc jid)
                     (push (list 'create jc jid) events)
                     buffer))
                  ((symbol-function 'jabber-mam-chat-opened)
                   (lambda (jc peer)
                     (push (list 'mam jc peer) events)))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (target &rest _) target)))
          (should (eq buffer
                      (jabber-chat-with 'connection
                                        "emma@example.com/laptop")))
          (should (equal (nreverse events)
                         '((create connection "emma@example.com/laptop")
                           (mam connection "emma@example.com")))))
      (kill-buffer buffer))))

(ert-deftest jabber-test-chat-mam-replay-does-not-create-buffer ()
  "A printable MAM replay does not create a chat buffer."
  (let ((stanza
         '(message ((from . "emma@example.com/laptop")
                    (to . "me@example.com")
                    (type . "chat")
                    (jabber-mam--origin . "t"))
                   (body () "archived")))
        created
        displayed)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc message) message))
              ((symbol-function 'jabber-chat--find-buffer)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-chat-create-buffer)
               (lambda (&rest _) (setq created t)))
              ((symbol-function 'jabber-chat--display-message)
               (lambda (&rest _) (setq displayed t))))
      (let ((jabber-chat-printers (list (lambda (&rest _) t))))
        (jabber-process-chat 'connection stanza))
      (should displayed)
      (should-not created))))

(ert-deftest jabber-test-chat-mam-replay-finds-buffer-by-account ()
  "MAM replay uses the peer buffer belonging to its connection."
  (let ((account-a-buffer (generate-new-buffer " *mam-account-a*"))
        (account-b-buffer (generate-new-buffer " *mam-account-b*"))
        (account-b-thread (generate-new-buffer " *mam-account-b-thread*"))
        (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
        displayed)
    (unwind-protect
        (progn
          (dolist (entry `((,account-b-buffer account-b)
                           (,account-a-buffer account-a)))
            (with-current-buffer (car entry)
              (setq-local major-mode 'jabber-chat-mode)
              (setq-local jabber-buffer-connection (cadr entry))
              (setq-local jabber-chatting-with "friend@example.com")
              (jabber-buffer-registry-register 'chat "friend@example.com")))
          (with-current-buffer account-b-thread
            (setq-local major-mode 'jabber-chat-mode)
            (setq-local jabber-buffer-connection 'account-b)
            (setq-local jabber-chatting-with "friend@example.com")
            (setq-local jabber-message-thread-id "thread-1"))
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (jc)
                       (pcase jc
                         ('account-a "a@example.com")
                         ('account-b "b@example.com"))))
                    ((symbol-function 'jabber-chat--decrypt-if-needed)
                     (lambda (_jc stanza) stanza))
                    ((symbol-function 'buffer-list)
                     (lambda (&rest _)
                       (list account-b-thread account-b-buffer
                             account-a-buffer)))
                    ((symbol-function 'jabber-chat-create-buffer)
                     (lambda (&rest _)
                       (ert-fail "MAM replay created a chat buffer")))
                    ((symbol-function 'jabber-chat--display-message)
                     (lambda (_jc _xml buffer &rest _)
                       (setq displayed buffer))))
            (let ((jabber-chat-printers (list (lambda (&rest _) t))))
              (jabber-process-chat
               'account-b
               '(message ((from . "friend@example.com/phone")
                          (to . "b@example.com") (type . "chat")
                          (jabber-mam--origin . "t"))
                         (body () "archived")))))
          (should (eq displayed account-b-buffer)))
      (kill-buffer account-a-buffer)
      (kill-buffer account-b-buffer)
      (kill-buffer account-b-thread))))

(ert-deftest jabber-test-chat-disabled-threads-load-original-backlog ()
  "Load threaded messages into a newly created parent chat buffer."
  (let* ((jc (jabber-test-chat--make-fake-jc "me@example.com"))
         (peer "emma@example.com/laptop")
         (jabber-message-thread-use-buffers nil)
         (jabber-chat-buffer-format " *jabber-test-disabled-%j-%a*")
         backlog-args
         buffer)
    (cl-letf (((symbol-function 'jabber-db-backlog)
               (lambda (&rest args)
                 (setq backlog-args args)
                 nil))
              ((symbol-function 'jabber-db-get-chat-encryption)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-mam-chat-opened) #'ignore))
      (unwind-protect
          (progn
            (setq buffer (jabber-chat-create-buffer jc peer))
            (should (eq t (nth 6 backlog-args))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
    (put jc :state-data (list :roster (mapcar (lambda (jid) (jabber-jid-symbol jid jc)) jids)))
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

;;; Native chat regression journeys

(require 'jabber-message-correct)
(require 'jabber-receipts)
(require 'jabber-console)

(defvar jabber-omemo--pending-send-operations)

(defun jabber-test-chat--journey-connection (user)
  "Return an inert established connection for USER."
  (let ((jc (make-symbol user)))
    (put jc :state :session-established)
    (put jc :state-data
         (list :username user :server "example.org" :resource "test"
               :connection 'test-transport :send-function #'ignore))
    jc))

(defun jabber-test-chat--owned-omemo-room (jc)
  "Admit a room and peer presence owned by fixture connection JC."
  (jabber-muc-add-groupchat "room@example.org" "self" jc)
  (jabber-muc-process-presence
   jc '(presence ((from . "room@example.org/peer"))
                 (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                    (item ((jid . "peer@example.org/mobile")
                           (role . "participant") (affiliation . "none"))))))
  (should (equal '("peer@example.org")
                 (jabber-muc--identity-jids jc "room@example.org"))))

(defmacro jabber-test-chat--journey (&rest body)
  "Run BODY in a real rendered chat with an inert transport."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-db-path nil)
           (jabber-muc--rooms (make-hash-table :test #'equal))
           (jabber-muc--room-jids (make-hash-table :test #'equal))
           (jabber-chat-default-encryption 'plaintext)
           (jabber-chat-display-help-at-point nil)
           (jabber-chat-display-images nil)
           (jabber-chat-display-link-previews nil)
           (jabber-print-rare-time nil)
           (jabber-chat-mode-hook nil)
           (jc (jabber-test-chat--journey-connection "me")))
       (jabber-chat-mode)
       (setq-local jabber-chatting-with "peer@example.org")
       (jabber-chat-mode-setup jc #'jabber-chat-pp)
       (setq-local jabber-send-function #'jabber-chat-send)
       (let ((jabber-connections (list jc))) ,@body))))

(ert-deftest jabber-test-chat-journey-send-failure-keeps-reply ()
  "Real transport rejection preserves Unicode input and one-shot metadata."
  (dolist (outcome '(error quit success))
    (jabber-test-chat--journey
      (setq-local jabber-chat-send-hooks '(jabber-message-reply--send-hook))
      (setq-local jabber-message-reply--id "reply")
      (setq-local jabber-message-reply--jid "peer@example.org")
      (setq-local jabber-message-reply--thread '(:thread-id "thread"))
      (setq-local jabber-message-reply--fallback-text "> λ\n")
      (insert "> λ\nanswer")
      (backward-char 2)
      (let ((offset (- (point) jabber-point-insert)))
        (pcase outcome
          ('error (plist-put (get jc :state-data) :connection nil))
          ('quit (plist-put (get jc :state-data) :send-function
                            (lambda (&rest _) (signal 'quit nil)))))
        (pcase outcome
          ('error (should-error (call-interactively (key-binding (kbd "RET")))))
          ('quit (should (eq 'quit
                             (condition-case nil
                                 (call-interactively (key-binding (kbd "RET")))
                               (quit 'quit)))))
          ('success (call-interactively (key-binding (kbd "RET")))))
        (if (eq outcome 'success)
            (progn
              (should (equal "" (jabber-chat--input-string)))
              (should (equal jabber-chat--input-history '("> λ\nanswer")))
              (should-not jabber-message-reply--id))
          (should (equal "> λ\nanswer" (jabber-chat--input-string)))
          (should (= offset (- (point) jabber-point-insert)))
          (should-not jabber-chat--input-history)
          (should (equal "reply" jabber-message-reply--id))
          (should (equal '(:thread-id "thread") jabber-message-reply--thread))
          (should (equal "> λ\n" jabber-message-reply--fallback-text))
          (should-not (ewoc-nth jabber-chat-ewoc 0)))))))

(ert-deftest jabber-test-chat-journey-console-ret-failure ()
  "The actual console RET command preserves rejected and cancelled XML."
  (dolist (outcome '(error quit success))
    (let* ((jc (jabber-test-chat--journey-connection "console"))
           (jabber-db-path nil)
           (jabber-debug-log-xml nil)
           (jabber-console-truncate-lines 0)
           (jabber-console-name-format " *test-console-%s*")
           (jabber-connections (list jc))
           (buffer (jabber-console-create-buffer jc))
           (body "<message><body>λ draft</body></message>"))
      (unwind-protect
          (with-current-buffer buffer
            (insert body)
            (cl-letf (((symbol-function 'jabber-send-string)
                       (lambda (_jc text)
                         (should (equal body text))
                         (pcase outcome
                           ('error (error "Rejected"))
                           ('quit (signal 'quit nil))))))
              (pcase outcome
                ('error (should-error (call-interactively (key-binding (kbd "RET")))))
                ('quit (should (eq 'quit
                                   (condition-case nil
                                       (call-interactively (key-binding (kbd "RET")))
                                     (quit 'quit)))))
                ('success (call-interactively (key-binding (kbd "RET"))))))
            (should (equal (jabber-chat--input-string)
                           (if (eq outcome 'success) "" body)))
            (should (equal jabber-chat--input-history
                           (and (eq outcome 'success) (list body)))))
        (kill-buffer buffer)))))

(ert-deftest jabber-test-chat-journey-correction-draft-undo ()
  "Length-changing corrections preserve the draft, point and undo/redo."
  (dolist (new-body '("a substantially longer corrected message" "x"))
    (jabber-test-chat--journey
      (jabber-chat-ewoc-enter
       (list :local (list :id "original" :body "original message"
                          :timestamp (current-time))))
      (goto-char (point-max))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (insert "draft λ")
      (undo-boundary)
      (backward-char 2)
      (let ((offset (- (point) jabber-point-insert)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) new-body)))
          (call-interactively #'jabber-correct-last-message))
        (should (= offset (- (point) jabber-point-insert)))
        (should (equal "draft λ" (jabber-chat--input-string)))
        (let ((transcript (buffer-substring-no-properties (point-min) jabber-point-insert)))
          (let ((last-command nil)) (undo-only 1))
          (should (equal "" (jabber-chat--input-string)))
          (undo-boundary)
          (undo-redo)
          (should (equal "draft λ" (jabber-chat--input-string)))
          (should (equal transcript
                         (buffer-substring-no-properties (point-min) jabber-point-insert))))))))

(ert-deftest jabber-test-chat-journey-correction-storage ()
  "A correction changes SQLite and the rendered message only after handoff."
  (dolist (outcome '(error quit success))
    (jabber-test-chat--journey
      (jabber-test-chat--with-db
        (jabber-db-store-message "me@example.org" "peer@example.org"
                                 "out" "chat" "original" 100 nil "original")
        (jabber-chat-ewoc-enter
         (list :local (list :id "original" :body "original" :timestamp '(0 100))))
        (pcase outcome
          ('error (plist-put (get jc :state-data) :connection nil))
          ('quit (plist-put (get jc :state-data) :send-function
                            (lambda (&rest _) (signal 'quit nil)))))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "corrected λ")))
          (pcase outcome
            ('error (should-error (call-interactively #'jabber-correct-last-message)))
            ('quit (should (eq 'quit (condition-case nil
                                        (call-interactively #'jabber-correct-last-message)
                                      (quit 'quit)))))
            ('success (call-interactively #'jabber-correct-last-message))))
        (should-not jabber-message-correct--pending-outgoing)
        (let* ((expected (if (eq outcome 'success) "corrected λ" "original"))
               (msg (cadr (ewoc-data (jabber-chat-ewoc-find-by-id "original")))))
          (should (equal expected (plist-get msg :body)))
          (should (eq (eq outcome 'success) (plist-get msg :edited)))
          (should (equal (list (list expected))
                         (sqlite-select jabber-db--connection "SELECT body FROM message"))))))))

(ert-deftest jabber-test-chat-journey-reaction-account-routing ()
  "Direct and received-carbon reactions update only their account, even renamed."
  (dolist (carbon '(nil t))
    (let* ((jabber-db-path nil)
           (jabber-chat-default-encryption 'plaintext)
           (jabber-chat-display-help-at-point nil)
           (jabber-chat-display-images nil)
           (jabber-chat-display-link-previews nil)
           (jabber-chat-mode-hook nil)
           (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
           (a-jc (jabber-test-chat--journey-connection "a"))
           (b-jc (jabber-test-chat--journey-connection "b"))
           (a (jabber-chat-create-buffer a-jc "peer@example.org"))
           (b (jabber-chat-create-buffer b-jc "peer@example.org")))
      (unwind-protect
          (progn
            (dolist (buffer (list a b))
              (with-current-buffer buffer
                (jabber-chat-ewoc-enter
                 (list :foreign (list :id "same-id" :from "peer@example.org"
                                      :body "peer message" :timestamp (current-time))))))
            (with-current-buffer a (rename-buffer " *renamed-chat-test*" t))
            (dolist (id '("same-id" "missing"))
              (let ((stanza `(message ((from . "peer@example.org/mobile") (type . "chat"))
                                     (reactions ((xmlns . "urn:xmpp:reactions:0") (id . ,id))
                                                (reaction nil "👍")))))
                (jabber-reactions--handle-message
                 a-jc (if carbon
                          `(message ((from . "a@example.org"))
                                    (received ((xmlns . "urn:xmpp:carbons:2"))
                                              (forwarded ((xmlns . "urn:xmpp:forward:0")) ,stanza)))
                        stanza))))
            (with-current-buffer a
              (should (equal '(("peer@example.org" "👍"))
                             (plist-get (cadr (ewoc-data (jabber-chat-ewoc-find-by-id "same-id")))
                                        :reactions))))
            (with-current-buffer b
              (should-not (plist-get (cadr (ewoc-data (jabber-chat-ewoc-find-by-id "same-id")))
                                     :reactions))))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest jabber-test-chat-journey-receipts-same-second ()
  "Public marker handling advances equal-second display state and SQLite."
  (dolist (times '(((0 100 100000) (0 100 900000)) ((0 100) (0 100))))
    (jabber-test-chat--journey
      (jabber-test-chat--with-db
        (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal)))
          (rename-buffer (jabber-chat-get-buffer "peer@example.org" jc))
          (jabber-buffer-registry-register 'chat "peer@example.org")
          (cl-loop for id in '("first" "second") for time in times do
                   (jabber-db-store-message "me@example.org" "peer@example.org"
                                            "out" "chat" id 100 nil id)
                   (jabber-db-update-receipt "me@example.org" "peer@example.org" id "delivered_at" 101)
                   (jabber-chat-ewoc-enter
                    (list :local (list :id id :body id :status :delivered :timestamp time))))
          (dolist (id '("first" "second" "first" "missing" "second"))
            (jabber-receipts--handle-message
             jc `(message ((from . "peer@example.org/mobile") (type . "chat"))
                          (displayed ((xmlns . "urn:xmpp:chat-markers:0") (id . ,id))))))
          (dolist (id '("first" "second"))
            (should (eq :displayed (plist-get (cadr (ewoc-data (jabber-chat-ewoc-find-by-id id))) :status))))
          (should (= 2 (caar (sqlite-select jabber-db--connection
                                           "SELECT count(*) FROM message WHERE displayed_at IS NOT NULL")))))))))

(ert-deftest jabber-test-chat-journey-explicit-thread-reply ()
  "The native thread hook and global hooks emit one reply with Unicode fallback."
  (jabber-test-chat--journey
    (let* ((jabber-message-thread-use-buffers t)
           (thread (jabber-message-thread-create-buffer
                    jc "peer@example.org" "chat" "thread-1" nil (current-buffer)
                    '(:id "root-link" :from "peer@example.org" :body "root")))
           sent)
      (unwind-protect
          (with-current-buffer thread
            (plist-put (get jc :state-data) :send-function
                       (lambda (_connection text) (setq sent text)))
            (let ((node (jabber-chat-ewoc-enter
                         '(:foreign (:id "explicit" :from "peer@example.org"
                                     :body "λ quoted" :thread-id "thread-1")))))
              (goto-char (ewoc-location node))
              (call-interactively #'jabber-chat-reply))
            (goto-char (point-max))
            (insert "answer")
            (let ((fallback jabber-message-reply--fallback-text))
              (call-interactively (key-binding (kbd "RET")))
              (should-not jabber-message-reply--id)
              (should-not jabber-message-thread--root-reply-id)
              (should (equal "" (jabber-chat--input-string)))
              (with-temp-buffer
                (insert sent)
                (let* ((stanza (car (xml-parse-region (point-min) (point-max))))
                       (replies (jabber-xml-get-children stanza 'reply))
                       (fb (jabber-xml-child-with-xmlns stanza "urn:xmpp:fallback:0")))
                  (should (= 1 (length replies)))
                  (should (equal "explicit" (jabber-xml-get-attribute (car replies) 'id)))
                  (should (equal (number-to-string (length fallback))
                                 (jabber-xml-get-attribute (car (jabber-xml-get-children fb 'body)) 'end)))))))
        (kill-buffer thread)))))

(ert-deftest jabber-test-chat-journey-correction-queued-handoff ()
  "A queued correction commits on native SM handoff, not queue acceptance."
  (require 'jabber-sm-runtime)
  (dolist (outcome '(success failure retired))
    (jabber-test-chat--journey
      (jabber-test-chat--with-db
        (jabber-db-store-message "me@example.org" "peer@example.org"
                                 "out" "chat" "original" 100 nil "original")
        (jabber-chat-ewoc-enter
         (list :local (list :id "original" :body "original" :timestamp '(0 100))))
        (put jc :state-data (append (get jc :state-data)
                                   (list :sm-enabled t :sm-outbound-count 0
                                         :sm-last-acked 0 :sm-pending-queue nil)))
        (let ((jabber-sm-max-in-flight 0))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "queued correction")))
            (call-interactively #'jabber-correct-last-message)))
        (should jabber-message-correct--pending-outgoing)
        (should (equal '(("original"))
                       (sqlite-select jabber-db--connection "SELECT body FROM message")))
        (let* ((entry (car (plist-get (get jc :state-data) :sm-pending-queue)))
               (callback (plist-get entry :success)))
          (should (functionp callback))
          (pcase outcome
            ('failure (jabber-sm--discard-pending (get jc :state-data) "discarded"))
            ('retired
             (fundamental-mode)
             (let ((jabber-sm-max-in-flight nil))
               (jabber-sm--drain-pending jc (get jc :state-data))))
            ('success (let ((jabber-sm-max-in-flight nil))
                        (jabber-sm--drain-pending jc (get jc :state-data)))))
          ;; Repeated/stale deliveries must not commit or reapply.
          (funcall callback)
          (funcall callback))
        (should-not jabber-message-correct--pending-outgoing)
        (should (equal (list (list (if (eq outcome 'failure) "original" "queued correction")))
                       (sqlite-select jabber-db--connection "SELECT body FROM message")))))))

(ert-deftest jabber-test-chat-journey-omemo-input-restored-once ()
  "Native OMEMO immediate and delayed session failure restores the draft once."
  (require 'jabber-omemo)
  (dolist (outcome '(immediate delayed quit))
    (jabber-test-chat--journey
      (setq-local jabber-chat-encryption 'omemo)
      (setq-local jabber-message-reply--id "reply")
      (setq-local jabber-message-reply--jid "peer@example.org")
      (let ((jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
            callback)
        (insert "encrypted draft λ")
        (cl-letf (((symbol-function 'jabber-omemo--ensure-sessions)
                   (lambda (_jc _peer on-result)
                     (pcase outcome
                       ('immediate (funcall on-result nil))
                       ('delayed (setq callback on-result))
                       ('quit (signal 'quit nil))))))
          (if (eq outcome 'quit)
              (should (eq 'quit (condition-case nil
                                    (call-interactively (key-binding (kbd "RET")))
                                  (quit 'quit))))
            (call-interactively (key-binding (kbd "RET")))))
        (when callback
          (should (equal "" (jabber-chat--input-string)))
          (should-not jabber-message-reply--id)
          (funcall callback nil)
          (funcall callback nil))
        (should (equal "encrypted draft λ" (jabber-chat--input-string)))
        (should (equal "reply" jabber-message-reply--id))
        (when (eq outcome 'quit) (should-not jabber-chat--input-history))))))

(ert-deftest jabber-test-chat-journey-input-successor-preserved ()
  "A synchronous sender cannot restore its draft over a replacement input."
  (jabber-test-chat--journey
    (insert "old draft")
    (setq-local jabber-send-function
                (lambda (&rest _)
                  (fundamental-mode)
                  (let ((inhibit-read-only t)) (erase-buffer))
                  (insert "successor draft")
                  (error "Retired input")))
    (should-error (jabber-chat-buffer-send))
    (should (equal "successor draft" (buffer-string)))
    (should-not jabber-chat--input-history)))

(ert-deftest jabber-test-chat-journey-alert-origin ()
  "Incoming hook chains cannot retarget automatic replies or current buffer."
  (dolist (mutation '(unchanged rename account peer mode killed title))
    (jabber-test-chat--journey
      (rename-buffer (jabber-chat-get-buffer "peer@example.org" jc) t)
      (let* ((origin (current-buffer))
             (other (generate-new-buffer " *jabber-alert-other*"))
             (second (jabber-test-chat--journey-connection "other"))
             (jabber-connections (list jc second))
             (jabber-autoanswer-alist '(("ping" . "pong")))
             (jabber-alert-message-function
              (lambda (&rest _)
                (when (eq mutation 'title)
                  (with-current-buffer origin
                    (setq-local jabber-buffer-connection second)))
                "Peer"))
             (jabber-message-hooks
              (list (lambda (&rest _)
                      (with-current-buffer origin
                        (pcase mutation
                          ('rename (rename-buffer " *renamed-alert-origin*" t))
                          ('account (setq-local jabber-buffer-connection second))
                          ('peer (setq-local jabber-chatting-with "other@example.org"))
                          ('mode (fundamental-mode))
                          ('killed (kill-buffer origin))))
                      (set-buffer other)
                      ;; Hook truth values must not stop ordinary hook chains.
                      t)))
             seen sent
             (jabber-alert-message-hooks
              (list (lambda (_from buffer _text _title)
                      (push (list buffer (current-buffer)) seen))
                    #'jabber-autoanswer-answer)))
        (unwind-protect
            (progn
              (plist-put (get jc :state-data) :send-function
                         (lambda (&rest args) (push args sent)))
              (plist-put (get second :state-data) :send-function
                         (lambda (&rest args) (push args sent)))
              (jabber-process-chat
               jc '(message ((from . "peer@example.org") (type . "chat")
                              (id . "incoming")) (body () "ping")))
              (should (= 1 (length seen)))
              (if (memq mutation '(unchanged rename))
                  (progn
                    (should (eq origin (caar seen)))
                    (should (eq origin (current-buffer)))
                    (should (= 1 (length sent))))
                (should-not (caar seen))
                (should-not sent))
              (unless (eq mutation 'killed)
                (should (eq origin (cadar seen)))
                (should (eq origin (current-buffer)))))
          (kill-buffer other))))))

(require 'jabber-core)
(require 'jabber-openpgp)
(require 'jabber-omemo)
(require 'jabber-openpgp-legacy)

(defun jabber-test-chat--send-key-fixture (function)
  "Call FUNCTION with an isolated send-capable GnuPG keyring."
  (unless (and (executable-find "gpg") (executable-find "gpgconf"))
    (ert-skip "GnuPG and gpgconf are required"))
  (let* ((home (make-temp-file "jabber-send-gpg-" t))
         (epg-gpg-home-directory home)
         (process-environment (copy-sequence process-environment))
         (jabber-openpgp-key-alist nil))
    (set-file-modes home #o700)
    (setenv "GNUPGHOME" home)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "gpg.conf" home)
            (insert "no-auto-key-retrieve\nauto-key-locate clear\n"))
          (dolist (jid '("me@example.org" "peer@example.org"))
            (should (zerop (call-process
                            "gpg" nil nil nil "--homedir" home "--batch"
                            "--pinentry-mode" "loopback" "--passphrase" ""
                            "--quick-generate-key" (concat "xmpp:" jid)
                            "ed25519" "sign" "0")))
            (let* ((key (car (epg-list-keys (epg-make-context 'OpenPGP)
                                            (concat "=xmpp:" jid))))
                   (fingerprint (jabber-openpgp--key-fingerprint key)))
              (should (zerop (call-process
                              "gpg" nil nil nil "--homedir" home "--batch"
                              "--pinentry-mode" "loopback" "--passphrase" ""
                              "--quick-add-key" fingerprint "cv25519" "encr" "0")))
              (push (cons jid fingerprint) jabber-openpgp-key-alist)))
          (funcall function))
      (call-process "gpgconf" nil nil nil "--homedir" home "--kill" "all")
      (delete-directory home t))))

(defun jabber-test-chat--correction-wire (wire encryption group)
  "Check captured WIRE for ENCRYPTION and GROUP, decrypting real ciphertext."
  (let* ((stanza (with-temp-buffer
                   (insert "<stream>" wire "</stream>")
                   (car (jabber-xml-get-children
                         (car (xml-parse-region (point-min) (point-max))) 'message))))
         (replace (jabber-xml-get-children stanza 'replace)))
    (should (equal (jabber-xml-get-attribute stanza 'to)
                   (if group "room@example.org" "peer@example.org")))
    (should (equal (jabber-xml-get-attribute stanza 'type)
                   (if group "groupchat" "chat")))
    (should (= 1 (length replace)))
    (should (equal "original" (jabber-xml-get-attribute (car replace) 'id)))
    (pcase encryption
      ('openpgp
       (let* ((cipher (base64-decode-string
                       (caddr (jabber-xml-child-with-xmlns stanza jabber-openpgp-xmlns))))
              ;; Inspect the wire directly: the receive provider's internal
              ;; return value also carries authentication evidence.
              (plain (decode-coding-string
                      (epg-decrypt-string (epg-make-context 'OpenPGP) cipher)
                      'utf-8))
              (inner (with-temp-buffer
                       (insert plain)
                       (car (xml-parse-region (point-min) (point-max)))))
              (payload (car (jabber-xml-get-children inner 'payload))))
         (should (eq (car inner) (if group 'crypt 'signcrypt)))
         (should (equal "corrected λ" (caddr (car (jabber-xml-get-children payload 'body)))))))
      ('openpgp-legacy
       (let ((cipher (jabber-openpgp-legacy--rearmor-message
                      (jabber-openpgp-legacy--detect-encrypted stanza))))
         (should (equal "corrected λ"
                        (decode-coding-string
                         (epg-decrypt-string (epg-make-context 'OpenPGP) cipher)
                         'utf-8)))))
      (_ (should (equal "corrected λ" (caddr (car (jabber-xml-get-children stanza 'body)))))))))

(defun jabber-test-chat--provider-correction (encryption group)
  "Exercise ENCRYPTION and GROUP correction through native SM and SQLite."
  (dolist (outcome '(immediate drained discarded error quit))
    (ert-info ((format "%S %S %S" encryption group outcome))
      (jabber-test-chat--journey
        (jabber-test-chat--with-db
          (setq-local jabber-chat-encryption encryption)
          (when group (setq-local jabber-group "room@example.org"))
          (let* ((jabber-muc-participants
                  '(("room@example.org" ("peer" jid "peer@example.org/mobile"))))
                 (queued (memq outcome '(drained discarded)))
                 (jabber-sm-max-in-flight (and queued 0))
                 (peer (if group "room@example.org" "peer@example.org"))
                 (node (jabber-chat-ewoc-enter
                        (list (if group :muc-local :local)
                              (list :id "original" :body "original"
                                    :from (if group "room@example.org/me" "me@example.org")
                                    :timestamp '(0 100)))))
                 wire)
            (jabber-db-store-message "me@example.org" peer "out"
                                     (if group "groupchat" "chat")
                                     "original" 100 (and group "me") "original")
            (put jc :name 'jabber-connection)
            (put jc :state-data
                 (append (get jc :state-data)
                         (list :sm-enabled t :sm-outbound-count 0
                               :sm-last-acked 0 :sm-pending-queue nil)))
            (plist-put (get jc :state-data) :send-function
                       (lambda (_transport text)
                         (pcase outcome
                           ('error (error "Fixture write failed"))
                           ('quit (signal 'quit nil))
                           (_ (push text wire)))))
            (goto-char (point-max))
            (insert "successor draft λ")
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "corrected λ")))
              (condition-case err
                  (call-interactively #'jabber-correct-last-message)
                ((error quit)
                 (unless (memq outcome '(error quit))
                   (signal (car err) (cdr err))))))
            (when queued
              (should jabber-message-correct--pending-outgoing)
              (should-not wire)
              (should (equal "original" (plist-get (cadr (ewoc-data node)) :body)))
              (should (equal '(("original"))
                             (sqlite-select jabber-db--connection "SELECT body FROM message")))
              (let* ((entry (car (plist-get (get jc :state-data) :sm-pending-queue)))
                     (success (plist-get entry :success))
                     (failure (plist-get entry :failure)))
                (should (functionp success))
                (should (functionp failure))
                (if (eq outcome 'discarded)
                    (put jc :state-data
                         (jabber-sm--discard-pending (get jc :state-data) "Discarded"))
                  (let ((jabber-sm-max-in-flight nil)
                        (deadline (+ (float-time) 2)))
                    ;; Real FSM acknowledgement publishes state before its native timer.
                    (fsm-send-sync jc `(:stanza (a ((xmlns . ,jabber-sm-xmlns) (h . "0")))))
                    (while (and jabber-message-correct--pending-outgoing
                                (< (float-time) deadline))
                      (accept-process-output nil 0.01))))
                (should-not jabber-message-correct--pending-outgoing)
                (should (equal (if (eq outcome 'drained) "corrected λ" "original")
                               (plist-get (cadr (ewoc-data node)) :body)))
                (funcall success)
                (funcall failure "Late failure")
                (funcall success)))
            (let ((expected (if (memq outcome '(immediate drained)) "corrected λ" "original")))
              (should-not jabber-message-correct--pending-outgoing)
              (should (equal expected (plist-get (cadr (ewoc-data node)) :body)))
              (should (equal (list (list expected))
                             (sqlite-select jabber-db--connection "SELECT body FROM message")))
              (should (equal "successor draft λ" (jabber-chat--input-string)))
              (if (memq outcome '(immediate drained))
                  (progn
                    (should (= 1 (length wire)))
                    (jabber-test-chat--correction-wire (car wire) encryption group))
                (should-not wire)))))))))

(ert-deftest jabber-test-chat-provider-plaintext-muc-completion ()
  "Plaintext MUC corrections wait for native handoff."
  (jabber-test-chat--provider-correction 'plaintext t))

(ert-deftest jabber-test-chat-provider-modern-completion ()
  "Real modern OpenPGP direct and MUC corrections wait for native handoff."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (group '(nil t))
       (jabber-test-chat--provider-correction 'openpgp group)))))

(ert-deftest jabber-test-chat-provider-legacy-completion ()
  "Real legacy OpenPGP direct and MUC corrections wait for native handoff."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (group '(nil t))
       (jabber-test-chat--provider-correction 'openpgp-legacy group)))))

(ert-deftest jabber-test-chat-provider-omemo-quit-retirement ()
  "Retained discovery callbacks cannot resend or duplicate cancelled drafts."
  (dolist (group '(nil t))
    (dolist (own '(nil t))
      (jabber-test-chat--journey
        (setq-local jabber-chat-encryption 'omemo)
        (when group
          (setq-local jabber-group "room@example.org")
          (setq-local jabber-send-function #'jabber-muc-send))
        (let ((jabber-muc-participants
               '(("room@example.org" ("peer" jid "peer@example.org/mobile"))))
              (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
              callback sent)
          (when group (jabber-test-chat--owned-omemo-room jc))
          (plist-put (get jc :state-data) :send-function
                     (lambda (&rest _) (setq sent t)))
          (insert "cancelled draft λ")
          (cl-labels ((discover (_jc peer cb)
                        (if (and own (not (equal peer "me@example.org")))
                            (funcall cb '((1 . session)))
                          (setq callback cb)
                          (signal 'quit nil))))
            (cl-letf (((symbol-function 'jabber-omemo--ensure-sessions) #'discover)
                      ((symbol-function 'jabber-omemo--ensure-sessions-multi) #'discover))
              (should (eq 'quit (condition-case nil
                                   (call-interactively (key-binding (kbd "RET")))
                                 (quit 'quit))))))
          (should callback)
          (should (equal "cancelled draft λ" (jabber-chat--input-string)))
          (should-not (gethash jc jabber-omemo--pending-send-operations))
          (jabber-chat--replace-input "successor draft")
          (funcall callback nil)
          (funcall callback '((1 . session)))
          (should-not sent)
          (should (equal "successor draft" (jabber-chat--input-string))))))))

(ert-deftest jabber-test-chat-provider-muc-ret-failure-context ()
  "MUC RET restores consumed reply metadata on synchronous error and quit."
  (dolist (outcome '(error quit))
    (jabber-test-chat--journey
      (setq-local jabber-group "room@example.org")
      (setq-local jabber-send-function #'jabber-muc-send)
      (setq-local jabber-message-reply--id "reply")
      (setq-local jabber-message-reply--jid "room@example.org/peer")
      (setq-local jabber-message-reply--fallback-text "> λ\n")
      (insert "> λ\nanswer")
      (plist-put (get jc :state-data) :send-function
                 (lambda (&rest _) (signal outcome '("Fixture refused"))))
      (should (eq outcome
                  (condition-case err
                      (call-interactively (key-binding (kbd "RET")))
                    ((error quit) (car err)))))
      (should (equal "> λ\nanswer" (jabber-chat--input-string)))
      (should (equal "reply" jabber-message-reply--id))
      (should (equal "> λ\n" jabber-message-reply--fallback-text))
      (should-not jabber-chat--input-history))))

(ert-deftest jabber-test-chat-provider-omemo-correction-failure ()
  "An OMEMO correction failure never inserts correction text into a draft."
  (dolist (group '(nil t))
    (dolist (cancel '(nil t))
      (jabber-test-chat--journey
        (setq-local jabber-chat-encryption 'omemo)
        (when group (setq-local jabber-group "room@example.org"))
        (let ((jabber-muc-participants
               '(("room@example.org" ("peer" jid "peer@example.org/mobile"))))
              (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
              (failures 0) callback)
          (when group (jabber-test-chat--owned-omemo-room jc))
          (insert "successor draft λ")
          (cl-labels ((discover (_jc _peer cb)
                        (setq callback cb)
                        (when cancel (signal 'quit nil))))
            (cl-letf (((symbol-function 'jabber-omemo--ensure-sessions) #'discover)
                      ((symbol-function 'jabber-omemo--ensure-sessions-multi) #'discover))
              (condition-case nil
                  (funcall (if group #'jabber-muc-send #'jabber-chat-send)
                           jc "corrected λ"
                           '((replace ((id . "original"))))
                           #'ignore (lambda (_reason) (cl-incf failures)))
                (quit nil))))
          (funcall callback nil)
          (funcall callback nil)
          (should (= failures 1))
          (should-not (gethash jc jabber-omemo--pending-send-operations))
          (should (equal "successor draft λ" (jabber-chat--input-string))))))))

(ert-deftest jabber-test-chat-journey-receipt-inbound-boundaries ()
  "Native markers persist exact equal-second boundaries across reopen."
  (dolist (reverse '(nil t))
    (jabber-test-chat--journey
      (jabber-test-chat--with-db
        (rename-buffer (jabber-chat-get-buffer "peer@example.org" jc))
        (dolist (entry '(("older" . 99.75) ("first" . 100.125)
                         ("second" . 100.875) ("future" . 100.875)))
          (jabber-db-store-message "me@example.org" "peer@example.org"
                                   "out" "chat" (car entry) (floor (cdr entry)) nil (car entry))
          (jabber-db-update-receipt "me@example.org" "peer@example.org"
                                    (car entry) "delivered_at" 101)
          (jabber-chat-ewoc-enter
           (list :local (list :id (car entry) :body (car entry) :status :delivered
                              :timestamp (seconds-to-time (cdr entry))))))
        (dolist (scope '(("other@example.org" "peer@example.org")
                         ("me@example.org" "other@example.org")))
          (jabber-db-store-message (car scope) (cadr scope)
                                   "out" "chat" "first" 99 nil "first")
          (jabber-db-update-receipt (car scope) (cadr scope) "first" "delivered_at" 101))
        (cl-labels ((marker (id)
                      (jabber-receipts--handle-message
                       jc `(message ((from . "peer@example.org/mobile") (type . "chat"))
                                    (displayed ((xmlns . "urn:xmpp:chat-markers:0") (id . ,id))))))
                    (marked ()
                      (sqlite-select jabber-db--connection
                                     "SELECT account, peer, stanza_id FROM message WHERE displayed_at IS NOT NULL ORDER BY id")))
          (marker "missing")
          (should-not (marked))
          (unless reverse
            (marker "first")
            (let ((expected '(("me@example.org" "peer@example.org" "older")
                              ("me@example.org" "peer@example.org" "first"))))
              (should (equal expected (marked)))
              (should (eq :delivered
                          (plist-get (cadr (ewoc-data (jabber-chat-ewoc-find-by-id "second"))) :status)))
              (jabber-db-close)
              (jabber-db-ensure-open)
              (should (equal expected (marked)))))
          (marker "second")
          (let ((expected '(("me@example.org" "peer@example.org" "older")
                            ("me@example.org" "peer@example.org" "first")
                            ("me@example.org" "peer@example.org" "second")))
                (state (sqlite-select jabber-db--connection
                                      "SELECT id, delivered_at, displayed_at FROM message ORDER BY id")))
            (should (equal expected (marked)))
            (dolist (id '("first" "second" "older" "missing"))
              (marker id)
              (should (equal state (sqlite-select jabber-db--connection
                                                   "SELECT id, delivered_at, displayed_at FROM message ORDER BY id"))))
            (jabber-db-close)
            (jabber-db-ensure-open)
            (should (equal expected (marked)))))))))

(defmacro jabber-test-chat--publication-fixture (&rest body)
  "Run BODY with native chat/SQLite fixtures and observed correction sends.
Expose `publication-node', `publication-attempts', `publication-wire' and
`publication-messages'.  The journey fixture also binds the inert `jc'."
  (declare (indent 0) (debug t))
  `(jabber-test-chat--journey
     (jabber-test-chat--with-db
       (setq-local jabber-chat-send-hooks nil)
       (let* ((jabber-debug-log-xml nil)
              (jabber-sm-max-in-flight nil)
              (original-send (symbol-function 'jabber-message-correct--send))
              (publication-attempts nil)
              (publication-wire nil)
              (publication-messages nil)
              (publication-node
               (jabber-chat-ewoc-enter
                (list :local (list :id "original" :body "original"
                                   :from "me@example.org"
                                   :timestamp (list 0 100))))))
         (jabber-db-store-message "me@example.org" "peer@example.org"
                                  "out" "chat" "original" 100 nil "original")
         (put jc :name 'jabber-connection)
         (put jc :state-data
              (append (get jc :state-data)
                      (list :sm-enabled t :sm-outbound-count 0
                            :sm-last-acked 0 :sm-pending-queue nil)))
         (plist-put (get jc :state-data) :send-function
                    (lambda (_transport text) (push text publication-wire)))
         (goto-char (point-max))
         (insert "unrelated draft λ")
         (cl-letf (((symbol-function 'jabber-message-correct--send)
                    (lambda (connection group text extra
                             &optional success failure)
                      (push (list :token jabber-message-correct--pending-outgoing
                                  :success success :failure failure :body text)
                            publication-attempts)
                      (funcall original-send connection group text extra
                               success failure)))
                   ((symbol-function 'message)
                    (lambda (format-string &rest arguments)
                      (let ((text (and format-string
                                       (apply #'format format-string arguments))))
                        (when text (push text publication-messages))
                        text))))
           ,@body)))))

(defun jabber-test-chat--publication-correct (body)
  "Submit a correction with BODY through the interactive command."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) body)))
    (call-interactively #'jabber-correct-last-message)))

(defun jabber-test-chat--publication-drain (jc)
  "Drain JC through a real FSM acknowledgement and its native timer."
  (let ((jabber-sm-max-in-flight nil)
        (deadline (+ (float-time) 2)))
    (fsm-send-sync
     jc `(:stanza (a ((xmlns . ,jabber-sm-xmlns) (h . "0")))))
    ;; Wait for native queue removal, NOT a pending token a broken commit can
    ;; strand.  Return from accept-process-output also returns from its timer.
    (while (and (plist-get (get jc :state-data) :sm-pending-queue)
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should-not (plist-get (get jc :state-data) :sm-pending-queue))
    (should (eq :session-established (get jc :state)))))

(defun jabber-test-chat--publication-rows ()
  "Return complete fixture message rows, including edited metadata."
  (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id"))

(defun jabber-test-chat--publication-replay (attempt node)
  "Check late callbacks from ATTEMPT leave NODE and all local state intact."
  (let ((pending jabber-message-correct--pending-outgoing)
        (pending-active (car-safe jabber-message-correct--pending-outgoing))
        (rows (jabber-test-chat--publication-rows))
        (data (copy-tree (ewoc-data node)))
        (text (buffer-string)))
    (should (functionp (plist-get attempt :success)))
    (should (functionp (plist-get attempt :failure)))
    (funcall (plist-get attempt :success))
    (funcall (plist-get attempt :failure) "Late publication failure")
    (funcall (plist-get attempt :success))
    (should (eq pending jabber-message-correct--pending-outgoing))
    (should (eq pending-active (car-safe pending)))
    (should (equal rows (jabber-test-chat--publication-rows)))
    (should (equal data (ewoc-data node)))
    (should (equal text (buffer-string)))))

(defun jabber-test-chat--publication-check-committed (node body)
  "Check NODE, rendered text and reopened SQLite agree on BODY."
  (should-not jabber-message-correct--pending-outgoing)
  (should (equal body (plist-get (cadr (ewoc-data node)) :body)))
  (should (plist-get (cadr (ewoc-data node)) :edited))
  (should (string-search body
                         (buffer-substring-no-properties
                          (point-min) jabber-point-insert)))
  (should (equal "unrelated draft λ" (jabber-chat--input-string)))
  (should (equal (list (list body 1))
                 (sqlite-select jabber-db--connection
                                "SELECT body, edited FROM message")))
  (let ((rows (jabber-test-chat--publication-rows)))
    (jabber-db-close)
    (jabber-db-ensure-open)
    (should (equal rows (jabber-test-chat--publication-rows)))))

(defun jabber-test-chat--publication-sqlite-abort (queued)
  "Exercise a real SQLite abort after handoff, optionally QUEUED."
  (jabber-test-chat--publication-fixture
    (let ((rows (jabber-test-chat--publication-rows))
          (text (buffer-string)))
      (sqlite-execute
       jabber-db--connection
       "CREATE TRIGGER publication_abort BEFORE UPDATE OF body ON message BEGIN SELECT RAISE(ABORT, 'publication sqlite abort'); END")
      (let ((jabber-sm-max-in-flight (and queued 0)))
        (jabber-test-chat--publication-correct "corrected λ"))
      (let* ((attempt (car publication-attempts))
             (token (plist-get attempt :token)))
        (should token)
        (when queued
          (should (eq token jabber-message-correct--pending-outgoing))
          (should (car token))
          (should-not publication-wire)
          (should (equal rows (jabber-test-chat--publication-rows)))
          (should (equal text (buffer-string)))
          (jabber-test-chat--publication-drain jc))
        ;; Establish native handoff and terminal settlement BEFORE any replay.
        (should (= 1 (length publication-wire)))
        (should-not (car token))
        (should-not jabber-message-correct--pending-outgoing)
        (should (equal text (buffer-string)))
        (should (equal "original"
                       (plist-get (cadr (ewoc-data publication-node)) :body)))
        (should-not (plist-get (cadr (ewoc-data publication-node)) :edited))
        (should (equal rows (jabber-test-chat--publication-rows)))
        (should (cl-some (lambda (text)
                           (string-match-p "publication sqlite abort" text))
                         publication-messages))
        (jabber-db-close)
        (jabber-db-ensure-open)
        (should (equal rows (jabber-test-chat--publication-rows)))
        ;; Remove the failure first: stale success must be inert even when an
        ;; accidental second publication would now succeed.
        (sqlite-execute jabber-db--connection "DROP TRIGGER publication_abort")
        (jabber-test-chat--publication-replay attempt publication-node)
        (should (= 1 (length publication-wire)))
        (let ((jabber-sm-max-in-flight 0))
          (jabber-test-chat--publication-correct "retry λ"))
        (let ((retry-token jabber-message-correct--pending-outgoing))
          (should retry-token)
          (should-not (eq token retry-token))
          (should (car retry-token))
          (should (= 2 (length publication-attempts)))
          (jabber-test-chat--publication-replay attempt publication-node)
          (should (= 1 (length publication-wire)))
          (jabber-test-chat--publication-drain jc)
          (should-not (car retry-token)))
        (should (= 2 (length publication-wire)))
        (jabber-test-chat--publication-check-committed publication-node "retry λ")
        (jabber-test-chat--publication-replay attempt publication-node)
        (should (= 2 (length publication-wire)))))))

(ert-deftest jabber-test-chat-repair-publication-sqlite-immediate ()
  "Release an immediate handed-off correction after a real SQLite abort."
  (jabber-test-chat--publication-sqlite-abort nil))

(ert-deftest jabber-test-chat-repair-publication-sqlite-queued ()
  "Release a native FSM-drained correction after a real SQLite abort."
  (jabber-test-chat--publication-sqlite-abort t))

(defun jabber-test-chat--publication-render-fault (queued fault)
  "Exercise printer FAULT after handoff, optionally QUEUED."
  (jabber-test-chat--publication-fixture
    (let* ((render-calls 0)
           (pending-at-render 'not-called)
           (jabber-chat-printers
            (cons (lambda (msg _who mode)
                    (when (and (eq mode :insert)
                               (equal "corrected λ" (plist-get msg :body)))
                      (cl-incf render-calls)
                      (setq pending-at-render
                            jabber-message-correct--pending-outgoing)
                      (signal fault (and (eq fault 'error)
                                         (list "publication render fault")))))
                  jabber-chat-printers)))
      (let ((jabber-sm-max-in-flight (and queued 0)))
        (jabber-test-chat--publication-correct "corrected λ"))
      (let* ((attempt (car publication-attempts))
             (token (plist-get attempt :token)))
        (when queued
          (should (eq token jabber-message-correct--pending-outgoing))
          (should (car token))
          (should-not publication-wire)
          (should (zerop render-calls))
          (should (equal '(("original"))
                         (sqlite-select jabber-db--connection
                                        "SELECT body FROM message")))
          (jabber-test-chat--publication-drain jc))
        (should (= 1 (length publication-wire)))
        (should (= 1 render-calls))
        (should-not pending-at-render)
        (should-not (car token))
        (should-not jabber-message-correct--pending-outgoing)
        (should (equal "unrelated draft λ" (jabber-chat--input-string)))
        ;; SQLite precedes rendering: do not demand a fictitious rollback of
        ;; an already handed-off stanza or of the successful local SQL write.
        (should (equal '(("corrected λ" 1))
                       (sqlite-select jabber-db--connection
                                      "SELECT body, edited FROM message")))
        (should (cl-some (lambda (text)
                           (string-match-p
                            (if (eq fault 'quit) "[Qq]uit" "publication render fault")
                            text))
                         publication-messages))
        (jabber-db-close)
        (jabber-db-ensure-open)
        (should (equal '(("corrected λ" 1))
                       (sqlite-select jabber-db--connection
                                      "SELECT body, edited FROM message")))
        (jabber-test-chat--publication-replay attempt publication-node)
        (should (= 1 render-calls))
        (should (= 1 (length publication-wire)))
        (let ((jabber-sm-max-in-flight 0))
          (jabber-test-chat--publication-correct "retry λ"))
        (let ((retry-token jabber-message-correct--pending-outgoing))
          (should retry-token)
          (should (car retry-token))
          (should-not (eq token retry-token))
          (jabber-test-chat--publication-replay attempt publication-node)
          (jabber-test-chat--publication-drain jc)
          (should-not (car retry-token)))
        (should (= 2 (length publication-attempts)))
        (should (= 2 (length publication-wire)))
        (should (= 1 render-calls))
        (jabber-test-chat--publication-check-committed publication-node "retry λ")))))

(ert-deftest jabber-test-chat-repair-publication-render-error-immediate ()
  "Settle an immediate correction before a native printer raises error."
  (jabber-test-chat--publication-render-fault nil 'error))

(ert-deftest jabber-test-chat-repair-publication-render-quit-immediate ()
  "Settle an immediate correction before a native printer raises quit."
  (jabber-test-chat--publication-render-fault nil 'quit))

(ert-deftest jabber-test-chat-repair-publication-render-error-queued ()
  "Settle a native FSM-drained correction before a printer raises error."
  (jabber-test-chat--publication-render-fault t 'error))

(ert-deftest jabber-test-chat-repair-publication-render-quit-queued ()
  "Settle a native FSM-drained correction before a printer raises quit."
  (jabber-test-chat--publication-render-fault t 'quit))

(defun jabber-test-chat--publication-successor (fault)
  "Preserve a printer-admitted successor through optional publication FAULT."
  (dolist (queued '(nil t))
    (ert-info ((format "queued=%S fault=%S" queued fault))
      (jabber-test-chat--publication-fixture
        (let* ((hook-runs 0)
               (hook-error nil)
               (pending-at-hook 'not-called)
               (successor-token nil)
               (jabber-chat-printers
                (cons (lambda (msg _who mode)
                        (when (and (zerop hook-runs) (eq mode :insert)
                                   (equal "corrected λ" (plist-get msg :body)))
                          (cl-incf hook-runs)
                          (setq pending-at-hook
                                jabber-message-correct--pending-outgoing)
                          ;; Native rendering is a reentry boundary.  Admit a
                          ;; real new correction, not an invented pending list.
                          ;; Queue it so the old renderer resumes while the new
                          ;; occurrence still owns the same buffer's slot.
                          ;; Keep back-pressure active until the surrounding
                          ;; drain returns, or it would immediately consume the
                          ;; new entry too.  The caller binds this option, so
                          ;; changing it here cannot escape the test phase.
                          (setq jabber-sm-max-in-flight 0)
                          (condition-case err
                              (let ((jabber-sm-max-in-flight 0))
                                (jabber-test-chat--publication-correct "successor λ")
                                (setq successor-token
                                      jabber-message-correct--pending-outgoing))
                            ((error quit) (setq hook-error err)))
                          (when fault
                            (signal fault (and (eq fault 'error)
                                               (list "publication successor fault"))))))
                      jabber-chat-printers)))
          (let ((jabber-sm-max-in-flight (and queued 0)))
            (jabber-test-chat--publication-correct "corrected λ"))
          (let* ((attempt (car (last publication-attempts)))
                 (token (plist-get attempt :token)))
            (when queued
              (should-not publication-wire)
              ;; This drain intentionally leaves a reentrantly queued successor.
              ;; Wait for the hook, rather than draining every generation.
              (let ((jabber-sm-max-in-flight nil)
                    (deadline (+ (float-time) 2)))
                (fsm-send-sync
                 jc `(:stanza (a ((xmlns . ,jabber-sm-xmlns) (h . "0")))))
                (while (and (zerop hook-runs) (< (float-time) deadline))
                  (accept-process-output nil 0.01))))
            ;; Assertions stay outside callbacks production can demote.
            (should (= 1 hook-runs))
            (should-not hook-error)
            (should-not pending-at-hook)
            (should-not (car token))
            (should successor-token)
            (should-not (eq token successor-token))
            (should (car successor-token))
            (should (eq successor-token jabber-message-correct--pending-outgoing))
            (should (= 2 (length publication-attempts)))
            (should (= 1 (length publication-wire)))
            (should (= 1 (length (plist-get (get jc :state-data) :sm-pending-queue))))
            (should (equal "unrelated draft λ" (jabber-chat--input-string)))
            (should (equal '(("corrected λ" 1))
                           (sqlite-select jabber-db--connection
                                          "SELECT body, edited FROM message")))
            (jabber-test-chat--publication-replay attempt publication-node)
            (should (= 1 (length publication-wire)))
            (jabber-test-chat--publication-drain jc)
            (should-not (car successor-token))
            (should (= 2 (length publication-wire)))
            (jabber-test-chat--publication-check-committed publication-node "successor λ")
            (jabber-test-chat--publication-replay attempt publication-node)
            (should (= 2 (length publication-wire)))))))))

(ert-deftest jabber-test-chat-repair-publication-successor-render-return ()
  "Keep a native printer's new pending correction after old rendering returns."
  (jabber-test-chat--publication-successor nil))

(ert-deftest jabber-test-chat-repair-publication-successor-render-error ()
  "Keep a native printer's new pending correction after old rendering errors."
  (jabber-test-chat--publication-successor 'error))

(ert-deftest jabber-test-chat-repair-publication-successor-render-quit ()
  "Keep a native printer's new pending correction after old rendering quits."
  (jabber-test-chat--publication-successor 'quit))


(defmacro jabber-test-chat--modern-fixture (group &rest body)
  "Run BODY in a modern OpenPGP chat, with optional GROUP routing."
  (declare (indent 1) (debug (form body)))
  `(jabber-test-chat--journey
     (setq-local jabber-chat-encryption 'openpgp)
     (setq-local jabber-chat-send-hooks '(jabber-message-reply--send-hook))
     (when ,group
       (setq-local jabber-group "room@example.org")
       (setq-local jabber-send-function #'jabber-muc-send))
     (let ((jabber-muc-participants
            '(("room@example.org" ("peer" jid "peer@example.org/mobile")))))
       ,@body)))

(defmacro jabber-test-chat--delayed-keys (&rest body)
  "Run BODY with native IQ discovery and captured peer-key continuations."
  (declare (indent 0) (debug t))
  `(let ((jabber-open-info-queries nil)
         (jabber-debug-log-xml nil)
         (lookup (symbol-function 'jabber-openpgp--recipient-key))
         (missing t) continuations request-ids)
     (cl-letf (((symbol-function 'jabber-openpgp--recipient-key)
                (lambda (jid)
                  (unless (and missing (equal jid "peer@example.org"))
                    (funcall lookup jid))))
               ((symbol-function 'jabber-openpgp--fetch-key)
                (lambda (connection jid callback)
                  (push callback continuations)
                  (jabber-send-iq
                   connection jid "get" '(query ((xmlns . "test:key")))
                   (lambda (_jc _xml _closure)
                     (funcall callback (funcall lookup jid)))
                   nil nil nil)
                  (push (caar jabber-open-info-queries) request-ids))))
       ,@body)))

(defun jabber-test-chat--keyboard (keys)
  "Dispatch KEYS in the current chat using the actual command loop."
  (save-window-excursion
    (switch-to-buffer (current-buffer))
    (execute-kbd-macro keys)))

(ert-deftest jabber-test-chat-repair-modern-immediate-input ()
  "Real GPG RET preserves rejected input, metadata, point and native undo."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (group '(nil t))
       (dolist (outcome '(transport crypto key quit success queued))
         (ert-info ((format "group=%S outcome=%S" group outcome))
           (jabber-test-chat--modern-fixture group
             (setq-local jabber-message-reply--id "reply")
             (setq-local jabber-message-reply--jid "peer@example.org")
             (setq-local jabber-message-reply--thread '(:thread-id "thread"))
             (setq-local jabber-message-reply--fallback-text "> λ\n")
             (buffer-enable-undo)
             (setq buffer-undo-list nil)
             (jabber-test-chat--keyboard "draft λ")
             (jabber-test-chat--keyboard (kbd "C-b C-b"))
             (undo-boundary)
             (let* ((offset (- (point) jabber-point-insert))
                    (undo buffer-undo-list)
                    (transcript (buffer-substring (point-min) jabber-point-insert))
                    (encrypt (symbol-function 'epg-encrypt-string))
                    (jabber-sm-max-in-flight (and (eq outcome 'queued) 0))
                    wire)
               (put jc :name 'jabber-connection)
               (put jc :state-data
                    (append (get jc :state-data)
                            (list :sm-enabled t :sm-outbound-count 0 :sm-last-acked 0)))
               (plist-put (get jc :state-data) :send-function
                          (lambda (_transport text)
                            (if (eq outcome 'transport)
                                (error "Rejected native wire")
                              (push text wire))))
               (cl-letf (((symbol-function 'jabber-openpgp--fetch-key)
                          (lambda (_jc _jid callback) (funcall callback nil)))
                         ((symbol-function 'epg-encrypt-string)
                          (lambda (&rest args)
                            (pcase outcome
                              ('crypto (error "Rejected encryption"))
                              ('quit (signal 'quit nil))
                              (_ (apply encrypt args))))))
                 (let ((jabber-openpgp-key-alist
                        (if (eq outcome 'key)
                            (cons '("me@example.org" . "missing-key")
                                  jabber-openpgp-key-alist)
                          jabber-openpgp-key-alist)))
                   (condition-case nil
                       (jabber-test-chat--keyboard (kbd "RET"))
                     ((error quit) nil))))
               (if (memq outcome '(success queued))
                   (progn
                     (should (equal "" (jabber-chat--input-string)))
                     (when (eq outcome 'queued)
                       (should-not wire)
                       (should-not jabber-chat--input-history)
                       (jabber-test-chat--publication-drain jc))
                     (should (= 1 (length wire)))
                     (should (equal '("draft λ") jabber-chat--input-history))
                     (should-not jabber-message-reply--id))
                 (should-not wire)
                 (should (equal "draft λ" (jabber-chat--input-string)))
                 (should (= offset (- (point) jabber-point-insert)))
                 (should (equal undo buffer-undo-list))
                 (should-not jabber-chat--input-history)
                 (should (equal "reply" jabber-message-reply--id))
                 (should (equal '(:thread-id "thread") jabber-message-reply--thread))
                 (should (equal "> λ\n" jabber-message-reply--fallback-text))
                 (should (equal transcript (buffer-substring (point-min) jabber-point-insert)))
                 (jabber-test-chat--keyboard (kbd "C-/"))
                 (should (equal "" (jabber-chat--input-string)))
                 (jabber-test-chat--keyboard (kbd "C-?"))
                 (should (equal "draft λ" (jabber-chat--input-string)))
                 (should (equal transcript (buffer-substring (point-min) jabber-point-insert))))))))))))

(ert-deftest jabber-test-chat-repair-modern-delayed-input ()
  "Native key results settle failed drafts once without stealing successors."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (group '(nil t))
       (dolist (outcome '(error quit missing discard success))
         (dolist (successor '(nil t))
           (ert-info ((format "group=%S outcome=%S successor=%S" group outcome successor))
             (jabber-test-chat--modern-fixture group
               (setq-local jabber-message-reply--id "reply")
               (buffer-enable-undo)
               (setq buffer-undo-list nil)
               (insert "old draft λ")
               (backward-char 2)
               (let ((offset (- (point) jabber-point-insert)) wire)
                 (plist-put (get jc :state-data) :send-function
                            (lambda (_transport text) (push text wire)))
                 (put jc :name 'jabber-connection)
                 (put jc :state-data
                      (append (get jc :state-data)
                              (list :sm-enabled t :sm-outbound-count 0 :sm-last-acked 0)))
                 (jabber-test-chat--delayed-keys
                   (jabber-test-chat--keyboard (kbd "RET"))
                   (should request-ids)
                   (should-not jabber-chat--input-history)
                   (should (equal "" (jabber-chat--input-string)))
                   (setq wire nil missing nil)
                   (when successor
                     (insert "successor λ")
                     (setq-local jabber-message-reply--id "successor-reply"))
                   (plist-put (get jc :state-data) :send-function
                              (lambda (_transport text)
                                (pcase outcome
                                  ('error (error "Delayed wire error"))
                                  ('quit (signal 'quit nil))
                                  (_ (push text wire)))))
                   (let ((jabber-sm-max-in-flight (and (eq outcome 'discard) 0)))
                     (if (eq outcome 'missing)
                         (funcall (car continuations) nil)
                       (condition-case nil
                           (jabber-process-iq jc `(iq ((type . "result") (id . ,(car request-ids)))))
                         ((error quit) nil))))
                   (when (eq outcome 'discard)
                     (should (plist-get (get jc :state-data) :sm-pending-queue))
                     (put jc :state-data
                          (jabber-sm--discard-pending (get jc :state-data) "Queue discarded")))
                   (should (equal (if successor "successor λ"
                                    (if (eq outcome 'success) "" "old draft λ"))
                                  (jabber-chat--input-string)))
                   (should (equal (and (eq outcome 'success) '("old draft λ"))
                                  jabber-chat--input-history))
                   (unless (or successor (eq outcome 'success))
                     (should (= offset (- (point) jabber-point-insert)))
                     (should (equal "reply" jabber-message-reply--id)))
                   (when successor (should (equal "successor-reply" jabber-message-reply--id)))
                   (should (= (if (eq outcome 'success) 1 0) (length wire)))
                   (let ((text (buffer-string))
                         (history (copy-sequence jabber-chat--input-history)))
                     (funcall (car continuations) nil)
                     (funcall (car continuations) (funcall lookup "peer@example.org"))
                     (should (equal text (buffer-string)))
                     (should (equal history jabber-chat--input-history)))
                   ;; A fresh explicit RET remains usable after a failed operation.
                   (unless (eq outcome 'success)
                     (plist-put (get jc :state-data) :send-function
                                (lambda (_transport text) (push text wire)))
                     (jabber-test-chat--keyboard (kbd "RET"))
                     (should (= 1 (length wire)))
                     (should (equal "" (jabber-chat--input-string))))))))))))))

(ert-deftest jabber-test-chat-repair-modern-delayed-correction-quit ()
  "Native discovery followed by encryption/wire quit releases exact correction."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (group '(nil t))
       (dolist (boundary '(encryption transport))
         (jabber-test-chat--modern-fixture group
           (setq-local jabber-chat-send-hooks nil)
           (jabber-chat-ewoc-enter
            (list (if group :muc-local :local)
                  (list :id "original" :body "original" :timestamp '(0 100)
                        :from (if group "room@example.org/me" "me@example.org"))))
           (insert "successor composer")
           (let ((encrypt (symbol-function 'epg-encrypt-string)) wire)
             (plist-put (get jc :state-data) :send-function
                        (lambda (_transport text) (push text wire)))
             (jabber-test-chat--delayed-keys
               (jabber-test-chat--publication-correct "corrected λ")
               (let ((token jabber-message-correct--pending-outgoing))
                 (should token)
                 (setq missing nil wire nil)
                 (plist-put (get jc :state-data) :send-function
                            (lambda (_transport _text) (signal 'quit nil)))
                 (cl-letf (((symbol-function 'epg-encrypt-string)
                            (lambda (&rest args)
                              (if (eq boundary 'encryption) (signal 'quit nil)
                                (apply encrypt args)))))
                   (should (eq 'quit
                               (condition-case nil
                                   (jabber-process-iq jc `(iq ((type . "result") (id . ,(car request-ids)))))
                                 (quit 'quit)))))
                 (should-not (car token))
                 (should-not jabber-message-correct--pending-outgoing)
                 (plist-put (get jc :state-data) :send-function
                            (lambda (_transport text) (push text wire)))
                 (funcall (car continuations) nil)
                 (funcall (car continuations) (funcall lookup "peer@example.org"))
                 (should-not wire)
                 (should (equal "successor composer" (jabber-chat--input-string)))
                 (jabber-test-chat--publication-correct "corrected λ")
                 (should-not jabber-message-correct--pending-outgoing)
                 (should (= 1 (length wire)))
                 (jabber-test-chat--correction-wire (car wire) 'openpgp group))))))))))

(ert-deftest jabber-test-chat-repair-modern-draft-ownership ()
  "Incoming transcript preserves a failed draft; edited or retargeted input does not."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (change '(incoming empty-successor retarget mode))
       (jabber-test-chat--modern-fixture nil
         (buffer-enable-undo)
         (setq buffer-undo-list nil)
         (jabber-test-chat--keyboard "old draft λ")
         (undo-boundary)
         (jabber-test-chat--delayed-keys
           (jabber-test-chat--keyboard (kbd "RET"))
           (pcase change
             ('incoming
              (jabber-chat-ewoc-enter
               '(:foreign (:id "incoming" :body "Incoming message λ"
                           :from "peer@example.org" :timestamp (0 100)))))
             ('empty-successor
              (insert "new draft")
              (delete-region jabber-point-insert (point-max)))
             ('retarget (setq-local jabber-chatting-with "other@example.org"))
             ('mode (fundamental-mode)))
           (let ((text (buffer-string)))
             (funcall (car continuations) nil)
             (if (eq change 'incoming)
                 (progn
                   (should (equal "old draft λ" (jabber-chat--input-string)))
                   (jabber-test-chat--keyboard (kbd "C-/"))
                   (should (equal "" (jabber-chat--input-string)))
                   (should (equal text (buffer-string))))
               (should (equal text (buffer-string)))))
           (should-not jabber-chat--input-history)))))))

(ert-deftest jabber-test-chat-repair-modern-encryption-owner ()
  "Revalidate the destination after real encryption without touching a successor."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (group '(nil t))
       (jabber-test-chat--modern-fixture group
         (insert "old draft")
         (let ((encrypt (symbol-function 'epg-encrypt-string)) wire)
           (plist-put (get jc :state-data) :send-function
                      (lambda (_transport text) (push text wire)))
           (cl-letf (((symbol-function 'epg-encrypt-string)
                      (lambda (&rest args)
                        (prog1 (apply encrypt args)
                          (if group (setq-local jabber-group "other@example.org")
                            (setq-local jabber-chatting-with "other@example.org"))
                          (insert "new owner draft")
                          (setq-local jabber-message-reply--id "new-reply")))))
             (jabber-test-chat--keyboard (kbd "RET")))
           (should-not wire)
           (should-not jabber-chat--input-history)
           (should (equal "new owner draft" (jabber-chat--input-string)))
           (should (equal "new-reply" jabber-message-reply--id))))))))

(ert-deftest jabber-test-chat-repair-modern-correction-input-isolation ()
  "A nested correction cannot acquire another submission's completion owner."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (jabber-test-chat--modern-fixture nil
       (jabber-chat-ewoc-enter
        (list :local (list :id "original" :body "original"
                           :timestamp (list 0 100))))
       (let ((calls 0)
             (jabber-chat--input-deferred nil))
         (let ((jabber-chat--input-completion (lambda (&rest _) (cl-incf calls))))
           (jabber-test-chat--publication-correct "corrected λ"))
         (should-not jabber-message-correct--pending-outgoing)
         (should-not jabber-chat--input-deferred)
         (should (zerop calls)))))))

(defun jabber-test-chat--r2-enable-sm (jc wire)
  "Enable native SM on JC and use WIRE at its transport boundary."
  (put jc :name 'jabber-connection)
  (put jc :state-data
       (append (get jc :state-data)
               (list :sm-enabled t :sm-outbound-count 0 :sm-last-acked 0)))
  (plist-put (get jc :state-data) :send-function wire))

(ert-deftest jabber-test-chat-r2-post-handoff-publication ()
  "Settle real RET before printer or local-message error/quit on either provider."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (provider '(plaintext openpgp))
       (dolist (queued '(nil t))
         (dolist (site '(printp insert hook success))
           (dolist (fault (if (eq site 'success) '(nil) '(error quit)))
             (ert-info ((format "%S queued=%S site=%S fault=%S"
                                provider queued site fault))
               (jabber-test-chat--modern-fixture nil
                 (setq-local jabber-chat-encryption provider)
                 (setq-local jabber-message-reply--id "r2-reply")
                 (setq-local jabber-message-reply--jid "peer@example.org")
                 (let* (wire reports at-publication
                        (trip
                         (lambda ()
                           (push (list (jabber-chat--input-string)
                                       (copy-sequence jabber-chat--input-history)
                                       jabber-message-reply--id
                                       (car jabber-chat--input-submission))
                                 at-publication)
                           (signal fault (and (eq fault 'error)
                                              '("R2 publication fault")))))
                        (jabber-chat-printers
                         (cons (lambda (_msg who mode)
                                 (when (and (eq who :local)
                                            (eq mode (if (eq site 'insert)
                                                         :insert :printp))
                                            (memq site '(insert printp)))
                                   (funcall trip)))
                               jabber-chat-printers))
                        (jabber-chat-local-message-functions
                         (and (eq site 'hook)
                              (list (lambda (_msg) (funcall trip))))))
                   (jabber-test-chat--r2-enable-sm
                    jc (lambda (_transport text) (push text wire)))
                   (insert "r2 sent λ")
                   (cl-letf (((symbol-function 'message)
                              (lambda (format-string &rest args)
                                (when format-string
                                  (push (apply #'format format-string args) reports)))))
                     (let ((jabber-sm-max-in-flight (and queued 0)))
                       (jabber-test-chat--keyboard (kbd "RET")))
                     (let* ((token jabber-chat--input-submission)
                            (entry (car (plist-get (get jc :state-data)
                                                   :sm-pending-queue))))
                       (when queued
                         (should (car token))
                         (should-not wire)
                         (should-not jabber-chat--input-history)
                         (should-not (ewoc-nth jabber-chat-ewoc 0))
                         (jabber-test-chat--publication-drain jc))
                       (should (= 1 (length wire)))
                       (should-not (car token))
                       (should (equal "" (jabber-chat--input-string)))
                       (should (equal '("r2 sent λ") jabber-chat--input-history))
                       (should-not jabber-message-reply--id)
                       (if fault
                           (progn
                             (should (equal '(("" ("r2 sent λ") nil nil))
                                            at-publication))
                             (should (cl-some
                                      (lambda (report)
                                        (string-match-p
                                         (if (eq fault 'quit) "[Qq]uit"
                                           "R2 publication fault") report))
                                      reports)))
                         (should (equal "r2 sent λ"
                                        (plist-get
                                         (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc 0)))
                                         :body)))
                         (should-not (ewoc-nth jabber-chat-ewoc 1)))
                       ;; No implicit retry, even after the local fault is gone.
                       (let ((jabber-chat-printers (cdr jabber-chat-printers))
                             (jabber-chat-local-message-functions nil)
                             (before (buffer-string)))
                         (jabber-test-chat--keyboard (kbd "RET"))
                         (when entry
                           (funcall (plist-get entry :success))
                           (funcall (plist-get entry :failure) "Late failure"))
                         (should (equal before (buffer-string)))
                         (should (= 1 (length wire))))))))))))))))

(defun jabber-test-chat--r2-replace-owner (change)
  "Apply owner CHANGE using native mode/setup for a replacement lifetime."
  (pcase change
    ('account
     (setq-local jabber-buffer-connection
                 (jabber-test-chat--journey-connection "new-account")))
    ('account-data
     (plist-put (get jabber-buffer-connection :state-data) :username "new-account"))
    ('peer (setq-local jabber-chatting-with "new-peer@example.org"))
    ('marker (setq-local jabber-point-insert (copy-marker jabber-point-insert)))
    ('detached (set-marker jabber-point-insert nil))
    ('mode (fundamental-mode))
    ('setup
     (let ((inhibit-read-only t)) (erase-buffer))
     (jabber-chat-mode)
     (setq-local jabber-chatting-with "new-peer@example.org")
     (jabber-chat-mode-setup
      (jabber-test-chat--journey-connection "new-account") #'jabber-chat-pp))
    ('rename (rename-buffer (generate-new-buffer-name "r2-renamed"))))
  (goto-char (point-max))
  (insert "successor draft λ")
  (setq-local jabber-chat--input-history (list "successor history")))

(ert-deftest jabber-test-chat-r2-queued-owner ()
  "Fence both providers' queued echoes after all captured owner replacements."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (provider '(plaintext openpgp))
       (dolist (change '(account account-data peer marker detached mode setup rename))
         (ert-info ((format "%S owner=%S" provider change))
           (jabber-test-chat--modern-fixture nil
             (setq-local jabber-chat-encryption provider)
             (setq-local jabber-chat-send-hooks nil)
             (let (wire)
               (jabber-test-chat--r2-enable-sm
                jc (lambda (_transport text) (push text wire)))
               (insert "old secret λ")
               (let ((jabber-sm-max-in-flight 0))
                 (jabber-test-chat--keyboard (kbd "RET")))
               (let* ((token jabber-chat--input-submission)
                      (entry (car (plist-get (get jc :state-data) :sm-pending-queue))))
                 (should (car token))
                 (should entry)
                 (jabber-test-chat--r2-replace-owner change)
                 (let ((before (buffer-string)))
                   (jabber-test-chat--publication-drain jc)
                   (should (= 1 (length wire)))
                   (should-not (car token))
                   (if (eq change 'rename)
                       (progn
                         (should (equal '("old secret λ" "successor history")
                                        jabber-chat--input-history))
                         (should (equal "successor draft λ" (jabber-chat--input-string)))
                         (should (equal "old secret λ"
                                        (plist-get (cadr (ewoc-data
                                                          (ewoc-nth jabber-chat-ewoc 0)))
                                                   :body))))
                     (should (equal before (buffer-string)))
                     (should (equal '("successor history") jabber-chat--input-history)))
                   (let ((settled (buffer-string))
                         (history (copy-sequence jabber-chat--input-history)))
                     (funcall (plist-get entry :success))
                     (funcall (plist-get entry :failure) "Late failure")
                     (should (equal settled (buffer-string)))
                     (should (equal history jabber-chat--input-history))
                     (should (= 1 (length wire))))))))))))))

(ert-deftest jabber-test-chat-r2-completion-replaces-owner ()
  "Revalidate after completion and caller callbacks, not just before settlement."
  (jabber-test-chat--send-key-fixture
   (lambda ()
     (dolist (provider '(plaintext openpgp))
       (dolist (queued '(nil t))
         (dolist (boundary '(completion success))
           (ert-info ((format "%S queued=%S boundary=%S" provider queued boundary))
             (jabber-test-chat--modern-fixture nil
               (setq-local jabber-chat-encryption provider)
               (setq-local jabber-chat-send-hooks nil)
               (let (wire before token (calls 0))
                 (jabber-test-chat--r2-enable-sm
                  jc (lambda (_transport text) (push text wire)))
                 (setq-local
                  jabber-send-function
                  (lambda (connection body)
                    (setq token jabber-chat--input-submission)
                    (let* ((completion jabber-chat--input-completion)
                           (replace
                            (lambda ()
                              (cl-incf calls)
                              (jabber-test-chat--r2-replace-owner 'setup)
                              (setq before (buffer-string))))
                           (jabber-chat--input-completion
                            (if (eq boundary 'completion)
                                (lambda (success)
                                  (prog1 (funcall completion success)
                                    (when success (funcall replace))))
                              completion)))
                      (jabber-chat-send connection body nil
                                        (and (eq boundary 'success) replace)))))
                 (insert "old secret λ")
                 (let ((jabber-sm-max-in-flight (and queued 0)))
                   (jabber-test-chat--keyboard (kbd "RET")))
                 (let ((entry (car (plist-get (get jc :state-data) :sm-pending-queue))))
                   (when queued
                     (should (car token))
                     (should-not wire)
                     (should (zerop calls))
                     (jabber-test-chat--publication-drain jc))
                   (should (= 1 calls))
                   (should (= 1 (length wire)))
                   (should-not (car token))
                   (should (equal before (buffer-string)))
                   (should (equal '("successor history") jabber-chat--input-history))
                   (should (equal "successor draft λ" (jabber-chat--input-string)))
                   (when entry
                     (funcall (plist-get entry :success))
                     (funcall (plist-get entry :failure) "Late failure")
                     (should (= 1 calls))
                     (should (equal before (buffer-string))))))))))))))

(ert-deftest jabber-test-chat-r2-plaintext-queue-rejection ()
  "Recover definite queue failure only into its untouched composer."
  (dolist (outcome '(drain discard))
    (dolist (successor '(nil t))
      (jabber-test-chat--journey
        (setq-local jabber-chat-send-hooks '(jabber-message-reply--send-hook))
        (setq-local jabber-message-reply--id "old-reply")
        (setq-local jabber-message-reply--jid "peer@example.org")
        (let ((attempts 0))
          (jabber-test-chat--r2-enable-sm
           jc (lambda (_transport _text)
                (cl-incf attempts) (error "R2 wire refusal")))
          (insert "rejected λ")
          (let ((jabber-sm-max-in-flight 0))
            (jabber-test-chat--keyboard (kbd "RET")))
          (let ((token jabber-chat--input-submission)
                (entry (car (plist-get (get jc :state-data) :sm-pending-queue))))
            (should (car token))
            (should-not jabber-message-reply--id)
            (should-not jabber-chat--input-history)
            (when successor (insert "new draft"))
            (pcase outcome
              ('drain
               (let ((jabber-sm-max-in-flight nil)
                     (deadline (+ (float-time) 2)))
                 (fsm-send-sync
                  jc `(:stanza (a ((xmlns . ,jabber-sm-xmlns) (h . "0")))))
                 (while (and (car token) (< (float-time) deadline))
                   (accept-process-output nil 0.01)))
               (should-not (get jc :state)))
              ('discard (jabber-sm--discard-pending (get jc :state-data) "Discarded")))
            (should-not (car token))
            (should (= attempts (if (eq outcome 'drain) 1 0)))
            (should (equal (if successor "new draft" "rejected λ")
                           (jabber-chat--input-string)))
            (should (equal (unless successor "old-reply") jabber-message-reply--id))
            (should-not jabber-chat--input-history)
            (should-not (ewoc-nth jabber-chat-ewoc 0))
            (let ((before (buffer-string)))
              (funcall (plist-get entry :success))
              (funcall (plist-get entry :failure) "Late failure")
              (should (equal before (buffer-string)))
              (should-not jabber-chat--input-history))))))))

;;; OMEMO native discovery ownership

(defun jabber-test-chat--omemo-iq-error (id)
  "Return a device discovery error for query ID."
  `(iq ((type . "error") (id . ,id) (from . "peer@example.org"))
       (error ((type . "cancel"))
              (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))

(defun jabber-test-chat--omemo-input-state ()
  "Return exact composer text, reply, history and undo state."
  (list (if (and (markerp jabber-point-insert)
                 (eq (marker-buffer jabber-point-insert) (current-buffer)))
            (jabber-chat--input-string)
          (buffer-string))
        (jabber-chat--send-context-state nil)
        (copy-tree jabber-chat--input-history)
        (copy-tree buffer-undo-list)))

(ert-deftest jabber-test-chat-r3-omemo-native-discovery-failure ()
  "Native RET, PubSub and FSM failure preserve each successor composer."
  (require 'jabber-omemo)
  (dolist (change '(nil draft cleared submission mode setup ewoc deleted))
    (ert-info ((format "OMEMO discovery failure change=%S" change))
      (jabber-test-chat--journey
       (jabber-test-chat--with-db
        (let ((jabber-open-info-queries nil)
              (jabber-debug-log-xml nil)
              (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
              (jabber-omemo--device-lists (make-hash-table :test #'equal))
              (jabber-omemo--sessions (make-hash-table :test #'equal))
              wire)
          (put jc :name 'jabber-connection)
          (plist-put (get jc :state-data) :blocking-status 'ready)
          (plist-put (get jc :state-data) :send-function
                     (lambda (_transport text) (push text wire)))
          (setq-local jabber-chat-encryption 'omemo)
          (setq-local jabber-chat-send-hooks '(jabber-message-reply--send-hook))
          (setq-local jabber-message-reply--id "old-reply")
          (setq-local jabber-message-reply--jid "peer@example.org")
          (buffer-enable-undo)
          (insert "old secret λ")
          (jabber-test-chat--keyboard (kbd "RET"))
          (should (= (length wire) 1))
          (should (string-match-p "<iq" (car wire)))
          (should (string-match-p "eu.siacs.conversations.axolotl.devicelist" (car wire)))
          (should (= (length jabber-open-info-queries) 1))
          (should-not jabber-message-reply--id)
          (should (equal "" (jabber-chat--input-string)))
          (let* ((response (jabber-test-chat--omemo-iq-error (caar jabber-open-info-queries)))
                 (operation (car (gethash jc jabber-omemo--pending-send-operations)))
                 (token jabber-chat--input-submission)
                 (node (ewoc-nth jabber-chat-ewoc -1)))
            (pcase change
              ((or 'draft 'cleared)
               (insert "successor draft λ")
               (when (eq change 'cleared)
                 (delete-region jabber-point-insert (point-max))))
              ('submission
               ;; A later successful RET owns even the empty composer.
               (setq-local jabber-chat-encryption 'plaintext)
               (insert "second submission λ")
               (jabber-test-chat--keyboard (kbd "RET")))
              ((or 'mode 'setup)
               (jabber-test-chat--r2-replace-owner change))
              ('ewoc
               (let ((inhibit-read-only t))
                 (setq-local jabber-chat-ewoc (ewoc-create #'ignore)))
               (goto-char (point-max))
               (insert "successor draft λ"))
              ('deleted (jabber-chat-ewoc-delete node)))
            (when (memq change '(draft mode setup ewoc))
              (setq-local jabber-message-reply--id "successor-reply")
              (setq-local jabber-message-reply--jid "successor@example.org"))
            (let ((before (jabber-test-chat--omemo-input-state))
                  (wire-count (length wire)))
              (fsm-send-sync jc (list :stanza response))
              (should-not (car token))
              (should-not jabber-open-info-queries)
              (should-not (gethash jc jabber-omemo--pending-send-operations))
              (should (= wire-count (length wire)))
              (if (memq change '(nil deleted))
                  (progn
                    (should (equal "old secret λ" (jabber-chat--input-string)))
                    (should (equal "old-reply" jabber-message-reply--id))
                    (should-not jabber-chat--input-history)
                    (when (null change)
                      (let ((transcript (buffer-substring (point-min) jabber-point-insert)))
                        (undo-boundary)
                        (jabber-test-chat--keyboard (kbd "C-/"))
                        (should (equal "" (jabber-chat--input-string)))
                        (should (equal transcript
                                       (buffer-substring (point-min) jabber-point-insert))))))
                (should (equal before (jabber-test-chat--omemo-input-state))))
              (unless (memq change '(mode setup ewoc deleted))
                (should (eq :undelivered (plist-get (cadr (ewoc-data node)) :status))))
              ;; Native duplicate IQ and both retained provider completions are inert.
              (let ((settled (buffer-string))
                    (state (jabber-test-chat--omemo-input-state))
                    (wire-count (length wire)))
                (fsm-send-sync jc (list :stanza response))
                (jabber-omemo--send-operation-finish operation 'failure "late")
                (jabber-omemo--send-operation-finish operation 'success)
                (should (equal settled (buffer-string)))
                (should (equal state (jabber-test-chat--omemo-input-state)))
                (should (= wire-count (length wire))))))))))))

(ert-deftest jabber-test-chat-r3-omemo-native-handoff ()
  "Native discovery, crypto and immediate/queued handoff settle input once."
  (require 'jabber-omemo)
  (dolist (queued '(nil t))
    (jabber-test-chat--journey
     (jabber-test-chat--with-db
      (let* ((jabber-omemo--device-ids (make-hash-table :test #'equal))
             (jabber-omemo--stores (make-hash-table :test #'equal))
             (jabber-omemo--device-lists (make-hash-table :test #'equal))
             (jabber-omemo--sessions (make-hash-table :test #'equal))
             (jabber-open-info-queries nil)
             (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
             (peer-store (jabber-omemo-deserialize-store (jabber-omemo-setup-store)))
             (bundle (jabber-omemo-get-bundle peer-store))
             wire)
        (put jc :name 'jabber-connection)
        (plist-put (get jc :state-data) :blocking-status 'ready)
        (jabber-test-chat--r2-enable-sm
         jc (lambda (_transport text) (push text wire)))
        (plist-put (get jc :state-data) :sm-inbound-count 0)
        (jabber-omemo--establish-session jc "peer@example.org" 23 bundle)
        (puthash (jabber-omemo--device-list-key "me@example.org" "me@example.org")
                 (list (jabber-omemo--get-device-id jc)) jabber-omemo--device-lists)
        (setq-local jabber-chat-encryption 'omemo)
        (setq-local jabber-chat-send-hooks '(jabber-message-reply--send-hook))
        (setq-local jabber-message-reply--id "old-reply")
        (setq-local jabber-message-reply--jid "peer@example.org")
        (insert "old secret λ")
        (jabber-test-chat--keyboard (kbd "RET"))
        (should (= (length wire) 1))
        (should (string-match-p "<iq" (car wire)))
        (should (= (length jabber-open-info-queries) 1))
        (should-not jabber-chat--input-history)
        (let ((operation (car (gethash jc jabber-omemo--pending-send-operations)))
              (token jabber-chat--input-submission)
              (id (caar jabber-open-info-queries)))
          (let ((jabber-sm-max-in-flight (and queued 0)))
            (fsm-send-sync
             jc `( :stanza
                   (iq ((type . "result") (id . ,id) (from . "peer@example.org"))
                       (pubsub ((xmlns . "http://jabber.org/protocol/pubsub"))
                               (items ((node . "eu.siacs.conversations.axolotl.devicelist"))
                                      (item ((id . "current"))
                                            (list ((xmlns . "eu.siacs.conversations.axolotl"))
                                                  (device ((id . "23")))))))))))
          (when queued
            (should (car token))
            (should-not jabber-chat--input-history)
            (should (= (length wire) 1))
            (jabber-test-chat--publication-drain jc))
          (should-not (car token))
          (should-not jabber-open-info-queries)
          (should-not (gethash jc jabber-omemo--pending-send-operations))
          (let* ((messages (seq-filter (lambda (text) (string-match-p "<message" text)) wire))
                 (stanza (with-temp-buffer
                           (insert "<stream>" (car messages) "</stream>")
                           (car (jabber-xml-get-children
                                 (car (xml-parse-region (point-min) (point-max)))
                                 'message))))
                 (encrypted (jabber-xml-child-with-xmlns stanza jabber-omemo-xmlns))
                 (header (car (jabber-xml-get-children encrypted 'header)))
                 (key (car (jabber-xml-get-children header 'key)))
                 (iv (car (jabber-xml-get-children header 'iv)))
                 (payload (car (jabber-xml-get-children encrypted 'payload)))
                 (decrypted-key
                  (jabber-omemo-decrypt-key
                   (jabber-omemo-make-session) peer-store t
                   (base64-decode-string (car (jabber-xml-node-children key))))))
            (should (= (length messages) 1))
            (should (equal "old-reply"
                           (jabber-xml-get-attribute
                            (jabber-xml-child-with-xmlns stanza "urn:xmpp:reply:0") 'id)))
            (should (equal "old secret λ"
                           (decode-coding-string
                            (jabber-omemo-decrypt-message
                             decrypted-key
                             (base64-decode-string (car (jabber-xml-node-children iv)))
                             (base64-decode-string (car (jabber-xml-node-children payload))))
                            'utf-8))))
          (should (equal '("old secret λ") jabber-chat--input-history))
          (should (equal "" (jabber-chat--input-string)))
          (should-not jabber-message-reply--id)
          (should (eq :sent (plist-get (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc -1))) :status)))
          (let ((state (jabber-test-chat--omemo-input-state))
                (count (length wire)))
            (jabber-omemo--send-operation-finish operation 'success)
            (jabber-omemo--send-operation-finish operation 'failure "late")
            (jabber-test-chat--keyboard (kbd "RET"))
            (should (= count (length wire)))
            (should (equal state (jabber-test-chat--omemo-input-state))))))))))

(ert-deftest jabber-test-chat-r3-omemo-quit-successor ()
  "A discovery quit and its retained callbacks cannot recover over an edit."
  (require 'jabber-omemo)
  (require 'jabber-muc)
  (dolist (group '(nil t))
    (dolist (fault '(error quit))
      (dolist (successor '(nil draft cleared))
        (jabber-test-chat--journey
         (let ((jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
               (jabber-muc-participants '(("room@example.org" ("peer" jid "peer@example.org"))))
               continuation)
           (when group (jabber-test-chat--owned-omemo-room jc))
           (when group
             (setq-local jabber-group "room@example.org")
             (setq-local jabber-send-function #'jabber-muc-send))
           (setq-local jabber-chat-encryption 'omemo)
           (setq-local jabber-message-reply--id "old-reply")
           (setq-local jabber-message-reply--jid "peer@example.org")
           (insert "old secret λ")
           (cl-letf (((symbol-function 'jabber-omemo--ensure-sessions)
                      (lambda (_jc _peer callback)
                        (setq continuation callback)
                        (when successor
                          (insert "successor")
                          (when (eq successor 'cleared)
                            (delete-region jabber-point-insert (point-max))))
                        (signal fault '("discovery interruption")))))
             (if (eq fault 'quit)
                 (should (eq 'quit (condition-case nil
                                       (call-interactively (key-binding (kbd "RET")))
                                     (quit 'quit))))
               (jabber-test-chat--keyboard (kbd "RET"))))
           (should-not (gethash jc jabber-omemo--pending-send-operations))
           (should (equal (pcase successor ('draft "successor") ('cleared "") (_ "old secret λ"))
                          (jabber-chat--input-string)))
           (should (equal (unless successor "old-reply") jabber-message-reply--id))
           (should-not jabber-chat--input-history)
           (let ((state (jabber-test-chat--omemo-input-state)))
             (funcall continuation nil)
             (funcall continuation '((23 . stale-session)))
             (should (equal state (jabber-test-chat--omemo-input-state))))))))))

(defun jabber-test-chat--r4-deferred-omemo (outcome change &optional group)
  "Exercise native delayed OMEMO OUTCOME across composer CHANGE.
Non-nil GROUP selects a MUC send through the same native discovery."
  (require 'jabber-omemo)
  (jabber-test-chat--journey
   (jabber-test-chat--with-db
    (let* ((jabber-omemo--device-ids (make-hash-table :test #'equal))
           (jabber-omemo--stores (make-hash-table :test #'equal))
           (jabber-omemo--device-lists (make-hash-table :test #'equal))
           (jabber-omemo--sessions (make-hash-table :test #'equal))
           (jabber-open-info-queries nil)
           (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
           (peer-store (jabber-omemo-deserialize-store (jabber-omemo-setup-store)))
           (bundle (jabber-omemo-get-bundle peer-store))
           (other (make-symbol "other-account"))
           (origin (current-buffer))
           (jabber-chat-buffer-format (buffer-name))
           (jabber-chat-default-encryption 'plaintext)
           (jabber-chat-mode-hook nil)
           (jabber-muc-participants
            '(("room@example.org" ("peer" jid "peer@example.org"))))
           (next-hooks 0)
           before wire)
      (put other :state-data (list :username "other" :server "example.org"))
      (put jc :name 'jabber-connection)
      (plist-put (get jc :state-data) :blocking-status 'ready)
      (jabber-test-chat--r2-enable-sm
       jc (lambda (_transport text)
            (when (and (eq outcome 'refuse) (string-match-p "<message" text))
              (error "Test transport refusal"))
            (push text wire)))
      (plist-put (get jc :state-data) :sm-inbound-count 0)
      (jabber-omemo--establish-session jc "peer@example.org" 23 bundle)
      (puthash (jabber-omemo--device-list-key "me@example.org" "me@example.org")
               (list (jabber-omemo--get-device-id jc)) jabber-omemo--device-lists)
      (setq-local jabber-chat-encryption 'omemo)
      (setq-local jabber-message-reply--id "old-reply")
      (setq-local jabber-message-reply--jid "peer@example.org")
      (setq-local jabber-message-reply--thread '(:thread-id "old-thread"))
      (when group
        (setq-local jabber-group "room@example.org")
        (setq-local jabber-send-function #'jabber-muc-send)
        (jabber-test-chat--owned-omemo-room jc))
      (setq-local jabber-chat-send-hooks '(jabber-message-reply--send-hook))
      (buffer-enable-undo)
      (jabber-test-chat--keyboard "old body")
      (jabber-test-chat--keyboard (kbd "RET"))
      (should (= 1 (length wire)))
      (should-not jabber-chat--input-history)
      (let* ((id (caar jabber-open-info-queries))
             (response
              `(iq ((type . "result") (id . ,id) (from . "peer@example.org"))
                   (pubsub ((xmlns . "http://jabber.org/protocol/pubsub"))
                           (items ((node . "eu.siacs.conversations.axolotl.devicelist"))
                                  (item ((id . "current"))
                                        (list ((xmlns . "eu.siacs.conversations.axolotl"))
                                              (device ((id . "23")))))))))
             (operation (car (gethash jc jabber-omemo--pending-send-operations)))
             (token jabber-chat--input-submission)
             (successor
              (lambda ()
                (when (memq change '(retarget hook-retarget))
                  (jabber-chat-create-buffer other "other-peer@example.org"))
                (goto-char (point-max))
                (insert "successor body")
                (setq-local jabber-message-reply--id "successor-reply")
                (setq-local jabber-message-reply--jid "successor@example.org")
                (setq-local jabber-message-reply--thread '(:thread-id "successor-thread"))
                (setq-local jabber-message-thread--root-reply-id "successor-root")
                ;; Native queue draining yields to the command loop; establish
                ;; its undo boundary now instead of racing the undo timer.
                (undo-boundary)
                (setq before (jabber-test-chat--omemo-input-state)))))
        (if (memq change '(hook-draft hook-retarget))
            (setq-local jabber-chat-send-hooks
                        (list (lambda (&rest _)
                                (funcall successor)
                                (when (memq outcome '(error quit))
                                  (signal outcome '("Test hook interruption"))))
                              (lambda (&rest _) (cl-incf next-hooks) nil)
                              #'jabber-message-reply--send-hook
                              #'jabber-message-thread--send-hook))
          (funcall successor))
        (let ((jabber-sm-max-in-flight (and (memq outcome '(queued discard)) 0))
              (debug-on-quit nil)
              quit-seen)
          (condition-case err
              (fsm-send-sync jc (list :stanza response))
            (quit (setq quit-seen t)
                  (unless (eq outcome 'quit) (signal (car err) (cdr err)))))
          (should (eq quit-seen (eq outcome 'quit))))
        (when (memq outcome '(queued discard))
          (should (= 1 (length (plist-get (get jc :state-data) :sm-pending-queue))))
          (should (car token))
          (should-not jabber-chat--input-history)
          (if (eq outcome 'discard)
              (jabber-sm--discard-pending (get jc :state-data) "Test discard")
            (jabber-test-chat--publication-drain jc)))
        (with-current-buffer origin
          (should-not (car token))
          (should-not (gethash jc jabber-omemo--pending-send-operations))
          (let ((sent (and (memq outcome '(success queued))
                           (not (memq change '(retarget hook-retarget))))))
            (should (= (length wire) (if sent 2 1)))
            (should (equal (nth 0 before) (jabber-chat--input-string)))
            (should (equal (nth 1 before) (jabber-chat--send-context-state nil)))
            (should (equal (nth 3 before) buffer-undo-list))
            (should (equal (and sent '("old body")) jabber-chat--input-history))
            (when sent
              (let* ((stanza (with-temp-buffer
                               (insert "<stream>" (car wire) "</stream>")
                               (car (jabber-xml-get-children
                                     (car (xml-parse-region (point-min) (point-max)))
                                     'message))))
                     (encrypted (jabber-xml-child-with-xmlns stanza jabber-omemo-xmlns))
                     (header (car (jabber-xml-get-children encrypted 'header)))
                     (key (car (jabber-xml-get-children header 'key)))
                     (iv (car (jabber-xml-get-children header 'iv)))
                     (payload (car (jabber-xml-get-children encrypted 'payload)))
                     (decrypted-key
                      (jabber-omemo-decrypt-key
                       (jabber-omemo-make-session) peer-store t
                       (base64-decode-string (car (jabber-xml-node-children key))))))
                (should (equal "old-reply"
                               (jabber-xml-get-attribute
                                (jabber-xml-child-with-xmlns stanza "urn:xmpp:reply:0") 'id)))
                (should (equal "old-thread"
                               (plist-get (jabber-message-thread-protocol-fields stanza) :thread-id)))
                (should (equal "old body"
                               (decode-coding-string
                                (jabber-omemo-decrypt-message
                                 decrypted-key
                                 (base64-decode-string (car (jabber-xml-node-children iv)))
                                 (base64-decode-string (car (jabber-xml-node-children payload))))
                                'utf-8)))))))
          (when (eq change 'hook-retarget) (should (zerop next-hooks)))
          (let ((state (jabber-test-chat--omemo-input-state))
                (transcript (buffer-string))
                (count (length wire)))
            (fsm-send-sync jc (list :stanza response))
            (jabber-omemo--send-operation-finish operation 'success)
            (jabber-omemo--send-operation-finish operation 'failure "duplicate")
            (should (= count (length wire)))
            (should (equal transcript (buffer-string)))
            (should (equal state (jabber-test-chat--omemo-input-state)))))))))

(ert-deftest jabber-test-chat-r4-omemo-captured-context ()
  "Deferred success, queue discard and refusal preserve successor state."
  (dolist (outcome '(success queued discard refuse))
    (ert-info ((format "outcome=%S" outcome))
      (jabber-test-chat--r4-deferred-omemo outcome 'draft))))

(ert-deftest jabber-test-chat-r4-omemo-muc-captured-context ()
  "Native MUC discovery likewise uses captured reply and thread extensions."
  (dolist (outcome '(success queued discard refuse))
    (ert-info ((format "MUC outcome=%S" outcome))
      (jabber-test-chat--r4-deferred-omemo outcome 'draft t))))

(ert-deftest jabber-test-chat-r4-omemo-hook-reentry ()
  "Callback edits survive success, error and quit without dynamic shadowing."
  (dolist (outcome '(success error quit))
    (ert-info ((format "outcome=%S" outcome))
      (jabber-test-chat--r4-deferred-omemo outcome 'hook-draft))))

(ert-deftest jabber-test-chat-r4-omemo-same-mode-retarget ()
  "Native same-mode replacement fences hooks, publication and handoff."
  (dolist (change '(retarget hook-retarget))
    (ert-info ((format "change=%S" change))
      (jabber-test-chat--r4-deferred-omemo 'success change))))

(ert-deftest jabber-test-chat-r4-captured-hook-nested-submit ()
  "An ordinary RET reentered from a captured hook consumes its own reply."
  (jabber-test-chat--journey
   (let (nested)
     (setq-local jabber-message-reply--id "nested-reply")
     (setq-local jabber-message-reply--jid "peer@example.org")
     (setq-local jabber-chat-send-hooks
                 (list (lambda (&rest _)
                         (let ((jabber-chat-send-hooks '(jabber-message-reply--send-hook)))
                           (insert "nested body")
                           (jabber-chat-buffer-send))
                         nil)
                       #'jabber-message-reply--send-hook))
     (cl-letf (((symbol-function 'jabber-send-sexp)
                (lambda (_jc stanza &optional success _failure)
                  (setq nested stanza)
                  (when success (funcall success)))))
       (jabber-chat--run-send-hooks (list 'message nil) "old body" "old-id" t))
     (should (equal "nested-reply"
                    (jabber-xml-get-attribute
                     (jabber-xml-child-with-xmlns nested "urn:xmpp:reply:0") 'id)))
     (should-not jabber-message-reply--id)
     (should (equal '("nested body") jabber-chat--input-history)))))

(provide 'jabber-test-chat)

;;; jabber-test-chat.el ends here
