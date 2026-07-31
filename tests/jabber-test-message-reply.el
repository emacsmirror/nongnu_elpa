;;; jabber-test-message-reply.el --- Tests for jabber-message-reply  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0461 Message Replies with XEP-0428 Fallback.

;;; Code:

(require 'ert)
(require 'ewoc)

(require 'jabber-xml)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-muc)
(require 'jabber-message-reply)

;;; Group 1: jabber-message-reply--build-fallback-text

(ert-deftest jabber-test-message-reply-fallback-single-line ()
  "Single-line body produces two-line fallback."
  (let ((result (jabber-message-reply--build-fallback-text "Alice" "Hello")))
    (should (equal "> Alice:\n> Hello\n" result))))

(ert-deftest jabber-test-message-reply-fallback-multi-line ()
  "Multi-line body quotes each line."
  (let ((result (jabber-message-reply--build-fallback-text "Bob" "Line 1\nLine 2\nLine 3")))
    (should (equal "> Bob:\n> Line 1\n> Line 2\n> Line 3\n" result))))

(ert-deftest jabber-test-message-reply-fallback-empty-body ()
  "Empty body produces author line plus empty quote."
  (let ((result (jabber-message-reply--build-fallback-text "Carol" "")))
    (should (equal "> Carol:\n\n" result))))

(ert-deftest jabber-test-message-reply-fallback-nil-body ()
  "Nil body produces author line plus newline."
  (let ((result (jabber-message-reply--build-fallback-text "Dave" nil)))
    (should (equal "> Dave:\n\n" result))))

;;; Group 1b: jabber-message-reply--strip-fallback

(ert-deftest jabber-test-message-reply-strip-fallback-range ()
  "A (START END) range is excised from the body."
  (should (equal "answer"
                 (jabber-message-reply--strip-fallback
                  "> alice:\n> hi\nanswer" '(0 14)))))

(ert-deftest jabber-test-message-reply-strip-fallback-all ()
  "A whole-body fallback strips to the empty string."
  (should (equal "" (jabber-message-reply--strip-fallback
                     "> alice:\n> hi" 'all))))

(ert-deftest jabber-test-message-reply-strip-fallback-nil ()
  "A nil range leaves the body unchanged."
  (should (equal "hello" (jabber-message-reply--strip-fallback "hello" nil))))

(ert-deftest jabber-test-message-reply-strip-fallback-malformed ()
  "Out-of-bounds or malformed ranges leave the body unchanged."
  (should (equal "short" (jabber-message-reply--strip-fallback
                          "short" '(0 99))))
  (should (equal "short" (jabber-message-reply--strip-fallback
                          "short" '(3 1))))
  (should (equal "short" (jabber-message-reply--strip-fallback
                          "short" "junk"))))

;;; Group 2: jabber-message-reply--select-id

(ert-deftest jabber-test-message-reply-select-id-1to1 ()
  "In 1:1 chat, select :id."
  (let ((msg (list :id "client-id-1" :server-id "server-id-1")))
    (should (equal "client-id-1"
                   (jabber-message-reply--select-id msg nil)))))

(ert-deftest jabber-test-message-reply-select-id-prefers-origin-id ()
  "In 1:1 chat, prefer :origin-id over :id."
  (let ((msg (list :id "client-id-1" :origin-id "origin-id-1")))
    (should (equal "origin-id-1"
                   (jabber-message-reply--select-id msg nil)))
    ;; MUC keeps requiring the server-assigned stanza-id.
    (should-not (jabber-message-reply--select-id msg t))))

(ert-deftest jabber-test-message-reply-select-id-muc-server ()
  "In MUC, prefer :server-id."
  (let ((msg (list :id "client-id-2" :server-id "server-id-2")))
    (should (equal "server-id-2"
                   (jabber-message-reply--select-id msg t)))))

(ert-deftest jabber-test-message-reply-select-id-muc-requires-server-id ()
  "In MUC without :server-id, do not use the stanza id."
  (let ((msg (list :id "client-id-3" :server-id nil)))
    (should-not (jabber-message-reply--select-id msg t))))

(ert-deftest jabber-test-message-reply-select-id-missing ()
  "Missing both IDs returns nil."
  (let ((msg (list :id nil :server-id nil)))
    (should-not (jabber-message-reply--select-id msg nil))))

;;; Group 2b: jabber-message-reply--author-name

(ert-deftest jabber-test-message-reply-author-name-muc-pm-uses-nick ()
  "MUC private messages quote the occupant nick, not the room name."
  (with-temp-buffer
    (cl-letf (((symbol-function 'jabber-muc-sender-p) (lambda (_jid) t)))
      (should (equal "alice"
                     (jabber-message-reply--author-name
                      "room@conf.example.com/alice"))))))

(ert-deftest jabber-test-message-reply-author-name-1to1-uses-username ()
  "1:1 chats quote the username part of the JID."
  (with-temp-buffer
    (cl-letf (((symbol-function 'jabber-muc-sender-p) (lambda (_jid) nil)))
      (should (equal "alice"
                     (jabber-message-reply--author-name
                      "alice@example.com/phone"))))))

;;; Group 3: jabber-message-reply--send-hook

(ert-deftest jabber-test-message-reply-send-hook-produces-elements ()
  "Send hook produces reply and fallback elements and clears state."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> Hello\n")
    (let* ((body "> Alice:\n> Hello\nReply text here")
           (elements (jabber-message-reply--send-hook body "new-id")))
      ;; Should produce elements
      (should elements)
      ;; Should contain a reply element
      (should (cl-some (lambda (el) (eq (car el) 'reply)) elements))
      ;; Should contain a fallback element
      (should (cl-some (lambda (el) (eq (car el) 'fallback)) elements))
      ;; State should be cleared
      (should-not jabber-message-reply--id)
      (should-not jabber-message-reply--jid)
      (should-not jabber-message-reply--fallback-text))))

(ert-deftest jabber-test-message-reply-send-hook-preserves-thread ()
  "An ordinary reply reuses the target message's thread context."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id")
    (setq-local jabber-message-reply--thread
                '(:thread-id "thread-1" :thread-parent-id "parent-1"))
    (let* ((jabber-chat--send-hook-stanza
            '(message ((type . "chat")) (body () "reply")))
           (elements (jabber-message-reply--send-hook "reply" "new-id")))
      (should
       (equal '((thread ((parent . "parent-1")) "thread-1"))
              (cl-remove-if-not
               (lambda (element) (eq (car element) 'thread))
               elements)))
      (should-not jabber-message-reply--thread))))

(ert-deftest jabber-test-message-reply-send-hook-does-not-duplicate-thread ()
  "An explicit outgoing thread element wins over pending reply context."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id")
    (setq-local jabber-message-reply--thread
                '(:thread-id "thread-1" :thread-parent-id nil))
    (let* ((jabber-chat--send-hook-stanza
            '(message ((type . "chat"))
                      (body () "reply")
                      (thread () "thread-1")))
           (elements (jabber-message-reply--send-hook "reply" "new-id")))
      (should-not (cl-find 'thread elements :key #'car)))))

(ert-deftest jabber-test-message-reply-send-hook-nil-when-no-reply ()
  "Send hook returns nil when no reply state is set."
  (with-temp-buffer
    (should-not (jabber-message-reply--send-hook "Hello" "msg-id"))))

(ert-deftest jabber-test-message-reply-send-hook-reply-attributes ()
  "Reply element has correct to and id attributes."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "bob@example.com/phone")
    (setq-local jabber-message-reply--fallback-text "> Bob:\n> Hey\n")
    (let* ((elements (jabber-message-reply--send-hook
                      "> Bob:\n> Hey\nYes!" "new-msg"))
           (reply-el (cl-find 'reply elements :key #'car)))
      (should reply-el)
      (let ((attrs (cadr reply-el)))
        (should (equal "urn:xmpp:reply:0" (cdr (assq 'xmlns attrs))))
        (should (equal "bob@example.com/phone" (cdr (assq 'to attrs))))
        (should (equal "target-msg" (cdr (assq 'id attrs))))))))

(ert-deftest jabber-test-message-reply-send-hook-no-fallback-when-no-quote ()
  "No fallback element when no quote text was inserted."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "carol@example.com")
    (setq-local jabber-message-reply--fallback-text nil)
    (let ((elements (jabber-message-reply--send-hook "Just a reply" "new-msg")))
      (should elements)
      (should (cl-some (lambda (el) (eq (car el) 'reply)) elements))
      (should-not (cl-some (lambda (el) (eq (car el) 'fallback)) elements)))))

(ert-deftest jabber-test-message-reply-send-hook-skips-fallback-when-edited ()
  "An edited quote suppresses the fallback range but keeps the reply."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> Hello\n")
    (let ((elements (jabber-message-reply--send-hook
                     "> Alice: Hello (edited)\nmy reply" "new-msg")))
      (should (cl-some (lambda (el) (eq (car el) 'reply)) elements))
      (should-not (cl-some (lambda (el) (eq (car el) 'fallback)) elements)))))

(ert-deftest jabber-test-message-reply-send-hook-fallback-range-matches-text ()
  "Fallback end offset equals the quote's code point count."
  (with-temp-buffer
    (let ((fb "> Alice:\n> Hëllo\n"))
      (setq-local jabber-message-reply--id "target-msg")
      (setq-local jabber-message-reply--jid "alice@example.com")
      (setq-local jabber-message-reply--fallback-text fb)
      (let* ((elements (jabber-message-reply--send-hook
                        (concat fb "my reply") "new-msg"))
             (fb-el (cl-find 'fallback elements :key #'car))
             (body-el (car (jabber-xml-get-children fb-el 'body))))
        (should body-el)
        (should (equal "0" (jabber-xml-get-attribute body-el 'start)))
        (should (equal (number-to-string (length fb))
                       (jabber-xml-get-attribute body-el 'end)))))))

(ert-deftest jabber-test-message-reply-send-hook-omits-empty-to ()
  "Replying to our own message (no author JID) omits the `to' attribute.
A `to=\"\"' is an invalid JID and makes strict parsers drop the reply."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id")
    (setq-local jabber-message-reply--jid "")
    (setq-local jabber-message-reply--fallback-text nil)
    (let* ((elements (jabber-message-reply--send-hook "Just a reply" "new-msg"))
           (reply-el (cl-find 'reply elements :key #'car))
           (attrs (cadr reply-el)))
      (should reply-el)
      (should (equal "orig-id" (cdr (assq 'id attrs))))
      (should-not (assq 'to attrs)))))

(ert-deftest jabber-test-message-reply-outgoing-body-keeps-fallback ()
  "The outgoing reply stanza body retains the quoted fallback text.
The send hook adds <reply>/<fallback> elements but must never strip
the quoted prefix from the body that is actually sent."
  (with-temp-buffer
    (let* ((fallback (jabber-message-reply--build-fallback-text "alice" "Hello"))
           (body (concat fallback "my reply"))
           (jabber-chat-send-hooks (list #'jabber-message-reply--send-hook)))
      (setq-local jabber-message-reply--id "orig-1")
      (setq-local jabber-message-reply--jid "alice@example.com")
      (setq-local jabber-message-reply--fallback-text fallback)
      (let ((stanza `(message ((to . "room@conf.example.com")
                               (type . "groupchat")
                               (id . "m1"))
                              (body () ,body))))
        (jabber-chat--run-send-hooks stanza body "m1")
        (let ((sent-body (car (jabber-xml-node-children
                               (car (jabber-xml-get-children stanza 'body))))))
          (should (string-prefix-p "> alice:\n> Hello\n" sent-body))
          (should (string-suffix-p "my reply" sent-body)))
        (should (jabber-xml-child-with-xmlns stanza "urn:xmpp:reply:0"))
        (should (jabber-xml-child-with-xmlns stanza "urn:xmpp:fallback:0"))))))

(ert-deftest jabber-test-message-reply-elements-builder ()
  "The elements builder emits reply with optional to and fallback."
  (let* ((els (jabber-message-reply--elements "id-1" "alice@example.com" 10))
         (reply-el (cl-find 'reply els :key #'car))
         (fb-el (cl-find 'fallback els :key #'car)))
    (should (equal "alice@example.com" (cdr (assq 'to (cadr reply-el)))))
    (should (equal "id-1" (cdr (assq 'id (cadr reply-el)))))
    (should fb-el)
    (should (equal "10" (jabber-xml-get-attribute
                         (car (jabber-xml-get-children fb-el 'body))
                         'end))))
  (let* ((els (jabber-message-reply--elements "id-2" nil nil))
         (reply-el (cl-find 'reply els :key #'car)))
    (should-not (assq 'to (cadr reply-el)))
    (should-not (cl-find 'fallback els :key #'car))))

(ert-deftest jabber-test-message-reply-correction-fallback-length ()
  "Quote length is kept only while the corrected body starts with it."
  (let ((msg (list :body "> a:\n> hi\nanswer"
                   :fallback-range '(0 10)
                   :reply-to-id "orig")))
    (should (= 10 (jabber-message-reply--correction-fallback-length
                   msg "> a:\n> hi\nbetter answer")))
    (should-not (jabber-message-reply--correction-fallback-length
                 msg "answer without quote"))))

(ert-deftest jabber-test-message-reply-send-hook-inert-during-correction ()
  "The send hook adds nothing and keeps its state during a correction."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> Hello\n")
    (let ((jabber-chat--sending-correction t))
      (should-not (jabber-message-reply--send-hook "corrected text" "c-1")))
    (should (equal "orig-1" jabber-message-reply--id))
    (should (equal "alice@example.com" jabber-message-reply--jid))))

(ert-deftest jabber-test-message-reply-state-survives-correction-stanza ()
  "A correction through the hook runner leaves the pending reply intact."
  (with-temp-buffer
    (let ((jabber-chat-send-hooks (list #'jabber-message-reply--send-hook)))
      (setq-local jabber-message-reply--id "orig-1")
      (setq-local jabber-message-reply--jid "alice@example.com")
      (setq-local jabber-message-reply--fallback-text nil)
      (let ((correction `(message ((to . "bob@example.com")
                                   (type . "chat")
                                   (id . "c-1"))
                                  (body () "fixed")
                                  (replace ((id . "old-1")
                                            (xmlns . "urn:xmpp:message-correct:0"))))))
        (jabber-chat--run-send-hooks correction "fixed" "c-1")
        (should-not (jabber-xml-child-with-xmlns correction "urn:xmpp:reply:0"))
        (should (equal "orig-1" jabber-message-reply--id)))
      ;; The next plain message still becomes the reply.
      (let ((stanza `(message ((to . "bob@example.com")
                               (type . "chat")
                               (id . "m-2"))
                              (body () "the actual reply"))))
        (jabber-chat--run-send-hooks stanza "the actual reply" "m-2")
        (should (jabber-xml-child-with-xmlns stanza "urn:xmpp:reply:0"))
        (should-not jabber-message-reply--id)))))

;;; Group 4: incoming fallback parsing

(ert-deftest jabber-test-message-reply-keeps-incoming-fallback ()
  "Incoming XEP-0461 reply fallback is kept in the displayed body."
  (let* ((stanza '(message ((from . "alice@example.com/tablet")
                            (id . "reply-1")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello\nActual reply")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/tablet")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "0")
                                            (end . "17"))))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "> Alice:\n> Hello\nActual reply" (plist-get msg :body)))
    (should (equal "orig-1" (plist-get msg :reply-to-id)))))

(ert-deftest jabber-test-message-reply-keeps-whole-body-fallback ()
  "Reply fallback with no body range is kept in the displayed body."
  (let* ((stanza '(message ((from . "alice@example.com/tablet")
                            (id . "reply-1")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/tablet")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0")))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "> Alice:\n> Hello" (plist-get msg :body)))
    (should (equal "orig-1" (plist-get msg :reply-to-id)))))

(ert-deftest jabber-test-message-reply-preserves-malformed-fallback ()
  "Malformed XEP-0461 fallback ranges leave the body unchanged."
  (let* ((body "> Alice:\n> Hello\nActual reply")
         (stanza `(message ((from . "alice@example.com/tablet")
                            (id . "reply-2")
                            (type . "chat"))
                           (body () ,body)
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/tablet")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "x")
                                            (end . "17"))))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal body (plist-get msg :body)))))

(ert-deftest jabber-test-message-reply-preserves-fallback-without-reply ()
  "Fallback text is preserved when no XEP-0461 reply element is present."
  (let* ((body "> Alice:\n> Hello\nActual reply")
         (stanza `(message ((from . "alice@example.com/tablet")
                            (id . "reply-3")
                            (type . "chat"))
                           (body () ,body)
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "0")
                                            (end . "17"))))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal body (plist-get msg :body)))))

(ert-deftest jabber-test-message-reply-preserves-whole-fallback-without-reply ()
  "Whole-body fallback is preserved when no XEP-0461 reply element is present."
  (let* ((body "> Alice:\n> Hello")
         (stanza `(message ((from . "alice@example.com/tablet")
                            (id . "reply-4")
                            (type . "chat"))
                           (body () ,body)
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0")))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal body (plist-get msg :body)))))

;;; Group 5: jabber-chat-reply input placement

(ert-deftest jabber-test-message-reply-inserts-fallback-at-input-start ()
  "`jabber-chat-reply' puts the quote at offset 0 of the input area.
The <fallback> range is start=0, so the quote must land at
`jabber-point-insert' even when a draft is already present, not at
`point-max'."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (d) (insert (format "%s\n" (plist-get (cadr d) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (let ((node (ewoc-enter-last
                   ewoc (list :foreign (list :id "orig-1"
                                             :from "alice@example.com/phone"
                                             :body "Hello")))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (insert "my draft")
        (goto-char (ewoc-location node))
        (jabber-chat-reply)
        (let ((input (buffer-substring-no-properties
                      jabber-point-insert (point-max)))
              (quote (jabber-message-reply--build-fallback-text "alice" "Hello")))
          (should (string-prefix-p quote input))
          (should (string-suffix-p "my draft" input))
          (should (equal quote jabber-message-reply--fallback-text))
          (should (equal "orig-1" jabber-message-reply--id))
          (should-not jabber-message-reply--thread))))))

(ert-deftest jabber-test-message-reply-captures-thread-context ()
  "Replying to a threaded message records its exact thread context."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (data)
                   (insert (format "%s\n" (plist-get (cadr data) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (let ((node
             (ewoc-enter-last
              ewoc
              '(:foreign (:id "orig-1"
                          :from "alice@example.com/phone"
                          :body "Hello"
                          :thread-id "thread-1"
                          :thread-parent-id "parent-1")))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (goto-char (ewoc-location node))
        (jabber-chat-reply)
        (should
         (equal '(:thread-id "thread-1" :thread-parent-id "parent-1")
                jabber-message-reply--thread))))))

(ert-deftest jabber-test-message-reply-captures-thread-summary-context ()
  "A locally registered root supplies reply context through its summary."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (data)
                   (insert (format "%s\n" (plist-get (cadr data) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (let ((node
             (ewoc-enter-last
              ewoc
              '(:foreign (:id "orig-1"
                          :from "alice@example.com/phone"
                          :body "Hello"
                          :thread-summary
                          (:thread-id "thread-1"
                           :thread-parent-id "parent-1"))))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (goto-char (ewoc-location node))
        (jabber-chat-reply)
        (should
         (equal '(:thread-id "thread-1" :thread-parent-id "parent-1")
                jabber-message-reply--thread))))))

(ert-deftest jabber-test-message-reply-quote-not-nested ()
  "Replying to a reply quotes only the answer, not the old quote."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (d) (insert (format "%s\n" (plist-get (cadr d) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (let ((node (ewoc-enter-last
                   ewoc (list :foreign
                              (list :id "reply-1"
                                    :from "alice@example.com/phone"
                                    :body "> alice:\n> hi\nanswer"
                                    :fallback-range '(0 14))))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (goto-char (ewoc-location node))
        (jabber-chat-reply)
        (let ((input (buffer-substring-no-properties
                      jabber-point-insert (point-max))))
          (should (equal "> alice:\n> answer\n" input)))))))

(ert-deftest jabber-test-message-reply-self-reply-uses-own-bare-jid ()
  "Replying to our own 1:1 message sets to= to our bare JID."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (d) (insert (format "%s\n" (plist-get (cadr d) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chatting-with "bob@example.com")
      (let ((node (ewoc-enter-last
                   ewoc (list :local (list :id "my-msg-1" :body "my message")))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-muc-sender-p)
                   (lambda (_jid) nil)))
          (goto-char (ewoc-location node))
          (jabber-chat-reply))
        (let* ((body (buffer-substring-no-properties
                      jabber-point-insert (point-max)))
               (elements (jabber-message-reply--send-hook body "new-id"))
               (reply-el (cl-find 'reply elements :key #'car)))
          (should (equal "me@example.com"
                         (cdr (assq 'to (cadr reply-el))))))))))

(ert-deftest jabber-test-message-reply-self-reply-muc-pm-omits-to ()
  "Replying to our own MUC-PM message omits the to attribute."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (d) (insert (format "%s\n" (plist-get (cadr d) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chatting-with "room@conf.example.com/me")
      (let ((node (ewoc-enter-last
                   ewoc (list :local (list :id "my-msg-1" :body "my message")))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "me@example.com"))
                  ((symbol-function 'jabber-muc-sender-p)
                   (lambda (_jid) t)))
          (goto-char (ewoc-location node))
          (jabber-chat-reply))
        (let* ((body (buffer-substring-no-properties
                      jabber-point-insert (point-max)))
               (elements (jabber-message-reply--send-hook body "new-id"))
               (reply-el (cl-find 'reply elements :key #'car)))
          (should reply-el)
          (should-not (assq 'to (cadr reply-el))))))))

;;; Group 6: jabber-chat-cancel-reply

(ert-deftest jabber-test-message-reply-cancel-removes-intact-quote ()
  "Cancelling a reply deletes the still-intact quote from the input area."
  (with-temp-buffer
    (setq-local jabber-point-insert (point-marker))
    (insert "> Alice:\n> Hello\nmy text")
    (setq-local jabber-message-reply--id "orig-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> Hello\n")
    (setq-local jabber-message-reply--thread
                '(:thread-id "thread-1" :thread-parent-id nil))
    (jabber-chat-cancel-reply)
    (should (equal "my text" (buffer-string)))
    (should-not jabber-message-reply--id)
    (should-not jabber-message-reply--fallback-text)
    (should-not jabber-message-reply--thread)))

(ert-deftest jabber-test-message-reply-cancel-keeps-edited-input ()
  "Cancelling a reply leaves edited input untouched but clears state."
  (with-temp-buffer
    (setq-local jabber-point-insert (point-marker))
    (insert "> Alice: edited quote\nmy text")
    (setq-local jabber-message-reply--id "orig-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> Hello\n")
    (jabber-chat-cancel-reply)
    (should (equal "> Alice: edited quote\nmy text" (buffer-string)))
    (should-not jabber-message-reply--id)
    (should-not jabber-message-reply--fallback-text)))

(ert-deftest jabber-test-message-reply-cancel-short-buffer-no-error ()
  "Cancelling with input shorter than the quote does not error."
  (with-temp-buffer
    (setq-local jabber-point-insert (point-marker))
    (insert "> A")
    (setq-local jabber-message-reply--id "orig-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> Hello\n")
    (jabber-chat-cancel-reply)
    (should (equal "> A" (buffer-string)))
    (should-not jabber-message-reply--id)))

(provide 'jabber-test-message-reply)

;;; jabber-test-message-reply.el ends here
