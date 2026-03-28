;;; jabber-message-reply-tests.el --- Tests for XEP-0461  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ewoc)

(load (expand-file-name "../lisp/jabber-xml.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-chatbuffer.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-message-reply.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Group 1: jabber-message-reply--build-fallback-text

(ert-deftest jabber-message-reply-test-fallback-single-line ()
  "Single-line body produces two-line fallback."
  (let ((result (jabber-message-reply--build-fallback-text "Alice" "Hello")))
    (should (equal "> Alice:\n> Hello\n" result))))

(ert-deftest jabber-message-reply-test-fallback-multi-line ()
  "Multi-line body quotes each line."
  (let ((result (jabber-message-reply--build-fallback-text "Bob" "Line 1\nLine 2\nLine 3")))
    (should (equal "> Bob:\n> Line 1\n> Line 2\n> Line 3\n" result))))

(ert-deftest jabber-message-reply-test-fallback-empty-body ()
  "Empty body produces author line plus empty quote."
  (let ((result (jabber-message-reply--build-fallback-text "Carol" "")))
    (should (equal "> Carol:\n\n" result))))

(ert-deftest jabber-message-reply-test-fallback-nil-body ()
  "Nil body produces author line plus newline."
  (let ((result (jabber-message-reply--build-fallback-text "Dave" nil)))
    (should (equal "> Dave:\n\n" result))))

;;; Group 2: jabber-message-reply--strip-fallback

(ert-deftest jabber-message-reply-test-strip-valid-offsets ()
  "Strip fallback prefix using valid start/end offsets."
  (let* ((body "> Anna:\n> We should bake a cake\nGreat idea!")
         (end (number-to-string (length "> Anna:\n> We should bake a cake\n")))
         (xml-data `(message ((id . "msg-2"))
                             (body () ,body)
                             (reply ((xmlns . "urn:xmpp:reply:0")
                                     (to . "anna@example.com")
                                     (id . "msg-1")))
                             (fallback ((xmlns . "urn:xmpp:fallback:0")
                                        (for . "urn:xmpp:reply:0"))
                                       (body ((start . "0")
                                              (end . ,end)))))))
    (should (equal "Great idea!"
                   (jabber-message-reply--strip-fallback body xml-data)))))

(ert-deftest jabber-message-reply-test-strip-no-fallback-element ()
  "No fallback element returns body unchanged."
  (let ((body "> Anna:\n> Hello\nReply text")
        (xml-data '(message ((id . "msg-3"))
                            (body () "> Anna:\n> Hello\nReply text")
                            (reply ((xmlns . "urn:xmpp:reply:0")
                                    (to . "anna@example.com")
                                    (id . "msg-1"))))))
    (should (equal body (jabber-message-reply--strip-fallback body xml-data)))))

(ert-deftest jabber-message-reply-test-strip-wrong-for-attr ()
  "Fallback with wrong for attribute returns body unchanged."
  (let ((body "> Quoted\nReply")
        (xml-data '(message ((id . "msg-4"))
                            (body () "> Quoted\nReply")
                            (fallback ((xmlns . "urn:xmpp:fallback:0")
                                       (for . "urn:xmpp:wrong:0"))
                                      (body ((start . "0")
                                             (end . "9")))))))
    (should (equal body (jabber-message-reply--strip-fallback body xml-data)))))

(ert-deftest jabber-message-reply-test-strip-bad-offsets ()
  "Fallback with end > body length returns body unchanged."
  (let ((body "Short")
        (xml-data '(message ((id . "msg-5"))
                            (body () "Short")
                            (fallback ((xmlns . "urn:xmpp:fallback:0")
                                       (for . "urn:xmpp:reply:0"))
                                      (body ((start . "0")
                                             (end . "100")))))))
    (should (equal body (jabber-message-reply--strip-fallback body xml-data)))))

(ert-deftest jabber-message-reply-test-strip-nil-body ()
  "Nil body returns nil without error."
  (let ((xml-data '(message ((id . "msg-6"))
                            (fallback ((xmlns . "urn:xmpp:fallback:0")
                                       (for . "urn:xmpp:reply:0"))
                                      (body ((start . "0")
                                             (end . "10")))))))
    (should-not (jabber-message-reply--strip-fallback nil xml-data))))

;;; Group 3: jabber-message-reply--select-id

(ert-deftest jabber-message-reply-test-select-id-1to1 ()
  "In 1:1 chat, select :id."
  (let ((msg (list :id "client-id-1" :server-id "server-id-1")))
    (should (equal "client-id-1"
                   (jabber-message-reply--select-id msg nil)))))

(ert-deftest jabber-message-reply-test-select-id-muc-server ()
  "In MUC, prefer :server-id."
  (let ((msg (list :id "client-id-2" :server-id "server-id-2")))
    (should (equal "server-id-2"
                   (jabber-message-reply--select-id msg t)))))

(ert-deftest jabber-message-reply-test-select-id-muc-fallback ()
  "In MUC without :server-id, fall back to :id."
  (let ((msg (list :id "client-id-3" :server-id nil)))
    (should (equal "client-id-3"
                   (jabber-message-reply--select-id msg t)))))

(ert-deftest jabber-message-reply-test-select-id-missing ()
  "Missing both IDs returns nil."
  (let ((msg (list :id nil :server-id nil)))
    (should-not (jabber-message-reply--select-id msg nil))))

;;; Group 4: jabber-message-reply--send-hook

(ert-deftest jabber-message-reply-test-send-hook-produces-elements ()
  "Send hook produces reply and fallback elements and clears state."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-length 20)
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
      (should-not jabber-message-reply--fallback-length))))

(ert-deftest jabber-message-reply-test-send-hook-nil-when-no-reply ()
  "Send hook returns nil when no reply state is set."
  (with-temp-buffer
    (should-not (jabber-message-reply--send-hook "Hello" "msg-id"))))

(ert-deftest jabber-message-reply-test-send-hook-reply-attributes ()
  "Reply element has correct to and id attributes."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "bob@example.com/phone")
    (setq-local jabber-message-reply--fallback-length 15)
    (let* ((elements (jabber-message-reply--send-hook
                      "> Bob:\n> Hey\nYes!" "new-msg"))
           (reply-el (cl-find 'reply elements :key #'car)))
      (should reply-el)
      (let ((attrs (cadr reply-el)))
        (should (equal "urn:xmpp:reply:0" (cdr (assq 'xmlns attrs))))
        (should (equal "bob@example.com/phone" (cdr (assq 'to attrs))))
        (should (equal "target-msg" (cdr (assq 'id attrs))))))))

(ert-deftest jabber-message-reply-test-send-hook-no-fallback-when-zero-length ()
  "No fallback element when fallback-length is 0."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "carol@example.com")
    (setq-local jabber-message-reply--fallback-length 0)
    (let ((elements (jabber-message-reply--send-hook "Just a reply" "new-msg")))
      (should elements)
      (should (cl-some (lambda (el) (eq (car el) 'reply)) elements))
      (should-not (cl-some (lambda (el) (eq (car el) 'fallback)) elements)))))

(provide 'jabber-message-reply-tests)

;;; jabber-message-reply-tests.el ends here
