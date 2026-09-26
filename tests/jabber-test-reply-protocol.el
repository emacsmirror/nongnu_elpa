;;; jabber-test-reply-protocol.el --- Shared reply parsing tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Expected protocol values shared by live, outgoing and archived messages.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-xml)
(require 'jabber-db)
(require 'jabber-chat)
(require 'jabber-mam)
(require 'jabber-message-correct)
(require 'jabber-message-reply)

(defun jabber-test-reply-protocol--fixtures ()
  "Return cases (NAME CHILDREN FIELDS BODY DISPLAY) with explicit expectations."
  (let ((reply '(reply ((xmlns . "urn:xmpp:reply:0")
                       (id . "root") (to . "alice@example.com"))))
        (fields '(:reply-to-id "root" :reply-to-jid "alice@example.com"
                  :fallback-range nil)))
    (append
     (list
      '(absent nil nil "answer" "answer")
      '(ordinary ((body () "ignored")) nil "answer" "answer")
      '(wrong-name ((other ((xmlns . "urn:xmpp:reply:0") (id . "bad"))))
                   nil "answer" "answer")
      '(wrong-namespace ((reply ((xmlns . "urn:other") (id . "bad"))))
                        nil "answer" "answer")
      '(missing-namespace ((reply ((id . "bad")))) nil "answer" "answer")
      (list 'ordinary-reply (list reply) fields "answer" "answer")
      (list 'unrelated-before-valid
            (list "whitespace" '(other ((xmlns . "urn:xmpp:reply:0")
                                        (id . "bad"))) reply)
            fields "answer" "answer")
      (list 'wrong-namespace-before-valid
            (list '(reply ((xmlns . "urn:other") (id . "bad"))) reply)
            fields "answer" "answer")
      (list 'duplicate-reply
            (list reply '(reply ((xmlns . "urn:xmpp:reply:0") (id . "second"))))
            fields "answer" "answer")
      (list 'missing-id-first
            (list '(reply ((xmlns . "urn:xmpp:reply:0"))) reply)
            '(:reply-to-id nil :reply-to-jid nil :fallback-range nil)
            "answer" "answer")
      (list 'empty-id-first
            (list '(reply ((xmlns . "urn:xmpp:reply:0") (id . "") (to . "")))
                  reply)
            '(:reply-to-id "" :reply-to-jid "" :fallback-range nil)
            "answer" "answer")
      '(fallback-without-reply
        ((fallback ((xmlns . "urn:xmpp:fallback:0")
                    (for . "urn:xmpp:reply:0")))) nil "answer" "answer"))
     (mapcar
      (lambda (case)
        (pcase-let ((`(,name ,fallbacks ,range ,body ,display) case))
          (list name (cons reply fallbacks)
                (list :reply-to-id "root" :reply-to-jid "alice@example.com"
                      :fallback-range range)
                body display)))
      '((whole-no-body
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))))
         all "quote" "")
        (whole-bare-body
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body nil))) all "quote" "")
        (range
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0") (end . "2"))))) (0 2) "> answer" "answer")
        (unicode
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0") (end . "5"))))) (0 5) "> α😀\nanswer" "answer")
        (partial-start
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0"))))) nil "answer" "answer")
        (partial-end
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((end . "2"))))) nil "answer" "answer")
        (nonnumeric
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "x") (end . "2"))))) nil "answer" "answer")
        (trailing-junk
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0") (end . "2x"))))) nil "answer" "answer")
        (empty-offsets
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "") (end . ""))))) nil "answer" "answer")
        (negative
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "-1") (end . "2"))))) nil "answer" "answer")
        (reversed
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "4") (end . "2"))))) (4 2) "answer" "answer")
        (out-of-bounds
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0") (end . "99"))))) (0 99) "answer" "answer")
        (wrong-fallback-name
         ((other ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))))
         nil "answer" "answer")
        (wrong-fallback-namespace
         ((fallback ((xmlns . "urn:other") (for . "urn:xmpp:reply:0"))))
         nil "answer" "answer")
        (wrong-for
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:other"))))
         nil "answer" "answer")
        (missing-for
         ((fallback ((xmlns . "urn:xmpp:fallback:0")))) nil "answer" "answer")
        (multiple-for-values
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:other")))
          (fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0") (end . "2"))))
          (fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:another"))))
         (0 2) "> answer" "answer")
        (duplicate-fallback
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0"))))
          (fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))))
         nil "answer" "answer")
        (duplicate-body
         ((fallback ((xmlns . "urn:xmpp:fallback:0") (for . "urn:xmpp:reply:0"))
                    (body ((start . "0") (end . "2"))) (body nil)))
         (0 2) "> answer" "answer"))))))

(defun jabber-test-reply-protocol--stanza (case)
  "Return a fresh incoming stanza for CASE."
  `(message ((from . "alice@example.com/phone") (to . "me@example.com")
             (type . "chat") (id . ,(symbol-name (car case))))
            (body nil ,(nth 3 case))
            ,@(copy-tree (nth 1 case))))

(defun jabber-test-reply-protocol--assert-fields (expected actual)
  "Assert the reply keys in ACTUAL have EXPECTED values."
  (dolist (key '(:reply-to-id :reply-to-jid :fallback-range))
    (should (equal (plist-get expected key) (plist-get actual key)))))

(defmacro jabber-test-reply-protocol--with-db (&rest body)
  "Run BODY with an isolated SQLite database and connection identity."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "jabber-reply-protocol-" t))
          (jabber-db-path (expand-file-name "test.sqlite" directory))
          (jabber-db--connection nil)
          (jabber-db-message-thread-stored-functions nil)
          (jabber-history-inhibit-received-message-functions nil)
          (jc (make-symbol "reply-protocol-connection")))
     (put jc :state-data '(:username "me" :server "example.com"))
     (unwind-protect
         (cl-letf (((symbol-function 'fsm-get-state-data)
                    (lambda (connection) (get connection :state-data))))
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (delete-directory directory t))))

(ert-deftest jabber-test-reply-protocol-pure-fixtures ()
  "Extract exact reply fields without changing stanzas or rendering bodies."
  (dolist (case (jabber-test-reply-protocol--fixtures))
    (ert-info ((symbol-name (car case)))
      (let* ((stanza (jabber-test-reply-protocol--stanza case))
             (before (copy-tree stanza)))
        (should (equal (nth 2 case) (jabber-xml-reply-fields stanza)))
        (should (equal stanza before))))))

(ert-deftest jabber-test-reply-protocol-live-fixtures ()
  "Live message construction and rendering respect semantic expectations."
  (dolist (case (jabber-test-reply-protocol--fixtures))
    (ert-info ((symbol-name (car case)))
      (let ((message (jabber-chat--build-msg-plist
                      (jabber-test-reply-protocol--stanza case) nil)))
        (jabber-test-reply-protocol--assert-fields (nth 2 case) message)
        (should (equal (nth 4 case)
                       (jabber-message-reply--strip-fallback
                        (plist-get message :body)
                        (plist-get message :fallback-range))))))))

(ert-deftest jabber-test-reply-protocol-incoming-backlog-fixtures ()
  "Incoming storage and real SQLite backlog preserve semantic reply values."
  (dolist (case (jabber-test-reply-protocol--fixtures))
    (ert-info ((symbol-name (car case)))
      (jabber-test-reply-protocol--with-db
        (jabber-db--message-handler jc (jabber-test-reply-protocol--stanza case))
        (let ((backlog (jabber-db-backlog "me@example.com" "alice@example.com")))
          (should (= 1 (length backlog)))
          (jabber-test-reply-protocol--assert-fields (nth 2 case) (car backlog))
          (should (equal (nth 3 case) (plist-get (car backlog) :body))))))))

(ert-deftest jabber-test-reply-protocol-outgoing-backlog-fixtures ()
  "Outgoing completed stanza metadata uses the same expected reply values."
  (dolist (case (jabber-test-reply-protocol--fixtures))
    (ert-info ((symbol-name (car case)))
      (jabber-test-reply-protocol--with-db
        (let ((jabber-chatting-with "alice@example.com")
              (jabber-buffer-connection jc)
              (jabber-chat-encryption nil)
              (jabber-chat--sending-correction nil)
              (jabber-chat--send-hook-stanza
               (jabber-test-reply-protocol--stanza case)))
          (jabber-db--outgoing-handler (nth 3 case) "outgoing"))
        (let ((backlog (jabber-db-backlog "me@example.com" "alice@example.com")))
          (should (= 1 (length backlog)))
          (jabber-test-reply-protocol--assert-fields (nth 2 case) (car backlog))
          (should (equal (nth 3 case) (plist-get (car backlog) :body))))))))

(ert-deftest jabber-test-reply-protocol-mam-backlog-fixtures ()
  "MAM inner stanzas retain expected reply fields through real SQLite."
  (dolist (case (jabber-test-reply-protocol--fixtures))
    (ert-info ((symbol-name (car case)))
      (jabber-test-reply-protocol--with-db
        (let* ((jabber-mam--syncing
                (list (list :id "query" :jc jc :to nil :page (list nil)
                            :current-p (jabber-mam--session-predicate jc))))
               (jabber-mam--dirty-peers nil)
               (outer
                `(message ((from . "me@example.com"))
                          (result ((xmlns . "urn:xmpp:mam:2")
                                   (id . "archive") (queryid . "query"))
                                  (forwarded ((xmlns . "urn:xmpp:forward:0"))
                                             (delay ((xmlns . "urn:xmpp:delay")
                                                     (stamp . "2026-01-01T00:00:00Z")))
                                             ,(jabber-test-reply-protocol--stanza case))))))
          (jabber-mam--process-message jc outer)
          (let ((backlog (jabber-db-backlog "me@example.com" "alice@example.com")))
            (should (= 1 (length backlog)))
            (jabber-test-reply-protocol--assert-fields (nth 2 case) (car backlog))
            (should (equal (nth 3 case) (plist-get (car backlog) :body)))))))))

(ert-deftest jabber-test-reply-protocol-native-send-unicode ()
  "Native reply send metadata uses code points and retains one-shot ownership."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "root")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> α😀\n")
    (let* ((elements (jabber-message-reply--send-hook "> α😀\nanswer" "outgoing"))
           (stanza `(message nil (body nil "> α😀\nanswer") ,@elements)))
      (should (equal '(:reply-to-id "root" :reply-to-jid "alice@example.com"
                       :fallback-range (0 5))
                     (jabber-xml-reply-fields stanza)))
      (should-not jabber-message-reply--id)
      (should-not jabber-message-reply--jid)
      (should-not jabber-message-reply--fallback-text))))

(ert-deftest jabber-test-reply-protocol-db-cold-load ()
  "Requiring storage in a fresh process must not load chat or reply UI."
  (let ((expression
         `(progn
            (setq load-path ',load-path jabber-db-path nil)
            (require 'jabber-db)
            (dolist (feature '(jabber-chat jabber-message-reply jabber-chatbuffer ewoc))
              (when (featurep feature) (error "Loaded UI: %s" feature)))
            (unless (equal '(:reply-to-id "root" :reply-to-jid nil :fallback-range nil)
                           (jabber-db--extract-reply-fields
                            '(message nil (reply ((xmlns . "urn:xmpp:reply:0")
                                                  (id . "root"))))))
              (error "Storage parser unavailable")))))
    (with-temp-buffer
      (should (= 0 (call-process (expand-file-name invocation-name invocation-directory)
                                 nil (current-buffer) nil "-Q" "--batch" "--eval"
                                 (prin1-to-string expression)))))))

(provide 'jabber-test-reply-protocol)
;;; jabber-test-reply-protocol.el ends here.
