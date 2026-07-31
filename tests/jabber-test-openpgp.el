;;; jabber-test-openpgp.el --- Tests for jabber-openpgp  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0373 OpenPGP send paths.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-openpgp)
(require 'jabber-openpgp-legacy)

(defmacro jabber-test-openpgp--with-db (&rest body)
  "Run BODY with a temporary message database."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "jabber-openpgp-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" dir))
          (jabber-db--connection nil))
     (unwind-protect
         (progn (jabber-db-ensure-open) ,@body)
       (jabber-db-close)
       (delete-directory dir t))))

;;; Group 1: MUC send hooks (XEP-0373)

(defmacro jabber-test-openpgp--with-muc-send-stubs (sent-var &rest body)
  "Run BODY with the OpenPGP MUC encrypt/send path stubbed.
SENT-VAR is bound to the stanza passed to `jabber-send-sexp'."
  (declare (indent 1) (debug t))
  `(let ((,sent-var nil))
     (cl-letf (((symbol-function 'jabber-openpgp--muc-participant-jids)
                (lambda (_group) '("alice@example.com")))
               ((symbol-function 'jabber-connection-bare-jid)
                (lambda (_jc) "me@example.com"))
               ((symbol-function 'jabber-openpgp--ensure-recipient-keys)
                (lambda (_jc _jids callback &optional _failure)
                  (funcall callback)))
               ((symbol-function 'jabber-openpgp--build-crypt-xml)
                (lambda (_jids _body) '(payload ())))
               ((symbol-function 'jabber-openpgp--encrypt)
                (lambda (_jc _xml _jids &optional _sign) "cipher"))
               ((symbol-function 'jabber-send-sexp)
                (lambda (_jc stanza) (setq ,sent-var stanza))))
       ,@body)))

(ert-deftest jabber-test-openpgp-muc-send-hooks-run-in-buffer ()
  "MUC send hooks run in the originating buffer, not the callback's."
  (let* ((muc-buffer (generate-new-buffer "*test-openpgp-muc*"))
         (hook-buffer nil)
         (jabber-chat-send-hooks
          (list (lambda (_body _id)
                  (setq hook-buffer (current-buffer))
                  '((probe ((xmlns . "test:probe"))))))))
    (unwind-protect
        (jabber-test-openpgp--with-muc-send-stubs sent
          (with-current-buffer muc-buffer
            (setq-local jabber-group "room@conf.example.com")
            (jabber-openpgp--send-muc 'fake-jc "hello"))
          (should (eq hook-buffer muc-buffer))
          (should sent)
          (should (jabber-xml-get-attribute sent 'id))
          (should (jabber-xml-get-children sent 'probe)))
      (kill-buffer muc-buffer))))

(ert-deftest jabber-test-openpgp-muc-send-dead-buffer-cancels ()
  "A dead originating buffer cancels the deferred encrypted send."
  (let* ((muc-buffer (generate-new-buffer "*test-openpgp-muc*"))
         (pending-callback nil)
         (jabber-chat-send-hooks
          (list (lambda (_body _id) '((probe ((xmlns . "test:probe"))))))))
    (jabber-test-openpgp--with-muc-send-stubs sent
      (cl-letf (((symbol-function 'jabber-openpgp--ensure-recipient-keys)
                 (lambda (_jc _jids callback &optional _failure)
                   (setq pending-callback callback))))
        (with-current-buffer muc-buffer
          (setq-local jabber-group "room@conf.example.com")
          (jabber-openpgp--send-muc 'fake-jc "hello")))
      ;; Buffer dies while the key fetch is in flight.
      (kill-buffer muc-buffer)
      (funcall pending-callback)
      (should-not sent))))

(ert-deftest jabber-test-openpgp-muc-preflight-preserves-send-context ()
  "Missing MUC recipients leave pending reply and thread state intact."
  (with-temp-buffer
    (setq-local jabber-group "room@conf.example.com")
    (setq-local jabber-message-reply--id "reply-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-text "> Alice:\n> root\n")
    (setq-local jabber-message-reply--thread
                '(:thread-id "thread-1" :thread-parent-id "parent-1"))
    (setq-local jabber-message-thread--root-reply-id "root-1")
    (setq-local jabber-message-thread--root-reply-jid "alice@example.com")
    (cl-letf (((symbol-function 'jabber-openpgp--muc-participant-jids)
               (lambda (_group) nil))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (should-error (jabber-openpgp--send-muc 'fake-jc "answer")
                    :type 'user-error))
    (should (equal "reply-1" jabber-message-reply--id))
    (should (equal "alice@example.com" jabber-message-reply--jid))
    (should (equal "> Alice:\n> root\n"
                   jabber-message-reply--fallback-text))
    (should (equal '(:thread-id "thread-1"
                     :thread-parent-id "parent-1")
                   jabber-message-reply--thread))
    (should (equal "root-1" jabber-message-thread--root-reply-id))
    (should (equal "alice@example.com"
                   jabber-message-thread--root-reply-jid))))

(ert-deftest jabber-test-openpgp-concurrent-sends-keep-thread-owner ()
  "Reverse key completion cannot move reply and thread metadata."
  (jabber-test-openpgp--with-db
    (with-temp-buffer
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chatting-with "friend@example.com")
      (setq-local jabber-chat-encryption 'openpgp)
      (setq-local jabber-chat-send-hooks
                  '(jabber-message-reply--send-hook
                    jabber-db--outgoing-handler))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" "root" 1
       "phone" "root-1" nil nil nil nil nil
       '(:thread-id "thread-1"))
      (let (callbacks sent
            (ticks 20))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) (cl-incf ticks)))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "me@example.com"))
                  ((symbol-function 'jabber-openpgp--ensure-recipient-keys)
                   (lambda (_jc _jids callback &optional _failure)
                     (push callback callbacks)))
                  ((symbol-function 'jabber-openpgp--build-signcrypt-xml)
                   (lambda (&rest _) "payload"))
                  ((symbol-function 'jabber-openpgp--encrypt)
                   (lambda (&rest _) "cipher"))
                  ((symbol-function 'jabber-chat--display-local-message)
                   #'ignore)
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc stanza &optional success _failure)
                     (push stanza sent)
                     (when success (funcall success)))))
          (setq-local jabber-message-reply--id "root-1")
          (setq-local jabber-message-reply--jid "friend@example.com")
          (setq-local jabber-message-reply--thread
                      '(:thread-id "thread-1"))
          (jabber-openpgp--send-chat 'fake-jc "first")
          (jabber-openpgp--send-chat 'fake-jc "second")
          (funcall (car callbacks))
          (funcall (cadr callbacks))
          (let ((first (car sent))
                (second (cadr sent)))
            (should (= 1 (length (jabber-xml-get-children first 'thread))))
            (should (jabber-xml-child-with-xmlns first "urn:xmpp:reply:0"))
            (should-not (jabber-xml-get-children second 'thread))
            (should-not
             (jabber-xml-child-with-xmlns second "urn:xmpp:reply:0")))
          (should
           (equal '(("first" "thread-1") ("second" nil))
                  (sqlite-select
                   jabber-db--connection
                   "SELECT body, thread_id FROM message \
WHERE body IN ('first', 'second') ORDER BY body"))))))))

;;; Group 2: MUC send hooks (XEP-0027 legacy)

(ert-deftest jabber-test-openpgp-legacy-muc-send-runs-hooks ()
  "Legacy MUC send stamps an id and runs the send hooks."
  (let* ((muc-buffer (generate-new-buffer "*test-openpgp-legacy-muc*"))
         (hook-buffer nil)
         (sent nil)
         (jabber-chat-send-hooks
          (list (lambda (_body _id)
                  (setq hook-buffer (current-buffer))
                  '((probe ((xmlns . "test:probe"))))))))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-openpgp-legacy--muc-participant-jids)
                   (lambda (_group) '("alice@example.com")))
                  ((symbol-function 'jabber-openpgp--our-key)
                   (lambda (_jc) 'our-key))
                  ((symbol-function 'jabber-openpgp--recipient-key)
                   (lambda (_jid) 'their-key))
                  ((symbol-function 'epg-find-configuration)
                   (lambda (_protocol) '((program . "gpg"))))
                  ((symbol-function 'epg-encrypt-string)
                   (lambda (&rest _) "-----BEGIN PGP MESSAGE-----\n\nZm9v\n-----END PGP MESSAGE-----"))
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc stanza) (setq sent stanza))))
          (with-current-buffer muc-buffer
            (setq-local jabber-group "room@conf.example.com")
            (jabber-openpgp-legacy--send-muc 'fake-jc "hello"))
          (should (eq hook-buffer muc-buffer))
          (should sent)
          (should (jabber-xml-get-attribute sent 'id))
          (should (jabber-xml-get-children sent 'probe)))
      (kill-buffer muc-buffer))))

(provide 'jabber-test-openpgp)
;;; jabber-test-openpgp.el ends here
