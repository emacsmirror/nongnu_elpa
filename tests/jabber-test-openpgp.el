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
                (lambda (_jc stanza &optional success _failure)
                  (setq ,sent-var stanza)
                  (when success (funcall success)))))
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
            (setq-local jabber-buffer-connection 'fake-jc)
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
            (should (= 1 (length (jabber-xml-get-children second 'thread))))
            (should-not
             (equal "thread-1"
                    (car (jabber-xml-node-children
                          (car (jabber-xml-get-children second 'thread))))))
            (should-not
             (jabber-xml-child-with-xmlns second "urn:xmpp:reply:0"))
            (should
             (equal
              `(("first" "thread-1")
                ("second"
                 ,(car (jabber-xml-node-children
                        (car (jabber-xml-get-children second 'thread))))))
              (sqlite-select
               jabber-db--connection
               "SELECT body, thread_id FROM message \
WHERE body IN ('first', 'second') ORDER BY body")))))))))

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

;;; Receive admission with real GnuPG

(defun jabber-test-openpgp--gpg (input &rest args)
  "Run isolated GnuPG with INPUT and ARGS, returning binary output."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert input)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary))
      (unless (zerop
               (apply #'call-process-region
                      (point-min) (point-max) "gpg" t (list t nil) nil
                      "--homedir" epg-gpg-home-directory
                      "--batch" "--yes" "--no-tty" args))
        (error "Fixture GnuPG operation failed")))
    (buffer-string)))

(defun jabber-test-openpgp--with-keys (function)
  "Call FUNCTION with disposable sender, recipient and unrelated keys.
All GnuPG and EasyPG operations use a private temporary home."
  (unless (and (executable-find "gpg") (executable-find "gpgconf"))
    (ert-skip "GnuPG with gpgconf is required"))
  (let* ((home (make-temp-file "jabber-gpg-" t))
         (epg-gpg-home-directory home)
         (process-environment (copy-sequence process-environment))
         (temporary-file-directory home))
    (set-file-modes home #o700)
    (setenv "GNUPGHOME" home)
    (unwind-protect
        (progn
          ;; No configuration, network key retrieval or real user agent.
          (with-temp-file (expand-file-name "gpg.conf" home)
            (insert "no-auto-key-retrieve\nauto-key-locate clear\n"))
          (dolist (uid '("xmpp:alice@example.org" "xmpp:bob@example.org"
                         "Alice <alice@example.org>"))
            (jabber-test-openpgp--gpg
             "" "--pinentry-mode" "loopback" "--passphrase" ""
             "--quick-generate-key" uid "ed25519" "sign" "0"))
          (let* ((ctx (epg-make-context 'OpenPGP))
                 (alice (car (epg-list-keys ctx "=xmpp:alice@example.org")))
                 (bob (car (epg-list-keys ctx "=xmpp:bob@example.org")))
                 (other (car (epg-list-keys ctx "=Alice <alice@example.org>"))))
            (jabber-test-openpgp--gpg
             "" "--pinentry-mode" "loopback" "--passphrase" ""
             "--quick-add-key" (jabber-openpgp--key-fingerprint bob)
             "cv25519" "encr" "0")
            ;; Sign with a subkey, not the primary fingerprint in the pin.
            (jabber-test-openpgp--gpg
             "" "--pinentry-mode" "loopback" "--passphrase" ""
             "--quick-add-key" (jabber-openpgp--key-fingerprint alice)
             "ed25519" "sign" "0")
            (funcall function alice bob other)))
      (call-process "gpgconf" nil nil nil "--homedir" home "--kill" "all")
      (delete-directory home t))))

(defun jabber-test-openpgp--cipher (xml recipient &optional signer corrupt)
  "Encrypt XML for RECIPIENT, optionally using SIGNER and CORRUPT signature."
  (let* ((bytes (encode-coding-string xml 'utf-8))
         (signed (if signer
                     (jabber-test-openpgp--gpg
                      bytes "--compress-algo" "none" "--local-user"
                      (jabber-openpgp--key-fingerprint signer) "--sign")
                   bytes))
         (packet (if corrupt
                     ;; Alter only literal XML, leaving its signature intact.
                     (replace-regexp-in-string "secret body" "forged body"
                                               signed t t)
                   signed)))
    (apply #'jabber-test-openpgp--gpg packet
           (append (when signer '("--no-literal"))
                   (list "--trust-model" "always" "--recipient"
                         (jabber-openpgp--key-fingerprint recipient)
                         "--encrypt")))))

(defun jabber-test-openpgp--envelope (&optional kind)
  "Return a fresh valid envelope of KIND, defaulting to signcrypt."
  (copy-tree
   `(,(or kind 'signcrypt) ((xmlns . ,jabber-openpgp-xmlns))
     (to ((jid . "bob@example.org")))
     (time ((stamp . "2026-09-26T10:00:00Z")))
     (payload () (body ((xmlns . "jabber:client")) "secret body")))))

(defun jabber-test-openpgp--receive (cipher &optional reject from to)
  "Receive CIPHER from FROM to TO; expect refusal when REJECT is non-nil."
  (let* ((element `(openpgp ((xmlns . ,jabber-openpgp-xmlns))
                          ,(base64-encode-string cipher t)))
         (stanza (copy-tree
                  `(message ((from . ,(or from "alice@example.org/phone"))
                             (to . ,(or to "bob@example.org/laptop")))
                            (body () "fallback") ,element)))
         (before (copy-tree stanza)))
    (if reject
        (progn
          (should-error (jabber-openpgp--decrypt-stanza nil stanza element))
          (should (equal stanza before))
          ;; The real chat refusal boundary must not publish plaintext either.
          (jabber-chat--try-decrypt
           nil stanza element
           '(:decrypt jabber-openpgp--decrypt-stanza :error-label "OpenPGP"))
          (should (equal (caddr (car (jabber-xml-get-children stanza 'body)))
                         "[OpenPGP: could not decrypt]")))
      (jabber-openpgp--decrypt-stanza nil stanza element)
      (should (equal (caddr (car (jabber-xml-get-children stanza 'body)))
                     "secret body")))))

(ert-deftest jabber-test-openpgp-real-signatures ()
  "Require a valid sender signature and exact XMPP UID, including subkeys."
  (jabber-test-openpgp--with-keys
   (lambda (alice bob other)
     (let* ((jabber-openpgp-key-alist
             `(("alice@example.org" . ,(jabber-openpgp--key-fingerprint alice))))
            (xml (jabber-sexp2xml (jabber-test-openpgp--envelope))))
       (jabber-test-openpgp--receive (jabber-test-openpgp--cipher xml bob alice))
       (jabber-test-openpgp--receive (jabber-test-openpgp--cipher xml bob) t)
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher xml bob alice t) t)
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher xml bob other) t)
       ;; A configured key is not an exemption from the XMPP UID requirement.
       (let ((jabber-openpgp-key-alist
              `(("alice@example.org" . ,(jabber-openpgp--key-fingerprint other)))))
         (jabber-test-openpgp--receive
          (jabber-test-openpgp--cipher xml bob other) t))
       (let ((jabber-openpgp-key-alist nil))
         (jabber-test-openpgp--receive
          (jabber-test-openpgp--cipher xml bob alice)))
       ;; Ephemeral key lookup cannot turn a sender mismatch into authentication.
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher xml bob alice) t "mallory@example.org")
       ;; A second key claiming the same UID cannot bypass the configured pin.
       (jabber-test-openpgp--gpg
        "" "--quick-add-uid" (jabber-openpgp--key-fingerprint other)
        "xmpp:alice@example.org")
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher xml bob other) t)
       (let ((jabber-openpgp-key-alist
              `(("alice@example.org" . ,(jabber-openpgp--key-fingerprint other)))))
         (jabber-test-openpgp--receive
          (jabber-test-openpgp--cipher xml bob other))
         (jabber-test-openpgp--gpg
          "" "--quick-revoke-uid" (jabber-openpgp--key-fingerprint other)
          "xmpp:alice@example.org")
         (jabber-test-openpgp--receive
          (jabber-test-openpgp--cipher xml bob other) t))
       (jabber-test-openpgp--gpg
        "" "--quick-add-uid" (jabber-openpgp--key-fingerprint other)
        "xmpp:άλικη@example.org")
       (let ((jabber-openpgp-key-alist
              `(("άλικη@example.org" . ,(jabber-openpgp--key-fingerprint other)))))
         (jabber-test-openpgp--receive
          (jabber-test-openpgp--cipher xml bob other) nil
          "άλικη@example.org/phone"))))))

(ert-deftest jabber-test-openpgp-real-epg-evidence ()
  "Prove EasyPG decrypts bad signatures and reports signing subkeys."
  (jabber-test-openpgp--with-keys
   (lambda (alice bob _other)
     (let ((xml (jabber-sexp2xml (jabber-test-openpgp--envelope))))
       (dolist (kind '(good bad unsigned))
         (let* ((cipher (jabber-test-openpgp--cipher
                         xml bob (unless (eq kind 'unsigned) alice)
                         (eq kind 'bad)))
                (context (epg-make-context 'OpenPGP))
                (plain (epg-decrypt-string context cipher))
                (signatures (epg-context-result-for context 'verify)))
           (should (string-match-p
                    (if (eq kind 'bad) "forged body" "secret body") plain))
           (should (epg-context-result-for context 'decryption-okay))
           (if (eq kind 'unsigned)
               (should-not signatures)
             (should (= (length signatures) 1))
             (should (eq (epg-signature-status (car signatures)) kind))
             (when (eq kind 'good)
               (should-not
                (equal (epg-signature-fingerprint (car signatures))
                       (jabber-openpgp--key-fingerprint alice)))))))
       ;; No public signing key: decryption still succeeds, verification does not.
       (let ((cipher (jabber-test-openpgp--cipher xml bob alice))
             (context (epg-make-context 'OpenPGP)))
         (jabber-test-openpgp--gpg
          "" "--delete-secret-and-public-key"
          (jabber-openpgp--key-fingerprint alice))
         (should (string-match-p "secret body" (epg-decrypt-string context cipher)))
         (should-not
          (eq (epg-signature-status
               (car (epg-context-result-for context 'verify))) 'good))
         (jabber-test-openpgp--receive cipher t))))))

(ert-deftest jabber-test-openpgp-real-envelope ()
  "Reject malformed signed envelopes before publishing their plaintext."
  (jabber-test-openpgp--with-keys
   (lambda (alice bob _other)
     (let ((jabber-openpgp-key-alist
            `(("alice@example.org" . ,(jabber-openpgp--key-fingerprint alice)))))
       (dolist (change
                (list (lambda (xml) (setcdr (assq 'xmlns (cadr xml)) "wrong:ns"))
                      (lambda (xml) (setcdr (assq 'jid (cadr (nth 2 xml)))
                                           "other@example.org"))
                      (lambda (xml) (setcdr (assq 'jid (cadr (nth 2 xml)))
                                           "bob@example.org/elsewhere"))
                      (lambda (xml) (setcdr (assq 'stamp (cadr (nth 3 xml))) ""))
                      (lambda (xml) (setcdr (assq 'stamp (cadr (nth 3 xml)))
                                           "2026-02-31T10:00:00Z"))
                      (lambda (xml) (setcdr (assq 'stamp (cadr (nth 3 xml)))
                                           "2026-09-26T99:00:00Z"))
                      (lambda (xml) (setcar (nthcdr 3 xml) '(unrelated ())))
                      (lambda (xml) (nconc xml (list (copy-tree (nth 3 xml)))))
                      (lambda (xml) (setcar (nthcdr 4 xml) '(unrelated ())))
                      (lambda (xml) (nconc xml (list (copy-tree (nth 4 xml)))))
                      (lambda (xml) (setcar (nthcdr 2 xml) '(unrelated ())))
                      (lambda (xml) (setcar (cdr (nth 4 xml))
                                           '((xmlns . "wrong:ns"))))))
         (let ((xml (jabber-test-openpgp--envelope)))
           (funcall change xml)
           (jabber-test-openpgp--receive
            (jabber-test-openpgp--cipher (jabber-sexp2xml xml) bob alice) t)))
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher
         (concat (jabber-sexp2xml (jabber-test-openpgp--envelope))
                 (jabber-sexp2xml (jabber-test-openpgp--envelope))) bob alice) t)
       ;; Namespace prefixes, fractional seconds and numeric zones are legal.
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher
         "<o:signcrypt xmlns:o='urn:xmpp:openpgp:0'><o:to jid='bob@example.org/laptop'/><o:time stamp='2026-09-26T10:00:00.123+03:00'/><o:payload><body xmlns='jabber:client'>secret body</body></o:payload></o:signcrypt>"
         bob alice))))))

(ert-deftest jabber-test-openpgp-real-crypt ()
  "Preserve unsigned crypt, whose recipient list is optional."
  (jabber-test-openpgp--with-keys
   (lambda (alice bob _other)
     (let* ((xml (jabber-test-openpgp--envelope 'crypt))
            (text (jabber-sexp2xml xml)))
       (jabber-test-openpgp--receive (jabber-test-openpgp--cipher text bob))
       (jabber-test-openpgp--receive (jabber-test-openpgp--cipher text bob alice) t)
       (setcdr (cdr xml) (cdddr xml))
       (jabber-test-openpgp--receive
        (jabber-test-openpgp--cipher (jabber-sexp2xml xml) bob))))))

(provide 'jabber-test-openpgp)
;;; jabber-test-openpgp.el ends here
