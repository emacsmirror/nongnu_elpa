;;; jabber-test-openpgp.el --- Tests for jabber-openpgp  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0373 OpenPGP send paths.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-openpgp)
(require 'jabber-openpgp-legacy)

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
                (lambda (_jc _jids callback) (funcall callback)))
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

(ert-deftest jabber-test-openpgp-muc-send-dead-buffer-still-sends ()
  "A dead originating buffer skips send hooks but the stanza still goes out."
  (let* ((muc-buffer (generate-new-buffer "*test-openpgp-muc*"))
         (pending-callback nil)
         (jabber-chat-send-hooks
          (list (lambda (_body _id) '((probe ((xmlns . "test:probe"))))))))
    (jabber-test-openpgp--with-muc-send-stubs sent
      (cl-letf (((symbol-function 'jabber-openpgp--ensure-recipient-keys)
                 (lambda (_jc _jids callback) (setq pending-callback callback))))
        (with-current-buffer muc-buffer
          (setq-local jabber-group "room@conf.example.com")
          (jabber-openpgp--send-muc 'fake-jc "hello")))
      ;; Buffer dies while the key fetch is in flight.
      (kill-buffer muc-buffer)
      (funcall pending-callback)
      (should sent)
      (should-not (jabber-xml-get-children sent 'probe)))))

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
