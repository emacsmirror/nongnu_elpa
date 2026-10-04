;;; vm-smime-test.el --- Tests for vm-smime.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; vm-smime.el signs, encrypts, verifies and decrypts S/MIME mail (#117), and
;; had no tests at all (#590).  These cover the parts that need no
;; certificate: which addresses a composition names, the three commands that
;; arm a composition for signing and encryption, and the choosing of
;; recipient certificates under the `links' method.
;;
;; The crypto itself is smime.el's, driven from `vm-mime-encode-composition';
;; testing it needs a key pair and is not attempted here.

;;; Code:

(require 'vm-test-init)
(require 'vm)
(require 'vm-smime)

;;; Whose message is it

(ert-deftest vm-smime-test-sender-comes-from-the-from-header ()
  "`vm-get-sender' returns the address, not the whole From header.
It picks which of `smime-keys' signs the message."
  (with-temp-buffer
    (insert "From: Me Myself <me@example.com>\nTo: you@example.com\n\nbody\n")
    (should (equal (vm-get-sender) "me@example.com"))))

(ert-deftest vm-smime-test-sender-falls-back-to-user-mail-address ()
  "A composition with no From is still signed as somebody."
  (let ((user-mail-address "fallback@example.com"))
    (with-temp-buffer
      (insert "To: you@example.com\n\nbody\n")
      (should (equal (vm-get-sender) "fallback@example.com")))))

(ert-deftest vm-smime-test-recipients-come-from-three-headers ()
  "To, Cc and Bcc all name recipients, and each needs a certificate.
Leaving Bcc out would encrypt a message its blind recipient could not read."
  (with-temp-buffer
    (insert "From: me@example.com\n"
            "To: A <a@x.com>, b@y.com\n"
            "Cc: c@z.com\n"
            "Bcc: d@w.com\n"
            "\nbody with an address in it: nobody@nowhere.com\n")
    (should (equal (sort (vm-get-recipients) #'string<)
                   '("a@x.com" "b@y.com" "c@z.com" "d@w.com")))))

(ert-deftest vm-smime-test-recipients-stop-at-the-headers ()
  "An address in the body is not a recipient.
`vm-get-recipients' narrows to the header block, so a quoted header in the
text of a message does not add anybody."
  (with-temp-buffer
    (insert "From: me@example.com\nTo: a@x.com\n\n"
            "> To: quoted@example.com\nand text\n")
    (should (equal (vm-get-recipients) '("a@x.com")))))

;;; Arming a composition

(defmacro vm-smime-test-with-composition (&rest body)
  "Run BODY in a Mail mode buffer with a key configured."
  (declare (indent 0))
  `(with-temp-buffer
     (mail-mode)
     (setq mode-name "Mail")
     (let ((smime-keys '(("me@example.com" "/nonexistent/key.pem"))))
       (insert "From: me@example.com\nTo: you@example.com\n\nbody\n")
       ,@body)))

(ert-deftest vm-smime-test-signing-is-a-flag-and-a-mode-line ()
  "`vm-smime-sign-message' arms the composition and says so."
  (vm-smime-test-with-composition
    (vm-smime-sign-message)
    (should vm-smime-sign-message)
    (should (equal mode-name "SIGNED Mail"))
    (vm-smime-sign-message)
    (should-not vm-smime-sign-message)
    (should (equal mode-name "Mail"))))

(ert-deftest vm-smime-test-encryption-is-a-flag-and-a-mode-line ()
  (vm-smime-test-with-composition
    (vm-smime-encrypt-message)
    (should vm-smime-encrypt-message)
    (should (equal mode-name "ENCRYPTED Mail"))
    (vm-smime-encrypt-message)
    (should-not vm-smime-encrypt-message)
    (should (equal mode-name "Mail"))))

(ert-deftest vm-smime-test-disarming-encryption-leaves-signing-said ()
  "REGRESSION: turning encryption off used to leave the mode line \"SINGED\".
`vm-smime-encrypt-message' rewrote \"SIGNED+\" to \"SINGED \", and
`vm-smime-sign-message' matches \"SIGNED\" -- so the misspelt marker could
not be cleared either, and the mode line claimed a state the flags did not
have for the rest of the composition.  Issue #590."
  (vm-smime-test-with-composition
    (vm-smime-sign-message)
    (vm-smime-encrypt-message)
    (should (equal mode-name "SIGNED+ENCRYPTED Mail"))
    (vm-smime-encrypt-message)
    (should vm-smime-sign-message)
    (should-not vm-smime-encrypt-message)
    (should (equal mode-name "SIGNED Mail"))
    ;; and signing can still be turned off, which is what the typo prevented
    (vm-smime-sign-message)
    (should (equal mode-name "Mail"))))

(ert-deftest vm-smime-test-sign-encrypt-toggles-both ()
  (vm-smime-test-with-composition
    (vm-smime-sign-encrypt-message)
    (should vm-smime-sign-message)
    (should vm-smime-encrypt-message)
    (should (equal mode-name "SIGNED+ENCRYPTED Mail"))
    (vm-smime-sign-encrypt-message)
    (should-not vm-smime-sign-message)
    (should-not vm-smime-encrypt-message)
    (should (equal mode-name "Mail"))))

(ert-deftest vm-smime-test-arming-needs-a-composition ()
  "The three commands refuse anywhere but a composition."
  (let ((text-quoting-style 'grave))
    (with-temp-buffer
      (fundamental-mode)
      (let ((smime-keys '(("me@example.com" "/nonexistent/key.pem"))))
        (dolist (command '(vm-smime-sign-message
                           vm-smime-encrypt-message
                           vm-smime-sign-encrypt-message))
          (should (equal (should-error (funcall command))
                         '(error "Command must be used in a VM Mail mode buffer."))))))))

(ert-deftest vm-smime-test-signing-needs-a-key ()
  "Signing without `smime-keys' says what to set, rather than failing at send.
The flag would otherwise be set on a composition that cannot be signed, and
the failure would come when the message was sent."
  (let ((smime-keys nil)
        (text-quoting-style 'grave))
    (with-temp-buffer
      (mail-mode)
      (should (string-match-p "smime-keys"
                              (cadr (should-error (vm-smime-sign-message))))))))

;;; Choosing recipient certificates

(ert-deftest vm-smime-test-certificate-links-are-found-by-address ()
  "The `links' method names a certificate after each recipient's address."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-smime" t)))
         (smime-certificate-directory dir)
         (vm-smime-get-recipient-certificate-method 'links))
    (unwind-protect
        (progn
          (write-region "cert" nil (expand-file-name "a@x.com" dir) nil 'quiet)
          (write-region "cert" nil (expand-file-name "b@y.com" dir) nil 'quiet)
          (with-temp-buffer
            (insert "From: me@example.com\nTo: a@x.com\nCc: b@y.com\n\nbody\n")
            (should (equal (sort (vm-smime-get-recipient-certfiles) #'string<)
                           (sort (list (expand-file-name "a@x.com" dir)
                                       (expand-file-name "b@y.com" dir))
                                 #'string<)))))
      (delete-directory dir t))))

(ert-deftest vm-smime-test-a-missing-certificate-is-offered-for-replacement ()
  "A recipient with no certificate is asked about, and dropped if declined.
Encrypting to a certificate that is not there cannot work, so the choice has
to be made before the message is sent."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-smime" t)))
         (smime-certificate-directory dir)
         (vm-smime-get-recipient-certificate-method 'links)
         (asked nil))
    (unwind-protect
        (progn
          (write-region "cert" nil (expand-file-name "a@x.com" dir) nil 'quiet)
          (with-temp-buffer
            (insert "From: me@example.com\nTo: a@x.com, missing@y.com\n\nbody\n")
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt) (setq asked prompt) nil)))
              (should (equal (vm-smime-get-recipient-certfiles)
                             (list (expand-file-name "a@x.com" dir)))))
            (should (string-match-p "missing@y.com" asked))))
      (delete-directory dir t))))

(provide 'vm-smime-test)

;;; vm-smime-test.el ends here
