;;; vm-pgp-test.el --- PGP/MIME messages, decrypted for display -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; An RFC 3156 message carries what its sender wrote as an encrypted MIME
;; entity inside a `multipart/encrypted' part.  Issue #490 is a Thunderbird
;; user's mail arriving at VM as two buttons, a version stamp and a lump of
;; ciphertext, with the message itself nowhere to be seen.  VM now decrypts it
;; with EPG and displays what is inside.
;;
;; These tests build a real encrypted message: a throwaway OpenPGP key in a
;; GnuPG home of their own, and a message encrypted to it in the shape
;; Thunderbird sends, protected headers and all.  Nothing is checked in, so
;; nothing expires and no key material lives in the repository.
;;
;; They skip when there is no gpg to run, and when the temporary directory's
;; name is too long: GnuPG puts its agent socket in the home directory it is
;; given, and a Unix socket path is limited to about a hundred characters.
;; That is not a fault to work around, it is the platform.

;;; Code:

(require 'vm-test-init)

(defvar vm-pgp-test--identity "VM Test <vmtest@example.com>"
  "The identity of the throwaway key these tests make.")

(defun vm-pgp-test--gpg-program ()
  "Return the gpg program EPG would run, or nil if there is none."
  (require 'epg-config)
  (condition-case nil
      (epg-find-configuration 'OpenPGP)
    (error nil)))

(defmacro vm-pgp-test--with-keyring (&rest body)
  "Run BODY with a GnuPG home of its own holding one throwaway key.
Skips the test rather than failing when gpg is missing or the directory name is
too long for an agent socket."
  (declare (indent 0) (debug t))
  `(let* ((config (vm-pgp-test--gpg-program))
          (home (file-name-as-directory (make-temp-file "vm-pgp" t))))
     (vm-test-skip-unless config "no gpg for EPG to run")
     ;; The socket is $GNUPGHOME/S.gpg-agent, and sun_path is 104 characters on
     ;; macOS, 108 on Linux.
     (vm-test-skip-unless (< (+ (length home) (length "S.gpg-agent")) 100)
                          (format "temporary directory name too long for a \
GnuPG socket: %s" home))
     (let ((process-environment (cons (concat "GNUPGHOME=" home)
                                      process-environment))
           (epg-pinentry-mode 'loopback))
       (unwind-protect
           (progn
             (set-file-modes home #o700)
             (with-temp-file (expand-file-name "gpg.conf" home)
               (insert "batch\npinentry-mode loopback\n"))
             (with-temp-file (expand-file-name "gpg-agent.conf" home)
               (insert "allow-loopback-pinentry\n"))
             (vm-pgp-test--make-key)
             ,@body)
         ;; Stop the agent this keyring started, or it lives on with nothing
         ;; to serve and the directory it used gone.
         (ignore-errors (call-process "gpgconf" nil nil nil "--kill" "all"))
         (delete-directory home t)))))

(defun vm-pgp-test--make-key ()
  "Make a throwaway OpenPGP key with no passphrase in the current GnuPG home."
  (with-temp-buffer
    (let ((status (call-process "gpg" nil t nil
                                "--quiet" "--batch" "--passphrase" ""
                                "--quick-generate-key" vm-pgp-test--identity
                                "default" "default" "never")))
      (unless (eq status 0)
        (ert-skip (format "could not make a test key: %s" (buffer-string)))))))

(defun vm-pgp-test--context ()
  "Return an EPG context that answers the passphrase prompt with the empty one."
  (let ((context (epg-make-context 'OpenPGP)))
    (epg-context-set-passphrase-callback context (lambda (&rest _) ""))
    context))

(defun vm-pgp-test--corrupt (armor)
  "Return ARMOR with its payload mangled, so that nothing can decrypt it.
The armour headers are left alone, so the part is still recognisably PGP data
and it is the decryption that fails, which is what a message encrypted to
somebody else comes to."
  (let ((lines (split-string armor "\n")))
    (mapconcat (lambda (line)
                 (if (and (> (length line) 20)
                          (not (string-prefix-p "-----" line)))
                     (make-string (length line) ?X)
                   line))
               lines "\n")))

(defun vm-pgp-test--write-message (file &optional body-type corrupt)
  "Write a Thunderbird-shaped PGP/MIME message to FILE, as a folder.
BODY-TYPE overrides the content type of the ciphertext part, for building an
envelope that is not the shape RFC 3156 describes.  CORRUPT mangles the
ciphertext so that it cannot be decrypted."
  (require 'epg)
  (let* ((inner (concat
                 "Content-Type: multipart/mixed; boundary=\"----INNER\";"
                 " protected-headers=\"v1\"\n"
                 "Subject: test encryption\n"
                 "From: Thunderbird person <tbird@example.com>\n"
                 "To: VM User <vmtest@example.com>\n"
                 "\n"
                 "------INNER\n"
                 "Content-Type: text/plain; charset=UTF-8\n"
                 "\n"
                 "Hello world!\n"
                 "------INNER--\n"))
         (context (vm-pgp-test--context))
         (recipients (epg-list-keys context "vmtest@example.com"))
         (ciphertext (let ((epg-context context))
                       (epg-context-set-armor context t)
                       (epg-encrypt-string context inner recipients))))
    (when corrupt
      (setq ciphertext (vm-pgp-test--corrupt ciphertext)))
    (with-temp-file file
      (insert "From tbird@example.com Mon Jan  1 00:00:00 2024\n"
              "From: Thunderbird person <tbird@example.com>\n"
              "To: VM User <vmtest@example.com>\n"
              "Subject: ...\n"
              "Message-ID: <pgpmime-1@example.com>\n"
              "MIME-Version: 1.0\n"
              "Content-Type: multipart/encrypted; boundary=\"----OUTER\";\n"
              " protocol=\"application/pgp-encrypted\"\n"
              "\n"
              "This is an OpenPGP/MIME encrypted message (RFC 4880 and 3156)\n"
              "------OUTER\n"
              "Content-Type: application/pgp-encrypted\n"
              "Content-Description: PGP/MIME version identification\n\n"
              "Version: 1\n\n"
              "------OUTER\n"
              "Content-Type: " (or body-type "application/octet-stream")
              "; name=\"encrypted.asc\"\n"
              "Content-Description: OpenPGP encrypted message\n"
              "Content-Disposition: inline; filename=\"encrypted.asc\"\n\n"
              ciphertext
              "\n------OUTER--\n\n"))))

(defun vm-pgp-test--write-signed-message (file &optional tamper)
  "Write an RFC 3156 PGP-signed message to FILE, as a folder.
TAMPER alters the signed text after signing, which is what a message that was
changed on its way looks like."
  (require 'epg)
  (let* ((signed (concat "Content-Type: text/plain; charset=UTF-8\n"
                         "\n"
                         "Signed hello!\n"))
         (context (vm-pgp-test--context))
         ;; RFC 3156 signs the entity as transmitted, with CRLF endings.
         (canonical (replace-regexp-in-string "\n" "\r\n" signed t t))
         (signature (progn (epg-context-set-armor context t)
                           (epg-sign-string context canonical t))))
    (when tamper
      (setq signed (replace-regexp-in-string "Signed hello!" "Tampered!"
                                             signed t t)))
    (with-temp-file file
      (insert "From signer@example.com Mon Jan  1 00:00:00 2024\n"
              "From: Signer <vmtest@example.com>\n"
              "To: VM User <vmtest@example.com>\n"
              "Subject: signed mail\n"
              "Message-ID: <pgpsigned-1@example.com>\n"
              "MIME-Version: 1.0\n"
              "Content-Type: multipart/signed; boundary=\"----SIG\";\n"
              " micalg=pgp-sha256; protocol=\"application/pgp-signature\"\n"
              "\n"
              "------SIG\n"
              signed
              "------SIG\n"
              "Content-Type: application/pgp-signature; name=\"signature.asc\"\n"
              "Content-Description: OpenPGP digital signature\n\n"
              signature
              "\n------SIG--\n\n"))))

(defun vm-pgp-test--write-inline-message (file kind &optional tamper)
  "Write a message whose body is PGP armour to FILE, as a folder.
KIND is `encrypted\' or `signed\'.  TAMPER alters a signed body after signing."
  (require 'epg)
  (let* ((text "Inline hello!\n")
         (context (vm-pgp-test--context))
         (body
          (progn
            (epg-context-set-armor context t)
            (cond
             ((eq kind 'encrypted)
              (epg-encrypt-string context text
                                  (epg-list-keys context "vmtest@example.com")))
             (t
              (let ((clear (epg-sign-string context text 'clear)))
                (if tamper
                    (replace-regexp-in-string "Inline hello!" "Tampered!"
                                              clear t t)
                  clear)))))))
    (with-temp-file file
      (insert "From signer@example.com Mon Jan  1 00:00:00 2024\n"
              "From: Signer <vmtest@example.com>\n"
              "To: VM User <vmtest@example.com>\n"
              "Subject: inline pgp\n"
              "Message-ID: <pgpinline-1@example.com>\n"
              "MIME-Version: 1.0\n"
              "Content-Type: text/plain; charset=UTF-8\n"
              "\n"
              body
              "\n"))))

(defmacro vm-pgp-test--with-inline-message (spec &rest body)
  "Write an inline PGP folder, visit it, decode it, then run BODY.
SPEC is (BUFFER-VAR KIND &optional TAMPER), as `vm-pgp-test--write-inline-message\'
takes them."
  (declare (indent 1) (debug t))
  `(let ((vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-current-warning vm-current-warning)
         (before (buffer-list))
         ,(car spec))
     (require 'vm)
     (unwind-protect
         (let ((file (expand-file-name "inline" home)))
           (vm-pgp-test--write-inline-message file ',(nth 1 spec) ,(nth 2 spec))
           (cl-letf* ((make (symbol-function 'epg-make-context))
                      ((symbol-function 'epg-make-context)
                       (lambda (&rest args)
                         (let ((context (apply make args)))
                           (epg-context-set-passphrase-callback
                            context (lambda (&rest _) ""))
                           context))))
             (vm-visit-folder file)
             (vm-decode-mime-message)
             (setq ,(car spec) (or vm-presentation-buffer (current-buffer)))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defmacro vm-pgp-test--with-signed-message (spec &rest body)
  "Write a signed PGP/MIME folder, visit it, decode it, then run BODY.
SPEC is (BUFFER-VAR &optional TAMPER), TAMPER going to
`vm-pgp-test--write-signed-message\'."
  (declare (indent 1) (debug t))
  `(let ((vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-current-warning vm-current-warning)
         (before (buffer-list))
         ,(car spec))
     (require 'vm)
     (unwind-protect
         (let ((file (expand-file-name "signed" home)))
           (vm-pgp-test--write-signed-message file ,(nth 1 spec))
           (vm-visit-folder file)
           (vm-decode-mime-message)
           (setq ,(car spec) (or vm-presentation-buffer (current-buffer)))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defmacro vm-pgp-test--with-message (spec &rest body)
  "Write a PGP/MIME folder, visit it, decode the message, then run BODY.
SPEC is (BUFFER-VAR &optional BODY-TYPE CORRUPT).  BUFFER-VAR is bound to the
buffer the reader would be looking at; BODY-TYPE and CORRUPT go to
`vm-pgp-test--write-message\'."
  (declare (indent 1) (debug t))
  `(let ((vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         ;; A message that will not decrypt leaves a warning recorded, and it
         ;; belongs to this test rather than to the ones that follow.
         (vm-current-warning vm-current-warning)
         (before (buffer-list))
         ,(car spec))
     (require 'vm)
     (unwind-protect
         (let ((file (expand-file-name "folder" home)))
           (vm-pgp-test--write-message file ,(nth 1 spec) ,(nth 2 spec))
           ;; VM's own decryption makes its context through `epg-make-context',
           ;; and the key here has an empty passphrase that no pinentry can be
           ;; asked for in batch.
           (cl-letf* ((make (symbol-function 'epg-make-context))
                      ((symbol-function 'epg-make-context)
                       (lambda (&rest args)
                         (let ((context (apply make args)))
                           (epg-context-set-passphrase-callback
                            context (lambda (&rest _) ""))
                           context))))
             (vm-visit-folder file)
             (vm-decode-mime-message)
             (setq ,(car spec) (or vm-presentation-buffer (current-buffer)))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun vm-pgp-test--shows (buffer text)
  "Return non-nil if BUFFER holds TEXT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (and (search-forward text nil t) t))))

(ert-deftest vm-pgp-test-encrypted-message-is-decrypted-for-display ()
  "REGRESSION: a PGP/MIME message shows the message, not its parts.
Issue #490.  The sender's text is inside a `multipart/encrypted' entity, and VM
showed the two parts of the envelope, a version stamp and the ciphertext, with
save buttons and nothing to read."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts t))
      (vm-pgp-test--with-message (presentation)
        (should (vm-pgp-test--shows presentation "Hello world!"))
        ;; None of the envelope is left on display.
        (should-not (vm-pgp-test--shows presentation "application/pgp-encrypted"))
        (should-not (vm-pgp-test--shows presentation "BEGIN PGP MESSAGE"))))))

(ert-deftest vm-pgp-test-decryption-can-be-switched-off ()
  "With `vm-mime-decrypt-pgp-parts' nil the parts are displayed as they arrive.
The other side of the branch, so the fix is not simply decrypting whatever
turns up."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts nil))
      (vm-pgp-test--with-message (presentation)
        (should-not (vm-pgp-test--shows presentation "Hello world!"))
        (should (vm-pgp-test--shows presentation "application/pgp-encrypted"))))))

(ert-deftest vm-pgp-test-undecryptable-message-still-shows-its-parts ()
  "A message that cannot be decrypted is displayed part by part.
The ciphertext here is mangled, which is what a message encrypted to somebody
else amounts to: the reader is left with what arrived rather than an empty
display, and a warning saying why.

Taking the secret key away instead does not work, and that is worth knowing:
gpg-agent still holds it and decrypts happily."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts t))
      (vm-pgp-test--with-message (presentation nil t)
        (should-not (vm-pgp-test--shows presentation "Hello world!"))
        (should (vm-pgp-test--shows presentation "application/pgp-encrypted"))))))

(ert-deftest vm-pgp-test-other-encrypted-shapes-are-left-alone ()
  "A `multipart/encrypted\' that is not RFC 3156 is displayed part by part.
The two parts have to be a version stamp and an application/octet-stream; this
one carries its second part as text/plain, so the multipart handler declines it
and the envelope is shown, version stamp and all.

What then happens to the armour in that text part is the inline path\'s
business, and it decrypts it, which is why this does not assert that the
plaintext is absent: the reader gets the message either way, and the shape check
is what is under test here."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts t))
      (vm-pgp-test--with-message (presentation "text/plain")
        (should (vm-pgp-test--shows presentation "application/pgp-encrypted"))))))

;;; Signed messages

;; RFC 3156 sends a signed message as the entity itself and a detached
;; signature beside it.  VM verified only S/MIME signatures; a PGP one was
;; displayed as an attachment and never checked, unless the deprecated vm-pgg
;; was loaded.

(ert-deftest vm-pgp-test-signed-message-is-verified ()
  "A PGP-signed message shows its text and says the signature is good.
It has to be the word from EPG and not merely the presence of a report: with a
weaker assertion than this, the first version of the verification reported
\"Bad signature\" for a message the test had just signed, and passed.

The report names the key rather than saying only that the signature was good,
because whose key it was is what the reader needs."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-verify-signatures t))
      (vm-pgp-test--with-signed-message (presentation)
        (should (vm-pgp-test--shows presentation "Signed hello!"))
        (should (vm-pgp-test--shows presentation "PGP signature: Good signature"))
        (should (vm-pgp-test--shows presentation "vmtest@example.com"))
        ;; The signature part itself is not left on display as an attachment.
        (should-not (vm-pgp-test--shows presentation "signature.asc"))
        ;; And the message appears once.  A handler that does not say it is
        ;; done leaves the generic path to display the whole thing again.
        (should (= 1 (with-current-buffer presentation
                       (how-many "Signed hello!" (point-min) (point-max)))))))))

(ert-deftest vm-pgp-test-tampered-message-says-so ()
  "A signed message whose text was altered is displayed, and the report says so.
The text is shown either way: hiding it would tell the reader less than showing
it with a warning does."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-verify-signatures t))
      (vm-pgp-test--with-signed-message (presentation t)
        (should (vm-pgp-test--shows presentation "Tampered!"))
        (should (vm-pgp-test--shows presentation "PGP signature: Bad signature"))
        (should (= 1 (with-current-buffer presentation
                       (how-many "Tampered!" (point-min) (point-max)))))))))

(ert-deftest vm-pgp-test-verification-can-be-switched-off ()
  "With `vm-mime-verify-signatures\' nil the parts are shown as they arrived.
That is the default, and it leaves the signature as an attachment, which is what
VM did with PGP signatures before it could check them."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-verify-signatures nil))
      (vm-pgp-test--with-signed-message (presentation)
        (should (vm-pgp-test--shows presentation "Signed hello!"))
        (should-not (vm-pgp-test--shows presentation "PGP signature:"))))))

;;; Inline PGP, armour in the body

;; Before RFC 3156, and still from some clients, PGP mail is armour in the body
;; of an ordinary text part.  VM displayed the armour.

(ert-deftest vm-pgp-test-inline-encrypted-message-is-decrypted ()
  "An encrypted block in the body is replaced by its plaintext."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts t))
      (vm-pgp-test--with-inline-message (presentation encrypted)
        (should (vm-pgp-test--shows presentation "Inline hello!"))
        (should-not (vm-pgp-test--shows presentation "BEGIN PGP MESSAGE"))))))

(ert-deftest vm-pgp-test-inline-clearsigned-message-is-verified ()
  "A clearsigned body shows the text it signs and what the signature says."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-verify-signatures t))
      (vm-pgp-test--with-inline-message (presentation signed)
        (should (vm-pgp-test--shows presentation "Inline hello!"))
        (should (vm-pgp-test--shows presentation "PGP signature: Good signature"))
        (should-not (vm-pgp-test--shows presentation "BEGIN PGP SIGNED MESSAGE"))
        (should-not (vm-pgp-test--shows presentation "BEGIN PGP SIGNATURE"))))))

(ert-deftest vm-pgp-test-inline-tampered-message-says-so ()
  "A clearsigned body altered after signing is reported as bad, and still shown."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-verify-signatures t))
      (vm-pgp-test--with-inline-message (presentation signed t)
        (should (vm-pgp-test--shows presentation "Tampered!"))
        (should (vm-pgp-test--shows presentation "PGP signature: Bad signature"))))))

(ert-deftest vm-pgp-test-inline-armour-is-left-alone-when-switched-off ()
  "With both settings nil the armour is displayed as the text it is.
That is what VM did before it could read any of this, and it is what someone
who wants to see the armour asks for by turning the settings off."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts nil)
          (vm-mime-verify-signatures nil))
      (vm-pgp-test--with-inline-message (presentation encrypted)
        (should (vm-pgp-test--shows presentation "BEGIN PGP MESSAGE"))
        (should-not (vm-pgp-test--shows presentation "Inline hello!"))))))

(provide 'vm-pgp-test)

;;; vm-pgp-test.el ends here
