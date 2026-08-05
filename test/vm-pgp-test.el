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
one carries its second part as text/plain, so it is not ours to decrypt."
  (vm-pgp-test--with-keyring
    (let ((vm-mime-decrypt-pgp-parts t))
      (vm-pgp-test--with-message (presentation "text/plain")
        (should-not (vm-pgp-test--shows presentation "Hello world!"))
        (should (vm-pgp-test--shows presentation "BEGIN PGP MESSAGE"))))))

(provide 'vm-pgp-test)

;;; vm-pgp-test.el ends here
