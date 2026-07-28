;;; vm-epg-test.el --- Tests for vm-epg.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for the EasyPG (epg) based PGP/MIME support module vm-epg.el.
;;
;; The tests avoid a real GnuPG installation wherever possible by mocking the
;; relevant epg entry points with `cl-letf'.  A handful of tests do construct a
;; real `epg-context' and are skipped when no OpenPGP configuration is found.
;;
;; Several tests are regression tests for bugs found during code review; each
;; is marked with "REGRESSION:" in its docstring.

;;; Code:

(require 'vm-test-init)
(require 'cl-lib)
(require 'seq)
(require 'rfc822)
(require 'sendmail)
(require 'vm-epg)

;;; Helpers

(defun vm-epg-test--gpg-p ()
  "Return non-nil if a usable OpenPGP configuration exists."
  (ignore-errors (epg-find-configuration 'OpenPGP)))

(defun vm-epg-test--secret-key-p ()
  "Return non-nil if a usable OpenPGP secret key exists (needed to sign)."
  (ignore-errors
    (and (vm-epg-test--gpg-p)
         ;; `epg-list-keys' with a non-nil MODE lists secret keys.
         (epg-list-keys (epg-make-context 'OpenPGP) nil t))))

(defun vm-epg-test--make-layout (type &optional parts)
  "Build a minimal MIME layout vector of content TYPE with PARTS."
  (let ((v (make-vector 17 nil)))
    (aset v 0 (list type))              ; slot 0 = type list
    (aset v 11 parts)                   ; slot 11 = parts
    v))

;;; ---------------------------------------------------------------------------
;;; CRLF utilities
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-crlf-cleanup ()
  "CRLF sequences are converted to LF."
  (with-temp-buffer
    (insert "a\r\nb\r\nc")
    (vm-epg-crlf-cleanup (point-min) (point-max))
    (should (equal (buffer-string) "a\nb\nc"))))

(ert-deftest vm-epg-test-make-crlf ()
  "LF characters are converted to CRLF."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (vm-epg-make-crlf (point-min) (point-max))
    (should (equal (buffer-string) "a\r\nb\r\nc\r\n"))))

(ert-deftest vm-epg-test-crlf-roundtrip ()
  "make-crlf followed by crlf-cleanup is the identity on LF text."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (vm-epg-make-crlf (point-min) (point-max))
    (vm-epg-crlf-cleanup (point-min) (point-max))
    (should (equal (buffer-string) "line1\nline2\nline3\n"))))

;;; ---------------------------------------------------------------------------
;;; Digest algorithm name (micalg)
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-digest-algo-name-sha256 ()
  "Digest id 8 maps to \"sha256\"."
  (should (equal (vm-epg-digest-algo-name 8) "sha256")))

(ert-deftest vm-epg-test-digest-algo-name-sha512 ()
  "REGRESSION: digest id 10 must map to \"sha512\", not the sha256 fallback.
`epg-digest-algorithm-alist' maps ID->NAME, so the lookup must use `assq'
and the entry's cdr; the original code used `rassq'/`car' and therefore
always returned the fallback."
  (should (equal (vm-epg-digest-algo-name 10) "sha512")))

(ert-deftest vm-epg-test-digest-algo-name-sha1 ()
  "REGRESSION: digest id 2 must map to \"sha1\"."
  (should (equal (vm-epg-digest-algo-name 2) "sha1")))

(ert-deftest vm-epg-test-digest-algo-name-unknown ()
  "An unknown digest id falls back to \"sha256\"."
  (should (equal (vm-epg-digest-algo-name 9999) "sha256")))

;;; ---------------------------------------------------------------------------
;;; Formatting verification results
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-format-verify-result-nil ()
  "A nil result yields a fixed message."
  (should (equal (vm-epg-format-verify-result nil) "No signature result")))

(ert-deftest vm-epg-test-format-verify-result-good ()
  "A good signature is formatted with its key id and validity."
  (let ((sig (epg-make-signature 'good "ABC123")))
    (setf (epg-signature-validity sig) 'full)
    (let ((s (vm-epg-format-verify-result (list sig))))
      (should (string-match-p "Good signature from key ABC123" s))
      (should (string-match-p "validity: full" s)))))

(ert-deftest vm-epg-test-format-verify-result-bad ()
  "A bad signature is formatted with the BAD marker."
  (let ((sig (epg-make-signature 'bad "X")))
    (setf (epg-signature-validity sig) 'unknown)
    (should (string-match-p "BAD signature"
                            (vm-epg-format-verify-result (list sig))))))

;;; ---------------------------------------------------------------------------
;;; Usable-key selection
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-find-usable-key-picks-by-capability ()
  "The first key with a subkey capable of USAGE is returned."
  (let* ((sa (epg-make-sub-key 'unknown '(sign) nil nil nil "SA" nil nil))
         (sb (epg-make-sub-key 'unknown '(encrypt sign) nil nil nil "SB" nil nil))
         (ka (epg-make-key nil))
         (kb (epg-make-key nil)))
    (setf (epg-key-sub-key-list ka) (list sa))
    (setf (epg-key-sub-key-list kb) (list sb))
    (should (eq (vm-epg-find-usable-key (list ka kb) 'encrypt) kb))
    (should (eq (vm-epg-find-usable-key (list ka kb) 'sign) ka))))

(ert-deftest vm-epg-test-find-usable-key-skips-expired ()
  "Expired/revoked subkeys are not selected."
  (let* ((s (epg-make-sub-key 'expired '(sign encrypt) nil nil nil "S" nil nil))
         (k (epg-make-key nil)))
    (setf (epg-key-sub-key-list k) (list s))
    (should (null (vm-epg-find-usable-key (list k) 'sign)))))

;;; ---------------------------------------------------------------------------
;;; MIME multipart boundary
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-multipart-boundary-format ()
  "The boundary starts with WORD+ and ends with 15 base64 characters."
  (let ((b (vm-epg-make-multipart-boundary "pgp+signed")))
    (should (string-prefix-p "pgp+signed+" b))
    (should (= (length b) (+ (length "pgp+signed+") 15)))
    (should (seq-every-p (lambda (c) (seq-contains-p vm-mime-base64-alphabet c))
                         (substring b (length "pgp+signed+"))))))

;;; ---------------------------------------------------------------------------
;;; Address extraction
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-get-emails ()
  "Recipient addresses are collected from the requested headers."
  (with-temp-buffer
    (insert "To: alice@example.com, Bob <bob@example.com>\n")
    (insert "CC: carol@example.com\n")
    (insert mail-header-separator "\n")
    (insert "body\n")
    (goto-char (point-min))
    (let ((addrs (vm-epg-get-emails '("To:" "CC:"))))
      (should (member "alice@example.com" addrs))
      (should (member "bob@example.com" addrs))
      (should (member "carol@example.com" addrs)))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: fetch missing keys (was: option only toggled armor)
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-fetch-missing-keys-when-enabled ()
  "REGRESSION: a no-pubkey signature triggers a keyserver fetch when enabled.
Previously `vm-epg-fetch-missing-keys' only toggled the context armor flag
and never fetched anything."
  (let ((received nil)
        (vm-epg-fetch-missing-keys t))
    (cl-letf (((symbol-function 'epg-receive-keys)
               (lambda (_ctx keys) (setq received keys))))
      (should (vm-epg-fetch-missing-keys-p
               'ctx (list (epg-make-signature 'no-pubkey "DEADBEEF"))))
      (should (equal received '("DEADBEEF"))))))

(ert-deftest vm-epg-test-fetch-missing-keys-when-disabled ()
  "No fetch is attempted when the option is nil."
  (let ((received nil)
        (vm-epg-fetch-missing-keys nil))
    (cl-letf (((symbol-function 'epg-receive-keys)
               (lambda (_ctx keys) (setq received keys))))
      (should-not (vm-epg-fetch-missing-keys-p
                   'ctx (list (epg-make-signature 'no-pubkey "X"))))
      (should-not received))))

(ert-deftest vm-epg-test-fetch-missing-keys-good-signature-noop ()
  "A signature with a present key requires no fetch."
  (let ((received nil)
        (vm-epg-fetch-missing-keys t))
    (cl-letf (((symbol-function 'epg-receive-keys)
               (lambda (_ctx keys) (setq received keys))))
      (should-not (vm-epg-fetch-missing-keys-p
                   'ctx (list (epg-make-signature 'good "X"))))
      (should-not received))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: encrypt with no recipient keys must error (not go symmetric)
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-encrypt-no-recipient-keys-errors ()
  "REGRESSION: encrypting with no usable recipient key must signal an error.
Passing a nil recipient list to `epg-encrypt-string' silently performs
symmetric (passphrase) encryption, which is not what the user asked for."
  (vm-test-skip-unless (vm-epg-test--gpg-p) "no OpenPGP configuration")
  (let ((encrypt-called nil))
    (cl-letf (((symbol-function 'vm-epg-prepare-composition)
               (lambda () (goto-char (point-max))))
              ((symbol-function 'vm-epg-get-recipient-keys) (lambda (_) nil))
              ((symbol-function 'epg-encrypt-string)
               (lambda (&rest _) (setq encrypt-called t) "CIPHER")))
      (with-temp-buffer
        (insert "Subject: x\n\nbody\n")
        (should-error (vm-epg-cleartext-encrypt nil))
        ;; The key point: no (symmetric) encryption was attempted.
        (should-not encrypt-called)))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: multipart/encrypted must report the part as handled (return t)
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-multipart-encrypted-t-on-decrypt-failure ()
  "REGRESSION: a failed decryption must still return t.
Otherwise `vm-decode-mime-layout' falls through and re-renders the raw
ciphertext parts as multipart/mixed.

The cipher buffer is reached through the real accessors rather than by
mocking them: `vm-buffer-of' is a `defsubst' and is inlined into the
byte-compiled function under test, so a `cl-letf' redefinition of it would
be silently ignored and the real code would `aref' a bogus value."
  (vm-test-skip-unless (vm-epg-test--gpg-p) "no OpenPGP configuration")
  (let* ((header (vm-epg-test--make-layout "application/pgp-encrypted"))
         (msg    (vm-epg-test--make-layout "application/octet-stream"))
         (top    (vm-epg-test--make-layout
                  "multipart/encrypted" (list header msg)))
         (cipher-buf (generate-new-buffer " *vm-epg-test-cipher*"))
         ;; A minimal "message object" as `vm-buffer-of' dereferences it:
         ;; (aref (aref message 1) 9) must be the buffer holding the cipher.
         (msg-inner (make-vector 10 nil))
         (msg-obj (make-vector 2 nil))
         (msg-sym (make-symbol "vm-epg-test-msg"))
         (vm-epg-auto-decrypt t))
    (unwind-protect
        (progn
          (with-current-buffer cipher-buf (insert "CIPHERTEXT"))
          (aset msg-inner 9 cipher-buf)
          (aset msg-obj 1 msg-inner)
          (set msg-sym msg-obj)
          ;; Wire the octet-stream layout to the message object and to the
          ;; cipher region: slot 13 = message symbol, slots 9/10 = body
          ;; start/end (see the `vm-mm-layout-*' accessors in vm-mime.el).
          (aset msg 13 msg-sym)
          (with-current-buffer cipher-buf
            (aset msg 9 (point-min))
            (aset msg 10 (point-max)))
          (cl-letf (((symbol-function 'vm-epg-state-set) #'ignore)
                    ((symbol-function 'vm-epg-get-mime-decoded) (lambda () nil))
                    ((symbol-function 'epg-decrypt-string)
                     (lambda (&rest _) (error "decrypt failed"))))
            (with-temp-buffer
              (should (eq t (vm-mime-display-internal-multipart/encrypted top)))
              (should (string-match-p "decrypt failed" (buffer-string))))))
      (kill-buffer cipher-buf))))

(ert-deftest vm-epg-test-multipart-encrypted-t-on-unknown-format ()
  "REGRESSION: an unrecognised multipart/encrypted structure returns t."
  (let* ((header (vm-epg-test--make-layout "text/plain"))
         (msg    (vm-epg-test--make-layout "text/plain"))
         (top    (vm-epg-test--make-layout
                  "multipart/encrypted" (list header msg))))
    (cl-letf (((symbol-function 'vm-epg-state-set) #'ignore)
              ((symbol-function 'vm-epg-get-mime-decoded) (lambda () nil)))
      (with-temp-buffer
        (should (eq t (vm-mime-display-internal-multipart/encrypted top)))
        (should (string-match-p "Unknown" (buffer-string)))))))

(ert-deftest vm-epg-test-multipart-encrypted-t-when-already-decoded ()
  "REGRESSION: an already-decoded part returns t (no re-render fall-through)."
  (let ((top (vm-epg-test--make-layout
              "multipart/encrypted"
              (list (vm-epg-test--make-layout "application/pgp-encrypted")
                    (vm-epg-test--make-layout "application/octet-stream")))))
    (cl-letf (((symbol-function 'vm-epg-state-set) #'ignore)
              ((symbol-function 'vm-epg-get-mime-decoded) (lambda () 'decoded)))
      (with-temp-buffer
        (should (eq t (vm-mime-display-internal-multipart/encrypted top)))))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: stray debug message must not corrupt format strings
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-set-signer-no-format-injection ()
  "REGRESSION: setting the signer must not crash on keys printed with a %.
The original code passed an already-formatted string as the format argument
of `message', so a key whose printed representation contained e.g. %d raised
\"Not enough arguments for format string\"."
  (vm-test-skip-unless (vm-epg-test--gpg-p) "no OpenPGP configuration")
  (cl-letf (((symbol-function 'vm-epg-get-author) (lambda () "me@example.com"))
            ((symbol-function 'epg-list-keys) (lambda (&rest _) '("k")))
            ((symbol-function 'vm-epg-find-usable-key)
             (lambda (&rest _) "signer-%d-key")))
    (let ((ctx (epg-make-context 'OpenPGP)))
      (vm-epg-set-signer ctx)
      (should (equal (epg-context-signers ctx) '("signer-%d-key"))))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: cleanup must tolerate a missing signature block
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-cleartext-cleanup-handles-missing-signature ()
  "REGRESSION: cleanup must not raise a `search-failed' error on malformed input.
The armor-stripping searches originally omitted the NOERROR argument."
  (with-temp-buffer
    (insert "-----BEGIN PGP SIGNED MESSAGE-----\n"
            "Hash: SHA256\n\n"
            "body text with no signature block\n")
    (goto-char (point-min))
    ;; Must complete without signalling.
    (should (progn (vm-epg-cleartext-cleanup 'verified "OUTPUT" nil) t))
    (should (string-match-p "OUTPUT" (buffer-string)))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: cleartext (sign-only) signatures must validate
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-sign-signs-crlf-canonical-body ()
  "REGRESSION: the detached signature must be computed over the CRLF form.
The verifier (`vm-mime-display-internal-multipart/signed') canonicalizes the
signed content to CRLF (RFC 3156) with `vm-epg-make-crlf' before checking, so
the signer must hash those same CRLF bytes.  The original code signed the raw
LF buffer text, so every sent sign-only message -- even one sent to oneself --
verified as an invalid signature.

This test needs no GnuPG: it mocks `epg-sign-string' and asserts that the
bytes handed to it are CRLF-canonical."
  (let ((mail-header-separator "--text follows this line--")
        (signed-bytes nil))
    (cl-letf (((symbol-function 'vm-epg-prepare-composition)
               (lambda ()
                 (goto-char (point-max))
                 (unless (bolp) (insert "\n"))
                 (vm-epg-goto-body-start)))
              ((symbol-function 'vm-epg-set-signer) #'ignore)
              ((symbol-function 'epg-sign-string)
               (lambda (_ctx text _mode) (setq signed-bytes text) "SIGNATURE")))
      (with-temp-buffer
        (insert "To: me@example.com\n"
                "Subject: sign test\n"
                mail-header-separator "\n"
                "first line\n"
                "second line\n"
                "third line\n")
        (vm-epg-sign-internal)
        ;; Something was signed ...
        (should signed-bytes)
        ;; ... and it is CRLF-canonical: it contains CRLF and no bare LF (an LF
        ;; either at the start of the string or not preceded by CR).
        (should (string-match-p "\r\n" signed-bytes))
        (should-not (string-match-p "\\(?:\\`\\|[^\r]\\)\n" signed-bytes))))))

(ert-deftest vm-epg-test-sign-verify-roundtrip ()
  "REGRESSION: a self-signed cleartext message must verify as good.
End-to-end check with a real GnuPG: `vm-epg-sign-internal' signs the body, and
the resulting multipart/signed part is verified exactly as the display code
does (extract the first part, canonicalize to CRLF, detached-verify against the
signature).  With the old LF-signing bug the signature came out invalid.

Requires a usable secret key; skipped otherwise, and skipped if signing itself
is unavailable (e.g. a passphrase cannot be supplied non-interactively)."
  (vm-test-skip-unless (vm-epg-test--secret-key-p) "no OpenPGP secret key")
  (let ((mail-header-separator "--text follows this line--")
        signature signed-part)
    (cl-letf (((symbol-function 'vm-epg-prepare-composition)
               (lambda ()
                 (goto-char (point-max))
                 (unless (bolp) (insert "\n"))
                 (vm-epg-goto-body-start)))
              ;; Use GnuPG's default secret key as the signer.
              ((symbol-function 'vm-epg-set-signer) #'ignore))
      (with-temp-buffer
        (insert "To: me@example.com\n"
                "Subject: roundtrip\n"
                mail-header-separator "\n"
                "one\ntwo\nthree\n")
        ;; Sign for real; skip (do not fail) if GnuPG cannot sign here.
        (condition-case err
            (vm-epg-sign-internal)
          (error (ert-skip (format "signing unavailable: %s"
                                   (error-message-string err)))))
        ;; Extract the transmitted first part and the signature just as a
        ;; receiver would -- with the LF line endings stored in the buffer.
        (goto-char (point-min))
        (re-search-forward "boundary=\"\\([^\"]+\\)\"")
        (let ((boundary (match-string 1)))
          (goto-char (point-min))
          (re-search-forward (concat "^--" (regexp-quote boundary) "\n"))
          (let ((p-start (point)))
            (re-search-forward (concat "\n--" (regexp-quote boundary) "\n"))
            (setq signed-part (buffer-substring-no-properties
                               p-start (match-beginning 0)))
            (goto-char (match-end 0))
            (re-search-forward "application/pgp-signature\n\n")
            (let ((s-start (point)))
              (re-search-forward (concat "\n--" (regexp-quote boundary) "--"))
              (setq signature (buffer-substring-no-properties
                               s-start (match-beginning 0))))))
        ;; Canonicalize the signed part to CRLF, exactly like the display code.
        (setq signed-part
              (with-temp-buffer
                (insert signed-part)
                (vm-epg-make-crlf (point-min) (point-max))
                (buffer-string)))
        (let ((context (epg-make-context 'OpenPGP)))
          (epg-verify-string context signature signed-part)
          (let ((result (epg-context-result-for context 'verify)))
            (should result)
            (should (eq (epg-signature-status (car result)) 'good))))))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: inline cleartext armor must be MIME-encoded, not inserted raw
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-cleartext-sign-encodes-armor ()
  "REGRESSION: the inline PGP armor must be transfer-encoded with the body.
When the body needs MIME encoding (e.g. a non-ASCII character forces
quoted-printable), `vm-epg-cleartext-sign' used to MIME-encode the body first
and then insert the ASCII armor verbatim into the quoted-printable part.  A
base64 signature line ending in `=' is then read as a quoted-printable soft
line break and merges with the following line (e.g. the last signature line
with `-----END PGP SIGNATURE-----'), corrupting the signature on the
receiving side.

The fix signs the raw body and MIME-encodes afterwards, so the armor's `='
bytes are escaped as `=3D'.  This test needs no GnuPG: it mocks
`epg-sign-string' to return armor whose last line ends in `=' inside a body
that forces quoted-printable, and asserts the stored armor is QP-escaped and
survives a decode intact."
  (let ((mail-header-separator "--text follows this line--")
        ;; Realistic cleartext armor: the signed text is inline and
        ;; human-readable, so it keeps the non-ASCII body (here \345 = LATIN
        ;; SMALL LETTER A WITH RING).  That non-ASCII byte is what forces the
        ;; whole part to quoted-printable.  The signature line ends in `=',
        ;; like real base64.
        (armor (concat "-----BEGIN PGP SIGNED MESSAGE-----\n"
                       "Hash: SHA512\n\n"
                       "Hej och h\345!\n"
                       "-----BEGIN PGP SIGNATURE-----\n\n"
                       "AbCdEf0123456789AbCdEf0123456789AbCdEf0123456=\n"
                       "-----END PGP SIGNATURE-----\n")))
    (cl-letf (((symbol-function 'vm-epg-set-signer) #'ignore)
              ((symbol-function 'epg-sign-string)
               (lambda (&rest _) armor)))
      (with-temp-buffer
        (mail-mode)
        (setq vm-send-using-mime t)
        (insert "To: me@example.com\n"
                "Subject: sign test\n"
                mail-header-separator "\n"
                ;; A non-ASCII byte forces quoted-printable transfer encoding.
                "Hej och h\345!\n")
        (vm-epg-cleartext-sign)
        (let ((text (buffer-string)))
          ;; The composition really was quoted-printable encoded ...
          (should (string-match-p "Content-Transfer-Encoding:[ \t]*quoted-printable"
                                  text))
          ;; ... and the armor's `=' bytes were escaped, so no bare `=' at end
          ;; of a signature line remains to be read as a soft line break.
          (should (string-match-p "=3D" text))
          (should-not (string-match-p "456=\n" text))
          ;; Decoding the body reproduces the armor with its lines intact.
          (goto-char (point-min))
          (search-forward (concat "\n" mail-header-separator "\n"))
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (with-temp-buffer
              (insert body)
              (quoted-printable-decode-region (point-min) (point-max))
              (goto-char (point-min))
              (should (search-forward
                       "456=\n-----END PGP SIGNATURE-----" nil t)))))))))

(ert-deftest vm-epg-test-cleartext-sign-verify-roundtrip ()
  "REGRESSION: an inline cleartext-signed non-ASCII message must verify good.
End-to-end with a real GnuPG: sign a body containing a non-ASCII character
(forcing quoted-printable), then decode the body as a receiver would and
detached-verify.  With the old code the armor was inserted raw into the
quoted-printable part, a `='-terminated signature line merged with the next
line, and verification failed."
  (vm-test-skip-unless (vm-epg-test--secret-key-p) "no OpenPGP secret key")
  (let ((mail-header-separator "--text follows this line--"))
    (cl-letf (((symbol-function 'vm-epg-set-signer) #'ignore))
      (with-temp-buffer
        (mail-mode)
        (setq vm-send-using-mime t)
        (insert "To: me@example.com\n"
                "Subject: roundtrip\n"
                mail-header-separator "\n"
                "Hej och h\345!\n")
        (condition-case err
            (vm-epg-cleartext-sign)
          (error (ert-skip (format "signing unavailable: %s"
                                   (error-message-string err)))))
        ;; Extract and MIME-decode the body exactly as a receiver would.
        (goto-char (point-min))
        (search-forward (concat "\n" mail-header-separator "\n"))
        (let ((armor (buffer-substring-no-properties (point) (point-max))))
          (with-temp-buffer
            (insert armor)
            (quoted-printable-decode-region (point-min) (point-max))
            (let ((context (epg-make-context 'OpenPGP)))
              (setf (epg-context-armor context) t)
              (epg-verify-string context (buffer-string))
              (let ((result (epg-context-result-for context 'verify)))
                (should result)
                (should (eq (epg-signature-status (car result)) 'good))))))))))

(provide 'vm-epg-test)

;;; vm-epg-test.el ends here