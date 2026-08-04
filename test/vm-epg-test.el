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

(defun vm-epg-test--spec-inherits (spec)
  "Return every face named by an `:inherit' attribute anywhere in SPEC.
SPEC is a `defface' spec, i.e. a list of (DISPLAY ATTRS) clauses."
  (let (faces)
    (dolist (clause spec)
      (let ((attrs (cadr clause)))
        (while (consp attrs)
          (when (eq (car attrs) :inherit)
            (let ((value (cadr attrs)))
              (setq faces (append faces
                                  (if (listp value) value (list value))))))
          (setq attrs (cddr attrs)))))
    faces))

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
  "Converting to CRLF and back again is the identity on LF text."
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
    (should (eq (vm-epg-find-usable-key (list ka kb) 'encrypt "a@b") kb))
    (should (eq (vm-epg-find-usable-key (list ka kb) 'sign "a@b") ka))))

(ert-deftest vm-epg-test-find-usable-key-skips-expired ()
  "Expired/revoked subkeys are not selected, and no usable key is an error.
Signalling rather than returning nil is deliberate: a silently dropped key
would yield a message not encrypted to, or not signed for, the named
address."
  (let* ((s (epg-make-sub-key 'expired '(sign encrypt) nil nil nil "S" nil nil))
         (k (epg-make-key nil)))
    (setf (epg-key-sub-key-list k) (list s))
    (should-error (vm-epg-find-usable-key (list k) 'sign "a@b")
                  :type 'error)))

(ert-deftest vm-epg-test-find-usable-key-error-names-usage-and-address ()
  "The error identifies which usage and address could not be satisfied."
  (let* ((err (should-error
               (vm-epg-find-usable-key nil 'encrypt "nobody@example.com")
               :type 'error))
         (text (error-message-string err)))
    (should (string-match-p "encrypt" text))
    (should (string-match-p "nobody@example\\.com" text))))

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
      (should (vm-epg-fetch-missing-keys-maybe
               'ctx (list (epg-make-signature 'no-pubkey "DEADBEEF"))))
      (should (equal received '("DEADBEEF"))))))

(ert-deftest vm-epg-test-fetch-missing-keys-when-disabled ()
  "No fetch is attempted when the option is nil."
  (let ((received nil)
        (vm-epg-fetch-missing-keys nil))
    (cl-letf (((symbol-function 'epg-receive-keys)
               (lambda (_ctx keys) (setq received keys))))
      (should-not (vm-epg-fetch-missing-keys-maybe
                   'ctx (list (epg-make-signature 'no-pubkey "X"))))
      (should-not received))))

(ert-deftest vm-epg-test-fetch-missing-keys-good-signature-noop ()
  "A signature with a present key requires no fetch."
  (let ((received nil)
        (vm-epg-fetch-missing-keys t))
    (cl-letf (((symbol-function 'epg-receive-keys)
               (lambda (_ctx keys) (setq received keys))))
      (should-not (vm-epg-fetch-missing-keys-maybe
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
                     (lambda (&rest _) (error "Decrypt failed"))))
            (with-temp-buffer
              (should (eq t (vm-mime-display-internal-multipart/encrypted top)))
              (should (string-match-p "Decrypt failed" (buffer-string))))))
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
;;; REGRESSION: auto-verify must not be gated on us-ascii / unencoded headers
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-cleartext-candidate-p ()
  "REGRESSION: an iso-8859-1 text/plain part is a cleartext-armor candidate.
Auto-verification used to be gated on `vm-mime-plain-message-p', which
requires a us-ascii charset and unencoded headers -- irrelevant to inline
PGP and so it suppressed verification for perfectly valid messages (e.g. an
iso-8859-1 body with RFC 2047 headers).  `vm-epg-cleartext-candidate-p'
accepts any text/plain part (and a message with no MIME layout), and rejects
non-text parts."
  ;; No MIME layout at all -> candidate.
  (cl-letf (((symbol-function 'vm-mm-layout) (lambda (_) nil)))
    (should (vm-epg-cleartext-candidate-p 'msg)))
  ;; text/plain, iso-8859-1 -> candidate (the case that used to be rejected).
  (let ((layout (vm-epg-test--make-layout "text/plain")))
    (aset layout 0 '("text/plain" "charset=iso-8859-1"))
    (cl-letf (((symbol-function 'vm-mm-layout) (lambda (_) layout)))
      (should (vm-epg-cleartext-candidate-p 'msg))))
  ;; A non-text part -> not a candidate.
  (let ((layout (vm-epg-test--make-layout "application/pgp-encrypted")))
    (cl-letf (((symbol-function 'vm-mm-layout) (lambda (_) layout)))
      (should-not (vm-epg-cleartext-candidate-p 'msg)))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: transfer-decode advice must fire for 7bit/8bit parts
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-transfer-advice-fires-without-point-motion ()
  "REGRESSION: cleartext automode must run for a part that decodes in place.
The advice on `vm-mime-transfer-decode-region' used to trigger only when
point advanced during decoding.  A 7bit or 8bit part is left untouched by
transfer-decoding, so point does not move and auto-verification was skipped
-- exactly for the plain PGP-signed messages it is meant to handle.  The
advice now scans the decode region [START, END] instead."
  (let ((layout (vm-epg-test--make-layout "text/plain"))
        (automode-called nil))
    ;; 8bit encoding: the real `vm-mime-transfer-decode-region' matches no
    ;; decode branch and leaves point where it is.
    (aset layout 2 "8bit")
    (cl-letf (((symbol-function 'vm-epg-cleartext-automode)
               (lambda () (setq automode-called t))))
      (with-temp-buffer
        (insert "-----BEGIN PGP SIGNED MESSAGE-----\nbody\n")
        ;; Call through the real (advised) function; point does not move.
        (vm-mime-transfer-decode-region layout (point-min) (point-max))
        (should automode-called)))))

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
\(forcing quoted-printable), then decode the body as a receiver would and
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

;;; ---------------------------------------------------------------------------
;;; REGRESSION: `vm-epg-ask-function' action symbols must name real commands
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-ask-function-choices-name-real-commands ()
  "REGRESSION: every action offered by `vm-epg-ask-function' is dispatchable.
`vm-epg-ask-hook' invokes an action symbol ACTION as the command
`vm-epg-ACTION'.  The customize type previously offered `encrypt-and-sign',
for which no `vm-epg-encrypt-and-sign' exists, so choosing it failed with a
void-function error at send time."
  (let ((type (get 'vm-epg-ask-function 'custom-type))
        (actions nil))
    ;; Collect the `const' values from the `choice' type, ignoring nil.
    (dolist (branch (cdr type))
      (when (eq (car branch) 'const)
        (let ((value (car (last branch))))
          (when (and value (symbolp value))
            (push value actions)))))
    (should actions)
    (dolist (action actions)
      (should (fboundp (intern (format "vm-epg-%s" action)))))))

(ert-deftest vm-epg-test-prompt-action-alist-names-real-commands ()
  "Every dispatchable action in `vm-epg-prompt-action-alist' is a command.
A nil action means take none, and `quit' aborts sending; neither is
dispatched as a command."
  (dolist (entry vm-epg-prompt-action-alist)
    (let ((action (nth 1 entry)))
      (when (and action (not (eq action 'quit)))
        (should (fboundp (intern (format "vm-epg-%s" action))))))))

(ert-deftest vm-epg-test-ask-hook-rejects-undispatchable-action ()
  "An action naming no command is reported against `vm-epg-ask-function'.
The error must mention the variable, rather than surfacing as a bare
void-function error from the `intern' dispatch."
  (let ((vm-mail-send-hook '(vm-epg-ask-hook))
        (vm-epg-ask-function (lambda () 'no-such-action)))
    (let ((err (should-error (vm-epg-ask-hook) :type 'error)))
      (should (string-match-p "vm-epg-ask-function"
                              (error-message-string err))))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION: snarfing reports keys imported, not keys considered
;;; ---------------------------------------------------------------------------

(defun vm-epg-test--make-import-result (considered imported)
  "Return an `epg-import-result' reporting CONSIDERED and IMPORTED keys.
Built via the constructor rather than by slot index, so it does not depend on
the internal layout of the struct."
  (let ((result (apply #'epg-make-import-result
                       (make-list (cdr (func-arity #'epg-make-import-result))
                                  0))))
    (setf (epg-import-result-considered result) considered)
    (setf (epg-import-result-imported result) imported)
    result))

(ert-deftest vm-epg-test-format-import-result-reports-imported ()
  "REGRESSION: the import report counts keys imported, not keys considered.
Re-snarfing a key already in the keyring considers it but imports nothing, so
reporting `epg-import-result-considered' claimed an import that did not
happen.  Both `vm-epg-snarf-keys' and
`vm-mime-display-internal-application/pgp-keys' share this formatter; they
previously duplicated the logic and only one of them was correct."
  (should (equal (vm-epg-format-import-result
                  (vm-epg-test--make-import-result 5 2))
                 "Imported 2 key(s)."))
  ;; The already-have-it case: considered but not imported.
  (should (equal (vm-epg-format-import-result
                  (vm-epg-test--make-import-result 1 0))
                 "Imported 0 key(s)."))
  ;; No result at all from EPG.
  (should (equal (vm-epg-format-import-result nil) "Imported 0 key(s).")))

(ert-deftest vm-epg-test-import-report-used-by-mime-handler ()
  "The application/pgp-keys handler reports the number of keys imported.
This path was already correct -- \"When importing, show the number of
imported, not considered, keys\" fixed it, leaving only the
`vm-epg-snarf-keys' path reporting the `considered' count -- so this locks
the behaviour in rather than covering a fix.  It is the counterpart to
`vm-epg-test-format-import-result-reports-imported', which covers the
formatter both paths now share.  Driven through the handler with EPG mocked,
so it checks behaviour and not the text of the source."
  (let ((layout (vm-epg-test--make-layout "application/pgp-keys"))
        (vm-epg-auto-snarf t))
    (cl-letf (((symbol-function 'vm-epg-state-set) #'ignore)
              ((symbol-function 'vm-mime-insert-mime-body) #'ignore)
              ((symbol-function 'vm-mime-transfer-decode-region) #'ignore)
              ((symbol-function 'epg-import-keys-from-string) #'ignore)
              ((symbol-function 'epg-context-result-for)
               (lambda (_ctx _op) (vm-epg-test--make-import-result 5 2))))
      (with-temp-buffer
        (should (vm-mime-display-internal-application/pgp-keys layout))
        ;; 2 imported out of 5 considered: the report must say 2.
        (should (string-match-p "Imported 2 key(s)\\." (buffer-string)))
        (should-not (string-match-p "Imported 5" (buffer-string)))))))

;;; ---------------------------------------------------------------------------
;;; Modeline state rendering
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-mode-line-items-are-faced ()
  "REGRESSION: the faced modeline states carry usable faces.
`vm-epg-mode-line-items' was nil, so the modeline faces were unreachable."
  (should vm-epg-mode-line-items)
  (dolist (state '(verified unknown error))
    (let ((string (cdr (assq state vm-epg-mode-line-items))))
      (should (stringp string))
      (should (> (length string) 0))
      (should (facep (get-text-property 0 'face string))))))

(ert-deftest vm-epg-test-modeline-faces-inherit-a-real-face ()
  "REGRESSION: modeline faces inherit `mode-line', not the XEmacs `modeline'.
GNU Emacs removed the obsolete `modeline' alias, so the specs vm-pgg was
copied from named a face that does not exist and contributed no attributes.
Checked against the defface spec rather than the resolved attributes, which
depend on the display and are largely unset in batch mode."
  (should-not (facep 'modeline))
  (dolist (face '(vm-epg-good-signature-modeline
                  vm-epg-unknown-signature-type-modeline
                  vm-epg-error-modeline))
    (should (facep face))
    (let ((spec (get face 'face-defface-spec)))
      (should spec)
      (dolist (inherited (vm-epg-test--spec-inherits spec))
        (should (facep inherited))))))

;;; ---------------------------------------------------------------------------
;;; vm-epg-save-work: protecting the composition
;;; ---------------------------------------------------------------------------

(defun vm-epg-test--vm-epg-buffer-names ()
  "Return the names of all live vm-epg work/recovery buffers."
  (delq nil (mapcar (lambda (b)
                      (and (string-match-p "VM-EPG" (buffer-name b))
                           (buffer-name b)))
                    (buffer-list))))

(defun vm-epg-test--kill-vm-epg-buffers ()
  "Kill any leftover vm-epg work/recovery buffers."
  (dolist (name (vm-epg-test--vm-epg-buffer-names))
    (kill-buffer name)))

(ert-deftest vm-epg-test-save-work-leaves-composition-alone-on-error ()
  "A failing FUNCTION leaves the composition untouched and leaks no buffer.
The whole point of `vm-epg-save-work' is that a failed sign or encrypt does
not leave a half-rewritten message behind."
  (vm-epg-test--kill-vm-epg-buffers)
  (unwind-protect
      (with-temp-buffer
        (insert "ORIGINAL COMPOSITION")
        (should-error (vm-epg-save-work (lambda () (error "Signing failed")))
                      :type 'error)
        (should (equal (buffer-string) "ORIGINAL COMPOSITION"))
        ;; The work buffer held only a copy of the untouched composition, so
        ;; there is nothing to recover and it must not be left lying around.
        (should-not (vm-epg-test--vm-epg-buffer-names)))
    (vm-epg-test--kill-vm-epg-buffers)))

(ert-deftest vm-epg-test-save-work-copies-result-back-on-success ()
  "On success the work buffer's contents replace the composition and it dies."
  (vm-epg-test--kill-vm-epg-buffers)
  (unwind-protect
      (with-temp-buffer
        (insert "ORIGINAL")
        (cl-letf (((symbol-function 'vm-mail-mode-show-headers) #'ignore))
          (vm-epg-save-work (lambda () (erase-buffer) (insert "SIGNED"))))
        (should (equal (buffer-string) "SIGNED"))
        (should-not (vm-epg-test--vm-epg-buffer-names)))
    (vm-epg-test--kill-vm-epg-buffers)))

(ert-deftest vm-epg-test-save-work-preserves-result-if-overwrite-fails ()
  "REGRESSION: a failure mid-overwrite must not destroy FUNCTION's result.
Once the composition has been erased, the work buffer holds the only copy.
It has to survive, under a name the user can find and that the next vm-epg
command will not erase -- the work buffer's own name is fixed and starts
with a space, so it is both reused and hidden from the buffer list."
  (vm-epg-test--kill-vm-epg-buffers)
  (unwind-protect
      (let ((real (symbol-function #'insert-buffer-substring))
            (calls 0))
        (with-temp-buffer
          (insert "ORIGINAL COMPOSITION")
          (cl-letf (((symbol-function 'vm-mail-mode-show-headers) #'ignore)
                    ((symbol-function 'insert-buffer-substring)
                     ;; Call 1 fills the work buffer; call 2 is the copy back
                     ;; into the composition, i.e. the dangerous window.
                     (lambda (&rest args)
                       (setq calls (1+ calls))
                       (if (= calls 2)
                           (error "Interrupted while overwriting")
                         (apply real args)))))
            (should-error (vm-epg-save-work
                           (lambda () (erase-buffer) (insert "SIGNED RESULT")))
                          :type 'error))
          (should (= calls 2)))
        ;; The result survived, under a visible name.
        (let ((names (vm-epg-test--vm-epg-buffer-names)))
          (should (= (length names) 1))
          (let ((name (car names)))
            (should (string-prefix-p "*VM-EPG-RECOVERY*" name))
            (should-not (string-prefix-p " " name))
            (should (equal (with-current-buffer name (buffer-string))
                           "SIGNED RESULT")))))
    (vm-epg-test--kill-vm-epg-buffers)))

(ert-deftest vm-epg-test-save-work-recovery-survives-a-later-run ()
  "REGRESSION: a later vm-epg command must not clobber a recovery buffer.
The work buffer has one fixed name, so before it was renamed aside the next
`vm-epg-save-work' call erased and then killed the only copy of the result."
  (vm-epg-test--kill-vm-epg-buffers)
  (unwind-protect
      (let ((real (symbol-function #'insert-buffer-substring))
            (calls 0))
        ;; First run: fails mid-overwrite, leaving a recovery buffer.
        (with-temp-buffer
          (insert "FIRST")
          (cl-letf (((symbol-function 'vm-mail-mode-show-headers) #'ignore)
                    ((symbol-function 'insert-buffer-substring)
                     (lambda (&rest args)
                       (setq calls (1+ calls))
                       (if (= calls 2)
                           (error "Interrupted while overwriting")
                         (apply real args)))))
            (should-error (vm-epg-save-work
                           (lambda () (erase-buffer) (insert "PRECIOUS")))
                          :type 'error)))
        ;; Second, unrelated and successful run.
        (with-temp-buffer
          (insert "SECOND")
          (cl-letf (((symbol-function 'vm-mail-mode-show-headers) #'ignore))
            (vm-epg-save-work (lambda () (erase-buffer) (insert "OTHER")))))
        ;; The recovery buffer and its contents are still there.
        (let ((names (vm-epg-test--vm-epg-buffer-names)))
          (should (= (length names) 1))
          (should (equal (with-current-buffer (car names) (buffer-string))
                         "PRECIOUS"))))
    (vm-epg-test--kill-vm-epg-buffers)))

;;; ---------------------------------------------------------------------------
;;; vm-pgg / vm-epg conflict detection
;;; ---------------------------------------------------------------------------

(ert-deftest vm-epg-test-pgg-conflict-warning-when-vm-pgg-loaded ()
  "REGRESSION: vm-epg warns when vm-pgg is also loaded.
vm-pgg warns when it is loaded *after* vm-epg, but the migration order --
an existing configuration that already requires vm-pgg gaining a
\(require 'vm-epg) -- was silent in both directions even though vm-epg
then overrides vm-pgg's MIME handlers."
  ;; `features' is not a special variable, so under lexical binding it cannot
  ;; be let-bound in a way the C-level `featurep' would see.  Register the
  ;; feature for real and undo it afterwards.
  (let ((already (featurep 'vm-pgg)))
    (unwind-protect
        (progn
          (provide 'vm-pgg)
          (let ((warning (vm-epg-pgg-conflict-warning)))
            (should (stringp warning))
            (should (string-match-p "vm-pgg" warning))
            (should (string-match-p "vm-mime-display-internal" warning))))
      (unless already
        (setq features (delq 'vm-pgg features))))))

(ert-deftest vm-epg-test-no-pgg-conflict-warning-when-vm-pgg-absent ()
  "No conflict warning is produced when vm-pgg is not loaded."
  (skip-unless (not (featurep 'vm-pgg)))
  (should-not (vm-epg-pgg-conflict-warning)))

(provide 'vm-epg-test)

;;; vm-epg-test.el ends here