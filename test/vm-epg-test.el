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
         ;; a message of VM's own making, whose buffer holds the cipher: the
         ;; slot numbers are not written down here, a hand-built vector
         ;; having had to be renumbered with them (emacs-vm/vm#861)
         (msg-obj (vm-make-message))
         (msg-sym (make-symbol "vm-epg-test-msg"))
         (vm-epg-auto-decrypt t))
    (unwind-protect
        (progn
          (with-current-buffer cipher-buf (insert "CIPHERTEXT"))
          (vm-set-buffer-of msg-obj cipher-buf)
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
        ;; A decode for display, which is the only kind the advice follows
        ;; since #581.
        (setq major-mode 'vm-presentation-mode)
        (insert "-----BEGIN PGP SIGNED MESSAGE-----\nbody\n")
        ;; Call through the real (advised) function; point does not move.
        (vm-mime-transfer-decode-region layout (point-min) (point-max))
        (should automode-called)))))

(ert-deftest vm-epg-test-transfer-advice-survives-a-shrinking-decode ()
  "REGRESSION: decoding a whole buffer must not put the scan region past its end.
The advice scanned [START, END] as they arrived, but quoted-printable and
base64 decoding replace the region by something shorter.  Where the region is
the whole buffer -- `vm-mime-send-body-to-file' decodes from point-min to
point-max of a work buffer -- the old END is outside the buffer afterwards and
`narrow-to-region' signalled `args-out-of-range'.

`vm-mime-send-body-to-file' swallows the error and warns, so with vm-epg loaded
writing a quoted-printable text part to a file wrote nothing at all, and an
HTML part sent to an external viewer arrived empty."
  (let ((layout (vm-epg-test--make-layout "text/plain"))
        (automode-region nil))
    (aset layout 2 "quoted-printable")
    (cl-letf (((symbol-function 'vm-epg-cleartext-automode)
               (lambda () (setq automode-region (cons (point-min) (point-max))))))
      (with-temp-buffer
        ;; as above: for display, so the advice follows it
        (setq major-mode 'vm-presentation-mode)
        ;; "=41" decodes to "A", so the region loses two characters of three.
        (insert "=41=42=43\n")
        (let ((size (buffer-size)))
          (vm-mime-transfer-decode-region layout (point-min) (point-max))
          (should (= (buffer-size) (- size 6)))
          ;; the region scanned is the decoded text, not the bytes it came from
          (should (equal automode-region (cons (point-min) (point-max)))))))))

(ert-deftest vm-epg-test-transfer-advice-only-follows-a-display ()
  "REGRESSION: a decode that is not for display does not run the automode.
Issue #581.  VM transfer-decodes for several reasons: `vm-mime-send-body-to-file'
writing a part to a file or handing one to an external viewer,
`vm-mime-send-body-to-folder', yanking a message into a composition, vm-vcard,
vm-w3m.  Those decode in a work buffer or a composition, where nothing is on
display and the automode has no message to work from, yet the advice ran there
too.

It did nothing, but only by accident: `vm-message-pointer' is nil in a work
buffer and `vm-epg-cleartext-decoded' is buffer-local and so nil there as well,
so the already-handled test compared nil with nil and took the do-nothing
branch.  This pins the intent instead of the accident."
  (let ((layout (vm-epg-test--make-layout "text/plain"))
        (calls 0))
    (aset layout 2 "8bit")
    (cl-letf (((symbol-function 'vm-epg-cleartext-automode)
               (lambda () (setq calls (1+ calls)))))
      ;; a work buffer, as `vm-mime-send-body-to-file' decodes in
      (with-temp-buffer
        (insert "-----BEGIN PGP SIGNED MESSAGE-----\nbody\n")
        (vm-mime-transfer-decode-region layout (point-min) (point-max))
        (should (= 0 calls)))
      ;; a composition, as yanking a message decodes in
      (with-temp-buffer
        (mail-mode)
        (insert "-----BEGIN PGP SIGNED MESSAGE-----\nbody\n")
        (vm-mime-transfer-decode-region layout (point-min) (point-max))
        (should (= 0 calls)))
      ;; and the display path still does
      (with-temp-buffer
        (setq major-mode 'vm-presentation-mode)
        (insert "-----BEGIN PGP SIGNED MESSAGE-----\nbody\n")
        (vm-mime-transfer-decode-region layout (point-min) (point-max))
        (should (= 1 calls))))))

(ert-deftest vm-epg-test-cleartext-display-buffer-p ()
  "The three modes a message is displayed in, and nothing else.
Issue #581."
  (dolist (mode '(vm-mode vm-virtual-mode vm-presentation-mode))
    (with-temp-buffer
      (setq major-mode mode)
      (should (vm-epg-cleartext-display-buffer-p))))
  (dolist (mode '(fundamental-mode mail-mode vm-summary-mode text-mode))
    (with-temp-buffer
      (setq major-mode mode)
      (should-not (vm-epg-cleartext-display-buffer-p)))))

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

Signed with the keyring the tests make for themselves, so it runs wherever
gpg does rather than only where the person running it happens to have a
secret key -- and it can never sign with theirs."
  (vm-epg-test--with-a-test-keyring
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
            (should (eq (epg-signature-status (car result)) 'good)))))))))


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
  (vm-epg-test--with-a-test-keyring
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
                (should (eq (epg-signature-status (car result)) 'good)))))))))))


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
  "Kill any leftover vm-epg work/recovery buffers, and the warnings they made.
The recovery path calls `display-warning', which creates `*Warnings*' if it is
not already there, and that outlives the test (issue #559)."
  (dolist (name (vm-epg-test--vm-epg-buffer-names))
    (kill-buffer name))
  (when (get-buffer "*Warnings*")
    (kill-buffer "*Warnings*")))

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

;;; A keyring of our own (emacs-vm/vm#643)
;;
;; The tests below that sign or encrypt used whatever secret key the machine
;; running them happened to have, which is the developer's own: not
;; reproducible, and not something a test suite should be reaching for.  They
;; make a keyring of their own instead.
;;
;; Generating a key costs about a third of a second with ed25519, so there is
;; no reason to keep one in the repository.  A committed private key is one a
;; scanner flags, one nobody can trust afterwards, and one that expires while
;; nobody is looking.

(defconst vm-epg-test--address "vm-test@example.invalid"
  "The address of the throwaway key.  The .invalid domain cannot resolve.")

(defun vm-epg-test--gpg-program ()
  "The gpg to test with, or nil when there is none."
  (or (executable-find "gpg") (executable-find "gpg2")))

(defun vm-epg-test--generate-key (user-id)
  "Generate a passphrase-less ed25519 key for USER-ID in the current GNUPGHOME.
Returns gpg's exit status.  ed25519 takes about a third of a second, which is
why these tests make keys instead of keeping one in the repository."
  (call-process (vm-epg-test--gpg-program) nil nil nil
                "--batch" "--pinentry-mode" "loopback"
                "--passphrase" "" "--quick-generate-key" user-id
                "default" "default" "never"))

(defmacro vm-epg-test--with-a-test-keyring (&rest body)
  "Run BODY with GNUPGHOME pointing at a fresh keyring holding one key.

The key is generated here rather than kept in the repository, and the home is
thrown away afterwards, agent and all.  Nothing here can reach the keyring of
whoever is running the tests: `epg-gpg-home-directory' and the GNUPGHOME in
`process-environment' both point at the temporary one, so a test cannot sign
with a real key or wake a passphrase prompt."
  (declare (indent 0) (debug t))
  `(let ((gpg (vm-epg-test--gpg-program)))
     (vm-test-skip-unless
      gpg
      "No gpg on PATH.  Install GnuPG to run the tests that sign and encrypt.")
     (let ((home (make-temp-file "vm-epg-home" t)))
       (set-file-modes home #o700)
       (unwind-protect
           (let* ((process-environment
                   (cons (concat "GNUPGHOME=" home) process-environment))
                  (epg-gpg-home-directory home)
                  (user-mail-address vm-epg-test--address)
                  (generated
                   (vm-epg-test--generate-key
                    (format "VM Test <%s>" vm-epg-test--address))))
             (vm-test-skip-unless
              (equal generated 0)
              "gpg could not generate a key; see its output for why")
             ,@body)
         ;; the agent is per home directory and outlives the test that started
         ;; it, so it is stopped before the directory goes
         (call-process gpg nil nil nil "--homedir" home "--quit-agent")
         (ignore-errors
           (call-process "gpgconf" nil nil nil "--homedir" home
                         "--kill" "gpg-agent"))
         (delete-directory home t)))))

(defmacro vm-epg-test--in-a-composition (&rest body)
  "Run BODY in a composition addressed to and from the test key."
  (declare (indent 0) (debug t))
  `(let ((mail-header-separator "--text follows this line--")
         (vm-send-using-mime t))
     (with-temp-buffer
       (mail-mode)
       (insert "From: VM Test <" vm-epg-test--address ">\n"
               "To: VM Test <" vm-epg-test--address ">\n"
               "Subject: for the keyring\n"
               mail-header-separator "\n"
               "A body to work on.\n")
       ,@body)))

(defun vm-epg-test--decrypt-composition ()
  "Decrypt the armored message in the current composition and return it.

Returns a cons of the plain text and the list of signatures it carried, so a
test can tell an encrypted message from a signed-and-encrypted one.  The
alternative is to assert on the ciphertext, which is the same either way."
  (goto-char (point-min))
  (should (re-search-forward
           "-----BEGIN PGP MESSAGE-----\\(.\\|\n\\)*-----END PGP MESSAGE-----"
           nil t))
  (let ((armor (match-string 0))
        (context (epg-make-context 'OpenPGP)))
    (setf (epg-context-home-directory context) epg-gpg-home-directory)
    (let ((plain (epg-decrypt-string context armor)))
      (cons plain (epg-context-result-for context 'verify)))))

(ert-deftest vm-epg-test-the-test-keyring-holds-only-its-own-key ()
  "The keyring the tests use has one key in it, and it is the test key.

If this fails, the tests below are using somebody's real keyring, which is
what this whole fixture exists to prevent."
  (vm-epg-test--with-a-test-keyring
    (let ((context (epg-make-context 'OpenPGP)))
      (setf (epg-context-home-directory context) epg-gpg-home-directory)
      (let ((owners (mapcar (lambda (key)
                              (epg-user-id-string
                               (car (epg-key-user-id-list key))))
                            (epg-list-keys context nil t))))
        (should (equal owners
                       (list (format "VM Test <%s>" vm-epg-test--address))))))))

(ert-deftest vm-epg-test-signing-a-composition ()
  "`vm-epg-sign' signs the composition, and the signature is a MIME part
rather than armor in the body: that is what multipart/signed means."
  (vm-epg-test--with-a-test-keyring
    (vm-epg-test--in-a-composition
      (cl-letf (((symbol-function 'vm-epg-set-signer) #'ignore))
        (vm-epg-sign))
      (let ((composed (buffer-string)))
        (should (string-match-p "multipart/signed" composed))
        ;; the part itself must be typed as the signature.  Matching the
        ;; string anywhere would be answered by the protocol= parameter of
        ;; the enclosing multipart, which says nothing about the part.
        (should (string-match-p "^Content-Type: application/pgp-signature$"
                                composed))
        (should (string-match-p "BEGIN PGP SIGNATURE" composed))
        ;; the text is still readable: signing does not hide it
        (should (string-match-p "A body to work on" composed))))))

(ert-deftest vm-epg-test-encrypting-a-composition ()
  "`vm-epg-encrypt' encrypts to the recipients, and the body is gone from
the composition: an encrypted message that still carried its plain text
would be the worst possible bug in this file."
  (vm-epg-test--with-a-test-keyring
    (vm-epg-test--in-a-composition
      (cl-letf (((symbol-function 'vm-epg-set-signer) #'ignore))
        (vm-epg-encrypt))
      (let ((composed (buffer-string)))
        (should (string-match-p "multipart/encrypted" composed))
        (should (string-match-p "application/pgp-encrypted" composed))
        (should (string-match-p "BEGIN PGP MESSAGE" composed))
        (should-not (string-match-p "A body to work on" composed)))
      ;; the recipient gets the text back, and it carries no signature:
      ;; signing is what the prefix argument is for
      (let ((decrypted (vm-epg-test--decrypt-composition)))
        (should (string-match-p "A body to work on" (car decrypted)))
        (should-not (cdr decrypted))))))

(ert-deftest vm-epg-test-signing-and-encrypting-together ()
  "`vm-epg-sign-and-encrypt' does both in one step, and the result is an
encrypted message: the signature is inside it, where only the recipient can
see it."
  (vm-epg-test--with-a-test-keyring
    (vm-epg-test--in-a-composition
      (cl-letf (((symbol-function 'vm-epg-set-signer) #'ignore))
        (vm-epg-sign-and-encrypt))
      (let ((composed (buffer-string)))
        (should (string-match-p "multipart/encrypted" composed))
        (should-not (string-match-p "A body to work on" composed)))
      ;; decrypting is the only way to see the signature, which is the whole
      ;; difference between this command and `vm-epg-encrypt'
      (let ((decrypted (vm-epg-test--decrypt-composition)))
        (should (string-match-p "A body to work on" (car decrypted)))
        (should (cdr decrypted))
        (should (eq (epg-signature-status (car (cdr decrypted))) 'good))))))

(defun vm-epg-test--import-attached-key (composed)
  "Import the application/pgp-keys part of COMPOSED and return its user IDs.

The import goes into a keyring of its own, as a correspondent's would, so
this says the attached bytes are a usable key rather than merely that a part
with the right Content-Type is present."
  (should (string-match
           (concat "Content-Type: application/pgp-keys\\(?:.\\|\n\\)*?"
                   "Content-Transfer-Encoding: base64\n\n"
                   "\\(\\(?:.\\|\n\\)*?\\)\n--")
           composed))
  (let ((key (base64-decode-string (match-string 1 composed)))
        (home (make-temp-file "vm-epg-import" t)))
    (set-file-modes home #o700)
    (unwind-protect
        (let ((context (epg-make-context 'OpenPGP)))
          (setf (epg-context-home-directory context) home)
          (epg-import-keys-from-string context key)
          (mapcar (lambda (k)
                    (epg-user-id-string (car (epg-key-user-id-list k))))
                  (epg-list-keys context)))
      (ignore-errors
        (call-process "gpgconf" nil nil nil "--homedir" home
                      "--kill" "gpg-agent"))
      (delete-directory home t))))

(ert-deftest vm-epg-test-attaching-a-public-key ()
  "`vm-epg-attach-public-key' puts the author's key in the composition as an
application/pgp-keys part, which is how a correspondent gets the key to
answer with."
  (vm-epg-test--with-a-test-keyring
    ;; a second key in the ring, so "the author's key" is a claim the test can
    ;; actually check: with one key in the keyring, exporting the wrong one
    ;; and exporting the right one look identical
    (should (equal 0 (vm-epg-test--generate-key
                      "Decoy <decoy@example.invalid>")))
    (vm-epg-test--in-a-composition
      (goto-char (point-max))
      (unwind-protect
          (progn
            (vm-epg-attach-public-key)
            (vm-mime-encode-composition)
            (let ((composed (buffer-string)))
              (should (string-match-p "^Content-Type: application/pgp-keys"
                                      composed))
              ;; named for the author, which is what the recipient sees,
              ;; in the part's name parameter and in its disposition alike.
              ;; The name pattern needs the leading delimiter: without it,
              ;; "filename=" answers for "name=" and the parameter goes
              ;; unchecked.
              (let ((named (concat "=\"" (regexp-quote vm-epg-test--address)
                                   "\\.asc\"")))
                (should (string-match-p (concat "[ \t;]name" named) composed))
                (should (string-match-p (concat "filename" named) composed)))
              ;; and the part is the key: a correspondent can import it
              (should (equal (vm-epg-test--import-attached-key composed)
                             (list (format "VM Test <%s>"
                                           vm-epg-test--address))))))
        ;; the command keeps the exported key in a buffer of its own, as the
        ;; source of the attachment; it is ours to kill afterwards
        (let ((exported (get-buffer (concat " *public key of "
                                            vm-epg-test--address "*"))))
          (when exported (kill-buffer exported)))))))

(ert-deftest vm-epg-test-inserting-a-public-key ()
  "REGRESSION: `vm-epg-insert-public-key' inserts ASCII armor, as it says.
The context it exported through had armor off, so the binary key packet went
into the message body: the recipient cannot import that, and it is not text.
`vm-epg-attach-public-key' shares the omission and is unaffected, since its
export becomes a base64 MIME part."
  (vm-epg-test--with-a-test-keyring
    (vm-epg-test--in-a-composition
      (goto-char (point-max))
      (vm-epg-insert-public-key)
      (let ((composed (buffer-string)))
        (should (string-match-p "BEGIN PGP PUBLIC KEY BLOCK" composed))
        (should (string-match-p "END PGP PUBLIC KEY BLOCK" composed))
        ;; inline, where the attach command would have made a MIME part
        (should-not (string-match-p "application/pgp-keys" composed))
        ;; and it is text: every character survives a text/plain body
        (should (string-match-p "\\`[[:print:][:space:]]*\\'" composed))))))


;;; The inline (cleartext) commands, over the test keyring

(defun vm-epg-test--armored-context ()
  "An OpenPGP context on the test keyring, producing ASCII armor."
  (let ((context (epg-make-context 'OpenPGP)))
    (setf (epg-context-armor context) t)
    (setf (epg-context-home-directory context) epg-gpg-home-directory)
    context))

(defun vm-epg-test--encrypt-to-the-test-key (plain &optional sign)
  "Return PLAIN encrypted to the test key as ASCII armor, signed if SIGN."
  (let* ((context (vm-epg-test--armored-context))
         (keys (epg-list-keys context vm-epg-test--address)))
    (should keys)
    (when sign
      (setf (epg-context-signers context)
            (epg-list-keys context vm-epg-test--address t)))
    (epg-encrypt-string context plain keys sign)))

(defun vm-epg-test--clearsign (plain)
  "Return PLAIN as an inline cleartext-signed message, signed by the test key."
  (let ((context (vm-epg-test--armored-context)))
    (setf (epg-context-signers context)
          (epg-list-keys context vm-epg-test--address t))
    (epg-sign-string context plain 'clear)))

(defmacro vm-epg-test--with-a-message (body-text &rest body)
  "Run BODY in a folder of one message whose body is BODY-TEXT.

The buffer is put in `vm-mode' because that is what the commands validate
against; without it they refuse with \"No VM folder buffer associated with
this buffer\" before reaching anything worth testing."
  (declare (indent 1) (debug t))
  `(vm-test-with-folder
       (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
               "From: sender@example.com\n"
               "Subject: an inline PGP message\n\n"
               ,body-text "\n")
     (setq major-mode 'vm-mode)
     ;; the folder buffer must be current when `vm-test-with-folder' cleans
     ;; up, or it reads `vm-presentation-buffer' in whatever buffer the
     ;; command left behind and the presentation copy is never killed
     (save-current-buffer
       ,@body)))

(defmacro vm-epg-test--with-a-keyring-and-a-folder (body-text &rest body)
  "Run BODY over a one-message folder whose body is BODY-TEXT, with a keyring.
BODY-TEXT is evaluated inside the keyring, so it can encrypt or sign with the
test key."
  (declare (indent 1) (debug t))
  `(vm-epg-test--with-a-test-keyring
     (vm-epg-test--with-a-message ,body-text ,@body)))

(ert-deftest vm-epg-test-cleartext-decrypt-shows-the-plain-text ()
  "`vm-epg-cleartext-decrypt' puts the plain text in the presentation copy.
The armor is gone from it, and the state line says the message was
encrypted."
  (vm-epg-test--with-a-keyring-and-a-folder
   (vm-epg-test--encrypt-to-the-test-key "the secret body\n")
   (vm-epg-cleartext-decrypt)
   (should (string-match-p "the secret body" (buffer-string)))
   (should-not (string-match-p "BEGIN PGP MESSAGE" (buffer-string)))
   (should (member " encrypted" vm-epg-state))))

(ert-deftest vm-epg-test-cleartext-decrypt-leaves-the-folder-alone ()
  "The folder keeps the ciphertext: only the presentation copy is rewritten.
Decrypting into the folder would write the plain text to disk on the next
save, which is the opposite of what the sender asked for."
  (vm-epg-test--with-a-keyring-and-a-folder
   (vm-epg-test--encrypt-to-the-test-key "the secret body\n")
   (let ((folder (current-buffer)))
     (vm-epg-cleartext-decrypt)
     (should-not (eq (current-buffer) folder))
     (with-current-buffer folder
       (should (string-match-p "BEGIN PGP MESSAGE" (buffer-string)))
       (should-not (string-match-p "the secret body" (buffer-string)))))))

(ert-deftest vm-epg-test-cleartext-decrypt-refuses-a-read-only-folder ()
  "A read-only folder is refused, as the docstring says.
Only the presentation copy is written, so the check is a policy rather than
a necessity -- which is exactly why it needs a test to keep it."
  (vm-epg-test--with-a-keyring-and-a-folder
   (vm-epg-test--encrypt-to-the-test-key "the secret body\n")
   (let ((vm-folder-read-only t)
         (text-quoting-style 'grave))
     (let ((err (should-error (vm-epg-cleartext-decrypt) :type 'error)))
       (should (string-match-p "read-only" (error-message-string err)))))))

(ert-deftest vm-epg-test-cleartext-decrypt-reports-a-failure-in-place ()
  "A message that cannot be decrypted shows the error where the armor was.
The armor is left in place -- there is nothing to replace it with -- and the
state says error rather than encrypted."
  (vm-epg-test--with-a-keyring-and-a-folder
   ;; armor that is not decryptable with any key we have
   (concat "-----BEGIN PGP MESSAGE-----\n\n"
           "hQEMAwAAAAAAAAAAAQf/YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=\n"
           "=abcd\n-----END PGP MESSAGE-----")
   (vm-epg-cleartext-decrypt)
   (should (member " ERROR" vm-epg-state))
   (should (string-match-p "BEGIN PGP MESSAGE" (buffer-string)))))

(ert-deftest vm-epg-test-cleartext-decrypt-verifies-a-signed-plain-text ()
  "When the plain text is itself inline-signed, the signature is verified too.
Encrypt-then-sign is the common case, and the reader should be told about the
signature without a second command."
  (vm-epg-test--with-a-keyring-and-a-folder
   (vm-epg-test--encrypt-to-the-test-key
    (vm-epg-test--clearsign "signed and then encrypted\n"))
   (vm-epg-cleartext-decrypt)
   (should (string-match-p "signed and then encrypted" (buffer-string)))
   (should (member " verified" vm-epg-state))))

(ert-deftest vm-epg-test-cleartext-verify-reports-a-good-signature ()
  "`vm-epg-cleartext-verify' replaces the armor with a description of the
signature and reports it verified."
  (vm-epg-test--with-a-test-keyring
    (with-temp-buffer
      (setq major-mode 'vm-presentation-mode)
      (insert (vm-epg-test--clearsign "text that was signed\n"))
      (vm-epg-cleartext-verify)
      (let ((shown (buffer-string)))
        (should (member " verified" vm-epg-state))
        ;; the text survives; the armor around it does not
        (should (string-match-p "text that was signed" shown))
        (should-not (string-match-p "BEGIN PGP SIGNATURE" shown))
        (should (string-match-p "Good signature from key" shown))))))

(ert-deftest vm-epg-test-cleartext-verify-reports-a-broken-signature ()
  "Text altered after signing is reported as an error, not as verified.
This is the whole point of the command, so it is worth asserting that a
signature that does not check comes back distinguishable from one that
does."
  (vm-epg-test--with-a-test-keyring
    (with-temp-buffer
      (setq major-mode 'vm-presentation-mode)
      (insert (vm-epg-test--clearsign "text that was signed\n"))
      (goto-char (point-min))
      (should (search-forward "text that was signed" nil t))
      (replace-match "text that was tampered with")
      (vm-epg-cleartext-verify)
      (should (member " ERROR" vm-epg-state))
      (should-not (member " verified" vm-epg-state)))))

;;; Who a message is encrypted to (#782)

(defconst vm-epg-test--author-address "vm-author@example.invalid"
  "The address the two-key tests compose from.")

(defconst vm-epg-test--recipient-address "vm-recipient@example.invalid"
  "The address the two-key tests compose to.")

(defmacro vm-epg-test--with-two-keys (&rest body)
  "Run BODY with a fresh keyring holding an author key and a recipient key.
`vm-epg-test--with-a-test-keyring' makes one key and addresses everything to
it, which cannot tell encrypting to the recipient from encrypting to the
author.  BODY sees `home', the GNUPGHOME the keys are in."
  (declare (indent 0) (debug t))
  `(let ((gpg (vm-epg-test--gpg-program)))
     (vm-test-skip-unless
      gpg
      "No gpg on PATH.  Install GnuPG to run the tests that sign and encrypt.")
     (let ((home (make-temp-file "vm-epg-home" t)))
       (set-file-modes home #o700)
       (unwind-protect
           (let* ((process-environment
                   (cons (concat "GNUPGHOME=" home) process-environment))
                  (epg-gpg-home-directory home)
                  (user-mail-address vm-epg-test--author-address))
             (vm-test-skip-unless
              (and (equal 0 (vm-epg-test--generate-key
                             (format "VM Author <%s>"
                                     vm-epg-test--author-address)))
                   (equal 0 (vm-epg-test--generate-key
                             (format "VM Recipient <%s>"
                                     vm-epg-test--recipient-address))))
              "gpg could not generate the keys; see its output for why")
             ,@body)
         (call-process gpg nil nil nil "--homedir" home "--quit-agent")
         (ignore-errors
           (call-process "gpgconf" nil nil nil "--homedir" home
                         "--kill" "gpg-agent"))
         (delete-directory home t)))))

(defun vm-epg-test--encryption-subkey-id (home address)
  "The key id gpg names in a session-key packet for ADDRESS's key in HOME.
The packet names the encryption subkey, not the primary key, so a test
comparing the two has to ask for the subkey."
  (with-temp-buffer
    (call-process (vm-epg-test--gpg-program) nil t nil
                  "--homedir" home "--batch" "--with-colons"
                  "--list-keys" address)
    (let (id)
      (dolist (line (split-string (buffer-string) "\n" t) id)
        (let ((fields (split-string line ":")))
          (when (and (equal (nth 0 fields) "sub")
                     (string-match-p "e" (or (nth 11 fields) "")))
            (setq id (nth 4 fields))))))))

(defun vm-epg-test--encrypted-to (home)
  "The key ids the PGP message in the current buffer is encrypted to."
  (goto-char (point-min))
  (re-search-forward "-----BEGIN PGP MESSAGE-----")
  (let ((start (match-beginning 0)))
    (re-search-forward "-----END PGP MESSAGE-----")
    (let ((file (make-temp-file "vm-epg-cipher"))
          (armor (buffer-substring-no-properties start (point)))
          ids)
      (unwind-protect
          (progn
            (with-temp-file file (insert armor))
            (with-temp-buffer
              (call-process (vm-epg-test--gpg-program) nil t nil
                            "--homedir" home "--batch" "--list-packets" file)
              (goto-char (point-min))
              (while (re-search-forward "keyid \\([0-9A-F]+\\)" nil t)
                (push (match-string 1) ids))))
        (delete-file file))
      (nreverse ids))))

(defun vm-epg-test--encrypt-from-author (home)
  "Encrypt a composition from the author to the recipient; answer the key ids."
  (let ((mail-header-separator "--text follows this line--")
        (vm-send-using-mime t))
    (with-temp-buffer
      (mail-mode)
      (insert "From: VM Author <" vm-epg-test--author-address ">\n"
              "To: VM Recipient <" vm-epg-test--recipient-address ">\n"
              "Subject: for the keyring\n"
              mail-header-separator "\n"
              "A body to work on.\n")
      (cl-letf (((symbol-function 'vm-epg-set-signer) #'ignore))
        (vm-epg-encrypt nil))
      (vm-epg-test--encrypted-to home))))

(ert-deftest vm-epg-test-encrypts-to-the-recipients-and-nobody-else ()
  "The author's own key is not added, so a filed copy cannot be read back.
The manual and NEWS say so, and this is what they say it about: `vm-pgg'
added the author's key and vm-epg does not (#782)."
  (vm-epg-test--with-two-keys
    (let ((author (vm-epg-test--encryption-subkey-id
                   home vm-epg-test--author-address))
          (recipient (vm-epg-test--encryption-subkey-id
                      home vm-epg-test--recipient-address)))
      (should author)
      (should recipient)
      (let ((ids (vm-epg-test--encrypt-from-author home)))
        (should (member recipient ids))
        (should-not (member author ids))))))

(ert-deftest vm-epg-test-gpg-conf-encrypt-to-reaches-the-encryption ()
  "`encrypt-to' in gpg.conf adds the author's key, as the manual says it does.
The advice is only worth giving because VM passes gpg no --no-encrypt-to;
were that to change, the manual would be telling the reader to do something
that no longer works (#782)."
  (vm-epg-test--with-two-keys
    (let ((author (vm-epg-test--encryption-subkey-id
                   home vm-epg-test--author-address))
          (recipient (vm-epg-test--encryption-subkey-id
                      home vm-epg-test--recipient-address)))
      (with-temp-file (expand-file-name "gpg.conf" home)
        (insert "encrypt-to " author "\n"))
      (let ((ids (vm-epg-test--encrypt-from-author home)))
        (should (member recipient ids))
        (should (member author ids))))))

(provide 'vm-epg-test)

;;; vm-epg-test.el ends here