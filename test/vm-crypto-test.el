;;; vm-crypto-test.el --- Tests for vm-crypto.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The digest primitives VM authenticates with.  `vm-hmac-md5' is what the
;; IMAP CRAM-MD5 exchange sends, so a mistake in it is not visible in the
;; answer, only in the server rejecting a password that is correct -- which is
;; how emacs-vm/vm#772 stayed unnoticed.  The vectors below are RFC 2202's,
;; and they are the whole point of this file: an HMAC that agrees with them
;; agrees with every server.

;;; Code:

(require 'vm-test-init)
(require 'vm-crypto)

(defun vm-crypto-test--octets (&rest bytes)
  "A unibyte string of BYTES.
`make-string' with a byte over 127 gives a multibyte string, whose characters
are not the octets HMAC is defined over, so the vectors have to be built this
way or they test the wrong thing."
  (apply #'unibyte-string bytes))

(defun vm-crypto-test--repeat (n byte)
  "A unibyte string of BYTE repeated N times."
  (apply #'unibyte-string (make-list n byte)))

;;; RFC 2202, the HMAC-MD5 test vectors

(ert-deftest vm-crypto-test-rfc2202-case-1 ()
  "Key of 16 0x0b octets."
  (should (equal (vm-hmac-md5 (vm-crypto-test--repeat 16 #x0b) "Hi There")
                 "9294727a3638bb1c13f48ef8158bfc9d")))

(ert-deftest vm-crypto-test-rfc2202-case-2 ()
  "An ASCII key and data, which is the ordinary case."
  (should (equal (vm-hmac-md5 "Jefe" "what do ya want for nothing?")
                 "750c783e6ab0b503eaa86e310a5db738")))

(ert-deftest vm-crypto-test-rfc2202-case-3 ()
  "Key and data both entirely above 127, where characters and octets differ."
  (should (equal (vm-hmac-md5 (vm-crypto-test--repeat 16 #xaa)
                              (vm-crypto-test--repeat 50 #xdd))
                 "56be34521d144c88dbb8c733f0e8b3f6")))

(ert-deftest vm-crypto-test-rfc2202-case-4 ()
  "A 25-octet key, shorter than the block and not a repeated byte."
  (should (equal (vm-hmac-md5 (apply #'unibyte-string (number-sequence 1 25))
                              (vm-crypto-test--repeat 50 #xcd))
                 "697eaf0aca3a3aea3a75164746ffaa79")))

(ert-deftest vm-crypto-test-rfc2202-case-5 ()
  "Key of 16 0x0c octets."
  (should (equal (vm-hmac-md5 (vm-crypto-test--repeat 16 #x0c)
                              "Test With Truncation")
                 "56461ef2342edc00f9bab995690efd4c")))

(ert-deftest vm-crypto-test-rfc2202-case-6 ()
  "REGRESSION: a key longer than the block size is hashed first.
Issue #772.  Without that the padded key stayed longer than the pads and
`vm-xor-string' signalled \"strings not of equal length\", so a password over
64 characters raised an internal error instead of logging in."
  (should (equal (vm-hmac-md5
                  (vm-crypto-test--repeat 80 #xaa)
                  "Test Using Larger Than Block-Size Key - Hash Key First")
                 "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd")))

(ert-deftest vm-crypto-test-rfc2202-case-7 ()
  "An over-long key and more than one block of data."
  (should (equal (vm-hmac-md5
                  (vm-crypto-test--repeat 80 #xaa)
                  (concat "Test Using Larger Than Block-Size Key and Larger"
                          " Than One Block-Size Data"))
                 "6f630fad67cda0ee1fb1f562db3aa53e")))

;;; The password as the reader typed it

(ert-deftest vm-crypto-test-an-accented-password-is-hashed-as-octets ()
  "REGRESSION: a password with a character above 127 gives the right digest.
Issue #772.  The CRAM-MD5 exchange XORed character codes rather than octets
and padded to 64 characters rather than 64 octets, so the digest was over the
wrong values and the server rejected a password that was correct -- reported
to the reader as \"IMAP password for %s incorrect\".  The expected value is
what `openssl dgst -md5 -mac HMAC' gives for the UTF-8 octets of the same
password."
  (should (equal (vm-hmac-md5 "caf\N{U+00E9}" "<test@example.com>")
                 "290c25a63eb39989bd2e2713fbe6838d")))

(ert-deftest vm-crypto-test-a-password-encoded-either-way-agrees ()
  "A multibyte password and its own UTF-8 octets hash the same.
That is the property the fix rests on: whichever of the two reaches
`vm-hmac-md5', the server sees one answer."
  (let ((typed "caf\N{U+00E9} \N{U+00FC}ber"))
    (should (equal (vm-hmac-md5 typed "challenge")
                   (vm-hmac-md5 (encode-coding-string typed 'utf-8)
                                "challenge")))))

(ert-deftest vm-crypto-test-a-long-password-does-not-signal ()
  "A password of any length answers a digest rather than raising.
65 characters is the first that used to signal, the block size being 64."
  (dolist (n '(63 64 65 200))
    (should (equal (length (vm-hmac-md5 (make-string n ?a) "challenge")) 32))))

;;; The pieces it is built from

(ert-deftest vm-crypto-test-md5-string ()
  "`vm-md5-string' is MD5 in hex, and encodes a multibyte string to UTF-8.
The UTF-8 part is why APOP, which uses this alone, is unaffected by #772."
  (should (equal (vm-md5-string "abc") "900150983cd24fb0d6963f7d28e17f72"))
  (should (equal (vm-md5-string "") "d41d8cd98f00b204e9800998ecf8427e"))
  (should (equal (vm-md5-string "caf\N{U+00E9}")
                 (vm-md5-string (encode-coding-string "caf\N{U+00E9}" 'utf-8)))))

(ert-deftest vm-crypto-test-md5-raw-string-is-sixteen-octets ()
  "`vm-md5-raw-string' answers the digest as bytes, not hex.
It is the inner digest of the HMAC, so it has to be the raw 16."
  (let ((raw (vm-md5-raw-string "abc")))
    (should (equal (length raw) 16))
    (should-not (multibyte-string-p raw))
    ;; the same bytes the hex says
    (should (equal (mapconcat (lambda (b) (format "%02x" b)) raw "")
                   "900150983cd24fb0d6963f7d28e17f72"))))

(ert-deftest vm-crypto-test-xor-string ()
  "`vm-xor-string' XORs octet by octet, and refuses unequal lengths."
  (should (equal (vm-xor-string (unibyte-string 0 255 15)
                                (unibyte-string 255 255 240))
                 (unibyte-string 255 0 255)))
  (should (equal (vm-xor-string "abc" "abc") (unibyte-string 0 0 0)))
  (should-error (vm-xor-string "ab" "abc") :type 'error))

(ert-deftest vm-crypto-test-string-as-octets ()
  "A unibyte string is its own octets; a multibyte one becomes its UTF-8."
  (let ((bytes (unibyte-string 200 201)))
    (should (eq (vm-string-as-octets bytes) bytes)))
  (should (equal (vm-string-as-octets "caf\N{U+00E9}")
                 (encode-coding-string "caf\N{U+00E9}" 'utf-8)))
  (should-not (multibyte-string-p (vm-string-as-octets "caf\N{U+00E9}"))))

(provide 'vm-crypto-test)

;;; vm-crypto-test.el ends here
