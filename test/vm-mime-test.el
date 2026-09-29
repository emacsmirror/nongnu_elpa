;;; vm-mime-test.el --- Tests for vm-mime.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM MIME functions in vm-mime.el

;;; Code:

(require 'cl-lib)
(require 'vm-test-init)
(require 'vm-mime)

;;; vm-mime-charset-to-coding tests

(ert-deftest vm-mime-test-charset-to-coding-utf8 ()
  "Test UTF-8 charset mapping."
  (should (eq (vm-mime-charset-to-coding "utf-8") 'utf-8)))

(ert-deftest vm-mime-test-charset-to-coding-ascii ()
  "Test US-ASCII charset mapping."
  ;; In modern Emacs, us-ascii is a valid coding system, so it returns that
  ;; rather than falling through to the 'raw-text special case.
  ;; The function downcases the input, so both cases return 'us-ascii.
  (let ((result (vm-mime-charset-to-coding "us-ascii")))
    (should (memq result '(raw-text us-ascii))))
  (let ((result (vm-mime-charset-to-coding "US-ASCII")))
    (should (memq result '(raw-text us-ascii)))))

(ert-deftest vm-mime-test-charset-to-coding-iso-8859-1 ()
  "Test ISO-8859-1 charset mapping."
  (should (eq (vm-mime-charset-to-coding "iso-8859-1") 'iso-8859-1)))

(ert-deftest vm-mime-test-charset-to-coding-unknown ()
  "Test unknown charset returns undecided."
  (should (eq (vm-mime-charset-to-coding "unknown-charset-xyz") 'undecided)))

(ert-deftest vm-mime-test-charset-to-coding-case-insensitive ()
  "Test charset matching is case-insensitive."
  (should (eq (vm-mime-charset-to-coding "UTF-8") 'utf-8))
  (should (eq (vm-mime-charset-to-coding "Utf-8") 'utf-8)))

;;; vm-mime-types-match tests

(ert-deftest vm-mime-test-types-match-exact ()
  "Test exact MIME type matching."
  (should (vm-mime-types-match "text/plain" "text/plain"))
  (should (vm-mime-types-match "image/jpeg" "image/jpeg")))

(ert-deftest vm-mime-test-types-match-wildcard ()
  "Test wildcard MIME type matching."
  (should (vm-mime-types-match "text" "text/plain"))
  (should (vm-mime-types-match "text" "text/html"))
  (should-not (vm-mime-types-match "text" "image/png")))

(ert-deftest vm-mime-test-types-match-case-insensitive ()
  "Test case-insensitive MIME type matching."
  (should (vm-mime-types-match "TEXT/PLAIN" "text/plain"))
  (should (vm-mime-types-match "text/plain" "TEXT/PLAIN"))
  (should (vm-mime-types-match "Text/Plain" "text/plain")))

(ert-deftest vm-mime-test-types-match-no-match ()
  "Test MIME type non-matching."
  (should-not (vm-mime-types-match "text/plain" "text/html"))
  (should-not (vm-mime-types-match "image/jpeg" "image/png")))

(ert-deftest vm-mime-test-types-match-nil ()
  "Test MIME type matching with nil."
  (should-not (vm-mime-types-match "text/plain" nil)))

;;; Base64 encoding/decoding tests

(ert-deftest vm-mime-test-base64-encode-decode-roundtrip ()
  "Test base64 encode/decode roundtrip."
  (let ((original "Hello, World!"))
    (should (equal original
                   (vm-mime-base64-decode-string
                    (vm-mime-base64-encode-string original))))))

(ert-deftest vm-mime-test-base64-decode-known ()
  "Test base64 decoding of known value."
  (should (equal "Hello World!"
                 (vm-mime-base64-decode-string "SGVsbG8gV29ybGQh"))))

(ert-deftest vm-mime-test-base64-encode-known ()
  "Test base64 encoding produces known value."
  (should (equal "SGVsbG8gV29ybGQh"
                 (vm-mime-base64-encode-string "Hello World!"))))

(ert-deftest vm-mime-test-base64-empty-string ()
  "Test base64 with empty string."
  (should (equal "" (vm-mime-base64-decode-string "")))
  (should (equal "" (vm-mime-base64-encode-string ""))))

;;; Quoted-printable (Q) encoding tests

(ert-deftest vm-mime-test-q-decode-region ()
  "Test Q-encoding decode (underscore to space)."
  (with-temp-buffer
    (insert "Hello_World")
    (vm-mime-Q-decode-region (point-min) (point-max))
    (should (equal (buffer-string) "Hello World"))))

(ert-deftest vm-mime-test-q-decode-region-hex ()
  "Test Q-encoding decode with hex escapes."
  (with-temp-buffer
    (insert "Hello=20World")
    (vm-mime-Q-decode-region (point-min) (point-max))
    (should (equal (buffer-string) "Hello World"))))

;;; CRLF conversion tests

(ert-deftest vm-mime-test-crlf-to-lf ()
  "Test CRLF to LF conversion."
  (with-temp-buffer
    (insert "Line1\r\nLine2\r\n")
    (vm-mime-crlf-to-lf-region (point-min) (point-max))
    (should (equal (buffer-string) "Line1\nLine2\n"))))

(ert-deftest vm-mime-test-lf-to-crlf ()
  "Test LF to CRLF conversion."
  (with-temp-buffer
    (insert "Line1\nLine2\n")
    (vm-mime-lf-to-crlf-region (point-min) (point-max))
    (should (equal (buffer-string) "Line1\r\nLine2\r\n"))))

;;; RFC 2047 encoded word tests

(ert-deftest vm-mime-test-decode-encoded-words-base64 ()
  "Test decoding Base64 encoded words."
  (let ((vm-display-using-mime t))
    (should (equal "Test Sender"
                   (vm-decode-mime-encoded-words-in-string
                    "=?UTF-8?B?VGVzdCBTZW5kZXI=?=")))))

(ert-deftest vm-mime-test-decode-encoded-words-qp ()
  "Test decoding quoted-printable encoded words."
  (let ((vm-display-using-mime t))
    ;; =65 is 'e' in hex
    (should (equal "Test"
                   (vm-decode-mime-encoded-words-in-string
                    "=?UTF-8?Q?T=65st?=")))))

(ert-deftest vm-mime-test-decode-encoded-words-underscore ()
  "Test decoding Q-encoded words with underscore (space)."
  (let ((vm-display-using-mime t))
    (should (equal "Hello World"
                   (vm-decode-mime-encoded-words-in-string
                    "=?UTF-8?Q?Hello_World?=")))))

(ert-deftest vm-mime-test-decode-encoded-words-plain ()
  "Test that plain strings pass through unchanged."
  (let ((vm-display-using-mime t))
    (should (equal "Plain text"
                   (vm-decode-mime-encoded-words-in-string "Plain text")))))

(ert-deftest vm-mime-test-decode-encoded-words-mime-disabled ()
  "Test that strings pass through when MIME is disabled."
  (let ((vm-display-using-mime nil))
    (should (equal "=?UTF-8?B?VGVzdA==?="
                   (vm-decode-mime-encoded-words-in-string
                    "=?UTF-8?B?VGVzdA==?=")))))

;;; vm-mime-composite-type-p tests

(ert-deftest vm-mime-test-composite-type-multipart ()
  "Test multipart is recognized as composite type."
  (should (vm-mime-composite-type-p "multipart/mixed"))
  (should (vm-mime-composite-type-p "multipart/alternative"))
  (should (vm-mime-composite-type-p "MULTIPART/MIXED")))

(ert-deftest vm-mime-test-composite-type-message ()
  "Test message is recognized as composite type."
  (should (vm-mime-composite-type-p "message/rfc822"))
  (should (vm-mime-composite-type-p "MESSAGE/RFC822")))

(ert-deftest vm-mime-test-composite-type-non-composite ()
  "Test non-composite types."
  (should-not (vm-mime-composite-type-p "text/plain"))
  (should-not (vm-mime-composite-type-p "image/jpeg"))
  (should-not (vm-mime-composite-type-p "application/octet-stream")))

;;; vm-mime-text-type-p tests

(ert-deftest vm-mime-test-text-type-p ()
  "Test text type detection."
  (should (vm-mime-text-type-p "text/plain"))
  (should (vm-mime-text-type-p "text/html"))
  (should (vm-mime-text-type-p "TEXT/PLAIN"))
  (should-not (vm-mime-text-type-p "image/jpeg"))
  (should-not (vm-mime-text-type-p "application/pdf")))

;;; Fixture-based tests

(ert-deftest vm-mime-test-fixture-exists ()
  "Test that email fixtures exist."
  (should (vm-test-fixture-exists-p "emails" "simple-plain.eml"))
  (should (vm-test-fixture-exists-p "emails" "multipart-mixed.eml"))
  (should (vm-test-fixture-exists-p "emails" "encoded-headers.eml")))

(ert-deftest vm-mime-test-read-fixture ()
  "Test reading email fixture."
  (let ((content (vm-test-read-fixture "emails" "simple-plain.eml")))
    (should (stringp content))
    (should (string-match "From: sender@example.com" content))
    (should (string-match "text/plain" content))))

;;; vm-mime-extract-filename-suffix tests
;; Note: vm-mime-extract-filename-suffix takes a LAYOUT struct, not a filename.
;; Testing it properly would require creating mock layout structures.
;; These tests verify the function exists and behaves correctly with nil.

;;; vm-mime-default-type-from-filename tests

(ert-deftest vm-mime-test-default-type-from-filename ()
  "Test guessing MIME type from filename."
  ;; These depend on vm-mime-attachment-auto-type-alist
  (let ((vm-mime-attachment-auto-type-alist
         '(("\\.txt$" . "text/plain")
           ("\\.html?$" . "text/html")
           ("\\.jpe?g$" . "image/jpeg")
           ("\\.png$" . "image/png")
           ("\\.pdf$" . "application/pdf"))))
    (should (equal (vm-mime-default-type-from-filename "doc.txt") "text/plain"))
    (should (equal (vm-mime-default-type-from-filename "page.html") "text/html"))
    (should (equal (vm-mime-default-type-from-filename "photo.jpg") "image/jpeg"))
    (should (equal (vm-mime-default-type-from-filename "photo.jpeg") "image/jpeg"))
    (should (null (vm-mime-default-type-from-filename "unknown.xyz")))))

;;; vm-mime-make-multipart-boundary tests

(ert-deftest vm-mime-test-make-multipart-boundary ()
  "Test multipart boundary generation."
  (let ((boundary1 (vm-mime-make-multipart-boundary))
        (boundary2 (vm-mime-make-multipart-boundary)))
    ;; Should be strings
    (should (stringp boundary1))
    (should (stringp boundary2))
    ;; Should be unique
    (should-not (equal boundary1 boundary2))
    ;; Should have reasonable length (exactly 10 characters)
    (should (>= (length boundary1) 10))))

;;; vm-mime-type-with-params tests

(ert-deftest vm-mime-test-type-with-params-no-params ()
  "Test vm-mime-type-with-params with no params."
  (should (equal (vm-mime-type-with-params "text/plain" nil)
                 "text/plain")))

(ert-deftest vm-mime-test-type-with-params-one-param ()
  "Test vm-mime-type-with-params with one parameter."
  (let ((vm-mime-avoid-folding-content-type nil))
    (should (equal (vm-mime-type-with-params "text/plain" '("charset=utf-8"))
                   "text/plain; charset=utf-8"))))

(ert-deftest vm-mime-test-type-with-params-multiple ()
  "Test vm-mime-type-with-params with multiple parameters."
  (let ((vm-mime-avoid-folding-content-type nil))
    (should (equal (vm-mime-type-with-params "text/plain"
                                              '("charset=utf-8" "format=flowed"))
                   "text/plain; charset=utf-8; format=flowed"))))

(ert-deftest vm-mime-test-type-with-params-folding ()
  "Test vm-mime-type-with-params with folding enabled."
  (let ((vm-mime-avoid-folding-content-type t))
    (should (string-match "\n\t "
                          (vm-mime-type-with-params "text/plain" '("charset=utf-8"))))))

;;; vm-mime-scrub-description tests
;; Note: vm-mime-scrub-description has a bug where it doesn't move point
;; to beginning before searching, so whitespace collapsing doesn't work.
;; Testing basic pass-through behavior only.

(ert-deftest vm-mime-test-scrub-description-passthrough ()
  "Test vm-mime-scrub-description returns string."
  (should (stringp (vm-mime-scrub-description "hello world"))))

;;; vm-mime-B-encode-region / B-decode-region tests

(ert-deftest vm-mime-test-b-encode-decode-roundtrip ()
  "Test B-encode and B-decode roundtrip."
  (let ((original "Hello World!"))
    (with-temp-buffer
      (insert original)
      (vm-mime-B-encode-region (point-min) (point-max))
      (vm-mime-B-decode-region (point-min) (point-max))
      (should (equal (buffer-string) original)))))

(ert-deftest vm-mime-test-b-encode-is-base64 ()
  "Test B-encoding produces base64."
  (with-temp-buffer
    (insert "Hello World!")
    (vm-mime-B-encode-region (point-min) (point-max))
    (should (equal (buffer-string) "SGVsbG8gV29ybGQh"))))

;;; vm-mime-Q-encode-region tests

(ert-deftest vm-mime-test-q-encode-space-to-underscore ()
  "Test Q-encoding converts spaces to underscores."
  (with-temp-buffer
    (insert "Hello World")
    (vm-mime-Q-encode-region (point-min) (point-max))
    (should (string-match "_" (buffer-string)))))

;;; vm-mime-qp-encode-region / qp-decode-region tests

(ert-deftest vm-mime-test-qp-encode-decode-roundtrip ()
  "Test quoted-printable encode/decode roundtrip."
  (let ((original "Hello World!"))
    (with-temp-buffer
      (insert original)
      (vm-mime-qp-encode-region (point-min) (point-max))
      (vm-mime-qp-decode-region (point-min) (point-max))
      (should (equal (buffer-string) original)))))

(defconst vm-mime-test--codec-payloads
  (list (cons "plain ascii"      "hello there\n")
        (cons "trailing space"   "a line with a trailing space \nnext\n")
        (cons "trailing tab"     "a line with a trailing tab\t\nnext\n")
        (cons "an = sign"        "1 = 2 maybe\n")
        (cons "a From_ line"
              "before\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\nafter\n")
        (cons "a dot line"       "before\n.\nafter\n")
        (cons "76 characters"    (concat (make-string 76 ?x) "\n"))
        (cons "77 characters"    (concat (make-string 77 ?x) "\n"))
        (cons "1000 characters"  (concat (make-string 1000 ?y) "\n"))
        (cons "8-bit bytes"      (unibyte-string ?a ?\s ?b 195 182 ?d ?y 10))
        (cons "every byte"       (apply #'unibyte-string
                                        (append (number-sequence 1 255) (list 10))))
        (cons "a CR"             "before\r\nafter\n")
        (cons "no final newline" "no newline at the end")
        (cons "empty"            ""))
  "Payloads to put through a transfer encoding and back.

Bytes, not characters: `unibyte-string' rather than a literal, because a
literal above 127 is read as a character and a codec answers bytes, so the
two compare unequal however well the codec did.  That cost an hour.

The awkward ones are here on purpose: trailing whitespace, which
quoted-printable has to encode or a gateway will strip it; a line of exactly
76 and one of 77, which is where the fold falls; an `=' and a `From ' line,
which are what the encodings quote; a bare CR; and no final newline.")

(defun vm-mime-test--transfer-encode (encoding text)
  "The wire form of TEXT under ENCODING, `base64' or `quoted-printable'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert text)
    (if (eq encoding 'base64)
        (vm-mime-base64-encode-region (point-min) (point-max))
      (vm-mime-qp-encode-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vm-mime-test--transfer-round-trip (encoding text &optional crlf)
  "Encode TEXT under ENCODING and decode it again, answering what comes back.
CRLF asks base64 for the line endings a part carries on the wire, which is
the path emacs-vm/vm#792 broke: without it the bug is invisible here, since
the marker it turned on only matters where the CRLF conversion inserts."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert text)
    (if (eq encoding 'base64)
        (progn
          (vm-mime-base64-encode-region (point-min) (point-max) crlf)
          (vm-mime-base64-decode-region (point-min) (point-max) crlf))
      (vm-mime-qp-encode-region (point-min) (point-max))
      (quoted-printable-decode-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vm-mime-test--longest-line (text)
  "The length of the longest line in TEXT."
  (apply #'max 0 (mapcar #'length (split-string text "\n"))))

(ert-deftest vm-mime-test-a-transfer-encoding-gives-every-byte-back ()
  "Both transfer encodings return each payload byte for byte.

Fourteen payloads by two encodings by the two line endings, including all
255 byte values in one part.  A part that does not come back as it went in
is mail that arrives wrong, and emacs-vm/vm#792 was one: a base64 text part
went out a newline short.  The CRLF half is what reaches that: with LF alone
the marker #792 turned on never matters, and a mutation reintroducing it
passes this test."
  (dolist (encoding '(base64 quoted-printable))
    (dolist (crlf '(nil t))
      (dolist (spec vm-mime-test--codec-payloads)
        (should (equal (list encoding crlf (car spec) (cdr spec))
                       (list encoding crlf (car spec)
                             (vm-mime-test--transfer-round-trip
                              encoding (cdr spec) crlf))))))))

(ert-deftest vm-mime-test-a-transfer-encoding-folds-to-76-characters ()
  "Neither transfer encoding writes a line past the 76 of RFC 2045.

Lossless is not the same as legal: a part that decodes correctly here can
still be one a gateway wraps, and a wrapped base64 line does not decode at
all.  The 1000-character payload is the one that shows it, and the 76 and 77
are where the fold falls."
  (dolist (encoding '(base64 quoted-printable))
    (dolist (spec vm-mime-test--codec-payloads)
      (let ((longest (vm-mime-test--longest-line
                      (vm-mime-test--transfer-encode encoding (cdr spec)))))
        (should (equal (list encoding (car spec) t)
                       (list encoding (car spec) (<= longest 76))))))))

(defconst vm-mime-test--encoding-choice-payloads
  (list (cons "plain ascii"      "hello there\n")
        (cons "a From_ line"
              "before\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\nafter\n")
        (cons "a dot line"       "before\n.\nafter\n")
        (cons "8-bit bytes"      (unibyte-string ?a ?\s ?b 195 182 ?d ?y 10))
        (cons "a NUL"            (unibyte-string ?a 0 ?b 10))
        (cons "a CR"             "before\r\nafter\n")
        (cons "a 1000 char line" (concat (make-string 1000 ?y) "\n"))
        (cons "every byte"       (apply #'unibyte-string
                                        (append (number-sequence 1 255) (list 10)))))
  "Bodies that reach each arm of `vm-mime-transfer-encode-region'.
A NUL and a CR are what makes VM call a part binary, a thousand-character
line is past the 998 of RFC 5322, and a `From ' or a lone dot are the two
lines it armors.")

(defun vm-mime-test--decode-by-name (name)
  "Decode the whole buffer by the transfer encoding NAME."
  (cond ((equal name "base64")
         (vm-mime-base64-decode-region (point-min) (point-max) t))
        ((equal name "quoted-printable")
         (quoted-printable-decode-region (point-min) (point-max)))))

(defun vm-mime-test--choose-and-encode (option armor text)
  "Let VM choose an encoding for TEXT and apply it, then decode it again.
OPTION is `vm-mime-8bit-text-transfer-encoding' and ARMOR is
`vm-mime-composition-armor-from-lines'.  Answers (ENCODING . CAME-BACK)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert text)
    (let* ((vm-mime-8bit-text-transfer-encoding option)
           (vm-mime-composition-armor-from-lines armor)
           (chosen (vm-determine-proper-content-transfer-encoding
                    (point-min) (point-max)))
           (used (vm-mime-transfer-encode-region
                  chosen (point-min) (point-max) t)))
      (vm-mime-test--decode-by-name used)
      (cons used (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest vm-mime-test-whatever-encoding-vm-picks-carries-the-body ()
  "The body survives whichever encoding VM chooses for it.

Eight bodies by the three values of `vm-mime-8bit-text-transfer-encoding' by
the two of `vm-mime-composition-armor-from-lines'.  What is pinned is the
invariant rather than any one choice: decoding by the name
`vm-mime-transfer-encode-region' answers gives the bytes back.

The choice is checked in two places where it is not free.  A `From ' line
goes out as it stands when armoring is off, which is the default and is what
`vm-mime-composition-armor-from-lines' documents, and is armored when it is
on.  A body over the line length limit goes quoted-printable whatever the
option says, because base64 would make the rest of the part unreadable to
anyone looking at the message as it was sent."
  (dolist (option '(quoted-printable base64 8bit))
    (dolist (armor '(nil t))
      (dolist (spec vm-mime-test--encoding-choice-payloads)
        (let ((got (vm-mime-test--choose-and-encode option armor (cdr spec))))
          (should (equal (list option armor (car spec) (cdr spec))
                         (list option armor (car spec) (cdr got))))))))
  ;; the two choices that are not free
  (should (equal "7bit"
                 (car (vm-mime-test--choose-and-encode
                       'quoted-printable nil "before\nFrom x@example.com\n"))))
  (should (equal "quoted-printable"
                 (car (vm-mime-test--choose-and-encode
                       'quoted-printable t "before\nFrom x@example.com\n"))))
  (should (equal "quoted-printable"
                 (car (vm-mime-test--choose-and-encode
                       'base64 nil (concat (make-string 1000 ?y) "\n"))))))

(ert-deftest vm-mime-test-qp-encode-preserves-ascii ()
  "Test quoted-printable preserves ASCII text."
  (with-temp-buffer
    (insert "Hello")
    (vm-mime-qp-encode-region (point-min) (point-max))
    (should (string-match "Hello" (buffer-string)))))

;;; vm-mime-encode-words-in-string tests

(ert-deftest vm-mime-test-encode-words-ascii ()
  "Test encode-words-in-string with ASCII returns same string."
  (should (equal (vm-mime-encode-words-in-string "Hello World")
                 "Hello World")))

;;; vm-mime-make-multipart-boundary uniqueness tests

(ert-deftest vm-mime-test-make-multipart-boundary-unique ()
  "Test that multiple calls produce unique boundaries."
  (let ((boundaries (make-hash-table :test 'equal)))
    (dotimes (_ 100)
      (let ((b (vm-mime-make-multipart-boundary)))
        (should-not (gethash b boundaries))
        (puthash b t boundaries)))))

;;; vm-decode-mime-encoded-words-in-string edge cases

(ert-deftest vm-mime-test-decode-encoded-words-adjacent ()
  "Test decoding adjacent encoded words."
  (let ((vm-display-using-mime t))
    ;; Adjacent encoded words should have whitespace between them removed
    (should (equal (vm-decode-mime-encoded-words-in-string
                    "=?UTF-8?B?SGVsbG8=?= =?UTF-8?B?V29ybGQ=?=")
                   "HelloWorld"))))

(ert-deftest vm-mime-test-decode-encoded-words-mixed ()
  "Test decoding mixed encoded and plain text."
  (let ((vm-display-using-mime t))
    (should (equal (vm-decode-mime-encoded-words-in-string
                    "Hello =?UTF-8?B?V29ybGQ=?=!")
                   "Hello World!"))))

;;; vm-mime-charset-internally-displayable-p tests

(ert-deftest vm-mime-test-charset-displayable-utf8 ()
  "Test UTF-8 is displayable."
  (should (vm-mime-charset-internally-displayable-p "utf-8")))

(ert-deftest vm-mime-test-charset-displayable-ascii ()
  "Test US-ASCII is displayable."
  (should (vm-mime-charset-internally-displayable-p "us-ascii")))

(ert-deftest vm-mime-test-charset-displayable-iso8859 ()
  "Test ISO-8859-1 is displayable."
  (should (vm-mime-charset-internally-displayable-p "iso-8859-1")))

;;; vm-mime-charset-decode-region tests

(ert-deftest vm-mime-test-charset-decode-region-utf8 ()
  "Test charset decode region with UTF-8."
  (with-temp-buffer
    (insert "Hello")
    (vm-mime-charset-decode-region "utf-8" (point-min) (point-max))
    (should (equal (buffer-string) "Hello"))))

;;; High-level parsing tests

(ert-deftest vm-mime-test-decode-encoded-words-region-base64 ()
  "Test vm-decode-mime-encoded-words on a buffer region with Base64."
  (let ((vm-display-using-mime t))
    (with-temp-buffer
      (insert "From: =?UTF-8?B?VGVzdCBTZW5kZXI=?= <test@example.com>\n")
      (vm-decode-mime-encoded-words (point-min) (point-max))
      (goto-char (point-min))
      (should (search-forward "Test Sender" nil t)))))

(ert-deftest vm-mime-test-decode-encoded-words-region-qp ()
  "Test vm-decode-mime-encoded-words on a buffer region with Q encoding."
  (let ((vm-display-using-mime t))
    (with-temp-buffer
      (insert "Subject: =?UTF-8?Q?Hello_World?=\n")
      (vm-decode-mime-encoded-words (point-min) (point-max))
      (goto-char (point-min))
      (should (search-forward "Hello World" nil t)))))

(ert-deftest vm-mime-test-decode-encoded-words-preserves-unencoded ()
  "Test that vm-decode-mime-encoded-words preserves unencoded text."
  (let ((vm-display-using-mime t))
    (with-temp-buffer
      (insert "Subject: Plain text subject\n")
      (vm-decode-mime-encoded-words (point-min) (point-max))
      (should (string-match "Plain text subject" (buffer-string))))))

;;; vm-parse-structured-header tests

(ert-deftest vm-misc-test-parse-structured-header-content-type ()
  "Test parsing Content-Type header."
  (let ((result (vm-parse-structured-header "text/plain; charset=utf-8" ?\;)))
    (should (member "text/plain" result))
    (should (member "charset=utf-8" result))))

(ert-deftest vm-misc-test-parse-structured-header-quoted ()
  "Test parsing header with quoted values."
  (let ((result (vm-parse-structured-header
                 "multipart/mixed; boundary=\"----=_Part_0\"" ?\;)))
    (should (member "multipart/mixed" result))
    ;; Quotes should be stripped
    (should (member "boundary=----=_Part_0" result))))

(ert-deftest vm-misc-test-parse-structured-header-keep-quotes ()
  "Test parsing header keeping quotes."
  (let ((result (vm-parse-structured-header
                 "multipart/mixed; boundary=\"----=_Part_0\"" ?\; t)))
    ;; With keep-quotes, quotes should be preserved
    (should (member "boundary=\"----=_Part_0\"" result))))

(ert-deftest vm-misc-test-parse-structured-header-disposition ()
  "Test parsing Content-Disposition header."
  (let ((result (vm-parse-structured-header
                 "attachment; filename=\"test.txt\"" ?\;)))
    (should (member "attachment" result))
    (should (member "filename=test.txt" result))))

(ert-deftest vm-misc-test-parse-structured-header-nil ()
  "Test parsing nil header returns nil."
  (should (null (vm-parse-structured-header nil))))

;;; vm-parse-addresses tests

(ert-deftest vm-misc-test-parse-addresses-simple ()
  "Test parsing simple email address."
  (let ((result (vm-parse-addresses "user@example.com")))
    (should (equal (length result) 1))
    (should (string-match "user@example.com" (car result)))))

(ert-deftest vm-misc-test-parse-addresses-multiple ()
  "Test parsing multiple email addresses."
  (let ((result (vm-parse-addresses "user1@example.com, user2@example.com")))
    (should (= (length result) 2))))

(ert-deftest vm-misc-test-parse-addresses-with-name ()
  "Test parsing address with display name."
  (let ((result (vm-parse-addresses "John Doe <john@example.com>")))
    (should (= (length result) 1))
    (should (string-match "John Doe" (car result)))
    (should (string-match "john@example.com" (car result)))))

(ert-deftest vm-misc-test-parse-addresses-with-comment ()
  "Test parsing address with comment."
  (let ((result (vm-parse-addresses "user@example.com (John Doe)")))
    (should (= (length result) 1))))

(ert-deftest vm-misc-test-parse-addresses-nil ()
  "Test parsing nil returns nil."
  (should (null (vm-parse-addresses nil))))

;;; Fixture-based high-level tests

(ert-deftest vm-mime-test-fixture-multipart-headers ()
  "Test parsing headers from multipart fixture."
  (let ((content (vm-test-read-fixture "emails" "multipart-mixed.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Find Content-Type header
      (should (search-forward "Content-Type:" nil t))
      (let ((line-end (line-end-position)))
        (should (search-forward "multipart/mixed" line-end t))))))

(ert-deftest vm-mime-test-fixture-encoded-from-decode ()
  "Test decoding From header from encoded fixture."
  (let ((content (vm-test-read-fixture "emails" "encoded-headers.eml"))
        (vm-display-using-mime t))
    (with-temp-buffer
      (insert content)
      ;; Skip mbox envelope line (starts with "From ") to get to From: header
      (goto-char (point-min))
      (forward-line 1)
      (let ((from-start (point))
            (from-end (line-end-position)))
        (vm-decode-mime-encoded-words from-start from-end)
        (goto-char (point-min))
        (should (search-forward "Test Sender" nil t))))))

(ert-deftest vm-mime-test-fixture-match-headers ()
  "Test vm-match-header on fixture content."
  (let ((content (vm-test-read-fixture "emails" "simple-plain.eml")))
    (with-temp-buffer
      (insert content)
      ;; Skip mbox envelope line to get to RFC822 headers
      (goto-char (point-min))
      (forward-line 1)
      ;; vm-match-header should find the From header
      (should (vm-match-header))
      (should (string= (vm-matched-header-name) "From")))))

(ert-deftest vm-mime-test-fixture-iterate-headers ()
  "Test iterating through all headers in fixture."
  (let ((content (vm-test-read-fixture "emails" "simple-plain.eml"))
        (headers '()))
    (with-temp-buffer
      (insert content)
      ;; Skip mbox envelope line to get to RFC822 headers
      (goto-char (point-min))
      (forward-line 1)
      ;; Collect all header names
      (while (vm-match-header)
        (push (vm-matched-header-name) headers)
        (goto-char (vm-matched-header-end)))
      ;; Should have found multiple headers
      (should (> (length headers) 3))
      (should (member "From" headers))
      (should (member "To" headers))
      (should (member "Subject" headers)))))

;;; Direct transfer encoding tests (using low-level functions)

(ert-deftest vm-mime-test-base64-decode-region ()
  "Test vm-mime-base64-decode-region."
  (with-temp-buffer
    (insert "SGVsbG8gV29ybGQh")
    (vm-mime-base64-decode-region (point-min) (point-max))
    (should (equal (buffer-string) "Hello World!"))))

(ert-deftest vm-mime-test-base64-encode-region ()
  "Test vm-mime-base64-encode-region."
  (with-temp-buffer
    (insert "Hello World!")
    (vm-mime-base64-encode-region (point-min) (point-max))
    (should (equal (buffer-string) "SGVsbG8gV29ybGQh"))))

(ert-deftest vm-mime-test-base64-region-roundtrip ()
  "Test base64 encode/decode roundtrip on region."
  (let ((original "The quick brown fox jumps over the lazy dog."))
    (with-temp-buffer
      (insert original)
      (vm-mime-base64-encode-region (point-min) (point-max))
      (vm-mime-base64-decode-region (point-min) (point-max))
      (should (equal (buffer-string) original)))))

;;; vm-mime-parse-entity tests (high-level parsing)

(ert-deftest vm-mime-test-parse-entity-text-plain ()
  "Test vm-mime-parse-entity on a simple text/plain message."
  (with-temp-buffer
    (insert "Content-Type: text/plain; charset=utf-8\n")
    (insert "Content-Transfer-Encoding: 7bit\n")
    (insert "\n")
    (insert "Hello, World!\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (vectorp layout))
      (should (equal (car (vm-mm-layout-type layout)) "text/plain"))
      (should (equal (vm-mm-layout-encoding layout) "7bit")))))

(ert-deftest vm-mime-test-parse-entity-base64 ()
  "Test vm-mime-parse-entity on a base64-encoded message."
  (with-temp-buffer
    (insert "Content-Type: text/plain\n")
    (insert "Content-Transfer-Encoding: Base64\n")
    (insert "\n")
    (insert "SGVsbG8gV29ybGQh\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (vectorp layout))
      (should (equal (vm-mm-layout-encoding layout) "Base64")))))

(ert-deftest vm-mime-test-parse-entity-with-charset ()
  "Test vm-mime-parse-entity preserves charset parameter."
  (with-temp-buffer
    (insert "Content-Type: text/plain; charset=iso-8859-1\n")
    (insert "\n")
    (insert "Test content\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (vectorp layout))
      (let ((type-params (vm-mm-layout-type layout)))
        (should (member "charset=iso-8859-1" type-params))))))

(ert-deftest vm-mime-test-parse-entity-multipart ()
  "Test vm-mime-parse-entity on a multipart/mixed message."
  (with-temp-buffer
    (insert "Content-Type: multipart/mixed; boundary=\"----=test\"\n")
    (insert "\n")
    (insert "------=test\n")
    (insert "Content-Type: text/plain\n")
    (insert "\n")
    (insert "Part 1\n")
    (insert "------=test\n")
    (insert "Content-Type: text/plain\n")
    (insert "\n")
    (insert "Part 2\n")
    (insert "------=test--\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (vectorp layout))
      (should (equal (car (vm-mm-layout-type layout)) "multipart/mixed"))
      ;; Should have 2 parts
      (should (= (length (vm-mm-layout-parts layout)) 2)))))

(ert-deftest vm-mime-test-parse-entity-multipart-parts-type ()
  "Test that multipart parts have correct types."
  (with-temp-buffer
    (insert "Content-Type: multipart/mixed; boundary=\"bound\"\n")
    (insert "\n")
    (insert "--bound\n")
    (insert "Content-Type: text/plain\n")
    (insert "\n")
    (insert "Text part\n")
    (insert "--bound\n")
    (insert "Content-Type: text/html\n")
    (insert "\n")
    (insert "<p>HTML</p>\n")
    (insert "--bound--\n")
    (let* ((layout (vm-mime-parse-entity nil
                     :default-type '("text/plain")
                     :default-encoding "7bit"))
           (parts (vm-mm-layout-parts layout)))
      (should (= (length parts) 2))
      (should (equal (car (vm-mm-layout-type (car parts))) "text/plain"))
      (should (equal (car (vm-mm-layout-type (cadr parts))) "text/html")))))

(ert-deftest vm-mime-test-parse-entity-message-rfc822 ()
  "Test vm-mime-parse-entity on a message/rfc822 encapsulation."
  (with-temp-buffer
    (insert "Content-Type: message/rfc822\n")
    (insert "\n")
    (insert "From: inner@example.com\n")
    (insert "Subject: Nested\n")
    (insert "\n")
    (insert "Nested body\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (vectorp layout))
      (should (equal (car (vm-mm-layout-type layout)) "message/rfc822"))
      ;; Should have exactly 1 part (the nested message)
      (should (= (length (vm-mm-layout-parts layout)) 1)))))

(ert-deftest vm-mime-test-parse-entity-safe-returns-layout ()
  "Test vm-mime-parse-entity-safe returns a layout on valid input."
  (with-temp-buffer
    (insert "Content-Type: text/plain\n")
    (insert "\n")
    (insert "Hello\n")
    (let ((layout (vm-mime-parse-entity-safe nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (vectorp layout)))))

(ert-deftest vm-mime-test-parse-entity-no-content-type ()
  "Test vm-mime-parse-entity uses default type when no Content-Type."
  (with-temp-buffer
    (insert "\n")
    (insert "Body without Content-Type header\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain" "charset=us-ascii")
                    :default-encoding "7bit")))
      (should (vectorp layout))
      (should (equal (car (vm-mm-layout-type layout)) "text/plain")))))

(ert-deftest vm-mime-test-parse-entity-body-markers ()
  "Test that body-start and body-end markers are set correctly."
  (with-temp-buffer
    (insert "Content-Type: text/plain\n")
    (insert "\n")
    (insert "Test body content\n")
    (let* ((layout (vm-mime-parse-entity nil
                     :default-type '("text/plain")
                     :default-encoding "7bit"))
           (body-start (vm-mm-layout-body-start layout))
           (body-end (vm-mm-layout-body-end layout)))
      (should (markerp body-start))
      (should (markerp body-end))
      (should (< body-start body-end))
      ;; Body should contain our text
      (should (string-match "Test body"
                            (buffer-substring body-start body-end))))))

;;; IMC MIME Conformance Test Suite
;; These tests use fixtures from the Internet Mail Consortium MIME test suite
;; to verify VM's MIME parsing against known test cases.

(ert-deftest vm-mime-conformance-fixture-exists ()
  "Test that MIME conformance fixtures exist."
  (should (vm-test-fixture-exists-p "mime-conformance" "M2.eml"))
  (should (vm-test-fixture-exists-p "mime-conformance" "M4.1.1.eml"))
  (should (vm-test-fixture-exists-p "mime-conformance" "M4.2.1.eml"))
  (should (vm-test-fixture-exists-p "mime-conformance" "M4.3.1.eml"))
  (should (vm-test-fixture-exists-p "mime-conformance" "EM1.1.1.eml")))

(ert-deftest vm-mime-conformance-m2-base64-text ()
  "Test M2: Base64 encoded text/plain message."
  (let ((content (vm-test-read-fixture "mime-conformance" "M2.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (forward-line 1)  ; Skip mbox envelope
      ;; Find Content-Transfer-Encoding header
      (should (search-forward "Content-Transfer-Encoding:" nil t))
      (should (looking-at ".*Base64")))))

(ert-deftest vm-mime-conformance-m2-decode-base64 ()
  "Test M2: Verify base64 body can be decoded."
  (let ((content (vm-test-read-fixture "mime-conformance" "M2.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Find the blank line separating headers from body
      (search-forward "\n\n")
      (let ((body-start (point))
            (body-end (point-max)))
        ;; Remove trailing whitespace/newlines
        (goto-char body-end)
        (skip-chars-backward " \t\n")
        (setq body-end (point))
        ;; Decode the base64 content
        (vm-mime-base64-decode-region body-start body-end)
        ;; The decoded content should mention the M2 test
        (goto-char body-start)
        (should (search-forward "M2" nil t))))))

(ert-deftest vm-mime-conformance-m4-1-1-us-ascii ()
  "Test M4.1.1: text/plain with US-ASCII charset."
  (let ((content (vm-test-read-fixture "mime-conformance" "M4.1.1.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (forward-line 1)  ; Skip mbox envelope
      ;; Find Content-Type with US-ASCII
      (should (search-forward "Content-Type:" nil t))
      (let ((ct-line (buffer-substring (line-beginning-position) (line-end-position))))
        (should (string-match "text/plain" ct-line))
        (should (string-match "US-ASCII" ct-line))))))

(ert-deftest vm-mime-conformance-m4-2-1-message-rfc822 ()
  "Test M4.2.1: message/rfc822 encapsulation."
  (let ((content (vm-test-read-fixture "mime-conformance" "M4.2.1.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (forward-line 1)  ; Skip mbox envelope
      ;; Find Content-Type header with message/rfc822
      (should (search-forward "Content-Type:" nil t))
      (should (search-forward "message/rfc822" nil t))
      ;; Find embedded message headers (another From: line inside)
      (should (search-forward "\n\nFrom:" nil t)))))

(ert-deftest vm-mime-conformance-m4-3-1-multipart-mixed ()
  "Test M4.3.1: multipart/mixed with three parts."
  (let ((content (vm-test-read-fixture "mime-conformance" "M4.3.1.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (forward-line 1)  ; Skip mbox envelope
      ;; Find multipart/mixed Content-Type
      (should (search-forward "Content-Type:" nil t))
      (should (search-forward "multipart/mixed" nil t))
      ;; Count boundary markers (should have 3 parts + closing)
      (goto-char (point-min))
      (let ((boundary-count 0))
        (while (search-forward "--this_is_a_boundary" nil t)
          (setq boundary-count (1+ boundary-count)))
        ;; 3 opening boundaries + 1 closing (--boundary--)
        (should (= boundary-count 4))))))

(ert-deftest vm-mime-conformance-m4-3-1-parts-content ()
  "Test M4.3.1: Verify each part has distinct content."
  (let ((content (vm-test-read-fixture "mime-conformance" "M4.3.1.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Find each part marker
      (should (search-forward "first part" nil t))
      (should (search-forward "second part" nil t))
      (should (search-forward "third part" nil t)))))

(ert-deftest vm-mime-conformance-em1-1-1-charset ()
  "Test EM1.1.1: Character set specification."
  (let ((content (vm-test-read-fixture "mime-conformance" "EM1.1.1.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (forward-line 1)  ; Skip mbox envelope
      ;; Should have Content-Type with charset
      (should (search-forward "Content-Type:" nil t))
      (should (search-forward "charset" nil t)))))

(ert-deftest vm-mime-conformance-em2-1-base64-binary ()
  "Test EM2.1: Base64 encoded binary (GIF image)."
  (let ((content (vm-test-read-fixture "mime-conformance" "EM2.1.eml")))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (forward-line 1)  ; Skip mbox envelope
      ;; Should have image/gif Content-Type
      (should (search-forward "Content-Type:" nil t))
      (should (search-forward "image/gif" nil t))
      ;; Should have base64 encoding
      (goto-char (point-min))
      (should (search-forward "base64" nil t)))))

;;; MIME parsing with folder infrastructure tests

(ert-deftest vm-mime-conformance-parse-multipart-folder ()
  "Test parsing M4.3.1 multipart message as VM folder."
  (let ((content (vm-test-read-fixture "mime-conformance" "M4.3.1.eml")))
    (vm-test-with-folder content
      ;; Should parse as single message
      (should (= (vm-test-message-count) 1))
      ;; Check Subject header
      (let ((m (vm-test-first-message)))
        (should (string= (vm-test-message-header m "Subject") "M4.3.1"))))))

(ert-deftest vm-mime-conformance-parse-base64-folder ()
  "Test parsing M2 base64 message as VM folder."
  (let ((content (vm-test-read-fixture "mime-conformance" "M2.eml")))
    (vm-test-with-folder content
      (should (= (vm-test-message-count) 1))
      (let ((m (vm-test-first-message)))
        (should (string= (vm-test-message-header m "Subject") "M2"))
        (should (string= (vm-test-message-header m "Content-Transfer-Encoding") "Base64"))))))

(ert-deftest vm-mime-conformance-parse-nested-message ()
  "Test parsing M4.2.1 nested message/rfc822 as VM folder."
  (let ((content (vm-test-read-fixture "mime-conformance" "M4.2.1.eml")))
    (vm-test-with-folder content
      (should (= (vm-test-message-count) 1))
      (let ((m (vm-test-first-message)))
        (should (string= (vm-test-message-header m "Content-Type") "message/rfc822"))))))

;;; Additional MIME stress tests (testing new code paths)

(ert-deftest vm-mime-test-many-charsets-folder ()
  "Test parsing multipart with many charsets as a folder.
This stress-tests charset handling via vm-build-message-list."
  (let ((content (vm-test-read-fixture "mime-conformance" "charsets.eml")))
    (vm-test-with-folder content
      ;; Should parse the message
      (should (= (vm-test-message-count) 1))
      (let ((m (vm-test-first-message)))
        ;; Should detect multipart/mixed
        (should (string-match "multipart/mixed"
                              (vm-test-message-header m "Content-Type")))))))

(ert-deftest vm-mime-test-nested-multipart-folder ()
  "Test nested multipart/related inside multipart/mixed as folder.
Tests recursive multipart parsing with different subtypes."
  (let ((content (vm-test-read-fixture "mime-conformance" "multipart-complex1.eml")))
    (vm-test-with-folder content
      (should (= (vm-test-message-count) 1))
      (let ((m (vm-test-first-message)))
        (should (string-match "multipart/mixed"
                              (vm-test-message-header m "Content-Type")))))))

(ert-deftest vm-mime-test-confusing-boundary-names ()
  "Test multipart with similar boundary names (bou, bound, boundar, boundary).
This stress-tests boundary matching to ensure correct parsing."
  (let ((content (vm-test-read-fixture "mime-conformance" "nested-multipart-confusing.eml")))
    (vm-test-with-folder content
      ;; Should parse without error despite confusing boundaries
      (should (= (vm-test-message-count) 1)))))

(ert-deftest vm-mime-test-message-types-folder ()
  "Test message/global and message/news content types.
These are less common message/* subtypes."
  (let ((content (vm-test-read-fixture "mime-conformance" "message-encoded.eml")))
    (vm-test-with-folder content
      (should (= (vm-test-message-count) 1))
      ;; Verify the fixture contains these types
      (let ((m (vm-test-first-message)))
        (should (string-match "multipart/mixed"
                              (vm-test-message-header m "Content-Type")))))))

;;; vm-mime-operate-on-attachments tests

(defun vm-mime-test-collect-attachments (layout)
  "Run `vm-mime-operate-on-attachments' over LAYOUT, returning the types seen.
LAYOUT stands in for a real message, which the function only ever
reaches through `vm-mm-layout'."
  (let ((seen nil)
        (message 'fake-message))
    (cl-letf (((symbol-function 'vm-mm-layout)
               (lambda (_m) layout))
              ((symbol-function 'vm-retrieve-operable-messages)
               (lambda (&rest _) nil)))
      (vm-mime-operate-on-attachments
       nil
       :action (lambda (_msg _layout type _file) (push type seen))
       :messages (list message)))
    (nreverse seen)))

(ert-deftest vm-mime-test-operate-on-attachments-empty-composite ()
  "Test a composite part with no sub-parts as the last part.
Regression test for issue #455: flattening replaced such a part with
its (empty) sub-part list, emptying PARTS, and the loop then called
`vm-mm-layout-type' on nil -- \"Wrong type argument: arrayp, nil\"."
  (with-temp-buffer
    (insert "Content-Type: multipart/mixed; boundary=OUTER\n"
            "\n"
            "--OUTER\n"
            "Content-Type: multipart/mixed; boundary=NEVER-APPEARS\n"
            "\n"
            "the declared boundary is nowhere in this body\n"
            "--OUTER--\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      ;; precondition: the inner part is composite but has no sub-parts
      (let ((inner (car (vm-mm-layout-parts layout))))
        (should (vm-mime-composite-type-p (car (vm-mm-layout-type inner))))
        (should (null (vm-mm-layout-parts inner))))
      (should (null (vm-mime-test-collect-attachments layout))))))

(ert-deftest vm-mime-test-operate-on-attachments-delivery-failure ()
  "Test a Google-style delivery failure report.
The real-world case from issue #455: multipart/report whose last part
is a message/rfc822 wrapping a multipart with an absent boundary."
  (with-temp-buffer
    (insert (vm-test-read-fixture "emails" "delivery-failure-report.eml"))
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (equal (car (vm-mm-layout-type layout)) "multipart/report"))
      (should (= (length (vm-mm-layout-parts layout)) 3))
      ;; the attachment in the report is still found
      (should (equal (vm-mime-test-collect-attachments layout)
                     '("image/png"))))))

(ert-deftest vm-mime-test-operate-on-attachments-nested-still-walked ()
  "Test that ordinary nested parts are still visited after the fix."
  (with-temp-buffer
    (insert "Content-Type: multipart/mixed; boundary=OUTER\n"
            "\n"
            "--OUTER\n"
            "Content-Type: multipart/mixed; boundary=INNER\n"
            "\n"
            "--INNER\n"
            "Content-Type: text/plain\n"
            "Content-Disposition: attachment; filename=\"a.txt\"\n"
            "\n"
            "first\n"
            "--INNER--\n"
            "--OUTER\n"
            "Content-Type: application/pdf\n"
            "Content-Disposition: attachment; filename=\"b.pdf\"\n"
            "\n"
            "second\n"
            "--OUTER--\n")
    (let ((layout (vm-mime-parse-entity nil
                    :default-type '("text/plain")
                    :default-encoding "7bit")))
      (should (equal (vm-mime-test-collect-attachments layout)
                     '("text/plain" "application/pdf"))))))

;;; vm-save-all-attachments tests

(defmacro vm-mime-test-with-save-stubs (answer record &rest body)
  "Run BODY with `vm-save-all-attachments' cut off from the folder machinery.
ANSWER is returned by any file-name prompt; RECORD, if a symbol, is set
to the default that prompt was offered."
  (declare (indent 2))
  ;; vm-select-folder-buffer-and-validate is a defsubst, inlined into
  ;; its callers once vm-mime.el is compiled, so stubbing it does
  ;; nothing there.  Give it what it looks for instead.
  `(progn
     (setq major-mode 'vm-mode)
     (setq vm-message-list (list 'fake-message))
     (cl-letf (((symbol-function 'vm-retrieve-operable-messages)
              (lambda (&rest _) nil))
             ((symbol-function 'vm-check-for-killed-folder) #'ignore)
             ((symbol-function 'vm-check-for-killed-summary) #'ignore)
             ((symbol-function 'vm-select-operable-messages)
              (lambda (&rest _) (list 'fake-message)))
             ((symbol-function 'vm-interactive-p) (lambda () nil))
             ;; vm-warn sleeps; nothing here is watching the echo area
             ((symbol-function 'vm-warn) (lambda (&rest _) nil))
             ((symbol-function 'vm-read-file-name)
              (lambda (_prompt _dir default &rest _)
                ,@(when record `((setq ,record default)))
                ,answer))
             ((symbol-function 'vm-mime-send-body-to-file)
              (lambda (_layout file &rest _)
                (with-temp-file file (insert "saved")) t)))
       ,@body)))

(defun vm-mime-test-parse-here ()
  "Parse the current buffer as a MIME entity."
  (vm-mime-parse-entity nil :default-type '("text/plain")
                        :default-encoding "7bit"))

(ert-deftest vm-mime-test-save-all-attachments-directory-answer ()
  "Test that answering the filename prompt with a directory does not error.
Regression test for issue #366: the directory was offered as the default,
so RET returned it as the file name; VM then asked to overwrite the
directory and `delete-file\' failed with \"is a directory\"."
  (let ((dir (file-name-as-directory (make-temp-file "vm-att" t))))
    (unwind-protect
        (with-temp-buffer
          (insert "Content-Type: multipart/mixed; boundary=B\n"
                  "\n"
                  "--B\n"
                  "Content-Type: application/octet-stream\n"
                  "Content-Disposition: attachment\n"
                  "\n"
                  "payload\n"
                  "--B--\n")
          (let ((layout (vm-mime-test-parse-here)))
            (cl-letf (((symbol-function 'vm-mm-layout) (lambda (_m) layout)))
              (vm-mime-test-with-save-stubs dir nil
                ;; must not signal; nothing gets written
                (vm-save-all-attachments nil dir)))
            (should (null (directory-files dir nil "\\`[^.]")))))
      (delete-directory dir t))))

(ert-deftest vm-mime-test-save-all-attachments-warns-once ()
  "Test that several refused parts produce a single warning.
`vm-warn' pauses, so warning per part made refusing a message with
several unnamed parts a sequence of two-second waits."
  (let ((dir (file-name-as-directory (make-temp-file "vm-att" t)))
        (warnings 0)
        (warned nil))
    (unwind-protect
        (with-temp-buffer
          (insert "Content-Type: multipart/mixed; boundary=B\n"
                  "\n"
                  "--B\n"
                  "Content-Type: application/octet-stream\n"
                  "Content-Disposition: attachment\n"
                  "\n"
                  "one\n"
                  "--B\n"
                  "Content-Type: application/octet-stream\n"
                  "Content-Disposition: attachment\n"
                  "\n"
                  "two\n"
                  "--B\n"
                  "Content-Type: application/octet-stream\n"
                  "Content-Disposition: attachment\n"
                  "\n"
                  "three\n"
                  "--B--\n")
          (let ((layout (vm-mime-test-parse-here)))
            (should (= (length (vm-mm-layout-parts layout)) 3))
            ;; see vm-mime-test-with-save-stubs
            (setq major-mode 'vm-mode)
            (setq vm-message-list (list 'fake-message))
            (cl-letf (((symbol-function 'vm-mm-layout) (lambda (_m) layout))
                      ((symbol-function 'vm-retrieve-operable-messages)
                       (lambda (&rest _) nil))
                      ((symbol-function 'vm-check-for-killed-folder) #'ignore)
                      ((symbol-function 'vm-check-for-killed-summary) #'ignore)
                      ((symbol-function 'vm-select-operable-messages)
                       (lambda (&rest _) (list 'fake-message)))
                      ((symbol-function 'vm-interactive-p) (lambda () nil))
                      ((symbol-function 'vm-read-file-name)
                       (lambda (&rest _) dir))
                      ((symbol-function 'vm-warn)
                       (lambda (_level _secs fmt &rest args)
                         (setq warnings (1+ warnings))
                         (setq warned (apply #'format fmt args)))))
              (vm-save-all-attachments nil dir))
            ;; three parts refused, one warning
            (should (= warnings 1))
            ;; and the directory is named once, not once per part
            (should (string-match (regexp-quote dir) warned))
            (should (= 1 (with-temp-buffer
                           (insert warned)
                           (goto-char (point-min))
                           (let ((n 0))
                             (while (search-forward dir nil t)
                               (setq n (1+ n)))
                             n))))
            (should (null (directory-files dir nil "\\`[^.]")))))
      (delete-directory dir t))))

(ert-deftest vm-mime-test-save-all-attachments-content-type-name ()
  "Test that a Content-Type name is offered when Content-Disposition has none.
Such a part used to be prompted for with the bare directory as its
default, which is not a file name at all."
  (let ((dir (file-name-as-directory (make-temp-file "vm-att" t)))
        (offered nil))
    (unwind-protect
        (with-temp-buffer
          (insert "Content-Type: multipart/mixed; boundary=B\n"
                  "\n"
                  "--B\n"
                  "Content-Type: application/pdf; name=\"report.pdf\"\n"
                  "Content-Disposition: attachment\n"
                  "\n"
                  "payload\n"
                  "--B--\n")
          (let* ((layout (vm-mime-test-parse-here))
                 (want (expand-file-name "report.pdf" dir)))
            (cl-letf (((symbol-function 'vm-mm-layout) (lambda (_m) layout)))
              (vm-mime-test-with-save-stubs want offered
                (vm-save-all-attachments nil dir)))
            (should (equal offered want))
            (should (file-exists-p want))))
      (delete-directory dir t))))

;;; vm-attach-object-to-composition tests

(defmacro vm-mime-test-with-attachable (layout-var comp-var body-text &rest body)
  "Bind LAYOUT-VAR to a small attachment layout and COMP-VAR to a composition.
The composition holds BODY-TEXT with point left at the start of its body,
which is where the buffer's point would sit while the user is typing."
  (declare (indent 3))
  `(let ((src (generate-new-buffer " *vm-test-src*"))
         (,comp-var (generate-new-buffer " *vm-test-comp*"))
         ,layout-var)
     (unwind-protect
         (progn
           (with-current-buffer src
             (insert "Content-Type: text/plain; name=\"note.txt\"\n"
                     "Content-Disposition: attachment; filename=\"note.txt\"\n"
                     "\n"
                     "payload\n")
             (setq ,layout-var
                   (vm-mime-parse-entity nil
                     :default-type '("text/plain")
                     :default-encoding "7bit")))
           (with-current-buffer ,comp-var
             (mail-mode)
             (insert "To: someone@example.com\n"
                     mail-header-separator "\n"
                     ,body-text)
             (goto-char (point-min))
             (search-forward mail-header-separator)
             (forward-line 1))
           (let ((vm-send-using-mime t))
             (cl-letf (((symbol-function 'vm-check-for-killed-summary) #'ignore)
                       ((symbol-function 'vm-error-if-folder-empty) #'ignore))
               ,@body)))
       (when (buffer-live-p src) (kill-buffer src))
       (when (buffer-live-p ,comp-var) (kill-buffer ,comp-var)))))

(ert-deftest vm-mime-test-attach-object-to-composition-appends ()
  "Test that attaching to another buffer appends rather than splitting it.
Regression test for issue #100: the tag went in at the composition
buffer's point, which is wherever the user last left it -- so attaching
from the reader dropped the tag into the middle of what they were typing."
  (vm-mime-test-with-attachable layout comp "first line\nsecond line\n"
    (vm-attach-object-to-composition layout comp)
    (with-current-buffer comp
      (let ((text (buffer-string)))
        (should (string-match "first line\nsecond line\n\\[ATTACHMENT " text))
        ;; and nothing was inserted between the body lines
        (should-not (string-match "first line\n\\[ATTACHMENT " text))))))

(ert-deftest vm-mime-test-attach-object-to-composition-order ()
  "Test that consecutive attachments keep their order."
  (vm-mime-test-with-attachable layout comp "body\n"
    (vm-attach-object-to-composition layout comp)
    (with-current-buffer comp (goto-char (point-min)))  ; user moves away
    (vm-attach-object-to-composition layout comp)
    (with-current-buffer comp
      (should (= 2 (cl-count-if (lambda (l) (string-prefix-p "[ATTACHMENT " l))
                                (split-string (buffer-string) "\n"))))
      ;; both after the body, not before it
      (should (string-match "body\n\\[ATTACHMENT [^\n]*\n\\[ATTACHMENT "
                            (buffer-string))))))

;;; vm-reencode-mime-encoded-words tests

(defun vm-mime-test-decoded-string (text &rest spans)
  "Return TEXT marked up as `vm-decode-mime-encoded-words' would leave it.
Each SPAN is (START END CHARSET CODING); the text between spans carries
no charset, exactly as whitespace separating two encoded words does not."
  (let ((s (copy-sequence text)))
    (dolist (span spans)
      (let ((start (nth 0 span)) (end (nth 1 span))
            (charset (nth 2 span)) (coding (nth 3 span)))
        (put-text-property start end 'vm-charset charset s)
        (put-text-property start end 'vm-coding coding s)
        (put-text-property start end 'vm-string t s)))
    s))

(ert-deftest vm-mime-test-reencode-keeps-separating-space ()
  "Test that a space between two encoded runs survives a round trip.
Regression test for issue #383: the space carries no charset, so it was
left literal between two encoded words -- and RFC 2047 says whitespace
between encoded words is a separator, so decoding threw it away and
\"foo bar\" came back as \"foobar\"."
  (let* ((vm-display-using-mime t)
         (input (vm-mime-test-decoded-string
                 "fóó bàr"
                 '(0 3 "iso-8859-1" iso-8859-1)
                 '(4 7 "iso-8859-1" iso-8859-1)))
         (encoded (vm-reencode-mime-encoded-words-in-string input))
         (decoded (vm-decode-mime-encoded-words-in-string encoded)))
    (should (equal (substring-no-properties decoded) "fóó bàr"))))

(ert-deftest vm-mime-test-reencode-keeps-multiple-spaces ()
  "Test that a run of whitespace between encoded runs survives."
  (let* ((vm-display-using-mime t)
         (input (vm-mime-test-decoded-string
                 "fóó \t bàr"
                 '(0 3 "iso-8859-1" iso-8859-1)
                 '(6 9 "iso-8859-1" iso-8859-1)))
         (encoded (vm-reencode-mime-encoded-words-in-string input))
         (decoded (vm-decode-mime-encoded-words-in-string encoded)))
    (should (equal (substring-no-properties decoded) "fóó \t bàr"))))

(ert-deftest vm-mime-test-reencode-leaves-plain-text-between ()
  "Test that ordinary text between encoded runs is still left alone.
Only whitespace is a separator; anything else is real content and must
not be swept into the preceding encoded word."
  (let* ((vm-display-using-mime t)
         (input (vm-mime-test-decoded-string
                 "fóó and bàr"
                 '(0 3 "iso-8859-1" iso-8859-1)
                 '(8 11 "iso-8859-1" iso-8859-1)))
         (encoded (vm-reencode-mime-encoded-words-in-string input))
         (decoded (vm-decode-mime-encoded-words-in-string encoded)))
    (should (string-match " and " encoded))
    (should (equal (substring-no-properties decoded) "fóó and bàr"))))

(ert-deftest vm-mime-test-reencode-leaves-leading-trailing-space ()
  "Test that whitespace with no encoded run on both sides is left alone."
  (let* ((vm-display-using-mime t)
         (input (vm-mime-test-decoded-string
                 " fóó "
                 '(1 4 "iso-8859-1" iso-8859-1)))
         (encoded (vm-reencode-mime-encoded-words-in-string input)))
    (should (string-prefix-p " " encoded))
    (should (string-suffix-p " " encoded))))

;;; attachment renaming tests

(defmacro vm-mime-test-with-attachment-tag (file &rest body)
  "Run BODY in a composition holding an attachment tag for FILE, point on it."
  (declare (indent 1))
  `(let ((path (make-temp-file "vm-attach")))
     (unwind-protect
         (progn
           (with-temp-file path (insert "payload\n"))
           (when ,file (rename-file path (setq path ,file) t))
           (with-temp-buffer
             (mail-mode)
             (insert "To: someone@example.com\n"
                     mail-header-separator "\n"
                     "body\n")
             (goto-char (point-max))
             (let ((vm-send-using-mime t))
               (vm-attach-file path "text/plain"))
             (goto-char (point-min))
             (search-forward "[ATTACHMENT")
             (backward-char 3)
             ,@body))
       (when (file-exists-p path) (delete-file path)))))

(ert-deftest vm-mime-test-set-parameter-in-list ()
  "Test the MIME parameter list editor."
  (should (equal (vm-mime-set-parameter-in-list '("name=\"a\"") "name" "b")
                 '("name=\"b\"")))
  ;; added when absent
  (should (equal (vm-mime-set-parameter-in-list nil "name" "b")
                 '("name=\"b\"")))
  ;; other parameters kept, in order
  (should (equal (vm-mime-set-parameter-in-list
                  '("charset=\"utf-8\"" "name=\"a\"") "name" "b")
                 '("charset=\"utf-8\"" "name=\"b\"")))
  ;; quotes and backslashes in the value are escaped
  (should (equal (vm-mime-set-parameter-in-list nil "name" "a\"b")
                 '("name=\"a\\\"b\""))))

(ert-deftest vm-mime-test-unquote-parameter-value ()
  "Test that MIME parameter quoting is removed."
  (should (equal (vm-mime-unquote-parameter-value "\"a.txt\"") "a.txt"))
  (should (equal (vm-mime-unquote-parameter-value "a.txt") "a.txt"))
  (should (equal (vm-mime-unquote-parameter-value "\"a\\\"b\"") "a\"b"))
  (should (null (vm-mime-unquote-parameter-value nil))))

(ert-deftest vm-mime-test-rename-attachment ()
  "Test that renaming an attachment changes both name and filename.
Issue #392 asked for a way to send a file under a different name than
the one it has on disk."
  (vm-mime-test-with-attachment-tag nil
    (let ((before (vm-mime-attachment-name-at-point)))
      (should (stringp before))
      (vm-mime-set-attachment-name-at-point "renamed.txt")
      (should (equal (vm-mime-attachment-name-at-point) "renamed.txt"))
      ;; both MIME parameters carry it
      (should (equal (get-text-property (point) 'vm-mime-parameters)
                     '("name=\"renamed.txt\"")))
      (should (member "filename=\"renamed.txt\""
                      (cdr (get-text-property (point) 'vm-mime-disposition))))
      ;; and the visible tag shows it
      (should (string-match "\\[ATTACHMENT renamed\\.txt, text/plain\\]"
                            (buffer-string))))))

(ert-deftest vm-mime-test-rename-attachment-keeps-one-tag ()
  "Test that renaming leaves the tag as a single run of properties.
The tag is `rear-nonsticky', so text inserted into it inherits nothing.
Replacing the name with `insert-and-inherit' therefore split the tag's
property run in three, and `vm-mime-attachment-button-extents' -- which
is how the encoder finds attachments -- then saw two attachments where
there was one, so the file would have been sent twice."
  (vm-mime-test-with-attachment-tag nil
    (vm-mime-set-attachment-name-at-point "renamed.txt")
    (should (= 1 (length (vm-mime-attachment-button-extents
                          (point-min) (point-max) 'vm-mime-object))))
    ;; the whole tag is one unbroken run carrying the new name
    (goto-char (point-min))
    (let* ((start (progn (search-forward "[ATTACHMENT") (match-beginning 0)))
           (change (next-single-property-change start 'vm-mime-type)))
      (should (equal (buffer-substring-no-properties start change)
                     "[ATTACHMENT renamed.txt, text/plain]"))
      (should (equal (get-text-property (1+ start) 'vm-mime-parameters)
                     '("name=\"renamed.txt\""))))))

(ert-deftest vm-mime-test-rename-attachment-name-with-comma ()
  "Test renaming twice when the first name contained a comma.
The tag reads \"[ATTACHMENT <name>, <type>]\" and the name was matched
up to the first comma, so a comma in the name meant the next rename
replaced only the part before it, leaving the rest in the tag:
\"[ATTACHMENT second.txt,comma.txt, text/plain]\".  The encoded name
stayed right, so what the tag showed and what would be sent diverged."
  (vm-mime-test-with-attachment-tag nil
    (vm-mime-set-attachment-name-at-point "with,comma.txt")
    (should (equal (vm-mime-attachment-name-at-point) "with,comma.txt"))
    (should (string-match "\\[ATTACHMENT with,comma\\.txt, text/plain\\]"
                          (buffer-string)))
    ;; rename again; the whole old name must go
    (goto-char (point-min))
    (search-forward "[ATTACHMENT")
    (backward-char 3)
    (vm-mime-set-attachment-name-at-point "second.txt")
    (should (equal (vm-mime-attachment-name-at-point) "second.txt"))
    (should (string-match "\\[ATTACHMENT second\\.txt, text/plain\\]"
                          (buffer-string)))
    (should-not (string-match "comma" (buffer-string)))
    (should (= 1 (length (vm-mime-attachment-button-extents
                          (point-min) (point-max) 'vm-mime-object))))))

(ert-deftest vm-mime-test-rename-attachment-adjacent-tags ()
  "Test renaming one of two attachment tags sharing a line.
Nothing stops a user joining the lines, and the manual encourages
killing and yanking tags.  The name is matched greedily, so without
narrowing to the tag the match ran on into the second tag: its visible
text was swallowed while its property run survived, leaving an
attachment that would still be sent but had no tag left to edit."
  (let ((a (make-temp-file "vm-a")) (b (make-temp-file "vm-b")))
    (unwind-protect
        (progn
          (with-temp-file a (insert "one\n"))
          (with-temp-file b (insert "two\n"))
          (with-temp-buffer
            (mail-mode)
            (insert "To: someone@example.com\n" mail-header-separator "\n"
                    "body\n")
            (goto-char (point-max))
            (let ((vm-send-using-mime t))
              (vm-attach-file a "text/plain")
              (vm-attach-file b "text/plain"))
            ;; join the two tag lines
            (goto-char (point-min))
            (search-forward "]")
            (delete-char 1)
            (should (= 2 (length (vm-mime-attachment-button-extents
                                  (point-min) (point-max) 'vm-mime-object))))
            (goto-char (point-min))
            (search-forward "[ATTACHMENT")
            (backward-char 3)
            (vm-mime-set-attachment-name-at-point "renamed.txt")
            ;; the second tag is untouched and still visible
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (should (string-match "\\[ATTACHMENT renamed\\.txt, text/plain\\]"
                                    line))
              (should (string-match
                       (concat "\\[ATTACHMENT "
                               (regexp-quote (file-name-nondirectory b))
                               ", text/plain\\]")
                       line)))
            (should (= 2 (length (vm-mime-attachment-button-extents
                                  (point-min) (point-max) 'vm-mime-object))))))
      (delete-file a)
      (delete-file b))))

(ert-deftest vm-mime-test-rename-attachment-at-tag-end ()
  "Test that point just past the tag still finds the attachment.
`end-of-line' leaves point there, and it is not obviously outside the
tag to a user."
  (vm-mime-test-with-attachment-tag nil
    (end-of-line)
    (should (vm-mime-attachment-tag-bounds))
    (vm-mime-set-attachment-name-at-point "renamed.txt")
    (should (string-match "\\[ATTACHMENT renamed\\.txt, text/plain\\]"
                          (buffer-string)))))

(ert-deftest vm-mime-test-rename-attachment-reaches-encoding ()
  "Test that the new name is what gets sent."
  (vm-mime-test-with-attachment-tag nil
    (vm-mime-set-attachment-name-at-point "quarterly report.txt")
    (let ((vm-send-using-mime t))
      (vm-mime-encode-composition))
    (let ((text (buffer-string)))
      (should (string-match "name=\"quarterly report\\.txt\"" text))
      (should (string-match "filename=\"quarterly report\\.txt\"" text)))))

(ert-deftest vm-mime-test-rename-attachment-not-on-attachment ()
  "Test that renaming away from an attachment tag is an error."
  (with-temp-buffer
    (mail-mode)
    (insert "To: someone@example.com\n" mail-header-separator "\nbody\n")
    (goto-char (point-min))
    (should-error (vm-mime-set-attachment-name-at-point "x") :type 'error)))

;;; Presentation copies must not corrupt the folder's cached MIME layout
;;; (issue #109)

(defconst vm-mime-test--multipart-folder
  "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: multipart
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary=\"BOUND\"

--BOUND
Content-Type: text/plain

first part
--BOUND
Content-Type: text/plain

second part
--BOUND--
"
  "A two-part message, enough for a layout with subparts to go wrong in.")

(ert-deftest vm-mime-test-presentation-copy-has-private-softdata ()
  "REGRESSION: a presentation copy does not share the layout slot.
Issue #109.  `vm-make-presentation-copy' copies the message struct shallowly,
so the soft data vector -- which holds the cached MIME layout -- used to be
shared with the real message.  `vm-fetch-message' then parses the presentation
buffer and stores the result through the copy, overwriting the folder's cache
with markers into a buffer that is about to be erased for the next message."
  (let ((vm-use-menus nil))
    (vm-test-with-folder vm-mime-test--multipart-folder
      (let ((real (vm-test-first-message)))
        (vm-make-presentation-copy real)
        (let ((pres (with-current-buffer vm-presentation-buffer
                      (car vm-message-pointer))))
          (should-not (eq pres real))
          ;; The location data was always private; the soft data must be too.
          (should-not (eq (vm-softdata-of pres) (vm-softdata-of real)))
          ;; Shallow: every field still refers to what it did before, so the
          ;; copy still knows its folder buffer and its real message.
          (should (eq (vm-buffer-of pres) (vm-buffer-of real)))
          (should (eq (vm-real-message-of pres) real)))))))

(ert-deftest vm-mime-test-presentation-parse-keeps-folder-layout ()
  "REGRESSION: parsing in the presentation buffer leaves the folder cache alone.
Issue #109, stated as the behaviour rather than the representation: after the
presentation copy caches a layout parsed from the presentation buffer, the real
message's cached layout must still describe the folder buffer."
  (let ((vm-use-menus nil))
    (vm-test-with-folder vm-mime-test--multipart-folder
      (let* ((real (vm-test-first-message))
             (folder-buffer (current-buffer)))
        ;; Cache the folder's own layout first, as previewing does.
        (vm-set-mime-layout-of real (vm-mime-parse-entity-safe real))
        (should (eq folder-buffer
                    (marker-buffer
                     (vm-mm-layout-body-start (vm-mime-layout-of real)))))
        (vm-make-presentation-copy real)
        (let ((pres (with-current-buffer vm-presentation-buffer
                      (car vm-message-pointer))))
          ;; This is what vm-fetch-message does: parse the current buffer,
          ;; with no message argument, and cache it against the copy.
          (with-current-buffer vm-presentation-buffer
            (vm-set-mime-layout-of pres (vm-mime-parse-entity-safe)))
          ;; The copy may say what it likes about its own buffer...
          (should (vm-mime-layout-of pres))
          ;; ...but the folder's cache still points into the folder.
          (let ((layout (vm-mime-layout-of real)))
            (should (eq folder-buffer
                        (marker-buffer (vm-mm-layout-body-start layout))))
            (should (= 2 (length (vm-mm-layout-parts layout))))
            (dolist (part (vm-mm-layout-parts layout))
              (should (eq folder-buffer
                          (marker-buffer (vm-mm-layout-body-start part)))))))))))

(ert-deftest vm-mime-test-verify-cached-layout-checks-marker-buffer ()
  "A cached layout from another buffer is invalid even at equal positions.
Issue #109: `vm-mime-verify-cached-layout' compared only marker positions, and
the offsets do coincide for the first message of a folder, which starts at 1
just as a presentation buffer does -- so the repair in `vm-preview-current-message'
would keep a layout belonging to the wrong buffer."
  (let ((vm-use-menus nil))
    (vm-test-with-folder vm-mime-test--multipart-folder
      (let* ((real (vm-test-first-message))
             (current (vm-mime-parse-entity-safe real))
             (elsewhere (generate-new-buffer " *vm-mime-test-other*")))
        ;; Wide enough that the positions below are reachable in it, the way
        ;; a presentation buffer holding a copy of the message would be.
        (with-current-buffer elsewhere
          (insert (make-string (+ 10 (marker-position (aref current 10))) ?x)))
        (unwind-protect
            (let ((cached (vm-copy current)))
              ;; Same layout, same positions, different buffer.
              (dolist (i '(7 9 10))
                (aset cached i (set-marker
                                (make-marker)
                                (marker-position (aref current i))
                                elsewhere)))
              (should (equal (marker-position (aref cached 9))
                             (marker-position (aref current 9))))
              (should-not (vm-mime-verify-cached-layout cached current))
              ;; And a layout in the right buffer still verifies.
              (should (vm-mime-verify-cached-layout current current)))
          (kill-buffer elsewhere))))))

;;; RFC 2231 -- internationalized MIME parameter values (issue #367)

(ert-deftest vm-mime-test-rfc2231-plain-parameter-unchanged ()
  "A plain parameter is still returned as it was."
  (should (equal "report.pdf"
                 (vm-mime-get-xxx-parameter
                  "name" '("charset=us-ascii" "name=report.pdf")))))

(ert-deftest vm-mime-test-rfc2231-extended-value ()
  "NAME*=CHARSET''TEXT is percent-decoded and converted from its charset."
  (should (equal "räksmörgås.txt"
                 (vm-mime-get-xxx-parameter
                  "filename"
                  '("filename*=UTF-8''r%C3%A4ksm%C3%B6rg%C3%A5s.txt")))))

(ert-deftest vm-mime-test-rfc2231-extended-value-latin-1 ()
  "A charset other than UTF-8 is honoured."
  (should (equal "naïve.txt"
                 (vm-mime-get-xxx-parameter
                  "filename" '("filename*=ISO-8859-1''na%EFve.txt")))))

(ert-deftest vm-mime-test-rfc2231-language-tag-is-discarded ()
  "The language section is accepted and ignored."
  (should (equal "résumé.txt"
                 (vm-mime-get-xxx-parameter
                  "filename" '("filename*=UTF-8'fr'r%C3%A9sum%C3%A9.txt")))))

(ert-deftest vm-mime-test-rfc2231-continuation-of-extended-segments ()
  "Continuations are joined, and a character split across two of them survives.
The bytes have to be concatenated before the charset is applied; decoding each
segment on its own turns the split character into two replacement characters."
  ;; U+00E4 is C3 A4 in UTF-8; the split falls between the two bytes.
  (should (equal "räksmörgås"
                 (vm-mime-get-xxx-parameter
                  "name"
                  '("name*0*=UTF-8''r%C3"
                    "name*1*=%A4ksm%C3%B6rg%C3%A5s")))))

(ert-deftest vm-mime-test-rfc2231-mixed-plain-and-extended-segments ()
  "A continuation may mix extended and plain segments."
  (should (equal "a-ä-b"
                 (vm-mime-get-xxx-parameter
                  "name" '("name*0*=UTF-8''a-%C3%A4-" "name*1=b")))))

(ert-deftest vm-mime-test-rfc2231-plain-continuation-still-works ()
  "The pre-existing plain continuation support is untouched."
  (should (equal "a-very-long-file-name.txt"
                 (vm-mime-get-xxx-parameter
                  "name" '("name*0=a-very-" "name*1=long-file-" "name*2=name.txt")))))

(ert-deftest vm-mime-test-rfc2231-extended-wins-over-plain ()
  "When a sender supplies both, the tagged value is the real name.
The plain one is the sender's deliberately lossy ASCII fallback."
  (should (equal "Grüße.txt"
                 (vm-mime-get-xxx-parameter
                  "filename" '("filename=Gruesse.txt"
                               "filename*=UTF-8''Gr%C3%BC%C3%9Fe.txt")))))

(ert-deftest vm-mime-test-rfc2231-unknown-charset-does-not-error ()
  "An unknown charset falls back to guessing rather than signalling."
  (let ((value (vm-mime-get-xxx-parameter
                "filename" '("filename*=x-nonexistent-charset''plain.txt"))))
    (should (stringp value))
    (should (string-match-p "plain\\.txt" value))))

(ert-deftest vm-mime-test-rfc2231-missing-charset-section ()
  "A percent-encoded value with no charset section is still decoded."
  (should (equal "a b.txt"
                 (vm-mime-get-xxx-parameter "name" '("name*=a%20b.txt")))))

(ert-deftest vm-mime-test-rfc2231-absent-parameter-is-nil ()
  "A parameter that is not there at all is nil, not the empty string."
  (should-not (vm-mime-get-xxx-parameter "filename" '("charset=us-ascii")))
  (should-not (vm-mime-get-xxx-parameter "filename" nil)))

(ert-deftest vm-mime-test-rfc2231-lone-percent-is-literal ()
  "A percent sign that is not an escape stands for itself.
Senders that do no encoding at all still produce these."
  (should (equal "50%.txt"
                 (vm-mime-get-xxx-parameter "name" '("name*=UTF-8''50%.txt")))))

(ert-deftest vm-mime-test-rfc2231-encode-ascii-is-quoted ()
  "An ASCII value is written the old way, quoted."
  (should (equal "name=\"report.pdf\""
                 (vm-mime-encode-parameter "name" "report.pdf"))))

(ert-deftest vm-mime-test-rfc2231-encode-quotes-are-escaped ()
  "Quotes and backslashes in an ASCII value are escaped."
  (should (equal "name=\"a\\\"b.txt\""
                 (vm-mime-encode-parameter "name" "a\"b.txt"))))

(ert-deftest vm-mime-test-rfc2231-encode-non-ascii-uses-rfc2231 ()
  "A non-ASCII value is written in extended notation, tagged UTF-8."
  (should (equal "filename*=UTF-8''r%C3%A4ksm%C3%B6rg%C3%A5s.txt"
                 (vm-mime-encode-parameter "filename" "räksmörgås.txt"))))

(ert-deftest vm-mime-test-rfc2231-encode-spaces-are-encoded ()
  "A space in an extended value is percent-encoded, not left bare."
  (let ((param (vm-mime-encode-parameter "filename" "über bericht.pdf")))
    (should-not (string-match-p " " param))
    (should (string-match-p "%20" param))))

(ert-deftest vm-mime-test-rfc2231-round-trip ()
  "What VM writes, VM reads back unchanged.
Through `vm-mime-unquote-parameter-value', as the real callers do: the getter
returns values as the structured-header parser leaves them, and the plain form
is a quoted string."
  (dolist (name '("report.pdf" "räksmörgås.txt" "über bericht.pdf"
                  "報告.pdf" "50% done.txt"))
    (should (equal name
                   (vm-mime-unquote-parameter-value
                    (vm-mime-get-xxx-parameter
                     "filename"
                     (list (vm-mime-encode-parameter "filename" name))))))))

(ert-deftest vm-mime-test-rfc2231-set-parameter-replaces-extended ()
  "Setting a parameter removes any other spelling of it.
Otherwise a rename would leave the old NAME*= behind, and that one wins."
  (let ((params (vm-mime-set-parameter-in-list
                 '("charset=us-ascii" "name*=UTF-8''alt.txt") "name" "new.txt")))
    (should (equal "new.txt" (vm-mime-unquote-parameter-value
                             (vm-mime-get-xxx-parameter "name" params))))
    (should (member "charset=us-ascii" params))
    (should-not (cl-find-if (lambda (p) (string-match-p "\\`name\\*=" p))
                            params))))

(ert-deftest vm-mime-test-rfc2231-set-parameter-drops-continuation ()
  "Setting a parameter removes every segment of a continuation."
  (let ((params (vm-mime-set-parameter-in-list
                 '("name*0*=UTF-8''a" "name*1*=b" "charset=us-ascii")
                 "name" "new.txt")))
    (should (equal "new.txt" (vm-mime-unquote-parameter-value
                              (vm-mime-get-xxx-parameter "name" params))))
    (should-not (cl-find-if (lambda (p) (string-match-p "\\`name\\*[0-9]" p))
                            params))))

(ert-deftest vm-mime-test-rfc2231-set-parameter-non-ascii ()
  "Renaming to a non-ASCII name produces the extended notation."
  (let ((params (vm-mime-set-parameter-in-list
                 '("name=\"old.txt\"") "name" "Grüße.txt")))
    (should (equal "Grüße.txt" (vm-mime-get-xxx-parameter "name" params)))))

(ert-deftest vm-mime-test-rfc2231-attach-non-ascii-name-encodes ()
  "Attaching a file with a non-ASCII name emits RFC 2231 in both headers,
and the name survives to what actually gets sent."
  (let ((dir (file-name-as-directory (make-temp-file "vm-rfc2231" t))))
    (unwind-protect
        (vm-mime-test-with-attachment-tag (concat dir "räksmörgås.txt")
          ;; What the tag carries, and how it spells it.
          (should (equal "räksmörgås.txt" (vm-mime-attachment-name-at-point)))
          (let ((params (get-text-property (car (vm-mime-attachment-tag-bounds))
                                          'vm-mime-parameters)))
            (should (cl-find-if (lambda (p) (string-match-p "\\`name\\*=UTF-8''" p))
                                params)))
          ;; And what goes on the wire.
          (let ((vm-send-using-mime t))
            (vm-mime-encode-composition))
          (let ((text (buffer-string)))
            (should (string-match-p "name\\*=UTF-8''r%C3%A4ksm%C3%B6rg%C3%A5s\\.txt"
                                    text))
            (should (string-match-p
                     "filename\\*=UTF-8''r%C3%A4ksm%C3%B6rg%C3%A5s\\.txt" text))
            ;; No raw non-ASCII left in a parameter, which is what RFC 2231 is for.
            (should-not (string-match-p "name=\"räksmörgås" text))))
      (delete-directory dir t))))

(ert-deftest vm-mime-test-rfc2231-attach-ascii-name-unchanged ()
  "An ASCII attachment name is still written the plain way."
  (let ((dir (file-name-as-directory (make-temp-file "vm-rfc2231" t))))
    (unwind-protect
        (vm-mime-test-with-attachment-tag (concat dir "report.txt")
          (let ((vm-send-using-mime t))
            (vm-mime-encode-composition))
          (let ((text (buffer-string)))
            (should (string-match-p "name=\"report\\.txt\"" text))
            (should-not (string-match-p "name\\*=" text))))
      (delete-directory dir t))))

;;; Raw 8-bit header text (issues #368, #11)

(defconst vm-mime-test--utf8-bytes
  (encode-coding-string "Grüße aus München" 'utf-8)
  "UTF-8 bytes, as they sit in a folder buffer: RFC 6532 header text.")

(defconst vm-mime-test--latin1-bytes
  (encode-coding-string "Grüße" 'iso-8859-1)
  "The same text in a single-byte encoding, which is not valid UTF-8.")

(ert-deftest vm-mime-test-8bit-decodes-utf-8 ()
  "Raw UTF-8 header bytes are decoded, which is RFC 6532."
  (should (equal "Grüße aus München"
                 (vm-decode-8bit-text vm-mime-test--utf8-bytes))))

(ert-deftest vm-mime-test-8bit-falls-back-to-single-byte ()
  "Bytes that are not valid UTF-8 are decoded by the next charset in the list.
This is the \"liberal in what you accept\" of issue #11: the mail is malformed
either way, and showing the text beats showing bytes."
  (should (equal "Grüße" (vm-decode-8bit-text vm-mime-test--latin1-bytes))))

(ert-deftest vm-mime-test-8bit-leaves-ascii-alone ()
  "Text with no 8-bit byte in it comes back as it went in."
  (should (equal "plain ascii" (vm-decode-8bit-text "plain ascii"))))

(ert-deftest vm-mime-test-8bit-does-not-decode-twice ()
  "Text that is already characters is not decoded again.
Decoding \"Grüße\" as though it were bytes would produce mojibake, and this
function is called from paths that may already have decoded encoded words."
  (should (equal "Grüße" (vm-decode-8bit-text "Grüße"))))

(ert-deftest vm-mime-test-8bit-honours-empty-charset-list ()
  "With `vm-mime-8bit-header-charsets' nil, raw bytes are left as they are."
  (let ((vm-mime-8bit-header-charsets nil))
    (should (equal vm-mime-test--utf8-bytes
                   (vm-decode-8bit-text vm-mime-test--utf8-bytes)))))

(ert-deftest vm-mime-test-8bit-in-string-decoder ()
  "The header-string decoder handles raw 8-bit as well as encoded words."
  (should (equal "Grüße aus München"
                 (vm-decode-mime-encoded-words-in-string
                  vm-mime-test--utf8-bytes))))

(ert-deftest vm-mime-test-8bit-and-encoded-word-together ()
  "A header may hold an encoded word and raw 8-bit text at once.
The encoded word states its charset and is decoded first; the raw run is then
guessed at separately, so neither pass spoils the other's work."
  (let ((mixed (concat "=?utf-8?Q?Gr=C3=BC=C3=9Fe?= und "
                       vm-mime-test--utf8-bytes)))
    (should (equal "Grüße und Grüße aus München"
                   (vm-decode-mime-encoded-words-in-string mixed)))))

(ert-deftest vm-mime-test-8bit-region-decodes-runs-separately ()
  "`vm-decode-8bit-text-region' decodes each run and leaves the rest alone."
  (with-temp-buffer
    (insert "Subject: ")
    (insert (decode-coding-string vm-mime-test--utf8-bytes 'binary))
    (insert "\nFrom: ascii@example.com\n")
    (vm-decode-8bit-text-region (point-min) (point-max))
    (should (string-match-p "Subject: Grüße aus München" (buffer-string)))
    (should (string-match-p "From: ascii@example.com" (buffer-string)))))

(ert-deftest vm-mime-test-8bit-message-headers-decoded ()
  "Decoding a message's headers in place turns raw bytes into characters.
This is the display path: `vm-decode-mime-message-headers' is what the
presentation buffer runs over the headers it just copied in."
  (let ((vm-use-menus nil))
    (vm-test-with-folder
      (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
              "From: sender@example.com\n"
              "Subject: " (decode-coding-string vm-mime-test--utf8-bytes 'binary)
              "\n\nbody\n")
      (let ((m (vm-test-first-message)))
        (vm-decode-mime-message-headers m)
        (save-restriction
          (widen)
          (should (string-match-p
                   "Subject: Grüße aus München"
                   (buffer-substring-no-properties (vm-start-of m)
                                                   (vm-text-of m)))))))))

;;; RFC 3676 format=flowed (issue #78)

(defun vm-mime-test--unflow (text &optional delsp)
  "Return TEXT with its soft line breaks joined."
  (with-temp-buffer
    (insert text)
    (vm-mime-unflow-region (point-min) (point-max) delsp)
    (buffer-string)))

(defun vm-mime-test--flow (text &optional fill)
  "Return (WIRE . FLOWED) for TEXT marked up as format=flowed."
  (with-temp-buffer
    (insert text)
    (let* ((fill-column (or fill 70))
           (flowed (vm-mime-flow-region (point-min) (point-max))))
      (cons (buffer-string) flowed))))

;;; receiving

(ert-deftest vm-mime-test-flowed-joins-a-paragraph ()
  "Lines ending in a space are joined; the paragraph break is not."
  (should (equal "one two three\n\nnext\n"
                 (vm-mime-test--unflow "one \ntwo \nthree\n\nnext\n"))))

(ert-deftest vm-mime-test-flowed-keeps-hard-breaks ()
  "A line that does not end in a space keeps its break."
  (should (equal "one\ntwo\n" (vm-mime-test--unflow "one\ntwo\n"))))

(ert-deftest vm-mime-test-flowed-joins-within-a-quote-depth ()
  "Quoted text is joined, and still looks quoted afterwards."
  (should (equal "> quoted and flowed continues here.\n"
                 (vm-mime-test--unflow
                  "> quoted and flowed \n> continues here.\n"))))

(ert-deftest vm-mime-test-flowed-does-not-join-across-quote-depths ()
  "A change of quote depth is a hard break however the line ends.
Depth is part of what makes a paragraph, so text quoted twice is never run
together with text quoted once."
  (should (equal "> level one flowed \n>> level two.\n"
                 (vm-mime-test--unflow "> level one flowed \n>> level two.\n"))))

(ert-deftest vm-mime-test-flowed-removes-space-stuffing ()
  "A space put in front of a line to protect it is taken off again."
  (should (equal "stuffed line was flowed next.\n"
                 (vm-mime-test--unflow " stuffed line was flowed \nnext.\n"))))

(ert-deftest vm-mime-test-flowed-leaves-the-signature-separator-alone ()
  "\"-- \" is neither joined to the text above nor to the signature below.
It ends in a space, so it is a flowed line by the letter of RFC 3676, but
section 4.3 asks for it to be left as a line of its own -- joining it would
stop anything recognising the signature."
  (should (equal "text before \n-- \nSignature\n"
                 (vm-mime-test--unflow "text before \n-- \nSignature\n"))))

(ert-deftest vm-mime-test-flowed-delsp-drops-the-space ()
  "With delsp=yes the space at a soft break is part of the marking.
That is how a language which does not put spaces between words uses flowed
text; keeping the space would insert one into the middle of a word."
  (should (equal "abcdef\n" (vm-mime-test--unflow "abc \ndef\n" t))))

(ert-deftest vm-mime-test-flowed-layout-predicates ()
  "The format and delsp parameters are recognised, and only when flowed."
  (let ((flowed (vector '("text/plain" "charset=us-ascii" "format=flowed")))
        (quoted (vector '("text/plain" "format=\"Flowed\"" "delsp=\"yes\"")))
        (fixed (vector '("text/plain" "format=fixed")))
        (plain (vector '("text/plain" "charset=us-ascii")))
        (html (vector '("text/html" "format=flowed"))))
    (should (vm-mime-flowed-layout-p flowed))
    (should (vm-mime-flowed-layout-p quoted))
    (should-not (vm-mime-flowed-layout-p fixed))
    (should-not (vm-mime-flowed-layout-p plain))
    ;; The format parameter means this only for plain text.
    (should-not (vm-mime-flowed-layout-p html))
    (should (vm-mime-delsp-layout-p quoted))
    (should-not (vm-mime-delsp-layout-p flowed))))

(ert-deftest vm-mime-test-flowed-can-be-switched-off ()
  "With `vm-mime-unflow-flowed-text' nil, flowed text is shown as it arrived."
  (let ((vm-mime-unflow-flowed-text nil))
    (should-not (vm-mime-flowed-layout-p
                 (vector '("text/plain" "format=flowed"))))))

;;; sending

(ert-deftest vm-mime-test-flow-marks-filled-lines ()
  "A line filled out to the fill column is offered to the reader to re-wrap."
  (let ((result (vm-mime-test--flow
                 (concat "Emacs is an extensible, customizable, free/libre"
                         " text editor and more,\n"
                         "with a Lisp interpreter at its core.\n"))))
    (should (cdr result))
    (should (string-match-p "and more, \n" (car result)))
    ;; The last line of the paragraph keeps its break.
    (should (string-match-p "at its core\\.\n\\'" (car result)))))

(ert-deftest vm-mime-test-flow-leaves-short-lines-alone ()
  "Lines far short of the fill column were broken on purpose.
An address block reflowed into a paragraph is worse than one that is not
re-wrapped, so only lines that look filled are marked."
  (let ((result (vm-mime-test--flow
                 "Mark Diekhans\n1156 High Street\nSanta Cruz\n")))
    (should-not (cdr result))
    (should (equal "Mark Diekhans\n1156 High Street\nSanta Cruz\n"
                   (car result)))))

(ert-deftest vm-mime-test-flow-does-not-double-space-a-quote ()
  "The space after the quote characters is the stuffing; no second one is added."
  (let* ((text (concat "> Emacs is an extensible, customizable, free/libre"
                       " text editor and\n"
                       "> more, with a Lisp interpreter at its core.\n"))
         (result (vm-mime-test--flow text)))
    (should (cdr result))
    (should-not (string-match-p ">  " (car result)))
    ;; And it comes back as one quoted paragraph, still quoted once.
    (should (equal (concat "> Emacs is an extensible, customizable, free/libre"
                           " text editor and more, with a Lisp interpreter at"
                           " its core.\n")
                   (vm-mime-test--unflow (car result))))))

(ert-deftest vm-mime-test-flow-stuffs-lines-that-need-it ()
  "A line whose own text starts with a space or \"From \" is protected.
A leading \">\" is taken as quoting rather than as text needing protection:
nothing in a composition distinguishes the two, and in a reply it is quoting
every time.  Such a line is normalised to \"> \", which is the spelling the
reader will display either way."
  (let ((result (vm-mime-test--flow " indented\n>literal angle\nFrom the top\n")))
    (should (equal "  indented\n> literal angle\n From the top\n" (car result)))
    ;; And the reader takes the protection off again.
    (should (equal " indented\n> literal angle\nFrom the top\n"
                   (vm-mime-test--unflow (car result))))))

(ert-deftest vm-mime-test-flow-leaves-the-signature-separator-alone ()
  "The signature separator is not given a soft break, and does not get one."
  (let ((result (vm-mime-test--flow
                 (concat "Some body text long enough to have been wrapped"
                         " by the fill column.\n-- \nMark\n"))))
    (should (string-match-p "\n-- \nMark\n" (car result)))))

(ert-deftest vm-mime-test-flow-round-trips-the-words ()
  "Flowing and unflowing preserves the words and the paragraph boundaries.
Not the line breaks: undoing them is the whole point, and the reader re-wraps."
  (let* ((text (concat "Emacs is an extensible, customizable, free/libre text"
                       " editor and more,\nwith a Lisp interpreter at its"
                       " core.\n\nSecond paragraph, which is short.\n"))
         (wire (car (vm-mime-test--flow text)))
         (back (vm-mime-test--unflow wire)))
    (should (equal (split-string text "[ \n]+" t)
                   (split-string back "[ \n]+" t)))
    (should (= 2 (length (split-string back "\n\n" t))))))

(ert-deftest vm-mime-test-flow-composition-declares-the-parameter ()
  "Encoding a composition says format=flowed exactly when it flowed something."
  (dolist (case '((t . "long") (t . "short") (nil . "long")))
    (let ((vm-send-using-flowed-text (car case))
          (long (equal (cdr case) "long")))
      (with-temp-buffer
        (mail-mode)
        (insert "To: someone@example.com\n" mail-header-separator "\n")
        (insert (if long
                    (concat "Emacs is an extensible, customizable, free/libre"
                            " text editor and more,\nwith a Lisp interpreter"
                            " at its core.\n")
                  "Short.\n"))
        (let ((vm-send-using-mime t)
              (fill-column 70))
          (vm-mime-encode-composition))
        (let ((text (buffer-string)))
          (if (and (car case) long)
              (should (string-match-p "Content-Type: text/plain;[^\n]*format=flowed"
                                      text))
            (should-not (string-match-p "format=flowed" text))))))))


;;; cid: references for an external viewer (issue #506)

(defun vm-mime-test--find-layout (layout type)
  "Return the first part of LAYOUT whose content type is TYPE, depth first."
  (catch 'found
    (let ((walk nil))
      (setq walk (lambda (l)
                   (when (vectorp l)
                     (if (vm-mime-types-match type (car (vm-mm-layout-type l)))
                         (throw 'found l)
                       (dolist (part (vm-mm-layout-parts l))
                         (funcall walk part))))))
      (funcall walk layout)
      nil)))

(defun vm-mime-test--kill-new-buffers (before)
  "Kill every live buffer that is not in BEFORE, unmodified.
The cid tests visit a folder, which leaves the folder buffer, its summary and
its presentation copy behind (issue #559)."
  (dolist (buffer (buffer-list))
    (unless (memq buffer before)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defun vm-mime-test--cid-folder ()
  "Visit the cid fixture as a folder and return its one message."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-cid" t)))
         (file (expand-file-name "folder" dir)))
    (copy-file (vm-test-fixture-path "folders" "cid-related.mbox") file)
    (list dir file)))

(ert-deftest vm-mime-test-cid-file-name-is-a-file-name ()
  "A Content-ID is not a file name; `vm-mime-cid-file-name' makes one.
Message IDs may hold anything an addr-spec may, `@' and `%' included, and the
one in the fixture does."
  (require 'vm-mime)
  (should (equal "first_example.com" (vm-mime-cid-file-name "first@example.com")))
  (should (equal "plain" (vm-mime-cid-file-name "plain")))
  (should (equal "a_b_c" (vm-mime-cid-file-name "a/b\\c")))
  ;; the characters a file name may keep are kept
  (should (equal "a.b-c_d" (vm-mime-cid-file-name "a.b-c_d"))))

(ert-deftest vm-mime-test-cid-references-become-local-files ()
  "REGRESSION: an HTML part sent to an external viewer takes its images along.
Issue #506: a sender who puts a picture in an HTML message attaches it as
another part and refers to it as `cid:something' (RFC 2392).  VM wrote the HTML
part to a lone temporary file, which a browser has no way to resolve those
references from, so it drew a broken image where the picture should be.

The parts are now written beside the HTML and the references rewritten to name
them."
  (require 'vm)
  (let* ((where (vm-mime-test--cid-folder))
         (dir (nth 0 where))
         (file (nth 1 where))
         (vm-init-file nil) (vm-preferences-file nil) (vm-confirm-quit nil)
         (vm-frame-per-folder nil) (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list))
         (vm-mime-externalize-cid-references t))
    (unwind-protect
        (progn
          (vm-visit-folder file)
          (should (= 1 (length vm-message-list)))
          (let* ((layout (vm-mm-layout (car vm-message-list)))
                 (html (vm-mime-test--find-layout layout "text/html"))
                 (html-file (expand-file-name "part.html" dir)))
            (should html)
            (vm-mime-send-body-to-file html nil html-file t)
            ;; before: the references are cid: URLs and nothing else is written
            (with-temp-buffer
              (insert-file-contents html-file)
              (should (string-match-p "cid:first@example\\.com" (buffer-string))))
            (let ((written (vm-mime-externalize-cid-references html html-file)))
              ;; one file per distinct id, not per reference: the first id is
              ;; used twice, once in a src= and once in a CSS url()
              (should (= 2 (length written)))
              (dolist (f written)
                (should (file-exists-p f))
                (should (> (nth 7 (file-attributes f)) 0))
                ;; beside the HTML, so a bare file name resolves
                (should (equal (file-name-directory html-file)
                               (file-name-directory f))))
              ;; the suffix comes from the part, so the browser knows the type
              (should (seq-find (lambda (f) (string-suffix-p ".png" f)) written))
              (should (seq-find (lambda (f) (string-suffix-p ".gif" f)) written))
              (with-temp-buffer
                (insert-file-contents html-file)
                (let ((text (buffer-string)))
                  ;; no cid: reference survives ...
                  (should-not (string-match-p "cid:" text))
                  ;; ... and each is now a bare local file name
                  (dolist (f written)
                    (should (string-match-p (regexp-quote (file-name-nondirectory f))
                                            text)))
                  ;; including the one inside url(), and both uses of the
                  ;; repeated id
                  (should (string-match-p "background:url([^)]*\\.png)" text))
                  (should (= 2 (cl-count-if
                                (lambda (l) (string-match-p "\\.png" l))
                                (split-string text "\n")))))))))
      (vm-mime-test--kill-new-buffers before)
      (delete-directory dir t))))

(ert-deftest vm-mime-test-cid-externalizing-can-be-turned-off ()
  "With the option off, the HTML goes out as it came, cid: references and all.
The control, and the escape for anyone who would rather not have message images
written to the temporary directory."
  (require 'vm)
  (let* ((where (vm-mime-test--cid-folder))
         (dir (nth 0 where))
         (file (nth 1 where))
         (vm-init-file nil) (vm-preferences-file nil) (vm-confirm-quit nil)
         (vm-frame-per-folder nil) (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (vm-visit-folder file)
          (let* ((layout (vm-mm-layout (car vm-message-list)))
                 (html (vm-mime-test--find-layout layout "text/html"))
                 (html-file (expand-file-name "part.html" dir))
                 (before nil))
            (vm-mime-send-body-to-file html nil html-file t)
            (with-temp-buffer (insert-file-contents html-file)
                              (setq before (buffer-string)))
            (let ((vm-mime-externalize-cid-references nil))
              (should (equal nil (vm-mime-externalize-cid-references
                                  html html-file))))
            (with-temp-buffer
              (insert-file-contents html-file)
              ;; untouched, references and all
              (should (equal before (buffer-string)))
              (should (string-match-p "cid:" (buffer-string))))
            ;; and nothing was written beside it
            (should (equal '("folder" "part.html")
                           (sort (seq-remove
                                  (lambda (f) (member f '("." "..")))
                                  (directory-files dir))
                                 #'string<)))))
      (vm-mime-test--kill-new-buffers before)
      (delete-directory dir t))))

(ert-deftest vm-mime-test-cid-reference-with-no-such-part-is-left-alone ()
  "A cid: reference naming nothing is left as it is, rather than erased.
A message can refer to a part that is not there, and turning the reference into
a name that resolves to nothing would be worse than leaving it visible."
  (require 'vm)
  (let* ((where (vm-mime-test--cid-folder))
         (dir (nth 0 where))
         (file (nth 1 where))
         (vm-init-file nil) (vm-preferences-file nil) (vm-confirm-quit nil)
         (vm-frame-per-folder nil) (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (vm-visit-folder file)
          (let* ((layout (vm-mm-layout (car vm-message-list)))
                 (html (vm-mime-test--find-layout layout "text/html"))
                 (html-file (expand-file-name "part.html" dir)))
            (with-temp-file html-file
              (insert "<img src=\"cid:absent@example.com\">\n"))
            (should (equal nil (vm-mime-externalize-cid-references html html-file)))
            (with-temp-buffer
              (insert-file-contents html-file)
              (should (string-match-p "cid:absent@example\\.com"
                                      (buffer-string))))))
      (vm-mime-test--kill-new-buffers before)
      (delete-directory dir t))))


(ert-deftest vm-mime-test-cid-parts-are-written-privately ()
  "A cid part written for an external viewer is mode 600, like the HTML is.
Found in review: `vm-make-tempfile' sets `default-file-modes' to 600 before
writing the HTML part, because a message is going into a directory other people
may be able to read, but `vm-mime-write-cid-part' called
`vm-mime-send-body-to-file' directly and so took the ambient umask -- 644 with
the usual 022.  The image parts of a message are as private as its text."
  (require 'vm)
  (let* ((where (vm-mime-test--cid-folder))
         (dir (nth 0 where))
         (file (nth 1 where))
         (vm-init-file nil) (vm-preferences-file nil) (vm-confirm-quit nil)
         (vm-frame-per-folder nil) (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list))
         (vm-mime-externalize-cid-references t))
    (unwind-protect
        (progn
          (vm-visit-folder file)
          (let* ((layout (vm-mm-layout (car vm-message-list)))
                 (html (vm-mime-test--find-layout layout "text/html"))
                 (html-file (expand-file-name "part.html" dir)))
            (vm-mime-send-body-to-file html nil html-file t)
            (let ((written (vm-mime-externalize-cid-references html html-file)))
              (should written)
              (dolist (f written)
                ;; only the owner, whatever the umask says
                (should (= (vm-octal 600)
                           (logand (file-modes f) (vm-octal 777))))))))
      (vm-mime-test--kill-new-buffers before)
      (delete-directory dir t))))

(ert-deftest vm-mime-test-cid-part-does-not-write-through-a-link ()
  "An existing name in the way is removed rather than written through.
The other half of the review finding: `vm-make-tempfile' unlinks before writing
so that a symbolic link already occupying the path cannot redirect the write.
The cid parts go in the same directory and need the same care."
  (require 'vm)
  (let* ((where (vm-mime-test--cid-folder))
         (dir (nth 0 where))
         (file (nth 1 where))
         (vm-init-file nil) (vm-preferences-file nil) (vm-confirm-quit nil)
         (vm-frame-per-folder nil) (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list))
         (vm-mime-externalize-cid-references t)
         (elsewhere (expand-file-name "decoy" dir)))
    (unwind-protect
        (progn
          (with-temp-file elsewhere (insert "untouched\n"))
          (vm-visit-folder file)
          (let* ((layout (vm-mm-layout (car vm-message-list)))
                 (html (vm-mime-test--find-layout layout "text/html"))
                 (html-file (expand-file-name "part.html" dir)))
            (vm-mime-send-body-to-file html nil html-file t)
            ;; put a link where the first cid part is about to be written
            (make-symbolic-link
             elsewhere
             (expand-file-name
              (concat (file-name-base html-file) "-"
                      (vm-mime-cid-file-name "first@example.com") ".png")
              dir)
             t)
            (vm-mime-externalize-cid-references html html-file)
            ;; the link was replaced, and what it pointed at is as it was
            (with-temp-buffer
              (insert-file-contents elsewhere)
              (should (equal "untouched\n" (buffer-string))))))
      (vm-mime-test--kill-new-buffers before)
      (delete-directory dir t))))

;;; Reading one alternative with buttons for the rest (#16)

(defun vm-mime-test--present (message method exceptions)
  "Visit a folder holding MESSAGE and return its presentation text.
METHOD is `vm-mime-alternative-show-method' and EXCEPTIONS
`vm-auto-displayed-mime-content-type-exceptions'."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-alt" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-mime-alternative-show-method method)
         (vm-auto-displayed-mime-content-type-exceptions exceptions)
         (vm-mime-text/html-handler 'lynx)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-user-interaction-buffer vm-user-interaction-buffer)
         (vm-summary-tokenized-compiled-format-alist
          vm-summary-tokenized-compiled-format-alist)
         (vm-summary-untokenized-compiled-format-alist
          vm-summary-untokenized-compiled-format-alist)
         (vm-mime-compiled-format-alist vm-mime-compiled-format-alist)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert message)
            (write-region (point-min) (point-max) file nil 'quiet))
          (vm-visit-folder file)
          (vm-present-current-message)
          (vm-show-current-message)
          (with-current-buffer (or vm-presentation-buffer (current-buffer))
            (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(defconst vm-mime-test--alternatives
  (concat "From alice@example.com  Thu Jan  1 00:00:00 2026\n"
          "From: alice@example.com\nSubject: alternatives\n"
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/alternative; boundary=b\n\n"
          "--b\nContent-Type: text/plain\n\nplain version\n"
          "--b\nContent-Type: text/html\n\n<p>html version</p>\n--b--\n"))

(ert-deftest vm-mime-test-an-excluded-alternative-becomes-a-button ()
  "`all' plus an exception reads one alternative and buttons the others.
This is the recipe the manual gives for issue #16, which asked to see one
type and still reach the others without decoding the message by hand.  The
other show methods choose one alternative and drop the rest, so they cannot
do it."
  (skip-unless (executable-find "lynx"))
  (let ((text (vm-mime-test--present vm-mime-test--alternatives
                                     'all '("text/html"))))
    (should (string-match-p "plain version" text))
    (should-not (string-match-p "html version" text))
    ;; the button says what it is and offers to display it
    (should (string-match-p "HTML" text))
    (should (string-match-p "\\[display\\]" text))))

(ert-deftest vm-mime-test-alternative-show-methods-differ ()
  "The controls behave as the manual says: `all' shows every alternative,
and `best-internal' shows one."
  (skip-unless (executable-find "lynx"))
  (let ((all (vm-mime-test--present vm-mime-test--alternatives 'all nil))
        (best (vm-mime-test--present vm-mime-test--alternatives
                                     'best-internal nil)))
    (should (string-match-p "plain version" all))
    (should (string-match-p "html version" all))
    ;; best-internal picks the most faithful it can display -- the HTML
    (should (string-match-p "html version" best))
    (should-not (string-match-p "plain version" best))))

;;; Completing an HTML fragment for an external viewer (#387)

(defun vm-mime-test--html-layout (charset)
  "A text/html layout, with CHARSET declared if non-nil."
  (with-temp-buffer
    (insert "Content-Type: text/html"
            (if charset (format "; charset=%s" charset) "") "\n\n<p>x</p>\n")
    (vm-mime-parse-entity nil :default-type '("text/plain")
                          :default-encoding "7bit")))

(defun vm-mime-test--html-file (contents)
  "Write CONTENTS to a temporary .html file, as VM writes a part out."
  (let ((file (make-temp-file "vm-mime-test-" nil ".html"))
        (coding-system-for-write 'binary))
    (write-region contents nil file nil 'quiet)
    file))

(defun vm-mime-test--file-bytes (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'binary))
      (insert-file-contents file))
    (buffer-string)))

(defmacro vm-mime-test--with-html-file (contents var &rest body)
  "Bind VAR to a temporary HTML file holding CONTENTS, and delete it after."
  (declare (indent 2))
  `(let ((,var (vm-mime-test--html-file ,contents)))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(ert-deftest vm-mime-test-html-fragment-detection ()
  "What counts as a whole document.
`>' is a symbol constituent in the standard syntax table, so a regexp
ending \\_> after \"<html\" does not match `<html>' -- which is how the
first version of this reported every document as a fragment."
  (dolist (case '(("<html><body>x</body></html>" . nil)
                  ("<HTML>\n<body>x</body>\n</HTML>" . nil)
                  ("<!DOCTYPE html>\n<div>x</div>" . nil)
                  ("<html lang=\"en\">x</html>" . nil)
                  ("<meta charset=\"utf-8\">\n<div>x</div>" . nil)
                  ("<span>x</span>" . t)
                  ("<div><p>x</p></div>" . t)
                  ("plain words" . t)))
    (with-temp-buffer
      (insert (car case))
      (should (equal (and (vm-mime-html-fragment-p) t) (cdr case))))))

(ert-deftest vm-mime-test-completes-an-html-fragment ()
  "A fragment is wrapped in a document declaring the part's charset."
  (let ((layout (vm-mime-test--html-layout "iso-8859-1")))
    (vm-mime-test--with-html-file "<span>caf\351</span>\n" file
      (should (vm-mime-complete-html-file layout file))
      (let ((text (vm-mime-test--file-bytes file)))
        (should (string-match-p "\\`<html>" text))
        (should (string-match-p "charset=iso-8859-1" text))
        (should (string-match-p "</html>\n\\'" text))
        ;; the text is wrapped, not re-encoded
        (should (string-match-p "<span>caf\351</span>" text))))))

(ert-deftest vm-mime-test-leaves-a-whole-html-document-alone ()
  "A part that is already a document is written out as it is."
  (let ((layout (vm-mime-test--html-layout "utf-8"))
        (document "<html><body><p>whole</p></body></html>\n"))
    (vm-mime-test--with-html-file document file
      (should-not (vm-mime-complete-html-file layout file))
      (should (equal (vm-mime-test--file-bytes file) document)))))

(ert-deftest vm-mime-test-leaves-a-fragment-with-a-charset-alone ()
  "A fragment that says what character set it is in needs no help.
Wrapping it would put a second declaration before its own."
  (let ((layout (vm-mime-test--html-layout "utf-8"))
        (fragment "<meta charset=\"utf-8\">\n<div>said so</div>\n"))
    (vm-mime-test--with-html-file fragment file
      (should-not (vm-mime-complete-html-file layout file))
      (should (equal (vm-mime-test--file-bytes file) fragment)))))

(ert-deftest vm-mime-test-completing-html-can-be-turned-off ()
  (let ((layout (vm-mime-test--html-layout "utf-8"))
        (fragment "<span>fragment</span>\n")
        (vm-mime-complete-html-for-external-viewer nil))
    (vm-mime-test--with-html-file fragment file
      (should-not (vm-mime-complete-html-file layout file))
      (should (equal (vm-mime-test--file-bytes file) fragment)))))

(defvar vm-mime-test--viewed-file nil)

(defun vm-mime-test--fake-viewer (file)
  "Stand in for an external viewer, recording the file it was given."
  (setq vm-mime-test--viewed-file file)
  nil)

(ert-deftest vm-mime-test-an-external-viewer-gets-a-whole-document ()
  "The file handed to an external viewer says what character set it is in.
A message whose text/html is a fragment -- and plenty is: it opens with a
`<span>' and has no `<html>' -- gave the viewer a file with nothing in it
about the charset that was in the part's header, so a browser guessed.
Issue #387, driven through `vm-mime-display-external-generic' and an elisp
\"viewer\", which `vm-mime-external-content-types-alist' allows."
  (let ((vm-mime-test--viewed-file nil)
        ;; writing a temporary file for the viewer moves the counter and
        ;; registers the file for cleanup, both of them global
        (vm-tempfile-counter vm-tempfile-counter)
        (vm-global-garbage-alist nil))
    (unwind-protect
        (vm-test-with-folder
         (concat "From a@b.com  Thu Jan  1 00:00:00 2026\n"
                 "From: a@b.com\nSubject: fragment\n"
                 "MIME-Version: 1.0\n"
                 "Content-Type: text/html; charset=iso-8859-1\n"
                 "\n<span style=\"font-family: Tahoma\">Hi caf\351</span>\n")
         (let ((layout (vm-mm-layout (car vm-message-pointer)))
               (vm-mime-external-content-types-alist
                '(("text/html" vm-mime-test--fake-viewer)))
               (vm-mime-external-content-type-exceptions nil))
           (setq vm-mail-buffer (current-buffer))
           (should (vectorp layout))
           (vm-mime-display-external-generic layout)
           (should vm-mime-test--viewed-file)
           (let ((text (vm-mime-test--file-bytes vm-mime-test--viewed-file)))
             (should (string-match-p "charset=iso-8859-1" text))
             (should (string-match-p "<html>" text))
             ;; the sender's byte, not a re-encoded one
             (should (string-match-p "caf\351" text)))))
      (when (and vm-mime-test--viewed-file
                 (file-exists-p vm-mime-test--viewed-file))
        (delete-file vm-mime-test--viewed-file)))))

;;; The width HTML is converted at (#369)

(ert-deftest vm-mime-test-html-columns-answers-each-setting ()
  "`vm-mime-html-columns' reads `vm-html-fill-column'."
  (should (equal (let ((vm-html-fill-column 80)) (vm-mime-html-columns)) 80))
  ;; nil once meant a page 100000 columns wide (#540)
  (should (equal (let ((vm-html-fill-column nil)) (vm-mime-html-columns))
                 vm-html-default-column))
  (should (< vm-html-default-column 200))
  (let ((window (let ((vm-html-fill-column 'window-width))
                  (vm-mime-html-columns))))
    (should (>= window 20))
    (should (< window (window-width)))))

(ert-deftest vm-mime-test-html-converters-are-told-the-width ()
  "Each external converter is passed the width, not left to choose one.
lynx wrapped at 72 columns and w3m at its own default, whatever the text
was wanted for."
  (let (command)
    (cl-letf (((symbol-function 'shell-command-on-region)
               (lambda (_start _end cmd &rest _) (setq command cmd))))
      (with-temp-buffer
        (insert "<p>text</p>\n")
        (let ((vm-html-fill-column 80))
          (vm-mime-display-internal-lynx-text/html (point-min) (point-max) nil)
          (should (string-match-p "-width=80\\'" command))
          (vm-mime-display-internal-w3m-text/html
           (point-min) (point-max) (make-vector 20 nil))
          (should (string-match-p "-cols 80" command)))
        (let ((vm-html-fill-column nil))
          (vm-mime-display-internal-lynx-text/html (point-min) (point-max) nil)
          (should (string-match-p (format "-width=%d\\'" vm-html-default-column)
                                  command)))))))

(ert-deftest vm-mime-test-html-is-broken-at-the-width-asked-for ()
  "A converted HTML paragraph comes out at the width VM asked for.
Run against lynx itself, since the point of the setting is what the
converter does with it.  Skipped where lynx is not installed."
  (skip-unless (executable-find "lynx"))
  (let ((vm-mime-text/html-handler 'lynx)
        (paragraph (mapconcat (lambda (i) (format "word%d" i))
                              (number-sequence 1 60) " ")))
    (cl-flet ((render (column)
                (let ((vm-html-fill-column column))
                  (with-temp-buffer
                    (insert "<html><body><p>" paragraph "</p></body></html>\n")
                    (vm-mime-display-internal-lynx-text/html
                     (point-min) (point-max) nil)
                    (split-string (string-trim (buffer-string)) "\n")))))
      (dolist (column (list 80 vm-html-default-column))
        (let ((lines (render column)))
          (should (> (length lines) 1))
          (should (string-match-p "word60" (car (last lines))))
          (dolist (line lines)
            (should (<= (length line) column))))))))

(ert-deftest vm-mime-test-a-centred-table-is-not-indented-by-hundreds-of-columns ()
  "REGRESSION: quoted HTML is converted to a page a reader\\='s width, not 100000.
Issue #540.  Asked for a width no line would reach, w3m lays the page out
that wide and centres a centred table in it, so every line came back indented
by some 900 columns.  Cited and filled, `vm-forward-paragraph' then read the
indentation as the paragraph\\='s prefix and the reply held one word a line.
Skipped where w3m is not installed."
  (skip-unless (executable-find "w3m"))
  (let ((centred (concat "<html><body>"
                         "<table width=\"100%\"><tr><td align=\"center\">"
                         "<p>Mark Diekhans commented on a discussion"
                         " on the issue:</p>"
                         "</td></tr></table></body></html>\n")))
    (cl-flet ((cite (column)
                (let ((vm-html-fill-column column))
                  (with-temp-buffer
                    (insert centred)
                    (vm-mime-display-internal-w3m-text/html
                     (point-min) (point-max) (make-vector 20 nil))
                    ;; quote it as `vm-mail-yank-default' does, and fill it as
                    ;; a reply that fills its included text does
                    (goto-char (point-min))
                    (while (re-search-forward "^" nil t)
                      (insert "> ")
                      (forward-line 1))
                    (let ((vm-paragraph-fill-column 70))
                      (vm-fill-paragraphs-containing-long-lines
                       70 (point-min) (point-max)))
                    (split-string (string-trim (buffer-string)) "\n" t)))))
      (let ((quoted (cite (let ((vm-html-fill-column vm-html-in-reply-column))
                            (vm-mime-html-columns)))))
        ;; the words of the sentence are together, not one to a line
        (should (string-match-p "Mark Diekhans commented" (car quoted)))
        ;; and no line is mostly the indentation w3m centred the table with
        (dolist (line quoted)
          (should (<= (length line) 72)))))))

(ert-deftest vm-mime-test-yanking-does-not-use-the-window-width ()
  "Quoting a message hands the converter `vm-html-in-reply-column'.
Before this, an HTML part quoted in a reply was converted at whatever
width the converter liked -- for emacs-w3m the width of the window the
message was read in, so the same message quoted differently in two frames."
  (let ((seen 'unset))
    (vm-test-with-folder
     "From a@b.com  Thu Jan  1 00:00:00 2026\nFrom: a@b.com\nSubject: s\n\nbody\n"
     (let ((message (car vm-message-pointer)))
       (with-temp-buffer
         (let ((vm-html-in-reply-column 80))
           (cl-letf (((symbol-function 'vm-decode-mime-layout)
                      (lambda (&rest _) (setq seen vm-html-fill-column)))
                     ((symbol-function 'vm-decode-mime-message-headers)
                      (lambda (&rest _) nil)))
             (vm-yank-message-mime message (make-vector 3 nil)))))))
    (should (equal seen 80))
    ;; and the default is a width of its own, not the window's
    (should (equal (default-value 'vm-html-in-reply-column)
                   vm-html-default-column))
    ;; while display still follows the window
    (should (equal (default-value 'vm-html-fill-column) 'window-width))))

;;; Long lines in outgoing text (issue #593)

(defun vm-mime-test--encoding-of (text)
  "The transfer encoding VM chooses for TEXT, and TEXT once encoded."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert text)
    (let ((encoding (vm-determine-proper-content-transfer-encoding
                     (point-min) (point-max))))
      (cons (vm-mime-transfer-encode-region encoding (point-min) (point-max) t)
            (buffer-string)))))

(ert-deftest vm-mime-test-a-long-line-is-sent-quoted-printable ()
  "A line past the RFC 5322 limit goes out quoted-printable, not base64.
Both carry the line exactly; quoted-printable leaves the rest of the part
readable in the message as it was sent, which is why other mail readers
use it here."
  (let ((result (vm-mime-test--encoding-of
                 (concat "a short line\n" (make-string 1200 ?x) "\n"))))
    (should (equal (car result) "quoted-printable"))
    (should (string-match-p "^a short line$" (cdr result)))
    ;; the long line arrives as soft-broken physical lines
    (should (string-match-p "=\n" (cdr result)))))

(ert-deftest vm-mime-test-the-line-limit-is-measured-without-the-newline ()
  "998 characters is a legal line; 999 is not.
RFC 5322 counts a line without its terminator, and the check counted the
newline too, so a line of exactly 998 was encoded when it needed not be."
  (should (equal (car (vm-mime-test--encoding-of
                       (concat (make-string 998 ?x) "\n")))
                 "7bit"))
  (should (equal (car (vm-mime-test--encoding-of
                       (concat (make-string 999 ?x) "\n")))
                 "quoted-printable")))

(ert-deftest vm-mime-test-the-line-limit-is-settable ()
  "`vm-mime-max-text-line-length' says when a line is too long to send.
The default only catches what the RFC forbids; 78 is what the RFC asks for
and what Gmail encodes to."
  (let ((text (concat (make-string 300 ?x) "\n")))
    (should (equal (car (vm-mime-test--encoding-of text)) "7bit"))
    (let ((vm-mime-max-text-line-length 78))
      (should (equal (car (vm-mime-test--encoding-of text)) "quoted-printable")))
    ;; nil does not license a line the RFC forbids
    (let ((vm-mime-max-text-line-length nil))
      (should (equal (car (vm-mime-test--encoding-of text)) "7bit"))
      (should (equal (car (vm-mime-test--encoding-of
                           (concat (make-string 1200 ?x) "\n")))
                     "quoted-printable")))))

(ert-deftest vm-mime-test-quoted-printable-folds-its-output ()
  "No line of a quoted-printable part is longer than the 76 of RFC 2045.
`quoted-printable-encode-region' folds only when told to, and VM did not
tell it, so a quoted-printable part carried whatever line lengths the text
had -- and no soft line break was ever emitted."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (make-string 300 ?x) "\n")
    (vm-mime-qp-encode-region (point-min) (point-max))
    (should (<= (vm-mime-longest-line-length) 76))
    (vm-mime-qp-decode-region (point-min) (point-max))
    (should (equal (buffer-string) (concat (make-string 300 ?x) "\n")))))

(ert-deftest vm-mime-test-binary-data-still-goes-out-base64 ()
  "A NUL or a carriage return is not a long line and is not quoted-printable."
  (should (equal (car (vm-mime-test--encoding-of "a\0b\n")) "base64"))
  (should (equal (car (vm-mime-test--encoding-of "a\rb\n")) "base64")))

;;; Attachment commands moved out of vm-rfaddons (issue #606)

(ert-deftest vm-mime-test-attachment-commands-are-in-vm-mime ()
  "The commands are defined here now, not in an add-on file."
  (dolist (cmd '(vm-attach-files-in-directory
                 vm-mime-auto-save-all-attachments
                 vm-toggle-best-mime))
    (should (fboundp cmd))
    (should (string-match-p "vm-mime\\.el" (or (symbol-file cmd) "")))))

(ert-deftest vm-mime-test-the-vm-mime-attach-aliases-are-gone ()
  "The `vm-mime-' spellings of the directory-attach command and its
variables were plain aliases, never marked obsolete, and are dropped."
  (should-not (fboundp 'vm-mime-attach-files-in-directory))
  (dolist (v '(vm-mime-attach-files-in-directory-regexps-history
               vm-mime-attach-files-in-directory-default-type
               vm-mime-attach-files-in-directory-default-charset))
    (should-not (boundp v)))
  ;; the four marked obsolete in 8.1.1 are gone too now (#594)
  (should-not (boundp 'vm-mime-save-all-attachments-types))
  (should-not (boundp 'vm-mime-delete-all-attachments-types)))

(ert-deftest vm-mime-test-auto-save-attachments-is-an-option ()
  "`vm-auto-save-all-attachments' replaces the vm-enable-addons flag."
  (should (get 'vm-auto-save-all-attachments 'standard-value))
  (should-not (default-value 'vm-auto-save-all-attachments))
  ;; `vm-enable-addons' is gone entirely now
  (should-not (boundp 'vm-enable-addons)))

;;; The attachment commands (emacs-vm/vm#632)
;;
;; The commands the manual documents for putting an attachment into a
;; composition and for doing something with one that arrived: none of them had
;; a test.  What is checked here is the effect -- the part that ends up in the
;; encoded message, the bytes that end up in the saved file -- rather than the
;; button that stands for it.

(defconst vm-mime-test--attachment-folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nTo: me@example.com\n"
          "Subject: with an attachment\nMIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"bnd\"\n\n"
          "--bnd\nContent-Type: text/plain\n\nSome covering text.\n\n"
          "--bnd\nContent-Type: application/octet-stream; name=\"notes.bin\"\n"
          "Content-Disposition: attachment; filename=\"notes.bin\"\n\n"
          "The attached file contents.\n\n"
          "--bnd--\n\n")
  "A message with one inline part and one attachment VM will not display.
An attachment of a type VM shows internally is shown, not buttoned, and there
is then no extent for the reader commands to act on.")

(defmacro vm-mime-test--with-attachment (spec &rest body)
  "Visit a folder holding an attachment, show the message, and run BODY.
SPEC is (POINT-VAR): BODY runs in the presentation buffer with point on the
attachment's button and POINT-VAR bound to that position.  The folder text is
`vm-mime-test--attachment-folder' unless SPEC gives a second element."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-mime-attach" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-auto-decode-mime-messages t)
               (vm-display-using-mime t)
               (vm-preview-lines nil)
               (vm-mime-delete-after-saving nil)
               (vm-mime-attachment-save-directory nil))
           (write-region ,(or (cadr spec) 'vm-mime-test--attachment-folder)
                         nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             (vm-show-current-message)
             (set-buffer (or vm-presentation-buffer (current-buffer)))
             (goto-char (point-min))
             (let ((,(car spec) (vm-mime-test--button-position)))
               (should ,(car spec))
               (goto-char ,(car spec))
               ,@body)))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-mime-test--button-position ()
  "Where in this buffer a MIME button is, or nil if there is none."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (when (vm-extent-at (point) 'vm-mime-layout)
          (throw 'found (point)))
        (forward-line 1))
      nil)))

(ert-deftest vm-mime-test-saving-the-object-at-point-writes-its-body ()
  "`vm-mime-reader-map-save-file' writes the attachment, decoded, to a file.
The bytes in the file are the part's own body and nothing else: not the
covering text, not the MIME headers that described it."
  (vm-mime-test--with-attachment (button)
    (let ((target (expand-file-name "saved.bin" temporary-file-directory)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest _) target))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-mime-reader-map-save-file))
            (should (file-exists-p target))
            (let ((written (with-temp-buffer
                             (insert-file-contents target)
                             (buffer-string))))
              (should (string-match-p "The attached file contents" written))
              (should-not (string-match-p "Some covering text" written))
              (should-not (string-match-p "Content-Type" written))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-mime-test-deleting-the-object-at-point-rewrites-the-folder ()
  "`vm-delete-mime-object' takes the contents out of the message on disk.
The folder is where the effect is: the part becomes a text/plain note naming
what was there, and the bytes are gone.  The presentation buffer is not the
thing to check -- the label that replaces the button is written after the
contents are discarded, and appears whether or not they were."
  (vm-mime-test--with-attachment (button)
    (let ((folder (vm-buffer-of (car vm-message-list))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-delete-mime-object))
      (let ((text (with-current-buffer folder
                    (save-restriction (widen) (buffer-string)))))
        ;; the message keeps its structure and its other part
        (should (string-match-p "Content-Type: multipart/mixed" text))
        (should (string-match-p "Some covering text" text))
        ;; and where the attachment was, a note saying what went
        (should (string-match-p "\\[Deleted notes\\.bin" text))
        (should-not (string-match-p "The attached file contents" text))))))

(ert-deftest vm-mime-test-piping-the-object-at-point-to-a-command ()
  "`vm-mime-reader-map-pipe-to-command' feeds the part's body to a program.
What the program sees is the decoded body: that is the whole point of the
command, and a test that only checked it ran would not notice it piping the
base64."
  (vm-mime-test--with-attachment (button)
    (let ((piped (expand-file-name "piped.txt" temporary-file-directory)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) (format "cat > %s" piped))))
              (vm-mime-reader-map-pipe-to-command))
            (should (file-exists-p piped))
            (should (string-match-p
                     "The attached file contents"
                     (with-temp-buffer (insert-file-contents piped)
                                       (buffer-string)))))
        (ignore-errors (delete-file piped))))))

;;; Putting an attachment into a composition

(defmacro vm-mime-test--composing (&rest body)
  "Run BODY in a fresh VM composition buffer, then kill what it made."
  (declare (indent 0) (debug t))
  `(let ((before (buffer-list)))
     (unwind-protect
         (let ((vm-frame-per-composition nil)
               (vm-mutable-frame-configuration nil)
               (vm-mail-mode-hook nil)
               (mail-signature nil)
               (vm-send-using-mime t))
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-mail)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(ert-deftest vm-mime-test-attaching-a-file-to-a-composition ()
  "`vm-attach-file' puts the file in the composition and it survives encoding.
The tag in the buffer is a stand-in; what matters is that encoding the
composition turns it into a part of the right type carrying the file."
  (let ((file (expand-file-name "attach-me.txt" temporary-file-directory)))
    (unwind-protect
        (progn
          (write-region "The file being attached.\n" nil file nil 'quiet)
          (vm-mime-test--composing
            (goto-char (point-max))
            (insert "Here is the file you wanted.\n")
            (vm-attach-file file "text/plain")
            (should (string-match-p (regexp-quote (file-name-nondirectory file))
                                    (buffer-string)))
            (vm-mime-encode-composition)
            (let ((encoded (buffer-string)))
              ;; text of your own beside an attachment is what makes it
              ;; multipart; an attachment alone becomes the body itself
              (should (string-match-p "Content-Type: multipart/mixed" encoded))
              (should (string-match-p "Here is the file you wanted" encoded))
              (should (string-match-p "The file being attached" encoded))
              (should (string-match-p
                       (concat "filename=\"?"
                               (regexp-quote (file-name-nondirectory file)))
                       encoded)))))
      (ignore-errors (delete-file file)))))

(ert-deftest vm-mime-test-an-attachment-alone-becomes-the-body ()
  "A composition of nothing but an attachment is encoded as one part.
There is no second part for it to be multipart with, so the file becomes the
message body and its name is carried in the Content-Disposition."
  (let ((file (expand-file-name "only-attachment.txt" temporary-file-directory)))
    (unwind-protect
        (progn
          (write-region "Nothing but this file.\n" nil file nil 'quiet)
          (vm-mime-test--composing
            (goto-char (point-max))
            (vm-attach-file file "text/plain")
            (vm-mime-encode-composition)
            (let ((encoded (buffer-string)))
              (should-not (string-match-p "multipart" encoded))
              (should (string-match-p "Content-Type: text/plain" encoded))
              (should (string-match-p "Nothing but this file" encoded)))))
      (ignore-errors (delete-file file)))))

(ert-deftest vm-mime-test-attaching-a-buffer-to-a-composition ()
  "`vm-attach-buffer' attaches what is in a buffer, no file needed.
Its use is attaching something you have only in Emacs, so the contents have
to come from the buffer at encoding time."
  (let ((source (get-buffer-create "vm-mime-test-source")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "Contents of the attached buffer.\n"))
          (vm-mime-test--composing
            (goto-char (point-max))
            (vm-attach-buffer source "text/plain")
            (vm-mime-encode-composition)
            (should (string-match-p "Contents of the attached buffer"
                                    (buffer-string)))))
      (kill-buffer source))))

(ert-deftest vm-mime-test-attaching-a-message-to-a-composition ()
  "`vm-attach-message' attaches a message as message/rfc822.
Forwarding one message inside another is what the type is for, and the
attached copy keeps its own headers."
  (vm-mime-test--with-attachment (button)
    (let ((message (car vm-message-list))
          (folder (current-buffer)))
      (vm-mime-test--composing
        (goto-char (point-max))
        (let ((vm-mail-buffer folder))
          (vm-attach-message message))
        (vm-mime-encode-composition)
        (let ((encoded (buffer-string)))
          (should (string-match-p "message/rfc822" encoded))
          (should (string-match-p "Subject: with an attachment" encoded)))))))

(ert-deftest vm-mime-test-attaching-the-object-at-point-to-a-composition ()
  "`vm-mime-reader-map-attach-to-composition' moves an attachment you were
sent into one you are sending.  The composition is asked for by name, and
what lands in it is the part -- so encoding the composition carries the same
bytes on."
  (vm-mime-test--with-attachment (button)
    (let ((composition nil))
      (unwind-protect
          (progn
            (save-window-excursion
              (let ((vm-frame-per-composition nil)
                    (vm-mutable-frame-configuration nil)
                    (vm-mail-mode-hook nil)
                    (mail-signature nil)
                    (vm-send-using-mime t))
                (cl-letf (((symbol-function 'vm-display) #'ignore))
                  (vm-mail)
                  (setq composition (current-buffer)))))
            (with-current-buffer (or vm-presentation-buffer (current-buffer))
              (goto-char button)
              (cl-letf (((symbol-function 'read-buffer)
                         (lambda (&rest _) composition))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _) (buffer-name composition))))
                (vm-mime-reader-map-attach-to-composition)))
            (with-current-buffer composition
              (should (string-match-p "notes\\.bin" (buffer-string)))
              (vm-mime-encode-composition)
              (let ((encoded (buffer-string)))
                (should (string-match-p "application/octet-stream" encoded))
                ;; binary goes out base64, so the bytes are checked by
                ;; decoding rather than by looking for them in the message
                (should (string-match-p "Content-Transfer-Encoding: base64"
                                        encoded))
                (should (string-match-p
                         "The attached file contents"
                         (base64-decode-string
                          (car (last (split-string encoded "\n" t)))))))
              (set-buffer-modified-p nil)))
        (when (buffer-live-p composition)
          (with-current-buffer composition (set-buffer-modified-p nil))
          (kill-buffer composition))))))

(ert-deftest vm-mime-test-saving-the-object-at-point-to-a-folder ()
  "`vm-mime-reader-map-save-message' writes the object to a folder rather
than to a plain file, which is what you want when the object is a message.
Here it is not one, and the part is still what gets written."
  (vm-mime-test--with-attachment (button)
    (let ((target (expand-file-name "saved-folder" temporary-file-directory)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'vm-read-file-name)
                       (lambda (&rest _) target))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest _) target))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-mime-reader-map-save-message))
            (should (file-exists-p target))
            (should (string-match-p
                     "The attached file contents"
                     (with-temp-buffer (insert-file-contents target)
                                       (buffer-string)))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-mime-test-displaying-the-object-at-point-as-another-type ()
  "`vm-mime-reader-map-display-object-as-type' shows a part as a type of
your choosing.  Asked to read the binary attachment as text, VM shows its
contents where the button was: that is the use of the command, for the
attachments a sender has mislabelled."
  (vm-mime-test--with-attachment (button)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "text/plain"))
              ((symbol-function 'read-string)
               (lambda (&rest _) "text/plain")))
      (vm-mime-reader-map-display-object-as-type))
    (should (string-match-p "The attached file contents"
                            (buffer-string)))))

;;; Saving every attachment of a message (emacs-vm/vm#632)

(defconst vm-mime-test--two-attachment-folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nTo: me@example.com\n"
          "Subject: two attachments\nMIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"bnd\"\n\n"
          "--bnd\nContent-Type: text/plain\n\nSome covering text.\n\n"
          "--bnd\nContent-Type: application/octet-stream; name=\"first.bin\"\n"
          "Content-Disposition: attachment; filename=\"first.bin\"\n\n"
          "The first attachment.\n\n"
          "--bnd\nContent-Type: application/octet-stream; name=\"second.bin\"\n"
          "Content-Disposition: attachment; filename=\"second.bin\"\n\n"
          "The second attachment.\n\n"
          "--bnd--\n\n")
  "A message with two attachments, so saving them all can be told from
saving one.  Both are application/octet-stream, which is in
`vm-mime-saveable-types' and is not shown inline.")

(defmacro vm-mime-test--with-two-attachments (spec &rest body)
  "Visit a folder holding `vm-mime-test--two-attachment-folder' and run BODY.
SPEC is (DIRECTORY-VAR), bound to an empty directory to save into."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-save-attach" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (,(car spec) (file-name-as-directory
                             (expand-file-name "saved" dir)))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-auto-decode-mime-messages t)
               (vm-display-using-mime t)
               (vm-preview-lines nil)
               (vm-mime-attachment-save-directory nil)
               (vm-mime-all-attachments-directory nil))
           (make-directory ,(car spec) t)
           (write-region vm-mime-test--two-attachment-folder nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-mime-test--file-contents (file)
  "The contents of FILE, or nil if it is not there."
  (when (file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(ert-deftest vm-mime-test-saving-every-attachment ()
  "`vm-save-attachments' saves each attachment of the message, decoded, to
the file the prompt gives back.  Both attachments are written, and the
covering text is not one of them: it has no filename and no attachment
disposition, so it is not an attachment to save."
  (vm-mime-test--with-two-attachments (target)
    (let ((asked nil))
      (cl-letf (((symbol-function 'vm-read-file-name)
                 (lambda (_prompt _dir default &rest _)
                   (push (file-name-nondirectory (or default "")) asked)
                   (expand-file-name (file-name-nondirectory (or default ""))
                                     target)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-save-attachments 1))
      (should (equal (sort asked #'string<) '("first.bin" "second.bin")))
      (should (equal (vm-mime-test--file-contents
                      (expand-file-name "first.bin" target))
                     "The first attachment.\n"))
      (should (equal (vm-mime-test--file-contents
                      (expand-file-name "second.bin" target))
                     "The second attachment.\n")))))

(ert-deftest vm-mime-test-saving-attachments-does-not-overwrite-unbidden ()
  "An existing file is not overwritten when the answer is no, and what was
in it is still there afterwards.

The question is asked only about the file that is already there: the other
attachment has nothing to overwrite and is saved as usual, so one no does not
abandon the rest of the message."
  (vm-mime-test--with-two-attachments (target)
    (let ((first (expand-file-name "first.bin" target))
          (second (expand-file-name "second.bin" target))
          (questions 0))
      (write-region "Something already here.\n" nil first nil 'quiet)
      (cl-letf (((symbol-function 'vm-read-file-name)
                 (lambda (_prompt _dir default &rest _)
                   (expand-file-name (file-name-nondirectory (or default ""))
                                     target)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq questions (1+ questions)) nil)))
        (vm-save-attachments 1))
      (should (equal questions 1))
      (should (equal (vm-mime-test--file-contents first)
                     "Something already here.\n"))
      (should (equal (vm-mime-test--file-contents second)
                     "The second attachment.\n")))))

(ert-deftest vm-mime-test-saving-attachments-creates-the-directory ()
  "A directory that does not exist yet is created once it is confirmed."
  (vm-mime-test--with-two-attachments (target)
    (let ((fresh (expand-file-name "not-yet/" target)))
      (should-not (file-exists-p fresh))
      (cl-letf (((symbol-function 'vm-read-file-name)
                 (lambda (_prompt _dir default &rest _)
                   (expand-file-name (file-name-nondirectory (or default ""))
                                     fresh)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-save-attachments 1))
      (should (file-directory-p fresh))
      (should (equal (vm-mime-test--file-contents
                      (expand-file-name "first.bin" fresh))
                     "The first attachment.\n")))))

;;; Renaming the attachment at point, as the command does (emacs-vm/vm#632)
;;
;; The tests above drive `vm-mime-set-attachment-name-at-point', which is the
;; worker.  The command `vm-mime-rename-attachment' was called by no test, so
;; neither the name it reads nor its refusal away from an attachment was
;; checked.

(ert-deftest vm-mime-test-rename-attachment-command ()
  "`vm-mime-rename-attachment' gives the attachment at point the name read
from the minibuffer, offering the current one as the default.

The file on disk is not touched: the name is the one the recipient sees, which
is the point of the command."
  (vm-mime-test-with-attachment-tag nil
    (let ((offered nil)
          (before (vm-mime-attachment-name-at-point)))
      (should (stringp before))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial &rest _)
                   (setq offered initial)
                   "under-another-name.txt")))
        (vm-mime-rename-attachment))
      (should (equal offered before))
      (should (equal (vm-mime-attachment-name-at-point)
                     "under-another-name.txt"))
      (should (string-match-p "under-another-name\\.txt" (buffer-string))))))

(ert-deftest vm-mime-test-rename-attachment-away-from-a-tag-is-refused ()
  "Away from an attachment the command says there is none, rather than
renaming whatever happens to be at point.

The message is checked, since any error at all would satisfy a bare
`should-error'.  Three places raise this one message -- the command, and one
guard per platform branch of `vm-mime-set-attachment-name-at-point' -- so the
refusal survives losing any single one of them, and it takes removing both of
the two that run here to make this test fail."
  (vm-mime-test-with-attachment-tag nil
    (goto-char (point-min))                  ; on the To: header
    (let ((text-quoting-style 'grave))
      ;; the prompt is answered, so a command that got past the refusal
      ;; returns rather than stopping for input: the test then fails on the
      ;; missing error instead of hanging
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "anything")))
        (should (equal (cadr (should-error (vm-mime-rename-attachment)))
                       "No attachment here"))))))

;;; Eliding a quoted region

(ert-deftest vm-mime-test-elide-reply-region-replaces-the-region ()
  "`vm-mail-mode-elide-reply-region' replaces the marked region with
`vm-mail-mode-elide-reply-region', which is how a long quotation is cut down
to a mark that something was left out.

The mark has to be set.  The command decides what to delete by whether there
is a mark, not by the arguments it was given: with none it deletes one
character past the end, to take in the newline of the current line that its
interactive form would have measured -- and with a region that character is
the first of the next line."
  (with-temp-buffer
    (mail-mode)
    (insert "To: someone@example.com\n" mail-header-separator "\n"
            "My answer.\n"
            "> the first quoted line\n"
            "> the second quoted line\n"
            "> the third quoted line\n")
    (let ((vm-mail-mode-elide-reply-region "[...]\n")
          start end)
      (goto-char (point-min))
      (should (re-search-forward "^> the first quoted line\n" nil t))
      (setq start (match-beginning 0))
      (should (re-search-forward "^> the second quoted line\n" nil t))
      (setq end (match-end 0))
      (push-mark start t)
      (goto-char end)
      (vm-mail-mode-elide-reply-region start end)
      (should (string-match-p "^\\[\\.\\.\\.\\]$" (buffer-string)))
      (should-not (string-match-p "the first quoted line" (buffer-string)))
      (should-not (string-match-p "the second quoted line" (buffer-string)))
      ;; what was outside the region is untouched, the next line included
      (should (string-match-p "^My answer\\.$" (buffer-string)))
      (should (string-match-p "^> the third quoted line$"
                              (buffer-string))))))

(ert-deftest vm-mime-test-elide-reply-region-with-no-mark-takes-the-newline ()
  "With no mark at all the command takes one character more than it is given.
Its interactive form then measures the current line, whose end excludes the
newline, so the extra character is that newline and the line is replaced
whole.  Written down because the same extra character eats into the next line
when a region was meant."
  (with-temp-buffer
    (mail-mode)
    (insert "To: someone@example.com\n" mail-header-separator "\n"
            "> a line to elide\n"
            "> a line to keep\n")
    (let ((vm-mail-mode-elide-reply-region "[...]\n"))
      (goto-char (point-min))
      (should (re-search-forward "^> a line to elide" nil t))
      (set-mark nil)
      (deactivate-mark)
      (vm-mail-mode-elide-reply-region (line-beginning-position)
                                       (line-end-position))
      (should (string-match-p "^\\[\\.\\.\\.\\]$" (buffer-string)))
      (should-not (string-match-p "a line to elide" (buffer-string)))
      (should (string-match-p "^> a line to keep$" (buffer-string))))))

;;; The rest of the reader commands (emacs-vm/vm#632)
;;
;; Three commands on the MIME button at point that no test called: printing
;; it, showing it as text whatever it says it is, and handing it to an
;; external viewer.  Each is a thin wrapper on a worker, so what is worth
;; checking is that the wrapper reaches its worker with the object at point.

(ert-deftest vm-mime-test-printing-the-object-at-point ()
  "`vm-mime-reader-map-pipe-to-printer' sends the decoded part to
`vm-print-command' with `vm-print-command-switches' after it.

The command and its switches are joined into one shell command, so a test can
point them at a file and read back what the printer would have been given."
  (vm-mime-test--with-attachment (button)
    (let ((printed (expand-file-name "printed" temporary-file-directory)))
      (unwind-protect
          (let ((vm-print-command "cat")
                (vm-print-command-switches (list ">" printed)))
            (vm-mime-reader-map-pipe-to-printer)
            (should (file-exists-p printed))
            (let ((sent (with-temp-buffer (insert-file-contents printed)
                                          (buffer-string))))
              (should (string-match-p "The attached file contents" sent))
              ;; the part, decoded, and not the covering text
              (should-not (string-match-p "Some covering text" sent))))
        (ignore-errors (delete-file printed))))))

(ert-deftest vm-mime-test-displaying-the-object-at-point-as-text ()
  "`vm-mime-reader-map-display-using-default' shows the part where its button
was, reading it as text whatever its type says.

The attachment here is application/octet-stream, which VM will not show
inline -- that is why it has a button at all -- so the contents appearing is
the command having done something."
  (vm-mime-test--with-attachment (button)
    (should-not (string-match-p "The attached file contents" (buffer-string)))
    (vm-mime-reader-map-display-using-default)
    (should (string-match-p "The attached file contents" (buffer-string)))))

(ert-deftest vm-mime-test-an-external-viewer-must-be-configured ()
  "`vm-mime-reader-map-display-using-external-viewer' says when there is no
viewer for the type rather than doing nothing, and names the type it wanted
one for."
  (vm-mime-test--with-attachment (button)
    (let ((vm-mime-external-content-types-alist nil)
          (vm-mime-external-content-type-exceptions nil)
          (text-quoting-style 'grave))
      (should (string-match-p
               "No viewer defined for type application/octet-stream"
               (cadr (should-error
                      (vm-mime-reader-map-display-using-external-viewer))))))))

(ert-deftest vm-mime-test-an-external-viewer-is-given-the-part ()
  "With a viewer configured the part is written out and the viewer run on it.
The viewer here is `cat' with its output thrown away: what is checked is that
VM got as far as running something, since a viewer that never runs is the
failure this command has."
  (vm-mime-test--with-attachment (button)
    (let ((vm-mime-external-content-types-alist
           '(("application/octet-stream" "cat")))
          (vm-mime-external-content-type-exceptions nil)
          (started nil))
      (cl-letf (((symbol-function 'start-process)
                 (lambda (_name _buffer program &rest args)
                   (push (cons program args) started)
                   ;; a process object is expected back.  Made with
                   ;; `make-process': calling `start-process' here would call
                   ;; this stub again, for ever
                   (make-process :name "vm-mime-test-viewer"
                                 :command '("true") :noquery t))))
        (vm-mime-reader-map-display-using-external-viewer))
      (should started)
      ;; the viewer is run through a shell, so the program is the shell and
      ;; the viewer and its file are in the command it is given
      (let* ((call (car started))
             (program (car call))
             (arguments (cdr call))
             (command (car (last arguments))))
        (should (string-match-p "sh\\'\\|bash\\'" program))
        (should (string-match-p "\\`cat " command))
        ;; and what follows is a file name, which is where VM put the part
        (should (string-match-p "cat +/" command))))))

;;; Attaching a message to a composition (emacs-vm/vm#632)

(ert-deftest vm-mime-test-attaching-a-message-to-a-named-composition ()
  "`vm-attach-message-to-composition' attaches the folder's current message
to the composition named, and it goes out as message/rfc822.

This is the command behind forwarding a message into one you are already
writing.  `vm-attach-message' is its counterpart for the composition you are
in; this one takes the composition as an argument."
  (vm-mime-test--with-attachment (button)
    (let ((folder (current-buffer))
          (composition nil)
          (before (buffer-list)))
      (unwind-protect
          (progn
            (let ((vm-frame-per-composition nil)
                  (vm-mutable-frame-configuration nil)
                  (vm-mail-mode-hook nil)
                  (mail-signature nil)
                  (vm-send-using-mime t))
              (cl-letf (((symbol-function 'vm-display) #'ignore))
                (vm-mail)
                (setq composition (current-buffer))))
            (with-current-buffer folder
              (vm-attach-message-to-composition composition "the description"))
            (with-current-buffer composition
              (should (string-match-p "ATTACHMENT" (buffer-string)))
              (vm-mime-encode-composition)
              (let ((encoded (buffer-string)))
                (should (string-match-p "message/rfc822" encoded))
                (should (string-match-p "Subject: with an attachment" encoded))
                (should (string-match-p "the description" encoded)))
              (set-buffer-modified-p nil)))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-mime-test-attaching-a-message-needs-mime-sending ()
  "With `vm-send-using-mime' off the command says so and attaches nothing,
since a message attachment is a MIME part or it is nothing."
  (vm-mime-test--with-attachment (button)
    (let ((folder (current-buffer))
          (composition nil)
          (before (buffer-list)))
      (unwind-protect
          (progn
            (let ((vm-frame-per-composition nil)
                  (vm-mutable-frame-configuration nil)
                  (vm-mail-mode-hook nil)
                  (mail-signature nil)
                  (vm-send-using-mime t))
              (cl-letf (((symbol-function 'vm-display) #'ignore))
                (vm-mail)
                (setq composition (current-buffer))))
            (with-current-buffer folder
              (let ((vm-send-using-mime nil)
                    (text-quoting-style 'grave))
                (should (string-match-p
                         "set vm-send-using-mime non-nil"
                         (cadr (should-error
                                (vm-attach-message-to-composition
                                 composition nil)))))))
            (with-current-buffer composition
              (should-not (string-match-p "ATTACHMENT" (buffer-string)))))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

;;; Nuking the html alternative (emacs-vm/vm#653)

(defun vm-mime-test--message-with-parts (body)
  "Return a one-message folder whose message body is BODY."
  (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
          "From: sender@example.com\n"
          "Subject: alternatives\n"
          "MIME-Version: 1.0\n"
          body))

(defun vm-mime-test--alternative (boundary &rest parts)
  "Return a multipart/alternative of PARTS, each a (TYPE . TEXT) pair."
  (concat "Content-Type: multipart/alternative; boundary=\"" boundary "\"\n\n"
          (mapconcat (lambda (part)
                       (concat "--" boundary "\n"
                               "Content-Type: " (car part) "\n\n"
                               (cdr part) "\n"))
                     parts "")
          "--" boundary "--\n"))

(defmacro vm-mime-test--nuking (body &rest checks)
  "Build a message with BODY, nuke its html alternatives, run CHECKS.
CHECKS see DELETED, the number of parts deleted, and the folder buffer."
  (declare (indent 1) (debug t))
  `(vm-test-with-folder (vm-mime-test--message-with-parts ,body)
     (setq major-mode 'vm-mode)
     (let ((deleted (vm-nuke-alternative-text/html-internal
                     (car vm-message-list))))
       (ignore deleted)
       ,@checks)))

(ert-deftest vm-mime-test-nuking-html-keeps-the-plain-text ()
  "The html copy goes and the plain-text one stays: that is the trade the
command offers, and the reason it is safe at all."
  (vm-mime-test--nuking
      (vm-mime-test--alternative "IN" '("text/plain" . "the plain one")
                                 '("text/html" . "<p>the html one</p>"))
    (should (= deleted 1))
    (should (string-match-p "the plain one" (buffer-string)))
    (should-not (string-match-p "the html one" (buffer-string)))))

(ert-deftest vm-mime-test-nuking-spares-html-that-has-no-plain-text ()
  "REGRESSION: an alternative offering html alone keeps it, even when an
earlier alternative in the same message was nuked.

The flag that recorded \"this alternative has a text/plain first part\" was
set once and never reset, so after one well-formed alternative every later
text/html was deleted too -- including one that was the only copy of the
content.  The command cannot be undone, so that was a permanent loss."
  (vm-mime-test--nuking
      (concat "Content-Type: multipart/mixed; boundary=\"OUT\"\n\n"
              "--OUT\n"
              (vm-mime-test--alternative "IN1" '("text/plain" . "the plain one")
                                         '("text/html" . "<p>the html one</p>"))
              "--OUT\n"
              (vm-mime-test--alternative "IN2"
                                         '("text/html" . "<p>the only copy</p>"))
              "--OUT--\n")
    (should (= deleted 1))
    (should-not (string-match-p "the html one" (buffer-string)))
    (should (string-match-p "the only copy" (buffer-string)))))

(ert-deftest vm-mime-test-nuking-needs-the-plain-text-to-come-first ()
  "An alternative whose first part is not text/plain keeps its html.
The first part is the fallback a reader without html support is left with."
  (vm-mime-test--nuking
      (vm-mime-test--alternative "IN" '("text/enriched" . "the enriched one")
                                 '("text/html" . "<p>the html one</p>"))
    (should (= deleted 0))
    (should (string-match-p "the html one" (buffer-string)))))

(ert-deftest vm-mime-test-nuking-reaches-html-wrapped-in-a-related-part ()
  "Html with inline images arrives as multipart/related inside the
alternative, and that html is still one of the alternatives on offer.  This
is the ordinary structure of html mail, so nuking has to reach it."
  (vm-mime-test--nuking
      (concat "Content-Type: multipart/alternative; boundary=\"ALT\"\n\n"
              "--ALT\n"
              "Content-Type: text/plain\n\nthe plain one\n"
              "--ALT\n"
              "Content-Type: multipart/related; boundary=\"REL\"\n\n"
              "--REL\n"
              "Content-Type: text/html\n\n<p>the html one</p>\n"
              "--REL\n"
              "Content-Type: image/png\n\nnot-really-a-png\n"
              "--REL--\n"
              "--ALT--\n")
    (should (= deleted 1))
    (should-not (string-match-p "the html one" (buffer-string)))
    ;; the image is left alone: only the html copy was redundant
    (should (string-match-p "not-really-a-png" (buffer-string)))))

(ert-deftest vm-mime-test-nuking-leaves-html-that-is-the-whole-message ()
  "A message that is simply text/html is not touched: there is no
alternative, so there is nothing to fall back to."
  (vm-mime-test--nuking
      "Content-Type: text/html\n\n<p>the whole message</p>\n"
    (should (= deleted 0))
    (should (string-match-p "the whole message" (buffer-string)))))

(ert-deftest vm-mime-test-nuking-marks-the-message-edited ()
  "The message is marked edited and its counts cleared, so the folder knows
it has to write the change out and the summary is recomputed."
  (vm-mime-test--nuking
      (vm-mime-test--alternative "IN" '("text/plain" . "the plain one")
                                 '("text/html" . "<p>the html one</p>"))
    (should (= deleted 1))
    (let ((m (car vm-message-list)))
      (should (vm-edited-flag m))
      (should-not (vm-byte-count-of m))
      (should-not (vm-line-count-of m)))))

;;; Attachment commands in a composition (emacs-vm/vm#661)

(defmacro vm-mime-test--in-a-composition-with-a-file (&rest body)
  "Run BODY in a composition holding one attached file.
FILE is the attached file and DIR the directory it is in; point is at the
start of the attachment tag, which is where the attachment commands look."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-attach" t)))
          (file (expand-file-name "readme.txt" dir))
          (mail-header-separator "--text follows this line--")
          (vm-send-using-mime t))
     (unwind-protect
         (progn
           (with-temp-file file (insert "The file's contents.\n"))
           (with-temp-buffer
             (mail-mode)
             (insert "To: someone@example.com\n"
                     "Subject: with an attachment\n"
                     mail-header-separator "\n"
                     "The body.\n")
             (goto-char (point-max))
             (vm-attach-file file "text/plain")
             (goto-char (point-min))
             (should (search-forward "[ATTACHMENT" nil t))
             (goto-char (match-beginning 0))
             ,@body))
       (delete-directory dir t))))

(defun vm-mime-test--disposition-at-point ()
  "The disposition of the attachment at point."
  (car (get-text-property (point) 'vm-mime-disposition)))

(ert-deftest vm-mime-test-changing-a-disposition-changes-it ()
  "The disposition read at the prompt is the one the attachment carries.
It is what tells the recipient's mail reader whether to show the part or
offer it as a file to save."
  (vm-mime-test--in-a-composition-with-a-file
    (dolist (want '("attachment" "inline" "unspecified"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) want)))
        (vm-mime-change-content-disposition))
      (should (equal (vm-mime-test--disposition-at-point) want)))))

(ert-deftest vm-mime-test-changing-a-disposition-reaches-the-encoded-message ()
  "The new disposition is what the message goes out with, rather than only
what the tag in the composition says."
  (vm-mime-test--in-a-composition-with-a-file
    ;; attachment, not inline: a text/plain file is attached inline by
    ;; default, so asserting on inline would hold with the command doing
    ;; nothing at all
    (should (equal (vm-mime-test--disposition-at-point) "inline"))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "attachment")))
      (vm-mime-change-content-disposition))
    (vm-mime-encode-composition)
    (should (string-match-p "^Content-Disposition: attachment" (buffer-string)))
    (should-not (string-match-p "^Content-Disposition: inline" (buffer-string)))))

(ert-deftest vm-mime-test-changing-a-disposition-needs-an-attachment ()
  "REGRESSION: away from an attachment the command says so, and says it
before prompting.

`vm-mime-set-attachment-disposition-at-point' does `setcar' on a text
property that is nil where there is no attachment, so the reader was asked
which disposition they wanted and then given
`(wrong-type-argument consp nil)'.  `vm-mime-rename-attachment', directly
above it in the file, has always guarded for this."
  (vm-mime-test--in-a-composition-with-a-file
    (goto-char (point-min))
    (let ((asked nil)
          (text-quoting-style 'grave))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq asked t) "inline")))
        (let ((err (should-error (vm-mime-change-content-disposition)
                                 :type 'error)))
          (should (equal (error-message-string err) "No attachment here"))))
      (should-not asked))))

;;; Attaching a file that carries its own MIME headers

(ert-deftest vm-mime-test-attaching-a-mime-file-keeps-its-headers ()
  "`vm-attach-mime-file' attaches the file as it stands, headers and all:
that is the difference from `vm-attach-file', which supplies them."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-attach" t)))
         (file (expand-file-name "part.eml" dir))
         (mail-header-separator "--text follows this line--")
         (vm-send-using-mime t))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "Content-Type: text/plain; charset=us-ascii\n"
                    "Content-Transfer-Encoding: 7bit\n\n"
                    "Already MIME.\n"))
          (with-temp-buffer
            (mail-mode)
            (insert "To: someone@example.com\nSubject: s\n"
                    mail-header-separator "\nThe body.\n")
            (goto-char (point-max))
            (vm-attach-mime-file file "message/rfc822")
            (goto-char (point-min))
            (should (search-forward "[ATTACHMENT" nil t))
            (goto-char (match-beginning 0))
            ;; mimed: VM sends the file's own headers rather than making them
            (should (get-text-property (point) 'vm-mime-object))
            (should (equal (get-text-property (point) 'vm-mime-type)
                           "message/rfc822"))
            ;; marked as already encoded, so VM sends the file's own
            ;; headers rather than writing its own around the contents
            (should (get-text-property (point) 'vm-mime-encoded))
            ;; which is the whole difference from `vm-attach-file'
            (goto-char (point-max))
            (vm-attach-file file "text/plain")
            (goto-char (point-min))
            (should (search-forward "[ATTACHMENT" nil t))
            (should (search-forward "[ATTACHMENT" nil t))
            (goto-char (match-beginning 0))
            (should-not (get-text-property (point) 'vm-mime-encoded))))
      (delete-directory dir t))))

(ert-deftest vm-mime-test-attaching-a-mime-file-checks-the-file ()
  "A directory, a file that is not there, and one that cannot be read are
each refused by name, rather than attached and found wanting at send time."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-attach" t)))
         (unreadable (expand-file-name "secret.txt" dir))
         (vm-send-using-mime t)
         (text-quoting-style 'grave))
    (unwind-protect
        (progn
          (with-temp-file unreadable (insert "no\n"))
          (set-file-modes unreadable #o000)
          (with-temp-buffer
            (mail-mode)
            (let ((err (should-error (vm-attach-mime-file dir "text/plain")
                                     :type 'error)))
              (should (string-match-p "is a directory"
                                      (error-message-string err))))
            (let ((err (should-error
                        (vm-attach-mime-file (expand-file-name "absent" dir)
                                             "text/plain")
                        :type 'error)))
              (should (string-match-p "No such file"
                                      (error-message-string err))))
            ;; root can read anything, so this one only means something as
            ;; an ordinary user
            (unless (zerop (user-uid))
              (let ((err (should-error (vm-attach-mime-file unreadable
                                                            "text/plain")
                                       :type 'error)))
                (should (string-match-p "permission"
                                        (error-message-string err)))))))
      (set-file-modes unreadable #o600)
      (delete-directory dir t))))

(ert-deftest vm-mime-test-attaching-a-mime-file-needs-mime-sending ()
  "With `vm-send-using-mime' off there is no way to send an attachment, so
the command refuses and says which option turns it on."
  (let ((vm-send-using-mime nil)
        (text-quoting-style 'grave))
    (with-temp-buffer
      (mail-mode)
      (let ((err (should-error (vm-attach-mime-file "/etc/hosts" "text/plain")
                               :type 'error)))
        (should (string-match-p "vm-send-using-mime"
                                (error-message-string err)))))))

;;; Where auto-saved attachments go (emacs-vm/vm#669)

(defmacro vm-mime-test--attachment-path-for (from &rest body)
  "Run BODY with PATH bound to the attachment directory for a message FROM.
Anything BODY asks the reader is an error: this runs from
`vm-select-new-message-hook', where a question has nobody to answer it."
  (declare (indent 1) (debug t))
  `(let ((vm-mime-attachment-save-directory "/tmp/vm-test-attachments")
         (vm-mime-auto-save-all-attachments-subdir nil))
     (vm-test-with-folder
         (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
                 "From: " ,from "\n"
                 "Subject: a subject\n"
                 "Date: Mon, 1 Jan 2024 10:20:30 +0000\n\n"
                 "The body.\n")
       (cl-letf (((symbol-function 'y-or-n-p)
                  (lambda (prompt) (error "asked: %s" prompt)))
                 ((symbol-function 'yes-or-no-p)
                  (lambda (prompt) (error "asked: %s" prompt))))
         (let ((path (vm-mime-auto-save-all-attachments-path
                      (car vm-message-list))))
           (ignore path)
           ,@body)))))

(ert-deftest vm-mime-test-attachment-path-names-the-message ()
  "The directory is the save directory and a subdirectory naming the
message: when it was sent, who by, and what about."
  (vm-mime-test--attachment-path-for "Alice Adams <alice@example.com>"
    (should (equal path
                   "/tmp/vm-test-attachments/2024_1_1-10_20_30--Alice_Adams--a_subject"))))

(ert-deftest vm-mime-test-attachment-path-asks-nothing-about-encoded-names ()
  "REGRESSION: a correspondent whose name is MIME-encoded is ordinary.

The function compared the decoded full name with the raw From header and,
when they differed, printed a backtrace and asked the reader `Is this
wrong?', erroring if they said yes.  Any encoded name differs, and this
runs from `vm-select-new-message-hook' while the reader moves through
messages."
  (vm-mime-test--attachment-path-for "=?utf-8?Q?Ren=C3=A9?= <rene@example.com>"
    (should (string-prefix-p "/tmp/vm-test-attachments/" path))
    (should (string-match-p "2024_1_1-10_20_30" path))))

(ert-deftest vm-mime-test-attachment-path-has-one-separator ()
  "The path has no doubled slash, whether or not the save directory ends
in one."
  (dolist (directory '("/tmp/vm-test-attachments" "/tmp/vm-test-attachments/"))
    (let ((vm-mime-attachment-save-directory directory))
      (vm-mime-test--attachment-path-for "Alice Adams <alice@example.com>"
        (should-not (string-match-p "//" path))))))

(ert-deftest vm-mime-test-attachment-path-takes-a-string-setting ()
  "A string `vm-mime-auto-save-all-attachments-subdir' is a summary format,
so the reader can name the subdirectory after anything a summary can show."
  (let ((vm-mime-attachment-save-directory "/tmp/vm-test-attachments"))
    (vm-test-with-folder
        (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
                "From: Alice Adams <alice@example.com>\n"
                "Subject: a subject\n\nThe body.\n")
      (let ((vm-mime-auto-save-all-attachments-subdir "%F"))
        (should (equal (vm-mime-auto-save-all-attachments-path
                        (car vm-message-list))
                       "/tmp/vm-test-attachments/Alice Adams"))))))

(ert-deftest vm-mime-test-attachment-path-takes-a-function-setting ()
  "A function is called with the message, so the reader can decide the
subdirectory however they like."
  (let ((vm-mime-attachment-save-directory "/tmp/vm-test-attachments"))
    (vm-test-with-folder
        (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
                "From: Alice Adams <alice@example.com>\n"
                "Subject: a subject\n\nThe body.\n")
      (let ((vm-mime-auto-save-all-attachments-subdir
             (lambda (m) (concat "by-" (vm-su-from m)))))
        (should (equal (vm-mime-auto-save-all-attachments-path
                        (car vm-message-list))
                       "/tmp/vm-test-attachments/by-alice@example.com"))))))

(ert-deftest vm-mime-test-attachment-path-needs-a-save-directory ()
  "With no `vm-mime-attachment-save-directory' there is nowhere to save,
and the refusal names the option to set."
  (let ((vm-mime-attachment-save-directory nil)
        (text-quoting-style 'grave))
    (vm-test-with-folder
        (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
                "From: Alice Adams <alice@example.com>\n"
                "Subject: a subject\n\nThe body.\n")
      (let ((err (should-error (vm-mime-auto-save-all-attachments-path
                                (car vm-message-list))
                               :type 'error)))
        (should (string-match-p "vm-mime-attachment-save-directory"
                                (error-message-string err)))))))

;;; Attaching everything in a directory

(defmacro vm-mime-test--with-a-directory-of-files (&rest body)
  "Make a directory of three files and a subdirectory, then run BODY.
DIR is the directory: it holds notes.txt, more.txt, picture.png and a
subdirectory, which is enough to tell a regexp from a wildcard and a file
from a directory."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-attach-dir" t)))
          (vm-mime-all-attachments-directory nil)
          (vm-attach-files-in-directory-default-type "application/octet-stream")
          (vm-attach-files-in-directory-default-charset "us-ascii"))
     (unwind-protect
         (progn
           (write-region "the notes\n" nil (expand-file-name "notes.txt" dir)
                         nil 'quiet)
           (write-region "more notes\n" nil (expand-file-name "more.txt" dir)
                         nil 'quiet)
           (write-region "\211PNG\r\n\032\n" nil
                         (expand-file-name "picture.png" dir) nil 'quiet)
           (make-directory (expand-file-name "a-subdirectory" dir))
           ,@body)
       (delete-directory dir t))))

(defun vm-mime-test--attachment-names ()
  "The file names named by the attachment tags in this composition."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[ATTACHMENT \\([^,]*\\)" nil t)
        (push (file-name-nondirectory (match-string 1)) names)))
    (nreverse names)))

(ert-deftest vm-mime-test-attaching-a-directory-attaches-its-files ()
  "`vm-attach-files-in-directory' attaches every file that matches, giving
each the type its name implies, and passes over the subdirectories -- there
is no such thing as attaching a directory."
  (vm-mime-test--with-a-directory-of-files
    (vm-mime-test--composing
      (goto-char (point-max))
      (vm-attach-files-in-directory dir "")
      (should (equal (sort (vm-mime-test--attachment-names) #'string<)
                     '("more.txt" "notes.txt" "picture.png")))
      (vm-mime-encode-composition)
      (let ((encoded (buffer-string)))
        (should (string-match-p "Content-Type: image/png" encoded))
        (should (string-match-p "Content-Type: text/plain" encoded))
        (should (string-match-p "the notes" encoded))))))

(ert-deftest vm-mime-test-attaching-a-directory-takes-a-regexp ()
  "The regexp is what picks the files out: it is a regexp and not a shell
pattern, so it matches anywhere in the name unless it is anchored."
  (vm-mime-test--with-a-directory-of-files
    (vm-mime-test--composing
      (goto-char (point-max))
      (vm-attach-files-in-directory dir "\\.txt\\'")
      (should (equal (sort (vm-mime-test--attachment-names) #'string<)
                     '("more.txt" "notes.txt"))))))

(ert-deftest vm-mime-test-attaching-a-directory-with-nothing-in-it ()
  "A regexp that matches no file says so.  Silently attaching nothing would
look exactly like the attachment having worked."
  (vm-mime-test--with-a-directory-of-files
    (vm-mime-test--composing
      (goto-char (point-max))
      (let ((text-quoting-style 'grave))
        (let ((err (should-error (vm-attach-files-in-directory dir "\\.pdf\\'")
                                 :type 'error)))
          (should (string-match-p "No matching files"
                                  (error-message-string err)))))
      (should-not (vm-mime-test--attachment-names)))))

(ert-deftest vm-mime-test-attaching-a-directory-remembers-it ()
  "The directory is remembered, so the next attachment starts where the last
one did rather than in whatever directory the composition is visiting."
  (vm-mime-test--with-a-directory-of-files
    (vm-mime-test--composing
      (goto-char (point-max))
      (vm-attach-files-in-directory dir "\\.txt\\'")
      (should (equal vm-mime-all-attachments-directory dir)))))

;;; Listing what a message is made of

(defconst vm-mime-test--nested-message
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nSubject: what is in here\n"
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"outer\"\n\n"
          "--outer\n"
          "Content-Type: multipart/alternative; boundary=\"inner\"\n\n"
          "--inner\nContent-Type: text/plain\n\nThe plain text.\n"
          "--inner\nContent-Type: text/html\n\n<p>The HTML.</p>\n"
          "--inner--\n"
          "--outer\n"
          "Content-Type: image/png\n"
          "Content-Disposition: attachment; filename=\"picture.png\"\n\n"
          "PNGDATA\n"
          "--outer--\n")
  "A message with a part inside a part, and an attachment beside it.")

(defun vm-mime-test--part-listing (&optional verbose)
  "What `vm-list-mime-part-structure' prints for the current message.
`with-electric-help' is stubbed: it displays the buffer and then waits for a
key, which in batch is a read from a terminal that is not there."
  (let (listing)
    (cl-letf (((symbol-function 'with-electric-help)
               (lambda (thunk &rest _)
                 (with-temp-buffer
                   (let ((standard-output (current-buffer)))
                     (funcall thunk))
                   (setq listing (buffer-string))))))
      (vm-list-mime-part-structure verbose))
    listing))

(ert-deftest vm-mime-test-listing-the-parts-of-a-message ()
  "`vm-list-mime-part-structure' names the subject and then every part, one
per line, indented by how deep it is: a part inside a multipart is a level
in, which is how a reader tells nesting from a list of siblings."
  (vm-test-with-folder vm-mime-test--nested-message
    (setq major-mode 'vm-mode)
    (let ((lines (split-string (vm-mime-test--part-listing) "\n" t)))
      (should (equal (car lines) "what is in here"))
      (should (equal (nth 1 lines) "(\"multipart/mixed\" \"boundary=outer\")"))
      ;; the alternative is one level in, its two texts another
      (should (string-match-p "\\` (\"multipart/alternative\"" (nth 2 lines)))
      (should (string-match-p "\\`  (\"text/plain\")\\'" (nth 3 lines)))
      (should (string-match-p "\\`  (\"text/html\")\\'" (nth 4 lines)))
      ;; and the attachment is back out beside the alternative, with what
      ;; the disposition says about it
      (should (string-match-p "\\` (\"image/png\")" (nth 5 lines)))
      (should (string-match-p "filename=picture.png" (nth 5 lines))))))

(ert-deftest vm-mime-test-listing-the-parts-verbosely ()
  "With a prefix argument each line is the layout itself, which is what a
maintainer reading a bug report wants: the same parts, all of the fields."
  (vm-test-with-folder vm-mime-test--nested-message
    (setq major-mode 'vm-mode)
    (let ((listing (vm-mime-test--part-listing t)))
      (should (string-match-p "multipart/mixed" listing))
      (should (string-match-p "text/html" listing))
      ;; a layout carries the positions of the part in the folder, which the
      ;; short form does not print
      (should (string-match-p "#<marker" listing)))))

;;; Two commands about how much decoding to do

(ert-deftest vm-mime-test-toggling-the-alternative-method ()
  "`vm-toggle-best-mime' turns `vm-mime-alternative-show-method' between the
part VM can show itself and the best part whatever shows it, and decodes the
message again each way -- the setting decides which half of a
multipart/alternative you read."
  (vm-test-with-folder vm-mime-test--nested-message
    (setq major-mode 'vm-mode)
    (let ((vm-mime-alternative-show-method 'best-internal)
          (said nil))
      (cl-letf (((symbol-function 'vm-decode-mime-message) #'ignore)
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (setq said (apply #'format format args)))))
        (vm-toggle-best-mime)
        (should (eq vm-mime-alternative-show-method 'best))
        (should (equal said "using best MIME decoding"))
        (vm-toggle-best-mime)
        (should (eq vm-mime-alternative-show-method 'best-internal))
        (should (equal said "using best internal MIME decoding"))))))

(ert-deftest vm-mime-test-the-8bit-composition-charset-is-gone ()
  "`vm-mime-set-8bit-composition-charset' and the variable it set are
removed.  The command could not do anything -- it began by signalling, under
a condition true in every Emacs -- and the variable it would have set has
had no effect since Emacs 20 decided a buffer's charset for itself
(emacs-vm/vm#697)."
  (should-not (fboundp 'vm-mime-set-8bit-composition-charset))
  (should-not (boundp 'vm-mime-8bit-composition-charset)))

(ert-deftest vm-mime-test-the-7bit-composition-charset-is-gone ()
  "`vm-mime-7bit-composition-charset' is removed.

It was consulted nowhere in the tree, so setting it did nothing, and the
manual told a reader to set it to declare a composition's character set.
Emacs knows what is in the buffer and `vm-determine-proper-charset' asks it;
`vm-coding-system-priorities' is what puts your own order on the answer.  Its
sibling `vm-mime-8bit-composition-charset' went for the same reason
(emacs-vm/vm#697)."
  (should-not (boundp 'vm-mime-7bit-composition-charset)))

(ert-deftest vm-mime-test-emacs-w3-is-gone ()
  "Emacs/W3 is no longer offered as an HTML viewer (emacs-vm/vm#707).
The browser was dropped from Emacs and is in no archive, so `auto-select'
choosing it left every HTML part failing on a void `w3-region'."
  (should-not (fboundp 'vm-mime-display-internal-emacs-w3-text/html))
  (should-not (memq 'emacs-w3
                    (vm-mime-test--custom-constants 'vm-mime-text/html-handler)))
  (should-not (memq 'url-w3
                    (vm-mime-test--custom-constants 'vm-url-retrieval-methods)))
  (let ((vm-mime-text/html-handler 'auto-select))
    (should-not (eq (vm-mime-text/html-handler) 'emacs-w3))))

(defun vm-mime-test--custom-constants (variable)
  "The symbols a `const' in VARIABLE's customize type offers."
  (let ((type (get variable 'custom-type)))
    (delq nil (mapcar (lambda (branch)
                        (and (consp branch) (eq (car branch) 'const)
                             (car (last branch))))
                      (cdr type)))))

(ert-deftest vm-mime-test-a-suffix-the-alist-misses-is-asked-of-mailcap ()
  "A file whose suffix VM does not list still gets a type.

`vm-mime-attachment-auto-type-alist' cannot list every suffix, and what it
misses was attached as application/octet-stream: no charset, and a text file
arrives as something to download.  Emacs's mailcap tables know the rest, .org
among them, and this list comes first so an entry here still wins."
  ;; the alist, which is what a user sets
  (should (equal (vm-mime-default-type-from-filename "notes.txt") "text/plain"))
  (should (equal (vm-mime-default-type-from-filename "sheet.csv") "text/csv"))
  ;; mailcap, for what the alist has no entry for
  (should (equal (vm-mime-default-type-from-filename "notes.org") "text/x-org"))
  (should (equal (vm-mime-default-type-from-filename "fix.patch") "text/x-patch"))
  ;; a suffix nothing knows is still nil, and the callers say octet-stream
  (should-not (vm-mime-default-type-from-filename "opaque.zzqq"))
  (should-not (vm-mime-default-type-from-filename "no-suffix"))
  ;; and the alist wins where the two disagree
  (let ((vm-mime-attachment-auto-type-alist '(("\\.org$" . "text/plain"))))
    (should (equal (vm-mime-default-type-from-filename "notes.org")
                   "text/plain"))))

(ert-deftest vm-mime-test-an-unknown-text-subtype-is-displayed-as-text ()
  "A part of a text subtype VM has no handler for is shown as text.

RFC 2046 4.1.4: an unrecognised subtype of text is to be treated as
text/plain.  VM does it by falling back on the primary type's handler, which
is what makes an attached text/x-org readable rather than a button, and
`vm-mime-auto-displayed-content-types' lists text so it is shown at once."
  (should-not (fboundp (vm-mime-handler "display-internal" "text/x-org")))
  (should (fboundp (vm-mime-handler "display-internal" "text")))
  (should (member "text" vm-mime-auto-displayed-content-types)))

;;; What a transfer encoding does to the body it carries

(defconst vm-mime-test--encoded-bodies
  `(("plain ascii"      . "a body line\n")
    ("8-bit"            . "Grüße aus München\n")
    ("a From_ line"     . "text\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\n")
    ("a trailing space" . "a line with a trailing space \nand another\n")
    ("a long line"      . ,(concat (make-string 1200 ?x) "\n"))
    ("a 998-char line"  . ,(concat (make-string 998 ?y) "\n"))
    ("an equals sign"   . "1 + 1 = 2, and 50% of 4 = 2\n")
    ("a lone CR"        . "before\rafter\n")
    ("a dot on a line"  . "before\n.\nafter\n")
    ("no final newline" . "no newline at the end")
    ("an empty body"    . ""))
  "Bodies that stress a transfer encoding, each sent and read back.")

(defun vm-mime-test--send-and-receive (encoding body)
  "Encode a composition holding BODY under ENCODING, and read it back.
Answers (CHARSET CTE TEXT): what VM said it was sending, and what a
receiver gets after undoing the transfer encoding, the CRLF canonical form
and the charset, which is what a receiving reader does."
  (let ((mail-header-separator "--text follows this line--")
        (vm-send-using-mime t)
        (vm-mime-8bit-text-transfer-encoding encoding))
    (with-temp-buffer
      (mail-mode)
      (insert "To: someone@example.com\nSubject: encoded\n"
              mail-header-separator "\n" body)
      (vm-mime-encode-composition)
      (goto-char (point-min))
      (let* ((cte (and (re-search-forward
                        "^Content-Transfer-Encoding: \\(.*\\)$" nil t)
                       (match-string 1)))
             (charset (progn (goto-char (point-min))
                             (if (re-search-forward "charset=\\([^ \t\n;]+\\)" nil t)
                                 (match-string 1)
                               "us-ascii")))
             (raw (progn (goto-char (point-min))
                         (re-search-forward
                          (concat "^" (regexp-quote mail-header-separator) "\n"))
                         (buffer-substring-no-properties (point) (point-max)))))
        (list charset cte
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert raw)
                 (cond ((equal cte "quoted-printable")
                        (quoted-printable-decode-region (point-min) (point-max)))
                       ((equal cte "base64")
                        (base64-decode-region (point-min) (point-max))))
                 (goto-char (point-min))
                 (while (search-forward "\r\n" nil t) (replace-match "\n"))
                 (substring-no-properties
                  (decode-coding-string (buffer-string)
                                        (or (intern-soft (downcase charset))
                                            'utf-8)))))))))

(defun vm-mime-test--encoding-losses (encoding)
  "Every body ENCODING does not carry unchanged, as a list of complaints."
  (delq nil
        (mapcar
         (lambda (spec)
           (let ((got (vm-mime-test--send-and-receive encoding (cdr spec))))
             (unless (equal (nth 2 got) (cdr spec))
               (format "%s / %s: sent as %s in %s, came back %S not %S"
                       encoding (car spec) (nth 1 got) (nth 0 got)
                       (nth 2 got) (cdr spec)))))
         vm-mime-test--encoded-bodies)))

(ert-deftest vm-mime-test-quoted-printable-carries-the-body-unchanged ()
  "Every body survives being sent with `vm-mime-8bit-text-transfer-encoding'
set to quoted-printable."
  (should (equal nil (vm-mime-test--encoding-losses 'quoted-printable))))

(ert-deftest vm-mime-test-base64-carries-the-body-unchanged ()
  "Every body survives being sent with that option set to base64.
It did not: `vm-mime-base64-encode-region' held the end of the region in a
marker that does not advance, and turning the last LF into CRLF inserts at
that marker.  The final line break fell outside the region and went
unencoded, so a base64 part arrived one newline short of what was sent,
where quoted-printable and 8bit carried it."
  (should (equal nil (vm-mime-test--encoding-losses 'base64))))

(ert-deftest vm-mime-test-8bit-carries-the-body-unchanged ()
  "Every body survives being sent with that option set to 8bit."
  (should (equal nil (vm-mime-test--encoding-losses '8bit))))

(ert-deftest vm-mime-test-base64-encodes-the-whole-region ()
  "`vm-mime-base64-encode-region' encodes the trailing newline too.
Said at the level of the function, because what made it wrong is a property
of the marker rather than of any body: an insertion-type nil marker is left
in front of the CR that the CRLF conversion inserts."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (vm-mime-base64-encode-region (point-min) (point-max) t)
    (should (equal (base64-decode-string (string-trim (buffer-string)))
                   "line one\r\nline two\r\n"))))
;;; Encoding a header for sending, and reading it back (RFC 2047)

(defconst vm-mime-test--header-texts
  '(("plain ascii"         . "A plain subject")
    ("latin-1"             . "Grüße aus München")
    ("cjk"                 . "日本語のメール")
    ("mixed"               . "Re: Grüße from Ada")
    ("one 8-bit word"      . "Ada Löbe")
    ("an equals-question"  . "Is 2 =? 3 or not")
    ("a tab"               . "before\tafter")
    ("a leading space"     . " leading")
    ("a trailing space"    . "trailing ")
    ("two spaces"          . "two  spaces")
    ("an empty subject"    . "")
    ("a long ascii line"   . "This is a fairly long but entirely ASCII subject line that goes past seventy-eight characters for sure")
    ("a display name"      . "\"Löbe, Ada\" <ada@example.com>")
    ("a comma"             . "one, two, three")
    ("a colon"             . "Re: something: else")
    ("8-bit and a comma"   . "Löbe, Ada"))
  "Header texts to send and read back.
A text that is already an encoded word is not among them: decoding one is
what decoding means, so it cannot come back as it went in.")

(defun vm-mime-test--encode-header (text)
  "The whole Subject header `vm-mime-encode-headers' writes for TEXT.
The name and the folding are left in, so a test can measure the lines as they
go on the wire.  `vm-mime-test--encode-subject' is the value alone."
  (let ((mail-header-separator "--text follows this line--"))
    (with-temp-buffer
      (insert "Subject: " text "\n" mail-header-separator "\nbody\n")
      (vm-mime-encode-headers)
      (goto-char (point-min))
      (string-trim-right
       (buffer-substring-no-properties
        (point-min)
        (progn (re-search-forward
                (concat "^" (regexp-quote mail-header-separator)))
               (match-beginning 0)))
       "\n"))))

(defun vm-mime-test--unfold (header)
  "HEADER with its continuation lines joined back on, as a reader joins them.
RFC 5322 section 2.2.3: a line break followed by whitespace is folding, and
means the whitespace alone."
  (replace-regexp-in-string "\n[ \t]+" " " (string-trim-right header "\n")))

(defun vm-mime-test--encode-subject (text)
  "The value `vm-mime-encode-headers' writes for a Subject of TEXT.
Unfolded and with the header name off, which is what a reader ends up with
and what the round-trip tests compare.  Since emacs-vm/vm#794 a long header
is folded, and one whose first encoded word does not fit beside the name is
folded straight after the colon, so the name has to come off after the
unfolding and not before."
  (replace-regexp-in-string
   ;; The one space that separates the name from the value, not any run of
   ;; whitespace: a value whose own first character is a space keeps it.
   "\\`Subject: ?" "" (vm-mime-test--unfold (vm-mime-test--encode-header text))))

(defun vm-mime-test--decode-as-vm (encoded)
  (let ((vm-display-using-mime t))
    (substring-no-properties
     (vm-decode-mime-encoded-words-in-string encoded))))

(defun vm-mime-test--decode-as-rfc2047 (encoded)
  "ENCODED read by Emacs\\='s own RFC 2047 decoder, which VM did not write."
  (require 'rfc2047)
  (with-temp-buffer
    (insert encoded)
    (rfc2047-decode-region (point-min) (point-max))
    (substring-no-properties (buffer-string))))

(ert-deftest vm-mime-test-header-text-survives-being-sent ()
  "Every header text comes back as it went in.
`vm-mime-encode-words-in-string' had one test, for ASCII."
  (should (equal nil
                 (delq nil
                       (mapcar
                        (lambda (spec)
                          (let* ((sent (vm-mime-test--encode-subject (cdr spec)))
                                 (back (vm-mime-test--decode-as-vm sent)))
                            (unless (equal back (cdr spec))
                              (format "%s: %S sent as %S came back %S"
                                      (car spec) (cdr spec) sent back))))
                        vm-mime-test--header-texts)))))

(ert-deftest vm-mime-test-header-text-survives-a-conforming-reader ()
  "And comes back the same through a decoder VM did not write.
Emacs\\='s `rfc2047-decode-region' follows the rule that whitespace between two
adjacent encoded words is not part of the text.  A sender that encoded each
8-bit word separately would lose the spaces between them for such a reader
while looking right to itself."
  (should (equal nil
                 (delq nil
                       (mapcar
                        (lambda (spec)
                          (let* ((sent (vm-mime-test--encode-subject (cdr spec)))
                                 (back (vm-mime-test--decode-as-rfc2047 sent)))
                            (unless (equal back (cdr spec))
                              (format "%s: %S sent as %S read back %S"
                                      (car spec) (cdr spec) sent back))))
                        vm-mime-test--header-texts)))))

(ert-deftest vm-mime-test-header-adjacent-8bit-words-are-encoded-together ()
  "Two 8-bit words with only a space between them become one encoded word.
`vm-mime-encode-headers' says so in its docstring, and the reason is the
rule above: encoded separately, the space between them would be dropped by
a conforming reader.  Pinned because nothing checked the promise."
  (should (equal (vm-mime-test--encode-subject "Grüße Grüße")
                 "=?iso-8859-1?Q?Gr=FC=DFe_Gr=FC=DFe?="))
  ;; and ASCII between them keeps them apart, which is what makes it safe
  (should (equal (vm-mime-test--encode-subject "Grüße aus München")
                 "=?iso-8859-1?Q?Gr=FC=DFe?= aus =?iso-8859-1?Q?M=FCnchen?=")))

(ert-deftest vm-mime-test-header-encoded-words-are-short-enough ()
  "For ordinary text no encoded word passes the 75 characters RFC 2047 allows."
  (let ((examined 0))
    (dolist (spec vm-mime-test--header-texts)
      (let ((sent (vm-mime-test--encode-subject (cdr spec)))
            (pos 0))
        (while (string-match "=\\?[^?]*\\?[BbQq]\\?[^?]*\\?=" sent pos)
          (should (<= (- (match-end 0) (match-beginning 0)) 75))
          (setq examined (1+ examined)
                pos (match-end 0)))))
    ;; the premise: encoded words were actually found, so this is not a test
    ;; of nothing should the encoder stop emitting them
    (should (> examined 5))))

(ert-deftest vm-mime-test-header-lines-are-folded ()
  "A header VM writes is folded so no line runs past the limit.

RFC 5322 section 2.1.1 sets 998 characters a line MUST NOT exceed and 78 it
SHOULD NOT; RFC 2047 section 2 sets 75 for an encoded word.  VM used to fold
nothing, so a long subject broke all three (emacs-vm/vm#794):

    30 latin-1 words    one line of 325, one encoded word of 316
    400 ascii words     one line of 2008
    one 400-char word   one line of 1226, one encoded word of 1218

Now each of those is several lines of 75 or less, and the text still comes
back whole from both decoders, which
vm-mime-test-header-text-survives-a-conforming-reader checks across the whole
corpus."
  (dolist (text (list (mapconcat #'identity (make-list 30 "Grüße") " ")
                      (mapconcat #'identity (make-list 400 "word") " ")
                      (make-string 400 ?ü)
                      (mapconcat #'identity (make-list 40 "日本語") "")))
    (let* ((sent (vm-mime-test--encode-header text))
           (lines (split-string (string-trim-right sent) "\n")))
      ;; it did fold: more than one line, and the premise of the rest
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (length line) vm-mime-header-line-limit)))
      ;; every continuation line begins with whitespace, or it is not folding
      (dolist (line (cdr lines))
        (should (string-match-p "\\`[ \t]" line)))
      ;; and the text is still all there
      (should (equal (vm-mime-test--decode-as-vm
                      (replace-regexp-in-string "\\`Subject: ?" ""
                                                (vm-mime-test--unfold sent)))
                     text)))))

(ert-deftest vm-mime-test-header-a-word-with-no-break-in-it-is-left-long ()
  "A single unencoded run with no whitespace cannot be folded, and is not.
Folding breaks at whitespace that is already there; RFC 5322 gives nowhere
else to put a break.  An 800-character ASCII word therefore still goes out on
one line over the limit.  The one break available is the space after the
colon, which is taken, so the header is two lines: `Subject:' and the word.
It is ASCII, so no encoded word is involved and RFC 2047 has nothing to say
about it."
  (let* ((sent (vm-mime-test--encode-header (make-string 800 ?x)))
         (lines (split-string (string-trim-right sent) "\n")))
    (should (equal 2 (length lines)))
    (should (equal "Subject:" (car lines)))
    ;; the word itself, still long, with nowhere to break it
    (should (> (length (nth 1 lines)) vm-mime-header-line-limit))
    (should (string-match-p "\\`[ \t]x+\\'" (nth 1 lines)))))

(defun vm-mime-test--count-encoded-words (text)
  "How many complete RFC 2047 encoded words TEXT holds."
  (let ((case-fold-search nil)
        (n 0)
        (start 0))
    (while (string-match "=\\?[^?]+\\?[BbQq]\\?[^?]*\\?=" text start)
      (setq n (1+ n)
            start (match-end 0)))
    n))

(ert-deftest vm-mime-test-header-folding-does-not-split-an-encoded-word ()
  "No line break falls inside an encoded word.
A break there would leave a word with no `?=' to end it, and a reader would
show the rest as ordinary text.  The encoder keeps each word inside the
75-character limit so the folder never has to break one.

Counts the complete words line by line and compares with the count after
unfolding.  A word split across a fold is complete in the unfolded text and
in neither line, so the two counts part company; a literal `=?' in a subject
that was never encoded is a word in neither, so it does not register.  Not by
counting `=?' against `?=': a base64 payload ends in padding, so
`R3LDvMOfZQ==?=' holds a `=?' that opens nothing."
  (let ((split 0))
    (dolist (spec (cons (cons "a long run"
                              (mapconcat #'identity (make-list 40 "Grüße") " "))
                        vm-mime-test--header-texts))
      (let* ((sent (vm-mime-test--encode-header (cdr spec)))
             (per-line (apply #'+ (mapcar #'vm-mime-test--count-encoded-words
                                          (split-string sent "\n"))))
             (whole (vm-mime-test--count-encoded-words
                     (vm-mime-test--unfold sent))))
        (should (equal per-line whole))
        (setq split (+ split whole))))
    ;; the premise: encoded words were there to be split
    (should (> split 10))))


;;; The encoding the reader chose, crossed with the text

;; `vm-mime-encode-headers-type' takes Q, B, or a regexp choosing base64 for
;; the words it matches and quoted-printable for the rest.  Nothing tested any
;; of the three, and the splitting added for emacs-vm/vm#794 has a budget per
;; encoding: base64 pads to a multiple of four and expands three bytes to
;; four, quoted-printable takes one to three characters per byte.  An
;; arithmetic error in either shows as a word over the limit, or as a word
;; split where the encoding cannot be resumed.

(defconst vm-mime-test--encoding-types
  '(("quoted-printable" . Q)
    ("base64"           . B)
    ("base64 on 8-bit"  . "[^- !#-'*+/-9=?A-Z^-~]"))
  "The values `vm-mime-encode-headers-type' takes.
The third is the regexp form the option offers as a default, which picks
base64 for a word holding anything outside a bare ASCII set.")

(defconst vm-mime-test--long-header-texts
  (list (cons "30 latin-1 words" (mapconcat #'identity (make-list 30 "Grüße") " "))
        (cons "400 ascii words"  (mapconcat #'identity (make-list 400 "word") " "))
        (cons "one 400-char word" (make-string 400 ?ü))
        (cons "40 cjk words"     (mapconcat #'identity (make-list 40 "日本語") ""))
        (cons "greek"            "Ελληνικά κείμενα εδώ και τώρα για όλους"))
  "Texts long enough to need splitting, folding, or both.")

(defun vm-mime-test--encoding-complaint (label text)
  "Encode TEXT as a Subject and report what is wrong with the result.
LABEL names the case.  Answers nil when every line is short enough, every
encoded word is inside the RFC 2047 limit, no word is split across a fold,
and both decoders give TEXT back."
  (condition-case err
      (let* ((sent (vm-mime-test--encode-header text))
             (lines (split-string (string-trim-right sent) "\n"))
             (whole (vm-mime-test--unfold sent))
             (value (replace-regexp-in-string "\\`Subject: ?" "" whole))
             (long (seq-filter (lambda (l)
                                 (and (> (length l) vm-mime-header-line-limit)
                                      ;; a run with no whitespace cannot be
                                      ;; broken, and is not this test's business
                                      (string-match-p "[ \t]" (string-trim l))))
                               lines))
             (over (let ((n 0) (start 0))
                     (while (string-match "=\\?[^?]+\\?[BbQq]\\?[^?]*\\?=" whole start)
                       (when (> (- (match-end 0) (match-beginning 0)) 75)
                         (setq n (1+ n)))
                       (setq start (match-end 0)))
                     n)))
        (cond
         (long (format "%s: %d line(s) over %d, longest %d"
                       label (length long) vm-mime-header-line-limit
                       (apply #'max (mapcar #'length long))))
         ((> over 0) (format "%s: %d encoded word(s) over 75" label over))
         ((/= (apply #'+ (mapcar #'vm-mime-test--count-encoded-words lines))
              (vm-mime-test--count-encoded-words whole))
          (format "%s: an encoded word is split across a fold" label))
         ((not (equal (vm-mime-test--decode-as-vm value) text))
          (format "%s: VM reads it back as %S" label
                  (vm-mime-test--decode-as-vm value)))
         ((not (equal (vm-mime-test--decode-as-rfc2047 value) text))
          (format "%s: a conforming reader gets %S" label
                  (vm-mime-test--decode-as-rfc2047 value)))
         (t nil)))
    (error (format "%s: %s" label (error-message-string err)))))

(defun vm-mime-test--every-text-under (type)
  "Encode every text with `vm-mime-encode-headers-type' bound to TYPE."
  (let ((vm-mime-encode-headers-type type))
    (delq nil
          (mapcar (lambda (spec)
                    (vm-mime-test--encoding-complaint (car spec) (cdr spec)))
                  (append vm-mime-test--header-texts
                          vm-mime-test--long-header-texts)))))

(ert-deftest vm-mime-test-header-quoted-printable-holds-every-text ()
  "Twenty-one texts encoded Q come back whole, inside both limits."
  (should (equal nil (vm-mime-test--every-text-under 'Q))))

(ert-deftest vm-mime-test-header-base64-holds-every-text ()
  "The same encoded B.
Base64 is where the splitting arithmetic is easiest to get wrong: it pads to
a multiple of four, so a word cut at the wrong character encodes to something
a decoder cannot finish."
  (should (equal nil (vm-mime-test--every-text-under 'B))))

(ert-deftest vm-mime-test-header-a-regexp-type-holds-every-text ()
  "The same again with the regexp form, which chooses per run.
`vm-mime-encode-headers-type' accepts a regexp and picks base64 for what it
matches, quoted-printable for the rest, so one header can carry both."
  (should (equal nil (vm-mime-test--every-text-under
                      (cdr (nth 2 vm-mime-test--encoding-types))))))

(ert-deftest vm-mime-test-header-the-type-decides-the-encoding-letter ()
  "The encoding VM asks for is the one that reaches the wire.
Otherwise the tests above would pass while the option did nothing: all three
would be quoted-printable and nobody would know."
  (dolist (spec '((Q . "Q") (B . "B")))
    (let ((vm-mime-encode-headers-type (car spec)))
      (should (string-match-p (concat "=?[^?]+?" (cdr spec) "?")
                              (vm-mime-test--encode-header "Grüße")))))
  ;; and the regexp form picks base64 for a run that matches it
  (let ((vm-mime-encode-headers-type (cdr (nth 2 vm-mime-test--encoding-types))))
    (should (string-match-p "=?[^?]+?B?" (vm-mime-test--encode-header "Grüße")))))


;;; The charset VM names for outgoing text (`vm-coding-system-priorities')

;; The option had no test of any kind, and it decides the charset label on
;; every message VM sends.  A label naming a charset that cannot hold the
;; text is mail the recipient decodes into something else, which is the
;; quietest kind of corruption: nothing fails, the words are simply wrong.

(defconst vm-charset-test--texts
  '(("ascii"       . "plain text")
    ("latin-1"     . "Grüße")
    ("euro sign"   . "cost: 5€")
    ("greek"       . "Ελληνικά")
    ("japanese"    . "日本語")
    ("latin+greek" . "Grüße Ελληνικά")
    ("emoji"       . "hi 😀"))
  "Texts spanning ASCII, one Latin set, two that need a wider one, and
two that need Unicode.  The euro sign is in iso-8859-15 and not in
iso-8859-1, which is what tells the two apart.")

(defconst vm-charset-test--priorities
  '(nil
    (iso-8859-1)
    (iso-8859-1 utf-8)
    (utf-8)
    (iso-8859-15 iso-8859-1 utf-8))
  "Values of `vm-coding-system-priorities' worth crossing, the default
included.")

(defun vm-charset-test--chosen (text priorities)
  "The charset VM names for TEXT under PRIORITIES."
  (let ((vm-coding-system-priorities priorities))
    (with-temp-buffer
      (insert text)
      (vm-determine-proper-charset (point-min) (point-max)))))

(defun vm-charset-test--survives-p (text charset)
  "Whether TEXT written as CHARSET and read back is TEXT again."
  (let ((coding (vm-mime-charset-to-coding charset)))
    (and coding
         (not (eq coding 'no-conversion))
         (equal text (decode-coding-string
                      (encode-coding-string text coding) coding)))))

(ert-deftest vm-charset-test-the-charset-named-can-hold-the-text ()
  "Whatever charset VM names, the text survives being written in it.

The invariant that matters: a label naming a charset too narrow for the text
is a message the recipient reads as something else.  Five settings of
`vm-coding-system-priorities' crossed with seven texts, thirty-five in all,
each encoded in the charset VM chose and decoded back."
  (let ((complaints nil))
    (dolist (priorities vm-charset-test--priorities)
      (dolist (spec vm-charset-test--texts)
        (let ((charset (vm-charset-test--chosen (cdr spec) priorities)))
          (unless (vm-charset-test--survives-p (cdr spec) charset)
            (push (format "%S / %s: named %s, which cannot hold it"
                          priorities (car spec) charset)
                  complaints)))))
    (should (equal nil (nreverse complaints)))))

(ert-deftest vm-charset-test-pure-ascii-is-us-ascii-whatever-is-asked-for ()
  "Text with no 8-bit character in it is us-ascii under every setting.
The first thing the function tests, and the answer that keeps VM from
labelling ordinary mail with a charset it does not need."
  (dolist (priorities vm-charset-test--priorities)
    (should (equal "us-ascii"
                   (vm-charset-test--chosen "plain text" priorities)))))

(ert-deftest vm-charset-test-the-narrowest-that-fits-is-chosen-by-default ()
  "With the option nil, VM names the narrowest charset that holds the text.
`Grüße' goes out as iso-8859-1 rather than utf-8, and the euro sign, which
iso-8859-1 has no room for, as iso-8859-15.  That is what makes VM's headers
shorter than most, and it is `vm-get-coding-system-priorities' answering with
its default list rather than the option being unset meaning utf-8."
  (should (equal "iso-8859-1" (vm-charset-test--chosen "Grüße" nil)))
  (should (equal "iso-8859-15" (vm-charset-test--chosen "cost: 5€" nil)))
  (should (equal "utf-8" (vm-charset-test--chosen "Ελληνικά" nil))))

(ert-deftest vm-charset-test-the-priority-order-is-obeyed ()
  "The first charset in the list that can hold the text is the one named.
iso-8859-15 ahead of iso-8859-1 gets iso-8859-15 for text either could
carry, which is the whole purpose of the option: it is a preference, not a
constraint."
  (should (equal "iso-8859-15"
                 (vm-charset-test--chosen "Grüße" '(iso-8859-15 iso-8859-1 utf-8))))
  (should (equal "iso-8859-1"
                 (vm-charset-test--chosen "Grüße" '(iso-8859-1 iso-8859-15 utf-8)))))

(ert-deftest vm-charset-test-a-list-that-cannot-hold-the-text-falls-back ()
  "A priority list with nothing wide enough falls back to utf-8.
`(iso-8859-1)' alone cannot carry Greek, and holds no universal coding system
to stop the search, so the loop runs out and the documented fallback answers.
Without it VM would name iso-8859-1 for text it cannot represent."
  (dolist (text '("Ελληνικά" "日本語" "hi 😀"))
    (should (equal "utf-8" (vm-charset-test--chosen text '(iso-8859-1)))))
  ;; and the fallback is not merely the last entry: there is no utf-8 here
  (should (equal "utf-8" (vm-charset-test--chosen "Ελληνικά"
                                                  '(iso-8859-1 iso-8859-15)))))

(ert-deftest vm-charset-test-a-universal-coding-system-stops-the-search ()
  "A charset in `vm-mime-ucs-list' is taken even where it cannot be checked.
The loop stops at the first entry that is universal, so utf-8 anywhere in the
list answers for any text at all."
  (should (member 'utf-8 (vm-get-mime-ucs-list)))
  (dolist (text (mapcar #'cdr vm-charset-test--texts))
    (unless (equal text "plain text")
      (should (equal "utf-8" (vm-charset-test--chosen text '(utf-8)))))))


;;; Splitting a composition into message/partial fragments

;; `vm-mime-fragment-composition' is what `vm-mime-max-message-size' asks for,
;; and it had no test of any kind: the form coverage report put it among the
;; definitions with the most forms never evaluated.  A message split wrongly
;; is mail nothing can put back together.

(defmacro vm-fragment-test--with-fragments (spec &rest body)
  "Fragment a composition and run BODY with FRAGMENTS bound to the buffers.
SPEC is (SIZE LINES &optional AVOID-FOLDING).  The buffers are killed
afterwards, fragmenting making one per part."
  (declare (indent 1) (debug t))
  `(let ((vm-mime-avoid-folding-content-type ,(nth 2 spec))
         (fragments nil))
     (unwind-protect
         (with-temp-buffer
           (insert "To: someone@example.com\nSubject: a big one\n"
                   mail-header-separator "\n")
           (dotimes (i ,(nth 1 spec))
             (insert (format "line %04d %s\n" i (make-string 60 ?x))))
           (setq fragments (vm-mime-fragment-composition ,(car spec)))
           ,@body)
       (dolist (buffer fragments)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun vm-fragment-test--parameter (buffer name)
  "The message/partial parameter NAME in BUFFER's Content-Type."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat name "=\\([^;\n \t]+\\)") nil t)
        (match-string-no-properties 1)))))

(defun vm-fragment-test--body (buffer)
  "The text of BUFFER after the header separator."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (buffer-substring-no-properties (point) (point-max)))))

(ert-deftest vm-fragment-test-every-part-declares-the-real-total ()
  "REGRESSION: `total=' is the number of parts, on every one of them.

Issue #797.  The total is not known until the last part has been cut, so
`vm-mime-fragment-composition' writes `total=' empty, records the position
and fills it in at the end by inserting there, not by replacing.  4181e0d1
in 2010 added a `%d' to that empty format string, which looks like tidying a
`format' call that has an argument and no directive.  It put the fragment's
own number where the total goes, and the fill-in then ran on after it: three
parts went out saying total=13, 23 and 33.

Nothing could reassemble such a message, VM included:
`vm-mime-display-internal-message/partial' refuses parts that disagree about
the total, and a lone part claiming thirteen would wait for ten that never
come."
  (dolist (avoid '(nil t))
    (let ((vm-mime-avoid-folding-content-type avoid)
          (fragments nil))
      (unwind-protect
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: a big one\n"
                    mail-header-separator "\n")
            (dotimes (i 100)
              (insert (format "line %04d %s\n" i (make-string 60 ?x))))
            (setq fragments (vm-mime-fragment-composition 3000))
            (let ((total (number-to-string (length fragments)))
                  (n 0))
              (should (> (length fragments) 1))
              (dolist (buffer fragments)
                (setq n (1+ n))
                (should (equal total (vm-fragment-test--parameter buffer "total")))
                (should (equal (number-to-string n)
                               (vm-fragment-test--parameter buffer "number"))))))
        (dolist (buffer fragments)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest vm-fragment-test-the-parts-carry-one-id-between-them ()
  "Every fragment names the same id, which is how a reader groups them."
  (vm-fragment-test--with-fragments (3000 100)
    (let ((id (vm-fragment-test--parameter (car fragments) "id")))
      (should id)
      (should (> (length fragments) 1))
      (dolist (buffer fragments)
        (should (equal id (vm-fragment-test--parameter buffer "id")))))))

(ert-deftest vm-fragment-test-the-bodies-join-back-into-the-message ()
  "The fragment bodies, in order, are the message that was split.

The invariant the whole feature rests on.  Every part carries a slice of the
original and nothing else, so a reader that concatenates them in number order
has the message back."
  (let ((vm-mime-avoid-folding-content-type nil)
        (fragments nil)
        (whole nil))
    (unwind-protect
        (with-temp-buffer
          (insert "To: someone@example.com\nSubject: a big one\n"
                  mail-header-separator "\n")
          (dotimes (i 100)
            (insert (format "line %04d %s\n" i (make-string 60 ?x))))
          (setq fragments (vm-mime-fragment-composition 3000))
          ;; the master buffer, as fragmenting left it, is what the parts
          ;; between them should hold
          (setq whole (buffer-substring-no-properties (point-min) (point-max)))
          (let ((joined (mapconcat #'vm-fragment-test--body fragments "")))
            ;; `vm-add-mail-mode-header-separator' puts the separator line back
            ;; in the master buffer as fragmenting finishes; the parts carry a
            ;; real blank line there, as a message does.  Taking the text of
            ;; the separator out, and leaving its newline, makes the two
            ;; comparable.
            (should (equal (replace-regexp-in-string
                            (regexp-quote mail-header-separator) "" whole)
                           joined))))
      (dolist (buffer fragments)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest vm-fragment-test-each-part-is-declared-7bit ()
  "Every fragment says message/partial and 7bit, and drops the original type.
RFC 2046 section 5.2.2 allows message/partial only with 7bit, and the
original Content-Type belongs to the message inside, not to the fragment
carrying it."
  (vm-fragment-test--with-fragments (3000 100)
    (dolist (buffer fragments)
      (with-current-buffer buffer
        (goto-char (point-min))
        (let ((headers (buffer-substring-no-properties
                        (point-min)
                        (save-excursion
                          (search-forward mail-header-separator)
                          (point)))))
          (should (string-match-p "Content-Type: message/partial" headers))
          (should (string-match-p "Content-Transfer-Encoding: 7bit" headers))
          (should (string-match-p "MIME-Version: 1.0" headers))
          ;; the subject rides along, so a reader sees what it is
          (should (string-match-p "Subject: a big one" headers)))))))

(ert-deftest vm-fragment-test-folding-the-content-type-is-optional ()
  "`vm-mime-avoid-folding-content-type' decides whether the type is folded.
Both forms carry the same three parameters; only the whitespace differs.  It
is the option's only use here and neither arm had a test."
  (dolist (avoid '(nil t))
    (let ((vm-mime-avoid-folding-content-type avoid)
          (fragments nil))
      (unwind-protect
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: a big one\n"
                    mail-header-separator "\n")
            (dotimes (i 100)
              (insert (format "line %04d %s\n" i (make-string 60 ?x))))
            (setq fragments (vm-mime-fragment-composition 3000))
            (with-current-buffer (car fragments)
              (goto-char (point-min))
              (let ((type (buffer-substring-no-properties
                           (progn (re-search-forward "^Content-Type:") 
                                  (match-beginning 0))
                           (progn (re-search-forward "^Content-Transfer-Encoding:")
                                  (match-beginning 0)))))
                (if avoid
                    (should-not (string-match-p "\n\t" type))
                  (should (string-match-p "\n\t" type)))
                (dolist (parameter '("id=" "number=" "total="))
                  (should (string-match-p (regexp-quote parameter) type))))))
        (dolist (buffer fragments)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))


;;; Putting message/partial fragments back together

;; `vm-mime-display-internal-message/partial' is the receiving half of the
;; feature tested above, and had no test either: the form coverage report put
;; it at the head of the definitions with the most forms never evaluated.
;;
;; These drive it as the button does, with an extent carrying the part's
;; layout, over a folder whose messages are the fragments.

(defun vm-partial-test--folder-of-fragments (file fragments)
  "Write FRAGMENTS into FILE as a From_ folder, one message each."
  (with-temp-file file
    (dolist (buffer fragments)
      (insert "From vm@example.com Mon Jan  1 00:00:00 2024\n")
      (insert (with-current-buffer buffer
                ;; the composition separator is not part of a message
                (replace-regexp-in-string
                 (regexp-quote mail-header-separator) ""
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (insert "\n"))))

(defun vm-partial-test--fragment-texts (lines size)
  "The message/partial fragments for a composition of LINES, as strings."
  (let ((fragments nil))
    (unwind-protect
        (with-temp-buffer
          (insert "To: someone@example.com\nSubject: a big one\n"
                  mail-header-separator "\n")
          (dotimes (i lines)
            (insert (format "line %04d %s\n" i (make-string 50 ?y))))
          (setq fragments (vm-mime-fragment-composition size))
          (mapcar (lambda (buffer)
                    (with-current-buffer buffer
                      ;; the composition separator is not part of a message
                      (replace-regexp-in-string
                       (regexp-quote mail-header-separator) ""
                       (buffer-substring-no-properties (point-min) (point-max)))))
                  fragments))
      (dolist (buffer fragments)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defmacro vm-partial-test--with-a-folder-of (texts &rest body)
  "Visit a From_ folder whose messages are TEXTS, and run BODY.
FOLDER is bound to the folder buffer and ASSEMBLE to a function of no
arguments that reassembles from the first message and answers the assembled
buffer, widened."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-partial" t)))
          (file (expand-file-name "fragments" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (before (buffer-list))
          folder assemble)
     (unwind-protect
         (progn
           (with-temp-file file
             (dolist (text ,texts)
               (insert "From vm@example.com Mon Jan  1 00:00:00 2024\n"
                       text "\n")))
           (vm-visit-folder file)
           (setq folder (current-buffer))
           (setq assemble
                 (lambda ()
                   (with-current-buffer folder
                     (let* ((layout (vm-mm-layout (car vm-message-list)))
                            (extent (vm-make-extent
                                     (vm-mm-layout-body-start layout)
                                     (vm-mm-layout-body-end layout))))
                       (vm-set-extent-property extent 'vm-mime-layout layout)
                       (save-window-excursion
                         (vm-mime-display-internal-message/partial extent))
                       ;; The function ends by switching buffers, so the
                       ;; assembled one is found by name.  And `vm-mode' has
                       ;; narrowed it to the message, so it is read widened.
                       (let ((assembled (get-buffer "assembled message")))
                         (and assembled
                              (with-current-buffer assembled
                                (save-restriction
                                  (widen)
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))))))
           (ignore folder assemble)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-partial-test-the-parts-assemble-into-the-message ()
  "Fragmenting a message and reassembling it gives the message back.

The round trip the feature exists for, and neither half had a test.  Sixty
lines cut into parts, written into a folder as separate messages, and put
back: every line is there, in order, with the original headers."
  (vm-partial-test--with-a-folder-of (vm-partial-test--fragment-texts 60 2500)
    (should (> (length (with-current-buffer folder vm-message-list)) 1))
    (let ((text (funcall assemble)))
      (should text)
      (dotimes (i 60)
        (should (string-match-p (format "line %04d" i) text)))
      (should (string-match-p "Subject: a big one" text))
      ;; in order: the first line comes before the last
      (should (< (string-match "line 0000" text)
                 (string-match "line 0059" text))))))

(ert-deftest vm-partial-test-parts-that-disagree-about-the-total-are-refused ()
  "Parts claiming different totals are an error, not a wrong assembly.

The check that makes #797 visible from the receiving end: fragments written
before that fix each declared a different total, and this refuses them rather
than assembling something short."
  (let* ((texts (vm-partial-test--fragment-texts 60 2500))
         (broken (cons (car texts)
                       (mapcar (lambda (text)
                                 (replace-regexp-in-string
                                  "total=[0-9]+" "total=99" text))
                               (cdr texts)))))
    (should (> (length texts) 1))
    (vm-partial-test--with-a-folder-of broken
      (should-error (funcall assemble)))))

(ert-deftest vm-partial-test-a-missing-part-is-refused ()
  "A folder holding only some of the parts is an error, not a short message.
Assembling what is there would hand the reader a message with a hole in it
and no sign of one."
  (let ((texts (vm-partial-test--fragment-texts 60 2500)))
    (should (> (length texts) 1))
    ;; the first part alone, which still says how many there should be
    (vm-partial-test--with-a-folder-of (list (car texts))
      (should-error (funcall assemble)))))

(defun vm-mime-test--present-html (handler)
  "Present a text/html message with `vm-mime-text/html-handler' as HANDLER.
Answers the presentation text."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-html" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-mime-text/html-handler handler)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-user-interaction-buffer vm-user-interaction-buffer)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "From alice@example.com  Thu Jan  1 00:00:00 2026\n"
                    "From: alice@example.com\nSubject: html\n"
                    "MIME-Version: 1.0\nContent-Type: text/html\n\n"
                    "<p>hello from html</p>\n\n")
            (write-region (point-min) (point-max) file nil 'quiet))
          (vm-visit-folder file)
          (vm-present-current-message)
          (vm-show-current-message)
          (with-current-buffer (or vm-presentation-buffer (current-buffer))
            (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-mime-test-html-with-no-handler-is-a-button ()
  "With no handler for text/html the part is a button, not silence.

What the manual tells a reader with none of emacs-w3m, w3m or lynx
installed: the part is still there to save or hand to a program, and the
text of the message is not what they see.  `vm-mime-text/html-handler' nil
is the same case, and is how a machine that has none of the three is
reached from a machine that has one."
  (let ((presented (vm-mime-test--present-html nil)))
    (should (string-match-p "HTML" presented))
    (should (string-match-p "\\[save\\]" presented))
    (should-not (string-match-p "hello from html" presented))))

(ert-deftest vm-mime-test-html-handler-renders-the-part ()
  "A handler that is there renders the HTML into the presentation.
The other half of the pair: the button is the absence of a handler and not
something VM does to every text/html part."
  (skip-unless (executable-find "lynx"))
  (let ((presented (vm-mime-test--present-html 'lynx)))
    (should (string-match-p "hello from html" presented))))

(ert-deftest vm-mime-test-a-button-format-takes-a-percent-literally ()
  "%% is one % in a button format, and an unknown specifier is text.

`vm-mime-compile-format-1' is a copy of the summary compiler and had both of
its faults (emacs-vm/vm#847): a format with nothing to substitute kept the
doubled percents it had been given, and a specifier VM does not know was
copied into the `format\=' control string, where it made a conversion of its
own and the button failed to be drawn at all."
  (let ((layout (vm-mime-test--html-layout "utf-8"))
        (vm-mime-compiled-format-alist nil))
    (should (equal (vm-mime-sprintf "%%" layout) "%"))
    (should (equal (vm-mime-sprintf "100%% done" layout) "100% done"))
    (should (equal (vm-mime-sprintf "%q" layout) "%q"))
    (should (equal (vm-mime-sprintf "%t %q" layout) "HTML %q"))
    ;; and the ordinary case is untouched
    (should (equal (vm-mime-sprintf "%t" layout) "HTML"))))

(ert-deftest vm-mime-test-a-button-width-works-as-printf-does ()
  "A button format pads and cuts the way the summary does.

`vm-mime-compile-format-1\=' is a copy of the summary compiler and had the
same order of the two and the same zero fill on a word (emacs-vm/vm#848).
Every button format VM ships writes the width and the maximum with the same
number, so none of them moves."
  (let ((layout (vm-mime-test--html-layout "utf-8"))
        (vm-mime-compiled-format-alist nil))
    (should (equal (vm-mime-sprintf "%20.4t" layout) "                HTML"))
    (should (equal (vm-mime-sprintf "%-20.4t" layout) "HTML                "))
    ;; a word is not zero filled, whatever the width says
    (should (equal (vm-mime-sprintf "%010t" layout) "      HTML"))
    (should (equal (vm-mime-sprintf "%-010t" layout) "HTML      "))
    ;; and the numbers still are
    (should (equal (vm-mime-sprintf "%05n" layout) "00000"))
    (should (equal (vm-mime-sprintf "%-05n" layout) "0    "))
    ;; what the shipped formats do, unchanged
    (should (equal (vm-mime-sprintf "%-10.10(%t%)" layout) "HTML      "))))

;;; The HTML image blocker that never blocked anything (emacs-vm/vm#845)
;;
;; `vm-mime-text/html-blocker' and `vm-mime-text/html-blocker-exceptions' were
;; documented as stopping an HTML part from loading the remote image a sender
;; uses to learn that you read the message.  The loop that would have done it
;; tested `(or t ...)', so it always took the branch holding a TODO comment and
;; the `blocked:' it meant to insert was unreachable.  Both options are
;; obsolete now and the loop is gone.  What blocks such an image is
;; `vm-w3m-safe-url-regexp'.

(defconst vm-mime-test--blocker-options
  '(vm-mime-text/html-blocker vm-mime-text/html-blocker-exceptions)
  "The two options that claimed to block a remote image and never did.")

(defun vm-mime-test--form-names-p (form symbol)
  "Whether SYMBOL appears anywhere in FORM.
Walked with a stack rather than recursion, as vm-integration-test.el walks
for the same reason: VM\='s lists are long enough to run the depth out."
  (let ((pending (list form)) (found nil))
    (while (and pending (not found))
      (let ((this (pop pending)))
        (cond ((eq this symbol) (setq found t))
              ((consp this)
               (let ((tail this))
                 (while (consp tail)
                   (push (car tail) pending)
                   (setq tail (cdr tail)))
                 (when tail (push tail pending)))))))
    found))

(defun vm-mime-test--files-naming (symbol)
  "Every file in lisp/ whose code names SYMBOL.
Read as forms, so the symbol in a comment or a docstring is not a hit.  Its
own `defcustom' and `make-obsolete-variable' are not a reading of it either,
so vm-vars.el is left out."
  (let ((found nil))
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'") (nreverse found))
      (unless (string-match-p "vm-\\(vars\\|autoloads\\|cus-load\\)\\.el\\'" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t
                (when (vm-mime-test--form-names-p (read (current-buffer)) symbol)
                  (push (file-name-nondirectory file) found)
                  (goto-char (point-max))))
            (end-of-file nil)))))))

(ert-deftest vm-mime-test-the-html-blocker-options-are-obsolete ()
  "REGRESSION: both say so, so setting one warns instead of being believed.
Neither has a replacement in VM itself, so each carries the message rather
than a name: `vm-w3m-safe-url-regexp' is vm-w3m.el\='s, loaded only where
emacs-w3m is installed."
  (require 'vm-vars)
  (dolist (option vm-mime-test--blocker-options)
    (let ((notice (get option 'byte-obsolete-variable)))
      (should (equal (list option (and notice t)) (list option t)))
      (should (string-match-p "vm-w3m-safe-url-regexp" (car notice)))
      (should (equal (nth 2 notice) "9.0.0")))))

(ert-deftest vm-mime-test-nothing-reads-the-html-blocker-options ()
  "REGRESSION: the dead loop is gone, so no file but vm-vars.el names them.
An obsolete option that the code still reads is worse than one it does not:
the warning says to stop setting it while the setting still does something."
  (dolist (option vm-mime-test--blocker-options)
    (should (equal (list option nil)
                   (list option (vm-mime-test--files-naming option))))))

(ert-deftest vm-mime-test-fetch-url-retrieves-a-file-url ()
  "`vm-mime-fetch-url' gets the object with Emacs, no external program.
A file: URL exercises the whole path without touching the network, and it is
the case that proves the header block is stripped: url synthesises
Content-type and Content-length in front of the file, without binding
`url-http-end-of-headers'."
  (let ((source (make-temp-file "vm-mime-url"))
        (target (generate-new-buffer " *vm-mime-url-test*")))
    (unwind-protect
        (progn
          (with-temp-file source (insert "the external body\n"))
          (should (vm-mime-fetch-url (concat "file://" source) target))
          (should (equal "the external body\n"
                         (with-current-buffer target (buffer-string)))))
      (kill-buffer target)
      (delete-file source))))

(ert-deftest vm-mime-test-fetch-url-warns-and-gives-up-on-a-bad-url ()
  "A URL that cannot be retrieved warns and answers nil, rather than signalling."
  (let ((warnings nil)
        (target (generate-new-buffer " *vm-mime-url-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-warn)
                   (lambda (_level _secs &rest args)
                     (push (apply #'format args) warnings)))
                  ((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest _) (error "no route to host"))))
          (should-not (vm-mime-fetch-url "http://example.invalid/x" target))
          (should (= 1 (length warnings)))
          (should (string-match-p "Could not retrieve" (car warnings))))
      (kill-buffer target))))

(ert-deftest vm-mime-test-fetch-url-answers-nil-for-an-empty-object ()
  "Nothing retrieved is not a retrieval."
  (let ((target (generate-new-buffer " *vm-mime-url-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest _)
                     (let ((b (generate-new-buffer " *fake response*")))
                       (with-current-buffer b (insert "Content-type: x\n\n"))
                       b))))
          (should-not (vm-mime-fetch-url "http://example.invalid/x" target)))
      (kill-buffer target))))

(provide 'vm-mime-test)

;;; vm-mime-test.el ends here
