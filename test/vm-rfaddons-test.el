;;; vm-rfaddons-test.el --- Tests for vm-rfaddons.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for the add-on functions in vm-rfaddons.el.

;;; Code:

(require 'vm-test-init)
(require 'vm-rfaddons)

;;; vm-mail-check-recipients tests

(defmacro vm-rfaddons-test-with-headers (headers &rest body)
  "Run BODY in a mail-mode buffer whose header section is HEADERS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,headers mail-header-separator "\n" "body\n")
     (let ((text-quoting-style 'grave))
       ,@body)))

(ert-deftest vm-rfaddons-test-check-recipients-plain ()
  "Test that ordinary recipients pass."
  (vm-rfaddons-test-with-headers
      "To: someone@example.com\nCC: a@example.com, b@example.org\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-rfaddons-test-check-recipients-missing-comma ()
  "Test that a genuinely missing separator is still caught."
  (vm-rfaddons-test-with-headers
      "To: first@example.com second@example.org\n"
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-rfaddons-test-check-recipients-missing-comma-in-cc ()
  "Test that the other recipient headers are checked too."
  (vm-rfaddons-test-with-headers
      "To: fine@example.com\nCC: first@example.com second@example.org\n"
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-rfaddons-test-check-recipients-encoded-word ()
  "Test that an encoded word containing an address is not a missing comma.
Regression test for issue #417: the check looked for two \"@\" anywhere in
the header, so a display name that is a MIME encoded word holding an
address -- which Exchange and Outlook both produce -- blocked sending
with \"Missing separator\"."
  (vm-rfaddons-test-with-headers
      (concat "To: Uday S Reddy "
              "=?utf-8?Q?=E2=80=8E[u.s.reddy@cs.bham.ac.uk]=E2=80=8E?="
              " <u.s.reddy@cs.bham.ac.uk>\n")
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-rfaddons-test-check-recipients-quoted-at ()
  "Test that a quoted display name containing \"@\" is allowed."
  (vm-rfaddons-test-with-headers
      "To: \"someone@elsewhere\" <someone@example.com>\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-rfaddons-test-check-recipients-encoded-word-and-real-error ()
  "Test that an encoded word does not mask a real missing separator."
  (vm-rfaddons-test-with-headers
      (concat "To: =?utf-8?Q?name?= <first@example.com>"
              " second@example.org\n")
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-rfaddons-test-check-recipients-comment ()
  "Test that an RFC 5322 comment containing \"@\" does not block sending.
A parenthesised comment may hold anything, an address included, and the
check counted its \"@\" as a second address."
  (vm-rfaddons-test-with-headers
      "To: a@example.com (the a@b guy)\n"
    (should (null (vm-mail-check-recipients))))
  (vm-rfaddons-test-with-headers
      "To: Jane <jane@example.com> (jane@old)\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-rfaddons-test-check-recipients-nested-comment ()
  "Test that nested comments are stripped too; RFC 5322 allows them."
  (vm-rfaddons-test-with-headers
      "To: a@example.com (outer (inner b@c) still)\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-rfaddons-test-check-recipients-comment-hides-nothing ()
  "Test that a comment does not mask a real missing separator."
  (vm-rfaddons-test-with-headers
      "To: a@example.com (note) b@example.org\n"
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-rfaddons-test-check-recipients-percent-in-address ()
  "Test that a \"%\" in an address does not break the error message.
The message has the address interpolated into it and was passed to
`error' as the format string, so \"%\" -- legal in a local part, and
used by percent-hack routing -- gave \"Not enough arguments for format
string\" instead of the missing-separator complaint."
  (vm-rfaddons-test-with-headers
      "To: a%s@example.com b%d@example.org\n"
    (let ((err (should-error (vm-mail-check-recipients) :type 'error)))
      (should (string-match "Missing separator" (cadr err))))))

(ert-deftest vm-rfaddons-test-check-recipients-strip ()
  "Test the helper that removes the parts allowed to contain \"@\"."
  (should (equal (vm-mail-check-recipients-strip
                  "=?utf-8?Q?a@b?= <c@d.example>")
                 " <c@d.example>"))
  (should (equal (vm-mail-check-recipients-strip
                  "\"a@b\" <c@d.example>")
                 " <c@d.example>"))
  (should (equal (vm-mail-check-recipients-strip "c@d.example")
                 "c@d.example")))

(provide 'vm-rfaddons-test)

;;; vm-rfaddons-test.el ends here
