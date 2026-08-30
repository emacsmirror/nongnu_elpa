;;; vm-folder-roundtrip-test.el --- Folder write/read round trips -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Every folder type VM will create, crossed with the message bodies that
;; stress a separator: file two messages, read the folder back, and check it
;; still holds two with the boundary in the right place.
;;
;; The primitives have had tests one at a time for a long while -- what a
;; leading separator is, what `vm-skip-past-trailing-message-separator' moves
;; over.  Nothing wrote a folder and read it back, so a caller that misused
;; those primitives was invisible: an mmdf folder could not be read at all for
;; three weeks and every primitive test passed throughout (emacs-vm/vm#786).

;;; Code:

(require 'vm-test-init)
(require 'vm-reply)
(require 'vm-folder)

(defconst vm-folder-roundtrip-test--bodies
  '(("plain"                . "a body line\n")
    ("no final newline"     . "a body line with no newline")
    ("trailing blank lines" . "a body line\n\n\n")
    ("a From_ line"         . "a body line\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\n")
    ("a From_ line first"   . "From nobody@example.com Mon Jan  1 00:00:00 2024\nrest\n")
    ("an mmdf separator"    . "a body line\n\001\001\001\001\nmore\n")
    ("a babyl separator"    . "a body line\n\037\014\nmore\n")
    ("a bare CR"            . "a body line\rwith a cr\n")
    ("an empty body"        . "")
    ("8-bit text"           . "a b\366dy line\n"))
  "Bodies that look like a separator, end like one, or end with nothing.
Each is filed as the first of two messages, so that what follows it has to be
found as a message of its own.")

(defun vm-folder-roundtrip-test--file (folder body)
  "File a composition with BODY in FOLDER, through its Fcc header.
The coding-system variables are bound as `vm-mail-send' binds them around the
Fcc, so that 8-bit text does not stop to ask what to write it in."
  (let ((coding-system-for-write (vm-binary-coding-system))
        (vm-dont-ask-coding-system-question t)
        (select-safe-coding-system-function nil))
    (with-temp-buffer
      (insert "To: someone@example.com\nSubject: filed\n"
              "Fcc: " folder "\n" mail-header-separator "\n" body)
      (vm-do-fcc-in-composition))))

(defun vm-folder-roundtrip-test--read (folder)
  "The bodies of the messages FOLDER holds, read as VM reads the folder."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert-file-contents folder)
    ;; As a visit has it: `vm-build-message-list' asks `vm-get-folder-type',
    ;; which reads the folder's name.  Without the name an mboxcl2 folder is
    ;; recognised only by guessing at its contents, which is deprecated.
    (setq-local buffer-file-name folder)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (vm-build-message-list)
    (mapcar (lambda (m)
              (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))
            vm-message-list)))

(defun vm-folder-roundtrip-test--check (type label body)
  "File BODY and then \"second\" into a new folder of TYPE, and read it back.
LABEL names the body in the failure message.  Answers nil when the round trip
is sound, and a string saying what went wrong when it is not."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-roundtrip" t)))
         (folder (expand-file-name "archive" dir))
         (vm-default-folder-type type))
    (unwind-protect
        (condition-case err
            (progn
              (vm-folder-roundtrip-test--file folder body)
              (vm-folder-roundtrip-test--file folder "second\n")
              ;; VM names an mboxcl2 folder it creates for its type (#767),
              ;; so ask where the copy actually went.
              (let ((bodies (vm-folder-roundtrip-test--read
                             (vm-new-folder-file-name folder))))
                (cond
                 ((/= 2 (length bodies))
                  (format "%s / %s: %d message(s), not 2: %S"
                          type label (length bodies) bodies))
                 ((not (equal (nth 1 bodies) "second\n"))
                  (format "%s / %s: the second message reads %S"
                          type label (nth 1 bodies)))
                 ((string-match-p "^Subject: filed" (nth 0 bodies))
                  (format "%s / %s: the first message swallowed the headers of \
the second: %S" type label (nth 0 bodies)))
                 (t nil))))
          (error (format "%s / %s: %s" type label (error-message-string err))))
      (delete-directory dir t))))

(defun vm-folder-roundtrip-test--every-body (type)
  "Round-trip every body in `vm-folder-roundtrip-test--bodies' through TYPE."
  (delq nil
        (mapcar (lambda (spec)
                  (vm-folder-roundtrip-test--check type (car spec) (cdr spec)))
                vm-folder-roundtrip-test--bodies)))

(ert-deftest vm-folder-roundtrip-test-From_ ()
  "A From_ folder holds what was filed in it, whatever the bodies look like."
  (should (equal nil (vm-folder-roundtrip-test--every-body 'From_))))

(ert-deftest vm-folder-roundtrip-test-mboxcl2 ()
  "An mboxcl2 folder does, its byte counts describing what was written."
  (should (equal nil (vm-folder-roundtrip-test--every-body 'mboxcl2))))

(ert-deftest vm-folder-roundtrip-test-mmdf ()
  "An mmdf folder does.
It could not be read at all between 2026-08-07 and the fix: every one of
these answered `end-of-buffer' (emacs-vm/vm#786)."
  (should (equal nil (vm-folder-roundtrip-test--every-body 'mmdf))))

(ert-deftest vm-folder-roundtrip-test-babyl ()
  "A babyl folder does."
  (should (equal nil (vm-folder-roundtrip-test--every-body 'babyl))))

(ert-deftest vm-folder-roundtrip-test-BellFrom_-is-read-as-From_ ()
  "A BellFrom_ folder is read back as From_, and its messages run together.
That type is a From_ folder without the blank line between messages, so it
has no signature of its own and nothing in its name says what it is: VM
answers From_ for it, and the From_ reader takes the next envelope line for
body text, which for that format it has to.  Recorded here rather than
asserted away, because what to do about it is a question for the maintainer
and not for this test (emacs-vm/vm#787).

Pinned so that fixing it is noticed here: this test is the one to delete."
  (let ((failures (vm-folder-roundtrip-test--every-body 'BellFrom_)))
    (should failures)
    (should (cl-every (lambda (f) (string-match-p "not 2\\|swallowed" f))
                      failures))))

(provide 'vm-folder-roundtrip-test)

;;; vm-folder-roundtrip-test.el ends here
