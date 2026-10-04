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

;;; Converting a folder from one type to another

(defconst vm-folder-roundtrip-test--convertible
  '(From_ mboxcl2 mmdf babyl)
  "The types a folder is converted between here.
BellFrom_ is left out: it cannot be read back as itself, so a conversion to
it cannot be checked (emacs-vm/vm#787).")

(defun vm-folder-roundtrip-test--convert (from to label body)
  "File BODY and \"second\" as FROM, convert to TO and back, and report.
Answers a plist: :before and :after the round trip, and :converted for the
messages the TO folder held in between.  A string instead, saying what went
wrong, when something signalled or a message went missing."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-convert" t))))
    (unwind-protect
        (condition-case err
            (let* ((folder (expand-file-name "archive" dir))
                   (written (let ((vm-default-folder-type from))
                              (vm-folder-roundtrip-test--file folder body)
                              (vm-folder-roundtrip-test--file folder "second\n")
                              (let ((vm-default-folder-type from))
                                (vm-new-folder-file-name folder))))
                   (before (vm-folder-roundtrip-test--read written))
                   (out (vm-folder-name-for-type (expand-file-name "out" dir) to))
                   (back (vm-folder-name-for-type (expand-file-name "back" dir) from)))
              (vm-change-folder-type-of-file written to nil out)
              (let ((converted (vm-folder-roundtrip-test--read out)))
                (vm-change-folder-type-of-file out from nil back)
                (let ((after (vm-folder-roundtrip-test--read back)))
                  (cond
                   ((/= 2 (length converted))
                    (format "%s -> %s / %s: %d message(s) after converting"
                            from to label (length converted)))
                   ((not (equal (nth 1 converted) "second\n"))
                    (format "%s -> %s / %s: the second message became %S"
                            from to label (nth 1 converted)))
                   ((/= 2 (length after))
                    (format "%s -> %s -> %s / %s: %d message(s) on the way back"
                            from to from label (length after)))
                   (t (list :before before :converted converted :after after))))))
          (error (format "%s -> %s / %s: %s" from to label
                         (error-message-string err))))
      (delete-directory dir t))))

(defun vm-folder-roundtrip-test--unquoted (bodies)
  "BODIES with one leading `>' taken off each line.
Two lists that agree after this differ only in the quoting a folder type
puts on a line that would otherwise read as a separator."
  (mapcar (lambda (body) (replace-regexp-in-string "^>" "" body)) bodies))

(defun vm-folder-roundtrip-test--every-conversion (check)
  "Convert between every pair of types, with every body, and collect CHECK.
CHECK is called with the plist a conversion answers and the pair and body it
came from; what it answers, when not nil, is collected as a complaint."
  (let (complaints)
    (dolist (from vm-folder-roundtrip-test--convertible)
      (dolist (to vm-folder-roundtrip-test--convertible)
        (unless (eq from to)
          (dolist (spec vm-folder-roundtrip-test--bodies)
            (let ((result (vm-folder-roundtrip-test--convert
                           from to (car spec) (cdr spec))))
              (if (stringp result)
                  (push result complaints)
                (let ((complaint (funcall check result from to (car spec))))
                  (when complaint (push complaint complaints)))))))))
    (nreverse complaints)))

(ert-deftest vm-folder-roundtrip-test-conversion-keeps-every-message ()
  "Converting a folder between any two types keeps both messages.
A hundred and twenty conversions: twelve ordered pairs of types, each with
every body.  Half read or write an mmdf folder, which could not be read at
all until emacs-vm/vm#786, so this could not have passed before that
whatever the conversion did."
  (should (equal nil (vm-folder-roundtrip-test--every-conversion
                      (lambda (&rest _) nil)))))

(ert-deftest vm-folder-roundtrip-test-conversion-changes-only-the-quoting ()
  "A conversion out and back changes nothing but a leading `>'.
`vm-munge-message-separators' prepends one to a body line a From_ or mmdf
folder would otherwise read as an envelope line.  It must; what it does not
do is take it off again on the way back out (emacs-vm/vm#789).  Nothing else
about the message may change, which is what this says."
  (should (equal nil
                 (vm-folder-roundtrip-test--every-conversion
                  (lambda (result from to label)
                    (unless (equal (vm-folder-roundtrip-test--unquoted
                                    (plist-get result :before))
                                   (vm-folder-roundtrip-test--unquoted
                                    (plist-get result :after)))
                      (format "%s -> %s -> %s / %s: %S became %S"
                              from to from label
                              (plist-get result :before)
                              (plist-get result :after))))))))

(ert-deftest vm-folder-roundtrip-test-nine-conversions-keep-a-quote ()
  "Nine of the hundred and twenty come back with a `>' the message lacked.
Pinned by name so that a decision on emacs-vm/vm#789 shows up here as a
changed list rather than as a test that quietly still passes.  Each is a
body the source type had no need to quote going to a type that does.

There is no babyl cell among them, and that is deliberate: VM does not quote
a babyl separator in a body, so going through a babyl folder costs nothing
here.  What it costs instead is that Rmail cannot open such a folder at all,
which emacs-vm/vm#801 decided to keep and the manual now describes."
  (let (lossy)
    (vm-folder-roundtrip-test--every-conversion
     (lambda (result from to label)
       (unless (equal (plist-get result :before) (plist-get result :after))
         (push (format "%s -> %s -> %s / %s" from to from label) lossy))
       nil))
    (should (equal (sort lossy #'string<)
                   '("From_ -> mmdf -> From_ / an mmdf separator"
                     "babyl -> From_ -> babyl / a From_ line"
                     "babyl -> From_ -> babyl / a From_ line first"
                     "babyl -> mmdf -> babyl / an mmdf separator"
                     "mboxcl2 -> From_ -> mboxcl2 / a From_ line"
                     "mboxcl2 -> From_ -> mboxcl2 / a From_ line first"
                     "mboxcl2 -> mmdf -> mboxcl2 / an mmdf separator"
                     "mmdf -> From_ -> mmdf / a From_ line"
                     "mmdf -> From_ -> mmdf / a From_ line first")))))

;;; Saving a message from a folder of one type into another

(defun vm-folder-roundtrip-test--fill (folder type body)
  "Make FOLDER a folder of TYPE holding BODY and then \"second\".
Answers the name it was actually written under, which for mboxcl2 says the
type (emacs-vm/vm#767)."
  (let ((vm-default-folder-type type))
    (vm-folder-roundtrip-test--file folder body)
    (vm-folder-roundtrip-test--file folder "second\n")
    (vm-new-folder-file-name folder)))

(defun vm-folder-roundtrip-test--save (from to body)
  "Save the first of two messages from a FROM folder into a TO folder.
Answers a plist: :sent, the body as the source folder holds it, and :landed,
the bodies the target folder holds afterwards.  A string instead when
something signalled."
  (let ((dir (file-name-as-directory (make-temp-file "vm-savetype" t)))
        (before (buffer-list)))
    (unwind-protect
        (condition-case err
            (let* ((src (vm-folder-roundtrip-test--fill
                         (expand-file-name "src" dir) from body))
                   (dst (vm-folder-name-for-type
                         (expand-file-name "dst" dir) to))
                   (vm-init-file nil)
                   (vm-preferences-file nil)
                   (vm-confirm-quit nil)
                   (vm-frame-per-folder nil)
                   (vm-mutable-frame-configuration nil)
                   (vm-summary-show-threads nil))
              ;; A target that exists and is of the wanted type, so that
              ;; nothing about it has to be guessed.
              (vm-folder-roundtrip-test--fill
               (expand-file-name "seed" dir) to "seed\n")
              (vm-visit-folder src)
              (let ((sent (car (vm-folder-roundtrip-test--read src))))
                (vm-save-message dst 1 nil t)
                (list :sent sent
                      :landed (vm-folder-roundtrip-test--read dst))))
          (error (format "%s -> %s: %s" from to (error-message-string err))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(defun vm-folder-roundtrip-test--every-save (check)
  "Save between every pair of types with every body, collecting CHECK."
  (let (complaints)
    (dolist (from vm-folder-roundtrip-test--convertible)
      (dolist (to vm-folder-roundtrip-test--convertible)
        (dolist (spec vm-folder-roundtrip-test--bodies)
          (let ((result (vm-folder-roundtrip-test--save
                         from to (cdr spec))))
            (if (stringp result)
                (push (format "%s / %s" result (car spec)) complaints)
              (let ((complaint (funcall check result from to (car spec))))
                (when complaint (push complaint complaints))))))))
    (nreverse complaints)))

(ert-deftest vm-folder-roundtrip-test-saving-lands-one-message ()
  "Saving a message into a folder of another type puts one message there.
The bytes were written as they stood, with no quoting for the target.  A
message out of a folder that had no need to quote them -- mboxcl2 counts its
bytes, mmdf and babyl have separators of their own -- then carried a line the
target read as a separator: into a From_ folder it became two messages, and
into an mmdf folder it made the folder unreadable."
  (should (equal nil
                 (vm-folder-roundtrip-test--every-save
                  (lambda (result from to label)
                    (let ((landed (plist-get result :landed)))
                      (unless (= 1 (length landed))
                        (format "%s -> %s / %s: %d messages landed: %S"
                                from to label (length landed) landed))))))))

(ert-deftest vm-folder-roundtrip-test-saving-changes-only-the-quoting ()
  "What lands differs from what was sent only by a leading `>'.
The target may have to quote a line that would read as its separator; it may
not do anything else to the message.  Which lines get a `>' and whether it
ever comes off again is emacs-vm/vm#789."
  (should (equal nil
                 (vm-folder-roundtrip-test--every-save
                  (lambda (result from to label)
                    (let ((landed (plist-get result :landed)))
                      (when (= 1 (length landed))
                        (unless (equal (vm-folder-roundtrip-test--unquoted
                                        (list (plist-get result :sent)))
                                       (vm-folder-roundtrip-test--unquoted
                                        landed))
                          (format "%s -> %s / %s: sent %S, landed %S"
                                  from to label (plist-get result :sent)
                                  (car landed))))))))))

(provide 'vm-folder-roundtrip-test)

;;; vm-folder-roundtrip-test.el ends here
