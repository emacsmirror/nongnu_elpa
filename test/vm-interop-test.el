;;; vm-interop-test.el --- VM's folders read by another program -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Every other folder test here writes a folder with VM and reads it back with
;; VM.  That cannot show whether the folder is one another program can read,
;; and VM's readers forgive things VM's writers do: a babyl folder with a
;; stray separator byte in it reads back perfectly in VM and is mis-split by
;; anything else.
;;
;; So these write folders with VM and count the messages with Python's
;; `mailbox' module, an implementation of mbox, MMDF and Babyl that owes
;; nothing to VM.  A count that differs from what VM filed is a folder whose
;; message boundaries are not where an outside reader puts them.
;;
;; Every test skips where python3 is missing, so this costs a machine without
;; it nothing.

;;; Code:

(require 'vm-test-init)
(require 'vm-reply)
(require 'vm-folder)

(defconst vm-interop-test--script
  (expand-file-name "vm-interop-count.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The reader that owes nothing to VM.")

(defconst vm-interop-test--bodies
  '(("plain"            . "a body line\n")
    ("a From_ line"     . "a body line\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\n")
    ("a From_ line first" . "From nobody@example.com Mon Jan  1 00:00:00 2024\nrest\n")
    ("an mmdf separator" . "a body line\n\001\001\001\001\nmore\n")
    ("a babyl separator" . "a body line\n\037\014\nmore\n")
    ("no final newline" . "a body line with no newline")
    ("8-bit text"       . "a b\303\266dy line\n"))
  "Bodies that hold, or look like, a message separator of some format.")

(defconst vm-interop-test--known-disagreements
  '(("mboxcl2" . "a From_ line")
    ("mboxcl2" . "a From_ line first")
    ("babyl"   . "a babyl separator"))
  "The cells where an outside reader is expected to disagree with VM.

The two mboxcl2 ones are the type doing its job: it stores a body as it
arrived and puts the length in a header, so a reader that ignores
Content-Length splits on the `From ' line.  The manual's mbox section says
so, and this is that claim measured rather than asserted.

The babyl one is the limit emacs-vm/vm#801 decided to keep.  VM quotes a body
separator for From_ and for mmdf and not for babyl, so `mailbox.Babyl\' finds
three messages where VM filed two, and Rmail refuses the folder outright.
Quoting it would have cost a `>\' on three more of the hundred and twenty
conversion round trips, and the decision was to leave babyl compatibility
where it has always been and describe it in the manual instead.")

(defun vm-interop-test--python-p ()
  "Whether python3 is here with its `mailbox' module."
  (and (executable-find "python3")
       (file-exists-p vm-interop-test--script)
       (eq 0 (call-process "python3" nil nil nil "-c" "import mailbox"))))

(defun vm-interop-test--file-into (folder body)
  "File a composition with BODY into FOLDER through its Fcc header."
  (let ((coding-system-for-write (vm-binary-coding-system))
        (vm-dont-ask-coding-system-question t)
        (select-safe-coding-system-function nil))
    (with-temp-buffer
      (insert "To: someone@example.com\nSubject: filed\n"
              "Fcc: " folder "\n" mail-header-separator "\n" body)
      (vm-do-fcc-in-composition))))

(defun vm-interop-test--outside-count (type folder)
  "How many messages the Python reader finds in FOLDER, read as TYPE.
A string when the reader refused it, so a refusal is a result and not an
error here."
  (with-temp-buffer
    (call-process "python3" nil t nil vm-interop-test--script
                  (symbol-name type) folder)
    (let ((answer (string-trim (buffer-string))))
      (if (string-match-p "\\`[0-9]+\\'" answer)
          (string-to-number answer)
        answer))))

(defun vm-interop-test--disagreements (type)
  "File every body into a folder of TYPE and report what Python makes of it.
Answers a list of (LABEL . COUNT) for the cells where the count is not the
two messages VM filed."
  (let ((found nil))
    (dolist (spec vm-interop-test--bodies)
      (let* ((dir (file-name-as-directory (make-temp-file "vm-interop" t)))
             (vm-default-folder-type type))
        (unwind-protect
            (let ((folder (expand-file-name "archive" dir)))
              (vm-interop-test--file-into folder (cdr spec))
              (vm-interop-test--file-into folder "second body\n")
              ;; VM names an mboxcl2 folder it creates for its type (#767)
              (let ((count (vm-interop-test--outside-count
                            type (vm-new-folder-file-name folder))))
                (unless (equal count 2)
                  (push (cons (car spec) count) found))))
          (delete-directory dir t))))
    (nreverse found)))

(defun vm-interop-test--expected (type)
  "The labels where TYPE is expected to disagree, from the known list."
  (delq nil (mapcar (lambda (cell)
                      (and (equal (car cell) (symbol-name type)) (cdr cell)))
                    vm-interop-test--known-disagreements)))

(defun vm-interop-test--check (type)
  "Compare the disagreements for TYPE with the ones expected.
`skip-unless' is a macro of ert's and belongs in the test body, not here."
  (let ((found (mapcar #'car (vm-interop-test--disagreements type)))
        (expected (vm-interop-test--expected type)))
    (should (equal (sort expected #'string<) (sort found #'string<)))))

(ert-deftest vm-interop-test-From_-folders-read-the-same-outside ()
  "Every From_ folder VM writes holds two messages for Python as well.
Including the bodies that look like an envelope line: VM quotes those, which
is what makes the folder readable by anything else."
  (skip-unless (vm-interop-test--python-p))
  (vm-interop-test--check 'From_))

(ert-deftest vm-interop-test-mmdf-folders-read-the-same-outside ()
  "Every mmdf folder VM writes holds two messages for Python as well.
The mmdf separator in a body is quoted, so `mailbox.MMDF' is not misled."
  (skip-unless (vm-interop-test--python-p))
  (vm-interop-test--check 'mmdf))

(ert-deftest vm-interop-test-mboxcl2-disagrees-only-where-the-manual-says ()
  "An mboxcl2 folder splits differently for a reader that ignores the length.

Two of the seven bodies, the ones holding a `From ' line, and no others.  The
manual's mbox section describes exactly this, and until now it was a claim
about other programs with nothing behind it.  Measured with Python's plain
mbox reader, which ignores Content-Length as that section says such a reader
does."
  (skip-unless (vm-interop-test--python-p))
  (vm-interop-test--check 'mboxcl2))

(ert-deftest vm-interop-test-babyl-disagrees-on-a-separator-in-a-body ()
  "A babyl folder is read differently outside where a body holds the separator.

emacs-vm/vm#801, kept deliberately.  VM quotes such a line for From_ and for
mmdf and not for babyl: `vm-find-leading-message-separator' wants a separator
followed by an attribute line, so a bare one in a body is invisible to it and
`vm-munge-message-separators' leaves it alone.  `mailbox.Babyl' then finds
three messages where VM filed two.

One of the seven bodies, and no others.  Quoting it would have cost a `>' on
three more conversion round trips, so #801 left babyl where it was and put the
limit in the manual.  This test is the one to change if that is ever revisited."
  (skip-unless (vm-interop-test--python-p))
  (vm-interop-test--check 'babyl))


;;; Babyl folders read by Emacs's own Rmail

;; Rmail is the reference implementation of the babyl format, so what it makes
;; of a folder VM wrote settles the question better than Python can, and it
;; ships with Emacs so there is nothing to skip for.
;;
;; Rmail has not written babyl since Emacs 23, mbox being its own format now,
;; so there is no Rmail writer to compare against.  What it still has is a
;; reader, `rmail-convert-babyl-to-mbox', and that is the one that matters:
;; VM's babyl folders have to be ones Rmail can open.

(defun vm-interop-test--rmail-reads (folder)
  "How many messages Rmail finds in babyl FOLDER.
A string when Rmail refused the file, so a refusal is a result rather than an
error here."
  (require 'rmail)
  (with-temp-buffer
    (insert-file-contents folder)
    (let ((inhibit-read-only t))
      (condition-case error-data
          (progn
            (rmail-convert-babyl-to-mbox)
            (goto-char (point-min))
            (let ((n 0))
              (while (re-search-forward "^From " nil t)
                (setq n (1+ n)))
              n))
        (error (error-message-string error-data))))))

(ert-deftest vm-interop-test-rmail-refuses-a-babyl-body-holding-a-separator ()
  "Rmail opens every babyl folder VM writes but one, and this is the one.

emacs-vm/vm#801, and the measurement the decision rests on.  Rmail is the
reference implementation of this format, and a body holding the babyl
separator does not merely confuse it: it refuses the folder with

    Search failed: \",, ?\"

having taken the bare separator for the start of a message and then looked for
the attribute line that is not there.  Every other body VM can put in a babyl
folder reads back as the two messages VM filed.

Kept rather than fixed.  Quoting the line makes Rmail read it, and costs a
`>' that nothing removes on three more of the hundred and twenty conversion
round trips; #801 chose to leave babyl compatibility where it has always been
and describe the limit in the manual.  So this pins the limit: if VM ever
starts quoting, this test fails and says so."
  (dolist (spec vm-interop-test--bodies)
    (let* ((dir (file-name-as-directory (make-temp-file "vm-rmail" t)))
           (vm-default-folder-type 'babyl))
      (unwind-protect
          (let ((folder (expand-file-name "archive" dir)))
            (vm-interop-test--file-into folder (cdr spec))
            (vm-interop-test--file-into folder "second body\n")
            (if (equal (car spec) "a babyl separator")
                ;; the one Rmail will not open
                (should (equal "Search failed: \",, ?\""
                               (vm-interop-test--rmail-reads folder)))
              (should (equal 2 (vm-interop-test--rmail-reads folder)))))
        (delete-directory dir t)))))

(provide 'vm-interop-test)

;;; vm-interop-test.el ends here
