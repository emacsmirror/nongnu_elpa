;;; vm-expunge-roundtrip-test.el --- Expunge write/read round trips -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Expunge rewrites a folder in place: it deletes the text between one
;; message separator and the next, for each message marked deleted, and
;; leaves the rest where it was.  Where the separator ends is a property of
;; the folder type, so every arithmetic error in it is a folder type away
;; from being caught.
;;
;; Expunge has had tests for years, all of them about `vm-message-list' --
;; which messages leave the list, what the pointer does, what the reverse
;; links hold.  None wrote the folder out and read it back, and none used a
;; folder that was not From_.  So this crosses the two axes that matter for
;; corruption: the folder type, and which of the messages go.
;;
;; Four messages give sixteen subsets, and all sixteen are run for each type,
;; the empty one as the control and the full one for the empty folder that
;; results.

;;; Code:

(require 'vm-test-init)
(require 'vm-reply)
(require 'vm-folder)
(require 'vm-delete)

(defconst vm-expunge-roundtrip-test--types
  '(From_ mboxcl2 mmdf babyl)
  "The folder types an expunge is checked in.
BellFrom_ is left out: it cannot be read back as itself, so what an expunge
left behind could not be checked (emacs-vm/vm#787).")

(defconst vm-expunge-roundtrip-test--count 4
  "How many messages each folder is built with.")

(defconst vm-expunge-roundtrip-test--decoys
  '("From nobody@example.com Mon Jan  1 00:00:00 2024\n"
    "\001\001\001\001\n"
    "\037\014\n"
    "")
  "Text put in a body that reads as the start or end of a message.
One per message, so that whichever messages an expunge takes, a boundary it
gets wrong lands on one of these.  The last is empty: a message whose body is
only the marker line has nothing else to find.")

(defun vm-expunge-roundtrip-test--body (i &optional adversarial)
  "The body of message I, distinct from every other and from a separator.
ADVERSARIAL non-nil follows the marker line with text that reads as a
separator, so an expunge that mistakes it deletes the wrong region."
  (format "body of message %d\n%s" i
          (if adversarial
              (nth (mod i (length vm-expunge-roundtrip-test--decoys))
                   vm-expunge-roundtrip-test--decoys)
            "")))

(defun vm-expunge-roundtrip-test--build (folder n &optional adversarial)
  "File N messages into FOLDER through Fcc, and answer where they landed.
ADVERSARIAL non-nil gives each a body that reads as a separator.
`vm-default-folder-type' decides the type, as it does for any folder VM
creates.  The coding-system variables are bound as `vm-mail-send' binds them
around the Fcc."
  (dotimes (i n)
    (let ((coding-system-for-write (vm-binary-coding-system))
          (vm-dont-ask-coding-system-question t)
          (select-safe-coding-system-function nil))
      (with-temp-buffer
        (insert "To: someone@example.com\n"
                (format "Subject: message %d\n" i)
                "Fcc: " folder "\n" mail-header-separator "\n"
                (vm-expunge-roundtrip-test--body i adversarial))
        (vm-do-fcc-in-composition))))
  ;; VM names an mboxcl2 folder it creates for its type (#767), so ask where
  ;; the copies actually went.
  (vm-new-folder-file-name folder))

(defun vm-expunge-roundtrip-test--read (folder)
  "The bodies of the messages FOLDER holds, read as VM reads the folder.
Nil when FOLDER is not there: saving a folder every message was expunged
from removes it, `vm-delete-empty-folders' defaulting to t, and no messages
is what that leaves either way."
  (if (not (file-exists-p folder))
      nil
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert-file-contents folder)
    ;; `vm-build-message-list' re-derives the type from the buffer, and reads
    ;; the folder's name to do it.
    (setq-local buffer-file-name folder)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (vm-build-message-list)
    (mapcar (lambda (m)
              (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))
            vm-message-list))))

(defun vm-expunge-roundtrip-test--gaps (folder)
  "Complain if the messages in FOLDER do not cover it end to end.
Every message runs from its own separator to the next one, so a folder is
tiled by its messages: any byte an expunge left behind shows up as a gap
between one message and the next, or as text after the last.

This is what a read-back cannot see.  A babyl reader skips from one separator
to the next and so forgives a stray `\037' before one; the folder still holds
it, and another reader of that format need not be so forgiving.  Only babyl
has anything of its own before the first message, its two-line header."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert-file-contents folder)
    (setq-local buffer-file-name folder)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (vm-build-message-list)
    (let ((complaints nil)
          (previous nil))
      (dolist (m vm-message-list)
        (cond ((null previous)
               (unless (or (eq vm-folder-type 'babyl)
                           (= (point-min) (vm-start-of m)))
                 (push (format "%d byte(s) before the first message"
                               (- (vm-start-of m) (point-min)))
                       complaints)))
              ((/= previous (vm-start-of m))
               (push (format "%d byte(s) left between messages"
                             (- (vm-start-of m) previous))
                     complaints)))
        (setq previous (vm-end-of m)))
      (when (and previous (/= previous (point-max)))
        (push (format "%d byte(s) left after the last message"
                      (- (point-max) previous))
              complaints))
      (nreverse complaints))))

(defconst vm-expunge-roundtrip-test--separators
  '((From_   "^From " 1 0)
    (mboxcl2 "^From " 1 0)
    (mmdf    "\001\001\001\001\n" 2 0)
    (babyl   "\037" 1 1))
  "How many separator bytes a folder of each type holds, per message.
Each entry is the type, a regexp, how many times it occurs per message, and
how many times it occurs outside the messages: a babyl folder ends its header
with a `\037' as well as each message, so it holds one more than it has
messages.

This is what sees a stray separator byte, where neither a read-back nor the
tiling can.  VM's babyl reader starts a message at the byte after the one
before, so it absorbs a stray `\037' into the next message's separator and
reports nothing: an expunge one byte short leaves `\037\037\014' in the file
and every other check here passes.")

(defun vm-expunge-roundtrip-test--separator-count (folder type messages)
  "Complain if FOLDER of TYPE does not hold one separator per message.
MESSAGES is how many it should hold."
  (let ((spec (assq type vm-expunge-roundtrip-test--separators)))
    (when spec
      (with-temp-buffer
        (insert-file-contents folder)
        (goto-char (point-min))
        (let ((found 0)
              (wanted (+ (* (nth 2 spec) messages) (nth 3 spec))))
          (while (re-search-forward (nth 1 spec) nil t)
            (setq found (1+ found)))
          (unless (= found wanted)
            (format "%d separator(s) in the file, wanted %d" found wanted)))))))

(defun vm-expunge-roundtrip-test--expunge (folder doomed)
  "Visit FOLDER, mark the messages at the indices in DOOMED, expunge and save."
  (let ((vm-init-file nil)
        (vm-preferences-file nil)
        (vm-confirm-quit nil)
        (vm-frame-per-folder nil)
        (vm-mutable-frame-configuration nil)
        (vm-summary-show-threads nil)
        (vm-folder-history vm-folder-history)
        (vm-last-visit-folder vm-last-visit-folder)
        (vm-user-interaction-buffer vm-user-interaction-buffer)
        (before (buffer-list)))
    (require 'vm)
    (unwind-protect
        (progn
          (vm-visit-folder folder)
          (dolist (i doomed)
            (vm-set-deleted-flag (nth i vm-message-list) t))
          (vm-expunge-folder)
          (vm-save-folder))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(defun vm-expunge-roundtrip-test--subsets (n)
  "Every subset of the indices below N, as lists, smallest first."
  (let ((subsets nil))
    (dotimes (bits (ash 1 n))
      (let ((indices nil))
        (dotimes (i n)
          (when (/= 0 (logand bits (ash 1 i)))
            (push i indices)))
        (push (nreverse indices) subsets)))
    (nreverse subsets)))

(defun vm-expunge-roundtrip-test--survivors (n doomed &optional adversarial)
  "The marker lines of the messages below N that are not in DOOMED.
Only the first line of each body is compared.  What follows it is a decoy
that the folder type may legitimately have quoted on the way in, VM munging
a body line that would read as an envelope line (emacs-vm/vm#789); what this
is asking is which messages are left and in what order, not what quoting
they went through."
  (let ((bodies nil))
    (dotimes (i n)
      (unless (memq i doomed)
        (push (car (split-string (vm-expunge-roundtrip-test--body i adversarial)
                                 "\n"))
              bodies)))
    (nreverse bodies)))

(defun vm-expunge-roundtrip-test--check (type doomed &optional adversarial)
  "Expunge DOOMED from a fresh folder of TYPE and report what is left.
ADVERSARIAL non-nil builds the folder with separator-like bodies.  Answers
nil when what remains is exactly what was not deleted, in order, and a string
saying what went wrong when it is not."
  (let* ((n vm-expunge-roundtrip-test--count)
         (dir (file-name-as-directory (make-temp-file "vm-expunge" t)))
         (vm-default-folder-type type))
    (unwind-protect
        (condition-case err
            (let* ((folder (vm-expunge-roundtrip-test--build
                            (expand-file-name "archive" dir) n adversarial))
                   (wanted (vm-expunge-roundtrip-test--survivors
                            n doomed adversarial)))
              (vm-expunge-roundtrip-test--expunge folder doomed)
              (let ((left (mapcar (lambda (text)
                                    ;; The marker line is the first line of
                                    ;; the body, which follows the headers.
                                    (car (split-string
                                          (if (string-match "\n\n" text)
                                              (substring text (match-end 0))
                                            text)
                                          "\n")))
                                  (vm-expunge-roundtrip-test--read folder))))
                (cond
                 ((not (equal left wanted))
                  (format "%s / expunged %S: %d message(s) left, wanted %d: \
%S rather than %S" type doomed (length left) (length wanted) left wanted))
                 ((and (file-exists-p folder)
                       (vm-expunge-roundtrip-test--gaps folder))
                  (format "%s / expunged %S: the folder is not tiled by its \
messages: %S" type doomed
                          (vm-expunge-roundtrip-test--gaps folder)))
                 ;; Only where the bodies hold no separator of their own: a
                 ;; decoy that reached the folder unquoted is a separator in
                 ;; the file that no message accounts for, which is what
                 ;; vm-expunge-roundtrip-test-which-types-quote-their-own-\
                 ;; separator is for.
                 ((and (not adversarial)
                       (file-exists-p folder)
                       (vm-expunge-roundtrip-test--separator-count
                        folder type (length wanted)))
                  (format "%s / expunged %S: %s" type doomed
                          (vm-expunge-roundtrip-test--separator-count
                           folder type (length wanted))))
                 (t nil))))
          (error (format "%s / expunged %S: %s"
                         type doomed (error-message-string err))))
      (delete-directory dir t))))

(defun vm-expunge-roundtrip-test--every-subset (type &optional adversarial)
  "Expunge every subset of a four-message folder of TYPE, and report."
  (delq nil
        (mapcar (lambda (doomed)
                  (vm-expunge-roundtrip-test--check type doomed adversarial))
                (vm-expunge-roundtrip-test--subsets
                 vm-expunge-roundtrip-test--count))))

(ert-deftest vm-expunge-roundtrip-test-From_ ()
  "Expunging any subset of a From_ folder leaves the rest of it readable."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'From_))))

(ert-deftest vm-expunge-roundtrip-test-mboxcl2 ()
  "Expunging any subset of an mboxcl2 folder does.
This is the type #466 chose for storing a message as it arrived, so a byte
count left describing the wrong text would be a loss with nothing to warn of
it."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'mboxcl2))))

(ert-deftest vm-expunge-roundtrip-test-mmdf ()
  "Expunging any subset of an mmdf folder does."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'mmdf))))

(ert-deftest vm-expunge-roundtrip-test-babyl ()
  "Expunging any subset of a babyl folder does."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'babyl))))


;;; The same sixteen again, with bodies that read as separators

(ert-deftest vm-expunge-roundtrip-test-From_-with-separator-bodies ()
  "Expunging a From_ folder whose bodies read as separators leaves the rest."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'From_ t))))

(ert-deftest vm-expunge-roundtrip-test-mboxcl2-with-separator-bodies ()
  "Expunging an mboxcl2 folder whose bodies read as separators does.
This type does not quote a body line, storing a byte count instead, so the
decoys reach the folder as they were written and an expunge that searches for
a separator rather than trusting the count would find one of them."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'mboxcl2 t))))

(ert-deftest vm-expunge-roundtrip-test-mmdf-with-separator-bodies ()
  "Expunging an mmdf folder whose bodies read as separators does."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'mmdf t))))

(ert-deftest vm-expunge-roundtrip-test-babyl-with-separator-bodies ()
  "Expunging a babyl folder whose bodies read as separators does."
  (should (equal nil (vm-expunge-roundtrip-test--every-subset 'babyl t))))


;;; Which types quote a body line that reads as their own separator

(ert-deftest vm-expunge-roundtrip-test-which-types-quote-their-own-separator ()
  "Record which folder types quote a body holding their own separator.
One message, its body holding the separator of the folder it is filed in,
with text either side of it.

VM reads all four back whole, so nothing is lost to VM.  What differs is the
file:

- From_ and mmdf quote the line, `vm-munge-message-separators' putting a `>'
  in front of it, so no outside reader is misled;
- mboxcl2 writes it as it stands, which is the point of the type: the byte
  count says where the message ends whatever the body holds (emacs-vm/vm#466);
- babyl writes it as it stands, and an outside reader cannot cope: Rmail
  refuses such a folder outright and Python\'s `mailbox.Babyl\' finds three
  messages where VM wrote two.  VM\'s babyl munging looks for a separator
  followed by an attribute line and a bare one in a body has none, so nothing
  is quoted.  Kept deliberately, emacs-vm/vm#801 having decided to leave
  babyl compatibility where it was and describe it in the manual instead.

Every regexp here is anchored at line start on purpose.  Unanchored,
`\001\001\001\001\' matches inside `>\001\001\001\001\' as well, so the count
rises whether the line was quoted or not and the measurement cannot tell the
two apart.  That is how this test first recorded mmdf as unquoted when it is
not."
  (dolist (spec '((From_   "From nobody@example.com Mon Jan  1 00:00:00 2024\n"
                           "^From " quoted)
                  (mboxcl2 "From nobody@example.com Mon Jan  1 00:00:00 2024\n"
                           "^From " raw)
                  (mmdf    "\001\001\001\001\n" "^\001\001\001\001" quoted)
                  (babyl   "\037\014\n" "^\037\014" raw)))
    (let* ((type (nth 0 spec))
           (body (nth 1 spec))
           (regexp (nth 2 spec))
           (expected (nth 3 spec))
           (dir (file-name-as-directory (make-temp-file "vm-quoting" t)))
           (vm-default-folder-type type))
      (unwind-protect
          (let* ((folder (expand-file-name "archive" dir))
                 (written (progn
                            (let ((coding-system-for-write
                                   (vm-binary-coding-system))
                                  (vm-dont-ask-coding-system-question t)
                                  (select-safe-coding-system-function nil))
                              (with-temp-buffer
                                (insert "To: someone@example.com\n"
                                        "Subject: only\n"
                                        "Fcc: " folder "\n"
                                        mail-header-separator "\n"
                                        "before\n" body "after\n")
                                (vm-do-fcc-in-composition)))
                            (vm-new-folder-file-name folder)))
                 (clean (progn
                          (let ((coding-system-for-write
                                 (vm-binary-coding-system))
                                (vm-dont-ask-coding-system-question t)
                                (select-safe-coding-system-function nil))
                            (with-temp-buffer
                              (insert "To: someone@example.com\n"
                                      "Subject: only\n"
                                      "Fcc: " (expand-file-name "clean" dir)
                                      "\n" mail-header-separator "\n"
                                      "before\nafter\n")
                              (vm-do-fcc-in-composition)))
                          (vm-new-folder-file-name
                           (expand-file-name "clean" dir))))
                 (count (lambda (file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (goto-char (point-min))
                            (let ((n 0))
                              (while (re-search-forward regexp nil t)
                                (setq n (1+ n)))
                              n))))
                 (bodies (vm-expunge-roundtrip-test--read written)))
            ;; Whatever the file holds, VM reads one message and keeps it all.
            (should (equal 1 (length bodies)))
            (should (string-match-p "before" (car bodies)))
            (should (string-match-p "after" (car bodies)))
            (if (eq expected 'quoted)
                (should (equal (funcall count written) (funcall count clean)))
              (should (> (funcall count written) (funcall count clean)))))
        (delete-directory dir t)))))


;;; What an expunge of every message leaves on disk

(ert-deftest vm-expunge-roundtrip-test-an-emptied-folder-is-removed ()
  "Expunging every message removes the folder file, except a babyl one.
`vm-delete-empty-folders' defaults to t and `vm-save-folder' acts on
`(zerop (buffer-size))', so what survives is whatever the format writes
outside its messages.  A babyl folder has a header of its own, so it is not
zero length and stays, holding no messages; the other three go.

Recorded rather than asserted away: it is the format's doing rather than a
decision VM made about babyl, and a reader who expunges a babyl folder empty
is left with a file where the others leave none."
  (dolist (type vm-expunge-roundtrip-test--types)
    (let* ((n vm-expunge-roundtrip-test--count)
           (dir (file-name-as-directory (make-temp-file "vm-expunge" t)))
           (vm-default-folder-type type))
      (unwind-protect
          (let ((folder (vm-expunge-roundtrip-test--build
                         (expand-file-name "archive" dir) n)))
            (vm-expunge-roundtrip-test--expunge folder (number-sequence 0 (1- n)))
            (if (eq type 'babyl)
                (progn
                  (should (file-exists-p folder))
                  (should (equal nil (vm-expunge-roundtrip-test--read folder))))
              (should-not (file-exists-p folder))))
        (delete-directory dir t)))))

(provide 'vm-expunge-roundtrip-test)

;;; vm-expunge-roundtrip-test.el ends here
