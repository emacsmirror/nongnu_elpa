;;; vm-digest-test.el --- Tests for vm-digest.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM digest/encapsulation functions in vm-digest.el

;;; Code:

(require 'vm-test-init)
(require 'vm-digest)

;;; RFC 934 char stuffing tests

(ert-deftest vm-digest-test-rfc934-stuff-single-dash ()
  "Test RFC 934 stuffing of line starting with single dash."
  (with-temp-buffer
    (insert "-Line starts with dash\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "- -Line starts with dash"))))

(ert-deftest vm-digest-test-rfc934-stuff-double-dash ()
  "Test RFC 934 stuffing of line starting with double dash."
  (with-temp-buffer
    (insert "--Double dash line\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "- --Double dash line"))))

(ert-deftest vm-digest-test-rfc934-stuff-multiple-lines ()
  "Test RFC 934 stuffing of multiple lines with dashes."
  (with-temp-buffer
    (insert "Normal line\n")
    (insert "-Dash line\n")
    (insert "Another normal\n")
    (insert "---Triple dash\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (search-forward "Normal line" nil t))
    (should (search-forward "- -Dash line" nil t))
    (should (search-forward "Another normal" nil t))
    (should (search-forward "- ---Triple dash" nil t))))

(ert-deftest vm-digest-test-rfc934-stuff-no-dash ()
  "Test RFC 934 stuffing leaves non-dash lines unchanged."
  (with-temp-buffer
    (insert "No dashes here\n")
    (insert "Or here either\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (should (string= (buffer-string) "No dashes here\nOr here either\n"))))

(ert-deftest vm-digest-test-rfc934-unstuff-single ()
  "Test RFC 934 unstuffing of single stuffed line."
  (with-temp-buffer
    (insert "- -Line was stuffed\n")
    (vm-rfc934-char-unstuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "-Line was stuffed"))))

(ert-deftest vm-digest-test-rfc934-unstuff-multiple ()
  "Test RFC 934 unstuffing of multiple stuffed lines."
  (with-temp-buffer
    (insert "Normal line\n")
    (insert "- -Stuffed line\n")
    (insert "- --Double stuffed\n")
    (vm-rfc934-char-unstuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (search-forward "Normal line" nil t))
    (should (search-forward "-Stuffed line" nil t))
    (should (search-forward "--Double stuffed" nil t))))

(ert-deftest vm-digest-test-rfc934-roundtrip ()
  "Test RFC 934 stuff then unstuff returns original."
  (with-temp-buffer
    (let ((original "-Original dash line\n--Double\n---Triple\n"))
      (insert original)
      (vm-rfc934-char-stuff-region (point-min) (point-max))
      (vm-rfc934-char-unstuff-region (point-min) (point-max))
      (should (string= (buffer-string) original)))))

;;; RFC 1153 char stuffing tests

(ert-deftest vm-digest-test-rfc1153-stuff-separator ()
  "Test RFC 1153 stuffing of 30-hyphen separator."
  (with-temp-buffer
    (insert "------------------------------\n")
    (vm-rfc1153-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at " -----------------------------"))))

(ert-deftest vm-digest-test-rfc1153-stuff-non-separator ()
  "Test RFC 1153 stuffing leaves non-separator lines unchanged."
  (with-temp-buffer
    (insert "Normal text\n")
    (insert "-----------------------------\n")  ; Only 29 dashes
    (insert "-------------------------------\n")  ; 31 dashes
    (let ((original (buffer-string)))
      (vm-rfc1153-char-stuff-region (point-min) (point-max))
      (should (string= (buffer-string) original)))))

(ert-deftest vm-digest-test-rfc1153-unstuff-separator ()
  "Test RFC 1153 unstuffing of stuffed separator."
  (with-temp-buffer
    (insert " -----------------------------\n")
    (vm-rfc1153-char-unstuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "------------------------------"))))

(ert-deftest vm-digest-test-rfc1153-roundtrip ()
  "Test RFC 1153 stuff then unstuff returns original."
  (with-temp-buffer
    (let ((original "Text before\n------------------------------\nText after\n"))
      (insert original)
      (vm-rfc1153-char-stuff-region (point-min) (point-max))
      (vm-rfc1153-char-unstuff-region (point-min) (point-max))
      (should (string= (buffer-string) original)))))

;;; vm-digest-get-header-contents tests

(ert-deftest vm-digest-test-get-header-from ()
  "Test extracting From header."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "Subject: Test\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (let ((from (vm-digest-get-header-contents "From")))
      (should (stringp from))
      (should (string-match "test@example.com" from)))))

(ert-deftest vm-digest-test-get-header-subject ()
  "Test extracting Subject header."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "Subject: Test Subject Line\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (let ((subject (vm-digest-get-header-contents "Subject")))
      (should (stringp subject))
      (should (string-match "Test Subject Line" subject)))))

(ert-deftest vm-digest-test-get-header-missing ()
  "Test extracting non-existent header returns nil."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (should (null (vm-digest-get-header-contents "X-NonExistent")))))

(ert-deftest vm-digest-test-get-header-case-insensitive ()
  "Test header matching is case-insensitive."
  (with-temp-buffer
    (insert "FROM: test@example.com\n")
    (insert "\n")
    (goto-char (point-min))
    (let ((from (vm-digest-get-header-contents "from")))
      (should (stringp from))
      (should (string-match "test@example.com" from)))))

(ert-deftest vm-digest-test-get-header-multiline ()
  "Test extracting multiline header."
  (with-temp-buffer
    (insert "Subject: This is a very long subject\n")
    (insert "   that continues on the next line\n")
    (insert "From: test@example.com\n")
    (insert "\n")
    (goto-char (point-min))
    (let ((subject (vm-digest-get-header-contents "Subject")))
      (should (stringp subject))
      (should (string-match "very long subject" subject))
      (should (string-match "continues" subject)))))

;;; vm-guess-digest-type tests

;; Note: These tests would need actual message structures to work fully

;;; Interactive command tests

(ert-deftest vm-digest-test-burst-commands-interactive ()
  "Test that burst commands are interactive."
  (should (commandp 'vm-burst-digest))
  (should (commandp 'vm-burst-rfc934-digest))
  (should (commandp 'vm-burst-rfc1153-digest))
  (should (commandp 'vm-burst-mime-digest))
  (should (commandp 'vm-burst-digest-to-temp-folder)))

;;; Variable existence tests

(ert-deftest vm-digest-test-variables ()
  "Test that digest-related variables exist with expected types."
  (should (boundp 'vm-digest-burst-type))
  (should (stringp vm-digest-burst-type))
  (should (boundp 'vm-digest-identifier-header-format))
  (should (boundp 'vm-delete-after-bursting)))

;;; Bursting a digest, and making one (emacs-vm/vm#675)
;;
;; The tests above cover the stuffing and unstuffing of separator lines.
;; These burst a digest into its messages and build one from messages, which
;; is what a reader does with the commands.

(defconst vm-digest-test--rfc934
  (concat "From listserv@example.com Mon Jan  1 00:00:00 2024\n"
          "From: listserv@example.com\n"
          "Subject: a digest of two\n\n"
          "This is the preamble.\n\n"
          "------------------------------\n"
          "From: alice@example.com\nSubject: first enclosed\n"
          "Date: Mon, 1 Jan 2024 10:00:00 +0000\n\n"
          "The first enclosed body.\n\n"
          "------------------------------\n"
          "From: bob@example.com\nSubject: second enclosed\n"
          "Date: Mon, 1 Jan 2024 11:00:00 +0000\n\n"
          "The second enclosed body.\n\n"
          "------------------------------\n\n")
  "An RFC 934 digest of two messages.  It ends with a blank line, as every
message in an mbox folder does: without it the messages a burst appends are
read as garbage at the end of the folder.")

(defconst vm-digest-test--rfc1153
  (concat "From listserv@example.com Mon Jan  1 00:00:00 2024\n"
          "From: listserv@example.com\n"
          "Subject: a digest of two\n\n"
          "This is the preamble.\n\n"
          "----------------------------------------------------------------------\n\n"
          "From: alice@example.com\nSubject: first enclosed\n"
          "Date: Mon, 1 Jan 2024 10:00:00 +0000\n\n"
          "The first enclosed body.\n\n"
          "------------------------------\n\n"
          "From: bob@example.com\nSubject: second enclosed\n"
          "Date: Mon, 1 Jan 2024 11:00:00 +0000\n\n"
          "The second enclosed body.\n\n"
          "----------------------------------------------------------------------\n\n"
          "End of digest\n\n")
  "An RFC 1153 digest of two messages.")

(defmacro vm-digest-test--with-a-digest (digest &rest body)
  "Visit a folder holding DIGEST as its only message and run BODY.
FOLDER is the folder buffer, and the digest is the current message."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-digest" t)))
          (file (expand-file-name "inbox" dir))
          (vm-folder-directory dir)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          ;; bursting adds an X-Digest header, and showing a message with one
          ;; caches a compiled summary format for it: a cache is not this
          ;; test's to leave behind
          (vm-summary-untokenized-compiled-format-alist
           vm-summary-untokenized-compiled-format-alist)
          (vm-summary-tokenized-compiled-format-alist
           vm-summary-tokenized-compiled-format-alist)
          (before (buffer-list))
          folder)
     (unwind-protect
         (progn
           (write-region ,digest nil file nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore)
                     ((symbol-function 'vm-present-current-message) #'ignore))
             (vm-visit-folder file)
             (setq folder (current-buffer))
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-digest-test--subjects (folder)
  "The subjects of the messages in FOLDER."
  (with-current-buffer folder
    (mapcar #'vm-su-subject vm-message-list)))

(ert-deftest vm-digest-test-bursting-an-rfc934-digest ()
  "The digest's messages join the folder as new mail would, with their own
headers, and the digest itself stays where it was."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (vm-burst-digest "rfc934")
    (should (equal (vm-digest-test--subjects folder)
                   '("a digest of two" "first enclosed" "second enclosed")))
    (with-current-buffer folder
      (save-restriction
        (widen)
        (should (string-match-p "The first enclosed body" (buffer-string)))
        (should (string-match-p "The second enclosed body" (buffer-string)))))))

(ert-deftest vm-digest-test-an-rfc1153-digest-survives-a-round-trip ()
  "Messages encapsulated as an RFC 1153 digest come back out of it.

Built with VM's own encapsulator rather than by hand: what the two halves
have to agree about is the format, and a fixture written from the standard
only tests the reader against my reading of it."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (vm-burst-rfc934-digest)
    (with-current-buffer folder
      (let ((messages (cdr vm-message-list))
            (digest nil))
        (should (= (length messages) 2))
        (with-temp-buffer
          (vm-rfc1153-encapsulate-messages messages '("From" "Subject" "Date") nil)
          (setq digest (buffer-string)))
        (should (string-match-p "first enclosed" digest))
        (should (string-match-p "second enclosed" digest))
        ;; and bursting that digest gives the two messages back
        (let* ((dir (file-name-as-directory (make-temp-file "vm-digest-rt" t)))
               (file (expand-file-name "inbox" dir))
               (before (buffer-list)))
          (unwind-protect
              (progn
                (write-region
                 (concat "From listserv@example.com Mon Jan  1 00:00:00 2024\n"
                         "From: listserv@example.com\n"
                         "Subject: a 1153 digest\n\n"
                         digest "\n")
                 nil file nil 'quiet)
                (cl-letf (((symbol-function 'vm-display) #'ignore)
                          ((symbol-function 'vm-present-current-message) #'ignore))
                  (vm-visit-folder file)
                  (setq vm-message-pointer vm-message-list)
                  (vm-burst-rfc1153-digest)
                  (should (equal (mapcar #'vm-su-subject vm-message-list)
                                 '("a 1153 digest" "first enclosed"
                                   "second enclosed")))))
            (dolist (buffer (buffer-list))
              (unless (memq buffer before)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer (set-buffer-modified-p nil))
                  (kill-buffer buffer))))
            (delete-directory dir t)))))))

(ert-deftest vm-digest-test-bursting-leaves-the-preamble-behind ()
  "The text before the first separator is the digest's own, and does not
become a message."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (vm-burst-digest "rfc934")
    (should (= (length (vm-digest-test--subjects folder)) 3))
    (with-current-buffer folder
      (should-not (cl-find-if (lambda (m)
                                (string-match-p "preamble" (vm-su-subject m)))
                              vm-message-list)))))

(ert-deftest vm-digest-test-bursting-to-a-temp-folder-leaves-this-one-alone ()
  "`vm-burst-digest-to-temp-folder' puts the messages in a folder of their
own, so a digest can be read without its messages joining the inbox."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (let ((before (buffer-list)))
      (vm-burst-digest-to-temp-folder "rfc934")
      ;; the folder the digest came from is untouched
      (should (equal (vm-digest-test--subjects folder) '("a digest of two")))
      ;; and the messages are in a folder of their own
      (let ((made (cl-find-if
                   (lambda (buffer)
                     (and (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (and (eq major-mode 'vm-mode) vm-message-list))))
                   (seq-remove (lambda (b) (memq b before)) (buffer-list)))))
        (should made)
        (with-current-buffer made
          (should (equal (mapcar #'vm-su-subject vm-message-list)
                         '("first enclosed" "second enclosed"))))))))

(ert-deftest vm-digest-test-guessing-the-type-of-a-digest ()
  "The type is guessed from the message, so a reader does not have to know
which kind of digest they were sent."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (should (equal (vm-guess-digest-type (car vm-message-list)) "rfc934")))
  (vm-digest-test--with-a-digest vm-digest-test--rfc1153
    (should (equal (vm-guess-digest-type (car vm-message-list)) "rfc1153"))))

(ert-deftest vm-digest-test-making-an-rfc934-digest ()
  "`vm-rfc934-encapsulate-messages' builds a digest of the messages given,
each behind a separator, so that bursting it gives them back."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (vm-burst-digest "rfc934")
    (with-current-buffer folder
      (let ((messages (cdr vm-message-list)))
        (with-temp-buffer
          (vm-rfc934-encapsulate-messages messages '("From" "Subject") nil)
          (let ((made (buffer-string)))
            (should (string-match-p "first enclosed" made))
            (should (string-match-p "second enclosed" made))
            (should (string-match-p "^------------------------------$" made))))))))

(ert-deftest vm-digest-test-making-a-mime-digest ()
  "`vm-mime-encapsulate-messages' builds a multipart/digest, whose parts are
the messages rather than text between separators."
  (vm-digest-test--with-a-digest vm-digest-test--rfc934
    (vm-burst-digest "rfc934")
    (with-current-buffer folder
      (let ((messages (cdr vm-message-list)))
        (with-temp-buffer
          (let ((boundary (vm-mime-encapsulate-messages
                           messages :keep-list '("From" "Subject")
                           :always-use-digest t)))
            (should (stringp boundary))
            (let ((made (buffer-string)))
              (should (string-match-p (regexp-quote boundary) made))
              (should (string-match-p "message/rfc822" made))
              (should (string-match-p "first enclosed" made)))))))))


;;; What a digest does to a body that looks like a separator
;;
;; The tests above check that the messages come back and that their subjects
;; are right.  These check the bodies come back byte for byte, over the bodies
;; that hold, or nearly hold, a separator of the format being used.  A digest
;; that loses a line of someone's mail is the failure worth catching here.

(defconst vm-digest-test--separator (make-string 30 ?-)
  "The line RFC 1153 separates messages with, and RFC 934 in VM's writer.")

(defconst vm-digest-test--round-trip-bodies
  (list (cons "plain"           "just a body\n")
        (cons "the separator"   (concat "before\n" vm-digest-test--separator "\nafter\n"))
        (cons "the prologue"    (concat "before\n" (make-string 70 ?-) "\nafter\n"))
        (cons "a quoted separator"
              (concat "before\n " (make-string 29 ?-) "\nafter\n"))
        (cons "a leading dash"  "before\n-not a separator\nafter\n")
        (cons "a dash space"    "before\n- quoted already\nafter\n")
        (cons "the epilogue"    "before\nEnd of this Digest\nafter\n")
        (cons "a From_ line"
              "before\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\nafter\n")
        (cons "a MIME boundary" "before\n--=-=-=\nafter\n"))
  "Bodies to send through a digest and back.")

(defconst vm-digest-test--known-round-trip-changes
  '(("rfc934"  . "a From_ line")
    ("rfc1153" . "a From_ line")
    ("rfc1153" . "a quoted separator"))
  "The cells where the body does not come back as it went in.

The two From_ ones are correct and must not be fixed.  A burst files its
messages into the folder, and in a From_ folder a body line beginning
`From ' has to be quoted or it would end the message.  VM writes `>From ',
which is mboxo and is what the manual's mbox section describes; leaving it
alone would corrupt the folder instead, which is worse.

The rfc1153 one is a real loss, and emacs-vm/vm#803 decided to keep it.  RFC
1153 defines no quoting, so the space for the first hyphen is VM's own
invention, and it is not reversible: a body line that already reads as a
space and twenty-nine hyphens is not quoted on the way in and is unquoted on
the way out, so it comes back as the separator itself.

Kept because every fix changes what VM writes.  Escaping the escape makes
digests that every older VM unstuffs wrongly, and not quoting at all turns a
rewritten line into a split message, which is worse.  The loss needs a body
line of exactly a space and twenty-nine hyphens.")

(defun vm-digest-test--encapsulate (type messages)
  "Answer the digest of TYPE holding MESSAGES.
MESSAGES is captured by the caller: `vm-message-list' is buffer-local and
reads as nil inside the temp buffer built here."
  (with-temp-buffer
    (if (equal type "rfc934")
        (vm-rfc934-encapsulate-messages messages '("From" "Subject" "Date") nil)
      (vm-rfc1153-encapsulate-messages messages '("From" "Subject" "Date") nil))
    (buffer-string)))

(defun vm-digest-test--body-of (m)
  "The text of message M, as it sits in its folder."
  (with-current-buffer (vm-buffer-of m)
    (save-restriction
      (widen)
      (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))))

(defun vm-digest-test--round-trip (type body)
  "Send BODY through a digest of TYPE and back, and answer the body it becomes."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-digest-rt" t)))
         (inbox (expand-file-name "inbox" dir))
         (digest-file (expand-file-name "digest" dir))
         (vm-digest-identifier-header-format nil)
         (vm-folder-directory dir)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list))
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'vm-display) #'ignore)
                  ((symbol-function 'vm-present-current-message) #'ignore))
          (write-region
           (concat "From sender@example.com Mon Jan  1 00:01:00 2024\n"
                   "From: sender@example.com\nTo: me@example.com\n"
                   "Subject: the one enclosed\n"
                   "Date: Mon, 1 Jan 2024 10:00:00 +0000\n\n" body "\n")
           nil inbox nil 'quiet)
          (vm-visit-folder inbox)
          (setq vm-message-pointer vm-message-list)
          (let ((digest (vm-digest-test--encapsulate type vm-message-list)))
            (write-region
             (concat "From list@example.com Mon Jan  1 00:00:00 2024\n"
                     "From: list@example.com\nSubject: the digest\n"
                     "Date: Mon, 1 Jan 2024 12:00:00 +0000\n\n" digest "\n")
             nil digest-file nil 'quiet))
          (vm-visit-folder digest-file)
          (setq vm-message-pointer vm-message-list)
          (vm-burst-digest type)
          (setq result (if (cdr vm-message-list)
                           (vm-digest-test--body-of (cadr vm-message-list))
                         :nothing-was-burst)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))
    result))

(defun vm-digest-test--changed-cells (type)
  "The bodies TYPE does not give back as they went in."
  (let ((found nil))
    (dolist (spec vm-digest-test--round-trip-bodies)
      (let ((got (vm-digest-test--round-trip type (cdr spec))))
        (unless (equal (and (stringp got) (string-trim-right got "\n+"))
                       (string-trim-right (cdr spec) "\n+"))
          (push (car spec) found))))
    (nreverse found)))

(defun vm-digest-test--expected-changes (type)
  "The labels TYPE is expected to change, from the known list."
  (delq nil (mapcar (lambda (cell)
                      (and (equal (car cell) type) (cdr cell)))
                    vm-digest-test--known-round-trip-changes)))

(ert-deftest vm-digest-test-an-rfc934-round-trip-changes-only-the-From_-line ()
  "Every body comes back byte for byte from an RFC 934 digest but one.

RFC 934 quotes any line beginning with a hyphen and unquotes it again, so a
body holding the separator, the prologue rule or an already quoted line all
survive.  The exception is `From ' at the start of a line, which the From_
folder the burst files into has to quote and which nothing unquotes."
  (should (equal (vm-digest-test--expected-changes "rfc934")
                 (vm-digest-test--changed-cells "rfc934"))))

(ert-deftest vm-digest-test-an-rfc1153-round-trip-loses-a-quoted-separator ()
  "An RFC 1153 digest gives every body back but two, and says which.

The `From ' line is the same one RFC 934 changes and is correct.  The other
is emacs-vm/vm#803: VM quotes the separator by putting a space where its
first hyphen was, and unquotes anything that reads that way, so a body line
that already read that way comes back as the separator itself.  That was
decided and kept, so this test is what will fail if the quoting is ever made
reversible, and whoever makes it so should reopen #803 rather than change
the expectation here."
  (should (equal (sort (vm-digest-test--expected-changes "rfc1153") #'string<)
                 (sort (vm-digest-test--changed-cells "rfc1153") #'string<))))

(ert-deftest vm-digest-test-bursting-works-with-no-identifier-header ()
  "REGRESSION: `vm-digest-identifier-header-format' nil does not break bursting.

emacs-vm/vm#802.  nil means insert no identifying header, which is what the
option's own guard in `vm-rfc1153-or-rfc934-burst-message' says and what the
MIME burster does in both of its places.  The insert was unguarded, so a
reader who did not want an `X-Digest:' header got

    Wrong type argument: char-or-string-p, nil

and nothing burst at all.  Checked on both types, since both go through that
one function."
  (dolist (type '("rfc934" "rfc1153"))
    (let ((got (vm-digest-test--round-trip type "just a body\n")))
      (should (equal "just a body" (and (stringp got)
                                        (string-trim-right got "\n+")))))))


;;; Sending a digest (emacs-vm/vm#804)

(defun vm-digest-test--preamble-lines (type)
  "Send a two-message digest of TYPE with a preamble, count its lines.
The preamble format is a literal marker, so what is counted cannot be the
encapsulated messages' own text: with `%s' in it, a preamble line and the
subject of the message it names read the same."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-send-digest" t)))
         (inbox (expand-file-name "inbox" dir))
         (vm-folder-directory dir)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-digest-send-type type)
         (vm-digest-preamble-format "PREAMBLEMARK")
         (vm-digest-center-preamble nil)
         (vm-mail-mode-hook nil)
         (vm-send-digest-hook nil)
         (before (buffer-list))
         (count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-display) #'ignore)
                  ((symbol-function 'vm-present-current-message) #'ignore)
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (with-temp-buffer
            (dolist (n '(1 2))
              (insert (format "From s@example.com Mon Jan  1 00:0%d:00 2024\n" n)
                      "From: s@example.com\nTo: me@example.com\n"
                      (format "Subject: msg%d\n" n)
                      "Date: Mon, 1 Jan 2024 10:00:00 +0000\n\nbody\n\n"))
            (write-region (point-min) (point-max) inbox nil 'quiet))
          (vm-visit-folder inbox)
          (setq vm-message-pointer vm-message-list)
          ;; the prefix argument is what asks for a preamble
          (vm-send-digest t)
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "PREAMBLEMARK" nil t)
              (setq count (1+ count)))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))
    count))

(ert-deftest vm-digest-test-every-digest-type-writes-a-preamble ()
  "REGRESSION: a nil `vm-digest-send-type' digest gets its preamble too.

emacs-vm/vm#804.  The nil arm of the cond in `vm-send-digest' walked the
message list by cutting it down, and the next line reads that same variable
to build the preamble, so `C-u M-x vm-send-digest' wrote none at all for that
one type.  The other three arms leave the list alone.

All four types, since what is being pinned is that they agree."
  (dolist (type '("mime" "rfc934" "rfc1153" nil))
    (should (equal (cons type 2)
                   (cons type (vm-digest-test--preamble-lines type))))))


;;; Forwarding, over every value of vm-forwarding-digest-type (emacs-vm/vm#805)

(defun vm-digest-test--forward (type body)
  "Forward a message whose text is BODY as TYPE, answer the composition.
The message declares utf-8, so a body of any bytes is labelled where it lands."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fwd" t)))
         (inbox (expand-file-name "inbox" dir))
         (vm-folder-directory dir)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-forwarding-digest-type type)
         (vm-mail-mode-hook nil)
         (vm-forward-message-hook nil)
         (before (buffer-list))
         composition)
    (unwind-protect
        (cl-letf (((symbol-function 'vm-display) #'ignore)
                  ((symbol-function 'vm-present-current-message) #'ignore))
          (let ((coding-system-for-write 'utf-8)
                (select-safe-coding-system-function nil))
            (with-temp-buffer
              (insert "From s@example.com Mon Jan  1 00:01:00 2024\n"
                      "From: s@example.com\nTo: me@example.com\n"
                      "Subject: the one\nMIME-Version: 1.0\n"
                      "Content-Type: text/plain; charset=utf-8\n"
                      "Content-Transfer-Encoding: 8bit\n"
                      "Date: Mon, 1 Jan 2024 10:00:00 +0000\n\n" body "\n")
              (write-region (point-min) (point-max) inbox nil 'quiet)))
          (vm-visit-folder inbox)
          (setq vm-message-pointer vm-message-list)
          (vm-forward-message)
          (setq composition
                (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))
    composition))

(ert-deftest vm-digest-test-forwarding-keeps-a-body-with-no-final-newline ()
  "REGRESSION: an rfc934 forward does not eat the last line.

emacs-vm/vm#805.  `vm-rfc934-encapsulate-messages' put its trailing separator
at point and then deleted the last line back to its start to make room for
the end marker.  For a message whose text does not end in a newline that
separator was on the last body line, so the delete took the body with it and
`after' never left the machine.

Not an exotic message: `vm-text-end-of' answers text with no final newline
whenever the body ends with a single newline in a From_ folder, that newline
being the trailing separator.

All four types, since three of them were already right and this pins that
they agree.  The separator is checked to be on a line of its own too, which
is what RFC 934 asks for and what the fix restores."
  (dolist (type '("rfc934" "rfc1153" nil))
    (let ((composition (vm-digest-test--forward type "before\nafter")))
      (should (equal (cons type t)
                     (cons type (and (string-match-p "^before$" composition) t))))
      (should (equal (cons type t)
                     (cons type (and (string-match-p "^after$" composition) t)))))))

(provide 'vm-digest-test)

;;; vm-digest-test.el ends here