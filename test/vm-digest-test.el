;;; vm-digest-test.el --- Tests for vm-digest.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

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

(provide 'vm-digest-test)

;;; vm-digest-test.el ends here