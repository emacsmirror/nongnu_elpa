;;; vm-icalendar-test.el --- Tests for vm-icalendar.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; A meeting invitation arrives as a text/calendar part, and VM showed the
;; iCalendar object as it stands.  These cover reading one and saying what it
;; means (#85), the forms real senders produce, and the import into the diary.

;;; Code:

(require 'vm-test-init)
(require 'vm)
(require 'vm-icalendar)

(defconst vm-icalendar-test--invitation
  (concat "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nMETHOD:REQUEST\r\n"
          "BEGIN:VEVENT\r\nUID:abc123\r\n"
          "DTSTART;TZID=Europe/London:20260810T140000\r\n"
          "DTEND;TZID=Europe/London:20260810T150000\r\n"
          "SUMMARY:Quarterly review\r\n"
          "LOCATION:Room 3\\, second floor\r\n"
          "ORGANIZER;CN=\"Alice Example\":mailto:alice@example.com\r\n"
          "ATTENDEE;CN=Bob;PARTSTAT=ACCEPTED:mailto:bob@example.com\r\n"
          "ATTENDEE;CN=Carol;PARTSTAT=NEEDS-ACTION:mailto:carol@example.com\r\n"
          "DESCRIPTION:Agenda:\\n1. IMAP\\n2. Anything else\r\n"
          "END:VEVENT\r\nEND:VCALENDAR\r\n")
  "An invitation of the kind Exchange and Google both send.")

;;; Reading the object

(ert-deftest vm-icalendar-test-unfolds-long-lines ()
  "A line split across two is one line again.
iCalendar folds at 75 octets by inserting CRLF and a space, and the
continuation belongs to the line before it."
  ;; The whitespace that starts the continuation belongs to the folding and
  ;; goes with it -- a sender folds at 75 octets wherever that falls, and
  ;; "one two" would be wrong for a line split in the middle of a word.
  (should (equal (vm-icalendar-unfold "SUMMARY:one\r\n two\r\nDTSTART:x\r\n")
                 "SUMMARY:onetwo\r\nDTSTART:x\r\n"))
  ;; a tab continues a line too, and a bare LF is folded the same way
  (should (equal (vm-icalendar-unfold "A:one\n\tmore\n") "A:onemore\n"))
  ;; a space the sender meant survives, being the second one
  (should (equal (vm-icalendar-unfold "A:one\r\n  two\r\n") "A:one two\r\n")))

(ert-deftest vm-icalendar-test-unescapes-text ()
  "Commas, semicolons and backslashes arrive escaped, and \\n is a break."
  (should (equal (vm-icalendar-unescape "Room 3\\, second floor")
                 "Room 3, second floor"))
  (should (equal (vm-icalendar-unescape "one\\ntwo") "one\ntwo"))
  (should (equal (vm-icalendar-unescape "a\\;b\\\\c") "a;b\\c")))

(ert-deftest vm-icalendar-test-parses-properties-and-parameters ()
  "Each line becomes name, parameters and value."
  (let ((properties (vm-icalendar-parse vm-icalendar-test--invitation)))
    (should (equal "REQUEST" (vm-icalendar-value properties "METHOD")))
    (should (equal "Quarterly review" (vm-icalendar-value properties "SUMMARY")))
    (should (equal "Room 3, second floor"
                   (vm-icalendar-value properties "LOCATION")))
    (should (equal ";TZID=Europe/London"
                   (vm-icalendar-parameters properties "DTSTART")))
    (should (equal 2 (length (vm-icalendar-values properties "ATTENDEE"))))))

(ert-deftest vm-icalendar-test-property-names-are-case-insensitive ()
  "A sender may write them in any case; RFC 5545 says they are the same."
  (let ((properties (vm-icalendar-parse "begin:VCALENDAR\r\nsummary:hello\r\n")))
    (should (equal "hello" (vm-icalendar-value properties "SUMMARY")))))

;;; Saying it in words

(ert-deftest vm-icalendar-test-says-what-the-invitation-says ()
  "The rendering answers what, when, where and who."
  (let ((text (vm-icalendar-format vm-icalendar-test--invitation)))
    (should (string-match-p "\\`Invitation" text))
    (should (string-match-p "Quarterly review" text))
    (should (string-match-p "Monday 10 August 2026 at 14:00" text))
    (should (string-match-p "until Monday 10 August 2026 at 15:00" text))
    (should (string-match-p "Where: Room 3, second floor" text))
    (should (string-match-p "Organizer: Alice Example <alice@example.com>" text))
    (should (string-match-p "Bob <bob@example.com> -- accepted" text))
    (should (string-match-p "Carol <carol@example.com> -- not answered" text))
    (should (string-match-p "1. IMAP" text))
    ;; and none of the object's own furniture
    (should-not (string-match-p "BEGIN:VCALENDAR\\|DTSTART;TZID" text))))

(ert-deftest vm-icalendar-test-names-the-method ()
  "REQUEST, REPLY and CANCEL are not words a reader should have to know."
  (dolist (case '(("REQUEST" . "Invitation")
                  ("CANCEL" . "Cancellation")
                  ("REPLY" . "Reply to an invitation")
                  ("PUBLISH" . "Announcement")))
    (let ((text (vm-icalendar-format
                 (format "BEGIN:VCALENDAR\r\nMETHOD:%s\r\nSUMMARY:x\r\n"
                         (car case)))))
      (should (string-prefix-p (cdr case) text))))
  ;; an unknown method is still shown, rather than swallowed
  (should (string-match-p "SOMETHINGELSE"
                          (vm-icalendar-format
                           "BEGIN:VCALENDAR\r\nMETHOD:SOMETHINGELSE\r\n"))))

(ert-deftest vm-icalendar-test-a-date-without-a-time ()
  "An all-day event has a date and no time."
  (let ((text (vm-icalendar-format
               "BEGIN:VCALENDAR\r\nMETHOD:PUBLISH\r\nDTSTART:20260811\r\n")))
    (should (string-match-p "Tuesday 11 August 2026" text))
    (should-not (string-match-p "at [0-9][0-9]:" text))))

(ert-deftest vm-icalendar-test-a-time-in-utc-says-so ()
  "A trailing Z means UTC, and the reader is told rather than misled."
  (let ((text (vm-icalendar-format
               "BEGIN:VCALENDAR\r\nDTSTART:20260811T090000Z\r\n")))
    (should (string-match-p "at 09:00 UTC" text))))

(ert-deftest vm-icalendar-test-an-unreadable-time-is-shown-as-it-came ()
  "A value this does not understand is shown, not dropped."
  (should (string-match-p "whenever"
                          (vm-icalendar-format
                           "BEGIN:VCALENDAR\r\nDTSTART:whenever\r\n"))))

(ert-deftest vm-icalendar-test-organizer-without-a-name ()
  "An ORGANIZER with no CN is just the address, with no empty angle brackets."
  (let ((text (vm-icalendar-format
               "BEGIN:VCALENDAR\r\nORGANIZER:mailto:alice@example.com\r\n")))
    (should (string-match-p "Organizer: alice@example.com" text))
    (should-not (string-match-p "<" text))))

(ert-deftest vm-icalendar-test-long-description-is-trimmed ()
  "The dial-in boilerplate is cut off, and says how much was cut."
  (let* ((body (mapconcat (lambda (n) (format "line %d" n))
                          (number-sequence 1 30) "\\n"))
         (vm-icalendar-description-lines 3)
         (text (vm-icalendar-format
                (format "BEGIN:VCALENDAR\r\nDESCRIPTION:%s\r\n" body))))
    (should (string-match-p "line 3" text))
    (should-not (string-match-p "line 4\\b" text))
    (should (string-match-p "27 more lines" text)))
  ;; and it can be turned off altogether
  (let ((vm-icalendar-show-description nil))
    (should-not (string-match-p
                 "secret"
                 (vm-icalendar-format
                  "BEGIN:VCALENDAR\r\nDESCRIPTION:secret\r\n")))))

;;; In a message

(defconst vm-icalendar-test--message
  (concat "From alice@example.com  Mon Aug 10 09:00:00 2026\n"
          "From: alice@example.com\nTo: b@example.com\n"
          "Subject: Quarterly review\nMIME-Version: 1.0\n"
          "Content-Type: text/calendar; method=REQUEST; charset=UTF-8\n\n"
          (replace-regexp-in-string "\r" "" vm-icalendar-test--invitation)
          "\n")
  "A message that is nothing but an invitation, as Exchange sends.")

(ert-deftest vm-icalendar-test-a-calendar-part-is-shown-as-words ()
  "Displaying the part puts the invitation in the buffer, not the object."
  (vm-test-with-folder vm-icalendar-test--message
    (let* ((message (car vm-message-pointer))
           (layout (vm-mm-layout message)))
      (should (vectorp layout))
      (should (vm-mime-types-match "text/calendar"
                                   (car (vm-mm-layout-type layout))))
      (with-temp-buffer
        (should (vm-mime-display-internal-text/calendar layout))
        (let ((shown (buffer-string)))
          (should (string-match-p "Invitation" shown))
          (should (string-match-p "Quarterly review" shown))
          (should-not (string-match-p "BEGIN:VCALENDAR" shown)))))))

(ert-deftest vm-icalendar-test-the-handler-is-what-vm-looks-for ()
  "VM dispatches on the type, so the name is the wiring.
`vm-mime-display-internal' builds `vm-mime-display-internal-<type>', and
older senders use text/x-vcalendar."
  (should (fboundp 'vm-mime-display-internal-text/calendar))
  (should (fboundp 'vm-mime-display-internal-text/x-vcalendar))
  (should (eq (vm-mime-handler "display-internal" "text/calendar")
              'vm-mime-display-internal-text/calendar)))

(ert-deftest vm-icalendar-test-finds-the-part-in-a-multipart ()
  "The calendar part of a message is found at any depth.
Real invitations arrive as multipart/alternative: a paragraph of text, an
HTML version of it, and the object."
  (with-temp-buffer
    (insert "Content-Type: multipart/alternative; boundary=b\n\n"
            "--b\nContent-Type: text/plain\n\nAlice invites you\n"
            "--b\nContent-Type: text/calendar; method=REQUEST\n\n"
            (replace-regexp-in-string "\r" "" vm-icalendar-test--invitation)
            "--b--\n")
    (let* ((layout (vm-mime-parse-entity nil :default-type '("text/plain")
                                         :default-encoding "7bit"))
           (found (vm-icalendar-part-of layout)))
      (should found)
      (should (vm-mime-types-match "text/calendar"
                                   (car (vm-mm-layout-type found)))))))

(ert-deftest vm-icalendar-test-no-calendar-part-is-not-an-error-to-find ()
  (with-temp-buffer
    (insert "Content-Type: text/plain\n\njust a message\n")
    (let ((layout (vm-mime-parse-entity nil :default-type '("text/plain")
                                        :default-encoding "7bit")))
      (should-not (vm-icalendar-part-of layout)))))

(ert-deftest vm-icalendar-test-import-adds-to-a-diary-file ()
  "`vm-icalendar-import' hands the part to Emacs's icalendar.el.
The diary file is written by icalendar.el, so what this checks is that VM
gives it the part and that the event arrives."
  (require 'icalendar)
  (let* ((dir (file-name-as-directory (make-temp-file "vm-ical" t)))
         (diary (expand-file-name "diary" dir))
         (before (buffer-list)))
    (unwind-protect
        (vm-test-with-folder vm-icalendar-test--message
          ;; `vm-icalendar-import' is the command, which asks the folder for
          ;; its current message; this is the half that takes a message.
          (vm-icalendar-import-message (car vm-message-pointer) diary)
          (should (file-exists-p diary))
          (with-temp-buffer
            (insert-file-contents diary)
            (should (string-match-p "Quarterly review" (buffer-string)))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-icalendar-test-import-says-when-there-is-nothing-to-import ()
  (let ((text-quoting-style 'grave))
    (vm-test-with-folder (concat "From a@example.com  Mon Aug 10 09:00:00 2026\n"
                                 "From: a@example.com\nSubject: plain\n\ntext\n")
      (should (equal (should-error
                      (vm-icalendar-import-message (car vm-message-pointer)))
                     '(error "This message has no calendar part"))))))

;;; Which importer is called, and what its answer means (emacs-vm/vm#858)
;;
;; `icalendar-import-buffer' is obsolete from Emacs 31.1, the release
;; `diary-icalendar-import-buffer' arrived in, and VM runs on 28.1, so both
;; are called and the new one first.  They report differently: the old one
;; answers t when it imported, the new one ends in `save-buffer' and answers
;; nil whatever happened, raising instead when it cannot read the text.

(ert-deftest vm-icalendar-test-import-asks-for-the-name-emacs-still-has ()
  "REGRESSION: the new name where Emacs has it, the old one where it has not."
  (require 'icalendar)
  (let ((called nil))
    (cl-letf (((symbol-function 'icalendar-import-buffer)
               (lambda (&rest _) (push 'old called) t)))
      (if (fboundp 'diary-icalendar-import-buffer)
          (cl-letf (((symbol-function 'diary-icalendar-import-buffer)
                     (lambda (&rest _) (push 'new called) nil)))
            (with-temp-buffer (vm-icalendar-import-into-diary "/dev/null"))
            (should (equal called '(new))))
        (with-temp-buffer (vm-icalendar-import-into-diary "/dev/null"))
        (should (equal called '(old)))))))

(ert-deftest vm-icalendar-test-a-nil-answer-from-the-new-name-is-not-a-failure ()
  "REGRESSION: `diary-icalendar-import-buffer' answers nil when it worked.
Reading that as failure reported `icalendar could not import this part' on
every successful import."
  (skip-unless (fboundp 'diary-icalendar-import-buffer))
  (let ((called nil))
    (cl-letf (((symbol-function 'diary-icalendar-import-buffer)
               (lambda (&rest _) (setq called t) nil)))
      (with-temp-buffer (vm-icalendar-import-into-diary "/dev/null"))
      (should called))))

(ert-deftest vm-icalendar-test-a-nil-answer-from-the-old-name-is-a-failure ()
  "Where only the old name exists, nil still means it did not import."
  (require 'icalendar)
  (let ((text-quoting-style 'grave))
    (cl-letf (((symbol-function 'diary-icalendar-import-buffer) nil)
              ((symbol-function 'icalendar-import-buffer) (lambda (&rest _) nil)))
      (should-not (fboundp 'diary-icalendar-import-buffer))
      (should (equal (should-error
                      (with-temp-buffer
                        (vm-icalendar-import-into-diary "/dev/null")))
                     '(error "icalendar could not import this part"))))))

(provide 'vm-icalendar-test)

;;; vm-icalendar-test.el ends here
