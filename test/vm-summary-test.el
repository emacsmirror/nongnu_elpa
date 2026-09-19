;;; vm-summary-test.el --- Tests for vm-summary.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM summary functions in vm-summary.el
;; Tests pure string manipulation and formatting functions.

;;; Code:

(require 'vm-test-init)
(require 'vm-summary)

;;; vm-string-width tests

(ert-deftest vm-summary-test-string-width-ascii ()
  "Test string width for ASCII strings."
  (should (= (vm-string-width "hello") 5))
  (should (= (vm-string-width "") 0))
  (should (= (vm-string-width "a") 1)))

(ert-deftest vm-summary-test-string-width-spaces ()
  "Test string width with spaces and tabs."
  (should (= (vm-string-width "a b") 3))
  (should (= (vm-string-width "   ") 3)))

;;; vm-left-justify-string tests

(ert-deftest vm-summary-test-left-justify-shorter ()
  "Test left justifying a string shorter than width."
  (should (equal (vm-left-justify-string "hi" 5) "hi   ")))

(ert-deftest vm-summary-test-left-justify-equal ()
  "Test left justifying a string equal to width."
  (should (equal (vm-left-justify-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-left-justify-longer ()
  "Test left justifying a string longer than width (no truncation)."
  (should (equal (vm-left-justify-string "hello world" 5) "hello world")))

;;; vm-right-justify-string tests

(ert-deftest vm-summary-test-right-justify-shorter ()
  "Test right justifying a string shorter than width."
  (should (equal (vm-right-justify-string "hi" 5) "   hi")))

(ert-deftest vm-summary-test-right-justify-equal ()
  "Test right justifying a string equal to width."
  (should (equal (vm-right-justify-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-right-justify-longer ()
  "Test right justifying a string longer than width (no truncation)."
  (should (equal (vm-right-justify-string "hello world" 5) "hello world")))

;;; vm-numeric-right-justify-string tests

(ert-deftest vm-summary-test-numeric-right-justify-shorter ()
  "Test numeric right justify pads with zeros."
  (should (equal (vm-numeric-right-justify-string "42" 5) "00042")))

(ert-deftest vm-summary-test-numeric-right-justify-equal ()
  "Test numeric right justify with equal length."
  (should (equal (vm-numeric-right-justify-string "12345" 5) "12345")))

;;; vm-truncate-roman-string tests

(ert-deftest vm-summary-test-truncate-roman-shorter ()
  "Test truncating a string shorter than width."
  (should (equal (vm-truncate-roman-string "hi" 5) "hi")))

(ert-deftest vm-summary-test-truncate-roman-exact ()
  "Test truncating a string exactly at width."
  (should (equal (vm-truncate-roman-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-truncate-roman-longer ()
  "Test truncating a string longer than width."
  (should (equal (vm-truncate-roman-string "hello world" 5) "hello")))

(ert-deftest vm-summary-test-truncate-roman-negative ()
  "Test truncating from end with negative width."
  (should (equal (vm-truncate-roman-string "hello world" -5) "world")))

;;; vm-truncate-string tests

(ert-deftest vm-summary-test-truncate-string-basic ()
  "Test basic string truncation."
  (should (equal (vm-truncate-string "hello" 3) "hel")))

(ert-deftest vm-summary-test-truncate-string-no-change ()
  "Test truncation when string is short enough."
  (should (equal (vm-truncate-string "hi" 10) "hi")))

(ert-deftest vm-summary-test-truncate-string-negative ()
  "A negative width keeps the last columns."
  (should (equal (vm-truncate-string "hello world" -5) "world")))

(ert-deftest vm-summary-test-truncate-string-wide-fits-the-width ()
  "A double-width character that would cross the limit is dropped, not kept.
Keeping it made a %-3s field three columns wide come back four, so every
column after it in the summary line was out by one."
  (should (equal (string-width (vm-truncate-string "あああ" 3)) 2))
  (should (equal (vm-truncate-string "あああ" 3) "あ")))

(ert-deftest vm-summary-test-truncate-string-wide-from-the-end ()
  "The same when counting from the end."
  (should (equal (string-width (vm-truncate-string "あああ" -3)) 2))
  (should (equal (vm-truncate-string "あああ" -3) "あ")))

(ert-deftest vm-summary-test-truncate-string-wide-on-a-boundary ()
  "A width that a double-width character lands on exactly keeps it."
  (should (equal (vm-truncate-string "あああ" 4) "ああ"))
  (should (equal (vm-truncate-string "あああ" -4) "ああ")))

;;; vm-default-chop-full-name tests

(ert-deftest vm-summary-test-chop-name-angle-brackets ()
  "Test chopping name with angle bracket format."
  (let ((result (vm-default-chop-full-name "John Doe <john@example.com>")))
    (should (equal (car result) "John Doe"))
    (should (equal (cadr result) "john@example.com"))))

(ert-deftest vm-summary-test-chop-name-parens ()
  "Test chopping name with parenthesis format."
  (let ((result (vm-default-chop-full-name "john@example.com (John Doe)")))
    (should (equal (car result) "John Doe"))
    (should (equal (cadr result) "john@example.com"))))

(ert-deftest vm-summary-test-chop-name-plain-email ()
  "Test chopping plain email address."
  (let ((result (vm-default-chop-full-name "john@example.com")))
    ;; Plain email should have nil for full-name
    (should (or (null (car result))
                (equal (car result) "john@example.com")))))

(ert-deftest vm-summary-test-chop-name-quoted ()
  "Test chopping name with quoted string."
  (let ((result (vm-default-chop-full-name "\"John Q. Doe\" <john@example.com>")))
    (should (stringp (car result)))
    (should (equal (cadr result) "john@example.com"))))

;;; vm-su-trim-subject tests (when stripping is disabled)

(ert-deftest vm-summary-test-trim-subject-passthrough ()
  "Test subject trimming when stripping is disabled."
  (let ((vm-summary-strip-subject-tags nil)
        (vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil))
    ;; With stripping disabled, should return subject as-is
    (should (equal (vm-su-trim-subject "Hello World") "Hello World"))))

(ert-deftest vm-summary-test-trim-subject-with-re ()
  "Test subject trimming with Re: prefix."
  (let ((vm-summary-strip-subject-tags t)
        (vm-subject-ignored-prefix "^\\(re: *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-tag-prefix-exceptions nil))
    (let ((result (vm-su-trim-subject "Re: Hello World")))
      (should (stringp result))
      ;; The Re: is moved to prefix but kept
      (should (string-match "Hello World" result)))))

;;; High-level header content extraction (requires message structure)
;; These tests verify the infrastructure but don't create full messages

(ert-deftest vm-summary-test-infrastructure-loaded ()
  "Test that summary functions are loaded."
  (should (fboundp 'vm-string-width))
  (should (fboundp 'vm-left-justify-string))
  (should (fboundp 'vm-right-justify-string))
  (should (fboundp 'vm-truncate-string))
  (should (fboundp 'vm-default-chop-full-name))
  (should (fboundp 'vm-su-trim-subject)))

;;; Integration tests using real messages

(defconst vm-summary-test-folder
  "From sender@example.com Mon Jan  1 00:00:00 2024
From: John Doe <john@example.com>
To: recipient@example.com
Cc: cc@example.com
Subject: Test Message
Date: Mon, 01 Jan 2024 10:30:45 +0000
Message-ID: <msg1@example.com>

This is the message body with some text.
Second line here.

"
  "Test folder for summary tests.")

;;; vm-su-from tests

(ert-deftest vm-summary-test-su-from ()
  "Test vm-su-from extracts sender address."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((from (vm-su-from msg)))
        (should (stringp from))
        (should (string-match "john" from))))))

;;; vm-su-decoded-full-name tests

(ert-deftest vm-summary-test-su-decoded-full-name ()
  "Test vm-su-decoded-full-name extracts full name."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((name (vm-su-decoded-full-name msg)))
        (should (or (null name) (stringp name)))))))

;;; vm-su-decoded-subject tests

(ert-deftest vm-summary-test-su-decoded-subject ()
  "Test vm-su-decoded-subject extracts subject."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((subject (vm-su-decoded-subject msg)))
        (should (stringp subject))
        (should (string-match "Test Message" subject))))))

;;; vm-su-decoded-to tests

(ert-deftest vm-summary-test-su-decoded-to ()
  "Test vm-su-decoded-to extracts recipients."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((to (vm-su-decoded-to msg)))
        (should (stringp to))
        (should (string-match "recipient" to))))))

;;; vm-su-message-id tests

(ert-deftest vm-summary-test-su-message-id ()
  "Test vm-su-message-id extracts message ID."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((msg-id (vm-su-message-id msg)))
        (should (stringp msg-id))
        (should (string-match "msg1@example.com" msg-id))))))

;;; Date component tests

(ert-deftest vm-summary-test-su-weekday ()
  "Test vm-su-weekday extracts day of week."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((weekday (vm-su-weekday msg)))
        (should (stringp weekday))
        (should (string-match "Mon" weekday))))))

(ert-deftest vm-summary-test-su-monthday ()
  "Test vm-su-monthday extracts day of month."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((monthday (vm-su-monthday msg)))
        (should (stringp monthday))
        (should (string-match "0?1" monthday))))))

(ert-deftest vm-summary-test-su-month ()
  "Test vm-su-month extracts month."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((month (vm-su-month msg)))
        (should (stringp month))
        (should (string-match "Jan" month))))))

(ert-deftest vm-summary-test-su-year ()
  "Test vm-su-year extracts year."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((year (vm-su-year msg)))
        (should (stringp year))
        (should (string-match "2024" year))))))

(ert-deftest vm-summary-test-su-hour ()
  "Test vm-su-hour extracts time."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((hour (vm-su-hour msg)))
        (should (stringp hour))
        (should (string-match "10:30" hour))))))

(ert-deftest vm-summary-test-su-zone ()
  "Test vm-su-zone extracts timezone."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((zone (vm-su-zone msg)))
        (should (stringp zone))))))

;;; vm-su-mark tests

(ert-deftest vm-summary-test-su-mark-unmarked ()
  "Test vm-su-mark returns empty for unmarked message."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg nil)
      (let ((mark (vm-su-mark msg)))
        (should (stringp mark))
        (should (string= mark " "))))))

(ert-deftest vm-summary-test-su-mark-marked ()
  "Test vm-su-mark returns indicator for marked message."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg t)
      (let ((mark (vm-su-mark msg)))
        (should (stringp mark))
        (should (not (string= mark " ")))))))

;;; vm-su-line-count tests

(ert-deftest vm-summary-test-su-line-count ()
  "Test vm-su-line-count returns line count string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((lines (vm-su-line-count msg)))
        (should (stringp lines))))))

;;; vm-su-byte-count tests

(ert-deftest vm-summary-test-su-byte-count ()
  "Test vm-su-byte-count returns byte count string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((bytes (vm-su-byte-count msg)))
        (should (stringp bytes))))))

;;; vm-su-attribute-indicators tests

(ert-deftest vm-summary-test-su-attribute-indicators ()
  "Test vm-su-attribute-indicators returns indicator string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indicators (vm-su-attribute-indicators msg)))
        (should (stringp indicators))))))

(ert-deftest vm-summary-test-su-attribute-indicators-short ()
  "Test vm-su-attribute-indicators-short returns short indicators."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indicators (vm-su-attribute-indicators-short msg)))
        (should (stringp indicators))))))

(ert-deftest vm-summary-test-su-attribute-indicators-long ()
  "Test vm-su-attribute-indicators-long returns long indicators."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indicators (vm-su-attribute-indicators-long msg)))
        (should (stringp indicators))))))

(defconst vm-summary-test--indicators
  '((deleted       "D   " "D      " "D")
    (new           "N   " "N      " "N")
    (unread        "U   " "U      " "U")
    (flagged       "!   " "!      " "!")
    (replied       "  R " " r     " " ")
    (forwarded     "  Z " "  z    " " ")
    (redistributed "  B " "   b   " " ")
    (filed         " F  " "    f  " " ")
    (written       " W  " "     w " " ")
    (edited        "   E" "      e" " "))
  "Each attribute and the %a, %A and %b indicators it produces on its own.
This is the table the manual\='s summary-format section prints, so a change
here is a change the manual has to make too.")

(ert-deftest vm-summary-test-each-attribute-has-its-documented-indicator ()
  "Every attribute shows the character in the column the manual gives it.

The manual had the seven-wide %A in upper case, where the code writes
`r', `z', `b', `f' and `w' in lower, and left `B' for a redistributed
message out of the four-wide %a altogether."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      (dolist (case vm-summary-test--indicators)
        (dolist (flag vm-summary-test--indicators)
          (funcall (intern (format "vm-set-%s-flag-of" (car flag)))
                   message nil))
        (funcall (intern (format "vm-set-%s-flag-of" (car case))) message t)
        (should (equal (list (car case)
                             (vm-su-attribute-indicators message)
                             (vm-su-attribute-indicators-long message)
                             (vm-su-attribute-indicators-short message))
                       case))))))

(ert-deftest vm-summary-test-r-and-R-are-the-recipients-including-cc ()
  "%r and %R name the To and the Cc, where %t and %T name the To alone.

Both were left in the summary as the literal text \"%r\": the regexp that
finds a specifier had no r or R in it, while the `cond\=' it feeds has had
branches for both calling `vm-su-to-cc\=' and `vm-su-to-cc-names\=' all along
(emacs-vm/vm#846).  The docstring of `vm-summary-format\=' and the manual
have documented them throughout."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      (should (equal (vm-summary-sprintf "%t" message) "recipient@example.com"))
      (should (equal (vm-summary-sprintf "%r" message)
                     "recipient@example.com, cc@example.com"))
      (should (equal (vm-summary-sprintf "%R" message)
                     "recipient@example.com, cc@example.com")))))

(ert-deftest vm-summary-test-r-and-R-compile-in-the-tokenized-format-too ()
  "The tokenized summary is where a folder\='s own lines come from.
Both paths go through the same compiler, so a specifier that works in one
and not the other would be a second bug, not this one."
  (vm-test-with-folder vm-summary-test-folder
    (let* ((message (car vm-message-list))
           (tokens (vm-summary-sprintf "%r|%R" message t))
           (text (mapconcat (lambda (token) (if (stringp token) token ""))
                            (flatten-tree tokens) "")))
      (should (string-match-p "recipient@example.com, cc@example.com|"
                              text)))))

(ert-deftest vm-summary-test-a-doubled-percent-is-a-single-percent ()
  "%% is one % wherever it appears, which is what the docstring promises.

It was one only in a format that had something else to substitute.  With
nothing else the compiler hands back its `format\=' control string without
calling `format\=' on it, so the doubling stayed in and a summary format of
\"100%% done\" summarised as \"100%% done\" (emacs-vm/vm#847)."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      (should (equal (vm-summary-sprintf "%%" message) "%"))
      (should (equal (vm-summary-sprintf "100%% done" message) "100% done"))
      (should (equal (vm-summary-sprintf "%%s" message) "%s"))
      ;; the case that always worked, alongside the ones that did not
      (should (equal (vm-summary-sprintf "%s %%" message) "Test Message %")))))

(ert-deftest vm-summary-test-an-unknown-specifier-is-left-as-it-stands ()
  "A specifier VM does not know is text, and does not break the other lines.

The text between the specifiers is copied into the `format\=' control string,
so a %q beside a live specifier reached `format\=' as a conversion of its own
and every summary line failed with \"Not enough arguments for format
string\".  A typo in `vm-summary-format\=' should cost the reader the typo,
not the summary."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      (should (equal (vm-summary-sprintf "%q" message) "%q"))
      (should (equal (vm-summary-sprintf "%s %q" message) "Test Message %q"))
      (should (equal (vm-summary-sprintf "%s %q" message t)
                     '("Test Message %q"))))))

(ert-deftest vm-summary-test-a-width-and-a-maximum-work-as-printf-does ()
  "The maximum cuts the substitution, the width pads what is left.

That is printf\='s order, and it was the other way round: the maximum was
applied to the padded text, so \"%20.4s\" answered four spaces and lost the
subject altogether (emacs-vm/vm#848).  A format writing the two with the
same number, as the default \"%-17.17F\" does, answers the same either way,
which is why nothing noticed."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      ;; subject "Test Message", line count 2, full name "John Doe"
      (should (equal (vm-summary-sprintf "%20s" message)
                     "        Test Message"))
      (should (equal (vm-summary-sprintf "%-20s" message)
                     "Test Message        "))
      (should (equal (vm-summary-sprintf "%.4s" message) "Test"))
      (should (equal (vm-summary-sprintf "%.-4s" message) "sage"))
      (should (equal (vm-summary-sprintf "%20.4s" message)
                     "                Test"))
      (should (equal (vm-summary-sprintf "%-20.4s" message)
                     "Test                "))
      (should (equal (vm-summary-sprintf "%20.-4s" message)
                     "                sage"))
      (should (equal (vm-summary-sprintf "%-17.17F" message)
                     "John Doe         ")))))

(ert-deftest vm-summary-test-a-zero-width-fills-a-number-and-not-a-word ()
  "A width beginning with 0 fills with zeros where the substitution is a
number, and with spaces everywhere else.

`%010w' used to answer \"0000Monday\", and a left-justified `%-05l' answered
\"20000\" for a line count of 2, which reads as a different number
(emacs-vm/vm#848).  `vm-summary-number-specifiers\=' is the set that a zero
fill means anything for."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      (should (equal (vm-summary-sprintf "%05l" message) "00002"))
      (should (equal (vm-summary-sprintf "%05c" message) "00059"))
      (should (equal (vm-summary-sprintf "%05y" message) "02024"))
      (should (equal (vm-summary-sprintf "%5l" message) "    2"))
      ;; a `-' beats the `0'
      (should (equal (vm-summary-sprintf "%-05l" message) "2    "))
      ;; and a word is padded with spaces whatever the width says
      (should (equal (vm-summary-sprintf "%010w" message) "    Monday"))
      (should (equal (vm-summary-sprintf "%010F" message) "  John Doe")))))

(ert-deftest vm-summary-test-a-group-takes-a-width-the-same-way ()
  "A group is padded and cut like any other substitution, in both paths.

The tokenized path is where a folder\='s own summary lines come from, and it
did the padding in the buffer in the other order: \"%-20.4(%s%)\" came out as
the whole subject there and as four columns from the compiled path, neither
of which is a column twenty wide (emacs-vm/vm#848)."
  (vm-test-with-folder vm-summary-test-folder
    (let ((message (car vm-message-list)))
      (should (equal (vm-summary-sprintf "%20.4(%s%)" message)
                     "                Test"))
      (should (equal (vm-summary-sprintf "%-20.4(%s%)" message)
                     "Test                "))
      (dolist (case '(("%20.4(%s%)" . "                Test")
                      ("%-20.4(%s%)" . "Test                ")
                      ("%.4(%s%)" . "Test")))
        (with-temp-buffer
          (vm-tokenized-summary-insert
           message (vm-summary-sprintf (car case) message t))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         (cdr case))))))))

(defconst vm-summary-test--with-attachments
  (concat "From a@b.c Mon Jan  1 00:00:00 2024\n"
          "From: a@b.c\nSubject: two parts\nMIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"b\"\n\n"
          "--b\nContent-Type: text/plain\n\nthe text\n"
          "--b\nContent-Type: application/pdf\n"
          "Content-Disposition: attachment; filename=\"one.pdf\"\n\npdf\n"
          "--b\nContent-Type: image/png\n"
          "Content-Disposition: attachment; filename=\"two.png\"\n\npng\n"
          "--b--\n\n"
          "From a@b.c Mon Jan  2 00:00:00 2024\n"
          "From: a@b.c\nSubject: plain\n\nnothing\n\n")
  "Two messages, the first carrying two attachments and the second none.")

(defmacro vm-summary-test--visiting (text &rest body)
  "Visit a folder holding TEXT as a real folder and run BODY in its buffer.
`vm-mime-operate-on-attachments\=', which is what counts them, wants a folder
buffer of VM\='s own making rather than a buffer holding the text."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-summary-visit" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "folder" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil))
           (write-region ,text nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-summary-test-the-attachment-indicator-counts-for-a-symbol ()
  "%P is the indicator alone for a string and the indicator and count for a
symbol, and nothing at all for a message carrying no attachment.

The docstring said the count was always there, the manual did not mention
it, and the `:type\=' offered the counting branch with ?$ in it, which is the
integer 36: a reader who took what Customize offered summarised a message
of two attachments as \"362\" (emacs-vm/vm#852)."
  (vm-summary-test--visiting vm-summary-test--with-attachments
    (let ((carrier (car vm-message-list))
          (plain (cadr vm-message-list)))
      (let ((vm-summary-attachment-indicator "$"))
        (should (equal (vm-summary-sprintf "%P" carrier) "$"))
        (should (equal (vm-summary-sprintf "%P" plain) "")))
      (let ((vm-summary-attachment-indicator '$))
        (should (equal (vm-summary-sprintf "%P" carrier) "$2"))
        (should (equal (vm-summary-sprintf "%P" plain) ""))))))

;;; vm-su-labels tests

(ert-deftest vm-summary-test-su-labels-none ()
  "Test vm-su-labels returns empty string when no labels."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((labels (vm-su-labels msg)))
        (should (stringp labels))
        (should (string= labels ""))))))

(ert-deftest vm-summary-test-su-labels-with-labels ()
  "Test vm-su-labels returns labels when present."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("important" "work"))
      (let ((labels (vm-su-labels msg)))
        (should (stringp labels))))))

;;; vm-su-thread-indent tests

(ert-deftest vm-summary-test-su-thread-indent ()
  "Test vm-su-thread-indent returns indent string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indent (vm-su-thread-indent msg)))
        (should (stringp indent))))))

;;; vm-su-interesting-from tests

(ert-deftest vm-summary-test-su-interesting-from ()
  "Test vm-su-interesting-from returns from or to based on context."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((from (vm-su-interesting-from msg)))
        (should (stringp from))))))

;;; vm-su-size tests

(ert-deftest vm-summary-test-su-size ()
  "Test vm-su-size returns human-readable size."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((size (vm-su-size msg)))
        (should (stringp size))))))

;;; vm-su-datestring tests

(ert-deftest vm-summary-test-su-datestring ()
  "Test vm-su-datestring returns formatted date."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((datestring (vm-su-datestring msg)))
        (should (stringp datestring))))))

;;; vm-su-month-number tests

(ert-deftest vm-summary-test-su-month-number ()
  "Test vm-su-month-number returns numeric month."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((month-num (vm-su-month-number msg)))
        (should (stringp month-num))
        (should (string-match "0?1" month-num))))))

;;; vm-su-hour-short tests

(ert-deftest vm-summary-test-su-hour-short ()
  "Test vm-su-hour-short returns short time format."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((hour (vm-su-hour-short msg)))
        (should (stringp hour))))))

;;; vm-su-decoded-to-names tests

(ert-deftest vm-summary-test-su-decoded-to-names ()
  "Test vm-su-decoded-to-names extracts recipient names."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((names (vm-su-decoded-to-names msg)))
        (should (or (null names) (stringp names)))))))

;;; vm-su-decoded-to-cc tests

(ert-deftest vm-summary-test-su-decoded-to-cc ()
  "Test vm-su-decoded-to-cc extracts To and CC."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((to-cc (vm-su-decoded-to-cc msg)))
        (should (stringp to-cc))
        (should (string-match "recipient" to-cc))))))

;;; vm-su-decoded-to-cc-names tests

(ert-deftest vm-summary-test-su-decoded-to-cc-names ()
  "Test vm-su-decoded-to-cc-names extracts To and CC names."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((names (vm-su-decoded-to-cc-names msg)))
        (should (or (null names) (stringp names)))))))

;;; vm-su-interesting-full-name tests

(ert-deftest vm-summary-test-su-interesting-full-name ()
  "Test vm-su-interesting-full-name returns appropriate name."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((name (vm-su-interesting-full-name msg)))
        (should (or (null name) (stringp name)))))))

;;; From_ envelope parsing for virtual messages (issue #447)

(defun vm-summary-test--make-virtual-message (real-m virtual-buffer)
  "Return a virtual message in VIRTUAL-BUFFER mirroring REAL-M.
Built the way `vm-build-virtual-message-list' builds one, in particular with
a location-data vector of markers that point nowhere.  Those markers are
shared by every virtual message in the folder and are only aimed at anything
by `vm-make-virtual-copy', for the one message being displayed -- so during
summary generation, which is what #447 is about, they point nowhere."
  (let ((vector (make-vector vm-location-data-vector-length nil))
        (i 0)
        (message (copy-sequence real-m)))
    (while (< i vm-location-data-vector-length)
      (aset vector i (vm-marker nil))
      (setq i (1+ i)))
    (vm-set-location-data-of message vector)
    (vm-set-softdata-of message (make-vector vm-softdata-vector-length nil))
    (vm-set-real-message-sym-of message (vm-real-message-sym-of real-m))
    (vm-set-buffer-of message virtual-buffer)
    message))

(defconst vm-summary-test--from_-folder
  "From alice@example.com Mon Jan  1 00:00:00 2024
Subject: No From header

Body text
"
  "A From_ folder whose message has no From: or Date: header.
`vm-su-do-author' only falls back to `vm-grok-From_-author', and the date
only falls back to `vm-grok-From_-date', when the headers are absent.")

(ert-deftest vm-summary-test-grok-from_-author-on-virtual-message ()
  "REGRESSION: the From_ author of a virtual message reads the real folder.
`vm-grok-From_-author' took its buffer and its position from the message it
was handed.  For a virtual message that is the virtual folder buffer and the
shared location markers, which point nowhere until the message is displayed,
so generating the summary of a virtual folder signalled a marker error
instead of returning the author (issue #447)."
  (vm-test-with-folder vm-summary-test--from_-folder
    (let ((real-m (car vm-message-list))
          (virtual-buffer (generate-new-buffer " *vm-test-virtual*")))
      (unwind-protect
          (let ((virtual-m (vm-summary-test--make-virtual-message
                            real-m virtual-buffer)))
            ;; Mirror a From_-flavoured virtual folder.
            (vm-set-message-type-of virtual-m 'From_)
            (should (equal (vm-grok-From_-author virtual-m)
                           "alice@example.com"))
            (should (equal (vm-grok-From_-author virtual-m)
                           (vm-grok-From_-author real-m))))
        (kill-buffer virtual-buffer)))))

(ert-deftest vm-summary-test-grok-from_-date-on-virtual-message ()
  "REGRESSION: the From_ date of a virtual message reads the real folder.
As for the author above: `vm-grok-From_-date' switched to the real message's
buffer but then used the virtual message's own start marker, mixing the two."
  (vm-test-with-folder vm-summary-test--from_-folder
    (let ((real-m (car vm-message-list))
          (virtual-buffer (generate-new-buffer " *vm-test-virtual*")))
      (unwind-protect
          (let ((virtual-m (vm-summary-test--make-virtual-message
                            real-m virtual-buffer)))
            (vm-set-message-type-of virtual-m 'From_)
            (should (equal (vm-grok-From_-date virtual-m)
                           "Mon Jan  1 00:00:00 2024"))
            (should (equal (vm-grok-From_-date virtual-m)
                           (vm-grok-From_-date real-m))))
        (kill-buffer virtual-buffer)))))

(ert-deftest vm-summary-test-grok-from_-ignores-non-from_-folders ()
  "A folder type without a From_ envelope line yields nil, not a guess."
  (vm-test-with-folder vm-summary-test--from_-folder
    (let ((m (car vm-message-list)))
      (vm-set-message-type-of m 'mmdf)
      (should-not (vm-grok-From_-author m))
      (should-not (vm-grok-From_-date m)))))

;;; Folding a thread in the summary (emacs-vm/vm#627)
;;
;; The four commands the manual documents for this -- vm-collapse-thread,
;; vm-expand-thread, vm-collapse-all-threads, vm-expand-all-threads -- had no
;; test between them.

(defconst vm-summary-test--thread
  (concat "From a@example.com Sat Aug  8 14:24:13 2026\n"
          "From: a@example.com\nMessage-ID: <root@example.com>\n"
          "Subject: the root\n\nRoot body.\n\n"
          "From b@example.com Sat Aug  8 15:00:00 2026\n"
          "From: b@example.com\nMessage-ID: <kid1@example.com>\n"
          "References: <root@example.com>\nSubject: Re: the root\n\nFirst reply.\n\n"
          "From c@example.com Sat Aug  8 16:00:00 2026\n"
          "From: c@example.com\nMessage-ID: <kid2@example.com>\n"
          "References: <root@example.com> <kid1@example.com>\n"
          "Subject: Re: the root\n\nSecond reply.\n\n")
  "A root and two replies, which References makes one thread of three.")

(defmacro vm-summary-test--with-thread (spec &rest body)
  "Visit a folder of one three-message thread and run BODY.
SPEC is (FOLDER-VAR).  Threads are shown and folding is on, and every message
is marked read: `vm-summary-visible' keeps new messages visible whatever the
fold says, so a folder of new mail cannot show folding at all."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-summary-thread" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "threaded" dir))
               (vm-summary-show-threads t)
               (vm-summary-enable-thread-folding t)
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil))
           (write-region vm-summary-test--thread nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (dolist (m vm-message-list)
               (vm-set-new-flag m nil)
               (vm-set-unread-flag m nil))
             (vm-update-summary-and-mode-line)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-summary-test--hidden-replies ()
  "How many of the thread's replies are invisible in the summary."
  (with-current-buffer vm-summary-buffer
    (let ((n 0))
      (dolist (m (cdr vm-message-list))
        (when (get-text-property (vm-su-start-of m) 'invisible)
          (setq n (1+ n))))
      n)))

(ert-deftest vm-summary-test-collapsing-a-thread-hides-its-replies ()
  "`vm-collapse-thread' hides the replies and leaves the root showing.
Called from Lisp with a root, which is what its docstring tells a program to
do -- and what used to signal args-out-of-range, the text properties being
applied to the folder buffer while the markers point into the summary."
  (vm-summary-test--with-thread (_folder)
    (let ((root (car vm-message-list)))
      (should (eq root (vm-thread-root root)))
      (should (= (vm-thread-count root) 3))
      (vm-collapse-thread nil root)
      (should (vm-collapsed-root-p root))
      (should (= (vm-summary-test--hidden-replies) 2))
      ;; the root itself stays visible, or there would be nothing to click
      (with-current-buffer vm-summary-buffer
        (should-not (get-text-property (vm-su-start-of root) 'invisible))))))

(ert-deftest vm-summary-test-expanding-a-thread-shows-them-again ()
  "`vm-expand-thread' undoes it, and marks the root expanded."
  (vm-summary-test--with-thread (_folder)
    (let ((root (car vm-message-list)))
      (vm-collapse-thread nil root)
      (should (= (vm-summary-test--hidden-replies) 2))
      (vm-expand-thread root)
      (should (vm-expanded-root-p root))
      (should-not (vm-collapsed-root-p root))
      (should (= (vm-summary-test--hidden-replies) 0)))))

(ert-deftest vm-summary-test-collapse-and-expand-all-threads ()
  "The all-threads commands do it to every thread in the folder.
They work from the folder buffer, unlike the two above, because they wrap the
per-thread call in the summary buffer themselves -- which is how the bug in
those two stayed hidden."
  (vm-summary-test--with-thread (_folder)
    (let ((root (car vm-message-list)))
      (vm-collapse-all-threads)
      (should (vm-collapsed-root-p root))
      (should (= (vm-summary-test--hidden-replies) 2))
      (vm-expand-all-threads)
      (should (vm-expanded-root-p root))
      (should (= (vm-summary-test--hidden-replies) 0)))))

(defun vm-summary-test--operable (count enable)
  "Select operable messages as a summary command would, and say what happened.
Answers (HOW-MANY . ASKED), with COUNT the command\='s prefix argument and
ENABLE the value of `vm-enable-thread-operations\='."
  (let ((asked nil))
    (let ((vm-enable-thread-operations enable)
          (vm-user-interaction-buffer vm-summary-buffer)
          (last-command nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) t)))
        (cons (length (vm-select-operable-messages count t "Save")) asked)))))

(ert-deftest vm-summary-test-a-thread-operation-takes-the-whole-thread ()
  "With thread operations on, a command on a collapsed root takes the thread.
`ask' does the same and asks first, which is the difference between the two."
  (vm-summary-test--with-thread (_folder)
    (let ((root (car vm-message-list)))
      (vm-collapse-thread nil root)
      (setq vm-message-pointer vm-message-list)
      (should (equal '(3 . nil) (vm-summary-test--operable 1 t)))
      (should (equal '(3 . t) (vm-summary-test--operable 1 'ask)))
      (should (equal '(1 . nil) (vm-summary-test--operable 1 nil))))))

(ert-deftest vm-summary-test-a-prefix-argument-is-not-a-thread-operation ()
  "A numeric prefix takes that many messages, and no thread and no question.

The manual said a prefix argument overrides the confirmation `ask' puts in
the way.  It does not: `vm-select-operable-messages' takes the count branch
whenever the count is neither nil nor 1, so C-u 2 s saves two messages from
point rather than the thread, and there is nothing to confirm because no
thread operation is being made."
  (vm-summary-test--with-thread (_folder)
    (let ((root (car vm-message-list)))
      (vm-collapse-thread nil root)
      (setq vm-message-pointer vm-message-list)
      (should (equal '(2 . nil) (vm-summary-test--operable 2 'ask)))
      ;; C-u on its own is 4, and the folder holds 3 from point
      (should (equal '(3 . nil) (vm-summary-test--operable 4 'ask))))))

(ert-deftest vm-summary-test-folding-needs-both-options ()
  "Folding refuses to run unless it is enabled and the summary is threaded.
Two separate refusals: without `vm-summary-enable-thread-folding' there is no
folding at all, and without `vm-summary-show-threads' there are no threads to
fold."
  (vm-summary-test--with-thread (_folder)
    (let ((text-quoting-style 'grave))
      (let ((vm-summary-enable-thread-folding nil))
        (should (string-match-p "folding"
                                (cadr (should-error (vm-collapse-thread))))))
      (let ((vm-summary-show-threads nil))
        (should (string-match-p "threads"
                                (cadr (should-error (vm-collapse-all-threads)))))))))

(ert-deftest vm-summary-test-a-new-reply-stays-visible-when-folded ()
  "A collapsed thread still shows a reply that has not been read.
`vm-summary-visible' is ((new)) out of the box, and folding a thread away
whose unread mail you have not seen would hide the reason you were looking."
  (vm-summary-test--with-thread (_folder)
    (let ((root (car vm-message-list)))
      (vm-set-new-flag (nth 1 vm-message-list) t)
      (vm-update-summary-and-mode-line)
      (vm-collapse-thread nil root)
      (should (= (vm-summary-test--hidden-replies) 1)))))

(defconst vm-summary-test--three
  (concat "From a@example.com Sat Aug  8 14:24:13 2026\n"
          "From: a@example.com\nSubject: one\n\nFirst body.\n\n"
          "From b@example.com Sat Aug  8 15:00:00 2026\n"
          "From: b@example.com\nSubject: two\n\nSecond body.\n\n"
          "From c@example.com Sat Aug  8 16:00:00 2026\n"
          "From: c@example.com\nSubject: three\n\nThird body.\n\n")
  "Three unrelated messages, so the summary has three lines and no threads.")

(defmacro vm-summary-test--with-summary (spec &rest body)
  "Visit a three-message folder with its summary built and run BODY.
SPEC is (FOLDER-VAR).  No threading, every message read, and the first
message selected, which is where the summary cursor starts."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-summary-cursor" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "three" dir))
               (vm-summary-show-threads nil)
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil))
           (write-region vm-summary-test--three nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (dolist (m vm-message-list)
               (vm-set-new-flag m nil)
               (vm-set-unread-flag m nil))
             (vm-update-summary-and-mode-line)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-summary-test--cursor-message ()
  "The message the summary cursor is on."
  (with-current-buffer vm-summary-buffer (vm-summary-message-at-point)))

(defun vm-summary-test--put-cursor-on (m)
  "Move the summary cursor to M, as a reader moving about the summary does."
  (with-current-buffer vm-summary-buffer
    (goto-char (vm-su-start-of m))
    (forward-line 0)))

(ert-deftest vm-summary-test-a-rebuild-leaves-the-cursor-where-it-was ()
  "Messages arriving during a fetch do not drag the cursor back.
A reader who moves to the end of the summary with \\[end-of-buffer] while a
fetch is running had the cursor pulled back to the selected message as each
bunch arrived: `vm-do-needed-summary-rebuild' set the summary pointer, which
moves point, and the reader could not stay where they had gone."
  (vm-summary-test--with-summary (_folder)
    (let ((last (car (last vm-message-list))))
      (vm-summary-test--put-cursor-on last)
      (should (eq (vm-summary-test--cursor-message) last))
      ;; what an arriving bunch does
      (vm-set-summary-redo-start-point t)
      (vm-update-summary-and-mode-line)
      (should (eq (vm-summary-test--cursor-message) last)))))

(ert-deftest vm-summary-test-a-rebuild-still-follows-the-selection ()
  "The cursor sitting on the selected message follows it, as it always did.
That is what moving through the folder looks like: the reader has not gone
anywhere else, so the pointer takes the cursor with it."
  (vm-summary-test--with-summary (_folder)
    (let ((first (car vm-message-list))
          (second (nth 1 vm-message-list)))
      (should (eq (vm-summary-test--cursor-message) first))
      (setq vm-message-pointer (cdr vm-message-list))
      (vm-set-summary-redo-start-point t)
      (vm-update-summary-and-mode-line)
      (should (eq (vm-summary-test--cursor-message) second)))))

(ert-deftest vm-summary-test-end-of-buffer-is-where-a-reader-lands ()
  "The cursor survives where \\[end-of-buffer] actually leaves it.
Which is `point-max', not the start of the last summary line -- and
`vm-summary-message-at-point' answers nil at end of buffer, so a fix that
only knew about a cursor on a message would not have covered the keystroke
the fault was reported against."
  (vm-summary-test--with-summary (_folder)
    (let (where)
      (with-current-buffer vm-summary-buffer
        (goto-char (point-max))
        (setq where (point)))
      (vm-set-summary-redo-start-point t)
      (vm-update-summary-and-mode-line)
      (with-current-buffer vm-summary-buffer
        (should (eobp))
        (should (= (point) where))))))

;;; The recovery from a header rfc822.el refuses

(ert-deftest vm-summary-test-a-corrupt-recipient-header-recovers ()
  "A header `rfc822-addresses' refuses leaves \"corrupted-header\", not an error.
The handler warned with `(vm-warn 0 5 err)', and `vm-warn' formats its
arguments -- so `format' was handed the condition object as its format
string and answered `wrong-type-argument stringp'.  The recovery below it
never ran, and the summary line failed outright (#781)."
  (vm-test-with-folder (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                               "From: alice@example.com\n"
                               "To: bob@example.com\nSubject: hi\n\nbody\n\n")
    (let ((m (car vm-message-list))
          (vm-verbosity 0))
      (cl-letf (((symbol-function 'rfc822-addresses)
                 (lambda (_s) (error "Unbalanced parentheses"))))
        (vm-su-do-recipients m)
        (should (equal (vm-decoded-to-cc-of m) "corrupted-header"))
        (vm-su-do-addressees m)
        (should (equal (vm-decoded-to-of m) "corrupted-header"))))))

(ert-deftest vm-summary-test-the-warning-survives-a-percent ()
  "The warning for that header says what went wrong, percent sign and all.
`vm-warn' formats what it is given, so an error text carrying a percent
signalled in place of reporting anything."
  (vm-test-with-folder (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                               "From: alice@example.com\n"
                               "To: bob@example.com\nSubject: hi\n\nbody\n\n")
    (let ((m (car vm-message-list))
          (vm-verbosity 5)
          (vm-current-warning nil)
          shown)
      (cl-letf (((symbol-function 'rfc822-addresses)
                 (lambda (_s) (error "Rubbish in address: 50%% off")))
                ((symbol-function 'vm-emit-message)
                 (lambda (_level text) (setq shown text))))
        (vm-su-do-recipients m)
        (should (string-match-p "50% off" shown))))))

;;; The date format the summary parses (emacs-vm/vm#831)

(defun vm-summary-test--parses-as-a-date (string)
  "Whether `vm-su-rfc822-date-format' matches STRING."
  (and (string-match vm-su-rfc822-date-format string) t))

(ert-deftest vm-summary-test-the-date-format-allows-space-and-dash ()
  "Spaces and the dashes the comment promises both separate the fields.
\"Some slop is allowed e.g. dashes between the monthday, month and year
because such malformed headers have been observed.\""
  (should (vm-summary-test--parses-as-a-date "13 Jan 2026 10:00:00 GMT"))
  (should (vm-summary-test--parses-as-a-date "13-Jan-2026 10:00:00 GMT"))
  (should (vm-summary-test--parses-as-a-date "Mon, 13 Jan 2026 10:00:00 -0800")))

(ert-deftest vm-summary-test-the-date-format-allows-nothing-else-between ()
  "REGRESSION: the separator is space, tab, newline or dash, and no more.

The class was written `[ \\t\\n---]', extra hyphens to get a literal one,
which Emacs reads as the range from newline to hyphen: 37 characters rather
than 4, among them `!' and every other punctuation mark below `0'.  So
`13!Jan!2026' parsed as a date (emacs-vm/vm#831)."
  (should-not (vm-summary-test--parses-as-a-date "13!Jan!2026 10:00:00 GMT"))
  (should-not (vm-summary-test--parses-as-a-date "13(Jan)2026 10:00:00 GMT"))
  (should-not (vm-summary-test--parses-as-a-date "13*Jan*2026 10:00:00 GMT")))

(provide 'vm-summary-test)

;;; vm-summary-test.el ends here
