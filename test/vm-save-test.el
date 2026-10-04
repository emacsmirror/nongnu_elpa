;;; vm-save-test.el --- Tests for vm-save.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM save functions in vm-save.el

;;; Code:

(require 'vm-test-init)
(require 'vm-save)

;;; Save function existence tests

(ert-deftest vm-save-test-functions-exist ()
  "Test that save functions exist."
  (should (fboundp 'vm-save-message))
  (should (fboundp 'vm-save-message-sans-headers))
  (should (fboundp 'vm-save-message-to-local-folder))
  (should (fboundp 'vm-auto-archive-messages))
  (should (fboundp 'vm-auto-select-folder)))

;;; Pipe function existence tests

(ert-deftest vm-save-test-pipe-functions-exist ()
  "Test that pipe functions exist."
  (should (fboundp 'vm-pipe-message-to-command))
  (should (fboundp 'vm-pipe-message-to-command-to-string))
  (should (fboundp 'vm-pipe-message-to-command-discard-output))
  (should (fboundp 'vm-pipe-messages-to-command))
  (should (fboundp 'vm-pipe-messages-to-command-to-string))
  (should (fboundp 'vm-pipe-messages-to-command-discard-output))
  (should (fboundp 'vm-pipe-message-part)))

;;; IMAP folder check

;;; Print function

(ert-deftest vm-save-test-print-message-exists ()
  "Test that vm-print-message exists."
  (should (fboundp 'vm-print-message)))

;;; vm-auto-select-folder tests

(ert-deftest vm-save-test-auto-select-folder-matches-from ()
  "Test vm-auto-select-folder matches From header."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: newsletter@lists.example.org
Subject: Weekly digest
Message-ID: <test@example.com>

Newsletter content
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters")
                     ("admin@" . "admin"))))
          (vm-save-using-auto-folders t))
      (should (equal "newsletters"
                     (vm-auto-select-folder vm-message-pointer))))))

(ert-deftest vm-save-test-auto-select-folder-matches-subject ()
  "Test vm-auto-select-folder matches Subject header."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: anyone@example.com
Subject: [BUG] Something is broken
Message-ID: <test@example.com>

Bug report
"
    (let ((vm-auto-folder-alist
           '(("Subject" ("\\[BUG\\]" . "bugs")
                        ("\\[FEATURE\\]" . "features"))))
          (vm-save-using-auto-folders t))
      (should (equal "bugs"
                     (vm-auto-select-folder vm-message-pointer))))))

(ert-deftest vm-save-test-auto-select-folder-matches-header-names-as-a-regexp ()
  "The first element of an entry matches header names rather than being one.
The manual called it HEADER-NAME and said the contents of the header it named
were searched, which is two things understated (emacs-vm/vm#776): it is a
regexp, and where more than one header name matches, their contents are
joined with a comma and a space before the search."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
To: alice@example.com
Cc: bob@example.com
From: anyone@example.com
Subject: hello
Message-ID: <test@example.com>

Body
"
    (let ((vm-save-using-auto-folders t))
      ;; a regexp over the names, not a name
      (let ((vm-auto-folder-alist '(("T." ("alice@" . "alices")))))
        (should (equal "alices" (vm-auto-select-folder vm-message-pointer))))
      ;; past the first header that matches the name
      (let ((vm-auto-folder-alist '(("To\\|Cc" ("bob@" . "bobs")))))
        (should (equal "bobs" (vm-auto-select-folder vm-message-pointer))))
      ;; and the contents of both, joined with a comma and a space
      (let ((vm-auto-folder-alist
             '(("To\\|Cc" ("alice@example.com, bob@example.com" . "both")))))
        (should (equal "both" (vm-auto-select-folder vm-message-pointer)))))))

(ert-deftest vm-save-test-auto-select-folder-no-match ()
  "Test vm-auto-select-folder returns nil when no match."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: random@example.com
Subject: Hello
Message-ID: <test@example.com>

Body
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters"))))
          (vm-save-using-auto-folders t))
      (should-not (vm-auto-select-folder vm-message-pointer)))))

(ert-deftest vm-save-test-auto-select-folder-disabled ()
  "Test vm-auto-select-folder returns nil when disabled."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: newsletter@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters"))))
          (vm-save-using-auto-folders nil))  ; disabled
      (should-not (vm-auto-select-folder vm-message-pointer)))))

(ert-deftest vm-save-test-auto-select-folder-case-fold ()
  "Test vm-auto-select-folder respects case-fold setting."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: NEWSLETTER@EXAMPLE.COM
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-auto-folder-alist
           '(("From" ("newsletter@" . "newsletters"))))
          (vm-save-using-auto-folders t)
          (vm-auto-folder-case-fold-search t))
      (should (equal "newsletters"
                     (vm-auto-select-folder vm-message-pointer))))))

;;; vm-pipe-message-part tests

(ert-deftest vm-save-test-pipe-message-part-whole-message ()
  "Test vm-pipe-message-part returns whole message with no prefix arg."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body text here
"
    (let* ((msg (vm-test-first-message))
           (prefix-arg nil)
           (region (vm-pipe-message-part msg prefix-arg)))
      ;; Should return headers-of to text-end-of
      (should (= (vm-headers-of msg) (nth 0 region)))
      (should (= (vm-text-end-of msg) (nth 1 region))))))

(ert-deftest vm-save-test-pipe-message-part-body-only ()
  "Test vm-pipe-message-part returns body with single prefix arg."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body text here
"
    (let* ((msg (vm-test-first-message))
           (prefix-arg '(4))  ; C-u
           (region (vm-pipe-message-part msg prefix-arg)))
      ;; Should return text-of to text-end-of (body only)
      (should (= (vm-text-of msg) (nth 0 region)))
      (should (= (vm-text-end-of msg) (nth 1 region))))))

(ert-deftest vm-save-test-pipe-message-part-headers-only ()
  "Test vm-pipe-message-part returns headers with double prefix arg."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body text here
"
    (let* ((msg (vm-test-first-message))
           (prefix-arg '(16))  ; C-u C-u
           (region (vm-pipe-message-part msg prefix-arg)))
      ;; Should return headers-of to text-of (headers only)
      (should (= (vm-headers-of msg) (nth 0 region)))
      (should (= (vm-text-of msg) (nth 1 region))))))

;;; vm-switch-to-command-output-buffer tests

(ert-deftest vm-save-test-switch-output-buffer-empty ()
  "Test vm-switch-to-command-output-buffer with empty output."
  (let ((buffer (get-buffer-create " *test-output*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (erase-buffer))
          ;; Should not error with empty buffer
          (vm-switch-to-command-output-buffer "test-cmd" buffer nil))
      (kill-buffer buffer))))

(ert-deftest vm-save-test-switch-output-buffer-with-content ()
  "Test vm-switch-to-command-output-buffer with output content."
  (let ((buffer (get-buffer-create " *test-output*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Some output"))
          ;; Should not error with content
          (vm-switch-to-command-output-buffer "test-cmd" buffer nil))
      (kill-buffer buffer))))

(ert-deftest vm-save-test-switch-output-buffer-discard ()
  "Test vm-switch-to-command-output-buffer with discard flag."
  (let ((buffer (get-buffer-create " *test-output*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Some output"))
          ;; With discard-output=t, should not display buffer
          (vm-switch-to-command-output-buffer "test-cmd" buffer t))
      (kill-buffer buffer))))

;;; Filed flag tests

(ert-deftest vm-save-test-filed-flag ()
  "Test that vm-set-filed-flag sets the filed attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not filed
      (should-not (vm-filed-flag msg))
      ;; Set filed flag
      (vm-set-filed-flag msg t)
      (should (vm-filed-flag msg))
      ;; Unset filed flag
      (vm-set-filed-flag msg nil)
      (should-not (vm-filed-flag msg)))))

;;; Written flag tests

(ert-deftest vm-save-test-written-flag ()
  "Test that vm-set-written-flag sets the written attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not written
      (should-not (vm-written-flag msg))
      ;; Set written flag
      (vm-set-written-flag msg t)
      (should (vm-written-flag msg)))))

;;; vm-auto-select-folder-for-save tests

(defmacro vm-save-test-with-auto-folder (folder-name &rest body)
  "Run BODY in a folder whose auto-folder-alist maps the message to FOLDER-NAME."
  (declare (indent 1))
  `(vm-test-with-folder
       "From sender@example.com Mon Jan  1 00:00:00 2024
From: newsletter@lists.example.org
Subject: Weekly digest
Message-ID: <test@example.com>

Newsletter content
"
     (let ((vm-auto-folder-alist
            (list (list "From" (cons "newsletter@" ,folder-name))))
           (vm-save-using-auto-folders t))
       ,@body)))

(ert-deftest vm-save-test-auto-select-for-save-passes-other-folder ()
  "Test that a folder other than the current one is still suggested."
  (vm-save-test-with-auto-folder "newsletters"
    (should (equal "newsletters"
                   (vm-auto-select-folder-for-save vm-message-pointer)))))

(ert-deftest vm-save-test-auto-select-for-save-skips-current-folder ()
  "Test that the folder the message is already in is not suggested.
Regression test for issue #163: `vm-auto-folder-alist' would offer to
save a message into the very folder holding it.  `vm-auto-archive-messages'
already skipped that case; the interactive save prompt did not."
  (vm-save-test-with-auto-folder "newsletters"
    ;; make the auto-selected folder resolve to this very buffer
    (cl-letf (((symbol-function 'vm-get-file-buffer)
               (let ((this (current-buffer)))
                 (lambda (f) (and (equal f "newsletters") this)))))
      (should (null (vm-auto-select-folder-for-save vm-message-pointer))))))

(ert-deftest vm-save-test-auto-select-for-save-relative-name ()
  "Test that a relative folder name is resolved against the folder directory.
`vm-save-message' expands the name that way, so comparing it against the
current folder has to as well, or the match is missed whenever
`default-directory' is not the folder directory."
  (vm-save-test-with-auto-folder "newsletters"
    (let ((vm-folder-directory "/folders/")
          (vm-foreign-folder-directory nil)
          (default-directory "/somewhere/else/")
          (seen nil))
      (cl-letf (((symbol-function 'vm-get-file-buffer)
                 (let ((this (current-buffer)))
                   (lambda (f)
                     ;; record what the name expanded to
                     (setq seen (expand-file-name f))
                     (and (equal seen "/folders/newsletters") this)))))
        (should (null (vm-auto-select-folder-for-save vm-message-pointer)))
        (should (equal seen "/folders/newsletters"))))))

(ert-deftest vm-save-test-auto-select-for-save-no-match ()
  "Test that no match still yields nil."
  (vm-save-test-with-auto-folder "newsletters"
    (let ((vm-auto-folder-alist nil))
      (should (null (vm-auto-select-folder-for-save vm-message-pointer))))))

;;; Which folder VM picks for you, and which folders are IMAP.  Both of these
;;; had a test asserting only that the function was bound.

(defconst vm-save-test--two-messages
  (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
          "From: alice@example.com\nTo: list@example.org\n"
          "Subject: [emacs-vm] a patch\n\nBody.\n\n"
          "From bob@elsewhere.test Mon Jan  1 00:00:00 2024\n"
          "From: bob@elsewhere.test\nTo: me@example.com\n"
          "Subject: lunch\n\nBody.\n\n")
  "One list message and one personal one, to be filed differently.")

(ert-deftest vm-save-test-auto-select-folder-matches-a-header ()
  "The folder is the one whose regexp matches the header named."
  (vm-test-with-folder vm-save-test--two-messages
    (let ((vm-save-using-auto-folders t)
          (alist '(("From" ("alice@example\\.com" . "from-alice"))
                   ("Subject" ("lunch" . "social")))))
      (should (equal (vm-auto-select-folder vm-message-list alist)
                     "from-alice"))
      (should (equal (vm-auto-select-folder (cdr vm-message-list) alist)
                     "social"))
      ;; nothing matches, and nothing is chosen
      (should-not (vm-auto-select-folder
                   vm-message-list '(("Subject" ("nothing here" . "x"))))))))

(ert-deftest vm-save-test-auto-select-folder-is-off-unless-asked-for ()
  "`vm-save-using-auto-folders' nil means the alist is not consulted at all."
  (vm-test-with-folder vm-save-test--two-messages
    (let ((vm-save-using-auto-folders nil)
          (alist '(("From" ("alice" . "from-alice")))))
      (should-not (vm-auto-select-folder vm-message-list alist)))))

(ert-deftest vm-save-test-auto-select-folder-folds-case-if-told-to ()
  "`vm-auto-folder-case-fold-search' decides whether the regexp cares."
  (vm-test-with-folder vm-save-test--two-messages
    (let ((vm-save-using-auto-folders t)
          (alist '(("From" ("ALICE@EXAMPLE" . "shouting")))))
      (let ((vm-auto-folder-case-fold-search t))
        (should (equal (vm-auto-select-folder vm-message-list alist)
                       "shouting")))
      (let ((vm-auto-folder-case-fold-search nil))
        (should-not (vm-auto-select-folder vm-message-list alist))))))

(ert-deftest vm-save-test-auto-select-folder-evaluates-a-form ()
  "A form instead of a string is evaluated with the match data of the header,
which is how a folder gets named after part of what it matched -- the
mailing-list name in a Subject, say.  It is documented and nothing tested it."
  (vm-test-with-folder vm-save-test--two-messages
    (let ((vm-save-using-auto-folders t)
          (alist '(("Subject" ("\\[\\([a-z-]+\\)\\]" . (match-string 1))))))
      (should (equal (vm-auto-select-folder vm-message-list alist)
                     "emacs-vm")))
    ;; a form returning a list is taken as an alist to look in next
    (let ((vm-save-using-auto-folders t)
          (alist '(("Subject" ("patch" . '(("From" ("alice" . "nested"))))))))
      (should (equal (vm-auto-select-folder vm-message-list alist)
                     "nested")))))

(ert-deftest vm-save-test-auto-select-folder-says-which-variable-is-wrong ()
  "A broken alist names `vm-auto-folder-alist' in the error, since that is
what the user has to go and fix."
  (vm-test-with-folder vm-save-test--two-messages
    (let ((vm-save-using-auto-folders t)
          (text-quoting-style 'grave))
      (should (string-match-p
               "vm-auto-folder-alist"
               (cadr (should-error
                      (vm-auto-select-folder
                       vm-message-list
                       '(("From" ("alice" . (this-is-not-a-function)))))))))))) 

(ert-deftest vm-save-test-auto-select-folder-for-save-avoids-this-folder ()
  "The folder a message is already in is not suggested for saving it."
  (vm-test-with-folder vm-save-test--two-messages
    (let ((vm-save-using-auto-folders t)
          (alist '(("From" ("alice@example\\.com" . "from-alice")))))
      (setq vm-folder-directory nil)
      (let ((buffer-file-name (expand-file-name "from-alice")))
        (should-not (vm-auto-select-folder-for-save vm-message-list alist)))
      (let ((buffer-file-name (expand-file-name "some-other-folder")))
        (should (equal (vm-auto-select-folder-for-save vm-message-list alist)
                       "from-alice"))))))

(ert-deftest vm-save-test-imap-folder-p-answers-for-the-folder-buffer ()
  "Whether this is an IMAP folder is a question about the folder buffer,
asked from the summary as often as not."
  (let ((folder (generate-new-buffer " *vm-save-test-folder*"))
        (summary (generate-new-buffer " *vm-save-test-summary*")))
    (unwind-protect
        (save-current-buffer
          (with-current-buffer folder (setq major-mode 'vm-mode))
          (set-buffer summary)
          (setq vm-mail-buffer folder)
          (with-current-buffer folder (setq vm-folder-access-method nil))
          (should-not (vm-imap-folder-p))
          (with-current-buffer folder (setq vm-folder-access-method 'imap))
          (should (vm-imap-folder-p))
          (with-current-buffer folder (setq vm-folder-access-method 'pop))
          (should-not (vm-imap-folder-p)))
      (kill-buffer folder)
      (kill-buffer summary))))

;;; Piping and saving without headers (emacs-vm/vm#632)
;;
;; `vm-pipe-message-to-command' and `vm-save-message-sans-headers' had no
;; test.  Both hand the message to something outside VM -- a shell command,
;; a file -- so what they hand over is the thing to check, and the prefix
;; argument that chooses how much of the message goes is the interesting part.

(defconst vm-save-test--pipe-folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nTo: me@example.com\n"
          "Subject: piping\nMessage-ID: <pipe@example.com>\n\n"
          "The body to be piped.\n\n"
          "From bob@example.com Sat Aug  8 16:05:00 2026\n"
          "From: bob@example.com\nTo: me@example.com\n"
          "Subject: second\nMessage-ID: <two@example.com>\n\n"
          "The second body.\n\n")
  "Two messages, so a command run over marks can be told from one message.")

(defconst vm-save-test--pipe-folder-one-newline
  (substring vm-save-test--pipe-folder 0 -1)
  "The same two messages, with one newline at the end of the file.
An mbox whose last body is not followed by a blank line.  The text of that
last message then ends in a full stop, so a pipe run with the trailing
separator off ends mid-line, which is what issue #881 hangs on.")

(defmacro vm-save-test--with-this-pipe-folder (text &rest body)
  "Visit a folder holding TEXT and run BODY in the folder buffer."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-pipe" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-last-pipe-command nil))
           (write-region ,text nil folder nil 'quiet)
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

(defmacro vm-save-test--with-pipe-folder (&rest body)
  "Visit the piping fixture and run BODY in the folder buffer."
  (declare (indent 0) (debug t))
  `(vm-save-test--with-this-pipe-folder vm-save-test--pipe-folder ,@body))

(defun vm-save-test--piped-to-file (file)
  "What a pipe wrote to FILE."
  (if (file-exists-p file)
      (with-temp-buffer (insert-file-contents file) (buffer-string))
    ""))

(ert-deftest vm-save-test-piping-a-message-sends-headers-and-body ()
  "`vm-pipe-message-to-command' hands the whole message to the command,
headers and body, but not the folder's own separator line -- what the
command sees has to be a message, not a piece of an mbox."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "piped" temporary-file-directory)))
      (unwind-protect
          (progn
            (vm-pipe-message-to-command (format "cat > %s" target))
            (let ((piped (vm-save-test--piped-to-file target)))
              (should (string-match-p "Subject: piping" piped))
              (should (string-match-p "The body to be piped" piped))
              (should-not (string-match-p "\\`From alice@example\\.com " piped))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-piping-with-a-prefix-sends-only-a-part ()
  "REGRESSION: the prefix argument chooses how much of the message goes.
Issue #634.  One \\[universal-argument] sends the text, two the headers, and
they are complementary -- a test of one without the other would not notice
the parts being swapped.

`vm-pipe-message-part' took the prefix as an argument and then ignored it,
reading the variable `prefix-arg' instead.  That is the prefix for the next
command and is nil while one is running, so every documented prefix did
nothing and the whole message went every time."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "piped-part" temporary-file-directory)))
      (unwind-protect
          (progn
            (vm-pipe-message-to-command (format "cat > %s" target) '(4))
            (let ((piped (vm-save-test--piped-to-file target)))
              (should (string-match-p "The body to be piped" piped))
              (should-not (string-match-p "Subject: piping" piped)))
            (vm-pipe-message-to-command (format "cat > %s" target) '(16))
            (let ((piped (vm-save-test--piped-to-file target)))
              (should (string-match-p "Subject: piping" piped))
              (should-not (string-match-p "The body to be piped" piped))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-piping-with-a-prefix-typed-at-the-keyboard ()
  "REGRESSION: the prefix works when it comes from the keyboard too.
Issue #634.  This is the path a user takes -- \\[universal-argument] \\[vm-pipe-message-to-command] --
where the prefix arrives as `current-prefix-arg' and the command's own
interactive spec passes it on.  A test that only called the function with an
argument would not have caught this one being read from the wrong variable,
since both were wrong in the same way."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "piped-typed" temporary-file-directory)))
      (unwind-protect
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) (format "cat > %s" target))))
            (let ((current-prefix-arg '(4)))
              (call-interactively 'vm-pipe-message-to-command))
            (let ((piped (vm-save-test--piped-to-file target)))
              (should (string-match-p "The body to be piped" piped))
              (should-not (string-match-p "Subject: piping" piped))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-piping-remembers-the-last-command ()
  "The command is remembered, since `vm-pipe-message-to-command' offers it
as the default next time."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "piped-remember" temporary-file-directory)))
      (unwind-protect
          (progn
            (vm-pipe-message-to-command (format "cat > %s" target))
            (should (equal vm-last-pipe-command (format "cat > %s" target))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-piping-to-a-string-returns-the-output ()
  "`vm-pipe-message-to-command-to-string' gives the command's output back
rather than displaying it, which is what a program calling it wants."
  (vm-save-test--with-pipe-folder
    (should (string-match-p
             "Subject: piping"
             (vm-pipe-message-to-command-to-string "cat")))))

(ert-deftest vm-save-test-saving-without-headers-writes-the-body-alone ()
  "`vm-save-message-sans-headers' writes the body and leaves the headers
out, and marks the message written -- that flag is how the summary shows
that a message has been filed somewhere."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "body-only" temporary-file-directory)))
      (unwind-protect
          (progn
            (vm-save-message-sans-headers target 1 t)
            (let ((saved (vm-save-test--piped-to-file target)))
              (should (string-match-p "The body to be piped" saved))
              (should-not (string-match-p "Subject: piping" saved))
              (should-not (string-match-p "From: alice" saved)))
            (should (vm-written-flag (car vm-message-list))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-saving-without-headers-appends ()
  "A second save appends rather than replacing what is there, which is what
makes the command usable for collecting bodies into one file."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "collected" temporary-file-directory)))
      (unwind-protect
          (progn
            (vm-save-message-sans-headers target 1 t)
            (setq vm-message-pointer (cdr vm-message-list))
            (vm-save-message-sans-headers target 1 t)
            (let ((saved (vm-save-test--piped-to-file target)))
              (should (string-match-p "The body to be piped" saved))
              (should (string-match-p "The second body" saved))))
        (ignore-errors (delete-file target))))))

;;; Piping every marked message in one run (issue #881)

(defun vm-save-test--pipe-all-marked (command)
  "Run COMMAND over every message of the folder, as `|s' does over marks."
  (vm-mark-all-messages)
  (let ((last-command 'vm-next-command-uses-marks))
    (vm-pipe-messages-to-command command)))

(defun vm-save-test--kill-the-pipe ()
  "Kill whatever is still running in the pipe's output buffer.
A test that fails here fails by timing out with the command still reading, and
the folder buffers are killed straight afterwards -- which would stop for
\"Buffer has a running process\" and read the answer from an empty stdin."
  (let ((process (get-buffer-process "*Shell Command Output*")))
    (when process (delete-process process))))

(ert-deftest vm-save-test-piping-all-returns-with-the-end-separator-off ()
  "REGRESSION: `|s' froze Emacs when the text it sent last had no newline.
Issue #881.  `start-process' gives the command a pty unless told otherwise,
and `process-send-eof' on a pty sends ^D, which the terminal driver turns
into end of file only at the start of a line.  The command never saw end of
file, never exited, and the wait loop at the end of the function span on
`vm-accept-process-output' until the reader typed C-g.

Here `vm-pipe-messages-to-command-end' is nil, so the last thing sent is the
message text, which ends in a full stop.  The timeout is the assertion:
unfixed, this does not fail, it hangs."
  (vm-save-test--with-this-pipe-folder vm-save-test--pipe-folder-one-newline
    (let ((target (expand-file-name "piped-all" temporary-file-directory))
          (vm-pipe-messages-to-command-end nil))
      (unwind-protect
          (with-timeout (30 (ert-fail "the command was still reading"))
            (vm-save-test--pipe-all-marked (format "cat > %s" target))
            (let ((piped (vm-save-test--piped-to-file target)))
              (should (string-match-p "The body to be piped" piped))
              (should (string-match-p "The second body" piped))))
        (vm-save-test--kill-the-pipe)
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-piping-all-returns-for-a-folder-with-no-final-newline ()
  "REGRESSION: the same hang with nothing set, on a folder file ending mid-line.
Issue #881.  The separators are the default here, so the last thing sent is the
folder's trailing separator, and for a file that ends without a newline that
separator is empty: the message text is again the last thing the command
reads.  Such a folder is legal and VM reads it."
  (vm-save-test--with-this-pipe-folder (substring vm-save-test--pipe-folder 0 -2)
    (let ((target (expand-file-name "piped-unterminated" temporary-file-directory)))
      (unwind-protect
          (with-timeout (30 (ert-fail "the command was still reading"))
            (vm-save-test--pipe-all-marked (format "cat > %s" target))
            (should (string-match-p "The second body"
                                    (vm-save-test--piped-to-file target))))
        (vm-save-test--kill-the-pipe)
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-piping-all-sends-every-message-with-its-separators ()
  "One run of the command gets every marked message, each between separators.
`vm-pipe-messages-to-command-start' and `vm-pipe-messages-to-command-end'
default to t, which means the folder's own leading and trailing separator, so
what the command reads is an mbox it can take apart again."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "piped-marked" temporary-file-directory)))
      (unwind-protect
          (with-timeout (30 (ert-fail "the command was still reading"))
            (vm-save-test--pipe-all-marked (format "cat > %s" target))
            (let ((piped (vm-save-test--piped-to-file target)))
              (should (string-match-p "The body to be piped" piped))
              (should (string-match-p "The second body" piped))
              ;; one From_ line per message, the folder's own
              (should (= 2 (cl-count-if (lambda (line)
                                          (string-prefix-p "From " line))
                                        (split-string piped "\n"))))))
        (vm-save-test--kill-the-pipe)
        (ignore-errors (delete-file target))))))

;;; Saving a message to a folder (emacs-vm/vm#672)
;;
;; These drive `vm-save-message' itself over a folder on disk.  The tests
;; above cover the pieces around it -- which folder is chosen, which flag is
;; set -- and left the saving to nothing.

(defmacro vm-save-test--with-a-folder-of (count &rest body)
  "Visit a folder of COUNT messages and run BODY with a place to save to.

DIR is a temporary directory and `vm-folder-directory'; SOURCE is the folder
visited, TARGET a name in DIR that does not exist yet.  Everything the visit
made is killed afterwards."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-save" t)))
          (source (expand-file-name "inbox" dir))
          (target (expand-file-name "archive" dir))
          (vm-folder-directory dir)
          (vm-foreign-folder-directory nil)
          (vm-confirm-new-folders nil)
          (vm-visit-when-saving nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-last-save-folder vm-last-save-folder)
          (before (buffer-list)))
     (unwind-protect
         (progn
           (with-temp-file source
             (dotimes (i ,count)
               (insert (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                                       "From: alice@example.com\n"
                                       "Subject: msg %d\n\nbody %d\n\n")
                               (1+ i) (1+ i)))))
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder source)
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-save-test--subjects-in (file)
  "The subjects in FILE, in order."
  (if (not (file-exists-p file))
      'no-such-file
    (with-temp-buffer
      (insert-file-contents file)
      (let (subjects)
        (goto-char (point-min))
        (while (re-search-forward "^Subject: \\(.*\\)$" nil t)
          (push (match-string-no-properties 1) subjects))
        (nreverse subjects)))))

(ert-deftest vm-save-test-saving-writes-the-message-to-the-folder ()
  "The message reaches the folder named, whole: its own From_ line, headers
and body, so the folder can be read back as a folder."
  (vm-save-test--with-a-folder-of 3
    (vm-save-message target 1)
    (should (equal (vm-save-test--subjects-in target) '("msg 1")))
    (with-temp-buffer
      (insert-file-contents target)
      (should (string-match-p "^From alice@example\\.com " (buffer-string)))
      (should (string-match-p "^body 1$" (buffer-string))))))

(ert-deftest vm-save-test-saving-takes-the-count-it-is-given ()
  "Saving 3 saves the current message and the two after it, in order."
  (vm-save-test--with-a-folder-of 4
    (vm-save-message target 3)
    (should (equal (vm-save-test--subjects-in target)
                   '("msg 1" "msg 2" "msg 3")))))

(ert-deftest vm-save-test-saving-appends-to-a-folder-that-exists ()
  "A second save adds to the folder rather than replacing what is there."
  (vm-save-test--with-a-folder-of 3
    (vm-save-message target 1)
    (vm-goto-message 2)
    (vm-save-message target 1)
    (should (equal (vm-save-test--subjects-in target) '("msg 1" "msg 2")))))

(ert-deftest vm-save-test-saving-flags-the-message-filed ()
  "A saved message is marked filed, which is how the summary shows that a
copy of it is somewhere else, and how `vm-auto-archive-messages' knows to
leave it alone."
  (vm-save-test--with-a-folder-of 3
    (should-not (vm-filed-flag (car vm-message-list)))
    (vm-save-message target 2)
    (should (vm-filed-flag (nth 0 vm-message-list)))
    (should (vm-filed-flag (nth 1 vm-message-list)))
    (should-not (vm-filed-flag (nth 2 vm-message-list)))))

(ert-deftest vm-save-test-a-relative-name-goes-to-the-folder-directory ()
  "A name with no directory in it is a folder of yours, not a file beside
whatever `default-directory' happens to be."
  (vm-save-test--with-a-folder-of 2
    (let ((default-directory "/"))
      (vm-save-message "archive" 1))
    (should (equal (vm-save-test--subjects-in target) '("msg 1")))))

(ert-deftest vm-save-test-saving-remembers-the-name-as-typed ()
  "`vm-last-save-folder' keeps the name as given, so the next save offers
it back the same way rather than as an expanded path."
  (vm-save-test--with-a-folder-of 2
    (vm-save-message "archive" 1)
    (should (equal vm-last-save-folder "archive"))))

(ert-deftest vm-save-test-a-new-folder-is-confirmed ()
  "With `vm-confirm-new-folders', a folder that does not exist is offered
before it is created, and answering no creates nothing."
  (vm-save-test--with-a-folder-of 2
    (let ((vm-confirm-new-folders t)
          (asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq asked prompt) nil)))
        (should-error (vm-save-message target 1) :type 'error))
      (should asked)
      (should (string-match-p "archive" asked))
      (should (equal (vm-save-test--subjects-in target) 'no-such-file))
      ;; and answering yes writes it
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-save-message target 1))
      (should (equal (vm-save-test--subjects-in target) '("msg 1"))))))

(ert-deftest vm-save-test-a-folder-of-unknown-type-is-refused ()
  "A folder VM cannot make out is refused by name rather than appended to
in a format that would corrupt it."
  (vm-save-test--with-a-folder-of 2
    (let ((text-quoting-style 'grave))
      (cl-letf (((symbol-function 'vm-get-folder-type) (lambda (&rest _) 'unknown)))
        (let ((err (should-error (vm-save-message target 1) :type 'error)))
          (should (string-match-p "unrecognized" (error-message-string err))))))
    (should (equal (vm-save-test--subjects-in target) 'no-such-file))))

(ert-deftest vm-save-test-a-read-only-folder-is-not-saved-into ()
  "Saving into a folder visited read-only is refused: the buffer would take
the message and the file would never see it."
  (vm-save-test--with-a-folder-of 2
    (let ((buffer (find-file-noselect target)))
      (unwind-protect
          (with-current-buffer buffer
            (setq vm-folder-read-only t)
            (setq major-mode 'vm-mode))
        (let ((vm-visit-when-saving t)
              (text-quoting-style 'grave))
          (should-error (vm-save-message target 1) :type 'error)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest vm-save-test-a-new-folder-is-created-with-the-configured-bits ()
  "`vm-default-folder-permission-bits' decides what a folder VM creates is
readable by.  Mail is not for everyone on the machine."
  (vm-save-test--with-a-folder-of 2
    (let ((vm-default-folder-permission-bits #o600))
      (vm-save-message target 1))
    (should (equal (file-modes target) #o600))))

(ert-deftest vm-save-test-saving-says-how-many-went ()
  "The report says how many messages were saved and where, so a count
given by a prefix argument can be seen to have been taken."
  (vm-save-test--with-a-folder-of 3
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level format &rest args)
                   (setq said (apply #'format format args)))))
        (vm-save-message target 2))
      (should said)
      (should (string-match-p "2 messages saved" said))
      (should (string-match-p "archive" said)))))

(ert-deftest vm-save-test-saving-with-no-count-saves-one ()
  "Called from Lisp with no count, one message is saved: the default is
what a command with no prefix argument means."
  (vm-save-test--with-a-folder-of 3
    (vm-save-message target)
    (should (equal (vm-save-test--subjects-in target) '("msg 1")))))

(ert-deftest vm-save-test-saving-takes-the-messages-it-is-given ()
  "A caller can name the messages, and then the count and the current
message do not decide: `vm-auto-archive-messages' saves this way."
  (vm-save-test--with-a-folder-of 4
    (vm-save-message target 1 (list (nth 2 vm-message-list)
                                    (nth 3 vm-message-list)))
    (should (equal (vm-save-test--subjects-in target) '("msg 3" "msg 4")))))

(ert-deftest vm-save-test-saving-into-a-visited-folder-writes-the-buffer ()
  "With `vm-visit-when-saving', a folder that is being visited takes the
message into its buffer rather than having it appended to the file
underneath it -- which the buffer would then overwrite."
  (vm-save-test--with-a-folder-of 2
    (write-region "" nil target nil 'quiet)
    (let* ((vm-visit-when-saving t)
           (buffer (find-file-noselect target))
           (said nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'vm-inform)
                       (lambda (_level format &rest args)
                         (setq said (apply #'format format args)))))
              (vm-save-message target 1))
            ;; in the buffer, and not yet in the file
            (with-current-buffer buffer
              (should (string-match-p "Subject: msg 1" (buffer-string)))
              (should (buffer-modified-p)))
            (should (equal (vm-save-test--subjects-in target) nil))
            (should (string-match-p "saved to buffer" said)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest vm-save-test-a-read-only-visited-folder-is-refused ()
  "A visited folder that is read-only signals rather than taking the
message: the buffer would hold it and the file would never see it."
  (vm-save-test--with-a-folder-of 2
    (write-region "" nil target nil 'quiet)
    (let* ((vm-visit-when-saving t)
           (buffer (find-file-noselect target)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (setq vm-folder-read-only t))
            (should-error (vm-save-message target 1) :type 'folder-read-only)
            (with-current-buffer buffer
              (should-not (string-match-p "Subject: msg 1" (buffer-string)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest vm-save-test-the-remembered-name-is-not-the-automatic-one ()
  "`vm-last-save-folder' is not overwritten when the folder saved to is the
one VM would have chosen anyway.  It is the reader's last choice, not a
record of what the rules did."
  (vm-save-test--with-a-folder-of 2
    (setq vm-last-save-folder "somewhere-else")
    (cl-letf (((symbol-function 'vm-auto-select-folder)
               (lambda (&rest _) "archive")))
      (vm-save-message "archive" 1))
    (should (equal vm-last-save-folder "somewhere-else"))))

(ert-deftest vm-save-test-a-type-mismatch-is-refused-or-converted ()
  "Saving a From_ message into a folder of another type is refused when
`vm-convert-folder-types' is off, and converted when it is on.  Appending
one format into a folder of another is how a folder stops being readable."
  (vm-save-test--with-a-folder-of 2
    ;; a target VM reads as mboxcl2, which needs a Content-Length
    (write-region "From VM ...\n\n" nil target nil 'quiet)
    (let ((text-quoting-style 'grave))
      (cl-letf (((symbol-function 'vm-get-folder-type) (lambda (&rest _) 'mboxcl2)))
        (let ((vm-convert-folder-types nil)
              (vm-check-folder-types t))
          (let ((err (should-error (vm-save-message target 1) :type 'error)))
            (should (string-match-p "type mismatch" (error-message-string err)))))
        ;; nothing was appended to the folder on the way to refusing
        (should-not (string-match-p "Subject: msg 1"
                                    (with-temp-buffer
                                      (insert-file-contents target)
                                      (buffer-string))))
        ;; and with conversion on, the message is written in the folder's
        ;; own format: an mboxcl2 message carries a Content-Length
        (let ((vm-convert-folder-types t)
              (vm-check-folder-types t))
          (vm-save-message target 1)
          (with-temp-buffer
            (insert-file-contents target)
            (should (string-match-p "Subject: msg 1" (buffer-string)))
            (should (string-match-p "^Content-Length:" (buffer-string)))))))))

(ert-deftest vm-save-test-a-converted-message-keeps-a-separator-the-reader-finds ()
  "An mboxcl2 folder's separators are matched by a regexp that asks only for
a line beginning with From and a space; a From_ folder's asks for a digit at
the end of it too.  Converting reused the message's own line, so saving an
mboxcl2 message whose envelope line ends in a time zone name wrote a From_
folder that read back as empty, with every message's text in it
(emacs-vm/vm#898)."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-save-sep" t)))
         (source (expand-file-name "src.mboxcl2" dir))
         (target (expand-file-name "dest.mbox" dir))
         (vm-folder-directory dir)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-check-folder-types t)
         (vm-convert-folder-types t)
         (before (buffer-list)))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-display) #'ignore)
                  ((symbol-function 'vm-present-current-message) #'ignore))
          (with-temp-buffer
            (dolist (n '(1 2))
              (let ((body (format "Body %d.\n" n)))
                (insert (format "From s%d@example.com Mon Jan  1 00:0%d:00 2024 PST\n"
                                n n)
                        "From: s@example.com\nTo: me@example.com\n"
                        (format "Subject: msg %d\n" n)
                        (format "Content-Length: %d\n" (length body))
                        "\n" body)))
            (write-region (point-min) (point-max) source nil 'quiet))
          (vm-visit-folder source)
          (should (eq vm-folder-type 'mboxcl2))
          (should (equal (length vm-message-list) 2))
          (setq vm-message-pointer vm-message-list)
          (vm-save-message target 2)
          ;; both are in the file
          (should (equal (vm-save-test--folder-subjects target) '("msg 1" "msg 2")))
          ;; and both are where the From_ reader will find them
          (vm-visit-folder target)
          (should (eq vm-folder-type 'From_))
          (should (equal (length vm-message-list) 2)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

;;; Archiving by the auto-folder rules

(defun vm-save-test--folder-subjects (file)
  "The Subject of every message in FILE, in order."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (subjects)
        (while (re-search-forward "^Subject: \\(.*\\)$" nil t)
          (push (match-string 1) subjects))
        (nreverse subjects)))))

(defmacro vm-save-test--archiving (&rest body)
  "Visit a folder of three messages and run BODY, ready to auto-archive.
`vm-auto-folder-alist' sends the odd-numbered subjects to `odd' and the even
ones to `even', both in DIR, so what went where is visible afterwards.  No
confirmation is asked: the command is called from Lisp here."
  (declare (indent 0) (debug t))
  `(vm-save-test--with-a-folder-of 3
     (let ((vm-auto-folder-alist
            (list (list "Subject"
                        (cons "msg [13]" (expand-file-name "odd" dir))
                        (cons "msg 2" (expand-file-name "even" dir)))))
           (vm-save-using-auto-folders t)
           (vm-confirm-for-auto-archive nil)
           (vm-delete-after-archiving nil))
       ,@body)))

(ert-deftest vm-save-test-archiving-files-each-message-by-its-rule ()
  "`vm-auto-archive-messages' saves every message to the folder its rules
name, and marks it filed so that a second archive does not save it again."
  (vm-save-test--archiving
    (vm-auto-archive-messages)
    (should (equal (vm-save-test--folder-subjects (expand-file-name "odd" dir))
                   '("msg 1" "msg 3")))
    (should (equal (vm-save-test--folder-subjects (expand-file-name "even" dir))
                   '("msg 2")))
    (dolist (m vm-message-list)
      (should (vm-filed-flag m)))
    ;; and again, with everything filed, files nothing more
    (vm-auto-archive-messages)
    (should (equal (vm-save-test--folder-subjects (expand-file-name "odd" dir))
                   '("msg 1" "msg 3")))))

(ert-deftest vm-save-test-archiving-passes-over-a-deleted-message ()
  "A message marked for deletion is not archived: it is on its way out, and
filing it would put it in the archive as well as in the folder."
  (vm-save-test--archiving
    (vm-set-deleted-flag (car vm-message-list) t)
    (vm-auto-archive-messages)
    (should (equal (vm-save-test--folder-subjects (expand-file-name "odd" dir))
                   '("msg 3")))
    (should-not (vm-filed-flag (car vm-message-list)))))

(ert-deftest vm-save-test-archiving-can-delete-what-it-filed ()
  "`vm-delete-after-archiving' marks each archived message deleted, which is
what makes archiving a way of emptying the folder."
  (vm-save-test--archiving
    (let ((vm-delete-after-archiving t))
      (vm-auto-archive-messages))
    (should (equal (vm-save-test--folder-subjects (expand-file-name "odd" dir))
                   '("msg 1" "msg 3")))
    (dolist (m vm-message-list)
      (should (vm-deleted-flag m)))))

(ert-deftest vm-save-test-archiving-asks-first-when-told-to ()
  "`vm-confirm-for-auto-archive' is the guard against a mistyped `A': saying
no leaves the folder alone."
  (vm-save-test--archiving
    (let ((vm-confirm-for-auto-archive t)
          (text-quoting-style 'grave))
      ;; `vm-interactive-p' is a macro over `called-interactively-p', so it
      ;; is the latter that a test can stub
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
        (should-error (vm-auto-archive-messages) :type 'error))
      (should-not (vm-save-test--folder-subjects (expand-file-name "odd" dir)))
      (dolist (m vm-message-list)
        (should-not (vm-filed-flag m))))))


;;; The guard on saving bodies into something that is a folder

;; `vm-save-message-sans-headers' writes message bodies with no separators
;; between them, which is right for a plain file and wrong for a folder: the
;; bodies would run together with whatever is there and the folder would be
;; misread from that point on.  So it asks first where the target looks like a
;; folder, and that question is the only thing standing between a reader and a
;; wrecked folder.
;;
;; The tests above cover writing to a plain file.  The line coverage report
;; had the guard among the lines never reached.

(defun vm-save-test--folder-shaped-file (path)
  "Write a From_ folder of one message at PATH."
  (with-temp-file path
    (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
            "From: alice@example.com\nSubject: already here\n\n"
            "a message that is already in this folder\n\n")))

(ert-deftest vm-save-test-saving-bodies-into-a-folder-asks-first ()
  "Pointed at a file that looks like a mail folder, the command asks.
Declining aborts and leaves the file as it was: bodies written into a folder
have no separators, so everything after them is read as part of the message
before."
  (vm-save-test--with-pipe-folder
    (let ((target (make-temp-file "vm-save-guard")))
      (unwind-protect
          (progn
            (vm-save-test--folder-shaped-file target)
            (let ((before (with-temp-buffer (insert-file-contents target)
                                            (buffer-string)))
                  (asked nil))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (prompt) (setq asked prompt) nil)))
                (should-error (vm-save-message-sans-headers target 1 t)))
              (should asked)
              (should (string-match-p "looks like a mail folder" asked))
              ;; nothing written
              (should (equal before
                             (with-temp-buffer (insert-file-contents target)
                                               (buffer-string))))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-saving-bodies-into-a-folder-obeys-a-yes ()
  "Answering yes appends the body anyway, the reader having been warned.
The guard is a question and not a refusal: collecting bodies out of a folder
into another folder is odd but it is the reader's business."
  (vm-save-test--with-pipe-folder
    (let ((target (make-temp-file "vm-save-guard")))
      (unwind-protect
          (progn
            (vm-save-test--folder-shaped-file target)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (vm-save-message-sans-headers target 1 t))
            (let ((after (with-temp-buffer (insert-file-contents target)
                                           (buffer-string))))
              (should (string-match-p "already in this folder" after))
              (should (string-match-p "The body to be piped" after))))
        (ignore-errors (delete-file target))))))

(ert-deftest vm-save-test-saving-bodies-into-a-plain-file-asks-nothing ()
  "A target that is not a folder is written without a question.
The guard has to be quiet in the ordinary case, or it would be in the way of
the command's whole purpose."
  (vm-save-test--with-pipe-folder
    (let ((target (expand-file-name "plain-target" temporary-file-directory)))
      (unwind-protect
          (let ((asked nil))
            (ignore-errors (delete-file target))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt) (setq asked prompt) t)))
              (vm-save-message-sans-headers target 1 t))
            (should-not asked)
            (should (string-match-p "The body to be piped"
                                    (with-temp-buffer
                                      (insert-file-contents target)
                                      (buffer-string)))))
        (ignore-errors (delete-file target))))))


;;; The two guards on saving into a local folder

;; `vm-save-message-to-local-folder' refuses two things before it writes, and
;; the line coverage report had both among the lines never reached.

(ert-deftest vm-save-test-a-new-folder-is-confirmed-when-asked-for ()
  "With `vm-confirm-new-folders' set, saving somewhere new asks first.
Declining aborts and creates nothing.  The option exists because a mistyped
folder name is otherwise a folder, and mail filed into it looks lost."
  (vm-save-test--with-a-folder-of 2
    (let ((vm-confirm-new-folders t)
          (asked nil))
      (should-not (file-exists-p target))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq asked prompt) nil)))
        (should-error (vm-save-message target 1)))
      (should asked)
      (should (string-match-p "does not exist, save there anyway" asked))
      (should-not (file-exists-p target)))))

(ert-deftest vm-save-test-a-new-folder-confirmed-is-written ()
  "Answering yes to that question saves, so the guard is a question only."
  (vm-save-test--with-a-folder-of 2
    (let ((vm-confirm-new-folders t))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-save-message target 1))
      (should (file-exists-p target))
      (should (string-match-p "Subject: msg 1"
                              (with-temp-buffer
                                (insert-file-contents target)
                                (buffer-string)))))))

(ert-deftest vm-save-test-saving-into-a-visited-folder-is-refused ()
  "Saving into a folder that is being visited is an error, not a write.

`vm-visit-when-saving' nil means VM writes the file directly.  Doing that to
a folder someone has open would put the buffer and the file out of step, and
the next save from that buffer would write the buffer over the top of what was
appended.  So it refuses, and says which folder."
  (vm-save-test--with-a-folder-of 2
    (let ((vm-visit-when-saving nil)
          (visiting nil))
      (unwind-protect
          (progn
            ;; something else has the target open
            (with-temp-file target
              (insert "From a@example.com Mon Jan  1 00:00:00 2024\n"
                      "From: a@example.com\nSubject: already\n\nbody\n\n"))
            (setq visiting (find-file-noselect target))
            (let ((error-message
                   (condition-case err
                       (progn (vm-save-message target 1) nil)
                     (error (error-message-string err)))))
              (should error-message)
              (should (string-match-p "being visited" error-message))))
        (when (buffer-live-p visiting)
          (with-current-buffer visiting (set-buffer-modified-p nil))
          (kill-buffer visiting))))))

(provide 'vm-save-test)

;;; vm-save-test.el ends here
