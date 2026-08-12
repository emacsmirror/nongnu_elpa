;;; vm-save-test.el --- Tests for vm-save.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

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

(defmacro vm-save-test--with-pipe-folder (&rest body)
  "Visit the piping fixture and run BODY in the folder buffer."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-pipe" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-last-pipe-command nil))
           (write-region vm-save-test--pipe-folder nil folder nil 'quiet)
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

(provide 'vm-save-test)

;;; vm-save-test.el ends here
