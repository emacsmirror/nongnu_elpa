;;; vm-avirtual-test.el --- Tests for vm-avirtual.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM additional virtual folder selectors in vm-avirtual.el

;;; Code:

(require 'vm-test-init)
(require 'vm-avirtual)

;;; vm-mail-vs-any tests

(ert-deftest vm-avirtual-test-mail-vs-any-returns-t ()
  "Test that vm-mail-vs-any always returns t."
  (should (eq t (vm-mail-vs-any))))

;;; vm-mail-vs-unknown tests

(ert-deftest vm-avirtual-test-mail-vs-unknown-returns-nil ()
  "Test that vm-mail-vs-unknown returns nil."
  (should (null (vm-mail-vs-unknown)))
  (should (null (vm-mail-vs-unknown "any-arg")))
  (should (null (vm-mail-vs-unknown '(complex arg)))))

;;; vm-mail-vs-header tests

(ert-deftest vm-avirtual-test-mail-vs-header-finds-match ()
  "Test vm-mail-vs-header finds matching header."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "Subject: Test Subject\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body text\n")
    (should (vm-mail-vs-header "Subject:.*Test"))))

(ert-deftest vm-avirtual-test-mail-vs-header-no-match ()
  "Test vm-mail-vs-header returns nil when no match."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body text\n")
    (should (null (vm-mail-vs-header "X-Missing:")))))

(ert-deftest vm-avirtual-test-mail-vs-header-not-in-body ()
  "Test vm-mail-vs-header doesn't match in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Subject: in body\n")
    (should (null (vm-mail-vs-header "Subject: in body")))))

;;; vm-mail-vs-text tests

(ert-deftest vm-avirtual-test-mail-vs-text-finds-match ()
  "Test vm-mail-vs-text finds matching text in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body contains special word\n")
    (should (vm-mail-vs-text "special word"))))

(ert-deftest vm-avirtual-test-mail-vs-text-no-match ()
  "Test vm-mail-vs-text returns nil when no match in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body text\n")
    (should (null (vm-mail-vs-text "nonexistent")))))

;;; vm-mail-vs-header-or-text tests

(ert-deftest vm-avirtual-test-mail-vs-header-or-text-finds-header ()
  "Test vm-mail-vs-header-or-text finds match in header."
  (with-temp-buffer
    (insert "From: unique@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body\n")
    (should (vm-mail-vs-header-or-text "unique@example"))))

(ert-deftest vm-avirtual-test-mail-vs-header-or-text-finds-body ()
  "Test vm-mail-vs-header-or-text finds match in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "unique-body-text\n")
    (should (vm-mail-vs-header-or-text "unique-body-text"))))

;;; vm-mail-vs-more-chars-than tests

(ert-deftest vm-avirtual-test-mail-vs-more-chars-than-true ()
  "Test vm-mail-vs-more-chars-than returns t when buffer is larger."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert (make-string 100 ?a))
    (should (vm-mail-vs-more-chars-than 50))))

(ert-deftest vm-avirtual-test-mail-vs-more-chars-than-false ()
  "Test vm-mail-vs-more-chars-than returns nil when buffer is smaller."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "short")
    (should (null (vm-mail-vs-more-chars-than 1000)))))

;;; vm-mail-vs-less-chars-than tests

(ert-deftest vm-avirtual-test-mail-vs-less-chars-than-true ()
  "Test vm-mail-vs-less-chars-than returns t when buffer is smaller."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "short")
    (should (vm-mail-vs-less-chars-than 1000))))

(ert-deftest vm-avirtual-test-mail-vs-less-chars-than-false ()
  "Test vm-mail-vs-less-chars-than returns nil when buffer is larger."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert (make-string 100 ?a))
    (should (null (vm-mail-vs-less-chars-than 50)))))

;;; vm-mail-vs-more-lines-than tests

(ert-deftest vm-avirtual-test-mail-vs-more-lines-than-true ()
  "Test vm-mail-vs-more-lines-than returns t when buffer has more lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (dotimes (_ 10) (insert "line\n"))
    (should (vm-mail-vs-more-lines-than 5))))

(ert-deftest vm-avirtual-test-mail-vs-more-lines-than-false ()
  "Test vm-mail-vs-more-lines-than returns nil when buffer has fewer lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "one line\n")
    (should (null (vm-mail-vs-more-lines-than 100)))))

;;; vm-mail-vs-less-lines-than tests

(ert-deftest vm-avirtual-test-mail-vs-less-lines-than-true ()
  "Test vm-mail-vs-less-lines-than returns t when buffer has fewer lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "short\n")
    (should (vm-mail-vs-less-lines-than 100))))

(ert-deftest vm-avirtual-test-mail-vs-less-lines-than-false ()
  "Test vm-mail-vs-less-lines-than returns nil when buffer has more lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (dotimes (_ 20) (insert "line\n"))
    (should (null (vm-mail-vs-less-lines-than 5)))))

;;; vm-mail-vs-eval tests

(ert-deftest vm-avirtual-test-mail-vs-eval ()
  "Test vm-mail-vs-eval evaluates expression."
  (should (= 6 (vm-mail-vs-eval nil '(+ 1 2 3))))
  (should (string= "hello" (vm-mail-vs-eval nil '"hello"))))

;;; Selector combinator tests

(ert-deftest vm-avirtual-test-mail-vs-and-all-true ()
  "Test vm-mail-vs-and returns t when all selectors match."
  (should (vm-mail-vs-and '(any) '(any))))

(ert-deftest vm-avirtual-test-mail-vs-and-one-false ()
  "Test vm-mail-vs-and returns nil when any selector fails."
  (should (null (vm-mail-vs-and '(any) '(new)))))  ; new returns nil in mail mode

(ert-deftest vm-avirtual-test-mail-vs-or-one-true ()
  "Test vm-mail-vs-or returns t when any selector matches."
  (should (vm-mail-vs-or '(new) '(any))))  ; any always returns t

(ert-deftest vm-avirtual-test-mail-vs-or-all-false ()
  "Test vm-mail-vs-or returns nil when all selectors fail."
  (should (null (vm-mail-vs-or '(new) '(deleted)))))  ; both return nil in mail mode

(ert-deftest vm-avirtual-test-mail-vs-not ()
  "Test vm-mail-vs-not inverts result."
  (should (vm-mail-vs-not '(new)))  ; new returns nil, so not-nil = t
  (should (null (vm-mail-vs-not '(any)))))  ; any returns t, so not-t = nil

;;; vm-virtual-check-case-fold-search tests

(ert-deftest vm-avirtual-test-case-fold-search-default ()
  "Test that vm-virtual-check-case-fold-search defaults to t."
  (should (eq t vm-virtual-check-case-fold-search)))

;;; Mail selector alist tests

(ert-deftest vm-avirtual-test-mail-selector-alist-complete ()
  "Test that mail selector alist has required entries."
  (dolist (sel '(and or not any header text recipient author subject
                 new unread read deleted filed written edited marked
                 undeleted unfiled unwritten unedited unmarked))
    (should (assq sel vm-mail-virtual-selector-function-alist))))

;;; Interactive command tests

(ert-deftest vm-avirtual-test-commands-interactive ()
  "Test that commands are interactive."
  (should (commandp 'vm-add-spam-word))
  (should (commandp 'vm-spam-words-rebuild))
  (should (commandp 'vm-virtual-auto-delete-message))
  (should (commandp 'vm-virtual-save-message)))

;;; Customization group tests

(ert-deftest vm-avirtual-test-customization-group ()
  "Test that vm-avirtual customization group is defined."
  (should (get 'vm-avirtual 'custom-group)))

;;; Omitting a message detaches it (issue #569)

;; A message omitted from a virtual folder used to stay registered as a mirror
;; of its real message.  Expunging the real message then expunged the omitted
;; one from a list it was no longer in, and its stale reverse link took the
;; following message instead: shared attributes carried the wrong expunged flag
;; back to the real folder, whose expunge loop ran on into messages nobody had
;; deleted.  Four messages, one deleted, three lost.
;;
;; These drive real folders rather than a stub: what broke was the interaction
;; between two folders' message lists, which is not visible in either alone.

(defun vm-avirtual-test--write-folder (file n)
  "Write a folder of N messages to FILE."
  (with-temp-file file
    (dotimes (i n)
      (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
              "From: alice@example.com\n"
              (format "Subject: subject %d\n" i)
              (format "Message-ID: <omit-%d@example.com>\n" i)
              "\n"
              (format "Body %d.\n\n" i)))))

(defmacro vm-avirtual-test--with-folders (spec &rest body)
  "Visit a generated real folder and two virtual folders over it, run BODY.
SPEC is (REAL-VAR VIRT-A-VAR VIRT-B-VAR &optional N), each bound to a buffer.
Everything the visits created is killed afterwards."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-avirtual" t)))
          (file (expand-file-name "real-folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-summary-show-threads nil)
          ;; Bound, not set: the folders are defined for the length of the test
          ;; only, and a leaked definition names a directory that is gone.
          (vm-virtual-folder-alist nil)
          ;; Same for what visiting records: these would otherwise carry a
          ;; deleted temporary directory into the tests that follow.
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list))
          ,(car spec) ,(nth 1 spec) ,(nth 2 spec))
     (require 'vm)
     (unwind-protect
         (progn
           (vm-avirtual-test--write-folder file ,(or (nth 3 spec) 4))
           (setq vm-virtual-folder-alist
                 (list (list "omit-a" (list (list file) '(any)))
                       (list "omit-b" (list (list file) '(any)))))
           (vm-visit-folder file)
           (setq ,(car spec) (current-buffer))
           (vm-visit-virtual-folder "omit-a")
           (setq ,(nth 1 spec) (current-buffer))
           (vm-visit-virtual-folder "omit-b")
           (setq ,(nth 2 spec) (current-buffer))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-avirtual-test--subjects (buffer)
  "Return the subjects of BUFFER's message list, in order."
  (with-current-buffer buffer
    (mapcar #'vm-su-subject vm-message-list)))

(defun vm-avirtual-test--links-consistent-p (buffer)
  "Return non-nil if every reverse link in BUFFER is the cons before it."
  (with-current-buffer buffer
    (let ((mp vm-message-list) (prev nil) (ok t))
      (while mp
        (unless (eq (vm-reverse-link-of (car mp)) prev) (setq ok nil))
        (setq prev mp mp (cdr mp)))
      ok)))

(defun vm-avirtual-test--expunge-nth (buffer n)
  "Mark message N of BUFFER deleted, counting from 0, and expunge."
  (with-current-buffer buffer
    (vm-set-deleted-flag (nth n vm-message-list) t)
    (vm-expunge-folder)))

(ert-deftest vm-avirtual-test-omit-deregisters-the-mirror ()
  "An omitted message is no longer a mirror of its real message.
That registration is what let `vm-expunge-folder' reach a message that had left
its folder."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (let (omitted real-m)
      (with-current-buffer virt-a
        (setq omitted (nth 1 vm-message-list))
        (setq real-m (vm-real-message-of omitted))
        (should (memq omitted (vm-virtual-messages-of real-m)))
        (vm-virtual-omit-message 1 (list omitted)))
      (should-not (memq omitted (vm-virtual-messages-of real-m)))
      ;; And it has no reverse link, having no list to be in.
      (should (null (vm-reverse-link-of omitted)))
      ;; The other folder's mirror is untouched.
      (should (= 1 (length (vm-virtual-messages-of real-m))))
      (should (memq (nth 1 (with-current-buffer virt-b vm-message-list))
                    (vm-virtual-messages-of real-m))))))

(ert-deftest vm-avirtual-test-omit-then-expunge-keeps-both-folders ()
  "REGRESSION: expunging after an omit removes only the deleted message.
Issue #569.  Before the fix both folders came back holding message 1 alone: the
omitted message's stale link spliced message 3 out of the virtual folder and
flagged it expunged, and the flag is shared with the real message, so the real
folder's expunge loop went on to take 3 and 4 as well."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (with-current-buffer virt-a
      (vm-virtual-omit-message 1 (list (nth 1 vm-message-list))))
    (should (equal '("subject 0" "subject 2" "subject 3")
                   (vm-avirtual-test--subjects virt-a)))
    (vm-avirtual-test--expunge-nth real 1)
    (should (equal '("subject 0" "subject 2" "subject 3")
                   (vm-avirtual-test--subjects real)))
    (should (equal '("subject 0" "subject 2" "subject 3")
                   (vm-avirtual-test--subjects virt-a)))
    ;; The folder that never omitted anything loses just the expunged message.
    (should (equal '("subject 0" "subject 2" "subject 3")
                   (vm-avirtual-test--subjects virt-b)))
    (should (vm-avirtual-test--links-consistent-p real))
    (should (vm-avirtual-test--links-consistent-p virt-a))
    (should (vm-avirtual-test--links-consistent-p virt-b))
    ;; Nothing else was flagged on the way.
    (with-current-buffer real
      (should (equal '(nil nil nil)
                     (mapcar #'vm-deleted-flag vm-message-list))))))

(ert-deftest vm-avirtual-test-omit-then-expunge-a-later-message ()
  "Omitting one message does not disturb expunging a different one.
The omitted message sits before the expunged one, so a splice that followed its
link would land inside the surviving part of the list."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (ignore virt-b)
    (with-current-buffer virt-a
      (vm-virtual-omit-message 1 (list (nth 1 vm-message-list))))
    (vm-avirtual-test--expunge-nth real 2)   ; subject 2
    (should (equal '("subject 0" "subject 1" "subject 3")
                   (vm-avirtual-test--subjects real)))
    (should (equal '("subject 0" "subject 3")
                   (vm-avirtual-test--subjects virt-a)))
    (should (vm-avirtual-test--links-consistent-p real))
    (should (vm-avirtual-test--links-consistent-p virt-a))))

(ert-deftest vm-avirtual-test-omit-then-expunge-the-omitted-message-elsewhere ()
  "Omitting in one virtual folder leaves the other folders expungeable.
The real message keeps exactly the mirrors that are still in a folder, so the
expunge reaches those and no others."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (with-current-buffer virt-a
      (vm-virtual-omit-message 1 (list (nth 1 vm-message-list))))
    (with-current-buffer virt-b
      (vm-virtual-omit-message 1 (list (nth 1 vm-message-list))))
    (vm-avirtual-test--expunge-nth real 1)
    (dolist (buffer (list real virt-a virt-b))
      (should (equal '("subject 0" "subject 2" "subject 3")
                     (vm-avirtual-test--subjects buffer)))
      (should (vm-avirtual-test--links-consistent-p buffer)))))

(ert-deftest vm-avirtual-test-omit-moves-the-message-pointer-off-it ()
  "Omitting the selected message selects the one before it.
`vm-virtual-omit-message' reaches it through the reverse link.  Left where it
was, the pointer holds a cons that is no longer in the list, and the folder goes
on presenting a message it does not have."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (ignore real virt-b)
    (with-current-buffer virt-a
      (let ((m (nth 1 vm-message-list)))
        (setq vm-message-pointer (cdr vm-message-list))
        (vm-virtual-omit-message 1 (list m))
        (should-not (memq m vm-message-list))
        (should (eq (car vm-message-pointer) (vm-test-first-message)))))))

(ert-deftest vm-avirtual-test-omitting-the-first-message-moves-forward ()
  "Omitting the first message selects the second: there is nothing before it.
The other side of the same branch, where the reverse link is nil."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (ignore real virt-b)
    (with-current-buffer virt-a
      (let ((m (car vm-message-list))
            (next (nth 1 vm-message-list)))
        (setq vm-message-pointer vm-message-list)
        (vm-virtual-omit-message 1 (list m))
        (should (eq next (car vm-message-pointer)))
        (should (eq next (vm-test-first-message)))
        (should (null (vm-reverse-link-of next)))))))

;;; Automatic deletion by selector

;; `vm-virtual-auto-delete-message' is what goes on `vm-arrived-messages-hook'
;; to flag spam as it arrives, and with `vm-virtual-auto-delete-message-expunge'
;; set it expunges immediately.  It had no test, and it is the only caller of
;; `vm-expunge-folder' with `:quiet t' and `:just-these-messages'.

(defmacro vm-avirtual-test--with-spam-selector (&rest body)
  "Run BODY in a visited folder of five messages with a spam selector defined.
The selector matches \"subject 1\" and so exactly one message of the five."
  (declare (indent 0) (debug t))
  `(vm-test-with-real-folder (5)
     (let ((vm-virtual-folder-alist
            '(("spam" (("does-not-matter") (subject "subject 1")))))
           (vm-virtual-auto-delete-message-selector "spam")
           (vm-virtual-auto-delete-message-folder nil)
           (vm-virtual-auto-delete-message-expunge nil))
       (setq vm-message-pointer vm-message-list)
       ,@body)))

(defun vm-avirtual-test--deleted-flags ()
  "Return the deleted flag of each message as t or nil, in order."
  (mapcar (lambda (m) (and (vm-deleted-flag m) t)) vm-message-list))

(ert-deftest vm-avirtual-test-auto-delete-flags-what-the-selector-matches ()
  "The matching message is flagged and labelled, and the others are untouched.
The label is the selector's name, which is how the summary shows why a message
was flagged."
  (vm-avirtual-test--with-spam-selector
    (vm-virtual-auto-delete-message 5)
    (should (equal '(nil t nil nil nil) (vm-avirtual-test--deleted-flags)))
    (should (equal '("spam") (vm-labels-of (nth 1 vm-message-list))))
    (should (null (vm-labels-of (car vm-message-list))))
    ;; Flagged, not expunged: the folder still has all five.
    (should (= 5 (length vm-message-list)))))

(ert-deftest vm-avirtual-test-auto-delete-expunges-when-told-to ()
  "With the expunge option set the matching message leaves the folder at once.
This is the one caller of `vm-expunge-folder' with `:just-these-messages', so
only the matched message goes, and the folder is left consistent."
  (vm-avirtual-test--with-spam-selector
    (let ((vm-virtual-auto-delete-message-expunge t))
      (vm-virtual-auto-delete-message 5)
      (should (equal '("subject 0" "subject 2" "subject 3" "subject 4")
                     (mapcar #'vm-su-subject vm-message-list)))
      (should (vm-test-reverse-links-consistent-p))
      (should (equal '(nil nil nil nil) (vm-avirtual-test--deleted-flags))))))

(ert-deftest vm-avirtual-test-auto-delete-leaves-non-matching-folders-alone ()
  "A selector that matches nothing flags nothing and does not fail."
  (vm-test-with-real-folder (5)
    (let ((vm-virtual-folder-alist
           '(("spam" (("does-not-matter") (subject "nothing matches this")))))
          (vm-virtual-auto-delete-message-selector "spam")
          (vm-virtual-auto-delete-message-folder nil)
          (vm-virtual-auto-delete-message-expunge t))
      (setq vm-message-pointer vm-message-list)
      (vm-virtual-auto-delete-message 5)
      (should (= 5 (length vm-message-list)))
      (should (equal '(nil nil nil nil nil) (vm-avirtual-test--deleted-flags))))))

(ert-deftest vm-avirtual-test-auto-delete-messages-starts-at-the-current-one ()
  "`vm-virtual-auto-delete-messages' covers the current message to the last.
That is what makes it right for `vm-arrived-messages-hook', where the pointer
sits at the first of the messages that just arrived: a match earlier in the
folder is not flagged."
  (vm-avirtual-test--with-spam-selector
    ;; Point past the matching message.
    (setq vm-message-pointer (nthcdr 2 vm-message-list))
    (vm-virtual-auto-delete-messages)
    (should (equal '(nil nil nil nil nil) (vm-avirtual-test--deleted-flags)))
    ;; From before it, the same command does flag it.
    (setq vm-message-pointer vm-message-list)
    (vm-virtual-auto-delete-messages)
    (should (equal '(nil t nil nil nil) (vm-avirtual-test--deleted-flags)))))

;;; Filtering by a table of selectors: vm-virtual-filter-alist

;; `vm-virtual-auto-delete-message' drives one hard-wired selector through one
;; hard-wired action list.  `vm-virtual-filter-alist' is the table of them
;; asked for in issue #542.

(defmacro vm-avirtual-test--with-filter-folder (&rest body)
  "Run BODY in a visited folder of five messages with two selectors defined.
\"one\" matches \"subject 1\" and \"three\" matches \"subject 3\", so each
picks out exactly one message of the five."
  (declare (indent 0) (debug t))
  `(vm-test-with-real-folder (5)
     (let ((vm-virtual-folder-alist
            '(("one"   (("does-not-matter") (subject "subject 1")))
              ("three" (("does-not-matter") (subject "subject 3")))))
           (vm-virtual-filter-alist nil))
       (setq vm-message-pointer vm-message-list)
       ,@body)))

(ert-deftest vm-avirtual-test-filter-labels-and-attributes ()
  "A rule labels and sets attributes on what its selector matches, only.
The names are the same ones `vm-add-message-labels' and
`vm-set-message-attributes' take, space separated."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist
           '(("one" :label "mine urgent" :attributes "read flagged"))))
      (should (= 1 (vm-virtual-filter-messages 5)))
      (let ((m (nth 1 vm-message-list)))
        (should (equal '("mine" "urgent") (sort (vm-labels-of m) #'string<)))
        (should (null (vm-new-flag m)))
        (should (null (vm-unread-flag m)))
        (should (vm-flagged-flag m)))
      ;; the other four are untouched
      (should (null (vm-labels-of (car vm-message-list))))
      (should (vm-new-flag (car vm-message-list))))))

(ert-deftest vm-avirtual-test-filter-every-matching-rule-runs ()
  "Two rules matching the same message both apply, in the order listed."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist
           '(("one" :label "first")
             ("one" :label "second" :attributes "read"))))
      (should (= 1 (vm-virtual-filter-messages 5)))
      (let ((m (nth 1 vm-message-list)))
        (should (equal '("first" "second") (sort (vm-labels-of m) #'string<)))
        (should (null (vm-new-flag m)))))))

(ert-deftest vm-avirtual-test-filter-skip-inbox-expunges ()
  "`:skip-inbox' takes the message back out of the folder.
The message is assimilated before any of this runs, so skipping the inbox is
a delete and an expunge after the fact, and the folder must be left
consistent by it."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist '(("three" :skip-inbox t))))
      (should (= 1 (vm-virtual-filter-messages 5)))
      (should (equal '("subject 0" "subject 1" "subject 2" "subject 4")
                     (mapcar #'vm-su-subject vm-message-list)))
      (should (vm-test-reverse-links-consistent-p)))))

(ert-deftest vm-avirtual-test-filter-expunges-once-for-all-rules ()
  "Two rules skipping two different messages remove both, and only those."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist '(("one"   :skip-inbox t)
                                     ("three" :skip-inbox t))))
      (should (= 2 (vm-virtual-filter-messages 5)))
      (should (equal '("subject 0" "subject 2" "subject 4")
                     (mapcar #'vm-su-subject vm-message-list)))
      (should (vm-test-reverse-links-consistent-p)))))

(ert-deftest vm-avirtual-test-filter-reports-what-it-did ()
  "The report counts every skipped message, not just the last one.
`vm-expunge-folder' is handed the list with `nreverse', which leaves the
variable pointing at its last cell, so the count has to be taken from the
reversed list."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist '(("one"   :skip-inbox t)
                                     ("three" :skip-inbox t)))
          (said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (setq said (apply #'format args)))))
        (vm-virtual-filter-messages 5))
      (should (equal "2 messages filtered, 2 expunged" said)))))

(ert-deftest vm-avirtual-test-filter-saves-to-a-folder ()
  "`:save' writes a copy of the message into the named folder."
  (vm-avirtual-test--with-filter-folder
    (let* ((saved (expand-file-name "saved" dir))
           (vm-virtual-filter-alist (list (list "one" :save saved)))
           (vm-confirm-new-folders nil))
      (should (= 1 (vm-virtual-filter-messages 5)))
      (should (file-exists-p saved))
      (with-temp-buffer
        (insert-file-contents saved)
        (should (string-match-p "subject 1" (buffer-string)))
        (should-not (string-match-p "subject 0" (buffer-string)))))))

(ert-deftest vm-avirtual-test-filter-save-takes-an-expression ()
  "`:save' evaluates a non-string, so the folder can be computed."
  (vm-avirtual-test--with-filter-folder
    (let* ((saved (expand-file-name "computed" dir))
           (vm-virtual-filter-alist
            (list (list "one" :save (list 'expand-file-name "computed" dir))))
           (vm-confirm-new-folders nil))
      (vm-virtual-filter-messages 5)
      (should (file-exists-p saved)))))

(ert-deftest vm-avirtual-test-filter-unmatched-message-is-untouched ()
  "A table whose selectors match nothing changes nothing and reports none."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-folder-alist
           '(("nobody" (("does-not-matter") (subject "no such subject")))))
          (vm-virtual-filter-alist '(("nobody" :label "x" :skip-inbox t))))
      (should (= 0 (vm-virtual-filter-messages 5)))
      (should (= 5 (length vm-message-list)))
      (should (equal '(nil nil nil nil nil) (vm-avirtual-test--deleted-flags))))))

(ert-deftest vm-avirtual-test-filter-unknown-folder-errors ()
  "A rule naming a virtual folder that does not exist is an error.
`vm-virtual-get-selector' returns nil for an unknown name, so without this
the rule would quietly match nothing and the user would be left looking for
the typo."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist '(("noe" :label "x")))
          (text-quoting-style 'grave))
      (let ((err (should-error (vm-virtual-filter-messages 5) :type 'error)))
        (should (string-match-p "No virtual folder \"noe\"" (cadr err)))
        ;; solution-directed: says where to define it and what is defined
        (should (string-match-p "vm-virtual-folder-alist" (cadr err)))
        (should (string-match-p "\"one\", \"three\"" (cadr err)))))))

(ert-deftest vm-avirtual-test-filter-new-messages-starts-at-the-current-one ()
  "`vm-virtual-filter-new-messages' covers the current message to the last.
That is what makes it right for `vm-arrived-messages-hook', where the pointer
sits at the first of the messages that just arrived: a match before it is left
alone."
  (vm-avirtual-test--with-filter-folder
    (let ((vm-virtual-filter-alist '(("one" :label "x"))))
      ;; Point past the matching message.
      (setq vm-message-pointer (nthcdr 2 vm-message-list))
      (vm-virtual-filter-new-messages)
      (should (null (vm-labels-of (nth 1 vm-message-list))))
      ;; From before it, the same command does label it.
      (setq vm-message-pointer vm-message-list)
      (vm-virtual-filter-new-messages)
      (should (equal '("x") (vm-labels-of (nth 1 vm-message-list)))))))

(provide 'vm-avirtual-test)

;;; vm-avirtual-test.el ends here