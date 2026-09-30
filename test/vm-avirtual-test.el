;;; vm-avirtual-test.el --- Tests for vm-avirtual.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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

;;; Virtual folder maintenance (emacs-vm/vm#632)

(defconst vm-avirtual-test--folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nSubject: badgers\n\nOne.\n\n"
          "From bob@example.com Sun Aug  9 16:00:00 2026\n"
          "From: bob@example.com\nSubject: otters\n\nTwo.\n\n")
  "Two messages, one of which an archive rule will match.")

(defmacro vm-avirtual-test--with-folder (spec &rest body)
  "Visit a folder of two messages and run BODY in it.
SPEC is (FILE-VAR ARCHIVE-VAR): the folder's file, and a name for an archive
folder that does not exist yet."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-avirtual" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "inbox" dir))
               (,(cadr spec) (expand-file-name "badger-archive" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-confirm-for-auto-archive nil)
               (vm-delete-after-archiving nil)
               (vm-virtual-folder-alist nil)
               (vm-virtual-auto-folder-alist nil))
           (write-region vm-avirtual-test--folder nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-avirtual-test-auto-select-folder-follows-the-rules ()
  "`vm-virtual-auto-select-folder' names the folder a message belongs in,
by finding a virtual folder whose selectors match it and looking that up in
`vm-virtual-auto-folder-alist'.

The entries of that alist are two-element lists.  Its docstring said
\"(VIRTUAL-FOLDER-NAME . FOLDER-NAME)\" until this was written, and a dotted
pair signals `wrong-type-argument listp': the entry is read with `cadr'."
  (vm-avirtual-test--with-folder (file archive)
    (let ((vm-virtual-folder-alist
           (list (list "badger-mail" (list (list file) '(subject "badgers")))))
          (vm-virtual-auto-folder-alist
           (list (list "badger-mail" archive))))
      (should (equal (vm-virtual-auto-select-folder (car vm-message-list))
                     archive))
      ;; the other message matches no virtual folder, so it has no home
      (should-not (vm-virtual-auto-select-folder (cadr vm-message-list))))))

(ert-deftest vm-avirtual-test-auto-archive-files-what-the-rules-match ()
  "`vm-virtual-auto-archive-messages' saves each message to the folder its
rules name, and leaves the messages no rule matches where they are."
  (vm-avirtual-test--with-folder (file archive)
    (let ((vm-virtual-folder-alist
           (list (list "badger-mail" (list (list file) '(subject "badgers")))))
          (vm-virtual-auto-folder-alist
           (list (list "badger-mail" archive))))
      (should-not (file-exists-p archive))
      (vm-virtual-auto-archive-messages)
      (should (file-exists-p archive))
      (let ((archived (with-temp-buffer (insert-file-contents archive)
                                        (buffer-string))))
        (should (string-match-p "Subject: badgers" archived))
        (should-not (string-match-p "Subject: otters" archived)))
      ;; the folder still holds both: archiving copies unless
      ;; vm-delete-after-archiving says otherwise
      (should (equal (length vm-message-list) 2)))))

(ert-deftest vm-avirtual-test-auto-archive-with-no-rules-does-nothing ()
  "With no rules nothing is archived, and nothing new is written.

The whole directory is compared, not just the folder the other tests archive
to: a command that filed everything under a name of its own making would
leave that file alone and pass a narrower test."
  (vm-avirtual-test--with-folder (file _archive)
    (let* ((dir (file-name-directory file))
           (before (sort (directory-files dir) #'string<)))
      (vm-virtual-auto-archive-messages)
      (should (equal (sort (directory-files dir) #'string<) before))
      (should (equal (length vm-message-list) 2)))))


;;; The composition-side selectors (emacs-vm/vm#648)
;;
;; These answer about the message being written, not about one in a folder:
;; vm-pcrisis conditions and auto-virtual selectors run them in the
;; composition buffer.

(defmacro vm-avirtual-test--in-a-composition (headers &rest body)
  "Run BODY in a composition whose headers are HEADERS.
HEADERS is inserted before `mail-header-separator', which is what every one
of these selectors splits the buffer on."
  (declare (indent 1) (debug t))
  `(let ((mail-header-separator "--text follows this line--"))
     (with-temp-buffer
       (mail-mode)
       (insert ,headers mail-header-separator "\n"
               "The body of the message.\n")
       ,@body)))

(ert-deftest vm-avirtual-test-replied-and-forwarded-read-their-own-list ()
  "`replied' and `forwarded' report what the composition was started from.
VM records the messages in `vm-reply-list' and `vm-forward-list' when it
sets the composition up."
  (vm-avirtual-test--in-a-composition "To: someone@example.com\n"
    (let ((vm-reply-list nil) (vm-forward-list nil))
      (should-not (vm-mail-vs-replied))
      (should-not (vm-mail-vs-forwarded)))
    (let ((vm-reply-list '(a-message)) (vm-forward-list nil))
      (should (vm-mail-vs-replied))
      (should-not (vm-mail-vs-forwarded)))
    (let ((vm-reply-list nil) (vm-forward-list '(a-message)))
      (should-not (vm-mail-vs-replied))
      (should (vm-mail-vs-forwarded)))))

(ert-deftest vm-avirtual-test-unreplied-is-about-replying ()
  "REGRESSION: `unreplied' asks whether the composition is a reply.

It called `vm-mail-vs-forwarded', so it answered about forwarding: a reply
matched `unreplied', and a forward did not.  Both `unreplied' and
`unanswered' are in `vm-mail-virtual-selector-function-alist', so any
vm-pcrisis condition or auto-virtual selector written with either got the
wrong answer."
  (vm-avirtual-test--in-a-composition "To: someone@example.com\n"
    ;; a reply is not unreplied
    (let ((vm-reply-list '(a-message)) (vm-forward-list nil))
      (should-not (vm-mail-vs-unreplied))
      (should-not (vm-mail-vs-unanswered)))
    ;; a forward is: it is not a reply
    (let ((vm-reply-list nil) (vm-forward-list '(a-message)))
      (should (vm-mail-vs-unreplied))
      (should (vm-mail-vs-unanswered)))
    ;; and a composition started from nothing is unreplied and unforwarded
    (let ((vm-reply-list nil) (vm-forward-list nil))
      (should (vm-mail-vs-unreplied))
      (should (vm-mail-vs-unforwarded)))))

(ert-deftest vm-avirtual-test-redistribution-is-read-from-the-headers ()
  "`redistributed' is a header rather than a list: VM writes Resent- headers
into the composition, and any of them counts."
  (vm-avirtual-test--in-a-composition "To: someone@example.com\n"
    (should-not (vm-mail-vs-redistributed))
    (should (vm-mail-vs-unredistributed)))
  (vm-avirtual-test--in-a-composition
      "To: someone@example.com\nResent-To: another@example.com\n"
    (should (vm-mail-vs-redistributed))
    (should-not (vm-mail-vs-unredistributed))))

(ert-deftest vm-avirtual-test-recipient-covers-every-recipient-header ()
  "`recipient' matches To, CC and BCC, and their Resent- forms.
A rule about who a message is going to must not miss the ones that are
addressed only in CC, or only as a redistribution."
  (dolist (header '("To" "CC" "BCC" "Resent-To" "Resent-CC" "Resent-BCC"))
    (vm-avirtual-test--in-a-composition
        (concat header ": someone@example.com\n")
      (should (vm-mail-vs-recipient "someone@example\\.com"))
      (should (vm-mail-vs-author-or-recipient "someone@example\\.com"))
      (should-not (vm-mail-vs-recipient "nobody@example\\.com")))))

(ert-deftest vm-avirtual-test-author-and-principal-are-different-headers ()
  "`author' reads From (or Sender) and `principal' reads Reply-To, so a
composition that redirects replies elsewhere is matched by the right one."
  (vm-avirtual-test--in-a-composition
      "From: writer@example.com\nReply-To: list@example.com\n"
    (should (vm-mail-vs-author "writer@example\\.com"))
    (should-not (vm-mail-vs-author "list@example\\.com"))
    (should (vm-mail-vs-principal "list@example\\.com"))
    (should-not (vm-mail-vs-principal "writer@example\\.com"))))

(ert-deftest vm-avirtual-test-sortable-subject-ignores-the-reply-prefix ()
  "`subject' matches what is written; `sortable-subject' matches the subject
with the Re: stripped, which is how a rule follows a thread."
  (vm-avirtual-test--in-a-composition "Subject: Re: the topic\n"
    (should (vm-mail-vs-subject "Re: the topic"))
    (should-not (vm-mail-vs-subject "\\`the topic"))
    (should (vm-mail-vs-sortable-subject "\\`the topic"))))

(ert-deftest vm-avirtual-test-header-and-text-stop-at-the-separator ()
  "`header' searches above `mail-header-separator' and `text' below it, so a
word in the body cannot match a header rule and the reverse."
  (vm-avirtual-test--in-a-composition "Subject: a distinctive word\n"
    (should (vm-mail-vs-header "distinctive"))
    (should-not (vm-mail-vs-text "distinctive"))
    (should (vm-mail-vs-text "body of the message"))
    (should-not (vm-mail-vs-header "body of the message"))
    ;; and header-or-text takes either
    (should (vm-mail-vs-header-or-text "distinctive"))
    (should (vm-mail-vs-header-or-text "body of the message"))))

(ert-deftest vm-avirtual-test-older-and-newer-than-read-the-date ()
  "`older-than' and `newer-than' count days from the Date header, and a
composition without one matches neither."
  (let ((old (format-time-string "%a, %d %b %Y %H:%M:%S %z"
                                 (time-subtract (current-time)
                                                (days-to-time 10)))))
    (vm-avirtual-test--in-a-composition (concat "Date: " old "\n")
      (should (vm-mail-vs-older-than 5))
      (should-not (vm-mail-vs-older-than 20))
      (should (vm-mail-vs-newer-than 20))
      (should-not (vm-mail-vs-newer-than 5))))
  (vm-avirtual-test--in-a-composition "To: someone@example.com\n"
    (should-not (vm-mail-vs-older-than 1))
    (should-not (vm-mail-vs-newer-than 1))))

;;; Making a virtual folder persistent (emacs-vm/vm#665)

(ert-deftest vm-avirtual-test-making-a-folder-persistent-saves-them-all ()
  "Every message of the virtual folder is written to a real folder named
after it, not just the one at point.  The name is the buffer's without the
parentheses VM puts around a virtual folder's name."
  (vm-avirtual-test--with-folders (real virt-a virt-b 4)
    (ignore real virt-b)
    (with-current-buffer virt-a
      (let ((saved nil))
        (cl-letf (((symbol-function 'vm-save-message)
                   (lambda (folder &optional count &rest _)
                     (setq saved (cons folder count)))))
          (vm-virtual-make-folder-persistent))
        (should (equal (car saved) (substring (buffer-name) 1 -1)))
        (should (= (cdr saved) (length vm-message-list)))
        (should (= (cdr saved) 4))))))

(ert-deftest vm-avirtual-test-making-a-real-folder-persistent-is-refused ()
  "A real folder is already on disk, so the command says what it is for
rather than saving the folder over itself."
  (vm-avirtual-test--with-folders (real virt-a virt-b)
    (ignore virt-a virt-b)
    (with-current-buffer real
      (let ((text-quoting-style 'grave))
        (cl-letf (((symbol-function 'vm-save-message)
                   (lambda (&rest _) (error "saved a real folder"))))
          (let ((err (should-error (vm-virtual-make-folder-persistent)
                                   :type 'error)))
            (should (string-match-p "not a virtual folder"
                                    (error-message-string err)))))))))

;;; The check for selectors one side has and the other lacks

(defmacro vm-avirtual-test--checking-selectors (message-side mail-side &rest body)
  "Run BODY with the two selector tables bound and `message' captured.
BODY sees REPORT, what the check said."
  (declare (indent 2) (debug t))
  `(let ((vm-virtual-selector-function-alist ,message-side)
         (vm-mail-virtual-selector-function-alist ,mail-side)
         (report nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args)
                  (setq report (apply #'format format args)))))
       ,@body)
     report))

(ert-deftest vm-avirtual-test-the-selector-check-names-what-is-missing ()
  "The check reports the selectors the other table lacks, by name.  It is
the only thing that notices the two tables drifting apart."
  (let ((report (vm-avirtual-test--checking-selectors
                    '((author . vm-vs-author) (folder-name . vm-vs-folder-name))
                    '((author . vm-mail-vs-author))
                  (vm-avirtual-check-for-missing-selectors))))
    (should (string-match-p "folder-name" report))
    (should (string-match-p "missing" report))
    ;; the one both tables have is not reported
    (should-not (string-match-p "author" report))))

(ert-deftest vm-avirtual-test-the-selector-check-is-quiet-when-they-agree ()
  "Two tables offering the same selectors report nothing missing."
  (let ((report (vm-avirtual-test--checking-selectors
                    '((author . vm-vs-author))
                    '((author . vm-mail-vs-author))
                  (vm-avirtual-check-for-missing-selectors))))
    (should (equal report "No selectors are missing"))))

(ert-deftest vm-avirtual-test-the-selector-check-looks-the-other-way-too ()
  "With a prefix argument the check runs the other way round, reporting what
the composition side has and the message side lacks."
  (let ((report (vm-avirtual-test--checking-selectors
                    '((author . vm-vs-author))
                    '((author . vm-mail-vs-author) (mail-mode . vm-mail-vs-mail-mode))
                  (vm-avirtual-check-for-missing-selectors t))))
    (should (string-match-p "mail-mode" report)))
  ;; and without it, that same pair reports nothing: the message side is
  ;; the smaller of the two here
  (let ((report (vm-avirtual-test--checking-selectors
                    '((author . vm-vs-author))
                    '((author . vm-mail-vs-author) (mail-mode . vm-mail-vs-mail-mode))
                  (vm-avirtual-check-for-missing-selectors))))
    (should (equal report "No selectors are missing"))))

(ert-deftest vm-avirtual-test-every-composition-selector-has-a-message-one ()
  "The composition selectors are a subset of the message ones.

A composition has no folder, no flags and no uid, so the message side is
larger; but a selector offered for compositions and not for messages would
be one a reader could write in a vm-pcrisis condition and not in a virtual
folder, which is a difference nobody intended.

Not under instrumentation: edebug evaluates a `defvar' as \[eval-defun] does,
which resets it, so instrumenting vm-vars.el throws away the selectors this
file adds to `vm-virtual-selector-function-alist' as it loads, and the
top-level call that added them is not re-run (emacs-vm/vm#870)."
  (skip-unless (not vm-test-instrumented))
  (let ((missing (seq-remove
                  (lambda (name) (assq name vm-virtual-selector-function-alist))
                  (mapcar #'car vm-mail-virtual-selector-function-alist))))
    (should (equal missing nil))))

;;; Adding a message to the virtual folders that want it (emacs-vm/vm#638)

(defmacro vm-avirtual-test--with-a-labelled-folder (order &rest body)
  "Visit a folder of two messages and a virtual folder of the labelled ones.
ORDER is `folder-first' or `virtual-first': which is visited first decides
which buffer the virtual folder's clause resolves to, and the update only
considers messages from that buffer.  FOLDER and VIRTUAL are the buffers."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-avirtual-update" t)))
          (file (expand-file-name "inbox" dir))
          (vm-folder-directory dir)
          (vm-virtual-folder-alist
           (list (list "wanted-only" (list (list file) '(label "wanted")))))
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list))
          folder virtual)
     (unwind-protect
         (progn
           (with-temp-file file
             (dolist (subject '("one" "two"))
               (insert (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                                       "From: alice@example.com\n"
                                       "Subject: %s\n\nThe body.\n\n")
                               subject))))
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (if (eq ,order 'virtual-first)
                 (progn
                   (vm-visit-virtual-folder "wanted-only")
                   (setq virtual (current-buffer))
                   (setq folder (or (vm-get-folder-buffer file)
                                    (progn (vm-visit-folder file) (current-buffer)))))
               (vm-visit-folder file)
               (setq folder (current-buffer))
               (vm-visit-virtual-folder "wanted-only")
               (setq virtual (current-buffer)))
             ,@body))
       (dolist (buffer (buffer-list))
         (when (and (buffer-live-p buffer) (not (memq buffer before)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory dir t))))

(defun vm-avirtual-test--virtual-subjects (virtual)
  "The subjects the virtual folder holds."
  (with-current-buffer virtual (mapcar #'vm-su-subject vm-message-list)))

(ert-deftest vm-avirtual-test-updating-adds-a-message-that-now-matches ()
  "A message that has come to match an open virtual folder is added to it
by `vm-virtual-update-folders'.

Labelling a message does not itself add it anywhere: the virtual folder is
still empty afterwards, which is what the command is for."
  (vm-avirtual-test--with-a-labelled-folder 'folder-first
    (should (equal (vm-avirtual-test--virtual-subjects virtual) nil))
    (with-current-buffer folder
      (setq vm-message-pointer (cdr vm-message-list))
      (vm-add-message-labels "wanted" 1)
      (should (equal (vm-avirtual-test--virtual-subjects virtual) nil))
      (vm-virtual-update-folders 1))
    (should (equal (vm-avirtual-test--virtual-subjects virtual) '("two")))))

(ert-deftest vm-avirtual-test-updating-works-whichever-was-visited-first ()
  "The virtual folder may have opened the real one itself, in which case
the message is in a buffer the virtual folder opened rather than one the
reader did.  The update has to reach it either way, since it only
considers messages from the buffer its own clause names."
  (vm-avirtual-test--with-a-labelled-folder 'virtual-first
    (with-current-buffer folder
      (setq vm-message-pointer (cdr vm-message-list))
      (vm-add-message-labels "wanted" 1)
      (vm-virtual-update-folders 1))
    (should (equal (vm-avirtual-test--virtual-subjects virtual) '("two")))))

(ert-deftest vm-avirtual-test-updating-leaves-out-what-does-not-match ()
  "A message that matches nothing is not added, and one already there is
not added twice."
  (vm-avirtual-test--with-a-labelled-folder 'folder-first
    (with-current-buffer folder
      ;; the first message never gets the label
      (setq vm-message-pointer vm-message-list)
      (vm-virtual-update-folders 1)
      (should (equal (vm-avirtual-test--virtual-subjects virtual) nil))
      ;; the second does, twice over
      (setq vm-message-pointer (cdr vm-message-list))
      (vm-add-message-labels "wanted" 1)
      (vm-virtual-update-folders 1)
      (vm-virtual-update-folders 1))
    (should (equal (vm-avirtual-test--virtual-subjects virtual) '("two")))))

(ert-deftest vm-avirtual-test-updating-takes-the-messages-it-is-given ()
  "A caller can name the messages rather than leaving the command to take
the count and the current message."
  (vm-avirtual-test--with-a-labelled-folder 'folder-first
    (with-current-buffer folder
      (let ((second (nth 1 vm-message-list)))
        ;; label the second while the pointer is on the first, so the
        ;; message list given to the command is what decides
        (setq vm-message-pointer (cdr vm-message-list))
        (vm-add-message-labels "wanted" 1)
        (setq vm-message-pointer vm-message-list)
        (vm-virtual-update-folders 1 (list second))))
    (should (equal (vm-avirtual-test--virtual-subjects virtual) '("two")))))

;;; The spam word list

(defmacro vm-avirtual-test--with-spam-words (words &rest body)
  "Run BODY with a spam words file holding WORDS, a list of strings.
The file, the list read from it and the regexp built from it are all
temporary: `vm-spam-words' is a cache of the file and outlives a test that
does not put it back."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-spam" t)))
          (vm-spam-words-file (expand-file-name "spam-words" dir))
          (vm-spam-words nil)
          (vm-spam-words-regexp nil)
          (before (buffer-list)))
     (unwind-protect
         (progn
           (with-temp-file vm-spam-words-file
             (dolist (word ,words) (insert word "\n")))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-avirtual-test--spam-words-on-disk ()
  "The words the spam words file holds."
  (with-temp-buffer
    (insert-file-contents vm-spam-words-file)
    (split-string (buffer-string) "\n" t)))

(ert-deftest vm-avirtual-test-a-spam-word-is-added-to-the-file ()
  "`vm-add-spam-word' writes the word to `vm-spam-words-file', which is what
makes it survive the session, and does not add it twice."
  (vm-avirtual-test--with-spam-words '("lottery")
    ;; the list is read from the file by the selector, not by the adding
    (vm-vs-spam-word nil)
    (should (member "lottery" vm-spam-words))
    (vm-add-spam-word "viagra")
    (should (equal (sort (vm-avirtual-test--spam-words-on-disk) #'string<)
                   '("lottery" "viagra")))
    (vm-add-spam-word "viagra")
    (should (equal (sort (vm-avirtual-test--spam-words-on-disk) #'string<)
                   '("lottery" "viagra")))))

(ert-deftest vm-avirtual-test-a-spam-word-file-without-a-final-newline ()
  "A word is added on a line of its own even when the file does not end in a
newline: two words on one line are one word nobody matches."
  (vm-avirtual-test--with-spam-words '("lottery")
    (with-temp-file vm-spam-words-file (insert "lottery"))
    (vm-add-spam-word "viagra")
    (should (equal (vm-avirtual-test--spam-words-on-disk)
                   '("lottery" "viagra")))))

(ert-deftest vm-avirtual-test-rebuilding-reads-the-file-again ()
  "The file is read once and cached, so a word added to it by hand is not
seen until `vm-spam-words-rebuild' throws the cache away and reads it again.
That is what the command is for."
  (vm-avirtual-test--with-spam-words '("lottery")
    (vm-vs-spam-word nil)
    (should (equal vm-spam-words '("lottery")))
    (with-temp-file vm-spam-words-file (insert "lottery\nviagra\n"))
    ;; still the old list: nothing re-reads the file by itself
    (vm-vs-spam-word nil)
    (should (equal vm-spam-words '("lottery")))
    (vm-spam-words-rebuild)
    (should (equal (sort (copy-sequence vm-spam-words) #'string<)
                   '("lottery" "viagra")))
    (should (string-match-p "viagra" vm-spam-words-regexp))))

(ert-deftest vm-avirtual-test-a-comment-is-not-a-spam-word ()
  "Lines beginning # or ; are comments, so a file can say what a word is for."
  (vm-avirtual-test--with-spam-words
      '("# the words that fill my inbox" "lottery" "; and another comment")
    (vm-vs-spam-word nil)
    (should (equal vm-spam-words '("lottery")))))

;;; Commands that had no test

(ert-deftest vm-avirtual-test-saving-uses-the-folder-the-rules-name ()
  "`vm-virtual-save-message' is `vm-save-message' with the folder guessed:
called from Lisp with a folder it saves there, and the guess is what the
interactive form offers as the default."
  (vm-avirtual-test--with-folder (file archive)
    (let ((vm-virtual-folder-alist
           (list (list "badger-mail" (list (list file) '(subject "badgers")))))
          (vm-virtual-auto-folder-alist
           (list (list "badger-mail" archive)))
          (vm-confirm-new-folders nil)
          (vm-visit-when-saving nil))
      (should (equal (vm-virtual-auto-select-folder (car vm-message-list))
                     archive))
      (vm-virtual-save-message archive 1)
      (should (file-exists-p archive))
      (with-temp-buffer
        (insert-file-contents archive)
        (should (string-match-p "badgers" (buffer-string))))
      (should (vm-filed-flag (car vm-message-list))))))

(ert-deftest vm-avirtual-test-checking-a-selector-reports-what-it-found ()
  "`vm-virtual-check-selector-interactive' says whether a virtual folder's
selectors match the message you are looking at, naming the message and the
answer.  It is how a rule that is not doing what its author meant gets
looked at."
  (vm-avirtual-test--with-folder (file archive)
    (let ((vm-virtual-folder-alist
           (list (list "badger-mail" (list (list file) '(subject "badgers")))))
          (before (buffer-list)))
      (unwind-protect
          (progn
            (vm-virtual-check-selector-interactive "badger-mail")
            (with-current-buffer "*VM virtual-folder-check*"
              (let ((said (buffer-string)))
                (should (string-match-p "badger-mail" said))
                (should (string-match-p "is true" said))))
            ;; and the message that does not match says so
            (setq vm-message-pointer (cdr vm-message-list))
            (vm-virtual-check-selector-interactive "badger-mail")
            (with-current-buffer "*VM virtual-folder-check*"
              (should (string-match-p "is false" (buffer-string)))))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest vm-avirtual-test-adding-a-selector-names-its-function ()
  "`vm-avirtual-add-selectors' is how this file registers the selectors it
defines: each name is paired with the `vm-vs-' function that answers it, and
listed as one the interactive selector reader offers."
  (let ((vm-virtual-selector-function-alist
         (copy-sequence vm-virtual-selector-function-alist))
        (vm-supported-interactive-virtual-selectors
         (copy-sequence vm-supported-interactive-virtual-selectors)))
    (vm-avirtual-add-selectors '(badgerish))
    (should (equal (cdr (assq 'badgerish vm-virtual-selector-function-alist))
                   'vm-vs-badgerish))
    (should (member '("badgerish") vm-supported-interactive-virtual-selectors))
    ;; and adding it twice leaves one of it
    (vm-avirtual-add-selectors '(badgerish))
    (should (equal (length (seq-filter
                            (lambda (entry) (eq (car entry) 'badgerish))
                            vm-virtual-selector-function-alist))
                   1))))

(ert-deftest vm-avirtual-test-finding-a-selector-in-a-specification ()
  "`vm-virtual-find-selector' digs a selector of a given kind out of a
folder definition, however deep it is: the definitions nest, and a caller
that wants the `label' of a folder should not have to know its structure."
  (let ((spec '((and (subject "badgers")
                     (or (label "urgent") (author "alice"))))))
    (should (equal (vm-virtual-find-selector spec 'label) '(label "urgent")))
    (should (equal (vm-virtual-find-selector spec 'author) '(author "alice")))
    (should-not (vm-virtual-find-selector spec 'recipient))))

(provide 'vm-avirtual-test)

;;; vm-avirtual-test.el ends here