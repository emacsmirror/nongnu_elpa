;;; vm-thread-test.el --- Tests for vm-thread.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM thread functions in vm-thread.el

;;; Code:

(require 'vm-test-init)
(require 'vm-thread)

;;; Thread symbol accessor tests

(ert-deftest vm-thread-test-youngest-date-accessors ()
  "Test vm-th-youngest-date-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-youngest-date-of sym "2024-01-01")
    (should (equal (vm-th-youngest-date-of sym) "2024-01-01"))))

(ert-deftest vm-thread-test-oldest-date-accessors ()
  "Test vm-th-oldest-date-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-oldest-date-of sym "2024-01-01")
    (should (equal (vm-th-oldest-date-of sym) "2024-01-01"))))

(ert-deftest vm-thread-test-oldest-subject-accessors ()
  "Test vm-th-oldest-subject-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-oldest-subject-of sym "Test Subject")
    (should (equal (vm-th-oldest-subject-of sym) "Test Subject"))))

(ert-deftest vm-thread-test-message-accessors ()
  "Test vm-th-message-of and setter."
  (let ((sym (make-symbol "test"))
        (msg 'test-message))
    (vm-th-set-message-of sym msg)
    (should (eq (vm-th-message-of sym) msg))))

(ert-deftest vm-thread-test-messages-accessors ()
  "Test vm-th-messages-of and setter."
  (let ((sym (make-symbol "test"))
        (msgs '(msg1 msg2 msg3)))
    (vm-th-set-messages-of sym msgs)
    (should (equal (vm-th-messages-of sym) msgs))))

(ert-deftest vm-thread-test-parent-accessors ()
  "Test vm-th-parent-of and setter."
  (let ((sym (make-symbol "test"))
        (parent (make-symbol "parent")))
    (vm-th-set-parent-of sym parent)
    (should (eq (vm-th-parent-of sym) parent))))

(ert-deftest vm-thread-test-children-accessors ()
  "Test vm-th-children-of and setter."
  (let ((sym (make-symbol "test"))
        (children '(child1 child2)))
    (vm-th-set-children-of sym children)
    (should (equal (vm-th-children-of sym) children))))

(ert-deftest vm-thread-test-date-accessors ()
  "Test vm-th-date-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-date-of sym "2024-06-15")
    (should (equal (vm-th-date-of sym) "2024-06-15"))))

;;; vm-th-add-child / vm-th-delete-child tests

(ert-deftest vm-thread-test-add-child ()
  "Test adding a child to a thread."
  (let ((parent (make-symbol "parent"))
        (child (make-symbol "child")))
    (vm-th-set-children-of parent nil)
    (vm-th-add-child parent child)
    (should (memq child (vm-th-children-of parent)))))

(ert-deftest vm-thread-test-delete-child ()
  "Test deleting a child from a thread."
  (let ((parent (make-symbol "parent"))
        (child1 (make-symbol "child1"))
        (child2 (make-symbol "child2")))
    (vm-th-set-children-of parent (list child1 child2))
    (vm-th-delete-child parent child1)
    (should-not (memq child1 (vm-th-children-of parent)))
    (should (memq child2 (vm-th-children-of parent)))))

;;; Thread subject symbol accessor tests
;; Note: ts-* functions require the symbol to have a vector value

(ert-deftest vm-thread-test-ts-root-accessors ()
  "Test vm-ts-root-of and setter."
  (let ((sym (make-symbol "subj"))
        (root (make-symbol "root")))
    ;; Initialize with a 4-element vector
    (set sym (make-vector 4 nil))
    (vm-ts-set-root-of sym root)
    (should (eq (vm-ts-root-of sym) root))))

(ert-deftest vm-thread-test-ts-root-date-accessors ()
  "Test vm-ts-root-date-of and setter."
  (let ((sym (make-symbol "subj")))
    (set sym (make-vector 4 nil))
    (vm-ts-set-root-date-of sym "2024-01-01")
    (should (equal (vm-ts-root-date-of sym) "2024-01-01"))))

(ert-deftest vm-thread-test-ts-members-accessors ()
  "Test vm-ts-members-of and setter."
  (let ((sym (make-symbol "subj"))
        (members '(m1 m2 m3)))
    (set sym (make-vector 4 nil))
    (vm-ts-set-members-of sym members)
    (should (equal (vm-ts-members-of sym) members))))

(ert-deftest vm-thread-test-ts-messages-accessors ()
  "Test vm-ts-messages-of and setter."
  (let ((sym (make-symbol "subj"))
        (messages '(msg1 msg2)))
    (set sym (make-vector 4 nil))
    (vm-ts-set-messages-of sym messages)
    (should (equal (vm-ts-messages-of sym) messages))))

;;; vm-th-thread-date-of tests
;; Note: uses symbol properties 'youngest-date and 'oldest-date

(ert-deftest vm-thread-test-thread-date-youngest ()
  "Test vm-th-thread-date-of with youngest-date criterion."
  (let ((sym (make-symbol "test")))
    (vm-th-set-youngest-date-of sym "2024-12-31")
    (vm-th-set-oldest-date-of sym "2024-01-01")
    (should (equal (vm-th-thread-date-of sym 'youngest-date) "2024-12-31"))))

(ert-deftest vm-thread-test-thread-date-oldest ()
  "Test vm-th-thread-date-of with oldest-date criterion."
  (let ((sym (make-symbol "test")))
    (vm-th-set-youngest-date-of sym "2024-12-31")
    (vm-th-set-oldest-date-of sym "2024-01-01")
    (should (equal (vm-th-thread-date-of sym 'oldest-date) "2024-01-01"))))

;;; vm-th-child-messages-of tests

(ert-deftest vm-thread-test-child-messages-of-empty ()
  "Test vm-th-child-messages-of with no children."
  (let ((sym (make-symbol "test")))
    (vm-th-set-children-of sym nil)
    (should (null (vm-th-child-messages-of sym)))))

;;; vm-th-safe-parent-p tests

(ert-deftest vm-thread-test-safe-parent-p-same-symbol ()
  "Test vm-th-safe-parent-p rejects same symbol as parent."
  (let ((sym (make-symbol "test")))
    (should-not (vm-th-safe-parent-p sym sym))))

(ert-deftest vm-thread-test-safe-parent-p-valid ()
  "Test vm-th-safe-parent-p accepts valid parent."
  (let ((child (make-symbol "child"))
        (parent (make-symbol "parent")))
    (vm-th-set-parent-of parent nil)  ; parent has no parent
    (should (vm-th-safe-parent-p child parent))))

;;; vm-th-root tests
;; Note: vm-th-root traverses parent pointers and requires
;; symbols to have proper values set. These are complex tests
;; that need full threading infrastructure.

;;; vm-ts-add-members tests

(ert-deftest vm-thread-test-ts-add-members ()
  "Test vm-ts-add-members adds to existing members."
  (let ((sym (make-symbol "subj")))
    (set sym (make-vector 4 nil))
    (vm-ts-set-members-of sym '(m1))
    (vm-ts-add-members sym '(m2 m3))
    (let ((members (vm-ts-members-of sym)))
      (should (memq 'm1 members))
      (should (memq 'm2 members))
      (should (memq 'm3 members)))))

;;; vm-ts-add-messages tests

(ert-deftest vm-thread-test-ts-add-messages ()
  "Test vm-ts-add-messages adds to existing messages."
  (let ((sym (make-symbol "subj")))
    (set sym (make-vector 4 nil))
    (vm-ts-set-messages-of sym '(msg1))
    (vm-ts-add-messages sym '(msg2))
    (let ((messages (vm-ts-messages-of sym)))
      (should (memq 'msg1 messages))
      (should (memq 'msg2 messages)))))

;;; vm-ts-merge tests

(ert-deftest vm-thread-test-ts-merge ()
  "Test vm-ts-merge merges two subject symbols."
  (let ((sym1 (make-symbol "subj1"))
        (sym2 (make-symbol "subj2")))
    ;; Initialize vectors
    (set sym1 (make-vector 4 nil))
    (set sym2 (make-vector 4 nil))
    ;; Set up data
    (vm-ts-set-members-of sym1 '(m1))
    (vm-ts-set-messages-of sym1 '(msg1))
    (vm-ts-set-members-of sym2 '(m2))
    (vm-ts-set-messages-of sym2 '(msg2))
    ;; Merge sym2 into sym1
    (vm-ts-merge sym1 sym2)
    ;; sym1 should have all members and messages
    (let ((members (vm-ts-members-of sym1))
          (messages (vm-ts-messages-of sym1)))
      (should (memq 'm1 members))
      (should (memq 'm2 members))
      (should (memq 'msg1 messages))
      (should (memq 'msg2 messages)))))

;;; vm-ts-subject-symbol tests
;; Note: vm-ts-subject-symbol uses 'oldest-subject property,
;; not 'subject. The function checks for proper thread setup.

;;; Integration tests with folders

(defconst vm-thread-test-folder
  "From sender1@example.com Mon Jan  1 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: Thread Test
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <thread1@example.com>

First message in thread.

From sender2@example.com Tue Jan  2 00:00:00 2024
From: sender2@example.com
To: recipient@example.com
Subject: Re: Thread Test
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <thread2@example.com>
In-Reply-To: <thread1@example.com>
References: <thread1@example.com>

Reply to first message.

"
  "Test folder with threading references.")

(ert-deftest vm-thread-test-folder-parse-references ()
  "Test that References header is parsed correctly."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg2 (nth 1 vm-message-list)))
      (let ((refs (vm-get-header-contents msg2 "References:")))
        (should (stringp refs))
        (should (string-match "thread1@example.com" refs))))))

(ert-deftest vm-thread-test-folder-parse-in-reply-to ()
  "Test that In-Reply-To header is parsed correctly."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg2 (nth 1 vm-message-list)))
      (let ((irt (vm-get-header-contents msg2 "In-Reply-To:")))
        (should (stringp irt))
        (should (string-match "thread1@example.com" irt))))))

;;; vm-thread-indentation tests

(ert-deftest vm-thread-test-indentation-unset ()
  "Test vm-thread-indentation returns empty when not threaded."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg (car vm-message-list)))
      ;; Before threading, indentation should be empty or 0
      (let ((indent (vm-thread-indentation msg)))
        (should (or (null indent) (= indent 0)))))))

;;; vm-thread-list tests

(ert-deftest vm-thread-test-thread-list-returns-list ()
  "Test vm-thread-list returns a list."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg (car vm-message-list)))
      ;; vm-thread-list auto-generates the list if not set
      (should (listp (vm-thread-list msg))))))

;;; vm-thread-root-p tests

(ert-deftest vm-thread-test-root-p-unthreaded ()
  "Test vm-thread-root-p for unthreaded message."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg (car vm-message-list)))
      ;; For unthreaded messages, should return nil
      (should (null (vm-thread-root-p msg))))))

;;; Thread building integration tests

(defconst vm-thread-test-threaded-folder
  "From root@example.com Mon Jan  1 00:00:00 2024
From: Root <root@example.com>
To: list@example.com
Subject: Thread Root
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <root@example.com>

Root message.

From reply1@example.com Tue Jan  2 00:00:00 2024
From: Reply1 <reply1@example.com>
To: list@example.com
Subject: Re: Thread Root
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <reply1@example.com>
In-Reply-To: <root@example.com>
References: <root@example.com>

First reply.

From reply2@example.com Wed Jan  3 00:00:00 2024
From: Reply2 <reply2@example.com>
To: list@example.com
Subject: Re: Thread Root
Date: Wed, 03 Jan 2024 12:00:00 +0000
Message-ID: <reply2@example.com>
In-Reply-To: <reply1@example.com>
References: <root@example.com> <reply1@example.com>

Second reply (to first reply).

From standalone@example.com Thu Jan  4 00:00:00 2024
From: Standalone <standalone@example.com>
To: list@example.com
Subject: Different Thread
Date: Thu, 04 Jan 2024 13:00:00 +0000
Message-ID: <standalone@example.com>

Standalone message, not part of thread.

"
  "Test folder with a proper thread structure.")

(ert-deftest vm-thread-test-build-threads ()
  "Test vm-build-threads initializes threading structures."
  (vm-test-with-folder vm-thread-test-threaded-folder
    ;; Build the threads
    (vm-build-threads nil)
    ;; Thread obarray should be created
    (should (vectorp vm-thread-obarray))))

(ert-deftest vm-thread-test-thread-symbol-created ()
  "Test that thread symbols are created for messages."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    ;; Each message should have a thread symbol
    (dolist (msg vm-message-list)
      (let ((id (vm-su-message-id msg)))
        (when id
          (should (intern-soft id vm-thread-obarray)))))))

(ert-deftest vm-thread-test-thread-list-generated ()
  "Test vm-thread-list returns list of ancestor message IDs."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    ;; Root should have itself in thread list
    (let* ((root-msg (car vm-message-list))
           (tl (vm-thread-list root-msg)))
      (should (listp tl))
      (should (> (length tl) 0)))))

(ert-deftest vm-thread-test-reply-has-parent-ref ()
  "Test that replies reference their parent."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let ((reply-msg (nth 1 vm-message-list)))
      ;; Second message (reply1) should have references to root
      (let ((refs (vm-get-header-contents reply-msg "References:")))
        (should (string-match "root@example.com" refs))))))

(ert-deftest vm-thread-test-deep-reply-refs ()
  "Test that deep replies have full reference chain."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let ((reply2-msg (nth 2 vm-message-list)))
      ;; Third message (reply2) should reference both root and reply1
      (let ((refs (vm-get-header-contents reply2-msg "References:")))
        (should (string-match "root@example.com" refs))
        (should (string-match "reply1@example.com" refs))))))

;;; Thread sort key tests

(ert-deftest vm-thread-test-sort-by-activity ()
  "Test thread activity sort key exists."
  (should (member "activity" vm-supported-sort-keys))
  (should (member "reversed-activity" vm-supported-sort-keys)))

;;; Thread indentation tests

(ert-deftest vm-thread-test-root-no-indent ()
  "Test root message has zero or no indentation."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let* ((root-msg (car vm-message-list))
           (indent (vm-thread-indentation root-msg)))
      ;; Root should have 0 or nil indentation
      (should (or (null indent) (= indent 0))))))

;;; vm-thread-symbol accessor coverage tests

(ert-deftest vm-thread-test-th-thread-symbol-basic ()
  "Test vm-th-thread-symbol creates symbol from message ID."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let ((msg (car vm-message-list)))
      (let ((sym (vm-th-thread-symbol msg)))
        (should (symbolp sym))))))

(ert-deftest vm-thread-test-canonical-message ()
  "Test vm-th-canonical-message returns the message for a thread symbol."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let ((msg (car vm-message-list)))
      ;; vm-th-canonical-message takes a message, not a symbol
      ;; It returns the canonical message for that message's thread symbol
      (let ((canonical (vm-th-canonical-message msg)))
        (when canonical
          (should (vectorp canonical)))))))

;;; Multiple thread tests

(ert-deftest vm-thread-test-separate-threads ()
  "Test messages without references are separate threads."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let ((root-msg (car vm-message-list))
          (standalone-msg (nth 3 vm-message-list)))
      ;; Root and standalone should have different thread symbols
      (let ((root-id (vm-su-message-id root-msg))
            (standalone-id (vm-su-message-id standalone-msg)))
        (should-not (equal root-id standalone-id))))))

;;; Thread collapse tests (existence)

(ert-deftest vm-thread-test-collapse-functions-exist ()
  "Test thread collapse functions exist."
  (should (fboundp 'vm-toggle-thread))
  (should (fboundp 'vm-expand-all-threads))
  (should (fboundp 'vm-collapse-all-threads)))

;;; Subject-based threading tests

(ert-deftest vm-thread-test-subject-symbol-accessor ()
  "Test vm-ts-subject-symbol accessor."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let* ((msg (car vm-message-list))
           (sym (vm-th-thread-symbol msg)))
      (when sym
        ;; vm-ts-subject-symbol derives subject symbol from thread symbol
        (let ((subj-sym (vm-ts-subject-symbol sym)))
          (when subj-sym
            (should (symbolp subj-sym))))))))

;;; Thread member collection tests

(ert-deftest vm-thread-test-visible-children-of ()
  "Test vm-th-visible-children-of filters properly."
  (let ((parent (make-symbol "parent"))
        (child-with-msg (make-symbol "child1"))
        (child-without-msg (make-symbol "child2")))
    ;; Set up parent with two children
    (vm-th-set-children-of parent (list child-with-msg child-without-msg))
    ;; Only first child has a message
    (vm-th-set-message-of child-with-msg 'test-msg)
    (vm-th-set-message-of child-without-msg nil)
    ;; visible-children should only return child with message
    (let ((visible (vm-th-visible-children-of parent)))
      (should (= (length visible) 1))
      (should (memq child-with-msg visible)))))

(ert-deftest vm-thread-test-child-messages-of ()
  "Test vm-th-child-messages-of collects messages from children."
  (let ((parent (make-symbol "parent"))
        (child1 (make-symbol "child1"))
        (child2 (make-symbol "child2")))
    ;; Set up children with messages
    (vm-th-set-message-of child1 'msg1)
    (vm-th-set-message-of child2 'msg2)
    (vm-th-set-children-of parent (list child1 child2))
    ;; child-messages should return both messages
    (let ((msgs (vm-th-child-messages-of parent)))
      (should (= (length msgs) 2))
      (should (memq 'msg1 msgs))
      (should (memq 'msg2 msgs)))))

;;; vm-th-root tests
;; Note: vm-th-root returns the message (not symbol) at the root
;; and requires vm-th-messages-of to be set on parent symbols.

(ert-deftest vm-thread-test-root-returns-message ()
  "Test vm-th-root returns the message of root symbol."
  (let ((root-sym (make-symbol "root"))
        (test-msg 'test-message))
    ;; Set up a root symbol with no parent and a message
    (vm-th-set-parent-of root-sym nil)
    (vm-th-set-message-of root-sym test-msg)
    (vm-th-set-messages-of root-sym (list test-msg))
    ;; vm-th-root should return the message
    (should (eq (vm-th-root root-sym) test-msg))))

(ert-deftest vm-thread-test-root-traverses-to-message ()
  "Test vm-th-root traverses parent chain to find root message."
  (let ((root (make-symbol "root"))
        (child (make-symbol "child"))
        (root-msg 'root-message)
        (child-msg 'child-message))
    ;; Set up a parent-child relationship with messages
    (vm-th-set-parent-of root nil)
    (vm-th-set-parent-of child root)
    (vm-th-set-message-of root root-msg)
    (vm-th-set-messages-of root (list root-msg))
    (vm-th-set-message-of child child-msg)
    ;; vm-th-root of child should return root's message
    (should (eq (vm-th-root child) root-msg))))

;;; nil-message robustness

(ert-deftest vm-thread-test-build-thread-list-nil-message ()
  "Test that `vm-build-thread-list' tolerates a nil message.
Issue #463 reported \"Wrong type argument: arrayp, nil\" from
`vm-so-sortable-subject' under `vm-build-thread-list' when an IMAP
retrieval failed partway.  The null guard that fixes it landed in
2011; this pins it so the crash cannot come back."
  (let ((vm-thread-debug nil))
    (should (null (vm-build-thread-list nil)))))


;;; the cost of building threads (issue #557)

(defun vm-thread-test--write-chain (file n)
  "Write a folder of N messages to FILE, each referencing the one before it."
  (with-temp-file file
    (dotimes (i n)
      (insert (format "From s%d@example.com Mon Jan  1 00:00:00 2024\n" i)
              (format "From: S%d <s%d@example.com>\n" i i)
              (format "Subject: msg %d\n" i)
              (format "Message-ID: <chain-%d@example.com>\n" i)
              (if (> i 0)
                  (format "References: <chain-%d@example.com>\n" (1- i))
                "")
              "\n"
              (format "Body %d.\n\n" i)))))

(ert-deftest vm-thread-test-reference-root-memoises-the-whole-path ()
  "REGRESSION: resolving a reference root remembers every id on the way up.
Issue #557.  `vm-thread-subtree' needs the subject symbol of every message in a
subtree, and finding one means climbing to the thread's root.  Climbed afresh
each time, that costs the depth of the thread per message, and with a subtree
computed for every message it made thread building cubic in the folder: 2000
messages in one reference chain took a minute, and 25 000 would have taken
hours.

Asserted as the mechanism rather than as a time, because a time is a property of
the machine.  What has to hold is that one climb populates the cache for the
whole path it walked, so the remaining messages of that thread cost nothing.  The
measurements are on the issue.

Also checks the cache cannot change the answer: root with and without it agree
for every message."
  (let* ((n 200)
         (dir (file-name-as-directory (make-temp-file "vm-thread-cost" t)))
         (file (expand-file-name "chain" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
    (require 'vm)
    (unwind-protect
        (progn
          (vm-thread-test--write-chain file n)
          (vm-visit-folder file)
          (should (= n (length vm-message-list)))
          (vm-build-threads vm-message-list)
          (let* ((deepest (vm-thread-symbol (car (last vm-message-list))))
                 (root (vm-thread-symbol (car vm-message-list)))
                 (cache (make-hash-table :test 'eq)))
            ;; One climb from the deepest message ...
            (should (eq root (vm-th-reference-root-sym deepest cache)))
            ;; ... leaves every id it passed known, which is the whole chain.
            (should (>= (hash-table-count cache) n))
            ;; The answer does not depend on the cache.
            (dolist (m vm-message-list)
              (let ((id-sym (vm-thread-symbol m)))
                (should (eq (vm-th-reference-root-sym id-sym)
                            (vm-th-reference-root-sym id-sym cache)))
                (should (eq root (vm-th-reference-root-sym id-sym cache)))))
            ;; And the thread itself is right: the first message's subtree is
            ;; the whole folder, and the last message is below it.
            (should (= n (length (vm-thread-subtree (car vm-message-list)))))
            (should (< 0 (vm-thread-indentation (car (last vm-message-list)))))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))


;;; asking rather than provoking an error (issue #476)

(ert-deftest vm-thread-root-p-does-not-provoke-an-error-when-unbuilt ()
  "REGRESSION: `vm-thread-root-p' answers without raising an error.
Issue #476: with threads not built it used to call through to
`vm-thread-subtree', which signals `vm-thread-error', and catch that to answer
nil.  `vm-summary-faces-add' asks this for every summary line, so an ordinary
folder with threading off raised and recovered an error once per line --
expensive, and it leaves `debug-on-signal' useless for anyone debugging
something else while reading mail.

`vm-thread-subtree' is stubbed to signal if it is reached at all, so the test
fails whichever way the old code went: by returning non-nil, or by getting
there."
  (vm-test-with-folder vm-thread-test-threaded-folder
    ;; deliberately not built
    (should-not (vectorp vm-thread-obarray))
    (let ((reached nil))
      (cl-letf (((symbol-function 'vm-thread-subtree)
                 (lambda (&rest _)
                   (setq reached t)
                   (signal 'vm-thread-error '(vm-thread-subtree)))))
        (should-not (vm-thread-root-p (car vm-message-list))))
      (should-not reached))))

(ert-deftest vm-thread-root-p-still-recognises-a-root ()
  "The control: with threads built, a root is still a root and a reply is not.
Without this, the test above could pass by answering nil for everything."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (should (vectorp vm-thread-obarray))
    (let* ((root (car vm-message-list))
           (reply (cadr vm-message-list)))
      ;; the fixture is a thread, so its first message is a root with children
      (should (> (vm-thread-count root) 1))
      (should (vm-thread-root-p root))
      (should-not (vm-thread-root-p reply)))))

(ert-deftest vm-thread-root-p-tolerates-a-threading-error ()
  "A genuine threading error still leaves the answer nil rather than failing.
The documented contract is that no exception escapes, and `vm-summary-faces-add'
relies on it: a summary line is not worth failing to draw over a threading
problem.  Only the unbuilt case stopped going through the error path."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (cl-letf (((symbol-function 'vm-thread-root)
               (lambda (&rest _) (signal 'vm-thread-error '(deliberate)))))
      (should-not (vm-thread-root-p (car vm-message-list))))))


;;; vm-thread-subtree-safe outside a folder buffer (issue #563)

(ert-deftest vm-thread-subtree-safe-uses-the-whole-subtree-when-built ()
  "REGRESSION: outside a vm-mode buffer, a built thread yields the whole subtree.
Issue #563: the check was `(vectorp \\='vm-thread-obarray)\\=' -- the quoted symbol,
never a vector -- where the other nine sites test the value.  So this branch could
only ever answer \"threads have not been built\" and return the singleton, whatever
the state of the folder.

The callers are the `thread-any\\=' and `thread-all\\=' virtual folder selectors.  Taking
the singleton would mean `thread-any\\=' matching only when the thread root matches, and
`thread-all\\=' being trivially true whenever it does."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let* ((folder (current-buffer))
           (root (car vm-message-list))
           (whole (length (vm-thread-subtree root))))
      ;; the fixture has to be a real thread or this proves nothing
      (should (> whole 1))
      ;; from the folder buffer, the first branch
      (let ((major-mode 'vm-mode))
        (should (= whole (length (vm-thread-subtree-safe root)))))
      ;; and from anywhere else, which is the branch that was dead
      (let ((major-mode 'vm-virtual-mode)
            (vm-mail-buffer folder))
        (should (= whole (length (vm-thread-subtree-safe root))))))))

(ert-deftest vm-thread-subtree-safe-falls-back-when-unbuilt ()
  "With threads unbuilt it still answers with just the message, as documented.
The control: the fix must not turn the fallback into a call that signals."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (should-not (vectorp vm-thread-obarray))
    (let ((folder (current-buffer))
          (msg (car vm-message-list)))
      (let ((major-mode 'vm-virtual-mode)
            (vm-mail-buffer folder))
        (should (equal (list msg) (vm-thread-subtree-safe msg)))))))

;;; Rearranging threads by hand (issue #574)

;; `vm-promote-subthread', `vm-demote-subthread' and `vm-attach-to-thread' let
;; the user override what the References headers say.  None of them had a test.
;; `vm-attach-to-thread' refuses an attach that would make a cycle, and used to
;; refuse it after taking the message out of its thread, leaving the folder
;; threaded differently and nothing to put it back.

(defconst vm-thread-test--fork-folder
  (mapconcat
   (lambda (spec)
     (let ((i (car spec)) (parent (cdr spec)))
       (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
               "From: alice@example.com\n"
               (format "Subject: subject %d\n" i)
               (format "Message-ID: <fork-%d@example.com>\n" i)
               (if parent
                   (format "References: <fork-%d@example.com>\n" parent)
                 "")
               "\n" (format "Body %d.\n\n" i))))
   '((0 . nil) (1 . 0) (2 . 1) (3 . 0) (4 . nil))
   "")
  "Messages structured 0 < 1 < 2 and 0 < 3, with 4 outside any thread.
The fork matters: the damage a refused attach did showed on the second child of
the root, not on the first.")

(defmacro vm-thread-test--with-fork (&rest body)
  "Run BODY in the fork folder with threads built and message 0 current."
  (declare (indent 0) (debug t))
  `(vm-test-with-folder vm-thread-test--fork-folder
     (setq major-mode 'vm-mode)
     (setq vm-mail-buffer nil)
     (let ((vm-summary-show-threads t))
       (cl-letf (((symbol-function 'vm-follow-summary-cursor) #'ignore)
                 ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
                 ((symbol-function 'vm-thread-mark-for-summary-update) #'ignore)
                 ((symbol-function 'vm-inform) #'ignore))
         (vm-build-threads-if-unbuilt)
         ,@body))))

(defun vm-thread-test--edit-message (n replace with)
  "Edit message N of the current folder, replacing REPLACE with WITH.
Drives `vm-edit-message-end', which is what discards the cached data and
re-threads the message -- the path an edit of a Subject takes."
  (let* ((m (nth n vm-message-list))
         (edit-buf (generate-new-buffer " *vm-thread-test-edit*")))
    (unwind-protect
        (progn
          (vm-set-edit-buffer-of m edit-buf)
          (with-current-buffer edit-buf
            (insert-buffer-substring (vm-buffer-of m)
                                     (vm-headers-of m) (vm-text-end-of m))
            (goto-char (point-min))
            (should (search-forward replace nil t))
            (replace-match with)
            (setq vm-message-pointer (list m)
                  vm-mail-buffer (vm-buffer-of m))
            (set-buffer-modified-p t))
          (cl-letf (((symbol-function 'vm-present-current-message) #'ignore)
                    ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
                    ((symbol-function 'vm-display) (lambda (&rest _) nil)))
            (with-current-buffer edit-buf (vm-edit-message-end))))
      (when (buffer-live-p edit-buf) (kill-buffer edit-buf)))))

(ert-deftest vm-thread-test-an-edited-subject-does-not-thread-a-root-under-its-child ()
  "REGRESSION: editing a Subject leaves the message where it was in its thread.
Issue #307, the half that was never reproduced.  `vm-unthread-message' is
called before the cached data is wiped, so that the old message id still
finds the right node, and it recorded the message's *old* subject on that
node.  Nothing replaced it afterwards: `vm-build-thread-list' fills the
field in only when it meets an older date.  `vm-ts-subject-symbol' reads
exactly that field, so an edited message went on being sorted under the
subject it used to have -- and joined that subject thread under whichever
message had taken over as its root.

For the root of a thread that is its own child: message 0 came out
indented under message 3, which references message 0.  Every message in
the thread shifted a step to the right with it, and
`vm-check-thread-integrity' saw nothing wrong, the database being
self-consistent."
  (vm-thread-test--with-fork
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))
    (vm-thread-test--edit-message 0 "Subject: subject 0" "Subject: quite another")
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))
    ;; the message is in the subject thread it now names, as its root
    (let* ((m (car vm-message-list))
           (s-sym (vm-ts-subject-symbol (vm-th-thread-symbol m))))
      (should (equal (symbol-name s-sym) "quite another"))
      (should (eq (vm-ts-root-of s-sym) (vm-th-thread-symbol m))))))

(ert-deftest vm-thread-test-an-edited-subject-is-what-the-thread-node-holds ()
  "The node's oldest-subject follows the edit rather than keeping the old one.
That field is what `vm-ts-subject-symbol' is computed from, so a stale one
is not cosmetic."
  (vm-thread-test--with-fork
    (let ((m (car vm-message-list)))
      (should (equal (vm-th-oldest-subject-of (vm-th-thread-symbol m))
                     "subject 0"))
      (vm-thread-test--edit-message 0 "Subject: subject 0" "Subject: quite another")
      (should (equal (vm-th-oldest-subject-of (vm-th-thread-symbol m))
                     "quite another")))))

(defun vm-thread-test--indentations ()
  "Return the thread indentation of each message in `vm-message-list'."
  (mapcar #'vm-thread-indentation vm-message-list))

(ert-deftest vm-thread-test-attach-to-thread-makes-a-child ()
  "Attaching puts the current message under the one visited last.
Message 4 is in no thread; under message 2 it is three deep."
  (vm-thread-test--with-fork
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))
    (setq vm-last-message-pointer (nthcdr 2 vm-message-list))
    (setq vm-message-pointer (nthcdr 4 vm-message-list))
    (vm-attach-to-thread)
    (should (equal '(0 1 2 1 3) (vm-thread-test--indentations)))))

(ert-deftest vm-thread-test-attach-to-thread-refuses-a-cycle-and-changes-nothing ()
  "REGRESSION: a refused attach leaves the threading as it was.
Issue #574.  Attaching the root under its own descendant is a cycle.  The check
used to run after `vm-unthread-message', so the refusal cost the root its
children: message 3 was left at indentation 0."
  (vm-thread-test--with-fork
    (setq vm-last-message-pointer (nthcdr 2 vm-message-list))
    (setq vm-message-pointer vm-message-list)
    (should-error (vm-attach-to-thread) :type 'error)
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))))

(ert-deftest vm-thread-test-attach-to-thread-needs-a-message-visited-first ()
  "With no last message there is nothing to attach to, and nothing is changed."
  (vm-thread-test--with-fork
    (setq vm-last-message-pointer nil)
    (setq vm-message-pointer (nthcdr 4 vm-message-list))
    (should-error (vm-attach-to-thread) :type 'error)
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))))

(ert-deftest vm-thread-test-demote-and-promote-move-the-whole-subtree ()
  "Demoting indents the message and everything under it, promoting undoes it.
Message 1 carries message 2 with it, and the messages outside the subtree do not
move."
  (vm-thread-test--with-fork
    (setq vm-message-pointer (nthcdr 1 vm-message-list))
    (vm-demote-subthread 2)
    (should (equal '(0 3 4 1 0) (vm-thread-test--indentations)))
    (vm-promote-subthread 1)
    (should (equal '(0 2 3 1 0) (vm-thread-test--indentations)))))

(ert-deftest vm-thread-test-demote-zero-resets-the-offset ()
  "A count of zero to demote puts the subtree back to its natural indentation."
  (vm-thread-test--with-fork
    (setq vm-message-pointer (nthcdr 1 vm-message-list))
    (vm-demote-subthread 3)
    (should (equal '(0 4 5 1 0) (vm-thread-test--indentations)))
    (vm-demote-subthread 0)
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))))

(ert-deftest vm-thread-test-promote-zero-goes-all-the-way-to-the-left ()
  "A count of zero to promote takes the message to indentation 0.
Its subtree moves by the same amount rather than to 0 as well, so a message
below it can end up left of where its parent now is.

The indentations are read once first, and that is not decoration: this case
takes its step from `vm-thread-indentation-of', the cached slot, and does
nothing at all while that is still empty.  Drawing a threaded summary is what
fills it in a real session."
  (vm-thread-test--with-fork
    (should (equal '(0 1 2 1 0) (vm-thread-test--indentations)))
    (setq vm-message-pointer (nthcdr 2 vm-message-list))
    (vm-promote-subthread 0)
    (should (equal '(0 1 0 1 0) (vm-thread-test--indentations)))))

;;; Toggling the threads display (emacs-vm/vm#632)

(defconst vm-thread-test--display-folder
  (concat
   ;; the root comes first in the file, its reply last, and an unrelated
   ;; message sits between them -- so threading has something to move
   "From alice@example.com Sat Aug  8 10:00:00 2026\n"
   "From: alice@example.com\nSubject: badgers\n"
   "Message-ID: <root@example.com>\n\nThe root.\n\n"
   "From carol@example.com Sat Aug  8 11:00:00 2026\n"
   "From: carol@example.com\nSubject: the roof\n"
   "Message-ID: <alone@example.com>\n\nNothing to do with badgers.\n\n"
   "From bob@example.com Sat Aug  8 12:00:00 2026\n"
   "From: bob@example.com\nSubject: Re: badgers\n"
   "Message-ID: <reply@example.com>\n"
   "References: <root@example.com>\n\nThe reply.\n\n")
  "A thread of two with an unrelated message between them in the file.")

(defmacro vm-thread-test--with-display-folder (&rest body)
  "Visit `vm-thread-test--display-folder' as a real folder and run BODY.
`vm-toggle-threads-display' sorts the folder, so this wants a folder VM has
really visited rather than a buffer holding the text."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-thread-display" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-summary-show-threads nil))
           (write-region vm-thread-test--display-folder nil folder nil 'quiet)
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

(ert-deftest vm-thread-test-toggle-threads-display ()
  "`vm-toggle-threads-display' turns the threads display on and off and sorts
the folder to match: by thread activity with it on, back to the order in the
file with it off.

With threads on the reply follows its root, which in the file it does not --
another message is between them.  Toggling back restores the file order
exactly, so nothing is left rearranged."
  (vm-thread-test--with-display-folder
    (let ((physical (mapcar #'vm-su-subject vm-message-list)))
      (should (equal physical '("badgers" "the roof" "Re: badgers")))
      (should-not vm-summary-show-threads)
      (vm-toggle-threads-display)
      (should vm-summary-show-threads)
      (let* ((threaded (mapcar #'vm-su-subject vm-message-list))
             (root (cl-position "badgers" threaded :test #'equal))
             (reply (cl-position "Re: badgers" threaded :test #'equal)))
        (should (equal reply (1+ root))))
      (vm-toggle-threads-display)
      (should-not vm-summary-show-threads)
      (should (equal (mapcar #'vm-su-subject vm-message-list) physical)))))

(ert-deftest vm-thread-test-toggling-threads-swaps-the-sort-keys ()
  "The toggle swaps the sort keys between physical order and activity, and
between their reversed forms.

The keys have to be set for that to be visible: a folder that has not been
sorted has none, the swap has nothing to match, and the keys are then whatever
the sort the toggle performs leaves behind -- which looks like the same answer
and tests nothing."
  (vm-thread-test--with-display-folder
    (setq vm-ml-sort-keys "physical-order")
    (vm-toggle-threads-display)
    (should (equal vm-ml-sort-keys "activity"))
    (vm-toggle-threads-display)
    (should (equal vm-ml-sort-keys "physical-order"))
    ;; and the reversed pair swap with each other, not with the plain ones
    (setq vm-ml-sort-keys "reversed-activity")
    (vm-toggle-threads-display)
    (should (equal vm-ml-sort-keys "reversed-physical-order"))))

(ert-deftest vm-thread-test-tracing-a-message-id-does-not-break-threading ()
  "REGRESSION: `vm-traced-message-ids' is a list of message ids, and
`vm-th-add-child' and `vm-th-delete-child' looked in its first element:

  (member (symbol-name id-sym) (car vm-traced-message-ids))

`member' on a string signals, so tracing any id made VM fail as soon as a
reply was threaded -- which is on visiting the folder.  Every other place
that consults the list has it right.

`vm-trace-message-id' is how the list gets a value, so the test uses that
rather than setting the variable itself."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((vm-traced-message-ids nil)
          (vm-thread-debug nil))
      (setq vm-message-pointer (cdr vm-message-list))
      (vm-trace-message-id)
      (should (equal vm-traced-message-ids '("<reply1@example.com>")))
      (vm-build-threads nil)
      (should (vm-th-parent-of (intern-soft "<reply1@example.com>"
                                            vm-thread-obarray))))))

(defun vm-thread-test--ids (args)
  "The message ids among ARGS, as strings.
The tracing calls pass an id, a thread symbol or the message itself."
  (delq nil (mapcar (lambda (thing)
                      (cond ((stringp thing) thing)
                            ((symbolp thing) (symbol-name thing))
                            ((vectorp thing) (vm-su-message-id thing))))
                    args)))

(ert-deftest vm-thread-test-a-traced-message-is-the-only-one-debugged ()
  "The debugger is entered for a traced id and for no other message: for
every message it would stop the folder being read at all."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((vm-traced-message-ids '("<reply1@example.com>"))
          (vm-thread-debug t)
          (vm-summary-show-threads t)
          (traced nil))
      (cl-letf (((symbol-function 'debug)
                 (lambda (_what &rest args)
                   (push (vm-thread-test--ids args) traced))))
        (vm-build-threads nil))
      (should traced)
      (should-not (cl-remove-if
                   (lambda (ids) (member "<reply1@example.com>" ids))
                   traced)))))

;;; What vm-build-reference-threads does with each of its arguments

(defconst vm-thread-test--deep-reference-folder
  (concat "From d@example.com Mon Jan  1 00:00:00 2024\n"
          "From: D <d@example.com>\n"
          "Subject: Re: something long ago\n"
          "Message-ID: <d@example.com>\n"
          "References: <a@example.com> <b@example.com> <c@example.com>\n"
          "\n"
          "The only message of the thread we have.\n\n")
  "One reply whose ancestors are named but not in the folder.")

(defun vm-thread-test--sym (id)
  "The thread symbol for message id ID, or nil if there is none."
  (intern-soft id vm-thread-obarray))

(ert-deftest vm-thread-test-an-untraced-ancestor-is-not-debugged ()
  "The ancestors a References header names are threaded in a pass of their
own, and the debugger is entered there for a traced id and no other."
  (vm-test-with-folder vm-thread-test--deep-reference-folder
    (let ((vm-traced-message-ids '("<d@example.com>"))
          (vm-thread-debug t)
          (vm-summary-show-threads t)
          (traced nil))
      (cl-letf (((symbol-function 'debug)
                 (lambda (_what &rest args)
                   (push (vm-thread-test--ids args) traced))))
        (vm-build-threads nil))
      (should traced)
      (should-not (cl-remove-if
                   (lambda (ids) (member "<d@example.com>" ids))
                   traced)))))

(ert-deftest vm-thread-test-references-thread-the-messages-we-lack ()
  "A References header names the whole line of ancestors, and each one is
recorded as the parent of the next even though none of them is in the
folder.  That is what puts a reply under the right root when the earlier
messages were never received or have been expunged."
  (vm-test-with-folder vm-thread-test--deep-reference-folder
    (vm-build-threads nil)
    (should (eq (vm-th-parent-of (vm-thread-test--sym "<b@example.com>"))
                (vm-thread-test--sym "<a@example.com>")))
    (should (eq (vm-th-parent-of (vm-thread-test--sym "<c@example.com>"))
                (vm-thread-test--sym "<b@example.com>")))
    (should (eq (vm-th-parent-of (vm-thread-test--sym "<d@example.com>"))
                (vm-thread-test--sym "<c@example.com>")))
    (should-not (vm-th-parent-of (vm-thread-test--sym "<a@example.com>")))))

(ert-deftest vm-thread-test-reindents-are-scheduled-only-when-asked ()
  "SCHEDULE-REINDENTS is what says the summary is already on screen and the
lines have to be redrawn.  Building the threads of a folder being visited
does not ask for it, and must not throw away the thread lines it has just
computed."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (vm-build-threads nil)
    (let ((m (car vm-message-list)))
      (vm-set-thread-list-of m '(computed))
      (vm-build-reference-threads (list m) nil nil)
      (should (equal (vm-thread-list-of m) '(computed)))
      (vm-build-reference-threads (list m) t nil)
      (should-not (vm-thread-list-of m)))))

(ert-deftest vm-thread-test-a-reply-invalidates-what-its-parent-cached ()
  "A reply arriving into a folder whose threads are built clears the cached
subtree of the message it answers: that subtree now has a message in it
that was not there when it was computed."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((root (car vm-message-list))
          (reply (nth 1 vm-message-list)))
      (setq vm-thread-obarray (make-vector 641 0)
            vm-thread-subject-obarray (make-vector 641 0))
      (vm-build-reference-threads (list root) nil t)
      (vm-set-thread-subtree-of root '(computed))
      (vm-build-reference-threads (list reply) nil nil)
      (should-not (vm-thread-subtree-of root)))))

(ert-deftest vm-thread-test-initializing-leaves-the-caches-alone ()
  "INITIALIZING says the threads database is being built from nothing, so
there is nothing cached to invalidate and the clearing is skipped.  The
same call without it clears."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((root (car vm-message-list))
          (reply (nth 1 vm-message-list)))
      (setq vm-thread-obarray (make-vector 641 0)
            vm-thread-subject-obarray (make-vector 641 0))
      (vm-build-reference-threads (list root) nil t)
      (vm-set-thread-subtree-of root '(computed))
      (vm-build-reference-threads (list reply) nil t)
      (should (equal (vm-thread-subtree-of root) '(computed)))
      (vm-set-thread-subtree-of root '(computed))
      (vm-build-reference-threads (list root) nil nil)
      (should-not (vm-thread-subtree-of root)))))

(ert-deftest vm-thread-test-a-message-with-no-parent-keeps-its-cache ()
  "A message with no parent at all -- a thread root -- has its subtree
cleared when it is threaded again, and not while the database is being
initialized, when there is nothing there to be stale."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((root (car vm-message-list)))
      (setq vm-thread-obarray (make-vector 641 0)
            vm-thread-subject-obarray (make-vector 641 0))
      (vm-build-reference-threads (list root) nil t)
      (vm-set-thread-subtree-of root '(computed))
      (vm-build-reference-threads (list root) nil t)
      (should (equal (vm-thread-subtree-of root) '(computed)))
      (vm-build-reference-threads (list root) nil nil)
      (should-not (vm-thread-subtree-of root)))))

(ert-deftest vm-thread-test-a-second-copy-of-a-reply-clears-the-cache ()
  "The same reply seen twice -- a duplicate copy in the folder -- names the
parent it already has, and the parent's cached subtree is cleared for it
just the same, unless the database is being initialized."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((root (car vm-message-list))
          (reply (nth 1 vm-message-list)))
      (setq vm-thread-obarray (make-vector 641 0)
            vm-thread-subject-obarray (make-vector 641 0))
      (vm-build-reference-threads (list root reply) nil t)
      (vm-set-thread-subtree-of root '(computed))
      (vm-build-reference-threads (list reply) nil t)
      (should (equal (vm-thread-subtree-of root) '(computed)))
      (vm-build-reference-threads (list reply) nil nil)
      (should-not (vm-thread-subtree-of root)))))

(ert-deftest vm-thread-test-progress-is-reported-every-so-many-messages ()
  "Building threads reports its progress once every tenth message rather
than for each one, a folder of thousands would otherwise spend its time in
the minibuffer.  The percentage is of two passes over the folder, so eight
messages report once, at ten steps of sixteen."
  (vm-test-with-folder vm-thread-test-threaded-folder
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (level fmt &rest args)
                   (when (string-match-p "Building threads" fmt)
                     (push (cons level (apply #'format fmt args)) said)))))
        (vm-build-reference-threads (append vm-message-list vm-message-list)
                                    nil t))
      (should (equal (length said) 1))
      (should (string-match-p "62%" (cdr (car said))))
      (should (equal (car (car said)) 7)))))

(provide 'vm-thread-test)

;;; vm-thread-test.el ends here
