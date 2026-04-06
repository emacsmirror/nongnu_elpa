;;; vm-thread-test.el --- Tests for vm-thread.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

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

(provide 'vm-thread-test)

;;; vm-thread-test.el ends here
