;;; vm-integration-test.el --- Integration tests for VM -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Integration tests that use full VM folders to test higher-level
;; functions. These tests exercise many underlying functions.

;;; Code:

(require 'vm-test-init)
(require 'vm-delete)
(require 'vm-mark)
(require 'vm-undo)
(require 'vm-sort)
(require 'vm-summary)

;;; Test folder with multiple messages

(defconst vm-integration-test-folder
  "From sender1@example.com Mon Jan  1 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: First Message
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <msg1@example.com>

This is the first message body.

From sender2@example.com Tue Jan  2 00:00:00 2024
From: sender2@example.com
To: recipient@example.com
Subject: Re: First Message
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <msg2@example.com>
In-Reply-To: <msg1@example.com>

This is a reply to the first message.

From sender3@example.com Wed Jan  3 00:00:00 2024
From: sender3@example.com
To: recipient@example.com
Subject: Another Thread
Date: Wed, 03 Jan 2024 12:00:00 +0000
Message-ID: <msg3@example.com>

This starts a new thread.

From sender1@example.com Thu Jan  4 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: Fourth Message
Date: Thu, 04 Jan 2024 13:00:00 +0000
Message-ID: <msg4@example.com>

Fourth message from sender1.

"
  "Test folder with 4 messages for integration tests.")

;;; Message accessors tests

(ert-deftest vm-integration-test-message-count ()
  "Test that folder has expected number of messages."
  (vm-test-with-folder vm-integration-test-folder
    (should (= (length vm-message-list) 4))))

;;; Note: vm-from-of, vm-subject-of, vm-message-id-of require cached data
;;; that is populated lazily. Use vm-su-* functions instead which parse headers.

(ert-deftest vm-integration-test-get-header-contents-from ()
  "Test vm-get-header-contents for From header."
  (vm-test-with-folder vm-integration-test-folder
    (let ((from (vm-get-header-contents (car vm-message-list) "From:")))
      (should (stringp from))
      (should (string-match "sender1" from)))))

(ert-deftest vm-integration-test-get-header-contents-subject ()
  "Test vm-get-header-contents for Subject header."
  (vm-test-with-folder vm-integration-test-folder
    (let ((subject (vm-get-header-contents (car vm-message-list) "Subject:")))
      (should (stringp subject))
      (should (string-match "First Message" subject)))))

(ert-deftest vm-integration-test-get-header-contents-message-id ()
  "Test vm-get-header-contents for Message-ID header."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg-id (vm-get-header-contents (car vm-message-list) "Message-ID:")))
      (should (stringp msg-id))
      (should (string-match "msg1@example.com" msg-id)))))

;;; Delete/undelete tests

(ert-deftest vm-integration-test-set-deleted-flag ()
  "Test vm-set-deleted-flag on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-deleted-flag msg))
      (vm-set-deleted-flag msg t)
      (should (vm-deleted-flag msg))
      (vm-set-deleted-flag msg nil)
      (should-not (vm-deleted-flag msg)))))

;;; Mark tests

(ert-deftest vm-integration-test-set-mark ()
  "Test vm-set-mark-of on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-mark-of msg))
      (vm-set-mark-of msg t)
      (should (vm-mark-of msg))
      (vm-set-mark-of msg nil)
      (should-not (vm-mark-of msg)))))

;;; Flag tests

(ert-deftest vm-integration-test-new-flag ()
  "Test new flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      ;; Initially messages may be new or not depending on parsing
      (vm-set-new-flag msg t)
      (should (vm-new-flag msg))
      (vm-set-new-flag msg nil)
      (should-not (vm-new-flag msg)))))

(ert-deftest vm-integration-test-unread-flag ()
  "Test unread flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-unread-flag msg t)
      (should (vm-unread-flag msg))
      (vm-set-unread-flag msg nil)
      (should-not (vm-unread-flag msg)))))

(ert-deftest vm-integration-test-replied-flag ()
  "Test replied flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-replied-flag msg))
      (vm-set-replied-flag msg t)
      (should (vm-replied-flag msg)))))

(ert-deftest vm-integration-test-forwarded-flag ()
  "Test forwarded flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-forwarded-flag msg))
      (vm-set-forwarded-flag msg t)
      (should (vm-forwarded-flag msg)))))

(ert-deftest vm-integration-test-filed-flag ()
  "Test filed flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-filed-flag msg))
      (vm-set-filed-flag msg t)
      (should (vm-filed-flag msg)))))

;;; Undo tests with real messages

(ert-deftest vm-integration-test-undo-record-delete ()
  "Test undo recording for delete operations."
  (vm-test-with-folder vm-integration-test-folder
    (let ((vm-undo-record-list nil)
          (msg (car vm-message-list)))
      ;; Delete should record undo
      (vm-set-deleted-flag msg t)
      ;; Check undo record was created
      (should vm-undo-record-list)
      (should (eq (car (car vm-undo-record-list)) 'vm-set-deleted-flag)))))

;;; Sort trim-subject tests

(ert-deftest vm-integration-test-trim-subject-of-real-message ()
  "Test vm-so-trim-subject on real message subject."
  (vm-test-with-folder vm-integration-test-folder
    (let ((vm-subject-ignored-prefix "^\\(re: *\\)+")
          (vm-subject-ignored-suffix nil)
          (vm-subject-tag-prefix nil)
          (vm-subject-significant-chars nil))
      ;; Second message has "Re: First Message" - get it via header
      (let ((subject (vm-get-header-contents (nth 1 vm-message-list) "Subject:")))
        (should (string-match "Re:" subject))
        (let ((trimmed (vm-so-trim-subject subject)))
          (should (string= trimmed "First Message")))))))

;;; Message body tests

(ert-deftest vm-integration-test-message-body ()
  "Test extracting message body."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (vm-find-and-set-text-of msg)
      (let ((body (buffer-substring-no-properties
                   (vm-text-of msg)
                   (vm-text-end-of msg))))
        (should (string-match "first message body" body))))))

;;; Summary tests

(ert-deftest vm-integration-test-su-from ()
  "Test vm-su-from on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should (string-match "sender1" (vm-su-from msg))))))

(ert-deftest vm-integration-test-su-subject ()
  "Test vm-su-subject on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should (string-match "First Message" (vm-su-subject msg))))))

(ert-deftest vm-integration-test-su-to ()
  "Test vm-su-to on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should (string-match "recipient" (vm-su-to msg))))))

;;; Header iteration

(ert-deftest vm-integration-test-iterate-headers ()
  "Test iterating headers on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list))
          (headers '()))
      (save-excursion
        (goto-char (vm-headers-of msg))
        (while (re-search-forward "^\\([^:]+\\):" (vm-text-of msg) t)
          (push (match-string 1) headers)))
      (should (member "From" headers))
      (should (member "To" headers))
      (should (member "Subject" headers)))))

;;; Multiple flag operations

(ert-deftest vm-integration-test-multiple-flags ()
  "Test setting multiple flags on same message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-deleted-flag msg t)
      (vm-set-replied-flag msg t)
      (vm-set-filed-flag msg t)
      (should (vm-deleted-flag msg))
      (should (vm-replied-flag msg))
      (should (vm-filed-flag msg)))))

;;; Message traversal

(ert-deftest vm-integration-test-message-links ()
  "Test message list links."
  (vm-test-with-folder vm-integration-test-folder
    (let* ((first (car vm-message-list))
           (second (nth 1 vm-message-list))
           (third (nth 2 vm-message-list)))
      ;; Test forward links
      (should (eq second (car (cdr vm-message-list))))
      (should (eq third (nth 2 vm-message-list))))))


;;; the session-beginning invariant behind #240

(ert-deftest vm-integration-test-session-beginning-is-always-bound ()
  "`vm-session-beginning' is bound before `vm' can be called.
Issue #240 was a recursive call in `vm', guarded by
`(unless (boundp \\='vm-session-beginning) ...)\\=' and commented as being there to
allow advice on the first call.  It could never run: the variable is defvar'd
unconditionally in vm-vars.el, which vm-autoloads.el requires, so it is bound
before `vm' is reachable.

This pins that invariant, so the guard cannot look meaningful again to someone
reading the old code in the history."
  (require 'vm-vars)
  (should (boundp 'vm-session-beginning)))

(ert-deftest vm-integration-test-session-initialization-binds-it-too ()
  "`vm-session-initialization' leaves `vm-session-beginning' bound and nil.
The second, independent reason the guard in #240 was dead: it stood *after* a
call to `vm-session-initialization', which requires vm-vars itself and then sets
the variable.  So even without the defvar reaching it first, the test could not
have been true by the time it was made."
  (require 'vm)
  (let ((vm-init-file nil)
        (vm-preferences-file nil))
    (vm-session-initialization)
    (should (boundp 'vm-session-beginning))
    (should (null vm-session-beginning))))

(ert-deftest vm-integration-test-vm-does-not-call-itself ()
  "`vm' visits a folder with one call to itself, not two.
A guard against reintroducing #240 rather than a reproduction of it: the removed
call was unreachable, so this passed before the change as well.  What it protects
is the shape -- if `vm' ever calls itself again, a folder visit will count two."
  (require 'vm)
  (let* ((dir (file-name-as-directory (make-temp-file "vm-240" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list))
         (calls 0)
         (counter (lambda (orig &rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "From a@example.com Mon Jan  1 00:00:00 2024\n"
                    "From: A <a@example.com>\nSubject: one\n\nBody.\n\n"))
          (advice-add 'vm :around counter)
          (unwind-protect
              (vm file)
            (advice-remove 'vm counter))
          (should (= 1 calls))
          (should (= 1 (length vm-message-list))))
      ;; the visit leaves the folder, its summary and its presentation copy
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))


;;; vm-startup-hook (issue #565)

(defvar vm-integration-test--startup-ran nil)

(ert-deftest vm-integration-test-startup-hook-runs-once-at-startup ()
  "`vm-startup-hook' runs when VM starts, and only then.
Issue #565: VM had no hook for \"VM has just started\", which is the gap that
produced the recursive call in `vm' removed for #240 -- a 2007 request for a way
to run code on VM's first invocation, answered by making `vm' call itself so
advice would see an extra call.

Run through `vm-session-initialization', which is where it belongs: the first
call does the work and runs the hook, and later calls do neither because
`vm-session-beginning' is nil by then."
  (require 'vm)
  (let ((vm-init-file nil)
        (vm-preferences-file nil)
        ;; as above: a second initialization rebuilds these
        (vm-buffers-needing-display-update vm-buffers-needing-display-update)
        (vm-buffers-needing-undo-boundaries vm-buffers-needing-undo-boundaries)
        (calls 0))
    ;; a session that has not begun yet
    (let ((vm-session-beginning t)
          (vm-startup-hook (list (lambda () (setq calls (1+ calls))))))
      (vm-session-initialization)
      (should (= 1 calls))
      ;; and again: the session has begun, so nothing runs a second time
      (vm-session-initialization)
      (should (= 1 calls)))))

(ert-deftest vm-integration-test-startup-hook-runs-after-vm-is-set-up ()
  "The hook runs last, so a function on it sees VM assembled and can override it.
That is the placement decision on #565: running before VM's own setup would mean
anything the hook did got overwritten, and `with-eval-after-load' already serves
whoever wants to act first."
  (require 'vm)
  (let ((vm-init-file nil)
        (vm-preferences-file nil)
        ;; A second initialization rebuilds the two obarrays VM uses as sets,
        ;; which the rest of the suite is holding; bound, so they go back.
        (vm-buffers-needing-display-update vm-buffers-needing-display-update)
        (vm-buffers-needing-undo-boundaries vm-buffers-needing-undo-boundaries)
        (session-flag 'unset))
    (let ((vm-session-beginning t)
          (vm-startup-hook
           (list (lambda ()
                   ;; by now the session is marked as begun, so a hook function
                   ;; may call VM commands without re-entering initialization
                   (setq session-flag vm-session-beginning)))))
      (vm-session-initialization))
    (should (eq nil session-flag))))

(ert-deftest vm-integration-test-startup-hook-is-a-hook-variable ()
  "It is a `defcustom' of type hook, like VM's other hooks.
So `add-hook' works on it and Customize offers it beside the rest."
  (require 'vm-vars)
  (should (boundp 'vm-startup-hook))
  (should (eq 'hook (get 'vm-startup-hook 'custom-type)))
  ;; Membership is recorded on the group, not on the variable.
  (should (assq 'vm-startup-hook (get 'vm-hooks 'custom-group)))
  (should (null (default-value 'vm-startup-hook))))

(ert-deftest vm-integration-test-every-alias-has-a-target ()
  "Every VM function alias resolves to a function that exists.
An alias to a deleted function is a `void-function' waiting for whoever still
calls the old name, and nothing in a byte-compile or a lint run notices: the
alias itself is well-formed.  Two were found this way,
`vm-mime-nuke-alternative-text/html' pointing at a misspelling of its target and
`vm-pine-fake-attachment-overlays' at a function deleted in 2011.

Only what is loaded at this point is checked, which is most of VM by the time
the suite runs."
  (require 'vm)
  (let (broken)
    (mapatoms
     (lambda (s)
       (when (and (string-prefix-p "vm" (symbol-name s))
                  (fboundp s))
         (let ((target (symbol-function s)))
           (when (and (symbolp target) target (not (fboundp target)))
             (push (format "%s -> %s" s target) broken))))))
    (should (equal nil (sort broken #'string<)))))

(ert-deftest vm-integration-test-every-binding-has-a-command ()
  "Every key VM binds runs a command that exists.
A keymap entry naming a deleted or misspelled command is a `void-function' the
first time somebody presses the key, and nothing in a byte-compile or a lint run
sees it: the `define-key' call is well-formed whatever symbol it is given.

The optional bindings are installed first, so they are covered too."
  (require 'vm)
  (vm-v8-key-bindings)
  (let (broken)
    (cl-labels ((walk (map path name)
                  (map-keymap
                   (lambda (event def)
                     (let ((keys (append path (list event))))
                       (cond ((keymapp def) (walk def keys name))
                             ((and (symbolp def) def (not (fboundp def)))
                              (push (format "%s %s -> %s" name
                                            (key-description (vconcat keys)) def)
                                    broken)))))
                   map)))
      (dolist (m '(vm-mode-map vm-summary-mode-map vm-mail-mode-map
                   vm-mime-reader-map vm-folders-summary-mode-map
                   vm-mode-virtual-map vm-mode-mark-map vm-mode-window-map
                   vm-mode-pipe-map))
        (when (and (boundp m) (keymapp (symbol-value m)))
          (walk (symbol-value m) nil (symbol-name m)))))
    (should (equal nil (sort (delete-dups broken) #'string<)))))

(ert-deftest vm-integration-test-no-accidental-duplicate-definitions ()
  "No function is defined in two files, bar the ones that mean to be.
Issue #584.  `vm-vs-and\', `vm-vs-or\' and `vm-vs-not\' were defined in both
vm-virtual.el and vm-avirtual.el, so which one you got depended on load order,
and the copy with the diagnostics lost.  Nothing warns about this: each
`defun\' is fine on its own.

The exception is the three `vm-mime-display-internal-*\' handlers that vm-epg.el
and vm-pgg.el both define.  That pair is deliberate -- they are alternative
implementations, only one is meant to be loaded, and both files and the manual
say so."
  (let ((seen (make-hash-table :test 'equal))
        (expected '("vm-mime-display-internal-application/pgp-keys"
                    "vm-mime-display-internal-multipart/encrypted"
                    "vm-mime-display-internal-multipart/signed"))
        duplicates)
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "^(def\\(un\\|subst\\|macro\\) \\([^ ()\n]+\\)" nil t)
            (let* ((name (match-string 2))
                   (where (gethash name seen)))
              (when (and where (not (equal where file)))
                (push name duplicates))
              (puthash name file seen))))))
    (should (equal expected (sort (delete-dups duplicates) #'string<)))))

(provide 'vm-integration-test)

;;; vm-integration-test.el ends here
