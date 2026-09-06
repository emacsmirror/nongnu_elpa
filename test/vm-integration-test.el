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
is the point -- if `vm' ever calls itself again, a folder visit will count two."
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

The optional bindings are installed first, so they are covered too, into
copies of the maps.  Installing them for real leaves them installed: the
functions call `define-key' on the map the variable holds, and the harness
restores VM's variables rather than what their values point at, so the run
carried VM 8 bindings from here on."
  (require 'vm)
  (vm-integration-test--check-bindings))

(defun vm-integration-test--check-bindings ()
  "Signal for every key in VM's maps bound to a command that does not exist."
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
                   vm-mime-reader-map
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

There is no longer an exception.  vm-epg.el and vm-pgg.el both defined the
three `vm-mime-display-internal-*\' handlers, and whichever loaded last held
them; vm-pgg defines its own under `vm-pgg-display-internal-*\' and takes the
shared names only when vm-epg has not (emacs-vm/vm#785)."
  (let ((seen (make-hash-table :test 'equal))
        (expected nil)
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

(ert-deftest vm-integration-test-obsolete-names-point-somewhere ()
  "Every `make-obsolete\' replacement is a name that exists.
An obsolescence notice is a promise about where to go instead, and a wrong name
in one is invisible: the notice only speaks when somebody uses the old name, and
if the old name is gone too it never speaks at all.  vm-misc.el offered
`vm-quoted-address\' for `vmrf-fix-quoted-address\', and neither existed; the
survivor is `vm-fix-quoted-address\'.

A replacement may be a function, a variable, a face or a customization group,
so all four count -- checking only `fboundp\' and `boundp\' reports every
renamed face as broken, which is what it did the first time I ran this."
  (require 'vm)
  (require 'vm-summary-faces)
  (dolist (feature '(vm-postpone vm-misc vm-vars vm-summary vm-mime vm-reply))
    (require feature nil t))
  (let (unresolved)
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "(make-obsolete\\(?:-variable\\)? '\\([^ \n)]+\\)[ \n]+'\\([^ \n)]+\\)"
                  nil t)
            (let ((old (intern (match-string 1))) (new (intern (match-string 2))))
              (unless (or (fboundp new) (boundp new) (facep new)
                          (get new 'variable-documentation) (get new 'custom-type))
                (push (format "%s -> %s (%s)" old new
                              (file-name-nondirectory file))
                      unresolved)))))))
    (should (equal nil (sort (delete-dups unresolved) #'string<)))))

(ert-deftest vm-integration-test-no-arity-probes ()
  "Nothing tells Emacs versions apart by calling a function and seeing if it fits.
Five places used to hand a function more arguments than an old Emacs took and
catch `wrong-number-of-arguments\\=' to fall back: `get-buffer-window\\=',
`insert-file-contents\\=' (falling back to running sed),
`base64-encode-region\\=', `next-window\\=' and `read-file-name\\='.  All were
dead at Emacs 28.1, the minimum this branch supports, and one of them --
`read-file-name\\=', whose sixth argument changed meaning rather than
disappearing -- hid a real defect for years, since the handler made the wrong
call look like a version difference.  See #476, #587 and #588.

Emacs signals `wrong-number-of-arguments\\=' for a genuine caller mistake, so
this also keeps such a mistake from being quietly absorbed."
  (let (probes)
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*(*wrong-number-of-arguments\\_>" nil t)
          (push (format "%s:%d" (file-name-nondirectory file)
                        (line-number-at-pos))
                probes))))
    (should (equal nil (nreverse probes)))))

(ert-deftest vm-integration-test-the-probed-arities-are-still-there ()
  "The arities those probes fell back from are the ones Emacs offers.
This is what made the handlers dead code, so it is what has to stay true."
  (dolist (probe '((insert-file-contents . 5)   ; file nil 0 4096
                   (base64-encode-region . 3)   ; start end no-line-break
                   (next-window . 3)            ; window minibuf all-frames
                   (get-buffer-window . 2)      ; buffer all-frames
                   (read-file-name . 6)))       ; ... initial predicate
    (let ((arity (func-arity (car probe))))
      (should (<= (car arity) (cdr probe)))
      (should (or (eq (cdr arity) 'many)
                  (>= (cdr arity) (cdr probe)))))))

;;; Switching folders, thread operations, and the button aliases
;;; (emacs-vm/vm#632)

(ert-deftest vm-integration-test-toggle-thread-operations ()
  "`vm-toggle-thread-operations' turns `vm-enable-thread-operations' on and
off.  With it on, a command applied to a collapsed thread applies to every
message in the thread, so this is the switch between operating on one message
and on a conversation."
  (let ((vm-enable-thread-operations nil))
    (cl-letf (((symbol-function 'vm-inform) #'ignore))
      (vm-toggle-thread-operations)
      (should vm-enable-thread-operations)
      (vm-toggle-thread-operations)
      (should-not vm-enable-thread-operations))))

(ert-deftest vm-integration-test-the-button-commands-have-their-old-names ()
  "`vm-move-to-next-button' and `vm-move-to-previous-button' are the names
the manual gives for the button commands, and are aliases for them.

Aliases are how VM keeps a name working after the command behind it is
renamed, so a broken one is a key binding or an init file that stops working
with no warning at all."
  (should (eq (symbol-function 'vm-move-to-next-button) 'vm-next-button))
  (should (eq (symbol-function 'vm-move-to-previous-button)
              'vm-previous-button))
  (should (commandp 'vm-move-to-next-button))
  (should (commandp 'vm-move-to-previous-button)))

(ert-deftest vm-integration-test-the-mime-part-structure-alias ()
  "`vm-mime-list-part-structure' is the documented name for
`vm-list-mime-part-structure'."
  (should (eq (symbol-function 'vm-mime-list-part-structure)
              'vm-list-mime-part-structure))
  (should (commandp 'vm-mime-list-part-structure)))

;;; save-restriction and the buffer it is entered in

(defconst vm-integration-test--deliberate-widens
  '("vm-gobble-crash-box" "vm-make-presentation-copy")
  "The two places that widen a buffer other than the protected one on purpose.
`vm-gobble-crash-box' widens the crash buffer it goes on to kill, and
`vm-make-presentation-copy' widens the presentation buffer before erasing and
refilling it.  Neither has a restriction anyone wants back.")

(defun vm-integration-test--widen-across-a-buffer-switch (file)
  "Answer the places in FILE where a buffer switch separates a `widen\\=' from
its nearest enclosing `save-restriction\\='.  Each is \"DEFUN:LINE\"."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let (found)
      (while (search-forward "(widen)" nil t)
        (let* ((widen-at (match-beginning 0))
               (opens (save-excursion (nth 9 (syntax-ppss widen-at))))
               (enclosing
                (mapcar (lambda (p)
                          (save-excursion
                            (goto-char (1+ p))
                            (ignore-errors (read (current-buffer)))))
                        opens))
               (save-at (car (last (delq nil (cl-mapcar
                                              (lambda (p head)
                                                (and (eq head 'save-restriction) p))
                                              opens enclosing)))))
               ;; A `with-current-buffer' counts only where it encloses the
               ;; `widen', since one that has closed again has put the buffer
               ;; back.  A `set-buffer' is a call, not a form around
               ;; anything, so for that the text in between is all there is
               ;; to go on.
               (switched
                (or (cl-some (lambda (head)
                               (memq head '(with-current-buffer with-temp-buffer)))
                             (and save-at
                                  (cl-mapcar (lambda (p head) (and (> p save-at) head))
                                             opens enclosing)))
                    (and save-at
                         (string-match
                          "(set-buffer\\_>"
                          (buffer-substring-no-properties save-at widen-at))))))
          (when (and save-at switched)
            (push (format "%s:%d"
                          (save-excursion
                            (goto-char (1+ (car opens)))
                            (ignore-errors (read (current-buffer))
                                           (format "%s" (read (current-buffer)))))
                          (line-number-at-pos widen-at))
                  found))))
      (nreverse found))))

(ert-deftest vm-integration-test-no-widen-across-a-buffer-switch ()
  "No `widen\\=' is separated from its `save-restriction\\=' by a buffer switch.
`save-restriction\\=' saves the restriction of the buffer it is entered in, so
entering it, switching buffer and then widening leaves the widened buffer
with nothing to put it back.  Five places did that, and the one with a test
of its own showed as a folder that stopped being narrowed to the message
being read (#780).

The two that remain widen a buffer they own; see
`vm-integration-test--deliberate-widens\\='.  A new offender names itself here,
which is the point: three of the five were in code no unit test reaches."
  (let (offenders)
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el"))
        (dolist (hit (vm-integration-test--widen-across-a-buffer-switch file))
          (unless (member (car (split-string hit ":"))
                          vm-integration-test--deliberate-widens)
            (push (format "%s (%s)" hit (file-name-nondirectory file))
                  offenders)))))
    (should (equal nil (sort offenders #'string<)))))

;;; Format strings the reader can see

(defconst vm-integration-test--formatters
  '((error . 1) (user-error . 1) (message . 1) (warn . 1)
    (vm-inform . 2) (vm-warn . 3))
  "Each message-formatting function, and where its format string sits.
`vm-inform' takes a level first and `vm-warn' a level and a duration.")

(defun vm-integration-test--literal-string-p (form)
  "Whether FORM is a string the reader can see, however it is assembled.
A `concat\\=' of literals is one, and so is an `if\\=' or a `cond\\=' whose every
branch is: what matters is that no data can reach the format string."
  (cond ((stringp form) t)
        ((not (consp form)) nil)
        ((eq (car form) 'concat)
         (cl-every #'vm-integration-test--literal-string-p (cdr form)))
        ;; `substitute-command-keys' only puts key descriptions in, so a
        ;; literal run through it is still a format string its author wrote
        ;; and can see the directives of.  vm.el's auto-save warning builds
        ;; one that way on purpose and passes it an argument.
        ((eq (car form) 'substitute-command-keys)
         (vm-integration-test--literal-string-p (nth 1 form)))
        ((eq (car form) 'if)
         (cl-every #'vm-integration-test--literal-string-p (nthcdr 2 form)))
        ((memq (car form) '(or and progn))
         (cl-every #'vm-integration-test--literal-string-p (cdr form)))
        ((memq (car form) '(when unless))
         (cl-every #'vm-integration-test--literal-string-p (nthcdr 2 form)))
        ((eq (car form) 'cond)
         (cl-every (lambda (clause)
                     (and (consp clause)
                          (cl-every #'vm-integration-test--literal-string-p
                                    (cdr clause))))
                   (cdr form)))
        (t nil)))

(defun vm-integration-test--computed-format-strings (form where found)
  "Add to FOUND every call in FORM whose format string is computed.
WHERE names the file and line.  Binding lists are walked for their values
only: a `dolist\\=' over a variable called `message\\=' is not a call to
`message\\='."
  (if (not (consp form))
      found
    (let ((head (car form)))
      (cond
       ((memq head '(quote function declare-function autoload defvar defcustom))
        found)
       ((eq head 'condition-case)
        (setq found (vm-integration-test--computed-format-strings
                     (nth 2 form) where found))
        (dolist (handler (nthcdr 3 form) found)
          (when (consp handler)
            (setq found (vm-integration-test--computed-format-strings
                         (cons 'progn (cdr handler)) where found)))))
       ((memq head '(defun defsubst defmacro cl-defun cl-defsubst cl-defmacro))
        (dolist (sub (nthcdr 3 form) found)
          (setq found (vm-integration-test--computed-format-strings
                       sub where found))))
       ((eq head 'lambda)
        (dolist (sub (nthcdr 2 form) found)
          (setq found (vm-integration-test--computed-format-strings
                       sub where found))))
       ((eq head 'cond)
        (dolist (clause (cdr form) found)
          (when (consp clause)
            (dolist (sub clause)
              (setq found (vm-integration-test--computed-format-strings
                           sub where found))))))
       ((memq head '(cl-case cl-ecase pcase pcase-exhaustive))
        (setq found (vm-integration-test--computed-format-strings
                     (nth 1 form) where found))
        (dolist (clause (nthcdr 2 form) found)
          (when (consp clause)
            (dolist (sub (cdr clause))
              (setq found (vm-integration-test--computed-format-strings
                           sub where found))))))
       ((memq head '(dolist dotimes cl-dolist cl-dotimes))
        (dolist (v (cdr (nth 1 form)))
          (setq found (vm-integration-test--computed-format-strings
                       v where found)))
        (dolist (sub (nthcdr 2 form) found)
          (setq found (vm-integration-test--computed-format-strings
                       sub where found))))
       ((memq head '(let let* cl-letf cl-letf*))
        (dolist (binding (nth 1 form))
          (when (consp binding)
            (dolist (v (cdr binding))
              (setq found (vm-integration-test--computed-format-strings
                           v where found)))))
        (dolist (sub (nthcdr 2 form) found)
          (setq found (vm-integration-test--computed-format-strings
                       sub where found))))
       (t
        (let ((pos (cdr (assq head vm-integration-test--formatters))))
          (when (and pos (> (length form) pos)
                     (not (vm-integration-test--literal-string-p (nth pos form))))
            (push (format "%s: (%s ...)" where head) found)))
        (dolist (sub form found)
          (setq found (vm-integration-test--computed-format-strings
                       sub where found))))))))

(ert-deftest vm-integration-test-format-strings-are-literal ()
  "Nothing hands a built string to a function that formats its own message.
`error\\=', `message\\=', `warn\\=', `vm-inform\\=' and `vm-warn\\=' all format what they
are given, so a string that has been through `format\\=' already is formatted a
second time and any percent in it is read as a directive.  Fifteen calls did
that, and `g' in a folder named `100% done\\=' signalled instead of saying what
had arrived (#781).

`\"%s\"' and the string as an argument is the fix, every time."
  (let (offenders)
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((form t))
            (while form
              (setq form (condition-case nil (read (current-buffer))
                           (end-of-file nil)))
              (when form
                (setq offenders
                      (vm-integration-test--computed-format-strings
                       form
                       (format "%s:%d" (file-name-nondirectory file)
                               (line-number-at-pos))
                       offenders))))))))
    (should (equal nil (sort offenders #'string<)))))

;;; A message written into a folder ends with a newline

(defconst vm-integration-test--terminates-otherwise
  '(("vm-digest.el"   . vm-mime-burst-layout)
    ("vm-folder.el"   . vm-change-folder-type)
    ("vm-folder.el"   . vm-convert-folder-type)
    ("vm-pop.el"      . vm-pop-retrieve-to-target)
    ("vm-postpone.el" . vm-postpone-message)
    ("vm-save.el"     . vm-save-message-to-local-folder))
  "Writers that end the message some other way than by asking where point is.

- `vm-mime-burst-layout\\=' inserts a newline whether one is wanted or not.
- `vm-postpone-message\\=' trims the trailing whitespace and writes \"\\n\\n\\n\".
- `vm-pop-retrieve-to-target\\=' ends the region at the start of the \".\" line
  that closes a POP response, so the text before it ends with a newline.
  IMAP needs a check because there the length is the server\\='s word for it.
- `vm-change-folder-type\\=', `vm-convert-folder-type\\=' and
  `vm-save-message-to-local-folder\\=' copy a message already delimited in a
  folder.  Adding a newline there would also have to correct the byte count
  written ahead of it, so it is not the same one-line change.")

(defun vm-integration-test--terminating-writers (file)
  "The functions in FILE that write a trailing message separator.
Each is (NAME . GUARDED), GUARDED saying whether the function asks anywhere
whether the text ends with a newline."
  (with-temp-buffer
    (insert-file-contents file)
    (let (found)
      (goto-char (point-min))
      (while (search-forward "(vm-trailing-message-separator" nil t)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (or (string-match-p "defun " line)
                      (string-match-p "declare-function" line))
            (save-excursion
              (beginning-of-defun)
              (let* ((name (save-excursion
                             (forward-char 1)
                             (ignore-errors (read (current-buffer))
                                            (read (current-buffer)))))
                     (body (buffer-substring-no-properties
                            (point)
                            (save-excursion (end-of-defun) (point))))
                     (guarded (string-match-p
                               "preceding-char\\|(bolp)\\|char-before\\|char-after (1-"
                               body))
                     (cell (assq name found)))
                (if cell
                    (setcdr cell (or (cdr cell) (and guarded t)))
                  (push (cons name (and guarded t)) found)))))))
      found)))

(ert-deftest vm-integration-test-a-message-written-to-a-folder-ends-with-a-newline ()
  "Every writer of a trailing message separator terminates the message first.
A message in a folder ends with a newline: the From_ trailing separator then
makes the blank line the next envelope line has to follow, and mmdf's and
babyl\\='s separators begin a line of their own.  mboxcl2 has no trailing
separator at all, so without the newline the next envelope line is glued to
the end of the previous body.

`vm-fcc-message-text\\=' filed a composition as it stood, and a composition
need not end with a newline, so two filed messages read back as one
(emacs-vm/vm#783).  Two more writers had the same gap.  The ones that
terminate some other way are named in
`vm-integration-test--terminates-otherwise\\=', with the reason for each."
  (let (offenders)
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el"))
        (pcase-dolist (`(,name . ,guarded)
                       (vm-integration-test--terminating-writers file))
          (unless (or guarded
                      (member (cons (file-name-nondirectory file) name)
                              vm-integration-test--terminates-otherwise))
            (push (format "%s (%s)" name (file-name-nondirectory file))
                  offenders)))))
    (should (equal nil (sort offenders #'string<)))))


;;; Keyword arguments belong to cl-defun, not defun

(defun vm-integration-test--lisp-files ()
  "Every VM lisp file worth auditing, the generated ones left out."
  (seq-remove (lambda (f)
                (member (file-name-nondirectory f)
                        '("vm-autoloads.el" "vm-cus-load.el"
                          "vm-version-conf.el")))
              (directory-files vm-test-lisp-dir t "\\.el\\'")))

(ert-deftest vm-integration-test-only-cl-defun-takes-keyword-arguments ()
  "No plain `defun', `defsubst' or `defmacro' has `&key' or `&aux' in its
arglist.

Emacs Lisp's own lambda list knows `&optional' and `&rest' and nothing else,
so a `&key' there is not a keyword marker: it becomes an ordinary variable
whose name happens to be `&key', and the argument after it becomes one more
positional.  `vm-retrieve-operable-messages' was written that way and worked
by coincidence, every caller passing `:fail t' so that the variable named
`&key' swallowed the `:fail' and `t' landed in `fail'.  Called as
`(f 1 mlist :fail)' it silently answered nil, and a second keyword would have
been a wrong-number-of-arguments error.

It also stops edebug reading the file, so `testcover' could not instrument
vm-folder.el at all and test/forms-coverage-report.el was blind to the
largest file in the tree.

Walks the arglist of every definition in lisp/."
  (let ((offenders nil))
    (dolist (file (vm-integration-test--lisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(\\(defun\\|defsubst\\|defmacro\\) " nil t)
          (let ((start (match-beginning 0)))
            (goto-char start)
            (let ((form (condition-case nil (read (current-buffer)) (error nil))))
              (when (and form (listp (nth 2 form))
                         (or (memq '&key (nth 2 form))
                             (memq '&aux (nth 2 form))))
                (push (format "%s:%d %s"
                              (file-name-nondirectory file)
                              (line-number-at-pos start)
                              (nth 1 form))
                      offenders)))))))
    (should (equal nil (nreverse offenders)))))

(defun vm-integration-test--instrument-in-a-subprocess ()
  "The VM files a fresh Emacs cannot instrument with `testcover', by name.

In a subprocess, and that is the whole point.  `testcover-start' leaves the
instrumentation in place, and instrumented code raises an error of testcover's
own the moment a form it thought constant returns something else: doing this
in the suite's own Emacs made a later test die with \"Value of form expected
to be constant does vary\" inside `vm-postpone.el'.  A test that instruments
the tree cannot share an Emacs with the tests that follow it."
  (let* ((lisp (expand-file-name "lisp" (file-name-directory
                                         (directory-file-name vm-test-lisp-dir))))
         (form `(let ((failed nil))
                  (require 'testcover)
                  (dolist (file (directory-files ,vm-test-lisp-dir t "\\.el\\'"))
                    (unless (member (file-name-nondirectory file)
                                    '("vm-autoloads.el" "vm-cus-load.el"
                                      "vm-version-conf.el"))
                      (condition-case err
                          (testcover-start file)
                        (error
                         (push (cons (file-name-nondirectory file)
                                     (error-message-string err))
                               failed)))))
                  (prin1 (nreverse failed)))))
    (ignore lisp)
    (with-temp-buffer
      (let ((status (call-process
                     (expand-file-name invocation-name invocation-directory)
                     nil t nil "-batch" "-Q" "-L" vm-test-lisp-dir
                     "--eval" (prin1-to-string form))))
        (should (equal status 0))
        (goto-char (point-max))
        (backward-sexp)
        (read (current-buffer))))))

(ert-deftest vm-integration-test-every-file-can-be-instrumented ()
  "edebug can read every VM file, so `testcover' can measure every one.

test/forms-coverage-report.el instruments the tree to report which forms ran.
A file edebug cannot parse is a file that report says nothing about, and it
says nothing quietly: the run still finishes and the numbers still look
plausible.  Two arglists did it, `&key' in a plain `defun' in vm-folder.el
and an `&optional' with no arguments after it in vm-imap.el, and between them
they hid the two largest files in the tree (emacs-vm/vm#795)."
  (should (equal nil (vm-integration-test--instrument-in-a-subprocess))))


;;; The example ~/.vm we install (emacs-vm/vm#790)

;; Loaded so the names it uses are defined here: Personality Crisis is not
;; pulled in by `vm' itself, and a name this Emacs has not seen looks missing.
(require 'vm-pcrisis)

(defconst vm-integration-test--example-vm
  (expand-file-name "example.vm" (file-name-directory
                                  (directory-file-name vm-test-lisp-dir)))
  "The example configuration at the top of the tree.
`make install' puts it in the doc directory beside README and the NEWS
files, so it is something a reader is handed and may copy to `~/.vm'.")

(defun vm-integration-test--forms-of (file)
  "Every top-level form of FILE, or a string saying why they could not be read."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case caught
          (while t (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))
        (error (error-message-string caught))))))

(defun vm-integration-test--names-that-are-missing (form)
  "The VM variables set and VM functions called by FORM that do not exist.

Quoted data is not walked: `esmtpmail-send-it-by-alist' holds forms naming
`vm-pop-login' and `vm-after-pop', which are that package's vocabulary and
not calls anything makes."
  (let ((missing nil))
    (cond
     ((not (consp form)) nil)
     ((memq (car form) '(quote function)) nil)
     ((memq (car form) '(setq setq-default))
      (let ((tail (cdr form)))
        (while tail
          (let ((symbol (car tail)))
            (when (and (symbolp symbol)
                       (string-prefix-p "vm" (symbol-name symbol))
                       (not (boundp symbol)))
              (push symbol missing)))
          (setq missing (append (vm-integration-test--names-that-are-missing
                                 (cadr tail))
                                missing))
          (setq tail (cddr tail)))))
     (t
      (when (and (symbolp (car form))
                 (string-prefix-p "vm" (symbol-name (car form)))
                 (not (fboundp (car form))))
        (push (car form) missing))
      (dolist (sub form)
        (setq missing (append (vm-integration-test--names-that-are-missing sub)
                              missing)))))
    missing))

(ert-deftest vm-integration-test-the-example-configuration-reads ()
  "REGRESSION: example.vm parses, so a reader who loads it gets a config.

emacs-vm/vm#790.  It did not.  The `vm-mime-type-converter-alist' form was
never closed, so every form after it became an argument to that `setq' and
loading the file answered

    Wrong number of arguments: setq, 3

Ten of the twenty top-level forms were swallowed, the w3m setup and the
Personality Crisis example among them.  `make install' puts this file in the
doc directory, so it is something a reader is handed."
  (let ((forms (vm-integration-test--forms-of vm-integration-test--example-vm)))
    (should (listp forms))
    ;; and all of it, not the first few
    (should (> (length forms) 15))))

(ert-deftest vm-integration-test-the-example-configuration-names-what-exists ()
  "Every VM variable example.vm sets and VM function it calls exists.

The other half of what `not tested' meant: a configuration naming an option
VM has renamed away is one a reader copies and then debugs."
  (let ((forms (vm-integration-test--forms-of vm-integration-test--example-vm))
        (missing nil))
    (should (listp forms))
    (dolist (form forms)
      (setq missing (append (vm-integration-test--names-that-are-missing form)
                            missing)))
    (should (equal nil (delete-dups (nreverse missing))))))

(provide 'vm-integration-test)

;;; vm-integration-test.el ends here
