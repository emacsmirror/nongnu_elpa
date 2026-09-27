;;; vm-edit-test.el --- Tests for vm-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM edit functions in vm-edit.el

;;; Code:

(require 'vm-test-init)
(require 'vm-edit)
(require 'vm-reply)

;;; Edit function existence tests

(ert-deftest vm-edit-test-functions-exist ()
  "Test that edit functions exist."
  (should (fboundp 'vm-edit-message))
  (should (fboundp 'vm-edit-message-other-frame))
  (should (fboundp 'vm-discard-cached-data))
  (should (fboundp 'vm-discard-cached-data-internal))
  (should (fboundp 'vm-edit-message-end))
  (should (fboundp 'vm-edit-message-abort)))

;;; vm-discard-cached-data-internal tests
;; Note: vm-discard-cached-data-internal requires full folder context.
;; These tests verify the underlying fillarray behavior on cached-data.

(ert-deftest vm-edit-test-fillarray-clears-cached-data ()
  "Test that fillarray clears cached data vector."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    ;; Set some cached values
    (vm-set-subject-of msg "Cached Subject")
    (vm-set-from-of msg "Cached From")
    (should (equal "Cached Subject" (vm-subject-of msg)))
    (should (equal "Cached From" (vm-from-of msg)))
    ;; Use fillarray like vm-discard-cached-data-internal does
    (fillarray (vm-cached-data-of msg) nil)
    ;; After fillarray, should be nil
    (should (null (vm-subject-of msg)))
    (should (null (vm-from-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-message-id ()
  "Test that fillarray clears cached message-id."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-message-id-of msg "<cached@example.com>")
    (should (equal "<cached@example.com>" (vm-message-id-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-message-id-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-byte-count ()
  "Test that fillarray clears cached byte count."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-byte-count-of msg "12345")
    (should (equal "12345" (vm-byte-count-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-byte-count-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-line-count ()
  "Test that fillarray clears cached line count."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-line-count-of msg "100")
    (should (equal "100" (vm-line-count-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-line-count-of msg)))))

;;; Edit state tests

(ert-deftest vm-edit-test-edited-flag ()
  "Test that the edited flag can be set and read."
  (let* ((msg (vm-make-message))
         (attrs (make-vector vm-attributes-vector-length nil)))
    (vm-set-attributes-of msg attrs)
    ;; Initially not edited
    (should-not (vm-edited-flag msg))
    ;; Set edited flag directly in attributes vector
    (aset attrs 7 t)
    (should (vm-edited-flag msg))
    ;; Clear edited flag
    (aset attrs 7 nil)
    (should-not (vm-edited-flag msg))))

;;; Edit buffer tracking

(ert-deftest vm-edit-test-edit-buffer-of ()
  "Test vm-edit-buffer-of accessor."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message))
          (edit-buf (generate-new-buffer " *test-edit*")))
      (unwind-protect
          (progn
            ;; Initially no edit buffer
            (should (null (vm-edit-buffer-of msg)))
            ;; Set edit buffer
            (vm-set-edit-buffer-of msg edit-buf)
            (should (eq edit-buf (vm-edit-buffer-of msg)))
            ;; Clear edit buffer
            (vm-set-edit-buffer-of msg nil)
            (should (null (vm-edit-buffer-of msg))))
        (kill-buffer edit-buf)))))

;;; editing an external (headers-only) message

(defvar vm-edit-test-folder
  (concat "From sender@example.com Sat Aug  1 12:00:00 2026\n"
          "From: sender@example.com\n"
          "To: me@example.com\n"
          "Subject: an external message\n"
          "\n"
          "the body as fetched from the server\n"
          "\n")
  "A one-message folder, used as an IMAP message held in external mode.")

(defun vm-edit-test-make-external (m)
  "Register M as a fetched IMAP message, as `vm-register-fetched-message' does.
Its body is present but marked for discarding once the fetched-message
limit evicts it, or when the folder discards fetched bodies wholesale."
  (vm-set-message-access-method-of m 'imap)
  (vm-set-body-to-be-retrieved-flag m nil t)
  (setq vm-fetched-messages (list m)
        vm-fetched-message-count 1)
  (vm-set-body-to-be-discarded-of m t))

(ert-deftest vm-edit-test-end-keeps-edited-external-body ()
  "Test that editing an external message stops its body being discarded.
Regression test for issue #376.  An external message keeps a
body-to-be-discarded flag; after an edit the edited text exists only in
the folder, so discarding it throws the edit away and the server's copy
comes back in its place."
  (vm-test-with-folder vm-edit-test-folder
    (let* ((m (car vm-message-list))
           (vm-enable-external-messages '(imap))
           (edit-buf (generate-new-buffer " *vm-edit-test*")))
      (unwind-protect
          (progn
            (vm-edit-test-make-external m)
            (should (vm-body-to-be-discarded-of m))
            ;; stand in for the user's edit session
            (setq vm-message-pointer vm-message-list)
            (with-current-buffer edit-buf
              (insert-buffer-substring
               (vm-buffer-of m) (vm-headers-of m) (vm-text-end-of m))
              (goto-char (point-min))
              (should (search-forward "as fetched from the server" nil t))
              (replace-match "as edited by hand")
              (setq vm-message-pointer (list m)
                    vm-mail-buffer (vm-buffer-of m))
              (set-buffer-modified-p t)
              (cl-letf (((symbol-function 'vm-present-current-message) #'ignore)
                        ((symbol-function 'vm-update-summary-and-mode-line)
                         #'ignore)
                        ((symbol-function 'vm-display)
                         (lambda (&rest _) nil)))
                (vm-edit-message-end)))
            ;; the edit landed, and the message is no longer registered
            ;; as a fetched one whose body may be thrown away
            (should (vm-edited-flag m))
            (should-not (vm-body-to-be-discarded-of m))
            (should-not (memq m vm-fetched-messages))
            ;; so discarding fetched bodies leaves the edited one alone
            (vm-discard-fetched-messages)
            (should (string-match "as edited by hand"
                                  (vm-test-message-body m)))
            (should-not (vm-body-to-be-retrieved-of m)))
        (when (buffer-live-p edit-buf) (kill-buffer edit-buf))))))

;;; a failed edit must leave the message alone (issue #307)

(defmacro vm-edit-test--with-edit-session (spec &rest body)
  "Set up an edit of the folder's first message and run BODY.
SPEC is (MSG-VAR EDIT-BUF-VAR NEW-TEXT): the edit buffer is filled with the
message and NEW-TEXT replaces its body, as a user's edit would.  BODY runs
with the display functions stubbed, since there is no display in batch."
  (declare (indent 1) (debug t))
  `(let* ((,(car spec) (car vm-message-list))
          (,(cadr spec) (generate-new-buffer " *vm-edit-test*")))
     (unwind-protect
         (progn
           (setq vm-message-pointer vm-message-list)
           ;; `vm-edit-message' associates the buffer with the message; the
           ;; association is what `vm-edit-message-end' clears on success.
           (vm-set-edit-buffer-of ,(car spec) ,(cadr spec))
           (with-current-buffer ,(cadr spec)
             (insert-buffer-substring
              (vm-buffer-of ,(car spec))
              (vm-headers-of ,(car spec)) (vm-text-end-of ,(car spec)))
             (goto-char (point-min))
             (should (search-forward "the body as fetched from the server" nil t))
             (replace-match ,(nth 2 spec))
             (setq vm-message-pointer (list ,(car spec))
                   vm-mail-buffer (vm-buffer-of ,(car spec)))
             (set-buffer-modified-p t))
           (cl-letf (((symbol-function 'vm-present-current-message) #'ignore)
                     ((symbol-function 'vm-update-summary-and-mode-line)
                      #'ignore)
                     ((symbol-function 'vm-display) (lambda (&rest _) nil)))
             ,@body))
       (when (buffer-live-p ,(cadr spec)) (kill-buffer ,(cadr spec))))))

(ert-deftest vm-edit-test-end-applies-the-edit ()
  "The ordinary case: ending an edit writes the new body and flags the message.
The control for the test below -- if this stopped working, that one would pass
by leaving the message alone for the wrong reason."
  (vm-test-with-folder vm-edit-test-folder
    (vm-edit-test--with-edit-session (m edit-buf "the body as edited by hand")
      (with-current-buffer edit-buf (vm-edit-message-end))
      (should (string-match "as edited by hand" (vm-test-message-body m)))
      (should (vm-edited-flag m)))))

(ert-deftest vm-edit-test-failed-edit-leaves-the-message-unchanged ()
  "REGRESSION: an edit whose bookkeeping fails does not change the message.
Issue #307.  `vm-edit-message-end' replaced the body first and discarded the
cached data second, and the second step can signal -- the report is of
`vm-discard-cached-data-internal' raising thread-integrity errors.  The body
was already overwritten by then, and nothing could put it back:
`vm-edit-message-abort' only kills the edit buffer.  \"Even though the edit was
apparently aborted, the message body has still changed.\"

Now the write is undone when the bookkeeping fails, and the error still
reaches the user."
  (vm-test-with-folder vm-edit-test-folder
    (let ((original (vm-test-message-body (car vm-message-list))))
      (vm-edit-test--with-edit-session (m edit-buf "the body as edited by hand")
        (cl-letf (((symbol-function 'vm-discard-cached-data-internal)
                   (lambda (&rest _) (error "thread integrity problem")))
                  ((symbol-function 'vm-warn) (lambda (&rest _) nil)))
          (with-current-buffer edit-buf
            (should-error (vm-edit-message-end))))
        ;; The message is exactly as it was ...
        (should (equal original (vm-test-message-body m)))
        (should-not (string-match-p "as edited by hand"
                                    (vm-test-message-body m)))
        ;; ... and is not marked as edited, since it was not.
        (should-not (vm-edited-flag m))
        ;; The edit buffer is still there, so the user's work is not lost
        ;; and they can try again.
        (should (buffer-live-p edit-buf))
        (should (eq edit-buf (vm-edit-buffer-of m)))))))

(ert-deftest vm-edit-test-failed-edit-keeps-the-folder-parsable ()
  "After a failed edit the folder still holds one well-formed message.
The restore puts the text back the same way round it was taken out, so the
message's own markers -- and the separator before it -- have to survive."
  (vm-test-with-folder vm-edit-test-folder
    (vm-edit-test--with-edit-session (m edit-buf "the body as edited by hand")
      (cl-letf (((symbol-function 'vm-discard-cached-data-internal)
                 (lambda (&rest _) (error "thread integrity problem")))
                ((symbol-function 'vm-warn) (lambda (&rest _) nil)))
        (with-current-buffer edit-buf
          (should-error (vm-edit-message-end))))
      (should (= 1 (length vm-message-list)))
      (save-restriction
        (widen)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          ;; One From_ separator, one copy of the headers, one body.
          (should (= 1 (cl-count-if (lambda (l) (string-prefix-p "From " l))
                                    (split-string text "\n"))))
          (should (= 1 (cl-count-if
                        (lambda (l) (string-prefix-p "Subject: " l))
                        (split-string text "\n"))))))
      ;; And the markers still delimit the message they did before.
      (should (string-match-p "\\`From: sender@example.com"
                              (buffer-substring-no-properties
                               (vm-headers-of m) (vm-text-of m)))))))

;;; Editing a message and putting it back (emacs-vm/vm#673)

(defmacro vm-edit-test--in-a-folder (spec &rest body)
  "Visit a folder of messages and run BODY, with FOLDER bound to its buffer.
SPEC is (COUNT) or (COUNT TEXT): TEXT is written instead of the default
two-message folder.  The message pointer is on the first message."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-edit" t)))
          (file (expand-file-name "inbox" dir))
          (vm-folder-directory dir)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-frame-per-edit nil)
          (before (buffer-list))
          folder)
     (unwind-protect
         (progn
           (with-temp-file file
             (insert (or ,(cadr spec)
                         (mapconcat
                          (lambda (n)
                            (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                                            "From: alice@example.com\n"
                                            "Subject: msg %d\n\nbody %d\n\n")
                                    n n))
                          (number-sequence 1 ,(car spec)) ""))))
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder file)
             (setq folder (current-buffer))
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-edit-test--folder-text (folder)
  "The whole text of FOLDER, which VM keeps narrowed to one message."
  (with-current-buffer folder
    (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))

(defun vm-edit-test--messages-in (folder)
  "How many messages FOLDER holds, counted by their From_ lines."
  (let ((text (vm-edit-test--folder-text folder)) (n 0) (start 0))
    (while (string-match "^From alice@example\\.com " text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(ert-deftest vm-edit-test-an-edit-reaches-the-folder ()
  "What you type in the edit buffer is what the folder holds afterwards,
and the message is marked edited.  The messages around it are untouched."
  (vm-edit-test--in-a-folder (2)
    (vm-edit-message)
    (goto-char (point-min))
    (should (search-forward "body 1" nil t))
    (replace-match "body one, rewritten")
    (vm-edit-message-end)
    (let ((text (vm-edit-test--folder-text folder)))
      (should (string-match-p "body one, rewritten" text))
      (should-not (string-match-p "body 1$" text))
      (should (string-match-p "body 2" text)))
    (with-current-buffer folder
      (should (vm-edited-flag (car vm-message-list)))
      (should-not (vm-edited-flag (nth 1 vm-message-list))))))

(ert-deftest vm-edit-test-an-edit-can-change-the-headers ()
  "Headers are edited like anything else: the edit buffer holds the message
from its first header to its last line."
  (vm-edit-test--in-a-folder (2)
    (vm-edit-message)
    (goto-char (point-min))
    (should (search-forward "Subject: msg 1" nil t))
    (replace-match "Subject: a better subject")
    (vm-edit-message-end)
    (should (string-match-p "Subject: a better subject"
                            (vm-edit-test--folder-text folder)))))

(ert-deftest vm-edit-test-aborting-changes-nothing ()
  "`vm-edit-message-abort' leaves the folder as it was, whatever was typed,
and the message unedited."
  (vm-edit-test--in-a-folder (2)
    (let ((before (vm-edit-test--folder-text folder)))
      (vm-edit-message)
      (goto-char (point-min))
      (should (search-forward "body 1" nil t))
      (replace-match "this should not survive")
      (vm-edit-message-abort)
      (should (equal (vm-edit-test--folder-text folder) before))
      (with-current-buffer folder
        (should-not (vm-edited-flag (car vm-message-list)))))))

(ert-deftest vm-edit-test-ending-an-unchanged-edit-says-so ()
  "Ending an edit that changed nothing says so and leaves the folder alone,
rather than writing the message back over itself and marking it edited."
  (vm-edit-test--in-a-folder (2)
    (let ((before (vm-edit-test--folder-text folder))
          (said nil))
      (vm-edit-message)
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level format &rest args)
                   (setq said (apply #'format format args)))))
        (vm-edit-message-end))
      (should (equal said "No change."))
      (should (equal (vm-edit-test--folder-text folder) before))
      (with-current-buffer folder
        (should-not (vm-edited-flag (car vm-message-list)))))))

(ert-deftest vm-edit-test-a-prefix-argument-forgets-the-edit ()
  "With a prefix argument the command marks the message unedited instead of
opening an edit buffer: it is how you tell VM the message is as it came."
  (vm-edit-test--in-a-folder (2)
    (vm-edit-message)
    (goto-char (point-min))
    (should (search-forward "body 1" nil t))
    (replace-match "body one, rewritten")
    (vm-edit-message-end)
    (with-current-buffer folder
      (should (vm-edited-flag (car vm-message-list)))
      (let ((buffers (length (buffer-list))))
        (vm-edit-message t)
        (should-not (vm-edited-flag (car vm-message-list)))
        ;; and no edit buffer was made
        (should (= (length (buffer-list)) buffers))))))

(ert-deftest vm-edit-test-an-edited-message-keeps-the-folder-whole ()
  "A message that gains a line beginning \"From \" does not split the folder
in two.  The separators are munged on the way back, which is what keeps an
edit from turning one message into two."
  (vm-edit-test--in-a-folder (2)
    (should (= (vm-edit-test--messages-in folder) 2))
    (vm-edit-message)
    (goto-char (point-max))
    (insert "From alice@example.com Tue Feb  2 00:00:00 2024\n"
            "Subject: not a new message\n\n")
    (vm-edit-message-end)
    (should (= (vm-edit-test--messages-in folder) 2))
    (with-current-buffer folder
      (should (= (length vm-message-list) 2)))))

(ert-deftest vm-edit-test-an-edited-message-ends-with-a-newline ()
  "A message whose last line was left unterminated gets its newline back:
without it the next message's From_ line would not start a line."
  (vm-edit-test--in-a-folder (2)
    (vm-edit-message)
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))
    (insert "no newline at the end")
    (vm-edit-message-end)
    (let ((text (vm-edit-test--folder-text folder)))
      (should (string-match-p "no newline at the end\n" text))
      (should (= (vm-edit-test--messages-in folder) 2)))))

(ert-deftest vm-edit-test-editing-discards-what-was-cached ()
  "The byte and line counts cached for the summary are dropped, so the
summary shows the message as it is now rather than as it arrived."
  (vm-edit-test--in-a-folder (2)
    (with-current-buffer folder
      (vm-set-byte-count-of (car vm-message-list) "999")
      (vm-set-line-count-of (car vm-message-list) "999"))
    (vm-edit-message)
    (goto-char (point-min))
    (should (search-forward "body 1" nil t))
    (replace-match "a longer body than it had before")
    (vm-edit-message-end)
    (with-current-buffer folder
      ;; discarded, and recomputed if anything asks again: what matters is
      ;; that the stale value is gone
      (should-not (equal (vm-byte-count-of (car vm-message-list)) "999"))
      (should-not (equal (vm-line-count-of (car vm-message-list)) "999")))))

(ert-deftest vm-edit-test-ending-outside-an-edit-buffer-is-refused ()
  "`vm-edit-message-end' in a buffer that is not an edit buffer says so."
  (let ((text-quoting-style 'grave))
    (with-temp-buffer
      (let ((vm-message-pointer nil))
        (let ((err (should-error (vm-edit-message-end) :type 'error)))
          (should (string-match-p "not a VM message edit buffer"
                                  (error-message-string err))))
        (let ((err (should-error (vm-edit-message-abort) :type 'error)))
          (should (string-match-p "not a VM message edit buffer"
                                  (error-message-string err))))))))

(ert-deftest vm-edit-test-editing-a-read-only-folder-is-refused ()
  "A folder visited read-only is not edited: the edit would have nowhere to
go back to."
  (vm-edit-test--in-a-folder (2)
    (with-current-buffer folder
      (setq vm-folder-read-only t)
      (let ((text-quoting-style 'grave))
        (should-error (vm-edit-message) :type 'folder-read-only)))))

;;; The edit buffer itself

(ert-deftest vm-edit-test-the-edit-buffer-holds-the-whole-message ()
  "The buffer holds the message from its first header to its last line, and
knows which folder it belongs to.  `vm-edit-message-end' reads all of that
back, so a buffer set up short of it writes a truncated message."
  (vm-edit-test--in-a-folder (2)
    (vm-edit-message)
    (should (eq vm-mail-buffer folder))
    (should (eq vm-system-state 'editing))
    (should buffer-offer-save)
    (should (equal (length vm-message-pointer) 1))
    (let ((text (buffer-string)))
      (should (string-match-p "\\`From: alice@example\\.com" text))
      (should (string-match-p "Subject: msg 1" text))
      (should (string-match-p "body 1" text))
      ;; the message and not the folder: the From_ separator is not part of
      ;; what is edited, nor is the message after it
      (should-not (string-match-p "^From alice@example\\.com Mon" text))
      (should-not (string-match-p "body 2" text)))))

(ert-deftest vm-edit-test-editing-again-returns-to-the-same-buffer ()
  "A second `vm-edit-message' on a message already being edited goes back to
that buffer with what has been typed in it, rather than starting again and
losing it."
  (vm-edit-test--in-a-folder (2)
    (vm-edit-message)
    (let ((edit-buf (current-buffer)))
      (goto-char (point-max))
      (insert "a line typed but not finished\n")
      (with-current-buffer folder (vm-edit-message))
      (should (eq (current-buffer) edit-buf))
      (should (string-match-p "not finished" (buffer-string))))))

;;; Editing a message that is not the reader's to edit

(ert-deftest vm-edit-test-an-unmirrored-virtual-message-is-refused ()
  "A virtual folder that does not mirror its real folder holds copies of the
messages, and editing one would change a copy that nothing writes back.  The
command says so rather than pretending to edit."
  (vm-edit-test--in-a-folder (2)
    (let* ((file (buffer-file-name folder))
           (vm-virtual-mirror nil)
           (vm-virtual-folder-alist
            (list (list "everything" (list (list file) '(any)))))
           (text-quoting-style 'grave))
      (cl-letf (((symbol-function 'vm-display) #'ignore))
        (vm-visit-virtual-folder "everything"))
      (should (eq major-mode 'vm-virtual-mode))
      (let ((err (should-error (vm-edit-message) :type 'error)))
        (should (string-match-p "unmirrored" (error-message-string err)))))))

;;; A folder that carries its own lengths

(ert-deftest vm-edit-test-an-edit-recomputes-the-content-length ()
  "In an mboxcl2 folder the body length is a header, and an edit that changes
the body has to change it too: a Content-Length that disagrees with the body
runs one message into the next when the folder is next read."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-edit-cl2" t)))
         (file (expand-file-name "inbox" dir))
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-frame-per-edit nil)
         (vm-trust-content-length t)
         (before (buffer-list))
         folder)
    (unwind-protect
        (progn
          ;; written here rather than with vm-mboxcl2-test.el's own writer:
          ;; a test file has to stand on its own, `--one' loading it alone
          (with-temp-file file
            (dolist (message '(("one" . "the first body\n")
                               ("two" . "the second body\n")))
              (insert "From sender@example.com Sat Aug  8 14:24:13 2026\n"
                      "From: sender@example.com\n"
                      "Subject: " (car message) "\n"
                      (format "Content-Length: %d\n"
                              (string-bytes (cdr message)))
                      "\n" (cdr message))))
          (cl-letf (((symbol-function 'vm-display) #'ignore))
            (vm-visit-folder file)
            (setq folder (current-buffer))
            (setq vm-message-pointer vm-message-list)
            (should (eq (vm-message-type-of (car vm-message-list)) 'mboxcl2))
            (vm-edit-message)
            (goto-char (point-min))
            (should (search-forward "the first body" nil t))
            (replace-match "a first body that is a good deal longer")
            (vm-edit-message-end))
          (let ((text (vm-edit-test--folder-text folder)))
            ;; one length, not the old one as well
            (should (equal (length (split-string text "Content-Length:" t)) 3))
            (let ((body "a first body that is a good deal longer\n"))
              (should (string-match-p
                       (format "Content-Length: %d" (string-bytes body))
                       text))))
          ;; and the folder still reads back as two messages
          (with-current-buffer folder
            (vm-save-folder))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (how-many "^From sender@example\\.com ") 2))
            (should (equal (how-many "^Subject: two$") 1))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))


;;; Editing a separator-shaped body into a folder of every type

;; The manual lists editing among the paths VM quotes on, alongside mail
;; arriving over POP and IMAP, composing, and bursting a digest.  Everything
;; above edits a From_ or an mboxcl2 folder, and every edit it makes puts
;; ordinary prose in the body.
;;
;; What a folder cannot survive is an edit that types a separator into a
;; message.  An edit rewrites the message in place, so it is the same class of
;; in-place arithmetic as an expunge, and it is the one path where the text
;; comes from the reader rather than from a server.

(defconst vm-edit-test--decoys
  '(("a From_ line"      . "From nobody@example.com Mon Jan  1 00:00:00 2024")
    ("an mmdf separator" . "\001\001\001\001")
    ("a babyl separator" . "\037\014")
    ("8-bit text"        . "Gr\u00fc\u00dfe"))
  "Lines a reader might type into a message that a folder reads as a separator.
The last is not a separator anywhere and is here as the control: an edit must
not damage it either.  Written as characters rather than as the UTF-8 bytes,
because that is what an edit buffer holds and what reading the folder back
gives: comparing a decoded string against raw bytes fails on a folder that is
perfectly correct.")

(defun vm-edit-test--file-into (folder body)
  "File a composition with BODY into FOLDER through its Fcc header.
The coding-system variables are bound as `vm-mail-send' binds them around the
Fcc, so 8-bit text does not stop to ask what to write it in."
  (let ((coding-system-for-write (vm-binary-coding-system))
        (vm-dont-ask-coding-system-question t)
        (select-safe-coding-system-function nil))
    (with-temp-buffer
      (insert "To: someone@example.com\nSubject: filed\n"
              "Fcc: " folder "\n" mail-header-separator "\n" body)
      (vm-do-fcc-in-composition))))

(defun vm-edit-test--read-bodies (folder type)
  "The bodies of the messages FOLDER holds, read as VM reads a folder of TYPE."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert-file-contents folder)
    ;; `vm-build-message-list' re-derives the type from the buffer name.
    (setq-local buffer-file-name (vm-folder-name-for-type folder type))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (vm-build-message-list)
    (mapcar (lambda (m)
              (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))
            vm-message-list)))

(defun vm-edit-test--type-a-decoy-into (type decoy)
  "Edit DECOY into the first of two messages in a folder of TYPE.
Answers a complaint, or nil when the folder still reads back as two messages
with the edit in the first of them."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-edit-decoy" t)))
         (vm-default-folder-type type)
         (vm-folder-directory dir)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-frame-per-edit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-confirm-quit nil)
         (before (buffer-list)))
    (unwind-protect
        (condition-case err
            (let ((folder (progn
                            (vm-edit-test--file-into
                             (expand-file-name "inbox" dir) "first body\n")
                            (vm-edit-test--file-into
                             (expand-file-name "inbox" dir) "second body\n")
                            (vm-new-folder-file-name
                             (expand-file-name "inbox" dir)))))
              (cl-letf (((symbol-function 'vm-display) #'ignore))
                (vm-visit-folder folder)
                (setq vm-message-pointer vm-message-list)
                (vm-edit-message)
                (goto-char (point-min))
                (unless (search-forward "first body" nil t)
                  (error "the edit buffer does not hold the message"))
                ;; An empty line before it, because VM's From_ reader takes a
                ;; line for a separator only where one precedes it.  Without
                ;; that the decoy is not dangerous for From_ at all, and the
                ;; test passes with the quoting taken out.
                (replace-match (concat "first body\n\n" decoy))
                (vm-edit-message-end)
                (vm-save-folder))
              (let ((bodies (vm-edit-test--read-bodies folder type)))
                (cond
                 ((/= 2 (length bodies))
                  (format "%s / %s: %d message(s) after the edit, not 2: %S"
                          type decoy (length bodies) bodies))
                 ((not (string-match-p "second body" (nth 1 bodies)))
                  (format "%s / %s: the second message reads %S"
                          type decoy (nth 1 bodies)))
                 ;; The decoy may have been quoted on the way in, which is
                 ;; the point of the quoting; what must not happen is losing
                 ;; it.  Compare the last line of it, a `>' being prepended.
                 ((not (string-match-p
                        (regexp-quote (car (last (split-string decoy "\n"))))
                        (nth 0 bodies)))
                  (format "%s / %s: the edit is not in the message: %S"
                          type decoy (nth 0 bodies)))
                 (t nil))))
          (error (format "%s / %s: %s" type decoy (error-message-string err))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(defun vm-edit-test--every-decoy (type)
  "Type every decoy into a message in a folder of TYPE, and report."
  (delq nil
        (mapcar (lambda (spec)
                  (vm-edit-test--type-a-decoy-into type (cdr spec)))
                vm-edit-test--decoys)))

(ert-deftest vm-edit-test-editing-a-separator-into-a-From_-folder ()
  "A separator typed into a message leaves a From_ folder readable.
This is the one of the four that turns on the quoting: taking the
`vm-munge-message-separators' call out of `vm-edit-message-end' fails it, the
folder reading back as three messages."
  (should (equal nil (vm-edit-test--every-decoy 'From_))))

(ert-deftest vm-edit-test-editing-a-separator-into-an-mboxcl2-folder ()
  "It leaves an mboxcl2 folder readable, its byte count following the edit.
Nothing is quoted here and nothing needs to be: the byte count says where the
message ends whatever the body holds, which is what the type is for
(emacs-vm/vm#466).  So this passes with the quoting taken out, and what it is
holding is the count being recomputed to match the edit."
  (should (equal nil (vm-edit-test--every-decoy 'mboxcl2))))

(ert-deftest vm-edit-test-editing-a-separator-into-an-mmdf-folder ()
  "It leaves an mmdf folder readable.
The other one that turns on the quoting, and the stricter of the two: an mmdf
separator needs no empty line before it, so an unquoted one splits the message
wherever it falls.  Taking the quoting out fails this."
  (should (equal nil (vm-edit-test--every-decoy 'mmdf))))

(ert-deftest vm-edit-test-editing-a-separator-into-a-babyl-folder ()
  "It leaves a babyl folder readable.
Measured, so as not to claim more than it checks: an edit does not quote a
`\037\014' typed into a babyl folder, though babyl is named in
`vm-munge-message-separators', and the folder reads back as two messages
regardless.  Its reader starts a message where the last one ended rather than
searching for the next separator, so a stray one in a body reaches nothing.
So this passes with the quoting taken out, and what it holds is that an edit
does not corrupt the folder."
  (should (equal nil (vm-edit-test--every-decoy 'babyl))))

(provide 'vm-edit-test)

;;; vm-edit-test.el ends here
