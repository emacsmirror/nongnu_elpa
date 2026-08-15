;;; vm-edit-test.el --- Tests for vm-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM edit functions in vm-edit.el

;;; Code:

(require 'vm-test-init)
(require 'vm-edit)

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

(provide 'vm-edit-test)

;;; vm-edit-test.el ends here
