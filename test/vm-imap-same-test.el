;;; vm-imap-same-test.el --- the two paths, compared -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The driver was written to do what the blocking implementation does without
;; waiting for it.  That claim is checkable: run the same sequence of commands
;; twice against identical servers, once on the driver and once with every
;; asynchronous entry point turned off, and compare what is left -- the
;; folder's messages, their UIDs and flags, what the folder still owes the
;; server, and what the server itself holds.
;;
;; A difference is a defect in one path or the other, and which one it is takes
;; an argument; that the two differ at all takes none.  This found two:
;;
;;   - the driver forgot to clear `vm-imap-retrieved-messages' for a message it
;;     had expunged on the server, which the blocking path does;
;;   - the blocking path left the expunge request itself in
;;     `vm-imap-messages-to-expunge' after the server had done it, so every
;;     later save asked again for a message the server no longer had.
;;
;; Both are fixed; this is what keeps them fixed.

;;; Code:

(require 'vm-test-init)
(require 'vm-imap-mock)
(require 'vm-imap-net)

(defvar vm-imap-same-test--messages
  (list "From: a@example.com\nSubject: one\n\nBody one.\n"
        "From: b@example.com\nSubject: two\n\nBody two.\n"
        "From: c@example.com\nSubject: three\n\nBody three.\n")
  "What the mailbox holds when a run starts.")

(defun vm-imap-same-test--state (mock)
  "What the folder and the server look like, as something comparable."
  (list
   :subjects (mapcar #'vm-su-subject vm-message-list)
   :uids (mapcar #'vm-imap-uid-of vm-message-list)
   :unread (mapcar (lambda (m) (and (vm-unread-flag m) t)) vm-message-list)
   :deleted (mapcar (lambda (m) (and (vm-deleted-flag m) t)) vm-message-list)
   :external (mapcar (lambda (m) (and (vm-body-to-be-retrieved-of m) t))
                     vm-message-list)
   :to-expunge (length vm-imap-messages-to-expunge)
   :retrieved (length vm-imap-retrieved-messages)
   :server-subjects
   (sort (mapcar (lambda (m)
                   (let ((text (vm-imap-mock-message-text m)))
                     (when (string-match "Subject: \\(.*\\)" text)
                       (match-string 1 text))))
                 (vm-imap-mock-messages mock "INBOX"))
         #'string-lessp)
   :server-flags
   (sort (mapcar (lambda (m)
                   (format "%s=%s" (vm-imap-mock-message-uid m)
                           (sort (mapcar #'downcase (vm-imap-mock-message-flags m))
                                 #'string-lessp)))
                 (vm-imap-mock-messages mock "INBOX"))
         #'string-lessp)))

(defun vm-imap-same-test--run (driven)
  "Run the same sequence with the driver on or off, and answer with the state.
DRIVEN nil turns off every asynchronous entry point, so VM does the whole
sequence the way it did before any of this was written."
  (let* ((mock (vm-imap-mock-start :messages vm-imap-same-test--messages))
         (cache (make-temp-file "vm-imap-same-cache" t))
         (vm-imap-folder-cache-directory cache)
         (vm-imap-server-timeout 10)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-imap-message-bunch-size 2)
         (vm-enable-external-messages '(imap))
         (before (buffer-list))
         (state nil)
         (off (lambda (&rest _) nil)))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-imap-net-get-spooled-mail)
                   (if driven (symbol-function 'vm-imap-net-get-spooled-mail) off))
                  ((symbol-function 'vm-imap-net-save-attributes)
                   (if driven (symbol-function 'vm-imap-net-save-attributes) off))
                  ((symbol-function 'vm-imap-net-send-changes)
                   (if driven (symbol-function 'vm-imap-net-send-changes) off))
                  ((symbol-function 'vm-imap-net-expunge-remote-messages)
                   (if driven (symbol-function 'vm-imap-net-expunge-remote-messages)
                     off))
                  ((symbol-function 'vm-imap-net-load-message-bodies)
                   (if driven (symbol-function 'vm-imap-net-load-message-bodies) off))
                  ((symbol-function 'vm-imap-net-synchronize)
                   (if driven (symbol-function 'vm-imap-net-synchronize) off)))
          (vm-visit-imap-folder (vm-imap-mock-spec mock))
          (vm-imap-net-wait nil 20)
          ;; read the first
          (vm-set-unread-flag (car vm-message-list) nil)
          (vm-set-attribute-modflag-of (car vm-message-list) t)
          ;; delete the second and expunge it here
          (vm-set-deleted-flag (nth 1 vm-message-list) t)
          (vm-expunge-folder)
          ;; save, which is what sends both to the server
          (set-buffer-modified-p t)
          (vm-save-folder)
          (vm-imap-net-wait nil 20)
          ;; new mail
          (vm-imap-mock-add-message
           mock "INBOX" "From: d@example.com\nSubject: four\n\nBody four.\n")
          (vm-get-new-mail)
          (vm-imap-net-wait nil 20)
          ;; a body that was left on the server
          (vm-unload-message 1 t)
          (vm-load-message 1)
          (vm-imap-net-wait nil 20)
          (setq state (vm-imap-same-test--state mock)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (vm-imap-mock-stop mock)
      (delete-directory cache t))
    state))

(defun vm-imap-same-test--differences (one other)
  "What differs between the two states, as a list of strings."
  (let ((differences nil))
    (while one
      (let ((key (car one)))
        (unless (equal (cadr one) (cadr other))
          (push (format "%s: driver %S, blocking %S" key (cadr one) (cadr other))
                differences))
        (setq one (cddr one)
              other (cddr other))))
    (nreverse differences)))

(ert-deftest vm-imap-same-test-both-paths-leave-the-same-folder ()
  "The driver leaves the folder and the server as the blocking path does.

Read a message, delete and expunge another, save, take in new mail, load a
body that was left on the server: twice, once each way, and compare
everything that is worth comparing afterwards."
  (let ((driven (vm-imap-same-test--run t))
        (blocking (vm-imap-same-test--run nil)))
    (should (equal nil (vm-imap-same-test--differences driven blocking)))
    ;; and the run did what it says: four messages arrived, one went
    (should (equal (plist-get driven :subjects) '("one" "three" "four")))
    (should (equal (plist-get driven :server-subjects) '("four" "one" "three")))))

(ert-deftest vm-imap-same-test-the-comparison-can-tell-them-apart ()
  "The comparison itself, against two states that differ.
A green run above says the two paths agreed; that is worth something only if
disagreement would have shown."
  (should (equal nil (vm-imap-same-test--differences '(:a 1 :b 2) '(:a 1 :b 2))))
  (should (vm-imap-same-test--differences '(:a 1 :b 2) '(:a 1 :b 3))))

(provide 'vm-imap-same-test)

;;; vm-imap-same-test.el ends here
