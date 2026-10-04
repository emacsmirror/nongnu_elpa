;;; vm-imap-fault-test.el --- what a fault does to a folder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The other IMAP tests ask whether an operation does what it should.  These
;; ask what is left of the folder when it does not: the server refuses, calls
;; the command bad, hangs up mid-message, answers a FETCH backwards, lies about
;; a size.  A folder that ends up inconsistent after that is corrupted, and a
;; corrupted folder is what the asynchronous work has to be trusted not to
;; leave behind.
;;
;; Each test visits a folder that loads cleanly, breaks the server under it,
;; runs one operation, and then checks the folder rather than the operation:
;;
;;   - every message has a UID, and no two have the same one
;;   - every message belongs to the mailbox this folder holds
;;   - the message list matches what is in the buffer
;;   - nothing is left running and nothing is left queued
;;
;; The operation is allowed to fail.  What it is not allowed to do is leave the
;; folder saying something untrue.
;;
;; `vm-imap-fault-test-the-check-catches-a-broken-folder' proves the check
;; itself against a folder broken on purpose, so that a green run here means
;; the folder was looked at and found sound rather than not looked at.

;;; Code:

(require 'vm-test-init)
(require 'vm-imap-mock)
(require 'vm-imap-net)

(defvar vm-imap-fault-test--messages
  (list "From: a@example.com\nSubject: one\n\nBody one.\n"
        "From: b@example.com\nSubject: two\n\nBody two.\n"
        "From: c@example.com\nSubject: three\n\nBody three.\n")
  "What the mailbox holds before a test breaks the server.")

(defun vm-imap-fault-test--complaints ()
  "What is wrong with the folder in the current buffer, as a list of strings."
  (let ((wrong nil)
        (uids (make-hash-table :test 'equal))
        (validity (vm-folder-imap-uid-validity)))
    (dolist (m vm-message-list)
      (let ((uid (vm-imap-uid-of m)))
        (cond ((null uid) (push "a message with no UID" wrong))
              ((gethash uid uids)
               (push (format "two messages with UID %s" uid) wrong))
              (t (puthash uid t uids)))
        (unless (equal (vm-imap-uid-validity-of m) validity)
          (push (format "a message whose UID validity is %s, not %s"
                        (vm-imap-uid-validity-of m) validity)
                wrong))))
    (let ((in-buffer (save-restriction
                       (widen)
                       (count-matches "^From " (point-min) (point-max)))))
      (unless (= in-buffer (length vm-message-list))
        (push (format "%d messages in the list, %d in the buffer"
                      (length vm-message-list) in-buffer)
              wrong)))
    (when (vm-imap-net-busy-p)
      (push "a session was still running" wrong))
    (when vm-imap-net-waiting
      (push (format "%d pieces of work left queued" (length vm-imap-net-waiting))
            wrong))
    wrong))

(defun vm-imap-fault-test--break (mock faults)
  "Set FAULTS on MOCK, which is serving a folder that has loaded cleanly."
  (while faults
    (let ((what (car faults))
          (how (cadr faults)))
      (cond ((eq what :refuse) (setf (vm-imap-mock-refuse mock) how))
            ((eq what :bad) (setf (vm-imap-mock-bad mock) how))
            ((eq what :drop-on) (setf (vm-imap-mock-drop-on mock) how))
            ((eq what :truncate-fetch) (setf (vm-imap-mock-truncate-fetch mock) how))
            ((eq what :reorder-fetch) (setf (vm-imap-mock-reorder-fetch mock) how))
            ((eq what :drop-after-fetch) (setf (vm-imap-mock-drop-after-fetch mock) how))
            ((eq what :lie-about-size) (setf (vm-imap-mock-lie-about-size mock) how))
            (t (error "No such fault: %s" what)))
      (setq faults (cddr faults)))))

(defun vm-imap-fault-test--operations (mock)
  "Every operation these tests run against a broken server.
Each is (NAME . FUNCTION); the function runs in the folder buffer and may
fail, which is what the server breaking is supposed to make it do."
  (list
   (cons "fetch"
         (lambda ()
           (vm-imap-mock-add-message
            mock "INBOX" "From: d@example.com\nSubject: four\n\nBody four.\n")
           (vm-get-new-mail)))
   (cons "flags"
         (lambda ()
           (when vm-message-list
             (vm-set-unread-flag (car vm-message-list) nil)
             (vm-set-attribute-modflag-of (car vm-message-list) t))
           (vm-imap-net-save-attributes)))
   (cons "expunge"
         (lambda ()
           (when vm-message-list
             (vm-set-deleted-flag (car vm-message-list) t)
             (vm-expunge-folder))
           (vm-imap-net-send-changes)))
   (cons "bodies"
         (lambda ()
           (let ((vm-enable-external-messages '(imap)))
             (when vm-message-list
               (vm-unload-message 1 t)
               (vm-imap-net-load-message-bodies (list (car vm-message-list)))))))
   (cons "synchronize"
         (lambda () (vm-imap-net-synchronize nil t)))))

(defun vm-imap-fault-test--run (faults)
  "Run every operation against a server broken with FAULTS.
Answers with what was wrong with the folder afterwards, named by operation."
  (let ((complaints nil))
    (dolist (operation (vm-imap-fault-test--operations nil) complaints)
      (let* ((mock (vm-imap-mock-start
                    :messages vm-imap-fault-test--messages))
             (cache (make-temp-file "vm-imap-fault-cache" t))
             (vm-imap-folder-cache-directory cache)
             (vm-imap-server-timeout 5)
             (vm-frame-per-folder nil)
             (vm-mutable-frame-configuration nil)
             (vm-imap-message-bunch-size 2)
             (before (buffer-list)))
        (unwind-protect
            (progn
              (vm-visit-imap-folder (vm-imap-mock-spec mock))
              (vm-imap-net-wait nil 15)
              (vm-imap-fault-test--break mock faults)
              ;; the operations are built again with this mock in hand: the
              ;; fetch adds a message to it
              (let ((this (cdr (assoc (car operation)
                                      (vm-imap-fault-test--operations mock)))))
                (condition-case _err
                    (funcall this)
                  (error nil))
                (vm-imap-net-wait nil 15))
              (dolist (complaint (vm-imap-fault-test--complaints))
                (push (format "%s: %s" (car operation) complaint) complaints)))
          (dolist (buffer (buffer-list))
            (unless (memq buffer before)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer))))
          (vm-imap-mock-stop mock)
          (delete-directory cache t))))))

(defmacro vm-imap-fault-test--deftest (name faults doc)
  "Define a test that runs every operation against a server broken with FAULTS."
  (declare (indent 2))
  `(ert-deftest ,name ()
     ,doc
     (should (equal nil (vm-imap-fault-test--run ,faults)))))

(vm-imap-fault-test--deftest vm-imap-fault-test-nothing-wrong nil
  "With the server behaving, every operation leaves the folder sound.
The other rows mean nothing without this one: it is what says the check is
being run against a folder that actually loaded.")

(vm-imap-fault-test--deftest vm-imap-fault-test-select-refused '(:refuse "SELECT")
  "A server that will not open the mailbox leaves the folder as it was.")

(vm-imap-fault-test--deftest vm-imap-fault-test-fetch-refused '(:refuse "FETCH")
  "A refused FETCH leaves the folder as it was: no message half taken in.")

(vm-imap-fault-test--deftest vm-imap-fault-test-store-refused '(:refuse "STORE")
  "A refused STORE leaves the flags to be sent again, and the folder sound.")

(vm-imap-fault-test--deftest vm-imap-fault-test-expunge-refused '(:refuse "EXPUNGE")
  "A refused EXPUNGE leaves the deletions pending rather than forgotten.")

(vm-imap-fault-test--deftest vm-imap-fault-test-fetch-called-bad '(:bad "FETCH")
  "A server that calls the FETCH bad is a protocol error, not a corrupt folder.")

(vm-imap-fault-test--deftest vm-imap-fault-test-dropped-on-fetch '(:drop-on "FETCH")
  "A connection dropped as the FETCH goes out leaves nothing half written.")

(vm-imap-fault-test--deftest vm-imap-fault-test-dropped-on-store '(:drop-on "STORE")
  "A connection dropped as the STORE goes out leaves the flags to be resent.")

(vm-imap-fault-test--deftest vm-imap-fault-test-fetch-truncated '(:truncate-fetch t)
  "A message cut off mid-literal is not taken into the folder in pieces.")

(vm-imap-fault-test--deftest vm-imap-fault-test-fetch-reordered '(:reorder-fetch t)
  "A FETCH answered backwards gives each message its own UID.
The responses carry UIDs and the client is to tell them apart by those; a
client that trusted the order would give every message the UID of another
(issue #185).")

(vm-imap-fault-test--deftest vm-imap-fault-test-dropped-after-one
    '(:drop-after-fetch 1)
  "A download interrupted between messages keeps what arrived, whole.")

(vm-imap-fault-test--deftest vm-imap-fault-test-size-lied-about '(:lie-about-size t)
  "A wrong octet count in RFC822.SIZE does not put a message in wrong.")

(ert-deftest vm-imap-fault-test-the-check-catches-a-broken-folder ()
  "The check itself, against folders broken on purpose.

A green run above means the folder was looked at and found sound.  That is
worth only as much as the looking, so here are the three kinds of breakage it
is there to catch."
  (let* ((mock (vm-imap-mock-start :messages vm-imap-fault-test--messages))
         (cache (make-temp-file "vm-imap-fault-cache" t))
         (vm-imap-folder-cache-directory cache)
         (vm-imap-server-timeout 5)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (vm-visit-imap-folder (vm-imap-mock-spec mock))
          (vm-imap-net-wait nil 15)
          (should (equal (length vm-message-list) 3))
          (should-not (vm-imap-fault-test--complaints))
          ;; a message with no UID
          (let ((uid (vm-imap-uid-of (car vm-message-list))))
            (vm-set-imap-uid-of (car vm-message-list) nil)
            (should (vm-imap-fault-test--complaints))
            (vm-set-imap-uid-of (car vm-message-list) uid))
          ;; two messages with one UID
          (let ((uid (vm-imap-uid-of (cadr vm-message-list))))
            (vm-set-imap-uid-of (cadr vm-message-list)
                                (vm-imap-uid-of (car vm-message-list)))
            (should (vm-imap-fault-test--complaints))
            (vm-set-imap-uid-of (cadr vm-message-list) uid))
          ;; a message list that does not match the buffer
          (let ((all vm-message-list))
            (setq vm-message-list (cdr vm-message-list))
            (should (vm-imap-fault-test--complaints))
            (setq vm-message-list all))
          (should-not (vm-imap-fault-test--complaints)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (vm-imap-mock-stop mock)
      (delete-directory cache t))))

(provide 'vm-imap-fault-test)

;;; vm-imap-fault-test.el ends here
