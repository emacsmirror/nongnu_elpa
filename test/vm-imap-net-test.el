;;; vm-imap-net-test.el --- IMAP over the non-blocking driver -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The IMAP reader written as generators, run against test/vm-imap-mock.el --
;; a real IMAP server on a local port, so what is tested is a session on a
;; socket rather than a string handed to a parser.
;;
;; The tokens are the ones vm-imap.el's own parser produces, positions in the
;; process buffer and all, so `vm-imap-response-matches' is what the tests
;; assert with, as the client code does.
;;
;; The tests wait for a session to finish.  That is the test waiting: the
;; session runs in the process filter, which is what would be happening
;; between a user's keystrokes.

;;; Code:

(require 'vm-test-init)
(require 'vm-imap-mock)
(require 'vm-imap-net)
(require 'vm-imap)
(require 'vm-net)

(defconst vm-imap-net-test--alice
  "From: alice@example.com\nTo: me@example.com\nSubject: badgers\n\nThe first body.\n"
  "A message for a mock mailbox.")

(defconst vm-imap-net-test--bob
  "From: bob@example.com\nTo: me@example.com\nSubject: otters\n\nThe second body.\n"
  "Another, so a test can tell one from the next.")

(defvar vm-imap-net-test--buffer nil
  "The process buffer of the session a test is running, for reading tokens.")

(defun vm-imap-net-test--run (mock iterator &optional timeout wait)
  "Run ITERATOR as an IMAP session against MOCK and answer with the session.
TIMEOUT is the session's own, WAIT how long this waits for it to finish."
  (let* ((buffer (generate-new-buffer " *vm-imap-net-test*"))
         (process (make-network-process
                   :name "vm-imap-net-test" :host 'local
                   :service (vm-imap-mock-port mock)
                   :buffer buffer :noquery t :coding 'binary))
         (session (vm-net-session :process process :name "imap"
                                  :timeout (or timeout 5))))
    (with-current-buffer buffer (vm-imap-net-init))
    (setq vm-imap-net-test--buffer buffer)
    (vm-net-start session iterator)
    (let ((deadline (+ (float-time) (or wait timeout 5))))
      (while (and (vm-net-session-live-p session) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (when (process-live-p process) (delete-process process))
    session))

(defmacro vm-imap-net-test--with-session (spec &rest body)
  "Run BODY with a mock server bound, and kill the session buffer after.
SPEC is (MOCK-VAR &rest ARGS) as for `vm-imap-mock-start'."
  (declare (indent 1) (debug t))
  `(let ((vm-imap-net-test--buffer nil))
     (unwind-protect
         (vm-imap-mock-with ,spec ,@body)
       (when (buffer-live-p vm-imap-net-test--buffer)
         (kill-buffer vm-imap-net-test--buffer)))))

(defun vm-imap-net-test--matches (response &rest pattern)
  "Whether RESPONSE matches PATTERN, read in the session's own buffer.
The tokens are positions in that buffer, so they mean nothing anywhere else."
  (with-current-buffer vm-imap-net-test--buffer
    (apply #'vm-imap-response-matches response pattern)))

(defun vm-imap-net-test--text (token)
  "The text TOKEN covers."
  (with-current-buffer vm-imap-net-test--buffer
    (buffer-substring-no-properties (nth 1 token) (nth 2 token))))

;;; Reading a session

(iter-defun vm-imap-net-test--login (user password)
  (iter-yield-from (vm-imap-net-open-session user password)))

(ert-deftest vm-imap-net-test-a-session-greets-and-logs-in ()
  "The greeting, CAPABILITY and LOGIN, none of them waiting: the whole
exchange happens in the process filter."
  (vm-imap-net-test--with-session (mock)
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login "vmtest" "secret"))))
      (should (eq (vm-net-session-state session) 'done))
      (let ((capabilities (car (vm-net-session-value session))))
        (should (memq 'IMAP4REV1 capabilities))
        (should (memq 'UIDPLUS capabilities)))
      (should (vm-imap-mock-received-p mock "LOGIN")))))

(ert-deftest vm-imap-net-test-a-refused-login-signals ()
  "A tagged NO ends the session with the server's own words, rather than
leaving the caller to work out that nothing happened."
  (vm-imap-net-test--with-session (mock :refuse "LOGIN")
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login "vmtest" "secret"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (string-match-p "server says"
                              (error-message-string
                               (vm-net-session-error session)))))))

(ert-deftest vm-imap-net-test-the-password-is-not-in-the-transcript ()
  "The session buffer is a transcript, and LOGIN's arguments are left out of
it.  The buffer is kept as the trace `vm-imap-submit-bug-report' sends."
  (vm-imap-net-test--with-session (mock)
    (vm-imap-net-test--run mock (vm-imap-net-test--login "vmtest" "secret"))
    (with-current-buffer vm-imap-net-test--buffer
      (should (string-match-p "LOGIN <parameters omitted>" (buffer-string)))
      (should-not (string-match-p "secret" (buffer-string))))))

;;; The tokens

(iter-defun vm-imap-net-test--select (user password mailbox)
  (iter-yield-from (vm-imap-net-open-session user password))
  (iter-yield-from (vm-imap-net-command
                    (format "SELECT \"%s\"" mailbox) "SELECT")))

(ert-deftest vm-imap-net-test-select-comes-back-as-tokens ()
  "SELECT's untagged lines arrive as the tokens vm-imap.el's parser makes,
so `vm-imap-response-matches' reads them without knowing which reader
produced them."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice
                                                        vm-imap-net-test--bob))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--select "vmtest" "secret" "INBOX")))
           (lines (vm-net-session-value session)))
      (should (eq (vm-net-session-state session) 'done))
      ;; * 2 EXISTS
      (should (cl-some (lambda (line)
                         (vm-imap-net-test--matches line '* 2 'EXISTS))
                       lines))
      ;; * OK [UIDVALIDITY n]
      (should (cl-some (lambda (line)
                         (vm-imap-net-test--matches
                          line '* 'OK '(vector UIDVALIDITY atom)))
                       lines))
      ;; and the last line is the tagged one
      (should (vm-imap-net-test--matches (car (last lines)) 'VM 'OK)))))

(ert-deftest vm-imap-net-test-a-literal-arrives-whole ()
  "A {n} literal is read by its octet count.  A message body is one, and an
off-by-one here is a message with a line missing or one glued to the next."
  (let ((long (concat "From: alice@example.com\nSubject: long\n\n"
                      (mapconcat (lambda (n) (format "line %d" n))
                                 (number-sequence 1 500) "\n")
                      "\n")))
    (vm-imap-net-test--with-session (mock :messages (list long))
      (let* ((session (vm-imap-net-test--run
                       mock
                       (vm-imap-net-test--fetch "vmtest" "secret" "INBOX"
                                                "1 (RFC822)")))
             (lines (vm-net-session-value session))
             (fetch (car lines)))
        (should (eq (vm-net-session-state session) 'done))
        (should (vm-imap-net-test--matches fetch '* 1 'FETCH 'list))
        (let* ((contents (cdr (nth 3 fetch)))
               (text (vm-imap-net-test--text (nth 1 contents))))
          (should (string-match-p "^line 1$" text))
          (should (string-match-p "^line 500$" text))
          ;; every octet the server said it would send, and no more: the
          ;; count in the {n} is what says where the literal ends
          (should (equal (length text) (length long))))))))

(iter-defun vm-imap-net-test--fetch (user password mailbox spec)
  (iter-yield-from (vm-imap-net-open-session user password))
  (iter-yield-from (vm-imap-net-command (format "SELECT \"%s\"" mailbox)))
  (iter-yield-from (vm-imap-net-command (format "FETCH %s" spec) "FETCH")))

(ert-deftest vm-imap-net-test-a-quoted-string-is-a-string-token ()
  "A quoted string comes back as a string token covering what was between
the quotes, which is how a flag list and a mailbox name arrive."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--list "vmtest" "secret")))
           (lines (vm-net-session-value session))
           (listing (car lines)))
      (should (eq (vm-net-session-state session) 'done))
      (should (vm-imap-net-test--matches listing '* 'LIST 'list 'string 'string))
      (should (equal (vm-imap-net-test--text (nth 4 listing)) "INBOX")))))

(iter-defun vm-imap-net-test--list (user password)
  (iter-yield-from (vm-imap-net-open-session user password))
  (iter-yield-from (vm-imap-net-command "LIST \"\" \"*\"" "LIST")))

;;; What the driver does when the server does not play along

(ert-deftest vm-imap-net-test-a-dropped-connection-ends-the-session ()
  "The server hanging up mid-command ends the session rather than leaving a
generator waiting for input that cannot arrive."
  (vm-imap-net-test--with-session (mock :drop-on "SELECT")
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--select "vmtest" "secret" "INBOX"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (vm-net-session-error session)))))

(ert-deftest vm-imap-net-test-a-truncated-literal-ends-the-session ()
  "A FETCH cut off mid-literal is a session that fails, not one that hangs:
the octet count says how much is coming and the connection says it is not."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice)
                                        :truncate-fetch t)
    (let ((session (vm-imap-net-test--run
                    mock
                    (vm-imap-net-test--fetch "vmtest" "secret" "INBOX"
                                             "1 (RFC822)")
                    2 4)))
      (should (eq (vm-net-session-state session) 'failed)))))

(ert-deftest vm-imap-net-test-a-silent-server-times-the-session-out ()
  "A server that accepts the connection and says nothing ends the session
when the timeout runs out, rather than waiting for ever."
  (vm-imap-net-test--with-session (mock :slow-greeting 2)
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login "vmtest" "secret") 0.5 4)))
      (should (eq (vm-net-session-state session) 'failed))
      (should (string-match-p "timed out"
                              (error-message-string
                               (vm-net-session-error session)))))))

;;; Tags

(ert-deftest vm-imap-net-test-each-command-has-a-tag-of-its-own ()
  "Every command is numbered, and `VM' in a pattern means the tag of the
command being waited for -- so a late response to an earlier command is not
read as the answer to this one."
  (vm-imap-net-test--with-session (mock)
    (vm-imap-net-test--run mock (vm-imap-net-test--select "vmtest" "secret"
                                                          "INBOX"))
    (let ((commands (vm-imap-mock-commands mock)))
      (should (equal (length commands) 4))
      (should (equal (mapcar (lambda (c) (car (split-string c " "))) commands)
                     '("vm1" "vm2" "vm3" "vm4"))))))


;;; A mailbox, and what is in it

(iter-defun vm-imap-net-test--open-and-select (mailbox &optional examine)
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (iter-yield-from (vm-imap-net-select mailbox examine)))

(ert-deftest vm-imap-net-test-select-says-what-the-mailbox-holds ()
  "SELECT answers the count, the UIDVALIDITY and whether the mailbox can be
written to, which is what the folder needs before it can ask for anything."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice
                                                        vm-imap-net-test--bob))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--open-and-select "INBOX")))
           (answer (vm-net-session-value session)))
      (should (eq (vm-net-session-state session) 'done))
      (should (equal (nth 0 answer) 2))
      (should (stringp (nth 2 answer)))
      (should (nth 3 answer))
      (should (nth 4 answer)))))

(ert-deftest vm-imap-net-test-examine-is-read-only ()
  "EXAMINE says the mailbox is not writable, which is what stops VM from
trying to store flags into one it opened only to read."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--open-and-select "INBOX" t)))
           (answer (vm-net-session-value session)))
      (should (eq (vm-net-session-state session) 'done))
      (should-not (nth 3 answer))
      (should (vm-imap-mock-received-p mock "EXAMINE")))))

(iter-defun vm-imap-net-test--data (mailbox)
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (iter-yield-from (vm-imap-net-select mailbox))
  (iter-yield-from (vm-imap-net-message-data 1 2)))

(ert-deftest vm-imap-net-test-the-bulk-fetch-brings-uid-size-and-flags ()
  "One command for the whole mailbox: the UIDs to know what is new, the
sizes to know what is too large, the flags to know what is read."
  (vm-imap-net-test--with-session (mock :messages
                                        (list (cons vm-imap-net-test--alice
                                                    '("\\Seen"))
                                              vm-imap-net-test--bob))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--data "INBOX")))
           (data (vm-net-session-value session)))
      (should (eq (vm-net-session-state session) 'done))
      (should (equal (length data) 2))
      (let ((first (assoc 1 data)))
        (should (equal (nth 1 first) "1"))
        (should (equal (nth 2 first)
                       (number-to-string (length vm-imap-net-test--alice))))
        (should (member "\\seen" (nthcdr 3 first))))
      (should-not (nthcdr 3 (assoc 2 data))))))

;;; Fetching

(defvar vm-imap-net-test--stored nil
  "What the store function was handed, newest last.")

(defun vm-imap-net-test--store (uid start end)
  (push (cons uid (buffer-substring-no-properties start end))
        vm-imap-net-test--stored))

(iter-defun vm-imap-net-test--fetch-range (mailbox first last)
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (iter-yield-from (vm-imap-net-select mailbox))
  (iter-yield-from (vm-imap-net-fetch first last t nil
                                      #'vm-imap-net-test--store)))

(ert-deftest vm-imap-net-test-a-fetched-message-is-handed-over-as-it-arrives ()
  "Each message goes to the store function as its response is read, with the
UID it came with -- a server may answer a range in any order, and the copy
has to know which message it is looking at (issue #185)."
  (let ((vm-imap-net-test--stored nil))
    (vm-imap-net-test--with-session (mock :messages
                                          (list vm-imap-net-test--alice
                                                vm-imap-net-test--bob))
      (let ((session (vm-imap-net-test--run
                      mock (vm-imap-net-test--fetch-range "INBOX" 1 2))))
        (should (eq (vm-net-session-state session) 'done))
        (should (equal (vm-net-session-value session) 2))
        (let ((stored (nreverse vm-imap-net-test--stored)))
          (should (equal (mapcar #'car stored) '("1" "2")))
          (should (string-match-p "badgers" (cdr (nth 0 stored))))
          (should (string-match-p "The second body" (cdr (nth 1 stored)))))))))

;; A wait must outlast the session's own timeout.  `vm-imap-net-wait' answers
;; nil when its deadline passes with the session still running, so a test that
;; waits exactly as long as `vm-imap-server-timeout' is asking which of two
;; deadlines fires first.  Two tests here did; the POP side is where it was caught
;; (emacs-vm/vm#863).
;;
;; The wait is twice the timeout here.  A session that finishes still finishes
;; in milliseconds; one that hangs is now reported by the assertion after the
;; wait, which says what was not deleted, rather than by the wait itself, which
;; says only that ten seconds passed.

;; A timeout here must outlast a garbage collection.  The suite's heap makes a
;; collection expensive -- 26 of them in one run, 34 seconds between them, and
;; one that took 11 seconds on its own -- and a pause that long inside a session
;; is longer than a ten second timeout, so the session gave up and the test
;; failed (emacs-vm/vm#863).  Emacs is stopped for the whole pause, so nothing
;; the test or the driver does can see it coming.
;;
;; The mock answers in milliseconds; these timeouts exist to stop a hang, not to
;; measure anything, so they cost nothing by being generous.  The wait is longer
;; again than the session's own timeout, so a session that does hang is reported
;; by the assertion that says what was not done rather than by a bare deadline.

(ert-deftest vm-imap-net-test-headers-only-fetches-headers ()
  "A message too large to want whole is fetched as its headers, and what
comes back is the headers and not the body."
  (let ((vm-imap-net-test--stored nil))
    (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
      (vm-imap-net-test--run
       mock
       (let ((store #'vm-imap-net-test--store))
         (vm-imap-net-test--fetch-headers "INBOX" store)))
      (let ((text (cdr (car vm-imap-net-test--stored))))
        (should (string-match-p "Subject: badgers" text))
        (should-not (string-match-p "The first body" text))))))

(iter-defun vm-imap-net-test--fetch-headers (mailbox store)
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (iter-yield-from (vm-imap-net-select mailbox))
  (iter-yield-from (vm-imap-net-fetch 1 1 t t store)))


;;; Into a folder

(defmacro vm-imap-net-test--visiting (spec &rest body)
  "Visit a mock IMAP folder with the blocking code and run BODY in it.
The folder is set up as VM sets one up -- access data, cache file and all --
so what BODY exercises is the asynchronous path against a real folder rather
than against a buffer a test invented."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cdr spec))
     (let* ((cache (make-temp-file "vm-imap-net-cache" t))
            (vm-imap-folder-cache-directory cache)
            (vm-imap-server-timeout 60)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (before (buffer-list)))
       (unwind-protect
           (progn (vm-visit-imap-folder (vm-imap-mock-spec ,(car spec)))
                  ;; visiting starts the fetch and returns without waiting for
                  ;; it, so what waits for the mail is whoever wants the mail
                  (vm-imap-net-wait nil 20)
                  ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory cache t)))))

(defun vm-imap-net-test--get-mail (mock &optional seconds full)
  "Fetch new mail into the current folder and answer with what came back.
FULL asks for the messages the folder was given once and no longer holds."
  (let ((answer 'not-called)
        (folder (current-buffer)))
    (vm-imap-net-get-mail (vm-imap-mock-spec mock)
                          (lambda (result) (setq answer result))
                          nil full)
    (let ((deadline (+ (float-time) (or seconds 10))))
      (while (and (eq answer 'not-called) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (with-current-buffer folder answer)))

(ert-deftest vm-imap-net-test-new-mail-lands-in-the-folder ()
  "A folder that was empty gets what arrived after it was visited, without
anything waiting for it: the whole fetch happens in the process filter."
  (vm-imap-net-test--visiting (mock)
    (should (null vm-message-list))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (should (equal (vm-imap-net-test--get-mail mock) 2))
    (should (equal (length vm-message-list) 2))
    (should (equal (vm-su-from (car vm-message-list)) "alice@example.com"))
    (should (string-match-p "The second body"
                            (vm-imap-net-test--body-of (cadr vm-message-list))))))

(defun vm-imap-net-test--body-of (message)
  "The text of MESSAGE as it sits in the folder buffer."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (vm-text-of message)
                                    (vm-text-end-of message))))

(ert-deftest vm-imap-net-test-a-message-already-here-is-not-fetched-again ()
  "The UID of every message the folder holds is compared with the server\\='s,
and only what is missing is asked for -- so a second fetch sends no FETCH of
a body at all."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should (equal (length vm-message-list) 1))
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (equal (length vm-message-list) 1))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (should (equal (vm-imap-net-test--get-mail mock) 1))
    (should (equal (length vm-message-list) 2))))

(ert-deftest vm-imap-net-test-the-uid-and-flags-are-kept-with-the-message ()
  "A fetched message carries the UID it was fetched under and the flags the
server reported, which is what stops it being fetched again and what makes
it show as read."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice '("\\Seen"))
    (should (equal (vm-imap-net-test--get-mail mock) 1))
    (let ((message (car vm-message-list)))
      (should (equal (vm-imap-uid-of message) "1"))
      (should (stringp (vm-imap-uid-validity-of message)))
      (should-not (vm-unread-flag message)))))

(ert-deftest vm-imap-net-test-a-fetch-of-many-goes-in-bunches ()
  "More messages than `vm-imap-message-bunch-size' are asked for a bunch at
a time, and every one of them arrives."
  (let ((vm-imap-message-bunch-size 4))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 10)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: sender%d@example.com\nSubject: number %d\n\nBody %d.\n"
                 i i i)))
      (should (equal (vm-imap-net-test--get-mail mock) 10))
      (should (equal (length vm-message-list) 10))
      (should (equal (cl-count-if (lambda (c) (string-match-p "BODY\\.PEEK" c))
                                  (vm-imap-mock-commands mock))
                     3)))))

(ert-deftest vm-imap-net-test-the-folder-has-a-current-message-during-the-fetch ()
  "As soon as the first bunch is in, the folder has a current message.

The folder is usable while the rest of the fetch runs, and a folder with
messages in its list and nothing in `vm-message-pointer' is not: every command
that works on the current message takes `(car vm-message-pointer)' and gets
nil.  Typing a space during the first fetch into an empty folder was
\"vm-scroll-forward: Wrong type argument: arrayp, nil\"."
  (let ((vm-imap-message-bunch-size 2)
        (seen nil))
    (vm-imap-net-test--visiting (mock)
      (should (null vm-message-list))
      (dotimes (i 8)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: sender%d@example.com\nSubject: number %d\n\nBody %d.\n"
                 i i i)))
      (let ((answer 'not-called)
            (folder (current-buffer)))
        (vm-imap-net-get-mail (vm-imap-mock-spec mock)
                              (lambda (result) (setq answer result)))
        (let ((deadline (+ (float-time) 10)))
          (while (and (eq answer 'not-called) (< (float-time) deadline))
            (accept-process-output nil 0.02)
            ;; what the reader would find if they typed now
            (with-current-buffer folder
              (when vm-message-list
                (push (and vm-message-pointer t) seen)))))
        (should (equal answer 8)))
      ;; looked at least once with messages in the folder, and never found it
      ;; without a current message
      (should seen)
      (should-not (memq nil seen)))))

(ert-deftest vm-imap-net-test-the-fetch-says-it-began-and-not-each-bunch ()
  "The fetch says it began, and keeps the per-bunch count out of the way.

The reader is using Emacs while it runs -- that is the point of it running in
the background -- so a line per bunch in the echo area is in the way of them.
The count goes to the mode line, live, and to the log at level 6.  What is
shown is that the fetch began, and afterwards what arrived."
  (let ((vm-imap-message-bunch-size 2)
        (vm-verbosity 5)                ; the default
        (said nil))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 6)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: sender%d@example.com\nSubject: number %d\n\nBody %d.\n"
                 i i i)))
      (let ((inform (symbol-function 'vm-inform)))
        (cl-letf (((symbol-function 'vm-inform)
                   (lambda (level &rest args)
                     (push (cons level (apply #'format-message args)) said)
                     (apply inform level args))))
          (should (equal (vm-imap-net-test--get-mail mock) 6))))
      (let ((shown (mapcar #'cdr
                           (seq-filter (lambda (line) (<= (car line) vm-verbosity))
                                       said))))
        ;; the long silent phase before any message arrives is worth saying
        (should (seq-find (lambda (text) (string-match-p "reading the list" text))
                          shown))
        ;; the bunches are not
        (should-not (seq-find (lambda (text)
                                (string-match-p "of 6 messages retrieved" text))
                              shown))
        ;; and they are still recorded, for a fetch that has to be explained
        (should (seq-find (lambda (line)
                            (and (= (car line) 6)
                                 (string-match-p "6 of 6 messages retrieved"
                                                 (cdr line))))
                          said))))))

(ert-deftest vm-imap-net-test-a-failed-fetch-tells-the-caller ()
  "A server that refuses the fetch ends the session and the folder hears
about it, rather than the callback never coming."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (setf (vm-imap-mock-refuse mock) "FETCH")
    (let ((result (vm-imap-net-test--get-mail mock)))
      (should (consp result))
      (should (string-match-p "server says" (error-message-string result)))
      (should (null vm-message-list)))))


;;; Flags, going up

(ert-deftest vm-imap-net-test-a-changed-flag-goes-to-the-server ()
  "Marking a message read in the folder stores \\Seen on the server, in the
same session as the fetch and before the server\\='s own flags are read -- or
what was just fetched would be written back over the change."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should (equal (length vm-message-list) 1))
    (let ((message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t))
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (vm-imap-mock-received-p mock "STORE 1 \\+FLAGS"))
    ;; flag names are case-insensitive in IMAP, and what VM sends for this
    ;; one is lower case
    (should (member "\\seen" (mapcar #'downcase
                                     (vm-imap-mock-flags mock "INBOX" 1))))))

(ert-deftest vm-imap-net-test-a-flag-the-server-refuses-is-not-lost ()
  "A STORE the server says NO to leaves the message with its modification
flag set, so the next synchronisation offers it again, and the fetch carries
on regardless."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t)
      (setf (vm-imap-mock-refuse mock) "STORE")
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
      (should (equal (vm-imap-net-test--get-mail mock) 1))
      (should (vm-attribute-modflag-of message))
      (should (equal (length vm-message-list) 2)))))

(ert-deftest vm-imap-net-test-saving-sends-the-changes-without-waiting ()
  "Saving an IMAP folder sends the flags and the expunges and returns.

The blocking save also worked out what the server had expunged, which means a
FETCH of the flags of every message in the mailbox: nineteen seconds and a
still Emacs on a folder of six thousand, on every quit.  What it sends is what
the reader changed; what the server did is the next fetch's business."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (let ((message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t))
    (vm-imap-mock-forget-commands mock)
    (set-buffer-modified-p t)
    (vm-save-folder)
    ;; started, and not finished: the save did not wait for the server
    (should (vm-imap-net-busy-p))
    (should (vm-imap-net-wait nil 90))
    (should (vm-imap-mock-received-p mock "STORE 1 \\+FLAGS"))
    (should (member "\\seen" (mapcar #'downcase
                                     (vm-imap-mock-flags mock "INBOX" 1))))
    ;; and nothing asked what the mailbox holds
    (should-not (vm-imap-mock-received-p mock "FETCH 1:2"))))

(ert-deftest vm-imap-net-test-saving-expunges-on-the-server ()
  "The deletions a folder is holding go up in the same session as the flags."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (let ((message (car vm-message-list)))
      (vm-set-deleted-flag message t)
      (vm-expunge-folder))
    (should (equal (length vm-message-list) 1))
    (vm-imap-mock-forget-commands mock)
    (vm-save-folder)
    ;; sent by a session the save did not wait for
    (should (vm-imap-net-busy-p))
    (should (vm-imap-net-wait nil 90))
    (should (vm-imap-mock-received-p mock "UID STORE"))
    (should (vm-imap-mock-received-p mock "EXPUNGE"))
    ;; and nothing downloaded the flags of the mailbox to get there
    (should-not (vm-imap-mock-received-p mock "FETCH 1:"))
    ;; and the folder has stopped holding them
    (should-not vm-imap-messages-to-expunge)))

(ert-deftest vm-imap-net-test-a-save-during-a-fetch-starts-no-second-session ()
  "Saving while a fetch is running leaves the changes for next time.

Two sessions writing into one mailbox would interleave, and the blocking path
is not the answer either: a reader who quit during a fetch of six thousand
messages waited out a second login and a second download of every flag."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    ;; a fetch under way, not waited for, recorded as the folder's session the
    ;; way `vm-imap-net-get-spooled-mail' records it
    (setq vm-imap-net-session
          (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
    (should (vm-imap-net-busy-p))
    (let ((session vm-imap-net-session))
      (should (eq (vm-imap-net-send-changes) 'later))
      ;; the same session, and no other
      (should (eq vm-imap-net-session session))
      (should (vm-imap-net-wait nil 90)))
    ;; and nothing was lost: the running fetch sends the folder's flags at its
    ;; start, so this one went up in that session rather than a second
    (should (member "\\seen" (mapcar #'downcase
                                     (vm-imap-mock-flags mock "INBOX" 1))))))

(ert-deftest vm-imap-net-test-a-second-operation-waits-for-the-first ()
  "Work asked for while a session is running is done when it ends.

Two sessions writing one folder would interleave their messages, flags and
expunges in one buffer and one cache file.  So the second waits, and it waits
in the folder rather than being turned away to the blocking path, which would
open the connection this is avoiding."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (setq vm-imap-net-session
          (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
    (should (vm-imap-net-busy-p))
    (let ((fetch vm-imap-net-session))
      ;; queued, not refused and not started
      (should (eq (vm-imap-net-send-changes) 'later))
      (should (eq vm-imap-net-session fetch))
      (should (equal (length vm-imap-net-waiting) 1))
      ;; and it runs when the fetch is done
      (should (vm-imap-net-wait nil 90))
      (should-not vm-imap-net-waiting)
      (should (vm-imap-net-wait nil 90))
      (should (member "\\seen" (mapcar #'downcase
                                       (vm-imap-mock-flags mock "INBOX" 1)))))))

(ert-deftest vm-imap-net-test-bodies-asked-for-during-a-fetch-wait-too ()
  "Loading a body while new mail is arriving does not open a second session.

`vm-imap-net-load-bodies' had no such check and overwrote the folder's
session slot, so two sessions wrote into one folder and only the second was
tracked."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (vm-unload-message 1 t)
      (should (vm-body-to-be-retrieved-of message))
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
      (setq vm-imap-net-session
            (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
      (let ((fetch vm-imap-net-session))
        (should (eq (vm-imap-net-load-message-bodies (list message)) 'later))
        (should (eq vm-imap-net-session fetch)))
      (should (vm-imap-net-wait nil 90))
      (should (vm-imap-net-wait nil 90))
      ;; the body came, in a session of its own, after the fetch
      (should-not (vm-body-to-be-retrieved-of message))
      (should (string-match-p "The first body"
                              (vm-imap-net-test--body-of message))))))

(iter-defun vm-imap-net-test--never-finishes ()
  "A session that waits for something that never arrives."
  (iter-yield (lambda () nil))
  'never)

(ert-deftest vm-imap-net-test-a-queued-body-for-a-gone-message-is-dropped ()
  "A body queued for a message the folder no longer has is not asked for.

What waits behind a session can wait behind one that expunges: the message it
was to fill is gone by the time it runs, and the answer would have nowhere to
be put."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (vm-unload-message 1 t)
      (setq vm-imap-net-session
            (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
      (should (eq (vm-imap-net-load-message-bodies (list message)) 'later))
      ;; expunged while the fetch it is waiting behind runs
      (vm-set-deleted-flag message t)
      (vm-expunge-folder :quiet t :just-these-messages (list message))
      (should (vm-imap-net-wait nil 90))
      ;; it ran, asked for nothing, and did not fail the session
      (vm-imap-net-wait nil 90)
      (should-not vm-imap-net-waiting))))

(ert-deftest vm-imap-net-test-a-quit-during-the-save-loses-nothing ()
  "The folder buffer being killed while the session runs is not an error.

Quitting writes the file and kills the buffer at once; the session outlives
it.  A message whose flags did not reach the server still has its modification
flag in the file, so the next visit sends them again."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((folder (current-buffer))
          (message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t)
      (vm-imap-mock-forget-commands mock)
      (set-buffer-modified-p t)
      (vm-save-folder)
      (let ((session vm-imap-net-session))
        ;; the session is still running when the buffer goes, which is what
        ;; this is about
        (should (vm-net-session-live-p session))
        (set-buffer-modified-p nil)
        (kill-buffer folder)
        (let ((deadline (+ (float-time) 10)))
          (while (and (vm-net-session-live-p session) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        ;; it finished rather than erroring, and said what it had to say
        (should (eq (vm-net-session-state session) 'done))))))

(ert-deftest vm-imap-net-test-synchronizing-goes-both-ways-without-waiting ()
  "`vm-imap-synchronize' on the driver: flags up, flags down, mail in.

The blocking version fetched the flags of every message in the mailbox with
Emacs held still, which on a folder of six thousand was half a minute."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should (equal (length vm-message-list) 1))
    (let ((message (car vm-message-list)))
      ;; the folder marked one read, and the server has marked another flag on
      ;; the same message behind VM's back
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob '("\\Answered"))
    (vm-imap-mock-forget-commands mock)
    (should (eq (vm-imap-net-synchronize nil t) t))
    ;; started, not finished
    (should (vm-imap-net-busy-p))
    (should (vm-imap-net-wait nil 90))
    ;; the folder's own flag went up
    (should (member "\\seen" (mapcar #'downcase
                                     (vm-imap-mock-flags mock "INBOX" 1))))
    ;; what arrived came in
    (should (equal (length vm-message-list) 2))
    ;; and the server's flags came down: the new one is answered here too
    (should (vm-replied-flag (nth 1 vm-message-list)))))

(ert-deftest vm-imap-net-test-synchronizing-takes-the-servers-flags ()
  "A flag set on the server reaches a message the folder already had.
This is the `retrieve-attributes' half, which a plain fetch does not do."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((message (car vm-message-list)))
      (should-not (vm-replied-flag message))
      ;; somebody else answered it, in another client
      (vm-imap-mock-set-flags mock "INBOX" 1 '("\\Answered"))
      (should (eq (vm-imap-net-synchronize nil t) t))
      (should (vm-imap-net-wait nil 90))
      (should (vm-replied-flag message)))))

(ert-deftest vm-imap-net-test-a-full-synchronize-sends-every-flag ()
  "A full synchronisation looks at every message's flags, not only at those
marked as changed.

The case it is for: a flag that differs from the server's and whose
modification flag has been lost, which nothing else would ever send."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (let ((message (car vm-message-list)))
      (vm-set-unread-flag message nil)
      ;; read here, and nothing says so: a plain synchronisation would pass
      ;; over this message
      (vm-set-attribute-modflag-of message nil))
    (vm-imap-mock-forget-commands mock)
    (should (eq (vm-imap-net-synchronize t t) t))
    (should (vm-imap-net-wait nil 90))
    (should (member "\\seen" (mapcar #'downcase
                                     (vm-imap-mock-flags mock "INBOX" 1))))))

(ert-deftest vm-imap-net-test-a-full-synchronize-keeps-what-the-cache-lacks ()
  "A full synchronisation does not delete mail the cache merely does not hold.

It used to: every UID the mailbox had and the cache had not was deleted on the
server, with no confirmation and no floor.  A cache truncated, restored from a
partial backup or read as the wrong type says exactly what a reader who
expunged says, and puts the whole mailbox on that list (emacs-vm/vm#752).

Server deletions come from `vm-imap-messages-to-expunge', which
`vm-expunge-folder' fills as the reader expunges; here it is emptied first, so
what the folder has lost is all the synchronise could go on."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (let ((message (nth 1 vm-message-list)))
      (vm-set-deleted-flag message t)
      (vm-expunge-folder :quiet t :just-these-messages (list message)))
    (should (equal (length vm-message-list) 1))
    (setq vm-imap-messages-to-expunge nil)   ; as if it had never been recorded
    (vm-imap-mock-forget-commands mock)
    (should (eq (vm-imap-net-synchronize t t) t))
    (should (vm-imap-net-wait nil 90))
    ;; both are still on the server, and nothing was even marked for deletion
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
    (should-not (vm-imap-mock-received-p mock "EXPUNGE"))
    ;; and it is not fetched back either: the folder asked for a full
    ;; synchronise, not a full retrieve
    (should (equal (length vm-message-list) 1))))

(ert-deftest vm-imap-net-test-a-mailbox-is-created-without-waiting ()
  "`vm-create-imap-folder' sends CREATE through the driver and returns.
One command to a server has no more business freezing Emacs than a fetch has."
  (vm-imap-mock-with (mock)
    (let* ((spec (vm-imap-mock-spec mock "Archive"))
           (vm-imap-server-timeout 60)
           (vm-imap-account-folder-cache nil)
           (before (buffer-list)))
      (unwind-protect
          (progn
            (should (vm-imap-net-mailbox-command
                     spec "CREATE \"Archive\"" "CREATE" "made it"))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not (vm-imap-mock-received-p mock "LOGOUT"))
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (vm-imap-mock-received-p mock "CREATE \"Archive\"")))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-a-mailbox-command-forgets-the-folder-cache ()
  "The account's folder list is forgotten when the server has done it, and not
before: it is the answer that makes the cache wrong."
  (vm-imap-mock-with (mock)
    (let* ((spec (vm-imap-mock-spec mock "Archive"))
           (account (vm-imap-account-name-for-spec spec))
           (vm-imap-server-timeout 60)
           (vm-imap-account-folder-cache (list (cons account '("INBOX"))))
           (before (buffer-list)))
      (unwind-protect
          (progn
            (should (vm-imap-net-mailbox-command
                     spec "CREATE \"Archive\"" "CREATE" "made it"))
            ;; still there while the command is in flight
            (should (assoc account vm-imap-account-folder-cache))
            (let ((deadline (+ (float-time) 10)))
              (while (and (assoc account vm-imap-account-folder-cache)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should-not (assoc account vm-imap-account-folder-cache)))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-a-mailbox-command-vm-cannot-send-says-so ()
  "A maildrop the driver cannot open answers nil, so the command still goes
the blocking way rather than silently not being sent."
  (let ((vm-imap-passwords nil)
        (auth-sources nil))
    ;; nobody to ask: the reader is not there, and a test that let VM ask
    ;; would sit in `read-passwd' until the runner gave up on it
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _) (error "no reader here"))))
      (should-not (vm-imap-net-mailbox-command
                   "imap:host:143:INBOX:login:someone:*" "CREATE \"x\""
                   "CREATE" "made it")))))

(ert-deftest vm-imap-net-test-listing-folders-does-not-wait ()
  "The account's mailboxes and their counts arrive without Emacs waiting.
A listing is a command per mailbox, so it is the slowest thing VM asks a
server for and the one worst spent frozen."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (vm-imap-mock-add-message mock "Archive" vm-imap-net-test--bob)
    (let* ((spec (vm-imap-mock-spec mock))
           (vm-imap-server-timeout 60)
           (answer 'not-called)
           (before (buffer-list)))
      (unwind-protect
          (progn
            (should (vm-imap-net-list-folders
                     spec (lambda (result) (setq answer result))))
            ;; asked, not answered
            (should (eq answer 'not-called))
            (let ((deadline (+ (float-time) 10)))
              (while (and (eq answer 'not-called) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (listp answer))
            (should (equal (sort (mapcar #'car answer) #'string-lessp)
                           '("Archive" "INBOX")))
            ;; the counts came from STATUS, one mailbox at a time
            (should (equal (nth 1 (assoc "INBOX" answer)) 1))
            (should (equal (nth 1 (assoc "Archive" answer)) 1))
            (should (numberp (nth 2 (assoc "INBOX" answer)))))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-a-mailbox-that-will-not-say-does-not-stop-a-listing ()
  "A STATUS the server refuses leaves that mailbox at zero, and the rest of
the listing still arrives.  Servers refuse STATUS on names they have just
listed often enough for this to matter."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice)
                           :refuse "STATUS")
    (let* ((spec (vm-imap-mock-spec mock))
           (vm-imap-server-timeout 60)
           (answer 'not-called)
           (before (buffer-list)))
      (unwind-protect
          (progn
            (should (vm-imap-net-list-folders
                     spec (lambda (result) (setq answer result))))
            (let ((deadline (+ (float-time) 10)))
              (while (and (eq answer 'not-called) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (listp answer))
            (should (equal (mapcar #'car answer) '("INBOX")))
            (should (equal (cdr (assoc "INBOX" answer)) '(0 0))))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-asking-what-a-mailbox-holds-does-not-wait ()
  "The UIDs a mailbox still holds arrive without Emacs waiting, and the
mailbox is examined rather than selected: asking is not reading."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice
                                           vm-imap-net-test--bob))
    (let* ((spec (vm-imap-mock-spec mock))
           (vm-imap-server-timeout 60)
           (answer 'not-called)
           (before (buffer-list)))
      (unwind-protect
          (progn
            (should (vm-imap-net-mailbox-uids
                     spec (lambda (result) (setq answer result))))
            (should (eq answer 'not-called))
            (let ((deadline (+ (float-time) 10)))
              (while (and (eq answer 'not-called) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (stringp (car answer)))          ; the UIDVALIDITY
            ;; order is the message data's, newest first; what the callers
            ;; want is the set
            (should (equal (sort (copy-sequence (cadr answer)) #'string-lessp)
                           '("1" "2")))
            (should (vm-imap-mock-received-p mock "EXAMINE"))
            (should-not (vm-imap-mock-received-p mock "\\`vm[0-9]+ SELECT")))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-completion-gets-its-names-from-the-driver ()
  "The names completion needs come back on a session of the driver's.

Completion has to answer with the names it has, so this is the one path that
waits on purpose.  What it must not do is open a connection of its own through
the blocking implementation."
  (vm-imap-mock-with (mock)
    (vm-imap-mock-add-message mock "Archive" vm-imap-net-test--alice)
    (let* ((spec (vm-imap-mock-spec mock))
           (vm-imap-server-timeout 60)
           (blocking nil)
           (before (buffer-list)))
      (unwind-protect
          (cl-letf (((symbol-function 'vm-imap-make-session)
                     (lambda (&rest _) (setq blocking t) nil)))
            (let ((names (vm-imap-net-mailbox-names spec nil 10)))
              (should (equal (sort names #'string-lessp) '("Archive" "INBOX")))
              (should-not blocking)))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-completion-answers-nothing-when-it-cannot-ask ()
  "A maildrop the driver cannot open answers with no names, so completion
still has the blocking path to fall back on."
  (let ((vm-imap-passwords nil)
        (auth-sources nil))
    (should-not (vm-imap-net-mailbox-names
                 "imap:host:143:INBOX:login:someone:*" nil 1))))

(ert-deftest vm-imap-net-test-the-mode-line-says-what-the-folder-is-doing ()
  "Every buffer showing the folder says when it is talking to a server.

A reader looking at the summary is looking at a folder that is being written
into, and the folder buffer may not be on screen at all.  What is queued is
counted with it."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should-not vm-ml-session)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (setq vm-imap-net-session
          (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
    (should (stringp vm-ml-session))
    ;; what is happening, not which protocol is doing it, and faced so that it
    ;; is not read as part of the folder's name
    (should (string-match-p "fetching" vm-ml-session))
    (should-not (string-match-p "IMAP" vm-ml-session))
    (should (eq (get-text-property 1 'face vm-ml-session) 'vm-net-session-face))
    ;; the summary says the same, without being the folder
    (when (and vm-summary-buffer (buffer-live-p vm-summary-buffer))
      (with-current-buffer vm-summary-buffer
        (should (equal vm-ml-session
                       (with-current-buffer vm-mail-buffer vm-ml-session)))))
    ;; and what waits behind it is counted
    (should (eq (vm-imap-net-send-changes) 'later))
    (should (string-match-p "\\+1" vm-ml-session))
    (should (vm-imap-net-wait nil 90))
    (should-not vm-ml-session)))

(ert-deftest vm-imap-net-test-the-mode-line-counts-what-has-arrived ()
  "The mode line says how far a fetch has got, not only that one is running.

\"fetching\" on a mailbox of six thousand says nothing about whether it is
getting anywhere.  The count each bunch reports goes to the echo area too, but
the next message wipes that; this stays until the next bunch moves it on."
  (let ((vm-imap-message-bunch-size 2)
        (seen nil))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 7)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: s%d@example.com\nSubject: m%d\n\nBody.\n" i i)))
      (let ((real (symbol-function 'vm-imap-net-assimilate)))
        (cl-letf (((symbol-function 'vm-imap-net-assimilate)
                   (lambda (&rest args)
                     (let ((answer (apply real args)))
                       (push (substring-no-properties (or vm-ml-session "")) seen)
                       answer))))
          (should (equal (vm-imap-net-test--get-mail mock) 7))))
      ;; a count that moves, and the total it is working towards
      (setq seen (nreverse seen))
      (should (equal (car seen) " fetching 0/7 "))
      (should (member " fetching 4/7 " seen))
      (should (equal (car (last seen)) " fetching 6/7 "))
      ;; and nothing left in the mode line once it is done
      (should-not vm-ml-session)
      ;; the summary was told the same thing, rather than keeping an older one
      (when (and vm-summary-buffer (buffer-live-p vm-summary-buffer))
        (with-current-buffer vm-summary-buffer
          (should (equal vm-ml-session
                         (with-current-buffer vm-mail-buffer vm-ml-session))))))))

(ert-deftest vm-imap-net-test-the-mode-line-says-which-phase-it-is-in ()
  "A fetch is several things in a row, and the mode line says which.
Before a message arrives the server has to be asked what it holds -- one
response per message, which on a mailbox of thousands is the long wait -- and
saying \"fetching\" through that says the wrong thing about it as well as
saying nothing about progress."
  (let ((seen nil))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 3)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: s%d@example.com\nSubject: m%d\n\nBody.\n" i i)))
      (let ((real (symbol-function 'vm-imap-net-note-progress)))
        (cl-letf (((symbol-function 'vm-imap-net-note-progress)
                   (lambda (&rest args)
                     (apply real args)
                     (with-current-buffer (car args)
                       (push (substring-no-properties (or vm-ml-session "")) seen)))))
          (should (equal (vm-imap-net-test--get-mail mock) 3))))
      (setq seen (nreverse seen))
      ;; the listing comes first, with the mailbox count as its total
      (should (equal (car seen) " listing 0/3 "))
      ;; and the fetch itself puts the session's own word back
      (should (member " fetching 0/3 " seen))
      (should-not (seq-find (lambda (s) (string-match-p "listing" s))
                            (cdr (member " fetching 0/3 " seen)))))))

(ert-deftest vm-imap-net-test-quitting-stops-what-the-folder-was-doing ()
  "Quitting a folder stops its session rather than leaving it writing.

The buffer is about to go, and a session that went on writing into it would be
writing into nothing.  Nothing is lost that is not still on the server: what
was fetched and not saved is fetched again next time."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (setq vm-imap-net-session
          (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
    (let ((session vm-imap-net-session))
      (should (vm-net-session-live-p session))
      ;; queue something behind it, which goes with it
      (should (eq (vm-imap-net-send-changes) 'later))
      (vm-imap-net-stop)
      (should-not (vm-net-session-live-p session))
      (should-not vm-imap-net-session)
      (should-not vm-imap-net-waiting)
      (should-not vm-ml-session)
      ;; and it said goodbye rather than being dropped: the generator's
      ;; unwind forms ran, which is where the LOGOUT is
      (let ((deadline (+ (float-time) 5)))
        (while (and (not (vm-imap-mock-received-p mock "LOGOUT"))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (vm-imap-mock-received-p mock "LOGOUT")))))

(ert-deftest vm-imap-net-test-a-second-session-is-refused-not-tolerated ()
  "Starting a second session on a folder is an error, not a race.

The queue is what keeps it from happening; this is what says so if a path is
ever added that does not go through the queue.  Two sessions writing one
folder is how a folder gets two sets of messages, flags and expunges in one
buffer and one cache file, and that is not something to find out about from a
corrupted cache."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (setq vm-imap-net-session
          (vm-imap-net-get-mail (vm-imap-mock-spec mock) #'ignore))
    (should (vm-imap-net-busy-p))
    (should-error (vm-imap-net-take-session (vm-net-session :name "second"))
                  :type 'error)
    (should (vm-imap-net-wait nil 90))))

(ert-deftest vm-imap-net-test-messages-from-elsewhere-stop-the-pairing ()
  "A folder that gains a message from somewhere else mid-fetch is not paired
up wrongly.

Each message taken in is given the UID of the entry beside it.  A message that
arrived from anywhere but this fetch would shift that pairing and give every
message after it the UID of another -- the folder would look right and be
wrong.  It signals instead."
  (vm-imap-net-test--visiting (mock)
    (let ((validity (vm-folder-imap-uid-validity)))
      ;; two written, one entry to pair them with
      (should-error (vm-imap-net-assimilate (list (list "1" 1 nil)) validity)
                    :type 'vm-imap-protocol-error))))

(ert-deftest vm-imap-net-test-a-uid-nobody-asked-for-stops-the-fetch ()
  "A UID the plan does not know is refused rather than written into a message."
  (should-error (vm-imap-net-entries-written '("7") '(("1" 1 nil)))
                :type 'vm-imap-protocol-error))

(ert-deftest vm-imap-net-test-a-changed-uid-validity-stops-the-write ()
  "The mailbox a message came from must still be the mailbox the folder holds."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should-error (vm-imap-net-assimilate nil "not-the-validity")
                  :type 'vm-imap-protocol-error)))

(ert-deftest vm-imap-net-test-a-connection-never-made-leaves-no-buffer ()
  "A session buffer goes with a connection that was never made.

A host that does not resolve, an stunnel that is not installed, a preauth hook
that answers with nothing: the buffer was made before the connection was
tried, and one was left behind for every attempt."
  (let ((vm-imap-passwords (list (list "imap-ssl:nowhere.invalid:993:*:login:me:*"
                                       "secret")))
        (vm-stunnel-program "no-such-stunnel-program")
        (before (buffer-list)))
    (should-error (vm-imap-net-open
                   "imap-ssl:nowhere.invalid:993:INBOX:login:me:*" "leak test"))
    (should-not (seq-filter (lambda (buffer)
                              (and (not (memq buffer before))
                                   (string-match-p "leak test"
                                                   (buffer-name buffer))))
                            (buffer-list)))))

(ert-deftest vm-imap-net-test-a-callback-that-fails-does-not-vanish ()
  "An error out of the caller's own callback is reported, not lost.

The session ends in a process filter, where Emacs prints \"error in process
filter\" and leaves the reader to guess whose it was."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((spec (vm-imap-mock-spec mock))
           (vm-imap-server-timeout 60)
           (warned nil)
           (before (buffer-list)))
      (unwind-protect
          ;; every warning, not only the last: VM says other things while a
          ;; session runs, and which of them comes last is not the point
          (cl-letf (((symbol-function 'vm-warn)
                     (lambda (_level _seconds &rest args)
                       (push (apply #'format args) warned))))
            (should (vm-imap-net-run-command
                     spec "NOOP" "NOOP" nil
                     (lambda (_result) (error "the callback went wrong"))))
            (let ((deadline (+ (float-time) 10))
                  (wanted (lambda ()
                            (seq-find (lambda (line)
                                        (string-match-p "the callback went wrong"
                                                        line))
                                      warned))))
              (while (and (not (funcall wanted)) (< (float-time) deadline))
                (accept-process-output nil 0.05))
              (should (funcall wanted))))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

;;; What the server no longer has

(ert-deftest vm-imap-net-test-a-message-gone-from-the-server-goes-locally ()
  "A message expunged on the server is expunged from the folder, which is
what keeps the two the same view of the mailbox."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    ;; take the first one off the server, as another client would
    (let ((message (car (vm-imap-mock-messages mock "INBOX"))))
      (setf (vm-imap-mock-message-expunged message) t))
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (equal (length vm-message-list) 1))
    (should (string-match-p "otters"
                            (vm-su-subject (car vm-message-list))))))


;;; Through the folder's own command

(ert-deftest vm-imap-net-test-get-new-mail-goes-through-the-driver ()
  "`vm-get-new-mail' on an IMAP folder starts the session and returns.  What
it returns is that it started, not what arrived: the messages land while
Emacs carries on, which is the point of the whole conversion."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (should (vm-imap-net-get-spooled-mail))
    ;; not here yet: nothing waited for it
    (should (null vm-message-list))
    (should (vm-imap-net-busy-p))
    (should (vm-imap-net-wait nil 90))
    (should (equal (length vm-message-list) 1))))

(ert-deftest vm-imap-net-test-an-unsupported-maildrop-is-left-to-the-old-path ()
  "A maildrop this cannot open without waiting answers nil, which is the
caller\\='s cue to use the blocking implementation rather than to fail."
  (vm-imap-net-test--visiting (mock)
    (cl-letf (((symbol-function 'vm-folder-imap-maildrop-spec)
               (lambda () "imap-ssh:host:143:INBOX:login:someone:*")))
      (should-not (vm-imap-net-get-spooled-mail)))))


;;; Bodies kept on the server

(ert-deftest vm-imap-net-test-a-body-comes-back-without-waiting ()
  "`vm-load-message' starts the fetch and returns; the body arrives in the
filter and the message has it then.  One UID FETCH for however many bodies
were asked for."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (let ((vm-enable-external-messages '(imap))
          (messages vm-message-list))
      (vm-unload-message 2 t)
      (should (vm-body-to-be-retrieved-of (car messages)))
      (should (vm-body-to-be-retrieved-of (cadr messages)))
      (should (vm-imap-net-load-message-bodies messages))
      (should (vm-imap-net-busy-p))
      (should (vm-imap-net-wait nil 90))
      (should-not (vm-body-to-be-retrieved-of (car messages)))
      (should (string-match-p "The first body"
                              (vm-imap-net-test--body-of (car messages))))
      (should (string-match-p "The second body"
                              (vm-imap-net-test--body-of (cadr messages))))
      ;; one command for both of them
      (should (equal (cl-count-if (lambda (c) (string-match-p "UID FETCH" c))
                                  (vm-imap-mock-commands mock))
                     1)))))

(ert-deftest vm-imap-net-test-load-message-goes-through-the-driver ()
  "The command `vm-load-message' itself takes the same path, so a body that
takes a minute to arrive does not stop Emacs for a minute."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (vm-unload-message 1 t)
      (should (equal (vm-imap-net-test--body-of message) ""))
      (vm-load-message 1)
      (should (vm-imap-net-wait nil 90))
      (should (string-match-p "The first body"
                              (vm-imap-net-test--body-of message))))))


(ert-deftest vm-imap-net-test-a-composition-is-filed-without-waiting ()
  "An Fcc to an IMAP mailbox goes through the driver.

Sending a message used to stop Emacs while a copy of it was appended to a
server.  There is no folder here to borrow a session from, so the append has
one of its own and nobody waits for it."
  (vm-imap-mock-with (mock)
    (let* ((spec (vm-imap-mock-spec mock))
           (vm-imap-server-timeout 60)
           (text "From: me@example.com\r\nSubject: filed\r\n\r\nA copy.\r\n")
           (before (buffer-list)))
      (unwind-protect
          (progn
            (should (vm-imap-net-append-text spec "INBOX" text))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not (vm-imap-mock-received-p mock "LOGOUT"))
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (vm-imap-mock-received-p mock "APPEND"))
            (should (equal (length (vm-imap-mock-messages mock "INBOX")) 1)))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-a-composition-vm-cannot-file-is-left-to-the-old-path ()
  "A maildrop the driver cannot open answers nil, so the caller still files it.
Nothing is silently not filed: the message has been sent by then."
  (let ((vm-imap-passwords nil)
        (auth-sources nil))
    (should-not (vm-imap-net-append-text
                 "imap:host:143:INBOX:login:someone:*" "INBOX" "text"))))

;;; Expunging on the server

(ert-deftest vm-imap-net-test-a-local-expunge-reaches-the-server ()
  "A message expunged from the folder is deleted and expunged on the server
in the next session, by UID -- a sequence number means something different
after every expunge."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (let ((message (car vm-message-list)))
      (vm-set-deleted-flag message t)
      (vm-expunge-folder))
    (should (equal (length vm-message-list) 1))
    (should vm-imap-messages-to-expunge)
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (vm-imap-mock-received-p mock "UID STORE"))
    (should (vm-imap-mock-received-p mock "EXPUNGE"))
    (should-not vm-imap-messages-to-expunge)
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 1))))


;;; Saving into a mailbox

(ert-deftest vm-imap-net-test-a-message-is-appended-to-a-mailbox ()
  "Saving to an IMAP mailbox APPENDs the message, flags and all, and makes
the mailbox if the server has not got one."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((answer 'not-called))
      (vm-imap-net-save-messages (vm-imap-mock-spec mock) "Archive"
                                 vm-message-list
                                 (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (equal answer 1))
      (should (vm-imap-mock-received-p mock "CREATE"))
      (should (vm-imap-mock-received-p mock "APPEND"))
      (let ((saved (vm-imap-mock-messages mock "Archive")))
        (should (equal (length saved) 1))
        (should (string-match-p
                 "badgers"
                 (vm-imap-mock-message-text (car saved))))))))


(ert-deftest vm-imap-net-test-an-appended-message-keeps-its-flagged-attribute ()
  "REGRESSION: a message saved to a mailbox keeps its flags and its labels.

`vm-imap-net-message-flags' sent \\Answered and \\Seen and nothing else, so
saving a message to an IMAP folder dropped the flagged attribute and every
label on the way (emacs-vm/vm#828).  Asserts on the flags the server was
given, which is the thing that was wrong: the message arrived either way.

The label travels here because the mock advertises `\\*' in its
PERMANENTFLAGS, which is a server saying it keeps keywords of its own."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((message (car vm-message-list))
          (answer 'not-called))
      (vm-set-flagged-flag message t)
      (vm-set-replied-flag message t)
      (vm-set-unread-flag message nil)
      (vm-add-message-labels "urgent" 1)
      (vm-imap-net-save-messages (vm-imap-mock-spec mock) "Archive"
                                 (list message)
                                 (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (equal answer 1)))
    (let ((saved (car (vm-imap-mock-messages mock "Archive"))))
      (should saved)
      (let ((flags (vm-imap-mock-message-flags saved)))
        (should (member "\\Flagged" flags))
        (should (member "\\Answered" flags))
        (should (member "\\Seen" flags))
        ;; never \Deleted: a copy is not saved in order to be deleted
        (should-not (member "\\Deleted" flags))
        ;; and the label, which the mailbox says it keeps
        (should (member "urgent" flags))))))

(ert-deftest vm-imap-net-test-a-mailbox-that-keeps-no-keywords-gets-none ()
  "REGRESSION: the copy arrives whole where the destination refuses keywords.
RFC 3501 has a server answer NO to an APPEND naming a flag it does not
support, and a refused APPEND loses the copy rather than the flag.  So what
the mailbox says it keeps is asked before anything is sent, and a keyword it
did not name is left out (emacs-vm/vm#828).

The mock without `\\*' in its PERMANENTFLAGS is what Gmail answers."
  (let ((vm-imap-mock-permanent-flags
         "\\Answered \\Flagged \\Deleted \\Seen \\Draft"))
    (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
      (let ((message (car vm-message-list))
            (answer 'not-called)
            said)
        (vm-set-flagged-flag message t)
        (vm-set-unread-flag message nil)
        (vm-add-message-labels "urgent" 1)
        (setq said
              (vm-imap-net-test--warnings
                (vm-imap-net-save-messages (vm-imap-mock-spec mock) "Archive"
                                           (list message)
                                           (lambda (result) (setq answer result)))
                (let ((deadline (+ (float-time) 10)))
                  (while (and (eq answer 'not-called) (< (float-time) deadline))
                    (accept-process-output nil 0.05)))))
        ;; the copy went, which is the thing a refused APPEND would have cost
        (should (equal answer 1))
        ;; and it says what did not go with it
        (should (seq-find (lambda (line)
                            (and (string-match-p "urgent" line)
                                 (string-match-p "PERMANENTFLAGS" line)))
                          said)))
      (let ((saved (car (vm-imap-mock-messages mock "Archive"))))
        (should saved)
        (let ((flags (vm-imap-mock-message-flags saved)))
          (should (member "\\Flagged" flags))
          (should (member "\\Seen" flags))
          (should-not (member "urgent" flags)))))))

(ert-deftest vm-imap-net-test-the-attributes-that-travel-as-keywords ()
  "`filed', `written', `forwarded' and `redistributed' go as keywords.
They are keywords on the sync path, so a saved copy carries what a
synchronised message carries (emacs-vm/vm#828)."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((message (car vm-message-list))
          (answer 'not-called))
      (vm-set-filed-flag message t)
      (vm-set-forwarded-flag message t)
      (vm-imap-net-save-messages (vm-imap-mock-spec mock) "Archive"
                                 (list message)
                                 (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (equal answer 1)))
    (let ((flags (vm-imap-mock-message-flags
                  (car (vm-imap-mock-messages mock "Archive")))))
      (should (member "filed" flags))
      (should (member "forwarded" flags))
      (should-not (member "written" flags))
      (should-not (member "redistributed" flags)))))

(defun vm-imap-net-test--until (predicate seconds)
  "Wait up to SECONDS for PREDICATE, answering with what it last said."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest vm-imap-net-test-the-mail-indicator-clears-when-the-mail-goes ()
  "REGRESSION: the Mail indicator goes off when the server no longer has mail.

Reported by @diekhans: it stayed on for an IMAP folder with no new mail
(emacs-vm/vm#839).  `vm-check-mail-itimer-function' did not re-check a folder
whose `vm-spooled-mail-waiting' was set, which is sound for a local spool,
where mail stays until something takes it, and wrong for a server, where the
mail stops being new without VM doing anything: read on a phone, moved by a
filter, taken by another Emacs.  Only a retrieval cleared the flag.

The mail is taken off the server here rather than fetched, which is the case
that used to stick: a fetch clears the flag on its own."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-mail-check-interval 60)
          (vm-mail-check-always nil))
      (vm-imap-mock-add-message
       mock "INBOX" "From: bob@example.com\nSubject: new\n\nBody.\n")
      (vm-imap-net-folder-check-mail)
      (vm-imap-net-test--until (lambda () vm-spooled-mail-waiting) 10)
      (should vm-spooled-mail-waiting)
      ;; and now it is gone from the server, with nothing VM did
      (setf (cdr (assoc "INBOX" (vm-imap-mock-mailboxes mock)))
            (list (car (vm-imap-mock-messages mock "INBOX"))))
      ;; The timer is cancelled and the watchdog let go afterwards.  This
      ;; leaked both, and an armed `vm-net--watch' is what
      ;; vm-net-test-a-poll-nobody-made-is-made-by-the-watchdog asserts on,
      ;; so it failed a thousand tests later in the same Emacs.
      (let ((timer (run-at-time nil nil #'ignore)))
        (unwind-protect
            (vm-check-mail-itimer-function timer)
          (cancel-timer timer)))
      (vm-imap-net-test--until (lambda () (null vm-spooled-mail-waiting)) 10)
      (should-not vm-spooled-mail-waiting)
      (vm-net--watch))))

;;; What only happens because nothing waits
;;
;; A session runs between whatever else Emacs is doing, so the things that go
;; wrong are the things that cannot go wrong when a command holds the floor:
;; a second command started while the first is still running, the folder
;; going away underneath, a message list that changed since the plan was
;; made, an answer arriving in pieces.

(ert-deftest vm-imap-net-test-a-second-fetch-does-not-start-on-top-of-the-first ()
  "Two fetches writing into one folder would interleave their messages.
The mail check runs from a timer, so this is not hypothetical: it fires
while the fetch a command started is still running."
  (vm-imap-net-test--visiting (mock)
    (dotimes (i 6)
      (vm-imap-mock-add-message
       mock "INBOX" (format "From: s%d@example.com\nSubject: m%d\n\nBody %d.\n" i i i)))
    (should (vm-imap-net-get-spooled-mail))
    (should (vm-imap-net-busy-p))
    (let ((session vm-imap-net-session))
      ;; the second one is turned away, and the first is still the folder's
      (should (vm-imap-net-get-spooled-mail))
      (should (eq session vm-imap-net-session)))
    (should (vm-imap-net-wait nil 90))
    (should (equal (length vm-message-list) 6))
    ;; two logins: the visit's own fetch and this one.  Three would mean the
    ;; second command opened a session of its own alongside the first.
    (should (equal (cl-count-if (lambda (c) (string-match-p "LOGIN" c))
                                (vm-imap-mock-commands mock))
                   2))))

(ert-deftest vm-imap-net-test-a-folder-closed-mid-session-stops-it ()
  "A folder can be quit while its own session is running.  The session stops
with something that says so, rather than writing into a dead buffer or
erroring somewhere further in where the cause is no longer visible."
  (let ((mock (vm-imap-mock-start))
        (cache (make-temp-file "vm-imap-net-cache" t)))
    (unwind-protect
        (let* ((vm-imap-folder-cache-directory cache)
               (vm-imap-server-timeout 60)
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (before (buffer-list))
               (session nil))
          (dotimes (i 4)
            (vm-imap-mock-add-message
             mock "INBOX" (format "From: s%d@example.com\nSubject: m%d\n\nBody.\n" i i)))
          (vm-visit-imap-folder (vm-imap-mock-spec mock))
          (setq session vm-imap-net-session)
          (should (vm-net-session-live-p session))
          (with-current-buffer (vm-net-session-buffer session)
            (should (get-buffer-process (current-buffer))))
          ;; the folder goes while its session is in flight
          (let ((folder (current-buffer)))
            (with-current-buffer folder (set-buffer-modified-p nil))
            (kill-buffer folder))
          (let ((deadline (+ (float-time) 10)))
            (while (and (vm-net-session-live-p session) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should-not (vm-net-session-live-p session))
          ;; and it left nothing running
          (should-not (process-live-p (vm-net-session-process session)))
          (dolist (buffer (buffer-list))
            (unless (memq buffer before)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))))
      (delete-directory cache t)
      (vm-imap-mock-stop mock))))

(ert-deftest vm-imap-net-test-a-response-in-pieces-is-read-the-same ()
  "A body that arrives in many chunks reads as the one that arrives in one.
On a real connection a literal spans TCP segments, and the reader is resumed
once per segment; the mock answers in one write, so this makes the pieces by
hand and feeds them through the driver."
  (let* ((body (mapconcat (lambda (n) (format "line %d" n))
                          (number-sequence 1 200) "\r\n"))
         (message (concat "Subject: pieces\r\n\r\n" body "\r\n"))
         (response (concat "* 1 FETCH (UID 7 BODY[] {" (number-to-string
                                                        (string-bytes message))
                           "}\r\n" message ")\r\n"
                           "vm1 OK FETCH completed\r\n"))
         (fetched nil))
    (with-temp-buffer
      (vm-imap-net-init)
      (setq vm-imap-current-tag "vm1")
      (let ((session (vm-net-session :name "imap"))
            (iterator nil))
        (setf (vm-net-session-buffer session) (current-buffer))
        (setq iterator
              (vm-imap-net-fetch-bodies-test-collector
               (lambda (uid start end)
                 (push (cons uid (buffer-substring-no-properties start end))
                       fetched))))
        (vm-net-start session iterator)
        ;; deliver it a hundred octets at a time, polling as the filter does
        (let ((sent 0))
          (while (< sent (length response))
            (let ((end (min (length response) (+ sent 100))))
              (goto-char (point-max))
              (insert (substring response sent end))
              (setq sent end))
            (vm-net-poll session)))
        (should (eq (vm-net-session-state session) 'done))
        (should (equal (length fetched) 1))
        (should (equal (car (car fetched)) "7"))
        (should (equal (cdr (car fetched)) message))))))

(iter-defun vm-imap-net-test--collect-fetch (store)
  "Read FETCH responses out of the buffer and hand each to STORE."
  (let ((done nil)
        response)
    (while (not done)
      (setq response (vm-imap-net-verify-response
                      (vm-imap-net-read-a-response) "FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
             (apply store (vm-imap-net-fetch-message-text response)))
            ((vm-imap-response-matches response 'VM 'OK)
             (setq done t))))
    t))

(defalias 'vm-imap-net-fetch-bodies-test-collector
  'vm-imap-net-test--collect-fetch)

(ert-deftest vm-imap-net-test-a-message-deleted-while-the-fetch-ran ()
  "The folder is live while its fetch runs, so what the plan was made from
can change under it.  A message expunged locally mid-fetch does not stop the
messages that are arriving from arriving."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should (equal (length vm-message-list) 1))
    (dotimes (i 5)
      (vm-imap-mock-add-message
       mock "INBOX" (format "From: s%d@example.com\nSubject: m%d\n\nBody.\n" i i)))
    (should (vm-imap-net-get-spooled-mail))
    ;; delete and expunge the message that was already here, mid-flight
    (vm-set-deleted-flag (car vm-message-list) t)
    (vm-expunge-folder)
    (should (vm-imap-net-wait nil 90))
    ;; the five arrived, and the one that went is gone
    (should (equal (length vm-message-list) 5))
    (should (equal (length (seq-filter (lambda (m) (vm-imap-uid-of m))
                                       vm-message-list))
                   5))))

(ert-deftest vm-imap-net-test-an-abandoned-session-runs-its-cleanup ()
  "`vm-net-abandon' closes the generator, so an `unwind-protect' in the
protocol runs -- which is what puts the folder and the server back in step
when a session is stopped part way."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
    (let* ((cleaned nil)
           (buffer (generate-new-buffer " *vm-imap-net-test*"))
           (process (make-network-process
                     :name "vm-imap-net-test" :host 'local
                     :service (vm-imap-mock-port mock)
                     :buffer buffer :noquery t :coding 'binary))
           (session (vm-net-session :process process :name "imap" :timeout 10)))
      (setq vm-imap-net-test--buffer buffer)
      (with-current-buffer buffer (vm-imap-net-init))
      (vm-net-start session (vm-imap-net-test--with-cleanup
                             (lambda () (setq cleaned t))))
      ;; let it get as far as waiting for what never comes
      (let ((deadline (+ (float-time) 2)))
        (while (and (not (vm-net-session-request session))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (vm-net-session-live-p session))
      (should-not cleaned)
      (vm-net-abandon session)
      (should cleaned)
      (should-not (vm-net-session-live-p session))
      (when (process-live-p process) (delete-process process)))))

(iter-defun vm-imap-net-test--with-cleanup (note)
  "Wait for something that never comes, and call NOTE on the way out."
  (unwind-protect
      (progn (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
             (iter-yield-from (vm-imap-net-command "NOOP"))
             (iter-yield (vm-net-request-match "^this never arrives\r\n"))
             'finished)
    (funcall note)))

(ert-deftest vm-imap-net-test-a-timeout-mid-fetch-leaves-the-folder-sound ()
  "A server that stops talking half way through leaves the folder with what
arrived and nothing half-written: the messages already stored are whole, and
the rest are simply not there."
  (vm-imap-net-test--visiting (mock)
    (dotimes (i 4)
      (vm-imap-mock-add-message
       mock "INBOX" (format "From: s%d@example.com\nSubject: m%d\n\nBody %d.\n" i i i)))
    (should (vm-imap-net-get-spooled-mail))
    ;; the server goes silent and the session times out
    (setf (vm-net-session-timeout vm-imap-net-session) 0.3)
    (vm-imap-mock-stop mock)
    (should (vm-imap-net-wait nil 20))
    (save-restriction
      (widen)
      ;; whatever is in the folder parses as whole messages
      (should (equal (length vm-message-list)
                     (cl-count-if (lambda (m) (vm-imap-uid-of m))
                                  vm-message-list))))))


;;; A maildrop used as a spool source

(defmacro vm-imap-net-test--spooling (spec &rest body)
  "Visit a local folder fed from a mock IMAP maildrop, and run BODY in it."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cdr spec))
     (let* ((dir (file-name-as-directory (make-temp-file "vm-imap-net-spool" t)))
            (cache (make-temp-file "vm-imap-net-cache" t))
            (local (expand-file-name "inbox" dir))
            (vm-imap-folder-cache-directory cache)
            (vm-imap-server-timeout 60)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (vm-auto-get-new-mail nil)
            (vm-spool-files (list (list local (vm-imap-mock-spec ,(car spec))
                                        (concat local ".crash"))))
            (before (buffer-list)))
       (unwind-protect
           (progn
             (write-region "" nil local nil 'quiet)
             (cl-letf (((symbol-function 'vm-display) #'ignore))
               (vm-visit-folder local)
               ,@body))
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory dir t)
         (delete-directory cache t)))))

(ert-deftest vm-imap-net-test-a-maildrop-fills-the-folder-without-waiting ()
  "A local folder fed from an IMAP maildrop gets its mail through the
driver: `vm-get-new-mail' starts the session and returns, the crash box is
written when the messages arrive, and the folder gobbles it then."
  (vm-imap-net-test--spooling (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (null vm-message-list))
    (vm-get-new-mail)
    ;; not here yet: nothing waited for it
    (should (null vm-message-list))
    (should (vm-imap-net-wait nil 90))
    (should (equal (length vm-message-list) 2))
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))
    ;; and the crash box was taken in, not left lying about
    (should-not (file-exists-p (nth 2 (car vm-spool-files))))))

(ert-deftest vm-imap-net-test-a-maildrop-message-is-not-fetched-twice ()
  "The UIDs fetched are remembered with the UIDVALIDITY they were valid
under, so a second run brings only what arrived since -- and a UID means
nothing without it, a recreated mailbox handing the same numbers to other
messages."
  (vm-imap-net-test--spooling (mock :messages (list vm-imap-net-test--alice))
    (vm-get-new-mail)
    (should (vm-imap-net-wait nil 90))
    (should (equal (length vm-message-list) 1))
    (should (equal (length vm-imap-retrieved-messages) 1))
    (should (nth 1 (car vm-imap-retrieved-messages)))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (vm-get-new-mail)
    (should (vm-imap-net-wait nil 90))
    (should (equal (length vm-message-list) 2))
    (should (equal (length vm-imap-retrieved-messages) 2))))

(ert-deftest vm-imap-net-test-a-maildrop-can-be-emptied-as-it-is-read ()
  "With auto-expunge on, what has been fetched is deleted and expunged in
the same session, so the server is not left holding a second copy."
  (let ((vm-imap-expunge-after-retrieving t))
    (vm-imap-net-test--spooling (mock :messages (list vm-imap-net-test--alice
                                                      vm-imap-net-test--bob))
      (vm-get-new-mail)
      (should (vm-imap-net-wait nil 90))
      (should (equal (length vm-message-list) 2))
      (should (vm-imap-mock-received-p mock "UID STORE"))
      (should (vm-imap-mock-received-p mock "EXPUNGE"))
      (should (null (vm-imap-mock-messages mock "INBOX"))))))

(ert-deftest vm-imap-net-test-mail-on-disk-is-not-fetched-a-second-time ()
  "A session that fails after writing the crash box leaves nothing to refetch.

The maildrop is set to delete what is fetched and the server refuses the
EXPUNGE, so the session fails with the mail already in the crash box.  The
folder gobbles that crash box when it next looks; unless it also knows it has
those UIDs, it fetches them again and both copies land -- three goes at a
two-message maildrop put four messages in the folder."
  (let ((vm-imap-expunge-after-retrieving t))
    (vm-imap-net-test--spooling (mock :messages (list vm-imap-net-test--alice
                                                      vm-imap-net-test--bob)
                                      :refuse "EXPUNGE")
      (let ((crash (nth 2 (car vm-spool-files))))
        (vm-get-new-mail)
        (should (vm-imap-net-wait nil 90))
        ;; the mail is on disk and the folder knows it has it, though the
        ;; session it came in on failed
        (should (file-exists-p crash))
        (should (equal (length vm-imap-retrieved-messages) 2))
        (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
        ;; so asking again brings in what is on disk, once
        (vm-get-new-mail)
        (should (vm-imap-net-wait nil 90))
        (vm-get-new-mail)
        (should (vm-imap-net-wait nil 90))
        (should (equal (mapcar #'vm-su-subject vm-message-list)
                       '("badgers" "otters")))))))

(ert-deftest vm-imap-net-test-an-extra-fetch-item-is-stepped-over ()
  "A server that answers with more than VM asked for still delivers the mail.

RFC 3501 7.4.2 lets a server send data items the client did not ask about, and
a server with CONDSTORE on sends MODSEQ with everything.  Refusing to read
past one failed the session on its first command: \"expected UID, RFC822.SIZE
and (FLAGS list) in FETCH response\", and no mailbox arrived at all."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob)
                                    :extra-fetch-items t)
    (should (equal (length vm-message-list) 2))
    (should (equal (mapcar #'vm-imap-uid-of vm-message-list) '("1" "2")))
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))))

(ert-deftest vm-imap-net-test-an-unsolicited-flag-report-is-not-message-data ()
  "A FETCH the server sent of its own accord is not taken for an answer.

Somebody else changing a message's flags has the server report them whenever
it next can (RFC 3501 7.4.1).  That response carries no UID, and taking it for
message data put an entry with no UID in the folder's tables: \"Wrong type
argument: stringp, nil\", and no mail."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob)
                                    :unsolicited-flags t)
    (should (equal (length vm-message-list) 2))
    (should (equal (mapcar #'vm-imap-uid-of vm-message-list) '("1" "2")))))

(ert-deftest vm-imap-net-test-the-item-skip-steps-over-one-item ()
  "`vm-imap-skip-fetch-item' takes one item off, whatever its structure.
The parsers walk the items of a FETCH response by name; this is what they do
with a name they do not know, and it has to leave the walk on the next name
rather than in the middle of a value."
  (with-temp-buffer
    (insert "MODSEQ (23) UID 7")
    ;; the token structures the reader produces: (TYPE START END) or (list TOKEN...)
    (let* ((modseq '(atom 1 7))
           (value '(list (atom 9 11)))
           (uid '(atom 13 16))
           (number '(atom 17 18))
           (contents (list modseq value uid number)))
      (should (equal (vm-imap-skip-fetch-item contents) (list uid number)))
      ;; a section in brackets, as BODY[]/BODY[HEADER] have
      (should (equal (vm-imap-skip-fetch-item
                      (list '(atom 1 5) '(vector) '(string 6 9) uid))
                     (list uid)))
      ;; and a name with nothing after it leaves nothing behind
      (should-not (vm-imap-skip-fetch-item (list modseq))))))

(ert-deftest vm-imap-net-test-expunging-what-is-gone-settles ()
  "An expunge request for a UID the mailbox no longer has is done with.

`vm-expunge-imap-messages' works from `vm-imap-retrieved-messages', and only
what the server expunged was struck off it.  A UID the mailbox no longer had
stayed, so the command opened a session for it again every time it was run --
for ever.  A UID that is not there is a deletion that has already happened,
which is what `vm-imap-net-note-expunged' says of the folder\\='s own list."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((spec (vm-imapdrop-sans-password (vm-imap-mock-spec mock)))
          (validity (vm-folder-imap-uid-validity)))
      (setq vm-imap-retrieved-messages
            (list (list "1" validity spec 'uid)
                  (list "99" validity spec 'uid)))
      (should (eq (vm-imap-net-expunge-retrieved) t))
      (let ((deadline (+ (float-time) 10)))
        (while (and vm-imap-retrieved-messages (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      ;; the one that was there is gone from the server, and neither is left
      ;; on the folder's list
      (should (null (vm-imap-mock-messages mock "INBOX")))
      (should-not vm-imap-retrieved-messages))))

(ert-deftest vm-imap-net-test-a-warning-does-not-stop-the-fetch ()
  "A session that has something to warn about does not hold Emacs to say it.

The server refuses every STORE, so saving the flags of two messages warns
twice.  Each warning was `sit-for' 2 in a process filter: four seconds of
Emacs stopped, for work that takes a tenth of one.  Timed rather than counted
-- what is wrong with a pause is the wall clock."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (setf (vm-imap-mock-refuse mock) "STORE")
    (dolist (message vm-message-list)
      (vm-set-unread-flag message nil)
      (vm-set-attribute-modflag-of message t))
    (let ((vm-verbosity 5)
          (vm-verbal-time 2)
          (start (float-time)))
      (vm-imap-net-save-attributes)
      (should (vm-imap-net-wait nil 20))
      (should (< (- (float-time) start) 2)))))

(ert-deftest vm-imap-net-test-a-fetch-keeps-vms-buffer-type-discipline ()
  "A fetch works with VM's own assertions checked.

`vm-buffer-types' is the stack VM keeps of what kind of buffer it is working
in, and `vm-buffer-type:assert' is how the folder code catches being run
somewhere else.  The driver never said what it was in, so
`vm-imap-get-synchronization-data' asserted a folder against a stack that said
nothing: with `vm-assertion-checking-off' nil -- `test-runner --assert', and
anyone debugging VM -- visiting an IMAP folder brought in no messages at all.

The assertion is a macro over `vm-assert', so this turns the checking on
rather than stubbing anything, and `inhibit-debugger' because `vm-assert'
binds `debug-on-error' and batch has nobody to debug for."
  (let ((vm-assertion-checking-off nil)
        (inhibit-debugger t)
        (said nil))
    (cl-letf* ((real (symbol-function 'vm-warn))
               ((symbol-function 'vm-warn)
                (lambda (level seconds &rest args)
                  (push (apply #'format args) said)
                  (apply real level seconds args))))
      (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                       vm-imap-net-test--bob))
        (should (equal (length vm-message-list) 2))
        (should-not (seq-filter (lambda (line)
                                  (string-match-p "assertion failed" line))
                                said))))))

(ert-deftest vm-imap-net-test-the-flush-timer-may-fire-mid-fetch ()
  "VM's own flush timer writing the folder does not spoil a fetch.

`vm-flush-interval' is 90 seconds by default, so `vm-flush-cached-data-all-folders'
runs in every VM session and writes X-VM headers into folder buffers, while a
fetch is collecting a bunch to put into the same folder.  Here the flush is
made to fire after every message that arrives."
  (let ((vm-imap-message-bunch-size 2)
        (flushes 0))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 6)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: s%d@example.com\nSubject: m%d\n\nBody %d.\n" i i i)))
      (let ((real (symbol-function 'vm-imap-net-hold)))
        (cl-letf (((symbol-function 'vm-imap-net-hold)
                   (lambda (&rest args)
                     (let ((answer (apply real args)))
                       (setq flushes (1+ flushes))
                       (vm-flush-cached-data-all-folders)
                       answer))))
          (should (equal (vm-imap-net-test--get-mail mock) 6))))
      (should (> flushes 0))
      (should (equal (length vm-message-list) 6))
      (should (equal (mapcar #'vm-imap-uid-of vm-message-list)
                     '("1" "2" "3" "4" "5" "6")))
      ;; the list and the buffer agree, which is what a stray write breaks
      (should (equal (length vm-message-list)
                     (save-restriction
                       (widen)
                       (count-matches "^From " (point-min) (point-max)))))
      ;; and it still saves and reads back as six messages
      (let ((file (buffer-file-name)))
        (set-buffer-modified-p t)
        (vm-save-folder)
        (should (equal (with-temp-buffer
                         (insert-file-contents file)
                         (count-matches "^From " (point-min) (point-max)))
                       6))))))

(ert-deftest vm-imap-net-test-two-maildrops-into-one-folder-take-turns ()
  "Two maildrops among a folder's spool files are fetched one after the other.

`vm-get-new-mail' walks the spool files and starts each fetch without waiting,
so the second was started while the first was still running: two sessions
writing one folder, one buffer and one cache file.  The folder's own refusal
came too late -- the second session was already talking to a server, and
unowned, so nothing could see it or stop it -- and the whole command failed
with \"a second session was started while IMAP movemail was running\".

The second waits its turn now, and both lots of mail arrive."
  (vm-imap-mock-with (one :messages (list "From: a@example.com\nSubject: from-one\n\nA.\n"))
    (vm-imap-mock-with (two :messages (list "From: b@example.com\nSubject: from-two\n\nB.\n"))
      (let* ((dir (file-name-as-directory (make-temp-file "vm-imap-two" t)))
             (cache (make-temp-file "vm-imap-two-cache" t))
             (local (expand-file-name "inbox" dir))
             (vm-imap-folder-cache-directory cache)
             (vm-imap-server-timeout 60)
             (vm-frame-per-folder nil)
             (vm-mutable-frame-configuration nil)
             (vm-auto-get-new-mail nil)
             (vm-spool-files (list (list local (vm-imap-mock-spec one)
                                         (concat local ".crash1"))
                                   (list local (vm-imap-mock-spec two)
                                         (concat local ".crash2"))))
             (before (buffer-list))
             (overlapped nil)
             (real (symbol-function 'vm-imap-net-take-session)))
        (unwind-protect
            (progn
              (write-region "" nil local nil 'quiet)
              (cl-letf (((symbol-function 'vm-display) #'ignore)
                        ((symbol-function 'vm-imap-net-take-session)
                         (lambda (session &rest more)
                           (when (and vm-imap-net-session
                                      (not (eq vm-imap-net-session session))
                                      (vm-net-session-live-p vm-imap-net-session))
                             (setq overlapped t))
                           (apply real session more))))
                (vm-visit-folder local)
                (vm-get-new-mail)
                (let ((deadline (+ (float-time) 20)))
                  (while (and (or (vm-imap-net-busy-p) vm-imap-net-waiting)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.05))))
              ;; both maildrops arrived, and never at the same time
              (should-not overlapped)
              (should (equal (sort (mapcar #'vm-su-subject vm-message-list)
                                   #'string-lessp)
                             '("from-one" "from-two")))
              ;; nothing left lying about
              (should-not (file-exists-p (concat local ".crash1")))
              (should-not (file-exists-p (concat local ".crash2"))))
          (dolist (buffer (buffer-list))
            (unless (memq buffer before)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer))))
          (delete-directory dir t)
          (delete-directory cache t))))))

(ert-deftest vm-imap-net-test-a-save-mid-fetch-leaves-no-duplicate ()
  "A save landing inside a bunch does not write a message twice over a crash.

The fetch used to write each message straight into the folder as it arrived,
so between messages the folder held text the message list did not know about.
A save then wrote that to the cache file with none of VM's own data on it, and
if Emacs went before the bunch was taken in, the next fetch had no UID for it
and brought it again: the reader saw the message twice.

The bunch is collected in a buffer of its own now and goes into the folder in
one piece, so a save can only ever see whole messages the list knows about."
  (let ((vm-imap-message-bunch-size 4))
    (vm-imap-mock-with (mock :messages (list "From: z@example.com\nSubject: first\n\nZ.\n"))
      (let* ((cache (make-temp-file "vm-imap-save-cache" t))
             (vm-imap-folder-cache-directory cache)
             (vm-imap-server-timeout 60)
             (vm-frame-per-folder nil)
             (vm-mutable-frame-configuration nil)
             (before (buffer-list))
             (folder nil) (file nil) (saved nil))
        (unwind-protect
            (progn
              (vm-visit-imap-folder (vm-imap-mock-spec mock))
              (setq folder (current-buffer) file (buffer-file-name))
              (should (vm-imap-net-wait nil 15))
              (should (equal (length vm-message-list) 1))
              (dotimes (i 4)
                (vm-imap-mock-add-message
                 mock "INBOX"
                 (format "From: s%d@example.com\nSubject: m%d\n\nBody.\n" i i)))
              ;; a save while the bunch is part way in
              (let ((real (symbol-function 'vm-imap-net-hold)))
                (cl-letf (((symbol-function 'vm-imap-net-hold)
                           (lambda (&rest args)
                             (let ((answer (apply real args)))
                               (unless saved
                                 (setq saved t)
                                 (with-current-buffer folder
                                   (set-buffer-modified-p t)
                                   (vm-save-folder)))
                               answer))))
                  (with-current-buffer folder (vm-get-new-mail))
                  (let ((deadline (+ (float-time) 10)))
                    (while (and (not saved) (< (float-time) deadline))
                      (accept-process-output nil 0.05)))))
              (should saved)
              ;; the file holds only what the folder had taken in
              (should (equal (with-temp-buffer
                               (insert-file-contents file)
                               (count-matches "^From " (point-min) (point-max)))
                             1))
              ;; the crash: stop, drop the buffer, visit again and fetch
              (with-current-buffer folder
                (vm-imap-net-stop)
                (set-buffer-modified-p nil))
              (kill-buffer folder)
              (vm-visit-imap-folder (vm-imap-mock-spec mock))
              (should (vm-imap-net-wait nil 20))
              (let ((subjects (mapcar #'vm-su-subject vm-message-list)))
                (should (equal (length subjects) 5))
                (should (equal (length subjects)
                               (length (delete-dups (copy-sequence subjects)))))))
          (dolist (buffer (buffer-list))
            (unless (memq buffer before)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer))))
          (delete-directory cache t))))))

(ert-deftest vm-imap-net-test-a-session-can-leave-its-traffic-behind ()
  "`vm-imap-keep-trace-buffer' keeps a driver session's buffer, as it does the
blocking path's.

The driver killed every session buffer the moment the session ended, so after
a save there was nothing left to look at -- and \"did VM send that delete?\" is
answered by the traffic and by nothing else."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (let ((vm-imap-keep-trace-buffer 2)
          (vm-kept-imap-buffers nil))
      (vm-set-deleted-flag (car vm-message-list) t)
      (vm-expunge-folder :quiet t)
      (set-buffer-modified-p t)
      (vm-save-folder)
      (should (vm-imap-net-wait nil 15))
      ;; the session that sent the changes is still there, named as the
      ;; blocking path names its own
      (should vm-kept-imap-buffers)
      (should (seq-find (lambda (buffer)
                          (string-match-p "\\`saved " (buffer-name buffer)))
                        vm-kept-imap-buffers))
      ;; and it holds what went out and what the server said back
      (should (seq-find
               (lambda (buffer)
                 (and (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (and (string-match-p "UID STORE" (buffer-string))
                             (string-match-p "EXPUNGE" (buffer-string))))))
               vm-kept-imap-buffers))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            vm-kept-imap-buffers))
    ;; and with the setting off, nothing is kept
    (let ((vm-imap-keep-trace-buffer nil)
          (vm-kept-imap-buffers nil))
      (vm-imap-net-synchronize nil t)
      (should (vm-imap-net-wait nil 15))
      (should-not vm-kept-imap-buffers))))

(ert-deftest vm-imap-net-test-a-session-says-goodbye ()
  "Every session says LOGOUT on its way out.  A server counts its
connections, and a client that drops them without a word leaves it to time
them out."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should (vm-imap-mock-received-p mock "LOGOUT"))))


;;; The connections that go through something else

(ert-deftest vm-imap-net-test-a-preauthenticated-session-does-not-log-in ()
  "A connection the hook made arrives authenticated: VM asks what the server
can do and gets on with it, rather than sending a LOGIN it has no password
for."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice)
                           :preauth t)
    (let* ((port (vm-imap-mock-port mock))
           (vm-imap-session-preauth-hook
            (list (lambda (&rest _)
                    (make-network-process :name "vm-imap-net-test-preauth"
                                          :host 'local :service port
                                          :noquery t :coding 'binary))))
           (spec (format "imap:127.0.0.1:%d:INBOX:preauth:vmtest:*" port))
           (opened (vm-imap-net-open spec "preauth"))
           (session (car opened)))
      (setq vm-imap-net-test--buffer (vm-net-session-buffer session))
      (unwind-protect
          (progn
            (vm-net-start session
                          (vm-imap-net-open-session (nth 2 opened)
                                                    (nth 3 opened)))
            (let ((deadline (+ (float-time) 10)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (vm-net-session-state session) 'done))
            (should (memq 'IMAP4REV1 (car (vm-net-session-value session))))
            (should-not (vm-imap-mock-received-p mock "LOGIN")))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))))))

(defvar vm-imap-net-test--stunnel-script
  (concat "printf '* OK ready\\r\\n'\n"
	  "while IFS= read -r line; do\n"
	  "  tag=${line%% *}\n"
	  "  case \"$line\" in\n"
	  "    *CAPABILITY*) printf '* CAPABILITY IMAP4REV1 AUTH=LOGIN\\r\\n'\n"
	  "                  printf '%s OK done\\r\\n' \"$tag\" ;;\n"
	  "    *LOGIN*)      printf '%s OK logged in\\r\\n' \"$tag\" ;;\n"
	  "    *LOGOUT*)     printf '%s OK bye\\r\\n' \"$tag\"; exit 0 ;;\n"
	  "  esac\n"
	  "done\n")
  "A shell script that talks enough IMAP to be logged in to.
Stands in for stunnel, which VM talks to over its standard input and output
rather than over a socket: there is no port here to point a mock server at.")

(ert-deftest vm-imap-net-test-a-tls-maildrop-does-not-ask-make-network-process ()
  "An imap-ssl maildrop is connected with `open-network-stream'.

`make-network-process' has no TLS: given `:type \\='tls' it answers
\"Unsupported connection type\" and nothing is connected at all.  Every
imap-ssl maildrop without an stunnel failed there, before a byte was sent.
Checked by watching which of the two is called, since a test cannot make a TLS
server to talk to."
  (let ((asked nil)
        (vm-stunnel-program nil)
        (vm-imap-passwords nil))
    (cl-letf (((symbol-function 'open-network-stream)
               (lambda (name buffer host service &rest parameters)
                 (setq asked (list 'open-network-stream host service
                                   (plist-get parameters :type)
                                   (plist-get parameters :nowait)))
                 ;; something process-like to hand back, made without the
                 ;; function this test has taken away
                 (start-process name buffer "cat")))
              ((symbol-function 'make-network-process)
               (lambda (&rest _)
                 (error "make-network-process cannot do TLS"))))
      (let* ((spec "imap-ssl:far.example.com:993:INBOX:login:vmtest:secret")
             (opened (vm-imap-net-open spec "tls"))
             (session (car opened)))
        (unwind-protect
            (progn
              (should (equal (nth 0 asked) 'open-network-stream))
              (should (equal (nth 1 asked) "far.example.com"))
              (should (equal (nth 2 asked) 993))
              (should (eq (nth 3 asked) 'tls))
              ;; and still without waiting for the connection
              (should (nth 4 asked)))
          (let ((process (vm-net-session-process session)))
            (when (process-live-p process) (delete-process process)))
          (let ((buffer (vm-net-session-buffer session)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest vm-imap-net-test-an-stunnel-maildrop-talks-over-the-programs-pipes ()
  "An imap-ssl maildrop with `vm-stunnel-program' set runs the program and
talks to it, greeting and login and all.

VM used to hand stunnel `-d 127.0.0.1:PORT' and wait for that port to answer,
which stunnel has no such option for and never did: the session sat until the
whole server timeout was up and then failed with \"did not start listening on
port\".  Told nothing to listen on, stunnel relays its own standard input and
output, which is what the blocking path has always used it for."
  (let* ((vm-stunnel-program "sh")
         (vm-stunnel-program-switches nil)
         (vm-imap-server-timeout 60)
         (spec "imap-ssl:far.example.com:993:INBOX:login:vmtest:secret")
         (opened nil)
         (session nil))
    (cl-letf (((symbol-function 'vm-setup-stunnel-random-data-if-needed)
               (lambda () nil))
              ((symbol-function 'vm-stunnel-configuration-args)
               (lambda (&rest _) (list "-c" vm-imap-net-test--stunnel-script))))
      (setq opened (vm-imap-net-open spec "stunnel")
            session (car opened)))
    (unwind-protect
        (let ((process (vm-net-session-process session)))
          ;; a program, not a connection: nothing was asked to listen anywhere
          (should (processp process))
          (should (eq (process-type process) 'real))
          (should-not (member "-d" (process-command process)))
          (vm-net-start session (vm-imap-net-open-session (nth 2 opened)
                                                          (nth 3 opened)))
          (let ((deadline (+ (float-time) 10)))
            (while (and (vm-net-session-live-p session)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (vm-net-session-state session) 'done))
          (should (memq 'IMAP4REV1 (car (vm-net-session-value session)))))
      (let ((process (vm-net-session-process session)))
        (when (process-live-p process) (delete-process process)))
      (let ((buffer (vm-net-session-buffer session)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest vm-imap-net-test-an-ssh-maildrop-waits-for-its-tunnel ()
  "An imap-ssh maildrop starts the tunnel and has no connection until the
tunnel is listening -- which is the wait the blocking path does inside
`vm-setup-ssh-tunnel', with `accept-process-output' and a loop of connect
attempts to find a free port."
  (let* ((vm-ssh-program "sleep")
         (vm-ssh-program-switches nil)
         (vm-ssh-remote-command "")
         (vm-imap-server-timeout 0.5)
         (spec "imap-ssh:far.example.com:143:INBOX:login:vmtest:secret")
         (opened (vm-imap-net-open spec "ssh"))
         (session (car opened))
         (finished nil))
    (setf (vm-net-session-finished session) (lambda (s) (setq finished s)))
    (unwind-protect
        (progn
          ;; no connection yet, and the session has not read anything
          (should-not (vm-net-session-process session))
          (vm-net-start session (vm-imap-net-open-session "vmtest" "secret"))
          (should (eq (vm-net-session-state session) 'running))
          (should-not (vm-net-session-request session))
          ;; sleep listens on nothing, so the session is failed and said so
          (let ((deadline (+ (float-time) 10)))
            (while (and (vm-net-session-live-p session)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (vm-net-session-state session) 'failed))
          (should (eq finished session))
          (should (string-match-p "did not start listening"
                                  (error-message-string
                                   (vm-net-session-error session)))))
      (let ((buffer (vm-net-session-buffer session)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest vm-imap-net-test-a-tunnelled-session-reads-what-comes-back ()
  "With something listening where the tunnel would be, the session runs as
any other does: the tunnel is only how the connection is made."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((port (vm-imap-mock-port mock))
           (session (vm-net-session :name "tunnelled" :timeout 10))
           (buffer (vm-imap-net-session-buffer "tunnelled")))
      (setf (vm-net-session-buffer session) buffer)
      (setq vm-imap-net-test--buffer buffer)
      (unwind-protect
          (progn
            (vm-net-start session (vm-imap-net-open-session "vmtest" "secret"))
            (should-not (vm-net-session-request session))
            ;; the tunnel comes up: sleep is the program, the mock is what is
            ;; listening on the port it was told to wait for
            (vm-net-tunnel session "sleep" (list "30") port 5
                           (lambda (tunnel)
                             (when tunnel
                               (vm-net-attach
                                session
                                (vm-imap-net-connect "tunnelled" "127.0.0.1"
                                                     port buffer)))))
            (let ((deadline (+ (float-time) 10)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (vm-net-session-state session) 'done))
            (should (memq 'IMAP4REV1 (car (vm-net-session-value session))))
            (should (vm-imap-mock-received-p mock "LOGIN")))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))))))


;;; More of what only happens because nothing waits

(ert-deftest vm-imap-net-test-a-server-may-answer-a-fetch-backwards ()
  "The UID in each response says which message it is, so a server that
answers a range in any order is read correctly.  Order is not something a
client may rely on, and issue #185 asked for the UID for this reason."
  (vm-imap-net-test--visiting (mock :reorder-fetch t)
    (dotimes (i 4)
      (vm-imap-mock-add-message
       mock "INBOX"
       (format "From: s%d@example.com\nSubject: number %d\n\nBody %d.\n" i i i)))
    (should (equal (vm-imap-net-test--get-mail mock) 4))
    (should (equal (length vm-message-list) 4))
    ;; each message has its own body, whatever order they arrived in
    (save-restriction
      (widen)
      (dolist (message vm-message-list)
        (let ((subject (vm-su-subject message))
              (body (buffer-substring-no-properties (vm-text-of message)
                                                    (vm-text-end-of message))))
          (should (string-match "number \\([0-9]+\\)" subject))
          (should (string-match-p (format "Body %s\\." (match-string 1 subject))
                                  body)))))))

(ert-deftest vm-imap-net-test-a-fetch-cut-off-leaves-whole-messages ()
  "A connection lost part way through a bunch leaves the folder holding the
messages that arrived, each of them whole, and not the beginning of the one
that did not."
  (vm-imap-net-test--visiting (mock :drop-after-fetch 2)
    (dotimes (i 5)
      (vm-imap-mock-add-message
       mock "INBOX"
       (format "From: s%d@example.com\nSubject: number %d\n\nBody %d.\n" i i i)))
    (let ((result (vm-imap-net-test--get-mail mock)))
      ;; the session failed, and said so
      (should (consp result)))
    ;; whatever is in the folder is whole: every message has a UID, and the
    ;; text of each ends where a message ends
    (save-restriction
      (widen)
      (dolist (message vm-message-list)
        (should (vm-imap-uid-of message))
        (should (string-match-p "Body [0-9]+\\.\n\\'"
                                (buffer-substring-no-properties
                                 (vm-text-of message)
                                 (vm-text-end-of message))))))))

(ert-deftest vm-imap-net-test-an-arrival-leaves-the-reader-where-they-were ()
  "Mail landing while the reader is on a message does not move them off it.
The fetch runs between their keystrokes, and a folder that jumped to the new
mail would take the message they were reading out from under them."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (vm-goto-message 2)
    (let ((here (car vm-message-pointer)))
      (vm-imap-mock-add-message mock "INBOX"
                                "From: c@example.com\nSubject: third\n\nNew.\n")
      (should (equal (vm-imap-net-test--get-mail mock) 1))
      (should (equal (length vm-message-list) 3))
      (should (eq (car vm-message-pointer) here)))))

(ert-deftest vm-imap-net-test-two-folders-fetch-side-by-side ()
  "Two folders fetching at once do not cross: each session writes into the
folder it belongs to, and neither waits for the other."
  (let ((first (vm-imap-mock-start))
        (second (vm-imap-mock-start :mailbox "Other"))
        (cache (make-temp-file "vm-imap-net-cache" t))
        (before (buffer-list)))
    (unwind-protect
        (let ((vm-imap-folder-cache-directory cache)
              (vm-imap-server-timeout 60)
              (vm-frame-per-folder nil)
              (vm-mutable-frame-configuration nil)
              (folder-one nil) (folder-two nil))
          (vm-imap-mock-add-message first "INBOX"
                                    "From: a@example.com\nSubject: one\n\nA.\n")
          (vm-imap-mock-add-message second "Other"
                                    "From: b@example.com\nSubject: two\n\nB.\n")
          (vm-visit-imap-folder (vm-imap-mock-spec first))
          (setq folder-one (current-buffer))
          (vm-visit-imap-folder (vm-imap-mock-spec second "Other"))
          (setq folder-two (current-buffer))
          (should-not (eq folder-one folder-two))
          ;; both sessions started, and both land
          (should (vm-imap-net-wait folder-one 20))
          (should (vm-imap-net-wait folder-two 20))
          (with-current-buffer folder-one
            (should (equal (mapcar #'vm-su-subject vm-message-list) '("one"))))
          (with-current-buffer folder-two
            (should (equal (mapcar #'vm-su-subject vm-message-list) '("two")))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory cache t)
      (vm-imap-mock-stop first)
      (vm-imap-mock-stop second))))

(ert-deftest vm-imap-net-test-an-abandoned-session-says-goodbye ()
  "LOGOUT is said on the way out of an abandoned session too: it is in an
`unwind-protect', and the driver closes the generator rather than dropping
it, which is what makes those forms run."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
    (let* ((buffer (vm-imap-net-session-buffer "goodbye"))
           (process (make-network-process
                     :name "vm-imap-net-test" :host 'local
                     :service (vm-imap-mock-port mock)
                     :buffer buffer :noquery t :coding 'binary))
           (session (vm-net-session :process process :name "imap" :timeout 10)))
      (setq vm-imap-net-test--buffer buffer)
      (vm-net-start session (vm-imap-net-get-new-mail (current-buffer) "INBOX"
                                                      "vmtest" "secret"))
      ;; let it get as far as being logged in
      (let ((deadline (+ (float-time) 5)))
        (while (and (not (vm-imap-mock-received-p mock "SELECT"))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (vm-net-abandon session)
      (let ((deadline (+ (float-time) 5)))
        (while (and (not (vm-imap-mock-received-p mock "LOGOUT"))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (vm-imap-mock-received-p mock "LOGOUT"))
      (when (process-live-p process) (delete-process process)))))

(defun vm-imap-net-test--bunch-buffers ()
  "The bunch-collecting buffers left alive."
  (seq-filter (lambda (buffer)
                (string-prefix-p " *vm-imap-bunch*" (buffer-name buffer)))
              (buffer-list)))

(ert-deftest vm-imap-net-test-an-abandoned-fetch-leaves-no-bunch-buffer ()
  "The buffer a bunch is collected in is killed however the fetch ends.
It was killed by the last form of the generator\'s body, which an abandoned
session or any error never reaches -- and the buffer is made before the first
thing that can fail, so a refused UIDVALIDITY leaked one every time."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
    (let* ((buffer (vm-imap-net-session-buffer "leak"))
           (process (make-network-process
                     :name "vm-imap-net-test" :host 'local
                     :service (vm-imap-mock-port mock)
                     :buffer buffer :noquery t :coding 'binary))
           (session (vm-net-session :process process :name "imap" :timeout 10)))
      (setq vm-imap-net-test--buffer buffer)
      (should-not (vm-imap-net-test--bunch-buffers))
      (vm-net-start session (vm-imap-net-get-new-mail (current-buffer) "INBOX"
                                                      "vmtest" "secret"))
      (let ((deadline (+ (float-time) 5)))
        (while (and (not (vm-imap-mock-received-p mock "SELECT"))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (vm-net-abandon session)
      (should-not (vm-imap-net-test--bunch-buffers))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest vm-imap-net-test-waiting-answers-nil-when-it-runs-out ()
  "`vm-imap-net-wait' says whether the session finished.  A caller that has
to have the mail can tell the difference between having it and having waited
long enough."
  (vm-imap-net-test--visiting (mock :slow-greeting 2)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (should (vm-imap-net-get-spooled-mail))
    (should-not (vm-imap-net-wait nil 0.2))
    (should (vm-imap-net-busy-p))
    (should (vm-imap-net-wait nil 20))))

(ert-deftest vm-imap-net-test-a-megabyte-arrives-whole ()
  "A message far larger than a TCP segment arrives whole, through the
socket, in whatever chunks the operating system chooses to deliver it in.
The literal is read by its octet count, and the count is what says where it
ends."
  (let* ((line "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde\n")
         (big (concat "From: alice@example.com\nSubject: large\n\n"
                      (mapconcat #'identity (make-list 16000 line) ""))))
    (should (> (length big) 1000000))
    (vm-imap-net-test--visiting (mock :messages (list big))
      (should (equal (length vm-message-list) 1))
      (save-restriction
        (widen)
        (let ((text (buffer-substring-no-properties
                     (vm-text-of (car vm-message-list))
                     (vm-text-end-of (car vm-message-list)))))
          ;; every line of it, and nothing after the last one
          (should (equal (length (split-string text "\n" t)) 16000))
          (should (string-prefix-p "0123456789abcdef" text)))))))


;;; The check that runs on a timer

(ert-deftest vm-imap-net-test-a-check-does-not-wait-and-says-what-it-found ()
  "`vm-check-for-spooled-mail' on an IMAP folder starts the check and
returns.  It cannot know the answer yet, and that is the point: the check
runs on a timer, and every round of it stopped Emacs for as long as the
server took."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (should (equal (length vm-message-list) 1))
    (setq vm-spooled-mail-waiting nil)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (let ((started (float-time)))
      (should (vm-check-for-spooled-mail nil t))
      ;; it did not wait for the server to answer
      (should (< (- (float-time) started) 0.5))
      (should-not vm-spooled-mail-waiting))
    (should (vm-imap-net-wait nil 90))
    (should vm-spooled-mail-waiting)
    ;; and it did not fetch anything: a check only looks
    (should (equal (length vm-message-list) 1))))

(ert-deftest vm-imap-net-test-a-check-with-nothing-new-says-so ()
  "A mailbox holding only what the folder has already reports no mail, and
the mode line stops saying there is some."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (setq vm-spooled-mail-waiting t)
    (should (vm-check-for-spooled-mail nil t))
    (should (vm-imap-net-wait nil 90))
    (should-not vm-spooled-mail-waiting)))

(ert-deftest vm-imap-net-test-a-password-vm-already-knows-is-enough ()
  "A maildrop written with `*' for its password is one VM is to find the
password for.  It may already know it -- from a session earlier in this
Emacs, or from auth-source -- and only asking the user is out of the
question inside a filter.  Refusing those sent every folder whose password
is not written down back to the blocking path."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((port (vm-imap-mock-port mock))
           (spec (format "imap:127.0.0.1:%d:INBOX:login:vmtest:*" port))
           (vm-imap-passwords nil))
      ;; not known yet, so this is one for the blocking path
      (should-error (vm-imap-net-open spec "x") :type 'vm-imap-net-no-password)
      ;; known now, as it would be after one login
      (setq vm-imap-passwords
            (list (list (vm-imapdrop-sans-password-and-mailbox spec) "secret")))
      (let* ((opened (vm-imap-net-open spec "known"))
             (session (car opened)))
        (setq vm-imap-net-test--buffer (vm-net-session-buffer session))
        (should (equal (nth 3 opened) "secret"))
        (vm-net-start session (vm-imap-net-open-session (nth 2 opened)
                                                        (nth 3 opened)))
        (let ((deadline (+ (float-time) 10)))
          (while (and (vm-net-session-live-p session) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (should (eq (vm-net-session-state session) 'done))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))))))


;;; The check on a maildrop, which is what the timer asks

(defmacro vm-imap-net-test--in-a-folder-with-spool (spec &rest body)
  "Visit a local folder whose spool file is MOCK's maildrop, and run BODY."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cdr spec))
     (let* ((dir (file-name-as-directory (make-temp-file "vm-imap-check" t)))
            (folder (expand-file-name "inbox" dir))
            (crash (expand-file-name "crash" dir))
            (vm-init-file nil)
            (vm-preferences-file nil)
            (vm-confirm-quit nil)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (vm-folder-history vm-folder-history)
            (vm-global-block-new-mail nil)
            (vm-auto-get-new-mail nil)
            (vm-imap-server-timeout 20)
            (vm-imap-retrieved-messages nil)
            (vm-crash-box crash)
            (vm-spool-files (list (list folder (vm-imap-mock-spec ,(car spec))
                                        crash)))
            (before (buffer-list)))
       (unwind-protect
           (progn
             (write-region "" nil folder nil 'quiet)
             (cl-letf (((symbol-function 'vm-display) #'ignore))
               (vm-visit-folder folder)
               ,@body))
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory dir t)))))

(defun vm-imap-net-test--settle (&optional seconds)
  "Let the outstanding mail checks answer."
  (let ((deadline (+ (float-time) (or seconds 20))))
    (while (and vm-mail-checks-outstanding (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(ert-deftest vm-imap-net-test-a-maildrop-check-does-not-wait ()
  "The check on an IMAP maildrop starts and returns.  It runs on a timer, and
every round of it stopped Emacs for as long as the server took -- 41 seconds
in one report, with half a second of that VM's own work."
  (vm-imap-net-test--in-a-folder-with-spool (mock :messages
                                                  (list vm-imap-net-test--alice))
    (let ((started (float-time)))
      (should-not (vm-check-for-spooled-mail nil t))
      (should (< (- (float-time) started) 0.5))
      (should vm-mail-checks-outstanding))
    ;; and the answer arrives afterwards
    (vm-imap-net-test--settle)
    (should vm-spooled-mail-waiting)
    (should-not vm-mail-checks-outstanding)))

(ert-deftest vm-imap-net-test-the-next-round-reports-the-last-answer ()
  "The round after the answer says there is mail: a check that cannot answer
in the round that started it would otherwise never report anything."
  (vm-imap-net-test--in-a-folder-with-spool (mock :messages
                                                  (list vm-imap-net-test--alice))
    (vm-check-for-spooled-mail nil t)
    (vm-imap-net-test--settle)
    (should (vm-check-for-spooled-mail nil t))))

(ert-deftest vm-imap-net-test-an-empty-maildrop-reports-nothing ()
  "A maildrop with nothing in it answers nil, and the folder says so."
  (vm-imap-net-test--in-a-folder-with-spool (mock :messages nil)
    (vm-check-for-spooled-mail nil t)
    (vm-imap-net-test--settle)
    (should-not vm-spooled-mail-waiting)))

(ert-deftest vm-imap-net-test-a-checked-maildrop-is-not-marked-seen ()
  "A check examines the mailbox rather than selecting it: looking for mail is
not reading it, and a check that marked messages seen would be a check that
changed the thing it was asked about."
  (vm-imap-net-test--in-a-folder-with-spool (mock :messages
                                                  (list vm-imap-net-test--alice))
    (vm-check-for-spooled-mail nil t)
    (vm-imap-net-test--settle)
    (should (vm-imap-mock-received-p mock "EXAMINE"))
    (should-not (vm-imap-mock-received-p mock "\\`vm[0-9]+ SELECT"))
    (should-not (member "\\Seen" (vm-imap-mock-flags mock "INBOX" 1)))))

(ert-deftest vm-imap-net-test-a-maildrop-vm-cannot-ask-is-left-to-the-old-path ()
  "A maildrop that would have to start a program or ask a question is not one
a timer may check, and `vm-imap-net-checkable-p' says so."
  (should (vm-imap-net-checkable-p "imap:host:143:INBOX:login:someone:secret"))
  (should-not (vm-imap-net-checkable-p "imap-ssh:host:143:INBOX:login:me:x"))
  (should-not (vm-imap-net-checkable-p "imap:host:143:INBOX:preauth:me:x"))
  (let ((vm-imap-passwords nil))
    (should-not (vm-imap-net-checkable-p "imap:host:143:INBOX:login:me:*")))
  ;; and one whose password VM has learned is checkable after all
  (let ((vm-imap-passwords (list (list "imap:host:143:*:login:me:*" "secret"))))
    (should (vm-imap-net-checkable-p "imap:host:143:INBOX:login:me:*"))))


;;; Passwords: read, never written

(ert-deftest vm-imap-net-test-a-password-is-remembered-after-the-server-took-it ()
  "The driver writes `vm-imap-passwords' when the login succeeds, and not
before.

The operations that still block look the password up there, so something has
to put it there -- but a wrong or unproven entry is worse than none: the
blocking path finds one, sends it, is refused, and never asks anybody
anything, which is a folder that cannot be logged into and a reader who is
never prompted."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((port (vm-imap-mock-port mock))
           (spec (format "imap:127.0.0.1:%d:INBOX:login:vmtest:secret" port))
           (key (vm-imapdrop-sans-password-and-mailbox spec))
           (vm-imap-passwords nil)
           (opened (vm-imap-net-open spec "remembering"))
           (session (car opened)))
      (setq vm-imap-net-test--buffer (vm-net-session-buffer session))
      ;; nothing yet: the server has not been asked
      (should (null vm-imap-passwords))
      (vm-net-start session (vm-imap-net-open-session (nth 2 opened)
                                                      (nth 3 opened)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (vm-net-session-live-p session) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (eq (vm-net-session-state session) 'done))
      (should (equal (car (cdr (assoc key vm-imap-passwords))) "secret"))
      (let ((process (vm-net-session-process session)))
        (when (process-live-p process) (delete-process process))))))

(ert-deftest vm-imap-net-test-a-refused-password-is-not-remembered ()
  "A login the server refuses leaves the cache alone: an entry there would
be sent again by every operation that still blocks, and none of them would
ask."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice)
                           :refuse "LOGIN")
    (let* ((port (vm-imap-mock-port mock))
           (spec (format "imap:127.0.0.1:%d:INBOX:login:vmtest:wrong" port))
           (vm-imap-passwords nil)
           (opened (vm-imap-net-open spec "refused"))
           (session (car opened)))
      (setq vm-imap-net-test--buffer (vm-net-session-buffer session))
      (vm-net-start session (vm-imap-net-open-session (nth 2 opened)
                                                      (nth 3 opened)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (vm-net-session-live-p session) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (eq (vm-net-session-state session) 'failed))
      (should (null vm-imap-passwords))
      (let ((process (vm-net-session-process session)))
        (when (process-live-p process) (delete-process process))))))

(ert-deftest vm-imap-net-test-a-useless-cached-password-is-not-a-password ()
  "A cache entry that is empty, a `*', or not a string at all is nothing to
log in with, and the maildrop is left to the blocking path -- which can ask."
  (let ((spec "imap:host:143:INBOX:login:someone:*")
        (key "imap:host:143:*:login:someone:*")
        (auth-sources nil))
    (dolist (useless (list nil "" "*" 42))
      (let ((vm-imap-passwords (list (list key useless))))
        (should-error (vm-imap-net-open spec "x")
                      :type 'vm-imap-net-no-password)
        (should-not (vm-imap-net-checkable-p spec))))
    ;; and a real one is enough
    (let ((vm-imap-passwords (list (list key "secret"))))
      (should (vm-imap-net-checkable-p spec)))))


;;; The log says which path took the work

(ert-deftest vm-imap-net-test-not-starting-says-so ()
  "Work that does not start is announced as such, rather than as started.

Without it the log said what VM was about to do rather than what it did: the
line said \"fetching new mail without waiting\" and no fetch happened, which
is how a folder that never updated came to look like a converted one."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((said nil)
          (vm-imap-passwords nil)
          (auth-sources nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (push (apply #'format args) said)))
                ((symbol-function 'vm-folder-imap-maildrop-spec)
                 ;; a password VM has not been told, and nobody to ask
                 (lambda () "imap:host:143:INBOX:login:someone:*")))
        (should-not (vm-imap-net-get-spooled-mail)))
      (should (seq-find (lambda (line)
                          (string-match-p "no password for the maildrop" line))
                        said))
      ;; and it does not claim to be fetching
      (should-not (seq-find (lambda (line)
                              (string-match-p "without waiting" line))
                            said)))))

(ert-deftest vm-imap-net-test-taking-the-work-says-so-after-it-started ()
  "The driver says it has the work once the session is running, so a line
saying so means the session exists."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (push (apply #'format args) said))))
        (should (vm-imap-net-get-spooled-mail)))
      (should (seq-find (lambda (line)
                          (string-match-p "fetching new mail without waiting"
                                          line))
                        said))
      (should (vm-imap-net-busy-p))
      (should (vm-imap-net-wait nil 90)))))


;;; Asking for a password, and not asking

(ert-deftest vm-imap-net-test-a-command-may-ask-for-a-password ()
  "A maildrop whose password VM does not hold is opened by a command asking
for it.  That is where the blocking path was being reached from: a fresh
Emacs holds no passwords, so the first fetch of every session went the old
way and stopped Emacs for as long as it took."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((port (vm-imap-mock-port mock))
           (spec (format "imap:127.0.0.1:%d:INBOX:login:vmtest:*" port))
           (vm-imap-passwords nil)
           (auth-sources nil)
           (vm-imap-ok-to-ask t)
           (asked 0))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq asked (1+ asked)) "secret")))
        (let* ((opened (vm-imap-net-open spec "asking" 'may-ask))
               (session (car opened)))
          (setq vm-imap-net-test--buffer (vm-net-session-buffer session))
          (should (equal asked 1))
          (should (equal (nth 3 opened) "secret"))
          (let ((process (vm-net-session-process session)))
            (when (process-live-p process) (delete-process process))))))))

(ert-deftest vm-imap-net-test-a-timer-never-asks-for-a-password ()
  "The check runs on a timer, and a question from a timer arrives while
somebody is typing something else.  It declines instead, and the blocking
path -- which the reader started -- can ask."
  (let ((spec "imap:host:143:INBOX:login:someone:*")
        (vm-imap-passwords nil)
        (auth-sources nil)
        (vm-imap-ok-to-ask t)
        (asked 0))
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _) (setq asked (1+ asked)) "secret")))
      ;; no MAY-ASK: this is what the check passes
      (should-error (vm-imap-net-open spec "checking")
                    :type 'vm-imap-net-no-password)
      (should (equal asked 0)))))

(ert-deftest vm-imap-net-test-asking-does-not-need-ok-to-ask-bound ()
  "`vm-imap-ok-to-ask' is nil unless something has bound it, and nothing
binds it on the way to a fetch.  Requiring it meant the question was never
put: the driver declined, the blocking path asked, and the reader waited for
the server with Emacs stopped -- which is what a log of three attempts
showed, each one saying \"password not remembered\"."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((port (vm-imap-mock-port mock))
           (spec (format "imap:127.0.0.1:%d:INBOX:login:vmtest:*" port))
           (vm-imap-passwords nil)
           (auth-sources nil)
           (vm-imap-ok-to-ask nil)   ; its default, and what a command sees
           (asked 0))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq asked (1+ asked)) "secret")))
        (let* ((opened (vm-imap-net-open spec "asking" 'may-ask))
               (session (car opened)))
          (setq vm-imap-net-test--buffer (vm-net-session-buffer session))
          (should (equal asked 1))
          (should (equal (nth 3 opened) "secret"))
          (let ((process (vm-net-session-process session)))
            (when (process-live-p process) (delete-process process))))))))


;;; An empty mailbox

(ert-deftest vm-imap-net-test-an-empty-mailbox-is-not-an-error ()
  "A mailbox with nothing in it has nothing to fetch, and saying so is not
the same as failing.

The plan went through `vm-imap-get-synchronization-data', which asks the
server for the UID list itself unless one is already there -- and an empty
mailbox's list is empty, so it asked, through the process a blocking session
would have had.  A session on the driver has no such process, and died of it:
every visit to an empty IMAP folder failed with `processp nil'."
  (vm-imap-net-test--visiting (mock)
    (should (null vm-message-list))
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (null vm-message-list))
    (should (eq (vm-net-session-state vm-imap-net-session) 'done))))

(ert-deftest vm-imap-net-test-an-empty-mailbox-checks-clean ()
  "The check on an empty mailbox answers no mail, rather than failing."
  (vm-imap-net-test--visiting (mock)
    (setq vm-spooled-mail-waiting t)
    (should (vm-check-for-spooled-mail nil t))
    (should (vm-imap-net-wait nil 90))
    (should-not vm-spooled-mail-waiting)
    (should (eq (vm-net-session-state vm-imap-net-session) 'done))))

(ert-deftest vm-imap-net-test-a-mailbox-emptied-behind-us-expunges-here ()
  "Every message gone from the server is gone from the folder, which is the
same answer the synchronisation data gives for a mailbox that still holds
something."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (should (equal (length vm-message-list) 2))
    (dolist (message (vm-imap-mock-messages mock "INBOX"))
      (setf (vm-imap-mock-message-expunged message) t))
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (null vm-message-list))))


;;; Taking the mail in as it arrives

(ert-deftest vm-imap-net-test-messages-are-taken-in-as-they-arrive ()
  "The folder shows what has arrived while the rest is still coming.

Taking two thousand messages into the message list at once, threading them
and rebuilding the summary, is several seconds in which Emacs answers
nothing -- the freeze the conversion removes, arriving from the other side.
So each bunch is taken in as it lands.

Counted at the point where it happens rather than by watching from outside:
against the mock the whole fetch finishes inside one `accept-process-output',
which is exactly the answer a real server does not give."
  (let ((vm-imap-message-bunch-size 2)
        (sizes nil))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 6)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: s%d@example.com\nSubject: m%d\n\nBody %d.\n" i i i)))
      (let ((real (symbol-function 'vm-imap-net-assimilate)))
        (cl-letf (((symbol-function 'vm-imap-net-assimilate)
                   (lambda (&rest args)
                     (let ((answer (apply real args)))
                       (push (length vm-message-list) sizes)
                       answer))))
          (should (equal (vm-imap-net-test--get-mail mock) 6))))
      (should (equal (length vm-message-list) 6))
      ;; three bunches, and the folder held two, then four, then six
      (should (equal (nreverse sizes) '(2 4 6))))))

(ert-deftest vm-imap-net-test-every-message-still-gets-its-own-uid ()
  "Bunch by bunch, each message is still paired with the entry it was
fetched for: the pairing is by position, and a slice taken at the wrong
offset would give message N the UID of message N minus a bunch."
  (let ((vm-imap-message-bunch-size 3))
    (vm-imap-net-test--visiting (mock)
      (dotimes (i 7)
        (vm-imap-mock-add-message
         mock "INBOX"
         (format "From: s%d@example.com\nSubject: number %d\n\nBody %d.\n" i i i)))
      (should (equal (vm-imap-net-test--get-mail mock) 7))
      (should (equal (length vm-message-list) 7))
      ;; the mock hands out UIDs 1..7 in order, and the subjects are in order
      (let ((n 0))
        (dolist (message vm-message-list)
          (setq n (1+ n))
          (should (equal (vm-imap-uid-of message) (number-to-string n)))
          (should (equal (vm-su-subject message)
                         (format "number %d" (1- n)))))))))


(ert-deftest vm-imap-net-test-a-password-once-given-is-not-asked-for-again ()
  "A password typed for one fetch serves the next.

Reported as \"password is not saved between getting mail\".  What the log
says when it happens is either \"asking for a password, VM has none\" or
\"forgetting the password for ...\", and those want different fixes: one is a
password that was never remembered, the other one that was thrown away."
  (vm-imap-mock-with (mock :messages (list vm-imap-net-test--alice))
    (let* ((port (vm-imap-mock-port mock))
           (spec (format "imap:127.0.0.1:%d:INBOX:login:vmtest:*" port))
           (vm-imap-passwords nil)
           (auth-sources nil)
           (asked 0))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq asked (1+ asked)) "secret")))
        (dolist (_round '(1 2))
          (let* ((opened (vm-imap-net-open spec "twice" 'may-ask))
                 (session (car opened))
                 (buffer (vm-net-session-buffer session)))
            (vm-net-start session (vm-imap-net-open-session (nth 2 opened)
                                                           (nth 3 opened)))
            (let ((deadline (+ (float-time) 10)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (vm-net-session-state session) 'done))
            (let ((process (vm-net-session-process session)))
              (when (process-live-p process) (delete-process process)))
            (when (buffer-live-p buffer) (kill-buffer buffer)))))
      ;; asked once, for two logins
      (should (equal asked 1)))))


;;; What the reader costs

(ert-deftest vm-imap-net-test-the-reader-is-not-a-hundred-times-slower ()
  "Reading a response line costs a fraction of a millisecond, not 100 of them.

A single `unwind-protect' inside `vm-imap-net-read-object' cost 100
milliseconds a response line -- generator.el re-establishes one on every
resume, and that is the hottest generator VM has.  Fetching 400 messages from
a server on this machine took 82 seconds of CPU because of it.  With the
form gone it is a quarter of a millisecond a line.

Timed rather than counted, since what went wrong was a constant factor and
nothing else would have shown it.  The bound is loose: 200 lines in under a
second, where the pathology took twenty.  It used to be checked against the
blocking reader as well, which is gone."
  (let* ((lines 200)
         (response (with-temp-buffer
                     (dotimes (i lines)
                       (insert (format "* %d FETCH (UID %d RFC822.SIZE %d FLAGS (\\Seen))\r\n"
                                       (1+ i) (1+ i) (+ 500 i))))
                     (insert "vm1 OK FETCH completed\r\n")
                     (buffer-string)))
         (driven 0))
    (with-temp-buffer
      (insert response)
      (vm-imap-net-init)
      (setq vm-imap-current-tag "vm1")
      (let ((start (float-time))
            (iterator (vm-imap-net-test--read-lines lines)))
        (condition-case nil
            (while t (iter-next iterator))
          (iter-end-of-sequence nil))
        (setq driven (- (float-time) start))))
    (should (> driven 0))
    (should (< driven 1.0))))

(iter-defun vm-imap-net-test--read-lines (n)
  "Read N response lines through the driver's reader."
  (let ((i 0))
    (while (< i n)
      (vm-imap-net-read-a-response)
      (setq i (1+ i)))))

(ert-deftest vm-imap-net-test-running-from-source-is-said-once ()
  "An uncompiled VM says so, once, rather than being slow silently.

Interpreted generators rebuild their closures on every call: a thousand
messages that take 0.8 seconds compiled take three minutes from source, in
pauses of tens of seconds.  A reader testing that would conclude the
asynchronous path blocks."
  (let ((vm-imap-net-said-it-is-uncompiled nil)
        (vm-verbosity 5)
        (vm-verbal-time 0)
        (said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _seconds &rest args)
                 (push (apply #'format args) said)))
              ((symbol-function 'vm-imap-net-parse-object)
               ;; an interpreted closure, which is what loading from source
               ;; leaves behind
               (eval '(lambda () nil) t)))
      (vm-imap-net-check-compiled)
      (vm-imap-net-check-compiled)
      (should (equal (length said) 1))
      (should (string-match-p "byte-compiled" (car said))))
    ;; and nothing to say when it is compiled
    (let ((vm-imap-net-said-it-is-uncompiled nil)
          (quiet nil))
      (cl-letf (((symbol-function 'vm-warn)
                 (lambda (&rest _) (setq quiet 'spoke))))
        (vm-imap-net-check-compiled)
        (when (byte-code-function-p (symbol-function 'vm-imap-net-parse-object))
          (should-not quiet))))))

(ert-deftest vm-imap-net-test-a-uid-the-folder-has-is-not-fetched-again ()
  "A UID the folder holds is left out of the plan, loudly, and the rest fetched.

Two of a UID in one folder is the same message twice: two summary lines, two
copies in the file, and every lookup by UID reaching whichever comes first.
The fetch is not abandoned over it -- the other messages are new mail."
  (let ((said nil))
    (vm-imap-net-test--visiting (mock)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
      (should (equal (vm-imap-net-test--get-mail mock) 2))
      ;; the folder forgets it has the first, as a folder whose plan was made
      ;; before something else took the message in would have it
      (let ((uid (vm-imap-uid-of (car vm-message-list))))
        (cl-letf (((symbol-function 'vm-imap-get-synchronization-data)
                   (lambda (&rest _)
                     (list (list (cons uid 1)) nil nil))))
          (cl-letf (((symbol-function 'vm-warn)
                     (lambda (_level _seconds &rest args)
                       (push (apply #'format args) said))))
            (should (equal (vm-imap-net-test--get-mail mock) 0)))))
      ;; nothing arrived, nothing was said twice over, and the folder is as it
      ;; was: two messages, two UIDs
      (should (equal (length vm-message-list) 2))
      (should (equal (length (delete-dups
                              (mapcar #'vm-imap-uid-of vm-message-list)))
                     2))
      (should (equal (length said) 1))
      (should (string-match-p "has already" (car said)))
      (should-not (vm-imap-mock-received-p mock "UID FETCH")))))

(ert-deftest vm-imap-net-test-a-uid-listed-twice-is-fetched-once ()
  "A server that lists a UID twice gets one fetch of it, and says so."
  (let ((said nil))
    (vm-imap-net-test--visiting (mock)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
      (cl-letf (((symbol-function 'vm-imap-get-synchronization-data)
                 (lambda (&rest _)
                   (list (list (cons "1" 1) (cons "1" 1)) nil nil)))
                ((symbol-function 'vm-warn)
                 (lambda (_level _seconds &rest args)
                   (push (apply #'format args) said))))
        (should (equal (vm-imap-net-test--get-mail mock) 1)))
      (should (equal (length vm-message-list) 1))
      (should (equal (length said) 1))
      (should (string-match-p "twice" (car said))))))

(ert-deftest vm-imap-net-test-a-uid-that-arrives-late-is-not-written-twice ()
  "A message that reaches the folder while its own fetch runs is not written.

The plan said the UID was new; by the time the text arrived the folder had
it.  Writing it would put the message in twice, so it is left out and said
out loud -- which is `vm-imap-net-uid-held-p' at the point of writing, the
plan's own check having been made before any of this was asked for."
  (let ((said nil))
    (vm-imap-net-test--visiting (mock)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
      (cl-letf (((symbol-function 'vm-imap-net-uid-held-p)
                 ;; UID 1 turns up in the folder mid-fetch
                 (lambda (uid) (equal uid "1")))
                ((symbol-function 'vm-warn)
                 (lambda (_level _seconds &rest args)
                   (push (apply #'format args) said))))
        ;; the answer counts what was asked for, which is the offset the
        ;; bunches are taken at; what arrived is what the folder holds
        (should (equal (vm-imap-net-test--get-mail mock) 2)))
      (should (equal (length vm-message-list) 1))
      (should (equal (vm-imap-uid-of (car vm-message-list)) "2"))
      (should (equal (length said) 1))
      (should (string-match-p "not written twice" (car said))))))

(defun vm-imap-net-test--flags-on-the-server (mock)
  "What flags each message in MOCK's INBOX carries, as (UID . FLAGS)."
  (mapcar (lambda (m)
            (cons (vm-imap-mock-message-uid m)
                  (sort (mapcar #'downcase (vm-imap-mock-message-flags m))
                        #'string-lessp)))
          (vm-imap-mock-messages mock "INBOX")))

(defun vm-imap-net-test--mark-read-after-a-shift ()
  "Mark the folder's second message read after the mailbox has shifted.
Another client expunges the first message, so every sequence number VM holds
is one too high.  Answers with the server's flags afterwards."
  (let ((answer nil))
    (vm-imap-net-test--visiting (mock :messages
                                      (list "From: a@example.com\nSubject: one\n\nOne.\n"
                                            "From: b@example.com\nSubject: two\n\nTwo.\n"
                                            "From: c@example.com\nSubject: three\n\nThree.\n"))
      (should (equal (length vm-message-list) 3))
      ;; another client deletes the first message, after VM has read the
      ;; mailbox and before it sends anything
      (setf (vm-imap-mock-message-expunged
             (car (vm-imap-mock-messages mock "INBOX")))
            t)
      (let ((second (nth 1 vm-message-list)))
        (should (equal (vm-imap-uid-of second) "2"))
        ;; VM still has UID 2 as the mailbox's second message, which it is
        ;; no longer
        (should (equal (vm-folder-imap-uid-msn "2") 2))
        (vm-set-unread-flag second nil)
        (vm-set-attribute-modflag-of second t))
      (should (vm-imap-net-save-attributes))
      (vm-imap-net-wait nil 90)
      (setq answer (vm-imap-net-test--flags-on-the-server mock)))
    answer))

(ert-deftest vm-imap-net-test-a-flag-lands-on-the-message-it-was-meant-for ()
  "Marking a message read marks that message, after the mailbox has shifted.

The sequence numbers VM holds are the ones the mailbox had when it last read
it.  Another client expunging a message shifts every number after it down, and
the server is told nothing until the next command; a STORE by number then
reaches whatever is at that position now.  Marking VM's second message read
set \\Seen on the third.

It is sent as UID STORE, so the number the folder cached cannot target a
stranger; a UID the mailbox no longer has matches nothing."
  (let ((flags (vm-imap-net-test--mark-read-after-a-shift)))
    ;; UID 1 is gone; UID 2 is the one that was marked, UID 3 untouched
    (should (equal flags '((2 "\\seen") (3))))))


;;; Fetching what the folder was given once and no longer holds (#751)

(ert-deftest vm-imap-net-test-a-full-fetch-brings-back-what-the-record-names ()
  "A message the record names and the folder has not got is fetched again
only when it is asked for.

`vm-imap-retrieved-messages' is what stops a message deleted here from
arriving again, and the same record leaves a folder short when the cache lost
the message some other way -- a partial restore.  Two prefix arguments to
`vm-get-new-mail' say so, and reach the plan as FULL-RETRIEVE."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (should (equal (vm-imap-net-test--get-mail mock) 1))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    ;; the folder is told it fetched the second message once already, which
    ;; is what its own `X-VM-IMAP-Retrieved' header says after a cache that
    ;; had it was restored from a backup that had not
    (setq vm-imap-retrieved-messages
          (list (list "2" (vm-folder-imap-uid-validity)
                      (vm-imapdrop-sans-password (vm-imap-mock-spec mock))
                      'uid)))
    ;; so an ordinary fetch passes it over and the folder stays short
    (should (equal (vm-imap-net-test--get-mail mock) 0))
    (should (equal (length vm-message-list) 1))
    ;; and asking for it brings it
    (should (equal (vm-imap-net-test--get-mail mock 10 t) 1))
    (should (equal (length vm-message-list) 2))
    (should (string-match-p "The second body"
                            (vm-imap-net-test--body-of (cadr vm-message-list))))))

(ert-deftest vm-imap-net-test-two-prefix-arguments-reach-the-driver ()
  "`vm-get-spooled-mail' hands its FULL to the driver rather than dropping it
on the way: the asynchronous path is what an IMAP folder takes, so a fetch
that ignored it would leave the command doing nothing at all."
  (let (asked)
    (vm-imap-net-test--visiting (mock)
      (cl-letf (((symbol-function 'vm-imap-net-get-mail)
                 (lambda (_source _callback &optional _may-ask full-retrieve)
                   (setq asked (cons full-retrieve asked))
                   nil)))
        (vm-get-spooled-mail nil nil)
        (vm-get-spooled-mail nil t))
      (should (equal (nreverse asked) '(nil t))))))

;;; The table of UIDs the folder already holds

(defun vm-imap-net-test--message-holding (uid validity)
  "A message carrying UID under VALIDITY, as one read from a folder does."
  (let ((message (vm-make-message)))
    (vm-test-init-message-data message)
    (vm-set-imap-uid-of message uid)
    (vm-set-imap-uid-validity-of message validity)
    message))

(defun vm-imap-net-test--append-held (uid)
  "Put a message for UID at the end of the folder's list, as an arrival is put.
Appending and not rewriting, which is the case the held-UID table follows
without being filled again."
  (let ((message (vm-imap-net-test--message-holding uid "7")))
    (if vm-message-list
        (setcdr (vm-last vm-message-list) (list message))
      (setq vm-message-list (list message)))
    message))

(defmacro vm-imap-net-test--in-a-folder-holding (uids &rest body)
  "Run BODY in a buffer that is an IMAP folder holding UIDS, validity \"7\"."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (vm-test-init-folder-variables)
     (setq vm-folder-access-method 'imap
           vm-folder-access-data (make-vector vm-folder-imap-access-data-length
                                              nil))
     (aset vm-folder-access-data 2 "7")
     (dolist (uid ,uids)
       (vm-imap-net-test--append-held uid))
     ,@body))

(ert-deftest vm-imap-net-test-the-held-uid-table-follows-an-append ()
  "A message appended to the folder is held, and the table is not built again.

A fetch does not block, so the reader can save a message into the folder while
the fetch for that same UID is in flight; the answer has to see it or the
message is written twice."
  (vm-imap-net-test--in-a-folder-holding '("1")
    (should (vm-imap-net-uid-held-p "1"))
    (should-not (vm-imap-net-uid-held-p "2"))
    (let ((table (vm-imap-net-uids-held)))
      (vm-imap-net-test--append-held "2")
      (should (vm-imap-net-uid-held-p "2"))
      ;; the same table, extended: nothing moved the generation
      (should (eq table (vm-imap-net-uids-held))))))

(ert-deftest vm-imap-net-test-the-held-uid-table-is-built-again-when-one-leaves ()
  "A message taken out of the folder is no longer held.
Removing one moves `vm-message-list-generation\\=', which is what says the
table can no longer be extended and has to be filled from nothing."
  (vm-imap-net-test--in-a-folder-holding '("1" "2")
    (let ((table (vm-imap-net-uids-held)))
      (should (vm-imap-net-uid-held-p "1"))
      ;; as an expunge does it: the message is spliced out and the generation
      ;; moves
      (setq vm-message-list (cdr vm-message-list))
      (vm-increment vm-message-list-generation)
      (should-not (vm-imap-net-uid-held-p "1"))
      (should (vm-imap-net-uid-held-p "2"))
      (should-not (eq table (vm-imap-net-uids-held))))))

(ert-deftest vm-imap-net-test-the-held-uid-table-drops-another-validity ()
  "UIDs of one UIDVALIDITY are not the folder's UIDs under the next.
A mailbox that has changed its validity has renumbered every message, so the
table is filled again and the old UIDs are gone from it."
  (vm-imap-net-test--in-a-folder-holding '("1")
    (should (vm-imap-net-uid-held-p "1"))
    (aset vm-folder-access-data 2 "8")
    (should-not (vm-imap-net-uid-held-p "1"))))

(ert-deftest vm-imap-net-test-the-held-uid-table-reads-each-message-once ()
  "Asking per arriving message reads each of the folder's messages once.
The check ran once per message and built the whole table each time, which is
quadratic in the mailbox: 6501 tables of up to 6505 entries and 0.95s of a
6.8s fetch, on the folder in issue #742."
  (let* ((reads 0)
         (note (symbol-function 'vm-imap-net-note-held-uid)))
    (cl-letf (((symbol-function 'vm-imap-net-note-held-uid)
               (lambda (message)
                 (setq reads (1+ reads))
                 (funcall note message))))
      (vm-imap-net-test--in-a-folder-holding '("1" "2" "3")
        (should-not (vm-imap-net-uid-held-p "4"))
        (should (equal reads 3))
        ;; a bunch arrives, and the next question reads only what it added
        (vm-imap-net-test--append-held "4")
        (should (vm-imap-net-uid-held-p "4"))
        (should (equal reads 4))
        ;; and a question with nothing new to see reads nothing
        (should-not (vm-imap-net-uid-held-p "5"))
        (should (equal reads 4))
        (vm-imap-net-test--append-held "5")
        (vm-imap-net-test--append-held "6")
        (should (vm-imap-net-uid-held-p "6"))
        ;; six messages read for four questions; building the table each time
        ;; would have read seventeen
        (should (equal reads 6))))))

(ert-deftest vm-imap-net-test-a-uid-given-after-the-list-is-not-missed ()
  "A message put into the list before it had a UID is held once it has one.
Both fetch paths append the messages and then give them their UIDs, so the
table can read a message that has none yet; the UID has to reach it anyway, or
the next fetch asks for a message the folder holds and writes it twice."
  (vm-imap-net-test--in-a-folder-holding '("1")
    ;; the table reads the list, and this message has nothing to give it
    (let ((late (vm-imap-net-test--message-holding nil nil)))
      (should-not (vm-imap-net-uid-held-p "2"))
      (setcdr (vm-last vm-message-list) (list late))
      (should-not (vm-imap-net-uid-held-p "2"))
      ;; and now it is what a fetch has just written
      (vm-set-imap-uid-of late "2")
      (vm-set-imap-uid-validity-of late "7")
      (vm-imap-net-note-held-message late)
      (should (vm-imap-net-uid-held-p "2")))))

(ert-deftest vm-imap-net-test-forgetting-the-held-uids-reads-the-list-again ()
  "Forgetting the table makes the next question read the whole list again.
What the blocking fetch does, having appended messages and given them their
UIDs without asking the table anything."
  (vm-imap-net-test--in-a-folder-holding '("1")
    (let ((late (vm-imap-net-test--message-holding nil nil)))
      (setcdr (vm-last vm-message-list) (list late))
      (should-not (vm-imap-net-uid-held-p "2"))
      (vm-set-imap-uid-of late "2")
      (vm-set-imap-uid-validity-of late "7")
      (vm-imap-net-forget-held-uids)
      (should (vm-imap-net-uid-held-p "2"))
      (should (vm-imap-net-uid-held-p "1")))))

(ert-deftest vm-imap-net-test-a-fetch-tells-the-held-uid-table-what-it-wrote ()
  "A fetch puts what it wrote into the held-UID table as it goes.
Read straight out of the table, without asking `vm-imap-net-uids-held' --
which would walk the list and find the message whether the fetch had said
anything or not.  The table is there because the plan built it."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (should (equal (vm-imap-net-test--get-mail mock) 1))
    (should vm-imap-net-held-uids)
    (should (gethash "1" vm-imap-net-held-uids))))

(ert-deftest vm-imap-net-test-a-partly-arrived-line-asks-for-more ()
  "A response line that is not all here answers with a request, and moves
nothing: the parse starts again from the beginning of the line once the
request is satisfied, so there is nothing to resume and no state to keep."
  (with-temp-buffer
    (vm-imap-net-init)
    (insert "* 1 FETCH (UID 7 FLA")
    (let ((read-point vm-imap-net-read-point)
          (answer (vm-imap-net-parse-response)))
      (should (functionp answer))
      (should-not (funcall answer))
      (should (equal vm-imap-net-read-point read-point))
      ;; the rest of it arrives
      (goto-char (point-max))
      (insert "GS (\\Seen))\r\n")
      (should (funcall answer))
      (let ((tokens (vm-imap-net-parse-response)))
        (should (vm-imap-response-matches tokens '* 'atom 'FETCH 'list))
        (should (> vm-imap-net-read-point read-point))))))

(ert-deftest vm-imap-net-test-a-split-literal-is-waited-for-once ()
  "A literal's octets are waited for by position, not by whatever arrives.
The count comes before the octets, so the wait knows where they end: a body
arriving in a thousand chunks is one wait and one parse, where a wait for the
buffer merely growing would be a thousand of each."
  (with-temp-buffer
    (vm-imap-net-init)
    (insert "* 1 FETCH (UID 7 BODY[] {20}\r\n12345")
    (let ((answer (vm-imap-net-parse-response)))
      (should (functionp answer))
      (should-not (funcall answer))
      ;; more of it, and still not enough: the buffer growing is not the
      ;; question being asked
      (goto-char (point-max))
      (insert "67890")
      (should-not (funcall answer))
      (goto-char (point-max))
      (insert "1234567890")
      (should (funcall answer)))))

(ert-deftest vm-imap-net-test-a-response-line-costs-almost-nothing ()
  "Reading a response line allocates a little and not a lot.

Every token used to be read by a generator of its own: 340,000 of them to
fetch 6500 messages, of which 2,000 reads ever waited for anything, and a
response line cost 3775 conses and 23,279 vector words where parsing the
tokens costs 84 and 112.  That allocation was what the garbage collections in
a fetch were -- 323M vector words for 13MB of mail, nine collections and a
worst pause of 0.618s (issue #742).

Bounds rather than figures, and loose ones: a hundred vector words and two
hundred conses a line, against the twenty-three thousand and the three
thousand seven hundred that were there.  Allocation and not time, because a
count does not depend on how busy the machine is.

Not under instrumentation: edebug allocates per form, which is most of what
the count then measures (emacs-vm/vm#870)."
  (skip-unless (not vm-test-instrumented))
  (let* ((lines 200)
         (text (with-temp-buffer
                 (dotimes (i lines)
                   (insert (format "* %d FETCH (UID %d RFC822.SIZE %d FLAGS (\\Seen))\r\n"
                                   (1+ i) (1+ i) (+ 500 i))))
                 (buffer-string))))
    (with-temp-buffer
      (insert text)
      (vm-imap-net-init)
      (setq vm-imap-current-tag "vm1")
      (garbage-collect)
      (let ((before (memory-use-counts))
            (iterator (vm-imap-net-test--read-lines lines)))
        (condition-case nil
            (while t (iter-next iterator))
          (iter-end-of-sequence nil))
        (let* ((after (memory-use-counts))
               (conses (- (nth 0 after) (nth 0 before)))
               (words (- (nth 2 after) (nth 2 before))))
          (should (< words (* 100 lines)))
          (should (< conses (* 200 lines))))))))

;;; CRAM-MD5 (emacs-vm/vm#822)
;;
;; The blocking implementation spoke CRAM-MD5 and the driver did not, so a
;; maildrop asking for it fell back to blocking on every fetch.  That, and
;; APOP and RPOP on the POP side, is why the blocking driver could not
;; simply be deleted.

(iter-defun vm-imap-net-test--login-cram-md5 (user password)
  "Greet and log in with CRAM-MD5, through the dispatch production uses.
`vm-imap-net-auth' is buffer-local to the session buffer and set by
`vm-imap-net-open', which this harness does not go through, so it is set
here.  Calling `vm-imap-net-authenticate-cram-md5' directly would skip the
greeting and read it as the challenge."
  (setq vm-imap-net-auth "cram-md5")
  (iter-yield-from (vm-imap-net-open-session user password)))

(ert-deftest vm-imap-net-test-a-session-authenticates-with-cram-md5 ()
  "REGRESSION: the driver logs in with CRAM-MD5.

RFC 2195: the server challenges, the client answers with its user name and
the HMAC-MD5 of that challenge keyed by the password.  The mock checks the
digest, so a wrong answer fails here rather than merely a missing one."
  (vm-imap-net-test--with-session (mock :cram-md5 t)
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login-cram-md5 "vmtest" "secret"))))
      (should (eq (vm-net-session-state session) 'done))
      ;; the capabilities come back, which happens only after the server has
      ;; accepted the authentication
      (should (memq 'IMAP4REV1 (car (vm-net-session-value session))))
      (should (vm-imap-mock-received-p mock "AUTHENTICATE CRAM-MD5"))
      (should (vm-imap-mock-authenticated mock)))))

(ert-deftest vm-imap-net-test-a-wrong-cram-md5-password-is-refused ()
  "A digest the server does not accept ends the session rather than proceeding."
  (vm-imap-net-test--with-session (mock :cram-md5 t :password "secret")
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login-cram-md5 "vmtest" "wrong"))))
      (should-not (eq (vm-net-session-state session) 'done))
      (should (vm-imap-mock-received-p mock "AUTHENTICATE CRAM-MD5"))
      (should-not (vm-imap-mock-authenticated mock)))))

(ert-deftest vm-imap-net-test-the-password-is-not-echoed-into-the-buffer ()
  "REGRESSION: the answer to the challenge is not written to the transcript.
The session buffer is kept for the trace `vm-imap-submit-bug-report' sends,
and the answer is a credential derived from the password."
  (vm-imap-net-test--with-session (mock :cram-md5 t)
    (vm-imap-net-test--run
     mock (vm-imap-net-test--login-cram-md5 "vmtest" "secret"))
    (with-current-buffer vm-imap-net-test--buffer
      (let ((transcript (buffer-string)))
        (should (string-match-p "AUTHENTICATE CRAM-MD5" transcript))
        (should (string-match-p "authentication response omitted" transcript))))))

(ert-deftest vm-imap-net-test-a-cram-md5-maildrop-is-not-declined ()
  "REGRESSION: `vm-imap-net-open' no longer refuses a CRAM-MD5 maildrop.
It signalled `vm-imap-net-no-password' for any auth but login, and that
signal is what sent the work to the blocking path."
  (vm-imap-mock-with (mock :cram-md5 t)
    (let ((opened (vm-imap-net-open
                   (vm-imap-mock-spec mock "INBOX" "cram-md5")
                   "cram-md5 test" nil)))
      (should opened)
      (let ((process (vm-net-session-process (nth 0 opened))))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p (vm-net-session-buffer (nth 0 opened)))
          (kill-buffer (vm-net-session-buffer (nth 0 opened)))))))
  ;; an auth VM does not know is an error the reader sees, naming what to
  ;; write instead.  There is no other path for it to be handed to.
  (vm-imap-mock-with (mock)
    (let* ((text-quoting-style 'grave)
           (message (cadr (should-error
                           (vm-imap-net-open
                            (vm-imap-mock-spec mock "INBOX" "kerberos_v4")
                            "unknown auth" nil)))))
      (should (string-match-p "kerberos_v4" message))
      (should (string-match-p "login, cram-md5 or preauth" message)))))

;;; The message size limit, on the driver (emacs-vm/vm#822)
;;
;; `vm-imap-max-message-size' with `vm-enable-external-messages' naming imap
;; means a message over the limit is fetched as headers only, its body left
;; on the server.  The coverage for this was against the blocking path, which
;; is going, and the driver had none -- so it is written here first, and the
;; blocking tests go after.

(ert-deftest vm-imap-net-test-a-message-over-the-limit-comes-as-headers ()
  "REGRESSION: a message larger than the limit is fetched headers-only.
The body is left on the server, which is what `vm-enable-external-messages'
asks for.  BODY.PEEK[HEADER] rather than BODY.PEEK[] is how the wire shows
it."
  (let ((vm-imap-max-message-size 10)
        (vm-enable-external-messages '(imap)))
    (vm-imap-net-test--visiting (mock)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
      (vm-imap-mock-forget-commands mock)
      (should (equal (vm-imap-net-test--get-mail mock) 2))
      (should (equal (length vm-message-list) 2))
      (should (vm-imap-mock-received-p mock "BODY.PEEK\\[HEADER\\]")))))

(ert-deftest vm-imap-net-test-the-limit-means-nothing-without-external-messages ()
  "REGRESSION: the limit is ignored unless external messages are enabled.
The whole message comes down, since there is nowhere to leave a body: VM
would have a folder of headers with no way to read any of them."
  (let ((vm-imap-max-message-size 10)
        (vm-enable-external-messages nil))
    (vm-imap-net-test--visiting (mock)
      (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
      (vm-imap-mock-forget-commands mock)
      (should (equal (vm-imap-net-test--get-mail mock) 1))
      (should-not (vm-imap-mock-received-p mock "BODY.PEEK\\[HEADER\\]")))))

;;; A keyword the server takes and discards (emacs-vm/vm#601)

(defmacro vm-imap-net-test--warnings (&rest body)
  "Run BODY and answer with what it warned about, newest last.
The driver warns with `vm-net-warn', which goes through `vm-warn'."
  (declare (indent 0) (debug t))
  `(let ((said nil))
     (cl-letf (((symbol-function 'vm-warn)
                (lambda (_level _seconds format &rest args)
                  (push (apply #'format format args) said))))
       ,@body)
     (nreverse said)))

(iter-defun vm-imap-net-test--store-flags (sign uid flags)
  "Log in, select INBOX and store FLAGS on UID, answering what was accepted."
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (iter-yield-from (vm-imap-net-select "INBOX"))
  (let ((accepted (iter-yield-from (vm-imap-net-store-flags sign uid flags))))
    (vm-imap-net-logout)
    accepted))

(ert-deftest vm-imap-net-test-a-discarded-keyword-is-reported ()
  "A server that answers OK to a keyword and does not keep it is complained
about, naming the keyword.  This is Gmail: a VM label is an IMAP keyword, and
Gmail takes the STORE, says OK, and stores nothing, so the label was lost with
nothing said at any point."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice) :drops-keywords t)
    (let ((said (vm-imap-net-test--warnings
                  (vm-imap-net-test--run
                   mock (vm-imap-net-test--store-flags "+" "1" '("important"))))))
      (should (= (length said) 1))
      (should (string-match-p "accepted and discarded" (car said)))
      (should (string-match-p "important" (car said))))
    ;; and the server really does not have it, which is the thing being detected
    (should-not (member "important" (vm-imap-mock-flags mock "INBOX" 1)))))

(ert-deftest vm-imap-net-test-a-kept-keyword-is-not-reported ()
  "A server that keeps the keyword is not complained about.  The check has to
be silent in the ordinary case, since it runs on every label VM stores."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice))
    (let ((said (vm-imap-net-test--warnings
                  (vm-imap-net-test--run
                   mock (vm-imap-net-test--store-flags "+" "1" '("important"))))))
      (should (equal said nil)))
    (should (member "important" (vm-imap-mock-flags mock "INBOX" 1)))))

(ert-deftest vm-imap-net-test-a-discarded-keyword-is-reported-once ()
  "Said once per session, not once per message: a folder of a thousand
messages carrying the same label would otherwise complain a thousand times."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice vm-imap-net-test--bob)
            :drops-keywords t)
    (let ((said (vm-imap-net-test--warnings
                  (vm-imap-net-test--run
                   mock (vm-imap-net-test--store-two-flags)))))
      (should (= (length said) 1)))))

(iter-defun vm-imap-net-test--store-two-flags ()
  "Store the same keyword on two messages of one session."
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (iter-yield-from (vm-imap-net-select "INBOX"))
  (iter-yield-from (vm-imap-net-store-flags "+" "1" '("important")))
  (iter-yield-from (vm-imap-net-store-flags "+" "2" '("important")))
  (vm-imap-net-logout))

(ert-deftest vm-imap-net-test-only-a-keyword-store-asks-for-the-flags-back ()
  "The protocol's own flags are stored with `.SILENT', so the ordinary
business of marking messages read costs no extra response; a store carrying a
keyword asks, because a keyword is what a server may discard."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice))
    (vm-imap-net-test--warnings
      (vm-imap-net-test--run
       mock (vm-imap-net-test--store-flags "+" "1" '("\\Seen"))))
    (should (seq-find (lambda (line) (string-match-p "FLAGS\\.SILENT" line))
                      (vm-imap-mock-log mock)))
    (vm-imap-net-test--warnings
      (vm-imap-net-test--run
       mock (vm-imap-net-test--store-flags "+" "1" '("important"))))
    (should (seq-find (lambda (line)
                        (and (string-match-p "STORE" line)
                             (string-match-p "+FLAGS (important)" line)))
                      (vm-imap-mock-log mock)))))

(ert-deftest vm-imap-net-test-a-dropped-keyword-is-still-offered ()
  "Unlike a refused flag, a discarded one is sent again.  A refusal is an
error the server means; this is a mailbox that cannot hold keywords, and one
that gains the ability should start working without restarting Emacs."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice) :drops-keywords t)
    (vm-imap-net-test--warnings
      (vm-imap-net-test--run
       mock (vm-imap-net-test--store-flags "+" "1" '("important"))))
    (should-not (member "important" (vm-imap-mock-flags mock "INBOX" 1)))
    ;; the server stops dropping, and the label lands without a restart
    (setf (vm-imap-mock-drops-keywords mock) nil)
    (vm-imap-net-test--warnings
      (vm-imap-net-test--run
       mock (vm-imap-net-test--store-flags "+" "1" '("important"))))
    (should (member "important" (vm-imap-mock-flags mock "INBOX" 1)))))

(ert-deftest vm-imap-net-test-a-removal-is-not-checked-for-a-dropped-keyword ()
  "Only the adding direction is checked.  A keyword still there after a
removal is a different fault, and not one Gmail has; asking on every removal
would cost a response line for nothing."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice) :drops-keywords t)
    (let ((said (vm-imap-net-test--warnings
                  (vm-imap-net-test--run
                   mock (vm-imap-net-test--store-flags "-" "1" '("important"))))))
      (should (equal said nil)))))

;;; A message too large for `vm-imap-max-message-size' (emacs-vm/vm#822)

(defconst vm-imap-net-test--whale
  (concat "From: whale@example.com\nSubject: enormous\n\n"
          (make-string 4000 ?x) "\n")
  "A message over four thousand bytes, for the size limit.")

(ert-deftest vm-imap-net-test-a-message-over-the-limit-is-left-on-the-server ()
  "A maildrop message over `vm-imap-max-message-size' is not fetched, and the
rest of the mail still is.  It is left on the server rather than fetched as
its headers: a local folder cannot go back for the body later, which is what
the option says happens there."
  (let ((vm-imap-max-message-size 2000)
        (said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _seconds format &rest args)
                 (push (apply #'format format args) said))))
      (vm-imap-net-test--spooling (mock :messages
                                        (list vm-imap-net-test--alice
                                              vm-imap-net-test--whale
                                              vm-imap-net-test--bob))
        (vm-get-new-mail)
        (should (vm-imap-net-wait nil 90))
        (should (equal (mapcar #'vm-su-subject vm-message-list)
                       '("badgers" "otters")))
        ;; still on the server, so raising the limit would get it
        (should (equal (length (vm-imap-mock-messages mock "INBOX")) 3))))
    (let ((about-size (seq-filter (lambda (line)
                                    (string-match-p "left on the server" line))
                                  said)))
      (should (= (length about-size) 1))
      (should (string-match-p "vm-imap-max-message-size" (car about-size))))))

(ert-deftest vm-imap-net-test-no-limit-fetches-every-message ()
  "Nil for the limit fetches everything, which is the default.  The size
check must not cost a message where no limit was asked for."
  (let ((vm-imap-max-message-size nil))
    (vm-imap-net-test--spooling (mock :messages
                                      (list vm-imap-net-test--alice
                                            vm-imap-net-test--whale))
      (vm-get-new-mail)
      (should (vm-imap-net-wait nil 90))
      (should (equal (mapcar #'vm-su-subject vm-message-list)
                     '("badgers" "enormous"))))))

(ert-deftest vm-imap-net-test-a-skipped-message-is-not-remembered-as-fetched ()
  "A message left for its size is not recorded in `vm-imap-retrieved-messages'.
Recorded, it would never be fetched even after the limit was raised."
  (let ((vm-imap-max-message-size 2000))
    (cl-letf (((symbol-function 'vm-warn) #'ignore))
      (vm-imap-net-test--spooling (mock :messages
                                        (list vm-imap-net-test--alice
                                              vm-imap-net-test--whale))
        (vm-get-new-mail)
        (should (vm-imap-net-wait nil 90))
        (should (equal (length vm-message-list) 1))
        ;; the limit goes up, and the message arrives on the next look
        (let ((vm-imap-max-message-size 8000))
          (vm-get-new-mail)
          (should (vm-imap-net-wait nil 90))
          (should (equal (mapcar #'vm-su-subject vm-message-list)
                         '("badgers" "enormous"))))))))

;;; A body the caller must have in hand (emacs-vm/vm#822)

(ert-deftest vm-imap-net-test-a-body-that-cannot-wait-comes-on-the-driver ()
  "A caller that must have the body now gets it, on the driver, with a wait.

`vm-retrieve-real-message-body' without `:may-arrive-later' is a save or a
copy: a message whose body has not arrived would be written as an empty one.
It used to fetch through the blocking implementation, a second connection into
a folder the driver may be writing.  It waits on the folder's own session
instead, which is what folder-name completion does."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (vm-imap-server-timeout 60)
          (message (car vm-message-list))
          (called nil))
      (vm-unload-message 1 t)
      (should (vm-body-to-be-retrieved-of message))
      (cl-letf (((symbol-function 'vm-fetch-imap-message)
                 (lambda (&rest _) (setq called t) nil)))
        ;; the answer is not the point and is not a documented one; the body
        ;; being in the folder when this returns is
        (vm-retrieve-real-message-body message :fail t))
      ;; the body is here, and the blocking fetch was not used to get it
      (should-not called)
      (should-not (vm-body-to-be-retrieved-of message))
      (should (string-match-p "The first body"
                              (vm-imap-net-test--body-of message))))))

(ert-deftest vm-imap-net-test-a-body-that-cannot-wait-reports-a-silent-server ()
  "A server that never answers is an error, not an empty message body.
The caller is saving or copying, so answering with no body would write one."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (vm-imap-server-timeout 0.5)
          (message (car vm-message-list)))
      (vm-unload-message 1 t)
      (should (vm-body-to-be-retrieved-of message))
      ;; the folder never reads as finished, so the wait runs out
      (cl-letf (((symbol-function 'vm-imap-net-unfinished-p) (lambda (&rest _) t)))
        (let ((error-data (should-error (vm-retrieve-real-message-body
                                         message :fail t)
                                        :type 'error)))
          (should (string-match-p "did not answer"
                                  (error-message-string error-data)))
          ;; the option is named, since raising it is the answer
          (should (string-match-p "vm-imap-server-timeout"
                                  (error-message-string error-data))))))))

(ert-deftest vm-imap-net-test-a-body-with-no-password-is-an-error ()
  "With no password the fetch cannot start, and that is an error rather than
a message written without its body."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (vm-unload-message 1 t)
      (cl-letf (((symbol-function 'vm-imap-net-load-message-bodies)
                 (lambda (&rest _) nil)))
        (let ((error-data (should-error (vm-retrieve-real-message-body
                                         message :fail t)
                                        :type 'error)))
          (should (string-match-p "no password"
                                  (error-message-string error-data))))))))

(ert-deftest vm-imap-net-test-a-body-that-may-arrive-later-does-not-wait ()
  "With `:may-arrive-later' nothing waits, which is the reading path."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (vm-unload-message 1 t)
      ;; does not wait: the body is not here when this returns
      (vm-retrieve-real-message-body message :may-arrive-later t)
      (should (vm-body-to-be-retrieved-of message))
      (should (vm-imap-net-wait nil 90))
      (should-not (vm-body-to-be-retrieved-of message)))))

;;; What the blocking session's own tests used to cover (emacs-vm/vm#822)

(iter-defun vm-imap-net-test--select-only (mailbox)
  "Log in and select MAILBOX, and answer what SELECT said."
  (iter-yield-from (vm-imap-net-open-session "vmtest" "secret"))
  (prog1 (iter-yield-from (vm-imap-net-select mailbox))
    (vm-imap-net-logout)))

(ert-deftest vm-imap-net-test-a-refused-select-stops-the-session ()
  "A SELECT the server answers NO stops the session with an error rather than
leaving VM to fetch from a mailbox it never selected."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice) :refuse "SELECT")
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--select-only "INBOX"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (vm-net-session-error session)))))

(ert-deftest vm-imap-net-test-a-mailbox-the-server-has-not-got-stops-the-session ()
  "Selecting a mailbox the server does not have fails rather than pretending."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice))
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--select-only "no-such-box"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (vm-net-session-error session)))))

(ert-deftest vm-imap-net-test-a-server-without-uidplus-is-still-usable ()
  "UIDPLUS is an extension, and a server without it is still usable.
VM asks for capabilities before it asks for anything else, so what it does
with a shorter list is worth knowing."
  (vm-imap-net-test--with-session
      (mock :messages (list vm-imap-net-test--alice) :no-uidplus t)
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--select-only "INBOX"))))
      (should (eq (vm-net-session-state session) 'done))
      ;; SELECT still answers with what the mailbox holds
      (should (equal (nth 0 (vm-net-session-value session)) 1)))))

;;; A body a folder type would otherwise read as a separator

(defconst vm-imap-net-test--separator-bodies
  '(("plain"              . "an ordinary body line.")
    ("a From_ line"       . "text\nFrom nobody@example.com Mon Jan  1 00:00:00 2024")
    ("a From_ line first" . "From nobody@example.com Mon Jan  1 00:00:00 2024\nrest")
    ("an mmdf separator"  . "text\n\001\001\001\001\nmore")
    ("a babyl separator"  . "text\n\037\014\nmore")
    ("8-bit"              . "Gr\303\274\303\237e"))
  "Bodies that a folder of some type would otherwise read as a separator.")

(defun vm-imap-net-test--message-with (n body)
  "A message numbered N carrying BODY."
  (concat "From: alice@example.com\nTo: vmtest@example.com\n"
          (format "Subject: message %d\nMessage-ID: <imap-%d@example.com>\n\n" n n)
          body "\n"))

(defun vm-imap-net-test--messages-in (file type)
  "Read FILE as a folder of TYPE; answer (TYPE-READ . COUNT).
`vm-build-message-list' asks `vm-get-folder-type' rather than trusting the
caller, so the buffer is given a name that states the type, as a folder VM
visits has."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert-file-contents file)
    (setq-local buffer-file-name (vm-folder-name-for-type file type))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (vm-build-message-list)
    (cons vm-folder-type (length vm-message-list))))

(defun vm-imap-net-test--move-into (type body)
  "Move two messages, the first carrying BODY, into a folder of TYPE.
On the driver, which is the only way in; answers a complaint, or nil when the
folder reads back as the two messages that were sent."
  (let ((dest (make-temp-file "vm-imap-net-dest"))
        (folder (generate-new-buffer " *vm-imap-net-move*")))
    (unwind-protect
        (condition-case err
            (vm-imap-mock-with (mock :messages
                                     (list (vm-imap-net-test--message-with 1 body)
                                           (vm-imap-net-test--message-with 2 "second body")))
              (let ((vm-imap-server-timeout 60)
                    (vm-imap-expunge-after-retrieving t)
                    (vm-imap-auto-expunge-alist nil)
                    (vm-imap-max-message-size nil)
                    (done nil))
                (with-current-buffer folder
                  (vm-test-init-folder-variables)
                  (setq vm-folder-type type)
                  (setq vm-imap-retrieved-messages nil)
                  (should (vm-imap-net-move-mail
                           (vm-imap-mock-spec mock) dest
                           (lambda (result) (setq done (or result t)))))
                  (let ((deadline (+ (float-time) 10)))
                    (while (and (not done) (< (float-time) deadline))
                      (accept-process-output nil 0.05)))
                  (when (vm-net-error-p done)
                    (error "%s" (error-message-string done))))
                (let ((read (vm-imap-net-test--messages-in dest type)))
                  (cond ((not (eq (car read) type))
                         (format "%s / %s: read back as %s" type body (car read)))
                        ((/= 2 (cdr read))
                         (format "%s / %s: %d messages, not 2" type body (cdr read)))
                        (t nil)))))
          (error (format "%s / %s: %s" type body (error-message-string err))))
      (when (buffer-live-p folder)
        (with-current-buffer folder (set-buffer-modified-p nil))
        (kill-buffer folder))
      (when (file-exists-p dest) (delete-file dest)))))

(ert-deftest vm-imap-net-test-a-separator-shaped-body-arrives-whole ()
  "Mail arriving from an IMAP maildrop lands as one message whatever it holds.
Every folder type VM will create, crossed with the bodies that a type would
otherwise read as a separator: twenty-four moves, each of two messages, each
folder read back afterwards as the two that were sent."
  (should (equal nil
                 (delq nil
                       (let (complaints)
                         (dolist (type '(From_ mboxcl2 mmdf babyl)
                                       (nreverse complaints))
                           (dolist (spec vm-imap-net-test--separator-bodies)
                             (push (vm-imap-net-test--move-into type (cdr spec))
                                   complaints))))))))

(ert-deftest vm-imap-net-test-a-killed-folder-has-nothing-outstanding ()
  "A folder that has been killed reads as finished rather than signalling.

`vm-imap-net-unfinished-p' is what a wait looks at, and a wait is now
something a command does -- a save of a message whose body is still on the
server.  `with-current-buffer' on a dead buffer would signal in the middle
of it."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((folder (current-buffer)))
      (should-not (vm-imap-net-unfinished-p folder))
      (set-buffer-modified-p nil)
      (kill-buffer folder)
      ;; the question is answerable, and the answer is no
      (should-not (vm-imap-net-unfinished-p folder))
      (should (vm-imap-net-wait folder 1)))))

(ert-deftest vm-imap-net-test-loading-a-body-says-it-is-on-its-way ()
  "`vm-load-message' says the bodies were asked for, not that they arrived.

Nothing waits on this path: the fetch shows each message again as its body
lands.  Saying \"1 message body loaded\" when the fetch has only just started
tells the reader the opposite of what happened."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (said nil))
      (vm-unload-message 1 t)
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level format &rest args)
                   (push (apply #'format format args) said))))
        (vm-load-message 1))
      (should (seq-find (lambda (line) (string-match-p "Retrieving 1 message body" line))
                        said))
      (should-not (seq-find (lambda (line) (string-match-p "bodies loaded\|body loaded" line))
                            said))
      (should (vm-imap-net-wait nil 90)))))

(ert-deftest vm-imap-net-test-loading-a-body-with-no-password-says-so-once ()
  "`vm-load-message' says so when the fetch cannot start, and loads nothing.

The messages used to fall through to a loop that asked the driver again for
each of them; now the one answer covers the lot, and the reader is told the
reason rather than being told that bodies were loaded.  They keep their flag,
so a later try still fetches them.

The count of asks is not asserted: presenting the current message asks for
its body too, and how often that happens is presentation's business.

This passes against the code before the change as well: the per-message loop
reached the same warning by a longer route.  It pins the property, not the
change."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice
                                                    vm-imap-net-test--bob))
    (let ((vm-enable-external-messages '(imap))
          (asked 0)
          (warned nil)
          (said nil))
      (vm-unload-message 2 t)
      (should (vm-body-to-be-retrieved-of (car vm-message-list)))
      (should (vm-body-to-be-retrieved-of (nth 1 vm-message-list)))
      (cl-letf (((symbol-function 'vm-imap-net-load-message-bodies)
                 (lambda (&rest _) (setq asked (1+ asked)) nil))
                ((symbol-function 'vm-warn)
                 (lambda (_level _seconds format &rest args)
                   (push (apply #'format format args) warned)))
                ((symbol-function 'vm-inform)
                 (lambda (_level format &rest args)
                   (push (apply #'format format args) said))))
        (vm-goto-message 1)
        (vm-load-message 2))
      (should (> asked 0))
      ;; the reason is given
      (should (seq-find (lambda (line) (string-match-p "no password" line))
                        warned))
      ;; and no load is claimed: what it says about loading, if anything, is
      ;; that none happened.  Taking the messages off the list either way used
      ;; to report "1 message body loaded" with nothing loaded.
      (let ((about-loading (seq-find (lambda (line)
                                       (string-match-p "loaded" line))
                                     said)))
        (should about-loading)
        (should (string-prefix-p "No " about-loading)))
      ;; both still want their bodies, for a later try
      (should (vm-body-to-be-retrieved-of (car vm-message-list)))
      (should (vm-body-to-be-retrieved-of (nth 1 vm-message-list))))))

(ert-deftest vm-imap-net-test-a-started-fetch-says-it-started ()
  "`vm-imap-net-get-spooled-mail' answers `started', not t.

The caller says what the folder holds when this answers.  With t it could not
tell a fetch that had begun from mail that had arrived, so a visit reported
the cached count as though the fetch were done -- which reads as nothing
having happened, and is what was reported (emacs-vm/vm#825)."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (should (eq (vm-imap-net-get-spooled-mail nil) 'started))
    ;; nothing has arrived yet: that is what `started' says
    (should (equal (length vm-message-list) 0))
    (should (vm-imap-net-wait nil 90))
    (should (equal (length vm-message-list) 1))))

(ert-deftest vm-imap-net-test-a-folder-already-fetching-says-started-too ()
  "A folder with a fetch running answers `started' as well: the mail is on
its way either way, and the caller must not report a count as final."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (should (eq (vm-imap-net-get-spooled-mail nil) 'started))
    (should (vm-imap-net-busy-p))
    (should (eq (vm-imap-net-get-spooled-mail nil) 'started))
    (should (vm-imap-net-wait nil 90))))

(ert-deftest vm-imap-net-test-a-visit-says-it-is-getting-new-mail ()
  "Visiting a folder says a fetch is under way rather than the old totals.

What `vm-emit-totals-blurb' counts at that moment is what the fetch has not
changed yet, and on a folder with a cache that is the old count.  The arrival
says what came (emacs-vm/vm#825)."
  (vm-imap-net-test--visiting (mock)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--alice)
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (level format &rest args)
                   (when (<= level 5)
                     (push (apply #'format format args) said)))))
        ;; the folder is visited, so this is step [16] of `vm' on its own
        (let ((vm-auto-get-new-mail t))
          (when (eq (vm-get-spooled-mail nil) 'started)
            (vm-inform 5 "%s: getting new mail..." (buffer-name)))))
      (should (seq-find (lambda (line)
                          (string-match-p "getting new mail" line))
                        said)))
    (should (vm-imap-net-wait nil 90))))

(provide 'vm-imap-net-test)

;;; vm-imap-net-test.el ends here
