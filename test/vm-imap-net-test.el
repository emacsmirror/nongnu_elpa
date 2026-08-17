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
it, as the blocking implementation leaves them out."
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
            (vm-imap-server-timeout 10)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (before (buffer-list)))
       (unwind-protect
           (progn (vm-visit-imap-folder (vm-imap-mock-spec ,(car spec)))
                  ;; visiting starts the fetch and returns without waiting for
                  ;; it, so what waits for the mail is whoever wants the mail
                  (vm-imap-net-wait nil 10)
                  ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory cache t)))))

(defun vm-imap-net-test--get-mail (mock &optional seconds)
  "Fetch new mail into the current folder and answer with what came back."
  (let ((answer 'not-called)
        (folder (current-buffer)))
    (vm-imap-net-get-mail (vm-imap-mock-spec mock)
                          (lambda (result) (setq answer result)))
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
  "The UID of every message the folder holds is compared with the server\='s,
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
same session as the fetch and before the server\='s own flags are read -- or
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
    (should (vm-imap-net-wait nil 10))
    (should (equal (length vm-message-list) 1))))

(ert-deftest vm-imap-net-test-an-unsupported-maildrop-is-left-to-the-old-path ()
  "A maildrop this cannot open without waiting answers nil, which is the
caller\='s cue to use the blocking implementation rather than to fail."
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
      (should (vm-imap-net-wait nil 10))
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
      (should (vm-imap-net-wait nil 10))
      (should (string-match-p "The first body"
                              (vm-imap-net-test--body-of message))))))


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
    (should (vm-imap-net-wait nil 10))
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
               (vm-imap-server-timeout 10)
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
      (setq response (iter-yield-from
                      (vm-imap-net-read-response-and-verify "FETCH")))
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
    (should (vm-imap-net-wait nil 10))
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
    (should (vm-imap-net-wait nil 10))
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
            (vm-imap-server-timeout 10)
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
    (should (vm-imap-net-wait nil 10))
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
    (should (vm-imap-net-wait nil 10))
    (should (equal (length vm-message-list) 1))
    (should (equal (length vm-imap-retrieved-messages) 1))
    (should (nth 1 (car vm-imap-retrieved-messages)))
    (vm-imap-mock-add-message mock "INBOX" vm-imap-net-test--bob)
    (vm-get-new-mail)
    (should (vm-imap-net-wait nil 10))
    (should (equal (length vm-message-list) 2))
    (should (equal (length vm-imap-retrieved-messages) 2))))

(ert-deftest vm-imap-net-test-a-maildrop-can-be-emptied-as-it-is-read ()
  "With auto-expunge on, what has been fetched is deleted and expunged in
the same session, so the server is not left holding a second copy."
  (let ((vm-imap-expunge-after-retrieving t))
    (vm-imap-net-test--spooling (mock :messages (list vm-imap-net-test--alice
                                                      vm-imap-net-test--bob))
      (vm-get-new-mail)
      (should (vm-imap-net-wait nil 10))
      (should (equal (length vm-message-list) 2))
      (should (vm-imap-mock-received-p mock "UID STORE"))
      (should (vm-imap-mock-received-p mock "EXPUNGE"))
      (should (null (vm-imap-mock-messages mock "INBOX"))))))

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
              (vm-imap-server-timeout 10)
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
          (should (vm-imap-net-wait folder-one 10))
          (should (vm-imap-net-wait folder-two 10))
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
    (should (vm-imap-net-wait nil 10))
    (should vm-spooled-mail-waiting)
    ;; and it did not fetch anything: a check only looks
    (should (equal (length vm-message-list) 1))))

(ert-deftest vm-imap-net-test-a-check-with-nothing-new-says-so ()
  "A mailbox holding only what the folder has already reports no mail, and
the mode line stops saying there is some."
  (vm-imap-net-test--visiting (mock :messages (list vm-imap-net-test--alice))
    (setq vm-spooled-mail-waiting t)
    (should (vm-check-for-spooled-mail nil t))
    (should (vm-imap-net-wait nil 10))
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
      (should-error (vm-imap-net-open spec "x") :type 'vm-imap-net-unsupported)
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
                      :type 'vm-imap-net-unsupported)
        (should-not (vm-imap-net-checkable-p spec))))
    ;; and a real one is enough
    (let ((vm-imap-passwords (list (list key "secret"))))
      (should (vm-imap-net-checkable-p spec)))))


;;; The log says which path took the work

(ert-deftest vm-imap-net-test-declining-says-so-and-why ()
  "A maildrop the driver will not open is announced as such, with the reason.

Without it the log said what VM was about to do rather than what it did: the
line said \"fetching new mail without waiting\" and the blocking path then
did the fetching, which is how a locked-up Emacs came to look like a
converted one."
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
                          (string-match-p "leaving it to the blocking path"
                                          line))
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
      (should (vm-imap-net-wait nil 10)))))


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
                    :type 'vm-imap-net-unsupported)
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
    (should (vm-imap-net-wait nil 10))
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
  "Reading a response is within reach of the blocking reader's speed.

A single `unwind-protect' inside `vm-imap-net-read-object' cost 100
milliseconds a response line -- generator.el re-establishes one on every
resume, and that is the hottest generator VM has.  Fetching 400 messages from
a server on this machine took 82 seconds of CPU because of it.  With the
form gone it is a quarter of a millisecond a line.

Timed rather than counted, since what went wrong was a constant factor and
nothing else would have shown it.  The bound is loose: fifty times the
blocking reader, where the pathology was four thousand."
  (let* ((lines 200)
         (response (with-temp-buffer
                     (dotimes (i lines)
                       (insert (format "* %d FETCH (UID %d RFC822.SIZE %d FLAGS (\\Seen))\r\n"
                                       (1+ i) (1+ i) (+ 500 i))))
                     (insert "vm1 OK FETCH completed\r\n")
                     (buffer-string)))
         (blocking 0)
         (driven 0))
    (with-temp-buffer
      (insert response)
      (setq vm-imap-read-point (point-min))
      (goto-char (point-min))
      (let ((start (float-time)) (n 0))
        (while (< n lines)
          (vm-imap-read-response nil)
          (setq vm-imap-read-point (point) n (1+ n)))
        (setq blocking (- (float-time) start))))
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
    (should (> blocking 0))
    (should (< driven (* 50 (max blocking 0.001))))))

(iter-defun vm-imap-net-test--read-lines (n)
  "Read N response lines through the driver's reader."
  (let ((i 0))
    (while (< i n)
      (iter-yield-from (vm-imap-net-read-response))
      (setq i (1+ i)))))

(provide 'vm-imap-net-test)

;;; vm-imap-net-test.el ends here
