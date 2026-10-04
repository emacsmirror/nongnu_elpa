;;; vm-imap-mock-test.el --- IMAP tests against the mock server -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; What VM's IMAP client does on the wire, checked against the server in
;; vm-imap-mock.el rather than against a live one.  These need nothing
;; installed and so always run, including on the machines where
;; vm-imap-live-test.el skips -- which is every machine without a
;; test/vm-live-config.el, and was every machine in CI.
;;
;; They are not a substitute for the live tests: a mock agrees with whatever
;; the person who wrote it believed about IMAP.  They are what keeps the
;; client's own logic -- literals, octet counts, UIDs across an expunge, what
;; it does when the server says NO -- from rotting between live runs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vm-imap-mock)

(eval-when-compile (require 'vm-test-init))

(defconst vm-imap-mock-test--alice
  "From: alice@example.com\nTo: me@example.com\nSubject: badgers\nMessage-ID: <one@example.com>\n\nThe first body.\n"
  "A message to put in a mock mailbox.")

(defconst vm-imap-mock-test--bob
  "From: bob@example.com\nTo: me@example.com\nSubject: otters\nMessage-ID: <two@example.com>\n\nThe second body.\n"
  "Another, so a test can tell one message from the next.")

(defmacro vm-imap-mock-test--with-session (spec &rest body)
  "Open a VM IMAP session against a mock server and run BODY in its buffer.
SPEC is (MOCK-VAR PROCESS-VAR &rest ARGS), ARGS going to `vm-imap-mock-start'.
BODY runs with the process buffer current, which is where VM's own IMAP
functions read their responses from -- called anywhere else they read an
empty buffer and time out.

`vm-buffer-types' says `process' while BODY runs, because that is what a real
caller has done by the time it gets here: VM pushes the type on its way into a
connection, and the functions BODY calls assert it.  Without it these tests
fail with VM's assertions checked -- `test-runner --assert\\=' -- which is a test
setting up a state no caller is in rather than anything wrong with VM."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cddr spec))
     (let* ((vm-imap-server-timeout 10)
            (,(cadr spec) (vm-imap-make-session (vm-imap-mock-spec ,(car spec))
                                                nil :purpose "test"))
            (vm-buffer-types (cons 'process vm-buffer-types)))
       (unwind-protect
           (with-current-buffer (process-buffer ,(cadr spec))
             ,@body)
         (when (processp ,(cadr spec))
           (ignore-errors (delete-process ,(cadr spec)))
           (when (buffer-live-p (process-buffer ,(cadr spec)))
             (kill-buffer (process-buffer ,(cadr spec)))))))))

(defmacro vm-imap-mock-test--visiting (spec &rest body)
  "Visit a mock IMAP folder as a VM folder and run BODY in it.
SPEC is (MOCK-VAR &rest ARGS) as for `vm-imap-mock-start'.  The cache
directory is a temporary one: VM writes a cache file per IMAP folder and it
would otherwise land in the user's home."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cdr spec))
     (let* ((cache (make-temp-file "vm-imap-mock-cache" t))
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

;;; The session

                ; read-write

;;; Fetching a folder

(ert-deftest vm-imap-mock-test-visiting-a-folder-retrieves-the-messages ()
  "Visiting an IMAP maildrop brings the messages in, headers and bodies.
This is the whole client path -- session, SELECT, the UID and size fetch, and
the BODY.PEEK[] that carries the message -- and until there was a mock it ran
in no test that a machine without a server could run."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (should (equal (length vm-message-list) 2))
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))
    (should (equal (vm-su-from (car vm-message-list)) "alice@example.com"))
    (should (string-match-p "The first body"
                            (vm-imap-mock-test--body-of (car vm-message-list))))
    (should (string-match-p "The second body"
                            (vm-imap-mock-test--body-of (cadr vm-message-list))))))

(defmacro vm-imap-mock-test--blocking (&rest body)
  "Run BODY with the asynchronous driver declining, so the old path runs.
`vm-get-spooled-mail' calls `vm-imap-net-get-spooled-mail' first and falls
back to `vm-imap-synchronize-folder' when it answers nil, which is what
happens when the driver cannot ask for something -- a password nobody can be
prompted for inside a process filter.  That fallback is the only caller of
`vm-imap-retrieve-messages'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'vm-imap-net-get-spooled-mail)
              (lambda (&rest _) nil)))
     ,@body))

(ert-deftest vm-imap-mock-test-an-extra-fetch-item-is-stepped-over ()
  "A server that answers with more than VM asked for still delivers the mail.

RFC 3501 7.4.2 lets a server send data items the client did not ask about, and
a server with CONDSTORE on sends MODSEQ with everything.  VM called that a
broken response and failed on its first command -- \"expected UID, RFC822.SIZE
and (FLAGS list) in FETCH response\" -- and the retrieval was stricter still,
matching (BODY[] string) or a UID before it and nothing else."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob)
            :extra-fetch-items t)
    (should (equal (length vm-message-list) 2))
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))
    ;; the bodies too, which come through the other parser
    (should (string-match-p "The first body"
                            (vm-imap-mock-test--body-of (car vm-message-list))))))

(ert-deftest vm-imap-mock-test-an-unsolicited-flag-report-is-not-message-data ()
  "A FETCH the server sent of its own accord is not taken for an answer.

Somebody else changing a message's flags has the server report them whenever it
next can (RFC 3501 7.4.1).  That response carries no UID, and taking it for
message data put an entry with no UID in the folder's tables: \"Wrong type
argument: stringp, nil\", and no mail."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob)
            :unsolicited-flags t)
    (should (equal (length vm-message-list) 2))
    (should (equal (mapcar #'vm-imap-uid-of vm-message-list) '("1" "2")))))

(defun vm-imap-mock-test--body-of (message)
  "The text of MESSAGE as it sits in the folder buffer."
  (with-current-buffer (vm-buffer-of message)
    (save-restriction
      (widen)
      (buffer-substring (vm-text-of message) (vm-text-end-of message)))))

(ert-deftest vm-imap-mock-test-an-empty-mailbox-visits-empty ()
  "An IMAP folder with nothing in it is a folder with no messages, not a
failure: VM asks for no message ranges at all."
  (vm-imap-mock-test--visiting (mock)
    (should (null vm-message-list))
    (should-not (vm-imap-mock-received-p mock "BODY"))))

(ert-deftest vm-imap-mock-test-the-message-arrives-whole ()
  "Every octet the server sent is in the folder.
A literal is counted in octets and read by count, so an off-by-one shows up
as a message with a line missing or one glued to the next."
  (let ((long (concat "From: alice@example.com\nSubject: long\n\n"
                      (mapconcat (lambda (n) (format "line %d" n))
                                 (number-sequence 1 200) "\n")
                      "\n")))
    (vm-imap-mock-test--visiting (mock :messages (list long))
      (should (equal (length vm-message-list) 1))
      (let ((body (vm-imap-mock-test--body-of (car vm-message-list))))
        (should (string-match-p "^line 1$" body))
        (should (string-match-p "^line 200$" body))))))

;;; Flags

(ert-deftest vm-imap-mock-test-a-seen-message-arrives-read ()
  "A message the server has flagged \\Seen is not new when it gets here.
The flags come down with the same fetch as the sizes, and they are what VM's
own attributes are set from."
  (vm-imap-mock-test--visiting
      (mock :messages (list (cons vm-imap-mock-test--alice '("\\Seen"))
                            vm-imap-mock-test--bob))
    (should (equal (length vm-message-list) 2))
    (should-not (vm-unread-flag (car vm-message-list)))
    (should (vm-new-flag (cadr vm-message-list)))))

;;; When the server does not play along

(ert-deftest vm-imap-mock-test-a-truncated-fetch-is-an-error ()
  "A download the server cuts off short is an error, not half a message.
The point is that the folder does not end up holding the fragment.

Visiting no longer signals it: the fetch happens after the visit has
returned, so the failure is a warning when it happens rather than an error
where the command was typed."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice)
                           :truncate-fetch t)
    (let* ((cache (make-temp-file "vm-imap-mock-cache" t))
           (vm-imap-folder-cache-directory cache)
           (vm-imap-server-timeout 10)
           (vm-frame-per-folder nil)
           (vm-mutable-frame-configuration nil)
           (warned nil)
           (before (buffer-list)))
      (unwind-protect
          (cl-letf (((symbol-function 'vm-warn)
                     (lambda (_l _secs &rest args)
                       (push (apply #'format args) warned))))
            (vm-visit-imap-folder (vm-imap-mock-spec mock))
            (vm-imap-net-wait nil 10)
            (should (null vm-message-list))
            (should warned))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (delete-directory cache t)))))

;;; Mailboxes

;;; Retrieving from an IMAP maildrop, and external message bodies
;;
;; The tests above visit an IMAP folder.  These use the mock the other way, as
;; a maildrop that `vm-get-new-mail' fetches from into a local folder, which is
;; a different path through the client and the one `vm-expunge-imap-messages'
;; works on: it deletes from the server the messages already retrieved, and it
;; has nothing to go on until something has been retrieved.

(defmacro vm-imap-mock-test--spooling (spec &rest body)
  "Visit a local folder fed from a mock IMAP maildrop, and run BODY in it.
SPEC is (MOCK-VAR &rest ARGS) as for `vm-imap-mock-start'.  BODY runs in the
local folder buffer, with `vm-spool-files' naming the mock as its spool."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cdr spec))
     (let* ((dir (file-name-as-directory (make-temp-file "vm-imap-spool" t)))
            (cache (make-temp-file "vm-imap-mock-cache" t))
            (local (expand-file-name "inbox" dir))
            (vm-imap-folder-cache-directory cache)
            (vm-imap-server-timeout 10)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            ;; visiting a folder fetches its spool by itself, which would mean
            ;; the messages had already arrived before the test asked for them
            ;; -- and a test of fetching twice would then be fetching once
            (vm-auto-get-new-mail nil)
            ;; the crash box has to be a file of its own: VM refuses to gobble
            ;; a crash box that is the folder
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
         (delete-directory cache t)
         (delete-directory dir t)))))

(ert-deftest vm-imap-mock-test-getting-new-mail-from-a-maildrop ()
  "`vm-get-new-mail' fetches from an IMAP maildrop into a local folder.
The messages arrive whole, and VM records their UIDs as retrieved -- that
list is what tells it not to fetch them again, and what
`vm-expunge-imap-messages' works from."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))
    (should (equal (length vm-imap-retrieved-messages) 2))
    (should (string-match-p "The first body"
                            (vm-imap-mock-test--body-of (car vm-message-list))))))

(ert-deftest vm-imap-mock-test-getting-new-mail-twice-fetches-once ()
  "A second `vm-get-new-mail' brings nothing new: the UIDs are remembered,
so the messages are not fetched again and the folder does not grow."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length vm-message-list) 2))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length vm-message-list) 2))))

(ert-deftest vm-imap-mock-test-expunging-what-has-been-retrieved ()
  "`vm-expunge-imap-messages' deletes from the server what has been
retrieved, and leaves the local copies alone.  It flags each UID \\Deleted
and closes the mailbox, which is what expunges them."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-expunge-imap-messages))
    ;; the expunge goes through the driver, so it lands after the command
    (should (vm-imap-mock-test--wait-until
             (lambda () (null (vm-imap-mock-messages mock "INBOX")))))
    (should (vm-imap-mock-received-p mock "STORE .*\\\\Deleted"))
    ;; the local folder still has them: this deletes from the server only
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))))

(ert-deftest vm-imap-mock-test-expunging-spares-what-was-never-retrieved ()
  "REGRESSION: a message VM has no copy of survives the expunge.

`vm-expunge-imap-messages' deletes what the folder recorded as retrieved and
nothing else.  Three messages on the server and two recorded, so the third
has to be there afterwards; a command that deleted whatever it found would
pass `expunging-what-has-been-retrieved' above, which retrieves both of its
two and expects an empty mailbox.

The coverage for this was against the blocking path (emacs-vm/vm#822), which
had no equivalent here.  How it is done is worth knowing and is not visible
from outside: VM flags each UID \\Deleted and then closes the mailbox, whose
implicit expunge does the deleting (RFC 3501 6.4.2).  No EXPUNGE command is
sent, so a test looking for one would conclude nothing had happened."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
    ;; a third arrives that this folder never fetched
    (vm-imap-mock-add-message mock "INBOX"
                              "From: carol@example.com\nSubject: never fetched\n\nA body.\n")
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 3))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-expunge-imap-messages))
    (should (vm-imap-mock-test--wait-until
             (lambda () (equal 1 (length (vm-imap-mock-messages mock "INBOX"))))))
    ;; and it is the one VM never had
    (let ((text (vm-imap-mock-message-text
                 (car (vm-imap-mock-messages mock "INBOX")))))
      (should (string-match-p "Subject: never fetched" text)))))

(ert-deftest vm-imap-mock-test-expunging-forgets-only-what-was-deleted ()
  "What the server deleted is forgotten and what it did not is kept.

An expunge that fails half way through must leave the rest to be offered
again rather than forgetting messages that are still on the server."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length vm-imap-retrieved-messages) 2))
    ;; the server refuses the STORE, so nothing is deleted and nothing is
    ;; forgotten
    (setf (vm-imap-mock-refuse mock) "STORE")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (let ((said (vm-imap-mock-test--warnings
                    (vm-expunge-imap-messages)
                    (vm-imap-mock-test--wait-until
                     (lambda () vm-imap-mock-test--said)))))
        (should said)))
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
    (should (equal (length vm-imap-retrieved-messages) 2))
    ;; and with the server willing, both go and both are forgotten
    (setf (vm-imap-mock-refuse mock) nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-expunge-imap-messages))
    ;; not yet: the command returned before the server was even asked, which
    ;; is what it no longer waits for
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
    (should (vm-imap-mock-test--wait-until
             (lambda () (null (vm-imap-mock-messages mock "INBOX")))))
    (should (vm-imap-mock-test--wait-until
             (lambda () (null vm-imap-retrieved-messages))))))

(ert-deftest vm-imap-mock-test-expunging-nothing-retrieved-touches-nothing ()
  "With nothing retrieved there is nothing to delete, and the server keeps
its messages."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-expunge-imap-messages))
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 1))
    (should-not (vm-imap-mock-received-p mock "STORE"))))

;;; Bodies kept on the server

(ert-deftest vm-imap-mock-test-unloading-a-body-and-fetching-it-back ()
  "`vm-unload-message' throws the body away and `vm-load-message' fetches it
again from the server.

This is what `vm-enable-external-messages' is for: the folder holds the
headers and the body is fetched when it is wanted.  The proof that it really
went is that the folder buffer no longer has it, and the proof that it comes
back is the UID FETCH the server sees."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (should (vm-message-can-be-external message))
      (should (string-match-p "The first body"
                              (vm-imap-mock-test--body-of message)))
      (vm-unload-message 1 t)
      (should (vm-body-to-be-retrieved-of message))
      (should (equal (vm-imap-mock-test--body-of message) ""))
      (vm-load-message 1)
      ;; the load goes through the driver, so the body lands after it returns
      (should (vm-imap-net-wait nil 10))
      (should-not (vm-body-to-be-retrieved-of message))
      (should (string-match-p "The first body"
                              (vm-imap-mock-test--body-of message)))
      (should (vm-imap-mock-received-p mock "UID FETCH")))))

(ert-deftest vm-imap-mock-test-refreshing-a-message-reloads-its-body ()
  "`vm-refresh-message' throws the body away and reads it again in one step,
which is what to do with a message whose copy here has gone wrong."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-enable-external-messages '(imap))
          (message (car vm-message-list)))
      (vm-refresh-message)
      (should (vm-imap-net-wait nil 10))
      (should-not (vm-body-to-be-retrieved-of message))
      (should (string-match-p "The first body"
                              (vm-imap-mock-test--body-of message)))
      (should (vm-imap-mock-received-p mock "UID FETCH")))))

;;; Synchronising, and pruning what the server no longer has

(defun vm-imap-mock-test--has-flag (mock mailbox uid flag)
  "Whether the message with UID in MAILBOX carries FLAG, whatever its case.
IMAP system flags are case-insensitive and VM sends \\deleted in lower case,
while a mock that stored what it was given keeps it that way."
  (cl-some (lambda (had) (equal (downcase had) (downcase flag)))
           (vm-imap-mock-flags mock mailbox uid)))

(ert-deftest vm-imap-mock-test-synchronize-sends-changes-and-fetches-new ()
  "`vm-imap-synchronize' pushes what changed here and pulls what is new there.
A message flagged deleted in the folder is flagged on the server, and a
message that arrived on the server since the folder was visited is fetched --
the two halves its docstring promises, in that order."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (should (equal (length vm-message-list) 1))
    (vm-set-deleted-flag (car vm-message-list) t)
    (vm-imap-mock-add-message mock "INBOX" vm-imap-mock-test--bob)
    (vm-imap-synchronize)
    (vm-imap-net-wait nil 10)
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))
    (should (vm-imap-mock-test--has-flag mock "INBOX" 1 "\\Deleted"))
    ;; the message it deleted is still here: synchronising does not expunge
    (should (vm-deleted-flag (car vm-message-list)))))

(ert-deftest vm-imap-mock-test-synchronize-needs-an-imap-folder ()
  "In a folder that is not an IMAP folder the command says so rather than
trying, and touches no server."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let* ((dir (file-name-as-directory (make-temp-file "vm-not-imap" t)))
           (local (expand-file-name "plain" dir))
           (vm-frame-per-folder nil)
           (vm-mutable-frame-configuration nil)
           (vm-auto-get-new-mail nil)
           (before (buffer-list))
           (said nil))
      (unwind-protect
          (progn
            (write-region (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
                                  "From: alice@example.com\nSubject: local\n\nA body.\n\n")
                          nil local nil 'quiet)
            (cl-letf (((symbol-function 'vm-display) #'ignore)
                      ((symbol-function 'vm-inform)
                       (lambda (_level format &rest args)
                         (push (apply #'format format args) said))))
              (vm-visit-folder local)
              (vm-imap-synchronize))
            (should (cl-find-if (lambda (s)
                                  (string-match-p "not an IMAP folder" s))
                                said))
            (should-not (vm-imap-mock-received-p mock "SELECT")))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (delete-directory dir t)))))

(ert-deftest vm-imap-mock-test-pruning-the-retrieved-list ()
  "`vm-prune-imap-retrieved-list' forgets the UIDs the server no longer has.

VM remembers what it has retrieved so as not to fetch it twice, and that list
grows for ever otherwise -- a message deleted on the server by something else
would be remembered as retrieved long after it was gone.  Here one of the two
is taken off the server behind VM's back, and the list comes back to one."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length vm-imap-retrieved-messages) 2))
    (setf (vm-imap-mock-message-expunged
           (car (vm-imap-mock-messages mock "INBOX")))
          t)
    (vm-prune-imap-retrieved-list (vm-imap-mock-spec mock))
    ;; the asking goes through the driver, so the pruning happens when the
    ;; server has answered rather than before this returns
    (should (vm-imap-mock-test--wait-until
             (lambda () (equal (length vm-imap-retrieved-messages) 1))))
    ;; the local messages are untouched: this prunes a memo, not the mail
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))))

(ert-deftest vm-imap-mock-test-pruning-keeps-what-the-server-still-has ()
  "With everything still on the server, nothing is forgotten."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (vm-prune-imap-retrieved-list (vm-imap-mock-spec mock))
    ;; nothing is forgotten, before or after the answer
    (should (equal (length vm-imap-retrieved-messages) 2))
    (vm-imap-mock-test--wait-until
     (lambda () (vm-imap-mock-received-p mock "LOGOUT")))
    (should (equal (length vm-imap-retrieved-messages) 2))))

(ert-deftest vm-imap-mock-test-pruning-says-so-when-it-cannot-ask ()
  "With no password for the maildrop, pruning says so and forgets nothing.
It used to go on to a blocking session, whose process variable was bound to
nil and never assigned: `(process-buffer nil)', so that half could not run at
all.  Nothing is remembered as pruned by a look that never happened."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (should (equal (length vm-imap-retrieved-messages) 2))
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-imap-net-mailbox-uids) (lambda (&rest _) nil))
                ((symbol-function 'vm-inform)
                 (lambda (_level format &rest args)
                   (push (apply #'format format args) said))))
        (vm-prune-imap-retrieved-list (vm-imap-mock-spec mock)))
      (should (seq-find (lambda (line) (string-match-p "no password" line)) said)))
    (should (equal (length vm-imap-retrieved-messages) 2))))

;;; Making, renaming and deleting mailboxes on the server

(defun vm-imap-mock-test--spec-for (mock mailbox)
  "A maildrop spec for MAILBOX on MOCK.
Built rather than edited from `vm-imap-mock-spec': `replace-regexp-in-string'
matches the case of what it replaced, so substituting a mixed-case name into
a spec naming INBOX gives the name back in capitals."
  (format "imap:127.0.0.1:%d:%s:login:%s:%s"
          (vm-imap-mock-port mock) mailbox
          (vm-imap-mock-user mock) (vm-imap-mock-password mock)))

(defun vm-imap-mock-test--wait-until (predicate &optional seconds)
  "Wait until PREDICATE answers non-nil, up to SECONDS, and answer with it.
The mailbox commands go through the driver: they return before the server has
done what they asked, so a test that reads the mailbox back waits for it."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defmacro vm-imap-mock-test--warnings (&rest body)
  "Run BODY and answer with the warnings VM gave, newest last.
A command that goes through the driver reports a refusal when the answer
arrives, so what a failure leaves behind is a warning and not a signal."
  (declare (indent 0) (debug t))
  `(let ((vm-imap-mock-test--said nil))
     (cl-letf (((symbol-function 'vm-warn)
                (lambda (_level _seconds &rest args)
                  (setq vm-imap-mock-test--said
                        (append vm-imap-mock-test--said
                                (list (apply #'format args)))))))
       ,@body)
     vm-imap-mock-test--said))

(defvar vm-imap-mock-test--said nil
  "Where `vm-imap-mock-test--warnings' collects what VM warned about.")

(defun vm-imap-mock-test--warned-about (text &optional warnings)
  "Whether WARNINGS, or what has been collected so far, has one matching TEXT.
A test waits for the warning it is about rather than for the first warning of
any kind: VM says \"running from source\" before anything else in a tree whose
lisp/ has not been byte-compiled."
  (seq-find (lambda (line) (string-match-p text line))
            (or warnings vm-imap-mock-test--said)))

(ert-deftest vm-imap-mock-test-creating-a-mailbox ()
  "`vm-create-imap-folder' makes the mailbox its spec names, and the server
has it afterwards."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-imap-server-timeout 10))
      (should (equal (vm-imap-mock-mailbox-names mock) '("INBOX")))
      (vm-create-imap-folder (vm-imap-mock-test--spec-for mock "Later"))
      (should (vm-imap-mock-test--wait-until
               (lambda () (member "Later" (vm-imap-mock-mailbox-names mock)))))
      (should (vm-imap-mock-received-p mock "CREATE")))))

(ert-deftest vm-imap-mock-test-creating-a-mailbox-that-exists ()
  "Making a mailbox that is already there is refused by the server, and VM
says so rather than reporting success."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let* ((vm-imap-server-timeout 10)
           ;; wait for the warning this is about, not for any warning: VM
           ;; says "running from source" first in a tree whose lisp/ is not
           ;; byte-compiled, and the wait ended on that
           (said (vm-imap-mock-test--warnings
                   (vm-create-imap-folder
                    (vm-imap-mock-test--spec-for mock "INBOX"))
                   (vm-imap-mock-test--wait-until
                    (lambda () (vm-imap-mock-test--warned-about
                                "CREATE failed"))))))
      (should (vm-imap-mock-test--warned-about "CREATE failed" said)))))

(ert-deftest vm-imap-mock-test-renaming-a-mailbox ()
  "`vm-rename-imap-folder' renames it on the server, and what was in it is
still in it under the new name."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-imap-server-timeout 10))
      (vm-imap-mock-add-message mock "Archive" vm-imap-mock-test--bob)
      (vm-rename-imap-folder (vm-imap-mock-test--spec-for mock "Archive")
                             (vm-imap-mock-test--spec-for mock "Old"))
      (should (vm-imap-mock-test--wait-until
               (lambda () (member "Old" (vm-imap-mock-mailbox-names mock)))))
      (should-not (member "Archive" (vm-imap-mock-mailbox-names mock)))
      (should (equal (length (vm-imap-mock-messages mock "Old")) 1))
      (should (vm-imap-mock-received-p mock "RENAME")))))

(ert-deftest vm-imap-mock-test-deleting-a-mailbox ()
  "`vm-delete-imap-folder' takes it off the server, and only the one named."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-imap-server-timeout 10))
      (vm-imap-mock-add-message mock "Archive" vm-imap-mock-test--bob)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-delete-imap-folder (vm-imap-mock-test--spec-for mock "Archive")))
      (should (vm-imap-mock-test--wait-until
               (lambda ()
                 (not (member "Archive" (vm-imap-mock-mailbox-names mock))))))
      (should (member "INBOX" (vm-imap-mock-mailbox-names mock)))
      (should (vm-imap-mock-received-p mock "DELETE")))))

(ert-deftest vm-imap-mock-test-deleting-a-mailbox-that-is-not-there ()
  "Deleting a mailbox the server does not have is reported, not passed over.
The mock answers NO, which is what a server does, and VM has to notice."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice)
                           :refuse "DELETE")
    (let ((vm-imap-server-timeout 10))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (let ((said (vm-imap-mock-test--warnings
                      (vm-delete-imap-folder
                       (vm-imap-mock-test--spec-for mock "Nowhere"))
                      (vm-imap-mock-test--wait-until
                       (lambda () (vm-imap-mock-test--warned-about
                                   "DELETE failed"))))))
          (should (vm-imap-mock-test--warned-about "DELETE failed" said))))
      (should (equal (vm-imap-mock-mailbox-names mock) '("INBOX"))))))

;;; Saving a message to an IMAP folder

(defvar vm-imap-mock-test--folder-text nil
  "What `vm-imap-mock-test--saving-from-a-file' writes as the folder.
Nil for its usual two messages.  A test binds it to put something else in
the folder, an eight-bit body for one.")

(defmacro vm-imap-mock-test--saving-from-a-file (spec &rest body)
  "Visit a file folder of two messages and run BODY, with MOCK serving IMAP.
SPEC is (MOCK-VAR &rest ARGS) as for `vm-imap-mock-start'.  This is the
half of `vm-save-message-to-imap-folder' that has to send the message: the
source folder is not on the server, so the save is an APPEND."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cdr spec))
     (let* ((dir (file-name-as-directory (make-temp-file "vm-imap-save" t)))
            (folder (expand-file-name "incoming" dir))
            (cache (make-temp-file "vm-imap-mock-cache" t))
            (vm-imap-folder-cache-directory cache)
            (vm-imap-server-timeout 10)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (vm-delete-after-saving nil)
            (vm-last-save-imap-folder nil)
            (before (buffer-list)))
       (unwind-protect
           (progn
             (let ((coding-system-for-write 'binary))
               (write-region
                (or vm-imap-mock-test--folder-text
                    (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                            vm-imap-mock-test--alice "\n"
                            "From bob@example.com Sat Aug  8 14:25:13 2026\n"
                            vm-imap-mock-test--bob "\n"))
                nil folder nil 'quiet))
             (cl-letf (((symbol-function 'vm-display) #'ignore))
               (vm-visit-folder folder)
               (setq vm-message-pointer vm-message-list)
               ,@body))
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory cache t)
         (delete-directory dir t)))))

(defun vm-imap-mock-test--saved-subjects (mock mailbox)
  "The Subject of every message MAILBOX holds on MOCK.
Without its carriage return: a message goes up in CRLF, which is what
`vm-imap-mock-test-saving-a-message-appends-it-to-the-mailbox' is about."
  (mapcar (lambda (message)
            (let ((text (vm-imap-mock-message-text message)))
              (when (string-match "^Subject: \\(.*?\\)\r?$" text)
                (match-string 1 text))))
          (vm-imap-mock-messages mock mailbox)))

(defun vm-imap-mock-test--wait-for (mock regexp)
  "Wait up to two seconds for MOCK to receive a command matching REGEXP.
VM sends LOGOUT without waiting for the answer -- some servers do not give
one -- so the server may not have read it when the command returns."
  (let ((deadline (+ 20 0)))
    (while (and (> deadline 0) (not (vm-imap-mock-received-p mock regexp)))
      (accept-process-output nil 0.1)
      (setq deadline (1- deadline))))
  (vm-imap-mock-received-p mock regexp))

(ert-deftest vm-imap-mock-test-saving-a-message-appends-it-to-the-mailbox ()
  "Saving to an IMAP folder sends the message to the server and marks it
filed here.  The mailbox is created on the way: saving to a folder that does
not exist yet is how the first one is made."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (let ((target (vm-imap-mock-test--spec-for mock "Saved")))
      (vm-save-message-to-imap-folder target)
      (vm-imap-net-wait nil 10)
      (should (equal (vm-imap-mock-test--saved-subjects mock "Saved")
                     '("badgers")))
      (should (vm-imap-mock-received-p mock "APPEND"))
      ;; in CRLF, as the protocol has it, and counted in octets: a literal
      ;; whose count disagrees with what follows loses the session
      (should (string-match-p
               "\r\n" (vm-imap-mock-message-text
                        (car (vm-imap-mock-messages mock "Saved")))))
      (should (vm-imap-mock-received-p mock "APPEND \"Saved\" .* {[0-9]+}"))
      (should (vm-filed-flag (car vm-message-list)))
      (should-not (vm-filed-flag (nth 1 vm-message-list))))))

(defconst vm-imap-mock-test--eight-bit
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nTo: me@example.com\n"
          "Subject: caf\303\251\nMessage-ID: <eight@example.com>\n"
          "MIME-Version: 1.0\n"
          "Content-Type: text/plain; charset=UTF-8\n"
          "Content-Transfer-Encoding: 8bit\n\n"
          "Two caf\303\251s and a na\303\257ve r\303\251sum\303\251.\n\n")
  "One message whose header and body carry UTF-8, so eight-bit on the wire.
Written as octets: a folder file is bytes, and these are the bytes.")

(ert-deftest vm-imap-mock-test-saving-an-eight-bit-message-announces-its-octets ()
  "REGRESSION: an APPEND promised more octets than it sent.
The literal count came from `string-bytes' on the text, and the text is
multibyte by then: `vm-imap-subst-CRLF-for-LF' works in a multibyte buffer,
where each byte of the folder over 0x7F becomes a character that
`string-bytes' counts as two.  A message with one UTF-8 `e' acute was
announced as four octets longer than it was sent, so the server went on
waiting for the rest of a literal that had finished, took the next command
line as message data, and the session was lost.

Every save of a message with an eight-bit header or body to an IMAP mailbox
went that way, which is most real mail.  The test message carries UTF-8 in
both."
  (let ((vm-imap-mock-test--folder-text vm-imap-mock-test--eight-bit))
    (vm-imap-mock-test--saving-from-a-file (mock)
      (let ((target (vm-imap-mock-test--spec-for mock "Saved")))
        (vm-save-message-to-imap-folder target)
        (vm-imap-net-wait nil 10)
        ;; it arrived at all, which it cannot have done if the count was wrong
        (should (equal (length (vm-imap-mock-messages mock "Saved")) 1))
        (let ((text (vm-imap-mock-message-text
                     (car (vm-imap-mock-messages mock "Saved")))))
          ;; and it arrived whole: the body is the last thing in the literal,
          ;; so a short count loses the end of it
          (should (string-match-p "r\303\251sum\303\251" text))
          (should (string-match-p "Subject: caf\303\251" text)))
        (should (vm-filed-flag (car vm-message-list)))))))

(ert-deftest vm-imap-mock-test-saving-remembers-the-folder-it-saved-to ()
  "`vm-last-save-imap-folder' is what the next save offers, so it is the
folder that was saved to and not the one before it."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (let ((target (vm-imap-mock-test--spec-for mock "Saved")))
      (vm-save-message-to-imap-folder target)
      (vm-imap-net-wait nil 10)
      (should (equal vm-last-save-imap-folder target)))))

(ert-deftest vm-imap-mock-test-saving-without-a-count-saves-one-message ()
  "Called from Lisp with no count, one message is saved: the count comes
from the prefix argument, and defaulting it to nothing would save the whole
folder or none of it."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder (vm-imap-mock-test--spec-for mock "Saved"))
    (vm-imap-net-wait nil 10)
    (should (equal (length (vm-imap-mock-messages mock "Saved")) 1))))

(ert-deftest vm-imap-mock-test-saving-a-count-of-two-saves-both ()
  "A count of two saves this message and the next, in that order."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder
     (vm-imap-mock-test--spec-for mock "Saved") 2)
    (vm-imap-net-wait nil 10)
    (should (equal (vm-imap-mock-test--saved-subjects mock "Saved")
                   '("badgers" "otters")))
    ;; one session for the lot, not one per message: a server counts
    ;; connections, and logging in again for each is what makes a save of a
    ;; hundred messages a hundred logins
    (should (equal (length (cl-remove-if-not
                            (lambda (line) (string-match-p "LOGIN" line))
                            (vm-imap-mock-commands mock)))
                   1))))

(ert-deftest vm-imap-mock-test-saving-a-given-list-ignores-the-count ()
  "A caller that passes the messages gets those messages saved.  That is how
`vm-save-message' and the marked-messages commands reach this, and the count
they also pass is not a second opinion about which."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder
     (vm-imap-mock-test--spec-for mock "Saved") 1 (cdr vm-message-list))
    (vm-imap-net-wait nil 10)
    (should (equal (vm-imap-mock-test--saved-subjects mock "Saved")
                   '("otters")))))

(ert-deftest vm-imap-mock-test-deleting-after-saving-is-a-setting ()
  "`vm-delete-after-saving' deletes the message that was saved and no other,
and leaves it alone when it is off -- a folder emptied by a save nobody asked
to empty is not a small mistake."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (let ((target (vm-imap-mock-test--spec-for mock "Saved")))
      (vm-save-message-to-imap-folder target)
      (vm-imap-net-wait nil 10)
      (should-not (vm-deleted-flag (car vm-message-list)))
      (let ((vm-delete-after-saving t))
        (vm-save-message-to-imap-folder target)
        (vm-imap-net-wait nil 10))
      (should (vm-deleted-flag (car vm-message-list)))
      (should-not (vm-deleted-flag (nth 1 vm-message-list))))))

(ert-deftest vm-imap-mock-test-saving-ends-the-session-it-opened ()
  "The session opened for the save is closed again: VM keeps no connection
for a command that is over, and a server counts them (dovecot's
`mail_max_userip_connections')."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder (vm-imap-mock-test--spec-for mock "Saved"))
    (vm-imap-net-wait nil 10)
    (should (vm-imap-mock-test--wait-for mock "LOGOUT"))))

(ert-deftest vm-imap-mock-test-saving-on-the-same-server-copies ()
  "Saving from an IMAP folder to another mailbox on the same server is a
COPY on the server, not a message sent back up: the message need not come
down here at all."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-delete-after-saving nil)
          (vm-last-save-imap-folder nil))
      (vm-imap-mock-add-mailbox mock "Saved")
      (vm-save-message-to-imap-folder
       (vm-imap-mock-test--spec-for mock "Saved"))
      (vm-imap-net-wait nil 10)
      (should (equal (vm-imap-mock-test--saved-subjects mock "Saved")
                     '("badgers")))
      (should (vm-imap-mock-received-p mock "UID COPY"))
      (should-not (vm-imap-mock-received-p mock "APPEND"))
      ;; and the message is not fetched to be copied: the point of asking the
      ;; server to do it is that the message never comes down
      (should-not (vm-imap-mock-received-p mock "BODY\\[\\]"))
      (should (vm-filed-flag (car vm-message-list))))))

(ert-deftest vm-imap-mock-test-copying-makes-the-mailbox-if-it-is-missing ()
  "REGRESSION: saving to a folder that does not exist yet makes it, whichever
path the save takes.  Issue #690: the append path sent CREATE first and the
copy path did not, so `S' to a new folder name worked from a file folder and
failed with the server\\='s NO [TRYCREATE] from an IMAP one -- a difference the
user did not ask for and cannot see."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-delete-after-saving nil)
          (vm-last-save-imap-folder nil))
      (vm-save-message-to-imap-folder
       (vm-imap-mock-test--spec-for mock "Nowhere"))
      (vm-imap-net-wait nil 10)
      (should (member "Nowhere" (vm-imap-mock-mailbox-names mock)))
      (should (equal (vm-imap-mock-test--saved-subjects mock "Nowhere")
                     '("badgers")))
      (should (vm-imap-mock-received-p mock "CREATE")))))

(ert-deftest vm-imap-mock-test-saving-says-how-many-and-where ()
  "The line at the end of a save is what tells the user it happened, so it
counts the messages, agrees with itself about the plural, and is at the
verbosity ordinary progress is reported at."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (let ((target (vm-imap-mock-test--spec-for mock "Saved"))
          (said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (level fmt &rest args)
                   (when (string-match-p "saved to" fmt)
                     (push (cons level (apply #'format fmt args)) said)))))
        (vm-save-message-to-imap-folder target)
        (vm-imap-net-wait nil 10)
        (should (equal (length said) 1))
        (should (equal (car (car said)) 5))
        (should (string-match-p "\\`1 message saved to " (cdr (car said))))
        (setq said nil)
        (vm-save-message-to-imap-folder target 2)
        (vm-imap-net-wait nil 10)
        (should (string-match-p "\\`2 messages saved to " (cdr (car said))))))))

(ert-deftest vm-imap-mock-test-creating-a-mailbox-leaves-the-parents-alone ()
  "REGRESSION: creating a mailbox inside a directory asks for that mailbox
and nothing else.  Issue #691: `vm-imap-create-mailbox' walked the name and
sent a CREATE for each parent first -- \"vmtest/\" for \"vmtest/saved\" -- which
a server refuses as a name in its own right, and then read one response too
many, so the session was out of step with what it had asked.  RFC 3501 has
the server make the parents."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder
     (vm-imap-mock-test--spec-for mock "Parent/Child"))
    (vm-imap-net-wait nil 10)
    (should (member "Parent/Child" (vm-imap-mock-mailbox-names mock)))
    (should (equal (vm-imap-mock-test--saved-subjects mock "Parent/Child")
                   '("badgers")))
    (should-not (vm-imap-mock-received-p mock "CREATE \"Parent/\""))))

;;; A tag of its own for every command (emacs-vm/vm#473)

(defun vm-imap-mock-test--tags (mock)
  "The tags of the commands MOCK received, in order."
  (delq nil (mapcar (lambda (line)
                      (when (string-match "\\`\\([^ ]+\\) " line)
                        (match-string 1 line)))
                    (vm-imap-mock-commands mock))))

(ert-deftest vm-imap-mock-test-every-command-carries-its-own-tag ()
  "Each command of a session is tagged differently.  Every one of them was
tagged `VM' until this was written, so a response could not be matched to
the command it answered -- which is why nothing could have two commands
outstanding, and why the asynchronous rewrite needs this first."
  (vm-imap-mock-test--visiting (mock :messages (list vm-imap-mock-test--alice))
    (let ((tags (vm-imap-mock-test--tags mock)))
      (should (> (length tags) 3))
      (should (equal (length tags) (length (delete-dups (copy-sequence tags)))))
      ;; and they are this session's, numbered from one
      (should (equal (car tags) "vm1"))
      (should-not (member "VM" tags)))))

;;; The buffer-type stack comes back the way it was (emacs-vm/vm#705)

(ert-deftest vm-imap-mock-test-a-bunch-costs-one-round-trip ()
  "Retrieval asks for a bunch of bodies and nothing else.
It used to ask the server for the size of the first message of every bunch,
which the bulk UID FETCH that opened the folder had already told it -- one
round trip per bunch, so 10,000 of them on a mailbox of 100,000 messages,
each one a wait on a server VM cannot do anything else during."
  (let ((messages (mapcar (lambda (n)
                            (format "From: sender%d@example.com\nSubject: m%d\n\nBody %d.\n"
                                    n n n))
                          (number-sequence 1 25)))
        (vm-imap-message-bunch-size 10))
    (vm-imap-mock-test--visiting (mock :messages messages)
      (should (equal (length vm-message-list) 25))
      (let ((commands (vm-imap-mock-commands mock)))
        ;; the sizes come with the UIDs and flags, in one command
        (should (cl-some (lambda (c)
                           (string-match-p "FETCH 1:25 (UID RFC822\\.SIZE FLAGS)" c))
                         commands))
        ;; and nothing asks for a size on its own
        (should-not (cl-some (lambda (c) (string-match-p "(RFC822\\.SIZE)" c))
                             commands))
        ;; three bunches, three fetches
        (should (equal (cl-count-if (lambda (c) (string-match-p "BODY\\.PEEK" c))
                                    commands)
                       3))))))

;;; a cache says its type in its name (issue #736)

(ert-deftest vm-imap-mock-test-a-new-cache-is-named-and-written-as-mboxcl2 ()
  "A cache VM creates says its type in its name, and is written in that type.
The file is VM's own and VM writes every message in it, so unlike any other
folder its type is known rather than guessed at.  The folder text is what
gets written, so a length on every message there is a cache that reads back
strictly."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (should (string-suffix-p vm-cache-folder-type-suffix buffer-file-name))
    (should (eq vm-folder-type 'mboxcl2))
    (should (equal (length vm-message-list) 2))
    (save-restriction
      (widen)
      (should (= 2 (how-many "^Content-Length:" (point-min) (point-max))))
      (goto-char (point-min))
      ;; two lengths in a row, each landing on the next message
      (should (vm-folder-looks-like-mboxcl2-p)))))

;;; what a background fetch says while it runs (issue #473)

(defmacro vm-imap-mock-test--shown (&rest body)
  "Run BODY and answer with the messages VM showed, oldest first.
What `vm-inform' put in the echo area, so a message above `vm-verbosity' --
logged and not shown -- does not appear here, which is the difference being
tested."
  (declare (indent 0) (debug t))
  `(let ((shown nil))
     (cl-letf (((symbol-function 'vm-emit-message)
                (lambda (level text)
                  (when (<= level vm-verbosity)
                    (setq shown (append shown (list text)))
                    text))))
       ,@body)
     shown))

(ert-deftest vm-imap-mock-test-a-fetch-says-the-start-and-the-end-only ()
  "A fetch running in the background does not talk over the echo area.
Whoever is using Emacs while it runs is the reason it runs in the background,
and a line per bunch of messages is in the way of them.  The count is in the
mode line, live, and every line is in the log.

So: the list is being read, the messages are being retrieved, and what
arrived.  Nothing per bunch, and the totals once rather than twice -- they
were printed on their own and again inside the line that followed."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (let* ((said (vm-imap-mock-test--shown
                   (vm-imap-net-synchronize t)
                   (vm-imap-net-wait nil 10)))
           (progress (seq-filter (lambda (s) (string-match-p "of [0-9]+ messages retrieved" s))
                                 said))
           (totals (seq-filter (lambda (s) (string-match-p "new, [0-9]+ unread" s))
                               said)))
      (should-not progress)
      (should (>= 1 (length totals))))))

;;; A keyword the server takes and does not keep (issue #601)

(defmacro vm-imap-mock-test--warnings (&rest body)
  "Run BODY with `vm-warn' captured, and answer with what it said."
  (declare (indent 0) (debug t))
  `(let ((said nil))
     (cl-letf (((symbol-function 'vm-warn)
                (lambda (_level _seconds format &rest args)
                  (push (apply #'format format args) said))))
       ,@body)
     (nreverse said)))

(ert-deftest vm-imap-mock-test-a-label-survives-a-server-that-keeps-no-keyword ()
  "REGRESSION: a synchronise against a server that keeps no keyword leaves the
folder's labels alone.

Issue #601.  The read-back gave each message exactly the keywords the server
reported, so against Gmail -- which takes a STORE of a keyword, answers OK and
keeps nothing -- the step that followed the save erased every label the save
had just failed to store.  The label was set here, absent there, and then
absent here too, which is why it looked as though setting it had done nothing."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice)
            :drops-keywords t)
    (let ((message (car vm-message-list)))
      (vm-set-decoded-labels-of message (list "important"))
      (vm-set-decoded-label-string-of message nil)
      (vm-imap-mock-test--warnings
        (vm-imap-synchronize)
        (vm-imap-net-wait nil 10))
      (should (equal (vm-decoded-labels-of message) (list "important")))
      ;; the server really did not keep it: that is the case being covered,
      ;; not a mock that quietly stored the keyword after all
      (should-not (member "important" (vm-imap-mock-flags mock "INBOX" 1))))))

(ert-deftest vm-imap-mock-test-a-server-that-keeps-keywords-still-removes-a-label ()
  "A server that carries keywords is still believed when it does not report
one: the label goes, which is how a label removed in another client reaches
this folder.

The other side of #601.  What decides it is whether any message in the mailbox
carries a keyword at all, so a mailbox with one keeps its say over the rest."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (let ((message (car vm-message-list)))
      (vm-imap-mock-set-flags mock "INBOX" 1 (list "important"))
      (vm-set-decoded-labels-of message (list "important" "urgent"))
      (vm-set-decoded-label-string-of message nil)
      (vm-imap-synchronize)
      (vm-imap-net-wait nil 10)
      (should (equal (vm-decoded-labels-of message) (list "important"))))))



;;; Expunging from the server what has been retrieved

;; `vm-expunge-imap-messages' sets \Deleted on every message the folder has
;; already retrieved and expunges.  Deleting the wrong one is not recoverable,
;; so what it must never do is touch a message that is not in
;; `vm-imap-retrieved-messages'.
;;
;; This is the blocking path, which runs only where the asynchronous driver
;; declines, so `vm-imap-net-expunge-retrieved' is stubbed to nil, as
;; vm-imap-mock-test--blocking does for retrieval.
;;
;; **The password has to be seeded.**  VM records a maildrop in
;; `vm-imap-retrieved-messages' with the password stripped
;; (`vm-imapdrop-sans-password'), so the session opened here has none and asks
;; for one.  In batch that blocks on stdin and no timer fires, which reads as
;; a hang with nothing to show for it.  The key `vm-imap-make-session' looks
;; the password up under is the maildrop without its password *and* without
;; its mailbox.

(defconst vm-imap-expunge-test--messages
  (list "From: a@example.com\nSubject: m1\n\nbody 1\n"
        "From: a@example.com\nSubject: m2\n\nbody 2\n"
        "From: a@example.com\nSubject: m3\n\nbody 3\n")
  "Three messages, so that one can be left alone.")

(defun vm-imap-expunge-test--subjects (mock)
  "The subjects MOCK's INBOX still holds."
  (mapcar (lambda (message)
            (let ((text (vm-imap-mock-message-text message)))
              (and (string-match "Subject: \\(m[0-9]\\)" text)
                   (match-string 1 text))))
          (vm-imap-mock-messages mock "INBOX")))

(ert-deftest vm-imap-mock-test-a-modified-cache-is-saved-on-exit ()
  "REGRESSION: `vm-save-folder-caches' writes a modified IMAP cache.

Issue #798.  An IMAP folder buffer is modified from the moment it is visited,
VM keeping each message\'s state in the message, so anything but a real quit
left the cache dirty and Emacs asked about it on exit.  The path it asks about
is a hash of the maildrop and means nothing to the reader; answering no throws
away the flags of everything since the last save.

Measured either side of the hook, and the file is checked rather than only the
buffer flag."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (let ((cache (current-buffer)))
      (should (buffer-modified-p cache))
      (vm-save-folder-caches)
      (should-not (buffer-modified-p cache))
      (should (string-match-p
               "badgers"
               (with-temp-buffer
                 (insert-file-contents (buffer-file-name cache))
                 (buffer-string)))))))

(ert-deftest vm-imap-mock-test-a-folder-the-reader-named-is-left-alone ()
  "The hook saves caches and nothing else.

A folder the reader chose is theirs, and Emacs asking whether to save it is
the right thing.  Only VM\'s own cache, under a name the reader never picked,
is written for them.  Without this the hook would be saving people\'s mail
folders behind their backs as Emacs exits."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-own" t)))
         (file (expand-file-name "myfolder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "From a@example.com Mon Jan  1 00:00:00 2024\n"
                    "From: a@example.com\nSubject: mine\n\nbody\n\n"))
          (vm-visit-folder file)
          (let ((mine (current-buffer)))
            (should-not (vm-cache-folder-name-p (buffer-file-name mine)))
            (set-buffer-modified-p t)
            (vm-save-folder-caches)
            (should (buffer-modified-p mine))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-imap-mock-test-the-exit-hook-is-registered ()
  "`vm-save-folder-caches' is on `kill-emacs-hook'.
The tests above call it directly; this is what makes Emacs call it."
  (should (memq 'vm-save-folder-caches
                (default-value 'kill-emacs-hook))))


;;; Where a first fetch leaves the reader (emacs-vm/vm#799)

(defconst vm-imap-bunch-test--count 25
  "Messages on the server, more than a bunch of them.")

(defun vm-imap-bunch-test--message (n &optional seen)
  "Message N, marked read on the server when SEEN."
  (let ((text (format "From: a@example.com\nTo: me@example.com\nSubject: msg %02d\n\nbody %d\n"
                      n n)))
    (if seen (cons text (list "\\Seen")) text)))

(defmacro vm-imap-bunch-test--fetching (spec &rest body)
  "Visit a fresh IMAP folder of `vm-imap-bunch-test--count' messages, run BODY.
SPEC is (SEEN BUNCH): SEEN messages are already read on the server and BUNCH is
`vm-imap-message-bunch-size'.  The cache directory is new, so this is a first
fetch.  BODY runs with the fetch still going and may call
`vm-imap-bunch-test--settle' to let it finish."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (mock :messages
                            (mapcar (lambda (n)
                                      (vm-imap-bunch-test--message
                                       n (<= n ,(car spec))))
                                    (number-sequence 1 vm-imap-bunch-test--count)))
     (let* ((cache (make-temp-file "vm-imap-bunch-cache" t))
            (vm-imap-folder-cache-directory cache)
            (vm-imap-server-timeout 20)
            (vm-imap-message-bunch-size ,(nth 1 spec))
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (before (buffer-list)))
       (unwind-protect
           (progn
             (vm-visit-imap-folder (vm-imap-mock-spec mock))
             ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory cache t)))))

(defun vm-imap-bunch-test--settle ()
  "Let the fetch finish."
  (vm-imap-net-wait nil 25))

(defun vm-imap-bunch-test--wait-for (n)
  "Wait until the folder holds at least N messages."
  (let ((tries 0))
    (while (and (< (length vm-message-list) n) (< tries 200))
      (setq tries (1+ tries))
      (accept-process-output nil 0.05))))

(defun vm-imap-bunch-test--at ()
  "The number of the message the folder is on, as a number.
`vm-number-of' answers a string."
  (and vm-message-pointer
       (string-to-number (vm-number-of (car vm-message-pointer)))))

(ert-deftest vm-imap-mock-test-a-read-folder-opens-at-its-last-message ()
  "REGRESSION: a fully read folder does not open at the bunch size.

Issue #799, reported by @l8gravely: a folder of three hundred and ninety read
messages opened at message ten, and at twenty when he changed
`vm-imap-message-bunch-size' to twenty.

A folder being fetched into for the first time has no current message, and a
command typed before one is chosen fails on nil, so `vm-imap-net-assimilate'
chooses one partway through.  With nothing new or unread to go to,
`vm-thoughtfully-select-message' falls back to the last message there is,
which at that moment is the last of the first bunch.  It then stood: the
second call, with the whole folder in hand, leaves a pointer that is not nil
alone.  And it was written to the cache as `X-VM-Bookmark', so every later
visit opened there as well.

Checked at three bunch sizes, because the number followed the option."
  (dolist (bunch '(5 10 20))
    (let ((vm-imap-message-bunch-size bunch))
      (vm-imap-bunch-test--fetching (25 bunch)
        (vm-imap-bunch-test--settle)
        (should (equal vm-imap-bunch-test--count (length vm-message-list)))
        (should (equal vm-imap-bunch-test--count (vm-imap-bunch-test--at)))))))

(ert-deftest vm-imap-mock-test-a-first-fetch-still-goes-to-the-first-unread ()
  "The fix does not touch the ordinary case.
`vm-jump-to-unread-messages' is what a folder with unread mail obeys, and the
manual describes it: the first new or unread message, whatever the bunch size.
Nine read on the server means message ten."
  (vm-imap-bunch-test--fetching (0 10)
    (vm-imap-bunch-test--settle)
    (should (equal 1 (vm-imap-bunch-test--at))))
  (vm-imap-bunch-test--fetching (9 10)
    (vm-imap-bunch-test--settle)
    (should (equal 10 (vm-imap-bunch-test--at)))))

(ert-deftest vm-imap-mock-test-a-reader-who-moves-during-a-fetch-is-left-there ()
  "A message the reader went to themselves is not taken away from them.

The safety half of #799.  The end of the fetch reconsiders only the message
the fetch itself chose, which it remembers in
`vm-imap-net-provisional-message'.  Someone who moved while the rest was
arriving keeps where they are, in a folder where the fix would otherwise have
sent them to the end."
  (vm-imap-bunch-test--fetching (25 10)
    (vm-imap-bunch-test--wait-for 3)
    (setq vm-message-pointer (nthcdr 2 vm-message-list))
    (should (equal 3 (vm-imap-bunch-test--at)))
    (vm-imap-bunch-test--settle)
    (should (equal 3 (vm-imap-bunch-test--at)))))


;;; Saying when a server will not keep a label (emacs-vm/vm#601)

(defun vm-imap-mock-test--label-and-save (label)
  "Put LABEL on the first message and send the flags, answering what was said.
The warning is the point, so `vm-net-warn' is captured rather than displayed."
  (let ((said nil))
    (cl-letf (((symbol-function 'vm-net-warn)
               (lambda (_level &rest args) (push (apply #'format args) said))))
      (setq vm-message-pointer vm-message-list)
      (vm-add-message-labels label 1)
      (vm-imap-net-save-attributes)
      (vm-imap-net-wait nil 10))
    (nreverse said)))

(ert-deftest vm-imap-mock-test-a-server-that-drops-labels-says-so ()
  "A label sent to a server that keeps no keywords is reported, not lost quietly.

emacs-vm/vm#601.  A server whose PERMANENTFLAGS does not offer `\\*' keeps no
keywords of its own.  It takes the STORE and answers OK all the same, so
nothing fails and nothing is refused: the label is simply gone the next time
the mailbox is read.  Gmail is such a server.

Said where a label is at risk rather than at every visit, so a reader who
sets none is never told, and once a folder rather than once a message."
  (let ((vm-imap-mock-permanent-flags
         "\\Answered \\Flagged \\Deleted \\Seen \\Draft"))
    (vm-imap-mock-test--visiting (mock :messages (list "From: a@b\n\nbody\n"))
      (let ((said (vm-imap-mock-test--label-and-save "urgent")))
        (should (equal 1 (length said)))
        (should (string-match-p "urgent" (car said)))
        (should (string-match-p "PERMANENTFLAGS" (car said)))
        ;; and it does not say it twice for the same folder
        (should (equal nil (vm-imap-mock-test--label-and-save "later")))))))

(ert-deftest vm-imap-mock-test-a-server-that-keeps-labels-says-nothing ()
  "The ordinary server advertises `\\*' and nothing is said.
A false warning would be worse than none: it would teach the reader to ignore
the real one."
  (vm-imap-mock-test--visiting (mock :messages (list "From: a@b\n\nbody\n"))
    (should (equal nil (vm-imap-mock-test--label-and-save "urgent")))))

(ert-deftest vm-imap-mock-test-a-system-flag-is-not-a-label ()
  "Marking a message read on such a server says nothing.
Only keywords are at risk; the system flags are in PERMANENTFLAGS by name."
  (let ((vm-imap-mock-permanent-flags
         "\\Answered \\Flagged \\Deleted \\Seen \\Draft"))
    (vm-imap-mock-test--visiting (mock :messages (list "From: a@b\n\nbody\n"))
      (let ((said nil))
        (cl-letf (((symbol-function 'vm-net-warn)
                   (lambda (_level &rest args) (push (apply #'format args) said))))
          (setq vm-message-pointer vm-message-list)
          (vm-set-labels (car vm-message-list) nil)
          (vm-set-unread-flag (car vm-message-list) nil)
          (vm-set-attribute-modflag-of (car vm-message-list) t)
          (vm-imap-net-save-attributes)
          (vm-imap-net-wait nil 10))
        (should (equal nil said))))))

(ert-deftest vm-imap-mock-test-which-flags-count-as-labels ()
  "A system flag begins with a backslash; a label is anything else."
  (should (equal '("urgent" "work")
                 (vm-imap-net-keywords-in '("\\Seen" "urgent" "\\Deleted" "work"))))
  (should (equal nil (vm-imap-net-keywords-in '("\\Seen" "\\Deleted"))))
  (should (equal nil (vm-imap-net-keywords-in nil))))

(provide 'vm-imap-mock-test)

;;; vm-imap-mock-test.el ends here


;;; Arriving mail whose body looks like a folder separator

;; `vm-imap-move-mail' is the IMAP maildrop path: mail on an IMAP server named
;; in `vm-spool-files', moved into a local folder.  It is the counterpart of
;; `vm-pop-move-mail', whose separator-shaped bodies are crossed with every
;; folder type in vm-pop-mock-test.el, and it had no test of any kind.
;;
;; What a folder cannot survive is a body its own type reads as a separator.
;; Retrieval is the path every received message takes, so it is where the
;; quoting has to happen.

(defconst vm-imap-mock-test--separator-bodies
  '(("plain"              . "an ordinary body line.")
    ("a From_ line"       . "text\nFrom nobody@example.com Mon Jan  1 00:00:00 2024")
    ("a From_ line first" . "From nobody@example.com Mon Jan  1 00:00:00 2024\nrest")
    ("an mmdf separator"  . "text\n\001\001\001\001\nmore")
    ("a babyl separator"  . "text\n\037\014\nmore")
    ("8-bit"              . "Gr\303\274\303\237e"))
  "Bodies that a folder of some type would otherwise read as a separator.")

(defun vm-imap-mock-test--message-with (n body)
  "A message numbered N carrying BODY."
  (concat "From: alice@example.com\nTo: vmtest@example.com\n"
          (format "Subject: message %d\nMessage-ID: <imap-%d@example.com>\n\n" n n)
          body "\n"))

(defun vm-imap-mock-test--messages-in (file type)
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

(defun vm-imap-mock-test--move-into (type body)
  "Move two messages, the first carrying BODY, into a folder of TYPE.
Answers a complaint, or nil when the folder reads back as the two messages
that were sent."
  (let ((dest (make-temp-file "vm-imap-mock-dest")))
    (unwind-protect
        (condition-case err
            (vm-imap-mock-with (mock :messages
                                     (list (vm-imap-mock-test--message-with 1 body)
                                           (vm-imap-mock-test--message-with 2 "second body")))
              (let ((vm-imap-server-timeout 10)
                    (vm-imap-ok-to-ask nil)
                    (vm-imap-expunge-after-retrieving t)
                    (vm-imap-retrieved-messages nil)
                    (vm-imap-auto-expunge-alist nil)
                    (vm-imap-max-message-size nil)
                    (vm-folder-type type))
                (vm-imap-move-mail (vm-imap-mock-spec mock) dest)
                (let ((read (vm-imap-mock-test--messages-in dest type)))
                  (cond ((not (eq (car read) type))
                         (format "%s / %s: read back as %s" type body (car read)))
                        ((/= 2 (cdr read))
                         (format "%s / %s: %d messages, not 2" type body (cdr read)))
                        (t nil)))))
          (error (format "%s / %s: %s" type body (error-message-string err))))
      (when (file-exists-p dest) (delete-file dest)))))

