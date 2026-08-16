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
empty buffer and time out."
  (declare (indent 1) (debug t))
  `(vm-imap-mock-with (,(car spec) ,@(cddr spec))
     (let* ((vm-imap-server-timeout 10)
            (,(cadr spec) (vm-imap-make-session (vm-imap-mock-spec ,(car spec))
                                                nil :purpose "test")))
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
                  ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory cache t)))))

;;; The session

(ert-deftest vm-imap-mock-test-a-session-logs-in-and-reads-capabilities ()
  "VM greets, asks CAPABILITY and logs in, in that order.
The capability list is what the rest of the session is decided by, so it has
to survive being parsed: the mock offers UIDPLUS and NAMESPACE."
  (vm-imap-mock-with (mock)
    (let* ((vm-imap-server-timeout 10)
           (process (vm-imap-make-session (vm-imap-mock-spec mock) nil
                                          :purpose "test")))
      (should (processp process))
      (unwind-protect
          (with-current-buffer (process-buffer process)
            (should (memq 'IMAP4REV1 vm-imap-capabilities))
            (should (memq 'UIDPLUS vm-imap-capabilities))
            (should (vm-imap-mock-received-p mock "\\`[^ ]+ CAPABILITY\\'"))
            (should (vm-imap-mock-received-p mock "LOGIN")))
        (delete-process process)
        (kill-buffer (process-buffer process))))))

(ert-deftest vm-imap-mock-test-a-bad-password-is-refused ()
  "A LOGIN the server answers NO gives no session, rather than a broken one."
  (vm-imap-mock-with (mock :password "secret")
    (let* ((vm-imap-server-timeout 10)
           (spec (replace-regexp-in-string ":secret\\'" ":wrong"
                                           (vm-imap-mock-spec mock)))
           (process (vm-imap-make-session spec nil :purpose "test")))
      (should-not (processp process))
      (should (vm-imap-mock-received-p mock "LOGIN")))))

(ert-deftest vm-imap-mock-test-selecting-a-mailbox-reports-what-is-in-it ()
  "SELECT gives back the message count and the UID validity.
Those two are what VM decides on: the count drives the fetch and a changed
UID validity means the cache it holds is worthless."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice
                                    vm-imap-mock-test--bob))
    (let ((result (vm-imap-select-mailbox process "INBOX" t)))
      (should (equal (nth 0 result) 2))         ; messages
      (should (equal (nth 2 result) "1000"))    ; uid validity
      (should (nth 3 result)))))                ; read-write

(ert-deftest vm-imap-mock-test-examine-selects-read-only ()
  "Asking for a read-only selection sends EXAMINE, and the mailbox comes back
read-only.  The server says which it gave in the [READ-ONLY] of its tagged
OK, and VM has to read it: believing a read-only mailbox writable is
believing it may store flags and expunge there."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice))
    (let ((result (vm-imap-select-mailbox process "INBOX" t t)))
      (should (equal (nth 0 result) 1))
      (should-not (nth 3 result))
      (should (vm-imap-mock-received-p mock "EXAMINE"))
      (should-not (vm-imap-mock-received-p mock "\\`[^ ]+ SELECT")))))

(ert-deftest vm-imap-mock-test-a-missing-mailbox-is-an-error ()
  "Selecting a mailbox the server does not have fails rather than pretending."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice))
    (should-error (vm-imap-select-mailbox process "no-such-box" t))))

(ert-deftest vm-imap-mock-test-uids-come-back-in-order ()
  "The UID list pairs each message number with the server's UID for it.
This is the mapping the whole IMAP folder is built on: a number is only good
for this session, a UID is what survives one."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice
                                    vm-imap-mock-test--bob))
    (vm-imap-select-mailbox process "INBOX" t)
    (let ((uids (vm-imap-get-uid-list process 1 2)))
      (should (equal (sort (mapcar #'car uids) #'<) '(1 2)))
      (should (equal (cdr (assq 1 uids)) "1"))
      (should (equal (cdr (assq 2 uids)) "2")))))

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

(ert-deftest vm-imap-mock-test-storing-a-flag-reaches-the-server ()
  "A flag VM sets is a STORE the server sees, and it sticks."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice))
    (vm-imap-select-mailbox process "INBOX" t)
    (vm-imap-send-command process "UID STORE 1 +FLAGS.SILENT (\\Deleted)")
    (should (vm-imap-read-ok-response process))
    (should (member "\\Deleted" (vm-imap-mock-flags mock "INBOX" 1)))))

(ert-deftest vm-imap-mock-test-expunge-removes-only-the-deleted ()
  "EXPUNGE takes away the messages flagged \\Deleted and leaves the rest."
  (vm-imap-mock-test--with-session
      (mock process :messages (list (cons vm-imap-mock-test--alice '("\\Deleted"))
                                    vm-imap-mock-test--bob))
    (vm-imap-select-mailbox process "INBOX" nil)
    (vm-imap-send-command process "EXPUNGE")
    (should (vm-imap-read-expunge-response process))
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 1))
    (should (equal (vm-imap-mock-message-uid
                    (car (vm-imap-mock-messages mock "INBOX")))
                   2))))

;;; When the server does not play along

(ert-deftest vm-imap-mock-test-a-refused-select-is-an-error ()
  "A SELECT the server answers NO stops the session with an error rather
than leaving VM to fetch from a mailbox it never selected."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice)
            :refuse "SELECT")
    (should-error (vm-imap-select-mailbox process "INBOX" t))))

(ert-deftest vm-imap-mock-test-a-dropped-connection-is-noticed ()
  "A server that hangs up mid-command is an error, not a wait for a
response that is never coming."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice)
            :drop-on "SELECT")
    (should-error (vm-imap-select-mailbox process "INBOX" t))))

(ert-deftest vm-imap-mock-test-a-session-survives-a-server-without-uidplus ()
  "UIDPLUS is an extension, and a server without it is still usable.
VM asks for capabilities before it asks for anything else, so what it does
with a shorter list is worth knowing."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice) :no-uidplus t)
    (should-not (memq 'UIDPLUS vm-imap-capabilities))
    (should (memq 'IMAP4REV1 vm-imap-capabilities))
    (should (equal (nth 0 (vm-imap-select-mailbox process "INBOX" t)) 1))))

(ert-deftest vm-imap-mock-test-a-truncated-fetch-is-an-error ()
  "A download the server cuts off short is an error, not half a message.
The connection goes with it, so what VM says is that it is not connected --
the point being that the folder does not end up holding the fragment."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice)
                           :truncate-fetch t)
    (let* ((cache (make-temp-file "vm-imap-mock-cache" t))
           (vm-imap-folder-cache-directory cache)
           (vm-imap-server-timeout 10)
           (vm-frame-per-folder nil)
           (vm-mutable-frame-configuration nil)
           (before (buffer-list)))
      (unwind-protect
          (should-error (vm-visit-imap-folder (vm-imap-mock-spec mock)))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (delete-directory cache t)))))

;;; Mailboxes

(ert-deftest vm-imap-mock-test-listing-the-mailboxes ()
  "LIST names the mailboxes on the server."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice))
    (vm-imap-mock-add-message mock "Archive" vm-imap-mock-test--bob)
    (let ((names (vm-imap-mailbox-list process nil)))
      (should (member "INBOX" names))
      (should (member "Archive" names)))))

(ert-deftest vm-imap-mock-test-creating-and-deleting-a-mailbox ()
  "CREATE makes a mailbox and DELETE takes it away, and the server agrees
with VM about which ones exist afterwards."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice))
    (vm-imap-create-mailbox process "Later")
    (should (member "Later" (vm-imap-mock-mailbox-names mock)))
    (vm-imap-delete-mailbox process "Later")
    (should-not (member "Later" (vm-imap-mock-mailbox-names mock)))))

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
    (should (equal (length vm-message-list) 2))
    (vm-get-new-mail)
    (should (equal (length vm-message-list) 2))))

(ert-deftest vm-imap-mock-test-expunging-what-has-been-retrieved ()
  "`vm-expunge-imap-messages' deletes from the server what has been
retrieved, and leaves the local copies alone.  It flags each UID \\Deleted
and closes the mailbox, which is what expunges them."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (should (equal (length (vm-imap-mock-messages mock "INBOX")) 2))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-expunge-imap-messages))
    (should (equal (vm-imap-mock-messages mock "INBOX") nil))
    (should (vm-imap-mock-received-p mock "STORE .*\\\\Deleted"))
    ;; the local folder still has them: this deletes from the server only
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))))

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
    (should (equal (length vm-imap-retrieved-messages) 2))
    (setf (vm-imap-mock-message-expunged
           (car (vm-imap-mock-messages mock "INBOX")))
          t)
    (vm-prune-imap-retrieved-list (vm-imap-mock-spec mock))
    (should (equal (length vm-imap-retrieved-messages) 1))
    ;; the local messages are untouched: this prunes a memo, not the mail
    (should (equal (mapcar #'vm-su-subject vm-message-list)
                   '("badgers" "otters")))))

(ert-deftest vm-imap-mock-test-pruning-keeps-what-the-server-still-has ()
  "With everything still on the server, nothing is forgotten."
  (vm-imap-mock-test--spooling
      (mock :messages (list vm-imap-mock-test--alice vm-imap-mock-test--bob))
    (vm-get-new-mail)
    (vm-prune-imap-retrieved-list (vm-imap-mock-spec mock))
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

(ert-deftest vm-imap-mock-test-creating-a-mailbox ()
  "`vm-create-imap-folder' makes the mailbox its spec names, and the server
has it afterwards."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-imap-server-timeout 10))
      (should (equal (vm-imap-mock-mailbox-names mock) '("INBOX")))
      (vm-create-imap-folder (vm-imap-mock-test--spec-for mock "Later"))
      (should (member "Later" (vm-imap-mock-mailbox-names mock)))
      (should (vm-imap-mock-received-p mock "CREATE")))))

(ert-deftest vm-imap-mock-test-creating-a-mailbox-that-exists ()
  "Making a mailbox that is already there is refused by the server, and VM
says so rather than reporting success."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-imap-server-timeout 10))
      (should-error (vm-create-imap-folder
                     (vm-imap-mock-test--spec-for mock "INBOX"))))))

(ert-deftest vm-imap-mock-test-renaming-a-mailbox ()
  "`vm-rename-imap-folder' renames it on the server, and what was in it is
still in it under the new name."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-imap-server-timeout 10))
      (vm-imap-mock-add-message mock "Archive" vm-imap-mock-test--bob)
      (vm-rename-imap-folder (vm-imap-mock-test--spec-for mock "Archive")
                             (vm-imap-mock-test--spec-for mock "Old"))
      (should (member "Old" (vm-imap-mock-mailbox-names mock)))
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
      (should-not (member "Archive" (vm-imap-mock-mailbox-names mock)))
      (should (member "INBOX" (vm-imap-mock-mailbox-names mock)))
      (should (vm-imap-mock-received-p mock "DELETE")))))

(ert-deftest vm-imap-mock-test-deleting-a-mailbox-that-is-not-there ()
  "Deleting a mailbox the server does not have is reported, not passed over.
The mock answers NO, which is what a server does, and VM has to notice."
  (vm-imap-mock-with (mock :messages (list vm-imap-mock-test--alice)
                           :refuse "DELETE")
    (let ((vm-imap-server-timeout 10))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (vm-delete-imap-folder
                       (vm-imap-mock-test--spec-for mock "Nowhere"))))
      (should (equal (vm-imap-mock-mailbox-names mock) '("INBOX"))))))

;;; Saving a message to an IMAP folder

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
             (write-region
              (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                      vm-imap-mock-test--alice "\n"
                      "From bob@example.com Sat Aug  8 14:25:13 2026\n"
                      vm-imap-mock-test--bob "\n")
              nil folder nil 'quiet)
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

(ert-deftest vm-imap-mock-test-saving-remembers-the-folder-it-saved-to ()
  "`vm-last-save-imap-folder' is what the next save offers, so it is the
folder that was saved to and not the one before it."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (let ((target (vm-imap-mock-test--spec-for mock "Saved")))
      (vm-save-message-to-imap-folder target)
      (should (equal vm-last-save-imap-folder target)))))

(ert-deftest vm-imap-mock-test-saving-without-a-count-saves-one-message ()
  "Called from Lisp with no count, one message is saved: the count comes
from the prefix argument, and defaulting it to nothing would save the whole
folder or none of it."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder (vm-imap-mock-test--spec-for mock "Saved"))
    (should (equal (length (vm-imap-mock-messages mock "Saved")) 1))))

(ert-deftest vm-imap-mock-test-saving-a-count-of-two-saves-both ()
  "A count of two saves this message and the next, in that order."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder
     (vm-imap-mock-test--spec-for mock "Saved") 2)
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
    (should (equal (vm-imap-mock-test--saved-subjects mock "Saved")
                   '("otters")))))

(ert-deftest vm-imap-mock-test-deleting-after-saving-is-a-setting ()
  "`vm-delete-after-saving' deletes the message that was saved and no other,
and leaves it alone when it is off -- a folder emptied by a save nobody asked
to empty is not a small mistake."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (let ((target (vm-imap-mock-test--spec-for mock "Saved")))
      (vm-save-message-to-imap-folder target)
      (should-not (vm-deleted-flag (car vm-message-list)))
      (let ((vm-delete-after-saving t))
        (vm-save-message-to-imap-folder target))
      (should (vm-deleted-flag (car vm-message-list)))
      (should-not (vm-deleted-flag (nth 1 vm-message-list))))))

(ert-deftest vm-imap-mock-test-saving-ends-the-session-it-opened ()
  "The session opened for the save is closed again: VM keeps no connection
for a command that is over, and a server counts them (dovecot's
`mail_max_userip_connections')."
  (vm-imap-mock-test--saving-from-a-file (mock)
    (vm-save-message-to-imap-folder (vm-imap-mock-test--spec-for mock "Saved"))
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
      (should (equal (vm-imap-mock-test--saved-subjects mock "Saved")
                     '("badgers")))
      (should (vm-imap-mock-received-p mock "UID COPY"))
      (should-not (vm-imap-mock-received-p mock "APPEND"))
      ;; and the message is not fetched to be copied: the point of asking the
      ;; server to do it is that the message never comes down
      (should-not (vm-imap-mock-received-p mock "BODY\\[\\]"))
      (should (vm-filed-flag (car vm-message-list))))))

(ert-deftest vm-imap-mock-test-copying-wants-the-mailbox-to-exist-already ()
  "Saving from an IMAP folder to a mailbox that is not there fails, where
saving to it from a file folder creates it: the copy path issues UID COPY and
takes the server's NO, and only the append path sends CREATE first."
  (vm-imap-mock-test--visiting
      (mock :messages (list vm-imap-mock-test--alice))
    (let ((vm-delete-after-saving nil)
          (vm-last-save-imap-folder nil))
      (should-error (vm-save-message-to-imap-folder
                     (vm-imap-mock-test--spec-for mock "Nowhere")))
      (should-not (member "Nowhere" (vm-imap-mock-mailbox-names mock))))))

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
        (should (equal (length said) 1))
        (should (equal (car (car said)) 5))
        (should (string-match-p "\\`1 message saved to " (cdr (car said))))
        (setq said nil)
        (vm-save-message-to-imap-folder target 2)
        (should (string-match-p "\\`2 messages saved to " (cdr (car said))))))))

(provide 'vm-imap-mock-test)

;;; vm-imap-mock-test.el ends here
