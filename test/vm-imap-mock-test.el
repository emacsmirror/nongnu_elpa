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

(ert-deftest vm-imap-mock-test-examine-is-sent-for-a-read-only-select ()
  "Asking for a read-only selection sends EXAMINE rather than SELECT.
What VM makes of the [READ-ONLY] it gets back is a separate matter, and a
broken one -- see the test in vm-imap-test.el for
`vm-imap-response-matches'."
  (vm-imap-mock-test--with-session
      (mock process :messages (list vm-imap-mock-test--alice))
    (let ((result (vm-imap-select-mailbox process "INBOX" t t)))
      (should (equal (nth 0 result) 1))
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

(provide 'vm-imap-mock-test)

;;; vm-imap-mock-test.el ends here
