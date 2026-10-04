;;; vm-imap-fuzz-test.el --- random sequences against an IMAP server -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; `vm-fuzz-test.el' drives a local folder through random sequences of the
;; commands that rearrange it.  This does the same for an IMAP folder on the
;; driver, and mixes in the other half of the problem: what somebody else does
;; to the mailbox while VM is working on it.  The operations are a reader's --
;; delete, expunge, mark read, save, get new mail, synchronise, load a body --
;; and the server's -- another client expunging a message, setting a flag,
;; delivering mail, or refusing the next command.
;;
;; After every operation the folder has to say something true about itself:
;;
;;   - every message has a UID, and no two have the same one.  A UID names one
;;     message for as long as the UIDVALIDITY holds, so two of them is one
;;     message held twice: two summary lines and one message on the server.
;;   - every message belongs to the mailbox this folder is looking at.
;;   - the message list matches the messages in the buffer.  A wrong splice
;;     leaves the two disagreeing without either looking wrong alone.
;;   - nothing is left running and nothing is left queued once the folder is
;;     quiet, or the next operation is working against a session it cannot see.
;;
;; The operation is allowed to fail: a refused command is a refused command.
;; What it is not allowed to do is leave the folder saying something untrue.
;;
;; What it catches, calibrated by breaking things on purpose: a server that
;; accepts a STORE and quietly does not apply it is caught by every seed tried,
;; and the second test below breaks the folder three ways to show the structural
;; checks fire.  What it does not reach is the flags going to the wrong message
;; through a stale sequence number: that needs the folder's number table
;; populated and stale at once, and the sequences do not line up on it --
;; `vm-imap-net-test-a-flag-lands-on-the-message-it-was-meant-for' is the test
;; for that one.
;;
;; The sequences are seeded, so a failure names the sequence that caused it.
;; The counts are small by default, a few seconds for the file; raise them from
;; the environment to search harder:
;;
;;     cd test && VM_IMAP_FUZZ_SEEDS=40 VM_IMAP_FUZZ_OPS=40 \
;;       make test-one testel=vm-imap-fuzz-test.el

;;; Code:

(require 'cl-lib)
(require 'vm-test-init)
(require 'vm-imap-mock)
(require 'vm-imap-net)

(defvar vm-imap-fuzz-test-seeds
  (string-to-number (or (getenv "VM_IMAP_FUZZ_SEEDS") "4"))
  "How many random sequences the fuzz test runs.")

(defvar vm-imap-fuzz-test-ops
  (string-to-number (or (getenv "VM_IMAP_FUZZ_OPS") "14"))
  "How many operations each random sequence performs.")

(defvar vm-imap-fuzz-test--refused nil
  "UIDs whose flags the server refused while a sequence ran.

A server that refuses a keyword has VM drop it rather than offer it for ever
(issue #391), so the folder ends up saying a message is read while the server
says it is not -- deliberately.  Those messages are left out of the check that
the two agree; every other message is not.")

(defconst vm-imap-fuzz-test--messages
  (list "From: a@example.com\nSubject: alpha\n\nThe first body.\n"
        "From: b@example.com\nSubject: beta\n\nThe second body.\n"
        "From: c@example.com\nSubject: gamma\n\nThe third body.\n")
  "What the mailbox holds when a sequence starts.")

;;; The invariants

(defun vm-imap-fuzz-test--flag-complaints (mock)
  "Where the folder and MOCK disagree about what has been read.

One way only: a message VM has as read, whose changes have all gone up, must
be read on the server too.  That is the direction VM is responsible for, and
it is where the flags went wrong -- stored against a sequence number the
mailbox had moved on from, they landed on another message, and both folder and
server looked self-consistent afterwards.

The other direction is legitimate: another client marks a message read and the
folder does not know until it next synchronises.  A message with changes still
pending is left alone, and so is one the server no longer has."
  (let ((wrong nil)
        (flags (make-hash-table :test 'equal)))
    ;; the flags are consed onto a t, so that a message with none is still
    ;; found: the value nil for "no flags" read as "not on the server", and the
    ;; check passed over exactly the messages it was written for
    (dolist (m (vm-imap-mock-messages mock "INBOX"))
      (puthash (number-to-string (vm-imap-mock-message-uid m))
               (cons t (mapcar #'downcase (vm-imap-mock-message-flags m)))
               flags))
    (dolist (m vm-message-list)
      (let ((uid (vm-imap-uid-of m)))
        ;; "read" is what `vm-imap-message-flag-changes' means by it: not
        ;; unread.  A new message is not exempt -- new is how it arrived, and
        ;; the reader can have read it since.
        (when (and uid (gethash uid flags)
                   (not (member uid vm-imap-fuzz-test--refused))
                   (not (vm-attribute-modflag-of m))
                   (not (vm-unread-flag m))
                   (not (member "\\seen" (cdr (gethash uid flags)))))
          (push (format "UID %s is read here and unread on the server" uid)
                wrong))))
    wrong))

(defun vm-imap-fuzz-test--complaints ()
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
      (push "a session was still running when the folder was quiet" wrong))
    (when vm-imap-net-waiting
      (push (format "%d pieces of work left queued"
                    (length vm-imap-net-waiting))
            wrong))
    wrong))

;;; The operations

(defun vm-imap-fuzz-test--quiet (&optional seconds)
  "Wait for the folder's session and whatever it queued behind it."
  (let ((deadline (+ (float-time) (or seconds 15))))
    (while (and (or (vm-imap-net-busy-p) vm-imap-net-waiting)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defun vm-imap-fuzz-test--a-message ()
  "A message of the folder, chosen at random, or nil if it has none."
  (and vm-message-list
       (nth (random (length vm-message-list)) vm-message-list)))

(defconst vm-imap-fuzz-test--reader-weights
  ;; Weighted rather than uniform: what the sequences have to reach often is
  ;; marking a message read and then saving, since that is where the folder
  ;; tells the server something and can tell it about the wrong message.
  '((mark-read . 4) (save . 4) (get-new-mail . 2) (synchronize . 2)
    (delete . 2) (expunge . 2) (undelete . 1))
  "How often each reader operation is drawn, by weight.")

(defun vm-imap-fuzz-test--draw (weights)
  "One key from WEIGHTS, an alist of (KEY . WEIGHT), at random."
  (let* ((total (apply #'+ (mapcar #'cdr weights)))
         (n (random total))
         (choice nil))
    (dolist (pair weights (or choice (car (car weights))))
      (when (and (null choice) (< n (cdr pair)))
        (setq choice (car pair)))
      (setq n (- n (cdr pair))))))

(defun vm-imap-fuzz-test--message-by-uid (uid)
  "The folder's message with UID, or nil."
  (car (seq-filter (lambda (m) (equal (vm-imap-uid-of m) uid))
                   vm-message-list)))

(defun vm-imap-fuzz-test--reader-operation (choice &optional uid)
  "Do reader operation CHOICE in the current folder, and describe it.
UID says which message to do it to, for a replay; without one a message is
chosen at random.  Every description carries the UID it used, so a sequence
that broke something can be run again."
  (let ((m (if uid
               (vm-imap-fuzz-test--message-by-uid uid)
             (vm-imap-fuzz-test--a-message))))
    (cond
     ((null m) (list 'folder-empty))
     ((eq choice 'delete) (vm-set-deleted-flag m t)
      (list 'delete (vm-imap-uid-of m)))
     ((eq choice 'undelete) (vm-set-deleted-flag m nil)
      (list 'undelete (vm-imap-uid-of m)))
     ((eq choice 'expunge) (vm-expunge-folder :quiet t) (list 'expunge))
     ((eq choice 'mark-read)
      ;; read, and not a toggle: the invariant is about what VM says it has
      ;; read, and a toggle would spend half its turns taking \Seen off
      (vm-set-unread-flag m nil)
      (vm-set-new-flag m nil)
      (vm-set-attribute-modflag-of m t)
      (list 'mark-read (vm-imap-uid-of m)))
     ((eq choice 'save) (set-buffer-modified-p t) (vm-save-folder) (list 'save))
     ((eq choice 'get-new-mail) (vm-get-new-mail) (list 'get-new-mail))
     ((eq choice 'synchronize) (vm-imap-net-synchronize nil t) (list 'synchronize))
     (t (list 'nothing)))))

(defun vm-imap-fuzz-test--server-message (mock uid)
  "MOCK's message with UID, or one at random when UID is nil."
  (let ((messages (vm-imap-mock-messages mock "INBOX")))
    (cond ((null messages) nil)
          (uid (car (seq-filter
                     (lambda (m) (equal (vm-imap-mock-message-uid m) uid))
                     messages)))
          (t (nth (random (length messages)) messages)))))

(defun vm-imap-fuzz-test--server-operation (mock choice &optional uid)
  "Do something to MOCK behind VM's back, and describe it.
UID says which of the server's messages, or which command to refuse, so that a
reported sequence can be run again."
  (let* ((messages (vm-imap-mock-messages mock "INBOX"))
         (m (vm-imap-fuzz-test--server-message mock uid)))
    (cond
     ((and (= choice 0) m)
      ;; another client deletes one
      (setf (vm-imap-mock-message-expunged m) t)
      (list 'server-expunged (vm-imap-mock-message-uid m)))
     ((and (= choice 1) m)
      (vm-imap-mock--set-flags m "+" (list "\\Seen"))
      (list 'server-marked-read (vm-imap-mock-message-uid m)))
     ((= choice 2)
      (vm-imap-mock-add-message
       mock "INBOX"
       (format "From: new%d@example.com\nSubject: arrival %d\n\nMore.\n"
               (random 1000) (random 1000)))
      (list 'server-delivered))
     ((= choice 3)
      ;; commands of one kind are refused until something clears it
      (setf (vm-imap-mock-refuse mock)
            (or (and (stringp uid) uid)
                (nth (random 3) '("STORE" "EXPUNGE" "FETCH"))))
      (list 'server-refuses (vm-imap-mock-refuse mock)))
     (t (setf (vm-imap-mock-refuse mock) nil)
        (list 'server-behaves)))))

(defun vm-imap-fuzz-test--note-refusals (mock)
  "Note the messages whose flags this server will refuse to take.

A server refusing STORE has VM drop the flag rather than offer it for ever
(issue #391), and any operation that talks to the server can be the one that
tries it -- a save, a fetch, a synchronisation.  What is pending when the
refusal is in force is what the folder will stop asking about, so those
messages are left out of the check that folder and server agree."
  (when (and (vm-imap-mock-refuse mock)
             (string-match-p "STORE" (vm-imap-mock-refuse mock)))
    (dolist (message vm-message-list)
      (when (vm-attribute-modflag-of message)
        (push (vm-imap-uid-of message) vm-imap-fuzz-test--refused)))))

(defun vm-imap-fuzz-test--operate (folder mock)
  "Perform one random operation on FOLDER or MOCK, and describe it."
  (with-current-buffer folder
    (vm-imap-fuzz-test--note-refusals mock)
    (let ((what (if (< (random 10) 7)
                    (vm-imap-fuzz-test--reader-operation
                     (vm-imap-fuzz-test--draw
                      vm-imap-fuzz-test--reader-weights))
                  (vm-imap-fuzz-test--server-operation mock (random 5)))))
      (vm-imap-fuzz-test--quiet)
      what)))

;;; Running a sequence

(defmacro vm-imap-fuzz-test--with-folder (spec &rest body)
  "Visit a mock IMAP folder and run BODY in it, with MOCK bound.
SPEC is (MOCK-VAR FOLDER-VAR)."
  (declare (indent 1) (debug t))
  `(let* ((,(car spec) (vm-imap-mock-start
                        :messages vm-imap-fuzz-test--messages))
          (cache (make-temp-file "vm-imap-fuzz-cache" t))
          (vm-imap-folder-cache-directory cache)
          (vm-imap-server-timeout 10)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-imap-message-bunch-size 2)
          (vm-enable-external-messages '(imap))
          (vm-confirm-quit nil)
          (before (buffer-list))
          (,(nth 1 spec) nil))
     (unwind-protect
         (progn
           (vm-visit-imap-folder (vm-imap-mock-spec ,(car spec)))
           (setq ,(nth 1 spec) (current-buffer))
           (vm-imap-fuzz-test--quiet)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (vm-imap-mock-stop ,(car spec))
       (delete-directory cache t))))

(defconst vm-imap-fuzz-test--server-choices
  '((server-expunged . 0) (server-marked-read . 1) (server-delivered . 2)
    (server-refuses . 3) (server-behaves . 4))
  "Which number each server operation is, for replaying one by name.")

(defun vm-imap-fuzz-test--redo (folder mock step)
  "Do STEP again, one entry of a history a sequence reported."
  (let ((what (car step))
        (target (cadr step)))
    (with-current-buffer folder
      (cond
       ((assq what vm-imap-fuzz-test--server-choices)
        (vm-imap-fuzz-test--server-operation
         mock (cdr (assq what vm-imap-fuzz-test--server-choices))
         target))
       ((memq what '(folder-empty nothing)) nil)
       (t (vm-imap-fuzz-test--reader-operation
           what (and (stringp target) target))))
      (vm-imap-fuzz-test--quiet))))

(defun vm-imap-fuzz-test-replay (history)
  "Run HISTORY, a sequence a fuzz failure reported, and say what broke.

For chasing what the sequences find: the report names every operation and the
message it was done to, and this does them again in order.  The mailbox starts
as the sequences start it, so the UIDs mean the same thing."
  (let ((problems nil)
        (vm-imap-fuzz-test--refused nil))
    (vm-imap-fuzz-test--with-folder (mock folder)
      (catch 'broken
        (dolist (step history)
          (with-current-buffer folder (vm-imap-fuzz-test--note-refusals mock))
          (vm-imap-fuzz-test--redo folder mock step)
          (setq problems (with-current-buffer folder
                           (append (vm-imap-fuzz-test--complaints)
                                   (vm-imap-fuzz-test--flag-complaints mock))))
          (when problems
            (setq problems (cons (format "at %S" step) problems))
            (throw 'broken nil)))))
    problems))

(defun vm-imap-fuzz-test--run (seed)
  "Run one seeded sequence, and answer with what broke, if anything."
  (random (format "vm-imap-fuzz-%d" seed))
  (let ((history nil)
        (vm-imap-fuzz-test--refused nil)
        (problems nil))
    (vm-imap-fuzz-test--with-folder (mock folder)
      (catch 'broken
        (dotimes (_ vm-imap-fuzz-test-ops)
          (unless (buffer-live-p folder) (throw 'broken nil))
          (push (vm-imap-fuzz-test--operate folder mock) history)
          (setq problems (with-current-buffer folder
                           (append (vm-imap-fuzz-test--complaints)
                                   (vm-imap-fuzz-test--flag-complaints mock))))
          (when problems (throw 'broken nil)))))
    (when problems
      (format "seed %d, after %S:\n  %s"
              seed (reverse history)
              (mapconcat #'identity (delete-dups problems) "\n  ")))))

;;; The tests

(ert-deftest vm-imap-fuzz-test-a-folder-survives-random-operations ()
  "Random reader commands and server changes leave the folder consistent.

Checked after every operation rather than at the end: an invariant that breaks
and is then tidied up by the next synchronisation would otherwise pass."
  (let ((broken nil))
    (dotimes (seed vm-imap-fuzz-test-seeds)
      (let ((problem (vm-imap-fuzz-test--run seed)))
        (when problem (push problem broken))))
    (should (equal nil (nreverse broken)))))

(ert-deftest vm-imap-fuzz-test-the-check-catches-a-broken-folder ()
  "The invariants themselves, against folders broken on purpose.

A green run above says the folder was looked at and found sound, which is
worth as much as the looking."
  (vm-imap-fuzz-test--with-folder (mock folder)
    (should (equal (length vm-message-list) 3))
    (should-not (vm-imap-fuzz-test--complaints))
    ;; a message with no UID
    (let ((uid (vm-imap-uid-of (car vm-message-list))))
      (vm-set-imap-uid-of (car vm-message-list) nil)
      (should (vm-imap-fuzz-test--complaints))
      (vm-set-imap-uid-of (car vm-message-list) uid))
    ;; two messages with one UID
    (let ((uid (vm-imap-uid-of (cadr vm-message-list))))
      (vm-set-imap-uid-of (cadr vm-message-list)
                          (vm-imap-uid-of (car vm-message-list)))
      (should (vm-imap-fuzz-test--complaints))
      (vm-set-imap-uid-of (cadr vm-message-list) uid))
    ;; a list that does not match the buffer
    (let ((all vm-message-list))
      (setq vm-message-list (cdr vm-message-list))
      (should (vm-imap-fuzz-test--complaints))
      (setq vm-message-list all))
    (should-not (vm-imap-fuzz-test--complaints))))

(provide 'vm-imap-fuzz-test)

;;; vm-imap-fuzz-test.el ends here
