;;; vm-mboxcl2-test.el --- An mboxcl2 folder stays readable -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; One invariant, checked across the operations that write a folder: after any
;; of them, an mboxcl2 folder still reads as mboxcl2, with the messages it
;; should have and the bodies they should have.
;;
;; mboxcl2 stores each message's length in a `Content-Length' header and finds
;; the next message by it, so an operation that writes a message without one,
;; or leaves a stale one behind, produces a folder that cannot be read back --
;; and until `vm-mboxcl2-strict' the reader guessed instead of saying so.  The
;; formats are otherwise identical, which is what makes this easy to get wrong
;; in a path nobody exercised with mboxcl2.

;;; Code:

(require 'vm-test-init)
(require 'vm-folder)
(require 'vm-save)
(require 'vm-edit)
(require 'vm-digest)
(require 'vm-sort)
(require 'vm-delete)
(require 'vm-undo)
(require 'vm-mime)

(defun vm-mboxcl2-test--write (file messages)
  "Write MESSAGES to FILE as an mboxcl2 folder.
MESSAGES is a list of (SUBJECT . BODY), or of (SUBJECT HEADERS . BODY) where
HEADERS is extra header lines -- the MIME ones, for a message with an
attachment, which have to be headers and not the first lines of the body.
Each message gets a correct Content-Length, so the folder starts out sound."
  (with-temp-buffer
    (dolist (message messages)
      (let* ((rest (cdr message))
             (extra (if (consp rest) (car rest) ""))
             (body (if (consp rest) (cdr rest) rest)))
        (insert "From sender@example.com Sat Aug  8 14:24:13 2026\n"
                "From: sender@example.com\n"
                "Subject: " (car message) "\n"
                extra
                (format "Content-Length: %d\n" (string-bytes body))
                "\n" body)))
    (write-region (point-min) (point-max) file nil 'quiet)))

(defun vm-mboxcl2-test--read (file)
  "Read FILE strictly and return its messages as (SUBJECT . BODY).
Strictly: `vm-mboxcl2-strict' is t, so a message with no length signals here
rather than being guessed at, which is the point of the check.

Widened: a folder buffer is narrowed to the message being previewed, and the
others are outside the accessible portion."
  (let ((vm-mboxcl2-strict t)
        (buffer (vm-get-file-buffer file)))
    ;; Read what is on disk.  A folder VM still has open would answer from
    ;; its buffer, and an operation that never reached the file would pass.
    (when buffer
      (with-current-buffer buffer
        (when (buffer-modified-p) (vm-save-folder))
        (set-buffer-modified-p nil))
      (kill-buffer buffer))
    (should (eq (vm-get-folder-type file) 'mboxcl2))
    (vm-mboxcl2-test--lengths-are-right file)
    (vm-visit-folder file)
    (prog1 (save-restriction
             (widen)
             (mapcar (lambda (m)
                       (cons (vm-su-subject m)
                             (buffer-substring-no-properties
                              (vm-text-of m) (vm-text-end-of m))))
                     vm-message-list))
      (set-buffer-modified-p nil))))

(defun vm-mboxcl2-test--lengths-are-right (file)
  "Signal unless FILE is a chain of messages its own lengths describe.
Walks it the way the format says to: a separator, headers, a blank line, then
exactly `Content-Length' bytes of body, and then either the end of the file or
the next separator.  A body containing a line that begins \"From \" is fine by
that rule and would break any check that split on separators, which is the
point of the format.

Reading the folder back with VM is not enough on its own: VM recovers from a
wrong length by looking for the next separator, so a stale one passes there
while another mailer, which believes the header, takes the wrong bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (let ((n 0))
      (while (not (eobp))
        (setq n (1+ n))
        (should (looking-at "From "))
        (let ((headers-end (save-excursion (search-forward "\n\n" nil t)))
              claimed)
          (should headers-end)
          (save-excursion
            (should (re-search-forward "^Content-Length: *\\([0-9]+\\)"
                                       headers-end t))
            (setq claimed (string-to-number (match-string 1))))
          (goto-char headers-end)
          ;; the body is exactly that many bytes, and the next thing is
          ;; another message or the end of the folder
          (should (<= (+ (point) claimed) (point-max)))
          (goto-char (+ (point) claimed))
          (should (or (eobp) (looking-at "From ")))))
      (should (> n 0)))))

(defmacro vm-mboxcl2-test-with-folders (spec &rest body)
  "Run BODY with two file names bound, in a directory of their own.
SPEC is (SOURCE-VAR TARGET-VAR)."
  (declare (indent 1) (debug t))
  (let ((source (nth 0 spec)) (target (nth 1 spec)))
    `(let ((dir (file-name-as-directory (make-temp-file "vm-mboxcl2" t))))
       (unwind-protect
           (let ((,source (expand-file-name "source" dir))
                 (,target (expand-file-name "target.mboxcl2" dir)))
             ,@body)
         (delete-directory dir t)))))

;;; Saving a message into one

(ert-deftest vm-mboxcl2-test-a-saved-message-arrives-with-a-length ()
  "`vm-save-message' into an mboxcl2 folder writes the length.
The message comes from a From_ folder, so this is the converting path: VM has
to compute a length the target folder did not have."
  (vm-mboxcl2-test-with-folders (source target)
    (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                          "From: alice@example.com\nSubject: from a From_ folder\n"
                          "\nA body of some length.\n\n")
                  nil source nil 'quiet)
    (vm-mboxcl2-test--write target '(("already here" . "First body.\n")))
    (vm-visit-folder source)
    (setq vm-message-pointer vm-message-list)
    (vm-save-message target 1)
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages)
                     '("already here" "from a From_ folder")))
      (should (equal (cdr (nth 1 messages)) "A body of some length.\n")))))

(ert-deftest vm-mboxcl2-test-a-message-saved-from-mboxcl2-keeps-its-length ()
  "Saving between two mboxcl2 folders takes the verbatim path, which copies
the message as it stands -- including the length it already has, which has to
be the right one for where it lands."
  (vm-mboxcl2-test-with-folders (source target)
    (setq source (concat source ".mboxcl2"))
    (vm-mboxcl2-test--write source '(("travelling" . "Body that travels.\n")))
    (vm-mboxcl2-test--write target '(("already here" . "First body.\n")))
    (vm-visit-folder source)
    (setq vm-message-pointer vm-message-list)
    (vm-save-message target 1)
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("already here" "travelling")))
      (should (equal (cdr (nth 1 messages)) "Body that travels.\n")))))

(ert-deftest vm-mboxcl2-test-a-message-with-a-From_-line-in-it-survives ()
  "A body with a line beginning \"From \" in it does not split the message.
mboxcl2 finds the end of a message by its length rather than by looking for
the next separator, so the line needs no quoting -- but it must not be
mangled on the way in either."
  (vm-mboxcl2-test-with-folders (source target)
    (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                          "From: alice@example.com\nSubject: quoting\n\n"
                          "Before.\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\n"
                          "After.\n\n")
                  nil source nil 'quiet)
    (vm-mboxcl2-test--write target '(("already here" . "First body.\n")))
    (vm-visit-folder source)
    (setq vm-message-pointer vm-message-list)
    (vm-save-message target 1)
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (= (length messages) 2))
      (should (string-match-p "Before\\." (cdr (nth 1 messages))))
      (should (string-match-p "After\\." (cdr (nth 1 messages)))))))

;;; Changing a message that is already in one

(ert-deftest vm-mboxcl2-test-editing-a-message-updates-its-length ()
  "Editing a message in an mboxcl2 folder writes a length for what it now is.
A body that grows or shrinks without its `Content-Length' following it makes
every message after it unreadable, since that is how the end of this one is
found."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("first" . "Short.\n")
                                     ("second" . "The one after.\n")))
    (vm-visit-folder target)
    (setq vm-message-pointer vm-message-list)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      (vm-edit-message)
      (goto-char (point-max))
      (insert "A good deal more text than the message had before.\n")
      (vm-edit-message-end))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("first" "second")))
      (should (string-match-p "A good deal more text" (cdr (nth 0 messages))))
      (should (equal (cdr (nth 1 messages)) "The one after.\n")))))

(ert-deftest vm-mboxcl2-test-an-expunge-leaves-the-rest-readable ()
  "Expunging a message leaves the others with their lengths intact."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("first" . "One.\n")
                                     ("second" . "Two.\n")
                                     ("third" . "Three.\n")))
    (vm-visit-folder target)
    (vm-set-deleted-flag (nth 1 vm-message-list) t)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      (vm-expunge-folder))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("first" "third")))
      (should (equal (cdr (nth 1 messages)) "Three.\n")))))

(ert-deftest vm-mboxcl2-test-moving-a-message-leaves-the-folder-readable ()
  "Moving a message's text within the folder keeps every length with its body.
`vm-physically-move-message' is what rewrites the folder when a sort is asked
to move messages rather than to reorder the summary, and it moves the text of
one message past another -- with the Content-Length header among the text it
moves, since that header is part of the message."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("first" . "One.\n")
                                     ("second" . "Two, which is longer.\n")
                                     ("third" . "Three.\n")))
    (vm-visit-folder target)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      ;; move the first message to where the third is
      (vm-physically-move-message (nth 0 vm-message-list) (nth 2 vm-message-list))
      (vm-mark-folder-modified-p (current-buffer)))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (= (length messages) 3))
      (should (equal (mapcar #'car messages) '("second" "first" "third")))
      (should (equal (cdr (nth 0 messages)) "Two, which is longer.\n"))
      (should (equal (cdr (nth 1 messages)) "One.\n"))
      (should (equal (cdr (nth 2 messages)) "Three.\n")))))

(ert-deftest vm-mboxcl2-test-a-sort-stores-the-order-without-rewriting ()
  "A sort reorders the summary and leaves the folder alone, as it does for any
folder type: the order goes into the folder's own header, not into the
arrangement of the messages.  Worth pinning while checking mboxcl2, since a
sort that did rewrite the folder would have to carry the lengths with it."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("charlie" . "Third alphabetically.\n")
                                     ("alpha" . "First alphabetically.\n")
                                     ("bravo" . "Second alphabetically.\n")))
    (vm-visit-folder target)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      (vm-sort-messages "subject")
      (should (equal (mapcar #'vm-su-subject vm-message-list)
                     '("alpha" "bravo" "charlie"))))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("charlie" "alpha" "bravo"))))))

;;; Changes that touch only the headers

(ert-deftest vm-mboxcl2-test-labels-do-not-disturb-the-lengths ()
  "Labelling a message writes headers, and a length counts only the body.
VM stuffs its own X-VM headers into a message whenever attributes change, and
the Thunderbird flags likewise; none of that is body, so none of it changes a
length -- which is worth pinning, because if a length ever did have to follow
a header change, every attribute change would be a folder rewrite."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("first" . "One.\n")
                                     ("second" . "Two.\n")))
    (vm-visit-folder target)
    (let ((m (car vm-message-list)))
      (vm-set-labels m (list "work"))
      (vm-set-deleted-flag m t)
      (vm-set-deleted-flag m nil))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("first" "second")))
      (should (equal (cdr (nth 0 messages)) "One.\n")))))

(ert-deftest vm-mboxcl2-test-thunderbird-flags-do-not-disturb-them-either ()
  "The same for the Thunderbird status headers VM keeps in step."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("first" . "One.\n")
                                     ("second" . "Two.\n")))
    (let ((vm-sync-thunderbird-status t))
      (vm-visit-folder target)
      (let ((m (car vm-message-list)))
        (vm-set-unread-flag m nil)
        (vm-set-flagged-flag m t))
      (let ((messages (vm-mboxcl2-test--read target)))
        (should (equal (mapcar #'car messages) '("first" "second")))
        (should (equal (cdr (nth 1 messages)) "Two.\n"))))))

;;; Mail arriving from a server

(ert-deftest vm-mboxcl2-test-a-retrieved-message-gets-a-length ()
  "A message arriving from POP or IMAP is given a length on the way in.
Both convert what the server sent -- a bare message, no separators -- into the
folder's own type with `vm-convert-folder-type-headers', which is where the
header comes from.  Checked here directly, since a test with a server is a
live test and this is arithmetic."
  (vm-mboxcl2-test-with-folders (_source target)
    (with-temp-buffer
      (insert "From: alice@example.com\nSubject: from a server\n\n"
              "The body as the server sent it.\n")
      (let ((vm-folder-type 'mboxcl2))
        (goto-char (point-min))
        (vm-convert-folder-type-headers 'baremessage 'mboxcl2)
        (should (string-match-p "^Content-Length: 32$" (buffer-string))))
      ;; and that is the body's length, not the whole message's
      (should (string-match-p "The body as the server sent it" (buffer-string))))))

;;; Messages that arrive by being made out of another one

(ert-deftest vm-mboxcl2-test-bursting-a-digest-gives-each-message-a-length ()
  "Bursting a digest into an mboxcl2 folder writes a length for every message
it makes.  The burst builds the messages in a work buffer and converts them to
the folder's type before appending, which is where the header comes from."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write
     target
     (list (cons "a digest"
                 (concat "This is a digest.\n\n"
                         "------------------------------\n"
                         "From: alice@example.com\nSubject: first enclosed\n\n"
                         "The first enclosed body.\n\n"
                         "------------------------------\n"
                         "From: bob@example.com\nSubject: second enclosed\n\n"
                         "The second enclosed body.\n\n"
                         "------------------------------\n"))))
    (vm-visit-folder target)
    (setq vm-message-pointer vm-message-list)
    (cl-letf (((symbol-function 'vm-display) #'ignore)
              ((symbol-function 'vm-present-current-message) #'ignore))
      (vm-burst-rfc934-digest))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages)
                     '("a digest" "first enclosed" "second enclosed")))
      (should (string-match-p "The first enclosed body"
                              (cdr (nth 1 messages))))
      (should (string-match-p "The second enclosed body"
                              (cdr (nth 2 messages)))))))

;;; Deleting an attachment, which is where a body shrinks

(defconst vm-mboxcl2-test--mime-headers
  (concat "MIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"sep\"\n")
  "The headers that make the message below a message with an attachment.")

(defconst vm-mboxcl2-test--with-attachment
  (concat "--sep\n"
          "Content-Type: text/plain\n\n"
          "Please find it attached.\n"
          "--sep\n"
          "Content-Type: application/octet-stream; name=\"thing.bin\"\n"
          "Content-Disposition: attachment; filename=\"thing.bin\"\n"
          "Content-Transfer-Encoding: base64\n\n"
          "VGhpcyBpcyBhIGZhaXJseSBsb25nIGF0dGFjaG1lbnQgd2hpY2ggd2lsbCBnbyBhd2F5Lgo=\n"
          "--sep--\n")
  "A body that is mostly an attachment.")

(ert-deftest vm-mboxcl2-test-deleting-an-attachment-updates-the-length ()
  "Deleting an attachment shrinks the body, and the length follows it.
This is the largest change VM makes to a message in place, and the folder is
unreadable by anything that believes the header if the length stays as it was."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write
     target (list (cons "with an attachment"
                       (cons vm-mboxcl2-test--mime-headers
                             vm-mboxcl2-test--with-attachment))
                  (cons "the one after" "Still here.\n")))
    (vm-visit-folder target)
    (setq vm-message-pointer vm-message-list)
    (cl-letf (((symbol-function 'vm-display) #'ignore)
              ((symbol-function 'vm-present-current-message) #'ignore)
              ((symbol-function 'vm-discard-cached-data) #'ignore))
      (let ((vm-mime-confirm-delete nil))
        (vm-delete-all-attachments 1)))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (= (length messages) 2))
      (should (equal (cdr (nth 1 messages)) "Still here.\n"))
      ;; the attachment's data is gone and the note about it is there
      (should-not (string-match-p "VGhpcyBpcyBh" (cdr (nth 0 messages))))
      (should (string-match-p "Please find it attached" (cdr (nth 0 messages)))))))

(ert-deftest vm-mboxcl2-test-mail-gobbled-from-a-crash-box-gets-lengths ()
  "New mail arriving through a crash box is converted on the way in.
That is the last step of every arrival -- movemail, POP and IMAP all leave
their mail in a crash box and `vm-gobble-crash-box' appends it -- and a From_
crash box going into an mboxcl2 folder has to gain a length per message."
  (vm-mboxcl2-test-with-folders (crash target)
    (vm-mboxcl2-test--write target '(("already here" . "First body.\n")))
    (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                          "From: alice@example.com\nSubject: newly arrived\n"
                          "\nFresh mail.\n\n"
                          "From bob@example.com Sun Aug  9 09:00:00 2026\n"
                          "From: bob@example.com\nSubject: and another\n"
                          "\nMore of it.\n\n")
                  nil crash nil 'quiet)
    (vm-visit-folder target)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      (should (vm-gobble-crash-box crash)))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages)
                     '("already here" "newly arrived" "and another")))
      (should (equal (cdr (nth 1 messages)) "Fresh mail.\n"))
      (should (equal (cdr (nth 2 messages)) "More of it.\n")))))

(ert-deftest vm-mboxcl2-test-a-crash-box-of-the-same-type-is-appended-as-it-is ()
  "A crash box that is already mboxcl2 keeps the lengths it came with."
  (vm-mboxcl2-test-with-folders (crash target)
    (setq crash (concat crash ".mboxcl2"))
    (vm-mboxcl2-test--write target '(("already here" . "First body.\n")))
    (vm-mboxcl2-test--write crash '(("newly arrived" . "Fresh mail.\n")))
    (vm-visit-folder target)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      (should (vm-gobble-crash-box crash)))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("already here" "newly arrived")))
      (should (equal (cdr (nth 1 messages)) "Fresh mail.\n")))))

;;; Converting a folder to the type it already is (emacs-vm/vm#623)

(ert-deftest vm-mboxcl2-test-the-current-type-is-offered-for-conversion ()
  "The type a folder already has is among the ones offered.
`vm-change-folder-type' used to remove it -- \"you cannot change to what you
are\" -- so in an mboxcl2 folder the completions were BellFrom_, From_, babyl
and mmdf, and the one conversion that repairs such a folder was the one it
would not offer (emacs-vm/vm#623).

The interactive spec is evaluated directly, since what is being checked is the
list it puts to the user."
  (vm-mboxcl2-test-with-folders (_source target)
    (vm-mboxcl2-test--write target '(("one" . "Body.\n")))
    (vm-visit-folder target)
    (should (eq vm-folder-type 'mboxcl2))
    (let ((offered nil))
      (cl-letf (((symbol-function 'vm-read-string)
                 (lambda (_prompt types &optional _multi)
                   (setq offered types)
                   "mboxcl2"))
                ((symbol-function 'vm-read-file-name)
                 (lambda (&rest _) (error "asked for a file with no prefix arg"))))
        (let ((current-prefix-arg nil))
          ;; type, file, output: neither file nor output without a prefix
          ;; argument
          (should (equal (eval (cadr (interactive-form 'vm-change-folder-type)) t)
                         '(mboxcl2 nil nil)))))
      (should (member "mboxcl2" offered))
      ;; and it offers the others as it always did
      (should (member "From_" offered))
      (should (member "babyl" offered)))))

(ert-deftest vm-mboxcl2-test-converting-to-the-same-type-fixes-stale-lengths ()
  "A folder whose lengths are wrong is repaired by converting it to mboxcl2.
A wrong length is not a missing one: the folder opens, because the reader falls
back on searching for the next separator, so nothing complains and the folder
is wrong for anything that believes the header.  Converting it to the type it
already has rewrites every message and recomputes every length."
  (vm-mboxcl2-test-with-folders (_source target)
    ;; written by hand, with lengths that are all wrong
    (with-temp-buffer
      (dolist (message '(("one" . "Body one is this long.\n")
                         ("two" . "Body two.\n")))
        (insert "From sender@example.com Sat Aug  8 14:24:13 2026\n"
                "From: sender@example.com\nSubject: " (car message) "\n"
                "Content-Length: 3\n\n" (cdr message)))
      (write-region (point-min) (point-max) target nil 'quiet))
    (should (eq (vm-get-folder-type target) 'mboxcl2))
    (vm-visit-folder target)
    (cl-letf (((symbol-function 'vm-display) #'ignore))
      (vm-change-folder-type 'mboxcl2))
    (let ((messages (vm-mboxcl2-test--read target)))
      (should (equal (mapcar #'car messages) '("one" "two")))
      (should (equal (cdr (nth 0 messages)) "Body one is this long.\n"))
      (should (equal (cdr (nth 1 messages)) "Body two.\n")))))

(provide 'vm-mboxcl2-test)

;;; vm-mboxcl2-test.el ends here
