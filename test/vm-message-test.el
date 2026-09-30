;;; vm-message-test.el --- Tests for vm-message.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM message struct accessors in vm-message.el

;;; Code:

(require 'vm-test-init)
(require 'vm-message)

;;; Constants tests

(ert-deftest vm-message-test-location-data-vector-length ()
  "Test location data vector length constant."
  (should (= vm-location-data-vector-length 6)))

(ert-deftest vm-message-test-softdata-vector-length ()
  "Test softdata vector length constant."
  (should (= vm-softdata-vector-length 22))
  (should (= (length vm-softdata-fields) vm-softdata-vector-length)))

(ert-deftest vm-message-test-every-softdata-accessor-reads-its-own-field ()
  "Each `vm-FIELD-of' reads the slot `vm-softdata-fields' names for it.
The vector is positional and every accessor carries its index as a literal,
so a slot added or taken out in one place and not the other is silent.  The
padded-number slot came out in emacs-vm/vm#861 and every index above it moved
down; this is what says they all moved together.

`:unused' has no accessor, and the two `-sym' fields hold a symbol whose
value is the message, so they are read by name here like any other slot."
  (let ((m (vm-make-message))
        (checked 0))
    (dotimes (i vm-softdata-vector-length)
      (let* ((field (symbol-name (aref vm-softdata-fields i)))
             (getter (intern-soft (concat "vm-" (substring field 1) "-of")))
             (token (intern (format "slot-%d" i))))
        (when (fboundp getter)
          (aset (vm-softdata-of m) i token)
          (should (eq token (funcall getter m)))
          (setq checked (1+ checked)))))
    ;; every field but `:unused\='
    (should (= checked (1- vm-softdata-vector-length)))))

(ert-deftest vm-message-test-attributes-vector-length ()
  "Test attributes vector length constant."
  (should (= vm-attributes-vector-length 20)))

(ert-deftest vm-message-test-cached-data-vector-length ()
  "Test cached data vector length constant."
  (should (= vm-cached-data-vector-length 50)))

(ert-deftest vm-message-test-mirror-data-vector-length ()
  "Test mirror data vector length constant."
  (should (= vm-mirror-data-vector-length 6)))

;;; vm-make-message tests

(ert-deftest vm-message-test-make-message-creates-vector ()
  "Test that vm-make-message creates a valid message struct."
  (let ((m (vm-make-message)))
    (should (vectorp m))
    (should (= (length m) 5))))

(ert-deftest vm-message-test-make-message-subvectors ()
  "Test that vm-make-message creates proper sub-vectors."
  (let ((m (vm-make-message)))
    ;; Check location data
    (should (vectorp (vm-location-data-of m)))
    (should (= (length (vm-location-data-of m)) vm-location-data-vector-length))
    ;; Check softdata
    (should (vectorp (vm-softdata-of m)))
    (should (= (length (vm-softdata-of m)) vm-softdata-vector-length))
    ;; Check mirror data
    (should (vectorp (vm-mirror-data-of m)))
    (should (= (length (vm-mirror-data-of m)) vm-mirror-data-vector-length))))

;;; Location data accessor tests

(ert-deftest vm-message-test-set-start-of ()
  "Test setting start marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-min-marker)))
        (vm-set-start-of m marker)
        (should (eq (vm-start-of m) marker))))))

(ert-deftest vm-message-test-set-headers-of ()
  "Test setting headers marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-marker)))
        (vm-set-headers-of m marker)
        (should (eq (vm-headers-of m) marker))))))

(ert-deftest vm-message-test-set-text-of ()
  "Test setting text marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-max-marker)))
        (vm-set-text-of m marker)
        (should (eq (vm-text-of m) marker))))))

(ert-deftest vm-message-test-set-end-of ()
  "Test setting end marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-max-marker)))
        (vm-set-end-of m marker)
        (should (eq (vm-end-of m) marker))))))

;;; Softdata accessor tests

(ert-deftest vm-message-test-set-number-of ()
  "Test setting message number."
  (let ((m (vm-make-message)))
    (vm-set-number-of m "42")
    (should (string= (vm-number-of m) "42"))))

(ert-deftest vm-message-test-set-mark-of ()
  "Test setting message mark."
  (let ((m (vm-make-message)))
    (vm-set-mark-of m t)
    (should (eq (vm-mark-of m) t))
    (vm-set-mark-of m nil)
    (should (null (vm-mark-of m)))))

(ert-deftest vm-message-test-set-buffer-of ()
  "Test setting message buffer."
  (let ((m (vm-make-message))
        (buf (current-buffer)))
    (vm-set-buffer-of m buf)
    (should (eq (vm-buffer-of m) buf))))

(ert-deftest vm-message-test-set-message-type-of ()
  "Test setting message type."
  (let ((m (vm-make-message)))
    (vm-set-message-type-of m 'From_)
    (should (eq (vm-message-type-of m) 'From_))))

(ert-deftest vm-message-test-set-thread-indentation ()
  "Test setting thread indentation."
  (let ((m (vm-make-message)))
    (vm-set-thread-indentation-of m 3)
    (should (= (vm-thread-indentation-of m) 3))))

(ert-deftest vm-message-test-set-thread-list ()
  "Test setting thread list."
  (let ((m (vm-make-message)))
    (vm-set-thread-list-of m '(a b c))
    (should (equal (vm-thread-list-of m) '(a b c)))))

;;; Mirror data accessor tests

(ert-deftest vm-message-test-set-edit-buffer ()
  "Test setting edit buffer."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (vm-set-edit-buffer-of m buf)
        (should (eq (vm-edit-buffer-of m) buf))))))

(ert-deftest vm-message-test-set-virtual-messages ()
  "Test setting virtual messages list."
  (let ((m (vm-make-message)))
    (vm-set-virtual-messages-of m '(vm1 vm2))
    (should (equal (vm-virtual-messages-of m) '(vm1 vm2)))))

(ert-deftest vm-message-test-set-stuff-flag ()
  "Test setting stuff flag."
  (let ((m (vm-make-message)))
    (vm-set-stuff-flag-of m t)
    (should (eq (vm-stuff-flag-of m) t))))

;;; Real/mirrored message accessor tests

(ert-deftest vm-message-test-set-real-message-sym ()
  "Test setting real message symbol."
  (let ((m (vm-make-message))
        (sym (make-symbol "test-message")))
    (vm-set-real-message-sym-of m sym)
    (should (eq (vm-real-message-sym-of m) sym))))

(ert-deftest vm-message-test-real-message-of ()
  "Test vm-real-message-of returns the message from symbol."
  (let ((m (vm-make-message)))
    ;; Set up real message symbol with message as its value
    (let ((sym (make-symbol "real-msg")))
      (set sym m)
      (vm-set-real-message-sym-of m sym)
      (should (eq (vm-real-message-of m) m)))))

;;; Attribute accessor tests

(ert-deftest vm-message-test-attributes-vector ()
  "Test that attributes vector can be set and retrieved."
  (let ((m (vm-make-message))
        (attrs (make-vector vm-attributes-vector-length nil)))
    (vm-set-attributes-of m attrs)
    (should (eq (vm-attributes-of m) attrs))))

;;; Cached data accessor tests

(ert-deftest vm-message-test-cached-data-vector ()
  "Test that cached data vector can be set and retrieved."
  (let ((m (vm-make-message))
        (cache (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of m cache)
    (should (eq (vm-cached-data-of m) cache))))

;;; Field constants tests

(ert-deftest vm-message-test-message-fields ()
  "Test that message fields constant is valid."
  (should (vectorp vm-message-fields))
  (should (= (length vm-message-fields) 5)))

(ert-deftest vm-message-test-location-data-fields ()
  "Test that location data fields constant is valid."
  (should (vectorp vm-location-data-fields))
  (should (= (length vm-location-data-fields) vm-location-data-vector-length)))

(ert-deftest vm-message-test-softdata-fields ()
  "Test that softdata fields constant is valid."
  (should (vectorp vm-softdata-fields))
  (should (= (length vm-softdata-fields) vm-softdata-vector-length)))

(ert-deftest vm-message-test-mirror-data-fields ()
  "Test that mirror data fields constant is valid."
  (should (vectorp vm-mirror-data-fields))
  (should (= (length vm-mirror-data-fields) vm-mirror-data-vector-length)))

;;; Reverse links (issue #453)

;; The links live in `vm-reverse-link-table', not in the message vector, so that
;; the collector does not recurse once per message down the whole folder.  What
;; these pin is that moving them out kept the semantics and did not introduce a
;; leak: `vm-expunge-message' decides which cons to splice from the link, so a
;; message with the wrong link loses a different message than the one asked for.

(ert-deftest vm-message-test-reverse-link-of-a-fresh-message-is-nil ()
  "A message not yet in any list has no reverse link, and says so rather than
signalling.  `vm-make-message' used to leave the link symbol unbound, so this
raised `void-variable'."
  (let ((m (vm-make-message)))
    (should (null (vm-reverse-link-of m)))))

(ert-deftest vm-message-test-reverse-links-are-per-message ()
  "Messages sharing a soft data vector still have independent reverse links.
`vm-make-presentation-copy' copies a message and its soft data shallowly.  When
the link was a symbol held in that vector the copy shared the original's link:
reading the copy gave the folder's link and setting it overwrote the folder's."
  (let* ((m (vm-make-message))
         (copy (copy-sequence m))
         (link-for-m (list (vm-make-message)))
         (link-for-copy (list (vm-make-message))))
    (vm-set-softdata-of copy (copy-sequence (vm-softdata-of m)))
    ;; Distinct vectors, as the presentation copy has: what used to be shared
    ;; was not the vector but the link symbol sitting in slot 6 of both.
    (should-not (eq (vm-softdata-of copy) (vm-softdata-of m)))
    (vm-set-reverse-link-of m link-for-m)
    ;; The copy has none of its own, and reading it does not invent one.
    (should (eq link-for-m (vm-reverse-link-of m)))
    (should (null (vm-reverse-link-of copy)))
    ;; Setting the copy's leaves the original's alone, and the reverse.
    (vm-set-reverse-link-of copy link-for-copy)
    (should (eq link-for-m (vm-reverse-link-of m)))
    (should (eq link-for-copy (vm-reverse-link-of copy)))))

(ert-deftest vm-message-test-reverse-link-can-be-cleared ()
  "Setting a reverse link to nil clears it, as it must for the list head."
  (let ((m (vm-make-message)))
    (vm-set-reverse-link-of m (list (vm-make-message)))
    (should (vm-reverse-link-of m))
    (vm-set-reverse-link-of m nil)
    (should (null (vm-reverse-link-of m)))))

(ert-deftest vm-message-test-reverse-link-table-is-weak-on-its-keys ()
  "The table holds messages weakly, and compares them by identity.
This asserts how the table is made rather than watching a collection do it,
because a collection costs what the session's whole heap costs -- tens of
seconds once the suite has run a while -- and what a change would alter is the
declaration.  Both halves matter.  Without weak keys the table would hold every
message of every folder ever visited for the life of the session, each kept
alive by its own link.  Weak on values instead would be worse than a leak: an
entry could go while its message was still in a folder, and
`vm-expunge-message' reads a missing link as \"this is the list head\" and
splices the head out in its place.  `eq' rather than `equal' because two
distinct messages can have identical contents, and because `equal' on a
message would recurse through the folder.  Issue #453."
  (should (eq 'key (hash-table-weakness vm-reverse-link-table)))
  (should (eq 'eq (hash-table-test vm-reverse-link-table))))

;;; Re-encoding the cached summary data (emacs-vm/vm#671)
;;
;; `vm-mime-encode-words-in-cache-vector' is what turns the decoded cache
;; back into something that can be written into the folder as X-VM-v5-Data.
;; It copies fifty slots one at a time, and had fifteen surviving mutations:
;; deleting any one of those copies loses a field silently, and the folder
;; then carries a summary line with a hole in it.

(defconst vm-message-test--decoded-slots
  '(7 8 11 13 14 17 28 37 38 39 40)
  "The slots holding decoded text as a string.
Everything else is a number, an ASCII field, or a flag -- except slot 18,
the tokenized summary, which is a list and is handled on its own.")

(defconst vm-message-test--tokenized-slot 18
  "The slot holding the tokenized summary: a list of strings and symbols.")

(defun vm-message-test--decoded (string)
  "STRING as MIME decoding leaves it: marked with the charset it came from."
  (propertize string 'vm-string t 'vm-charset "utf-8" 'vm-coding 'utf-8))

(defun vm-message-test--cache-vector ()
  "A cache vector whose every slot says which slot it is."
  (let ((vector (make-vector vm-cached-data-vector-length nil)))
    (dotimes (i vm-cached-data-vector-length)
      (aset vector i (cond ((memq i vm-message-test--decoded-slots)
                            (vm-message-test--decoded (format "René %d" i)))
                           ((= i vm-message-test--tokenized-slot)
                            (list (vm-message-test--decoded "René 18")))
                           (t (format "slot %d" i)))))
    vector))

(ert-deftest vm-message-test-the-cache-keeps-every-slot ()
  "Every slot of the cache vector comes through.

Losing one loses a field of the summary data written into the folder, and
nothing else would notice: the folder is still well formed, and the missing
field only shows as a summary line that has stopped saying something."
  (let* ((vm-display-using-mime t)
         (before (vm-message-test--cache-vector))
         (after (vm-mime-encode-words-in-cache-vector before)))
    (should (= (length after) vm-cached-data-vector-length))
    (dotimes (i vm-cached-data-vector-length)
      (should (aref after i))
      ;; the encoded slots are base64, so the marker is read back rather
      ;; than looked for: a round trip says the slot survived and says what
      ;; was in it
      (let* ((held (aref after i))
             (text (if (listp held) (car held) held)))
        (should (string-match-p (format "%d\\'" i)
                                (vm-decode-mime-encoded-words-in-string text)))))))

(ert-deftest vm-message-test-the-decoded-slots-are-encoded-again ()
  "The decoded text goes back to MIME encoded words, because the folder is
a file of ASCII: what is written is what another reader has to decode."
  (let* ((vm-display-using-mime t)
         (after (vm-mime-encode-words-in-cache-vector
                 (vm-message-test--cache-vector))))
    (dolist (slot vm-message-test--decoded-slots)
      (should (string-match-p "=?utf-8?" (aref after slot)))
      (should-not (string-match-p "é" (aref after slot))))
    ;; the tokenized summary is a list, and its tokens are encoded too
    (let ((tokens (aref after vm-message-test--tokenized-slot)))
      (should (listp tokens))
      (should (string-match-p "=?utf-8?" (car tokens))))))

(ert-deftest vm-message-test-the-other-slots-are-left-alone ()
  "A slot that was never decoded is copied as it stands.

Not merely equal: encoding a number or a flag would be a way of quietly
changing what the folder says."
  (let* ((vm-display-using-mime t)
         (before (vm-message-test--cache-vector))
         (after (vm-mime-encode-words-in-cache-vector before)))
    (dotimes (i vm-cached-data-vector-length)
      (unless (or (memq i vm-message-test--decoded-slots)
                  (= i vm-message-test--tokenized-slot))
        (should (equal (aref after i) (aref before i)))))))

(ert-deftest vm-message-test-the-cache-vector-is-a-copy ()
  "The vector handed back is a new one: the message keeps its decoded cache
for the summary, and the encoded copy is only for writing out."
  (let* ((vm-display-using-mime t)
         (before (vm-message-test--cache-vector))
         (after (vm-mime-encode-words-in-cache-vector before)))
    (should-not (eq after before))
    (dolist (slot vm-message-test--decoded-slots)
      ;; the original still holds the decoded text
      (should (string-match-p "é" (aref before slot))))))

(provide 'vm-message-test)

;;; vm-message-test.el ends here