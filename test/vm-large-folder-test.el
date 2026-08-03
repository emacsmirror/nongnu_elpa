;;; vm-large-folder-test.el --- VM on a folder of many messages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Two issues are about size rather than about any particular message.
;;
;; #373: "Recreating summary..." on a 25000-message folder ran for over 25
;; minutes.  Reported in 2013 with no measurement since.
;;
;; #453: Emacs crashed in the garbage collector, and Pip Cet diagnosed it as
;; stack overflow in the recursive mark_object walking VM's message list --
;; which is a chain of vectors linked through *symbols* (`vm-make-message'
;; interposes an uninterned symbol per message, so that the structure is not
;; purely self-referential and the Lisp debugger can print it).  Thousands of
;; messages means thousands of levels of symbol/cons/vector to mark.
;;
;; The fixture is generated here rather than checked in: a folder of this size
;; is megabytes, and its shape -- how many messages, whether they thread -- is
;; the thing under test, so it belongs in code.
;;
;; Two size knobs keep the suite quick, and either can be raised from the
;; environment to measure at a reported size:
;;
;;     cd test && VM_LARGE_FOLDER=25000 make test-one testel=vm-large-folder-test.el
;;
;; They are separate because the costs are not alike: reading and summarizing
;; are near enough linear in the number of messages, while building threads goes
;; up with about the cube of a thread's depth.  See `vm-large-folder-test-size'
;; and `vm-large-folder-test-chain-size'.
;;
;; The timings are reported through `message', not asserted: what a machine
;; takes is not a property of VM, and a test that fails on a slow day teaches
;; nobody anything.  What is asserted is that the work completes and the results
;; are the right size.

;;; Code:

(require 'cl-lib)
(require 'vm-test-init)
(require 'vm-folder)

(defvar vm-large-folder-test-size
  (string-to-number (or (getenv "VM_LARGE_FOLDER") "2000"))
  "How many messages the generated folder holds.
Set VM_LARGE_FOLDER to measure at a larger size.")

(defvar vm-large-folder-test-chain-size
  (string-to-number (or (getenv "VM_LARGE_FOLDER_CHAIN") "300"))
  "How many messages the single-chain folder holds.
Much smaller than `vm-large-folder-test-size', and separately settable, because
thread building costs about the cube of the chain length: 500 messages in one
chain take under a second, 1000 take seven, 2000 take a minute.  That is the
finding recorded on #373, and it is also why this number cannot follow the
other one.")

(defun vm-large-folder-test--write (file n &optional threaded)
  "Write a folder of N messages to FILE.
THREADED nil leaves the messages unrelated.  THREADED t makes the folder one
chain of N, each message referencing the one before it -- the deepest shape
thread building can be given.  THREADED a number makes threads of that many
messages, which is what a mailing list archive looks like."
  (with-temp-file file
    (dotimes (i n)
      (insert (format "From sender%d@example.com Mon Jan  1 00:00:00 2024\n" i)
              (format "From: Sender %d <sender%d@example.com>\n" i i)
              "To: me@example.com\n"
              (format "Subject: message number %d\n" i)
              "Date: Mon, 01 Jan 2024 00:00:00 +0000\n"
              (format "Message-ID: <big-%d@example.com>\n" i)
              (cond ((and (numberp threaded)
                          (/= 0 (mod i threaded)))
                     (format "References: <big-%d@example.com>\n" (1- i)))
                    ((and (eq threaded t) (> i 0))
                     (format "References: <big-%d@example.com>\n" (1- i)))
                    (t ""))
              "\n"
              (format "Body of message %d.\n\n" i)))))

(defmacro vm-large-folder-test--with-folder (spec &rest body)
  "Generate a folder and visit it, then clean up everything.
SPEC is (FILE-VAR N &optional THREADED)."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-large" t)))
          (,(car spec) (expand-file-name "large-folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (before (buffer-list)))
     (require 'vm)
     (unwind-protect
         (progn
           (vm-large-folder-test--write ,(car spec) ,(cadr spec) ,(nth 2 spec))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory dir t))))

(defmacro vm-large-folder-test--timed (what &rest body)
  "Run BODY, report how long it took as WHAT, and return its value."
  (declare (indent 1) (debug t))
  `(let ((start (float-time))
         (value (progn ,@body)))
     (message "  %s: %.2f s" ,what (- (float-time) start))
     value))

(ert-deftest vm-large-folder-test-visit-and-summarize ()
  "A folder of many messages is read and summarized, and the summary is right.
Issue #373.  The interesting number is in the message log; what is asserted is
that every message is found and gets exactly one summary line."
  (let ((n vm-large-folder-test-size))
    (vm-large-folder-test--with-folder (file n)
      (vm-large-folder-test--timed "visit (parse and summarize)"
        (vm-visit-folder file))
      (should (= n (length vm-message-list)))
      (vm-large-folder-test--timed "vm-do-summary on its own"
        (vm-do-summary))
      ;; One line per message, and the last one really is the last message.
      (with-current-buffer vm-summary-buffer
        (should (= n (count-lines (point-min) (point-max))))
        (goto-char (point-max))
        (forward-line -1)
        (should (string-match-p (format "message number %d" (1- n))
                                (buffer-substring (point) (point-max))))))))

(ert-deftest vm-large-folder-test-threading-a-single-chain ()
  "Threading a folder that is one long reference chain completes and is right.
Issue #373 again: with `vm-summary-show-threads' on, \"Recreating summary\" also
builds the thread database, and a chain of N messages is the deepest shape it can
be asked for.  Deliberately a small N -- see
`vm-large-folder-test-chain-size' for why."
  (let ((n vm-large-folder-test-chain-size))
    (vm-large-folder-test--with-folder (file n t)
      (vm-visit-folder file)
      (should (= n (length vm-message-list)))
      (vm-large-folder-test--timed "vm-build-threads over one chain"
        (vm-build-threads vm-message-list))
      ;; The first message roots the thread and the last one is n-1 deep.
      (should (= 0 (vm-thread-indentation (car vm-message-list))))
      (should (< 0 (vm-thread-indentation (car (last vm-message-list))))))))

(ert-deftest vm-large-folder-test-gc-walks-the-message-list ()
  "REGRESSION: garbage collection survives a long chain of messages.
Issue #453.  VM's message list is a chain of vectors linked through uninterned
symbols, and Emacs used to mark that recursively -- thousands of messages meant
thousands of stack frames of symbol/cons/vector and, for Pieter van Oostrum, a
crash (debbugs #39962).

A crash here takes the whole batch run with it, which is the point: if this
returns at all, the marking held."
  (let ((n vm-large-folder-test-size))
    ;; Shallow threads: what the collector walks here is the message list, one
    ;; link per message, so length is what matters and thread depth is beside
    ;; the point -- and a deep chain would make this test cost minutes.
    (vm-large-folder-test--with-folder (file n 10)
      (vm-visit-folder file)
      (should (= n (length vm-message-list)))
      ;; Thread it too, so the symbol obarray is populated and the graph is the
      ;; one a real session has.
      (vm-build-threads vm-message-list)
      (vm-large-folder-test--timed "3 x garbage-collect"
        (dotimes (_ 3) (garbage-collect)))
      ;; Still intact afterwards.
      (should (= n (length vm-message-list)))
      (should (vm-su-subject (car (last vm-message-list)))))))

(provide 'vm-large-folder-test)

;;; vm-large-folder-test.el ends here
