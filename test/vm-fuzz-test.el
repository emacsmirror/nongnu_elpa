;;; vm-fuzz-test.el --- random operation sequences over a folder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The other tests check the sequences someone thought of.  These drive a folder
;; through random sequences of the commands that rearrange it and check, after
;; every single operation, things that have to hold whatever the sequence was.
;;
;; Two invariants earn their keep, both of them drawn from bugs already found:
;;
;; - Each message's text is where the message says it is.  `vm-expunge-message'
;;   takes the cons to splice from a reverse link while `vm-expunge-folder'
;;   deletes the text separately, so a wrong link makes the list and the buffer
;;   disagree without either looking wrong on its own (#453, #570).
;; - Every mirror registered on a real message is in the message list of its own
;;   folder.  A registration that outlives the list is what #569 and #571 were,
;;   and it is invisible until some later command walks the mirrors.
;;
;; Reverted the fix for #569, the multi-folder test below fails on its first
;; omit, in every seed tried.  A random test that cannot fail is worth nothing,
;; so that is the calibration.
;;
;; The sequences are seeded, so a failure is reproducible: `random' with a string
;; seeds deterministically.  The counts are deliberately small, a second or so
;; for the file; raise them from the environment to search harder:
;;
;;     cd test && VM_FUZZ_SEEDS=200 VM_FUZZ_OPS=60 make test-one testel=vm-fuzz-test.el

;;; Code:

(require 'cl-lib)
(require 'vm-test-init)
(require 'vm-folder)

(defvar vm-fuzz-test-seeds
  (string-to-number (or (getenv "VM_FUZZ_SEEDS") "6"))
  "How many random sequences each fuzz test runs.")

(defvar vm-fuzz-test-ops
  (string-to-number (or (getenv "VM_FUZZ_OPS") "20"))
  "How many operations each random sequence performs.")

(defvar vm-fuzz-test-size 6
  "How many messages the generated folder holds.")

;;; Invariants

(defun vm-fuzz-test--conses (list)
  "Return every cons of LIST, so membership of one can be asserted."
  (let ((all nil))
    (while list (push list all) (setq list (cdr list)))
    all))

(defun vm-fuzz-test--body-number (m)
  "Return the number of the body the folder text of M holds, or nil.
A virtual message's location markers belong to the real message's buffer, so
this asks the real message where its text is."
  (let ((real (vm-real-message-of m)))
    (with-current-buffer (vm-buffer-of real)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (vm-text-of real))
          (when (re-search-forward "^Body \\([0-9]+\\)\\."
                                   (vm-text-end-of real) t)
            (string-to-number (match-string 1))))))))

(defun vm-fuzz-test--subject-number (m)
  "Return the number in M's subject."
  (string-to-number
   (replace-regexp-in-string "[^0-9]" "" (or (vm-su-subject m) ""))))

(defun vm-fuzz-test--check-folder (buffer)
  "Return a list of what is wrong in folder BUFFER, empty if nothing is."
  (with-current-buffer buffer
    (let ((problems nil)
          (mp vm-message-list)
          (prev nil)
          (seen nil))
      (while mp
        (unless (eq (vm-reverse-link-of (car mp)) prev)
          (push (format "%s: reverse link of \"%s\" is not the cons before it"
                        (buffer-name) (vm-su-subject (car mp)))
                problems))
        (when (memq (car mp) seen)
          (push (format "%s: a message appears twice in the list"
                        (buffer-name))
                problems))
        (push (car mp) seen)
        (setq prev mp mp (cdr mp)))
      (dolist (m vm-message-list)
        (let ((want (vm-fuzz-test--subject-number m))
              (got (vm-fuzz-test--body-number m)))
          (unless (equal want got)
            (push (format "%s: \"%s\" points at body %S"
                          (buffer-name) (vm-su-subject m) got)
                  problems))))
      (when (and vm-message-pointer
                 (not (memq vm-message-pointer
                            (vm-fuzz-test--conses vm-message-list))))
        (push (format "%s: the message pointer is not a cons of the list"
                      (buffer-name))
              problems))
      problems)))

(defun vm-fuzz-test--check-mirrors (real)
  "Return what is wrong with the mirrors registered in folder REAL."
  (let ((problems nil))
    (with-current-buffer real
      (dolist (m vm-message-list)
        (dolist (mirror (vm-virtual-messages-of m))
          (let ((buffer (vm-buffer-of mirror)))
            (cond
             ((not (buffer-live-p buffer))
              (push (format "a mirror of \"%s\" is in a killed buffer"
                            (vm-su-subject m))
                    problems))
             ((not (with-current-buffer buffer
                     (memq mirror vm-message-list)))
              (push (format "a mirror of \"%s\" in %s is not in that folder"
                            (vm-su-subject m) (buffer-name buffer))
                    problems)))))))
    problems))

;;; Operations

(defun vm-fuzz-test--operate (buffer)
  "Perform one random operation in folder BUFFER, and describe it."
  (with-current-buffer buffer
    (let* ((n (length vm-message-list))
           (virtual (eq major-mode 'vm-virtual-mode))
           (k (if (> n 0) (random n) 0))
           (choice (if (= n 0) 99 (random (if virtual 7 6)))))
      (when (> n 0)
        (setq vm-message-pointer (nthcdr k vm-message-list)))
      (cond
       ((= choice 0) (vm-delete-message 1) (list 'delete (buffer-name) k))
       ((= choice 1) (vm-undelete-message 1) (list 'undelete (buffer-name) k))
       ((= choice 2) (vm-expunge-folder) (list 'expunge (buffer-name)))
       ((= choice 3) (vm-sort-messages "subject") (list 'sort (buffer-name)))
       ((= choice 4)
        ;; Moving off either end signals, and that is not a failure.
        (condition-case nil
            (progn (vm-move-message-forward 1) (list 'move (buffer-name) k))
          (error (list 'move-refused (buffer-name) k))))
       ((= choice 5) (vm-toggle-flag-message 1) (list 'flag (buffer-name) k))
       ((= choice 6)
        (vm-virtual-omit-message 1 (list (nth k vm-message-list)))
        (list 'omit (buffer-name) k))
       (t (list 'folder-empty (buffer-name)))))))

(defmacro vm-fuzz-test--with-folders (spec &rest body)
  "Visit a generated folder, and virtual folders over it, then run BODY.
SPEC is (REAL-VAR VIRTUALS-VAR HOW-MANY-VIRTUALS).  BODY is run with the real
folder current."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-fuzz" t)))
          (file (expand-file-name "folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-summary-show-threads nil)
          (vm-circular-folders nil)
          (vm-move-after-deleting nil)
          (vm-move-after-undeleting nil)
          (vm-virtual-folder-alist nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-user-interaction-buffer vm-user-interaction-buffer)
          (before (buffer-list))
          ,(car spec) (,(nth 1 spec) nil))
     (require 'vm)
     (unwind-protect
         (progn
           (vm-test-write-simple-folder file vm-fuzz-test-size)
           (setq vm-virtual-folder-alist
                 (let ((alist nil))
                   (dotimes (i ,(nth 2 spec))
                     (push (list (format "fuzz-%d" i)
                                 (list (list file) '(any)))
                           alist))
                   alist))
           (vm-visit-folder file)
           (setq ,(car spec) (current-buffer))
           (dotimes (i ,(nth 2 spec))
             (vm-visit-virtual-folder (format "fuzz-%d" i))
             (push (current-buffer) ,(nth 1 spec)))
           (with-current-buffer ,(car spec) ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-fuzz-test--run (seed real virtuals)
  "Run one seeded sequence over REAL and VIRTUALS.  Return what broke, if any."
  (random (format "vm-fuzz-%d" seed))
  (let ((history nil)
        (problems nil))
    (catch 'broken
      (dotimes (_ vm-fuzz-test-ops)
        (let ((buffers (cl-remove-if-not #'buffer-live-p
                                         (cons real virtuals))))
          (push (vm-fuzz-test--operate
                 (nth (random (length buffers)) buffers))
                history))
        (setq problems
              (append (vm-fuzz-test--check-mirrors real)
                      (apply #'append
                             (mapcar #'vm-fuzz-test--check-folder
                                     (cl-remove-if-not #'buffer-live-p
                                                       (cons real virtuals))))))
        (when problems (throw 'broken nil))))
    (when problems
      (format "seed %d, after %S:\n  %s"
              seed (reverse history)
              (mapconcat #'identity (delete-dups problems) "\n  ")))))

;;; The tests

;; Each seed gets a folder of its own.  Sharing one would let the first sequence
;; empty it and leave the rest with nothing to do, which reads as a fast pass.

(ert-deftest vm-fuzz-test-one-folder-survives-random-operations ()
  "Random deletes, expunges, sorts, moves and flags leave one folder consistent.
Checked after every operation, not only at the end: an invariant that breaks and
is then repaired by the next sort would otherwise pass."
  (dotimes (seed vm-fuzz-test-seeds)
    (vm-fuzz-test--with-folders (real virtuals 0)
      (ignore virtuals)
      (let ((broken (vm-fuzz-test--run seed real nil)))
        (should-not broken)))))

(ert-deftest vm-fuzz-test-virtual-folders-survive-random-operations ()
  "The same with two virtual folders over the real one, and omit in the mix.
This is the configuration #569 and #571 were about: mirrors registered on a real
message, three lists to keep in step, and `vm-expunge-folder' splicing all of
them in one pass."
  (dotimes (seed vm-fuzz-test-seeds)
    (vm-fuzz-test--with-folders (real virtuals 2)
      (let ((broken (vm-fuzz-test--run seed real virtuals)))
        (should-not broken)))))

(provide 'vm-fuzz-test)

;;; vm-fuzz-test.el ends here
