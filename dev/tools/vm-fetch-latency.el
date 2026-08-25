;;; vm-fetch-latency.el --- what stops Emacs during an asynchronous fetch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Times an IMAP fetch on the non-blocking driver and says where the pauses
;; are.  A step is one `vm-net--resume' call: Emacs is stopped for exactly as
;; long as that runs, so the worst step is the worst stutter a reader sees.
;;
;; Run it against a mailbox big enough to show the problem -- the pauses go
;; with the size of the heap, and a dozen messages have none:
;;
;;   make
;;   emacs -batch -Q -L lisp -l dev/tools/vm-fetch-latency.el \
;;     imap:127.0.0.1:143:big:login:vmtest1:vm-lives
;;
;; dev/tools/fill-imap-mailbox.py fills such a mailbox.  Add -functions for
;; the longest single call of every VM function, which is how the quadratic
;; `vm-imap-net-uid-held-p' was found; the step report on its own is what says
;; whether a pause is VM working or Emacs collecting.
;;
;; It must be run against a compiled tree.  The parse runs once per token and
;; the generators around it rebuild their closures per call interpreted, so a
;; measurement taken from source is of the interpreter and nothing else.

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'vm-autoloads)
(require 'vm-imap-net)
(require 'vm-net)
(require 'vm-imap)

(defvar vm-fetch-latency-steps nil
  "Every step of the fetch: (SECONDS COLLECTING COLLECTIONS), newest first.")

(defvar vm-fetch-latency-slices 0
  "How many steps handed Emacs back with work still to do.")

(defvar vm-fetch-latency-longest (make-hash-table :test 'eq)
  "Function to (SECONDS . CALLS), the longest single call of each.")

(defvar vm-fetch-latency-allocation nil
  "What `memory-use-counts' said before the fetch.")

;;; Watching the driver

(defun vm-fetch-latency--time-a-step (original &rest arguments)
  "Time one ORIGINAL resume of a session, called with ARGUMENTS.
The collection inside a step is what tells a pause VM is responsible for
from one it merely happens to be running during."
  (let ((start (float-time))
	(collections gcs-done)
	(collected gc-elapsed))
    (unwind-protect (apply original arguments)
      (push (list (- (float-time) start)
		  (- gc-elapsed collected)
		  (- gcs-done collections))
	    vm-fetch-latency-steps))))

(defun vm-fetch-latency--count-a-slice (&rest _)
  "Count a step that asked to be called again."
  (setq vm-fetch-latency-slices (1+ vm-fetch-latency-slices)))

(defun vm-fetch-latency--time-a-call (symbol)
  "Record the longest single call of SYMBOL, nested work included."
  (advice-add symbol :around
	      (lambda (original &rest arguments)
		(let ((start (float-time)))
		  (unwind-protect (apply original arguments)
		    (vm-fetch-latency--remember symbol (- (float-time) start)))))
	      (list (cons 'name 'vm-fetch-latency))))

(defun vm-fetch-latency--remember (symbol taken)
  "Remember that a call of SYMBOL took TAKEN seconds."
  (let ((seen (gethash symbol vm-fetch-latency-longest)))
    (puthash symbol
	     (cons (max taken (or (car seen) 0)) (1+ (or (cdr seen) 0)))
	     vm-fetch-latency-longest)))

(defun vm-fetch-latency--vm-functions ()
  "Every VM function that can be advised."
  (seq-filter (lambda (symbol)
		(and (fboundp symbol)
		     (not (special-form-p symbol))
		     (not (macrop symbol))
		     ;; this file's own functions: timing the timer is a
		     ;; recursion, and timing the fetch is timing everything
		     (not (string-prefix-p "vm-fetch-latency" (symbol-name symbol)))
		     ;; the wait is the whole fetch, which is not a call
		     ;; worth reporting as a long one
		     (not (eq symbol 'vm-imap-net-wait))))
	      (apropos-internal "^vm-")))

(defun vm-fetch-latency--watch (functions)
  "Time the driver's steps, and every VM function too when FUNCTIONS."
  (advice-add 'vm-net--resume :around #'vm-fetch-latency--time-a-step)
  (advice-add 'vm-net--continue-soon :before #'vm-fetch-latency--count-a-slice)
  (when functions
    (dolist (symbol (vm-fetch-latency--vm-functions))
      (ignore-errors (vm-fetch-latency--time-a-call symbol)))))

;;; Running the fetch

(defun vm-fetch-latency--require-compiled ()
  "Refuse to measure an uncompiled tree."
  (let ((reader (symbol-function 'vm-imap-net-parse-object)))
    (unless (or (byte-code-function-p reader)
		(and (fboundp 'subr-native-elisp-p)
		     (subr-native-elisp-p reader)))
      (error "VM is not compiled here, so any measurement would be of the \
interpreter: run make first"))))

(defun vm-fetch-latency--fetch (spec seconds)
  "Visit SPEC, wait up to SECONDS for its fetch, and answer with what it took."
  (let ((cache (make-temp-file "vm-fetch-latency" t))
	(start (float-time)))
    (unwind-protect
	(let ((vm-imap-folder-cache-directory cache)
	      (vm-imap-server-timeout seconds)
	      (vm-frame-per-folder nil)
	      (vm-mutable-frame-configuration nil))
	  (setq vm-fetch-latency-allocation (memory-use-counts))
	  (vm-visit-imap-folder spec)
	  (unless (vm-imap-net-wait nil seconds)
	    (error "The fetch did not finish inside %d seconds" seconds))
	  (cons (- (float-time) start) (length vm-message-list)))
      (vm-fetch-latency--leave-quietly)
      (delete-directory cache t))))

(defun vm-fetch-latency--leave-quietly ()
  "Let batch Emacs exit without asking about the folder it just filled."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (set-buffer-modified-p nil)))))

;;; Saying what happened

(defconst vm-fetch-latency-allocation-names
  '(conses floats "vector words" symbols "string characters" intervals strings)
  "What `memory-use-counts' answers with, in the order it answers.")

(defun vm-fetch-latency--report-steps (took messages)
  "Print the steps of a fetch of MESSAGES messages that TOOK seconds."
  (let ((steps (sort (copy-sequence vm-fetch-latency-steps)
		     (lambda (a b) (> (car a) (car b))))))
    (princ (format "%d messages, %.1fs wall, %d steps, %d slices\n"
		   messages took (length steps) vm-fetch-latency-slices))
    (princ (format "%d collections in the steps, %.2fs of them\n"
		   (apply #'+ (mapcar (lambda (step) (nth 2 step)) steps))
		   (apply #'+ (mapcar (lambda (step) (nth 1 step)) steps))))
    (princ "\nlongest steps, and the collection inside each:\n")
    (dolist (step (seq-take steps 8))
      (princ (format "  %.3fs  %.3fs collecting, %d collections\n"
		     (nth 0 step) (nth 1 step) (nth 2 step))))))

(defun vm-fetch-latency--report-allocation ()
  "Print what the fetch allocated.
Garbage is what forces the collections, so a pause that is collection is
answered here rather than by anything the driver does."
  (princ "\nallocated by the fetch:\n")
  (cl-loop for name in vm-fetch-latency-allocation-names
	   for after in (memory-use-counts)
	   for before in vm-fetch-latency-allocation
	   do (princ (format "  %-18s %14d\n" name (- after before)))))

(defun vm-fetch-latency--report-functions ()
  "Print the functions with the longest single call."
  (let (rows)
    (maphash (lambda (symbol seen) (push (list (car seen) (cdr seen) symbol) rows))
	     vm-fetch-latency-longest)
    (princ (format "\n%-9s %8s  %s\n" "longest" "calls" "function"))
    (dolist (row (seq-take (sort rows (lambda (a b) (> (car a) (car b)))) 20))
      (princ (format "%-9.3f %8d  %s\n" (nth 0 row) (nth 1 row) (nth 2 row))))))

;;; The command line

(defun vm-fetch-latency--arguments (arguments)
  "The maildrop and options in ARGUMENTS, as (SPEC FUNCTIONS SECONDS)."
  (let ((spec (car (seq-remove (lambda (argument)
				 (string-prefix-p "-" argument))
			       arguments))))
    (unless spec
      (error "Name the maildrop to fetch, as \
imap:HOST:PORT:MAILBOX:login:USER:PASSWORD"))
    (list spec
	  (member "-functions" arguments)
	  (string-to-number (or (cadr (member "-seconds" arguments)) "600")))))

(defun vm-fetch-latency (arguments)
  "Measure the fetch ARGUMENTS name, and print where its pauses are."
  (vm-fetch-latency--require-compiled)
  (cl-destructuring-bind (spec functions seconds)
      (vm-fetch-latency--arguments arguments)
    (vm-fetch-latency--watch functions)
    (let ((fetch (vm-fetch-latency--fetch spec seconds)))
      (vm-fetch-latency--report-steps (car fetch) (cdr fetch)))
    (vm-fetch-latency--report-allocation)
    (when functions (vm-fetch-latency--report-functions))))

(when noninteractive
  (vm-fetch-latency argv)
  (setq argv nil))

(provide 'vm-fetch-latency)

;;; vm-fetch-latency.el ends here
