;;; flamegraph-tests.el --- Tests for flamegraph.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dmitry Gutov

;; Author: Dmitry Gutov <dmitry@gutov.dev>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Run with:
;;   emacs -Q -batch -L . -l test/flamegraph-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'profiler)
(require 'flamegraph)

;;; Frame name + embedded source location parsing

(ert-deftest flamegraph-test-entry-location-perf ()
  "FUNC:/abs/path:LINE form from \"perf script -F +srcline\" folded stacks."
  (should (equal (flamegraph--entry-location
                  "redisplay_window:/path/to/emacs/src/xdisp.c:21020")
                 (cons "/path/to/emacs/src/xdisp.c" 21020))))

(ert-deftest flamegraph-test-entry-location-pyspy ()
  "NAME (FILE:LINE) form, as py-spy emits."
  (should (equal (flamegraph--entry-location "work (app/main.py:20)")
                 (cons "app/main.py" 20))))

(ert-deftest flamegraph-test-entry-location-rbspy ()
  "NAME - FILE:LINE form, as rbspy emits."
  (should (equal (flamegraph--entry-location "block in foo - lib/foo.rb:12")
                 (cons "lib/foo.rb" 12))))

(ert-deftest flamegraph-test-entry-location-none ()
  "Strings without an embedded location return nil."
  (should (null (flamegraph--entry-location "emacs")))
  (should (null (flamegraph--entry-location "[unknown]")))
  (should (null (flamegraph--entry-location ""))))

;;; Display name strips the embedded location

(ert-deftest flamegraph-test-frame-display-name-strips-location ()
  (should (equal (flamegraph--frame-display-name
                  "redisplay_window:/path/to/emacs/src/xdisp.c:21020")
                 "redisplay_window"))
  (should (equal (flamegraph--frame-display-name "work (app/main.py:20)")
                 "work"))
  (should (equal (flamegraph--frame-display-name "block in foo - lib/foo.rb:12")
                 "block in foo")))

(ert-deftest flamegraph-test-frame-display-name-pass-through ()
  (should (equal (flamegraph--frame-display-name "emacs") "emacs"))
  (should (equal (flamegraph--frame-display-name "[unknown]") "[unknown]")))

;;; skip-through-p / callee-name-acceptable-p

(ert-deftest flamegraph-test-skip-through-p ()
  (should (flamegraph--skip-through-p 'if))
  (should (flamegraph--skip-through-p 'let))
  (should (flamegraph--skip-through-p 'progn))
  (should (flamegraph--skip-through-p 'condition-case))
  (should-not (flamegraph--skip-through-p 'mapcar))
  (should-not (flamegraph--skip-through-p 'flamegraph-describe))
  (should-not (flamegraph--skip-through-p "string-entry"))
  (should-not (flamegraph--skip-through-p nil)))

(ert-deftest flamegraph-test-callee-name-acceptable-p ()
  (should (flamegraph--callee-name-acceptable-p "foo"))
  (should (flamegraph--callee-name-acceptable-p "foo-bar"))
  (should (flamegraph--callee-name-acceptable-p
           "Fnext_single_char_property_change"))
  (should-not (flamegraph--callee-name-acceptable-p ""))
  (should-not (flamegraph--callee-name-acceptable-p "[unknown]"))
  (should-not (flamegraph--callee-name-acceptable-p "foo bar"))
  (should-not (flamegraph--callee-name-acceptable-p "(foo)")))

;;; Enclosing form region

(ert-deftest flamegraph-test-enclosing-form-region ()
  "From a position on `bar', the enclosing form is `(bar (baz))'."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar (baz)))")
    (goto-char (point-min))
    (re-search-forward "\\_<bar\\_>")
    (let ((r (flamegraph--enclosing-form-region (match-beginning 0))))
      (should r)
      (should (equal (buffer-substring-no-properties (car r) (cdr r))
                     "(bar (baz))")))))

;;; Walker — `flamegraph--collect-nested-call-sites'

(defmacro flamegraph-test--with-elisp-source (src &rest body)
  "Run BODY in a temp buffer holding SRC in `emacs-lisp-mode'."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,src)
     (goto-char (point-min))
     ,@body))

(defun flamegraph-test--region-texts (regions)
  "Texts spanned by REGIONS (a list of (BEG . END)) in current buffer."
  (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cdr r)))
          regions))

(ert-deftest flamegraph-test-walker-strict-nesting ()
  "Real-function chains narrow strictly: a stray sibling is not matched.
For `(foo (bar (baz)))' with a separate `(bar 'unrelated)' elsewhere,
only the structurally-nested foo/bar/baz should be returned — the
stray `bar' is outside the foo-form's region."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (foo (bar (baz)))
  (bar 'unrelated))"
    (let* ((baz (profiler-make-calltree :entry 'baz :count 1))
           (bar (profiler-make-calltree :entry 'bar :count 1 :children (list baz)))
           (foo (profiler-make-calltree :entry 'foo :count 1 :children (list bar)))
           (outer (profiler-make-calltree :entry 'outer :count 2
                                          :children (list foo)))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max))))
      (should (equal (flamegraph-test--region-texts regions)
                     '("foo" "bar" "baz"))))))

(ert-deftest flamegraph-test-walker-macro-expansion-transparent ()
  "Skip-through frames absent from source must not stop the descent.
The profiler records post-expansion frames (here `while'/`let' from
`dolist'); the walker should treat them as transparent and still find
`company--perform' in the source."
  (flamegraph-test--with-elisp-source
      "(defun outer (xs)
  (dolist (x xs)
    (company--perform x)))"
    (let* ((cp (profiler-make-calltree :entry 'company--perform :count 100))
           (let-n (profiler-make-calltree :entry 'let :count 100
                                          :children (list cp)))
           (while-n (profiler-make-calltree :entry 'while :count 100
                                            :children (list let-n)))
           (outer (profiler-make-calltree :entry 'outer :count 100
                                          :children (list while-n)))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max))))
      (should (equal (flamegraph-test--region-texts regions)
                     '("company--perform"))))))

(ert-deftest flamegraph-test-walker-no-special-form-anchoring ()
  "Special forms must never anchor the search.
A literal `if' in source unrelated to the profile's top-level `if'
(typical with `when'/`unless' macro expansion) should not trap the
walker into searching only inside that literal `if' form — the real
callees outside it must still be found."
  (flamegraph-test--with-elisp-source
      "(defun caller ()
  (when test                                ; profile records (if test ...)
    (company-abort))
  (if cond                                  ; literal `if', unrelated
      (company-call-frontends 'cmd)
    (run-with-timer 1 nil 'k)))"
    (let* ((cabort (profiler-make-calltree :entry 'company-abort :count 1))
           (ccf    (profiler-make-calltree :entry 'company-call-frontends
                                           :count 1))
           (rwt    (profiler-make-calltree :entry 'run-with-timer :count 1))
           (deep   (profiler-make-calltree :entry 'progn :count 3
                                           :children (list cabort ccf rwt)))
           (top-if (profiler-make-calltree :entry 'if :count 3
                                           :children (list deep)))
           (caller (profiler-make-calltree :entry 'caller :count 3
                                           :children (list top-if)))
           (regions (flamegraph--collect-nested-call-sites
                     caller (point-min) (point-max))))
      (should (equal (sort (flamegraph-test--region-texts regions) #'string<)
                     '("company-abort" "company-call-frontends"
                       "run-with-timer"))))))

(ert-deftest flamegraph-test-walker-unsearchable-names-skipped ()
  "An unsearchable name (e.g. \"[unknown]\") must not error or recurse."
  (flamegraph-test--with-elisp-source
      "(defun outer () (foo))"
    (let* ((foo (profiler-make-calltree :entry 'foo :count 1))
           (unk (profiler-make-calltree :entry "[unknown]" :count 1))
           (outer (profiler-make-calltree :entry 'outer :count 2
                                          :children (list unk foo)))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max))))
      (should (equal (flamegraph-test--region-texts regions) '("foo"))))))

;;; `flamegraph--symbol-location'

(ert-deftest flamegraph-test-symbol-location ()
  "Find the source file and line of a real Elisp function."
  (let ((loc (flamegraph--symbol-location 'flamegraph-describe)))
    (should loc)
    (should (stringp (car loc)))
    (should (string-suffix-p "flamegraph.el" (car loc)))
    (should (integerp (cdr loc)))
    (should (> (cdr loc) 0))))

(ert-deftest flamegraph-test-symbol-location-unbound ()
  (should (null (flamegraph--symbol-location
                 'flamegraph-test--does-not-exist))))

;;; `flamegraph--source-snippet' (end-to-end)

(defmacro flamegraph-test--with-temp-source (var content &rest body)
  "Bind VAR to a temp .el file containing CONTENT, run BODY, then delete."
  (declare (indent 2))
  `(let ((,var (make-temp-file "flamegraph-snip-" nil ".el" ,content)))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(ert-deftest flamegraph-test-source-snippet-marks-sampled-line ()
  "The sampled line carries the ▸ marker."
  (flamegraph-test--with-temp-source path
      "(defun outer ()\n  (foo (bar))\n  (baz))\n"
    (let ((s (flamegraph--source-snippet path 2 nil)))
      (should s)
      (should (string-match-p "^ ▸ +2 " s)))))

(ert-deftest flamegraph-test-source-snippet-applies-highlight-face ()
  "Callee matches in the snippet carry the `flamegraph-call-site' face."
  (flamegraph-test--with-temp-source path
      "(defun outer ()\n  (foo (bar)))\n"
    (let* ((bar (profiler-make-calltree :entry 'bar :count 1))
           (foo (profiler-make-calltree :entry 'foo :count 1
                                        :children (list bar)))
           (outer (profiler-make-calltree :entry 'outer :count 1
                                          :children (list foo)))
           (s (flamegraph--source-snippet
               path 1
               (lambda (b e)
                 (flamegraph--collect-nested-call-sites outer b e)))))
      (should s)
      (should (text-property-any 0 (length s)
                                 'face 'flamegraph-call-site s)))))

(provide 'flamegraph-tests)
;;; flamegraph-tests.el ends here
