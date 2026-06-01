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

(ert-deftest flamegraph-test-enclosing-head-form-region ()
  "Only a form head can anchor that form's argument region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo #'bar (baz))")
    (goto-char (point-min))
    (re-search-forward "\\_<foo\\_>")
    (should (flamegraph--enclosing-head-form-region (match-beginning 0)))
    (goto-char (point-min))
    (re-search-forward "\\_<bar\\_>")
    (should-not (flamegraph--enclosing-head-form-region (match-beginning 0)))
    (goto-char (point-min))
    (re-search-forward "\\_<baz\\_>")
    (should (flamegraph--enclosing-head-form-region (match-beginning 0)))))

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
  "Texts spanned by REGIONS (a list of (BEG END WEIGHT)) in current buffer."
  (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cadr r)))
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

(ert-deftest flamegraph-test-walker-self-recursion-not-renested ()
  "A function called once in source but recursing deeply in the profile
is shown once, not as a chain: searching a matched call's arguments must
not re-match the call's own head symbol."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (rec x))"
    (let* ((rec3 (profiler-make-calltree :entry 'rec :count 1))
           (rec2 (profiler-make-calltree :entry 'rec :count 2
                                         :children (list rec3)))
           (rec1 (profiler-make-calltree :entry 'rec :count 3
                                         :children (list rec2)))
           (outer (profiler-make-calltree :entry 'outer :count 3
                                          :children (list rec1)))
           (shown (make-hash-table :test 'eq))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max) shown)))
      ;; Only the one written `(rec x)' is a call site.
      (should (equal (flamegraph-test--region-texts regions) '("rec")))
      (should (gethash rec1 shown))
      (should-not (gethash rec2 shown))
      (should-not (gethash rec3 shown)))))

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

(ert-deftest flamegraph-test-walker-ignores-string-and-comment-matches ()
  "A callee name mentioned in a docstring or comment is not a call site:
only the real call in code is matched, not the earlier textual mentions."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  \"Docstring mentioning foo.\"
  ;; A comment mentioning foo.
  (foo))"
    (let* ((foo (profiler-make-calltree :entry 'foo :count 1))
           (outer (profiler-make-calltree :entry 'outer :count 1
                                          :children (list foo)))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max))))
      ;; Exactly one region: the call on the last line, not the docstring
      ;; or comment occurrences.
      (should (equal (flamegraph-test--region-texts regions) '("foo")))
      (should (= 4 (line-number-at-pos (car (car regions))))))))

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

(ert-deftest flamegraph-test-walker-shows-external-anonymous-callback-frame ()
  "An external callback invoked via `funcall' is shown as a frame, but
its body is not part of the current definition."
  (flamegraph-test--with-elisp-source
      "(defun outer (cb)
  (funcall cb))"
    (let* ((deep (profiler-make-calltree :entry 'deep-call :count 10))
           (clos (profiler-make-calltree
                  :entry (make-byte-code 257 "\300\207" [] 2)
                  :count 10 :children (list deep)))
           (fc   (profiler-make-calltree :entry 'funcall :count 10
                                         :children (list clos)))
           (outer (profiler-make-calltree :entry 'outer :count 10
                                          :children (list fc)))
           (shown (make-hash-table :test 'eq)))
      (flamegraph--collect-nested-call-sites
       outer (point-min) (point-max) shown)
      (should (gethash fc shown))
      (should (gethash clos shown))
      (should-not (gethash deep shown)))))

(ert-deftest flamegraph-test-walker-shows-source-lambda-callback ()
  "An anonymous callback is shown when it corresponds to a lambda in the
current definition, but the lambda's callees still use normal anchoring."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (funcall (lambda ()
             (deep-call))))"
    (let* ((hidden (profiler-make-calltree :entry 'hidden-body :count 5))
           (deep (profiler-make-calltree :entry 'deep-call :count 10
                                         :children (list hidden)))
           (clos (profiler-make-calltree
                  :entry (make-byte-code 257 "\300\207" [] 2)
                  :count 10 :children (list deep)))
           (fc   (profiler-make-calltree :entry 'funcall :count 10
                                         :children (list clos)))
           (outer (profiler-make-calltree :entry 'outer :count 10
                                          :children (list fc)))
           (shown (make-hash-table :test 'eq))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max) shown)))
      (should (equal (flamegraph-test--region-texts regions)
                     '("funcall" "lambda" "deep-call")))
      (should (gethash fc shown))
      (should (gethash clos shown))
      (should (gethash deep shown))
      (should-not (gethash hidden shown)))))

(ert-deftest flamegraph-test-walker-multiple-source-lambdas-use-normal-matching ()
  "Anonymous callbacks use the same all-occurrences matching as named calls."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (register (lambda ()
              (first-call))
            (lambda ()
              (second-call))))"
    (let* ((first-call (profiler-make-calltree :entry 'first-call :count 6))
           (second-call (profiler-make-calltree :entry 'second-call :count 4))
           (first-clos (profiler-make-calltree
                        :entry (make-byte-code 257 "\300\207" [] 2)
                        :count 6 :children (list first-call)))
           (second-clos (profiler-make-calltree
                         :entry (make-byte-code 257 "\300\207" [] 2)
                         :count 4 :children (list second-call)))
           (register (profiler-make-calltree :entry 'register :count 10
                                             :children (list first-clos
                                                             second-clos)))
           (outer (profiler-make-calltree :entry 'outer :count 10
                                          :children (list register)))
           (shown (make-hash-table :test 'eq))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max) shown)))
      ;; As with named functions, the profile does not identify which
      ;; textual occurrence corresponds to this frame, so each anonymous
      ;; frame searches all lambda occurrences in the current region.
      (should (equal (flamegraph-test--region-texts regions)
                     '("register" "lambda" "first-call" "lambda"
                       "lambda" "lambda" "second-call")))
      (should (gethash first-clos shown))
      (should (gethash first-call shown))
      (should (gethash second-clos shown))
      (should (gethash second-call shown)))))

(ert-deftest flamegraph-test-walker-shown-marks-source-calls ()
  "SHOWN marks calls found in the source and their source-found nesting,
but not a matched callee's own body.
`wrap', `inner' and `leaf' are in the source -> shown; `inner' nests
under `wrap'.  `not-in-source' is `leaf''s body -> not shown."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (wrap (inner))
  (leaf))"
    (let* ((inner (profiler-make-calltree :entry 'inner :count 10))
           (wrap  (profiler-make-calltree :entry 'wrap :count 10
                                          :children (list inner)))
           (hidden (profiler-make-calltree :entry 'not-in-source :count 5))
           (leaf  (profiler-make-calltree :entry 'leaf :count 5
                                          :children (list hidden)))
           (outer (profiler-make-calltree :entry 'outer :count 20
                                          :children (list wrap leaf)))
           (shown (make-hash-table :test 'eq))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max) shown)))
      (should (equal (flamegraph-test--region-texts regions)
                     '("wrap" "inner" "leaf")))
      (should (gethash wrap shown))
      (should (gethash inner shown))
      (should (gethash leaf shown))
      (should-not (gethash hidden shown)))))

(ert-deftest flamegraph-test-walker-quoted-function-does-not-anchor-body ()
  "A `#'callee' argument identifies CALLEE but not CALLEE's body.
Calls in the same argument list still belong to the surrounding call's
region."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (apply #'callee buffer
         (list (argument-call))))"
    (let* ((callee-body (profiler-make-calltree :entry 'callee-body
                                                :count 10))
           (callee (profiler-make-calltree :entry 'callee :count 10
                                           :children (list callee-body)))
           (argument-call (profiler-make-calltree :entry 'argument-call
                                                  :count 2))
           (list-node (profiler-make-calltree :entry 'list :count 2
                                              :children (list argument-call)))
           (apply-node (profiler-make-calltree :entry 'apply :count 12
                                               :children (list callee
                                                               list-node)))
           (outer (profiler-make-calltree :entry 'outer :count 12
                                          :children (list apply-node)))
           (shown (make-hash-table :test 'eq))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max) shown)))
      (should (equal (flamegraph-test--region-texts regions)
                     '("apply" "callee" "list" "argument-call")))
      (should (gethash apply-node shown))
      (should (gethash callee shown))
      (should-not (gethash callee-body shown))
      (should (gethash list-node shown))
      (should (gethash argument-call shown)))))

(ert-deftest flamegraph-test-walker-below-threshold-dropped ()
  "Callees below `flamegraph-call-site-threshold' are dropped with subtree.
hot at 50% is kept; cold at 1% (and its child) are not."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (hot (deep))
  (cold))"
    (let* ((deep  (profiler-make-calltree :entry 'deep :count 1))
           (hot   (profiler-make-calltree :entry 'hot :count 50
                                          :children (list deep)))
           (cold  (profiler-make-calltree :entry 'cold :count 1))
           (outer (profiler-make-calltree :entry 'outer :count 100
                                          :children (list hot cold)))
           (flamegraph-call-site-threshold 0.02)
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max))))
      ;; hot (50%) kept; deep is 1% of outer -> dropped; cold (1%) dropped.
      (should (equal (flamegraph-test--region-texts regions) '("hot"))))))

(ert-deftest flamegraph-test-walker-hides-matched-callee-body ()
  "A call found in source hides its own body, even through skip-throughs;
a call concealed by a macro (under a skip-through in the described body)
is still shown."
  (flamegraph-test--with-elisp-source
      "(defun outer ()
  (when flag
    (concealed))
  (matched))"
    (let* ((;; `matched' body: matched -> if -> deep-fn  (callee internals)
            deep-fn (profiler-make-calltree :entry 'deep-fn :count 40))
           (body-if (profiler-make-calltree :entry 'if :count 40
                                            :children (list deep-fn)))
           (matched (profiler-make-calltree :entry 'matched :count 40
                                            :children (list body-if)))
           ;; `when' expands to `if' (skip-through) in the described body,
           ;; concealing the real call `concealed'.
           (concealed (profiler-make-calltree :entry 'concealed :count 30))
           (when-if (profiler-make-calltree :entry 'if :count 30
                                            :children (list concealed)))
           (outer (profiler-make-calltree :entry 'outer :count 100
                                          :children (list matched when-if)))
           (shown (make-hash-table :test 'eq)))
      (flamegraph--collect-nested-call-sites
       outer (point-min) (point-max) shown)
      (should (gethash matched shown))      ; in source
      (should (gethash when-if shown))      ; scaffolding leading to a call
      (should (gethash concealed shown))    ; macro-concealed current-def call
      (should-not (gethash body-if shown))  ; matched's body
      (should-not (gethash deep-fn shown))))) ; matched's body, deeper

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

(defun flamegraph-test--has-face (string face)
  "Non-nil if FACE is applied anywhere in STRING (symbol or in a list)."
  (cl-loop for i below (length string)
           for f = (get-text-property i 'face string)
           thereis (or (eq f face) (and (listp f) (memq face f)))))

(defun flamegraph-test--text-has-face (string text face)
  "Non-nil if TEXT in STRING has FACE at its first character."
  (let ((pos (string-match-p (regexp-quote text) string)))
    (and pos
         (let ((f (get-text-property pos 'face string)))
           (or (eq f face) (and (listp f) (memq face f)))))))

(ert-deftest flamegraph-test-source-snippet-applies-highlight-face ()
  "Callee matches in the snippet carry a heat face scaled by share.
foo at 100% of outer is hot; bar at 10% is warm."
  (flamegraph-test--with-temp-source path
      "(defun outer ()\n  (foo (bar)))\n"
    (let* ((bar (profiler-make-calltree :entry 'bar :count 1))
           (foo (profiler-make-calltree :entry 'foo :count 10
                                        :children (list bar)))
           (outer (profiler-make-calltree :entry 'outer :count 10
                                          :children (list foo)))
           (s (flamegraph--source-snippet path 1 outer)))
      (should s)
      (should (flamegraph-test--has-face s 'flamegraph-call-site-hot))
      (should (flamegraph-test--has-face s 'flamegraph-call-site-warm)))))

(ert-deftest flamegraph-test-source-snippet-threshold-only-affects-regions ()
  "Below-threshold callees fill SHOWN but are not highlighted."
  (flamegraph-test--with-temp-source path
      "(defun outer ()
  (foo)
  (cold))
"
    (let* ((foo (profiler-make-calltree :entry 'foo :count 99))
           (cold (profiler-make-calltree :entry 'cold :count 1))
           (outer (profiler-make-calltree :entry 'outer :count 100
                                          :children (list foo cold)))
           (flamegraph-call-site-threshold 0.02)
           (shown (make-hash-table :test 'eq))
           (s (flamegraph--source-snippet path 1 outer shown)))
      (should s)
      (should (gethash foo shown))
      (should (gethash cold shown))
      (should (flamegraph-test--text-has-face
               s "foo" 'flamegraph-call-site-hot))
      (should-not (flamegraph-test--text-has-face
                   s "cold" 'flamegraph-call-site-cool)))))

(ert-deftest flamegraph-test-source-snippet-lines-ret-visits-source ()
  "RET on a snippet source line, including the final EOL, visits that line."
  (flamegraph-test--with-temp-source path
      "(defun outer ()
  (foo)
  (bar))
"
    (let ((snippet (flamegraph--source-snippet path 2 nil))
          visited)
      (should snippet)
      (cl-letf (((symbol-function 'flamegraph--visit-file)
                 (lambda (p l) (setq visited (cons p l)))))
        (with-temp-buffer
          (insert snippet)
          (goto-char (point-min))
          (re-search-forward " 3  ")
          (end-of-line)
          (should-not (button-at (point)))
          (let ((map (get-text-property (point) 'keymap)))
            (should map)
            (call-interactively (lookup-key map (kbd "RET")))))
        (should (equal visited (cons path 3)))))))

;;; Calls-tree rendering — `flamegraph--insert-call-tree'

(defun flamegraph-test--render-call-tree (node ref &optional shown-nodes)
  "Render NODE's call tree (REF samples for percent scaling) and return
the resulting string.  Only nodes in SHOWN-NODES are rendered; when it is
nil, every descendant of NODE is shown."
  (let ((shown (make-hash-table :test 'eq)))
    (if shown-nodes
        (dolist (n shown-nodes) (puthash n t shown))
      (cl-labels ((mark (n)
                    (dolist (k (profiler-calltree-children n))
                      (puthash k t shown)
                      (mark k))))
        (mark node)))
    (with-temp-buffer
      (flamegraph--insert-call-tree node ref ref 0 nil shown t)
      (buffer-string))))

(defun flamegraph-test--line-with (str lines)
  "Return the first LINES element containing STR as a whole symbol."
  (cl-find-if (lambda (l)
                (string-match-p (concat "\\_<" (regexp-quote str) "\\_>") l))
              lines))

(defun flamegraph-test--indent (line)
  "Column where LINE's name starts, after the leading count field."
  (if (string-match "\\`[ ,0-9]+?\\([^ ,0-9]\\)" line)
      (match-beginning 1)
    0))

(ert-deftest flamegraph-test-call-tree-children-sorted-by-count ()
  "Direct children appear sorted by count, descending."
  (let* ((bar (profiler-make-calltree :entry 'bar :count 30))
         (baz (profiler-make-calltree :entry 'baz :count 70))
         (foo (profiler-make-calltree :entry 'foo :count 100
                                      :children (list bar baz))))
    (let* ((out (flamegraph-test--render-call-tree foo 100))
           (baz-pos (string-match "\\_<baz\\_>" out))
           (bar-pos (string-match "\\_<bar\\_>" out)))
      (should baz-pos)
      (should bar-pos)
      (should (< baz-pos bar-pos)))))

(ert-deftest flamegraph-test-call-tree-shown-child-indented ()
  "A shown child renders its shown sub-children indented underneath."
  (let* ((real-child (profiler-make-calltree :entry 'real-fn :count 80))
         (if-node    (profiler-make-calltree :entry 'if :count 80
                                             :children (list real-child)))
         (other      (profiler-make-calltree :entry 'other-fn :count 20))
         (root (profiler-make-calltree :entry 'root :count 100
                                       :children (list if-node other))))
    (let* ((out (flamegraph-test--render-call-tree
                 root 100 (list if-node real-child other)))
           (lines (split-string out "\n" t))
           (if-line    (flamegraph-test--line-with "if" lines))
           (rf-line    (flamegraph-test--line-with "real-fn" lines))
           (other-line (flamegraph-test--line-with "other-fn" lines)))
      (should if-line)
      (should rf-line)
      (should other-line)
      ;; real-fn is indented deeper than its parent.
      (should (> (flamegraph-test--indent rf-line)
                 (flamegraph-test--indent if-line)))
      ;; The sibling sits at the same depth as `if'.
      (should (= (flamegraph-test--indent if-line)
                 (flamegraph-test--indent other-line))))))

(ert-deftest flamegraph-test-call-tree-unshown-omitted ()
  "A child absent from SHOWN is omitted, along with its subtree."
  (let* ((grand (profiler-make-calltree :entry 'grand :count 50))
         (kid   (profiler-make-calltree :entry 'kid :count 50
                                        :children (list grand)))
         (root (profiler-make-calltree :entry 'root :count 100
                                       :children (list kid))))
    ;; Only kid is shown, not grand.
    (let* ((out (flamegraph-test--render-call-tree root 100 (list kid)))
           (lines (split-string out "\n" t)))
      (should (flamegraph-test--line-with "kid" lines))
      (should-not (flamegraph-test--line-with "grand" lines)))))

(defun flamegraph-test--name-dimmed-p (str name)
  "Non-nil if NAME's button in STR carries the `shadow' face."
  (let ((pos (string-match (concat "\\_<" (regexp-quote name) "\\_>") str)))
    (and pos
         (let ((face (get-text-property pos 'face str)))
           (and (listp face) (memq 'shadow face))))))

(ert-deftest flamegraph-test-call-tree-dims-below-threshold ()
  "Callees below `flamegraph-call-site-threshold' get a dimmed name;
those at or above it do not."
  (let* ((cold (profiler-make-calltree :entry 'cold :count 1))
         (hot  (profiler-make-calltree :entry 'hot :count 99))
         (root (profiler-make-calltree :entry 'root :count 100
                                       :children (list hot cold)))
         (flamegraph-call-site-threshold 0.02)
         (out (flamegraph-test--render-call-tree root 100)))
    ;; cold is 1% -> dimmed; hot is 99% -> normal.
    (should (flamegraph-test--name-dimmed-p out "cold"))
    (should-not (flamegraph-test--name-dimmed-p out "hot"))))

(ert-deftest flamegraph-test-call-tree-percent-scaled-to-ref ()
  "Sub-children's percentage uses REF (the described frame's count),
not their immediate parent — so all percents add up under that frame."
  ;; root 1000, if 500, fn 250 → fn's printed share is 25%, not 50%.
  (let* ((fn (profiler-make-calltree :entry 'fn :count 250))
         (if-node (profiler-make-calltree :entry 'if :count 500
                                          :children (list fn)))
         (root (profiler-make-calltree :entry 'root :count 1000
                                       :children (list if-node))))
    (let* ((out (flamegraph-test--render-call-tree root 1000 (list if-node fn)))
           (fn-line (flamegraph-test--line-with
                     "fn" (split-string out "\n" t))))
      (should fn-line)
      (should (string-match-p "(25%)" fn-line))
      (should-not (string-match-p "(50%)" fn-line)))))

(ert-deftest flamegraph-test-call-tree-rows-are-describe-buttons ()
  "Every rendered row is a clickable describe-button."
  (let* ((bar (profiler-make-calltree :entry 'bar :count 1))
         (foo (profiler-make-calltree :entry 'foo :count 1
                                      :children (list bar)))
         (shown (make-hash-table :test 'eq)))
    (puthash bar t shown)
    (with-temp-buffer
      (flamegraph--insert-call-tree foo 1 1 0 nil shown t)
      (goto-char (point-min))
      (let ((b (next-button (point))))
        (should b)
        (should (eq (button-type b) 'help-xref))
        (should (eq (button-get b 'help-function)
                    #'flamegraph--describe-frame))))))

;;; Heading rendering in `flamegraph--describe-frame'

(defun flamegraph-test--describe-help (node &optional profiler-el-calls)
  "Render NODE in the help buffer via `flamegraph--describe-frame' and
return that buffer's contents (a propertized string)."
  (require 'help-mode)
  (flamegraph--describe-frame node (profiler-calltree-count node) nil
                              profiler-el-calls)
  (with-current-buffer (help-buffer) (buffer-string)))

(ert-deftest flamegraph-test-describe-symbol-heading-is-help-button ()
  "For an fboundp symbol entry the heading is a `help-function' button."
  (let ((node (profiler-make-calltree :entry 'flamegraph-describe
                                      :count 10)))
    (flamegraph-test--describe-help node)
    (with-current-buffer (help-buffer)
      (goto-char (point-min))
      (let ((b (button-at (point))))
        (should b)
        (should (eq (button-type b) 'help-function))
        (should (equal (button-get b 'help-args)
                       '(flamegraph-describe)))))))

(ert-deftest flamegraph-test-describe-string-heading-is-plain ()
  "For a non-symbol entry, the heading is plain text (no button at point 1)."
  (let ((node (profiler-make-calltree
               :entry "redisplay_window:/tmp/xdisp.c:1"
               :count 10)))
    (flamegraph-test--describe-help node)
    (with-current-buffer (help-buffer)
      (goto-char (point-min))
      (should-not (and (button-at (point))
                       (eq (button-type (button-at (point)))
                           'help-function))))))

(ert-deftest flamegraph-test-describe-folded-stack-calls-stay-flat ()
  "String frames get source snippets but not profiler.el Calls pruning."
  (flamegraph-test--with-temp-source path
      "(defun outer ()
  (foo (bar)))
"
    (let* ((bar (profiler-make-calltree :entry "bar" :count 10))
           (foo (profiler-make-calltree :entry "foo" :count 10
                                        :children (list bar)))
           (outer (profiler-make-calltree
                   :entry (format "outer (%s:1)" path)
                   :count 10 :children (list foo)))
           (out (flamegraph-test--describe-help outer)))
      (should (string-match-p "foo (bar)" out))
      (should (flamegraph-test--has-face out 'flamegraph-call-site-hot))
      (should (string-match "\nCalls\n" out))
      (let ((calls (substring out (match-end 0))))
        (should (flamegraph-test--line-with "foo" (split-string calls "\n" t)))
        (should-not (flamegraph-test--line-with "bar"
                                                (split-string calls "\n" t)))))))

;;; `flamegraph-find-source' branches

(defun flamegraph-test--frame (entry)
  "Build a `flamegraph-frame' wrapping a calltree node carrying ENTRY."
  (flamegraph--frame
   :node (profiler-make-calltree :entry entry :count 1)
   :start 0 :width 1 :depth 0))

(defmacro flamegraph-test--with-mocks (bindings &rest body)
  "Run BODY with each (FN REPL) in BINDINGS shadowing FN's definition."
  (declare (indent 1))
  `(cl-letf* ,(mapcar (lambda (b) `((symbol-function ',(car b)) ,(cadr b)))
                      bindings)
     ,@body))

(ert-deftest flamegraph-test-find-source-fboundp-symbol ()
  "An fboundp symbol entry routes to `find-function-other-window'."
  (let (called)
    (flamegraph-test--with-mocks
        ((flamegraph--frame-at-point
          (lambda () (flamegraph-test--frame 'flamegraph-describe)))
         (find-function-other-window (lambda (sym) (setq called sym))))
      (flamegraph-find-source))
    (should (eq called 'flamegraph-describe))))

(ert-deftest flamegraph-test-find-source-embedded-location ()
  "A string entry with an embedded srcline visits via `flamegraph--visit-file'."
  (flamegraph-test--with-temp-source path "(defun outer () 1)\n"
    (let (visited)
      (flamegraph-test--with-mocks
          ((flamegraph--frame-at-point
            (lambda ()
              (flamegraph-test--frame (format "outer:%s:1" path))))
           (flamegraph--visit-file
            (lambda (p l) (setq visited (cons p l)))))
        (flamegraph-find-source))
      (should (equal visited (cons path 1))))))

(ert-deftest flamegraph-test-find-source-no-frame ()
  "Without a frame at point the command messages and exits."
  (let (msg)
    (flamegraph-test--with-mocks
        ((flamegraph--frame-at-point (lambda () nil))
         (message (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (flamegraph-find-source))
    (should (equal msg "No frame at point"))))

(ert-deftest flamegraph-test-find-source-no-location ()
  "A string entry without an embedded srcline messages and exits."
  (let (msg)
    (flamegraph-test--with-mocks
        ((flamegraph--frame-at-point
          (lambda () (flamegraph-test--frame "emacs")))
         (message (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (flamegraph-find-source))
    (should (string-match-p "\\`No source location in: emacs" msg))))

(ert-deftest flamegraph-test-find-source-missing-file ()
  "When the resolved file doesn't exist the command messages and exits."
  (let (msg)
    (flamegraph-test--with-mocks
        ((flamegraph--frame-at-point
          (lambda () (flamegraph-test--frame "foo:/does/not/exist.c:10")))
         (message (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (flamegraph-find-source))
    (should (string-match-p "\\`Source file not found:" msg))))

;;; `flamegraph--resolve-source' precedence

(ert-deftest flamegraph-test-resolve-source-custom-dir-wins ()
  "`flamegraph-source-directory' takes precedence over DIRECTORY."
  (let ((flamegraph-source-directory "/from/custom/"))
    (should (equal (flamegraph--resolve-source "foo.c" "/from/data/")
                   "/from/custom/foo.c"))))

(ert-deftest flamegraph-test-resolve-source-data-dir ()
  "DIRECTORY (the data file's dir) is used when custom is nil."
  (let ((flamegraph-source-directory nil))
    (should (equal (flamegraph--resolve-source "foo.c" "/from/data/")
                   "/from/data/foo.c"))))

(ert-deftest flamegraph-test-resolve-source-default-dir ()
  "`default-directory' is the last resort."
  (let ((flamegraph-source-directory nil)
        (default-directory "/from/default/"))
    (should (equal (flamegraph--resolve-source "foo.c" nil)
                   "/from/default/foo.c"))))

(ert-deftest flamegraph-test-resolve-source-absolute-bypasses-base ()
  "Absolute paths bypass every base directory."
  (let ((flamegraph-source-directory "/from/custom/"))
    (should (equal (flamegraph--resolve-source "/abs/foo.c" "/from/data/")
                   "/abs/foo.c"))))

(provide 'flamegraph-tests)
;;; flamegraph-tests.el ends here
