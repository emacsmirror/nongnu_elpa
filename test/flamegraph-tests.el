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

(ert-deftest flamegraph-test-walker-reached-tracks-source-nesting ()
  "REACHED marks a node iff a nested call of it is found in the source.
`wrap' is found and its child `inner' is found inside it -> reached;
`leaf' is found but its child is not in source -> not reached."
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
           (reached (make-hash-table :test 'eq))
           (regions (flamegraph--collect-nested-call-sites
                     outer (point-min) (point-max) reached)))
      (should (equal (flamegraph-test--region-texts regions)
                     '("wrap" "inner" "leaf")))
      (should (gethash wrap reached))
      (should-not (gethash leaf reached)))))

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

;;; Calls-tree rendering — `flamegraph--insert-call-tree'

(defun flamegraph-test--render-call-tree (node ref &optional reached-nodes)
  "Render NODE's call tree (REF samples for percent scaling) and return
the resulting string.  REACHED-NODES are the calltrees to mark as
expandable (their children shown nested)."
  (let ((reached (make-hash-table :test 'eq)))
    (dolist (n reached-nodes) (puthash n t reached))
    (with-temp-buffer
      (flamegraph--insert-call-tree node ref ref 0 nil reached)
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

(ert-deftest flamegraph-test-call-tree-reached-expanded-indented ()
  "A reached child renders its sub-children indented underneath; an
unreached sibling does not, and stays at the top depth."
  (let* ((real-child (profiler-make-calltree :entry 'real-fn :count 80))
         (if-node    (profiler-make-calltree :entry 'if :count 80
                                             :children (list real-child)))
         (other      (profiler-make-calltree :entry 'other-fn :count 20))
         (root (profiler-make-calltree :entry 'root :count 100
                                       :children (list if-node other))))
    (let* ((out (flamegraph-test--render-call-tree root 100 (list if-node)))
           (lines (split-string out "\n" t))
           (if-line    (flamegraph-test--line-with "if" lines))
           (rf-line    (flamegraph-test--line-with "real-fn" lines))
           (other-line (flamegraph-test--line-with "other-fn" lines)))
      (should if-line)
      (should rf-line)
      (should other-line)
      ;; real-fn is indented deeper than its reached parent.
      (should (> (flamegraph-test--indent rf-line)
                 (flamegraph-test--indent if-line)))
      ;; The unreached sibling sits at the same depth as `if'.
      (should (= (flamegraph-test--indent if-line)
                 (flamegraph-test--indent other-line))))))

(ert-deftest flamegraph-test-call-tree-unreached-not-expanded ()
  "A child not in REACHED is a leaf: its own descendants are not inlined."
  (let* ((grand (profiler-make-calltree :entry 'grand :count 50))
         (kid   (profiler-make-calltree :entry 'kid :count 50
                                        :children (list grand)))
         (root (profiler-make-calltree :entry 'root :count 100
                                       :children (list kid))))
    ;; kid is not passed as reached.
    (let* ((out (flamegraph-test--render-call-tree root 100))
           (lines (split-string out "\n" t)))
      (should (flamegraph-test--line-with "kid" lines))
      (should-not (flamegraph-test--line-with "grand" lines)))))

(ert-deftest flamegraph-test-call-tree-percent-scaled-to-ref ()
  "Sub-children's percentage uses REF (the described frame's count),
not their immediate parent — so all percents add up under that frame."
  ;; root 1000, if 500, fn 250 → fn's printed share is 25%, not 50%.
  (let* ((fn (profiler-make-calltree :entry 'fn :count 250))
         (if-node (profiler-make-calltree :entry 'if :count 500
                                          :children (list fn)))
         (root (profiler-make-calltree :entry 'root :count 1000
                                       :children (list if-node))))
    (let* ((out (flamegraph-test--render-call-tree root 1000 (list if-node)))
           (fn-line (flamegraph-test--line-with
                     "fn" (split-string out "\n" t))))
      (should fn-line)
      (should (string-match-p "(25%)" fn-line))
      (should-not (string-match-p "(50%)" fn-line)))))

(ert-deftest flamegraph-test-call-tree-rows-are-describe-buttons ()
  "Every rendered row is a clickable describe-button."
  (let* ((bar (profiler-make-calltree :entry 'bar :count 1))
         (foo (profiler-make-calltree :entry 'foo :count 1
                                      :children (list bar))))
    (with-temp-buffer
      (flamegraph--insert-call-tree foo 1 1 0 nil (make-hash-table :test 'eq))
      (goto-char (point-min))
      (let ((b (next-button (point))))
        (should b)
        (should (eq (button-type b) 'help-xref))
        (should (eq (button-get b 'help-function)
                    #'flamegraph--describe-frame))))))

;;; Heading rendering in `flamegraph--describe-frame'

(defun flamegraph-test--describe-help (node)
  "Render NODE in the help buffer via `flamegraph--describe-frame' and
return that buffer's contents (a propertized string)."
  (require 'help-mode)
  (flamegraph--describe-frame node (profiler-calltree-count node) nil)
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
