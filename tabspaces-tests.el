;;; tabspaces-tests.el --- Tests for tabspaces  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Colin McLear

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Regression tests for tabspaces.  Run in batch with:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs -Q --batch -L . -l tabspaces.el -l tabspaces-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The suite targets pure helpers and the session save/load path, which
;; do not need a graphical frame.  Tests that touch tab-bar state
;; initialize it explicitly so they also pass under --batch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tabspaces)

(defmacro tabspaces-tests--with-session-file (var content &rest body)
  "Write CONTENT to a temp file bound to VAR, run BODY, delete the file."
  (declare (indent 2))
  `(let ((,var (make-temp-file "tabspaces-test-session" nil ".el")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,content))
           ,@body)
       (delete-file ,var))))

;;;; Session file round-trip

(ert-deftest tabspaces-test-session-roundtrip ()
  "The writer's output loads back into identical data."
  (let ((file (make-temp-file "tabspaces-test-session" nil ".el"))
        (tabspaces-project-tab-map '(("/tmp/projA/" . "projA")))
        (list '((("/tmp/a.txt" (:kind eshell :dir "/tmp/" :name "*eshell*"))
                 "Tab1" nil))))
    (unwind-protect
        (progn
          (tabspaces--write-session-file file list)
          (let ((tabspaces-project-tab-map nil)
                (tabspaces--session-list nil))
            (tabspaces--load-session-file file)
            (should (equal tabspaces-project-tab-map
                           '(("/tmp/projA/" . "projA"))))
            (should (equal tabspaces--session-list list))))
      (delete-file file))))

(ert-deftest tabspaces-test-write-session-file-note ()
  "The optional NOTE lands in the file as a comment."
  (let ((file (make-temp-file "tabspaces-test-session" nil ".el"))
        (tabspaces-project-tab-map nil))
    (unwind-protect
        (progn
          (tabspaces--write-session-file file nil "Non-project tabs only")
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward ";; Non-project tabs only" nil t))))
      (delete-file file))))

;;;; Safe session loading

(ert-deftest tabspaces-test-load-ignores-code ()
  "Non-conforming forms are ignored, never evaluated."
  (defvar tabspaces-tests--evil nil)
  (tabspaces-tests--with-session-file file
      (concat
       ;; Unquoted value: must not be evaluated or applied.
       "(setq tabspaces--session-list (progn (setq tabspaces-tests--evil t) nil))\n"
       ;; Arbitrary call: must not run.
       "(setq tabspaces-tests--evil t)\n"
       ;; Conforming form after malicious ones: must still apply.
       "(setq tabspaces-project-tab-map '((\"/ok\" . \"ok\")))\n")
    (let ((tabspaces-project-tab-map nil)
          (tabspaces--session-list 'untouched)
          (tabspaces-tests--evil nil))
      (tabspaces--load-session-file file)
      (should-not tabspaces-tests--evil)
      (should (eq tabspaces--session-list 'untouched))
      (should (equal tabspaces-project-tab-map '(("/ok" . "ok")))))))

(ert-deftest tabspaces-test-load-only-known-variables ()
  "A conforming setq shape targeting any other variable is ignored."
  (defvar tabspaces-tests--other 'untouched)
  (tabspaces-tests--with-session-file file
      "(setq tabspaces-tests--other '(1 2 3))\n"
    (let ((tabspaces-tests--other 'untouched))
      (tabspaces--load-session-file file)
      (should (eq tabspaces-tests--other 'untouched)))))

(ert-deftest tabspaces-test-load-rejects-circular ()
  "Circular #N= reader syntax is rejected instead of read.
A circular structure bound to the session variables would hang or
crash later traversals such as `tabspaces--rewrite-window-state'."
  (tabspaces-tests--with-session-file file
      "(setq tabspaces-project-tab-map '#1=((\"/a\" . \"a\") . #1#))\n"
    (let ((tabspaces-project-tab-map 'untouched))
      (tabspaces--load-session-file file)
      (should (eq tabspaces-project-tab-map 'untouched)))))

(ert-deftest tabspaces-test-load-tolerates-truncated-file ()
  "A truncated file applies earlier valid forms and does not signal."
  (tabspaces-tests--with-session-file file
      (concat "(setq tabspaces-project-tab-map '((\"/ok\" . \"ok\")))\n"
              "(setq tabspaces--session-list '((")
    (let ((tabspaces-project-tab-map nil)
          (tabspaces--session-list 'untouched))
      (tabspaces--load-session-file file)
      (should (equal tabspaces-project-tab-map '(("/ok" . "ok"))))
      (should (eq tabspaces--session-list 'untouched)))))

;;;; Project tab map

(ert-deftest tabspaces-test-remember-project-tab-replaces ()
  "Re-recording a project replaces its entry instead of duplicating.
Regression test for the assq-delete-all bug: `assq-delete-all'
compares string keys with `eq' and silently kept the old entry."
  (let ((tabspaces-project-tab-map nil))
    (tabspaces--remember-project-tab "/path/a" "OldName")
    (tabspaces--remember-project-tab "/path/b" "B")
    (tabspaces--remember-project-tab "/path/a" "NewName")
    (should (= (length tabspaces-project-tab-map) 2))
    (should (equal (cdr (assoc "/path/a" tabspaces-project-tab-map))
                   "NewName"))
    (should (equal (cdr (assoc "/path/b" tabspaces-project-tab-map)) "B"))))

(ert-deftest tabspaces-test-get-project-for-tab ()
  "Lookup handles exact names and numbered <N> duplicates."
  (let ((tabspaces-project-tab-map '(("/path/a" . "projA"))))
    (should (equal (tabspaces--get-project-for-tab "projA") "/path/a"))
    (should (equal (tabspaces--get-project-for-tab "projA<2>") "/path/a"))
    (should-not (tabspaces--get-project-for-tab "other"))))

(ert-deftest tabspaces-test-unique-numbered-tab-name ()
  "Numbered suffixes skip names already in use."
  (should (equal (tabspaces--generate-unique-numbered-tab-name "proj" nil)
                 "proj"))
  (should (equal (tabspaces--generate-unique-numbered-tab-name
                  "proj" '("proj"))
                 "proj<2>"))
  (should (equal (tabspaces--generate-unique-numbered-tab-name
                  "proj" '("proj" "proj<2>"))
                 "proj<3>")))

;;;; Echo area

(ert-deftest tabspaces-test-echo-area-cleanup-guard ()
  "Cleanup restores `tab-bar-show' only when setup actually ran.
Regression test: cleanup used to set `tab-bar-show' unconditionally,
hiding the tab bar when the echo-area feature was never enabled."
  ;; Inactive: cleanup must not touch tab-bar-show.
  (let ((tab-bar-show t)
        (tabspaces--echo-area-active nil))
    (tabspaces--echo-area-cleanup)
    (should (eq tab-bar-show t)))
  ;; Active: cleanup restores the stored original value.
  (let ((tab-bar-show nil)
        (tabspaces--echo-area-active t)
        (tabspaces--original-tab-bar-show 'original))
    (tabspaces--echo-area-cleanup)
    (should (eq tab-bar-show 'original))
    (should-not tabspaces--echo-area-active)))

;;;; Tab naming

(ert-deftest tabspaces-test-name-tab-is-pure ()
  "The naming function returns a string and performs no tab switch.
Regression test: it used to call `tab-bar-switch-to-tab' when the
current tab name matched the project name, which is unsafe inside
`tab-bar-tab-name-function'."
  (tab-bar-tabs)                        ; initialize tab-bar state
  (let ((switched nil))
    (cl-letf (((symbol-function 'tabspaces--project-name)
               (lambda () "projX"))
              ((symbol-function 'tab-bar-switch-to-tab)
               (lambda (&rest _) (setq switched t))))
      ;; Non-matching current tab name.
      (should (equal (tabspaces--name-tab-by-project-or-default) "projX"))
      ;; Matching current tab name: same string, still no switch.
      (cl-letf (((symbol-function 'tab-bar-tab-name-current)
                 (lambda () "projX")))
        (should (equal (tabspaces--name-tab-by-project-or-default) "projX")))
      (should-not switched))
    ;; No project: falls back to the counted default name.
    (cl-letf (((symbol-function 'tabspaces--project-name)
               (lambda () "-")))
      (should (stringp (tabspaces--name-tab-by-project-or-default))))))

;;;; Buffer removal

(ert-deftest tabspaces-test-remove-buffer-clears-frame-lists ()
  "Removal drops the buffer from both frame buffer lists.
Regression test: the old code discarded the result of `delete' on a
stale local reference and never touched `buried-buffer-list'."
  (tab-bar-tabs)
  (let ((buf (generate-new-buffer "tabspaces-test-target"))
        (tabspaces-remove-to-default nil))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'buffer-list
                               (cons buf (frame-parameter nil 'buffer-list)))
          (set-frame-parameter nil 'buried-buffer-list (list buf))
          (tabspaces-remove-buffer buf)
          (should-not (memq buf (frame-parameter nil 'buffer-list)))
          (should-not (memq buf (frame-parameter nil 'buried-buffer-list))))
      (kill-buffer buf))))

;;;; Session restore helpers

(ert-deftest tabspaces-test-restore-record-unknown-kind ()
  "Unknown kinds and malformed records are collected, not fatal."
  (let ((tabspaces--restore-unknown-kinds nil))
    (should-not (tabspaces--restore-buffer-record
                 '(:kind tabspaces-tests--no-such-kind :dir "/tmp/")))
    (should (equal tabspaces--restore-unknown-kinds
                   '(tabspaces-tests--no-such-kind)))
    (should-not (tabspaces--restore-buffer-record '(1 2 3)))
    (should (memq 'malformed-record tabspaces--restore-unknown-kinds))))

(ert-deftest tabspaces-test-store-buffers-skips-unhandled ()
  "File buffers serialize to paths; plain temp buffers are skipped."
  (let* ((file (make-temp-file "tabspaces-test-file"))
         (fbuf (find-file-noselect file))
         (tbuf (generate-new-buffer "tabspaces-test-plain")))
    (unwind-protect
        (should (equal (tabspaces--store-buffers (list fbuf tbuf))
                       (list (buffer-file-name fbuf))))
      (kill-buffer fbuf)
      (kill-buffer tbuf)
      (delete-file file))))

(ert-deftest tabspaces-test-rewrite-window-state ()
  "Buffer-name substitution covers all three reference shapes."
  (let ((state '(leaf (buffer "old" (point . 1))
                      (next-buffers "old" "keep")
                      (prev-buffers ("old" 5 9) ("keep" 2 2))))
        (subst '(("old" . "new"))))
    (should (equal (tabspaces--rewrite-window-state state subst)
                   '(leaf (buffer "new" (point . 1))
                          (next-buffers "new" "keep")
                          ;; Substituted prev-buffers entries reset marker
                          ;; positions to 1 (see the function's docstring).
                          (prev-buffers ("new" 1 1) ("keep" 2 2)))))
    ;; nil subst returns state unchanged.
    (should (eq (tabspaces--rewrite-window-state state nil) state))))

(ert-deftest tabspaces-test-report-dupes ()
  "Duplicate detection returns each duplicated element once."
  (should (equal (tabspaces--report-dupes '("a" "b" "a" "c" "a" "b"))
                 '("b" "a")))
  (should-not (tabspaces--report-dupes '("a" "b" "c"))))

(provide 'tabspaces-tests)
;;; tabspaces-tests.el ends here
