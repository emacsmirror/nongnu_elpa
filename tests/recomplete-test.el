;;; recomplete-test.el --- Recomplete test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-recomplete
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This is a test for `recomplete'.
;;

;;; Usage

;;
;; To test this file run:
;;
;;     `emacs -batch -l tests/recomplete-test.el -f ert-run-tests-batch-and-exit'
;;

;;; Code:

(require 'ert)

;; ---------------------------------------------------------------------------
;; Setup Environment

(setq recomplete-basedir (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path recomplete-basedir)
(add-to-list 'load-path (concat recomplete-basedir "/../with-command-redo"))
(require 'with-command-redo)
(require 'recomplete)

;; ---------------------------------------------------------------------------
;; Test Internal Replacement Function

(defmacro ert-replace-in-region (id str-src args output replaced-range)
  `(ert-deftest ,id ()
     "Generic test."
     (with-temp-buffer
       (insert ,str-src)
       (let ((result (apply 'recomplete-replace-in-region ,args)))
         (should (equal ,output (buffer-substring-no-properties (point-min) (point-max))))
         (should (equal result ,replaced-range))))))

(ert-replace-in-region replace-in-region-nop "" '("" 1 1) "" (cons 1 1))
(ert-replace-in-region replace-in-region-single-same "x" '("x" 1 2) "x" (cons 1 1))
(ert-replace-in-region replace-in-region-single-different "x" '("y" 1 2) "y" (cons 1 2))

(ert-replace-in-region replace-in-region-word-same "hello" '("hello" 1 6) "hello" (cons 1 1))
(ert-replace-in-region replace-in-region-word-same-start "HELLO" '("Hello" 1 6) "Hello" (cons 2 6))
(ert-replace-in-region replace-in-region-word-same-end "HELLO" '("hELLO" 1 6) "hELLO" (cons 1 2))
(ert-replace-in-region
 replace-in-region-word-different-1 "hello" '("world" 1 6) "world" (cons 1 6))
(ert-replace-in-region
 replace-in-region-word-different-2 "hello" '("HELLO" 1 6) "HELLO" (cons 1 6))
(ert-replace-in-region
 replace-in-region-word-contract "commpletion" '("completion" 1 12) "completion" (cons 3 3))
(ert-replace-in-region
 replace-in-region-word-expand "copletion" '("completion" 1 10) "completion" (cons 3 4))

(ert-replace-in-region
 replace-in-region-word-mix-same "pre hello post" '("hello" 5 10) "pre hello post" (cons 5 5))
(ert-replace-in-region
 replace-in-region-word-mix-same-start
 "pre HELLO post"
 '("Hello" 5 10)
 "pre Hello post"
 (cons 6 10))
(ert-replace-in-region
 replace-in-region-word-mix-same-end "pre HELLO post" '("hELLO" 5 10) "pre hELLO post" (cons 5 6))
(ert-replace-in-region
 replace-in-region-word-mix-different-1
 "pre hello post"
 '("world" 5 10)
 "pre world post"
 (cons 5 10))
(ert-replace-in-region
 replace-in-region-word-mix-different-2
 "pre hello post"
 '("HELLO" 5 10)
 "pre HELLO post"
 (cons 5 10))
(ert-replace-in-region
 replace-in-region-word-mix-contract
 "pre commpletion post"
 '("completion" 5 16)
 "pre completion post"
 (cons 7 7))
(ert-replace-in-region
 replace-in-region-word-mix-expand
 "pre copletion post"
 '("completion" 5 14)
 "pre completion post"
 (cons 7 8))

;; ---------------------------------------------------------------------------
;; Test Cycling Utilities

(defmacro simulate-input (keys)
  "Simulate keyboard input KEYS via `execute-kbd-macro'."
  (declare (indent 0))
  `(let ((minibuffer-message-timeout 0))
     (execute-kbd-macro ,keys)))

(defun buffer-reset-text (initial-buffer-text)
  "Use INITIAL-BUFFER-TEXT to initialize the buffer with text."
  (buffer-disable-undo)
  (erase-buffer)
  (save-excursion (insert initial-buffer-text))
  (buffer-enable-undo))

;; ---------------------------------------------------------------------------
;; Test Cycling

(defmacro with-recomplete-test (initial-buffer-text command &rest body)
  "Run BODY in a buffer with INITIAL-BUFFER-TEXT, binding M-p to COMMAND.
Messages are suppressed."
  (declare (indent 2))
  `(let ((buf (generate-new-buffer "recomplete-test")))
     (switch-to-buffer buf)
     (buffer-reset-text ,initial-buffer-text)
     (let ((inhibit-message t))
       (local-set-key (kbd "M-p") ,command)
       ,@body)
     (kill-buffer buf)))

(ert-deftest dabbrev-cycling-single-line-display-nil ()
  "Dabbrev cycling should work when `recomplete-single-line-display' is nil.
Without cycling, the second press tries to expand the already-expanded word
which has no further expansions, causing an error."
  (with-recomplete-test "unique_alpha unique_beta un" #'recomplete-dabbrev
    (goto-char (point-max))
    (let ((recomplete-single-line-display nil))
      ;; Press M-p twice to cycle to the second expansion.
      (simulate-input
        (kbd "M-p M-p"))
      ;; With cycling: "un" expands to "unique_beta", then cycles to "unique_alpha".
      ;; Without cycling (bug): "un" expands to "unique_beta", then a fresh
      ;; expand of "unique_beta" finds no new expansions and errors.
      (should
       (equal
        "unique_alpha unique_beta unique_alpha"
        (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest case-style-first-result ()
  "Pressing M-p once on a snake_case word should produce kebab-case."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "M-p"))
    (should (equal "hello-world" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-symbol-first-result ()
  "Pressing M-p on a kebab-case symbol in `emacs-lisp-mode' should produce PascalCase."
  (with-recomplete-test "hello-world" #'recomplete-case-style-symbol
    (emacs-lisp-mode)
    (local-set-key (kbd "M-p") #'recomplete-case-style-symbol)
    (simulate-input
      (kbd "M-p"))
    (should (equal "HelloWorld" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-reverse-direction ()
  "Pressing C-g between M-p presses should reverse the cycling direction."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    ;; Rebind C-g so it sets `this-command' to `keyboard-quit'
    ;; without actually signaling (which would abort the macro).
    (local-set-key
     (kbd "C-g")
     (lambda ()
       (interactive)
       (setq this-command 'keyboard-quit)))
    ;; M-p (forward to hello-world), C-g (reverse), M-p (backward to hello_world).
    ;; Without reversal, M-p M-p would give HelloWorld.
    (simulate-input
      (kbd "M-p C-g M-p"))
    (should (equal "hello_world" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-numeric-arg-forward ()
  "Numeric prefix 2 should jump two options ahead in one press.
Choices for \"hello_world\" rotate to (kebab pascal snake);
arg=2 should land on PascalCase."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "M-2 M-p"))
    (should (equal "HelloWorld" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-numeric-arg-negative ()
  "Negative prefix should cycle backward.
Choices for \"hello_world\" rotate to (kebab pascal snake) with the original
at index 2, so arg=-1 should land on PascalCase (index 1, the previous item
in the cycle), not on the original word at index 2."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "M-- M-p"))
    (should (equal "HelloWorld" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-numeric-arg-negative-sticky ()
  "A negative prefix should flip the chain into reverse so successive
default presses continue cycling backward (as if `keyboard-quit' was
pressed).  After M-- M-p (-> PascalCase), a default M-p should land on
kebab-case, not snake_case (the original)."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "M-- M-p M-p"))
    (should (equal "hello-world" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-numeric-arg-modulo ()
  "Numeric prefix exceeding the option count should wrap via modulo.
With 3 choices, arg=4 should be equivalent to arg=1 (kebab-case)."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "M-4 M-p"))
    (should (equal "hello-world" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest dabbrev-numeric-arg-forward ()
  "Numeric prefix 2 should expand to the second dabbrev candidate.
For \"unique_alpha unique_beta un\", \"un\" expands to (unique_beta unique_alpha);
arg=2 should land on unique_alpha."
  (with-recomplete-test "unique_alpha unique_beta un" #'recomplete-dabbrev
    (goto-char (point-max))
    (simulate-input
      (kbd "M-2 M-p"))
    (should
     (equal
      "unique_alpha unique_beta unique_alpha"
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest dabbrev-numeric-arg-negative ()
  "Negative prefix should cycle backward.
Dabbrev candidates do not include the original abbreviation, so arg=-1
enters the cycle from the end and lands on the last candidate.
For \"...a\" with candidates (almond avocado apple), arg=-1 -> index -1
-> mod 3 = 2 -> apple."
  (with-recomplete-test "apple banana avocado almond a" #'recomplete-dabbrev
    (goto-char (point-max))
    (simulate-input
      (kbd "M-- M-p"))
    (should
     (equal
      "apple banana avocado almond apple"
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest dabbrev-universal-arg-default ()
  "Bare `C-u' (arg=4) should wrap via modulo over a 3-candidate set.
Candidates for \"a\" in proximity order are (almond avocado apple);
arg=4 -> index 0 + 3 = 3 -> mod 3 = 0 -> almond."
  (with-recomplete-test "apple banana avocado almond a" #'recomplete-dabbrev
    (goto-char (point-max))
    (simulate-input
      (kbd "C-u M-p"))
    (should
     (equal
      "apple banana avocado almond almond"
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest dabbrev-universal-arg-explicit ()
  "`C-u 3' as a numeric prefix should land on the 3rd candidate.
arg=3 -> index 0 + 2 = 2 -> apple."
  (with-recomplete-test "apple banana avocado almond a" #'recomplete-dabbrev
    (goto-char (point-max))
    (simulate-input
      (kbd "C-u 3 M-p"))
    (should
     (equal
      "apple banana avocado almond apple"
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-universal-arg-explicit ()
  "`C-u 2' as a numeric prefix should jump two options ahead.
Choices for \"hello_world\" rotate to (kebab pascal snake);
arg=2 -> index 0 + 1 = 1 -> PascalCase."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "C-u 2 M-p"))
    (should (equal "HelloWorld" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-universal-arg-negative ()
  "`C-u - 2' (arg=-2) should cycle backward two steps in the cycle.
Choices for \"hello_world\" rotate to (kebab pascal snake) with the original
at index 2, so arg=-2 should land on kebab-case (index 0, two steps back
in the cycle from the original)."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "C-u - 2 M-p"))
    (should (equal "hello-world" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest dabbrev-numeric-arg-negative-sticky ()
  "Like `case-style-numeric-arg-negative-sticky' but for an impl with a
bookended cycle (no anchor).  After M-- M-p (-> last candidate
\"apple\"), a default M-p should continue backward to \"avocado\"."
  (with-recomplete-test "apple banana avocado almond a" #'recomplete-dabbrev
    (goto-char (point-max))
    (simulate-input
      (kbd "M-- M-p M-p"))
    (should
     (equal
      "apple banana avocado almond avocado"
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest case-style-numeric-arg-chained-fresh ()
  "Each press with an explicit numeric prefix is a fresh first call
because `digit-argument' (from M-N) breaks the cycle chain.  M-2 M-p
M-2 M-p on \"hello_world\" runs two independent forward-by-2 cycles;
the second starts fresh on PascalCase and lands on kebab-case."
  (with-recomplete-test "hello_world" #'recomplete-case-style
    (simulate-input
      (kbd "M-2 M-p M-2 M-p"))
    (should (equal "hello-world" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Test recomplete-with-callback (custom-impl integration)

(defun recomplete-test--stub-anchored-at-1 (cycle-index fn-cache)
  "Stub impl exercising `cycle-index-init' anchored at 1.
The choice list places the buffer's initial text \"current\" at index 0,
so callers pass CYCLE-INDEX-INIT=1 to anchor the cycle there - the
pattern used by `cycle-at-point' and similar external impls."
  (pcase-let ((`(,result-choices ,word-beg ,word-end) (or fn-cache '(nil nil nil))))
    (unless result-choices
      (setq word-beg 1)
      (setq word-end (1+ (length "current")))
      (setq result-choices '("current" "alpha" "beta" "gamma"))
      (setq fn-cache (list result-choices word-beg word-end)))
    (let ((word (nth (mod cycle-index (length result-choices)) result-choices)))
      (recomplete-replace-in-region word word-beg word-end))
    (list result-choices fn-cache)))

(ert-deftest with-callback-anchored-forward ()
  "An impl passing CYCLE-INDEX-INIT=1 anchors the cycle at index 1, so
arg=1 lands on \"alpha\" (index 1, skipping \"current\" at index 0)."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "current")
    (let ((inhibit-message t))
      (recomplete-with-callback 'recomplete-test--stub-anchored-at-1 1 1))
    (should (equal "alpha" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest with-callback-anchored-backward ()
  "An impl passing CYCLE-INDEX-INIT=1 anchors the cycle at index 1; arg=-1
lands on \"gamma\" - the previous item in the cycle from the anchor
\(index 1-2=-1 mod 4 = 3)."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "current")
    (let ((inhibit-message t))
      (recomplete-with-callback 'recomplete-test--stub-anchored-at-1 -1 1))
    (should (equal "gamma" (buffer-substring-no-properties (point-min) (point-max))))))


(provide 'recomplete-test)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; recomplete-test.el ends here
