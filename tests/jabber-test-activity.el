;;; jabber-test-activity.el --- Tests for jabber-activity  -*- lexical-binding: t; -*-

;;; Commentary:

;; Activity tracking and mode-line integration.

;;; Code:

;;; Code:

(require 'ert)
(require 'jabber-activity)

;; Declare dynamically-bound variables used in let-bindings below.
(defvar jabber-activity-jids)
(defvar jabber-activity-personal-jids)
(defvar jabber-activity-mode-string)
(defvar jabber-activity-count-string)
(defvar jabber-activity--updating)
(defvar jabber-activity-update-hook)
(defvar jabber-activity-make-string)
(defvar jabber-activity-make-strings)
(defvar jabber-activity-shorten-minimum)
(defvar jabber-activity-shorten-aggressively)
(defvar jabber-activity--shortened-names)
(defvar jabber-activity-shorten-cutoff)
(defvar jabber-activity-name-alist)
(defvar *jabber-roster*)

;; Declare functions loaded at runtime via `load'.
(declare-function jabber-activity-common-prefix "jabber-activity" (s1 s2))
(declare-function jabber-activity-make-strings-shorten "jabber-activity" (jids))
(declare-function jabber-activity-mode-line-update "jabber-activity" ())
(declare-function jabber-activity-make-name-alist "jabber-activity" ())

;;; Group 1: jabber-activity-common-prefix

(ert-deftest jabber-test-activity-common-prefix-basic ()
  "Common prefix of strings with shared start."
  (should (= 3 (jabber-activity-common-prefix "abcdef" "abcxyz"))))

(ert-deftest jabber-test-activity-common-prefix-empty ()
  "Common prefix when one or both strings are empty."
  (should (= 0 (jabber-activity-common-prefix "" "abc")))
  (should (= 0 (jabber-activity-common-prefix "abc" "")))
  (should (= 0 (jabber-activity-common-prefix "" ""))))

(ert-deftest jabber-test-activity-common-prefix-identical ()
  "Common prefix of identical strings."
  (should (= 5 (jabber-activity-common-prefix "hello" "hello"))))

(ert-deftest jabber-test-activity-common-prefix-no-match ()
  "Common prefix of strings with no shared start."
  (should (= 0 (jabber-activity-common-prefix "abc" "xyz"))))

(ert-deftest jabber-test-activity-common-prefix-substring ()
  "Common prefix when one string is a prefix of the other."
  (should (= 3 (jabber-activity-common-prefix "abc" "abcdef")))
  (should (= 3 (jabber-activity-common-prefix "abcdef" "abc"))))

;;; Group 2: jabber-activity-make-strings-shorten

(ert-deftest jabber-test-activity-shorten-unique ()
  "Shortened names should be unique."
  (let ((jabber-activity-make-string #'identity)
	(jabber-activity-shorten-minimum 1)
	(jabber-activity-shorten-aggressively nil)
	(jabber-activity--shortened-names (make-hash-table :test #'equal)))
    (let* ((result (jabber-activity-make-strings-shorten
		    '("alice" "alex" "bob")))
	   (names (mapcar #'cdr result)))
      (should (= (length names) (length (cl-remove-duplicates names :test #'string=)))))))

(ert-deftest jabber-test-activity-shorten-minimum-length ()
  "Shortened names respect jabber-activity-shorten-minimum."
  (let ((jabber-activity-make-string #'identity)
	(jabber-activity-shorten-minimum 3)
	(jabber-activity-shorten-aggressively nil)
	(jabber-activity--shortened-names (make-hash-table :test #'equal)))
    (let ((result (jabber-activity-make-strings-shorten
		   '("alice" "bob" "carol"))))
      (dolist (entry result)
	(should (>= (length (cdr entry)) 3))))))

(ert-deftest jabber-test-activity-shorten-aggressively ()
  "Aggressive shortening allows prefixes shorter than minimum."
  (let ((jabber-activity-make-string #'identity)
	(jabber-activity-shorten-minimum 5)
	(jabber-activity-shorten-aggressively t)
	(jabber-activity--shortened-names (make-hash-table :test #'equal)))
    (let* ((result (jabber-activity-make-strings-shorten
		    '("alice" "bob" "carol")))
	   (names (mapcar #'cdr result)))
      ;; With no shared prefixes and aggressive mode, names should be
      ;; shortened below the minimum of 5.
      (should (cl-some (lambda (n) (< (length n) 5)) names))
      ;; But still unique.
      (should (= (length names)
		 (length (cl-remove-duplicates names :test #'string=)))))))

;;; Group 3: re-entrance guard

(ert-deftest jabber-test-activity-reentrance-guard ()
  "Recursive calls to mode-line-update should be suppressed."
  (let ((jabber-activity-jids nil)
	(jabber-activity-personal-jids nil)
	(jabber-activity-mode-string "")
	(jabber-activity-count-string "0")
	(jabber-activity--updating nil)
	(jabber-activity-update-hook nil)
	(call-count 0))
    ;; Hook that tries to re-enter.
    (add-hook 'jabber-activity-update-hook
	      (lambda ()
		(cl-incf call-count)
		(jabber-activity-mode-line-update)))
    (unwind-protect
	(progn
	  (setq jabber-activity-jids '("test@example.com"))
	  (setq jabber-activity-name-alist
		'(("test@example.com" . "test")))
	  (jabber-activity-mode-line-update)
	  ;; Hook fires once; re-entrant call is blocked.
	  (should (= call-count 1)))
      (remove-hook 'jabber-activity-update-hook t))))

;;; Group 4: compare-before-update

(ert-deftest jabber-test-activity-no-update-when-unchanged ()
  "force-mode-line-update should not fire when strings are unchanged."
  (let ((jabber-activity-jids nil)
	(jabber-activity-personal-jids nil)
	(jabber-activity-mode-string "")
	(jabber-activity-count-string "0")
	(jabber-activity--updating nil)
	(jabber-activity-update-hook nil)
	(hook-called nil))
    (add-hook 'jabber-activity-update-hook (lambda () (setq hook-called t)))
    (unwind-protect
	(progn
	  ;; No JIDs, mode-string is already "", count is "0".
	  (jabber-activity-mode-line-update)
	  (should-not hook-called))
      (remove-hook 'jabber-activity-update-hook t))))

(ert-deftest jabber-test-activity-update-when-changed ()
  "Hook fires when the mode string changes."
  (let ((jabber-activity-jids nil)
	(jabber-activity-personal-jids nil)
	(jabber-activity-mode-string "old")
	(jabber-activity-count-string "1")
	(jabber-activity--updating nil)
	(jabber-activity-update-hook nil)
	(hook-called nil))
    (add-hook 'jabber-activity-update-hook (lambda () (setq hook-called t)))
    (unwind-protect
	(progn
	  ;; No JIDs, so new string will be "" which differs from "old".
	  (jabber-activity-mode-line-update)
	  (should hook-called))
      (remove-hook 'jabber-activity-update-hook t))))

;;; Group 5: cutoff truncation

(ert-deftest jabber-test-activity-cutoff-overflow ()
  "Mode string shows overflow indicator when exceeding cutoff."
  (let ((jabber-activity-shorten-cutoff 2)
	(jabber-activity-jids '("a@x" "b@x" "c@x" "d@x"))
	(jabber-activity-personal-jids nil)
	(jabber-activity-name-alist '(("a@x" . "A") ("b@x" . "B")
				      ("c@x" . "C") ("d@x" . "D")))
	(jabber-activity-mode-string "")
	(jabber-activity-count-string "0")
	(jabber-activity--updating nil)
	(jabber-activity-update-hook nil))
    (jabber-activity-mode-line-update)
    (should (string-match-p ", \\+2\\]\\'" jabber-activity-mode-string))
    (should (string= jabber-activity-count-string "4"))))

(ert-deftest jabber-test-activity-no-cutoff ()
  "No overflow indicator when cutoff is nil."
  (let ((jabber-activity-shorten-cutoff nil)
	(jabber-activity-jids '("a@x" "b@x" "c@x"))
	(jabber-activity-personal-jids nil)
	(jabber-activity-name-alist '(("a@x" . "A") ("b@x" . "B")
				      ("c@x" . "C")))
	(jabber-activity-mode-string "")
	(jabber-activity-count-string "0")
	(jabber-activity--updating nil)
	(jabber-activity-update-hook nil))
    (jabber-activity-mode-line-update)
    (should-not (string-match-p "\\+" jabber-activity-mode-string))))

;;; Group 6: cache invalidation

(ert-deftest jabber-test-activity-cache-invalidation ()
  "Cache is cleared when name alist is rebuilt."
  (let ((jabber-activity-make-strings #'jabber-activity-make-strings-shorten)
	(jabber-activity-make-string #'identity)
	(jabber-activity-shorten-minimum 1)
	(jabber-activity-shorten-aggressively nil)
	(jabber-activity--shortened-names (make-hash-table :test #'equal))
	(jabber-activity-name-alist '(("foo@bar" . "foo")))
	(*jabber-roster* nil))
    ;; Populate cache.
    (jabber-activity-make-strings-shorten '("foo@bar"))
    (should (= 1 (hash-table-count jabber-activity--shortened-names)))
    ;; Rebuild name alist should clear cache.
    (jabber-activity-make-name-alist)
    (should (= 0 (hash-table-count jabber-activity--shortened-names)))))

(provide 'jabber-test-activity)

;;; jabber-test-activity.el ends here
