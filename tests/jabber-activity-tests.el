;;; jabber-activity-tests.el --- ERT tests for jabber-activity  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub out dependencies so we can load jabber-activity in isolation.
(unless (featurep 'jabber-core)
  (provide 'jabber-core)
  (defvar *jabber-roster* nil)
  (defvar jabber-jid-obarray (make-vector 127 0)))

(unless (featurep 'jabber-util)
  (provide 'jabber-util)
  (defun jabber-jid-displayname (jid) jid)
  (defun jabber-jid-user (jid) jid)
  (defun jabber-jid-username (jid)
    (when (string-match "\\`\\([^@]+\\)@" jid)
      (match-string 1 jid)))
  (defun jabber-jid-resource (jid)
    (when (string-match "/\\(.+\\)\\'" jid)
      (match-string 1 jid))))

;; Stub MUC functions referenced by declare-function.
(unless (fboundp 'jabber-muc-sender-p)
  (defun jabber-muc-sender-p (_jid) nil))
(unless (fboundp 'jabber-muc-find-buffer)
  (defun jabber-muc-find-buffer (_group) nil))
(unless (fboundp 'jabber-chat-find-buffer)
  (defun jabber-chat-find-buffer (_jid) nil))
(unless (fboundp 'jabber-muc-private-find-buffer)
  (defun jabber-muc-private-find-buffer (_group _nick) nil))
(unless (fboundp 'jabber-muc-joined-p)
  (defun jabber-muc-joined-p (_group) nil))
(unless (fboundp 'jabber-muc-looks-like-personal-p)
  (defun jabber-muc-looks-like-personal-p (_msg &optional _group) nil))

(load (expand-file-name "../lisp/jabber-activity.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; Group 1: jabber-activity-common-prefix

(ert-deftest jabber-activity-test-common-prefix-basic ()
  "Common prefix of strings with shared start."
  (should (= 3 (jabber-activity-common-prefix "abcdef" "abcxyz"))))

(ert-deftest jabber-activity-test-common-prefix-empty ()
  "Common prefix when one or both strings are empty."
  (should (= 0 (jabber-activity-common-prefix "" "abc")))
  (should (= 0 (jabber-activity-common-prefix "abc" "")))
  (should (= 0 (jabber-activity-common-prefix "" ""))))

(ert-deftest jabber-activity-test-common-prefix-identical ()
  "Common prefix of identical strings."
  (should (= 5 (jabber-activity-common-prefix "hello" "hello"))))

(ert-deftest jabber-activity-test-common-prefix-no-match ()
  "Common prefix of strings with no shared start."
  (should (= 0 (jabber-activity-common-prefix "abc" "xyz"))))

(ert-deftest jabber-activity-test-common-prefix-substring ()
  "Common prefix when one string is a prefix of the other."
  (should (= 3 (jabber-activity-common-prefix "abc" "abcdef")))
  (should (= 3 (jabber-activity-common-prefix "abcdef" "abc"))))

;;; Group 2: jabber-activity-make-strings-shorten

(ert-deftest jabber-activity-test-shorten-unique ()
  "Shortened names should be unique."
  (let ((jabber-activity-make-string #'identity)
	(jabber-activity-shorten-minimum 1)
	(jabber-activity-shorten-aggressively nil)
	(jabber-activity--shortened-names (make-hash-table :test #'equal)))
    (let* ((result (jabber-activity-make-strings-shorten
		    '("alice" "alex" "bob")))
	   (names (mapcar #'cdr result)))
      (should (= (length names) (length (cl-remove-duplicates names :test #'string=)))))))

(ert-deftest jabber-activity-test-shorten-minimum-length ()
  "Shortened names respect jabber-activity-shorten-minimum."
  (let ((jabber-activity-make-string #'identity)
	(jabber-activity-shorten-minimum 3)
	(jabber-activity-shorten-aggressively nil)
	(jabber-activity--shortened-names (make-hash-table :test #'equal)))
    (let ((result (jabber-activity-make-strings-shorten
		   '("alice" "bob" "carol"))))
      (dolist (entry result)
	(should (>= (length (cdr entry)) 3))))))

(ert-deftest jabber-activity-test-shorten-aggressively ()
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

(ert-deftest jabber-activity-test-reentrance-guard ()
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

(ert-deftest jabber-activity-test-no-update-when-unchanged ()
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

(ert-deftest jabber-activity-test-update-when-changed ()
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

(ert-deftest jabber-activity-test-cutoff-overflow ()
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

(ert-deftest jabber-activity-test-no-cutoff ()
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

(ert-deftest jabber-activity-test-cache-invalidation ()
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

(provide 'jabber-activity-tests)

;;; jabber-activity-tests.el ends here
