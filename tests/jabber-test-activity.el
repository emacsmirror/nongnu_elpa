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
(defvar jabber-buffer-connection)
(defvar jabber-connections)
(defvar jabber-roster-list)

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
  "Do not call `force-mode-line-update' when strings are unchanged."
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
	(jabber-roster-list nil))
    ;; Populate cache.
    (jabber-activity-make-strings-shorten '("foo@bar"))
    (should (= 1 (hash-table-count jabber-activity--shortened-names)))
    ;; Rebuild name alist should clear cache.
    (jabber-activity-make-name-alist)
    (should (= 0 (hash-table-count jabber-activity--shortened-names)))))

;;; Group 7: missing buffer recovery

(ert-deftest jabber-test-activity-switch-to-recovers-missing-chat-buffer ()
  "Missing 1:1 activity buffers are recreated through chat setup."
  (let ((jid "friend@example.org")
        (jc 'test-connection)
        (jabber-connections '(test-connection))
        (jabber-buffer-connection nil)
        (jabber-activity-jids '("friend@example.org"))
        (jabber-activity-personal-jids nil)
        (created nil)
        (cleanup-called nil))
    (with-temp-buffer
      (let ((target (generate-new-buffer " *jabber-test-chat*")))
        (unwind-protect
            (cl-letf (((symbol-function 'jabber-activity-find-buffer-name)
                       (lambda (_jid) nil))
                      ((symbol-function 'jabber-chat-with)
                       (lambda (actual-jc actual-jid &optional _other-window)
                         (setq created (list actual-jc actual-jid))
                         (switch-to-buffer target)))
                      ((symbol-function 'fsm-get-state-data)
                       (lambda (_jc) nil))
                      ((symbol-function 'jabber-muc-sender-p)
                       (lambda (_jid) nil))
                      ((symbol-function 'jabber-muc-joined-p)
                       (lambda (_jid &optional _jc) nil))
                      ((symbol-function 'jabber-activity-clean)
                       (lambda () (setq cleanup-called t))))
              (jabber-activity-switch-to jid)
              (should (equal created (list jc jid)))
              (should cleanup-called)
              (should (equal jabber-activity-jids (list jid))))
          (when (buffer-live-p target)
            (kill-buffer target)))))))

(ert-deftest jabber-test-activity-switch-to-recovers-missing-muc-buffer ()
  "Missing active MUC activity buffers are recreated through MUC setup."
  (let ((jid "room@conference.example.org")
        (jabber-activity-jids '("room@conference.example.org"))
        (jabber-activity-personal-jids nil)
        (switched nil))
    (with-temp-buffer
      (let ((target (generate-new-buffer " *jabber-test-muc*")))
        (unwind-protect
            (cl-letf (((symbol-function 'jabber-activity-find-buffer-name)
                       (lambda (_jid) nil))
                      ((symbol-function 'jabber-muc-sender-p)
                       (lambda (_jid) nil))
                      ((symbol-function 'jabber-muc-joined-p)
                       (lambda (_jid &optional _jc) t))
                      ((symbol-function 'jabber-muc-switch-to)
                       (lambda (actual-jid)
                         (setq switched actual-jid)
                         (switch-to-buffer target)))
                      ((symbol-function 'jabber-activity-clean)
                       (lambda () nil)))
              (jabber-activity-switch-to jid)
              (should (string= switched jid))
              (should (equal jabber-activity-jids (list jid))))
          (when (buffer-live-p target)
            (kill-buffer target)))))))

(ert-deftest jabber-test-activity-switch-to-missing-buffer-without-connection-falls-back ()
  "Missing activity buffers still fall back when no connection can create one."
  (let ((jid "friend@example.org")
        (jabber-connections nil)
        (jabber-buffer-connection nil)
        (jabber-activity-jids '("friend@example.org"))
        (jabber-activity-personal-jids '("friend@example.org"))
        (message-text nil))
    (cl-letf (((symbol-function 'jabber-activity-find-buffer-name)
               (lambda (_jid) nil))
              ((symbol-function 'jabber-chat-with)
               (lambda (&rest _args)
                 (ert-fail "jabber-chat-with should not be called")))
              ((symbol-function 'jabber-muc-sender-p)
               (lambda (_jid) nil))
              ((symbol-function 'jabber-muc-joined-p)
               (lambda (_jid &optional _jc) nil))
              ((symbol-function 'jabber-activity-mode-line-update)
               (lambda () nil))
              ((symbol-function 'jabber-activity-clean)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-text (apply #'format format-string args)))))
      (jabber-activity-switch-to jid)
      (should-not jabber-activity-jids)
      (should-not jabber-activity-personal-jids)
      (should (string= message-text
                       "Buffer for friend@example.org no longer exists")))))

(provide 'jabber-test-activity)

;;; jabber-test-activity.el ends here
