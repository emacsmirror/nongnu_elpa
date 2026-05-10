;;; jabber-roster-tests.el --- Tests for jabber-roster  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables that jabber-muc.el expects at load time:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-roster)

;;; Group 1: jabber-roster-sort-by-status

(ert-deftest jabber-test-roster-sort-by-status-online-vs-away ()
  "Online user sorts before away user."
  (let ((jabber-sort-order '("chat" "" "away" "dnd" "xa"))
        (a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'show "")
    (put b 'show "away")
    (should (< (jabber-roster-sort-by-status a b) 0))))

(ert-deftest jabber-test-roster-sort-by-status-same ()
  "Same status returns 0."
  (let ((jabber-sort-order '("chat" "" "away" "dnd" "xa"))
        (a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'show "away")
    (put b 'show "away")
    (should (= (jabber-roster-sort-by-status a b) 0))))

(ert-deftest jabber-test-roster-sort-by-status-offline-last ()
  "Offline (nil show) sorts after online."
  (let ((jabber-sort-order '("chat" "" "away" "dnd" "xa"))
        (a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'show nil)
    (put b 'show "")
    (should (> (jabber-roster-sort-by-status a b) 0))))

;;; Group 2: jabber-roster-sort-by-displayname

(ert-deftest jabber-test-roster-sort-by-displayname-order ()
  "Alphabetical ordering by display name."
  (let ((jabber-jid-obarray (make-vector 127 0))
        (a (intern "alice@example.com" (make-vector 127 0)))
        (b (intern "bob@example.com" (make-vector 127 0))))
    (put a 'name "Alice")
    (put b 'name "Bob")
    (should (< (jabber-roster-sort-by-displayname a b) 0))))

(ert-deftest jabber-test-roster-sort-by-displayname-equal ()
  "Same name returns 0."
  (let ((jabber-jid-obarray (make-vector 127 0))
        (a (intern "alice@example.com" (make-vector 127 0)))
        (b (intern "alice2@example.com" (make-vector 127 0))))
    (put a 'name "Alice")
    (put b 'name "Alice")
    (should (= (jabber-roster-sort-by-displayname a b) 0))))

;;; Group 3: jabber-roster-sort-by-group

(ert-deftest jabber-test-roster-sort-by-group-different ()
  "Different groups sort alphabetically."
  (let ((a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'groups '("Friends"))
    (put b 'groups '("Work"))
    (should (< (jabber-roster-sort-by-group a b) 0))))

(ert-deftest jabber-test-roster-sort-by-group-same ()
  "Same group returns 0."
  (let ((a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'groups '("Friends"))
    (put b 'groups '("Friends"))
    (should (= (jabber-roster-sort-by-group a b) 0))))

(ert-deftest jabber-test-roster-sort-by-group-no-group ()
  "No group falls back to empty string."
  (let ((a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'groups nil)
    (put b 'groups '("Work"))
    (should (< (jabber-roster-sort-by-group a b) 0))))

;;; Group 4: jabber-fix-status

(ert-deftest jabber-test-fix-status-trailing-newlines ()
  "Trailing newlines are removed."
  (let ((jabber-remove-newlines nil))
    (should (string= (jabber-fix-status "Hello\n\n") "Hello"))))

(ert-deftest jabber-test-fix-status-internal-newlines-removed ()
  "Internal newlines removed when jabber-remove-newlines is t."
  (let ((jabber-remove-newlines t))
    (should (string= (jabber-fix-status "line1\nline2") "line1 line2"))))

(ert-deftest jabber-test-fix-status-internal-newlines-kept ()
  "Internal newlines kept when jabber-remove-newlines is nil."
  (let ((jabber-remove-newlines nil))
    (should (string= (jabber-fix-status "line1\nline2") "line1\nline2"))))

(ert-deftest jabber-test-fix-status-nil ()
  "Nil input returns nil."
  (should (null (jabber-fix-status nil))))

;;; Group 5: Face definitions

(ert-deftest jabber-test-roster-faces-use-inherit ()
  "Modernized roster faces use :inherit."
  (dolist (face-spec '((jabber-roster-user-online . success)
                       (jabber-roster-user-away . warning)
                       (jabber-roster-user-xa . shadow)
                       (jabber-roster-user-dnd . error)
                       (jabber-roster-user-error . error)
                       (jabber-roster-user-offline . shadow)
                       (jabber-roster-groupchat . font-lock-type-face)
                       (jabber-roster-groupchat-nick . shadow)
                       (jabber-roster-unread . font-lock-warning-face)))
    (let* ((face (car face-spec))
           (expected-parent (cdr face-spec))
           (spec (face-default-spec face)))
      (should (facep face))
      (when expected-parent
        (let* ((attrs (cdar spec))
               (inherit (plist-get attrs :inherit)))
          (should (eq inherit expected-parent)))))))

(provide 'jabber-roster-tests)
;;; jabber-roster-tests.el ends here
