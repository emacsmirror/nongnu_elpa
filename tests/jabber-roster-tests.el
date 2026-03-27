;;; jabber-roster-tests.el --- Tests for jabber-roster  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables that jabber-muc.el expects at load time:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-roster)
(require 'jabber-muc)

(defmacro jabber-muc-test-with-rooms (rooms &rest body)
  "Run BODY with ROOMS as active groupchats.
ROOMS is an alist of (group . nickname).  Each room gets a single
entry with JC=nil."
  (declare (indent 1))
  `(let ((jabber-muc--rooms (make-hash-table :test #'equal)))
     (dolist (r ,rooms)
       (puthash (car r) (list (cons nil (cdr r))) jabber-muc--rooms))
     ,@body))

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

;;; Group 5: jabber-roster-filter-display

(ert-deftest jabber-test-roster-filter-show-offline ()
  "All buddies shown when jabber-show-offline-contacts is t."
  (let ((jabber-show-offline-contacts t)
        (a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'connected t)
    (put b 'connected nil)
    (should (= (length (jabber-roster-filter-display (list a b))) 2))))

(ert-deftest jabber-test-roster-filter-hide-offline ()
  "Only connected buddies shown when jabber-show-offline-contacts is nil."
  (let ((jabber-show-offline-contacts nil)
        (a (make-symbol "alice@example.com"))
        (b (make-symbol "bob@example.com")))
    (put a 'connected t)
    (put b 'connected nil)
    (should (= (length (jabber-roster-filter-display (list a b))) 1))
    (should (eq (car (jabber-roster-filter-display (list a b))) a))))

(ert-deftest jabber-test-roster-filter-empty-list ()
  "Empty input returns empty list."
  (let ((jabber-show-offline-contacts t))
    (should (null (jabber-roster-filter-display nil)))))

;;; Group 6: jabber-roster-separator

(ert-deftest jabber-test-roster-separator-has-face ()
  "Separator string has jabber-separator face."
  (let ((sep (jabber-roster-separator)))
    (should (eq (get-text-property 0 'face sep) 'jabber-separator))))

(ert-deftest jabber-test-roster-separator-is-intangible ()
  "Separator string has cursor-intangible property."
  (let ((sep (jabber-roster-separator)))
    (should (get-text-property 0 'cursor-intangible sep))))

(ert-deftest jabber-test-roster-separator-nonempty ()
  "Separator string is at least 1 character."
  (let ((sep (jabber-roster-separator)))
    (should (>= (length sep) 1))))

;;; Group 7: jabber-roster-mode

(ert-deftest jabber-test-roster-mode-derived-from-special ()
  "jabber-roster-mode derives from special-mode."
  (with-temp-buffer
    (jabber-roster-mode)
    (should (derived-mode-p 'special-mode))))

(ert-deftest jabber-test-roster-mode-read-only ()
  "Roster buffer is read-only."
  (with-temp-buffer
    (jabber-roster-mode)
    (should buffer-read-only)))

(ert-deftest jabber-test-roster-mode-no-line-numbers ()
  "Line numbers are disabled in roster mode."
  (with-temp-buffer
    (jabber-roster-mode)
    (should (null display-line-numbers))))

(ert-deftest jabber-test-roster-mode-margins ()
  "Left margin is set."
  (with-temp-buffer
    (jabber-roster-mode)
    (should (= left-margin-width 1))))

(ert-deftest jabber-test-roster-mode-no-fringes ()
  "Fringes are disabled."
  (with-temp-buffer
    (jabber-roster-mode)
    (should (= left-fringe-width 0))
    (should (= right-fringe-width 0))))

(ert-deftest jabber-test-roster-mode-imenu ()
  "imenu index function is set."
  (with-temp-buffer
    (jabber-roster-mode)
    (should (eq imenu-create-index-function
                #'jabber-roster-imenu-create-index))))

;;; Group 8: jabber-roster-mode keymap

(ert-deftest jabber-test-roster-keymap-delete-bindings ()
  "d, D, and C-k all bind to delete."
  (should (eq (lookup-key jabber-roster-mode-map "d")
              #'jabber-roster-delete-at-point))
  (should (eq (lookup-key jabber-roster-mode-map "D")
              #'jabber-roster-delete-at-point))
  (should (eq (lookup-key jabber-roster-mode-map (kbd "C-k"))
              #'jabber-roster-delete-at-point)))

(ert-deftest jabber-test-roster-keymap-help-bindings ()
  "h, H, and ? all bind to jabber-roster-menu."
  (should (eq (lookup-key jabber-roster-mode-map "h")
              #'jabber-roster-menu))
  (should (eq (lookup-key jabber-roster-mode-map "H")
              #'jabber-roster-menu))
  (should (eq (lookup-key jabber-roster-mode-map "?")
              #'jabber-roster-menu)))

(ert-deftest jabber-test-roster-keymap-inherits-common ()
  "Roster keymap inherits C-c C-c from jabber-common-keymap."
  (should (eq (lookup-key jabber-roster-mode-map (kbd "C-c C-c"))
              #'jabber-chat-menu)))

;;; Group 9: Face definitions

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

;;; Group 10: jabber-roster-imenu-create-index

(ert-deftest jabber-test-roster-imenu-contacts ()
  "Imenu indexes contact groups and JIDs."
  (jabber-muc-test-with-rooms nil
    (with-temp-buffer
      (let ((line1 "Friends")
            (line2 "alice@example.com"))
        (insert (propertize line1 'jabber-group "Friends") "\n")
        (insert (propertize line2 'jabber-jid "alice@example.com") "\n"))
      (let ((index (jabber-roster-imenu-create-index)))
        (should (assoc "Contacts" index))
        (let ((contacts (cdr (assoc "Contacts" index))))
          (should (assoc "Friends" contacts))
          (should (assoc "alice@example.com" contacts)))))))

(ert-deftest jabber-test-roster-imenu-groupchats ()
  "Imenu indexes groupchat JIDs under Groupchats."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (with-temp-buffer
      (insert (propertize "Groupchats" 'jabber-group "Groupchats") "\n")
      (insert (propertize "room@conference.example.com"
                          'jabber-jid "room@conference.example.com")
              "\n")
      (let ((index (jabber-roster-imenu-create-index)))
        (should (assoc "Groupchats" index))
        (let ((gcs (cdr (assoc "Groupchats" index))))
          (should (assoc "Groupchats" gcs))
          (should (assoc "room@conference.example.com" gcs)))))))

(ert-deftest jabber-test-roster-imenu-empty-buffer ()
  "Empty buffer returns nil index."
  (jabber-muc-test-with-rooms nil
    (with-temp-buffer
      (should (null (jabber-roster-imenu-create-index))))))

;;; Group 11: Deferred refresh

(ert-deftest jabber-test-roster-needs-refresh-default-nil ()
  "Deferred refresh flag starts as nil."
  (should (null jabber-roster--needs-refresh)))

(ert-deftest jabber-test-roster-last-muc-generation-default-zero ()
  "MUC generation counter starts at zero."
  (should (= jabber-roster--last-muc-generation 0)))

(provide 'jabber-roster-tests)
;;; jabber-roster-tests.el ends here
