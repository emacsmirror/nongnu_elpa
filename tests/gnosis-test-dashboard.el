;;; gnosis-test-dashboard.el --- Dashboard tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for gnosis-dashboard.el functionality.
;; Uses a temporary SQLite database so the user's real DB is untouched.

;;; Code:
(require 'ert)
(require 'gnosis)
(require 'gnosis-dashboard)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

;; ──────────────────────────────────────────────────────────
;; Test helpers
;; ──────────────────────────────────────────────────────────

(defvar gnosis-test--db-file nil
  "Path to temporary test database file.")

(defmacro gnosis-test-with-db (&rest body)
  "Run BODY with a fresh temporary gnosis database.
Rebinds `gnosis-db' and initialises the schema."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-dash-" nil ".db"))
          (gnosis-db (emacsql-sqlite-open gnosis-test--db-file))
          (gnosis--id-cache nil))
     (unwind-protect
         (progn
           (emacsql-with-transaction gnosis-db
             (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
               (emacsql gnosis-db [:create-table $i1 $S2] table schema)))
           ,@body)
       (emacsql-close gnosis-db)
       (delete-file gnosis-test--db-file))))

(defmacro gnosis-test-with-dashboard-buffer (&rest body)
  "Run BODY in a temporary dashboard buffer with tabulated-list-mode.
Stubs `pop-to-buffer-same-window' so tests work in batch mode."
  (declare (indent 0) (debug t))
  `(let ((gnosis-dashboard-buffer-name "*Gnosis Dashboard Test*"))
     (get-buffer-create gnosis-dashboard-buffer-name)
     (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                (lambda (buf &rest _) (set-buffer (get-buffer-create buf)))))
       (unwind-protect
           (progn ,@body)
         (when (get-buffer gnosis-dashboard-buffer-name)
           (kill-buffer gnosis-dashboard-buffer-name))))))

(defun gnosis-test--add-deck (name)
  "Add a deck with NAME to the test DB.  Return its id."
  (let ((id (+ (random 90000) 10000)))
    (gnosis--insert-into 'decks `([,id ,name]))
    id))

(defun gnosis-test--add-basic-thema (deck-id keimenon answer
                                     &optional tags parathema thema-id suspend)
  "Insert a basic thema into the test DB.  Return its id.
DECK-ID, KEIMENON, ANSWER are required.
TAGS defaults to (\"test\"), PARATHEMA to \"\".
SUSPEND: 1 to suspend, 0 or nil for active."
  (let* ((id (or thema-id (gnosis-generate-id)))
         (tags (or tags '("test")))
         (parathema (or parathema ""))
         (suspend (or suspend 0))
         (hypothesis '(""))
         (answer (if (listp answer) answer (list answer))))
    (emacsql-with-transaction gnosis-db
      (gnosis--insert-into 'themata `([,id "basic" ,keimenon ,hypothesis
                                           ,answer ,tags ,deck-id]))
      (gnosis--insert-into 'review `([,id ,gnosis-algorithm-gnosis-value
                                          ,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,id ,(gnosis-algorithm-date)
                                              ,(gnosis-algorithm-date) 0 0 0 0
                                              ,suspend 0]))
      (gnosis--insert-into 'extras `([,id ,parathema ""])))
    id))

(defun gnosis-test--add-activity (date total new)
  "Insert an activity-log row for DATE with TOTAL and NEW counts."
  (gnosis--insert-into 'activity-log `([,date ,total ,new])))

(defun gnosis-test--add-link (thema-id node-id)
  "Insert a link from THEMA-ID to NODE-ID string."
  (gnosis--insert-into 'links `([,thema-id ,node-id])))

(defun gnosis-test--setup-tags ()
  "Populate tags table from existing themata in test DB."
  (let ((tags (gnosis-get-tags--unique)))
    (emacsql-with-transaction gnosis-db
      (cl-loop for tag in tags
               do (condition-case nil
                      (gnosis--insert-into 'tags `[,tag])
                    (error nil))))))

;; ──────────────────────────────────────────────────────────
;; Pure function tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-streak-empty ()
  "Streak with no dates returns \"0\"."
  (should (equal (gnosis-dashboard--streak nil) "0")))

(ert-deftest gnosis-test-dashboard-streak-yesterday ()
  "Streak with only yesterday returns \"1\"."
  (let ((dates (list (gnosis-algorithm-date -1))))
    (should (equal (gnosis-dashboard--streak dates) "1"))))

(ert-deftest gnosis-test-dashboard-streak-consecutive ()
  "Streak with consecutive past days counts correctly."
  (let ((dates (list (gnosis-algorithm-date -1)
                     (gnosis-algorithm-date -2)
                     (gnosis-algorithm-date -3))))
    (should (equal (gnosis-dashboard--streak dates) "3"))))

(ert-deftest gnosis-test-dashboard-streak-today-only ()
  "Streak with only today returns \"1\"."
  (let ((dates (list (gnosis-algorithm-date))))
    (should (equal (gnosis-dashboard--streak dates) "1"))))

(ert-deftest gnosis-test-dashboard-streak-today-plus-consecutive ()
  "Streak includes today bonus on top of consecutive past days."
  (let ((dates (list (gnosis-algorithm-date)
                     (gnosis-algorithm-date -1)
                     (gnosis-algorithm-date -2))))
    (should (equal (gnosis-dashboard--streak dates) "3"))))

(ert-deftest gnosis-test-dashboard-streak-gap ()
  "A gap in dates stops the streak."
  ;; Yesterday and 3-days-ago, but NOT 2-days-ago
  (let ((dates (list (gnosis-algorithm-date -1)
                     (gnosis-algorithm-date -3))))
    (should (equal (gnosis-dashboard--streak dates) "1"))))

(ert-deftest gnosis-test-dashboard-sort-total-themata ()
  "Sort entries by column index 1 (total themata count)."
  (let ((entry-small '("tag-a" ["tag-a" "5"]))
        (entry-large '("tag-b" ["tag-b" "20"])))
    (should (gnosis-dashboard-sort-total-themata entry-small entry-large))
    (should-not (gnosis-dashboard-sort-total-themata entry-large entry-small))))

;; ──────────────────────────────────────────────────────────
;; Data-formatting function tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-output-themata-basic ()
  "Output-themata returns correctly formatted entries."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "What is 2+2?" "4"
                                              '("math")))
           (id2 (gnosis-test--add-basic-thema deck-id "Capital?" "Athens"
                                              '("geo")))
           (entries (gnosis-dashboard--output-themata (list id1 id2))))
      ;; Two entries returned
      (should (= (length entries) 2))
      ;; Each entry is (id vector)
      (let* ((e1 (cl-find id1 entries :key #'car))
             (vec1 (cadr e1)))
        (should e1)
        ;; Keimenon is first field
        (should (string-search "What is 2+2?" (aref vec1 0)))
        ;; Type is "basic"
        (should (equal (aref vec1 4) "basic"))
        ;; Not suspended → "No"
        (should (equal (aref vec1 5) "No"))))))

(ert-deftest gnosis-test-dashboard-output-themata-suspended ()
  "Suspended themata show \"Yes\" in suspend column."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q?" "A" '("t") nil nil 1))
           (entries (gnosis-dashboard--output-themata (list id1)))
           (e1 (cl-find id1 entries :key #'car))
           (vec1 (cadr e1)))
      (should (equal (aref vec1 5) "Yes")))))

(ert-deftest gnosis-test-dashboard-output-themata-list-tags ()
  "List-valued tags are joined with commas."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q?" "A"
                                              '("math" "algebra")))
           (entries (gnosis-dashboard--output-themata (list id1)))
           (vec (cadr (car entries))))
      ;; Tags field (index 3) should contain both tags
      (should (string-search "math" (aref vec 3)))
      (should (string-search "algebra" (aref vec 3))))))

(ert-deftest gnosis-test-dashboard-output-themata-strips-org-links ()
  "Org-mode links in keimenon are simplified to description only."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema
                 deck-id "See [[id:abc-123][My Node]] for details" "A"))
           (entries (gnosis-dashboard--output-themata (list id1)))
           (vec (cadr (car entries))))
      ;; Link syntax removed, description kept
      (should (string-search "My Node" (aref vec 0)))
      (should-not (string-search "[[id:" (aref vec 0))))))

(ert-deftest gnosis-test-dashboard-output-themata-strips-newlines ()
  "Newlines in fields are replaced with spaces."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Line1\nLine2" "A"))
           (entries (gnosis-dashboard--output-themata (list id1)))
           (vec (cadr (car entries))))
      (should-not (string-search "\n" (aref vec 0)))
      (should (string-search "Line1 Line2" (aref vec 0))))))

(ert-deftest gnosis-test-dashboard-deck-thema-count ()
  "Deck thema count returns correct count string."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck")))
      ;; Empty deck
      (should (equal (gnosis-dashboard-deck-thema-count deck-id) '("0")))
      ;; Add two themata
      (gnosis-test--add-basic-thema deck-id "Q1" "A1")
      (gnosis-test--add-basic-thema deck-id "Q2" "A2")
      (should (equal (gnosis-dashboard-deck-thema-count deck-id) '("2"))))))

(ert-deftest gnosis-test-dashboard-deck-thema-count-nonexistent ()
  "Deck thema count returns nil for nonexistent deck."
  (gnosis-test-with-db
    (should (null (gnosis-dashboard-deck-thema-count 99999)))))

(ert-deftest gnosis-test-dashboard-output-tag ()
  "Output-tag returns (tag count-string)."
  (gnosis-test-with-db
    (let ((deck-id (gnosis-test--add-deck "test-deck")))
      (gnosis-test--add-basic-thema deck-id "Q1" "A1" '("math"))
      (gnosis-test--add-basic-thema deck-id "Q2" "A2" '("math" "geo"))
      (gnosis-test--add-basic-thema deck-id "Q3" "A3" '("geo"))
      (let ((result (gnosis-dashboard-output-tag "math")))
        (should (equal (car result) "math"))
        (should (equal (cadr result) "2")))
      (let ((result (gnosis-dashboard-output-tag "geo")))
        (should (equal (cadr result) "2"))))))

(ert-deftest gnosis-test-dashboard-output-deck ()
  "Output-deck returns (name count) list."
  (gnosis-test-with-db
    (let ((deck-id (gnosis-test--add-deck "my-deck")))
      (gnosis-test--add-basic-thema deck-id "Q1" "A1")
      (gnosis-test--add-basic-thema deck-id "Q2" "A2")
      (let ((result (gnosis-dashboard-output-deck deck-id)))
        (should (equal (car result) "my-deck"))
        (should (equal (cadr result) "2"))))))

(ert-deftest gnosis-test-dashboard-get-themata-links ()
  "Get-themata-links returns thema IDs linked to a node."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (_id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3"))
           (node-id "fake-node-uuid"))
      ;; Link two themata to the node
      (gnosis-test--add-link id1 node-id)
      (gnosis-test--add-link id2 node-id)
      (let ((result (gnosis-dashboard-get-themata-links node-id)))
        (should (= (length result) 2))
        ;; source column is TEXT but emacsql returns integers
        (should (member id1 result))
        (should (member id2 result))))))

;; ──────────────────────────────────────────────────────────
;; Entry manipulation tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-remove-entries ()
  "Remove-entries removes matching entries and updates thema-ids."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3")))
      (gnosis-test-with-dashboard-buffer
        (with-current-buffer gnosis-dashboard-buffer-name
          (tabulated-list-mode)
          (setq tabulated-list-format [("Col" 10 t)])
          (tabulated-list-init-header)
          (setq tabulated-list-entries
                (list (list id1 ["Q1"]) (list id2 ["Q2"]) (list id3 ["Q3"])))
          (setq gnosis-dashboard-thema-ids (list id1 id2 id3))
          (tabulated-list-print t)
          ;; Remove id2
          (gnosis-dashboard--remove-entries (list id2))
          ;; Two entries remain
          (should (= (length tabulated-list-entries) 2))
          (should-not (cl-find id2 tabulated-list-entries :key #'car))
          ;; thema-ids also updated
          (should-not (member id2 gnosis-dashboard-thema-ids)))))))

(ert-deftest gnosis-test-dashboard-update-entries ()
  "Update-entries refreshes data from DB for specified IDs."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Original Q" "A1")))
      (gnosis-test-with-dashboard-buffer
        (with-current-buffer gnosis-dashboard-buffer-name
          (tabulated-list-mode)
          (setq tabulated-list-format [("K" 10 t) ("H" 10 t) ("A" 10 t)
                                       ("T" 10 t) ("Ty" 10 t) ("S" 5 t)])
          (tabulated-list-init-header)
          ;; Initial entries from DB
          (let ((initial (gnosis-dashboard--output-themata (list id1))))
            (setq tabulated-list-entries initial)
            (tabulated-list-print t)
            ;; Modify the thema in DB
            (gnosis-update 'themata '(= keimenon "Updated Q") `(= id ,id1))
            ;; Refresh
            (gnosis-dashboard--update-entries (list id1))
            ;; Entry should reflect the update
            (let* ((entry (cl-find id1 tabulated-list-entries :key #'car))
                   (vec (cadr entry)))
              (should (string-search "Updated Q" (aref vec 0))))))))))

;; ──────────────────────────────────────────────────────────
;; Buffer rendering tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-render-themata ()
  "Output-themata renders a buffer with themata-mode active."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1" '("math")))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2" '("geo"))))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata (list id1 id2))
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; Themata mode should be active
          (should gnosis-dashboard-themata-mode)
          ;; Other modes disabled
          (should-not gnosis-dashboard-decks-mode)
          (should-not gnosis-dashboard-tags-mode)
          (should-not gnosis-dashboard-nodes-mode)
          ;; Entries populated
          (should (= (length tabulated-list-entries) 2))
          ;; Current state tracked
          (should (equal (plist-get gnosis-dashboard--current :type) 'themata)))))))

(ert-deftest gnosis-test-dashboard-render-decks ()
  "Output-decks renders a buffer with decks-mode active."
  (gnosis-test-with-db
    (let ((deck-id (gnosis-test--add-deck "deck-1")))
      (gnosis-test--add-basic-thema deck-id "Q1" "A1")
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-decks)
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; Decks mode active
          (should gnosis-dashboard-decks-mode)
          ;; Other modes disabled
          (should-not gnosis-dashboard-themata-mode)
          (should-not gnosis-dashboard-tags-mode)
          (should-not gnosis-dashboard-nodes-mode)
          ;; At least one entry
          (should (>= (length tabulated-list-entries) 1)))))))

(ert-deftest gnosis-test-dashboard-render-tags ()
  "Output-tags renders a buffer with tags-mode active."
  (gnosis-test-with-db
    (let ((deck-id (gnosis-test--add-deck "test-deck")))
      (gnosis-test--add-basic-thema deck-id "Q1" "A1" '("math"))
      (gnosis-test--add-basic-thema deck-id "Q2" "A2" '("geo"))
      (gnosis-test--setup-tags)
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-tags)
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; Tags mode active
          (should gnosis-dashboard-tags-mode)
          ;; Other modes disabled
          (should-not gnosis-dashboard-themata-mode)
          (should-not gnosis-dashboard-decks-mode)
          (should-not gnosis-dashboard-nodes-mode)
          ;; Two unique tags
          (should (= (length tabulated-list-entries) 2)))))))

(ert-deftest gnosis-test-dashboard-mode-switching ()
  "Switching between views disables the previous mode."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1" '("math"))))
      (gnosis-test--setup-tags)
      (gnosis-test-with-dashboard-buffer
        ;; Start in themata mode
        (gnosis-dashboard-output-themata (list id1))
        (with-current-buffer gnosis-dashboard-buffer-name
          (should gnosis-dashboard-themata-mode)
          (should-not gnosis-dashboard-decks-mode))
        ;; Switch to decks
        (gnosis-dashboard-output-decks)
        (with-current-buffer gnosis-dashboard-buffer-name
          (should gnosis-dashboard-decks-mode)
          (should-not gnosis-dashboard-themata-mode))
        ;; Switch to tags
        (gnosis-dashboard-output-tags)
        (with-current-buffer gnosis-dashboard-buffer-name
          (should gnosis-dashboard-tags-mode)
          (should-not gnosis-dashboard-decks-mode)
          (should-not gnosis-dashboard-themata-mode))
        ;; Switch back to themata
        (gnosis-dashboard-output-themata (list id1))
        (with-current-buffer gnosis-dashboard-buffer-name
          (should gnosis-dashboard-themata-mode)
          (should-not gnosis-dashboard-tags-mode)
          (should-not gnosis-dashboard-decks-mode))))))

(ert-deftest gnosis-test-dashboard-render-themata-stores-ids ()
  "Output-themata stores current IDs for history navigation."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (ids (list id1 id2)))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata ids)
        (with-current-buffer gnosis-dashboard-buffer-name
          (should (equal gnosis-dashboard-themata-current-ids ids))
          (should (equal gnosis-dashboard-thema-ids ids)))))))

;; ──────────────────────────────────────────────────────────
;; Mark/selection tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-mark-all-and-unmark ()
  "Mark-all collects all IDs, unmark-all clears them."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2")))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata (list id1 id2))
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; Mark all
          (gnosis-dashboard-mark-all)
          (should (= (length gnosis-dashboard--selected-ids) 2))
          ;; Unmark all
          (gnosis-dashboard-unmark-all)
          (should (null gnosis-dashboard--selected-ids)))))))

;; ──────────────────────────────────────────────────────────
;; Content search helper tests
;; ──────────────────────────────────────────────────────────

(defmacro gnosis-test-with-org-files (file-specs &rest body)
  "Create temporary org files per FILE-SPECS then run BODY.
FILE-SPECS is a list of (ID CONTENT) pairs.
Binds `org-gnosis-dir' to the temp directory."
  (declare (indent 1) (debug t))
  `(let ((org-gnosis-dir (make-temp-file "gnosis-test-org-" t)))
     (unwind-protect
         (progn
           (dolist (spec ,file-specs)
             (let ((id (nth 0 spec))
                   (content (nth 1 spec)))
               (with-temp-file (expand-file-name
                                (format "20250101-%s.org" id) org-gnosis-dir)
                 (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n%s" id content)))))
           ,@body)
       (delete-directory org-gnosis-dir t))))

(ert-deftest gnosis-test-dashboard-search-files-all ()
  "Search all files returns matching node IDs."
  (gnosis-test-with-org-files
      '(("node-aaa" "Emacs is a great editor")
        ("node-bbb" "Vim is also popular")
        ("node-ccc" "Emacs and Vim are both editors"))
    (let ((result (gnosis-dashboard-nodes--search-files "Emacs")))
      (should (= (length result) 2))
      (should (member "node-aaa" result))
      (should (member "node-ccc" result)))))

(ert-deftest gnosis-test-dashboard-search-files-with-filter ()
  "Search with node-ids filter restricts to subset."
  (gnosis-test-with-org-files
      '(("node-aaa" "Emacs is a great editor")
        ("node-bbb" "Vim is also popular")
        ("node-ccc" "Emacs and Vim are both editors"))
    (let ((result (gnosis-dashboard-nodes--search-files
                   "Emacs" '("node-aaa"))))
      (should (= (length result) 1))
      (should (equal (car result) "node-aaa")))))

(ert-deftest gnosis-test-dashboard-search-files-no-matches ()
  "Search with no matches returns nil."
  (gnosis-test-with-org-files
      '(("node-aaa" "Emacs is a great editor"))
    (let ((result (gnosis-dashboard-nodes--search-files "nonexistent-term")))
      (should (null result)))))

;; ──────────────────────────────────────────────────────────
;; Header-line tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-header-line-themata ()
  "Output-themata prepends count badge to header-line."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2")))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata (list id1 id2))
        (with-current-buffer gnosis-dashboard-buffer-name
          (should (listp header-line-format))
          (should (string-search "2" (car header-line-format))))))))

(ert-deftest gnosis-test-dashboard-header-line-tags ()
  "Output-tags prepends count badge to header-line."
  (gnosis-test-with-db
    (let ((deck-id (gnosis-test--add-deck "test-deck")))
      (gnosis-test--add-basic-thema deck-id "Q1" "A1" '("math"))
      (gnosis-test--add-basic-thema deck-id "Q2" "A2" '("geo"))
      (gnosis-test--setup-tags)
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-tags)
        (with-current-buffer gnosis-dashboard-buffer-name
          (should (listp header-line-format))
          (should (string-search "2" (car header-line-format))))))))

(ert-deftest gnosis-test-dashboard-header-line-decks ()
  "Output-decks prepends count badge to header-line."
  (gnosis-test-with-db
    (let ((deck-id (gnosis-test--add-deck "deck-1")))
      (gnosis-test--add-basic-thema deck-id "Q1" "A1")
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-decks)
        (with-current-buffer gnosis-dashboard-buffer-name
          (should (listp header-line-format))
          (should (string-search "1" (car header-line-format))))))))

;; ──────────────────────────────────────────────────────────
;; Review count filter tests
;; ──────────────────────────────────────────────────────────

(defun gnosis-test--set-review-count (id n)
  "Set the review count N for thema ID in review-log."
  (gnosis-update 'review-log `(= n ,n) `(= id ,id)))

(ert-deftest gnosis-test-get-themata-by-reviews-basic ()
  "Get themata filtered by review count."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3")))
      ;; All start at n=0
      (gnosis-test--set-review-count id2 3)
      (gnosis-test--set-review-count id3 5)
      ;; max-reviews=0: only never-reviewed
      (let ((result (gnosis-get-themata-by-reviews 0)))
        (should (= (length result) 1))
        (should (member id1 result)))
      ;; max-reviews=3: id1 (0) and id2 (3)
      (let ((result (gnosis-get-themata-by-reviews 3)))
        (should (= (length result) 2))
        (should (member id1 result))
        (should (member id2 result)))
      ;; max-reviews=5: all three
      (let ((result (gnosis-get-themata-by-reviews 5)))
        (should (= (length result) 3))))))

(ert-deftest gnosis-test-get-themata-by-reviews-with-subset ()
  "Get themata by review count restricted to a subset."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3")))
      ;; All at n=0
      (gnosis-test--set-review-count id3 5)
      ;; Restrict to id1 and id3 with max-reviews=0
      (let ((result (gnosis-get-themata-by-reviews 0 (list id1 id3))))
        (should (= (length result) 1))
        (should (member id1 result))))))

(ert-deftest gnosis-test-dashboard-show-new-renders ()
  "Show-new renders matching themata in the dashboard."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "New Q" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Old Q" "A2")))
      (gnosis-test--set-review-count id2 5)
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-themata-show-new 0)
        (with-current-buffer gnosis-dashboard-buffer-name
          (should gnosis-dashboard-themata-mode)
          (should (= (length tabulated-list-entries) 1))
          (should (equal (caar tabulated-list-entries) id1)))))))

(ert-deftest gnosis-test-dashboard-filter-by-reviews ()
  "Filter current themata by review count."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3")))
      (gnosis-test--set-review-count id2 2)
      (gnosis-test--set-review-count id3 5)
      (gnosis-test-with-dashboard-buffer
        ;; Start with all themata displayed
        (gnosis-dashboard-output-themata (list id1 id2 id3))
        (with-current-buffer gnosis-dashboard-buffer-name
          (should (= (length tabulated-list-entries) 3))
          ;; Filter to max 2 reviews
          (gnosis-dashboard-filter-themata-by-reviews 2)
          (should (= (length tabulated-list-entries) 2))
          (let ((displayed-ids (mapcar #'car tabulated-list-entries)))
            (should (member id1 displayed-ids))
            (should (member id2 displayed-ids))
            (should-not (member id3 displayed-ids))))))))

(ert-run-tests-batch-and-exit)
