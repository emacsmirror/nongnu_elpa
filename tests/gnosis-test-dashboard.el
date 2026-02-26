;;; gnosis-test-dashboard.el --- Dashboard tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for gnosis-dashboard.el functionality.
;; Uses a temporary SQLite database so the user's real DB is untouched.

;;; Code:
(require 'ert)
(require 'gnosis)
(require 'gnosis-tl)
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

;; ──────────────────────────────────────────────────────────
;; gnosis-tl pure function tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-column-specs-basic ()
  "Column specs extracts name, width, pad-right from format vector."
  (let ((specs (gnosis-tl--column-specs [("Name" 20 t) ("Age" 5 t)])))
    (should (= (length specs) 2))
    ;; First column: pad-right defaults to 1
    (should (equal (plist-get (nth 0 specs) :name) "Name"))
    (should (= (plist-get (nth 0 specs) :width) 20))
    (should (= (plist-get (nth 0 specs) :pad-right) 1))
    ;; Last column: pad-right forced to 0
    (should (equal (plist-get (nth 1 specs) :name) "Age"))
    (should (= (plist-get (nth 1 specs) :pad-right) 0))))

(ert-deftest gnosis-test-tl-column-specs-single ()
  "Single column gets pad-right 0."
  (let ((specs (gnosis-tl--column-specs [("Only" 10 t)])))
    (should (= (length specs) 1))
    (should (= (plist-get (nth 0 specs) :pad-right) 0))))

(ert-deftest gnosis-test-tl-column-specs-custom-pad ()
  "Custom :pad-right is respected for non-last columns."
  (let ((specs (gnosis-tl--column-specs [("A" 10 t :pad-right 3) ("B" 5 t)])))
    (should (= (plist-get (nth 0 specs) :pad-right) 3))
    (should (= (plist-get (nth 1 specs) :pad-right) 0))))

(ert-deftest gnosis-test-tl-pad-column-short-text ()
  "Short text is padded to width."
  (let ((result (gnosis-tl--pad-column "hi" 10 1 nil)))
    ;; "hi" + 8 spaces padding + 1 pad-right = 11 chars
    (should (= (length result) 11))
    (should (string-prefix-p "hi" result))))

(ert-deftest gnosis-test-tl-pad-column-truncation ()
  "Long text is truncated with ellipsis."
  (let ((result (gnosis-tl--pad-column "a very long string" 8 1 nil)))
    (should (string-search "..." result))
    ;; Total visual width: 8 (column) + 1 (pad-right) = 9
    (should (= (string-width result) 9))))

(ert-deftest gnosis-test-tl-pad-column-right-align ()
  "Right-aligned text has leading spaces."
  (let ((result (gnosis-tl--pad-column "hi" 10 0 t)))
    ;; 8 spaces + "hi" = 10 chars
    (should (= (string-width result) 10))
    (should (string-suffix-p "hi" result))))

(ert-deftest gnosis-test-tl-format-line-properties ()
  "Format-line attaches tabulated-list-id and tabulated-list-entry properties."
  (let* ((tabulated-list-padding 2)
         (specs (gnosis-tl--column-specs [("Col1" 10 t) ("Col2" 5 t)]))
         (cols (vector "hello" "world"))
         (line (gnosis-tl--format-line 42 cols specs)))
    ;; Has the right text properties
    (should (equal (get-text-property 0 'tabulated-list-id line) 42))
    (should (equal (get-text-property 0 'tabulated-list-entry line) cols))
    ;; Ends with newline
    (should (string-suffix-p "\n" line))
    ;; Contains the column text
    (should (string-search "hello" line))
    (should (string-search "world" line))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl entry operation tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-replace-entry ()
  "Replace-entry swaps a single line without full re-render."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])
            (3 ["Carol" "300"])))
    (tabulated-list-print)
    ;; Verify Bob is present
    (goto-char (point-min))
    (should (search-forward "Bob" nil t))
    ;; Replace Bob's entry
    (gnosis-tl-replace-entry 2 ["Robert" "250"])
    ;; Bob is gone, Robert is present
    (goto-char (point-min))
    (should-not (search-forward "Bob" nil t))
    (goto-char (point-min))
    (should (search-forward "Robert" nil t))
    ;; Other entries untouched
    (goto-char (point-min))
    (should (search-forward "Alice" nil t))
    (goto-char (point-min))
    (should (search-forward "Carol" nil t))
    ;; tabulated-list-get-id works on the replaced line
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal (tabulated-list-get-id) 2)))
      (forward-line 1))
    (should (equal (tabulated-list-get-id) 2))
    ;; Total line count unchanged (3 entries)
    (should (= (count-lines (point-min) (point-max)) 3))))

(ert-deftest gnosis-test-tl-replace-entry-preserves-point ()
  "Replace-entry preserves cursor position."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice"])
            (2 ["Bob"])
            (3 ["Carol"])))
    (tabulated-list-print)
    ;; Position on Carol (line 3)
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (tabulated-list-get-id) 3))
    ;; Replace Alice (line 1) -- cursor should stay on Carol
    (gnosis-tl-replace-entry 1 ["Alicia"])
    (should (equal (tabulated-list-get-id) 3))))

(ert-deftest gnosis-test-tl-replace-entry-nonexistent ()
  "Replace-entry with nonexistent ID is a no-op."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice"])))
    (tabulated-list-print)
    (let ((before (buffer-string)))
      (gnosis-tl-replace-entry 999 ["Ghost"])
      (should (equal (buffer-string) before)))))

(ert-deftest gnosis-test-tl-delete-entry ()
  "Delete-entry removes a single line without full re-render."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])
            (3 ["Carol" "300"])))
    (tabulated-list-print)
    (should (= (count-lines (point-min) (point-max)) 3))
    ;; Delete Bob
    (gnosis-tl-delete-entry 2)
    ;; Bob is gone
    (goto-char (point-min))
    (should-not (search-forward "Bob" nil t))
    ;; Others remain
    (goto-char (point-min))
    (should (search-forward "Alice" nil t))
    (goto-char (point-min))
    (should (search-forward "Carol" nil t))
    ;; Line count decreased
    (should (= (count-lines (point-min) (point-max)) 2))
    ;; tabulated-list-get-id works on remaining lines
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) 1))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 3))))

(ert-deftest gnosis-test-tl-delete-entry-preserves-point ()
  "Delete-entry preserves cursor position."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice"])
            (2 ["Bob"])
            (3 ["Carol"])))
    (tabulated-list-print)
    ;; Position on Carol (line 3)
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (tabulated-list-get-id) 3))
    ;; Delete Alice (line 1) -- cursor should stay on Carol
    (gnosis-tl-delete-entry 1)
    (should (equal (tabulated-list-get-id) 3))))

(ert-deftest gnosis-test-tl-delete-entry-nonexistent ()
  "Delete-entry with nonexistent ID is a no-op."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice"])))
    (tabulated-list-print)
    (let ((before (buffer-string)))
      (gnosis-tl-delete-entry 999)
      (should (equal (buffer-string) before)))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl-print tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-print-basic ()
  "gnosis-tl-print renders entries into the buffer."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])
            (3 ["Carol" "300"])))
    (gnosis-tl-print)
    ;; All entries present
    (goto-char (point-min))
    (should (search-forward "Alice" nil t))
    (goto-char (point-min))
    (should (search-forward "Bob" nil t))
    (goto-char (point-min))
    (should (search-forward "Carol" nil t))
    ;; Correct line count
    (should (= (count-lines (point-min) (point-max)) 3))
    ;; tabulated-list-id properties are set
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'tabulated-list-id) 1))
    (forward-line 1)
    (should (equal (get-text-property (point) 'tabulated-list-id) 2))
    (forward-line 1)
    (should (equal (get-text-property (point) 'tabulated-list-id) 3))))

(ert-deftest gnosis-test-tl-print-sorting ()
  "gnosis-tl-print sorts entries by tabulated-list-sort-key."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Charlie" "300"])
            (2 ["Alice" "100"])
            (3 ["Bob" "200"])))
    ;; Sort by Name ascending (flip=nil)
    (setq tabulated-list-sort-key (cons "Name" nil))
    (gnosis-tl-print)
    ;; First entry should be Alice
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'tabulated-list-id) 2))
    ;; Second should be Bob
    (forward-line 1)
    (should (equal (get-text-property (point) 'tabulated-list-id) 3))
    ;; Third should be Charlie
    (forward-line 1)
    (should (equal (get-text-property (point) 'tabulated-list-id) 1))))

(ert-deftest gnosis-test-tl-print-sorting-descending ()
  "gnosis-tl-print respects descending sort (flip=t)."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])
            (3 ["Charlie" "300"])))
    ;; Sort by Name descending (flip=t)
    (setq tabulated-list-sort-key (cons "Name" t))
    (gnosis-tl-print)
    ;; First entry should be Charlie
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'tabulated-list-id) 3))
    (forward-line 1)
    (should (equal (get-text-property (point) 'tabulated-list-id) 2))
    (forward-line 1)
    (should (equal (get-text-property (point) 'tabulated-list-id) 1))))

(ert-deftest gnosis-test-tl-print-remember-pos ()
  "gnosis-tl-print restores cursor to saved entry ID."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice"])
            (2 ["Bob"])
            (3 ["Carol"])))
    (gnosis-tl-print)
    ;; Position on Bob
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    ;; Re-render with remember-pos
    (gnosis-tl-print t)
    ;; Should still be on Bob
    (should (equal (tabulated-list-get-id) 2))))

(ert-deftest gnosis-test-tl-print-empty ()
  "gnosis-tl-print with no entries produces empty buffer."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries nil)
    (gnosis-tl-print)
    (should (= (buffer-size) 0))))

(ert-deftest gnosis-test-tl-print-remember-pos-column ()
  "gnosis-tl-print restores both entry and column position."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])
            (3 ["Carol" "300"])))
    (gnosis-tl-print)
    ;; Position on Bob, move into the line
    (goto-char (point-min))
    (forward-line 1)
    (move-to-column 10)
    (let ((col (current-column)))
      (should (equal (tabulated-list-get-id) 2))
      ;; Re-render with remember-pos
      (gnosis-tl-print t)
      ;; Same entry and column
      (should (equal (tabulated-list-get-id) 2))
      (should (= (current-column) col)))))

(ert-deftest gnosis-test-tl-print-sorted-same-as-tabulated-list ()
  "gnosis-tl-print with sort key produces same order as tabulated-list-print."
  (let ((entries '((1 ["Charlie" "300"])
                   (2 ["Alice" "100"])
                   (3 ["Bob" "200"]))))
    ;; tabulated-list-print with sort
    (let (tl-ids)
      (with-temp-buffer
        (tabulated-list-mode)
        (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
              tabulated-list-padding 2
              tabulated-list-sort-key (cons "Name" nil)
              tabulated-list-entries (copy-sequence entries))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (while (not (eobp))
          (push (tabulated-list-get-id) tl-ids)
          (forward-line 1))
        (setq tl-ids (nreverse tl-ids)))
      ;; gnosis-tl-print with same sort
      (let (our-ids)
        (with-temp-buffer
          (tabulated-list-mode)
          (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
                tabulated-list-padding 2
                tabulated-list-sort-key (cons "Name" nil)
                tabulated-list-entries (copy-sequence entries))
          (tabulated-list-init-header)
          (gnosis-tl-print)
          (goto-char (point-min))
          (while (not (eobp))
            (push (tabulated-list-get-id) our-ids)
            (forward-line 1))
          (setq our-ids (nreverse our-ids)))
        (should (equal tl-ids our-ids))))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl-render-lines tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-render-lines-basic ()
  "render-lines returns a propertized string with all entries."
  (let* ((fmt [("Name" 20 t) ("Val" 10 t)])
         (entries '((1 ["Alice" "100"])
                    (2 ["Bob" "200"])))
         (result (gnosis-tl-render-lines entries fmt 2)))
    ;; Returns a string
    (should (stringp result))
    ;; Contains both entries
    (should (string-search "Alice" result))
    (should (string-search "Bob" result))
    ;; Has text properties
    (should (equal (get-text-property 0 'tabulated-list-id result) 1))
    ;; Two newlines (one per entry)
    (should (= (cl-count ?\n result) 2))))

(ert-deftest gnosis-test-tl-render-lines-empty ()
  "render-lines with no entries returns empty string."
  (let ((result (gnosis-tl-render-lines nil [("Name" 20 t)] 2)))
    (should (equal result ""))))

(ert-deftest gnosis-test-tl-render-lines-padding ()
  "render-lines respects padding parameter."
  (let* ((fmt [("Name" 20 t)])
         (entries '((1 ["Alice"])))
         (result-0 (gnosis-tl-render-lines entries fmt 0))
         (result-4 (gnosis-tl-render-lines entries fmt 4)))
    ;; result-4 should be 4 chars longer (leading spaces)
    (should (= (- (length result-4) (length result-0)) 4))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl--get-sorter tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-get-sorter-nil-key ()
  "No sorter when tabulated-list-sort-key is nil."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-sort-key nil)
    (should (null (gnosis-tl--get-sorter)))))

(ert-deftest gnosis-test-tl-get-sorter-unsortable ()
  "No sorter when column has nil (not sortable) predicate."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 nil) ("Val" 10 t)])
    (setq tabulated-list-sort-key (cons "Name" nil))
    (should (null (gnosis-tl--get-sorter)))))

(ert-deftest gnosis-test-tl-get-sorter-default-ascending ()
  "Default sorter (t predicate) sorts by string< ascending."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-sort-key (cons "Name" nil))
    (let ((sorter (gnosis-tl--get-sorter)))
      (should (functionp sorter))
      ;; "Alice" < "Bob"
      (should (funcall sorter '(1 ["Alice" "x"]) '(2 ["Bob" "y"])))
      ;; "Bob" NOT < "Alice"
      (should-not (funcall sorter '(2 ["Bob" "y"]) '(1 ["Alice" "x"]))))))

(ert-deftest gnosis-test-tl-get-sorter-default-descending ()
  "Default sorter with flip=t reverses to descending."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (setq tabulated-list-sort-key (cons "Name" t))
    (let ((sorter (gnosis-tl--get-sorter)))
      (should (functionp sorter))
      ;; Flipped: "Bob" before "Alice"
      (should (funcall sorter '(2 ["Bob" "y"]) '(1 ["Alice" "x"])))
      (should-not (funcall sorter '(1 ["Alice" "x"]) '(2 ["Bob" "y"]))))))

(ert-deftest gnosis-test-tl-get-sorter-equal-elements ()
  "Sorter returns nil for equal elements in both directions.
This is the critical bug fix: (not nil) => t was wrong."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)])
    ;; Ascending: equal elements
    (setq tabulated-list-sort-key (cons "Name" nil))
    (let ((sorter (gnosis-tl--get-sorter)))
      (should-not (funcall sorter '(1 ["Alice"]) '(2 ["Alice"]))))
    ;; Descending: equal elements must ALSO return nil
    (setq tabulated-list-sort-key (cons "Name" t))
    (let ((sorter (gnosis-tl--get-sorter)))
      (should-not (funcall sorter '(1 ["Alice"]) '(2 ["Alice"]))))))

(ert-deftest gnosis-test-tl-get-sorter-custom-predicate ()
  "Custom function predicate is called correctly."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          [("Name" 20 t)
           ("Count" 10 (lambda (a b)
                         (< (string-to-number (aref (cadr a) 1))
                            (string-to-number (aref (cadr b) 1)))))])
    (setq tabulated-list-sort-key (cons "Count" nil))
    (let ((sorter (gnosis-tl--get-sorter)))
      (should (funcall sorter '(1 ["x" "5"]) '(2 ["y" "10"])))
      (should-not (funcall sorter '(2 ["y" "10"]) '(1 ["x" "5"]))))))

(ert-deftest gnosis-test-tl-get-sorter-matches-tabulated-list ()
  "gnosis-tl--get-sorter produces same sort order as tabulated-list--get-sorter."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)])
    (let ((entries '((1 ["Charlie" "300"])
                     (2 ["Alice" "100"])
                     (3 ["Bob" "200"])
                     (4 ["Alice" "050"]))))
      ;; Ascending
      (setq tabulated-list-sort-key (cons "Name" nil))
      (let ((our (sort (copy-sequence entries) (gnosis-tl--get-sorter)))
            (theirs (sort (copy-sequence entries) (tabulated-list--get-sorter))))
        (should (equal (mapcar #'car our) (mapcar #'car theirs))))
      ;; Descending
      (setq tabulated-list-sort-key (cons "Name" t))
      (let ((our (sort (copy-sequence entries) (gnosis-tl--get-sorter)))
            (theirs (sort (copy-sequence entries) (tabulated-list--get-sorter))))
        (should (equal (mapcar #'car our) (mapcar #'car theirs)))))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl--column-at-point tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-column-at-point-first ()
  "Cursor on first column returns index 0."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice" "100"])))
    (gnosis-tl-print)
    (goto-char (point-min))
    (move-to-column 2)  ;; past padding, on first column
    (should (= (gnosis-tl--column-at-point) 0))))

(ert-deftest gnosis-test-tl-column-at-point-second ()
  "Cursor on second column returns index 1."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice" "100"])))
    (gnosis-tl-print)
    (goto-char (point-min))
    ;; padding(2) + Name(20) + pad-right(1) = 23
    (move-to-column 23)
    (should (= (gnosis-tl--column-at-point) 1))))

(ert-deftest gnosis-test-tl-column-at-point-on-padding ()
  "Cursor on padding (before first column) returns 0."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice" "100"])))
    (gnosis-tl-print)
    (goto-char (point-min))
    (move-to-column 0)
    (should (= (gnosis-tl--column-at-point) 0))))

(ert-deftest gnosis-test-tl-column-at-point-three-cols ()
  "Column detection works for 3-column format."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("A" 10 t) ("B" 15 t) ("C" 8 t)]
          tabulated-list-padding 0)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["aaa" "bbb" "ccc"])))
    (gnosis-tl-print)
    (goto-char (point-min))
    ;; Column A: 0..9 (width 10), pad-right 1
    (move-to-column 5)
    (should (= (gnosis-tl--column-at-point) 0))
    ;; Column B starts at 11: A(10) + pad(1)
    (move-to-column 11)
    (should (= (gnosis-tl--column-at-point) 1))
    ;; Column C starts at 27: A(10) + pad(1) + B(15) + pad(1)
    (move-to-column 27)
    (should (= (gnosis-tl--column-at-point) 2))))

(ert-deftest gnosis-test-tl-column-at-point-custom-pad-right ()
  "Column detection respects custom :pad-right."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("A" 10 t :pad-right 3) ("B" 10 t)]
          tabulated-list-padding 0)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["aaa" "bbb"])))
    (gnosis-tl-print)
    (goto-char (point-min))
    ;; Column A: 0..9, pad-right 3, so B starts at 13
    (move-to-column 12)
    (should (= (gnosis-tl--column-at-point) 0))
    (move-to-column 13)
    (should (= (gnosis-tl--column-at-point) 1))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl-sort tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-sort-by-column-name ()
  "gnosis-tl-sort detects column at point and sorts."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Charlie" "300"])
            (2 ["Alice" "100"])
            (3 ["Bob" "200"])))
    (gnosis-tl-print)
    ;; Move past padding to column text
    (goto-char (point-min))
    (move-to-column 2)
    (gnosis-tl-sort)
    ;; After sorting by Name ascending, Alice should be first
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) 2))
    ;; Sort key was set
    (should (equal (car tabulated-list-sort-key) "Name"))
    (should-not (cdr tabulated-list-sort-key))))

(ert-deftest gnosis-test-tl-sort-toggle-direction ()
  "Pressing gnosis-tl-sort twice on same column flips direction."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])
            (3 ["Charlie" "300"])))
    (gnosis-tl-print)
    ;; Move past padding to column text
    (goto-char (point-min))
    (move-to-column 2)
    ;; First sort: ascending
    (gnosis-tl-sort)
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) 1))  ;; Alice first
    ;; Move past padding again for second sort
    (move-to-column 2)
    ;; Second sort: descending
    (gnosis-tl-sort)
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) 3))))  ;; Charlie first

(ert-deftest gnosis-test-tl-sort-by-column-number ()
  "gnosis-tl-sort with numeric prefix sorts by column index."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Charlie" "100"])
            (2 ["Alice" "300"])
            (3 ["Bob" "200"])))
    (gnosis-tl-print)
    ;; Sort by column 1 ("Val") ascending
    (gnosis-tl-sort 1)
    (goto-char (point-min))
    ;; "100" < "200" < "300" (string<)
    (should (equal (tabulated-list-get-id) 1))   ;; Charlie "100"
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 3))   ;; Bob "200"
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2)))) ;; Alice "300"

(ert-deftest gnosis-test-tl-sort-preserves-position ()
  "gnosis-tl-sort preserves cursor on the same entry."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          '((1 ["Charlie"])
            (2 ["Alice"])
            (3 ["Bob"])))
    (gnosis-tl-print)
    ;; Position on Bob (line 3)
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (tabulated-list-get-id) 3))
    ;; Sort — Bob should still be under cursor
    (gnosis-tl-sort 0)
    (should (equal (tabulated-list-get-id) 3))))

(ert-deftest gnosis-test-tl-sort-same-as-tabulated-list-sort ()
  "gnosis-tl-sort produces same entry order as tabulated-list-sort."
  (let ((entries '((1 ["Charlie" "300"])
                   (2 ["Alice" "100"])
                   (3 ["Bob" "200"]))))
    ;; Sort with tabulated-list-sort
    (let (tl-ids)
      (with-temp-buffer
        (tabulated-list-mode)
        (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
              tabulated-list-padding 2
              tabulated-list-sort-key nil
              tabulated-list-entries (copy-sequence entries))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (tabulated-list-sort 0)
        (goto-char (point-min))
        (while (not (eobp))
          (push (tabulated-list-get-id) tl-ids)
          (forward-line 1))
        (setq tl-ids (nreverse tl-ids)))
      ;; Sort with gnosis-tl-sort
      (let (our-ids)
        (with-temp-buffer
          (tabulated-list-mode)
          (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
                tabulated-list-padding 2
                tabulated-list-sort-key nil
                tabulated-list-entries (copy-sequence entries))
          (tabulated-list-init-header)
          (gnosis-tl-print)
          (goto-char (point-min))
          (gnosis-tl-sort 0)
          (goto-char (point-min))
          (while (not (eobp))
            (push (tabulated-list-get-id) our-ids)
            (forward-line 1))
          (setq our-ids (nreverse our-ids)))
        (should (equal tl-ids our-ids))))))

;; ──────────────────────────────────────────────────────────
;; gnosis-tl-print equivalence tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-print-same-ids-as-tabulated-list-print ()
  "gnosis-tl-print produces the same entry IDs per line as tabulated-list-print."
  (let* ((fmt [("Name" 20 t) ("Val" 10 t)])
         (entries '((1 ["Alice" "100"])
                    (2 ["Bob" "200"])
                    (3 ["Carol" "300"]))))
    ;; Render with tabulated-list-print
    (let (tl-ids)
      (with-temp-buffer
        (tabulated-list-mode)
        (setq tabulated-list-format fmt
              tabulated-list-padding 2
              tabulated-list-sort-key nil
              tabulated-list-entries (copy-sequence entries))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (while (not (eobp))
          (push (tabulated-list-get-id) tl-ids)
          (forward-line 1))
        (setq tl-ids (nreverse tl-ids)))
      ;; Render with gnosis-tl-print
      (let (our-ids)
        (with-temp-buffer
          (tabulated-list-mode)
          (setq tabulated-list-format fmt
                tabulated-list-padding 2
                tabulated-list-sort-key nil
                tabulated-list-entries (copy-sequence entries))
          (tabulated-list-init-header)
          (gnosis-tl-print)
          (goto-char (point-min))
          (while (not (eobp))
            (push (tabulated-list-get-id) our-ids)
            (forward-line 1))
          (setq our-ids (nreverse our-ids)))
        ;; Same IDs in same order
        (should (equal tl-ids our-ids))))))

;; ──────────────────────────────────────────────────────────
;; gnosis-dashboard--compute-column-format tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-compute-column-format ()
  "compute-column-format returns a 6-column format vector."
  (let ((fmt (gnosis-dashboard--compute-column-format 120)))
    (should (vectorp fmt))
    (should (= (length fmt) 6))
    ;; Column names
    (should (equal (car (aref fmt 0)) "Keimenon"))
    (should (equal (car (aref fmt 5)) "Suspend"))
    ;; Widths are positive
    (dotimes (i 6)
      (should (> (nth 1 (aref fmt i)) 0)))))

(ert-deftest gnosis-test-dashboard-compute-column-format-narrow ()
  "compute-column-format enforces minimum widths for narrow windows."
  (let ((fmt (gnosis-dashboard--compute-column-format 30)))
    ;; Minimums enforced
    (should (>= (nth 1 (aref fmt 0)) 10))  ;; Keimenon
    (should (>= (nth 1 (aref fmt 4)) 5))   ;; Type
    (should (>= (nth 1 (aref fmt 5)) 3)))) ;; Suspend

;; ──────────────────────────────────────────────────────────
;; gnosis-tl-append-entries tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-tl-append-entries-basic ()
  "Append-entries adds lines at the end of the buffer."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    ;; Render initial entries
    (setq tabulated-list-entries
          '((1 ["Alice" "100"])
            (2 ["Bob" "200"])))
    (gnosis-tl-print)
    (should (= (count-lines (point-min) (point-max)) 2))
    ;; Append more entries
    (gnosis-tl-append-entries '((3 ["Carol" "300"])
                                (4 ["Dave" "400"])))
    ;; Now 4 lines
    (should (= (count-lines (point-min) (point-max)) 4))
    ;; All entries present with correct IDs
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) 1))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 3))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 4))))

(ert-deftest gnosis-test-tl-append-entries-preserves-point ()
  "Append-entries does not move point."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice"]) (2 ["Bob"])))
    (gnosis-tl-print)
    ;; Position on Bob
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    ;; Append — cursor should stay on Bob
    (gnosis-tl-append-entries '((3 ["Carol"])))
    (should (equal (tabulated-list-get-id) 2))))

(ert-deftest gnosis-test-tl-append-entries-empty ()
  "Append-entries with empty list is a no-op."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq tabulated-list-entries '((1 ["Alice"])))
    (gnosis-tl-print)
    (let ((before (buffer-string)))
      (gnosis-tl-append-entries nil)
      (should (equal (buffer-string) before)))))

(ert-deftest gnosis-test-tl-append-entries-to-empty-buffer ()
  "Append-entries works on an empty buffer."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t) ("Val" 10 t)]
          tabulated-list-padding 2)
    (tabulated-list-init-header)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (gnosis-tl-append-entries '((1 ["Alice" "100"])))
    (should (= (count-lines (point-min) (point-max)) 1))
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) 1))))

;; ──────────────────────────────────────────────────────────
;; Progressive rendering tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-progressive-render-small ()
  "Progressive render with fewer entries than chunk size renders all at once."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3")))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata (list id1 id2 id3))
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; All 3 entries rendered immediately (< chunk size)
          (should (= (length tabulated-list-entries) 3))
          (should (= (count-lines (point-min) (point-max)) 3))
          ;; IDs are correct
          (goto-char (point-min))
          (let ((ids nil))
            (while (not (eobp))
              (push (tabulated-list-get-id) ids)
              (forward-line 1))
            (should (= (length ids) 3))))))))

(ert-deftest gnosis-test-dashboard-progressive-render-chunked ()
  "Progressive render splits entries when exceeding chunk size."
  (gnosis-test-with-db
    (let* ((gnosis-dashboard-render-chunk-size 2)
           (deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3"))
           (id4 (gnosis-test--add-basic-thema deck-id "Q4" "A4"))
           (id5 (gnosis-test--add-basic-thema deck-id "Q5" "A5")))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata (list id1 id2 id3 id4 id5))
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; First chunk: only 2 entries rendered synchronously
          (should (= (length tabulated-list-entries) 2))
          (should (= (count-lines (point-min) (point-max)) 2)))))))

(ert-deftest gnosis-test-dashboard-progressive-render-stale-gen ()
  "Progressive render timer no-ops when generation is stale."
  (gnosis-test-with-db
    (let* ((gnosis-dashboard-render-chunk-size 2)
           (deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2"))
           (id3 (gnosis-test--add-basic-thema deck-id "Q3" "A3")))
      (gnosis-test-with-dashboard-buffer
        (gnosis-dashboard-output-themata (list id1 id2 id3))
        (with-current-buffer gnosis-dashboard-buffer-name
          ;; First chunk rendered
          (should (= (length tabulated-list-entries) 2))
          ;; Simulate navigation away (bumps generation)
          (cl-incf gnosis-dashboard--load-generation)
          ;; Manually call append-chunk with stale gen — should be no-op
          (let ((old-count (count-lines (point-min) (point-max))))
            (gnosis-dashboard--append-chunk
             (current-buffer)
             (list (list (list id3 ["Q3" "" "" "test" "basic" "No"])))
             (last tabulated-list-entries)
             (1- gnosis-dashboard--load-generation))
            (should (= (count-lines (point-min) (point-max)) old-count))))))))

;; ──────────────────────────────────────────────────────────
;; Benchmark tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-dashboard-print-entry-benchmark ()
  "Benchmark gnosis-tl-print vs tabulated-list-print at various sizes."
  (let ((fmt [("Keimenon" 30 t) ("Hypothesis" 20 t) ("Answer" 20 t)
              ("Tags" 15 t) ("Type" 10 t) ("Suspend" 10 t)])
        (cols ["A sample keimenon text" "some hypothesis"
               "the answer" "tag1,tag2" "basic" "No"]))
    (dolist (n '(2000 10000 40000))
      (let ((entries (cl-loop for i from 1 to n
                              collect (list i cols))))
        ;; tabulated-list-print
        (with-temp-buffer
          (tabulated-list-mode)
          (setq tabulated-list-format fmt
                tabulated-list-padding 2
                tabulated-list-sort-key nil
                tabulated-list-entries entries)
          (tabulated-list-init-header)
          (let ((start (float-time)))
            (tabulated-list-print t)
            (message "tabulated-list-print x%d: %.3fs (%.0fus/entry)"
                     n (- (float-time) start)
                     (* 1e6 (/ (- (float-time) start) n)))))
        ;; gnosis-tl-print
        (with-temp-buffer
          (tabulated-list-mode)
          (setq tabulated-list-format fmt
                tabulated-list-padding 2
                tabulated-list-sort-key nil
                tabulated-list-entries entries)
          (tabulated-list-init-header)
          (let ((start (float-time)))
            (gnosis-tl-print)
            (message "gnosis-tl-print x%d: %.3fs (%.0fus/entry)"
                     n (- (float-time) start)
                     (* 1e6 (/ (- (float-time) start) n)))))
        ;; gnosis-tl-render-lines (pure, no buffer)
        (let ((start (float-time)))
          (gnosis-tl-render-lines entries fmt 2)
          (message "gnosis-tl-render-lines x%d: %.3fs (%.0fus/entry)"
                   n (- (float-time) start)
                   (* 1e6 (/ (- (float-time) start) n))))))
    (should t)))

(ert-run-tests-batch-and-exit)
