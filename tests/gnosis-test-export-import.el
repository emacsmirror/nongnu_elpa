;;; gnosis-test-export-import.el --- Export/import tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for deck export and import functionality.
;; Uses a temporary SQLite database so the user's real DB is untouched.

;;; Code:
(require 'ert)
(require 'gnosis)
(require 'gnosis-export-import)

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
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-" nil ".db"))
          (gnosis-db (emacsql-sqlite-open gnosis-test--db-file))
          (gnosis--id-cache nil))
     (unwind-protect
         (progn
           ;; Create all tables
           (emacsql-with-transaction gnosis-db
             (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
               (emacsql gnosis-db [:create-table $i1 $S2] table schema)))
           ,@body)
       (emacsql-close gnosis-db)
       (delete-file gnosis-test--db-file))))

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

(defun gnosis-test--kill-export-buffer (deck-name)
  "Kill the export buffer for DECK-NAME if it exists."
  (let ((buf (get-buffer (format "EXPORT: %s" deck-name))))
    (when buf (kill-buffer buf))))

;; ──────────────────────────────────────────────────────────
;; Export tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-export-deck-basic ()
  "Export a deck and verify the org buffer content."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "test-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "What is 2+2?" "4"
                                              '("math" "basic")))
           (id2 (gnosis-test--add-basic-thema deck-id "Capital of Greece?" "Athens"
                                              '("geo")))
           (export-file (concat (make-temp-file "gnosis-export-") ".org")))
      (unwind-protect
          (progn
            (gnosis-export-deck deck-id export-file nil)
            (should (file-exists-p export-file))
            (with-temp-buffer
              (insert-file-contents export-file)
              (let ((content (buffer-string)))
                ;; Deck header present
                (should (string-search "#+DECK: test-deck" content))
                ;; Themata count in header
                (should (string-search "#+THEMATA: 2" content))
                ;; Both themata exported
                (should (string-search (number-to-string id1) content))
                (should (string-search (number-to-string id2) content))
                ;; Tags in org format
                (should (string-search ":math:basic:" content))
                (should (string-search ":geo:" content))
                ;; Properties present
                (should (string-search ":GNOSIS_ID:" content))
                (should (string-search ":GNOSIS_TYPE: basic" content))
                ;; Content present
                (should (string-search "What is 2+2?" content))
                (should (string-search "Capital of Greece?" content))
                (should (string-search "4" content))
                (should (string-search "Athens" content)))))
        (when (file-exists-p export-file)
          (delete-file export-file))
        (gnosis-test--kill-export-buffer "test-deck")))))

(ert-deftest gnosis-test-export-deck-new-ids ()
  "Export with new-p replaces IDs with NEW."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "new-id-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (export-file (concat (make-temp-file "gnosis-export-new-") ".org")))
      (unwind-protect
          (progn
            (gnosis-export-deck deck-id export-file t)
            (with-temp-buffer
              (insert-file-contents export-file)
              (let ((content (buffer-string)))
                (should (string-search ":GNOSIS_ID: NEW" content))
                (should-not (string-search
                             (format ":GNOSIS_ID: %d" id1) content)))))
        (when (file-exists-p export-file)
          (delete-file export-file))
        (gnosis-test--kill-export-buffer "new-id-deck")))))

(ert-deftest gnosis-test-export-empty-deck ()
  "Exporting an empty deck produces a file with just the header."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "empty-deck"))
           (export-file (concat (make-temp-file "gnosis-export-empty-") ".org")))
      (unwind-protect
          (progn
            (gnosis-export-deck deck-id export-file nil)
            (with-temp-buffer
              (insert-file-contents export-file)
              (let ((content (buffer-string)))
                (should (string-search "#+DECK: empty-deck" content))
                (should-not (string-search "* Thema" content)))))
        (when (file-exists-p export-file)
          (delete-file export-file))
        (gnosis-test--kill-export-buffer "empty-deck")))))

(ert-deftest gnosis-test-export-excludes-suspended ()
  "Export without include-suspended skips suspended themata."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "susp-deck"))
           (_id1 (gnosis-test--add-basic-thema deck-id "Active Q" "A1" '("a")))
           (_id2 (gnosis-test--add-basic-thema deck-id "Suspended Q" "A2"
                                               '("s") nil nil 1))
           (export-file (concat (make-temp-file "gnosis-export-susp-") ".org")))
      (unwind-protect
          (progn
            ;; Export without suspended
            (gnosis-export-deck deck-id export-file nil nil)
            (with-temp-buffer
              (insert-file-contents export-file)
              (let ((content (buffer-string)))
                (should (string-search "#+THEMATA: 1" content))
                (should (string-search "Active Q" content))
                (should-not (string-search "Suspended Q" content)))))
        (when (file-exists-p export-file)
          (delete-file export-file))
        (gnosis-test--kill-export-buffer "susp-deck")))))

(ert-deftest gnosis-test-export-includes-suspended ()
  "Export with include-suspended includes all themata."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "susp-deck2"))
           (_id1 (gnosis-test--add-basic-thema deck-id "Active Q" "A1" '("a")))
           (_id2 (gnosis-test--add-basic-thema deck-id "Suspended Q" "A2"
                                               '("s") nil nil 1))
           (export-file (concat (make-temp-file "gnosis-export-susp2-") ".org")))
      (unwind-protect
          (progn
            ;; Export with suspended
            (gnosis-export-deck deck-id export-file nil t)
            (with-temp-buffer
              (insert-file-contents export-file)
              (let ((content (buffer-string)))
                (should (string-search "#+THEMATA: 2" content))
                (should (string-search "Active Q" content))
                (should (string-search "Suspended Q" content)))))
        (when (file-exists-p export-file)
          (delete-file export-file))
        (gnosis-test--kill-export-buffer "susp-deck2")))))

;; ──────────────────────────────────────────────────────────
;; Import tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-import-creates-deck ()
  "Importing a file creates the deck if it doesn't exist."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "import-deck"))
           (_id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1" '("tag1")))
           (export-file (concat (make-temp-file "gnosis-import-") ".org")))
      (unwind-protect
          (progn
            (gnosis-export-deck deck-id export-file t)
            (gnosis-test--kill-export-buffer "import-deck")
            ;; Import into a fresh DB to check deck creation
            (let* ((db-file2 (make-temp-file "gnosis-test2-" nil ".db"))
                   (gnosis-db (emacsql-sqlite-open db-file2))
                   (gnosis--id-cache nil))
              (unwind-protect
                  (progn
                    (emacsql-with-transaction gnosis-db
                      (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
                        (emacsql gnosis-db [:create-table $i1 $S2] table schema)))
                    ;; Deck should not exist yet
                    (should-not (gnosis-get 'id 'decks '(= name "import-deck")))
                    (gnosis-import-deck export-file)
                    ;; Deck should now exist
                    (should (gnosis-get 'id 'decks '(= name "import-deck")))
                    ;; Thema should exist
                    (should (= 1 (length (gnosis-select 'id 'themata nil t)))))
                (emacsql-close gnosis-db)
                (delete-file db-file2))))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

(ert-deftest gnosis-test-import-roundtrip ()
  "Export then import: thema count and content survive the roundtrip."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "roundtrip"))
           (_id1 (gnosis-test--add-basic-thema deck-id "What is Emacs?" "A text editor"
                                               '("emacs" "editor")))
           (_id2 (gnosis-test--add-basic-thema deck-id "What is Lisp?" "A language"
                                               '("lisp") "See SICP"))
           (_id3 (gnosis-test--add-basic-thema deck-id "What is org?" "A mode"
                                               '("org")))
           (export-file (concat (make-temp-file "gnosis-roundtrip-") ".org")))
      (unwind-protect
          (progn
            (gnosis-export-deck deck-id export-file t)
            (gnosis-test--kill-export-buffer "roundtrip")
            ;; Import into a fresh DB
            (let* ((db-file2 (make-temp-file "gnosis-rt2-" nil ".db"))
                   (gnosis-db (emacsql-sqlite-open db-file2))
                   (gnosis--id-cache nil))
              (unwind-protect
                  (progn
                    (emacsql-with-transaction gnosis-db
                      (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
                        (emacsql gnosis-db [:create-table $i1 $S2] table schema)))
                    (gnosis-import-deck export-file)
                    ;; 3 themata imported
                    (should (= 3 (length (gnosis-select 'id 'themata nil t))))
                    ;; Content preserved
                    (let ((all-keimenon (gnosis-select 'keimenon 'themata nil t)))
                      (should (member "What is Emacs?" all-keimenon))
                      (should (member "What is Lisp?" all-keimenon))
                      (should (member "What is org?" all-keimenon)))
                    ;; Parathema preserved
                    (let* ((thema-id (gnosis-get 'id 'themata
                                                 '(= keimenon "What is Lisp?")))
                           (parathema (gnosis-get 'parathema 'extras
                                                  `(= id ,thema-id))))
                      (should (string-search "See SICP" parathema)))
                    ;; Tags preserved
                    (let* ((thema-id (gnosis-get 'id 'themata
                                                 '(= keimenon "What is Emacs?")))
                           (tags (gnosis-get 'tags 'themata `(= id ,thema-id))))
                      (should (member "emacs" tags))
                      (should (member "editor" tags))))
                (emacsql-close gnosis-db)
                (delete-file db-file2))))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

(ert-deftest gnosis-test-import-updates-existing-thema ()
  "Importing with existing IDs updates themata rather than duplicating."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "update-deck"))
           (id1 (gnosis-test--add-basic-thema deck-id "Old question" "Old answer"
                                              '("old")))
           (export-file (concat (make-temp-file "gnosis-update-") ".org")))
      (unwind-protect
          (progn
            ;; Export with real IDs (not NEW)
            (gnosis-export-deck deck-id export-file nil)
            (gnosis-test--kill-export-buffer "update-deck")
            ;; Modify the exported file: change the answer
            (with-temp-file export-file
              (insert-file-contents export-file)
              (goto-char (point-min))
              (when (search-forward "Old answer" nil t)
                (replace-match "New answer")))
            ;; Import back — should update, not duplicate
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
              (gnosis-import-deck export-file))
            ;; Still just 1 thema
            (should (= 1 (length (gnosis-select 'id 'themata nil t))))
            ;; Answer updated
            (let ((answer (gnosis-get 'answer 'themata `(= id ,id1))))
              (should (member "New answer" answer))))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

(ert-deftest gnosis-test-import-confirms-existing-deck ()
  "Importing into an existing deck prompts; declining aborts."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "confirm-deck"))
           (_id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (export-file (concat (make-temp-file "gnosis-confirm-") ".org")))
      (unwind-protect
          (progn
            (gnosis-export-deck deck-id export-file nil)
            (gnosis-test--kill-export-buffer "confirm-deck")
            ;; Decline the prompt — should abort
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
              (should-error (gnosis-import-deck export-file)
                            :type 'user-error)))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

;; ──────────────────────────────────────────────────────────
;; Export: read-only property tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-export-insert-thema-content ()
  "Inserting a thema produces all sections (keimenon, hypothesis, answer, parathema)."
  (with-temp-buffer
    (gnosis-export--insert-thema "1" "basic" "Question?" "- hint" "- answer" "extra"
				 '("tag1" "tag2"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\*\\* Keimenon" text))
      (should (string-match-p "Question?" text))
      (should (string-match-p "\\*\\* Hypothesis" text))
      (should (string-match-p "- hint" text))
      (should (string-match-p "\\*\\* Answer" text))
      (should (string-match-p "- answer" text))
      (should (string-match-p "\\*\\* Parathema" text))
      (should (string-match-p "extra" text)))))

(ert-deftest gnosis-test-export-insert-thema-read-only-no-leak ()
  "Read-only on PROPERTIES block does not leak into content sections."
  (with-temp-buffer
    (gnosis-export--insert-thema "1" "basic" "Q" "H" "A" "P" '("t"))
    (goto-char (point-min))
    (search-forward "** Keimenon")
    (should-not (get-text-property (point) 'read-only))
    (search-forward "Q")
    (should-not (get-text-property (match-beginning 0) 'read-only))))

(ert-deftest gnosis-test-export-multiple-themata-no-read-only-error ()
  "Inserting multiple themata in sequence does not signal text-read-only."
  (with-temp-buffer
    (gnosis-export--insert-thema "1" "basic" "Q1" "H1" "A1" "P1" '("t"))
    (gnosis-export--insert-thema "2" "cloze" "Q2" "H2" "A2" "P2" '("t"))
    (gnosis-export--insert-thema "3" "mcq" "Q3" "H3" "A3" "P3" '("t"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "GNOSIS_ID: 1" text))
      (should (string-match-p "GNOSIS_ID: 2" text))
      (should (string-match-p "GNOSIS_ID: 3" text))
      (should (string-match-p "Q3" text)))))

;; ──────────────────────────────────────────────────────────
;; ID cache tests
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-id-cache-generates-unique ()
  "gnosis-generate-id with cache produces unique IDs and registers them."
  (let ((gnosis--id-cache (make-hash-table :test 'equal)))
    ;; Seed cache with one known ID
    (puthash 12345678901 t gnosis--id-cache)
    (let ((new-id (gnosis-generate-id 11)))
      ;; Should not collide
      (should-not (= new-id 12345678901))
      ;; Should be registered in cache
      (should (gethash new-id gnosis--id-cache)))))

(ert-deftest gnosis-test-id-cache-no-db-query ()
  "gnosis-generate-id with cache works without a DB connection."
  (let ((gnosis--id-cache (make-hash-table :test 'equal))
        (gnosis-db nil))
    ;; Should not error — cache means no DB query needed
    (let ((id (gnosis-generate-id)))
      (should (integerp id))
      (should (gethash id gnosis--id-cache)))))

(ert-run-tests-batch-and-exit)
