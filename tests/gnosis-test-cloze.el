;;; gnosis-test-cloze.el --- Cloze extraction tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for cloze extraction, tag removal, and Anki-like syntax
;; ({{cN::answer::hint}}).  All tested functions are pure — no database needed.

;;; Code:

(require 'ert)
(require 'gnosis)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

;; ──────────────────────────────────────────────────────────
;; gnosis-cloze-extract-contents
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-cloze-extract-single ()
  "Extract a single cloze with double braces and double colons."
  (let ((result (gnosis-cloze-extract-contents "{{c1::Penicillin}}")))
    (should (equal result '(("Penicillin"))))))

(ert-deftest gnosis-test-cloze-extract-single-brace ()
  "Extract a single cloze with single braces and single colon."
  (let ((result (gnosis-cloze-extract-contents "{c1:Penicillin}")))
    (should (equal result '(("Penicillin"))))))

(ert-deftest gnosis-test-cloze-extract-with-hint ()
  "Extract cloze with hint — hint is included in extracted content."
  (let ((result (gnosis-cloze-extract-contents
                 "{{c1::Penicillin::antibiotic}}")))
    (should (equal result '(("Penicillin::antibiotic"))))))

(ert-deftest gnosis-test-cloze-extract-multiple-groups ()
  "Extract clozes from different cX groups."
  (let ((result (gnosis-cloze-extract-contents
                 "{{c1::alpha}} and {{c2::beta}}")))
    (should (= (length result) 2))
    (should (equal (nth 0 result) '("alpha")))
    (should (equal (nth 1 result) '("beta")))))

(ert-deftest gnosis-test-cloze-extract-same-group ()
  "Multiple clozes in the same cX group are collected together."
  (let ((result (gnosis-cloze-extract-contents
                 "{{c1::alpha}} and {{c1::beta}}")))
    (should (= (length result) 1))
    (should (equal (car result) '("alpha" "beta")))))

(ert-deftest gnosis-test-cloze-extract-mixed-groups-with-hints ()
  "Multiple groups, some with hints."
  (let ((result (gnosis-cloze-extract-contents
                 "{{c1::drug::type}} treats {{c2::infection}}")))
    (should (= (length result) 2))
    (should (equal (nth 0 result) '("drug::type")))
    (should (equal (nth 1 result) '("infection")))))

(ert-deftest gnosis-test-cloze-extract-anki-mc-hint ()
  "Anki-style hint with options (rapid or slow) is preserved."
  (let ((result (gnosis-cloze-extract-contents
                 "{{c1::rapid::rapid or slow}}-onset food poisoning")))
    (should (equal (car result) '("rapid::rapid or slow")))))

(ert-deftest gnosis-test-cloze-extract-inline-text ()
  "Clozes embedded in longer text are extracted correctly."
  (let ((result (gnosis-cloze-extract-contents
                 "The {{c1::mitochondria}} is the {{c2::powerhouse}} of the cell")))
    (should (= (length result) 2))
    (should (equal (nth 0 result) '("mitochondria")))
    (should (equal (nth 1 result) '("powerhouse")))))

(ert-deftest gnosis-test-cloze-extract-no-cloze ()
  "Plain text without clozes returns empty list."
  (should (null (gnosis-cloze-extract-contents "No clozes here"))))

;; ──────────────────────────────────────────────────────────
;; gnosis-cloze-extract-answers
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-cloze-answers-no-hint ()
  "Answers without hints are returned unchanged."
  (let ((result (gnosis-cloze-extract-answers '(("Penicillin")))))
    (should (equal result '(("Penicillin"))))))

(ert-deftest gnosis-test-cloze-answers-strip-hint ()
  "Hint portion after :: is stripped from answers."
  (let ((result (gnosis-cloze-extract-answers
                 '(("Penicillin::antibiotic")))))
    (should (equal result '(("Penicillin"))))))

(ert-deftest gnosis-test-cloze-answers-strip-mc-hint ()
  "MC-style hints (rapid or slow) are stripped from answers."
  (let ((result (gnosis-cloze-extract-answers
                 '(("rapid::rapid or slow")))))
    (should (equal result '(("rapid"))))))

(ert-deftest gnosis-test-cloze-answers-multiple-groups ()
  "Answers are extracted per group, preserving nesting."
  (let ((result (gnosis-cloze-extract-answers
                 '(("drug::type") ("infection")))))
    (should (equal result '(("drug") ("infection"))))))

(ert-deftest gnosis-test-cloze-answers-same-group-multiple ()
  "Multiple clozes in one group all get their hints stripped."
  (let ((result (gnosis-cloze-extract-answers
                 '(("cleft lip::face" "cleft palate::face")))))
    (should (equal result '(("cleft lip" "cleft palate"))))))

;; ──────────────────────────────────────────────────────────
;; gnosis-cloze-extract-hints
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-cloze-hints-present ()
  "Hints are extracted from content with :: separator."
  (let ((result (gnosis-cloze-extract-hints
                 '(("Penicillin::antibiotic")))))
    (should (equal result '(("antibiotic"))))))

(ert-deftest gnosis-test-cloze-hints-absent ()
  "Content without hints yields nil for that position."
  (let ((result (gnosis-cloze-extract-hints '(("Penicillin")))))
    (should (equal result '((nil))))))

(ert-deftest gnosis-test-cloze-hints-mc-options ()
  "MC-style hint (rapid or slow) is extracted as a single string."
  (let ((result (gnosis-cloze-extract-hints
                 '(("rapid::rapid or slow")))))
    (should (equal result '(("rapid or slow"))))))

(ert-deftest gnosis-test-cloze-hints-mixed ()
  "Groups with and without hints are handled correctly."
  (let ((result (gnosis-cloze-extract-hints
                 '(("drug::type") ("infection")))))
    (should (equal (nth 0 result) '("type")))
    (should (equal (nth 1 result) '(nil)))))

(ert-deftest gnosis-test-cloze-hints-same-group ()
  "Multiple clozes in same group each get their own hint."
  (let ((result (gnosis-cloze-extract-hints
                 '(("cleft lip::face" "cleft palate::face")))))
    (should (equal result '(("face" "face"))))))

;; ──────────────────────────────────────────────────────────
;; gnosis-cloze-remove-tags
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-cloze-remove-double-brace ()
  "Double-brace cloze tags are removed, leaving content."
  (should (string= (gnosis-cloze-remove-tags "{{c1::Penicillin}}")
                   "Penicillin")))

(ert-deftest gnosis-test-cloze-remove-single-brace ()
  "Single-brace cloze tags are removed."
  (should (string= (gnosis-cloze-remove-tags "{c1:drug}")
                   "drug")))

(ert-deftest gnosis-test-cloze-remove-with-hint ()
  "Cloze tags with hints are removed — only content remains, hint dropped."
  (should (string= (gnosis-cloze-remove-tags "{{c1::Penicillin::antibiotic}}")
                   "Penicillin")))

(ert-deftest gnosis-test-cloze-remove-mc-hint ()
  "MC-style hint is also removed."
  (should (string= (gnosis-cloze-remove-tags
                    "{{c1::rapid::rapid or slow}}-onset food poisoning")
                   "rapid-onset food poisoning")))

(ert-deftest gnosis-test-cloze-remove-multiple ()
  "Multiple cloze tags in a sentence are all removed."
  (should (string= (gnosis-cloze-remove-tags
                    "{{c1::drug::type}} treats {{c2::infections}}")
                   "drug treats infections")))

(ert-deftest gnosis-test-cloze-remove-preserves-plain-text ()
  "Text without cloze tags is returned unchanged."
  (should (string= (gnosis-cloze-remove-tags "No clozes here")
                   "No clozes here")))

(ert-deftest gnosis-test-cloze-remove-same-group-multiple ()
  "Multiple clozes in same group with hints are all cleaned."
  (should (string= (gnosis-cloze-remove-tags
                    "{{c2::cleft lip::face}} and {{c2::cleft palate::face}}")
                   "cleft lip and cleft palate")))

;; ──────────────────────────────────────────────────────────
;; gnosis-cloze-check
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-cloze-check-found ()
  "Cloze check returns t when all answers are in the sentence."
  (should (gnosis-cloze-check "Penicillin treats infections"
                              '("Penicillin"))))

(ert-deftest gnosis-test-cloze-check-multiple-found ()
  "Cloze check with multiple answers all present."
  (should (gnosis-cloze-check "Penicillin treats bacterial infections"
                              '("Penicillin" "infections"))))

(ert-deftest gnosis-test-cloze-check-missing ()
  "Cloze check returns nil when an answer is not in the sentence."
  (should-not (gnosis-cloze-check "Penicillin treats infections"
                                  '("Amoxicillin"))))

(ert-deftest gnosis-test-cloze-check-nil-answer ()
  "Cloze check with nil answer list returns t (vacuously true)."
  (should (gnosis-cloze-check "Anything" nil)))

;; ──────────────────────────────────────────────────────────
;; Full pipeline: extract → answers/hints from Anki syntax
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-cloze-pipeline-anki-simple ()
  "Full pipeline: Anki-like {{c1::answer}} without hint."
  (let* ((text "The {{c1::mitochondria}} is the powerhouse of the cell")
         (contents (gnosis-cloze-extract-contents text))
         (answers (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (clean (gnosis-cloze-remove-tags text)))
    (should (equal answers '(("mitochondria"))))
    (should (equal hints '((nil))))
    (should (string= clean "The mitochondria is the powerhouse of the cell"))
    (should (gnosis-cloze-check clean (car answers)))))

(ert-deftest gnosis-test-cloze-pipeline-anki-with-hint ()
  "Full pipeline: Anki-like {{c1::answer::hint}}."
  (let* ((text "{{c1::Penicillin::antibiotic}} is a beta-lactam")
         (contents (gnosis-cloze-extract-contents text))
         (answers (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (clean (gnosis-cloze-remove-tags text)))
    (should (equal answers '(("Penicillin"))))
    (should (equal hints '(("antibiotic"))))
    (should (string= clean "Penicillin is a beta-lactam"))
    (should (gnosis-cloze-check clean (car answers)))))

(ert-deftest gnosis-test-cloze-pipeline-anki-mc-options ()
  "Full pipeline: hint with multiple choice options."
  (let* ((text "{{c1::rapid::rapid or slow}}-onset food poisoning")
         (contents (gnosis-cloze-extract-contents text))
         (answers (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (clean (gnosis-cloze-remove-tags text)))
    (should (equal answers '(("rapid"))))
    (should (equal hints '(("rapid or slow"))))
    (should (string= clean "rapid-onset food poisoning"))
    (should (gnosis-cloze-check clean (car answers)))))

(ert-deftest gnosis-test-cloze-pipeline-multi-group ()
  "Full pipeline: two cX groups, one with hint."
  (let* ((text "{{c1::Enterotoxins::Which exotoxin}} from {{c2::Staph aureus}}")
         (contents (gnosis-cloze-extract-contents text))
         (answers (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (clean (gnosis-cloze-remove-tags text)))
    (should (= (length answers) 2))
    (should (equal (nth 0 answers) '("Enterotoxins")))
    (should (equal (nth 1 answers) '("Staph aureus")))
    (should (equal (nth 0 hints) '("Which exotoxin")))
    (should (equal (nth 1 hints) '(nil)))
    (should (string= clean "Enterotoxins from Staph aureus"))
    (should (gnosis-cloze-check clean (nth 0 answers)))
    (should (gnosis-cloze-check clean (nth 1 answers)))))

(ert-deftest gnosis-test-cloze-pipeline-same-group-with-hints ()
  "Full pipeline: same cX group, multiple clozes each with hint."
  (let* ((text "associated with {{c2::cleft lip::face}} and {{c2::cleft palate::face}}")
         (contents (gnosis-cloze-extract-contents text))
         (answers (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (clean (gnosis-cloze-remove-tags text)))
    (should (= (length answers) 1))
    (should (equal (car answers) '("cleft lip" "cleft palate")))
    (should (equal (car hints) '("face" "face")))
    (should (string= clean "associated with cleft lip and cleft palate"))
    (should (gnosis-cloze-check clean (car answers)))))

;; ──────────────────────────────────────────────────────────
;; Import: cloze with Anki syntax + nil answer → auto-extract
;; ──────────────────────────────────────────────────────────

(defvar gnosis-test--db-file nil)

(defmacro gnosis-test-with-db (&rest body)
  "Run BODY with a fresh temporary gnosis database."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-" nil ".db"))
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

(ert-deftest gnosis-test-import-cloze-anki-syntax ()
  "Importing a cloze thema with {{cN::answer::hint}} and nil answer auto-extracts."
  (gnosis-test-with-db
    (let* ((deck-id (let ((id (+ (random 90000) 10000)))
                      (gnosis--insert-into 'decks `([,id "anki-test"]))
                      id))
           (export-file (concat (make-temp-file "gnosis-cloze-import-") ".org")))
      (unwind-protect
          (progn
            ;; Write an org file with Anki-like cloze syntax and empty answer
            (with-temp-file export-file
              (insert "#+DECK: anki-test\n")
              (insert "#+THEMATA: 1\n\n")
              (insert "* Thema :test:\n")
              (insert ":PROPERTIES:\n")
              (insert ":GNOSIS_ID: NEW\n")
              (insert ":GNOSIS_TYPE: cloze\n")
              (insert ":END:\n")
              (insert "** Keimenon\n")
              (insert "{{c1::Penicillin::antibiotic}} is a beta-lactam\n\n")
              (insert "** Hypothesis\n\n")
              (insert "** Answer\n\n")
              (insert "** Parathema\n")
              (insert "A common drug.\n"))
            ;; Import (stub y-or-n-p for existing deck prompt)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
              (gnosis-import-deck export-file))
            ;; Should have created 1 thema (1 cX group)
            (let ((themata (gnosis-select 'id 'themata nil t)))
              (should (= 1 (length themata))))
            ;; Answer should be extracted as ("Penicillin")
            (let* ((id (car (gnosis-select 'id 'themata nil t)))
                   (answer (gnosis-get 'answer 'themata `(= id ,id)))
                   (keimenon (gnosis-get 'keimenon 'themata `(= id ,id))))
              (should (member "Penicillin" answer))
              ;; Keimenon should have tags removed
              (should (string= keimenon "Penicillin is a beta-lactam"))
              ;; Hint should be extracted
              (let ((hypothesis (gnosis-get 'hypothesis 'themata `(= id ,id))))
                (should (member "antibiotic" hypothesis)))))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

(ert-deftest gnosis-test-import-cloze-anki-multi-group ()
  "Importing cloze with multiple cX groups creates multiple themata."
  (gnosis-test-with-db
    (let* ((deck-id (let ((id (+ (random 90000) 10000)))
                      (gnosis--insert-into 'decks `([,id "multi-cloze"]))
                      id))
           (export-file (concat (make-temp-file "gnosis-multi-cloze-") ".org")))
      (unwind-protect
          (progn
            (with-temp-file export-file
              (insert "#+DECK: multi-cloze\n")
              (insert "#+THEMATA: 1\n\n")
              (insert "* Thema :test:\n")
              (insert ":PROPERTIES:\n")
              (insert ":GNOSIS_ID: NEW\n")
              (insert ":GNOSIS_TYPE: cloze\n")
              (insert ":END:\n")
              (insert "** Keimenon\n")
              (insert "{{c1::Toxin}} from {{c2::Staph aureus}} causes disease\n\n")
              (insert "** Hypothesis\n\n")
              (insert "** Answer\n\n")
              (insert "** Parathema\n"))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
              (gnosis-import-deck export-file))
            ;; Should create 2 themata (c1 and c2)
            (let ((themata (gnosis-select 'id 'themata nil t)))
              (should (= 2 (length themata))))
            ;; Both should share the same clean keimenon
            (let ((all-keimenon (gnosis-select 'keimenon 'themata nil t)))
              (should (cl-every (lambda (k)
                                  (string= k "Toxin from Staph aureus causes disease"))
                                all-keimenon))))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

;; ──────────────────────────────────────────────────────────
;; Chunked import helpers
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-split-chunks-basic ()
  "Splitting text into chunks groups headings correctly."
  (let* ((text (concat "#+DECK: test\n\n"
                       "* Thema\nA\n* Thema\nB\n* Thema\nC\n"
                       "* Thema\nD\n* Thema\nE\n"))
         (chunks (gnosis--import-split-chunks text 2)))
    (should (= 3 (length chunks)))
    ;; First chunk has 2 headings
    (should (= 2 (with-temp-buffer
                   (insert (nth 0 chunks))
                   (count-matches "^\\* Thema" (point-min) (point-max)))))
    ;; Last chunk has 1 heading
    (should (= 1 (with-temp-buffer
                   (insert (nth 2 chunks))
                   (count-matches "^\\* Thema" (point-min) (point-max)))))))

(ert-deftest gnosis-test-split-chunks-single ()
  "All entries in one chunk when chunk-size >= total."
  (let* ((text "* Thema\nA\n* Thema\nB\n")
         (chunks (gnosis--import-split-chunks text 100)))
    (should (= 1 (length chunks)))))

(ert-deftest gnosis-test-import-chunk-basic ()
  "gnosis--import-chunk processes a chunk and inserts themata."
  (gnosis-test-with-db
    (let* ((deck-id (let ((id (+ (random 90000) 10000)))
                      (gnosis--insert-into 'decks `([,id "chunk-test"]))
                      id))
           (id-cache (make-hash-table :test 'equal))
           (header "#+DECK: chunk-test")
           (chunk (concat "* Thema\n"
                          ":PROPERTIES:\n"
                          ":GNOSIS_ID: NEW\n"
                          ":GNOSIS_TYPE: basic\n"
                          ":END:\n"
                          "** Keimenon\nWhat is 2+2?\n\n"
                          "** Hypothesis\n\n"
                          "** Answer\n- 4\n\n"
                          "** Parathema\n\n"
                          "* Thema\n"
                          ":PROPERTIES:\n"
                          ":GNOSIS_ID: NEW\n"
                          ":GNOSIS_TYPE: cloze\n"
                          ":END:\n"
                          "** Keimenon\n{{c1::Emacs}} is a text editor\n\n"
                          "** Hypothesis\n\n"
                          "** Answer\n\n"
                          "** Parathema\n"))
           (errors (gnosis--import-chunk header chunk deck-id id-cache)))
      (should (null errors))
      ;; Basic thema + cloze thema (1 cX group) = 2 themata
      (should (= 2 (length (gnosis-select 'id 'themata nil t))))
      ;; IDs should be registered in cache
      (should (= 2 (hash-table-count id-cache))))))

(ert-run-tests-batch-and-exit)
