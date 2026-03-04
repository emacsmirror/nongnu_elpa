;;; gnosis-test-bulk-link.el --- Bulk link tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for gnosis-utils-replace-string-with-link and
;; gnosis-utils-string-outside-links-p.  All tested functions are
;; pure — no database needed.

;;; Code:

(require 'ert)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

(require 'gnosis-utils)

;; ──────────────────────────────────────────────────────────
;; gnosis-utils-string-outside-links-p
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-outside-links-plain-text ()
  "String found in plain text returns t."
  (should (gnosis-utils-string-outside-links-p "Emacs is great" "Emacs")))

(ert-deftest gnosis-test-outside-links-no-match ()
  "String not present returns nil."
  (should-not (gnosis-utils-string-outside-links-p "Emacs is great" "Vim")))

(ert-deftest gnosis-test-outside-links-only-in-desc ()
  "String only inside link description returns nil."
  (should-not (gnosis-utils-string-outside-links-p
               "Learn about [[id:abc][Emacs]] today"
               "Emacs")))

(ert-deftest gnosis-test-outside-links-only-in-target ()
  "String only inside link target returns nil."
  (should-not (gnosis-utils-string-outside-links-p
               "See [[id:emacs-node][editor]] for details"
               "emacs")))

(ert-deftest gnosis-test-outside-links-in-both ()
  "String in link and outside — returns t."
  (should (gnosis-utils-string-outside-links-p
           "Emacs: see [[id:abc][Emacs]] reference"
           "Emacs")))

(ert-deftest gnosis-test-outside-links-after-link ()
  "String after a link returns t."
  (should (gnosis-utils-string-outside-links-p
           "See [[id:abc][reference]] about Emacs"
           "Emacs")))

(ert-deftest gnosis-test-outside-links-between-links ()
  "String between two links returns t."
  (should (gnosis-utils-string-outside-links-p
           "[[id:a][foo]] Emacs [[id:b][bar]]"
           "Emacs")))

(ert-deftest gnosis-test-outside-links-bare-link ()
  "String inside bare [[target]] link returns nil."
  (should-not (gnosis-utils-string-outside-links-p
               "See [[Emacs]] for info"
               "Emacs")))

;; ──────────────────────────────────────────────────────────
;; gnosis-utils-replace-string-with-link — basic
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-replace-basic ()
  "Basic replacement in plain text."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Emacs is great" "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result) "[[id:node-1][Emacs]] is great"))))

(ert-deftest gnosis-test-replace-multiple-occurrences ()
  "All occurrences in plain text are replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Emacs and Emacs" "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "[[id:node-1][Emacs]] and [[id:node-1][Emacs]]"))))

(ert-deftest gnosis-test-replace-no-match ()
  "No occurrences returns MODIFIED-P nil."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Vim is great" "Emacs" "node-1")))
    (should-not (car result))
    (should (string= (cdr result) "Vim is great"))))

;; ──────────────────────────────────────────────────────────
;; gnosis-utils-replace-string-with-link — skipping links
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-replace-skip-link-desc ()
  "String inside link description is NOT replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Learn about [[id:abc][Emacs]] today" "Emacs" "node-1")))
    (should-not (car result))
    (should (string= (cdr result)
                     "Learn about [[id:abc][Emacs]] today"))))

(ert-deftest gnosis-test-replace-skip-link-target ()
  "String inside link target is NOT replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "See [[id:emacs-ref][editor]]" "emacs" "node-1")))
    (should-not (car result))
    (should (string= (cdr result) "See [[id:emacs-ref][editor]]"))))

(ert-deftest gnosis-test-replace-outside-but-not-inside ()
  "String outside link is replaced; inside link is preserved."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Emacs: see [[id:abc][Emacs]] reference" "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "[[id:node-1][Emacs]]: see [[id:abc][Emacs]] reference"))))

(ert-deftest gnosis-test-replace-after-link ()
  "String after a link is replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "See [[id:abc][reference]] about Emacs" "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "See [[id:abc][reference]] about [[id:node-1][Emacs]]"))))

(ert-deftest gnosis-test-replace-between-links ()
  "String between two links is replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "[[id:a][foo]] Emacs [[id:b][bar]]" "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "[[id:a][foo]] [[id:node-1][Emacs]] [[id:b][bar]]"))))

(ert-deftest gnosis-test-replace-skip-bare-link ()
  "String inside bare [[target]] link is NOT replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "See [[Emacs]] for info" "Emacs" "node-1")))
    (should-not (car result))
    (should (string= (cdr result) "See [[Emacs]] for info"))))

(ert-deftest gnosis-test-replace-multiple-links-and-text ()
  "Multiple links interspersed with replaceable text."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Emacs has [[id:a][Emacs Lisp]] and Emacs config"
                 "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "[[id:node-1][Emacs]] has [[id:a][Emacs Lisp]] and [[id:node-1][Emacs]] config"))))

;; ──────────────────────────────────────────────────────────
;; Edge cases
;; ──────────────────────────────────────────────────────────

(ert-deftest gnosis-test-replace-empty-text ()
  "Empty text returns unmodified."
  (let ((result (gnosis-utils-replace-string-with-link "" "Emacs" "node-1")))
    (should-not (car result))
    (should (string= (cdr result) ""))))

(ert-deftest gnosis-test-replace-text-is-only-links ()
  "Text consisting only of links with the search string — no changes."
  (let ((result (gnosis-utils-replace-string-with-link
                 "[[id:abc][Emacs]]" "Emacs" "node-1")))
    (should-not (car result))
    (should (string= (cdr result) "[[id:abc][Emacs]]"))))

(ert-deftest gnosis-test-replace-case-sensitive ()
  "Replacement is case-sensitive."
  (let ((result (gnosis-utils-replace-string-with-link
                 "emacs and Emacs" "Emacs" "node-1")))
    (should (car result))
    (should (string= (cdr result) "emacs and [[id:node-1][Emacs]]"))))

(ert-deftest gnosis-test-replace-regex-chars-in-string ()
  "Special regex characters in search string are handled safely."
  (let ((result (gnosis-utils-replace-string-with-link
                 "Use C-x C-f (find-file) to open" "(find-file)" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "Use C-x C-f [[id:node-1][(find-file)]] to open"))))

(ert-deftest gnosis-test-replace-adjacent-to-link ()
  "String immediately adjacent to a link is still replaced."
  (let ((result (gnosis-utils-replace-string-with-link
                 "foo[[id:a][bar]]foo" "foo" "node-1")))
    (should (car result))
    (should (string= (cdr result)
                     "[[id:node-1][foo]][[id:a][bar]][[id:node-1][foo]]"))))

(ert-run-tests-batch-and-exit)
