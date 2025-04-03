;;; gnosis-test-auto.el --- Gnosis Auto tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Testing module for gnosis.

;; Before making any push on master we should be passing the following
;; tests.

;;; Code:
(require 'ert)
(require 'gnosis)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

(ert-deftest gnosis-test-algorithm-next-interval-proto ()
  "Test next interval for proto values."
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 1.3
						 :success t
						 :successful-reviews 0
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 0
						 :lethe 3)
		 (gnosis-algorithm-date 1)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 1.3
						 :success t
						 :successful-reviews 1
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 0
						 :lethe 3)
		 (gnosis-algorithm-date 2)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 1.3
						 :success t
						 :successful-reviews 2
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 0
						 :lethe 3)
		 (gnosis-algorithm-date 3)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 1.3
						 :success t
						 :successful-reviews 3
						 :amnesia 0.5
						 :proto '(1 2 3 70)
						 :c-fails 0
						 :lethe 3)
		 (gnosis-algorithm-date 70)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 2.0
						 :success t
						 :successful-reviews 4
						 :amnesia 0.5
						 :proto '(1 2 3 70)
						 :c-fails 0
						 :lethe 3)
		 (gnosis-algorithm-date 2)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 3.0
						 :success t
						 :successful-reviews 5
						 :amnesia 0.5
						 :proto '(1 2 3 70)
						 :c-fails 0
						 :lethe 3)
		 (gnosis-algorithm-date 3))))

(ert-deftest gnosis-test-algorithm-next-interval-lethe ()
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 0
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 3)
		 (gnosis-algorithm-date)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 0
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 0
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 20.0
						 :success nil
						 :successful-reviews 2
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date 5)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 20.0
						 :success nil
						 :successful-reviews 2
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 5
						 :lethe 4)
		 (gnosis-algorithm-date))))

(ert-deftest gnosis-test-algorithm-next-interval-success ()
  "Test next interval for successful non-proto recalls."
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 2.0
						 :success t
						 :successful-reviews 5
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 500
						 :lethe 4)
		 (gnosis-algorithm-date 20)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 3
						 :gnosis-synolon 1.3
						 :success t
						 :successful-reviews 5
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 300
						 :lethe 4)
		 (gnosis-algorithm-date 4))))

(ert-deftest gnosis-test-algorithm-next-interval-amnesia ()
  "Test next interval for failed non-proto recalls."
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 3
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date 5)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 3
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 3
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date 2)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 2
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 3
						 :amnesia 0.5
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date 1)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 3
						 :amnesia 0.7
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date 3)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 3
						 :amnesia 0.8
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date 2)))
  (should (equal (gnosis-algorithm-next-interval :last-interval 10
						 :gnosis-synolon 1.3
						 :success nil
						 :successful-reviews 3
						 :amnesia 1.0
						 :proto '(1 2 3)
						 :c-fails 3
						 :lethe 4)
		 (gnosis-algorithm-date))))

(ert-deftest gnosis-test-algorithm-next-gnosis-synolon ()
  "Test algorithm for gnosis synolon (totalis)."
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.35 0.30 1.30)
		  :success t
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 1
		  :c-failures 0)
		 '(0.35 0.30 1.65)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 1.30)
		  :success t
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 1
		  :c-failures 0)
		 '(0.45 0.30 1.75)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 1
		  :c-failures 0)
		 '(0.45 0.30 1.70)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success nil
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 1
		  :c-failures 0)
		 '(0.45 0.30 3.2))))

(ert-deftest gnosis-test-algorithm-test-epignosis ()
  "Test epignosis during anagnosis events."
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success t
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 3
		  :c-failures 0)
		 '(0.75 0.30 3.95)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success t
		  :epignosis 0.2
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 3
		  :c-failures 0)
		 '(0.65 0.30 3.95)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success t
		  :epignosis 0.2
		  :agnoia 0.2
		  :anagnosis 4
		  :c-successes 3
		  :c-failures 0)
		 '(0.45 0.30 3.95))))

(ert-deftest gnosis-test-algorithm-test-agnoia ()
  "Test epignosis during anagnosis events."
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success nil
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3)
		 '(0.45 0.5 3.2)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success nil
		  :epignosis 0.3
		  :agnoia 0.3
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3)
		 '(0.45 0.6 3.2)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 3.5)
		  :success nil
		  :epignosis 0.3
		  :agnoia 0.3
		  :anagnosis 4
		  :c-successes 0
		  :c-failures 3)
		 '(0.45 0.3 3.2))))

(ert-deftest gnosis-test-get-note-deck-value ()
  "Test recovery of deck amnesia values."
  (let ((test-values '((:deck "demo"
			      (:proto (0 1 3) :anagnosis 3 :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 3))
		       (:deck "demo2"
			      (:proto (0 1 3)))
		       (:tag "demo"
			     (:proto (1 2) :anagnosis 2 :epignosis 0.2 :agnoia 0.3 :amnesia 0.4 :lethe 4)))))
    (should (= (gnosis-get-custom-deck-value "demo" :amnesia test-values) 0.5))
    (should (= (gnosis-get-note-deck-epignosis nil "demo" test-values) 0.5))
    (should (= (gnosis-get-note-deck-amnesia nil "demo"  test-values) 0.5))
    (should (= (gnosis-get-note-deck-agnoia nil "demo"  test-values) 0.3))
    (should (= (gnosis-get-note-deck-anagnosis nil "demo" test-values) 3))
    (should (= (gnosis-get-note-deck-lethe nil "demo" test-values) 3))
    (should (= (gnosis-get-note-deck-lethe nil "demo" test-values) 3))
    (should (= (gnosis-get-note-deck-lethe nil "demo2" test-values) gnosis-algorithm-lethe-value))
    (should (= (gnosis-get-note-deck-anagnosis nil "demo2" test-values) gnosis-algorithm-anagnosis-value))
    (should (= (gnosis-get-note-deck-epignosis nil "demo2" test-values) gnosis-algorithm-epignosis-value))
    (should (= (gnosis-get-note-deck-agnoia nil "demo2" test-values) gnosis-algorithm-agnoia-value))
    (should (= (gnosis-get-note-deck-amnesia nil "demo2" test-values) gnosis-algorithm-amnesia-value))))

(ert-deftest gnosis-test-get-custom-tag-amnesia ()
  "Test recovery of tag amnesia values."
  (let ((test-values '((:deck "tag1" (:proto (99 99 99) :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 3))
		       (:tag "tag1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag2" (:proto (1 2) :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 4))
		       (:tag "tag3" (:proto (2 4 10) :epignosis 0.5 :agnoia 0.5 :amnesia 0.9 :lethe 2)))))
    (should (equal (gnosis-get-custom-tag-values nil :amnesia '("tag1") test-values) (list 0.3)))
    (should (equal (gnosis-get-note-tag-amnesia nil '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-note-tag-amnesia nil '("tag1" "tag2") test-values) 0.5))
    (should (equal (gnosis-get-note-tag-amnesia nil '("tag1" "tag2" "tag3") test-values) 0.9))
    (should (equal (gnosis-get-note-tag-amnesia nil '("tag2" "tag1") test-values) 0.5))))

(ert-deftest gnosis-test-get-proto ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag1" (:epignosis 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.5))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.5)))))
    (should (equal (gnosis-get-note-proto nil '("tag1") "deck1" test-values) '(0 1 3)))
    (should (equal (gnosis-get-note-proto nil '("tag1" "tag2") "deck1" test-values) '(2 2 2)))
    (should (equal (gnosis-get-note-proto nil '("tag1" "tag2" "tag3") "deck1" test-values) '(2 2 2 1)))))

(ert-deftest gnosis-test-get-note-amnesia ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag1" (:proto (10 1) :epignosis 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.5 :amnesia 0.2))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.5 :amnesia 0.6)))))
    (should (equal (gnosis-get-note-amnesia nil "deck1" '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-note-amnesia nil "deck1" '("tag1" "tag2") test-values) 0.2))
    (should (equal (gnosis-get-note-amnesia nil "deck1" '("tag1" "tag3") test-values) 0.6))
    (should (equal (gnosis-get-note-amnesia nil "deck1" '("tag2" "tag3") test-values) 0.6))))

(ert-deftest gnosis-test-get-note-epginosis ()
  (let ((test-values'((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		      (:tag "tag1" (:proto (10 1) :amnesia 0.5))
		      (:tag "tag2" (:proto (2 2 2) :epignosis 0.6 :amnesia 0.2))
		      (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.7 :amnesia 0.4)))))
    (should (equal (gnosis-get-note-epignosis nil "deck1" '("tag1") test-values) 0.5))
    (should (equal (gnosis-get-note-epignosis nil "deck1" '("tag1" "tag2") test-values) 0.6))
    (should (equal (gnosis-get-note-epignosis nil "deck1" '("tag2" "tag3") test-values) 0.7))))

(ert-deftest gnosis-test-get-note-agnoia ()
  (let ((test-values'((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		      (:tag "tag1" (:proto (10 1) :epignosis 0.4 :amnesia 0.5))
		      (:tag "tag2" (:proto (2 2 2) :epignosis 0.6 :amnesia 0.2 :agnoia 0.4))
		      (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.7 :amnesia 0.4 :agnoia 0.5)))))
    (should (equal (gnosis-get-note-agnoia nil "deck1" '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-note-agnoia nil "deck1" '("tag1" "tag2") test-values) 0.4))
    (should (equal (gnosis-get-note-agnoia nil "deck1" '("tag1" "tag2" "tag3") test-values) 0.5))))

(ert-deftest gnosis-test-get-note-anagnosis ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :anagnosis 3 :amnesia 0.3 :lethe 3))
		       (:deck "deck2" (:anagnosis 1 :amnesia 0.3 :lethe 3))
		       (:tag "tag1" (:proto (10 1)))
		       (:tag "tag2" (:proto (2 2 2) :amnesia 0.2 :agnoia 0.4 :anagnosis 2))
		       (:tag "tag3" (:proto (1 1 1 1) :amnesia 0.3)))))
    (should (equal (gnosis-get-note-anagnosis nil "deck1" '("tag1") test-values) 3))
    (should (equal (gnosis-get-note-anagnosis nil "deck1" '("tag1" "tag2") test-values) 2))
    (should (equal (gnosis-get-note-anagnosis nil "deck2" '("tag1" "tag2") test-values) 2))))

(ert-deftest gnosis-test-get-note-lethe ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :anagnosis 3 :amnesia 0.3 :lethe 3))
		       (:deck "deck2" (:anagnosis 1 :lethe 9))
		       (:tag "tag1" (:proto (10 1) :lethe nil))
		       (:tag "tag2" (:proto (2 2 2) :lethe 2))
		       (:tag "tag3" (:proto (1 1 1 1) :amnesia 0.3 :lethe 1)))))
    (should (equal (gnosis-get-note-lethe nil "deck1" '("tag1") test-values) 3))
    (should (equal (gnosis-get-note-lethe nil "deck1" '("tag2") test-values) 2))
    (should (equal (gnosis-get-note-lethe nil "deck2" '("tag3" "tag2") test-values) 1))
    (should (equal (gnosis-get-note-lethe nil "deck2" '("tag1" "tag2") test-values) 2))))


(ert-run-tests-batch-and-exit)
