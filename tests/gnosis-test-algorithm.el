;;; gnosis-test-algorithm.el --- Gnosis Algorithm tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

;; Testing module for gnosis algorithm functions.

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
  (let ((gnosis-algorithm-interval-fuzz 0))
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
		 (gnosis-algorithm-date 3)))))

(ert-deftest gnosis-test-algorithm-next-interval-lethe ()
  (let ((gnosis-algorithm-interval-fuzz 0))
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
		 (gnosis-algorithm-date)))))

(ert-deftest gnosis-test-algorithm-next-interval-success ()
  "Test next interval for successful non-proto recalls."
  (let ((gnosis-algorithm-interval-fuzz 0))
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
		 (gnosis-algorithm-date 4)))))

(ert-deftest gnosis-test-algorithm-next-interval-amnesia ()
  "Test next interval for failed non-proto recalls."
  (let ((gnosis-algorithm-interval-fuzz 0))
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
		 (gnosis-algorithm-date)))))

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
  (let ((gnosis-algorithm-synolon-max 3.0))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.5)
		  :success t
		  :epignosis 0.3
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 3
		  :c-failures 0)
		 '(0.75 0.30 2.95)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.5)
		  :success t
		  :epignosis 0.2
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 3
		  :c-failures 0)
		 '(0.65 0.30 2.95)))
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.5)
		  :success t
		  :epignosis 0.2
		  :agnoia 0.2
		  :anagnosis 4
		  :c-successes 3
		  :c-failures 0)
		 '(0.45 0.30 2.95)))))

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

(ert-deftest gnosis-test-get-thema-deck-value ()
  "Test recovery of deck amnesia values."
  (let ((test-values '((:deck "demo"
			      (:proto (0 1 3) :anagnosis 3 :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 3))
		       (:deck "demo2"
			      (:proto (0 1 3)))
		       (:tag "demo"
			     (:proto (1 2) :anagnosis 2 :epignosis 0.2 :agnoia 0.3 :amnesia 0.4 :lethe 4)))))
    (should (= (gnosis-get-custom-deck-value "demo" :amnesia test-values) 0.5))
    (should (= (gnosis-get-thema-deck-epignosis nil "demo" test-values) 0.5))
    (should (= (gnosis-get-thema-deck-amnesia nil "demo"  test-values) 0.5))
    (should (= (gnosis-get-thema-deck-agnoia nil "demo"  test-values) 0.3))
    (should (= (gnosis-get-thema-deck-anagnosis nil "demo" test-values) 3))
    (should (= (gnosis-get-thema-deck-lethe nil "demo" test-values) 3))
    (should (= (gnosis-get-thema-deck-lethe nil "demo" test-values) 3))
    (should (= (gnosis-get-thema-deck-lethe nil "demo2" test-values) gnosis-algorithm-lethe-value))
    (should (= (gnosis-get-thema-deck-anagnosis nil "demo2" test-values) gnosis-algorithm-anagnosis-value))
    (should (= (gnosis-get-thema-deck-epignosis nil "demo2" test-values) gnosis-algorithm-epignosis-value))
    (should (= (gnosis-get-thema-deck-agnoia nil "demo2" test-values) gnosis-algorithm-agnoia-value))
    (should (= (gnosis-get-thema-deck-amnesia nil "demo2" test-values) gnosis-algorithm-amnesia-value))))

(ert-deftest gnosis-test-get-custom-tag-amnesia ()
  "Test recovery of tag amnesia values."
  (let ((test-values '((:deck "tag1" (:proto (99 99 99) :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 3))
		       (:tag "tag1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag2" (:proto (1 2) :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 4))
		       (:tag "tag3" (:proto (2 4 10) :epignosis 0.5 :agnoia 0.5 :amnesia 0.9 :lethe 2)))))
    (should (equal (gnosis-get-custom-tag-values nil :amnesia '("tag1") test-values) (list 0.3)))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag1" "tag2") test-values) 0.5))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag1" "tag2" "tag3") test-values) 0.9))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag2" "tag1") test-values) 0.5))))

(ert-deftest gnosis-test-get-proto ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag1" (:epignosis 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.5))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.5)))))
    (should (equal (gnosis-get-thema-proto nil '("tag1") "deck1" test-values) '(0 1 3)))
    (should (equal (gnosis-get-thema-proto nil '("tag1" "tag2") "deck1" test-values) '(2 2 2)))
    (should (equal (gnosis-get-thema-proto nil '("tag1" "tag2" "tag3") "deck1" test-values) '(2 2 2 1)))))

(ert-deftest gnosis-test-get-thema-amnesia ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag1" (:proto (10 1) :epignosis 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.5 :amnesia 0.2))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.5 :amnesia 0.6)))))
    (should (equal (gnosis-get-thema-amnesia nil "deck1" '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-thema-amnesia nil "deck1" '("tag1" "tag2") test-values) 0.2))
    (should (equal (gnosis-get-thema-amnesia nil "deck1" '("tag1" "tag3") test-values) 0.6))
    (should (equal (gnosis-get-thema-amnesia nil "deck1" '("tag2" "tag3") test-values) 0.6))))

(ert-deftest gnosis-test-get-thema-epginosis ()
  (let ((test-values'((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		      (:tag "tag1" (:proto (10 1) :amnesia 0.5))
		      (:tag "tag2" (:proto (2 2 2) :epignosis 0.6 :amnesia 0.2))
		      (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.7 :amnesia 0.4)))))
    (should (equal (gnosis-get-thema-epignosis nil "deck1" '("tag1") test-values) 0.5))
    (should (equal (gnosis-get-thema-epignosis nil "deck1" '("tag1" "tag2") test-values) 0.6))
    (should (equal (gnosis-get-thema-epignosis nil "deck1" '("tag2" "tag3") test-values) 0.7))))

(ert-deftest gnosis-test-get-thema-agnoia ()
  (let ((test-values'((:deck "deck1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		      (:tag "tag1" (:proto (10 1) :epignosis 0.4 :amnesia 0.5))
		      (:tag "tag2" (:proto (2 2 2) :epignosis 0.6 :amnesia 0.2 :agnoia 0.4))
		      (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.7 :amnesia 0.4 :agnoia 0.5)))))
    (should (equal (gnosis-get-thema-agnoia nil "deck1" '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-thema-agnoia nil "deck1" '("tag1" "tag2") test-values) 0.4))
    (should (equal (gnosis-get-thema-agnoia nil "deck1" '("tag1" "tag2" "tag3") test-values) 0.5))))

(ert-deftest gnosis-test-get-thema-anagnosis ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :anagnosis 3 :amnesia 0.3 :lethe 3))
		       (:deck "deck2" (:anagnosis 1 :amnesia 0.3 :lethe 3))
		       (:tag "tag1" (:proto (10 1)))
		       (:tag "tag2" (:proto (2 2 2) :amnesia 0.2 :agnoia 0.4 :anagnosis 2))
		       (:tag "tag3" (:proto (1 1 1 1) :amnesia 0.3)))))
    (should (equal (gnosis-get-thema-anagnosis nil "deck1" '("tag1") test-values) 3))
    (should (equal (gnosis-get-thema-anagnosis nil "deck1" '("tag1" "tag2") test-values) 2))
    (should (equal (gnosis-get-thema-anagnosis nil "deck2" '("tag1" "tag2") test-values) 2))))

(ert-deftest gnosis-test-get-thema-lethe ()
  (let ((test-values '((:deck "deck1" (:proto (0 1 3) :anagnosis 3 :amnesia 0.3 :lethe 3))
		       (:deck "deck2" (:anagnosis 1 :lethe 9))
		       (:tag "tag1" (:proto (10 1) :lethe nil))
		       (:tag "tag2" (:proto (2 2 2) :lethe 2))
		       (:tag "tag3" (:proto (1 1 1 1) :amnesia 0.3 :lethe 1)))))
    (should (equal (gnosis-get-thema-lethe nil "deck1" '("tag1") test-values) 3))
    (should (equal (gnosis-get-thema-lethe nil "deck1" '("tag2") test-values) 2))
    (should (equal (gnosis-get-thema-lethe nil "deck2" '("tag3" "tag2") test-values) 1))
    (should (equal (gnosis-get-thema-lethe nil "deck2" '("tag1" "tag2") test-values) 2))))


(ert-deftest gnosis-test-algorithm-synolon-cap ()
  "Test that gnosis-synolon is capped at `gnosis-algorithm-synolon-max'."
  (let ((gnosis-algorithm-synolon-max 3.0))
    ;; Success exceeding cap: 2.8 + 0.5 = 3.3 → capped to 3.0
    (should (equal (gnosis-algorithm-next-gnosis
		    :gnosis '(0.5 0.30 2.8)
		    :success t
		    :epignosis 0.1
		    :agnoia 0.2
		    :anagnosis 3
		    :c-successes 1
		    :c-failures 0)
		   '(0.5 0.30 3.0)))
    ;; Success under cap: 1.3 + 0.35 = 1.65
    (should (equal (gnosis-algorithm-next-gnosis
		    :gnosis '(0.35 0.30 1.3)
		    :success t
		    :epignosis 0.1
		    :agnoia 0.2
		    :anagnosis 3
		    :c-successes 1
		    :c-failures 0)
		   '(0.35 0.30 1.65)))
    ;; Failure path is not capped: 3.5 - 0.30 = 3.2
    (should (equal (gnosis-algorithm-next-gnosis
		    :gnosis '(0.45 0.30 3.5)
		    :success nil
		    :epignosis 0.1
		    :agnoia 0.2
		    :anagnosis 3
		    :c-successes 0
		    :c-failures 1)
		   '(0.45 0.30 3.2)))))

(ert-deftest gnosis-test-algorithm-lethe-gnosis ()
  "Test that lethe events reduce gnosis-plus."
  ;; Lethe triggers (c-failures >= lethe): gnosis-plus reduced by epignosis
  ;; Also anagnosis triggers: gnosis-minus increased by agnoia
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3
		  :lethe 3)
		 '(0.35 0.5 1.7)))
  ;; Lethe triggers but anagnosis does not (c-failures=4, anagnosis=3)
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 4
		  :lethe 4)
		 '(0.35 0.30 1.7)))
  ;; Lethe does not trigger (c-failures < lethe): gnosis-plus unchanged
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 2
		  :lethe 3)
		 '(0.45 0.30 1.7)))
  ;; Lethe with gnosis-plus floor: 0.15 - 0.1 = 0.05 → floored to 0.1
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.15 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3
		  :lethe 3)
		 '(0.1 0.5 1.7)))
  ;; No lethe param (backward compat): gnosis-plus unchanged
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 5)
		 '(0.45 0.30 1.7))))

(ert-deftest gnosis-test-algorithm-interval-fuzz ()
  "Test interval fuzzing behavior."
  ;; With fuzz disabled, result is exact
  (let ((gnosis-algorithm-interval-fuzz 0))
    (should (equal (gnosis-algorithm-next-interval :last-interval 10
						   :gnosis-synolon 2.0
						   :success t
						   :successful-reviews 5
						   :amnesia 0.5
						   :proto '(1 2 3)
						   :c-fails 0
						   :lethe 3)
		   (gnosis-algorithm-date 20))))
  ;; Intervals <= 2 are never fuzzed regardless of setting
  (let ((gnosis-algorithm-interval-fuzz 0.5))
    (should (= (gnosis-algorithm-fuzz-interval 1) 1))
    (should (= (gnosis-algorithm-fuzz-interval 2) 2))
    (should (= (gnosis-algorithm-fuzz-interval 0) 0)))
  ;; With fuzz enabled, result stays within expected range
  (let ((gnosis-algorithm-interval-fuzz 0.1)
	(results (cl-loop repeat 100
			  collect (gnosis-algorithm-fuzz-interval 20.0))))
    (should (cl-every (lambda (r) (and (>= r 18.0) (<= r 22.0))) results))
    ;; Not all identical (randomness works)
    (should (> (length (cl-remove-duplicates results)) 1))))

(ert-deftest gnosis-test-algorithm-proto-unaffected-by-events ()
  "Test that proto intervals are not affected by lethe, anagnosis, or fuzz."
  (let ((gnosis-algorithm-interval-fuzz 0))
    ;; Proto interval used even during lethe event (c-fails >= lethe)
    (should (equal (gnosis-algorithm-next-interval :last-interval 0
						   :gnosis-synolon 1.3
						   :success t
						   :successful-reviews 0
						   :amnesia 0.5
						   :proto '(1 2 3)
						   :c-fails 10
						   :lethe 3)
		   (gnosis-algorithm-date 1)))
    ;; Proto interval used even with high synolon
    (should (equal (gnosis-algorithm-next-interval :last-interval 0
						   :gnosis-synolon 100.0
						   :success t
						   :successful-reviews 1
						   :amnesia 0.5
						   :proto '(1 2 3)
						   :c-fails 0
						   :lethe 3)
		   (gnosis-algorithm-date 2)))
    ;; Failed during proto still gives interval 0 (not amnesia-based)
    (should (equal (gnosis-algorithm-next-interval :last-interval 5
						   :gnosis-synolon 2.0
						   :success nil
						   :successful-reviews 1
						   :amnesia 0.5
						   :proto '(1 2 3)
						   :c-fails 1
						   :lethe 3)
		   (gnosis-algorithm-date 2)))
    ;; Proto intervals not fuzzed even with high fuzz
    (let ((gnosis-algorithm-interval-fuzz 0.5))
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
		     (gnosis-algorithm-date 2))))))

(ert-deftest gnosis-test-algorithm-synolon-floor ()
  "Test that gnosis-synolon never drops below 1.3 on repeated failures."
  ;; Single failure near floor: 1.5 - 0.30 = 1.2 → floored to 1.3
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.35 0.30 1.5)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 1)
		 '(0.35 0.30 1.3)))
  ;; Already at floor: 1.3 - 0.30 = 1.0 → floored to 1.3
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.35 0.30 1.3)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 1)
		 '(0.35 0.30 1.3)))
  ;; Large gnosis-minus still respects floor: 1.4 - 0.8 = 0.6 → 1.3
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.35 0.80 1.4)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 1)
		 '(0.35 0.80 1.3)))
  ;; Repeated failures with anagnosis increasing gnosis-minus still floors
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.35 0.50 1.4)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3)
		 '(0.35 0.7 1.3))))

(ert-deftest gnosis-test-algorithm-lethe-anagnosis-interaction ()
  "Test combined lethe and anagnosis events on gnosis score."
  ;; Both lethe and anagnosis trigger simultaneously (c-failures=3, lethe=3, anagnosis=3):
  ;; anagnosis increases gnosis-minus, lethe decreases gnosis-plus
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3
		  :lethe 3)
		 '(0.35 0.5 1.7)))
  ;; Lethe at 6, anagnosis at 3: at c-failures=6 both trigger
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 6
		  :lethe 6)
		 '(0.35 0.5 1.7)))
  ;; Lethe triggers but anagnosis doesn't: only gnosis-plus reduced
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 5
		  :lethe 5)
		 '(0.35 0.30 1.7)))
  ;; Anagnosis triggers but lethe doesn't: only gnosis-minus increased
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.45 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3
		  :lethe 5)
		 '(0.45 0.5 1.7)))
  ;; Both trigger with gnosis-plus near floor: floor applies
  (should (equal (gnosis-algorithm-next-gnosis
		  :gnosis '(0.15 0.30 2.0)
		  :success nil
		  :epignosis 0.1
		  :agnoia 0.2
		  :anagnosis 3
		  :c-successes 0
		  :c-failures 3
		  :lethe 3)
		 '(0.1 0.5 1.7))))

(ert-deftest gnosis-test-algorithm-date ()
  "Test gnosis-algorithm-date returns correct format and offsets."
  ;; No offset returns today
  (let ((today (gnosis-algorithm-date)))
    (should (= (length today) 3))
    (should (integerp (nth 0 today)))  ; year
    (should (integerp (nth 1 today)))  ; month
    (should (integerp (nth 2 today)))) ; day
  ;; Offset 0 equals no offset
  (should (equal (gnosis-algorithm-date 0) (gnosis-algorithm-date)))
  ;; Positive offset is in the future
  (let ((today (gnosis-algorithm-date))
        (tomorrow (gnosis-algorithm-date 1)))
    (should (= (gnosis-algorithm-date-diff today tomorrow) 1)))
  ;; Negative offset is in the past
  (let ((yesterday (gnosis-algorithm-date -1))
        (today (gnosis-algorithm-date)))
    (should (= (gnosis-algorithm-date-diff yesterday today) 1)))
  ;; Larger offsets
  (let ((today (gnosis-algorithm-date))
        (future (gnosis-algorithm-date 30)))
    (should (= (gnosis-algorithm-date-diff today future) 30)))
  ;; Non-integer offset signals error
  (should-error (gnosis-algorithm-date 1.5))
  (should-error (gnosis-algorithm-date "3")))

(ert-deftest gnosis-test-algorithm-date-diff ()
  "Test gnosis-algorithm-date-diff calculations."
  ;; Same date returns 0
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 1 1)) 0))
  ;; Simple differences
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 1 2)) 1))
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 1 31)) 30))
  ;; Cross-month
  (should (= (gnosis-algorithm-date-diff '(2025 1 31) '(2025 2 1)) 1))
  ;; Cross-year
  (should (= (gnosis-algorithm-date-diff '(2024 12 31) '(2025 1 1)) 1))
  ;; Larger span
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 12 31)) 364))
  ;; date2 < date signals error
  (should-error (gnosis-algorithm-date-diff '(2025 1 2) '(2025 1 1))))

(ert-run-tests-batch-and-exit)
