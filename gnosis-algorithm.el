;;; gnosis-algorithm.el --- Spaced Repetition Algorithm for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

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

;; Module that handles date and interval calculation as well as
;; gnosis-score for gnosis.

;;; Code:

(require 'cl-lib)
(require 'calendar)

(defcustom gnosis-algorithm-proto '(0 1 2)
  "Gnosis proto interval for the first successful reviews.

Values for the first proto successful intervals.  There is no
restriction for list length."
  :group 'gnosis
  :type '(list integer))

(defcustom gnosis-algorithm-gnosis-value '(0.35 0.30 1.3)
  "Starting gnosis score.

First item : Increase value (gnosis-plus)
Second item: Decrease value (gnosis-minus)
Third item : Total gnosis (gnosis-synolon/totalis) -> Total gnosis score"
  :group 'gnosis
  :type '(list float))

(defcustom gnosis-algorithm-amnesia-value 0.5
  "Gnosis amnesia value.

Used to calcuate new interval upon a failed recall i.e the memmory loss.

The closer this value is to 1, the more the memory loss."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-epignosis-value 0.1
  "Value to increase gnosis-plus upon anagnosis.

Epignosis means knowledge accuracy."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-agnoia-value 0.2
  "Value to increase gnosis-minus upon anagnosis.

Agnoia refers to the lack of knowledge."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-anagnosis-value 3
  "Threshold value for anagnosis event.

Anagosis is the process recognition & understanding of a context/gnosis.

Anagnosis events update gnosis-plus & gnosis-minus values, depending
on the success or failure of recall."
  :group 'gnosis
  :type 'integer)

(defcustom gnosis-algorithm-lethe-value 2
  "Threshold value for hitting a lethe event.

Lethe is the process of being unable to recall a memory/gnosis.

On lethe events the next interval is set to 0."
  :group 'gnosis
  :type 'integer)

(defun gnosis-algorithm-replace-at-index (index new-item list)
  "Replace item at INDEX with NEW-ITEM in LIST."
  (cl-loop for item in list
	   for i from 0
	   collect (if (= i index) new-item item)))

(defun gnosis-algorithm-round-items (list)
  "Round all items in LIST to 2 decimal places."
  (cl-loop for item in list
	   collect (/ (round (* item 100)) 100.0)))

(defun gnosis-algorithm-date (&optional offset)
  "Return the current date in a list (year month day).
Optional integer OFFSET is a number of days from the current date."
  (cl-assert (or (integerp offset) (null offset)) nil "Date offset must be an integer or nil")
  (let* ((base-time (current-time))
         (target-time (if offset
                          (time-add base-time (days-to-time offset))
                        base-time))
         (decoded-time (decode-time target-time)))
    (list (decoded-time-year decoded-time)
          (decoded-time-month decoded-time)
          (decoded-time-day decoded-time))))

(defun gnosis-algorithm-date-diff (date &optional date2)
  "Find the difference between DATE2 and DATE.

If DATE2 is nil, current date will be used instead.

DATE format must be given as (year month day)."
  (let* ((given-date (encode-time 0 0 0 (caddr date) (cadr date) (car date)))
	 (date2 (if date2 (encode-time 0 0 0 (caddr date2) (cadr date2) (car date2))
		  (current-time)))
	 (diff (- (time-to-days date2)
		  (time-to-days given-date))))
    (if (>= diff 0) diff (error "`DATE2' must be higher than `DATE'"))))

(cl-defun gnosis-algorithm-next-gnosis (&key gnosis success epignosis agnoia anagnosis
					     c-successes c-failures)
  "Return the neo GNOSIS value. (gnosis-plus gnosis-minus gnsois-synolon)

Calculate the new e-factor given existing GNOSIS and SUCCESS, either t or nil.

Next GNOSIS is calculated as follows:

Upon a successful review, increase gnosis-synolon value (nth 2 gnosis) by
gnosis-plus value (nth 0 gnosis).

Upon a failed review, decrease gnosis-synolon by gnosis-minus value
 (nth 1 gnosis).

ANAGNOSIS is an event threshold, updating either the gnosis-plus or
gnosis-minus values.

When C-SUCCESSES (consecutive successes) reach ANAGNOSIS,
increase gnosis-plus by EPIGNOSIS.

When C-FAILURES reach ANAGOSNIS, increase gnosis-minus by AGNOIA."
  (cl-assert (listp gnosis) nil "Assertion failed: gnosis must be a list of floats.")
  (cl-assert (booleanp success) nil "Assertion failed: success must be a boolean value")
  (cl-assert (and (floatp epignosis) (< epignosis 1)) nil "Assertion failed: epignosis must be a float < 1")
  (cl-assert (and (floatp agnoia) (< agnoia 1)) nil "Assertion failed: agnoia must be a float < 1")
  (cl-assert (integerp anagnosis) nil "Assertion failed: anagosis must be an integer.")
  (let ((anagnosis-p (= (% (max 1 (if success c-successes c-failures)) anagnosis) 0))
	(neo-gnosis
	 (if success
	     (gnosis-algorithm-replace-at-index 2 (+ (nth 2 gnosis) (nth 0 gnosis)) gnosis)
	   (gnosis-algorithm-replace-at-index 2 (max 1.3 (- (nth 2 gnosis) (nth 1 gnosis))) gnosis))))
    ;; TODO: Change amnesia & epignosis value upon reaching a lethe or anagnosis event.
    (cond ((and success anagnosis-p)
	   (setf neo-gnosis (gnosis-algorithm-replace-at-index 0 (+ (nth 0 gnosis) epignosis) neo-gnosis)))
	  ((and (not success) anagnosis-p
		(setf neo-gnosis
		      (gnosis-algorithm-replace-at-index 1 (+ (nth 1 gnosis) agnoia) neo-gnosis)))))
    (gnosis-algorithm-round-items neo-gnosis)))

(cl-defun gnosis-algorithm-next-interval (&key last-interval gnosis-synolon success successful-reviews
					       amnesia proto c-fails lethe)
  "Calculate next interval.

LAST-INTERVAL: Number of days since last review

C-FAILS: Total consecutive failed reviews.

GNOSIS-SYNOLON: Current gnosis-synolon (gnosis totalis).

SUCCESS: non-nil when review was successful.

SUCCESSFUL-REVIEWS: Number of successful reviews.

AMNESIA: \"Forget\" value, used to calculate next interval upon failed
review.

PROTO: List of proto intervals, for successful reviews.
Until successfully completing proto reviews, for every failed attempt
the next interval will be set to 0.

LETHE: Upon having C-FAILS >= lethe, set next interval to 0."
  (cl-assert (booleanp success) nil "Success value must be a boolean")
  (cl-assert (integerp successful-reviews) nil "Successful-reviews must be an integer")
  (cl-assert (and (floatp amnesia) (<= amnesia 1)) nil "Amnesia must be a float <=1")
  (cl-assert (and (<= amnesia 1) (> amnesia 0)) nil "Value of amnesia must be a float <= 1")
  (cl-assert (and (integerp lethe) (>= lethe 1)) nil "Value of lethe must be an integer >= 1")
  ;; This should only occur in testing env or when the user has made breaking changes.
  (let* ((last-interval (if (<= last-interval 0) 1 last-interval)) ;; If last-interval is 0, use 1 instead.
	 (amnesia (- 1 amnesia)) ;; inverse amnesia
	 (interval (cond ((and (< successful-reviews (length proto))
			       success)
			  (nth successful-reviews proto))
			 ;; Lethe event, reset interval.
			 ((and (>= c-fails lethe)
			       (not success))
			  0)
			 (t (let* ((success-interval (* gnosis-synolon last-interval))
				   (failure-interval (* amnesia last-interval)))
			      (if success success-interval
				;; Make sure failure interval is never
				;; higher than success and at least 0
			        (max (min success-interval failure-interval) 0)))))))
    (gnosis-algorithm-date (round interval))))


(provide 'gnosis-algorithm)
;;; gnosis-algorithm.el ends here
