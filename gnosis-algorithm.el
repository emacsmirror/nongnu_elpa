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

;; Handles date calculation as well as ef & interval calculations.

;; Gnosis implements a highly customizable algorithm, inspired by SM-2.
;; Gnosis algorithm does not use user's subjective rating of a note to
;; determine the next review interval, but instead uses the user's
;; success or failure in recalling the answer of a note.

;; Each gnosis note has an ef (easiness factor), which is a list of 3
;; values.  The last value is the total ef for a note, which will be
;; used to determine the next interval upon a successful answer recall,
;; the second value is the ef-decrease value, this value will be
;; subtracted from the the total ef upon failure to recall the answer of
;; a note, the first value is the ef increase, will be added to the
;; total ef upon a successful recall.

;; Each gnosis deck has a gnosis-algorithm-ef-threshold, it's an
;; integer value that refers to the consecutive success or failures to
;; recall an answer.  Upon reaching the threshold, gnosis-algorithm-ef-decrease
;; or gnosis-algorithm-ef-increase will be applied to the ef-increase or
;; ef-decrease of note.

;;; Code:

(require 'cl-lib)
(require 'calendar)

(defcustom gnosis-algorithm-proto '(0 1 2)
  "Gnosis proto interval for the first successful reviews.

Values for the first proto successful intervals.  There is no
restriction for length."
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
  "Gnosis forgetting factor.

Used to calcuate new interval for failed questions.

This value should be less than 1.0."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-epignosis-value 0.1
  "Value to increase gnosis-plus upon anagnosis.

Epignosis means knowledge accuracy.."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-ef-decrease 0.2
  "Value to decrease ef decrease value with.

Decrease ef decrease value by this amount for every
`gnosis-algorithm-ef-threshold' number of failed reviews."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-ef-threshold 3
  "Threshold for updating ef increase/decrease values.

Refers to the number of consecutive successful or failed reviews."
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
  (let* ((now (decode-time))
         (now (list (decoded-time-month now)
                    (decoded-time-day now)
                    (decoded-time-year now))))
    (let ((date (if (zerop (or offset 0))
                    now
                  (calendar-gregorian-from-absolute
                   (+ offset (calendar-absolute-from-gregorian now))))))
      (list (nth 2 date) (nth 0 date) (nth 1 date)))))

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

(cl-defun gnosis-algorithm-next-ef (&key ef success increase decrease threshold
					 c-successes c-failures)
  "Return the new EF, (increase-value decrease-value total-value)

Calculate the new e-factor given existing EF and SUCCESS, either t or nil.

Next EF is calculated as follows:

Upon a successful review, increase total ef value (nth 2) by
ef-increase value (nth 0).

Upon a failed review, decrease total ef by ef-decrease value (nth 1).

For every THRESHOLD of C-SUCCESSES (consecutive successful reviews)
reviews, increase ef-increase by INCREASE.

For every THRESHOLD of C-FAILURES reviews, decrease ef-decrease value
by DECREASE."
  (cl-assert (listp ef) nil "Assertion failed: ef must be a list")
  (cl-assert (booleanp success) nil "Assertion failed: success must be a boolean value")
  (cl-assert (numberp increase) nil "Assertion failed: increase must be a number")
  (cl-assert (numberp decrease) nil "Assertion failed: decrease must be a number")
  (cl-assert (numberp threshold) nil "Assertion failed: threshold must be a number")
  (let ((threshold-p (= (% (max 1 (if success c-successes c-failures)) threshold) 0))
	(new-ef (if success (gnosis-algorithm-replace-at-index 2 (+ (nth 2 ef) (nth 0 ef)) ef)
		  (gnosis-algorithm-replace-at-index 2 (max 1.3 (- (nth 2 ef) (nth 1 ef))) ef))))
    (cond ((and success threshold-p)
	   (setf new-ef (gnosis-algorithm-replace-at-index 0 (+ (nth 0 ef) increase) new-ef)))
	  ((and (not success) threshold-p
		(setf new-ef (gnosis-algorithm-replace-at-index 1 (+ (nth 1 ef) decrease) new-ef)))))
    (gnosis-algorithm-round-items new-ef)))

(cl-defun gnosis-algorithm-next-interval (&key last-interval ef success successful-reviews
					       failure-factor initial-interval c-fails
					       threshold)
  "Calculate next interval.

LAST-INTERVAL: Number of days since last review

C-FAILS: Total consecutive failed reviews.

EF: Easiness factor

SUCCESS: non-nil when review was successful.

SUCCESSFUL-REVIEWS: Number of successful reviews.

FAILURE-FACTOR: Factor to multiply last interval by if review was unsuccessful.

INITIAL-INTERVAL: List of initial intervals for initial successful
reviews.  Will be used to determine the next interval for the first 2
successful reviews.  Until successfully completing INITIAL-INTERVAL reviews, for every failed attempt next interval will be set to 0.

THRESHOLD: Upon having C-FAILS >= threshold, set next interval to 0."
  (cl-assert (< gnosis-algorithm-ff 1) "Value of `gnosis-algorithm-ff' must be lower than 1")
  ;; This should only occur in testing env or when the user has made breaking changes.
  (let* ((last-interval (if (<= last-interval 0) 1 last-interval)) ;; If last-interval is 0, use 1 instead.
	 (interval (cond ((and (= successful-reviews 0) success)
			  (car initial-interval))
			 ((and (= successful-reviews 1) success)
			  (cadr initial-interval))
			 ;; If it's still on initial stage, review the
			 ;; same day
			 ((and (or (< successful-reviews (length initial-interval))
				   ;; reset threshold
				   (and threshold (>= c-fails threshold)))
			       (not success))
			  0)
			 (t (let* ((success-interval (* ef last-interval))
				   (failure-interval (* last-interval failure-factor)))
			      (if success success-interval
				;; Make sure failure interval is never
				;; higher than success
			        (min success-interval failure-interval)))))))
    (gnosis-algorithm-date (round interval))))


(provide 'gnosis-algorithm)
;;; gnosis-algorithm.el ends here
