;;; gnosis-algorithm.el --- Spaced Repetition Algorithm for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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

Used to calculate new interval upon a failed recall i.e the memory loss.

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

(defcustom gnosis-algorithm-synolon-max 3.0
  "Maximum value for gnosis-synolon.

Caps the interval multiplier to prevent excessively long intervals
for mature cards."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-interval-fuzz 0.1
  "Fuzz factor for interval calculation.

Adds random variation to prevent review clustering.
A value of 0.1 means +/- 10%%.  Set to 0 to disable."
  :group 'gnosis
  :type 'float)

(defun gnosis-algorithm-round-items (list)
  "Round all items in LIST to 2 decimal places."
  (cl-loop for item in list
	   collect (/ (round (* item 100)) 100.0)))

(defun gnosis-algorithm-fuzz-interval (interval)
  "Apply random fuzz to INTERVAL based on `gnosis-algorithm-interval-fuzz'.

Returns INTERVAL with +/- fuzz variation.  Returns unmodified for
intervals less than 2."
  (if (or (<= interval 2) (zerop gnosis-algorithm-interval-fuzz))
      interval
    (let ((fuzz (- (* 2 gnosis-algorithm-interval-fuzz (/ (random 1001) 1000.0))
		   gnosis-algorithm-interval-fuzz)))
      (* interval (1+ fuzz)))))

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
					     c-successes c-failures lethe)
  "Return the neo GNOSIS value. (gnosis-plus gnosis-minus gnosis-synolon)

Calculate the new e-factor given existing GNOSIS and SUCCESS, either t or nil.

Next GNOSIS is calculated as follows:

Upon a successful review, increase gnosis-synolon by gnosis-plus.

Upon a failed review, decrease gnosis-synolon by gnosis-minus.

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
  (let* ((g-plus (nth 0 gnosis))
	 (g-minus (nth 1 gnosis))
	 (g-synolon (nth 2 gnosis))
	 (anagnosis-p (= (% (max 1 (if success c-successes c-failures)) anagnosis) 0))
	 ;; Update synolon
	 (neo-synolon (if success
			  (min (+ g-synolon g-plus) gnosis-algorithm-synolon-max)
			(max 1.3 (- g-synolon g-minus))))
	 ;; Anagnosis event: adjust plus or minus
	 (neo-plus (if (and success anagnosis-p)
		       (+ g-plus epignosis)
		     g-plus))
	 (neo-minus (if (and (not success) anagnosis-p)
			(+ g-minus agnoia)
		      g-minus))
	 ;; Lethe event: reduce plus to slow future interval growth
	 (neo-plus (if (and lethe (not success) (>= c-failures lethe))
		       (max 0.1 (- neo-plus epignosis))
		     neo-plus)))
    (gnosis-algorithm-round-items (list neo-plus neo-minus neo-synolon))))

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
  ;; If last-interval is 0, use 1 instead, only for successful reviews.
  (let* ((last-interval (if (and (<= last-interval 0) success) 1 last-interval))
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
    (gnosis-algorithm-date (round (gnosis-algorithm-fuzz-interval interval)))))


(provide 'gnosis-algorithm)
;;; gnosis-algorithm.el ends here
