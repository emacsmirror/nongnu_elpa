;;; gnosis-algorithm.el --- Spaced Repetition Algorithm for Gnosis  -*- lexical-binding: t; -*-

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

;; Work in progress

;;; Code:

(require 'cl-lib)
(require 'calendar)

(defcustom gnosis-algorithm-interval '(1 3)
  "Gnosis initial interval.

Interval by which a new question is displayed or when it's ef is at 1.3.

First item: First interval
Second item: Second interval."
  :group 'gnosis
  :type 'list)

(defcustom gnosis-algorithm-ef '(0.3 0.3 1.3)
  "Gnosis easiness factor.

First item : Increase factor
Second item: Decrease factor
Third item : Starting ef"
  :group 'gnosis
  :type 'list)

(defcustom gnosis-algorithm-ff 0.5
  "Gnosis forgetting factor.

Used to calcuate new interval for failed questions."
  :group 'gnosis
  :type 'float)


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

(defun gnosis-algorithm-date-diff (date)
  "Find the difference between the current date and the given DATE.

DATE format must be given as (yyyy mm dd)
The structure of the given date is (YEAR MONTH DAY)."
  (let ((given-date (encode-time 0 0 0 (caddr date) (cadr date) (car date))))
    (- (time-to-days (current-time))
       (time-to-days given-date))))

(defun gnosis-algorithm-e-factor (ef quality)
  "Calculate new e-factor given existing EF and binary QUALITY, 0 or 1."
  (cond
   ((not (numberp quality))
    (error "Invalid argument passed to gnosis-algorithm-e-factor"))
   ((= quality 0) ;; If the quality score is 0 (fail), decrease the ef by a small penalty
    (max 1.3 (- ef (cadr gnosis-algorithm-ef))))
   ((= quality 1) ;; If the quality score is 1 (pass), increase the ef by a small reward
    (+ ef (car gnosis-algorithm-ef)))
   (t (error "Invalid quality score passed to gnosis-algorithm-e-factor"))))

(defun gnosis-algorithm-next-interval (last-interval n ef success ff)
  "Calculate next interval.
- LAST-INTERVAL : The number of days since the item was last reviewed.
- N : Number of times the item has been reviewed.
- EF : Easiness Factor.
- SUCCESS : Success of the recall, ranges from 0 (unsuccessful) to 1
  (successful).
- FF: Failure factor

Returns a tuple: (INTERVAL N EF) where,
- Next review date in (year month day) format.
- N : Incremented by 1.
- EF : Modified based on the recall success for the item."
  (cl-assert (and (>= success 0)
		  (<= success 1)))
  ;; Calculate the next easiness factor.
  (let* ((next-ef (gnosis-algorithm-e-factor ef success))
         ;; Calculate the next interval.
         (interval
          (cond
           ;; Immediately next day if it's the first time review.
           ((<= n 1) (car gnosis-algorithm-interval))
           ;; After 3 days if it's second review.
           ((= n 2) (cadr gnosis-algorithm-interval))
           ;; Increase last interval by 1 if recall was successful. Keep last interval if unsuccessful.
           (t (if (= success 1)
                  (* ef last-interval)
                (* ff last-interval))))))
    (list (gnosis-algorithm-date (round interval)) next-ef)))

(provide 'gnosis-algorithm)
;;; gnosis-algorithm.el ends here
