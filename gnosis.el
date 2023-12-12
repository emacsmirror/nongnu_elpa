;;; gnosis.el --- Learning tool for GNU Emacs  -*- lexical-binding: t; -*-

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

;; TODO: Create cloze question type & make it easily extensible for
;; other types


(require 'emacsql)
(require 'cl-lib)
(require 'animate)

(defgroup gnosis nil
  "Spaced repetition learning tool."
  :group 'external
  :prefix "gnosis-")

(defvar gnosis-db (emacsql-sqlite (concat user-emacs-directory "gnosis.db")))

(cl-defun gnosis--select (table values &optional (restrictions '1=1))
  "Select VALUES from TABLE, optionally with RESTRICTIONS."
  (emacsql gnosis-db `[:select ,values :from ,table :where ,restrictions]))

(cl-defun gnosis--create-table (table-name &optional values)
  "Create TABLE-NAME for VALUES."
  (emacsql gnosis-db `[:create-table ,table-name ,values]))

(cl-defun gnosis--drop-table (table)
  "Drop TABLE from gnosis-db."
  (emacsql gnosis-db `[:drop-table ,table]))

(cl-defun gnosis--insert-into (table-name values)
  "Insert VALUES to TABLE-NAME."
  (emacsql gnosis-db `[:insert :into ,table-name :values ,values]))

(defun gnosis--get-id (table value id)
  "Get VALUE for question ID from TABLE."
  (caar (gnosis--select table value `(= id ,id))))

(defun gnosis--display-question (id)
  "Display main row for question ID."
  (let ((question (gnosist--get-id 'notes 'main id)))
    ;; Animate.el is used only for testing purposes.
    (animate-string question 5)))

(defun gnosis--ask-input (prompt)
  "PROMPT user for input until `q' is given.

The user is prompted to provide input for the 'PROMPT' message, and
the returns the list of inputs in reverse order."
  (let ((input nil))
    (while (not (equal (car input) "q"))
      (add-to-list 'input (read-string (concat prompt " (q for quit): "))))
    (when (equal (car input) "q")
      (pop input))
    (reverse input)))

(cl-defun gnosis-create-mcq-question (&key question choices correct-answer tags)
  "Create a QUESTION with a list of multiple CHOICES.

MCQ type questions consist of a main `QUESTION', which is displayed &
the user will be prompted to find the `CORRECT-ANSWER', which is the
correct number choice of `CHOICES'.

TAGS are used to organize questions.

This function can be used interactively, or if you prefer you may also
use it like this:
 (gnosis-create-mcq-question
  :question \"Which one is the greatest editor?\"
  :choices (list \"Emacs\" \"Vim\" \"VSCode\" \"Ed\")
  :correct-answer 1
  :tags (list \"emacs\" \"editors\"))"
  (interactive
   (list :question (read-string "Question: ")
         :choices (gnosis--ask-input "Choices")
         :correct-answer (string-to-number (read-string "Which is the correct answer (number)? "))
	 :tags (gnosis--ask-input "Tags")))
  (when (equal (numberp correct-answer))
    (error "The correct answer must be the number of the correct answer"))
  (gnosis--insert-into 'notes `([nil "mcq" ,question ,choices ,correct-answer ,tags, nil, nil nil])))

(defun gnosis-create-question (type)
  "Create question as TYPE."
  (interactive (list (completing-read "Type: " '(MCQ Cloze Basic))))
  (pcase type
    ("MCQ" (call-interactively 'gnosis-create-mcq-question))
    ("Cloze" (message "Not ready yet."))
    ("Basic" (message "Not ready yet."))
    (_ (message "No such type."))))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosist--get-id 'notes 'options id))
	(history-add-new-input nil)) ;; Disable history
    (completing-read "Answer: " choices)))

(defun gnosis-review-mcq-choices (id)
  "Display multiple choice answers for question ID."
  (let ((canswer (gnosist--get-id 'notes 'answer id))
	(choices (gnosist--get-id 'notes 'options id))
	(user-choice (gnosis-mcq-answer id)))
    (if (equal (nth (- canswer 1) choices) user-choice)
	(message "Correct!")
      (message "False"))))

(defun gnosis-review (id)
  "Start review for question ID."
  (let ((type (gnosist--get-id 'notes 'type id)))
    (pcase type
      ("mcq" (gnosis-review-mcq-choices id))
      ("basic" (message "Not Ready yet."))
      ("cloze" (message "Not Ready yet.")))))

;; testing
(defun gnosis-test-buffer ()
  "Create testing buffer."
  (interactive)
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*gnosis*"))
    (read-only-mode 0)
    (erase-buffer)
    (gnosis--display-question 1)
    (gnosis-review 1)
    (gnosis-mode)))

(defun gnosis-init ()
  "Create notes content table."
  (interactive)
  ;;(make-directory (concat user-emacs-directory "gnosis"))
  (condition-case nil
      (gnosis--drop-table 'notes)
    (error (message "No NOTES table to drop.")))
  (gnosis--create-table 'notes '([(id integer :primary-key)
				  (type text)
				  (main text)
				  options
				  answer
				  tags
				  (ef integer :default 1.3)
				  (rev_times integer :default 0)
				  (rev_interval integer :default 0)])))

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

(define-derived-mode gnosis-mode special-mode "Gnosis"
  "Gnosis Mode."
  :interactive t
  (display-line-numbers-mode 0)
  :lighter " gnosis-mode")

;; Gnosis Algorithm ;;
;;;;;;;;;;;;;;;;;;;;;;

(defcustom gnosis-interval '(1 3)
  "Gnosis algorithm interval.
- Interval by which a new question is displayed or when it's ef is at 1.3.
First item: First interval
Second item: Second interval.

e.g if you use:
 (setq gnosis-interval '(2 4))
Upon successfully recalling a question, it's next interval will be in
2 days. Recalling the same question successfully in 2 days, will put
it's interval at 4 days, afterwards it's next interval will be calculated using `gnosis-ef'."
  :group 'gnosis
  :type 'list)

(defcustom gnosis-ef '(0.2 0.2)
  "Gnosis easiness factor.

First item : Increase factor
Second item: Decrease factor"
  :group 'gnosis
  :type 'list)

(defcustom gnosis-ff 0.5
  "Gnosis forgetting factor.

Used to calcuate new interval for failed questions."
  :group 'gnosis
  :type 'float)

(defun gnosis-calculate-e-factor (ef quality)
  "Calculate new e-factor given existing EF and binary QUALITY, 0 or 1."
  (cond
   ((not (numberp quality))
    (error "Invalid argument passed to gnosis-calculate-e-factor"))
   ((= quality 0) ;; If the quality score is 0 (fail), decrease the ef by a small penalty
    (max 1.3 (- ef (cadr gnosis-ef))))
   ((= quality 1) ;; If the quality score is 1 (pass), increase the ef by a small reward
    (+ ef (car gnosis-ef)))
   (t (error "Invalid quality score passed to gnosis-calculate-e-factor"))))

(defun gnosis-calculate-next-interval (last-interval n ef success ff)
  "Calculate next interval.
- LAST-INTERVAL : The number of days since the item was last reviewed.
- N : Number of times the item has been reviewed.
- EF : The 'easiness factor'.
- SUCCESS : Success of the recall, ranges from 0 (unsuccessful) to 1
  (successful).
- FF: Failure factor

Returns a tuple: (INTERVAL N EF) where,
- INTERVAL : The number of days until the item should next be reviewed.
- N : Incremented by 1.
- EF : Modified based on the recall success for the item."
  ;; Ensure valid parameters.
  ;; (cl-assert (> n 0))
  (cl-assert (and (>= success 0)
		  (<= success 1)))
  ;; Calculate the next easiness factor.
  (let* ((next-ef (gnosis-calculate-e-factor ef success))
         ;; Calculate the next interval.
         (interval
          (cond
	   ;; If ef is 1.3, repeat question in the same day.
	   ((= ef 1.3) 0)
           ;; Immediately next day if it's the first time review.
           ((<= n 1) (car gnosis-interval))
           ;; After 3 days if it's second review.
           ((= n 2) (cadr gnosis-interval))
           ;; Increase last interval by 1 if recall was successful. Keep last interval if unsuccessful.
           (t (if (= success 1)
                  (* ef last-interval)
                (* ff last-interval))))))
    (list (round interval) (1+ n) next-ef)))


(provide 'gnosis)
;;; gnosis.el ends here
