;;; qbank.el --- Learning tool for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/qbank
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

(cl-defun qbank--select (table values &optional (restrictions '1=1))
  "Select VALUES from TABLE, optionally with RESTRICTIONS."
  (emacsql-with-connection (db (emacsql-sqlite "test2.db"))
    (emacsql db `[:select ,values :from ,table :where ,restrictions])))

(cl-defun qbank--create-table (table-name &optional values)
  "Create TABLE-NAME for VALUES."
  (emacsql-with-connection (db (emacsql-sqlite "test2.db"))
    (emacsql db `[:create-table ,table-name ,values])))

(cl-defun qbank--insert-into (table-name values)
  "Insert VALUES to TABLE-NAME."
  (emacsql-with-connection (db (emacsql-sqlite "test2.db"))
    (emacsql db `[:insert :into ,table-name :values ,values])))

(defun qbank--get-question (id)
  "Get question row for question ID."
  (caar (qbank--select 'qbank1 'question `(= question_id ,id))))

(defun qbank--get-correct-answer (id)
  "Get correct answer for question ID."
  (caar (qbank--select 'qbank1 'answer `(= question_id ,id))))

(defun qbank--get-mcanswers (id)
  "Get multiple choices for question ID."
  (caar (qbank--select 'qbank1 'mchoices `(= question_id ,id))))

(defun qbank--display-question (id)
  "Display question for question ID."
  (let ((question (qbank--get-question id)))
    ;; Animate.el is used only for testing purposes.
    (animate-string question 5)))

(defun qbank--mcanswers-choice (id)
  "Display multiple choice answers for question ID."
  (let ((mcanswers (qbank--get-mcanswers id)))
    (completing-read "Answer: " mcanswers)))

(defun qbank--input-mcanswers ()
  "Prompt user for multiple choice answers."
  (let ((mcqs nil))
    (while (not (equal (car mcqs) "q"))
      (add-to-list 'mcqs (read-string "Choices (q for quit): ")))
    (when (equal (car mcqs) "q")
      (pop mcqs))
    (reverse mcqs)))


(defun qbank-create-mcq-question ()
  "Create question as MCQ type."
  (interactive)
  (let ((question (read-string "Question: "))
	(choices (unless (equal choices "q")
		   (read-string "Choices: ")))))
  (qbank--insert-into '))

(cl-defun qbank-create-mcq-question (&key question choices correct-answer)
  "Create a QUESTION with a list of multiple CHOICES and one CORRECT-ANSWER.

This function can be used interactively, or if you prefer you may also
use it like this:
 (qbank-create-mcq-question
  :question \"Which one is the greatest editor?\"
  :choices (list \"Emacs\" \"Vim\" \"VSCode\" \"Ed\")
  :correct-answer 1)"
  (interactive
   (list :question (read-string "Question: ")
         :choices (qbank-input-questions)
         :correct-answer (string-to-number (read-string "Which is the correct answer?"))))
  (qbank--insert-into 'qbank1 `([nil ,question ,choices ,correct-answer])))

;; Fix: review for seperate question types.
(defun qbank-review (id)
  "Start review for question ID."
  (let ((canswer (qbank--get-correct-answer id))
	(choices (qbank--get-mcanswers id))
	(user-choice (qbank--mcanswers-choice id)))
    (if (equal (nth (- canswer 1) choices) user-choice)
	(message "Correct!")
      (message "False"))))

(defun qbank-test-buffer ()
  "Create testing buffer."
  (interactive)
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*qbank*"))
    (qbank--display-question 13)
    (qbank-review 13)))
