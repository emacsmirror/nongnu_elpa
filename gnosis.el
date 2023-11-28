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

(defvar gnosis-db (emacsql-sqlite (concat user-emacs-directory "gnosis.db")))

(cl-defun gnosis--select (table values &optional (restrictions '1=1))
  "Select VALUES from TABLE, optionally with RESTRICTIONS."
  (emacsql gnosis-db `[:select ,values :from ,table :where ,restrictions]))

(cl-defun gnosis--create-table (table-name &optional values)
  "Create TABLE-NAME for VALUES."
  (emacsql gnosis-db `[:create-table ,table-name ,values]))

(cl-defun gnosis--insert-into (table-name values)
  "Insert VALUES to TABLE-NAME."
  (emacsql gnosis-db `[:insert :into ,table-name :values ,values]))

(defun gnosis--get-question (id)
  "Get question row for question ID."
  (caar (gnosis--select 'notes 'question `(= question_id ,id))))

(defun gnosis--get-correct-answer (id)
  "Get correct answer for question ID."
  (caar (gnosis--select 'notes 'answer `(= question_id ,id))))

(defun gnosis--get-mcanswers (id)
  "Get multiple choices for question ID."
  (caar (gnosis--select 'notes 'choices `(= question_id ,id))))

(defun gnosis--display-question (id)
  "Display question for question ID."
  (let ((question (gnosis--get-question id)))
    ;; Animate.el is used only for testing purposes.
    (animate-string question 5)))

(defun gnosis--mcanswers-choice (id)
  "Display multiple choice answers for question ID."
  (let ((mcanswers (gnosis--get-mcanswers id)))
    (completing-read "Answer: " mcanswers)))

(defun gnosis--input-mcanswers ()
  "Prompt user for multiple choice answers."
  (let ((mcqs nil))
    (while (not (equal (car mcqs) "q"))
      (add-to-list 'mcqs (read-string "Choices (q for quit): ")))
    (when (equal (car mcqs) "q")
      (pop mcqs))
    (reverse mcqs)))

(cl-defun gnosis-create-mcq-question (&key question choices correct-answer)
  "Create a QUESTION with a list of multiple CHOICES and one CORRECT-ANSWER.

This function can be used interactively, or if you prefer you may also
use it like this:
 (gnosis-create-mcq-question
  :question \"Which one is the greatest editor?\"
  :choices (list \"Emacs\" \"Vim\" \"VSCode\" \"Ed\")
  :correct-answer 1)"
  (interactive
   (list :question (read-string "Question: ")
         :choices (gnosis--input-mcanswers)
         :correct-answer (string-to-number (read-string "Which is the correct answer? "))))
  (gnosis--insert-into 'qbank1 `([nil ,question ,choices ,correct-answer])))

(defun gnosis-create-question (type)
  "Create question as TYPE."
  (interactive (list (completing-read "Type: " '(MCQ Cloze Basic))))
  (pcase type
    ("MCQ" (call-interactively 'gnosis-create-mcq-question))
    ("Cloze" (message "Not ready yet."))
    ("Basic" (message "Not ready yet."))
    (_ (message "No such type."))))

;; Fix: review for seperate question types.
(defun gnosis-review (id)
  "Start review for question ID."
  (let ((canswer (gnosis--get-correct-answer id))
	(choices (gnosis--get-mcanswers id))
	(user-choice (gnosis--mcanswers-choice id)))
    (if (equal (nth (- canswer 1) choices) user-choice)
	(message "Correct!")
      (message "False"))))

(defun gnosis-test-buffer ()
  "Create testing buffer."
  (interactive)
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*gnosis*"))
    (gnosis--display-question 13)
    (gnosis-review 13)))

;;; gnosis.el ends here
