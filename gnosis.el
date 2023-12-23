;;; gnosis.el --- Spaced Repetition Learning Tool  -*- lexical-binding: t; -*-

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
(require 'emacsql-sqlite)
(require 'cl-lib)
(require 'gnosis-algorithm)

(defgroup gnosis nil
  "Spaced repetition learning tool."
  :group 'external
  :prefix "gnosis-")

(defvar gnosis-db (emacsql-sqlite (concat user-emacs-directory "gnosis.db")))

(cl-defun gnosis--select (value table &optional (restrictions '1=1))
  "Select VALUE from TABLE, optionally with RESTRICTIONS."
  (emacsql gnosis-db `[:select ,value :from ,table :where ,restrictions]))

(cl-defun gnosis--create-table (table &optional values)
  "Create TABLE for VALUES."
  (emacsql gnosis-db `[:create-table ,table ,values]))

(cl-defun gnosis--drop-table (table)
  "Drop TABLE from gnosis-db."
  (emacsql gnosis-db `[:drop-table ,table]))

(cl-defun gnosis--insert-into (table values)
  "Insert VALUES to TABLE."
  (emacsql gnosis-db `[:insert :into ,table :values ,values]))

(cl-defun gnosis-update (table value where)
  "Update records in TABLE with to new VALUE based on the given WHERE condition.
Example:
 (gnosis-update `''notes `''(= main \"NEW VALUE\") `''(= id 12))"
  (emacsql gnosis-db `[:update ,table :set ,value :where ,where]))

(cl-defun gnosis-get (value table &optional (restrictions '1=1))
  "Get VALUE from TABLE, optionally with where RESTRICTIONS."
  (caar (gnosis--select value table restrictions)))

(defun gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql gnosis-db `[:delete :from ,table :where ,value]))

(defun gnosis--display-question (id)
  "Display main row for question ID."
  (let ((question (gnosis-get 'main 'notes `(= id ,id))))
    (with-current-buffer
	(switch-to-buffer
	 (get-buffer-create "*gnosis*"))
      (erase-buffer)
      (insert question)
      (sit-for 0.5))))

(defun gnosis--ask-input (prompt)
  "PROMPT user for input until `q' is given.

The user is prompted to provide input for the 'PROMPT' message, and
the returns the list of inputs in reverse order."
  (let ((input nil))
    (while (not (equal (car input) "q"))
      (push (read-string (concat prompt " (q for quit): ")) input))
    (when (equal (car input) "q")
      (pop input))
    (reverse input)))

(defun gnosis-add-deck (name)
  "Create deck with NAME."
  (interactive (list (read-string "Deck Name: ")))
  (gnosis--insert-into 'decks `([nil ,name])))

(defun gnosis--get-deck-name ()
  "Get name from table DECKS."
  (when (equal (gnosis--select 'name 'decks) nil)
    (error "No decks found"))
  (completing-read "Deck: " (gnosis--select 'name 'decks)))

(defun gnosis--get-deck-id ()
  "Select id for deck name."
  (let ((deck (gnosis--get-deck-name)))
    (gnosis-get 'id 'decks `(= name ,deck))))

(defun gnosis-delete-deck (id)
  "Delete deck with id value of ID."
  (interactive (list (gnosis--get-deck-id)))
  (gnosis--delete 'decks `(= id ,id)))

(cl-defun gnosis-add-note-mcq (&key deck question choices correct-answer tags)
  "Create a NOTE with a list of multiple CHOICES.

MCQ type consists of a main `QUESTION' that is displayed to the user.
The user will be prompted to select the correct answer from a list of
`CHOICES'. The `CORRECT-ANSWER' should be the index of the correct
choice in the `CHOICES' list. Each note must correspond to one `DECK'.
TAGS are used to organize questions."
  (interactive
   (list :deck (gnosis--get-deck-id)
	 :question (read-string "Question: ")
         :choices (gnosis--ask-input "Choices")
	 ;; NOTE: string-to-number transforms non-number strings to 0
         :correct-answer (string-to-number (read-string "Which is the correct answer (number)? "))
	 :tags (gnosis--ask-input "Tags")))
  (cond ((or (not (numberp correct-answer)) (equal correct-answer 0))
	 (error "Correct answer value must be the index number of the correct answer"))
	((null tags)
	 (setq tags 'untagged)))
  (gnosis--insert-into 'notes `([nil "mcq" ,question ,choices ,correct-answer ,tags ,deck]))
  ;; Get last inserted note-id
  (let ((note-id (caar (last (gnosis--select 'id 'notes))))
	(date (gnosis-algorithm-date)))
    (gnosis--insert-into 'review `([,note-id ,gnosis-algorithm-ef ,gnosis-algorithm-ff ,gnosis-algorithm-interval]))
    (gnosis--insert-into 'review-log `([,note-id ,date ,date 0 0 0]))))

(defun gnosis-add-note (type)
  "Create note as TYPE."
  (interactive (list (completing-read "Type: " '(MCQ Cloze Basic) nil t)))
  (pcase type
    ("MCQ" (call-interactively 'gnosis-add-note-mcq))
    ("Cloze" (message "Not ready yet."))
    ("Basic" (message "Not ready yet."))
    (_ (message "No such type."))))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'options 'notes `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (completing-read "Answer: " choices)))

;; Review
(defun gnosis-review--algorithm (id success)
  "Get next review date & ef for note with value of id ID.

SUCCESS is a binary value, 1 = success, 0 = failure.
Returns a list of the form ((yyyy mm dd) ef)."
  (let ((ff gnosis-algorithm-ff)
	(ef (nth 2 (gnosis-get 'ef 'review `(= id ,id)))))
    (gnosis-algorithm-next-interval (gnosis-review--get-offset id)
				    (gnosis-get 'n 'review-log `(= id ,id))
				    ef
				    success
				    ff)))

(defun gnosis-review--get-due-notes ()
  "Get due notes for current date."
  (gnosis--select 'id 'review-log `(<= next-rev ',(gnosis-algorithm-date))))

(defun gnosis-review--get-offset (id)
  "Get offset for note with value of id ID."
  (let ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev)))

(defun gnosis-review--new-ef (id success)
  "Update ef for note with value of id ID.

SUCCESS is a binary value, 1 = success, 0 = failure."
  (cl-assert (or (equal success 1) (equal success 0)))
  (let ((ef (gnosis-review--algorithm id success)))
    (if (equal success 1)
	(nth 0 ef)
      (nth 1 ef))))

(defun gnosis-review--success (id)
  "Update review-log for note with value of id ID."
  (let ((ef (gnosis-review--new-ef id 1)))
    ;; (gnosis-update 'review-log `(= last-rev ',(gnosis-algorithm-date)) `(= id ,id))
    (gnosis-update 'review-log `(= next-rev ',(car (gnosis-review--algorithm id 1))) `(= id ,id))
    (gnosis-update 'review `(= ef ,ef) `(= id ,id))))

(cl-defun gnosis-review-update--last-rev (id)
  "Update last-rev, from review-log, for note with value of id ID.

Change last-rev to current date."
  (let ((date (gnosis-algorithm-date)))
    (gnosis-update 'review-log `(= last-rev ',date) `(= id ,id))))

(defun gnosis-review-mcq-choices (id)
  "Display multiple choice answers for question ID."
  (let ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	(choices (gnosis-get 'options 'notes `(= id ,id)))
	(user-choice (gnosis-mcq-answer id)))
    (if (equal (nth (- answer 1) choices) user-choice)
	(message "Correct!")
      (message "False")))
  (sit-for 0.5))

(defun gnosis-review-note (id)
  "Start review for note with value of id ID."
  (let ((type (gnosis-get 'type 'notes `(= id id))))
    (gnosis--display-question id)
    (pcase type
      ("mcq" (gnosis-review-mcq-choices id))
      ("basic" (message "Not Ready yet."))
      ("cloze" (message "Not Ready yet."))
      (_ (error "Malformed note type")))))

(defun gnosis-review ()
  "Start gnosis session."
  (interactive)
  (let ((due-notes (gnosis-review--get-due-notes)))
    (if (null due-notes)
	(message "No due notes.")
      (cl-loop for note in due-notes
	       do (gnosis-review-note (car note)))))
  (message "Review session finished."))

;;; Database Schemas
;; Enable foreign_keys
;; TODO: Redo when eval
(emacsql gnosis-db "PRAGMA foreign_keys = ON")


(defvar gnosis-db-schema-decks '([(id integer :primary-key :autoincrement)
				  (name text :not-null)]))

(defvar gnosis-db-schema-notes '([(id integer :primary-key :autoincrement)
				  (type text :not-null)
				  (main text :not-null)
				  (options text :not-null)
				  (answer text :not-null)
				  (tags text :default untagged)
				  (deck-id integer)]
				 (:foreign-key [deck-id] :references decks [id]
					       :on-delete :cascade)))

(defvar gnosis-db-schema-review '([(id integer :not-null) ;; note-id
				   (ef integer :not-null) ;; Easiness factor
				   (ff integer :not-null) ;; Forgetting factor
				   (interval integer :not-null)] ;; Interval
				  (:foreign-key [id] :references notes [id]
						:on-delete :cascade)))

(defvar gnosis-db-schema-review-log '([(id integer :not-null)       ;; note-id
				       (last-rev integer :not-null) ;; Last review date
				       (next-rev integer :not-null) ;; Next review date
				       (failures integer :not-null) ;; Number of consecutive review failures
				       (suspend  integer :not-null) ;; binary value, 1 = suspend note
				       (n integer :not-null)]       ;; Number of reviews
				      (:foreign-key [id] :references notes [id]
						    :on-delete :cascade)))

;; testing
(defun gnosis-test-buffer ()
  "Create testing buffer."
  (interactive)
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*gnosis*"))
    (read-only-mode 0)
    (erase-buffer)
    (gnosis--display-question 4)
    (gnosis-review 4)
    (gnosis-mode)))

(defun gnosis-init ()
  "Create notes content table."
  (interactive)
  ;;(make-directory (concat user-emacs-directory "gnosis"))
  (dolist (table '(notes decks review review-log))
    (condition-case nil
	(gnosis--drop-table table)
      (error (message "No %s table to drop." table))))
  ;; Enable foreign_keys
  (emacsql gnosis-db "PRAGMA foreign_keys = ON")
  ;; Create decks table
  (gnosis--create-table 'decks gnosis-db-schema-decks)
  ;; Create notes table
  (gnosis--create-table 'notes gnosis-db-schema-notes)
  ;; Create review table
  (gnosis--create-table 'review gnosis-db-schema-review)
  ;; Create review-log table
  (gnosis--create-table 'review-log gnosis-db-schema-review-log)
  (gnosis-add-deck "Anatomy"))

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

(define-derived-mode gnosis-mode special-mode "Gnosis"
  "Gnosis Mode."
  :interactive t
  (display-line-numbers-mode 0)
  :lighter " gnosis-mode")


(provide 'gnosis)
;;; gnosis.el ends here
