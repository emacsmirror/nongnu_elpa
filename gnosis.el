;;; gnosis.el --- Spaced Repetition Learning Tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2") (emacsql "20230228"))

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
(require 'gnosis-faces)

(defgroup gnosis nil
  "Spaced repetition learning tool."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-dir (concat user-emacs-directory "gnosis")
  "Gnosis directory."
  :type 'directory
  :group 'gnosis)


(defvar gnosis-images-dir (concat gnosis-dir "/" "images")
  "Gnosis images directory.")

(unless (file-exists-p gnosis-images-dir)
  (make-directory gnosis-dir)
  (make-directory gnosis-images-dir))

(defvar gnosis-db (emacsql-sqlite-open (concat gnosis-dir "/" "gnosis.db"))
  "Gnosis database.")

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

(defun gnosis-display--question (id)
  "Display main row for note ID."
  (let ((question (gnosis-get 'main 'notes `(= id ,id))))
    (with-current-buffer
	(switch-to-buffer
	 (get-buffer-create "*gnosis*"))
      (erase-buffer)
      (fill-paragraph (insert (propertize question 'face 'gnosis-face-main))))))

(defun gnosis-display--correct-answer-mcq (id user-choice)
  "Display correct answer & USER-CHOICE for MCQ note type with ID."
  (let* ((correct-answer (gnosis-get 'answer 'notes `(= id ,id)))
	 (options (gnosis-get 'options 'notes `(= id ,id)))
	 (answer (nth correct-answer options)))
    (with-current-buffer
	(switch-to-buffer
	 (get-buffer-create "*gnosis*"))
      (insert (concat "\n\nCorrect answer: "
		      (propertize answer 'face 'gnosis-face-correct-answer)
		      "\nYour answer: "
		      (propertize user-choice 'face 'gnosis-face-user-choice))))))
(defun gnosis-display--correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ note."
  (with-current-buffer
      (switch-to-buffer
       (get-buffer-create "*gnosis*"))
    (insert (concat "\n\nCorrect answer: "
		    (propertize answer 'face 'gnosis-face-correct-answer)
		    "\nYour answer: "
		    (propertize user-choice 'face 'gnosis-face-user-choice)))))

(defun gnosis-display--extra (id)
  "Display extra information for note ID."
  (let ((extras (gnosis-get 'extra-notes 'extras `(= id ,id))))
    (with-current-buffer (switch-to-buffer (get-buffer-create "*gnosis*"))
      (insert (propertize "\n\n-----\n" 'face 'gnosis-face-seperator))
      (fill-paragraph (insert (concat "\n" (propertize extras 'face 'gnosis-face-extra)))))))

(defun gnosis-display--image (id)
  "Display image for note ID."
  (let* ((img (gnosis-get 'images 'extras `(= id ,id)))
	 (path-to-image (concat gnosis-images-dir "/" img))
	 (image (create-image path-to-image 'png nil :width 500 :height 300)))
    (when img
      (with-current-buffer (switch-to-buffer (get-buffer-create "*gnosis*"))
	(insert "\n\n")
	(insert-image image)))))

(cl-defun gnosis--prompt (prompt &optional (downcase nil) (split nil))
  "PROMPT user for input until `q' is given.

The user is prompted to provide input for the 'PROMPT' message.
Returns the list of non-'q' inputs in reverse order of their entry.

Set DOWNCASE to t to downcase all input given.
Set SPLIT to t to split all input given."
  (cl-loop with input = nil
           for response = (read-string (concat prompt " (q for quit): "))
	   do (if downcase (setf response (downcase response)))
           for response-parts = (if split (split-string response " ") (list response))
           if (member "q" response-parts) return (nreverse input)
           do (cl-loop for part in response-parts
	               unless (string-empty-p part)
                       do (push part input))))

(defun gnosis-add-deck (name)
  "Create deck with NAME."
  (interactive (list (read-string "Deck Name: ")))
  (gnosis--insert-into 'decks `([nil ,name]))
  (message "Created deck '%s'" name))

(defun gnosis--get-deck-name ()
  "Get name from table DECKS."
  (when (equal (gnosis--select 'name 'decks) nil)
    (error "No decks found"))
  (completing-read "Deck: " (gnosis--select 'name 'decks)))

(cl-defun gnosis--get-deck-id (&optional (deck (gnosis--get-deck-name)))
  "Get id for DECK name."
  (gnosis-get 'id 'decks `(= name ,deck)))

(defun gnosis-delete-deck (deck)
  "Delete DECK."
  (interactive (list (gnosis--get-deck-name)))
  (gnosis--delete 'decks `(= name ,deck))
  (message "Deleted deck %s" deck))

(defun gnosis-add-note-fields (deck type main options answer extra tags suspend image)
  "Add fields for new note.

DECK: Deck name for new note
TYPE: New note type (mcq,cloze,basic)
MAIN: Note's main part
OPTIONS: Note's options (optional, used for MCQ type)
ANSWER: Correct answer for note, for MCQ is an integer while for
cloze/basic a string/list of the right answer(s)
EXTRA: Extra information to display after answering note
TAGS: Tags to organize notes
SUSPEND: Integer value of 1 or 0, where 1 suspends the card
IMAGE: Image to display during review."
  (gnosis--insert-into 'notes `([nil ,type ,main ,options ,answer ,tags ,(gnosis--get-deck-id deck)]))
  (gnosis--insert-into 'review `([nil ,gnosis-algorithm-ef ,gnosis-algorithm-ff ,gnosis-algorithm-interval]))
  (gnosis--insert-into 'review-log `([nil ,(gnosis-algorithm-date) ,(gnosis-algorithm-date) 0 0 0 0 ,suspend 0]))
  (gnosis--insert-into 'extras `([nil ,extra ,image])))

(cl-defun gnosis-add-note-mcq (&key deck question choices correct-answer extra (image nil) tags (suspend 0))
  "Create a NOTE with a list of multiple CHOICES.

MCQ type consists of a main `QUESTION' that is displayed to the user.
The user will be prompted to select the correct answer from a list of
`CHOICES'. The `CORRECT-ANSWER' should be the index of the correct
choice in the `CHOICES' list. Each note must correspond to one `DECK'.

`EXTRA' are extra information displayed after an answer is given.
`TAGS' are used to organize questions.
`SUSPEND' is a binary value, where 1 is for suspend."
  (interactive
   (list :deck (gnosis--get-deck-name)
	 :question (read-string "Question: ")
         :choices (gnosis--prompt "Choices")
	 ;; NOTE: string-to-number transforms non-number strings to 0
         :correct-answer (string-to-number (read-string "Which is the correct answer (number)? "))
	 :extra (read-string "Extra: ")
	 :tags (gnosis--prompt "Tags" t t)))
  (cond ((or (not (numberp correct-answer)) (equal correct-answer 0))
	 (error "Correct answer value must be the index number of the correct answer"))
	((null tags)
	 (setf tags 'untagged)))
  (gnosis-add-note-fields deck "mcq" question choices correct-answer extra tags suspend image))

(cl-defun gnosis-add-note-cloze (&key deck note tags (suspend 0) extra (image nil))
  "Add cloze type note."
  (interactive (list :deck (gnosis--get-deck-name)
		     :note (read-string "Cloze note: ")
		     :extra (read-string "Extra: ")
		     :tags (gnosis--prompt "Tags" t t)))
  (let ((notags-note (gnosis-cloze-remove-tags note))
	(clozes (gnosis-cloze-get-clozes note)))
    (cl-loop for cloze in clozes
	     ;; TODO: OPTIONS need to be hints
	     do (gnosis-add-note-fields deck "cloze" notags-note "" cloze extra tags suspend image))))

(defun gnosis-add-note (type)
  "Create note as TYPE."
  (interactive (list (completing-read "Type: " '(MCQ Cloze Basic) nil t)))
  (pcase type
    ("MCQ" (call-interactively 'gnosis-add-note-mcq))
    ("Cloze" (call-interactively 'gnosis-add-note-cloze))
    ("Basic" (message "Not ready yet."))
    (_ (message "No such type."))))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'options 'notes `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (completing-read "Answer: " choices)))

(defun gnosis-cloze-extract-answers (string)
  "Extract items from a STRING, and group them based on the cx tag."
  (let ((res '())
        (pos 0))
    (while (string-match "{\\{1,2\\}\\(c[0-9]+\\)::\\([^}]*\\)}\\{1,2\\}" string pos)
      (let* ((tag (match-string 1 string))
            (text (match-string 2 string))
            (pair (assoc tag res)))
        (if pair
            (setcdr pair (append (cdr pair) (list text)))
          (push (list tag text) res)))
      (setf pos (match-end 0)))
    ;; Reverse the final result and each sublist to maintain original order
    ;; As our push/assoc approach prepends elements, not appends them
    ;; (mapcar (lambda (x) (cons (car x) (cdr x))) (nreverse res)) ;; check for cl-rest
    (mapcar (lambda (x) (cdr x)) (nreverse res))))
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

(defun gnosis-review-get-due-notes ()
  "Get due notes id for current date.

Select notes where:
 - Next review date <= current date
 - Not suspended."
  (emacsql gnosis-db `[:select [id] :from review-log :where (and (<= next-rev ',(gnosis-algorithm-date))
								 (= suspend 0))]))

(defun gnosis-review--get-offset (id)
  "Get offset for note with value of id ID."
  (let ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev)))

(defun gnosis-review-round (num)
  "Round NUM to 1 decimal.

This function is used to round floating point numbers to 1 decimal,
such as the easiness factor (ef)."
  (/ (round (* num 100.00)) 100.00))

(defun gnosis-review-new-ef (id success)
  "Get new ef for note with value of id ID.

SUCCESS is a binary value, 1 = success, 0 = failure.
Returns a list of the form (ef-increase ef-decrease ef)."
  (let ((ef (nth 1 (gnosis-review--algorithm id success)))
	(old-ef (gnosis-get 'ef 'review `(= id ,id))))
    (cl-substitute (gnosis-review-round ef) (nth 2 old-ef) old-ef)))

(defun gnosis-review--success (id)
  "Update review-log for note with value of id ID."
  (let ((ef (gnosis-review-new-ef id 1)))
    ;; Update review-log
    (gnosis-update 'review-log `(= last-rev ',(gnosis-algorithm-date)) `(= id ,id))
    (gnosis-update 'review-log `(= next-rev ',(car (gnosis-review--algorithm id 1))) `(= id ,id))
    (gnosis-update 'review-log `(= n (+ 1 ,(gnosis-get 'n 'review-log `(= id ,id)))) `(= id ,id))
    ;; Update review
    (gnosis-update 'review `(= ef ',ef) `(= id ,id))))

(defun gnosis-review-mcq (id)
  "Display multiple choice answers for question ID."
  (gnosis-display--image id)
  (let ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	(choices (gnosis-get 'options 'notes `(= id ,id)))
	(user-choice (gnosis-mcq-answer id)))
    (if (equal (nth answer choices) user-choice)
        (progn (gnosis-review--success id)
	       (message "Correct!"))
      (message "False"))
    (sit-for 0.5)
    (gnosis-display--correct-answer-mcq id user-choice)
    (gnosis-display--extra id)))

(defun gnosis-review-note (id)
  "Start review for note with value of id ID."
  (let ((type (gnosis-get 'type 'notes `(= id id))))
    (gnosis-display--question id)
    (pcase type
      ("mcq" (gnosis-review-mcq id))
      ("basic" (message "Not Ready yet."))
      ("cloze" (message "Not Ready yet."))
      (_ (error "Malformed note type")))))

(defun gnosis-review ()
  "Start gnosis session."
  (interactive)
  (let* ((due-notes (gnosis-review-get-due-notes))
         (note-count 0)
         (total-notes (length due-notes)))
    (if (null due-notes)
        (message "No due notes.")
      (cl-loop for note in due-notes
               do (progn
                    (gnosis-review-note (car note))
                    (setf note-count (+ note-count 1))
                    (when (and (< note-count total-notes)
                               (not (y-or-n-p "Review next note?")))
                      (cl-return)))
               finally (message "Review session finished. %d note(s) reviewed." note-count)))))

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
				  (deck-id integer :not-null)]
				 (:foreign-key [deck-id] :references decks [id]
					       :on-delete :cascade)))

(defvar gnosis-db-schema-review '([(id integer :primary-key :not-null) ;; note-id
				   (ef integer :not-null) ;; Easiness factor
				   (ff integer :not-null) ;; Forgetting factor
				   (interval integer :not-null)] ;; Interval
				  (:foreign-key [id] :references notes [id]
						:on-delete :cascade)))

(defvar gnosis-db-schema-review-log '([(id integer :primary-key :not-null) ;; note-id
				       (last-rev integer :not-null)  ;; Last review date
				       (next-rev integer :not-null)  ;; Next review date
				       (c-success integer :not-null) ;; number of consecutive successful reviews
				       (t-success integer :not-null) ;; Number of total successful reviews
				       (c-fails integer :not-null)   ;; Number of consecutive failed reviewss
				       (t-fails integer :not-null)   ;; Number of total failed reviews
				       (suspend integer :not-null)   ;; Binary value, 1=suspended
				       (n integer :not-null)]        ;; Number of reviews
				      (:foreign-key [id] :references notes [id]
						    :on-delete :cascade)))

(defvar gnosis-db-schema-extras '([(id integer :primary-key :not-null)
				   (extra-notes string)
				   (images string)]
				  (:foreign-key [id] :references notes [id]
						:on-delete :cascade)))


(defun gnosis-init ()
  "Create notes content table."
  (interactive)
  ;;(make-directory (concat user-emacs-directory "gnosis"))
  (dolist (table '(notes decks review review-log extras))
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
  ;; Create extras table
  (gnosis--create-table 'extras gnosis-db-schema-extras)
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
