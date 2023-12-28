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

(defcustom gnosis-cloze-char "__"
  "Gnosis cloze character."
  :type 'string
  :group 'gnosis)


(defvar gnosis-images-dir (concat gnosis-dir "/" "images")
  "Gnosis images directory.")

(unless (file-exists-p gnosis-images-dir)
  (make-directory gnosis-dir)
  (make-directory gnosis-images-dir))

(defvar gnosis-db (emacsql-sqlite-open (concat gnosis-dir "/" "gnosis.db"))
  "Gnosis database.")

(cl-defun gnosis-select (value table &optional (restrictions '1=1))
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
  (caar (gnosis-select value table restrictions)))

(defun gnosis-get-note-tags (id)
  "Return tags for note ID."
  (gnosis-get 'tags 'notes `(= id ,id)))

(defun gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql gnosis-db `[:delete :from ,table :where ,value]))

(defmacro with-gnosis-buffer (&rest body)
  "Execute BODY in gnosis buffer."
  `(with-current-buffer (switch-to-buffer (get-buffer-create "*gnosis*"))
     (gnosis-mode)
     ,@body))

;; TODO: Use gnosis-completing-read to display total notes for review
;; option
(defun gnosis-completing-read (prompt candidates)
  "Gnosis `completing-read' alternative.

`PROMPT' for choice between `CANDIDATES'.
CANDIDATES need to be in a format of ((\"Option1\" \"Additional Info\")).

WARNING: This function is still under development, DO NOT use this as is now."
  (let* ((collection (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (affixation-function . ,(lambda (cands)
                                                       (mapcar
							(lambda (cand)
                                                          (list cand "" (format "  %s"
										(or (cdr (assoc cand candidates)) ""))))
							cands))))
                         (complete-with-action action candidates str pred))))
         (choice (completing-read prompt collection)))
    choice))

(defun gnosis-display--question (id)
  "Display main row for note ID."
  (let ((question (gnosis-get 'main 'notes `(= id ,id))))
    (with-gnosis-buffer
     (erase-buffer)
     (fill-paragraph (insert (concat "\n"
				     (propertize question 'face 'gnosis-face-main)))))))

(defun gnosis-display--cloze-sentence (sentence clozes)
  "Display cloze sentence for SENTENCE with CLOZES."
  (with-gnosis-buffer
   (erase-buffer)
   (fill-paragraph
    (insert
     (concat "\n"
	     (gnosis-cloze-replace-words sentence clozes (propertize gnosis-cloze-char 'face 'gnosis-face-cloze)))))))

(defun gnosis-display--basic-answer (answer success user-input)
  "Display ANSWER.

When SUCCESS nil, display USER-INPUT as well"
  (with-gnosis-buffer
   (insert
    (concat "\n\n"
	    (propertize "Answer:" 'face 'gnosis-face-directions)
	    " "
	    (propertize answer 'face 'gnosis-face-correct)))
   ;; Insert user wrong answer
   (when (not success)
     (insert (concat "\n"
		     (propertize "Your answer:" 'face 'gnosis-face-directions)
		     " "
		     (propertize user-input 'face 'gnosis-face-false))))))

(defun gnosis-display--hint (hint)
  "Display HINT."
  (with-gnosis-buffer
   (goto-char (point-max))
   (insert (concat
	    (propertize "\n\n-----\n" 'face 'gnosis-face-seperator)
	    (propertize hint 'face 'gnosis-face-hint)))))

(defun gnosis-display--cloze-correct (cloze-chars correct)
  "Replace CLOZE-CHARS with CORRECT."
  (with-gnosis-buffer
   (goto-char (point-min))
   (search-forward cloze-chars nil t)
   (replace-match (propertize correct 'face 'gnosis-face-correct))))

(cl-defun gnosis-display--cloze-user-answer (user-input &optional (false nil))
  "Display USER-INPUT answer for cloze note.

If FALSE t, use gnosis-face-false face"
  (with-gnosis-buffer
   (goto-char (point-max))
   (insert (concat "\n\n"
		   (propertize "Your answer:" 'face 'gnosis-face-directions)
		   " "
		   (propertize user-input 'face (if false 'gnosis-face-false 'gnosis-face-correct))))))

(defun gnosis-display--correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ note."
  (with-gnosis-buffer
   (insert (concat "\n\n"
		   (propertize "Correct Answer:" 'face 'gnosis-face-directions)
		   " "
		   (propertize answer 'face 'gnosis-face-correct)
		   "\n"
		   (propertize "Your answer:" 'face 'gnosis-face-directions)
		   " "
		   (propertize user-choice 'face (if (string= answer user-choice)
						     'gnosis-face-correct
						   'gnosis-face-false))))))

(defun gnosis-display--extra (id)
  "Display extra information for note ID."
  (let ((extras (gnosis-get 'extra-notes 'extras `(= id ,id))))
    (with-gnosis-buffer
      (goto-char (point-max))
      (insert (propertize "\n\n-----\n" 'face 'gnosis-face-seperator))
      (fill-paragraph (insert (concat "\n" (propertize extras 'face 'gnosis-face-extra)))))))

(defun gnosis-display--image (id)
  "Display image for note ID."
  (let* ((img (gnosis-get 'images 'extras `(= id ,id)))
	 (path-to-image (concat gnosis-images-dir "/" img))
	 (image (create-image path-to-image 'png nil :width 500 :height 300)))
    (when img
      (with-gnosis-buffer
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
  (when (equal (gnosis-select 'name 'decks) nil)
    (error "No decks found"))
  (completing-read "Deck: " (gnosis-select 'name 'decks)))

(cl-defun gnosis--get-deck-id (&optional (deck (gnosis--get-deck-name)))
  "Get id for DECK name."
  (gnosis-get 'id 'decks `(= name ,deck)))

(defun gnosis-delete-deck (deck)
  "Delete DECK."
  (interactive (list (gnosis--get-deck-name)))
  (gnosis--delete 'decks `(= name ,deck))
  (message "Deleted deck %s" deck))

(defun gnosis-suspend-deck (&optional deck)
  "Suspend all note(s) with DECK id.

When called with a prefix, unsuspends all notes in deck."
  (unless deck (setf deck (gnosis--get-deck-id)))
  (let ((notes (gnosis-select 'id 'notes `(= deck-id ,deck)))
	(suspend (if current-prefix-arg 0 1)))
    (cl-loop for note in notes
	     do (gnosis-update 'review-log `(= suspend ,suspend) `(= id ,(car note))))))

(defun gnosis-suspend-tag ()
  "Suspend all note(s) with tag."
  (let ((notes (gnosis-select-by-tag (gnosis-prompt-tag)))
	(suspend (if current-prefix-arg 0 1)))
    (cl-loop for note in notes
	     do (gnosis-update 'review-log `(= suspend ,suspend) `(= id ,note)))))

(defun gnosis-suspend ()
  "Suspend note(s) with specified values."
  (interactive)
  (let ((item (completing-read "Suspend by: " '("Deck" "Tag"))))
    (pcase item
      ("Deck" (gnosis-suspend-deck))
      ("Tag" (gnosis-suspend-tag))
      (_ (message "Not ready yet.")))))


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


;; Adding note(s) consists firstly of a hidden 'gnosis-add-note--TYPE'
;; function that does the computation & error checking to generate a
;; note from given input. Secondly, 'gnosis-add-note-TYPE' normal
;; function, which prompts for user input and passes it to the hidden
;; function.


(cl-defun gnosis-add-note--mcq (&key deck question choices correct-answer extra (image nil) tags (suspend 0))
  "Create a NOTE with a list of multiple CHOICES.

MCQ type consists of a main `QUESTION' that is displayed to the user.
The user will be prompted to select the correct answer from a list of
`CHOICES'. The `CORRECT-ANSWER' should be the index of the correct
choice in the `CHOICES' list. Each note must correspond to one `DECK'.

`EXTRA' are extra information displayed after an answer is given.
`TAGS' are used to organize questions.
`SUSPEND' is a binary value, where 1 is for suspend."
  (cond ((or (not (numberp correct-answer)) (equal correct-answer 0))
	 (error "Correct answer value must be the index number of the correct answer"))
	((null tags)
	 (setf tags 'untagged)))
  (gnosis-add-note-fields deck "mcq" question choices correct-answer extra tags suspend image))

(defun gnosis-add-note-mcq ()
  "Add note(s) of type `MCQ' interactively to selected deck."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `MCQ' to `%s' deck? " deck))
      (gnosis-add-note--mcq :deck deck
			    :question (read-string "Question: ")
			    :choices (gnosis--prompt "Choices")
			    :correct-answer (string-to-number (read-string "Which is the correct answer (number)? "))
			    :extra (read-string "Extra: ")
			    :tags (gnosis-prompt-tag)))))

(cl-defun gnosis-add-note--basic (&key deck question hint answer extra (image nil) tags (suspend 0))
  "Add Basic type note."
  (gnosis-add-note-fields deck "basic" question hint answer extra tags suspend image))

(defun gnosis-add-note-basic ()
  "Add note(s) of type `Basic' interactively to selected deck."
  (interactive)
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `basic' to `%s' deck? " deck))
      (gnosis-add-note--basic :deck deck
			      :question (read-string "Question: ")
			      :answer (read-string "Answer: ")
			      :hint (read-string "Hint: ")
			      :extra (read-string "Extra: ")
			      :tags (gnosis-prompt-tag)))))

(cl-defun gnosis-add-note--cloze (&key deck note hint tags (suspend 0) extra (image nil))
  "Add cloze type note.

`EXTRA' are extra information displayed after an answer is given.
`TAGS' are used to organize questions.
`SUSPEND' is a binary value, where 1 is for suspend."
  (let ((notags-note (gnosis-cloze-remove-tags note))
	(clozes (gnosis-cloze-extract-answers note)))
    (cl-loop for cloze in clozes
	     do (gnosis-add-note-fields deck "cloze" notags-note hint cloze extra tags suspend image))))

(defun gnosis-add-note-cloze ()
  "Add note(s) of type cloze interactively to selected deck."
  (interactive)
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `basic' to `%s' deck? " deck))
      (gnosis-add-note--cloze :deck deck
			      :note (read-string "Question: ")
			      :hint (read-string "Hint: ")
			      :extra (read-string "Extra: ")
			      :tags (gnosis-prompt-tag)))))


;;;###autoload
(defun gnosis-add-note (type)
  "Create note(s) as TYPE interactively."
  (interactive (list (completing-read "Type: " '(MCQ Cloze Basic) nil t)))
  (pcase type
    ("MCQ" (gnosis-add-note-mcq))
    ("Cloze" (gnosis-add-note-cloze))
    ("Basic" (gnosis-add-note-basic))
    (_ (message "No such type."))))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'options 'notes `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (completing-read "Answer: " choices)))

(defun gnosis-cloze-remove-tags (string)
  "Replace cx-tags in STRING.

Works both with {} and {{}} to make easier to import anki notes."
  (let* ((regex "{\\{1,2\\}c\\([0-9]+\\)::?\\(.*?\\)}\\{1,2\\}")
         (result (replace-regexp-in-string regex "\\2" string)))
    result))

(defun gnosis-cloze-replace-words (string words new)
  "In STRING replace WORDS with NEW."
  (cl-assert (listp words))
  (cl-loop for word
	   in words
	   do (setf string (replace-regexp-in-string (concat "\\<" word "\\>") ;; use word boundary indentifiers
						     new string)))
  string)

(defun gnosis-cloze-extract-answers (str)
  "Extract cloze answers for STR.

Return a list of cloze answers for STR, organized by cX-tag.

Valid cloze formats include:
\"This is an {c1:example}\"
\"This is an {c1::example}\"
\"This is an {{c1:example}}\"
\"This is an {{c1::example}}\""
  (let ((result-alist '())
        (start 0))
    (while (string-match "{\\{1,2\\}c\\([0-9]+\\)::?\\(.*?\\)}\\{1,2\\}" str start)
      (let* ((tag (match-string 1 str))
             (content (match-string 2 str)))
        (if (assoc tag result-alist)
            (push content (cdr (assoc tag result-alist)))
          (push (cons tag (list content)) result-alist))
        (setf start (match-end 0))))
    (mapcar (lambda (tag-group) (nreverse (cdr tag-group)))
	    (nreverse result-alist))))

(defun gnosis-compare-strings (str1 str2)
  "Compare STR1 and STR2.

Compare 2 strings, ignoring case and whitespace."
  (let ((modified-str1 (downcase (replace-regexp-in-string "\\s-" "" str1)))
        (modified-str2 (downcase (replace-regexp-in-string "\\s-" "" str2))))
    (string= modified-str1 modified-str2)))

(defun gnosis-unique-tags ()
  "Return a list of unique strings for tags in gnosis-db."
  (cl-loop for tags in (gnosis-select 'tags 'notes)
           nconc tags into all-tags
           finally return (delete-dups all-tags)))

(defun gnosis-select-by-tag (input-tags)
  "Return note id for every note with INPUT-TAGS."
  (unless (listp input-tags)
    (error "`input-tags' need to be a list"))
  (cl-loop for (id tags) in (emacsql gnosis-db [:select [id tags] :from notes])
           when (cl-every (lambda (tag) (member tag tags)) input-tags)
           collect id))

(defun gnosis-suspended-p (id)
  "Return t if note with ID is suspended."
  (if (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1)
      t
    nil))

(defun gnosis-prompt-tag ()
  "Prompt user to enter tags, until they enter `q'.

Returns a list of unique entered tags."
  (interactive)
  (let ((tags '())
        (tag ""))
    (while (not (string= tag "q"))
      (setf tag (completing-read "Add tag (q for quit): " (gnosis-unique-tags) nil nil))
      (unless (or (string= tag "q") (member tag tags))
        (push tag tags)))
    (reverse tags)))

;; Review
;;;;;;;;;;
(defun gnosis-review--algorithm (id success)
  "Get next review date & ef for note with value of id ID.

SUCCESS is a binary value, 1 = success, 0 = failure.
Returns a list of the form ((yyyy mm dd) ef)."
  (let ((ff gnosis-algorithm-ff)
	(ef (nth 2 (gnosis-get 'ef 'review `(= id ,id))))
	(c-success (gnosis-get 'c-success 'review-log `(= id ,id))))
    (gnosis-algorithm-next-interval (gnosis-review--get-offset id)
				    (gnosis-get 'n 'review-log `(= id ,id))
				    ef success ff c-success)))

(defun gnosis-review-is-due-p (note-id)
  "Return t if unsuspended note with NOTE-ID is due today."
  (emacsql gnosis-db `[:select [id] :from review-log :where (and (<= next-rev ',(gnosis-algorithm-date))
								 (= suspend 0)
								 (= id ,note-id))]))

(defun gnosis-review-get-due-notes ()
  "Get due notes id for current date.

Select notes where:
 - Next review date <= current date
 - Not suspended."
  (emacsql gnosis-db `[:select [id] :from review-log :where (and (<= next-rev ',(gnosis-algorithm-date))
								 (= suspend 0))]))

(defun gnosis-review-due-notes--with-tags ()
  "Return a list of due note tags."
  (let ((due-notes (gnosis-review-get-due-notes)))
    (cl-remove-duplicates
     (cl-mapcan (lambda (note-id)
                  (gnosis-get-note-tags (car note-id)))
	        due-notes)
     :test 'equal)))

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

(defun gnosis-review--update (id success)
  "Update review-log for note with value of id ID.

SUCCESS is a binary value, 1 is for successful review."
  (let ((ef (gnosis-review-new-ef id 1)))
    ;; Update review-log
    (gnosis-update 'review-log `(= last-rev ',(gnosis-algorithm-date)) `(= id ,id))
    (gnosis-update 'review-log `(= next-rev ',(car (gnosis-review--algorithm id success))) `(= id ,id))
    (gnosis-update 'review-log `(= n (+ 1 ,(gnosis-get 'n 'review-log `(= id ,id)))) `(= id ,id))
    ;; Update review
    (gnosis-update 'review `(= ef ',ef) `(= id ,id))
    (if (= success 1)
	(progn (gnosis-update 'review-log `(= c-success ,(1+ (gnosis-get 'c-success 'review-log `(= id ,id)))) `(= id ,id))
	       (gnosis-update 'review-log `(= t-success ,(1+ (gnosis-get 't-success 'review-log `(= id ,id)))) `(= id ,id))
	       (gnosis-update 'review-log `(= c-fails 0) `(= id ,id)))
      (gnosis-update 'review-log `(= c-fails ,(1+ (gnosis-get 'c-fails 'review-log `(= id ,id)))) `(= id ,id))
      (gnosis-update 'review-log `(= t-fails ,(1+ (gnosis-get 't-fails 'review-log `(= id ,id)))) `(= id ,id))
      (gnosis-update 'review-log `(= c-success 0) `(= id ,id)))))

(defun gnosis-review-mcq (id)
  "Display multiple choice answers for question ID."
  (gnosis-display--image id)
  (gnosis-display--question id)
  (let* ((choices (gnosis-get 'options 'notes `(= id ,id)))
	 (answer (nth (- (gnosis-get 'answer 'notes `(= id ,id)) 1) choices))
	 (user-choice (gnosis-mcq-answer id)))
    (if (string= answer user-choice)
        (progn (gnosis-review--update id 1)
	       (message "Correct!"))
      (gnosis-review--update id 0)
      (message "False"))
    (sit-for 0.3)
    (gnosis-display--correct-answer-mcq answer user-choice)
    (gnosis-display--extra id)))

(defun gnosis-review-basic (id)
  "Review basic type note for ID."
  (gnosis-display--image id)
  (gnosis-display--question id)
  (gnosis-display--hint (gnosis-get 'options 'notes `(= id ,id)))
  (let* ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	 (user-input (read-string "Answer: "))
	 (success (gnosis-compare-strings answer user-input)))
    (gnosis-display--basic-answer answer success user-input)
    (gnosis-display--extra id)
    (gnosis-review--update id (if success 1 0))))

(defun gnosis-review-cloze--input (cloze)
  "Prompt for user input during cloze review.

If user-input is equal to CLOZE, return t."
  (let ((user-input (read-string "Answer: ")))
    (cons (gnosis-compare-strings user-input cloze) user-input)))

(defun gnosis-review-cloze--reveal (clozes)
  "Reveal CLOZES."
  (cl-loop for cloze in clozes do (gnosis-display--cloze-correct gnosis-cloze-char cloze)))

(defun gnosis-review-cloze (id)
  "Review cloze type note for ID."
  (let* ((main (gnosis-get 'main 'notes `(= id ,id)))
	 (clozes (gnosis-get 'answer 'notes `(= id ,id)))
	 (clozes-num (length clozes))
	 (num 0)
	 (hint (gnosis-get 'options 'notes `(= id ,id))))
    (gnosis-display--image id)
    (gnosis-display--cloze-sentence main clozes)
    (gnosis-display--hint hint)
    (cl-loop for cloze in clozes
	     do (let ((input (gnosis-review-cloze--input cloze)))
		  (if (equal (car input) t)
		      (progn (gnosis-review-cloze--reveal (list (nth num clozes)))
			     (setf num (1+ num)))
		    (gnosis-review-cloze--reveal clozes)
		    (gnosis-display--cloze-user-answer (cdr input) t)
		    (gnosis-review--update id 0)
		    (cl-return)))
	     finally (progn (gnosis-review-cloze--reveal clozes)
			    (gnosis-review--update id 1))))
  (gnosis-display--extra id))

(defun gnosis-review-note (id)
  "Start review for note with value of id ID, if note is unsuspended."
  (cond ((gnosis-suspended-p id)
         (message "Note is suspended."))
        (t
         (let ((type (gnosis-get 'type 'notes `(= id ,id))))
           (pcase type
             ("mcq" (gnosis-review-mcq id))
             ("basic" (gnosis-review-basic id))
             ("cloze" (gnosis-review-cloze id))
             (_ (error "Malformed note type")))))))

(defun gnosis-review-all-with-tags ()
  "Review all note(s) with specified tag(s)."
  (let ((notes (gnosis-select-by-tag (gnosis-prompt-tag)))
	(note-count 0))
    (cl-loop for note in notes
	     do (progn (gnosis-review-note note)
		       (setf note-count (1+ note-count))
		       (when (not (y-or-n-p "Review next?"))
			 (message "Review session finished. %d note(s) reviewed." note-count)
			 (cl-return)))
	     finally (message "Review session finished. %d note(s) reviewed." note-count))))

(defun gnosis-review-due-tags ()
  "Review due notes, with specified tag."
  (let ((notes (gnosis-select-by-tag
		(list (completing-read "Start session for tag: " (gnosis-review-due-notes--with-tags)))))
	(note-count 0))
    (cl-loop for note
	     in notes do (progn (gnosis-review-note note)
				(setf note-count (1+ note-count ))
				(when (not (y-or-n-p "Review next note?"))
				  (message "Review session finished. %d note(s) reviewed." note-count)
				  (cl-return)))
	     finally (message "Review session finished. %d note(s) reviewed." note-count))))

(defun gnosis-review-all-due-notes ()
  "Review all due notes."
  (let* ((due-notes (gnosis-review-get-due-notes))
         (note-count 0)
         (total-notes (length due-notes)))
    (if (null due-notes)
        (message "No due notes.")
      (when (y-or-n-p (format "You have %s total notes for review, start session?" total-notes))
	(cl-loop for note in due-notes
		 do (progn (gnosis-review-note (car note))
			   (setf note-count (+ note-count 1))
			   (when (not (y-or-n-p "Review next note?"))
			     (message "Review session finished. %d note(s) reviewed." note-count)
			     (cl-return)))
		 finally (message "Review session finished. %d note(s) reviewed." note-count))))))
;;;###autoload
(defun gnosis-review ()
  "Start gnosis review session."
  (interactive)
  (let ((review-type (completing-read "Review: " '("Due notes"
						   "Due notes of specified tag(s)"
						   "Notes with tag(s)"))))
    (pcase review-type
      ("Due notes" (gnosis-review-all-due-notes))
      ("Due notes of specified tag(s)" (gnosis-review-due-tags))
      ("Notes with tag(s)" (gnosis-review-all-with-tags)))))

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
