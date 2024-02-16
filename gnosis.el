;;; gnosis.el --- Spaced Repetition System For Note Taking & Self Testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis
;; Version: 0.1.6

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2") (emacsql "20240124"))

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

;; Gnosis, pronounced "noh-sis", is a spaced repetition system for
;; note taking & self testing, where notes are taken in a
;; Question/Answer/Explanation format & reviewed in spaced
;; intervals.
;; 
;; Gnosis can help you better understand and retain the material by
;; encouraging active engagement. It also provides a clear structure for
;; your notes & review sessions, making it easier to study.

;;; Code:

(require 'emacsql-sqlite)
(require 'cl-lib)

(require 'gnosis-algorithm)
(require 'vc)

(defgroup gnosis nil
  "Spaced Repetition System For Note Taking & Self Testing."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-dir (locate-user-emacs-file "gnosis")
  "Gnosis directory."
  :type 'directory
  :group 'gnosis)

(defcustom gnosis-cloze-string "__"
  "Gnosis string to represent a cloze."
  :type 'string
  :group 'gnosis)

(defcustom gnosis-string-difference 1
  "Threshold value for string comparison in Gnosis.

This variable determines the maximum acceptable Levenshtein distance
between two strings to consider them as similar."
  :type 'integer
  :group 'gnosis)

(defcustom gnosis-auto-push nil
  "Automatically run `gnosis-auto-push-command' at the end of every review session."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-mcq-display-choices t
  "When t, display choices for mcq notes during review.

Users that use a completion framework like ivy/helm/vertico may want
to set this to nil, as the choices will be displayed in the completion
framework's minibuffer."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-auto-push-command "push"
  "Git shell command to run at the end of a review session.

Command specified will be executed when `gnosis-auto-push' is enabled.

It should be provided as a string without the `git' prefix, assuming
that git is available in the system's PATH. For example, setting it
to \"push\" will execute the command 'git push'."
  :type 'string
  :group 'gnosis)

(defvar gnosis-images-dir (expand-file-name "images" gnosis-dir)
  "Gnosis images directory.")

(unless (file-directory-p gnosis-dir)
  (make-directory gnosis-dir)
  (make-directory gnosis-images-dir))

(defconst gnosis-db
  (emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))
  "Gnosis database file.")

(defvar gnosis-testing nil
  "When t, warn user he is in a testing environment.")

(defconst gnosis-db-version 1
  "Gnosis database version.")

(defvar gnosis-note-types '(MCQ Cloze Basic Double y-or-n)
  "Gnosis available note types.")

;;; Faces

(defgroup gnosis-faces nil
  "Faces used by gnosis."
  :group 'gnosis
  :tag "Gnosis Faces"
  :prefix 'gnosis-face)

(defface gnosis-face-extra
  '((t :inherit italic
       :foreground "#9C91E4"))
  "Face for extra-notes."
  :group 'gnosis-faces)

(defface gnosis-face-main
  '((t :inherit default))
  "Face for the main section from note."
  :group 'gnosis-face-faces)

(defface gnosis-face-seperator
  '((t :inherit warning))
  "Face for section seperator."
  :group 'gnosis-face)

(defface gnosis-face-directions
  '((t :inherit underline))
  "Face for gnosis directions."
  :group 'gnosis-face)

(defface gnosis-face-correct
  '((t :inherit match))
  "Face for user choice."
  :group 'gnosis-face)

(defface gnosis-face-cloze
  '((t :inherit cursor))
  "Face for clozes."
  :group 'gnosis-face)

(defface gnosis-face-false
  '((t :inherit error))
  "Face for user choice."
  :group 'gnosis-face)

(defface gnosis-face-hint
  '((t :inherit warning))
  "Face for user choice."
  :group 'gnosis-face)

(defface gnosis-face-cloze-unanswered
  '((t :inherit underline))
  "Face for user choice."
  :group 'gnosis-face)

(defface gnosis-face-next-review
  '((t :inherit bold))
  "Face for next review."
  :group 'gnosis-face)

(cl-defun gnosis-select (value table &optional (restrictions '1=1) (flatten nil))
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (let ((output (emacsql gnosis-db `[:select ,value :from ,table :where ,restrictions])))
    (if flatten
	(apply #'append output)
      output)))

(cl-defun gnosis--create-table (table &optional values)
  "Create TABLE for VALUES."
  (emacsql gnosis-db `[:create-table ,table ,values]))

(cl-defun gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (emacsql gnosis-db `[:drop-table ,table]))

(cl-defun gnosis--insert-into (table values)
  "Insert VALUES to TABLE."
  (emacsql gnosis-db `[:insert :into ,table :values ,values]))

(cl-defun gnosis-update (table value where)
  "Update records in TABLE with to new VALUE based on the given WHERE condition.

Example:
 (gnosis-update ='notes ='(= main \"NEW VALUE\") ='(= id 12))"
  (emacsql gnosis-db `[:update ,table :set ,value :where ,where]))

(cl-defun gnosis-get (value table &optional (restrictions '1=1))
  "Return caar of VALUE from TABLE, optionally with where RESTRICTIONS."
  (caar (gnosis-select value table restrictions)))

(defun gnosis-get-note-tags (id)
  "Return tags for note ID."
  (gnosis-get 'tags 'notes `(= id ,id)))

(defun gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql gnosis-db `[:delete :from ,table :where ,value]))

(cl-defun gnosis-completing-read (prompt options info &optional (face-for-info 'font-lock-doc-face))
  "A version of `completing-read' with text properties, padding & choosable face.
Returns selected option from OPTIONS.

WARNING: Do NOT use htis functions as is now!

PROMPT is a string to prompt with; normally it ends in a colon and a space.
OPTIONS is a list of strings.
INFO is a list of strings, which will be displayed as additional info for option
FACE-FOR-INFO is the face used to display info for option."
  (let* ((choices (cl-mapcar 'cons options info))
         (max-choice-length (apply #'max (mapcar #'length options)))
         (formatted-choices
          (mapcar (lambda (choice)
                    (cons (concat (format "%s" (car choice))
                                  (make-string (- max-choice-length (length (car choice))) ? )
                                  "      "
                                  (propertize (format "%s" (cdr choice)) 'face face-for-info))
                          (car choice)))
                  choices)))
    (cdr (assoc (completing-read prompt formatted-choices nil t)
		formatted-choices))))

(defun gnosis-replace-item-at-index (index new-item list)
  "Replace item at INDEX in LIST with NEW-ITEM."
  (cl-loop for i from 0 for item in list
           if (= i index) collect new-item
           else collect item))

(defun gnosis-display-question (id)
  "Display main row for note ID."
  (let ((question (gnosis-get 'main 'notes `(= id ,id))))
    (erase-buffer)
    (fill-paragraph (insert "\n"  (propertize question 'face 'gnosis-face-main)))))

(defun gnosis-display-mcq-options (id)
  "Display answer options for mcq note ID."
  (let ((options (apply #'append (gnosis-select 'options 'notes `(= id 1) t)))
	(option-num 1))
    (insert "\n\n" (propertize "Options:" 'face 'gnosis-face-directions))
    (cl-loop for option in options
	     do (insert (format "\n%s. %s" option-num option))
	     (setf option-num (1+ option-num)))))

(defun gnosis-display-cloze-sentence (sentence clozes)
  "Display cloze sentence for SENTENCE with CLOZES."
  (erase-buffer)
  (fill-paragraph
   (insert "\n"
	   (gnosis-cloze-replace-words sentence clozes (propertize gnosis-cloze-string 'face 'gnosis-face-cloze)))))

(defun gnosis-display-basic-answer (answer success user-input)
  "Display ANSWER.

When SUCCESS nil, display USER-INPUT as well"
  (insert "\n\n"
	  (propertize "Answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize answer 'face 'gnosis-face-correct))
  ;; Insert user wrong answer
  (when (not success)
    (insert "\n"
	    (propertize "Your answer:" 'face 'gnosis-face-directions)
	    " "
	    (propertize user-input 'face 'gnosis-face-false))))

(cl-defun gnosis-display-y-or-n-answer (&key answer success)
  "Display y-or-n answer for note ID.

ANSWER is the correct answer, either y or n. Answer is either 121 or
110, which are the char values for y & n respectively
SUCCESS is t when user-input is correct, else nil"
  (let ((answer (if (equal answer 121) "y" "n")))
    (insert
     "\n\n"
     (propertize "Answer:" 'face 'gnosis-face-directions)
     " "
     (propertize answer 'face (if success 'gnosis-face-correct 'gnosis-face-false)))))


(defun gnosis-display-hint (hint)
  "Display HINT."
  (let ((hint (or hint "")))
    (goto-char (point-max))
    (insert
     (propertize "\n\n-----\n" 'face 'gnosis-face-seperator)
     (propertize hint 'face 'gnosis-face-hint))))

(cl-defun gnosis-display-cloze-reveal (&key (cloze-char gnosis-cloze-string) replace (success t) (face nil))
  "Replace CLOZE-CHAR with REPLACE.

If FACE nil, propertize replace using `gnosis-face-correct', or
`gnosis-face-false' when (not SUCCESS). Else use FACE value."
  (goto-char (point-min))
  (search-forward cloze-char nil t)
  (replace-match (propertize replace 'face (if (not face)
					       (if success 'gnosis-face-correct 'gnosis-face-false)
					     face))))

(cl-defun gnosis-display-cloze-user-answer (user-input &optional (false t))
  "Display USER-INPUT answer for cloze note upon failed review.

If FALSE t, use gnosis-face-false face"
  (goto-char (point-max))
  (insert "\n\n"
	  (propertize "Your answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize user-input 'face (if false 'gnosis-face-false 'gnosis-face-correct))))

(defun gnosis-display-correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ note."
  (insert  "\n\n"
	   (propertize "Correct Answer:" 'face 'gnosis-face-directions)
	   " "
	   (propertize answer 'face 'gnosis-face-correct)
	   "\n"
	   (propertize "Your answer:" 'face 'gnosis-face-directions)
	   " "
	   (propertize user-choice 'face (if (string= answer user-choice)
					     'gnosis-face-correct
					   'gnosis-face-false))))

(defun gnosis-display-extra (id)
  "Display extra information for note ID."
  (let ((extras (or (gnosis-get 'extra-notes 'extras `(= id ,id)) "")))
    (goto-char (point-max))
    (insert (propertize "\n\n-----\n" 'face 'gnosis-face-seperator))
    (fill-paragraph (insert "\n" (propertize extras 'face 'gnosis-face-extra)))))

(defun gnosis-display-image (id)
  "Display image for note ID."
  (let* ((img (gnosis-get 'images 'extras `(= id ,id)))
	 (path-to-image (concat (file-name-as-directory gnosis-images-dir) img))
	 (image (create-image path-to-image 'png nil :width 500 :height 300)))
    (when img
      (insert "\n\n")
      (insert-image image))))

(defun gnosis-display-next-review (id)
  "Display next interval for note ID."
  (let ((interval (gnosis-get 'next-rev 'review-log `(= id ,id))))
    (goto-char (point-max))
    (insert "\n\n"
	    (propertize "Next review:" 'face 'gnosis-face-directions)
	    " "
	    (propertize (format "%s" interval) 'face 'gnosis-face-next-review))))

(cl-defun gnosis--prompt (prompt &optional (downcase nil) (split nil))
  "PROMPT user for input until `q' is given.

The user is prompted to provide input for the PROMPT message.
Returns the list of non-q inputs in reverse order of their entry.

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

;;;###autoload
(defun gnosis-add-deck (name)
  "Create deck with NAME."
  (interactive (list (read-string "Deck Name: ")))
  (when gnosis-testing
    (unless (y-or-n-p "You are using a testing environment! Continue?")
      (error "Aborted")))
  (if (gnosis-get 'name 'decks `(= name ,name))
      (error "Deck `%s' already exists" name)
    (gnosis--insert-into 'decks `([nil ,name]))
    (message "Created deck '%s'" name)))

(defun gnosis--get-deck-name ()
  "Return name from table DECKS."
  (when (equal (gnosis-select 'name 'decks) nil)
    (error "No decks found"))
  (completing-read "Deck: " (gnosis-select 'name 'decks)))

(cl-defun gnosis--get-deck-id (&optional (deck (gnosis--get-deck-name)))
  "Return id for DECK name."
  (gnosis-get 'id 'decks `(= name ,deck)))

;;;###autoload
(defun gnosis-delete-deck (deck)
  "Delete DECK."
  (interactive (list (gnosis--get-deck-name)))
  (gnosis--delete 'decks `(= name ,deck))
  (message "Deleted deck %s" deck))

;; TODO: Redo this as a single function
(cl-defun gnosis-suspend-note (id &optional (suspend 1))
  "Suspend note with ID.

SUSPEND: 1 to suspend, 0 to unsuspend."
  (gnosis-update 'review-log `(= suspend ,suspend) `(= id ,id)))

(cl-defun gnosis-suspend-deck (&optional (deck (gnosis--get-deck-id)))
  "Suspend all note(s) with DECK id.

When called with a prefix, unsuspends all notes in deck."
  (let ((notes (gnosis-select 'id 'notes `(= deck-id ,deck)))
	(suspend (if current-prefix-arg 0 1))
	(note-count 0))
    (cl-loop for note in notes
	     do (gnosis-update 'review-log `(= suspend ,suspend) `(= id ,(car note)))
	     (setq note-count (1+ note-count))
	     finally (if (equal suspend 0)
			 (message "Unsuspended %s notes" note-count)
		       (message "Suspended %s notes" note-count)))))

(defun gnosis-suspend-tag ()
  "Suspend all note(s) with tag.

When called with a prefix, unsuspends all notes for tag."
  (let ((notes (gnosis-select-by-tag (gnosis-tag-prompt nil t)))
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


(defun gnosis-add-note-fields (deck type main options answer extra tags suspend image second-image)
  "Insert fields for new note.

DECK: Deck NAME, as a string, for new note.
TYPE: Note type e.g \"mcq\"
MAIN: Note's main part
OPTIONS: Note's options, e.g choices for mcq for OR hints for
cloze/basic type
ANSWER: Correct answer for note, for MCQ is an integer while for
cloze/basic a string/list of the right answer(s)
EXTRA: Extra information to display after answering note
TAGS: Tags to organize notes
SUSPEND: Integer value of 1 or 0, where 1 suspends the card
IMAGE: Image to display during review.
SECOND-IMAGE: Image to display after user-input.

NOTE: If a gnosis--insert-into fails, the whole transaction will be
 (or at least it should). Else there will be an error for foreign key
 constraint."
  (condition-case nil
      (progn
	;; Refer to `gnosis-db-schema-SCHEMA' e.g `gnosis-db-schema-review-log'
        (gnosis--insert-into 'notes   `([nil ,type ,main ,options ,answer ,tags ,(gnosis--get-deck-id deck)]))
        (gnosis--insert-into 'review  `([nil ,gnosis-algorithm-ef ,gnosis-algorithm-ff ,gnosis-algorithm-interval]))
        (gnosis--insert-into 'review-log `([nil ,(gnosis-algorithm-date) ,(gnosis-algorithm-date) 0 0 0 0 ,suspend 0]))
        (gnosis--insert-into 'extras `([nil ,extra ,image ,second-image])))
    (error (message "An error occurred during insertion"))))


;; Adding note(s) consists firstly of a hidden 'gnosis-add-note--TYPE'
;; function that does the computation & error checking to generate a
;; note from given input. Secondly, 'gnosis-add-note-TYPE' normal
;; function, which prompts for user input and passes it to the hidden
;; function.

(cl-defun gnosis-add-note--mcq (&key deck question choices correct-answer
				     extra (image nil) tags (suspend 0) (second-image nil))
  "Create a NOTE with a list of multiple CHOICES.

MCQ type consists of a main `QUESTION' that is displayed to the user.
The user will be prompted to select the correct answer from a list of
`CHOICES'. The `CORRECT-ANSWER' should be the index of the correct
choice in the `CHOICES' list. Each note must correspond to one `DECK'.

`IMAGE' Image to display before user-input
`SECOND-IMAGE' Image to display after user-input
`EXTRA' are extra information displayed after an answer is given.
`TAGS' are used to organize questions.
`SUSPEND' is a binary value, where 1 is for suspend."
  (cond ((or (not (numberp correct-answer)) (equal correct-answer 0))
	 (error "Correct answer value must be the index number of the correct answer"))
	((null tags)
	 (setf tags 'untagged)))
  (gnosis-add-note-fields deck "mcq" question choices correct-answer extra tags suspend image second-image))

(defun gnosis-add-note-mcq ()
  "Add note(s) of type `MCQ' interactively to selected deck.

Create a note type MCQ for specified deck, that consists of:
STEM: The question or problem statement
OPTIONS: Options for the user to select
ANSWER: Answer is the index NUMBER of the correct answer from OPTIONS.
EXTRA: Information to display after user-input
TAGS: Used to organize notes

Refer to `gnosis-add-note--mcq' for more."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `MCQ' to `%s' deck? " deck))
      (gnosis-add-note--mcq :deck deck
			    :question (read-string "Question: ")
			    :choices (gnosis--prompt "Choices")
			    :correct-answer (string-to-number (read-string "Which is the correct answer (number)? "))
			    :extra (read-string "Extra: ")
			    :tags (gnosis-tag-prompt)))))

(cl-defun gnosis-add-note--basic (&key deck question hint answer
				       extra (image nil) tags (suspend 0) (second-image nil))
  "Add Basic type note.

DECK: Deck name for note.
QUESTION: Quesiton to display for note.
ANSWER: Answer for QUESTION, which user will be prompted to type
HINT: Hint to display during review, before user-input.
EXTRA: Extra information to display after user-input/giving an answer.
IMAGE: Image to display before user-input.
TAGS: Tags used to organize notes
SUSPEND: Binary value of 0 & 1, when 1 note will be ignored."
  (gnosis-add-note-fields deck "basic" question hint answer extra tags suspend image second-image))

(defun gnosis-add-note-basic ()
  "Add note(s) of type `Basic' interactively to selected deck.

Basic note type is a simple question/answer note, where user first
sees a \"main\" part, which is usually a question, and he is prompted
to input the answer.

Refer to `gnosis-add-note--basic' for more."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `basic' to `%s' deck? " deck))
      (gnosis-add-note--basic :deck deck
			      :question (read-string "Question: ")
			      :answer (read-string "Answer: ")
			      :hint (read-string "Hint: ")
			      :extra (read-string "Extra: ")
			      :tags (gnosis-tag-prompt)))))

(cl-defun gnosis-add-note--double (&key deck question hint answer extra (image nil) tags (suspend 0) (second-image nil))
  "Add Double type note.

Essentially, a \"note\" that generates 2 basic notes. The second one
reverses question/answer.

DECK: Deck name for note.
QUESTION: Quesiton to display for note.
ANSWER: Answer for QUESTION, which user will be prompted to type
HINT: Hint to display during review, before user-input.
EXTRA: Extra information to display after user-input/giving an answer.
IMAGE: Image to display before user-input.
TAGS: Tags used to organize notes
SUSPEND: Binary value of 0 & 1, when 1 note will be ignored."
  (gnosis-add-note-fields deck "basic" question hint answer extra tags suspend image second-image)
  (gnosis-add-note-fields deck "basic" answer hint question extra tags suspend image second-image))

(defun gnosis-add-note-double ()
  "Add note(s) of type double interactively to selected deck.

Essentially, a \"note\" that generates 2 basic notes. The second one
reverses question/answer.

Refer to `gnosis-add-note--double' for more."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `double' to `%s' deck? " deck))
      (gnosis-add-note--double :deck deck
			       :question (read-string "Question: ")
			       :answer (read-string "Answer: ")
			       :image (when (y-or-n-p "Add image to display during review?")
					(completing-read "Select image: " (gnosis-directory-files)))
			       :hint (read-string "Hint: ")
			       :extra (read-string "Extra: ")
			       :tags (gnosis-tag-prompt)))))

(cl-defun gnosis-add-note--y-or-n (&key deck question hint answer extra (image nil) tags (suspend 0) (second-image nil))
  "Add y-or-n type note.

DECK: Deck name for note.
QUESTION: Quesiton to display for note.
ANSWER: Answer for QUESTION, either `121' (char value for yes) or `110'
        (char value for no).
HINT: Hint to display during review, before user-input.
EXTRA: Extra information to display after user-input/giving an answer.
IMAGE: Image to display before user-input.
TAGS: Tags used to organize notes
SUSSPEND: Binary value of 0 & 1, when 1 note will be ignored.
SECOND-IMAGE: Image to display after user-input."
  (gnosis-add-note-fields deck "y-or-n" question hint answer extra tags suspend image second-image))

(defun gnosis-add-note-y-or-n ()
  "Add note(s) of type `y-or-n'.

refer to `gnosis-add-note--y-or-n' for more information about keyword values."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `y-or-n' to `%s' deck? " deck))
      (gnosis-add-note--y-or-n :deck deck
			       :question (read-string "Question: ")
                               :answer (read-char-choice "Answer: [y] or [n]? " '(?y ?n))
			       :hint (read-string "Hint: ")
			       :extra (read-string "Extra: ")
			       :tags (gnosis-tag-prompt)))))


(cl-defun gnosis-add-note--cloze (&key deck note hint tags (suspend 0) extra (image nil) (second-image nil))
  "Add cloze type note.

DECK: Deck name for note.
NOTE: Note with clozes, format for clozes is as follows:
      This is a {c1:cloze} note type.
      This is a {{c1::cloze}} note type.

Anki like syntax is supported with double brackets & double colon, as
well as single brackets({}) and colon(:), or even a mix.

For each cX: tag, there will be gerenated a cloze note type.
Example:
      {c1:Preformed enterotoxins} from
      {c2:Staphylococcus aureus} causes {c3:rapid} onset
      food poisoning

Generates 3 cloze note types. Where the \"main\" part of the note is the full
note, with the cloze(s) extracted & used as the \"answer\".

One cloze note may have multiple clozes
Example:
      {c1:Streptococcus agalactiae (GBS)} and {c1:Listeria
      monocytogenes} are CAMP test positive
   
HINT: Hint to display during review, before user-input.
   NOTE! In gnosis-db, hint is referred to as `options', same column
   options used in mcq.
IMAGE: Image to display before user-input.
TAGS: Tags used to organize notes
SUSPEND: When t, note will be ignored.
EXTRA: Extra information displayed after user-input.
SECOND-IMAGE: Image to display after user-input."
  (let ((notags-note (gnosis-cloze-remove-tags note))
	(clozes (gnosis-cloze-extract-answers note)))
    (cl-loop for cloze in clozes
	     do (gnosis-add-note-fields deck "cloze" notags-note hint cloze extra tags suspend image second-image))))

(defun gnosis-add-note-cloze ()
  "Add note(s) of type cloze interactively to selected deck.

Note with clozes, format for clozes is as follows:
      This is a {c1:cloze} note type.
      This is a {{c1::cloze}} note type.

Anki like syntax is supported with double brackes and colon, as well
as single brackets({}) and colon(:), or even a mix.

One cloze note may have multiple clozes
Example:
      {c1:Streptococcus agalactiae (GBS)} and {c1:Listeria
      monocytogenes} are CAMP test positive

For each cX: tag, there will be gerenated a cloze note type.
Example:
      {c1:Preformed enterotoxins} from
      {c2:Staphylococcus aureus} causes {c3:rapid} onset
      food poisoning

Generates 3 cloze note types. Where the \"main\" part of the note is
the full note, with the cloze(s) extracted & used as the \"answer\".

See `gnosis-add-note--cloze' for more reference."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `cloze' to `%s' deck? " deck))
      (gnosis-add-note--cloze :deck deck
			      :note (read-string "Question: ")
			      :hint (read-string "Hint: ")
			      :extra (read-string "Extra: ")
			      :tags (gnosis-tag-prompt)))))

;;;###autoload
(defun gnosis-add-note (type)
  "Create note(s) as TYPE interactively."
  (interactive (list (completing-read "Type: " gnosis-note-types nil t)))
  (when gnosis-testing
    (unless (y-or-n-p "You are using a testing environment! Continue?")
      (error "Aborted")))
  (let ((func-name (intern (format "gnosis-add-note-%s" (downcase type)))))
    (if (fboundp func-name)
        (funcall func-name)
      (message "No such type."))))

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
  "In STRING replace only the first occurrence of each word in WORDS with NEW."
  (cl-assert (listp words))
  (cl-loop for word in words
           do (when (string-match (concat "\\<" word "\\>") string)
                (setq string (replace-match new t t string))))
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
  (<= (string-distance (downcase (replace-regexp-in-string "\\s-" "" str1))
		       (downcase (replace-regexp-in-string "\\s-" "" str2)))
      gnosis-string-difference))


(defun gnosis-directory-files (&optional dir regex)
  "Return a list of file paths, relative to DIR directory.

DIR is the base directory path from which to start the recursive search.
REGEX is the regular expression pattern to match the file names against.

This function traverses the subdirectories of DIR recursively,
collecting file paths that match the regular expression. The file
paths are returned as a list of strings, with each string representing
a relative file path to DIR.

By default, DIR value is `gnosis-images-dir' & REGEX value is \"^[^.]\""
  (let ((dir (or dir gnosis-images-dir))
	(regex (or regex "^[^.]")))
    (apply #'append
           (cl-loop for path in (directory-files dir t directory-files-no-dot-files-regexp)
                    if (file-directory-p path)
                    collect (mapcar (lambda (file) (concat (file-relative-name path dir) "/" file))
                                    (gnosis-directory-files path regex))
                    else if (string-match-p regex (file-name-nondirectory path))
                    collect (list (file-relative-name path dir))))))

(defun gnosis-select-image (&optional prompt)
  "Return PATH for file in `gnosis-images-dir'.

Optionally, add cusotm PROMPT."
  (let* ((prompt (or prompt "Select image: "))
	 (image (completing-read prompt (gnosis-directory-files gnosis-images-dir))))
    image))

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (cl-loop for tags in (gnosis-select 'tags 'notes '1=1 t)
           nconc tags into all-tags
           finally return (delete-dups all-tags)))

(defun gnosis-select-by-tag (input-tags)
  "Return note ID's for every note with INPUT-TAGS."
  (unless (listp input-tags)
    (error "`input-tags' need to be a list"))
  (cl-loop for (id tags) in (emacsql gnosis-db [:select [id tags] :from notes])
           when (and (cl-every (lambda (tag) (member tag tags)) input-tags)
		     (not (gnosis-suspended-p id)))
           collect id))

(defun gnosis-suspended-p (id)
  "Return t if note with ID is suspended."
  (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1))

(defun gnosis-get-deck-due-notes (&optional deck-id)
  "Return due notes for deck, with value of DECK-ID.

if DUE is t, return only due notes"
  (let* ((deck (or deck-id (gnosis--get-deck-id)))
	 (notes (gnosis-select 'id 'notes `(= deck-id ,deck) t)))
    (cl-loop for note in notes
	     when (and (not (gnosis-suspended-p note))
		       (gnosis-review-is-due-p note))
	     collect note)))

(defun gnosis-past-or-present-p (date)
  "Compare the input DATE with the current date.
Return t if DATE is today or in the past, nil if it's in the future.
DATE is a list of the form (year month day)."
  (let* ((now (gnosis-algorithm-date))
         (time-now (encode-time 0 0 0 (nth 2 now) (nth 1 now) (nth 0 now)))
         (time-date (encode-time 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
    (not (time-less-p time-now time-date))))


(cl-defun gnosis-tag-prompt (&key (prompt "Selected tags") (match nil) (due nil))
  "PROMPT user to select tags, until they enter `q'.
Prompt user to select tags, generated from `gnosis-get-tags--unique'.

PROMPT: Prompt string value
MATCH: Require match, t or nil value
DUE: if t, return tags for due notes from `gnosis-due-tags'.
Returns a list of unique tags."
  (let* ((tags '())
         (tag ""))
    (while (not (string= tag "q"))
      (setf tag (completing-read (concat prompt (format " %s (q for quit): " tags))
				 (cons "q" (if due (gnosis-review-get-due-tags)
					     (gnosis-get-tags--unique)))
				 nil match))
      (unless (or (string= tag "q") (member tag tags))
        (push tag tags)))
    (reverse tags)))

;; Review
;;;;;;;;;;
(defun gnosis-review-is-due-p (note-id)
  "Check if note with value of NOTE-ID for id is due for review.

Check if it's suspended, and if it's due today."
  (and (not (gnosis-suspended-p note-id))
       (gnosis-review-is-due-today-p note-id)))

(defun gnosis-review-is-due-today-p (id)
  "Return t if note with ID is due today.

This function ignores if note is suspended. Refer to
`gnosis-review-is-due-p' if you need to check for suspended value as
well."
  (let ((next-rev (gnosis-get 'next-rev 'review-log `(= id ,id))))
    (gnosis-past-or-present-p next-rev)))

(defun gnosis-review-get-due-notes ()
  "Return a list due notes id for current date."
  (let ((notes (gnosis-select 'id 'notes '1=1 t)))
    (cl-loop for note in notes
	     when (gnosis-review-is-due-p note)
	     collect note)))

(defun gnosis-review-get-due-tags ()
  "Return a list of due note tags."
  (let ((due-notes (gnosis-review-get-due-notes)))
    (cl-remove-duplicates
     (cl-mapcan (lambda (note-id)
                  (gnosis-get-note-tags note-id))
	        due-notes)
     :test #'equal)))

(defun gnosis-review--algorithm (id success)
  "Return next review date & ef for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure.

Returns a list of the form ((yyyy mm dd) ef)."
  (let ((ff gnosis-algorithm-ff)
	(ef (nth 2 (gnosis-get 'ef 'review `(= id ,id))))
	(t-success (gnosis-get 't-success 'review-log `(= id ,id)))
	(c-success (gnosis-get 'c-success 'review-log `(= id ,id)))
	(c-fails (gnosis-get 'c-fails 'review-log `(= id ,id)))
	(t-fails (gnosis-get 't-fails 'review-log `(= id ,id)))
	(initial-interval (gnosis-get 'interval 'review `(= id ,id))))
    (gnosis-algorithm-next-interval :last-interval (max (gnosis-review--get-offset id) 1) ;; last-interv always >=1
				    :review-num (gnosis-get 'n 'review-log `(= id ,id))
				    :ef ef
				    :success success
				    :failure-factor ff
				    :successful-reviews t-success
				    :successful-reviews-c  c-success
				    :fails-c c-fails
				    :fails-t t-fails
				    :initial-interval initial-interval)))

(defun gnosis-review--get-offset (id)
  "Return offset for note with value of id ID."
  (let ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev)))

(defun gnosis-review-round (num)
  "Round NUM to 2 decimals.

This function is used to round floating point numbers to 2 decimals,
such as the easiness factor (ef)."
  (/ (round (* num 100.00)) 100.00))

(defun gnosis-review-new-ef (id success)
  "Return new ef for note with value of id ID.

Returns a list of the form (ef-increase ef-decrease ef).
SUCCESS is a boolean value, t for success, nil for failure."
  (let ((ef (nth 1 (gnosis-review--algorithm id success)))
	(old-ef (gnosis-get 'ef 'review `(= id ,id))))
    (cl-substitute (gnosis-review-round ef) (nth 2 old-ef) old-ef)))

(defun gnosis-review--update (id success)
  "Update review-log for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure."
  (let ((ef (gnosis-review-new-ef id success))
	(next-rev (car (gnosis-review--algorithm id success))))
    ;; Update review-log
    (gnosis-update 'review-log `(= last-rev ',(gnosis-algorithm-date)) `(= id ,id))
    (gnosis-update 'review-log `(= next-rev ',next-rev) `(= id ,id))
    (gnosis-update 'review-log `(= n (+ 1 ,(gnosis-get 'n 'review-log `(= id ,id)))) `(= id ,id))
    ;; Update review
    (gnosis-update 'review `(= ef ',ef) `(= id ,id))
    (if success
	(progn (gnosis-update 'review-log
			      `(= c-success ,(1+ (gnosis-get 'c-success 'review-log `(= id ,id)))) `(= id ,id))
	       (gnosis-update 'review-log `(= t-success ,(1+ (gnosis-get 't-success 'review-log `(= id ,id))))
			      `(= id ,id))
	       (gnosis-update 'review-log `(= c-fails 0) `(= id ,id)))
      (gnosis-update 'review-log `(= c-fails ,(1+ (gnosis-get 'c-fails 'review-log `(= id ,id)))) `(= id ,id))
      (gnosis-update 'review-log `(= t-fails ,(1+ (gnosis-get 't-fails 'review-log `(= id ,id)))) `(= id ,id))
      (gnosis-update 'review-log `(= c-success 0) `(= id ,id)))))

(defun gnosis-review-mcq (id)
  "Display multiple choice answers for question ID."
  (gnosis-display-image id)
  (gnosis-display-question id)
  (when gnosis-mcq-display-choices
    (gnosis-display-mcq-options id))
  (let* ((choices (gnosis-get 'options 'notes `(= id ,id)))
	 (answer (nth (- (gnosis-get 'answer 'notes `(= id ,id)) 1) choices))
	 (user-choice (gnosis-mcq-answer id)))
    (if (string= answer user-choice)
        (progn
	  (gnosis-review--update id t)
	  (message "Correct!"))
      (gnosis-review--update id nil)
      (message "False"))
    (gnosis-display-correct-answer-mcq answer user-choice)
    (gnosis-display-extra id)
    (gnosis-display-next-review id)))

(defun gnosis-review-basic (id)
  "Review basic type note for ID."
  (gnosis-display-image id)
  (gnosis-display-question id)
  (gnosis-display-hint (gnosis-get 'options 'notes `(= id ,id)))
  (let* ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	 (user-input (read-string "Answer: "))
	 (success (gnosis-compare-strings answer user-input)))
    (gnosis-display-basic-answer answer success user-input)
    (gnosis-display-extra id)
    (gnosis-review--update id success)
    (gnosis-display-next-review id)))

(defun gnosis-review-y-or-n (id)
  "Review y-or-n type note for ID."
  (gnosis-display-image id)
  (gnosis-display-question id)
  (gnosis-display-hint (gnosis-get 'options 'notes `(= id ,id)))
  (let* ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	 (user-input (read-char-choice "[y]es or [n]o: " '(?y ?n)))
	 (success (equal answer user-input)))
    (gnosis-display-y-or-n-answer :answer answer :success success)
    (gnosis-display-extra id)
    (gnosis-review--update id success)
    (gnosis-display-next-review id)))

(defun gnosis-review-cloze--input (cloze)
  "Prompt for user input during cloze review.

If user-input is equal to CLOZE, return t."
  (let ((user-input (read-string "Answer: ")))
    (cons (gnosis-compare-strings user-input cloze) user-input)))

(defun gnosis-review-cloze-reveal-unaswered (clozes)
  "Reveal CLOZES.

Used to reveal all clozes left with `gnosis-face-cloze-unanswered' face."
  (cl-loop for cloze in clozes do (gnosis-display-cloze-reveal :replace cloze
							       :face 'gnosis-face-cloze-unanswered)))

(defun gnosis-review-cloze (id)
  "Review cloze type note for ID."
  (let* ((main (gnosis-get 'main 'notes `(= id ,id)))
	 (clozes (gnosis-get 'answer 'notes `(= id ,id)))
	 (num 1)
	 (clozes-num (length clozes))
	 (hint (gnosis-get 'options 'notes `(= id ,id))))
    (gnosis-display-image id)
    (gnosis-display-cloze-sentence main clozes)
    (gnosis-display-hint hint)
    (cl-loop for cloze in clozes
	     do (let ((input (gnosis-review-cloze--input cloze)))
		  (if (equal (car input) t)
		      ;; Reveal only one cloze
		      (progn (gnosis-display-cloze-reveal :replace cloze)
			     (setf num (1+ num)))
		    ;; Reveal cloze for wrong input, with `gnosis-face-false'
		    (gnosis-display-cloze-reveal :replace cloze :success nil)
		    ;; Do NOT remove the _when_ statement, unexpected
		    ;; bugs occur if so depending on the number of
		    ;; clozes.
		    (when (< num clozes-num) (gnosis-review-cloze-reveal-unaswered clozes))
		    (gnosis-display-cloze-user-answer (cdr input))
		    (gnosis-review--update id nil)
		    (cl-return)))
	     ;; Update note after all clozes are revealed successfully
	     finally (gnosis-review--update id t)))
  (gnosis-display-extra id)
  (gnosis-display-next-review id))

(defun gnosis-review-note (id)
  "Start review for note with value of id ID, if note is unsuspended."
  (when (gnosis-suspended-p id)
    (message "Suspended note with id: %s" id)
    (sit-for 0.3)) ;; this should only occur in testing/dev cases
  (let* ((type (gnosis-get 'type 'notes `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s" (downcase type)))))
    (if (fboundp func-name)
        (progn
          (with-current-buffer (switch-to-buffer (get-buffer-create "*gnosis*"))
            (gnosis-mode)
            (funcall func-name id)))
      (error "Malformed note type: '%s'" type))))

(defun gnosis-review-commit (note-num)
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one. It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number of notes.

NOTE-NUM: The number of notes reviewed in the session."
  (let ((git (executable-find "git"))
	(default-directory gnosis-dir))
    (unless git
      (error "Git not found, please install git"))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-create-repo 'Git))
    ;; TODO: Redo this using vc
    (shell-command (concat git " add " (shell-quote-argument "gnosis.db")))
    (shell-command (concat git " commit -m "
			   (shell-quote-argument (concat (format "Total notes for session: %d " note-num)))))
    (when gnosis-auto-push
      (shell-command (concat git " " gnosis-auto-push-command)))
    (message "Review session finished. %d notes reviewed." note-num)))

(defun gnosis-review--session (notes)
  "Start review session for NOTES.

NOTES: List of note ids"
  (let ((note-count 0))
    (if (null notes)
	(message "No notes for review.")
      (when (y-or-n-p (format "You have %s total notes for review, start session?" (length notes)))
	(cl-loop for note in notes
		 do (gnosis-review-note note)
		 (setf note-count (1+ note-count))
		 (pcase (car (read-multiple-choice
			      "Note actions"
			      '((?n "next")
				(?s "suspend")
				(?e "edit")
				(?q "quit"))))
		   (?n nil)
		   (?s (gnosis-suspend-note note))
		   (?e (gnosis-edit-note note)
		       (recursive-edit))
		   (?q (gnosis-review-commit note-count)
		       (cl-return)))
		 finally (gnosis-review-commit note-count))))))


;; Editing notes

(defun gnosis-edit-note (id)
  "Edit note with value of id ID."
  (pcase (completing-read "Edit: " '(contents ef) nil t)
    ("contents" (gnosis-edit-note-contents id))
    ("ef" (gnosis-edit-ef id))
    (_ (message "No such value."))))

(defun gnosis-edit-ef (id)
  "Edit easiness factor values for note with id value ID."
  (let ((ef-full (caar (gnosis-select 'ef 'review `(= id ,id))))
	(old-value-index (pcase (completing-read "Change Factor: " '("Increase" "Decrease" "Total"))
			   ("Total" 2)
			   ("Decrease" 1)
			   ("Increase" 0)))
	(new-value (float (string-to-number (read-string "New value: ")))))
    ;; error checking.
    (cond ((>= 0 new-value) (error "New value needs to be a number & higher than `0'"))
	  ;; Check if when total-ef is selected, new value is higher than 1.3
	  ((and (>= 1.3 new-value) (= old-value-index 2) (error "New total ef needs to be higher than `1.3'"))))
    ;; Use `gnosis-replace-item-at-index' to generate new list with
    ;; new ef value. Change ef value at gnosis-db using
    ;; `gnosis-update'
    (gnosis-update 'review `(= ef ',(gnosis-replace-item-at-index old-value-index new-value ef-full)) `(= id ,id))))

(defun gnosis-edit-note-contents (id)
  "Edit the contents of a note with the given ID.

This function creates an Emacs Lisp buffer named *gnosis-edit* and populates it
with the values of the note identified by the specified ID. The note values are
inserted as keywords for the `gnosis-edit-update-note' function.

To make changes, edit the values in the buffer, and then evaluate the
`gnosis-edit-update-note' expression to save the changes.

The note fields that will be shown in the buffer are:
   - ID: The identifier of the note.
   - MAIN: The main content of the note.
   - OPTIONS: Additional options related to the note.
   - ANSWER: The answer associated with the note.
   - TAGS: The tags assigned to the note.
   - EXTRA-NOTES: Any extra notes for the note.
   - IMAGE: An image associated with the note.
   - SECOND-IMAGE: Another image associated with the note.

The buffer automatically indents the expressions for readability.
After finishing editing, evaluate the entire expression to apply the
changes."
  (let ((id (gnosis-get 'id 'notes `(= id ,id)))
	(main (gnosis-get 'main 'notes `(= id ,id)))
	(options (gnosis-get 'options 'notes `(= id ,id)))
	(answer (gnosis-get 'answer 'notes `(= id ,id)))
	(tags (gnosis-get 'tags 'notes `(= id ,id)))
	(extra-notes (gnosis-get 'extra-notes 'extras `(= id ,id)))
	(image (gnosis-get 'images 'extras `(= id ,id)))
	(second-image (gnosis-get 'extra-image 'extras `(= id ,id))))
    (with-current-buffer (switch-to-buffer (get-buffer-create "*gnosis-edit*"))
      (gnosis-edit-mode)
      (erase-buffer)
      (insert ";;\n;; You are editing a gnosis note. DO NOT change the value of id.\n\n")
      (insert "(gnosis-edit-update-note ")
      (cl-loop for (field value) in `((id ,id)
				      (main ,main)
				      (options ,options)
				      (answer ,answer)
				      (tags ,tags)
				      (extra-notes ,extra-notes)
				      (image ,image)
				      (second-image ,second-image))
	       do (cond ((eq field 'id)
			 (insert (format ":id %s \n" (propertize (number-to-string value) 'read-only t))))
			((numberp value)
			 (insert (format ":%s %s\n" field value)))
			((and (listp value)
			      (not (equal value nil)))
			 (insert (format ":%s '%s\n" field (format "%s" (cl-loop for item in value
										 collect (format "\"%s\"" item))))))
			((null value)
			 (insert (format ":%s %s\n" field 'nil)))
			(t (insert (format ":%s \"%s\"\n" field value)))))
      (delete-char -1) ;; delete extra line
      (insert ")")
      (insert "\n;; After finishing editing, save changes with `<C-c> <C-c>'\n;; Do NOT exit without saving.")
      (indent-region (point-min) (point-max)))))

(defun gnosis-edit-save-exit ()
  "Save edits and exit."
  (interactive)
  (eval-buffer)
  (kill-buffer)
  (exit-recursive-edit))

(defvar-keymap gnosis-edit-mode-map
  :doc "gnosis-edit keymap"
  "C-c C-c" #'gnosis-edit-save-exit)

(define-derived-mode gnosis-edit-mode emacs-lisp-mode "Gnosis EDIT"
  "Gnosis Edit Mode."
  :interactive t
  :lighter " Gnosis Edit"
  :keymap gnosis-edit-mode-map)


(cl-defun gnosis-edit-update-note (&key id main options answer tags (extra-notes nil) (image nil) (second-image nil))
  "Update note with id value of ID.

ID: Note id
MAIN: Main part of note, the stem part of MCQ, question for basic, etc.
OPTIONS: Options for mcq type notes/Hint for basic & cloze type notes
ANSWER: Answer for MAIN, user is asked for input, if equal user-input
= answer review is marked as successfull
TAGS: Tags for note, used to organize & differentiate between notes
EXTRA-NOTES: Notes to display after user-input
IMAGE: Image to display before user-input
SECOND-IMAGE: Image to display after user-input"
  ;; Construct the update clause for the emacsql update statement.
  (cl-loop for (field . value) in
           `((main . ,main)
             (options . ,options)
             (answer . ,answer)
             (tags . ,tags)
             (extra-notes . ,extra-notes)
             (image . ,image)
             (second-image . ,second-image))
           when value
           do (cond ((memq field '(extra-notes image second-image))
		     (gnosis-update 'extras `(= ,field ,value) `(= id ,id)))
		    ((listp value)
		     (gnosis-update 'notes `(= ,field ',value) `(= id ,id)))
		    (t (gnosis-update 'notes `(= ,field ,value) `(= id ,id))))))

(cl-defun gnosis-get-notes-for-deck (&optional (deck (gnosis--get-deck-id)))
  "Return a list of ID vlaues for each note with value of deck-id DECK."
  (gnosis-select 'id 'notes `(= deck-id ,deck) '1=1 t))

(defun gnosis-export-note (id)
  "Export fields for note with value of id ID.

ID: Identifier of the note to export.

This function retrieves the fields of a note with the given ID and
inserts them into the current buffer. Each field is represented as a
property list entry. The following fields are exported: type, main,
options, answer, tags, extra-notes, image, and second-image.

The exported fields are formatted as key-value pairs with a colon,
e.g., :field value. The fields are inserted sequentially into the
buffer. For certain field values, like lists or nil, special
formatting is applied.

If the value is a list, the elements are formatted as strings and
enclosed in double quotes.

If the value is nil, the field is exported as :field nil.

All other values are treated as strings and exported with double
quotes.

The final exported note is indented using the `indent-region' function
to improve readability."
  (let ((type (gnosis-get 'type 'notes `(= id ,id)))
	(main (gnosis-get 'main 'notes `(= id ,id)))
	(options (gnosis-get 'options 'notes `(= id ,id)))
	(answer (gnosis-get 'answer 'notes `(= id ,id)))
	(tags (gnosis-get 'tags 'notes `(= id ,id)))
	(extra-notes (gnosis-get 'extra-notes 'extras `(= id ,id)))
	(image (gnosis-get 'images 'extras `(= id ,id)))
	(second-image (gnosis-get 'extra-image 'extras `(= id ,id))))
    (cl-loop for (field . value) in `((type . ,type)
				      (main . ,main)
				      (options . ,options)
				      (answer . ,answer)
				      (tags . ,tags)
				      (extra-notes . ,extra-notes)
				      (image . ,image)
				      (second-image . ,second-image))
	     do (cond ((member field '(extra-notes image second-image))
		       (insert (format ":%s \"%s\"\n" field value)))
		      ((numberp value)
		       (insert (format ":%s %s\n" field value)))
		      ((listp value)
		       (insert (format ":%s %s\n" field (format "%s" (cl-loop for item in value
									      collect (format "\"%s\"" item))))))
		      ((equal value nil)
		       (insert (format ":%s %s\n" field 'nil)))
		      (t (insert (format ":%s \"%s\"\n" field value))
			 (indent-region (point-min) (point-max)))))))

(defun gnosis-export-deck (filename)
  "Export notes for deck in FILENAME.

FILENAME: The name of the file to save the exported deck.

This function prompts the user to provide a deck name and allows the
user to specify a filename for exporting notes belonging to that deck.
It then retrieves all the notes associated with the deck and exports
them.

The exported notes are formatted as an Emacs Lisp code block that can
be evaluated to recreate the deck with its associated notes. The
resulting code is saved to a file with the provided FILENAME and a
'.el' extension is added automatically.

Each note is exported using the `gnosis-export-note` function. The
generated code includes a call to `gnosis-define-deck` with the deck
name and all notes formatted as nested lists"
  (interactive (list (read-string "Filename: ")))
  (let ((notes (gnosis-get-notes-for-deck))
	(deck-name (read-string "Export deck as (name): ")))
    (with-temp-file (concat filename ".el")
      (insert "(gnosis-define-deck " "'" deck-name " '(")
      (cl-loop for note in notes
	       do (insert "(") (gnosis-export-note note) (insert ")" "\n")
	       finally (insert "))")))))

;; TODO: Add defcustom to have suspended as 0 or 1 depending on
;; gnosis-add-decks-suspended t or nil
(cl-defun gnosis-define-deck (deck notes &optional (suspended 0))
  "Define DECK consisting of NOTES, optionally add them as SUSPENDED.

The `gnosis-define-deck` function adds a new deck with the specified
name to `gnosis-db'. It also adds each note from the given list
of `notes` to the deck. The function takes three optional arguments:
`deck`, `notes`, and `suspended`.

- `deck`: The name of the deck to be added. It should be provided as a
          symbol.

- `notes`: A list containing the notes to be added to the deck. Each
           note should be represented as a property list with the
           following keys: `:type`, `:main`, `:options`, `:answer`

- extras include :`:extra-notes`, `:tags`, `:image`, and `:second-image`.

- `suspended`: An optional argument specifying whether the deck should
               be created in a suspended state. A non-zero value
               suspends the deck, while a value of 0 (default) creates
               the deck in an active state.

When calling `gnosis-define-deck`, the deck is added to the Gnosis
system by calling `gnosis-add-deck`. Each note is added to the deck
using `gnosis-add-note-fields`. The function iterates over the list of
`notes` and extracts the necessary fields from each note's property
list before adding them to the deck.

The purpose of this function is to create a full deck with its
associated notes in `gnosis-db', ready for further processing or
review."
  (gnosis-add-deck (symbol-name deck))
  (sit-for 0.1) ;;
  (cl-loop for note in notes
	   do (gnosis-add-note-fields (symbol-name deck)
				      (plist-get note :type)
				      (plist-get note :main)
				      (plist-get note :options)
				      (plist-get note :answer)
				      (plist-get note :extra-notes)
				      (plist-get note :tags)
				      suspended
				      (plist-get note :image)
				      (plist-get note :second-image))))


;;;###autoload
(defun gnosis-review ()
  "Start gnosis review session."
  (interactive)
  (let ((review-type (completing-read "Review: " '("Due notes"
						   "Due notes of deck"
						   "Due notes of specified tag(s)"
						   "All notes of tag(s)"))))
    (pcase review-type
      ("Due notes" (gnosis-review--session (gnosis-review-get-due-notes)))
      ("Due notes of deck" (gnosis-review--session (gnosis-get-deck-due-notes)))
      ("Due notes of specified tag(s)" (gnosis-review--session
					(gnosis-select-by-tag (gnosis-tag-prompt :match t :due t))))
      ("All notes of tag(s)" (gnosis-review--session (gnosis-select-by-tag (gnosis-tag-prompt :match t)))))))

;;; Database Schemas
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
				   (interval integer :not-null)] ;; Initial Interval
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
				   (images string)
				   (extra-image string)]
				  (:foreign-key [id] :references notes [id]
						:on-delete :cascade)))

(defun gnosis-db-init ()
  "Create gnosis essential directories & database."
  (unless (length= (emacsql gnosis-db [:select name :from sqlite-master :where (= type table)]) 6)
    ;; Enable foreign keys
    (emacsql gnosis-db "PRAGMA foreign_keys = ON")
    ;; Gnosis version
    (emacsql gnosis-db (format "PRAGMA user_version = %s" gnosis-db-version))
    ;; Create decks table
    (gnosis--create-table 'decks gnosis-db-schema-decks)
    ;; Create notes table
    (gnosis--create-table 'notes gnosis-db-schema-notes)
    ;; Create review table
    (gnosis--create-table 'review gnosis-db-schema-review)
    ;; Create review-log table
    (gnosis--create-table 'review-log gnosis-db-schema-review-log)
    ;; Create extras table
    (gnosis--create-table 'extras gnosis-db-schema-extras)))

(gnosis-db-init)

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

(define-derived-mode gnosis-mode special-mode "Gnosis"
  "Gnosis Mode."
  :interactive t
  (read-only-mode 0)
  (display-line-numbers-mode 0)
  :lighter " gnosis-mode")

(provide 'gnosis)
;;; gnosis.el ends here
