;;; gnosis.el --- Spaced Repetition System For Note Taking & Self Testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis
;; Version: 0.2.2

;; Package-Requires: ((emacs "27.2") (emacsql "20240124") (compat "29.1.4.2"))

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

;; Gnosis, is a spaced repetition system for note taking & self
;; testing, where notes are taken in a Question/Answer/Explanation
;; format & reviewed in spaced intervals, determined by the success or
;; failure to recall a given answer for question.

;; Gnosis implements a highly customizable algorithm, inspired by SM-2.
;; Gnosis algorithm does not use user's subjective rating of a note to
;; determine the next review interval, but instead uses the user's
;; success or failure in recalling the answer of a note.

;;; Code:

(require 'emacsql-sqlite)
(require 'cl-lib)

(require 'gnosis-algorithm)
(require 'gnosis-string-edit)

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

(defcustom gnosis-vc-auto-push nil
  "Run `vc-push' at the end of every review session."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-mcq-display-choices nil
  "When t, display choices for mcq notes during review.

Users that use a completion framework like ivy/helm/vertico may want
to set this to nil, as the choices will be displayed in the completion
framework's minibuffer."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-completing-read-function
  (cond ((or (bound-and-true-p ivy-mode)
	     (bound-and-true-p helm-mode)
	     (bound-and-true-p vertico-mode))
	 #'completing-read)
	(t #'ido-completing-read))
  "Function to use for `completing-read'."
  :type 'function
  :group 'gnosis)

(defcustom gnosis-image-height nil
  "Height of image to display during review.

When nil, the image will be displayed at its original size."
  :type 'integer
  :group 'gnosis)

(defcustom gnosis-image-width nil
  "Width of image to display during review.

When nil, the image will be displayed at its original size."
  :type 'integer
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

(defvar gnosis-dashboard-note-ids nil
  "Store note ids for dashboard.")

(defconst gnosis-db-version 2
  "Gnosis database version.")

(defvar gnosis-note-types '("MCQ" "Cloze" "Basic" "Double" "y-or-n")
  "Gnosis available note types.")

(defvar gnosis-previous-note-tags '()
  "Tags input from previously added note.")

(defvar gnosis-previous-note-hint nil
  "Hint input from previously added note.")

(defvar gnosis-cloze-guidance
  '("Cloze questions are formatted like this:\n
{c1:Cyproheptadine} is a(n) {c2:5-HT2} receptor antagonist used to treat {c2:serotonin syndrome}

- For each `cX`-tag there will be created a cloze type note, the above
  example creates 2 cloze type notes.)" . "")
  "Guidance for cloze note type.

car value is the prompt, cdr is the prewritten string.")

(defvar gnosis-mcq-guidance
  '("Write question options after the `--'.  Each `-' corresponds to an option\n-Example Option 1\n-{Correct Option}\nCorrect Option must be inside {}" . "Question\n--\n- Option\n- {Correct Option}")
  "Guidance for MCQ note type.

car value is the prompt, cdr is the prewritten string.")

(defcustom gnosis-mcq-separator "\n--\n"
  "Separator for stem field and options in mcq note type.

Seperate the question/stem from options."
  :type 'string
  :group 'gnosis)

(defcustom gnosis-mcq-option-separator "-"
  "Separator for options in mcq note type."
  :type 'string
  :group 'gnosis)

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

(defface gnosis-face-separator
  '((t :inherit warning))
  "Face for section separator."
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

(defun gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql gnosis-db `[:delete :from ,table :where ,value]))

;; (defun gnosis-delete-note (id)
;;   "Delete note with ID."
;;   (when (y-or-n-p "Delete note?")
;;     (emacsql-with-transaction gnosis-db (gnosis--delete 'notes `(= id ,id)))))

;; (defun gnosis-delete-deck (id)
;;   "Delete deck with ID."
;;   (interactive (list (gnosis--get-deck-id)))
;;   (let ((deck-name (gnosis--get-deck-name id)))
;;     (when (y-or-n-p (format "Delete deck `%s'? " deck-name))
;;       (gnosis--delete 'decks `(= id ,id))
;;       (message "Deleted deck `%s'" deck-name))))

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
  (let ((options (apply #'append (gnosis-select 'options 'notes `(= id ,id) t)))
	(option-num 1))
    (insert "\n\n" (propertize "Options:" 'face 'gnosis-face-directions))
    (cl-loop for option in options
	     do (insert (format "\n%s.  %s" option-num option))
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

ANSWER is the correct answer, either y or n.  Answer is either 121 or
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
     (propertize "\n\n-----\n" 'face 'gnosis-face-separator)
     (propertize hint 'face 'gnosis-face-hint))))

(cl-defun gnosis-display-cloze-reveal (&key (cloze-char gnosis-cloze-string) replace (success t) (face nil))
  "Replace CLOZE-CHAR with REPLACE.

If FACE nil, propertize replace using `gnosis-face-correct', or
`gnosis-face-false' when (not SUCCESS).  Else use FACE value."
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

(cl-defun gnosis-display-image (id &optional (image 'images))
  "Display image for note ID.

IMAGE is the image type to display, usually should be either `images'
or `extra-image'.  Instead of using `extra-image' post review, prefer
`gnosis-display-extra' which displays the `extra-image' as well.

Refer to `gnosis-db-schema-extras' for informations on images stored."
  (let* ((img (gnosis-get image 'extras `(= id ,id)))
	 (path-to-image (expand-file-name (or img "") (file-name-as-directory gnosis-images-dir)))
	 (image (create-image path-to-image 'png nil :width gnosis-image-width :height gnosis-image-height)))
    (cond ((and img (file-exists-p path-to-image))
	   (insert "\n\n")
	   (insert-image image))
	  ((or (not img) (string-empty-p img))
	   (insert "\n\n")))))

(defun gnosis-display-extra (id)
  "Display extra information & extra-image for note ID."
  (let ((extras (or (gnosis-get 'extra-notes 'extras `(= id ,id)) "")))
    (goto-char (point-max))
    (insert (propertize "\n\n-----\n" 'face 'gnosis-face-separator))
    (gnosis-display-image id 'extra-image)
    (fill-paragraph (insert "\n" (propertize extras 'face 'gnosis-face-extra)))))

;;;###autoload
(defun gnosis-read-string-from-buffer (prompt string)
  "Switch to a new buffer to edit STRING in a recursive edit.
The user finishes editing with \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-done], or aborts with \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-abort]).

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If nil, no prompt will be
inserted in the buffer.

Also see `gnosis-string-edit'."
  (gnosis-string-edit
   prompt
   string
   (lambda (edited)
     (setq string (substring-no-properties edited))
     (exit-recursive-edit))
   :abort-callback (lambda ()
                     (exit-recursive-edit)
                     (error "Aborted edit")))
  (recursive-edit)
  string)

(defun gnosis-display-next-review (id success)
  "Display next interval of note ID for SUCCESS."
  (let* ((interval (car (gnosis-review-algorithm id success)))
	 (next-review-msg (format "\n\n%s %s"
				  (propertize "Next review:" 'face 'gnosis-face-directions)
				  (propertize (format "%s" interval) 'face 'gnosis-face-next-review))))
    (if (search-backward "Next review" nil t)
	;; Delete previous result, and override with new this should
	;; occur only when used with `gnosis-review-override'
        (progn (delete-region (point) (progn (end-of-line) (point)))
	       (insert (propertize (replace-regexp-in-string "\n" "" next-review-msg)
				   'face (if success 'gnosis-face-correct 'gnosis-face-false))))
      ;; Default behaviour
      (goto-char (point-max))
      (insert next-review-msg))))

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
    (gnosis--insert-into 'decks `([nil ,name nil nil nil nil nil]))
    (message "Created deck '%s'" name)))

(defun gnosis--get-deck-name (&optional id)
  "Get deck name for ID, or prompt for deck name when ID is nil."
  (when (equal (gnosis-select 'name 'decks) nil)
    (error "No decks found.  Please create a deck first with `gnosis-add-deck'"))
  (if id
      (gnosis-get 'name 'decks `(= id ,id))
    (funcall gnosis-completing-read-function "Deck: " (gnosis-select 'name 'decks))))

(cl-defun gnosis--get-deck-id (&optional (deck (gnosis--get-deck-name)))
  "Return id for DECK name."
  (gnosis-get 'id 'decks `(= name ,deck)))


(cl-defun gnosis-suspend-note (id)
  "Suspend note with ID."
  (let ((suspended (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1)))
    (when (y-or-n-p (if suspended "Unsuspend note? " "Suspend note? "))
      (if suspended
	  (gnosis-update 'review-log '(= suspend 0) `(= id ,id))
	(gnosis-update 'review-log '(= suspend 1) `(= id ,id))))))

(cl-defun gnosis-suspend-deck (&optional (deck (gnosis--get-deck-id)))
  "Suspend all note(s) with DECK id.

When called with a prefix, unsuspends all notes in deck."
  (let* ((notes (gnosis-select 'id 'notes `(= deck-id ,deck) t))
	 (suspend (if current-prefix-arg 0 1))
	 (note-count 0)
	 (confirm (y-or-n-p (if (= suspend 0) "Unsuspend all notes for deck? " "Suspend all notes for deck? "))))
    (when confirm
      (cl-loop for note in notes
	       do (gnosis-update 'review-log `(= suspend ,suspend) `(= id ,note))
	       (setq note-count (1+ note-count))
	       finally (if (equal suspend 0)
			   (message "Unsuspended %s notes" note-count)
			 (message "Suspended %s notes" note-count))))))

(defun gnosis-suspend-tag ()
  "Suspend all note(s) with tag.

When called with a prefix, unsuspends all notes for tag."
  (let ((notes (gnosis-select-by-tag (gnosis-tag-prompt)))
	(suspend (if current-prefix-arg 0 1)))
    (cl-loop for note in notes
	     do (gnosis-update 'review-log `(= suspend ,suspend) `(= id ,note)))))

(defun gnosis-suspend ()
  "Suspend note(s) with specified values."
  (interactive)
  (let ((item (funcall gnosis-completing-read-function "Suspend by: " '("Deck" "Tag"))))
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
 (or at least it should).  Else there will be an error for foreign key
 constraint."
  (let* ((deck-id (gnosis--get-deck-id deck))
	 (initial-interval (gnosis-get-deck-initial-interval deck-id)))
    (emacsql-with-transaction gnosis-db
      ;; Refer to `gnosis-db-schema-SCHEMA' e.g `gnosis-db-schema-review-log'
      (gnosis--insert-into 'notes   `([nil ,type ,main ,options ,answer ,tags ,deck-id]))
      (gnosis--insert-into 'review  `([nil ,gnosis-algorithm-ef ,gnosis-algorithm-ff ,initial-interval]))
      (gnosis--insert-into 'review-log `([nil ,(gnosis-algorithm-date) ,(gnosis-algorithm-date) 0 0 0 0 ,suspend 0]))
      (gnosis--insert-into 'extras `([nil ,extra ,image ,second-image])))))

;; Adding note(s) consists firstly of a hidden 'gnosis-add-note--TYPE'
;; function that does the computation & error checking to generate a
;; note from given input.  Secondly, 'gnosis-add-note-TYPE' normal
;; function, which prompts for user input and passes it to the hidden
;; function.

(cl-defun gnosis-add-note--mcq (&key deck question choices correct-answer
				     extra (images nil) tags (suspend 0))
  "Create a NOTE with a list of multiple CHOICES.

MCQ type consists of a main `QUESTION' that is displayed to the user.
The user will be prompted to select the correct answer from a list of
`CHOICES'.  The `CORRECT-ANSWER' should be the index of the correct
choice in the `CHOICES' list.  Each note must correspond to one `DECK'.

`IMAGES' cons cell, where car is the image to display before and cdr
is the image to display post review

`EXTRA' are extra information displayed after an answer is given.
`TAGS' are used to organize questions.
`SUSPEND' is a binary value, where 1 is for suspend."
  (when (or (not (numberp correct-answer))
	    (equal correct-answer 0))
    (error "Correct answer value must be the index number of the correct answer"))
  (gnosis-add-note-fields deck "mcq" question choices correct-answer extra tags suspend (car images) (cdr images)))

(defun gnosis-add-note-mcq ()
  "Add note(s) of type `MCQ' interactively to selected deck.

Prompt user for input to create a note of type `MCQ'.

Stem field is seperated from options by `gnosis-mcq-separator', and
each option is seperated by `gnosis-mcq-option-separator'.  The correct
answer is surrounded by curly braces, e.g {Correct Answer}.

Refer to `gnosis-add-note--mcq' & `gnosis-prompt-mcq-input' for more."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `MCQ' to `%s' deck? " deck))
      (let* ((input (gnosis-prompt-mcq-input))
	     (stem (caar input))
	     (choices (cdr (car input)))
	     (correct-choice (cadr input)))
	(gnosis-add-note--mcq :deck deck
			      :question stem
			      :choices choices
			      :correct-answer correct-choice
			      :extra (gnosis-read-string-from-buffer "Extra" "")
			      :images (gnosis-select-images)
			      :tags (gnosis-prompt-tags--split gnosis-previous-note-tags))))))

(cl-defun gnosis-add-note--basic (&key deck question hint answer
				       extra (images nil) (tags) (suspend 0))
  "Add Basic type note.

DECK: Deck name for note.
QUESTION: Quesiton to display for note.
ANSWER: Answer for QUESTION, which user will be prompted to type
HINT: Hint to display during review, before user-input.
EXTRA: Extra information to display after user-input/giving an answer.
IMAGES: Cons cell, where car is the image to display before user-input
	and cdr is the image to display post review.
TAGS: Tags used to organize notes
SUSPEND: Binary value of 0 & 1, when 1 note will be ignored."
  (gnosis-add-note-fields deck "basic" question hint answer extra tags suspend (car images) (cdr images)))

(defun gnosis-add-note-basic ()
  "Add note(s) of type `Basic' interactively to selected deck.

Basic note type is a simple question/answer note, where user first
sees a \"main\" part, which is usually a question, and he is prompted
to input the answer.

Refer to `gnosis-add-note--basic' for more."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `basic' to `%s' deck? " deck))
      (gnosis-add-note--basic :deck deck
			      :question (gnosis-read-string-from-buffer "Question: " "")
			      :answer (read-string "Answer: ")
			      :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			      :extra (gnosis-read-string-from-buffer "Extra: " "")
			      :images (gnosis-select-images)
			      :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))))

(cl-defun gnosis-add-note--double (&key deck question hint answer extra (images nil) tags (suspend 0))
  "Add Double type note.

Essentially, a \"note\" that generates 2 basic notes.  The second one
reverses question/answer.

DECK: Deck name for note.
QUESTION: Quesiton to display for note.
ANSWER: Answer for QUESTION, which user will be prompted to type
HINT: Hint to display during review, before user-input.
EXTRA: Extra information to display after user-input/giving an answer.
IMAGES: Cons cell, where car is the image to display before user-input
	and cdr is the image to display post review.
TAGS: Tags used to organize notes
SUSPEND: Binary value of 0 & 1, when 1 note will be ignored."
  (gnosis-add-note-fields deck "basic" question hint answer extra tags suspend (car images) (cdr images))
  (gnosis-add-note-fields deck "basic" answer hint question extra tags suspend (car images) (cdr images)))

(defun gnosis-add-note-double ()
  "Add note(s) of type double interactively to selected deck.

Essentially, a \"note\" that generates 2 basic notes.  The second one
reverses question/answer.

Refer to `gnosis-add-note--double' for more."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `double' to `%s' deck? " deck))
      (gnosis-add-note--double :deck deck
			       :question (read-string "Question: ")
			       :answer (read-string "Answer: ")
			       :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			       :extra (gnosis-read-string-from-buffer "Extra" "")
			       :images (gnosis-select-images)
			       :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))))

(cl-defun gnosis-add-note--y-or-n (&key deck question hint answer extra (images nil) tags (suspend 0))
  "Add y-or-n type note.

DECK: Deck name for note.

QUESTION: Quesiton to display for note.

ANSWER: Answer for QUESTION, either `121' (char value for yes) or `110'
        (char value for no).

HINT: Hint to display during review, before user-input.

EXTRA: Extra information to display after user-input/giving an answer.

IMAGES: Cons cell, where car is the image to display before user-input
	and cdr is the image to display post review.

TAGS: Tags used to organize notes

SUSSPEND: Binary value of 0 & 1, when 1 note will be ignored."
  (gnosis-add-note-fields deck "y-or-n" question hint answer extra tags suspend (car images) (cdr images)))

(defun gnosis-add-note-y-or-n ()
  "Add note(s) of type `y-or-n'.

Refer to `gnosis-add-note--y-or-n' for more information about keyword values."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `y-or-n' to `%s' deck? " deck))
      (gnosis-add-note--y-or-n :deck deck
			       :question (gnosis-read-string-from-buffer "Question: " "")
                               :answer (read-char-choice "Answer: [y] or [n]? " '(?y ?n))
			       :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			       :extra (gnosis-read-string-from-buffer "Extra" "")
			       :images (gnosis-select-images)
			       :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))))


(cl-defun gnosis-add-note--cloze (&key deck note hint tags (suspend 0) extra (images nil))
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

Generates 3 cloze note types.  Where the \"main\" part of the note is the full
note, with the cloze(s) extracted & used as the \"answer\".

One cloze note may have multiple clozes
Example:
      {c1:Streptococcus agalactiae (GBS)} and {c1:Listeria
      monocytogenes} are CAMP test positive
   
HINT: Hint to display during review, before user-input.

   NOTE: In gnosis-db, hint is referred to as `options', same column
   options used in mcq.

IMAGES: Cons cell, where car is the image to display before user-input
	and cdr is the image to display post review.

TAGS: Tags used to organize notes

SUSPEND: When t, note will be ignored.

EXTRA: Extra information displayed after user-input."
  (let ((notags-note (gnosis-cloze-remove-tags note))
	(clozes (gnosis-cloze-extract-answers note)))
    (cl-loop for cloze in clozes
	     do (gnosis-add-note-fields deck "cloze" notags-note hint cloze extra tags suspend
					(car images) (cdr images)))))

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

Generates 3 cloze note types.  Where the \"main\" part of the note is
the full note, with the cloze(s) extracted & used as the \"answer\".

See `gnosis-add-note--cloze' for more reference."
  (let ((deck (gnosis--get-deck-name)))
    (while (y-or-n-p (format "Add note of type `cloze' to `%s' deck? " deck))
      (gnosis-add-note--cloze :deck deck
			      :note (gnosis-read-string-from-buffer (or (car gnosis-cloze-guidance) "")
								    (or (cdr gnosis-cloze-guidance) ""))
			      :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			      :extra (gnosis-read-string-from-buffer "Extra" "")
			      :images (gnosis-select-images)
			      :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))))

;;;###autoload
(defun gnosis-add-note (type)
  "Create note(s) as TYPE interactively."
  (interactive (list (funcall gnosis-completing-read-function "Type: " gnosis-note-types nil t)))
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
    (funcall gnosis-completing-read-function "Answer: " choices)))

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
           do (if (string-match (concat "\\b" word "\\b") string)
                  (setq string (replace-match new t t string))
                ;; This error will be produced when user has edited a
                ;; note to an invalid cloze.
                (error "`%s' is an invalid cloze for question: `%s'"
                       word string)))
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
  (let ((string-compare-func (if (or (> (length str1) gnosis-string-difference)
				     (> (length str2) gnosis-string-difference))
				 #'(lambda (str1 str2) (<= (string-distance str1 str2) gnosis-string-difference))
			       #'string=)))
    (funcall string-compare-func
	     (downcase (replace-regexp-in-string "\\s-" "" str1))
	     (downcase (replace-regexp-in-string "\\s-" "" str2)))))


(defun gnosis-directory-files (&optional dir regex)
  "Return a list of file paths, relative to DIR directory.

DIR is the base directory path from which to start the recursive search.
REGEX is the regular expression pattern to match the file names against.

This function traverses the subdirectories of DIR recursively,
collecting file paths that match the regular expression.  The file
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

(defun gnosis-select-images (&optional prompt)
  "Return PATH for file in `gnosis-images-dir'.

Optionally, add cusotm PROMPT."
  (if (y-or-n-p "Include images?")
      (let* ((prompt (or prompt "Select image: "))
	     (image (if (y-or-n-p "Add review image?")
			(funcall gnosis-completing-read-function prompt
				 (cons nil (gnosis-directory-files gnosis-images-dir)))
		      nil))
	     (extra-image (if (y-or-n-p "Add post review image?")
			      (funcall gnosis-completing-read-function prompt
				       (cons nil (gnosis-directory-files gnosis-images-dir))))))
	(cons image extra-image))
    nil))

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (cl-loop for tags in (gnosis-select 'tags 'notes '1=1 t)
           nconc tags into all-tags
           finally return (delete-dups all-tags)))

(defun gnosis-select-by-tag (input-tags)
  "Return note ID's for every note with INPUT-TAGS."
  (cl-assert (listp input-tags) t "Input tags must be a list")
  (cl-loop for (id tags) in (gnosis-select '[id tags] 'notes)
           when (and (cl-every (lambda (tag) (member tag tags)) input-tags)
		     (not (gnosis-suspended-p id)))
           collect id))

(defun gnosis-suspended-p (id)
  "Return t if note with ID is suspended."
  (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1))

(defun gnosis-get-deck-notes (&optional deck-id due)
  "Return notes for deck, with value of DECK-ID.

If DUE is t, return only due notes."
  (let ((notes (gnosis-select 'id 'notes `(= deck-id ,(or deck-id (gnosis--get-deck-id))) t)))
    (if (or due nil)
	(cl-loop for note in notes
		 when (and (not (gnosis-suspended-p note))
			   (gnosis-review-is-due-p note))
		 collect note)
      notes)))

(defun gnosis-past-or-present-p (date)
  "Compare the input DATE with the current date.
Return t if DATE is today or in the past, nil if it's in the future.
DATE is a list of the form (year month day)."
  (let* ((now (gnosis-algorithm-date))
         (time-now (encode-time 0 0 0 (nth 2 now) (nth 1 now) (nth 0 now)))
         (time-date (encode-time 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
    (not (time-less-p time-now time-date))))

(cl-defun gnosis-tag-prompt (&key (prompt "Selected tags:") (due nil))
  "PROMPT user to select tags, until they enter `q'.

Prompt user to select tags, generated from `gnosis-get-tags--unique'.
PROMPT: Prompt string value
MATCH: Require match, t or nil value
DUE: if t, return tags for due notes from `gnosis-due-tags'."
  (let ((tags '()))
    (cond ((and due (null (gnosis-review-get-due-notes)))
	   (error "No due notes"))
	  (t (cl-loop for tag = (completing-read
				 (concat prompt (format " (%s) (q for quit): " (mapconcat #'identity tags " ")))
				 (cons "q" (if due (gnosis-review-get-due-tags)
					     (gnosis-get-tags--unique)))
				 nil t)
		      until (string= tag "q")
		      unless (member tag tags)
		      do (push tag tags))
	     tags))))

(defun gnosis-hint-prompt (previous-hint &optional prompt)
  "Prompt user for hint.

PROMPT: Prompt string value
PREVIOUS-HINT: Previous hint value, if any.  If nil, use PROMPT as
default value."
  (let* ((prompt (or prompt "Hint: "))
	 (hint (read-string prompt previous-hint)))
    (setf gnosis-previous-note-hint hint)
    hint))

(defun gnosis-prompt-mcq-input ()
  "Prompt for MCQ content.

Return a list of the form ((QUESTION CHOICES) CORRECT-CHOICE-INDEX)."
  (let ((user-input (gnosis-read-string-from-buffer (or (car gnosis-mcq-guidance) "")
						    (or (cdr gnosis-mcq-guidance) ""))))
    (unless (string-match-p gnosis-mcq-separator user-input)
      (error "Separator %s not found" gnosis-mcq-separator))
    (let* ((input-seperated (split-string user-input gnosis-mcq-separator t "[\s\n]"))
	   (stem (car input-seperated))
	   (input (split-string
		   (mapconcat 'identity (cdr input-seperated) "\n")
		   gnosis-mcq-option-separator t "[\s\n]"))
	   (correct-choice-index
	    (or (cl-position-if (lambda (string) (string-match "{.*}" string)) input)
		(error "Correct choice not found.  Use {} to indicate the correct option")))
	   (choices (mapcar (lambda (string) (replace-regexp-in-string "{\\|}" "" string)) input)))
      (list (cons stem choices) (+ correct-choice-index 1)))))

(defun gnosis-prompt-tags--split (&optional previous-note-tags)
  "Prompt user for tags, split string by space.

Return a list of tags, split by space.  If PREVIOUS-NOTE-TAGS is
provided, use it as the default value."
  (let* ((previous-note-tags (or nil previous-note-tags))
	 (tags (split-string (read-from-minibuffer "Tags: " (mapconcat #'identity previous-note-tags " ")) " ")))
    (setf gnosis-previous-note-tags tags)
    (if (equal tags '("")) '("untagged") tags)))

;; Review
;;;;;;;;;;
(defun gnosis-review-is-due-p (note-id)
  "Check if note with value of NOTE-ID for id is due for review.

Check if it's suspended, and if it's due today."
  (and (not (gnosis-suspended-p note-id))
       (gnosis-review-is-due-today-p note-id)))

(defun gnosis-review-is-due-today-p (id)
  "Return t if note with ID is due today.

This function ignores if note is suspended.  Refer to
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
                  (gnosis-get 'tags 'notes `(= id ,note-id)))
	        due-notes)
     :test #'equal)))

(defun gnosis-review-algorithm (id success)
  "Return next review date & ef for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure.

Returns a list of the form ((yyyy mm dd) (ef-increase ef-decrease ef-total))."
  (let ((ff gnosis-algorithm-ff)
	(ef (gnosis-get 'ef 'review `(= id ,id)))
	(t-success (gnosis-get 't-success 'review-log `(= id ,id))) ;; total successful reviews
	(c-success (gnosis-get 'c-success 'review-log `(= id ,id))) ;; consecutive successful reviews
	(c-fails (gnosis-get 'c-fails 'review-log `(= id ,id))) ;; consecutive failed reviews
	;; (t-fails (gnosis-get 't-fails 'review-log `(= id ,id))) ;; total failed reviews
	;; (review-num (gnosis-get 'n 'review-log `(= id ,id))) ;; total reviews
	(last-interval (max (gnosis-review--get-offset id) 1))) ;; last interval
    (list (gnosis-algorithm-next-interval :last-interval last-interval
					  :ef ef
					  :success success
					  :successful-reviews t-success
					  :failure-factor ff
					  :initial-interval (gnosis-get-note-initial-interval id))
	  (gnosis-algorithm-next-ef :ef ef
				    :success success
				    :increase (gnosis-get-ef-increase id)
				    :decrease (gnosis-get-ef-decrease id)
				    :threshold (gnosis-get-ef-threshold id)
				    :c-successes c-success
				    :c-failures c-fails))))

(defun gnosis-review--get-offset (id)
  "Return offset for note with value of id ID."
  (let ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev)))

(defun gnosis-review--update (id success)
  "Update review-log for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure."
  (let ((ef (cadr (gnosis-review-algorithm id success)))
	(next-rev (car (gnosis-review-algorithm id success))))
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
  (gnosis-display-question id)
  (gnosis-display-image id)
  (when gnosis-mcq-display-choices
    (gnosis-display-mcq-options id))
  (let* ((choices (gnosis-get 'options 'notes `(= id ,id)))
	 (answer (nth (- (gnosis-get 'answer 'notes `(= id ,id)) 1) choices))
	 (user-choice (gnosis-mcq-answer id))
	 (success (string= answer user-choice)))
    (gnosis-display-correct-answer-mcq answer user-choice)
    (gnosis-display-extra id)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-basic (id)
  "Review basic type note for ID."
  (gnosis-display-question id)
  (gnosis-display-image id)
  (gnosis-display-hint (gnosis-get 'options 'notes `(= id ,id)))
  (let* ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	 (user-input (read-string "Answer: "))
	 (success (gnosis-compare-strings answer user-input)))
    (gnosis-display-basic-answer answer success user-input)
    (gnosis-display-extra id)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-y-or-n (id)
  "Review y-or-n type note for ID."
  (gnosis-display-question id)
  (gnosis-display-image id)
  (gnosis-display-hint (gnosis-get 'options 'notes `(= id ,id)))
  (let* ((answer (gnosis-get 'answer 'notes `(= id ,id)))
	 (user-input (read-char-choice "[y]es or [n]o: " '(?y ?n)))
	 (success (equal answer user-input)))
    (gnosis-display-y-or-n-answer :answer answer :success success)
    (gnosis-display-extra id)
    (gnosis-display-next-review id success)
    success))

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
	 (num 1) ;; Number of clozes revealed
	 (hint (gnosis-get 'options 'notes `(= id ,id)))
	 (success nil))
    (gnosis-display-cloze-sentence main clozes)
    (gnosis-display-image id)
    (gnosis-display-hint hint)
    (cl-loop for cloze in clozes
	     do (let ((input (gnosis-review-cloze--input cloze)))
		  (if (equal (car input) t)
		      ;; Reveal only one cloze
		      (progn (gnosis-display-cloze-reveal :replace cloze)
			     (setf num (1+ num)))
		    ;; Reveal cloze for wrong input, with `gnosis-face-false'
		    (gnosis-display-cloze-reveal :replace cloze :success nil)
		    (gnosis-display-cloze-user-answer (cdr input))
		    ;; Reveal all clozes left with `gnosis-face-cloze-unanswered' face
		    (gnosis-review-cloze-reveal-unaswered (nthcdr num clozes))
		    (setf success nil)
		    (cl-return)))
	     ;; Update note after all clozes are revealed successfully
	     finally (setf success t))
    (gnosis-display-extra id)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-note (id)
  "Start review for note with value of id ID, if note is unsuspended."
  (when (gnosis-suspended-p id)
    (message "Suspended note with id: %s" id)
    (sit-for 0.3)) ;; this should only occur in testing/dev cases
  (let* ((type (gnosis-get 'type 'notes `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s" (downcase type)))))
    (if (fboundp func-name)
        (progn
	  (pop-to-buffer-same-window (get-buffer-create "*gnosis*"))
          (gnosis-mode)
          (funcall func-name id))
      (error "Malformed note type: '%s'" type))))


;;;###autoload
(cl-defun gnosis-vc-push (&optional (dir gnosis-dir))
  "Run `vc-push' in DIR."
  (interactive)
  (let ((default-directory dir))
    (vc-push)))

;;;###autoload
(cl-defun gnosis-vc-pull (&optional (dir gnosis-dir))
  "Run `vc-pull' in DIR."
  (interactive)
  (let ((default-directory dir))
    (vc-pull)
    ;; Reopen gnosis-db after pull
    (setf gnosis-db (emacsql-sqlite-open (expand-file-name "gnosis.db" dir)))))

(defun gnosis-review-commit (note-num)
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one.  It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number of notes.

NOTE-NUM: The number of notes reviewed in the session."
  (let ((git (executable-find "git"))
	(default-directory gnosis-dir))
    (unless git
      (error "Git not found, please install git"))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-create-repo 'Git))
    ;; TODO: Redo this using vc
    (unless gnosis-testing
      (shell-command (format "%s %s %s" git "add" (shell-quote-argument "gnosis.db")))
      (shell-command (format "%s %s %s" git "commit -m"
			     (shell-quote-argument (format "Total notes for session: %d" note-num)))))
    (when (and gnosis-vc-auto-push
	       (not gnosis-testing))
      (gnosis-vc-push))
    (message "Review session finished.  %d notes reviewed." note-num)))

(defun gnosis-review-override (id success)
  "Override review result of note ID.

Reverse the result of review SUCCESS."
  (let ((success-new (if success nil t)))
    (gnosis-display-next-review id success-new)
    (if (y-or-n-p (format "Override review result as %s?" (if success-new "`SUCCESS'" "`FAILURE'")))
	(gnosis-review--update id success-new)
      (gnosis-review-override id success-new))))

(defun gnosis-review--session (notes)
  "Start review session for NOTES.

NOTES: List of note ids"
  (let ((note-count 0))
    (if (null notes)
	(message "No notes for review.")
      (when (y-or-n-p (format "You have %s total notes for review, start session?" (length notes)))
	(cl-loop for note in notes
		 do (let ((success (gnosis-review-note note)))
		      (setf note-count (1+ note-count))
		      (pcase (car (read-multiple-choice
				   "Note actions"
				   '((?n "next")
				     (?o "override")
				     (?s "suspend")
				     (?e "edit")
				     (?q "quit"))))
			(?n (gnosis-review--update note success))
			(?o (gnosis-review-override note success))
			(?s (gnosis-suspend-note note))
			(?e (gnosis-review--update note success)
			    (gnosis-edit-note note t)
			    (recursive-edit))
			(?q (gnosis-review-commit note-count)
			    (cl-return))))
		 finally (gnosis-review-commit note-count))))))


;; Editing notes
(defun gnosis-edit-read-only-values (&rest values)
  "Make the provided VALUES read-only in the whole buffer."
  (goto-char (point-min))
  (dolist (value values)
    (while (search-forward value nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'read-only t)))
  (goto-char (point-min)))

(cl-defun gnosis-edit-note (id &optional (recursive-edit nil) (dashboard "notes"))
  "Edit the contents of a note with the given ID.

This function creates an Emacs Lisp buffer named *gnosis-edit* on the
same window and populates it with the values of the note identified by
the specified ID using `gnosis-export-note'.  The note values are
inserted as keywords for the `gnosis-edit-update-note' function.

To make changes, edit the values in the buffer, and then evaluate the
`gnosis-edit-update-note' expression to save the changes.

RECURSIVE-EDIT: If t, exit `recursive-edit' after finishing editing.
It should only be t when starting a recursive edit, when editing a
note during a review session.

DASHBOARD: Dashboard to return after editing.  Default value is
Notes.

The buffer automatically indents the expressions for readability.
After finishing editing, evaluate the entire expression to apply the
changes."
  (pop-to-buffer-same-window (get-buffer-create "*gnosis-edit*"))
  (gnosis-edit-mode)
  (erase-buffer)
  (insert ";;\n;; You are editing a gnosis note.\n\n")
  (insert "(gnosis-edit-update-note ")
  (gnosis-export-note id)
  (insert ")")
  (insert "\n\n;; After finishing editing, save changes with `<C-c> <C-c>'\n;; Avoid exiting without saving.")
  (indent-region (point-min) (point-max))
  ;; Insert id & fields as read-only values
  (gnosis-edit-read-only-values (format ":id %s" id) ":main" ":options" ":answer"
				":tags" ":extra-notes" ":image" ":second-image"
				":ef" ":ff" ":suspend")
  (local-unset-key (kbd "C-c C-c"))
  (local-set-key (kbd "C-c C-c") (lambda () (interactive) (if recursive-edit
							 (gnosis-edit-save-exit 'exit-recursive-edit)
						       (gnosis-edit-save-exit 'gnosis-dashboard dashboard
									      gnosis-dashboard-note-ids)))))

(defun gnosis-edit-deck--export (id)
  "Export deck with ID.

WARNING: This export is only for editing said deck!

Insert deck values:
 `ef-increase', `ef-decrease', `ef-threshold', `failure-factor'"
  (let ((name (gnosis-get 'name 'decks `(= id ,id)))
	(ef-increase (gnosis-get 'ef-increase 'decks `(= id ,id)))
	(ef-decrease (gnosis-get 'ef-decrease 'decks `(= id ,id)))
	(ef-threshold (gnosis-get 'ef-threshold 'decks `(= id ,id)))
	(failure-factor (gnosis-get 'failure-factor 'decks `(= id ,id)))
	(initial-interval (gnosis-get 'initial-interval 'decks `(= id ,id))))
    (insert
     (format "\n:id %s\n:name \"%s\"\n:ef-increase %s\n:ef-decrease %s\n:ef-threshold %s\n:failure-factor %s\n :initial-interval '%s"
	     id name ef-increase ef-decrease ef-threshold failure-factor initial-interval))))

(defun gnosis-assert-int-or-nil (value description)
  "Assert that VALUE is an integer or nil.

DESCRIPTION is a string that describes the value."
  (unless (or (null value) (integerp value))
    (error "Invalid value: %s, %s" value description)))

(defun gnosis-assert-float-or-nil (value description &optional less-than-1)
  "Assert that VALUE is a float or nil.

DESCRIPTION is a string that describes the value.
LESS-THAN-1: If t, assert that VALUE is a float less than 1."
  (if less-than-1
      (unless (or (null value) (and (floatp value) (< value 1)))
	(error "Invalid value: %s, %s" value description))
    (unless (or (null value) (floatp value))
      (error "Invalid value: %s, %s" value description))))

(defun gnosis-assert-number-or-nil (value description)
  "Assert that VALUE is a number or nil.

DESCRIPTION is a string that describes the value."
  (unless (or (null value) (numberp value))
    (error "Invalid value: %s, %s" value description)))

(cl-defun gnosis-edit-update-deck (&key id name ef-increase ef-decrease ef-threshold failure-factor initial-interval)
  "Update deck with id value of ID.

NAME: Name of deck
EF-INCREASE: Easiness factor increase value
EF-DECREASE: Easiness factor decrease value
EF-THRESHOLD: Easiness factor threshold value
FAILURE-FACTOR: Failure factor value
INITIAL-INTERVAL: Initial interval for notes of deck"
  (gnosis-assert-float-or-nil failure-factor "failure-factor must be a float less than 1" t)
  (gnosis-assert-int-or-nil ef-threshold "ef-threshold must be an integer")
  (gnosis-assert-number-or-nil ef-increase "ef-increase must be a number")
  (cl-assert (or (and (listp initial-interval)
		      (and (cl-every #'integerp initial-interval)
			   (length= initial-interval 2)))
		 (null initial-interval))
	     nil "Initial-interval must be a list of 2 integers")
  (cl-loop for (field . value) in
	   `((ef-increase . ,ef-increase)
	     (ef-decrease . ,ef-decrease)
	     (ef-threshold . ,ef-threshold)
	     (failure-factor . ,failure-factor)
	     (initial-interval . ',initial-interval)
	     (name . ,name))
	   when value
	   do (gnosis-update 'decks `(= ,field ,value) `(= id ,id))))

(defun gnosis-edit-deck (&optional id)
  "Edit the contents of a deck with the given ID."
  (interactive "P")
  (let ((id (or id (gnosis--get-deck-id))))
    (pop-to-buffer-same-window (get-buffer-create "*gnosis-edit*"))
    (gnosis-edit-mode)
    (erase-buffer)
    (insert ";;\n;; You are editing a gnosis deck.\n\n")
    (insert "(gnosis-edit-update-deck ")
    (gnosis-edit-deck--export id)
    (insert ")")
    (insert "\n\n;; After finishing editing, save changes with `<C-c> <C-c>'\n;; Avoid exiting without saving.")
    (indent-region (point-min) (point-max))
    (gnosis-edit-read-only-values (format ":id %s" id) ":name" ":ef-increase"
				  ":ef-decrease" ":ef-threshold" ":failure-factor")
    (local-unset-key (kbd "C-c C-c"))
    (local-set-key (kbd "C-c C-c") (lambda () (interactive) (gnosis-edit-save-exit 'gnosis-dashboard "decks")))))

(cl-defun gnosis-edit-save-exit (&optional exit-func &rest args)
  "Save edits and exit using EXIT-FUNC, with ARGS."
  (interactive)
  (eval-buffer)
  (quit-window t)
  (when exit-func
    (apply exit-func args)))

(defvar-keymap gnosis-edit-mode-map
  :doc "gnosis-edit keymap"
  "C-c C-c" #'gnosis-edit-save-exit)

(define-derived-mode gnosis-edit-mode emacs-lisp-mode "Gnosis EDIT"
  "Gnosis Edit Mode."
  :interactive nil
  :lighter " Gnosis Edit"
  :keymap gnosis-edit-mode-map)

(cl-defun gnosis-edit-update-note (&key id main options answer tags (extra-notes nil) (image nil) (second-image nil)
					ef ff suspend)
  "Update note with id value of ID.

ID: Note id
MAIN: Main part of note, the stem part of MCQ, question for basic, etc.
OPTIONS: Options for mcq type notes/Hint for basic & cloze type notes
ANSWER: Answer for MAIN
TAGS: Tags for note, used to organize & differentiate between notes
EXTRA-NOTES: Notes to display after user-input
IMAGE: Image to display before user-input
SECOND-IMAGE: Image to display after user-input
EF: Easiness factor value
FF: Failure factor value
SUSPEND: Suspend note, 0 for unsuspend, 1 for suspend"
  (cl-assert (stringp main) nil "Main must be a string")
  (cl-assert (or (stringp image) (null image)) nil
	     "Image must be a string, path to image file from `gnosis-images-dir', or nil")
  (cl-assert (or (stringp second-image) (null second-image)) nil
	     "Second-image must be a string, path to image file from `gnosis-images-dir', or nil")
  (cl-assert (or (stringp extra-notes) (null extra-notes)) nil
	     "Extra-notes must be a string, or nil")
  (cl-assert (listp tags) nil "Tags must be a list of strings")
  (cl-assert (and (listp ef) (length= ef 3)) nil "ef must be a list of 3 floats")
  (cl-assert (or (stringp options) (listp options)) nil "Options must be a string, or a list for MCQ")
  (cl-assert (or (= suspend 0) (= suspend 1)) nil "Suspend must be either 0 or 1")
  ;; Construct the update clause for the emacsql update statement.
  (cl-loop for (field . value) in
           `((main . ,main)
             (options . ,options)
             (answer . ,answer)
             (tags . ,tags)
             (extra-notes . ,extra-notes)
             (images . ,image)
             (extra-image . ,second-image)
	     (ef . ',ef)
	     (ff . ,ff)
	     (suspend . ,suspend))
           when value
           do (cond ((memq field '(extra-notes images extra-image))
		     (gnosis-update 'extras `(= ,field ,value) `(= id ,id)))
		    ((memq field '(ef ff))
		     (gnosis-update 'review `(= ,field ,value) `(= id ,id)))
		    ((eq field 'suspend)
		     (gnosis-update 'review-log `(= ,field ,value) `(= id ,id)))
		    ((listp value)
		     (gnosis-update 'notes `(= ,field ',value) `(= id ,id)))
		    (t (gnosis-update 'notes `(= ,field ,value) `(= id ,id))))))

(defun gnosis-get-ef-increase (id)
  "Return ef-increase for note with value of id ID."
  (let ((ef-increase (gnosis-get 'ef-increase 'decks `(= id ,(gnosis-get 'deck-id 'notes `(= id ,id))))))
    (or ef-increase gnosis-algorithm-ef-increase)))

(defun gnosis-get-ef-decrease (id)
  "Return ef-decrease for note with value of id ID."
  (let ((ef-decrease (gnosis-get 'ef-decrease 'decks `(= id ,(gnosis-get 'deck-id 'notes `(= id ,id))))))
    (or ef-decrease gnosis-algorithm-ef-decrease)))

(defun gnosis-get-ef-threshold (id)
  "Return ef-threshold for note with value of id ID."
  (let ((ef-threshold (gnosis-get 'ef-threshold 'decks `(= id ,(gnosis-get 'deck-id 'notes `(= id ,id))))))
    (or ef-threshold gnosis-algorithm-ef-threshold)))

(defun gnosis-get-deck-initial-interval (id)
  "Return initial-interval for notes of deck ID."
  (let ((initial-interval (gnosis-get 'initial-interval 'decks `(= id ,id))))
    (or initial-interval gnosis-algorithm-interval)))

(defun gnosis-get-note-initial-interval (id)
  "Return initial-interval for note with ID."
  (let ((deck-id (gnosis-get 'deck-id 'notes `(= id ,id))))
    (gnosis-get-deck-initial-interval deck-id)))

(cl-defun gnosis-export-note (id &optional (export-for-deck nil))
  "Export fields for note with value of id ID.

ID: Identifier of the note to export.
EXPORT-FOR-DECK: If t, add type field and remove review fields

This function retrieves the fields of a note with the given ID and
inserts them into the current buffer.  Each field is represented as a
property list entry.  The following fields are exported: type, main,
options, answer, tags, extra-notes, image, and second-image.

The exported fields are formatted as key-value pairs with a colon,
e.g., :field value.  The fields are inserted sequentially into the
buffer.  For certain field values, like lists or nil, special
formatting is applied.

If the value is a list, the elements are formatted as strings and
enclosed in double quotes.

If the value is nil, the field is exported as :field nil.

All other values are treated as strings and exported with double
quotes.

The final exported note is indented using the `indent-region' function
to improve readability."
  (let ((values (append (gnosis-select '[id main options answer tags] 'notes `(= id ,id) t)
			(gnosis-select '[extra-notes images extra-image] 'extras `(= id ,id) t)
			(gnosis-select '[ef ff] 'review `(= id ,id) t)
			(gnosis-select 'suspend 'review-log `(= id ,id) t)))
	(fields '(:id :main :options :answer :tags :extra-notes :image :second-image :ef :ff :suspend)))
    (when export-for-deck
      (setf values (append (gnosis-select 'type 'notes `(= id ,id) t)
			   (butlast (cdr values) 3)))
      (setf fields (append '(:type) (butlast (cdr fields) 3))))
    (cl-loop for value in values
             for field in fields
             do (insert
		 (cond ((listp value)
			(format "\n%s '%s" (symbol-name field) (prin1-to-string value)))
		       (t (format "\n%s %s" (symbol-name field) (prin1-to-string value))))))))

;;;###autoload
(defun gnosis-review ()
  "Start gnosis review session."
  (interactive)
  (let ((review-type (funcall gnosis-completing-read-function "Review: " '("Due notes"
									   "Due notes of deck"
									   "Due notes of specified tag(s)"
									   "All notes of tag(s)"))))
    (pcase review-type
      ("Due notes" (gnosis-review--session (gnosis-collect-note-ids :due t)))
      ("Due notes of deck" (gnosis-review--session (gnosis-collect-note-ids :due t :deck t)))
      ("Due notes of specified tag(s)" (gnosis-review--session (gnosis-collect-note-ids :due t :tags t)))
      ("All notes of tag(s)" (gnosis-review--session (gnosis-collect-note-ids :tags t))))))

;;; Database Schemas
(defvar gnosis-db-schema-decks '([(id integer :primary-key :autoincrement)
				  (name text :not-null)
				  (failure-factor float)
				  (ef-increase float)
				  (ef-decrease float)
				  (ef-threshold integer)
				  (initial-interval listp)]))

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
				   ;; Despite the name 'images', this
				   ;; is a single string value.  At
				   ;; first it was designed to hold a
				   ;; list of strings for image paths,
				   ;; but it was changed to just a
				   ;; string to hold a single image
				   ;; path.
				   (images string)
				   ;; Extra image path to show after review
				   (extra-image string)]
				  ;; Note that the value of the images
				  ;; above is PATH inside
				  ;; `gnosis-images-dir'
				  (:foreign-key [id] :references notes [id]
						:on-delete :cascade)))

(defun gnosis-search-note (&optional query)
  "Search for note QUERY.

Return note ids for notes that match QUERY."
  (cl-assert (or (stringp query) (eq query nil)))
  (let* ((query (or query (read-string "Search for note: ")))
         (words (split-string query))
         (clause `(and ,@(mapcar (lambda (word)
                                   `(like main ,(format "%%%s%%" word)))
                                 words))))
    (gnosis-select 'id 'notes clause t)))

;; Dashboard

(defun gnosis-dashboard-output-note (id)
  "Output contents for note with ID, formatted for gnosis dashboard."
  (cl-loop for item in (append (gnosis-select '[main options answer tags type] 'notes `(= id ,id) t)
			       (gnosis-select 'suspend 'review-log `(= id ,id) t))
           if (listp item)
           collect (mapconcat #'identity item ",")
           else
           collect (replace-regexp-in-string "\n" " " (format "%s" item))))

(cl-defun gnosis-collect-note-ids (&key (tags nil) (due nil) (deck nil) (query nil))
  "Return list of note ids based on TAGS, DUE, DECKS, QUERY.

TAGS: boolean value, t to specify tags.
DUE: boolean value, t to specify due notes.
DECK: boolean value, t to specify notes from deck.
QUERY: String value,"
  (cl-assert (and (booleanp due) (booleanp tags) (booleanp deck) (or (stringp query) (null query)))
	     nil "provide boolean value")
  (cond ((and (null tags) (null due) (null deck) (null query))
	 (gnosis-select 'id 'notes '1=1 t))
	;; All due notes
	((and (null tags) due (null deck))
	 (gnosis-review-get-due-notes))
	;; All notes for tags
	((and tags (null due) (null deck))
	 (gnosis-select-by-tag (gnosis-tag-prompt)))
	;; All due notes for tags
	((and tags due (null deck))
	 (gnosis-select-by-tag (gnosis-tag-prompt :due t)))
	;; All notes for deck
	((and (null tags) (null due) deck)
	 (gnosis-get-deck-notes nil nil))
	;; All due notes for deck
	((and (null tags) due deck)
	 (gnosis-get-deck-notes nil t))
	((and (null tags) (null due) (null deck) query)
	 (gnosis-search-note query))))

(defun gnosis-dashboard-output-notes (note-ids)
  "Return NOTE-IDS contents on gnosis dashboard."
  (cl-assert (listp note-ids) t "`note-ids' must be a list of note ids.")
  (pop-to-buffer "*gnosis-dashboard*")
  (gnosis-dashboard-mode)
  (setf tabulated-list-format [("Main" 30 t)
			       ("Options" 20 t)
			       ("Answer" 25 t)
			       ("Tags" 25 t)
			       ("Type" 10 t)
			       ("Suspend" 2 t)]
	tabulated-list-entries (cl-loop for id in note-ids
					for output = (gnosis-dashboard-output-note id)
					when output
					collect (list (number-to-string id) (vconcat output)))
	gnosis-dashboard-note-ids note-ids)
  (tabulated-list-init-header)
  ;; Keybindings, for editing, suspending, deleting notes.
  ;; We use `local-set-key' to bind keys to the buffer to avoid
  ;; conflicts when using the dashboard for displaying either notes
  ;; or decks.
  (local-set-key (kbd "e") #'gnosis-dashboard-edit-note)
  (local-set-key (kbd "s") #'(lambda () (interactive)
			       (gnosis-suspend-note (string-to-number (tabulated-list-get-id)))
			       (gnosis-dashboard-output-notes gnosis-dashboard-note-ids)
			       (revert-buffer t t t)))
  (local-set-key (kbd "a") #'gnosis-add-note)
  (local-set-key (kbd "r") #'gnosis-dashboard))

(defun gnosis-dashboard-deck-note-count (id)
  "Return total note count for deck with ID."
  (let ((note-count (caar (emacsql gnosis-db (format "SELECT COUNT(*) FROM notes WHERE deck_id=%s" id)))))
    (when (gnosis-select 'id 'decks `(= id ,id))
      (list (number-to-string note-count)))))

(defun gnosis-dashboard-output-deck (id)
  "Output contents from deck with ID, formatted for gnosis dashboard."
  (cl-loop for item in (append (gnosis-select
				'[name failure-factor ef-increase ef-decrease ef-threshold initial-interval]
				'decks `(= id ,id) t)
			       (mapcar 'string-to-number (gnosis-dashboard-deck-note-count id)))
	   when (listp item)
	   do (cl-remove-if (lambda (x) (and (vectorp x) (zerop (length x)))) item)
	   collect (format "%s" item)))

(defun gnosis-dashboard-output-decks ()
  "Return deck contents for gnosis dashboard."
  (pop-to-buffer "*gnosis-dashboard*")
  (gnosis-dashboard-mode)
  (setq tabulated-list-format [("Name" 15 t)
			       ("failure-factor" 15 t)
			       ("ef-increase" 15 t)
			       ("ef-decrease" 15 t)
			       ("ef-threshold" 15 t)
			       ("Initial Interval" 20 t)
			       ("Total Notes" 10 t)])
  (tabulated-list-init-header)
  (let ((max-id (apply 'max (gnosis-select 'id 'decks '1=1 t))))
    (setq tabulated-list-entries
	  (cl-loop for id from 1 to max-id
		   for output = (gnosis-dashboard-output-deck id)
		   when output
		   collect (list (number-to-string id) (vconcat output)))))
  (local-set-key (kbd "e") #'gnosis-dashboard-edit-deck)
  (local-set-key (kbd "a") #'(lambda () (interactive)
			       (gnosis-add-deck (read-string "Deck name: "))
			       (gnosis-dashboard-output-decks)
			       (revert-buffer t t t)))
  (local-set-key (kbd "s") #'(lambda () (interactive)
			       (gnosis-suspend-deck
				(string-to-number (tabulated-list-get-id)))
			       (gnosis-dashboard-output-decks)
			       (revert-buffer t t t)))
  (local-set-key (kbd "RET") #'(lambda () (interactive)
				 (gnosis-dashboard "notes"
						   (gnosis-collect-note-ids
						    :deck (string-to-number (tabulated-list-get-id)))))))

(defun gnosis-dashboard-edit-note (&optional dashboard)
  "Get note id from tabulated list and edit it.

DASHBOARD: Dashboard to return to after editing."
  (interactive)
  (let ((id (tabulated-list-get-id))
	(dashboard (or dashboard "notes")))
    (gnosis-edit-note (string-to-number id) nil dashboard)
    (message "Editing note with id: %s" id)))

(defun gnosis-dashboard-edit-deck ()
  "Get deck id from tabulated list and edit it."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gnosis-edit-deck (string-to-number id))))

(defvar-keymap gnosis-dashboard-mode-map
  :doc "gnosis-dashboard keymap"
  "q" #'quit-window)

(define-derived-mode gnosis-dashboard-mode tabulated-list-mode "Gnosis Dashboard"
  "Major mode for displaying Gnosis dashboard."
  :keymap gnosis-dashboard-mode-map
  (setq tabulated-list-padding 2
	tabulated-list-sort-key nil))

;;;###autoload
(cl-defun gnosis-dashboard (&optional dashboard-type (note-ids nil))
  "Display gnosis dashboard.

NOTE-IDS: List of note ids to display on dashboard.  When nil, prompt
for dashboard type.

DASHBOARD-TYPE: either 'Notes' or 'Decks' to display the respective dashboard."
  (interactive)
  (let ((dashboard-type (or dashboard-type (cadr (read-multiple-choice
						  "Display dashboard for:"
						  '((?n "notes")
						    (?d "decks")
						    (?t "tags")
						    (?s "search")))))))
    (if note-ids (gnosis-dashboard-output-notes note-ids)
      (pcase dashboard-type
	("notes" (gnosis-dashboard-output-notes (gnosis-collect-note-ids)))
	("decks" (gnosis-dashboard-output-decks))
	("tags"  (gnosis-dashboard-output-notes (gnosis-collect-note-ids :tags t)))
	("search" (gnosis-dashboard-output-notes (gnosis-collect-note-ids :query (read-string "Search for note: "))))))
    (tabulated-list-print t)))

(defun gnosis-db-init ()
  "Create gnosis essential directories & database."
  (let ((gnosis-curr-version (caar (emacsql gnosis-db (format "PRAGMA user_version")))))
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
      (gnosis--create-table 'extras gnosis-db-schema-extras))
    ;; Update database schema for version
    (cond ((= gnosis-curr-version 1) ;; Update to version 2
	   (emacsql gnosis-db [:alter-table decks :add failure-factor])
	   (emacsql gnosis-db [:alter-table decks :add ef-increase])
	   (emacsql gnosis-db [:alter-table decks :add ef-decrease])
	   (emacsql gnosis-db [:alter-table decks :add ef-threshold])
	   (emacsql gnosis-db [:alter-table decks :add initial-interval])
	   (emacsql gnosis-db (format "PRAGMA user_version = %s" gnosis-db-version))))))

(gnosis-db-init)

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode gnosis-modeline-mode
  "Minor mode for showing gnosis total due notes on modeline."
  :global t
  :group 'gnosis
  :lighter nil
  (if gnosis-modeline-mode
      (progn
        (add-to-list 'global-mode-string '(:eval
          (format " G:%d" (length (gnosis-review-get-due-notes)))))
        (force-mode-line-update))
    (setq global-mode-string
          (seq-remove (lambda (item)
                        (and (listp item) (eq (car item) :eval)
                             (string-prefix-p " G:" (format "%s" (eval (cadr item))))))
                      global-mode-string))
    (force-mode-line-update)))

(define-derived-mode gnosis-mode special-mode "Gnosis"
  "Gnosis Mode."
  :interactive nil
  (read-only-mode 0)
  (display-line-numbers-mode 0)
  :lighter " gnosis-mode")

(provide 'gnosis)
;;; gnosis.el ends here
