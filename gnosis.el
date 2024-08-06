;;; gnosis.el --- Spaced Repetition System  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis
;; Version: 0.3.2

;; Package-Requires: ((emacs "27.2") (emacsql "20240124") (compat "29.1.4.2") (transient "0.7.2"))

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

;; Gnosis is a spaced repetition system for note taking and
;; self-testing.  Notes are organized in a Question/Answer/Explanation
;; format and reviewed at spaced intervals.  Interval durations are
;; based on the success or failure of recalling the answer to each
;; question.

;; Gnosis uses a highly customizable algorithm.  Unlike traditional
;; methods, it doesn't depend on subjective user ratings to determine
;; the next review interval.  Instead, it evaluates the user's success
;; or failure in recalling an answer by typing it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'vc)
(require 'emacsql-sqlite)

(require 'gnosis-algorithm)
(require 'gnosis-string-edit)
(require 'gnosis-dashboard)

(require 'animate)

(defgroup gnosis nil
  "Spaced Repetition System For Note Taking & Self Testing."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-dir (locate-user-emacs-file "gnosis")
  "Gnosis directory."
  :type 'directory
  :group 'gnosis)

(defcustom gnosis-cloze-string "[...]"
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

(defcustom gnosis-review-keybindings
  '((?n . "next")
    (?o . "override")
    (?s . "suspend")
    (?e . "edit")
    (?q . "quit"))
  "List of action bindings for `gnosis-review-actions'.

Each element should be a list of a single character (the key),
a string describing the action."
  :type '(alist :key-type character :value-type string)
  :group 'gnosis)

(defvar gnosis-images-dir (expand-file-name "images" gnosis-dir)
  "Gnosis images directory.")

(unless (file-directory-p gnosis-dir)
  (make-directory gnosis-dir)
  (make-directory gnosis-images-dir))

(defvar gnosis-db
  (emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))
  "Gnosis database file.")

(defvar gnosis-testing nil
  "When t, warn user he is in a testing environment.")

(defconst gnosis-db-version 3
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

(defvar gnosis-mc-cloze-guidance
  '("MC-Cloze Example: This is an example answer&&option2&&option3" . ""))

(defcustom gnosis-mc-cloze-separator "&&"
  "Sseparator for choices on multiple choice clozes."
  :type 'string
  :group 'gnosis)

(defcustom gnosis-mcq-separator "\n--\n"
  "Separator for stem field and options in mcq note type.

Seperate the question/stem from options."
  :type 'string
  :group 'gnosis)

(defcustom gnosis-mcq-option-separator "-"
  "Separator for options in mcq note type."
  :type 'string
  :group 'gnosis)

(defcustom gnosis-center-content-p t
  "Non-nil means center content."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-apply-highlighting-p t
  "Non-nil means apply syntax highlighting."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-new-notes-limit nil
  "Total new notes limit."
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Number"))
  :group 'gnosis)

(defcustom gnosis-review-new-first t
  "Review new notes first.

When nil, review new notes last."
  :type 'bolean
  :group 'gnosis)

(defvar gnosis-due-notes-total nil
  "Total due notes.")

(defvar gnosis-review-notes nil
  "Review notes.")

(defvar gnosis-custom-values
  '((:deck "demo" (:proto (0 1 3) :anagnsois 3 :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 3))
    (:tag "demo" (:proto (1 2) :anagnosis 3 :epignosis 0.5 :agnoia 0.3 :amnesia 0.45 :lethe 3)))
  "Custom review values for adjusting gnosis algorithm.")

;;; Faces

(defgroup gnosis-faces nil
  "Faces used by gnosis."
  :group 'gnosis
  :tag "Gnosis Faces"
  :prefix 'gnosis-face)

(defface gnosis-face-extra
  '((t :inherit font-lock-doc-face))
  "Face for extra-notes."
  :group 'gnosis-faces)

(defface gnosis-face-main
  '((t :inherit default))
  "Face for the main section from note."
  :group 'gnosis-face-faces)

(defface gnosis-face-separator
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
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
  '((t :inherit (ffap italic)))
  "Face for clozes."
  :group 'gnosis-face)

(defface gnosis-face-false
  '((t :inherit error))
  "Face for user choice."
  :group 'gnosis-face)

(defface gnosis-face-unanswered
  '((t :inherit (italic underline)))
  "Face for unanswered clozes."
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

(defun gnosis-select-id (value table id)
  "Select VALUE from TABLE for note ID."
  (gnosis-select value table `(= id ,id) t))

(defun gnosis-table-exists-p (table)
  "Check if TABLE exists."
  (let ((tables (mapcar (lambda (str) (replace-regexp-in-string "_" "-" (symbol-name str)))
			(cdr (gnosis-select 'name 'sqlite-master '(= type table) t)))))
    (member (symbol-name table) tables)))

(cl-defun gnosis--create-table (table &optional values)
  "Create TABLE for VALUES."
  (unless (gnosis-table-exists-p table)
    (emacsql gnosis-db `[:create-table ,table ,values])))

(cl-defun gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (when (gnosis-table-exists-p table)
    (emacsql gnosis-db `[:drop-table ,table])))

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

(defun gnosis-get-type (id)
  "Return note for note ID."
  (car (gnosis-select-id 'type 'notes id)))

(defun gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql gnosis-db `[:delete :from ,table :where ,value]))

(defun gnosis-delete-note (id &optional verification)
  "Delete note with ID.

When VERIFICATION is non-nil, skip `y-or-n-p' prompt."
  (when (or verification (y-or-n-p "Delete note?"))
    (emacsql-with-transaction gnosis-db (gnosis--delete 'notes `(= id ,id)))))

(defun gnosis-delete-deck (&optional id)
  "Delete deck with ID."
  (interactive)
  (let* ((id (or id (gnosis--get-deck-id)))
	 (deck-name (gnosis--get-deck-name id)))
    (when (y-or-n-p (format "Delete deck `%s'? " deck-name))
      (emacsql-with-transaction gnosis-db (gnosis--delete 'decks `(= id ,id)))
      (message "Deleted deck `%s'" deck-name))))

(defun gnosis-shuffle (seq)
  "Shuffle SEQ."
  (cl-loop with len = (length seq)
           for i from len downto 2
           do (let ((j (random i)))  ; Get random index < i.
                (cl-rotatef (nth (1- i) seq) (nth j seq)))  ; Swap elements.
           finally return seq))

(defun gnosis-completing-read (prompt seq)
  "Call `gnosis-completing-read-function' with shuffled SEQ.

PROMPT: Prompt for `gnosis-completing-read-function'
History is disabled."
  (let ((history-add-new-input nil))
    (funcall gnosis-completing-read-function prompt (gnosis-shuffle seq))))

(defun gnosis-insert-separator ()
  "Insert a dashed line spanning the entire width of the buffer."
  (interactive)
  (let* ((width (window-width))
         (dash-line (concat (make-string width ?-))))
    (insert "\n" dash-line "\n")
    ;; Apply an overlay to hide only the dashes
    (let ((start (save-excursion (forward-line -1) (point)))
          (end (point)))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face 'gnosis-face-separator)
        (overlay-put overlay 'display (make-string width ?\s))))))

(defun gnosis-center-current-line (&optional center?)
  "Centers text in the current line ignoring leading spaces.

Acts only when CENTER? is t."
  (interactive)
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (text (string-trim (buffer-substring start end)))
         (padding (max (/ (- (window-width) (length text)) 2) 0))
	 (center? (or center? gnosis-center-content-p)))
    (if center?
	(progn (delete-region start end)
	       (insert (make-string padding ? ) text))
      (insert text))))

(defun gnosis-center-string (input-string &optional center?)
  "Center each line of the given INPUT-STRING in the current window width.

Acts only when CENTER? is t."
  (let ((window-width (window-width))
	(center? (or center? gnosis-center-content-p)))
    (if center?
	(mapconcat
	 (lambda (line)
           (let* ((text (string-trim line))
                  (wrapped (with-temp-buffer
                             (insert text)
                             (fill-region (point-min) (point-max))
                             (buffer-string)))
                  (lines (split-string wrapped "\n")))
             (mapconcat
              (lambda (line)
		(let ((padding (max (/ (- window-width (length line)) 2) 0)))
                  (concat (make-string padding ? ) line)))
              lines
              "\n")))
	 (split-string input-string "\n")
	 "\n")
      input-string)))

(defun gnosis-apply-center-buffer-overlay (&optional point)
  "Center text in buffer starting at POINT using `gnosis-center-current-line'.
This will not be applied to sentences that start with double space."
  (save-excursion
    (goto-char (or point (point-min)))
    (while (not (or (= (point-max) (point)) (looking-at "^  ")))
      (gnosis-center-current-line)
      (forward-line 1))))

(defun gnosis-apply-syntax-overlay ()
  "Apply custom font overlays for syntax highlighting, and remove delimiters."
  (let ((syntax-highlights '(("\\*\\([^*[:space:]][^*\n]*[^*[:space:]]\\)\\*" . bold)
                             ("/\\([^/[:space:]][^/\n]*[^/[:space:]]\\)/" . italic)
                             ("=\\([^=[:space:]][^=\n]*[^=[:space:]]\\)=" . font-lock-constant-face)
                             ("~\\([^~[:space:]][^~\n]*[^~[:space:]]\\)~" . font-lock-keyword-face)
                             ("_\\([^_[:space:]][^_\n]*[^_[:space:]]\\)_" . underline))))
    (when gnosis-apply-highlighting-p
      (save-excursion
	(cl-loop for (regex . face) in syntax-highlights
		 do (progn
                      (goto-char (point-min))
                      (while (re-search-forward regex nil t)
			(let ((start (match-beginning 1))
                              (end (match-end 1)))
			  (overlay-put (make-overlay start end) 'face face)
			  (delete-region end (match-end 0))
			  (delete-region (match-beginning 0) start)))))))))

(defun gnosis-display-question (id &optional fill-paragraph-p)
  "Display main row for note ID.

If FILL-PARAGRAPH-P, insert question using `fill-paragraph'."
  (let ((question (gnosis-get 'main 'notes `(= id ,id)))
	(fill-paragraph-p (or fill-paragraph-p t)))
    (erase-buffer)
    (if fill-paragraph-p
	(fill-paragraph (insert "\n"  (propertize question 'face 'gnosis-face-main)))
      (insert "\n"  (propertize question 'face 'gnosis-face-main)))
    (gnosis-insert-separator)
    (gnosis-apply-center-buffer-overlay)
    (gnosis-apply-syntax-overlay)))

(cl-defun gnosis-display-image (id &optional (image 'images))
  "Display image for note ID.

IMAGE is the image type to display, usually should be either =images'
or =extra-image'.  Instead of using =extra-image' post review, prefer
=gnosis-display-extra' which displays the =extra-image' as well.

Refer to =gnosis-db-schema-extras' for informations on images stored."
  (let* ((img (gnosis-get image 'extras `(= id ,id)))
         (path-to-image (expand-file-name (or img "") (file-name-as-directory gnosis-images-dir)))
         (image (create-image path-to-image 'png nil :width gnosis-image-width :height gnosis-image-height))
         (image-width (car (image-size image t)))
         (frame-width (window-text-width))) ;; Width of the current window in columns
    (cond ((or (not img) (string-empty-p img))
           (insert "\n\n"))
          ((and img (file-exists-p path-to-image))
           (let* ((padding-cols (/ (- frame-width (floor (/ image-width (frame-char-width)))) 2))
                  (padding (make-string (max 0 padding-cols) ?\s)))
             (insert "\n\n" padding)  ;; Insert padding before the image
             (insert-image image)
             (insert "\n\n"))))))

(defun gnosis-display-mcq-options (id)
  "Display answer options for mcq note ID."
  (let ((options (apply #'append (gnosis-select 'options 'notes `(= id ,id) t)))
	(option-num 1))
    (insert "\n" (propertize "Options:" 'face 'gnosis-face-directions))
    (cl-loop for option in options
	     do (insert (format "\n%s.  %s" option-num option))
	     (setf option-num (1+ option-num)))))

(defun gnosis-cloze-create (str clozes &optional cloze-string)
  "Replace CLOZES in STR with CLOZE-STRING."
  (cl-assert (listp clozes) nil "Adding clozes: Clozes need to be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (dolist (cloze clozes)
        (when (search-forward cloze nil t)
          (replace-match (propertize cloze-string 'face 'gnosis-face-cloze) nil t)))
      (buffer-string))))

(defun gnosis-cloze-add-hints (str hints &optional cloze-string)
  "Replace CLOZE-STRING in STR with HINTS."
  (cl-assert (listp hints) nil "Hints must be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string))
        (count 0))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (search-forward cloze-string nil t)
        (when (and (nth count hints) (search-backward cloze-string nil t))
          (replace-match (propertize (format "[%s]" (nth count hints)) 'face 'gnosis-face-cloze)))
        (setq count (1+ count)))
      (buffer-string))))

(defun gnosis-cloze-mark-answers (str answers face)
  "Mark ANSWERS in STR with FACE."
  (cl-assert (listp answers) nil "Answers to mark must be a list.")
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (dolist (answer answers)
      (when (search-forward answer nil t)
	(replace-match (propertize answer 'face face) nil t)))
    (buffer-string)))

(defun gnosis-cloze-mark-false (str answers)
  "Mark contents of STR as false for ANSWERS.

First item of answers will be marked as false, while the rest unanswered."
  (let* ((false (car answers))
	 (unanswered (cdr answers))
         (str-with-false (and answers (gnosis-cloze-mark-answers str (list false) 'gnosis-face-false)))
	 final)
    (if unanswered
	(setq final (gnosis-cloze-mark-answers str-with-false (if (listp unanswered) unanswered
								(list unanswered))
					       'gnosis-face-unanswered))
      (setq final (or str-with-false str)))
    final))

(defun gnosis-display-cloze-string (str clozes hints correct false)
  "Display STR with CLOZES and HINTS.

Applies highlighting for CORRECT & FALSE."
  (let* ((cloze-str (gnosis-cloze-create str clozes))
	 (str-with-hints (gnosis-cloze-add-hints cloze-str hints))
	 (str-with-c-answers (gnosis-cloze-mark-answers str-with-hints correct 'gnosis-face-correct))
	 (final (gnosis-cloze-mark-false str-with-c-answers false)))
    (erase-buffer)
    (insert "\n" (gnosis-center-string final))
    (gnosis-insert-separator)
    (gnosis-apply-syntax-overlay)))

(defun gnosis-display-basic-answer (answer success user-input)
  "Display ANSWER.

When SUCCESS nil, display USER-INPUT as well"
  (insert "\n\n"
	  (propertize "Answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize answer 'face 'gnosis-face-correct))
  (gnosis-center-current-line)
  ;; Insert user wrong answer
  (when (not success)
    (insert "\n"
	    (propertize "Your answer:" 'face 'gnosis-face-directions)
	    " "
	    (propertize user-input 'face 'gnosis-face-false))
    (gnosis-center-current-line)))

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
     (propertize answer 'face (if success 'gnosis-face-correct 'gnosis-face-false)))
    (gnosis-center-current-line)))


(defun gnosis-display-hint (hint)
  "Display HINT."
  (let ((hint (or hint "")))
    (unless (string-empty-p hint)
      (goto-char (point-max))
      (and (not (string-empty-p hint))
	   (insert (gnosis-center-string (propertize hint 'face 'gnosis-face-hint))))
      (gnosis-insert-separator))))

(cl-defun gnosis-display-cloze-user-answer (user-input &optional (false t))
  "Display USER-INPUT answer for cloze note upon failed review.

If FALSE t, use gnosis-face-false face"
  (goto-char (point-max))
  (insert "\n\n"
	  (propertize "Your answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize user-input 'face (if false 'gnosis-face-false 'gnosis-face-correct)))
  (gnosis-center-current-line)
  (newline))

(defun gnosis-display-correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ note."
  (insert (gnosis-center-string
	   (format "%s %s\n%s %s"
		   (propertize "Correct Answer:" 'face 'gnosis-face-directions)
		   (propertize answer 'face 'gnosis-face-correct)
		   (propertize "Your answer:" 'face 'gnosis-face-directions)
		   (propertize user-choice 'face (if (string= answer user-choice)
						     'gnosis-face-correct
						   'gnosis-face-false))))
	  "\n")
  (gnosis-insert-separator))

(defun gnosis-display-extra (id)
  "Display extra information & extra-image for note ID."
  (let ((extras (or (gnosis-get 'extra-notes 'extras `(= id ,id)) "")))
    (goto-char (point-max))
    (gnosis-display-image id 'extra-image)
    (insert "\n" (gnosis-center-string
		  (propertize extras 'face 'gnosis-face-extra))
	    "\n")
    (gnosis-apply-syntax-overlay)))

;;;###autoload
(defun gnosis-read-string-from-buffer (prompt string)
  "Switch to a new buffer to edit STRING in a recursive edit.
The user finishes editing with \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-done], or aborts with \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-abort]).

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If nil, no prompt will be
inserted in the buffer.

Also see `gnosis-string-edit'."
  (gnosis-string-edit prompt  string
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
	;; occur only when used for overriding review result.
        (progn (delete-region (point) (progn (end-of-line) (point)))
	       (insert (propertize (replace-regexp-in-string "\n" "" next-review-msg)
				   'face (if success 'gnosis-face-correct 'gnosis-face-false))))
      ;; Default behaviour
      (goto-char (point-max))
      (insert (gnosis-center-string next-review-msg)))))

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
    (let ((deck-id (gnosis-generate-id 5 t)))
      (gnosis--insert-into 'decks `([,deck-id ,name]))
      (message "Created deck '%s'" name))))

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

(defun gnosis-get-note-deck-name (id)
  "Return deck name of note ID."
  (let ((deck (gnosis-get 'deck-id 'notes `(= id ,id))))
    (and deck (gnosis--get-deck-name deck))))

(defun gnosis-get-deck--note (id &optional name)
  "Get deck id for note ID.

If NAME is t, return name of deck."
  (let* ((id-clause `(= id ,id))
	 (deck (gnosis-get 'deck-id 'notes id-clause)))
    (if name (gnosis--get-deck-name deck) deck)))

(cl-defun gnosis-suspend-note (id &optional verification)
  "Suspend note with ID.

When VERIFICATION is non-nil, skips `y-or-n-p' prompt."
  (let* ((suspended (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1))
	 (verification (or verification (y-or-n-p (if suspended "Unsuspend note? " "Suspend note? ")))))
    (when verification
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
  (let ((item (gnosis-completing-read "Suspend by: " '("Deck" "Tag"))))
    (pcase item
      ("Deck" (gnosis-suspend-deck))
      ("Tag" (gnosis-suspend-tag))
      (_ (message "Not ready yet.")))))

(defun gnosis-generate-id (&optional length deck-p)
  "Generate a unique gnosis ID.

Default to generating a note id, when DECK-P is t generates a deck id.

LENGTH: length of id, default to a random number between 10-15."
  (let* ((length (or length (+ (random 5) 10)))
         (max-val (expt 10 length))
         (min-val (expt 10 (1- length)))
         (id (+ (random (- max-val min-val)) min-val))
	 (current-ids (if deck-p (gnosis-select 'id 'decks '1=1 t)
			(gnosis-select 'id 'notes '1=1 t))))
    (if (member id current-ids)
        (gnosis-generate-id length)
      id)))

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

If a gnosis--insert-into fails, the whole transaction will be."
  (let* ((deck-id (gnosis--get-deck-id deck))
	 (initial-interval (gnosis-get-deck-initial-interval deck-id))
	 (note-id (gnosis-generate-id)))
    (emacsql-with-transaction gnosis-db
      ;; Refer to `gnosis-db-schema-SCHEMA' e.g `gnosis-db-schema-review-log'
      (gnosis--insert-into 'notes `([,note-id ,type ,main ,options ,answer ,tags ,deck-id]))
      (gnosis--insert-into 'review  `([,note-id ,gnosis-algorithm-ef ,gnosis-algorithm-ff ,initial-interval]))
      (gnosis--insert-into 'review-log `([,note-id ,(gnosis-algorithm-date)
						   ,(gnosis-algorithm-date) 0 0 0 0 ,suspend 0]))
      (gnosis--insert-into 'extras `([,note-id ,extra ,image ,second-image])))))

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

(defun gnosis-add-note-mcq (deck)
  "Add note(s) of type `MCQ' interactively to selected deck.

DECK: Deck to add gnosis

Prompt user for input to create a note of type `MCQ'.

Stem field is seperated from options by `gnosis-mcq-separator', and
each option is seperated by `gnosis-mcq-option-separator'.  The correct
answer is surrounded by curly braces, e.g {Correct Answer}.

Refer to `gnosis-add-note--mcq' & `gnosis-prompt-mcq-input' for more."
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
			  :tags (gnosis-prompt-tags--split gnosis-previous-note-tags))))

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

(defun gnosis-add-note-basic (deck)
  "Add note(s) of type `Basic' interactively to selected deck.

DECK: Deck name to add gnosis

Basic note type is a simple question/answer note, where user first
sees a \"main\" part, which is usually a question, and he is prompted
to input the answer.

Refer to `gnosis-add-note--basic' for more."
  (gnosis-add-note--basic :deck deck
			  :question (gnosis-read-string-from-buffer "Question: " "")
			  :answer (read-string "Answer: ")
			  :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			  :extra (gnosis-read-string-from-buffer "Extra: " "")
			  :images (gnosis-select-images)
			  :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))

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

(defun gnosis-add-note-double (deck)
  "Add note(s) of type double interactively to selected deck.

DECK: Deck name to add gnosis

Essentially, a \"note\" that generates 2 basic notes.  The second one
reverses question/answer.

Refer to `gnosis-add-note--double' for more."
  (gnosis-add-note--double :deck deck
			   :question (read-string "Question: ")
			   :answer (read-string "Answer: ")
			   :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			   :extra (gnosis-read-string-from-buffer "Extra" "")
			   :images (gnosis-select-images)
			   :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))

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

(defun gnosis-add-note-y-or-n (deck)
  "Add note(s) of type `y-or-n'.

DECK: Deck name to add gnosis

Refer to `gnosis-add-note--y-or-n' for more information about keyword values."
  (gnosis-add-note--y-or-n :deck deck
			   :question (gnosis-read-string-from-buffer "Question: " "")
			   :answer (read-char-choice "Answer: [y] or [n]? " '(?y ?n))
			   :hint (gnosis-hint-prompt gnosis-previous-note-hint)
			   :extra (gnosis-read-string-from-buffer "Extra" "")
			   :images (gnosis-select-images)
			   :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))


(cl-defun gnosis-add-note--cloze (&key deck note tags (suspend 0) extra (images nil))
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
  (let* ((notags-note (gnosis-cloze-remove-tags note))
	 (cloze-contents (gnosis-cloze-extract-contents note))
	 (clozes (gnosis-cloze-extract-answers cloze-contents))
	 (hints (gnosis-cloze-extract-hints cloze-contents)))
    (cl-loop for cloze in clozes
	     for hint in hints
	     do (gnosis-add-note-fields deck "cloze" notags-note hint cloze extra tags suspend
					(car images) (cdr images)))))

(defun gnosis-add-note-cloze (deck)
  "Add note(s) of type cloze interactively to selected deck.

DECK: Deck name to add gnosis

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
  (gnosis-add-note--cloze :deck deck
			  :note (gnosis-read-string-from-buffer (or (car gnosis-cloze-guidance) "")
								(or (cdr gnosis-cloze-guidance) ""))
			  :extra (gnosis-read-string-from-buffer "Extra" "")
			  :images (gnosis-select-images)
			  :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))

(cl-defun gnosis-mc-cloze-extract-options (str &optional (char gnosis-mc-cloze-separator))
  "Extract options for MC-CLOZE note type from STR.

CHAR: separator for mc-cloze, default to `gnosis-mc-cloze-separator'"
  (cl-remove-if
   #'null
   (mapcar (lambda (s)
             (when (string-match-p (regexp-quote char) s)
               (split-string s (regexp-quote char))))
           (split-string str " "))))

(cl-defun gnosis-add-note--mc-cloze (&key deck question options answer
					  extra (images nil) tags (suspend 0))
  "Add MC-CLOZE note type to DECK.

Refer to `gnosis-add-note-mc-cloze' for how this procedure should be used

DECK: Deck to add note to
QUESTION: Question, a string
OPTIONS: Answer options, a list of strings
ANSWER: the correct string, from OPTIONS.
EXTRA: Extra notes
IMAGES: Images to display during & after review
TAGS: Tags for note
SUSPEND: whether to suspend not"
  (cl-assert (stringp deck) nil "Deck name must be a string")
  (cl-assert (stringp question) nil "Question must be a string")
  (cl-assert (listp options) nil "Options must be a list")
  (cl-assert (stringp extra) nil "Extra value must be a string")
  (cl-assert (listp images) nil "Images must be a list of string paths")
  (cl-assert (listp tags) nil "Tags value must be a list of tags as strings")
  (cl-assert (or (= suspend 1) (= suspend 0)) nil "Suspend value must be either 0 or 1")
  (gnosis-add-note-fields deck "mc-cloze" question options answer extra tags (or suspend 0)
			  (car images) (cdr images)))

(defun gnosis-add-note-mc-cloze (deck)
  "Add MC-CLOZE note type to DECK.

MC-CLOZE (Multiple Choice Cloze) note type consists of a sentence with a
single cloze, for which the user will be prompted to select the correct
answer."
  (interactive)
  (let* ((input (gnosis-read-string-from-buffer (or (car gnosis-mc-cloze-guidance) "")
						(or (cdr gnosis-mc-cloze-guidance) "")))
	 (question (gnosis-mc-cloze-remove-separator input))
	 (options (gnosis-mc-cloze-extract-options input)))
    ;; Create a note for each option extracted
    (cl-loop for option in options
	     do (gnosis-add-note--mc-cloze :deck deck
					   :question question
					   :options option
					   :answer (car option)
					   :extra (gnosis-read-string-from-buffer "Extra" "")
					   :images (gnosis-select-images)
					   :tags (gnosis-prompt-tags--split gnosis-previous-note-tags)))))

;;;###autoload
(defun gnosis-add-note (&optional deck type)
  "Create note(s) as TYPE interactively.

DECK: Deck name to add gnosis
TYPE: Type of gnosis note, must be one of `gnosis-note-types'"
  (interactive)
  (when gnosis-testing
    (unless (y-or-n-p "You are using a testing environment! Continue?")
      (error "Aborted")))
  (let* ((deck (or deck (gnosis--get-deck-name)))
	 (type (or type (completing-read "Type: " gnosis-note-types nil t)))
	 (func-name (intern (format "gnosis-add-note-%s" (downcase type)))))
    (if (fboundp func-name)
	(progn (funcall func-name deck)
	       (pcase (cadr (read-multiple-choice
			     "Add more gnosis?"
			     '((?y "yes")
			       (?r "repeat")
			       (?n "no"))))
		 ("yes" (gnosis-add-note))
		 ("repeat" (gnosis-add-note deck type))
		 ("no" nil)))
      (message "No such type"))))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'options 'notes `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (gnosis-completing-read "Answer: " choices)))

(defun gnosis-cloze-check (sentence clozes)
  "Check if CLOZES are found in SENTENCE."
  (catch 'not-found
    (dolist (cloze clozes)
      (unless (string-match-p cloze sentence)
        (throw 'not-found nil)))
    t))

(defun gnosis-cloze-remove-tags (string)
  "Replace cloze tags and hints in STRING.

Works with both single (:), double colons (::), single braces ({}) and
double braces ({{}})."
  (let* ((regex "{\\{1,2\\}c[0-9]+:\\{1,2\\}\\([^:}]*\\).*?\\(}\\|::}\\)\\{1,2\\}")
         (result (replace-regexp-in-string regex "\\1" string)))
    result))

(defun gnosis-cloze-extract-contents (str)
  "Extract cloze contents for STR.

Return a list of cloze tag contents for STR, organized by cX-tag.

Valid cloze formats include:
\"This is an {c1:example}\"
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

(defun gnosis-cloze-extract-answers (nested-lst)
  "Extract cloze answers for string clozes inside the NESTED-LST.

This function should be used in combination with `gnosis-cloze-extract-answers'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (replace-regexp-in-string "::\\(.*\\)" "" str))
                    lst))
          nested-lst))

(defun gnosis-cloze-extract-hints (nested-lst)
  "Extract cloze hints for string clozes inside the NESTED-LST.

This function should be used in combination with `gnosis-cloze-extract-answers'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (when (string-match "::\\(.*\\)" str)
                        (match-string 1 str)))
                    lst))
          nested-lst))

(defun gnosis-mc-cloze-remove-separator (string &optional separator)
  "Remove SEPARATOR and all followed words from STRING."
  (let* ((separator (or separator gnosis-mc-cloze-separator))
	 (result (replace-regexp-in-string (format "%s[^ ]*" separator) "" string)))
    result))

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
			(gnosis-completing-read prompt
						(cons nil (gnosis-directory-files gnosis-images-dir)))
		      nil))
	     (extra-image (if (y-or-n-p "Add post review image?")
			      (gnosis-completing-read prompt
						      (cons nil (gnosis-directory-files gnosis-images-dir))))))
	(cons image extra-image))
    nil))

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (cl-loop for tags in (gnosis-select 'tags 'notes '1=1 t)
           nconc tags into all-tags
           finally return (delete-dups all-tags)))

(defun gnosis-select-by-tag (input-tags &optional due)
  "Return note ID's for every note with INPUT-TAGS.

If DUE, return only due notes."
  (cl-assert (listp input-tags) t "Input tags must be a list")
  (cl-assert (booleanp due) "Due value must be a boolean")
  (cl-loop for (id tags) in (gnosis-select '[id tags] 'notes)
           when (and (cl-every (lambda (tag) (member tag tags)) input-tags)
		     (not (gnosis-suspended-p id))
		     (if due (gnosis-review-is-due-p id) t))
           collect id))

(defun gnosis-get-tag-notes (tag)
  "Return note ids for TAG."
  (let ((notes (gnosis-select 'id 'notes `(like tags ',(format "%%\"%s\"%%" tag)) t)))
    notes))

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
    (cl-loop for tag = (completing-read
			(concat prompt (format " (%s) (q for quit): " (mapconcat #'identity tags " ")))
			(cons "q" (if due (gnosis-review-get-due-tags)
				    (gnosis-get-tags--unique)))
			nil t)
	     until (string= tag "q")
	     unless (member tag tags)
	     do (push tag tags))
    tags))

(defun gnosis-hint-prompt (previous-hint &optional prompt)
  "Prompt user for hint.

PROMPT: Prompt string value
PREVIOUS-HINT: Previous hint value, if any.  If nil, use PROMPT as
default value."
  (let* ((prompt (or prompt "Hint: "))
	 (hint (read-string prompt previous-hint)))
    (setf gnosis-previous-note-hint hint)
    hint))

(defun gnosis-prompt-mcq-input (&optional prompt string)
  "PROMPT for MCQ note content.

STRING: Guidance string."
  (let ((user-input (gnosis-read-string-from-buffer (or prompt (car gnosis-mcq-guidance) "")
						    (or string (cdr gnosis-mcq-guidance) ""))))
    (cond ((not (string-match-p gnosis-mcq-separator user-input))
	   (gnosis-prompt-mcq-input (format "`gnosis-mcq-separator': %s not found!" gnosis-mcq-separator)
				    user-input))
	  ((not (string-match "{.*}" user-input))
	   (gnosis-prompt-mcq-input (format "Please wrap the right option with {}")
				    user-input))
	  (t (gnosis-mcq-process-input user-input)))))

(defun gnosis-mcq-process-input (user-input &optional stem-separator option-separator)
  "Process USER-INPUT for MCQ note.

STEM-SEPARATOR: Separator of question stem & options
OPTION-SEPARATOR: Separator of each option

Return ((QUESTION CHOICES) CORRECT-CHOICE-INDEX)"
  (let* ((stem-separator (or stem-separator gnosis-mcq-separator))
	 (option-separator (or option-separator gnosis-mcq-option-separator))
	 (input-separated (split-string user-input stem-separator t "[\s\n]"))
	 (stem (car input-separated))
	 (input (split-string
		 (mapconcat 'identity (cdr input-separated) "\n")
		 option-separator t "[\s\n]"))
	 (correct-choice-index
	  ;; Make sure correct choice is given
	  (or (cl-position-if (lambda (string) (string-match "{.*}" string)) input)
	      (error "Correct choice not found.  Use {} to indicate the correct option")))
	 (choices (mapcar (lambda (string) (replace-regexp-in-string "{\\|}" "" string)) input)))
    (list (cons stem choices) (+ correct-choice-index 1))))

(defun gnosis-prompt-tags--split (&optional previous-note-tags)
  "Prompt user for tags, split string by space.

Return a list of tags, split by space.  If PREVIOUS-NOTE-TAGS is
provided, use it as the default value."
  (let* ((previous-note-tags (or nil previous-note-tags))
	 (tags (split-string (read-from-minibuffer "Tags: " (mapconcat #'identity previous-note-tags " ")) " ")))
    (setf gnosis-previous-note-tags tags)
    (if (equal tags '("")) '("untagged") tags)))

;; Collecting note ids

;; TODO: Rewrite.  Tags should be an input of strings, interactive
;; handling should be done by "helper" funcs
(cl-defun gnosis-collect-note-ids (&key (tags nil) (due nil) (deck nil) (query nil))
  "Return list of note ids based on TAGS, DUE, DECKS, QUERY.

TAGS: boolean value, t to specify tags.
DUE: boolean value, t to specify due notes.
DECK: Integer, specify deck id.
QUERY: String value,"
  (cl-assert (and (booleanp due) (booleanp tags) (or (numberp deck) (null deck)) (or (stringp query) (null query)))
	     nil "Incorrect value passed to `gnosis-collect-note-ids'")
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
	 (gnosis-select-by-tag (gnosis-tag-prompt) t))
	;; All notes for deck
	((and (null tags) (null due) deck)
	 (gnosis-get-deck-notes deck nil))
	;; All due notes for deck
	((and (null tags) deck due)
	 (gnosis-get-deck-notes deck t))
	;; Query
	((and (null tags) (null due) (null deck) query)
	 (gnosis-search-note query))))

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
  (let* ((old-notes (cl-loop for note in (gnosis-select 'id 'review-log '(and (> n 0)
									      (= suspend 0))
							t)
			     when (gnosis-review-is-due-p note)
			     collect note))
	 (new-notes (cl-loop for note in (gnosis-select 'id 'review-log '(and (= n 0)
									      (= suspend 0))
							t)
			     when (gnosis-review-is-due-today-p note)
			     collect note)))
    (if gnosis-review-new-first
	(append (cl-subseq new-notes 0 gnosis-new-notes-limit) old-notes)
      (append old-notes (cl-subseq new-notes 0 gnosis-new-notes-limit)))))

(defun gnosis-review-get-due-tags ()
  "Return a list of due note tags."
  (let ((due-notes (gnosis-review-get-due-notes)))
    (cl-remove-duplicates
     (cl-mapcan (lambda (note-id)
                  (gnosis-get 'tags 'notes `(= id ,note-id)))
	        due-notes)
     :test #'equal)))

(defun gnosis-review--get-offset (id)
  "Return offset for note with value of id ID."
  (let ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev)))

(defun gnosis-review-last-interval (id)
  "Return last review interval for note ID."
  (let* ((where-id-clause `(= id ,id))
         (last-rev (gnosis-get 'last-rev 'review-log where-id-clause))
	 (rev-date (gnosis-get 'next-rev 'review-log where-id-clause)))
    (max (gnosis-algorithm-date-diff last-rev rev-date) 1)))

(defun gnosis-review-algorithm (id success)
  "Return next review date & ef for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure.

Returns a list of the form ((yyyy mm dd) (ef-increase ef-decrease ef-total))."
  (let ((ff (gnosis-get-note-ff id))
	(ef (gnosis-get 'ef 'review `(= id ,id)))
	(t-success (gnosis-get 't-success 'review-log `(= id ,id))) ;; total successful reviews
	(c-success (gnosis-get 'c-success 'review-log `(= id ,id))) ;; consecutive successful reviews
	(c-fails (gnosis-get 'c-fails 'review-log `(= id ,id))) ;; consecutive failed reviews
	;; (t-fails (gnosis-get 't-fails 'review-log `(= id ,id))) ;; total failed reviews
	;; (review-num (gnosis-get 'n 'review-log `(= id ,id))) ;; total reviews
	;; (last-interval (max (gnosis-review--get-offset id) 1))
	(last-interval (gnosis-review-last-interval id))) ;; last interval
    (list (gnosis-algorithm-next-interval :last-interval last-interval
					  :ef (nth 2 ef) ;; total ef is used for next interval
					  :success success
					  :successful-reviews t-success
					  :c-fails c-fails
					  :threshold 3 ;;TODO: Create a gnosis-interval-thershold
					  :failure-factor ff
					  :initial-interval (gnosis-get-note-initial-interval id))
	  (gnosis-algorithm-next-ef :ef ef
				    :success success
				    :increase (gnosis-get-ef-increase id)
				    :decrease (gnosis-get-ef-decrease id)
				    :threshold (gnosis-get-ef-threshold id)
				    :c-successes c-success
				    :c-failures c-fails))))

(defun gnosis-review--update (id success)
  "Update review-log for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure."
  (let ((ef (cadr (gnosis-review-algorithm id success)))
	(next-rev (car (gnosis-review-algorithm id success))))
    ;; Update activity-log
    (gnosis-review-increment-activity-log (gnosis-review-is-note-new-p id))
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

(defun gnosis-review-result (id success)
  "Update review note ID results for SUCCESS."
  (gnosis-review--update id success)
  (setf gnosis-due-notes-total (length (gnosis-review-get-due-notes))))

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

(defun gnosis-review-cloze (id)
  "Review cloze type note for ID."
  (let* ((main (gnosis-get 'main 'notes `(= id ,id)))
	 (clozes (gnosis-get 'answer 'notes `(= id ,id)))
	 (num 0) ;; Number of clozes revealed
	 (hints (gnosis-get 'options 'notes `(= id ,id)))
	 (success nil))
    ;; Quick fix for old cloze note versions.
    (cond ((and (stringp hints) (string-empty-p hints))
	   (setq hints nil))
	  ((and (not (listp hints)) (not (string-empty-p hints)))
	   (setq hints (list hints))))
    ;; Initially display the sentence with no reveals
    (gnosis-display-cloze-string main clozes hints nil nil)
    (cl-loop for cloze in clozes
	     do (let ((input (gnosis-review-cloze--input cloze)))
		  (if (equal (car input) t)
		      ;; Correct answer -> reveal the current cloze
		      (progn (cl-incf num)
			     (gnosis-display-cloze-string main (nthcdr num clozes)
							    (nthcdr num hints)
							    (cl-subseq clozes 0 num)
							    nil))
		    ;; Incorrect answer
		    (gnosis-display-cloze-string main nil nil (cl-subseq clozes 0 num) (member cloze clozes))
		    (gnosis-display-cloze-user-answer (cdr input))
		    (setq success nil)
		    (cl-return)))
	     ;; Update note after all clozes are revealed successfully
	     finally (setq success t))
    (gnosis-display-extra id)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-mc-cloze (id)
  "Review MC-CLOZE note of ID."
  (let ((main (gnosis-get 'main 'notes `(= id ,id)))
	;; Cloze needs to be a list, we take car as the answer
	(cloze (list (gnosis-get 'answer 'notes `(= id ,id))))
	(user-choice nil)
	(success nil))
    (gnosis-display-cloze-string main cloze nil nil nil)
    (gnosis-display-image id)
    (setf user-choice (gnosis-mcq-answer id)
	  success (string= user-choice (car cloze)))
    (if success
	(gnosis-display-cloze-string main nil nil cloze nil)
      (gnosis-display-cloze-string main nil nil nil cloze))
    ;; Display user answer only upon failure
    (unless success
      (gnosis-display-cloze-user-answer user-choice))
    (gnosis-display-extra id)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-is-note-new-p (id)
  "Return t if note with ID is new."
  (let ((reviews (car (gnosis-select-id 'n 'review-log id))))
    (not (> reviews 0))))

(defun gnosis-review-increment-activity-log (new? &optional date)
  "Increament activity log for DATE by one.

If NEW? is non-nil, increment new notes log by 1."
  (let* ((current-total-value (gnosis-get-date-total-notes))
	 (inc-total (cl-incf current-total-value))
	 (current-new-value (gnosis-get-date-new-notes))
	 (inc-new (cl-incf current-new-value))
	 (date (or date (gnosis-algorithm-date))))
    (gnosis-update 'activity-log `(= reviewed-total ,inc-total) `(= date ',date))
    (and new? (gnosis-update 'activity-log `(= reviewed-new ,inc-new) `(= date ',date)))))

(defun gnosis-review-note (id &optional date)
  "Start review for note with value of id ID, if note is unsuspended.

DATE: Date to log the note review on the activity-log."
  (when (gnosis-suspended-p id)
    (message "Suspended note with id: %s" id)
    (sit-for 0.3)) ;; this should only occur in testing/dev cases
  (let* ((type (gnosis-get 'type 'notes `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s" (downcase type))))
	 (date (or date (gnosis-algorithm-date))))
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
    ;; Fix sync by adding a small delay
    (sit-for 0.3)
    ;; Reopen gnosis-db after pull
    (setf gnosis-db (emacsql-sqlite-open (expand-file-name "gnosis.db" dir)))))

(defun gnosis-review-commit (note-count)
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one.  It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number of notes.

NOTE-COUNT: The number of notes reviewed in the session to be commited."
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
			     (shell-quote-argument
			      (format "Reviewed %d notes." note-count)))))
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))
    (message "Review session finished.")))

(defun gnosis-review-action--edit (success note note-count)
  "Edit NOTE during review.

Save current contents of *gnosis-edit* buffer, if any, and start
editing NOTE with it's new contents.

After done editing, call `gnosis-review-actions' with SUCCESS NOTE
NOTE-COUNT."
  (gnosis-edit-save-exit)
  (gnosis-edit-note note t)
  (recursive-edit)
  (gnosis-review-actions success note note-count))

(defun gnosis-review-action--quit (success note note-count)
  "Quit review session.

Update result for NOTE review with SUCCESS and commit session for NOTE-COUNT.

This function should be used with `gnosis-review-actions', to finish
the review session."
  (gnosis-review-result note success)
  (gnosis-review-commit note-count)
  ;; Break the loop of `gnosis-review-session'
  (throw 'review-loop t))

(defun gnosis-review-action--suspend (success note note-count)
  "Suspend/Unsuspend NOTE.

This function should be used with `gnosis-review-actions', which
should be recursively called using SUCCESS, NOTE, NOTE-COUNT."
  (gnosis-suspend-note note)
  (gnosis-review-actions success note note-count))

(defun gnosis-review-action--override (success note note-count)
  "Override current review result for SUCCESS.

This function should be used with `gnosis-review-actions', which will
be called with new SUCCESS value plus NOTE & NOTE-COUNT."
  (setf success (if success nil t))
  (gnosis-display-next-review note success)
  (gnosis-review-actions success note note-count))

(defun gnosis-validate-actions-keys ()
  "Ensure all actions in `gnosis-review-actions-keys` are valid."
  (let ((valid-actions '("next" "override" "suspend" "edit" "quit")))
    (dolist (entry gnosis-review-keybindings)
      (cl-assert (member (cdr entry) valid-actions) nil
                 "Invalid action: %s" (cdr entry)))))

(defun gnosis-review-actions (success note note-count)
  "Specify action during review of note.

SUCCESS: Review result
NOTE: Note ID
NOTE-COUNT: Total notes reviewed

To customize the keybindings, adjust `gnosis-review-keybindings'."
  (gnosis-validate-actions-keys)
  (let* ((choices (mapcar (lambda (pair)
                            (list (car pair) (cdr pair)))
                          gnosis-review-keybindings))
         (choice (car (read-multiple-choice "Note actions" choices)))
         (action (alist-get choice gnosis-review-keybindings)))
    (pcase action
      ("next" (gnosis-review-result note success))
      ("override" (gnosis-review-action--override success note note-count))
      ("suspend" (gnosis-review-action--suspend success note note-count))
      ("edit" (gnosis-review-action--edit success note note-count))
      ("quit" (gnosis-review-action--quit success note note-count)))))

(defun gnosis-review-session (notes &optional due note-count)
  "Start review session for NOTES.

NOTES: List of note ids
DUE: If due is non-nil, session will loop for due notes.
NOTE-COUNT: Total notes to be commited for session."
  (let ((note-count (or note-count 0))
	(date (gnosis-algorithm-date)))
    (if (null notes)
	(message "No notes for review.")
      (setf gnosis-review-notes notes)
      (catch 'review-loop
	(cl-loop for note in notes
		 do (let ((success (gnosis-review-note note date)))
		      (cl-incf note-count)
		      (gnosis-review-actions success note note-count))
		 finally
		 ;; TODO: Add optional arg to repeat for specific deck/tag
		 ;; Repeat until there are no due notes
		 (and due (gnosis-review-session (gnosis-collect-note-ids :due t) t note-count))))
      (gnosis-dashboard)
      (gnosis-review-commit note-count))))

;;;###autoload
(defun gnosis-review ()
  "Start gnosis review session."
  (interactive)
  ;; Refresh modeline
  (setq gnosis-due-notes-total (length (gnosis-review-get-due-notes)))
  ;; Select review type
  (let ((review-type (gnosis-completing-read "Review: " '("Due notes"
							  "Due notes of deck"
							  "Due notes of specified tag(s)"
							  "All notes of tag(s)"))))
    (pcase review-type
      ("Due notes" (gnosis-review-session (gnosis-collect-note-ids :due t) t))
      ("Due notes of deck" (gnosis-review-session
			    (gnosis-collect-note-ids :due t :deck (gnosis--get-deck-id))))
      ("Due notes of specified tag(s)" (gnosis-review-session (gnosis-collect-note-ids :due t :tags t)))
      ("All notes of deck" (gnosis-review-session (gnosis-collect-note-ids :deck (gnosis--get-deck-id))))
      ("All notes of tag(s)" (gnosis-review-session (gnosis-collect-note-ids :tags t))))))


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
		      (cl-every #'integerp initial-interval))
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
  (when (get-buffer "*gnosis-edit*")
    (switch-to-buffer "*gnosis-edit*")
    (eval-buffer)
    (quit-window t)
    (when exit-func
      (apply exit-func args))))

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
  (when (and (string= (gnosis-get-type id) "cloze")
	     (not (stringp options)))
    (cl-assert (or (listp options) (stringp options)) nil "Options must be a list or a string.")
    (cl-assert (gnosis-cloze-check main answer) nil "Clozes are not part of the question (main).")
    (cl-assert (>= (length answer) (length options)) nil "Hints (options) must be equal or less than clozes (answer).")
    (cl-assert (cl-every (lambda (item) (or (null item) (stringp item))) options) nil "Hints (options) must be either nil or a string."))
  ;; Construct the update clause for the emacsql update statement.
  (cl-loop for (field . value) in `((main . ,main)
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


(defun gnosis-get-date-total-notes (&optional date)
  "Return total notes reviewed for DATE.

If entry for DATE does not exist, it will be created.

Defaults to current date."
  (cl-assert (listp date) nil "Date must be a list.")
  (let* ((date (or date (gnosis-algorithm-date)))
	 (reviewed-total (car (gnosis-select 'reviewed-total 'activity-log `(= date ',date) t)))
	 (reviewed-new (or (car (gnosis-select 'reviewed-new 'activity-log `(= date ',date) t)) 0)))
    (or reviewed-total
	(progn
	  ;; Using reviewed-new instead of hardcoding 0 just to not mess up tests.
	  (and (equal date (gnosis-algorithm-date))
	       (gnosis--insert-into 'activity-log `([,date 0 ,reviewed-new])))
	  0))))

(defun gnosis-get-date-new-notes (&optional date)
  "Return total notes reviewed for DATE.

Defaults to current date."
  (cl-assert (listp date) nil "Date must be a list.")
  (let* ((date (or date (gnosis-algorithm-date)))
	 (reviewed-new (or (car (gnosis-select 'reviewed-new 'activity-log `(= date ',date) t)) 0)))
    reviewed-new))

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
			(gnosis-select '[gnosis amnesia] 'review `(= id ,id) t)
			(gnosis-select 'suspend 'review-log `(= id ,id) t)))
	(fields (list :id :main :options :answer :tags
		      :extra-notes :image :second-image :gnosis :amnesia :suspend)))
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
				   (gnosis integer :not-null)
				   (amnesia integer :not-null)]
				  (:foreign-key [id] :references notes [id]
						:on-delete :cascade)))

(defvar gnosis-db-schema-review-log '([(id integer :primary-key :not-null) ;; note-id
				       (last-rev integer :not-null)  ;; Last review date
				       (next-rev integer :not-null)  ;; Next review date
				       (c-success integer :not-null) ;; Consecutive successful reviews
				       (t-success integer :not-null) ;; Total successful reviews
				       (c-fails integer :not-null)   ;; Consecutive failed reviewss
				       (t-fails integer :not-null)   ;; Total failed reviews
				       (suspend integer :not-null)   ;; Binary value, 1=suspended
				       (n integer :not-null)]        ;; Number of reviews
				      (:foreign-key [id] :references notes [id]
						    :on-delete :cascade)))

(defvar gnosis-db-schema-activity-log '([(date text :not-null)
					 (reviewed-total integer :not-null)
					 (reviewed-new integer :not-null)]))

(defvar gnosis-db-schema-extras '([(id integer :primary-key :not-null)
				   (extra-notes string)
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
         (clause-main `(and ,@(mapcar (lambda (word)
					`(like main ,(format "%%%s%%" word)))
                                      words)))
	 (clause-answer `(and ,@(mapcar (lambda (word)
					  `(like answer ,(format "%%%s%%" word)))
					words))))
    (append (gnosis-select 'id 'notes clause-main t)
	    (gnosis-select 'id 'notes clause-answer t))))

(defun gnosis-db-update-v2 ()
  "Update to first gnosis-db version."
  (emacsql-with-transaction gnosis-db
    (emacsql gnosis-db [:alter-table decks :add failure-factor])
    (emacsql gnosis-db [:alter-table decks :add ef-increase])
    (emacsql gnosis-db [:alter-table decks :add ef-decrease])
    (emacsql gnosis-db [:alter-table decks :add ef-threshold])
    (emacsql gnosis-db [:alter-table decks :add initial-interval])
    (emacsql gnosis-db (format "PRAGMA user_version = 2"))
    (gnosis--create-table 'activity-log gnosis-db-schema-activity-log)
    ;; Update to most recent gnosis db version.
    (gnosis-db-update-v3)))

(defun gnosis-db-update-v3 ()
  "Upgrade database to version 3."
  (emacsql-with-transaction gnosis-db
    (emacsql gnosis-db [:alter-table decks :drop-column failure-factor])
    (emacsql gnosis-db [:alter-table decks :drop-column ef-increase])
    (emacsql gnosis-db [:alter-table decks :drop-column ef-threshold])
    (emacsql gnosis-db [:alter-table decks :drop-column ef-decrease])
    (emacsql gnosis-db [:alter-table decks :drop-column initial-interval])
    ;; Review changes
    (emacsql gnosis-db [:alter-table review :rename ef :to gnosis])
    (emacsql gnosis-db [:alter-table review :rename ff :to amnesia])
    (emacsql gnosis-db [:alter-table review :drop-column interval])
    ;; Add activity log
    (gnosis--create-table 'activity-log gnosis-db-schema-activity-log)
    ;; Update version
    (emacsql gnosis-db (format "PRAGMA user_version = %s" gnosis-db-version))))

(defun gnosis-db-init ()
  "Create essential directories & database."
  (let ((gnosis-curr-version (caar (emacsql gnosis-db (format "PRAGMA user_version")))))
    (unless (length= (emacsql gnosis-db [:select name :from sqlite-master :where (= type table)]) 7)
      (emacsql-with-transaction gnosis-db
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
	(gnosis--create-table 'extras gnosis-db-schema-extras)
	;; Create activity-log table
	(gnosis--create-table 'activity-log gnosis-db-schema-activity-log)))
    ;; Update database schema for version
    (cond ((<= gnosis-curr-version 2)
	   (gnosis-db-update-v3)))))

(gnosis-db-init)

;;;; Gnosis Demo ;;;;
;;;;;;;;;;;;;;;;;;;;;

(defun gnosis-animate-string (string vpos &optional hpos string-section face)
  "Display STRING animations starting at position VPOS, HPOS in BUFFER-NAME.

If STRING-SECTION and FACE are provided, highlight the occurrences of
STRING-SECTION in the STRING with FACE.

If STRING-SECTION is nil, apply FACE to the entire STRING."
  (let ((animate-n-steps 60))
    (goto-char (point-min))
    (animate-string string vpos hpos)
    (and face
	 (if string-section
	     (progn
	       (goto-char (point-min))
	       (while (search-forward string-section nil t)
		 (add-text-properties (match-beginning 0) (match-end 0) `(face ,face))))
	   (add-text-properties (line-beginning-position) (line-end-position) `(face ,face))))))

;;;###autoload
(defun gnosis-demo ()
  "Start gnosis demo."
  (interactive)
  (pop-to-buffer-same-window "*Gnosis Demo*")
  (fundamental-mode)
  (setq-local display-line-numbers nil)
  (erase-buffer)
  (gnosis-animate-string "Welcome to the Gnosis demo!" 2 nil "Gnosis demo" 'underline)
  (sit-for 1)
  (gnosis-animate-string "Gnosis is a tool designed to create a gnostikon" 3 nil "gnostikon" 'bold-italic)
  (sit-for 1.5)
  (gnosis-animate-string "--A place to store & test your knowledge--" 4 nil nil 'italic)
  (sit-for 1)
  (gnosis-animate-string "The objective of gnosis is to maximize memory retention, through repetition." 6 nil
			 "maximize memory retention" 'underline)
  (sit-for 1)
  (gnosis-animate-string "Remember, repetitio est mater memoriae" 8 nil
			 "repetitio est mater memoriae" 'bold-italic)
  (sit-for 0.5)
  (gnosis-animate-string "-- repetition is the mother of memory --" 9 nil
			 "repetition is the mother of memory" 'italic)
  (sit-for 1)
  (gnosis-animate-string "Consistency is key; be sure to do your daily reviews!" 11 nil "Consistency is key" 'bold)
  (sit-for 1)
  (when (y-or-n-p "Try out demo gnosis review session?")
    (gnosis-demo-create-deck)
    (gnosis-review-session (gnosis-select-by-tag '("demo")))))

(defun gnosis-demo-create-deck ()
  "Create demo deck."
  (let ((deck-name "demo")
	(note-tags '("demo")))
    (if (not (cl-some #'(lambda (x) (member "demo" x)) (gnosis-select 'name 'decks)))
	(progn (gnosis-add-deck deck-name)
	       (gnosis-add-note--basic :deck deck-name
				       :question "Repetitio est mater memoriae"
				       :hint "Translate this Latin phrase to English."
				       :answer "Repetition is the mother of memory"
				       :extra "/Regular review/ at increasing intervals *reinforces* *memory* *retention*.  Strengthening neural connections & making it easier to recall information long-term"
				       :tags note-tags)
	       (gnosis-add-note--mc-cloze :deck deck-name
					  :question "Consistency is _key_ to using gnosis effectively."
					  :options '("Consistency" "Procrastination" "Incosistency")
					  :answer "Consistency"
					  :extra "Avoid monotony, try to engage with the material actively, and stay _consistent_!"
					  :tags note-tags)
	       (gnosis-add-note--mcq :deck deck-name
				     :question "Which one is the capital of Greece?"
				     :choices '("Athens" "Sparta" "Rome" "Berlin")
				     :correct-answer 1
				     :extra "Athens (Αθήνα) is the largest city of Greece & one of the world's oldest cities, with it's recorded history spanning over 3,500 years."
				     :tags note-tags)
	       (gnosis-add-note--cloze :deck deck-name
				       :note "GNU Emacs is an extensible editor created by {{c1::Richard}} {{c1::Stallman}} in {{c2::1984::year}}"
				       :tags note-tags
				       :extra "Emacs was originally implemented in 1976 on the MIT AI Lab's Incompatible Timesharing System (ITS), as a collection of TECO macros. The name “Emacs” was originally chosen as an abbreviation of “Editor MACroS”. =This version of Emacs=, GNU Emacs, was originally *written in 1984*")
	       (gnosis-add-note--y-or-n :deck deck-name
					:question "Is GNU Emacs the greatest program ever written?"
					:hint "Duh"
					:answer 121
					:extra ""
					:tags note-tags))
      (error "Demo deck already exists"))))

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode gnosis-modeline-mode
  "Minor mode for showing gnosis total due notes on modeline."
  :global t
  :group 'gnosis
  :lighter nil
  (setq gnosis-due-notes-total (length (gnosis-review-get-due-notes)))
  (if gnosis-modeline-mode
      (progn
        (add-to-list 'global-mode-string '(:eval
					   (format " G:%d" gnosis-due-notes-total)))
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
