;;; gnosis.el --- Spaced Repetition System  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; Version: 0.5.7

;; Package-Requires: ((emacs "27.2") (emacsql "4.1.0") (compat "29.1.4.2") (transient "0.7.2") (org-gnosis "0.0.9"))

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

;; Gnosis is a spaced repetition system-like that enhances memory
;; retention through active recall.  It employs a Q&A format, where each
;; gnosis consists of a question, answer, and explanation.

;; Gnosis algorithm is highly adjustable, allowing users to set specific
;; values not just for note decks but for tags as well.  Gnosis'
;; adjustability allows users to fine-tune settings not only for entire
;; note collections but also for specific tagged topics, thereby creating
;; a personalized learning environment for each topic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'vc-git)
(require 'emacsql-sqlite)
(require 'transient)
(require 'animate)

(require 'org)
(require 'org-element)

(require 'gnosis-algorithm)
(require 'org-gnosis)

(defgroup gnosis nil
  "Spaced Repetition System For Note Taking & Self Testing."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-dir (locate-user-emacs-file "gnosis")
  "Gnosis directory."
  :type 'directory)

(unless (file-directory-p gnosis-dir)
  (make-directory gnosis-dir))

(defcustom gnosis-cloze-string "[...]"
  "Gnosis string to represent a cloze."
  :type 'string)

(defcustom gnosis-string-difference 1
  "Threshold value for string comparison in Gnosis.

This variable determines the maximum acceptable Levenshtein distance
between two strings to consider them as similar."
  :type 'integer)

(defcustom gnosis-vc-auto-push nil
  "Run `vc-push' at the end of every review session."
  :type 'boolean)

(defcustom gnosis-completing-read-function
  (cond ((or (bound-and-true-p ivy-mode)
	     (bound-and-true-p helm-mode)
	     (bound-and-true-p vertico-mode)
	     (bound-and-true-p fido-mode))
	 #'completing-read)
	(t #'ido-completing-read))
  "Function to use for `completing-read'."
  :type 'function)

(defcustom gnosis-new-notes-limit nil
  "Total new notes limit."
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Number")))

(defcustom gnosis-review-new-first t
  "Review new notes first.

When nil, review new notes last."
  :type 'bolean)

(defcustom gnosis-default-average-review-period 30
  "The number of days used to calculate the average reviews on gnosis dashboard."
  :type 'integer)

;;; Faces

(defface gnosis-face-separator
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face for section separator.")

(defface gnosis-face-directions
  '((t :inherit underline))
  "Face for gnosis directions.")

(defface gnosis-face-correct
  '((t :inherit match))
  "Face for user choice.")

(defface gnosis-face-cloze
  '((t :inherit (highlight italic)))
  "Face for clozes.")

(defface gnosis-face-false
  '((t :inherit error))
  "Face for user choice.")

(defface gnosis-face-unanswered
  '((t :inherit (italic underline)))
  "Face for unanswered clozes.")

(defface gnosis-face-hint
  '((t :inherit warning))
  "Face for user choice.")

(defface gnosis-face-cloze-unanswered
  '((t :inherit underline))
  "Face for user choice.")

(defface gnosis-face-next-review
  '((t :inherit bold))
  "Face for next review.")

(defface gnosis-face-dashboard-header
  '((t :inherit (bold font-lock-constant-face)))
  "Face for dashboard header.

Avoid using an increased height value as this messes up with
`gnosis-center-string' implementation")

(defconst gnosis-db
  (emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))
  "Gnosis database.")

(defvar gnosis-testing nil
  "Change this to non-nil when running manual tests.")

(defconst gnosis-db-version 3
  "Gnosis database version.")

(defvar gnosis-note-types
  '(("Basic" . gnosis-add-note--basic)
    ("MCQ" .  gnosis-add-note--mcq)
    ("Double" .  gnosis-add-note--double)
    ("Cloze" . gnosis-add-note--cloze)
    ("MC-cloze" . gnosis-add-note--mc-cloze))
  "Mapping of Notes & their respective functions.")

(defvar gnosis-previous-note-tags '()
  "Tags input from previously added note.")

(defvar gnosis-previous-note-hint nil
  "Hint input from previously added note.")

(defvar gnosis-due-notes-total nil
  "Total due notes.")

(defvar gnosis-review-notes nil
  "Review notes.")

(defvar gnosis-review-buffer-name "*gnosis*"
  "Review buffer name.")

(defvar gnosis-export-separator "\n- ")

;; TODO: Make this as a defcustom.
(defvar gnosis-custom-values
  '((:deck "demo" (:proto (0 1 3) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			  :amnesia 0.5 :lethe 3))
    (:tag "demo" (:proto (1 2) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			 :amnesia 0.45 :lethe 3)))
  "Custom review values for adjusting gnosis algorithm.")

(defvar gnosis-custom--valid-values
  '(:proto :anagnosis :epignosis :agnoia :amnesia :lethe))

(defvar gnosis-review-editing-p nil
  "Boolean value to check if user is currently in a review edit.")


(defun gnosis-select (value table &optional restrictions flatten)
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (let* ((restrictions (or restrictions '(= 1 1)))
	 (flatten (or flatten nil))
	 (output (emacsql gnosis-db `[:select ,value :from ,table :where ,restrictions])))
    (if flatten (apply #'append output) output)))

(defun gnosis-table-exists-p (table)
  "Check if TABLE exists."
  (let ((tables (mapcar (lambda (str) (replace-regexp-in-string "_" "-" (symbol-name str)))
			(cdr (gnosis-select 'name 'sqlite-master '(= type table) t)))))
    (member (symbol-name table) tables)))

(defun gnosis--create-table (table &optional values)
  "Create TABLE for VALUES."
  (unless (gnosis-table-exists-p table)
    (emacsql gnosis-db `[:create-table ,table ,values])))

(defun gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (emacsql gnosis-db `[:drop-table ,table]))

(defun gnosis-drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (when (gnosis-table-exists-p table)
    (gnosis--drop-table table)))

(defun gnosis--insert-into (table values)
  "Insert VALUES to TABLE."
  (emacsql gnosis-db `[:insert :into ,table :values ,values]))

(defun gnosis-update (table value where)
  "Update records in TABLE with to new VALUE based on the given WHERE condition.

Example:
 (gnosis-update ='notes ='(= keimenon \"NEW VALUE\") ='(= id 12))"
  (emacsql gnosis-db `[:update ,table :set ,value :where ,where]))

(defun gnosis-get (value table &optional restrictions)
  "Return caar of VALUE from TABLE, optionally with where RESTRICTIONS."
  (caar (gnosis-select value table restrictions)))

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

(defun gnosis-calculate-average-daily-reviews (&optional days)
  "Calculate average reviews over the last DAYS days."
  (let* ((days (or days gnosis-default-average-review-period))
	 (review-counts '())
         (collect-reviews
          (lambda (d)
            (let ((day-reviews (gnosis-select 'reviewed-total 'activity-log
                                             `(and (> reviewed-total 0)
                                                   (= date ',(gnosis-algorithm-date
							      (- d))))
                                             t)))
              (setq review-counts (append review-counts day-reviews))))))
    ;; Collect reviews for each day
    (dotimes (d days)
      (funcall collect-reviews d))
    ;; Return average, avoiding division by zero
    (if (> (length review-counts) 0)
        (/ (apply '+ review-counts) (float (length review-counts)))
      0)))

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
    (funcall gnosis-completing-read-function prompt
	     (gnosis-shuffle (copy-sequence seq)))))

(defun gnosis-insert-separator ()
  "Insert a dashed line spanning the entire width of the buffer."
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

Acts only when CENTER? is non-nil."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (text (string-trim (buffer-substring start end)))
         (padding (max (/ (- (window-width) (length text)) 2) 0))
	 (center? (or center? t)))
    (if center?
	(progn (delete-region start end)
	       (insert (make-string padding ? ) text))
      (insert text))))

(defun gnosis-center-string (str)
  "Center each line of STR in current window width.
Replaces links `[[source][description]]' with `description'."
  (let* ((width (window-width))
         (lines (split-string str "\n")))
    (mapconcat
     (lambda (line)
       (if (string-blank-p line)
           ""  ;; Preserve blank lines
         (let* ((trimmed (string-trim line))
                ;; Replace links with just the description part
                (processed (replace-regexp-in-string
			    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
			    "\\2"
			    trimmed))
                ;; Fill the text to wrap it properly
                (wrapped (with-temp-buffer
                           (insert processed)
                           (fill-region (point-min) (point-max))
                           (buffer-string)))
                ;; Process each wrapped line with proper centering
                (wrapped-lines (split-string wrapped "\n")))
           (mapconcat
	    (lambda (wline)
	      (let ((padding (max 0 (/ (- width (string-width wline)) 2))))
                (concat (make-string padding ?\s) wline)))
	    wrapped-lines
	    "\n"))))
     lines
     "\n")))

(defun gnosis-apply-center-buffer-overlay (&optional point)
  "Center text in buffer starting at POINT using `gnosis-center-current-line'.
This will not be applied to sentences that start with double space."
  (save-excursion
    (goto-char (or point (point-min)))
    (while (not (or (= (point-max) (point)) (looking-at "^  ")))
      (gnosis-center-current-line)
      (forward-line 1))))

(defun gnosis-org-format-string (str)
  "Return STR fontified as in `org-mode'."
  (with-temp-buffer
    (org-mode)
    (insert str)
    (font-lock-ensure)
    (buffer-string)))

(defun gnosis-display-keimenon (str)
  "Display STR as keimenon."
  (with-current-buffer gnosis-review-buffer-name
    (erase-buffer)
    (insert "\n" (gnosis-center-string str))
    (gnosis-insert-separator)
    (gnosis-apply-center-buffer-overlay)))

(defun gnosis-display-image (keimenon)
  "Dipslay image link from KEIMENON in new window."
  (let ((image-path (and (string-match "\\[file:\\(.*?\\)\\]" keimenon)
			 (match-string 1 keimenon))))
    (when image-path
      (find-file-other-window image-path)
      (switch-to-buffer-other-window gnosis-review-buffer-name))))

(defun gnosis-trim-quotes (str)
  "Remove prefix and suffxi quotes for STR."
  (string-remove-prefix "\"" (string-remove-suffix "\"" str)))

(defun gnosis-cloze-create (str clozes &optional cloze-string)
  "Replace CLOZES in STR with CLOZE-STRING, preserving whitespace pattern."
  (cl-assert (listp clozes) nil "Adding clozes: Clozes need to be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert (gnosis-center-string (gnosis-org-format-string str)))
      (dolist (cloze clozes)
        (let* ((cloze-text (gnosis-trim-quotes cloze))
               (replacement (concat
                             (and (string-match "^\\s-+" cloze-text)
				  (match-string 0 cloze-text))
                             (propertize cloze-string 'face 'gnosis-face-cloze)
                             (and (string-match "\\s-+$" cloze-text)
				  (match-string 0 cloze-text)))))
          (goto-char (point-min))
          (when (search-forward cloze-text nil t)
            (replace-match replacement t t))))
      (buffer-string))))

(defun gnosis-cloze-add-hints (str hints &optional cloze-string)
  "Replace CLOZE-STRING in STR with HINTS, skipping empty hints."
  (cl-assert (listp hints) nil "Hints must be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (cl-loop for hint in hints
               while (search-forward cloze-string nil t)
               do
	       (when (and hint (not (string-empty-p hint)) (not (string= hint "nil"))
			  (not (string= "\"\"" hint))
			  (search-backward cloze-string nil t))
                 (replace-match (propertize (format "[%s]" hint)
					    'face 'gnosis-face-cloze))
                 (goto-char (match-end 0)))) ; Move point to end of match
      (buffer-string))))

(defun gnosis-cloze-mark-answers (str answers face)
  "Mark ANSWERS in STR with FACE.

Replaces first occurence of answer in STR with FACE."
  (cl-assert (listp answers) nil "Answers to mark must be a list.")
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (dolist (answer answers)
      (let ((answer-text (gnosis-trim-quotes answer)))
        (when (search-forward answer-text nil t)
          (replace-match
           (mapconcat
            (lambda (char)
              (if (not (memq char '(?\s ?\t ?\n)))
                  (propertize (char-to-string char) 'face face)
                (char-to-string char)))
            answer-text
            "")
           nil t))))
    (buffer-string)))

(defun gnosis-cloze-mark-false (str answers)
  "Mark contents of STR as false for ANSWERS.

First item of answers will be marked as false, while the rest unanswered."
  (let* ((false (car answers))
	 (unanswered (cdr answers))
         (str-with-false (and answers
			      (gnosis-cloze-mark-answers str (list false)
							 'gnosis-face-false)))
	 final)
    (if unanswered
	(setq final (gnosis-cloze-mark-answers str-with-false
					       (if (listp unanswered) unanswered
						 (list unanswered))
					       'gnosis-face-unanswered))
      (setq final (or str-with-false str)))
    final))

(defun gnosis-display-cloze-string (str clozes hints correct false)
  "Display STR with CLOZES and HINTS.

Applies highlighting for CORRECT & FALSE."
  (let* ((cloze-str (gnosis-cloze-create str clozes))
	 (str-with-hints (gnosis-cloze-add-hints cloze-str hints))
	 (str-with-c-answers
	  (gnosis-cloze-mark-answers str-with-hints correct 'gnosis-face-correct))
	 (final (gnosis-cloze-mark-false str-with-c-answers false)))
    (gnosis-display-keimenon final)))

(defun gnosis-display-basic-answer (answer success user-input)
  "Display ANSWER.

When SUCCESS nil, display USER-INPUT as well"
  (with-current-buffer gnosis-review-buffer-name
      (goto-char (point-max))
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
    (gnosis-center-current-line))))

(defun gnosis-display-hint (hint)
  "Display HINT."
  (let ((hint (or hint "")))
    (unless (string-empty-p hint)
      (goto-char (point-max))
      (and (not (string-empty-p hint))
	   (insert (gnosis-center-string (propertize hint 'face 'gnosis-face-hint))))
      (gnosis-insert-separator))))

(defun gnosis-display-cloze-user-answer (user-input &optional false)
  "Display USER-INPUT answer for cloze note upon failed review.

If FALSE t, use gnosis-face-false face"
  (goto-char (point-max))
  (insert "\n\n"
	  (propertize "Your answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize user-input 'face
		      (if false 'gnosis-face-false 'gnosis-face-correct)))
  (gnosis-center-current-line)
  (newline))

(defun gnosis-display-correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ note."
  (goto-char (point-max))
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

(defun gnosis-display-parathema (parathema)
  "Display PARATHEMA."
  (when (and parathema (not (string-empty-p parathema)))
    (search-backward "----") ; search back for separator
    (forward-line 1)
    (insert "\n" (gnosis-center-string (gnosis-org-format-string parathema)) "\n")))

(defun gnosis-display-next-review (id success)
  "Display next interval of note ID for SUCCESS."
  (with-current-buffer gnosis-review-buffer-name
    (let* ((interval (car (gnosis-review-algorithm id success)))
	   (next-review-msg (format "\n\n%s %s"
				    (propertize "Next review:" 'face 'gnosis-face-directions)
				    (propertize
				     (replace-regexp-in-string
				      "[]()[:space:]]"
				      (lambda (match)
					(if (string= match " ") "/" ""))
				      (format "%s" interval) t t)
				     'face 'gnosis-face-next-review))))
      (if (search-backward "Next review" nil t)
	  ;; Delete previous result, and override with new this should
	  ;; occur only when used for overriding review result.
          (progn (delete-region (point) (progn (end-of-line) (point)))
		 (insert (propertize (replace-regexp-in-string "\n" "" next-review-msg)
				     'face (if success 'gnosis-face-correct
					     'gnosis-face-false))))
	;; Default behaviour
	(goto-char (point-max))
	(insert (gnosis-center-string next-review-msg))))))

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
  (when (and (equal (gnosis-select 'name 'decks) nil)
	     (y-or-n-p "No decks found, create deck?"))
    (gnosis-add-deck (read-string "Deck name: ")))
  (if id
      (gnosis-get 'name 'decks `(= id ,id))
    (funcall gnosis-completing-read-function "Deck: " (gnosis-select 'name 'decks))))

(cl-defun gnosis--get-deck-id (&optional (deck (gnosis--get-deck-name)))
  "Return id for DECK name."
  (gnosis-get 'id 'decks `(= name ,deck)))

(defun gnosis-get-deck-id (&optional deck)
  "Return note id for DECK.

If DECK does not exist, create it."
  (cl-assert (stringp deck) nil "DECK must be a string.")
  (let* ((deck (or deck (gnosis--get-deck-name)))
	 (deck-id (gnosis-select 'id 'decks `(= name ,deck) t)))
    (if deck-id (car deck-id)
      (gnosis-add-deck deck)
      (gnosis-get-deck-id deck))))

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

(cl-defun gnosis-toggle-suspend-notes (ids &optional verification)
  "Toggle Suspend value for note with ID.

When VERIFICATION is non-nil, skips `y-or-n-p' prompt."
  (cl-assert (listp ids) nil "IDS value needs to be a list.")
  (let* ((items-num (length ids))
         (suspended (and (= items-num 1)
                         (= (gnosis-get 'suspend 'review-log `(= id ,(car ids))) 1)))
         (verification
          (or verification
              (cond ((= items-num 1)
                     (y-or-n-p
                      (if suspended "Unsuspend note? " "Suspend note? ")))
                    (t (y-or-n-p
                        (format "Toggle suspend value for %s items? " items-num)))))))
    (when verification
      (emacsql gnosis-db
               [:update review-log
                :set (= suspend (- 1 suspend))
                :where (in id $v1)]
               (vconcat ids)))))

(cl-defun gnosis-suspend-deck (&optional (deck (gnosis--get-deck-id)))
  "Suspend all note(s) with DECK id.

When called with a prefix, unsuspends all notes in deck."
  (let* ((notes (gnosis-select 'id 'notes `(= deck-id ,deck) t))
	 (suspend (if current-prefix-arg 0 1))
	 (confirm
	  (y-or-n-p
	   (if (= suspend 0)
	       "Unsuspend all notes for deck? " "Suspend all notes for deck? "))))
    (when confirm
      (emacsql gnosis-db `[:update review-log :set (= suspend ,suspend) :where
				   (in id ,(vconcat notes))])
      (if (equal suspend 0)
	  (message "Unsuspended %s notes" (length notes))
	(message "Suspended %s notes" (length notes))))))

(defun gnosis-generate-id (&optional length deck-p)
  "Generate a unique gnosis ID.

Default to generating a note id, when DECK-P is t generates a deck id.

LENGTH: length of id, default to a random number between 10-15."
  (let* ((length (or length (+ (random 5) 10)))
         (max-val (expt 10 length))
         (min-val (expt 10 (1- length)))
         (id (+ (random (- max-val min-val)) min-val))
	 (current-ids (if deck-p (gnosis-select 'id 'decks nil t)
			(gnosis-select 'id 'notes nil t))))
    (if (member id current-ids)
        (gnosis-generate-id length)
      id)))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'hypothesis 'notes `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (gnosis-completing-read "Answer: " choices)))

(defun gnosis-cloze-check (sentence clozes)
  "Return t if all CLOZES are found in SENTENCE."
  (cl-every (lambda (cloze)
              (string-match-p
               (regexp-quote
	        (gnosis-trim-quotes cloze))
               sentence))
            clozes))
;; TODO: use a better name to indicate that it also removes hints from STRING.
(defun gnosis-cloze-remove-tags (string)
  "Replace cloze tags and hints in STRING.

Works with both single (:), double colons (::), single braces ({}) and
double braces ({{}}).

Also removes content after a double semicolon (::), which indicate a hint."
  (let* ((regex "{\\{1,2\\}c[0-9]+:\\{1,2\\}\\([^:{}]*?\\)\\(::[^{}]*\\)?}\\{1,2\\}")
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

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (replace-regexp-in-string "::\\(.*\\)" "" str))
                    lst))
          nested-lst))

(defun gnosis-cloze-extract-hints (nested-lst)
  "Extract cloze hints for string clozes inside the NESTED-LST.

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (when (string-match "::\\(.*\\)" str)
                        (match-string 1 str)))
                    lst))
          nested-lst))

(defun gnosis-compare-strings (str1 str2)
  "Compare STR1 and STR2, ignoring case and whitespace."
  (let* ((normalized-str1 (downcase
			   (replace-regexp-in-string "\\s-" ""
						     (gnosis-trim-quotes str1))))
         (normalized-str2 (downcase
			   (replace-regexp-in-string "\\s-" ""
						     (gnosis-trim-quotes str2))))
         (max-length (max (length normalized-str1) (length normalized-str2))))
    (if (> max-length gnosis-string-difference)
        (<= (string-distance normalized-str1 normalized-str2) gnosis-string-difference)
      (string= normalized-str1 normalized-str2))))

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (cl-loop for tags in (apply 'append
			      (emacsql gnosis-db [:select :distinct tags :from notes]))
           nconc tags into all-tags
           finally return (delete-dups all-tags)))

(defun gnosis-collect-tag-note-ids (tags &optional ids)
  "Collect note IDS for TAGS."
  (cl-assert (listp tags))
  (if (null tags) ids
    (gnosis-collect-tag-note-ids (cdr tags)
                                 (append ids (gnosis-get-tag-notes (car tags))))))

(defun gnosis-select-by-tag (input-tags &optional due suspended-p)
  "Return note ID's for every note with INPUT-TAGS.

If DUE, return only due notes.
If SUSPENDED-P, return suspended notes as well."
  (cl-assert (listp input-tags) t "Input tags must be a list")
  (cl-assert (booleanp due) "Due value must be a boolean")
  (let ((ids (gnosis-collect-tag-note-ids input-tags)))
    ;; Filter the collected IDs based on due and suspension status
    (cl-loop for id in ids
             when (and (or (not suspended-p) (not (gnosis-suspended-p id)))
                       (if due (gnosis-review-is-due-p id) t))
             collect id)))

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
  (let ((notes (gnosis-select 'id 'notes `(= deck-id ,(or deck-id (gnosis--get-deck-id)))
			      t)))
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

(defun gnosis-tags--update (tags)
  "Update db for TAGS."
  (emacsql-with-transaction gnosis-db
    (cl-loop for tag in tags
	     do (gnosis--insert-into 'tags `[,tag]))))

(cl-defun gnosis-tags--prompt (&key (prompt "Tags (seperated by ,): ")
				    (predicate nil)
				    (require-match nil)
				    (initial-input nil))
  "Prompt to select tags with PROMPT."
  (gnosis-tags-refresh)
  (let* ((tags (gnosis-get-tags--unique))
	 (input (delete-dups
		 (completing-read-multiple
		  prompt tags predicate require-match initial-input))))
    input))

(defun gnosis-tags-prompt ()
  "Tag prompt for adding notes.

If you only require a tag prompt, refer to `gnosis-tags--prompt'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This function is meant to be used in an org-mode buffer"))
  (save-excursion
    (let ((input (gnosis-tags--prompt))
	  (current-tags (org-get-tags)))
      (outline-up-heading 99)
      (when input
	(gnosis-tags--update input)
	(setf gnosis-previous-note-tags input)
        (org-set-tags (append input current-tags))))))

(defun gnosis-tags-refresh ()
  "Refresh tags value."
  (let ((tags (gnosis-get-tags--unique)))
    ;; Delete all values from tags table.
    (gnosis--delete 'tags nil)
    ;; Insert all unique tags from notes.
    (emacsql-with-transaction gnosis-db
      (cl-loop for tag in tags
	       do (gnosis--insert-into 'tags `[,tag])))))

(defun gnosis-tag-rename (tag &optional new-tag)
  "Rename TAG to NEW-TAG.

Replace dashes (-) to underscores (_) for NEW-TAG, as org currently
does not accept heading tags with dashes."
  (let ((new-tag (or new-tag
		     (replace-regexp-in-string
		      "-" "_" (read-string "New tag name: ")))))
    (cl-loop for note in (gnosis-get-tag-notes tag)
	     do (let* ((tags (car (gnosis-select '[tags] 'notes `(= id ,note) t)))
		       (new-tags (cl-substitute new-tag tag tags :test #'string-equal)))
		  (gnosis-update 'notes `(= tags ',new-tags) `(= id ,note))))
    ;; Update tags in database
    (gnosis-tags-refresh)
    (message "Renamed tag '%s' to '%s'" tag new-tag)))

;; Links
(defun gnosis-extract-id-links (input &optional start)
  "Extract all link IDs from INPUT string and return them as a list.

START is the search starting position, used internally for recursion."
  (let ((start (or start 0)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" input start)
        (cons (match-string 1 input)
              (gnosis-extract-id-links input (match-end 0)))
      nil)))

;; TODO: Rewrite this! Tags should be an input of strings,
;; interactive handling should be done by "helper" funcs
(cl-defun gnosis-collect-note-ids (&key (tags nil) (due nil) (deck nil) (query nil))
  "Return list of note ids based on TAGS, DUE, DECKS, QUERY.

TAGS: boolean value, t to specify tags.
DUE: boolean value, t to specify due notes.
DECK: Integer, specify deck id.
QUERY: String value."
  (cl-assert (and (booleanp due) (booleanp tags)
		  (or (numberp deck) (null deck))
		  (or (stringp query) (null query)))
	     nil "Incorrect value passed to `gnosis-collect-note-ids'")
  (cond ((and (null tags) (null due) (null deck) (null query))
	 (gnosis-select 'id 'notes nil t))
	;; All due notes
	((and (null tags) due (null deck))
	 (gnosis-review-get-due-notes))
	;; All notes for tags
	((and tags (null due) (null deck))
	 (gnosis-select-by-tag (gnosis-tags--prompt :require-match t)))
	;; All due notes for tags
	((and tags due (null deck))
	 (gnosis-select-by-tag (gnosis-tags--prompt :require-match t) t))
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

(defun gnosis-review-get--due-notes ()
  "Return due note IDs & due dates."
  (let* ((old-notes (cl-loop for note in
			     (gnosis-select '[id next-rev] 'review-log
					    '(and (> n 0)
						  (= suspend 0))
					    nil)
			     when (gnosis-past-or-present-p (cadr note))
			     collect note))
	 (new-notes (cl-loop for note in
			     (gnosis-select '[id next-rev] 'review-log
					    '(and (= n 0)
						  (= suspend 0))
					    nil)
			     when (gnosis-past-or-present-p (cadr note))
			     collect note)))
    (if gnosis-review-new-first
	(append (cl-subseq new-notes 0 gnosis-new-notes-limit) old-notes)
      (append old-notes (cl-subseq new-notes 0 gnosis-new-notes-limit)))))

(defun gnosis-review-get-due-notes ()
  "Return all due note IDs."
  (mapcar #'car (gnosis-review-get--due-notes)))

(defun gnosis-review-get-overdue-notes (&optional note-ids)
  "Return overdue notes for current DATE.

Optionally, provide NOTE-IDS of which the overdue ones will be returned."
  (cl-loop for note in (or note-ids (gnosis-review-get--due-notes))
	   when (not (equal (cadr note) (gnosis-algorithm-date)))
	   collect (car note)))

(defun gnosis-review-get-due-notes--no-overdue (&optional note-ids)
  "Return due notes, without overdue.

Optionally, provide a list for due NOTE-IDS."
  (let ((note-ids (or note-ids (length (gnosis-review-get-due-notes)))))
    (cl-set-difference note-ids (gnosis-review-get-overdue-notes note-ids))))

(defun gnosis-review-last-interval (id)
  "Return last review interval for note ID."
  (let* ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id)))
	 (rev-date (gnosis-get 'next-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev rev-date)))

(defun gnosis-review-algorithm (id success)
  "Return next review date & gnosis for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure.

Returns a list of the form ((yyyy mm dd) (ef-increase ef-decrease ef-total))."
  (let ((amnesia (gnosis-get-note-amnesia id))
	(gnosis (gnosis-get 'gnosis 'review `(= id ,id)))
	(t-success (gnosis-get 't-success 'review-log `(= id ,id))) ;; total successful reviews
	(c-success (gnosis-get 'c-success 'review-log `(= id ,id))) ;; consecutive successful reviews
	(c-fails (gnosis-get 'c-fails 'review-log `(= id ,id))) ;; consecutive failed reviews
	;; (t-fails (gnosis-get 't-fails 'review-log `(= id ,id))) ;; total failed reviews
	;; (review-num (gnosis-get 'n 'review-log `(= id ,id))) ;; total reviews
	;; (last-interval (max (gnosis-review--get-offset id) 1))
	(last-interval (gnosis-review-last-interval id))) ;; last interval
    (list
     (gnosis-algorithm-next-interval
      :last-interval last-interval
      :gnosis-synolon (nth 2 gnosis)
      :success success
      :successful-reviews t-success
      :c-fails c-fails
      :lethe (gnosis-get-note-lethe id)
      :amnesia amnesia
      :proto (gnosis-get-note-proto id))
     (gnosis-algorithm-next-gnosis
      :gnosis gnosis
      :success success
      :epignosis (gnosis-get-note-epignosis id)
      :agnoia (gnosis-get-note-agnoia id)
      :anagnosis (gnosis-get-note-anagnosis id)
      :c-successes c-success
      :c-failures c-fails))))

(defun gnosis-review--update (id success)
  "Update review-log for note with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure."
  (let ((gnosis (cadr (gnosis-review-algorithm id success)))
	(next-rev (car (gnosis-review-algorithm id success))))
    ;; Update activity-log
    (gnosis-review-increment-activity-log (gnosis-review-is-note-new-p id))
    ;; Update review-log
    (gnosis-update 'review-log `(= last-rev ',(gnosis-algorithm-date)) `(= id ,id))
    (gnosis-update 'review-log `(= next-rev ',next-rev) `(= id ,id))
    (gnosis-update 'review-log `(= n (+ 1 ,(gnosis-get 'n 'review-log `(= id ,id)))) `(= id ,id))
    ;; Update review
    (gnosis-update 'review `(= gnosis ',gnosis) `(= id ,id))
    (if success
	(progn (gnosis-update 'review-log
			      `(= c-success ,(1+ (gnosis-get 'c-success 'review-log `(= id ,id))))
			      `(= id ,id))
	       (gnosis-update 'review-log
			      `(= t-success ,(1+ (gnosis-get 't-success 'review-log `(= id ,id))))
			      `(= id ,id))
	       (gnosis-update 'review-log `(= c-fails 0) `(= id ,id)))
      (gnosis-update 'review-log
		     `(= c-fails ,(1+ (gnosis-get 'c-fails 'review-log `(= id ,id)))) `(= id ,id))
      (gnosis-update 'review-log
		     `(= t-fails ,(1+ (gnosis-get 't-fails 'review-log `(= id ,id)))) `(= id ,id))
      (gnosis-update 'review-log `(= c-success 0) `(= id ,id)))))

(defun gnosis-review-result (id success)
  "Update review note ID results for SUCCESS."
  (gnosis-review--update id success)
  (setf gnosis-due-notes-total (length (gnosis-review-get-due-notes))))

(defun gnosis-review-mcq (id)
  "Review MCQ note with ID."
  (gnosis-display-image (gnosis-get 'keimenon 'notes `(= id ,id)))
  (gnosis-display-keimenon (gnosis-org-format-string
			    (gnosis-get 'keimenon 'notes `(= id ,id))))
  (let* ((answer (car (gnosis-get 'answer 'notes `(= id ,id))))
	 (user-choice (gnosis-mcq-answer id))
	 (success (string= answer user-choice)))
    (gnosis-display-correct-answer-mcq answer user-choice)
    (gnosis-display-parathema (gnosis-get 'parathema 'extras `(= id ,id)))
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-basic (id)
  "Review basic type note for ID."
  (let* ((hypothesis (car (gnosis-get 'hypothesis 'notes `(= id ,id))))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
	 (keimenon (gnosis-get 'keimenon 'notes `(= id ,id)))
	 (answer (car (gnosis-get 'answer 'notes `(= id ,id)))))
    (gnosis-display-image keimenon)
    (gnosis-display-keimenon (gnosis-org-format-string keimenon))
    (gnosis-display-hint hypothesis)
    (let* ((answer answer)
	   (user-input (read-string "Answer: "))
	   (success (gnosis-compare-strings answer user-input)))
      (gnosis-display-basic-answer answer success user-input)
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review id success)
      success)))

(defun gnosis-review-cloze--input (clozes &optional user-input)
  "Prompt for USER-INPUT during cloze review.

CLOZES is a list of possible correct answers.

Returns a cons; ='(position . user-input) if correct,
='(nil . user-input) if incorrect."
  (let* ((user-input (or user-input (read-string "Answer: ")))
         (position (cl-position user-input clozes :test #'gnosis-compare-strings)))
    (cons position user-input)))

(defun gnosis-review-cloze (id)
  "Review cloze type note for ID."
  (let* ((keimenon (gnosis-get 'keimenon 'notes `(= id ,id)))
         (all-clozes (gnosis-get 'answer 'notes `(= id ,id)))
         (all-hints (gnosis-get 'hypothesis 'notes `(= id ,id)))
         (revealed-clozes '()) ;; List of revealed clozes
         (unrevealed-clozes all-clozes)
         (unrevealed-hints all-hints)
         (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
         (success t))
    ;; Initially display the sentence with no reveals
    (gnosis-display-cloze-string keimenon unrevealed-clozes unrevealed-hints nil nil)
    (catch 'done
      (while unrevealed-clozes
        (let* ((input (gnosis-review-cloze--input unrevealed-clozes))
               (position (car input))
               (matched-cloze (when position (nth position unrevealed-clozes)))
               (matched-hint (when (and position (< position (length unrevealed-hints)))
                               (nth position unrevealed-hints))))
          (if matched-cloze
              ;; Correct answer - move cloze from unrevealed to revealed
              (progn
                ;; Add to revealed clozes list, preserving original order
                (setq revealed-clozes
                      (cl-sort (cons matched-cloze revealed-clozes)
                               #'< :key (lambda (cloze)
                                          (cl-position cloze all-clozes))))
                ;; Remove from unrevealed lists by position
                (setq unrevealed-clozes (append (cl-subseq unrevealed-clozes 0 position)
                                               (cl-subseq unrevealed-clozes (1+ position))))
                (when (and matched-hint (< position (length unrevealed-hints)))
		  (setq unrevealed-hints (append (cl-subseq unrevealed-hints 0 position)
                                                (cl-subseq unrevealed-hints (1+ position)))))
                ;; Display with updated revealed/unrevealed lists
                (gnosis-display-cloze-string keimenon unrevealed-clozes unrevealed-hints
                                           revealed-clozes nil))
            ;; Incorrect answer
            (gnosis-display-cloze-string keimenon nil nil
                                       revealed-clozes unrevealed-clozes)
            (gnosis-display-cloze-user-answer (cdr input))
            (setq success nil)
            (throw 'done nil)))))
    (gnosis-display-parathema parathema)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-mc-cloze (id)
  "Review mc-cloze type note for ID."
  (let* ((keimenon (gnosis-get 'keimenon 'notes `(= id ,id)))
	 (cloze (gnosis-get 'answer 'notes `(= id ,id)))
	 (options (gnosis-get 'hypothesis 'notes `(= id ,id)))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
	 (user-input)
	 (success))
    (gnosis-display-cloze-string keimenon cloze nil nil nil)
    (setq user-input (gnosis-completing-read "Select answer: "
					     (gnosis-shuffle options)))
    (if (string= user-input (car cloze))
	(progn
	  (gnosis-display-cloze-string keimenon nil nil cloze nil)
	  (setq success t))
      (gnosis-display-cloze-string keimenon nil nil nil cloze)
      (gnosis-display-correct-answer-mcq (car cloze) user-input))
    (gnosis-display-parathema parathema)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-is-note-new-p (id)
  "Return t if note with ID is new."
  (let ((reviews (car (gnosis-select 'n 'review-log `(= id ,id) t))))
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

(defun gnosis-history-clear ()
  "Delete all activity log entries."
  (interactive)
  (when (y-or-n-p "Delete all activity log?")
    (emacsql gnosis-db [:delete :from activity-log])))

(defun gnosis-review--display-note (id)
  "Display note with ID and call the appropriate review func."
  (let* ((type (gnosis-get 'type 'notes `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s" (downcase type)))))
    (if (fboundp func-name)
        (progn
	  (unless (eq major-mode 'gnosis-mode)
	    (pop-to-buffer-same-window (get-buffer-create gnosis-review-buffer-name))
            (gnosis-mode)
	    (gnosis-review-update-header 0))
	  (window-configuration-to-register :gnosis-pre-image)
          (funcall func-name id))
      (error "Malformed note type: '%s'" type))))

(defun gnosis-review-process-note (note &optional note-count)
  "Process review for NOTE and update session statistics.

Displays the note, processes the review result, and updates the
header.  Returns the incremented NOTE-COUNT after processing.

This is a helper function for `gnosis-review-session'."
  (let ((success (gnosis-review--display-note note))
	(note-count (or note-count 0)))
    (cl-incf note-count)
    (gnosis-review-actions success note note-count)
    (jump-to-register :gnosis-pre-image)
    (gnosis-review-update-header note-count)
    note-count))

(defun gnosis-review-update-header (reviewed-count &optional remaining-reviews)
  "Update the review session header with current stats.

REVIEWED-COUNT: Total number of items that have been reviewed in
current session.
REMAINING-REVIEWS: Total number of remaining items to be reviewed."
  (with-current-buffer (get-buffer-create gnosis-review-buffer-name)
    (let ((remaining-reviews (or remaining-reviews gnosis-due-notes-total)))
      (setq-local header-line-format
                  (gnosis-center-string
		   (format "%s %s %s"
                           (propertize (number-to-string reviewed-count)
                                       'face 'font-lock-type-face)
			   (propertize "|" 'face 'font-lock-comment-face)
                           (propertize (number-to-string remaining-reviews)
				       'face 'gnosis-face-false)))))))

(defun gnosis-review-session (notes &optional due note-count)
  "Start review session for NOTES.
NOTES: List of note ids
DUE: If due is non-nil, session will loop for due notes.
NOTE-COUNT: Total notes to be commited for session."
  (let ((note-count (or note-count 0)))
    (if (null notes)
        (message "No notes for review.")
      (setf gnosis-review-notes notes)
      (catch 'review-loop
        (cl-loop for note in notes
                 do (setq note-count (gnosis-review-process-note note note-count))
                 finally
                 (and due (gnosis-review-session
                           (gnosis-collect-note-ids :due t) t note-count))))
      (gnosis-dashboard)
      (gnosis-review-commit note-count))))

(defun gnosis-review-commit (note-num)
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one.  It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number NOTE-NUM."
  (let ((git (executable-find "git"))
	(default-directory gnosis-dir))
    (unless git
      (error "Git not found, please install git"))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-git-create-repo))
    (unless gnosis-testing
      (shell-command
       (format "%s add gnosis.db" git))
      (gnosis--shell-cmd-with-password
       (format "%s commit -m 'Total notes reviewed: %d'" git note-num)))
    (sit-for 0.1)
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))
    (message "Review session finished.  %d notes reviewed." note-num)))

(defun gnosis-review-action--edit (success note note-count)
  "Edit NOTE during review.

Save current contents of *gnosis-edit* buffer, if any, and start
editing NOTE with it's new contents.

After done editing, call `gnosis-review-actions' with SUCCESS NOTE
NOTE-COUNT."
  (gnosis-edit-note note)
  (setf gnosis-review-editing-p t)
  (recursive-edit)
  (gnosis-review-actions success note note-count))

(defun gnosis-review-action--quit (success note)
  "Quit review session.

Update result for NOTE review with SUCCESS and commit session for NOTE-COUNT.

This function should be used with `gnosis-review-actions', to finish
the review session."
  (gnosis-review-result note success)
  ;; Break the review loop of `gnosis-review-session'
  (throw 'review-loop t))

(defun gnosis-review-action--suspend (success note note-count)
  "Suspend/Unsuspend NOTE.

This function should be used with `gnosis-review-actions', which
should be recursively called using SUCCESS, NOTE, NOTE-COUNT."
  (gnosis-toggle-suspend-notes (list note))
  (gnosis-review-actions success note note-count))

(defun gnosis-review-action--override (success note note-count)
  "Override current review result for SUCCESS.

This function should be used with `gnosis-review-actions', which will
be called with new SUCCESS value plus NOTE & NOTE-COUNT."
  (setf success (if success nil t))
  (gnosis-display-next-review note success)
  (gnosis-review-actions success note note-count))

(defun gnosis-review-actions (success id note-count)
  "Specify action during review of note.

SUCCESS: Review result
ID: Note ID
NOTE-COUNT: Total notes reviewed

To customize the keybindings, adjust `gnosis-review-keybindings'."
  (let* ((prompt
	  "Action: %sext gnosis, %sverride result, %suspend note, %sdit note, %suit: ")
	 (choice (read-char-choice
		  (apply #'format prompt
			 (mapcar
			  (lambda (str) (propertize str 'face 'match)) '("n" "o" "s" "e" "q")))
		  '(?n ?o ?s ?e ?q))))
    (pcase choice
      (?n (gnosis-review-result id success))
      (?o (gnosis-review-action--override success id note-count))
      (?s (gnosis-review-action--suspend success id note-count))
      (?e (gnosis-review-action--edit success id note-count))
      (?q (gnosis-review-action--quit success id)))))

;;;###autoload
(defun gnosis-review ()
  "Start gnosis review session."
  (interactive)
  ;; Refresh modeline
  (setq gnosis-due-notes-total (length (gnosis-review-get-due-notes)))
  ;; reset pre-image register
  (set-register :gnosis-pre-image nil)
  ;; Select review type
  (let ((review-type
	 (gnosis-completing-read "Review: "
				 '("Due notes"
				   "Due notes of deck"
				   "Due notes of specified tag(s)"
				   "Overdue notes"
				   "Due notes (Without Overdue)"
				   "All notes of deck"
				   "All notes of tag(s)"))))
    (pcase review-type
      ("Due notes" (gnosis-review-session (gnosis-collect-note-ids :due t) t))
      ("Due notes of deck" (gnosis-review-session
			    (gnosis-collect-note-ids :due t :deck (gnosis--get-deck-id))))
      ("Due notes of specified tag(s)" (gnosis-review-session
					(gnosis-collect-note-ids :due t :tags t)))
      ("Overdue notes" (gnosis-review-session (gnosis-review-get-overdue-notes)))
      ("Due notes (Without Overdue)" (gnosis-review-session
				      (gnosis-review-get-due-notes--no-overdue)))
      ("All notes of deck" (gnosis-review-session
			    (gnosis-collect-note-ids :deck (gnosis--get-deck-id))))
      ("All notes of tag(s)" (gnosis-review-session (gnosis-collect-note-ids :tags t))))))

(defun gnosis-review--select-topic ()
  "Prompt for topic from org-gnosis database and return it's id."
  (let* ((topic-title (gnosis-completing-read "Select topic: " (org-gnosis-select 'title 'nodes)))
	 (topic-id (caar (org-gnosis-select 'id 'nodes `(= title ,topic-title)))))
    topic-id))

;;;###autoload
(defun gnosis-review-topic (&optional node-id)
  "Review gnosis for topic with NODE-ID."
  (interactive)
  (let* ((node-id (or node-id (gnosis-review--select-topic)))
	 (node-title (car (org-gnosis-select 'title 'nodes `(= id ,node-id) t)))
	 (gnosis-questions (gnosis-select 'source 'links `(= dest ,node-id) t)))
    (if (and gnosis-questions
	     (y-or-n-p (format "Review %s thema(s) for '%s'?"
			       (length gnosis-questions)
			       node-title)))
	(gnosis-review-session gnosis-questions)
      (message "No thema found for %s (id:%s)" node-title node-id))))

(defun gnosis-add-note-fields (deck-id type keimenon hypothesis answer
				       parathema tags suspend links
				       &optional review-image gnosis-id)
  "Insert fields for new note.

DECK-ID: Deck ID for new note.
TYPE: Note type e.g \"mcq\"
KEIMENON: Note's keimenon
HYPOTHESIS: Note hypothesis, e.g choices for mcq for OR hints for
cloze/basic note
ANSWER: Correct answer for note, for MCQ is an integer while for
cloze/basic a string/list of the right answer(s)
PARATHEMA: Parathema information to display after the answer
TAGS: Tags to organize notes
SUSPEND: Integer value of 1 or 0, where 1 suspends the card.
LINKS: List of id links."
  (cl-assert (integerp deck-id) nil "Deck ID must be an integer")
  (cl-assert (stringp type) nil "Type must be a string")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string")
  (cl-assert (listp hypothesis) nil "Hypothesis value must be a list")
  (cl-assert (listp answer) nil "Answer value must be a list")
  (cl-assert (stringp parathema) nil "Parathema must be a string")
  (cl-assert (listp tags) nil "Tags must be a list")
  (cl-assert (listp links) nil "Links must be a list")
  (let* ((gnosis-id (or gnosis-id (gnosis-generate-id)))
	 (review-image (or review-image "")))
    (emacsql-with-transaction gnosis-db
      ;; Refer to `gnosis-db-schema-SCHEMA' e.g `gnosis-db-schema-review-log'
      (gnosis--insert-into 'notes `([,gnosis-id ,(downcase type) ,keimenon ,hypothesis
					      ,answer ,tags ,deck-id]))
      (gnosis--insert-into 'review  `([,gnosis-id ,gnosis-algorithm-gnosis-value
						,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,gnosis-id ,(gnosis-algorithm-date)
						   ,(gnosis-algorithm-date) 0 0 0 0
						   ,suspend 0]))
      (gnosis--insert-into 'extras `([,gnosis-id ,parathema ,review-image]))
      (cl-loop for link in links
	       do (gnosis--insert-into 'links `([,gnosis-id ,link]))))))

(defun gnosis-update-note (id keimenon hypothesis answer parathema tags links
			      &optional deck-id type)
  "Update note entry for ID.

If gnosis ID does not exist, create it anew."
  (let ((id (if (stringp id) (string-to-number id) id)))
    (if (member id (gnosis-select 'id 'notes nil t))
	(emacsql-with-transaction gnosis-db
	  (gnosis-update 'notes `(= keimenon ,keimenon) `(= id ,id))
	  (gnosis-update 'notes `(= hypothesis ',hypothesis) `(= id ,id))
	  (gnosis-update 'notes `(= answer ',answer) `(= id ,id))
	  (gnosis-update 'extras `(= parathema ,parathema) `(= id ,id))
	  (gnosis-update 'notes `(= tags ',tags) `(= id ,id))
	  (gnosis--delete 'links `(= source ,id))
	  (cl-loop for link in links
		   do (gnosis--insert-into 'links `([,id ,link]))))
      (message "Gnosis with id: %d does not exist, creating anew." id )
      (gnosis-add-note-fields deck-id type keimenon hypothesis answer parathema tags
			      0 links nil id))))

;;;;;;;;;;;;;;;;;;;;;; NOTE HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These functions provide assertions depending on the type of note.
;;
;; Each note should use a helper function that calls to provide
;; assertions, such as length of hypothesis and answer, for said
;; note.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gnosis-add-note--basic (id deck-id type keimenon hypothesis
				   answer parathema tags suspend links)
  "Default format for adding a note.

DECK-ID: Integer value of deck-id.
TYPE: String representing the type of note.
KEIMENON: String for the note text.
HYPOTHESIS: List of a signle string.
ANSWER: List of a single string.
PARATHEMA: String for the parathema text.
TAGS: List of note tags.
SUSPEND: Integer value of 0 for nil and 1 for true (suspended).
LINKS: List of id links in PARATHEMA."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (stringp type) nil "Type must be a string.")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (or (null hypothesis)
		 (and (listp hypothesis)
		      (= (length hypothesis) 1)))
	     nil "Hypothesis value must be a list of a single item or nil.")
  (cl-assert (and (listp answer)
		  (= (length answer) 1))
	     nil "Answer value must be a list of a signle item")
  (cl-assert (listp tags) nil "Tags must be a list.")
  (cl-assert (or (= suspend 0)
		 (= suspend 1))
	     nil "Suspend value must either 0 or 1")
  (cl-assert (listp links) nil "Links must be a list")
  (if (equal id "NEW")
      (gnosis-add-note-fields deck-id type keimenon (or hypothesis (list ""))
			       answer parathema tags suspend links)
    (gnosis-update-note id keimenon hypothesis answer parathema tags links deck-id type)))

(defun gnosis-add-note--double (id deck-id type keimenon hypothesis
				    answer parathema tags suspend links)
  "Double note format.

Changes TYPE to basic & inserts a second basic note with ANSWER
and KEIMENON reversed."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (stringp type) nil "Type must be a string.")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (listp hypothesis) nil "Hypothesis value must be a list.")
  (cl-assert (and (listp answer) (= (length answer) 1))
	     nil "Answer value must be a list of a signle item")
  (cl-assert (listp tags) nil "Tags must be a list.")
  (cl-assert (or (= suspend 0) (= suspend 1)) nil "Suspend value must either 0 or 1")
  (cl-assert (listp links) nil "Links must be a list")
  ;; Change type to basic
  (let ((type "basic")
	(hypothesis (or hypothesis (list ""))))
    (if (equal id "NEW")
	(progn
	  (gnosis-add-note-fields deck-id type keimenon hypothesis
				   answer parathema tags suspend links)
	  (gnosis-add-note-fields deck-id type (car answer) hypothesis
				   (list keimenon) parathema tags suspend links))
      ;; There should not be a double type note in database to
      ;; update.  This is used for testing purposes.
      (gnosis-update-note id keimenon hypothesis answer parathema tags links deck-id type))))

(defun gnosis-add-note--mcq (id deck-id type keimenon hypothesis
				answer parathema tags suspend links)
  "Helper function for MCQ note type.

Provide assertions for MCQ type notes.

DECK-ID: ID for deck.
ID: Integer for note ID.
TYPE: String for type, must be \"mcq\".
HYPOTHESIS: List of strings or nil, hypothesis in MCQ note types
serve as choices to select from.
ANSWER: List of one time, the right answer.  Must be member of
HYPOTHESIS.
TAGS: List of tags.
PARATHEMA: Parathesis for THEMA.
SUSPEND: integer value, 1 or 0.
LINKS: list of strings."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (string= type "mcq") nil "TYPE value must be \"mcq\".")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (and (listp hypothesis)
		  (> (length hypothesis) 1))
	     nil "Hypothesis value must be a list greater than 1 item.")
  (cl-assert (and (listp answer)
		  (= (length answer) 1)
		  (member (car answer) hypothesis))
	     nil "Answer value must be a single item, member of the Hypothesis")
  (cl-assert (and (listp tags)
		  (cl-every 'stringp tags))
	     nil "Tags must be a list.")
  (cl-assert (or (= suspend 0)
		 (= suspend 1))
	     nil "Suspend value must either 0 or 1")
  (cl-assert (and (listp links)
		  (cl-every 'stringp links))
	     nil "Links must be a list")
  (if (equal id "NEW")
      (gnosis-add-note-fields deck-id type keimenon (or hypothesis (list ""))
			      answer parathema tags suspend links)
    (gnosis-update-note id keimenon hypothesis answer parathema tags links deck-id type)))

(defun gnosis-add-note--cloze (id deck-id type keimenon hypothesis
				  answer parathema tags suspend links)
  "Helper for cloze type notes.

Provide assertions for cloze type notes.

DECK-ID: ID for deck.
ID: Integer for note ID.
TYPE: String for type, must be \"cloze\".
HYPOTHESIS: List of strings or nil, hypothesis in cloze note types
serve as hints.
ANSWER: List of answers for clozes.
TAGS: List of tags.
PARATHEMA: Parathesis for thema.
SUSPEND: integer value, 1 or 0.
LINKS: list of strings."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (string= type "cloze") nil "Type for cloze type must be \"cloze\".")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (or (>= (length answer) (length hypothesis))
		 (null hypothesis))
	     nil
	     "Hypothesis value must be a list or nil, less or equal in length of Answer.")
  (cl-assert (listp answer) nil "Answer value must be a list.")
  (cl-assert (and (listp tags)
		  (cl-every 'stringp tags))
	     nil "Tags must be a list of strings..")
  (cl-assert (or (= suspend 0)
		 (= suspend 1))
	     nil "Suspend value must either 0 or 1")
  (cl-assert (and (listp links)
		  (cl-every 'stringp links))
	     nil "Links must be a list")
  (cl-assert (gnosis-cloze-check keimenon answer) nil
	     "Clozes (answer) values are not part of keimenon")
  (if (equal id "NEW")
      (progn
	(if (null answer)
	    ;; if answer is left null, extract all contents from keimenon.
	    (let* ((contents (gnosis-cloze-extract-contents keimenon))
		   (keimenon-new (gnosis-cloze-remove-tags keimenon))
		   (clozes (gnosis-cloze-extract-answers contents))
		   (hints (gnosis-cloze-extract-hints contents)))
	      (cl-loop for cloze in clozes
		       for hint in hints
		       do
		       (gnosis-add-note-fields deck-id type keimenon-new hint cloze parathema
						  tags suspend links)))
	  (gnosis-add-note-fields deck-id type keimenon (or hypothesis (list ""))
				  answer parathema tags suspend links)))
    (gnosis-update-note id keimenon hypothesis answer parathema tags links deck-id type)))

(defun gnosis-add-note--mc-cloze (id deck-id type keimenon hypothesis
				  answer parathema tags suspend links)
  "Helper for mc-cloze type notes.

Provide assertions for mc-cloze type notes.

DECK-ID: ID for deck.
ID: Integer for note ID.
TYPE: String for type, must be \"mc-cloze\".
HYPOTHESIS: List of strings or nil, hypothesis in mc-cloze note types
serve as hints.
ANSWER: List of answers for mc-clozes.
TAGS: List of tags.
PARATHEMA: Parathesis for thema.
SUSPEND: integer value, 1 or 0.
LINKS: list of strings."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (string= type "mc-cloze") nil "TYPE value must be \"mc-cloze\" .")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (and (listp hypothesis)
		  (> (length hypothesis) (length answer)))
	     nil "Hypothesis value must be a list, greater in length of ANSWER.")
  (cl-assert (and (listp answer) (length= answer 1)) nil
	     "ANSWER value must be a list of one item.")
  (cl-assert (and (listp tags)
		  (cl-every 'stringp tags))
	     nil "Tags must be a list of strings.")
  (cl-assert (or (= suspend 0)
		 (= suspend 1))
	     nil "Suspend value must either 0 or 1")
  (cl-assert (listp links) nil "Links must be a list")
  (cl-assert (gnosis-cloze-check keimenon answer) nil
	     "Clozes (answer) values are not part of keimenon")
  (if (equal id "NEW")
      (gnosis-add-note-fields deck-id type keimenon (or hypothesis (list ""))
			      answer parathema tags suspend links)
    (gnosis-update-note id keimenon hypothesis answer parathema tags links deck-id type)))

(defun gnosis-export--insert-read-only (string)
  "Insert STRING as read-only."
  (let ((start (point)))
    (insert string)
    ;; Set the just inserted string as read-only
    (add-text-properties start (point) '(read-only t))
    ;; Since the space is inserted outside of the read-only region, it's editable
    (let ((inhibit-read-only t))
      (insert " "))))

(defun gnosis-export-make-read-only (&rest values)
  "Make the provided VALUES read-only in the whole buffer."
  (goto-char (point-min))
  (dolist (value values)
    (while (search-forward value nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'read-only t)))
  (goto-char (point-min)))

(cl-defun gnosis-export--insert-note (id type &optional keimenon hypothesis
				      answer parathema tags example)
  "Insert note for note ID.

TYPE: Note type, refer to `gnosis-note-types'
KEIMENON: Text user is first presented with.
HYPOTHESIS: Hypothesis for what the ANSWER is
ANSWER: The revelation after KEIMENON
PARATHEMA: The text where NOTE is derived from.
TAGS: List of NOTE tags
EXAMPLE: Boolean value, if non-nil do not add properties for note."
  (let ((components `(("** Keimenon" . ,keimenon)
                      ("** Hypothesis" . ,hypothesis)
                      ("** Answer" . ,answer)
                      ("** Parathema" . ,parathema))))
    (insert "\n* Thema")
    (org-set-tags tags)
    (unless example
      (org-set-property "GNOSIS_ID" id)
      (org-set-property "GNOSIS_TYPE" type)
      (gnosis-export-make-read-only ":PROPERTIES:"
				 (format "GNOSIS_ID: %s" id)
				 (format "GNOSIS_TYPE: %s" type)
				 ":END:"))
    (dolist (comp components)
      (goto-char (point-max))
      (gnosis-export--insert-read-only (car comp))
      (insert "\n" (or (cdr comp) "") "\n\n"))))

(defun gnosis-export-parse--deck-name (&optional parsed-data)
  "Retrieve deck name from PARSED-DATA."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
	 (title (org-element-map parsed-data 'keyword
		  (lambda (kw)
		    (when (string= (org-element-property :key kw) "DECK")
                      (org-element-property :value kw)))
		  nil t)))
    title))

(defun gnosis-export-parse-notes (&optional separator)
  "Extract content for each level-2 heading for note headings with a GNOSIS_ID.

Split content of Hypothesis and Answer headings using SEPARATOR."
  (let ((sep (or separator gnosis-export-separator))
        results)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let* ((level (org-element-property :level headline))
               (gnosis-id (org-element-property :GNOSIS_ID headline))
               (gnosis-type (org-element-property :GNOSIS_TYPE headline))
               (tags (org-element-property :tags headline)))
          (when (and (= level 1) gnosis-id gnosis-type)
            (let (entry)
              (push gnosis-id entry)
              (push gnosis-type entry)
              (dolist (child (org-element-contents headline))
                (when (eq 'headline (org-element-type child))
                  (let* ((child-title (org-element-property :raw-value child))
                         (child-text (substring-no-properties
                                    (string-trim
                                     (org-element-interpret-data
                                      (org-element-contents child)))))
                         (processed-text
                          (cond
                           ((and (member child-title '("Hypothesis" "Answer"))
                                 (not (string-empty-p child-text)))
                            (mapcar (lambda (s)
                                    (string-trim
                                     (string-remove-prefix "-"
                                      (string-remove-prefix sep s))))
                                  (split-string child-text sep t "[ \t\n]+")))
                           ((string-empty-p child-text) nil)
                           (t child-text))))
                    (push processed-text entry))))
              (push tags entry)
              (push (nreverse entry) results)))))
      nil nil)
    results))

(defun gnosis-export-notes (ids &optional new-p)
  "Export notes for IDS.

If NEW-P replace the ids of notes with NEW, used for new notes to
generate new note id."
  (cl-assert (listp ids) nil "IDS value must be a list.")
  ;; Extract just the ID values if they're in a list structure
  (let ((id-values (mapcar (lambda (id)
                             (if (listp id) (car id) id))
                           ids)))
    ;; Process each note
    (dolist (id id-values)
      (let ((note-data (append (gnosis-select '[type keimenon hypothesis answer tags]
                                              'notes `(= id ,id) t)
                               (gnosis-select 'parathema 'extras `(= id ,id) t))))
        (gnosis-export--insert-note
         (if new-p "NEW" (number-to-string id))
         (nth 0 note-data)
         (nth 1 note-data)
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 2 note-data) gnosis-export-separator))
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 3 note-data) gnosis-export-separator))
         (nth 5 note-data)
         (nth 4 note-data))))))

(defun gnosis-export-deck (&optional deck filename new-p)
  "Export contents of DECK to FILENAME."
  (interactive (list (gnosis--get-deck-id)
                     (read-file-name "Export to file: ")
		     (not (y-or-n-p "Export with current note ids? "))))
  (let* ((deck-name (gnosis--get-deck-name deck))
	 (filename (if (file-directory-p filename)
		       (expand-file-name deck-name filename)
		     filename)))
    (unless (string-match-p "\\.org$" filename)
      (setq filename (concat (or filename deck-name) ".org")))
    (with-current-buffer (get-buffer-create (format "EXPORT: %s" deck-name))
      (let ((inhibit-read-only t))
        (org-mode)
        (erase-buffer)
        (insert (format "#+DECK: %s\n\n" deck-name))
        (let ((note-ids (gnosis-select 'id 'notes `(= deck-id ,deck))))
          (gnosis-export-notes note-ids new-p)
          (when filename
            (write-file filename)
            (message "Exported deck to %s" filename)))))))

(defun gnosis-save-note (note deck)
  "Save NOTE for DECK."
  (let* ((id (nth 0 note))
	 (type (nth 1 note))
	 (keimenon (nth 2 note))
	 (hypothesis (nth 3 note))
	 (answer (nth 4 note))
	 (parathema (or (nth 5 note) ""))
	 (tags (nth 6 note))
	 (links (append (gnosis-extract-id-links parathema)
			(gnosis-extract-id-links keimenon)))
	 (note-func (cdr (assoc (downcase type)
				  (mapcar (lambda (pair) (cons (downcase (car pair))
							  (cdr pair)))
					  gnosis-note-types)))))
    (funcall note-func id deck type keimenon hypothesis
	     answer parathema tags 0 links)))

(defun gnosis-save ()
  "Save notes in current buffer."
  (interactive nil gnosis-edit-mode)
  (let ((notes (gnosis-export-parse-notes))
	(deck (gnosis--get-deck-id (gnosis-export-parse--deck-name))))
    (cl-loop for note in notes
	     do (gnosis-save-note note deck))
    (gnosis-edit-quit)))

;;;###autoload
(defun gnosis-save-deck (deck-name)
  "Save notes for deck with DECK-NAME."
  (interactive
   (progn
     (unless (eq major-mode 'org-mode)
       (user-error "This function can only be used in org-mode buffers"))
     (list (read-string "Deck name: " (gnosis-export-parse--deck-name)))))
  (let ((notes (gnosis-export-parse-notes))
	(deck (gnosis-get-deck-id deck-name)))
    (cl-loop for note in notes
	     do (gnosis-save-note note deck))))

;;;###autoload
(defun gnosis-import-deck (file)
  "Save gnosis deck from FILE."
  (interactive "fFile: ")
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (gnosis-save-deck (gnosis-export-parse--deck-name))))

;;;###autoload
(defun gnosis-add-note (deck type &optional keimenon hypothesis
			      answer parathema tags example)
  "Add note with TYPE in DECK."
  (interactive (list
		(gnosis--get-deck-name)
		(downcase (completing-read "Select type: " gnosis-note-types))))
  (window-configuration-to-register :gnosis-edit)
  (pop-to-buffer "*Gnosis NEW*")
  (with-current-buffer "*Gnosis NEW*"
    (let ((inhibit-read-only 1))
      (erase-buffer))
    (insert "#+DECK: " deck)
    (gnosis-edit-mode)
    (gnosis-export--insert-note "NEW" type keimenon hypothesis
				answer parathema tags example))
  (search-backward "keimenon")
  (forward-line))

(defun gnosis-edit-note (id)
  "Edit note with ID."
  (window-configuration-to-register :gnosis-edit)
  (with-current-buffer (pop-to-buffer "*Gnosis Edit*")
    (let ((inhibit-read-only 1)
	  (deck-name (gnosis--get-deck-name
		      (gnosis-get 'deck-id 'notes `(= id ,id)))))
      (erase-buffer)
      (insert "#+DECK: " deck-name))
    (gnosis-edit-mode)
    (gnosis-export-notes (list id))
    (search-backward "keimenon")
    (forward-line)))

(defun gnosis-edit-quit ()
  "Quit recrusive edit & kill current buffer."
  (interactive nil gnosis-edit-mode)
  (kill-buffer)
  (jump-to-register :gnosis-edit)
  (when gnosis-review-editing-p
    (setf gnosis-review-editing-p nil)
    (exit-recursive-edit)))

(defvar-keymap gnosis-edit-mode-map
  :doc "gnosis org mode map"
  "C-c C-c" #'gnosis-save
  "C-c C-q" #'gnosis-tags-prompt
  "C-c C-k" #'gnosis-edit-quit)

(define-derived-mode gnosis-edit-mode org-mode "Gnosis Org"
  "Gnosis Org Mode."
  :interactive nil
  :lighter " Gnosis Edit"
  :keymap gnosis-edit-mode-map
  (setq header-line-format
	(substitute-command-keys
	 " Save note by running \\[gnosis-save] or \\[gnosis-edit-quit] to quit")))

(defun gnosis-validate-custom-values (new-value)
  "Validate the structure and values of NEW-VALUE for gnosis-custom-values."
  (unless (listp new-value)
    (error "GNOSIS-CUSTOM-VALUES should be a list of entries"))
  (dolist (entry new-value)
    (unless (and (listp entry) (= (length entry) 3)
                 (memq (nth 0 entry) '(:deck :tag))
                 (stringp (nth 1 entry))
                 (listp (nth 2 entry))) ; Ensure the third element is a plist
      (error
       "Each entry should a have :deck or :tag keyword, a string, and a plist of custom values"))
    (let ((proto (plist-get (nth 2 entry) :proto))
          (anagnosis (plist-get (nth 2 entry) :anagnosis))
          (epignosis (plist-get (nth 2 entry) :epignosis))
          (agnoia (plist-get (nth 2 entry) :agnoia))
          (amnesia (plist-get (nth 2 entry) :amnesia))
          (lethe (plist-get (nth 2 entry) :lethe)))
      (unless (and (listp proto) (cl-every #'integerp proto))
        (error "Proto must be a list of integer values"))
      (unless (or (null anagnosis) (integerp anagnosis))
        (error "Anagnosis should be an integer"))
      (unless (or (null epignosis) (numberp epignosis))
        (error "Epignosis should be a number"))
      (unless (or (null agnoia) (numberp agnoia))
        (error "Agnoia should be a number"))
      (unless (or (null amnesia) (and (numberp amnesia) (<= amnesia 1) (>= amnesia 0)))
        (error "Amnesia should be a number between 0 and 1"))
      (unless (or (null lethe) (and (integerp lethe) (> lethe 0)))
        (error "Lethe should be an integer greater than 0")))))

(defun gnosis-custom-values-watcher (symbol new-value _operation _where)
  "Watcher for gnosis custom values.

SYMBOL to watch changes for.
NEW-VALUE is the new value set to the variable.
OPERATION is the type of operation being performed.
WHERE is the buffer or object where the change happens."
  (when (eq symbol 'gnosis-custom-values)
    (gnosis-validate-custom-values new-value)))

(add-variable-watcher 'gnosis-custom-values 'gnosis-custom-values-watcher)

;; Validate custom values during review process as well.
(defun gnosis-get-custom-values--validate (plist valid-keywords)
  "Verify that PLIST consists of VALID-KEYWORDS."
  (let ((keys (let (ks)
                (while plist
                  (setq ks (cons (car plist) ks))
                  (setq plist (cddr plist)))
                ks)))
    (let ((invalid-key (cl-find-if (lambda (key) (not (member key valid-keywords))) keys)))
      (if invalid-key
          (error "Invalid custom keyword found in: %s" invalid-key)
        t))))

(defun gnosis-get-custom-values (key search-value &optional values)
  "Return SEARCH-VALUE for KEY from VALUES.

VALUES: Defaults to `gnosis-custom-values'."
  (cl-assert (or (eq key :deck) (eq key :tag)) nil "Key value must be either :tag or :deck")
  (cl-assert (stringp search-value) nil "Search-value must be the name of tag or deck as a string.")
  (let ((results)
	(values (or values gnosis-custom-values)))
    (dolist (rule values)
      (when (and (plist-get rule key)
                 (equal (plist-get rule key) search-value))
        (setq results (append results (nth 2 rule)))))
    (gnosis-get-custom-values--validate results gnosis-custom--valid-values)
    results))

(defun gnosis-get-custom-deck-value (deck value &optional values)
  "Return custom VALUE for note DECK."
  (plist-get (gnosis-get-custom-values :deck deck values) value))

(defun gnosis-get-custom-tag-values (id keyword &optional custom-tags custom-values)
  "Return KEYWORD values for note ID."
  (cl-assert (keywordp keyword) nil "keyword must be a keyword!")
  (let ((tags (if id (gnosis-get 'tags 'notes `(= id ,id)) custom-tags)))
    (cl-loop for tag in tags
	     ;; Only collect non-nil values
	     when (plist-get (gnosis-get-custom-values :tag tag custom-values) keyword)
	     collect (plist-get (gnosis-get-custom-values :tag tag custom-values)
				keyword))))

(defun gnosis-get-note-tag-amnesia (id &optional custom-tags custom-values)
  "Return tag MINIMUM amnesia for note ID.

The closer the amnesia value is to 0, the closer it is to total
amnesia i.e next interval to be 0.

CUSTOM-TAGS: Specify tags for note id.
CUSTOM-VALUES: Specify values for tags."
  (let ((amnesia-values (gnosis-get-custom-tag-values id :amnesia
						      custom-tags custom-values)))
    (and amnesia-values (apply #'max amnesia-values))))

(defun gnosis-get-note-deck-amnesia (id &optional custom-deck custom-values)
  "Return tag amnesia for note ID.

Optionally, use CUSTOM-DECK and CUSTOM-VALUES."
  (let ((deck (or (gnosis-get-note-deck-name id) custom-deck )))
    (or (gnosis-get-custom-deck-value deck :amnesia custom-values)
	gnosis-algorithm-amnesia-value)))

(defun gnosis-get-note-amnesia (id &optional custom-deck custom-tags custom-values )
  "Return amnesia value for note ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-TAGS: Specify custom tags for note id.
CUSTOM-VALUES: Specify custom values."
  (let* ((deck-amnesia (gnosis-get-note-deck-amnesia id custom-deck custom-values))
         (tags-amnesia (gnosis-get-note-tag-amnesia id custom-tags custom-values))
	 (note-amnesia (or tags-amnesia deck-amnesia)))
    (if (>= note-amnesia 1)
	(error "Amnesia value must be lower than 1")
      note-amnesia)))

(defun gnosis-get-note-tag-epignosis (id &optional custom-tags custom-values)
  "Return tag epignosis for note ID.

CUSTOM-TAGS: Specify custom tags for note id.
CUSTOM-VALUES: Specify custom values."
  (let* ((epignosis-values (gnosis-get-custom-tag-values id :epignosis custom-tags custom-values)))
    (and epignosis-values (apply #'max epignosis-values))))

(defun gnosis-get-note-deck-epignosis (id &optional custom-deck custom-values)
  "Return deck epignosis for note ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-VALUES: Specify custom values."
  (let ((deck (or (gnosis-get-note-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :epignosis custom-values)
	gnosis-algorithm-epignosis-value)))

(defun gnosis-get-note-epignosis (id &optional custom-deck custom-tags custom-values)
  "Return epignosis value for note ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-TAGS: Specify custom tags for note id.
CUSTOM-VALUES: Specify custom values."
  (let* ((deck-epignosis (gnosis-get-note-deck-epignosis id custom-deck custom-values))
         (tag-epignosis (gnosis-get-note-tag-epignosis id custom-tags custom-values))
	 (note-epignosis (or tag-epignosis deck-epignosis)))
    (if (>= note-epignosis 1)
	(error "Epignosis value must be lower than 1")
      note-epignosis)))

(defun gnosis-get-note-tag-agnoia (id &optional custom-tags custom-values)
  "Return agnoia value for note ID.

CUSTOM-TAGS: Specify custom tags for note id.
CUSTOM-VALUES: Specify custom values."
  (let ((agnoia-values (gnosis-get-custom-tag-values id :agnoia custom-tags custom-values)))
    (and agnoia-values (apply #'max agnoia-values))))

(defun gnosis-get-note-deck-agnoia (id &optional custom-deck custom-values)
  "Return agnoia value for note ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-VALUES: Specify custom values."
  (let ((deck (or (gnosis-get-note-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :agnoia custom-values)
	gnosis-algorithm-agnoia-value)))

(defun gnosis-get-note-agnoia (id &optional custom-deck custom-tags custom-values)
  "Return agnoia value for note ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-TAGS: Specify custom tags for note id.
CUSTOM-VALUES: Specify custom values."
  (let* ((deck-agnoia (gnosis-get-note-deck-agnoia id custom-deck custom-values))
         (tag-agnoia (gnosis-get-note-tag-agnoia id custom-tags custom-values))
	 (note-agnoia (or tag-agnoia deck-agnoia)))
    (if (>= note-agnoia 1)
	(error "Agnoia value must be lower than 1")
      note-agnoia)))

(defun gnosis-proto-max-values (proto-values)
  "Return max values from PROTO-VALUES."
  (if (not (and (listp proto-values) (cl-every #'listp proto-values)))
      proto-values
    (let* ((max-len (apply #'max (mapcar #'length proto-values)))
           (padded-lists (mapcar (lambda (lst)
                                   (append lst (make-list (- max-len (length lst)) 0)))
                                 proto-values)))
      (apply #'cl-mapcar #'max padded-lists))))

(defun gnosis-get-note-proto (id &optional custom-tags custom-deck custom-values)
  "Return tag proto values for note ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let* ((deck (or custom-deck (gnosis-get-note-deck-name id)))
	 (tags-proto (gnosis-get-custom-tag-values id :proto custom-tags custom-values))
	 (decks-proto (gnosis-get-custom-deck-value deck :proto custom-values)))
    (if tags-proto (gnosis-proto-max-values tags-proto)
      (gnosis-proto-max-values (or decks-proto gnosis-algorithm-proto)))))

(defun gnosis-get-note-tag-anagnosis (id &optional custom-tags custom-values)
  "Return the minimum anagnosis tag value for note ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead."
  (let ((anagnosis-values (gnosis-get-custom-tag-values id :anagnosis custom-tags custom-values)))
    (and anagnosis-values (apply #'min anagnosis-values))))

(defun gnosis-get-note-deck-anagnosis (id &optional custom-deck custom-values)
  "Return anagnosis deck value for note ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let ((deck (or (gnosis-get-note-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :anagnosis custom-values)
	gnosis-algorithm-anagnosis-value)))

(defun gnosis-get-note-anagnosis (id &optional custom-deck custom-tags custom-values)
  "Return minimum anagnosis value for note ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let* ((deck-anagnosis (gnosis-get-note-deck-anagnosis id custom-deck custom-values))
	 (tag-anagnosis (gnosis-get-note-tag-anagnosis id custom-tags custom-values))
	 (note-anagnosis (or tag-anagnosis deck-anagnosis)))
    note-anagnosis))

(defun gnosis-get-note-deck-lethe (id &optional custom-deck custom-values)
  "Return lethe deck value for note ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let ((deck (or (gnosis-get-note-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :lethe custom-values)
	gnosis-algorithm-lethe-value)))

(defun gnosis-get-note-tag-lethe (id &optional custom-tags custom-values)
  "Return note ID tag lethe values.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead."
  (let ((lethe-values (gnosis-get-custom-tag-values id :lethe custom-tags custom-values)))
    (and lethe-values (apply #'min lethe-values))))

(defun gnosis-get-note-lethe (id &optional custom-deck custom-tags custom-values)
  "Return note ID lethe value.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let* ((deck-lethe (gnosis-get-note-deck-lethe id custom-deck custom-values))
	 (tag-lethe (gnosis-get-note-tag-lethe id custom-tags custom-values))
	 (note-lethe (or tag-lethe deck-lethe)))
    note-lethe))

(defun gnosis-get-date-total-notes (&optional date)
  "Return total notes reviewed for DATE.

If entry for DATE does not exist, it will be created.

Defaults to current date."
  (cl-assert (listp date) nil "Date must be a list.")
  (let* ((date (or date (gnosis-algorithm-date)))
	 (date-log (gnosis-select
		    '[date reviewed-total reviewed-new] 'activity-log
		    `(= date ',(gnosis-algorithm-date)) t))
	 (reviewed-total (cadr date-log))
	 (reviewed-new (or (caddr date-log) 0)))
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
	 (reviewed-new (or (car (gnosis-select 'reviewed-new 'activity-log `(= date ',date) t))
			   0)))
    reviewed-new))
;; TODO: Auto tag overdue tags.
(defun gnosis-tags--append (id tag)
  "Append TAG to the list of tags of note ID."
  (cl-assert (numberp id) nil "ID must be the note id number")
  (cl-assert (stringp tag) nil "Tag must a string")
  (let* ((current-tags (gnosis-get 'tags 'notes `(= id ,id)))
	 (new-tags (append current-tags (list tag))))
    (gnosis-update 'notes `(= tags ',new-tags) `(= id ,id))))

(defun gnosis-search-note (&optional query)
  "Search for note QUERY.

Return note ids for notes that match QUERY."
  (cl-assert (or (stringp query) (eq query nil)))
  (let* ((query (or query (read-string "Search for note: ")))
         (words (split-string query))
         (clause-keimenon `(and ,@(mapcar (lambda (word)
					`(like keimenon ,(format "%%%s%%" word)))
                                      words)))
	 (clause-answer `(and ,@(mapcar (lambda (word)
					  `(like answer ,(format "%%%s%%" word)))
					words))))
    (append (gnosis-select 'id 'notes clause-keimenon t)
	    (gnosis-select 'id 'notes clause-answer t))))

;;; Database Schemas
(defconst gnosis-db--schemata
  '((decks
     ([(id integer :primary-key :autoincrement)
       (name text :not-null)]
      (:unique [name])))
    (notes
     ([(id integer :primary-key :autoincrement)
       (type text :not-null)
       (keimenon text :not-null)
       (hypothesis text :not-null)
       (answer text :not-null)
       (tags text :default untagged)
       (deck-id integer :not-null)]
      (:foreign-key [deck-id] :references decks [id]
		    :on-delete :cascade)))
    (review
     ([(id integer :primary-key :not-null) ;; note-id
       (gnosis integer :not-null)
       (amnesia integer :not-null)]
      (:foreign-key [id] :references notes [id]
		    :on-delete :cascade)))
    (review-log
     ([(id integer :primary-key :not-null) ;; note-id
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
    (activity-log
     ([(date text :not-null)
       (reviewed-total integer :not-null)
       (reviewed-new integer :not-null)]))
    (extras
     ([(id integer :primary-key :not-null)
       (parathema string)
       (review-image string)]
      (:foreign-key [id] :references notes [id]
		    :on-delete :cascade)))
     (tags
      ([(tag text :primary-key)]
       (:unique [tag])))
     (links
      ([(source text)
	(dest text)]
       (:foreign-key [source] :references notes [id]
		     :on-delete :cascade)
       (:unique [source dest])))))

(defun gnosis-update--make-list (column)
  "Make COLUMN values into a list."
  (let ((results (emacsql gnosis-db `[:select [id ,column] :from notes])))
    (dolist (row results)
      (let ((id (car row))
            (old-value (cadr row)))
	;; Update each entry, converting the value to a list representation
	(unless (listp old-value)
	  (emacsql gnosis-db `[:update notes
				       :set (= ,column $s1)
				       :where (= id $s2)]
		   (list old-value)
		   id)
	  (message "Update Note: %d" id))))))

(defun gnosis-db-update-v4 ()
  "Update to databse version v4."
  (let ((tags (gnosis-get-tags--unique)))
    (pcase-dolist (`(,table ,schema) (seq-filter (lambda (schema)
						   (member (car schema) '(links tags)))
						 gnosis-db--schemata))
      (emacsql gnosis-db [:create-table :if-not-exists $i1 $S2] table schema))
    (cl-loop for tag in tags
	     do (gnosis--insert-into 'tags `[,tag]))
    (emacsql gnosis-db [:alter-table notes :rename-column main :to keimenon])
    (emacsql gnosis-db [:alter-table notes :rename-column options :to hypothesis])
    (emacsql gnosis-db [:alter-table extras :rename-column extra-notes :to parathema])
    (emacsql gnosis-db [:alter-table extras :rename-column images :to review-image])
    (emacsql gnosis-db [:alter-table extras :drop-column extra-image])
    ;; Make sure all hypothesis & answer values are lists
    (gnosis-update--make-list 'hypothesis)
    (gnosis-update--make-list 'answer)
    ;; Fix MCQs
    (cl-loop for note in (gnosis-select 'id 'notes '(= type "mcq") t)
	     do (funcall
		 (lambda (id)
		   (let* ((data (gnosis-select '[hypothesis answer] 'notes `(= id ,id) t))
			  (hypothesis (nth 0 data))
			  (old-answer (car (nth 1 data)))
			  (new-answer (when (integerp old-answer)
					(list (nth (- 1 old-answer) hypothesis)))))
		     (when (integerp old-answer)
		       (gnosis-update 'notes `(= answer ',new-answer) `(= id ,id)))))
		 note))
    ;; Replace y-or-n with MCQ
    (cl-loop for note in (gnosis-select 'id 'notes '(= type "y-or-n") t)
	     do (funcall (lambda (id)
			   (let ((data (gnosis-select '[type hypothesis answer]
						      'notes `(= id ,id) t)))
			     (when (string= (nth 0 data) "y-or-n")
			       (gnosis-update 'notes '(= type "mcq") `(= id ,id))
			       (gnosis-update 'notes '(= hypothesis '("Yes" "No"))
					      `(= id ,id))
			       (if (= (car (nth 2 data)) 121)
				   (gnosis-update 'notes '(= answer '("Yes"))
						  `(= id ,id))
				 (gnosis-update 'notes '(= answer '("No"))
						`(= id ,id))))))
			 note))
    ;; Replace - with _, org does not support tags with dash.
    (cl-loop for tag in (gnosis-get-tags--unique)
	     ;; Replaces dashes to underscores.
	     if (string-match-p "-" tag)
	     do (gnosis-tag-rename tag (replace-regexp-in-string "-" "_" tag)))))

(defun gnosis-db-init ()
  "Create essential directories & database."
  (let ((gnosis-curr-version (caar (emacsql gnosis-db  [:pragma user-version]))))
    (unless (length> (emacsql gnosis-db [:select name :from sqlite-master
						 :where (= type table)])
		     3)
      (emacsql-with-transaction gnosis-db
	(pcase-dolist (`(,table ,schema) gnosis-db--schemata)
	  (emacsql gnosis-db [:create-table $i1 $S2] table schema))
        (emacsql gnosis-db [:pragma (= user-version org-gnosis-db-version)])))
    ;; Update database schema for version
    (cond ((= gnosis-curr-version 2)
	   (gnosis-db-update-v4)))))

(gnosis-db-init)

;; Dashboard
;;;;;;;;;;;;

(defvar gnosis-dashboard-note-ids nil
  "Store note ids for dashboard.")

(defvar gnosis-dashboard-buffer-name "*Gnosis Dashboard*"
  "Name of gnosis-dashboard buffer.")

(defvar gnosis-dashboard--current
  '(:type nil :ids nil)
  "Current values to return after edits.")

(defvar gnosis-dashboard--selected-ids nil
  "Selected ids from the tabulated list.")


(defun gnosis-dashboard-return (&optional current-values)
  "Return to dashboard for CURRENT-VALUES."
  (interactive)
  (let* ((current-values (or current-values gnosis-dashboard--current))
	 (type (plist-get current-values :type))
	 (ids (plist-get current-values :ids)))
    (cond ((eq type 'notes)
	   (gnosis-dashboard-output-notes ids))
	  ((eq type 'decks)
	   (gnosis-dashboard-output-decks))
	  ((eq type 'tags)
	   (gnosis-dashboard-output-tags))
	  ((eq type 'history)
	   (gnosis-dashboard-history)))))

(defun gnosis-dashboard--streak (dates &optional num date)
  "Return current review streak number as a string.

DATES: Dates in the activity log, a list of dates in (YYYY MM DD).
NUM: Streak number.
DATE: Integer, used with `gnosis-algorithm-date' to get previous dates."
  (let ((num (or num 0))
	(date (or date -1)))
    (cond ((> num 666)
	   "+666") ;; do not go over 666, avoiding `max-lisp-eval-depth'
	  ((member (gnosis-algorithm-date date) dates)
	   (gnosis-dashboard--streak dates (cl-incf num) (- date 1)))
	  (t (number-to-string (if (member (gnosis-algorithm-date) dates)
				   (+ 1 num)
				 num))))))

(defun gnosis-dashboard-output-average-rev ()
  "Output the average daily notes reviewed as a string for the dashboard."
  (format "%.2f" (gnosis-calculate-average-daily-reviews)))

(defun gnosis-dashboard-edit-note ()
  "Edit note with ID."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gnosis-edit-note id)))

(defun gnosis-dashboard-suspend-note ()
  "Suspend note."
  (interactive nil gnosis-dashboard-notes-mode)
  (let ((current-line (line-number-at-pos)))
    (gnosis-toggle-suspend-notes
     (or gnosis-dashboard--selected-ids (list (tabulated-list-get-id))))
    (gnosis-dashboard-output-notes gnosis-dashboard-note-ids)
    (revert-buffer t t t)
    (forward-line (- current-line 1))))

(defun gnosis-dashboard-delete ()
  "Delete note."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (if gnosis-dashboard--selected-ids
	(gnosis-dashboard-marked-delete)
      (gnosis-delete-note (tabulated-list-get-id))
      (gnosis-dashboard-output-notes gnosis-dashboard-note-ids)
      (revert-buffer t t t))
    (forward-line (- current-line 1))))

(defun gnosis-dashboard-search-note (&optional str)
  "Search for notes with STR."
  (interactive)
  (gnosis-dashboard-output-notes
   (gnosis-collect-note-ids :query (or str (read-string "Search for note: ")))))

(defvar-keymap gnosis-dashboard-notes-mode-map
  :doc "Keymap for notes dashboard."
  "q" #'gnosis-dashboard
  "e" #'gnosis-dashboard-edit-note
  "s" #'gnosis-dashboard-suspend-note
  "SPC" #'gnosis-dashboard-search-note
  "a" #'gnosis-add-note
  "r" #'gnosis-dashboard-return
  "g" #'gnosis-dashboard-return
  "d" #'gnosis-dashboard-delete
  "m" #'gnosis-dashboard-mark-toggle
  "M" #'gnosis-dashboard-mark-all
  "u" #'gnosis-dashboard-mark-toggle
  "U" #'gnosis-dashboard-unmark-all)

(define-minor-mode gnosis-dashboard-notes-mode
  "Minor mode for gnosis dashboard notes output."
  :keymap gnosis-dashboard-notes-mode-map
  (gnosis-dashboard-decks-mode -1)
  (gnosis-dashboard-tags-mode -1))

(defun gnosis-dashboard--output-notes (note-ids)
  "Output tabulated-list format for NOTE-IDS."
  (cl-assert (listp note-ids))
  (let ((entries (emacsql gnosis-db
			  `[:select
			    [notes:id notes:keimenon notes:hypothesis notes:answer
				      notes:tags notes:type review-log:suspend]
			    :from notes
			    :join review-log :on (= notes:id review-log:id)
			    :where (in notes:id ,(vconcat note-ids))])))
    (cl-loop for sublist in entries
             collect
	     (list (car sublist)
                   (vconcat
		    (cl-loop for item in (cdr sublist)
			     if (listp item)
			     collect (mapconcat (lambda (x) (format "%s" x)) item ",")
			     else
			     collect
			     (replace-regexp-in-string "\n" " " (format "%s" item))))))))

(defun gnosis-dashboard-output-notes (note-ids)
  "Return NOTE-IDS contents on gnosis dashboard."
  (cl-assert (listp note-ids) t "`note-ids' must be a list of note ids.")
  (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
  (gnosis-dashboard-enable-mode)
  (gnosis-dashboard-notes-mode)
  (setf tabulated-list-format `[("Keimenon" ,(/ (window-width) 4) t)
                                ("Hypothesis" ,(/ (window-width) 6) t)
                                ("Answer" ,(/ (window-width) 6) t)
                                ("Tags" ,(/ (window-width) 5) t)
                                ("Type" ,(/ (window-width) 10) t)
                                ("Suspend" ,(/ (window-width) 6) t)]
        gnosis-dashboard-note-ids note-ids
        tabulated-list-entries nil)
  (make-local-variable 'tabulated-list-entries)
  (tabulated-list-init-header)
  (let ((inhibit-read-only t)
	(entries (gnosis-dashboard--output-notes note-ids)))
    (erase-buffer)
    (insert (format "Loading %s notes..." (length note-ids)))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)
    (setf gnosis-dashboard--current
	  `(:type notes :ids ,note-ids))))

(defun gnosis-dashboard-deck-note-count (id)
  "Return total note count for deck with ID."
  (let ((note-count (length (gnosis-select 'id 'notes `(= deck-id ,id) t))))
    (when (gnosis-select 'id 'decks `(= id ,id))
      (list (number-to-string note-count)))))

(defun gnosis-dashboard-output-tag (tag)
  "Output TAG name and total notes."
  (let ((notes (gnosis-get-tag-notes tag)))
    `(,tag ,(number-to-string (length notes)))))

(defun gnosis-dashboard-sort-total-notes (entry1 entry2)
  "Sort function for the total notes column, for ENTRY1 and ENTRY2."
  (let ((total1 (string-to-number (elt (cadr entry1) 1)))
        (total2 (string-to-number (elt (cadr entry2) 1))))
    (< total1 total2)))

(defun gnosis-dashboard-rename-tag ()
  "Rename TAG to NEW-TAG."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (gnosis-tag-rename (tabulated-list-get-id))
    (gnosis-dashboard-output-tags)
    (forward-line (- current-line 1))))

(defun gnosis-dashboard-delete-tag (&optional tag)
  "Rename TAG to NEW-TAG."
  (interactive)
  (let ((tag (or tag (tabulated-list-get-id))))
    (when (y-or-n-p (format "Delete tag %s?"
			    (propertize tag 'face 'font-lock-keyword-face)))
      (cl-loop for note in (gnosis-get-tag-notes tag)
	       do (let* ((tags (car (gnosis-select '[tags] 'notes `(= id ,note) t)))
			 (new-tags (remove tag tags)))
		    (gnosis-update 'notes `(= tags ',new-tags) `(= id ,note))))
      ;; Update tags in database
      (gnosis-tags-refresh)
      ;; Output tags anew
      (gnosis-dashboard-output-tags))))


(defun gnosis-dashboard-rename-deck (&optional deck-id new-name)
  "Rename deck where DECK-ID with NEW-NAME."
  (interactive)
  (let ((deck-id (or deck-id (string-to-number (tabulated-list-get-id))))
	(new-name (or new-name (read-string "New deck name: "))))
    (gnosis-update 'decks `(= name ,new-name) `(= id ,deck-id))
    (gnosis-dashboard-output-decks)))

(defun gnosis-dashboard-suspend-tag (&optional tag)
  "Suspend notes of TAG."
  (interactive)
  (let* ((tag (or tag (tabulated-list-get-id)))
	 (notes (gnosis-get-tag-notes tag))
	 (suspend (if current-prefix-arg 0 1))
	 (confirm-msg (y-or-n-p
		       (if (= suspend 0)
			   "Unsuspend all notes for tag? "
			 "Suspend all notes for tag?"))))
    (when confirm-msg
      (emacsql gnosis-db
	       `[:update review-log :set (= suspend ,suspend) :where
			 (in id ,(vconcat notes))])
      (if (= suspend 0)
	  (message "Unsuspended %s notes" (length notes))
	(message "Suspended %s notes" (length notes))))))

(defun gnosis-dashboard-tag-view-notes (&optional tag)
  "View notes for TAG."
  (interactive)
  (let ((tag (or tag (tabulated-list-get-id))))
    (gnosis-dashboard-output-notes (gnosis-get-tag-notes tag))))

(defvar-keymap gnosis-dashboard-tags-mode-map
  "RET" #'gnosis-dashboard-tag-view-notes
  "e" #'gnosis-dashboard-rename-tag
  "q" #'gnosis-dashboard
  "s" #'gnosis-dashboard-suspend-tag
  "r" #'gnosis-dashboard-rename-tag
  "d" #'gnosis-dashboard-delete-tag
  "g" #'gnosis-dashboard-return)

(define-minor-mode gnosis-dashboard-tags-mode
  "Mode for dashboard output of tags."
  :keymap gnosis-dashboard-tags-mode-map)

(defun gnosis-dashboard-output-tags (&optional tags)
  "Format gnosis dashboard with output of TAGS."
  (gnosis-tags-refresh) ;; Refresh tags
  (let ((tags (or tags (gnosis-get-tags--unique))))
    (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
    (gnosis-dashboard-enable-mode)
    (gnosis-dashboard-tags-mode)
    (setf gnosis-dashboard--current '(:type 'tags))
    (setq tabulated-list-format [("Name" 35 t)
                                 ("Total Notes" 10 gnosis-dashboard-sort-total-notes)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop for tag in tags
                   collect (list (car (gnosis-dashboard-output-tag tag))
                                 (vconcat (gnosis-dashboard-output-tag tag)))))
    (tabulated-list-print t)))

(defun gnosis-dashboard-output-deck (id)
  "Output contents from deck ID, formatted for gnosis dashboard."
  (let* ((deck-name (gnosis-select 'name 'decks `(= id ,id) t))
         (note-count (gnosis-dashboard-deck-note-count id))
         (combined-data (append deck-name (mapcar #'string-to-number note-count))))
    (mapcar (lambda (item) (format "%s" item))
            (seq-filter (lambda (item)
                         (not (and (vectorp item) (seq-empty-p item))))
                       combined-data))))

(defvar-keymap gnosis-dashboard-decks-mode-map
  "e" #'gnosis-dashboard-rename-deck
  "r" #'gnosis-dashboard-rename-deck
  "q" #'gnosis-dashboard
  "a" #'gnosis-dashboard-decks-add
  "s" #'gnosis-dashboard-decks-suspend-deck
  "d" #'gnosis-dashboard-decks-delete
  "RET" #'gnosis-dashboard-decks-view-deck)

(define-minor-mode gnosis-dashboard-decks-mode
  "Minor mode for deck output."
  :keymap gnosis-dashboard-decks-mode-map)

(defun gnosis-dashboard-output-decks ()
  "Return deck contents for gnosis dashboard."
  (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
  (gnosis-dashboard-enable-mode)
  (gnosis-dashboard-decks-mode)
  (setq tabulated-list-format [("Name" 15 t)
			       ("Total Notes" 10 gnosis-dashboard-sort-total-notes)])
  (tabulated-list-init-header)
  (setq tabulated-list-entries
	(cl-loop for id in (gnosis-select 'id 'decks nil t)
		 for output = (gnosis-dashboard-output-deck id)
		 when output
		 collect (list (number-to-string id) (vconcat output))))
  (tabulated-list-print t)
  (setf gnosis-dashboard--current `(:type decks :ids ,(gnosis-select 'id 'decks nil t))))

(defun gnosis-dashboard-decks-add ()
  "Add deck & refresh."
  (interactive)
  (gnosis-add-deck (read-string "Deck name: "))
  (gnosis-dashboard-output-decks)
  (revert-buffer t t t))

(defun gnosis-dashboard-decks-suspend-deck (&optional deck-id)
  "Suspend notes for DECK-ID.

When called with called with a prefix, unsuspend all notes of deck."
  (interactive)
  (let ((deck-id (or deck-id (string-to-number (tabulated-list-get-id)))))
    (gnosis-suspend-deck deck-id)
    (gnosis-dashboard-output-decks)
    (revert-buffer t t t)))

(defun gnosis-dashboard-decks-delete (&optional deck-id)
  "Delete DECK-ID."
  (interactive)
  (let ((deck-id (or deck-id (string-to-number (tabulated-list-get-id)))))
    (gnosis-delete-deck deck-id)
    (gnosis-dashboard-output-decks)
    (revert-buffer t t t)))

(defun gnosis-dashboard-decks-view-deck (&optional deck-id)
  "View notes of DECK-ID."
  (interactive)
  (let ((deck-id (or deck-id (string-to-number (tabulated-list-get-id)))))
    (gnosis-dashboard-output-notes (gnosis-collect-note-ids :deck deck-id))))

(defun gnosis-dashboard-history (&optional history)
  "Display review HISTORY."
  (interactive)
  (let* ((history (or history
		      (gnosis-select '[date reviewed-total reviewed-new]
				     'activity-log)))
	 (buffer (get-buffer-create "*Gnosis History*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer))
      (tabulated-list-mode)
      (setq tabulated-list-format
            `[("Date" ,(/ (window-width) 6) t)
              ("Total Reviews" ,(/ (window-width) 6) gnosis-dashboard-sort-total-notes)
              ("New" ,(/ (window-width) 6) gnosis-dashboard-sort-total-notes)])
      (make-local-variable 'tabulated-list-entries)
      ;; Sort for date
      (setq tabulated-list-sort-key (cons "Date" t))
      (setq tabulated-list-entries
            (cl-loop for entry in history
                     collect (list (car entry)
                                   (vector (propertize
					    (format "%04d/%02d/%02d"
						    (nth 0 (car entry))
						    (nth 1 (car entry))
						    (nth 2 (car entry)))
					    'face 'org-date)
                                           (number-to-string (cadr entry))
                                           (number-to-string (caddr entry))))))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (setq gnosis-dashboard--current
	    '(:type history)))
    (pop-to-buffer buffer)))

(defvar-keymap gnosis-dashboard-mode-map
  :doc "gnosis-dashboard keymap"
  "q" #'quit-window
  "h" #'gnosis-dashboard-menu
  "H" #'gnosis-dashboard-history
  "r" #'gnosis-review
  "a" #'gnosis-add-note
  "A" #'gnosis-add-deck
  "s" #'gnosis-dashboard-suffix-query
  "n" #'(lambda () (interactive) (gnosis-dashboard-output-notes (gnosis-collect-note-ids)))
  "d" #'gnosis-dashboard-suffix-decks
  "t" #'(lambda () (interactive) (gnosis-dashboard-output-tags)))

(define-derived-mode gnosis-dashboard-mode tabulated-list-mode "Gnosis Dashboard"
  "Major mode for displaying Gnosis dashboard."
  :keymap gnosis-dashboard-mode-map
  :interactive nil
  (setq-local header-line-format nil)
  (setq tabulated-list-padding 2
	tabulated-list-sort-key nil
	gnosis-dashboard--selected-ids nil)
  (display-line-numbers-mode 0))

(defun gnosis-dashboard-enable-mode ()
  "Enable `gnosis-dashboard-mode'."
  (when (and (string= (buffer-name) gnosis-dashboard-buffer-name)
	     (not (eq major-mode 'gnosis-dashboard-mode)))
    (gnosis-dashboard-mode)))

(cl-defun gnosis-dashboard--search (&optional dashboard-type (note-ids nil))
  "Display gnosis dashboard.

NOTE-IDS: List of note ids to display on dashboard.  When nil, prompt
for dashboard type.

DASHBOARD-TYPE: either Notes or Decks to display the respective dashboard."
  (interactive)
  (let ((dashboard-type (or dashboard-type
			    (cadr (read-multiple-choice
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
	("search" (gnosis-dashboard-search-note))))
    (tabulated-list-print t)))

(defun gnosis-dashboard-mark-toggle ()
  "Toggle mark on the current item in the tabulated-list."
  (interactive)
  (let ((inhibit-read-only t)
        (entry (tabulated-list-get-entry))
	(id (tabulated-list-get-id)))
    (if entry
        (let ((beg (line-beginning-position))
              (end (line-end-position))
              (overlays (overlays-in (line-beginning-position) (line-end-position))))
          (if (cl-some (lambda (ov) (overlay-get ov 'gnosis-mark)) overlays)
              (progn
                (remove-overlays beg end 'gnosis-mark t)
		(setq gnosis-dashboard--selected-ids
		      (remove id gnosis-dashboard--selected-ids)))
            (let ((ov (make-overlay beg end)))
	      (unless (member id gnosis-dashboard--selected-ids)
		(setf gnosis-dashboard--selected-ids
		      (cons id gnosis-dashboard--selected-ids)))
              (overlay-put ov 'face 'highlight)
              (overlay-put ov 'gnosis-mark t)))
	  (forward-line))
      (message "No entry at point"))))

(defun gnosis-dashboard-unmark-all ()
  "Unmark all items in the tabulated-list."
  (interactive)
  (let ((inhibit-read-only t))
    (setq gnosis-dashboard--selected-ids nil)
    (remove-overlays nil nil 'gnosis-mark t)
    (message "All items unmarked")))

(defun gnosis-dashboard-mark-all ()
  "Mark all items in the tabulated-list buffer and collect their IDs."
  (interactive)
  (when (derived-mode-p 'tabulated-list-mode)
    (let ((inhibit-read-only t))
      ;; Clear existing marks
      (remove-overlays (point-min) (point-max) 'gnosis-mark t)
      ;; Apply overlay to the entire buffer at once
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'face 'highlight)
        (overlay-put ov 'gnosis-mark t))
      ;; Set selected IDs
      (setq gnosis-dashboard--selected-ids gnosis-dashboard-note-ids)
      (message "Marked %d items" (count-lines (point-min) (point-max))))))

(defun gnosis-dashboard-marked-delete ()
  "Delete marked note entries."
  (interactive)
  (when (y-or-n-p "Delete selected notes?")
    (cl-loop for note in gnosis-dashboard--selected-ids
	     do (gnosis-delete-note note t))
    (gnosis-dashboard-return)))

(defun gnosis-dashboard-marked-suspend ()
  "Suspend marked note entries."
  (interactive)
  (when (y-or-n-p "Toggle SUSPEND on selected notes?")
    (gnosis-toggle-suspend-notes gnosis-dashboard--selected-ids nil)
    (gnosis-dashboard-return)))

(transient-define-suffix gnosis-dashboard-suffix-query (query)
  "Search for note content for QUERY."
  (interactive "sSearch for note content: ")
  (gnosis-dashboard-output-notes (gnosis-collect-note-ids :query query)))

(transient-define-suffix gnosis-dashboard-suffix-decks ()
  (interactive)
  (gnosis-dashboard-output-decks))

(transient-define-prefix gnosis-dashboard-menu ()
  "Transient buffer for gnosis dashboard interactions."
  [["Actions"
    ("r" "Review" gnosis-review)
    ("a" "Add note" gnosis-add-note)
    ("A" "Add deck" gnosis-add-deck)
    ("q" "Quit" quit-window)
    "\n"]
   ["Notes"
    ("s" "Search" gnosis-dashboard-suffix-query)
    ("n" "Notes" (lambda () (interactive)
		   (gnosis-dashboard-output-notes
		    (gnosis-collect-note-ids))))
    ("d" "Decks" gnosis-dashboard-suffix-decks)
    ("t" "Tags" (lambda () (interactive)
		  (gnosis-dashboard-output-tags)))]
   ["History"
    ("H" "View Review History" gnosis-dashboard-history)]])

;;;###autoload
(defun gnosis-dashboard ()
  "Launch gnosis dashboard."
  (interactive)
  (let* ((buffer (get-buffer-create gnosis-dashboard-buffer-name))
	 (due-log (gnosis-review-get--due-notes))
	 (due-note-ids (mapcar #'car due-log))
	 (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (gnosis-dashboard-mode)
      (insert "\n"
	      (gnosis-center-string
	       (format "%s" (propertize "Gnosis Dashboard" 'face
					'gnosis-face-dashboard-header))))
      (gnosis-insert-separator)
      (insert (gnosis-center-string
	       (format "\nReviewed today: %s (New: %s)"
		       (propertize
			(number-to-string (gnosis-get-date-total-notes))
			'face
			'font-lock-variable-use-face)
		       (propertize
			(number-to-string (gnosis-get-date-new-notes))
			'face
			'font-lock-keyword-face))))
      (insert "\n")
      (insert (gnosis-center-string
	       (format "Due notes: %s (Overdue: %s)"
		       (propertize
			(number-to-string (length due-note-ids))
			'face 'error)
		       (propertize
			(number-to-string
			 (length (gnosis-review-get-overdue-notes)))
			'face 'warning))))
      (insert "\n\n")
      (insert (gnosis-center-string
	       (format "Daily Average: %s"
		       (propertize
			(gnosis-dashboard-output-average-rev)
			'face 'font-lock-type-face))))
      (insert "\n")
      (insert (gnosis-center-string
	       (format "Current streak: %s day(s)"
		       (propertize
			(gnosis-dashboard--streak
			 (gnosis-select 'date 'activity-log '(> reviewed-total 0) t))
			'face 'success))))
      (insert "\n\n"))
    (pop-to-buffer-same-window buffer)
    (goto-char (point-min))
    (gnosis-dashboard-enable-mode)))

;; VC functions ;;
;;;;;;;;;;;;;;;;;;

(defun gnosis--shell-cmd-with-password (command)
  "Run COMMAND and watch for password prompt."
  (let ((process (start-process-shell-command "shell-cmd" nil command)))
    (set-process-filter
     process
     (lambda (proc output)
       (when (string-match-p "password:" output)
         (process-send-string proc
			      (concat (read-passwd "Password: ") "\n")))
       (message "%s" output)))))

;;;###autoload
(cl-defun gnosis-vc-push (&optional (dir gnosis-dir))
  "Run `git push' in DIR."
  (interactive)
  (let ((default-directory dir)
	(git (executable-find "git")))
    (gnosis--shell-cmd-with-password
     (format "%s push" git))))

;;;###autoload
(cl-defun gnosis-vc-pull (&optional (dir gnosis-dir))
  "Run `git pull' in DIR."
  (interactive)
  (let ((default-directory dir))
    (let ((process (async-shell-command (format "%s pull" (executable-find "git")))))
      (lambda (proc event)
	(when (string= event "finished\n")
	  (setf gnosis-db
		(emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))))))))

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
        (add-to-list 'global-mode-string
                     '(:eval
                       (if (and gnosis-due-notes-total (> gnosis-due-notes-total 0))
                           (format " G:%d" gnosis-due-notes-total)
                         "")))
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
