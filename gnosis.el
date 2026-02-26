;;; gnosis.el --- Knowledge System  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; Version: 0.7.0

;; Package-Requires: ((emacs "27.2") (emacsql "4.1.0") (compat "29.1.4.2") (transient "0.7.2") (org-gnosis "0.2.0"))

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

;; Gnosis is a personal knowledge management and review system that
;; integrates a note-taking system with spaced repetition and
;; self-testing.  It works together with org-gnosis, which provides a
;; Zettelkasten-style note-taking system where notes (nodes) are org
;; files indexed in an SQLite database.
;;
;; The intended workflow is:
;;
;; 1. Write notes on a topic using `org-gnosis-find'.
;; 2. Create themata (flashcard-like questions) related to the topic
;;    using `gnosis-add-thema'.
;; 3. Link themata to note topics by inserting org-gnosis links in
;;    the keimenon (question text) or parathema (extra context) using
;;    `org-gnosis-insert'.
;; 4. Review themata with spaced repetition via `gnosis-review', or
;;    review all themata linked to a specific topic via
;;    `gnosis-review-topic'.
;;
;; Gnosis and org-gnosis maintain separate SQLite databases.  The
;; gnosis database stores themata, decks, review history, and links
;; from themata to org-gnosis nodes.  The org-gnosis database stores
;; nodes, tags, and links between nodes.
;;
;; The spaced repetition algorithm is highly adjustable, allowing
;; users to set specific values not just for thema decks but for tags
;; as well, creating a personalized learning environment for each
;; topic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'vc-git)
(require 'emacsql-sqlite)
(require 'emacsql-sqlite-builtin nil t)
(require 'transient)
(require 'animate)

(require 'org)
(require 'org-element)

(require 'gnosis-algorithm)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'org-gnosis)


(defgroup gnosis nil
  "Spaced Repetition System For Thema Taking & Self Testing."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-dir (locate-user-emacs-file "gnosis")
  "Gnosis directory."
  :type 'directory)

(unless (file-directory-p gnosis-dir)
  (make-directory gnosis-dir))

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

(defcustom gnosis-new-themata-limit nil
  "Total new themata limit."
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Number")))

(defcustom gnosis-review-new-first t
  "Review new themata first.

When nil, review new themata last."
  :type 'boolean)

(defcustom gnosis-default-average-review-period 360
  "Number of days of which the average review score will be calculated."
  :type 'integer)

(defcustom gnosis-center-content-during-review t
  "Default value for centering content during review sessions.

This is the global default used when creating new review buffers.
When non-nil, center content during review sessions.
When nil, content will be displayed left-aligned instead of centered."
  :type 'boolean)

(defvar-local gnosis-center-content t
  "Buffer-local variable controlling content centering.

When non-nil, center content in the current buffer.
This is set automatically based on buffer type:
- Review buffers: uses `gnosis-center-content-during-review'
- Dashboard buffers: always t (centered)
- Other buffers: defaults to t")

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

(defvar gnosis-db
  (emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))
  "Gnosis database.")

(autoload 'gnosis-dashboard "gnosis-dashboard" nil t)

(defvar gnosis-cloze-string "(...)")

(defvar gnosis-testing nil
  "Change this to non-nil when running manual tests.")


(defconst gnosis-db-version 4
  "Gnosis database version.")

(defvar gnosis-thema-types
  '(("Basic" . gnosis-add-thema--basic)
    ("MCQ" .  gnosis-add-thema--mcq)
    ("Double" .  gnosis-add-thema--double)
    ("Cloze" . gnosis-add-thema--cloze)
    ("MC-cloze" . gnosis-add-thema--mc-cloze))
  "Mapping of Themata & their respective functions.")

(defvar gnosis-previous-thema-tags '()
  "Tags input from previously added thema.")

(defvar gnosis-previous-thema-hint nil
  "Hint input from previously added thema.")

(defvar gnosis-due-themata-total nil
  "Total due themata.")


;; Review autoloads
(autoload 'gnosis-review "gnosis-review" nil t)
(autoload 'gnosis-review-topic "gnosis-review" nil t)
(autoload 'gnosis-review-get-due-themata "gnosis-review")
(autoload 'gnosis-review-get--due-themata "gnosis-review")
(autoload 'gnosis-review-is-due-p "gnosis-review")
(autoload 'gnosis-review-is-due-today-p "gnosis-review")
(autoload 'gnosis-review-is-thema-new-p "gnosis-review")
(autoload 'gnosis-review-get-overdue-themata "gnosis-review")
(autoload 'gnosis-review-algorithm "gnosis-review")
(autoload 'gnosis-display-next-review "gnosis-review")
(autoload 'gnosis-get-linked-nodes "gnosis-review")
(autoload 'gnosis-monkeytype-start "gnosis-review" nil t)
(autoload 'gnosis-history-clear "gnosis-review" nil t)

;; Export/import autoloads
(autoload 'gnosis-export--insert-thema "gnosis-export-import")
(autoload 'gnosis-export-themata "gnosis-export-import")
(autoload 'gnosis-export-parse-themata "gnosis-export-import")
(autoload 'gnosis-export-parse--deck-name "gnosis-export-import")
(autoload 'gnosis-export-deck "gnosis-export-import" nil t)
(autoload 'gnosis-export-deck-async "gnosis-export-import" nil t)
(autoload 'gnosis-save-thema "gnosis-export-import")
(autoload 'gnosis-save "gnosis-export-import" nil t)
(autoload 'gnosis-save-deck "gnosis-export-import" nil t)
(autoload 'gnosis-import-deck "gnosis-export-import" nil t)
(autoload 'gnosis-import-deck-async "gnosis-export-import" nil t)

(defvar gnosis-export-separator "\n- ")

(defvar gnosis-save-hook nil
  "Hook run after a successful `gnosis-save'.
Each function is called with the saved thema ID (integer).")

(defcustom gnosis-custom-values
  '((:deck "demo" (:proto (0 1 3) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			  :amnesia 0.5 :lethe 3))
    (:tag "demo" (:proto (1 2) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			 :amnesia 0.45 :lethe 3)))
  "Custom review values for adjusting gnosis algorithm.

Each entry is a list of (SCOPE NAME PARAMETERS) where:
- SCOPE is :deck or :tag
- NAME is the deck/tag name string
- PARAMETERS is a plist with keys:
  :proto (list of integers), :anagnosis (integer),
  :epignosis (number), :agnoia (number),
  :amnesia (number 0-1), :lethe (positive integer)"
  :type '(repeat sexp)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (gnosis-validate-custom-values value)
         (set-default symbol value))
  :group 'gnosis)

(defvar gnosis-custom--valid-values
  '(:proto :anagnosis :epignosis :agnoia :amnesia :lethe))

(defvar gnosis-review-editing-p nil
  "Boolean value to check if user is currently in a review edit.")

(defvar gnosis--id-cache nil
  "Hash table of existing thema IDs, bound during batch import.
When non-nil, `gnosis-generate-id' and `gnosis-update-thema' use this
for O(1) lookups instead of querying the database per thema.")

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
 (gnosis-update ='themata ='(= keimenon \"NEW VALUE\") ='(= id 12))"
  (emacsql gnosis-db `[:update ,table :set ,value :where ,where]))

(defun gnosis-get (value table &optional restrictions)
  "Return caar of VALUE from TABLE, optionally with where RESTRICTIONS."
  (caar (gnosis-select value table restrictions)))

(defun gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql gnosis-db `[:delete :from ,table :where ,value]))

(defun gnosis-delete-thema (id &optional verification)
  "Delete thema with ID.

When VERIFICATION is non-nil, skip `y-or-n-p' prompt."
  (when (or verification (y-or-n-p "Delete thema?"))
    (emacsql-with-transaction gnosis-db (gnosis--delete 'themata `(= id ,id)))))

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
	 (dates (cl-loop for d from 0 below days
			 collect (gnosis-algorithm-date (- d))))
	 (review-counts (gnosis-select 'reviewed-total 'activity-log
				       `(and (> reviewed-total 0)
					     (in date ,(vconcat dates)))
				       t)))
    (if review-counts
	(/ (apply #'+ review-counts) (float (length review-counts)))
      0)))

(defun gnosis-shuffle (seq)
  "Shuffle SEQ."
  (cl-loop with len = (length seq)
           for i from len downto 2
           do (let ((j (random i)))  ; Get random index < i.
                (cl-rotatef (nth (1- i) seq) (nth j seq)))  ; Swap elements.
           finally return seq))

(defun gnosis-completing-read (prompt seq &optional require-match)
  "Call `gnosis-completing-read-function' with shuffled SEQ.

PROMPT: Prompt for `gnosis-completing-read-function'
History is disabled."
  (let ((history-add-new-input nil))
    (funcall gnosis-completing-read-function prompt
	     (gnosis-shuffle (copy-sequence seq)) nil require-match)))

(defun gnosis-insert-separator ()
  "Insert a dashed line separator.

Width depends on `gnosis-center-content':
- When non-nil, spans the entire window width.
- When nil, uses `fill-column' width."
  (let* ((width (if gnosis-center-content
                    (window-width)
                  fill-column))
         (dash-line (concat (make-string width ?-))))
    (insert "\n" dash-line "\n")
    ;; Apply an overlay to hide only the dashes
    (let ((start (save-excursion (forward-line -1) (point)))
          (end (point)))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face 'gnosis-face-separator)
        (overlay-put overlay 'display (make-string width ?\s))))))

(defun gnosis-center-current-line ()
  "Centers text in the current line ignoring leading spaces."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (text (string-trim (buffer-substring start end)))
         (padding (max (/ (- (window-width) (length text)) 2) 0)))
    (delete-region start end)
    (insert (make-string padding ? ) text)))

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

(defun gnosis-format-string (str)
  "Format STR for display, optionally centering based on buffer preference.

When `gnosis-center-content' is non-nil, centers the text.
Otherwise, just processes org-links without centering."
  (if gnosis-center-content
      (gnosis-center-string str)
    (replace-regexp-in-string
     "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
     "\\2"
     str)))

(defun gnosis-apply-center-buffer-overlay (&optional point)
  "Center text in buffer starting at POINT using `gnosis-center-current-line'.
This will not be applied to sentences that start with double space.

Respects `gnosis-center-content' buffer-local setting."
  (when gnosis-center-content
    (save-excursion
      (goto-char (or point (point-min)))
      (while (not (or (= (point-max) (point)) (looking-at "^  ")))
        (gnosis-center-current-line)
        (forward-line 1)))))

(defun gnosis-org-format-string (str)
  "Return STR fontified as in `org-mode'."
  (with-temp-buffer
    (org-mode)
    (insert str)
    (font-lock-ensure)
    (buffer-string)))

(defun gnosis-cloze-create (str clozes &optional cloze-string)
  "Replace CLOZES in STR with CLOZE-STRING, preserving whitespace pattern."
  (cl-assert (listp clozes) nil "Adding clozes: Clozes need to be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert (gnosis-org-format-string str))
      (dolist (cloze clozes)
        (let* ((cloze-text (gnosis-utils-trim-quotes cloze))
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
                 (replace-match (propertize (format "(%s)" hint)
					    'face 'gnosis-face-cloze))
                 (goto-char (match-end 0)))) ; Move point to end of match
      (buffer-string))))

(defun gnosis-cloze-mark-false (str answers)
  "Mark contents of STR as false for ANSWERS.

First item of answers will be marked as false, while the rest unanswered."
  (let* ((false (car answers))
	 (unanswered (cdr answers))
         (str-with-false (and answers
			      (gnosis-utils-highlight-words str (list false)
							 'gnosis-face-false)))
	 final)
    (if unanswered
	(setq final (gnosis-utils-highlight-words str-with-false
					       (if (listp unanswered) unanswered
						 (list unanswered))
					       'gnosis-face-unanswered))
      (setq final (or str-with-false str)))
    final))

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
  "Return thema id for DECK.

If DECK does not exist, create it."
  (cl-assert (stringp deck) nil "DECK must be a string.")
  (let* ((deck (or deck (gnosis--get-deck-name)))
	 (deck-id (gnosis-select 'id 'decks `(= name ,deck) t)))
    (if deck-id (car deck-id)
      (gnosis-add-deck deck)
      (gnosis-get-deck-id deck))))

(defun gnosis-get-thema-deck-name (id)
  "Return deck name of thema ID."
  (let ((deck (gnosis-get 'deck-id 'themata `(= id ,id))))
    (and deck (gnosis--get-deck-name deck))))

(defun gnosis-get-deck--thema (id &optional name)
  "Get deck id for thema ID.

If NAME is t, return name of deck."
  (let* ((id-clause `(= id ,id))
	 (deck (gnosis-get 'deck-id 'themata id-clause)))
    (if name (gnosis--get-deck-name deck) deck)))

(cl-defun gnosis-toggle-suspend-themata (ids &optional verification)
  "Toggle Suspend value for themata IDS.

When VERIFICATION is non-nil, skips `y-or-n-p' prompt."
  (cl-assert (listp ids) nil "IDS value needs to be a list.")
  (let* ((items-num (length ids))
         (suspended (and (= items-num 1)
                         (= (gnosis-get 'suspend 'review-log `(= id ,(car ids))) 1)))
         (verification
          (or verification
              (cond ((= items-num 1)
                     (y-or-n-p
                      (if suspended "Unsuspend thema? " "Suspend thema? ")))
                    (t (y-or-n-p
                        (format "Toggle suspend value for %s items? " items-num)))))))
    (when verification
      (emacsql gnosis-db
               [:update review-log
                :set (= suspend (- 1 suspend))
                :where (in id $v1)]
               (vconcat ids)))))

(cl-defun gnosis-suspend-deck (&optional (deck (gnosis--get-deck-id)))
  "Suspend all thema(s) with DECK id.

When called with a prefix, unsuspends all themata in deck."
  (let* ((themata (gnosis-select 'id 'themata `(= deck-id ,deck) t))
	 (suspend (if current-prefix-arg 0 1))
	 (confirm
	  (y-or-n-p
	   (if (= suspend 0)
	       "Unsuspend all themata for deck? " "Suspend all themata for deck? "))))
    (when confirm
      (emacsql gnosis-db `[:update review-log :set (= suspend ,suspend) :where
				   (in id ,(vconcat themata))])
      (if (equal suspend 0)
	  (message "Unsuspended %s themata" (length themata))
	(message "Suspended %s themata" (length themata))))))

(defun gnosis-generate-id (&optional length deck-p)
  "Generate a unique gnosis ID.

Default to generating a thema id, when DECK-P is t generates a deck id.
When `gnosis--id-cache' is bound, uses hash table lookup instead of DB query.

LENGTH: length of id, default to a random number between 10-15."
  (let* ((length (or length (+ (random 5) 10)))
         (max-val (expt 10 length))
         (min-val (expt 10 (1- length)))
         (id (+ (random (- max-val min-val)) min-val))
	 (exists (if (and gnosis--id-cache (not deck-p))
		     (gethash id gnosis--id-cache)
		   (member id (if deck-p (gnosis-select 'id 'decks nil t)
				(gnosis-select 'id 'themata nil t))))))
    (if exists
        (gnosis-generate-id length)
      (when gnosis--id-cache
        (puthash id t gnosis--id-cache))
      id)))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'hypothesis 'themata `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (gnosis-completing-read "Answer: " choices)))

(defun gnosis-cloze-check (sentence clozes)
  "Return t if all CLOZES are found in SENTENCE."
  (cl-every (lambda (cloze)
              (string-match-p
               (regexp-quote
	        (gnosis-utils-trim-quotes cloze))
               sentence))
            clozes))
;; TODO: use a better name to indicate that it also removes hints from STRING.
(defun gnosis-cloze-remove-tags (string)
  "Replace cloze tags and hints in STRING.

Works with both single (:), double colons (::), single braces ({}) and
double braces ({{}}).

Also removes content after a double semicolon (::), which indicate a hint."
  (let* ((regex "{\\{1,2\\}c[0-9]+:\\{1,2\\}\\(.*?\\)\\(::[^{}]*\\)?}\\{1,2\\}")
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
						     (gnosis-utils-trim-quotes str1))))
         (normalized-str2 (downcase
			   (replace-regexp-in-string "\\s-" ""
						     (gnosis-utils-trim-quotes str2))))
         (max-length (max (length normalized-str1) (length normalized-str2))))
    (if (> max-length gnosis-string-difference)
        (<= (string-distance normalized-str1 normalized-str2) gnosis-string-difference)
      (string= normalized-str1 normalized-str2))))

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (cl-loop for tags in (apply 'append
			      (emacsql gnosis-db [:select :distinct tags :from themata]))
           nconc tags into all-tags
           finally return (delete-dups all-tags)))

(defun gnosis-collect-tag-thema-ids (tags &optional ids)
  "Collect thema IDS for TAGS."
  (cl-assert (listp tags))
  (if (null tags) ids
    (gnosis-collect-tag-thema-ids (cdr tags)
                                 (append ids (gnosis-get-tag-themata (car tags))))))

(defun gnosis-select-by-tag (input-tags &optional due suspended-p)
  "Return thema ID's for every thema with INPUT-TAGS.

If DUE, return only due themata.
If SUSPENDED-P, return suspended themata as well."
  (cl-assert (listp input-tags) t "Input tags must be a list")
  (cl-assert (booleanp due) "Due value must be a boolean")
  (let ((ids (gnosis-collect-tag-thema-ids input-tags)))
    ;; Filter the collected IDs based on due and suspension status
    (cl-loop for id in ids
             when (and (or (not suspended-p) (not (gnosis-suspended-p id)))
                       (if due (gnosis-review-is-due-p id) t))
             collect id)))

(defun gnosis-get-tag-themata (tag)
  "Return thema ids for TAG."
  (let ((themata (gnosis-select 'id 'themata `(like tags ',(format "%%\"%s\"%%" tag)) t)))
    themata))

(defun gnosis-suspended-p (id)
  "Return t if thema with ID is suspended."
  (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1))

(defun gnosis-get-deck-themata (&optional deck-id due)
  "Return themata for deck, with value of DECK-ID.

If DUE is t, return only due themata."
  (let ((themata (gnosis-select 'id 'themata `(= deck-id ,(or deck-id (gnosis--get-deck-id)))
				t)))
    (if due
	(cl-loop for thema in themata
		 when (and (not (gnosis-suspended-p thema))
			   (gnosis-review-is-due-p thema))
		 collect thema)
      themata)))

(defun gnosis--date-to-int (date)
  "Convert DATE list (year month day) to YYYYMMDD integer for fast comparison."
  (+ (* (nth 0 date) 10000) (* (nth 1 date) 100) (nth 2 date)))

(defun gnosis-past-or-present-p (date)
  "Compare the input DATE with the current date.
Return t if DATE is today or in the past, nil if it's in the future.
DATE is a list of the form (year month day)."
  (<= (gnosis--date-to-int date) (gnosis--date-to-int (gnosis-algorithm-date))))

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
  "Tag prompt for adding themata.

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
	(setf gnosis-previous-thema-tags input)
        (org-set-tags (append input current-tags))))))

(defun gnosis-tags-refresh ()
  "Refresh tags value."
  (let ((tags (gnosis-get-tags--unique)))
    ;; Delete all values from tags table.
    (gnosis--delete 'tags nil)
    ;; Insert all unique tags from themata.
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
    (cl-loop for thema in (gnosis-get-tag-themata tag)
	     do (let* ((tags (car (gnosis-select '[tags] 'themata `(= id ,thema) t)))
		       (new-tags (cl-substitute new-tag tag tags :test #'string-equal)))
		  (gnosis-update 'themata `(= tags ',new-tags) `(= id ,thema))))
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
(cl-defun gnosis-collect-thema-ids (&key (tags nil) (due nil) (deck nil) (query nil))
  "Return list of thema ids based on TAGS, DUE, DECKS, QUERY.

TAGS: boolean value, t to specify tags.
DUE: boolean value, t to specify due themata.
DECK: Integer, specify deck id.
QUERY: String value."
  (cl-assert (and (booleanp due) (booleanp tags)
		  (or (numberp deck) (null deck))
		  (or (stringp query) (null query)))
	     nil "Incorrect value passed to `gnosis-collect-thema-ids'")
  (cond ((and (null tags) (null due) (null deck) (null query))
	 (gnosis-select 'id 'themata nil t))
	;; All due themata
	((and (null tags) due (null deck))
	 (gnosis-review-get-due-themata))
	;; All themata for tags
	((and tags (null due) (null deck))
	 (gnosis-select-by-tag (gnosis-tags--prompt :require-match t)))
	;; All due themata for tags
	((and tags due (null deck))
	 (gnosis-select-by-tag (gnosis-tags--prompt :require-match t) t))
	;; All themata for deck
	((and (null tags) (null due) deck)
	 (gnosis-get-deck-themata deck nil))
	;; All due themata for deck
	((and (null tags) deck due)
	 (gnosis-get-deck-themata deck t))
	;; Query
	((and (null tags) (null due) (null deck) query)
	 (gnosis-search-thema query))))


(defun gnosis-get-themata-by-reviews (max-reviews &optional thema-ids)
  "Return thema IDs with at most MAX-REVIEWS total reviews.
When THEMA-IDS is non-nil, restrict to that subset."
  (gnosis-select 'id 'review-log
                 (if thema-ids
                     `(and (<= n ,max-reviews)
                           (in id ,(vconcat thema-ids)))
                   `(<= n ,max-reviews))
                 t))


(defun gnosis-add-thema-fields (deck-id type keimenon hypothesis answer
				       parathema tags suspend links
				       &optional review-image gnosis-id)
  "Insert fields for new thema.

DECK-ID: Deck ID for new thema.
TYPE: Thema type e.g \"mcq\"
KEIMENON: Thema's keimenon
HYPOTHESIS: Thema hypothesis, e.g choices for mcq for OR hints for
cloze/basic thema
ANSWER: Correct answer for thema, for MCQ is an integer while for
cloze/basic a string/list of the right answer(s)
PARATHEMA: Parathema information to display after the answer
TAGS: Tags to organize themata
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
      (gnosis--insert-into 'themata `([,gnosis-id ,(downcase type) ,keimenon ,hypothesis
					      ,answer ,tags ,deck-id]))
      (gnosis--insert-into 'review  `([,gnosis-id ,gnosis-algorithm-gnosis-value
						,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,gnosis-id ,(gnosis-algorithm-date)
						   ,(gnosis-algorithm-date) 0 0 0 0
						   ,suspend 0]))
      (gnosis--insert-into 'extras `([,gnosis-id ,parathema ,review-image]))
      (cl-loop for link in links
	       do (gnosis--insert-into 'links `([,gnosis-id ,link]))))))

(defun gnosis-update-thema (id keimenon hypothesis answer parathema tags links
			      &optional deck-id type)
  "Update thema entry for ID.

If gnosis ID does not exist, create it anew.
When `gnosis--id-cache' is bound, uses hash table for existence check."
  (let ((id (if (stringp id) (string-to-number id) id)))
    (if (if gnosis--id-cache
	    (gethash id gnosis--id-cache)
	  (member id (gnosis-select 'id 'themata nil t)))
	(emacsql-with-transaction gnosis-db
	  (gnosis-update 'themata `(= keimenon ,keimenon) `(= id ,id))
	  (gnosis-update 'themata `(= hypothesis ',hypothesis) `(= id ,id))
	  (gnosis-update 'themata `(= answer ',answer) `(= id ,id))
	  (gnosis-update 'extras `(= parathema ,parathema) `(= id ,id))
	  (gnosis-update 'themata `(= tags ',tags) `(= id ,id))
	  (when type
	    (gnosis-update 'themata `(= type ,type) `(= id ,id)))
	  (gnosis--delete 'links `(= source ,id))
	  (cl-loop for link in links
		   do (gnosis--insert-into 'links `([,id ,link]))))
      (message "Gnosis with id: %d does not exist, creating anew." id )
      (gnosis-add-thema-fields deck-id type keimenon hypothesis answer parathema tags
			      0 links nil id))))

;;;;;;;;;;;;;;;;;;;;;; THEMA HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These functions provide assertions depending on the type of thema.
;;
;; Each thema should use a helper function that calls to provide
;; assertions, such as length of hypothesis and answer, for said
;; thema.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gnosis-add-thema--basic (id deck-id type keimenon hypothesis
				   answer parathema tags suspend links)
  "Default format for adding a thema.

ID: Thema ID.
DECK-ID: Integer value of deck-id.
TYPE: String representing the type of thema.
KEIMENON: String for the thema text.
HYPOTHESIS: List of a single string.
ANSWER: List of a single string.
PARATHEMA: String for the parathema text.
TAGS: List of thema tags.
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
	     nil "Answer value must be a list of a single item")
  (cl-assert (listp tags) nil "Tags must be a list.")
  (cl-assert (or (= suspend 0)
		 (= suspend 1))
	     nil "Suspend value must either 0 or 1")
  (cl-assert (listp links) nil "Links must be a list")
  (if (equal id "NEW")
      (gnosis-add-thema-fields deck-id type keimenon (or hypothesis (list ""))
			       answer parathema tags suspend links)
    (gnosis-update-thema id keimenon hypothesis answer parathema tags links deck-id type)))

(defun gnosis-add-thema--double (id deck-id type keimenon hypothesis
				    answer parathema tags suspend links)
  "Double thema format.

Changes TYPE to basic & inserts a second basic thema with ANSWER
and KEIMENON reversed."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (stringp type) nil "Type must be a string.")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (listp hypothesis) nil "Hypothesis value must be a list.")
  (cl-assert (and (listp answer) (= (length answer) 1))
	     nil "Answer value must be a list of a single item")
  (cl-assert (listp tags) nil "Tags must be a list.")
  (cl-assert (or (= suspend 0) (= suspend 1)) nil "Suspend value must either 0 or 1")
  (cl-assert (listp links) nil "Links must be a list")
  ;; Change type to basic
  (let ((type "basic")
	(hypothesis (or hypothesis (list ""))))
    (if (equal id "NEW")
	(progn
	  (gnosis-add-thema-fields deck-id type keimenon hypothesis
				   answer parathema tags suspend links)
	  (gnosis-add-thema-fields deck-id type (car answer) hypothesis
				   (list keimenon) parathema tags suspend links))
      ;; There should not be a double type thema in database to
      ;; update.  This is used for testing purposes.
      (gnosis-update-thema id keimenon hypothesis answer parathema tags links deck-id type))))

(defun gnosis-add-thema--mcq (id deck-id type keimenon hypothesis
				answer parathema tags suspend links)
  "Helper function for MCQ thema type.

Provide assertions for MCQ type themata.

DECK-ID: ID for deck.
ID: Integer for thema ID.
TYPE: String for type, must be \"mcq\".
HYPOTHESIS: List of strings or nil, hypothesis in MCQ thema types
serve as choices to select from.
ANSWER: List of one time, the right answer.  Must be member of
HYPOTHESIS.
TAGS: List of tags.
PARATHEMA: Parathema for THEMA.
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
      (gnosis-add-thema-fields deck-id type keimenon (or hypothesis (list ""))
			      answer parathema tags suspend links)
    (gnosis-update-thema id keimenon hypothesis answer parathema tags links deck-id type)))

(defun gnosis-add-thema--cloze (id deck-id type keimenon hypothesis
				  answer parathema tags suspend links)
  "Helper for cloze type themata.

Provide assertions for cloze type themata.

DECK-ID: ID for deck.
ID: Integer for thema ID.
TYPE: String for type, must be \"cloze\".
HYPOTHESIS: List of strings or nil, hypothesis in cloze thema types
serve as hints.
ANSWER: List of answers for clozes.
TAGS: List of tags.
PARATHEMA: Parathema for thema.
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
  (let ((keimenon-clean (gnosis-cloze-remove-tags keimenon)))
    (if (equal id "NEW")
	(if (null answer)
	    ;; if answer is left null, extract all contents from keimenon.
	    (let* ((contents (gnosis-cloze-extract-contents keimenon))
		   (clozes (gnosis-cloze-extract-answers contents))
		   (hints (gnosis-cloze-extract-hints contents)))
	      (cl-loop for cloze in clozes
		       for hint in hints
		       do
		       (gnosis-add-thema-fields deck-id type keimenon-clean hint cloze parathema
						tags suspend links)))
	  (gnosis-add-thema-fields deck-id type keimenon-clean (or hypothesis (list ""))
				   answer parathema tags suspend links))
      (gnosis-update-thema id keimenon-clean hypothesis answer parathema tags links deck-id type))))

(defun gnosis-add-thema--mc-cloze (id deck-id type keimenon hypothesis
				  answer parathema tags suspend links)
  "Helper for mc-cloze type themata.

Provide assertions for mc-cloze type themata.

DECK-ID: ID for deck.
ID: Integer for thema ID.
TYPE: String for type, must be \"mc-cloze\".
HYPOTHESIS: List of strings or nil, hypothesis in mc-cloze thema types
serve as answer options.
ANSWER: List of answers for mc-clozes.
TAGS: List of tags.
PARATHEMA: Parathema for thema.
SUSPEND: integer value, 1 or 0.
LINKS: list of strings."
  (cl-assert (integerp deck-id) nil "Deck-id value must be an integer.")
  (cl-assert (string= type "mc-cloze") nil "TYPE value must be \"mc-cloze\" .")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (and (listp hypothesis)
		  (> (length hypothesis) (length answer)))
	     nil "Hypothesis value must be a list, greater in length of ANSWER.")
  (cl-assert (and (listp answer) (length= answer 1)
		  (member (car answer) hypothesis))
	     nil
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
      (gnosis-add-thema-fields deck-id type keimenon (or hypothesis (list ""))
			      answer parathema tags suspend links)
    (gnosis-update-thema id keimenon hypothesis answer parathema tags links deck-id type)))

;;;###autoload
(defun gnosis-add-thema (deck type &optional keimenon hypothesis
			      answer parathema tags example)
  "Add thema with TYPE in DECK."
  (interactive (list
		(gnosis--get-deck-name)
		(downcase (completing-read "Select type: " gnosis-thema-types))))
  (window-configuration-to-register :gnosis-edit)
  (pop-to-buffer "*Gnosis NEW*")
  (with-current-buffer "*Gnosis NEW*"
    (let ((inhibit-read-only 1))
      (erase-buffer))
    (insert "#+DECK: " deck)
    (gnosis-edit-mode)
    (gnosis-export--insert-thema "NEW" type keimenon hypothesis
				answer parathema tags example))
  (search-backward "keimenon")
  (forward-line))

(defun gnosis-edit-thema (id)
  "Edit thema with ID."
  (window-configuration-to-register :gnosis-edit)
  (with-current-buffer (pop-to-buffer "*Gnosis Edit*")
    (let ((inhibit-read-only 1)
	  (deck-name (gnosis--get-deck-name
		      (gnosis-get 'deck-id 'themata `(= id ,id)))))
      (erase-buffer)
      (insert "#+DECK: " deck-name))
    (gnosis-edit-mode)
    (gnosis-export-themata (list id))
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
  "C-c C-o" #'org-gnosis-goto-id
  "C-c C-k" #'gnosis-edit-quit)

(define-derived-mode gnosis-edit-mode org-mode "Gnosis Org"
  "Gnosis Org Mode."
  :interactive nil
  :lighter " Gnosis Edit"
  :keymap gnosis-edit-mode-map
  (setq header-line-format
	(substitute-command-keys
	 " Save thema by running \\[gnosis-save] or \\[gnosis-edit-quit] to quit")))

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
  "Return custom VALUE for thema DECK."
  (plist-get (gnosis-get-custom-values :deck deck values) value))

(defun gnosis-get-custom-tag-values (id keyword &optional custom-tags custom-values)
  "Return KEYWORD values for thema ID."
  (cl-assert (keywordp keyword) nil "keyword must be a keyword!")
  (let ((tags (if id (gnosis-get 'tags 'themata `(= id ,id)) custom-tags)))
    (cl-loop for tag in tags
	     ;; Only collect non-nil values
	     when (plist-get (gnosis-get-custom-values :tag tag custom-values) keyword)
	     collect (plist-get (gnosis-get-custom-values :tag tag custom-values)
				keyword))))

(defun gnosis-get-thema-tag-amnesia (id &optional custom-tags custom-values)
  "Return tag MINIMUM amnesia for thema ID.

The closer the amnesia value is to 0, the closer it is to total
amnesia i.e next interval to be 0.

CUSTOM-TAGS: Specify tags for thema id.
CUSTOM-VALUES: Specify values for tags."
  (let ((amnesia-values (gnosis-get-custom-tag-values id :amnesia
						      custom-tags custom-values)))
    (and amnesia-values (apply #'max amnesia-values))))

(defun gnosis-get-thema-deck-amnesia (id &optional custom-deck custom-values)
  "Return tag amnesia for thema ID.

Optionally, use CUSTOM-DECK and CUSTOM-VALUES."
  (let ((deck (or (gnosis-get-thema-deck-name id) custom-deck )))
    (or (gnosis-get-custom-deck-value deck :amnesia custom-values)
	gnosis-algorithm-amnesia-value)))

(defun gnosis-get-thema-amnesia (id &optional custom-deck custom-tags custom-values )
  "Return amnesia value for thema ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-TAGS: Specify custom tags for thema id.
CUSTOM-VALUES: Specify custom values."
  (let* ((deck-amnesia (gnosis-get-thema-deck-amnesia id custom-deck custom-values))
         (tags-amnesia (gnosis-get-thema-tag-amnesia id custom-tags custom-values))
	 (thema-amnesia (or tags-amnesia deck-amnesia)))
    (if (>= thema-amnesia 1)
	(error "Amnesia value must be lower than 1")
      thema-amnesia)))

(defun gnosis-get-thema-tag-epignosis (id &optional custom-tags custom-values)
  "Return tag epignosis for thema ID.

CUSTOM-TAGS: Specify custom tags for thema id.
CUSTOM-VALUES: Specify custom values."
  (let* ((epignosis-values (gnosis-get-custom-tag-values id :epignosis custom-tags custom-values)))
    (and epignosis-values (apply #'max epignosis-values))))

(defun gnosis-get-thema-deck-epignosis (id &optional custom-deck custom-values)
  "Return deck epignosis for thema ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-VALUES: Specify custom values."
  (let ((deck (or (gnosis-get-thema-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :epignosis custom-values)
	gnosis-algorithm-epignosis-value)))

(defun gnosis-get-thema-epignosis (id &optional custom-deck custom-tags custom-values)
  "Return epignosis value for thema ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-TAGS: Specify custom tags for thema id.
CUSTOM-VALUES: Specify custom values."
  (let* ((deck-epignosis (gnosis-get-thema-deck-epignosis id custom-deck custom-values))
         (tag-epignosis (gnosis-get-thema-tag-epignosis id custom-tags custom-values))
	 (thema-epignosis (or tag-epignosis deck-epignosis)))
    (if (>= thema-epignosis 1)
	(error "Epignosis value must be lower than 1")
      thema-epignosis)))

(defun gnosis-get-thema-tag-agnoia (id &optional custom-tags custom-values)
  "Return agnoia value for thema ID.

CUSTOM-TAGS: Specify custom tags for thema id.
CUSTOM-VALUES: Specify custom values."
  (let ((agnoia-values (gnosis-get-custom-tag-values id :agnoia custom-tags custom-values)))
    (and agnoia-values (apply #'max agnoia-values))))

(defun gnosis-get-thema-deck-agnoia (id &optional custom-deck custom-values)
  "Return agnoia value for thema ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-VALUES: Specify custom values."
  (let ((deck (or (gnosis-get-thema-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :agnoia custom-values)
	gnosis-algorithm-agnoia-value)))

(defun gnosis-get-thema-agnoia (id &optional custom-deck custom-tags custom-values)
  "Return agnoia value for thema ID.

CUSTOM-DECK: Specify custom deck.
CUSTOM-TAGS: Specify custom tags for thema id.
CUSTOM-VALUES: Specify custom values."
  (let* ((deck-agnoia (gnosis-get-thema-deck-agnoia id custom-deck custom-values))
         (tag-agnoia (gnosis-get-thema-tag-agnoia id custom-tags custom-values))
	 (thema-agnoia (or tag-agnoia deck-agnoia)))
    (if (>= thema-agnoia 1)
	(error "Agnoia value must be lower than 1")
      thema-agnoia)))

(defun gnosis-proto-max-values (proto-values)
  "Return max values from PROTO-VALUES."
  (if (not (and (listp proto-values) (cl-every #'listp proto-values)))
      proto-values
    (let* ((max-len (apply #'max (mapcar #'length proto-values)))
           (padded-lists (mapcar (lambda (lst)
                                   (append lst (make-list (- max-len (length lst)) 0)))
                                 proto-values)))
      (apply #'cl-mapcar #'max padded-lists))))

(defun gnosis-get-thema-proto (id &optional custom-tags custom-deck custom-values)
  "Return tag proto values for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let* ((deck (or custom-deck (gnosis-get-thema-deck-name id)))
	 (tags-proto (gnosis-get-custom-tag-values id :proto custom-tags custom-values))
	 (decks-proto (gnosis-get-custom-deck-value deck :proto custom-values)))
    (if tags-proto (gnosis-proto-max-values tags-proto)
      (gnosis-proto-max-values (or decks-proto gnosis-algorithm-proto)))))

(defun gnosis-get-thema-tag-anagnosis (id &optional custom-tags custom-values)
  "Return the minimum anagnosis tag value for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead."
  (let ((anagnosis-values (gnosis-get-custom-tag-values id :anagnosis custom-tags custom-values)))
    (and anagnosis-values (apply #'min anagnosis-values))))

(defun gnosis-get-thema-deck-anagnosis (id &optional custom-deck custom-values)
  "Return anagnosis deck value for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let ((deck (or (gnosis-get-thema-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :anagnosis custom-values)
	gnosis-algorithm-anagnosis-value)))

(defun gnosis-get-thema-anagnosis (id &optional custom-deck custom-tags custom-values)
  "Return minimum anagnosis value for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let* ((deck-anagnosis (gnosis-get-thema-deck-anagnosis id custom-deck custom-values))
	 (tag-anagnosis (gnosis-get-thema-tag-anagnosis id custom-tags custom-values))
	 (thema-anagnosis (or tag-anagnosis deck-anagnosis)))
    thema-anagnosis))

(defun gnosis-get-thema-deck-lethe (id &optional custom-deck custom-values)
  "Return lethe deck value for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let ((deck (or (gnosis-get-thema-deck-name id) custom-deck)))
    (or (gnosis-get-custom-deck-value deck :lethe custom-values)
	gnosis-algorithm-lethe-value)))

(defun gnosis-get-thema-tag-lethe (id &optional custom-tags custom-values)
  "Return thema ID tag lethe values.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead."
  (let ((lethe-values (gnosis-get-custom-tag-values id :lethe custom-tags custom-values)))
    (and lethe-values (apply #'min lethe-values))))

(defun gnosis-get-thema-lethe (id &optional custom-deck custom-tags custom-values)
  "Return thema ID lethe value.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead.
CUSTOM-DECK: Custom deck to be used instead."
  (let* ((deck-lethe (gnosis-get-thema-deck-lethe id custom-deck custom-values))
	 (tag-lethe (gnosis-get-thema-tag-lethe id custom-tags custom-values))
	 (thema-lethe (or tag-lethe deck-lethe)))
    thema-lethe))

(defun gnosis-get-date-total-themata (&optional date)
  "Return total themata reviewed for DATE.

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

(defun gnosis-get-date-new-themata (&optional date)
  "Return total themata reviewed for DATE.

Defaults to current date."
  (cl-assert (listp date) nil "Date must be a list.")
  (let* ((date (or date (gnosis-algorithm-date)))
	 (reviewed-new (or (car (gnosis-select 'reviewed-new 'activity-log `(= date ',date) t))
			   0)))
    reviewed-new))
;; TODO: Auto tag overdue tags.
(defun gnosis-tags--append (id tag)
  "Append TAG to the list of tags of thema ID."
  (cl-assert (numberp id) nil "ID must be the thema id number")
  (cl-assert (stringp tag) nil "Tag must a string")
  (let* ((current-tags (gnosis-get 'tags 'themata `(= id ,id)))
	 (new-tags (append current-tags (list tag))))
    (gnosis-update 'themata `(= tags ',new-tags) `(= id ,id))))

(defun gnosis-search-thema (&optional query)
  "Search for thema QUERY.

Return thema ids for themata that match QUERY."
  (cl-assert (or (stringp query) (eq query nil)))
  (let* ((query (or query (read-string "Search for thema: ")))
         (words (split-string query))
         (clause-keimenon `(and ,@(mapcar (lambda (word)
					`(like keimenon ,(format "%%%s%%" word)))
                                      words)))
	 (clause-answer `(and ,@(mapcar (lambda (word)
					  `(like answer ,(format "%%%s%%" word)))
					words))))
    (append (gnosis-select 'id 'themata clause-keimenon t)
	    (gnosis-select 'id 'themata clause-answer t))))

;;; Database Schemas
(defconst gnosis-db--schemata
  '((decks
     ([(id integer :primary-key :autoincrement)
       (name text :not-null)]
      (:unique [name])))
    (themata
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
     ([(id integer :primary-key :not-null) ;; thema-id
       (gnosis integer :not-null)
       (amnesia integer :not-null)]
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (review-log
     ([(id integer :primary-key :not-null) ;; thema-id
       (last-rev integer :not-null)  ;; Last review date
       (next-rev integer :not-null)  ;; Next review date
       (c-success integer :not-null) ;; Consecutive successful reviews
       (t-success integer :not-null) ;; Total successful reviews
       (c-fails integer :not-null)   ;; Consecutive failed reviewss
       (t-fails integer :not-null)   ;; Total failed reviews
       (suspend integer :not-null)   ;; Binary value, 1=suspended
       (n integer :not-null)]        ;; Number of reviews
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (activity-log
     ([(date text :not-null)
       (reviewed-total integer :not-null)
       (reviewed-new integer :not-null)]))
    (extras
     ([(id integer :primary-key :not-null)
       (parathema string)
       (review-image string)]
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (tags
     ([(tag text :primary-key)]
      (:unique [tag])))
    (links
     ([(source text)
       (dest text)]
      (:foreign-key [source] :references themata [id]
		    :on-delete :cascade)
      (:unique [source dest])))))

(defun gnosis-update--make-list (column)
  "Make COLUMN values into a list."
  (let ((results (emacsql gnosis-db `[:select [id ,column] :from themata])))
    (dolist (row results)
      (let ((id (car row))
            (old-value (cadr row)))
	;; Update each entry, converting the value to a list representation
	(unless (listp old-value)
	  (emacsql gnosis-db `[:update themata
				       :set (= ,column $s1)
				       :where (= id $s2)]
		   (list old-value)
		   id)
	  (message "Update Thema: %d" id))))))

(defun gnosis-db-update-v4 ()
  "Update to databse version v4."
  (let ((tags (gnosis-get-tags--unique)))
    (pcase-dolist (`(,table ,schema) (seq-filter (lambda (schema)
						   (member (car schema) '(links tags)))
						 gnosis-db--schemata))
      (emacsql gnosis-db [:create-table :if-not-exists $i1 $S2] table schema))
    (cl-loop for tag in tags
	     do (gnosis--insert-into 'tags `[,tag]))
    (emacsql gnosis-db [:alter-table themata :rename-column main :to keimenon])
    (emacsql gnosis-db [:alter-table themata :rename-column options :to hypothesis])
    (emacsql gnosis-db [:alter-table extras :rename-column extra-themata :to parathema])
    (emacsql gnosis-db [:alter-table extras :rename-column images :to review-image])
    (emacsql gnosis-db [:alter-table extras :drop-column extra-image])
    ;; Make sure all hypothesis & answer values are lists
    (gnosis-update--make-list 'hypothesis)
    (gnosis-update--make-list 'answer)
    ;; Fix MCQs
    (cl-loop for thema in (gnosis-select 'id 'themata '(= type "mcq") t)
	     do (funcall
		 (lambda (id)
		   (let* ((data (gnosis-select '[hypothesis answer] 'themata `(= id ,id) t))
			  (hypothesis (nth 0 data))
			  (old-answer (car (nth 1 data)))
			  (new-answer (when (integerp old-answer)
					(list (nth (- 1 old-answer) hypothesis)))))
		     (when (integerp old-answer)
		       (gnosis-update 'themata `(= answer ',new-answer) `(= id ,id)))))
		 thema))
    ;; Replace y-or-n with MCQ
    (cl-loop for thema in (gnosis-select 'id 'themata '(= type "y-or-n") t)
	     do (funcall (lambda (id)
			   (let ((data (gnosis-select '[type hypothesis answer]
						      'themata `(= id ,id) t)))
			     (when (string= (nth 0 data) "y-or-n")
			       (gnosis-update 'themata '(= type "mcq") `(= id ,id))
			       (gnosis-update 'themata '(= hypothesis '("Yes" "No"))
					      `(= id ,id))
			       (if (= (car (nth 2 data)) 121)
				   (gnosis-update 'themata '(= answer '("Yes"))
						  `(= id ,id))
				 (gnosis-update 'themata '(= answer '("No"))
						`(= id ,id))))))
			 thema))
    ;; Replace - with _, org does not support tags with dash.
    (cl-loop for tag in (gnosis-get-tags--unique)
	     ;; Replaces dashes to underscores.
	     if (string-match-p "-" tag)
	     do (gnosis-tag-rename tag (replace-regexp-in-string "-" "_" tag)))))

(defun gnosis-db-update-v5 ()
  "Update database v5."
  (emacsql gnosis-db [:alter-table notes :rename-to themata])
  (emacsql gnosis-db `[:pragma (= user-version ,gnosis-db-version)]))

(defun gnosis-db-init ()
  "Create essential directories & database."
  (let ((gnosis-curr-version (caar (emacsql gnosis-db  [:pragma user-version]))))
    (unless (length> (emacsql gnosis-db [:select name :from sqlite-master
						 :where (= type table)])
		     3)
      (emacsql-with-transaction gnosis-db
	(pcase-dolist (`(,table ,schema) gnosis-db--schemata)
	  (emacsql gnosis-db [:create-table $i1 $S2] table schema))
        (emacsql gnosis-db `[:pragma (= user-version ,gnosis-db-version)])))
    ;; Update database schema for version
    (cond ((= gnosis-curr-version 2)
	   (gnosis-db-update-v4))
	  ((< gnosis-curr-version 3)
	   (gnosis-db-update-v5)))))

(gnosis-db-init)

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
  "Run `git pull' in DIR.

Reopens the gnosis database after successful pull."
  (interactive)
  (unless (executable-find "git")
    (error "Git is not installed or not in PATH"))
  (let ((default-directory dir))
    (set-process-sentinel
     (start-process "gnosis-git-pull" "*gnosis-git-pull*"
                    (executable-find "git") "pull")
     (lambda (proc event)
       (cond
        ((string-match-p "finished" event)
         (when (zerop (process-exit-status proc))
           (setf gnosis-db
                 (emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
           (message "Gnosis: Git pull successful, database reopened")))
        ((string-match-p "exited abnormally" event)
         (message "Gnosis: Git pull failed with exit code %s"
                  (process-exit-status proc))))))))

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode gnosis-modeline-mode
  "Minor mode for showing gnosis total due themata on modeline."
  :global t
  :group 'gnosis
  :lighter nil
  (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata)))
  (if gnosis-modeline-mode
      (progn
        (add-to-list 'global-mode-string
                     '(:eval
                       (if (and gnosis-due-themata-total (> gnosis-due-themata-total 0))
                           (propertize (format " [%d] " gnosis-due-themata-total) 'face 'warning
                                       'gnosis-modeline t)
                         "")))
        (force-mode-line-update))
    (setq global-mode-string
          (seq-remove (lambda (item)
                        (and (listp item)
                             (eq (car item) :eval)
                             (get-text-property 0 'gnosis-modeline (format "%s" (eval (cadr item))))))
                      global-mode-string))
    (force-mode-line-update)))

(define-derived-mode gnosis-mode special-mode "Gnosis"
  "Gnosis Mode."
  :interactive nil
  (read-only-mode 0)
  (display-line-numbers-mode 0)
  ;; Initialize centering based on user preference
  (setq-local gnosis-center-content gnosis-center-content-during-review)
  :lighter " gnosis-mode")

;;; Bulk link operations

(defun gnosis--count-themata-with-string (themata string)
  "Count how many THEMATA contain STRING in keimenon outside existing links."
  (cl-count-if (lambda (thema)
                 (gnosis-utils-string-outside-links-p (nth 1 thema) string))
               themata))

(defun gnosis--themata-to-update (themata string node-id)
  "Return list of (ID . NEW-KEIMENON) for THEMATA needing updates."
  (cl-loop for thema in themata
           for thema-id = (nth 0 thema)
           for keimenon = (nth 1 thema)
           for result = (gnosis-utils-replace-string-with-link keimenon string node-id)
           when (car result)
           collect (cons thema-id (cdr result))))

(defun gnosis--update-themata-keimenon (updates)
  "Apply UPDATES list of (ID . NEW-KEIMENON) to database."
  (emacsql-with-transaction gnosis-db
    (dolist (update updates)
      (gnosis-update 'themata `(= keimenon ,(cdr update)) `(= id ,(car update))))))

(defun gnosis--commit-bulk-link (count string)
  "Commit bulk link changes for COUNT themata with STRING."
  (let ((git (executable-find "git"))
        (default-directory gnosis-dir))
    (unless gnosis-testing
      (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
        (vc-git-create-repo))
      (shell-command (format "%s add gnosis.db" git))
      (gnosis--shell-cmd-with-password
       (format "%s commit -m 'Bulk link: %d themata updated with %s'"
               git count string)))
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))))

(defun gnosis-bulk-link-themata (ids string node-id)
  "Replace STRING with org-link to NODE-ID in themata with IDS.
Return list of updated thema IDs."
  (when (string-empty-p string)
    (user-error "String cannot be empty"))
  (unless node-id
    (user-error "Node not found"))
  (let* ((themata (gnosis-select '[id keimenon] 'themata
                                 `(in id ,(vconcat ids))))
         (count (gnosis--count-themata-with-string themata string)))
    (if (zerop count)
        (progn (message "No themata contain '%s'" string) nil)
      (when (y-or-n-p (format "Replace '%s' in %d themata? " string count))
        (let ((updates (gnosis--themata-to-update themata string node-id)))
          (gnosis--update-themata-keimenon updates)
          (gnosis--commit-bulk-link (length updates) string)
          (message "Updated %d themata with links to '%s'" (length updates) string)
          (mapcar #'car updates))))))

(defun gnosis-bulk-link-string (string node-id)
  "Replace all instances of STRING in themata keimenon with org-link to NODE-ID."
  (interactive
   (let* ((string (read-string "String to replace: "))
          (nodes (org-gnosis-select '[id title] 'nodes))
          (node-title (gnosis-completing-read "Select node: " (mapcar #'cadr nodes)))
          (node-id (car (cl-find node-title nodes :key #'cadr :test #'string=))))
     (list string node-id)))
  (gnosis-bulk-link-themata (gnosis-select 'id 'themata nil t) string node-id))

;;; Link integrity

(defun gnosis--all-link-dests ()
  "Return all unique dest UUIDs from gnosis links table."
  (cl-remove-duplicates (gnosis-select 'dest 'links nil t) :test #'equal))

(defun gnosis--orphaned-link-dests ()
  "Return dest UUIDs in gnosis links that have no matching org-gnosis node."
  (let ((link-dests (gnosis--all-link-dests))
        (node-ids (org-gnosis-select 'id 'nodes nil t)))
    (cl-set-difference link-dests node-ids :test #'equal)))

(defun gnosis--orphaned-links ()
  "Return (source dest) rows where dest has no matching org-gnosis node."
  (let ((orphaned-dests (gnosis--orphaned-link-dests)))
    (when orphaned-dests
      (gnosis-select '[source dest] 'links
                     `(in dest ,(vconcat orphaned-dests))))))

(defun gnosis--thema-expected-links (keimenon parathema)
  "Extract expected link IDs from KEIMENON and PARATHEMA text."
  (cl-remove-duplicates
   (append (gnosis-extract-id-links keimenon)
           (gnosis-extract-id-links parathema))
   :test #'equal))

(defun gnosis--stale-links ()
  "Return (source dest) pairs in DB but not in thema text.
Fetches all themata, extras, and links in bulk queries."
  (let* ((themata (gnosis-select '[id keimenon] 'themata nil))
         (extras (gnosis-select '[id parathema] 'extras nil))
         (all-links (gnosis-select '[source dest] 'links nil))
         (extras-map (make-hash-table :test 'equal)))
    ;; Build extras lookup
    (dolist (extra extras)
      (puthash (car extra) (cadr extra) extras-map))
    ;; Find links in DB that aren't in text
    (cl-loop for (source dest) in all-links
             for keimenon = (cadr (cl-find source themata :key #'car))
             for parathema = (gethash source extras-map "")
             for expected = (gnosis--thema-expected-links
                             (or keimenon "") (or parathema ""))
             unless (member dest expected)
             collect (list source dest))))

(defun gnosis--missing-links ()
  "Return (source dest) pairs in thema text but not in DB.
Fetches all themata, extras, and links in bulk queries."
  (let* ((themata (gnosis-select '[id keimenon] 'themata nil))
         (extras (gnosis-select '[id parathema] 'extras nil))
         (all-links (gnosis-select '[source dest] 'links nil))
         (extras-map (make-hash-table :test 'equal))
         (links-set (make-hash-table :test 'equal)))
    ;; Build extras lookup
    (dolist (extra extras)
      (puthash (car extra) (cadr extra) extras-map))
    ;; Build existing links set
    (dolist (link all-links)
      (puthash (format "%s-%s" (car link) (cadr link)) t links-set))
    ;; Find links in text that aren't in DB
    (cl-loop for (id keimenon) in themata
             for parathema = (gethash id extras-map "")
             for expected = (gnosis--thema-expected-links
                             (or keimenon "") (or parathema ""))
             append (cl-loop for dest in expected
                             for key = (format "%s-%s" id dest)
                             unless (gethash key links-set)
                             collect (list id dest)))))

;;;###autoload
(defun gnosis-links-check ()
  "Report link health between gnosis and org-gnosis databases."
  (interactive)
  (let ((orphaned (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links))
        (missing (gnosis--missing-links)))
    (message "Link health: %d orphaned, %d stale, %d missing"
             (length orphaned) (length stale) (length missing))))

(defun gnosis--delete-orphaned-links (orphaned-dests)
  "Delete links whose dest is in ORPHANED-DESTS."
  (when orphaned-dests
    (emacsql-with-transaction gnosis-db
      (emacsql gnosis-db `[:delete :from links
                           :where (in dest ,(vconcat orphaned-dests))]))))

(defun gnosis--delete-stale-links (stale-links)
  "Delete STALE-LINKS list of (source dest) from links table."
  (when stale-links
    (emacsql-with-transaction gnosis-db
      (dolist (link stale-links)
        (emacsql gnosis-db `[:delete :from links
                             :where (and (= source ,(car link))
                                         (= dest ,(cadr link)))])))))

(defun gnosis--insert-missing-links (missing-links)
  "Insert MISSING-LINKS list of (source dest) into links table."
  (when missing-links
    (emacsql-with-transaction gnosis-db
      (dolist (link missing-links)
        (gnosis--insert-into 'links `([,(car link) ,(cadr link)]))))))

(defun gnosis--commit-link-cleanup (orphaned stale missing)
  "Commit link cleanup changes for ORPHANED, STALE, and MISSING counts."
  (let ((git (executable-find "git"))
        (default-directory gnosis-dir))
    (unless gnosis-testing
      (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
        (vc-git-create-repo))
      (shell-command (format "%s add gnosis.db" git))
      (gnosis--shell-cmd-with-password
       (format "%s commit -m 'Link cleanup: %d orphaned, %d stale removed, %d missing added'"
               git orphaned stale missing)))
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))))

;;;###autoload
(defun gnosis-links-cleanup ()
  "Remove orphaned and stale links from gnosis database."
  (interactive)
  (let ((orphaned-dests (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links)))
    (if (and (null orphaned-dests) (null stale))
        (message "No orphaned or stale links found")
      (when (y-or-n-p (format "Remove %d orphaned + %d stale links? "
                              (length orphaned-dests) (length stale)))
        (gnosis--delete-orphaned-links orphaned-dests)
        (gnosis--delete-stale-links stale)
        (gnosis--commit-link-cleanup (length orphaned-dests) (length stale) 0)
        (message "Removed %d orphaned + %d stale links"
                 (length orphaned-dests) (length stale))))))

;;;###autoload
(defun gnosis-links-sync ()
  "Full re-sync: remove orphaned/stale links and insert missing ones."
  (interactive)
  (let ((orphaned-dests (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links))
        (missing (gnosis--missing-links)))
    (if (and (null orphaned-dests) (null stale) (null missing))
        (message "All links are in sync")
      (when (y-or-n-p (format "Sync links: remove %d orphaned + %d stale, add %d missing? "
                              (length orphaned-dests) (length stale) (length missing)))
        (gnosis--delete-orphaned-links orphaned-dests)
        (gnosis--delete-stale-links stale)
        (gnosis--insert-missing-links missing)
        (gnosis--commit-link-cleanup (length orphaned-dests) (length stale) (length missing))
        (message "Synced: removed %d orphaned + %d stale, added %d missing"
                 (length orphaned-dests) (length stale) (length missing))))))

(provide 'gnosis)
;;; gnosis.el ends here
