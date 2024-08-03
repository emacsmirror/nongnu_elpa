;;; gnosis-dashboard.el --- Spaced Repetition Algorithm for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2") (transient "0.7.2"))

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

;; This an extension of gnosis.el

;;; Code:
(require 'cl-lib)
(require 'calendar)
(require 'transient)

(declare-function gnosis-select "gnosis.el")
(declare-function gnosis-delete-note "gnosis.el")
(declare-function gnosis-suspend-note "gnosis.el")
(declare-function gnosis-collect-note-ids "gnosis.el")
(declare-function gnosis-edit-deck "gnosis.el")
(declare-function gnosis-edit-note "gnosis.el")
(declare-function gnosis-delete-deck "gnosis.el")
(declare-function gnosis-suspend-deck "gnosis.el")
(declare-function gnosis-add-deck "gnosis.el")
(declare-function gnosis-add-note "gnosis.el")
(declare-function gnosis-insert-separator "gnosis.el")
(declare-function gnosis-get-date-total-notes "gnosis.el")
(declare-function gnosis-center-string "gnosis.el")
(declare-function gnosis-get-date-new-notes "gnosis.el")
(declare-function gnosis-review-get-due-notes "gnosis.el")
(declare-function gnosis-algorithm-date "gnosis-algorithm.el")

(defcustom gnosis-dashboard-months 2
  "Number of additional months to display on dashboard."
  :type 'integer
  :group 'gnosis)

(defvar gnosis-dashboard-note-ids nil
  "Store note ids for dashboard.")

(defvar gnosis-dashboard-search-value nil
  "Store search value.")

(defvar gnosis-dashboard--current
  '(:type nil :ids nil)
  "Current values to return after edits.")

(defface gnosis-dashboard-header-face
  '((t :foreground "#ff0a6a" :weight bold))
  "My custom face for both light and dark backgrounds.")

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
	  ((eq type 'decks )
	   (gnosis-dashboard-output-decks))
	  ((eq type 'tags )
	   (gnosis-dashboard-output-tags)))))

(defun gnosis-dashboard-generate-dates (&optional year)
  "Return a list of all dates (year month day) for YEAR."
  (let* ((current-year (or (decoded-time-year (decode-time)) year))
         (result '()))
    (dotimes (month 12)
      (let ((days-in-month (calendar-last-day-of-month (+ month 1) current-year)))
        (dotimes (day days-in-month)
          (push (list current-year (+ month 1) (+ day 1)) result))))
    (nreverse result)))

(defun gnosis-dashboard-year-stats (&optional year)
  "Return YEAR review stats."
  (let ((notes nil))
    (cl-loop for date in (gnosis-dashboard-generate-dates (and year))
	     do (setq notes (append notes (list (gnosis-get-date-total-notes date)))))
    notes))

(defun gnosis-dashboard--streak (dates &optional num date)
  "Return current review streak.

DATES: Dates in the activity log.
NUM: Streak number.
DATE: Integer, used with `gnosis-algorithm-date' to get previous dates."
  (let ((num (or num 0))
	(date (or date 0)))
    (if (member (gnosis-algorithm-date date) dates)
	(gnosis-dashboard--streak dates (cl-incf num) (- date 1))
      num)))

(defun gnosis-dashboard-month-reviews (month)
  "Return reviewes for MONTH in current year."
  (cl-assert (and (integerp month)
		  (< month 12))
	     nil "Month must be an integer, lower than 12.")
  (let* ((month-dates (cl-loop for date in (gnosis-dashboard-generate-dates)
			       if (and (= (nth 1 date) month)
				       (= (nth 0 date) (decoded-time-year (decode-time))))
			       collect date))
	 (month-reviews (cl-loop for date in month-dates
				 collect (gnosis-get-date-total-notes date))))
    month-reviews))

;; TODO: Optionally, add dates where no review was made.
(defun gnosis-dashboard-output-average-rev ()
  "Output the average daily notes reviewed for current year.

Skips days where no note was reviewed."
    (let ((total 0)
	  (entries (gnosis-dashboard-year-stats)))
      (cl-loop for entry in entries
	       when (not (= entry 0))
	       do (setq total (+ total entry)))
      (/ total (max (length (remove 0 entries)) 1))))

;; TODO: Add more conds & faces
(defun gnosis-dashboard--graph-propertize (string num)
  "Propertize STRING depending on the NUM of reviews."
  (cond ((= num 0)
	 (propertize string 'face 'shadow))
	((> num 0)
	 (propertize string 'face 'font-lock-constant-face))))

(defun gnosis-dashboard--add-padding (str-length)
  "Add padding for STR-LENGTH."
  (let ((padding (/ (- (window-width) str-length) 2)))
    (make-string padding ?\s)))

(defun gnosis-dashboard-reviews-graph (dates &optional )
  "Insert graph for month DATES.

Optionally, use  when using multiple months."
  (let ((count 0)
	(row 0)
	(start-column (current-column))
	(end-column nil))
    (cl-loop for day in dates
	     when (= count 0)
	     do (let ((current-column (current-column)))
		  (and (< (move-to-column start-column) start-column)
		       ;; Add spaces to reach start-column.
		       (insert (make-string (- start-column current-column) ?\s))))
	     (insert " ")
	     do (end-of-line)
	     (insert (gnosis-dashboard--graph-propertize (format "[%s] " (if (= day 0) "-" "x")) day))
	     (cl-incf count)
	     when (= count 7)
	     do
	     (setq end-column (current-column))
	     (setq count 0)
	     (insert " ")
	     (cl-incf row)
	     (end-of-line)
	     (when (and (/= (forward-line 1) 0) (eobp))
	       (insert "\n")
	       (forward-line 0)))
    (insert (make-string (- end-column (current-column)) ?\s))
    (insert " ")))
;; TODO: Refactor this!
(defun gnosis-dashboard-month-overview (&optional num)
  "Insert review graph for MONTHS."
  (gnosis-insert-separator)
  (let* ((point (point))
	 (month (car (calendar-current-date))))
    (insert (gnosis-dashboard--add-padding (min (* (max num 1) 50) (window-width))))
    (while (<= month (+ (car (calendar-current-date)) num))
      ;; (insert (format "%d" month))
      (gnosis-dashboard-reviews-graph (gnosis-dashboard-month-reviews month))
      (goto-char point)
      (end-of-line)
      (cl-incf month))))

(defun gnosis-dashboard-output-note (id)
  "Output contents for note with ID, formatted for gnosis dashboard."
  (cl-loop for item in (append (gnosis-select '[main options answer tags type] 'notes `(= id ,id) t)
			       (gnosis-select 'suspend 'review-log `(= id ,id) t))
           if (listp item)
           collect (mapconcat #'identity item ",")
           else
           collect (replace-regexp-in-string "\n" " " (format "%s" item))))

(defun gnosis-dashboard-edit-note (&optional id)
  "Edit note with ID."
  (interactive)
  (let ((id (or id (string-to-number (tabulated-list-get-id)))))
    (gnosis-edit-note id)))

(defun gnosis-dashboard-suspend-note ()
  "Suspend note."
  (interactive)
  (if gnosis-dashboard--selected-ids
      (gnosis-dashboard-marked-suspend)
    (gnosis-suspend-note (string-to-number (tabulated-list-get-id)))
    (gnosis-dashboard-output-notes gnosis-dashboard-note-ids)
    (revert-buffer t t t)))

(defun gnosis-dashboard-delete ()
  "Delete note."
  (interactive)
  (if gnosis-dashboard--selected-ids
      (gnosis-dashboard-marked-delete)
    (gnosis-delete-note (string-to-number (tabulated-list-get-id)))
    (gnosis-dashboard-output-notes gnosis-dashboard-note-ids)
    (revert-buffer t t t)))

(defvar-keymap gnosis-dashboard-notes-mode-map
  :doc "Keymap for notes dashboard."
  "e" #'gnosis-dashboard-edit-note
  "s" #'gnosis-dashboard-suspend-note
  "a" #'gnosis-add-note
  "r" #'gnosis-dashboard-return
  "g" #'gnosis-dashboard-return
  "d" #'gnosis-dashboard-delete
  "m" #'gnosis-dashboard-mark-toggle
  "u" #'gnosis-dashboard-mark-toggle)

(define-minor-mode gnosis-dashboard-notes-mode
  "Minor mode for gnosis dashboard notes output."
  :keymap gnosis-dashboard-notes-mode-map)

(defun gnosis-dashboard-output-notes (note-ids)
  "Return NOTE-IDS contents on gnosis dashboard."
  (cl-assert (listp note-ids) t "`note-ids' must be a list of note ids.")
  (pop-to-buffer-same-window "*gnosis-dashboard*")
  (gnosis-dashboard-mode)
  (gnosis-dashboard-notes-mode)
  (setf tabulated-list-format `[("Main" ,(/ (window-width) 4) t)
				("Options" ,(/ (window-width) 6) t)
				("Answer" ,(/ (window-width) 6) t)
				("Tags" ,(/ (window-width) 5) t)
				("Type" ,(/ (window-width) 10) T)
				("Suspend" ,(/ (window-width) 6) t)]
	tabulated-list-entries (cl-loop for id in note-ids
					for output = (gnosis-dashboard-output-note id)
					when output
					collect (list (number-to-string id) (vconcat output)))
	gnosis-dashboard-note-ids note-ids)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (setf gnosis-dashboard--current `(:type notes :ids ,note-ids)))

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

(defun gnosis-dashboard-rename-tag (&optional tag new-tag )
  "Rename TAG to NEW-TAG."
  (interactive)
  (let ((new-tag (or new-tag (read-string "News tag name: ")))
	(tag (or tag (tabulated-list-get-id))))
    (cl-loop for note in (gnosis-get-tag-notes tag)
	     do (let* ((tags (car (gnosis-select '[tags] 'notes `(= id ,note) t)))
		       (new-tags (cl-substitute new-tag tag tags :test #'string-equal)))
		  (gnosis-update 'notes `(= tags ',new-tags) `(= id ,note))))))

(defun gnosis-dashboard-tag-view-notes (&optional tag)
  "View notes for TAG."
  (interactive)
  (let ((tag (or tag (tabulated-list-get-id))))
    (gnosis-dashboard-output-notes (gnosis-get-tag-notes tag))))

(defvar-keymap gnosis-dashboard-tags-mode-map
  "RET" #'gnosis-dashboard-tag-view-notes
  "e" #'gnosis-dashboard-rename-tag
  "r" #'gnosis-dashboard-rename-tag
  "g" #'gnosis-dashboard-return)

(define-minor-mode gnosis-dashboard-tags-mode
  "Mode for dashboard output of tags."
  :keymap gnosis-dashboard-tags-mode-map)

(defun gnosis-dashboard-output-tags (&optional tags)
  "Format gnosis dashboard with output of TAGS."
  (let ((tags (or tags (gnosis-get-tags--unique))))
    (pop-to-buffer-same-window "*gnosis-dashboard*")
    (gnosis-dashboard-mode)
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
  "Output contents from deck with ID, formatted for gnosis dashboard."
  (cl-loop for item in (append (gnosis-select 'name
				'decks `(= id ,id) t)
			       (mapcar 'string-to-number (gnosis-dashboard-deck-note-count id)))
	   when (listp item)
	   do (cl-remove-if (lambda (x) (and (vectorp x) (zerop (length x)))) item)
	   collect (format "%s" item)))

(defvar-keymap gnosis-dashboard-decks-mode-map
  "e" #'gnosis-dashboard-edit-deck
  "a" #'gnosis-dashboard-decks-add
  "s" #'gnosis-dashboard-decks-suspend-deck
  "d" #'gnosis-dashboard-decks-delete
  "RET" #'gnosis-dashboard-decks-view-deck)

(define-minor-mode gnosis-dashboard-decks-mode
  "Minor mode for deck output."
  :keymap gnosis-dashboard-decks-mode-map)

(defun gnosis-dashboard-output-decks ()
  "Return deck contents for gnosis dashboard."
  (pop-to-buffer-same-window "*gnosis-dashboard*")
  (gnosis-dashboard-mode)
  (gnosis-dashboard-decks-mode)
  (setq tabulated-list-format [("Name" 15 t)
			       ("Total Notes" 10 gnosis-dashboard-sort-total-notes)])
  (tabulated-list-init-header)
  (setq tabulated-list-entries
	(cl-loop for id in (gnosis-select 'id 'decks '1=1 t)
		 for output = (gnosis-dashboard-output-deck id)
		 when output
		 collect (list (number-to-string id) (vconcat output))))
  (tabulated-list-print t)
  (setf gnosis-dashboard--current `(:type decks :ids ,(gnosis-select 'id 'decks '1=1 t))))

(defun gnosis-dashboard-decks-add ()
  "Add deck & refresh."
  (interactive)
  (gnosis-add-deck (read-string "Deck name: "))
  (gnosis-dashboard-output-decks)
  (revert-buffer t t t))

(defun gnosis-dashboard-edit-deck ()
  "Get deck id from tabulated list and edit it."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gnosis-edit-deck (string-to-number id))))

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

(defvar-keymap gnosis-dashboard-mode-map
  :doc "gnosis-dashboard keymap"
  "q" #'quit-window
  "h" #'gnosis-dashboard-menu)

(define-derived-mode gnosis-dashboard-mode tabulated-list-mode "Gnosis Dashboard"
  "Major mode for displaying Gnosis dashboard."
  :keymap gnosis-dashboard-mode-map
  (setq tabulated-list-padding 2
	tabulated-list-sort-key nil
	gnosis-dashboard--selected-ids nil)
  (display-line-numbers-mode 0))

(cl-defun gnosis-dashboard--search (&optional dashboard-type (note-ids nil))
  "Display gnosis dashboard.

NOTE-IDS: List of note ids to display on dashboard.  When nil, prompt
for dashboard type.

DASHBOARD-TYPE: either 'Notes' or 'Decks' to display the respective dashboard."
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
	("search" (gnosis-dashboard-output-notes
		   (gnosis-collect-note-ids :query (read-string "Search for note: "))))))
    (tabulated-list-print t)))

(defun gnosis-dashboard-mark-toggle ()
  "Toggle mark on the current item in the tabulated-list."
  (interactive)
  (let ((inhibit-read-only t)
        (entry (tabulated-list-get-entry))
	(id (tabulated-list-get-id)))
    (if (derived-mode-p 'tabulated-list-mode)
        (if entry
            (let ((beg (line-beginning-position))
                  (end (line-end-position))
                  (overlays (overlays-in (line-beginning-position) (line-end-position))))
              (if (cl-some (lambda (ov) (overlay-get ov 'gnosis-mark)) overlays)
                  (progn
                    (remove-overlays beg end 'gnosis-mark t)
		    (setq gnosis-dashboard--selected-ids (remove id gnosis-dashboard--selected-ids))
                    ;; (message "Unmarked: %s" (aref entry 0))
		    )
                (let ((ov (make-overlay beg end)))
		  (setf gnosis-dashboard--selected-ids
			(append gnosis-dashboard--selected-ids (list id)))
                  (overlay-put ov 'face 'highlight)
                  (overlay-put ov 'gnosis-mark t)
                  ;; (message "Marked: %s" (aref entry 0))
		  )))
          (message "No entry at point"))
      (message "Not in a tabulated-list-mode"))))

(defun gnosis-dashboard-unmark-all ()
  "Unmark all items in the tabulated-list."
  (interactive)
  (let ((inhibit-read-only t))
    (setq gnosis-dashboard--selected-ids nil)
    (remove-overlays nil nil 'gnosis-mark t)
    (message "All items unmarked")))

(defun gnosis-dashboard-marked-delete ()
  "Delete marked note entries."
  (interactive)
  (when (y-or-n-p "Delete selected notes?")
    (cl-loop for note in gnosis-dashboard--selected-ids
	     do (gnosis-delete-note (string-to-number note) t))
    (gnosis-dashboard-return)))

(defun gnosis-dashboard-marked-suspend ()
  "Suspend marked note entries."
  (interactive)
  (when (y-or-n-p "Toggle SUSPEND on selected notes?")
    (cl-loop for note in gnosis-dashboard--selected-ids
	     do (gnosis-suspend-note (string-to-number note) t))
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
    ("a" "Add note" gnosis-add-note)]
   ["Dashboard" ("s" "Search" gnosis-dashboard--search)]])

;; TODO: Create a dashboard utilizing widgets
;;;###autoload
(defun gnosis-dashboard ()
  "Test function to create an editable field and a search button."
  (interactive)
  (delete-other-windows)
  (let ((buffer-name "*Gnosis Dashboard*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))  ;; Kill the existing buffer if it exists
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (widget-insert "\n"
		       (gnosis-center-string
			(format "%s" (propertize "Gnosis Dashboard" 'face 'gnosis-dashboard-header-face))))
	(gnosis-insert-separator)
	;; (widget-insert (gnosis-center-string (propertize "Stats:" 'face 'underline)) "\n\n")
	(widget-insert (gnosis-center-string
			(format "Reviewed today: %s | New: %s"
				(propertize
				 (number-to-string (gnosis-get-date-total-notes))
				 'face
				 'font-lock-variable-use-face)
				(propertize
				 (number-to-string (gnosis-get-date-new-notes))
				 'face
				 'font-lock-keyword-face))))
	(insert "\n")
	(widget-insert (gnosis-center-string
			(format "Daily Average: %s"
				(propertize (number-to-string (gnosis-dashboard-output-average-rev))
					    'face 'font-lock-type-face))))
	(insert "\n")
	(widget-insert (gnosis-center-string
			 (format "Due notes: %s"
				(propertize
				 (number-to-string (length (gnosis-review-get-due-notes)))
				 'face 'error))))
	(insert "\n\n")
	(widget-insert (gnosis-center-string
			(format "Current streak: %s days"
				(propertize
				 (number-to-string
				  (gnosis-dashboard--streak
				   (gnosis-select 'date 'activity-log '1=1 t)))
				 'face 'success))))
	(insert "\n\n")
        ;; (gnosis-dashboard-month-overview (or gnosis-dashboard-months 0))
        (use-local-map widget-keymap)
        (widget-setup))
      (pop-to-buffer-same-window buffer)
      (goto-char (point-min))
      (gnosis-dashboard-menu))))

(provide 'gnosis-dashboard)
;;; gnosis-dashboard.el ends here
