;;; gnosis-dashboard.el --- Dashboard Module for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2"))

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

;;; Code:

(require 'gnosis-monkeytype)
(require 'gnosis)

(defface gnosis-face-dashboard-header
  '((t :inherit (bold font-lock-constant-face)))
  "Face for dashboard header.

Avoid using an increased height value as this messes up with
`gnosis-center-string' implementation"
  :group 'gnosis)

(defvar gnosis-dashboard-thema-ids nil
  "Store thema ids for dashboard.")

(defvar gnosis-dashboard-buffer-name "*Gnosis Dashboard*"
  "Name of gnosis-dashboard buffer.")

(defvar gnosis-dashboard--current
  '(:type nil :ids nil)
  "Current values to return after edits.")

(defvar gnosis-dashboard--selected-ids nil
  "Selected ids from the tabulated list.")

(defvar gnosis-dashboard-modules
  '(gnosis-dashboard-module-header
    gnosis-dashboard-module-today-stats
    gnosis-dashboard-module-average-rev))

(defvar gnosis-dashboard-module-header
  (lambda ()
    (insert "\n"
	    (gnosis-center-string
	     (format "%s" (propertize "Gnosis Dashboard" 'face
				      'gnosis-face-dashboard-header))))))

(defvar gnosis-dashboard-module-today-stats
  (lambda ()
    (insert
     (gnosis-center-string
      (format "\nReviewed today: %s (New: %s)"
	      (propertize
	       (number-to-string (gnosis-get-date-total-themata))
	       'face 'success)
	      (propertize
	       (number-to-string (gnosis-get-date-new-themata))
	       'face 'font-lock-keyword-face)))
     "\n"
     (gnosis-center-string
      (format "Due themata: %s (Overdue: %s)"
	      (propertize
	       (number-to-string
		(length (mapcar #'car (gnosis-review-get--due-themata))))
	       'face 'error)
	      (propertize
	       (number-to-string
		(length (gnosis-review-get-overdue-themata)))
	       'face 'warning))))))

(defvar gnosis-dashboard-module-average-rev
  (lambda ()
    (insert
     (gnosis-center-string
      (format "Daily Average: %s"
	      (propertize
	       (format "%.2f" (gnosis-calculate-average-daily-reviews))
	       'face 'font-lock-type-face)))
     "\n"
     (gnosis-center-string
      (format "Current streak: %s day(s)"
	      (propertize
	       (gnosis-dashboard--streak
		(gnosis-select 'date 'activity-log '(> reviewed-total 0) t))
	       'face 'success))))))

(defvar gnosis-dashboard-module-monkeytype
  (lambda ()
    (and gnosis-monkeytype-wpm-result
	 (insert
	  (gnosis-center-string
	   (format "Latest WPM: %.2f" gnosis-monkeytype-wpm-result))))))

(defun gnosis-dashboard-return (&optional current-values)
  "Return to dashboard for CURRENT-VALUES."
  (interactive)
  (let* ((current-values (or current-values gnosis-dashboard--current))
	 (type (plist-get current-values :type))
	 (ids (plist-get current-values :ids)))
    (cond ((eq type 'themata)
	   (gnosis-dashboard-output-themata ids))
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
  "Output the average daily themata reviewed as a string for the dashboard."
  (format "%.2f" (gnosis-calculate-average-daily-reviews)))

(defun gnosis-dashboard-edit-thema ()
  "Edit thema with ID."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gnosis-edit-thema id)))

(defun gnosis-dashboard-suspend-thema ()
  "Suspend thema."
  (interactive nil gnosis-dashboard-themata-mode)
  (let ((current-line (line-number-at-pos)))
    (gnosis-toggle-suspend-themata
     (or gnosis-dashboard--selected-ids (list (tabulated-list-get-id))))
    (gnosis-dashboard-output-themata gnosis-dashboard-thema-ids)
    (revert-buffer t t t)
    (forward-line (- current-line 1))))

(defun gnosis-dashboard-delete ()
  "Delete thema."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (if gnosis-dashboard--selected-ids
	(gnosis-dashboard-marked-delete)
      (gnosis-delete-thema (tabulated-list-get-id))
      (gnosis-dashboard-output-themata gnosis-dashboard-thema-ids)
      (revert-buffer t t t))
    (forward-line (- current-line 1))))

(defun gnosis-dashboard-search-thema (&optional str)
  "Search for themata with STR."
  (interactive)
  (gnosis-dashboard-output-themata
   (gnosis-collect-thema-ids :query (or str (read-string "Search for thema: ")))))

(defvar-keymap gnosis-dashboard-themata-mode-map
  :doc "Keymap for themata dashboard."
  "q" #'gnosis-dashboard
  "e" #'gnosis-dashboard-edit-thema
  "s" #'gnosis-dashboard-suspend-thema
  "SPC" #'gnosis-dashboard-search-thema
  "a" #'gnosis-add-thema
  "r" #'gnosis-dashboard-return
  "g" #'gnosis-dashboard-return
  "d" #'gnosis-dashboard-delete
  "m" #'gnosis-dashboard-mark-toggle
  "M" #'gnosis-dashboard-mark-all
  "u" #'gnosis-dashboard-mark-toggle
  "U" #'gnosis-dashboard-unmark-all)

(define-minor-mode gnosis-dashboard-themata-mode
  "Minor mode for gnosis dashboard themata output."
  :keymap gnosis-dashboard-themata-mode-map
  (gnosis-dashboard-decks-mode -1)
  (gnosis-dashboard-tags-mode -1))

(defun gnosis-dashboard--output-themata (thema-ids)
  "Output tabulated-list format for THEMA-IDS."
  (cl-assert (listp thema-ids))
  (let ((entries (emacsql gnosis-db
			  `[:select
			    [themata:id themata:keimenon themata:hypothesis themata:answer
				      themata:tags themata:type review-log:suspend]
			    :from themata
			    :join review-log :on (= themata:id review-log:id)
			    :where (in themata:id ,(vconcat thema-ids))])))
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

(defun gnosis-dashboard-output-themata (thema-ids)
  "Return THEMA-IDS contents on gnosis dashboard."
  (cl-assert (listp thema-ids) t "`thema-ids' must be a list of thema ids.")
  (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
  (gnosis-dashboard-enable-mode)
  (gnosis-dashboard-themata-mode)
  (setf tabulated-list-format `[("Keimenon" ,(/ (window-width) 4) t)
                                ("Hypothesis" ,(/ (window-width) 6) t)
                                ("Answer" ,(/ (window-width) 6) t)
                                ("Tags" ,(/ (window-width) 5) t)
                                ("Type" ,(/ (window-width) 10) t)
                                ("Suspend" ,(/ (window-width) 6) t)]
        gnosis-dashboard-thema-ids thema-ids
        tabulated-list-entries nil)
  (make-local-variable 'tabulated-list-entries)
  (tabulated-list-init-header)
  (let ((inhibit-read-only t)
	(entries (gnosis-dashboard--output-themata thema-ids)))
    (erase-buffer)
    (insert (format "Loading %s themata..." (length thema-ids)))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)
    (setf gnosis-dashboard--current
	  `(:type themata :ids ,thema-ids))))

(defun gnosis-dashboard-deck-thema-count (id)
  "Return total thema count for deck with ID."
  (let ((thema-count (length (gnosis-select 'id 'themata `(= deck-id ,id) t))))
    (when (gnosis-select 'id 'decks `(= id ,id))
      (list (number-to-string thema-count)))))

(defun gnosis-dashboard-output-tag (tag)
  "Output TAG name and total themata."
  (let ((themata (gnosis-get-tag-themata tag)))
    `(,tag ,(number-to-string (length themata)))))

(defun gnosis-dashboard-sort-total-themata (entry1 entry2)
  "Sort function for the total themata column, for ENTRY1 and ENTRY2."
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
      (cl-loop for thema in (gnosis-get-tag-themata tag)
	       do (let* ((tags (car (gnosis-select '[tags] 'themata `(= id ,thema) t)))
			 (new-tags (remove tag tags)))
		    (gnosis-update 'themata `(= tags ',new-tags) `(= id ,thema))))
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
  "Suspend themata of TAG."
  (interactive)
  (let* ((tag (or tag (tabulated-list-get-id)))
	 (themata (gnosis-get-tag-themata tag))
	 (suspend (if current-prefix-arg 0 1))
	 (confirm-msg (y-or-n-p
		       (if (= suspend 0)
			   "Unsuspend all themata for tag? "
			 "Suspend all themata for tag?"))))
    (when confirm-msg
      (emacsql gnosis-db
	       `[:update review-log :set (= suspend ,suspend) :where
			 (in id ,(vconcat themata))])
      (if (= suspend 0)
	  (message "Unsuspended %s themata" (length themata))
	(message "Suspended %s themata" (length themata))))))

(defun gnosis-dashboard-tag-view-themata (&optional tag)
  "View themata for TAG."
  (interactive)
  (let ((tag (or tag (tabulated-list-get-id))))
    (gnosis-dashboard-output-themata (gnosis-get-tag-themata tag))))

(defvar-keymap gnosis-dashboard-tags-mode-map
  "RET" #'gnosis-dashboard-tag-view-themata
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
                                 ("Total Themata" 10 gnosis-dashboard-sort-total-themata)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop for tag in tags
                   collect (list (car (gnosis-dashboard-output-tag tag))
                                 (vconcat (gnosis-dashboard-output-tag tag)))))
    (tabulated-list-print t)))

(defun gnosis-dashboard-output-deck (id)
  "Output contents from deck ID, formatted for gnosis dashboard."
  (let* ((deck-name (gnosis-select 'name 'decks `(= id ,id) t))
         (thema-count (gnosis-dashboard-deck-thema-count id))
         (combined-data (append deck-name (mapcar #'string-to-number thema-count))))
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
			       ("Total Themata" 10 gnosis-dashboard-sort-total-themata)])
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
  "Suspend themata for DECK-ID.

When called with called with a prefix, unsuspend all themata of deck."
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
  "View themata of DECK-ID."
  (interactive)
  (let ((deck-id (or deck-id (string-to-number (tabulated-list-get-id)))))
    (gnosis-dashboard-output-themata (gnosis-collect-thema-ids :deck deck-id))))

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
              ("Total Reviews" ,(/ (window-width) 6) gnosis-dashboard-sort-total-themata)
              ("New" ,(/ (window-width) 6) gnosis-dashboard-sort-total-themata)])
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
  "a" #'gnosis-add-thema
  "A" #'gnosis-add-deck
  "s" #'gnosis-dashboard-suffix-query
  "n" #'(lambda () (interactive) (gnosis-dashboard-output-themata (gnosis-collect-thema-ids)))
  "d" #'gnosis-dashboard-suffix-decks
  "t" #'(lambda () (interactive) (gnosis-dashboard-output-tags))
  "m" #'gnosis-monkeytype-start)

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

(cl-defun gnosis-dashboard--search (&optional dashboard-type (thema-ids nil))
  "Display gnosis dashboard.

THEMA-IDS: List of thema ids to display on dashboard.  When nil, prompt
for dashboard type.

DASHBOARD-TYPE: either Themata or Decks to display the respective dashboard."
  (interactive)
  (let ((dashboard-type (or dashboard-type
			    (cadr (read-multiple-choice
				   "Display dashboard for:"
				   '((?n "themata")
				     (?d "decks")
				     (?t "tags")
				     (?s "search")))))))
    (if thema-ids (gnosis-dashboard-output-themata thema-ids)
      (pcase dashboard-type
	("themata" (gnosis-dashboard-output-themata (gnosis-collect-thema-ids)))
	("decks" (gnosis-dashboard-output-decks))
	("tags"  (gnosis-dashboard-output-themata (gnosis-collect-thema-ids :tags t)))
	("search" (gnosis-dashboard-search-thema))))
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
      (setq gnosis-dashboard--selected-ids gnosis-dashboard-thema-ids)
      (message "Marked %d items" (count-lines (point-min) (point-max))))))

(defun gnosis-dashboard-marked-delete ()
  "Delete marked thema entries."
  (interactive)
  (when (y-or-n-p "Delete selected themata?")
    (cl-loop for thema in gnosis-dashboard--selected-ids
	     do (gnosis-delete-thema thema t))
    (gnosis-dashboard-return)))

(defun gnosis-dashboard-marked-suspend ()
  "Suspend marked thema entries."
  (interactive)
  (when (y-or-n-p "Toggle SUSPEND on selected themata?")
    (gnosis-toggle-suspend-themata gnosis-dashboard--selected-ids nil)
    (gnosis-dashboard-return)))

(transient-define-suffix gnosis-dashboard-suffix-query (query)
  "Search for thema content for QUERY."
  (interactive "sSearch for thema content: ")
  (gnosis-dashboard-output-themata (gnosis-collect-thema-ids :query query)))

(transient-define-suffix gnosis-dashboard-suffix-decks ()
  (interactive)
  (gnosis-dashboard-output-decks))

(transient-define-prefix gnosis-dashboard-menu ()
  "Transient buffer for gnosis dashboard interactions."
  [["Actions"
    ("r" "Review" gnosis-review)
    ("a" "Add thema" gnosis-add-thema)
    ("A" "Add deck" gnosis-add-deck)
    ("q" "Quit" quit-window)
    "\n"]
   ["Themata"
    ("s" "Search" gnosis-dashboard-suffix-query)
    ("n" "Themata" (lambda () (interactive)
		   (gnosis-dashboard-output-themata
		    (gnosis-collect-thema-ids))))
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
	 (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (gnosis-dashboard-mode)
      (dolist (module gnosis-dashboard-modules)
	(funcall (symbol-value module))
	(gnosis-insert-separator))
      ;; Delete last separator
      (beginning-of-visual-line)
      (kill-whole-line))
    (pop-to-buffer-same-window buffer)
    (goto-char (point-min))
    (gnosis-dashboard-enable-mode)))

(provide 'gnosis-dashboard)
;;; gnosis-dashboard.el ends here
