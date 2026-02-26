;;; gnosis-dashboard.el --- Dashboard Module for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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
(require 'gnosis-tl)
(require 'org-gnosis)

(defface gnosis-face-dashboard-header
  '((t :inherit (bold font-lock-constant-face)))
  "Face for dashboard header.

Avoid using an increased height value as this messes up with
`gnosis-center-string' implementation"
  :group 'gnosis)

(defcustom gnosis-dashboard-nodes-default-sort-column "Backlinks"
  "Default column to sort nodes dashboard by."
  :type '(radio (const :tag "Title" "Title")
                (const :tag "Links (forward links count)" "Links")
                (const :tag "Backlinks (backlinks count)" "Backlinks")
                (const :tag "Themata (themata links count)" "Themata"))
  :group 'gnosis)

(defcustom gnosis-dashboard-nodes-default-sort-ascending nil
  "Whether to sort nodes dashboard in ascending order.

When nil, sort in descending order (larger values first).
When non-nil, sort in ascending order (smaller values first)."
  :type 'boolean
  :group 'gnosis)

(defvar gnosis-dashboard-timer-delay 0.01)

(defvar gnosis-dashboard-thema-ids nil
  "Store thema ids for dashboard.")

(defvar gnosis-dashboard-buffer-name "*Gnosis Dashboard*"
  "Name of gnosis-dashboard buffer.")

(defvar gnosis-dashboard--current
  '(:type nil :ids nil)
  "Current values to return after edits.")

(defvar gnosis-dashboard--selected-ids nil
  "Selected ids from the tabulated list.")

(defvar gnosis-dashboard--entry-cache (make-hash-table :test 'equal)
  "Cache mapping thema id to its formatted tabulated-list entry.
Populated by `gnosis-dashboard--output-themata', invalidated on
edit, delete, and suspend.")

(defvar gnosis-dashboard--rendered-ids nil
  "Thema IDs of the last rendered all-themata view.")

(defvar gnosis-dashboard--rendered-width nil
  "Window width of the last rendered all-themata view.")

(defvar gnosis-dashboard--rendered-text nil
  "Pre-rendered buffer text (with properties) of the all-themata view.
Persists across dashboard opens.  Invalidated only when data changes
via `gnosis-dashboard--update-entries', `gnosis-dashboard--remove-entries',
or `gnosis-dashboard-rebuild-cache'.")


(defvar gnosis-dashboard-themata-mode)

(defvar gnosis-dashboard-modules
  '(gnosis-dashboard-module-header
    gnosis-dashboard-module-today-stats
    gnosis-dashboard-module-average-rev))

(defvar gnosis-dashboard-themata-history nil
  "Stack of previous themata views for navigation history.")

(defvar gnosis-dashboard--view-history nil
  "Stack of previous dashboard views for cross-mode navigation.
Each entry is a function to restore that view
 (e.g. `gnosis-dashboard-output-decks').")

(defvar gnosis-dashboard-themata-current-ids nil
  "Current list of thema IDs being displayed.")

(defvar gnosis-dashboard-nodes-history nil
  "Stack of previous node views for navigation history.")

(defvar gnosis-dashboard-nodes-current-ids nil
  "Current list of node IDs being displayed.")

(defvar gnosis-dashboard--load-generation 0
  "Generation counter to cancel stale async loads.")

(defvar gnosis-dashboard-module-header
  (lambda ()
    (insert "\n"
	    (gnosis-center-string
	     (format "%s" (propertize "Gnosis Dashboard" 'face
				      'gnosis-face-dashboard-header))))))

(defvar gnosis-dashboard-module-today-stats
  (lambda ()
    (let* ((due (gnosis-review-get--due-themata))
           (due-count (length due))
           (today (gnosis-algorithm-date))
           (overdue-count (cl-count-if-not
                           (lambda (thema) (equal (cadr thema) today))
                           due)))
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
                (propertize (number-to-string due-count) 'face 'error)
                (propertize (number-to-string overdue-count)
                            'face 'warning)))))))

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

(defvar-local gnosis-dashboard--base-header-line nil
  "Base `header-line-format' before count badge is prepended.")

(defun gnosis-dashboard--set-header-line (count)
  "Set COUNT badge in the tabulated-list column headers.
Safe to call multiple times; always rebuilds from the base header."
  (let ((base (or gnosis-dashboard--base-header-line header-line-format))
        (badge (concat (propertize (format " %d " count)
                                   'face '(:inherit shadow))
                       " ")))
    (setq-local gnosis-dashboard--base-header-line base)
    (setq-local header-line-format (list badge base))))

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

(defun gnosis-dashboard--streak (dates)
  "Return current review streak number as a string.
DATES: Dates in the activity log, a list of dates in (YYYY MM DD)."
  (let ((date-set (make-hash-table :test 'equal))
        (count 0))
    (dolist (d dates)
      (puthash d t date-set))
    (cl-loop for i from -1 downto -9999
             while (gethash (gnosis-algorithm-date i) date-set)
             do (cl-incf count))
    (when (gethash (gnosis-algorithm-date) date-set)
      (cl-incf count))
    (number-to-string count)))

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
  (let ((ids (or gnosis-dashboard--selected-ids
                 (list (tabulated-list-get-id)))))
    (gnosis-toggle-suspend-themata ids)
    (gnosis-dashboard--update-entries ids)
    (setq gnosis-dashboard--selected-ids nil)))

(defun gnosis-dashboard-delete ()
  "Delete thema."
  (interactive)
  (if gnosis-dashboard--selected-ids
      (gnosis-dashboard-marked-delete)
    (let ((id (tabulated-list-get-id)))
      (gnosis-delete-thema id)
      (gnosis-dashboard--remove-entries (list id)))))

(defun gnosis-dashboard-search-thema (&optional str)
  "Search for themata with STR."
  (interactive)
  ;; Save current themata view and position to history before showing new search results
  (when gnosis-dashboard-themata-current-ids
    (push (cons (tabulated-list-get-id) gnosis-dashboard-themata-current-ids)
          gnosis-dashboard-themata-history))
  (gnosis-dashboard-output-themata
   (gnosis-collect-thema-ids :query (or str (read-string "Search for thema: ")))))

(defun gnosis-dashboard-filter-themata (&optional str ids)
  "Filter themata IDS by searching within them for STR.
If IDS is not provided, use current themata being displayed."
  (interactive)
  (let* ((ids (or ids gnosis-dashboard-themata-current-ids))
         (query (or str (read-string "Filter current themata: "))))
    ;; Validate inputs
    (unless ids (user-error "No themata to filter"))
    (when (string-empty-p query) (user-error "Search query cannot be empty"))
    ;; Filter and display
    (let ((filtered (cl-intersection ids
                                    (gnosis-collect-thema-ids :query query)
                                    :test #'equal)))
      (if filtered
          (progn
            ;; Save current position and IDs to history
            (push (cons (tabulated-list-get-id) ids) gnosis-dashboard-themata-history)
            (gnosis-dashboard-output-themata filtered))
        (message "No themata match the filter")))))

(defun gnosis-dashboard-themata-show-new (max-reviews)
  "Show themata with at most MAX-REVIEWS total reviews.
With prefix arg, prompt for count.  Default 0 (never reviewed)."
  (interactive (list (if current-prefix-arg
                        (read-number "Max reviews: " 0)
                      0)))
  (let ((ids (gnosis-get-themata-by-reviews max-reviews)))
    (if ids
        (progn
          (setq gnosis-dashboard-themata-history nil
                gnosis-dashboard--view-history nil)
          (gnosis-dashboard-output-themata ids))
      (message "No themata with at most %d reviews" max-reviews))))

(defun gnosis-dashboard-filter-themata-by-reviews (max-reviews)
  "Filter current themata to those with at most MAX-REVIEWS total reviews.
With prefix arg, prompt for count.  Default 0 (never reviewed)."
  (interactive (list (if current-prefix-arg
                        (read-number "Max reviews: " 0)
                      0)))
  (unless gnosis-dashboard-themata-current-ids
    (user-error "No themata to filter"))
  (let ((filtered (gnosis-get-themata-by-reviews
                   max-reviews gnosis-dashboard-themata-current-ids)))
    (if filtered
        (progn
          (push (cons (tabulated-list-get-id) gnosis-dashboard-themata-current-ids)
                gnosis-dashboard-themata-history)
          (gnosis-dashboard-output-themata filtered))
      (message "No themata in current view with at most %d reviews" max-reviews))))

(defun gnosis-dashboard-themata-back ()
  "Go back to the previous themata view, nodes view, or main dashboard."
  (interactive)
  (cond
   ;; If themata history exists, go back in themata
   (gnosis-dashboard-themata-history
    (let* ((previous (pop gnosis-dashboard-themata-history))
           (previous-id (car previous))
           (previous-ids (cdr previous)))
      (gnosis-dashboard-output-themata previous-ids)
      ;; Restore cursor position
      (when previous-id
        (goto-char (point-min))
        (while (and (not (eobp))
                   (not (equal (tabulated-list-get-id) previous-id)))
          (forward-line 1)))))
   ;; If no themata history but we're in themata mode and nodes history exists
   ((and (not gnosis-dashboard-themata-history)
         gnosis-dashboard-themata-mode
         gnosis-dashboard-nodes-history)
    (gnosis-dashboard-nodes-back))
   ;; If view history exists, go back to previous view (decks, tags, etc.)
   (gnosis-dashboard--view-history
    (funcall (pop gnosis-dashboard--view-history)))
   ;; Otherwise go to main dashboard
   (t
    (gnosis-dashboard))))

(transient-define-prefix gnosis-dashboard-themata-mode-menu ()
  "Transient menu for themata dashboard mode."
  [["Navigate"
    ("q" "Back" gnosis-dashboard-themata-back)
    ("SPC" "Search" gnosis-dashboard-search-thema)
    ("l" "Filter current" gnosis-dashboard-filter-themata)
    ("n" "Filter new/low reviews" gnosis-dashboard-filter-themata-by-reviews)
    ("g" "Refresh" gnosis-dashboard-return :transient t)
    ("RET" "Edit at point" gnosis-dashboard-edit-thema)]
   ["Edit"
    ("e" "Edit thema" gnosis-dashboard-edit-thema :transient t)
    ("a" "Add thema" gnosis-add-thema :transient t)
    ("s" "Suspend" gnosis-dashboard-suspend-thema :transient t)
    ("d" "Delete" gnosis-dashboard-delete :transient t)
    ("b" "Bulk link" gnosis-dashboard-bulk-link :transient t)]
   ["Mark"
    ("m" "Toggle mark" gnosis-dashboard-mark-toggle :transient t)
    ("M" "Mark all" gnosis-dashboard-mark-all :transient t)
    ("u" "Unmark" gnosis-dashboard-mark-toggle :transient t)
    ("U" "Unmark all" gnosis-dashboard-unmark-all :transient t)]])

(defvar-keymap gnosis-dashboard-themata-mode-map
  :doc "Keymap for themata dashboard."
  "?" #'gnosis-dashboard-themata-mode-menu
  "h" #'gnosis-dashboard-themata-mode-menu
  "q" #'gnosis-dashboard-themata-back
  "e" #'gnosis-dashboard-edit-thema
  "s" #'gnosis-dashboard-suspend-thema
  "SPC" #'gnosis-dashboard-search-thema
  "l" #'gnosis-dashboard-filter-themata
  "n" #'gnosis-dashboard-filter-themata-by-reviews
  "a" #'gnosis-add-thema
  "r" #'gnosis-dashboard-return
  "g" #'gnosis-dashboard-return
  "d" #'gnosis-dashboard-delete
  "m" #'gnosis-dashboard-mark-toggle
  "M" #'gnosis-dashboard-mark-all
  "b" #'gnosis-dashboard-bulk-link
  "u" #'gnosis-dashboard-mark-toggle
  "U" #'gnosis-dashboard-unmark-all
)

(define-minor-mode gnosis-dashboard-themata-mode
  "Minor mode for gnosis dashboard themata output."
  :keymap gnosis-dashboard-themata-mode-map)

(defun gnosis-dashboard--format-entry (row)
  "Format a single database ROW into a tabulated-list entry.
ROW is (id keimenon hypothesis answer tags type suspend)."
  (let* ((fields (cl-loop for item in (cdr row)
			  if (listp item)
			  collect (mapconcat (lambda (x) (format "%s" x)) item ",")
			  else collect
			  (let ((formatted (replace-regexp-in-string "\n" " " (format "%s" item))))
			    (replace-regexp-in-string
			     "\\[\\[id:[^]]+\\]\\[\\(.*?\\)\\]\\]"
			     "\\1" formatted)))))
    (list (car row)
	  (vconcat (append (butlast fields)
			   (list (if (equal (car (last fields)) "1")
				     "Yes" "No")))))))

(defun gnosis-dashboard--output-themata (thema-ids)
  "Output tabulated-list format for THEMA-IDS.
Uses `gnosis-dashboard--entry-cache' to avoid re-querying known entries."
  (cl-assert (listp thema-ids))
  (let* ((uncached (cl-remove-if
		    (lambda (id) (gethash id gnosis-dashboard--entry-cache))
		    thema-ids)))
    ;; Fetch and cache only the missing entries
    (when uncached
      (let ((rows (emacsql gnosis-db
			   `[:select
			     [themata:id themata:keimenon themata:hypothesis themata:answer
				       themata:tags themata:type review-log:suspend]
			     :from themata
			     :join review-log :on (= themata:id review-log:id)
			     :where (in themata:id ,(vconcat uncached))])))
	(dolist (row rows)
	  (puthash (car row) (gnosis-dashboard--format-entry row)
		   gnosis-dashboard--entry-cache))))
    ;; Return all requested entries from cache (preserving order)
    (cl-loop for id in thema-ids
	     for entry = (gethash id gnosis-dashboard--entry-cache)
	     when entry collect entry)))

(defun gnosis-dashboard--update-entries (ids)
  "Re-fetch and update tabulated-list entries for IDS.
Replaces only the affected lines in the buffer via `gnosis-tl-replace-entry'."
  (dolist (id ids)
    (remhash id gnosis-dashboard--entry-cache))
  (setq gnosis-dashboard--rendered-text nil)
  (let* ((new-entries (gnosis-dashboard--output-themata ids))
         (update-map (make-hash-table :test 'equal)))
    (dolist (entry new-entries)
      (puthash (car entry) entry update-map))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (or (gethash (car entry) update-map) entry))
                  tabulated-list-entries))
    (dolist (entry new-entries)
      (gnosis-tl-replace-entry (car entry) (cadr entry)))))

(defun gnosis-dashboard--remove-entries (ids)
  "Remove tabulated-list entries for IDS.
Deletes only the affected lines in the buffer via `gnosis-tl-delete-entry'."
  (let ((id-set (make-hash-table :test 'equal)))
    (dolist (id ids)
      (puthash id t id-set)
      (remhash id gnosis-dashboard--entry-cache))
    (setq gnosis-dashboard--rendered-text nil)
    (setq tabulated-list-entries
          (cl-remove-if (lambda (entry) (gethash (car entry) id-set))
                        tabulated-list-entries))
    (setq gnosis-dashboard-thema-ids
          (cl-remove-if (lambda (id) (gethash id id-set))
                        gnosis-dashboard-thema-ids))
    (dolist (id ids)
      (gnosis-tl-delete-entry id))))

(defun gnosis-dashboard--refresh-cache-entry (id)
  "Silently re-cache the formatted entry for thema ID.
Does not touch any buffer — only updates `gnosis-dashboard--entry-cache'."
  (remhash id gnosis-dashboard--entry-cache)
  (gnosis-dashboard--output-themata (list id)))

(defun gnosis-dashboard-update-entry (id)
  "Update the tabulated-list entry for thema ID in place.
Called from `gnosis-save-hook'.
When in themata view, updates the visible list.  Otherwise silently
refreshes the cache so the next themata view is current."
  (if gnosis-dashboard-themata-mode
      (progn
        (gnosis-dashboard--update-entries (list id))
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not (equal (tabulated-list-get-id) id)))
          (forward-line 1)))
    (gnosis-dashboard--refresh-cache-entry id)))

(add-hook 'gnosis-save-hook #'gnosis-dashboard-update-entry)

(defcustom gnosis-dashboard-chunk-size 500
  "Number of themata per chunk for background cache warming.
Each chunk fetches this many entries then yields to the event loop."
  :type 'integer
  :group 'gnosis)

(defcustom gnosis-dashboard-render-chunk-size 5000
  "Number of entries per chunk for progressive rendering.
The first chunk is rendered immediately; remaining chunks are
appended via timers so the UI stays responsive."
  :type 'integer
  :group 'gnosis)

(defun gnosis-dashboard--warm-cache-chunk (chunks total warmed)
  "Process one chunk of thema IDs for background cache warming.
CHUNKS: remaining list of id-sublists.  TOTAL: total thema count.
WARMED: count of entries processed so far.
Continues as long as the dashboard buffer exists."
  (when (and (get-buffer gnosis-dashboard-buffer-name) chunks)
    (let* ((ids (car chunks))
           (uncached (cl-remove-if
                      (lambda (id) (gethash id gnosis-dashboard--entry-cache))
                      ids))
           (new-warmed (+ warmed (length ids))))
      (when uncached
        (let ((rows (emacsql gnosis-db
                     `[:select
                       [themata:id themata:keimenon themata:hypothesis themata:answer
                                   themata:tags themata:type review-log:suspend]
                       :from themata
                       :join review-log :on (= themata:id review-log:id)
                       :where (in themata:id ,(vconcat uncached))])))
          (dolist (row rows)
            (puthash (car row) (gnosis-dashboard--format-entry row)
                     gnosis-dashboard--entry-cache))))
      (if (cdr chunks)
          (progn
            (message "Warming cache... %d/%d" new-warmed total)
            (run-with-timer gnosis-dashboard-timer-delay nil
                            #'gnosis-dashboard--warm-cache-chunk
                            (cdr chunks) total new-warmed))
        (message "Cache warmed (%d themata)" total)
        (gnosis-dashboard--start-prerender)))))

(defun gnosis-dashboard-warm-cache ()
  "Warm the entry cache for all themata in the background.
Continues as long as the dashboard buffer exists, regardless of
which view the user navigates to."
  (let* ((all-ids (gnosis-select 'id 'themata nil t))
         (chunks (let (result (rest all-ids))
                   (while rest
                     (push (seq-take rest gnosis-dashboard-chunk-size) result)
                     (setq rest (nthcdr gnosis-dashboard-chunk-size rest)))
                   (nreverse result))))
    (when chunks
      (run-with-timer gnosis-dashboard-timer-delay nil
                      #'gnosis-dashboard--warm-cache-chunk
                      chunks (length all-ids) 0))))

(defun gnosis-dashboard--start-prerender ()
  "Begin background pre-rendering of the all-themata view.
Called after cache warming completes.  Splits entries into chunks
and processes them via timer chain, storing the final result in
`gnosis-dashboard--rendered-text'."
  (let* ((buf (get-buffer gnosis-dashboard-buffer-name))
         (win (and buf (get-buffer-window buf)))
         (w (if win (window-width win) 80))
         (all-ids (gnosis-select 'id 'themata nil t))
         (gen gnosis-dashboard--load-generation)
         (fmt (gnosis-dashboard--compute-column-format w))
         (entries (cl-loop for id in all-ids
                           for entry = (gethash id gnosis-dashboard--entry-cache)
                           when entry collect entry))
         (chunks (let (result (rest entries))
                   (while rest
                     (push (seq-take rest gnosis-dashboard-chunk-size) result)
                     (setq rest (nthcdr gnosis-dashboard-chunk-size rest)))
                   (nreverse result))))
    (when chunks
      (run-with-timer gnosis-dashboard-timer-delay nil
                      #'gnosis-dashboard--prerender-chunk
                      chunks fmt w all-ids gen nil))))

(defun gnosis-dashboard--prerender-chunk (chunks fmt width all-ids gen acc)
  "Render one CHUNKS entry for the pre-render cache.
FMT: column format vector.  WIDTH: window width.
ALL-IDS: full thema ID list.  GEN: load generation for staleness.
ACC: accumulated result strings."
  (when (and (get-buffer gnosis-dashboard-buffer-name)
             (= gen gnosis-dashboard--load-generation))
    (let ((rendered (gnosis-tl-render-lines (car chunks) fmt 2)))
      (push rendered acc)
      (if (cdr chunks)
          (run-with-timer gnosis-dashboard-timer-delay nil
                          #'gnosis-dashboard--prerender-chunk
                          (cdr chunks) fmt width all-ids gen acc)
        ;; Final chunk — assemble and store
        (let ((text (apply #'concat (nreverse acc))))
          (when (= gen gnosis-dashboard--load-generation)
            (setq gnosis-dashboard--rendered-text text
                  gnosis-dashboard--rendered-ids all-ids
                  gnosis-dashboard--rendered-width width))
          (message "Pre-render complete (%d themata)" (length all-ids)))))))

(defun gnosis-dashboard-rebuild-cache ()
  "Clear and rebuild the themata entry cache."
  (interactive)
  (clrhash gnosis-dashboard--entry-cache)
  (setq gnosis-dashboard--rendered-text nil)
  (message "Cache cleared, rebuilding...")
  (gnosis-dashboard-warm-cache))

(defun gnosis-dashboard--compute-column-format (width)
  "Compute the themata column format vector for window WIDTH.
Distributes available width (minus padding and column gaps)
proportionally so all columns fit."
  (let ((avail (- width 7)))
    `[("Keimenon"   ,(max 10 (/ (* avail 28) 100)) t)
      ("Hypothesis" ,(max 8  (/ (* avail 16) 100)) t)
      ("Answer"     ,(max 8  (/ (* avail 16) 100)) t)
      ("Tags"       ,(max 8  (/ (* avail 18) 100)) t)
      ("Type"       ,(max 5  (/ (* avail 10) 100)) t)
      ("Suspend"    ,(max 3  (/ (* avail 8) 100)) t)]))

(defun gnosis-dashboard--set-column-format ()
  "Set `tabulated-list-format' based on current window width."
  (setf tabulated-list-format
        (gnosis-dashboard--compute-column-format (window-width))))


(defun gnosis-dashboard--progressive-render (entries gen)
  "Render ENTRIES progressively in chunks.
First chunk is rendered immediately; remaining chunks are appended
via timers.  GEN is the load generation for staleness checks."
  (let* ((size gnosis-dashboard-render-chunk-size)
         (first-chunk (seq-take entries size))
         (rest (nthcdr size entries)))
    ;; Render first chunk immediately
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (gnosis-tl--render-into-buffer first-chunk tabulated-list-format
                                     (or tabulated-list-padding 0)))
    (setq tabulated-list-entries (copy-sequence first-chunk))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    ;; Schedule remaining chunks
    (when rest
      (let* ((tail (last tabulated-list-entries))
             (buf (current-buffer))
             (chunks (let (result (r rest))
                       (while r
                         (push (seq-take r size) result)
                         (setq r (nthcdr size r)))
                       (nreverse result))))
        (run-with-timer gnosis-dashboard-timer-delay nil
                        #'gnosis-dashboard--append-chunk
                        buf chunks tail gen)))))

(defun gnosis-dashboard--append-chunk (buf chunks entries-tail gen)
  "Append one chunk of entries to BUF.
CHUNKS: remaining list of entry sublists.
ENTRIES-TAIL: last cons cell of `tabulated-list-entries' for nconc.
GEN: load generation — no-op if stale."
  (when (and (buffer-live-p buf)
             (= gen gnosis-dashboard--load-generation)
             chunks)
    (with-current-buffer buf
      (let ((chunk (car chunks)))
        (gnosis-tl-append-entries chunk)
        ;; Extend tabulated-list-entries via nconc to the tail
        (let ((new-tail (copy-sequence chunk)))
          (setcdr entries-tail new-tail)
          (setq entries-tail (last new-tail))))
      (if (cdr chunks)
          (run-with-timer gnosis-dashboard-timer-delay nil
                          #'gnosis-dashboard--append-chunk
                          buf (cdr chunks) entries-tail gen)
        ;; Final chunk — update header badge, defer cache string
        (gnosis-dashboard--set-header-line (length tabulated-list-entries))
        (let ((ids (copy-sequence gnosis-dashboard-thema-ids))
              (w (window-width)))
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq gnosis-dashboard--rendered-ids ids
                       gnosis-dashboard--rendered-width w
                       gnosis-dashboard--rendered-text
                       (buffer-string)))))))))))

(defun gnosis-dashboard-output-themata (thema-ids)
  "Display THEMA-IDS in the gnosis dashboard."
  (cl-assert (listp thema-ids) t "`thema-ids' must be a list of thema ids.")
  (cl-incf gnosis-dashboard--load-generation)
  (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
  (gnosis-dashboard-enable-mode)
  ;; Disable other dashboard modes
  (gnosis-dashboard-nodes-mode -1)
  (gnosis-dashboard-decks-mode -1)
  (gnosis-dashboard-tags-mode -1)
  ;; Enable themata mode
  (gnosis-dashboard-themata-mode 1)
  ;; Store current thema IDs for history
  (setq gnosis-dashboard-themata-current-ids thema-ids)
  (gnosis-dashboard--set-column-format)
  (setf gnosis-dashboard-thema-ids thema-ids
        tabulated-list-sort-key nil)
  (make-local-variable 'tabulated-list-entries)
  (tabulated-list-init-header)
  (setq-local gnosis-dashboard--base-header-line header-line-format)
  (setf gnosis-dashboard--current
	`(:type themata :ids ,thema-ids))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (and gnosis-dashboard--rendered-text
             (equal thema-ids gnosis-dashboard--rendered-ids)
             (= (window-width) gnosis-dashboard--rendered-width))
        ;; Cache hit — insert cached text and restore entries from data cache
        (progn
          (insert gnosis-dashboard--rendered-text)
          (setq tabulated-list-entries
                (gnosis-dashboard--output-themata thema-ids)))
      ;; Cache miss — compute entries and render progressively
      (let* ((entries (gnosis-dashboard--output-themata thema-ids))
             (sorter (gnosis-tl--get-sorter)))
        (when sorter
          (setq entries (sort entries sorter)))
        (gnosis-dashboard--progressive-render
         entries gnosis-dashboard--load-generation))))
  (gnosis-dashboard--set-header-line (length thema-ids)))

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
  "Delete TAG from all themata."
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
    ;; Clear history for fresh start from tags
    (setq gnosis-dashboard-themata-history nil)
    (push #'gnosis-dashboard-output-tags gnosis-dashboard--view-history)
    (gnosis-dashboard-output-themata (gnosis-get-tag-themata tag))))

(transient-define-prefix gnosis-dashboard-tags-mode-menu ()
  "Transient menu for tags dashboard mode."
  [["Navigate"
    ("RET" "View themata" gnosis-dashboard-tag-view-themata)
    ("q" "Back to dashboard" gnosis-dashboard)
    ("g" "Refresh" gnosis-dashboard-return :transient t)]
   ["Edit"
    ("e" "Rename tag" gnosis-dashboard-rename-tag :transient t)
    ("r" "Rename tag" gnosis-dashboard-rename-tag :transient t)
    ("s" "Suspend tag" gnosis-dashboard-suspend-tag :transient t)
    ("d" "Delete tag" gnosis-dashboard-delete-tag :transient t)]])

(defvar-keymap gnosis-dashboard-tags-mode-map
  "?" #'gnosis-dashboard-tags-mode-menu
  "h" #'gnosis-dashboard-tags-mode-menu
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
    ;; Disable other dashboard modes
    (gnosis-dashboard-themata-mode -1)
    (gnosis-dashboard-nodes-mode -1)
    (gnosis-dashboard-decks-mode -1)
    ;; Enable tags mode
    (gnosis-dashboard-tags-mode 1)
    (setf gnosis-dashboard--current '(:type tags))
    (setq tabulated-list-format [("Name" 35 t)
                                 ("Total Themata" 10 gnosis-dashboard-sort-total-themata)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop for tag in tags
                   for output = (gnosis-dashboard-output-tag tag)
                   collect (list (car output)
                                 (vconcat output))))
    (tabulated-list-print t)
    (gnosis-dashboard--set-header-line (length tabulated-list-entries))))

(defun gnosis-dashboard-output-deck (id)
  "Output contents from deck ID, formatted for gnosis dashboard."
  (let* ((deck-name (gnosis-select 'name 'decks `(= id ,id) t))
         (thema-count (gnosis-dashboard-deck-thema-count id))
         (combined-data (append deck-name (mapcar #'string-to-number thema-count))))
    (mapcar (lambda (item) (format "%s" item))
            (seq-filter (lambda (item)
                         (not (and (vectorp item) (seq-empty-p item))))
                       combined-data))))

(transient-define-prefix gnosis-dashboard-decks-mode-menu ()
  "Transient menu for decks dashboard mode."
  [["Navigate"
    ("RET" "View deck" gnosis-dashboard-decks-view-deck)
    ("q" "Back to dashboard" gnosis-dashboard)]
   ["Edit"
    ("e" "Rename deck" gnosis-dashboard-rename-deck :transient t)
    ("r" "Rename deck" gnosis-dashboard-rename-deck :transient t)
    ("a" "Add deck" gnosis-dashboard-decks-add :transient t)
    ("s" "Suspend deck" gnosis-dashboard-decks-suspend-deck :transient t)
    ("d" "Delete deck" gnosis-dashboard-decks-delete :transient t)]])

(defvar-keymap gnosis-dashboard-decks-mode-map
  "?" #'gnosis-dashboard-decks-mode-menu
  "h" #'gnosis-dashboard-decks-mode-menu
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
  ;; Disable other dashboard modes
  (gnosis-dashboard-themata-mode -1)
  (gnosis-dashboard-nodes-mode -1)
  (gnosis-dashboard-tags-mode -1)
  ;; Enable decks mode
  (gnosis-dashboard-decks-mode 1)
  (setq tabulated-list-format [("Name" 15 t)
			       ("Total Themata" 10 gnosis-dashboard-sort-total-themata)])
  (tabulated-list-init-header)
  (setq tabulated-list-entries
	(cl-loop for id in (gnosis-select 'id 'decks nil t)
		 for output = (gnosis-dashboard-output-deck id)
		 when output
		 collect (list (number-to-string id) (vconcat output))))
  (tabulated-list-print t)
  (gnosis-dashboard--set-header-line (length tabulated-list-entries))
  (setf gnosis-dashboard--current `(:type decks :ids ,(gnosis-select 'id 'decks nil t))))

(defun gnosis-dashboard-decks-add ()
  "Add deck & refresh."
  (interactive)
  (gnosis-add-deck (read-string "Deck name: "))
  (gnosis-dashboard-output-decks)
  (revert-buffer t t t))

(defun gnosis-dashboard-decks-suspend-deck (&optional deck-id)
  "Suspend themata for DECK-ID.

When called with a prefix, unsuspend all themata of deck."
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
    ;; Clear history for fresh start from decks
    (setq gnosis-dashboard-themata-history nil)
    (push #'gnosis-dashboard-output-decks gnosis-dashboard--view-history)
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
  ;; Main menu
  "h" #'gnosis-dashboard-menu
  ;; Navigate
  "n" #'gnosis-dashboard-menu-nodes
  "t" #'gnosis-dashboard-menu-themata
  "D" #'gnosis-dashboard-output-decks
  ;; Sort (override tabulated-list-sort with fast version)
  "S" #'gnosis-tl-sort
  ;; Actions
  "r" #'gnosis-review
  "a" #'gnosis-add-thema
  "d" #'gnosis-add-deck
  "SPC" #'gnosis-dashboard-search-thema
  "q" #'quit-window)

(define-derived-mode gnosis-dashboard-mode tabulated-list-mode "Gnosis Dashboard"
  "Major mode for displaying Gnosis dashboard."
  :keymap gnosis-dashboard-mode-map
  :interactive nil
  (setq-local header-line-format nil)
  ;; Character "…" can mess up column-width depending on the font used.
  (setq-local truncate-string-ellipsis "...")
  ;; Dashboard always centers content
  (setq-local gnosis-center-content t)
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
	("search" (gnosis-dashboard-search-thema))))))

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
    (gnosis-dashboard--remove-entries gnosis-dashboard--selected-ids)
    (setq gnosis-dashboard--selected-ids nil)))

(defun gnosis-dashboard-marked-suspend ()
  "Suspend marked thema entries."
  (interactive)
  (when (y-or-n-p "Toggle SUSPEND on selected themata?")
    (gnosis-toggle-suspend-themata gnosis-dashboard--selected-ids nil)
    (gnosis-dashboard--update-entries gnosis-dashboard--selected-ids)
    (setq gnosis-dashboard--selected-ids nil)))

(defun gnosis-dashboard-bulk-link ()
  "Bulk link string in marked or all displayed themata."
  (interactive nil gnosis-dashboard-themata-mode)
  (let* ((ids (or gnosis-dashboard--selected-ids
                  gnosis-dashboard-themata-current-ids))
         (string (read-string "String to replace: "))
         (nodes (org-gnosis-select '[id title] 'nodes))
         (node-title (gnosis-completing-read "Select node: " (mapcar #'cadr nodes)))
         (node-id (car (cl-find node-title nodes :key #'cadr :test #'string=)))
         (updated (gnosis-bulk-link-themata ids string node-id)))
    (when updated
      (gnosis-dashboard--update-entries updated)
      (setq gnosis-dashboard--selected-ids nil))))

(transient-define-suffix gnosis-dashboard-suffix-query (query)
  "Search for thema content for QUERY."
  (interactive "sSearch for thema content: ")
  (gnosis-dashboard-output-themata (gnosis-collect-thema-ids :query query)))

(transient-define-prefix gnosis-dashboard-menu-nodes ()
  "Transient menu for node operations."
  [["Nodes"
    ("a" "View all nodes" (lambda () (interactive)
                           (setq gnosis-dashboard-nodes-history nil)
                           (gnosis-dashboard-output-nodes)))
    ("t" "View nodes by tag" gnosis-dashboard-nodes-search-by-tag)
    ("i" "View isolated nodes" (lambda () (interactive)
                                (setq gnosis-dashboard-nodes-history nil)
                                (gnosis-dashboard-output-nodes)
                                (gnosis-dashboard-nodes-show-isolated)))
    ("q" "Back" transient-quit-one)]])

(defun gnosis-dashboard-themata-show-orphaned ()
  "Show themata with orphaned links (referencing deleted org-gnosis nodes)."
  (interactive nil gnosis-dashboard-themata-mode)
  (let* ((orphaned-rows (gnosis--orphaned-links))
         (thema-ids (when orphaned-rows
                      (cl-remove-duplicates (mapcar #'car orphaned-rows)))))
    (if thema-ids
        (progn
          (setq gnosis-dashboard-themata-history nil
                gnosis-dashboard--view-history nil)
          (gnosis-dashboard-output-themata thema-ids))
      (message "No themata with orphaned links"))))

(transient-define-prefix gnosis-dashboard-menu-themata ()
  "Transient menu for themata operations."
  [["Themata"
    ("a" "View all themata" (lambda () (interactive)
                             (setq gnosis-dashboard-themata-history nil
                                   gnosis-dashboard--view-history nil)
                             (gnosis-dashboard-output-themata (gnosis-collect-thema-ids))))
    ("s" "Search themata" gnosis-dashboard-suffix-query)
    ("d" "View by decks" (lambda () (interactive)
                          (gnosis-dashboard-output-decks)))
    ("t" "View by tags" (lambda () (interactive)
                         (gnosis-dashboard-output-tags)))
    ("n" "View new" gnosis-dashboard-themata-show-new)
    ("o" "Show orphaned" gnosis-dashboard-themata-show-orphaned)
    ("q" "Back" transient-quit-one)]])

(transient-define-prefix gnosis-dashboard-menu ()
  "Transient buffer for gnosis dashboard interactions."
  [["Navigate"
    ("n" "Nodes" gnosis-dashboard-menu-nodes)
    ("t" "Themata" gnosis-dashboard-menu-themata)
    ("D" "Decks" (lambda () (interactive) (gnosis-dashboard-output-decks)))
    ("q" "Quit" quit-window)]
   ["Actions"
    ("r" "Review" gnosis-review)
    ("a" "Add thema" gnosis-add-thema)
    ("d" "Add deck" gnosis-add-deck)
    ("m" "Monkeytype" gnosis-monkeytype-start)
    ("h" "History" gnosis-dashboard-history)]
   ["Import/Export"
    ("e" "Export deck" gnosis-export-deck)
    ("i" "Import deck" gnosis-import-deck)]
   ["Maintenance"
    ("s" "Sync nodes" org-gnosis-db-sync)
    ("S" "Rebuild nodes" (lambda () (interactive) (org-gnosis-db-sync t)))
    ("l" "Link health" gnosis-links-check)
    ("L" "Link sync" gnosis-links-sync)
    ("c" "Rebuild cache" gnosis-dashboard-rebuild-cache)]])

(defun gnosis-dashboard--load-stats (buffer marker generation)
  "Load dashboard statistics into BUFFER at MARKER position.
GENERATION prevents stale updates when the user navigates away."
  (when (and (buffer-live-p buffer)
             (= generation gnosis-dashboard--load-generation))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (delete-region marker (point-max))
        (goto-char marker)
        (let ((first t))
          (dolist (module (cdr gnosis-dashboard-modules))
            (if first
                (setq first nil)
              (gnosis-insert-separator))
            (funcall (symbol-value module))))
        (goto-char (point-min))))))

;;;###autoload
(defun gnosis-dashboard ()
  "Launch gnosis dashboard."
  (interactive)
  (cl-incf gnosis-dashboard--load-generation)
  (let* ((buffer (get-buffer-create gnosis-dashboard-buffer-name))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (gnosis-dashboard-mode)
      ;; Show header immediately
      (funcall (symbol-value (car gnosis-dashboard-modules)))
      (gnosis-insert-separator)
      (let ((stats-start (point-marker))
            (gen gnosis-dashboard--load-generation))
        (insert (gnosis-center-string "Loading statistics..."))
        (pop-to-buffer-same-window buffer)
        (goto-char (point-min))
        (gnosis-dashboard-enable-mode)
        ;; Defer expensive stats modules
        (run-with-timer gnosis-dashboard-timer-delay nil
                        #'gnosis-dashboard--load-stats
                        buffer stats-start gen)
        ;; Start background cache warming after stats have a head start
        (run-with-timer (* 2 gnosis-dashboard-timer-delay) nil
                        #'gnosis-dashboard-warm-cache))
      (gnosis-dashboard-menu))))

(defun gnosis-dashboard-sort-count (entry1 entry2)
  "Sort function for numeric count columns.
Compares ENTRY1 and ENTRY2 by converting string values to numbers."
  (let* ((col-name (car tabulated-list-sort-key))
         (col-index (tabulated-list--column-number col-name)))
    (< (string-to-number (aref (cadr entry1) col-index))
       (string-to-number (aref (cadr entry2) col-index)))))

(defun gnosis-dashboard-get-themata-links (node-id)
  "Return list of thema IDs that link to NODE-ID.
Queries the gnosis database links table where dest = NODE-ID."
  (gnosis-select 'source 'links `(= dest ,node-id) t))

(defun gnosis-dashboard-get-themata-link-titles (node-id)
  "Return list of keimenon for themata that link to NODE-ID."
  (let ((thema-ids (gnosis-dashboard-get-themata-links node-id)))
    (mapcar (lambda (id)
              (car (gnosis-select 'keimenon 'themata `(= id ,id) t)))
            thema-ids)))

(defun gnosis-dashboard-get-backlink-titles (node-id)
  "Return list of titles for nodes that link to NODE-ID (backlinks)."
  (let ((backlink-ids (org-gnosis-select 'source 'links `(= dest ,node-id) t)))
    (mapcar (lambda (id)
              (car (org-gnosis-select 'title 'nodes `(= id ,id) t)))
            backlink-ids)))

(defun gnosis-dashboard-get-backlink-ids (node-id)
  "Return list of node IDs that link to NODE-ID (backlinks)."
  (org-gnosis-select 'source 'links `(= dest ,node-id) t))

(defun gnosis-dashboard-get-forward-link-ids (node-id)
  "Return list of node IDs that NODE-ID links to (forward links)."
  (org-gnosis-select 'dest 'links `(= source ,node-id) t))

(defun gnosis-dashboard-nodes--data (&optional node-ids)
  "Get nodes data formatted for tabulated-list-mode.
If NODE-IDS is provided, only get data for those nodes.
Returns list of (ID [TITLE LINK-COUNT BACKLINK-COUNT THEMATA-LINKS-COUNT])."
  (let* ((nodes-data (org-gnosis-get-nodes-data node-ids))
	 (all-ids (mapcar #'car nodes-data))
	 ;; Bulk fetch forward links (1 query instead of N)
	 (fwd-raw (if all-ids
		      (org-gnosis-select '[source dest] 'links
					 `(in source ,(vconcat all-ids)))
		    (org-gnosis-select '[source dest] 'links)))
	 (fwd-hash (let ((h (make-hash-table :test 'equal)))
		     (dolist (link fwd-raw h)
		       (puthash (nth 0 link)
				(1+ (or (gethash (nth 0 link) h) 0)) h))))
	 ;; Bulk fetch themata links (1 query instead of N)
	 (themata-raw (if all-ids
			  (gnosis-select '[dest source] 'links
					 `(in dest ,(vconcat all-ids)))
			(gnosis-select '[dest source] 'links)))
	 (themata-hash (let ((h (make-hash-table :test 'equal)))
			 (dolist (link themata-raw h)
			   (puthash (nth 0 link)
				    (1+ (or (gethash (nth 0 link) h) 0)) h)))))
    (mapcar
     (lambda (node)
       (let* ((id (nth 0 node))
	      (title (nth 1 node))
	      (link-count (number-to-string (or (gethash id fwd-hash) 0)))
	      (backlink-count (number-to-string (nth 2 node)))
	      (themata-links-count (number-to-string (or (gethash id themata-hash) 0))))
	 (list id (vector title link-count backlink-count themata-links-count))))
     nodes-data)))

(defun gnosis-dashboard-nodes--show-related (get-ids-fn no-results-msg &optional display-fn)
  "Show related items for the node at point.

GET-IDS-FN takes a node-id and returns related IDs.
NO-RESULTS-MSG is displayed when no related items are found.
DISPLAY-FN displays results, defaults to `gnosis-dashboard-output-nodes'."
  (let* ((node-id (tabulated-list-get-id))
         (related-ids (funcall get-ids-fn node-id))
         (display-fn (or display-fn #'gnosis-dashboard-output-nodes)))
    (if related-ids
        (progn
          (push (cons node-id gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (funcall display-fn related-ids))
      (message "%s" no-results-msg))))

(defun gnosis-dashboard-nodes-show-links ()
  "Show forward links of the node at point."
  (interactive)
  (gnosis-dashboard-nodes--show-related
   #'gnosis-dashboard-get-forward-link-ids
   "No forward links found for this node"))

(defun gnosis-dashboard-nodes-show-backlinks ()
  "Show backlinks of the node at point."
  (interactive)
  (gnosis-dashboard-nodes--show-related
   #'gnosis-dashboard-get-backlink-ids
   "No backlinks found for this node"))

(defun gnosis-dashboard-nodes-show-themata-links ()
  "Show themata that link to the node at point."
  (interactive)
  ;; Clear themata history for fresh start in themata view
  (setq gnosis-dashboard-themata-history nil)
  (gnosis-dashboard-nodes--show-related
   #'gnosis-dashboard-get-themata-links
   "No themata link to this node"
   #'gnosis-dashboard-output-themata))

(defun gnosis-dashboard-nodes-show-isolated ()
  "Show isolated nodes (nodes with no connections at all).
Isolated nodes have no backlinks, no forward links, and no themata links."
  (interactive)
  (let* ((all-nodes-data (org-gnosis-get-nodes-data))
         (isolated-ids (cl-loop for node in all-nodes-data
                               for id = (nth 0 node)
                               for backlink-count = (nth 2 node)
                               when (and (= backlink-count 0)
                                        (= (length (gnosis-dashboard-get-forward-link-ids id)) 0)
                                        (= (length (gnosis-dashboard-get-themata-links id)) 0))
                               collect id)))
    (if isolated-ids
        (progn
          ;; Save current view and position to history
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          ;; Show isolated nodes
          (gnosis-dashboard-output-nodes isolated-ids))
      (message "No isolated nodes found"))))

(defun gnosis-dashboard-nodes-search-by-title (query)
  "Search ALL nodes by title for QUERY.
Searches the database for nodes whose titles contain the search term."
  (interactive "sSearch all nodes by title: ")
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let* ((all-nodes (org-gnosis-select '[id title] 'nodes))
         (matching-ids (cl-loop for node in all-nodes
                               for id = (nth 0 node)
                               for title = (nth 1 node)
                               when (string-match-p (regexp-quote query) title)
                               collect id)))
    (if matching-ids
        (progn
          ;; Save current view to history
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes found with title matching '%s'" query))))

(defun gnosis-dashboard-nodes-filter-by-title (query)
  "Filter CURRENT nodes by title for QUERY.
Only searches within currently displayed nodes."
  (interactive "sFilter current nodes by title: ")
  (unless gnosis-dashboard-nodes-current-ids
    (user-error "No nodes to filter"))
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let* ((current-nodes (org-gnosis-select '[id title] 'nodes
                                           `(in id ,(vconcat gnosis-dashboard-nodes-current-ids))))
         (matching-ids (cl-loop for node in current-nodes
                               for id = (nth 0 node)
                               for title = (nth 1 node)
                               when (string-match-p (regexp-quote query) title)
                               collect id)))
    (if matching-ids
        (progn
          ;; Save current view to history
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes in current view match '%s'" query))))

(defun gnosis-dashboard-nodes--search-files (query &optional node-ids)
  "Search org files in `org-gnosis-dir' for QUERY, return matching node IDs.
When NODE-IDS is non-nil, only search files whose node ID is in that list."
  (let ((files (directory-files org-gnosis-dir t "^[0-9].*\\.org$"))
        (matching-ids '()))
    (dolist (file files)
      (when (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward "^:ID:[[:space:]]+\\([^[:space:]]+\\)" nil t)
            (let ((id (match-string 1)))
              (when (or (null node-ids) (member id node-ids))
                (goto-char (point-min))
                (when (search-forward query nil t)
                  (push id matching-ids))))))))
    (nreverse matching-ids)))

(defun gnosis-dashboard-nodes-search-by-content (query)
  "Search ALL nodes by file content in org-gnosis-dir."
  (interactive "sSearch all nodes by content: ")
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let ((matching-ids (gnosis-dashboard-nodes--search-files query)))
    (if matching-ids
        (progn
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes found matching '%s'" query))))

(defun gnosis-dashboard-nodes-filter-by-content (query)
  "Filter CURRENT nodes by searching file content."
  (interactive "sFilter current nodes by content: ")
  (unless gnosis-dashboard-nodes-current-ids
    (user-error "No nodes to filter"))
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let ((matching-ids (gnosis-dashboard-nodes--search-files
                       query gnosis-dashboard-nodes-current-ids)))
    (if matching-ids
        (progn
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes in current view match '%s'" query))))

(defun gnosis-dashboard-nodes-search-by-tag (tag)
  "Search ALL nodes by TAG."
  (interactive
   (list (completing-read "Search nodes by tag: "
                          (org-gnosis-select 'tag 'tags nil t)
                          nil t)))
  (when (string-empty-p tag)
    (user-error "Tag cannot be empty"))
  (let ((matching-ids (org-gnosis--nodes-by-tag tag)))
    (if matching-ids
        (progn
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes found with tag '%s'" tag))))

(defun gnosis-dashboard-nodes-filter-by-tag (tag)
  "Filter CURRENT nodes by TAG."
  (interactive
   (list (completing-read "Filter nodes by tag: "
                          (org-gnosis-select 'tag 'tags nil t)
                          nil t)))
  (unless gnosis-dashboard-nodes-current-ids
    (user-error "No nodes to filter"))
  (when (string-empty-p tag)
    (user-error "Tag cannot be empty"))
  (let* ((nodes-with-tag (org-gnosis--nodes-by-tag tag))
         (matching-ids (cl-intersection gnosis-dashboard-nodes-current-ids nodes-with-tag
                                        :test #'equal)))
    (if matching-ids
        (progn
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes in current view have tag '%s'" tag))))

(defun gnosis-dashboard-nodes-show-due ()
  "Show nodes linked to today's due themata."
  (interactive)
  (let* ((due-thema-ids (gnosis-review-get-due-themata))
         (node-ids (when due-thema-ids
                     (cl-remove-duplicates
                      (gnosis-select 'dest 'links
                                     `(in source ,(vconcat due-thema-ids)) t)
                      :test #'equal))))
    (if node-ids
        (progn
          (push (cons (tabulated-list-get-id) gnosis-dashboard-nodes-current-ids)
                gnosis-dashboard-nodes-history)
          (gnosis-dashboard-output-nodes node-ids))
      (message "No nodes linked to due themata"))))

(defun gnosis-dashboard-nodes-back ()
  "Go back to the previous nodes view, or to main dashboard if at top level."
  (interactive)
  (if gnosis-dashboard-nodes-history
      (let* ((previous (pop gnosis-dashboard-nodes-history))
             (previous-id (car previous))
             (previous-ids (cdr previous)))
        (gnosis-dashboard-output-nodes previous-ids)
        ;; Restore cursor position
        (when previous-id
          (goto-char (point-min))
          (while (and (not (eobp))
                     (not (equal (tabulated-list-get-id) previous-id)))
            (forward-line 1))))
    ;; No history - go back to main dashboard
    (gnosis-dashboard)))

(defun gnosis-dashboard-nodes-visit ()
  "Visit the node at point."
  (interactive)
  (let* ((node-id (tabulated-list-get-id))
         (title (car (org-gnosis-select 'title 'nodes `(= id ,node-id) t))))
    (org-gnosis-find title)))

(defun gnosis-dashboard-nodes-refresh ()
  "Refresh the current nodes view."
  (interactive)
  (gnosis-dashboard-output-nodes gnosis-dashboard-nodes-current-ids))

(defun gnosis-dashboard-nodes--sort-by (column &optional ascending)
  "Sort nodes dashboard by COLUMN.
If ASCENDING is non-nil, sort in ascending order, otherwise descending.
Moves cursor to the beginning of the buffer after sorting."
  (setq tabulated-list-sort-key (cons column (not ascending)))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (goto-char (point-min)))

(transient-define-prefix gnosis-dashboard-nodes-sort-menu ()
  "Sort menu for nodes dashboard."
  [["Sort By"
    ("C-t" "Title" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Title" t)))
    ("l" "Links" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Links")))
    ("b" "Backlinks" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Backlinks")))
    ("t" "Themata" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Themata")))
    ("q" "Cancel" transient-quit-one)]])

(transient-define-prefix gnosis-dashboard-nodes-search-menu ()
  "Search menu for searching ALL nodes."
  [["Search All Nodes"
    ("C-t" "By title" gnosis-dashboard-nodes-search-by-title)
    ("c" "By content" gnosis-dashboard-nodes-search-by-content)
    ("t" "By tag" gnosis-dashboard-nodes-search-by-tag)
    ("q" "Cancel" transient-quit-one)]])

(transient-define-prefix gnosis-dashboard-nodes-filter-menu ()
  "Filter menu for filtering CURRENT nodes."
  [["Filter Current Nodes"
    ("C-t" "By title" gnosis-dashboard-nodes-filter-by-title)
    ("c" "By content" gnosis-dashboard-nodes-filter-by-content)
    ("t" "By tag" gnosis-dashboard-nodes-filter-by-tag)
    ("q" "Cancel" transient-quit-one)]])

(defun gnosis-dashboard-nodes-review ()
  "Review themata for node at point."
  (interactive)
  (gnosis-review-topic (tabulated-list-get-id)))

(defun gnosis-dashboard-nodes-review-with-depth ()
  "Review themata for node at point, prompting for link depths."
  (interactive)
  (gnosis-review-topic (tabulated-list-get-id)
		       (read-number "Forward link depth: " 1)
		       (read-number "Backlink depth: " 0)))

(transient-define-prefix gnosis-dashboard-nodes-mode-menu ()
  "Transient menu for nodes dashboard mode."
  [["Navigate"
    ("RET" "Visit node" gnosis-dashboard-nodes-visit)
    ("q" "Back" gnosis-dashboard-nodes-back)
    ("g" "Refresh" gnosis-dashboard-nodes-refresh :transient t)]
   ["Search/Filter/Sort"
    ("SPC" "Search all..." gnosis-dashboard-nodes-search-menu)
    ("l" "Filter current..." gnosis-dashboard-nodes-filter-menu)
    ("s" "Sort..." gnosis-dashboard-nodes-sort-menu)]
   ["View"
    ("f" "Show links" gnosis-dashboard-nodes-show-links)
    ("b" "Show backlinks" gnosis-dashboard-nodes-show-backlinks)
    ("t" "Show themata links" gnosis-dashboard-nodes-show-themata-links)
    ("i" "Show isolated" gnosis-dashboard-nodes-show-isolated)
    ("d" "Show due" gnosis-dashboard-nodes-show-due)]
   ["Review"
    ("r" "Review topic" gnosis-dashboard-nodes-review)
    ("R" "Review with depth" gnosis-dashboard-nodes-review-with-depth)]])

(defvar-keymap gnosis-dashboard-nodes-mode-map
  :doc "Keymap for nodes dashboard."
  "?" #'gnosis-dashboard-nodes-mode-menu
  "h" #'gnosis-dashboard-nodes-mode-menu
  "q" #'gnosis-dashboard-nodes-back
  "f" #'gnosis-dashboard-nodes-show-links
  "b" #'gnosis-dashboard-nodes-show-backlinks
  "t" #'gnosis-dashboard-nodes-show-themata-links
  "i" #'gnosis-dashboard-nodes-show-isolated
  "d" #'gnosis-dashboard-nodes-show-due
  "r" #'gnosis-dashboard-nodes-review
  "R" #'gnosis-dashboard-nodes-review-with-depth
  "s" #'gnosis-dashboard-nodes-sort-menu
  "SPC" #'gnosis-dashboard-nodes-search-menu
  "l" #'gnosis-dashboard-nodes-filter-menu
  "g" #'gnosis-dashboard-nodes-refresh
  "RET" #'gnosis-dashboard-nodes-visit)

(define-minor-mode gnosis-dashboard-nodes-mode
  "Minor mode for gnosis dashboard nodes output."
  :keymap gnosis-dashboard-nodes-mode-map)

(defun gnosis-dashboard-output-nodes (&optional node-ids)
  "Display org-gnosis nodes in dashboard.
If NODE-IDS is provided, display only those nodes. Otherwise display all nodes.
Shows title, link count, backlink count, and themata links count."
  (interactive)
  (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
  (gnosis-dashboard-enable-mode)
  ;; Disable other dashboard modes
  (gnosis-dashboard-themata-mode -1)
  (gnosis-dashboard-decks-mode -1)
  (gnosis-dashboard-tags-mode -1)
  ;; Enable nodes mode
  (gnosis-dashboard-nodes-mode 1)
  (setf tabulated-list-format `[("Title" ,(/ (window-width) 2) t)
                                ("Links" ,(/ (window-width) 8) gnosis-dashboard-sort-count)
                                ("Backlinks" ,(/ (window-width) 8) gnosis-dashboard-sort-count)
                                ("Themata" ,(/ (window-width) 8) gnosis-dashboard-sort-count)]
        tabulated-list-entries nil
        ;; Set default sort based on user preferences
        ;; Note: tabulated-list uses FLIP where t=descending, nil=ascending
        ;; So we invert gnosis-dashboard-nodes-default-sort-ascending
        tabulated-list-sort-key (cons gnosis-dashboard-nodes-default-sort-column
                                      (not gnosis-dashboard-nodes-default-sort-ascending)))
  (make-local-variable 'tabulated-list-entries)
  (tabulated-list-init-header)
  (let* ((inhibit-read-only t)
         (entries (gnosis-dashboard-nodes--data node-ids))
         ;; Extract actual node IDs being displayed
         (displayed-ids (mapcar #'car entries)))
    (erase-buffer)
    (insert (format "Loading %s nodes..." (length entries)))
    (setq tabulated-list-entries entries)
    ;; Store current node IDs (now always populated)
    (setq gnosis-dashboard-nodes-current-ids displayed-ids)
    (tabulated-list-print t)
    (gnosis-dashboard--set-header-line (length entries))))

(provide 'gnosis-dashboard)
;;; gnosis-dashboard.el ends here
