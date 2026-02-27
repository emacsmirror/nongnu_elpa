;;; gnosis-export-import.el --- Export/import for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

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

;; Export and import operations for gnosis decks and themata.
;;
;; This module handles:
;; - Exporting decks to org-mode files (`gnosis-export-deck',
;;   `gnosis-export-deck-async')
;; - Importing decks from org-mode files (`gnosis-import-deck',
;;   `gnosis-import-deck-async')
;; - Parsing exported org buffers (`gnosis-export-parse-themata',
;;   `gnosis-export-parse--deck-name')
;; - Saving edited themata (`gnosis-save', `gnosis-save-deck')

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'org)
(require 'org-element)

(defun gnosis-export--insert-read-only (string)
  "Insert STRING as read-only."
  (let ((start (point)))
    (insert string)
    ;; Set the just inserted string as read-only
    (add-text-properties start (point) '(read-only t))
    ;; Since the space is inserted outside of the read-only region, it's editable
    (let ((inhibit-read-only t))
      (insert " "))))

(cl-defun gnosis-export--insert-thema (id type &optional keimenon hypothesis
				      answer parathema tags example)
  "Insert thema for thema ID.

TYPE: Thema type, refer to `gnosis-thema-types'
KEIMENON: Text user is first presented with.
HYPOTHESIS: Hypothesis for what the ANSWER is
ANSWER: The revelation after KEIMENON
PARATHEMA: The text where THEMA is derived from.
TAGS: List of THEMA tags
EXAMPLE: Boolean value, if non-nil do not add properties for thema."
  (let ((components `(("** Keimenon" . ,keimenon)
                      ("** Hypothesis" . ,hypothesis)
                      ("** Answer" . ,answer)
                      ("** Parathema" . ,parathema))))
    (goto-char (point-max))
    (insert "\n* Thema")
    (when tags
      (insert " :" (mapconcat #'identity tags ":") ":"))
    (insert "\n")
    (unless example
      (let ((start (point)))
        (insert ":PROPERTIES:\n:GNOSIS_ID: " id "\n:GNOSIS_TYPE: " type "\n:END:\n")
        (add-text-properties start (point)
			    '(read-only t rear-nonsticky (read-only)))))
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

(defun gnosis-export-parse-themata (&optional separator)
  "Extract content for each level-2 heading for thema headings with a GNOSIS_ID.

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
            (let ((line (line-number-at-pos
                         (org-element-property :begin headline)))
                  entry)
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
              (push line entry)
              (push (nreverse entry) results)))))
      nil nil)
    results))

(defun gnosis-export-themata (ids &optional new-p)
  "Export themata for IDS.

If NEW-P replace the ids of themata with NEW, used for new themata to
generate new thema id."
  (cl-assert (listp ids) nil "IDS value must be a list.")
  ;; Extract just the ID values if they're in a list structure
  (let ((id-values (mapcar (lambda (id)
                             (if (listp id) (car id) id))
                           ids)))
    ;; Process each thema
    (dolist (id id-values)
      (let ((thema-data (append (gnosis-select '[type keimenon hypothesis answer tags]
                                              'themata `(= id ,id) t)
                               (gnosis-select 'parathema 'extras `(= id ,id) t))))
        (gnosis-export--insert-thema
         (if new-p "NEW" (number-to-string id))
         (nth 0 thema-data)
         (nth 1 thema-data)
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 2 thema-data) gnosis-export-separator))
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 3 thema-data) gnosis-export-separator))
         (nth 5 thema-data)
         (nth 4 thema-data))))))

(defun gnosis-export-deck (&optional deck filename new-p include-suspended)
  "Export contents of DECK to FILENAME.

When NEW-P, replace thema IDs with NEW for fresh import.
When INCLUDE-SUSPENDED, also export suspended themata."
  (interactive (list (gnosis--get-deck-id)
                     (read-file-name "Export to file: ")
		     (not (y-or-n-p "Export with current thema ids? "))
		     (y-or-n-p "Include suspended themata? ")))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (deck-name (gnosis--get-deck-name deck))
	 (filename (if (file-directory-p filename)
		       (expand-file-name deck-name filename)
		     filename)))
    (unless (string-match-p "\\.org$" filename)
      (setq filename (concat (or filename deck-name) ".org")))
    (with-current-buffer (get-buffer-create (format "EXPORT: %s" deck-name))
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (org-mode)
        (erase-buffer)
        (insert (format "#+DECK: %s\n" deck-name))
        ;; Batch-fetch: 2 queries instead of 2*N
        (let* ((all-themata (emacsql (gnosis--ensure-db)
                             [:select [id type keimenon hypothesis answer tags]
                              :from themata :where (= deck-id $s1)] deck))
               (all-ids (mapcar #'car all-themata))
               (suspended-ids (when (and all-ids (not include-suspended))
                                (mapcar #'car
                                        (emacsql (gnosis--ensure-db)
                                         [:select id :from review-log
                                          :where (and (in id $v1) (= suspend 1))]
                                         (vconcat all-ids)))))
               (all-themata (if suspended-ids
                                (cl-remove-if (lambda (row)
                                                (member (car row) suspended-ids))
                                              all-themata)
                              all-themata))
               (all-ids (mapcar #'car all-themata))
               (all-extras (when all-ids
                             (emacsql (gnosis--ensure-db)
                              [:select [id parathema] :from extras
                               :where (in id $v1)] (vconcat all-ids))))
               (extras-ht (let ((ht (make-hash-table :test 'equal
                                                     :size (length all-ids))))
                            (dolist (row all-extras ht)
                              (puthash (car row) (cadr row) ht)))))
          (insert (format "#+THEMATA: %d\n\n" (length all-themata)))
          (dolist (row all-themata)
            (let* ((id (nth 0 row))
                   (type (nth 1 row))
                   (hypothesis (nth 3 row))
                   (answer (nth 4 row))
                   (tags (nth 5 row))
                   (parathema (gethash id extras-ht "")))
              (gnosis-export--insert-thema
               (if new-p "NEW" (number-to-string id))
               type
               (nth 2 row)
               (concat (string-remove-prefix "\n" gnosis-export-separator)
                       (mapconcat #'identity hypothesis gnosis-export-separator))
               (concat (string-remove-prefix "\n" gnosis-export-separator)
                       (mapconcat #'identity answer gnosis-export-separator))
               parathema
               tags)))
          (when filename
            (write-file filename)
            (message "Exported deck to %s" filename)))))))

(defun gnosis-export-deck-async (&optional deck filename new-p include-suspended
                                           chunk-size)
  "Export contents of DECK to FILENAME asynchronously.

Like `gnosis-export-deck' but uses `run-with-timer' between chunks
so Emacs stays responsive during large exports.  CHUNK-SIZE controls
how many themata to insert per batch (default 500).

When NEW-P, replace thema IDs with NEW for fresh import.
When INCLUDE-SUSPENDED, also export suspended themata."
  (interactive (list (gnosis--get-deck-id)
                     (read-file-name "Export to file: ")
                     (not (y-or-n-p "Export with current thema ids? "))
                     (y-or-n-p "Include suspended themata? ")))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (chunk-size (or chunk-size 500))
         (deck-name (gnosis--get-deck-name deck))
         (filename (if (file-directory-p filename)
                       (expand-file-name deck-name filename)
                     filename)))
    (unless (string-match-p "\\.org$" filename)
      (setq filename (concat (or filename deck-name) ".org")))
    (let ((buffer (get-buffer-create (format "EXPORT: %s" deck-name))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (buffer-disable-undo)
          (org-mode)
          (erase-buffer)
          (insert (format "#+DECK: %s\n" deck-name))
          ;; Batch-fetch: 2 queries instead of 2*N
          (let* ((all-themata (emacsql (gnosis--ensure-db)
                               [:select [id type keimenon hypothesis answer tags]
                                :from themata :where (= deck-id $s1)] deck))
                 (all-ids (mapcar #'car all-themata))
                 (suspended-ids (when (and all-ids (not include-suspended))
                                  (mapcar #'car
                                          (emacsql (gnosis--ensure-db)
                                           [:select id :from review-log
                                            :where (and (in id $v1) (= suspend 1))]
                                           (vconcat all-ids)))))
                 (all-themata (if suspended-ids
                                  (cl-remove-if (lambda (row)
                                                  (member (car row) suspended-ids))
                                                all-themata)
                                all-themata))
                 (all-ids (mapcar #'car all-themata))
                 (all-extras (when all-ids
                               (emacsql (gnosis--ensure-db)
                                [:select [id parathema] :from extras
                                 :where (in id $v1)] (vconcat all-ids))))
                 (extras-ht (let ((ht (make-hash-table :test 'equal
                                                       :size (length all-ids))))
                              (dolist (row all-extras ht)
                                (puthash (car row) (cadr row) ht))))
                 (total (length all-themata)))
            (insert (format "#+THEMATA: %d\n\n" total))
            (message "Exporting %d themata..." total)
            (cl-labels
                ((process-next (remaining exported)
                   (if (null remaining)
                       (progn
                         (when filename
                           (with-current-buffer buffer
                             (write-file filename))
                           (message "Exported deck to %s" filename)))
                     (let ((count 0))
                       (with-current-buffer buffer
                         (let ((inhibit-read-only t))
                           (while (and remaining (< count chunk-size))
                             (let* ((row (car remaining))
                                    (id (nth 0 row))
                                    (type (nth 1 row))
                                    (hypothesis (nth 3 row))
                                    (answer (nth 4 row))
                                    (tags (nth 5 row))
                                    (parathema (gethash id extras-ht "")))
                               (gnosis-export--insert-thema
                                (if new-p "NEW" (number-to-string id))
                                type
                                (nth 2 row)
                                (concat (string-remove-prefix "\n" gnosis-export-separator)
                                        (mapconcat #'identity hypothesis
                                                   gnosis-export-separator))
                                (concat (string-remove-prefix "\n" gnosis-export-separator)
                                        (mapconcat #'identity answer
                                                   gnosis-export-separator))
                                parathema
                                tags))
                             (setq remaining (cdr remaining))
                             (cl-incf count))))
                       (let ((new-exported (+ exported count)))
                         (message "Exporting... %d/%d themata" new-exported total)
                         (run-with-timer 0.01 nil
                                         #'process-next remaining new-exported))))))
              (process-next all-themata 0))))))))

(defun gnosis-save-thema (thema deck)
  "Save THEMA for DECK.
Returns nil on success, or an error message string on failure."
  (let* ((id (nth 0 thema))
	 (type (nth 1 thema))
	 (keimenon (nth 2 thema))
	 (hypothesis (nth 3 thema))
	 (answer (nth 4 thema))
	 (parathema (or (nth 5 thema) ""))
	 (tags (nth 6 thema))
	 (line (nth 7 thema))
	 (links (append (gnosis-extract-id-links parathema)
			(gnosis-extract-id-links keimenon)))
	 (thema-func (cdr (assoc (downcase type)
				  (mapcar (lambda (pair) (cons (downcase (car pair))
							  (cdr pair)))
					  gnosis-thema-types)))))
    (condition-case err
        (progn
          (funcall thema-func id deck type keimenon hypothesis
	           answer parathema tags 0 links)
          nil)
      (error (format "Line %s (id:%s): %s" (or line "?") id
                     (error-message-string err))))))

(defun gnosis-save ()
  "Save themata in current buffer."
  (interactive nil gnosis-edit-mode)
  (let* ((gc-cons-threshold most-positive-fixnum)
         (themata (gnosis-export-parse-themata))
	 (deck (gnosis--get-deck-id (gnosis-export-parse--deck-name)))
	 (gnosis--id-cache (let ((ht (make-hash-table :test 'equal)))
			     (dolist (id (gnosis-select 'id 'themata nil t) ht)
			       (puthash id t ht))))
	 (errors nil)
	 (edited-id (string-to-number (caar themata))))
    (emacsql-with-transaction (gnosis--ensure-db)
      (cl-loop for thema in themata
	       for err = (gnosis-save-thema thema deck)
	       when err do (push err errors)))
    (if errors
        (user-error "Failed to import %d thema(ta):\n%s"
                    (length errors) (mapconcat #'identity (nreverse errors) "\n"))
      (gnosis-edit-quit)
      (run-hook-with-args 'gnosis-save-hook edited-id))))

;;;###autoload
(defun gnosis-save-deck (deck-name)
  "Save themata for deck with DECK-NAME.

If a deck with DECK-NAME already exists, prompt for confirmation
before importing into it."
  (interactive
   (progn
     (unless (eq major-mode 'org-mode)
       (user-error "This function can only be used in org-mode buffers"))
     (list (read-string "Deck name: " (gnosis-export-parse--deck-name)))))
  (when (and (gnosis-get 'id 'decks `(= name ,deck-name))
	     (not (y-or-n-p (format "Deck '%s' already exists.  Import into it? "
				    deck-name))))
    (user-error "Aborted"))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (themata (gnosis-export-parse-themata))
	 (deck (gnosis-get-deck-id deck-name))
	 (gnosis--id-cache (let ((ht (make-hash-table :test 'equal)))
			     (dolist (id (gnosis-select 'id 'themata nil t) ht)
			       (puthash id t ht))))
	 (errors nil))
    (emacsql-with-transaction (gnosis--ensure-db)
      (cl-loop for thema in themata
	       for err = (gnosis-save-thema thema deck)
	       when err do (push err errors)))
    (if errors
        (user-error "Failed to import %d thema(ta):\n%s"
                    (length errors) (mapconcat #'identity (nreverse errors) "\n"))
      (message "Imported %d themata for deck '%s'" (length themata) deck-name))))

;;;###autoload
(defun gnosis-import-deck (file)
  "Save gnosis deck from FILE."
  (interactive "fFile: ")
  (let ((gc-cons-threshold most-positive-fixnum))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (gnosis-save-deck (gnosis-export-parse--deck-name)))))

(defun gnosis--import-split-chunks (text chunk-size)
  "Split org TEXT into chunks of CHUNK-SIZE themata.

Return a list of strings, each containing up to CHUNK-SIZE
`* Thema' headings."
  (let ((headings '())
        (start 0))
    ;; Find all `* Thema' positions
    (while (string-match "^\\* Thema" text start)
      (push (match-beginning 0) headings)
      (setf start (1+ (match-beginning 0))))
    (setq headings (nreverse headings))
    (let ((chunks '())
          (total (length headings)))
      (cl-loop for i from 0 below total by chunk-size
               for beg = (nth i headings)
               for end-idx = (min (+ i chunk-size) total)
               for end = (if (< end-idx total)
                             (nth end-idx headings)
                           (length text))
               do (push (substring text beg end) chunks))
      (nreverse chunks))))

(defun gnosis--import-chunk (header chunk deck-id id-cache)
  "Import a single CHUNK of org text.

HEADER is the #+DECK line to prepend.  DECK-ID is the resolved
deck id.  ID-CACHE is the shared `gnosis--id-cache' hash table.
Returns a list of error strings (nil on full success)."
  (let ((gc-cons-threshold most-positive-fixnum)
        (gnosis--id-cache id-cache)
        (errors nil))
    (with-temp-buffer
      (insert header "\n" chunk)
      (org-mode)
      (let ((themata (gnosis-export-parse-themata)))
        (emacsql-with-transaction (gnosis--ensure-db)
          (cl-loop for thema in themata
                   for err = (gnosis-save-thema thema deck-id)
                   when err do (push err errors)))))
    (nreverse errors)))

;;;###autoload
(defun gnosis-import-deck-async (file &optional chunk-size)
  "Import gnosis deck from FILE asynchronously in chunks.

CHUNK-SIZE controls how many themata to process per batch
\(default 500).  Uses `run-with-timer' between chunks so Emacs
stays responsive.  Progress is reported in the echo area."
  (interactive "fFile: ")
  (let* ((chunk-size (or chunk-size 500))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         ;; Extract header (everything before first `* Thema')
         (header-end (or (string-match "^\\* Thema" text) 0))
         (header (string-trim-right (substring text 0 header-end)))
         (deck-name (with-temp-buffer
                      (insert header)
                      (org-mode)
                      (gnosis-export-parse--deck-name)))
         (deck-id (progn
                    (when (and (gnosis-get 'id 'decks `(= name ,deck-name))
                               (not (y-or-n-p
                                     (format "Deck '%s' already exists.  Import into it? "
                                             deck-name))))
                      (user-error "Aborted"))
                    (gnosis-get-deck-id deck-name)))
         (id-cache (let ((ht (make-hash-table :test 'equal)))
                     (dolist (id (gnosis-select 'id 'themata nil t) ht)
                       (puthash id t ht))))
         (chunks (gnosis--import-split-chunks text chunk-size))
         (total-chunks (length chunks))
         ;; Count total themata from the text
         (total-themata (with-temp-buffer
                          (insert text)
                          (count-matches "^\\* Thema" (point-min) (point-max))))
         (imported 0)
         (all-errors '()))
    (message "Importing %d themata in %d chunks..." total-themata total-chunks)
    (cl-labels
        ((process-next (remaining chunk-n)
           (if (null remaining)
               ;; Done
               (if all-errors
                   (message "Import complete: %d themata, %d errors"
                            imported (length all-errors))
                 (message "Import complete: %d themata for deck '%s'"
                          imported deck-name))
             (let* ((chunk (car remaining))
                    (errors (gnosis--import-chunk header chunk deck-id id-cache))
                    ;; Count headings in this chunk
                    (n (with-temp-buffer
                         (insert chunk)
                         (count-matches "^\\* Thema" (point-min) (point-max)))))
               (setq imported (+ imported n))
               (when errors
                 (setq all-errors (append all-errors errors)))
               (message "Importing... %d/%d themata (chunk %d/%d)"
                        imported total-themata chunk-n total-chunks)
               (run-with-timer 0.01 nil
                               #'process-next (cdr remaining) (1+ chunk-n))))))
      (process-next chunks 1))))

(provide 'gnosis-export-import)
;;; gnosis-export-import.el ends here
