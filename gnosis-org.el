;;; gnosis-org.el --- Org module for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; This module provides parsing of org-mode buffers for gnosis.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)

(defun gnosis-org--global-props (name &optional buffer)
  "Get the plists of global org properties by NAME in BUFFER.

NAME is a string representing the property name to search for.
BUFFER defaults to the current buffer if not specified."
  (cl-assert (stringp name) nil "NAME must be a string.")
  (with-current-buffer (or buffer (current-buffer))
    (let ((elements (org-element-map (org-element-parse-buffer) 'keyword
                      (lambda (el)
                        (when (string= (org-element-property :key el) name)
                          el))
                      nil t)))
      (if elements elements
        (message "No properties found for %s" name)
        nil))))

(defun gnosis-org--heading-props (property &optional buffer)
  "Get the values of a custom PROPERTY from all headings in BUFFER.

PROPERTY is a string representing the property name to search for.
BUFFER defaults to the current buffer if not specified."
  (cl-assert (stringp property) nil "PROPERTY must be a string.")
  (with-current-buffer (or buffer (current-buffer))
    (let ((results nil))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (let ((prop (org-element-property (intern (concat ":" property)) headline)))
            (when prop
              (push prop results)))))
      (if results (reverse results)
        (message "No custom properties found for %s" property)
        nil))))

(defun gnosis-org--insert-read-only (string)
  "Insert STRING as read-only."
  (let ((start (point)))
    (insert string)
    ;; Set the just inserted string as read-only
    (add-text-properties start (point) '(read-only t))
    ;; Since the space is inserted outside of the read-only region, it's editable
    (let ((inhibit-read-only t))
      (insert " "))))

(defun gnosis-org-make-read-only (&rest values)
  "Make the provided VALUES read-only in the whole buffer."
  (goto-char (point-min))
  (dolist (value values)
    (while (search-forward value nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'read-only t)))
  (goto-char (point-min)))

(cl-defun gnosis-org--insert-thema (id type &optional keimenon hypothesis apocalypse parathema tags example)
  "Insert thema for note ID.

TYPE: Thema type, refer to `gnosis-thema-types'
KEIMENON: Text user is first presented with.
HYPOTHESIS: Hypothesis for what the APOCALYPSE is
APOCALYPSE: The revelation after KEIMENON
PARATHEMA: The text where THEMA is derived from.
TAGS: List of THEMA tags
EXAMPLE: Boolean value, if non-nil do not add properties for thema."
  (let ((components `(("** Keimenon" . ,keimenon)
                      ("** Hypothesis" . ,hypothesis)
                      ("** Apocalypse" . ,apocalypse)
                      ("** Parathema" . ,parathema))))
    (insert "\n* Thema")
    (org-set-tags tags)
    (unless example
      (org-set-property "GNOSIS_ID" id)
      (org-set-property "GNOSIS_TYPE" type)
      (gnosis-org-make-read-only ":PROPERTIES:"
				 (format "GNOSIS_ID: %s" id)
				 (format "GNOSIS_TYPE: %s" type)
				 ":END:"))
    (dolist (comp components)
      (goto-char (point-max))
      (gnosis-org--insert-read-only (car comp))
      (insert "\n" (or (cdr comp) "") "\n\n"))))

(defun gnosis-org-parse--deck-name (&optional parsed-data)
  "Retrieve deck name from PARSED-DATA."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
	 (title (org-element-map parsed-data 'keyword
		  (lambda (kw)
		    (when (string= (org-element-property :key kw) "DECK")
                      (org-element-property :value kw)))
		  nil t)))
    title))

(defun gnosis-org-parse-themas ()
  "Extract content for each level-2 heading for thema headings with a GNOSIS_ID."
  (let (results)
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
                  (let ((child-text (org-element-interpret-data (org-element-contents child))))
                    (setq child-text (string-trim child-text))
                    (if (string-empty-p child-text)
                        (push nil entry) ; Push nil if the content is empty
                      (push (substring-no-properties child-text) entry)))))
              (push tags entry) ;; Add tags last
              (push (nreverse entry) results)))))
      nil nil)
    results))

;;;; TODO: Rewrite function that export deck without read-only values.
;;;; Make them only with built-in to work with async.el
;; (defun gnosis-org-export-deck (deck)
;;   "Export DECK in an org file."
;;   (interactive (list (gnosis--get-deck-id)))
;;   ;; (find-file (read-file-name "File: "))
;;   ;; TODO: Retrieve all values instead of just ids and then insert them async
;;   (let* ((notes (append (gnosis-select '[type keimenon hypothesis apocalypse tags] 'notes `(= deck-id ,deck))
;; 			;; (gnosis-select 'parathema 'extras `(= deck-id ,deck) t)
;; 			nil))
;; 	 (deck-name (car (gnosis-select 'name 'decks `(= id ,deck) t))))
;;     (async-start
;;      (lambda () (let ((inhibit-read-only 1))
;; 	     (find-file (format "/tmp/%s.org" (downcase deck-name)))
;; 	     (erase-buffer)
;; 	     (org-mode)
;; 	     (insert "#+DECK: " deck-name "\n\n")
;; 	     (cl-loop for note in notes
;; 		      do
;; 		      (insert "\n* Thema")
;; 		      (insert "\n** Keimenon\n")
;; 		      (insert (format "\n%s\n" (nth 1 note))))
;; 	     (save-buffer)))
;;     ;; (let ((inhibit-read-only 1))
;;     ;;   (erase-buffer))
;;     ;; (org-mode)
;;     ;; (insert "#+DECK: " deck-name)
;;     ;; (org-set-property "GNOSIS_DECK" (number-to-string deck))
;;     ;; (goto-char (point-max))
;;     ;; (cl-loop for note in notes
;;     ;; 	     do (gnosis-export-note note))
;;     ;; (save-buffer)
;;     )))

(provide 'gnosis-org)
;;; gnosis-org.el ends here.
