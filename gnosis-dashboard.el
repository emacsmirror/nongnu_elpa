;;; gnosis-dashboard.el --- Spaced Repetition Algorithm for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Thanos Apollo

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

;; This an extension of gnosis.el

;;; Code:
(require 'cl-lib)
(require 'calendar)

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

(defvar gnosis-dashboard-note-ids nil
  "Store note ids for dashboard.")

(defvar gnosis-dashboard-search-value nil
  "Store search value.")

(defface gnosis-dashboard-header-face
  '((t :inherit (outline-1) :weight bold))
  "Face for the dashboard header.."
  :group 'gnosis)
(defun gnosis-dashboard-output-note (id)
  "Output contents for note with ID, formatted for gnosis dashboard."
  (cl-loop for item in (append (gnosis-select '[main options answer tags type] 'notes `(= id ,id) t)
			       (gnosis-select 'suspend 'review-log `(= id ,id) t))
           if (listp item)
           collect (mapconcat #'identity item ",")
           else
           collect (replace-regexp-in-string "\n" " " (format "%s" item))))

(defun gnosis-dashboard-output-notes (note-ids)
  "Return NOTE-IDS contents on gnosis dashboard."
  (cl-assert (listp note-ids) t "`note-ids' must be a list of note ids.")
  (pop-to-buffer "*gnosis-dashboard*")
  (gnosis-dashboard-mode)
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
  (local-set-key (kbd "r") #'gnosis-dashboard)
  (local-set-key (kbd "d") #'(lambda () (interactive)
			       (gnosis-delete-note (string-to-number (tabulated-list-get-id)))
			       (gnosis-dashboard-output-notes gnosis-dashboard-note-ids)
			       (revert-buffer t t t)))
  (local-unset-key (kbd "RET")))

(defun gnosis-dashboard-deck-note-count (id)
  "Return total note count for deck with ID."
  (let ((note-count (length (gnosis-select 'id 'notes `(= deck-id ,id) t))))
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
  (setq tabulated-list-entries
	(cl-loop for id in (gnosis-select 'id 'decks '1=1 t)
		 for output = (gnosis-dashboard-output-deck id)
		 when output
		 collect (list (number-to-string id) (vconcat output))))
  (local-set-key (kbd "e") #'gnosis-dashboard-edit-deck)
  (local-set-key (kbd "a") #'(lambda () "Add deck & refresh" (interactive)
			       (gnosis-add-deck (read-string "Deck name: "))
			       (gnosis-dashboard-output-decks)
			       (revert-buffer t t t)))
  (local-set-key (kbd "s") #'(lambda () "Suspend notes" (interactive)
			       (gnosis-suspend-deck
				(string-to-number (tabulated-list-get-id)))
			       (gnosis-dashboard-output-decks)
			       (revert-buffer t t t)))
  (local-set-key (kbd "d") #'(lambda () "Delete deck" (interactive)
			       (gnosis-delete-deck (string-to-number (tabulated-list-get-id)))
			       (gnosis-dashboard-output-decks)
			       (revert-buffer t t t)))
  (local-set-key (kbd "RET") #'(lambda () "View notes of deck" (interactive)
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


(provide 'gnosis-dashboard)
;;; gnosis-dashboard.el ends here
