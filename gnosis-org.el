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

;; Under development.

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
;; TODO: Add support for tags.
(cl-defun gnosis-org-insert-heading (&key main id answer type)
  "Insert an Org heading in current buffer.

- MAIN as the title.
- ID as GNOSIS_ID.
- ANSWER as the subheading.
- TYPE as the note type.

If BUFFER is not specified, defaults to the current buffer."
  (cl-assert (stringp main) nil "MAIN must be a string representing the heading title.")
  (cl-assert (stringp id) nil "ID must be a string representing the GNOSIS_ID.")
  (cl-assert (stringp type) nil "TYPE must be a string representing the TYPE property.")
  (let ((main (if (string-match-p "\n" main) (replace-regexp-in-string "\n" "\\\\n" main) main))
	(answer (cond ((stringp answer)
		       answer)
		      ((numberp answer)
		       (number-to-string answer))
		      (t (mapconcat 'identity answer ", ")))))
    (goto-char (point-max)) ;; Ensure we're at the end of the buffer
    (insert (format "* %s\n:PROPERTIES:\n:GNOSIS_ID: %s\n:TYPE: %s\n:END:\n** %s\n"
		    main id type answer))
    (message "Inserted heading: %s with GNOSIS_ID %s and TYPE %s" main id type)))

(provide 'gnosis-org)
;;; gnosis-org.el ends here.
