;;; gnosis-tl.el --- Fast single-entry tabulated-list operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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

;; Single-entry replace and delete operations for tabulated-list buffers.
;; Avoids calling `tabulated-list-print' (which re-renders ALL entries)
;; when only one line needs to change.  Full renders still use the
;; standard `tabulated-list-print'.

;;; Code:

(require 'tabulated-list)
(require 'cl-lib)

(defvar gnosis-tl-ellipsis "..."
  "String appended to truncated column text.")

;;; Column spec handling (pure)

(defun gnosis-tl--column-specs (format)
  "Extract column specs from tabulated-list FORMAT vector.
Returns list of plists: (:name :width :pad-right :right-align).
The last column gets pad-right 0 (no trailing space)."
  (let ((len (length format)))
    (cl-loop for i below len
             for col = (aref format i)
             for props = (nthcdr 3 col)
             collect (list :name (nth 0 col)
                           :width (nth 1 col)
                           :pad-right (if (= i (1- len)) 0
                                        (or (plist-get props :pad-right) 1))
                           :right-align (plist-get props :right-align)))))

;;; Line formatting (pure -- string in, string out)

(defun gnosis-tl--pad-column (text width pad-right right-align)
  "Pad or truncate TEXT to WIDTH with PAD-RIGHT extra spaces.
When RIGHT-ALIGN is non-nil, right-align text within WIDTH.
Truncated text is suffixed with `gnosis-tl-ellipsis'."
  (let* ((truncated (if (> (string-width text) width)
                        (truncate-string-to-width text width nil nil
                                                  gnosis-tl-ellipsis)
                      text))
         (padding (max 0 (- width (string-width truncated)))))
    (if right-align
        (concat (make-string padding ?\s) truncated (make-string pad-right ?\s))
      (concat truncated (make-string (+ padding pad-right) ?\s)))))

(defun gnosis-tl--format-line (id cols specs)
  "Build a propertized line string for entry ID with COLS and SPECS.
COLS is the entry's column vector.  SPECS is from `gnosis-tl--column-specs'.
Returns a single string with tabulated-list text properties attached."
  (let ((parts (list (make-string (or tabulated-list-padding 0) ?\s)))
        (last-idx (1- (length specs))))
    (cl-loop for spec in specs
             for i from 0
             for raw = (or (aref cols i) "")
             for text = (if (stringp raw) raw (format "%s" raw))
             for width = (plist-get spec :width)
             for segment = (if (= i last-idx)
                               (if (> (string-width text) width)
                                   (truncate-string-to-width text width nil nil
                                                             gnosis-tl-ellipsis)
                                 text)
                             (gnosis-tl--pad-column
                              text width
                              (plist-get spec :pad-right)
                              (plist-get spec :right-align)))
             do (push (propertize segment
                                  'tabulated-list-column-name
                                  (plist-get spec :name))
                      parts))
    (let ((line (concat (apply #'concat (nreverse parts)) "\n")))
      (add-text-properties 0 (length line)
                           `(tabulated-list-id ,id
                             tabulated-list-entry ,cols)
                           line)
      line)))

;;; Single-entry buffer operations

(defun gnosis-tl-replace-entry (id new-cols)
  "Replace the displayed line for entry ID with NEW-COLS in place.
NEW-COLS is the column vector for the entry.
Point is preserved via `save-excursion'."
  (let ((inhibit-read-only t)
        (specs (gnosis-tl--column-specs tabulated-list-format)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'tabulated-list-id) id)))
        (forward-line 1))
      (when (and (not (eobp))
                 (equal (get-text-property (point) 'tabulated-list-id) id))
        (let ((beg (line-beginning-position))
              (end (progn (forward-line 1) (point))))
          (delete-region beg end)
          (goto-char beg)
          (insert (gnosis-tl--format-line id new-cols specs)))))))

(defun gnosis-tl-delete-entry (id)
  "Delete the displayed line for entry ID from the buffer.
Point is preserved via `save-excursion'."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'tabulated-list-id) id)))
        (forward-line 1))
      (when (and (not (eobp))
                 (equal (get-text-property (point) 'tabulated-list-id) id))
        (delete-region (line-beginning-position)
                       (progn (forward-line 1) (point)))))))

(provide 'gnosis-tl)
;;; gnosis-tl.el ends here
