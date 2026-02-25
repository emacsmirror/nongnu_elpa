;;; gnosis-tl.el --- Fast tabulated-list operations for large datasets  -*- lexical-binding: t; -*-

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

;; Fast tabulated-list operations for large datasets.
;;
;; Emacs's `tabulated-list-print' is expensive at scale: every
;; `insert' triggers modification hooks, undo recording, marker
;; adjustment, interval-tree updates, and cache invalidation.
;; With ~8 inserts per entry across 6 columns, this adds up fast.
;;
;; Key optimizations:
;;
;; 1. Wrap the erase+render cycle with `inhibit-modification-hooks',
;;    eliminating per-mutation Elisp hook dispatch.  Standard
;;    `tabulated-list-print' does NOT use this flag.
;;
;; 2. Skip `tabulated-list-column-name' text properties (one per
;;    column per entry).  Instead, `gnosis-tl--column-at-point'
;;    computes the column from cursor position and format widths.
;;
;; Also provides single-entry replace/delete operations that avoid
;; re-rendering the entire buffer when only one line changes.

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

;;; Column position detection

(defun gnosis-tl--column-at-point ()
  "Return the column index at point based on cursor position and format widths.
Computes which tabulated-list column the cursor is in by walking
through `tabulated-list-format' widths starting from `tabulated-list-padding'."
  (let* ((col (current-column))
         (pos (- col (or tabulated-list-padding 0)))
         (n-cols (length tabulated-list-format))
         (last-idx (1- n-cols))
         (accum 0))
    (if (< pos 0)
        0
      (cl-loop for i below n-cols
               for spec = (aref tabulated-list-format i)
               for width = (nth 1 spec)
               for pad-right = (if (= i last-idx) 0
                                 (or (plist-get (nthcdr 3 spec) :pad-right) 1))
               do (setq accum (+ accum width pad-right))
               when (< pos accum) return i
               finally return last-idx))))

;;; Sorting

(defun gnosis-tl--get-sorter ()
  "Return a comparison function for `tabulated-list-entries', or nil.
Mirrors `tabulated-list--get-sorter': uses `tabulated-list-sort-key'
and `tabulated-list-format' to build the comparator.  Returns nil
when no sort key is set or the column is not sortable."
  (when (and tabulated-list-sort-key
             (car tabulated-list-sort-key))
    (let* ((sort-col (car tabulated-list-sort-key))
           (n (tabulated-list--column-number sort-col))
           (sorter (nth 2 (aref tabulated-list-format n))))
      (when (eq sorter t)
        (setq sorter (lambda (a b)
                       (let ((a (aref (cadr a) n))
                             (b (aref (cadr b) n)))
                         (string< (if (stringp a) a (car a))
                                  (if (stringp b) b (car b)))))))
      (when sorter
        (if (cdr tabulated-list-sort-key)
            (lambda (a b) (funcall sorter b a))
          sorter)))))

;;; Bulk rendering

(defun gnosis-tl--render-into-buffer (entries format padding)
  "Render ENTRIES into the current buffer at point.
FORMAT is the `tabulated-list-format' vector.  PADDING is the
`tabulated-list-padding' integer.

Uses pre-computed format strings and an ASCII fast-path to
minimise per-entry overhead.  Properties set per line:
`tabulated-list-id', `tabulated-list-entry'."
  (let* ((specs (gnosis-tl--column-specs format))
         (n-cols (length specs))
         (last-idx (1- n-cols))
         (ellipsis gnosis-tl-ellipsis)
         (pad-str (when (> padding 0) (make-string padding ?\s)))
         ;; Pre-compute per-column vectors (avoid plist-get in hot loop)
         (widths    (vconcat (mapcar (lambda (s) (plist-get s :width)) specs)))
         (pad-rights (vconcat (cl-loop for s in specs for i from 0
                                       collect (if (= i last-idx) 0
                                                 (plist-get s :pad-right)))))
         (right-aligns (vconcat (mapcar (lambda (s) (plist-get s :right-align)) specs)))
         ;; Pre-computed "%-Ns" format strings for ASCII fast-path
         (fmt-strs (vconcat (cl-loop for i below n-cols
                                     collect (format "%%-%ds"
                                                     (+ (aref widths i)
                                                        (aref pad-rights i)))))))
    (dolist (entry entries)
      (let ((id (car entry))
            (cols (cadr entry))
            (beg (point))
            (i 0))
        (when pad-str (insert pad-str))
        (while (< i n-cols)
          (let* ((raw (or (aref cols i) ""))
                 (text (if (stringp raw) raw (format "%s" raw)))
                 (len (length text))
                 (ascii-p (= len (string-bytes text)))
                 (width (aref widths i)))
            (if (= i last-idx)
                ;; Last column — no padding, just truncate if needed
                (insert (if (and ascii-p (<= len width))
                            text
                          (let ((sw (if ascii-p len (string-width text))))
                            (if (> sw width)
                                (truncate-string-to-width text width nil nil ellipsis)
                              text))))
              ;; Non-last columns — pad to width + pad-right
              (if (and ascii-p (<= len width))
                  ;; Fast path: single C-level format call does padding
                  (insert (format (aref fmt-strs i) text))
                ;; Slow path: multibyte or truncation needed
                (let ((sw (if ascii-p len (string-width text)))
                      (pr (aref pad-rights i)))
                  (if (> sw width)
                      (insert (truncate-string-to-width text width nil nil ellipsis)
                              (make-string pr ?\s))
                    (if (aref right-aligns i)
                        (insert (make-string (- width sw) ?\s) text
                                (make-string pr ?\s))
                      (insert text (make-string (+ (- width sw) pr) ?\s))))))))
          (setq i (1+ i)))
        (insert ?\n)
        (add-text-properties beg (point)
                             (list 'tabulated-list-id id
                                   'tabulated-list-entry cols))))))

(defun gnosis-tl-render-lines (entries format padding)
  "Render ENTRIES into a single propertized string.
FORMAT is the `tabulated-list-format' vector.  PADDING is the
`tabulated-list-padding' integer.  Returns the concatenated text
of all formatted lines.  Pure function — no buffer side effects."
  (with-temp-buffer
    (let ((inhibit-modification-hooks t))
      (gnosis-tl--render-into-buffer entries format padding))
    (buffer-string)))

(defun gnosis-tl-print (&optional remember-pos)
  "Fast drop-in replacement for `tabulated-list-print'.
Renders directly into the current buffer using optimised bulk
insertion.  When REMEMBER-POS is non-nil, restore point to the
same entry ID and column."
  (let* ((saved-id (and remember-pos (tabulated-list-get-id)))
         (saved-col (and remember-pos (current-column)))
         (inhibit-read-only t)
         (entries (if (functionp tabulated-list-entries)
                      (funcall tabulated-list-entries)
                    tabulated-list-entries))
         (sorter (gnosis-tl--get-sorter)))
    (when sorter
      (setq entries (sort entries sorter))
      (unless (functionp tabulated-list-entries)
        (setq tabulated-list-entries entries)))
    (let ((inhibit-modification-hooks t))
      (erase-buffer)
      (gnosis-tl--render-into-buffer entries tabulated-list-format
                                     (or tabulated-list-padding 0)))
    (set-buffer-modified-p nil)
    (if (and saved-id remember-pos)
        (progn
          (goto-char (point-min))
          (while (and (not (eobp))
                      (not (equal (get-text-property (point) 'tabulated-list-id)
                                  saved-id)))
            (forward-line 1))
          (when saved-col
            (move-to-column saved-col)))
      (goto-char (point-min)))))

(defun gnosis-tl-sort (&optional n)
  "Sort the current tabulated-list by column at point.
Like `tabulated-list-sort' but re-renders with `gnosis-tl-print'.
With numeric prefix N, sort the Nth column.  With prefix -1,
restore original order."
  (interactive "P")
  (when (and n (or (>= n (length tabulated-list-format))
                   (< n -1)))
    (user-error "Invalid column number"))
  (if (equal n -1)
      (progn
        (setq tabulated-list-sort-key nil)
        (tabulated-list-init-header)
        (gnosis-tl-print t))
    (let ((name (car (aref tabulated-list-format
                          (if n n (gnosis-tl--column-at-point))))))
      (unless (nth 2 (assoc name (append tabulated-list-format nil)))
        (user-error "Cannot sort by %s" name))
      (if (equal name (car tabulated-list-sort-key))
          (setcdr tabulated-list-sort-key
                  (not (cdr tabulated-list-sort-key)))
        (setq tabulated-list-sort-key (cons name nil)))
      (tabulated-list-init-header)
      (gnosis-tl-print t))))

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
