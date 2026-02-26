;;; gnosis-utils.el --- Gnosis Utilities Module  -*- lexical-binding: t; -*-

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

;; Module that provides general utilities for gnosis

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun gnosis-utils-trim-quotes (str)
  "Remove prefix and suffix quotes for STR."
  (string-remove-prefix "\"" (string-remove-suffix "\"" str)))


(defun gnosis-utils-highlight-words (str words face &optional default-face)
  "Highlight WORDS in STR with FACE.

Replaces first occurence of word in STR with FACE.

Optionally, use custom DEFAULT-FACE."
  (cl-assert (listp words) nil "Words to mark must be a list.")
  (with-temp-buffer
    (insert (if default-face (propertize str 'face default-face) str))
    (goto-char (point-min))
    (dolist (answer words)
      (let ((answer-text (gnosis-utils-trim-quotes answer)))
        (when (search-forward answer-text nil t)
          (replace-match
           (mapconcat
            (lambda (char)
              (if (not (memq char '(?\s ?\t ?\n)))
                  (propertize (char-to-string char) 'face face)
                (char-to-string char)))
            answer-text
            "")
           nil t))))
    (buffer-string)))

(defconst gnosis-utils--org-link-re
  "\\[\\[[^]]*\\]\\[[^]]*\\]\\]\\|\\[\\[[^]]*\\]\\]"
  "Regexp matching org-mode links: [[target][desc]] or [[target]].")

(defun gnosis-utils-string-outside-links-p (text string)
  "Return non-nil if STRING appears in TEXT outside of org-links."
  (let ((case-fold-search nil)
        (target (regexp-quote string))
        (pos 0))
    (catch 'found
      (while (string-match gnosis-utils--org-link-re text pos)
        (when (string-match-p target (substring text pos (match-beginning 0)))
          (throw 'found t))
        (setq pos (match-end 0)))
      (string-match-p target (substring text pos)))))

(defun gnosis-utils-replace-string-with-link (text string node-id)
  "Replace STRING in TEXT with org-link to NODE-ID, skipping existing links.
Returns (MODIFIED-P . NEW-TEXT)."
  (let ((case-fold-search nil)
        (target (regexp-quote string))
        (replacement (format "[[id:%s][%s]]" node-id string))
        (pos 0)
        (parts nil))
    (while (string-match gnosis-utils--org-link-re text pos)
      (push (replace-regexp-in-string
             target replacement
             (substring text pos (match-beginning 0)) t t)
            parts)
      (push (match-string 0 text) parts)
      (setq pos (match-end 0)))
    (push (replace-regexp-in-string
           target replacement (substring text pos) t t)
          parts)
    (let ((new-text (apply #'concat (nreverse parts))))
      (cons (not (string= text new-text)) new-text))))

(provide 'gnosis-utils)
;;; gnosis-utils.el ends here
