;;; gnosis-utils.el --- Gnosis Utilities Module  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

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
  "Remove prefix and suffxi quotes for STR."
  (string-remove-prefix "\"" (string-remove-suffix "\"" str)))


(defun gnosis-utils-highlight-words (str words face &optional default-face)
  "Highlight WORDS in STR with FACE.

Replaces first occurence of word in STR with FACE.

Optionally, use custom DEFAULT-FACE."
  (cl-assert (listp words) nil "Words to mark must be a list.")
  (with-temp-buffer
    (insert (propertize str 'face default-face))
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

(provide 'gnosis-utils)
;;; gnosis-utils.el ends here
