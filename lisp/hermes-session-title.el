;;; hermes-session-title.el --- Session title policy for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

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

;; Shared policy for collision-resistant Emacs-Hermes session titles.  Raw
;; canonical titles remain durable server values while chat and session UIs
;; derive concise labels from their strict structure.

;;; Code:

(require 'project)
(require 'subr-x)

(defconst hermes-session-title--limit 100
  "Maximum dashboard session-title length.")

(defconst hermes-session-title--regexp
  (concat "\\`\\(.+\\)--"
          "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\.[0-9]\\{6\\}Z\\)"
          "--emacs\\'")
  "Regexp matching canonical Emacs-Hermes session titles.")

(defun hermes-session-title--parts (title)
  "Return canonical TITLE as (LABEL . TIMESTAMP), or nil."
  (when (and (stringp title)
             (string-match hermes-session-title--regexp title))
    (cons (match-string 1 title) (match-string 2 title))))

(defun hermes-session-title-canonicalize (label &optional existing time)
  "Return canonical title for LABEL.
Preserve the timestamp from canonical EXISTING when present.  Otherwise use
TIME, defaulting to `current-time'.  Clamp LABEL to the dashboard title limit."
  (let ((label (string-trim label)))
    (when (string-empty-p label)
      (user-error "Title must not be empty"))
    (let* ((parts (hermes-session-title--parts existing))
           (timestamp (or (cdr parts)
                          (format-time-string "%Y%m%dT%H%M%S.%6NZ"
                                              (or time (current-time)) t)))
           (suffix (format "--%s--emacs" timestamp))
           (max-label-length (- hermes-session-title--limit (length suffix)))
           (bounded-label (substring label 0 (min (length label)
                                                   max-label-length))))
      (concat bounded-label suffix))))

(defun hermes-session-title-project-label (&optional fallback)
  "Return current project basename, FALLBACK, or `Hermes Chat'."
  (or (when-let* ((project (project-current nil))
                  (root (project-root project)))
        (file-name-nondirectory (directory-file-name root)))
      (and (stringp fallback)
           (not (string-empty-p (string-trim fallback)))
           (string-trim fallback))
      "Hermes Chat"))

(defun hermes-session-title-chat-display (title)
  "Return concise chat display text for TITLE.
Leave titles outside the strict Emacs-Hermes canonical format unchanged."
  (or (car (hermes-session-title--parts title)) title))

(defun hermes-session-title-browser-display (title)
  "Return dated session-browser display text for TITLE.
Leave titles outside the strict Emacs-Hermes canonical format unchanged."
  (if-let* ((parts (hermes-session-title--parts title))
            (label (car parts))
            (timestamp (cdr parts)))
      (format "%s · %s-%s-%s %s:%s"
              label
              (substring timestamp 0 4)
              (substring timestamp 4 6)
              (substring timestamp 6 8)
              (substring timestamp 9 11)
              (substring timestamp 11 13))
    title))

(provide 'hermes-session-title)
;;; hermes-session-title.el ends here
