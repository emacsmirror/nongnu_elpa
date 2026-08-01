;;; org-history-dirl.el --- .dir-locals.el functions for tracking files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; URL: https://codeberg.org/Anoncheg/emacs-org-history
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:

;;; Code:

;; (require 'cl-seq)
(require 'seq)
(require 'cl-extra); for cl-some
(require 'org-history-debug)

;; -=-= code
;; ----------------------- append-after-save ---------------
;; [Original Config] ---> Extract 'org-mode entries (File A's rules)
;;                              |
;;                              v
;;                      Add File B's new rule to the list
;;                              |
;;                              v
;; [Strip old org-mode] -> [Insert combined rules] -> [Save file]
;;
;; Result .dir-locals.el example (Per-file):
;; (("folder1/file" . ((org-mode . ((mode . org-history)))))
;;  ("folder2/otherfile" . ((org-mode . ((mode . org-history))))))
;;
;; File .dir-locals.el may have more global options that we check also:
;; ((org-mode (mode . org-history)) ; Per-mode entry
;;  (nil (mode . org-history))) ; Default (nil) entry

(defun org-history-dirl--filter-list-by-car (key config)
  "Return all entries in CONFIG matching KEY."
  (org-history-debug-print "org-history-dirl--filter-list-by-car %s %s" key config)
  ;; cl-remove-if-not?
  (mapcar #'cadr (seq-filter (lambda (item) (equal (car item) key)) config)))


(defun org-history-dirl--contains-mode-p (entries majormode minormode)
  "Return t if ENTRY enables MINORMODE.
Argument ENTRIES is alist, which is list of cons.
Argument MAJORMODE - one of entry may be cons with sub-alist associated
 for MAJORMODE.
Argument MINORMODE - is mode that what search to be activated with
 pseudo-variable \='mode."
  (org-history-debug-print "org-history-dirl--contains-mode-p N0 %s" entries majormode minormode)
  (cond
   ;; Case 1: A proper list: '((mode . org-history)) or '((org-mode . ((mode . org-history)))) or '((org-mode . (mode . org-history)))
   ((when (proper-list-p entries)
      (cl-some (lambda (val)
                 (org-history-debug-print "org-history-dirl--contains-mode-p N1 %s" val)
                 (org-history-dirl--contains-mode-p val majormode minormode)) ; Recursive call
               entries)))
   ;; continue if not found
   ;; Case 2: Cons '(mode . org-history)
   ((when (consp entries)
      (org-history-debug-print "org-history-dirl--contains-mode-p N2 %s" entries)
      (equal entries (cons 'mode minormode))))
   ;; continue if not found
   ;; Case 3: '(org-mode . ((mode . org-history))) or  '(org-mode . (mode . org-history))
   ((when (and (consp entries)
               (equal (car entries) majormode))
      (org-history-debug-print "org-history-dirl--contains-mode-p N3 %s" entries)
      (if (proper-list-p (cdr entries))
          (progn
            (org-history-debug-print "org-history-dirl--contains-mode-p N31 %s" (cdr entries))
            (cl-some (lambda (val)
                       (org-history-dirl--contains-mode-p val majormode minormode)) ; Recursive call
                     (cdr entries)))
        ;; else
        (org-history-debug-print "org-history-dirl--contains-mode-p N32 %s" entries)
        (equal (cdr entries) (cons 'mode minormode)))))

   ;; default
   (t nil)))


;; ## 1. Per-file
(defun org-history-dirl--dir-locals-per-file-p (rel-file-name config  majormode minormode)
  "Check if any REL-FILE-NAME entry in CONFIG enables MINORMODE.
REL-FILE-NAME is relative filename in .dir-locals.el.
Argument MAJORMODE - one of entry may be cons with sub-alist associated
 for MAJORMODE.
Argument MINORMODE - is mode that what search to be activated with
 pseudo-variable \='mode."
  ;; ("file" . ((org-mode . ((mode . org-history)))))
  ;; (nil . ((org-mode . ((mode . org-history)))))
  ;; (nil . ((mode . org-history)))
  (org-history-debug-print "org-history-dirl--dir-locals-per-file-p N1 %s %s" rel-file-name config)
  (cl-some
   (lambda (entry)
     (org-history-debug-print "org-history-dirl--dir-locals-per-file-p N2 %s" entry)
     (org-history-dirl--contains-mode-p entry majormode minormode))
   (org-history-dirl--filter-list-by-car rel-file-name config)))


;; ## 4. Main function
(defun org-history-dirl--dir-locals-p (&optional rel-file-name config)
  "Check if .dir-locals.el enables `org-history-mode' for file.
CONFIG, if given, is the parsed .dir-locals.el.
Uses `default-directory',
Uses REL-FILE-NAME (relative to git root) or variable `buffer-file-name'
 variable."
  (let* ((rel-file-name (or rel-file-name
                            (and buffer-file-name
                                 (file-relative-name buffer-file-name default-directory))))
         (majormode 'org-mode)
         (minormode 'org-history)
         (config (or config
                     (let ((dl-file (expand-file-name ".dir-locals.el" default-directory)))
                       (when (file-exists-p dl-file)
                         (with-temp-buffer
                           (insert-file-contents dl-file)
                           (ignore-errors (read (current-buffer)))))))))
    (org-history-debug-print "org-history-dirl--dir-locals-p N1" rel-file-name config)
    (or (org-history-dirl--contains-mode-p (org-history-dirl--filter-list-by-car majormode config) majormode minormode) ; Per-mode entry
        ;; (not (org-history-debug-print "org-history-dirl--dir-locals-p N2"))
        (org-history-dirl--dir-locals-per-file-p rel-file-name config majormode minormode) ; Per-file
        ;; (not (org-history-debug-print "org-history-dirl--dir-locals-p N3"))
        (org-history-dirl--dir-locals-per-file-p nil config majormode minormode)))) ; per-folder Default (nil) entry

;; (let ((default-directory (vc-git-root buffer-file-name)))
;;   (org-history-dirl--dir-locals-p))


(defun org-history-dirl-append ()
  "Add `org-history-mode' activation to .dir-locals.el file in git root.
Uses variable `buffer-file-name', `default-directory` should point to a
 git root.
After call, dont forget to add file to git with help of
 `org-history--vc-add-file'.
Accuratelly merges with existing settings in file.
Return .dir-locals.el full path."
  (interactive)
  (unless buffer-file-name
    (user-error "org-history: Current buffer is not visiting a file"))
  (let* ((git-root (vc-git-root (or buffer-file-name default-directory)))
         (file-path (expand-file-name ".dir-locals.el" git-root))
         (rel-file-name (file-relative-name buffer-file-name git-root))
         ;; Read existing config, should be list of cons cells
         (config (when (file-exists-p file-path)
                   (with-temp-buffer
                     (insert-file-contents file-path)
                     (ignore-errors (read (current-buffer))))))
         ;; Compose new entry for file (relative path)
         (new-rule `(,rel-file-name . ((org-mode . ((mode . org-history)))))))
    (unless (org-history-dirl--dir-locals-p rel-file-name config)
      ;; update config
      (let ((old-entry (assoc rel-file-name config)))
        (if old-entry
            ;; update existing org-mode entry
            (let* ((mode-list (cdr old-entry))
                   (org-entry (assoc 'org-mode mode-list)))
              (if org-entry
                  (unless (member '(mode . org-history) (cdr org-entry))
                    (setcdr org-entry
                            (append (cdr org-entry) '((mode . org-history)))))
                ;; add org-mode entry
                (push '(org-mode . ((mode . org-history))) mode-list))
              ;; update the file entry with new mode-list
              (setcdr old-entry mode-list))
          ;; if no entry for file, add one
          (push new-rule config)))
      ;; write back
      (with-temp-file file-path
        (let (print-level print-length)
          (pp config (current-buffer))))
      ;; (when (vc-root-dir)
      ;;   (org-history--vc-add-file file-path 'Git))
      (message ".dir-locals.el: file %s was added." rel-file-name)
      file-path)))


;;; provide

(provide 'org-history-dirl)

;;; org-history-dirl.el ends here
