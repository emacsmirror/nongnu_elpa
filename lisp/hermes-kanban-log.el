;;; hermes-kanban-log.el --- Worker-log diff engine for Hermes Kanban  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; Pure diff detection, validation, and fontification for Hermes Kanban
;; worker logs, plus the two hunk-navigation commands bound in
;; `hermes-kanban-log-mode'.  `hermes-kanban' requires this file and keeps
;; the log buffer, mode, and transport code.
;;
;; This deliberately duplicates part of the unified-diff walker in
;; `hermes-chat-format' rather than depending on the chat layer: the kanban
;; variant additionally accepts the gateway-rendered "a/path → b/path"
;; header line and rejects hunks whose body leaves leftover line counts,
;; because worker logs interleave diff-shaped noise that chat transcripts
;; never produce.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode)
(require 'subr-x)

(defun hermes-kanban--sanitize-log-content (content)
  "Return CONTENT normalized for human-readable log display."
  (replace-regexp-in-string "\r\n?" "\n" (or content "") t t))

(defun hermes-kanban--diff-hunk-counts ()
  "Return old/new line counts for a unified diff hunk at point."
  (when (looking-at diff-hunk-header-re-unified)
    (cons (if-let* ((count (match-string 2)))
              (string-to-number count)
            1)
          (if-let* ((count (match-string 4)))
              (string-to-number count)
            1))))

(defun hermes-kanban--diff-hunk-header-p ()
  "Return non-nil when point is at a unified diff hunk header."
  (hermes-kanban--diff-hunk-counts))

(defun hermes-kanban--diff-header-line-p ()
  "Return non-nil when point is at unified diff file metadata."
  (or (looking-at
       (concat "^\\(?:diff --git \\|index \\|old mode \\|new mode \\|"
               "new file mode \\|deleted file mode \\|similarity index \\|"
               "dissimilarity index \\|rename from \\|rename to \\|"
               "copy from \\|copy to \\|--- \\|\\+\\+\\+ \\)"))
      (looking-at "^.+ → .+$")))

(defun hermes-kanban--diff-body-line-counts ()
  "Return old/new line counts for the current unified diff body line."
  (cond
   ((looking-at "^\\\\ No newline at end of file") '(0 . 0))
   ((looking-at "^\\+") '(0 . 1))
   ((looking-at "^-") '(1 . 0))
   ((looking-at "^ ") '(1 . 1))))

(defun hermes-kanban--diff-omission-line-p ()
  "Return non-nil at Hermes' explicit truncated-diff summary."
  (looking-at
   (concat "^… omitted [0-9]+ diff line(s)"
           "\\(?: across [0-9]+ additional file(s)/section(s)\\)?$")))

(defun hermes-kanban--consume-diff-hunk ()
  "Move over a valid unified diff hunk at point.
Return non-nil when the consumed hunk contains an added or removed line."
  (let ((start (point)))
    (when-let* ((counts (hermes-kanban--diff-hunk-counts)))
      (let ((old-left (car counts))
            (new-left (cdr counts))
            saw-change valid)
        (forward-line 1)
        (setq valid t)
        (while (and valid
                    (not (and (<= old-left 0) (<= new-left 0)))
                    (not (eobp)))
          (let ((line-counts (hermes-kanban--diff-body-line-counts)))
            (cond
             (line-counts
              (let ((old-count (car line-counts))
                    (new-count (cdr line-counts)))
                (if (or (> old-count old-left)
                        (> new-count new-left))
                    (setq valid nil)
                  (when (or (and (= old-count 1) (= new-count 0))
                            (and (= old-count 0) (= new-count 1)))
                    (setq saw-change t))
                  (setq old-left (- old-left old-count)
                        new-left (- new-left new-count))
                  (forward-line 1))))
             ((and saw-change (hermes-kanban--diff-omission-line-p))
              (setq old-left 0
                    new-left 0))
             (t (setq valid nil)))))
        (when (or (> old-left 0) (> new-left 0))
          (setq valid nil))
        (while (and valid
                    (not (eobp))
                    (looking-at "^\\\\ No newline at end of file"))
          (forward-line 1))
        (if (and valid saw-change)
            t
          (goto-char start)
          nil)))))

(defun hermes-kanban--diff-range-at-point ()
  "Return embedded unified diff range at point as zero-based offsets, or nil."
  (let ((start (point))
        saw-hunk keep-scanning)
    (when (or (hermes-kanban--diff-header-line-p)
              (hermes-kanban--diff-hunk-header-p))
      (while (hermes-kanban--diff-header-line-p)
        (forward-line 1))
      (setq keep-scanning t)
      (while (and keep-scanning
                  (hermes-kanban--diff-hunk-header-p))
        (if (hermes-kanban--consume-diff-hunk)
            (setq saw-hunk t)
          (setq keep-scanning nil)))
      (if (and saw-hunk (< start (point)))
          (cons (1- start) (1- (point)))
        (goto-char start)
        nil))))

(defun hermes-kanban--diff-blocks (content)
  "Return embedded unified diff ranges in CONTENT as zero-based conses."
  (with-temp-buffer
    (insert (substring-no-properties content))
    (goto-char (point-min))
    (cl-loop until (eobp)
             for range = (hermes-kanban--diff-range-at-point)
             if range
             collect range
             and do (goto-char (1+ (cdr range)))
             else do (forward-line 1))))

(defun hermes-kanban--fontify-diff-string (text)
  "Return TEXT fontified with `diff-mode', or TEXT on failure."
  (condition-case nil
      (with-temp-buffer
        (insert (substring-no-properties text))
        (delay-mode-hooks (diff-mode))
        (font-lock-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (buffer-string))
    (error text)))

(defun hermes-kanban--fontify-log-diffs (text)
  "Return TEXT with embedded unified diff blocks fontified."
  (let ((blocks (hermes-kanban--diff-blocks text)))
    (if (null blocks)
        text
      (with-temp-buffer
        (let ((pos 0))
          (dolist (block blocks)
            (insert (substring text pos (car block)))
            (insert (hermes-kanban--fontify-diff-string
                     (substring text (car block) (cdr block))))
            (setq pos (cdr block)))
          (insert (substring text pos)))
        (buffer-string)))))

(defun hermes-kanban--render-log-content (content)
  "Return CONTENT normalized, ANSI-colored, and diff-fontified for display."
  (hermes-kanban--fontify-log-diffs
   (ansi-color-apply (hermes-kanban--sanitize-log-content content))))

(defun hermes-kanban-log--refontify-buffer ()
  "Reapply embedded diff faces in the current worker-log buffer."
  (save-restriction
    (widen)
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          (base (point-min))
          (inhibit-read-only t))
      (with-silent-modifications
        (dolist (block (hermes-kanban--diff-blocks content))
          (let* ((start (+ base (car block)))
                 (end (+ base (cdr block)))
                 (fontified (hermes-kanban--fontify-diff-string
                             (substring content (car block) (cdr block))))
                 (offset 0))
            (remove-list-of-text-properties start end '(face font-lock-face))
            (while (< offset (length fontified))
              (let* ((next (next-single-property-change
                            offset 'face fontified (length fontified)))
                     (face (get-text-property offset 'face fontified)))
                (when face
                  (put-text-property (+ start offset) (+ start next)
                                     'face face))
                (setq offset next)))))))))

(defun hermes-kanban-log--valid-hunk-header-p ()
  "Return non-nil when point is at a validated embedded diff hunk header.
A header is validated by consuming it like `hermes-kanban--diff-range-at-point'
does, so header-shaped log text that the fontifier rejected is skipped."
  (save-excursion (hermes-kanban--consume-diff-hunk)))

(defun hermes-kanban-log-next-hunk (&optional arg)
  "Move to the next validated embedded unified diff hunk.
ARG is a positive repeat count, as in `diff-hunk-next'.  Only hunks that
pass `hermes-kanban-log--valid-hunk-header-p' are visited, so incomplete
header-shaped blocks are skipped.  Point is left unchanged when no valid
hunk follows."
  (interactive "p")
  (let ((count (prefix-numeric-value arg)))
    (when (> count 0)
      (dotimes (_ count)
        ;; ORIGIN excludes the hunk point already sits on, so a second
        ;; `n' from a hunk header advances past it instead of re-matching.
        (let ((origin (point))
              done)
          (while (and (not done)
                      (re-search-forward diff-hunk-header-re-unified nil t))
            (let ((header (match-beginning 0)))
              (cond
               ((<= header origin)
                (goto-char (match-end 0)))
               ((save-excursion
                  (goto-char header)
                  (hermes-kanban--consume-diff-hunk))
                (goto-char header)
                (setq done t))
               (t (goto-char (match-end 0))))))
          (unless done (goto-char origin)))))))

(defun hermes-kanban-log-previous-hunk (&optional arg)
  "Move to the previous validated embedded unified diff hunk.
ARG is a positive repeat count, as in `diff-hunk-prev'.  Only hunks that
pass `hermes-kanban-log--valid-hunk-header-p' are visited, so incomplete
header-shaped blocks are skipped.  Point is left unchanged when no valid
hunk precedes point."
  (interactive "p")
  (let ((count (prefix-numeric-value arg)))
    (when (> count 0)
      (dotimes (_ count)
        ;; re-search-backward lands at match-beginning, so a candidate is
        ;; validated in place; an invalid header is naturally left behind
        ;; by the next backward search.
        (let ((origin (point))
              done)
          (while (and (not done)
                      (re-search-backward diff-hunk-header-re-unified nil t))
            (cond
             ((>= (point) origin))
             ((hermes-kanban-log--valid-hunk-header-p)
              (setq done t))
             (t)))
          (unless done (goto-char origin)))))))

(provide 'hermes-kanban-log)
;;; hermes-kanban-log.el ends here
