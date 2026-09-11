;;; hermes-chat-draft.el --- Literal draft highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

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

;; Fontify only the editable tail in disposable buffers.  Owned face-only
;; overlays leave submission, undo, text properties, and literal markup alone.
;; Markdown owns fence recognition; language modes never run their hooks.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'hermes-chat-buffer)

(defconst hermes-chat-draft--limit 16384
  "Maximum draft characters fontified per idle pass.
Larger drafts remain unhighlighted rather than blocking input on large pastes.")

(defvar-local hermes-chat-draft--timer nil
  "Pending idle fontification timer.")
(defvar-local hermes-chat-draft--token nil
  "Identity of the current draft fontification request.")
(defvar-local hermes-chat-draft--overlays nil
  "Face overlays owned by draft highlighting.")

(defun hermes-chat-draft--faces (start end)
  "Return face spans between START and END, relative to START.
Copy no syntax, invisibility, display, editing, or keymap properties."
  (cl-loop for pos = start then next
           while (< pos end)
           for next = (min (next-single-property-change pos 'face nil end)
                           (next-single-property-change pos 'font-lock-face nil end))
           for face = (get-text-property pos 'face)
           for font-face = (get-text-property pos 'font-lock-face)
           when (or face font-face)
           collect (list (- pos start) (- next start)
                         (cond ((and face font-face) (list face font-face))
                               (face face) (t font-face)))))

(defun hermes-chat-draft--language-mode (language)
  "Return LANGUAGE's programming major mode, or nil for plain code.
Load autoload definitions to inspect their declared ancestry, never invoke a
mode to discover its type.  Minor modes and arbitrary commands are not modes
suitable for fontifying source, even when their names end in `-mode'."
  (condition-case nil
      (when-let* ((mode (markdown-get-lang-mode language))
                  ((symbolp mode))
                  ((fboundp mode)))
        (when (autoloadp (symbol-function mode))
          (autoload-do-load (symbol-function mode) mode))
        (and (provided-mode-derived-p mode 'prog-mode) mode))
    (error nil)))

(defun hermes-chat-draft--code-faces (mode start end)
  "Return MODE face spans for START through END in a disposable buffer."
  (let ((text (buffer-substring-no-properties start end)))
    (condition-case nil
        (with-temp-buffer
          (insert text)
          (delay-mode-hooks
            (funcall mode)
            (font-lock-ensure))
          (hermes-chat-draft--faces (point-min) (point-max)))
      (error nil))))

(defun hermes-chat-draft--fontify-fences ()
  "Apply native language faces using Markdown's fence syntax properties.
Unlike Markdown's native fontifier, never reuse globally named mode buffers.
An opening fence without its closing delimiter extends to the draft end."
  (dolist (pair markdown-fenced-block-pairs)
    (when (memq (cadar pair) '(markdown-gfm-block-begin markdown-tilde-fence-begin))
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (markdown-match-propertized-text (cadar pair) (point-max)))
        (let* ((opening (match-beginning 0))
               (width (- (match-end 1) (match-beginning 1)))
               (start (progn (goto-char opening) (line-beginning-position 2)))
               (lang (markdown-code-block-lang (cons opening (cadar pair))))
               (end (progn
                      (goto-char start)
                      (if (re-search-forward
                           (markdown-maybe-funcall-regexp (caadr pair) width) nil t)
                          (match-beginning 0)
                        (point-max))))
               (mode (and lang (hermes-chat-draft--language-mode lang))))
          (when (< start end)
            (if mode
                (progn
                  (remove-text-properties start end '(face nil))
                  (dolist (span (hermes-chat-draft--code-faces mode start end))
                    (put-text-property (+ start (nth 0 span)) (+ start (nth 1 span))
                                       'face (nth 2 span))))
              (put-text-property start end 'face 'markdown-pre-face))
            ;; Match `markdown-fontify-code-blocks-generic': native faces
            ;; take precedence, with the block face underneath every span.
            (font-lock-append-text-property start end 'face 'markdown-code-face))
          (goto-char (min (point-max) (1+ end))))))))

(defun hermes-chat-draft--fontify (text)
  "Return Markdown and native code face spans for literal TEXT."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (markdown-mode))
    ;; The dependency's native path owns shared buffers and runs mode hooks.
    (setq-local markdown-fontify-code-blocks-natively nil)
    (font-lock-ensure)
    (hermes-chat-draft--fontify-fences)
    (hermes-chat-draft--faces (point-min) (point-max))))

(defun hermes-chat-draft--cancel ()
  "Retire pending draft work and remove only its owned face overlays."
  (setq hermes-chat-draft--token nil)
  (when (timerp hermes-chat-draft--timer)
    (cancel-timer hermes-chat-draft--timer))
  (setq hermes-chat-draft--timer nil)
  (mapc #'delete-overlay hermes-chat-draft--overlays)
  (setq hermes-chat-draft--overlays nil))

(defun hermes-chat-draft--current-p (token marker)
  "Return non-nil if TOKEN and MARKER still own this editable draft."
  (and (eq major-mode 'hermes-chat-mode)
       (eq token hermes-chat-draft--token)
       token
       (eq marker hermes-chat--input-marker)
       (markerp marker)
       (eq (marker-buffer marker) (current-buffer))))

(defun hermes-chat-draft--refresh (buffer token marker)
  "Highlight BUFFER's draft only while TOKEN and MARKER remain current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (hermes-chat-draft--current-p token marker)
        (setq hermes-chat-draft--timer nil)
        (save-restriction
          (widen)
          (let* ((start (marker-position marker))
                 (size (- (point-max) start))
                 (tick (buffer-chars-modified-tick))
                 (spans (and (<= size hermes-chat-draft--limit)
                             (condition-case nil
                                 (hermes-chat-draft--fontify
                                  (buffer-substring-no-properties start (point-max)))
                               (error nil)))))
            ;; A language fontifier may run arbitrary Lisp.  Recheck ownership.
            (when (and (hermes-chat-draft--current-p token marker)
                       (= start (marker-position marker))
                       (= tick (buffer-chars-modified-tick)))
              (mapc #'delete-overlay hermes-chat-draft--overlays)
              (setq hermes-chat-draft--overlays
                    (mapcar
                     (lambda (span)
                       (let ((overlay (make-overlay (+ start (nth 0 span))
                                                    (+ start (nth 1 span)) nil t nil)))
                         (overlay-put overlay 'face (nth 2 span))
                         overlay))
                     spans)))))))))

(defun hermes-chat-draft--changed (_start end _old-length)
  "Debounce draft edits ending at END; ignore transcript-only edits."
  (when (and (markerp hermes-chat--input-marker)
             (eq (marker-buffer hermes-chat--input-marker) (current-buffer))
             (>= end (marker-position hermes-chat--input-marker)))
    (when (timerp hermes-chat-draft--timer)
      (cancel-timer hermes-chat-draft--timer))
    (setq hermes-chat-draft--token (list 'draft)
          hermes-chat-draft--timer
          (run-with-idle-timer 0.15 nil #'hermes-chat-draft--refresh
                               (current-buffer) hermes-chat-draft--token
                               hermes-chat--input-marker))))

(defun hermes-chat-draft--activate ()
  "Enable draft highlighting without resetting an existing chat buffer."
  (when (eq major-mode 'hermes-chat-mode)
    (add-hook 'after-change-functions #'hermes-chat-draft--changed nil t)
    (add-hook 'kill-buffer-hook #'hermes-chat-draft--cancel nil t)
    (add-hook 'change-major-mode-hook #'hermes-chat-draft--cancel nil t)
    (hermes-chat-draft--changed (point-max) (point-max) 0)))

(provide 'hermes-chat-draft)
;;; hermes-chat-draft.el ends here
