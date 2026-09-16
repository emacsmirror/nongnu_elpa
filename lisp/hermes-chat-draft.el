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
(require 'hermes-chat-format)
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

(defun hermes-chat-draft--fontify (text)
  "Return Markdown and native code face spans for literal TEXT."
  (with-temp-buffer
    (insert text)
    (hermes-chat--fontify-markdown-buffer t)
    (hermes-chat--face-spans (point-min) (point-max))))

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
