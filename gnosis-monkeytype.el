;;; gnosis-monkeytype.el --- Typing Module for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

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

;; Monketype module for gnosis.

;;; Code:

(defface gnosis-monketype-face-dimmed
  '((t :foreground "#888888"))
  "Face for untyped text."
  :group 'gnosis)

(defface gnosis-monkeytype-face-correct
  '((t :foreground "#08CB00"))
  "Face for correctly typed text."
  :group 'gnosis)

(defface gnosis-monkeytype-face-wrong
  '((t :foreground "#FF004D"))
  "Face for correctly typed text."
  :group 'gnosis)

(defcustom gnosis-monkeytype-themata '("basic" "cloze" "mc-cloze")
  "Thema Types to monketype."
  :type '(repeat string)
  :group 'gnosis)

(defcustom gnosis-monkeytype-enable t
  "Enable gnosis monkeytyping for wrong answers."
  :type 'boolean
  :group 'gnosis)

(defvar gnosis-monkeytype-buffer-name "*gnosis-monkeytype*")

(defvar gnosis-monkeytype-string nil)

(defun gnosis-monkeytype--format-text (text)
  "Format TEXT using a temp buffer."
  (with-temp-buffer
    (insert (propertize text 'face 'gnosis-monketype-face-dimmed))
    (delete-trailing-whitespace)
    (buffer-string)))

(defun gnosis-monkeytype--handle-change (_beg end _len)
  "Handler buffer change at END."
  (when (and (eq (current-buffer) (get-buffer gnosis-monkeytype-buffer-name))
	     (eq this-command 'self-insert-command))
    (let ((correct-char (char-after end))
          (typed-char (char-before end)))
      (when (and correct-char typed-char)
	(message "Comparing: %c with %c POS: %d" typed-char correct-char end))
      (if (and correct-char typed-char (char-equal correct-char typed-char))
          (progn
            (delete-char -1)
            (put-text-property (1- end) end 'face 'gnosis-monkeytype-face-correct)
            (goto-char end)
            ;; Check if complete
            (when (= end (1+ (length gnosis-monkeytype-string)))
	      (kill-buffer (current-buffer))
              (exit-recursive-edit))
	    ;; Forward line when at the end
	    (and (eolp) (forward-line 1)))
        (when (and correct-char typed-char)
          (delete-char -1)
          (goto-char (1- end)))))))

(defun gnosis-monkeytype-exit ()
  "Exit monkeytyping."
  (interactive nil gnosis-monkeytype-mode)
  (kill-buffer (current-buffer))
  (ignore-errors (throw 'monkeytype-loop t))
  (exit-recursive-edit))

(defun gnosis-monkeytype--calculate-wpm (text start-time)
  "Calculate and display WPM based on TEXT and START-TIME."
  (let* ((end-time (current-time))
         (elapsed-seconds (float-time (time-subtract end-time start-time)))
         (elapsed-minutes (/ elapsed-seconds 60.0))
         (word-count (length (split-string text "\\s-+")))
         (wpm (/ word-count elapsed-minutes)))
    (message "WPM: %.2f (Time: %.2f seconds)" wpm elapsed-seconds)
    wpm))

(defun gnosis-monkeytype (text thema-type)
  "Monkeytype TEXT for selected THEMA-TYPE."
  (when (and gnosis-monkeytype-enable (member thema-type gnosis-monkeytype-themata))
    (with-current-buffer (get-buffer-create gnosis-monkeytype-buffer-name)
      (erase-buffer)
      (let ((text-formatted (gnosis-monkeytype--format-text text))
	    (start-time (current-time)))
	(setq gnosis-monkeytype-string text-formatted)
	(gnosis-monkeytype-mode)
	(insert text-formatted)
	(switch-to-buffer (get-buffer-create gnosis-monkeytype-buffer-name))
	(goto-char (point-min))
	(add-hook 'after-change-functions #'gnosis-monkeytype--handle-change)
	(recursive-edit)
	(setq gnosis-monkeytype-wpm-result
	      (gnosis-monkeytype--calculate-wpm text-formatted start-time))))))

(defvar-keymap gnosis-monkeytype-mode-map
  :doc "gnosis-monkeytype mode map"
  :parent text-mode-map
  "RET" #'forward-line
  "C-c C-k" #'gnosis-monkeytype-exit)

(define-derived-mode gnosis-monkeytype-mode text-mode "Gnosis Monkeytype"
  "Gnosis Monkeytype Mode."
  :interactive nil
  :lighter " gnosis-monkeytype-mode"
  :keymap gnosis-monkeytype-mode-map
  (setq-local post-self-insert-hook nil)
  (setq-local header-line-format
	      (substitute-command-keys
	       " Wrong answer, monkeytype the thema. \\[gnosis-monkeytype-exit] to exit.")))

(provide 'gnosis-monkeytype)
;;; gnosis-monkeytype.el ends here
