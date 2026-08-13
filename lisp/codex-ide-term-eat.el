;;; codex-ide-term-eat.el --- Eat backend for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, terminal
;; URL: https://git.thanosapollo.org/emacs-codex-ide

;; This file is not part of GNU Emacs.

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

;; Eat session creation, input forwarding, cursor handling, and scroll sync.

;;; Code:

(require 'eat)
(require 'seq)
(require 'subr-x)

;;; User options

(defcustom codex-ide-term-blink-cursor nil
  "Non-nil lets the Codex TUI drive a blinking cursor.
When nil, the cursor stays steady even though Codex requests a blinking
one via its terminal cursor-style escape."
  :type 'boolean
  :group 'codex-ide)

;;; Scroll and point synchronization

(defun codex-ide-term-eat--adrift-point-p (pos begin)
  "Non-nil when POS should re-sync to the terminal cursor.
BEGIN is the terminal display start.  POS re-syncs inside the display
region, whose contents Codex erases and redraws wholesale, and at
`point-min', where scrollback purges collapse dragged points and markers.
Anywhere else in the scrollback POS is a deliberate browsing position."
  (or (>= pos begin) (= pos (point-min))))

(defun codex-ide-term-eat--emacs-mode-p ()
  "Return non-nil if the current Eat buffer is in Emacs input mode."
  (not (or eat--semi-char-mode eat--char-mode eat--line-mode)))

(defun codex-ide-term-eat--synchronize-scroll (windows)
  "Synchronize point and windows with the terminal cursor.
WINDOWS is Eat's snapshot of positions that were following the cursor.
In Eat Emacs mode, leave it unchanged so navigation remains free.  In
terminal input modes, also sync points a redraw set adrift."
  (if (codex-ide-term-eat--emacs-mode-p)
      (eat--synchronize-scroll windows)
    (let ((begin (eat-term-display-beginning eat-terminal)))
      (eat--synchronize-scroll
       (append (and (or (memq 'buffer windows)
                        (codex-ide-term-eat--adrift-point-p (point) begin))
                    '(buffer))
               (seq-filter
                (lambda (window)
                  (or (memq window windows)
                      (codex-ide-term-eat--adrift-point-p
                       (window-point window) begin)))
                (get-buffer-window-list)))))))

(defun codex-ide-term-eat--return-live ()
  "Restore Eat input and follow the live cursor in the current buffer."
  (eat-semi-char-mode)
  (eat--synchronize-scroll (list 'buffer (selected-window))))

(defun codex-ide-term-eat--synchronize-window (window)
  "Synchronize WINDOW with the Eat terminal cursor."
  (with-current-buffer (window-buffer window)
    (when eat-terminal
      (eat--synchronize-scroll (list window)))))

;;; Cursor appearance

(defun codex-ide-term-eat--normalize-cursor-state (state blink-cursor)
  "Return Eat cursor STATE adjusted for BLINK-CURSOR."
  (if blink-cursor
      state
    (pcase state
      (:blinking-block :block)
      (:blinking-bar :bar)
      (:blinking-underline :underline)
      (_ state))))

(defun codex-ide-term-eat--set-cursor (terminal state)
  "Apply cursor STATE to TERMINAL through Eat's original callback."
  (when-let* ((original
               (eat-term-parameter
                terminal 'codex-ide-term-eat--original-set-cursor-function)))
    (funcall original terminal
             (codex-ide-term-eat--normalize-cursor-state
              state codex-ide-term-blink-cursor))))

(defun codex-ide-term-eat--install-cursor-adapter ()
  "Install the Codex cursor adapter in the current Eat terminal."
  (when eat-terminal
    (let ((current (eat-term-parameter eat-terminal 'set-cursor-function)))
      (unless (eq current #'codex-ide-term-eat--set-cursor)
        (setf (eat-term-parameter
               eat-terminal
               'codex-ide-term-eat--original-set-cursor-function)
              current)
        (setf (eat-term-parameter eat-terminal 'set-cursor-function)
              #'codex-ide-term-eat--set-cursor))
      (codex-ide-term-eat--set-cursor
       eat-terminal (eat-term-cursor-type eat-terminal)))))

;;; Process lifecycle

(defun codex-ide-term-eat--available-p ()
  "Return non-nil when Eat is available."
  t)

(defun codex-ide-term-eat--configure-buffer ()
  "Configure the current Codex Eat buffer."
  (setq-local eat--synchronize-scroll-function
              #'codex-ide-term-eat--synchronize-scroll)
  (add-hook 'window-buffer-change-functions
            #'codex-ide-term-eat--synchronize-window nil t)
  (codex-ide-term-eat--install-cursor-adapter))

(defun codex-ide-term-eat--prepare-buffer (buffer-name working-dir)
  "Prepare and return an Eat BUFFER-NAME for WORKING-DIR."
  (let ((buffer (get-buffer-create buffer-name)))
    (condition-case err
        (progn
          (with-current-buffer buffer
            (setq default-directory (or working-dir default-directory))
            (unless (eq major-mode 'eat-mode)
              (eat-mode))
            (codex-ide-term-eat--configure-buffer))
          buffer)
      (error
       (kill-buffer buffer)
       (signal (car err) (cdr err))))))

(defun codex-ide-term-eat--make-process (buffer program args env)
  "Start PROGRAM with ARGS and ENV in prepared Eat BUFFER."
  (with-current-buffer buffer
    (let ((process-environment (append env process-environment)))
      (eat-exec buffer (buffer-name buffer) program nil args))
    (codex-ide-term-eat--configure-buffer)
    (or (get-buffer-process buffer)
        (error "Failed to create Eat process"))))

(defun codex-ide-term-eat--send-string (string)
  "Send STRING to the current Eat terminal."
  (when eat-terminal
    (eat-term-send-string eat-terminal string)))

(defun codex-ide-term-eat--send-return ()
  "Send RET to the current Eat terminal."
  (codex-ide-term-eat--send-string "\r"))

(defun codex-ide-term-eat--send-escape ()
  "Send ESC to the current Eat terminal."
  (codex-ide-term-eat--send-string "\e"))

(provide 'codex-ide-term-eat)

;;; codex-ide-term-eat.el ends here
