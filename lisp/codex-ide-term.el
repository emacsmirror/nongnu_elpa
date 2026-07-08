;;; codex-ide-term.el --- eat integration for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, terminal
;; URL: https://git.thanosapollo.org/emacs-codex

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

;; eat integration for `codex-ide'.  Codex runs as a real terminal process;
;; this file only owns eat session creation, input forwarding, scroll/point
;; synchronization, and display-size synchronization.

;;; Code:

(require 'eat)
(require 'subr-x)

;;; User options

(defcustom codex-ide-term-blink-cursor nil
  "Non-nil lets the Codex TUI drive a blinking cursor.
When nil, the cursor stays steady even though Codex requests a blinking
one via its terminal cursor-style escape."
  :type 'boolean
  :group 'codex-ide)

;;; Scroll and point synchronization

(defun codex-ide-term--adrift-point-p (pos begin)
  "Non-nil when POS should re-sync to the terminal cursor.
BEGIN is the terminal display start.  POS re-syncs inside the display
region, whose contents Codex erases and redraws wholesale, and at
`point-min', where scrollback purges (ESC [3J, emitted by Codex resize
reflows) collapse every dragged point and marker.  Anywhere else in
the scrollback POS is a deliberate browsing position."
  (or (>= pos begin) (= pos (point-min))))

(defun codex-ide-term--synchronize-scroll (windows)
  "Synchronize point and windows with the terminal cursor.
WINDOWS is eat's snapshot, taken before the output was processed, of
the positions that were following the cursor; those always sync.  Also
sync any point the redraw set adrift (see
`codex-ide-term--adrift-point-p'), where eat's default sync would
strand it.  Other scrollback positions stay put so the user can browse
history while output streams."
  (let ((begin (eat-term-display-beginning eat-terminal)))
    (eat--synchronize-scroll
     (append (and (or (memq 'buffer windows)
                      (codex-ide-term--adrift-point-p (point) begin))
                  '(buffer))
             (seq-filter (lambda (window)
                           (or (memq window windows)
                               (codex-ide-term--adrift-point-p
                                (window-point window) begin)))
                         (get-buffer-window-list))))))

(defun codex-ide-term--snap-window-point (window)
  "Synchronize WINDOW with the terminal cursor.
When a window shows the buffer again while Codex is idle, no output
arrives to run the scroll sync, and the window point restored from
`window-prev-buffers' has usually collapsed to `point-min'.  Reuses
eat's own sync so the window is also recentered on the TUI frame."
  (with-current-buffer (window-buffer window)
    (when eat-terminal
      (eat--synchronize-scroll (list window)))))

;;; Cursor appearance

(defun codex-ide-term--steady-cursor (cursor-shape)
  "Return CURSOR-SHAPE with eat's blink frequency disabled."
  (list (car cursor-shape) nil (nth 2 cursor-shape)))

(defun codex-ide-term--apply-steady-cursor ()
  "Disable cursor blinking in the current eat buffer.
Preserves the user's cursor shapes and only clears the blink frequency,
honoring `codex-ide-term-blink-cursor'."
  (unless codex-ide-term-blink-cursor
    (dolist (var '(eat-very-visible-cursor-type
                   eat-very-visible-vertical-bar-cursor-type
                   eat-very-visible-horizontal-bar-cursor-type))
      (set (make-local-variable var)
           (codex-ide-term--steady-cursor (symbol-value var))))))

;;; Process lifecycle

(defun codex-ide-term--configure-buffer ()
  "Configure the current Codex eat buffer.
Must run after `eat-mode', which resets the sync function."
  (setq-local eat--synchronize-scroll-function
              #'codex-ide-term--synchronize-scroll)
  (add-hook 'window-buffer-change-functions
            #'codex-ide-term--snap-window-point nil t)
  (codex-ide-term--apply-steady-cursor))

(defun codex-ide-term--make-process (buffer-name program args env working-dir)
  "Start PROGRAM with ARGS in an eat buffer named BUFFER-NAME.
ENV is a list of \"KEY=VALUE\" strings prepended to the process
environment.  WORKING-DIR is the working directory.  Returns the
process object."
  (let ((buffer (get-buffer-create buffer-name))
        (default-directory (or working-dir default-directory)))
    (with-current-buffer buffer
      (unless (eq major-mode 'eat-mode)
        (eat-mode))
      (codex-ide-term--configure-buffer)
      ;; `eat-exec' takes an argv list, so no shell quoting is needed.
      (let ((process-environment (append env process-environment)))
        (eat-exec buffer buffer-name program nil args)))
    (or (get-buffer-process buffer)
        (error "Failed to create eat process"))))

(defun codex-ide-term--send-string (string)
  "Send STRING to the current eat buffer's terminal."
  (when eat-terminal
    (eat-term-send-string eat-terminal string)))

(defun codex-ide-term--send-return ()
  "Send RET to the current eat buffer's terminal."
  (codex-ide-term--send-string "\r"))

(defun codex-ide-term--send-escape ()
  "Send ESC to the current eat buffer's terminal."
  (codex-ide-term--send-string "\e"))

(defun codex-ide-term--sync-dimensions (buffer window)
  "Sync BUFFER terminal dimensions to WINDOW through eat's resize hook."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (with-current-buffer buffer
      (when-let* ((process (get-buffer-process buffer))
                  (adjust (process-get process 'adjust-window-size-function)))
        (when (functionp adjust)
          (funcall adjust process (list window)))))))

(provide 'codex-ide-term)

;;; codex-ide-term.el ends here
