;;; hermes.el --- Emacs frontend for Hermes Agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.3.1") (websocket "1.15"))

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

;; Fresh Emacs frontend for Hermes Agent.  The MVP entry point is a small
;; keymap-popup dashboard with Chat as its first vertical slice.

;;; Code:

(require 'keymap-popup)
(require 'hermes-chat)

(defcustom hermes-dashboard-buffer-name "*Hermes Dashboard*"
  "Name of the Hermes dashboard buffer."
  :type 'string
  :group 'hermes)

(defface hermes-dashboard-heading
  '((t :inherit (bold font-lock-constant-face)))
  "Face used for the Hermes dashboard heading."
  :group 'hermes)

(defvar hermes-dashboard-mode-map)

(keymap-popup-define hermes-dashboard-mode-map
  "Hermes Dashboard"
  :parent special-mode-map
  :description "Hermes Dashboard"
  :group "Actions"
  "c" ("Chat" hermes-chat))

(define-derived-mode hermes-dashboard-mode special-mode "Hermes Dashboard"
  "Major mode for the Hermes dashboard."
  :keymap hermes-dashboard-mode-map
  :interactive nil
  (setq-local header-line-format nil)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode 0)))

(defun hermes-dashboard--insert-welcome ()
  "Insert the Hermes dashboard welcome text at point."
  (insert "\n"
          (propertize "Hermes" 'face 'hermes-dashboard-heading)
          "\n\n"
          "Emacs frontend for Hermes Agent.\n\n"
          "Press "
          (propertize "c" 'face 'help-key-binding)
          " for Chat from the dashboard popup.\n"))

(defun hermes-dashboard--render ()
  "Render the Hermes dashboard in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (hermes-dashboard--insert-welcome)
    (goto-char (point-min))))

;;;###autoload
(defun hermes ()
  "Open the Hermes dashboard."
  (interactive)
  (let ((buffer (get-buffer-create hermes-dashboard-buffer-name)))
    (with-current-buffer buffer
      (hermes-dashboard-mode)
      (hermes-dashboard--render))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (keymap-popup hermes-dashboard-mode-map))))

(provide 'hermes)
;;; hermes.el ends here
