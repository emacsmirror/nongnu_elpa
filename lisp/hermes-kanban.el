;;; hermes-kanban.el --- Kanban board browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over `hermes kanban ... --json'.  Kanban is not
;; exposed on the dashboard gateway, so this drives the `hermes' CLI directly.
;; RET shows a task, `b' switches board, `g' refreshes.

;;; Code:

(require 'tabulated-list)
(require 'hermes-transport)

(defun hermes-kanban--run-json (args)
  "Run `hermes kanban' with ARGS (a list) plus --json; return parsed JSON.
Signals a `user-error' on a missing command, non-zero exit, or bad output."
  (unless (and hermes-command (executable-find hermes-command))
    (user-error "Cannot find the hermes executable (set `hermes-command')"))
  (with-temp-buffer
    (let* ((status (apply #'call-process hermes-command nil t nil
                          "kanban" (append args (list "--json"))))
           (out (string-trim (buffer-string)))
           (parsed (and (eql status 0) (hermes-transport--json-read out))))
      (cond
       ((not (eql status 0)) (user-error "Hermes kanban failed: %s" out))
       ((car parsed) (cdr parsed))
       (t (user-error "Could not parse hermes kanban output"))))))

(defun hermes-kanban--field (task key)
  "Return TASK's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get task key)) ""))

(defun hermes-kanban--rows (tasks)
  "Return `tabulated-list' entries for kanban TASKS."
  (mapcar
   (lambda (task)
     (list (hermes-kanban--field task 'id)
           (vector (hermes-kanban--field task 'status)
                   (hermes-kanban--field task 'priority)
                   (hermes-kanban--field task 'assignee)
                   (hermes-kanban--field task 'title))))
   tasks))

(defvar-local hermes-kanban--board nil
  "Board slug shown in this buffer, or nil for the current board.")

(defun hermes-kanban--board-args ()
  "Return CLI args selecting the buffer's board, or nil."
  (and hermes-kanban--board (list "--board" hermes-kanban--board)))

(defun hermes-kanban--revert (&rest _)
  "Refresh the kanban board."
  (hermes-kanban--list hermes-kanban--board))

(defvar-keymap hermes-kanban-mode-map
  :doc "Keymap for `hermes-kanban-mode'."
  :parent tabulated-list-mode-map
  "RET" #'hermes-kanban-show
  "b" #'hermes-kanban-switch-board)

(define-derived-mode hermes-kanban-mode tabulated-list-mode "Hermes Kanban"
  "Major mode for browsing a Hermes Kanban board."
  :interactive nil
  (setq tabulated-list-format
        [("Status" 12 t) ("Pri" 4 t) ("Assignee" 16 t) ("Title" 50 t)])
  (setq-local revert-buffer-function #'hermes-kanban--revert)
  (tabulated-list-init-header))

(defun hermes-kanban--list (board)
  "Render BOARD's tasks (nil for the current board) in the kanban buffer."
  (let ((tasks (hermes-kanban--run-json
                (append (and board (list "--board" board)) (list "list")))))
    (with-current-buffer (get-buffer-create "*Hermes Kanban*")
      (unless (derived-mode-p 'hermes-kanban-mode)
        (hermes-kanban-mode))
      (setq hermes-kanban--board board)
      (setq tabulated-list-entries (hermes-kanban--rows tasks))
      (tabulated-list-print t)
      (pop-to-buffer (current-buffer)))))

(defun hermes-kanban--display-task (task)
  "Show a single kanban TASK in a detail buffer."
  (with-current-buffer (get-buffer-create "*Hermes Kanban Task*")
    (unless (derived-mode-p 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Title:    %s\nStatus:   %s\nPriority: %s\nAssignee: %s\n\n%s\n"
                      (hermes-kanban--field task 'title)
                      (hermes-kanban--field task 'status)
                      (hermes-kanban--field task 'priority)
                      (hermes-kanban--field task 'assignee)
                      (hermes-kanban--field task 'body))))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun hermes-kanban-show ()
  "Show the kanban task at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No task on this line"))
    (hermes-kanban--display-task
     (hermes-kanban--run-json
      (append (hermes-kanban--board-args) (list "show" id))))))

(defun hermes-kanban-switch-board ()
  "Switch the board shown in the current buffer."
  (interactive)
  (let* ((boards (hermes-kanban--run-json (list "boards" "list")))
         (slugs (delq nil (mapcar (lambda (board)
                                    (hermes-transport--scalar-string
                                     (hermes-transport--get board 'slug)))
                                  boards)))
         (choice (completing-read "Board: " slugs nil t)))
    (unless (string-empty-p choice)
      (hermes-kanban--list choice))))

;;;###autoload
(defun hermes-list-kanban ()
  "Browse the Hermes Kanban board."
  (interactive)
  (hermes-kanban--list nil))

(provide 'hermes-kanban)
;;; hermes-kanban.el ends here
