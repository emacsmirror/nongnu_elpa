;;; radio.el --- Listen to Internet radio -*- lexical-binding: t -*-

;; Copyright (C) 2025 Roi Martin

;; Author: Roi Martin <jroi.martin@gmail.com>
;; Maintainer: Roi Martin <jroi.martin@gmail.com>
;; URL: https://github.com/jroimartin/radio
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; radio is a GNU Emacs package that enables users to listen to
;; Internet radio stations.

;;; Code:

(defcustom radio-stations-alist nil
  "List of radio stations.

Elements are of the form (NAME . URL).

NAME is the name of the radio station.
URL  is the URL of the radio station."
  :type '(alist :key-type string :value-type string)
  :group 'radio)

(defcustom radio-command "mpv --terminal=no --video=no"
  "Command used to play a radio station."
  :type 'string
  :group 'radio)

(defvar radio--current-station nil
  "Radio station currently being played.")

(defvar radio--current-proc nil
  "Current media player process.")

;; From tabulated-list.el
(defvar tabulated-list-entries)
(defvar tabulated-list-format)
(defvar tabulated-list-sort-key)

(defun radio--play (station)
  "Play radio station.

STATION must be a cons of the form (NAME . URL).  If a station is
being played, it is stopped first."
  (radio-stop)
  (setq radio--current-station station)
  (let* ((url (cdr station))
	 (cmd (split-string-shell-command radio-command))
	 (program (car cmd))
	 (program-args `(,@(cdr cmd) ,url))
	 (start-process-args `(,program nil ,program ,@program-args))
	 (proc (apply #'start-process start-process-args)))
    (setq radio--current-proc proc)))

;;;###autoload
(defun radio-stop ()
  "Stop playing current radio station.

If no station is being played, calling this function has no
effect."
  (interactive)
  (setq radio--current-station nil)
  (when radio--current-proc
    (delete-process radio--current-proc))
  (setq radio--current-proc nil))

(defun radio-list-stations--play ()
  "Play the selected radio station and refresh the station list."
  (interactive)
  (when-let ((station (tabulated-list-get-id)))
    (radio--play station)
    (tabulated-list-revert)))

(defun radio-list-stations--stop ()
  "Stop playing current radio station and refresh the station list."
  (interactive)
  (radio-stop)
  (tabulated-list-revert))

(defun radio-list-stations--refresh ()
  "Refresh the radio station list."
  (setq tabulated-list-entries nil)
  (dolist (station radio-stations-alist)
    (let ((name (car station))
	  (url (cdr station))
	  (status (if (equal station radio--current-station) "▶" "")))
      (push (list station (vector status name url))
	    tabulated-list-entries)))
  (tabulated-list-init-header))

(defvar-keymap radio-mode-map
  :doc "Keymap used by `radio-mode'."
  "RET" #'radio-list-stations--play
  "s" #'radio-list-stations--stop)

(define-derived-mode radio-mode tabulated-list-mode "Radio Stations"
  "Major mode for listing radio stations."
  (setq tabulated-list-format [("Status" 6 t)
			       ("Station" 30 t)
			       ("URL" 0 t)])
  (setq tabulated-list-sort-key '("Station" . nil))
  (add-hook 'tabulated-list-revert-hook #'radio-list-stations--refresh nil t))

;;;###autoload
(defun radio-list-stations ()
  "Display a list of all radio stations."
  (interactive)
  (let ((buf (get-buffer-create "*Station List*")))
    (with-current-buffer buf
      (radio-mode)
      (radio-list-stations--refresh)
      (tabulated-list-print))
    (pop-to-buffer-same-window buf)))

;;;###autoload
(defun radio (station-name)
  "Play a radio station.

When called from Lisp, STATION-NAME must be the name of one of
the stations defined in `radio-stations-alist'."
  (interactive (list (completing-read "Play radio station: "
				      (mapcar #'car radio-stations-alist)
				      nil t)))
  (if-let ((station (assoc station-name radio-stations-alist)))
      (radio--play station)
    (message "Unknown station `%s'" station-name)))

(provide 'radio)

;;; radio.el ends here
