;;; radio.el --- Listen to Internet radio -*- lexical-binding: t -*-

;; Copyright (C) 2025 Roi Martin

;; Author: Roi Martin <jroi.martin@gmail.com>
;; Maintainer: Roi Martin <jroi.martin@gmail.com>
;; URL: https://github.com/jroimartin/radio
;; Version: 0.1.3
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

(require 'tabulated-list)

(defgroup radio ()
  "Listen to Internet radio."
  :group 'multimedia)

(defcustom radio-stations-alist nil
  "Alist of radio stations.

Elements are of the form (NAME . URL), where NAME is the name of
the radio station and URL is the URL of the radio station."
  :risky t
  :type '(alist :key-type string :value-type string))

(defcustom radio-command '("mpv" "--terminal=no" "--video=no" ":url")
  "Command used to play a radio station.

The string :url is replaced with the URL of the radio station."
  :risky t
  :type '(repeat string))

(defvar radio--current-proc nil
  "Current media player process.")

(defvar radio-line-mode--string nil
  "The string (optionally) displayed in the mode line.")

(put 'radio-line-mode--string 'risky-local-variable t)

(defun radio-command--replace-url (url)
  "Replace the station URL in `radio-command'."
  (mapcar
   (lambda (arg)
     (if (equal arg ":url") url arg))
   radio-command))

(defun radio--play (station)
  "Play radio station.

STATION must be a cons of the form (NAME . URL).  If a station is
being played, it is stopped first."
  (radio-stop)
  (let* ((cmd (radio-command--replace-url (cdr station)))
	 (program (car cmd))
	 (start-process-args `(,program nil ,program ,@(cdr cmd))))
    (setq radio--current-proc (apply #'start-process start-process-args))
    (process-put radio--current-proc :radio-station station)
    (radio-line-mode--set (format "[Station: %s]" (car station)))))

;;;###autoload
(defun radio (station-name)
  "Play a radio station.

When called from Lisp, STATION-NAME must be the name of one of
the stations defined in `radio-stations-alist'."
  (interactive (list (completing-read "Play radio station: "
				      radio-stations-alist
				      nil t)))
  (radio--play
   (or (assoc station-name radio-stations-alist)
       (user-error "Unknown station `%s'" station-name))))

;;;###autoload
(defun radio-stop ()
  "Stop playing current radio station.

If no station is being played, calling this function has no
effect."
  (interactive)
  (when (process-live-p radio--current-proc)
    (delete-process radio--current-proc))
  (setq radio--current-proc nil)
  (radio-line-mode--set ""))

(defun radio-list-stations--play ()
  "Play the selected radio station and refresh the station list."
  (interactive)
  (when-let* ((station (tabulated-list-get-id)))
    (radio--play station)
    (tabulated-list-revert)))

(defun radio-list-stations--stop ()
  "Stop playing current radio station and refresh the station list."
  (interactive)
  (radio-stop)
  (tabulated-list-revert))

(defun radio-list-stations--generate ()
  "Generate the radio station list for `tabulated-list-mode'."
  (let ((current-station
	 (and radio--current-proc
	      (process-get radio--current-proc :radio-station))))
    (mapcar
     (lambda (station)
       (let* ((is-current (equal station current-station))
	      (face (if is-current 'bold 'default))
	      (status (if is-current "▶" "")))
	 `(,station [,status
		     ,(propertize (car station) 'face face)
		     ,(propertize (cdr station) 'face face)])))
     radio-stations-alist)))

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
  (setq tabulated-list-entries #'radio-list-stations--generate)
  (tabulated-list-init-header))

;;;###autoload
(defun radio-list-stations ()
  "Display a list of all radio stations."
  (interactive)
  (with-current-buffer (get-buffer-create "*Station List*")
    (radio-mode)
    (tabulated-list-print)
    (pop-to-buffer-same-window (current-buffer))))

(defun radio-line-mode--set (string)
  "Set mode line status to STRING and force update."
  (setq radio-line-mode--string string)
  (force-mode-line-update))

;;;###autoload
(define-minor-mode radio-line-mode
  "Toggle radio status display in mode line."
  :global t
  (or global-mode-string (setq global-mode-string '("")))
  (if radio-line-mode
      (or (memq 'radio-line-mode--string global-mode-string)
	  (setq global-mode-string
		(append global-mode-string '(radio-line-mode--string))))
    (setq global-mode-string
          (delq 'radio-line-mode--string global-mode-string))))

(provide 'radio)

;;; radio.el ends here
