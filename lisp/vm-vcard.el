;;; vm-vcard.el --- vcard parsing and formatting routines for VM  -*- lexical-binding: t; -*-
;;
;; This file is an add-on for VM

;; Copyright (C) 1997, 2000 Noah S. Friedman
;; Copyright (C) 2024-2025 The VM Developers

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1997-10-03


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'vm-mime)
(eval-and-compile
  (require 'vm-misc)
  (vm-load-features '(vcard) byte-compile-current-file))

(and (boundp 'vcard-api-version) (string-lessp vcard-api-version "2.0")
     (error "vm-vcard.el requires vcard API version 2.0 or later."))

;;;###autoload
(defvar vm-vcard-format-function nil
  "*Function to use for formatting vcards; if nil, use default.")

;;;###autoload
(defvar vm-vcard-filter nil
  "*Filter function to use for formatting vcards; if nil, use default.")

;;;###autoload
(defun vm-mime-display-internal-text/x-vcard (layout)
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (insert (vm-vcard-format-layout layout)))
  t)

;;;###autoload
(defun vm-mime-display-internal-text/vcard (layout)
  (vm-mime-display-internal-text/x-vcard layout))

;;;###autoload
(defun vm-mime-display-internal-text/directory (layout)
  (vm-mime-display-internal-text/x-vcard layout))

(defun vm-vcard-format-layout (layout)
  (let* ((beg (vm-mm-layout-body-start layout))
         (end (vm-mm-layout-body-end layout))
         (buf (if (markerp beg) (marker-buffer beg) (current-buffer)))
         (raw (vm-vcard-decode (with-current-buffer buf
                                 (save-restriction
                                   (widen)
                                   (buffer-substring beg end)))
                               layout))
         (vcard-pretty-print-function (or vm-vcard-format-function
                                          vcard-pretty-print-function)))
    (condition-case err
        (vcard-pretty-print (vcard-parse-string raw vm-vcard-filter))
        (error (format "Error parsing text/x-vcard MIME attachment:\nerror:%s\ndata:\n%s" err raw)))))

(defun vm-vcard-decode (string layout)
  (let ((buf (generate-new-buffer " *vcard decoding*")))
    (with-current-buffer buf
      (insert string)
      (vm-mime-transfer-decode-region layout (point-min) (point-max))
      (setq string (buffer-substring (point-min) (point-max))))
    (kill-buffer buf))
  string)

(defun vm-vcard-format-simple (vcard)
  (concat "\n\n--\n" (vcard-format-sample-string vcard) "\n\n"))

(provide 'vm-vcard)
;;; vm-vcard.el ends here.
