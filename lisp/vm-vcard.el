;;; vm-vcard.el --- vcard parsing and formatting routines for VM  -*- lexical-binding: t; -*-
;;
;; This file is an add-on for VM

;; Copyright (C) 1997, 2000 Noah S. Friedman
;; Copyright (C) 2024-2026 The VM Developers

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
  (vm-load-features-silent-when-compiling '(vcard)))
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(defvar vcard-pretty-print-function)  ;; from vcard.el, used for dynamic binding

;; vcard.el functions
(declare-function vcard-pretty-print "ext:vcard" (vcard))
(declare-function vcard-parse-string "ext:vcard" (string &optional filter))
(declare-function vcard-format-sample-string "ext:vcard" (vcard))

(and (boundp 'vcard-api-version) (string-lessp vcard-api-version "2.0")
     (error "vm-vcard.el requires vcard API version 2.0 or later."))

;;;###autoload
(defvar vm-vcard-format-function nil
  "*Function to use for formatting vcards; if nil, use default.")

;;;###autoload
(defvar vm-vcard-filter nil
  "*Filter function to use for formatting vcards; if nil, use default.")

(defun vm-vcard-available-p ()
  "Whether vcard.el is installed, VM shipping no copy of its own.
`vm-load-features-silent-when-compiling' above tolerates its absence, so
this is asked again at display time rather than assumed."
  (and (require 'vcard nil t) t))

;;;###autoload
(defun vm-mime-display-internal-text/x-vcard (layout)
  "Insert the vCard part LAYOUT as vcard.el formats it.
Answer nil when vcard.el is not installed.  That is how an internal MIME
displayer declines, and VM then offers the part as an attachment; the
formatting reads variables that belong to vcard.el, so without the file it
signalled void-variable from inside the display (#779)."
  (when (vm-vcard-available-p)
    (let ((inhibit-read-only t)
          (buffer-read-only nil))
      (insert (vm-vcard-format-layout layout)))
    t))

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
