;;; jabber-festival.el --- Festival alert hooks  -*- lexical-binding: t; -*-

;; Copyright (C) 2005  Magnus Henoch
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile (require 'jabber-alert))
(require 'festival nil t)

;; Oddly enough, there are two different implementations of
;; festival.el.  One is distributed with the festival package
;; (http://festvox.org/festival), which provides the speech-to-text
;; executable.  The other was written by Dave Pearson
;; (https://github.com/davep/festival.el).
;;
;; EmacsWiki points to Pearson’s version.  And that is the version
;; which straight.el's loads by default.  Neither version is provided
;; by either GNU ELPA, or MELPA.
;;
;; The version bundled with the binary package supplies
;; ‘festival-say-string’, Pearson's version supplies ‘festival-say’
;; instead.  We support both, suppressing ‘check-declare’ warnings by
;; prefixing the filename with "ext:".

;; Global reference declarations

(declare-function festival-say-string "ext:festival.el" (say))
(declare-function festival-say "ext:festival.el" (format &rest args))

;;


(when (featurep 'festival)
  (cond
   ((fboundp 'festival-say-string)
    (define-jabber-alert
     festival "Voice messages through Festival"
     (lambda (text &optional title)
       (festival-say-string (or title text)))))
   ((fboundp 'festival-say)
    (define-jabber-alert
     festival "Voice messages through Festival"
     (lambda (text &optional title)
       (festival-say (or title text)))))
   (t
    (define-jabber-alert
     festival "Voice messages through Festival"
     (lambda (_text &optional _title)
       (error "Unsupported festival.el implementation."))))))

(provide 'jabber-festival)

;;; jabber-festival.el ends here