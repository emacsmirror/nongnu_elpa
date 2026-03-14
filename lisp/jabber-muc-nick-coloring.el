;;; jabber-muc-nick-coloring.el --- Add nick coloring abilyty to emacs-jabber  -*- lexical-binding: t; -*-

;; Copyright 2009, 2010, 2012, 2013 Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;;; Code:

(require 'color)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defcustom jabber-muc-participant-colors nil
  "Alist of used colors.
Format is (nick . color).  Color may be
in #RGB or textual (like red or blue) notation.  Colors will be
added in #RGB notation for unknown nicks."
  :type '(alist :key-type string :value-type color)
  :group 'jabber-chat)

(defcustom jabber-muc-colorize-local nil
  "Colorize MUC messages from you."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-muc-colorize-foreign nil
  "Colorize MUC messages not from you."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-muc-nick-saturation 1.0
  "Default saturation for nick coloring."
  :type 'float
  :group 'jabber-chat)

(defcustom jabber-muc-nick-value 1.0
  "Default value for nick coloring."
  :type 'float
  :group 'jabber-chat)

(defun jabber-muc-nick-hsv-to-hsl (h s v)
  "Convert color consisting of H, S and V to list of HSL values."
  ;; https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_HSL
  (let* ((hue h)
         (luminance (* v (- 1 (/ s 2.0))))
         (saturation (if (or (= luminance 0) (= luminance 1))
                         0
                       (/ (- v luminance) (min luminance (- 1 luminance))))))
    (list hue saturation luminance)))

(defun jabber-muc-nick-gen-color (nick)
  "Return a good enough color from the available pool."
  (let* ((pool-index (mod (string-to-number (substring (md5 nick) 0 6) 16) 360))
         (hue (/ pool-index 360.0))
         (saturation jabber-muc-nick-saturation)
         (value jabber-muc-nick-value)
         (hsl (jabber-muc-nick-hsv-to-hsl hue saturation value)))
    (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb hsl))))

(defun jabber-muc-nick-get-color (nick)
  "Get NICKs color."
  (let ((color (cdr (assoc nick jabber-muc-participant-colors))))
    (if color
        color
      (push (cons nick (jabber-muc-nick-gen-color nick)) jabber-muc-participant-colors)
      (cdr (assoc nick jabber-muc-participant-colors)))))

(provide 'jabber-muc-nick-coloring)

;;; jabber-muc-nick-coloring.el ends here
