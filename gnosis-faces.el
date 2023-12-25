;;; gnosis-faces.el --- Spaced Repetition Learning Tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; This is part of the package gnosis.el

;;; Code:

(defgroup gnosis-faces nil
  "Faces used by gnosis."
  :group 'gnosis
  :tag "Gnosis Faces"
  :prefix 'gnosis-face)


(defface gnosis-face-extra
  '((t :inherit markdown-italic-face))
  "Face for extra-notes from note."
  :group 'gnosis-faces)

(defface gnosis-face-main
  '((t :inherit default))
  "Face for main section from note."
  :group 'gnosis-face-faces)

(defface gnosis-face-seperator
  '((t :inherit homoglyph))
  "Face for section seperator."
  :group 'gnosis-face)


(provide 'gnosis-faces)
;;; gnosis-faces.el ends here


