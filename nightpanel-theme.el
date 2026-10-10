;;; nightpanel-theme.el --- Saab instrument cluster colorscheme  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Greg Felice

;; Author: Greg Felice <gregfelice@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; Maintainer: Greg Felice <gregfelice@gmail.com>
;; URL: https://github.com/gregfelice/nightpanel-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces theme

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Nightpanel is a dark theme modelled on a Saab instrument cluster seen
;; at night: a pure black canvas, instrument-scale green for body text,
;; and amber reserved for the things that would be a needle or a warning
;; lamp on a real dashboard.
;;
;; The palette is deliberately narrow.  Green carries ordinary text and
;; structure; amber marks anything that wants your eye (search matches,
;; strings, constants, TODO keywords); red appears only for genuine
;; errors.  Comments and inactive chrome recede to a dim green so the
;; code itself is the brightest thing on screen.
;;
;; Usage:
;;
;;     (load-theme 'nightpanel t)
;;
;; Or interactively with `M-x load-theme'.
;;
;; Nightpanel targets true-colour displays.  It sets 24-bit hex values
;; without terminal fallbacks, so on a low-colour tty Emacs will
;; approximate them and the result will not match a GUI frame.

;;; Code:

(deftheme nightpanel
  "Saab instrument cluster — pure black, instrument green, amber needle.")

(custom-theme-set-faces 'nightpanel
;;;; Base
  '(default ((t (:background "#0A0A0A" :foreground "#7DB890"))))
  '(cursor ((t (:background "#B08030"))))
  '(region ((t (:background "#1A3020"))))
  '(highlight ((t (:background "#1A3020" :foreground "#26DE81"))))
  '(secondary-selection ((t (:background "#0A5C35"))))
  '(fringe ((t (:background "#0A0A0A" :foreground "#2E5040"))))
  '(vertical-border ((t (:foreground "#2A2A2A"))))
  '(window-divider ((t (:foreground "#2A2A2A"))))
  '(window-divider-first-pixel ((t (:foreground "#2A2A2A"))))
  '(window-divider-last-pixel ((t (:foreground "#2A2A2A"))))

;;;; Line numbers
  '(line-number ((t (:foreground "#2E5040" :background "#0A0A0A"))))
  '(line-number-current-line ((t (:foreground "#26DE81" :background "#0A0A0A" :weight bold))))

;;;; Mode line
  '(mode-line ((t (:background "#000000" :foreground "#7DB890" :box nil))))
  '(mode-line-inactive ((t (:background "#000000" :foreground "#2E5040" :box nil))))
  '(mode-line-emphasis ((t (:foreground "#26DE81" :weight bold))))
  '(mode-line-highlight ((t (:foreground "#B08030"))))
  '(mode-line-buffer-id ((t (:foreground "#26DE81" :weight bold))))

;;;; Header / tab line
  '(header-line ((t (:background "#111111" :foreground "#7DB890"))))
  '(tab-line ((t (:background "#000000" :foreground "#2E5040"))))
  '(tab-bar ((t (:background "#000000" :foreground "#2E5040"))))
  '(tab-bar-tab ((t (:background "#111111" :foreground "#7DB890"))))
  '(tab-bar-tab-inactive ((t (:background "#000000" :foreground "#2E5040"))))

;;;; Minibuffer
  '(minibuffer-prompt ((t (:foreground "#26DE81" :weight bold))))

;;;; Search / isearch
  '(isearch ((t (:background "#E8930A" :foreground "#0A0A0A" :weight bold))))
  '(isearch-fail ((t (:background "#EF4444" :foreground "#0A0A0A"))))
  '(lazy-highlight ((t (:background "#B08030" :foreground "#0A0A0A"))))
  '(match ((t (:background "#B08030" :foreground "#0A0A0A"))))

;;;; Messages
  '(error ((t (:foreground "#EF4444" :weight bold))))
  '(warning ((t (:foreground "#E8930A" :weight bold))))
  '(success ((t (:foreground "#26DE81" :weight bold))))
  '(shadow ((t (:foreground "#2E5040"))))

;;;; Links
  '(link ((t (:foreground "#26DE81" :underline t))))
  '(link-visited ((t (:foreground "#5A8A6A" :underline t))))

;;;; font-lock (syntax)
  '(font-lock-comment-face ((t (:foreground "#2E5040" :slant italic))))
  '(font-lock-comment-delimiter-face ((t (:foreground "#2E5040"))))
  '(font-lock-doc-face ((t (:foreground "#5A8A6A" :slant italic))))
  '(font-lock-string-face ((t (:foreground "#B08030"))))
  '(font-lock-keyword-face ((t (:foreground "#26DE81" :weight bold))))
  '(font-lock-builtin-face ((t (:foreground "#26DE81"))))
  '(font-lock-function-name-face ((t (:foreground "#9EC8A8" :weight bold))))
  '(font-lock-variable-name-face ((t (:foreground "#7DB890"))))
  '(font-lock-type-face ((t (:foreground "#9EC8A8"))))
  '(font-lock-constant-face ((t (:foreground "#E8930A"))))
  '(font-lock-preprocessor-face ((t (:foreground "#B08030"))))
  '(font-lock-warning-face ((t (:foreground "#E8930A" :weight bold))))
  '(font-lock-negation-char-face ((t (:foreground "#EF4444"))))

;;;; Show-paren / parens
  '(show-paren-match ((t (:foreground "#26DE81" :weight bold :underline t))))
  '(show-paren-mismatch ((t (:background "#EF4444" :foreground "#0A0A0A"))))

;;;; Diffs / version control
  '(diff-added ((t (:foreground "#26DE81" :background "#1A3020"))))
  '(diff-removed ((t (:foreground "#EF4444"))))
  '(diff-changed ((t (:foreground "#B08030"))))
  '(diff-header ((t (:foreground "#9EC8A8" :weight bold))))
  '(diff-file-header ((t (:foreground "#26DE81" :weight bold))))
  '(diff-hunk-header ((t (:foreground "#B08030"))))

;;;; Completion / company / corfu (best-effort)
  '(completions-common-part ((t (:foreground "#26DE81" :weight bold))))
  '(completions-first-difference ((t (:foreground "#B08030"))))

;;;; org-mode (best-effort)
  '(org-level-1 ((t (:foreground "#26DE81" :weight bold))))
  '(org-level-2 ((t (:foreground "#9EC8A8" :weight bold))))
  '(org-level-3 ((t (:foreground "#7DB890" :weight bold))))
  '(org-level-4 ((t (:foreground "#5A8A6A"))))
  '(org-todo ((t (:foreground "#E8930A" :weight bold))))
  '(org-done ((t (:foreground "#2E5040"))))
  '(org-block ((t (:background "#111111"))))
  '(org-code ((t (:foreground "#B08030"))))
  '(org-verbatim ((t (:foreground "#B08030")))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nightpanel)

;;; nightpanel-theme.el ends here
