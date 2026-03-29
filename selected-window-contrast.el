;;; selected-window-contrast.el --- Highlight window and cursor at switching  -*- lexical-binding: t -*-

;; Copyright (c) 2025 Anoncheg
;;
;; Maintainer: Anoncheg <vitalij@gmx.com>
;; Author: Anoncheg <vitalij@gmx.com>
;; Keywords:  color, windows, faces, buffer, background
;; URL: https://codeberg.org/Anoncheg/selected-window-contrast
;; Version: 0.4
;; Created: 11 dec 2024
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not,
;; see <https://www.gnu.org/licenses/agpl-3.0.en.html>.

;;; Commentary:

;; Highlight the selected window by adjusting the contrast of the text
;  foreground and background relative to the current theme. It works
;  well if you switch themes frequently; contrast will be
;  preserved. This also applies to the modeline.
;  Modeline and cursor highlighting - working in TTY.
;; We also highlight the cursor position, which can be disabled with
;; (setopt selected-window-contrast-cursor-flag nil) in .emacs.
;;  or
;;  M-x customize-variable RET selected-window-contrast-cursor-flag

;;;; Usage:

;; (add-to-list 'load-path "path_to/selected-window-contrast") ; optional
;; (when (require 'selected-window-contrast nil 'noerror)
;;   (setopt selected-window-contrast-bg-selected 0.95)
;;   (setopt selected-window-contrast-bg-others 0.75)
;;   (setopt selected-window-contrast-text-selected 0.9)
;;   (setopt selected-window-contrast-text-others 0.6)
;;   (add-hook 'buffer-list-update-hook
;;             #'selected-window-contrast-highlight-selected-window))

;;;;  How this works:

;;  1) We get color with `face-attribute' `selected-frame' for
;;  foreground and backgraound.
;;  2) Convert color to HSL
;;  3) adjust brightness in direction of foreground-background average
;;  4) convert color to RGB, then to HEX
;;  5) apply color

;; Customize: M-x customize-group RET selected-window-contrast

;; Note, if you use C-o to switch windows, this may conflict with
;;  rectangle-mark-mode-map
;; Rebind keys then:
;; (keymap-set rectangle-mark-mode-map "C-c o" #'open-rectangle)
;; (keymap-set rectangle-mark-mode-map "C-o" #'other-window)

;;;; Donate:

;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether TRX-TRON) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;; Code:

;; Touch: Global variables bound deep is not good, it is a type of the inversion of control.
;; I am the best that is why I am the winner.
(require 'color)
(require 'rect)

(defgroup selected-window-contrast nil
  "Highlight by brightness of text and background."
  :group 'faces)

;; - configurable:
(defcustom selected-window-contrast-bg-selected nil
  "Non-nil used to set selected window background contrast in [0-1] range.
Higher value increase contrast between text and background.
This value change contrast of text regarding to background."
  :type '(choice (restricted-sexp :tag "Contrast value"
                                  :match-alternatives
                                  ((lambda (x) (and (numberp x) (<= 0 x 1))))
                                  :message "Contrast value must be between 0 and 1")
                 (const :tag "Don't change default contrast of theme." nil)))

(defcustom selected-window-contrast-bg-others 0.8
  "Non-nil used to set not selected windows background contrast.
in [0-1] range."
  :type '(choice
          (const :tag "Don't change default contrast of theme." nil)
          (restricted-sexp :tag "Contrast value"
                           :match-alternatives
                           ((lambda (x) (and (numberp x) (<= 0 x 1))))
                           :message "Contrast value must be between 0 and 1")))

(defcustom selected-window-contrast-text-selected nil
  "Non-nil used to set not selected windows text contrast in [0-1] range."
  :type '(choice (restricted-sexp :tag "Contrast value"
                                  :match-alternatives
                                  ((lambda (x) (and (numberp x) (<= 0 x 1))))
                                  :message "Contrast value must be between 0 and 1")
                 (const :tag "Don't change default contrast of theme." nil)))

(defcustom selected-window-contrast-text-others nil
  "Non-nil used to set not selected windows text contrast in [0-1] range."
  :type '(choice (restricted-sexp :tag "Contrast value"
                                  :match-alternatives
                           ((lambda (x) (and (numberp x) (<= 0 x 1))))
                           :message "Contrast value must be between 0 and 1")
                 (const :tag "Don't change default contrast of theme." nil)))

(defcustom selected-window-contrast-cursor-flag t
  "Non-nil means enable highlighting cursor by region around it."
  :type 'boolean)

(defcustom selected-window-contrast-flag t
  "Non-nil means enable highlighting by contrast."
  :type 'boolean)

(defcustom selected-window-contrast-region-timeout 0.5
  "Highlighting curson, second for which to show rectangle around.
A float greater than 0."
  :type 'float
  :set (lambda (sym val)
         (if (> val 0)
             (set-default sym val)
           (error "Value must be greater than 0"))))

(defcustom selected-window-contrast-region-width 8
  "Highlighting curson, width in chars of rectangle."
  :type 'number)

(defcustom selected-window-contrast-region-lines 2
  "Highlighting curson, height in lines of rectangle."
  :type 'number)

(defun selected-window-contrast--get-current-colors ()
  "Get current text and background color of default face.
Returns list: (foreground background), both strings."
  (list (face-attribute 'default :foreground)
        (face-attribute 'default :background)))

(defun selected-window-contrast-adjust-contrast (text-color background-color bg-mag text-mag)
  "Maximize visual contrast between TEXT-COLOR and BACKGROUND-COLOR.
This function remaps the lightness component of both input colors so that:
- Colors above the midpoint (0.5) are pushed towards full white.
- Colors below the midpoint are pushed towards full black.
Arguments:
 TEXT-COLOR        String, name or hex.
 BACKGROUND-COLOR  String, name or hex.
 BG-MAG (float in [0,1]) controls stretching of contrast for background.
 TEXT-MAG (float in [0,1], optional): stretching of contrast for text.
Returns:
 List: (NEW-TEXT-RGB NEW-BACKGROUND-RGB), each as (R G B) floats in [0,1]."
  (let ((text-rgb (color-name-to-rgb text-color))
        (back-rgb (color-name-to-rgb background-color)))
    (if (and text-rgb back-rgb)
        (let* ((text-hsl (apply #'color-rgb-to-hsl text-rgb))
               (bg-hsl   (apply #'color-rgb-to-hsl back-rgb))
               (mid 0.5))
          (list
           (if (not text-mag)
               (apply #'color-hsl-to-rgb text-hsl)
             ;; else
             (let* ((t-l (nth 2 text-hsl))
                    (new-t-l (if (> t-l mid)
                                 (+ mid (* text-mag (- 1.0 mid)))
                               (- mid (* text-mag mid)))))
               (apply #'color-hsl-to-rgb (list (nth 0 text-hsl) (nth 1 text-hsl) new-t-l))))
           (if (not bg-mag)
               (apply #'color-hsl-to-rgb bg-hsl)
             ;; else
             (let* ((b-l (nth 2 bg-hsl))
                    (new-b-l (if (> b-l mid)
                                 (+ mid (* bg-mag (- 1.0 mid)))
                               (- mid (* bg-mag mid)))))
               (apply #'color-hsl-to-rgb (list (nth 0 bg-hsl) (nth 1 bg-hsl) new-b-l))))))
      ;; else
      (message "Unable to recognize text-color: %s or background-color: %s" text-color background-color))))

(defun selected-window-contrast--rgb-to-hex (rgb &optional digits)
  "Convert normalized RGB list to hex string.
RGB: list of 3 floats in [0,1].  DIGITS: 2 or 4 digits/component."
  (apply #'color-rgb-to-hex (append rgb (list (or digits 2)))))

(defun selected-window-contrast-change-window (contrast-background contrast-text)
  "Increase contrast between text and background in buffer.
CONTRAST-BACKGROUND, CONTRAST-TEXT: float in [0,1]; of contrast 1 - is
full contrast, 0 - no contrast.
Works on both dark (light text/dark bg) and light (dark text/light bg) themes."
  (unless (or (when (and contrast-background
                         (or (not (numberp contrast-background))
                             (not (<= 0 contrast-background 1))))
                (message "Contrast-background must be a number in [0,1]"))
              (when (and contrast-text
                         (or (not (numberp contrast-text))
                             (not (<= 0 contrast-text 1))))
                (message "contrast-text must be a number in [0,1]")))
    (let* ((current-colors (selected-window-contrast--get-current-colors))
           (new-colors (selected-window-contrast-adjust-contrast (nth 0 current-colors)
                                                                 (nth 1 current-colors)
                                                                 contrast-background
                                                                 contrast-text))
           (background-rgb (nth 1 new-colors))
           (text-rgb (nth 0 new-colors)))
      (if (and contrast-background contrast-text)
          ;; :foreground (selected-window-contrast--rgb-to-hex text-rgb)
          (buffer-face-set (list :foreground (selected-window-contrast--rgb-to-hex text-rgb)
                                 :background (selected-window-contrast--rgb-to-hex background-rgb)))
        ;; else
        (when contrast-text
          (buffer-face-set (list :foreground (selected-window-contrast--rgb-to-hex text-rgb)
                                 :background (face-attribute 'default :background))))
        (when contrast-background
          (buffer-face-set (list :foreground (face-attribute 'default :foreground)
                                 :background (selected-window-contrast--rgb-to-hex background-rgb))))))))

(defun selected-window-contrast-change-modeline (contrast-background contrast-text)
  "Adjust modeline brightness of text and background.
Arguments CONTRAST-BACKGROUND, CONTRAST-TEXT is float value to increase
or decrease contrast."
  (let* ((back (face-attribute 'mode-line-active :background))
         (fore (face-attribute 'mode-line-active :foreground)))
    (when (or (eq back 'unspecified) (eq back 'unspecified-bg))
      (setq back (face-attribute 'default :background)))
    (when (eq fore 'unspecified)
      (setq fore (face-attribute 'default :foreground)))

    (if (or (eq back 'unspecified)
            (eq back 'unspecified-bg)
            (eq fore 'unspecified))
        (message "backgound or foreground color is unspecified in active mode line.")
      ;; else
      (let* ((new-colors (selected-window-contrast-adjust-contrast fore
                                                                   back
                                                                   contrast-background
                                                                   contrast-text))
             (new-fore (apply #'selected-window-contrast--rgb-to-hex (nth 0 new-colors)))
             (new-back (apply #'selected-window-contrast--rgb-to-hex (nth 1 new-colors))))
        (set-face-attribute 'mode-line-active nil
                            :foreground new-fore
                            :background new-back)
        t))))

(defvar selected-window-contrast-prev-window nil
  "Saved current window, because `previous-window' is not working.
Used in `selected-window-contrast-mark-small-rectangle-temporary'.")

(defun selected-window-contrast--pre-command-hook ()
  "For every key pressed, disable highlighing of pointer.
Fix for case when we switch to buffer and do something fast while cursor
 highlighted with rectangle."
  (deactivate-mark)
  (remove-hook 'pre-command-hook #'selected-window-contrast--pre-command-hook t))

(defun selected-window-contrast-mark-small-rectangle-temporary (window)
  "Mark a 2x2 rectangle around point for 1 sec, to hightlight WINDOW.
Use `rectangle-mark-mode'.  Deactivate rectangle after 1 second or less."
  (interactive)
  (when (and window
             (eq window (selected-window))
             (not (window-minibuffer-p window))
             (not (and selected-window-contrast-prev-window ; check alive
                       (window-buffer selected-window-contrast-prev-window) ; check alive
                       (window-minibuffer-p selected-window-contrast-prev-window))))
    (unless (region-active-p)
      (let ((inhibit-message t) (message-log-max nil))
        ;; Enable rectangle selection.
        (rectangle-mark-mode 1)
        (rectangle-next-line selected-window-contrast-region-lines) ; 2
        (rectangle-forward-char selected-window-contrast-region-width) ; 8
        (rectangle-exchange-point-and-mark))

      (add-hook 'pre-command-hook #'selected-window-contrast--pre-command-hook nil t)

      ;; Start timer to deactivate mark and rectangle mode.
      (run-with-timer selected-window-contrast-region-timeout
                      nil (lambda (buf)
                            (with-current-buffer buf
                              (when (region-active-p)
                                (let ((inhibit-message t) (message-log-max nil))
                                  ;; (exchange-point-and-mark)
                                  (deactivate-mark)
                                  (remove-hook 'pre-command-hook #'selected-window-contrast--pre-command-hook t)))))
                      (current-buffer))))
  ;; save current window, because `previous-window' is not working.
  (setq selected-window-contrast-prev-window (selected-window)))

(defun selected-window-contrast-highlight-selected-window-with-timeout ()
  "Highlight not selected windows with a different background color.
Timeout 0.1 sec.
For case of opening new frame with new buffer by call:
$ emacsclient -c ~/file"
  (run-with-idle-timer 0.4 nil #'selected-window-contrast-highlight-selected-window))

(defun selected-window-contrast-highlight-selected-window ()
  "Highlight not selected windows with a different background color."
  (let ((cbn (buffer-name (current-buffer)))
        (sw (selected-window)))
    (when (/= (aref cbn 0) ?\s) ; ignore system buffers
      ;; - not selected:
      (when (and selected-window-contrast-flag
                 (display-graphic-p))
        (walk-windows (lambda (w)
                        (unless (or (eq sw w)
                                    ;; Here may be several windows with same buffer,
                                    ;; to prevent double appling change.
                                    (eq cbn (buffer-name (window-buffer w))))
                          (with-selected-window w
                            (buffer-face-set 'default)
                            (selected-window-contrast-change-window
                             selected-window-contrast-bg-others
                             selected-window-contrast-text-others))))
                      -1)) ; -1 means to not include minibuffer

      ;; - selected:
      (when (and selected-window-contrast-flag
                 (display-graphic-p))
        (selected-window-contrast-change-window selected-window-contrast-bg-selected
                                                selected-window-contrast-text-selected))
      (if selected-window-contrast-cursor-flag
          (add-hook 'window-selection-change-functions
                    #'selected-window-contrast-mark-small-rectangle-temporary nil t)
        ;; else
        (remove-hook 'window-selection-change-functions
                     #'selected-window-contrast-mark-small-rectangle-temporary t)))))

(provide 'selected-window-contrast)
;;; selected-window-contrast.el ends here
