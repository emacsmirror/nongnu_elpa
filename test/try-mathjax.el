;;; try-mathjax.el --- Interactive isolated MathJax demo -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; emacs -Q -l test/try-mathjax.el
;; This session uses the independent single-file extension.

;;; Code:
(load (expand-file-name "mathjax-standalone-init.el"
                        (file-name-directory load-file-name)) nil t)
(setopt markdown-ts-appear-enable-math-preview t
        markdown-ts-appear-math-renderer 'mathjax
        markdown-ts-appear-mathjax-edit-preview nil
        markdown-ts-appear-mathjax-center-display t
        markdown-ts-appear-mathjax-delay 0.2)
(find-file (expand-file-name "../examples/mathjax.md"
                            (file-name-directory load-file-name)))
(goto-char (point-min))
(search-forward "\\sigma(x)")
(markdown-ts-appear-mode 1)
;;; try-mathjax.el ends here
