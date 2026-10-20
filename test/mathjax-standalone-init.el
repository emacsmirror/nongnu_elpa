;;; mathjax-standalone-init.el --- Load the standalone extension -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'package)
(package-initialize)
(setq load-prefer-newer t native-comp-jit-compilation nil)
(let ((root (expand-file-name ".." (file-name-directory load-file-name))))
  (setq load-path (cons root (delete root load-path))))
(require 'markdown-ts-appear-mathjax)
(require 'markdown-ts-appear)
;;; mathjax-standalone-init.el ends here
