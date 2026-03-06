;;; typst-ts-variables.el --- typst-ts-mode variables  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 The typst-ts-mode Project Contributors

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Internal variables and customizable variables.

;;; Code:

(defgroup typst-ts nil
  "Tree Sitter enabled Typst Writing."
  :prefix "typst-ts"
  :group 'text
  :group 'languages)


(defcustom typst-ts-grammar-location nil
  "Specify typst tree sitter grammar file location.
This is used for grammar minimum version check.  The modified time of the
grammar file is used for comparing.
This variable is used in `typst-ts-check-grammar-version'."
  :type '(choice (string :tag "typst tree sitter grammar file location")
                 (const :tag "Don't enable grammar version check" nil)))

;; `git log -n1 --date=raw` or `git log -n1 --format="%at"`
(defvar typst-ts--grammar-minimum-version-timestamp 1713791627
  "Timestamp for the minimum supported typst tree sitter grammar version.")

(defcustom typst-ts-enable-raw-blocks-highlight nil
  "Whether to enable raw block highlighting.
NOTE this option must be set before the first loading(opening typst file)"
  :type 'boolean)

(defcustom typst-ts-electric-return t
  "Whether `typst-ts-return' auto inserts list items or not."
  :type 'boolean)


;; code from Auctex
(defcustom typst-ts-math-script-display '((raise -0.5) . (raise 0.5))
  "Display specification for subscript and superscript content.
The car is used for subscript, the cdr is used for superscripts."
  :type '(cons (choice (sexp :tag "Subscript form")
                       (const :tag "No lowering" nil))
               (choice (sexp :tag "Superscript form")
                       (const :tag "No raising" nil)))
  :group 'typst-ts)


;;; Compile ====================================================================

(defgroup typst-ts-compile nil
  "Typst TS Compilation."
  :prefix "typst-ts-compile"
  :group 'typst-ts)

(defcustom typst-ts-compile-executable-location "typst"
  "The location or name(if in variable `exec-path') for Typst executable."
  :type 'string
  :group 'typst-ts-compile)

(defcustom typst-ts-compile-hide-compilation-buffer-if-success t
  "Hide compilation buffer if typst compile command is executed successfully."
  :type 'boolean
  :group 'typst-ts-compile)

(defcustom typst-ts-compile-options ""
  "User defined compile options for `typst-ts-compile'.
The compile options will be passed to the end of
`<typst-executable> compile <target-file>' command.
`<target-file>' is `typst-main-file' when set, otherwise current file."
  :type 'string
  :group 'typst-ts-compile)

(defcustom typst-ts-output-directory ""
  "User defined output directory for `typst-ts-compile` and `typst-ts-watch`."
  :type 'string
  :group 'typst-ts-compile)

(defvar-local typst-main-file nil
  "Main Typst file to compile for current buffer.
When nil, compile the current buffer file.  When non-nil, it should be a path
to the main Typst file, either absolute or relative to the current buffer's
directory.

This variable is intended to be used as a file-local variable.")

(defvar typst-ts-main-file-last nil
  "Last main Typst file selected by `typst-ts-main-file-ask'.
Stored as an absolute path and used as default candidate for other buffers.")

(put 'typst-main-file 'safe-local-variable
     (lambda (value)
       (or (null value)
           (and (stringp value)
                (> (length value) 0)))))


(defcustom typst-ts-preview-function 'browse-url
  "Function that opens PDF documents for preview."
  :type 'function
  :group 'typst-ts-compile)


(defcustom typst-ts-compile-before-compilation-hook nil
  "Hook runs after compile."
  :type 'hook
  :group 'typst-ts-compile)

(defcustom typst-ts-compile-after-compilation-hook nil
  "Hook runs after compile.
Note the requirement of this hook is the same as `compilation-finish-functions'.
Also note that this hook runs with typst buffer(the buffer you are editing) as
the current buffer."
  :type 'hook
  :group 'typst-ts-compile)


;;; Watch Mode =================================================================

(defcustom typst-ts-watch-options '()
  "User defined compile options for `typst-ts-watch'.
The compile options will be passed to the
`<typst-executable> watch <target-file>' sub-command.
`<target-file>' is `typst-main-file' when set, otherwise current file."
  :type '(list string)
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-process-name "*Typst-Watch*"
  "Prefix of process name for `typst watch' sub-command.
Watch sessions are scoped by target file, and each session appends that path."
  :type 'string
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-process-buffer-name "*Typst-Watch*"
  "Prefix of process buffer name for `typst watch' sub-command.
Watch sessions are scoped by target file, and each session appends that path."
  :type 'string
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-auto-display-compilation-error t
  "Whether the typst watch process buffer should be displayed automatically.
This means the buffer will be displayed when error occurs, hide when error
is eliminated."
  :type 'boolean
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-display-buffer-parameters
  '(display-buffer-at-bottom
    (window-height . fit-window-to-buffer))
  "Display buffer parameters.
Note that since the major mode of Typst watch buffer is derived from compilation
 mode.  If you have a rule like \='((derived-mode . compilation-mode) ...)\=' in
your `display-buffer-alist', then this option will be covered by that rule."
  :type 'symbol
  :group 'typst-ts-watch)

(defvar typst-ts-watch-before-watch-hook nil
  "Hook runs before compile.")
(defvar typst-ts-watch-after-watch-hook nil
  "Hook runs after compile.")


(provide 'typst-ts-variables)

;;; typst-ts-variables.el ends here
