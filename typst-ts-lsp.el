;;; typst-ts-lsp.el --- Eglot tinymist integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 The typst-ts-mode Project Contributors

;; This file is NOT part of GNU Emacs.
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

;; NOTE we don't provide `eglot-server-programs' configuration here, because
;; user may have already defined entries before typst-ts-mode load, which will
;; be override by our provided configuration.  If we want to detect whether
;; users have their own entries, we can use `eglot--lookup-mode', but the name
;; of the function means it's an private function and subject to change in the
;; in the future.
;; It's better to send a patch to eglot upstream to add the Typst language server.

;;; Code:

(defgroup typst-ts-lsp nil
  "Typst TS eglot integration with tinymist."
  :prefix "typst-ts-compile"
  :group 'typst-ts)

(defcustom typst-ts-lsp-download-path
  (file-name-concat (locate-user-emacs-file ".cache")
                    "lsp" "tinymist" "tinymist")
  "Install path for the language server."
  :type 'file
  :group 'typst-ts-lsp)

;;;###autoload
(defun typst-ts-lsp-download-binary ()
  "Download latest tinymist binary to `typst-ts-lsp-download-path'.
Will override old versions."
  (interactive)
  (unless (file-exists-p typst-ts-lsp-download-path)
    (make-directory (file-name-directory typst-ts-lsp-download-path) t))
  (let ((url-prefix
         "https://github.com/Myriad-Dreamin/tinymist/releases/latest/download/tinymist-")
        (arch (concat (car (split-string system-configuration "-")) "-"))
        (os (pcase system-type
              ('darwin "apple-darwin")
              ('gnu/linux "unknown-linux-gnu")
              (_ (user-error "Unknown OS"))))
        (targz ".tar.gz")
        (sha256 ".sha256"))
    (let* ((url-path (concat url-prefix arch os targz))
           (url-hash (concat url-path sha256))
           (tinymist-archive
            (expand-file-name
             (concat (file-name-directory typst-ts-lsp-download-path)
                     "tinymist-" arch os targz)))
           (hash-file (concat tinymist-archive sha256))
           (inside-archive (concat "tinymist-" arch os "/tinymist")))
      (url-copy-file url-path tinymist-archive t)
      (url-copy-file url-hash hash-file t)
      (when (=
             0
             (call-process "sha256sum" nil nil nil
                           "-c" hash-file))
        (user-error "The hashes don't match"))
      (call-process "tar" nil nil nil
                    "-xzf" tinymist-archive
                    "-C" (file-name-directory tinymist-archive)
                    "--strip-components=1"
                    inside-archive)
      (delete-file tinymist-archive t)
      (delete-file hash-file t)
      (rename-file (concat (file-name-directory tinymist-archive) "tinymist")
                   typst-ts-lsp-download-path
                   t))))

(provide 'typst-ts-lsp)
;;; typst-ts-lsp.el ends here
