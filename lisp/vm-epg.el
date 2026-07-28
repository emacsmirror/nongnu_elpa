;;; vm-epg.el --- PGP/MIME support for VM via epg.el  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 2026 The VM Developers
;;
;; Based on vm-pgg.el by Robert Widhopf-Fenk and Jens Gustedt.

;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;; This is a replacement for vm-pgg.el, providing PGP/MIME support for VM
;; using the epg (EasyPG) package, which is bundled with GNU Emacs, instead
;; of the obsolete pgg package.
;;
;; Load it by adding to your VM configuration:
;;
;;      (require 'vm-epg)
;;
;; If you set `vm-mime-auto-displayed-content-types' and/or
;; `vm-mime-internal-content-types' make sure that they contain
;; "application/pgp-keys" or set them before loading vm-epg.
;; Otherwise public keys are not detected automatically.
;;
;; To customize vm-epg use: M-x customize-group RET vm-epg RET
;;
;; Displaying of messages in the PGP(/MIME) format will automatically trigger:
;;  * decryption of encrypted MIME parts
;;  * verification of signed MIME parts
;;  * snarfing of public keys
;;
;; The status of the current message will also be displayed in the modeline.
;;
;; To create messages according to PGP/MIME you should use:
;;  * M-x vm-epg-encrypt       for encrypting
;;  * M-x vm-epg-sign          for signing
;;  * C-u M-x vm-epg-encrypt   for encrypting + signing
;;
;; All these commands are also available in the menu PGP/MIME which is
;; activated by the minor mode `vm-epg-compose-mode'.

;;; References:
;;
;; For PGP/MIME see:
;; * https://www.rfc-editor.org/rfc/rfc2015
;; * https://www.rfc-editor.org/rfc/rfc2440
;; * https://www.rfc-editor.org/rfc/rfc3156
;;

;;; Code:

(require 'vm-macro)

(eval-and-compile
  (require 'vm-misc)
  (require 'epg))

(require 'easymenu)
(require 'vm-misc)
(require 'vm-folder)
(require 'vm-window)
(require 'vm-mime)
(require 'vm-reply)
(require 'vm-motion)
(require 'epa)

(declare-function rfc822-addresses "ext:rfc822" (header-text))

;; avoid warnings
(defvar vm-mode-line-format)
(defvar vm-message-pointer)
(defvar vm-presentation-buffer)
(defvar vm-summary-buffer)
(defvar vm-epg-cleartext-state)

;;; Custom group and faces

(defgroup vm-epg nil
  "PGP and PGP/MIME support for VM."
  :group 'vm-ext)

(defface vm-epg-bad-signature
  '((((type tty) (class color))
     (:foreground "red" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "red" :bold t))
    (((background dark))
     (:foreground "red" :bold t)))
  "The face used to highlight bad signature messages."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-good-signature
  '((((type tty) (class color))
     (:foreground "green" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "green4"))
    (((background dark))
     (:foreground "green")))
  "The face used to highlight good signature messages."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-unknown-signature-type
  '((((type tty) (class color))
     (:bold t))
    (((type tty))
     (:bold t)))
  "The face used to highlight unknown signature types."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-error
  '((((type tty) (class color))
     (:foreground "red" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "red" :bold t))
    (((background dark))
     (:foreground "red" :bold t)))
  "The face used to highlight error messages."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-bad-signature-modeline
  '((((type tty) (class color))
     (:inherit modeline :foreground "red" :bold t))
    (((type tty))
     (:inherit modeline :bold t))
    (((background light))
     (:inherit modeline :foreground "red" :bold t))
    (((background dark))
     (:inherit modeline :foreground "red" :bold t)))
  "The face used to highlight bad signature messages in the modeline."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-good-signature-modeline
  '((((type tty) (class color))
     (:inherit modeline :foreground "green" :bold t))
    (((type tty))
     (:inherit modeline :bold t))
    (((background light))
     (:inherit modeline :foreground "green4"))
    (((background dark))
     (:inherit modeline :foreground "green")))
  "The face used to highlight good signature messages in the modeline."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-unknown-signature-type-modeline
  '((((type tty) (class color))
     (:inherit modeline :bold t))
    (((type tty))
     (:inherit modeline :bold t)))
  "The face used to highlight unknown signature types in the modeline."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-error-modeline
  '((((type tty) (class color))
     (:inherit modeline :foreground "red" :bold t))
    (((type tty))
     (:inherit modeline :bold t))
    (((background light))
     (:inherit modeline :foreground "red"))
    (((background dark))
     (:inherit modeline :foreground "red")))
  "The face used to highlight error messages in the modeline."
  :group 'vm-epg
  :group 'faces)

;;; Customizable variables

(defcustom vm-epg-fetch-missing-keys t
  "If t, fetch missing keys from a keyserver when verifying signatures."
  :group 'vm-epg
  :type 'boolean)

(defcustom vm-epg-auto-snarf t
  "If t, snarfing of keys will happen automatically."
  :group 'vm-epg
  :type 'boolean)

(defcustom vm-epg-auto-decrypt t
  "If t, decrypting will happen automatically."
  :group 'vm-epg
  :type 'boolean)

(defcustom vm-epg-get-author-headers '("From:" "Sender:")
  "The list of headers used to identify the author of an outgoing message.
The first address found in these headers is used to select the signing key.
If nil, the default EPG signing key is used."
  :group 'vm-epg
  :type '(repeat string))

(defcustom vm-epg-sign-text-transfer-encoding 'quoted-printable
  "The encoding used for signed MIME parts of type text.
See `vm-epg-sign' for details."
  :group 'vm-epg
  :type '(choice (const quoted-printable) (const base64)))

;;; Compose minor mode

(defvar vm-epg-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c#s" 'vm-epg-sign)
    (define-key map "\C-c#e" 'vm-epg-encrypt)
    (define-key map "\C-c#E" 'vm-epg-sign-and-encrypt)
    (define-key map "\C-c#a" 'vm-epg-ask-hook)
    (define-key map "\C-c#k" 'vm-epg-attach-public-key)
    map))

(defvar vm-epg-compose-mode-menu nil
  "The composition menu of vm-epg.")

(easy-menu-define
  vm-epg-compose-mode-menu (list vm-epg-compose-mode-map)
  "PGP/MIME compose mode menu."
  '("PGP/MIME"
    ["Sign"              vm-epg-sign t]
    ["Encrypt"           vm-epg-encrypt t]
    ["Sign+Encrypt"      vm-epg-sign-and-encrypt t]
    ["Ask For An Action" vm-epg-ask-hook t]
    "----"
    ["Attach Public Key" vm-epg-attach-public-key t]
    ["Insert Public Key" vm-epg-insert-public-key t]))

(defvar vm-epg-compose-mode nil
  "Non-nil means PGP/MIME composition mode key bindings and menu are available.")

(make-variable-buffer-local 'vm-epg-compose-mode)

(defun vm-epg-compose-mode (&optional arg)
  "Minor mode for composing PGP/MIME messages with EPG.

Switch mode on/off according to ARG.

\\<vm-epg-compose-mode-map>"
  (interactive)
  (setq vm-epg-compose-mode
        (if (null arg) (not vm-epg-compose-mode)
          (> (prefix-numeric-value arg) 0))))

(defvar vm-epg-compose-mode-string " vm-epg"
  "String to put in mode line when `vm-epg-compose-mode' is active.")

(defcustom vm-epg-ask-function 'vm-epg-prompt-for-action
  "The function to use in `vm-epg-ask-hook'."
  :group 'vm-epg
  :type '(choice
          (const
           :tag "do nothing"
           :doc "Disable `vm-epg-ask-hook'"
           nil)
          (const
           :tag "sign"
           :doc "Ask whether to sign the message before sending"
           sign)
          (const
           :tag "encrypt"
           :doc "Ask whether to encrypt the message before sending"
           encrypt)
          (const
           :tag "encrypt and sign"
           :doc "Ask whether to encrypt and sign the message before sending"
           encrypt-and-sign)
          (function
           :tag "ask for the action"
           :doc "Will prompt for an action by calling `vm-epg-prompt-for-action'"
           vm-epg-prompt-for-action)
          (function
           :tag "your own function"
           :doc "Should return one of the other const values.")))

(if (not (assq 'vm-epg-compose-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'vm-epg-compose-mode vm-epg-compose-mode-map)
                minor-mode-map-alist)))

(if (not (assq 'vm-epg-compose-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(vm-epg-compose-mode vm-epg-compose-mode-string)
                minor-mode-alist)))

(defun vm-epg-compose-mode-activate ()
  "Activate `vm-epg-compose-mode'."
  (vm-epg-compose-mode 1))

(add-hook 'vm-mail-mode-hook 'vm-epg-compose-mode-activate t)

;;; Address/key helpers

(defun vm-epg-get-emails (headers)
  "Return email addresses found in the given HEADERS."
  (let (content addresses)
    (while headers
      (setq content (vm-mail-mode-get-header-contents (car headers)))
      (when content
        (setq addresses (append (rfc822-addresses content) addresses)))
      (setq headers (cdr headers)))
    addresses))

(defvar vm-epg-get-recipients-headers '("To:" "CC:" "BCC:")
  "The list of headers to get recipients from.")

(defun vm-epg-get-recipients ()
  "Return a list of recipient email addresses."
  (vm-epg-get-emails vm-epg-get-recipients-headers))

(defun vm-epg-get-author ()
  "Return the email address of the message author."
  (car (vm-epg-get-emails vm-epg-get-author-headers)))

(defun vm-epg-find-usable-key (keys usage)
  "Find a usable key from KEYS for USAGE (\\='sign or \\='encrypt)."
  (catch 'found
    (while keys
      (let ((pointer (epg-key-sub-key-list (car keys))))
        (while pointer
          (if (and (memq usage (epg-sub-key-capability (car pointer)))
                   (not (memq (epg-sub-key-validity (car pointer))
                              '(revoked expired))))
              (throw 'found (car keys)))
          (setq pointer (cdr pointer))))
      (setq keys (cdr keys)))))

(defun vm-epg-get-recipient-keys (context)
  "Return a list of EPG key objects for the current message recipients.
Uses CONTEXT for key lookup."
  (delq nil
        (mapcar (lambda (addr)
                  (vm-epg-find-usable-key
                   (epg-list-keys context
                                  (if (string-search "@" addr)
                                      (concat "<" addr ">")
                                    addr))
                   'encrypt))
                (vm-epg-get-recipients))))

(defun vm-epg-set-signer (context)
  "Set the signer in CONTEXT to the author.
Uses CONTEXT and `vm-epg-get-author' to identify the sender."
  (let ((author (vm-epg-get-author)))
    (when author
      (let ((signer
	     (vm-epg-find-usable-key
	      (epg-list-keys context author t)
	      'sign)))
        (when signer
          (setf (epg-context-signers context) (list signer)))))))

;;; Composition helpers

(defun vm-epg-goto-body-start ()
  "Go to the start of the message body and return point."
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n"))
  (goto-char (match-end 0))
  (point))

(defun vm-epg-encode-composition-maybe ()
  "MIME-encode the composition unless it is already encoded."
  (unless (vm-mail-mode-get-header-contents "MIME-Version:")
    (if vm-do-fcc-before-mime-encode
        (vm-do-fcc-before-mime-encode))
    (vm-mime-encode-composition)))

(defun vm-epg-normalize-composition-body ()
  "Show headers, trim trailing whitespace, ensure a final newline and
move point to the start of the body.  Does not MIME-encode."
  (vm-mail-mode-show-headers)
  ;; ensure newline at the end
  (goto-char (point-max))
  (skip-chars-backward " \t\r\n\f")
  (delete-region (point) (point-max))
  (insert "\n")
  ;; skip headers
  (vm-epg-goto-body-start))

(defun vm-epg-prepare-composition ()
  "Prepare the composition buffer for encrypting or signing.
MIME-encodes the composition first -- this is required for PGP/MIME
detached signatures, which per RFC 3156 are computed over the MIME
canonical (transfer-encoded) form of the body -- and then normalizes
the body."
  ;; encode message if not already encoded
  (vm-epg-encode-composition-maybe)
  (vm-epg-normalize-composition-body))

;;; Modeline state

(defvar vm-epg-state nil
  "State of the currently viewed message.")
(make-variable-buffer-local 'vm-epg-state)

(defvar vm-epg-state-message nil
  "The message for `vm-epg-state'.")
(make-variable-buffer-local 'vm-epg-state-message)

(defvar vm-epg-mode-line-items nil
  "An alist mapping states to modeline strings.")

(if (not (member 'vm-epg-state vm-mode-line-format))
    (setq vm-mode-line-format (append '("" vm-epg-state) vm-mode-line-format)))

(defun vm-epg-state-set (&rest states)
  "Set the message state displayed in the modeline according to STATES.
If STATES is nil, clear it."
  (save-excursion
    (vm-select-folder-buffer-if-possible)
    (when (not (equal (car vm-message-pointer) vm-epg-state-message))
      (setq vm-epg-state-message (car vm-message-pointer))
      (setq vm-epg-state nil)
      (when vm-presentation-buffer
        (with-current-buffer vm-presentation-buffer
          (setq vm-epg-state nil)))
      (when vm-summary-buffer
        (with-current-buffer vm-summary-buffer
          (setq vm-epg-state nil))))
    ;; add prefix
    (if (and states (not vm-epg-state))
        (setq vm-epg-state '("PGP:")))
    ;; add new states
    (let (s)
      (while states
        (setq s (car states)
              vm-epg-state (append vm-epg-state
                                   (list (or (cdr (assoc s vm-epg-mode-line-items))
                                             (format " %s" s))))
              states (cdr states))))
    ;; propagate state
    (setq states vm-epg-state)
    (when vm-presentation-buffer
      (with-current-buffer vm-presentation-buffer
        (setq vm-epg-state states)))
    (when vm-summary-buffer
      (with-current-buffer vm-summary-buffer
        (setq vm-epg-state states)))))

;;; Cleartext PGP handling

(defvar vm-epg-cleartext-begin-regexp
  "^-----BEGIN PGP \\(\\(SIGNED \\)?MESSAGE\\|PUBLIC KEY BLOCK\\)-----$"
  "Regexp used to match PGP armor.")

(defvar vm-epg-cleartext-end-regexp
  "^-----END PGP %s-----$"
  "Regexp used to match PGP armor.")

(defcustom vm-epg-cleartext-search-limit 4096
  "Number of bytes to search into the message for a PGP clear text armor."
  :type 'integer
  :group 'vm-epg)

(defun vm-epg-make-presentation-copy ()
  "Make a presentation copy for cleartext PGP messages."
  (let* ((m (car vm-message-pointer))
         (layout (vm-mm-layout m)))
    (vm-make-presentation-copy m)
    (save-current-buffer
      (vm-replace-buffer-in-windows (current-buffer)
                                    vm-presentation-buffer))
    (set-buffer vm-presentation-buffer)
    (goto-char (point-min))
    (forward-line 1)
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point))
      (vm-reorder-message-headers
       nil :keep-list vm-visible-headers
       :discard-regexp vm-invisible-header-regexp)
      (vm-decode-mime-message-headers m)
      (when (vectorp layout)
        (goto-char (point-min))
        (search-forward "\n\n")
        (vm-decode-mime-layout layout)
        (delete-region (point) (point-max)))
      (vm-energize-urls-in-message-region)
      (vm-highlight-headers-maybe)
      (vm-energize-headers-and-xfaces))))

(defun vm-epg-cleartext-automode-button (label action)
  "Replace current PGP armor with a button labeled LABEL that calls ACTION."
  (save-excursion
    (unless (eq major-mode 'vm-presentation-mode)
      (vm-epg-make-presentation-copy))
    (goto-char (match-beginning 0))
    (let ((buffer-read-only nil)
          (start (point))
          o)
      (if (re-search-forward (format vm-epg-cleartext-end-regexp
                                     (match-string 1))
                             (point-max) t)
          (delete-region start (match-end 0)))
      (insert label)
      (setq o (make-overlay start (point)))
      (overlay-put o 'vm-epg t)
      (overlay-put o 'face vm-mime-button-face)
      (overlay-put o 'vm-button t)
      (overlay-put o 'mouse-face 'vm-mime-button-mouse-face)
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap [mouse-2] action)
        (define-key keymap "\r" action)
        (overlay-put o 'local-map keymap)))))

(defvar vm-epg-cleartext-decoded nil
  "State of the cleartext message.")
(make-variable-buffer-local 'vm-epg-cleartext-decoded)

(defun vm-epg-set-cleartext-decoded ()
  "Record that the current message has been decoded."
  (save-excursion
    (vm-select-folder-buffer)
    (setq vm-epg-cleartext-decoded (car vm-message-pointer))))

(defun vm-epg-cleartext-automode ()
  "Check for PGP ASCII armor and trigger automatic verification/decryption."
  (save-excursion
    (vm-select-folder-buffer-if-possible)
    (if (equal vm-epg-cleartext-decoded (car vm-message-pointer))
        (setq vm-epg-cleartext-decoded nil)
      (setq vm-epg-cleartext-decoded nil)
      (if vm-presentation-buffer
          (set-buffer vm-presentation-buffer))
      (goto-char (point-min))
      (when (and (vm-mime-plain-message-p (car vm-message-pointer))
                 (re-search-forward vm-epg-cleartext-begin-regexp
                                    (+ (point) vm-epg-cleartext-search-limit)
                                    t))
        (cond ((string= (match-string 1) "SIGNED MESSAGE")
               (vm-epg-set-cleartext-decoded)
               (vm-epg-cleartext-verify))
              ((string= (match-string 1) "MESSAGE")
               (vm-epg-set-cleartext-decoded)
               (if vm-epg-auto-decrypt
                   (vm-epg-cleartext-decrypt)
                 (vm-epg-cleartext-automode-button
                  "Decrypt PGP message\n"
                  (lambda ()
                    (interactive)
                    (let ((vm-epg-auto-decrypt t))
                      (vm-epg-cleartext-decrypt))))))
              ((string= (match-string 1) "PUBLIC KEY BLOCK")
               (vm-epg-set-cleartext-decoded)
               (if vm-epg-auto-snarf
                   (vm-epg-snarf-keys)
                 (vm-epg-cleartext-automode-button
                  "Snarf PGP key\n"
                  (lambda ()
                    (interactive)
                    (let ((vm-epg-auto-snarf t))
                      (vm-epg-snarf-keys))))))
              (t
               (error "This should never happen!")))))))

(advice-add 'vm-present-current-message
            :after #'vm-epg--present-cleartext-automode)
(defun vm-epg--present-cleartext-automode (&rest _)
  "Decode or check signature on clear text messages."
  (vm-epg-state-set)
  (when (and vm-epg-cleartext-decoded
             (not (equal vm-epg-cleartext-decoded (car vm-message-pointer))))
    (setq vm-epg-cleartext-decoded nil))
  (when (and (not (eq vm-system-state 'previewing))
             (not vm-mime-decoded))
    (vm-epg-cleartext-automode)))

(advice-add 'vm-scroll-forward :around #'vm-epg--scroll-cleartext-automode)
(defun vm-epg--scroll-cleartext-automode (orig-fun &rest args)
  "Decode or check signature on clear text messages when scrolling."
  (let ((vm-system-state-was
         (save-excursion
           (vm-select-folder-buffer-if-possible)
           vm-system-state)))
    (apply orig-fun args)
    (vm-epg-state-set)
    (when (and (eq vm-system-state-was 'previewing)
               (not vm-mime-decoded))
      (vm-epg-cleartext-automode))))

;;; Cleartext cleanup
;;
;; `vm-epg-cleartext-output' holds text to be inserted and
;; `vm-epg-cleartext-output-face' the face to apply to it.
;; These are set by the verify/decrypt functions and consumed by
;; `vm-epg-cleartext-cleanup', which is called from the advice on
;; `vm-mime-display-internal-text/plain'.

(defvar vm-epg-cleartext-output nil
  "Text output from the last EPG cleartext operation.")

(defvar vm-epg-cleartext-output-face nil
  "Face to apply to `vm-epg-cleartext-output'.")

(defun vm-epg-cleartext-cleanup (status)
  "Remove ASCII armor and insert EPG output depending on STATUS."
  (let (start end)
    (setq start (and (re-search-forward "^-----BEGIN PGP SIGNED MESSAGE-----$"
                                        nil t)
                     (match-beginning 0))
          end   (and start (search-forward "\n\n" nil t)
                     (match-end 0)))
    (when (and start end)
      (delete-region start end))
    (setq start (and (re-search-forward "^-----BEGIN PGP SIGNATURE-----$" nil t)
                     (match-beginning 0))
          end (and start
                   (re-search-forward "^-----END PGP SIGNATURE-----$" nil t)
                   (match-end 0)))
    (when (and start end)
      (delete-region start end))
    ;; add output from PGP
    (insert "\n")
    (let ((start (point)) end)
      ;; TODO Behöver granskas, skiljer sig
      (insert (or vm-epg-cleartext-output ""))
      (vm-epg-crlf-cleanup start (point))
      (setq end (point))
      (put-text-property start end 'face
			 ;; TODO Är tillägget med vm-epg-cleartext-output-face rätt eller en hallucination?
                         (or vm-epg-cleartext-output-face
                             (if (eq status 'error)
                                 'vm-epg-bad-signature
                               'vm-epg-good-signature))))))

(advice-add 'vm-mime-transfer-decode-region
            :around #'vm-epg--transfer-cleartext-automode)
(defun vm-epg--transfer-cleartext-automode (orig-fun &optional layout
                                                      &rest args)
  "Decode or check signature on clear text message parts."
  (let ((vm-epg-part-start (point)))
    (apply orig-fun layout args)
    (when (and (vm-mime-text-type-layout-p layout)
               (< vm-epg-part-start (point)))
      (save-excursion
        (save-restriction
          (narrow-to-region vm-epg-part-start (point))
          (vm-epg-cleartext-automode)
          (widen))))))

(advice-add 'vm-mime-display-internal-text/plain
            :around #'vm-epg--display-cleartext-automode)
(defun vm-epg--display-cleartext-automode (orig-fun &rest args)
  "Decode or check signature on clear text message parts.
Faces would be lost if charset conversion happens after our work, so we do
the cleanup here after verification/decoding."
  (let ((vm-epg-cleartext-state nil)
	;; TODO Lokalt definierad version av globala variabler? Hallucination?
        (vm-epg-cleartext-output nil)
        (vm-epg-cleartext-output-face nil)
        (start (point))
        end)
    (let ((ret (apply orig-fun args)))
      (when vm-epg-cleartext-state
        (setq end (point))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (vm-epg-cleartext-cleanup vm-epg-cleartext-state)
          (widen)))
      ret)))

;;; Cleartext sign/encrypt/verify/decrypt

;;;###autoload
(defun vm-epg-cleartext-encrypt (sign)
  "Encrypt the composition as cleartext; with a prefix also SIGN it."
  (interactive "P")
  (save-excursion
    ;; Normalize but do NOT MIME-encode yet: the armor must be inserted into
    ;; the raw body and transfer-encoded together with it (see
    ;; `vm-epg-encode-composition-maybe' below).
    (vm-epg-normalize-composition-body)
    (let* ((start (point))
           (end (point-max))
           (context (epg-make-context 'OpenPGP))
           (plain (buffer-substring-no-properties start end))
           encrypted)
      (setf (epg-context-armor context) t)
      (when sign
        (vm-epg-set-signer context))
      (let ((keys (vm-epg-get-recipient-keys context)))
        ;; A nil recipient list makes `epg-encrypt-string' silently perform
        ;; symmetric (passphrase) encryption, which is never what the user
        ;; wants here.  Refuse instead.
        (unless keys
          (error "No usable PGP public key found for any recipient"))
        (condition-case err
            (setq encrypted (epg-encrypt-string context plain keys sign))
          (error
           (error "Encrypt error: %s" (error-message-string err)))))
      (delete-region start end)
      (insert encrypted))
    ;; Transfer-encode the composition *after* the armor is in place, so any
    ;; MIME encoding (e.g. quoted-printable for a non-ASCII body) is applied to
    ;; the armor too.  When called from the PGP/MIME `vm-epg-encrypt-internal'
    ;; path the composition is already encoded, so this is a no-op there.
    (vm-epg-encode-composition-maybe)))

;;;###autoload
(defun vm-epg-cleartext-sign ()
  "Sign the message body as cleartext PGP."
  (interactive)
  (save-excursion
    ;; Normalize but do NOT MIME-encode yet.  The OpenPGP cleartext signature
    ;; framework (RFC 4880 sec. 7) signs the *content*, and per RFC 2045 the
    ;; content is the transfer-*decoded* octets, so the verifier checks the
    ;; signature against the armor after MIME decoding.  We must therefore
    ;; sign the raw body and then transfer-encode the resulting armor as a
    ;; whole: otherwise a base64 signature line ending in "=" is read as a
    ;; quoted-printable soft line break and merges with the next line (e.g.
    ;; "-----END PGP SIGNATURE-----"), corrupting the signature.
    (vm-epg-normalize-composition-body)
    (let* ((start (point))
           (end (point-max))
           (context (epg-make-context 'OpenPGP))
           (plain (buffer-substring-no-properties start end))
           signed)
      (setf (epg-context-armor context) t)
      (vm-epg-set-signer context)
      (condition-case err
          (setq signed (epg-sign-string context plain 'clear))
        (error
         (error "Signing error: %s" (error-message-string err))))
      (delete-region start end)
      (insert signed))
    ;; Now transfer-encode, so the armor's "=" bytes are escaped (=3D) and
    ;; survive the recipient's MIME decoding intact.
    (vm-epg-encode-composition-maybe)))

(defun vm-epg-format-verify-result (result)
  "Format EPG verification RESULT (a list of `epg-signature' objects) as a string."
  (if (null result)
      "No signature result"
    (mapconcat
     (lambda (sig)
       (let ((status (epg-signature-status sig))
             (key-id (epg-signature-key-id sig))
             (validity (epg-signature-validity sig)))
         (format "%s signature from key %s (validity: %s)"
                 (cond ((eq status 'good) "Good")
                       ((eq status 'bad) "BAD")
                       (t (format "%s" status)))
                 (or key-id "unknown")
                 (or validity "unknown"))))
     result
     "\n")))

(defun vm-epg-fetch-missing-keys-p (context result)
  "Fetch public keys missing for RESULT into CONTEXT if enabled.
RESULT is a list of `epg-signature' objects.  When
`vm-epg-fetch-missing-keys' is non-nil and one or more signatures were made
by a key that is not in the local keyring (status `no-pubkey'), attempt to
receive those keys from a keyserver.  Return non-nil if any keys were
fetched, so the caller can verify the message again."
  (when vm-epg-fetch-missing-keys
    (let ((missing (delq nil
                         (mapcar (lambda (sig)
                                   (and (eq (epg-signature-status sig)
                                            'no-pubkey)
                                        (epg-signature-key-id sig)))
                                 result))))
      (when missing
        (condition-case _err
            (progn (epg-receive-keys context missing) t)
          (error nil))))))

;;;###autoload
(defun vm-epg-cleartext-verify ()
  "Verify the signature in the current message."
  (interactive)
  (message "Verifying PGP cleartext message...")
  (when (vm-interactive-p)
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p)))

  ;; make a presentation copy
  (unless (eq major-mode 'vm-presentation-mode)
    (vm-epg-make-presentation-copy))

  ;; verify
  (save-excursion
    (goto-char (point-min))
    ;; TODO Här skiljer sig EPG från PGG, granskas
    (let* ((buffer-read-only nil)
           (context (epg-make-context 'OpenPGP))
           (message-text (buffer-substring-no-properties (point) (point-max)))
           result status)
      (setf (epg-context-armor context) t)
      (condition-case _err
          (epg-verify-string context message-text)
        (error nil))
      (setq result (epg-context-result-for context 'verify))
      ;; If a signature was made by a key we do not have, optionally fetch it
      ;; from a keyserver and verify again.
      (when (vm-epg-fetch-missing-keys-p context result)
        (condition-case _err
            (epg-verify-string context message-text)
          (error nil))
        (setq result (epg-context-result-for context 'verify)))
      (vm-epg-state-set 'signed)
      (setq status
            (if (and result
                     (eq (epg-signature-status (car result)) 'good))
                'verified
              'error))
      (vm-epg-state-set status)
      (setq vm-epg-cleartext-output (vm-epg-format-verify-result result))
      (setq vm-epg-cleartext-output-face
            (if (eq status 'verified)
                'vm-epg-good-signature
              'vm-epg-bad-signature))
      (if (boundp 'vm-epg-cleartext-state)
          (setq vm-epg-cleartext-state status)
        (vm-epg-cleartext-cleanup status)))))

;;;###autoload
(defun vm-epg-cleartext-decrypt ()
  "Decrypt the contents of the current message."
  (interactive)
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)

  ;; make a presentation copy
  (unless (eq major-mode 'vm-presentation-mode)
    (vm-epg-make-presentation-copy))
  (goto-char (point-min))

  ;; decrypt
  ;; TODO Här skiljer sig EPG från PGG, granskas
  (let (start end cipher plain)
    (setq start (and (re-search-forward "^-----BEGIN PGP MESSAGE-----$" nil t)
                     (match-beginning 0))
          end   (and start
                     (re-search-forward "^-----END PGP MESSAGE-----$" nil t)
                     (match-end 0)))
    (unless (and start end)
      (error "No complete PGP MESSAGE armor found"))
    (setq cipher (buffer-substring-no-properties start end))

    (vm-epg-state-set 'encrypted)

    (condition-case err
        (setq plain (epg-decrypt-string (epg-make-context 'OpenPGP) cipher))
      (error
       (let ((buffer-read-only nil))
         (vm-epg-state-set 'error)
         (goto-char start)
         (let ((msg (error-message-string err)))
           (insert msg)
           (put-text-property start (point) 'face 'vm-epg-error)))))

    (when plain
      ;; replace cipher with plaintext
      (let ((buffer-read-only nil))
        (delete-region start end)
        (insert plain))

      ;; if the decrypted content is signed, also verify it
      (goto-char start)
      (when (looking-at "^-----BEGIN PGP \\(SIGNED \\)?MESSAGE-----$")
        (vm-epg-cleartext-verify)))))

;;; CRLF utilities

(defun vm-epg-crlf-cleanup (start end)
  "Convert CRLF to LF in region from START to END."
  (save-excursion
    (goto-char start)
    (while (search-forward "\r\n" end t)
      (replace-match "\n" t t))))

(defun vm-epg-make-crlf (start end)
  "Convert LF to CRLF in region from START to END."
  (save-excursion
    (goto-char end)
    (while (search-backward "\n" start t)
      (replace-match "\r\n" t t)
      (backward-char))))

;;; MIME state tracking

(defvar vm-epg-mime-decoded nil
  "Saves decoded state for later use, i.e. decoding to buttons.")
(make-variable-buffer-local 'vm-epg-mime-decoded)

(defun vm-epg-get-mime-decoded ()
  "Return `vm-epg-mime-decoded'."
  (save-excursion
    (vm-select-folder-buffer)
    vm-epg-mime-decoded))

(defvar vm-epg-recursion nil
  "Detect recursive calls.")

(advice-add 'vm-decode-mime-message :around #'vm-epg--clear-state)
(defun vm-epg--clear-state (orig-fun &rest args)
  "Clear the modeline state before decoding."
  (vm-select-folder-buffer)
  (when (not vm-epg-recursion)
    (setq vm-epg-mime-decoded vm-mime-decoded))
  (setq vm-epg-state-message nil)
  (setq vm-epg-state nil)
  (if (vm-mime-plain-message-p (car vm-message-pointer))
      (if vm-epg-cleartext-decoded
          (vm-present-current-message))
    (let ((vm-epg-recursion t))
      (apply orig-fun args))))

;;; MIME multipart/encrypted handler

(defun vm-epg-mime-decrypt (button)
  "Decrypt the MIME part associated with BUTTON."
  (let ((vm-epg-auto-decrypt t)
        (layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    (goto-char (vm-extent-start-position button))
    (let ((buffer-read-only nil))
      (vm-decode-mime-layout button t))))

;;;###autoload
(defun vm-mime-display-internal-multipart/encrypted (layout)
  "Display multipart/encrypted LAYOUT."
  (vm-epg-state-set 'encrypted)
  (let* ((part-list (vm-mm-layout-parts layout))
         (header (car part-list))
         (message (car (cdr part-list))))
    (cond ((eq (vm-epg-get-mime-decoded) 'decoded)
           nil)
          ((not (and (= (length part-list) 2)
                     (vm-mime-types-match (car (vm-mm-layout-type header))
                                          "application/pgp-encrypted")
                     (vm-mime-types-match (car (vm-mm-layout-type message))
                                          "application/octet-stream")))
           (insert "Unknown multipart/encrypted format."))
          ((not vm-epg-auto-decrypt)
           (let ((buffer-read-only nil))
             (vm-mime-insert-button
              :caption
              (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
              :action 'vm-epg-mime-decrypt
              :layout layout)))
          (t
	   ;; TODO Här skiljer sig EPG och PGG, granskas
           (let* ((cipher
                   (with-current-buffer (vm-buffer-of
                                         (vm-mm-layout-message message))
                     (save-restriction
                       (widen)
                       (buffer-substring-no-properties
                        (vm-mm-layout-body-start message)
                        (vm-mm-layout-body-end message)))))
                  (context (epg-make-context 'OpenPGP))
                  plain)
             (condition-case err
                 (setq plain (epg-decrypt-string context cipher))
               (error
                (vm-epg-state-set 'error)
                (let ((start (point)))
                  (insert (error-message-string err))
                  (put-text-property start (point) 'face 'vm-epg-error))))
             (when plain
             (let* ((epg-buf (get-buffer-create " *vm-epg-decrypted*"))
                    parsed)
               (with-current-buffer epg-buf
                 (erase-buffer)
                 (insert plain)
                 (vm-epg-crlf-cleanup (point-min) (point-max))
                 (setq parsed (vm-mime-parse-entity-safe
                               nil :passing-message-only t)))
               (if parsed
                   (vm-decode-mime-layout parsed)
                 (insert-buffer-substring epg-buf)))
             ;; check if the decrypted content was also signed
             (let ((verify-result (epg-context-result-for context 'verify)))
               (when verify-result
                 (let ((sig (car verify-result)))
                   (if (eq (epg-signature-status sig) 'good)
                       (progn
                         (vm-epg-state-set 'signed 'verified)
                         (let ((start (point)))
                           (insert "\n" (vm-epg-format-verify-result verify-result) "\n")
                           (put-text-property start (point) 'face
                                              'vm-epg-good-signature)))
                     (vm-epg-state-set 'signed 'error)))))
             t))))
    ;; Always report the part as handled -- even on decrypt failure or an
    ;; unrecognised structure -- so VM does not fall through and re-render the
    ;; raw ciphertext parts as multipart/mixed.
    t))

;;; MIME multipart/signed handler

;;;###autoload
(defun vm-mime-display-internal-multipart/signed (layout)
  "Display multipart/signed LAYOUT."
  (vm-epg-state-set 'signed)
  (let* ((part-list (vm-mm-layout-parts layout))
         (message (car part-list))
         (signature (car (cdr part-list)))
         status start end)
    (cond ((eq (vm-epg-get-mime-decoded) 'decoded)
           nil)
          ((not (and (= (length part-list) 2)
                     signature
                     (vm-mime-types-match (car (vm-mm-layout-type signature))
                                          "application/pgp-signature")))
           ;; insert the message
           (vm-decode-mime-layout message)
           (vm-epg-state-set 'unknown)
           (setq start (point))
           (insert
            (format
             "******* unknown signature type %s *******\n"
             (car (and signature (vm-mm-layout-type signature)))))
           (setq end (point))
           (when signature
             (vm-decode-mime-layout signature))
           (put-text-property start end 'face 'vm-epg-unknown-signature-type)
           t)
          (t
           ;; insert the message content
           (vm-decode-mime-layout message)
           ;; collect the raw signature bytes
           (setq start (point))
           (vm-mime-insert-mime-body signature)
           (setq end (point))
	   ;; TODO Här skiljer sig EPG från PGG, granskas
           (let ((sig-string (buffer-substring-no-properties start end)))
             (delete-region start end)
             ;; collect the raw signed content (with CRLF per RFC 3156)
             (setq start (point))
             (vm-insert-region-from-buffer
              (marker-buffer (vm-mm-layout-header-start message))
              (vm-mm-layout-header-start message)
              (vm-mm-layout-body-end message))
             (setq end (point-marker))
             (vm-epg-make-crlf start end)
             (let ((signed-text (buffer-substring-no-properties start end)))
               (delete-region start end)
               ;; verify
               (let ((context (epg-make-context 'OpenPGP)))
                 (condition-case _err
                     (epg-verify-string context sig-string signed-text)
                   (error nil))
                 (setq status (epg-context-result-for context 'verify))
                 ;; Fetch a missing signer key from a keyserver, then re-verify.
                 (when (vm-epg-fetch-missing-keys-p context status)
                   (condition-case _err
                       (epg-verify-string context sig-string signed-text)
                     (error nil))
                   (setq status (epg-context-result-for context 'verify))))
               ;; insert verification result
               (insert "\n")
               (setq start (point))
               (let ((good (and status
                                (eq (epg-signature-status (car status)) 'good))))
                 (if good
                     (progn
                       (vm-epg-state-set 'verified)
                       (insert (vm-epg-format-verify-result status))
                       (vm-epg-crlf-cleanup start (point)))
                   (vm-epg-state-set 'error)
                   (insert (vm-epg-format-verify-result status)))
                 (setq end (point))
                 (put-text-property start end 'face
                                    (if good
                                        'vm-epg-good-signature
                                      'vm-epg-bad-signature)))))
           t))))

;;; application/pgp-keys handler

(eval-and-compile
  (if (listp vm-mime-internal-content-types)
      (add-to-list 'vm-mime-internal-content-types "application/pgp-keys"))
  (add-to-list 'vm-mime-button-format-alist
               '("application/pgp-keys" . "Snarf %d"))
  (add-to-list 'vm-mime-button-format-alist
               '("multipart/encrypted" . "Decrypt PGP/MIME message")))

(defun vm-epg-mime-snarf-keys (button)
  "Import the keys from the MIME part associated with BUTTON."
  (let ((vm-epg-auto-snarf t)
        (layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    (goto-char (vm-extent-start-position button))
    (let ((buffer-read-only nil))
      (vm-decode-mime-layout button t))))

;;;###autoload
(defun vm-mime-display-internal-application/pgp-keys (layout)
  "Import keys from LAYOUT and display the result."
  (vm-epg-state-set 'public-key)
  (if vm-epg-auto-snarf
      (let ((start (point)) end)
        (vm-mime-insert-mime-body layout)
        (setq end (point-marker))
        (vm-mime-transfer-decode-region layout start end)
	;; TODO Här skiljer sig EPG från PGG, granskas
        (let* ((key-text (buffer-substring-no-properties start end))
               (context (epg-make-context 'OpenPGP))
               import-result)
          (delete-region start end)
          (condition-case err
              (progn
                (epg-import-keys-from-string context key-text)
                (setq import-result (epg-context-result-for context 'import))
                (insert (format "Imported %d key(s).\n"
                                (if import-result
                                    (epg-import-result-considered import-result)
                                  0))))
            (error
             (insert (format "Key import failed: %s\n"
                             (error-message-string err)))))))
    (let ((buffer-read-only nil))
      (vm-mime-insert-button
       :caption
       (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
       :action 'vm-epg-mime-snarf-keys
       :layout layout)))
  t)

;;;###autoload
(defun vm-epg-snarf-keys ()
  "Snarf keys from the current message."
  (interactive)
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (save-restriction
    (if vm-presentation-buffer
        (set-buffer vm-presentation-buffer))
    (goto-char (point-min))
    (search-forward "\n\n")
    (goto-char (match-end 0))
    ;; TODO Här skiljer sig EPG från PGG, granskas
    (let* ((key-text (buffer-substring-no-properties (point) (point-max)))
           (context (epg-make-context 'OpenPGP)))
      (condition-case err
          (progn
            (epg-import-keys-from-string context key-text)
            (let ((result (epg-context-result-for context 'import)))
              (message "Imported %d key(s)."
                       (if result
                           (epg-import-result-considered result)
                         0))))
        (error
         (error "Snarfing failed: %s" (error-message-string err)))))))

;;; Public key attachment

;;;###autoload
(defun vm-epg-attach-public-key ()
  "Attach your public key to a composition."
  (interactive)
  (let* ((author (or (and vm-epg-get-author-headers (vm-epg-get-author))
                     (read-string "User ID: ")))
         (context (epg-make-context 'OpenPGP))
         (keys (epg-list-keys context author))
         (description (concat "public key of " author))
         (buffer (get-buffer-create (concat " *" description "*")))
         start)
    (unless keys
      (error "%s has no public key!" author))
    (with-current-buffer buffer
      (erase-buffer)
      (setq start (point))
      ;; TODO Samma felmeddelande tillagt ovan, ändrad kod
      (insert (epg-export-keys-to-string context keys))
      (when (= start (point))
        (error "%s has no public key!" author)))
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (setq start (point))
      (vm-attach-object buffer
                        :type "application/pgp-keys"
                        :params (list (concat "name=\"" author ".asc\""))
                        :description description)
      (let ((disposition (list "attachment"
                               (concat "filename=\"" author ".asc\"")))
            (end (point)))
        (put-text-property start end 'vm-mime-disposition disposition)))))

;;;###autoload
(defun vm-epg-insert-public-key ()
  "Insert your public key into the composition at point."
  (interactive)
  (let* ((author (or (and vm-epg-get-author-headers (vm-epg-get-author))
                     (read-string "User ID: ")))
         (context (epg-make-context 'OpenPGP))
         (keys (epg-list-keys context author)))
    (unless keys
      (error "%s has no public key!" author))
    (insert (epg-export-keys-to-string context keys))))

;;; MIME multipart boundary

(defun vm-epg-make-multipart-boundary (word)
  "Create a MIME part boundary starting with WORD and return it."
  (if word (setq word (concat word "+")))
  (let ((boundary (concat word (make-string 15 ?a)))
        (i (length word)))
    (random)
    (while (< i (length boundary))
      (aset boundary i (aref vm-mime-base64-alphabet
                             (random (length vm-mime-base64-alphabet))))
      (vm-increment i))
    boundary))

(defun vm-epg-save-work (function &rest args)
  "Call FUNCTION with ARGS, restoring the composition buffer on error."
  (let ((composition-buffer (current-buffer))
        (work-buffer (get-buffer-create " *VM-EPG-WORK*")))
    (with-current-buffer work-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (insert-buffer-substring composition-buffer)
      (setq major-mode 'mail-mode)
      (apply function args))
    (vm-mail-mode-show-headers)
    (erase-buffer)
    (insert-buffer-substring work-buffer)
    (kill-buffer work-buffer)))

;;; Digest algorithm name for micalg header

(defun vm-epg-digest-algo-name (algo-id)
  "Return the lowercase name of digest algorithm with id ALGO-ID.
Falls back to \"sha256\" for unknown IDs."
  (let ((entry (assq algo-id epg-digest-algorithm-alist)))
    (if entry
        (downcase (cdr entry))
      "sha256")))

;;; Sign composition

;;;###autoload
(defun vm-epg-sign ()
  "Sign the composition with PGP/MIME.

If the composition is not yet encoded, it is encoded before signing.
Signing of already 8bit-encoded messages is discouraged.

RFC 2015 and its successor 3156 forbid the use of 8bit encoding for signed
messages.  Lines starting with \"From \" also cause problems.

To avoid issues, ensure `vm-mime-8bit-text-transfer-encoding' is not 8bit
and `vm-mime-composition-armor-from-lines' is t."
  (interactive)

  (when (vm-mail-mode-get-header-contents "MIME-Version:")
    (goto-char (point-min))
    (when (re-search-forward "Content-Transfer-Encoding:\\s-*8bit" nil t)
      (describe-function 'vm-epg-sign)
      (error "Signing is broken for 8bit encoding!"))
    (goto-char (point-min))
    (when (re-search-forward "^From\\s-+" nil t)
      (describe-function 'vm-epg-sign)
      (error "Signing is broken for lines starting with \"From \"!")))

  (vm-epg-save-work 'vm-epg-sign-internal))

(defun vm-epg-sign-internal ()
  "Perform the PGP/MIME signing."
  (let ((vm-mime-8bit-text-transfer-encoding
         vm-epg-sign-text-transfer-encoding)
        (vm-mime-composition-armor-from-lines t))
    (vm-epg-prepare-composition))

  (let ((content-type (vm-mail-mode-get-header-contents "Content-Type:"))
        (encoding (vm-mail-mode-get-header-contents "Content-Transfer-Encoding:"))
        (boundary (vm-epg-make-multipart-boundary "pgp+signed"))
        (micalg "sha256")
        body-start)
    ;; prepare body
    (setq body-start (vm-marker (vm-epg-goto-body-start)))
    (insert "Content-Type: " (or content-type "text/plain") "\n")
    (insert "Content-Transfer-Encoding: " (or encoding "7bit") "\n")
    (unless (looking-at "\n")
      (insert "\n"))
    ;; sign
    (save-excursion
      (let* ((context (epg-make-context 'OpenPGP))
             ;; RFC 3156: the signature must be computed over the MIME
             ;; canonical (CRLF) form of the body.
             (body-text (let ((lf-text (buffer-substring-no-properties
                                        body-start (point-max))))
                          (with-temp-buffer
                            (insert lf-text)
                            (vm-epg-make-crlf (point-min) (point-max))
                            (buffer-string))))
             signature)
        (setf (epg-context-armor context) t)
        (vm-epg-set-signer context)
        (condition-case err
            (setq signature (epg-sign-string context body-text 'detached))
          (error
           (error "Signing error: %s" (error-message-string err))))
        ;; extract micalg from the signing result
        (let ((sign-result (epg-context-result-for context 'sign)))
          (when (and sign-result (car sign-result))
            (setq micalg
                  (vm-epg-digest-algo-name
                   (epg-new-signature-digest-algorithm (car sign-result))))))
        ;; assemble signed MIME structure
        (goto-char body-start)
        (insert "This is an OpenPGP/MIME signed message (RFC 2440 and 3156)\n")
        (insert "--" boundary "\n")
        (goto-char (point-max))
        (insert "\n--" boundary "\n")
        (insert "Content-Type: application/pgp-signature\n\n")
        (insert signature)
        (insert "\n--" boundary "--\n")))
    ;; fix headers
    (vm-mail-mode-remove-header "MIME-Version:")
    (vm-mail-mode-remove-header "Content-Type:")
    (vm-mail-mode-remove-header "Content-Transfer-Encoding:")
    (mail-position-on-field "MIME-Version")
    (insert "1.0")
    (mail-position-on-field "Content-Type")
    (insert "multipart/signed; boundary=\"" boundary "\";\n"
            "\tmicalg=pgp-" micalg "; protocol=\"application/pgp-signature\"")))

;;; Encrypt composition

;;;###autoload
(defun vm-epg-encrypt (&optional sign)
  "Encrypt the composition as PGP/MIME.  With a prefix arg SIGN also sign it."
  (interactive "P")
  (vm-epg-save-work 'vm-epg-encrypt-internal sign))

(defun vm-epg-encrypt-internal (sign)
  "Perform PGP/MIME encryption; if SIGN is non-nil also sign."
  (unless (vm-mail-mode-get-header-contents "MIME-Version:")
    (if vm-do-fcc-before-mime-encode
        (vm-do-fcc-before-mime-encode))
    (vm-mime-encode-composition))
  (let ((content-type (vm-mail-mode-get-header-contents "Content-Type:"))
        (encoding (vm-mail-mode-get-header-contents "Content-Transfer-Encoding:"))
        (boundary (vm-epg-make-multipart-boundary "pgp+encrypted"))
        body-start)
    (setq body-start (vm-marker (vm-epg-goto-body-start)))
    (insert "Content-Type: " (or content-type "text/plain") "\n")
    (insert "Content-Transfer-Encoding: " (or encoding "7bit") "\n")
    (insert "\n")
    (goto-char (point-max))
    (insert "\n")
    ;; encrypt the body (cleartext style, result replaces body)
    (vm-epg-cleartext-encrypt sign)
    ;; wrap in multipart/encrypted structure
    (goto-char body-start)
    (insert "This is an OpenPGP/MIME encrypted message (RFC 2440 and 3156)\n")
    (insert "--" boundary "\n")
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert "--" boundary "\n")
    (insert "Content-Type: application/octet-stream\n\n")
    (goto-char (point-max))
    (insert "\n--" boundary "--\n")
    ;; fix headers
    (vm-mail-mode-remove-header "MIME-Version:")
    (vm-mail-mode-remove-header "Content-Type:")
    (vm-mail-mode-remove-header "Content-Transfer-Encoding:")
    (mail-position-on-field "MIME-Version")
    (insert "1.0")
    (mail-position-on-field "Content-Type")
    (insert "multipart/encrypted; boundary=\"" boundary "\";\n"
            "\tprotocol=\"application/pgp-encrypted\"")))

(defun vm-epg-sign-and-encrypt ()
  "Sign and encrypt the composition as PGP/MIME."
  (interactive)
  (vm-epg-encrypt t))

;;; Ask hook

(defvar vm-epg-prompt-last-action nil
  "The action last taken in `vm-epg-prompt-for-action'.")

(defvar vm-epg-prompt-action-alist
  '((?s sign "Sign")
    (?e encrypt "encrypt")
    (?E sign-and-encrypt "both")
    (?n nil "nothing")
    (?q quit "quit"))
  "Alist of (KEY ACTION LABEL) elements for `vm-epg-prompt-for-action'.")

(defun vm-epg-prompt-for-action ()
  "Prompt for a PGP action and return it."
  (interactive)
  (let (prompt event action)
    (setq prompt (mapconcat (lambda (a)
                              (format "%s (%c)" (nth 2 a) (car a)))
                            vm-epg-prompt-action-alist ", ")
          action (mapcar (lambda (a)
                           (if (eq (nth 1 a) vm-epg-prompt-last-action)
                               (downcase (nth 2 a))))
                         vm-epg-prompt-action-alist)
          prompt (format "%s (default %s)?"
                         prompt
                         (car (delete nil action)))
          action nil)
    (while (not event)
      (setq event (read-key-sequence prompt))
      (setq event (if (stringp event) (aref event 0)))
      (if (eq event ?\r)
          (setq action vm-epg-prompt-last-action)
        (setq action (assoc event vm-epg-prompt-action-alist))
        (if action
            (setq action (nth 1 action))
          (setq event nil))))
    (when (eq action 'quit)
      (error "Sending aborted!"))
    (if action
        (message "Action is %s." action)
      (message "No action selected."))
    (setq vm-epg-prompt-last-action action)
    action))

;;;###autoload
(defun vm-epg-ask-hook ()
  "Ask whether to sign or encrypt outgoing messages with PGP/MIME.

Add to `vm-mail-send-hook' to be asked each time you send a message.
See `vm-epg-ask-function' to determine which function is used.

This hook should be last in `vm-mail-send-hook' as signing depends on the
message not being modified afterwards.  Add it like:

       (add-hook \\='vm-mail-send-hook #\\='vm-epg-ask-hook t)"
  (interactive)

  ;; ensure we are the last hook
  (when (and (member 'vm-epg-ask-hook vm-mail-send-hook)
             (cdr (member 'vm-epg-ask-hook vm-mail-send-hook)))
    (describe-function 'vm-epg-ask-hook)
    (error "`vm-epg-ask-hook' must be the last hook in `vm-mail-send-hook'!"))

  (let ((handler vm-epg-ask-function)
        action)
    (when handler
      (setq action (if (fboundp handler)
                       (funcall handler)
                     (if (y-or-n-p (format "%s the composition? " handler))
                         handler)))
      (when action
        (funcall (intern (format "vm-epg-%s" action)))))))

(provide 'vm-epg)

;;; vm-epg.el ends here
