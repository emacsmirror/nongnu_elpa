;;; vm-epg.el --- PGP/MIME support for VM via epg.el  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 2026 The VM Developers
;;
;; Author:      The VM Developers
;; Keywords:    VM helpers, PGP, OpenPGP, mail
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
;; Do NOT load vm-pgg and vm-epg together.  Both define the same MIME
;; display handlers -- `vm-mime-display-internal-multipart/encrypted',
;; `vm-mime-display-internal-multipart/signed' and
;; `vm-mime-display-internal-application/pgp-keys' -- so whichever package
;; is loaded last silently wins, and the other package's customizations
;; then have no effect.  vm-pgg is deprecated; remove any `(require
;; 'vm-pgg)' from your configuration when switching to vm-epg.
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
;; To create messages according to PGP/MIME use one of:
;;  * C-c # s  M-x vm-epg-sign              sign
;;  * C-c # e  M-x vm-epg-encrypt           encrypt
;;  * C-c # E  M-x vm-epg-sign-and-encrypt  sign + encrypt
;;
;; A prefix argument to `vm-epg-encrypt' (C-u M-x vm-epg-encrypt) also signs,
;; and is equivalent to `vm-epg-sign-and-encrypt'.
;;
;; PGP/MIME is the recommended format.  For the older inline ("cleartext")
;; format, where the ASCII armor is placed directly in the message body, use:
;;  * C-c # C-s  M-x vm-epg-cleartext-sign     sign inline
;;  * C-c # C-e  M-x vm-epg-cleartext-encrypt  encrypt inline
;;
;; Inline PGP cannot cover attachments and interacts badly with MIME
;; transfer encodings; prefer the PGP/MIME commands above.  Incoming inline
;; PGP is always handled, regardless of which format you send.
;;
;; Public keys can be sent with:
;;  * C-c # k  M-x vm-epg-attach-public-key  attach as application/pgp-keys
;;  *          M-x vm-epg-insert-public-key  insert armor at point
;;
;; To be asked at send time whether to sign or encrypt, use
;; `vm-epg-ask-hook' (C-c # a); see its docstring for how to install it.
;;
;; All these commands are also available in the menu PGP/MIME which is
;; activated by the minor mode `vm-epg-compose-mode'.

;;; References:
;;
;; For PGP/MIME see:
;; * https://www.rfc-editor.org/rfc/rfc3156  PGP/MIME (obsoletes RFC 2015)
;; * https://www.rfc-editor.org/rfc/rfc4880  OpenPGP (obsoletes RFC 2440)
;;
;; Both RFC 2015 and RFC 2440 are obsolete and are listed only because
;; older PGP/MIME implementations, and some of the message text generated
;; here, still refer to them:
;; * https://www.rfc-editor.org/rfc/rfc2015
;; * https://www.rfc-editor.org/rfc/rfc2440
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

;; The modeline faces below inherit from `mode-line' so that the state
;; indicator keeps the modeline's background and box.  (vm-pgg spelled this
;; `modeline', the XEmacs name; in GNU Emacs the obsolete `modeline' alias
;; has been removed, so inheriting from it silently yields no attributes.)

(defface vm-epg-good-signature-modeline
  '((((type tty) (class color))
     (:inherit mode-line :foreground "green" :bold t))
    (((type tty))
     (:inherit mode-line :bold t))
    (((background light))
     (:inherit mode-line :foreground "green4"))
    (((background dark))
     (:inherit mode-line :foreground "green")))
  "The face used to highlight good signature messages in the modeline."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-unknown-signature-type-modeline
  '((((type tty) (class color))
     (:inherit mode-line :bold t))
    (((type tty))
     (:inherit mode-line :bold t)))
  "The face used to highlight unknown signature types in the modeline."
  :group 'vm-epg
  :group 'faces)

(defface vm-epg-error-modeline
  '((((type tty) (class color))
     (:inherit mode-line :foreground "red" :bold t))
    (((type tty))
     (:inherit mode-line :bold t))
    (((background light))
     (:inherit mode-line :foreground "red"))
    (((background dark))
     (:inherit mode-line :foreground "red")))
  "The face used to highlight error and bad signature messages in the modeline."
  :group 'vm-epg
  :group 'faces)

;;; Customizable variables

(defcustom vm-epg-fetch-missing-keys t
  "If non-nil, fetch missing keys from a keyserver when verifying signatures.
When a signature was made by a key that is not in your keyring, contact the
keyserver configured for GnuPG to retrieve it and then verify again.  This
makes displaying a signed message reach out to the network."
  :group 'vm-epg
  :type 'boolean)

(defcustom vm-epg-auto-snarf t
  "If non-nil, snarf public keys automatically.
Snarfing means importing the public keys found in a message into your GnuPG
keyring.  When nil, a button is shown instead and keys are imported only
when you activate it."
  :group 'vm-epg
  :type 'boolean)

(defcustom vm-epg-auto-decrypt t
  "If non-nil, decrypt encrypted messages automatically when displaying them.
When nil, a button is shown instead and decryption happens only when you
activate it."
  :group 'vm-epg
  :type 'boolean)

(defcustom vm-epg-get-author-headers '("From:" "Sender:")
  "The list of headers used to identify the author of an outgoing message.
The first address found in these headers is used to select the signing key.
If nil, the default EPG signing key is used."
  :group 'vm-epg
  :type '(repeat string))

(defcustom vm-epg-sign-text-transfer-encoding 'quoted-printable
  "The content transfer encoding used for signed MIME parts of type text.

RFC 3156 forbids 8bit encoding in signed messages, because a gateway that
re-encodes the body would invalidate the signature.  `vm-epg-sign'
therefore binds `vm-mime-8bit-text-transfer-encoding' to this value while
it encodes the composition, overriding your normal setting for the duration
of the signing operation.

Both choices are signature-safe; `quoted-printable' keeps mostly-ASCII text
readable to humans and to non-MIME tools, while `base64' is more compact for
text that is largely non-ASCII."
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
    ;; Inline (cleartext) PGP, on the control-modified variants of the
    ;; corresponding PGP/MIME bindings.
    (define-key map "\C-c#\C-s" 'vm-epg-cleartext-sign)
    (define-key map "\C-c#\C-e" 'vm-epg-cleartext-encrypt)
    map)
  "Keymap for `vm-epg-compose-mode'.")

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
    ["Insert Public Key" vm-epg-insert-public-key t]
    "----"
    ["Sign (inline PGP)"    vm-epg-cleartext-sign t]
    ["Encrypt (inline PGP)" vm-epg-cleartext-encrypt t]))

(defvar vm-epg-compose-mode nil
  "Non-nil when `vm-epg-compose-mode' is active in this buffer.
Its key bindings and PGP/MIME menu are then available.")

(make-variable-buffer-local 'vm-epg-compose-mode)

(defun vm-epg-compose-mode (&optional arg)
  "Minor mode for composing PGP/MIME messages with EPG.

Toggle the mode when ARG is nil, enable it when ARG is a positive number
and disable it otherwise.  Enabling makes the bindings below and the
PGP/MIME menu available in the composition buffer.

\\{vm-epg-compose-mode-map}"
  (interactive)
  (setq vm-epg-compose-mode
        (if (null arg) (not vm-epg-compose-mode)
          (> (prefix-numeric-value arg) 0))))

(defvar vm-epg-compose-mode-string " vm-epg"
  "String to put in mode line when `vm-epg-compose-mode' is active.")

(defcustom vm-epg-ask-function 'vm-epg-prompt-for-action
  "What `vm-epg-ask-hook' should do before sending a message.

The value is either an action symbol or a function:

  nil               do nothing;
  `sign'            ask whether to sign;
  `encrypt'         ask whether to encrypt;
  `sign-and-encrypt' ask whether to sign and encrypt;
  a function        called with no arguments, returning one of the action
                    symbols above, or nil for no action.

An action symbol ACTION selects the command `vm-epg-ACTION', so any value
other than those listed must name an existing `vm-epg-' command."
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
           :tag "sign and encrypt"
           :doc "Ask whether to sign and encrypt the message before sending"
           sign-and-encrypt)
          (function
           :tag "ask for the action"
           :doc "Prompt for an action via `vm-epg-prompt-for-action'"
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
  "Activate `vm-epg-compose-mode'.
Added to `vm-mail-mode-hook' when this file is loaded, so every VM
composition buffer gets the PGP/MIME bindings and menu."
  (vm-epg-compose-mode 1))

(add-hook 'vm-mail-mode-hook 'vm-epg-compose-mode-activate t)

;;; Address/key helpers

(defun vm-epg-get-emails (headers)
  "Return the email addresses found in the composition's HEADERS.
HEADERS is a list of header names including the colon, as in \\='(\"To:\").
Only meaningful in a composition buffer."
  (let (content addresses)
    (while headers
      (setq content (vm-mail-mode-get-header-contents (car headers)))
      (when content
        (setq addresses (append (rfc822-addresses content) addresses)))
      (setq headers (cdr headers)))
    addresses))

(defcustom vm-epg-get-recipients-headers '("To:" "CC:" "BCC:")
  "The list of headers used to identify the recipients of an outgoing message.
Every address found in these headers must have a usable encryption key, or
encryption fails."
  :group 'vm-epg
  :type '(repeat string))

(defun vm-epg-get-recipients ()
  "Return the recipient email addresses of the composition.
Collected from the headers named by `vm-epg-get-recipients-headers'."
  (vm-epg-get-emails vm-epg-get-recipients-headers))

(defun vm-epg-get-author ()
  "Return the email address of the composition's author, or nil.
This is the first address found in the headers named by
`vm-epg-get-author-headers'."
  (car (vm-epg-get-emails vm-epg-get-author-headers)))

(defun vm-epg-find-usable-key (keys usage addr)
  "Return the first key in KEYS usable for USAGE, which is `sign' or `encrypt'.
A key qualifies when one of its subkeys is capable of USAGE and is neither
revoked nor expired.  ADDR is the address the keys were looked up for, used
only in the error message.

Signal an error when no key in KEYS qualifies; never return nil.  Silently
dropping an unusable key would produce a message that is not encrypted to,
or not signed for, the address the user named."
  (catch 'found
    (while keys
      (let ((pointer (epg-key-sub-key-list (car keys))))
        (while pointer
          (if (and (memq usage (epg-sub-key-capability (car pointer)))
                   (not (memq (epg-sub-key-validity (car pointer))
                              '(revoked expired))))
              (throw 'found (car keys)))
          (setq pointer (cdr pointer))))
      (setq keys (cdr keys)))
    ;; No usable key found
    (error
     "No usable %s key found for %s" usage addr)))

(defun vm-epg-get-recipient-keys (context)
  "Return the list of EPG keys to encrypt the composition to.
CONTEXT is the `epg-context' the keys are looked up in.  There is one key per
address returned by `vm-epg-get-recipients'; an address containing \"@\" is
looked up bracketed, as \"<addr>\", so that it matches a full user ID rather
than any substring.

Signal an error, via `vm-epg-find-usable-key', if any recipient has no
usable encryption key."
  (mapcar (lambda (addr)
            (vm-epg-find-usable-key
             (epg-list-keys context
                            (if (string-search "@" addr)
				(concat "<" addr ">")
                              addr))
             'encrypt
	     addr))
          (vm-epg-get-recipients)))

(defun vm-epg-set-signer (context)
  "Set the signing key in CONTEXT to the composition author's secret key.
The author comes from `vm-epg-get-author'.  When no author address can be
found, CONTEXT is left alone, so EPG falls back on GnuPG's default signing
key; when one is found but has no usable secret signing key, signal an
error rather than silently signing as somebody else."
  (let ((author (vm-epg-get-author)))
    (when author
      (let ((signer
	     (vm-epg-find-usable-key
	      (epg-list-keys context author 'secret)
	      'sign
	      author)))
        (setf (epg-context-signers context) (list signer))))))

;;; Composition helpers

(defun vm-epg-goto-body-start ()
  "Move point past the composition's header separator and return point.
Signal an error if the buffer has no `mail-header-separator' line, which
means it is not a composition buffer."
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n"))
  (goto-char (match-end 0))
  (point))

(defun vm-epg-encode-composition-maybe ()
  "MIME-encode the composition unless it is already encoded.
The presence of a MIME-Version header is taken to mean it is.  Also performs
any pending FCC first when `vm-do-fcc-before-mime-encode' is set, so that the
filed copy is the unencoded one."
  (unless (vm-mail-mode-get-header-contents "MIME-Version:")
    (if vm-do-fcc-before-mime-encode
        (vm-do-fcc-before-mime-encode))
    (vm-mime-encode-composition)))

(defun vm-epg-normalize-composition-body ()
  "Normalize the composition body and leave point at its start.
Reveal any hidden headers, strip trailing whitespace, and ensure the body
ends in exactly one newline.  Return point, the start of the body.

Does not MIME-encode; callers that need that must arrange it themselves,
either before (see `vm-epg-prepare-composition') or after (see
`vm-epg-cleartext-sign')."
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
  "The PGP state of the currently viewed message, as modeline constructs.
This is a list of strings spliced into `vm-mode-line-format': the prefix
\"PGP:\" followed by one entry per state reported by `vm-epg-state-set'.
Nil means no PGP state is shown.  It is kept in sync across the folder,
presentation and summary buffers.")
(make-variable-buffer-local 'vm-epg-state)

(defvar vm-epg-state-message nil
  "The message that `vm-epg-state' describes.
`vm-epg-state-set' compares this against the current message to detect that
the user has moved on, and clears the stale state.")
(make-variable-buffer-local 'vm-epg-state-message)

(defvar vm-epg-mode-line-items
  (list (cons 'verified
              (propertize " verified" 'face 'vm-epg-good-signature-modeline))
        (cons 'unknown
              (propertize " unknown" 'face
                          'vm-epg-unknown-signature-type-modeline))
        (cons 'error
              (propertize " ERROR" 'face 'vm-epg-error-modeline)))
  "Alist mapping a state symbol to the modeline string shown for it.
The string is displayed by `vm-epg-state-set' as part of `vm-epg-state'.
States absent from this alist -- `signed', `encrypted' and `public-key' --
are shown unpropertized as \" STATE\".")

(if (not (member 'vm-epg-state vm-mode-line-format))
    (setq vm-mode-line-format (append '("" vm-epg-state) vm-mode-line-format)))

(defun vm-epg-state-set (&rest states)
  "Add STATES to the PGP status shown in the modeline for the current message.
Each of STATES is a symbol such as `signed', `encrypted', `verified',
`error', `unknown' or `public-key'; it is rendered via
`vm-epg-mode-line-items'.  States accumulate, so a signed and encrypted
message can report both.

Calling with no STATES only refreshes: the accumulated state is discarded
whenever the current message differs from `vm-epg-state-message', so moving
to another message clears the display.  The result is propagated to the
folder, presentation and summary buffers."
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
  "Regexp matching the start of an inline PGP ASCII armor.
Group 1 is the armor type -- \"SIGNED MESSAGE\", \"MESSAGE\" or \"PUBLIC KEY
BLOCK\" -- which selects what `vm-epg-cleartext-automode' does with it, and
which is also the value to substitute into `vm-epg-cleartext-end-regexp' to
find the matching end line.")

(defvar vm-epg-cleartext-end-regexp
  "^-----END PGP %s-----$"
  "Format string producing a regexp matching the end of an inline PGP armor.
Pass it through `format' with the armor type captured by group 1 of
`vm-epg-cleartext-begin-regexp'; it is not a usable regexp on its own.")

(defcustom vm-epg-cleartext-search-limit 4096
  "Number of bytes to search into the message for a PGP clear text armor."
  :type 'integer
  :group 'vm-epg)

(defun vm-epg-make-presentation-copy ()
  "Make a presentation copy of the current message for inline PGP work.
The cleartext commands rewrite the message text in place -- stripping armor,
inserting plaintext or verification results -- so they must operate on a
presentation copy, leaving the folder itself unmodified.  Sets
`vm-presentation-buffer' and makes it the current buffer."
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
  "Replace the current PGP armor with a button labeled LABEL bound to ACTION.
ACTION is an interactive function, invoked by RET or a middle click on it.
The armor to replace is the one just matched by
`vm-epg-cleartext-begin-regexp'; this uses that match data to find the
extent of the armor, so call it directly after the search."
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
  "The message whose inline PGP armor has already been handled, or nil.
Holds a VM message object, not a state symbol.  `vm-epg-cleartext-automode'
consults it so that re-displaying the same message does not verify or
decrypt its armor a second time.")
(make-variable-buffer-local 'vm-epg-cleartext-decoded)

(defun vm-epg-set-cleartext-decoded ()
  "Record the current message in `vm-epg-cleartext-decoded'.
Marks its inline PGP armor as handled, so that re-displaying the message
does not verify or decrypt it again."
  (save-excursion
    (vm-select-folder-buffer)
    (setq vm-epg-cleartext-decoded (car vm-message-pointer))))

;; Note: this test is deliberately looser than `vm-mime-plain-message-p',
;; whose charset and encoded-header restrictions are irrelevant to inline
;; PGP.  The armor lines are 7-bit ASCII regardless of the part's charset,
;; encoded *headers* never affect the *body*, and verification runs on the
;; transfer-decoded body before any charset conversion.  Requiring
;; `us-ascii' and unencoded headers merely suppressed verification for
;; perfectly valid messages, such as an `iso-8859-1' body with RFC 2047
;; headers.

(defun vm-epg-cleartext-candidate-p (m)
  "Return non-nil if message M may carry inline (cleartext) PGP armor.
True when M has no MIME layout, or its top-level part is `text/plain'.

The `text/plain' restriction is not cosmetic: the verify/cleanup/display
path is wired only to that subtype, via the advice on
`vm-mime-display-internal-text/plain' and `vm-epg-cleartext-cleanup'."
  (save-match-data
    (let ((o (vm-mm-layout m))
          (case-fold-search t))
      (or (not (vectorp o))
          (vm-mime-types-match "text/plain" (car (vm-mm-layout-type o)))))))

(defun vm-epg-cleartext-automode ()
  "Check for inline PGP ASCII armor and act on it.
Search the first `vm-epg-cleartext-search-limit' bytes of the message being
displayed for an armor recognized by `vm-epg-cleartext-begin-regexp', then
verify a signature, decrypt a message, or import a public key block as
appropriate.  Decryption and key import are performed directly only when
`vm-epg-auto-decrypt' resp. `vm-epg-auto-snarf' is non-nil; otherwise a
button is inserted to do it on demand.  Do nothing if this message's armor
has already been handled, per `vm-epg-cleartext-decoded'."
  (save-excursion
    (vm-select-folder-buffer-if-possible)
    (if (equal vm-epg-cleartext-decoded (car vm-message-pointer))
        (setq vm-epg-cleartext-decoded nil)
      (setq vm-epg-cleartext-decoded nil)
      (if vm-presentation-buffer
          (set-buffer vm-presentation-buffer))
      (goto-char (point-min))
      (when (and (vm-epg-cleartext-candidate-p (car vm-message-pointer))
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
               ;; Unreachable unless `vm-epg-cleartext-begin-regexp' gains an
               ;; alternative in group 1 that is not handled above.
               (error "Unhandled PGP armor type %S"
                      (match-string 1))))))))

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
  "Decode or check signature on clear text messages when scrolling.
Around advice for `vm-scroll-forward': apply ORIG-FUN to ARGS, then run
`vm-epg-cleartext-automode' if the scroll ended the preview of a message,
which is the point at which its body first becomes available."
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
;; The cleartext verify/decrypt commands both compute a result -- the human
;; readable OUTPUT text to show and the FACE to show it in -- and then hand
;; that result to `vm-epg-cleartext-cleanup', which strips the ASCII armor and
;; inserts the output in its place.
;;
;; This result cannot be returned normally to the display advice: the two are
;; separated by VM's own `vm-mime-display-internal-text/plain' and
;; `vm-mime-transfer-decode-region', whose return values we do not control.
;; The single dynamic variable `vm-epg-cleartext-result' bridges that gap: the
;; display advice binds it, the verify/decrypt commands running underneath
;; store their result into it, and the advice applies the result after the
;; original display function returns.  See `vm-epg-cleartext-set-result'.

(defun vm-epg-cleartext-cleanup (status &optional output face)
  "Replace the inline PGP armor at point with the outcome of an EPG operation.
STATUS is `verified' or `error'.  OUTPUT is the human readable text to insert
in place of the armor, or nil for the empty string.  FACE is the face to
apply to that text; when nil it is derived from STATUS, giving
`vm-epg-bad-signature' for `error' and `vm-epg-good-signature' otherwise."
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
      (insert (or output ""))
      (vm-epg-crlf-cleanup start (point))
      (setq end (point))
      (put-text-property start end 'face
                         (or face
                             (if (eq status 'error)
                                 'vm-epg-bad-signature
                               'vm-epg-good-signature))))))

(advice-add 'vm-mime-transfer-decode-region
            :around #'vm-epg--transfer-cleartext-automode)
(defun vm-epg--transfer-cleartext-automode (orig-fun &optional layout
                                                      start end &rest args)
  "Decode or check signature on clear text message parts.
Around advice for `vm-mime-transfer-decode-region': apply ORIG-FUN to
LAYOUT, START, END and ARGS, then run `vm-epg-cleartext-automode' over the
freshly transfer-decoded region \[START, END], where the body is decoded but
not yet charset-converted -- the form the cleartext signature is computed
over.

The region is taken from the decode arguments rather than from how far point
moved: transfer-decoding advances point only for encodings that actually
transform the text (base64, quoted-printable, uuencode).  A 7bit or 8bit part
is left untouched, so a point-motion test would wrongly skip exactly the
plain PGP-signed messages this is meant to handle."
  (apply orig-fun layout start end args)
  (when (and (vm-mime-text-type-layout-p layout)
             start end (< start end))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (vm-epg-cleartext-automode)
        (widen)))))

(defvar vm-epg-cleartext-result 'none
  "Result of a cleartext verify/decrypt run under the display advice.
`vm-epg--display-cleartext-automode' binds this to nil around the display of
a text part; a verify/decrypt command running underneath then stores its
result here via `vm-epg-cleartext-set-result', and the advice applies it
afterwards.  Values:
  none  -- not running under the display advice (the global default);
  nil   -- under the advice, but no cleartext result was produced;
  plist -- a result, with keys :status, :output and :face.")

(defun vm-epg-cleartext-set-result (status output face)
  "Record the cleartext STATUS, OUTPUT and FACE for the display advice.
Return non-nil when running under `vm-epg--display-cleartext-automode' (which
will do the cleanup); return nil otherwise, so the caller knows it must run
`vm-epg-cleartext-cleanup' itself."
  (unless (eq vm-epg-cleartext-result 'none)
    (setq vm-epg-cleartext-result
          (list :status status :output output :face face))
    t))

(advice-add 'vm-mime-display-internal-text/plain
            :around #'vm-epg--display-cleartext-automode)
(defun vm-epg--display-cleartext-automode (orig-fun &rest args)
  "Decode or check signature on clear text message parts.
Around advice for `vm-mime-display-internal-text/plain': apply ORIG-FUN to
ARGS, then apply any result a verify/decrypt command left in
`vm-epg-cleartext-result'.

The cleanup has to happen here, after ORIG-FUN returns, rather than in the
verify/decrypt command itself: the faces applied to the inserted output
would be lost to the charset conversion that ORIG-FUN performs afterwards."
  (let ((vm-epg-cleartext-result nil)
        (start (point))
        end)
    (let ((ret (apply orig-fun args)))
      (when vm-epg-cleartext-result
        (setq end (point))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (vm-epg-cleartext-cleanup
           (plist-get vm-epg-cleartext-result :status)
           (plist-get vm-epg-cleartext-result :output)
           (plist-get vm-epg-cleartext-result :face))
          (widen)))
      ret)))

;;; Cleartext sign/encrypt/verify/decrypt

;;;###autoload
(defun vm-epg-cleartext-encrypt (sign)
  "Encrypt the composition body in place as inline PGP ASCII armor.
With a prefix argument, SIGN non-nil, sign it as well.

This replaces the body with the armor rather than building a MIME structure
around it, so it cannot cover attachments; prefer `vm-epg-encrypt', which
produces PGP/MIME.  Also used internally by `vm-epg-encrypt' to armor the
body it then wraps.

Every recipient must have a usable encryption key: with no recipient key
this signals an error rather than falling back to symmetric (passphrase)
encryption, which is never what is wanted for mail."
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
  "Sign the composition body in place as inline PGP ASCII armor.
This uses the OpenPGP cleartext signature framework, which leaves the text
readable and appends the signature to the body, rather than building a
multipart/signed MIME structure around it.  It therefore cannot cover
attachments; prefer `vm-epg-sign', which produces PGP/MIME."
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
  "Return a human readable description of EPG verification RESULT.
RESULT is a list of `epg-signature' objects, as returned by
`epg-context-result-for' for the `verify' operation; one line is produced
per signature.  A nil RESULT means EPG reported no signature at all."
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

(defun vm-epg-fetch-missing-keys-maybe (context result)
  "Fetch into CONTEXT any public keys that RESULT reports as missing.
RESULT is a list of `epg-signature' objects.  When
`vm-epg-fetch-missing-keys' is non-nil and one or more signatures were made
by a key that is not in the local keyring (status `no-pubkey'), attempt to
receive those keys from a keyserver.  Return non-nil if any keys were
fetched, so the caller can verify the message again.

Note that this contacts the network, so despite returning a boolean it is
not a cheap predicate."
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
  "Verify the inline PGP signature in the current message.
Replace the ASCII armor in a presentation copy of the message with a
description of the signature, faced according to whether it verified, and
report the outcome in the modeline.  The folder itself is not modified.

If the signing key is not in your keyring and `vm-epg-fetch-missing-keys'
is non-nil, try to fetch it from a keyserver first."
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
      (when (vm-epg-fetch-missing-keys-maybe context result)
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
      (let ((output (vm-epg-format-verify-result result))
            (face (if (eq status 'verified)
                      'vm-epg-good-signature
                    'vm-epg-bad-signature)))
        ;; When running under the display advice, hand it the result to apply
        ;; later; otherwise strip the armor and insert the output right here.
        (unless (vm-epg-cleartext-set-result status output face)
          (vm-epg-cleartext-cleanup status output face))))))

;;;###autoload
(defun vm-epg-cleartext-decrypt ()
  "Decrypt the inline PGP message in the current message.
Replace the ASCII armor in a presentation copy with the plaintext, leaving
the folder unmodified, and report the outcome in the modeline.  A decryption
failure inserts the error text instead, faced with `vm-epg-error'.

If the plaintext is itself an inline signed message, verify it as well with
`vm-epg-cleartext-verify'.

Refuses to run on a read-only folder even though only the presentation copy
is written, unlike `vm-epg-cleartext-verify', which does not check."
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
  "Convert CRLF line endings to LF between START and END.
Used on text coming back from EPG, which is in the MIME canonical CRLF form,
before it is shown in a buffer."
  (save-excursion
    (goto-char start)
    (while (search-forward "\r\n" end t)
      (replace-match "\n" t t))))

(defun vm-epg-make-crlf (start end)
  "Convert LF line endings to CRLF between START and END.
Used to put a body into the MIME canonical form that RFC 3156 requires a
signature to be computed over.  Works backwards from END so that the growing
text does not invalidate the region."
  (save-excursion
    (goto-char end)
    (while (search-backward "\n" start t)
      (replace-match "\r\n" t t)
      (backward-char))))

;;; MIME state tracking

(defvar vm-epg-mime-decoded nil
  "VM's `vm-mime-decoded' as it was when the current decode started.
Captured by `vm-epg--clear-state' because the MIME handlers here need to
know whether VM is decoding the message for the first time or toggling an
already-decoded message back to buttons; by the time a handler runs,
`vm-mime-decoded' has already been updated.  Read it with
`vm-epg-get-mime-decoded', which looks in the folder buffer.")
(make-variable-buffer-local 'vm-epg-mime-decoded)

(defun vm-epg-get-mime-decoded ()
  "Return `vm-epg-mime-decoded' from the folder buffer.
The MIME handlers run in the presentation buffer, where the variable is not
the one that `vm-epg--clear-state' set."
  (save-excursion
    (vm-select-folder-buffer)
    vm-epg-mime-decoded))

(defvar vm-epg-recursion nil
  "Non-nil while `vm-epg--clear-state' is inside `vm-decode-mime-message'.
The advice must capture `vm-mime-decoded' only for the outermost decode; a
nested decode -- one started by a handler defined here, for instance to
render decrypted content -- would otherwise overwrite it.")

(advice-add 'vm-decode-mime-message :around #'vm-epg--clear-state)
(defun vm-epg--clear-state (orig-fun &rest args)
  "Clear the modeline state before decoding.
Around advice for `vm-decode-mime-message': reset `vm-epg-state' for the new
message and remember `vm-mime-decoded' in `vm-epg-mime-decoded', then apply
ORIG-FUN to ARGS.  A plain (non-MIME) message is re-presented instead of
decoded, so that inline PGP armor gets another chance to be handled."
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
  "Decrypt the MIME part associated with BUTTON, replacing the button.
The action of the button inserted by
`vm-mime-display-internal-multipart/encrypted' when `vm-epg-auto-decrypt'
is nil."
  (let ((vm-epg-auto-decrypt t)
        (layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    (goto-char (vm-extent-start-position button))
    (let ((buffer-read-only nil))
      (vm-decode-mime-layout button t))))

;;;###autoload
(defun vm-mime-display-internal-multipart/encrypted (layout)
  "Display the PGP/MIME multipart/encrypted part LAYOUT, decrypting it.
Insert the decrypted content at point, parsed and displayed as MIME in its
own right, and report the outcome in the modeline.  If the plaintext also
carried a signature, verify that and report it too.  When
`vm-epg-auto-decrypt' is nil, insert a button that decrypts on demand
instead.

Always return t, so that VM treats the part as handled.  Returning nil on a
decrypt failure or an unrecognized structure would make VM fall through and
re-render the raw ciphertext parts as multipart/mixed.

This is VM's dispatch name for the content type, so it deliberately does not
carry the `vm-epg-' prefix.  Note that vm-pgg defines a function of the same
name; see the commentary at the top of this file."
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
                             (insert "\n"
                                     (vm-epg-format-verify-result verify-result)
                                     "\n")
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
  "Display the PGP/MIME multipart/signed part LAYOUT, verifying its signature.
Insert the signed content at point, followed by a description of the
signature faced according to whether it verified, and report the outcome in
the modeline.  Per RFC 3156 the signature is checked against the CRLF form
of the signed part, headers included.  A signature part of any type other
than application/pgp-signature is reported as an unknown signature type and
the content is still shown.

If the signing key is not in your keyring and `vm-epg-fetch-missing-keys' is
non-nil, try to fetch it from a keyserver and verify again.

This is VM's dispatch name for the content type, so it deliberately does not
carry the `vm-epg-' prefix.  Note that vm-pgg defines a function of the same
name; see the commentary at the top of this file."
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
                 (when (vm-epg-fetch-missing-keys-maybe context status)
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

;; Register the PGP content types with VM's MIME machinery.  This runs at
;; compile time as well as at load time (`eval-and-compile') so that the
;; button formats are in place while the rest of this file is byte-compiled;
;; the effect on the compiling Emacs is harmless and is discarded with it.
;;
;; `vm-mime-internal-content-types' is only extended when it is a list: the
;; value t means "display every type internally", which already covers
;; application/pgp-keys and must not be turned into a list.  Note that a
;; user who sets this variable *after* loading vm-epg discards this entry;
;; see the commentary at the top of this file.
(eval-and-compile
  (if (listp vm-mime-internal-content-types)
      (add-to-list 'vm-mime-internal-content-types "application/pgp-keys"))
  (add-to-list 'vm-mime-button-format-alist
               '("application/pgp-keys" . "Snarf %d"))
  (add-to-list 'vm-mime-button-format-alist
               '("multipart/encrypted" . "Decrypt PGP/MIME message")))

(defun vm-epg-format-import-result (result)
  "Return a message describing the outcome of a key import RESULT.
RESULT is an `epg-import-result' object, or nil if EPG reported none.

Reports the number of keys actually added to the keyring
\(`epg-import-result-imported'), not the number considered: importing a key
you already have considers it but imports nothing, so a `considered' count
would claim an import that did not happen."
  (format "Imported %d key(s)."
          (if result (epg-import-result-imported result) 0)))

(defun vm-epg-mime-snarf-keys (button)
  "Import the keys from the MIME part associated with BUTTON.
The action of the button inserted by
`vm-mime-display-internal-application/pgp-keys' when `vm-epg-auto-snarf' is
nil."
  (let ((vm-epg-auto-snarf t)
        (layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    (goto-char (vm-extent-start-position button))
    (let ((buffer-read-only nil))
      (vm-decode-mime-layout button t))))

;;;###autoload
(defun vm-mime-display-internal-application/pgp-keys (layout)
  "Import the public keys in the application/pgp-keys part LAYOUT.
Replace the part with a report of how many keys were added to your keyring.
When `vm-epg-auto-snarf' is nil, insert a button that imports on demand
instead.

This is VM's dispatch name for the content type, so it deliberately does not
carry the `vm-epg-' prefix.  Note that vm-pgg defines a function of the same
name; see the commentary at the top of this file."
  (vm-epg-state-set 'public-key)
  (if vm-epg-auto-snarf
      (let ((start (point)) end)
        (vm-mime-insert-mime-body layout)
        (setq end (point-marker))
        (vm-mime-transfer-decode-region layout start end)
        (let* ((key-text (buffer-substring-no-properties start end))
               (context (epg-make-context 'OpenPGP))
               import-result)
          (delete-region start end)
          (condition-case err
              (progn
                (epg-import-keys-from-string context key-text)
                (setq import-result (epg-context-result-for context 'import))
                (insert (vm-epg-format-import-result import-result) "\n"))
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
  "Import into your keyring the public keys in the body of the current message.
This treats the whole message body as key material, so it is meant for
messages that are an inline PGP public key block.  Keys arriving as an
application/pgp-keys MIME part are handled by
`vm-mime-display-internal-application/pgp-keys' instead."
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
    (let* ((key-text (buffer-substring-no-properties (point) (point-max)))
           (context (epg-make-context 'OpenPGP)))
      (condition-case err
          (progn
            (epg-import-keys-from-string context key-text)
            (message "%s" (vm-epg-format-import-result
                           (epg-context-result-for context 'import))))
        (error
         (error "Snarfing failed: %s" (error-message-string err)))))))

;;; Public key attachment

;;;###autoload
(defun vm-epg-attach-public-key ()
  "Attach a public key to the composition as an application/pgp-keys part.
The key exported is the author's, taken from the headers listed in
`vm-epg-get-author-headers'.  When that yields no address, or the variable
is nil, prompt for a user ID -- so any key in your keyring can be sent, not
only your own."
  (interactive)
  (let* ((author (or (and vm-epg-get-author-headers (vm-epg-get-author))
                     (read-string "User ID: ")))
         (context (epg-make-context 'OpenPGP))
         (keys (epg-list-keys context author))
         (description (concat "public key of " author))
         (buffer (get-buffer-create (concat " *" description "*")))
         start)
    (unless keys
      (error "%s has no public key" author))
    (with-current-buffer buffer
      (erase-buffer)
      (setq start (point))
      (insert (epg-export-keys-to-string context keys))
      (when (= start (point))
        (error "%s has no public key" author)))
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
  "Insert a public key as ASCII armor into the composition at point.
The key is selected as for `vm-epg-attach-public-key', but is inserted
inline rather than attached as a MIME part."
  (interactive)
  (let* ((author (or (and vm-epg-get-author-headers (vm-epg-get-author))
                     (read-string "User ID: ")))
         (context (epg-make-context 'OpenPGP))
         (keys (epg-list-keys context author)))
    (unless keys
      (error "%s has no public key" author))
    (insert (epg-export-keys-to-string context keys))))

;;; MIME multipart boundary

(defun vm-epg-make-multipart-boundary (word)
  "Return a MIME multipart boundary string beginning with WORD.
WORD, if non-nil, is followed by \"+\" and 15 random base64 characters; a nil
WORD gives just the 15 random characters."
  (if word (setq word (concat word "+")))
  (let ((boundary (concat word (make-string 15 ?a)))
        (i (length word)))
    (while (< i (length boundary))
      (aset boundary i (aref vm-mime-base64-alphabet
                             (random (length vm-mime-base64-alphabet))))
      (vm-increment i))
    boundary))

(defun vm-epg-save-work (function &rest args)
  "Apply FUNCTION to ARGS on a scratch copy of the composition buffer.
The current buffer's text is copied into a work buffer, FUNCTION is applied
there, and the result is copied back over the composition only once FUNCTION
has returned normally.

If FUNCTION signals, the error propagates and the composition is left
exactly as it was.  This matters because a failed sign or encrypt otherwise
tends to leave a half-rewritten message -- headers already replaced, body
not yet armored -- that the user cannot easily repair.

The work buffer is killed on the way out, except when the failure struck
while the composition was being overwritten: it then holds the only copy of
FUNCTION's result, so it is left behind for the user to recover from."
  (let ((composition-buffer (current-buffer))
        (work-buffer (get-buffer-create " *VM-EPG-WORK*"))
        (overwriting nil))
    (unwind-protect
        (progn
          (with-current-buffer work-buffer
            (buffer-disable-undo)
            (erase-buffer)
            (insert-buffer-substring composition-buffer)
            (setq major-mode 'mail-mode)
            (apply function args))
          (vm-mail-mode-show-headers)
          ;; Past this point the composition no longer holds a usable copy,
          ;; so an error or a C-g must not take the work buffer with it.
          (setq overwriting t)
          (erase-buffer)
          (insert-buffer-substring work-buffer)
          (setq overwriting nil))
      (unless overwriting
        (kill-buffer work-buffer)))))

;;; Digest algorithm name for micalg header

(defun vm-epg-digest-algo-name (algo-id)
  "Return the lowercase name of the digest algorithm with id ALGO-ID.
ALGO-ID is looked up in `epg-digest-algorithm-alist'; the name is used for
the `micalg' parameter of a multipart/signed Content-Type, which RFC 3156
specifies in lowercase.  Falls back to \"sha256\" for an unknown id."
  (let ((entry (assq algo-id epg-digest-algorithm-alist)))
    (if entry
        (downcase (cdr entry))
      "sha256")))

;;; Sign composition

;;;###autoload
(defun vm-epg-sign ()
  "Sign the composition with PGP/MIME, as a multipart/signed message.

RFC 3156 forbids 8bit content transfer encoding in signed messages, and
lines beginning with \"From \" must be armored, because a mail gateway that
re-encodes either one would invalidate the signature.

If the composition is not yet MIME-encoded, this encodes it in a
signature-safe way, using `vm-epg-sign-text-transfer-encoding' in place of
`vm-mime-8bit-text-transfer-encoding' and forcing
`vm-mime-composition-armor-from-lines' on; your normal settings for those
two variables therefore do not matter here.

If the composition has *already* been MIME-encoded -- for instance because
you encoded it yourself with `vm-mime-encode-composition' -- this cannot
re-encode it safely, so it checks for the two hazards above and refuses to
sign rather than produce a signature that breaks in transit."
  (interactive)

  (when (vm-mail-mode-get-header-contents "MIME-Version:")
    (goto-char (point-min))
    (when (re-search-forward "Content-Transfer-Encoding:\\s-*8bit" nil t)
      (describe-function 'vm-epg-sign)
      (error "Cannot sign: composition is already encoded as 8bit"))
    (goto-char (point-min))
    (when (re-search-forward "^From\\s-+" nil t)
      (describe-function 'vm-epg-sign)
      (error "Cannot sign: unarmored line starting with \"From \" in body")))

  (vm-epg-save-work 'vm-epg-sign-internal))

(defun vm-epg-sign-internal ()
  "Rewrite the current buffer as a PGP/MIME multipart/signed message.
Intended to be run by `vm-epg-save-work' on a scratch copy, not called
directly; `vm-epg-sign' is the command.  The `micalg' parameter of the
resulting Content-Type is taken from the digest algorithm EPG actually
used, rather than assumed."
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
        (insert "This is an OpenPGP/MIME signed message (RFC 4880 and 3156)\n")
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
  "Encrypt the composition as PGP/MIME, a multipart/encrypted message.
With a prefix argument, SIGN non-nil, sign it as well, which is what
`vm-epg-sign-and-encrypt' does.

Every recipient address found in the headers listed in
`vm-epg-get-recipients-headers' must have a usable encryption key in your
keyring; otherwise this signals an error and leaves the composition
untouched.  Note that the message is never encrypted to a passphrase: if no
recipient key can be found, it refuses rather than falling back to symmetric
encryption."
  (interactive "P")
  (vm-epg-save-work 'vm-epg-encrypt-internal sign))

(defun vm-epg-encrypt-internal (sign)
  "Rewrite the current buffer as a PGP/MIME message, signing it if SIGN.
Intended to be run by `vm-epg-save-work' on a scratch copy, not called
directly; `vm-epg-encrypt' is the command."
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
    (insert "This is an OpenPGP/MIME encrypted message (RFC 4880 and 3156)\n")
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

;;;###autoload
(defun vm-epg-sign-and-encrypt ()
  "Sign and encrypt the composition as PGP/MIME.
Equivalent to `vm-epg-encrypt' with a prefix argument."
  (interactive)
  (vm-epg-encrypt t))

;;; Ask hook

(defvar vm-epg-prompt-last-action nil
  "The action last chosen in `vm-epg-prompt-for-action'.
It is offered as the default on the next prompt, selected by RET.")

(defvar vm-epg-prompt-action-alist
  '((?s sign "sign")
    (?e encrypt "encrypt")
    (?E sign-and-encrypt "both")
    (?n nil "nothing")
    (?q quit "quit"))
  "Alist of (KEY ACTION LABEL) elements for `vm-epg-prompt-for-action'.
KEY is the character that selects the entry, ACTION the symbol returned for
it, and LABEL the word shown in the prompt.  ACTION nil means take no
action; the pseudo-action `quit' aborts sending.  Any other ACTION selects
the command `vm-epg-ACTION'.")

(defun vm-epg-prompt-for-action ()
  "Prompt for a PGP action and return it.
The choices come from `vm-epg-prompt-action-alist'; RET repeats the previous
choice, and `q' aborts sending with an error."
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
      (error "Sending aborted"))
    (if action
        (message "Action is %s." action)
      (message "No action selected."))
    (setq vm-epg-prompt-last-action action)
    action))

;;;###autoload
(defun vm-epg-ask-hook ()
  "Ask whether to sign or encrypt outgoing messages with PGP/MIME.

Add to `vm-mail-send-hook' to be asked each time you send a message.
`vm-epg-ask-function' controls what is asked: by default
`vm-epg-prompt-for-action' offers a choice of actions, but it can also name
a single action to confirm, or your own function.

This hook must be last in `vm-mail-send-hook', and signals an error if it is
not: signing covers the message as it stands, so a later hook that modified
the message would invalidate the signature.  Add it with the APPEND argument
to `add-hook':

       (add-hook \\='vm-mail-send-hook #\\='vm-epg-ask-hook t)"
  (interactive)

  ;; ensure we are the last hook
  (when (and (member 'vm-epg-ask-hook vm-mail-send-hook)
             (cdr (member 'vm-epg-ask-hook vm-mail-send-hook)))
    (describe-function 'vm-epg-ask-hook)
    (error "`vm-epg-ask-hook' must be the last hook in `vm-mail-send-hook'"))

  (let ((handler vm-epg-ask-function)
        action)
    (when handler
      ;; `functionp', not `fboundp': the value may be a lambda or closure
      ;; rather than a symbol, and `fboundp' signals on those.  An action
      ;; symbol such as `sign' names no function, so it takes the other
      ;; branch and is merely confirmed.
      (setq action (if (functionp handler)
                       (funcall handler)
                     (if (y-or-n-p (format "%s the composition? " handler))
                         handler)))
      (when action
        (let ((command (intern (format "vm-epg-%s" action))))
          ;; Report a bad `vm-epg-ask-function' value against that variable,
          ;; rather than letting a void-function error surface at send time.
          (unless (fboundp command)
            (error "Invalid action `%s' from `vm-epg-ask-function': no %s"
                   action command))
          (funcall command))))))

(provide 'vm-epg)

;;; vm-epg.el ends here
