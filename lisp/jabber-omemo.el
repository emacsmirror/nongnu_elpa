;;; jabber-omemo.el --- OMEMO encryption for jabber.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is part of emacs-jabber.

;; emacs-jabber is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; emacs-jabber is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs-jabber.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Public Elisp API for OMEMO 0.3 (eu.siacs.conversations.axolotl).
;; Wraps the jabber-omemo-core dynamic module (picomemo).
;;
;; This file handles loading the native module, building it on demand
;; if missing, and re-exports the core functions under the public
;; jabber-omemo- namespace.

;;; Code:

(require 'cl-lib)

(unless module-file-suffix
  (error "jabber-omemo requires Emacs compiled with dynamic module support"))

(defvar jabber-omemo-build-command "make -C src"
  "Shell command to build the jabber-omemo-core dynamic module.
Run from the emacs-jabber project root.")

(cl-eval-when (load eval)
  (unless (require 'jabber-omemo-core nil t)
    (let ((src-dir (expand-file-name
                    "../src"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))))
      (if (and (file-exists-p (expand-file-name "jabber-omemo-core.c" src-dir))
               (or noninteractive
                   (yes-or-no-p
                    "jabber-omemo-core module not found.  Build it now? ")))
          (let ((default-directory
                 (file-name-directory
                  (directory-file-name src-dir))))
            (message "Building jabber-omemo-core module...")
            (unless (zerop (call-process-shell-command
                            jabber-omemo-build-command nil
                            (get-buffer-create "*jabber-omemo-build*") t))
              (pop-to-buffer "*jabber-omemo-build*")
              (error "Failed to build jabber-omemo-core module"))
            (require 'jabber-omemo-core))
        (user-error "OMEMO support requires the jabber-omemo-core native module")))))

;; Declare internal C functions for the byte-compiler.
(declare-function jabber-omemo--setup-store "jabber-omemo-core")
(declare-function jabber-omemo--deserialize-store "jabber-omemo-core")
(declare-function jabber-omemo--serialize-store "jabber-omemo-core")
(declare-function jabber-omemo--get-bundle "jabber-omemo-core")
(declare-function jabber-omemo--rotate-signed-pre-key "jabber-omemo-core")
(declare-function jabber-omemo--refill-pre-keys "jabber-omemo-core")
(declare-function jabber-omemo--encrypt-message "jabber-omemo-core")
(declare-function jabber-omemo--decrypt-message "jabber-omemo-core")
(declare-function jabber-omemo--make-session "jabber-omemo-core")
(declare-function jabber-omemo--initiate-session "jabber-omemo-core")
(declare-function jabber-omemo--serialize-session "jabber-omemo-core")
(declare-function jabber-omemo--deserialize-session "jabber-omemo-core")
(declare-function jabber-omemo--encrypt-key "jabber-omemo-core")
(declare-function jabber-omemo--decrypt-key "jabber-omemo-core")
(declare-function jabber-omemo--heartbeat "jabber-omemo-core")

;; Public API

(defun jabber-omemo-setup-store ()
  "Generate a new OMEMO device store.
Returns a serialized store as a unibyte string."
  (jabber-omemo--setup-store))

(defun jabber-omemo-deserialize-store (blob)
  "Deserialize BLOB into an OMEMO store object.
Returns a user-ptr; freed automatically by GC."
  (jabber-omemo--deserialize-store blob))

(defun jabber-omemo-serialize-store (store-ptr)
  "Serialize STORE-PTR back to a unibyte string."
  (jabber-omemo--serialize-store store-ptr))

(defun jabber-omemo-get-bundle (store-ptr)
  "Extract the public bundle from STORE-PTR.
Returns a plist with keys :identity-key, :signed-pre-key,
:signed-pre-key-id, :signature, :pre-keys."
  (jabber-omemo--get-bundle store-ptr))

(defun jabber-omemo-rotate-signed-pre-key (store-ptr)
  "Rotate the signed pre-key in STORE-PTR.
Mutates the store; caller must re-serialize."
  (jabber-omemo--rotate-signed-pre-key store-ptr))

(defun jabber-omemo-refill-pre-keys (store-ptr)
  "Refill removed pre-keys in STORE-PTR.
Mutates the store; caller must re-serialize."
  (jabber-omemo--refill-pre-keys store-ptr))

(defun jabber-omemo-encrypt-message (plaintext)
  "Encrypt PLAINTEXT (a unibyte string) with OMEMO 0.3.
Returns a plist (:key KEY :iv IV :ciphertext CT),
all unibyte strings."
  (jabber-omemo--encrypt-message plaintext))

(defun jabber-omemo-decrypt-message (key iv ciphertext)
  "Decrypt an OMEMO 0.3 message.
KEY is a unibyte string (>= 32 bytes: 16 AES key + auth tag).
IV is a 12-byte unibyte string.
CIPHERTEXT is the encrypted payload.
Returns the plaintext as a unibyte string."
  (jabber-omemo--decrypt-message key iv ciphertext))

(defun jabber-omemo-make-session ()
  "Allocate an empty OMEMO session.
Returns a session user-ptr; freed automatically by GC.
Use for the receiving side of a pre-key message."
  (jabber-omemo--make-session))

(defun jabber-omemo-initiate-session (store-ptr sig spk ik pk spk-id pk-id)
  "Initiate an OMEMO session with a remote device's bundle.
STORE-PTR is the local OMEMO store.
SIG is a 64-byte signature, SPK/IK/PK are 33-byte serialized keys.
SPK-ID and PK-ID are integer key IDs.
Returns a session user-ptr; freed automatically by GC."
  (jabber-omemo--initiate-session store-ptr sig spk ik pk spk-id pk-id))

(defun jabber-omemo-serialize-session (session-ptr)
  "Serialize SESSION-PTR to a unibyte string."
  (jabber-omemo--serialize-session session-ptr))

(defun jabber-omemo-deserialize-session (blob)
  "Deserialize BLOB into an OMEMO session object.
Returns a session user-ptr; freed automatically by GC."
  (jabber-omemo--deserialize-session blob))

(defun jabber-omemo-encrypt-key (session-ptr key)
  "Encrypt KEY for a recipient using SESSION-PTR.
KEY is a unibyte string (the message encryption key).
Returns a plist (:data BYTES :pre-key-p BOOL)."
  (jabber-omemo--encrypt-key session-ptr key))

(defun jabber-omemo-decrypt-key (session-ptr store-ptr pre-key-p msg)
  "Decrypt an encrypted key message.
SESSION-PTR is the session with the sender.
STORE-PTR is the local OMEMO store.
PRE-KEY-P is non-nil if this is a pre-key message.
MSG is the encrypted key message as a unibyte string.
Returns the decrypted key as a unibyte string."
  (jabber-omemo--decrypt-key session-ptr store-ptr pre-key-p msg))

(defun jabber-omemo-heartbeat (session-ptr store-ptr)
  "Check if a heartbeat message is needed after decryption.
SESSION-PTR is the session to check.
STORE-PTR is the local OMEMO store.
Returns heartbeat message bytes or nil."
  (jabber-omemo--heartbeat session-ptr store-ptr))

(provide 'jabber-omemo)
;;; jabber-omemo.el ends here
