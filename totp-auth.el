;;; totp-auth.el --- RFC6238 TOTP -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022,2023 Vivek Das Mohapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Vivek Das Mohapatra <vivek@etla.org>
;; Keywords: 2FA two-factor totp otp password comm
;; URL: https://gitlab.com/fledermaus/totp.el
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (base32 "0.1"))

;;; Commentary:
;; totp-auth.el - Time-based One Time Password support for Emacs
;;
;; This package generates RFC6238 Time-based One Time Passwords
;; (in other words, what Google Authenticator implements)
;; and displays them (as well as optionally copying them to
;; the clipboard/primary selection), updating them as they expire.
;;
;; It retrieves the shared secrets used to generate TOTP tokens
;; with ‘auth-sources’ and/or the freedesktop secrets API (aka
;; Gnome Keyring or KWallet).
;;
;; You can call it with the command ‘totp-auth’, ie:
;;
;;    M-x totp-auth RET
;;
;; You can tab-complete based on the label of the secret.
;; Depending on the setting of ‘totp-auth-display-token-method’ the
;; TOTP token will be displayed (and kept up to date) either in
;; an Emacs buffer or a freedesktop notification.
;;
;; If you want to import TOTP secrets from other apps you can call:
;;
;;   M-x totp-auth-import-file RET
;;
;; If you want the latest generated token automatically
;; copied to your GUI's selection for easy pasting, you
;; can customize ‘totp-auth-auto-copy-password’.

;;; Code:
(eval-and-compile
  (let ((load-path load-path)
        (this-file (or load-file-name
                       byte-compile-current-file
                       buffer-file-name)))
    (when (not (and (locate-library "base32")
                    (locate-library "hmac")))
      (add-to-list 'load-path (file-name-directory this-file)))
    (require 'base32)
    (require 'totp-auth-hmac))
  ;; this is to reduce warnings for melpa - it's not actually necessary
  (ignore-errors (require 'notifications))
  (require 'auth-source)
  (require 'secrets)
  (require 'bindat)
  (require 'url-parse)
  (require 'url-util)
  (require 'mailcap))

(defgroup totp-auth nil "Time-based One Time Passwords."
  :prefix "totp"
  :group 'data)

(defconst totp-auth-xdg-schema "org.freedesktop.Secret.TOTP")

(defcustom totp-auth-alt-xdg-schemas
  '("com.github.bilelmoussaoui.Authenticator")
  "A list of fallback XDG schemas which are associated with TOTP secrets.
This is used only to read TOTP secrets stored by other applications."
  :type '(repeat string)
  :group 'totp-auth)

(defcustom totp-auth-minimum-ui-grace 3
  "The minimum time to expiry a TOTP must have for interactive use.
If the generated token has less then this much time to live then
interactive code MAY instead generate the next TOTP in sequence
and wait until it is valid before giving it to the user.
Noninteractive TOTP code MUST return TOTP values along with their
lifespan (at the time of generation) and their absolute expiry time."
  :type  'integer
  :group 'totp)

(defcustom totp-auth-max-tokens 1024
  "The maximum number of tokens totp will try to fetch and process."
  :group 'totp-auth
  :type  'integer)

(defcustom totp-auth-file-import-command '("zbarimg" "-q" "@file@")
  "The command and parameters used to parse a QR code image.
@file@ is a placeholder for the file name."
  :group 'totp-auth
  :type  '(repeat string))

(defcustom totp-auth-file-export-command
  '("qrencode" "-l" "M" "@type@" "-o" "@file@")
  "The command and parameters used to convert a data stream  to a QR code.
@file@ is a placeholder for the target filename.
@type@ is a placeholder for a supported output type and will be determined by
‘totp-auth-file-export-type-map’."
  :group 'totp-auth
  :type  '(repeat string))

(defcustom totp-auth-file-export-type-map '((png  "-t" "PNG")
                                            (svg  "-t" "SVG")
                                            (eps  "-t" "EPS"))
  "A mapping from image type (as per function ‘image-type’) to export argument.
Defaults to a map usable by qrencode (see ‘totp-auth-file-export-command’).
May also be a function, which should take one argument (the image type symbol)
and return a list of arguments to pass to the QR encoder."
  :group 'totp-auth
  :type  '(choice (alist :tag "Fixed type map"
                         :key-type symbol
                         :value-type (repeat string))
                  (function :tag "Function (func (image-type-symbol) …)")))

(defcustom totp-auth-export-url-max-size 1536
  "Export byte size limit for otpauth-migration URLs.
The total size of any generated otpauth-migration scheme URL
will not exceed this size."
  :group 'totp-auth
  :type 'integer)

(defcustom totp-auth-secrets-create-item-workaround t
  "The replace parame of freedesktop secrets CreateItem is unreliable.
If this option is on (the default) then we attempt
delete duplicated secrets when we save a secret via this API.\n
If it is off then you are likely to end up with multiple copies of
a secret if you ever re-import it."
  :group 'totp-auth
  :type  'boolean)

(defcustom totp-auth-auto-copy-password '(PRIMARY CLIPBOARD)
  "If set \\[totp-auth] will copy tokens into the selected copy/paste backends.
The behaviour is implemented by ‘totp-auth-update-paste-buffers’ as follows:
 - When the token is generated, it is placed in the selected copy areas
 - If the copy area still contains the previous value when the token
   expires and is regenerated it is replaced with the new value."
  :group 'totp-auth
  :type '(choice
          (const :tag "Off" nil)
          (set :tag "Choose Copy Method(s)"
           (const :tag "Primary (middle-click etc)"  PRIMARY)
           (const :tag "Clipboard (Paste, C-y, C-v)" CLIPBOARD)
           (const :tag "Secondary"                   SECONDARY))))

(defcustom totp-auth-display-token-method nil
  "Choose the TOTP token display mechanism.
A Custom function it must accept a ‘totp-auth-generate-otp’ SECRET
and optional LABEL as its first two arguments."
  :group 'totp-auth
  :type '(choice
          (const :tag "Notification if possible, otherwise TOTP buffer" nil)
          (const :tag "Desktop notification"
                 totp-auth-display-token-notification)
          (const :tag "TOTP buffer" totp-auth-display-token-buffer)
          (function :tag "Custom function")))

(defcustom totp-auth-sources nil
  "Serves the same purpose as ‘auth-sources’, but for the TOTP package.
If unset (the default) this will be initialised to a list
consisting of the contents of ‘auth-sources’ with the freedesktop
secrets service login session prepended to it, if it is available."
  :group 'totp-auth
  :type `(repeat :tag "Authentication Sources"
                 (choice
                  (const :tag "TOTP Secrets Collection" "secrets:TOTP")
                  (const :tag "Default Secrets Collection" default)
                  (const :tag "Login Secrets Collection" "secrets:login")

                  (const :tag "Default internet Mac OS Keychain"
                         macos-keychain-internet)

                  (const :tag "Default generic Mac OS Keychain"
                         macos-keychain-generic)
                  (string :tag "Just a file")

                  (list :tag "Source definition"
                        (const :format "" :value :source)
                        (choice :tag "Authentication backend choice"
                                (string :tag "Authentication Source (file)")
                                (list
                                 :tag "Secret Service API/KWallet/GNOME Keyring"
                                 (const :format "" :value :secrets)
                                 (choice :tag "Collection to use"
                                         (string :tag "Collection name")
                                         (const :tag "Default" default)
                                         (const :tag "Login" "Login")
                                         (const
                                          :tag "Temporary" "session")))
                                (list
                                 :tag "Mac OS internet Keychain"
                                 (const :format ""
                                        :value :macos-keychain-internet)
                                 (choice :tag "Collection to use"
                                         (string :tag "internet Keychain path")
                                         (const :tag "default" default)))
                                (list
                                 :tag "Mac OS generic Keychain"
                                 (const :format ""
                                        :value :macos-keychain-generic)
                                 (choice :tag "Collection to use"
                                         (string :tag "generic Keychain path")
                                         (const :tag "default" default))))
                        (repeat :tag "Extra Parameters" :inline t
                                (choice :tag "Extra parameter"
                                        (list
                                         :tag "Host"
                                         (const :format "" :value :host)
                                         (choice :tag "Host (machine) choice"
                                                 (const :tag "Any" t)
                                                 (regexp
                                                  :tag "Regular expression")))
                                        (list
                                         :tag "Protocol"
                                         (const :format "" :value :port)
                                         (choice
                                          :tag "Protocol"
                                          (const :tag "Any" t)
                                          ,@auth-source-protocols-customize))
                                        (list :tag "User" :inline t
                                              (const :format "" :value :user)
                                              (choice
                                               :tag "Personality/Username"
                                               (const :tag "Any" t)
                                               (string
                                                :tag "Name"))))))
                  (sexp :tag "A data structure (external provider)"))))

(defun totp-auth-sources ()
  "Initialise variable ‘totp-auth-sources’ if necessary and return it."
  (or totp-auth-sources
      (let ((case-fold-search t) login totp)
        ;; find "login" and "TOTP" collections
        (mapc (lambda (s)
                (cond ((string-match "^login$" s) (setq login (concat "secrets:" s)))
                      ((string-match "^totp$"  s) (setq totp  (concat "secrets:" s)))))
              (ignore-errors (secrets-list-collections)))
        ;; add the freedesktop login collection we found to our auth
        ;; source list _if_ it isn't there (remembering that it may be
        ;; referred to as 'default):
        (setq totp-auth-sources
              (if (and login
                       (not (memq   'default auth-sources))
                       (not (member login    auth-sources)))
                  (copy-sequence (cons login auth-sources))
                (copy-sequence auth-sources)))
        ;; and prepend the TOTP collection if it exists
        (if (and totp (not (member totp totp-auth-sources)))
            (setq totp-auth-sources (cons totp totp-auth-sources)))
        totp-auth-sources)))

(defun totp-auth-wrap-otpauth-url (s)
  "Take a TOTP secret S and encode it as an otpauth url.
This is not an exact reverse of ‘totp-auth-unwrap-otpauth-url’ since that
function ignores some otpauth attributes for compatibility with other
authenticators."
  (let ((service (cdr (assq :service s)))
        (user    (cdr (assq :user    s)))
        (secret  (cdr (assq :secret  s)))
        (digits  (cdr (assq :digits  s)))
        (allowed (cons ?@ url-unreserved-chars)))
    (or (memq digits '(6 8 10))
        (setq digits 6))
    (if (> (length user) 0)
        (format "otpauth://totp/%s%%3A%s?secret=%s;digits=%d"
                (url-hexify-string service allowed)
                (url-hexify-string user    allowed)
                (url-hexify-string secret  allowed) digits)
      (format "otpauth://totp/%s?secret=%s;digits=%d"
              (url-hexify-string service allowed)
              (url-hexify-string secret  allowed) digits)) ))

(defun totp-auth-unwrap-otpauth-url (u)
  "Unpack an otpauth url U and extract the bits we care about.
Some settings (eg the chunk size) are ignored because they've
never been handled by google authenticator either, which just uses
the default."
  (let (srv service query secret digits user)
    (setq u       (url-path-and-query u)
          srv     (replace-regexp-in-string "^/" "" (car u))
          srv     (url-unhex-string srv)
          query   (url-parse-query-string (cdr u))
          secret  (cadr (assoc "secret" query))
          digits  (cadr (assoc "digits" query)))
    (setq digits (if digits (string-to-number digits) 6)
          digits (if (< digits 6) 6 (if (> digits 10) 10 digits)))
    (if (string-match "^\\(.*?\\):\\(.*\\)" srv)
        (setq service (match-string 1 srv)
              user    (match-string 2 srv))
      (setq service srv))
    `((:service . ,service)
      (:user    . ,user   )
      (:secret  . ,secret )
      (:digits  . ,digits )) ))

(defun totp-auth-unwrap-otp-blob (blob &optional label)
  "Unwrap a stored TOTP BLOB.
BLOB may be either an otpauth URL or a bare base32 encoded TOTP secret
Returns an alist of the form:\n
  ((:service . \"SOME-SERVICE-LABEL\")
   (:user    . \"SOME-USER-IDENT\")
   (:secret  . \"deadbeefdeadbeefdeadbeefdeadbeef\")
   (:digits  . 6))\n
Note that :user may be nil, :digits defaults to 6 if unspecified,
and service will default to LABEL if the stored blob was simply the
base32 encoded secret.\n
The secret will NOT be base32 decoded."
  (let ((u (url-generic-parse-url blob)))
    (if (equal (url-type u) "otpauth")
        ;; otpauth:// URL. extract the bits we care about:
        (totp-auth-unwrap-otpauth-url u)
      ;; bare base32 encoded secret. make some stuff up:
      `((:secret  . ,(url-filename u))
        (:digits  . 6)
        (:service . ,label))) ))

(defun totp-auth-storage-backends (&optional encrypted)
  "Return a list of available storage backends based on ‘auth-sources’.
If ENCRYPTED is true then only encrypted backends are considered.
Each entry is an alist of the form:
  ((:source    . function ‘auth-source-backend’ object)
   (:handler   . :secrets for a desktop secrets API or :default)
   (:encrypted . t if the backend is nontrivially encrypted, nil otherwise))"
  (delq nil
        (mapcar (lambda (s &optional secure type source handler)
                  (setq source  (slot-value s 'source)
                        type    (slot-value s 'type)
                        secure  (or (eq type 'secrets)
                                    (eq type 'plist)
                                    (equal (file-name-extension source) "gpg"))
                        handler (if (eq type 'secrets) :secrets :default))
                  (if (and encrypted (not secure))
                      nil
                    (list (cons :source    s)
                          (cons :handler   handler)
                          (cons :encrypted secure))))
                (mapcar #'auth-source-backend-parse (totp-auth-sources)))))

(defun totp-auth-get-secrets-from-secrets-source (source)
  "Return an alist of secrets from SOURCE (a desktop secrets API auth-source).
The car of each cell will be the label by which the secrets API identifies this
secret, the cdr will be an alist as returned by ‘totp-auth-unwrap-otp-blob’."
  (let (found vault next)
    (setq vault (slot-value source 'source))
    (mapc
     (lambda (schema)
       (mapc
        (lambda (label)
          (setq next  (secrets-get-secret vault label)
                ;;x   (message "secret:%S" next)
                ;;x   (message "attr  :%S" (secrets-get-attributes vault label))
                next  (totp-auth-unwrap-otp-blob next label)
                next  (cons label next)
                found (cons next found)))
        (secrets-search-items vault :xdg:schema schema)))
     (cons totp-auth-xdg-schema totp-auth-alt-xdg-schemas))
    found))

(defun totp-auth-get-secrets-from-default-source (source)
  "Return an alist of secrets from SOURCE (an auth-secrets source).
The car of each cell will be a [user@]host label and the cdr will be the
TOTP secret."
  (let (found)
    (mapc (lambda (x &optional host user secret label otpmeta)
            (setq host   (plist-get x :host  )
                  user   (plist-get x :user  )
                  secret (plist-get x :secret))
            (if (and host user)
                (setq label (concat user "@" host))
              (setq label (or user host)))
            (if (functionp secret) (setq secret (funcall secret)))
            (setq otpmeta (totp-auth-unwrap-otp-blob secret label)
                  found   (cons (cons label otpmeta) found)))
          (auth-source-search-backends (list source)
                                       (list :port "totp")
                                       totp-auth-max-tokens nil nil
                                       '(:port :secret)))
    found))

(defun totp-auth-get-secrets-from-backend (backend)
  "Fetch secrets from a specific auth-source BACKEND."
  (when (cdr (assq :encrypted backend))
    (let (source)
      (setq source (cdr (assq :source  backend)))
      (cond ((eq (cdr (assq :handler backend)) :secrets)
             (totp-auth-get-secrets-from-secrets-source source))
            (t
             (totp-auth-get-secrets-from-default-source source))) )))

(defun totp-auth-same-secret (a b)
  "Test whether secrets A and B are the same.
\nNOTE: This is not a strict test of equality - rather we are checking to
see if the user and service components of the secret identifier are the
same, ie probably intended for the same target."
  (and (equal (assq :service a) (assq :service b))
       (equal (assq :user    a) (assq :user    b))))

(defun totp-auth-get-backend-for-secret (s)
  "Return the backend in which secret S is stored,
or the default encrypted backend, or nil."
  (let (backends vault default target secrets)
    (setq backends (totp-auth-storage-backends)
          default  (car (totp-auth-storage-backends :encrypted)))
    (while (and (not target) backends)
      (setq vault    (car backends)
            secrets  (totp-auth-get-secrets-from-backend vault)
            backends (cdr backends))
      (if (cl-member s secrets :test 'totp-auth-same-secret)
          (setq target vault)))
    (or target default)))

(defun totp-auth-secret-make-label (secret)
  "Take a ‘totp-auth-unwrap-otp-blob’ SECRET and generate a label from it.
The label will be based on its user and service fields."
  (let (user srv-host)
    (setq user     (cdr (assq :user secret))
          srv-host (cdr (or (assq :service secret)
                            (assq :host    secret))))
    (if (and user (> (length user) 0) srv-host)
        (concat user "@" srv-host)
      (or srv-host user "nobody@unknown"))))

(defun totp-auth-secret-make-label-and-wrapper (secret &optional label)
  "Take a ‘totp-auth-unwrap-otp-blob’ SECRET and make a LABEL and otpauth URL.
LABEL is used as the default label.  If not supplied, ine is generated for you
by ‘totp-auth-secret-make-label’.\n
Returns a cons cell of the form \\='(LABEL . OTPAUTH-URL)"
  (let ((wrapped (totp-auth-wrap-otpauth-url secret)))
    (if (not label)
        (setq label (totp-auth-secret-make-label secret)))
    (cons label wrapped)))

(defun totp-auth-get-item-attribute (item attribute)
  "Take a freedesktop secrets ITEM and return its ATTRIBUTE value."
  (ignore-errors
    (cadr (assoc attribute (cdr (assoc "Attributes" item))))))

(defun totp-auth-save-secret-to-secrets-source (source secret &optional label)
  "Save SECRET to a freedesktop Secrets Service.
SECRET is described in ‘totp-auth-unwrap-otp-blob’.
The secret is saved with the with description LABEL.\n
SOURCE is an auth-source representing the Secrets Service Collection
to save in (usually the login keyring).\n
If LABEL is not supplied, one is constructed based on the contents
of SECRET.
Gnome Keyring and KWallet are examples of the freedesktop secrets services."
  (let (payload vault created)
    (setq payload (totp-auth-secret-make-label-and-wrapper secret label)
          vault   (slot-value source 'source))
    ;; (message "(secrets-create-item %S %S %S :xdg:schema %S)"
    ;;          vault
    ;;          (car payload)
    ;;          (cdr payload)
    ;;          totp-xdg-schema)
    (setq created (secrets-create-item vault
                                       (car payload)
                                       (cdr payload)
                                       :xdg:schema totp-auth-xdg-schema))
    ;; de-duplicate by hand:
    (when totp-auth-secrets-create-item-workaround
      (let (path props schema maybe-dup stored)
        (setq stored (cdr (assq :secret secret)) ;; secret we just stored
              path   (secrets-collection-path vault))
        (dolist (item-path (secrets-get-items path))
          (when (not (equal created item-path))
            (setq props  (secrets-get-item-properties item-path)
                  schema (totp-auth-get-item-attribute props "xdg:schema"))
            (when (equal totp-auth-xdg-schema schema)
              (setq maybe-dup (secrets-get-secret vault item-path)
                    maybe-dup (totp-auth-unwrap-otp-blob maybe-dup)
                    maybe-dup (cdr (assq :secret maybe-dup))) ;; another secret
              (when (equal stored maybe-dup) ;; new and old secrets are equal
                (secrets-delete-item vault item-path)))))))
    created))

(defun totp-auth-save-secret-to-default-source (source secret &optional label)
  "Save SECRET (see ‘totp-auth-unwrap-otp-blob’) to the auth-source SOURCE.
\nSOURCE is any valid auth-source except a freedesktop Secrets Service.\n
LABEL is used as a hint when constructing the host attribute of the
stored secret if it is both supplied and the secret does not have a
host value."
  (let (payload user host password saver)
    (setq user     (or (cdr (assq :user secret)) "-")
          payload  (totp-auth-secret-make-label-and-wrapper secret label)
          host     (or (cdr (or (assq :service secret)
                               (assq :host    secret)))
                      (car payload))
          password (cdr payload))
    (setq saver (apply (slot-value source 'create-function)
                 `(:backend ,source
                            :host    ,host
                            :user    ,user
                            :port    "totp"
                            :secret  ,password
                            :create  t))
          saver (and saver
                     (car saver)
                     (plist-get (car saver) :save-function)))
    (if saver
        (funcall saver)
      (message "No saver for secret %s in backend %S" (car payload) source))))

(defun totp-auth-save-secret (secret &optional backend)
  "Save SECRET (see ‘totp-auth-unwrap-otp-blob’) to BACKEND.
\nIf BACKEND is unspecified search the available secret sources for SECRET
and save to the first one that contains it.\n
If SECRET is not found (see ‘totp-auth-get-backend-for-secret’) then choose
the first encrypted backend returned by ‘totp-auth-storage-backends’."
  (if (not backend)
      (setq backend (or (totp-auth-get-backend-for-secret secret)
                        (car (totp-auth-storage-backends :encrypted)))))
  (let ((source (cdr (assq :source backend))))
    (cond ((eq (cdr (assq :handler backend)) :secrets)
           (totp-auth-save-secret-to-secrets-source source secret))
          (t
           (totp-auth-save-secret-to-default-source source secret)))))

(defun totp-auth-secrets (&optional match)
  "Return a list of TOTP secrets with their labels.
MATCH may be a STRING, or nil.
If MATCH is nil, all available secrets are returned.
If it is a string starting with ~ or / it is used as a regular expression
to choose secrets based on their labels (see below).
If it is a string starting with = then the label must match exactly (note
that labels do not have to be unique to a single secret).
Any other value is used for a simple substring match with the label.
\nEach Item of the returned list is a cons of a label and
a structure conforming to ‘totp-auth-unwrap-otp-blob’."
  (let (secrets)
    (setq secrets
          (apply #'nconc
                 (mapcar #'totp-auth-get-secrets-from-backend
                         (totp-auth-storage-backends))))
    ;; If we were given a filter, apply it:
    (if (and (stringp match) (not (zerop (length match))))
        (let (filter)
          (if (eq (aref match 0) ?=)
              (setq match  (substring match 1)
                    filter (lambda (a) (if (equal (car a) match) a nil)))
            (if (memq (aref match 0) '(?/ ?~))
                (setq match (substring match 1)))
            (setq filter (lambda (a) (if (string-match match (car a)) a nil))))
          (delq nil (mapcar filter secrets)))
      ;; no filter, return it all:
      secrets)))

(defun totp-auth-hmac-message (counter)
  "Take COUNTER (an integer) and return its 8-byte big-endian representation."
  (let ((hi-4 (logand #xffffffff (base32-lsh counter -32)))
        (lo-4 (logand #xffffffff counter)))
    (bindat-pack '((:hi4 u32) (:lo4 u32))
                 `((:hi4 . ,hi-4)
                   (:lo4 . ,lo-4)))))

(defun totp-auth-truncate-hash (hmac-hash)
  "Given a 20 byte string or vector HMAC-HASH:
Use the lowest 4 bits of the final byte as an offset,
Read 4 bytes starting at that offset as a big-endian 32-bit integer,
with the highest bit forced to 0 (ie a 31 bit integer)."
  (let (offset b0 b1 b2 b3)
    (setq offset (logand #x0f (aref hmac-hash (1- (length hmac-hash))))
          b0     (logand #x7f (aref hmac-hash offset))
          b1     (logand #xff (aref hmac-hash (+ 1 offset)))
          b2     (logand #xff (aref hmac-hash (+ 2 offset)))
          b3     (logand #xff (aref hmac-hash (+ 3 offset))))
    (logior (base32-lsh b0 24)
            (base32-lsh b1 16)
            (base32-lsh b2 8) b3)))

(defvar totp-auth-override-time nil
  "This value is used instead of the seconds since epoch if it is set.")

(defun totp-auth-generate-otp (secret &optional digits offset chunk algo)
  "Given the following:
- a string (or ‘totp-auth-unwrap-otp-blob’ struct) SECRET
- a length DIGITS (default 6)
- an integer time skew OFFSET (default 0)
- a time slice size CHUNK (default 30)
- a cryptographic hash algorithm ALGO (default sha1)
Return (TOTP TTL EXPIRY) where TOTP is the time-based one time password,
TTL is the number of seconds the password is good for at the time of generation
and EXPIRY is the seconds after the epoch when the TOTP expires."
  (if (listp secret)
      (setq secret (cdr (assq :secret secret))))
  (let ((digits     (or digits   6))
        (offset     (or offset   0))
        (chunk      (or chunk   30))
        (algo       (or algo 'sha1))
        (secret     (base32-decode (upcase secret)))
        (now        (or totp-auth-override-time (floor (time-to-seconds))))
        then counter ttl expiry msg hash totp)
    (setq then    (- now offset)
          counter (/ then chunk)
          ttl     (- chunk (% now  chunk))
          expiry  (+ now  chunk)
          msg     (totp-auth-hmac-message counter)
          hash    (totp-auth-hmac secret msg algo)
          totp    (% (totp-auth-truncate-hash hash) (expt 10 digits)))
    (let ((fmt (format "%%0%dd" digits)))
      (setq totp (format fmt totp)))
    (list totp ttl expiry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI code
(defvar totp-auth-display-ttl    nil)
(defvar totp-auth-display-label  nil)
(defvar totp-auth-display-expiry nil)
(defvar totp-auth-display-secret nil)
(defvar totp-auth-display-oldpwd nil)

(defun totp-auth-update-paste-buffers (old new)
  "For each copy/paste buffer selected by ‘totp-auth-auto-copy-password’:
Update the contents to password NEW (if it contains password OLD,
or if OLD is unset)."
  ;;(message "totp-auth-update-paste-buffers %S (%S)" old new totp-auth-auto-copy-password)
  (mapc (lambda (type &optional ok)
          (with-demoted-errors "gui get/set selection error: %S"
            (setq ok (if old (equal old (gui-get-selection type)) t))
            (if ok (gui-set-selection type (or new "")))))
        totp-auth-auto-copy-password))

(defun totp-auth-cancel-this-timer ()
  "Cancel the timer whose callback this is called from."
  (let ((n 1) (cancelled 0) f cb cb-args)
    (while (and (setq f (backtrace-frame n #'totp-auth-cancel-this-timer))
                (not (car f)))
      (setq n (1+ n)))
    (when (and f (car f))
      (setq cb      (cadr f)
            cb-args (cddr f))
      (dolist (timer timer-list)
        (when (and (eq (timer--function timer) cb)
                   (equal (timer--args timer) cb-args))
          (cancel-timer timer)
          (setq cancelled (1+ cancelled)))))
    cancelled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOTP buffer based UI
(defun totp-auth-update-token-display (buf &optional otp token)
  "Update a TOTP token display buffer BUF with the lifespan and current token.
Will also call ‘totp-auth-update-paste-buffers’.
OTP and TOKEN are used internally and need not be passed."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (erase-buffer)
        (if (or (not totp-auth-display-ttl)
                (not totp-auth-display-expiry)
                (not totp-auth-display-oldpwd))
            ;; metadata unset, need to generate TOTP
            (setq otp                 (totp-auth-generate-otp totp-auth-display-secret)
                  token               (nth 0 otp)
                  totp-auth-display-ttl    (nth 1 otp)
                  totp-auth-display-expiry (nth 2 otp))
          ;; metadata already set, work out our new ttl:
          (setq token totp-auth-display-oldpwd
                totp-auth-display-ttl
                (floor (- (time-to-seconds) totp-auth-display-expiry))))
        ;; regenerate metadata if the ttl is <= 0
        (if (>= 0 totp-auth-display-ttl)
            (setq otp (totp-auth-generate-otp totp-auth-display-secret)
                  token               (nth 0 otp)
                  totp-auth-display-ttl    (nth 1 otp)
                  totp-auth-display-expiry (nth 2 otp)))
        ;; update the copy/paste buffers if necessary:
        (totp-auth-update-paste-buffers totp-auth-display-oldpwd token)
        (setq totp-auth-display-oldpwd token)
        ;; display the current token
        (insert (format "TOTP %s [%02ds]: %s\n"
                        totp-auth-display-label totp-auth-display-ttl token)))
    (totp-auth-cancel-this-timer)))

(defun totp-auth-display-token-buffer (secret &optional label)
  "Display buffer with the current token for SECRET with label LABEL."
  (let (ui-buffer)
    (or label
        (setq label (totp-auth-secret-make-label secret)))
    (setq ui-buffer (get-buffer-create (format "*TOTP %s*" label)))
    (set-buffer ui-buffer)
    (mapc 'make-local-variable '(totp-auth-display-ttl
                                 totp-auth-display-label
                                 totp-auth-display-expiry
                                 totp-auth-display-oldpwd
                                 totp-auth-display-secret))
    (setq totp-auth-display-label  label
          totp-auth-display-secret (cdr (assq :secret secret))
          totp-auth-display-oldpwd nil
          totp-auth-display-ttl    nil
          totp-auth-display-expiry nil)
    (pop-to-buffer ui-buffer)
    (run-with-timer 0 1 #'totp-auth-update-token-display ui-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop Notification based UI
(defun totp-auth-notification-action (id key secret)
  "Handle a desktop notification “copy” action.
ID is the freedesktop notification id.
KEY is the action (we currently only handle \"copy\").
SECRET is a suitable argument for ‘totp-auth-generate-otp’.
\nCopy the current OTP token for SECRET with `totp-auth-update-paste-buffers',
then close the notification.
\nIf the current token is about to expire (see ‘totp-auth-minimum-ui-grace’)
then wait until it is time to renew the token before doing anything."
  (when (equal "copy" key)
    (let (otp ttl token)
      (setq otp (totp-auth-generate-otp secret)
            ttl (nth 1 otp))
      (when (>= totp-auth-minimum-ui-grace ttl)
        (sit-for ttl)
        (setq otp (totp-auth-generate-otp secret)))
      (setq token (nth 0 otp))
      (let ((totp-auth-auto-copy-password (or totp-auth-auto-copy-password '(PRIMARY))))
        (totp-auth-update-paste-buffers nil token)))
    (notifications-close-notification id)))

(defun totp-auth-update-token-notification (id label secret)
  "Update a notification displaying a TOTP token.
ID is the freedesktop notifications id (an unsigned 32 but integer).
LABEL is the descriptive label of the OTP secret.
SECRET is a suitable secret usable by ‘totp-auth-generate-otp’.
Usually called from a timer set by ‘totp-auth-display-token-notification’."
  (let (otp text ttl)
    (setq otp  (totp-auth-generate-otp secret)
          ttl  (nth 1 otp)
          text (if (>= totp-auth-minimum-ui-grace ttl)
                   "Generating…  [⌛]"
                 (format "%s  [%02ds]" (nth 0 otp) ttl)))
    (notifications-notify
     :title       label
     :replaces-id id
     :body        text
     :actions    '("default" "Close" "copy" "Copy")
     :timeout     0
     :resident    t)))

(defun totp-auth-display-token-notification (secret &optional label)
  "Display a notification with the current token for SECRET with label LABEL."
  ;; this is only required if the user has explicitly configured display
  ;; via notifications - the default path checks to see if notifications
  ;; support can be loaded before we get here:
  (require 'notifications)
  (or label
      (setq label (totp-auth-secret-make-label secret)))
  (let (nid update)
    (setq update (timer-create)
          nid    (notifications-notify
                  :title     label
                  :body      "Generating…  [⌛]"
                  :actions  '("default" "Close" "copy" "Copy")
                  :timeout   0
                  :resident  t
                  :on-action (lambda (id key)
                               (totp-auth-notification-action id key secret))
                  :on-close  (lambda (_id _key) (cancel-timer update))))
    (timer-set-time     update (current-time) 1)
    (timer-set-function update
                        #'totp-auth-update-token-notification (list nid label secret))
    (timer-activate     update)
    update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic UI
(defun totp-auth-display-token (secret &optional label)
  "Display the TOTP token for secret.
Display method is determined by ‘totp-auth-display-token-method’.
SECRET is a string or structure consumable by ‘totp-auth-generate-otp’,
LABEL is a label or description of the secret (eg its user and service
information).
LABEL will be initialised by ‘totp-auth-secret-make-label’ if unset."
  (or label
      (setq label (totp-auth-secret-make-label secret)))
  (if totp-auth-display-token-method
      (funcall totp-auth-display-token-method secret label)
    (if (ignore-errors (and (require 'notifications)
                            (notifications-get-server-information)))
        (totp-auth-display-token-notification secret label)
      (totp-auth-display-token-buffer secret label))))

;;;###autoload
(defun totp-auth-add-secret (secret &optional service user digits)
  "Store a SECRET for USER @ SERVICE.
SECRET may be:
  A base32 encoded secret string
  An otpauth:// URL
If SECRET is a base32 secret string then SERVICE must be supplied.
If both a URL and SERVICE, USER or DIGITS are supplied then the explicit
values passed in will override the URL.
DIGITS defaults to 6 if not otherwise specified."
  (interactive
   (let (s u)
     (setq s (read-passwd "Secret: ")
           u (url-generic-parse-url s))
     (if (not (equal (url-type u) "otpauth"))
         (list s
               (read-string "Service: ")
               (read-string "User: " )
               (read-string "Size: " nil nil "6"))
       (setq s (totp-auth-unwrap-otpauth-url u))
       (mapcar (lambda (k) (cdr (assq k s)) )
               '(:secret :service :user :digits)))))
  (totp-auth-save-secret `((:service . ,service)
                           (:user    . ,user)
                           (:secret  . ,secret)
                           (:digits  . ,(or digits 6)))))

;;;###autoload
(defun totp-auth (&optional secret label)
  "Generate a TOTP token for SECRET, identified by LABEL, and show it."
  (interactive
   (let ((secrets (totp-auth-secrets))
         (completion-ignore-case t)
         (completion-styles '(substring))
         key)
     (setq key (completing-read "Generate TOTP: " secrets))
     (list (or (cdr (assoc key secrets)) (length secrets)) key)))
  (if (and (called-interactively-p 'interactive) (numberp secret))
      (display-message-or-buffer
       (format "No secrets for %S found (%d available)" label secret))
    (totp-auth-display-token secret label)))

(autoload 'totp-auth-import-file "totp-auth-interop"
  "Import an RFC6238 TOTP secret or secrets from FILE.
FILE is processed by ‘totp-load-file’ and each secret extracted
is passed to ‘totp-save-secret’."
  t)

(autoload 'totp-auth-export-file "totp-auth-interop"
  "Export TOTP SECRETS to FILE.
FILE is a destination file.
If it matches ‘epa-file-name-regexp’ then a text file is saved.
If ‘totp-auth-image-type-from-filename’ returns an image type for file then
a QR code is generated instead.
TYPE may be :otpauth-migration or :otpauth - which URL scheme to use.
\nSECRETS is a list of ‘totp-auth-unwrap-otp-blob’ secrets, or a string, or nil.
If it is nil all secrets are exported.
If it is a string beginning with ~ or / it is used as a regular expression
to match the labels of the secrets to export from ‘totp-auth-secrets’.
If it begins with = the rest of the string is used as an exact match.
Any other string is used as a substring to look for in the labels."
  t)

(provide 'totp-auth)
;;; totp-auth.el ends here
