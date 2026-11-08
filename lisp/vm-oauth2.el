;;; vm-oauth2.el --- authorizing a mail account for XOAUTH2  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM

;; VM is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; VM is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; VM speaks XOAUTH2 to IMAP and POP servers and reads the access token out
;; of auth-source; `vm-net-xoauth2-response' is the mechanism and
;; `vm-imap-net-access-token' the lookup.  Where the token comes from is a
;; separate question, and four answers are in use:
;;
;;   auth-source-xoauth2-plugin on GNU ELPA, which makes an auth-source
;;     lookup answer with a freshly refreshed token.  VM needs nothing for
;;     this and this file is not involved: VM asks auth-source as it always
;;     does and gets a token back.
;;   a helper program -- oauth2ms, mutt_oauth2.py, pizauth -- writing the
;;     token where auth-source finds it.
;;   a proxy, DavMail or a local dovecot, which VM reaches with an ordinary
;;     LOGIN and which holds the tokens itself.
;;   this file: `vm-oauth2-authorize' runs the first authorization with
;;     oauth2.el and writes that package's own store, which is the one the
;;     plugin and the helpers read.
;;
;; VM reads tokens through auth-source and nowhere else, so the first three
;; routes need nothing here at all.  What this adds is the authorization
;; itself, presented the way a reader at a terminal needs it: the URL in a
;; buffer of its own before the browser is tried, and a browser that cannot
;; start reported rather than fatal.  oauth2.el puts the URL inside a
;; minibuffer prompt and lets a `browse-url' failure abort the attempt, which
;; over ssh with no DISPLAY is every attempt.
;;
;; Reading a token back out of oauth2.el's store is deliberately not here.
;; Its one public reader, `oauth2-auth-and-store', starts an interactive
;; authorization when it finds nothing -- a browser and a question, from
;; inside a process filter, where nothing can be asked.  Serving tokens to
;; auth-source is `auth-source-xoauth2-plugin''s job and it does it.
;;
;; oauth2.el is optional, the way BBDB is.  Nothing here is loaded until a
;; command is run, nothing else in VM refers to the package, and a VM with no
;; oauth2.el installed says so and names where to get it.
;;
;; VM ships no client credentials.  Anything compiled into a program that
;; readers install is public by definition, and the registration is per
;; reader in any case.  `vm-oauth2-client-id' and `vm-oauth2-client-secret'
;; are where the reader's own go.

;;; Code:

(require 'browse-url)
(require 'vm-macro)
(eval-when-compile (require 'vm-misc))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function oauth2-auth-and-store "ext:oauth2"
		  (auth-url token-url scope client-id client-secret
			    &optional redirect-uri state user-name host-name
			    use-pkce))
(declare-function oauth2-token-p "ext:oauth2" (object))
(declare-function oauth2-token-access-token "ext:oauth2" (token))
(declare-function oauth2-token-access-response "ext:oauth2" (token))

(defgroup vm-oauth2 nil
  "Authorizing a mail account for XOAUTH2."
  :group 'vm)

(defcustom vm-oauth2-providers
  '(("google"
     :auth-url "https://accounts.google.com/o/oauth2/auth"
     :token-url "https://oauth2.googleapis.com/token"
     :scope "https://mail.google.com/")
    ("microsoft"
     :auth-url "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
     :token-url "https://login.microsoftonline.com/common/oauth2/v2.0/token"
     :scope "offline_access https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All"))
  "Where VM asks for an access token, by provider name.

Each entry is (NAME :auth-url URL :token-url URL :scope SCOPE).  The two
built in are the endpoints Google and Microsoft publish, with the scope that
covers reading mail and nothing else: Google has one for all of IMAP, POP
and SMTP, Microsoft one per protocol.  No credentials are here, those being
the reader's own: see `vm-oauth2-client-id'.

A provider not listed is added here.  The names are what
`vm-oauth2-authorize' completes on."
  :group 'vm-oauth2
  :type '(alist :key-type (string :tag "Provider")
		:value-type (plist :key-type symbol :value-type string)))

(defcustom vm-oauth2-client-id nil
  "The client id VM authorizes with, from your own registration.

VM ships none.  A client id and secret compiled into a program that readers
install are public by definition, and a provider makes each reader register
anyway: with Google it is a Desktop application under a project of your own,
with Microsoft an app registration in your tenant.

nil, and `vm-oauth2-authorize' says so rather than failing at the provider."
  :group 'vm-oauth2
  :type '(choice (const :tag "Not registered" nil) string))

(defcustom vm-oauth2-client-secret nil
  "The client secret that goes with `vm-oauth2-client-id', or nil.
Not a secret in the ordinary sense for an installed application, the
provider having no way to keep one on a reader's machine, which is why the
authorization code and PKCE exist.  Microsoft wants one and Google issues
one; a client registered without one leaves this nil."
  :group 'vm-oauth2
  :type '(choice (const :tag "None" nil) string))

(defcustom vm-oauth2-redirect-uri nil
  "Where the provider sends the authorization code, or nil for oauth2.el's own.

oauth2.el defaults to \"urn:ietf:wg:oauth:2.0:oob\", which hands the code to
the reader to paste.  Google stopped accepting that in January 2023 and
answers `invalid_request', so a Google account needs something else here and
it has to be one the registration lists.  A Desktop client registered with
Google may use a loopback address, \"http://127.0.0.1:PORT\"; nothing in VM
or in oauth2.el listens on one, so the code has to be read out of the
browser's address bar and pasted anyway.

Set to whatever your own registration says."
  :group 'vm-oauth2
  :type '(choice (const :tag "oauth2.el's own" nil) string))

(defcustom vm-oauth2-use-pkce t
  "Whether to use PKCE (RFC 7636) in the authorization.
Proof Key for Code Exchange: the client sends a digest of a secret it made
up and then the secret itself, so a code intercepted on its way back is of
no use without it.  Providers want it for installed applications, where the
client secret cannot be kept."
  :group 'vm-oauth2
  :type 'boolean)

(defun vm-oauth2-available-p ()
  "Whether oauth2.el is installed."
  (and (locate-library "oauth2") t))

(defun vm-oauth2-provider (name)
  "The entry in `vm-oauth2-providers' called NAME, or an error naming the rest."
  (or (cdr (assoc name vm-oauth2-providers))
      (error "No OAuth2 provider called %s.  VM knows %s; add another to %s"
	     name
	     (mapconcat #'car vm-oauth2-providers ", ")
	     "vm-oauth2-providers")))

(defun vm-oauth2-require ()
  "Load oauth2.el, or say where to get it."
  (unless (vm-oauth2-available-p)
    (error (concat "This needs oauth2.el, which is not installed."
		   "  M-x package-install RET oauth2 RET gets it from GNU"
		   " ELPA; or let auth-source answer with a token and VM"
		   " needs none of this")))
  (require 'oauth2))

(defun vm-oauth2-show-url (url)
  "Show URL in a buffer of its own, for copying into a browser by hand.

Shown before the browser is tried rather than after it fails.  oauth2.el
puts the URL inside a minibuffer prompt, which is where a 400 character URL
is hardest to copy from, and a reader over ssh with no browser has nothing
else to go on."
  (with-current-buffer (get-buffer-create "*VM OAuth2*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Authorize VM to read this account.\n\n"
	      "Open this URL, sign in, and paste the code VM asks for below.\n"
	      "VM is about to try your browser as well; no browser is not a\n"
	      "failure, the URL above being the whole of what is needed.\n\n"
	      url
	      "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defvar vm-oauth2--browser nil
  "What `vm-oauth2-browse' opens a URL with, or nil for the ordinary default.
Bound by `vm-oauth2-authorize' to the reader's own setting, read before
that setting is bound away.

Not read back out of `browse-url-browser-function', which is where it came
from: a plain `let' on a variable with no buffer-local binding rebinds the
default itself, so `default-value' inside the binding answers
`vm-oauth2-browse' and the browser opens itself until the stack runs out.")

(defvar vm-oauth2--handlers nil
  "What `browse-url-handlers' held before `vm-oauth2-authorize' bound it away.")

(defun vm-oauth2-browse (url &rest _args)
  "Show URL, then open it in the browser the reader would have used.

Bound over `browse-url' for the authorization, so that a browser which
cannot start is information rather than the end of the attempt: over ssh
with no DISPLAY `browse-url' signals, and it signals inside oauth2.el before
the prompt that would have taken a pasted code is reached.

The reader's own settings are put back for the call inside, this function
being what they were bound away for."
  (vm-oauth2-show-url url)
  (let ((browse-url-handlers vm-oauth2--handlers)
	(browse-url-browser-function
	 (or vm-oauth2--browser #'browse-url-default-browser)))
    (condition-case err
	(browse-url url)
      (error
       (vm-inform 5 "No browser (%s); the URL is in *VM OAuth2*"
		  (error-message-string err))))))

;;;###autoload
(defun vm-oauth2-authorize (account provider)
  "Authorize ACCOUNT with PROVIDER and keep the token oauth2.el gives back.

ACCOUNT is the mail address the token is for, which is what the maildrop\\='s
user field names and what auth-source is asked under.  PROVIDER is a key of
`vm-oauth2-providers'.

This asks a question and opens a browser, so it is a command and runs
nowhere else.  Nothing can be asked from inside a process filter, which is
where a session runs, so the first authorization of an account happens here
and the sessions afterwards only read what it stored.

The token goes in oauth2.el\\='s own store, `oauth2-token-file', encrypted by
plstore.  VM keeps no store of its own: writing the same one is what makes
an account set up here work with everything else that reads it."
  (interactive
   (list (read-string "Mail address to authorize: ")
	 (completing-read "Provider: " vm-oauth2-providers nil t
			  (caar vm-oauth2-providers))))
  (vm-oauth2-require)
  (unless vm-oauth2-client-id
    (error (concat "No client id: register VM with %s as an installed"
		   " application and set vm-oauth2-client-id")
	   provider))
  (let* ((entry (vm-oauth2-provider provider))
	 ;; What the reader would have used, read before it is bound away:
	 ;; see `vm-oauth2--browser'.
	 (vm-oauth2--browser browse-url-browser-function)
	 (vm-oauth2--handlers browse-url-handlers)
	 ;; `browse-url' takes a handler from `browse-url-handlers' before it
	 ;; looks at `browse-url-browser-function', so both have to go or the
	 ;; URL never reaches `vm-oauth2-browse'
	 (browse-url-handlers nil)
	 (browse-url-browser-function #'vm-oauth2-browse)
	 (token (oauth2-auth-and-store
		 (plist-get entry :auth-url)
		 (plist-get entry :token-url)
		 (plist-get entry :scope)
		 vm-oauth2-client-id
		 vm-oauth2-client-secret
		 vm-oauth2-redirect-uri
		 nil account nil vm-oauth2-use-pkce)))
    (unless token
      (error "%s gave no token for %s" provider account))
    ;; oauth2.el answers with a token structure whether the provider gave a
    ;; token or refused to: the refusal is in the response it kept, the access
    ;; token is nil, and nothing signals.  Said here, or VM reports an
    ;; authorization that did not happen and the session afterwards finds no
    ;; token with nothing anywhere to say why.
    (when (and (oauth2-token-p token)
	       (null (oauth2-token-access-token token)))
      (let* ((response (oauth2-token-access-response token))
	     (code (cdr (assq 'error response)))
	     (said (or (cdr (assq 'error_description response)) code)))
	(error "%s would not authorize %s%s%s" provider account
	       (if said (format ": %s" said) "")
	       (if (equal code "invalid_grant")
		   (concat ".  A code is good once, for a few minutes, and"
			   " only to the link that asked for it")
		 ""))))
    (vm-inform 5 "%s is authorized; write xoauth2 in its maildrop" account)
    token))

(provide 'vm-oauth2)
;;; vm-oauth2.el ends here
