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

(require 'auth-source)
(require 'browse-url)
(require 'cl-lib)
(require 'url-util)
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

Read only where `vm-oauth2-catch-redirect' is nil.  With that on, which is
the default, VM listens on a loopback port and names that, so there is
nothing to set here and nothing to paste.

oauth2.el defaults to \"urn:ietf:wg:oauth:2.0:oob\", which hands the code to
the reader to paste.  Google stopped accepting that in January 2023 and
answers `invalid_request', so a Google account needs something else here and
it has to be one the registration lists.

Set to whatever your own registration says."
  :group 'vm-oauth2
  :type '(choice (const :tag "oauth2.el's own" nil) string))

(defcustom vm-oauth2-catch-redirect t
  "Whether VM takes the authorization code off the redirect itself.

A provider answers an authorization by redirecting the browser to an address
the client named, with the code in it.  With this on VM listens on a loopback
port, names that address, and reads the code out of the request the browser
makes: nothing is copied, and the browser is answered with a page saying so.

RFC 8252 7.3 is why the port needs no registering: the authorization server
has to accept any port on a loopback redirect.  What does need registering is
the client, as a desktop application; a web application is held to the exact
addresses it listed and cannot use this.

The browser has to be on the machine Emacs is on, since the redirect goes to
that machine's own loopback.  Where it is not, the wait runs out and VM asks
for the code as it always did, to be read out of the browser's address bar
and pasted."
  :group 'vm-oauth2
  :type 'boolean)

(defcustom vm-oauth2-redirect-timeout 300
  "How long to wait for the browser to come back with the code, in seconds.
Long enough to sign in, pick an account and read a consent screen.  When it
runs out VM asks for the code instead, which is the answer for a browser on
another machine: its redirect goes to its own loopback and never arrives."
  :group 'vm-oauth2
  :type 'integer)

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

(defvar vm-oauth2--code nil
  "The authorization code the browser brought, while one is being waited for.")

(defvar vm-oauth2--refusal nil
  "What the provider sent in place of a code, while one is being waited for.")

(defvar vm-oauth2--catching nil
  "Whether this authorization is listening for the redirect itself.
What `vm-oauth2-show-url' tells the reader to do next turns on it.")

(defun vm-oauth2--page (text)
  "An HTTP response whose whole page is TEXT.
The browser follows the redirect expecting a page; without one it shows its
own failure to connect, which reads as the authorization having gone wrong."
  (let ((body (format (concat "<!DOCTYPE html>\n<html><head>"
			      "<meta charset=\"utf-8\"><title>VM</title>"
			      "</head>\n<body><p>%s</p></body></html>\n")
		      text)))
    (concat "HTTP/1.1 200 OK\r\n"
	    "Content-Type: text/html; charset=utf-8\r\n"
	    (format "Content-Length: %d\r\n" (string-bytes body))
	    "Connection: close\r\n"
	    "\r\n"
	    body)))

(defun vm-oauth2--parameter (query name)
  "The value NAME has in QUERY, a URL query string, or nil."
  (let ((query (concat "&" query)))
    (when (string-match (concat "&" (regexp-quote name) "=\\([^&]*\\)") query)
      (url-unhex-string (match-string 1 query)))))

(defun vm-oauth2--answer (connection text)
  "Answer CONNECTION with TEXT as its page, and close it."
  (ignore-errors (process-send-string connection (vm-oauth2--page text)))
  (ignore-errors (delete-process connection)))

(defun vm-oauth2--filter (connection request)
  "Take the code out of REQUEST, which the browser made of CONNECTION.

The redirect reaches VM as an ordinary GET and only its first line matters,
which a browser sends in one piece.  A request with neither a code nor an
error in it is somebody else knocking, and is answered and ignored."
  (if (string-match "\\`GET +[^ ?]*\\?\\([^ ]*\\) HTTP/" request)
      (let* ((query (match-string 1 request))
	     (code (vm-oauth2--parameter query "code"))
	     (refused (vm-oauth2--parameter query "error")))
	(cond (code
	       (setq vm-oauth2--code code)
	       (vm-oauth2--answer
		connection "VM has the code.  You can close this page."))
	      (refused
	       (setq vm-oauth2--refusal refused)
	       (vm-oauth2--answer
		connection (format "The provider refused: %s." refused)))
	      (t (vm-oauth2--answer connection "VM is not waiting for this."))))
    (vm-oauth2--answer connection "VM is not waiting for this.")))

(defun vm-oauth2--listen ()
  "A server on a loopback port of its own, for the provider to redirect to.
The port is `process-contact' with `:service'."
  (make-network-process :name "vm-oauth2-redirect"
			:server t :service t :host 'local :family 'ipv4
			:noquery t :coding 'binary
			:filter #'vm-oauth2--filter))

(defun vm-oauth2--redirect-uri (server)
  "The address SERVER is listening on, as the provider is to be given it."
  (format "http://127.0.0.1:%d/" (process-contact server :service)))

(defun vm-oauth2--wait-for-the-code (seconds)
  "Wait up to SECONDS for the browser to arrive with the code.
Answers with the code, or nil where none came and the reader is to be asked.
A refusal signals: the provider has answered, and answered no."
  (let ((deadline (+ (float-time) seconds)))
    (vm-inform 5 "Waiting for the browser to answer; C-g stops waiting")
    (while (and (null vm-oauth2--code) (null vm-oauth2--refusal)
		(< (float-time) deadline))
      (accept-process-output nil 0.2))
    (when vm-oauth2--refusal
      (error "The provider refused the authorization: %s" vm-oauth2--refusal))
    vm-oauth2--code))

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
	      "Open this URL and sign in.  VM is about to try your browser as\n"
	      "well; no browser is not a failure, the URL below being the\n"
	      "whole of what is needed.\n\n"
	      (if vm-oauth2--catching
		  (concat "VM is listening for the answer and takes the code"
			  " itself.  A\nbrowser on another machine cannot"
			  " reach it: VM asks for the code\nby hand when the"
			  " wait runs out, and it is in that browser's"
			  " address\nbar, after \"code=\".\n\n")
		"Then paste the code VM asks for below; it is in your\nbrowser's address bar, after \"code=\".\n\n")
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

(defvar vm-imap-account-alist)
(defvar vm-pop-folder-alist)
(declare-function vm-imap-parse-spec-to-list "vm-imap" (spec))
(declare-function vm-pop-parse-spec-to-list "vm-pop" (spec))

(defun vm-oauth2--maildrop-accounts ()
  "The user names of the XOAUTH2 maildrops VM has, newest spelling first.
What `vm-oauth2-authorize' completes on: the account to authorize is one of
these and typing a mail address again is one more way to get it wrong.

Read defensively, since this file is autoloaded and runs before the rest of
VM: a VM that has not been started yet offers nothing to complete on."
  (let ((accounts nil))
    (dolist (entry (and (boundp 'vm-imap-account-alist) vm-imap-account-alist))
      (let ((parts (ignore-errors (vm-imap-parse-spec-to-list (car entry)))))
	(when (equal (nth 4 parts) "xoauth2")
	  (push (nth 5 parts) accounts))))
    (dolist (entry (and (boundp 'vm-pop-folder-alist) vm-pop-folder-alist))
      (let ((parts (ignore-errors (vm-pop-parse-spec-to-list (car entry)))))
	(when (equal (nth 3 parts) "xoauth2")
	  (push (nth 4 parts) accounts))))
    (delete-dups (delq nil accounts))))

(defun vm-oauth2--host-for-account (account)
  "The server a maildrop of ACCOUNT names, or nil.
For defaulting the provider: the account is a mail address and says nothing
about who holds the mailbox."
  (let ((host nil))
    (dolist (entry (and (boundp 'vm-imap-account-alist) vm-imap-account-alist))
      (let ((parts (ignore-errors (vm-imap-parse-spec-to-list (car entry)))))
	(when (and (null host) (equal (nth 5 parts) account))
	  (setq host (nth 1 parts)))))
    (dolist (entry (and (boundp 'vm-pop-folder-alist) vm-pop-folder-alist))
      (let ((parts (ignore-errors (vm-pop-parse-spec-to-list (car entry)))))
	(when (and (null host) (equal (nth 4 parts) account))
	  (setq host (nth 1 parts)))))
    host))

(defun vm-oauth2-provider-for-host (host)
  "The provider in `vm-oauth2-providers' that serves HOST, or nil.
By name, which is all there is to go on: a maildrop names a server and an
authorization names a provider, and nothing in between says which is whose."
  (cond ((null host) nil)
	((string-match-p "\\(gmail\\|googlemail\\|google\\)\\." host) "google")
	((string-match-p "\\(outlook\\|office365\\|microsoft\\)\\." host)
	 "microsoft")))

;;;###autoload
(defun vm-oauth2-offer-authorization (account host)
  "Ask whether to authorize ACCOUNT at HOST now, and do it if so.
Answers with non-nil where it authorized.  Called where a session wants a
token and auth-source has none: the command that started it can ask, where
the session itself could not.

Nothing is offered that cannot be done: without oauth2.el this says so and
answers nil, leaving the caller to report a missing token as it always did."
  (and (vm-oauth2-available-p)
       (y-or-n-p (format "No OAuth2 token for %s.  Authorize it now? "
			 account))
       (let ((provider (or (vm-oauth2-provider-for-host host)
			   (completing-read "Provider: " vm-oauth2-providers
					    nil t (caar vm-oauth2-providers)))))
	 (vm-oauth2-authorize account provider)
	 t)))

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

The provider answers by redirecting the browser, and VM listens on a
loopback port for that redirect and takes the code out of it, so there is
nothing to copy.  See `vm-oauth2-catch-redirect', which also says what
happens where the browser is on another machine.

The token goes in oauth2.el\\='s own store, `oauth2-token-file', encrypted by
plstore.  VM keeps no store of its own: writing the same one is what makes
an account set up here work with everything else that reads it."
  (interactive
   (let* ((accounts (vm-oauth2--maildrop-accounts))
	  (account (completing-read
		    (format "Mail address to authorize%s: "
			    (if accounts "" " (none known yet)"))
		    accounts nil nil nil nil (car accounts)))
	  ;; the provider the account's own maildrop points at, where one of
	  ;; VM's maildrops names it
	  (host (vm-oauth2--host-for-account account)))
     (list account
	   (completing-read "Provider: " vm-oauth2-providers nil t
			    (or (vm-oauth2-provider-for-host host)
				(caar vm-oauth2-providers))))))
  (vm-oauth2-require)
  (unless vm-oauth2-client-id
    (error (concat "No client id: register VM with %s as an installed"
		   " application and set vm-oauth2-client-id")
	   provider))
  (let* ((entry (vm-oauth2-provider provider))
	 ;; listening before the browser is opened, the redirect being the
	 ;; answer to it
	 (server (and vm-oauth2-catch-redirect (vm-oauth2--listen)))
	 (vm-oauth2--catching (and server t))
	 (vm-oauth2--code nil)
	 (vm-oauth2--refusal nil)
	 ;; oauth2.el opens the browser and then reads the code with
	 ;; `read-string'.  Answered out of the redirect where one arrives,
	 ;; and by asking where none does, which is a browser on another
	 ;; machine: its redirect went to that machine's own loopback.
	 (ask (symbol-function 'read-string))
	 ;; What the reader would have used, read before it is bound away:
	 ;; see `vm-oauth2--browser'.
	 (vm-oauth2--browser browse-url-browser-function)
	 (vm-oauth2--handlers browse-url-handlers)
	 ;; `browse-url' takes a handler from `browse-url-handlers' before it
	 ;; looks at `browse-url-browser-function', so both have to go or the
	 ;; URL never reaches `vm-oauth2-browse'
	 (browse-url-handlers nil)
	 (browse-url-browser-function #'vm-oauth2-browse))
    (unwind-protect
	(let ((token
	       (cl-letf (((symbol-function 'read-string)
			  (lambda (&rest args)
			    (or (and server
				     (vm-oauth2--wait-for-the-code
				      vm-oauth2-redirect-timeout))
				(apply ask args)))))
		 (oauth2-auth-and-store
		  (plist-get entry :auth-url)
		  (plist-get entry :token-url)
		  (plist-get entry :scope)
		  vm-oauth2-client-id
		  vm-oauth2-client-secret
		  (if server
		      (vm-oauth2--redirect-uri server)
		    vm-oauth2-redirect-uri)
		  nil account nil vm-oauth2-use-pkce))))
	  (unless token
	    (error "%s gave no token for %s" provider account))
	  ;; oauth2.el answers with a token structure whether the provider gave
	  ;; a token or refused to: the refusal is in the response it kept, the
	  ;; access token is nil, and nothing signals.  Said here, or VM reports
	  ;; an authorization that did not happen and the session afterwards
	  ;; finds no token with nothing anywhere to say why.
	  (when (and (oauth2-token-p token)
		     (null (oauth2-token-access-token token)))
	    (let* ((response (oauth2-token-access-response token))
		   (code (cdr (assq 'error response)))
		   (said (or (cdr (assq 'error_description response)) code)))
	      (error "%s would not authorize %s%s%s" provider account
		     (if said (format ": %s" said) "")
		     (if (equal code "invalid_grant")
			 (concat ".  A code is good once, for a few minutes,"
				 " and only to the link that asked for it")
		       ""))))
	  ;; what auth-source answered before there was a token is remembered
	  ;; for two hours, so without this the session that asked for the
	  ;; authorization still finds nothing
	  (auth-source-forget-all-cached)
	  (vm-inform 5 "%s is authorized; write xoauth2 in its maildrop"
		     account)
	  token)
      (when server (delete-process server)))))

(provide 'vm-oauth2)
;;; vm-oauth2.el ends here
