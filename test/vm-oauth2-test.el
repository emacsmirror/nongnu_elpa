;;; vm-oauth2-test.el --- Tests for vm-oauth2.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for `vm-oauth2-authorize' and what it is built on.  oauth2.el is
;; an optional companion VM does not ship, so both sides matter: naming it
;; here puts this file in the runner's no-optional pass as well, where the
;; absence is real rather than stubbed.
;;
;; Nothing here reaches a provider.  What is worth holding shut is the part
;; that is VM's: that the package is optional, that a reader with no browser
;; still gets the URL, and that a missing client id is reported rather than
;; sent.

;;; Code:

(require 'vm-test-init)
(require 'vm-oauth2)

(defmacro vm-oauth2-test--without-oauth2-el (&rest body)
  "Run BODY with oauth2.el reported absent.
Said through `vm-oauth2-available-p' rather than by emptying `features' or
`load-path', neither of which tells us what a machine without the package
does."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'vm-oauth2-available-p) #'ignore))
     ,@body))

(ert-deftest vm-oauth2-test-says-where-to-get-the-package ()
  "Without oauth2.el the error names the package and how to install it.
A reader who has got as far as running the command has already decided to
use OAuth2, so the useful answer is the install line, not that a function is
void."
  (vm-oauth2-test--without-oauth2-el
    (let* ((text-quoting-style 'grave)
           (message (cadr (should-error (vm-oauth2-require)))))
      (should (string-match-p "oauth2" message))
      (should (string-match-p "package-install" message))
      ;; and the route that needs none of it
      (should (string-match-p "auth-source" message)))))

(ert-deftest vm-oauth2-test-is-optional-on-a-machine-that-really-lacks-it ()
  "The same, with nothing stubbed, in the runner's no-optional pass.
Skipped where oauth2.el is installed, which the stubbed test above covers."
  (skip-unless (not (locate-library "oauth2")))
  (should-not (vm-oauth2-available-p))
  (should-error (vm-oauth2-require)))

(ert-deftest vm-oauth2-test-an-unregistered-client-is-reported ()
  "No client id is an error before anything is sent to the provider.
VM ships no credentials and never will, so this is the state every reader
starts in and the message has to say what to do about it."
  (skip-unless (vm-oauth2-available-p))
  (let* ((vm-oauth2-client-id nil)
         (text-quoting-style 'grave)
         (message (cadr (should-error (vm-oauth2-authorize "a@b.c" "google")))))
    (should (string-match-p "client id" message))
    (should (string-match-p "vm-oauth2-client-id" message))))

(ert-deftest vm-oauth2-test-an-unknown-provider-names-the-known-ones ()
  "A provider VM has no endpoints for says which it has, and where to add one."
  (let* ((text-quoting-style 'grave)
         (message (cadr (should-error (vm-oauth2-provider "yahoo")))))
    (should (string-match-p "yahoo" message))
    (should (string-match-p "google" message))
    (should (string-match-p "vm-oauth2-providers" message))))

(ert-deftest vm-oauth2-test-the-built-in-providers-ask-for-mail-scopes ()
  "The two entries carry the scope that reads mail, and an https endpoint.
A scope that does not cover IMAP gets a token the server refuses, which
looks like a bad password and is not one."
  (let ((google (vm-oauth2-provider "google"))
        (microsoft (vm-oauth2-provider "microsoft")))
    (should (equal (plist-get google :scope) "https://mail.google.com/"))
    (should (string-match-p "IMAP.AccessAsUser.All"
                            (plist-get microsoft :scope)))
    (should (string-match-p "POP.AccessAsUser.All"
                            (plist-get microsoft :scope)))
    ;; a refresh token is only issued where it is asked for
    (should (string-match-p "offline_access" (plist-get microsoft :scope)))
    (dolist (entry (list google microsoft))
      (dolist (key '(:auth-url :token-url))
        (should (string-prefix-p "https://" (plist-get entry key)))))))

(ert-deftest vm-oauth2-test-the-url-is-shown-before-the-browser-is-tried ()
  "REGRESSION: the URL reaches a buffer even when no browser will start.
oauth2.el calls `browse-url' and then prompts with the URL inside the prompt
string.  Over ssh with no DISPLAY `browse-url' signals, which ends the
authorization before the prompt is reached, and a 400 character URL in a
minibuffer prompt is the worst place to copy one from anyway."
  (let ((url "https://accounts.example.com/o/oauth2/auth?client_id=x&scope=y")
        (asked nil))
    (unwind-protect
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (&rest _) (setq asked t) (error "no DISPLAY")))
                  ((symbol-function 'vm-inform) #'ignore))
          (vm-oauth2-browse url)
          ;; it did try
          (should asked)
          ;; and the URL is there to be copied, alone on its line
          (with-current-buffer "*VM OAuth2*"
            (goto-char (point-min))
            (should (re-search-forward (concat "^" (regexp-quote url) "$")
                                       nil t))))
      (when (get-buffer "*VM OAuth2*") (kill-buffer "*VM OAuth2*")))))

(ert-deftest vm-oauth2-test-the-browser-gets-the-readers-own-setting ()
  "REGRESSION: the browser VM opens is the reader's, and VM does not recurse.

`vm-oauth2-browse' is bound over `browse-url' for the authorization and has
to put the reader's own setting back for the call inside.  Reading it out of
`browse-url-browser-function' there does not work and is not merely
imprecise: a plain `let' on a variable with no buffer-local binding rebinds
the default, so `default-value' answers `vm-oauth2-browse' and the browser
opens itself until `max-lisp-eval-depth' stops it.  Which is what the
command did."
  (let ((seen nil))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-oauth2-show-url) #'ignore))
          ;; as `vm-oauth2-authorize' sets it up
          (let* ((vm-oauth2--browser (lambda (url &rest _) (setq seen url)))
                 (vm-oauth2--handlers nil)
                 (browse-url-handlers nil)
                 (browse-url-browser-function #'vm-oauth2-browse))
            (vm-oauth2-browse "https://example.com/auth"))
          (should (equal seen "https://example.com/auth")))
      (when (get-buffer "*VM OAuth2*") (kill-buffer "*VM OAuth2*")))))

(ert-deftest vm-oauth2-test-the-authorization-binds-the-browser-safely ()
  "REGRESSION: `vm-oauth2-authorize' captures the browser before hiding it.
The whole of the recursion above was the order of two bindings, so this
drives the command itself rather than the function under it: the URL
oauth2.el asks to be opened has to reach the reader's browser exactly once."
  (skip-unless (vm-oauth2-available-p))
  (let ((opened nil)
        (vm-oauth2-client-id "id")
        (vm-oauth2-client-secret "secret"))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-oauth2-show-url) #'ignore)
                  ((symbol-function 'vm-inform) #'ignore)
                  ;; stands for oauth2.el, which would open a browser and
                  ;; then ask for a pasted code
                  ((symbol-function 'oauth2-auth-and-store)
                   (lambda (auth-url &rest _)
                     (browse-url auth-url)
                     'token))
                  ((default-value 'browse-url-browser-function)
                   (lambda (url &rest _) (push url opened))))
          (should (eq (vm-oauth2-authorize "a@b.c" "google") 'token))
          (should (equal (length opened) 1))
          (should (string-prefix-p "https://accounts.google.com/"
                                   (car opened))))
      (when (get-buffer "*VM OAuth2*") (kill-buffer "*VM OAuth2*")))))

(ert-deftest vm-oauth2-test-a-refused-authorization-is-reported ()
  "REGRESSION: a provider that refuses is an error, not a success message.

oauth2.el answers with a token structure either way.  A refusal leaves the
access token nil and the provider's words in the response it kept, and
signals nothing, so VM said the account was authorized, nothing was stored,
and the next session found no token with nothing anywhere to say why.  That
is what a reader chasing a stale code saw: a cheerful message and no mail."
  (let ((vm-oauth2-client-id "id")
        (vm-oauth2-client-secret "secret")
        (informed nil))
    (cl-letf (((symbol-function 'vm-oauth2-require) #'ignore)
              ((symbol-function 'vm-oauth2-show-url) #'ignore)
              ((symbol-function 'vm-inform)
               (lambda (_level format &rest args)
                 (push (apply #'format format args) informed)))
              ;; what oauth2.el hands back when the provider says no
              ((symbol-function 'oauth2-token-p) (lambda (_) t))
              ((symbol-function 'oauth2-token-access-token) #'ignore)
              ((symbol-function 'oauth2-token-access-response)
               (lambda (_) '((error . "invalid_grant")
                             (error_description . "Bad Request"))))
              ((symbol-function 'oauth2-auth-and-store)
               (lambda (&rest _) 'refused)))
      (let ((message (cadr (should-error
                            (vm-oauth2-authorize "a@b.c" "google")))))
        (should (string-match-p "google" message))
        (should (string-match-p "a@b.c" message))
        ;; what the provider said, and what to do about that one
        (should (string-match-p "Bad Request" message))
        (should (string-match-p "good once" message)))
      ;; and it did not say the account was authorized
      (should-not (cl-some (lambda (said) (string-match-p "authorized" said))
                           informed)))))

(provide 'vm-oauth2-test)

;;; vm-oauth2-test.el ends here
