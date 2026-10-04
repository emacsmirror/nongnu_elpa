;;; vm-renamed.el --- Names VM renamed, and what they are now  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Every name a configuration could carry that VM has renamed, with the name
;; to use instead.  The old name is not an alias.  It signals, and the signal
;; says what to write:
;;
;;     vmpc-conditions was renamed to vm-pcrisis-conditions in VM 9.0.0;
;;     rename it in your configuration
;;
;; An alias was the obvious thing and it does not work.  `define-obsolete-
;; variable-alias' warns when the file naming the old name is byte-compiled,
;; and nobody byte-compiles ~/.vm or an init file, so a reader who kept the
;; old name was told nothing at all and their configuration went on working
;; until the alias was removed.  Nineteen of these were a bare `defvaralias'
;; and could not warn even then.  Grace that says nothing is not a transition;
;; it moves the same surprise to the next release.
;;
;; A function takes a stub that signals.  A variable takes a watcher, which
;; fires when a configuration sets it.  The old name is left unbound, so
;; reading it signals `void-variable' rather than answering nil as an alias to
;; an unset variable would.
;;
;; Delete this file when the grace period ends.  Nothing else refers to these
;; names: that is the point of keeping them in one place.

;;; Code:

(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(defconst vm-renamed-in "9.0.0"
  "The release these names were renamed in, as the signal reports it.")

(defconst vm-renamed-variables
  '(
    (vmpc-conditions                                . vm-pcrisis-conditions)
    (vmpc-actions                                   . vm-pcrisis-actions)
    (vmpc-default-rules                             . vm-pcrisis-default-rules)
    (vmpc-actions-alist                             . vm-pcrisis-default-rules)
    (vmpc-reply-rules                               . vm-pcrisis-reply-rules)
    (vmpc-reply-alist                               . vm-pcrisis-reply-rules)
    (vmpc-forward-rules                             . vm-pcrisis-forward-rules)
    (vmpc-forward-alist                             . vm-pcrisis-forward-rules)
    (vmpc-automorph-rules                           . vm-pcrisis-automorph-rules)
    (vmpc-automorph-alist                           . vm-pcrisis-automorph-rules)
    (vmpc-mail-rules                                . vm-pcrisis-mail-rules)
    (vmpc-mail-alist                                . vm-pcrisis-mail-rules)
    (vmpc-newmail-rules                             . vm-pcrisis-newmail-rules)
    (vmpc-newmail-alist                             . vm-pcrisis-newmail-rules)
    (vmpc-resend-rules                              . vm-pcrisis-resend-rules)
    (vmpc-resend-alist                              . vm-pcrisis-resend-rules)
    (vmpc-default-profile                           . vm-pcrisis-default-profile)
    (vmpc-auto-profiles-file                        . vm-pcrisis-auto-profiles-file)
    (vmpc-auto-profiles-expunge-days                . vm-pcrisis-auto-profiles-expunge-days)
    (vmpc-current-state                             . vm-pcrisis-current-state)
    (vmpc-current-buffer                            . vm-pcrisis-current-buffer)
    (vmpc-actions-to-run                            . vm-pcrisis-actions-to-run)
    (vmpc-expect-default-signature                  . vm-pcrisis-expect-default-signature)
    (vmpc-prompt-for-profile-headers                . vm-pcrisis-prompt-for-profile-headers)
    (vm-fetched-message-limit                       . vm-external-fetched-message-limit)
    (vm-trust-From_-with-Content-Length             . vm-trust-content-length)
    (vm-honor-mime-content-disposition              . vm-mime-honor-content-disposition)
    (vm-auto-displayed-mime-content-types           . vm-mime-auto-displayed-content-types)
    (vm-auto-displayed-mime-content-type-exceptions . vm-mime-auto-displayed-content-type-exceptions)
    (vm-mime-savable-types                          . vm-mime-saveable-types)
    (vm-mime-savable-type-exceptions                . vm-mime-saveable-type-exceptions)
    (vm-summary-uninteresting-senders-arrow         . vm-summary-recipient-marker)
    (vm-mutable-windows                             . vm-mutable-window-configuration)
    (vm-mutable-frames                              . vm-mutable-frame-configuration)
    (vm-vs-spam-score-headers                       . vm-spam-score-headers)
    (vm-supported-interactive-virtual-selectors     . vm-vs-interactive)
    (vm-virtual-selector-function-alist             . vm-vs-alist))
  "Renamed variables, as (OLD . NEW).
Setting OLD signals and names NEW.  See the Commentary.")

(defconst vm-renamed-functions
  '(
    (vmpc-virtual-check-selector          . vm-pcrisis-virtual-check-selector)
    (vmpc-add-header                      . vm-pcrisis-add-header)
    (vmpc-automorph                       . vm-pcrisis-automorph)
    (vmpc-backward-tab-header-or-tab-stop . vm-pcrisis-backward-tab-header-or-tab-stop)
    (vmpc-body-match                      . vm-pcrisis-body-match)
    (vmpc-build-actions-to-run-list       . vm-pcrisis-build-actions-to-run-list)
    (vmpc-build-true-conditions-list      . vm-pcrisis-build-true-conditions-list)
    (vmpc-composition-buffer              . vm-pcrisis-composition-buffer)
    (vmpc-delete-header                   . vm-pcrisis-delete-header)
    (vmpc-fix-auto-profiles-file          . vm-pcrisis-fix-auto-profiles-file)
    (vmpc-folder-account-match            . vm-pcrisis-folder-account-match)
    (vmpc-folder-match                    . vm-pcrisis-folder-match)
    (vmpc-header-match                    . vm-pcrisis-header-match)
    (vmpc-insert-header                   . vm-pcrisis-insert-header)
    (vmpc-load-auto-profiles              . vm-pcrisis-load-auto-profiles)
    (vmpc-migrate-profiles-to-BBDB        . vm-pcrisis-migrate-profiles-to-BBDB)
    (vmpc-mode                            . vm-pcrisis-mode)
    (vmpc-my-identities                   . vm-pcrisis-my-identities)
    (vmpc-none-true-yet                   . vm-pcrisis-none-true-yet)
    (vmpc-only-from-match                 . vm-pcrisis-only-from-match)
    (vmpc-other-cond                      . vm-pcrisis-other-cond)
    (vmpc-pre-function                    . vm-pcrisis-pre-function)
    (vmpc-pre-signature                   . vm-pcrisis-pre-signature)
    (vmpc-prompt-for-profile              . vm-pcrisis-prompt-for-profile)
    (vmpc-read-actions                    . vm-pcrisis-read-actions)
    (vmpc-run-action                      . vm-pcrisis-run-action)
    (vmpc-run-actions                     . vm-pcrisis-run-actions)
    (vmpc-signature                       . vm-pcrisis-signature)
    (vmpc-substitute-header               . vm-pcrisis-substitute-header)
    (vmpc-substitute-replied-header       . vm-pcrisis-substitute-replied-header)
    (vmpc-tab-header-or-tab-stop          . vm-pcrisis-tab-header-or-tab-stop)
    (vmpc-toggle-no-automorph             . vm-pcrisis-toggle-no-automorph)
    (vmpc-true-conditions                 . vm-pcrisis-true-conditions))
  "Renamed functions, as (OLD . NEW).
Calling OLD signals and names NEW.  Several of these are Personality Crisis
conditions and actions, which a configuration names as data in
`vm-pcrisis-conditions' and `vm-pcrisis-actions' rather than calling.")

(defun vm-renamed-explanation (old new)
  "What to tell a reader who has used OLD instead of NEW."
  (format "%s was renamed to %s in VM %s; rename it in your configuration"
          old new vm-renamed-in))

(defun vm-renamed-note-a-variable (old new)
  "Make setting OLD signal, naming NEW.
A value already there is a configuration read before VM was loaded.  That one
is reported rather than signalled: VM is loading, and an error here stops it."
  (when (boundp old)
    ;; `display-warning' and not `vm-warn': this file is loaded from
    ;; vm-vars.el, which vm-misc.el requires, so vm-warn is not defined yet.
    ;; A warning buffer is the right place for it anyway, an init file being
    ;; where this value came from.
    (display-warning 'vm (vm-renamed-explanation old new) :warning))
  (add-variable-watcher
   old
   (lambda (symbol _value operation _where)
     (when (memq operation '(set let))
       (error "%s" (vm-renamed-explanation symbol new))))))

(defun vm-renamed-note-a-function (old new)
  "Make calling OLD signal, naming NEW."
  (defalias old
    (lambda (&rest _)
      (error "%s" (vm-renamed-explanation old new)))
    (vm-renamed-explanation old new)))

(defun vm-renamed-note-them-all ()
  "Install every renamed name.  Called as this file loads."
  (dolist (pair vm-renamed-variables)
    (vm-renamed-note-a-variable (car pair) (cdr pair)))
  (dolist (pair vm-renamed-functions)
    (vm-renamed-note-a-function (car pair) (cdr pair))))

(vm-renamed-note-them-all)

(provide 'vm-renamed)
;;; vm-renamed.el ends here
