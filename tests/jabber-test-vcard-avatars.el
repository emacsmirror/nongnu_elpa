;;; jabber-test-vcard-avatars.el --- Tests for vCard avatars  -*- lexical-binding: t; -*-

;;; Commentary:

;; vCard avatar presence publishing.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'jabber-vcard-avatars)
(require 'jabber-xml)

;;; Group 1: presence elements

(ert-deftest jabber-test-vcard-avatars-presence-no-hash-omits-element ()
  "Publishing with no known avatar hash serializes no nil elements."
  (let ((jabber-vcard-avatars-publish t)
        (jabber-vcard-avatars-current-hash (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "alice@example.com")))
      (should-not (jabber-vcard-avatars-presence-element nil))
      (should-not (string-match-p
                   "<nil"
                   (jabber-sexp2xml
                    `(presence ()
                               ,@(jabber-vcard-avatars-presence-element nil))))))))

(provide 'jabber-test-vcard-avatars)
;;; jabber-test-vcard-avatars.el ends here
