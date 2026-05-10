;;; jabber-openpgp-legacy-tests.el --- Tests for XEP-0027 legacy OpenPGP  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-xml)

;; Load the module under test.  It has top-level side effects that need
;; jabber-disco and jabber-chat, so stub what is missing.
(unless (fboundp 'jabber-disco-advertise-feature)
  (defun jabber-disco-advertise-feature (_feature) nil))
(unless (fboundp 'jabber-chat-register-decrypt-handler)
  (defun jabber-chat-register-decrypt-handler (_id &rest _props) nil))
(unless (fboundp 'jabber-chat--set-body)
  (defun jabber-chat--set-body (xml-data text)
    (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
      (if body-el
          (setcar (cddr body-el) text)
        (nconc xml-data (list `(body () ,text)))))
    xml-data))
(defvar jabber-presence-element-functions nil)
(defvar jabber-presence-chain nil)

(require 'jabber-openpgp-legacy)

;;; Group 1: detect-encrypted

(ert-deftest jabber-openpgp-legacy-test-detect-encrypted-returns-stripped ()
  "detect-encrypted returns stripped armor text for XEP-0027 stanza."
  (let ((xml-data `(message ((from . "alice@example.com/res")
                             (type . "chat"))
                            (body () "This message is encrypted.")
                            (x ((xmlns . "jabber:x:encrypted"))
                               "hQEOA7Le..."))))
    (should (string= "hQEOA7Le..."
                      (jabber-openpgp-legacy--detect-encrypted xml-data)))))

(ert-deftest jabber-openpgp-legacy-test-detect-encrypted-returns-nil ()
  "detect-encrypted returns nil for plain stanza."
  (let ((xml-data '(message ((from . "alice@example.com")
                             (type . "chat"))
                            (body () "hello plain"))))
    (should-not (jabber-openpgp-legacy--detect-encrypted xml-data))))

(ert-deftest jabber-openpgp-legacy-test-detect-encrypted-non-string-child ()
  "detect-encrypted returns nil when x element child is not a string."
  (let ((xml-data '(message ((from . "alice@example.com"))
                            (x ((xmlns . "jabber:x:encrypted"))
                               (inner ())))))
    (should-not (jabber-openpgp-legacy--detect-encrypted xml-data))))

(provide 'jabber-openpgp-legacy-tests)
;;; jabber-openpgp-legacy-tests.el ends here
