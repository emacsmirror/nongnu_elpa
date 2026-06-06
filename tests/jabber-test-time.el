;;; jabber-test-time.el --- Tests for jabber-time  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0012, XEP-0090, and XEP-0202 time handling.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'jabber-time)

;;; Last activity

(ert-deftest jabber-test-time-return-last-sends-query ()
  (let ((sent nil)
        (iq '(iq ((from . "romeo@example.net/orchard")
                  (id . "last1"))
		 (query ((xmlns . "jabber:iq:last"))))))
    (cl-letf (((symbol-function 'jabber-autoaway-get-idle-time)
               (lambda () 42.7))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq sent args))))
      (jabber-return-last 'jc iq)
      (should (equal sent
                     (list 'jc
                           "romeo@example.net/orchard"
                           "result"
                           '(query ((xmlns . "jabber:iq:last")
                                    (seconds . "42")))
                           nil nil nil nil
                           "last1"))))))

(provide 'jabber-test-time)

;;; jabber-test-time.el ends here
