;;; jabber-test-httpupload.el --- Tests for jabber-httpupload  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0363 HTTP File Upload.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'jabber-httpupload)

;;; Slot parsing

(ert-deftest jabber-test-httpupload-parse-slot-answer ()
  (let* ((slot `(iq ()
                    (slot ((xmlns . ,jabber-httpupload-xmlns))
                          (put ((xmlns . ,jabber-httpupload-xmlns)
                                (url . "https://upload.example.net/file"))
                               (header ((name . "Authorization"))
                                       "Bearer token"))
                          (get ((xmlns . ,jabber-httpupload-xmlns)
                                (url . "https://download.example.net/file"))))))
         (result (jabber-httpupload-parse-slot-answer slot)))
    (should (equal result
                   '(("https://upload.example.net/file"
                      ("Authorization" . "Bearer token"))
                     "https://download.example.net/file")))))

(ert-deftest jabber-test-httpupload-parse-slot-rejects-wrong-namespace ()
  (let ((slot '(iq ()
                   (slot ((xmlns . "urn:xmpp:other"))
                         (put ((xmlns . "urn:xmpp:other")
                               (url . "https://upload.example.net/file")))
                         (get ((xmlns . "urn:xmpp:other")
                               (url . "https://download.example.net/file")))))))
    (should-error (jabber-httpupload-parse-slot-answer slot) :type 'error)))

;;; Discovery

(ert-deftest jabber-test-httpupload-discover-errors-with-no-items ()
  (cl-letf (((symbol-function 'fsm-get-state-data)
             (lambda (_jc) '(:server "example.net")))
            ((symbol-function 'jabber-disco-get-items)
             (lambda (jc _jid _node callback closure)
               (funcall callback jc closure nil)))
            ((symbol-function 'message) #'ignore))
    (should-error
     (jabber-httpupload--discover-and-upload 'jc "/tmp/file.txt" #'ignore)
     :type 'user-error)))

(ert-deftest jabber-test-httpupload-discover-errors-without-feature ()
  (let ((items (list ["Archive" "archive.example.net" nil]
                     ["Proxy" "proxy.example.net" nil])))
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:server "example.net")))
              ((symbol-function 'jabber-disco-get-items)
               (lambda (jc _jid _node callback closure)
                 (funcall callback jc closure items)))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (jc jid _node callback closure)
                 (funcall callback jc closure
                          (list nil (list (format "feature:%s" jid))))))
              ((symbol-function 'message) #'ignore))
      (should-error
       (jabber-httpupload--discover-and-upload 'jc "/tmp/file.txt" #'ignore)
       :type 'user-error))))

(ert-deftest jabber-test-httpupload-discover-uploads-with-feature ()
  (let ((items (list ["Upload" "upload.example.net" nil]))
        (jabber-httpupload-support nil)
        (uploaded nil))
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:server "example.net")))
              ((symbol-function 'jabber-disco-get-items)
               (lambda (jc _jid _node callback closure)
                 (funcall callback jc closure items)))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (jc _jid _node callback closure)
                 (funcall callback jc closure
                          (list nil (list jabber-httpupload-xmlns)))))
              ((symbol-function 'jabber-httpupload--upload)
               (lambda (jc filepath callback)
                 (setq uploaded (list jc filepath callback))))
              ((symbol-function 'message) #'ignore))
      (jabber-httpupload--discover-and-upload 'jc "/tmp/file.txt" #'ignore)
      (should (equal jabber-httpupload-support
                     '((jc . "upload.example.net"))))
      (should (equal uploaded
                     (list 'jc "/tmp/file.txt" #'ignore))))))

(provide 'jabber-test-httpupload)

;;; jabber-test-httpupload.el ends here
