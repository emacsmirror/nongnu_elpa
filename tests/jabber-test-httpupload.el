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

;;; Service metadata

(defun jabber-test-httpupload--max-size-form (size)
  "Return an HTTP Upload disco form advertising SIZE."
  `(x ((xmlns . ,jabber-xdata-xmlns) (type . "result"))
      (field ((var . "FORM_TYPE") (type . "hidden"))
             (value () ,jabber-httpupload-xmlns))
      (field ((var . "max-file-size"))
             (value () ,size))))

(ert-deftest jabber-test-httpupload-records-max-file-size ()
  (let ((jabber-httpupload-support nil)
        (jabber-httpupload-max-file-size nil)
        (result (list nil
                      (list jabber-httpupload-xmlns)
                      (list (jabber-test-httpupload--max-size-form "512")))))
    (jabber-httpupload--record-support 'jc "upload.example.net" result)
    (should (equal jabber-httpupload-support
                   '((jc . "upload.example.net"))))
    (should (equal jabber-httpupload-max-file-size '((jc . 512))))))

(ert-deftest jabber-test-httpupload-rejects-oversized-file-before-slot-request ()
  (let ((jabber-httpupload-support '((jc . "upload.example.net")))
        (jabber-httpupload-max-file-size '((jc . 3)))
        (slot-requested nil)
        (file (make-temp-file "jabber-httpupload-test")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "1234"))
          (cl-letf (((symbol-function 'jabber-send-iq)
                     (lambda (&rest _args)
                       (setq slot-requested t))))
            (should-error
             (jabber-httpupload--upload 'jc file #'ignore)
             :type 'user-error)
            (should-not slot-requested)))
      (delete-file file))))

;;; Slot errors

(ert-deftest jabber-test-httpupload-slot-error-file-too-large ()
  (let ((xml `(iq ((type . "error"))
                  (error ((type . "modify"))
                         (file-too-large ((xmlns . ,jabber-httpupload-xmlns))
                                         (max-file-size () "20000"))))))
    (should (string= (jabber-httpupload--slot-error-message "file.jpg" xml)
                     "File file.jpg is too large for HTTP Upload (maximum 20000 bytes)"))))

(ert-deftest jabber-test-httpupload-slot-error-retry ()
  (let ((xml `(iq ((type . "error"))
                  (error ((type . "wait"))
                         (retry ((xmlns . ,jabber-httpupload-xmlns)
                                 (stamp . "2017-12-03T23:42:05Z")))))))
    (should (string= (jabber-httpupload--slot-error-message "file.jpg" xml)
                     "HTTP Upload temporarily unavailable for file.jpg; retry after 2017-12-03T23:42:05Z"))))

(ert-deftest jabber-test-httpupload-slot-error-generic-stanza-error ()
  (let ((xml `(iq ((type . "error"))
                  (error ((type . "auth"))
                         (forbidden ((xmlns . ,jabber-stanzas-xmlns)))))))
    (should (string= (jabber-httpupload--slot-error-message "file.jpg" xml)
                     "HTTP Upload slot rejected for file.jpg: Forbidden"))))

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
        (jabber-httpupload-max-file-size nil)
        (uploaded nil))
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:server "example.net")))
              ((symbol-function 'jabber-disco-get-items)
               (lambda (jc _jid _node callback closure)
                 (funcall callback jc closure items)))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (jc _jid _node callback closure)
                 (funcall callback jc closure
                          (list nil
                                (list jabber-httpupload-xmlns)
                                (list (jabber-test-httpupload--max-size-form
                                       "4096"))))))
              ((symbol-function 'jabber-httpupload--upload)
               (lambda (jc filepath callback)
                 (setq uploaded (list jc filepath callback))))
              ((symbol-function 'message) #'ignore))
      (jabber-httpupload--discover-and-upload 'jc "/tmp/file.txt" #'ignore)
      (should (equal jabber-httpupload-support
                     '((jc . "upload.example.net"))))
      (should (equal jabber-httpupload-max-file-size '((jc . 4096))))
      (should (equal uploaded
                     (list 'jc "/tmp/file.txt" #'ignore))))))

(provide 'jabber-test-httpupload)

;;; jabber-test-httpupload.el ends here
