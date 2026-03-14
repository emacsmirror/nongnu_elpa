;;; jabber-httpupload.el --- HTTP File Upload (XEP-0363)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This file implements XEP-0363: HTTP File Upload, providing a way to
;; send files through XMPP by uploading them to the server's HTTP
;; storage.  The procedure is:
;;
;; 1. Discover HTTP Upload support via Disco (urn:xmpp:http:upload:0).
;; 2. Request a slot (PUT + GET URLs) from the upload service.
;; 3. Upload the file to the PUT URL via curl.
;; 4. Send the GET URL to the recipient (with OOB metadata), or copy
;;    it to the kill ring.
;;
;; Commands:
;;   `jabber-httpupload-send-file'   - Upload and send to a contact/MUC.
;;   `jabber-httpupload-upload-file' - Upload and copy URL to kill ring.

;;; Code:

(require 'fsm)
(require 'mailcap)
(require 'jabber)
(eval-when-compile
  (require 'cl-lib))

(declare-function jabber-chat-send "jabber-chat.el"
                  (jc body &optional extra-elements))
(declare-function jabber-chat-create-buffer "jabber-chat.el" (jc chat-with))
(declare-function jabber-muc-joined-p "jabber-muc.el" (group))
(defvar jabber-oob-xmlns)              ; jabber-xml.el

(defconst jabber-httpupload-xmlns "urn:xmpp:http:upload:0"
  "XML namespace for XEP-0363 HTTP File Upload.")

(defgroup jabber-httpupload nil "Jabber HTTP Upload Settings."
  :group 'jabber)

(defcustom jabber-httpupload-upload-function #'jabber-httpupload-put-file-curl
  "Function to upload a file to the HTTP server.
Must accept (FILEPATH HEADERS PUT-URL CALLBACK CALLBACK-ARG
&optional IGNORE-CERT-PROBLEMS) and call (funcall CALLBACK
CALLBACK-ARG) on success.  Return non-nil if the upload started."
  :group 'jabber-httpupload
  :type 'function)

;; Discovering support

(defvar jabber-httpupload-support nil
  "Alist of Jabber connections and the node with HTTP Upload support.
Each element is (jabber-connection . upload-iri).")

(defun jabber-httpupload-test-all-connections-support ()
  "Test all connections in `jabber-connections' for HTTP Upload support.
Store the results in `jabber-httpupload-support'.  If the connection was
already tested and the test was successful, do not re-test it."
  (dolist (jc jabber-connections)
    (unless (jabber-httpupload-server-has-support jc)
      (jabber-httpupload-test-connection-support jc))))

(defun jabber-httpupload-test-connection-support (jc)
  "Test if HTTP Upload is supported on the JC connection's server.
If supported, store the item IRI in `jabber-httpupload-support'."
  (jabber-httpupload-apply-to-items jc
                   (lambda (jc result)
                     (jabber-httpupload-test-item-support jc (elt result 1)))))

(defun jabber-httpupload-test-item-support (jc iri)
  "Test if the IRI Disco item supports HTTP Upload.
Get the Disco Info from IRI on JC; if the HTTP Upload namespace
is present, store the IRI in `jabber-httpupload-support'."
  (jabber-disco-get-info jc iri nil
                         (lambda (jc _data result)
                           (when (member jabber-httpupload-xmlns
                                         (nth 1 result))
                             (push (cons jc iri) jabber-httpupload-support)))
                         nil))

(defun jabber-httpupload-apply-to-items (jc callback)
  "Retrieve Disco items from JC's server and call CALLBACK on each.
CALLBACK receives two arguments: the Jabber connection and the item vector."
  (let ((node (plist-get (fsm-get-state-data jc) :server)))
    (jabber-disco-get-items jc node nil
                            (lambda (jc _data result)
                              (dolist (item result)
                                (funcall callback jc item)))
                            nil)))

(defun jabber-httpupload-server-has-support (jc)
  "Return (JC . upload-iri) if the server supports HTTP Upload, nil otherwise."
  (assq jc jabber-httpupload-support))

;; Slot parsing

(defun jabber-httpupload-parse-slot-answer (xml-data)
  "Parse PUT/GET URLs from a slot response XML-DATA.
Return ((put-url . ((header-name . header-value) ...)) get-url)."
  (let ((put (jabber-xml-path xml-data '(slot put)))
        (get (jabber-xml-path xml-data '(slot get))))
    (list (cons
           (jabber-xml-get-attribute put 'url)
           (cl-loop for header in (jabber-xml-get-children put 'header)
                    for name = (jabber-xml-get-attribute header 'name)
                    when (member name '("Authorization" "Cookie" "Expires"))
                    for value = (car (jabber-xml-node-children header))
                    when value
                    collect (cons name value)))
          (jabber-xml-get-attribute get 'url))))

;; Curl upload

(defun jabber-httpupload-ignore-certificate (jc)
  "Return non-nil if JC's server is in `jabber-invalid-certificate-servers'."
  (member (plist-get (fsm-get-state-data jc) :server)
          jabber-invalid-certificate-servers))

(defun jabber-httpupload-put-file-curl (filepath headers put-url
                                        callback callback-arg
                                        &optional ignore-cert-problems)
  "Upload FILEPATH to PUT-URL via curl with HEADERS.
When done, call (funcall CALLBACK CALLBACK-ARG).
IGNORE-CERT-PROBLEMS allows connecting to servers with invalid
certificates.  Return the process on success, nil if curl is not found."
  (when-let* ((curl-path (executable-find "curl")))
    (let ((buffer (get-buffer-create "*jabber-httpupload-curl*"))
          (command
           `("--upload-file" ,filepath
             ,@(cl-loop for (name . value) in headers
                        append (list "-H" (format "%s: %s" name value)))
             ,put-url)))
      (when ignore-cert-problems
        (push "--insecure" command))
      (push curl-path command)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "%s Uploading with curl:\n%S\n"
                          (current-time-string) command))))
      (make-process :name "jabber-httpupload-curl"
                    :buffer buffer
                    :command command
                    :sentinel (lambda (process event)
                                (with-current-buffer (process-buffer process)
                                  (let ((inhibit-read-only t))
                                    (goto-char (point-max))
                                    (insert (format "Sentinel: %S\n" event))))
                                (when (string= event "finished\n")
                                  (funcall callback callback-arg)))))))

;; Core upload pipeline

(defun jabber-httpupload--upload (jc filepath callback)
  "Upload FILEPATH via HTTP Upload on JC.
On success, call (funcall CALLBACK get-url)."
  (unless (jabber-httpupload-server-has-support jc)
    (error "Server for this connection has no HTTP Upload support"))
  (let* ((filepath (expand-file-name filepath))
         (size (file-attribute-size (file-attributes filepath)))
         (content-type
          (or (and-let* ((ext (file-name-extension filepath)))
                (mailcap-extension-to-mime ext))
              "application/octet-stream"))
         (filename (file-name-nondirectory filepath)))
    (jabber-send-iq jc (cdr (jabber-httpupload-server-has-support jc)) "get"
                    `(request ((xmlns . ,jabber-httpupload-xmlns)
                               (filename . ,filename)
                               (size . ,size)
                               (content-type . ,content-type)))
                    (lambda (_jc xml-data _data)
                      (let* ((urls (jabber-httpupload-parse-slot-answer xml-data))
                             (get-url (cadr urls))
                             (put-url (caar urls))
                             (headers (cdar urls)))
                        (push (cons "content-length" size) headers)
                        (push (cons "content-type" content-type) headers)
                        (unless (funcall jabber-httpupload-upload-function
                                         filepath headers put-url
                                         callback get-url
                                         (jabber-httpupload-ignore-certificate jc))
                          (error "Upload function failed to PUT %s" filename))))
                    nil
                    (lambda (_jc xml-data _data)
                      (error "HTTP Upload slot rejected for %s: %S"
                             filename xml-data))
                    nil)))

;; Pending OOB for deferred sends (C-c C-a in chat buffers)

(defvar-local jabber-httpupload--pending-url nil
  "URL from a pending upload, awaiting send.")

(defun jabber-httpupload--send-hook (body _id)
  "Attach OOB element if BODY contains a pending upload URL.
Returns the OOB element list for `jabber-chat-send-hooks', and
clears the pending state.  If the URL is no longer in BODY (user
deleted it), the pending state is cleared with no effect."
  (when-let* ((url jabber-httpupload--pending-url))
    (setq jabber-httpupload--pending-url nil)
    (when (string-match-p (regexp-quote url) body)
      (list `(x ((xmlns . ,jabber-oob-xmlns))
                 (url () ,url))))))

(add-hook 'jabber-chat-send-hooks #'jabber-httpupload--send-hook)

;; Sending the URL

(defun jabber-httpupload--send-url (jc jid get-url)
  "Send GET-URL to JID with OOB metadata.
For groupchat, send directly.  For 1:1, use `jabber-chat-send'."
  (if (jabber-muc-joined-p jid)
      (jabber-send-sexp jc
        `(message ((to . ,jid) (type . "groupchat"))
                  (body () ,get-url)
                  (x ((xmlns . ,jabber-oob-xmlns))
                     (url () ,get-url))))
    (with-current-buffer (jabber-chat-create-buffer jc jid)
      (jabber-chat-send
       jc get-url
       (list `(x ((xmlns . ,jabber-oob-xmlns))
                  (url () ,get-url)))))))

;; Interactive commands

;;;###autoload
(defun jabber-httpupload-send-file (jc jid filepath)
  "Upload FILEPATH and send the URL to JID via JC."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Send file to: " nil nil nil 'full t)
                     (read-file-name "File to send: ")))
  (jabber-httpupload--upload
   jc filepath
   (lambda (get-url)
     (jabber-httpupload--send-url jc jid get-url))))

;;;###autoload
(defun jabber-httpupload-upload-file (jc filepath)
  "Upload FILEPATH and copy the URL to the kill ring."
  (interactive (list (jabber-read-account)
                     (read-file-name "File to upload: ")))
  (jabber-httpupload--upload
   jc filepath
   (lambda (get-url)
     (kill-new get-url)
     (message "Uploaded: %s (copied to kill ring)" get-url))))

(add-hook 'jabber-post-connect-hooks #'jabber-httpupload-test-connection-support)

(provide 'jabber-httpupload)

;;; jabber-httpupload.el ends here
