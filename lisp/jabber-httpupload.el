;;; jabber-httpupload.el --- Emacs Jabber HTTP Upload Implementation -*- lexical-binding: t; -*-

;; Copyright 2021 cnngimenez
;;
;; Author: cnngimenez
;; Maintainer: cnngimenez
;; Version: 0.1.0
;; Keywords: comm
;; URL: https://github.com/cnngimenez/emacs-jabber

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file implements XEP-0363: HTTP Upload
;; (https://xmpp.org/extensions/xep-0363.html), providing a way to
;; send files (images, audio, etc) through XMPP clients by using
;; server space, and the HTTP protocol to upload and download from it.
;; The advantage is that the sender user does not need to be connected
;; after sharing the file, and the receiver may be disconnected while
;; the sender is uploading.

;; The procedure to send a file is as follows -

;; 1. Use Disco queries to discover if the server supports the HTTP Upload (~urn:xmpp:http:upload~ namespace).
;; 2. Request a slot to the upload Disco item. The server will answer with a GET and PUT URL.
;; 3. Upload the file to the HTTP server by using the PUT URL.
;; 4. Usually, send the GET URL to the other XMPP clients to allow them to access the uploaded file.
;;
;; TODO -
;; 1. Use wget to send the file
;; 2. Recording audio and sending

;;; Code:

(require 'seq)
(require 'fsm)
(require 'mailcap)
(require 'jabber)
(eval-when-compile
  (require 'cl-lib))

;; * Configuration variables *

(defgroup jabber-httpupload nil "Jabber HTTP Upload Settings."
  :group 'jabber)

(defcustom jabber-httpupload-upload-function #'jabber-httpupload-put-file-curl
  "The function used to upload the file.
Some functions calls external programs such as Curl and wget,
please check their documentation for more information."
  :group 'jabber-httpupload
  :type 'function)

;; TODO Recording and sending audio
;; (defcustom jabber-httpupload-record-command "sox -d -t ogg $(filename).ogg"
;;   "What is the command used to record audio?
;; Use $(filename) where the temporal filename should be."
;;   :group 'jabber-httpupload
;;   :type 'function)

;; Disco is used to discover if HTTP Upload is supported on the server
;; side. Two queries are used:

;; 1. An IQ Disco items request to get all items supported by the
;;    server.
;; 2. For each item, an IQ Disco info request to test if the item is
;;    the Upload service.

;; The namespace of the HTTP Upload feature is
;; "urn:xmpp:http:upload:0". This will be used on the second query to
;; detect which item is the upload service.

;; For more information, see XML examples at the
;; [[https://xmpp.org/extensions/xep-0363.html#disco][Discovering
;; Support section of XEP-0363]].

;; This implementation requires an initialization step to fill the
;; `jabber-httpupload-support' variable. This variable registers all
;; connections with their HTTP Upload item. If one of the server
;; associated to a connection does not support HTTP Upload, then it
;; will be registered with a `nil' item.

;; * Discovering support *

(defvar jabber-httpupload-support nil
  "Alist of Jabber connections and the node with HTTP Upload support.
This is filled by the `jabber-httpupload-test-all-connections-suport'.
Each element are of the form (jabber-connection . string/nil).  If the value is
a string, it is the upload item IRI, if nil means no support.")

(defun jabber-httpupload-test-all-connections-support ()
  "Test all connections in `jabber-connections' for HTTP Upload support.
Store the results at `jabber-httpupload-support'.
If the connection is already tested, ignore it."
  (let ((connections (seq-difference jabber-connections
                                     (mapcar #'car jabber-httpupload-support))))
    (dolist (jc connections)
      (jabber-httpupload-test-connection-support jc))))

(defun jabber-httpupload-test-connection-support (jc)
  "Test if HTTP Upload is supported on the JC connection's server.
If it is supported, store the item IRI at `jabber-httpupload-support'.

This function is asynchronous, thus it won't return any results."
  (jabber-httpupload-apply-to-items jc
                   (lambda (jc result)
                     (jabber-httpupload-test-item-support jc (elt result 1)))))

;; CALLBACK receives three arguments: the jabber connection, extra
;; data and the query result. The result is a list of features
;; supported by the server. For example, if the client receives the
;; following IQ answer:
;;
;; <iq from="upload.server.org"
;;     type="result"
;;     to="myjid@server.org/pc1"
;;     id="emacs-iq-24678.666.622936">
;;   <query xmlns="http://jabber.org/protocol/disco#info">
;;     <identity name="HTTP File Upload" type="file" category="store"/>
;;     <feature var="http://jabber.org/protocol/disco#info"/>
;;     <feature var="http://jabber.org/protocol/disco#items"/>
;;     <feature var="urn:xmpp:http:upload:0"/>
;;     <feature var="urn:xmpp:http:upload"/>
;;     <x xmlns="jabber:x:data" type="result">
;;       <field type="hidden" var="FORM_TYPE">
;;         <value>urn:xmpp:http:upload:0</value>
;;       </field>
;;       <field type="text-single" var="max-file-size">
;;         <value>500000</value>
;;       </field>
;;     </x>
;;     <x xmlns="jabber:x:data" type="result">
;;       <field type="hidden" var="FORM_TYPE">
;;         <value>urn:xmpp:http:upload</value>
;;       </field>
;;       <field type="text-single" var="max-file-size">
;;         <value>500000</value>
;;       </field>
;;     </x>
;;   </query>
;; </iq>
;;
;; The result would be:
;;
;; ((["HTTP File Upload" "store" "file"])
;;  ("http://jabber.org/protocol/disco#info"
;;   "http://jabber.org/protocol/disco#items"
;;   "urn:xmpp:http:upload:0"
;;   "urn:xmpp:http:upload"))
;;
;; This Disco item supports HTTP Upload because the
;; "urn:xmpp:http:upload:0" namespace is in the second list.

(defun jabber-httpupload-test-item-support (jc iri)
  "Test if the IRI Disco item supports HTTP Upload.
Get the Disco Info from the provided IRI at the current JC jabber connection,
if the HTTP Upload namespace feature is in the answer, store the IRI
in `jabber-httpupload-support'."
  (jabber-disco-get-info jc iri nil
                         (lambda (jc _data result)
                           (when (member "urn:xmpp:http:upload:0"
                                         (nth 1 result))
                             ;; This item supports HTTP Upload... register it!
                             (push (cons jc iri) jabber-httpupload-support)))
                         nil))

;; CALLBACK receives three arguments: the jabber connection, extra
;; data and the query result. The result is a list of vector with the
;; node name, its IRI and any other properties.
;;
;; For example, if the client receives the following XML:
;;
;;   <iq from="server.org" type="result" to="myjid@server.org/pc1" id="emacs-iq-24677.56646.166389">
;;     <query xmlns="http://jabber.org/protocol/disco#items">
;;       <item jid="conference.server.org" name="MUC chats!"/>
;;       <item jid="upload.server.org"/>
;;     </query>
;;   </iq>
;;
;; The result would be:

;;  (["MUC chats!" "conference.server.org" nil] [nil "upload.server.org" nil])

(defun jabber-httpupload-apply-to-items (jc callback)
  "Retrieve al Disco IRIs from the server connected in JC.
Return a list of IRI strings.

JC is a jabber connection.
CALLBACK is a function that receives two arguments: Jabber connection and
the item vector."
  (let ((node (plist-get (fsm-get-state-data jc) :server)))
    (jabber-disco-get-items jc node nil
                            (lambda (jc _data result)
                              (dolist (item result)
		                (message "item: %S" item)
                                (funcall callback jc item)))
                            nil)))

(defun jabber-httpupload-server-has-support (jc)
  "Check if the server has HTTP Upload support.
Return the tuple (jabber-connection . upload-url) when there is support from
the server.  Return nil when the server does not support HTTP Upload.

If the server is not in `jabber-httpupload-support', then it is considered as
it is not supported.  It SHOULD be tested on-line with
`jabber-httpupload-test-connection-support' as soon as the connection and
authentication is established.

JC is the Jabber Connection to use."

  (seq-find (lambda (tuple)
              (and (equal jc (car tuple))
                   (cdr tuple)))
            jabber-httpupload-support))

;; * Requesting a slot *

;; The XEP specifies that the client must ask for a "slot" before
;; uploading the file to the server. The slot is a fresh URL that will
;; be enabled for the client to upload the file. The server may give
;; two URLs in one slot query: the uploading URL and the GET URL to
;; share.

;; The server may limit the file size to upload.

;; <iq from='upload.montague.tld'
;;     id='step_03'
;;     to='romeo@montague.tld/garden'
;;     type='result'>
;;   <slot xmlns='urn:xmpp:http:upload:0'>
;;     <put url='https://upload.montague.tld/4a771ac1-f0b2-4a4a-9700-f2a26fa2bb67/tr%C3%A8s%20cool.jpg'>
;;       <header name='Authorization'>Basic Base64String==</header>
;;       <header name='Cookie'>foo=bar; user=romeo</header>
;;     </put>
;;     <get url='https://download.montague.tld/4a771ac1-f0b2-4a4a-9700-f2a26fa2bb67/tr%C3%A8s%20cool.jpg' />
;;   </slot>
;; </iq>

(defun jabber-httpupload-parse-slot-answer (xml-data)
  "Retrieve the slot data from the XML-DATA information.
The XML-DATA is the stanza receive from the Jabber Connection after requesting
the slot for a file.
The returned list contains a cons pair of the PUT URL and PUT headers as
the first element and the GET URL as the second element.  PUT headers
are cons pairs containing the header name and header value."
  (let ((put (jabber-xml-path xml-data '(slot put)))
        (get (jabber-xml-path xml-data '(slot get))))
    (list (cons
           ;; PUT URL.
           (jabber-xml-get-attribute put 'url)
           ;; PUT headers.
           (cl-loop for header in (jabber-xml-get-children put 'header)
                    for name = (jabber-xml-get-attribute header 'name)
                    when (member name '("Authorization" "Cookie" "Expires"))
                    for value = (car (jabber-xml-node-children header))
                    when value
                    collect (cons name value)))
          ;; GET URL.
          (jabber-xml-get-attribute get 'url))))

(defun jabber-httpupload--request-slot-successful (jc xml-data data)
  "Callback function used when the slot request succeeded.
XML-DATA is the received XML from the server.
DATA is a triple (filedata success-callback success-args) where:
  FILEDATA is a triple (filename size content-type)
  SUCCESS-CALLBACK is a function to call after parsing and requesting the
    upload.
  It should accept following arguments: JC XML-DATA FILEDATA PUT-GET-URLS
    and SUCCESS-ARGS.
  SUCCESS-ARGS is a list to pass to the SUCCESS-CALLBACK."
  (let ((urls (jabber-httpupload-parse-slot-answer xml-data))
        (filedata (car data))
        (success-callback (nth 1 data))
        (success-args (nth 2 data)))
    (funcall success-callback jc xml-data filedata urls success-args)))

;; Maybe this function should be added as lambda inside the jabber-httpupload-request-slot...
(defun jabber-httpupload--request-slot-failed (jc xml-data data)
  "Callback function used when the slot request failed.

DATA is a list (filedata error-callback error-args) where:
  FILEDATA is a triple (filename size content-type)
  ERROR-CALLBACK is a function to call.  If no error-callback is provided, then
  `error' is used.  Its arguments are JC XML-DATA FILEDATA ERROR-ARGS.
  ERROR-ARGS is list passed to the ERROR-CALLBACK."
  (let ((filedata (car data))
        (error-callback (nth 1 data))
        (error-args (nth 2 data)))
    (if error-callback
        (funcall error-callback jc xml-data filedata error-args)
      (error (format "The file %s cannot be uploaded: SLOT rejected. %S"
                     (car data) xml-data)))))

;; The XML used to request a slot is similar to the following -
;; <iq from='romeo@montague.tld/garden'
;;     id='step_03'
;;     to='upload.montague.tld'
;;     type='get'>
;;   <request xmlns='urn:xmpp:http:upload:0'
;;     filename='très cool.jpg'
;;     size='23456'
;;     content-type='image/jpeg' />
;; </iq>

(defun jabber-httpupload-request-slot (jc filedata success-callback success-args
                         &optional error-callback error-args)
  "Request a slot for HTTP Upload to the server's connection.
JC is an active Jabber Connection.
FILEDATA is a list with (filename size content-type).
SUCCESS-CALLBACK is a function name to call when the slot is received.  Its
  arguments should be: jc xml-data data and put-get-URLs.
SUCCESS-ARGS is a list of arguments used by the SUCCESS-CALLBACK
ERROR-CALLBACK is a function to call on failure.  Its arguments should be:
  jc xml-data.
ERROR-ARGS is a list with arguments for ERROR-CALLBACK."
  (let ((filename (file-name-nondirectory (car filedata)))
        (size (nth 1 filedata))
        (content-type (nth 2 filedata)))
    (jabber-send-iq jc (cdr (jabber-httpupload-server-has-support jc)) "get"
                    `(request ((xmlns . "urn:xmpp:http:upload:0")
                               (filename . ,filename)
                               (size . ,size)
                               (content-type . ,content-type)))
                    #'jabber-httpupload--request-slot-successful
                    (list filedata success-callback success-args)
                    #'jabber-httpupload--request-slot-failed
                    (list filedata error-callback error-args))))

;; * Uploading the file *

;; Use the HTTP protocol to upload the file to the PUT URL provided by
;; the slot.

;; The following functions call the upload programs asynchronously.
;; When the program ends, a callback function is called with one
;; argument provided by the caller function.

;; The uploading process supports multiple calls. For example, the
;; user may call `jabber-httpupload-send-file' again while the upload process of a
;; previous `jabber-httpupload-send-file' call is still running.

;; Also, a callback can be provided in order to send the URL to the
;; receiving Jabber client or to perform any other action after
;; uploading the file.

(defun jabber-httpupload-ignore-certificate (jc)
  "Should the SSL/TLS certificates be ignore from JC connection?
Check if JC URL is in the variable `jabber-invalid-certificate-servers', if it
is the XMPP and HTTPs connection should be established regarding their
certificate validation status."
  (member (plist-get (fsm-get-state-data jc) :server)
          jabber-invalid-certificate-servers))

(defun jabber-httpupload-upload-file (filepath headers put-url
                              callback callback-arg
                              &optional ignore-cert-problems)
  "Update the given file at FILEPATH to PUT-URL using HEADERS.
The content-type (MIME type) in HEADERS must match the one provided to
the Jabber Connection with `jabber-httpupload-request-slot'.
IGNORE-CERT-PROBLEMS allows to connect with HTTPS servers with invalid or
non-trusted SSL/TLS certificates.
When the process ends, a callback function is called using the following
code: (funcall CALLBACK CALLBACK-ARG)"
  (unless (funcall jabber-httpupload-upload-function filepath headers put-url
                   callback callback-arg
                   ignore-cert-problems)
    (error (concat "The upload function failed to PUT the file to the server. "
                   "Try other function or install the required program"))))

;; Multiple files can be uploaded in parallel, and thus multiple
;; subprocess could be working at the same time. This happens when the
;; user calls interactively `jabber-httpupload-send-file' twice or while a file is
;; still uploading.

;; This variable keeps track of the subprocesses and their callback
;; along with any data required by these functions.

(defvar jabber-httpupload-upload-processes nil
  "Alist of running processes uploading the file to the server.
List of running processes uploading the file to the server
associated with their callback and arguments. Each element has
the following format: (process . (callback arg))")

;; When the file has been uploaded, the process is still registered
;; with its callback function. This callback should be called and the
;; process deleted from the system.

(defun jabber-httpupload-process-ended (process)
  "What to do when an upload process ends.
PROCESS is the process that ended.
First remove the process from `jabber-httpupload-upload-processes',
then call its callback with the provided argument."
  (let* ((data (assq process jabber-httpupload-upload-processes))
         (callback (cadr data))
         (callback-arg (caddr data)))
    (setq jabber-httpupload-upload-processes
          (assq-delete-all process jabber-httpupload-upload-processes))
    (funcall callback callback-arg)))

;; Using CURL to send the file

;; These functions call curl to send the file to the server. A
;; sentinel is required to check when the subprocess finishes to call
;; the next function (usually, send the URL to the other jabber
;; client).

(defun jabber-httpupload-curl-sentinel (process event)
  "Detect when Curl ends and act accordingly.
PROCESS is the asynchronous Curl call.
EVENT is a string describing the reason the sentinel were called.

When EVENT is \"finished\n\", then the function
`jabber-httpupload-process-ended' is called."
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "Sentinel: %S event received." event))))
  (when (string= event "finished\n")
    (jabber-httpupload-process-ended process)))

;; This is the function used to send a file to the server by running a curl subprocess.
(defun jabber-httpupload-put-file-curl (filepath headers put-url
                                callback callback-arg
                                &optional ignore-cert-problems)
  "Use Curl to upload the file at FILEPATH to PUT-URL.
The PUT request includes all HEADERS in their current order.
IGNORE-CERT-PROBLEMS enable the use of HTTPS connections with invalid or
non-trusted SSL/TLS certificates.  If nil, curl will validate the certificate
provided by the HTTP/S Web server.
When the process ends, the function CALLBACK is called like the following
call: (funcall CALLBACK CALLBACK-ARG).
The process is registered at `jabber-httpupload-upload-processes' AList with
the provided CALLBACK and CALLBACK-ARG."
  (let* ((exec-path (executable-find "curl"))
         (header-args
          (mapconcat
           (pcase-lambda (`(,name . ,value))
             (concat "-H " (shell-quote-argument (format "%s: %s" name value))))
           headers
           " "))
         (cmd (format "%s %s --upload-file '%s' %s '%s'"
                      exec-path
                      (if ignore-cert-problems
                          "--insecure"
                        "")
                      filepath header-args put-url)))
    (when exec-path
      (with-current-buffer (get-buffer-create "*jabber-httpupload-put-file-curl*")
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format  "%s Uploading to %s with curl:\n$ %s"
                           (current-time-string)
                           put-url
                           cmd))
          (let ((process (start-process-shell-command "jabber-httpupload-put-file-curl"
                                                      (current-buffer)
                                                      cmd)))
            (push (cons process (list callback callback-arg))
                  jabber-httpupload-upload-processes)
            (set-process-sentinel process #'jabber-httpupload-curl-sentinel))
          (insert "-- done --")
          t)))))

;; * Send the file URL to the client *

;; The following message is similar to one sent by Conversations -
;;
;; <message from="from_jid@fromserver.org/Resource"
;;          id="fc824dcb-c654-4911-a22b-25718dfe4590"
;;          type="chat"
;;          to="to_jid@toserver.org">
;;   <body>https://fromserver.org:5281/upload/kFTT5ET9JeF_CC6s/_IJNy8ZUSRGiKyVxjf5FkA.jpg</body>
;;   <request xmlns="urn:xmpp:receipts"/>
;;   <markable xmlns="urn:xmpp:chat-markers:0"/>
;;   <origin-id id="fc824dcb-c654-4911-a22b-25718dfe4590" xmlns="urn:xmpp:sid:0"/>
;;   <x xmlns="jabber:x:oob">
;;     <url>https://fromserver.org:5281/upload/kFTT5ET9JeF_CC6s/_IJNy8ZUSRGiKyVxjf5FkA.jpg</url>
;;   </x>
;;   <stanza-id xmlns="urn:xmpp:sid:0"
;;              id="7e18d73a-278c-4e5e-bd09-61c12187e5d6"
;;              by="to_jid@toserver.org"/>
;; </message>

;; The message should add the "body" and "x" tags.

(defun jabber-httpupload-send-file-url (jc jid get-url)
  "Send the GET URL address to the JID user.
The message requiers the GET-URL of the slot file, the receiver's JID
and the JC Jabber Connection."
  ;; This could be a possibliity, but... cannot send the x tag.
  ;; (jabber-send-message jc jid nil get-url nil)
  (let* ((id (apply #'format "emacs-msg-%d.%d.%d" (current-time)))
	 (fromjid (jabber-connection-original-jid jc))
         (type (if (assoc jid *jabber-active-groupchats*)
                   "groupchat"
                 "chat"))
	 (body get-url)
	 (stanza-to-send `(message ((to . ,jid)
                                    (from . ,fromjid)
                                    (type . ,type)
				    (id . ,id))
                                   (body () ,body)
                                   (x ((xmlns . "jabber:x:oob"))
                                      (url () ,body)))))
    (unless (equal type "groupchat")
          (dolist (hook jabber-chat-send-hooks)
      (if (eq hook t)
	  ;; Local hook referring to global...
	  (when (local-variable-p 'jabber-chat-send-hooks)
	    (dolist (global-hook (default-value 'jabber-chat-send-hooks))
	      (nconc stanza-to-send (funcall global-hook body id))))
	(nconc stanza-to-send (funcall hook body id))))
	  (jabber-maybe-print-rare-time
	   (ewoc-enter-last jabber-chat-ewoc (list :local stanza-to-send :time (current-time)))))
    ;; ...and send it...
    (jabber-send-sexp jc stanza-to-send)))

;; * Chat buffer *

;; ** Send file (complete process) **

;; The following functions add interactive commands to the chat buffer
;; to send the GET URL to the current (or selected) client.

(defun jabber-httpupload-send-file (jc jid filepath)
  "Send the file at FILEPATH to the user JID.
JC is the Jabber Connection to send the file URL."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Send file to: " nil nil nil 'full t)
                     (read-file-name "File to send:")))
  (unless (jabber-httpupload-server-has-support jc)
    (error "The Jabber Connection provided has no HTTP Upload support"))
  (let* ((size (file-attribute-size (file-attributes filepath)))
         (content-type (mailcap-extension-to-mime (file-name-extension filepath)))
         (filedata (list filepath size content-type)))
    (jabber-httpupload-request-slot jc filedata
                   #'jabber-httpupload--slot-reserved
                   (list jid))))

;; The following functions are callbacks used in the following order:

;; 1. `jabber-httpupload-request-slot' calls `jabber-httpupload--slot-reserved'.
;; 2. `jabber-httpupload--slot-reserved' calls `jabber-httpupload--upload-done'.
;; 3. `jabber-httpupload--upload-done' calls `jabber-httpupload-send-file-url'.

;; This form of calling is required because of the asynchronous
;; behaviour of the upload file function.

(defun jabber-httpupload--upload-done (data)
  "Callback function used when the upload is done.
When the upload process finished, a callback function is called with an
argument.
This function is expected to be used as the CALLBACK argument for the function
`jabber-httpupload-upload-file', DATA is its CALLBACK-ARG argument.
Also, see `jabber-httpupload-process-ended' for more information.
DATA is expected to have the following foramt: (jc jid get-url).
After the upload is done, send the get-url to the destined Jabber user JID."
  (let ((jc (car data))
        (jid (nth 1 data))
        (get-url (nth 2 data)))
    (condition-case err
        (jabber-httpupload-send-file-url jc jid get-url)
      (error "Cannot send message.  Error: %S" err))))

;; When the slot is reserved, the HTTP upload should be started.
(defun jabber-httpupload--slot-reserved (jc _xml-data filedata urls extra-data)
  "Callback function used when the slot request succeeded.
JC is the current Jabber Connection.
XML-DATA is the received XML from the server.
FILEDATA is a triple `(filepath size content-type).
URLS is a tuple `((put-url ((header-name . header-value) ...)) get-url).
EXTRA-DATA is a list `(jid)"
  (let ((filepath (car filedata))
        (jid (car extra-data))
        (get-url (cadr urls))
        (put-url (caar urls))
        (headers (cdar urls)))
    (push (cons "content-length" (nth 1 filedata)) headers)
    (push (cons "content-type" (nth 2 filedata)) headers)
    (message "jabber-httpupload: slot PUT and GET URLs: %s %s" put-url get-url)
    (condition-case err
        (jabber-httpupload-upload-file (expand-file-name filepath)
                      headers
                      put-url
                      #'jabber-httpupload--upload-done (list jc jid get-url)
                      (jabber-httpupload-ignore-certificate jc))
      (error "Cannot upload the file.  Error: %S" err))))

;; TODO Recording and sending audio **
;; (defun jabber-httpupload--record-audio ()
;;   "Create a new audio record and save the file into a temporal directory."
;;   (let ((process (start-process-shell-command
;;                   "jabber-httpupload-record-audio"
;;                   (current-buffer)
;;                   (replace-string "$(filename"
;;                                   "/tmp/jabber-httpupload-record"
;;                                   jabber-httpupload-record-command))))
;;     (set-process-sentinel process #'jabber-httpupload-record-sentinel)))

;; * Add hooks *
;; Some function should start automatically.

;; ** Test connection support after session is established **
;; Call `jabber-httpupload-test-connection-support' as soon as

;; * Adding functions to hooks *
;; ** Test HTTP Upload support after connecting **
(add-hook 'jabber-post-connect-hooks #'jabber-httpupload-test-connection-support)

(provide 'jabber-httpupload)

;;; jabber-httpupload.el ends here
