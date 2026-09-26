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

(ert-deftest jabber-test-httpupload-parse-slot-answer-inherited-namespace ()
  (let* ((slot `(iq ()
                    (slot ((xmlns . ,jabber-httpupload-xmlns))
                          (get ((url . "https://download.example.net/file")))
                          (put ((url . "https://upload.example.net/file"))
                               (header ((name . "Authorization"))
                                       "Bearer token")))))
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

;;; Curl upload

(ert-deftest jabber-test-httpupload-curl-sentinel-calls-callback-on-zero-exit ()
  "Curl sentinel calls its callback when curl exits successfully."
  (let ((buffer (generate-new-buffer " *jabber-curl-test*"))
        (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer)
                   (lambda (_process) buffer))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0)))
          (jabber-httpupload--curl-sentinel
           'process "finished\n"
           (lambda (arg)
             (setq called arg))
           'done)
          (should (eq called 'done))
          (with-current-buffer buffer
            (should (string-match-p "Sentinel: \"finished" (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest jabber-test-httpupload-curl-sentinel-reports-nonzero-exit ()
  "Curl sentinel reports nonzero exit without calling its callback."
  (let ((buffer (generate-new-buffer " *jabber-curl-test*"))
        (called nil)
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "curl: (22) upload rejected\nAuthorization: Bearer secret\n"))
          (cl-letf (((symbol-function 'process-buffer)
                     (lambda (_process) buffer))
                    ((symbol-function 'process-status)
                     (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status)
                     (lambda (_process) 22))
                    ((symbol-function 'process-get)
                     (lambda (_process prop)
                       (and (eq prop :jabber-httpupload-filename)
                            "file.jpg")))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (jabber-httpupload--curl-sentinel
             'process "exited abnormally with code 22\n"
             (lambda (_arg)
               (setq called t))
             'done)
            (should-not called)
            (should (equal messages
                           '("HTTP Upload failed for file.jpg: exit status 22; event: exited abnormally with code 22; curl output: curl: (22) upload rejected
Authorization: <redacted>")))))
      (kill-buffer buffer))))

(ert-deftest jabber-test-httpupload-curl-sentinel-reports-signal ()
  "Curl sentinel reports signal status without calling its callback."
  (let ((called nil)
        (messages nil))
    (cl-letf (((symbol-function 'process-buffer)
               (lambda (_process) nil))
              ((symbol-function 'process-status)
               (lambda (_process) 'signal))
              ((symbol-function 'process-exit-status)
               (lambda (_process) 15))
              ((symbol-function 'process-get)
               (lambda (_process prop)
                 (and (eq prop :jabber-httpupload-filename)
                      "file.jpg")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (jabber-httpupload--curl-sentinel
       'process "killed\n"
       (lambda (_arg)
         (setq called t))
       'done)
      (should-not called)
      (should (equal messages
                     '("HTTP Upload failed for file.jpg: signal status 15; event: killed"))))))

(ert-deftest jabber-test-httpupload-curl-log-redacts-header-values ()
  "Curl process log omits request header values."
  (let ((buffer (generate-new-buffer " *jabber-curl-test*"))
        command)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/bin/curl"))
                  ((symbol-function 'get-buffer-create)
                   (lambda (_name) buffer))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq command (plist-get args :command))
                     'process))
                  ((symbol-function 'process-put)
                   (lambda (&rest _args) nil)))
          (should
           (jabber-httpupload-put-file-curl
            "/tmp/file.jpg"
            '(("Authorization" . "Bearer secret")
              ("Cookie" . "sid=secret")
              ("content-type" . "image/jpeg"))
            "https://upload.example.net/file.jpg"
            #'ignore 'done))
          (should (member "Authorization: Bearer secret" command))
          (with-current-buffer buffer
            (let ((log (buffer-string)))
              (should (string-match-p "Authorization: <redacted>" log))
              (should (string-match-p "Cookie: <redacted>" log))
              (should-not (string-match-p "Bearer secret" log))
              (should-not (string-match-p "sid=secret" log))
              (should-not (string-match-p "image/jpeg" log)))))
      (kill-buffer buffer))))

;;; Discovery

(ert-deftest jabber-test-httpupload-discover-errors-with-no-items ()
  (cl-letf (((symbol-function 'fsm-get-state-data)
             (lambda (_jc) '(:server "example.net")))
            ((symbol-function 'jabber-disco-get-items)
             (lambda (jc _jid _node callback closure &optional _force)
               (funcall callback jc closure nil)))
            ((symbol-function 'message) #'ignore))
    (should-error
     (jabber-httpupload--discover-and-upload 'jc #'ignore)
     :type 'user-error)))

(ert-deftest jabber-test-httpupload-discover-errors-without-feature ()
  (let ((items (list ["Archive" "archive.example.net" nil]
                     ["Proxy" "proxy.example.net" nil])))
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:server "example.net")))
              ((symbol-function 'jabber-disco-get-items)
               (lambda (jc _jid _node callback closure &optional _force)
                 (funcall callback jc closure items)))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (jc jid _node callback closure &optional _force)
                 (funcall callback jc closure
                          (list nil (list (format "feature:%s" jid))))))
              ((symbol-function 'message) #'ignore))
      (should-error
       (jabber-httpupload--discover-and-upload 'jc #'ignore)
       :type 'user-error))))

(ert-deftest jabber-test-httpupload-discover-uploads-with-feature ()
  (let ((items (list ["Upload" "upload.example.net" nil]))
        (jabber-httpupload-support nil)
        (jabber-httpupload-max-file-size nil)
        (uploaded nil))
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:server "example.net")))
              ((symbol-function 'jabber-disco-get-items)
               (lambda (jc _jid _node callback closure &optional _force)
                 (funcall callback jc closure items)))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (jc _jid _node callback closure &optional _force)
                 (funcall callback jc closure
                          (list nil
                                (list jabber-httpupload-xmlns)
                                (list (jabber-test-httpupload--max-size-form
                                       "4096"))))))
              ((symbol-function 'message) #'ignore))
      (jabber-httpupload--discover-and-upload 'jc (lambda () (setq uploaded t)))
      (should (equal jabber-httpupload-support
                     '((jc . "upload.example.net"))))
      (should (equal jabber-httpupload-max-file-size '((jc . 4096))))
      (should uploaded))))

(ert-deftest jabber-test-httpupload-selected-service-keeps-limit ()
  "Later discovery must not attach another service's limit to the first."
  (let ((jabber-httpupload-support nil)
        (jabber-httpupload-max-file-size nil))
    (cl-labels ((info (limit)
                 `(nil (,jabber-httpupload-xmlns)
                       ((x ()
                           (field ((var . "FORM_TYPE"))
                                  (value () ,jabber-httpupload-xmlns))
                           (field ((var . "max-file-size"))
                                  (value () ,limit)))))))
      (jabber-httpupload--record-support 'jc "first.example" (info "100"))
      (jabber-httpupload--record-support 'jc "second.example" (info "900"))
      (should (equal (cdr (assq 'jc jabber-httpupload-support)) "first.example"))
      (should (= (cdr (assq 'jc jabber-httpupload-max-file-size)) 100))
      (jabber-httpupload--record-support 'jc "second.example" '(nil nil nil))
      (should (= (cdr (assq 'jc jabber-httpupload-max-file-size)) 100))
      (jabber-httpupload--record-support 'jc "first.example" (info "200"))
      (should (= (cdr (assq 'jc jabber-httpupload-max-file-size)) 200))
      (jabber-httpupload--record-support 'jc "first.example" '(nil nil nil))
      (should-not (assq 'jc jabber-httpupload-max-file-size)))))

;;; Native discovery, service limits and disconnection

(defun jabber-test-httpupload--native-connection (name)
  ;; Install the native FSM's established-state data without authenticating.
  (let ((jc (make-symbol name)))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data (list :username name :server "example.invalid"
                              :resource "probe" :ever-session-established t))
    (put jc :deferred nil)
    jc))

(defmacro jabber-test-httpupload--native-isolated (&rest body)
  (declare (indent 0) (debug t))
  `(let ((jabber-db-path nil)
         (jabber-httpupload-support nil)
         (jabber-httpupload--discoveries nil)
         (jabber-disco-items-cache (make-hash-table :test #'equal))
         (jabber-httpupload-max-file-size nil)
         (jabber-disco-info-cache (make-hash-table :test #'equal))
         (jabber-open-info-queries nil)
         (jabber-httpupload-pre-upload-transform nil)
         (jabber-auto-reconnect nil)
         (jabber-jid-obarray (make-vector 31 0))
         (jabber-roster-list nil)
         (jabber-connections nil)
         (kill-ring nil)
         (interprogram-cut-function nil)
         (jabber-test-httpupload-native-wire nil)
         (jabber-httpupload-upload-function
          (lambda (&rest _args) (error "Unexpected HTTP upload"))))
     (cl-letf (((symbol-function 'jabber-send-sexp)
                (lambda (jc xml &rest _args) (push (cons jc xml) jabber-test-httpupload-native-wire)))
               ((symbol-function 'jabber-send-string) (lambda (&rest _args) t))
               ((symbol-function 'make-network-process)
                (lambda (&rest _args) (error "Network forbidden")))
               ((symbol-function 'open-network-stream)
                (lambda (&rest _args) (error "Network forbidden")))
               ((symbol-function 'url-retrieve)
                (lambda (&rest _args) (error "HTTP forbidden")))
               ((symbol-function 'start-process)
                (lambda (&rest _args) (error "External process forbidden"))))
       ,@body)))

(defun jabber-test-httpupload--native-xml (text)
  (with-temp-buffer
    (insert text)
    (car (xml-parse-region (point-min) (point-max)))))

(defun jabber-test-httpupload--native-reply (jc request payload)
  (jabber-process-iq
   jc (jabber-test-httpupload--native-xml
       (format "<iq type='result' from='%s' id='%s'>%s</iq>"
               (jabber-xml-get-attribute request 'to)
               (jabber-xml-get-attribute request 'id) payload))))

(defun jabber-test-httpupload--native-discover (jc service limit)
  ;; No direct calls to record-support or max-size parser: get-info sends a
  ;; real IQ; native IQ dispatch and native disco parser consume wire XML.
  (remhash (cons service nil) jabber-disco-info-cache)
  (let (wire)
    (cl-letf (((symbol-function 'jabber-send-sexp)
               (lambda (owner xml &rest _args)
                 (should (eq owner jc)) (push xml wire))))
      (jabber-httpupload-test-item-support jc service))
    (should (= (length wire) 1))
    (should (equal (jabber-xml-get-attribute (car wire) 'to) service))
    (jabber-test-httpupload--native-reply
     jc (car wire)
     (concat "<query xmlns='http://jabber.org/protocol/disco#info'>"
             "<identity category='store' type='file'/>"
             "<feature var='urn:xmpp:http:upload:0'/>"
             (if limit
                 (format (concat "<x xmlns='jabber:x:data' type='result'>"
                                 "<field var='FORM_TYPE' type='hidden'>"
                                 "<value>urn:xmpp:http:upload:0</value></field>"
                                 "<field var='max-file-size'><value>%d</value></field></x>")
                         limit)
               "")
             "</query>"))
    (should-not jabber-open-info-queries)))

(defun jabber-test-httpupload--native-tuple (jc)
  (list (cdr (assq jc jabber-httpupload-support))
        (cdr (assq jc jabber-httpupload-max-file-size))))

(defun jabber-test-httpupload--native-upload-at-size (jc service limit size)
  ;; Exercise the PUBLIC command, real validation, native slot IQ creation,
  ;; native result dispatch/slot parsing and terminal callback. HTTP alone
  ;; is intercepted (file bytes/headers/URL are inspected, never transmitted).
  (let ((file (make-temp-file "jabber-test-httpupload-native-file-" nil ".bin"))
        wire puts)
    (unwind-protect
        (progn
          (with-temp-file file (insert (make-string size ?x)))
          (let ((jabber-httpupload-upload-function
                 (lambda (path headers put-url callback callback-arg &rest _)
                   (push (list path headers put-url) puts)
                   (should (= (file-attribute-size (file-attributes path)) size))
                   (should (= (cdr (assoc "content-length" headers)) size))
                   (should (equal put-url (concat "https://" service "/put")))
                   (funcall callback callback-arg)
                   t)))
            (cl-letf (((symbol-function 'jabber-send-sexp)
                       (lambda (owner xml &rest _)
                         (should (eq owner jc)) (push xml wire))))
              (if (and limit (> size limit))
                  (progn
                    (should-error (jabber-httpupload-upload-file jc file)
                                  :type 'user-error)
                    (should-not wire)
                    (should-not puts))
                (jabber-httpupload-upload-file jc file)
                (should (= (length wire) 1))
                (let* ((iq (car wire)) (request (car (jabber-xml-get-children iq 'request))))
                  (should (equal (jabber-xml-get-attribute iq 'to) service))
                  (should (equal (jabber-xml-get-attribute request 'xmlns)
                                 jabber-httpupload-xmlns))
                  (should (= (jabber-xml-get-attribute request 'size) size))
                  (jabber-test-httpupload--native-reply jc iq
                                                        (format (concat "<slot xmlns='urn:xmpp:http:upload:0'>"
                                                                        "<put url='https://%s/put'/>"
                                                                        "<get url='https://%s/get'/></slot>")
                                                                service service)))
                (should (= (length puts) 1))
                (should (equal (car kill-ring) (concat "https://" service "/get")))
                (should-not jabber-open-info-queries)))))
      (delete-file file))))

(defun jabber-test-httpupload--native-matrix (a b reverse-order)
  (jabber-test-httpupload--native-isolated
   (let* ((jc (jabber-test-httpupload--native-connection "matrix"))
          (items (list (cons "a.example.invalid" a)
                       (cons "b.example.invalid" b)))
          (order (if reverse-order (reverse items) items))
          (service (caar order)) (limit (cdar order)))
     (dolist (entry order) (jabber-test-httpupload--native-discover jc (car entry) (cdr entry)))
     (should (equal (jabber-test-httpupload--native-tuple jc) (list service limit)))

     (dolist (size '(50 100 101 500 900 901 1000))
       (jabber-test-httpupload--native-upload-at-size jc service limit size)))))

(dolist (a '(100 900 nil))
  (dolist (b '(100 900 nil))
    (dolist (reverse-order '(nil t))
      (eval `(ert-deftest ,(intern (format "jabber-test-httpupload-native-matrix-a%s-b%s-%s" a b
                                           (if reverse-order "BA" "AB"))) ()
               (jabber-test-httpupload--native-matrix ,a ,b ,reverse-order)) t))))

(dolist (service '("a.example.invalid" "b.example.invalid"))
  (eval `(ert-deftest ,(intern (format "jabber-test-httpupload-native-refresh-%s" service)) ()
           (jabber-test-httpupload--native-isolated
            (let ((jc (jabber-test-httpupload--native-connection "refresh")))
              (dolist (limit '(100 900 nil 50))
                (jabber-test-httpupload--native-discover jc ,service limit)
                (should (equal (jabber-test-httpupload--native-tuple jc) (list ,service limit)))
                (jabber-test-httpupload--native-upload-at-size jc ,service limit 500))))) t))

(defun jabber-test-httpupload--native-disconnect (mode)
  (jabber-test-httpupload--native-isolated
   (let* ((a (jabber-test-httpupload--native-connection "account-a"))
          (b (jabber-test-httpupload--native-connection "account-b"))
          (jabber-connections (list a b))
          (resets nil)
          (jabber-lifecycle-session-reset-functions
           (cons (lambda (jc) (push jc resets))
                 jabber-lifecycle-session-reset-functions)))
     (jabber-test-httpupload--native-discover a "a.example.invalid" 100)
     (jabber-test-httpupload--native-discover b "b.example.invalid" 900)
     (pcase mode
       ('one (jabber-disconnect-one a) (jabber-disconnect-one a))
       ('all (jabber-disconnect) (jabber-disconnect))
       ('loss (fsm-send-sync a '(:connection-dead nil "fixture transport loss"))))
     ;; Validate native terminal transition before diagnosing upload cleanup.
     (should-not (memq a jabber-connections))
     (should (null (get a :state)))
     (should (plist-get (fsm-get-state-data a) :terminalized))
     (should (= (cl-count a resets) 1))
     (if (eq mode 'all)
         (progn (should-not jabber-connections)
                (should (= (cl-count b resets) 1)))
       (should (memq b jabber-connections))
       (should (equal (jabber-test-httpupload--native-tuple b) '("b.example.invalid" 900))))

     (should-not (assq a jabber-httpupload-support))
     (should-not (assq a jabber-httpupload-max-file-size))
     (when (eq mode 'all)
       (should-not jabber-httpupload-support)
       (should-not jabber-httpupload-max-file-size)))))

(ert-deftest jabber-test-httpupload-native-disconnect-one () (jabber-test-httpupload--native-disconnect 'one))
(ert-deftest jabber-test-httpupload-native-disconnect-all () (jabber-test-httpupload--native-disconnect 'all))
(ert-deftest jabber-test-httpupload-native-disconnect-loss () (jabber-test-httpupload--native-disconnect 'loss))

;;; Retired continuations and sibling responses

(defconst jabber-test-httpupload--native-items
  "<query xmlns='http://jabber.org/protocol/disco#items'><item jid='a.example.invalid'/><item jid='b.example.invalid'/></query>")

(defconst jabber-test-httpupload--native-info
  "<query xmlns='http://jabber.org/protocol/disco#info'><feature var='urn:xmpp:http:upload:0'/></query>")

(ert-deftest jabber-test-httpupload-native-sibling-settlement ()
  "Success, errors and quits all retire duplicate and sibling discovery IQs."
  (dolist (failure '(nil error quit))
    (jabber-test-httpupload--native-isolated
     (let ((jc (jabber-test-httpupload--native-connection "siblings"))
           (calls 0))
       ;; An unrelated consumer's opaque data must survive upload retirement.
       (jabber-send-iq jc "other.example.invalid" "get" '(query ())
                       #'ignore 'opaque #'ignore 'opaque)
       (let ((unrelated (car jabber-open-info-queries)))
         (jabber-httpupload--discover-and-upload
          jc (lambda ()
               (cl-incf calls)
               (should (= (length jabber-open-info-queries) 1))
               (when failure (signal failure '("Originating failure")))))
         (let ((items (cdar jabber-test-httpupload-native-wire)))
           (jabber-test-httpupload--native-reply
            jc items jabber-test-httpupload--native-items)
           (let ((infos (mapcar #'cdr (cl-subseq jabber-test-httpupload-native-wire 0 2)))
                 (callbacks (cl-subseq jabber-open-info-queries 0 2))
                 caught)
             (condition-case err
                 (jabber-test-httpupload--native-reply
                  jc (car infos) jabber-test-httpupload--native-info)
               ((error quit) (setq caught err)))
             (should (equal caught (and failure (list failure "Originating failure"))))
             (should (= calls 1))
             (should (equal jabber-open-info-queries (list unrelated)))
             (should-not jabber-httpupload--discoveries)
             (jabber-test-httpupload--native-reply
              jc items jabber-test-httpupload--native-items)
             (dolist (info infos)
               (jabber-test-httpupload--native-reply
                jc info jabber-test-httpupload--native-info))
             ;; Even already-captured callbacks cannot resurrect an operation.
             (dolist (entry callbacks)
               (let ((data (cdr (nth 1 entry))))
                 (funcall (car data) jc (cdr data)
                          (list nil (list jabber-httpupload-xmlns)))))
             (should (= calls 1))
             (should (equal jabber-open-info-queries (list unrelated))))))))))

(ert-deftest jabber-test-httpupload-native-reset-retires-discovery ()
  "Reset fences items, info and background discovery but preserves other accounts."
  (dolist (phase '(items info background))
    (jabber-test-httpupload--native-isolated
     (let* ((a (jabber-test-httpupload--native-connection "retired"))
            (b (jabber-test-httpupload--native-connection "live"))
            (jabber-connections (list a b))
            (file (make-temp-file "jabber-upload-reset-" nil nil "draft")))
       (unwind-protect
           (progn
             (jabber-httpupload-test-item-support b "live.example.invalid")
             (let ((b-query (car jabber-open-info-queries))
                   (b-wire (cdar jabber-test-httpupload-native-wire)))
               (if (eq phase 'background)
                   (jabber-httpupload-test-connection-support a)
                 (jabber-httpupload-upload-file a file))
               (unless (eq phase 'items)
                 (jabber-test-httpupload--native-reply
                  a (cdar jabber-test-httpupload-native-wire)
                  jabber-test-httpupload--native-items))
               (let ((old-wire (mapcar #'cdr
                                       (cl-remove-if-not
                                        (lambda (entry) (eq (car entry) a))
                                        jabber-test-httpupload-native-wire)))
                     (old-queries (delq b-query (copy-sequence jabber-open-info-queries))))
                 (jabber-disconnect-one a)
                 (jabber-disconnect-one a)
                 (should (equal jabber-open-info-queries (list b-query)))
                 (should (= (length jabber-httpupload--discoveries) 1))
                 (dolist (stanza old-wire)
                   (jabber-test-httpupload--native-reply
                    a stanza (if (equal (jabber-xml-get-attribute
                                         (jabber-iq-query stanza) 'xmlns)
                                        jabber-disco-xmlns-items)
                                 jabber-test-httpupload--native-items
                               jabber-test-httpupload--native-info)))
                 (dolist (entry old-queries)
                   (let ((data (cdr (nth 1 entry))))
                     (funcall (car data) a (cdr data)
                              (list nil (list jabber-httpupload-xmlns)))))
                 (should-not (assq a jabber-httpupload-support))
                 (should-not (assq a jabber-httpupload-max-file-size))
                 (should-not kill-ring)
                 (should-not (cl-find-if
                              (lambda (entry)
                                (eq (car (jabber-iq-query (cdr entry))) 'request))
                              jabber-test-httpupload-native-wire))
                 ;; The other account's outstanding response remains usable.
                 (jabber-test-httpupload--native-reply
                  b b-wire jabber-test-httpupload--native-info)
                 (should (equal (jabber-test-httpupload--native-tuple b)
                                '("live.example.invalid" nil)))
                 ;; Fresh logical-session discovery can choose a new tuple.
                 (put a :state :session-established)
                 (put a :state-data (list :server "example.invalid"))
                 (jabber-test-httpupload--native-discover a "new.example.invalid" 900)
                 (should (equal (jabber-test-httpupload--native-tuple a)
                                '("new.example.invalid" 900)))
                 (should-not jabber-open-info-queries)
                 (should-not jabber-httpupload--discoveries))))
         (delete-file file))))))

(provide 'jabber-test-httpupload)

;;; jabber-test-httpupload.el ends here
