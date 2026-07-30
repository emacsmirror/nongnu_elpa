;;; jabber-test-conn.el --- Tests for jabber-conn  -*- lexical-binding: t; -*-

;;; Commentary:

;; Network transport helpers.

;;; Code:

(require 'ert)
(require 'jabber-conn)
(require 'jabber-core)

(defvar jabber-account-list)
(defvar jabber-default-resource)
(defvar jabber-process-buffer)
(defvar jabber-debug-keep-process-buffers)

;;; Proxy configuration

(ert-deftest jabber-conn-test-normalize-socks5-proxy ()
  "A complete SOCKS5 proxy plist is accepted unchanged."
  (let ((proxy '(:type socks5 :host "127.0.0.1" :port 9050)))
    (should (equal (jabber-conn--normalize-proxy proxy) proxy))))

(ert-deftest jabber-conn-test-rejects-invalid-socks5-proxy ()
  "Invalid or unsupported proxy settings are rejected."
  (dolist (proxy '((:type socks4 :host "127.0.0.1" :port 9050)
                   (:type socks5 :port 9050)
                   (:type socks5 :host "" :port 9050)
                   (:type socks5 :host "127.0.0.1" :port 0)
                   (:type socks5 :host "127.0.0.1" :port 65536)))
    (should-error (jabber-conn--normalize-proxy proxy))))

(ert-deftest jabber-conn-test-connect-all-passes-account-proxy ()
  "Connecting configured accounts passes their proxy setting through."
  (let* ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
         (jabber-account-list
          `(("romeo@example.com" (:password . "secret") (:proxy . ,proxy))))
         (jabber-default-resource "emacs")
         (jabber-connections nil)
         connect-args)
    (cl-letf (((symbol-function 'jabber-connect)
               (lambda (&rest args) (setq connect-args args))))
      (jabber-connect-all)
      (should (equal (car (last connect-args)) proxy)))))

(ert-deftest jabber-conn-test-start-constructor-keeps-old-arity ()
  "The generated FSM constructor accepts its original arguments."
  (cl-letf (((symbol-function 'jabber-network-connect) #'ignore))
    (let ((fsm (start-jabber-connection
                "romeo" "example.com" "emacs"
                nil "secret" nil nil 'starttls)))
      (should-not (plist-get (fsm-get-state-data fsm) :proxy)))))

;;; SOCKS5 protocol

(ert-deftest jabber-conn-test-socks5-request-uses-domain-name ()
  "The SOCKS5 CONNECT request sends the target hostname to the proxy."
  (should
   (equal (string-to-list
           (jabber-conn--socks5-request "xmpp.example" 5222))
          '(5 1 0 3 12
              120 109 112 112 46 101 120 97 109 112 108 101
              20 102))))

(ert-deftest jabber-conn-test-socks5-request-rejects-long-hostname ()
  "A SOCKS5 CONNECT request rejects hostnames longer than one byte."
  (should-error
   (jabber-conn--socks5-request (make-string 256 ?a) 5222)))

(ert-deftest jabber-conn-test-socks5-method-parser-waits-for-full-frame ()
  "The SOCKS5 method parser leaves a partial frame incomplete."
  (should
   (equal (jabber-conn--socks5-parse-method (unibyte-string 5))
          '(:status incomplete))))

(ert-deftest jabber-conn-test-socks5-method-parser-returns-remainder ()
  "The SOCKS5 method parser accepts no-auth and returns trailing bytes."
  (should
   (equal (jabber-conn--socks5-parse-method
           (concat (unibyte-string 5 0) (unibyte-string 1 2)))
          `(:status ok :rest ,(unibyte-string 1 2)))))

(ert-deftest jabber-conn-test-socks5-method-parser-rejects-authentication ()
  "The SOCKS5 method parser rejects methods requiring authentication."
  (let ((result (jabber-conn--socks5-parse-method (unibyte-string 5 2))))
    (should (eq (plist-get result :status) 'error))
    (should (string-match-p "authentication" (plist-get result :message)))))

(ert-deftest jabber-conn-test-socks5-method-parser-rejects-version ()
  "The SOCKS5 method parser rejects a non-SOCKS5 response."
  (let ((result (jabber-conn--socks5-parse-method (unibyte-string 4 0))))
    (should (eq (plist-get result :status) 'error))
    (should (string-match-p "version" (plist-get result :message)))))

(ert-deftest jabber-conn-test-socks5-reply-parser-waits-for-full-frame ()
  "The SOCKS5 reply parser waits for the complete variable-length frame."
  (dolist (bytes (list (unibyte-string 5 0 0)
                       (unibyte-string 5 0 0 3)
                       (concat (unibyte-string 5 0 0 3 3) "fo")))
    (should
     (equal (jabber-conn--socks5-parse-reply bytes)
            '(:status incomplete)))))

(ert-deftest jabber-conn-test-socks5-reply-parser-accepts-domain-frame ()
  "The SOCKS5 reply parser accepts a domain response and returns its remainder."
  (let ((frame (concat (unibyte-string 5 0 0 3 3)
                       "foo"
                       (unibyte-string 0 80 9))))
    (should
     (equal (jabber-conn--socks5-parse-reply frame)
            `(:status ok :rest ,(unibyte-string 9))))))

(ert-deftest jabber-conn-test-socks5-reply-parser-reports-standard-failures ()
  "Every assigned SOCKS5 CONNECT failure has an explanatory error."
  (dolist (reply (number-sequence 1 8))
    (let ((result
           (jabber-conn--socks5-parse-reply
            (unibyte-string 5 reply 0 1 127 0 0 1 0 0))))
      (should (eq (plist-get result :status) 'error))
      (should (> (length (plist-get result :message)) 0)))))

(ert-deftest jabber-conn-test-socks5-reply-parser-rejects-malformed-header ()
  "Malformed SOCKS5 CONNECT response headers are rejected."
  (dolist (bytes (list (unibyte-string 4 0 0 1 127 0 0 1 0 0)
                       (unibyte-string 5 0 1 1 127 0 0 1 0 0)
                       (unibyte-string 5 0 0 2 0 0)))
    (should
     (eq (plist-get (jabber-conn--socks5-parse-reply bytes) :status)
         'error))))

;;; Proxy transport

(ert-deftest jabber-conn-test-network-connect-reads-proxy-from-fsm ()
  "The network connector preserves its API and reads proxy state from FSM."
  (let ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
        async-args)
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_fsm) (list :proxy proxy)))
              ((symbol-function 'jabber-network-connect-async)
               (lambda (&rest args) (setq async-args args))))
      (jabber-network-connect 'fake-fsm "example.com" nil nil)
      (should
       (equal async-args
              (list 'fake-fsm "example.com" nil nil proxy))))))

(ert-deftest jabber-conn-test-proxy-process-connects-in-binary ()
  "A proxied process connects to the proxy endpoint using binary coding."
  (let ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
        process-args)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (setq process-args args)
                 'fake-process)))
      (should
       (eq (jabber-conn--make-process
            "xmpp.example" 5222 nil nil "example.com" proxy)
           'fake-process))
      (should (equal (plist-get process-args :host) "127.0.0.1"))
      (should (= (plist-get process-args :service) 9050))
      (should (eq (plist-get process-args :coding) 'binary))
      (should-not (plist-member process-args :tls-parameters)))))

(ert-deftest jabber-conn-test-direct-process-remains-unchanged ()
  "A direct process still connects to its target using UTF-8."
  (let (process-args)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (setq process-args args)
                 'fake-process)))
      (jabber-conn--make-process
       "xmpp.example" 5222 nil nil "example.com" nil)
      (should (equal (plist-get process-args :host) "xmpp.example"))
      (should (= (plist-get process-args :service) 5222))
      (should (eq (plist-get process-args :coding) 'utf-8))
      (should-not (plist-member process-args :tls-parameters)))))

(ert-deftest jabber-conn-test-proxy-negotiates-before-connected ()
  "The FSM receives :connected only after fragmented SOCKS5 negotiation."
  (let ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
        (jabber-process-buffer " *jabber-test-process*")
        (jabber-connection-timeout nil)
        proc sentinel filter sent fsm-event coding)
    (cl-letf (((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer &rest _)
                 (setq proc (make-pipe-process
                             :name "jabber-test-process"
                             :buffer buffer))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn) (setq sentinel fn)))
              ((symbol-function 'set-process-filter)
               (lambda (_proc fn) (setq filter fn)))
              ((symbol-function 'set-process-coding-system)
               (lambda (_proc read write) (setq coding (list read write))))
              ((symbol-function 'process-send-string)
               (lambda (_proc bytes) (push bytes sent)))
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (setq fsm-event event)))
              ((symbol-function 'fsm-send-sync)
               (lambda (_fsm event) (setq fsm-event event))))
      (unwind-protect
          (progn
            (jabber-network-connect-async
             'fake-fsm "xmpp.example" nil nil proxy)
            (funcall sentinel proc "open\n")
            (should-not fsm-event)
            (should (equal (car sent) (unibyte-string 5 1 0)))
            (funcall filter proc (unibyte-string 5))
            (should-not fsm-event)
            (funcall filter proc (unibyte-string 0))
            (should
             (equal (car sent)
                    (jabber-conn--socks5-request "xmpp.example" 5222)))
            (funcall filter proc (unibyte-string 5 0 0 1 127))
            (should-not fsm-event)
            (funcall filter proc (unibyte-string 0 0 1 0 0))
            (should (equal fsm-event (list :connected proc nil)))
            (should (equal coding '(utf-8 utf-8))))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p (process-buffer proc))
          (kill-buffer (process-buffer proc)))))))

(ert-deftest jabber-conn-test-proxy-timeout-cleans-negotiation ()
  "The connection timeout remains active during SOCKS5 negotiation."
  (let ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
        (jabber-process-buffer " *jabber-test-process*")
        (jabber-debug-keep-process-buffers nil)
        (jabber-connection-timeout 10)
        proc sentinel timeout-callback fsm-event)
    (cl-letf (((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer &rest _)
                 (setq proc (make-pipe-process
                             :name "jabber-test-process"
                             :buffer buffer))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn) (setq sentinel fn)))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat fn)
                 (setq timeout-callback fn)
                 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (setq fsm-event event))))
      (jabber-network-connect-async
       'fake-fsm "xmpp.example" nil nil proxy)
      (funcall sentinel proc "open\n")
      (funcall timeout-callback)
      (should-not (process-live-p proc))
      (should-not (buffer-live-p (process-buffer proc)))
      (should
       (equal fsm-event
              '(:connection-failed
                ("Couldn't connect to xmpp.example:5222: connection timed out")))))))

(ert-deftest jabber-conn-test-proxy-attempts-have-independent-state ()
  "A partial SOCKS5 reply from one attempt cannot affect another."
  (let (filters sent successes)
    (cl-letf (((symbol-function 'set-process-filter)
               (lambda (proc fn) (push (cons proc fn) filters)))
              ((symbol-function 'process-send-string)
               (lambda (proc bytes) (push (cons proc bytes) sent)))
              ((symbol-function 'set-process-coding-system) #'ignore))
      (jabber-conn--start-socks5
       'first "first.example" 5222
       (lambda (proc) (push proc successes)) #'ignore)
      (jabber-conn--start-socks5
       'second "second.example" 5222
       (lambda (proc) (push proc successes)) #'ignore)
      (funcall (cdr (assq 'first filters)) 'first (unibyte-string 5))
      (funcall (cdr (assq 'second filters)) 'second (unibyte-string 5 0))
      (funcall (cdr (assq 'second filters))
               'second (unibyte-string 5 0 0 1 127 0 0 1 0 0))
      (should (equal successes '(second)))
      (should
       (equal (cdr (assq 'second sent))
              (jabber-conn--socks5-request "second.example" 5222))))))

(ert-deftest jabber-conn-test-proxy-filter-converts-errors-to-failure ()
  "A negotiation error reaches the connection failure boundary."
  (let (filter failure)
    (cl-letf (((symbol-function 'set-process-filter)
               (lambda (_proc fn) (setq filter fn)))
              ((symbol-function 'set-process-coding-system) #'ignore)
              ((symbol-function 'process-send-string) #'ignore))
      (jabber-conn--start-socks5
       'fake-process (make-string 256 ?a) 5222 #'ignore
       (lambda (_proc message) (setq failure message)))
      (funcall filter 'fake-process (unibyte-string 5 0))
      (should (string-match-p "longer than 255 bytes" failure)))))

(ert-deftest jabber-conn-test-proxy-filter-ignores-stale-process ()
  "A callback for another process cannot settle the current attempt."
  (let (filter success)
    (cl-letf (((symbol-function 'set-process-filter)
               (lambda (_proc fn) (setq filter fn)))
              ((symbol-function 'set-process-coding-system) #'ignore)
              ((symbol-function 'process-send-string) #'ignore))
      (jabber-conn--start-socks5
       'current "example.com" 5222
       (lambda (proc) (setq success proc)) #'ignore)
      (funcall filter 'stale (unibyte-string 5 0))
      (funcall filter 'stale
               (unibyte-string 5 0 0 1 127 0 0 1 0 0))
      (should-not success))))

(ert-deftest jabber-conn-test-proxy-handoff-preserves-immediate-close ()
  "A close after SOCKS success reaches the FSM sentinel."
  (let ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
        (jabber-process-buffer " *jabber-test-process*")
        (jabber-connection-timeout nil)
        proc sentinel filter events)
    (cl-letf (((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer &rest _)
                 (setq proc (make-pipe-process
                             :name "jabber-test-process"
                             :buffer buffer))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn) (setq sentinel fn)))
              ((symbol-function 'set-process-filter)
               (lambda (_proc fn) (setq filter fn)))
              ((symbol-function 'set-process-coding-system) #'ignore)
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (push event events)))
              ((symbol-function 'fsm-send-sync)
               (lambda (_fsm event)
                 (push event events)
                 (setq sentinel
                       (lambda (process status)
                         (push (list :sentinel process status) events))))))
      (unwind-protect
          (progn
            (jabber-network-connect-async
             'fake-fsm "xmpp.example" nil nil proxy)
            (funcall sentinel proc "open\n")
            (funcall filter proc (unibyte-string 5 0))
            (funcall filter proc
                     (unibyte-string 5 0 0 1 127 0 0 1 0 0))
            (funcall sentinel proc "closed\n")
            (should
             (equal (nreverse events)
                    `((:connected ,proc nil)
                      (:sentinel ,proc "closed\n")))))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p (process-buffer proc))
          (kill-buffer (process-buffer proc)))))))

;;; Connection state

(defun jabber-test-conn--state-handler (state)
  "Return the `jabber-connection' handler for STATE."
  (gethash state (get 'jabber-connection :fsm-event)))

(ert-deftest jabber-conn-test-ordinary-reconnect-clears-encryption ()
  "An ordinary TCP reconnect clears encryption state from the old socket."
  (let* ((connection 'new-connection)
	 (result (funcall (jabber-test-conn--state-handler :connecting)
			  'fake-fsm '(:encrypted t)
			  (list :connected connection nil) #'ignore))
	 (state-data (cadr result)))
    (should (eq (car result) :connected))
    (should (eq (plist-get state-data :connection) connection))
    (should-not (plist-get state-data :encrypted))))

(ert-deftest jabber-conn-test-direct-tls-sets-encryption ()
  "A direct TLS connection records that its socket is encrypted."
  (let* ((connection 'new-connection)
	 (result (funcall (jabber-test-conn--state-handler :connecting)
			  'fake-fsm '(:encrypted nil)
			  (list :connected connection t) #'ignore))
	 (state-data (cadr result)))
    (should (eq (car result) :connected))
    (should (eq (plist-get state-data :connection) connection))
    (should (eq (plist-get state-data :encrypted) t))))

(ert-deftest jabber-conn-test-reconnect-selects-starttls ()
  "An ordinary reconnect negotiates advertised STARTTLS."
  (let* ((connect-result
	  (funcall (jabber-test-conn--state-handler :connecting)
		   'fake-fsm '(:connection-type starttls :encrypted t)
		   '(:connected new-connection nil) #'ignore))
	 (features
	  `(features nil (starttls ((xmlns . ,jabber-tls-xmlns)))))
	 (result
	  (funcall (jabber-test-conn--state-handler :connected)
		   'fake-fsm (cadr connect-result)
		   (list :stanza features) #'ignore)))
    (should (eq (car result) :starttls))))

(ert-deftest jabber-conn-test-configured-proxy-reconnects-to-starttls ()
  "A configured proxy survives an FSM reconnect and reaches STARTTLS."
  (let* ((proxy '(:type socks5 :host "127.0.0.1" :port 9050))
         (jabber-account-list
          `(("romeo@example.com"
             (:password . "secret")
             (:connection-type . starttls)
             (:proxy . ,proxy))))
         (jabber-default-resource "emacs")
         (jabber-connections nil)
         (jabber-lost-connection-hooks nil)
         (jabber-process-buffer " *jabber-test-process*")
         (jabber-connection-timeout nil)
         (real-async (symbol-function 'jabber-network-connect-async))
         connector-proxies proc starttls-called)
    (cl-letf (((symbol-function 'jabber-network-connect-async)
               (lambda (_fsm _server _network-server _port proxy)
                 (push proxy connector-proxies)))
              ((symbol-function 'jabber-lifecycle-dispatch-session-reset)
               #'ignore)
              ((symbol-function
                'jabber-lifecycle-dispatch-connection-list-changed)
               #'ignore)
              ((symbol-function 'jabber-send-stream-header) #'ignore)
              ((symbol-function 'jabber-starttls-initiate)
               (lambda (_fsm) (setq starttls-called t)))
              ((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer &rest _)
                 (setq proc (make-pipe-process
                             :name "jabber-test-process"
                             :buffer buffer))))
              ((symbol-function 'process-send-string) #'ignore))
      (unwind-protect
          (progn
            (jabber-connect-all)
            (let ((fsm (car jabber-connections)))
              (should (equal (plist-get (fsm-get-state-data fsm) :proxy)
                             proxy))
              (fsm-send-sync fsm '(:connection-failed ("first attempt")))
              (fsm-send-sync fsm :timeout)
              (should (equal connector-proxies (list proxy proxy)))
              (funcall real-async fsm "example.com" nil nil proxy)
              (funcall (process-sentinel proc) proc "open\n")
              (funcall (process-filter proc) proc (unibyte-string 5 0))
              (funcall (process-filter proc) proc
                       (unibyte-string 5 0 0 1 127 0 0 1 0 0))
              (fsm-send-sync
               fsm
               `(:stanza
                  (features nil
                            (starttls ((xmlns . ,jabber-tls-xmlns))))))
              (should (eq (get fsm :state) :starttls))
              (should starttls-called)))
        (when (process-live-p proc)
          (delete-process proc))
        (when (and proc (buffer-live-p (process-buffer proc)))
          (kill-buffer (process-buffer proc)))))))

;;; Failed async connection cleanup

(ert-deftest jabber-conn-test-failed-target-kills-process-buffer ()
  "A failed async connection target cleans up its process buffer."
  (let ((jabber-process-buffer " *jabber-test-process*")
        (jabber-debug-keep-process-buffers nil)
        (jabber-connection-timeout nil)
        proc
        sentinel
        fsm-event)
    (cl-letf (((symbol-function 'jabber-srv-targets)
               (lambda (&rest _) '(("example.com" 5222 nil))))
              ((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer _directtls-p _server
                        &optional _proxy)
                 (setq proc (make-pipe-process
                             :name "jabber-test-process"
                             :buffer buffer))
                 proc))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn) (setq sentinel fn)))
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (setq fsm-event event))))
      (jabber-network-connect-async 'fake-fsm "example.com" nil nil)
      (funcall sentinel proc "failed with code 1\n")
      (should-not (process-live-p proc))
      (should-not (buffer-live-p (process-buffer proc)))
      (should (equal '(:connection-failed
                       ("Couldn't connect to example.com:5222: failed with code 1"))
                     fsm-event)))))

(ert-deftest jabber-conn-test-setup-error-kills-generated-buffer ()
  "A setup error after buffer creation kills the generated buffer."
  (let ((jabber-process-buffer " *jabber-test-process*")
        (jabber-debug-keep-process-buffers nil)
        (jabber-connection-timeout nil)
        generated-buffer
        fsm-event)
    (cl-letf (((symbol-function 'jabber-srv-targets)
               (lambda (&rest _) '(("example.com" 5222 nil))))
              ((symbol-function 'generate-new-buffer)
               (lambda (name)
                 (setq generated-buffer (get-buffer-create name))
                 generated-buffer))
              ((symbol-function 'jabber-conn--make-process)
               (lambda (&rest _) (error "setup failed")))
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (setq fsm-event event))))
      (jabber-network-connect-async 'fake-fsm "example.com" nil nil)
      (should-not (buffer-live-p generated-buffer))
      (should (equal '(:connection-failed
                       ("Couldn't connect to example.com:5222: setup failed"))
                     fsm-event)))))

(ert-deftest jabber-conn-test-keeps-failed-buffer-when-debugging ()
  "Debug buffer retention preserves failed process buffers."
  (let ((jabber-debug-keep-process-buffers t)
        (buffer (generate-new-buffer " *jabber-test-process*"))
        proc)
    (unwind-protect
        (progn
          (setq proc (make-pipe-process
                      :name "jabber-test-process"
                      :buffer buffer))
          (jabber-conn--delete-failed-process proc buffer)
          (should-not (process-live-p proc))
          (should (buffer-live-p buffer)))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'jabber-test-conn)

;;; jabber-test-conn.el ends here
