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
            (should (equal fsm-event (list :connected proc nil
                                            (get 'fake-fsm :connect-attempt))))
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
              `(:connection-failed
                ("Couldn't connect to xmpp.example:5222: connection timed out")
                ,(get 'fake-fsm :connect-attempt)))))))

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
                    `((:connected ,proc nil ,(get 'fake-fsm :connect-attempt))
                      (:sentinel ,proc "closed\n")))))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p (process-buffer proc))
          (kill-buffer (process-buffer proc)))))))

;;; Connection state

(defun jabber-test-conn--state-handler (state)
  "Return the `jabber-connection' handler for STATE."
  (gethash state (get 'jabber-connection :fsm-event)))

(ert-deftest jabber-conn-test-closed-handoff-uses-error-state-cleanup ()
  "A process closed before FSM handoff is rejected and its buffer is cleaned."
  (dolist (keep-buffer '(nil t))
    (let* ((buffer (generate-new-buffer " *jabber-closed-handoff*"))
           (connection (make-pipe-process
                        :name "jabber-closed-handoff" :buffer buffer))
           (fsm (make-symbol "jabber-closed-handoff"))
           (jabber-connections (list fsm))
           (jabber-auto-reconnect t)
           (jabber-debug-keep-process-buffers keep-buffer)
           (jabber-lost-connection-hooks nil)
           (jabber-lifecycle-session-reset-functions nil)
           (state-data
            (list :username "romeo" :server "example.org" :resource "emacs"
                  :ever-session-established t))
           result)
      (unwind-protect
          (progn
            (delete-process connection)
            (setq result
                  (funcall (jabber-test-conn--state-handler :connecting)
                           fsm state-data
                           (list :connected connection nil) #'ignore))
            (should-not (car result))
            (should (equal
                     (plist-get (cadr result) :disconnection-reason)
                     "Connection closed before protocol handoff"))
            (pcase-let ((`(,cleaned ,delay)
                         (funcall
                          (gethash nil (get 'jabber-connection :fsm-enter))
                          fsm (cadr result))))
              (should (= delay jabber-reconnect-delay))
              (should-not (plist-get cleaned :connection)))
            (should-not (process-live-p connection))
            (should (eq (buffer-live-p buffer) keep-buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-conn-test-ordinary-reconnect-clears-encryption ()
  "An ordinary TCP reconnect clears old transport state."
  (let* ((connection 'new-connection)
	 (result (funcall (jabber-test-conn--state-handler :connecting)
			  'fake-fsm '(:encrypted t :disconnection-reason "old")
			  (list :connected connection nil) #'ignore))
	 (state-data (cadr result)))
    (should (eq (car result) :connected))
    (should (eq (plist-get state-data :connection) connection))
    (should-not (plist-get state-data :encrypted))
    (should-not (plist-get state-data :disconnection-reason))))

(ert-deftest jabber-conn-test-stale-dead-event-does-not-close-successor ()
  "Loss evidence for an old transport must not close its successor."
  (let* ((old-transport (make-symbol "old-transport"))
         (new-transport (make-symbol "new-transport"))
         (state-data (list :connection new-transport
                           :disconnection-reason nil))
         (handler (jabber-test-conn--state-handler :session-established))
         (stale (funcall handler 'fake-fsm state-data
                         (list :connection-dead old-transport "old failure")
                         #'ignore))
         (current (funcall handler 'fake-fsm state-data
                           (list :connection-dead new-transport "new failure")
                           #'ignore)))
    (should (eq (car stale) :session-established))
    (should (eq (cadr stale) state-data))
    (should-not (car current))
    (should (equal (plist-get (cadr current) :disconnection-reason)
                   "new failure"))))

(ert-deftest jabber-conn-test-bare-dead-event-is-ignored ()
  "Loss evidence without a transport identity cannot close a session."
  (let* ((state-data (list :connection (make-symbol "transport")))
         (handler (jabber-test-conn--state-handler :session-established))
         (result
          (condition-case nil
              (funcall handler 'fake-fsm state-data :connection-dead #'ignore)
            (error 'handler-error))))
    (should (eq (car-safe result) :session-established))
    (should (eq (cadr result) state-data))))

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

(ert-deftest jabber-conn-test-starttls-requires-tls-namespace ()
  "A local-name collision must not impose STARTTLS on plaintext policy."
  (let* ((features
	  '(features nil
		     (starttls ((xmlns . "urn:example:not-tls")))))
	 (result
	  (funcall (jabber-test-conn--state-handler :connected)
		   'fake-fsm '(:connection-type network :encrypted nil)
		   (list :stanza features) #'ignore)))
    (should-not (eq (car result) :starttls))))

(ert-deftest jabber-conn-test-starttls-policy-survives-stripped-feature ()
  "Configured STARTTLS policy must attempt TLS without an advertisement."
  (let* ((features
	  '(features nil
		     (mechanisms
		      ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl")))))
	 (result
	  (funcall (jabber-test-conn--state-handler :connected)
		   'fake-fsm '(:connection-type starttls :encrypted nil)
		   (list :stanza features) #'ignore)))
    (should (eq (car result) :starttls))))

(ert-deftest jabber-conn-test-starttls-required-overrides-plaintext-policy ()
  "A server-required TLS feature must be negotiated before SASL."
  (let* ((features
	  `(features nil
		     (starttls ((xmlns . ,jabber-tls-xmlns))
			       (required nil))))
	 (result
	  (funcall (jabber-test-conn--state-handler :connected)
		   'fake-fsm '(:connection-type network :encrypted nil)
		   (list :stanza features) #'ignore)))
    (should (eq (car result) :starttls))))

(ert-deftest jabber-conn-test-starttls-required-validates-child-namespace ()
  "Required is recognized only in the inherited or explicit TLS namespace."
  (let ((inherited
	 `(features nil
		    (starttls ((xmlns . ,jabber-tls-xmlns))
			      (required nil))))
	(explicit
	 `(features nil
		    (starttls ((xmlns . ,jabber-tls-xmlns))
			      (required ((xmlns . ,jabber-tls-xmlns))))))
	(wrong
	 `(features nil
		    (starttls ((xmlns . ,jabber-tls-xmlns))
			      (required ((xmlns . "urn:example:not-tls")))))))
    (should (jabber-conn--starttls-required-p inherited))
    (should (jabber-conn--starttls-required-p explicit))
    (should-not (jabber-conn--starttls-required-p wrong))))

(ert-deftest jabber-conn-test-starttls-rejects-premature-stream-start ()
  "A stream restart before TLS succeeds must fail closed."
  (let* ((state-data '(:connection connection :server "example.com"))
	 (result
	  (funcall (jabber-test-conn--state-handler :starttls)
		   'fake-fsm state-data
		   '(:stream-start "premature" "1.0") #'ignore)))
    (should-not (car result))
    (should-not (plist-get (cadr result) :encrypted))
    (should (string-match-p
	     "Unexpected stream restart during STARTTLS"
	     (plist-get (cadr result) :disconnection-reason)))))

(ert-deftest jabber-conn-test-starttls-rejects-stale-features ()
  "Pre-TLS features must not be accepted as a STARTTLS response."
  (let* ((state-data '(:connection connection :server "example.com"))
	 (fsm (make-symbol "jabber-test-starttls"))
	 (features
	  `(features nil
		     (starttls ((xmlns . ,jabber-tls-xmlns))
			       (required nil))))
	 result)
    (put fsm :state-data state-data)
    (setq result
	  (funcall (jabber-test-conn--state-handler :starttls)
		   fsm state-data (list :stanza features) #'ignore))
    (should-not (car result))
    (should-not (plist-get (cadr result) :encrypted))
    (should (string-match-p
	     "Unexpected STARTTLS response"
	     (plist-get (cadr result) :disconnection-reason)))))

(ert-deftest jabber-conn-test-starttls-rejects-wrong-response-namespace ()
  "A proceed element outside the TLS namespace must fail closed."
  (let* ((state-data '(:connection connection :server "example.com"))
	 (fsm (make-symbol "jabber-test-starttls"))
	 (proceed '(proceed ((xmlns . "urn:example:not-tls"))))
	 result)
    (put fsm :state-data state-data)
    (setq result
	  (funcall (jabber-test-conn--state-handler :starttls)
		   fsm state-data (list :stanza proceed) #'ignore))
    (should-not (car result))
    (should-not (plist-get (cadr result) :encrypted))
    (should (string-match-p
	     "Unexpected STARTTLS response"
	     (plist-get (cadr result) :disconnection-reason)))))

(ert-deftest jabber-conn-test-starttls-rejects-queued-pre-restart-features ()
  "Queued pre-TLS features must not cross a successful TLS restart."
  (let ((features
	 `(features nil
		    (starttls ((xmlns . ,jabber-tls-xmlns))
			      (required nil))))
	(connection (make-symbol "jabber-test-connection"))
	sasl-called)
    (cl-letf (((symbol-function 'jabber-network-connect) #'ignore)
	      ((symbol-function 'jabber-send-stream-header) #'ignore)
	      ((symbol-function 'jabber-starttls-initiate) #'ignore)
	      ((symbol-function 'gnutls-negotiate)
	       (lambda (&rest _) connection))
	      ((symbol-function 'jabber-sasl-start-auth)
	       (lambda (&rest _)
		 (setq sasl-called t))))
      (let ((fsm (start-jabber-connection
		  "romeo" "example.com" "emacs"
		  nil "secret" nil nil 'starttls)))
	(fsm-send-sync fsm (list :connected connection nil))
	(fsm-send-sync fsm '(:stream-start "before-tls" "1.0"))
	(fsm-send-sync fsm (list :stanza features))
	(fsm-send-sync
	 fsm `(:stanza (proceed ((xmlns . ,jabber-tls-xmlns)))))
	(fsm-send-sync fsm (list :stanza features))
	(should-not (get fsm :state))
	(should-not sasl-called)
	(should (string-match-p
		 "before the restarted stream header"
		 (plist-get (fsm-get-state-data fsm) :disconnection-reason)))))))

(ert-deftest jabber-conn-test-starttls-restarts-with-fresh-features ()
  "A successful TLS restart authenticates from post-TLS features only."
  (let* ((pre-tls-features
	  `(features nil
		     (starttls ((xmlns . ,jabber-tls-xmlns))
			       (required nil))))
	 (post-tls-features
	  '(features nil
		     (mechanisms
		      ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))
		      (mechanism nil "PLAIN"))))
	 (connection (make-symbol "jabber-test-connection"))
	 negotiated-features
	 tls-called)
    (cl-letf (((symbol-function 'jabber-network-connect) #'ignore)
	      ((symbol-function 'jabber-send-stream-header) #'ignore)
	      ((symbol-function 'jabber-starttls-initiate) #'ignore)
	      ((symbol-function 'gnutls-negotiate)
	       (lambda (&rest _)
		 (setq tls-called t)
		 connection))
	      ((symbol-function 'jabber-sasl-start-auth)
	       (lambda (_fsm features)
		 (setq negotiated-features features)
		 '(sasl-state))))
      (let ((fsm (start-jabber-connection
		  "romeo" "example.com" "emacs"
		  nil "secret" nil nil 'starttls)))
	(fsm-send-sync fsm (list :connected connection nil))
	(fsm-send-sync fsm '(:stream-start "before-tls" "1.0"))
	(fsm-send-sync fsm (list :stanza pre-tls-features))
	(should (eq (get fsm :state) :starttls))
	(fsm-send-sync
	 fsm `(:stanza (proceed ((xmlns . ,jabber-tls-xmlns)))))
	(should tls-called)
	(should (eq (get fsm :state) :connected))
	(should (plist-get (fsm-get-state-data fsm) :encrypted))
	(fsm-send-sync fsm '(:stream-start "after-tls" "1.0"))
	(fsm-send-sync fsm (list :stanza post-tls-features))
	(should (eq (get fsm :state) :sasl-auth))
	(should (eq negotiated-features post-tls-features))
	(should-not (eq negotiated-features pre-tls-features))))))

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
              ;; Model a reconnect, not an initial connection failure.
              (put fsm :state-data
                   (plist-put (fsm-get-state-data fsm)
                              :ever-session-established t))
              (fsm-send-sync fsm '(:connection-failed ("first attempt")))
              (fsm-send-sync fsm :timeout)
              (should (equal connector-proxies (list proxy proxy)))
              (funcall real-async fsm "example.com" nil nil proxy)
              (funcall (process-sentinel proc) proc "open\n")
              (funcall (process-filter proc) proc (unibyte-string 5 0))
              (funcall (process-filter proc) proc
                       (unibyte-string 5 0 0 1 127 0 0 1 0 0))
              (fsm-send-sync fsm '(:stream-start "reconnected" "1.0"))
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

;;; Transport event ownership

(ert-deftest jabber-conn-test-stale-sentinel-preserves-successor ()
  "A late sentinel cannot close a successor, including virtual transports."
  (let ((jabber-connections nil)
        (jabber-lost-connection-hooks nil))
    (cl-letf (((symbol-function 'jabber-network-connect) #'ignore)
              ((symbol-function 'jabber-send-stream-header) #'ignore)
              ((symbol-function 'jabber-lifecycle-dispatch-session-reset)
               #'ignore)
              ((symbol-function
                'jabber-lifecycle-dispatch-connection-list-changed) #'ignore))
      (let* ((fsm (start-jabber-connection
                   "romeo" "example.com" "emacs"
                   nil "secret" nil nil 'starttls))
             (old (make-symbol "old-transport"))
             (current (make-symbol "current-virtual-transport")))
        (fsm-send-sync fsm (list :connected current))
        (let ((state-data (fsm-get-state-data fsm)))
          (fsm-send-sync fsm (list :sentinel old "closed\n"))
          (should (eq (get fsm :state) :connected))
          (should (eq (fsm-get-state-data fsm) state-data))
          (should-not (plist-get state-data :disconnection-reason)))
        (fsm-send-sync fsm (list :sentinel current "closed\n"))
        (should-not (get fsm :state))
        (should (equal (plist-get (fsm-get-state-data fsm)
                                 :disconnection-reason)
                       "closed"))))))

(ert-deftest jabber-conn-test-stale-filter-preserves-parse-buffers ()
  "Old transport data is ignored before insertion or XML parser effects."
  (let ((old-buffer (generate-new-buffer " *jabber-old-filter*"))
        (new-buffer (generate-new-buffer " *jabber-new-filter*"))
        old current parsed)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-network-connect) #'ignore)
                  ((symbol-function 'jabber-send-stream-header) #'ignore)
                  ((symbol-function 'jabber-filter)
                   (lambda (process _fsm) (push process parsed))))
          (setq old (make-pipe-process :name "jabber-old" :buffer old-buffer))
          (setq current (make-pipe-process :name "jabber-new" :buffer new-buffer))
          (let ((fsm (start-jabber-connection
                      "romeo" "example.com" "emacs"
                      nil "secret" nil nil 'starttls)))
            (fsm-send-sync fsm (list :connected current))
            (fsm-send-sync fsm (list :filter old "<message/>"))
            (should-not parsed)
            (should (equal (with-current-buffer old-buffer (buffer-string)) ""))
            (should (equal (with-current-buffer new-buffer (buffer-string)) ""))
            (fsm-send-sync fsm (list :filter current "<presence/>"))
            (should (equal parsed (list current)))
            (should (equal (with-current-buffer new-buffer (buffer-string))
                           "<presence/>"))
            (set-process-sentinel current #'ignore)))
      (when old (delete-process old))
      (when current
        (set-process-sentinel current #'ignore)
        (delete-process current))
      (kill-buffer old-buffer)
      (kill-buffer new-buffer))))

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
      (should (equal `(:connection-failed
                       ("Couldn't connect to example.com:5222: failed with code 1")
                       ,(get 'fake-fsm :connect-attempt))
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
      (should (equal `(:connection-failed
                       ("Couldn't connect to example.com:5222: setup failed")
                       ,(get 'fake-fsm :connect-attempt))
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

(ert-deftest jabber-conn-test-disconnect-cancels-targets ()
  "Cancel TCP and SOCKS attempts, timers, and late completions before retry."
  (dolist (timeout '(nil 60))
    (dolist (phase '(tcp handoff method reply))
      (let* ((jabber-connections nil)
             (jabber-process-buffer " *jabber-test-cancel*")
             (jabber-debug-keep-process-buffers nil)
             (jabber-connection-timeout timeout)
             (proxy (unless (memq phase '(tcp handoff))
                      '(:type socks5 :host "proxy.test" :port 1080)))
             (native-run-at-time (symbol-function 'run-at-time))
             processes buffers targets timers jc fresh)
        (cl-letf (((symbol-function 'jabber-srv-targets)
                   (lambda (&rest _) '(("first.test" 5222 nil) ("second.test" 5222 nil))))
                  ((symbol-function 'jabber-conn--make-process)
                   (lambda (host _port buffer &rest _)
                     (push host targets)
                     (push buffer buffers)
                     (let ((process (make-pipe-process :name "jabber-test-cancel"
                                                       :buffer buffer :noquery t)))
                       (push process processes)
                       process)))
                  ((symbol-function 'process-send-string) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (time repeat function &rest args)
                     (let ((timer (apply native-run-at-time time repeat function args)))
                       (when (equal time 60) (push timer timers))
                       timer))))
          (unwind-protect
              (progn
                (setq jc (start-jabber-connection
                          "alice" "example.test" "desktop" nil nil nil nil 'network proxy))
                (push jc jabber-connections)
                (let* ((process (car processes))
                       (buffer (process-buffer process))
                       (sentinel (process-sentinel process))
                       filter)
                  (when (eq phase 'handoff)
                    (funcall sentinel process "open\n"))
                  (when proxy
                    (funcall sentinel process "open\n")
                    (setq filter (process-filter process))
                    (when (eq phase 'reply)
                      (funcall filter process (unibyte-string 5 0))))
                  (jabber-disconnect-one jc)
                  (should-not (get jc :state))
                  (should (plist-get (fsm-get-state-data jc) :disconnection-expected))
                  (should-not (memq jc jabber-connections))
                  (should-not (process-live-p process))
                  (should-not (buffer-live-p buffer))
                  (dolist (timer timers) (should-not (memq timer timer-list)))
                  ;; Old callbacks must not start another target or affect a
                  ;; newly connecting account, even when invoked explicitly.
                  (setq fresh (start-jabber-connection
                               "alice" "example.test" "desktop" nil nil nil nil 'network proxy))
                  (push fresh jabber-connections)
                  (funcall sentinel process "failed\n")
                  (funcall sentinel process "open\n")
                  (when filter
                    (funcall filter process (unibyte-string 5 0 0 1 127 0 0 1 0 0)))
                  (dolist (timer timers)
                    (unless (memq timer timer-list)
                      (apply (timer--function timer) (timer--args timer))))
                  (accept-process-output nil 0.01)
                  (should (equal targets '("first.test" "first.test")))
                  (should (eq (get fresh :state) :connecting))
                  (should (process-live-p (car processes)))
                  ;; The fresh attempt can still complete normally.
                  (let ((new (car processes)))
                    (funcall (process-sentinel new) new "open\n")
                    (when proxy
                      (funcall (process-filter new) new (unibyte-string 5 0))
                      (funcall (process-filter new) new
                               (unibyte-string 5 0 0 1 127 0 0 1 0 0)))
                    (let ((deadline (+ (float-time) 2)))
                      (while (and (eq (get fresh :state) :connecting)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.01)))
                    (should (eq (get fresh :state) :connected))
                    (should (eq new (plist-get (fsm-get-state-data fresh) :connection))))))
            (dolist (timer timers) (cancel-timer timer))
            (dolist (fsm (list jc fresh))
              (when fsm (jabber-disconnect-one fsm)))
            (dolist (process processes)
              (set-process-sentinel process #'ignore)
              (delete-process process))
            (dolist (buffer buffers)
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))


(ert-deftest jabber-conn-test-replacement-rejects-queued-result ()
  "A queued result from an old attempt cannot claim its replacement."
  (dolist (status '("open\n" "failed\n"))
    (let ((jabber-connections nil)
          (jabber-process-buffer " *jabber-test-replacement*")
          (jabber-debug-keep-process-buffers nil)
          (jabber-connection-timeout nil)
          processes buffers jc)
      (cl-letf (((symbol-function 'jabber-srv-targets)
                 (lambda (&rest _) '(("target.test" 5222 nil))))
                ((symbol-function 'jabber-conn--make-process)
                 (lambda (_host _port buffer &rest _)
                   (push buffer buffers)
                   (let ((process (make-pipe-process :name "jabber-test-replacement"
                                                     :buffer buffer :noquery t)))
                     (push process processes)
                     process)))
                ((symbol-function 'process-send-string) #'ignore))
        (unwind-protect
            (progn
              (setq jc (start-jabber-connection
                        "alice" "example.test" "desktop" nil nil nil nil 'network))
              (push jc jabber-connections)
              (let ((old (car processes)))
                (funcall (process-sentinel old) old status)
                (jabber-network-connect-async jc "example.test" nil nil)
                (accept-process-output nil 0.01)
                (should-not (process-live-p old))
                (should (eq (get jc :state) :connecting))
                (should (process-live-p (car processes)))
                (funcall (process-sentinel (car processes)) (car processes) "open\n")
                (let ((deadline (+ (float-time) 2)))
                  (while (and (eq (get jc :state) :connecting)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.01)))
                (should (eq (get jc :state) :connected))
                (should (eq (car processes)
                            (plist-get (fsm-get-state-data jc) :connection)))))
          (when jc (jabber-disconnect-one jc))
          (dolist (process processes)
            (set-process-sentinel process #'ignore)
            (delete-process process))
          (dolist (buffer buffers)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun jabber-conn-test--cancel-during-entry (phase)
  "Cancel a native reconnect while its PHASE yields, then replace it."
  (dolist (timeout '(nil 60))
    (let* ((jc (make-symbol "jabber-test-entry"))
           (jabber-connections (list jc))
           (jabber-auto-reconnect t)
           (jabber-reconnect-delay 600)
           (jabber-direct-tls-lookup nil)
           (jabber-connection-timeout timeout)
           (jabber-debug-keep-process-buffers nil)
           (jabber-process-buffer " *jabber-test-entry*")
           (state (jabber-sm--reset
                   (list :username "alice" :server "example.test"
                         :resource "desktop" :connection-type 'network
                         :connection 'old-transport :send-function #'ignore
                         :ever-session-established t)))
           (failures 0)
           (creates 0)
           cancelled old-attempt fresh processes buffers timers)
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data state)
      (plist-put state :sm-enabled t)
      (plist-put state :sm-id "resume-id")
      (when (eq phase 'process)
        (plist-put state :network-server "old.test"))
      (jabber-sm--enqueue-pending
       state '(message nil (body nil "queued")) nil
       (lambda (_reason) (cl-incf failures)))
      (cl-labels
          ((cancel-and-replace ()
             (jabber-disconnect-one jc)
             (setq cancelled t)
             ;; Install a successor before the old connecting entry returns.
             (setq fresh (start-jabber-connection
                          "alice" "example.test" "desktop" nil nil
                          "fresh.test" 5222 'network))
             (push fresh jabber-connections)))
        (cl-letf (((symbol-function 'dns-query-asynchronous)
                   (lambda (_name callback &rest _)
                     ;; Retain native SRV discovery and dns-query's wait loop.
                     (setq old-attempt (get jc :connect-attempt))
                     (push (run-at-time
                            0 nil (lambda ()
                                    (cancel-and-replace)
                                    (funcall callback nil)))
                           timers)
                     t))
                  ((symbol-function 'jabber-conn--make-process)
                   (lambda (host _port buffer &rest _)
                     (cl-incf creates)
                     (push buffer buffers)
                     (let ((process (make-pipe-process
                                     :name "jabber-test-entry"
                                     :buffer buffer :noquery t)))
                       (push process processes)
                       (when (equal host "old.test")
                         (setq old-attempt (get jc :connect-attempt))
                         (push (run-at-time 0 nil #'cancel-and-replace) timers)
                         (let ((deadline (+ (float-time) 2)))
                           (while (and (not cancelled) (< (float-time) deadline))
                             (accept-process-output nil 0.01))))
                       process)))
                  ((symbol-function 'process-send-string) #'ignore))
          (unwind-protect
              (progn
                (fsm-send-sync jc '(:connection-dead old-transport "lost"))
                (should (timerp (get jc :timeout)))
                (fsm-send-sync jc :timeout)
                (should cancelled)
                (should old-attempt)
                (should-not (get jc :state))
                (should (plist-get (fsm-get-state-data jc) :terminalized))
                (should (plist-get (fsm-get-state-data jc) :disconnection-expected))
                (should-not (memq jc jabber-connections))
                (dolist (key '(:sm-pending-queue :sm-recovered-queue :sm-outbound-queue))
                  (should-not (plist-get (fsm-get-state-data jc) key)))
                (should-not (get jc :timeout))
                (should-not (get jc :connect-attempt))
                (should-not (get jc :connect-cancel))
                (should (= failures 1))
                ;; Neither repeated stop nor late results may settle it again.
                (jabber-disconnect-one jc)
                (fsm-send-sync jc (list :connected 'old-transport nil old-attempt))
                (fsm-send-sync jc (list :connection-failed '("late") old-attempt))
                (fsm-send-sync jc :timeout)
                (should (= failures 1))
                (should-not (get jc :state))
                (should-not (plist-get (fsm-get-state-data jc) :sm-pending-queue))
                (should (equal jabber-connections (list fresh)))
                (should (eq (get fresh :state) :connecting))
                (should (= creates (if (eq phase 'process) 2 1)))
                (dolist (process (cdr processes))
                  (should-not (process-live-p process))
                  (funcall (process-sentinel process) process "failed\n")
                  (funcall (process-sentinel process) process "open\n"))
                (dolist (buffer (cdr buffers))
                  (should-not (buffer-live-p buffer)))
                (let ((new (car processes)))
                  (should (process-live-p new))
                  (funcall (process-sentinel new) new "open\n")
                  (let ((deadline (+ (float-time) 2)))
                    (while (and (eq (get fresh :state) :connecting)
                                (< (float-time) deadline))
                      (accept-process-output nil 0.01)))
                  (should (eq (get fresh :state) :connected))
                  (should (eq new (plist-get (fsm-get-state-data fresh) :connection))))
                (should (= failures 1))
                (should (= creates (if (eq phase 'process) 2 1))))
            (dolist (timer timers) (cancel-timer timer))
            (dolist (fsm (list jc fresh))
              (when fsm
                (jabber-disconnect-one fsm)
                (fsm-stop-timer fsm)
                (jabber-conn--cancel-connect fsm)))
            (dolist (process processes)
              (set-process-sentinel process #'ignore)
              (delete-process process))
            (dolist (buffer buffers)
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(ert-deftest jabber-conn-test-disconnect-during-native-dns-entry ()
  "Keep terminal settlement when public disconnect interrupts native DNS."
  (jabber-conn-test--cancel-during-entry 'dns))

(ert-deftest jabber-conn-test-disconnect-during-process-entry ()
  "Keep terminal settlement when public disconnect interrupts process setup."
  (jabber-conn-test--cancel-during-entry 'process))

(ert-deftest jabber-conn-test-connecting-entry-preserves-successor-timer ()
  "Preserve a retry timer installed by a reentrant connecting failure."
  (let* ((jc (make-symbol "jabber-test-entry-timer"))
         (jabber-connections (list jc))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 600)
         (state (jabber-sm--reset
                 (list :username "alice" :server "example.test"
                       :ever-session-established t)))
         successor-state successor-timer)
    (put jc :name 'jabber-connection)
    (cl-letf (((symbol-function 'jabber-get-connect-function)
               (lambda (_type)
                 (lambda (fsm &rest _)
                   (fsm-send-sync fsm '(:connection-failed ("setup failed")))
                   (setq successor-state (fsm-get-state-data fsm)
                         successor-timer (get fsm :timeout))))))
      (unwind-protect
          (progn
            (fsm-update jc :connecting state nil)
            (should-not (get jc :state))
            (should (eq successor-state (fsm-get-state-data jc)))
            (should (timerp successor-timer))
            (should (eq successor-timer (get jc :timeout)))
            (should (memq successor-timer timer-list)))
        (jabber-disconnect-one jc)
        (fsm-stop-timer jc)))))

(defun jabber-conn-test--replacement-cleanup (action timeout)
  "Exercise ACTION in predecessor cleanup with connection TIMEOUT."
  (let ((jabber-connections nil)
        (jabber-process-buffer " *jabber-test-cleanup-admission*")
        (jabber-debug-keep-process-buffers nil)
        (jabber-connection-timeout timeout)
        (native-run-at-time (symbol-function 'run-at-time))
        (native-srv-targets (symbol-function 'jabber-srv-targets))
        jc processes buffers hosts discoveries timers
        old-token outer-token newer-token newer-cancel newer-process)
    (cl-letf (((symbol-function 'jabber-conn--make-process)
               (lambda (host _port buffer &rest _)
                 (push host hosts)
                 (push buffer buffers)
                 (let ((process (make-pipe-process
                                 :name "jabber-test-cleanup-admission"
                                 :buffer buffer :noquery t)))
                   (push process processes)
                   process)))
              ((symbol-function 'jabber-srv-targets)
               (lambda (server network-server port &optional proxy)
                 (push network-server discoveries)
                 (funcall native-srv-targets server network-server port proxy)))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (time repeat function &rest args)
                 (let ((timer (apply native-run-at-time time repeat function args)))
                   ;; Native editing may also allocate an undo boundary timer.
                   (when (equal time 60) (push timer timers))
                   timer))))
      (unwind-protect
          (progn
            ;; Native construction connects before account registration.
            (setq jc (start-jabber-connection
                      "alice" "example.test" "desktop" nil nil
                      "old.test" 5222 'network))
            (should-not (memq jc jabber-connections))
            (should (process-live-p (car processes)))
            (setq old-token (get jc :connect-attempt))
            (push jc jabber-connections)
            (with-current-buffer (car buffers)
              (add-hook
               'kill-buffer-hook
               (lambda ()
                 (setq outer-token (get jc :connect-attempt))
                 (if (eq action 'stop)
                     (jabber-disconnect-one jc)
                   (jabber-network-connect-async
                    jc "example.test" "newer.test" 5222)
                   (setq newer-token (get jc :connect-attempt)
                         newer-cancel (get jc :connect-cancel)
                         newer-process (car processes))))
               nil t))
            (jabber-network-connect-async jc "example.test" "outer.test" 5222)
            (if (eq action 'stop)
                (progn
                  (should (equal hosts '("old.test")))
                  (should (equal discoveries '("old.test")))
                  (should (= (length timers) (if timeout 1 0))))
              (should (equal hosts '("newer.test" "old.test")))
              (should (equal discoveries '("newer.test" "old.test")))
              (should (= (length timers) (if timeout 2 0)))
              (should newer-token)
              (should newer-cancel)
              (should-not (eq newer-token outer-token))
              (should (eq newer-token (get jc :connect-attempt)))
              (should (eq newer-cancel (get jc :connect-cancel)))
              (should (process-live-p newer-process))
              (should-not (process-live-p (cadr processes)))
              (should-not (buffer-live-p (cadr buffers)))
              ;; The winner's timer remains active until normal handoff.
              (when timeout
                (should (memq (car timers) timer-list))
                (should-not (memq (cadr timers) timer-list)))
              (funcall (process-sentinel newer-process) newer-process "open\n")
              (let ((deadline (+ (float-time) 2)))
                (while (and (eq (get jc :state) :connecting)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (eq (get jc :state) :connected))
              (should (eq newer-process
                          (plist-get (fsm-get-state-data jc) :connection))))
            ;; Teardown must see B's admission, not A or an empty slot.
            (should outer-token)
            (should-not (eq old-token outer-token))
            (jabber-disconnect-one jc)
            (jabber-disconnect-one jc)
            (should-not (get jc :state))
            (should (plist-get (fsm-get-state-data jc) :terminalized))
            (should (plist-get (fsm-get-state-data jc) :disconnection-expected))
            (should-not (memq jc jabber-connections))
            (should-not (get jc :timeout))
            (should-not (get jc :connect-attempt))
            (should-not (get jc :connect-cancel))
            (should-not (cl-some #'process-live-p processes))
            (should-not (cl-some #'buffer-live-p buffers))
            (dolist (timer timers)
              (should-not (memq timer timer-list))
              (should-not (memq timer timer-idle-list))))
        (when jc
          (jabber-disconnect-one jc)
          (jabber-conn--cancel-connect jc)
          (fsm-stop-timer jc))
        (dolist (timer timers) (cancel-timer timer))
        (dolist (process processes)
          (set-process-sentinel process #'ignore)
          (delete-process process))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (setq kill-buffer-hook nil))
            (kill-buffer buffer)))))))

(ert-deftest jabber-conn-test-replacement-cleanup-stop-without-timeout ()
  "Respect public stop during replacement cleanup without a timeout."
  (jabber-conn-test--replacement-cleanup 'stop nil))

(ert-deftest jabber-conn-test-replacement-cleanup-stop-with-timeout ()
  "Respect public stop during replacement cleanup with a finite timeout."
  (jabber-conn-test--replacement-cleanup 'stop 60))

(ert-deftest jabber-conn-test-replacement-cleanup-successor-without-timeout ()
  "Preserve a cleanup-hook successor without a timeout."
  (jabber-conn-test--replacement-cleanup 'replace nil))

(ert-deftest jabber-conn-test-replacement-cleanup-successor-with-timeout ()
  "Preserve a cleanup-hook successor with a finite timeout."
  (jabber-conn-test--replacement-cleanup 'replace 60))

(defun jabber-conn-test--handoff-change (action keep-buffer timeout hook)
  "Exercise ACTION during native handoff with KEEP-BUFFER, TIMEOUT and HOOK."
  (let ((jabber-connections nil)
        (jabber-process-buffer " *jabber-test-handoff-change*")
        (jabber-debug-keep-process-buffers keep-buffer)
        (jabber-connection-timeout timeout)
        (jabber-auto-reconnect t)
        (jabber-reconnect-delay 600)
        (native-send (symbol-function 'process-send-string))
        (native-run-at-time (symbol-function 'run-at-time))
        (failures 0)
        jc processes buffers timers writes fired successor-state
        successor-token successor-cancel successor-timer retired-filter
        retired-sentinel)
    (cl-letf (((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer &rest _)
                 (push buffer buffers)
                 (let ((process (make-pipe-process
                                 :name "jabber-test-handoff-change"
                                 :buffer buffer :noquery t)))
                   (push process processes)
                   process)))
              ((symbol-function 'process-send-string)
               (lambda (process wire)
                 (push process writes)
                 (funcall native-send process wire)))
              ((symbol-function 'run-at-time)
               (lambda (time repeat function &rest args)
                 (let ((timer (apply native-run-at-time time repeat function args)))
                   (when (memq time '(60 600)) (push timer timers))
                   timer))))
      (unwind-protect
          (progn
            (setq jc (start-jabber-connection
                      "alice" "example.test" "desktop" nil nil
                      "old.test" 5222 'network))
            (push jc jabber-connections)
            (plist-put (fsm-get-state-data jc) :ever-session-established t)
            (jabber-sm--enqueue-pending
             (fsm-get-state-data jc) '(message nil (body nil "pending")) nil
             (lambda (_reason) (cl-incf failures)))
            (let ((old (car processes))
                  (buffer (car buffers)))
              (with-current-buffer buffer
                (insert "transport bytes")
                (add-hook
                 hook
                 (lambda (&rest _)
                   (unless fired
                     (setq fired t)
                     (pcase action
                       ('error (error "Native handoff hook failed"))
                       ('stop (jabber-disconnect-one jc))
                       ('replace
                        (jabber-network-connect-async
                         jc "example.test" "newer.test" 5222))
                       ('retry
                        (fsm-send-sync jc '(:connection-failed ("handoff lost")))))
                     (setq successor-state (fsm-get-state-data jc)
                           successor-token (get jc :connect-attempt)
                           successor-cancel (get jc :connect-cancel)
                           successor-timer (get jc :timeout)
                           retired-filter (process-filter old)
                           retired-sentinel (process-sentinel old))))
                 nil t))
              (if (eq action 'error)
                  ;; Inspect the real handler's condition before fsm.el's
                  ;; debug/error adapter consumes it.
                  (funcall (gethash :connecting
                                    (get 'jabber-connection :fsm-event))
                           jc (fsm-get-state-data jc)
                           (list :connected old nil (get jc :connect-attempt))
                           #'ignore)
                (funcall (process-sentinel old) old "open\n"))
              (let ((deadline (+ (float-time) 2)))
                (while (and (not fired) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should fired)
              (if (eq action 'normal)
                  (progn
                    (should (eq (get jc :state) :connected))
                    (should (eq old (plist-get (fsm-get-state-data jc) :connection)))
                    (should (plist-get (fsm-get-state-data jc) :awaiting-stream-start))
                    (should (equal writes (list old)))
                    (should (process-live-p old))
                    (should-not (get jc :connect-attempt))
                    (should-not (get jc :connect-cancel))
                    (should (= (buffer-size buffer) 0)))
                (should-not writes)
                (should (eq successor-state (fsm-get-state-data jc)))
                (should (eq retired-filter (process-filter old)))
                (should (eq retired-sentinel (process-sentinel old)))
                (should-not (process-live-p old))
                (should (eq (buffer-live-p buffer) keep-buffer))
                (should (eq successor-token (get jc :connect-attempt)))
                (should (eq successor-cancel (get jc :connect-cancel)))
                (should (eq successor-timer (get jc :timeout)))
                (pcase action
                  ('stop
                   (should-not (get jc :state))
                   (should (= failures 1))
                   (should (plist-get successor-state :terminalized))
                   (should (plist-get successor-state :disconnection-expected))
                   (should-not (memq jc jabber-connections))
                   (should-not (plist-get successor-state :sm-pending-queue)))
                  ('retry
                   (should-not (get jc :state))
                   (should (timerp successor-timer))
                   (should (memq successor-timer timer-list))
                   (should (memq jc jabber-connections)))
                  ('replace
                   (should (eq (get jc :state) :connecting))
                   (should successor-token)
                   (should successor-cancel)
                   (should (process-live-p (car processes)))
                   (when timeout (should (memq (car timers) timer-list)))
                   (let ((new (car processes)))
                     (funcall (process-sentinel new) new "open\n")
                     (let ((deadline (+ (float-time) 2)))
                       (while (and (eq (get jc :state) :connecting)
                                   (< (float-time) deadline))
                         (accept-process-output nil 0.01)))
                     (should (eq (get jc :state) :connected))
                     (should (eq new (plist-get (fsm-get-state-data jc) :connection)))
                     (should (equal writes (list new)))))))
              (jabber-disconnect-one jc)
              (jabber-disconnect-one jc)
              (should (= failures 1))
              (should-not (get jc :state))
              (should-not (memq jc jabber-connections))
              (should-not (plist-get (fsm-get-state-data jc) :sm-pending-queue))
              (should-not (get jc :connect-attempt))
              (should-not (get jc :connect-cancel))
              (should-not (get jc :timeout))
              (should-not (cl-some #'process-live-p processes))
              (unless keep-buffer (should-not (cl-some #'buffer-live-p buffers)))
              (dolist (timer timers) (should-not (memq timer timer-list)))))
        (when jc
          (jabber-disconnect-one jc)
          (jabber-conn--cancel-connect jc)
          (fsm-stop-timer jc))
        (dolist (timer timers) (cancel-timer timer))
        (dolist (process processes)
          (set-process-sentinel process #'ignore)
          (delete-process process))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq before-change-functions nil after-change-functions nil))
            (kill-buffer buffer)))))))

(ert-deftest jabber-conn-test-handoff-change-stop ()
  "Preserve terminal settlement after native buffer modification hooks."
  (dolist (timeout '(nil 60))
    (dolist (keep-buffer '(nil t))
      (dolist (hook '(before-change-functions after-change-functions))
        (jabber-conn-test--handoff-change 'stop keep-buffer timeout hook)))))

(ert-deftest jabber-conn-test-handoff-change-successor ()
  "Preserve same-FSM replacement and retire only the captured transport."
  (dolist (timeout '(nil 60))
    (dolist (keep-buffer '(nil t))
      (dolist (hook '(before-change-functions after-change-functions))
        (jabber-conn-test--handoff-change 'replace keep-buffer timeout hook)))))

(ert-deftest jabber-conn-test-handoff-change-retry-timer ()
  "Preserve the authoritative retry timer after handoff cleanup reentry."
  (dolist (timeout '(nil 60))
    (dolist (keep-buffer '(nil t))
      (jabber-conn-test--handoff-change
       'retry keep-buffer timeout 'before-change-functions))))

(ert-deftest jabber-conn-test-handoff-change-current-error ()
  "Propagate a native modification-hook error while the attempt is current."
  (dolist (hook '(before-change-functions after-change-functions))
    (should
     (equal (should-error
             (jabber-conn-test--handoff-change 'error nil nil hook))
            '(error "Native handoff hook failed")))))

(ert-deftest jabber-conn-test-handoff-change-normal ()
  "Complete normal handoff after harmless native modification hooks."
  (dolist (timeout '(nil 60))
    (dolist (keep-buffer '(nil t))
      (jabber-conn-test--handoff-change
       'normal keep-buffer timeout 'before-change-functions))))

(ert-deftest jabber-conn-test-handoff-send-reentry ()
  "Preserve current state and timer after a synchronous stream-header send."
  (dolist (action '(stop retry reply))
    (let ((jabber-connections nil)
          (jabber-auto-reconnect t)
          (jabber-reconnect-delay 600)
          (failures 0)
          fired successor-state successor-timer jc)
      (cl-letf (((symbol-function 'jabber-network-connect) #'ignore))
        (unwind-protect
            (progn
              (setq jc (start-jabber-connection
                        "alice" "example.test" "desktop" nil nil
                        "old.test" 5222 'network))
              (push jc jabber-connections)
              (let ((state (fsm-get-state-data jc)))
                (plist-put state :ever-session-established t)
                (jabber-sm--enqueue-pending
                 state '(message nil (body nil "pending")) nil
                 (lambda (_reason) (cl-incf failures)))
                (plist-put
                 state :send-function
                 (lambda (_connection _wire)
                   (unless fired
                     (setq fired t)
                     (pcase action
                       ('stop (jabber-disconnect-one jc))
                       ('retry
                        (fsm-send-sync jc '(:sentinel transport "lost\n")))
                       ('reply (fsm-send-sync jc '(:stream-start "new-stream" "1.0"))))
                     (setq successor-state (fsm-get-state-data jc)
                           successor-timer (get jc :timeout))))))
              (fsm-send-sync jc '(:connected transport nil))
              (should fired)
              (should (eq successor-state (fsm-get-state-data jc)))
              (should (eq successor-timer (get jc :timeout)))
              (pcase action
                ('stop
                 (should-not (get jc :state))
                 (should (plist-get successor-state :terminalized))
                 (should-not (plist-get successor-state :sm-pending-queue))
                 (should (= failures 1)))
                ('retry
                 (should-not (get jc :state))
                 (should (timerp successor-timer))
                 (should (memq successor-timer timer-list)))
                ('reply
                 (should (eq (get jc :state) :connected))
                 (should-not (plist-get successor-state :awaiting-stream-start))
                 (should (equal (plist-get successor-state :session-id) "new-stream"))))
              (jabber-disconnect-one jc)
              (jabber-disconnect-one jc)
              (should (= failures 1)))
          (when jc
            (jabber-disconnect-one jc)
            (fsm-stop-timer jc)))))))

(provide 'jabber-test-conn)

;;; jabber-test-conn.el ends here
