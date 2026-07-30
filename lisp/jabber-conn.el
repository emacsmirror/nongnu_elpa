;;; jabber-conn.el --- Network transport functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni
;; mostly inspired by Gnus.

;; Copyright (C) 2005 - Carl Henrik Lunde - chlunde+jabber+@ping.uio.no
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
;;
;; A collection of functions, that hide the details of transmitting to
;; and from a Jabber Server.  Mostly inspired by Gnus.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'fsm)
(require 'jabber-util)

(require 'gnutls)
(require 'jabber-srv)

(defconst jabber-tls-xmlns "urn:ietf:params:xml:ns:xmpp-tls"
  "RFC 6120 XMPP STARTTLS namespace.")

(defgroup jabber-conn nil "Jabber Connection Settings."
  :group 'jabber)

(defconst jabber-default-connection-type 'starttls
  "Default connection type.
See `jabber-connect-methods'.")

(defcustom jabber-invalid-certificate-servers ()
  "Jabber servers for which we accept invalid TLS certificates.
This is a list of server names, each matching the hostname part
of your JID.

This option has effect only when using native GnuTLS."
  :type '(repeat string))

(defcustom jabber-direct-tls-lookup t
  "Whether to query _xmpps-client SRV records for direct TLS.
When non-nil, `jabber-srv-targets' queries both _xmpps-client._tcp
and _xmpp-client._tcp SRV records per XEP-0368, merging them by
priority and weight.  Direct TLS targets use TLS-on-connect without
a STARTTLS upgrade."
  :type 'boolean
  :group 'jabber-conn)

(defvar jabber-connect-methods
  '((network jabber-network-connect jabber-network-send)
    (starttls jabber-network-connect jabber-network-send)
    (virtual jabber-virtual-connect jabber-virtual-send))
  "Alist of connection methods and functions.
First item is the symbol naming the method.
Second item is the connect function.
Third item is the send function.")

;; Global reference declarations

(declare-function gnutls-negotiate "gnutls.el"
                  (&rest spec
                         &key process type hostname priority-string
                         trustfiles crlfiles keylist min-prime-bits
                         verify-flags verify-error verify-hostname-error
                         &allow-other-keys))
(defvar jabber-process-buffer)          ; jabber.el
(defvar jabber-debug-keep-process-buffers) ; jabber.el

;;

(defun jabber-conn--normalize-proxy (proxy)
  "Return validated PROXY settings, or nil when PROXY is nil."
  (when proxy
    (let ((type (plist-get proxy :type))
          (host (plist-get proxy :host))
          (port (plist-get proxy :port)))
      (unless (eq type 'socks5)
        (error "Unsupported Jabber proxy type: %S" type))
      (unless (and (stringp host) (> (length host) 0))
        (error "Jabber SOCKS5 proxy host must be a non-empty string"))
      (unless (and (integerp port) (<= 1 port) (<= port 65535))
        (error "Jabber SOCKS5 proxy port must be between 1 and 65535"))
      proxy)))

(defun jabber-conn--socks5-request (host port)
  "Return a SOCKS5 CONNECT request for HOST and PORT."
  (let* ((name (encode-coding-string host 'utf-8 t))
         (length (length name)))
    (unless (<= length 255)
      (error "SOCKS5 target hostname is longer than 255 bytes"))
    (unless (and (integerp port) (<= 1 port) (<= port 65535))
      (error "SOCKS5 target port must be between 1 and 65535"))
    (concat (unibyte-string 5 1 0 3 length)
            name
            (unibyte-string (ash port -8) (logand port 255)))))

(defun jabber-conn--socks5-parse-method (bytes)
  "Parse a SOCKS5 method response from BYTES."
  (if (< (length bytes) 2)
      '(:status incomplete)
    (let ((version (aref bytes 0))
          (method (aref bytes 1)))
      (cond
       ((/= version 5)
        '(:status error :message "Invalid SOCKS5 method response version"))
       ((= method 0)
        (list :status 'ok :rest (substring bytes 2)))
       ((= method 255)
        '(:status error :message "SOCKS5 proxy has no acceptable authentication method"))
       (t
        '(:status error :message "SOCKS5 proxy requires unsupported authentication"))))))

(defconst jabber-conn--socks5-reply-errors
  '((1 . "general server failure")
    (2 . "connection not allowed")
    (3 . "network unreachable")
    (4 . "host unreachable")
    (5 . "connection refused")
    (6 . "TTL expired")
    (7 . "command not supported")
    (8 . "address type not supported"))
  "SOCKS5 CONNECT reply error messages.")

(defun jabber-conn--socks5-reply-length (bytes)
  "Return the complete SOCKS5 reply length for BYTES.
Return nil when more bytes are needed, or signal on an invalid address type."
  (pcase (aref bytes 3)
    (1 10)
    (3 (when (>= (length bytes) 5)
         (+ 7 (aref bytes 4))))
    (4 22)
    (_ (error "Invalid SOCKS5 reply address type"))))

(defun jabber-conn--socks5-parse-reply (bytes)
  "Parse a SOCKS5 CONNECT response from BYTES."
  (if (< (length bytes) 4)
      '(:status incomplete)
    (cond
     ((/= (aref bytes 0) 5)
      '(:status error :message "Invalid SOCKS5 CONNECT response version"))
     ((/= (aref bytes 2) 0)
      '(:status error :message "Invalid SOCKS5 CONNECT response reserved byte"))
     (t
      (condition-case err
          (let ((length (jabber-conn--socks5-reply-length bytes)))
            (if (or (null length) (< (length bytes) length))
                '(:status incomplete)
              (let ((reply (aref bytes 1)))
                (if (= reply 0)
                    (list :status 'ok :rest (substring bytes length))
                  (list :status 'error :message
                        (or (alist-get reply jabber-conn--socks5-reply-errors)
                            (format "unknown SOCKS5 failure %d" reply)))))))
        (error (list :status 'error :message (error-message-string err))))))))

(defun jabber-get-connect-function (type)
  "Get the connect function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq type jabber-connect-methods)))
    (nth 1 entry)))

(defun jabber-get-send-function (type)
  "Get the send function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq type jabber-connect-methods)))
    (nth 2 entry)))

(defun jabber-srv-targets (server network-server port &optional proxy)
  "Find connection targets for SERVER.
If NETWORK-SERVER and/or PORT are specified, use them (always STARTTLS).
When PROXY is non-nil, bypass SRV lookup and use the explicit target
or SERVER on port 5222.
Otherwise query SRV records; when `jabber-direct-tls-lookup' is non-nil,
query both _xmpps-client and _xmpp-client per XEP-0368.

Returns a list of (HOST PORT DIRECTTLS-P) where DIRECTTLS-P is
non-nil for direct TLS targets."
  (if (or proxy network-server port)
      ;; User override: cannot assume direct TLS without SRV.
      (list (list (or network-server server)
		  (or port 5222)
		  nil))
    (or (condition-case nil
	    (if jabber-direct-tls-lookup
		(jabber-srv-lookup-mixed server)
	      (mapcar (lambda (pair)
			(list (car pair) (cdr pair) nil))
		      (jabber-srv-lookup
		       (concat "_xmpp-client._tcp." server))))
	  (error nil))
	(list (list server 5222 nil)))))

;; Plain TCP/IP connection
(defun jabber-network-connect (fsm server network-server port)
  "Connect to a Jabber SERVER with a plain network connection.
NETWORK-SERVER is the explicit host overriding SRV resolution, or nil.
PORT is the explicit port or nil for SRV/defaults.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (jabber-network-connect-async
   fsm server network-server port
   (plist-get (fsm-get-state-data fsm) :proxy)))

(defun jabber-conn--tls-parameters (server)
  "Build :tls-parameters for direct TLS to SERVER.
SERVER is the JID domain, used for SNI and certificate verification."
  (let ((verifyp (not (member server jabber-invalid-certificate-servers))))
    (cons 'gnutls-x509pki
          (gnutls-boot-parameters
           :type 'gnutls-x509pki
           :hostname server
           :verify-hostname-error verifyp
           :verify-error verifyp))))

(defcustom jabber-connection-timeout 30
  "Seconds to wait for each connection target before trying the next.
Set to nil to disable the per-target timeout and rely on the OS
TCP timeout instead."
  :type '(choice (integer :tag "Seconds")
		 (const :tag "No timeout" nil))
  :group 'jabber-conn)

(defun jabber-conn--make-process
    (host port buffer directtls-p server &optional proxy)
  "Create a network process connecting to HOST:PORT in BUFFER.
When DIRECTTLS-P is non-nil, use TLS-on-connect with SNI for SERVER.
When PROXY is non-nil, connect to its endpoint using binary coding."
  (let ((args (list :name "jabber"
		    :buffer buffer
		    :host (or (plist-get proxy :host) host)
		    :service (or (plist-get proxy :port) port)
		    :coding (if proxy 'binary 'utf-8)
		    :nowait t)))
    (when directtls-p
      (setq args (nconc args
			(list :tls-parameters
			      (jabber-conn--tls-parameters server)))))
    (apply #'make-network-process args)))

(defun jabber-conn--socks5-step (state bytes host port)
  "Advance SOCKS5 STATE with BYTES for HOST and PORT.
Return a plist describing the next state, bytes to send, success,
or an error."
  (let* ((stage (plist-get state :stage))
         (pending (concat (plist-get state :pending) bytes))
         (result (if (eq stage 'method)
                     (jabber-conn--socks5-parse-method pending)
                   (jabber-conn--socks5-parse-reply pending))))
    (pcase (plist-get result :status)
      ('incomplete (list :state (list :stage stage :pending pending)))
      ('error (list :error (plist-get result :message)))
      ('ok
       (if (eq stage 'method)
           (list :state (list :stage 'reply
                              :pending (plist-get result :rest))
                 :send (jabber-conn--socks5-request host port))
         (list :connected t :rest (plist-get result :rest)))))))

(defun jabber-conn--socks5-filter (expected host port success failure)
  "Return a SOCKS5 filter for EXPECTED connecting to HOST and PORT.
Call SUCCESS after negotiation, or FAILURE with an error message."
  (let ((state (list :stage 'method :pending (unibyte-string)))
        settled)
    (lambda (process bytes)
      (when (and (eq process expected) (not settled))
        (condition-case err
            (let ((result (jabber-conn--socks5-step state bytes host port)))
              (cond
               ((plist-get result :error)
                (setq settled t)
                (funcall failure process (plist-get result :error)))
               ((plist-get result :connected)
                (setq settled t)
                (set-process-filter process nil)
                (set-process-coding-system process 'utf-8 'utf-8)
                (funcall success process))
               (t
                (setq state (plist-get result :state))
                (when-let* ((send (plist-get result :send)))
                  (process-send-string process send)))))
          (error
           (setq settled t)
           (funcall failure process (error-message-string err))))))))

(defun jabber-conn--start-socks5 (process host port success failure)
  "Start SOCKS5 negotiation on PROCESS for HOST and PORT.
Call SUCCESS or FAILURE when negotiation reaches a terminal state."
  (condition-case err
      (progn
        (set-process-coding-system process 'binary 'binary)
        (set-process-filter
         process
         (jabber-conn--socks5-filter process host port success failure))
        (process-send-string process (unibyte-string 5 1 0)))
    (error (funcall failure process (error-message-string err)))))

(defun jabber-conn--delete-failed-process (connection buffer)
  "Delete failed CONNECTION and BUFFER unless debug retention is enabled."
  (when (processp connection)
    (delete-process connection))
  (when (and (buffer-live-p buffer)
             (not jabber-debug-keep-process-buffers))
    (kill-buffer buffer)))

(defun jabber-network-connect-async
    (fsm server network-server port &optional proxy)
  "Asynchronously connect FSM to SERVER, trying each SRV target in turn.
NETWORK-SERVER and PORT are explicit overrides, or nil to use SRV/defaults.
When PROXY is non-nil, establish SOCKS5 before reporting success."
  ;; Get all potential targets...
  (let* ((proxy (jabber-conn--normalize-proxy proxy))
         (targets (jabber-srv-targets server network-server port proxy))
	errors)
    ;; ...and connect to them one after another, asynchronously, until
    ;; connection succeeds.
    (cl-labels
        ((connect
           (target remaining-targets)
	   (let ((host (nth 0 target))
		 (svc (nth 1 target))
		 (directtls-p (nth 2 target))
		 (proc nil)
		 (process-buffer nil)
		 (timeout-timer nil)
		 (settled nil))
	     (cl-labels ((cancel-timeout
			   ()
			   (when timeout-timer
			     (cancel-timer timeout-timer)
			     (setq timeout-timer nil)))
			 (connection-successful
			   (c)
			   (unless settled
			     (setq settled t)
			     (cancel-timeout)
			     ;; Direct success runs inside the old sentinel, so it
			     ;; must remain asynchronous.  SOCKS success runs in
			     ;; the filter and changes sentinel ownership before
			     ;; a close can be delivered.
			     (if proxy
				 (fsm-send-sync
				  fsm (list :connected c directtls-p))
			       (fsm-send
				fsm (list :connected c directtls-p)))))
			 (connection-failed
			   (c status)
			   (unless settled
			     (setq settled t)
			     (cancel-timeout)
			     (when (and (> (length status) 0)
					(eq (aref status (1- (length status))) ?\n))
			       (setq status (substring status 0 -1)))
			     (let ((err
				    (format "Couldn't connect to %s:%s: %s"
					    host svc status)))
			       (message "%s" err)
			       (push err errors))
			     (jabber-conn--delete-failed-process c process-buffer)
			     (if remaining-targets
				 (progn
				   (message
				    "Connecting to %s:%s..."
				    (nth 0 (car remaining-targets))
				    (nth 1 (car remaining-targets)))
				   (connect (car remaining-targets)
					    (cdr remaining-targets)))
			       (fsm-send fsm (list :connection-failed
						   (nreverse errors)))))))
	       (condition-case e
		   (let ((buffer (generate-new-buffer jabber-process-buffer)))
                     (setq process-buffer buffer)
                     (setq proc
                           (jabber-conn--make-process
                            host svc buffer directtls-p server proxy))
		     (set-process-sentinel
		      proc
		      (lambda (connection status)
			(cond
			 ((string-match "^open" status)
			  (if proxy
			      (jabber-conn--start-socks5
			       connection host svc
			       #'connection-successful #'connection-failed)
			    (connection-successful connection)))
			 ((string-match "^failed" status)
			  (connection-failed connection status))
			 ((string-match "^deleted" status)
			  nil)
			 (t
			  (if proxy
			      (connection-failed connection status)
			    (message "Unknown sentinel status `%s'" status))))))
		     (when jabber-connection-timeout
		       (setq timeout-timer
			     (run-at-time
			      jabber-connection-timeout nil
			      (lambda ()
				(connection-failed
				 proc "connection timed out"))))))
		 (file-error
                  (jabber-conn--delete-failed-process proc process-buffer)
		  (connection-failed nil (car (cddr e))))
		 (error
                  (jabber-conn--delete-failed-process proc process-buffer)
		  (connection-failed nil (error-message-string e))))))))
      (message "Connecting to %s:%s..."
	       (nth 0 (car targets)) (nth 1 (car targets)))
      (connect (car targets) (cdr targets)))))

(defun jabber-network-send (connection string)
  "Send STRING via the plain TCP/IP CONNECTION to the Jabber server."
  (process-send-string connection string))


(defun jabber-starttls-initiate (fsm)
  "Initiate a STARTTLS connection on FSM."
  (jabber-send-sexp fsm
		    `(starttls ((xmlns . ,jabber-tls-xmlns)))))

(defun jabber-starttls-process-input (fsm xml-data)
  "Process result of starttls request on FSM.
On failure, signal an error.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (cond
   ((eq (car xml-data) 'proceed)
    (let* ((state-data (fsm-get-state-data fsm))
	   (connection (plist-get state-data :connection))
	   (hostname (plist-get state-data :server))
	   (verifyp (not (member hostname jabber-invalid-certificate-servers))))
      (gnutls-negotiate
       :process connection
       :hostname hostname
       :verify-hostname-error verifyp
       :verify-error verifyp)))
   ((eq (car xml-data) 'failure)
    (error "Command rejected by server"))))

(define-obsolete-variable-alias '*jabber-virtual-server-function*
  'jabber-virtual-server-function "0.11.0")
(defvar jabber-virtual-server-function nil
  "Function to use for sending stanzas on a virtual connection.
The function should accept two arguments, the connection object
and a string that the connection wants to send.")

(defun jabber-virtual-connect (fsm _server _network-server _port)
  "Connect to a virtual \"server\".
Use `jabber-virtual-server-function' as send function.
FSM is the finite state machine created in jabber.el library."
  (unless (functionp jabber-virtual-server-function)
    (error "No virtual server function specified"))
  ;; We pass the fsm itself as "connection object", as that is what a
  ;; virtual server needs to send stanzas.
  (fsm-send fsm (list :connected fsm)))

(defun jabber-virtual-send (connection string)
  "Send STRING through CONNECTION via the virtual-server function."
  (funcall jabber-virtual-server-function connection string))

(provide 'jabber-conn)
;;; jabber-conn.el ends here
