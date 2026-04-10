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
(require 'jabber-core)
(require 'fsm)

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

(defun jabber-srv-targets (server network-server port)
  "Find connection targets for SERVER.
If NETWORK-SERVER and/or PORT are specified, use them (always STARTTLS).
Otherwise query SRV records; when `jabber-direct-tls-lookup' is non-nil,
query both _xmpps-client and _xmpp-client per XEP-0368.

Returns a list of (HOST PORT DIRECTTLS-P) where DIRECTTLS-P is
non-nil for direct TLS targets."
  (if (or network-server port)
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
  "Connect to a Jabber server with a plain network connection.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (jabber-network-connect-async fsm server network-server port))

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

(defun jabber-conn--make-process (host port buffer directtls-p server)
  "Create a network process connecting to HOST:PORT in BUFFER.
When DIRECTTLS-P is non-nil, use TLS-on-connect with SNI for SERVER."
  (let ((args (list :name "jabber"
		    :buffer buffer
		    :host host :service port
		    :coding 'utf-8
		    :nowait t)))
    (when directtls-p
      (setq args (nconc args
			(list :tls-parameters
			      (jabber-conn--tls-parameters server)))))
    (apply #'make-network-process args)))

(defun jabber-network-connect-async (fsm server network-server port)
  ;; Get all potential targets...
  (let ((targets (jabber-srv-targets server network-server port))
	errors)
    ;; ...and connect to them one after another, asynchronously, until
    ;; connection succeeds.
    (cl-labels
        ((connect
           (target remaining-targets)
	   (let ((host (nth 0 target))
		 (svc (nth 1 target))
		 (directtls-p (nth 2 target))
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
			     ;; This mustn't be `fsm-send-sync', because the FSM
			     ;; needs to change the sentinel, which cannot be done
			     ;; from inside the sentinel.
			     (fsm-send fsm (list :connected c directtls-p))))
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
			     (when c (delete-process c))
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
		   (let ((proc (jabber-conn--make-process
				host svc
				(generate-new-buffer jabber-process-buffer)
				directtls-p server)))
		     (set-process-sentinel
		      proc
		      (lambda (connection status)
			(cond
			 ((string-match "^open" status)
			  (connection-successful connection))
			 ((string-match "^failed" status)
			  (connection-failed connection status))
			 ((string-match "^deleted" status)
			  nil)
			 (t
			  (message "Unknown sentinel status `%s'" status)))))
		     (when jabber-connection-timeout
		       (setq timeout-timer
			     (run-at-time
			      jabber-connection-timeout nil
			      (lambda ()
				(connection-failed
				 proc "connection timed out"))))))
		 (file-error
		  (connection-failed nil (car (cddr e))))
		 (error
		  (connection-failed nil (error-message-string e))))))))
      (message "Connecting to %s:%s..."
	       (nth 0 (car targets)) (nth 1 (car targets)))
      (connect (car targets) (cdr targets)))))

(defun jabber-network-send (connection string)
  "Send a string via a plain TCP/IP connection to the Jabber Server."
  (process-send-string connection string))


(defun jabber-starttls-initiate (fsm)
  "Initiate a STARTTLS connection."
  (jabber-send-sexp fsm
   `(starttls ((xmlns . ,jabber-tls-xmlns)))))

(defun jabber-starttls-process-input (fsm xml-data)
  "Process result of starttls request.
On failure, signal error.

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

(defvar *jabber-virtual-server-function* nil
  "Function to use for sending stanzas on a virtual connection.
The function should accept two arguments, the connection object
and a string that the connection wants to send.")

(defun jabber-virtual-connect (fsm _server _network-server _port)
  "Connect to a virtual \"server\".
Use `*jabber-virtual-server-function*' as send function.
FSM is the finite state machine created in jabber.el library."
  (unless (functionp *jabber-virtual-server-function*)
    (error "No virtual server function specified"))
  ;; We pass the fsm itself as "connection object", as that is what a
  ;; virtual server needs to send stanzas.
  (fsm-send fsm (list :connected fsm)))

(defun jabber-virtual-send (connection string)
  (funcall *jabber-virtual-server-function* connection string))

(provide 'jabber-conn)
;;; jabber-conn.el ends here
