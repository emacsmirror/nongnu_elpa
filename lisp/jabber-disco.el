;;; jabber-disco.el --- service discovery functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
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

;; Jabber discovery module, handles service discovery functions.

;;; Code:

(require 'cl-lib)
(require 'jabber-db)
(require 'jabber-iq)
(require 'jabber-xml)
(require 'jabber-xdata)

(defvar jabber-xdata-xmlns)            ; jabber-xml.el

(defconst jabber-disco-xmlns-info "http://jabber.org/protocol/disco#info"
  "XEP-0030 Service Discovery info namespace.")

(defconst jabber-disco-xmlns-items "http://jabber.org/protocol/disco#items"
  "XEP-0030 Service Discovery items namespace.")

(defconst jabber-caps-xmlns "http://jabber.org/protocol/caps"
  "XEP-0115 Entity Capabilities namespace.")

;;
;;; Respond to disco requests

(jabber-chain-add 'jabber-presence-chain #'jabber-process-caps 10)

(defvar jabber-caps-cache (make-hash-table :test 'equal)
  "Globally reusable, verified capabilities keyed by (HASH . VER).")

(defvar jabber-caps--pending (make-hash-table :test 'equal)
  "Pending capabilities requests keyed by (HASH . VER).
Each value contains :active, :queue and :timer.  Candidates retain a
session owner, full JID and advertised node; they are not verified data.")

(defun jabber-disco--owner (jc)
  "Return a session-qualified observation owner for connection JC.
Ordinary FSM state copies preserve ownership; a new transport or stream
retires it.  Never derive an owner from the current buffer."
  (when jc
    (let ((state (fsm-get-state-data jc)))
      (list jc (plist-get state :connection) (plist-get state :session-id)
            (plist-get state :username) (plist-get state :server)))))

(defun jabber-disco--owner-current-p (owner)
  "Return non-nil if OWNER still identifies a connected session."
  (and owner (memq (car owner) jabber-connections)
       (equal owner (jabber-disco--owner (car owner)))))

(defun jabber-disco--cache-key (jc jid node)
  "Return JC's session-qualified observation key for JID and NODE."
  (list (jabber-disco--owner jc) jid node))

(defconst jabber-caps-hash-names
  '(("sha-1" . sha1)
    ("sha-224" . sha224)
    ("sha-256" . sha256)
    ("sha-384" . sha384)
    ("sha-512" . sha512))
  "Hash function name map.
Maps names defined in http://www.iana.org/assignments/hash-function-text-names
to symbols accepted by `secure-hash'.

XEP-0115 currently recommends SHA-1, but let's be future-proof.")

;; Keys are (OWNER JID NODE), where NODE may be nil.
;; Values are (identities features), where each identity is ["name"
;; "category" "type"], and each feature is a string.
(defvar jabber-disco-info-cache (make-hash-table :test 'equal))

;; Keys are (OWNER JID NODE).  Values are items, where each
;; item is ["name" "jid" "node"] (some values may be nil).
(defvar jabber-disco-items-cache (make-hash-table :test 'equal))

(defun jabber-disco--cache-observation (key value cache)
  "Store observation VALUE under KEY in CACHE, reclaiming retired owners.
Before each insertion, prune both observation tables against current session
ownership.  Thus retired generations cannot accumulate as observations arrive.
Leave other live owners and the global verified capabilities cache untouched."
  (dolist (table (list jabber-disco-info-cache jabber-disco-items-cache))
    (maphash (lambda (observation _value)
               (unless (jabber-disco--owner-current-p (car observation))
                 (remhash observation table)))
             table))
  (puthash key value cache))

(defvar jabber-advertised-features
  (list jabber-disco-xmlns-info
        jabber-disco-xmlns-items
        jabber-caps-xmlns)
  "Features advertised on service discovery requests.

Don't add your feature to this list directly.  Instead, call
`jabber-disco-advertise-feature'.")

(defvar jabber-disco-items-nodes
  (list
   (list "" nil nil))
  "Alist of node names and information about returning disco item data.
Key is node name as a string, or \"\" for no node specified.  Value is
a list of two items.

First item is data to return.  If it is a function, that function is
called and its return value is used; if it is a list, that list is
used.  The list should be the XML data to be returned inside the
<query/> element, like this:

\((item ((name . \"Name of first item\")
	(jid . \"first.item\")
	(node . \"node\"))))

Second item is access control function.  That function is passed the
JID, and returns non-nil if access is granted.  If the second item is
nil, access is always granted.")

(defvar jabber-disco-info-nodes
  (list
   (list "" #'jabber-disco-return-client-info nil))
  "Alist of node names and information returning disco info data.
Key is node name as a string, or \"\" for no node specified.  Value is
a list of two items.

First item is data to return.  If it is a function, that function is
called and its return value is used; if it is a list, that list is
used.  The list should be the XML data to be returned inside the
<query/> element, like this:

\((identity ((category . \"client\")
	    (type . \"pc\")
	    (name . \"Jabber client\")))
 (feature ((var . \"some-feature\"))))

Second item is access control function.  That function is passed the
JID, and returns non-nil if access is granted.  If the second item is
nil, access is always granted.")

(defvar jabber-presence-element-functions nil) ; jabber-presence.el

(defvar jabber-disco-features-changed-hook nil
  "Hook run after connected clients advertise a new feature.")

;;

(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons jabber-disco-xmlns-info 'jabber-return-disco-info))
(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons jabber-disco-xmlns-items 'jabber-return-disco-info))

(defun jabber-caps-get-cached (jid &optional jc)
  "Get verified capabilities for full JID observed on connection JC.
Return (IDENTITIES FEATURES), or nil if no owned observation is known.
An omitted JC never selects an ambient account."
  (when (jabber-disco--owner-current-p (jabber-disco--owner jc))
    (let* ((symbol (jabber-jid-symbol jid jc))
           (resource (or (jabber-jid-resource jid) ""))
           (resource-plist (cdr (assoc resource (get symbol 'resources))))
           (key (plist-get resource-plist 'caps))
           (owner (plist-get resource-plist 'caps-owner)))
      (when (and key (equal owner (jabber-disco--owner jc)))
        (gethash key jabber-caps-cache)))))

;;;###autoload
(defun jabber-process-caps (jc xml-data)
  "Look for entity capabilities in presence stanzas.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (type (jabber-xml-get-attribute xml-data 'type))
	 (c (jabber-xml-path xml-data `((,jabber-caps-xmlns . "c")))))
    (when (and (null type) c)
      (jabber-xml-let-attributes
	  (_ext hash node ver) c
	(cond
	 (hash
	  ;; If the <c/> element has a hash attribute, it follows the
	  ;; "modern" version of XEP-0115.
	  (jabber-process-caps-modern jc from hash node ver))
	 (t
	  ;; No hash attribute.  Use legacy version of XEP-0115.
	  ;; TODO: do something clever here.
	  ))))))

(defun jabber-caps--store-hash (jid key jc)
  "Store caps hash KEY for full JID observed on connection JC.
KEY is (HASH . VER).  Retain the session owner with the resource mapping."
  (let* ((symbol (jabber-jid-symbol jid jc))
         (resource (or (jabber-jid-resource jid) ""))
         (resource-entry (assoc resource (get symbol 'resources)))
         (properties (plist-put (plist-put (cdr resource-entry) 'caps key)
                                'caps-owner (jabber-disco--owner jc))))
    (if resource-entry
        (setcdr resource-entry properties)
      (push (cons resource properties) (get symbol 'resources)))
    (remhash (jabber-disco--cache-key jc jid nil) jabber-disco-info-cache)))

(defun jabber-caps--query-if-needed (jc jid hash node ver key cache-entry)
  "Resolve capabilities advertised by JID on JC as HASH, NODE and VER.
KEY is (HASH . VER); CACHE-ENTRY contains only verified capabilities.
Keep unverified candidates, including their owners, in a separate table."
  (let* ((owner (jabber-disco--owner jc))
         (info (or cache-entry (jabber-db-caps-lookup hash ver))))
    (when (jabber-disco--owner-current-p owner)
      (if info
          (progn
            (puthash key info jabber-caps-cache)
            (jabber-disco--cache-observation
             (list owner jid nil) info jabber-disco-info-cache))
        (let* ((candidate (list owner jid node))
               (pending (gethash key jabber-caps--pending)))
          (if pending
              (unless (equal candidate (plist-get pending :active))
                (cl-pushnew candidate (plist-get pending :queue) :test #'equal))
            (setq pending (list :active nil :queue (list candidate) :timer nil))
            (puthash key pending jabber-caps--pending)
            (jabber-caps-try-next key pending)))))))

(defun jabber-process-caps-modern (jc jid hash node ver)
  "Process capabilities advertised by JID on connection JC.
HASH, NODE and VER are the XEP-0115 advertisement fields."
  (when (and (assoc hash jabber-caps-hash-names)
             (stringp node) (stringp ver)
             (jabber-disco--owner-current-p (jabber-disco--owner jc)))
    (let ((key (cons hash ver)))
      (jabber-caps--store-hash jid key jc)
      (jabber-caps--query-if-needed
       jc jid hash node ver key (gethash key jabber-caps-cache)))))

(defun jabber-caps--active-p (key pending candidate)
  "Return non-nil if KEY still owns PENDING and its active CANDIDATE."
  (and (eq pending (gethash key jabber-caps--pending))
       (eq candidate (plist-get pending :active))))

(defun jabber-caps--cancel-attempt (pending)
  "Retire PENDING's active timer and exact IQ continuations."
  (when-let* ((timer (plist-get pending :timer)))
    (cancel-timer timer))
  (let ((candidate (plist-get pending :active)))
    (setq jabber-open-info-queries
          (cl-delete-if
           (lambda (query)
             (let ((callback (nth 1 query)))
               (and (eq (car-safe callback) #'jabber-process-caps-info-result)
                    (eq (nth 2 callback) pending)
                    (eq (nth 3 callback) candidate))))
           jabber-open-info-queries)))
  (setf (plist-get pending :active) nil
        (plist-get pending :timer) nil))

(defun jabber-caps--settle (key pending)
  "Remove the pending request for KEY if it still owns PENDING."
  (when (eq pending (gethash key jabber-caps--pending))
    (jabber-caps--cancel-attempt pending)
    (remhash key jabber-caps--pending)))

(defun jabber-process-caps-info-result (jc xml-data closure-data)
  "Verify caps XML-DATA received on JC for CLOSURE-DATA.
CLOSURE-DATA retains the hash key, pending request and owned candidate."
  (pcase-let ((`(,key ,pending ,candidate) closure-data))
    (when (and (jabber-caps--active-p key pending candidate)
               (eq jc (caar candidate)))
      (if (and (jabber-disco--owner-current-p (car candidate))
               (equal (cdr key)
                      (jabber-caps-ver-string (jabber-iq-query xml-data)
                                              (car key))))
          (let ((info (jabber-disco-parse-info xml-data)))
            (jabber-caps--settle key pending)
            (puthash key info jabber-caps-cache)
            (jabber-db-caps-store (car key) (cdr key) (car info) (cadr info)))
        (jabber-caps-try-next key pending)))))

(defun jabber-process-caps-info-error (jc _xml-data closure-data)
  "Advance the owned caps request in CLOSURE-DATA after an error on JC."
  (pcase-let ((`(,key ,pending ,candidate) closure-data))
    (when (and (jabber-caps--active-p key pending candidate)
               (eq jc (caar candidate)))
      (jabber-caps-try-next key pending))))

(defun jabber-caps--timeout (key pending candidate)
  "Advance KEY's PENDING request only if CANDIDATE still owns its timer."
  (when (jabber-caps--active-p key pending candidate)
    (jabber-caps-try-next key pending)))

(defun jabber-caps-try-next (key pending)
  "Query the next live candidate for KEY in the owned PENDING request.
Discard retired candidates.  Never reuse a failed candidate's connection
or advertised node, and expire each attempt after ten seconds."
  (when (eq pending (gethash key jabber-caps--pending))
    (jabber-caps--cancel-attempt pending)
    (let ((candidate (pop (plist-get pending :queue))))
      (while (and candidate
                  (not (jabber-disco--owner-current-p (car candidate))))
        (setq candidate (pop (plist-get pending :queue))))
      (if (null candidate)
          (jabber-caps--settle key pending)
        (setf (plist-get pending :active) candidate
              (plist-get pending :timer)
              (run-at-time 10 nil #'jabber-caps--timeout key pending candidate))
        (pcase-let ((`(,owner ,jid ,node) candidate))
          (condition-case err
              (jabber-send-iq
               (car owner) jid "get"
               `(query ((xmlns . ,jabber-disco-xmlns-info)
                        (node . ,(concat node "#" (cdr key)))))
               #'jabber-process-caps-info-result (list key pending candidate)
               #'jabber-process-caps-info-error (list key pending candidate))
            (error
             (when (jabber-caps--active-p key pending candidate)
               (jabber-caps-try-next key pending))
             (message "Capabilities query failed: %s" (error-message-string err)))
            (quit
             (when (jabber-caps--active-p key pending candidate)
               (jabber-caps--settle key pending))
             (signal (car err) (cdr err)))))))))

(defun jabber-caps--identity-string (identities)
  "Build the identity portion of a caps verification string.
IDENTITIES is a list of <identity> XML nodes.
Return the concatenated sorted identity entries."
  (mapconcat
   (lambda (identity)
     (jabber-xml-let-attributes (category type xml:lang name) identity
       (concat category "/" type "/" xml:lang "/" name "<")))
   (sort identities #'jabber-caps-identity-<)))

(defun jabber-caps--feature-string (features)
  "Build the feature portion of a caps verification string.
FEATURES is a list of feature var strings.
Return the concatenated sorted feature entries."
  (mapconcat (lambda (f) (concat f "<"))
             (sort features #'string<)))

(defun jabber-caps--form-string (forms)
  "Build the XEP-0128 data form portion of a caps verification string.
FORMS is a list of <x> XML nodes (already filtered for FORM_TYPE).
Return the concatenated sorted form entries."
  (let ((sorted (sort forms (lambda (a b)
                              (string< (jabber-xdata-form-type a)
                                       (jabber-xdata-form-type b))))))
    (mapconcat
     (lambda (form)
       (let ((fields (sort (jabber-xml-get-children form 'field)
                           (lambda (a b)
                             (string< (jabber-xml-get-attribute a 'var)
                                      (jabber-xml-get-attribute b 'var))))))
         (concat
          (jabber-xdata-form-type form) "<"
          (mapconcat
           (lambda (field)
             (if (string= (jabber-xml-get-attribute field 'var) "FORM_TYPE")
                 ""
               (let ((values (sort (mapcar (lambda (v)
                                             (car (jabber-xml-node-children v)))
                                           (jabber-xml-get-children field 'value))
                                   #'string<)))
                 (concat (jabber-xml-get-attribute field 'var) "<"
                         (mapconcat (lambda (v) (concat (or v "") "<"))
                                    values)))))
           fields))))
     sorted)))

(defun jabber-caps-ver-string (query hash)
  "Create an XEP-0115 version string for a QUERY node with a specified HASH."
  ;; XEP-0115, section 5.1
  (let* ((identities (jabber-xml-get-children query 'identity))
	 (features (mapcar (lambda (f) (jabber-xml-get-attribute f 'var))
			   (jabber-xml-get-children query 'feature)))
	 (forms (cl-remove-if-not
		 (lambda (x)
		   (and (string= (jabber-xml-get-xmlns x) jabber-xdata-xmlns)
			(jabber-xdata-form-type x)))
		 (jabber-xml-get-children query 'x)))
	 (s (encode-coding-string
	     (concat (jabber-caps--identity-string identities)
		     (jabber-caps--feature-string features)
		     (jabber-caps--form-string forms))
	     'utf-8 t))
	 (algorithm (cdr (assoc hash jabber-caps-hash-names))))
    (base64-encode-string (jabber-caps--secure-hash algorithm s) t)))

(defun jabber-caps--secure-hash (algorithm string)
  "Compute and return a secure hash from STRING using ALGORITHM."
  (secure-hash algorithm string nil nil t))

(defun jabber-caps-identity-< (a b)
  "Compare two Jabber identity XML elements A and B, return t if A < B."
  (let ((a-category (jabber-xml-get-attribute a 'category))
	(b-category (jabber-xml-get-attribute b 'category)))
    (or (string< a-category b-category)
	(and (string= a-category b-category)
	     (let ((a-type (jabber-xml-get-attribute a 'type))
		   (b-type (jabber-xml-get-attribute b 'type)))
	       (or (string< a-type b-type)
		   (and (string= a-type b-type)
			(let ((a-xml:lang (jabber-xml-get-attribute a 'xml:lang))
			      (b-xml:lang (jabber-xml-get-attribute b 'xml:lang)))
			  (string< a-xml:lang b-xml:lang)))))))))

(defvar jabber-caps-default-hash-function "sha-1"
  "Hash function to use when sending caps in presence stanzas.
The value should be a key in `jabber-caps-hash-names'.")

(defvar jabber-caps-current-hash nil
  "The current disco hash we're sending out in presence stanzas.")

(defconst jabber-caps-node "http://emacs-jabber.sourceforge.net")

;;;###autoload
(defun jabber-disco-advertise-feature (feature)
  "Add a new FEATURE to `jabber-advertised-features', if not already present."
  (unless (member feature jabber-advertised-features)
    (push feature jabber-advertised-features)
    (when jabber-caps-current-hash
      (jabber-caps-recalculate-hash)
      (run-hooks 'jabber-disco-features-changed-hook))))

(defun jabber-caps-recalculate-hash ()
  "Update `jabber-caps-current-hash' for feature list change.
Also update `jabber-disco-info-nodes', so we return results for
the right node."
  (let* ((old-hash jabber-caps-current-hash)
	 (old-node (and old-hash (concat jabber-caps-node "#" old-hash)))
	 (new-hash
	  (jabber-caps-ver-string `(query () ,@(jabber-disco-return-client-info))
				  jabber-caps-default-hash-function))
	 (new-node (concat jabber-caps-node "#" new-hash)))
    (when old-node
      (let ((old-entry (assoc old-node jabber-disco-info-nodes)))
	(when old-entry
	  (setq jabber-disco-info-nodes (delq old-entry jabber-disco-info-nodes)))))
    (push (list new-node #'jabber-disco-return-client-info nil)
	  jabber-disco-info-nodes)
    (setq jabber-caps-current-hash new-hash)))

;;;###autoload
(defun jabber-caps-presence-element (_jc)
  "Generate XML presence element using `jabber-caps-current-hash' and _JC param."
  (unless jabber-caps-current-hash
    (jabber-caps-recalculate-hash))
  (list
   `(c ((xmlns . ,jabber-caps-xmlns)
	(hash . ,jabber-caps-default-hash-function)
	(node . ,jabber-caps-node)
	(ver . ,jabber-caps-current-hash)))))

(add-to-list 'jabber-presence-element-functions #'jabber-caps-presence-element)

(defun jabber-return-disco-info (jc xml-data)
  "Respond to a service discovery request.
See XEP-0030.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((to (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (xmlns (jabber-iq-xmlns xml-data))
	 (which-alist (cond
		       ((string= xmlns jabber-disco-xmlns-info) jabber-disco-info-nodes)
		       ((string= xmlns jabber-disco-xmlns-items) jabber-disco-items-nodes)))
	 (node (or
		(jabber-xml-get-attribute (jabber-iq-query xml-data) 'node)
		""))
	 (return-list (cdr (assoc node which-alist)))
	 (func (nth 0 return-list))
	 (access-control (nth 1 return-list)))
    (if return-list
	(if (and (functionp access-control)
		 (not (funcall access-control jc to)))
	    (jabber-signal-error "Cancel" 'not-allowed)
	  ;; Access control passed
	  (let ((result (if (functionp func)
			    (funcall func jc xml-data)
			  func)))
	    (jabber-send-iq jc to "result"
			    `(query ((xmlns . ,xmlns)
				     ,@(when node
					 (list (cons 'node node))))
				    ,@result)
			    nil nil nil nil id)))

      ;; No such node
      (jabber-signal-error "Cancel" 'item-not-found))))

(defun jabber-disco-return-client-info (&optional _jc _xml-data)
  "Return a Jabber Disco information according to the client env.

Generate a list which represents the identity and
features supported by the Emacs Jabber client.

The type of the client is decided based on the window system.

If Emacs is running under a window system (x, w32, mac, ns), the type
is classified as pc, otherwise console."
  `(
    ;; If running under a window system, this is
    ;; a GUI client.  If not, it is a console client.
    (identity ((category . "client")
	       (name . "Emacs Jabber client")
	       (type . ,(if (memq window-system
				  '(x w32 mac ns))
			    "pc"
			  "console"))))
    ,@(mapcar
       #'(lambda (featurename)
	   `(feature ((var . ,featurename))))
       jabber-advertised-features)))

(defun jabber-get-disco-items (jc to &optional node)
  "Send a service discovery request for items.

JC, the Jabber connection, is typically required to be active.
TO is the JID (Jabber ID) of the entity to request items from.
NODE is an optional parameter specifying a particular node to request items for."
  (interactive
   (let ((jc (jabber-read-account)))
     (list jc
	   (jabber-read-jid-completing "Send items disco request to: " nil nil nil 'full t jc)
	   (jabber-read-node "Node (or leave empty): "))))
  (jabber-send-iq jc to
		  "get"
		  (list 'query (append (list (cons 'xmlns jabber-disco-xmlns-items))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-items
		  #'jabber-process-data "Item discovery failed"))

(defun jabber-get-disco-info (jc to &optional node)
  "Send a service discovery request for info.

JC is the Jabber connection.
TO is the JID (Jabber ID) of the entity to request items from.
NODE is an optional parameter specifying a particular node to request
items for."
  (interactive
   (let ((jc (jabber-read-account)))
     (list jc
	   (jabber-read-jid-completing "Send info disco request to: " nil nil nil 'full t jc)
	   (jabber-read-node "Node (or leave empty): "))))
  (jabber-send-iq jc to
		  "get"
		  (list 'query (append (list (cons 'xmlns jabber-disco-xmlns-info))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-info
		  #'jabber-process-data "Info discovery failed"))

(defun jabber-process-disco-info (jc xml-data)
  "Handle results from info disco requests.
JC is the Jabber connection.  XML-DATA is the IQ result stanza.
Return a formatted string with identities and features."
  (let ((result
         (with-temp-buffer
           (dolist (x (jabber-xml-node-children (jabber-iq-query xml-data)))
             (cond
              ((eq (jabber-xml-node-name x) 'identity)
               (let ((name (jabber-xml-get-attribute x 'name))
                     (category (jabber-xml-get-attribute x 'category))
                     (type (jabber-xml-get-attribute x 'type)))
                 (insert (propertize (or name
                                         (concat category
                                                 (when type (concat " (" type ")"))))
                                     'face 'jabber-title)
                         "\n\n")
                 (when type
                   (insert "Type:\t\t" type "\n"))
                 (insert "\n")))
              ((eq (jabber-xml-node-name x) 'feature)
               (let ((var (jabber-xml-get-attribute x 'var)))
                 (insert "Feature:\t" var "\n")))))
           (buffer-string))))
    (when (length> result 0)
      (put-text-property 0 (length result) 'jabber-jid
                         (jabber-xml-get-attribute xml-data 'from) result)
      (put-text-property 0 (length result) 'jabber-account jc result)
      result)))

(defun jabber-process-disco-items (jc xml-data)
  "Handle results from items disco requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let ((items (jabber-xml-get-children (jabber-iq-query xml-data) 'item)))
    (if items
	(dolist (item items)
	  (let ((jid (jabber-xml-get-attribute item 'jid))
		(name (jabber-xml-get-attribute item 'name))
		(node (jabber-xml-get-attribute item 'node)))
	    (insert
	     (propertize
	      (concat
	       (propertize
		(concat jid "\n" (if node (format "Node: %s\n" node)))
		'face 'jabber-title)
	       name "\n\n")
	      'jabber-jid jid
	      'jabber-account jc
	      'jabber-node node))))
      (insert "No items found.\n"))))

(defun jabber-disco--owned-callback (jc callback)
  "Return CALLBACK guarded by JC's captured session and receiving owner."
  (let ((owner (jabber-disco--owner jc)))
    (lambda (received-jc arg1 arg2)
      (when (and (eq received-jc jc) (jabber-disco--owner-current-p owner))
        (funcall callback jc arg1 arg2)))))

(defun jabber-disco--got-error (jc xml-data callback-data)
  "Deliver the error in XML-DATA on JC to CALLBACK-DATA."
  (when (car callback-data)
    (funcall (car callback-data) jc (cdr callback-data)
             (jabber-iq-error xml-data))))

(defun jabber-disco-get-info (jc jid node callback closure-data &optional force
                                 response-predicate)
  "Get disco info for JID and NODE, using connection JC.

Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and result as third argument when result is available.
On success, result is (IDENTITIES FEATURES), where each identity is [\"name\"
\"category\" \"type\"], and each feature is a string.
On error, result is the error node, recognizable by (eq (car result) \\='error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate only this session's cache and get fresh data.
Discard callbacks after JC is removed or its transport or stream changes.
RESPONSE-PREDICATE is passed to `jabber-send-iq'; when supplied, bypass
the shared cache so that only an admitted wire response supplies the result."
  (when force
    (remhash (jabber-disco--cache-key jc jid node) jabber-disco-info-cache))
  (let ((result (unless (or force response-predicate)
                  (jabber-disco-get-info-immediately jid node jc))))
    (if result
	(and callback
             (run-with-timer 0 nil (jabber-disco--owned-callback jc callback)
                             jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . ,jabber-disco-xmlns-info)
			       ,@(when node `((node . ,node)))))
		      (jabber-disco--owned-callback jc #'jabber-disco-got-info)
                      (cons callback closure-data)
		      (jabber-disco--owned-callback jc #'jabber-disco--got-error)
		      (cons callback closure-data) nil response-predicate))))

(defun jabber-disco-got-info (jc xml-data callback-data)
  "Process the received jabber-disco info query response.

Parse received disco-info from XML-DATA and caches
it.  If a CALLBACK-DATA function is provided, it's called with the
JC, CALLBACK-DATA and RESULT.

JC: The jabber connection.
XML-DATA: The XML data containing the info query response.
CALLBACK-DATA: Optional function to be triggered after processing info
query response."
  (let ((jid (jabber-xml-get-attribute xml-data 'from))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data)
					'node))
	(result (jabber-disco-parse-info xml-data)))
    (jabber-disco--cache-observation (jabber-disco--cache-key jc jid node)
                                   result jabber-disco-info-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))

(defun jabber-disco-parse-info (xml-data)
  "Extract data from an <iq/> stanza containing a disco#info result.
See `jabber-disco-get-info' for a description of the return value.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (list
   (mapcar
    #'(lambda (id)
	(vector (jabber-xml-get-attribute id 'name)
		(jabber-xml-get-attribute id 'category)
		(jabber-xml-get-attribute id 'type)))
    (jabber-xml-get-children (jabber-iq-query xml-data) 'identity))
   (mapcar
    #'(lambda (feature)
	(jabber-xml-get-attribute feature 'var))
    (jabber-xml-get-children (jabber-iq-query xml-data) 'feature))
   (cl-remove-if-not
    (lambda (x)
      (string= (jabber-xml-get-xmlns x) jabber-xdata-xmlns))
    (jabber-xml-get-children (jabber-iq-query xml-data) 'x))))

(defun jabber-disco-get-info-immediately (jid node &optional jc)
  "Get cached disco info for JID and NODE observed on connection JC.
Return nil if no owned info is available; never select an ambient account.
Fill the cache with `jabber-disco-get-info'."
  (when (jabber-disco--owner-current-p (jabber-disco--owner jc))
    (or (gethash (jabber-disco--cache-key jc jid node) jabber-disco-info-cache)
        (and (null node) (jabber-caps-get-cached jid jc)))))

(defun jabber-disco-get-items (jc jid node callback closure-data &optional force)
  "Get disco items for JID and NODE, using connection JC.

Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and items result as third argument when result is
available.
On success, result is a list of items, where each
item is [\"name\" \"jid\" \"node\"] (some values may be nil).
On error, result is the error node, recognizable by (eq (car result) \\='error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate only this session's cache and get fresh data.
Discard callbacks after JC is removed or its transport or stream changes."
  (when force
    (remhash (jabber-disco--cache-key jc jid node) jabber-disco-items-cache))
  (let ((result (gethash (jabber-disco--cache-key jc jid node)
                         jabber-disco-items-cache)))
    (if result
	(and callback
             (run-with-timer 0 nil (jabber-disco--owned-callback jc callback)
                             jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . ,jabber-disco-xmlns-items)
			       ,@(when node `((node . ,node)))))
		      (jabber-disco--owned-callback jc #'jabber-disco-got-items)
                      (cons callback closure-data)
		      (jabber-disco--owned-callback jc #'jabber-disco--got-error)
		      (cons callback closure-data)))))

(defun jabber-disco-got-items (jc xml-data callback-data)
  "Process received Jabber disco items.

Processes the received disco items XML-DATA from the
Jabber connection JC & updates the disco items cache.

If a callback function is provided in CALLBACK-DATA, it will then be
called with JC, the remaining CALLBACK-DATA, and the obtained RESULT."
  (let ((jid (jabber-xml-get-attribute xml-data 'from))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data)
					'node))
	(result
	 (mapcar
	  #'(lambda (item)
	      (vector
	       (jabber-xml-get-attribute item 'name)
	       (jabber-xml-get-attribute item 'jid)
	       (jabber-xml-get-attribute item 'node)))
	  (jabber-xml-get-children (jabber-iq-query xml-data) 'item))))
    (jabber-disco--cache-observation (jabber-disco--cache-key jc jid node)
                                   result jabber-disco-items-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))

(provide 'jabber-disco)
;;; jabber-disco.el ends here.
