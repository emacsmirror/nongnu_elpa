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

(require 'jabber-iq)
(require 'jabber-xml)
(require 'jabber-menu)

;; Global reference declarations

(declare-function jabber-chain-add "jabber-core"
                  (chain-var handler &optional depth))
(declare-function jabber-db-caps-store "jabber-db.el"
                  (hash ver identities features))
(declare-function jabber-db-caps-lookup "jabber-db.el" (hash ver))

(defvar jabber-presence-chain)          ; jabber-core.el
(defvar jabber-connections)             ; jabber-core.el
(defvar jabber-xdata-xmlns)            ; jabber-xml.el

(defconst jabber-disco-xmlns-info "http://jabber.org/protocol/disco#info"
  "XEP-0030 Service Discovery info namespace.")

(defconst jabber-disco-xmlns-items "http://jabber.org/protocol/disco#items"
  "XEP-0030 Service Discovery items namespace.")

(defconst jabber-caps-xmlns "http://jabber.org/protocol/caps"
  "XEP-0115 Entity Capabilities namespace.")

;;
;;; Respond to disco requests

(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-presence-chain #'jabber-process-caps 10))

(defvar jabber-caps-cache (make-hash-table :test 'equal))

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

;; Keys are ("jid" . "node"), where "node" is nil if appropriate.
;; Values are (identities features), where each identity is ["name"
;; "category" "type"], and each feature is a string.
(defvar jabber-disco-info-cache (make-hash-table :test 'equal))

;; Keys are ("jid" . "node").  Values are (items), where each
;; item is ["name" "jid" "node"] (some values may be nil).
(defvar jabber-disco-items-cache (make-hash-table :test 'equal))

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

;; Global reference declarations

(declare-function jabber-send-current-presence "jabber-presence.el"
                  (&optional jc))
(declare-function jabber-widget-xdata-formtype "jabber-widget.el" (x))
(defvar jabber-presence-element-functions) ; jabber-presence.el

;;

(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons jabber-disco-xmlns-info 'jabber-return-disco-info))
(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons jabber-disco-xmlns-items 'jabber-return-disco-info))

(defun jabber-caps-get-cached (jid)
  "Get disco info from Entity Capabilities cache.
JID should be a string containing a full JID.
Return (IDENTITIES FEATURES), or nil if not in cache."
  (let* ((symbol (jabber-jid-symbol jid))
	 (resource (or (jabber-jid-resource jid) ""))
	 (resource-plist (cdr (assoc resource (get symbol 'resources))))
	 (key (plist-get resource-plist 'caps)))
    (when key
      (let ((cache-entry (gethash key jabber-caps-cache)))
	(when (and (consp cache-entry) (not (floatp (car cache-entry))))
	  cache-entry)))))

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

(defun jabber-caps--store-hash (jid key)
  "Store caps hash KEY in the resource plist for JID.
KEY is a cons cell (HASH . VER) identifying the entity capabilities.
JID is a full JID string; the resource portion is used as the
key in the symbol's `resources' property."
  (let* ((symbol (jabber-jid-symbol jid))
	 (resource (or (jabber-jid-resource jid) ""))
	 (resource-entry (assoc resource (get symbol 'resources)))
	 (new-resource-plist (plist-put (cdr resource-entry) 'caps key)))
    (if resource-entry
	(setf (cdr resource-entry) new-resource-plist)
      (push (cons resource new-resource-plist) (get symbol 'resources)))))

(defun jabber-caps--query-if-needed (jc jid hash node ver key cache-entry)
  "Decide whether to send a disco#info query for entity capabilities.
JC is the Jabber connection.  JID is the full JID of the entity.
HASH, NODE, and VER are the XEP-0115 capability fields.
KEY is (HASH . VER), the cache key.  CACHE-ENTRY is the current
value in `jabber-caps-cache' for KEY.

When CACHE-ENTRY is a pending query (timestamp float), either
add JID to the fallback list or re-query if the timeout (10s)
has elapsed.  When CACHE-ENTRY is nil, record a pending query
and send a disco#info request.  Otherwise, copy the cached
capabilities into `jabber-disco-info-cache' for JID."
  (cl-flet ((request-disco-info
	      ()
	      (jabber-send-iq
	       jc jid
	       "get"
	       `(query ((xmlns . ,jabber-disco-xmlns-info)
			(node . ,(concat node "#" ver))))
	       #'jabber-process-caps-info-result (list hash node ver)
	       #'jabber-process-caps-info-error (list hash node ver))))
    (cond
     ((and (consp cache-entry)
	   (floatp (car cache-entry)))
      ;; We have a record of asking someone about this hash.
      (if (< (- (float-time) (car cache-entry)) 10.0)
	  ;; We asked someone about this hash less than 10 seconds ago.
	  ;; Let's add the new JID to the entry, just in case that
	  ;; doesn't work out.
	  (cl-pushnew jid (cdr cache-entry) :test #'string=)
	;; We asked someone about it more than 10 seconds ago.
	;; They're probably not going to answer.  Let's ask
	;; this contact about it instead.
	(setf (car cache-entry) (float-time))
	(request-disco-info)))
     ((null cache-entry)
      ;; Check persistent storage before querying the network.
      (let ((db-entry (jabber-db-caps-lookup hash ver)))
        (if db-entry
            (progn
              (puthash key db-entry jabber-caps-cache)
              (puthash (cons jid nil) db-entry jabber-disco-info-cache))
          (puthash key (list (float-time)) jabber-caps-cache)
          (request-disco-info))))
     (t
      ;; We already know what this hash represents, so we
      ;; can cache info for this contact.
      (puthash (cons jid nil) cache-entry jabber-disco-info-cache)))))

(defun jabber-process-caps-modern (jc jid hash node ver)
  "Processes the capabilities of a contact which supports XEP-0115 v1.5 or later.

JC is the jabber connection of the sender,
JID is the Jabber ID of the entity sending the capabilities information.
HASH is the generated hash representing the sender's capabilities.
NODE is the namespace of the format.
and VER is the entity's version number."
  (when (assoc hash jabber-caps-hash-names)
    ;; We support the hash function used.
    (let* ((key (cons hash ver))
	   (cache-entry (gethash key jabber-caps-cache)))
      (jabber-caps--store-hash jid key)
      (jabber-caps--query-if-needed jc jid hash node ver key cache-entry))))

(defun jabber-process-caps-info-result (jc xml-data closure-data)
  "Process the result of a jabber server's caps info request.

JC is the jabber connection.
XML-DATA is the XML data received from the server.
CLOSURE-DATA is in the format of (HASH NODE VER), where HASH is the
verification hash received from the server.
NODE represents the software identification, and VER is the software version.

If the verification string matches with VER, the software's discovery
/disco/ information will be stored in the jabber-caps-cache,
otherwise, it will try the next available option."
  (pcase-let* ((`(,hash ,node ,ver) closure-data)
	       (key (cons hash ver))
	       (query (jabber-iq-query xml-data))
	       (verification-string (jabber-caps-ver-string query hash)))
    (if (string= ver verification-string)
	;; The hash is correct; save info.
	(let ((info (jabber-disco-parse-info xml-data)))
	  (puthash key info jabber-caps-cache)
	  (jabber-db-caps-store hash ver (car info) (cadr info)))
      ;; The hash is incorrect.
      (jabber-caps-try-next jc hash node ver))))

(defun jabber-process-caps-info-error (jc _xml-data closure-data)
  "Process error in caps info for Jabber.

JC is the Jabber connection.

CLOSURE-DATA is a list of three parameters: hash, node, and version.

This function makes another attempt to process the caps info when an
error occurs."
  (pcase-let ((`(,hash ,node ,ver) closure-data))
    (jabber-caps-try-next jc hash node ver)))

(defun jabber-caps-try-next (jc hash node ver)
  "Try the next JID for a cached entry in Jabber CAPS Cache.

JC is the Jabber connection.
HASH is the hash value of the CAPS.
NODE is the node identifier in the XEP-0115 specification.
VER is the version string of the CAPS."
  (let* ((key (cons hash ver))
	 (cache-entry (gethash key jabber-caps-cache)))
    (when (floatp (car-safe cache-entry))
      (let ((next-jid (pop (cdr cache-entry))))
	;; Do we know someone else we could ask about this hash?
	(if next-jid
	    (progn
	      (setf (car cache-entry) (float-time))
	      (jabber-send-iq
	       jc next-jid
	       "get"
	       `(query ((xmlns . ,jabber-disco-xmlns-info)
			(node . ,(concat node "#" ver))))
	       #'jabber-process-caps-info-result (list hash node ver)
	       #'jabber-process-caps-info-error (list hash node ver)))
	  ;; No, forget about it for now.
	  (remhash key jabber-caps-cache))))))

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
                              (string< (jabber-widget-xdata-formtype a)
                                       (jabber-widget-xdata-formtype b))))))
    (mapconcat
     (lambda (form)
       (let ((fields (sort (jabber-xml-get-children form 'field)
                           (lambda (a b)
                             (string< (jabber-xml-get-attribute a 'var)
                                      (jabber-xml-get-attribute b 'var))))))
         (concat
          (jabber-widget-xdata-formtype form) "<"
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
			(jabber-widget-xdata-formtype x)))
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
      ;; If we're already connected, we need to send updated presence
      ;; for the new feature.
      (mapc #'jabber-send-current-presence jabber-connections))))

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

;;;###autoload
(with-eval-after-load "jabber-presence"
  (add-to-list 'jabber-presence-element-functions #'jabber-caps-presence-element))

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
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Send items disco request to: " nil nil nil 'full t)
		     (jabber-read-node "Node (or leave empty): ")))
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
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Send info disco request to: " nil nil nil 'full t)
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq jc to
		  "get"
		  (list 'query (append (list (cons 'xmlns jabber-disco-xmlns-info))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-info
		  #'jabber-process-data "Info discovery failed"))

(defun jabber-process-disco-info (jc xml-data)
  "Handle results from info disco requests.
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

(defun jabber-disco-get-info (jc jid node callback closure-data &optional force)
  "Get disco info for JID and NODE, using connection JC.

Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and result as third argument when result is available.
On success, result is (IDENTITIES FEATURES), where each identity is [\"name\"
\"category\" \"type\"], and each feature is a string.
On error, result is the error node, recognizable by (eq (car result) \\='error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate cache and get fresh data."
  (when force
    (remhash (cons jid node) jabber-disco-info-cache))
  (let ((result (unless force (jabber-disco-get-info-immediately jid node))))
    (if result
	(and callback (run-with-timer 0 nil callback jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . ,jabber-disco-xmlns-info)
			       ,@(when node `((node . ,node)))))
		      #'jabber-disco-got-info (cons callback closure-data)
		      (lambda (jc xml-data callback-data)
			(when (car callback-data)
			  (funcall (car callback-data) jc (cdr callback-data) (jabber-iq-error xml-data))))
		      (cons callback closure-data)))))

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
    (puthash (cons jid node) result jabber-disco-info-cache)
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
    (jabber-xml-get-children (jabber-iq-query xml-data) 'feature))))

(defun jabber-disco-get-info-immediately (jid node)
  "Get cached disco info for JID and NODE.
Return nil if no info available.

Fill the cache with `jabber-disco-get-info'."
  (or
   ;; Check "normal" cache...
   (gethash (cons jid node) jabber-disco-info-cache)
   ;; And then check Entity Capabilities.
   (and (null node) (jabber-caps-get-cached jid))))

(defun jabber-disco-get-items (jc jid node callback closure-data &optional force)
  "Get disco items for JID and NODE, using connection JC.

Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and items result as third argument when result is
available.
On success, result is a list of items, where each
item is [\"name\" \"jid\" \"node\"] (some values may be nil).
On error, result is the error node, recognizable by (eq (car result) \='error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate cache and get fresh data."
  (when force
    (remhash (cons jid node) jabber-disco-items-cache))
  (let ((result (gethash (cons jid node) jabber-disco-items-cache)))
    (if result
	(and callback (run-with-timer 0 nil callback jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . ,jabber-disco-xmlns-items)
			       ,@(when node `((node . ,node)))))
		      #'jabber-disco-got-items (cons callback closure-data)
		      (lambda (jc xml-data callback-data)
			(when (car callback-data)
			  (funcall (car callback-data) jc (cdr callback-data) (jabber-iq-error xml-data))))
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
    (puthash (cons jid node) result jabber-disco-items-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))

(defun jabber-disco-get-items-immediately (jid node)
  "Retrieve items from `jabber-disco-items-cache' using JID & NODE as key."
  (gethash (cons jid node) jabber-disco-items-cache))

(defun jabber-disco-publish (jc node item-name item-jid item-node)
  "Publish the given item under disco node NODE.

JC is the Jabber connection."
  (jabber-send-iq jc nil
		  "set"
		  `(query ((xmlns . ,jabber-disco-xmlns-items)
			   ,@(when node `((node . ,node))))
			  (item ((action . "update")
				 (jid . ,item-jid)
				 ,@(when item-name
				     `((name . ,item-name)))
				 ,@(when item-node
				     `((node . ,item-node))))))
		  'jabber-report-success "Disco publish"
		  'jabber-report-success "Disco publish"))

(defun jabber-disco-publish-remove (jc node item-jid item-node)
  "Remove the given item from published disco items.

JC: Jabber Client connection.
NODE: Disco node to remove item from.  Can be nil.
ITEM-JID: JID (Jabber ID) of the disco item to be removed.
ITEM-NODE: Specific node of the disco item to be removed.  Can be nil."
  (jabber-send-iq jc nil
		  "set"
		  `(query ((xmlns . ,jabber-disco-xmlns-items)
			   ,@(when node `((node . ,node))))
			  (item ((action . "remove")
				 (jid . ,item-jid)
				 ,@(when item-node
				     `((node . ,item-node))))))
		  'jabber-report-success "Disco removal"
		  'jabber-report-success "Disco removal"))

(provide 'jabber-disco)
;;; jabber-disco.el ends here.
