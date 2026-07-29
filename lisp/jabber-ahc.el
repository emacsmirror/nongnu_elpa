;;; jabber-ahc.el --- Ad-Hoc Commands by JEP-0050  -*- lexical-binding: t; -*-

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
;;

;;; Code:

(require 'keymap-popup)
(require 'jabber-disco)
(require 'jabber-widget)
(require 'jabber-xdata-form)

(defconst jabber-ahc-xmlns "http://jabber.org/protocol/commands"
  "XEP-0050 Ad-Hoc Commands namespace.")

(defvar jabber-ahc-sessionid nil
  "Session ID of Ad-Hoc Command session.")

(defvar jabber-ahc-node nil
  "Node to send commands to.")

(defvar-local jabber-ahc--submit-to nil
  "JID receiving commands from the current ad-hoc session.")

(defvar-local jabber-ahc--has-form nil
  "Whether the current ad-hoc step supplied an editable data form.")

(defvar jabber-ahc-commands nil
  "Alist of ad-hoc commands provided.

The keys are node names as strings (which means that they must
not conflict).  The values are plists having the following properties -

acl     - function taking connection object and JID of requester,
	  returning non-nil for access allowed.  No function means
          open for everyone.
name	- name of command
func	- function taking connection object and entire IQ stanza as
          arguments and returning a <command/> node

Use the function `jabber-ahc-add' to add a command to this list.")

;; Global reference declarations

(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-xdata-xmlns)            ; jabber-xml.el

;;

;;; SERVER
(add-to-list 'jabber-disco-info-nodes
	     (list jabber-ahc-xmlns
		   `((identity ((category . "automation")
				(type . "command-list")
				(name . "Ad-Hoc Command list")))
		     (feature ((var . ,jabber-ahc-xmlns)))
		     (feature ((var . ,jabber-disco-xmlns-items)))
		     (feature
		      ((var . ,jabber-disco-xmlns-info))))))

(defun jabber-ahc-add (node name func acl)
  "Add a command to internal lists.
NODE is the node name to be used.  It must be unique.
NAME is the natural-language name of the command.
FUNC is a function taking the entire IQ stanza as single argument when
this command is invoked, and returns a <command/> node.
ACL is a function taking JID as single argument, returning non-nil for
access allowed.  nil means open for everyone."
  (add-to-list 'jabber-ahc-commands (cons node (list 'name name
						     'func func
						     'acl acl)))
  (add-to-list 'jabber-disco-info-nodes
	       (list node `((identity ((category . "automation")
				       (type . "command-node")
				       (name . ,name)))
			    (feature ((var . ,jabber-ahc-xmlns)))
			    (feature ((var . ,jabber-disco-xmlns-info)))
			    (feature ((var . ,jabber-xdata-xmlns)))))))

(jabber-disco-advertise-feature jabber-ahc-xmlns)
(add-to-list 'jabber-disco-items-nodes
	     (list jabber-ahc-xmlns #'jabber-ahc-disco-items nil))
(defun jabber-ahc-disco-items (jc xml-data)
  "Return commands in response to disco#items request.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((jid (jabber-xml-get-attribute xml-data 'from)))
    (mapcar (function
	     (lambda (command)
	       (let ((node (car command))
		     (plist (cdr command)))
		 (let ((acl (plist-get plist 'acl))
		       (name (plist-get plist 'name)))
		   (when (or (not (functionp acl))
			     (funcall acl jc jid))
		     `(item ((name . ,name)
			     (jid . ,(jabber-connection-jid jc))
			     (node . ,node))))))))
	    jabber-ahc-commands)))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons jabber-ahc-xmlns 'jabber-ahc-process))
(defun jabber-ahc-process (jc xml-data)
  "Dispatch an inbound ad-hoc-command IQ over JC.
XML-DATA is the IQ stanza."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data) 'node)))
    ;; find command
    (let* ((plist (cdr (assoc node jabber-ahc-commands)))
	   (acl (plist-get plist 'acl))
	   (func (plist-get plist 'func)))
      (if plist
	  ;; found
	  (if (or (not (functionp acl))
		  (funcall acl jc to))
	      ;; access control passed
	      (jabber-send-iq jc to "result"
			      (funcall func jc xml-data)
			      nil nil nil nil id)
	    ;; ...or failed
	    (jabber-signal-error "Cancel" 'not-allowed))
	;; No such node
	(jabber-signal-error "Cancel" 'item-not-found)))))

;;; CLIENT
(defconst jabber-ahc--command-page-size 4
  "Number of discovered commands displayed on one popup page.")

(defvar-local jabber-ahc--command-items nil
  "Discovered commands available in the current buffer.")

(defvar-local jabber-ahc--command-connection nil
  "Connection used to discover commands in the current buffer.")

(defvar-local jabber-ahc--command-target nil
  "JID queried for commands in the current buffer.")

(defvar-local jabber-ahc--command-page 0
  "Index of the displayed command page in the current buffer.")

(defvar jabber-ahc-command-list-map)

(defun jabber-ahc-get-list (jc to)
  "Request list of ad-hoc commands from TO.

See XEP-0050.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Request command list from: " nil nil nil nil nil)))
  (jabber-disco-get-items
   jc to jabber-ahc-xmlns
   #'jabber-ahc--command-list-result
   (list (current-buffer) to)))

(defun jabber-ahc--command-list-map (jc to items)
  "Prepare and return the popup map for XEP-0050 ITEMS from TO over JC."
  (setq-local jabber-ahc--command-items
              (seq-filter (lambda (item)
                            (and (aref item 1) (aref item 2)))
                          items))
  (setq-local jabber-ahc--command-connection jc)
  (setq-local jabber-ahc--command-target to)
  (setq-local jabber-ahc--command-page 0)
  jabber-ahc-command-list-map)

(defun jabber-ahc--command-at (slot)
  "Return command at zero-based SLOT on the current page."
  (nth (+ slot (* jabber-ahc--command-page
                  jabber-ahc--command-page-size))
       jabber-ahc--command-items))

(defun jabber-ahc--command-description (slot)
  "Return the description for command SLOT on the current page."
  (when-let* ((item (jabber-ahc--command-at slot)))
    (or (aref item 0) (aref item 2))))

(defun jabber-ahc--execute-slot (slot)
  "Execute command SLOT on the current page."
  (when-let* ((item (jabber-ahc--command-at slot)))
    (jabber-ahc-execute-command
     (jabber-ahc--resolve-connection jabber-ahc--command-connection)
     (aref item 1) (aref item 2))))

(defun jabber-ahc-execute-command-1 ()
  "Execute the first command on the current page."
  (interactive)
  (jabber-ahc--execute-slot 0))

(defun jabber-ahc-execute-command-2 ()
  "Execute the second command on the current page."
  (interactive)
  (jabber-ahc--execute-slot 1))

(defun jabber-ahc-execute-command-3 ()
  "Execute the third command on the current page."
  (interactive)
  (jabber-ahc--execute-slot 2))

(defun jabber-ahc-execute-command-4 ()
  "Execute the fourth command on the current page."
  (interactive)
  (jabber-ahc--execute-slot 3))

(defun jabber-ahc-command-previous-page ()
  "Show the previous discovered-command page."
  (interactive)
  (setq-local jabber-ahc--command-page
              (max 0 (1- jabber-ahc--command-page))))

(defun jabber-ahc-command-next-page ()
  "Show the next discovered-command page."
  (interactive)
  (setq-local jabber-ahc--command-page
              (min (1- (jabber-ahc--command-page-count))
                   (1+ jabber-ahc--command-page))))

(defun jabber-ahc--command-page-count ()
  "Return the number of discovered-command pages."
  (ceiling (length jabber-ahc--command-items)
           jabber-ahc--command-page-size))

(defun jabber-ahc--command-has-next-page-p ()
  "Return non-nil when another discovered-command page exists."
  (< (* (1+ jabber-ahc--command-page) jabber-ahc--command-page-size)
     (length jabber-ahc--command-items)))

(keymap-popup-define jabber-ahc-command-list-map
  "Discovered XEP-0050 commands."
  :exit-key "C-g"
  :description
  (lambda () (format "Commands for %s" jabber-ahc--command-target))
  :group "Commands"
  "1" ((lambda () (jabber-ahc--command-description 0))
       jabber-ahc-execute-command-1
       :if (lambda () (jabber-ahc--command-at 0)))
  "2" ((lambda () (jabber-ahc--command-description 1))
       jabber-ahc-execute-command-2
       :if (lambda () (jabber-ahc--command-at 1)))
  "3" ((lambda () (jabber-ahc--command-description 2))
       jabber-ahc-execute-command-3
       :if (lambda () (jabber-ahc--command-at 2)))
  "4" ((lambda () (jabber-ahc--command-description 3))
       jabber-ahc-execute-command-4
       :if (lambda () (jabber-ahc--command-at 3)))
  :group "Navigation"
  "[" ("Previous page" jabber-ahc-command-previous-page
       :stay-open t
       :if (lambda () (> jabber-ahc--command-page 0)))
  "]" ("Next page" jabber-ahc-command-next-page
       :stay-open t
       :if (lambda () (jabber-ahc--command-has-next-page-p))))

(defun jabber-ahc--command-list-result (jc context result)
  "Display XEP-0050 command RESULT for JC using CONTEXT.
CONTEXT contains the originating buffer and queried JID."
  (let ((buffer (car context))
        (to (cadr context)))
    (cond
     ((eq (car-safe result) 'error)
      (message "Command discovery failed: %s" (jabber-parse-error result)))
     ((not (buffer-live-p buffer))
      (message "Command list arrived after its buffer was closed"))
     ((null result)
      (message "No ad-hoc commands found for %s" to))
     (t
      (with-current-buffer buffer
        (pop-to-buffer buffer)
        (keymap-popup (jabber-ahc--command-list-map jc to result)))))))

(defun jabber-ahc-execute-command (jc to node)
  "Execute ad-hoc command NODE on TO.

See XEP-0050.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Execute command of: " nil nil nil nil nil)
		     (jabber-read-node "Node of command: ")))
  (jabber-send-iq jc to
		  "set"
		  `(command ((xmlns . ,jabber-ahc-xmlns)
			     (node . ,node)
			     (action . "execute")))
		  #'jabber-process-data #'jabber-ahc-display
		  #'jabber-process-data "Command execution failed"))

(defun jabber-ahc--xdata (query)
  "Return QUERY's XEP-0004 child, if any."
  (seq-find
   (lambda (x)
     (string= (jabber-xml-get-attribute x 'xmlns) jabber-xdata-xmlns))
   (jabber-xml-get-children query 'x)))

(defun jabber-ahc--action-names (actions)
  "Return the actions permitted by XEP-0050 ACTIONS."
  (if (null actions)
      '("complete" "cancel")
    (append
     (cl-loop for child in (jabber-xml-node-children actions)
              for name = (symbol-name (jabber-xml-node-name child))
              when (member name '("prev" "next" "complete"))
              collect name)
     '("cancel"))))

(defun jabber-ahc--default-action (actions names)
  "Return the default action from ACTIONS, constrained to NAMES."
  (let ((default (if actions
                     (jabber-xml-get-attribute actions 'execute)
                   "complete")))
    (and (member default names) default)))

(defun jabber-ahc--action-key (name default)
  "Return the popup key for action NAME, given DEFAULT."
  (if (equal name default)
      "RET"
    (alist-get name '(("prev" . "p")
                      ("next" . "n")
                      ("complete" . "c")
                      ("cancel" . "q"))
              nil nil #'equal)))

(defun jabber-ahc--action-command (name context)
  "Return a command that submits action NAME using CONTEXT."
  (lambda ()
    (interactive)
    (jabber-ahc-submit (intern name) context)))

(defun jabber-ahc--menu-actions (actions context)
  "Return popup action plists for XEP-0050 ACTIONS using CONTEXT."
  (let* ((names (delete-dups (jabber-ahc--action-names actions)))
         (default (jabber-ahc--default-action actions names)))
    (mapcar
     (lambda (name)
       (list :key (jabber-ahc--action-key name default)
             :label (capitalize name)
             :command (jabber-ahc--action-command name context)
             :submits-form
             (and (member name '("next" "complete")) t)))
     names)))

(defun jabber-ahc--render-notes (notes)
  "Insert XEP-0050 NOTES at point."
  (dolist (note notes)
    (let ((type (jabber-xml-get-attribute note 'type)))
      (when (member type '("warn" "error"))
        (insert (capitalize type) ": "))
      (insert (or (car (jabber-xml-node-children note)) "") "\n")))
  (when notes
    (insert "\n")))

(defun jabber-ahc--open-form (xdata actions context)
  "Open editable XDATA with XEP-0050 ACTIONS using CONTEXT."
  (pop-to-buffer (current-buffer))
  (jabber-xdata-form-open
   (if xdata
       (jabber-xdata-parse xdata)
     '(:title "Ad-hoc command" :fields nil))
   (jabber-ahc--menu-actions actions context)))

(defun jabber-ahc-display (jc xml-data)
  "Render the ad-hoc-command result IQ XML-DATA on connection JC."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (node (jabber-xml-get-attribute query 'node))
	 (notes (jabber-xml-get-children query 'note))
	 (sessionid (jabber-xml-get-attribute query 'sessionid))
	 (status (jabber-xml-get-attribute query 'status))
	 (actions (car (jabber-xml-get-children query 'actions)))
	 (xdata (jabber-ahc--xdata query))
	 (inhibit-read-only t))

    (insert (pcase status
              ("executing" "Executing command\n\n")
              ("completed" "Command completed\n\n")
              ("canceled" "Command canceled\n\n")
              (_ "")))
    (jabber-ahc--render-notes notes)
    (cond
     ((and xdata
           (string= (jabber-xml-get-attribute xdata 'type) "result"))
      (jabber-widget-render-xdata-search-results xdata))
     ((string= status "executing")
      (jabber-ahc--open-form
       xdata actions
       (list :connection jc
             :to from
             :node node
             :sessionid sessionid
             :has-form (not (null xdata))))))))

(defun jabber-ahc--resolve-connection (jc)
  "Return the active connection corresponding to JC."
  (or (and (jabber-connection-active-p jc) jc)
      (when-let* ((replacement
                   (and jc
                        (ignore-errors
                          (jabber-find-active-connection jc))))
                  ((jabber-connection-active-p replacement)))
        replacement)
      (user-error "The Jabber connection is no longer active")))

(defun jabber-ahc-submit (action &optional context)
  "Submit ad-hoc command ACTION using captured CONTEXT."
  (let* ((context
          (or context
              (list :connection jabber-buffer-connection
                    :to jabber-ahc--submit-to
                    :node jabber-ahc-node
                    :sessionid jabber-ahc-sessionid
                    :has-form jabber-ahc--has-form)))
         (jc (jabber-ahc--resolve-connection
              (plist-get context :connection)))
        (submission
         (when (and (plist-get context :has-form)
                    (memq action '(next complete)))
           (jabber-xdata-form-submit-form))))
    (jabber-send-iq jc (plist-get context :to)
		  "set"
		  `(command ((xmlns . ,jabber-ahc-xmlns)
			     (sessionid . ,(plist-get context :sessionid))
			     (node . ,(plist-get context :node))
			     (action . ,(symbol-name action)))
			    ,@(and submission (list submission)))
		  #'jabber-process-data #'jabber-ahc-display
		  #'jabber-process-data "Command execution failed")))

(provide 'jabber-ahc)
;;; jabber-ahc.el ends here.
