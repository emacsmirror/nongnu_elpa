;;; jabber-bookmarks.el --- bookmarks according to XEP-0048  -*- lexical-binding: t; -*-

;; Copyright (C) 2007, 2008 - Magnus Henoch - mange@freemail.hu

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

;;; Code:

(require 'cl-lib)

(require 'jabber-private)
(require 'jabber-widget)
(require 'jabber-pubsub)

(defconst jabber-bookmarks-xmlns "storage:bookmarks"
  "XEP-0048 bookmarks namespace.")

(defconst jabber-bookmarks2-xmlns "urn:xmpp:bookmarks:1"
  "XEP-0402 Bookmarks 2 namespace.")

(defconst jabber-bookmarks2--publish-options
  '(("pubsub#persist_items" . "true")
    ("pubsub#max_items" . "max")
    ("pubsub#send_last_published_item" . "never")
    ("pubsub#access_model" . "whitelist"))
  "Publish-options required by XEP-0402.")

;; Global reference declarations

(defvar jabber-muc-default-nicknames)   ; jabber-muc.el
(defvar jabber-muc-autojoin)            ; jabber-muc.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

(declare-function jabber-muc-joined-p "jabber-muc" (group))
(declare-function jabber-muc-join "jabber-muc" (jc group nickname &optional popup))
(declare-function jabber-muc-leave "jabber-muc" (jc group))
(declare-function jabber-muc-get-buffer "jabber-muc" (group &optional jc))
(declare-function fsm-get-state-data "fsm" (fsm))

;; Disco feature: request PubSub notifications for bookmarks
(jabber-disco-advertise-feature (concat jabber-bookmarks2-xmlns "+notify"))

;;

(defvar jabber-bookmarks (make-hash-table :test 'equal)
  "Mapping from bare JIDs to bookmark lists.
Values are a list of bookmark plists, or t if no bookmarks were found.
nil means bookmarks have not been retrieved yet.")

;;;###autoload
(defun jabber-get-conference-data (jc conference-jid cont &optional key)
  "Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache."
  (if (null cont)
      (let ((cache (jabber-get-bookmarks-from-cache jc)))
       (if (and cache (listp cache))
        (jabber-get-conference-data-internal
         cache conference-jid key)))
    (jabber-get-bookmarks
     jc
     (lambda (jc result)
       (let ((entry (jabber-get-conference-data-internal result conference-jid key)))
	 (funcall cont jc entry))))))

(defun jabber-get-conference-data-internal (result conference-jid key)
  (let ((entry (cl-dolist (plist result)
                 (when (string= (plist-get plist :jid) conference-jid)
                   (cl-return plist)))))
    (if key
	(plist-get entry key)
      entry)))

;;;###autoload
(defun jabber-parse-conference-bookmark (node)
  "Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password."
  (when (eq (jabber-xml-node-name node) 'conference)
    (list :jid (jabber-xml-get-attribute node 'jid)
	  :name (jabber-xml-get-attribute node 'name)
	  :autojoin (member (jabber-xml-get-attribute node 'autojoin)
			    '("true" "1"))
	  :nick (car (jabber-xml-node-children
		      (car (jabber-xml-get-children node 'nick))))
	  :password (car (jabber-xml-node-children
			  (car (jabber-xml-get-children node 'password)))))))

;;; XEP-0402 parse/build helpers

(defun jabber-bookmarks2--parse-item (item)
  "Parse a PubSub ITEM sexp into a bookmark plist.
JID comes from the item id attribute; the rest from the
<conference> child element.
Returns (:jid JID :name NAME :autojoin BOOL :nick NICK :password PASS),
or nil if ITEM has no <conference> child."
  (let* ((jid (jabber-xml-get-attribute item 'id))
         (conf (car (jabber-xml-get-children item 'conference))))
    (when conf
      (list :jid jid
            :name (jabber-xml-get-attribute conf 'name)
            :autojoin (member (jabber-xml-get-attribute conf 'autojoin)
                              '("true" "1"))
            :nick (car (jabber-xml-node-children
                        (car (jabber-xml-get-children conf 'nick))))
            :password (car (jabber-xml-node-children
                            (car (jabber-xml-get-children conf 'password))))))))

(defun jabber-bookmarks2--build-conference (plist)
  "Build an XEP-0402 <conference> element from bookmark PLIST.
PLIST keys: :name, :autojoin, :nick, :password.
The :jid key is not included in the element (it becomes the PubSub item id)."
  `(conference ((xmlns . ,jabber-bookmarks2-xmlns)
                ,@(when (plist-get plist :name)
                    `((name . ,(plist-get plist :name))))
                ,@(when (plist-get plist :autojoin)
                    '((autojoin . "true"))))
               ,@(when (plist-get plist :nick)
                   `((nick () ,(plist-get plist :nick))))
               ,@(when (plist-get plist :password)
                   `((password () ,(plist-get plist :password))))))

;;; XEP-0402 PubSub event handler (live sync)

(defun jabber-bookmarks2--update-cache (jc bookmark)
  "Update the bookmark cache for JC with BOOKMARK plist.
Replaces any existing entry with the same :jid."
  (let* ((my-jid (jabber-connection-bare-jid jc))
         (old (let ((c (gethash my-jid jabber-bookmarks)))
                (when (listp c) c)))
         (jid (plist-get bookmark :jid))
         (new (cons bookmark
                    (cl-remove-if (lambda (bm) (string= (plist-get bm :jid) jid))
                                  old))))
    (puthash my-jid new jabber-bookmarks)))

(defun jabber-bookmarks2--remove-from-cache (jc jid)
  "Remove the bookmark for JID from the cache for JC."
  (let* ((my-jid (jabber-connection-bare-jid jc))
         (old (let ((c (gethash my-jid jabber-bookmarks)))
                (when (listp c) c)))
         (new (cl-remove-if (lambda (bm) (string= (plist-get bm :jid) jid))
                            old)))
    (puthash my-jid (or new t) jabber-bookmarks)))

(defun jabber-bookmarks2--maybe-join (jc bookmark)
  "Join the room in BOOKMARK if autojoin is set and not already joined."
  (let ((jid (plist-get bookmark :jid)))
    (unless (jabber-muc-joined-p jid)
      (jabber-muc-join jc jid
                       (or (plist-get bookmark :nick)
                           (plist-get (fsm-get-state-data jc) :username))))))

(defun jabber-bookmarks2--maybe-leave (jc jid)
  "Leave room JID if currently joined, with a status message."
  (when (jabber-muc-joined-p jid)
    (let ((buf (jabber-muc-get-buffer jid)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (insert (propertize "\n*** Left room (bookmark removed by another client)\n"
                              'face 'shadow)))))
    (jabber-muc-leave jc jid)))

(defun jabber-bookmarks2--handle-event (jc _from _node items)
  "Handle PubSub event notifications for bookmarks.
JC is the connection.  ITEMS is the list of <item> and <retract>
child elements from the event."
  (dolist (child items)
    (pcase (jabber-xml-node-name child)
      ('item
       (let ((bookmark (jabber-bookmarks2--parse-item child)))
         (when bookmark
           (jabber-bookmarks2--update-cache jc bookmark)
           (if (plist-get bookmark :autojoin)
               (jabber-bookmarks2--maybe-join jc bookmark)
             (jabber-bookmarks2--maybe-leave jc (plist-get bookmark :jid))))))
      ('retract
       (let ((jid (jabber-xml-get-attribute child 'id)))
         (jabber-bookmarks2--remove-from-cache jc jid)
         (jabber-bookmarks2--maybe-leave jc jid))))))

(eval-after-load "jabber-pubsub"
  '(push (cons jabber-bookmarks2-xmlns
               #'jabber-bookmarks2--handle-event)
         jabber-pubsub-node-handlers))

;;; Fetch bookmarks

;;;###autoload
(defun jabber-get-bookmarks (jc cont &optional refresh)
  "Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and a list of bookmark plists.
CONT is called asynchronously.
If REFRESH is non-nil, always fetch from server."
  (let ((bookmarks (gethash (jabber-connection-bare-jid jc) jabber-bookmarks)))
    (if (and (not refresh) bookmarks)
        (run-with-timer 0 nil cont jc (when (listp bookmarks) bookmarks))
      ;; Try XEP-0402 (PubSub) first, fall back to XEP-0049
      (jabber-pubsub-request
       jc nil jabber-bookmarks2-xmlns
       (lambda (jc xml-data _closure)
         (jabber-bookmarks2--handle-fetch jc xml-data cont))
       (lambda (jc _xml-data _closure)
         (jabber-bookmarks--get-legacy jc cont))))))

(defun jabber-bookmarks2--handle-fetch (jc xml-data cont)
  "Process a PubSub items response for XEP-0402 bookmarks.
Parses items, updates cache, and calls CONT with the plist list."
  (let* ((pubsub (car (jabber-xml-get-children xml-data 'pubsub)))
         (items-node (car (jabber-xml-get-children pubsub 'items)))
         (items (jabber-xml-get-children items-node 'item))
         (plists (delq nil (mapcar #'jabber-bookmarks2--parse-item items)))
         (value (or plists t)))
    (puthash (jabber-connection-bare-jid jc) value jabber-bookmarks)
    (funcall cont jc (when (listp value) value))))

(defun jabber-bookmarks--get-legacy (jc cont)
  "Fetch bookmarks via XEP-0049 Private XML Storage (legacy fallback).
Parses conference elements to plists and calls CONT."
  (let ((callback (lambda (jc result)
                    (jabber-bookmarks--handle-legacy jc result cont))))
    (jabber-private-get jc 'storage jabber-bookmarks-xmlns
                        callback callback)))

(defun jabber-bookmarks--handle-legacy (jc result cont)
  "Process an XEP-0049 storage response.
Parses conference elements to plists, updates cache, calls CONT."
  (let* ((children (when (eq (jabber-xml-node-name result) 'storage)
                     (jabber-xml-node-children result)))
         (plists (delq nil (mapcar (lambda (node)
                                     (when (eq (jabber-xml-node-name node)
                                               'conference)
                                       (jabber-parse-conference-bookmark node)))
                                   children)))
         (value (or plists t)))
    (puthash (jabber-connection-bare-jid jc) value jabber-bookmarks)
    (funcall cont jc (when (listp value) value))))

;;;###autoload
(defun jabber-get-bookmarks-from-cache (jc)
  "Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil."
  (gethash (jabber-connection-bare-jid jc) jabber-bookmarks))

(defun jabber-bookmarks2--publish (jc plist &optional callback error-callback)
  "Publish a single bookmark PLIST to PubSub via JC.
CALLBACK and ERROR-CALLBACK follow `jabber-send-iq' conventions."
  (jabber-pubsub-publish jc nil jabber-bookmarks2-xmlns
                         (plist-get plist :jid)
                         (jabber-bookmarks2--build-conference plist)
                         jabber-bookmarks2--publish-options
                         callback error-callback))

(defun jabber-bookmarks2--retract (jc room-jid &optional callback error-callback)
  "Remove bookmark for ROOM-JID from PubSub via JC.
CALLBACK and ERROR-CALLBACK follow `jabber-send-iq' conventions."
  (jabber-pubsub-retract jc nil jabber-bookmarks2-xmlns room-jid
                         t callback error-callback))

(defun jabber-set-bookmarks (jc new-bookmarks &optional callback)
  "Set bookmarks to NEW-BOOKMARKS, a list of bookmark plists.
Diffs against cache: publishes added/changed, retracts removed.
Falls back to XEP-0049 Private XML on PubSub error.
CALLBACK, if non-nil, is called with JC and t or nil on
success or failure."
  (unless callback (setq callback #'ignore))
  (let* ((my-jid (jabber-connection-bare-jid jc))
         (old (let ((c (gethash my-jid jabber-bookmarks)))
                (when (listp c) c)))
         (old-jids (mapcar (lambda (bm) (plist-get bm :jid)) old))
         (new-jids (mapcar (lambda (bm) (plist-get bm :jid)) new-bookmarks))
         (to-retract (cl-set-difference old-jids new-jids :test #'string=))
         (pending 0)
         (failed nil))
    ;; Track completions
    (cl-flet ((done (_jc _xml _closure)
                (cl-decf pending)
                (when (zerop pending)
                  (unless failed
                    (puthash my-jid (or new-bookmarks t) jabber-bookmarks))
                  (funcall callback jc (not failed))))
              (fail (_jc _xml _closure)
                (setq failed t)
                (cl-decf pending)
                (when (zerop pending)
                  ;; Fall back to XEP-0049 bulk write
                  (jabber-bookmarks--set-legacy jc new-bookmarks callback))))
      ;; Publish each bookmark
      (dolist (bm new-bookmarks)
        (cl-incf pending)
        (jabber-bookmarks2--publish jc bm #'done #'fail))
      ;; Retract removed
      (dolist (jid to-retract)
        (cl-incf pending)
        (jabber-bookmarks2--retract jc jid #'done #'fail))
      ;; If nothing to do, succeed immediately
      (when (zerop pending)
        (puthash my-jid (or new-bookmarks t) jabber-bookmarks)
        (funcall callback jc t)))))

(defun jabber-bookmarks--set-legacy (jc bookmarks &optional callback)
  "Write BOOKMARKS via XEP-0049 Private XML Storage (legacy fallback).
BOOKMARKS is a list of plists.  Converts to XEP-0048 XML format."
  (unless callback (setq callback #'ignore))
  (let ((xml-elems
         (mapcar (lambda (bm)
                   `(conference ((jid . ,(plist-get bm :jid))
                                 ,@(when (plist-get bm :name)
                                     `((name . ,(plist-get bm :name))))
                                 (autojoin . ,(if (plist-get bm :autojoin)
                                                  "1" "0")))
                                ,@(when (plist-get bm :nick)
                                    `((nick () ,(plist-get bm :nick))))
                                ,@(when (plist-get bm :password)
                                    `((password () ,(plist-get bm :password))))))
                 bookmarks)))
    (jabber-private-set
     jc
     `(storage ((xmlns . ,jabber-bookmarks-xmlns)) ,@xml-elems)
     callback t
     callback nil)))

;;;###autoload
(defun jabber-edit-bookmarks (jc)
  "Create a buffer for editing bookmarks interactively.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-get-bookmarks jc #'jabber-bookmarks--show-editor t))

(defun jabber-bookmarks--plist-to-widget (plist)
  "Convert a bookmark PLIST to the widget value format.
Returns (conference JID NAME AUTOJOIN NICK PASSWORD)."
  (list 'conference
        (or (plist-get plist :jid) "")
        (or (plist-get plist :name) "")
        (not (not (plist-get plist :autojoin)))
        (or (plist-get plist :nick) "")
        (or (plist-get plist :password) "")))

(defconst jabber-bookmarks--widget-spec
  '(repeat
    :tag "Bookmarks"
    (list :tag "Conference"
          (const :format "" conference)
          (string :tag "JID")
          (string :tag "Name")
          (checkbox :tag "Autojoin" :format "%[%v%] Autojoin?\n")
          (string :tag "Nick")
          (string :tag "Password")))
  "Widget type spec for the bookmark editor.")

(defun jabber-bookmarks--maybe-insert-import-notice ()
  "Insert variable import notice if local MUC variables are set."
  (when (or (bound-and-true-p jabber-muc-autojoin)
            (bound-and-true-p jabber-muc-default-nicknames))
    (widget-insert
     "The variables `jabber-muc-autojoin' and/or `jabber-muc-default-nicknames'\n"
     "contain values.  They are only available to jabber.el on this machine.\n"
     "You may want to import them into your bookmarks, to make them available\n"
     "to any client on any machine.\n")
    (widget-create 'push-button :notify 'jabber-bookmarks-import
                   "Import values from variables")
    (widget-insert "\n\n")))

(defun jabber-bookmarks--show-editor (jc bookmarks)
  "Populate the bookmark editor buffer with BOOKMARKS.
JC is the Jabber connection.  BOOKMARKS is a list of plists."
  (let ((values (mapcar #'jabber-bookmarks--plist-to-widget bookmarks)))
    (with-current-buffer (get-buffer-create "Edit bookmarks")
      (jabber-init-widget-buffer nil)
      (setq jabber-buffer-connection jc)
      (widget-insert (propertize (concat "Edit bookmarks for "
                                         (jabber-connection-bare-jid jc))
                                 'face 'jabber-title-large)
                     "\n\n")
      (jabber-bookmarks--maybe-insert-import-notice)
      (push (cons 'bookmarks
                  (widget-create jabber-bookmarks--widget-spec :value values))
            jabber-widget-alist)
      (widget-insert "\n")
      (widget-create 'push-button :notify 'jabber-bookmarks-submit "Submit")
      (widget-setup)
      (widget-minor-mode 1)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

(defun jabber-bookmarks--widget-to-plist (entry)
  "Convert a widget ENTRY to a bookmark plist.
ENTRY is (conference JID NAME AUTOJOIN NICK PASSWORD)."
  (pcase-let ((`(,_type ,jid ,name ,autojoin ,nick ,password) entry))
    (list :jid jid
          :name (unless (zerop (length name)) name)
          :autojoin autojoin
          :nick (unless (zerop (length nick)) nick)
          :password (unless (zerop (length password)) password))))

(defun jabber-bookmarks-submit (&rest _ignore)
  "Save the bookmark editor contents to the server."
  (let* ((raw (widget-value (cdr (assq 'bookmarks jabber-widget-alist))))
         (plists (delq nil
                       (mapcar (lambda (entry)
                                 (when (eq (car entry) 'conference)
                                   (jabber-bookmarks--widget-to-plist entry)))
                               raw))))
    (jabber-set-bookmarks
     jabber-buffer-connection plists
     (lambda (_jc ok)
       (message "Storing bookmarks...%s" (if ok "done" "failed"))))))

(defun jabber-bookmarks-import (&rest _ignore)
  "Import `jabber-muc-autojoin' and `jabber-muc-default-nicknames' into the editor."
  (let* ((value (widget-value (cdr (assq 'bookmarks jabber-widget-alist))))
	 (conferences (mapcar
		       #'cdr
		       (cl-remove-if-not
			(lambda (entry)
			  (eq (car entry) 'conference))
			value))))
    (dolist (default-nickname jabber-muc-default-nicknames)
      (pcase-let* ((`(,muc-jid . ,nick) default-nickname)
	           (entry (assoc muc-jid conferences)))
	(if entry
	    (setf (nth 3 entry) nick)
	  (setq entry (list muc-jid "" nil nick ""))
	  (push entry conferences)
	  (push (cons 'conference entry) value))))
    (dolist (autojoin jabber-muc-autojoin)
      (let ((entry (assoc autojoin conferences)))
	(if entry
	    (setf (nth 2 entry) t)
	  (setq entry (list autojoin "" t "" ""))
	  (push (cons 'conference entry) value))))
    (widget-value-set (cdr (assq 'bookmarks jabber-widget-alist)) value)
    (widget-setup)))

(provide 'jabber-bookmarks)

;;; jabber-bookmarks.el ends here