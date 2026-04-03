;;; jabber-bookmarks.el --- bookmarks according to XEP-0048  -*- lexical-binding: t; -*-

;; Copyright (C) 2007, 2008 - Magnus Henoch - mange@freemail.hu
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

;;; Code:

(require 'cl-lib)

(require 'jabber-private)
(require 'jabber-pubsub)
(require 'transient)

(defconst jabber-bookmarks-xmlns "storage:bookmarks"
  "XEP-0048 bookmarks namespace.")

(defconst jabber-bookmarks2-xmlns "urn:xmpp:bookmarks:1"
  "XEP-0402 Bookmarks 2 namespace.")

(defconst jabber-bookmarks2-compat-xmlns "urn:xmpp:bookmarks:1#compat"
  "XEP-0402 compat feature advertised in server disco.")

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

(defvar jabber-pre-disconnect-hook)       ; jabber-core.el

(declare-function jabber-disco-get-info "jabber-disco.el"
                  (jc jid node callback closure-data &optional force))
(declare-function jabber-muc-joined-p "jabber-muc" (group &optional jc))
(declare-function jabber-muc-nickname "jabber-muc" (group &optional jc))
(declare-function jabber-muc-join "jabber-muc" (jc group nickname &optional popup))
(declare-function jabber-muc-leave "jabber-muc" (jc group))
(declare-function jabber-muc-get-buffer "jabber-muc" (group &optional jc))
(declare-function fsm-get-state-data "fsm" (fsm))

;; Disco feature: request PubSub notifications for bookmarks
(jabber-disco-advertise-feature (concat jabber-bookmarks2-xmlns "+notify"))

(defcustom jabber-bookmarks-auto-add t
  "Whether to automatically bookmark rooms on join.
When non-nil, joining a room automatically adds it to bookmarks
with autojoin enabled, matching the behavior of Dino and
Conversations."
  :group 'jabber-chat
  :type 'boolean)

(defvar jabber-bookmarks (make-hash-table :test 'equal)
  "Mapping from bare JIDs to bookmark lists.
Values are a list of bookmark plists, or t if no bookmarks were found.
nil means bookmarks have not been retrieved yet.")

(defvar jabber-bookmarks--legacy-accounts (make-hash-table :test 'equal)
  "Set of bare JIDs whose servers only support XEP-0049 legacy bookmarks.
Non-nil value means the account fell back to legacy on last fetch.")

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
    (unless (jabber-muc-joined-p jid jc)
      (jabber-muc-join jc jid
                       (or (plist-get bookmark :nick)
                           (plist-get (fsm-get-state-data jc) :username))))))

(defun jabber-bookmarks2--maybe-leave (jc jid)
  "Leave room JID if currently joined, with a status message."
  (when (jabber-muc-joined-p jid jc)
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
child elements from the event.
Legacy accounts ignore these events."
  (unless (jabber-bookmarks--legacy-p jc)
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
           (jabber-bookmarks2--maybe-leave jc jid)))))))

(with-eval-after-load "jabber-pubsub"
  (setf (alist-get jabber-bookmarks2-xmlns jabber-pubsub-node-handlers
                   nil nil #'equal)
        #'jabber-bookmarks2--handle-event))

;;; Fetch bookmarks

;;;###autoload
(defun jabber-get-bookmarks (jc cont &optional refresh)
  "Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and a list of bookmark plists.
CONT is called asynchronously.
If REFRESH is non-nil, always fetch from server and re-detect protocol."
  (let ((bookmarks (gethash (jabber-connection-bare-jid jc) jabber-bookmarks)))
    (if (and (not refresh) bookmarks)
        (run-with-timer 0 nil cont jc (when (listp bookmarks) bookmarks))
      (jabber-bookmarks--detect-and-fetch jc cont refresh))))

(defun jabber-bookmarks--detect-and-fetch (jc cont &optional refresh)
  "Detect bookmark protocol via disco and fetch.
Query bare JID for the `#compat' feature.  If present, use XEP-0402
PubSub; otherwise fall back to XEP-0049 Private XML Storage.
When REFRESH is non-nil, re-run disco detection."
  (if (and (not refresh) (jabber-bookmarks--legacy-p jc))
      (jabber-bookmarks--get-legacy jc cont)
    (jabber-disco-get-info
     jc (jabber-connection-bare-jid jc) nil
     (lambda (jc _closure result)
       (if (and (listp result)
                (not (eq (car result) 'error))
                (member jabber-bookmarks2-compat-xmlns (cadr result)))
           (jabber-bookmarks2--fetch jc cont)
         (jabber-bookmarks--get-legacy jc cont)))
     nil)))

(defun jabber-bookmarks2--fetch (jc cont)
  "Fetch bookmarks via XEP-0402 PubSub.
CONT is called with JC and the bookmark plist list.
Falls back to legacy on PubSub error."
  (jabber-pubsub-request
   jc nil jabber-bookmarks2-xmlns
   (lambda (jc xml-data _closure)
     (jabber-bookmarks2--handle-fetch jc xml-data cont))
   (lambda (jc _xml-data _closure)
     (message "jabber-bookmarks: PubSub fetch error, falling back to legacy")
     (jabber-bookmarks--get-legacy jc cont))))

(defun jabber-bookmarks2--handle-fetch (jc xml-data cont)
  "Process a PubSub items response for XEP-0402 bookmarks.
Parses items, updates cache, and calls CONT with the plist list."
  (let* ((pubsub (car (jabber-xml-get-children xml-data 'pubsub)))
         (items-node (car (jabber-xml-get-children pubsub 'items)))
         (items (jabber-xml-get-children items-node 'item))
         (plists (delq nil (mapcar #'jabber-bookmarks2--parse-item items)))
         (value (or plists t)))
    (remhash (jabber-connection-bare-jid jc) jabber-bookmarks--legacy-accounts)
    (puthash (jabber-connection-bare-jid jc) value jabber-bookmarks)
    (funcall cont jc (when (listp value) value))))

(defun jabber-bookmarks--get-legacy (jc cont)
  "Fetch bookmarks via XEP-0049 Private XML Storage (legacy fallback).
Parses conference elements to plists and calls CONT."
  (jabber-private-get jc 'storage jabber-bookmarks-xmlns
                      (lambda (jc result)
                        (jabber-bookmarks--handle-legacy jc result cont))
                      (lambda (jc _result)
                        (message "jabber-bookmarks: legacy fetch failed for %s"
                                 (jabber-connection-bare-jid jc))
                        (funcall cont jc nil))))

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
    (puthash (jabber-connection-bare-jid jc) t jabber-bookmarks--legacy-accounts)
    (puthash (jabber-connection-bare-jid jc) value jabber-bookmarks)
    (funcall cont jc (when (listp value) value))))

;;;###autoload
(defun jabber-get-bookmarks-from-cache (jc)
  "Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil."
  (gethash (jabber-connection-bare-jid jc) jabber-bookmarks))

(defun jabber-bookmarks--legacy-p (jc)
  "Return non-nil if JC uses legacy XEP-0049 bookmarks."
  (gethash (jabber-connection-bare-jid jc) jabber-bookmarks--legacy-accounts))

(defun jabber-bookmarks--cache-snapshot (jc)
  "Return the current bookmark cache value for JC."
  (gethash (jabber-connection-bare-jid jc) jabber-bookmarks))

(defun jabber-bookmarks--restore-cache (jc snapshot)
  "Restore the bookmark cache for JC to SNAPSHOT."
  (puthash (jabber-connection-bare-jid jc) snapshot jabber-bookmarks))

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

(defun jabber-bookmarks--save-all (jc callback)
  "Write the full bookmark cache for JC via XEP-0049 Private XML Storage.
CALLBACK is called with JC, nil, and success flag (t or nil)."
  (let* ((my-jid (jabber-connection-bare-jid jc))
         (bookmarks (let ((c (gethash my-jid jabber-bookmarks)))
                      (when (listp c) c))))
    (jabber-bookmarks--set-legacy
     jc bookmarks
     (lambda (jc _xml success)
       (funcall callback jc nil success)))))

(defun jabber-set-bookmarks (jc new-bookmarks &optional callback)
  "Set bookmarks to NEW-BOOKMARKS, a list of bookmark plists.
Diffs against cache: publishes added/changed, retracts removed.
For legacy accounts, writes via XEP-0049 Private XML Storage.
For PubSub accounts, publishes per-item and falls back to XEP-0049
on error.
CALLBACK, if non-nil, is called with JC and t or nil on
success or failure."
  (unless callback (setq callback #'ignore))
  (if (jabber-bookmarks--legacy-p jc)
      (jabber-bookmarks--set-legacy jc new-bookmarks callback)
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
          (funcall callback jc t))))))

(defun jabber-bookmarks--set-legacy (jc bookmarks &optional callback)
  "Write BOOKMARKS via XEP-0049 Private XML Storage (legacy fallback).
BOOKMARKS is a list of plists.  Converts to XEP-0048 XML format.
CALLBACK is called with JC, XML-DATA, and t on success or nil on failure."
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
     (lambda (jc xml-data _closure)
       (message "jabber-bookmarks: legacy write failed for %s"
                (jabber-connection-bare-jid jc))
       (funcall callback jc xml-data nil))
     nil)))

;;; Tabulated-list bookmark editor

(defvar jabber-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'jabber-bookmarks-add)
    (define-key map "d" #'jabber-bookmarks-delete)
    (define-key map "t" #'jabber-bookmarks-toggle-autojoin)
    (define-key map "e" #'jabber-bookmarks-edit)
    (define-key map "h" #'jabber-bookmarks-menu)
    (define-key map "?" #'jabber-bookmarks-menu)
    map)
  "Keymap for `jabber-bookmarks-mode'.")

(defun jabber-bookmarks--column-format ()
  "Compute `tabulated-list-format' based on window width."
  (let* ((w (- (window-width) (* 5 2))) ; subtract padding (5 cols * 2)
         (jid-w      (floor (* w 0.35)))
         (name-w     (floor (* w 0.20)))
         (autojoin-w (floor (* w 0.10)))
         (nick-w     (floor (* w 0.20)))
         (password-w (- w jid-w name-w autojoin-w nick-w)))
    (vector (list "JID" jid-w t)
            (list "Name" name-w t)
            (list "Autojoin" autojoin-w t)
            (list "Nick" nick-w t)
            (list "Password" password-w t))))

(define-derived-mode jabber-bookmarks-mode tabulated-list-mode "Bookmarks"
  "Major mode for displaying XMPP bookmarks."
  (setq tabulated-list-format (jabber-bookmarks--column-format))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (setq tabulated-list-entries #'jabber-bookmarks--entries)
  (add-hook 'tabulated-list-revert-hook #'jabber-bookmarks--revert nil t))

(defun jabber-bookmarks--entries ()
  "Build tabulated-list entries from the bookmark cache."
  (let ((cache (jabber-get-bookmarks-from-cache jabber-buffer-connection)))
    (when (listp cache)
      (mapcar (lambda (bm)
                (list (plist-get bm :jid)
                      (vector (or (plist-get bm :jid) "")
                              (or (plist-get bm :name) "")
                              (if (plist-get bm :autojoin) "true" "false")
                              (or (plist-get bm :nick) "")
                              (if (plist-get bm :password) "***" ""))))
              cache))))

(defun jabber-bookmarks--revert ()
  "Re-fetch bookmarks from server before reverting."
  (jabber-get-bookmarks
   jabber-buffer-connection
   (lambda (_jc _bookmarks)
     (when-let* ((buf (get-buffer "*jabber-bookmarks*")))
       (with-current-buffer buf
         (setq tabulated-list-format (jabber-bookmarks--column-format))
         (tabulated-list-init-header)
         (tabulated-list-print t))))
   t))

;;;###autoload
(defun jabber-edit-bookmarks (jc)
  "Display bookmarks in a tabulated list.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-get-bookmarks jc #'jabber-bookmarks--show-editor t))

(defun jabber-bookmarks--show-editor (jc _bookmarks)
  "Populate the bookmark editor buffer.
JC is the Jabber connection."
  (with-current-buffer (get-buffer-create "*jabber-bookmarks*")
    (jabber-bookmarks-mode)
    (setq jabber-buffer-connection jc)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;; Direct commands

(defun jabber-bookmarks--get-bookmark-at-point ()
  "Return the bookmark plist for the entry at point, or nil."
  (when-let* ((jid (tabulated-list-get-id)))
    (let ((cache (jabber-get-bookmarks-from-cache jabber-buffer-connection)))
      (when (listp cache)
        (cl-find jid cache
                 :key (lambda (bm) (plist-get bm :jid))
                 :test #'string=)))))

(defun jabber-bookmarks-add (jid)
  "Add a bookmark for JID with autojoin enabled."
  (interactive "sRoom JID: ")
  (jabber-bookmarks--publish-one jabber-buffer-connection jid))

(defun jabber-bookmarks-auto-add-maybe (jc jid nick)
  "Bookmark JID with NICK if `jabber-bookmarks-auto-add' is enabled.
Does nothing if JID is already bookmarked.  JC is the connection."
  (when jabber-bookmarks-auto-add
    (let ((cache (jabber-get-bookmarks-from-cache jc)))
      (unless (and (listp cache)
                   (cl-find jid cache
                            :key (lambda (bm) (plist-get bm :jid))
                            :test #'string=))
        (jabber-bookmarks--publish-one jc jid nick)))))

(defun jabber-bookmarks--publish-one (jc jid &optional nick)
  "Publish a bookmark for JID via JC.
NICK, if non-nil, is stored in the bookmark."
  (let ((plist (list :jid jid :autojoin t
                     :nick (or nick (jabber-muc-nickname jid jc)))))
    (if (jabber-bookmarks--legacy-p jc)
        (let ((snapshot (jabber-bookmarks--cache-snapshot jc)))
          (jabber-bookmarks2--update-cache jc plist)
          (jabber-bookmarks--save-all
           jc (lambda (jc _xml success)
                (if success
                    (progn
                      (jabber-bookmarks2--maybe-join jc plist)
                      (jabber-bookmarks--refresh-buffer)
                      (message "Bookmark added: %s" jid))
                  (jabber-bookmarks--restore-cache jc snapshot)
                  (jabber-bookmarks--refresh-buffer)
                  (message "Failed to add bookmark: %s" jid)))))
      (jabber-bookmarks2--publish
       jc plist
       (lambda (jc _xml _closure)
         (jabber-bookmarks2--update-cache jc plist)
         (jabber-bookmarks2--maybe-join jc plist)
         (jabber-bookmarks--refresh-buffer)
         (message "Bookmark added: %s" jid))
       (lambda (_jc _xml _closure)
         (message "Failed to add bookmark: %s" jid))))))

(defun jabber-bookmarks--retract-one (jc jid)
  "Remove bookmark for JID via JC."
  (if (jabber-bookmarks--legacy-p jc)
      (let ((snapshot (jabber-bookmarks--cache-snapshot jc)))
        (jabber-bookmarks2--remove-from-cache jc jid)
        (jabber-bookmarks--save-all
         jc (lambda (_jc _xml success)
              (if success
                  (progn
                    (jabber-bookmarks--refresh-buffer)
                    (message "Bookmark removed: %s" jid))
                (jabber-bookmarks--restore-cache jc snapshot)
                (jabber-bookmarks--refresh-buffer)
                (message "Failed to remove bookmark: %s" jid)))))
    (jabber-bookmarks2--retract
     jc jid
     (lambda (_jc _xml _closure)
       (jabber-bookmarks2--remove-from-cache jc jid)
       (jabber-bookmarks--refresh-buffer)
       (message "Bookmark removed: %s" jid))
     (lambda (_jc _xml _closure)
       (message "Failed to remove bookmark: %s" jid)))))

(defun jabber-bookmarks-delete ()
  "Delete the bookmark at point."
  (interactive)
  (let ((jid (tabulated-list-get-id))
        (jc jabber-buffer-connection))
    (unless jid (user-error "No bookmark at point"))
    (when (yes-or-no-p (format "Delete bookmark %s? " jid))
      (jabber-bookmarks--retract-one jc jid))))

(defun jabber-bookmarks-toggle-autojoin ()
  "Toggle autojoin for the bookmark at point."
  (interactive)
  (let ((bm (jabber-bookmarks--get-bookmark-at-point)))
    (unless bm (user-error "No bookmark at point"))
    (let* ((jid (plist-get bm :jid))
           (new-autojoin (not (plist-get bm :autojoin)))
           (new-plist (plist-put (copy-sequence bm) :autojoin new-autojoin))
           (jc jabber-buffer-connection)
           (on-success (lambda (jc)
                         (if new-autojoin
                             (jabber-bookmarks2--maybe-join jc new-plist)
                           (jabber-bookmarks2--maybe-leave jc jid))
                         (jabber-bookmarks--refresh-buffer)
                         (message "%s autojoin %s" jid
                                  (if new-autojoin "on" "off")))))
      (if (jabber-bookmarks--legacy-p jc)
          (let ((snapshot (jabber-bookmarks--cache-snapshot jc)))
            (jabber-bookmarks2--update-cache jc new-plist)
            (jabber-bookmarks--save-all
             jc (lambda (jc _xml success)
                  (if success
                      (funcall on-success jc)
                    (jabber-bookmarks--restore-cache jc snapshot)
                    (jabber-bookmarks--refresh-buffer)
                    (message "Failed to toggle autojoin for %s" jid)))))
        (jabber-bookmarks2--publish
         jc new-plist
         (lambda (jc _xml _closure)
           (jabber-bookmarks2--update-cache jc new-plist)
           (funcall on-success jc))
         (lambda (_jc _xml _closure)
           (message "Failed to toggle autojoin for %s" jid)))))))

(defun jabber-bookmarks--refresh-buffer ()
  "Refresh the bookmarks buffer if it exists."
  (when-let* ((buf (get-buffer "*jabber-bookmarks*")))
    (with-current-buffer buf
      (tabulated-list-print t))))

;;; Transient editor

(defun jabber-bookmarks-set-nick ()
  "Change nick for the bookmark at point."
  (interactive)
  (jabber-bookmarks--set-field :nick "Nick"))

(defun jabber-bookmarks-set-name ()
  "Change name for the bookmark at point."
  (interactive)
  (jabber-bookmarks--set-field :name "Name"))

(defun jabber-bookmarks-set-password ()
  "Change password for the bookmark at point."
  (interactive)
  (jabber-bookmarks--set-field :password "Password"))

(defun jabber-bookmarks--set-field (key prompt)
  "Set field KEY of bookmark at point, prompting with PROMPT."
  (let ((bm (jabber-bookmarks--get-bookmark-at-point)))
    (unless bm (user-error "No bookmark at point"))
    (let* ((old (or (plist-get bm key) ""))
           (new (read-string (format "%s: " prompt) old))
           (new-val (unless (string-empty-p new) new))
           (new-plist (plist-put (copy-sequence bm) key new-val))
           (jc jabber-buffer-connection)
           (jid (plist-get bm :jid))
           (on-success (lambda ()
                         (jabber-bookmarks--refresh-buffer)
                         (message "%s %s set to %s" jid prompt
                                  (or new-val "(empty)")))))
      (if (jabber-bookmarks--legacy-p jc)
          (let ((snapshot (jabber-bookmarks--cache-snapshot jc)))
            (jabber-bookmarks2--update-cache jc new-plist)
            (jabber-bookmarks--save-all
             jc (lambda (_jc _xml success)
                  (if success
                      (funcall on-success)
                    (jabber-bookmarks--restore-cache jc snapshot)
                    (jabber-bookmarks--refresh-buffer)
                    (message "Failed to set %s for %s" prompt jid)))))
        (jabber-bookmarks2--publish
         jc new-plist
         (lambda (_jc _xml _closure)
           (jabber-bookmarks2--update-cache jc new-plist)
           (funcall on-success))
         (lambda (_jc _xml _closure)
           (message "Failed to set %s for %s" prompt jid)))))))

(transient-define-prefix jabber-bookmarks-edit ()
  "Edit bookmark at point."
  [:description
   (lambda () (format "Edit: %s" (or (tabulated-list-get-id) "(none)")))
   [("a" "Toggle autojoin" jabber-bookmarks-toggle-autojoin)
    ("n" "Change nick" jabber-bookmarks-set-nick)
    ("N" "Change name" jabber-bookmarks-set-name)
    ("p" "Change password" jabber-bookmarks-set-password)]])

(transient-define-prefix jabber-bookmarks-menu ()
  "Bookmarks commands."
  [["Bookmark"
    ("a" "Add bookmark" jabber-bookmarks-add)
    ("d" "Delete bookmark" jabber-bookmarks-delete)
    ("t" "Toggle autojoin" jabber-bookmarks-toggle-autojoin)
    ("e" "Edit bookmark" jabber-bookmarks-edit)
    ("g" "Refresh" revert-buffer)]])

;;; Disconnect cleanup

(defun jabber-bookmarks--on-disconnect ()
  "Pre-disconnect hook.  Clear bookmark caches."
  (clrhash jabber-bookmarks)
  (clrhash jabber-bookmarks--legacy-accounts))

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-pre-disconnect-hook #'jabber-bookmarks--on-disconnect))

(provide 'jabber-bookmarks)

;;; jabber-bookmarks.el ends here