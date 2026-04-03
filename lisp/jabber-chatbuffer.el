;;; jabber-chatbuffer.el --- functions common to all chat buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2005, 2007, 2008 - Magnus Henoch - mange@freemail.hu
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

(require 'jabber-util)
(require 'jabber-core)
(require 'jabber-menu)
(require 'transient)

(defvar jabber-point-insert nil
  "Position where the message being composed starts.")

(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")

(defvar jabber-chat-mode-hook nil
  "Hook called at the end of `jabber-chat-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")

(defvar jabber-chat-ewoc nil
  "The ewoc showing the messages of this chat buffer.")

(defvar-local jabber-chat--msg-nodes nil
  "Hash table mapping stanza IDs to ewoc nodes.
Enables O(1) lookup for in-place updates (receipts, corrections).")

(defvar-local jabber-chat-mam-syncing nil
  "Non-nil while this buffer's peer has an active MAM sync.")

(defvar-local jabber-chat--backlog-generation 0
  "Generation counter for chunked backlog inserts.
Incremented before each new insert sequence so stale timers from a
previous sequence detect the mismatch and stop.")

;; Global reference declarations

(declare-function jabber-muc-nick-completion-at-point "jabber-muc-nick-completion.el" ())
(declare-function jabber-httpupload--upload "jabber-httpupload"
                  (jc filepath callback))
(declare-function jabber-omemo--prefetch-sessions "jabber-omemo"
                  (jc jid))
(declare-function jabber-omemo--prefetch-muc-sessions "jabber-omemo"
                  (jc group))
(declare-function jabber-omemo--muc-participant-jids "jabber-omemo"
                  (group participants))
(declare-function jabber-omemo-fingerprints "jabber-omemo" ())
(declare-function jabber-connection-bare-jid "jabber-util" (jc))
(declare-function jabber-blocking-toggle-chat-peer "jabber-blocking" (jc))
(declare-function jabber-get-info "jabber-info" (jc to))
(declare-function jabber-roster-change "jabber-presence" (jc jid name groups))
(declare-function jabber-roster-delete "jabber-presence" (jc jid))
(declare-function jabber-mam-sync-buffer "jabber-mam" (count))
(declare-function jabber-moderation-retract "jabber-moderation" ())
(declare-function jabber-moderation-retract-by-occupant "jabber-moderation" ())
(declare-function jabber-chat-reply "jabber-message-reply" ())
(declare-function jabber-chat-cancel-reply "jabber-message-reply" ())
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-jid-resource "jabber-util" (jid))
(declare-function jabber-db-set-chat-encryption "jabber-db"
                  (account peer encryption))
(declare-function jabber-db-get-chat-encryption "jabber-db"
                  (account peer))

;;

(defvar-local jabber-buffer-connection nil
  "The connection used by this buffer.")

(defvar jabber-chatting-with)              ; jabber-chat.el
(defvar jabber-chat-header-line-format)   ; jabber-chat.el
(defvar jabber-chat-earliest-backlog)     ; jabber-chat.el
(defvar jabber-group)                      ; jabber-muc.el
(defvar jabber-muc-header-line-format)    ; jabber-muc.el
(defvar jabber-muc-participants)          ; jabber-muc.el
(defvar jabber-httpupload--pending-url)    ; jabber-httpupload.el

;;; Buffer lookup registry

(defvar jabber-chatbuffer--registry (make-hash-table :test #'equal)
  "Hash table mapping (TYPE . KEY) to a live buffer.
TYPE is `chat', `muc', or `muc-private'.
KEY: bare JID for chat; group JID for muc; \"group/nick\" for muc-private.")

(defun jabber-chatbuffer--registry-put (type key)
  "Register current buffer under TYPE and KEY."
  (puthash (cons type key) (current-buffer) jabber-chatbuffer--registry))

(defun jabber-chatbuffer--registry-get (type key)
  "Return the live buffer registered under TYPE and KEY, or nil."
  (let* ((k (cons type key))
         (buf (gethash k jabber-chatbuffer--registry)))
    (if (buffer-live-p buf)
        buf
      (remhash k jabber-chatbuffer--registry)
      nil)))

(defun jabber-chatbuffer--registry-remove ()
  "Remove current buffer from registry.  Used as `kill-buffer-hook'."
  (cond
   ((and (local-variable-p 'jabber-group) jabber-group)
    (remhash (cons 'muc jabber-group) jabber-chatbuffer--registry))
   ((and (local-variable-p 'jabber-chatting-with) jabber-chatting-with)
    ;; MUC-private: jabber-chatting-with is "group/nick" (has resource).
    ;; 1:1 chat: jabber-chatting-with may be full JID or bare — always
    ;; normalise to bare JID to match the key stored at registration time.
    (if (jabber-jid-resource jabber-chatting-with)
        (remhash (cons 'muc-private jabber-chatting-with)
                 jabber-chatbuffer--registry)
      (remhash (cons 'chat (jabber-jid-user jabber-chatting-with))
               jabber-chatbuffer--registry)))))

(add-hook 'kill-buffer-hook #'jabber-chatbuffer--registry-remove)

(defun jabber-chat-attach-file (filepath)
  "Upload FILEPATH and insert the URL into the composition area.
The file is uploaded via HTTP Upload.  Once the upload finishes,
the GET URL is inserted at point so you can preview and edit
before sending with RET."
  (interactive "fFile to upload: ")
  (require 'jabber-httpupload)
  (unless jabber-buffer-connection
    (error "No active connection in this buffer"))
  (let ((buffer (current-buffer)))
    (jabber-httpupload--upload
     jabber-buffer-connection filepath
     (lambda (get-url)
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert get-url)
         (setq jabber-httpupload--pending-url get-url)
         (message "Uploaded: %s (send with RET)" get-url))))))

(defcustom jabber-chat-default-encryption 'omemo
  "Default encryption mode for new chat buffers."
  :type '(choice (const :tag "OMEMO" omemo)
                 (const :tag "OpenPGP" openpgp)
                 (const :tag "PGP (legacy)" openpgp-legacy)
                 (const :tag "Plaintext" plaintext))
  :group 'jabber-chat)

(defvar-local jabber-chat-encryption nil
  "Encryption mode for this chat buffer.
Possible values: `plaintext', `omemo', `openpgp', `openpgp-legacy'.
Set from `jabber-chat-default-encryption' on buffer creation.")

(defvar-local jabber-chat-encryption-message ""
  "Header-line string showing current encryption state.")

(defface jabber-chat-encryption-omemo
  '((t :inherit success))
  "Face for OMEMO encryption indicator in chat header."
  :group 'jabber-chat)

(defface jabber-chat-encryption-openpgp
  '((t :inherit success))
  "Face for OpenPGP encryption indicator in chat header."
  :group 'jabber-chat)

(defface jabber-chat-encryption-openpgp-legacy
  '((t :inherit success))
  "Face for legacy PGP encryption indicator in chat header."
  :group 'jabber-chat)

(defface jabber-chat-encryption-plaintext
  '((t :inherit error))
  "Face for plaintext indicator in chat header."
  :group 'jabber-chat)

(defun jabber-chat-encryption--update-header ()
  "Update `jabber-chat-encryption-message' from current state."
  (setq jabber-chat-encryption-message
        (propertize
         (pcase jabber-chat-encryption
           ('omemo "[OMEMO]")
           ('openpgp "[OpenPGP]")
           ('openpgp-legacy "[PGP]")
           (_ "[plaintext]"))
         'face (pcase jabber-chat-encryption
                 ('omemo 'jabber-chat-encryption-omemo)
                 ('openpgp 'jabber-chat-encryption-openpgp)
                 ('openpgp-legacy 'jabber-chat-encryption-openpgp-legacy)
                 (_ 'jabber-chat-encryption-plaintext)))))

(defun jabber-chat--peer-jid ()
  "Return the bare JID of the chat peer in this buffer.
Works for both 1:1 chat (`jabber-chatting-with') and MUC (`jabber-group')."
  (cond
   ((bound-and-true-p jabber-chatting-with)
    (jabber-jid-user jabber-chatting-with))
   ((bound-and-true-p jabber-group)
    jabber-group)))

(defun jabber-chat-encryption--save (mode)
  "Persist encryption MODE for the current chat buffer."
  (when-let* ((jc jabber-buffer-connection)
              (peer (jabber-chat--peer-jid)))
    (jabber-db-set-chat-encryption
     (jabber-connection-bare-jid jc) peer mode)))

(defun jabber-chat-encryption-set-omemo ()
  "Set encryption to OMEMO for this chat buffer."
  (interactive)
  (require 'jabber-omemo)
  (unless (bound-and-true-p jabber-omemo--available)
    (user-error "OMEMO encryption requires the jabber-omemo-core native module"))
  (setq jabber-chat-encryption 'omemo)
  (jabber-chat-encryption--save 'omemo)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update)
  (when jabber-buffer-connection
    (cond
     ((bound-and-true-p jabber-chatting-with)
      (jabber-omemo--prefetch-sessions
       jabber-buffer-connection jabber-chatting-with))
     ((bound-and-true-p jabber-group)
      (jabber-omemo--prefetch-muc-sessions
       jabber-buffer-connection jabber-group))))
  (when (and (bound-and-true-p jabber-group)
             (null (jabber-omemo--muc-participant-jids
                    jabber-group
                    (cdr (assoc jabber-group jabber-muc-participants)))))
    (message "OMEMO: no participant JIDs visible — room may be anonymous")))

(defun jabber-chat-encryption-set-openpgp ()
  "Set encryption to OpenPGP for this chat buffer."
  (interactive)
  (require 'jabber-openpgp)
  (setq jabber-chat-encryption 'openpgp)
  (jabber-chat-encryption--save 'openpgp)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update))

(defun jabber-chat-encryption-set-openpgp-legacy ()
  "Set encryption to legacy PGP (XEP-0027) for this chat buffer."
  (interactive)
  (require 'jabber-openpgp-legacy)
  (setq jabber-chat-encryption 'openpgp-legacy)
  (jabber-chat-encryption--save 'openpgp-legacy)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update))

(defun jabber-chat-encryption-set-plaintext ()
  "Set encryption to plaintext for this chat buffer."
  (interactive)
  (setq jabber-chat-encryption 'plaintext)
  (jabber-chat-encryption--save 'plaintext)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update))

(transient-define-prefix jabber-chat-encryption-menu ()
  "Select encryption for this chat buffer."
  [:description
   (lambda () (format "Encryption (current: %s)" jabber-chat-encryption))
   ("o" "OMEMO" jabber-chat-encryption-set-omemo)
   ("g" "OpenPGP" jabber-chat-encryption-set-openpgp)
   ("l" "PGP (legacy)" jabber-chat-encryption-set-openpgp-legacy)
   ("p" "Plaintext" jabber-chat-encryption-set-plaintext)])

(defun jabber-chat-show-fingerprints ()
  "Display OMEMO fingerprints for the current chat peer."
  (interactive)
  (require 'jabber-omemo)
  (jabber-omemo-fingerprints))

(defvar jabber-backlog-number)            ; jabber-db.el

(transient-define-argument jabber-chat:-n ()
  :description "Message count"
  :class 'transient-option
  :shortarg "-n"
  :argument "-n"
  :reader #'transient-read-number-N+
  :always-read t)

(defun jabber-chat--transient-msg-count ()
  "Extract the message count from the current transient's -n argument.
Return a positive integer, or nil if -n is unset or empty."
  (let ((val (cl-some (lambda (a)
                        (and (string-prefix-p "-n" a)
                             (substring a 2)))
                      (transient-args transient-current-command))))
    (and val (not (string-empty-p val))
         (let ((num (string-to-number val)))
           (and (> num 0) num)))))

(transient-define-suffix jabber-chat--refresh-suffix ()
  "Refresh buffer from DB using the -n count from the transient."
  :transient nil
  (interactive)
  (jabber-chat-buffer-refresh (jabber-chat--transient-msg-count)))

(transient-define-suffix jabber-chat--mam-sync-buffer-suffix ()
  "Sync & redraw using the -n count from the transient."
  :transient nil
  (interactive)
  (jabber-mam-sync-buffer
   (or (jabber-chat--transient-msg-count) jabber-backlog-number)))

(defun jabber-chat-get-info ()
  "Show version, disco info and ping for the current chat peer."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (jabber-get-info jabber-buffer-connection jabber-chatting-with))

(defun jabber-chat-add-contact ()
  "Add the current chat peer to the roster."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (let* ((jid (jabber-jid-user jabber-chatting-with))
         (sym (jabber-jid-symbol jid)))
    (jabber-roster-change
     jabber-buffer-connection sym
     (read-string (format "Name for %s: " jid))
     nil)))

(defun jabber-chat-remove-contact ()
  "Remove the current chat peer from the roster."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (let ((jid (jabber-jid-user jabber-chatting-with)))
    (when (yes-or-no-p (format "Remove %s from roster? " jid))
      (jabber-roster-delete jabber-buffer-connection jid))))

(transient-define-prefix jabber-chat-operations-menu ()
  "Chat buffer operations."
  :value (lambda () (list (format "-n%d" jabber-backlog-number)))
  [["Encryption"
    ("e" "Encryption..." jabber-chat-encryption-menu)
    ("f" "Fingerprints" jabber-chat-show-fingerprints)]
   ["Files"
    ("a" "Attach file" jabber-chat-attach-file)]
   ["Contact"
    ("I" "Get info" jabber-chat-get-info)
    ("A" "Add contact" jabber-chat-add-contact)
    ("D" "Remove contact" jabber-chat-remove-contact)
    ("B" "Block/unblock user" jabber-blocking-toggle-chat-peer)]
   ["Messages"
    ("E" "Edit last message" jabber-correct-last-message)
    ("r" "Reply to message" jabber-chat-reply)]
   ["MUC"
    ("m" "MUC operations..." jabber-muc-menu)
    ("M" "Retract message at point" jabber-moderation-retract)
    ("X" "Retract all by occupant" jabber-moderation-retract-by-occupant)]
   ["Buffer"
    ("-n" jabber-chat:-n)
    ("R" "Refresh" jabber-chat--refresh-suffix)
    ("S" "Sync & refresh" jabber-chat--mam-sync-buffer-suffix)]])

;; Spell check only what you're currently writing.
(defun jabber-chat-mode-flyspell-verify ()
  "Return non-nil if point is in the composition area."
  (>= (point) jabber-point-insert))

(defun jabber-chat-newline ()
  "Insert a newline in the composition area without sending."
  (interactive)
  (insert "\n"))

(defvar-keymap jabber-chat-mode-map
  :parent jabber-common-keymap
  "RET"     #'jabber-chat-buffer-send
  "S-<return>"   #'jabber-chat-newline
  "TAB"     #'completion-at-point
  "C-c C-a" #'jabber-chat-attach-file
  "C-c C-o" #'jabber-chat-operations-menu
  "C-c C-e" #'jabber-chat-encryption-menu
  "C-c C-r" #'jabber-chat-reply
  "C-c C-k" #'jabber-chat-cancel-reply)

(define-derived-mode jabber-chat-mode fundamental-mode "jabber-chat"
  "Major mode for Jabber chat buffers.
\\{jabber-chat-mode-map}"
  (visual-line-mode 1)
  (setq-local word-wrap t)
  (display-line-numbers-mode 0)
  (put 'jabber-chat-mode 'flyspell-mode-predicate #'jabber-chat-mode-flyspell-verify))

(defun jabber-chat-mode-setup (jc ewoc-pp)
  "Initialize chat buffer state for connection JC.
EWOC-PP is the pretty-printer function for the message EWOC."
  (add-hook 'completion-at-point-functions #'jabber-muc-nick-completion-at-point nil t)

  (setq-local jabber-send-function nil)
  (setq-local scroll-conservatively 5)
  ;; jabber-chat-ewoc and jabber-point-insert are conditionally set in
  ;; the `unless' block below; make-local-variable is idempotent and
  ;; preserves the existing value on repeated calls.
  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-chat-ewoc)
  (setq jabber-buffer-connection jc)

  (unless jabber-chat-ewoc
    (let ((buffer-undo-list t))
      (setq jabber-chat-ewoc
            (ewoc-create ewoc-pp nil (concat (jabber-separator) "\n") 'nosep))
      (setq jabber-chat--msg-nodes (make-hash-table :test 'equal))
      (goto-char (point-max))
      (put-text-property (point-min) (point) 'read-only t)
      (let ((inhibit-read-only t))
        (put-text-property (point-min) (point) 'front-sticky t)
        (put-text-property (point-min) (point) 'rear-nonsticky t))
      (setq jabber-point-insert (point-marker))))
  (unless jabber-chat-encryption
    (let ((saved (when-let* ((peer (jabber-chat--peer-jid)))
                   (jabber-db-get-chat-encryption
                    (jabber-connection-bare-jid jabber-buffer-connection)
                    peer))))
      (setq jabber-chat-encryption
            (or saved jabber-chat-default-encryption))
      ;; MUC buffers default to plaintext until the user explicitly
      ;; enables OMEMO, unless they previously saved a preference.
      (when (bound-and-true-p jabber-group)
        (unless saved
          (setq jabber-chat-encryption 'plaintext))))
    (when (eq jabber-chat-encryption 'omemo)
      (require 'jabber-omemo nil t)
      (unless (bound-and-true-p jabber-omemo--available)
        (setq jabber-chat-encryption 'plaintext))))
  (jabber-chat-encryption--update-header))

(declare-function jabber-chat-create-buffer "jabber-chat" (jc chat-with))
(declare-function jabber-muc-create-buffer "jabber-muc" (jc group))
(declare-function jabber-muc-sender-p "jabber-muc" (jid))
(declare-function jabber-db-backlog "jabber-db"
                  (account peer &optional count start-time resource msg-type))
(declare-function jabber-chat-insert-backlog-entry "jabber-chat"
                  (msg-plist))
(declare-function jabber-chat--insert-backlog-chunked "jabber-chat"
                  (buffer entries callback &optional generation))
(declare-function jabber-chat-display-buffer-images "jabber-chat" ())

(defun jabber-chat-buffer-redraw-noselect ()
  "Kill the current chat buffer and recreate it from the database.
Returns the new buffer without selecting it."
  (let ((jc jabber-buffer-connection)
	(group (bound-and-true-p jabber-group))
	(chat-with (bound-and-true-p jabber-chatting-with)))
    (kill-buffer (current-buffer))
    (if group
	(jabber-muc-create-buffer jc group)
      (jabber-chat-create-buffer jc chat-with))))

(defun jabber-chat-buffer-redraw ()
  "Kill the current chat buffer and recreate it from the database."
  (interactive)
  (switch-to-buffer (jabber-chat-buffer-redraw-noselect)))

(defun jabber-chat-buffer-refresh (&optional count)
  "Refresh the current chat buffer from the database without killing it.
Clears the ewoc and reloads backlog entries in place.  Cancels any
in-progress chunked insert by bumping the generation counter.
COUNT overrides `jabber-backlog-number' for this refresh."
  (interactive)
  (cl-incf jabber-chat--backlog-generation)
  (let ((generation jabber-chat--backlog-generation)
        (buffer-undo-list t)
        (inhibit-read-only t)
        (node (ewoc-nth jabber-chat-ewoc 0)))
    ;; Delete all ewoc nodes
    (while node
      (let ((next (ewoc-next jabber-chat-ewoc node)))
        (ewoc-delete jabber-chat-ewoc node)
        (setq node next)))
    ;; Clear message ID tracking
    (clrhash jabber-chat--msg-nodes)
    ;; Reload from DB
    (let* ((peer (jabber-chat--peer-jid))
           (account (jabber-connection-bare-jid jabber-buffer-connection))
           (resource (when (and (bound-and-true-p jabber-chatting-with)
                                (not (bound-and-true-p jabber-group))
                                (jabber-muc-sender-p jabber-chatting-with))
                       (jabber-jid-resource jabber-chatting-with)))
           (msg-type (when (and (bound-and-true-p jabber-group)
                                (not resource))
                       "groupchat"))
           (entries (jabber-db-backlog account peer count nil resource
                                      msg-type)))
      (if (null entries)
          (setq jabber-chat-earliest-backlog (float-time))
        (setq jabber-chat-earliest-backlog
              (float-time (plist-get (car (last entries)) :timestamp)))
        (jabber-chat--insert-backlog-chunked
         (current-buffer) entries
         #'jabber-chat-display-buffer-images
         generation)))))

(defun jabber-chat-buffer-send ()
  (interactive)
  ;; If user accidentally hits RET without writing anything, just
  ;; ignore it.
  (when (cl-plusp (- (point-max) jabber-point-insert))
    ;; If connection was lost...
    (unless (memq jabber-buffer-connection jabber-connections)
      ;; ...maybe there is a new connection to the same account.
      (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
	(if new-jc
	    ;; If so, just use it.
	    (setq jabber-buffer-connection new-jc)
	  ;; Otherwise, ask for a new account.
	  (setq jabber-buffer-connection (jabber-read-account t)))))

    (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
      (funcall jabber-send-function jabber-buffer-connection body))))

(defun jabber-chat-buffer-switch ()
  "Switch to a specified jabber chat buffer."
  (interactive)
  (let* ((jabber-buffers (cl-loop for buffer in (buffer-list)
                                  when (with-current-buffer buffer
                                         (eq major-mode 'jabber-chat-mode))
                                  collect (buffer-name buffer)))
         (jabber-buffer (and jabber-buffers
                             (completing-read "Switch to jabber buffer: "
                                              jabber-buffers))))
    (if jabber-buffer
        (switch-to-buffer jabber-buffer)
      (error "No jabber buffer found"))))
(defun jabber-chat-redisplay (&optional all-chats)
  "Regenerate the EWOC text and header for one or more buffers.
With prefix argument, regenerate all `jabber-chat-mode' buffers,
otherwise regenerate the current buffer display.
Scrolls each buffer so the chat log is visible with the prompt
line at the bottom of the window."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (mapc
     (lambda (buffer)
       (with-current-buffer buffer
         (let ((buffer-undo-list t))
           (ewoc-refresh jabber-chat-ewoc))
         (setq header-line-format
               (if (bound-and-true-p jabber-group)
                   jabber-muc-header-line-format
                 jabber-chat-header-line-format))
         (when-let* ((peer (jabber-chat--peer-jid))
                     (saved (jabber-db-get-chat-encryption
                             (jabber-connection-bare-jid
                              jabber-buffer-connection)
                             peer)))
           (setq jabber-chat-encryption saved))
         (jabber-chat-encryption--update-header)
         (force-mode-line-update)
         (when-let* ((win (get-buffer-window buffer)))
           (with-selected-window win
             (goto-char jabber-point-insert)
             (recenter -1)))))
     (seq-filter
      (lambda (buffer)
        (with-current-buffer buffer
          (and (eq major-mode 'jabber-chat-mode)
               (or all-chats
                   (eq buffer current-buffer)))))
      (buffer-list)))))


;;; Ewoc mutation API (undo-suppressed)
;;
;; All ewoc mutations in chat buffers go through these wrappers to
;; keep the undo list clean.  Only the composition area (after
;; `jabber-point-insert') records undo entries.

(defun jabber-chat-ewoc-enter (data)
  "Insert DATA into the chat ewoc and register by stanza ID.
DATA is (TYPE MSG-PLIST).  When the plist has a non-nil :id or
:server-id, the returned ewoc node is stored in
`jabber-chat--msg-nodes' for O(1) lookup.  Returns the ewoc node,
or nil if the message was a duplicate."
  (let* ((msg (cadr data))
         (msg-p (listp msg))
         (id (and msg-p (plist-get msg :id)))
         (sid (and msg-p (plist-get msg :server-id))))
    ;; Skip if this stanza ID is already displayed.
    (unless (or (and id (gethash id jabber-chat--msg-nodes))
                (and sid (gethash sid jabber-chat--msg-nodes)))
      (let ((buffer-undo-list t)
            (node (ewoc-enter-last jabber-chat-ewoc data)))
        (when id (puthash id node jabber-chat--msg-nodes))
        (when sid (puthash sid node jabber-chat--msg-nodes))
        node))))

(defun jabber-chat-ewoc-find-by-id (stanza-id)
  "Return the ewoc node for STANZA-ID, or nil."
  (when (and stanza-id jabber-chat--msg-nodes)
    (gethash stanza-id jabber-chat--msg-nodes)))

(defun jabber-chat-ewoc-invalidate (node)
  "Redraw ewoc NODE without recording undo."
  (let ((buffer-undo-list t))
    (ewoc-invalidate jabber-chat-ewoc node)))

(defun jabber-chat-ewoc-delete (node)
  "Delete ewoc NODE without recording undo."
  (let ((buffer-undo-list t)
        (inhibit-read-only t))
    (ewoc-delete jabber-chat-ewoc node)))

;;; Cleanup on disconnect

(defvar jabber-connections)             ; jabber-core.el

(defun jabber-chatbuffer--kill-stale ()
  "Kill chat buffers whose connection is no longer active."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'jabber-buffer-connection buf)
      (unless (memq (buffer-local-value 'jabber-buffer-connection buf)
                    jabber-connections)
        (kill-buffer buf)))))

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-post-disconnect-hook #'jabber-chatbuffer--kill-stale))

(provide 'jabber-chatbuffer)
;;; jabber-chatbuffer.el ends here
