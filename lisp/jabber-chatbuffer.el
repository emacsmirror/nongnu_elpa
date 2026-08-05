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

;;; Commentary:
;;

;;; Code:

(require 'jabber-util)
(require 'jabber-buffer-registry)
(require 'jabber-input)
(require 'jabber-core)
(require 'jabber-db)
(require 'jabber-muc-protocol)
(require 'help-at-pt)

(defcustom jabber-scrolltobottom-all nil
  "Non-nil means explicit input recentering affects all chat windows.
When `jabber-chat-buffer-recenter-input' is called, recenter all
visible windows displaying the current chat buffer whose window point
is in the input area, at or after `jabber-point-insert'.  The default
nil preserves the current behavior of recentering only one visible
window.  Receive-time message insertion does not automatically adjust
windows."
  :type 'boolean
  :group 'jabber-chat)

(defun jabber-chat-buffer--recenter-input-p (window)
  "Return non-nil when WINDOW should recenter to the input area."
  (and (window-live-p window)
       (with-current-buffer (window-buffer window)
         (and (markerp jabber-point-insert)
              (eq (marker-buffer jabber-point-insert)
                  (window-buffer window))
              (>= (window-point window) jabber-point-insert)))))

(defun jabber-chat-buffer--recenter-input-window (window)
  "Recenter WINDOW so the input area is at the bottom."
  (with-selected-window window
    (let ((resize-mini-windows nil))
      (save-excursion
        (goto-char jabber-point-insert)
        (recenter -1)))))

(defun jabber-chat-buffer--recenter-input-all ()
  "Recenter all visible `current-buffer' windows following the input area."
  (dolist (window (get-buffer-window-list (current-buffer) nil 'visible))
    (when (jabber-chat-buffer--recenter-input-p window)
      (jabber-chat-buffer--recenter-input-window window))))

(defun jabber-chat-buffer-recenter-input ()
  "Recenter visible `current-buffer' window(s) to the input area."
  (if jabber-scrolltobottom-all
      (jabber-chat-buffer--recenter-input-all)
    (when-let* ((window (get-buffer-window (current-buffer))))
      (when (jabber-chat-buffer--recenter-input-p window)
        (jabber-chat-buffer--recenter-input-window window)))))

(defun jabber-chat-buffer-with-scrolltobottom (&rest values)
  "Return the last of VALUES without scroll side effects."
  (car (last values)))

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

(defvar-local jabber-chat-header-line-format-override nil
  "Buffer-specific chat header format, or nil to use the default.")

(defvar-local jabber-chat--backlog-generation 0
  "Generation counter for chunked backlog inserts.
Incremented before each new insert sequence so stale timers from a
previous sequence detect the mismatch and stop.")

(declare-function jabber-muc-nick-completion-at-point
                  "jabber-muc-nick-completion" ())
(autoload 'jabber-muc-nick-completion-at-point "jabber-muc-nick-completion")

(defvar jabber-chatting-with)              ; jabber-chat.el
(defvar jabber-chat-header-line-format)   ; jabber-chat.el
(defvar jabber-chat-earliest-backlog)     ; jabber-chat.el
(defvar jabber-group)                      ; jabber-muc.el
(defvar jabber-muc-header-line-format)    ; jabber-muc.el

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

(defvar jabber-backlog-number)            ; jabber-db.el

(defvar-local jabber-chat-buffer-msg-count nil
  "Per-buffer message count for backlog and sync.
When non-nil, overrides `jabber-backlog-number' for refresh and
MAM sync in this buffer.  Set via the operations menu.")

(defun jabber-chat-buffer-msg-count ()
  "Return the effective message count for this buffer."
  (or jabber-chat-buffer-msg-count jabber-backlog-number))

;; Spell check only what you're currently writing.
(defun jabber-chat-mode-flyspell-verify ()
  "Return non-nil if point is in the composition area."
  (>= (point) jabber-point-insert))

(defun jabber-chat-newline ()
  "Insert a newline in the composition area without sending."
  (interactive)
  (insert "\n"))

(defvar-keymap jabber-chat-mode-map
  "S-<return>"   #'jabber-chat-newline
  "TAB"     #'completion-at-point
  "<backtab>" #'backward-button)

(defcustom jabber-chat-display-help-at-point t
  "When non-nil, show local help at point in chat buffers automatically.
After Emacs has been idle for `help-at-pt-timer-delay' seconds, the
`help-echo' text at point -- such as who reacted to a message -- is
printed in the echo area.  The same information is always available on
demand with \\[display-local-help] and, in a graphical frame, as a mouse
tooltip, regardless of this setting.

Changing this takes effect in newly created chat buffers."
  :type 'boolean
  :group 'jabber-chat)

(defun jabber-chat--enable-help-at-point ()
  "Display `help-echo' at point automatically in the current chat buffer.
Reuse the shared `help-at-pt' idle timer and scope it to this buffer with
a buffer-local `help-at-pt-display-when-idle', leaving other buffers
unaffected."
  (setq-local help-at-pt-display-when-idle '(help-echo))
  (help-at-pt-set-timer))

(define-derived-mode jabber-chat-mode fundamental-mode "jabber-chat"
  "Major mode for Jabber chat buffers.
\\{jabber-chat-mode-map}"
  (visual-line-mode 1)
  (setq-local word-wrap t)
  (display-line-numbers-mode 0)
  (when jabber-chat-display-help-at-point
    (jabber-chat--enable-help-at-point))
  (put 'jabber-chat-mode 'flyspell-mode-predicate #'jabber-chat-mode-flyspell-verify))

;;; bug-reference integration

(defcustom jabber-bug-reference-alist
  '(("jabber-el@conference\\.hmm\\.st"
     "\\(#\\([0-9]+\\)\\)"
     "https://todos.thanosapollo.org/r/emacs-jabber/%s"))
  "Alist mapping JID patterns to `bug-reference-mode' configurations.
Each entry has the form (JID-REGEXP BUG-REGEXP URL-FORMAT).

JID-REGEXP is matched against the MUC room JID (e.g.
\"emacs@conference.jabber.org\") or 1:1 chat partner bare JID.
BUG-REGEXP and URL-FORMAT are set as `bug-reference-bug-regexp'
and `bug-reference-url-format' respectively.

To activate bug references in chat buffers, add
`bug-reference-mode' to `jabber-chat-mode-hook':

  (add-hook \\='jabber-chat-mode-hook #\\='bug-reference-mode)"
  :type '(repeat (list (regexp :tag "JID regexp")
                       (regexp :tag "Bug regexp")
                       (choice :tag "URL format"
                               (string :tag "Format string")
                               (function :tag "Function"))))
  :group 'jabber-chat)

(defun jabber-bug-reference--try-setup (jid)
  "Try to configure `bug-reference-mode' for JID.
Match JID against `jabber-bug-reference-alist' and set the
buffer-local bug-reference variables on the first match."
  (catch 'done
    (dolist (entry jabber-bug-reference-alist)
      (when (string-match-p (nth 0 entry) jid)
        (setq-local bug-reference-bug-regexp (nth 1 entry))
        (setq-local bug-reference-url-format (nth 2 entry))
        (throw 'done t)))))

(defun jabber-bug-reference-setup ()
  "Try setting up `bug-reference-mode' for Jabber chat buffers.
Added to `bug-reference-auto-setup-functions' so that activating
`bug-reference-mode' in a chat buffer automatically configures the
bug regexp and URL format from `jabber-bug-reference-alist'."
  (when (derived-mode-p 'jabber-chat-mode)
    (when-let* ((jid (jabber-chat--peer-jid)))
      (jabber-bug-reference--try-setup jid))))

(add-hook 'bug-reference-auto-setup-functions #'jabber-bug-reference-setup)

(defun jabber-chat-mode-setup (jc ewoc-pp)
  "Initialize chat buffer state for connection JC.
EWOC-PP is the pretty-printer function for the message EWOC."
  (add-hook 'completion-at-point-functions #'jabber-muc-nick-completion-at-point nil t)

  (setq-local jabber-send-function nil)
  (setq-local scroll-conservatively 101)
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
      (unless (eq (bound-and-true-p jabber-omemo--available) t)
        (setq jabber-chat-encryption 'plaintext))))
  (jabber-chat-encryption--update-header))

;; Chat owns rendering while depending on this lower-level buffer module.
(declare-function jabber-chat--insert-backlog-chunked
                  "jabber-chat" (buffer entries callback &optional generation))
(declare-function jabber-chat-display-buffer-images "jabber-chat" ())
(autoload 'jabber-chat--insert-backlog-chunked "jabber-chat")
(autoload 'jabber-chat-display-buffer-images "jabber-chat")

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
With prefix argument ALL-CHATS, regenerate all `jabber-chat-mode'
buffers; otherwise regenerate the current buffer display.
Scroll each buffer so the chat log is visible with the prompt line
at the bottom of the window."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (mapc
     (lambda (buffer)
       (with-current-buffer buffer
         (let ((buffer-undo-list t))
           (ewoc-refresh jabber-chat-ewoc))
         (setq header-line-format
               (or jabber-chat-header-line-format-override
                   (if (bound-and-true-p jabber-group)
                       jabber-muc-header-line-format
                     jabber-chat-header-line-format)))
         (when-let* ((peer (jabber-chat--peer-jid))
                     (saved (jabber-db-get-chat-encryption
                             (jabber-connection-bare-jid
                              jabber-buffer-connection)
                             peer)))
           (setq jabber-chat-encryption saved))
         (jabber-chat-encryption--update-header)
         (force-mode-line-update)
         (jabber-chat-buffer-recenter-input)))
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

(defun jabber-chat-buffer--shift-undo-list (shift)
  "Translate buffer positions in `buffer-undo-list' by SHIFT."
  (unless (or (zerop shift) (atom buffer-undo-list))
    (let ((list buffer-undo-list)
          elt)
      (while list
        (setq elt (car list))
        (cond ((integerp elt)
               (setcar list (+ elt shift)))
              ((or (atom elt)
                   (markerp (car elt)))
               nil)
              ((integerp (car elt))
               (setcar elt (+ (car elt) shift))
               (setcdr elt (+ (cdr elt) shift)))
              ((stringp (car elt))
               (setcdr elt (+ (cdr elt)
                              (* (if (natnump (cdr elt)) 1 -1)
                                 shift))))
              ((null (car elt))
               (let ((cons (nthcdr 3 elt)))
                 (setcar cons (+ (car cons) shift))
                 (setcdr cons (+ (cdr cons) shift)))))
        (setq list (cdr list))))))

(defun jabber-chat-ewoc--muc-data-p (data)
  "Return non-nil when DATA is a MUC message entry."
  (memq (car-safe data) '(:muc-local :muc-foreign :muc-error)))

(defun jabber-chat-ewoc--client-id-key (msg id muc-p)
  "Return the index key for MSG client ID ID.
MUC-P makes client IDs sender-scoped."
  (if (and muc-p (plist-get msg :from))
      (list :muc (plist-get msg :from) id)
    id))

(defun jabber-chat-ewoc-duplicate-p (data)
  "Return non-nil when message DATA is already displayed."
  (let* ((msg (cadr data))
         (msg-p (listp msg))
         (id (and msg-p (plist-get msg :id)))
         (sid (and msg-p (plist-get msg :server-id)))
         (id-key (and id
                      (jabber-chat-ewoc--client-id-key
                       msg id (jabber-chat-ewoc--muc-data-p data)))))
    (or (and id-key (gethash id-key jabber-chat--msg-nodes))
        (and sid (gethash sid jabber-chat--msg-nodes)))))

(defun jabber-chat-ewoc-register-node (node data)
  "Register EWOC NODE under the message identities in DATA."
  (let* ((msg (cadr data))
         (msg-p (listp msg))
         (id (and msg-p (plist-get msg :id)))
         (sid (and msg-p (plist-get msg :server-id)))
         (id-key (and id
                      (jabber-chat-ewoc--client-id-key
                       msg id (jabber-chat-ewoc--muc-data-p data)))))
    (when id-key (puthash id-key node jabber-chat--msg-nodes))
    (when sid (puthash sid node jabber-chat--msg-nodes))
    node))

(defun jabber-chat-ewoc-enter (data)
  "Insert DATA into the chat ewoc and register by stanza ID.
DATA is (TYPE MSG-PLIST).  When the plist has a non-nil :id or
:server-id, the returned ewoc node is stored in
`jabber-chat--msg-nodes' for O(1) lookup.  Returns the ewoc node,
or nil if the message was a duplicate."
  (unless (jabber-chat-ewoc-duplicate-p data)
    (let ((preinsert-point (and (markerp jabber-point-insert)
                                (marker-position jabber-point-insert))))
      (let ((node (let ((buffer-undo-list t))
                    (ewoc-enter-last jabber-chat-ewoc data))))
        (when preinsert-point
          (jabber-chat-buffer--shift-undo-list
           (- jabber-point-insert preinsert-point)))
        (jabber-chat-ewoc-register-node node data)))))

(defun jabber-chat-ewoc--msg-matches-id-p (msg stanza-id)
  "Return non-nil when MSG has STANZA-ID as :id, :origin-id or :server-id."
  (and (listp msg)
       (or (equal stanza-id (plist-get msg :id))
           (equal stanza-id (plist-get msg :origin-id))
           (equal stanza-id (plist-get msg :server-id)))))

(defun jabber-chat-ewoc--find-by-id-scan (stanza-id &optional sender)
  "Scan `jabber-chat-ewoc' for STANZA-ID, optionally from SENDER."
  (let ((node (and jabber-chat-ewoc (ewoc-nth jabber-chat-ewoc 0)))
        found)
    (while (and node (not found))
      (let ((msg (cadr (ewoc-data node))))
        (if (and (jabber-chat-ewoc--msg-matches-id-p msg stanza-id)
                 (or (null sender)
                     (equal sender (plist-get msg :from))))
          (setq found node)
          (setq node (ewoc-next jabber-chat-ewoc node)))))
    found))

(defun jabber-chat-ewoc--backfill-node-ids (node)
  "Backfill non-nil message IDs from NODE into `jabber-chat--msg-nodes'."
  (let* ((msg (cadr (ewoc-data node)))
         (id (and (listp msg) (plist-get msg :id)))
         (sid (and (listp msg) (plist-get msg :server-id)))
         (id-key (and id
                      (jabber-chat-ewoc--client-id-key
                       msg id (jabber-chat-ewoc--muc-data-p
                               (ewoc-data node))))))
    (when id-key (puthash id-key node jabber-chat--msg-nodes))
    (when sid (puthash sid node jabber-chat--msg-nodes))))

(defun jabber-chat-ewoc-find-by-id (stanza-id)
  "Return the ewoc node for STANZA-ID, or nil."
  (when (and stanza-id jabber-chat--msg-nodes)
    (or (gethash stanza-id jabber-chat--msg-nodes)
        (when-let* ((node (jabber-chat-ewoc--find-by-id-scan stanza-id)))
          (jabber-chat-ewoc--backfill-node-ids node)
          node))))

(defun jabber-chat-ewoc-find-by-id-and-sender (stanza-id sender)
  "Return the ewoc node for STANZA-ID sent by full JID SENDER."
  (when (and stanza-id sender jabber-chat--msg-nodes)
    (or (gethash (list :muc sender stanza-id) jabber-chat--msg-nodes)
        (when-let* ((node (jabber-chat-ewoc--find-by-id-scan
                           stanza-id sender)))
          (jabber-chat-ewoc--backfill-node-ids node)
          node))))

(defun jabber-chat-ewoc-unregister-node (node)
  "Remove all message index entries that refer to ewoc NODE."
  (when jabber-chat--msg-nodes
    (let (keys)
      (maphash (lambda (key value)
                 (when (eq value node)
                   (push key keys)))
               jabber-chat--msg-nodes)
      (dolist (key keys)
        (remhash key jabber-chat--msg-nodes)))))

(defun jabber-chat-ewoc-invalidate (node)
  "Redraw ewoc NODE without recording undo."
  (let ((buffer-undo-list t))
    (ewoc-invalidate jabber-chat-ewoc node)))

(defun jabber-chat-ewoc-delete (node)
  "Delete ewoc NODE without recording undo."
  (let ((buffer-undo-list t)
        (inhibit-read-only t))
    (ewoc-delete jabber-chat-ewoc node)))

;;; View preservation across refresh
;;
;; This mirrors ERC's `erc--scrolltobottom-all' (erc-goodies.el): for each
;; visible window, a window at the prompt is recentered to keep the prompt
;; at the bottom, and a window reading history is left where it was.
;;
;; ERC appends, so a history reader's `window-start' and point are never
;; disturbed and its save/restore is only insurance.  A refresh rebuilds
;; the whole ewoc, destroying every `window-start' marker and collapsing
;; point, so we must restore actively.  Two adaptations follow:
;;
;; - Raw positions don't survive the rebuild, so we anchor on a stable
;;   stanza id -- the topmost visible message -- and put that message back
;;   at `window-start' once the ewoc is rebuilt.
;; - We restore every visible window, not just the selected one.  ERC's
;;   selected-window-only mode is safe only because append leaves the rest
;;   untouched; for us, skipping a window means the rebuild clobbers it.
;;
;; The one place we cannot match ERC: it keeps a history reader's exact
;; point, but our rebuild collapses it (and a message's rendered length
;; can change across reload), so we settle point on the anchored message.

(defun jabber-chat-buffer--node-stanza-id (node)
  "Return the stable index key of message ewoc NODE, or nil."
  (and node
       (let* ((data (ewoc-data node))
              (msg (cadr data)))
         (and (listp msg)
              (if (jabber-chat-ewoc--muc-data-p data)
                  (or (plist-get msg :server-id)
                      (when-let* ((id (plist-get msg :id)))
                        (jabber-chat-ewoc--client-id-key msg id t)))
                (or (plist-get msg :id)
                    (plist-get msg :server-id)))))))

(defun jabber-chat-buffer--window-anchor (window)
  "Return a view anchor for WINDOW.
The anchor is the symbol `bottom' when the window follows the input
area, or a (`msg' . STANZA-ID) cons naming the topmost visible message
so the view can be restored after the ewoc is rebuilt."
  (if (jabber-chat-buffer--recenter-input-p window)
      'bottom
    ;; window-start may sit on a rare-time or typing node that won't
    ;; survive the rebuild; step forward to the first message node.
    (let ((node (and jabber-chat-ewoc
                     (ewoc-locate jabber-chat-ewoc (window-start window)))))
      (while (and node (not (jabber-chat-buffer--node-stanza-id node)))
        (setq node (ewoc-next jabber-chat-ewoc node)))
      (if-let* ((id (jabber-chat-buffer--node-stanza-id node)))
          (cons 'msg id)
        'bottom))))

(defun jabber-chat-buffer--capture-view ()
  "Capture per-window view anchors for the current buffer.
Returns an alist mapping each window showing the buffer to the anchor
from `jabber-chat-buffer--window-anchor'.  Call before a refresh clears
the ewoc."
  (mapcar (lambda (window)
            (cons window (jabber-chat-buffer--window-anchor window)))
          (get-buffer-window-list (current-buffer) nil 'visible)))

(defun jabber-chat-buffer--restore-bottom (window)
  "Force WINDOW to the input area, moving point there, then recenter.
For use when point may have collapsed to the top during the rebuild,
i.e. the anchored message is no longer loaded.  Unlike the `bottom'
anchor path, this overwrites point, so do not call it for a window that
was composing a message."
  (when (markerp jabber-point-insert)
    (set-window-point window jabber-point-insert)
    (jabber-chat-buffer--recenter-input-window window)))

(defun jabber-chat-buffer--restore-view (anchors)
  "Restore per-window view from ANCHORS captured before a refresh.
Windows that followed the input area return to the bottom; windows that
were reading history are scrolled back to their anchored message, or to
the bottom when that message is no longer loaded."
  (dolist (entry anchors)
    (let ((window (car entry))
          (anchor (cdr entry)))
      (when (window-live-p window)
        (pcase anchor
          ('bottom
           ;; Point sits in the input area (after the footer) and so
           ;; survived the clear; recenter without moving it to preserve
           ;; a half-typed message's cursor.
           (jabber-chat-buffer--recenter-input-window window))
          (`(msg . ,id)
           (if-let* ((node (if (stringp id)
                               (jabber-chat-ewoc-find-by-id id)
                             (gethash id jabber-chat--msg-nodes))))
               (let ((pos (ewoc-location node)))
                 (set-window-start window pos)
                 (set-window-point window pos))
             (jabber-chat-buffer--restore-bottom window))))))))

;;; Buffer refresh

(defun jabber-chat-buffer--refresh-complete (anchors)
  "Finish a chat buffer refresh after backlog insertion completes.
ANCHORS is the per-window view captured by
`jabber-chat-buffer--capture-view' before the ewoc was cleared."
  (jabber-chat-display-buffer-images)
  (jabber-chat-buffer--restore-view anchors))

(defun jabber-chat-buffer-refresh ()
  "Refresh the current chat buffer from the database without killing it.
Clears the ewoc and reloads backlog entries in place.  Cancels any
in-progress chunked insert by bumping the generation counter.
Uses `jabber-chat-buffer-msg-count' for the number of messages.
Each window's view is captured before the clear and restored after the
reload, so a reader scrolled up in history is not yanked to the top."
  (interactive)
  (cl-incf jabber-chat--backlog-generation)
  (let ((generation jabber-chat--backlog-generation)
        (count (jabber-chat-buffer-msg-count))
        (anchors (jabber-chat-buffer--capture-view))
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
           (entries
            (if (bound-and-true-p jabber-message-thread-id)
                (jabber-db-thread-backlog
                 account peer
                 (or (bound-and-true-p jabber-message-thread-type)
                     msg-type "chat")
                 jabber-message-thread-id count)
              (jabber-db-backlog
               account peer count nil resource msg-type
               (and (boundp 'jabber-message-thread-use-buffers)
                    (not jabber-message-thread-use-buffers))))))
      (if (null entries)
          (progn
            (setq jabber-chat-earliest-backlog (float-time))
            (jabber-chat-buffer--restore-view anchors))
        (setq jabber-chat-earliest-backlog
              (float-time (plist-get (car (last entries)) :timestamp)))
        (jabber-chat--insert-backlog-chunked
         (current-buffer) entries
         (lambda () (jabber-chat-buffer--refresh-complete anchors))
         generation)))))

;;; Cleanup on disconnect


(defun jabber-chatbuffer--kill-stale ()
  "Kill chat buffers whose connection is no longer active."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'jabber-buffer-connection buf)
      (unless (memq (buffer-local-value 'jabber-buffer-connection buf)
                    jabber-connections)
        (kill-buffer buf)))))

;;; MAM hook listeners

(defvar jabber-mam-peer-syncing-functions)    ; jabber-mam.el
(defvar jabber-mam-sync-complete-functions)  ; jabber-mam.el

(defun jabber-chat--handle-mam-peer-syncing (peer type syncing-p)
  "Update syncing indicator for PEER's chat buffer.
TYPE is \"groupchat\" or \"chat\".  SYNCING-P is non-nil when
sync starts, nil when it ends."
  (when-let* ((kind (if (string= type "groupchat") 'muc 'chat))
              (buffer (jabber-buffer-registry-find kind peer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq jabber-chat-mam-syncing syncing-p)
      (force-mode-line-update))))

(add-hook 'jabber-mam-peer-syncing-functions #'jabber-chat--handle-mam-peer-syncing)

(defun jabber-chat--handle-mam-sync-complete (peers)
  "Refresh chat buffers that received MAM messages.
PEERS is a list of (ACCOUNT PEER TYPE) entries."
  (dolist (entry peers)
    (pcase-let* ((`(,account ,peer ,type) entry)
           (parent-buffers
            (seq-filter
             (lambda (candidate)
               (with-current-buffer candidate
                 (and (eq major-mode 'jabber-chat-mode)
                      (not (bound-and-true-p jabber-message-thread-id))
                      (bound-and-true-p jabber-buffer-connection)
                      (equal account
                             (jabber-connection-bare-jid
                              jabber-buffer-connection))
                      (if (string= type "groupchat")
                          (equal peer (bound-and-true-p jabber-group))
                        (and (not (bound-and-true-p jabber-group))
                             (equal peer
                                    (and
                                     (bound-and-true-p jabber-chatting-with)
                                     (jabber-jid-user
                                      jabber-chatting-with))))))))
             (buffer-list))))
      (dolist (thread-buffer (buffer-list))
        (with-current-buffer thread-buffer
          (when (and (bound-and-true-p jabber-message-thread-id)
                     (bound-and-true-p jabber-buffer-connection)
                     (equal account
                            (jabber-connection-bare-jid
                             jabber-buffer-connection))
                     (equal peer
                            (bound-and-true-p jabber-message-thread-peer))
                     (equal type
                            (bound-and-true-p jabber-message-thread-type)))
            (when (get-buffer-window thread-buffer t)
              (jabber-db-mark-message-thread-read
               account peer type jabber-message-thread-id))
            (jabber-chat-buffer-refresh))))
      (dolist (parent-buffer parent-buffers)
        (when (buffer-live-p parent-buffer)
          (with-current-buffer parent-buffer
            (jabber-chat-buffer-refresh)))))))

(add-hook 'jabber-mam-sync-complete-functions #'jabber-chat--handle-mam-sync-complete)

(add-hook 'jabber-post-disconnect-hook #'jabber-chatbuffer--kill-stale)

(provide 'jabber-chatbuffer)
;;; jabber-chatbuffer.el ends here
