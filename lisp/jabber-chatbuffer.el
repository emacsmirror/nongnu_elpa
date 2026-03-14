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

;; Global reference declarations

(declare-function jabber-muc-nick-completion-at-point "jabber-nick-completion.el" ())
(declare-function jabber-httpupload--upload "jabber-httpupload"
                  (jc filepath callback))
(declare-function jabber-omemo--prefetch-sessions "jabber-omemo"
                  (jc jid))
(declare-function jabber-omemo-fingerprints "jabber-omemo" ())
(declare-function jabber-connection-bare-jid "jabber-util" (jc))
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-db-set-chat-encryption "jabber-db"
                  (account peer encryption))
(declare-function jabber-db-get-chat-encryption "jabber-db"
                  (account peer))

;;

;;;###autoload
(defvar-local jabber-buffer-connection nil
  "The connection used by this buffer.")

(defvar jabber-chatting-with)              ; jabber-chat.el
(defvar jabber-chat-header-line-format)   ; jabber-chat.el
(defvar jabber-group)                      ; jabber-muc.el
(defvar jabber-muc-header-line-format)    ; jabber-muc.el
(defvar jabber-httpupload--pending-url)    ; jabber-httpupload.el

(defun jabber-chat-attach-file (filepath)
  "Upload FILEPATH and insert the URL into the composition area.
The file is uploaded via HTTP Upload.  Once the upload finishes,
the GET URL is inserted at point so you can preview and edit
before sending with RET."
  (interactive "fFile to upload: ")
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
                 (const :tag "Plaintext" plaintext))
  :group 'jabber-chat)

(defvar-local jabber-chat-encryption nil
  "Encryption mode for this chat buffer.
Possible values: `plaintext', `omemo'.  Set from
`jabber-chat-default-encryption' on buffer creation.")

(defvar-local jabber-chat-encryption-message ""
  "Header-line string showing current encryption state.")
(put 'jabber-chat-encryption-message 'risky-local-variable t)

(defface jabber-chat-encryption-omemo
  '((t :inherit success))
  "Face for OMEMO encryption indicator in chat header."
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
           (_ "[plaintext]"))
         'face (pcase jabber-chat-encryption
                 ('omemo 'jabber-chat-encryption-omemo)
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
  (setq jabber-chat-encryption 'omemo)
  (jabber-chat-encryption--save 'omemo)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update)
  (when (and jabber-buffer-connection jabber-chatting-with)
    (jabber-omemo--prefetch-sessions
     jabber-buffer-connection jabber-chatting-with)))

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
   ("p" "Plaintext" jabber-chat-encryption-set-plaintext)])

(defun jabber-chat-show-fingerprints ()
  "Display OMEMO fingerprints for the current chat peer."
  (interactive)
  (require 'jabber-omemo)
  (jabber-omemo-fingerprints))

(transient-define-prefix jabber-chat-operations-menu ()
  "Chat buffer operations."
  [["Encryption"
    ("e" "Encryption..." jabber-chat-encryption-menu)
    ("f" "Fingerprints" jabber-chat-show-fingerprints)]
   ["Files"
    ("a" "Attach file" jabber-chat-attach-file)]
   ["Buffer"
    ("r" "Redisplay" jabber-chat-redisplay)]])

;; Spell check only what you're currently writing.
(defun jabber-chat-mode-flyspell-verify ()
  "Return non-nil if point is in the composition area."
  (>= (point) jabber-point-insert))

(defvar-keymap jabber-chat-mode-map
  :parent jabber-common-keymap
  "RET"     #'jabber-chat-buffer-send
  "TAB"     #'completion-at-point
  "C-c C-a" #'jabber-chat-attach-file
  "C-c C-o" #'jabber-chat-operations-menu
  "C-c C-e" #'jabber-chat-encryption-menu)

(define-derived-mode jabber-chat-mode fundamental-mode "jabber-chat"
  "Major mode for Jabber chat buffers.
\\{jabber-chat-mode-map}"
  (display-line-numbers-mode 0)
  (put 'jabber-chat-mode 'flyspell-mode-predicate #'jabber-chat-mode-flyspell-verify))

(defun jabber-chat-mode-setup (jc ewoc-pp)
  "Initialize chat buffer state for connection JC.
EWOC-PP is the pretty-printer function for the message EWOC."
  (make-local-variable 'jabber-send-function)
  (make-local-variable 'scroll-conservatively)
  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-chat-ewoc)
  (make-local-variable 'buffer-undo-list)

  (add-hook 'completion-at-point-functions #'jabber-muc-nick-completion-at-point nil t)

  (setq jabber-buffer-connection jc
        scroll-conservatively 5
        buffer-undo-list t)

  (unless jabber-chat-ewoc
    (setq jabber-chat-ewoc
          (ewoc-create ewoc-pp nil (concat (jabber-separator) "\n") 'nosep))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker)))
  (unless jabber-chat-encryption
    (let ((saved (when-let* ((peer (jabber-chat--peer-jid)))
                   (jabber-db-get-chat-encryption
                    (jabber-connection-bare-jid jabber-buffer-connection)
                    peer))))
      (setq jabber-chat-encryption
            (or saved jabber-chat-default-encryption)))
    (when (eq jabber-chat-encryption 'omemo)
      (require 'jabber-omemo)))
  (jabber-chat-encryption--update-header))

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
otherwise regenerate the current buffer display."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (mapc
     (lambda (buffer)
       (with-current-buffer buffer
         (ewoc-refresh jabber-chat-ewoc)
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
         (forward-line)))
     (seq-filter
      (lambda (buffer)
        (with-current-buffer buffer
          (and (eq major-mode 'jabber-chat-mode)
               (or all-chats
                   (eq buffer current-buffer)))))
      (buffer-list)))))


(provide 'jabber-chatbuffer)
;;; jabber-chatbuffer.el ends here
