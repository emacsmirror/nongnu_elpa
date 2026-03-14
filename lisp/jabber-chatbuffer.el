;;; jabber-chatbuffer.el --- functions common to all chat buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2005, 2007, 2008 - Magnus Henoch - mange@freemail.hu

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

;;

;;;###autoload
(defvar-local jabber-buffer-connection nil
  "The connection used by this buffer.")

(defvar jabber-chatting-with)              ; jabber-chat.el
(defvar jabber-group)                      ; jabber-muc.el
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

;; Spell check only what you're currently writing.
(defun jabber-chat-mode-flyspell-verify ()
  "Return non-nil if point is in the composition area."
  (>= (point) jabber-point-insert))

(defvar-keymap jabber-chat-mode-map
  :parent jabber-common-keymap
  "RET"     #'jabber-chat-buffer-send
  "TAB"     #'completion-at-point
  "C-c C-a" #'jabber-chat-attach-file)

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
    (setq jabber-point-insert (point-marker))))

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
  "Regenerate the EWOC text for one or more buffers.
With prefix argument, regenerate all `jabber-chat-mode' buffers,
otherwise regenerate the current buffer display."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (mapc
     (lambda (buffer)
       (with-current-buffer buffer
         (ewoc-refresh jabber-chat-ewoc)
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
