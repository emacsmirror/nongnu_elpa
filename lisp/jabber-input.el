;;; jabber-input.el --- Shared Jabber input buffer support  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; State and sending behavior shared by chat and XML console input buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jabber-util)

(defvar jabber-point-insert nil
  "Position where the message being composed starts.")

(defvar jabber-send-function nil
  "Function for sending a message from a Jabber input buffer.")

(defvar-local jabber-buffer-connection nil
  "Jabber connection associated with the current buffer.")

(defvar-local jabber-chat--input-history nil
  "Sent chat inputs, newest first.")

(defvar-local jabber-chat--input-history-index nil
  "Current index while navigating `jabber-chat--input-history'.")

(defvar-local jabber-chat--input-history-draft ""
  "Draft restored after moving forward past the newest history entry.")

(defun jabber-chat--input-position ()
  "Return the start of the current buffer's composition area."
  (unless (and (markerp jabber-point-insert)
               (eq (marker-buffer jabber-point-insert) (current-buffer)))
    (user-error "No Jabber chat input marker in this buffer"))
  (marker-position jabber-point-insert))

(defun jabber-chat--point-in-input-p ()
  "Return non-nil when point is in the composition area."
  (>= (point) (jabber-chat--input-position)))

(defun jabber-chat--input-string ()
  "Return the current composition area as a plain string."
  (buffer-substring-no-properties
   (jabber-chat--input-position) (point-max)))

(defun jabber-chat--replace-input (content)
  "Replace the current composition area with CONTENT."
  (let ((position (jabber-chat--input-position)))
    (delete-region position (point-max))
    (goto-char position)
    (insert content)))

(defun jabber-chat--record-input-history (content)
  "Record non-empty CONTENT in the current buffer's input history."
  (when (and (stringp content) (not (string-empty-p content)))
    (let ((text (substring-no-properties content)))
      (setq jabber-chat--input-history
            (cons text (delete text jabber-chat--input-history))
            jabber-chat--input-history-index nil
            jabber-chat--input-history-draft ""))))

(defun jabber-chat-input-history-previous ()
  "Replace the composition area with the previous sent input."
  (interactive)
  (unless (jabber-chat--point-in-input-p)
    (user-error "Point is outside the Jabber input area"))
  (unless jabber-chat--input-history
    (user-error "No Jabber input history"))
  (when (null jabber-chat--input-history-index)
    (setq jabber-chat--input-history-draft (jabber-chat--input-string)))
  (setq jabber-chat--input-history-index
        (min (1- (length jabber-chat--input-history))
             (1+ (or jabber-chat--input-history-index -1))))
  (jabber-chat--replace-input
   (nth jabber-chat--input-history-index jabber-chat--input-history)))

(defun jabber-chat-input-history-next ()
  "Replace the composition area with the next sent input or saved draft."
  (interactive)
  (unless (jabber-chat--point-in-input-p)
    (user-error "Point is outside the Jabber input area"))
  (unless (numberp jabber-chat--input-history-index)
    (user-error "Already at newest Jabber input"))
  (setq jabber-chat--input-history-index
        (and (> jabber-chat--input-history-index 0)
             (1- jabber-chat--input-history-index)))
  (jabber-chat--replace-input
   (if jabber-chat--input-history-index
       (nth jabber-chat--input-history-index jabber-chat--input-history)
     jabber-chat--input-history-draft)))

(defvar jabber-chat--input-completion nil
  "Completion function captured by an asynchronous input provider.
Call with non-nil on transport handoff, nil on definite failure.
The return value on failure says whether the original draft was restored.")

(defvar jabber-chat--input-deferred nil
  "Non-nil when the current input provider owns deferred completion.")

(defvar-local jabber-chat--input-submission nil
  "Identity of the most recent input submission.")

(defun jabber-chat--capture-input-owner ()
  "Return a predicate for the current input buffer's exact owner.
Track its marker, mode, connection, account and direct or group peer.
A live buffer alone does not authorize completion or local publication."
  (let ((buffer (current-buffer))
        (input jabber-point-insert)
        (mode major-mode)
        (connection jabber-buffer-connection)
        (account (and jabber-buffer-connection
                      (jabber-connection-bare-jid jabber-buffer-connection)))
        (peer (bound-and-true-p jabber-chatting-with))
        (group (bound-and-true-p jabber-group)))
    (lambda ()
      (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (and (eq input jabber-point-insert)
                  (or (null input)
                      (and (markerp input) (eq (marker-buffer input) buffer)))
                  (eq mode major-mode)
                  (eq connection jabber-buffer-connection)
                  (equal account (and connection
                                      (jabber-connection-bare-jid connection)))
                  (equal peer (bound-and-true-p jabber-chatting-with))
                  (equal group (bound-and-true-p jabber-group))))))))

(defun jabber-chat-buffer-send (&optional extra-elements)
  "Send the input composed below the prompt in the current buffer.
EXTRA-ELEMENTS are optional XML elements for the outgoing stanza."
  (interactive)
  (when (cl-plusp (- (point-max) jabber-point-insert))
    (unless (memq jabber-buffer-connection jabber-connections)
      (setq jabber-buffer-connection
            (or (jabber-find-active-connection jabber-buffer-connection)
                (jabber-read-account t))))
    (let* ((buffer (current-buffer))
           (input jabber-point-insert)
           (connection jabber-buffer-connection)
           (owner-p (jabber-chat--capture-input-owner))
           (offset (- (point) input))
           (undo buffer-undo-list)
           (token (list t))
           (body (delete-and-extract-region input (point-max)))
           edited
           (changed
            (lambda (beg _end _old-length)
              ;; Transcript writers bind `inhibit-read-only'; their marker
              ;; shifts do not transfer ownership of the writable tail.
              (when (and (not inhibit-read-only)
                         (marker-position input) (>= beg input))
                (setq edited t))))
           (jabber-chat--input-deferred nil)
           (jabber-chat--input-completion
            (lambda (success)
              (when (car token)
                (setcar token nil)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (remove-hook 'after-change-functions changed t)
                    (when (funcall owner-p)
                      (if success
                          (jabber-chat--record-input-history body)
                        (when (and (eq token jabber-chat--input-submission)
                                   (eq (marker-buffer input) buffer)
                                   (= input (point-max))
                                   (not edited))
                          ;; The rejected edit is not an undo transaction.
                          (let ((buffer-undo-list t))
                            (goto-char input)
                            (insert body))
                          (setq buffer-undo-list undo)
                          (goto-char (+ input (max 0 (min offset (length body)))))
                          t))))))))
           returned)
      (setq jabber-chat--input-submission token)
      (add-hook 'after-change-functions changed nil t)
      (unwind-protect
          (prog1
              (if extra-elements
                  (funcall jabber-send-function connection body extra-elements)
                (funcall jabber-send-function connection body))
            (setq returned t)
            (unless jabber-chat--input-deferred
              (funcall jabber-chat--input-completion t)))
        (unless returned
          (funcall jabber-chat--input-completion nil))))))

(provide 'jabber-input)

;;; jabber-input.el ends here
