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

(defun jabber-chat-buffer-send (&optional extra-elements)
  "Send the input composed below the prompt in the current buffer.
EXTRA-ELEMENTS are optional XML elements for the outgoing stanza."
  (interactive)
  (when (cl-plusp (- (point-max) jabber-point-insert))
    (unless (memq jabber-buffer-connection jabber-connections)
      (setq jabber-buffer-connection
            (or (jabber-find-active-connection jabber-buffer-connection)
                (jabber-read-account t))))
    (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
      (prog1
          (if extra-elements
              (funcall jabber-send-function
                       jabber-buffer-connection body extra-elements)
            (funcall jabber-send-function jabber-buffer-connection body))
        (jabber-chat--record-input-history body)))))

(provide 'jabber-input)

;;; jabber-input.el ends here
