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
(require 'jabber-util)

(defvar jabber-point-insert nil
  "Position where the message being composed starts.")

(defvar jabber-send-function nil
  "Function for sending a message from a Jabber input buffer.")

(defvar-local jabber-buffer-connection nil
  "Jabber connection associated with the current buffer.")

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
      (if extra-elements
          (funcall jabber-send-function
                   jabber-buffer-connection body extra-elements)
        (funcall jabber-send-function jabber-buffer-connection body)))))

(provide 'jabber-input)

;;; jabber-input.el ends here
