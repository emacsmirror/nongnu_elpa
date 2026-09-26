;;; jabber-console.el --- XML Console mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 - Demyan Rogozhin <demyan.rogozhin@gmail.com>
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

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

;; Use *-jabber-console-* for sending custom XMPP code.  Be careful!

;;; Code:

(require 'jabber-keymap)
(require 'jabber-input)
(require 'jabber-stanza)
(require 'jabber-util)
(require 'jabber-truncate)
;; Chat-buffer support requires the core, which loads the console.
(declare-function jabber-chat-buffer--call-with-transcript
                  "jabber-chatbuffer" (function &rest args))
(autoload 'jabber-chat-buffer--call-with-transcript "jabber-chatbuffer")
(require 'xml)
(require 'ewoc)
(require 'sgml-mode) ;we base on this mode to hightlight XML

(defcustom jabber-console-name-format "*-jabber-console-%s-*"
  "Format for console buffer name.  %s mean connection jid."
  :type 'string
  :group 'jabber-debug)

(defcustom jabber-console-truncate-lines 3000
  "Approximate number of lines to retain in the console buffer.
Keep whole XML entries at the cutoff and always retain the newest entry.
Zero disables truncation.  The editable draft is never truncated."
  :type 'integer
  :group 'jabber-debug)

(defvar jabber-console-mode-hook nil
  "Hook called at the end of `jabber-console-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")

(defvar jabber-console-ewoc nil
  "The ewoc showing the XML elements of this stream buffer.")

(defvar-keymap jabber-console-mode-map
  ;; Keep common commands without inheriting special-mode's typing suppression.
  :parent (let ((map (copy-keymap jabber-common-keymap)))
            (set-keymap-parent map sgml-mode-map)
            map)
  "RET" #'jabber-chat-buffer-send)

(defun jabber-console-create-buffer (jc)
  "Get or create the XMPP console buffer for connection JC."
  (with-current-buffer
      (get-buffer-create (format jabber-console-name-format (jabber-connection-bare-jid jc)))
    (unless (eq major-mode 'jabber-console-mode)
      (jabber-console-mode))
    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)
    (current-buffer)))

(defun jabber-console-send (jc data)
  "Echo DATA into the console buffer for JC and send it raw to the server."
  ;; Put manual string into buffers ewoc
  (jabber-process-console jc "raw" data)
  ;; ...than sent it to server
  (jabber-send-string jc data))

(defun jabber-console-comment (str)
  "Insert STR as a timestamped comment into the console buffer."
  (let ((string (concat
                 comment-start str "@" (jabber-encode-time (current-time)) ":"
                 comment-end "\n")))
    (when (stringp jabber-debug-log-xml)
      (jabber-append-string-to-file string jabber-debug-log-xml))
    (insert string)))

(defun jabber-console-pp (data)
  "Pretty-print DATA, an XML-sexp or raw bytes, into the console buffer."
  (let ((start (point))
        (direction (car data))
        (xml-list (cdr data))
        (raw (cadr data)))
    (jabber-console-comment direction)
    (if (stringp raw)
        ;; raw code input
        (progn
          (insert raw)
          (when (stringp jabber-debug-log-xml)
            (jabber-append-string-to-file raw jabber-debug-log-xml)))
      ;; receive/sending
      (progn
        (xml-print xml-list)
        (when (stringp jabber-debug-log-xml)
          (jabber-append-string-to-file
           "\n" jabber-debug-log-xml 'xml-print xml-list))))
    ;; Own the separator too: EWOC's default wrapper adds it after this guard.
    (insert "\n")
    (add-text-properties start (point)
                         '(read-only t front-sticky t rear-nonsticky t))))

(define-derived-mode jabber-console-mode sgml-mode "Jabber Console"
  "Major mode for debug XMPP protocol."
  (setq-local jabber-send-function #'jabber-console-send)
  (setq-local jabber-point-insert nil)
  (setq-local jabber-console-ewoc nil)

  (let ((buffer-undo-list t))
    (setq jabber-console-ewoc
          (ewoc-create #'jabber-console-pp "\n" "<!-- + -->\n" t))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker))))

(put 'jabber-console-mode 'mode-class 'special)

(defun jabber-console-sanitize (xml-data)
  "Sanitize XML-DATA for `jabber-process-console'."
  (if (listp xml-data)
      (jabber-tree-map (lambda (x) (if (numberp x) (format "%s" x) x)) xml-data)
    xml-data))

;;;###autoload
(defun jabber-process-console (jc direction xml-data)
  "Log XML-DATA i/o for JC as XML in \"*-jabber-console-JID-*\" buffer.
DIRECTION is a marker string (typically \"send\", \"recv\", or \"raw\")."
  (with-current-buffer (jabber-console-create-buffer jc)
    (jabber-chat-buffer--call-with-transcript
     #'ewoc-enter-last jabber-console-ewoc
     (list direction (jabber-console-sanitize xml-data)))
    (when (> jabber-console-truncate-lines 0)
      (let ((jabber-log-lines-to-keep jabber-console-truncate-lines))
        (jabber-truncate-top (current-buffer) jabber-console-ewoc)))))

(setq jabber-stanza-log-function #'jabber-process-console)

(provide 'jabber-console)
;;; jabber-console.el ends here
