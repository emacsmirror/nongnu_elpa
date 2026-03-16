;;; jabber-srv.el --- SRV DNS lookups for XMPP  -*- lexical-binding: t; -*-

;; Copyright (C) 2005, 2007, 2018  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; RFC 2782 SRV record lookups.  Originally a separate package (srv.el),
;; now inlined into jabber.el.

;;; Code:

(require 'cl-lib)
(require 'dns)

(defun jabber-srv--dns-query (target)
  "Perform DNS SRV query for TARGET.
Uses `dns-query' on systems with UDP support, falls back to
nslookup on Windows."
  (if (featurep 'make-network-process '(:type datagram))
      (dns-query target 'SRV t)
    (jabber-srv--nslookup target)))

(defun jabber-srv--nslookup (target)
  "Call nslookup to perform an SRV query for TARGET.
Fallback for systems without UDP socket support (Windows)."
  (with-temp-buffer
    (call-process "nslookup" nil t nil "-type=srv" target)
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward
              (concat "[ \t]*priority += \\(.*\\)\r?\n"
                      "[ \t]*weight += \\(.*\\)\r?\n"
                      "[ \t]*port += \\(.*\\)\r?\n"
                      "[ \t]*svr hostname += \\(.*\\)\r?\n")
              nil t)
        (push
         (list
          (list 'data
                (list
                 (list 'priority (string-to-number (match-string 1)))
                 (list 'weight (string-to-number (match-string 2)))
                 (list 'port (string-to-number (match-string 3)))
                 (list 'target (match-string 4)))))
         results))
      (list (list 'answers results)))))

(defun jabber-srv--group-by-priority (answers)
  "Group ANSWERS by priority, sorted lowest first.
Returns an alist of (PRIORITY . ENTRIES)."
  (let (groups)
    (dolist (a answers)
      (let* ((priority (cadr (assq 'priority a)))
             (entry (assq priority groups)))
        (if entry
            (push a (cdr entry))
          (push (cons priority (list a)) groups))))
    (sort groups (lambda (a b) (< (car a) (car b))))))

(defun jabber-srv--weighted-select (entries)
  "Select ENTRIES in weighted random order per RFC 2782.
Returns the entries reordered by weighted random selection."
  (let ((weight-acc 0)
        weight-order
        result)
    (dolist (a entries)
      (cl-incf weight-acc (cadr (assq 'weight a)))
      (push (cons weight-acc a) weight-order))
    (setq weight-order (nreverse weight-order))
    (while weight-order
      (let* ((r (random (1+ weight-acc)))
             (next (cl-dolist (a weight-order)
                     (when (>= (car a) r)
                       (cl-return a)))))
        (push (cdr next) result)
        (setq weight-order (delq next weight-order))))
    (nreverse result)))

;;;###autoload
(defun jabber-srv-lookup (target)
  "Perform SRV lookup of TARGET and return connection candidates.
TARGET is a string of the form \"_Service._Proto.Name\".

Returns a list of (HOST . PORT) pairs sorted by priority with
weighted randomization per RFC 2782.  The caller should attempt
connections in order.  Returns nil if no SRV records were found."
  (let* ((result (jabber-srv--dns-query target))
         (answers (mapcar (lambda (a) (cadr (assq 'data a)))
                          (cadr (assq 'answers result)))))
    (when (and answers
              (not (and (length= answers 1)
                        (string= (cadr (assq 'target (car answers))) "."))))
      (let (ordered)
        (dolist (group (jabber-srv--group-by-priority answers))
          (setq ordered (nconc ordered
                               (jabber-srv--weighted-select (cdr group)))))
        (mapcar (lambda (a) (cons (cadr (assq 'target a))
                                  (cadr (assq 'port a))))
                ordered)))))

(provide 'jabber-srv)

;;; jabber-srv.el ends here
