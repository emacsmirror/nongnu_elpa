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

(defun jabber-srv--fetch-answers (target)
  "Perform DNS SRV query for TARGET and return parsed answer records.
Returns a list of alists, each containing priority, weight, port,
and target entries.  Returns nil if no records found, or `:dot' if
the single-dot target (\"service not available\") was returned."
  (let* ((result (jabber-srv--dns-query target))
         (answers (mapcar (lambda (a) (cadr (assq 'data a)))
                          (cadr (assq 'answers result)))))
    (cond
     ((null answers) nil)
     ((and (length= answers 1)
           (string= (cadr (assq 'target (car answers))) "."))
      :dot)
     (t answers))))

(defun jabber-srv--sort-answers (answers)
  "Sort ANSWERS by priority with weighted randomization per RFC 2782.
ANSWERS is a list of alists as returned by `jabber-srv--fetch-answers'.
Returns the entries in connection-attempt order."
  (let (ordered)
    (dolist (group (jabber-srv--group-by-priority answers))
      (setq ordered (nconc ordered
                           (jabber-srv--weighted-select (cdr group)))))
    ordered))

(defun jabber-srv--tag-answers (answers directtls-p)
  "Tag each record in ANSWERS with DIRECTTLS-P flag.
Adds a (directtls DIRECTTLS-P) entry to each alist so the flag
survives the priority/weight sort pipeline."
  (mapcar (lambda (a) (cons (list 'directtls directtls-p) a))
          answers))

(defun jabber-srv--has-fallback-p (targets server)
  "Return non-nil if TARGETS already includes SERVER on port 5222 via STARTTLS."
  (cl-some (lambda (t_)
             (and (string= (nth 0 t_) server)
                  (= (nth 1 t_) 5222)
                  (not (nth 2 t_))))
           targets))

;;;###autoload
(defun jabber-srv-lookup-mixed (server)
  "Query both _xmpps-client and _xmpp-client SRV records for SERVER.
Merges results by priority and weight per RFC 2782.  Returns a list
of (HOST PORT DIRECTTLS-P) where DIRECTTLS-P is non-nil for targets
from _xmpps-client._tcp (XEP-0368 direct TLS).

Always appends SERVER:5222 STARTTLS as a lowest-priority fallback
unless the SRV results already include it."
  (let ((xmpps (condition-case nil
                   (jabber-srv--fetch-answers
                    (concat "_xmpps-client._tcp." server))
                 (error nil)))
        (xmpp (condition-case nil
                  (jabber-srv--fetch-answers
                   (concat "_xmpp-client._tcp." server))
                (error nil))))
    ;; :dot means "service explicitly unavailable"
    (when (eq xmpps :dot) (setq xmpps nil))
    (when (eq xmpp :dot) (setq xmpp nil))
    (let ((merged (nconc (jabber-srv--tag-answers xmpps t)
                         (jabber-srv--tag-answers xmpp nil))))
      (when merged
        (let ((result (mapcar (lambda (a)
                                (list (cadr (assq 'target a))
                                      (cadr (assq 'port a))
                                      (cadr (assq 'directtls a))))
                              (jabber-srv--sort-answers merged))))
          ;; Append domain:5222 STARTTLS fallback if not already present.
          (unless (jabber-srv--has-fallback-p result server)
            (setq result (nconc result (list (list server 5222 nil)))))
          result)))))

;;;###autoload
(defun jabber-srv-lookup (target)
  "Perform SRV lookup of TARGET and return connection candidates.
TARGET is a string of the form \"_Service._Proto.Name\".

Returns a list of (HOST . PORT) pairs sorted by priority with
weighted randomization per RFC 2782.  The caller should attempt
connections in order.  Returns nil if no SRV records were found."
  (let ((answers (jabber-srv--fetch-answers target)))
    (when (and answers (not (eq answers :dot)))
      (mapcar (lambda (a) (cons (cadr (assq 'target a))
                                (cadr (assq 'port a))))
              (jabber-srv--sort-answers answers)))))

(provide 'jabber-srv)

;;; jabber-srv.el ends here
