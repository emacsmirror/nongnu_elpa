;;; vm-avirtual.el --- additional functions for virtual folder selectors  -*- lexical-binding: t; -*-
;;
;; This file is an add-on for VM
;; 
;; Copyright (C) 2000-2006 Robert Widhopf-Fenk
;; Copyright (C) 2024-2025 The VM Developers
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Tested with XEmacs 21.4.19 & VM 7.19
;; Keywords:    VM, virtual folders 
;; X-URL:       http://www.robf.de/Hacking/elisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:
;;
;; Virtual folders are one of the greatest features offered by VM, however
;; sometimes I do not want to visit a virtual folder in order to do something
;; on messages.  E.g. I have a virtual folder selector for spam messages and I
;; want VM to mark those messages matching the selector for deletion when
;; retrieving new messages.  This can be done with a trick described in
;; the VM-FAQ, however this created two new buffers polluting my buffer space.
;; So this package provides a function `vm-auto-delete-messages' for this
;; purpose without drawbacks. 
;; 
;; Then after I realized I was maintaining three different variables for
;; actually the same things.  They were `vm-auto-folder-alist' for automatic
;; selection of folders when saving messages, `vm-virtual-folder-alist' for my
;; loved virtual folders and `vm-pcrisis-conditions' in order to solve the handling
;; of my different email-addresses.
;;
;; This was kind of annoying, since virtual folder selectors offer the
;; best way of specifying conditions, but they only work on messages
;; within folders and not on messages which are currently being
;; composed. So I decided to extend virtual folder selectors also to
;; message composing, although not all of the selectors are meaningful
;; for `mail-mode'.
;;
;; I wrote functions which can replace (*) the existing ones and others that
;; add new (+) functionality.  Finally I came up with the following ones:
;;       * vm-virtual-auto-archive-messages 
;;       * vm-virtual-save-message 
;;       * vmpc-check-virtual-selector
;;       + vm-virtual-auto-delete-messages
;;       + vm-virtual-auto-delete-message
;;       + vm-virtual-omit-message
;;       + vm-virtual-update-folders
;;       + vm-virtual-apply-function
;; and the following variables
;;      vm-virtual-check-case-fold-search
;;      vm-virtual-auto-delete-message-selector
;;      vm-virtual-auto-folder-alist
;;      vm-virtual-message
;; and a couple of new selectors
;;      mail-mode       if in mail-mode evals its `argument' else `nil'
;;      vm-mode         if in vm-mode evals its `arg' else `nil'
;;      eval            evaluates its `arg' (write own complex selectors)
;;
;; So by using theses new features I can maintain just one selector for
;; e.g. my private email-address and get the right folder for saving messages,
;; visiting the corresponding virtual folders, auto archiving, setting the FCC
;; header and setting up `vm-pcrisis-conditions'.  Do you know a mailer than can
;; beat this?
;;
;; My default selector for spam messages:
;; 
;; ("spam" ("received")
;;  (vm-mode
;;   (and (new) (undeleted)
;;        (or
;;         ;; kill all those where all authors/recipients
;;         ;; are unknown to my BBDB, i.e. messages from
;;         ;; strangers who are not recognized by me.
;;         ;; (c't 12/2001) 
;;         (not (in-bbdb))
;;         ;; authors that I do not know
;;         (and (not (in-bbdb authors))
;;              (or
;;               ;;  with bad content
;;               (spam-word)
;;               ;; they hide ID codes by long subjects
;;               (subject "       ")
;;               ;; HTML only messages
;;               (header "^Content-Type: text/html")
;;               ;; for 8bit encoding "chinese" spam
;;               (header "[¡-ÿ][¡-ÿ][¡-ÿ][¡-ÿ]")
;;               ;; for qp-encoding "chinese" spam
;;               (header "=[A-F][0-9A-F]=[A-F][0-9A-F]=[A-F][0-9A-F]=[A-F][0-9A-F]=[A-F][0-9A-F]")
;;               ))))))
;;
;;; Feel free to send me any comments or bug reports.
;;
;;; Code:

(require 'vm-macro)
(require 'vm-vars)
(require 'vm-thread)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

;; FIXME: Cyclic dependency, we can't require `vm-virtual'.
(declare-function vm-vs-spam-word "vm-virtual" (m &optional part))

(declare-function vm-get-folder-buffer "vm" (folder))
;; The following function is erroneously called for fsfemacs as well
(declare-function bbdb-message-search "ext:bbdb-com" (name mail))

;; vm-save.el function
(declare-function vm-save-message "vm-save"
		  (folder &optional count mlist quiet))

;; vm-virtual.el function - cyclic dependency
(declare-function vm-build-virtual-message-list "vm-virtual"
		  (new-messages &optional dont-finalize))

; group already defined in vm-vars

(defgroup vm-avirtual nil
  "VM additional virtual folder selectors and functions."
  :group 'vm-ext)

;;----------------------------------------------------------------------------
(eval-and-compile
  (require 'vm-misc)
  (require 'regexp-opt)
  (require 'time-date)
  (vm-load-features-silent-when-compiling '(bbdb bbdb-autoloads bbdb-com)))

(defconst vm-bbdb-address-headers
  '((authors "From:" "Resent-From:" "Reply-To:" "Sender:")
    (recipients "Resent-To:" "Resent-CC:" "To:" "CC:" "BCC:"))
  "The headers each address class of the `in-bbdb' selector reads.
BBDB 2.x had this as `bbdb-get-addresses-headers' and did the reading;
BBDB 3 has no equivalent -- its own version works from the message the MUA
is showing, which is not what a selector needs.  Issue #567.")

;;----------------------------------------------------------------------------
(defvar vm-mail-virtual-selector-function-alist
  '(;; standard selectors 
    (and . vm-mail-vs-and)
    (or . vm-mail-vs-or)
    (not . vm-mail-vs-not)
    (any . vm-mail-vs-any)
    (header . vm-mail-vs-header)
    (text . vm-mail-vs-text)
    (header-or-text . vm-mail-vs-header-or-text)
    (recipient . vm-mail-vs-recipient)
    (author . vm-mail-vs-author)
    (principal . vm-mail-vs-principal)
    (author-or-recipient . vm-mail-vs-author-or-recipient)
    (subject . vm-mail-vs-subject)
    (sortable-subject . vm-mail-vs-sortable-subject)
    (more-chars-than . vm-mail-vs-more-chars-than)
    (less-chars-than . vm-mail-vs-less-chars-than)
    (more-lines-than . vm-mail-vs-more-lines-than)
    (less-lines-than . vm-mail-vs-less-lines-than)
    (replied . vm-mail-vs-replied)
    (answered . vm-mail-vs-answered)
    (forwarded . vm-mail-vs-forwarded)
    (redistributed . vm-mail-vs-redistributed)
    (unreplied . vm-mail-vs-unreplied)
    (unanswered . vm-mail-vs-unanswered)
    (unforwarded . vm-mail-vs-unforwarded)
    (unredistributed . vm-mail-vs-unredistributed)

    ;; unknown selectors which return always nil
    (new . vm-mail-vs-unknown)
    (unread . vm-mail-vs-unknown)
    (read . vm-mail-vs-unknown)
    (unseen . vm-mail-vs-unknown)
    (recent . vm-mail-vs-unknown)
    (deleted . vm-mail-vs-unknown)
    (filed . vm-mail-vs-unknown)
    (written . vm-mail-vs-unknown)
    (edited . vm-mail-vs-unknown)
    (marked . vm-mail-vs-unknown)
    (undeleted . vm-mail-vs-unknown)
    (unfiled . vm-mail-vs-unknown)
    (unwritten . vm-mail-vs-unknown)
    (unedited . vm-mail-vs-unknown)
    (unmarked . vm-mail-vs-unknown)
    (expanded . vm-mail-vs-unknown)
    (collapsed . vm-mail-vs-unknown)
    (virtual-folder-member . vm-mail-vs-unknown)
    (label . vm-mail-vs-unknown)
    (sent-before . vm-mail-vs-unknown)
    (sent-after . vm-mail-vs-unknown)

    
    ;; new selectors 
    (mail-mode . vm-mail-vs-mail-mode)
    (vm-mode . vm-vs-vm-mode)
    (eval . vm-mail-vs-eval)
    (older-than . vm-mail-vs-older-than)
    (newer-than . vm-mail-vs-newer-than)
    (in-bbdb . vm-mail-vs-in-bbdb)
    ))

;;-----------------------------------------------------------------------------
(defun vm-avirtual-add-selectors (selectors)
  (let ((alist 'vm-virtual-selector-function-alist)
        (sup-alist 'vm-supported-interactive-virtual-selectors)
        sel)
    
    (while selectors
      (setq sel (car selectors))
      (add-to-list alist (cons sel (intern (format "vm-vs-%s" sel))))
      (add-to-list sup-alist (list (format "%s" sel)))
      (setq selectors (cdr selectors)))))

(vm-avirtual-add-selectors
 '(mail-mode 
   vm-mode 
   eval 
   selected 
   in-bbdb 
   folder-name 
   ))

;;-----------------------------------------------------------------------------
;; we redefine the basic selectors for some extra features ...

;; `vm-vs-or\', `vm-vs-and\' and `vm-vs-not\' used to be redefined here, to add
;; the case folding and the diagnostics this file's checker prints.  Both are in
;; the definitions in vm-virtual.el now.  Redefining them here only worked when
;; this file happened to load after that one, and it does not: vm-summary.el
;; pulls this in through vm-summary-faces.el, and vm.el requires vm-virtual
;; afterwards, so the copies here never won and the diagnostics never printed.

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-avirtual-check-for-missing-selectors (&optional arg)
  "Check if there are selectors missing for either vm-mode or mail-mode."
  (interactive "P")
  (let ((a (if arg vm-mail-virtual-selector-function-alist
             vm-virtual-selector-function-alist))
        (b (mapcar (lambda (s) (car s))
                   (if arg vm-virtual-selector-function-alist
                     vm-mail-virtual-selector-function-alist)))
        l)
    (while a
      (if (not (memq (caar a) b))
          (setq l (concat (format "%s" (caar a)) ", " l)))
      (setq a (cdr a)))
    (if l
        (message "Selectors %s are missing" l)
      (message "No selectors are missing"))))

;;---------------------------------------------------------------------------
;; new virtual folder selectors
(defvar vm-virtual-message nil
  "Set to the VM message vector when doing a `vm-vs-eval'.")

(defun vm-vs-folder-name (m regexp)
  "Virtual selector to check if the current folder's name matches REGEXP."
  (setq m (vm-real-message-of m))
  (string-match regexp (buffer-name (marker-buffer (vm-start-of m)))))

(defun vm-vs-vm-mode (&rest selectors)
  (if (not (equal major-mode 'mail-mode))
      (apply 'vm-vs-or selectors)
    nil))

(defun vm-vs-selected (m)
  (save-excursion
    (vm-select-folder-buffer)
    (eq m (car vm-message-pointer))))

(defun vm-bbdb-class-headers (address-class)
  "The headers the `in-bbdb' selector reads for ADDRESS-CLASS.
Every header of every class when ADDRESS-CLASS is nil."
  (if (null address-class)
      (apply #'append (mapcar #'cdr vm-bbdb-address-headers))
    (or (cdr (assq address-class vm-bbdb-address-headers))
        (error "No such address class: %s.  There is %s"
               address-class
               (mapconcat #'symbol-name
                          (mapcar #'car vm-bbdb-address-headers) " and ")))))

(defun vm-bbdb-known-address-p (contents only-first)
  "Whether BBDB has a record for an address in CONTENTS, a header's text.
With ONLY-FIRST, only the first address in it is looked up, which is what
`bbdb-get-only-first-address-p' asked for in BBDB 2.x."
  (let ((addresses (vm-parse-addresses contents))
        (found nil))
    (when (and only-first addresses)
      (setq addresses (list (car addresses))))
    (while (and addresses (not found))
      (let ((components (mail-extract-address-components (car addresses))))
        ;; One call, where this was two: `bbdb-message-search' tries name and
        ;; mail together, then mail, then name.  It also matches exactly
        ;; rather than as a regexp, which is what you want of an address --
        ;; `foo+bar@example.com' is not the regexp anyone meant.  Issue #549.
        (setq found (bbdb-message-search (car components) (cadr components))
              addresses (cdr addresses))))
    found))

(defun vm-bbdb-search-headers (contents-list only-first)
  "Whether BBDB knows an address in any of CONTENTS-LIST."
  (let ((found nil))
    (while (and contents-list (not found))
      (setq found (vm-bbdb-known-address-p (car contents-list) only-first)
            contents-list (cdr contents-list)))
    found))

(defun vm-vs-in-bbdb (m &optional address-class only-first)
  "check if one of the email addresses in the message headers is known
in BBDB."
  ;; `bbdb-message-search' lives in bbdb-com.el and BBDB does not autoload
  ;; it, where the `bbdb-search-simple' this replaced was in bbdb.el.  VM
  ;; never requires BBDB itself, so ask for the file that has it (#549).
  (require 'bbdb-com)
  ;; The addresses are gathered here rather than by BBDB.  2.x's
  ;; `bbdb-get-addresses' took a function to read a header with, which is how
  ;; a selector could ask about a message other than the one on screen; BBDB 3
  ;; dropped it, and its replacement reads the message the MUA is displaying.
  ;; Issue #567.
  (let ((message (vm-real-message-of m)))
    (vm-bbdb-search-headers
     (delq nil (mapcar (lambda (header) (vm-get-header-contents message header))
                       (vm-bbdb-class-headers address-class)))
     only-first)))

(defun vm-mail-vs-in-bbdb (&optional address-class only-first)
  "check if one of the email addresses in the message headers is known
in BBDB."
  (require 'bbdb-com)
  (vm-bbdb-search-headers
   (delq nil (mapcar #'vm-mail-mode-get-header-contents
                     (vm-bbdb-class-headers address-class)))
   only-first))

;;;###autoload
(defun vm-add-spam-word (word)
  "Add a new WORD to the list of spam words."
  (interactive (list (if (region-active-p)
                         (buffer-substring (point) (mark))
                       (read-string "Spam word: "))))
  (save-excursion 
    (when (not (member word vm-spam-words))
      (if (get-file-buffer vm-spam-words-file)
          (set-buffer (get-file-buffer vm-spam-words-file))
        (set-buffer (find-file-noselect vm-spam-words-file)))
      (goto-char (point-max))
      ;; if the last character is no newline, then append one!
      (if (and (not (= (point) (point-min)))
               (save-excursion
                 (backward-char 1)
                 (not (looking-at "\n"))))
          (insert "\n"))
      (insert word)
      ;; FIXME Why not basic-save-buffer?
      (save-buffer)
      (setq vm-spam-words (cons word vm-spam-words))
      (setq vm-spam-words-regexp (regexp-opt vm-spam-words)))))

;;;###autoload
(defun vm-spam-words-rebuild ()
  "Discharge the internal cached data about spam words."
  (interactive)
  (setq vm-spam-words nil
        vm-spam-words-regexp nil)
  (if (get-file-buffer vm-spam-words-file)
      (kill-buffer (get-file-buffer vm-spam-words-file)))
  (vm-vs-spam-word nil)
  (vm-inform 5 "%d spam words are installed" (length vm-spam-words)))

;;---------------------------------------------------------------------------
;; new mail virtual folder selectors 

(defun vm-mail-vs-eval (&rest selectors)
  (eval (cadr selectors)))

(defun vm-mail-vs-mail-mode (&rest selectors)
  (if (equal major-mode 'mail-mode)
      (apply 'vm-mail-vs-or selectors)
    nil))

(defalias 'vm-vs-mail-mode 'vm-mail-vs-mail-mode)

(defun vm-mail-vs-or (&rest selectors)
  (let ((result nil) selector arglist
        (case-fold-search vm-virtual-check-case-fold-search))
    (while selectors
      (setq selector (car (car selectors))
            arglist (cdr (car selectors))
            result (apply (cdr (assq selector
                                     vm-mail-virtual-selector-function-alist))
                          arglist)
            selectors (if result nil (cdr selectors)))
      (if vm-virtual-check-diagnostics
          (princ (format "%sor: %s (%S%s)\n" 
                         (make-string vm-virtual-check-level ? )
                         (if result t nil) selector
                         (if arglist (format " %S" arglist) "")))))
    result))

(defun vm-mail-vs-and (&rest selectors)
  (let ((result t) selector arglist)
    (while selectors
      (setq selector (car (car selectors))
            arglist (cdr (car selectors))
            result (apply (cdr (assq selector
                                     vm-mail-virtual-selector-function-alist))
                          arglist)
            selectors (if (null result) nil (cdr selectors)))
      (if vm-virtual-check-diagnostics
          (princ (format "%sand: %s (%S%s)\n" 
                         (make-string vm-virtual-check-level ? )
                         (if result t nil) selector
                         (if arglist (format " %S" arglist) "")))))
    result))

(defun vm-mail-vs-not (arg)
  (let ((selector (car arg))
        (arglist (cdr arg))
        result)
    (setq result 
	  (apply 
	   (cdr (assq selector vm-mail-virtual-selector-function-alist))
	   arglist))
    (if vm-virtual-check-diagnostics
        (princ (format "%snot: %s for (%S%s)\n"
                       (make-string vm-virtual-check-level ? )
                       (if result t nil) selector
                       (if arglist (format " %S" arglist) ""))))
    (not result)))

;; return just nil for those selectors not known for mail-mode
(defun vm-mail-vs-unknown (&optional _arg)
  nil)

(defun vm-mail-vs-any ()
  t)

(defun vm-mail-vs-author (arg)
  (let ((val (vm-mail-mode-get-header-contents "Sender\\|From:")))
    (and val (string-match arg val))))

(defun vm-mail-vs-principal (arg)
  (let ((val (vm-mail-mode-get-header-contents "Reply-To:")))
    (and val (string-match arg val))))

(defun vm-mail-vs-recipient (arg)
  (let (val)
    (or
     (and (setq val (vm-mail-mode-get-header-contents "\\(Resent-\\)?To:"))
          (string-match arg val))
     (and (setq val (vm-mail-mode-get-header-contents "\\(Resent-\\)?CC:"))
          (string-match arg val))
     (and (setq val (vm-mail-mode-get-header-contents "\\(Resent-\\)?BCC:"))
          (string-match arg val)))))

(defun vm-mail-vs-author-or-recipient (arg)
  (or (vm-mail-vs-author arg)
      (vm-mail-vs-recipient arg)))

(defun vm-mail-vs-subject (arg)
  (let ((val (vm-mail-mode-get-header-contents "Subject:")))
    (and val (string-match arg val))))

(defun vm-mail-vs-sortable-subject (arg)
  (let ((case-fold-search t)
        (subject (vm-mail-mode-get-header-contents "Subject:")))
    (when subject
      (setq subject (vm-so-trim-subject subject))
      (string-match arg subject))))

(defun vm-mail-vs-header (arg)
  (save-excursion
    (let ((start (point-min)) end)
      (goto-char start)
      (search-forward (concat "\n" mail-header-separator "\n"))
      (setq end (match-beginning 0))
      (goto-char start)
      (re-search-forward arg end t))))

(defun vm-mail-vs-text (arg)
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (re-search-forward arg (point-max) t)))

(defun vm-mail-vs-header-or-text (arg)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward arg (point-max) t)))

(defun vm-mail-vs-more-chars-than (arg)
  (> (- (point-max) (point-min) (length mail-header-separator) 2) arg))

(defun vm-mail-vs-less-chars-than (arg)
  (< (- (point-max) (point-min) (length mail-header-separator) 2) arg))

(defun vm-mail-vs-more-lines-than (arg)
  (> (- (count-lines (point-min) (point-max)) 1) arg))

(defun vm-mail-vs-less-lines-than (arg)
  (< (- (count-lines (point-min) (point-max)) 1) arg))

(defun vm-mail-vs-replied ()
  vm-reply-list)
(fset 'vm-mail-vs-answered 'vm-mail-vs-replied)

(defun vm-mail-vs-forwarded ()
  vm-forward-list)

(defun vm-mail-vs-redistributed ()
  (vm-mail-mode-get-header-contents "Resent-[^:]+:"))

(defun vm-mail-vs-unreplied ()
  (not (vm-mail-vs-replied)))
(fset 'vm-mail-vs-unanswered 'vm-mail-vs-unreplied)

(defun vm-mail-vs-unforwarded ()
  (not (vm-mail-vs-forwarded )))

(defun vm-mail-vs-unredistributed ()
  (not (vm-mail-vs-redistributed )))

(defun vm-mail-vs-older-than (arg)
  (let* ((date (vm-mail-mode-get-header-contents "Date:"))
         (days (and date (days-between (current-time-string) date))))
    (and days (> days arg))))

(defun vm-mail-vs-newer-than (arg)
  (let* ((date (vm-mail-mode-get-header-contents "Date:"))
         (days (and date (days-between (current-time-string) date))))
    (and days (<= days arg))))

;;----------------------------------------------------------------------------

(defun vm-virtual-folder-member-p (name folder-list)
  "Checks if the VM folder with NAME, currently loaded, is among
the folders listed in FOLDER-LIST."
  (let (buffer)
    (catch 'found
      (while folder-list
	(setq buffer (vm-get-folder-buffer (car folder-list)))
	(when (and buffer (buffer-name buffer)
		   (string-match name (buffer-name buffer)))
	  (throw 'found t))
	(setq folder-list (cdr folder-list)))
      nil)))
        
;;;###autoload
(defun vm-virtual-get-selector (vfolder &optional valid-folder-list)
  "Return the selector of virtual folder VFOLDER for VALID-FOLDER-LIST."
  (interactive 
   (list (vm-read-string "Virtual folder: " vm-virtual-folder-alist)
         (if (equal major-mode 'mail-mode) 
	     nil
           (save-excursion 
	     (vm-select-folder-buffer)
	     (list (buffer-name))))))

  (let ((clauses (cadr (assoc vfolder vm-virtual-folder-alist)))
        (selector nil)
	(folders valid-folder-list))
    (when clauses
      (if (null folders)
          (setq selector (append (cdr clauses) selector))
        (while folders
          (when (vm-virtual-folder-member-p (car folders) (car clauses))
              (setq selector (append (cdr clauses) selector)))
          (setq folders (cdr folders)))))

    selector))

;;-----------------------------------------------------------------------------

;;;###autoload
(defun vm-virtual-check-selector (selector &optional msg virtual)
  "Return t if SELECTOR matches the message MSG.
If VIRTUAL is true we check the current message and not the real one."
  (if msg
      (if virtual
          (apply 'vm-vs-or msg selector)
        (with-current-buffer (vm-buffer-of (vm-real-message-of msg))
          (apply 'vm-vs-or msg selector)))
    (if (eq major-mode 'mail-mode)
        (apply 'vm-mail-vs-or selector))))

;;;###autoload
(defun vm-virtual-check-selector-interactive (selector &optional diagnostics)
  "Return t if SELECTOR matches the current message.
Called with an prefix argument we display more diagnostics about the selector
evaluation.  Information is displayed in the order of evaluation and indented
according to the level of recursion. The displayed information is has the
format: 
	FATHER-SELECTOR: RESULT CHILD-SELECTOR"
  (interactive 
   (list  (vm-read-string "Virtual folder: " vm-virtual-folder-alist)
          current-prefix-arg))
  (save-excursion
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (vm-follow-summary-cursor)
    (let ((msg (car vm-message-pointer))
          (virtual (eq major-mode 'vm-virtual-mode))
          (vm-virtual-check-diagnostics (or vm-virtual-check-diagnostics
                                            diagnostics)))
      (with-output-to-temp-buffer "*VM virtual-folder-check*"
        (with-current-buffer "*VM virtual-folder-check*"
          (toggle-truncate-lines t))
        (princ (format "Checking %S on <%s> from %s\n\n" selector
                       (vm-su-subject msg) (vm-su-from msg)))
        (princ (format "\nThe virtual folder selector `%s' is %s\n"
                       selector
                       (if (vm-virtual-check-selector
                            (vm-virtual-get-selector selector)
                            msg virtual)
                           "true"
                         "false")))))))

;;----------------------------------------------------------------------------
(defvar vm-pcrisis-current-state nil)
;;;###autoload
(defun vm-pcrisis-virtual-check-selector (selector &optional folder-list)
  "Checks SELECTOR based on the Personality Crisis state, original or current."
  (setq selector (vm-virtual-get-selector selector folder-list))
  (if (null selector)
      (error "no virtual folder %s!" selector))
  (cond ((or (eq vm-pcrisis-current-state 'reply)
             (eq vm-pcrisis-current-state 'forward)
             (eq vm-pcrisis-current-state 'resend))
         (vm-virtual-check-selector selector (car vm-message-pointer)))
        ((eq vm-pcrisis-current-state 'automorph)
         (vm-virtual-check-selector selector))))

;;----------------------------------------------------------------------------
;;;###autoload
(defun vm-virtual-apply-function (count &optional selector function)
  "Apply a FUNCTION to the next COUNT messages matching SELECTOR." 
  (interactive "p")
  (when (vm-interactive-p)
      (vm-follow-summary-cursor)
      (setq selector (vm-virtual-get-selector
                      (vm-read-string "Virtual folder: "
                                      vm-virtual-folder-alist)))
      (setq function
	    (key-binding (read-key-sequence "VM command: "))))

  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))

  (let ((mlist (vm-select-operable-messages 
		(or count 1) (vm-interactive-p)"Apply to"))
        (count 0))

    (while mlist
      (if (vm-virtual-check-selector selector (car mlist))
          (progn (funcall function (car mlist))
                 (vm-increment count)))
      (setq mlist (cdr mlist)))

    count))

;;----------------------------------------------------------------------------
;;;###autoload
(defun vm-virtual-update-folders (&optional count message-list)
  "Add the current message to all virtual folders that are
applicable.  

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are added.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
added.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are added, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are added."
  (interactive "p")
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))

  (let ((new-messages (or message-list
                          (vm-select-operable-messages
			   count (vm-interactive-p) "Update")))
        b-list)
    (setq new-messages (copy-sequence new-messages))
    (if (and new-messages vm-virtual-buffers)
        (save-excursion
          (setq b-list vm-virtual-buffers)
          (while b-list
            ;; buffer might be dead
            (if (buffer-name (car b-list))
                (let (tail-cons)
                  (set-buffer (car b-list))
                  (setq tail-cons (vm-last vm-message-list))
                  (vm-build-virtual-message-list new-messages)
                  (if (or (null tail-cons) (cdr tail-cons))
                      (progn
                        (setq vm-ml-sort-keys nil)
                        (if vm-thread-obarray
                            (vm-build-threads (cdr tail-cons)))
                        (vm-set-summary-redo-start-point
                         (or (cdr tail-cons) vm-message-list))
                        (vm-set-numbering-redo-start-point
                         (or (cdr tail-cons) vm-message-list))
                        (if (null vm-message-pointer)
                            (progn (setq vm-message-pointer vm-message-list
                                         vm-need-summary-pointer-update t)
                                   (if vm-message-pointer
                                       (vm-present-current-message))))
                        (setq vm-messages-needing-summary-update new-messages
                              vm-need-summary-pointer-update t)
                        (vm-update-summary-and-mode-line)
                        (if vm-summary-show-threads
                            (vm-sort-messages (or vm-ml-sort-keys "activity")))))))
            (setq b-list (cdr b-list)))))
    new-messages))

;;----------------------------------------------------------------------------
(defun vm-virtual-deregister-message (m)
  "Detach the virtual message M, which has left its folder's message list.
Nothing may reach M through its real message afterwards.  Step 2 of
`vm-expunge-folder' walks the real message's mirrors and expunges each one from
its own folder, so a mirror left registered is expunged from a list it is not
in: its reverse link is stale, and the message that link now precedes is
spliced out and flagged expunged instead.  Attributes are shared with the real
message, so that flag comes back to the real folder and its expunge loop
carries on into messages nobody deleted (#569).

M's reverse link goes too, having nothing to describe."
  (let ((real-m (vm-real-message-of m)))
    (vm-set-virtual-messages-of
     real-m (delq m (vm-virtual-messages-of real-m))))
  (vm-set-reverse-link-of m nil))

;;;###autoload
(defun vm-virtual-omit-message (&optional count message-list)
  "Omits a message from a virtual folder.
IMHO allowing it for real folders makes no sense.  One rather should create a
virtual folder of all messages."
  (interactive "p")
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))

  (if (not (eq major-mode 'vm-virtual-mode))
      (error "This is no virtual folder."))

  (let ((old-messages (or message-list
                          (vm-select-operable-messages
			   count (vm-interactive-p) "Omit")))
        prev curr
        (mp vm-message-list))

    (while mp
      (if (not (member (car mp) old-messages))
          nil
        (setq prev (vm-reverse-link-of (car mp))
              curr (or (cdr prev) vm-message-list))
        (vm-set-numbering-redo-start-point (or prev t))
        (vm-set-summary-redo-start-point (or prev t))
        (if (eq vm-message-pointer curr)
            (setq vm-system-state nil
                  vm-message-pointer (or prev (cdr curr))))
        (if (eq vm-last-message-pointer curr)
            (setq vm-last-message-pointer nil))
        (if (null prev)
            (progn
              (setq vm-message-list (cdr vm-message-list))
              (and (cdr curr)
                   (vm-set-reverse-link-of (car (cdr curr)) nil)))
          (setcdr prev (cdr curr))
          (and (cdr curr)
               (vm-set-reverse-link-of (car (cdr curr)) prev)))
        ;; CURR is out of the list, so its message is out of the folder.
        (vm-increment vm-message-list-generation)
        (vm-virtual-deregister-message (car curr)))
      (setq mp (cdr mp)))

    (vm-update-summary-and-mode-line)
    (if vm-summary-show-threads
        (vm-sort-messages (or vm-ml-sort-keys "activity")))
    old-messages))

;;----------------------------------------------------------------------------

(defcustom vm-virtual-auto-delete-message-selector "spam"
  "*Name of virtual folder selector used for automatically deleting a message.
Actually they are only marked for deletion."
  :group 'vm-avirtual
  :type 'string)

(defcustom vm-virtual-auto-delete-message-folder nil
  "*When set to a folder name we save affected messages there."
  :group 'vm-avirtual
  :type '(choice (file :tag "VM folder" "spam")
                 (const :tag "Disabled" nil)))

(defcustom vm-virtual-auto-delete-message-expunge nil
  "*When true we expunge the affected right after marking and saving them."
  :group 'vm-avirtual
  :type 'boolean)

;;;###autoload
(defun vm-virtual-auto-delete-message (&optional count selector)
  "*Mark messages matching a virtual folder selector for deletion.
The virtual folder selector can be configured by the variable
`vm-virtual-auto-delete-message-selector'.

This function does not visit the virtual folder, but checks only the current
message, therefore it is much faster and not so disturbing like the method
described in the VM-FAQ.

In order to automatically mark spam for deletion use the function
`vm-virtual-auto-delete-messages'.  See its documentation on how to hook it
into VM!"
  (interactive "p")
  
  (setq selector (or selector
                       (vm-virtual-get-selector
                        vm-virtual-auto-delete-message-selector)))

  (let (spammlist)
    (setq count (vm-virtual-apply-function
                 count
                 selector
                 (lambda (msg)
		   (setq spammlist (cons msg spammlist))
		   (vm-set-labels
		    msg (list vm-virtual-auto-delete-message-selector))
		   (vm-set-deleted-flag msg t)
		   (vm-mark-for-summary-update msg t))))

    (when spammlist
      (setq spammlist (reverse spammlist))
      ;; save them 
      (if vm-virtual-auto-delete-message-folder
          (let ((vm-arrived-messages-hook nil)
                (vm-arrived-message-hook nil)
                (mlist spammlist))
            (while mlist
              (let ((vm-message-pointer mlist))
                (vm-save-message vm-virtual-auto-delete-message-folder))
              (setq mlist (cdr mlist)))))
      ;; expunge them 
      (if vm-virtual-auto-delete-message-expunge
          (vm-expunge-folder :quiet t :just-these-messages spammlist)))
    
    (vm-display nil nil '(vm-delete-message vm-delete-message-backward)
                (list this-command))
    
    (vm-update-summary-and-mode-line)
    
    (message "%s message%s %s"
             (if (> count 0) count "No")
             (if (= 1 count) "" "s")
             (concat
              (if vm-virtual-auto-delete-message-folder
                  (format "saved to %s and "
                          vm-virtual-auto-delete-message-folder)
                "")
              (if vm-virtual-auto-delete-message-expunge
                  "expunged right away"
                "marked for deletion")))))
  
;;;###autoload
(defun vm-virtual-auto-delete-messages ()
  "*Mark all messages from the current up to the last for (spam-)deletion.
Add this to `vm-arrived-messages-hook'.

See the function `vm-virtual-auto-delete-message' for details.

 (add-hook \\='vm-arrived-messages-hook #\\='vm-virtual-auto-delete-messages)
"
  (interactive)

  (if (vm-interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-virtual-auto-delete-message (length vm-message-pointer)))

;;----------------------------------------------------------------------------
;; Filtering by a table of selectors, rather than the single selector of
;; `vm-virtual-auto-delete-message-selector'.

;;;###autoload
(defcustom vm-virtual-filter-alist nil
  "*Rules deciding what happens to a message when it arrives.
Non-nil value should be an alist of the form

        ((VIRTUAL-FOLDER-NAME . ACTIONS)
          ...)

where VIRTUAL-FOLDER-NAME names a virtual folder in
`vm-virtual-folder-alist', whose selector says which messages the rule
applies to, and ACTIONS is a property list of what to do with them:

  :label STRING       attach the labels named in STRING, which is a
                      list separated by spaces or commas, as `vm-add-message-labels'
                      takes them
  :attributes STRING  set the attributes named in STRING, a space
                      separated list of `vm-supported-attribute-names',
                      as `vm-set-message-attributes' takes them
  :save FOLDER        save a copy in FOLDER.  FOLDER is a string or an
                      expression evaluating to one
  :skip-inbox t       keep the message out of the folder: it is flagged
                      deleted and expunged once every rule has run

Every rule that matches is applied, in the order they appear here, so a
message can be labelled by one rule and saved by another.

To have the rules run on incoming mail:

 (add-hook \\='vm-arrived-messages-hook #\\='vm-virtual-filter-new-messages)

The message is written into the folder before any of this happens, so
`:skip-inbox' removes it again rather than preventing its arrival.

An example, taking two rules from `vm-virtual-folder-alist':

 (setq vm-virtual-folder-alist
       \\='((\"from-arik\" ((\"inbox\") (author \"arik\")))
         (\"spam\"      ((\"inbox\") (spam-word)))))
 (setq vm-virtual-filter-alist
       \\='((\"from-arik\" :label \"arik\" :attributes \"read\")
         (\"spam\"      :save \"spam-folder\" :skip-inbox t)))"
  :group 'vm-avirtual
  :type '(repeat
          (cons :tag "Rule"
                (string :tag "Virtual folder name")
                (plist :options ((:label string)
                                 (:attributes string)
                                 (:save sexp)
                                 (:skip-inbox boolean))))))

(defun vm-virtual-filter-selector (vfolder)
  "Return the selector of virtual folder VFOLDER, which must be defined.
Unlike `vm-virtual-get-selector' this signals rather than returning nil,
because a rule of `vm-virtual-filter-alist' naming a folder that does
not exist would otherwise match nothing and say nothing."
  (or (vm-virtual-get-selector vfolder)
      (error (concat "No virtual folder %S for a rule of "
                     "vm-virtual-filter-alist; define it in "
                     "vm-virtual-folder-alist, which has %s")
             vfolder
             (if vm-virtual-folder-alist
                 (mapconcat (lambda (f) (format "%S" (car f)))
                            vm-virtual-folder-alist ", ")
               "no folders in it"))))

(defun vm-virtual-filter-save (m folder)
  "Save message M in FOLDER, which is a string or an expression giving one."
  (let ((vm-message-pointer (list m))
        (vm-arrived-messages-hook nil)
        (vm-arrived-message-hook nil))
    (vm-save-message (if (stringp folder) folder (eval folder t)))))

(defun vm-virtual-filter-act (m actions)
  "Carry out ACTIONS on message M.  Return t if M is to skip the inbox."
  (when (plist-get actions :label)
    (vm-add-or-delete-message-labels (plist-get actions :label) (list m) 'all))
  (when (plist-get actions :attributes)
    (dolist (name (vm-parse (plist-get actions :attributes)
                            "[ \t]*\\([^ \t]+\\)"))
      (vm-set-message-attribute m name)))
  (when (plist-get actions :save)
    (vm-virtual-filter-save m (plist-get actions :save)))
  (when (plist-get actions :skip-inbox)
    (vm-set-deleted-flag m t)
    (vm-mark-for-summary-update m t)
    t))

(defun vm-virtual-filter-message (m)
  "Apply every rule of `vm-virtual-filter-alist' that matches message M.
Return nil if no rule matched, `skip' if M is to skip the inbox, and t
if a rule matched but M stays."
  (let ((result nil))
    (dolist (rule vm-virtual-filter-alist result)
      (when (vm-virtual-check-selector
             (vm-virtual-filter-selector (car rule)) m)
        (setq result (if (vm-virtual-filter-act m (cdr rule))
                         'skip
                       (or result t)))))))

;;;###autoload
(defun vm-virtual-filter-messages (&optional count)
  "Apply `vm-virtual-filter-alist' to the next COUNT messages.
Messages matched by a rule with `:skip-inbox' are expunged once every
rule has run.  Returns the number of messages some rule matched."
  (interactive "p")
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((mlist (vm-select-operable-messages
                (or count 1) (vm-interactive-p) "Filter"))
        (matched 0)
        (skipped nil))
    (dolist (m mlist)
      (let ((result (vm-virtual-filter-message m)))
        (when result
          (vm-increment matched))
        (when (eq result 'skip)
          (setq skipped (cons m skipped)))))
    (when skipped
      ;; back into folder order, and keep the list: its length is reported
      (setq skipped (nreverse skipped))
      (vm-expunge-folder :quiet t :just-these-messages skipped))
    (vm-update-summary-and-mode-line)
    (when (> matched 0)
      (vm-inform 5 "%d message%s filtered%s" matched
                 (if (= matched 1) "" "s")
                 (if skipped
                     (format ", %d expunged" (length skipped))
                   "")))
    matched))

;;;###autoload
(defun vm-virtual-filter-new-messages ()
  "Apply `vm-virtual-filter-alist' to the messages that have just arrived.
Add this to `vm-arrived-messages-hook':

 (add-hook \\='vm-arrived-messages-hook #\\='vm-virtual-filter-new-messages)

Like `vm-virtual-auto-delete-messages', this runs from the current
message to the last, which on arrival is exactly the new mail."
  (interactive)
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-virtual-filter-messages (length vm-message-pointer)))

;;----------------------------------------------------------------------------
;;;###autoload
(defcustom vm-virtual-auto-folder-alist nil
  "*Non-nil value should be an alist that VM will use to choose a default
folder name when messages are saved.  The alist should be of the form
        ((VIRTUAL-FOLDER-NAME FOLDER-NAME)
          ...)
where VIRTUAL-FOLDER-NAME is a string, and FOLDER-NAME
is a string or an s-expression that evaluates to a string.

Each entry is a two-element list, as the example below shows.  This said
\"(VIRTUAL-FOLDER-NAME . FOLDER-NAME)\" until 2026-08-12; the entry is read
with `cadr', so a dotted pair whose tail is the folder name signals
\"wrong-type-argument listp\" instead of saving anything.

This allows you to extend `vm-virtual-auto-select-folder' to generate
a folder name.  Your function may use `folder' to get the currently chosen
folder name and `mp' (a vm-message-pointer) to access the message. 

Example:
 (setq vm-virtual-auto-folder-alist
       \\='((\"spam\" (concat folder \"-\"
                           (format-time-string \"%y%m\" (current-time))))))

This will return \"spam-0008\" as a folder name for messages matching the
virtual folder selector of the virtual folder \"spam\" during August in year
2000."
  :type 'sexp
  :group 'vm-avirtual)

(defcustom vm-virtual-make-up-auto-folder-names t
  "*Non-nil value causes the vm-avirtual.el package to make up
auto-folder names from virtual folder names, so that all messages
belonging to a virtual folder are saved to real folders with the
same name.  Any auto-folder names suggested in
`vm-virtual-auto-folder-alist' will take priority over such made
up names."
  :type 'boolean
  :group 'vm-avirtual)


;;;###autoload
(defun vm-virtual-auto-select-folder (&optional m virtual-folder-alist
                                                valid-folder-list
                                                not-to-history)
  "Return the first matching virtual folder.
This is a more powerful replacement of `vm-auto-select-folder'.
It is used by `vm-virtual-save-message' for finding the folder to
save the current message.  It may also be used for finding the
right FCC for outgoing messages.                    RobF, 2004-05-02

The matching virtual folder is found for the message M (defaults to
the current message).

VIRTUAL-FOLDER-ALIST is the association list of virtual folder definitions
 (defaults to `vm-virtual-folder-alist').

VALID-FOLDER-LIST is the list of folder names that may be regarded as
the folder names of the message.  (The default is the name of
the current folder.  If the message is virtual, it is the folder name of
the underlying real message.)

NOT-TO-HISTORY says that the history of last saved folders should not
be altered.  (By default, it is updated with the folder name returned
by this function.)

This is not yet the whole story!                    USR, 2013-01-18"

  (unless m (setq m (car vm-message-pointer)))
  (unless virtual-folder-alist
    (setq virtual-folder-alist vm-virtual-folder-alist))
  (unless valid-folder-list
    (setq valid-folder-list 
	  (cond ((eq major-mode 'mail-mode)
		 nil)
		((eq major-mode 'vm-mode)
		 (save-excursion
		   (vm-select-folder-buffer)
		   (list (buffer-name))))
		((eq major-mode 'vm-virtual-mode)
		 (list (buffer-name
			(vm-buffer-of
			 (vm-real-message-of m))))))))
  
  (let ((vfolders virtual-folder-alist)
        vfolder selector matching-vfolders auto-folders)

    (when t;(and m (aref m 0) (aref (aref m 0) 0)
                  ;; set matching-vfolders in reverse order of priority
      (while vfolders
	(setq vfolder (caar vfolders))
        (setq selector (vm-virtual-get-selector 
			vfolder valid-folder-list))
        (when (and selector (vm-virtual-check-selector selector m))
          (setq matching-vfolders (cons vfolder matching-vfolders))
          (if not-to-history
              (setq vfolders nil)))
        (setq vfolders (cdr vfolders)))
      
      
      ;; find auto-folders for matching-vfolders in order of priority
      (vm-mapc
       (lambda (vfolder)
	 (let ((auto-folder 
		(cadr (assoc vfolder vm-virtual-auto-folder-alist))))
	   (cond (auto-folder
		  (setq auto-folders 
			(cons (eval auto-folder) auto-folders)))
		 (vm-virtual-make-up-auto-folder-names
		  (setq auto-folders
			(cons vfolder auto-folders)))
		 )))
       matching-vfolders)

      ;; adjust the vm-folder-history
      (when (and (not not-to-history) auto-folders)
        (let ((folders (cdr auto-folders)) folder)
          (while folders
            (setq folder (vm-abbreviate-file-name
			  (expand-file-name (car folders) vm-folder-directory))
                  vm-folder-history (delete folder vm-folder-history)
                  vm-folder-history (nconc (list folder) vm-folder-history)
                  folders (cdr folders)))))

      ;; return the first match
      (car auto-folders))))
  
;;-----------------------------------------------------------------------------
;;;###autoload
(defvar vm-sort-compare-auto-folder-cache nil)
(add-to-list 'vm-supported-sort-keys "auto-folder")

(defun vm-sort-compare-auto-folder (m1 m2)
  (let* (s1 s2)
    (if (setq s1 (assoc m1 vm-sort-compare-auto-folder-cache))
        (setq s1 (cdr s1))
      (setq s1 (vm-virtual-auto-select-folder m1))
      (add-to-list 'vm-sort-compare-auto-folder-cache (cons m1 s1)))
    (if (setq s2 (assoc m2 vm-sort-compare-auto-folder-cache))
        (setq s2 (cdr s2))
      (setq s2 (vm-virtual-auto-select-folder m2))
      (add-to-list 'vm-sort-compare-auto-folder-cache (cons m2 s2)))
    (cond ((or (and (null s1) s2)
               (and s1 s2 (string-lessp s1 s2)))
           t)
          ((or (and (null s1) (null s2))
               (and s1 s2 (string-equal s1 s2)))
           '=)
          (t nil))))

;;;###autoload
(defun vm-sort-insert-auto-folder-names ()
  "Head each run of messages in the summary with the folder it would be filed to.
Called interactively it sorts the folder by auto-folder first, so that the
messages destined for one folder are together, and then writes that
folder\'s name above each run.  The names are display only: they are removed
and rewritten each time, and no message is changed.

Which folder a message would go to is `vm-virtual-auto-select-folder\''s
answer, from `vm-virtual-auto-folder-alist\'."
  (interactive)
  (if (vm-interactive-p)
      (vm-sort-messages "auto-folder"))
  (save-excursion
    (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
    ;; remove old descriptions
    (with-current-buffer vm-summary-buffer
      (goto-char (point-min))
      (let ((buffer-read-only nil)
            (s (point-min))
            (p (point-min)))
        (while (setq p (next-single-property-change p 'vm-auto-folder))
          (if (get-text-property (1+ p) 'vm-auto-folder)
              (setq s p)
            (delete-region s p))
          (setq p (1+ p)))))
    ;; add new descriptions
    (let ((ml vm-message-list)
          (oldf "")
          m f)
      (while ml
        (setq m (car ml)
              f (cdr (assoc m vm-sort-compare-auto-folder-cache)))
        (when (not (equal oldf f))
          (setq m (vm-su-start-of m))
          (with-current-buffer (marker-buffer m)
            (let ((buffer-read-only nil))
              (goto-char m)
              (insert (format "%s\n" (or f "no default folder")))
              (put-text-property m (point) 'vm-auto-folder t)
              (put-text-property m (point) 'face 'blue)
              ;; fix messages summary mark 
              (set-marker m (point))))
          (setq oldf f))
        (setq ml (cdr ml))))))
        
;;----------------------------------------------------------------------------
;;;###autoload
(defun vm-virtual-save-message (&optional folder count)
  "Save the current message to a mail folder.
Like `vm-save-message' but the default folder is guessed by
`vm-virtual-auto-select-folder'."
  (interactive
   (list
    ;; protect value of last-command
    (let ((last-command last-command)
          (this-command this-command))
      (vm-follow-summary-cursor)
      (let ((default (save-current-buffer
                       (vm-select-folder-buffer)
                       (or (vm-virtual-auto-select-folder)
                           vm-last-save-folder)))
            (dir (or vm-folder-directory default-directory)))
        (cond ((and default
                    (let ((default-directory dir))
                      (file-directory-p default)))
               (vm-read-file-name "Save in folder: "
                                  dir nil nil default 'vm-folder-history))
              (default
                (vm-read-file-name
                 (format "Save in folder: (default %s) " default)
                 dir default nil nil 'vm-folder-history))
              (t
               (vm-read-file-name "Save in folder: " dir nil)))))
    (prefix-numeric-value current-prefix-arg)))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-save-message folder count))

;;----------------------------------------------------------------------------
;;;###autoload
(defun vm-virtual-auto-archive-messages (&optional prompt)
  "With a prefix ARG ask user before saving." 
  (interactive "P")
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)

  (let ((auto-folder)
        (archived 0))
    (unwind-protect
	(let ((vm-message-pointer	; local copy
	       (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-operable-messages
		    0 (vm-interactive-p) "Archive")))
	      (vm-last-save-folder vm-last-save-folder) ; shadowed
	      (vm-move-after-deleting nil)		; shadowed
	      (done nil)
	      msg stop-point)
	  (setq vm-message-pointer (or vm-message-pointer vm-message-list))
	  ;; Double check if the user really wants to archive
	  (unless 
	      (or (not vm-confirm-for-auto-archive)
		  (null vm-message-pointer)
		  (not (vm-interactive-p))
		  (y-or-n-p 
		   (format "Auto archive %s messages? "
			   (if (eq last-command 'vm-next-command-uses-marks)
			       "marked" "all"))))
	    (error "Aborted"))
	  (vm-inform 5 "Archiving...")
	  ;; mark the place where we should stop.
	  (setq stop-point (vm-last vm-message-pointer))
	  (while (not done)
	    (setq msg (car vm-message-pointer))
	    (when (and (not (vm-filed-flag msg))
		       (not (vm-deleted-flag msg))
		       (setq auto-folder
			     (vm-virtual-auto-select-folder msg))
		       (not (eq (vm-get-file-buffer auto-folder)
				(current-buffer)))
		       (or (not prompt)
			   (y-or-n-p
			    (format "Save message %s in folder %s? "
				    (vm-number-of (car vm-message-pointer))
				    auto-folder))))
	      (condition-case nil
		  (let ((vm-delete-after-saving vm-delete-after-archiving)
			(last-command 'vm-virtual-auto-archive-messages))
		    (vm-save-message auto-folder 1 nil 'quiet)
		    (vm-increment archived)
		    (vm-inform 6 "%d archived, still working..." archived))
		(error nil)))
            (setq done (eq vm-message-pointer stop-point)
                  vm-message-pointer (cdr vm-message-pointer))))
      ;; unwind-protection
      ;; fix mode line
      (intern (buffer-name) vm-buffers-needing-display-update)
      (vm-update-summary-and-mode-line))
    (if (zerop archived)
        (vm-inform 5 "No messages were archived")
      (vm-inform 5 "%d message%s archived"
		 archived (if (= 1 archived) "" "s")))))

;;----------------------------------------------------------------------------
;;;###autoload
(defun vm-virtual-make-folder-persistent ()
  "Save all messages of current virtual folder in the real folder
with the same name."
  (interactive)
  (save-excursion
    (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
    (if (eq major-mode 'vm-virtual-mode)
        (let ((file (substring (buffer-name) 1 -1)))
          (vm-goto-message 0)
          (vm-save-message file (length vm-message-list))
          (vm-inform 5 "Saved virtual folder in file \"%s\"" file))
      (error "This is not a virtual folder"))))

;;----------------------------------------------------------------------------

;;; Virtual folders from BBDB, from vm-rfaddons.el (issue #606)

(declare-function bbdb-record-xfields "ext:bbdb" (record))
(declare-function bbdb-record-mail "ext:bbdb" (record))
(declare-function bbdb-split "ext:bbdb" (separator string))
(declare-function bbdb-records "ext:bbdb" ())
(declare-function bbdb-save "ext:bbdb" (&optional prompt noisy))

;;;###autoload
(defun bbdb/vm-set-virtual-folder-alist ()
  "Create a `vm-virtual-folder-alist' according to the records in the bbdb.
For each record that has a `vm-virtual' attribute, add or modify the
corresponding BBDB-VM-VIRTUAL element of the `vm-virtual-folder-alist'.

  (BBDB-VM-VIRTUAL ((vm-primary-inbox)
                    (author-or-recipient BBDB-RECORD-NET-REGEXP)))

The element gets added to the `element-name' sublist of the
`vm-virtual-folder-alist'."
  (interactive)
  (let (notes-field  email-regexp folder selector)
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-xfields record))
      (when (and (listp notes-field)
                 (setq folder (cdr (assq 'vm-virtual notes-field))))
        (setq email-regexp (mapconcat (lambda (addr)
					(regexp-quote addr))
                                      (bbdb-record-mail record) "\\|"))
        (unless (zerop (length email-regexp))
          (setq folder (or (assoc folder vm-virtual-folder-alist)
                           (car
                            (setq vm-virtual-folder-alist
                                  (nconc (list (list folder
                                                     (list (list vm-primary-inbox)
                                                           (list 'author-or-recipient))))
                                               vm-virtual-folder-alist))))
                folder (cadr folder)
                selector (assoc 'author-or-recipient folder))

          (if (cdr selector)
              (if (not (string-match (regexp-quote email-regexp)
                                     (cadr selector)))
                  (setcdr selector (list (concat (cadr selector) "\\|"
                                                 email-regexp))))
            (nconc selector (list email-regexp)))))
      )
    ))

(defun vm-virtual-find-selector (selector-spec type)
  "Return the first selector of TYPE in SELECTOR-SPEC."
  (let ((s (assoc type selector-spec)))
    (unless s
      (while (and (not s) selector-spec)
        (setq s (and (listp (car selector-spec))
                     (vm-virtual-find-selector (car selector-spec) type))
              selector-spec (cdr selector-spec))))
    s))

(defcustom bbdb/vm-virtual-folder-alist-by-mail-alias-alist nil
  "*A list of (ALIAS . FOLDER-NAME) pairs, which map an alias to a folder."
  :group 'vm-avirtual
  :type '(repeat (cons :tag "Mapping Definition"
                       (regexp :tag "Alias")
                       (string :tag "Folder Name"))))

;;;###autoload
(defun bbdb/vm-set-virtual-folder-alist-by-mail-alias ()
  "Create a `vm-virtual-folder-alist' according to the records in the bbdb.
For each record check wheather its alias is in the variable 
`bbdb/vm-virtual-folder-alist-by-mail-alias-alist' and then
add/modify the corresponding VM-VIRTUAL element of the
`vm-virtual-folder-alist'. 

  (BBDB-VM-VIRTUAL ((vm-primary-inbox)
                    (author-or-recipient BBDB-RECORD-NET-REGEXP)))

The element gets added to the `element-name' sublist of the
`vm-virtual-folder-alist'."
  (interactive)
  (let (notes-field email-regexp mail-aliases folder selector)
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-xfields record))
      (when (and (listp notes-field)
                 (setq mail-aliases (cdr (assq 'mail-alias notes-field)))
                 (setq mail-aliases (bbdb-split "," mail-aliases)))
        (setq folder nil)
        (while mail-aliases
          (setq folder
                (assoc (car mail-aliases)
                       bbdb/vm-virtual-folder-alist-by-mail-alias-alist))
          
          (when (and folder
                     (setq folder (cdr folder)
                           email-regexp (mapconcat (lambda (addr)
						     (regexp-quote addr))
                                                   (bbdb-record-mail record)
                                                   "\\|"))
                     (> (length email-regexp) 0))
            (setq folder (or (assoc folder vm-virtual-folder-alist)
                             (car
                              (setq vm-virtual-folder-alist
                                    (nconc
                                     (list
                                      (list folder
                                            (list (list vm-primary-inbox)
                                                  (list 'author-or-recipient))
                                            ))
                                     vm-virtual-folder-alist))))
                  folder (cadr folder)
                  selector (vm-virtual-find-selector folder
                                                     'author-or-recipient))
            (unless selector
              (nconc (cdr folder) (list (list 'author-or-recipient))))
            (if (cdr selector)
                (if (not (string-match (regexp-quote email-regexp)
                                       (cadr selector)))
                    (setcdr selector (list (concat (cadr selector) "\\|"
                                                   email-regexp))))
              (nconc selector (list email-regexp))))
          (setq mail-aliases (cdr mail-aliases)))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-obsolete-function-alias 'vmpc-virtual-check-selector
  'vm-pcrisis-virtual-check-selector "9.0.0")

(provide 'vm-avirtual)
;;; vm-avirtual.el ends here
