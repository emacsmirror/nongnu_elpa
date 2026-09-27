;;; vm-grepmail.el --- VM interface for grepmail  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;; 
;; Copyright (C) 2001-2005 Robert Widhopf-Fenk
;; Copyright (C) 2024-2026 The VM Developers
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Tested with XEmacs 21.4.15 & VM 7.19
;; Keywords:    VM helpers
;; X-URL:       http://www.robf.de/Hacking/elisp

;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA. 

;;; Commentary:
;;
;; Add the following line to your .vm
;;      (require 'vm-grepmail)
;;

;;; Bugs:
;;
;; Somehow/sometimes the parsing stuff might create a corrupted folder but
;; sofar I have not been able to reproduce this problem!
;;
;; I would be thankful if you could provide me with an testing example.
;;

;;; Code:

(require 'vm-macro)
(require 'vm-misc)
(require 'vm-minibuf)
(require 'vm-startup)
(require 'vm-motion)
(require 'vm-summary)
(require 'vm-folder)
(require 'vm-window)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; group already defined in vm-vars.el

(defgroup vm-grepmail nil
  "The VM grepmail lib"
  :group 'vm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom vm-grepmail-command "grepmail"
  "*Path to the program."
  :group 'vm-grepmail
  :type 'file)

;;;###autoload
(defcustom vm-grepmail-arguments (list "-q" "-m" "-R" "-e"
                                       (format "%S" user-full-name))
  "*Arguments for grepmail program."
  :group 'vm-grepmail
  :type '(repeat (string)))

(defvar vm-grepmail-arguments-history
  nil
  "*History of previously used grepmail parameters.")

(defvar vm-grepmail-folders-history nil
  "*History for folders/directories for grepmail program.")

(defvar vm-grepmail-folder-buffer nil)

;;;###autoload
(defun vm-grepmail (arguments folders)
  "A not so excellent interface to grepmail.
Grepmail is a fast perl-script for finding mails which got lost in the
folder jungle.  End your input or folders and directories with an empty sting
or the default folder.

ARGUMENTS the command line arguments to grepmail.
FOLDERS should be a list of files/directories to search in."
  (interactive (list
                (split-string
                 (read-string "grepmail arguments: "
                              (mapconcat 'identity vm-grepmail-arguments " ")
                              'vm-grepmail-arguments-history))
                (let ((default (or vm-folder-directory
                                   "~/Mail"))
                      fd folders)
                  (while (or (not (string= fd (expand-file-name default)))
                             (string= fd ""))
                    (setq fd (vm-read-file-name
                              (format "Search folder/directory %s%s: "
                                      (if (not folders)
                                          "[end list with RET]" "")
                                      (if folders
                                          (concat "("
                                                  (mapconcat 'identity
                                                   folders ", ")
                                                  ")")
                                        ""))
                              default
                              default
                              t nil 'vm-grepmail-folders-history)
                          fd (expand-file-name fd))
                    (if (not (string= fd (expand-file-name default)))
                        (cl-pushnew fd folders :test #'equal)))
                  (if (null folders)
                      (setq folders (list fd)))
                  folders)))

  (setq vm-grepmail-arguments arguments)
  (setq vm-grepmail-folders-history
        (append folders vm-grepmail-folders-history))
  
  (let ((folder-buffer (format "* VM folder: grepmail %s %s *"
                               arguments folders))
        (process-buffer (get-buffer-create
                         (format "* grepmail %s %s *"
                                 arguments folders)))
        (vm-folder-directory (or vm-folder-directory "~/Mail"))
        process)

    (when (get-buffer folder-buffer)
      (set-buffer folder-buffer)
      (if vm-summary-buffer (kill-buffer vm-summary-buffer))
      (if vm-presentation-buffer (kill-buffer vm-presentation-buffer))
      (kill-buffer folder-buffer))
    
    (setq folder-buffer (get-buffer-create folder-buffer))
    (set-buffer folder-buffer)
    (setq default-directory (expand-file-name vm-folder-directory))
    (setq buffer-read-only nil)
    (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
    (switch-to-buffer folder-buffer)
    (set-buffer-modified-p nil)
    (vm-mode)
    (font-lock-mode -1)
    (vm-update-summary-and-mode-line)
    (vm-display (current-buffer) t
                '(vm-scroll-forward vm-scroll-backward)
                '(reading-message))
    
    (vm-summarize t t)
    (vm-display (current-buffer) nil nil '(reading-message))
    (vm-display (current-buffer) t nil '(vm-next-message reading-message))

    (with-current-buffer process-buffer
      (setq default-directory (expand-file-name vm-folder-directory))
      (erase-buffer)
      (switch-to-buffer process-buffer)
      (make-local-variable 'vm-grepmail-folder-buffer)
      (setq vm-grepmail-folder-buffer folder-buffer)

      (setq process
            (apply 'start-process-shell-command "grepmail"
                   process-buffer
                   vm-grepmail-command
                   (append arguments folders)))
      
      (if (null process)
          (error "Cannot start grepmail"))
      ;; set the send-filter
      (set-process-coding-system process 'raw-text-unix 'raw-text-unix)
      (set-process-filter process 'vm-grepmail-process-filter)
      (set-process-sentinel process 'vm-grepmail-process-done)
      process)))

(defun vm-grepmail-process-filter (process output)
  "The PROCESS insert OUTPUT into an folder biuffer."
  (condition-case nil ;err
      (progn
        (set-buffer (process-buffer process))
        (goto-char (point-max))
        (insert output)
        (let (end)
          (goto-char (1+ (point-min)))
          (when (and (string-match "^\nFrom " output)
                     (setq end (and (re-search-forward "^\nFrom "
                                                       (point-max) t)
                                    (match-beginning 0))))
            (vm-grepmail-grab-message (current-buffer) (point-min) end)
            (delete-region (point-min) end)))
        (sit-for 0))
    (error nil
           ;; TODO: there are some problems here but we ignore them
           ))
  )

(defun vm-grepmail-finished-cleanly-p (state status)
  "Whether a grepmail process in STATE with exit STATUS did its job.
It has to have exited, and exited zero.  Anything else -- a non-zero status, a
signal, a process still running -- leaves the output truncated or empty, and
what is in the buffer is not a result to go on and read."
  (and (memq state '(exit finished))
       (equal status 0)))

(defun vm-grepmail-process-done (process state)
  "Called when the grepmail PROCESS is finished returning STATE."
  (message "grepmail cleanup.")
  ;; STATE as the sentinel was given it.  Asking `process-status' again here
  ;; overwrote it, and the condition it fed was inverted besides: it raised
  ;; only where the status was zero, so a grepmail that failed or was killed
  ;; was taken for success and its truncated output read as a folder
  ;; (emacs-vm/vm#773).
  (unless (vm-grepmail-finished-cleanly-p state (process-exit-status process))
    (error "Grepmail terminated abnormally with %S %d"
           state (process-exit-status process)))

  ;; grab the last message
  (set-buffer (process-buffer process))
  (goto-char (point-max))
  (beginning-of-line)
  (vm-grepmail-grab-message (current-buffer) (point-min) (point))

  ;; cleanup
  (let ((folder-buffer vm-grepmail-folder-buffer)
	(process-buffer (process-buffer process)))
    ;; `kill-buffer' on the buffer meant, not `kill-this-buffer', which is an
    ;; interactive command that acts on the selected window's buffer.
    (kill-buffer process-buffer)
    (set-buffer folder-buffer)
    (vm-next-message 1)
    (vm-clear-modification-flag-undos)
    (set-buffer-modified-p nil)
    (setq major-mode 'vm-virtual-mode)
    (if (vm-multiple-frames-possible-p)
        (vm-set-hooks-for-frame-deletion)))
  (message "grepmail is finished."))

(defun vm-grepmail-grab-message (message-buffer start end)
  "Assimilates a message after it is complete.
MESSAGE-BUFFER is the buffer of the message.
START the start position in the process output buffer.
END the end position in the process output buffer."
  (with-current-buffer vm-grepmail-folder-buffer
    (let ((buffer-read-only nil))
      (save-restriction
       (widen)
       (goto-char (point-max))
       (insert-buffer-substring message-buffer start end)
       (cond ((eq major-mode 'vm-mode)
              (vm-clear-modification-flag-undos)))
       (vm-check-for-killed-summary)
       (vm-assimilate-new-messages)
       (vm-update-summary-and-mode-line)
       (set-buffer-modified-p nil))))
  (sit-for 0))

(provide 'vm-grepmail)
;;; vm-grepmail.el ends here
