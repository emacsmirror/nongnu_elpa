;;; hermes-chat.el --- EWOC chat buffer for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.3.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERC/emacs-jabber-shaped chat buffer for hermes-el.  Transcript entries are
;; EWOC nodes before a read-only prompt; the input tail remains writable.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'ewoc)
(require 'subr-x)
(require 'hermes-transport)

(defcustom hermes-chat-buffer-name "*Hermes Chat*"
  "Name of the Hermes chat buffer."
  :type 'string
  :group 'hermes)

(defvar-local hermes-chat--ewoc nil
  "EWOC displaying chat transcript entries in the current Hermes chat buffer.")

(defvar-local hermes-chat--input-marker nil
  "Marker at the beginning of the writable chat input tail.")

(defvar-local hermes-chat--nodes nil
  "Hash table mapping Hermes entry IDs to EWOC nodes.")

(defvar-local hermes-chat--process nil
  "Current Hermes transport process or token for this buffer.")

(defvar-local hermes-chat--session-id nil
  "Hermes session ID associated with this buffer, if known.")

(defvar-local hermes-chat--pending-assistant-id nil
  "ID of the assistant entry awaiting transport completion.")

(defvar hermes-chat--entry-counter 0
  "Counter used to generate local chat entry IDs.")

(defun hermes-chat--next-id (role)
  "Return a new local entry ID for ROLE."
  (format "%s-%d" role (cl-incf hermes-chat--entry-counter)))

(defun hermes-chat--entry-with (entry &rest props)
  "Return a copy of ENTRY with PROPS applied."
  (let ((copy (copy-sequence entry)))
    (while props
      (setq copy (plist-put copy (pop props) (pop props))))
    copy))

(defun hermes-chat--displayable-char-p (char)
  "Return non-nil if CHAR is safe to display in chat content."
  (or (memq char '(?\t ?\n))
      (and (>= char ?\s)
           (/= char ?\177)
           (not (and (>= char #x80)
                     (<= char #x9f))))))

(defun hermes-chat--sanitize-content (content)
  "Return CONTENT without carriage returns and other control bytes.
Tabs, newlines, and printable multibyte characters are preserved."
  (let ((text (or content "")))
    (unless (multibyte-string-p text)
      (setq text (decode-coding-string text 'utf-8-unix t)))
    (with-temp-buffer
      (dolist (char (string-to-list text))
        (when (hermes-chat--displayable-char-p char)
          (insert-char char)))
      (buffer-string))))

(defun hermes-chat--diff-line-kind ()
  "Return the diff kind for the current line, or nil."
  (cond
   ((looking-at "\\(?:diff --git\\|index \\|@@\\|--- \\|\\+\\+\\+ \\)") 'header)
   ((looking-at "\\+") 'added)
   ((looking-at "-") 'removed)))

(defun hermes-chat--diff-line-p ()
  "Return non-nil if the current line looks like diff content."
  (hermes-chat--diff-line-kind))

(defun hermes-chat--diff-context-line-p ()
  "Return non-nil if the current line looks like unified-diff context."
  (looking-at " "))

(defun hermes-chat--markdown-diff-ranges ()
  "Return diff-fence ranges in the current buffer as zero-based offsets."
  (let (ranges)
    (goto-char (point-min))
    (while (re-search-forward "^```[ \t]*\\(?:diff\\|patch\\)\\(?:[ \t].*\\)?\n" nil t)
      (let ((start (point)))
        (if (re-search-forward "^```[ \t]*$" nil t)
            (push (cons (1- start) (1- (match-beginning 0))) ranges)
          (push (cons (1- start) (1- (point-max))) ranges))))
    ranges))

(defun hermes-chat--inline-diff-ranges ()
  "Return inline diff-like ranges in the current buffer as zero-based offsets."
  (let (ranges)
    (goto-char (point-min))
    (while (not (eobp))
      (if (hermes-chat--diff-line-p)
          (let ((start (point))
                saw-added saw-removed saw-header)
            (while (and (not (eobp))
                        (or (hermes-chat--diff-line-p)
                            (and (or saw-added saw-removed saw-header)
                                 (hermes-chat--diff-context-line-p))))
              (pcase (hermes-chat--diff-line-kind)
                ('added (setq saw-added t))
                ('removed (setq saw-removed t))
                ('header (setq saw-header t)))
              (forward-line 1))
            (when (or saw-header (and saw-added saw-removed))
              (push (cons (1- start) (1- (point))) ranges)))
        (forward-line 1)))
    ranges))

(defun hermes-chat--merge-ranges (ranges)
  "Return RANGES sorted and merged."
  (let (merged)
    (dolist (range (sort (copy-sequence ranges)
                         (lambda (left right) (< (car left) (car right))))
                   (nreverse merged))
      (when (< (car range) (cdr range))
        (if (and merged (<= (car range) (cdar merged)))
            (setcdr (car merged) (max (cdr range) (cdar merged)))
          (push (cons (car range) (cdr range)) merged))))))

(defun hermes-chat--diff-ranges (content)
  "Return diff-like ranges in CONTENT as zero-based offsets."
  (with-temp-buffer
    (insert content)
    (hermes-chat--merge-ranges
     (append (hermes-chat--markdown-diff-ranges)
             (hermes-chat--inline-diff-ranges)))))

(defun hermes-chat--fontified-diff-string (content)
  "Return CONTENT fontified with `diff-mode'."
  (with-temp-buffer
    (insert content)
    (delay-mode-hooks (diff-mode))
    (font-lock-mode 1)
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(defun hermes-chat--fontify-diff-ranges (start content)
  "Fontify diff-like ranges from CONTENT inserted at START."
  (dolist (range (reverse (hermes-chat--diff-ranges content)))
    (let* ((begin (+ start (car range)))
           (end (+ start (cdr range)))
           (fontified (hermes-chat--fontified-diff-string
                       (buffer-substring-no-properties begin end))))
      (delete-region begin end)
      (goto-char begin)
      (insert fontified))))

(defun hermes-chat--make-entry (role content &optional status id metadata)
  "Return a chat entry plist for ROLE and CONTENT.
STATUS defaults to `done'.  ID defaults to a generated local ID.
METADATA is stored as the entry's `:metadata' plist."
  (list :id (or id (hermes-chat--next-id role))
        :role role
        :status (or status 'done)
        :content (hermes-chat--sanitize-content content)
        :created (current-time)
        :metadata metadata))

(defun hermes-chat--role-face (role)
  "Return face used to display ROLE."
  (pcase role
    ('user 'font-lock-keyword-face)
    ('assistant 'font-lock-function-name-face)
    ('tool 'font-lock-type-face)
    ('system 'shadow)
    (_ 'default)))

(defun hermes-chat--status-string (status)
  "Return display string for STATUS."
  (pcase status
    ('pending " [pending]")
    ('streaming " [streaming]")
    ('error " [error]")
    (_ "")))

(defun hermes-chat--print-entry (entry)
  "Insert a display representation of chat ENTRY at point."
  (let* ((role (plist-get entry :role))
         (status (plist-get entry :status))
         (content (or (plist-get entry :content) "")))
    (insert (propertize (capitalize (symbol-name role))
                        'face (hermes-chat--role-face role))
            (propertize (hermes-chat--status-string status) 'face 'shadow)
            ":")
    (if (string-empty-p content)
        (insert "\n")
      (insert "\n")
      (let ((content-start (point)))
        (insert content)
        (hermes-chat--fontify-diff-ranges content-start content)
        (goto-char (+ content-start (length content)))
        (insert "\n")))))

(defun hermes-chat--input-position ()
  "Return the numeric input marker position."
  (and (markerp hermes-chat--input-marker)
       (marker-position hermes-chat--input-marker)))

(defun hermes-chat--point-in-input-p ()
  "Return non-nil if point is in the writable input tail."
  (let ((pos (hermes-chat--input-position)))
    (and pos (>= (point) pos))))

(defun hermes-chat--protect-transcript ()
  "Make transcript and prompt read-only while keeping input tail writable."
  (when-let* ((pos (hermes-chat--input-position)))
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max)
                              '(read-only nil front-sticky nil rear-nonsticky nil))
      (add-text-properties (point-min) pos
                           '(read-only t front-sticky t rear-nonsticky t)))))

(defmacro hermes-chat--preserve-input-point (&rest body)
  "Run BODY preserving point's offset into the writable input tail."
  (declare (indent 0) (debug t))
  `(let ((offset (and (hermes-chat--point-in-input-p)
                      (- (point) (hermes-chat--input-position)))))
     (prog1 (progn ,@body)
       (hermes-chat--protect-transcript)
       (when offset
         (goto-char (min (point-max)
                         (+ (hermes-chat--input-position) offset)))))))

(defun hermes-chat--setup-buffer ()
  "Initialize the current buffer as an empty Hermes chat buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (setq hermes-chat--nodes (make-hash-table :test 'equal)
          hermes-chat--pending-assistant-id nil
          hermes-chat--process nil
          hermes-chat--ewoc (ewoc-create #'hermes-chat--print-entry
                                         nil "\n> " 'nosep))
    (goto-char (point-max))
    ;; Keep the marker at the beginning of the editable input tail.  With an
    ;; insertion-type marker, normal typing moves the marker after the inserted
    ;; text, making the input appear empty to `hermes-chat-input-string'.
    (setq hermes-chat--input-marker (copy-marker (point) nil))
    (hermes-chat--protect-transcript)
    (goto-char hermes-chat--input-marker)))

(defun hermes-chat--register-node (entry node)
  "Register ENTRY's ID for NODE and return NODE."
  (when-let* ((id (plist-get entry :id)))
    (puthash id node hermes-chat--nodes))
  node)

(defun hermes-chat--insert-entry (entry)
  "Insert ENTRY into the current chat EWOC and return its node."
  (hermes-chat--preserve-input-point
    (let ((node (let ((inhibit-read-only t)
                      (buffer-undo-list t))
                  (ewoc-enter-last hermes-chat--ewoc entry))))
      (hermes-chat--register-node entry node))))

(defun hermes-chat--entries ()
  "Return chat entries from the current buffer in display order."
  (let (entries)
    (ewoc-map (lambda (entry) (push entry entries)) hermes-chat--ewoc)
    (nreverse entries)))

(defun hermes-chat--update-entry (id function)
  "Update entry ID by applying FUNCTION to its entry plist."
  (let ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (unless node
      (user-error "No Hermes chat entry with id %s" id))
    (hermes-chat--preserve-input-point
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (entry (funcall function (ewoc-data node))))
        (ewoc-set-data node entry)
        (ewoc-invalidate hermes-chat--ewoc node)
        entry))))

(defun hermes-chat--append-assistant-content (assistant-id content status)
  "Append CONTENT to ASSISTANT-ID and set STATUS."
  (hermes-chat--update-entry
   assistant-id
   (lambda (entry)
     (hermes-chat--entry-with
      entry
      :status status
      :content (concat (or (plist-get entry :content) "")
                       (hermes-chat--sanitize-content content))))))

(defun hermes-chat--mark-assistant (assistant-id status &optional content)
  "Set ASSISTANT-ID to STATUS, optionally replacing CONTENT."
  (hermes-chat--update-entry
   assistant-id
   (lambda (entry)
     (if content
         (hermes-chat--entry-with entry :status status
                                  :content (hermes-chat--sanitize-content content))
       (hermes-chat--entry-with entry :status status)))))

(defun hermes-chat--handle-transport-event (assistant-id event)
  "Apply transport EVENT to ASSISTANT-ID in the current chat buffer."
  (pcase (plist-get event :type)
    ('delta
     (hermes-chat--append-assistant-content
      assistant-id (or (plist-get event :content) "") 'streaming))
    ('done
     (hermes-chat--mark-assistant assistant-id 'done)
     (setq hermes-chat--pending-assistant-id nil
           hermes-chat--process nil))
    ('error
     (hermes-chat--append-assistant-content
      assistant-id
      (let ((content (or (plist-get event :content) "")))
        (if (string-empty-p content) "Transport error" content))
      'error)
     (setq hermes-chat--pending-assistant-id nil
           hermes-chat--process nil))
    (_
     (message "Unknown Hermes transport event: %S" event))))

(defun hermes-chat-input-string ()
  "Return the current input tail as a plain string."
  (let ((pos (hermes-chat--input-position)))
    (unless pos
      (user-error "No Hermes chat input marker in this buffer"))
    (buffer-substring-no-properties pos (point-max))))

(defun hermes-chat-newline ()
  "Insert a literal newline in the Hermes chat input tail."
  (interactive)
  (unless (hermes-chat--point-in-input-p)
    (goto-char (hermes-chat--input-position)))
  (insert "\n"))

(defun hermes-chat-send ()
  "Send the current Hermes chat input."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (unless (hermes-chat--point-in-input-p)
    (user-error "Point is not in the Hermes chat input area"))
  (when hermes-chat--pending-assistant-id
    (user-error "A Hermes reply is still pending"))
  (let* ((input (hermes-chat-input-string))
         (content (string-trim input)))
    (when (string-empty-p content)
      (user-error "No Hermes input to send"))
    (delete-region (hermes-chat--input-position) (point-max))
    (let* ((user-entry (hermes-chat--make-entry 'user content 'done))
           (assistant-entry (hermes-chat--make-entry 'assistant "" 'pending))
           (assistant-id (plist-get assistant-entry :id))
           (buffer (current-buffer)))
      (hermes-chat--insert-entry user-entry)
      (hermes-chat--insert-entry assistant-entry)
      (setq hermes-chat--pending-assistant-id assistant-id)
      (condition-case err
          (setq hermes-chat--process
                (funcall hermes-transport-send-function
                         content
                         (lambda (event)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (hermes-chat--handle-transport-event
                                assistant-id event))))))
        (error
         (hermes-chat--handle-transport-event
          assistant-id
          (list :type 'error :content (error-message-string err)))
         (message "Hermes transport failed: %s" (error-message-string err)))))))

(defvar-keymap hermes-chat-mode-map
  :doc "Keymap for `hermes-chat-mode'."
  "RET" #'hermes-chat-send
  "C-j" #'hermes-chat-newline
  "S-<return>" #'hermes-chat-newline)

(define-derived-mode hermes-chat-mode fundamental-mode "Hermes Chat"
  "Major mode for Hermes chat buffers."
  :keymap hermes-chat-mode-map
  :interactive nil
  (visual-line-mode 1)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 5)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode 0))
  (hermes-chat--setup-buffer))

;;;###autoload
(defun hermes-chat ()
  "Open the Hermes chat buffer."
  (interactive)
  (let ((buffer (get-buffer-create hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'hermes-chat-mode)
        (hermes-chat-mode)))
    (pop-to-buffer-same-window buffer)
    (goto-char (or (hermes-chat--input-position) (point-max)))))

(provide 'hermes-chat)
;;; hermes-chat.el ends here
