;;; jabber-xdata-form.el --- Text editor for XMPP data forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Edit staged XEP-0004 forms in a text-first submission buffer.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'string-edit)
(require 'subr-x)
(require 'jabber-util)
(require 'jabber-xdata)

(defface jabber-xdata-form-changed-value
  '((t :inherit (warning button)))
  "Face for changed XEP-0004 field values."
  :group 'jabber)

(defface jabber-xdata-form-pending-action
  '((t :inherit (warning button)))
  "Face for actions that submit changed XEP-0004 values."
  :group 'jabber)

(defvar-local jabber-xdata-form--form nil
  "Plain-data XEP-0004 form edited in the current buffer.")

(defvar-local jabber-xdata-form--original-form nil
  "Original XEP-0004 form used to detect staged changes.")

(defvar-local jabber-xdata-form--actions nil
  "Submission actions available in the current form buffer.")

(defun jabber-xdata-form-form ()
  "Return the XEP-0004 form being edited in the current buffer."
  jabber-xdata-form--form)

(defun jabber-xdata-form--option-candidates (field)
  "Return completion candidates for FIELD's advertised options."
  (mapcar (lambda (option)
            (let ((label (plist-get option :label))
                  (value (plist-get option :value)))
              (cons (if (equal label value)
                        value
                      (format "%s [%s]" label value))
                    value)))
          (plist-get field :options)))

(defun jabber-xdata-form--clear-candidate (candidates)
  "Return a clear-selection candidate distinct from CANDIDATES."
  (let ((label "Clear selection"))
    (while (assoc label candidates)
      (setq label (concat label " ")))
    (cons label :clear)))

(defun jabber-xdata-form--list-candidates (field)
  "Return completion candidates for list FIELD."
  (let ((candidates (jabber-xdata-form--option-candidates field)))
    (if (plist-get field :required)
        candidates
      (cons (jabber-xdata-form--clear-candidate candidates)
            candidates))))

(defun jabber-xdata-form--option-label (field value)
  "Return FIELD's display label for VALUE."
  (or (cl-loop for option in (plist-get field :options)
               when (equal value (plist-get option :value))
               return (plist-get option :label))
      value))

(defun jabber-xdata-form--prompt (field)
  "Return a minibuffer prompt for FIELD."
  (let ((label (or (plist-get field :label)
                   (plist-get field :var)))
        (description (plist-get field :description)))
    (if description
        (format "%s (%s): " label description)
      (format "%s: " label))))

(defun jabber-xdata-form--summary (field)
  "Return a short display summary for FIELD."
  (let ((values (plist-get field :values)))
    (pcase (plist-get field :type)
      ("boolean"
       (if (equal values '("1")) "on" "off"))
      ("text-private"
       (if (seq-some (lambda (value) (not (string-empty-p value))) values)
           "set"
         "empty"))
      ((or "list-single" "list-multi")
       (mapconcat (lambda (value)
                    (jabber-xdata-form--option-label field value))
                  values ", "))
      (_
       (mapconcat #'identity values ", ")))))

(defun jabber-xdata-form--read-list-single (field)
  "Read one advertised value for FIELD."
  (let* ((candidates (jabber-xdata-form--list-candidates field))
         (current (car (plist-get field :values)))
         (default (and current (car (rassoc current candidates))))
         (selected (completing-read
                    (jabber-xdata-form--prompt field)
                    candidates nil t nil nil default)))
    (if (eq (cdr (assoc selected candidates)) :clear)
        nil
      (list (or (cdr (assoc selected candidates)) selected)))))

(defun jabber-xdata-form--read-list-multi (field)
  "Read advertised values for FIELD."
  (let* ((candidates (jabber-xdata-form--list-candidates field))
         (defaults
          (mapcar (lambda (value)
                    (car (rassoc value candidates)))
                  (plist-get field :values)))
         (selected (jabber-completing-read-multiple
                    (jabber-xdata-form--prompt field) candidates defaults t)))
    (if (seq-some (lambda (value)
                    (eq (cdr (assoc value candidates)) :clear))
                  selected)
        nil
      (mapcar (lambda (value)
                (or (cdr (assoc value candidates)) value))
              selected))))

(defun jabber-xdata-form--read-jid-multi (field)
  "Read one or more JIDs for FIELD."
  (jabber-completing-read-multiple
   (jabber-xdata-form--prompt field)
   (mapcar #'symbol-name (jabber-concat-rosters))
   (plist-get field :values)))

(defun jabber-xdata-form--read-jid-single (field)
  "Read one JID for FIELD."
  (list
   (completing-read
    (jabber-xdata-form--prompt field)
    (mapcar #'symbol-name (jabber-concat-rosters))
    nil nil nil nil (car (plist-get field :values)))))

(defun jabber-xdata-form--read-values (field)
  "Read new values for FIELD according to its XEP-0004 type."
  (let* ((type (plist-get field :type))
         (current (car (plist-get field :values))))
    (pcase type
      ("boolean"
       (list (if (equal current "1") "0" "1")))
      ("list-single"
       (jabber-xdata-form--read-list-single field))
      ("list-multi"
       (jabber-xdata-form--read-list-multi field))
      ("jid-multi"
       (jabber-xdata-form--read-jid-multi field))
      ("jid-single"
       (jabber-xdata-form--read-jid-single field))
      ("text-private"
       (list (read-passwd (jabber-xdata-form--prompt field) nil current)))
      ("text-multi"
       (split-string (read-string-from-buffer
                      (jabber-xdata-form--prompt field)
                      (string-join (plist-get field :values) "\n"))
                     "[\n\r]" nil))
      (_
       (list (read-string (jabber-xdata-form--prompt field) current))))))

(defun jabber-xdata-form--dirty-p ()
  "Return non-nil when the current form has staged changes."
  (not (equal jabber-xdata-form--form
              jabber-xdata-form--original-form)))

(defun jabber-xdata-form--field-dirty-p (field)
  "Return non-nil when FIELD differs from its server value."
  (let ((original
         (jabber-xdata-field jabber-xdata-form--original-form
                             (plist-get field :var))))
    (not (equal (plist-get field :values)
                (plist-get original :values)))))

(defun jabber-xdata-form--field-at-point ()
  "Return the field variable at point, if any."
  (or (get-char-property (point) 'jabber-xdata-field)
      (get-char-property (line-beginning-position) 'jabber-xdata-field)))

(defun jabber-xdata-form--goto-field (var)
  "Move point to the value button for field VAR."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'jabber-xdata-field var #'equal))
              (button (next-button (prop-match-beginning match))))
    (goto-char (button-start button))))

(defun jabber-xdata-form-edit-field (&optional var)
  "Edit field VAR, or the field at point."
  (interactive)
  (let* ((var (or var (jabber-xdata-form--field-at-point)))
         (field (and var (jabber-xdata-field jabber-xdata-form--form var))))
    (unless field
      (user-error "No editable field at point"))
    (setq-local jabber-xdata-form--form
                (jabber-xdata-set-values
                 jabber-xdata-form--form var
                 (jabber-xdata-form--read-values field)))
    (jabber-xdata-form--render var)))

(defun jabber-xdata-form-reset-field (&optional var)
  "Restore field VAR, or the field at point, to its server value."
  (interactive)
  (let* ((var (or var (jabber-xdata-form--field-at-point)))
         (original
          (and var
               (jabber-xdata-field jabber-xdata-form--original-form var))))
    (unless original
      (user-error "No editable field at point"))
    (setq-local jabber-xdata-form--form
                (jabber-xdata-set-values
                 jabber-xdata-form--form var
                 (plist-get original :values)))
    (jabber-xdata-form--render var)))

(defun jabber-xdata-form-reset ()
  "Restore every field to its server-provided value."
  (interactive)
  (let ((var (jabber-xdata-form--field-at-point)))
    (setq-local jabber-xdata-form--form jabber-xdata-form--original-form)
    (jabber-xdata-form--render var)))

(defun jabber-xdata-form--edit-button (button)
  "Edit the field represented by BUTTON."
  (jabber-xdata-form-edit-field
   (button-get button 'jabber-xdata-field)))

(defun jabber-xdata-form--insert-fixed (field)
  "Insert the fixed text from FIELD."
  (when-let* ((text (string-join (plist-get field :values) "\n"))
              ((not (string-empty-p text))))
    (unless (and (> (point) (1+ (point-min)))
                 (eq (char-before) ?\n)
                 (eq (char-before (1- (point))) ?\n))
      (insert "\n"))
    (insert text "\n\n")))

(defun jabber-xdata-form--value-text (field)
  "Return FIELD's value text for display."
  (let ((summary (jabber-xdata-form--summary field)))
    (if (string-empty-p summary) "not set" summary)))

(defun jabber-xdata-form--insert-field (field)
  "Insert one editable FIELD."
  (let* ((start (point))
         (var (plist-get field :var))
         (label (or (plist-get field :label) var))
         (changed (jabber-xdata-form--field-dirty-p field)))
    (insert label)
    (when (plist-get field :required)
      (insert " *"))
    (insert ": ")
    (apply
     #'insert-text-button
     (jabber-xdata-form--value-text field)
     (append
      (list 'action #'jabber-xdata-form--edit-button
            'jabber-xdata-field var
            'follow-link t
            'help-echo "Edit this field")
      (when changed
        (list 'face 'jabber-xdata-form-changed-value))))
    (insert "\n")
    (when-let* ((description (plist-get field :description)))
      (insert (format "  %s\n" description)))
    (add-text-properties start (point)
                         (list 'jabber-xdata-field var
                               'rear-nonsticky
                               '(jabber-xdata-field)))))

(defun jabber-xdata-form--insert-fields ()
  "Insert all visible fields from the current form."
  (dolist (field (plist-get jabber-xdata-form--form :fields))
    (pcase (plist-get field :type)
      ("hidden")
      ("fixed"
       (jabber-xdata-form--insert-fixed field))
      (_
       (when (plist-get field :var)
         (jabber-xdata-form--insert-field field))))))

(defun jabber-xdata-form--action (key)
  "Return the current form action bound to KEY."
  (seq-find (lambda (action)
              (equal (plist-get action :key) key))
            jabber-xdata-form--actions))

(defun jabber-xdata-form--action-key-label (key)
  "Return a display label for action KEY."
  (pcase key
    ("RET" "C-c C-c")
    ("q" "q/C-c C-k")
    (_ key)))

(defun jabber-xdata-form--action-face (action)
  "Return the display face for ACTION."
  (and (jabber-xdata-form--dirty-p)
       (plist-get action :submits-form)
       'jabber-xdata-form-pending-action))

(defun jabber-xdata-form--activate-action (button)
  "Run the action represented by BUTTON."
  (jabber-xdata-form--run-action
   (button-get button 'jabber-xdata-action)))

(defun jabber-xdata-form--insert-action (action)
  "Insert one ACTION button."
  (let* ((key (plist-get action :key))
         (label (plist-get action :label))
         (text (format "[%s] %s"
                       (jabber-xdata-form--action-key-label key)
                       label)))
    (apply
     #'insert-text-button
     text
     (append
      (list 'action #'jabber-xdata-form--activate-action
            'jabber-xdata-action action
            'follow-link t
            'help-echo (format "Run %s" label))
      (when-let* ((face (jabber-xdata-form--action-face action)))
        (list 'face face))))))

(defun jabber-xdata-form--insert-actions ()
  "Insert the current form actions."
  (when jabber-xdata-form--actions
    (insert "\nActions\n")
    (cl-loop for action in jabber-xdata-form--actions
             for first = t then nil
             unless first do (insert "  ")
             do (jabber-xdata-form--insert-action action))
    (insert "\n")))

(defun jabber-xdata-form--header-line ()
  "Return the header line for the current form buffer."
  (concat
   (substitute-command-keys
    " Edit: \\<jabber-xdata-form-mode-map>\\[jabber-xdata-form-edit-field]")
   (if (jabber-xdata-form--action "RET")
       (substitute-command-keys
        "  Submit: \\[jabber-xdata-form-submit]")
     "  Choose an action below")
   (when (jabber-xdata-form--action "q")
     (substitute-command-keys
      "  Cancel: \\[jabber-xdata-form-cancel]"))
   (when (jabber-xdata-form--dirty-p)
     (propertize "  Pending changes" 'face 'warning))))

(defun jabber-xdata-form--render (&optional selected-var)
  "Render the current form and return to SELECTED-VAR."
  (let ((inhibit-read-only t)
        (dirty (jabber-xdata-form--dirty-p)))
    (erase-buffer)
    (insert (or (plist-get jabber-xdata-form--form :title)
                "XMPP data form")
            "\n")
    (dolist (instruction (plist-get jabber-xdata-form--form :instructions))
      (insert instruction "\n"))
    (insert "\n")
    (jabber-xdata-form--insert-fields)
    (jabber-xdata-form--insert-actions)
    (setq-local header-line-format (jabber-xdata-form--header-line))
    (set-buffer-modified-p dirty)
    (goto-char (point-min))
    (if selected-var
        (jabber-xdata-form--goto-field selected-var)
      (when-let* ((button (next-button (point-min))))
        (goto-char (button-start button))))))

(defun jabber-xdata-form-refresh ()
  "Refresh the current XEP-0004 form buffer."
  (interactive)
  (jabber-xdata-form--render (jabber-xdata-form--field-at-point)))

(defun jabber-xdata-form--ensure-complete ()
  "Signal a user error when the current form is incomplete."
  (when-let* ((missing
               (jabber-xdata--missing-required-fields
                jabber-xdata-form--form)))
    (when-let* (((derived-mode-p 'jabber-xdata-form-mode))
                (field
                 (seq-find
                  (lambda (candidate)
                    (and (plist-get candidate :var)
                         (not (equal (plist-get candidate :type) "hidden"))))
                  missing)))
      (jabber-xdata-form--goto-field (plist-get field :var)))
    (let ((labels
           (mapcar (lambda (field)
                     (or (plist-get field :label)
                         (plist-get field :var)
                         "Unnamed field"))
                   missing)))
      (user-error "Required field%s missing: %s"
                  (if (cdr labels) "s" "")
                  (string-join labels ", ")))))

(defun jabber-xdata-form--run-action (action)
  "Run ACTION from the current form buffer."
  (unless action
    (user-error "That action is not available"))
  (when (plist-get action :submits-form)
    (jabber-xdata-form--ensure-complete))
  (let ((form-buffer (current-buffer))
        (command (plist-get action :command)))
    (call-interactively command)
    (unless (plist-get action :stay-open)
      (when (buffer-live-p form-buffer)
        (with-current-buffer form-buffer
          (set-buffer-modified-p nil)
          (quit-window 'kill))))))

(defun jabber-xdata-form--run-key (key)
  "Run the current form action bound to KEY."
  (jabber-xdata-form--run-action
   (jabber-xdata-form--action key)))

(defun jabber-xdata-form-submit ()
  "Run the default submission action."
  (interactive)
  (jabber-xdata-form--run-key "RET"))

(defun jabber-xdata-form-previous ()
  "Run the previous-step action."
  (interactive)
  (jabber-xdata-form--run-key "p"))

(defun jabber-xdata-form-next ()
  "Run the next-step action."
  (interactive)
  (jabber-xdata-form--run-key "n"))

(defun jabber-xdata-form-complete ()
  "Run the completion action."
  (interactive)
  (jabber-xdata-form--run-key "c"))

(defun jabber-xdata-form-cancel ()
  "Run the cancellation action."
  (interactive)
  (jabber-xdata-form--run-key "q"))

(defvar-keymap jabber-xdata-form-mode-map
  :doc "Keymap for staged XEP-0004 forms."
  :parent special-mode-map
  "RET" #'push-button
  "e" #'jabber-xdata-form-edit-field
  "u" #'jabber-xdata-form-reset-field
  "U" #'jabber-xdata-form-reset
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "C-c C-c" #'jabber-xdata-form-submit
  "C-c C-k" #'jabber-xdata-form-cancel
  "p" #'jabber-xdata-form-previous
  "n" #'jabber-xdata-form-next
  "c" #'jabber-xdata-form-complete
  "q" #'jabber-xdata-form-cancel
  "g" #'jabber-xdata-form-refresh)

(define-derived-mode jabber-xdata-form-mode special-mode "Jabber-Form"
  "Major mode for staging and submitting an XEP-0004 form.

Edit the field at point with \\[jabber-xdata-form-edit-field].
Restore its server value with \\[jabber-xdata-form-reset-field], or
restore the whole form with \\[jabber-xdata-form-reset].
Submit with \\[jabber-xdata-form-submit] and cancel with
\\[jabber-xdata-form-cancel]."
  :interactive nil
  (setq-local truncate-lines nil))

(defun jabber-xdata-form-submit-form ()
  "Return the edited form as an XEP-0004 submission.
Signal a user error when required fields are empty."
  (jabber-xdata-form--ensure-complete)
  (jabber-xdata-submit jabber-xdata-form--form))

(defun jabber-xdata-form-open (form actions)
  "Edit plain-data XEP-0004 FORM with staged ACTIONS.
Each action is a plist with :key, :label, and :command entries."
  (let* ((title (or (plist-get form :title) "XMPP data form"))
         (buffer (generate-new-buffer (format "*Jabber form: %s*" title))))
    (with-current-buffer buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form form)
      (setq-local jabber-xdata-form--original-form form)
      (setq-local jabber-xdata-form--actions actions)
      (jabber-xdata-form--render))
    (pop-to-buffer buffer)
    (message "%s"
             (substitute-command-keys
              (concat
               "Edit with \\<jabber-xdata-form-mode-map>"
               "\\[jabber-xdata-form-edit-field], submit with "
               "\\[jabber-xdata-form-submit], or cancel with "
               "\\[jabber-xdata-form-cancel]")))
    buffer))

(provide 'jabber-xdata-form)

;;; jabber-xdata-form.el ends here
