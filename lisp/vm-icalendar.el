;;; vm-icalendar.el --- show text/calendar parts readably  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A meeting invitation arrives as a text/calendar part: an iCalendar object
;; (RFC 5545), which is legible in the way a postal form is legible.  VM used
;; to show it as it stands, all of it, folded at 75 columns:
;;
;;     BEGIN:VCALENDAR
;;     METHOD:REQUEST
;;     BEGIN:VEVENT
;;     DTSTART;TZID=Europe/London:20260810T140000
;;     ORGANIZER;CN=Alice Example:mailto:alice@example.com
;;     ...
;;
;; This shows what the invitation says -- what, when, where, who -- and leaves
;; the object itself available as an attachment.  Issue #85.
;;
;; Importing into the diary is `vm-icalendar-import', which hands the part to
;; Emacs's own icalendar.el.  Replying to an invitation (accepting or
;; declining, which means composing a METHOD:REPLY) is not done here.

;;; Code:

(require 'vm-mime)
(eval-and-compile
  (require 'vm-misc))
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function icalendar-import-buffer "icalendar"
                  (&optional diary-filename do-not-ask non-marking))
(defvar diary-file)                     ; calendar/diary-lib.el

(defcustom vm-icalendar-show-description t
  "Whether a calendar part's DESCRIPTION is shown along with the rest.
The description of an invitation is often several screens of dial-in
numbers and legal boilerplate, and the parts worth reading -- what, when,
where, who -- come before it."
  :type 'boolean
  :group 'vm-mime)

(defcustom vm-icalendar-description-lines 10
  "How many lines of a calendar part's DESCRIPTION to show, or nil for all."
  :type '(choice (const :tag "All of it" nil)
                 (integer :tag "Lines"))
  :group 'vm-mime)

(defconst vm-icalendar-method-descriptions
  '(("REQUEST" . "Invitation")
    ("REPLY" . "Reply to an invitation")
    ("CANCEL" . "Cancellation")
    ("PUBLISH" . "Announcement")
    ("ADD" . "Addition to an event")
    ("REFRESH" . "Request for the latest version")
    ("COUNTER" . "Counter-proposal")
    ("DECLINECOUNTER" . "Refusal of a counter-proposal"))
  "What each iCalendar METHOD means, in words.  RFC 5546.")

;;; Reading the object

(defun vm-icalendar-unfold (text)
  "Undo iCalendar line folding in TEXT.
A long line is split by inserting CRLF and a space or tab, and the
continuation belongs to the line before it (RFC 5545 section 3.1)."
  (replace-regexp-in-string "\r?\n[ \t]" "" text))

(defun vm-icalendar-unescape (value)
  "Undo the escaping iCalendar uses in a text value.
\\n is a line break, and comma, semicolon and backslash are escaped
because they separate things (RFC 5545 section 3.3.11)."
  (let ((out "")
        (i 0)
        (len (length value)))
    (while (< i len)
      (let ((c (aref value i)))
        (if (and (eq c ?\\) (< (1+ i) len))
            (let ((next (aref value (1+ i))))
              (setq out (concat out (cond ((memq next '(?n ?N)) "\n")
                                          (t (string next))))
                    i (+ i 2)))
          (setq out (concat out (string c))
                i (1+ i)))))
    out))

(defun vm-icalendar-parse (text)
  "Return the properties of TEXT, an iCalendar object, as an alist.
Each element is (NAME PARAMETERS . VALUE), NAME upcased, PARAMETERS the
text between the first `;' and the `:', and VALUE unescaped.  The
structure -- which property belongs to which component -- is not kept:
what this is for is showing an invitation, and an invitation is one
VEVENT."
  (let ((properties nil))
    (dolist (line (split-string (vm-icalendar-unfold text) "\r?\n" t))
      (when (string-match "\\`\\([A-Za-z0-9-]+\\)\\([^:]*\\):\\(.*\\)\\'" line)
        (push (cons (upcase (match-string 1 line))
                    (cons (match-string 2 line)
                          (vm-icalendar-unescape (match-string 3 line))))
              properties)))
    (nreverse properties)))

(defun vm-icalendar-value (properties name)
  "The value of property NAME in PROPERTIES, or nil."
  (cddr (assoc name properties)))

(defun vm-icalendar-values (properties name)
  "Every value of property NAME in PROPERTIES, in order."
  (let ((found nil))
    (dolist (p properties)
      (when (equal (car p) name)
        (push (cddr p) found)))
    (nreverse found)))

(defun vm-icalendar-parameters (properties name)
  "The parameter text of the first property NAME in PROPERTIES."
  (cadr (assoc name properties)))

;;; Saying it in words

(defun vm-icalendar-common-name (parameters value)
  "A person's name and address, from a CAL-ADDRESS property.
PARAMETERS is the property's parameter text, which may hold CN=; VALUE is
usually `mailto:someone@example.com'."
  (let ((address (replace-regexp-in-string "\\`[Mm][Aa][Ii][Ll][Tt][Oo]:" ""
                                           (or value "")))
        (name (and parameters
                   (string-match "CN=\\(\"[^\"]*\"\\|[^;:]*\\)" parameters)
                   (replace-regexp-in-string
                    "\\`\"\\|\"\\'" "" (match-string 1 parameters)))))
    (cond ((and name (not (string-empty-p name)) (not (equal name address)))
           (format "%s <%s>" name address))
          (t address))))

(defun vm-icalendar-time (value)
  "VALUE, an iCalendar date or date-time, in words.
A date-time is UTC when it ends in Z, and otherwise floating or in the
zone its TZID names -- which is not resolved here: showing 14:00 as the
sender wrote it is better than showing it in the wrong zone."
  (cond ((null value) nil)
        ((string-match
          "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\
\\(?:T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(Z\\)?\\)?\\'"
          value)
         (let* ((year (string-to-number (match-string 1 value)))
                (month (string-to-number (match-string 2 value)))
                (day (string-to-number (match-string 3 value)))
                (hour (match-string 4 value))
                (minute (match-string 5 value))
                (utc (match-string 7 value))
                (encoded (encode-time 0 0 12 day month year)))
           (concat (format-time-string "%A %e %B %Y" encoded)
                   (when hour (format " at %s:%s%s" hour minute
                                      (if utc " UTC" ""))))))
        (t value)))

(defun vm-icalendar-attendee-line (properties)
  "The attendees named in PROPERTIES, one per line, with their answers."
  (let ((lines nil))
    (dolist (p properties)
      (when (equal (car p) "ATTENDEE")
        (let* ((parameters (cadr p))
               (who (vm-icalendar-common-name parameters (cddr p)))
               (status (and parameters
                            (string-match "PARTSTAT=\\([A-Za-z-]+\\)" parameters)
                            (match-string 1 parameters))))
          (push (concat "  " who
                        (pcase status
                          ("ACCEPTED" " -- accepted")
                          ("DECLINED" " -- declined")
                          ("TENTATIVE" " -- tentatively")
                          ("NEEDS-ACTION" " -- not answered")
                          (_ "")))
                lines))))
    (nreverse lines)))

(defun vm-icalendar-description (properties)
  "The DESCRIPTION in PROPERTIES, trimmed to `vm-icalendar-description-lines'."
  (let ((text (vm-icalendar-value properties "DESCRIPTION")))
    (when (and text vm-icalendar-show-description
               (not (string-empty-p (string-trim text))))
      (let* ((lines (split-string (string-trim text) "\n"))
             (limit vm-icalendar-description-lines)
             (shown (if (and limit (> (length lines) limit))
                        (append (seq-take lines limit)
                                (list (format "  [%d more lines; the part itself has all of it]"
                                              (- (length lines) limit))))
                      lines)))
        (mapconcat #'identity shown "\n")))))

(defun vm-icalendar-format (text)
  "TEXT, an iCalendar object, in words."
  (let* ((properties (vm-icalendar-parse text))
         (method (vm-icalendar-value properties "METHOD"))
         (summary (vm-icalendar-value properties "SUMMARY"))
         (start (vm-icalendar-time (vm-icalendar-value properties "DTSTART")))
         (end (vm-icalendar-time (vm-icalendar-value properties "DTEND")))
         (location (vm-icalendar-value properties "LOCATION"))
         (organizer (when (assoc "ORGANIZER" properties)
                      (vm-icalendar-common-name
                       (vm-icalendar-parameters properties "ORGANIZER")
                       (vm-icalendar-value properties "ORGANIZER"))))
         (recurrence (vm-icalendar-value properties "RRULE"))
         (attendees (vm-icalendar-attendee-line properties))
         (description (vm-icalendar-description properties))
         (lines nil))
    (push (or (cdr (assoc method vm-icalendar-method-descriptions))
              (if method (format "Calendar message (%s)" method) "Calendar message"))
          lines)
    (when summary (push (format "  %s" summary) lines))
    (when start (push (format "  When: %s%s" start
                              (if end (format " until %s" end) ""))
                      lines))
    (when recurrence (push (format "  Repeats: %s" recurrence) lines))
    (when location (push (format "  Where: %s" location) lines))
    (when organizer (push (format "  Organizer: %s" organizer) lines))
    (when attendees
      (push "  Attendees:" lines)
      (dolist (a attendees) (push a lines)))
    (when description
      (push "" lines)
      (push description lines))
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

;;; Showing it

(defun vm-icalendar-part-text (layout)
  "The decoded text of LAYOUT, a text/calendar part."
  (let ((beg (vm-mm-layout-body-start layout))
        (end (vm-mm-layout-body-end layout))
        (buffer (generate-new-buffer " *vm-icalendar*")))
    (unwind-protect
        (let ((raw (with-current-buffer (if (markerp beg)
                                            (marker-buffer beg)
                                          (current-buffer))
                     (save-restriction
                       (widen)
                       (buffer-substring beg end)))))
          (with-current-buffer buffer
            (insert raw)
            (vm-mime-transfer-decode-region layout (point-min) (point-max))
            (buffer-substring (point-min) (point-max))))
      (kill-buffer buffer))))

;;;###autoload
(defun vm-mime-display-internal-text/calendar (layout)
  "Show a text/calendar part as what it says rather than as what it is.
Issue #85."
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (insert (condition-case err
                (vm-icalendar-format (vm-icalendar-part-text layout))
              (error
               ;; Better the object than an error where the invitation was.
               (format "Calendar part (could not be read: %s)\n%s"
                       (error-message-string err)
                       (vm-icalendar-part-text layout))))))
  t)

;;;###autoload
(defun vm-mime-display-internal-text/x-vcalendar (layout)
  "Older senders call it text/x-vcalendar."
  (vm-mime-display-internal-text/calendar layout))

;;;###autoload
(defun vm-icalendar-import (&optional file)
  "Add the calendar part of the current message to the diary.
Hands the part to Emacs's own `icalendar-import-buffer', which writes into
FILE, `diary-file' by default.  Issue #85."
  (interactive)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-icalendar-import-message (vm-real-message-of (car vm-message-pointer))
                               file))

(defun vm-icalendar-import-message (message &optional file)
  "Add MESSAGE's calendar part to the diary.
The command is `vm-icalendar-import'; this is the part of it that takes a
message, so that it can be called with one."
  (let* ((layout (vm-icalendar-part-of (vm-mm-layout message))))
    (unless layout
      (error "This message has no calendar part"))
    (require 'icalendar)
    (let ((buffer (generate-new-buffer " *vm-icalendar-import*")))
      (unwind-protect
          (with-current-buffer buffer
            (insert (vm-icalendar-part-text layout))
            (vm-icalendar-import-into-diary
             (or file (bound-and-true-p diary-file)))
            (vm-inform 5 "Calendar entry added to the diary"))
        (kill-buffer buffer)))))

(defun vm-icalendar-import-into-diary (diary)
  "Import this buffer's iCalendar text into the diary file DIARY.
Raises where it could not, so a caller that reaches the next line has had
the entry filed.

`diary-icalendar-import-buffer' where Emacs has it, and
`icalendar-import-buffer' where it has not.  The new name arrived in Emacs
31.1, which is the release that obsoleted the old one, and VM runs on 28.1,
so both are needed and the new one is asked for first (emacs-vm/vm#858).

They do not answer alike, which is why neither is simply called in the
other's place.  The old one answers t when it imported and nil when it did
not.  The new one ends in `save-buffer' and answers nil whatever happened,
and raises when it cannot read the text; that is all there is to go on, and
it is enough.  It also shows the diary buffer, which VM does not want over
a folder, so nothing is displayed for the length of the call."
  (if (fboundp 'diary-icalendar-import-buffer)
      (let ((display-buffer-alist
             (cons '("\\`" (display-buffer-no-window) (allow-no-window . t))
                   display-buffer-alist)))
        (diary-icalendar-import-buffer diary t))
    (unless (with-suppressed-warnings ((obsolete icalendar-import-buffer))
              (icalendar-import-buffer diary t))
      (error "icalendar could not import this part"))))

(defun vm-icalendar-part-of (layout)
  "The first text/calendar part of LAYOUT, at any depth, or nil."
  (when (vectorp layout)
    (if (or (vm-mime-types-match "text/calendar" (car (vm-mm-layout-type layout)))
            (vm-mime-types-match "text/x-vcalendar"
                                 (car (vm-mm-layout-type layout))))
        layout
      (let ((parts (vm-mm-layout-parts layout))
            (found nil))
        (while (and parts (not found))
          (setq found (vm-icalendar-part-of (car parts))
                parts (cdr parts)))
        found))))

(provide 'vm-icalendar)

;;; vm-icalendar.el ends here
