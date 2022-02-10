;;; why-this.el --- Show why the current line contains this -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Version: 1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: tools, convenience, vc
;; URL: https://codeberg.org/akib/emacs-why-this

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; why-this shows when and why the current line was last changed.  Enable
;; with M-x why-this-mode.  Or see why a line was change without enabling
;; with M-x why-this.  See editing history of whole file with heat map with
;; M-x why-this-annotate.

;;; Code:

(require 'subr-x)
(require 'timezone)
(require 'color)

(defgroup why-this nil
  "Show why the current line contains this."
  :group 'tools
  :link '(url-link "https://codeberg.org/akib/emacs-why-this")
  :prefix "why-this-")

(defcustom why-this-backends '(why-this-git
                               why-this-hg)
  "List of enabled backends.

Each backend is a function taking variable number of arguments, where the
first argument is the command (which is a symbol):

`supported-p'
  The backend should return whether the current buffer is supported by it.

`line-data'
  The backend should return a list of plist containing data about lines
  from BEGIN to END (exclusive), where BEGIN is the second argument and END
  is the third argument.  Each plist should describe BEGIN+Nth line, where
  N is the index of the plist in list.  Plist should contain the following
  properties:

    `:id'       Unique ID for each change (or commit).
    `:author'   Name of the author.
    `:time'     Time of change (local).
    `:desc'     Single line description of change."
  :type '(repeat (function :tag "Backend"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-message-time-format #'why-this-relative-time
  "Format string or formatter function for time in message.

See `format-time-string' for the syntax of format string.

When the value is a function it should take the time as the first argument
and return a string."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-message-format "     %A, %t * %i"
  "Format string or formatter function for formatting message.

All characters are written as is, except certain constructs which are
substituted by text describing the author, time or message:

  %a    Author name, as returned by the backend.
  %A    Author's nick name.
  %t    Time when last changed, formatted with
        `why-this-message-time-format'.
  %i    Description.

The value can also be a function to do the formatting itself.  The function
should take a plist as the first and only argument.  The plist is same as
the plists returned by backends when `line-data' command is given (see
`why-this-backends'), with an additional property `:backend' whose value is
is the backend which generated the plist."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-minimum-column 55
  "Minimum column for displaying message.

Messages are never shown before this column.  Set to 0 to disable."
  :type 'integer
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-echo-time-format #'why-this-relative-time
  "Format string or formatter function for time in echo area message.

See `format-time-string' for the syntax of format string.

When the value is a function it should take the time as the first argument
and return a string."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-echo-format "%A, %t * %i"
  "Format string or formatter function for formatting echo area message.

See `why-this-message-format' for possible values.
`why-this-echo-time-format' is used to format time."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-enable-tooltip t
  "Non-nil means show tooltip."
  :type 'boolean
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-nick-name-alist nil
  "Alist of nick name of authors.

Each element is of the following form: (NICK . AUTHORS), where NICK is the
nick name and AUTHORS is list of the name of authors corresponding to
NICK."
  :type '(repeat (cons (string :tag "Nick")
                       (repeat (string :tag "Author"))))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-idle-delay 0.5
  "Idle delay for rendering in seconds."
  :type 'number
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-width 70
  "Width of annotation done by `why-this-annotate'."
  :type 'integer
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-author-format "%s"
  "Format string or formatter function for author name in annotation.

See `format' for the syntax of format string.

When the value is a function it should take the author name as the first
argument and return a string to show."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-description-format "%s"
  "Format string or formatter function for description in annotation.

See `format' for the syntax of format string.

When the value is a function it should take the description of change as
the first argument and return a string to show."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-time-format #'why-this-relative-time
  "Format string or formatter function for time in annotation.

See `format-time-string' for the syntax of format string.

When the value is a function it should take the time as the first argument
and return a string."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-author-length 20
  "Length of author name in annotation done by `why-this-annotate'."
  :type 'integer
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-separator " \x2502 "
  "Separator between annotation and file contents."
  :type 'string
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-enable-heat-map t
  "Non-nil means show heat map in annotation buffer."
  :type 'boolean
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-heat-map-cold "#dde3f4"
  "Cold background for heat map in annotation buffer."
  :type 'color
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-annotate-heat-map-warm "#f0e0d4"
  "Warm background for heat map in annotation buffer."
  :type 'color
  :package-version '(why-this "1.0")
  :group 'why-this)

(defcustom why-this-calculate-background t
  "Non-nil means calculate background for message."
  :type 'boolean
  :package-version '(why-this "1.0")
  :group 'why-this)

(defface why-this-face
  '((t :foreground "#82b0ec"
       :italic t))
  "Face for Why-This data."
  :package-version '(why-this "1.0")
  :group 'why-this)

(defvar why-this--git-author-name (string-trim
                                   (shell-command-to-string
                                    "git config --get user.name"))
  "Name of author.")

(defvar why-this--overlays nil
  "Overlays created by Why-This.")

(defvar why-this--idle-timer nil
  "Timer for rendering.")

(defvar why-this--buffer-count 0
  "Count of buffers with Why-This mode enabled.")

(defvar-local why-this--backend nil
  "Backend for current buffer.")

(defun why-this-nick-name (author)
  "Return nick name of AUTHOR."
  (catch 'name
    (dolist (nick why-this-nick-name-alist)
      (when (member author (cdr nick))
        (throw 'name (car nick))))
    author))

(defun why-this-relative-time (time &optional exact)
  "Format TIME as a relative age.

When EXACT is non-nil, be as exact as possible."
  (let* ((elapsed (max (floor (float-time (time-since time))) 0))
         str
         (calc-time
          (lambda (type length)
            (let ((count (/ elapsed length)))
              (setq str (concat str
                                (unless (zerop count)
                                  (if (and (not exact)
                                           (eq count 1))
                                      (if (string= type "day")
                                          "Yesterday"
                                        (format "A%s %s "
                                                (if (string= type "hour")
                                                    "n" "")
                                                type))
                                    (format "%i %ss " count type))))
                    elapsed (% elapsed length))))))
    (if (zerop elapsed)
        "Now"
      (funcall calc-time "year" (floor (* 365.25 24 3600)))
      (when (or exact (zerop (length str)))
        (funcall calc-time "month" (* 30 24 3600))
        (when (or exact (zerop (length str)))
          (funcall calc-time "week" (* 7 24 3600))
          (when (or exact (zerop (length str)))
            (funcall calc-time "day" (* 24 3600))
            (when (or exact (zerop (length str)))
              (funcall calc-time "hour" 3600)
              (when (or exact (zerop (length str)))
                (funcall calc-time "minute" 60)
                (when (or exact (zerop (length str)))
                  (funcall calc-time "second" 1)))))))
      (if (and (not exact) (string= str "Yesterday"))
          str
        (concat str "ago")))))

(defun why-this-format-time (format time)
  "Format TIME using FORMAT."
  (if (functionp format)
      (funcall format time)
    (format-time-string format time)))

(defun why-this-format-data (format time-format data)
  "Format DATA using FORMAT and TIME-FORMAT.

TIME-FORMAT is used to format data."
  (if (functionp format)
      (funcall format data)
    (let ((alist `((?a . (plist-get data :author))
                   (?A . (why-this-nick-name (plist-get data :author)))
                   (?t . (why-this-format-time time-format
                                               (plist-get data :time)))
                   (?i . (plist-get data :desc)))))
      (replace-regexp-in-string
       "%."
       (lambda (str)
         (let ((char (aref str 1)))
           (if (eq char ?%)
               "%"
             (let ((sexp (cdr (assoc char alist))))
               (if sexp
                   (eval sexp `((data . ,data)
                                (time-format . ,time-format)))
                 str)))))
       format t t))))

(defun why-this--overlay-bg-type (pos)
  "Return the background type for overlay at POS."
  (cond
   ((and (use-region-p)
         (>= pos (region-beginning))
         (< pos (region-end)))
    'region)
   ((eq (line-number-at-pos)
        (line-number-at-pos pos))
    'line)
   (t
    nil)))

(defun why-this--get-face (type)
  "Return face for showing message with background type TYPE."
  (if (not why-this-calculate-background)
      'why-this-face
    `(:background
      ,(face-background
        (pcase type
          ('region
           (if (bound-and-true-p solaire-mode)
               'solaire-region-face
             'region))
          ('line
           (if (bound-and-true-p hl-line-mode)
               (if (bound-and-true-p solaire-mode)
                   'solaire-hl-line-face
                 'hl-line)
             'why-this-face))
          (_
           'why-this-face))
        nil t)
      :inherit why-this-face)))

(defvar why-this-mode)

(defun why-this--render ()
  "Render overlays."
  (while why-this--overlays
    (delete-overlay (car (pop why-this--overlays))))
  (when why-this-mode
    (let* ((begin (line-number-at-pos (if (use-region-p)
                                          (region-beginning)
                                        (point))))
           (end (1+ (line-number-at-pos (if (use-region-p)
                                            (region-end)
                                          (point)))))
           (backend why-this--backend)
           (data (funcall backend 'line-data begin end)))
      (dolist (i (number-sequence 0 (- end begin 1)))
        (let (line-begin
              line-end)
          (save-excursion
            (goto-char (point-min))
            (setq line-begin (line-beginning-position (+ begin i)))
            (setq line-end (line-end-position (+ begin i))))
          (let ((ov (make-overlay line-end line-end))
                (type (why-this--overlay-bg-type line-end))
                (column (- line-end line-begin)))
            (overlay-put ov 'why-this-message (why-this-format-data
                                               why-this-message-format
                                               why-this-message-time-format
                                               (append `(:backend ,backend)
                                                       (nth i data))))
            (overlay-put ov 'why-this-props
                         (list 'cursor t
                               'face (why-this--get-face type)
                               'help-echo
                               (when why-this-enable-tooltip
                                 (why-this-format-data
                                  why-this-echo-format
                                  why-this-echo-time-format
                                  (append `(:backend ,backend)
                                          (nth i data))))))
            (overlay-put ov 'after-string
                         (apply
                          #'propertize
                          (concat
                           (make-string (max (- why-this-minimum-column
                                                column)
                                             0)
                                        ? )
                           (overlay-get ov 'why-this-message))
                          (overlay-get ov 'why-this-props)))
            (overlay-put ov 'why-this-column column)
            (overlay-put ov 'why-this-line (+ begin i))
            (overlay-put ov 'why-this-bg-type type)
            (push (cons ov (current-buffer)) why-this--overlays)))))))

(defun why-this--render-non-blocking ()
  "Render overlays, but don't block Emacs."
  (while-no-input
    (why-this--render)))

(defun why-this--update-overlays ()
  "Update all overlays."
  (let ((begin (line-number-at-pos (if (use-region-p)
                                       (region-beginning)
                                     (point))))
        (end (1+ (line-number-at-pos (if (use-region-p)
                                         (region-end)
                                       (point))))))
    (setq
     why-this--overlays
     (delq
      nil
      (mapcar
       (lambda (ov)
         (if (and (eq (cdr ov) (current-buffer))
                  (let ((line (line-number-at-pos
                               (overlay-start (car ov)))))
                    (and (>= line begin)
                         (< line end)
                         (eq line (overlay-get (car ov) 'why-this-line)))))
             (progn
               (let* ((ov-start (overlay-start (car ov)))
                      line-begin
                      line-end
                      column)
                 (save-excursion
                   (goto-char ov-start)
                   (setq line-begin (line-beginning-position))
                   (setq line-end (line-end-position))
                   (setq column (- line-end line-begin)))
                 (unless (eq ov-start line-end)
                   (move-overlay (car ov) line-end line-end))
                 (unless (eq (overlay-get (car ov) 'why-this-column)
                             column)
                   (overlay-put (car ov) 'after-string
                                (apply
                                 #'propertize
                                 (concat
                                  (make-string
                                   (max (- why-this-minimum-column column)
                                        0)
                                   ? )
                                  (overlay-get (car ov) 'why-this-message))
                                 (overlay-get (car ov) 'why-this-props)))
                   (overlay-put (car ov) 'why-this-column column)))
               (when why-this-calculate-background
                 (let ((type (why-this--overlay-bg-type
                              (overlay-start (car ov)))))
                   (unless (eq (overlay-get (car ov) 'why-this-bg-type)
                               type)
                     (overlay-put (car ov) 'why-this-props
                                  (plist-put (overlay-get (car ov)
                                                          'why-this-props)
                                             'face (why-this--get-face
                                                    type)))
                     (overlay-put (car ov) 'after-string
                                  (propertize
                                   (overlay-get (car ov) 'after-string)
                                   'face (why-this--get-face type)))
                     (overlay-put (car ov) 'why-this-bg-type type))))
               ov)
           (delete-overlay (car ov))
           nil))
       why-this--overlays)))))

;;;###autoload
(defun why-this-supported-p ()
  "Return non-nil if the Why-This mode is support in current buffer.

Actually the supported backend is returned."
  (catch 'yes
    (dolist (backend why-this-backends)
      (let ((result (funcall backend 'supported-p)))
        (when result
          (throw 'yes backend))))
    nil))

(defun why-this--insert-and-truncate (str len)
  "Insert and truncate STR to LEN using overlay."
  (if (<= (length str) len)
      (insert str)
    (let* ((visible-len (- len 3))
           (visible (substring str 0 visible-len))
           (invisible (substring str visible-len)))
      (insert (propertize visible 'help-echo str))
      (let ((point (point))
            ov)
        (insert invisible)
        (setq ov (make-overlay point (point)))
        (overlay-put ov 'invisible 'ellipsis)
        (overlay-put ov 'isearch-open-invisible #'delete-overlay)))))

(defun why-this--mix-colors (a b ratio)
  "Mix A and B by RATIO."
  (let* ((a-color (color-name-to-rgb a))
         (b-color (color-name-to-rgb b))
         (mix (lambda (i)
                (+ (nth i a-color)
                   (* (- (nth i b-color)
                         (nth i a-color))
                      ratio)))))
    (color-rgb-to-hex (funcall mix 0)
                      (funcall mix 1)
                      (funcall mix 2)
                      2)))

;;;###autoload
(defun why-this ()
  "Show why the current line contains this."
  (interactive)
  (let ((backend (why-this-supported-p)))
    (if backend
        (message "%s" (why-this-format-data
                       why-this-echo-format
                       why-this-echo-time-format
                       (append
                        `(:backend ,backend)
                        (car (funcall backend 'line-data
                                      (line-number-at-pos)
                                      (1+ (line-number-at-pos)))))))
      (user-error "No backend"))))

;;;###autoload
(defun why-this-annotate ()
  "Annotate current buffer with editing history."
  (interactive)
  (let ((backend (why-this-supported-p)))
    (if (not backend)
        (user-error "No backend")
      (save-excursion
        (font-lock-fontify-region (point-min) (point-max)))
      (let* ((line-count (line-number-at-pos (1- (point-max))))
             (data (funcall backend 'line-data 1 (1+ line-count)))
             (contents (split-string (buffer-substring (point-min)
                                                       (point-max))
                                     "\n"))
             (i 0)
             (change-times (mapcar
                            (lambda (line)
                              (float-time (plist-get line :time)))
                            data))
             (newest-change (apply #'max change-times))
             (oldest-change (apply #'min change-times))
             (last-change-begin 0)
             (add-heat
              (lambda ()
                (let (ov)
                  (setq ov (make-overlay last-change-begin (point)))
                  (overlay-put ov 'face
                               `(:background
                                 ,(why-this--mix-colors
                                   why-this-annotate-heat-map-cold
                                   why-this-annotate-heat-map-warm
                                   (if (equal newest-change
                                              oldest-change)
                                       0.5
                                     (/ (- (float-time
                                            (plist-get (nth (1- i) data)
                                                       :time))
                                           oldest-change)
                                        (- newest-change
                                           oldest-change))))
                                 :extend t))))))
        (with-current-buffer (get-buffer-create
                              (format "*why-this-annotate %s*"
                                      (buffer-name)))
          (why-this-annotate-mode)
          (setq buffer-read-only nil)
          (erase-buffer)
          (dolist (line data)
            (if (and (not (zerop i))
                     (equal (plist-get line :id)
                            (plist-get (nth (1- i) data) :id)))
                (insert
                 (format (format "%%%is" why-this-annotate-width) "")
                 why-this-annotate-separator
                 (format (format "%%%ii" (length (number-to-string
                                                  line-count)))
                         (1+ i))
                 " "
                 (nth i contents)
                 "\n")
              (unless (zerop i)
                (let (ov)
                  (setq ov (make-overlay (line-beginning-position 0)
                                         (point)))
                  (overlay-put ov 'face `(:underline
                                          ,(face-foreground 'default)
                                          :extend t)))
                (when why-this-annotate-enable-heat-map
                  (funcall add-heat)))
              (setq last-change-begin (point))
              (let* ((time (why-this-format-time
                            why-this-annotate-time-format
                            (plist-get line :time)))
                     (author (format
                              (format "%%-%is"
                                      why-this-annotate-author-length)
                              (format why-this-annotate-author-format
                                      (plist-get line :author))))
                     (desc-length (- why-this-annotate-width
                                     why-this-annotate-author-length
                                     (length time) 4))
                     (desc (format
                            (format "%%-%is" desc-length)
                            (format why-this-annotate-description-format
                                    (plist-get line :desc)))))
                (why-this--insert-and-truncate
                 author why-this-annotate-author-length)
                (insert "  ")
                (why-this--insert-and-truncate desc desc-length)
                (insert
                 "  "
                 time
                 why-this-annotate-separator
                 (format (format "%%%ii" (length (number-to-string
                                                  line-count)))
                         (1+ i))
                 " "
                 (nth i contents)
                 "\n")))
            (setq i (1+ i)))
          (when why-this-annotate-enable-heat-map
            (funcall add-heat))
          (setq buffer-read-only t)
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

;;;###autoload
(define-minor-mode why-this-mode
  "Toggle showing why the current line was changed."
  nil " Why-This" nil
  :group 'why-this
  (setq why-this--backend (why-this-supported-p))
  (if (not why-this--backend)
      (setq why-this-mode nil)
    (if why-this-mode
        (progn
          (add-hook 'post-command-hook #'why-this--update-overlays nil t)
          (when why-this--idle-timer
            (cancel-timer why-this--idle-timer)
            (setq why-this--idle-timer nil))
          (setq why-this--idle-timer
                (run-with-idle-timer why-this-idle-delay t
                                     #'why-this--render-non-blocking))
          (setq why-this--buffer-count (1+ why-this--buffer-count)))
      (remove-hook 'post-command-hook #'why-this--update-overlays t)
      (setq why-this--buffer-count (1- why-this--buffer-count))
      (when (zerop why-this--buffer-count)
        (cancel-timer why-this--idle-timer)
        (setq why-this--idle-timer nil)))))

;;;###autoload
(define-globalized-minor-mode global-why-this-mode why-this-mode
  why-this-mode
  :group 'why-this)

(define-derived-mode why-this-annotate-mode
  special-mode "Why-This-Annotate"
  "Major mode for output buffer of `why-this-annotate'."
  :group 'why-this
  (add-to-invisibility-spec '(ellipsis . t)))

(defun why-this-git (cmd &rest args)
  "Git backend for Why-This mode.

Do CMD with ARGS."
  (pcase cmd
    ('supported-p
     (and (buffer-file-name)
          (string= "true\n" (shell-command-to-string
                             "git rev-parse --is-inside-work-tree"))))
    ('line-data
     (when (> (- (nth 1 args) (nth 0 args)) 0)
       (let* ((temp-file (let ((file (make-temp-file "why-this-git-"))
                               (text (buffer-substring-no-properties
                                      (point-min) (point-max))))
                           (with-temp-file file
                             (insert text))
                           file))
              (command (format (concat
                                "git blame -L %i,%i \"%s\" --porcelain"
                                " --contents \"%s\" ; echo $?")
                               (nth 0 args) (1- (nth 1 args))
                               (buffer-file-name) temp-file))
              (blame (butlast
                      (split-string (shell-command-to-string command)
                       "\n")))
              (status (string-to-number (car (last blame))))
              line-data
              (i 0)
              (uncommitted-commit-hash
               "0000000000000000000000000000000000000000")
              (add-uncommitted
               (lambda ()
                 (setq line-data
                       (append line-data
                               (list
                                (list
                                 :id uncommitted-commit-hash
                                 :author why-this--git-author-name
                                 :time (current-time)
                                 :desc "Uncommitted changes")))))))
         (delete-file temp-file)
         (setq blame (butlast blame))
         (when (zerop status)
           (let (commit-alist)
             (while (< i (length blame))
               (let* ((commit (car (split-string (nth i blame))))
                      (data (assoc commit commit-alist)))
                 (setq i (1+ i))
                 (unless data
                   (while (not (eq (aref (nth i blame) 0) ?\t))
                     (unless (equal commit uncommitted-commit-hash)
                       (let ((line (nth i blame)))
                         (string-match split-string-default-separators
                                       line)
                         (push (cons (substring line 0 (match-beginning 0))
                                     (substring line (match-end 0)))
                               data)))
                     (setq i (1+ i)))
                   (push (cons commit data) commit-alist))
                 (while (not (eq (aref (nth i blame) 0) ?\t))
                   (setq i (1+ i)))
                 (if (equal commit uncommitted-commit-hash)
                     (funcall add-uncommitted)
                   (setq
                    line-data
                    (append
                     line-data
                     (list
                      (list
                       :id commit
                       :author (cdr (assoc-string "author" data))
                       :time (time-convert
                              (string-to-number
                               (cdr (assoc-string "author-time" data))))
                       :desc (cdr (assoc-string "summary" data)))))))
                 (setq i (1+ i))))))
         (while (< (length line-data) (- (nth 1 args) (nth 0 args)))
           (funcall add-uncommitted))
         line-data)))))

(defun why-this-hg (cmd &rest args)
  "Mercurial backend for Why-This mode.

Do CMD with ARGS."
  (pcase cmd
    ('supported-p
     (and (buffer-file-name)
          (string= "t" (shell-command-to-string
                        (format "hg annotate \"%s\" --template \"t\""
                                (buffer-file-name))))))
    ('line-data
     (when (> (- (nth 1 args) (nth 0 args)) 0)
       (let ((output
              (car
               (read-from-string
                (shell-command-to-string
                 (format
                  (concat
                   "hg annotate \"%s\" --template \"({lines %% '(:id"
                   " {rev} :author \\\"{person(user)}\\\" :time"
                   " ({hgdate(date)}) :desc \\\"{sub(\\\"\\\\\\\"\\\","
                   " \\\"\\\\\\\\\\\\\\\"\\\", sub(\\\"\\n.*\\\","
                   " \\\"\\\", desc))}\\\") '})\"")
                  (buffer-file-name))))))
             data)
         (dolist (i (number-sequence (1- (nth 1 args)) (nth 0 args) -1))
           (if (<= i (length output))
               (let ((plist (nth (1- i) output)))
                 (setq plist
                       (plist-put plist :time
                                  (time-convert
                                   (car (plist-get plist :time)))))
                 (push plist data))
             (setq data
                   (append data
                           (list
                            (list
                             :id nil
                             :author user-full-name
                             :time (current-time)
                             :desc "Uncommitted changes"))))))
         data)))))

(provide 'why-this)
;;; why-this.el ends here
