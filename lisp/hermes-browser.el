;;; hermes-browser.el --- Shared foundation for Hermes dashboard browsers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

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

;; Foundation shared by the dashboard browser views (sessions, rollback,
;; cron, subagents, inventory, ...).  It provides dashboard client
;; provisioning -- reusing a live chat connection when one exists, else a
;; transient client released when its work settles -- so each browser
;; composes its RPC over a promise without re-implementing the plumbing.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-chat)

;;; Faces

(defface hermes-browser-identifier
  '((t :inherit shadow))
  "Face for identifiers in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-profile
  '((t :inherit font-lock-constant-face))
  "Face for profile and assignee names in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-count
  '((t :inherit font-lock-constant-face))
  "Face for counts and priorities in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-active
  '((t :inherit font-lock-keyword-face))
  "Face for active states in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-success
  '((t :inherit success))
  "Face for successful states in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-pending
  '((t :inherit warning))
  "Face for pending states in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-error
  '((t :inherit error))
  "Face for failed or blocked states in Hermes browser rows."
  :group 'hermes)

(defface hermes-browser-muted
  '((t :inherit shadow))
  "Face for inactive states and secondary data in Hermes browser rows."
  :group 'hermes)

(defun hermes-browser--face-cell (value face)
  "Return VALUE as a string carrying FACE when both are non-empty."
  (let ((text (if (stringp value)
                  (copy-sequence value)
                (format "%s" (or value "")))))
    (if (and face (not (string-empty-p text)))
        (propertize text 'face face)
      text)))

(defun hermes-browser--status-face (status)
  "Return the shared semantic face for STATUS, or nil when unknown."
  (let ((status (downcase (substring-no-properties
                           (format "%s" (or status ""))))))
    (cond
     ((member status '("active" "in-progress" "open" "running" "streaming"
                       "working"))
      'hermes-browser-active)
     ((member status '("complete" "completed" "configured" "done" "enabled"
                       "healthy" "ok" "on" "ready" "succeeded" "success"))
      'hermes-browser-success)
     ((member status '("connecting" "paused" "pending" "queued" "scheduled"
                       "todo" "triage" "waiting" "warning"))
      'hermes-browser-pending)
     ((member status '("blocked" "critical" "degraded" "error" "failed"
                       "failure" "rejected" "unhealthy"))
      'hermes-browser-error)
     ((member status '("archived" "closed" "disabled" "idle" "info" "off"
                       "stopped" "unknown"))
      'hermes-browser-muted))))

(defun hermes-browser--status-cell (status)
  "Return STATUS styled with its shared semantic face when known."
  (hermes-browser--face-cell status (hermes-browser--status-face status)))

(defun hermes-browser--existing-client ()
  "Return a live dashboard client from any Hermes chat buffer, or nil."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'hermes-chat-mode)
                    (hermes-chat--dashboard-client-live-p
                     hermes-chat--dashboard-client)
                    hermes-chat--dashboard-client)))
           (buffer-list)))

(defun hermes-browser--with-client (fn)
  "Call FN with a connected CLIENT and a DONE cleanup thunk.
Reuses a live chat connection when one exists; otherwise connects a transient
client that DONE stops.  Shared by the dashboard browser commands."
  (let* ((existing (hermes-browser--existing-client))
         (client (or existing
                     (hermes-dashboard-transport-start :callback #'ignore)))
         (done (lambda ()
                 (unless existing
                   (hermes-dashboard-transport-stop client)))))
    (funcall fn client done)))

(defun hermes-browser--run-on-client (make-promise &optional on-success)
  "Run MAKE-PROMISE on a dashboard client, releasing it when its promise settles.
MAKE-PROMISE receives the CLIENT and returns a promise.  ON-SUCCESS, when given,
receives the resolved result; rejections are reported with a `Hermes:' message.
Shared by the dashboard browser commands."
  (hermes-browser--with-client
   (lambda (client done)
     (hermes--promise-catch
      (hermes--promise-then
       (hermes--promise-finally (funcall make-promise client) done)
       on-success)
      (lambda (reason) (message "Hermes: %s" reason))))))

(defun hermes-browser--notify (title body)
  "Show desktop notification TITLE/BODY, falling back to the echo area.
Uses `notifications-notify' when D-Bus notifications are available, and quietly
degrades to a `message' on systems or builds without them."
  (or (and (require 'notifications nil t)
           (fboundp 'notifications-notify)
           (ignore-errors
             (notifications-notify :title title :body body :app-name "Hermes")
             t))
      (progn (message "%s: %s" title body) nil)))

;;; Dynamic column widths

(defun hermes-browser--visible-window-width ()
  "Return the current buffer's visible text width, or nil if not visible."
  (and-let* ((window (get-buffer-window (current-buffer) t)))
    (window-body-width window)))

(defun hermes-browser--display-width (&optional width)
  "Return WIDTH or the visible text width, clamped to a positive value."
  (max 1 (or width
             (hermes-browser--visible-window-width)
             (window-body-width))))

(defun hermes-browser--sum (numbers)
  "Return the sum of NUMBERS."
  (apply #'+ numbers))

(defun hermes-browser--shrink-widths (widths target)
  "Return WIDTHS reduced to fit TARGET while keeping columns positive."
  (let ((widths (copy-sequence widths)))
    (while (> (hermes-browser--sum widths) target)
      (let ((max-width 1)
            max-cell)
        (dotimes (i (length widths))
          (let ((width (nth i widths)))
            (when (> width max-width)
              (setq max-width width
                    max-cell i))))
        (if max-cell
            (setcar (nthcdr max-cell widths) (1- max-width))
          (setq target (hermes-browser--sum widths)))))
    widths))

(defun hermes-browser--allocate-column-widths (width specs)
  "Return column widths fitting WIDTH for SPECS.
Each item in SPECS is (MINIMUM WEIGHT).  The returned widths account for
one character of `tabulated-list' padding between columns and fit WIDTH
when WIDTH can hold one character per column plus padding."
  (let* ((column-count (length specs))
         (separator-width (max 0 (1- column-count)))
         (available (max column-count
                         (- (hermes-browser--display-width width)
                            separator-width)))
         (minimums (mapcar #'car specs))
         (minimum-total (hermes-browser--sum minimums)))
    (if (> minimum-total available)
        (hermes-browser--shrink-widths minimums available)
      (let* ((weights (mapcar #'cadr specs))
             (weight-total (hermes-browser--sum weights))
             (remaining (- available minimum-total))
             (widths (copy-sequence minimums))
             (assigned 0))
        (when (> weight-total 0)
          (dotimes (i column-count)
            (let ((share (/ (* remaining (nth i weights)) weight-total)))
              (setq assigned (+ assigned share))
              (setcar (nthcdr i widths) (+ (nth i widths) share))))
          (let ((left (- remaining assigned))
                (i 0))
            (while (> left 0)
              (when (> (nth i weights) 0)
                (setcar (nthcdr i widths) (1+ (nth i widths)))
                (setq left (1- left)))
              (setq i (% (1+ i) column-count)))))
        widths))))

(defun hermes-browser--dynamic-format (width specs)
  "Return a `tabulated-list-format' vector fitting WIDTH for SPECS.
Each SPEC is (NAME MINIMUM WEIGHT &optional SORT MAX): NAME is the header,
MINIMUM the smallest width, WEIGHT its share of leftover space, SORT the sort
predicate, and MAX an optional width cap."
  (let ((widths (hermes-browser--allocate-column-widths
                 width (mapcar (lambda (spec) (list (nth 1 spec) (nth 2 spec)))
                               specs))))
    (apply #'vector
           (cl-loop for spec in specs
                    for column-width in widths
                    for max = (nth 4 spec)
                    collect (list (nth 0 spec)
                                  (if max (min column-width max) column-width)
                                  (nth 3 spec))))))

(defmacro hermes-define-list-browser (name &rest body)
  "Define a `tabulated-list' browser NAME backed by a dashboard RPC.
NAME is the short browser name; the macro defines `hermes-NAME-mode',
`hermes-NAME-mode-map', `hermes-NAME--render', `hermes-NAME--revert', and the
command `hermes-list-NAME'.  BODY is a plist:

  :title           display/mode-line name (string)
  :buffer          browser buffer name (string)
  :columns         static `tabulated-list-format' vector
  :dynamic-columns window-responsive column specs, each
                   (NAME MINIMUM WEIGHT &optional SORT MAX); columns flex with
                   the window width and re-fit on resize.  Mutually exclusive
                   with `:columns'
  :command         list-command symbol when it differs from `hermes-list-NAME';
                   it must match the caller's `(autoload ...)' cookie
  :fetch           function (CLIENT -> promise) issuing the dashboard RPC
  :rows            pure function (RESULT -> list of `tabulated-list' entries)
  :keys            extra bindings, spliced into `defvar-keymap'
  :doc             major-mode docstring, optional
  :command-doc     list-command docstring, optional
  :on-result       function (RESULT) called in the buffer after each render,
                   for side effects only (e.g. failure notifications)

`:fetch' and `:rows' must be pure: this macro owns the only side effects -- the
buffer render and the dashboard client plumbing."
  (declare (indent 1))
  (let ((mode (intern (format "hermes-%s-mode" name)))
        (map (intern (format "hermes-%s-mode-map" name)))
        (render (intern (format "hermes-%s--render" name)))
        (revert (intern (format "hermes-%s--revert" name)))
        (format-fn (intern (format "hermes-%s--format" name)))
        (size-change (intern (format "hermes-%s--window-size-change" name)))
        (command (or (plist-get body :command)
                     (intern (format "hermes-list-%s" name))))
        (title (plist-get body :title))
        (buffer (plist-get body :buffer))
        (columns (plist-get body :columns))
        (dynamic (plist-get body :dynamic-columns))
        (fetch (plist-get body :fetch))
        (rows (plist-get body :rows))
        (keys (plist-get body :keys))
        (doc (plist-get body :doc))
        (command-doc (plist-get body :command-doc))
        (on-result (plist-get body :on-result)))
    `(progn
       (defvar-keymap ,map
         :doc ,(format "Keymap for `%s'." mode)
         :parent tabulated-list-mode-map
         ,@keys)
       ,@(and dynamic
              `((defun ,format-fn (&optional width)
                  ,(format "Return the dynamic `tabulated-list-format' for the %s browser."
                           title)
                  (hermes-browser--dynamic-format width ',dynamic))
                (defun ,size-change (window)
                  ,(format "Re-fit %s columns when WINDOW changes size." title)
                  (when (derived-mode-p ',mode)
                    (let ((old-format tabulated-list-format))
                      (setq tabulated-list-format
                            (,format-fn (window-body-width window)))
                      (unless (equal old-format tabulated-list-format)
                        (tabulated-list-init-header)
                        (tabulated-list-print t)))))))
       (define-derived-mode ,mode tabulated-list-mode ,title
         ,(or doc (format "Major mode for the %s browser." title))
         :interactive nil
         (setq tabulated-list-format ,(if dynamic `(,format-fn) columns))
         (setq-local revert-buffer-function #',revert)
         ,@(and dynamic
                `((add-hook 'window-size-change-functions #',size-change nil t)))
         (tabulated-list-init-header))
       (defun ,render (result)
         ,(format "Render dashboard RESULT into the %s buffer in place." title)
         (with-current-buffer (get-buffer-create ,buffer)
           (unless (derived-mode-p ',mode)
             (,mode))
           ,@(and dynamic
                  `((setq tabulated-list-format (,format-fn))
                    (tabulated-list-init-header)))
           (setq tabulated-list-entries (funcall ,rows result))
           (tabulated-list-print t)
           ,@(and on-result `((funcall ,on-result result)))))
       (defun ,revert (&rest _)
         ,(format "Refresh the %s browser without re-displaying it." title)
         (hermes-browser--run-on-client ,fetch #',render))
       (defun ,command ()
         ,(or command-doc (format "Browse %s from the Hermes dashboard." title))
         (interactive)
         (hermes-browser--run-on-client
          ,fetch
          ,(if dynamic
               `(lambda (result)
                  (pop-to-buffer (get-buffer-create ,buffer))
                  (,render result))
             `(lambda (result)
                (,render result)
                (pop-to-buffer ,buffer))))))))

(provide 'hermes-browser)
;;; hermes-browser.el ends here
