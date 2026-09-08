;;; hermes-files.el --- Read-only remote managed files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Browse remote paths through the managed-files API.  The
;; server owns root/parent policy; no remote path is a local filename.
;; Reads are limited to 4 MiB and listings to 2000 rows (no pagination API).
;; Views are inert UTF-8 text, native raster images, or binary metadata,
;; never visited files or automatically selected modes.  Save explicitly from
;; the completed viewer to a NEW local file.
;; Existing destinations and symlink components are refused.  As with ordinary
;; Emacs file writes, use a trusted local directory (ancestor replacement by
;; another local process is not an atomic openat/no-follow transaction).

;;; Code:

(require 'hermes-browser)
(require 'keymap-popup)
(require 'image)

(defconst hermes-files--max-bytes (* 4 1024 1024)
  "Maximum decoded size accepted for a managed file.")
(defconst hermes-files--max-entries 2000
  "Maximum number of rows rendered from one directory response.")
(defvar-local hermes-files--client nil "Client retained by this browser.")
(defvar-local hermes-files--release nil "Idempotent client release thunk.")
(defvar-local hermes-files--path nil "Server-published current directory.")
(defvar-local hermes-files--parent nil "Server-published parent directory.")
(defvar-local hermes-files--rows nil "Current server entry objects.")
(defvar-local hermes-files--bytes nil "Validated literal bytes in a viewer.")
(defvar-local hermes-files--status "Loading" "Current request status.")
(defvar-local hermes-files--limited-p nil "Non-nil if the listing was capped.")

(defvar-local hermes-files--pending-viewer nil
  "Exact (BUFFER GENERATION) of this browser's pending read viewer.")
(defvar-local hermes-files--subscription nil
  "Transport event subscription owned by this pending request.")

(defun hermes-files--unobserve ()
  "Release this request's transport event observation."
  (when hermes-files--subscription
    (hermes-dashboard-transport-unsubscribe
     hermes-files--client hermes-files--subscription)
    (setq hermes-files--subscription nil)))

(defun hermes-files--retire ()
  "Retire this request and synchronously cancel its exact pending viewer."
  (hermes-browser--next-request-generation)
  (hermes-files--unobserve)
  (let ((pending hermes-files--pending-viewer))
    (setq hermes-files--pending-viewer nil)
    (when (and pending (hermes-browser--request-current-mode-p
                        (car pending) (cadr pending) 'hermes-file-view-mode))
      (with-current-buffer (car pending)
        (hermes-files-cancel)))))

(defun hermes-files--stop ()
  "Retire requests and release this browser's retained client."
  (hermes-files--retire)
  (let ((release hermes-files--release))
    (setq hermes-files--release nil hermes-files--client nil)
    (when release (funcall release))))

(defun hermes-files-cancel ()
  "Cancel pending requests without closing the browser."
  (interactive)
  (hermes-files--retire)
  (setq hermes-files--status "Cancelled")
  (when (and (derived-mode-p 'hermes-file-view-mode) (not hermes-files--bytes))
    (setq header-line-format " Managed file read cancelled; retry from browser | ? Help")))

(defun hermes-files--path-p (value)
  "Return non-nil if VALUE is a nonempty server path token."
  (and (stringp value) (not (string-empty-p value))
       (not (string-match-p "\0" value))))

(defun hermes-files--request (route path success &optional target)
  "GET ROUTE with PATH and call SUCCESS only for its current owner.
Optional TARGET is an already displayed inert viewer, never a local path."
  (hermes-files--retire)
  (let* ((buffer (current-buffer))
         (generation hermes-browser--request-generation)
         (client hermes-files--client)
         (connection (and client
                          (hermes-dashboard-transport-client-generation client)))
         (endpoint (and client
                        (hermes-dashboard-transport--api-client-base-url client)))
         (instance hermes-instance)
         (identity (copy-tree instance))
         (directory (copy-sequence hermes-files--path))
         (rows hermes-files--rows)
         (target-generation (and target (buffer-local-value
                                         'hermes-browser--request-generation target)))
         (current-p
          (lambda ()
            (and (hermes-browser--request-current-mode-p
                  buffer generation 'hermes-files-mode)
                 (eq client (buffer-local-value 'hermes-files--client buffer))
                 (equal connection
                        (hermes-dashboard-transport-client-generation client))
                 (equal endpoint
                        (hermes-dashboard-transport--api-client-base-url client))
                 (eq instance (buffer-local-value 'hermes-instance buffer))
                 (equal identity instance)
                 (equal directory (buffer-local-value 'hermes-files--path buffer))
                 (eq rows (buffer-local-value 'hermes-files--rows buffer))
                 (or (null target)
                     (hermes-browser--request-current-mode-p
                      target target-generation 'hermes-file-view-mode))))))
    (unless client (user-error "Reopen the managed file browser"))
    (setq hermes-files--status "Loading"
          hermes-files--pending-viewer (and target (list target target-generation)))
    (setq hermes-files--subscription
          (hermes-dashboard-transport-subscribe
           client nil
           (lambda ()
             ;; Settlement checks the request owner, not the retired connection
             ;; or viewer: neither may still authorize content publication.
             (when (and (hermes-browser--request-current-mode-p
                         buffer generation 'hermes-files-mode)
                        (eq client (buffer-local-value 'hermes-files--client buffer))
                        (not (equal connection
                                    (hermes-dashboard-transport-client-generation client))))
               (with-current-buffer buffer (hermes-files-cancel))))))
    (hermes--promise-catch
     (hermes--promise-then
      (condition-case err
          (hermes-dashboard-transport-api-request-async
           "GET" route :query (and path (list (cons 'path path))) :client client)
        ((error quit) (hermes--promise-rejected (car err))))
      (lambda (result)
        (when (funcall current-p)
          (with-current-buffer buffer
            (funcall success result)
            (hermes-files--unobserve)
            (setq hermes-files--status "Ready" hermes-files--pending-viewer nil)))))
     (lambda (_reason)
       ;; Do not echo response bodies: file content can contain secrets.
       (when (funcall current-p)
         (with-current-buffer buffer
           (hermes-files--unobserve)
           (setq hermes-files--pending-viewer nil
                 hermes-files--status "Read failed (invalid or unavailable response)"))
         (when target
           (with-current-buffer target
             (setq hermes-files--status "Failed"
                   header-line-format " Managed file read failed; retry from browser | ? Help"))))))))

(defun hermes-files--label (text)
  "Return TEXT quoted for inert, single-line display."
  (let ((print-escape-newlines t) (print-escape-control-characters t))
    (prin1-to-string text)))

(defun hermes-files--entry (entry)
  "Return a tabulated row for server ENTRY, rejecting malformed identities."
  (let ((path (hermes-transport--get entry 'path))
        (name (hermes-transport--get entry 'name))
        (directory (eq t (hermes-transport--get entry 'is_directory)))
        (size (hermes-transport--get entry 'size)))
    (unless (and (hermes-files--path-p path) (stringp name)
                 (or directory (and (integerp size) (>= size 0))))
      (error "Invalid managed file entry"))
    (list entry (vector (hermes-browser--face-cell
                         (hermes-files--label name) 'hermes-browser-name)
                        (if directory "Directory" "File")
                        (if directory "" (number-to-string size))))))

(defun hermes-files--position (position)
  "Capture the logical file row and column at POSITION."
  (save-excursion
    (goto-char position)
    (let ((entry (tabulated-list-get-id)))
      (list (hermes-transport--get entry 'path)
            (hermes-transport--get entry 'name) (current-column) position))))

(defun hermes-files--restore-position (anchor)
  "Return the position of logical ANCHOR in the refreshed listing."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (let ((entry (tabulated-list-get-id)))
                  (not (and (equal (car anchor) (hermes-transport--get entry 'path))
                            (equal (cadr anchor) (hermes-transport--get entry 'name))))))
      (forward-line 1))
    (if (eobp) (min (nth 3 anchor) (point-max))
      (move-to-column (nth 2 anchor))
      (point))))

(defun hermes-files--name-width ()
  "Return the Name width fitting the narrowest displaying text viewport."
  (let ((widths
         (save-excursion
           (mapcar
            (lambda (window)
              (with-selected-window window
                (floor (/ (- (window-body-width window t)
                             (if display-line-numbers
                                 (line-number-display-width t) 0))
                          (float (window-font-width window))))))
            (get-buffer-window-list (current-buffer) nil t)))))
    ;; Reserve the continuation column, padding, Kind, Bytes and separators.
    (max 4 (- (if widths (apply #'min widths) (window-body-width))
              1 tabulated-list-padding 9 12 2))))

(defun hermes-files--context ()
  "Return compact server context, retaining its full text in help echo."
  (let* ((instance (hermes-instance-name hermes-instance))
         (path (if hermes-files--path (hermes-files--label hermes-files--path)
                 "Server default"))
         (suffix (format " | %d rows%s | ? Help" (length hermes-files--rows)
                         (if hermes-files--limited-p " (limited)" "")))
         (prefix (concat "Files " (truncate-string-to-width instance 12 nil nil t) " | "))
         (width (max 4 (- (window-body-width) (string-width prefix)
                          (string-width suffix) (length hermes-files--status) 4))))
    (propertize (concat prefix (truncate-string-to-width path width nil nil t) suffix)
                'help-echo (concat instance " | " path suffix))))

(defun hermes-files--print ()
  "Print native columns, preserving logical point and all window viewports."
  (let ((position (hermes-files--position (point)))
        (windows (mapcar (lambda (w)
                          (list w (hermes-files--position (window-start w))
                                (hermes-files--position (window-point w))))
                        (get-buffer-window-list (current-buffer) nil t))))
    (setq tabulated-list-format
          (vector (list "Name" (hermes-files--name-width) t)
                  (list "Kind" 9 t) (list "Bytes" 12 t :right-align t :pad-right 0)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (pcase-dolist (`(,window ,start ,point) windows)
      (set-window-start window (hermes-files--restore-position start) t)
      (set-window-point window (hermes-files--restore-position point)))
    (goto-char (hermes-files--restore-position position))))

(defun hermes-files--resize ()
  "Refit native file columns to the current window geometry."
  (when (and (get-buffer-window (current-buffer) t)
             (/= (cadr (aref tabulated-list-format 0)) (hermes-files--name-width)))
    (hermes-files--print)))

(defun hermes-files--render (result)
  "Render validated directory RESULT without selecting any window."
  (let* ((path (hermes-transport--get result 'path))
         (parent (hermes-transport--get result 'parent))
         (entries (append (hermes-transport--get result 'entries) nil))
         (rows (mapcar #'hermes-files--entry
                       (seq-take entries hermes-files--max-entries))))
    (unless (and (hermes-files--path-p path)
                 (or (null parent) (hermes-files--path-p parent)))
      (error "Invalid managed directory"))
    (setq hermes-files--path path hermes-files--parent parent
          hermes-files--rows (mapcar #'car rows)
          hermes-files--limited-p (> (length entries) hermes-files--max-entries)
          tabulated-list-entries rows)
    (hermes-files--print)))

(defun hermes-files-refresh (&rest _)
  "Refresh the current managed directory without redisplaying it."
  (interactive)
  (hermes-files--request "/api/files" hermes-files--path #'hermes-files--render))

(defun hermes-files--navigate (path)
  "Ask the server to list exact remote PATH without local expansion."
  (unless (hermes-files--path-p path) (user-error "No published directory"))
  (setq hermes-files--path path hermes-files--parent nil hermes-files--rows nil
        hermes-files--limited-p nil tabulated-list-entries nil)
  (hermes-files--print)
  (hermes-files-refresh))

(defun hermes-files-directory (path)
  "Browse remote directory PATH, subject to the server's path policy.
Read a literal remote token, without local filename completion or expansion."
  (interactive
   (let ((owner (current-buffer))
         (generation hermes-browser--request-generation))
     (let ((path (read-string "Remote directory: " hermes-files--path)))
       (unless (and (eq owner (current-buffer))
                    (hermes-browser--request-current-mode-p
                     owner generation 'hermes-files-mode))
         (user-error "Managed directory changed during prompt"))
       (list path))))
  (hermes-files--navigate path))

(defun hermes-files-up ()
  "Visit the parent published by the backend, if any."
  (interactive)
  (unless hermes-files--parent (user-error "No parent published by server"))
  (hermes-files--navigate hermes-files--parent))

(defun hermes-files--decode (result path)
  "Return validated literal bytes from RESULT for exact PATH."
  (let ((size (hermes-transport--get result 'size))
        (url (hermes-transport--get result 'data_url)))
    (unless (and (equal path (hermes-transport--get result 'path))
                 (integerp size) (<= 0 size hermes-files--max-bytes)
                 (stringp url)
                 (<= (length url) (+ 1024 (* 4 (/ (+ size 2) 3))))
                 (string-match "\\`data:[^;,\n]*;base64,\\([A-Za-z0-9+/]*=*\\)\\'" url))
      (error "Invalid or oversized managed file"))
    (let* ((encoded (match-string 1 url))
           (bytes (base64-decode-string encoded)))
      (unless (and (= (length bytes) size)
                   (equal encoded (base64-encode-string bytes t)))
        (error "Managed file size or encoding mismatch"))
      bytes)))

(defun hermes-files--preview (bytes)
  "Classify BYTES as (KIND . TEXT), without invoking file handlers.
Only explicit PNG, JPEG and GIF signatures select native image decoders.
Other content is text only when valid UTF-8 without binary controls."
  (cond
   ((string-prefix-p (unibyte-string 137 80 78 71 13 10 26 10) bytes) '(png))
   ((string-prefix-p (unibyte-string 255 216 255) bytes) '(jpeg))
   ((or (string-prefix-p "GIF87a" bytes) (string-prefix-p "GIF89a" bytes)) '(gif))
   (t
    (let ((text (decode-coding-string bytes 'utf-8-unix)))
      ;; Invalid UTF-8 becomes raw-byte characters in Emacs, not Unicode.
      (if (or (string-match-p "[\0-\10\13\14\16-\37\177-\237]" text)
              (seq-some (lambda (char) (> char #x10ffff)) text))
          '(binary)
        (cons 'text text))))))

(defun hermes-files--view (bytes path)
  "Render a safe preview of validated BYTES for remote PATH.
Retain the original bytes independently for saving, even without image support."
  (let* ((preview (hermes-files--preview bytes))
         (kind (car preview))
         (image (and (memq kind '(png jpeg gif))
                     (display-images-p) (image-type-available-p kind)
                     (condition-case nil
                         (create-image bytes kind t)
                       (error nil))))
         (label (cond ((eq kind 'text) "UTF-8 text")
                      ((eq kind 'binary) "Binary (no preview)")
                      (image (upcase (symbol-name kind)))
                      (t (format "%s preview unavailable" (upcase (symbol-name kind)))))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (set-buffer-multibyte t)
      (cond ((eq kind 'text) (insert (cdr preview)))
            (image (insert-image image))
            (t (insert (format "%s\n%d bytes retained.  Press s to save a NEW local file.\n"
                               label (length bytes)))))
      (goto-char (point-min)))
    (setq hermes-files--bytes bytes hermes-files--status "Ready"
          header-line-format
          (format " %s | %s | %s | ? Help"
                  (hermes-instance-name hermes-instance)
                  (hermes-files--label path) label))))

(defun hermes-files-open ()
  "Descend into the selected directory or open an inert file preview."
  (interactive)
  (let* ((row (tabulated-list-get-id))
         (path (hermes-transport--get row 'path)))
    (unless (memq row hermes-files--rows) (user-error "No current file row"))
    (if (eq t (hermes-transport--get row 'is_directory))
        (hermes-files--navigate path)
      (unless (<= (hermes-transport--get row 'size) hermes-files--max-bytes)
        (user-error "Managed file exceeds the 4 MiB viewer/download limit"))
      (let ((browser (current-buffer))
            (instance hermes-instance)
            (viewer (generate-new-buffer "*Hermes File*")))
        (with-current-buffer viewer
          (hermes-file-view-mode)
          (setq-local hermes-instance instance)
          (setq header-line-format
                (format " %s | ? Help" (hermes-files--label path))))
        ;; Display now, never from a network callback.  Subsequent typing or
        ;; window switches are not displaced by completion.
        (pop-to-buffer viewer)
        (with-current-buffer browser
          (hermes-files--request
           "/api/files/read" path
           (lambda (result)
             (let ((bytes (hermes-files--decode result path)))
               (with-current-buffer viewer
                 (hermes-files--view bytes path))))
           viewer))))))

(defun hermes-file-save (&optional filename)
  "Save validated viewer bytes to new local FILENAME, without overwriting.
Refuse remote names, existing files and symlink components.  Choose a trusted
local directory; concurrent hostile ancestor replacement is not supported."
  (interactive)
  (unless (and (derived-mode-p 'hermes-file-view-mode) hermes-files--bytes)
    (user-error "No completed managed file read"))
  (let ((owner (current-buffer)) (bytes hermes-files--bytes))
    (unless filename
      (setq filename (read-file-name "Save bytes to NEW local file: " nil nil nil)))
    (unless (and (eq owner (current-buffer))
                 (derived-mode-p 'hermes-file-view-mode)
                 (eq bytes hermes-files--bytes))
      (user-error "Managed byte viewer changed during prompt")))
  (when (file-remote-p filename) (user-error "Choose a local destination"))
  (let* ((destination (expand-file-name filename))
         (parent (file-name-directory destination)))
    (when (or (file-exists-p destination) (file-symlink-p destination))
      (user-error "Destination exists; choose a new filename"))
    (let ((component destination))
      (while component
        (when (file-symlink-p component) (user-error "Symlink destination refused"))
        (let ((next (directory-file-name (file-name-directory component))))
          (setq component (unless (equal next component) next)))))
    (unless (file-directory-p parent) (user-error "Destination directory is missing"))
    (let ((bytes hermes-files--bytes))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert bytes)
        (let ((coding-system-for-write 'no-conversion)
              (write-region-annotate-functions nil)
              (write-region-post-annotation-function nil)
              (format-alist nil)
              (file-name-handler-alist nil))
          (write-region (point-min) (point-max) destination nil 'silent nil 'excl))))
    (message "Saved managed bytes to %s" destination)))

(keymap-popup-define hermes-files-mode-map
  "Keys for the read-only managed file browser."
  :parent tabulated-list-mode-map
  :exit-key "<escape>"
  :group "Browse"
  "RET" ("Open" hermes-files-open)
  "^" ("Parent" hermes-files-up)
  "g" ("Refresh" hermes-files-refresh)
  "d" ("Directory" hermes-files-directory)
  :group "View"
  "C-g" ("Cancel read" hermes-files-cancel)
  "q" ("Quit view" hermes-files-quit)
  "<escape>" ("Dismiss menu" keymap-popup-dismiss)
  "?" ("Help" hermes-files-mode-map-popup))

(define-derived-mode hermes-files-mode tabulated-list-mode "Hermes Files"
  "Browse remote managed files without remote mutations.
Keep native sortable headings above the rows and server context in the mode
line.  Names and server paths retain their full quoted text in help echo."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-files-refresh)
  (setq-local mode-line-process '(:eval (concat " " hermes-files--status)))
  (setq-local mode-line-format
              '("%e " (:eval (string-replace "%" "%%" (hermes-files--context)))
                mode-line-process))
  (add-hook 'kill-buffer-hook #'hermes-files--stop nil t)
  (add-hook 'change-major-mode-hook #'hermes-files--stop nil t)
  (add-hook 'window-configuration-change-hook #'hermes-files--resize nil t)
  (hermes-files--print))

(defun hermes-files-quit ()
  "Cancel pending display work and quit this window."
  (interactive)
  (hermes-files-cancel)
  (quit-window))

(keymap-popup-define hermes-file-view-mode-map
  "Keys for the inert managed file viewer."
  :parent special-mode-map
  :exit-key "<escape>"
  :group "File"
  "s" ("Save NEW local file" hermes-file-save :inapt-if (lambda () (not hermes-files--bytes)))
  "q" ("Back / quit view" hermes-files-quit)
  :group "View"
  "C-g" ("Cancel read" hermes-files-cancel)
  "<escape>" ("Dismiss menu" keymap-popup-dismiss)
  "?" ("Help" hermes-file-view-mode-map-popup))

(define-derived-mode hermes-file-view-mode special-mode "Hermes File"
  "Preview managed content without file modes, local variables or evaluation."
  :interactive nil
  (hermes-browser--next-request-generation)
  (setq-local buffer-undo-list t)
  (setq-local mode-line-process '(:eval (concat " " hermes-files--status))))

;;;###autoload
(defun hermes-files ()
  "Browse backend-managed files using the owning dashboard instance.
RET opens directories or an inert viewer; save bytes explicitly from there.
Use `hermes-files-directory' to enter a remote directory, even if listing fails.
The backend supplies root and parent policy.  No local Dired or remote writes
are used.  Reads are capped at 4 MiB, listings at the first 2000 entries."
  (interactive)
  (let ((instance (copy-tree (hermes-instance-resolve)))
        (buffer (generate-new-buffer "*Hermes Files*")))
    (with-current-buffer buffer
      (hermes-files-mode)
      (hermes-browser--own-instance instance)
      (condition-case err
          (let* ((hermes-dashboard-transport-url (hermes-instance-url instance))
                 (existing (hermes-browser--existing-client))
                 (client
                  (if (and existing (hermes-dashboard-transport--client-viable-p existing))
                      (progn
                        ;; The shared browser helper only borrows chat clients.
                        ;; This buffer instead owns one reference until teardown.
                        (hermes-dashboard-transport--cancel-idle-timer existing)
                        (cl-incf (hermes-dashboard-transport-client-refcount existing))
                        existing)
                    (hermes-dashboard-transport-acquire :callback #'ignore))))
            (setq hermes-files--client client
                  hermes-files--release
                  (lambda () (hermes-dashboard-transport-release client))))
        ((error quit) (kill-buffer buffer) (signal (car err) (cdr err)))))
    (pop-to-buffer buffer)
    (hermes-files-refresh)))

(provide 'hermes-files)
;;; hermes-files.el ends here
