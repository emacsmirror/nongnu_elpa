;;; hermes-preview.el --- Owned native output previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An optional chat action layered above the managed-file browser.  Reuse its
;; bounded decoding, byte retention, native raster display and save-new-file
;; policy.  HTML uses an attribute-free passive DOM; SVG admits only passive
;; geometry.  Original bytes remain available independently of presentation.
;; The backend file API exposes current bytes, not artifact version history.

;;; Code:

(require 'hermes-files)
(require 'image)
(require 'dom)
(require 'shr)

(declare-function image-flush "image.c" (spec &optional frame))
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function image-transforms-p "image.c" (&optional frame))

(defvar-local hermes-preview--owner nil "Captured chat attachment.")
(defvar-local hermes-preview--cleanup nil "Idempotent pending-read cleanup.")
(defvar-local hermes-preview--descriptor nil "Original preview descriptor.")
(defvar-local hermes-preview--source-p nil "Non-nil when showing literal source.")
(defvar-local hermes-preview--image nil "Original rendered image specification.")

(defun hermes-preview--current-p (owner)
  "Return non-nil if captured chat OWNER still authorizes a read."
  (pcase-let ((`(,buffer ,lifetime ,session ,client ,connection ,endpoint) owner))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (derived-mode-p 'hermes-chat-mode)
                (equal lifetime hermes-chat--lifecycle-generation)
                (equal session hermes-chat--dashboard-active-session-id)
                (eq client hermes-chat--dashboard-client)))
         (or (null client)
             (and (equal connection
                         (hermes-dashboard-transport-client-generation client))
                  (equal endpoint
                         (hermes-dashboard-transport--api-client-base-url client)))))))

(defun hermes-preview--finish (status)
  "Settle this viewer with STATUS and release its pending-read resources."
  (let ((cleanup hermes-preview--cleanup))
    (setq hermes-preview--cleanup nil)
    (when cleanup (funcall cleanup)))
  (setq hermes-files--status status)
  (unless (equal status "Ready")
    (setq header-line-format (format " Preview | %s | g Retry | ? Help" status))))

(defun hermes-preview-cancel ()
  "Cancel this preview's pending read without stopping a shared transport."
  (interactive)
  (when hermes-preview--cleanup
    (hermes-preview--finish "Cancelled")))

(defun hermes-preview-quit ()
  "Cancel this preview's pending read and quit its window."
  (interactive)
  (hermes-preview-cancel)
  (quit-window))

(defun hermes-preview--html-dom (node)
  "Return only inert text and passive formatting from HTML NODE.
Drop every attribute, resource, link target, script and active container."
  (cond
   ((stringp node) node)
   ((not (consp node)) nil)
   ((memq (dom-tag node) '(script style iframe object embed link meta base
                                  form input button textarea select svg math)) nil)
   (t
    (cons (if (memq (dom-tag node)
                    '(html body div span p br hr h1 h2 h3 h4 h5 h6 pre code
                           blockquote ul ol li table tr td th thead tbody
                           b strong i em u s sub sup))
              (dom-tag node) 'div)
          (cons nil (delq nil (mapcar #'hermes-preview--html-dom
                                      (dom-children node))))))))

(defun hermes-preview--svg-safe-p (node)
  "Return non-nil for SVG NODE with only passive geometry.
Reject unknown tags and attributes, URI values, CSS and event handlers."
  (or (stringp node)
      (and (consp node)
           (memq (dom-tag node) '(svg g path rect circle ellipse line polyline
                                      polygon text tspan title desc))
           (seq-every-p
            (lambda (attribute)
              (let ((key (car attribute)) (value (cdr attribute)))
                (or (and (eq key 'xmlns)
                         (equal value "http://www.w3.org/2000/svg"))
                    (and (memq key '(width height viewBox x y x1 y1 x2 y2 cx cy
                                           r rx ry d points fill stroke stroke-width
                                           opacity fill-opacity stroke-opacity
                                           transform font-size font-family font-weight font-style
                                           text-anchor stroke-linecap stroke-linejoin))
                         (stringp value)
                         (string-match-p "\\`[A-Za-z0-9#.,+%() /-]*\\'" value)
                         (not (string-match-p "url\\|var\\|://" (downcase value)))))))
            (dom-attributes node))
           (seq-every-p #'hermes-preview--svg-safe-p (dom-children node)))))

(defun hermes-preview--render-markup (kind text)
  "Render passive KIND markup from TEXT, or return nil for source fallback."
  (when (and (fboundp 'libxml-parse-html-region)
             ;; No DTD, entity declarations or processing instructions.  The
             ;; HTML doctype alone is harmless and may occur in generated HTML.
             (not (string-match-p "<!\\(?:ENTITY\\|DOCTYPE[^>]*\\[\\)" (upcase text))))
    (condition-case nil
        (pcase kind
          ('html
           (let ((dom (with-temp-buffer
                        (insert text)
                        (libxml-parse-html-region (point-min) (point-max)))))
             (when dom
               (let ((shr-inhibit-images t) (shr-use-fonts nil))
                 (shr-insert-document (hermes-preview--html-dom dom)))
               t)))
          ('svg
           (when (and (not (string-match-p "<!\\|<\\?" text))
                      (display-images-p) (image-type-available-p 'svg))
             (let ((dom (with-temp-buffer
                          (insert text)
                          (libxml-parse-xml-region (point-min) (point-max)))))
               (when (and (eq (dom-tag dom) 'svg)
                          (hermes-preview--svg-safe-p dom))
                 (insert-image (create-image text 'svg t :max-width 800
                                             :max-height 800))
                 t)))))
      (error nil))))

(defun hermes-preview--view (bytes)
  "Display BYTES while retaining them unchanged for explicit saving."
  (unless (<= (length bytes) hermes-files--max-bytes)
    (user-error "Output exceeds the 4 MiB preview limit"))
  (let* ((descriptor hermes-preview--descriptor)
         (path (or (plist-get descriptor :path) (plist-get descriptor :label)))
         (kind (or (plist-get descriptor :kind)
                   (cond ((string-match-p "\\.\\(?:html?\\|xhtml\\)\\'" path) 'html)
                         ((string-match-p "\\.svg\\'" path) 'svg))))
         (text (hermes-files--preview bytes)))
    (hermes-files--view bytes path)
    (when (and kind (eq (car text) 'text) (not hermes-preview--source-p))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (hermes-preview--render-markup kind (cdr text))
          (erase-buffer)
          (insert "Native preview unavailable or unsafe; source follows.\n\n"
                  (cdr text)))
        (goto-char (point-min))))
    (let ((image (get-text-property (point-min) 'display)))
      (setq hermes-preview--image
            (and (eq (car-safe image) 'image) (copy-sequence image))))
    (setq header-line-format
          (format " Preview | %s | %s | s Save bytes | ? Help"
                  (hermes-files--label path)
                  (if hermes-preview--source-p "Source" "Inert view")))))

(defun hermes-preview-source ()
  "Toggle between inert presentation and retained literal source.
Wait for a pending read to finish, or cancel it first."
  (interactive)
  (when hermes-preview--cleanup
    (user-error "Read pending; wait or use C-g to cancel before changing view"))
  (unless (and hermes-files--bytes
               (eq (car (hermes-files--preview hermes-files--bytes)) 'text))
    (user-error "Output has no completed text source"))
  (setq hermes-preview--source-p (not hermes-preview--source-p))
  (hermes-preview--view hermes-files--bytes))

(defun hermes-preview--zoom (factor)
  "Scale this preview's rendered image by FACTOR, or reset when nil.
Limit zoom to 10–400 percent of the initial displayed size."
  (when hermes-preview--cleanup
    (user-error "Read pending; wait or use C-g to cancel before zooming"))
  (let ((image (get-text-property (point-min) 'display)))
    (unless (and hermes-preview--image (eq (car-safe image) 'image)
                 (not hermes-preview--source-p))
      (user-error "No rendered image; zoom is unavailable in source or text views"))
    (unless (and (fboundp 'image-transforms-p)
                 (memq 'scale (image-transforms-p)))
      (user-error "This Emacs frame does not support native image scaling"))
    (condition-case err
        (let* ((original hermes-preview--image)
               (size (image-size original t))
               (scale (and factor
                           (max 0.1 (min 4.0 (* factor (/ (float (car (image-size image t)))
                                                          (car size)))))))
               (next (copy-sequence original)))
          ;; The native interactive helpers defer to an idle timer and may
          ;; switch decoders.  Change only this owned data-backed spec instead.
          (when scale
            (setf (image-property next :scale) 1.0
                  (image-property next :max-width) nil
                  (image-property next :max-height) nil
                  (image-property next :width) (max 1 (round (* scale (car size))))
                  (image-property next :height) (max 1 (round (* scale (cdr size))))))
          (image-size next t)
          (image-flush image)
          (setcdr image (cdr next))
          (force-window-update (current-buffer)))
      (error (user-error "Native image zoom unavailable: %s" (error-message-string err))))))

(defun hermes-preview-zoom-in ()
  "Enlarge the rendered image by 25 percent, up to four times its initial size."
  (interactive)
  (hermes-preview--zoom 1.25))

(defun hermes-preview-zoom-out ()
  "Shrink the rendered image by 20 percent, down to a tenth of its initial size."
  (interactive)
  (hermes-preview--zoom 0.8))

(defun hermes-preview-zoom-reset ()
  "Restore the image's initial preview size, including any SVG fitting."
  (interactive)
  (hermes-preview--zoom nil))

(defun hermes-preview-copy-target ()
  "Copy this preview's exact remote target or source label."
  (interactive)
  (kill-new (or (plist-get hermes-preview--descriptor :path)
                (plist-get hermes-preview--descriptor :label)))
  (message "Copied preview target"))

(defun hermes-preview--read ()
  "Read this preview's remote path and settle any synchronous setup failure."
  (condition-case err
      (hermes-preview--start-read)
    ((error quit)
     (hermes-preview--finish
      (if (or (eq (car err) 'quit)
              (not (hermes-preview--current-p hermes-preview--owner)))
          "Cancelled" "Read unavailable; y Copy target"))
     (signal (car err) (cdr err)))))

(defun hermes-preview--start-read ()
  "Start reading this preview with its captured owning client."
  (hermes-preview-cancel)
  (unless (hermes-preview--current-p hermes-preview--owner)
    (user-error "Preview attachment changed; reopen from its chat"))
  (let* ((viewer (current-buffer))
         (owner hermes-preview--owner)
         (chat (car owner))
         (client (nth 3 owner))
         (path (plist-get hermes-preview--descriptor :path))
         (token (list nil))
         (cancel (lambda ()
                   (when (buffer-live-p viewer)
                     (with-current-buffer viewer (hermes-preview-cancel))))))
    (unless client
      (hermes-preview--finish "No connected client; y Copy target")
      (user-error "No connected client; copy the target instead"))
    (when (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*:" path)
      (user-error "Remote URLs are not managed-file paths; copy the target instead"))
    (hermes-dashboard-transport--cancel-idle-timer client)
    (cl-incf (hermes-dashboard-transport-client-refcount client))
    (setq hermes-preview--cleanup
          (lambda ()
            (when (car token)
              (hermes-dashboard-transport-unsubscribe client (car token)))
            (when (buffer-live-p chat)
              (with-current-buffer chat
                (remove-hook 'hermes-chat-lifecycle-invalidation-hook cancel t)
                (remove-hook 'kill-buffer-hook cancel t)
                (remove-hook 'change-major-mode-hook cancel t)))
            (hermes-dashboard-transport-release client)))
    (with-current-buffer chat
      (add-hook 'hermes-chat-lifecycle-invalidation-hook cancel nil t)
      (add-hook 'kill-buffer-hook cancel nil t)
      (add-hook 'change-major-mode-hook cancel nil t))
    (setcar token
            (hermes-dashboard-transport-subscribe
             client nil (lambda ()
                          (unless (hermes-preview--current-p owner)
                            (funcall cancel)))))
    (setq hermes-files--status "Loading"
          header-line-format " Preview | Loading | C-g Cancel | ? Help")
    (let ((request hermes-preview--cleanup))
      (hermes--promise-catch
       (hermes--promise-then
        (condition-case err
            (hermes-dashboard-transport-api-request-async
             "GET" "/api/files/read" :query (list (cons 'path path)) :client client
             :current-p (lambda ()
                          (and (buffer-live-p viewer)
                               (with-current-buffer viewer
                                 (and (derived-mode-p 'hermes-preview-mode)
                                      (eq request hermes-preview--cleanup)
                                      (equal owner hermes-preview--owner)))
                               (hermes-preview--current-p owner))))
          ((error quit) (hermes--promise-rejected (car err))))
        (lambda (result)
          (when (buffer-live-p viewer)
            (with-current-buffer viewer
              (when (eq request hermes-preview--cleanup)
                (if (not (hermes-preview--current-p owner))
                    (hermes-preview--finish "Cancelled")
                  (hermes-preview--view (hermes-files--decode result path))
                  (hermes-preview--finish "Ready")))))))
       (lambda (_reason)
         (when (buffer-live-p viewer)
           (with-current-buffer viewer
             (when (eq request hermes-preview--cleanup)
               (hermes-preview--finish
                (if (hermes-preview--current-p owner)
                    "Read failed or unavailable; y Copy target" "Cancelled"))))))))))

(defun hermes-preview-retry ()
  "Retry reading the current remote target from the same chat attachment."
  (interactive)
  (unless (plist-get hermes-preview--descriptor :path)
    (user-error "Source artifact does not require a remote read"))
  (hermes-preview--read))

(keymap-popup-define hermes-preview-mode-map
		     "Keys for generated-output previews."
		     :parent hermes-file-view-mode-map
		     :exit-key "<escape>"
		     :group "Output"
		     "s" ("Save NEW local file" hermes-file-save :inapt-if (lambda () (not hermes-files--bytes)))
		     "v" ("Source / inert view" hermes-preview-source :inapt-if (lambda () hermes-preview--cleanup))
		     "y" ("Copy target" hermes-preview-copy-target)
		     "g" ("Retry read" hermes-preview-retry)
		     :group "Image"
		     "+" ("Zoom in" hermes-preview-zoom-in)
		     "-" ("Zoom out" hermes-preview-zoom-out)
		     "0" ("Reset zoom" hermes-preview-zoom-reset)
		     :group "View"
		     "C-g" ("Cancel read" hermes-preview-cancel)
		     "q" ("Quit view" hermes-preview-quit)
		     "?" ("Help" hermes-preview-mode-map-popup))

(define-key hermes-preview-mode-map (kbd "n") #'next-line)
(define-key hermes-preview-mode-map (kbd "p") #'previous-line)
(define-key hermes-preview-mode-map (kbd "f") #'forward-char)
(define-key hermes-preview-mode-map (kbd "b") #'backward-char)

(define-derived-mode hermes-preview-mode hermes-file-view-mode "Hermes Preview"
  "View generated outputs without executing markup or reading local paths."
  :interactive nil
  (add-hook 'kill-buffer-hook #'hermes-preview-cancel nil t)
  (add-hook 'change-major-mode-hook #'hermes-preview-cancel nil t))

;;;###autoload
(defun hermes-preview-open (descriptor owner)
  "Open output DESCRIPTOR captured from chat attachment OWNER.
OWNER contains buffer, lifetime, session, client, connection and endpoint.
Remote files are current server bytes, not historical artifact revisions."
  (unless (hermes-preview--current-p owner)
    (user-error "Preview attachment changed; reopen from its chat"))
  (let* ((source (plist-get descriptor :source))
         (bytes (and source (encode-coding-string source 'utf-8-unix))))
    (when (and bytes (> (length bytes) hermes-files--max-bytes))
      (user-error "Output exceeds the 4 MiB preview limit"))
    (let ((viewer (generate-new-buffer "*Hermes Preview*"))
          (instance (buffer-local-value 'hermes-instance (car owner))))
      (with-current-buffer viewer
	(hermes-preview-mode)
	(setq-local hermes-instance instance)
	(setq hermes-preview--owner owner hermes-preview--descriptor descriptor))
      (pop-to-buffer viewer)
      (cond
       (bytes (hermes-preview--view bytes))
       ((string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*:" (plist-get descriptor :path))
	(setq hermes-files--status "Unsupported remote URL")
	(let ((inhibit-read-only t))
          (insert "Remote URLs are not managed-file paths and are never fetched here.\n"
                  "Use y to copy the exact target for an explicit external action.\n\n"
                  (plist-get descriptor :path)))
	(setq header-line-format " Preview | URL not fetched | y Copy target | ? Help"))
       (t (hermes-preview--read)))
      viewer)))

(provide 'hermes-preview)
;;; hermes-preview.el ends here
