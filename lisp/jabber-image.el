;;; jabber-image.el --- image display support  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared image creation and async fetching for avatars and inline
;; previews.  All images use dynamic sizing via `image-property'
;; with :max-width/:max-height instead of ImageMagick scaling.

;;; Code:

(require 'mm-decode)
(require 'url-queue)
(require 'url-parse)

(defgroup jabber-image nil
  "Image display settings."
  :group 'jabber)

(defcustom jabber-image-max-width 300
  "Maximum width in pixels for inline images."
  :type 'integer
  :group 'jabber-image)

(defcustom jabber-image-max-height 300
  "Maximum height in pixels for inline images."
  :type 'integer
  :group 'jabber-image)

(defun jabber-image--mime-to-type (mime-type)
  "Return an image type symbol for MIME-TYPE string, or nil."
  (when mime-type
    (pcase mime-type
      ("image/png"  'png)
      ("image/jpeg" 'jpeg)
      ("image/gif"  'gif)
      ("image/webp" 'webp)
      ("image/svg+xml" 'svg)
      ("image/bmp"  'bmp)
      ("image/x-xbitmap" 'xbm)
      ("image/x-xpixmap" 'xpm)
      ("image/tiff" 'tiff)
      (_ nil))))

(defun jabber-image-create (data &optional mime-type max-width max-height)
  "Create a dynamically-sized image from raw DATA string.
MIME-TYPE is a MIME type string like \"image/png\"; if nil Emacs
auto-detects the type.  MAX-WIDTH and MAX-HEIGHT default to
`jabber-image-max-width' and `jabber-image-max-height'."
  (let ((image (create-image data
                             (jabber-image--mime-to-type mime-type)
                             t)))
    (setf (image-property image :max-width)
          (or max-width jabber-image-max-width))
    (setf (image-property image :max-height)
          (or max-height jabber-image-max-height))
    image))

(defun jabber-image-create-from-file (file &optional max-width max-height)
  "Create a dynamically-sized image from FILE path.
MAX-WIDTH and MAX-HEIGHT default to `jabber-image-max-width' and
`jabber-image-max-height'."
  (let ((image (create-image file)))
    (setf (image-property image :max-width)
          (or max-width jabber-image-max-width))
    (setf (image-property image :max-height)
          (or max-height jabber-image-max-height))
    image))

(defun jabber-image-fetch (url callback &rest cbargs)
  "Fetch image at URL asynchronously.
When complete, call CALLBACK with the image object (or nil on
error) followed by CBARGS.  Image is sized per
`jabber-image-max-width' and `jabber-image-max-height'."
  (url-queue-retrieve
   url
   (lambda (status cb args)
     (let ((url-buffer (current-buffer))
           (image (unless (plist-get status :error)
                    (goto-char (point-min))
                    (when (re-search-forward "\r?\n\r?\n" nil t)
                      (let* ((handle (mm-dissect-buffer t))
                             (img (mm-get-image handle)))
                        (when img
                          (setf (image-property img :max-width)
                                jabber-image-max-width)
                          (setf (image-property img :max-height)
                                jabber-image-max-height)
                          img))))))
       (kill-buffer url-buffer)
       (apply cb image args)))
   (list callback cbargs)
   'silent
   'inhibit-cookies))

(defun jabber-image--replace-placeholder (image beg end buffer)
  "Replace placeholder between BEG and END in BUFFER with IMAGE."
  (when (and image (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (put-text-property beg end 'display image)))))

(defun jabber-image--load-at-point (url beg end buffer)
  "Fetch URL and display the image over the placeholder in BUFFER."
  (jabber-image-fetch
   url
   #'jabber-image--replace-placeholder
   beg end buffer))

(defvar jabber-image-placeholder-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'jabber-image-placeholder-click)
    (define-key map (kbd "RET") #'jabber-image-placeholder-click)
    map)
  "Keymap for clickable image placeholders.")

(defun jabber-image-placeholder-click ()
  "Load the image at the placeholder under point."
  (interactive)
  (let ((url (get-text-property (point) 'jabber-image-url))
        (beg (previous-single-property-change
              (1+ (point)) 'jabber-image-url))
        (end (next-single-property-change
              (point) 'jabber-image-url)))
    (when url
      (jabber-image--load-at-point
       url (or beg (point-min)) (or end (point-max))
       (current-buffer)))))

(defun jabber-image-insert-placeholder (url &optional text)
  "Insert a clickable image placeholder for URL at point.
TEXT is the display text; defaults to \"[Image: FILENAME]\" where
FILENAME is extracted from URL."
  (let ((label (or text
                   (format "[Image: %s]"
                           (file-name-nondirectory
                            (url-filename
                             (url-generic-parse-url url)))))))
    (insert (propertize label
                        'face 'link
                        'jabber-image-url url
                        'keymap jabber-image-placeholder-keymap
                        'mouse-face 'highlight
                        'help-echo "Click to load image"))))

(provide 'jabber-image)

;;; jabber-image.el ends here
