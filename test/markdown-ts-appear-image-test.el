;;; markdown-ts-appear-image-test.el --- Image preview tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise image previews with real SVG files.  Batch tests only
;; override display availability; GUI tests also exercise actual redisplay.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst markdown-ts-appear-image-test--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(add-to-list 'load-path markdown-ts-appear-image-test--root)
(require 'markdown-ts-appear)

(defmacro markdown-ts-appear-image-test--with-buffer (content &rest body)
  "Create an image-enabled Markdown buffer with CONTENT and evaluate BODY."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (and (treesit-ready-p '(markdown markdown-inline))
                       (image-type-available-p 'svg)))
     (with-temp-buffer
       (setq default-directory markdown-ts-appear-image-test--root)
       (insert ,content)
       (let ((markdown-ts-inline-images t)
             (markdown-ts-appear-enable-math-preview nil)
             (treesit-font-lock-level 4))
         (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
           (markdown-ts-mode)
           (goto-char (point-max))
           (unwind-protect
               (progn
                 (markdown-ts-appear-mode 1)
                 (font-lock-ensure)
                 ,@body)
             (when markdown-ts-appear-mode
               (markdown-ts-appear-mode -1))))))))

(defun markdown-ts-appear-image-test--previews ()
  "Return native Markdown image overlays in the current buffer."
  (seq-filter (lambda (overlay) (overlay-get overlay 'markdown-ts-image))
              (overlays-in (point-min) (point-max))))

(defun markdown-ts-appear-image-test--assert-preview ()
  "Check that one standalone image is displayed below its source link."
  (let ((previews (markdown-ts-appear-image-test--previews)))
    (should (= (length previews) 1))
    (let ((text (overlay-get (car previews) 'after-string)))
      (should (string-prefix-p "\n" text))
      (should (eq (car (get-text-property 1 'display text)) 'image)))))

(ert-deftest markdown-ts-appear-image-test-preview-survives-source-reveal ()
  (let ((source "Before\n\n![demo](examples/demo.svg)\n\nAfter\n"))
    (markdown-ts-appear-image-test--with-buffer source
      (set-buffer-modified-p nil)
      (markdown-ts-appear-image-test--assert-preview)
      (goto-char (point-min))
      (search-forward "![demo]")
      (let ((beg (- (point) (length "![demo]")))
            (end (save-excursion (search-forward ")"))))
        (dotimes (_ 3)
          (goto-char (+ beg 2))
          (markdown-ts-appear--update)
          (should-not (text-property-not-all beg end 'invisible nil))
          (markdown-ts-appear-image-test--assert-preview)
          (markdown-ts-appear-stop)
          (should (text-property-not-all beg end 'invisible nil))
          (markdown-ts-appear-image-test--assert-preview)
          (markdown-ts-appear-start)
          (should-not (text-property-not-all beg end 'invisible nil))
          (markdown-ts-appear-image-test--assert-preview)
          (goto-char (point-max))
          (markdown-ts-appear--update)
          (markdown-ts-appear-image-test--assert-preview))
        (goto-char end)
        (markdown-ts-appear--update)
        (should-not (text-property-not-all beg end 'invisible nil))
        (markdown-ts-appear-image-test--assert-preview))
      (should (equal source (buffer-string)))
      (should-not (buffer-modified-p))
      ;; Disabling appear mode restores native image previews.
      (markdown-ts-appear-mode -1)
      (markdown-ts-appear-image-test--assert-preview))))

(ert-deftest markdown-ts-appear-image-test-preserves-foreign-previews ()
  (markdown-ts-appear-image-test--with-buffer
      "![demo](examples/demo.svg)\n\n```mermaid\ngraph TD; A-->B\n```\n"
    (let* ((native (car (markdown-ts-appear-image-test--previews)))
           (foreign (make-overlay (overlay-start native) (overlay-end native)))
           (diagram (make-overlay (- (point-max) 5) (1- (point-max))))
           (preview (overlay-get native 'after-string)))
      (overlay-put foreign 'after-string preview)
      (overlay-put diagram 'after-string preview)
      (goto-char 4)
      (markdown-ts-appear--update)
      (markdown-ts-appear-image-test--assert-preview)
      (markdown-ts-appear-mode -1)
      (should (eq (overlay-get foreign 'after-string) preview))
      (should (eq (overlay-get diagram 'after-string) preview))
      (should (eq (overlay-buffer foreign) (current-buffer)))
      (should (eq (overlay-buffer diagram) (current-buffer)))
      (markdown-ts-toggle-inline-images)
      (font-lock-ensure)
      (should-not (markdown-ts-appear-image-test--previews))
      (should (overlay-buffer foreign))
      (should (overlay-buffer diagram)))))

(ert-deftest markdown-ts-appear-image-test-fold-and-delete ()
  (markdown-ts-appear-image-test--with-buffer
      "# Heading\n\n![demo](examples/demo.svg)\n\n# End\n"
    (markdown-ts-appear-image-test--assert-preview)
    (goto-char (point-min))
    (outline-hide-subtree)
    (font-lock-ensure)
    (should-not (markdown-ts-appear-image-test--previews))
    (outline-show-all)
    (font-lock-ensure)
    (markdown-ts-appear-image-test--assert-preview)
    (search-forward "![demo]")
    (delete-region (- (point) (length "![demo]")) (line-end-position))
    (font-lock-ensure)
    (should-not (markdown-ts-appear-image-test--previews))))

(ert-deftest markdown-ts-appear-image-test-cleans-changed-syntax ()
  (dolist (edit (list
                (lambda () (goto-char (point-min)) (delete-char 1))
                (lambda ()
                  (goto-char (point-min)) (insert "```\n")
                  (goto-char (point-max)) (insert "```\n"))
                (lambda ()
                  (goto-char (point-min)) (insert "`")
                  (end-of-line) (insert "`"))))
    (markdown-ts-appear-image-test--with-buffer "![demo](examples/demo.svg)\n"
      (let ((preview (car (markdown-ts-appear-image-test--previews))))
        (should preview)
        (funcall edit)
        (font-lock-ensure)
        (should-not (markdown-ts-appear-image-test--previews))
        (should-not (overlay-buffer preview))))))

(ert-deftest markdown-ts-appear-image-test-reuses-preview-and-attached-state ()
  (markdown-ts-appear-image-test--with-buffer
      "![demo](examples/demo.svg)\n\nAfter\n"
    (let* ((preview (car (markdown-ts-appear-image-test--previews)))
           (text (overlay-get preview 'after-string)))
      (overlay-put preview 'image-test-extension t)
      (cl-letf (((symbol-function 'create-image)
                 (lambda (&rest _) (ert-fail "Unchanged image was reloaded"))))
        (dotimes (_ 3)
          (goto-char 4)
          (markdown-ts-appear--update)
          (goto-char (point-max))
          (markdown-ts-appear--update)
          (should (eq preview (car (markdown-ts-appear-image-test--previews))))
          (should (eq text (overlay-get preview 'after-string))))
        (goto-char (point-min))
        (insert "Text before the image.\n\n")
        (font-lock-ensure)
        (should (eq preview (car (markdown-ts-appear-image-test--previews))))
        (should (eq text (overlay-get preview 'after-string)))
        (should (overlay-get preview 'image-test-extension))))))

(ert-deftest markdown-ts-appear-image-test-resizes-cached-previews ()
  (markdown-ts-appear-image-test--with-buffer "![demo](examples/demo.svg)\n"
    (let* ((preview (car (markdown-ts-appear-image-test--previews)))
           (image (get-text-property 1 'display (overlay-get preview 'after-string)))
           (markdown-ts-image-max-width 240))
      (cl-letf (((symbol-function 'file-attributes)
                 (lambda (&rest _) (ert-fail "Resizing checked an image file")))
                ((symbol-function 'create-image)
                 (lambda (&rest _) (ert-fail "Resizing reloaded an image")))
                ((symbol-function 'treesit-update-ranges)
                 (lambda (&rest _) (ert-fail "Resizing reparsed image sources"))))
        (markdown-ts-appear-image--refresh-views))
      (should (eq preview (car (markdown-ts-appear-image-test--previews))))
      (let ((resized (get-text-property 1 'display (overlay-get preview 'after-string))))
        (should (= (plist-get (cdr resized) :max-width) 240))
        (should (equal (plist-get (cdr resized) :file)
                       (plist-get (cdr image) :file)))))))

(ert-deftest markdown-ts-appear-image-test-decodes-local-paths-once ()
  (skip-unless (image-type-available-p 'svg))
  (let ((directory (make-temp-file "markdown-ts-appear-image-" t)))
    (unwind-protect
        (dolist (name '("图片 space.svg" "literal%20.svg" "parentheses(1).svg"))
          (let* ((file (expand-file-name name directory))
                 (url (concat directory "/" (url-hexify-string name))))
            (copy-file (expand-file-name "examples/demo.svg"
                                         markdown-ts-appear-image-test--root)
                       file)
            (dolist (destination (list url (concat "<" url ">")))
              (markdown-ts-appear-image-test--with-buffer
                  (format "![demo](%s)\n" destination)
                (markdown-ts-appear-image-test--assert-preview)
                (let* ((preview (car (markdown-ts-appear-image-test--previews)))
                       (image (get-text-property 1 'display
                                                 (overlay-get preview 'after-string))))
                  (should (equal (plist-get (cdr image) :file) file)))))))
      (delete-directory directory t))))

(ert-deftest markdown-ts-appear-image-test-updates-edited-destination ()
  (markdown-ts-appear-image-test--with-buffer
      "![demo](examples/demo.svg)\n\nAfter\n"
    (let ((preview (car (markdown-ts-appear-image-test--previews))))
      (goto-char (point-min))
      (search-forward "examples/demo.svg")
      (replace-match "test/image-tall.svg" t t)
      (font-lock-ensure)
      (should (eq preview (car (markdown-ts-appear-image-test--previews))))
      (let ((image (get-text-property 1 'display (overlay-get preview 'after-string))))
        (should (equal (plist-get (cdr image) :file)
                       (expand-file-name "test/image-tall.svg"
                                         markdown-ts-appear-image-test--root))))
      (goto-char (point-min))
      (search-forward "test/image-tall.svg")
      (replace-match "" t t)
      (font-lock-ensure)
      (should-not (markdown-ts-appear-image-test--previews)))))

(ert-deftest markdown-ts-appear-image-test-refreshes-changed-file ()
  (skip-unless (image-type-available-p 'svg))
  (let ((file (make-temp-file "markdown-ts-appear-image-" nil ".svg")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='32'/>"))
          (markdown-ts-appear-image-test--with-buffer (format "![demo](%s)\n" file)
            (let* ((preview (car (markdown-ts-appear-image-test--previews)))
                   (before (get-text-property 1 'display (overlay-get preview 'after-string))))
              (when (display-graphic-p)
                (should (equal (image-size before t) '(64 . 32))))
              (with-temp-file file
                (insert "<svg xmlns='http://www.w3.org/2000/svg' width='128' height='40'/>"))
              (font-lock-fontify-region (point-min) (point-max))
              (should (eq preview (car (markdown-ts-appear-image-test--previews))))
              (let ((after (get-text-property 1 'display (overlay-get preview 'after-string))))
                (should-not (eq before after))
                (when (display-graphic-p)
                  (should (equal (image-size after t) '(128 . 40))))))))
      (delete-file file))))

(ert-deftest markdown-ts-appear-image-test-respects-remote-policy ()
  (require 'url-handlers)
  (let ((markdown-ts-display-remote-inline-images nil)
        (downloads 0))
    (cl-letf (((symbol-function 'url-insert-file-contents)
               (lambda (&rest _)
                 (cl-incf downloads)
                 (insert "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'/>"))))
      (markdown-ts-appear-image-test--with-buffer "![demo](https://example.invalid/image.svg)\n"
        (should-not (markdown-ts-appear-image-test--previews))
        (should (= downloads 0))
        (setq markdown-ts-display-remote-inline-images 'download)
        (markdown-ts-appear-image--refresh)
        (markdown-ts-appear-image-test--assert-preview)
        (should (= downloads 1))
        (goto-char 4)
        (markdown-ts-appear--update)
        (markdown-ts-appear-image-test--assert-preview)
        (should (= downloads 1))
        (setq markdown-ts-display-remote-inline-images nil)
        (markdown-ts-appear-image--refresh)
        (should-not (markdown-ts-appear-image-test--previews))))))

(ert-deftest markdown-ts-appear-image-test-clone-keeps-base-previews ()
  (markdown-ts-appear-image-test--with-buffer "![demo](examples/demo.svg)\n"
    (let ((preview (car (markdown-ts-appear-image-test--previews)))
          (clone (clone-indirect-buffer " *image-clone*" nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (should-not markdown-ts-appear-mode)
              (should-not markdown-ts-appear-image--sources))
            (should (eq preview (car (markdown-ts-appear-image-test--previews))))
            (should (overlay-buffer preview)))
        (kill-buffer clone))
      (markdown-ts-appear-image-test--assert-preview))))

(ert-deftest markdown-ts-appear-image-test-unrelated-edit-does-not-touch-files ()
  (markdown-ts-appear-image-test--with-buffer
      "Ordinary prose.\n\n![demo](examples/demo.svg)\n\nAfter\n"
    (let ((preview (car (markdown-ts-appear-image-test--previews))))
      (cl-letf (((symbol-function 'file-attributes)
                 (lambda (&rest _) (ert-fail "Unrelated edit checked an image file"))))
        (goto-char 3)
        (insert "new text ")
        (run-hooks 'post-command-hook))
      (should (eq preview (car (markdown-ts-appear-image-test--previews)))))))

(ert-deftest markdown-ts-appear-image-test-distant-fence-and-total-deletion ()
  (markdown-ts-appear-image-test--with-buffer
      "Before\n\n![demo](examples/demo.svg)\n\nAfter\n"
    (let ((preview (car (markdown-ts-appear-image-test--previews))))
      (goto-char (point-min))
      (insert "```\n")
      (font-lock-ensure)
      (should-not (overlay-buffer preview))
      (should-not markdown-ts-appear-image--sources)
      (delete-region (point-min) (+ (point-min) 4))
      (font-lock-ensure)
      (markdown-ts-appear-image-test--assert-preview)
      (setq preview (car (markdown-ts-appear-image-test--previews)))
      (erase-buffer)
      (run-hooks 'post-command-hook)
      (should-not (overlay-buffer preview))
      (should-not markdown-ts-appear-image--sources))))

(ert-deftest markdown-ts-appear-image-test-gui-window-resize ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-image-test--with-buffer "![demo](examples/demo.svg)\n\nAfter\n"
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((second (split-window-right)))
        (set-window-buffer second (current-buffer))
        (window-resize second -8 t)
        (run-hooks 'window-configuration-change-hook)
        (let ((deadline (+ (float-time) 3)))
          (while (and markdown-ts-appear-image--resize-timer
                      (< (float-time) deadline))
            (sit-for 0.05)))
        (should-not markdown-ts-appear-image--resize-timer)
        (let ((previews (markdown-ts-appear-image-test--previews)))
          (should (= 2 (length previews)))
          (dolist (preview previews)
            (let* ((window (overlay-get preview 'window))
                   (image (get-text-property 1 'display (overlay-get preview 'after-string))))
              (should (window-live-p window))
              (should (= (plist-get (cdr image) :max-width) (window-body-width window t)))))
          (delete-window second)
          (markdown-ts-appear-image--refresh-views)
          (should (= 1 (length (markdown-ts-appear-image-test--previews)))))))))

(ert-deftest markdown-ts-appear-image-test-gui-tall-image-motion ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-image-test--with-buffer
      "Before\n\n![tall](test/image-tall.svg)\n\nAfter\n"
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (goto-char (point-min))
      (set-window-start nil (point-min))
      (redisplay t)
      (let ((target (save-excursion (search-forward "After") (line-beginning-position)))
            (line-move-visual t)
            (steps 0))
        (while (and (< (point) target) (< steps 200))
          (execute-kbd-macro (kbd "C-n"))
          (redisplay t)
          (cl-incf steps))
        (should (>= (point) target))
        (markdown-ts-appear-image-test--assert-preview)
        (setq steps 0)
        (while (and (not (bobp)) (< steps 200))
          (execute-kbd-macro (kbd "C-p"))
          (redisplay t)
          (cl-incf steps))
        (should (bobp))
        (markdown-ts-appear-image-test--assert-preview)))))

(ert-deftest markdown-ts-appear-image-test-gui-evil-insert ()
  (skip-unless (and (display-graphic-p) (require 'evil nil t)))
  (markdown-ts-appear-image-test--with-buffer
      "Before\n\n![demo](examples/demo.svg)\n\nAfter\n"
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((was-enabled evil-mode))
        (evil-mode 1)
        (add-hook 'evil-insert-state-entry-hook #'markdown-ts-appear-start nil t)
        (add-hook 'evil-insert-state-exit-hook #'markdown-ts-appear-stop nil t)
        (unwind-protect
            (progn
              (evil-normal-state)
              (markdown-ts-appear-stop)
              (goto-char (point-min))
              (search-forward "demo")
              (backward-char)
              (redisplay t)
              (markdown-ts-appear-image-test--assert-preview)
              (execute-kbd-macro "i")
              (redisplay t)
              (should markdown-ts-appear--region)
              (markdown-ts-appear-image-test--assert-preview)
              (execute-kbd-macro (kbd "<escape>"))
              (redisplay t)
              (should-not markdown-ts-appear--region)
              (markdown-ts-appear-image-test--assert-preview))
          (unless was-enabled
            (evil-mode -1)))))))

(ert-deftest markdown-ts-appear-image-test-gui-click-preview ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-image-test--with-buffer
      "Before\n\n![demo](examples/demo.svg)\n\nAfter\n"
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (goto-char (point-min))
      (set-window-start nil (point-min))
      (redisplay t)
      (let* ((preview (car (markdown-ts-appear-image-test--previews)))
             (anchor (posn-at-point (overlay-start preview)))
             (click (posn-at-x-y 20 (+ (cdr (posn-x-y anchor))
                                      (frame-char-height) 20)
                                 (selected-window))))
        (should click)
        (execute-kbd-macro (vector (list 'mouse-1 click)))
        (redisplay t)
        (should markdown-ts-appear--region)
        (should (equal (buffer-substring-no-properties
                        (car markdown-ts-appear--region)
                        (cdr markdown-ts-appear--region))
                       "![demo](examples/demo.svg)"))
        (markdown-ts-appear-image-test--assert-preview)))))

;;; markdown-ts-appear-image-test.el ends here
