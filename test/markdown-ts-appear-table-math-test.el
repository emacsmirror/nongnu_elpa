;;; markdown-ts-appear-table-math-test.el --- Inline table math tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'markdown-ts-appear)

(defconst markdown-ts-appear-table-math-test--image
  '(image :type svg :data "<svg xmlns='http://www.w3.org/2000/svg' width='13' height='16'><rect width='13' height='16'/></svg>" :width 13))

(defmacro markdown-ts-appear-table-math-test--buffer (text renderer &rest body)
  "Show TEXT using RENDERER as the math backend, then evaluate BODY."
  (declare (indent 2))
  `(progn
     (skip-unless (require 'latex-to-svg-for-markdown nil t))
     (let ((buffer (generate-new-buffer " *table-math-test*"))
           (markdown-ts-inline-images nil)
           (markdown-ts-appear-enable-math-preview t)
           (markdown-ts-appear-diagram-renderers nil))
       (unwind-protect
           (save-window-excursion
             (switch-to-buffer buffer)
             (insert ,text)
             (markdown-ts-mode)
             (setq-local latex-to-svg-frontend-number-equations nil
                         latex-to-svg-frontend-reconcile-idle nil)
             (goto-char (point-max))
             (cl-letf (((symbol-function 'latex-to-svg-backend) ,renderer)
                       ((symbol-function 'latex-to-svg-backend-engine-used) (lambda (&rest _) 'latex))
                       ((symbol-function 'latex-to-svg-frontend--graphical-p) (lambda () t))
                       ((symbol-function 'latex-to-svg-frontend--appearance) (lambda (&rest _) '("#000000" "#ffffff" 20)))
                       ((symbol-function 'display-images-p) (lambda (&rest _) t))
                       ((symbol-function 'window-font-width) (lambda (&rest _) 8))
                       ((symbol-function 'markdown-ts-appear-table--window-width) (lambda (_) 80))
                       ((symbol-function 'image-size)
                        (lambda (image &rest _) (cons (or (image-property image :width) 13) 16))))
               (markdown-ts-appear-mode 1)
               (markdown-ts-appear-stop)
               (font-lock-ensure)
               ,@body))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (markdown-ts-appear-mode -1))
           (kill-buffer buffer))))))

(defun markdown-ts-appear-table-math-test--row ()
  "Return the wrapped row at point."
  (seq-find (lambda (ov) (overlay-get ov 'markdown-ts-appear-table--wrapped)) (overlays-at (point))))

(defun markdown-ts-appear-table-math-test--flush ()
  "Run the pending table refresh without waiting for an idle timer."
  (when-let* ((timer markdown-ts-appear-table--resize-timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))))

(ert-deftest markdown-ts-appear-table-math-test-images-reveal-and-cached-motion ()
  (dolist (policy '(row table))
    (let ((calls 0) (markdown-ts-appear-table-reveal policy))
      (markdown-ts-appear-table-math-test--buffer
          "| A | B |\n|---|---|\n| x | before $x^2$ after |\n\nAfter\n"
          (lambda (&rest _) (cl-incf calls) markdown-ts-appear-table-math-test--image)
        (goto-char (point-min)) (search-forward "$x^2$") (backward-char 5)
        (let* ((beg (point)) (row (markdown-ts-appear-table-math-test--row))
               (text (overlay-get row 'before-string))
               (index (string-search "\uFFFC" text)))
          (should (equal (overlay-get row 'display) ""))
          (should (= 2 (get-text-property index 'markdown-ts-appear-table--columns text)))
          (should (= 13 (image-property (get-text-property index 'display text) :width)))
          (run-hooks 'post-command-hook)
          (should (overlay-get (cadr markdown-ts-appear-table--cursor-overlays) 'before-string))
          (let* ((layout (markdown-ts-appear-table--layout row))
                 (map (markdown-ts-appear-table--layout-map layout))
                 (position (aref map (- beg (overlay-start row)))))
            (dotimes (offset 5)
              (should (eq position (aref map (+ offset (- beg (overlay-start row))))))))
          (let ((before calls))
            (cl-letf (((symbol-function 'markdown-ts-appear-table--tables)
                       (lambda (&rest _) (ert-fail "Motion rescanned tables")))
                      ((symbol-function 'markdown-ts-appear-table--render-table)
                       (lambda (&rest _) (ert-fail "Motion rebuilt a layout"))))
              (dotimes (index 20)
                (goto-char (if (cl-evenp index) beg (+ beg 5)))
                (run-hooks 'post-command-hook)))
            (should (= before calls)))
          (goto-char beg)
          (markdown-ts-appear-start) (run-hooks 'post-command-hook)
          (should-not (overlay-get row 'before-string))
          (should-not (overlay-get row 'display))
          (should-not markdown-ts-appear-table--cursor-overlays)
          (markdown-ts-appear-stop) (run-hooks 'post-command-hook)
          (should (overlay-get (cadr markdown-ts-appear-table--cursor-overlays) 'before-string))
          (goto-char (point-max)) (run-hooks 'post-command-hook)
          (should (overlay-get row 'before-string)))))))

(ert-deftest markdown-ts-appear-table-math-test-async-updates-only-owning-table ()
  (let (ready callback)
    (markdown-ts-appear-table-math-test--buffer
        "| A | B |\n|---|---|\n| x | $x$ |\n\nBetween\n\n| C | D |\n|---|---|\n| y | plain |\n\nAfter\n"
        (lambda (_source &rest options)
          (if ready markdown-ts-appear-table-math-test--image
            (setq callback (plist-get options :callback)) nil))
      (should callback)
      (goto-char (point-min)) (search-forward "plain")
      (let ((other (markdown-ts-appear-table-math-test--row)))
        (setq ready t)
        (funcall callback nil)
        (should (overlay-buffer other))
        (should markdown-ts-appear-table--dirty)
        (markdown-ts-appear-table-math-test--flush)
        (should (overlay-buffer other))
        (should-not markdown-ts-appear-table--dirty)
        (goto-char (point-min)) (search-forward "$x$")
        (should (overlay-get (markdown-ts-appear-table-math-test--row) 'before-string))))))

(ert-deftest markdown-ts-appear-table-math-test-failed-and-display-math-stay-source ()
  (markdown-ts-appear-table-math-test--buffer
      "| A | B |\n|---|---|\n| bad | \\(x_1\\) |\n| display | $$x$$ |\n| code | `$x$` |\n\nAfter\n"
      (lambda (&rest _) nil)
    (goto-char (point-min)) (search-forward "bad")
    (should (string-search "\\(x_1\\)" (overlay-get (markdown-ts-appear-table-math-test--row) 'display)))
    (search-forward "display")
    (should (string-search "$$x$$" (overlay-get (markdown-ts-appear-table-math-test--row) 'display)))
    (should-not (seq-some (lambda (ov) (overlay-get ov 'markdown-ts-appear-table--rich))
                          markdown-ts-appear-table--overlays))))

(ert-deftest markdown-ts-appear-table-math-test-image-fit-and-mode-disable ()
  (markdown-ts-appear-table-math-test--buffer
      "| A | B |\n|---|---|\n| x | $x$ |\n\nAfter\n"
      (lambda (&rest _)
        (let ((image (copy-sequence markdown-ts-appear-table-math-test--image)))
          (setf (image-property image :width) 800)
          image))
    (markdown-ts-appear-table--render 20)
    (goto-char (point-min)) (search-forward "$x$")
    (let* ((row (markdown-ts-appear-table-math-test--row))
           (text (overlay-get row 'before-string))
           (index (string-search "\uFFFC" text))
           (columns (get-text-property index 'markdown-ts-appear-table--columns text)))
      (should (<= columns 12))
      (should (= (* columns 8) (image-property (get-text-property index 'display text) :width)))
      (latex-to-svg-for-markdown-mode -1)
      (markdown-ts-appear-table-math-test--flush)
      (should-not (seq-some (lambda (ov) (overlay-get ov 'markdown-ts-appear-table--rich))
                            markdown-ts-appear-table--overlays)))))

(ert-deftest markdown-ts-appear-table-math-test-failed-edit-drops-old-image ()
  (let (callback)
    (markdown-ts-appear-table-math-test--buffer
        "| A | B |\n|---|---|\n| x | $x$ |\n\nAfter\n"
        (lambda (source &rest options)
          (if (equal source "$x$") markdown-ts-appear-table-math-test--image
            (setq callback (plist-get options :callback)) nil))
      (goto-char (point-min)) (search-forward "$x")
      (markdown-ts-appear-start) (run-hooks 'post-command-hook)
      (delete-char -1) (insert "bad") (run-hooks 'post-command-hook)
      (goto-char (point-max)) (run-hooks 'post-command-hook)
      (should callback)
      (funcall callback '(:kind compile :engine latex :message "Invalid formula"))
      (markdown-ts-appear-table-math-test--flush)
      (goto-char (point-min)) (search-forward "$bad$")
      (let ((row (markdown-ts-appear-table-math-test--row)))
        (should-not (overlay-get row 'before-string))
        (should (string-search "$bad$" (overlay-get row 'display)))))))

(ert-deftest markdown-ts-appear-table-math-test-late-result-after-disable ()
  (let (callback)
    (markdown-ts-appear-table-math-test--buffer
        "| A | B |\n|---|---|\n| x | $x$ |\n\nAfter\n"
        (lambda (_source &rest options) (setq callback (plist-get options :callback)) nil)
      (should callback)
      (markdown-ts-appear-mode -1)
      (funcall callback nil)
      (should-not markdown-ts-appear-table--overlays)
      (should-not markdown-ts-appear-table--resize-timer)
      (should-not (advice-member-p #'markdown-ts-appear-math--image-shown
                                  'latex-to-svg-frontend--show-image)))))

(ert-deftest markdown-ts-appear-table-math-test-per-window-sizing-reuses-image ()
  (let ((calls 0))
    (markdown-ts-appear-table-math-test--buffer
        "| A | B |\n|---|---|\n| x | $x$ |\n\nAfter\n"
        (lambda (&rest _)
          (cl-incf calls)
          (let ((image (copy-sequence markdown-ts-appear-table-math-test--image)))
            (setf (image-property image :width) 800)
            image))
      (let ((left (selected-window)) (right (split-window-right)) (before calls))
        (set-window-buffer right (current-buffer))
        (cl-letf (((symbol-function 'markdown-ts-appear-table--window-width)
                   (lambda (window) (if (eq window left) 40 20))))
          (markdown-ts-appear-table--render)
          (let ((sizes (mapcar
                        (lambda (window)
                          (let* ((row (seq-find
                                       (lambda (ov)
                                         (and (eq (overlay-get ov 'window) window)
                                              (overlay-get ov 'markdown-ts-appear-table--rich)))
                                       markdown-ts-appear-table--overlays))
                                 (text (overlay-get row 'before-string))
                                 (image (get-text-property (string-search "\uFFFC" text) 'display text)))
                            (image-property image :width)))
                        (list left right))))
            (should (> (car sizes) (cadr sizes)))
            (should (<= (cadr sizes) (* 12 8))))
          (should (= before calls)))))))

(ert-deftest markdown-ts-appear-table-math-test-gui-pixels-and-cursor ()
  (skip-unless (and (display-graphic-p) (require 'latex-to-svg-for-markdown nil t)))
  (let ((buffer (generate-new-buffer " *table-math-gui*"))
        (image (create-image (image-property markdown-ts-appear-table-math-test--image :data) 'svg t :scale 1))
        (markdown-ts-appear-enable-math-preview t)
        (markdown-ts-inline-images nil))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "| Name | Math |\n|---|---|\n| value | before $x$ after |\n| pair | \\(x\\)\\(y\\) |\n\nAfter\n")
          (markdown-ts-mode)
          (setq-local latex-to-svg-frontend-number-equations nil
                      latex-to-svg-frontend-reconcile-idle nil)
          (goto-char (point-max))
          (cl-letf (((symbol-function 'latex-to-svg-backend) (lambda (&rest _) image)))
            (markdown-ts-appear-mode 1) (markdown-ts-appear-stop)
            (font-lock-ensure) (markdown-ts-appear-table--post-command) (redisplay t)
            (goto-char (point-min)) (search-forward "$x$") (backward-char 3)
            (let* ((beg (point)) (row (markdown-ts-appear-table-math-test--row))
                   (display (overlay-get row 'before-string))
                   (line (car (string-lines display)))
                   (columns (cl-reduce #'+ (mapcar #'markdown-ts-appear-table--width
                                                  (markdown-ts-appear-table--glyphs line))))
                   (unit (window-font-width nil 'markdown-ts-appear-table)))
              ;; Check actual pixels: nested images in an overlay display string
              ;; would have the wrong width even if all Lisp metadata looked right.
              (should (= (* columns unit)
                         (car (window-text-pixel-size nil (overlay-start row) (overlay-end row)))))
              (run-hooks 'post-command-hook) (redisplay t)
              (let* ((layout (markdown-ts-appear-table--layout row))
                     (entry (aref (markdown-ts-appear-table--layout-map layout)
                                  (- beg (overlay-start row)))))
                (should (= (* unit (markdown-ts-appear-table--position-column entry))
                           (aref (window-cursor-info) 1))))
              (execute-kbd-macro (kbd "C-f"))
              (run-hook-with-args 'pre-redisplay-functions (selected-window))
              (should (= (point) (+ beg 3)))
              (execute-kbd-macro (kbd "C-b"))
              (run-hook-with-args 'pre-redisplay-functions (selected-window))
              (should (= (point) beg)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (markdown-ts-appear-mode -1))
        (kill-buffer buffer)))))

(provide 'markdown-ts-appear-table-math-test)
;;; markdown-ts-appear-table-math-test.el ends here
