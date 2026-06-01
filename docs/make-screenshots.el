;;; make-screenshots.el --- regenerate docs/img/*.png headlessly  -*- lexical-binding: t; -*-

;; Headless alternative to update-screenshots.sh: profiles a real magit
;; operation, lays out the flamegraph and a describe buffer side by side,
;; and exports the four PNGs with `x-export-frames' — cropping each pane
;; by its actual pixel geometry, so no window-manager decorations or
;; hardcoded crop offsets are involved.
;;
;; Usage (needs a graphical display; a headless Xvfb works):
;;   emacs -Q --display "$DISPLAY" -l docs/make-screenshots.el
;;
;; The profiled repo defaults to this checkout; override with
;;   FLAMEGRAPH_SHOT_REPO=/path/to/repo

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name ".." here)))
  (add-to-list 'load-path root)
  (let ((default-directory "~/.emacs.d/elpa/"))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

(require 'flamegraph)
(require 'magit)
(require 'cl-lib)

(defconst flamegraph-shot--out
  (expand-file-name "img" (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defconst flamegraph-shot--pane-width 1316)
(defconst flamegraph-shot--frame-width 2643)
(defconst flamegraph-shot--targets
  '((light . magit-insert-head-branch-header)
    (dark . magit-insert-unpushed-to-upstream)))

;;; Display settings (kept in sync with update-screenshots.sh).
(defun flamegraph-shot--set-font ()
  (set-face-attribute 'default nil
                      :height 110 :weight 'semi-light :family "Cascadia Mono"))

(flamegraph-shot--set-font)
(setq-default line-spacing 1)
(setq window-divider-default-right-width 0
      window-divider-default-bottom-width 0
      window-divider-default-places nil)
(window-divider-mode -1)
(dolist (feature '(menu tool scroll))
  (funcall (intern (format "%s-bar-mode" feature)) -1))

(defun flamegraph-shot--find (node name)
  "Depth-first search NODE's tree for the calltree whose entry is NAME."
  (or (and (eq (profiler-calltree-entry node) name) node)
      (cl-some (lambda (k) (flamegraph-shot--find k name))
               (profiler-calltree-children node))))

(defun flamegraph-shot--profile ()
  "Profile repeated `magit-refresh' and display the flame graph."
  (let* ((repo (or (getenv "FLAMEGRAPH_SHOT_REPO")
                   (expand-file-name ".." flamegraph-shot--out)))
         (default-directory (file-name-as-directory repo)))
    (magit-status-setup-buffer default-directory)
    (profiler-start 'cpu)
    (dotimes (_ 40) (magit-refresh))
    (profiler-stop)
    (flamegraph-profiler-report)))

(defun flamegraph-shot--layout (mode)
  "Build the side-by-side describe (left) and flamegraph (right) windows.
Return (DESCRIBE-WINDOW . FLAMEGRAPH-WINDOW)."
  (let ((fg-buf (get-buffer (format "*Flamegraph: %s*" "CPU"))))
    (with-current-buffer fg-buf
      (let* ((target (alist-get mode flamegraph-shot--targets))
             (node (flamegraph-shot--find flamegraph--top target)))
        (when node
          ;; Zoom to the selected frame's parent so the harness frames
          ;; (normal-top-level … flamegraph-shot--profile) drop out.
          (when-let* ((parent (profiler-calltree-parent node)))
            (flamegraph--goto-root parent))
          (flamegraph--describe-frame node flamegraph--grand-total nil
                                      flamegraph--profiler-el-calls))))
    (delete-other-windows (get-buffer-window fg-buf))
    (let* ((fg-win (get-buffer-window fg-buf))
           (help-win (split-window fg-win flamegraph-shot--pane-width 'left t)))
      (set-window-buffer help-win (help-buffer))
      (with-selected-window fg-win
        (flamegraph-redraw))
      (cons help-win fg-win))))

(defun flamegraph-shot--crop (src win out &optional width x)
  "Crop WIN's pixel region out of SRC PNG into OUT via ImageMagick.
Use WIDTH and X instead of WIN's pixel geometry when non-nil.
If the requested WIDTH extends beyond SRC, pad on the right."
  (let ((x (or x (window-pixel-left win)))
        (y (window-pixel-top win))
        (w (or width (window-pixel-width win)))
        (h (window-pixel-height win)))
    (call-process "convert" nil nil nil src "-strip"
                  "-crop" (format "%dx%d+%d+%d" w h x y)
                  "+repage" "-gravity" "West"
                  "-background" (face-background 'default nil t)
                  "-extent" (format "%dx%d" w h)
                  out)))

(defun flamegraph-shot--load-theme (theme)
  "Load THEME for a screenshot pass."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (flamegraph-shot--set-font)
  (frame-set-background-mode (selected-frame)))

(defun flamegraph-shot--shoot (mode)
  "Export both panes for MODE (light/dark)."
  (let ((frame (selected-frame))
        (tmp (expand-file-name (format "frame-%s.png" mode)
                               temporary-file-directory)))
    (flamegraph-shot--load-theme (if (eq mode 'dark) 'modus-vivendi 'modus-operandi))
    (set-frame-size frame flamegraph-shot--frame-width 1002 t)
    (let ((wins (flamegraph-shot--layout mode)))
      (redisplay t)
      (with-temp-file tmp
        (set-buffer-multibyte nil)
        (insert (x-export-frames frame 'png)))
      (flamegraph-shot--crop tmp (car wins)
                             (expand-file-name (format "describe-%s.png" mode)
                                               flamegraph-shot--out)
                             flamegraph-shot--pane-width)
      (flamegraph-shot--crop tmp (cdr wins)
                             (expand-file-name (format "flamegraph-%s.png" mode)
                                               flamegraph-shot--out)
                             flamegraph-shot--pane-width))
    (delete-file tmp)
    (message "wrote %s shots" mode)))

(make-frame-on-display (getenv "DISPLAY") '((name . "flamegraph-shot")))
(let ((frame (car (last (frame-list)))))
  (select-frame-set-input-focus frame)
  (set-frame-size frame flamegraph-shot--frame-width 1002 t))
(flamegraph-shot--profile)
(flamegraph-shot--shoot 'light)
(flamegraph-shot--shoot 'dark)
(kill-emacs 0)
