;;; flamegraph.el --- Flame graphs for the Emacs profiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dmitry Gutov

;; Author: Dmitry Gutov <dmitry@gutov.dev>
;; Keywords: lisp, tools
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; View profiling data as a flame graph.  Data can come from Emacs's native
;; profiler (see `profiler.el') or from a folded stacks file (the common
;; flame-graph interchange format).
;;
;; Start the profiler with `profiler-start', run the code you want to
;; measure, then `M-x flamegraph-profiler-report' to view the result.
;; `M-x flamegraph-find-profiler-report' opens a profile previously saved with
;; `profiler-report-write-profile'.  `M-x flamegraph-find-file' opens a
;; folded-stacks file.
;;
;; The graph is drawn top-down ("icicle" orientation): the outermost frame
;; (bottom of the call stack) is the top row, and the stack grows downward.
;; Each box's width is proportional to the number of samples (or bytes, for
;; memory profiles) collected while that frame was on the stack.
;;
;; In the *Flamegraph* buffer:
;;   RET / mouse-1   zoom into the frame at point (make it the new root)
;;   u               zoom back out to the parent frame
;;   t               reset the zoom to the whole graph
;;   l / r           go back / forward through the zoom history
;;   n / TAB         move to the next frame
;;   p / S-TAB       move to the previous frame
;;   d               describe the function at point
;;   f               visit the frame's source (definition or file:line)
;;   g               redraw (e.g. after resizing the window)
;;   q               quit
;;
;; The pipeline has three stages:
;;   1. A data source produces a `profiler-calltree': the profiler via
;;      `profiler-calltree-build', a folded-stacks file by direct parsing.
;;   2. `flamegraph--frames' lays that tree out into a flat list of
;;      `flamegraph-frame' records, each carrying a fractional
;;      START and WIDTH in [0,1] plus a DEPTH.
;;   3. The frames are rendered into the buffer, which you can then navigate
;;      and zoom.

;;; Code:

(require 'cl-lib)
(require 'profiler)
(require 'cursor-sensor)

(defgroup flamegraph nil
  "Flame graphs for the Emacs profiler."
  :group 'profiler
  :prefix "flamegraph-")

(defcustom flamegraph-width nil
  "Width of the flame-graph canvas, in columns.
If nil, use the width of the window displaying the buffer."
  :type '(choice (const :tag "Window width" nil) natnum))

(defcustom flamegraph-text-padding 5
  "Padding in pixels between a frame's left edge and its label.
On a text terminal a \"pixel\" is one column."
  :type 'natnum)

(defcustom flamegraph-frame-border 1
  "Width in pixels of the border stroke at each frame's left edge.
0 disables it.  Frames too narrow to spare the pixels get no border.
On a text terminal the unit is columns."
  :type 'natnum)

(defcustom flamegraph-source-directory nil
  "Directory to resolve relative source paths against when visiting a frame.
Used by \\[flamegraph-find] for folded stacks whose frames embed a
FILE:LINE location.  If nil, paths are resolved against the directory of
the data file the graph was loaded from."
  :type '(choice (const :tag "Data file's directory" nil) directory))

;;; Layout: call tree -> flat list of frames

(cl-defstruct (flamegraph-frame
               (:constructor flamegraph--frame)
               (:copier nil))
  "A single laid-out box of the flame graph.
NODE is the `profiler-calltree' node it stands for.  START and WIDTH
give its horizontal extent as fractions in [0,1] of the whole graph,
and DEPTH is its row (0 is the outermost frame)."
  node depth start width)

(defun flamegraph--collect (node depth start total frames)
  "Lay out calltree NODE and its descendants, prepending to FRAMES.
DEPTH is NODE's row.  START is its left edge and TOTAL the denominator,
both in sample/byte counts.  Return (FRAMES . MAX-DEPTH)."
  (let* ((count (profiler-calltree-count node))
         (frame (flamegraph--frame
                 :node node :depth depth
                 :start (/ (float start) total)
                 :width (/ (float count) total)))
         (max-depth depth)
         (child-start start))
    (push frame frames)
    (dolist (child (profiler-calltree-children node))
      (let ((res (flamegraph--collect
                  child (1+ depth) child-start total frames)))
        (setq frames (car res)
              max-depth (max max-depth (cdr res))))
      (cl-incf child-start (profiler-calltree-count child)))
    (cons frames max-depth)))

(defun flamegraph--frames (root)
  "Lay out the flame graph rooted at ROOT, a `profiler-calltree' node.
If ROOT is the dummy top node (its entry is nil), its children become
the depth-0 frames; otherwise ROOT itself is the single depth-0 frame.
Return a list (FRAMES TOTAL MAX-DEPTH), where TOTAL is the count the
widths are relative to."
  (let (frames (max-depth 0) (total 0))
    (if (profiler-calltree-entry root)
        ;; Zoomed in: ROOT spans the full width.
        (progn
          (setq total (max 1 (profiler-calltree-count root)))
          (let ((res (flamegraph--collect root 0 0 total nil)))
            (setq frames (car res) max-depth (cdr res))))
      ;; Full view: the dummy root's children are the outermost frames.
      (setq total (max 1 (apply #'+ (mapcar #'profiler-calltree-count
                                            (profiler-calltree-children root)))))
      (let ((start 0))
        (dolist (child (profiler-calltree-children root))
          (let ((res (flamegraph--collect child 0 start total frames)))
            (setq frames (car res)
                  max-depth (max max-depth (cdr res))))
          (cl-incf start (profiler-calltree-count child)))))
    (list frames total max-depth)))

;;; Frame appearance

(defun flamegraph--entry-name (entry)
  "Return a display name for calltree ENTRY."
  (cond ((eq entry t) "Others")
        ((eq entry '...) "...")
        ((stringp entry) entry)
        ((symbolp entry) (symbol-name entry))
        (t (require 'help-fns)
           (substring-no-properties (help-fns-function-name entry)))))

(defun flamegraph--color (name)
  "Return a warm \"hot\" background color for the frame named NAME.
The same NAME always maps to the same color, as in classic flame graphs."
  (let ((h (abs (sxhash-equal name))))
    (format "#%02x%02x%02x"
            (+ 205 (% h 50))            ; red:   205-254
            (% (/ h 50) 230)            ; green: 0-229
            (% (/ h 11500) 55))))       ; blue:  0-54

(defun flamegraph--darken (hex factor)
  "Scale each channel of \"#rrggbb\" HEX by FACTOR (0..1)."
  (format "#%02x%02x%02x"
          (round (* factor (string-to-number (substring hex 1 3) 16)))
          (round (* factor (string-to-number (substring hex 3 5) 16)))
          (round (* factor (string-to-number (substring hex 5 7) 16)))))

(defun flamegraph--percent (count total)
  "Format COUNT as a percentage of TOTAL with ~3 significant figures."
  (format "%.3g%%" (/ (* 100.0 count) total)))

(defun flamegraph--help-echo (_window object pos)
  (let ((frame (get-text-property pos 'flamegraph-frame object)))
    (when (and frame (bufferp object))
      (let* ((node (flamegraph-frame-node frame))
             (count (profiler-calltree-count node))
             (total (buffer-local-value 'flamegraph--grand-total object)))
        (format "%s  —  %s (%s of total)"
                (flamegraph--entry-name (profiler-calltree-entry node))
                (profiler-format-number count)
                (flamegraph--percent count total))))))

;;; Text renderer

;; Rows are laid out in pixels using `(space :align-to PIXEL)'.  Because
;; every frame is positioned at an absolute pixel offset, widths are exact
;; (not rounded to whole columns) and parent/child edges line up across
;; rows.  Frames abut; adjacent ones are told apart by color, and a
;; background gap is drawn only where the data leaves empty space.  On a
;; text terminal a "pixel" is one column, so the same code degrades to a
;; column layout.

(defvar-local flamegraph--frame-positions nil
  "Sorted vector of buffer positions, one per drawn frame.")

(defun flamegraph--canvas-width ()
  "Return the canvas width in pixels (columns on a text terminal)."
  (let ((cw (frame-char-width)))
    (max (* 40 cw)
         (or (and flamegraph-width (* flamegraph-width cw))
             (let ((win (get-buffer-window (current-buffer) t)))
               (and win
                    ;; Keep a spare character inside redisplay's wrap
                    ;; boundary.  Otherwise right-edge stretch glyphs can
                    ;; either wrap or get truncated, hiding tiny frames.
                    (- (window-body-width win t) cw)))
             (* 80 cw)))))

(defun flamegraph--render-text (frames max-depth)
  "Draw FRAMES into the current buffer, one row per depth.
MAX-DEPTH is the deepest row."
  (let* ((cw (frame-char-width))
         (total-px (flamegraph--canvas-width))
         (pad (if (display-graphic-p) flamegraph-text-padding 0))
         (border (if (display-graphic-p) flamegraph-frame-border 0))
         (rows (make-vector (1+ max-depth) nil))
         positions)
    ;; Bucket frames by row as pixel spans, dropping ones too narrow.
    (dolist (frame frames)
      (let ((x0 (round (* (flamegraph-frame-start frame) total-px)))
            (x1 (round (* (+ (flamegraph-frame-start frame)
                             (flamegraph-frame-width frame))
                          total-px))))
        (when (>= (- x1 x0) 1)          ; skip sub-pixel frames
          (push (list x0 x1 frame)
                (aref rows (flamegraph-frame-depth frame))))))
    (dotimes (depth (1+ max-depth))
      (let ((x 0))
        (dolist (seg (sort (aref rows depth) :key #'car))
          (pcase-let* ((`(,x0 ,x1 ,frame) seg)
                       (node (flamegraph-frame-node frame))
                       (name (flamegraph--entry-name
                              (profiler-calltree-entry node)))
                       (color (flamegraph--color name))
                       (mf (list :background color
                                 :box (list :line-width (- flamegraph-frame-border)
                                            :color "black")))
                       ;; A border that fits, except at the row's left edge
                       ;; (where it would double the window edge).
                       (bw (if (and (> x0 0) (> (- x1 x0) (* 2 border))) border 0))
                       (label (truncate-string-to-width
                               name (max 0 (/ (- x1 x0 bw pad) cw))))
                       (props (list 'face (list :background color
                                                :foreground "black")
                                    'mouse-face mf
                                    'flamegraph-frame frame
                                    'help-echo #'flamegraph--help-echo)))
            ;; Background gap only where the data leaves empty space.
            (when (> x0 x)
              (insert (propertize " " 'display `(space :align-to (,x0))
                                  'cursor-intangible t
                                  'front-sticky '(cursor-intangible)
                                  'rear-nonsticky '(cursor-intangible))))
            ;; Border stroke at the frame's left edge, when it can spare it.
            (when (> bw 0)
              (insert (propertize " "
                                  'display `(space :align-to (,(+ x0 bw)))
                                  'cursor-intangible t
                                  'front-sticky '(cursor-intangible)
                                  'rear-nonsticky '(cursor-intangible)
                                  'face (list :background (flamegraph--darken color 0.6))
                                  'flamegraph-frame frame
                                  'help-echo #'flamegraph--help-echo)))
            ;; Padding before the label.
            (when (and (> pad 0) (not (string-empty-p label)))
              (insert (propertize " "
                                  'display `(space :align-to (,(+ x0 bw pad)))
                                  'cursor-intangible t
                                  'front-sticky '(cursor-intangible)
                                  'rear-nonsticky '(cursor-intangible)
                                  'face (list :background color :foreground "black")
                                  'mouse-face mf
                                  'flamegraph-frame frame
                                  'help-echo #'flamegraph--help-echo)))
            ;; Record the frame's first tangible position (the label, or the
            ;; fill when unlabeled) for navigation.
            (push (point) positions)
            (unless (string-empty-p label)
              (insert (apply #'propertize label props)))
            (insert (apply #'propertize " "
                           'display `(space :align-to (,x1)) props))
            (setq x x1)))
        (insert "\n")))
    (setq flamegraph--frame-positions
          (vconcat (sort positions #'<)))))

;;; Buffer state and drawing

(defvar-local flamegraph--top nil
  "The full call tree: a dummy root node whose children are the
outermost frames.")
(defvar-local flamegraph--root nil
  "Currently zoomed-in calltree node, or nil for the full view.")
(defvar-local flamegraph--history nil
  "Stack of previously viewed roots, for `flamegraph-back'.")
(defvar-local flamegraph--forward nil
  "Stack of roots backed out of, for `flamegraph-forward'.")
(defvar-local flamegraph--grand-total 0
  "Total count of the whole graph.")
(defvar-local flamegraph--unit "samples"
  "Count unit, as a string (e.g. \"samples\" or \"bytes\").")
(defvar-local flamegraph--title ""
  "Short label for the data source, shown in the header.")
(defvar-local flamegraph--directory nil
  "Directory of the data file, for resolving relative source paths.")

(defun flamegraph--header ()
  "Return the header line for the current view."
  (let* ((root flamegraph--root)
         (zoomed (and root (profiler-calltree-entry root)))
         (left (format " %s · %s %s · root: %s%s"
                       flamegraph--title
                       (profiler-format-number flamegraph--grand-total)
                       flamegraph--unit
                       (if zoomed
                           (flamegraph--entry-name (profiler-calltree-entry root))
                         "all")
                       (if zoomed
                           (format " (%s)" (flamegraph--percent
                                            (profiler-calltree-count root)
                                            flamegraph--grand-total))
                         ""))))
    (concat (string-replace "%" "%%" left)
            (when zoomed
              (let ((hint "l: back · t: reset "))
                (concat
                 (propertize " " 'display
                             `(space :align-to (- right ,(string-width hint))))
                 hint))))))

(defun flamegraph--draw ()
  "(Re)draw the flame graph in the current buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (root (or flamegraph--root flamegraph--top)))
    (erase-buffer)
    (pcase-let ((`(,frames ,_total ,max-depth)
                 (flamegraph--frames root)))
      (flamegraph--render-text frames max-depth))
    (setq header-line-format (flamegraph--header))
    ;; Each row begins with a background gap, so land on the first frame.
    (goto-char (if (length> flamegraph--frame-positions 0)
                   (aref flamegraph--frame-positions 0)
                 (point-min)))))

;;; Commands

(defun flamegraph--frame-at-point ()
  "Return the `flamegraph-frame' at point, or nil."
  (get-text-property (point) 'flamegraph-frame))

(defun flamegraph--goto-root (root)
  "Switch the view to ROOT, recording the change in the zoom history."
  (unless (eq root flamegraph--root)
    (push flamegraph--root flamegraph--history)
    (setq flamegraph--forward nil
          flamegraph--root root)
    (flamegraph--draw)))

(defun flamegraph-zoom (&optional event)
  "Zoom into the frame at point (or at EVENT), making it the new root."
  (interactive (list last-nonmenu-event))
  (when (and event (not (eq event 'return)))
    (let ((posn (event-end event)))
      (when (posnp posn) (posn-set-point posn))))
  (let ((frame (flamegraph--frame-at-point)))
    (when frame
      (flamegraph--goto-root (flamegraph-frame-node frame)))))

(defun flamegraph-zoom-out ()
  "Zoom out to the parent of the current root frame."
  (interactive)
  (let* ((root flamegraph--root)
         (parent (and root (profiler-calltree-parent root))))
    (if (null root)
        (message "Already at the top level")
      ;; A parent whose entry is nil is the dummy top: go to the full view.
      (flamegraph--goto-root
       (and parent (profiler-calltree-entry parent) parent)))))

(defun flamegraph-zoom-reset ()
  "Reset the zoom to the whole graph (the top level)."
  (interactive)
  (if (null flamegraph--root)
      (message "Already at the top level")
    (flamegraph--goto-root nil)))

(defun flamegraph-back ()
  "Return to the previously viewed root (zoom history)."
  (interactive)
  (if (null flamegraph--history)
      (message "No earlier view")
    (push flamegraph--root flamegraph--forward)
    (setq flamegraph--root (pop flamegraph--history))
    (flamegraph--draw)))

(defun flamegraph-forward ()
  "Go forward to the next viewed root (zoom history)."
  (interactive)
  (if (null flamegraph--forward)
      (message "No later view")
    (push flamegraph--root flamegraph--history)
    (setq flamegraph--root (pop flamegraph--forward))
    (flamegraph--draw)))

(defun flamegraph--goto (next)
  "Move point to the next drawn frame, or the previous one if NEXT is nil."
  (let ((pt (point)) (best nil))
    (cl-loop for p across flamegraph--frame-positions do
             (when (if next
                       (and (> p pt) (or (null best) (< p best)))
                     (and (< p pt) (or (null best) (> p best))))
               (setq best p)))
    (if best
        (goto-char best)
      (message "No %s frame" (if next "next" "previous")))))

(defun flamegraph-next ()
  "Move point to the next frame."
  (interactive)
  (flamegraph--goto t))

(defun flamegraph-previous ()
  "Move point to the previous frame."
  (interactive)
  (flamegraph--goto nil))

(defun flamegraph-describe ()
  "Describe the frame at point.
For Emacs functions this shows the usual function documentation.  For
other frames it shows a report: the sampled source line in context, the
frame's sample counts, and buttons to its caller and callees."
  (interactive)
  (let ((frame (flamegraph--frame-at-point)))
    (if (null frame)
        (message "No frame at point")
      (let* ((node (flamegraph-frame-node frame))
             (entry (profiler-calltree-entry node)))
        (if (or (and (symbolp entry) (fboundp entry)) (functionp entry))
            (progn (require 'help-fns) (describe-function entry))
          (flamegraph--describe-frame
           node flamegraph--grand-total flamegraph--directory))))))

(defun flamegraph--entry-location (entry)
  "Extract a (FILE . LINE) source location embedded in ENTRY, or nil.
Recognizes the FILE:LINE form that profilers such as py-spy and rbspy put
in each frame (e.g. \"work (app/main.py:20)\")."
  (when (string-match "\\([^ ()]*\\.[^ ():]+\\):\\([0-9]+\\)" entry)
    (cons (match-string 1 entry)
          (string-to-number (match-string 2 entry)))))

(defun flamegraph--resolve-source (file directory)
  "Expand FILE against the configured source directory and return it.
A relative FILE is resolved against `flamegraph-source-directory', then
DIRECTORY, then `default-directory'."
  (expand-file-name file (or flamegraph-source-directory
                             directory
                             default-directory)))

(defun flamegraph--visit-file (path line)
  "Visit PATH at LINE in another window."
  (find-file-other-window path)
  (when (> line 0)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun flamegraph-find ()
  "Visit the source of the function of the frame at point.
For Emacs functions this jumps to the definition.  For folded stacks
whose frames embed a FILE:LINE location (as py-spy and rbspy emit), it
visits that file and line.  Note that this is the line the frame was
executing when sampled, which is somewhere in the function body, not
necessarily its definition."
  (interactive)
  (let ((frame (flamegraph--frame-at-point)))
    (if (null frame)
        (message "No frame at point")
      (let ((entry (profiler-calltree-entry
                    (flamegraph-frame-node frame))))
        (cond
         ((and (symbolp entry) (fboundp entry))
          (require 'find-func)
          (find-function-other-window entry))
         ((stringp entry)
          (let ((loc (flamegraph--entry-location entry)))
            (if (null loc)
                (message "No source location in: %s" entry)
              (let ((path (flamegraph--resolve-source
                           (car loc) flamegraph--directory)))
                (if (file-exists-p path)
                    (flamegraph--visit-file path (cdr loc))
                  (message "Source file not found: %s" path))))))
         (t (message "Cannot find definition of: %s"
                     (flamegraph--entry-name entry))))))))

(defun flamegraph--frame-display-name (entry)
  "Return ENTRY's name with any embedded FILE:LINE location stripped."
  (if (and (stringp entry)
           (string-match "\\([^ ()]*\\.[^ ():]+\\):\\([0-9]+\\)" entry))
      (string-trim-right (substring entry 0 (match-beginning 0)) "[ (-]+")
    (flamegraph--entry-name entry)))

(defun flamegraph--snippet-line (n marked)
  "Format buffer line N for the snippet, marking it when MARKED."
  (format " %s %4d  %s"
          (if marked "▸" " ")
          n
          (buffer-substring (line-beginning-position) (line-end-position))))

(defun flamegraph--source-snippet (path line &optional context)
  "Return source context around LINE of PATH as a fontified string, or nil.
The file is loaded in its major mode so the text is syntax-highlighted.
Besides CONTEXT lines on each side of the sampled LINE (which is marked),
the enclosing structural lines up to the definition are kept — each line
that is less indented than the one below it — so the nesting that leads
to the sampled line stays visible.  Skipped runs are elided with `⋯'.
CONTEXT defaults to 4."
  (setq context (or context 4))
  (when (and (file-readable-p path) (> line 0))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((buffer-file-name path)
            (enable-local-variables nil))
        (delay-mode-hooks (set-auto-mode)))
      (let ((maxline (line-number-at-pos (point-max))))
        (when (<= line maxline)
          (let* ((lo (max 1 (- line context)))
                 (hi (min maxline (+ line context)))
                 (def (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (let ((p (point)))
                          (ignore-errors (beginning-of-defun))
                          (if (< (point) p) (line-number-at-pos) 1))))
                 (keep nil))
            ;; The sampled line and its immediate context.
            (cl-loop for n from lo to hi do (push n keep))
            ;; Enclosing structural lines (each less indented than the one
            ;; below) from the sampled line up to the definition.
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line))
              (let ((min-ind (current-indentation))
                    (n line))
                (while (> n def)
                  (forward-line -1)
                  (setq n (1- n))
                  (unless (looking-at-p "[ \t]*$")
                    (let ((ind (current-indentation)))
                      (when (< ind min-ind)
                        (push n keep)
                        (setq min-ind ind)))))))
            (push def keep)
            (setq keep (sort (delete-dups keep) #'<))
            ;; Render kept lines in order, eliding the gaps with `⋯'.
            (let ((prev nil) (out nil))
              (dolist (n keep)
                (when (and prev (> n (1+ prev)))
                  (push "      ⋯" out))
                (goto-char (point-min))
                (forward-line (1- n))
                (ignore-errors
                  (font-lock-ensure (line-beginning-position)
                                    (line-end-position)))
                (push (flamegraph--snippet-line n (= n line)) out)
                (setq prev n))
              (mapconcat #'identity (nreverse out) "\n"))))))))

(defun flamegraph--describe-button (node total directory)
  "Insert a button that describes NODE, passing TOTAL and DIRECTORY along."
  (insert-text-button (flamegraph--frame-display-name
                       (profiler-calltree-entry node))
                      'type 'help-xref
                      'help-function #'flamegraph--describe-frame
                      'help-args (list node total directory)))

(defun flamegraph--describe-frame (node total directory)
  "Show a report for calltree NODE in the *Help* buffer.
TOTAL is the grand total for percentages; DIRECTORY resolves relative
source paths.  Caller and callee names are buttons describing those
frames, with Help-style back/forward navigation."
  (require 'help-mode)
  (help-setup-xref (list #'flamegraph--describe-frame node total directory)
                   (called-interactively-p 'interactive))
  (let* ((entry (profiler-calltree-entry node))
         (loc (and (stringp entry) (flamegraph--entry-location entry)))
         (path (and loc (flamegraph--resolve-source (car loc) directory)))
         (count (profiler-calltree-count node))
         (kids (sort (copy-sequence (profiler-calltree-children node))
                     (lambda (a b) (> (profiler-calltree-count a)
                                      (profiler-calltree-count b)))))
         (self (- count (apply #'+ 0 (mapcar #'profiler-calltree-count kids))))
         (parent (profiler-calltree-parent node)))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert (flamegraph--frame-display-name entry))
        (when loc
          (insert "  ")
          (insert-text-button
           (format "%s:%d" (car loc) (cdr loc))
           'action (lambda (_)
                     (if (file-exists-p path)
                         (flamegraph--visit-file path (cdr loc))
                       (message "Source file not found: %s" path)))
           'follow-link t
           'help-echo "mouse-1, RET: visit this line")
          (when-let* ((snippet (flamegraph--source-snippet path (cdr loc))))
            (insert "\n\n" snippet)))
        (insert (format "\n\nSamples  %s (%s of total)    self  %s (%s)\n"
                        (profiler-format-number count)
                        (flamegraph--percent count total)
                        (profiler-format-number self)
                        (flamegraph--percent self total)))
        (when (and parent (profiler-calltree-entry parent))
          (insert "\nCalled from  ")
          (flamegraph--describe-button parent total directory)
          (insert "\n"))
        (when kids
          (insert "\nCalls\n")
          (dolist (k kids)
            (insert (format "  %7s  " (profiler-format-number
                                       (profiler-calltree-count k))))
            (flamegraph--describe-button k total directory)
            (insert (format "  (%s)\n"
                            (flamegraph--percent
                             (profiler-calltree-count k) count)))))))))

(defun flamegraph-redraw ()
  "Redraw the flame graph, e.g. to pick up a new window width."
  (interactive)
  (flamegraph--draw))

(defvar-local flamegraph--last-echo nil
  "Last message shown by `flamegraph--echo'.")

(defun flamegraph--echo ()
  "Show information about the frame at point in the echo area.
Do nothing when the echo area already shows an unrelated message.
On an intangible gap, describe the frame point is about to relocate to,
since this runs before that relocation happens."
  (let ((current (current-message))
        (frame (or (flamegraph--frame-at-point)
                   (let ((pos (and (bound-and-true-p cursor-intangible-mode)
                                   (ignore-errors
                                     (cursor-sensor-tangible-pos
                                      (point) (selected-window))))))
                     (and pos (get-text-property pos 'flamegraph-frame))))))
    (when (and frame (or (null current) (equal current flamegraph--last-echo)))
      (let* ((node (flamegraph-frame-node frame))
             (count (profiler-calltree-count node))
             (msg (format "%s  %s %s  (%s of total)"
                          (flamegraph--entry-name (profiler-calltree-entry node))
                          (profiler-format-number count)
                          flamegraph--unit
                          (flamegraph--percent count flamegraph--grand-total))))
        (setq flamegraph--last-echo msg)
        (message "%s" msg)))))

;;; Mode

(defvar-keymap flamegraph-mode-map
  :doc "Keymap for `flamegraph-mode'."
  "RET"       #'flamegraph-zoom
  "<mouse-1>" #'flamegraph-zoom
  "u"         #'flamegraph-zoom-out
  "t"         #'flamegraph-zoom-reset
  "l"         #'flamegraph-back
  "r"         #'flamegraph-forward
  "n"         #'flamegraph-next
  "TAB"       #'flamegraph-next
  "p"         #'flamegraph-previous
  "<backtab>" #'flamegraph-previous
  "d"         #'flamegraph-describe
  "f"         #'flamegraph-find
  "g"         #'flamegraph-redraw
  "q"         #'quit-window)

(define-derived-mode flamegraph-mode special-mode "Flamegraph"
  "Major mode for viewing a flame graph."
  (setq buffer-read-only t
        truncate-lines t
        buffer-undo-list t
        left-fringe-width 0)
  (cursor-intangible-mode 1)
  (add-hook 'post-command-hook #'flamegraph--echo nil t))

;;; Entry points

(defun flamegraph--show (top unit title &optional directory)
  "Display a flame graph for calltree TOP and return its buffer.
TOP is a dummy root node whose children are the outermost frames.  UNIT
names the count unit (e.g. \"samples\"); TITLE labels the data source in
the header.  DIRECTORY, if given, resolves relative source paths when
visiting a frame."
  (let ((buffer (get-buffer-create (format "*Flamegraph: %s*" title))))
    (with-current-buffer buffer
      (flamegraph-mode)
      (profiler-calltree-sort top #'profiler-calltree-count>)
      (setq flamegraph--top top
            flamegraph--root nil
            flamegraph--history nil
            flamegraph--forward nil
            flamegraph--unit unit
            flamegraph--title title
            flamegraph--directory directory
            flamegraph--grand-total
            (max 1 (apply #'+ (mapcar #'profiler-calltree-count
                                      (profiler-calltree-children top))))))
    ;; Display first, then draw, so the canvas spans the actual window.
    (pop-to-buffer buffer)
    (set-window-fringes (selected-window) 0 nil)
    (flamegraph--draw)
    buffer))

(defun flamegraph--profile (profile)
  "Display a flame graph for PROFILE, a `profiler-profile' object."
  (flamegraph--show
   (profiler-calltree-build (profiler-profile-log profile))
   (if (eq (profiler-profile-type profile) 'memory) "bytes" "samples")
   (if (eq (profiler-profile-type profile) 'cpu) "CPU" "Memory")))

;;;###autoload
(defun flamegraph-profiler-report ()
  "Display a flame graph of the current profiler results.
Uses the CPU profile if one was recorded, otherwise the memory profile."
  (interactive)
  (when (and (fboundp 'profiler-cpu-running-p) (profiler-cpu-running-p))
    (setq profiler-cpu-log (profiler-cpu-log)))
  (when (profiler-memory-running-p)
    (setq profiler-memory-log (profiler-memory-log)))
  (cond (profiler-cpu-log
         (flamegraph--profile (profiler-cpu-profile)))
        (profiler-memory-log
         (flamegraph--profile (profiler-memory-profile)))
        (t (user-error "No profiler run recorded"))))

;;;###autoload
(defun flamegraph-find-profiler-report (filename)
  "Display a flame graph for the profile saved in FILENAME."
  (interactive (list (read-file-name "Find profile: " default-directory)))
  (flamegraph--profile (profiler-read-profile filename)))

;;; Folded stacks

(defun flamegraph--read-folded (filename)
  "Parse folded stacks in FILENAME into a calltree root node.
Each non-empty line has the form \"FRAME;FRAME;... COUNT\", outermost
frame first, as produced by Brendan Gregg's stackcollapse-* tools.
COUNT is added to every frame along the stack."
  (let ((root (profiler-make-calltree)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match "\\`\\(.+\\) \\([0-9]+\\)\\'" line)
            (let ((node root)
                  (count (string-to-number (match-string 2 line))))
              (dolist (frame (split-string (match-string 1 line) ";" t))
                (let ((children (profiler-calltree-children node))
                      (child nil))
                  (while children
                    (if (equal (profiler-calltree-entry (car children)) frame)
                        (setq child (car children) children nil)
                      (setq children (cdr children))))
                  (unless child
                    (setq child (profiler-make-calltree :entry frame :parent node))
                    (push child (profiler-calltree-children node)))
                  (cl-incf (profiler-calltree-count child) count)
                  (setq node child))))))
        (forward-line)))
    root))

;;;###autoload
(defun flamegraph-find-file (filename)
  "Display a flame graph for the folded stacks in FILENAME.
This is the interchange format produced by Brendan Gregg's
stackcollapse-*.pl tools and read by flamegraph.pl: one line per stack,
frames separated by \";\" (outermost first), then a space and a count."
  (interactive (list (read-file-name "Folded stacks file: " nil nil t)))
  (flamegraph--show (flamegraph--read-folded filename)
                    "samples"
                    (file-name-nondirectory filename)
                    (file-name-directory (expand-file-name filename))))

(provide 'flamegraph)
;;; flamegraph.el ends here
