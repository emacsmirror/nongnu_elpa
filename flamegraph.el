;;; flamegraph.el --- Flame graphs for Emacs's native profiler and folded stacks files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dmitry Gutov

;; Author: Dmitry Gutov <dmitry@gutov.dev>
;; Homepage: https://github.com/dgutov/emacs-flamegraph/
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
;; `M-x flamegraph-find-profile' opens a profile previously saved with
;; `profiler-report-write-profile', or a folded-stacks file.
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
;;   d               describe the frame at point
;;   mouse-2         describe the clicked frame
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
Used by \\[flamegraph-find-source] for folded stacks whose frames embed a
FILE:LINE location.  If nil, paths are resolved against the directory of
the data file the graph was loaded from."
  :type '(choice (const :tag "Data file's directory" nil) directory))

(defcustom flamegraph-call-site-threshold 0.02
  "Minimum share of a frame's samples for a callee to appear in its snippet.
Callees below this fraction (of the described frame's count) are not
highlighted in the describe-buffer source."
  :type 'float)

(defface flamegraph-call-site-hot
  '((((background light)) :background "#ff9b3d")
    (((background dark))  :background "#9c4a1c"))
  "Face for the hottest callee occurrences in a description buffer's source.")

(defface flamegraph-call-site-warm
  '((((background light)) :background "#ffd24d")
    (((background dark))  :background "#7a5817"))
  "Face for moderately hot callee occurrences in a description buffer's source.")

(defface flamegraph-call-site-cool
  '((((background light)) :background "#fff0c0")
    (((background dark))  :background "#4a3a14"))
  "Face for minor callee occurrences in a description buffer's source.")

(defun flamegraph--call-site-face (weight)
  "Return the call-site heat face for fractional WEIGHT in [0,1]."
  (cond ((>= weight 0.40) 'flamegraph-call-site-hot)
        ((>= weight 0.10) 'flamegraph-call-site-warm)
        (t                'flamegraph-call-site-cool)))

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
  (let ((max-depth depth)
        (stack (list (list node depth start))))
    (while stack
      (pcase-let ((`(,node ,depth ,start) (pop stack)))
        (let ((child-start start))
          (push (flamegraph--frame
                 :node node :depth depth
                 :start (/ (float start) total)
                 :width (/ (float (profiler-calltree-count node)) total))
                frames)
          (when (> depth max-depth) (setq max-depth depth))
          (dolist (child (profiler-calltree-children node))
            (push (list child (1+ depth) child-start) stack)
            (cl-incf child-start (profiler-calltree-count child))))))
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
  "Return ENTRY as a plain string, without stripping profiler metadata."
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

(defun flamegraph--canvas-width (win cw)
  "Return the canvas width in pixels (columns on a text terminal).
WIN is the window showing the buffer, or nil; CW is its char width."
  (max (* 40 cw)
       (or (and flamegraph-width (* flamegraph-width cw))
           (and win
                ;; Keep a spare character inside redisplay's wrap
                ;; boundary.  Otherwise right-edge stretch glyphs can
                ;; either wrap or get truncated, hiding tiny frames.
                (- (window-body-width win t) cw))
           (* 80 cw))))

(defun flamegraph--render-text (frames max-depth)
  "Draw FRAMES into the current buffer, one row per depth.
MAX-DEPTH is the deepest row."
  (let* ((win (get-buffer-window (current-buffer) t))
         (cw (if win (window-font-width win) (frame-char-width)))
         (total-px (flamegraph--canvas-width win cw))
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
  "Stack of previous view states, for `flamegraph-back'.")
(defvar-local flamegraph--forward nil
  "Stack of view states backed out of, for `flamegraph-forward'.")
(defvar-local flamegraph--grand-total 0
  "Total count of the whole graph.")
(defvar-local flamegraph--unit "samples"
  "Count unit, as a string (e.g. \"samples\" or \"bytes\").")
(defvar-local flamegraph--title ""
  "Short label for the data source, shown in the header.")
(defvar-local flamegraph--directory nil
  "Directory of the data file, for resolving relative source paths.")
(defvar-local flamegraph--profiler-el-calls nil
  "Non-nil when calltree entries come from profiler.el.")

(defun flamegraph--header ()
  "Return the header line for the current view."
  (let* ((root flamegraph--root)
         (zoomed (and root (profiler-calltree-entry root)))
         (left (format " %s · %s %s · root: %s%s"
                       flamegraph--title
                       (profiler-format-number flamegraph--grand-total)
                       flamegraph--unit
                       (if zoomed
                           (flamegraph--frame-display-name (profiler-calltree-entry root))
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

(defun flamegraph--draw (&optional point)
  "(Re)draw the flame graph in the current buffer.
If POINT is non-nil, restore point there after drawing."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (root (or flamegraph--root flamegraph--top)))
    (erase-buffer)
    (pcase-let ((`(,frames ,_total ,max-depth)
                 (flamegraph--frames root)))
      (flamegraph--render-text frames max-depth))
    (setq header-line-format (flamegraph--header))
    ;; Each row begins with a background gap, so land on the first frame
    ;; unless the caller is restoring a previous view.
    (goto-char (if point
                   (min (max point (point-min)) (point-max))
                 (if (length> flamegraph--frame-positions 0)
                     (aref flamegraph--frame-positions 0)
                   (point-min))))))

;;; Commands

(defun flamegraph--frame-at-point ()
  "Return the `flamegraph-frame' at point, or nil."
  (get-text-property (point) 'flamegraph-frame))

(defun flamegraph--view-state ()
  "Return the current root and point as a restorable view state."
  (cons flamegraph--root (point)))

(defun flamegraph--goto-root (root)
  "Switch the view to ROOT, recording the change in the zoom history."
  (unless (eq root flamegraph--root)
    (push (flamegraph--view-state) flamegraph--history)
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
  "Return to the previously viewed root and point (zoom history)."
  (interactive)
  (if (null flamegraph--history)
      (message "No earlier view")
    (push (flamegraph--view-state) flamegraph--forward)
    (let ((state (pop flamegraph--history)))
      (setq flamegraph--root (car state))
      (flamegraph--draw (cdr state)))))

(defun flamegraph-forward ()
  "Go forward to the next viewed root and point (zoom history)."
  (interactive)
  (if (null flamegraph--forward)
      (message "No later view")
    (push (flamegraph--view-state) flamegraph--history)
    (let ((state (pop flamegraph--forward)))
      (setq flamegraph--root (car state))
      (flamegraph--draw (cdr state)))))

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

(defun flamegraph-describe (&optional event)
  "Describe the frame at point or EVENT.
Shows a report: the frame's name (clickable for Emacs functions, to
their usual documentation), the sampled source line in context, the
sample counts, and buttons to the caller and callees."
  (interactive (list last-nonmenu-event))
  (when event
    (let ((posn (event-end event)))
      (when (posnp posn) (posn-set-point posn))))
  (let ((frame (flamegraph--frame-at-point)))
    (if (null frame)
        (message "No frame at point")
      (flamegraph--describe-frame
       (flamegraph-frame-node frame)
       flamegraph--grand-total flamegraph--directory
       flamegraph--profiler-el-calls))))

(defun flamegraph--entry-location (entry)
  "Extract a (FILE . LINE) source location embedded in ENTRY, or nil.
Recognizes the FILE:LINE form that profilers such as py-spy and rbspy put
in each frame (e.g. \"work (app/main.py:20)\"), and the FUNC:FILE:LINE form
that \"perf script -F +srcline\" folded stacks use (e.g.
\"redisplay_window:/path/xdisp.c:21020\")."
  (when (string-match "\\([^ ():]*\\.[^ ():]+\\):\\([0-9]+\\)" entry)
    (cons (match-string 1 entry)
          (string-to-number (match-string 2 entry)))))

(defun flamegraph--symbol-location (symbol)
  "Return (FILE . LINE) of SYMBOL's defining source, or nil.
Uses `find-function-noselect', so handles autoloads and — when the user
has set `find-function-C-source-directory' — subrs."
  (when (and (symbolp symbol) (fboundp symbol))
    (require 'find-func)
    (condition-case nil
        (let ((res (find-function-noselect symbol t)))
          (when (consp res)
            (let ((buf (car res)) (pos (cdr res)))
              (when (and (bufferp buf) (buffer-file-name buf))
                (with-current-buffer buf
                  (cons (buffer-file-name buf)
                        (line-number-at-pos pos)))))))
      (error nil))))

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
    (forward-line (1- line))
    (pulse-momentary-highlight-one-line (point))))

(defun flamegraph-find-source ()
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
                  (message "Source file not found: %s"
                           (abbreviate-file-name path)))))))
         (t (message "Cannot find definition of: %s"
                     (flamegraph--frame-display-name entry))))))))

(defun flamegraph--frame-display-name (entry)
  "Return ENTRY's name with embedded source location text stripped."
  (cond
   ((and (stringp entry)
         (string-match "\\([^ ():]*\\.[^ ():]+\\):\\([0-9]+\\)" entry))
    (string-trim-right (substring entry 0 (match-beginning 0)) "[ (:-]+"))
   ((and (stringp entry)
         (string-match "\\`\\(.+\\) - (unknown)\\'" entry))
    (match-string 1 entry))
   (t (flamegraph--entry-name entry))))

(defun flamegraph--frame-search-name (entry)
  "Return ENTRY's name for matching against source text.
This can differ from `flamegraph--frame-display-name' when a profiler
adds metadata around the source spelling of the frame."
  (cond
   ;; profiler.el: anonymous function objects are written as lambda forms.
   ((flamegraph--el-anonymous-function-p entry) "lambda")
   ;; rbspy: Ruby native frames include implementation/source metadata.
   ((and (stringp entry)
         (string-match
          "\\`\\(.+?\\) \\[c function\\] - (unknown)\\'" entry))
    (match-string 1 entry))
   (t (flamegraph--frame-display-name entry))))

(defvar-keymap flamegraph--snippet-line-map
  :doc "Keymap active on source snippet lines."
  "RET" #'flamegraph--snippet-visit-source)

(defun flamegraph--snippet-visit-source ()
  "Visit the source line described by text properties at point."
  (interactive)
  (let ((path (get-text-property (point) 'flamegraph-source-path))
        (line (get-text-property (point) 'flamegraph-source-line)))
    (when (and path line)
      (flamegraph--visit-file path line))))

(defun flamegraph--snippet-line (path n marked)
  "Format buffer line N for the snippet, marking it when MARKED."
  (let ((line (format " %s %4d  %s
"
                      (if marked "▸" " ")
                      n
                      (buffer-substring (line-beginning-position)
                                        (line-end-position)))))
    (add-text-properties
     0 (length line)
     `(keymap ,flamegraph--snippet-line-map
       flamegraph-source-path ,path
       flamegraph-source-line ,n)
     line)
    line))

(defun flamegraph--source-snippet (path line &optional node shown context)
  "Return source context around LINE of PATH as a fontified string, or nil.
The file is loaded in its major mode so the text is syntax-highlighted.
Besides CONTEXT lines on each side of the sampled LINE (which is marked),
the enclosing structural lines up to the definition are kept — each line
that is less indented than the one below it — so the nesting that leads
to the sampled line stays visible.  When NODE (a calltree) is non-nil, its
nested calls found in the enclosing defun are highlighted with a heat face
and their lines kept too (see `flamegraph--collect-nested-call-sites').
When SHOWN is non-nil, it is filled for Calls tree rendering.  Skipped
runs are elided with `⋯'.  CONTEXT defaults to 4."
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
                 (def-pos (save-excursion
                            (goto-char (point-min))
                            (forward-line (1- line))
                            (end-of-line)
                            (let ((p (point)))
                              (ignore-errors (beginning-of-defun))
                              (if (< (point) p) (point) (point-min)))))
                 (def (line-number-at-pos def-pos))
                 (def-end (save-excursion
                            (goto-char def-pos)
                            (ignore-errors (end-of-defun))
                            (if (> (point) def-pos) (point) (point-max))))
                 (regions (and node
                               (flamegraph--collect-nested-call-sites
                                node def-pos def-end shown)))
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
            ;; Lines containing a callee match.
            (dolist (r regions)
              (push (line-number-at-pos (car r)) keep))
            (setq keep (sort (delete-dups keep) #'<))
            ;; Render kept lines in order, eliding the gaps with `⋯'.
            (let ((prev nil) (out nil))
              (dolist (n keep)
                (when (and prev (> n (1+ prev)))
                  (push "      ⋯
" out))
                (goto-char (point-min))
                (forward-line (1- n))
                (ignore-errors
                  (font-lock-ensure (line-beginning-position)
                                    (line-end-position)))
                ;; Apply highlights to any regions that fall on this line
                ;; — additively over the font-lock faces.
                (let ((bol (line-beginning-position))
                      (eol (line-end-position)))
                  (dolist (r regions)
                    (pcase-let ((`(,rb ,re ,w) r))
                      (when (and (<= bol rb) (>= eol re))
                        (add-face-text-property
                         rb re (flamegraph--call-site-face w))))))
                (push (flamegraph--snippet-line path n (= n line)) out)
                (setq prev n))
              (apply #'concat (nreverse out)))))))))

(defun flamegraph--skip-through-p (entry)
  "Non-nil if ENTRY is a frame to descend through transparently when
anchoring a frame's callees to source (e.g. `if', `let', `progn').
Macros often add these special forms, so the profile records them where
the source had a macro call instead.  They also make poor anchors in
their own right: matching one by name is fairly likely to land on an
unrelated occurrence.
FIXME: `apply' and `funcall' play a similar role (e.g. a `cl-defmethod'
dispatch reaches the method body through `apply') but are not yet
treated as skip-throughs, so the walk dead-ends at them."
  (and (symbolp entry) (special-form-p entry)))

(defun flamegraph--enclosing-form-region (pos)
  "Return (BEG . END) of the form surrounding the callee name at POS.
Uses `backward-up-list' to find the parenthesized group POS sits in
and `forward-sexp' to bound it.  Returns nil if no enclosing form
exists (top level)."
  (save-excursion
    (goto-char pos)
    (condition-case nil
        (let ((beg (progn (backward-up-list) (point))))
          (cons beg (progn (forward-sexp) (point))))
      (error nil))))

(defun flamegraph--enclosing-head-form-region (pos)
  "Return enclosing form region when POS is at that form's head.
For an occurrence in `(foo ...)', POS on `foo' returns the form's
region.  For quoted or function-quoted references such as `'foo' or
`#'foo' in another call's arguments, return nil: the occurrence can
identify that frame, but it cannot anchor that frame's own callees."
  (when-let* ((region (flamegraph--enclosing-form-region pos)))
    (save-excursion
      (goto-char (1+ (car region)))
      (forward-comment (point-max))
      (and (= (point) pos) region))))

(defun flamegraph--callee-name-acceptable-p (name)
  "Whether NAME is plausibly a symbol we can usefully search in source.
Rejects empty strings and anything containing whitespace, brackets,
parens, comma, or semicolon — `\\=\\_<...\\=\\_>' below enforces real
symbol boundaries in the target buffer's syntax."
  (and (stringp name) (not (string-empty-p name))
       (not (string-match-p "[][[:space:]();,]" name))))

(defun flamegraph--el-anonymous-function-p (entry)
  "Non-nil if ENTRY is an anonymous Elisp function object."
  (and (not (symbolp entry)) (functionp entry)))

(defun flamegraph--collect-nested-call-sites (node beg end &optional shown)
  "Walk NODE's call subtree against the buffer region [BEG END],
recording each child whose display name matches `\\=\\_<NAME\\=\\_>' in
the current region as a highlight region.

Skip-through children (see `flamegraph--skip-through-p') are transparent:
not searched, never narrow the region, just descended through.  A
function match narrows the region for that child's own descendants, which
preserves structural attribution — a `(foo (bar …))' in source bounds the
search for `bar' to within the `foo' call.

Each region is (POS-BEG POS-END WEIGHT), WEIGHT being the callee's share
of NODE's count, for heat styling.  Callees below
`flamegraph-call-site-threshold' are omitted from the regions, but still
traversed.

SHOWN, if non-nil, is a hash table into which every node to display in
the \"Calls\" tree is marked: a skip-through that leads to a shown call,
a function matched in the source, or a function not in the source but
whose parent is scaffolding (a skip-through, or the described frame
itself) — i.e. a macro-concealed call of the current definition.  A
function not in the source whose parent is a *matched* call is the
callee's own body and is omitted.  An anonymous function object is searched
as `lambda', using the same source anchoring flow as named functions; if no
lambda form is found, the object itself is still shown as an unanchored
callback frame.  SHOWN ignores the threshold, so the tree may show cold
callees the snippet omits."
  (syntax-propertize end)
  (let ((total (max 1 (profiler-calltree-count node)))
        regions)
    (cl-labels
        ;; Walk N's children against REGION.  CONCEALED-OK means a child
        ;; not found in source is current-definition code hidden by a macro
        ;; (N is scaffolding), so still shown; otherwise it is N's callee
        ;; body.  Return non-nil if anything in the subtree was shown.
        ((walk (n region concealed-ok)
           (let ((any nil))
             (dolist (k (profiler-calltree-children n))
               (let* ((entry (profiler-calltree-entry k))
                      (anonymous (flamegraph--el-anonymous-function-p entry))
                      (name (flamegraph--frame-search-name entry))
                      (weight (/ (float (profiler-calltree-count k)) total)))
                 (cond
                  ((flamegraph--skip-through-p entry)
                   ;; Scaffolding: shown only if it leads to a shown call.
                   ;; It conceals current-definition code only where the
                   ;; surrounding context already does (CONCEALED-OK), not
                   ;; inside a matched callee's body.
                   (when (walk k region concealed-ok)
                     (when shown (puthash k t shown))
                     (setq any t)))
                  ((flamegraph--callee-name-acceptable-p name)
                   (save-excursion
                     (goto-char (car region))
                     (let ((re (concat "\\_<" (regexp-quote name) "\\_>"))
                           (found nil))
                       ;; A match narrows the region for this child's own
                       ;; walk, whose callee-body children are then hidden.
                       (while (re-search-forward re (cdr region) t)
                         (let ((mb (match-beginning 0))
                               (me (match-end 0)))
                           ;; Skip occurrences in strings or comments (e.g. a
                           ;; name mentioned in a docstring): they are not
                           ;; calls, and their enclosing form would escape the
                           ;; current region.  `syntax-ppss' moves point, so
                           ;; guard it.
                           (unless (save-excursion (nth 8 (syntax-ppss mb)))
                             (setq found t)
                             (when (>= weight flamegraph-call-site-threshold)
                               (push (list mb me weight) regions))
                             (when-let* ((sub (flamegraph--enclosing-head-form-region
                                               mb)))
                               ;; Search K's own calls in this call's
                               ;; arguments: from past the matched head to the
                               ;; form end, clamped to REGION.  References in
                               ;; arguments, like #'foo, identify `foo' but do
                               ;; not expose `foo''s body at that site.
                               (walk k (cons me (min (cdr sub) (cdr region)))
                                     nil)))))
                       (when (or found concealed-ok anonymous)
                         (when shown (puthash k t shown))
                         (setq any t))))))))
             any)))
      (walk node (cons beg end) t))
    (nreverse regions)))

(defun flamegraph--insert-call-tree (node total ref depth directory shown
                                      profiler-el-calls)
  "Insert NODE's children whose nodes are present in SHOWN.
A child absent from SHOWN is skipped, along with its subtree.  When SHOWN
contains descendants, render them recursively under their shown parents.
TOTAL is the grand total for the call graph; REF is the count of the
originally-described frame, used so the printed percentages add up under
that frame.  DEPTH controls the indentation.  PROFILER-EL-CALLS is passed
through to describe buttons."
  (let ((kids (sort (copy-sequence (profiler-calltree-children node))
                    (lambda (a b) (> (profiler-calltree-count a)
                                     (profiler-calltree-count b))))))
    (dolist (k kids)
      (when (gethash k shown)
        (insert (format "%7s  " (profiler-format-number
                                 (profiler-calltree-count k))))
        (insert (make-string (* 2 depth) ?\s))
        ;; Dim callees below the threshold: they are absent from the snippet
        ;; (see `flamegraph-call-site-threshold'), so mark why here.
        (flamegraph--describe-button
         k total directory
         (and profiler-el-calls
              (< (/ (float (profiler-calltree-count k)) ref)
                 flamegraph-call-site-threshold))
         profiler-el-calls)
        (insert (format "  (%s)\n"
                        (flamegraph--percent
                         (profiler-calltree-count k) ref)))
        (flamegraph--insert-call-tree k total ref (1+ depth) directory
                                      shown profiler-el-calls)))))

(defun flamegraph--describe-button (node total directory &optional dim
                                      profiler-el-calls)
  "Insert a button that describes NODE, passing context along.
With DIM non-nil, shadow the name (but keep it clickable)."
  (apply #'insert-text-button (flamegraph--frame-display-name
                               (profiler-calltree-entry node))
         'type 'help-xref
         'help-function #'flamegraph--describe-frame
         'help-args (list node total directory profiler-el-calls)
         (when dim '(face (shadow button)))))

(defun flamegraph--describe-frame (node total directory &optional profiler-el-calls)
  "Show a report for calltree NODE in the *Help* buffer.
TOTAL is the grand total for percentages; DIRECTORY resolves relative
source paths.  PROFILER-EL-CALLS means entries come from profiler.el;
source matches may add structure to the Calls tree.  Otherwise they only
affect the snippet.  Caller and callee names are buttons describing those
frames, with Help-style back/forward navigation."
  (require 'help-mode)
  (help-setup-xref (list #'flamegraph--describe-frame node total directory
                         profiler-el-calls)
                   (called-interactively-p 'interactive))
  (let* ((entry (profiler-calltree-entry node))
         (loc (cond ((stringp entry) (flamegraph--entry-location entry))
                    ((symbolp entry) (flamegraph--symbol-location entry))))
         (path (when loc
                 (if (stringp entry)
                     (flamegraph--resolve-source (car loc) directory)
                   (car loc))))
         (count (profiler-calltree-count node))
         (kids (sort (copy-sequence (profiler-calltree-children node))
                     (lambda (a b) (> (profiler-calltree-count a)
                                      (profiler-calltree-count b)))))
         (self (- count (apply #'+ 0 (mapcar #'profiler-calltree-count kids))))
         (parent (profiler-calltree-parent node))
         (shown (make-hash-table :test 'eq))
         (snippet (when loc
                    (flamegraph--source-snippet
                     path (cdr loc) node
                     (and profiler-el-calls shown)))))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (if (and (symbolp entry) (fboundp entry))
            (insert-text-button (flamegraph--frame-display-name entry)
                                'type 'help-function
                                'help-args (list entry))
          (insert (flamegraph--frame-display-name entry)))
        (when loc
          (insert "  ")
          (insert-text-button
           (format "%s:%d" (abbreviate-file-name (car loc)) (cdr loc))
           'action (lambda (_)
                     (if (file-exists-p path)
                         (flamegraph--visit-file path (cdr loc))
                       (message "Source file not found: %s"
                                (abbreviate-file-name path))))
           'follow-link t
           'help-echo "mouse-1, RET: visit this line")
          (when snippet
            (insert "\n\n" snippet)))
        (insert (format "%sSamples  %s (%s of total)    self  %s (%s)\n"
                        (if snippet "\n" "\n\n")
                        (profiler-format-number count)
                        (flamegraph--percent count total)
                        (profiler-format-number self)
                        (flamegraph--percent self total)))
        (when (and parent (profiler-calltree-entry parent))
          (insert "\nCalled from  ")
          (flamegraph--describe-button parent total directory nil
                                      profiler-el-calls)
          (insert "\n"))
        (when kids
          ;; With no profiler.el source matches for the Calls tree, fall
          ;; back to the direct children as a flat list.
          (when (zerop (hash-table-count shown))
            (dolist (k kids) (puthash k t shown)))
          (insert "\nCalls\n")
          (flamegraph--insert-call-tree node total count 0 directory
                                        shown profiler-el-calls))))))

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
  "<mouse-2>" #'flamegraph-describe
  "u"         #'flamegraph-zoom-out
  "t"         #'flamegraph-zoom-reset
  "l"         #'flamegraph-back
  "r"         #'flamegraph-forward
  "n"         #'flamegraph-next
  "TAB"       #'flamegraph-next
  "p"         #'flamegraph-previous
  "<backtab>" #'flamegraph-previous
  "d"         #'flamegraph-describe
  "f"         #'flamegraph-find-source
  "g"         #'flamegraph-redraw
  "q"         #'quit-window)

(define-derived-mode flamegraph-mode special-mode "Flamegraph"
  "Major mode for viewing a flame graph."
  (setq buffer-read-only t
        truncate-lines t
        buffer-undo-list t
        left-fringe-width 0)
  (cursor-intangible-mode 1)
  (add-hook 'post-command-hook #'flamegraph--echo nil t)
  (add-hook 'text-scale-mode-hook #'flamegraph-redraw nil t))

;;; Entry points

(defun flamegraph--show (top unit title &optional directory profiler-el-calls)
  "Display a flame graph for calltree TOP and return its buffer.
TOP is a dummy root node whose children are the outermost frames.  UNIT
names the count unit (e.g. \"samples\"); TITLE labels the data source in
the header.  DIRECTORY, if given, resolves relative source paths when
visiting a frame.  PROFILER-EL-CALLS means entries come from profiler.el."
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
            flamegraph--profiler-el-calls profiler-el-calls
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
   (if (eq (profiler-profile-type profile) 'cpu) "CPU" "Memory")
   nil t))

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

;;; File input

(defun flamegraph--read-folded-buffer ()
  "Parse folded stacks in the current buffer into a calltree root node.
Each non-empty line has the form \"FRAME;FRAME;... COUNT\", outermost
frame first, as produced by Brendan Gregg's stackcollapse-* tools.
COUNT is added to every frame along the stack."
  (let ((root (profiler-make-calltree)))
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
      (forward-line))
    root))

;;;###autoload
(defun flamegraph-find-profile (filename)
  "Display a flame graph for profile data in FILENAME.
Emacs profiler profiles and folded stacks files are detected automatically."
  (interactive (list (read-file-name "Find flame graph file: " nil nil t)))
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (if (looking-at-p "[[:space:]]*(")
        (flamegraph--profile (read (current-buffer)))
      (flamegraph--show (flamegraph--read-folded-buffer)
                        "samples"
                        (file-name-nondirectory filename)
                        (file-name-directory (expand-file-name filename))))))

(provide 'flamegraph)
;;; flamegraph.el ends here
