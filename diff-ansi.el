;;; diff-ansi.el --- Display diff's using alternative diffing tools -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-diff-ansi
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Support showing color diffs using external tools.

;;; Usage

;; See readme.rst.
;;

;;; Code:
(require 'ansi-color)


;; ---------------------------------------------------------------------------
;; Compatibility

(when (and (version< emacs-version "31.1") (not (and (fboundp 'incf) (fboundp 'decf))))
  (defmacro incf (place &optional delta)
    "Increment PLACE by DELTA or 1."
    (declare (debug (gv-place &optional form)))
    (gv-letplace (getter setter) place
      (funcall setter `(+ ,getter ,(or delta 1)))))
  (defmacro decf (place &optional delta)
    "Decrement PLACE by DELTA or 1."
    (declare (debug (gv-place &optional form)))
    (gv-letplace (getter setter) place
      (funcall setter `(- ,getter ,(or delta 1))))))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup diff-ansi nil
  "Configure smooth scrolling when jumping to new locations."
  :group 'scrolling)

(defcustom diff-ansi-use-magit-revision-diff t
  "Use diff-ansi for `magit-revision-mode'."
  :type 'boolean)


(defcustom diff-ansi-tool 'delta
  "Command to use for generating the diff."
  :type
  '(choice (symbol :tag "Use `delta' command." delta)
           (symbol :tag "Use `diff-so-fancy' command." diff-so-fancy)
           (symbol :tag "Use `ydiff' command." ydiff)

           (symbol :tag "Use `diff-ansi-tool-custom' command." custom)))

;; Note that this has it's values extracted and isn't used directly.
(defface diff-ansi-default-face (list (list t :foreground "black" :background "black"))
  "Face used to render black color.")

(defcustom diff-ansi-tool-custom nil
  "Command to use when `diff-ansi-tool' is set to `custom'.
This must take the diff content as the `standard-input'."
  :type '(repeat string))

;; Extra arguments for command presets.

(defcustom diff-ansi-extra-args-for-delta
  (list "--side-by-side" "--no-gitconfig" "--true-color=always" "--paging=never")
  "Additional arguments to pass to `delta'."
  :type '(repeat string))

(defcustom diff-ansi-extra-args-for-diff-so-fancy (list)
  "Additional arguments to pass to `diff-so-fancy'."
  :type '(repeat string))

(defcustom diff-ansi-extra-args-for-ydiff (list "--side-by-side")
  "Additional arguments to pass to `ydiff'."
  :type '(repeat string))

;; ---------------------------------------------------------------------------
;; Custom Variables (Advanced)

(defcustom diff-ansi-method 'multiprocess
  "Convert ANSI escape sequences."
  :type
  '(choice (symbol :tag "Convert immediately." immediate)
           (symbol :tag "Convert progressively (using a timer)." progressive)
           (symbol :tag "Convert using multiple sub-processes." multiprocess)))

(defcustom diff-ansi-chunks-size 16384
  "Number of characters to process at once.
Used for `progressive' and `multiprocess' methods."
  :type 'integer)

(defcustom diff-ansi-multiprocess-jobs nil
  "Number of simultaneous jobs to launch when using `multiprocess' method.

A nil value detects the number of processes on the system (when supported)."
  :type 'integer)

(defcustom diff-ansi-verbose-progress nil
  "When enabled, show the progress of progressive conversion in the echo area.

It can be useful to show progress when viewing very large diffs."
  :type 'boolean)

;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local diff-ansi--ansi-color-timer nil)

(defvar diff-ansi--ansi-color-bg nil)

(defconst diff-ansi--code-block-for-multiprocess-defs
  ;; NOTE:
  ;; - `diff-ansi--face-has-bg' is a bit heavy but necessary
  ;;   to ensure all faces have a black-background.
  ;; - A single `let' or `progn' is needed for the code to be properly quoted.
  (quote (progn
           (defun diff-ansi--face-has-bg (face)
             (when (and face (listp face))
               (or (plist-get face :background)
                   (diff-ansi--face-has-bg (car face))
                   (diff-ansi--face-has-bg (cdr-safe face)))))

           (defun diff-ansi--ansi-color-apply-text-property-face--local (beg end face)
             (when face
               (unless (diff-ansi--face-has-bg face)
                 (setq face
                       (cond
                        ((listp face)
                         (list face diff-ansi--ansi-color-bg))
                        (t
                         (cons face diff-ansi--ansi-color-bg)))))
               (put-text-property beg end 'face face)))

           (defun diff-ansi--ansi-color-apply-on-region-with-bg-impl (beg end)
             (let ((ansi-color-apply-face-function
                    #'diff-ansi--ansi-color-apply-text-property-face--local))
               (put-text-property beg end 'face diff-ansi--ansi-color-bg)
               (ansi-color-apply-on-region beg end))))))

;; Evaluate locally too.
(eval diff-ansi--code-block-for-multiprocess-defs)


;; ---------------------------------------------------------------------------
;; Forward Declarations

;; Needed since the the `eval' causes the function to be hidden.
(declare-function diff-ansi--ansi-color-apply-on-region-with-bg-impl "diff-ansi")
(declare-function magit-wash-sequence "magit-section")


;; ---------------------------------------------------------------------------
;; Generic Functions

(defmacro diff-ansi--with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

Advice are triplets of (SYMBOL HOW FUNCTION),
see `advice-add' documentation."
  (declare (indent 1))
  (let ((advice-list advice)
        (body-let nil)
        (body-advice-add nil)
        (body-advice-remove nil)
        (item nil))
    (unless (listp advice-list)
      (error "Advice must be a list"))
    (cond
     ((null advice-list)
      (macroexp-warn-and-return
       "An empty advice argument was found"
       `(progn
          ,@body)))
     (t
      (while (setq item (pop advice-list))
        (unless (and (listp item) (eq 3 (length item)))
          (error "Each advice must be a list of 3 items"))
        (let ((fn-sym (gensym))
              (fn-advise (pop item))
              (fn-advice-ty (pop item))
              (fn-body (pop item)))
          ;; Build the calls for each type.
          (push (list fn-sym fn-body) body-let)
          (push (list 'advice-add fn-advise fn-advice-ty fn-sym) body-advice-add)
          (push (list 'advice-remove fn-advise fn-sym) body-advice-remove)))
      (setq body-let (nreverse body-let))
      (setq body-advice-add (nreverse body-advice-add))

      ;; Compose the call.
      `(let ,body-let
         (unwind-protect
             (progn
               ,@body-advice-add
               ,@body)
           ,@body-advice-remove))))))

(defmacro diff-ansi--with-temp-echo-area (&rest body)
  "Run BODY with the message temporarily overwritten."
  `(let ((omessage (current-message)))
     (unwind-protect
         (progn
           ,@body)
       (let ((message-log-max nil))
         (message omessage)))))

(defmacro diff-ansi--with-temp-directory (name &rest body)
  "Bind NAME to the name of a new temporary file and evaluate BODY.
Delete the temporary file after BODY exits normally or non-locally.
NAME will be bound to the file name of the temporary file.

The following keyword arguments are supported:

:prefix STRING
  If non-nil, pass STRING to `make-temp-file' as the PREFIX argument.

:suffix STRING
  If non-nil, pass STRING to `make-temp-file' as the SUFFIX argument."
  (declare (indent 1) (debug (symbolp body)))
  (unless (symbolp name)
    (error "Expected name to be as symbol, found %S" (type-of name)))
  (let ((keyw nil)
        (prefix nil)
        (suffix nil)
        (extra-keywords nil))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:prefix (setq prefix (pop body)))
        (:suffix (setq suffix (pop body)))
        (_
         (push keyw extra-keywords)
         (pop body))))
    (when extra-keywords
      (error "Invalid keywords: %s" (mapconcat #'symbol-name extra-keywords " ")))
    (let ((temp-file (make-symbol "temp-file"))
          (prefix (or prefix ""))
          (suffix (or suffix "")))
      `(let* ((,temp-file (file-name-as-directory (make-temp-file ,prefix t ,suffix nil)))
              (,name (file-name-as-directory ,temp-file)))
         (unwind-protect
             (progn
               ,@body)
           (ignore-errors
             (delete-directory ,temp-file :recursive)))))))

;; See: https://emacs.stackexchange.com/a/70105/2418
(defun diff-ansi--call-process-pipe-chain (&rest args)
  "Call a chain of commands in ARGS, each argument must be a list of strings.

Additional keyword arguments may also be passed in.

:input - is used to pipe input into the first commands `standard-input'.
- nil: no input is passed in.
- string: text to be passed to the standard input.
- buffer: buffer to be passed to the standard input

:output - is used as the target for the final commands output.
- nil: output is ignored.
- t: output is returned as a string.
- string: output is written to file-name.
- buffer: output is written to the buffer."
  (declare (important-return-value nil))
  (let ((is-first t)

        ;; Keywords.
        (output nil)
        (input nil)

        ;; Iteration vars.
        (buf-src nil)
        (buf-dst nil))

    ;; Parse keywords.
    (let ((args-no-keywords nil))
      (while args
        (let ((arg-current (pop args)))
          (cond
           ((keywordp arg-current)
            (unless args
              (error "Keyword argument %S has no value!" arg-current))
            (let ((v (pop args)))
              (pcase arg-current
                (:input
                 (cond
                  ((null v)) ; No input.
                  ((eq v t)) ; String input (standard input data).
                  ((stringp v))
                  ((bufferp v)
                   (unless (buffer-live-p v)
                     (error "Input buffer is invalid %S" v)))
                  (t
                   (error "Input expected a buffer, string or nil")))
                 (setq input v))
                (:output
                 (cond
                  ((null output)) ; No output.
                  ((eq output t)) ; String output (file path).
                  ((stringp output)
                   (when (string-equal output "")
                     (error "Empty string used as file-path")))
                  ((bufferp output)
                   (unless (buffer-live-p output)
                     (error "Input buffer is invalid %S" output)))

                  (t
                   (error "Output expected a buffer, string or nil")))
                 (setq output v))

                (_ (error "Unknown argument %S" arg-current)))))
           ((listp arg-current)
            (push arg-current args-no-keywords))
           (t
            (error "Arguments must be keyword, value pairs or lists of strings, found %S = %S"
                   (type-of arg-current)
                   arg-current)))))

      (setq args (nreverse args-no-keywords)))

    ;; Setup two temporary buffers for source and destination,
    ;; looping over arguments, executing and piping contents.
    (with-temp-buffer
      (setq buf-src (current-buffer))
      (with-temp-buffer
        (setq buf-dst (current-buffer))

        ;; Loop over commands.
        (while args
          (let ((arg-current (pop args)))
            (with-current-buffer buf-dst
              (erase-buffer))
            (let* ((sentinel-called nil)
                   (buf-dst-final
                    (cond
                     ;; Last command, use output.
                     ((and (null args) (bufferp output))
                      output)
                     (t
                      buf-dst)))
                   (proc
                    (make-process
                     :name "call-process-pipe-chain"
                     :buffer buf-dst-final
                     ;; Write to the intermediate buffer or the final output.
                     :connection-type 'pipe
                     :command arg-current
                     :sentinel (lambda (_proc _msg) (setq sentinel-called t)))))

              (cond
               ;; Initially, we can only use the input argument.
               (is-first
                (cond
                 ((null input))
                 ((bufferp input)
                  (with-current-buffer input
                    (process-send-region proc (point-min) (point-max))))
                 ((stringp input)
                  (process-send-string proc input))))
               (t
                (with-current-buffer buf-src
                  (process-send-region proc (point-min) (point-max))
                  (erase-buffer))))

              (process-send-eof proc)
              (while (not sentinel-called)
                (accept-process-output))

              ;; Check exit-code.
              (let ((exit-code (process-exit-status proc)))
                (unless (zerop exit-code)
                  (error "Command exited code=%d: %S" exit-code arg-current))))

            ;; Swap source/destination buffers.
            (setq buf-src
                  (prog1 buf-dst
                    (setq buf-dst buf-src)))

            (setq is-first nil)))

        ;; Return the result.
        (cond
         ;; Ignore output.
         ((null output))
         ;; Already written into.
         ((bufferp output))
         ;; Write to the output as a file-path.
         ((stringp output)
          (with-current-buffer buf-src
            (write-region nil nil output nil 0)))
         ;; Return the output as a string.
         ((eq output t)
          ;; Since the argument was swapped, extract from the 'source'.
          (with-current-buffer buf-src
            (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun diff-ansi--call-process-parallel (args &rest keywords)
  "Return the result of commands in ARGS.

ARGS must be a list of commands (themselves a list of strings).

Optional keywords in KEYWORDS.

:idle FLOAT
  The time (in seconds) to idle between checking for each processes result.

:jobs INTEGER
  The maximum number of processes to run at once.

:output BOOLEAN / FUNCTION
  The destination for the output.

  - nil: ignores output.
  - t: returns the result as an array of strings.
  - function: runs the function with argument index & string result arguments.

`:progress-message' STRING
  When set, show progress percentage."
  (declare (important-return-value nil))
  ;; Keyword arguments.
  (let ((idle nil)
        (jobs nil)
        (output nil)
        (output-is-fn nil)
        (progress-message nil))

    ;; Parse keywords.
    (while keywords
      (let ((arg-current (pop keywords)))
        (cond
         ((keywordp arg-current)
          (unless keywords
            (error "Keyword argument %S has no value!" arg-current))
          (let ((v (pop keywords)))
            (pcase arg-current
              (:idle
               (cond
                ((null v)) ; Ignore.
                ((floatp v))
                (t
                 (error "Argument :idle expected a float, not a %S" (type-of v))))
               (setq idle v))

              (:jobs
               (cond
                ((null v)) ; Ignore.
                ((eq v t)) ; String output (result vector)
                ((symbolp v))
                (t
                 (error "Output expected nil, t or a symbol")))
               (setq jobs v))

              (:output
               (cond
                ((null v)) ; Ignore.
                ((eq v t)) ; String output (result vector)
                ((functionp v)
                 (setq output-is-fn t))
                (t
                 (error "Output expected nil, t or a symbol, not a %S" (type-of v))))
               (setq output v))

              (:progress-message
               (cond
                ((null v)) ; Ignore.
                ((stringp v)
                 (setq output-is-fn t))
                (t
                 (error "Progress-message expected nil or string, not a %S" (type-of v))))
               (setq progress-message v))

              (_ (error "Unknown argument %S" arg-current)))))
         (t
          (error "Optional arguments must be keyword, value pairs, found %S = %S"
                 (type-of arg-current)
                 arg-current)))))

    ;; Set defaults when unset.
    (unless idle
      (setq idle 0.01))

    ;; TODO: detect.
    (unless jobs
      (setq jobs
            (cond
             ((fboundp 'num-processors) ; Emacs 29+
              (* 2 (num-processors)))
             (t
              (or diff-ansi-multiprocess-jobs 64)))))

    (let* ((args-len (length args))
           (results (make-vector args-len nil))
           (procs (make-vector args-len nil))
           (jobs-pending nil)
           (count-complete 0)
           (count-running 0)
           (no-error t)

           (output-funcall-next 0)

           (filter-job-fn
            (lambda (proc str)
              (let ((id (process-get proc :my-job-id)))
                (aset results id (cons str (aref results id))))))

           (sentinel-job-fn
            (lambda (proc _msg)
              (let ((id (process-get proc :my-job-id)))
                ;; Clear the process in the array so it's never touched again.
                (aset procs id nil)
                (let ((exit-code (process-exit-status proc)))
                  (unless (zerop exit-code)
                    (setq no-error nil)
                    (error "Command exited code=%d: %S" exit-code (aref args id))))

                (delete-process proc)

                (incf count-complete)
                (decf count-running)
                (when output
                  (let ((str (mapconcat #'identity (reverse (aref results id)) "")))
                    (aset results id str)))

                (when progress-message
                  (let ((message-log-max nil))
                    (message "%s%.2f%%"
                             progress-message
                             (* 100.0 (/ (float count-complete) args-len))))))))

           (start-job-fn
            (lambda (i)
              (incf count-running)
              (let ((proc
                     (make-process
                      :name (concat "call-process-parallel" (number-to-string i))
                      ;; Write to the intermediate buffer or the final output.
                      :connection-type 'pipe
                      :command (aref args i)
                      :filter filter-job-fn
                      :sentinel sentinel-job-fn)))

                (process-put proc :my-job-id i)))))

      ;; Convert `args' to a vector for fast indexing.
      (setq args (vconcat args))

      ;; Setup indices to represent jobs that aren't complete.
      (let ((i args-len))
        (while (not (zerop i))
          (push (decf i) jobs-pending)))

      ;; Main loop to complete
      (diff-ansi--with-temp-echo-area
       (while (progn
                ;; Run every time, important to run before exiting.
                (when output-is-fn
                  (let ((str nil)
                        (is-modified nil))
                    (while (and (< output-funcall-next args-len)
                                (stringp (setq str (aref results output-funcall-next))))
                      ;; Overwrite with `t' (never to use again).
                      (funcall output output-funcall-next str)
                      (aset results output-funcall-next t)
                      (incf output-funcall-next)
                      (setq is-modified t))
                    (when is-modified

                      (redisplay))))

                ;; Check if exiting is needed.
                (and no-error (/= count-complete args-len)))

         ;; Fill the queue.
         (while (and jobs-pending (< count-running jobs))
           (funcall start-job-fn (pop jobs-pending)))

         (sleep-for idle)))

      (unless no-error
        (dotimes (i args-len)
          (let ((proc (aref procs i)))
            (when proc
              (delete-process proc)))))

      (cond
       ((eq output t)
        results)
       (t
        nil)))))

(defun diff-ansi--face-attr-to-hex-literal (face attr)
  "Return the HEX color from FACE and ATTR."
  (declare (important-return-value t))
  (apply #'format
         (cons
          "#%02x%02x%02x"
          (mapcar
           ;; Shift by -8 to map the value returned by `color values':
           ;; 0..65535 to 0..255 for `#RRGGBB` string formatting.
           (lambda (n) (ash n -8)) (color-values (face-attribute face attr))))))


;; ---------------------------------------------------------------------------
;; Diff/ANSI Wrapper

(defun diff-ansi--ansi-color-apply-on-region-with-bg (beg end)
  "Wrapper function for `ansi-color-apply-on-region', applying color in (BEG END)."
  (declare (important-return-value nil))
  (setq diff-ansi--ansi-color-bg
        (list
         :background (diff-ansi--face-attr-to-hex-literal 'diff-ansi-default-face :background)
         :extend t))
  (diff-ansi--ansi-color-apply-on-region-with-bg-impl beg end))

(defun diff-ansi--ansi-color-apply-on-region-with-bg-str (black-color)
  "Create string that can be passed to a sub-process using BLACK-COLOR."
  (declare (important-return-value t))
  (format "(progn %s %s %s %s %s)"
          "(require 'ansi-color)"
          (format "(defconst diff-ansi--ansi-color-bg (list :background \"%s\" :extend t))"
                  black-color)
          diff-ansi--code-block-for-multiprocess-defs
          "(diff-ansi--ansi-color-apply-on-region-with-bg-impl (point-min) (point-max))"
          "(prin1 (buffer-substring (point-min) (point-max)) #'external-debugging-output)"))


;; ---------------------------------------------------------------------------
;; Diff Implementations

(defun diff-ansi--command-preset-impl ()
  "Return the command to run delta."
  (declare (important-return-value t))
  (pcase diff-ansi-tool
    ('delta
     (append
      (list
       "delta" (format "--width=%d" (window-body-width (get-buffer-window (current-buffer)))))
      diff-ansi-extra-args-for-delta))
    ('diff-so-fancy (append (list "diff-so-fancy") diff-ansi-extra-args-for-diff-so-fancy))
    ('ydiff
     (append
      (list
       "ydiff" "--color=always"
       (format "--width=%d" (/ (window-body-width (get-buffer-window (current-buffer))) 2)))
      diff-ansi-extra-args-for-ydiff))
    ('custom diff-ansi-tool-custom)
    (_ (error "Unknown tool %S" diff-ansi-tool))))


;; ---------------------------------------------------------------------------
;; ANSI Conversion (Immediate)

(defun diff-ansi--immediate-impl (beg end &optional target-buf)
  "Colorize the text between BEG and END immediately.
Store the result in TARGET-BUF when non-nil."
  (declare (important-return-value nil))
  (let ((diff-command (diff-ansi--command-preset-impl))
        (diff-str (buffer-substring-no-properties beg end)))

    (unless target-buf
      (delete-region beg end)
      (goto-char beg))

    (with-current-buffer (or target-buf (current-buffer))
      (let ((inhibit-read-only t))
        (setq beg (point))
        (diff-ansi--call-process-pipe-chain diff-command :input diff-str :output (current-buffer))
        (setq end (point))

        (diff-ansi--ansi-color-apply-on-region-with-bg beg (point))))))


;; ---------------------------------------------------------------------------
;; ANSI Conversion (Progressive)

(defun diff-ansi--ansi-color-timer-cancel ()
  "Cancel the timer."
  (declare (important-return-value nil))
  (cancel-timer diff-ansi--ansi-color-timer)
  (setq diff-ansi--ansi-color-timer nil))

(defun diff-ansi-progressive-highlight-impl (buf beg range timer)
  "Callback to update colors for BUF in RANGE for TIMER.
Argument BEG is only used to calculate the progress percentage."
  (declare (important-return-value nil))
  (cond
   ((not (buffer-live-p buf))
    ;; The buffer was closed (most likely by the user) while the timer was running,  cancel it.
    (cancel-timer timer))
   ((input-pending-p)
    nil)
   (t
    (with-current-buffer buf
      (cond
       ((null diff-ansi--ansi-color-timer)
        ;; Local variables may have been cleared,
        ;; in this case use the timer passed in to this function.
        (cancel-timer timer))
       (t
        (let* ((do-redisplay nil)
               (inhibit-read-only t)
               (end (cdr range))
               (end-trailing-chars (- (buffer-size) end))
               (disp-beg (car range))
               (disp-end
                (min
                 end ; Clamp twice because `pos-eol' could exceed the value.
                 (save-excursion
                   (goto-char (min (+ disp-beg diff-ansi-chunks-size) end))
                   (pos-eol)))))
          (save-excursion
            (cond
             ((eq disp-beg disp-end)
              (when diff-ansi--ansi-color-timer
                (diff-ansi--ansi-color-timer-cancel)
                (when diff-ansi-verbose-progress
                  (message nil))))
             (t
              (let ((disp-end-mark (set-marker (make-marker) disp-end)))
                (diff-ansi--ansi-color-apply-on-region-with-bg disp-beg disp-end)

                (setq do-redisplay t)
                ;; Update the display start and actual end.
                (let ((disp-beg-next (marker-position disp-end-mark))
                      (end-next (- (buffer-size) end-trailing-chars)))

                  (setcar range disp-beg-next)
                  (setcdr range end-next)

                  (when diff-ansi-verbose-progress
                    (let ((message-log-max nil))
                      (message "diff-ansi: %2d%% complete"
                               (min (/ (* 100 (- disp-beg-next beg)) (- end-next beg))
                                    ;; Never show 100 because there is work left to do,
                                    ;; actual completion will hide the message.
                                    99)))))))))

          ;; Re-display outside the block that moves the cursor.
          (when do-redisplay
            (redisplay)))))))))

(defun diff-ansi--progressive-impl (beg end &optional target-buf)
  "Colorize the text between BEG and END using a timer.
Store the result in TARGET-BUF when non-nil."
  (declare (important-return-value nil))
  (let ((diff-command (diff-ansi--command-preset-impl))
        (diff-str (buffer-substring-no-properties beg end)))

    (unless target-buf
      (delete-region beg end)
      (goto-char beg))

    (with-current-buffer (or target-buf (current-buffer))
      ;; Potentially an existing timer will exist.
      (when diff-ansi--ansi-color-timer
        (diff-ansi--ansi-color-timer-cancel))

      (let ((inhibit-read-only t))
        (setq beg (point))
        (diff-ansi--call-process-pipe-chain diff-command :input diff-str :output (current-buffer))
        (setq end (point))

        ;; Postpone activation until the timer can take it's self as an argument.
        (diff-ansi--with-advice ((#'timer-activate :override (lambda (&rest _) nil)))
          (setq diff-ansi--ansi-color-timer (run-at-time 0.0 0.001 nil))
          (timer-set-function diff-ansi--ansi-color-timer #'diff-ansi-progressive-highlight-impl
                              (list
                               (current-buffer) beg (cons beg end) diff-ansi--ansi-color-timer)))
        (timer-activate diff-ansi--ansi-color-timer)))))


;; ---------------------------------------------------------------------------
;; ANSI Conversion (Multi-Processing)

(defun diff-ansi--multiprocess-impl (beg end &optional target-buf)
  "Colorize the text between BEG and END using multiple processes.
Store the result in TARGET-BUF when non-nil."
  (declare (important-return-value nil))
  (let ((diff-command (diff-ansi--command-preset-impl))
        (diff-str (buffer-substring-no-properties beg end))
        (per-chunk-args nil))

    (unless target-buf
      (delete-region beg end)
      (goto-char beg))

    (with-current-buffer (or target-buf (current-buffer))
      (let ((inhibit-read-only t))
        (diff-ansi--with-temp-directory temp-dir
          :prefix
          "diff-ansi"
          (with-temp-buffer
            ;; Use the temp buffer.
            (diff-ansi--call-process-pipe-chain
             diff-command
             :input diff-str
             :output (current-buffer))

            (goto-char (point-min))
            (let* ((black-color
                    (diff-ansi--face-attr-to-hex-literal 'diff-ansi-default-face :background))
                   (emacs-bin (expand-file-name invocation-name invocation-directory))
                   (emacs-eval-arg (diff-ansi--ansi-color-apply-on-region-with-bg-str black-color))

                   (point-prev (point))
                   ;; Index (for unique names.
                   (i 0))
              (while (not (eobp))
                (setq point-prev (point))
                (goto-char (min (+ (point) diff-ansi-chunks-size) (point-max)))
                (goto-char (pos-eol))

                (let ((output (concat temp-dir (number-to-string i))))
                  (write-region point-prev (point) output nil 0)
                  ;; The "site-file" can cause messages that break our logic
                  ;; which requires predictable process output, see: #4.
                  (push (list
                         emacs-bin
                         ;; Site files can generate warnings, interfering with the batch operation.
                         ;; For example a warning about a header not including lexical-binding
                         ;; will cause the command to fail entirely.
                         "--no-site-file"
                         "--no-site-lisp"
                         "--batch"
                         output
                         "--eval"
                         emacs-eval-arg)
                        per-chunk-args))
                (incf i))

              (setq per-chunk-args (nreverse per-chunk-args))))

          ;; Out of the temp buffer.
          (setq beg (point))
          (diff-ansi--call-process-parallel per-chunk-args
                                            :output (lambda (_i str) (insert (read str)))
                                            :progress-message "Diff progress: ")
          (setq end (point)))))))


;; ---------------------------------------------------------------------------
;; Internal Logic

;; Helper function (not public).
(defun diff-ansi--magit-insert-revision-diff-advice-fn (old-fn &rest args)
  "Internal function use to advise using `diff-ansi-advice-add'.

This calls OLD-FN with ARGS."
  (declare (important-return-value nil))
  (let ((point-begin (point)))
    (diff-ansi--with-advice ((#'magit-wash-sequence :override (lambda (&rest _) nil)))

      (apply old-fn args)

      (with-demoted-errors "diff-ansi: %S"
        ;; Don't do anything to the diff, it may cause problems.
        (diff-ansi-region point-begin (point-max))))))

(defun diff-ansi--enable ()
  "Enable the buffer local minor mode."
  (declare (important-return-value nil))
  (when diff-ansi-use-magit-revision-diff
    (require 'magit-diff)
    (advice-add
     'magit-insert-revision-diff
     :around #'diff-ansi--magit-insert-revision-diff-advice-fn)))

(defun diff-ansi--disable ()
  "Disable the buffer local minor mode."
  (declare (important-return-value nil))
  (when diff-ansi-use-magit-revision-diff
    (require 'magit-diff)
    (advice-remove 'magit-insert-revision-diff #'diff-ansi--magit-insert-revision-diff-advice-fn)))

;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun diff-ansi-region (beg end &rest args)
  "Convert the diff between points BEG and END into an ANSI-diff.

Optional keywords in ARGS.

`:create-buffer'
  When t, create a new buffer for the diff contents and return it.
  Note that the buffer is not made active.

`:preserve-point'
  When t, place the cursor at the relative line offset based on the location
  in the original buffer (this uses a simple method that doesn't ensure
  an exact match for the current line).

Return the buffer used to write data into on success."
  (declare (important-return-value nil))

  ;; Keyword arguments.
  (let ((create-buffer nil)
        (preserve-point nil)
        ;; End keyword arguments.

        ;; To update end after the buffer has been resized.
        (end-trailing-chars (- (buffer-size) end))

        ;; For `preserve-point'.
        (lines-to-beg nil)
        (lines-to-end nil)
        ;; For `into-buffer'.
        (target-buf nil))

    ;; Handle keyword arguments.
    (while args
      (let ((arg-current (pop args)))
        (cond
         ((keywordp arg-current)
          (unless args
            (error "Keyword argument %S has no value!" arg-current))
          (let ((v (pop args)))
            (pcase arg-current
              (:create-buffer
               (cond
                ((null v)) ; No input.
                ((eq v t)) ; OK.
                (t
                 (error "Expected `:create-buffer', to be nil or t")))
               (setq create-buffer v))

              (:preserve-point
               (cond
                ((null v)) ; No input.
                ((eq v t)) ; OK.
                (t
                 (error "Expected `:preserve-point', to be nil or t")))
               (setq preserve-point v))
              (_ (error "Unknown argument %S" arg-current)))))
         (t
          (error "All arguments must be keyword, value pairs, found %S = %S"
                 (type-of arg-current)
                 arg-current)))))

    (when preserve-point
      (setq lines-to-beg (count-lines (point) beg))
      (setq lines-to-end (count-lines (point) end)))

    (when create-buffer
      (setq target-buf (generate-new-buffer (concat (buffer-name) " (ansi-diff)")))
      (with-current-buffer target-buf
        (setq buffer-read-only t)
        (setq buffer-undo-list t)
        ;; We don't have our own mode (is it even needed?).
        ;; Just set text mode for basic navigation functionality.
        (text-mode)
        ;; Don't display fill column.
        (setq-local fill-column nil)))

    (let ((method diff-ansi-method))

      ;; Fallback to immediate when the task wont be split.
      (when (<= (- end beg) diff-ansi-chunks-size)
        (setq method 'immediate))

      (cond
       ((eq method 'immediate)
        (diff-ansi--immediate-impl beg end target-buf))
       ((eq method 'progressive)
        (diff-ansi--progressive-impl beg end target-buf))
       ((eq method 'multiprocess)
        (diff-ansi--multiprocess-impl beg end target-buf))
       (t
        (error "Unknown method: %S" method))))

    ;; Update end.
    (unless target-buf
      (setq end (- (point-max) end-trailing-chars)))

    ;; This is only an approximation, as the diff views may not be exactly compatible.
    (when preserve-point
      (with-current-buffer (or target-buf (current-buffer))
        (when target-buf
          (setq beg (point-min))
          (setq end (point-max)))
        (cond
         ((zerop lines-to-beg)
          (goto-char beg))
         ((zerop lines-to-end)
          (goto-char end))
         (t
          ;; Match the relative vertical offset.
          (let* ((lines-total-after (count-lines beg end))
                 (factor (/ (float lines-total-after) (float (+ lines-to-beg lines-to-end)))))
            (goto-char beg)
            (forward-line (truncate (* lines-to-beg factor))))))))

    (cond
     (create-buffer
      (set-buffer-modified-p nil)
      target-buf)
     (t
      (current-buffer)))))

;;;###autoload
(defun diff-ansi-buffer ()
  "Create an ANSI-diff buffer from the existing plain-diff buffer."
  (declare (important-return-value nil))
  (interactive)
  (let ((buf (diff-ansi-region (point-min) (point-max) :create-buffer t :preserve-point t)))
    (switch-to-buffer buf)))

;;;###autoload
(define-minor-mode diff-ansi-mode
  "Enable `diff-ansi' integration."
  :global
  nil
  (cond
   (diff-ansi-mode
    (diff-ansi--enable))
   (t
    (diff-ansi--disable))))

(provide 'diff-ansi)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; diff-ansi.el ends here
