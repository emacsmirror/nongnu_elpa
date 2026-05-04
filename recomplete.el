;;; recomplete.el --- Immediately (re)complete actions -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2020  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-recomplete
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (with-command-redo "0.1"))

;;; Commentary:

;; This package provides mechanism for immediate completion, running again cycles over options.

;;; Usage:

;;
;; A key can be bound to a completion action,
;; to cycle in the reverse direction you can either press \\[keyboard-quit]
;; which causes the next completion to move in the opposite direction
;; or bind a key to cycle backward.
;;
;; Example:
;;
;;   ;; Map Alt-P to correct the current word.
;;   (global-set-key (kbd "M-p") 'recomplete-ispell-word)

;;; Code:

(eval-when-compile
  (require 'seq))

;; TODO: make this lazy load (not everyone needs to use).
(require 'dabbrev)

(declare-function with-command-redo-fn "with-command-redo")


;; ---------------------------------------------------------------------------
;; Compatibility

(eval-when-compile
  (when (version< emacs-version "31.1")
    (defmacro incf (place &optional delta)
      "Increment PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(+ ,getter ,(or delta 1)))))
    (defmacro decf (place &optional delta)
      "Decrement PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(- ,getter ,(or delta 1)))))))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup recomplete nil
  "Extensible, immediate completion utility."
  :group 'tools)

(defcustom recomplete-single-line-display t
  "Display completion options to a single line, centered around the current item."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Implementation: Utilities

(defmacro recomplete--with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

Each advice is a triplet of (SYMBOL HOW FUNCTION),
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

(defmacro recomplete--with-messages-as-list (message-list &rest body)
  "Run BODY adding any message call to the MESSAGE-LIST list."
  (declare (indent 1))
  `(let ((temp-message-list (list)))
     (recomplete--with-advice
         ((#'message
           :override
           (lambda (&rest args)
             ;; Only check if non-null because this is a signal not to log at all.
             (when message-log-max
               (push (apply #'format-message args) temp-message-list)))))
       (unwind-protect
           (progn
             ,@body)
         ;; Protected.
         (setq ,message-list (append ,message-list (nreverse temp-message-list)))))))

(defun recomplete--rotate-list-by-elt-and-remove (seq elt)
  "Split SEQ at ELT, removing it, so the elements after it are positioned first."
  (declare (important-return-value t))
  (let ((p (seq-position seq elt)))
    (cond
     ((null p)
      (seq-subseq seq 0))
     ((zerop p)
      (seq-subseq seq 1))
     (t
      (append (seq-subseq seq (1+ p)) (seq-subseq seq 0 p))))))

(defun recomplete--rotate-list-by-elt (seq elt)
  "Split SEQ at ELT, so the elements after it are positioned first."
  (declare (important-return-value t))
  (let ((p (seq-position seq elt)))
    (cond
     ((null p)
      (seq-subseq seq 0))
     (t
      (setq p (mod (1+ p) (length seq)))
      (append (seq-subseq seq p) (seq-subseq seq 0 p))))))

(defun recomplete--build-case-style-choices (word-init do-kebab-case)
  "Build a rotated case-style choice list for WORD-INIT.
When DO-KEBAB-CASE is non-nil, kebab-case (`-' as separator) is included
in the cycle alongside snake_case; otherwise only snake_case is used.

The returned list places WORD-INIT at index `len-1' (when present) so
cycling forward returns to it after a full pass."
  (declare (important-return-value t))
  (let ((word-split
         (mapcar
          #'downcase
          ;; `split-string' modified match-data.
          (save-match-data
            ;; Setting `case-fold-search' is needed for replace to work properly, see #2.
            (split-string (string-trim (let ((case-fold-search nil))
                                         (replace-regexp-in-string
                                          "\\([[:lower:]]\\)\\([[:upper:]]\\)"
                                          "\\1_\\2"
                                          word-init))
                                       "_")
                          (cond
                           (do-kebab-case
                            "[_\\-]")
                           (t
                            "_"))))))
        (result-choices nil))

    (push (string-join (mapcar #'capitalize word-split) "") result-choices)

    (cond
     ;; Single word, just add lower-case.
     ((null (cdr word-split))
      (push (car word-split) result-choices))
     ;; Multiple words.
     (t
      (dolist (ch
               (cond
                (do-kebab-case
                 (list ?- ?_))
                (t
                 (list ?_))))
        (push (string-join word-split (char-to-string ch)) result-choices))))

    (recomplete--rotate-list-by-elt result-choices word-init)))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun recomplete--on-other-command (fn-cache)
  "Handler for non-modifying external commands during a recomplete chain.
FN-CACHE is the recomplete adapter's cache.
Return non-nil to break the chain."
  (declare (important-return-value t))
  (cond
   ((eq this-command 'keyboard-quit)
    ;; Toggle reverse direction, keep chain alive.
    (let ((cycle-reverse (null (alist-get 'cycle-reverse fn-cache))))
      (setf (alist-get 'cycle-reverse fn-cache) cycle-reverse)
      ;; Re-display the message.
      (let ((msg-prefix
             (cond
              (cycle-reverse
               "<")
              (t
               ">")))
            (msg-text (alist-get 'msg-text fn-cache)))
        ;; Don't log because it's not useful to keep the selection.
        (let ((message-log-max nil))
          (message "%s%s" msg-prefix msg-text))))
    nil)
   (t
    t)))

(defun recomplete--format-and-display (result-choices cycle-index cycle-reverse)
  "Format RESULT-CHOICES with CYCLE-INDEX highlighted and display.
CYCLE-REVERSE determines the direction prefix.
Return the formatted message text."
  (declare (important-return-value t))
  (let ((msg-prefix
         (cond
          (cycle-reverse
           "<")
          (t
           ">")))

        ;; Notes on formatting output:
        ;; - Use double spaces as a separator even though it uses more room because:
        ;;   - Words may visually run together without this.
        ;;   - Some completions may contain spaces.
        ;; - Formatting ensures text doesn't *move* when the active item changes.
        (msg-text
         (string-join (let ((iter-index 0))
                        (mapcar
                         (lambda (iter-word)
                           (prog1 (cond
                                   ((eq iter-index cycle-index)
                                    (format "[%s]" (propertize iter-word 'face 'match)))
                                   (t
                                    (format " %s " iter-word)))
                             (incf iter-index)))
                         result-choices))
                      "")))

    ;; Single line display.
    (when recomplete-single-line-display
      (with-current-buffer (window-buffer (minibuffer-window))
        (let ((msg-width (string-width msg-text))
              (display-width (- (window-body-width (minibuffer-window)) (length msg-prefix))))
          (when (> msg-width display-width)
            (let ((msg-start nil)
                  (msg-end nil)
                  (ellipsis "..."))
              (let* ((word-beg (next-property-change 0 msg-text))
                     (word-end (next-property-change word-beg msg-text)))
                (setq msg-start
                      (max 0
                           (min (- (/ (+ word-beg word-end) 2) (/ display-width 2))
                                (- msg-width display-width))))
                (setq msg-end (min msg-width (+ msg-start display-width))))

              (setq msg-text (truncate-string-to-width msg-text msg-end msg-start 0 ellipsis))

              (unless (zerop msg-start)
                (setq msg-text (concat ellipsis (substring msg-text (length ellipsis))))))))))

    ;; Don't log because it's not useful to keep the selection.
    (let ((message-log-max nil))
      (message "%s%s" msg-prefix msg-text))

    msg-text))


;; ---------------------------------------------------------------------------
;; Implementation: ISpell Word

(defun recomplete-impl-ispell (cycle-index fn-cache)
  "Run `ispell-word', using the choice at CYCLE-INDEX.
Argument FN-CACHE stores the result for reuse."
  (declare (important-return-value t))
  (pcase-let ((`(,result-choices ,word-beg ,word-end) (or fn-cache '(nil nil nil))))

    (unless result-choices
      (recomplete--with-advice
          (('ispell-command-loop
            :override
            (lambda (miss _guess _word start end)
              (when miss
                (setq result-choices miss)
                (setq word-beg (marker-position start))
                (setq word-end (marker-position end))
                ;; Return the word that makes the correction; we handle this ourselves next.
                nil))))
        (ispell-word))

      (when result-choices
        (setq fn-cache (list result-choices word-beg word-end))))

    (when result-choices
      (let ((word-at-index (nth (mod cycle-index (length result-choices)) result-choices)))
        (recomplete-replace-in-region word-at-index word-beg word-end)))

    (list result-choices fn-cache)))


;; ---------------------------------------------------------------------------
;; Implementation: Case Style Cycle (Word Mode)

(defun recomplete-impl-case-style (cycle-index fn-cache)
  "Cycle case styles using the choice at CYCLE-INDEX.
Argument FN-CACHE stores the result for reuse."
  (declare (important-return-value t))
  (pcase-let ((`(,result-choices ,word-beg ,word-end) (or fn-cache '(nil nil nil))))

    (unless result-choices
      (let ((word-range (bounds-of-thing-at-point 'word)))
        (unless word-range
          (user-error "No symbol under cursor"))
        (setq word-beg (car word-range))
        (setq word-end (cdr word-range))

        ;; Scan backward over "-_".
        (let ((pos-prev (1+ word-beg)))
          (while (null (eql pos-prev word-beg))
            (setq pos-prev word-beg)
            (when (memq (char-before word-beg) '(?_ ?-))
              (let ((range
                     (save-excursion
                       (goto-char (- word-beg 2))
                       (bounds-of-thing-at-point 'word))))
                (when (and range (eql (cdr range) (- word-beg 1)))
                  (setq word-beg (car range)))))))
        ;; Scan forward over "-_".
        (let ((pos-prev (1- word-end)))
          (while (null (eql pos-prev word-end))
            (setq pos-prev word-end)
            (when (memq (char-after word-end) '(?_ ?-))
              (let ((range
                     (save-excursion
                       (goto-char (+ word-end 2))
                       (bounds-of-thing-at-point 'word))))
                (when (and range (eql (car range) (+ word-end 1)))
                  (setq word-end (cdr range))))))))

      (let ((word-init (buffer-substring-no-properties word-beg word-end)))
        (setq result-choices (recomplete--build-case-style-choices word-init t))
        (setq fn-cache (list result-choices word-beg word-end))))

    (let ((word-at-index (nth (mod cycle-index (length result-choices)) result-choices)))
      (recomplete-replace-in-region word-at-index word-beg word-end))

    (list result-choices fn-cache)))


;; ---------------------------------------------------------------------------
;; Implementation: Case Style Cycle (Programming Mode)

(defun recomplete-impl-case-style-symbol (cycle-index fn-cache)
  "Cycle case styles using the choice at CYCLE-INDEX.
Argument FN-CACHE stores the result for reuse."
  (declare (important-return-value t))
  (pcase-let ((`(,result-choices ,word-beg ,word-end) (or fn-cache '(nil nil nil))))

    (unless result-choices
      (let ((word-range (bounds-of-thing-at-point 'symbol)))
        (unless word-range
          (user-error "No symbol under cursor"))
        ;; Contract around separator characters.
        ;; This is done so conventions such as:
        ;; `__some_identifier__' -> `__SomeIdentifier__' or...
        ;; `_private_name' -> `_PrivateName'
        ;; Instead of stripping them, as surrounding separator characters
        ;; would otherwise be stripped and these characters can have a special
        ;; meaning (depending on the language).
        (save-excursion
          (goto-char (car word-range))
          (unless (zerop (skip-chars-forward "-_" (cdr word-range)))
            (setcar word-range (point)))
          (goto-char (cdr word-range))
          (unless (zerop (skip-chars-backward "-_" (car word-range)))
            (setcdr word-range (point))))
        (when (eql (car word-range) (cdr word-range))
          (user-error "No symbol under cursor containing non separators"))
        (setq word-beg (car word-range))
        (setq word-end (cdr word-range)))

      (let ((do-kebab-case (memq (char-syntax ?-) '(?_ ?w)))
            (word-init (buffer-substring-no-properties word-beg word-end)))
        (setq result-choices (recomplete--build-case-style-choices word-init do-kebab-case))
        (setq fn-cache (list result-choices word-beg word-end))))

    (let ((word-at-index (nth (mod cycle-index (length result-choices)) result-choices)))
      (recomplete-replace-in-region word-at-index word-beg word-end))

    (list result-choices fn-cache)))

;; ---------------------------------------------------------------------------
;; Implementation: `dabbrev'

(defun recomplete-impl-dabbrev (cycle-index fn-cache)
  "Expand using `dabbrev' at CYCLE-INDEX.
Argument FN-CACHE stores the result for reuse."
  (declare (important-return-value t))
  (pcase-let ((`(,result-choices ,word-beg ,word-end) (or fn-cache '(nil nil nil))))

    ;; Call `dabbrev' when not cached.
    (unless result-choices
      ;; In case this fails entirely.
      (save-excursion
        (save-match-data
          (dabbrev--reset-global-variables)

          (let ((word-init (dabbrev--abbrev-at-point)))
            ;; `dabbrev--abbrev-at-point' returns the text from the abbrev
            ;; start up to point, so the abbrev ends at point.
            (setq word-end (point))
            (setq word-beg (- word-end (length word-init)))

            (let ((ignore-case-p (dabbrev--ignore-case-p word-init)))
              (setq result-choices (dabbrev--find-all-expansions word-init ignore-case-p)))

            (unless result-choices
              ;; Trim the string since it can contain newlines.
              (user-error "No abbreviations for %S found!" (string-trim word-init)))

            ;; Exclude this word from the list of options (if it exists at all).
            (setq result-choices
                  (recomplete--rotate-list-by-elt-and-remove result-choices word-init)))

          (setq fn-cache (list result-choices word-beg word-end)))))

    (let ((word-at-index (nth (mod cycle-index (length result-choices)) result-choices)))
      (recomplete-replace-in-region word-at-index word-beg word-end))

    (list result-choices fn-cache)))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun recomplete-replace-in-region (str beg end)
  "Utility to replace region from BEG to END with STR.
Return the region replaced."
  (declare (important-return-value nil))
  ;; While not needed for replacement, setting the mark allows
  ;; the whole word that was replaced to be re-selected (C-x C-x).
  (set-mark beg)

  (let ((len (length str))
        (i-beg nil)
        (i-end nil)
        (i-end-ofs nil))

    ;; Check for skip end.
    (let ((i 0))
      (let ((len-test (min (- end beg) len)))
        (while (< i len-test)
          (let ((i-next (1+ i)))
            (cond
             ((eq (aref str (- len i-next)) (char-after (- end i-next)))
              (setq i i-next))
             (t ; Break.
              (setq len-test i))))))
      (unless (zerop i)
        (setq i-end (- len i))
        (decf len i)
        (decf end i)
        (setq i-end-ofs i)))

    ;; Check for skip start.
    (let ((i 0))
      (let ((len-test (min (- end beg) len)))
        (while (< i len-test)
          (cond
           ((eq (aref str i) (char-after (+ beg i)))
            (incf i))
           (t ; Break.
            (setq len-test i)))))
      (unless (zerop i)
        (setq i-beg i)
        (incf beg i)))

    (when (or i-beg i-end)
      (setq str (substring str (or i-beg 0) (or i-end len))))

    (goto-char beg)

    (unless (eq beg end)
      (delete-region beg end))
    (unless (string-empty-p str)
      (insert str))

    (when i-end-ofs
      ;; Leave the cursor where it would be if the end wasn't clipped.
      (goto-char (+ (point) i-end-ofs)))
    (cons beg (+ beg (length str)))))

;; Make public since users may want to add their own callbacks.

;;;###autoload
(defun recomplete-with-callback (fn-symbol cycle-offset &optional cycle-index-init)
  "Run FN-SYMBOL, chaining executions for this function.

Argument CYCLE-OFFSET The offset for cycling words,
1 or -1 for forward/backward.

Optional argument CYCLE-INDEX-INIT The initial index to use,
defaulting to zero (which makes sense for corrections) you may wish to set
the value to 1 when the current symbol is included in the list (so as to
step onto the next item)."
  (declare (important-return-value nil))

  ;; Default to 1 (one step forward).
  (setq cycle-offset (or cycle-offset 1))

  (with-command-redo-fn
   (list :id fn-symbol :on-other-command #'recomplete--on-other-command)
   (lambda (props) (recomplete--adapter-call fn-symbol cycle-offset cycle-index-init props))))

(defun recomplete--adapter-call (user-fn-symbol cycle-offset cycle-index-init props)
  "Adapter bridging `with-command-redo' and recomplete callback protocols.
USER-FN-SYMBOL is the recomplete callback.
CYCLE-OFFSET is the cycling step (1 or -1).
CYCLE-INDEX-INIT is the initial cycle index.
PROPS is the `with-command-redo' plist; :result and :cache are written via
`plist-put' before return."
  (declare (important-return-value t))
  (let* ((fn-cache (plist-get props :cache))
         (cycle-reverse (and fn-cache (alist-get 'cycle-reverse fn-cache)))
         (user-cache (and fn-cache (alist-get 'user-cache fn-cache)))
         (cycle-index
          (cond
           ((null fn-cache)
            ;; First call.
            (or cycle-index-init 0))
           (t
            ;; Subsequent call: apply offset (possibly reversed) to previous index.
            (let ((prev-cycle-index (alist-get 'cycle-index fn-cache))
                  (effective-offset
                   (cond
                    (cycle-reverse
                     (- cycle-offset))
                    (t
                     cycle-offset))))
              (+ effective-offset prev-cycle-index)))))
         (message-list (list)))

    (pcase-let ((`(,result-choices ,result-user-cache)
                 (recomplete--with-messages-as-list message-list
                   (funcall user-fn-symbol cycle-index user-cache))))
      (cond
       ((null result-choices)
        ;; Failure: show captured messages.
        (dolist (msg message-list)
          (message "%s" msg))
        (plist-put props :result nil)
        nil)

       (t
        ;; Success: compute cycle index and display.
        (setq cycle-index (mod cycle-index (length result-choices)))

        (let ((msg-text (recomplete--format-and-display result-choices cycle-index cycle-reverse)))

          ;; Adapter cache, threaded through `with-command-redo' as `:cache'.
          ;; Keys are:
          ;; - `cycle-index': position in the choice list (normalized via `mod').
          ;; - `cycle-reverse': when non-nil, reverse the cycle direction.
          ;;   Mutated in-place by `recomplete--on-other-command' on `keyboard-quit',
          ;;   so this entry must always be present.
          ;; - `msg-text': cached formatted choices, re-displayed by
          ;;   `recomplete--on-other-command' when the direction toggles.
          ;; - `user-cache': opaque cache for the user callback (`fn-cache' in
          ;;   `recomplete-impl-*'), passed through unchanged.
          (plist-put
           props
           :cache
           (list
            (cons 'cycle-index cycle-index)
            (cons 'cycle-reverse cycle-reverse)
            (cons 'msg-text msg-text)
            (cons 'user-cache result-user-cache)))
          t))))))

;; ISpell.
;;;###autoload
(defun recomplete-ispell-word (arg)
  "Run `ispell-word', using the first suggestion, or cycle forward.
ARG is the offset to cycle, default is 1, -1 to cycle backwards."
  (declare (important-return-value nil))
  (interactive "*p")
  (recomplete-with-callback 'recomplete-impl-ispell arg))

;; Case Style Cycle.
;;;###autoload
(defun recomplete-case-style (arg)
  "Cycles over common case-styles.
ARG is the offset to cycle, default is 1, -1 to cycle backwards."
  (declare (important-return-value nil))
  (interactive "*p")
  (recomplete-with-callback 'recomplete-impl-case-style arg))

;; Case Style Cycle Programming Mode.
;;;###autoload
(defun recomplete-case-style-symbol (arg)
  "Cycles over common case-styles.
ARG is the offset to cycle, default is 1, -1 to cycle backwards."
  (declare (important-return-value nil))
  (interactive "*p")
  (recomplete-with-callback 'recomplete-impl-case-style-symbol arg))

;; Abbreviations.
;;;###autoload
(defun recomplete-dabbrev (arg)
  "Run `dabbrev', using the first suggestion, or cycle forward.
ARG is the offset to cycle, default is 1, -1 to cycle backwards."
  (declare (important-return-value nil))
  (interactive "*p")
  (recomplete-with-callback 'recomplete-impl-dabbrev arg))

(provide 'recomplete)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; recomplete.el ends here
