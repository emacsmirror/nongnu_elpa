;;; org-history-outline.el --- Attach dates at the end of Org outlines -*- lexical-binding: t; -*-

;; Copyright (C) 2026 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; URL: https://codeberg.org/Anoncheg/emacs-org-history

;;; License

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:

;; To get date use `org-history--vc-git-get-range-last-mod-date'.

;;; Code:

(require 'org-history-debug)
(require 'color)

;; -=-= variables

(defcustom org-history-outline-max-days (* 360 2)
  "Maximum number of days to keep in the outline history.
This value must be an integer representing the day retention threshold.
Used to color range for dates, adjusted to oldest commit date in repo."
  :group 'org-history
  :type 'integer
  :local t
  :safe #'integerp)

(defcustom org-history-outline-min-days 90
  "Minimum number of days to keep in the outline history.
This value must be an integer representing the day retention threshold.
Used to calculate `max-days' range for colors of dates."
  :group 'org-history
  :type 'integer
  :safe #'integerp)

(defcustom org-history-outline-date-column 60
  "Column to put date at."
  :group 'org-history
  :type 'integer
  :safe #'integerp)

(defcustom org-history-outline-sync-max-file-size (* 200 1024)
  "Max file size when we simple waiting without asynchronous call.
This prevent freezing of Emacs for large files.
If file size is large than this value we run
 asynchronous process to get git blame for file."
  :group 'org-history
  :type 'integer
  :safe #'integerp)

(defcustom org-history-outline-date-render-fn #'org-history-outline-default-render-date
  "Function used to format and propertize the date string for the overlay.
It should accept two arguments: `date-str`, `color-hex`, `days-old`
 number and return a propertized string."
  :type 'function
  :group 'org-history)

(defvar org-history-hide-dates-flag) ; in org-history.el

;; -=-= Attach overlay

(defun org-history-outline-default-render-date (date-str color-hex days-old)
  "Default formatter for the outline date overlay.
Arguments: DATE-STR is in form of YYYY-MM-DD, COLOR-HEX is string like
 #e66519991999, DAYS-OLD is number.
Returns a propertized string with a bracketed date using COLOR-HEX."
  (propertize (format "[%s]" date-str)
              'face `(:foreground ,color-hex :weight bold)
              'help-echo (format "Age: %d days old" days-old)
              'read-only nil
              ;; 'intangible t
              'cursor-intangible t))

(defun org-history-outline-default-render-daysold (date-str color-hex days-old)
  "Default formatter for the outline date overlay.
Arguments: DATE-STR is in form of YYYY-MM-DD, COLOR-HEX is list,
 DAYS-OLD is number.
Returns a propertized string with a bracketed date using COLOR-HEX."
  (propertize (format "☼ %d days old" days-old)
              'face `(:foreground ,color-hex)
              'read-only nil
              ;; 'intangible t
              'cursor-intangible t
              'help-echo (format "[%s]" date-str)))

(defvar org-history-outline--attach-hook nil
  "Hook run by `org-history-outline--attach-date` after putting overlays.")

(defun org-history-outline--attach-date (date-str)
  "Attach overlay with date at the end header with color gradient.
Cursors should be at header position.
DATE-STR is in form of YYYY-MM-DD.
Smooth `vc-annotate' color gradient is used by how old date is.
Overlay is not modifiable and dont modify buffer content.
Automatically deletes older date overlays on the same headline when
 updated."
  ;; (interactive "sEnter date (YYYY-MM-DD): ")
  (let* ((time-attr (condition-case nil
                        (date-to-time date-str)
                      (user-error "Invalid date format! Use YYYY-MM-DD")))
         ;; 1. Max out at 0 to avoid future negative date math bugs safely in one line
         (days-old (max 0 (floor (- (float-time) (float-time time-attr)) 86400)))
         ;; 2. Generate smooth vc-annotate aging color wheel
         (ratio (/ (min days-old org-history-outline-max-days) (float org-history-outline-max-days)))
         (hue (* ratio 0.66)) ; 0.0 = Red, 0.66 = Blue
         (rgb (color-hsl-to-rgb hue 0.8 0.5))
         (color-hex (apply #'color-rgb-to-hex rgb)) ; string

         ;; --- THE CHARACTER CALCULATION SETUP ---
         (lend (line-end-position))
         (lstart (1- lend))
         ;; (target-column 60)

         ;; Calculate the current text column width manually
         (current-line-length (- lend (line-beginning-position)))
         ;; Determine needed padding (fallback to at least 2 spaces if line is longer than target)
         (padding-needed (max 2 (- org-history-outline-date-column current-line-length)))
         ;; Indentation for dates.
         (padding-needed (+ padding-needed (org-outline-level)))
         ;; Generate a real string containing exactly that many spaces
         (calculated-spaces (make-string padding-needed ?\s)))

    ;; 3. CLEANUP: Clear overlays sitting exactly on that last character slot
    (remove-overlays lstart lend 'identity 'org-history-date)
    (remove-overlays lstart lend 'identity 'org-history-hint)

    ;; 4. CREATION: Render using calculated hard padding strings
    (let* ((ov (make-overlay lstart lend))
           (last-char (buffer-substring-no-properties lstart lend))
           ;; Call the user-customizable function here:
           (date-text (funcall org-history-outline-date-render-fn date-str color-hex days-old))
           (hint (get-text-property 0 'help-echo date-text))
           (ov-hint (make-overlay (1- lstart) lend)))

      (overlay-put ov 'identity 'org-history-date)
      (overlay-put ov 'priority 100)

      ;; Concatenate the last character, the exact computed spaces, and the date.
      (overlay-put ov 'display (concat last-char calculated-spaces date-text))

      (overlay-put ov-hint 'help-echo hint)
      (overlay-put ov-hint 'identity 'org-history-hint)

      (run-hooks 'org-history-outline--attach-hook))))


(defun org-history-outline-clear-all-date ()
  "Instantly remove all custom date overlays from the entire buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'identity 'org-history-date)
  (remove-overlays (point-min) (point-max) 'identity 'org-history-hint)
  (font-lock-flush)
  (message "Successfully cleared all header date overlays."))

;; -=-= git blame

(defun org-history-outline--process-git-blame-output ()
  "Process result of git blame command from beginning of current buffer.
The returned hash table uses `eql' as its test, where keys are line
numbers (integers starting from 1) and values are plain strings representing
the last modification date formatted as \"YYYY-MM-DD\"."
  (org-history-debug-print "org-history-outline--process-git-blame-output %s" (current-buffer))
  (let ((line-dates (make-hash-table :test 'eql))
        (line-num 1)
        last-date-str)
    (goto-char (point-min))
    ;; Optimization 1: Anchor search to the current line to limit regex engine workload
    (while (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" (line-end-position) t)
      (let ((date-str (match-string-no-properties 1)))
        ;; Optimization 2: String Deduplication to drastically cut GC pressure
        (if (and last-date-str (string= date-str last-date-str))
            (puthash line-num last-date-str line-dates)
          (setq last-date-str date-str)
          (puthash line-num date-str line-dates)))
      (setq line-num (1+ line-num))
      ;; Optimization 3: Move explicitly to the next line to avoid double-matching
      ;; if a code comment accidentally contains a date string.
      (forward-line 1))
    line-dates))

;; -=-= git-blame-file

(defvar-local org-history-outline--blame-proc nil
  "Tracks the active git blame process for the current buffer.
Set in function `org-history-outline--git-blame-file-main'")

(defun org-history-outline--vc-git-blame-file-async (file callback)
  "Run git blame on FILE asynchronously.
Updates a progress reporter using a repeating timer every second.
Inserts the result at the end of the current buffer upon completion.
Interrupts the process and cancels the timer if the buffer is closed.
If calling buffer not exist at finish message about it and return.
Call CALLBACK with one argument of calling buffer if success."
  (interactive (list (buffer-file-name)))
  (unless file (user-error "Current buffer is not visiting a file"))
  (org-history-debug-print "org-history-outline--vc-git-blame-file-async N1 %s" file)

  (let* ((calling-buf (current-buffer))
         (output-buf (generate-new-buffer " *git-blame-async*"))
         (process-args (list "--no-pager" "blame" "-M" "-w" "--date=short" "-c" file))
         (reporter (make-progress-reporter (format "Running git blame on %s..."
                                                   (file-name-nondirectory file))))
         proc
         timer
         kill-hook
         cleanup-resources) ; Declare the variable first for closure safety

    ;; 1. NATIVE CLEANUP FUNCTION (Stored in a lexical variable)
    (setq cleanup-resources
          (lambda ()
            (when (timerp timer)
              (cancel-timer timer)
              (setq timer nil))
            (progress-reporter-done reporter)
            (when (buffer-live-p output-buf)
              (kill-buffer output-buf))
            ;; Remove the safety buffer-local hook to avoid dead code execution later
            (when (buffer-live-p calling-buf)
              (with-current-buffer calling-buf
                (remove-hook 'kill-buffer-hook kill-hook t)))))

    (setq kill-hook (lambda ()
                      (when (process-live-p proc)
                        (delete-process proc)) ; Triggers the sentinel with status 'signal
                      (funcall cleanup-resources))) ; Kills the timer immediately
    ;; 2. START THE REPEATING TIMER
    (setq timer (run-with-timer
                 1.0 0.7
                 (lambda ()
                   (progress-reporter-update reporter))))

    ;; 3. START THE ASYNC PROCESS
    (setq proc
          (make-process
           :name "git-blame-process"
           :buffer output-buf
           :command (cons vc-git-program process-args)
           :connection-type 'pipe
           :noquery t
           :sentinel (lambda (process _event)
                       (let ((status (process-status process))
                             (exit-code (process-exit-status process)))

                         (when (memq status '(exit signal))
                           ;; Wrap everything in unwind-protect to guarantee cleanup
                           (unwind-protect
                               (condition-case err
                                   (cond
                                    ;; Case A: Process was explicitly killed (e.g. by buffer closure)
                                    ((eq status 'signal)
                                     (message "Git blame process interrupted."))

                                    ;; Case B: Target buffer died through other means
                                    ((not (buffer-live-p calling-buf))
                                     (message "Git blame aborted: target buffer no longer exists."))

                                    ;; Case C: Git process failed with an error code
                                    ((not (zerop exit-code))
                                     (let ((err-msg (if (buffer-live-p output-buf)
                                                        (with-current-buffer output-buf (string-trim (buffer-string)))
                                                      "Unknown Git error")))
                                       (message "Git blame failed: %s" (if (string-empty-p err-msg) "Exit code non-zero" err-msg))))

                                    ;; Success Case: Output parsed out of the automatically filled output-buf
                                    (t
                                     (unless (buffer-live-p output-buf)
                                       (user-error "Sentitne output-buf is not alive")) ; we use user-error, because throwing a raw error inside the sentinel without catching it will break the sentinel execution flow.

                                     (with-current-buffer output-buf
                                       (funcall callback calling-buf)))) ; CALLBACK!

                                 ;; Universal Guard Clause for the Sentinel Logic
                                 (error
                                  (message "Blame completion error: %s" (error-message-string err)))) ; condition-case

                             ;; Clean up using the lexical native lambda function
                             (funcall cleanup-resources)))))))

    ;; 4. BUFFER CLOSE INTERRUPT MECHANISM
    (with-current-buffer calling-buf
      (add-hook 'kill-buffer-hook
                kill-hook
                nil t)) ; Buffer-local hook
    proc))


(defun org-history-outline--git-blame-file-main (file &optional async-callback)
  "Return a hash table mapping line numbers to last modification for FILE.

Called from `org-history-outline--add-dates' with callback to save
 result in cache at upper org-history.el

Enhanced version of `org-history--vc-git-get-range-last-mod-date'.
Uses `default-directory'.

FILE should be relative to `default-directory' or full path.
If FILE does not exist, is not registered under Git, or the underlying
`git blame' command fails, an empty hash table is returned.

To minimize memory allocation and prevent Garbage Collection (GC) pressure,
consecutive lines that share identical modification dates point to the exact
same string object in memory.  The search is strictly bounded line-by-line
to prevent layout syntax errors from desynchronizing the line counter.

When ASYNC-CALLBACK is not provided,
The returned hash table uses `eql' as its test, where keys are line
numbers (integers starting from 1) and values are plain strings representing
the last modification date formatted as \"YYYY-MM-DD\".

When ASYNC-CALLBACK provided, return immediately and call
ASYNC-CALLBACK in current buffer with rutern value above."
  (org-history-debug-print "org-history-outline--git-blame-file-main N1 async=%s %s %s" async-callback file default-directory)
  (when (and (file-exists-p file) (vc-git-responsible-p default-directory))
    (if async-callback
        (unless (process-live-p org-history-outline--blame-proc)
          (setq org-history-outline--blame-proc
                (org-history-outline--vc-git-blame-file-async file
                                                              (lambda (caller-buf)
                                                                (let ((blame-table (org-history-outline--process-git-blame-output)))
                                                                  ;; (org-history-debug-print "org-history-outline--git-blame-file-main N2 %s" (current-buffer) caller-buf blame-table)
                                                                  ;; in current-buffer
                                                                  (when (buffer-live-p caller-buf)
                                                                    (with-current-buffer caller-buf
                                                                      (funcall async-callback blame-table))))))))
      ;; else
      (with-temp-buffer
        (org-history-debug-print "org-history-outline--git-blame-file-main N3")
        (when (zerop (vc-git-command t 0 nil "blame" "-M" "-w" "--date=short" "-c" "--no-progress" file)) ; git blame -M -w --date=short -c --no-progress data_science
          (org-history-outline--process-git-blame-output))))))

;; -=-= Process-tasks

(defvar-local org-history-outline--git-blame-cache nil
  "Cache for optimization: Hash table - Key is line-num, value is date-str.
Set to return value of function
 `org-history-outline--git-blame-file-main' that call git blame and
 process result in `org-history-outline--process-git-blame-output'
 function, called in function `org-history-outline--add-dates'.
Value used in `org-history-outline--process-tasks' function." )


(defvar-local org-history-outline--git-last-commit nil
  "Used to check if cache required to be updated.")


(defun org-history-outline--update-max-days (file-oldest)
  "Calculate and update `org-history-outline-max-days' based on FILE-OLDEST.
Clamps the calculated days between `org-history-outline-min-days' and the
current `org-history-outline-max-days'."
  (when file-oldest
    (let ((calculated-days (- (org-today)
                              (org-time-string-to-absolute file-oldest))))
      ;; Restrict 'calculated-days' to a strict min and max range
      (setq org-history-outline-max-days
            (max org-history-outline-min-days
                 (min calculated-days org-history-outline-max-days)))
      (message "org-history: max-days set to %s days." org-history-outline-max-days))))


(defun org-history-outline--process-tasks (head-markers blame-table &optional set-oldest)
  "Process HEAD-MARKERS instantly by pre-caching Git blame data using native loops.
Should be called in target buffer.
If optional argument SET-OLDEST, `org-history-outline-max-days' will be
 set to oldest date during applying if it not older than
 `org-history-outline-max-days' orginal value.
Argument BLAME-TABLE is from `org-history-outline--git-blame-cache'."
  (org-history-debug-print "org-history-outline--process-tasks N1 %s %s" set-oldest (current-buffer))
  (org-history-debug-print "org-history-outline--process-tasks N11" head-markers)

  ;; PHASE 1: Process ranges instantly using native loops
  (let* (file-oldest
         (tasks-with-dates
          (mapcar (lambda (task)
                    ;; ISO 8601 strings sort chronologically when sorted alphabetically
                    (let ((header-pos (1- (marker-position task))) ; we use  (1-, because we created marker with 1+ [[file:~/sources/emacs-org-history/org-history.el::460::(push (copy-marker (1+ (point))) head-markers)) ; position of begining of heading +1]]
                          (latest "1970-01-01")
                          start end)
                      (set-marker task nil) ; free marker
                      (save-excursion
                        (goto-char header-pos)
                        (setq start (line-number-at-pos))
                        (org-end-of-subtree t t)
                        (setq end (- (line-number-at-pos) 2)) ; idk why but if you move header the last line of it change lost history.
                        (when (< end start)
                          (setq end start)))

                      ;; Native loop over the line range to find the newest date
                      (let ((l start))
                        (while (<= l end)
                          ;; 3. Thought: Combined the `gethash` lookup and string comparison directly to avoid deep nesting.
                          (let ((l-date (gethash l blame-table)))
                            (when (and l-date (string> l-date latest))
                              (setq latest l-date)))
                          (setq l (1+ l))))

                      ;; Now update file-oldest with the oldest date found
                      (unless (string= latest "1970-01-01")
                        (when (or (not file-oldest)
                                  (string< latest file-oldest))
                          (setq file-oldest latest)))

                      ;; Return pair: (marker . date-str) or nil if unchanged from epoch
                      (cons header-pos (unless (string= latest "1970-01-01") latest))))
                  head-markers)))

    (org-history-debug-print "org-history-outline--process-tasks N2 %s" set-oldest file-oldest)

    ;; Adjust max-days for colour
    (when (and set-oldest file-oldest)
      (org-history-outline--update-max-days file-oldest))

    (org-history-debug-print "org-history-outline--process-tasks N3" tasks-with-dates)

    ;; PHASE 2: Apply Overlays using native dolist
    (dolist (cell tasks-with-dates)
      (org-history-debug-print "org-history-outline--process-tasks N4 %s" cell)
      ;; 5. Thought: Used `pcase-dolist` or direct destructuring for cleaner pair extraction, and removed commented-out code.
      (let ((header-pos (car cell))
            (date-str (cdr cell)))
        (org-history-debug-print "org-history-outline--process-tasks N41 %s %s" header-pos date-str)
        (when date-str
          (save-excursion
            (goto-char header-pos)
            (org-history-outline--attach-date date-str)
            (org-history-debug-print "org-history-outline--process-tasks N42")))))))

(defun org-history-outline--add-dates (head-markers commit-hash &optional set-oldest)
  "Process HEAD-MARKERS instantly by pre-caching Git blame data using native loops.
Called by Called by `org-history-add-dates'.
Uses variable `buffer-file-name' function.
If optional argument SET-OLDEST, `org-history-outline-max-days' will be
 set to oldest date during applying if it not older than
 `org-history-outline-max-days' orginal value.

Argument COMMIT-HASH full hash of commit for current file, mandatory."
  (org-history-debug-print "org-history-outline--add-dates N1 %s %s" set-oldest commit-hash org-history-outline--git-last-commit)
  ;; todo, if call before finish.
  ;; todo, if called too frequently
  (let ((is-cache-update (not (string-equal commit-hash org-history-outline--git-last-commit))) ; no cache or update
        (is-file-big (> (file-attribute-size (file-attributes (buffer-file-name)))
                        org-history-outline-sync-max-file-size))

        (callback-for-blame-and-cache
         (lambda (git-blame-table)
           (org-history-debug-print "org-history-outline--add-dates N3 async %s" set-oldest)
           (setq org-history-outline--git-blame-cache git-blame-table)
           (org-history-outline--process-tasks head-markers git-blame-table set-oldest) ; head-markers put here in lambda
           (setq org-history-outline--git-last-commit commit-hash))))

    (org-history-debug-print "org-history-outline--add-dates N2 %s %s" is-cache-update is-file-big)
    (if is-cache-update
        (progn
          (if is-file-big
              ;; Case 1: async
              (progn
                ;; Update it from cache fast
                (when org-history-outline--git-blame-cache
                  (org-history-outline--process-tasks head-markers org-history-outline--git-blame-cache))
                ;; then request update
                (org-history-outline--git-blame-file-main (buffer-file-name) callback-for-blame-and-cache))
            ;; else - Case 2: sync
            (unless (bound-and-true-p org-history-hide-dates-flag) ; we cant call async because it will be too long and heavy, we will hide it anyway, with sync we can make it fast.
              (setq org-history-outline--git-blame-cache (org-history-outline--git-blame-file-main (buffer-file-name)))
              (org-history-debug-print "org-history-outline--add-dates N4sync" org-history-outline--git-blame-cache)
              (org-history-outline--process-tasks head-markers org-history-outline--git-blame-cache)
              (setq org-history-outline--git-last-commit commit-hash))))
      ;; else - ;; Case 3: use cache
      (org-history-debug-print "org-history-outline--add-dates N5")
      (unless org-history-outline--git-blame-cache
        (error "Internal error org-history-outline--git-blame-cache should set" ))
      (org-history-outline--process-tasks head-markers org-history-outline--git-blame-cache))))


;;; provide

(provide 'org-history-outline)

;;; org-history-outline.el ends here
