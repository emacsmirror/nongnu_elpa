;;; fedi.el --- Helper functions for fediverse clients  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Marty Hiatt and mastodon.el authors
;; Author: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.1") (markdown-mode "2.5"))
;; Homepage: https://codeberg.org/martianh/fedi.el

;; This file is not part of GNU Emacs.

;; fedi-http.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fedi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fedi.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; fedi.el adapts mastodon-http.el from
;; <https://codeberg.org/martianh/mastodon.el> to make it easy to write
;; endpoint-hitting functions for JSON APIs.

;; It provides `fedi-request' to easily generate request functions, handles
;; constructing form data parameters and sending JSON payloads (for POST/PUT,
;; etc.).

;; Responses are checked with `fedi-http--triage', and processed with
;; `fedi-http--process-response'. If a response returns HTML, it is rendered
;; with `shr', otherwise the JSON is pased and returned.

;; Because of mastodon.el works, there is also code for handling link headers
;; in responses. Mastodon uses these for pagination in some cases. If your
;; service also des, you can handle them too.

;;; Code:

(require 'fedi-http)

(defvar fedi-instance-url nil
  "The URL of the instance to connect to.")

(defvar fedi-package-prefix nil
  "The name of your package, without following dash.
Used to construct function names in `fedi-request'.")

(defgroup fedi nil
  "Fedi."
  :prefix "fedi-"
  :group 'external)

;;; REQUEST MACRO
;; this is an example of a request macro for defining request functions.
;; `lem.el' now defines its own rather than wrapping around this, for
;; simplicity, and you probably don't want to use it either, but you can still
;; use it as a guide to writing your own. see also `lem-def-request' in
;; `lem-api.el'.
;; (defmacro fedi-request
;;     (method name endpoint
;;             &optional args docstring params man-params opt-bools json headers)
;;   "Create a http request function NAME, using http METHOD, for ENDPOINT.
;; ARGS are for the function.
;; PARAMS is an list of elements from which to build an alist of
;; form parameters to send with the request.
;; MAN-PARAMS is an alist, to append to the one created from PARAMS.
;; JSON means to encode params as a JSON payload.
;; HEADERS is an alist that will be bound as `url-request-extra-headers'.

;; This macro is designed to generate functions for fetching data
;; from JSON APIs.

;; To use it, you first need to set `fedi-package-prefix' to the
;; name of your package, and set `fedi-instance-url' to the URL of
;; an instance of your fedi service.

;; The name of functions generated with this will be the result of:
;; \(concat fedi-package-prefix \"-\" name).

;; The full URL for the endpoint is constructed by `fedi-http--api',
;; which see. ENDPOINT does not require a preceding slash.

;; For example, to make a GET request, called PKG-search to endpoint /search:

;; \(fedi-request \"get\" \"search\" \"search\"
;;   (q)
;;   \"Make a GET request.
;; Q is the search query.\"
;;   \\=(q))

;; This macro doesn't handle authenticated requests, as these differ
;; between services. But you can easily wrap it in another macro
;; that handles auth by providing info using HEADERS or AUTH-PARAM."
;;   (declare (debug t)
;;            (indent 3))
;;   (let ((req-fun (intern (concat "fedi-http--" method))))
;;     `(defun ,(intern (concat fedi-package-prefix "-" name)) ,args
;;        ,docstring
;;        (let* ((req-url (fedi-http--api ,endpoint))
;;               (url-request-method ,(upcase method))
;;               (url-request-extra-headers ,headers)
;;               (bools (remove nil
;;                              (list ,@(fedi-make-params-alist
;;                                       opt-bools #'fedi-arg-when-boolean))))
;;               (params-alist (remove nil
;;                                     (list ,@(fedi-make-params-alist
;;                                              params #'fedi-arg-when-expr))))
;;               (params (if ',man-params
;;                           (append ,man-params params-alist)
;;                         params-alist))
;;               (params (append params bools))
;;               (response
;;                (cond ((or (equal ,method "post")
;;                           (equal ,method "put"))
;;                       ;; FIXME: deal with headers nil arg here:
;;                       (funcall #',req-fun req-url params nil ,json))
;;                      (t
;;                       (funcall #',req-fun req-url params)))))
;;          (fedi-http--triage response
;;                             (lambda ()
;;                               (with-current-buffer response
;;                                 (fedi-http--process-json))))))))

;; This trick doesn't actually do what we want, as our macro is called
;; to define functions, so must be called with all possible arguments, rather
;; than only those of a given function call.
;; Still, it solves the problem of the server rejecting nil param values.
(defun fedi-arg-when-expr (arg &optional coerce)
  "Return a cons of a string and a symbol type of ARG.
Also replace _ with - (for Lemmy's type_ param)."
  (let ((str
         (string-replace "-" "_" ; for "type_" etc.
                         (symbol-name arg))))
    ;; FIXME: when the when test fails, it adds nil to the list in the
    ;; expansion, so we have to call (remove nil) on the result.
    (if coerce
        `(when ,arg
           (cons ,str (if (numberp ,arg)
                          (number-to-string ,arg)
                        ,arg)))
      `(when ,arg
         (cons ,str ,arg)))))

;; (fedi-arg-when-expr 'sort)

(defun fedi-make-params-alist (args fun &optional coerce)
  "Call FUN on each of ARGS."
  (cl-loop while args
           collecting (funcall fun (pop args) coerce)))

;; (fedi-make-params-alist '(sort type))

;; (defun fedi-arg-when-boolean (arg)
;;   "ARG."
;;   (let ((str
;;          (string-replace "-" "_"
;;                          (symbol-name arg))))
;;     `(when ,arg (cons ,str "true"))))


;;; BUFFER MACRO

(defmacro fedi-with-buffer (buffer mode-fun other-window &rest body)
  "Evaluate BODY in a new or existing buffer called BUFFER.
MODE-FUN is called to set the major mode.
OTHER-WINDOW means call `switch-to-buffer-other-window' rather
than `switch-to-buffer'."
  (declare (debug t)
           (indent 3))
  `(with-current-buffer (get-buffer-create ,buffer)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (funcall ,mode-fun)
       (if ,other-window
           (switch-to-buffer-other-window ,buffer)
         (switch-to-buffer ,buffer))
       ,@body
       (goto-char (point-min)))))

;;; NAV

(defun fedi--goto-pos (fun prop &optional refresh pos)
  "Search for item with FUN.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (let* ((npos (funcall fun
                        (or pos (point))
                        prop
                        (current-buffer))))
    (if npos
        (if (not (get-text-property npos prop))
            (fedi--goto-pos fun prop refresh npos)
          (goto-char npos))
      (if refresh
          (funcall refresh)
        (message "Nothing further")))))

;; (defun fedi-next-item ()
;;   "Move to next item."
;;   (interactive)
;;   (fedi--goto-pos #'next-single-property-change)) ;#'fedi-ui-more))

;; (defun fedi-prev-item ()
;;   "Move to prev item."
;;   (interactive)
;;   (fedi--goto-pos #'previous-single-property-change))

(defun fedi-next-tab-item (&optional previous prop)
  "Move to the next interesting item.
This could be the next toot, link, or image; whichever comes first.
Don't move if nothing else to move to is found, i.e. near the end of the buffer.
This also skips tab items in invisible text, i.e. hidden spoiler text.
PREVIOUS means move to previous item.
PROP is the text property to search for."
  (interactive)
  (let (next-range
        (search-pos (point)))
    (while (and (setq next-range
                      (fedi--find-next-or-previous-property-range
                       prop search-pos previous))
                (get-text-property (car next-range) 'invisible)
                (setq search-pos (if previous
                                     (1- (car next-range))
                                   (1+ (cdr next-range)))))
      ;; do nothing, all the action is in the while condition
      )
    (if (null next-range)
        (message "Nothing else here.")
      (goto-char (car next-range))
      (if-let ((hecho (fedi--property 'help-echo)))
          (message "%s" hecho)))))

(defun fedi-previous-tab-item ()
  "Move to the previous interesting item.
This could be the previous toot, link, or image; whichever comes
first. Don't move if nothing else to move to is found, i.e. near
the start of the buffer. This also skips tab items in invisible
text, i.e. hidden spoiler text."
  (interactive)
  (fedi-next-tab-item :previous))

;;; HEADINGS

(defvar fedi-horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

(defun fedi-format-heading (name)
  "Format a heading for NAME, a string."
  (let* ((name (if (symbolp name)
                   (symbol-name name)
                 name))
         (name (string-replace "-" " " name)))
    (propertize
     (concat " " lem-ui-horiz-bar "\n "
             (upcase name)
             "\n " lem-ui-horiz-bar "\n")
     'face 'success)))

(defun fedi-insert-heading (name)
  "Insert heading for NAME, a string."
  (insert (fedi-format-heading name)))

;;; SYMBOLS

(defcustom fedi-symbols
  '((reply     . ("💬" . "R"))
    (boost     . ("🔁" . "B"))
    (favourite . ("⭐" . "F"))
    (bookmark  . ("🔖" . "K"))
    (media     . ("📹" . "[media]"))
    (verified  . ("✓" . "V"))
    (locked    . ("🔒" . "[locked]"))
    (private   . ("🔒" . "[followers]"))
    (direct    . ("✉" . "[direct]"))
    (edited    . ("✍" . "[edited]"))
    (upvote    . ("⬆" . "[upvotes]"))
    (person    . ("👤" . "[people]"))
    (pinned    . ("📌" . "[pinned]"))
    (replied   . ("⬇" . "↓"))
    (community . ("👪" . "[community]"))
    (reply-bar . ("│" . "|")) ;┃
    (deleted   . ("🗑" . "[deleted]")))
  "A set of symbols (and fallback strings) to be used in timeline.
If a symbol does not look right (tofu), it means your
font settings do not support it."
  :type '(alist :key-type symbol :value-type string)
  :group 'fedi)

(defun fedi-symbol (name)
  "Return the unicode symbol (as a string) corresponding to NAME.
If symbol is not displayable, an ASCII equivalent is returned. If
NAME is not part of the symbol table, '?' is returned."
  (if-let* ((symbol (alist-get name fedi-symbols)))
      (if (char-displayable-p (string-to-char (car symbol)))
          (car symbol)
        (cdr symbol))
    "?"))

(defun fedi-font-lock-comment (&rest strs)
  "Font lock comment face STRS."
  (propertize (mapconcat #'identity strs "")
              'face font-lock-comment-face))

(defun fedi-thing-json ()
  "Get json of thing at point, comment, post, community or user."
  (get-text-property (point) 'json))

(defun fedi--property (prop)
  "Get text property PROP from item at point."
  (get-text-property (point) prop))

;;; FEDI-URL-P

(defun fedi-fedilike-url-p (query)
  "Return non-nil if QUERY resembles a fediverse URL."
  ;; calqued off https://github.com/tuskyapp/Tusky/blob/c8fc2418b8f5458a817bba221d025b822225e130/app/src/main/java/com/keylesspalace/tusky/BottomSheetActivity.kt
  ;; thx to Conny Duck!
  (let* ((uri-parsed (url-generic-parse-url query))
         (query (url-filename uri-parsed)))
    (save-match-data
      (or (string-match "^/@[^/]+$" query)
          (string-match "^/@[^/]+/[[:digit:]]+$" query)
          (string-match "^/user[s]?/[[:alnum:]]+$" query)
          (string-match "^/notice/[[:alnum:]]+$" query)
          (string-match "^/objects/[-a-f0-9]+$" query)
          (string-match "^/notes/[a-z0-9]+$" query)
          (string-match "^/display/[-a-f0-9]+$" query)
          (string-match "^/profile/[[:alpha:]]+$" query)
          (string-match "^/p/[[:alpha:]]+/[[:digit:]]+$" query)
          (string-match "^/[[:alpha:]]+$" query)
          (string-match "^/u/[_[:alnum:]]+$" query)
          (string-match "^/c/[_[:alnum:]]+$" query)
          (string-match "^/post/[[:digit:]]+$" query)
          (string-match "^/comment/[[:digit:]]+$" query)))))

;;; TIMESTAMPS

(defvar-local fedi-timestamp-next-update nil
  "The timestamp when the buffer should next be scanned to update the timestamps.")

(defvar-local fedi-timestamp-update-timer nil
  "The timer that, when set will scan the buffer to update the timestamps.")

(defcustom fedi-enable-relative-timestamps t
  "Whether to show relative (to the current time) timestamps.
This will require periodic updates of a timeline buffer to
keep the timestamps current as time progresses."
  :type '(boolean :tag "Enable relative timestamps and background updater task"))

(defun fedi--find-property-range (property start-point
                                           &optional search-backwards)
  "Return nil if no such range is found.
If PROPERTY is set at START-POINT returns a range around
START-POINT otherwise before/after START-POINT.
SEARCH-BACKWARDS determines whether we pick point
before (non-nil) or after (nil)"
  (if (get-text-property start-point property)
      ;; We are within a range, so look backwards for the start:
      (cons (previous-single-property-change
             (if (equal start-point (point-max)) start-point (1+ start-point))
             property nil (point-min))
            (next-single-property-change start-point property nil (point-max)))
    (if search-backwards
        (let* ((end (or (previous-single-property-change
                         (if (equal start-point (point-max))
                             start-point (1+ start-point))
                         property)
                        ;; we may either be just before the range or there
                        ;; is nothing at all
                        (and (not (equal start-point (point-min)))
                             (get-text-property (1- start-point) property)
                             start-point)))
               (start (and end (previous-single-property-change
                                end property nil (point-min)))))
          (when end
            (cons start end)))
      (let* ((start (next-single-property-change start-point property))
             (end (and start (next-single-property-change
                              start property nil (point-max)))))
        (when start
          (cons start end))))))

(defun fedi--find-next-or-previous-property-range
    (property start-point search-backwards)
  "Find (start . end) property range after/before START-POINT.
Does so while PROPERTY is set to a consistent value (different
from the value at START-POINT if that is set).
Return nil if no such range exists.
If SEARCH-BACKWARDS is non-nil it find a region before
START-POINT otherwise after START-POINT."
  (if (get-text-property start-point property)
      ;; We are within a range, we need to start the search from
      ;; before/after this range:
      (let ((current-range (fedi--find-property-range property start-point)))
        (if search-backwards
            (unless (equal (car current-range) (point-min))
              (fedi--find-property-range
               property (1- (car current-range)) search-backwards))
          (unless (equal (cdr current-range) (point-max))
            (fedi--find-property-range
             property (1+ (cdr current-range)) search-backwards))))
    ;; If we are not within a range, we can just defer to
    ;; fedi--find-property-range directly.
    (fedi--find-property-range property start-point search-backwards)))

(defun fedi--consider-timestamp-for-updates (timestamp)
  "Take note that TIMESTAMP is used in buffer and ajust timers as needed.
This calculates the next time the text for TIMESTAMP will change
and may adjust existing or future timer runs should that time
before current plans to run the update function.
The adjustment is only made if it is significantly (a few
seconds) before the currently scheduled time. This helps reduce
the number of occasions where we schedule an update only to
schedule the next one on completion to be within a few seconds.
If relative timestamps are disabled (i.e. if
`mastodon-tl--enable-relative-timestamps' is nil), this is a
no-op."
  (when fedi-enable-relative-timestamps
    (let ((this-update (cdr (fedi--relative-time-details timestamp))))
      (when (time-less-p this-update
                         (time-subtract fedi-timestamp-next-update
                                        (seconds-to-time 10)))
        (setq fedi-timestamp-next-update this-update)
        (when fedi-timestamp-update-timer
          ;; We need to re-schedule for an earlier time
          (cancel-timer fedi-timestamp-update-timer)
          (setq fedi-timestamp-update-timer
                (run-at-time (time-to-seconds (time-subtract this-update
                                                             (current-time)))
                             nil ;; don't repeat
                             #'fedi--update-timestamps-callback
                             (current-buffer) nil)))))))

(defun fedi--update-timestamps-callback (buffer previous-marker)
  "Update the next few timestamp displays in BUFFER.
Start searching for more timestamps from PREVIOUS-MARKER or
from the start if it is nil."
  ;; only do things if the buffer hasn't been killed in the meantime
  (when (and fedi-enable-relative-timestamps ; just in case
             (buffer-live-p buffer))
    (save-excursion
      (with-current-buffer buffer
        (let ((previous-timestamp (if previous-marker
                                      (marker-position previous-marker)
                                    (point-min)))
              (iteration 0)
              next-timestamp-range)
          (if previous-marker
              ;; a follow-up call to process the next batch of timestamps.
              ;; Release the marker to not slow things down.
              (set-marker previous-marker nil)
            ;; Otherwise this is a rew run, so let's initialize the next-run time.
            (setq fedi-timestamp-next-update (time-add (current-time)
                                                       (seconds-to-time 300))
                  fedi-timestamp-update-timer nil))
          (while (and (< iteration 5)
                      (setq next-timestamp-range
                            (fedi--find-property-range 'timestamp
                                                       previous-timestamp)))
            (let* ((start (car next-timestamp-range))
                   (end (cdr next-timestamp-range))
                   (timestamp (get-text-property start 'timestamp))
                   (current-display (get-text-property start 'display))
                   (new-display (fedi--relative-time-description timestamp)))
              (unless (string= current-display new-display)
                (let ((inhibit-read-only t))
                  (add-text-properties
                   start end
                   (list 'display
                         (fedi--relative-time-description timestamp)))))
              (fedi--consider-timestamp-for-updates timestamp)
              (setq iteration (1+ iteration)
                    previous-timestamp (1+ (cdr next-timestamp-range)))))
          (if next-timestamp-range
              ;; schedule the next batch from the previous location to
              ;; start very soon in the future:
              (run-at-time 0.1 nil #'fedi--update-timestamps-callback buffer
                           (copy-marker previous-timestamp))
            ;; otherwise we are done for now; schedule a new run for when needed
            (setq fedi-timestamp-update-timer
                  (run-at-time (time-to-seconds
                                (time-subtract fedi-timestamp-next-update
                                               (current-time)))
                               nil ;; don't repeat
                               #'fedi--update-timestamps-callback
                               buffer nil))))))))

(defun fedi--relative-time-details (timestamp &optional current-time)
  "Return cons of (descriptive string . next change) for the TIMESTAMP.
Use the optional CURRENT-TIME as the current time (only used for
reliable testing).
The descriptive string is a human readable version relative to
the current time while the next change timestamp give the first
time that this description will change in the future.
TIMESTAMP is assumed to be in the past."
  (let* ((now (or current-time (current-time)))
         (time-difference (time-subtract now timestamp))
         (seconds-difference (float-time time-difference))
         (regular-response
          (lambda (seconds-difference multiplier unit-name)
            (let ((n (floor (+ 0.5 (/ seconds-difference multiplier)))))
              (cons (format "%d %ss ago" n unit-name)
                    (* (+ 0.5 n) multiplier)))))
         (relative-result
          (cond
           ((< seconds-difference 60)
            (cons "just now"
                  60))
           ((< seconds-difference (* 1.5 60))
            (cons "1 minute ago"
                  90)) ;; at 90 secs
           ((< seconds-difference (* 60 59.5))
            (funcall regular-response seconds-difference 60 "minute"))
           ((< seconds-difference (* 1.5 60 60))
            (cons "1 hour ago"
                  (* 60 90))) ;; at 90 minutes
           ((< seconds-difference (* 60 60 23.5))
            (funcall regular-response seconds-difference (* 60 60) "hour"))
           ((< seconds-difference (* 1.5 60 60 24))
            (cons "1 day ago"
                  (* 1.5 60 60 24))) ;; at a day and a half
           ((< seconds-difference (* 60 60 24 6.5))
            (funcall regular-response seconds-difference (* 60 60 24) "day"))
           ((< seconds-difference (* 1.5 60 60 24 7))
            (cons "1 week ago"
                  (* 1.5 60 60 24 7))) ;; a week and a half
           ((< seconds-difference (* 60 60 24 7 52))
            (if (= 52 (floor (+ 0.5 (/ seconds-difference 60 60 24 7))))
                (cons "52 weeks ago"
                      (* 60 60 24 7 52))
              (funcall regular-response seconds-difference (* 60 60 24 7) "week")))
           ((< seconds-difference (* 1.5 60 60 24 365))
            (cons "1 year ago"
                  (* 60 60 24 365 1.5))) ;; a year and a half
           (t
            (funcall regular-response seconds-difference (* 60 60 24 365.25) "year")))))
    (cons (car relative-result)
          (time-add timestamp (seconds-to-time (cdr relative-result))))))

(defun fedi--relative-time-description (timestamp &optional current-time)
  "Return a string with a human readable TIMESTAMP relative to the current time.
Use the optional CURRENT-TIME as the current time (only used for
reliable testing).
E.g. this could return something like \"1 min ago\", \"yesterday\", etc.
TIME-STAMP is assumed to be in the past."
  (car (fedi--relative-time-details timestamp current-time)))

;;; LIVE BUFFERS

(defun fedi-live-buffers (prefix)
  "Return a list of all live buffers with PREFIX in their name."
  (cl-loop for b in (buffer-list)
           when (string-prefix-p prefix (buffer-name b))
           collect (get-buffer b)))

(defun fedi-kill-all-buffers (prefix)
  "Kill any and all open fedi buffers, hopefully."
  (let ((fedi-buffers (fedi-live-buffers prefix)))
    (cl-loop for x in fedi-buffers
             do (kill-buffer x))))

(defun fedi-switch-to-buffer (prefix)
  "Switch to a live fedi buffer."
  (let* ((bufs (fedi-live-buffers prefix))
         (buf-names (mapcar #'buffer-name bufs))
         (choice (completing-read "Switch to buffer: "
                                  buf-names)))
    (switch-to-buffer choice)))

;; ACTION/RESPONSE

(defun fedi-do-item-completing (fetch-fun list-fun prompt action-fun)
  "Fetch items, choose one, and do an action.
FETCH-FUN is the function to fetch data.
LIST-FUN is called on the data to return a collection for
`completing-read'. It should return a string (name, handle) as
its first element, and an id as second element. A third element
will be used as an annotation.
PROMPT is for the same.
ACTION-FUN is called with 2 args: the chosen item's id and the
candidate's car, a string, usually its name or a handle."
  (let* ((data (funcall fetch-fun))
         (list (funcall list-fun data))
         (completion-extra-properties
          (when list
            (list :annotation-function
                  (lambda (i)
                    (let ((annot (nth 2 (assoc i list #'equal))))
                      (concat
                       (propertize " " 'display
                                   '(space :align-to (- right-margin 51)))
                       (string-limit (car (string-lines annot)) 50)))))))
         (choice (when list (completing-read prompt list)))
         (id (when list (nth 1 (assoc choice list #'equal)))))
    (if (not list)
        (user-error "No items returned")
      (funcall action-fun id choice))))

(defun fedi-response-msg (response &optional key value format-str)
  "Check RESPONSE, JSON from the server, and message on success.
Used to handle server responses after the user
does some action, such as subscribing, blocking, etc.
KEY returns the value of a field from RESPONSE, using `alist-get'.
VALUE specifies how to check the value: possible values are
:non-nil, :json-false and t.
FORMAT-STR is passed to message if the value check passes.
If the check doesn't pass, error.
Currently we error if we receive incorrect KEY or CHECK args,
even though the request may have succeeded."
  (if (stringp response) ; a string is an error
      (error "Error: %s" response))
  (let ((field (alist-get key response)))
    (cond ((eq value :non-nil) ; value json exists
           (if field
               (message format-str)
             (error "Error")))
          ((or (eq value t) ; json val t or :json-false
               (eq value :json-false))
           (if (eq field value)
               (message format-str)
             (error "Error")))
          (t
           (error "Error handling response data, but request succeeded")))))

(provide 'fedi)
;;; fedi.el ends here
