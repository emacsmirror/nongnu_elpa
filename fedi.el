;;; fedi.el --- Helper functions for fediverse clients  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Marty Hiatt and mastodon.el authors
;; Author: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.1"))
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
(defmacro fedi-request
    (method name endpoint
            &optional args docstring params man-params opt-bools json headers)
  "Create a http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function.
PARAMS is an list of elements from which to build an alist of
form parameters to send with the request.
MAN-PARAMS is an alist, to append to the one created from PARAMS.
JSON means to encode params as a JSON payload.
HEADERS is an alist that will be bound as `url-request-extra-headers'.

This macro is designed to generate functions for fetching data
from JSON APIs.

To use it, you first need to set `fedi-package-prefix' to the
name of your package, and set `fedi-instance-url' to the URL of
an instance of your fedi service.

The name of functions generated with this will be the result of:
\(concat fedi-package-prefix \"-\" name).

The full URL for the endpoint is constructed by `fedi-http--api',
which see. ENDPOINT does not require a preceding slash.

For example, to make a GET request, called PKG-search to endpoint /search:

\(fedi-request \"get\" \"search\" \"search\"
  (q)
  \"Make a GET request.
Q is the search query.\"
  \\=(q))

This macro doesn't handle authenticated requests, as these differ
between services. But you can easily wrap it in another macro
that handles auth by providing info using HEADERS or AUTH-PARAM."
  (declare (debug t)
           (indent 3))
  (let ((req-fun (intern (concat "fedi-http--" method))))
    `(defun ,(intern (concat fedi-package-prefix "-" name)) ,args
       ,docstring
       (let* ((req-url (fedi-http--api ,endpoint))
              (url-request-method ,(upcase method))
              (url-request-extra-headers ,headers)
              (bools (remove nil
                             (list ,@(fedi-make-params-alist
                                      opt-bools #'fedi-arg-when-boolean))))
              (params-alist (remove nil
                                    (list ,@(fedi-make-params-alist
                                             params #'fedi-arg-when-expr))))
              (params (if ',man-params
                          (append ,man-params params-alist)
                        params-alist))
              (params (append params bools))
              (response
               (cond ((or (equal ,method "post")
                          (equal ,method "put"))
                      ;; FIXME: deal with headers nil arg here:
                      (funcall #',req-fun req-url params nil ,json))
                     (t
                      (funcall #',req-fun req-url params)))))
         (fedi-http--triage response
                            (lambda ()
                              (with-current-buffer response
                                (fedi-http--process-json))))))))

;; This trick doesn't actually do what we want, as our macro is called
;; to define functions, so must be called with all possible arguments, rather
;; than only those of a given function call.
;; Still, it solves the problem of the server rejecting nil param values.
(defun fedi-arg-when-expr (arg)
  "Return a cons of a string and a symbol type of ARG.
Also replace _ with - (for Lemmy's type_ param)."
  (let ((str
         (string-replace "-" "_" ; for "type_" etc.
                         (symbol-name arg))))
    ;; FIXME: when the when test fails, it adds nil to the list in the
    ;; expansion, so we have to call (remove nil) on the result.
    `(when ,arg
       (cons ,str ,arg))))

;; (fedi-arg-when-expr 'sort)

(defun fedi-make-params-alist (args fun)
  "Call FUN on each of ARGS."
  (cl-loop while args
           collecting (funcall fun (pop args))))

;; (fedi-make-params-alist '(sort type))

(defun fedi-arg-when-boolean (arg)
  "ARG."
  (let ((str
         (string-replace "-" "_"
                         (symbol-name arg))))
    `(when ,arg (cons ,str "true"))))


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

(defun fedi--goto-pos (fun &optional refresh pos)
  "Search for item with FUN.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (let* ((npos (funcall fun
                        (or pos (point))
                        'byline-top
                        (current-buffer))))
    (if npos
        (if (not (get-text-property npos 'byline-top))
            (fedi--goto-pos fun refresh npos)
          (goto-char npos))
      (funcall refresh))))

(defun fedi-next-item ()
  "Move to next item."
  (interactive)
  (fedi--goto-pos #'next-single-property-change)) ;#'fedi-ui-more))

(defun fedi-prev-item ()
  "Move to prev item."
  (interactive)
  (fedi--goto-pos #'previous-single-property-change))

;;; HEADINGS

(defvar fedi-horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

(defun fedi-format-heading (name)
  "Format a heading for NAME, a string."
  (propertize
   (concat " " fedi-horiz-bar "\n "
           (upcase name)
           "\n " fedi-horiz-bar "\n")
   'face 'success))

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
    (verified  . ("" . "V"))
    (locked    . ("🔒" . "[locked]"))
    (private   . ("🔒" . "[followers]"))
    (direct    . ("✉" . "[direct]"))
    (edited    . ("✍" . "[edited]"))
    (upvote    . ("⬆" . "[upvotes]"))
    (person    . ("👤" . "[people]"))
    (pinned    . ("📌" . "[pinned]"))
    (replied   . ("⬇" . "↓"))
    (community . ("👪" . "[community]"))
    (reply-bar . ("┃" . "|")))
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
          (string-match "^/u/[_[:alpha:]]+$" query)
          (string-match "^/c/[_[:alnum:]]+$" query)
          (string-match "^/post/[[:digit:]]+$" query)
          (string-match "^/comment/[[:digit:]]+$" query)))))


(provide 'fedi)
;;; fedi.el ends here
