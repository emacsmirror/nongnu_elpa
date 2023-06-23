;;; fedi.el --- Helper functions for fediverse clients  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Marty Hiatt and mastodon.el authors
;; Author: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))
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

(defmacro fedi-request
    (method name endpoint
            &optional args docstring params man-params json headers)
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
              (params-alist (remove nil
                                    (list ,@(fedi-make-params-alist params))))
              (params (if ',man-params
                          (append ,man-params params-alist)
                        params-alist))
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

(defun fedi-arg-when-expr (arg)
  (let ((str
         (string-replace "-" "_" ; for "type_"
                         (symbol-name arg))))
    ;; FIXME: when the when test fails, it adds nil to the list in the expansion.
    ;; so we have to call (remove nil) on the result.
    `(when ,arg
       (cons ,str
             ;;(symbol-name arg)
             ,arg))))

;; (fedi-arg-when-expr 'sort)

(defun fedi-make-params-alist (args)
  (cl-loop while args
           collecting (fedi-arg-when-expr (pop args))))

;; (fedi-make-params-alist '(sort type))

(provide 'fedi)
;;; fedi.el ends here
