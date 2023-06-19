;;; fedi.el --- helper functions for fediverse clients  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.0.1
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

;; fedi.el

;;; Code:

(require 'fedi-http)

;;; MACRO
(defvar fedi-package-prefix nil
  "The name of your package, without following dash. Used to
construct function names in `fedi-request'.")

;; this macro can be used for quickly creating request commands to endpoints.
;; see the examples in lem.el. it doesn't handle authenticated requests, as
;; these differ highly between services. but you can easily modify it to
;; incorporate auth tokens. see `lem-request' in lem.el, or
;; `mastodon-http--authorized-request' for examples of how to handle this.

;; maybe if you add extra-headers, extra-params args here, you can handle
;; various auth types. you cd wrap it with a library specific macro to
;; abstract out the auth stuff from all your functions.

;; e.g. masto:
;; (let (((url-request-extra-headers
;;         (unless ,unauthenticated-p
;;           (list (cons "Authorization"
;;                       (concat "Bearer " (mastodon-auth--access-token)))))))))

;; eg lem:
;; (params (unless ,no-auth
;;           (append `(("auth" . ,lem-auth-token))
;;                   ,params)))

;; and then you can remove  `mastodon-http--authorized-request'

(defmacro fedi-request
    (method name endpoint &optional args params auth-param json headers)
  "Create http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function.
PARAMS is an alist of form parameters to send with the request.
AUTH-PARAM is a single-item alist, to append to params. It is a
separate arg so that this macro can be wrapped with another one
handling auth for all functions that need it.
JSON means to encode params as a JSON payload.
HEADERS is an alist that will be bound as `url-request-extra-headers'.
To use this macro, you first need to set `fedi-package-prefix' to
the name of your package."
  (declare (debug t)
           (indent 1))
  (let ((req-fun (intern (concat "fedi-http--" method))))
    `(defun ,(intern (concat fedi-package-prefix "-" name)) ,args
       (let* ((url (fedi-http--api ,endpoint))
              (url-request-method ,(upcase method))
              (url-request-extra-headers ,headers)
              (params (if ,auth-param
                          (append ,auth-param ,params)
                        ,params))
              (response
               (cond ((or (equal ,method "post")
                          (equal ,method "put"))
                      (funcall #',req-fun url params nil ,json))
                     ((equal ,method "get")
                      (funcall #',req-fun url params)))))
         ;; FIXME: ideally here we would handle 404/500 responses as html if
         ;; its returned
         (fedi-http--triage response
                            (lambda ()
                              (with-current-buffer response
                                ;; (fedi-http--process-json)
                                (fedi-http--process-response :no-headers))))))))

(provide 'fedi)
;;; fedi.el ends here
