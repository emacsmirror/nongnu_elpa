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
(defmacro fedi-request (method name endpoint &optional args params json)
  "Create http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function, PARAMS is an alist of form parameters.
JSON means to send params as a JSON payload.
Before calling this, set `fedi-package-prefix' to the name of your package."
  (declare (debug t)
           (indent 1))
  (let ((req-fun (intern (concat "fedi-http--" method))))
    `(defun ,(intern (concat fedi-package-prefix "-" name)) ,args
       (let* ((url (fedi-http--api ,endpoint))
              (response
               (cond ((or (equal ,method "post")
                          (equal ,method "put"))
                      (funcall #',req-fun url ,params nil :unauthed ,json))
                     ((equal ,method "get")
                      (funcall #',req-fun url ,params)))))
         ;; FIXME: ideally here we would handle 404/500 responses as html, as
         ;; in `fedi-http--process-response'. perhaps its code needs to move
         ;; io to `fedi-http--triage'? or it's just that lemmy, my testing
         ;; ground, has no html for 404 responses like some masto servers do
         (fedi-http--triage response
                            (lambda ()
                              (with-current-buffer response
                                (fedi-http--process-json))))))))


(provide 'fedi)
;;; fedi.el ends here
