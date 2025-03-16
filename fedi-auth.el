;;; fedi.el --- Auth utilities -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Marty Hiatt
;; Author: Marty Hiatt <mousebot@disroot.org>
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

;; client/user authentication utilities

;;; Code:

(require 'auth-source)

(defmacro fedi-auth-authorized-request (method token body
                                               &optional unauthenticated-p)
  "Make a METHOD request, with auth TOKEN.
Call BODY. If UNAUTHENTICATED-P is non-nil, don't set token in the auth
header."
  (declare (debug 'body)
           (indent 2))
  `(let ((url-request-method ,method)
         (url-request-extra-headers
          (unless ,unauthenticated-p
            (list (cons "Authorization"
                        (concat "token " ,token))))))
     ,body))

(defun fedi-auth-source-get (user host &optional create)
  "Fetch an auth source token, searching with USER and HOST.
If CREATE, prompt for a token and save it if there is no such entry.
Return a list of user, password/secret, and the item's save-function."
  (let* ((auth-source-creation-prompts
          '((secret . "%u access token: ")))
         (source
          (car
           (auth-source-search :host host :user user
                               :require '(:user :secret)
                               ;; "create" doesn't work here!:
                               :create (if create t nil)))))
    (when source
      (let ((creds
             `(,(plist-get source :user)
               ,(auth-info-password source)
               ,(plist-get source :save-function))))
        ;; FIXME: is this ok to be here?
        (when create ;; call save function:
          (when (functionp (nth 2 creds))
            (funcall (nth 2 creds))))
        creds))))

(provide 'fedi-auth)
;;; fedi-auth.el ends here
