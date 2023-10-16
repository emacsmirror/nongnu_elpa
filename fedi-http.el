;;; fedi-http.el --- HTTP request/response functions  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Marty Hiatt and mastodon.el authors
;; Author: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.0.2
;; Homepage: https://codeberg.org/martianh/fedi.el

;; This file is not part of GNU Emacs.

;; This file is part of fedi.el

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

;; fed-http.el provides HTTP request/response convenience functions.

;; All request functions aim to have regular arguments: a URL, a parameters
;; alist, optional headers if appropriate, callback if async, etc.

;; There are also helpfer functions for constructing query parameters and
;; array parameter lists.

;; Code from mastodon-http.el, see its boilerplate for authorship, etc.

;;; Code:

(require 'json)
;; (require 'request) ; for attachments upload
(require 'url)

(autoload 'shr-render-buffer "shr")
(autoload 'fedi-auth--access-token "fedi-auth")
(autoload 'fedi-toot--update-status-fields "fedi-toot")

;; (defvar fedi-toot--media-attachment-ids)
;; (defvar fedi-toot--media-attachment-filenames)

(defvar fedi-instance-url)
(defvar fedi-http--api-version)

(defconst fedi-http--timeout 15
  "HTTP request timeout, in seconds.  Has no effect on Emacs < 26.1.")

;; via https://github.com/SqrtMinusOne/reverso.el, thanks Korytov Pavel!
;; to set a user-agent var for your package:
;; (defvar lem-user-agent
;;   (nth (random (length fedi-user-agents))
;;        fedi-user-agents)
;;   "User-Agent to use for requests.")
(defconst fedi-user-agents
  '("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36"
    "Mozilla/5.0 (X11; Linux x86_64; rv:103.0) Gecko/20100101 Firefox/103.0"
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:103.0) Gecko/20100101 Firefox/103.0"
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36"
    "Mozilla/5.0 (Windows NT 10.0; rv:103.0) Gecko/20100101 Firefox/103.0"
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36"
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.6 Safari/605.1.15"
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:103.0) Gecko/20100101 Firefox/103.0")
  "User-Agents to use for reverso.el requests.
A random one is picked at package initialization.")

(defun fedi-http--api (endpoint &optional url ver-str)
  "Return Fedi API URL for ENDPOINT."
  (concat (or url fedi-instance-url) "/api/"
          (or ver-str fedi-http--api-version)
          "/" endpoint))

(defun fedi-http--response ()
  "Capture response buffer content as string."
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun fedi-http--response-body (pattern)
  "Return substring matching PATTERN from `fedi-http--response'."
  (let ((resp (fedi-http--response)))
    (string-match pattern resp)
    (match-string 0 resp)))

(defun fedi-http--status ()
  "Return HTTP Response Status Code from `fedi-http--response'."
  (let* ((status-line (fedi-http--response-body "^HTTP/1.*$")))
    (string-match "[0-9][0-9][0-9]" status-line)
    (match-string 0 status-line)))

(defun fedi-http--url-retrieve-synchronously (url &optional silent)
  "Retrieve URL asynchronously.
This is a thin abstraction over the system
`url-retrieve-synchronously'.  Depending on which version of this
is available we will call it with or without a timeout.
SILENT means don't message."
  (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
      (url-retrieve-synchronously url)
    (url-retrieve-synchronously url (or silent nil) nil fedi-http--timeout)))

(defun fedi-http--triage (response success)
  "Determine if RESPONSE was successful.
Call SUCCESS if successful. Message status and JSON error from
RESPONSE if unsuccessful."
  (let ((status (with-current-buffer response
                  (fedi-http--status))))
    (if (string-prefix-p "2" status)
        (funcall success)
      (if (string-prefix-p "404" status)
          (message "Error %s: page not found" status)
        (let ((json-response (with-current-buffer response
                               (fedi-http--process-json))))
          (message "Error %s: %s" status (alist-get 'error json-response)))))))

(defun fedi-http--read-file-as-string (filename)
  "Read a file FILENAME as a string. Used to generate image preview."
  (with-temp-buffer
    (insert-file-contents filename)
    (string-to-unibyte (buffer-string))))

(defun fedi-http--build-params-string (params)
  "Build a request parameters string from parameters alist PARAMS."
  ;; (url-build-query-string args nil))
  ;; url-build-query-string adds 'nil' for empty params so lets stick with our
  ;; own:
  (mapconcat (lambda (p)
               (concat (url-hexify-string (car p))
                       "=" (url-hexify-string (cdr p))))
             params "&"))

(defun fedi-http--build-array-params-alist (param-str array)
  "Return parameters alist using PARAM-STR and ARRAY param values.
Used for API form data parameters that take an array."
  (cl-loop for x in array
           collect (cons param-str x)))

(defun fedi-http--post (url &optional params headers json)
  "POST synchronously to URL, optionally with PARAMS and HEADERS.
JSON means we are posting a JSON payload, so we add headers and
json-string PARAMS."
  (let* ((url-request-data
          (when params
            (if json
                (json-encode params)
              (fedi-http--build-params-string params))))
         ;; TODO: perhaps leave these headers to the package now that
         ;; `fedi-request' takes header args?
         (headers (when json
                    (append headers
                            '(("Content-Type" . "application/json")
                              ("Accept" . "application/json")))))
         (url-request-extra-headers
          (append url-request-extra-headers ; set in macro
                  headers)))
    (with-temp-buffer
      (fedi-http--url-retrieve-synchronously url))))

(defun fedi-http--concat-params-to-url (url params)
  "Build a query string with PARAMS and concat to URL."
  (if params
      (concat url "?"
              (fedi-http--build-params-string params))
    url))

(defun fedi-http--get (url &optional params silent)
  "Make synchronous GET request to URL.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message."
  (let ((url (fedi-http--concat-params-to-url url params)))
    (fedi-http--url-retrieve-synchronously url silent)))

(defun fedi-http--get-response (url &optional params no-headers silent vector)
  "Make synchronous GET request to URL. Return JSON and response headers.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
NO-HEADERS means don't collect http response headers.
VECTOR means return json arrays as vectors."
  (with-current-buffer (fedi-http--get url params silent)
    (fedi-http--process-response no-headers vector)))

(defun fedi-http--get-json (url &optional params silent vector)
  "Return only JSON data from URL request.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
VECTOR means return json arrays as vectors."
  (car (fedi-http--get-response url params :no-headers silent vector)))

(defun fedi-http--process-json ()
  "Return only JSON data from async URL request.
Callback to `fedi-http--get-json-async', usually
`fedi-tl--init*', is run on the result."
  (car (fedi-http--process-response :no-headers)))

(defun fedi-http--render-html-err (string)
  "Render STRING as HTML in a temp buffer.
STRING should be HTML for a 404 errror."
  (with-temp-buffer
    (insert string)
    (shr-render-buffer (current-buffer))
    (view-mode))) ; for 'q' to kill buffer and window
;; FIXME: this is awful, it pops up also:
;; (error ""))) ; stop subsequent processing

(defun fedi-http--process-response (&optional no-headers vector)
  "Process http response.
Return a cons of JSON list and http response headers.
If response is HTML, it's likely an error so render it with shr.
If NO-HEADERS is non-nil, just return the JSON.
VECTOR means return json arrays as vectors.
Callback to `fedi-http--get-response-async'."
  ;; view raw response:
  ;; (switch-to-buffer (current-buffer))
  (let ((headers (unless no-headers
                   (fedi-http--process-headers))))
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-array-type (if vector 'vector 'list))
          (json-string (decode-coding-string
                        (buffer-substring-no-properties (point) (point-max))
                        'utf-8)))
      (kill-buffer)
      (cond ((or (string-empty-p json-string) (null json-string))
             nil)
            ;; if we get html, just render it and error:
            ;; ideally we should handle the status code in here rather than
            ;; this crappy hack?
            ((string-prefix-p "\n<" json-string) ; html hack
             (fedi-http--render-html-err json-string))
            ;; if no json or html, maybe we have a plain string error message
            ;; (misskey does this, but there are probably better ways to do
            ;; this):
            ((not (or (string-prefix-p "\n{" json-string)
                      (string-prefix-p "\n[" json-string)))
             (error "%s" json-string))
            (t
             `(,(json-read-from-string json-string) . ,headers))))))

(defun fedi-http--process-headers ()
  "Return an alist of http response headers."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let* ((head-str (buffer-substring-no-properties
                    (point-min)
                    (re-search-forward "^$" nil 'move)))
         (head-list (split-string head-str "\n")))
    (mapcar (lambda (x)
              (let ((list (split-string x ": ")))
                (cons (car list) (cadr list))))
            head-list)))

(defun fedi-http--delete (url &optional params)
  "Make DELETE request to URL.
PARAMS is an alist of any extra parameters to send with the request."
  ;; url-request-data only works with POST requests?
  (let ((url (fedi-http--concat-params-to-url url params)))
    (with-temp-buffer
      (fedi-http--url-retrieve-synchronously url))))

(defun fedi-http--put (url &optional params headers json)
  "Make PUT request to URL.
PARAMS is an alist of any extra parameters to send with the request.
HEADERS is an alist of any extra headers to send with the request.
If JSON, encode params as JSON."
  (let* ((url-request-data
          (when params
            (if json
                (json-encode params)
              (fedi-http--build-params-string params))))
         (headers (when json
                    (append headers
                            '(("Content-Type" . "application/json")
                              ("Accept" . "application/json")))))
         (url-request-extra-headers
          (append url-request-extra-headers ; auth set in macro
                  headers)))
    (with-temp-buffer
      (fedi-http--url-retrieve-synchronously url))))

(defun fedi-http--patch-json (url &optional params json)
  "Make synchronous PATCH request to URL. Return JSON response.
Optionally specify the PARAMS to send."
  (with-current-buffer (fedi-http--patch url params json)
    (fedi-http--process-json)))

(defun fedi-http--patch (url &optional params json)
  "Make synchronous PATCH request to BASE-URL.
Optionally specify the PARAMS to send."
  (let* ((url-request-data
          (when params
            (if json
                (json-encode params)
              (fedi-http--build-params-string params))))
         ;; (url (fedi-http--concat-params-to-url base-url params)))))
         (headers (when json
                    '(("Content-Type" . "application/json")
                      ("Accept" . "application/json"))))
         (url-request-extra-headers
          (append url-request-extra-headers headers)))
    (fedi-http--url-retrieve-synchronously url)))

 ;; Asynchronous functions

(defun fedi-http--get-async (url &optional params callback &rest cbargs)
  "Make GET request to URL.
Pass response buffer to CALLBACK function with args CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (let ((url (fedi-http--concat-params-to-url url params)))
    (url-retrieve url callback cbargs)))

(defun fedi-http--get-response-async (url &optional params callback &rest cbargs)
  "Make GET request to URL. Call CALLBACK with http response and CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (fedi-http--get-async
   url
   params
   (lambda (status)
     (when status ; for flakey servers
       (apply callback (fedi-http--process-response) cbargs)))))

(defun fedi-http--get-json-async (url &optional params callback &rest cbargs)
  "Make GET request to URL. Call CALLBACK with json-list and CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (fedi-http--get-async
   url
   params
   (lambda (status)
     (when status ;; only when we actually get sth?
       (apply callback (fedi-http--process-json) cbargs)))))

(defun fedi-http--post-async (url params _headers
                                  &optional callback &rest cbargs)
  "POST asynchronously to URL with PARAMS and HEADERS.
Then run function CALLBACK with arguements CBARGS."
  (let (;(request-timeout 5)
        (url-request-data (when params
                            (fedi-http--build-params-string params))))
    (with-temp-buffer
      (url-retrieve url callback cbargs))))

;; ;; TODO: test for curl first?
;; (defun fedi-http--post-media-attachment (url filename caption)
;;   "Make POST request to upload FILENAME with CAPTION to the server's media URL.
;; The upload is asynchronous. On succeeding,
;; `fedi-toot--media-attachment-ids' is set to the id(s) of the
;; item uploaded, and `fedi-toot--update-status-fields' is run."
;;   (let* ((file (file-name-nondirectory filename))
;;          (request-backend 'curl))
;;     (request
;;       url
;;       :type "POST"
;;       :params `(("description" . ,caption))
;;       :files `(("file" . (,file :file ,filename
;;                                 :mime-type "multipart/form-data")))
;;       :parser 'json-read
;;       :headers `(("Authorization" . ,(concat "Bearer "
;;                                              (fedi-auth--access-token))))
;;       :sync nil
;;       :success (cl-function
;;                 (lambda (&key data &allow-other-keys)
;;                   (when data
;;                     (push (alist-get 'id data)
;;                           fedi-toot--media-attachment-ids) ; add ID to list
;;                     (message "%s file %s with id %S and caption '%s' uploaded!"
;;                              (capitalize (alist-get 'type data))
;;                              file
;;                              (alist-get 'id data)
;;                              (alist-get 'description data))
;;                     (fedi-toot--update-status-fields))))
;;       :error (cl-function
;;               (lambda (&key error-thrown &allow-other-keys)
;;                 (cond
;;                  ;; handle curl errors first (eg 26, can't read file/path)
;;                  ;; because the '=' test below fails for them
;;                  ;; they have the form (error . error message 24)
;;                  ((not (proper-list-p error-thrown)) ; not dotted list
;; 		  (message "Got error: %s. Shit went south." (cdr error-thrown)))
;;                  ;; handle fedi api errors
;;                  ;; they have the form (error http 401)
;; 		 ((= (car (last error-thrown)) 401)
;;                   (message "Got error: %s Unauthorized: The access token is invalid"
;;                            error-thrown))
;;                  ((= (car (last error-thrown)) 422)
;;                   (message "Got error: %s Unprocessable entity: file or file\
;;  type is unsupported or invalid"
;;                            error-thrown))
;;                  (t
;;                   (message "Got error: %s Shit went south"
;;                            error-thrown))))))))

(provide 'fedi-http)
;;; fedi-http.el ends here
