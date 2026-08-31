;;; jabber-link-preview.el --- local previews for shared links  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Fetch and parse compact, local-only previews for HTTPS links.  Link
;; metadata is derived on demand and is never added to XMPP stanzas.

;;; Code:

(require 'dom)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defgroup jabber-link-preview nil
  "Local previews for links in chat messages."
  :group 'jabber-chat)

(defcustom jabber-link-preview-max-html-bytes (* 256 1024)
  "Maximum HTML response size accepted for a link preview."
  :type 'natnum
  :group 'jabber-link-preview)
(defcustom jabber-link-preview-title-length 200
  "Maximum number of characters displayed from a preview title."
  :type 'natnum
  :group 'jabber-link-preview)
(defcustom jabber-link-preview-description-length 500
  "Maximum number of characters displayed from a preview description."
  :type 'natnum
  :group 'jabber-link-preview)
(defcustom jabber-link-preview-program "curl"
  "Program used to retrieve link preview HTML."
  :type 'string
  :group 'jabber-link-preview)
(defcustom jabber-link-preview-timeout 8
  "Maximum number of seconds allowed for a preview retrieval."
  :type 'natnum
  :group 'jabber-link-preview)

(defconst jabber-link-preview--response-overhead-bytes (* 64 1024)
  "Maximum response-header bytes retained before a preview body.")

(defconst jabber-link-preview--url-regexp
  "https://[^ \t\n<>\"]+"
  "Regexp matching candidate preview URLs.")
(defconst jabber-link-preview--image-regexp
  "\\.\\(?:avif\\|gif\\|jpe?g\\|png\\|svg\\|tiff?\\|webp\\)\\(?:[?#].*\\)?\\'"
  "Regexp matching URLs that are already handled as inline images.")

(defvar jabber-link-preview--cache (make-hash-table :test #'equal)
  "Session cache mapping URLs to metadata plists or loading tokens.")

(defun jabber-link-preview--clean-text (text &optional limit)
  "Normalize whitespace in TEXT and truncate it to LIMIT characters."
  (when-let* ((text (and text
                         (string-trim
                          (replace-regexp-in-string
                           "[ \t\n\r]+" " " text))))
              ((not (string-empty-p text))))
    (if limit
        (truncate-string-to-width text limit nil nil "…")
      text)))
(defun jabber-link-preview--meta-content (dom attribute value)
  "Return content of the first meta node in DOM whose ATTRIBUTE is VALUE."
  (when-let* ((node (seq-find
                     (lambda (candidate)
                       (equal (dom-attr candidate attribute) value))
                     (dom-by-tag dom 'meta))))
    (dom-attr node 'content)))
(defun jabber-link-preview--title (dom)
  "Return the preferred title from DOM."
  (or (jabber-link-preview--meta-content dom 'property "og:title")
      (when-let* ((node (car (dom-by-tag dom 'title))))
        (dom-texts node))))
(defun jabber-link-preview--absolute-url (url base-url)
  "Resolve URL against BASE-URL, returning nil when URL is empty."
  (when-let* ((url (jabber-link-preview--clean-text url)))
    (url-expand-file-name url base-url)))
(defun jabber-link-preview-parse-html (html url)
  "Parse HTML fetched from URL and return a preview metadata plist.
Return nil when the document has no usable title."
  (with-temp-buffer
    (insert html)
    (when-let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                (title (jabber-link-preview--clean-text
                        (jabber-link-preview--title dom)
                        jabber-link-preview-title-length)))
      (let ((site (jabber-link-preview--clean-text
                   (or (jabber-link-preview--meta-content
                        dom 'property "og:site_name")
                       (url-host (url-generic-parse-url url)))))
            (description
             (jabber-link-preview--clean-text
              (or (jabber-link-preview--meta-content
                   dom 'property "og:description")
                  (jabber-link-preview--meta-content
                   dom 'name "description"))
              jabber-link-preview-description-length))
            (image (jabber-link-preview--absolute-url
                    (jabber-link-preview--meta-content
                     dom 'property "og:image")
                    url))
            (image-alt (jabber-link-preview--clean-text
                        (jabber-link-preview--meta-content
                         dom 'property "og:image:alt"))))
        (append (list :url url :site site :title title)
                (and description (list :description description))
                (and image (list :image image))
                (and image-alt (list :image-alt image-alt)))))))
(defun jabber-link-preview--parse-result (html url)
  "Return parsed preview metadata for HTML at URL or an error plist."
  (if (not (fboundp 'libxml-parse-html-region))
      (list :error 'parser-unavailable)
    (condition-case nil
        (or (jabber-link-preview-parse-html html url)
            (list :error 'metadata))
      (error (list :error 'parser)))))

(defun jabber-link-preview--youtube-oembed-url (url)
  "Return YouTube's oEmbed endpoint for watch URL, or nil."
  (let* ((parsed (url-generic-parse-url url))
         (host (downcase (or (url-host parsed) ""))))
    (when (and (member host '("youtube.com" "www.youtube.com"))
               (string-prefix-p "/watch?" (url-filename parsed)))
      (format "https://www.youtube.com/oembed?url=%s&format=json"
              (url-hexify-string url)))))

(defun jabber-link-preview--parse-youtube-json (json url)
  "Return preview metadata parsed from YouTube oEmbed JSON for URL."
  (condition-case nil
      (let* ((data (json-parse-string json :object-type 'plist
                                      :null-object nil :false-object nil))
             (title (plist-get data :title)))
        (when title
          (append (list :url url
                        :site (or (plist-get data :provider_name) "YouTube")
                        :title title)
                  (when-let* ((author (plist-get data :author_name)))
                    (list :description author))
                  (when-let* ((image (plist-get data :thumbnail_url)))
                    (list :image image)))))
    (error nil)))

(defun jabber-link-preview--trim-url (url)
  "Remove common sentence punctuation from the end of URL."
  (replace-regexp-in-string "[.,;:!?)}\\]]+\\'" "" url))
(defun jabber-link-preview--private-ipv4-p (address)
  "Return non-nil when IPv4 ADDRESS vector is not globally routable."
  (let ((a (aref address 0))
        (b (aref address 1)))
    (or (= a 0) (= a 10) (= a 127)
        (and (= a 100) (<= 64 b) (<= b 127))
        (and (= a 169) (= b 254))
        (and (= a 172) (<= 16 b) (<= b 31))
        (and (= a 192) (= b 0))
        (and (= a 192) (= b 168))
        (and (= a 198) (or (= b 18) (= b 19) (= b 51)))
        (and (= a 203) (= b 0))
        (>= a 224))))
(defun jabber-link-preview--public-address-p (address)
  "Return non-nil when IPv4 ADDRESS vector is globally routable."
  (and (= (length address) 5)
       (not (jabber-link-preview--private-ipv4-p address))))
(defun jabber-link-preview-safe-url-p (url &optional resolve)
  "Return non-nil when URL is an eligible public HTTPS URL.
When RESOLVE is non-nil, resolve its host and require every answer to
be globally routable."
  (when-let* ((parsed (url-generic-parse-url url))
              ((equal (url-type parsed) "https"))
              ((memq (url-port parsed) '(nil 443)))
              (host (url-host parsed))
              ((not (string-match-p
                     "\\`\\(?:localhost\\|.*\\.localhost\\)\\'"
                     (downcase host))))
              (addresses (or (and resolve
                                  (network-lookup-address-info host 'ipv4))
                             (and (string-match-p "\\`[0-9.]+\\'" host)
                                  (network-lookup-address-info
                                   host nil 'numeric))
                             (and (not resolve) '(unchecked)))))
    (or (eq (car addresses) 'unchecked)
        (seq-every-p #'jabber-link-preview--public-address-p addresses))))
(defun jabber-link-preview--resolved-address (url)
  "Return one vetted address string for URL, or nil."
  (when-let* ((host (url-host (url-generic-parse-url url)))
              (addresses (network-lookup-address-info host 'ipv4))
              ((seq-every-p #'jabber-link-preview--public-address-p
                            addresses)))
    (format-network-address (car addresses) t)))
(defun jabber-link-preview-url (text)
  "Return the first eligible non-image preview URL in TEXT."
  (when (stringp text)
    (let ((start 0)
          found)
      (while (and (not found)
                  (string-match jabber-link-preview--url-regexp text start))
        (let ((url (jabber-link-preview--trim-url (match-string 0 text))))
          (when (and (jabber-link-preview-safe-url-p url)
                     (not (string-match-p jabber-link-preview--image-regexp
                                          (downcase url))))
            (setq found url))
          (setq start (match-end 0))))
      found)))

(defun jabber-link-preview-format (metadata)
  "Return a compact propertized preview card for METADATA."
  (let* ((url (plist-get metadata :url))
         (site (or (plist-get metadata :site) url))
         (lines (delq nil (list site
                                (plist-get metadata :title)
                                (plist-get metadata :description))))
         (card (concat "\n" (mapconcat (lambda (line) (concat "│ " line))
                                        lines "\n"))))
    (add-text-properties
     1 (length card)
     (list 'jabber-chat-link-preview-url url
           'mouse-face 'highlight
           'help-echo "RET: open link; w: copy URL")
     card)
    (put-text-property 1 (+ 2 (length site)) 'face 'shadow card)
    card))

(defun jabber-link-preview-get (url)
  "Return cached preview metadata or state for URL."
  (gethash url jabber-link-preview--cache))
(defun jabber-link-preview-put (url value)
  "Cache VALUE for URL and return VALUE."
  (puthash url value jabber-link-preview--cache)
  value)

(defun jabber-link-preview--curl-command (url address)
  "Return the curl command for URL pinned to ADDRESS."
  (let* ((host (url-host (url-generic-parse-url url)))
         (pin-address (if (string-match-p ":" address)
                          (format "[%s]" address)
                        address)))
    (list jabber-link-preview-program
          "--silent" "--show-error"
          "--proto" "=https"
          "--noproxy" "*"
          "--connect-timeout" (number-to-string jabber-link-preview-timeout)
          "--max-time" (number-to-string jabber-link-preview-timeout)
          "--resolve" (format "%s:443:%s" host pin-address)
          "--header" "Accept: text/html"
          "--header" (format "Range: bytes=0-%d"
                               jabber-link-preview-max-html-bytes)
          "--include" "--url" url)))

(defun jabber-link-preview--abort-oversize (process)
  "Mark PROCESS oversized and stop it."
  (process-put process 'jabber-link-preview-too-large t)
  (when (process-live-p process)
    (delete-process process)))
(defun jabber-link-preview--header-end (process)
  "Return cached response header end for PROCESS, finding it if needed."
  (or (process-get process 'jabber-link-preview-header-end)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (process-put process 'jabber-link-preview-header-end (point))
          (point)))))

(defun jabber-link-preview--header-value (name end)
  "Return response header NAME before END in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^%s:[ \t]*\\([^\r\n;]+\\)" (regexp-quote name))
             end t)
        (string-trim (match-string-no-properties 1))))))
(defun jabber-link-preview--process-filter (process chunk)
  "Insert retrieval CHUNK for PROCESS while enforcing response limits."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert chunk)
      (let ((header-end (jabber-link-preview--header-end process)))
        (cond ((and (null header-end)
                    (> (buffer-size)
                       jabber-link-preview--response-overhead-bytes))
               (delete-region
                (1+ jabber-link-preview--response-overhead-bytes)
                (point-max))
               (jabber-link-preview--abort-oversize process))
              ((and header-end
                    (> header-end
                       jabber-link-preview--response-overhead-bytes))
               (delete-region
                (1+ jabber-link-preview--response-overhead-bytes)
                (point-max))
               (jabber-link-preview--abort-oversize process))
              ((and header-end
                    (> (string-bytes
                        (buffer-substring-no-properties
                         header-end (point-max)))
                       jabber-link-preview-max-html-bytes))
               (delete-region
                (min (point-max)
                     (+ header-end jabber-link-preview-max-html-bytes))
                (point-max))
               (jabber-link-preview--abort-oversize process)))))))

(defun jabber-link-preview--buffer-result (process url)
  "Return preview result from PROCESS buffer for URL."
  (let ((oversized (process-get process 'jabber-link-preview-too-large)))
    (cond ((and (not oversized)
                (not (zerop (process-exit-status process))))
         (list :error 'fetch))
        (t
         (with-current-buffer (process-buffer process)
           (goto-char (point-min))
           (if (not (re-search-forward "\r?\n\r?\n" nil t))
               (list :error (if oversized 'size 'response))
             (let ((body-start (point))
                   (status (save-excursion
                             (goto-char (point-min))
                             (and (looking-at
                                   "HTTP/[0-9.]+ \\([0-9]+\\)")
                                  (string-to-number (match-string 1)))))
                   (content-type
                    (jabber-link-preview--header-value
                     "Content-Type" (point))))
               (cond ((not (and status (<= 200 status) (< status 300)))
                      (list :error 'response))
                     ((not (and content-type
                                (if (eq (process-get process
                                                     'jabber-link-preview-kind)
                                        'youtube)
                                    (string-equal-ignore-case
                                     content-type "application/json")
                                  (string-equal-ignore-case
                                   content-type "text/html"))))
                      (list :error 'mime))
                     (t
                      (let* ((body (buffer-substring-no-properties
                                    body-start (point-max)))
                             (result
                              (if (eq (process-get process
                                                   'jabber-link-preview-kind)
                                      'youtube)
                                  (or (jabber-link-preview--parse-youtube-json
                                       body url)
                                      (list :error 'metadata))
                                (jabber-link-preview--parse-result body url))))
                        (if (and oversized (plist-get result :error))
                            (list :error 'size)
                          result)))))))))))

(defun jabber-link-preview--process-sentinel (process _event)
  "Finish link preview retrieval PROCESS."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'jabber-link-preview-delivered)))
    (process-put process 'jabber-link-preview-delivered t)
    (let ((buffer (process-buffer process))
          (callback (process-get process 'jabber-link-preview-callback))
          (cbargs (process-get process 'jabber-link-preview-cbargs))
          (url (process-get process 'jabber-link-preview-url))
          result)
      (unwind-protect
          (setq result
                (condition-case nil
                    (jabber-link-preview--buffer-result process url)
                  (error (list :error 'response))))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (apply callback result cbargs))))
(defun jabber-link-preview--start-process
    (fetch-url source-url kind address callback cbargs)
  "Fetch FETCH-URL at ADDRESS for SOURCE-URL using KIND parser."
  (let* ((buffer (generate-new-buffer " *jabber-link-preview*"))
         process)
    (with-current-buffer buffer (set-buffer-multibyte nil))
    (setq process
          (make-process
           :name "jabber-link-preview"
           :buffer buffer
           :command (jabber-link-preview--curl-command fetch-url address)
           :coding 'binary
           :connection-type 'pipe
           :filter #'jabber-link-preview--process-filter
           :sentinel #'jabber-link-preview--process-sentinel
           :noquery t))
    (process-put process 'jabber-link-preview-callback callback)
    (process-put process 'jabber-link-preview-cbargs cbargs)
    (process-put process 'jabber-link-preview-url source-url)
    (process-put process 'jabber-link-preview-kind kind)
    process))

(defun jabber-link-preview-fetch (url callback &rest cbargs)
  "Fetch preview metadata for URL and call CALLBACK with it and CBARGS.
Retrieval uses a vetted, pinned public address, refuses redirects, sends
no cookies or referrer, and aborts when response limits are exceeded."
  (cond ((not (executable-find jabber-link-preview-program))
         (apply callback (list :error 'program-unavailable) cbargs))
        ((not (jabber-link-preview-safe-url-p url))
         (apply callback (list :error 'unsafe) cbargs))
        ((not (fboundp 'libxml-parse-html-region))
         (apply callback (list :error 'parser-unavailable) cbargs))
        (t
         (let* ((oembed (jabber-link-preview--youtube-oembed-url url))
                (fetch-url (or oembed url))
                (kind (and oembed 'youtube)))
           (if-let* ((address
                      (jabber-link-preview--resolved-address fetch-url)))
             (jabber-link-preview--start-process
              fetch-url url kind address callback cbargs)
             (apply callback (list :error 'unsafe) cbargs))))))

(provide 'jabber-link-preview)

;;; jabber-link-preview.el ends here
