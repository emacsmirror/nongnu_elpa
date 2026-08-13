;;; hermes-system.el --- Gateway status and logs for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Native read-only views over dashboard status and log REST endpoints.

;;; Code:

(require 'pp)
(require 'subr-x)
(require 'hermes-dashboard-transport)
(require 'hermes-browser)

(defvar-local hermes-system--heading nil
  "Heading rendered by the current system buffer.")

(defvar-local hermes-system--path nil
  "REST path fetched by the current system buffer.")

(defvar-local hermes-system--query nil
  "REST query fetched by the current system buffer.")

(defun hermes-system--api (client path &optional query)
  "Return dashboard GET PATH promise through CLIENT with QUERY."
  (hermes-dashboard-transport-api-request-async
   "GET" path :query query :client client))

(defun hermes-system--redact-text (text)
  "Return management TEXT with credential-shaped values redacted."
  (let ((case-fold-search t)
        (safe (hermes-dashboard-transport--redact-secret text)))
    (setq safe
          (replace-regexp-in-string
           "\\(bearer[ \t]+\\)[^[:space:],;}]+" "\\1<redacted>" safe t nil))
    (replace-regexp-in-string
     "\\(\\(?:api[-_ ]?key\\|token\\|secret\\|password\\)[\"']?[ \t]*[:=][ \t]*[\"']?\\)[^\"'[:space:],;}]+"
     "\\1<redacted>" safe t nil)))

(defun hermes-system--result-text (result)
  "Return display text for system RESULT."
  (if-let* ((reason (hermes-transport--get result 'error)))
      (concat (propertize "Error: " 'face 'error) (format "%s" reason))
    (if-let* ((lines (hermes-transport--get result 'lines)))
        (string-join lines "\n")
      (pp-to-string result))))

(defun hermes-system--render (buffer result)
  "Render RESULT in live system BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'hermes-system-mode)
        (let ((inhibit-read-only t)
              (line (line-number-at-pos)))
          (erase-buffer)
          (insert (propertize hermes-system--heading 'face 'bold) "\n\n")
          (insert (hermes-system--redact-text
                   (hermes-system--result-text result)))
          (goto-char (point-min))
          (forward-line (1- line)))))))

(defun hermes-system--fetch (buffer)
  "Fetch and render the REST view owned by BUFFER."
  (with-current-buffer buffer
    (let ((generation (hermes-browser--next-request-generation))
          (path hermes-system--path)
          (query hermes-system--query))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes--promise-catch
          (hermes-system--api client path query)
          (lambda (reason) (list :error reason))))
       (lambda (result)
         (when (hermes-browser--request-current-mode-p
                buffer generation 'hermes-system-mode)
           (hermes-system--render buffer result)))))))

(defun hermes-system--open (buffer-name heading path &optional query)
  "Open BUFFER-NAME for HEADING fetched from PATH with QUERY."
  (let ((instance (hermes-instance-resolve))
        (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (hermes-system-mode)
      (hermes-browser--own-instance instance)
      (setq hermes-system--heading heading
            hermes-system--path path
            hermes-system--query query))
    (pop-to-buffer buffer)
    (hermes-system--fetch buffer)))

(defun hermes-system-status ()
  "Show gateway status in a native buffer."
  (interactive)
  (hermes-system--open "*Hermes Status*" "Hermes Gateway Status"
                       "/api/status"))

(defun hermes-system--bounded-log-lines (lines)
  "Return requested log LINES clamped to the backend's 1..500 tail range."
  (min 500 (max 1 (if lines (prefix-numeric-value lines) 100))))

(defun hermes-system-logs (&optional lines)
  "Show tail of gateway logs, limited to LINES."
  (interactive "P")
  (hermes-system--open
   "*Hermes Logs*" "Hermes Gateway Logs" "/api/logs"
   `((file . "agent") (lines . ,(hermes-system--bounded-log-lines lines)))))

(defun hermes-system--revert (&rest _)
  "Refresh the current Hermes system buffer."
  (hermes-system--fetch (current-buffer)))

(defvar-keymap hermes-system-mode-map
  :doc "Keymap for Hermes status and log buffers."
  :parent special-mode-map
  "g" #'revert-buffer)

(define-derived-mode hermes-system-mode special-mode "Hermes System"
  "Major mode for Hermes gateway status and logs."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-system--revert))

(provide 'hermes-system)
;;; hermes-system.el ends here
