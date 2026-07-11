;;; hermes-chat-slash.el --- Slash commands for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; Slash commands for `hermes-chat': parsing `/command arg' input, the
;; `commands.catalog' cache and its `completion-at-point' function, the
;; native in-client command table, and gateway dispatch through
;; `slash.exec' with `command.dispatch' fallback.  Part of the one logical
;; chat module (see the require note in `hermes-chat.el'); it preserves
;; the existing `hermes-chat--*' symbols.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-buffer)

(declare-function hermes-chat--alias-content "hermes-chat" (name arg))
(declare-function hermes-chat--commands-categories-content "hermes-chat" (result))
(declare-function hermes-chat--handle-command-result "hermes-chat" (result arg))
(declare-function hermes-chat--dashboard-control-client "hermes-chat-dashboard" ())
(declare-function hermes-chat--dashboard-client-live-p "hermes-chat-dashboard" (client))
(declare-function hermes-chat--with-dashboard-session "hermes-chat-dashboard" (content buffer action &optional reject))

(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)

(defvar-local hermes-chat--commands-cache nil
  "Cached slash command catalog as an alist of (NAME . DESCRIPTION).")

(defun hermes-chat--subcommand-name (value)
  "Return VALUE as a slash command name without leading slash."
  (and-let* ((name (hermes-chat--scalar-string value)))
    (string-remove-prefix "/" name)))

(defun hermes-chat--format-subcommand-entry (entry)
  "Return readable catalog line for subcommand ENTRY."
  (let* ((command (hermes-chat--subcommand-name (car-safe entry)))
         (items (hermes-chat--listify (cdr-safe entry)))
         (subs (delq nil
                     (mapcar (lambda (item)
                               (when-let* ((sub (hermes-chat--scalar-string item)))
                                 (format "/%s %s" command sub)))
                             items))))
    (and (hermes-transport--non-empty-string command) subs
         (concat "  " (string-join subs ", ")))))

(defun hermes-chat--commands-subcommands-content (result)
  "Return readable subcommand catalog section from RESULT."
  (let* ((sub (hermes-transport--get result 'sub))
         (entries (hermes-chat--maplike-entries sub))
         (lines (delq nil
                      (mapcar #'hermes-chat--format-subcommand-entry entries))))
    (and lines
         (string-join (cons "Subcommands" lines) "\n"))))

(defun hermes-chat--command-name (value)
  "Return VALUE as a bare slash command name, or nil."
  (and-let* ((name (hermes-chat--scalar-string value)))
    (hermes-transport--non-empty-string (string-remove-prefix "/" name))))

(defun hermes-chat--catalog-pairs-candidates (pairs)
  "Return (NAME . DESCRIPTION) cells for catalog PAIRS."
  (delq nil
        (mapcar
         (lambda (pair)
           (when-let* ((name (hermes-chat--command-name
                              (hermes-chat--pair-command pair))))
             (cons name (hermes-chat--scalar-string
                         (hermes-chat--pair-description pair)))))
         (hermes-chat--listify pairs))))

(defun hermes-chat--catalog-candidates (result)
  "Return an alist of (NAME . DESCRIPTION) slash commands from catalog RESULT."
  (let ((candidates
         (append
          (mapcan (lambda (category)
                    (hermes-chat--catalog-pairs-candidates
                     (hermes-transport--get category 'pairs)))
                  (hermes-chat--listify
                   (hermes-transport--get result 'categories)))
          (hermes-chat--catalog-pairs-candidates
           (hermes-transport--get result 'pairs)))))
    (cl-delete-duplicates candidates :key #'car :test #'equal :from-end t)))

(defun hermes-chat--commands-catalog-content (result)
  "Return readable command catalog RESULT content."
  (let ((warning (hermes-chat--result-string result 'warning)))
    (string-join
     (delq nil
           (list (and (hermes-transport--non-empty-string warning)
                      (format "Warning: %s" warning))
                 (hermes-chat--commands-categories-content result)
                 (hermes-chat--commands-subcommands-content result)))
     "\n\n")))

(defun hermes-chat--parse-slash (content)
  "Return (NAME . ARG) when CONTENT is a slash command."
  (when (string-prefix-p "/" content)
    (let* ((rest (substring content 1))
           (space (string-match-p "[ \t\n]" rest)))
      (if space
          (cons (downcase (substring rest 0 space))
                (string-trim-left (substring rest space)))
        (cons (downcase rest) "")))))


(defun hermes-chat--dashboard-dispatch-command (name arg &optional preserve-content)
  "Dispatch dashboard command NAME with ARG and render its result.
PRESERVE-CONTENT is restored if session bootstrap fails before dispatch."
  (let ((buffer (current-buffer))
        (raw (or preserve-content (hermes-chat--alias-content name arg))))
    (hermes-chat--with-dashboard-session
     raw buffer
     (lambda (live-client)
       (hermes-dashboard-transport-command-dispatch
        live-client name arg
        :session-id hermes-chat--dashboard-active-session-id
        :resolve (lambda (result)
                   (hermes-chat--in-buffer buffer
                     (hermes-chat--handle-command-result result arg)))
        :reject (lambda (message)
                  (hermes-chat--in-buffer buffer
                    (hermes-chat--command-error message))))))))

(defun hermes-chat--dashboard-slash-exec (name arg raw)
  "Run RAW slash command, falling back to command dispatch for NAME/ARG."
  (let ((buffer (current-buffer))
        (preserve-content (concat "/" raw)))
    (hermes-chat--with-dashboard-session
     preserve-content buffer
     (lambda (live-client)
       (hermes-dashboard-transport-slash-exec
        live-client raw
        :session-id hermes-chat--dashboard-active-session-id
        :resolve (lambda (result)
                   (hermes-chat--in-buffer buffer
                     (hermes-chat--handle-command-result result arg)))
        :reject (lambda (_message)
                  (hermes-chat--in-buffer buffer
                    (hermes-chat--dashboard-dispatch-command
                     name arg preserve-content))))))))

(defun hermes-chat--fetch-commands-catalog ()
  "Fetch the slash command catalog into the buffer cache, when connected."
  (when (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (let ((buffer (current-buffer)))
      (hermes-dashboard-transport-commands-catalog
       hermes-chat--dashboard-client
       :resolve (lambda (result)
                  (hermes-chat--in-buffer buffer
                    (setq hermes-chat--commands-cache
                          (hermes-chat--catalog-candidates result))))))))

(defun hermes-chat--command-candidates ()
  "Return cached slash command candidates, fetching the catalog if needed."
  (unless hermes-chat--commands-cache
    (hermes-chat--fetch-commands-catalog))
  hermes-chat--commands-cache)

(defun hermes-chat-refresh-commands ()
  "Refresh the cached slash command catalog from the dashboard."
  (interactive)
  (setq hermes-chat--commands-cache nil)
  (hermes-chat--fetch-commands-catalog))

(defun hermes-chat--slash-completion-bounds ()
  "Return (START . END) of the slash command name at point, or nil.
Only matches while typing the /command word in the writable input tail."
  (let ((input (hermes-chat--input-position)))
    (and input
         (hermes-chat--point-in-input-p)
         (> (point) input)
         (eq (char-after input) ?/)
         (let ((name-start (1+ input)))
           (and (>= (point) name-start)
                (not (string-match-p
                      "[ \t\n]"
                      (buffer-substring-no-properties name-start (point))))
                (cons name-start (point)))))))

(defun hermes-chat--slash-capf ()
  "Completion-at-point for Hermes slash commands in the input tail."
  (when-let* ((bounds (hermes-chat--slash-completion-bounds))
              (candidates (hermes-chat--command-candidates)))
    (list (car bounds) (cdr bounds)
          (mapcar #'car candidates)
          :exclusive 'no
          :annotation-function
          (lambda (cand)
            (when-let* ((desc (cdr (assoc cand candidates))))
              (concat "  " desc))))))

(defun hermes-chat-show-commands ()
  "Fetch and display the dashboard slash command catalog."
  (interactive)
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-control-client)))
    (hermes-dashboard-transport-commands-catalog
     client
     :resolve (lambda (result)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--insert-local-status
                   (hermes-chat--commands-catalog-content result) 'done)))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (hermes-chat--command-error message))))))

(defvar hermes-chat--native-slash-commands nil
  "Native in-client slash commands as (NAMES . HANDLER) entries.
NAMES is a list of aliases; HANDLER takes the command's ARG string (empty
when none).  Populated by `hermes-chat', which owns the commands the
handlers call; names absent here fall through to the gateway via
`hermes-chat--dashboard-slash-exec'.")

(defun hermes-chat--native-slash-handler (name)
  "Return the native handler for slash command NAME, or nil when none.
NAME is matched against each alias list in `hermes-chat--native-slash-commands'."
  (and name
       (cdr (cl-find-if (lambda (entry) (member name (car entry)))
                        hermes-chat--native-slash-commands))))

(defun hermes-chat--handle-slash-content (content)
  "Handle slash command CONTENT from the input tail.
Native control commands run in-client through
`hermes-chat--native-slash-commands'; everything else dispatches to the gateway
via `hermes-chat--dashboard-slash-exec'."
  (pcase-let ((`(,name . ,arg) (hermes-chat--parse-slash content)))
    (hermes-chat--delete-input-tail)
    (if-let* ((handler (hermes-chat--native-slash-handler name)))
        (funcall handler (or arg ""))
      (hermes-chat--dashboard-slash-exec name arg (substring content 1)))))
(provide 'hermes-chat-slash)
;;; hermes-chat-slash.el ends here
