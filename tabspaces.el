;;; tabspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Colin McLear

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 1.10.0
;; Package-Requires: ((emacs "27.1") (project "0.8.1"))
;; Keywords: convenience, frames
;; Homepage: https://codeberg.org/mclear-tools/tabspaces

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides several functions to facilitate a frame-based
;; tab workflow with one workspace per tab, integration with project.el (for
;; project-based workspaces) and buffer isolation per tab (i.e. a "tabspace"
;; workspace).  The package assumes project.el and tab-bar.el are both present
;; (they are built-in to Emacs 27.1+).

;;;; Acknowledgements

;; Much of the package code is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources
;; - https://github.com/florommel/bufferlo

;;; Code:

;;;; Requirements

(require 'tab-bar)
(require 'project)
(require 'vc)
(require 'seq)
(require 'cl-lib)
(require 'dired-x)

(declare-function magit-init "magit-status")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function ibuffer-current-buffer "ibuffer" (&optional must-be-live))
(declare-function vterm "vterm")
(declare-function eat "eat")

;; Forward declarations for buffer-kind handlers.  These special variables
;; are defined in their respective packages, which we do not require here.
(defvar eshell-buffer-name)
(defvar dired-buffers)
(defvar vterm-buffer-name)
(defvar eat-buffer-name)

;;;; Variables

(defgroup tabspaces nil
  "Manage tab/workspace buffers."
  :group 'convenience)

(defcustom tabspaces-default-tab "Default"
  "Specify a default tab by name TAB."
  :type 'string)

(defcustom tabspaces-remove-to-default t
  "Add buffer to default tabspace when removed from current tabspace."
  :type 'boolean)

(defcustom tabspaces-include-buffers '("*scratch*")
  "Buffers that should always get included in a new tab or frame.
This is a list of buffer names, matched exactly (not as regular
expressions), which overrides buffers excluded by
`tabspaces-exclude-buffers'."
  :type '(repeat string))

(defcustom tabspaces-exclude-buffers nil
  "Buffers that should always get excluded in a new tab or frame.
This is a list of buffer names, matched exactly (not as regular
expressions), which does not override buffers inside
`tabspaces-include-buffers'."
  :type '(repeat string))

(defcustom tabspaces-use-filtered-buffers-as-default nil
  "When t, remap `switch-to-buffer' to `tabspaces-switch-to-buffer'."
  :type 'boolean)

(defcustom tabspaces-keymap-prefix (kbd "C-c TAB")
  "Key sequence prefix for the tabspaces command map.
The value is a key sequence as returned by `kbd'.  For backward
compatibility, a string in `kbd' syntax (the option's former
format) is also accepted.  Set to nil to disable automatic keymap
binding."
  :type '(choice (const :tag "Disabled" nil)
                 key-sequence))

(defcustom tabspaces-initialize-project-with-todo t
  "Whether to create a `tabspaces-todo-file-name' file in new workspaces.
When non-nil, create the file in the project when creating a
workspace for it."
  :type 'boolean)

(defcustom tabspaces-todo-file-name "project-todo.org"
  "The name of the TODO file to create if non-existing for new workspaces."
  :type 'string)

(defcustom tabspaces-project-switch-commands project-switch-commands
  "Available commands when switch between projects.
Change this value if you wish to run a specific command, such as
`find-file' on project switch.  Otherwise this will default to
the value of `project-switch-commands'."
  :type (get 'project-switch-commands 'custom-type))

(defcustom tabspaces-project-fallback-to-tab t
  "When non-nil, project.el commands fall back to the current tab's project.
A buffer that belongs to no project (e.g. *scratch*, an org file
elsewhere on disk) normally makes `project-current' return nil, so
project.el commands prompt or fail even though the surrounding
workspace has an obvious project.  With this option, the current
tab's project (per `tabspaces-project-tab-map') is used as a
fallback.  A buffer that is itself inside a project keeps its own
project; the tab never overrides it."
  :type 'boolean)

(defcustom tabspaces-project-switch-opens-workspace nil
  "When non-nil, `project-switch-project' opens a tabspaces workspace.
Routes the stock \\[project-switch-project] through
`tabspaces-open-or-create-project-and-workspace', so vanilla
project.el switching also creates or reuses the project's tab.
Takes effect when `tabspaces-mode' is enabled; re-enable the mode
after changing this option."
  :type 'boolean)

(defcustom tabspaces-initialize-project-with-vc t
  "When non-nil, put newly created projects under version control.
Uses magit when available, otherwise `vc-create-repo'.  When nil,
write an empty \".project\" marker file into the new directory
instead; add \".project\" to `project-vc-extra-root-markers' so
project.el recognizes such directories as projects."
  :type 'boolean)

(defcustom tabspaces-fully-resolve-paths nil
  "Resolve \".\", \"..\", etc. in project paths."
  :type 'boolean)

(defcustom tabspaces-session t
  "Whether to save tabspaces across sessions."
  :type 'boolean)

(defcustom tabspaces-session-auto-restore nil
  "Whether to restore tabspaces on session startup."
  :type 'boolean)

(defcustom tabspaces-session-file (locate-user-emacs-file "tabsession.el")
  "File for saving tabspaces session."
  :type 'file)

(defcustom tabspaces-session-auto-save-delay nil
  "Idle seconds before auto-saving sessions, or nil to disable.
When non-nil and `tabspaces-session' is enabled, sessions are saved
after this many seconds of idle time, so a crash or a killed Emacs
loses at most the changes since the last idle period.  Capturing
window configurations briefly cycles through the tabs.  The default
\(nil) saves only on exit, via `kill-emacs-hook'."
  :type '(choice (const :tag "Disabled" nil) number))

(defcustom tabspaces-session-project-session-store 'project
  "Determines where project session files are stored.
Can be one of:
- \\='project (default) - Store in the project root directory
- a string path - Store all project sessions in this directory
- a function - Called with project root path to determine session file location"
  :type '(choice
          (const :tag "In project directory" project)
          (directory :tag "In specific directory")
          (function :tag "Custom function")))

(defcustom tabspaces-echo-area-enable nil
  "Display tabs in echo area instead of tab-bar when enabled."
  :type 'boolean)

(defcustom tabspaces-echo-area-format-function #'tabspaces--echo-area-format-tabs
  "Function to format tabs for echo area display."
  :type 'function)

(defcustom tabspaces-echo-area-idle-delay 1.0
  "Number of seconds to wait before showing tabs when idle."
  :type 'number
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Restart timer with new delay if echo area is enabled
         (when (and (boundp 'tabspaces-echo-area-enable)
                    tabspaces-echo-area-enable
                    (boundp 'tabspaces--idle-timer)
                    tabspaces--idle-timer)
           (tabspaces--setup-idle-timer))))

(defvar tabspaces-project-tab-map '()
  "Alist mapping full project paths to their respective tab names.")

(defvar tabspaces--in-project-switch nil
  "Non-nil while a tabspaces command drives `project-switch-project'.
Lets `tabspaces--project-switch-advice' distinguish internal calls
\(which must run the stock command) from a direct user invocation.")

;;;; Echo Area Display

(defvar tabspaces--tabs-visible nil
  "Non-nil when tabs are currently displayed in the echo area.")

(defvar tabspaces--idle-timer nil
  "Timer object for displaying tabs after idle time.")

(defvar tabspaces--original-tab-bar-show nil
  "Original value of `tab-bar-show' before echo area display is enabled.")

(defvar tabspaces--echo-area-active nil
  "Non-nil when `tabspaces--echo-area-setup' has modified `tab-bar-show'.
Guards `tabspaces--echo-area-cleanup' so it only restores
`tab-bar-show' when setup actually ran.")

(defun tabspaces--echo-area-format-tabs ()
  "Format all tabs for echo area display using the configured tab-bar formatter.
Returns a formatted string containing all tabs, or nil if only one tab
exists."
  (let ((tabs (tab-bar-tabs)))
    (when (> (length tabs) 1)
      (let ((i 0))
        (mapconcat (lambda (tab)
                     (setq i (1+ i))
                     ;; Display indexes are 1-based; the 10th and later tabs
                     ;; display as 0, matching `tab-bar-select-tab' key digits.
                     (funcall tab-bar-tab-name-format-function
                              tab (if (< i 10) i 0)))
                   tabs "")))))

(defun tabspaces--echo-area-display (&rest _)
  "Display formatted tabs in the echo area without logging to *Messages*.
Sets the visibility flag to indicate tabs are currently shown.
Optional ARGS are ignored, allowing use as advice."
  (when tabspaces-echo-area-enable
    (let ((tabs-display (funcall tabspaces-echo-area-format-function)))
      (when tabs-display
        (setq tabspaces--tabs-visible t)
        ;; Binding `message-log-max' to nil keeps the display out of the
        ;; *Messages* buffer entirely.
        (let ((message-log-max nil))
          (message "%s" tabs-display))))))

(defun tabspaces--idle-display ()
  "Display tabs in echo area after idle period.
Only displays if echo area feature is enabled, multiple tabs exist,
and the minibuffer is not active."
  (when (and tabspaces-echo-area-enable
             (> (length (tab-bar-tabs)) 1)
             (not (active-minibuffer-window)))
    (tabspaces--echo-area-display)))

(defun tabspaces--setup-idle-timer ()
  "Initialize idle timer to display tabs after inactivity.
Cancels any existing timer before creating a new one."
  (when tabspaces--idle-timer
    (cancel-timer tabspaces--idle-timer))
  (setq tabspaces--idle-timer
        (run-with-idle-timer tabspaces-echo-area-idle-delay t #'tabspaces--idle-display)))

(defun tabspaces--cancel-idle-timer ()
  "Cancel and clear the idle display timer."
  (when tabspaces--idle-timer
    (cancel-timer tabspaces--idle-timer)
    (setq tabspaces--idle-timer nil)))

(defun tabspaces-restart-idle-timer ()
  "Restart the echo area idle timer with current delay settings.
Useful for troubleshooting or after changing the delay value."
  (interactive)
  (when tabspaces-echo-area-enable
    (tabspaces--setup-idle-timer)
    (message "Idle timer restarted with delay: %.1f seconds" tabspaces-echo-area-idle-delay)))

(defun tabspaces-echo-area-timer-status ()
  "Display current status of the echo area idle timer.
Shows if timer is active, the delay setting, and other relevant info."
  (interactive)
  (let ((status-parts '()))
    (push (format "Echo area enabled: %s" (if tabspaces-echo-area-enable "yes" "no")) status-parts)
    (push (format "Idle delay: %.1f seconds" tabspaces-echo-area-idle-delay) status-parts)
    (push (format "Timer active: %s" (if tabspaces--idle-timer "yes" "no")) status-parts)
    (when tabspaces--idle-timer
      (push (format "Timer object: %s" tabspaces--idle-timer) status-parts))
    (push (format "Number of tabs: %d" (length (tab-bar-tabs))) status-parts)
    (message (mapconcat #'identity status-parts ", "))))

(defun tabspaces--echo-area-setup ()
  "Initialize echo area tab display when enabled.
Hides the visual tab-bar and sets up idle timer for tab display."
  (when tabspaces-echo-area-enable
    ;; Ensure tab-bar-mode is enabled for tab functionality
    (unless tab-bar-mode (tab-bar-mode 1))
    ;; Store original setting and hide visual tab-bar
    (setq tabspaces--original-tab-bar-show tab-bar-show)
    (setq tabspaces--echo-area-active t)
    (setq tab-bar-show nil)
    ;; Force tab-bar update after brief delay to override other configurations
    (run-with-timer 0.1 nil
                    (lambda ()
                      (setq tab-bar-show nil)
                      (when (fboundp 'tab-bar--update-tab-bar-lines)
                        (tab-bar--update-tab-bar-lines))))
    ;; Configure automatic display via idle timer only
    (tabspaces--setup-idle-timer)))

(defun tabspaces--echo-area-cleanup ()
  "Clean up echo area tab display configuration.
Restores original tab-bar visibility and removes timer."
  ;; Restore original tab-bar visibility, but only if setup ran; otherwise
  ;; this would clobber the user's `tab-bar-show' with nil.
  (when tabspaces--echo-area-active
    (setq tab-bar-show tabspaces--original-tab-bar-show)
    (setq tabspaces--echo-area-active nil))
  ;; Clean up timer
  (tabspaces--cancel-idle-timer)
  ;; Reset state variables
  (setq tabspaces--tabs-visible nil))

(defun tabspaces-show-workspaces ()
  "Display current workspaces in the echo area on command."
  (interactive)
  (let ((tabs-display (funcall tabspaces-echo-area-format-function)))
    (if tabs-display
        (message "%s" tabs-display)
      (message "Only one workspace active"))))

(defun tabspaces-toggle-echo-area-display ()
  "Toggle echo area tab display feature on or off.
When enabled, tabs will appear in the echo area after idle time and
during tab operations.  When disabled, tabs are only shown in the tab-bar."
  (interactive)
  (if (or tabspaces--tabs-visible tabspaces-echo-area-enable)
      ;; Turn off: disable feature and clear current display
      (progn
        (setq tabspaces-echo-area-enable nil)
        (setq tabspaces--tabs-visible nil)
        (message nil)
        (tabspaces--echo-area-cleanup)
        (run-with-timer 0.5 nil (lambda () (message "Echo area tabs disabled"))))
    ;; Turn on: enable feature and show tabs
    (progn
      (setq tabspaces-echo-area-enable t)
      (tabspaces--echo-area-setup)
      (tabspaces--echo-area-display)
      (run-with-timer 0.5 nil (lambda () (message "Echo area tabs enabled"))))))

;;;; Create Buffer Workspace

(defun tabspaces-reset-buffer-list ()
  "Reset the current tab's `buffer-list'.
Only the current window buffers and buffers in
`tabspaces-include-buffers' are kept in the `buffer-list' and
`buried-buffer-list'."
  (interactive)
  ;; (elisp) Current Buffer: The current-tab uses `buffer-list' and
  ;; `buried-buffer-list'.  A hidden tab keeps these as `wc-bl' and
  ;; `wc-bbl'.
  (set-frame-parameter nil
                       'buffer-list
                       (let ((window-buffers (mapcar #'window-buffer (window-list))))
                         (seq-filter (lambda (buffer)
                                       (or (member buffer window-buffers)
                                           (and (member (buffer-name buffer)
                                                        tabspaces-include-buffers)
                                                (not (member (buffer-name buffer)
                                                             tabspaces-exclude-buffers)))))
                                     (frame-parameter nil 'buffer-list))))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (seq-filter (lambda (buffer)
                                     (and (member (buffer-name buffer)
                                                  tabspaces-include-buffers)
                                          (not (member (buffer-name buffer)
                                                       tabspaces-exclude-buffers))))
                                   (frame-parameter nil 'buried-buffer-list))))

(defun tabspaces--tab-post-open-function (_tab)
  "Reset buffer list on new tab creation."
  (tabspaces-reset-buffer-list))

;;;; Filter Workspace Buffers

(defun tabspaces--local-buffer-p (buffer)
  "Return whether BUFFER is in the list of local buffers."
  (or (member (buffer-name buffer) tabspaces-include-buffers)
      (memq buffer (frame-parameter nil 'buffer-list))))

(defun tabspaces--set-buffer-predicate (frame)
  "Set the buffer predicate of FRAME to `tabspaces--local-buffer-p'."
  (set-frame-parameter frame 'buffer-predicate #'tabspaces--local-buffer-p))

(defun tabspaces--reset-buffer-predicate (frame)
  "Reset the buffer predicate of FRAME if it is `tabspaces--local-buffer-p'."
  (when (eq (frame-parameter frame 'buffer-predicate) #'tabspaces--local-buffer-p)
    (set-frame-parameter frame 'buffer-predicate nil)))

(defun tabspaces--buffer-list (&optional frame tabnum)
  "Return a list of all live buffers associated with the current frame and tab.
A non-nil value of FRAME selects a specific frame instead of the
current one.  If TABNUM is nil, the current tab is used.  If it is
non-nil, then specify a tab index in the given frame."
  (let ((list
         (if tabnum
             (let ((tab (nth tabnum (frame-parameter frame 'tabs))))
               (if (eq 'current-tab (car tab))
                   (frame-parameter frame 'buffer-list)
                 (cdr (assq 'wc-bl tab))))
           (frame-parameter frame 'buffer-list))))
    (seq-filter #'buffer-live-p list)))

;;;; Project Workspace Helper Functions

(defun tabspaces--current-tab-name ()
  "Get name of current tab."
  (cdr (assq 'name (tab-bar--current-tab))))

(defun tabspaces--list-tabspaces ()
  "Return a list of `tab-bar' tab/workspace names.
Goes through `tab-bar-tabs-function' so a user-customized tab list
provider is honored."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun tabspaces--project-name ()
  "Get name of the current buffer's project via project.el.
Return `-' if the buffer is not part of a project.  Covers both
version-controlled projects and projects recognized through markers
in `project-vc-extra-root-markers'."
  (let ((project (project-current)))
    (if project
        (file-name-nondirectory (directory-file-name (project-root project)))
      "-")))

(defun tabspaces--name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name along with a counter.
Intended for use as `tab-bar-tab-name-function', so it must be free
of side effects and always return a string."
  (let ((project-name (tabspaces--project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current-with-count)
      project-name)))

(defun tabspaces--add-to-default-tabspace (buffer)
  "Add BUFFER to default tabspace buffer list."
  (let ((tab-names (tabspaces--list-tabspaces)))
    (when (and tabspaces-remove-to-default
               (member tabspaces-default-tab tab-names))
      ;; add buffer to default tabspace
      (tab-bar-select-tab-by-name tabspaces-default-tab)
      (display-buffer buffer)
      (switch-to-buffer buffer t nil)
      (if (one-window-p t)
          (previous-buffer)
        (delete-window))
      (tab-bar-switch-to-recent-tab))))

;;;; Tab-Anchored Project Context

(defvar tabspaces--resolving-tab-project nil
  "Non-nil while `tabspaces--tab-project' resolves the tab's project.
Guards against re-entry from two directions.  Resolving the root runs
`project-find-functions', which includes `tabspaces--tab-project'
itself when `tabspaces-mode' is enabled.  Looking up the current tab's
name can also re-enter: for tabs without an explicit name,
`tab-bar--current-tab' recomputes the name via
`tab-bar-tab-name-function', and a user-supplied name function may
call `project-current'.  The binding must therefore cover the tab-name
lookup, not just the root resolution.")

(defun tabspaces--tab-project (_dir)
  "Return the current tab's project when no other backend claims _DIR.
Runs at the tail end of `project-find-functions', so it only fires
when earlier backends (e.g. `project-try-vc') found nothing.  This
anchors project context to the workspace: project.el commands issued
from a buffer outside any project (e.g. *scratch*) operate on the
tab's project instead of prompting.  Controlled by
`tabspaces-project-fallback-to-tab'."
  (when (and tabspaces-project-fallback-to-tab
             (not tabspaces--resolving-tab-project))
    (let* ((tabspaces--resolving-tab-project t)
           (root (tabspaces--get-project-for-tab (tabspaces--current-tab-name))))
      (when (and root (file-directory-p root))
        (or (project--find-in-directory root)
            (cons 'transient root))))))

;;;; Interactive Functions

;;;;; Open Project & File
(defun tabspaces-project-switch-project-open-file (dir)
  "Switch to another project by running an Emacs command.
Open file using `project-find-file'.  NOTE: this function does *not*
open or switch to a new workspace.  Rather it switches to a new
project and opens a file via `completing-read'.  If you prefer to
use the project.el command-menu, then use
`project-switch-project'

When called, this function will use the project corresponding
to the selected directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((project-switch-commands tabspaces-project-switch-commands)
        ;; Honor this command's no-workspace contract even when
        ;; `tabspaces-project-switch-opens-workspace' routes
        ;; `project-switch-project' through workspaces.
        (tabspaces--in-project-switch t))
    (project-switch-project dir)))

;;;;; Buffer Functions

(defun tabspaces-remove-buffer (&optional buffer)
  "Bury and remove BUFFER from current tabspace.
If BUFFER is nil, remove current buffer.  If
`tabspaces-remove-to-default' is t then add the buffer to the
default tabspace after remove, unless we're already in the default
tabspace, in which case remove from the default as well."
  (let* ((buffer (get-buffer (or buffer (current-buffer))))
         (in-default-tab (string= (tabspaces--current-tab-name)
                                  tabspaces-default-tab)))
    ;; delete window of buffer
    (cond
     ((eq buffer (window-buffer (selected-window)))
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     ((get-buffer-window buffer)
      (select-window (get-buffer-window buffer) t)
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     (t
      (message "Buffer `%s' removed from `%s' tabspace."
               buffer (tabspaces--current-tab-name))))
    (bury-buffer buffer)
    ;; Delete buffer from tabspace buffer lists
    (set-frame-parameter nil 'buffer-list
                         (delete buffer (frame-parameter nil 'buffer-list)))
    (set-frame-parameter nil 'buried-buffer-list
                         (delete buffer (frame-parameter nil 'buried-buffer-list)))
    ;; If specified AND we're not in default tab, add buffer to default tabspace
    (when (and tabspaces-remove-to-default (not in-default-tab))
      (tabspaces--add-to-default-tabspace buffer))))

(defun tabspaces-remove-current-buffer ()
  "Bury and remove current buffer from current tabspace."
  (interactive)
  (tabspaces-remove-buffer))

(defun tabspaces-remove-selected-buffer (buffer)
  "Remove selected BUFFER from the frame's buffer list.
If `tabspaces-remove-to-default' is t then add the buffer to the
default tabspace."
  (interactive
   (list
    (let ((blst (mapcar #'buffer-name (tabspaces--buffer-list))))
      ;; select buffer
      (read-buffer (format "Remove buffer from `%s' tabspace: "
                           (tabspaces--current-tab-name))
                   nil t
                   (lambda (b) (member (car b) blst))))))
  (tabspaces-remove-buffer buffer))

(defun tabspaces-switch-to-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivalent to `switch-to-buffer'.
The arguments NORECORD and FORCE-SAME-WINDOW are passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (tabspaces--buffer-list)))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (switch-to-buffer buffer norecord force-same-window))

(defun tabspaces-switch-buffer-and-tab (buffer &optional norecord force-same-window)
  "Switch to the tab of chosen BUFFER, or create buffer.
If BUFFER does not exist in the `buffer-list', the user can either
create a new tab with the new buffer or open a new buffer in the
current tab.  NORECORD and FORCE-SAME-WINDOW are passed to
`switch-to-buffer'."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (buffer-list)))))
      (read-buffer
       "Switch to tab for buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))

  ;; Action on buffer
  (let* ((tabcand nil)
         (buflst nil)
         ;; Provide flat list of all buffers in all tabs (and print dupe buffers).
         ;; This is the list of all buffers to search through.
         (bufflst (flatten-tree (dolist (tab (tabspaces--list-tabspaces) buflst)
                                  (push (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))) buflst))))
         ;; A second `member' past the first hit detects a duplicate
         ;; without building a full list of duplicates.
         (dupe (member buffer (cdr (member buffer bufflst)))))
    ;; Run through conditions:
    (cond
     ;; 1. Buffer exists and is not open in more than one tabspace.
     ((and (get-buffer buffer)
           (not dupe))
      (dolist (tab (tabspaces--list-tabspaces))
        (when (cl-member buffer (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))
                         :key #'buffer-name :test #'equal)
          (tab-bar-switch-to-tab tab)
          (tabspaces-switch-to-buffer buffer))))
     ;; 2. Buffer exists and is open in more than one tabspace.
     ((and (get-buffer buffer)
           dupe)
      (dolist (tab (tabspaces--list-tabspaces))
        (when (cl-member buffer (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))
                         :key #'buffer-name :test #'equal)
          (push tab tabcand)))
      (tab-bar-switch-to-tab (completing-read "Select tab: " tabcand))
      (tabspaces-switch-to-buffer buffer))
     ;; 3. Buffer does not exist.
     ((yes-or-no-p "Buffer not found -- create a new workspace with buffer?")
      (switch-to-buffer-other-tab buffer))
     ;; 4. Default -- create buffer in current tabspace.
     (t
      (switch-to-buffer buffer norecord force-same-window)))))

;;;###autoload
(defun tabspaces-ibuffer-switch-buffer-and-tab ()
  "In ibuffer, switch to the tab containing the buffer at point."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (tabspaces-switch-buffer-and-tab (buffer-name buf))))

(defun tabspaces-clear-buffers (&optional frame)
  "Clear the tabspace's buffer list, except for the current buffer.
If FRAME is nil, use the current frame."
  (interactive)
  (set-frame-parameter frame 'buffer-list
                       (list (if frame
                                 (with-selected-frame frame
                                   (current-buffer))
                               (current-buffer)))))

;;;;; Switch or Create Workspace
;; Some convenience functions for opening/closing workspaces and buffers.
;; Some of these are just wrappers around built-in functions.
;;;###autoload
(defun tabspaces-switch-or-create-workspace (&optional workspace)
  "Switch to the tab WORKSPACE, creating it if it does not exist."
  (interactive
   (list (completing-read "Select or create tab: "
                          (tabspaces--list-tabspaces) nil nil)))
  (cond ((member workspace (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab workspace))
        (t
         (tab-new)
         (tab-rename workspace))))

;;;;; Close Workspace
(defalias 'tabspaces-close-workspace #'tab-bar-close-tab)

;;;;; Rename Workspace
(defalias 'tabspaces-rename-workspace #'tab-bar-rename-tab
  "Rename the current workspace/tab.
While `tabspaces-mode' is enabled, renaming also updates
`tabspaces-project-tab-map' (via advice on `tab-bar-rename-tab'),
so per-project session saving keeps tracking the renamed tab.")

(defun tabspaces--sync-tab-rename (orig-fun name &optional tab-number)
  "Update `tabspaces-project-tab-map' when a project tab is renamed.
Installed as :around advice on `tab-bar-rename-tab', which also
serves `tab-bar-rename-tab-by-name'.  Without this, renaming a
project tab leaves the map stale, and per-project session save
silently reclassifies the tab as a non-project tab.  ORIG-FUN,
NAME, and TAB-NUMBER are the advised function and its arguments.

The renamed tab is identified by diffing tab names before and after
the call rather than by index, because TAB-NUMBER's clamping rules
differ across Emacs versions (0, negative, and oversized values do
not mean what `tab-bar-select-tab' makes them mean).  If tab names
are not unique the diff can be ambiguous, in which case the map is
left alone."
  (let* ((before (tabspaces--list-tabspaces))
         (result (funcall orig-fun name tab-number))
         (after (tabspaces--list-tabspaces))
         (old-names (seq-difference before after))
         (new-names (seq-difference after before)))
    (when (and (= 1 (length old-names))
               (= 1 (length new-names)))
      (let ((entry (rassoc (car old-names) tabspaces-project-tab-map)))
        (when entry
          (setcdr entry (car new-names)))))
    result))

;;;;; Close Workspace & Kill Buffers
(defun tabspaces-kill-buffers-close-workspace ()
  "Kill all buffers in the workspace and then close the workspace itself."
  (interactive)
  (let ((buf (tabspaces--buffer-list)))
    (unwind-protect
        (cl-loop for b in buf
                 for n = (buffer-name b)
                 unless (or (member n tabspaces-exclude-buffers)
                            (member n tabspaces-include-buffers))
                 do (kill-buffer b))
      (tab-bar-close-tab))))

;;;;; Open or Create Project in Workspace

(defun tabspaces--remember-project-tab (project-directory tab-name)
  "Record TAB-NAME as the tab for PROJECT-DIRECTORY in the project map.
Replaces any existing entry for PROJECT-DIRECTORY.  Keys are strings,
so `assoc-delete-all' is required; `assq-delete-all' compares with
`eq' and never matches a string."
  (setq tabspaces-project-tab-map
        (cons (cons project-directory tab-name)
              (assoc-delete-all project-directory tabspaces-project-tab-map))))

(defun tabspaces--get-project-for-tab (tab-name)
  "Get project root path for TAB-NAME, or nil if not a project tab.
Handles numbered tabs like \"ProjectName<2>\" by checking both exact
match and base name without suffix."
  (or
   ;; First try exact match
   (car (rassoc tab-name tabspaces-project-tab-map))
   ;; Then try stripping numbered suffix like "<2>"
   (when (string-match "\\`\\(.+\\)<[0-9]+>\\'" tab-name)
     (let ((base-name (match-string 1 tab-name)))
       (car (rassoc base-name tabspaces-project-tab-map))))))

(defun tabspaces-rename-existing-tab (old-name new-name)
  "Rename an existing tab from OLD-NAME to NEW-NAME."
  (let ((tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (when (equal (alist-get 'name tab) old-name)
        (tab-bar-rename-tab-by-name old-name new-name)))))

(defun tabspaces-generate-descriptive-tab-name (project-path existing-tab-names)
  "Generate a unique tab name from PROJECT-PATH.
Checks for conflicts against EXISTING-TAB-NAMES."
  (let* ((parts (nreverse (split-string (directory-file-name project-path) "/")))
         (base-name (car parts))
         (parent-dir (nth 1 parts))
         (grandparent-dir (nth 2 parts))
         (simple-tab-name base-name)
         (complex-tab-name (if parent-dir
                               (format "%s (%s/%s)" base-name (or grandparent-dir "") parent-dir)
                             base-name)))
    (if (member simple-tab-name existing-tab-names)
        (let ((existing-path (rassoc simple-tab-name tabspaces-project-tab-map)))
          (when existing-path
            ;; Generate a new complex name for the existing conflict
            (let ((new-name-for-existing (tabspaces-generate-complex-name (car existing-path))))
              ;; Rename the existing tab
              (tabspaces-rename-existing-tab simple-tab-name new-name-for-existing)
              ;; Update the map with the new name for the existing path
              (setcdr existing-path new-name-for-existing)))
          ;; Use the complex name for the new tab to avoid future conflicts
          complex-tab-name)
      ;; No conflict, add to map and use the simple name
      (add-to-list 'tabspaces-project-tab-map (cons project-path simple-tab-name))
      simple-tab-name)))

(defun tabspaces-generate-complex-name (project-path)
  "Generate a complex tab name from PROJECT-PATH.
The name is based on the grandparent and parent directory names."
  (let* ((parts (nreverse (split-string (directory-file-name project-path) "/")))
         (base-name (car parts))
         (parent-dir (nth 1 parts))
         (grandparent-dir (nth 2 parts)))
    (format "%s (%s/%s)" base-name (or grandparent-dir "") parent-dir)))

(defun tabspaces--generate-unique-numbered-tab-name (base-name existing-names)
  "Return BASE-NAME, suffixed with <N> if it collides with EXISTING-NAMES."
  (let ((counter 2)
        (new-name base-name))
    (while (member new-name existing-names)
      (setq new-name (format "%s<%d>" base-name counter)
            counter (1+ counter)))
    new-name))

(defun tabspaces--project-switch-advice (orig-fun dir &rest args)
  "Route `project-switch-project' through tabspaces workspaces.
Installed as :around advice when
`tabspaces-project-switch-opens-workspace' is non-nil.  A user
invocation hands DIR to
`tabspaces-open-or-create-project-and-workspace'; internal calls
fall through to ORIG-FUN with ARGS."
  (if tabspaces--in-project-switch
      (apply orig-fun dir args)
    (tabspaces-open-or-create-project-and-workspace dir)))

;; Replace read-directory-name so that we can create new projects when necessary
(defun tabspaces--read-directory-name (prompt &optional dir default mustmatch)
  "Read a directory name with PROMPT, and create it if it does not exist.
DIR, DEFAULT, and MUSTMATCH are passed to `read-directory-name'."
  (let ((dir-name (read-directory-name prompt dir default mustmatch)))
    (unless (file-directory-p dir-name)
      (when (yes-or-no-p (format "Directory %s does not exist.  Create it?" dir-name))
        (make-directory dir-name t)))
    dir-name))

;; Replace project-prompt-project-dir for project creation
(defun tabspaces-prompt-project-dir ()
  "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
  (project--ensure-read-project-list)
  (let* ((dir-choice "... (choose a dir)")
         (choices
          ;; XXX: Just using this for the category (for the substring
          ;; completion style).
          (project--file-completion-table
           (append project--list `(,dir-choice))))
         (pr-dir ""))
    (while (equal pr-dir "")
      ;; If the user simply pressed RET, explain and ask again.
      (setq pr-dir (completing-read "Select project: " choices nil t))
      (when (equal pr-dir "")
        (message "Please select a project or directory")
        (sit-for 1)))
    ;; `equal', not `file-equal-p': dir-choice is the literal menu
    ;; entry string above, not a file name.
    (if (equal pr-dir dir-choice)
        (tabspaces--read-directory-name "Select directory: " nil nil nil)
      pr-dir)))

;;;###autoload
(defun tabspaces-open-or-create-project-and-workspace (&optional project prefix)
  "Open or create PROJECT and its workspace with a descriptive tab name.
With universal argument PREFIX, always create a new tab for the project."
  (interactive
   (list (tabspaces-prompt-project-dir) current-prefix-arg))
  (let* ((project-switch-commands tabspaces-project-switch-commands)
         ;; Mark internal `project-switch-project' calls so the advice
         ;; installed by `tabspaces-project-switch-opens-workspace' does
         ;; not re-enter this command.
         (tabspaces--in-project-switch t)
         ;; Open new tabs onto *scratch* so the prior tab's current buffer
         ;; isn't inherited into the new workspace's buffer-list (issue #80).
         (tab-bar-new-tab-choice (lambda () (get-buffer-create "*scratch*")))
         (project (if tabspaces-fully-resolve-paths
                      (expand-file-name project)  ; Resolve relative paths
                    project))
         (existing-tab-names (tabspaces--list-tabspaces))
         (original-tab-name (or (cdr (assoc project tabspaces-project-tab-map))
                                (tabspaces-generate-descriptive-tab-name project existing-tab-names)))
         (tab-name original-tab-name)
         (session (tabspaces--get-project-session-file-for-restore project))
         (project-directory project)  ; Use the full path as the project directory
         (project-exists (member (list project) project--list))
         (create-new-tab (or prefix (not (member tab-name existing-tab-names)))))

    (message "Tabspaces: Project directory: %s" project-directory)

    ;; Remember project if it exists on disk but is not yet registered
    (let ((pr (and (not project-exists)
                   (project--find-in-directory project-directory))))
      (when pr
        (project-remember-project pr)
        (setq project-exists t)))

    ;; Now manage the workspace based on the project state:
    (cond
     ;; If there is no tab nor project, create both
     ((not project-exists)
      (message "Tabspaces - Creating new project and tab")
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (let ((default-directory project-directory))
        (message "Tabspaces: default directory set to %s" default-directory)
        (if tabspaces-initialize-project-with-vc
            (if (fboundp 'magit-init)
                (magit-init project-directory)
              (call-interactively #'vc-create-repo))
          ;; Marker file so project.el can recognize the directory; see
          ;; `tabspaces-initialize-project-with-vc'.
          (write-region "" nil (expand-file-name ".project" project-directory)
                        nil 'silent))
        (delete-other-windows)
        (when (and tabspaces-initialize-project-with-todo
                   (not (file-exists-p (expand-file-name tabspaces-todo-file-name project-directory))))
          (with-temp-buffer
            (write-file (expand-file-name tabspaces-todo-file-name project-directory))))
        (when tabspaces-initialize-project-with-vc
          (if (fboundp 'magit-status-setup-buffer)
              (magit-status-setup-buffer project-directory)
            (project-vc-dir)))
        (dired-jump-other-window))
      ;; Remember new project
      (let ((pr (project--find-in-directory project-directory)))
        (if pr
            (project-remember-project pr)
          (message "Tabspaces: %s not recognized as a project; add \".project\" to `project-vc-extra-root-markers'"
                   project-directory))))

     ;; If project and tab exist, but we want a new tab
     ((and project-exists
           (member tab-name existing-tab-names)
           create-new-tab)
      (message "Tabspaces - Creating new tab for existing project and tab")
      (let ((new-tab-name (tabspaces--generate-unique-numbered-tab-name tab-name existing-tab-names)))
        (tab-bar-new-tab)
        (tab-bar-rename-tab new-tab-name)
        (setq tab-name new-tab-name))
      (project-switch-project project))

     ;; If project and tab exist, switch to it
     ((and project-exists
           (member tab-name existing-tab-names))
      (message "Tabspaces - Switching to existing tab")
      (tab-bar-switch-to-tab tab-name))

     ;; If project exists, but no corresponding tab, open a new tab
     (project-exists
      (message "Tabspaces - Creating new tab for existing project")
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (if (and tabspaces-session-auto-restore
               (file-exists-p session))
          (tabspaces-restore-session session)
        (project-switch-project project)))

     (t
      (message "Tabspaces - No project found or created.")
      nil))

    (message "Tabspaces: Conditional execution completed")

    ;; Update tabspaces-project-tab-map (only for the main tab, not numbered
    ;; duplicates).
    (unless (string-match-p "<[0-9]+>\\'" tab-name)
      (tabspaces--remember-project-tab project-directory tab-name))))

;;;; Tabspace Sessions
(defconst tabspaces-session-header
  ";; -------------------------------------------------------------------------
;; Tabspaces Session File for Emacs
;; -------------------------------------------------------------------------
" "Header to place in Tabspaces session file.")

(defvar tabspaces--session-list nil
  "Store `tabspaces' session tabs and buffers.")

;;;; Buffer-kind registration

(defvar tabspaces--buffer-kind-handlers nil
  "Alist of (KIND SAVE-FN RESTORE-FN) for non-file buffer kinds.
SAVE-FN takes a buffer and returns a plist record (with :kind) or nil.
RESTORE-FN takes a plist record and returns the created buffer or nil.
KIND is the symbol used as the :kind value in serialized records.

The alist is walked front-to-back on save: the first SAVE-FN that
returns non-nil for a given buffer wins.  On restore, the entry is
looked up by KIND via `assq'.

Built-in handlers for `dired', `eshell', and `shell' are registered at
the end of this file.  User registrations issued after the package is
loaded are prepended to this list (see
`tabspaces-register-buffer-kind') so they take precedence on save.")

(defvar tabspaces--restore-unknown-kinds nil
  "Accumulator for unknown :kind values encountered during restore.
Dynamically bound by `tabspaces-restore-session'.  Declared here so
the helper `tabspaces--restore-buffer-record' can push to it from
outside the let-binding scope under `lexical-binding'.")

;;;###autoload
(defun tabspaces-register-buffer-kind (kind save-fn restore-fn)
  "Register handlers for non-file buffer KIND.
SAVE-FN takes a buffer and returns a plist (with :kind KIND) or nil
to skip the buffer.  RESTORE-FN takes such a plist and returns the
created buffer or nil to skip the record.

Re-registering KIND replaces any prior entry.  Re-registrations are
prepended to `tabspaces--buffer-kind-handlers', so the most recently
registered handler runs first on save.  The built-in handlers shipped
with tabspaces are registered at the end of this file.  User
registrations issued after `(require \\='tabspaces)' therefore take
precedence on save.

Restore-fn bodies must create buffers but must NOT call
window-configuration-changing functions like `pop-to-buffer-other-window'
or `delete-other-windows'.  The outer restore loop wraps each
record's handler in `save-window-excursion' and then calls
`window-state-put' to set the final layout."
  (setq tabspaces--buffer-kind-handlers
        (cons (list kind save-fn restore-fn)
              (assq-delete-all kind tabspaces--buffer-kind-handlers))))

;; Helper functions
(defun tabspaces--buffer-record (b)
  "Return a serializable session record for buffer B, or nil to skip.
File-visiting buffers are returned as bare path strings (legacy
format).  Other buffers are dispatched through
`tabspaces--buffer-kind-handlers'.  The first save-fn that returns
non-nil wins."
  (when (buffer-live-p b)
    (with-current-buffer b
      (cond
       (buffer-file-name)
       (t (catch 'found
            (dolist (entry tabspaces--buffer-kind-handlers)
              (let ((rec (funcall (nth 1 entry) b)))
                (when rec (throw 'found rec))))
            nil))))))

(defun tabspaces--store-buffers (bufs)
  "Return list of session records for BUFS, skipping unhandled buffers.
Each record is either a file path string or a plist of the form
\(:kind SYMBOL :dir DIR :name NAME ...) per
`tabspaces--buffer-kind-handlers'."
  (delq nil (mapcar #'tabspaces--buffer-record bufs)))

(defun tabspaces--write-session-file (file session-list &optional note)
  "Write SESSION-LIST and `tabspaces-project-tab-map' to FILE.
NOTE, if non-nil, is an extra comment line placed after the header."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          ;; Print in full: a user's global truncation settings would
          ;; silently corrupt the session file.
          (print-length nil)
          (print-level nil))
      (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
              tabspaces-session-header
              ";; Created " (current-time-string) "\n\n")
      (when note
        (insert ";; " note "\n\n"))
      (insert ";; Project to tab name mapping:")
      (print `(setq tabspaces-project-tab-map ',tabspaces-project-tab-map))
      (insert ";; Tabs and buffers:")
      (print `(setq tabspaces--session-list ',session-list)))))

;; Save global session
;;;###autoload
(defun tabspaces-save-session ()
  "Save all tabspaces with their buffers and window configurations."
  (interactive)
  ;; Start from an empty list.
  (setq tabspaces--session-list nil)
  (let ((curr (tab-bar--current-tab-index)))
    ;; loop over tabs
    (cl-loop for tab in (tabspaces--list-tabspaces)
             do
             (tab-bar-select-tab-by-name tab)
             (setq tabspaces--session-list
                   (append tabspaces--session-list
                           (list (list
                                  (tabspaces--store-buffers (tabspaces--buffer-list))
                                  tab
                                  (window-state-get nil t))))))
    ;; As tab-bar-select-tab starts counting from 1, we need to add 1 to the index.
    (tab-bar-select-tab (+ curr 1)))
  ;; Write to file
  (tabspaces--write-session-file tabspaces-session-file tabspaces--session-list)
  (message "Global tabspaces session file '%s' saved" tabspaces-session-file))

;; Save current project session
(defun tabspaces-save-current-project-session (&optional session-file)
  "Save tabspace name, buffers, and window config for current tab & project.
Optional SESSION-FILE parameter specifies where to save the session file.
If not provided, uses the location specified by
`tabspaces-session-project-session-store'."
  (interactive)
  (unless (project-current)
    (error "Not in a project"))
  (let ((tabspaces--session-list nil) ;; Start from an empty list.
        (ctab (tabspaces--current-tab-name))
        (current-session (or session-file
                             (tabspaces--get-project-session-file))))
    ;; Ensure directory exists
    (make-directory (file-name-directory current-session) t)
    ;; Get buffers and window state
    (add-to-list 'tabspaces--session-list
                 (list (tabspaces--store-buffers (tabspaces--buffer-list))
                       ctab
                       (window-state-get nil t))) ;; t means include buffer names
    ;; Write to file
    (tabspaces--write-session-file current-session tabspaces--session-list)
    (message "Current project tabspaces session file '%s' saved" current-session)))

;; Save all project sessions
(defun tabspaces-save-all-project-sessions ()
  "Save each project tab to its own session file.
Iterates through all tabs, identifies which are associated with projects
via `tabspaces-project-tab-map', and saves each project tab's session
to its respective project directory based on
`tabspaces-session-project-session-store'."
  (let ((curr (tab-bar--current-tab-index))
        (saved-projects '()))
    (condition-case err
        (progn
          (dolist (tab-name (tabspaces--list-tabspaces))
            (let ((project-root (tabspaces--get-project-for-tab tab-name)))
              (when project-root
                ;; Switch to the project tab
                (tab-bar-select-tab-by-name tab-name)
                ;; Get session file path for this project
                (let* ((session-file (tabspaces--get-project-session-file-for-restore project-root))
                       (tabspaces--session-list nil)
                       (ctab tab-name))
                  ;; Ensure directory exists
                  (make-directory (file-name-directory session-file) t)
                  ;; Store buffers and window state
                  (add-to-list 'tabspaces--session-list
                               (list (tabspaces--store-buffers (tabspaces--buffer-list))
                                     ctab
                                     (window-state-get nil t)))
                  ;; Write to file
                  (tabspaces--write-session-file session-file tabspaces--session-list)
                  (push project-root saved-projects)))))
          ;; Restore original tab
          (tab-bar-select-tab (+ curr 1))
          (when saved-projects
            (message "Saved %d project session(s)" (length saved-projects))))
      (error
       (message "Error saving project sessions: %s" (error-message-string err))
       ;; Try to restore original tab even on error
       (ignore-errors (tab-bar-select-tab (+ curr 1)))))))

;; Save non-project tabs to global session
(defun tabspaces-save-non-project-tabs ()
  "Save tabs not associated with projects to the global session file.
This preserves non-project workspaces when using per-project session mode."
  (let ((curr (tab-bar--current-tab-index))
        (non-project-session-list nil))
    (condition-case err
        (progn
          (dolist (tab-name (tabspaces--list-tabspaces))
            (unless (tabspaces--get-project-for-tab tab-name)
              ;; This is a non-project tab
              (tab-bar-select-tab-by-name tab-name)
              (setq non-project-session-list
                    (append non-project-session-list
                            (list (list
                                   (tabspaces--store-buffers (tabspaces--buffer-list))
                                   tab-name
                                   (window-state-get nil t)))))))
          ;; Restore original tab
          (tab-bar-select-tab (+ curr 1))
          ;; Only write if there are non-project tabs
          (when non-project-session-list
            (tabspaces--write-session-file
             tabspaces-session-file non-project-session-list
             "Non-project tabs only (project tabs saved separately)")
            (message "Saved %d non-project tab(s) to global session" (length non-project-session-list))))
      (error
       (message "Error saving non-project tabs: %s" (error-message-string err))
       (ignore-errors (tab-bar-select-tab (+ curr 1)))))))

;; Smart session saver - dispatches based on configuration
(defun tabspaces--save-session-smart ()
  "Save sessions intelligently based on configuration.
If `tabspaces-session-project-session-store' is set, saves each project
tab to its own file and non-project tabs to the global file.
Otherwise, saves everything to the global session file (traditional behavior)."
  (cond
   ;; Per-project saving enabled
   ((and tabspaces-session
         tabspaces-session-project-session-store)
    (tabspaces-save-all-project-sessions)
    (tabspaces-save-non-project-tabs))

   ;; Traditional global saving
   (tabspaces-session
    (tabspaces-save-session))))

;;;; Session Auto-Save

(defvar tabspaces--session-auto-save-timer nil
  "Idle timer that periodically saves tabspaces sessions.")

(defun tabspaces--session-auto-save ()
  "Save sessions from the idle timer, quietly.
Skips while the minibuffer is active, since capturing window
configurations switches tabs.  Messages are suppressed so the
periodic save does not spam the echo area; errors still surface."
  (when (and tabspaces-session
             (not (active-minibuffer-window)))
    (let ((inhibit-message t))
      (tabspaces--save-session-smart))))

(defun tabspaces--cancel-session-auto-save ()
  "Cancel the session auto-save idle timer."
  (when tabspaces--session-auto-save-timer
    (cancel-timer tabspaces--session-auto-save-timer)
    (setq tabspaces--session-auto-save-timer nil)))

(defun tabspaces--setup-session-auto-save ()
  "Start the session auto-save idle timer if configured.
Does nothing unless both `tabspaces-session' and
`tabspaces-session-auto-save-delay' are non-nil."
  (tabspaces--cancel-session-auto-save)
  (when (and tabspaces-session tabspaces-session-auto-save-delay)
    (setq tabspaces--session-auto-save-timer
          (run-with-idle-timer tabspaces-session-auto-save-delay t
                               #'tabspaces--session-auto-save))))

;; Restore session functions
(defun tabspaces--get-project-session-file ()
  "Get the session file path based on configuration."
  (let* ((project-root (let ((project (project-current)))
                         (if project
                             (project-root project)
                           (error "Not in a project"))))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (session-name (concat "." project-name "-tabspaces-session.el")))
    (pcase tabspaces-session-project-session-store
      ('project (expand-file-name session-name project-root))
      ((pred stringp)
       (expand-file-name session-name tabspaces-session-project-session-store))
      ((pred functionp)
       (funcall tabspaces-session-project-session-store project-root))
      (_ (expand-file-name session-name project-root)))))

(defun tabspaces--get-project-session-file-for-restore (project)
  "Get the session file path for PROJECT based on configuration."
  (let* ((project-name (file-name-nondirectory (directory-file-name project)))
         (session-name (concat "." project-name "-tabspaces-session.el")))
    (pcase tabspaces-session-project-session-store
      ('project (expand-file-name session-name project))
      ((pred stringp)
       (expand-file-name session-name tabspaces-session-project-session-store))
      ((pred functionp)
       (funcall tabspaces-session-project-session-store project))
      (_ (expand-file-name session-name project)))))

;;;###autoload
(defun tabspaces-reuse-existing-buffer (name)
  "Return the buffer named NAME iff it is in the current tab's `buffer-list'.
Return nil if no such buffer exists, or if a buffer with NAME exists
but in another tab.  Intended for use inside restore-fns registered
via `tabspaces-register-buffer-kind': call this first and fall
through to create a fresh buffer when the result is nil.  Per-tab
dedup preserves workspace isolation when the same buffer name lives
in multiple tabs."
  (car (memq (get-buffer name) (tabspaces--buffer-list))))

(defun tabspaces--restore-buffer-record (rec)
  "Materialize one buffer from session record REC.
Returns the buffer created or reused, or nil if skipped.
Pushes unknown :kind values (or the sentinel `malformed-record' for
records that match no expected shape) into the dynamically-bound
`tabspaces--restore-unknown-kinds' accumulator.  Errors signalled by
`find-file' on a legacy file record or by a user-registered handler
are caught and logged so one bad record does not abort the entire
restore loop."
  (cond
   ((stringp rec)
    (condition-case err
        (find-file rec)
      (error
       (message "tabspaces: file restore skipped (%s): %S" rec err)
       nil)))
   ((and (consp rec) (plist-get rec :kind))
    (let* ((kind (plist-get rec :kind))
           (entry (assq kind tabspaces--buffer-kind-handlers))
           (restore-fn (and entry (nth 2 entry))))
      (cond
       (restore-fn
        (condition-case err
            (funcall restore-fn rec)
          (error
           (message "tabspaces: handler for %s signalled: %S" kind err)
           nil)))
       (t (push kind tabspaces--restore-unknown-kinds)
          nil))))
   (t
    ;; Record matched no expected shape (neither bare string nor plist
    ;; with :kind).  Surface it via the unknown-kinds channel so the
    ;; user gets a breadcrumb at end of restore instead of a silent drop.
    (push 'malformed-record tabspaces--restore-unknown-kinds)
    nil)))

(defun tabspaces--load-session-file (file)
  "Read session data from FILE without evaluating arbitrary code.
Session files written by tabspaces contain only `setq' forms that
assign quoted literals to `tabspaces-project-tab-map' and
`tabspaces--session-list'.  Read those assignments and apply them;
ignore anything else.  Project session files live inside project
directories, which may come from untrusted sources (e.g. a cloned
repository), so the file must never be handed to `load-file' or
otherwise evaluated."
  (with-temp-buffer
    (insert-file-contents file)
    (condition-case err
        ;; `read-circle' nil rejects #N=/#N# syntax, which would otherwise
        ;; let a crafted file bind a circular structure to the session
        ;; variables and hang or crash later traversals of them.
        (let ((read-circle nil))
          (while t
            (pcase (read (current-buffer))
              (`(setq ,(and (or 'tabspaces-project-tab-map
                                'tabspaces--session-list)
                            var)
                      ',val)
               (set var val))
              (form (message "tabspaces: ignoring unexpected form in %s: %S"
                             file (car-safe form))))))
      (end-of-file nil)
      (error
       (message "tabspaces: unreadable session file %s: %S" file err)))))

(defun tabspaces--rewrite-window-state (state subst)
  "Return STATE with buffer NAMEs substituted per alist SUBST.
SUBST is an alist of (saved-name . actual-name) pairs.  Walks the
three buffer name reference shapes in window-state output: leaf
\(buffer NAME . _) entries, (next-buffers . (NAMES)) forward history
lists, and (prev-buffers . ((NAME M1 M2) ...)) backward history with
marker positions.  Substituted prev-buffers entries emit (NAME 1 1)
so `window--state-put-2' creates valid markers at position 1.  Saved
point is lost for those slots, but navigation works without relying
on undocumented nil-marker behaviour."
  (cond
   ((null subst) state)
   ((and (consp state) (eq (car state) 'buffer) (stringp (cadr state)))
    (let ((repl (assoc (cadr state) subst)))
      (if repl (cons 'buffer (cons (cdr repl) (cddr state))) state)))
   ((and (consp state) (eq (car state) 'next-buffers))
    (cons 'next-buffers
          (mapcar (lambda (n)
                    (let ((r (assoc n subst))) (if r (cdr r) n)))
                  (cdr state))))
   ((and (consp state) (eq (car state) 'prev-buffers))
    (cons 'prev-buffers
          (mapcar (lambda (e)
                    (let ((r (assoc (car e) subst)))
                      (cond
                       (r (list (cdr r) 1 1))
                       (t e))))
                  (cdr state))))
   ((consp state)
    (cons (tabspaces--rewrite-window-state (car state) subst)
          (tabspaces--rewrite-window-state (cdr state) subst)))
   (t state)))


;;;###autoload
(defun tabspaces-restore-session (&optional project-or-session-file)
  "Restore tabspaces session.
If PROJECT-OR-SESSION-FILE is:
- nil: if in a project tab and per-project storage is enabled,
  restore the current project's session; otherwise restore the
  global session from `tabspaces-session-file'
- a file path: restore that specific session file
- a project path: restore that project's session based on
  `tabspaces-session-project-session-store'"
  (interactive)
  (let ((session-file
         (cond
          ;; No argument - check if we're in a project tab with per-project storage
          ((null project-or-session-file)
           (if (and tabspaces-session-project-session-store
                    (project-current))
               ;; We're in a project - restore this project's session
               (let* ((project-root (project-root (project-current)))
                      (project-session (tabspaces--get-project-session-file-for-restore project-root)))
                 (if (file-exists-p project-session)
                     project-session
                   ;; Project session doesn't exist, fall back to global
                   tabspaces-session-file))
             ;; Not in a project or per-project storage disabled - use global
             tabspaces-session-file))
          ;; File path - use directly
          ((file-exists-p project-or-session-file)
           project-or-session-file)
          ;; Project path - get session file location
          (t
           (tabspaces--get-project-session-file-for-restore project-or-session-file)))))

    (if (file-exists-p session-file)
        (progn
          (tabspaces--load-session-file session-file)
          (let ((tabspaces--restore-unknown-kinds nil)
                (skipped-remote 0))
            ;; Use placeholder buffer to avoid pollution.  Cleanup runs in
            ;; the `unwind-protect' below so the placeholder is never left
            ;; stranded, even if a tab fails to restore mid-loop.
            (unwind-protect
                (cl-loop for elm in tabspaces--session-list do
                         (switch-to-buffer "*tabspaces--placeholder*")
                         (tabspaces-switch-or-create-workspace (cadr elm))
                         (let ((subst nil))
                           (save-window-excursion
                             (dolist (rec (car elm))
                               (let ((remote
                                      (cond ((stringp rec) (file-remote-p rec))
                                            ((consp rec)
                                             ;; A handler may omit :dir, and
                                             ;; (file-remote-p nil) signals an
                                             ;; error, so guard the type.
                                             (let ((dir (plist-get rec :dir)))
                                               (and (stringp dir)
                                                    (file-remote-p dir)))))))
                                 (cond
                                  (remote (cl-incf skipped-remote))
                                  (t (let ((buf (tabspaces--restore-buffer-record rec))
                                           (sname (and (consp rec)
                                                       (plist-get rec :name))))
                                       (when buf
                                         (switch-to-buffer buf)
                                         (when (and sname
                                                    (not (equal sname
                                                                (buffer-name buf))))
                                           (push (cons sname (buffer-name buf))
                                                 subst)))))))))
                           (when (caddr elm) ; If window state exists
                             ;; A saved layout can need more space than the
                             ;; current frame offers (e.g. the tiny initial
                             ;; frame during daemon startup), in which case
                             ;; `window-state-put' signals "Window too small to
                             ;; accommodate state".  Demote it so the tab keeps
                             ;; its buffers and the rest of the session restores.
                             (with-demoted-errors "tabspaces: window layout not restored: %S"
                               (window-state-put
                                (tabspaces--rewrite-window-state (caddr elm) subst)
                                nil 'safe)))))
              ;; Clean up placeholder buffer
              (cl-loop for elm in tabspaces--session-list do
                       (tabspaces-switch-or-create-workspace (cadr elm))
                       (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
              (when (get-buffer "*tabspaces--placeholder*")
                (kill-buffer "*tabspaces--placeholder*")))
            ;; Summary messages.  These are informational.  The final
            ;; confirmation message below is what lands in the minibuffer.
            (when (> skipped-remote 0)
              (message "tabspaces: %d remote buffer(s) skipped (TRAMP)"
                       skipped-remote))
            (when tabspaces--restore-unknown-kinds
              (message "tabspaces: unknown buffer kinds in session: %S"
                       (delete-dups tabspaces--restore-unknown-kinds)))
            (message "Restored session from %s" session-file)))
      (message "No session file found at %s" session-file))))

;; Make sure session file exists
(defun tabspaces--create-session-file ()
  "Create the tabspaces session file if it does not exist."
  (unless (file-exists-p tabspaces-session-file)
    (with-temp-buffer
      (write-file tabspaces-session-file))
    (message "Created tabspaces session file: %s" tabspaces-session-file)))

;; Restore session used for startup
(defun tabspaces--restore-session-safe ()
  "Run `tabspaces-restore-session' without ever aborting Emacs startup.
An unhandled error here propagates through `after-init-hook'; under
`emacs --daemon' that makes the daemon refuse to start (\"server did not
start correctly\").  Degrade to whatever restored, plus a logged error."
  (message "Restoring tabspaces session on startup.")
  (condition-case err
      (tabspaces-restore-session)
    (error
     (message "tabspaces: session restore failed, starting clean: %S" err))))

(defun tabspaces--deferred-startup-restore ()
  "Run the deferred startup restore in the first client frame, then unhook.
Used under `emacs --daemon', where the only frame at startup is the tiny
initial frame.  Restoring window layouts needs a real, correctly sized
frame, so we wait for the first `emacsclient' frame before restoring."
  (remove-hook 'server-after-make-frame-hook #'tabspaces--deferred-startup-restore)
  (tabspaces--restore-session-safe))

(defun tabspaces--restore-session-on-startup ()
  "Restore tabspaces session on startup.

Under `emacs --daemon' the only frame at startup is the tiny initial frame,
which is too small to hold saved window layouts -- `window-state-put' would
signal \"Window too small to accommodate state\".  In that case defer the
restore to the first client frame (via `server-after-make-frame-hook'), so
tabs and their layouts land, at the correct size, on the frame the user
actually sees.

Note that this makes restore asynchronous under the daemon: the session is
not restored when `tabspaces-mode' is enabled, but later when the first
client frame connects.  Init code that runs after enabling the mode and
expects the restored tabs to already exist should account for this."
  (tabspaces--create-session-file)
  (if (and (daemonp) (not (frame-parameter nil 'client)))
      (progn
        (message "tabspaces: deferring session restore until first client frame.")
        (add-hook 'server-after-make-frame-hook #'tabspaces--deferred-startup-restore))
    (tabspaces--restore-session-safe)))

;;;; Built-in buffer-kind handlers

;; These registrations must be the last top-level forms in the session section
;; so that user registrations issued after `(require 'tabspaces)' land ahead
;; of the built-ins in `tabspaces--buffer-kind-handlers' and take precedence
;; on save (see `tabspaces-register-buffer-kind' docstring).

;; Each handler validates `:dir' before proceeding and wraps the creation
;; body in `condition-case' so a stale directory or transient error drops
;; just that buffer (with a logged breadcrumb) rather than poisoning the
;; rest of the tab's restore.

(tabspaces-register-buffer-kind
 'dired
 (lambda (b)
   (with-current-buffer b
     (when (and (derived-mode-p 'dired-mode)
                (not (derived-mode-p 'image-dired-thumbnail-mode))
                (not (consp dired-directory)))
       (list :kind 'dired
             :dir default-directory
             :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      (t (or (tabspaces-reuse-existing-buffer name)
             (condition-case err
                 (let* ((default-directory dir))
                   ;; Drop the stale cross-tab cache entry for DIR in place.
                   ;; A `let' rebinding would shadow the global, so the new
                   ;; `dired-advertise' from `dired-noselect' would mutate
                   ;; the local binding and the restored buffer would never
                   ;; appear in the global registry.  `assoc-delete-all' is
                   ;; the right tool here because dired keys with strings.
                   ;; `assq-delete-all' is a no-op on string keys because it
                   ;; compares with `eq'.  Note: in a multi-frame setup
                   ;; this `setq' is process-global, so a session restore
                   ;; on frame B can orphan frame A's dired buffer from
                   ;; the registry until that buffer is reverted.
                   (setq dired-buffers
                         (assoc-delete-all (expand-file-name dir)
                                           dired-buffers))
                   (let ((buf (dired-noselect dir)))
                     (when (and buf
                                (not (equal name (buffer-name buf)))
                                (not (get-buffer name)))
                       (with-current-buffer buf (rename-buffer name)))
                     buf))
               (error
                (message "tabspaces: dired restore skipped (%s): %S" dir err)
                nil))))))))

(tabspaces-register-buffer-kind
 'eshell
 (lambda (b)
   (with-current-buffer b
     (when (derived-mode-p 'eshell-mode)
       (list :kind 'eshell
             :dir default-directory
             :name (buffer-name)))))
 (lambda (rec)
   ;; Load eshell so `(eshell t)' below has its function definitions
   ;; available.  The top-of-file `defvar eshell-buffer-name' already
   ;; declares the symbol special for the byte-compiler, so this
   ;; `require' is not for dynamic-binding semantics.  It exists to
   ;; force eshell.el to load before a user's first restore, in case
   ;; no earlier command has triggered the autoload.
   (require 'eshell)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      (t (or (tabspaces-reuse-existing-buffer name)
             (condition-case err
                 (let* ((default-directory dir)
                        ;; Bind the saved buffer name.  (eshell t) calls
                        ;; (generate-new-buffer eshell-buffer-name), adding a
                        ;; `<N>' suffix on collision.  Cross-tab collisions
                        ;; are captured by the restore loop's substitution
                        ;; alist for window-state-put.
                        (eshell-buffer-name name))
                   (eshell t)
                   ;; `(eshell t)' selects the new buffer, so
                   ;; `(current-buffer)' is the reliable handle across
                   ;; Emacs versions where the return value may differ.
                   (current-buffer))
               (error
                (message "tabspaces: eshell restore skipped (%s): %S" dir err)
                nil))))))))

(tabspaces-register-buffer-kind
 'shell
 (lambda (b)
   (with-current-buffer b
     (when (derived-mode-p 'shell-mode)
       (list :kind 'shell
             :dir default-directory
             :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      (t (or (tabspaces-reuse-existing-buffer name)
             (condition-case err
                 (let* ((default-directory dir)
                        ;; `shell' reuses an existing buffer with the given
                        ;; name, which would be a cross-tab leak.
                        ;; `generate-new-buffer-name' pre-resolves to a
                        ;; guaranteed-unique name.
                        (buf (shell (generate-new-buffer-name name))))
                   buf)
               (error
                (message "tabspaces: shell restore skipped (%s): %S" dir err)
                nil))))))))

(tabspaces-register-buffer-kind
 'vterm
 (lambda (b)
   (with-current-buffer b
     (when (derived-mode-p 'vterm-mode)
       (list :kind 'vterm
             :dir default-directory
             :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      ((not (require 'vterm nil t))
       (message "tabspaces: vterm not installed; skipping %s" name)
       nil)
      (t (or (tabspaces-reuse-existing-buffer name)
             (condition-case err
                 (let ((default-directory dir)
                       ;; `vterm' with a non-nil, non-string argument
                       ;; creates a new session named with
                       ;; `vterm-buffer-name', adding a `<N>' suffix on
                       ;; collision.  Cross-tab collisions are captured by
                       ;; the restore loop's substitution alist.
                       (vterm-buffer-name name))
                   (let ((buf (vterm t)))
                     ;; `vterm' returns the buffer in current releases;
                     ;; older ones only select it.
                     (if (bufferp buf) buf (current-buffer))))
               (error
                (message "tabspaces: vterm restore skipped (%s): %S" dir err)
                nil))))))))

(tabspaces-register-buffer-kind
 'eat
 (lambda (b)
   (with-current-buffer b
     (when (derived-mode-p 'eat-mode)
       (list :kind 'eat
             :dir default-directory
             :name (buffer-name)))))
 (lambda (rec)
   (let ((name (plist-get rec :name))
         (dir  (plist-get rec :dir)))
     (cond
      ((not (and dir (stringp dir) (file-directory-p dir))) nil)
      ((not (require 'eat nil t))
       (message "tabspaces: eat not installed; skipping %s" name)
       nil)
      (t (or (tabspaces-reuse-existing-buffer name)
             (condition-case err
                 (let ((default-directory dir)
                       ;; Like eshell: `eat' with a non-nil second argument
                       ;; creates a new session named `eat-buffer-name',
                       ;; uniquified on collision.
                       (eat-buffer-name name))
                   (let ((buf (eat nil t)))
                     (if (bufferp buf) buf (current-buffer))))
               (error
                (message "tabspaces: eat restore skipped (%s): %S" dir err)
                nil))))))))

;;;; Define Keymaps
(defvar tabspaces-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'tabspaces-clear-buffers)
    (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
    (define-key map (kbd "d") 'tabspaces-close-workspace)
    (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
    (define-key map (kbd "n") 'tabspaces-rename-workspace)
    (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
    (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
    (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
    (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
    (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
    (define-key map (kbd "w") 'tabspaces-show-workspaces)
    (define-key map (kbd "T") 'tabspaces-toggle-echo-area-display)
    map)
  "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
(fset 'tabspaces-command-map tabspaces-command-map)

(defun tabspaces--normalize-prefix (value)
  "Return VALUE as a raw key sequence.
A string in canonical `kbd' syntax is converted with `kbd', for
compatibility with the former string format of
`tabspaces-keymap-prefix'.  Canonical syntax is what
`key-description' produces, so a string qualifies exactly when the
conversion round-trips back to it.  Raw key sequences never
round-trip and so pass through unchanged, as do vectors and nil."
  (if (and (stringp value)
           (condition-case nil
               (equal (key-description (kbd value)) value)
             (error nil)))
      (kbd value)
    value))

(defvar tabspaces-mode-map
  (let ((map (make-sparse-keymap)))
    (when tabspaces-keymap-prefix
      (define-key map (tabspaces--normalize-prefix tabspaces-keymap-prefix)
                  'tabspaces-command-map))
    map)
  "Keymap for Tabspaces mode.")

;;;; Define Minor Mode
;;;###autoload
(define-minor-mode tabspaces-mode
  "Create a global minor mode for `tabspaces', or buffer-isolated workspaces.
This uses Emacs `tab-bar' and `project.el'."
  :lighter ""
  :keymap tabspaces-mode-map
  :global t
  (cond (tabspaces-mode
         ;; Set up tabspace isolated buffers
         (dolist (frame (frame-list))
           (tabspaces--set-buffer-predicate frame))
         (add-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
         (add-to-list 'tab-bar-tab-post-open-functions #'tabspaces--tab-post-open-function)
         ;; Anchor project context to the tab for buffers outside any
         ;; project.  Depth 90 keeps it after the stock backends, so it
         ;; only fires when they find nothing.
         (add-hook 'project-find-functions #'tabspaces--tab-project 90)
         ;; Keep `tabspaces-project-tab-map' in sync across tab renames.
         (advice-add 'tab-bar-rename-tab :around #'tabspaces--sync-tab-rename)
         (when tabspaces-project-switch-opens-workspace
           (advice-add 'project-switch-project :around
                       #'tabspaces--project-switch-advice))
         ;; Option to always use filtered buffers when minor mode is enabled.
         (when tabspaces-use-filtered-buffers-as-default
           ;; Remap switch-to-buffer
           (define-key (current-global-map) [remap switch-to-buffer] #'tabspaces-switch-to-buffer))
         (when tabspaces-session
           (add-hook 'kill-emacs-hook #'tabspaces--save-session-smart)
           (tabspaces--setup-session-auto-save))
         (when tabspaces-session-auto-restore
           (tabspaces--restore-session-on-startup))
         ;; Setup echo area display if enabled
         (tabspaces--echo-area-setup))
        (t
         ;; Remove all modifications
         (dolist (frame (frame-list))
           (tabspaces--reset-buffer-predicate frame))
         ;; Remove the remap whenever it points at our command, regardless of
         ;; the current option value: the option may have been toggled off
         ;; after the mode was enabled.
         (when (eq (lookup-key (current-global-map) [remap switch-to-buffer])
                   #'tabspaces-switch-to-buffer)
           (define-key (current-global-map) [remap switch-to-buffer] nil))
         (setq tab-bar-tab-post-open-functions (remove #'tabspaces--tab-post-open-function tab-bar-tab-post-open-functions))
         (remove-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
         (remove-hook 'project-find-functions #'tabspaces--tab-project)
         (advice-remove 'tab-bar-rename-tab #'tabspaces--sync-tab-rename)
         (advice-remove 'project-switch-project #'tabspaces--project-switch-advice)
         (remove-hook 'kill-emacs-hook #'tabspaces--save-session-smart)
         (tabspaces--cancel-session-auto-save)
         ;; Cancel a pending deferred restore (daemon case: mode disabled
         ;; before the first client frame ever connected).
         (remove-hook 'server-after-make-frame-hook #'tabspaces--deferred-startup-restore)
         ;; Cleanup echo area display
         (tabspaces--echo-area-cleanup))))

;;; Provide
(provide 'tabspaces)
;;; tabspaces.el ends here
