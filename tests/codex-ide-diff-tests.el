;;; codex-ide-diff-tests.el --- Tests for codex-ide-diff.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide-diff)
(require 'ediff)

;;; Pure builder tests

(ert-deftest codex-ide-diff-buffer-name-path ()
  "Path policy names buffers from FILE-PATH and SIDE."
  (let ((codex-ide-diff-buffer-naming 'path))
    (should (equal "*codex-ide-diff:foo.el[old]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'old)))
    (should (equal "*codex-ide-diff:foo.el[new]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'new)))))

(ert-deftest codex-ide-diff-buffer-name-generic ()
  "Generic policy uses an incrementing counter for buffer names."
  (let ((codex-ide-diff-buffer-naming 'generic)
        (codex-ide-diff--counter 0))
    (should (equal "*codex-ide-diff-1[old]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'old)))
    (should (equal "*codex-ide-diff-2[new]*"
                   (codex-ide-diff--buffer-name "/tmp/foo.el" 'new)))))

(ert-deftest codex-ide-diff-make-temp-buffer ()
  "Temp buffer creation inserts content and is read-only."
  (let ((buf (codex-ide-diff--make-temp-buffer
              " *test-diff-temp*" "hello world")))
    (unwind-protect
        (with-current-buffer buf
          (should (equal "hello world" (buffer-string)))
          (should buffer-read-only)
          (should-not (buffer-modified-p)))
      (kill-buffer buf))))

(ert-deftest codex-ide-diff-make-temp-buffer-empty ()
  "Temp buffer with nil content is empty, not errored."
  (let ((buf (codex-ide-diff--make-temp-buffer " *test-diff-empty*" nil)))
    (unwind-protect
        (with-current-buffer buf
          (should (string= "" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest codex-ide-diff-make-temp-buffer-pair ()
  "Buffer pair returns (A . B) with correct content, both live."
  (let ((pair (codex-ide-diff--make-temp-buffer-pair
               "old text" "new text" "/tmp/foo.el")))
    (unwind-protect
        (progn
          (should (consp pair))
          (should (buffer-live-p (car pair)))
          (should (buffer-live-p (cdr pair)))
          (with-current-buffer (car pair)
            (should (equal "old text" (buffer-string))))
          (with-current-buffer (cdr pair)
            (should (equal "new text" (buffer-string)))))
      (when (buffer-live-p (car pair)) (kill-buffer (car pair)))
      (when (buffer-live-p (cdr pair)) (kill-buffer (cdr pair))))))

;;; Asynchronous review lifecycle

(defun codex-ide-diff-test--accept (owner)
  "Accept OWNER through the actual control-buffer command."
  (with-current-buffer (plist-get owner :control)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (call-interactively (key-binding codex-ide-diff-accept-key)))))

(ert-deftest codex-ide-diff-review-edited-content ()
  "Real Ediff returns exact edited text and never modifies the target."
  (let* ((directory (make-temp-file "codex-diff-test-" t))
         (path (expand-file-name "target.el" directory))
         (before (buffer-list))
         owner result live)
    (unwind-protect
        (progn
          (write-region "disk original" nil path nil 'silent)
          (setq live (find-file-noselect path))
          (with-current-buffer live (insert "unsaved "))
          (setq owner (codex-ide-diff-review
                       "old" "proposal" path
                       (lambda (status content) (push (list status content) result))))
          (should-not result)
          (with-current-buffer (plist-get owner :buffer-a)
            (should buffer-read-only)
            (should (equal (buffer-string) "old")))
          (with-current-buffer (plist-get owner :buffer-b)
            (should-not buffer-read-only)
            (erase-buffer)
            (insert "edited λ\n")
            (narrow-to-region 2 3))
          (codex-ide-diff-test--accept owner)
          (should (equal result '((accepted "edited λ\n"))))
          (codex-ide-diff-cancel owner)
          (should (= (length result) 1))
          (with-current-buffer live
            (should (equal (buffer-string) "unsaved disk original")))
          (with-temp-buffer
            (insert-file-contents path)
            (should (equal (buffer-string) "disk original")))
          (should-not codex-ide-diff--active)
          (dolist (buffer (buffer-list))
            (unless (or (memq buffer before) (eq buffer live))
              (should-not (string-match-p "\\`\\*\\(?:[Ee]diff\\|codex-ide-diff\\)" (buffer-name buffer))))))
      (when owner (codex-ide-diff-cancel owner))
      (when (buffer-live-p live)
        (with-current-buffer live (set-buffer-modified-p nil))
        (kill-buffer live))
      (delete-directory directory t))))

(ert-deftest codex-ide-diff-review-quit-cancel-and-kill ()
  "Quit rejects; cancellation and killed review buffers never accept."
  (dolist (action '(quit cancel control old proposal))
    (let (owner result)
      (unwind-protect
          (progn
            (setq owner (codex-ide-diff-review
                         "old" "new" "fixture"
                         (lambda (status content) (push (list status content) result))))
            (pcase action
              ('quit (with-current-buffer (plist-get owner :control)
                       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                         (call-interactively (key-binding "q")))))
              ('cancel (codex-ide-diff-cancel owner))
              (_ (kill-buffer (plist-get owner
                                         (pcase action
                                           ('control :control)
                                           ('old :buffer-a)
                                           ('proposal :buffer-b))))))
            (when-let* ((timer (plist-get owner :timer)))
              (should-not result)
              (timer-event-handler timer))
            (should (equal result (list (list (if (eq action 'quit)
                                                  'rejected 'cancelled) nil))))
            (dolist (key '(:control :buffer-a :buffer-b))
              (should-not (buffer-live-p (plist-get owner key)))))
        (when owner (codex-ide-diff-cancel owner))))))

(ert-deftest codex-ide-diff-review-busy-and-stale ()
  "A concurrent review is busy, and stale cancellation preserves its successor."
  (let (first second result)
    (unwind-protect
        (progn
          (setq first (codex-ide-diff-review "a" "b" "one" #'ignore))
          (should-error (codex-ide-diff-review "c" "d" "two" #'ignore)
                        :type 'user-error)
          (codex-ide-diff-cancel first)
          (setq second (codex-ide-diff-review
                        "c" "d" "two" (lambda (&rest args) (setq result args))))
          (codex-ide-diff-cancel first)
          (codex-ide-diff--settle first 'accepted)
          (should (eq second codex-ide-diff--active))
          (should-not result))
      (when first (codex-ide-diff-cancel first))
      (when second (codex-ide-diff-cancel second)))))

(ert-deftest codex-ide-diff-review-setup-error ()
  "A failure after allocating the Ediff control buffer releases owned resources."
  (let ((ediff-startup-hook (list (lambda () (error "fixture failure"))))
        result)
    (should-error (codex-ide-diff-review
                   "a" "b" "fixture" (lambda (&rest args) (setq result args))))
    (should (equal result '(cancelled nil)))
    (should-not codex-ide-diff--active)
    (dolist (buffer (buffer-list))
      (unless (equal (buffer-name buffer) "*Ediff Registry*")
        (should-not (string-match-p "\\`\\*\\(?:[Ee]diff\\|codex-ide-diff\\)" (buffer-name buffer)))))))

(ert-deftest codex-ide-diff-review-preserves-changed-layout ()
  "Closing a review does not replace a subsequently arranged foreign layout."
  (save-window-excursion
    (let ((foreign (generate-new-buffer " *foreign*")) owner)
      (unwind-protect
          (progn
            (setq owner (codex-ide-diff-review "a" "b" "fixture" #'ignore))
            (dolist (window (window-list))
              (set-window-dedicated-p window nil))
            (delete-other-windows)
            (switch-to-buffer foreign)
            (split-window-right)
            (let ((config (current-window-configuration)))
              (codex-ide-diff-cancel owner)
              (should (window-configuration-equal-p
                       config (current-window-configuration)))))
        (when owner (codex-ide-diff-cancel owner))
        (kill-buffer foreign)))))

(ert-deftest codex-ide-diff-preview-read-only-boolean ()
  "Compatibility preview keeps both buffers read-only and returns a boolean."
  (cl-letf (((symbol-function 'recursive-edit)
             (lambda ()
               (with-current-buffer (plist-get codex-ide-diff--active :buffer-b)
                 (should buffer-read-only))
               (codex-ide-diff-test--accept codex-ide-diff--active))))
    (should (eq t (codex-ide-diff-preview "a" "b" "fixture"))))
  (cl-letf (((symbol-function 'recursive-edit)
             (lambda () (codex-ide-diff-cancel codex-ide-diff--active))))
    (should-not (codex-ide-diff-preview "a" "b" "fixture"))))

(ert-deftest codex-ide-diff-review-cancelled-confirmation ()
  "Declining acceptance leaves the proposal pending until explicit rejection."
  (let (owner result)
    (unwind-protect
        (progn
          (setq owner (codex-ide-diff-review
                       "a" "b" "fixture" (lambda (&rest args) (setq result args))))
          (with-current-buffer (plist-get owner :control)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
              (call-interactively (key-binding codex-ide-diff-accept-key))))
          (should-not result)
          (should (eq (plist-get owner :status) 'pending)))
      (when owner (codex-ide-diff-cancel owner)))))

(ert-deftest codex-ide-diff-review-callback-error-releases-owner ()
  "A failing consumer cannot leave Ediff resources or the busy owner behind."
  (let ((owner (codex-ide-diff-review
                "a" "b" "fixture" (lambda (&rest _) (error "Callback fixture")))))
    (should-error (codex-ide-diff-test--accept owner))
    (should-not codex-ide-diff--active)
    (dolist (key '(:control :buffer-a :buffer-b))
      (should-not (buffer-live-p (plist-get owner key))))))

(ert-deftest codex-ide-diff-review-early-setup-error ()
  "Failure during Ediff mode setup cancels and removes partially built buffers."
  (let ((ediff-mode-hook (list (lambda () (error "Early fixture")))) result)
    (should-error (codex-ide-diff-review
                   "a" "b" "fixture" (lambda (&rest args) (setq result args))))
    (should (equal result '(cancelled nil)))
    (should-not codex-ide-diff--active)
    (should-not (get-buffer "*Ediff Control Panel*"))
    (should-not (get-buffer "*codex-ide-diff:fixture[old]*"))))

(ert-deftest codex-ide-diff-review-restores-owned-layout ()
  "A review whose layout remains current restores the original windows."
  (save-window-excursion
    (with-current-buffer (window-buffer (selected-window))
      (let ((config (current-window-configuration))
            (owner (codex-ide-diff-review "a" "b" "fixture" #'ignore)))
        (codex-ide-diff-cancel owner)
        (should (window-configuration-equal-p config (current-window-configuration)))))))

(ert-deftest codex-ide-diff-review-preserves-foreign-tab ()
  "Cancellation from a different tab preserves the selected tab and its layout."
  (let ((foreign (generate-new-buffer " *foreign-tab*"))
        (tab-bar-show nil)
        (tab-bar-was-enabled tab-bar-mode) owner)
    (unwind-protect
        (progn
          (setq owner (codex-ide-diff-review "a" "b" "fixture" #'ignore))
          (tab-bar-new-tab)
          (dolist (window (window-list)) (set-window-dedicated-p window nil))
          (delete-other-windows)
          (switch-to-buffer foreign)
          (split-window-right)
          (let ((tab (tab-bar--current-tab-find))
                (config (current-window-configuration)))
            (codex-ide-diff-cancel owner)
            (should (eq tab (tab-bar--current-tab-find)))
            (should (window-configuration-equal-p config (current-window-configuration)))))
      (when owner (codex-ide-diff-cancel owner))
      (tab-bar-close-tab)
      (unless tab-bar-was-enabled (tab-bar-mode -1))
      (kill-buffer foreign))))

(ert-deftest codex-ide-diff-review-preserves-foreign-frame ()
  "Closing a review from another frame preserves both frames' foreign layouts."
  (skip-unless (display-graphic-p))
  (let* ((origin (selected-frame))
         (foreign-frame (make-frame))
         (foreign (generate-new-buffer " *foreign-frame*"))
         owner origin-config)
    (unwind-protect
        (progn
          (select-frame origin)
          (setq owner (codex-ide-diff-review "a" "b" "fixture" #'ignore))
          (dolist (window (window-list)) (set-window-dedicated-p window nil))
          (delete-other-windows)
          (switch-to-buffer foreign)
          (setq origin-config (current-window-configuration))
          (select-frame foreign-frame)
          (switch-to-buffer foreign)
          (split-window-right)
          (let ((config (current-window-configuration)))
            (codex-ide-diff-cancel owner)
            (should (eq (selected-frame) foreign-frame))
            (should (window-configuration-equal-p config (current-window-configuration)))
            (should (window-configuration-equal-p
                     origin-config (with-selected-frame origin
                                     (current-window-configuration))))))
      (when owner (codex-ide-diff-cancel owner))
      (when (frame-live-p foreign-frame) (delete-frame foreign-frame))
      (kill-buffer foreign))))

(ert-deftest codex-ide-diff-review-teardown-error-cleans-named-auxiliary ()
  "Fallback teardown releases auxiliary buffers referenced by their names."
  (let* ((owner (codex-ide-diff-review "a" "b" "fixture" #'ignore))
         (auxiliary (with-current-buffer (plist-get owner :control)
                      (get-buffer-create ediff-msg-buffer))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ediff-really-quit)
                     (lambda (&rest _) (error "Teardown fixture"))))
            (codex-ide-diff-cancel owner))
          (should-not (buffer-live-p auxiliary)))
      (when (buffer-live-p auxiliary) (kill-buffer auxiliary))
      (codex-ide-diff-cancel owner))))

(ert-deftest codex-ide-diff-review-teardown-error-cancels ()
  "A failed native teardown cannot report an accepted proposal."
  (let (owner result)
    (setq owner (codex-ide-diff-review
                 "a" "b" "fixture" (lambda (&rest args) (setq result args))))
    (cl-letf (((symbol-function 'ediff-really-quit)
               (lambda (&rest _) (error "Teardown fixture"))))
      (codex-ide-diff-test--accept owner))
    (should (equal result '(cancelled nil)))
    (should-not (memq (plist-get owner :control) ediff-session-registry))
    (should-not codex-ide-diff--active)
    (dolist (key '(:control :buffer-a :buffer-b))
      (should-not (buffer-live-p (plist-get owner key))))))

(ert-deftest codex-ide-diff-review-return-to-owning-tab ()
  "Returning to the owning tab restores its original layout on acceptance."
  (save-window-excursion
    (let ((tab-bar-show nil)
          (tab-bar-was-enabled tab-bar-mode)
          config owner)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (tab-bar-close-tab)
            (setq config (current-window-configuration))
            (setq owner (codex-ide-diff-review "a" "b" "fixture" #'ignore))
            (tab-bar-new-tab)
            (should-not (alist-get 'codex-ide-diff-owner (tab-bar--current-tab-find)))
            (tab-bar-close-tab)
            (codex-ide-diff-test--accept owner)
            (should (window-configuration-equal-p config (current-window-configuration)))
            (should-not (alist-get 'codex-ide-diff-owner (tab-bar--current-tab-find))))
        (when owner (codex-ide-diff-cancel owner))
        (unless tab-bar-was-enabled (tab-bar-mode -1))))))

(ert-deftest codex-ide-diff-review-setup-callback-starts-successor ()
  "A synchronous failure callback can start a successor without stale setup hooks."
  (let (first second failed result)
    (let ((ediff-mode-hook
           (list (lambda ()
                   (unless failed
                     (setq failed t first codex-ide-diff--active)
                     (error "First setup fixture"))))))
      (unwind-protect
          (progn
            (should-error
             (codex-ide-diff-review
              "a" "b" "first"
              (lambda (&rest _)
                (setq second (codex-ide-diff-review
                              "c" "d" "second"
                              (lambda (&rest args) (setq result args)))))))
            (should-not (buffer-live-p (plist-get first :control)))
            (should (eq second codex-ide-diff--active))
            (codex-ide-diff-test--accept second)
            (should (equal result '(accepted "d"))))
        (when second (codex-ide-diff-cancel second))))))

(ert-deftest codex-ide-diff-review-preserves-shared-ediff-buffers ()
  "Native and fallback cleanup preserve another session's shared scratch buffers."
  (let ((shared (mapcar (lambda (symbol)
                          (let ((buffer (get-buffer-create (default-value symbol))))
                            (with-current-buffer buffer
                              (erase-buffer)
                              (insert "foreign contents"))
                            buffer))
                        '(ediff-tmp-buffer ediff-msg-buffer ediff-debug-buffer))))
    (unwind-protect
        (dolist (fail '(nil t))
          (let ((ediff-mode-hook
                 (when fail (list (lambda () (error "Shared buffer fixture"))))))
            (if fail
                (should-error (codex-ide-diff-review "a" "b" "fixture" #'ignore))
              (let ((owner (codex-ide-diff-review "a" "b" "fixture" #'ignore)))
                (unwind-protect
                    (progn
                      (with-current-buffer (plist-get owner :buffer-b)
                        (goto-char (point-max))
                        (insert " edited"))
                      (with-current-buffer (plist-get owner :control)
                        (call-interactively (key-binding "!"))))
                  (codex-ide-diff-cancel owner)))))
          (dolist (buffer shared)
            (should (buffer-live-p buffer))
            (with-current-buffer buffer
              (should (equal (buffer-string) "foreign contents")))))
      (mapc #'kill-buffer shared))))


;;; Queued presentation

(defmacro codex-ide-diff-test--with-queued-review (&rest body)
  "Run BODY with isolated receipt state and a stable terminal owner."
  (declare (indent 0))
  `(let* ((codex-ide-diff--receipts (make-hash-table :test 'equal))
          (codex-ide-diff--queued-review nil)
          (codex-ide-diff--active nil)
          (global-mode-string nil)
          (terminal (generate-new-buffer " *queued-terminal*"))
          (session (list :root temporary-file-directory :id 1 :buffer terminal)))
     (unwind-protect
         (cl-letf (((symbol-function 'codex-ide-diff--request-session)
                    (lambda (buffer) (and (eq buffer terminal) session))))
           ,@body)
       (codex-ide-diff--cancel-requests)
       (maphash (lambda (_ receipt)
                  (dolist (key '(:timer :start-timer))
                    (when-let* ((timer (plist-get receipt key))) (cancel-timer timer))))
                codex-ide-diff--receipts)
       (kill-buffer terminal))))

(ert-deftest codex-ide-diff-queued-notification-preserves-layout ()
  "A timer only notifies; explicit show opens the queued review."
  (save-window-excursion
    (codex-ide-diff-test--with-queued-review
     (let* ((config (current-window-configuration))
            (tab (tab-bar--current-tab-find))
            (result (codex-ide-diff--request-start
                     (buffer-name terminal) "queued" "nested/a.el" "old" "new"))
            (receipt (gethash (cdr (assoc "review_id" result)) codex-ide-diff--receipts))
            notified)
       (cl-letf (((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (setq notified (apply #'format format-string args)))))
         (timer-event-handler (plist-get receipt :start-timer)))
       (should-not codex-ide-diff--active)
       (should (eq tab (tab-bar--current-tab-find)))
       (should (window-configuration-equal-p config (current-window-configuration)))
       (should (memq 'codex-ide-diff--mode-line global-mode-string))
       (should (string-match-p "codex-ide-diff-show" notified))
       (call-interactively #'codex-ide-diff-show)
       (should (eq codex-ide-diff--active (plist-get receipt :ui-owner)))
       (should-not codex-ide-diff--queued-review)
       (should-not (memq 'codex-ide-diff--mode-line global-mode-string))
       (codex-ide-diff-test--accept codex-ide-diff--active)
       (should (equal (cdr (assoc "content" (codex-ide-diff--request-result
                                             (plist-get receipt :id)))) "new"))))))

(ert-deftest codex-ide-diff-cancelled-queue-cannot-notify-or-open ()
  "Late queue notifications and explicit opening cannot revive cancellation."
  (codex-ide-diff-test--with-queued-review
   (let* ((result (codex-ide-diff--request-start
                   (buffer-name terminal) "cancel" "a.el" "old" "new"))
          (receipt (gethash (cdr (assoc "review_id" result)) codex-ide-diff--receipts))
          (timer (plist-get receipt :start-timer)) notified)
     (codex-ide-diff--request-result (plist-get receipt :id) t)
     (cl-letf (((symbol-function 'message) (lambda (&rest _) (setq notified t))))
       (timer-event-handler timer))
     (should-not notified)
     (should-not (memq 'codex-ide-diff--mode-line global-mode-string))
     (should-error (codex-ide-diff-show) :type 'user-error)
     (should-not codex-ide-diff--active))))

(ert-deftest codex-ide-diff-show-refuses-minibuffer ()
  "Explicit show preserves a queued proposal during minibuffer input."
  (codex-ide-diff-test--with-queued-review
   (codex-ide-diff--request-start (buffer-name terminal) "mini" "a.el" "old" "new")
   (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () (selected-window))))
     (should-error (codex-ide-diff-show) :type 'user-error))
   (should codex-ide-diff--queued-review)
   (should-not codex-ide-diff--active)))

(ert-deftest codex-ide-diff-proposal-skips-change-major-mode-hook ()
  "Choosing a proposal's mode does not run global mode-change hooks."
  (let ((change-major-mode-hook
         (list (lambda () (error "Mode change hook must not run")))))
    (let ((buffer (codex-ide-diff--make-temp-buffer
                   " *test-proposal-mode*" "(example)" "/tmp/example.el")))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq major-mode 'emacs-lisp-mode)))
        (kill-buffer buffer)))))

(ert-deftest codex-ide-diff-proposal-mode-and-header ()
  "Proposals use the target mode without hooks or file association, with key hints."
  (let ((emacs-lisp-mode-hook (list (lambda () (error "Mode hook must not run"))))
        (find-file-hook (list (lambda () (error "File hook must not run"))))
        (codex-ide-diff-accept-key (kbd "C-c C-y"))
        (default-directory temporary-file-directory)
        owner)
    (unwind-protect
        (progn
          (setq owner (codex-ide-diff-review
                       "(old)" "(new)\n;; Local Variables:\n;; eval: (error \"No eval\")\n;; End:\n"
                       (expand-file-name "nested/target.el" default-directory) #'ignore))
          (with-current-buffer (plist-get owner :buffer-b)
            (should (eq major-mode 'emacs-lisp-mode))
            (should-not buffer-file-name))
          (with-current-buffer (plist-get owner :control)
            (let ((header (symbol-value header-line-format)))
              (should (string-match-p "nested/target.el" header))
              (should (string-match-p "C-c C-y" header))
              (should (string-match-p "q.*reject" header))
              (should (string-match-p "?.*help" header))
              (let ((start (string-match "C-c C-y" header)))
                (should (eq (get-text-property start 'face header)
                            'help-key-binding))))))
      (when owner (codex-ide-diff-cancel owner)))))

(ert-deftest codex-ide-diff-preview-is-programmatic ()
  "The legacy boolean preview has no interactive raw-content prompts."
  (should-not (commandp #'codex-ide-diff-preview)))

(ert-deftest codex-ide-diff-renders-literal-header-and-pending-indicator ()
  "Render actual mode lines, preserving percent characters and semantic faces."
  ;; GNU Emacs returns an empty string from `format-mode-line' in batch mode.
  (when noninteractive (ert-skip "Mode-line rendering requires an interactive display"))
  (save-window-excursion
    (dolist (existing '(nil (" existing")))
      (codex-ide-diff-test--with-queued-review
        (setq global-mode-string (copy-tree existing))
        (let* ((codex-ide-diff-accept-key (kbd "C-c %"))
               (result (codex-ide-diff--request-start
                        (buffer-name terminal) "render" "nested/%b.el" "old" "new"))
               (id (cdr (assoc "review_id" result)))
               (rendered (format-mode-line 'global-mode-string)))
          (should (string-match-p "Codex review pending" rendered))
          (should (eq (get-text-property
                       (string-match "Codex review pending" rendered) 'face rendered)
                      'codex-ide-diff-pending))
          (codex-ide-diff-show)
          (should (equal (format-mode-line 'global-mode-string)
                         (if existing " existing" "")))
          (with-current-buffer (plist-get codex-ide-diff--active :control)
            (let ((header (format-mode-line header-line-format)))
              (should (string-match-p "nested/%b.el" header))
              (should (string-match-p "C-c % accept" header))
              (should (eq (get-text-property (string-match "C-c %" header) 'face header)
                          'help-key-binding))
              (should (eq (key-binding codex-ide-diff-accept-key)
                          #'codex-ide-diff-accept))))
          (codex-ide-diff-test--accept codex-ide-diff--active)
          (should (equal (cdr (assoc "status" (codex-ide-diff--request-result id)))
                         "accepted")))
        (codex-ide-diff--request-start
         (buffer-name terminal) "cancel-render" "a.el" "old" "new")
        (codex-ide-diff--request-result (plist-get codex-ide-diff--queued-review :id) t)
        (should (equal (format-mode-line 'global-mode-string)
                       (if existing " existing" "")))))))

(ert-deftest codex-ide-diff-proposal-initializes-core-mode ()
  "Proposal buffers retain fontification and syntax setup without user hooks."
  (let ((noninteractive nil)
        (emacs-lisp-mode-hook (list (lambda () (error "User mode hook ran"))))
        (after-change-major-mode-hook (list (lambda () (error "User after hook ran"))))
        (change-major-mode-after-body-hook (list (lambda () (error "User body hook ran")))))
    (let ((buffer (codex-ide-diff--make-temp-buffer
                   "*proposal-core-test*" "(defun example () ##)" "/tmp/example.el")))
      (unwind-protect
          (with-current-buffer buffer
            (should font-lock-mode)
            (should syntax-propertize-function)
            (should parse-sexp-lookup-properties)
            (font-lock-ensure)
            (goto-char (point-min))
            (search-forward "defun")
            (should (get-text-property (1- (point)) 'face))
            (search-forward "##")
            (should (get-text-property (- (point) 2) 'syntax-table)))
        (kill-buffer buffer)))))

(provide 'codex-ide-diff-tests)
;;; codex-ide-diff-tests.el ends here
