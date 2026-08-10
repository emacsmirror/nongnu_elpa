;;; vm-page-test.el --- Tests for vm-page.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM page functions in vm-page.el

;;; Code:

(require 'vm-test-init)
(require 'vm-page)

;;; Page function existence tests

(ert-deftest vm-page-test-functions-exist ()
  "Test that page navigation functions exist."
  (should (fboundp 'vm-scroll-forward))
  (should (fboundp 'vm-scroll-backward))
  (should (fboundp 'vm-scroll-forward-one-line))
  (should (fboundp 'vm-scroll-backward-one-line))
  (should (fboundp 'vm-beginning-of-message))
  (should (fboundp 'vm-end-of-message))
  (should (fboundp 'vm-widen-page))
  (should (fboundp 'vm-narrow-to-page)))

;;; Button navigation functions

(ert-deftest vm-page-test-button-functions-exist ()
  "Test that button navigation functions exist."
  (should (fboundp 'vm-next-button))
  (should (fboundp 'vm-previous-button))
  (should (fboundp 'vm-move-to-next-button))
  (should (fboundp 'vm-move-to-previous-button)))

;;; Header highlighting functions

(ert-deftest vm-page-test-highlight-functions-exist ()
  "Test that header highlighting functions exist."
  (should (fboundp 'vm-highlight-headers))
  (should (fboundp 'vm-highlight-headers-maybe))
  (should (fboundp 'vm-energize-urls))
  (should (fboundp 'vm-energize-headers))
  (should (fboundp 'vm-energize-urls-in-message-region)))

;;; Presentation functions

(ert-deftest vm-page-test-presentation-functions-exist ()
  "Test that presentation functions exist."
  (should (fboundp 'vm-present-current-message))
  (should (fboundp 'vm-preview-current-message))
  (should (fboundp 'vm-show-current-message))
  (should (fboundp 'vm-expose-hidden-headers)))

;;; X-Face functions

(ert-deftest vm-page-test-xface-functions-exist ()
  "Test that X-Face display functions exist."
  (should (fboundp 'vm-display-xface)))

;;; vm-emit-eom-blurb tests

(ert-deftest vm-page-test-emit-eom-blurb-exists ()
  "Test vm-emit-eom-blurb exists."
  (should (fboundp 'vm-emit-eom-blurb)))

;;; vm-howl-if-eom tests

(ert-deftest vm-page-test-howl-if-eom-exists ()
  "Test vm-howl-if-eom exists."
  (should (fboundp 'vm-howl-if-eom)))

;;; URL handling variables

(ert-deftest vm-page-test-url-variables-exist ()
  "Test that URL handling variables exist."
  (should (boundp 'vm-highlight-url-face))
  (should (boundp 'vm-url-browser)))

;;; Header visibility variables

(ert-deftest vm-page-test-header-variables-exist ()
  "Test that header display variables exist."
  (should (boundp 'vm-visible-headers))
  (should (boundp 'vm-invisible-header-regexp)))

;;; vm-narrow-for-preview tests

(ert-deftest vm-page-test-narrow-for-preview-exists ()
  "Test vm-narrow-for-preview exists."
  (should (fboundp 'vm-narrow-for-preview)))

;;; Scrolling variables

(ert-deftest vm-page-test-scroll-variables-exist ()
  "Test that scroll-related variables exist."
  (should (boundp 'vm-auto-next-message))
  (should (boundp 'vm-honor-page-delimiters)))

;;; vm-url-help tests

(ert-deftest vm-page-test-url-help-exists ()
  "Test vm-url-help function exists."
  (should (fboundp 'vm-url-help)))


;;; Exposing headers while reading a later page (#513)

(defmacro vm-page-test-with-paged-message (&rest body)
  "Show a three-page message read, and run BODY in the buffer showing it.
`vm-honor-page-delimiters\=' is on, and the message carries a header that
`vm-visible-headers\=' hides, so exposing them is observable."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-page-test" t)))
          (file (expand-file-name "folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-honor-page-delimiters t)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-user-interaction-buffer vm-user-interaction-buffer)
          (vm-current-warning vm-current-warning)
          (vm-summary-tokenized-compiled-format-alist
           vm-summary-tokenized-compiled-format-alist)
          (before (buffer-list)))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert "From alice@example.com  Mon Jan  1 00:00:00 2026\n"
                     "From: alice@example.com\nTo: b@example.com\n"
                     "Subject: pages\nX-Hidden-Thing: secret\n\n"
                     "page one text\n\f\npage two text\n\f\npage three text\n"))
           (vm-visit-folder file)
           (let ((m (car vm-message-pointer)))
             (vm-set-new-flag m nil)
             (vm-set-unread-flag m nil))
           (vm-preview-current-message)
           (vm-show-current-message)
           (with-current-buffer (or vm-presentation-buffer (current-buffer))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-page-test--goto-last-page ()
  "Show the last page of the message, as paging through it would."
  (vm-widen-page)
  (goto-char (point-max))
  (forward-page -1)
  (vm-narrow-to-page))

(ert-deftest vm-page-test-exposing-headers-keeps-the-page ()
  "REGRESSION: `t\=' on a later page left you looking at the first one.
`vm-narrow-to-page\=' narrows to the page point is in, and
`vm-expose-hidden-headers\=' sent point to the top of the message first, so
whoever pressed `t\=' on page three was thrown back to page one -- which the
reporter took for the command having failed.  Issue #513."
  (vm-page-test-with-paged-message
    (vm-page-test--goto-last-page)
    (let ((page-start (point-min))
          (page-end (point-max)))
      (should (string-match-p "page three" (buffer-substring page-start page-end)))
      (vm-expose-hidden-headers)
      (should (equal page-start (point-min)))
      (should (equal page-end (point-max))))))

(ert-deftest vm-page-test-exposing-headers-still-exposes-them ()
  "The headers are exposed, even though the page did not move."
  (vm-page-test-with-paged-message
    (vm-page-test--goto-last-page)
    (should-not vm-headers-exposed)
    (vm-expose-hidden-headers)
    (should vm-headers-exposed)
    (save-restriction
      (widen)
      (should (string-match-p "X-Hidden-Thing" (buffer-string))))))

(ert-deftest vm-page-test-exposing-headers-still-toggles ()
  "Pressing it twice puts the headers back.
The state used to be read off the narrowing -- exposed meant the visible
region began at the message rather than at its visible headers -- which a
page narrowing makes meaningless, so it is kept in `vm-headers-exposed\='."
  (vm-page-test-with-paged-message
    (vm-page-test--goto-last-page)
    (let ((page-start (point-min)))
      (vm-expose-hidden-headers)
      (should vm-headers-exposed)
      (vm-expose-hidden-headers)
      (should-not vm-headers-exposed)
      ;; and still on the same page after both
      (should (equal page-start (point-min))))))

(ert-deftest vm-page-test-exposing-headers-on-the-first-page ()
  "On page one the region grows upward to show the headers, as it always did.
Nothing is skipped past: the text that was being read is still there."
  (vm-page-test-with-paged-message
    (let ((page-end (point-max)))
      (should-not (string-match-p "X-Hidden-Thing"
                                  (buffer-substring (point-min) (point-max))))
      (vm-expose-hidden-headers)
      (should vm-headers-exposed)
      (let ((visible (buffer-substring (point-min) (point-max))))
        (should (string-match-p "X-Hidden-Thing" visible))
        (should (string-match-p "page one text" visible)))
      (should (equal page-end (point-max))))))

(ert-deftest vm-page-test-exposing-headers-without-page-delimiters ()
  "With `vm-honor-page-delimiters\=' nil the old rule still decides.
Nothing narrows to a page there, so the visible region says whether the
headers are exposed, and that is what the command reads."
  (vm-page-test-with-paged-message
    (let ((vm-honor-page-delimiters nil))
      (vm-widen-page)
      (vm-expose-hidden-headers)
      (should (string-match-p "X-Hidden-Thing"
                              (buffer-substring (point-min) (point-max))))
      (vm-expose-hidden-headers)
      (should-not (string-match-p "X-Hidden-Thing"
                                  (buffer-substring (point-min) (point-max))))
      ;; and the whole message is visible, not a page of it
      (should (string-match-p "page three text"
                              (buffer-substring (point-min) (point-max)))))))

;;; Shrunken headers (issue #606)

(defconst vm-page-test--folded-headers
  (concat "From: alice@example.com\n"
          "To: one@example.com,\n"
          "        two@example.com,\n"
          "        three@example.com\n"
          "Subject: lunch\n"
          "\n" "Body.\n")
  "A message whose To header runs onto three lines, as a folded header does.")

(defun vm-page-test--shrunken-overlays ()
  "The overlays `vm-shrunken-headers' made in the current buffer."
  (seq-filter (lambda (o) (overlay-get o 'vm-shrunken-headers))
              (overlays-in (point-min) (point-max))))

(ert-deftest vm-page-test-shrunken-headers-folds-a-continued-header ()
  "A header running onto more lines is hidden behind an overlay; a short one is not.
The `To' header here has two continuation lines and `Subject' has none."
  (with-temp-buffer
    (insert vm-page-test--folded-headers)
    (vm-shrunken-headers)
    (let ((hidden (vm-page-test--shrunken-overlays)))
      (should (= 1 (length hidden)))
      (should (overlay-get (car hidden) 'invisible))
      ;; what is hidden is the continuation, not the header's first line
      (let ((text (buffer-substring-no-properties
                   (overlay-start (car hidden)) (overlay-end (car hidden)))))
        (should (string-match-p "two@example.com" text))
        (should-not (string-match-p "^To:" text))
        (should-not (string-match-p "Subject:" text))))))

(ert-deftest vm-page-test-shrunken-headers-toggle-shows-them-again ()
  "Toggling flips the overlay rather than making another one."
  (with-temp-buffer
    (insert vm-page-test--folded-headers)
    (vm-shrunken-headers)
    (should (overlay-get (car (vm-page-test--shrunken-overlays)) 'invisible))
    (vm-shrunken-headers 'toggle)
    (should (= 1 (length (vm-page-test--shrunken-overlays))))
    (should-not (overlay-get (car (vm-page-test--shrunken-overlays)) 'invisible))
    (vm-shrunken-headers 'toggle)
    (should (overlay-get (car (vm-page-test--shrunken-overlays)) 'invisible))))

(ert-deftest vm-page-test-shrunken-headers-stops-at-the-body ()
  "Only the header section is folded.
An indented line in the body is a quotation or a code sample, not a folded
header, and hiding it would hide the message."
  (with-temp-buffer
    (insert "From: alice@example.com\n"
            "Subject: lunch\n"
            "\n"
            "    indented body line\n"
            "    another one\n")
    (vm-shrunken-headers)
    (should (null (vm-page-test--shrunken-overlays)))))

(ert-deftest vm-page-test-shrunken-headers-leaves-the-buffer-unmodified ()
  "Folding is a display change: it must not mark the folder buffer modified.
The overlays would otherwise make VM think the folder needs saving."
  (with-temp-buffer
    (insert vm-page-test--folded-headers)
    (set-buffer-modified-p nil)
    (vm-shrunken-headers)
    (should-not (buffer-modified-p))))

(ert-deftest vm-page-test-shrunken-headers-is-off-by-default ()
  "`vm-enable-shrunken-headers' replaces the vm-enable-addons flag it had."
  (should-not (default-value 'vm-enable-shrunken-headers))
  (should (get 'vm-enable-shrunken-headers 'standard-value))
  ;; the addon list it was a flag in is gone entirely
  (should-not (boundp 'vm-enable-addons)))

(provide 'vm-page-test)

;;; vm-page-test.el ends here
