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

;;; Header highlighting functions

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


;;; Scrolling variables

(ert-deftest vm-page-test-scroll-variables-exist ()
  "Test that scroll-related variables exist."
  (should (boundp 'vm-auto-next-message))
  (should (boundp 'vm-honor-page-delimiters)))

;;; vm-url-help tests



;;; Exposing headers while reading a later page (#513)

(defmacro vm-page-test-with-paged-message (&rest body)
  "Show a three-page message read, and run BODY in the buffer showing it.
`vm-honor-page-delimiters' is on, and the message carries a header that
`vm-visible-headers' hides, so exposing them is observable."
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
  "REGRESSION: `t' on a later page left you looking at the first one.
`vm-narrow-to-page' narrows to the page point is in, and
`vm-expose-hidden-headers' sent point to the top of the message first, so
whoever pressed `t' on page three was thrown back to page one -- which the
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
page narrowing makes meaningless, so it is kept in `vm-headers-exposed'."
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
  "With `vm-honor-page-delimiters' nil the old rule still decides.
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

;;; What a preview shows, what the end-of-message blurb says, and what the
;;; URL help offers.  These three had a test each asserting the function was
;;; bound, which is true of any file that loads.

(defconst vm-page-test--folder
  (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
          "From: alice@example.com\nSubject: first\n\n"
          "Line one\nLine two\nLine three\n\n"
          "From bob@example.com Mon Jan  1 00:00:00 2024\n"
          "From: bob@example.com\nSubject: second\n\nOther body.\n\n")
  "Two messages, the first with three body lines to preview part of.")

(ert-deftest vm-page-test-narrow-for-preview-shows-what-was-asked-for ()
  "A preview shows the headers and `vm-preview-lines' lines of the body.
The number is the whole point of the option, and nothing checked it."
  (vm-test-with-folder vm-page-test--folder
    (setq vm-message-pointer vm-message-list)
    (let ((m (car vm-message-list)))
      (let ((vm-preview-lines 1))
        (vm-narrow-for-preview)
        (should (= (point-min) (vm-vheaders-of m)))
        (should (string-match-p "Subject: first" (buffer-string)))
        (should (string-match-p "Line one" (buffer-string)))
        (should-not (string-match-p "Line two" (buffer-string))))
      (let ((vm-preview-lines 2))
        (vm-narrow-for-preview)
        (should (string-match-p "Line two" (buffer-string)))
        (should-not (string-match-p "Line three" (buffer-string)))))))

(ert-deftest vm-page-test-narrow-for-preview-of-zero-lines-shows-headers ()
  "Zero preview lines means the headers and no body."
  (vm-test-with-folder vm-page-test--folder
    (setq vm-message-pointer vm-message-list)
    (let ((vm-preview-lines 0))
      (vm-narrow-for-preview)
      (should (string-match-p "Subject: first" (buffer-string)))
      (should-not (string-match-p "Line one" (buffer-string))))))

(ert-deftest vm-page-test-narrow-for-preview-t-shows-the-whole-message ()
  "`vm-preview-lines' t means the message rather than a preview of it."
  (vm-test-with-folder vm-page-test--folder
    (setq vm-message-pointer vm-message-list)
    (let ((m (car vm-message-list))
          (vm-preview-lines t))
      (vm-narrow-for-preview)
      (should (= (point-max) (vm-text-end-of m)))
      (should (string-match-p "Line three" (buffer-string))))))

(ert-deftest vm-page-test-narrow-for-preview-does-not-run-past-the-message ()
  "Asking for more lines than the message has shows the message, not the next.
The folder is one buffer, so the next message is a few characters away."
  (vm-test-with-folder vm-page-test--folder
    (setq vm-message-pointer vm-message-list)
    (let ((m (car vm-message-list))
          (vm-preview-lines 500))
      (vm-narrow-for-preview)
      (should (= (point-max) (vm-text-end-of m)))
      (should-not (string-match-p "second" (buffer-string))))))

(ert-deftest vm-page-test-emit-eom-blurb-says-nothing-when-not-wanted ()
  "With `vm-auto-next-message' nil the end of a message is not announced,
which is what that option's docstring promises."
  (vm-test-with-folder vm-page-test--folder
    (setq vm-message-pointer vm-message-list)
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (&rest args) (setq said args)))
                ((symbol-function 'vm-summary-sprintf) (lambda (&rest _) "x")))
        (let ((vm-auto-next-message nil))
          (vm-emit-eom-blurb)
          (should-not said))
        (let ((vm-auto-next-message t))
          (vm-emit-eom-blurb)
          (should said)
          (should (string-match-p "End of message" (nth 1 said))))))))

(ert-deftest vm-page-test-emit-eom-blurb-names-the-recipient-for-your-own-mail ()
  "In a folder of sent mail the blurb says who it went to, not who sent it.
`vm-summary-uninteresting-senders' is what tells VM the sender is you."
  (vm-test-with-folder vm-page-test--folder
    (setq vm-message-pointer vm-message-list)
    (let ((said nil))
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (&rest args) (setq said args)))
                ((symbol-function 'vm-summary-sprintf) (lambda (&rest _) "x")))
        (let ((vm-auto-next-message t)
              (vm-summary-uninteresting-senders "alice"))
          (vm-emit-eom-blurb)
          (should (string-match-p "End of message %s to" (nth 1 said))))
        (let ((vm-auto-next-message t)
              (vm-summary-uninteresting-senders "nobody-here"))
          (vm-emit-eom-blurb)
          (should (string-match-p "End of message %s from" (nth 1 said))))))))

(ert-deftest vm-page-test-url-help-names-the-browser-it-would-use ()
  "The help text on a URL says where button 2 would send it."
  (let ((vm-url-browser "/usr/bin/firefox"))
    (should (string-match-p "/usr/bin/firefox" (vm-url-help nil))))
  (let ((vm-url-browser 'vm-mouse-send-url-to-netscape))
    (should (string-match-p "Netscape" (vm-url-help nil))))
  (let ((vm-url-browser 'browse-url))
    (should (string-match-p "browse-url" (vm-url-help nil)))
    (should (string-match-p "button 2" (vm-url-help nil)))
    (should (string-match-p "button 3" (vm-url-help nil))))
  ;; customize's function type allows a lambda, which has no name to print
  (let ((vm-url-browser (lambda (url) url)))
    (should (stringp (vm-url-help nil)))))

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

;;; What energizing a message's URLs does, in place of a test that the five
;;; highlight functions were bound.

(defun vm-page-test--url-overlays ()
  "The overlays in the current buffer that mark URLs, in buffer order."
  (sort (seq-filter (lambda (o) (overlay-get o 'vm-url))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun vm-page-test--url-strings ()
  "The text of each URL overlay in the current buffer."
  (mapcar (lambda (o) (buffer-substring (overlay-start o) (overlay-end o)))
          (vm-page-test--url-overlays)))

(ert-deftest vm-page-test-energize-urls-marks-each-url ()
  "Each URL gets an overlay over exactly the URL, and nothing else does.
The overlay is what button 2 and RET act on, so its bounds are the click
target."
  (with-temp-buffer
    (insert "See http://example.com/a and mailto:someone@example.com now.\n"
            "Not a url at all.\n")
    (let ((vm-url-search-limit nil)
          (vm-highlight-url-face 'vm-highlight-url)
          (vm-url-browser 'browse-url))
      (vm-energize-urls)
      (should (equal (vm-page-test--url-strings)
                     '("http://example.com/a" "mailto:someone@example.com")))
      ;; and they are buttons: highlighted under the mouse, with a keymap
      (let ((o (car (vm-page-test--url-overlays))))
        (should (eq (overlay-get o 'mouse-face) 'highlight))
        (should (overlay-get o 'vm-button))
        (should (keymapp (overlay-get o 'local-map)))
        (should (eq (overlay-get o 'balloon-help) 'vm-url-help))))))

(ert-deftest vm-page-test-energize-urls-does-not-double-up ()
  "Energizing twice leaves one overlay per URL: the old ones are removed
first, which is what makes re-presenting a message safe."
  (with-temp-buffer
    (insert "http://example.com/a\n")
    (let ((vm-url-search-limit nil)
          (vm-highlight-url-face 'vm-highlight-url)
          (vm-url-browser 'browse-url))
      (vm-energize-urls)
      (vm-energize-urls)
      (should (= (length (vm-page-test--url-overlays)) 1)))))

(ert-deftest vm-page-test-energize-urls-can-take-the-energy-back ()
  "With a prefix argument the URLs are stripped and none are marked again."
  (with-temp-buffer
    (insert "http://example.com/a and http://example.com/b\n")
    (cl-letf (((symbol-function 'vm-inform) #'ignore))
      (let ((vm-url-search-limit nil)
            (vm-highlight-url-face 'vm-highlight-url)
            (vm-url-browser 'browse-url))
        (vm-energize-urls)
        (should (= (length (vm-page-test--url-overlays)) 2))
        (vm-energize-urls t)
        (should-not (vm-page-test--url-overlays))))))

(ert-deftest vm-page-test-energize-urls-searches-only-the-ends-of-a-big-region ()
  "`vm-url-search-limit' stops VM reading a huge message end to end: it looks
at the first and last half-limit only, so a URL in the middle is not marked.
The docstring promises this and nothing checked it."
  (with-temp-buffer
    (insert "http://example.com/first\n")
    (insert (make-string 4000 ?x) "\n")
    (insert "http://example.com/middle\n")
    (insert (make-string 4000 ?x) "\n")
    (insert "http://example.com/last\n")
    (let ((vm-highlight-url-face 'vm-highlight-url)
          (vm-url-browser 'browse-url))
      (let ((vm-url-search-limit 200))
        (vm-energize-urls)
        (should (equal (vm-page-test--url-strings)
                       '("http://example.com/first"
                         "http://example.com/last"))))
      ;; without a limit the whole message is searched
      (let ((vm-url-search-limit nil))
        (vm-energize-urls)
        (should (= (length (vm-page-test--url-overlays)) 3))))))

(ert-deftest vm-page-test-energize-urls-in-message-region-needs-a-reason ()
  "Nothing is marked when there is neither a face to show it nor a browser to
send it to, since the overlay would do nothing."
  (with-temp-buffer
    (insert "http://example.com/a\n")
    (let ((vm-url-search-limit nil)
          (vm-highlight-url-face nil)
          (vm-url-browser nil))
      (vm-energize-urls-in-message-region (point-min) (point-max))
      (should-not (vm-page-test--url-overlays)))
    (let ((vm-url-search-limit nil)
          (vm-highlight-url-face nil)
          (vm-url-browser 'browse-url))
      (vm-energize-urls-in-message-region (point-min) (point-max))
      (should (= (length (vm-page-test--url-overlays)) 1)))))

;;; Moving between the buttons in a message.  `vm-next-button' and its three
;;; relatives had one test between them, that they were bound.

(defun vm-page-test--button (start end)
  "Make the text from START to END a VM button, and return its overlay."
  (let ((o (make-overlay start end)))
    (overlay-put o 'vm-button t)
    (overlay-put o 'mouse-face 'highlight)
    o))

(ert-deftest vm-page-test-move-to-button-goes-to-the-next-one ()
  "Moving forward lands on the start of the next button, one per count."
  (with-temp-buffer
    (insert "see http://one.example/ and http://two.example/ and end")
    (let ((first (progn (goto-char (point-min))
                        (search-forward "http://one.example/")
                        (vm-page-test--button (match-beginning 0) (point))))
          (second (progn (search-forward "http://two.example/")
                         (vm-page-test--button (match-beginning 0) (point)))))
      (goto-char (point-min))
      (vm-move-to-xxxx-button 1 t)
      (should (= (point) (overlay-start first)))
      (goto-char (point-min))
      (vm-move-to-xxxx-button 2 t)
      (should (= (point) (overlay-start second))))))

(ert-deftest vm-page-test-move-to-button-goes-back-as-well ()
  "Moving backward lands on the start of the previous button."
  (with-temp-buffer
    (insert "see http://one.example/ and http://two.example/ and end")
    (let ((first (progn (goto-char (point-min))
                        (search-forward "http://one.example/")
                        (vm-page-test--button (match-beginning 0) (point)))))
      (goto-char (point-min))
      (search-forward "http://two.example/")
      (vm-page-test--button (match-beginning 0) (point))
      (goto-char (point-max))
      (vm-move-to-xxxx-button 2 nil)
      (should (= (point) (overlay-start first))))))

(ert-deftest vm-page-test-move-to-button-leaves-point-alone-when-there-is-none ()
  "With no button to go to, point does not move and the error says so.
The docstrings promise both, and a command that moved point and then failed
would lose the reader's place."
  (with-temp-buffer
    (insert "no buttons here at all\n")
    (goto-char 5)
    (let ((text-quoting-style 'grave))
      (should (equal (cadr (should-error (vm-move-to-xxxx-button 1 t)))
                     "No more buttons"))
      (should (= (point) 5)))))

(ert-deftest vm-page-test-move-to-button-ignores-what-is-not-a-button ()
  "An overlay without `vm-button' is not one: font-lock's overlays, and the
mouse-face VM puts on a URL it will not act on, are both passed over."
  (with-temp-buffer
    (insert "see http://one.example/ and http://two.example/ end")
    (goto-char (point-min))
    (search-forward "http://one.example/")
    (let ((decoration (make-overlay (match-beginning 0) (point))))
      (overlay-put decoration 'mouse-face 'highlight))
    (search-forward "http://two.example/")
    (let ((button (vm-page-test--button (match-beginning 0) (point))))
      (goto-char (point-min))
      (vm-move-to-xxxx-button 1 t)
      (should (= (point) (overlay-start button))))))

;;; Moving between the buttons of a message (emacs-vm/vm#632)
;;
;; `vm-next-button' and `vm-previous-button' step between the MIME buttons of
;; the message on show.  Neither had a test.  They select the window the
;; message is in, so these display the presentation buffer in the window batch
;; Emacs has: without that `vm-get-visible-buffer-window' finds nothing and
;; `select-window' is handed nil.

(defconst vm-page-test--two-button-folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nSubject: two attachments\n"
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"bnd\"\n\n"
          "--bnd\nContent-Type: text/plain\n\nCovering text.\n\n"
          "--bnd\nContent-Type: application/octet-stream; name=\"first.bin\"\n"
          "Content-Disposition: attachment; filename=\"first.bin\"\n\n"
          "The first attachment.\n\n"
          "--bnd\nContent-Type: application/octet-stream; name=\"second.bin\"\n"
          "Content-Disposition: attachment; filename=\"second.bin\"\n\n"
          "The second attachment.\n\n"
          "--bnd--\n\n")
  "A message with two parts VM will not display inline, so two buttons.")

(defmacro vm-page-test--with-buttons (&rest body)
  "Show a message with two buttons and run BODY in its presentation buffer."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-buttons" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-auto-decode-mime-messages t)
               (vm-display-using-mime t)
               (vm-preview-lines nil)
               (vm-honor-page-delimiters nil))
           (write-region vm-page-test--two-button-folder nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             (vm-show-current-message)
             (should vm-presentation-buffer)
             (set-window-buffer (selected-window) vm-presentation-buffer)
             (set-buffer vm-presentation-buffer)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-page-test--on-a-button-p ()
  "Whether point is on a MIME button."
  (and (vm-extent-at (point) 'vm-mime-layout) t))

(ert-deftest vm-page-test-next-button-steps-through-the-buttons ()
  "`vm-next-button' moves to each button in turn, and says when there are no
more rather than moving somewhere unhelpful."
  (vm-page-test--with-buttons
    (goto-char (point-min))
    (should-not (vm-page-test--on-a-button-p))
    (vm-next-button 1)
    (should (vm-page-test--on-a-button-p))
    (let ((first (point)))
      (vm-next-button 1)
      (should (vm-page-test--on-a-button-p))
      (should (> (point) first))
      (let ((second (point))
            (text-quoting-style 'grave))
        ;; there is no third
        (should (equal (cadr (should-error (vm-next-button 1)))
                       "No more buttons"))
        ;; and point stayed where it was, as the docstring promises
        (should (equal (point) second))))))

(ert-deftest vm-page-test-previous-button-goes-back ()
  "`vm-previous-button' walks the other way, and stops at the first button."
  (vm-page-test--with-buttons
    (goto-char (point-min))
    (vm-next-button 1)
    (let ((first (point)))
      (vm-next-button 1)
      (should (> (point) first))
      (vm-previous-button 1)
      (should (equal (point) first))
      (let ((text-quoting-style 'grave))
        (should (equal (cadr (should-error (vm-previous-button 1)))
                       "No more buttons"))
        (should (equal (point) first))))))

(ert-deftest vm-page-test-a-negative-count-reverses-the-direction ()
  "A negative count sends each command the other way, which is what its
docstring says: negative N to `vm-next-button' moves to the Nth previous."
  (vm-page-test--with-buttons
    (goto-char (point-min))
    (vm-next-button 1)
    (let ((first (point)))
      (vm-next-button 1)
      (let ((second (point)))
        (vm-next-button -1)
        (should (equal (point) first))
        (vm-previous-button -1)
        (should (equal (point) second))))))

;;; Moving about in the message being read

(ert-deftest vm-page-test-beginning-of-message-goes-to-the-top ()
  "`vm-beginning-of-message' puts point at the start of the message and, with
page delimiters honoured, shows the first page again -- which is what makes it
a way back from the bottom of a long message."
  (vm-page-test-with-paged-message
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (vm-page-test--goto-last-page)
      (should (string-match-p "page three" (buffer-string)))
      (vm-beginning-of-message)
      (should (equal (point) (point-min)))
      (should (string-match-p "page one text" (buffer-string)))
      (should-not (string-match-p "page three" (buffer-string))))))

(ert-deftest vm-page-test-end-of-message-goes-to-the-bottom ()
  "`vm-end-of-message' shows the message if it was only being previewed, puts
point at its end, and leaves the last page showing."
  (vm-page-test-with-paged-message
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (setq vm-system-state 'previewing)
      (vm-end-of-message)
      (should (eq vm-system-state 'reading))
      (should (equal (point) (point-max)))
      (should (string-match-p "page three text" (buffer-string)))
      (should-not (string-match-p "page one" (buffer-string))))))


;;; Colouring quoted text and the signature (emacs-vm/vm#811)

(defconst vm-page-test--quoted-body
  (concat "Some text.\n"
          "> once quoted\n"
          ">> twice quoted\n"
          "MD> initials then quoted\n"
          "> > > > > > deeply quoted\n"
          "plain again\n"
          "-- \nthe signature\nsecond line of it\n")
  "A body with every level of quoting these tests care about, and a signature.")

(defun vm-page-test--faces-put-on (text)
  "Colour TEXT as a message body, and answer (FACE . FIRST-LINE) for each.
Only the overlays VM marks as its own, since that is what it takes off again
when the next message is shown."
  (with-temp-buffer
    (insert text)
    (vm-fontify-citations (point-min) (point-max))
    (vm-fontify-signature (point-min) (point-max))
    (let (found)
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (overlay-get overlay 'vm-highlight)
          (push (cons (overlay-get overlay 'face)
                      (buffer-substring-no-properties
                       (overlay-start overlay)
                       (save-excursion (goto-char (overlay-start overlay))
                                       (line-end-position))))
                found)))
      (sort found (lambda (a b) (string< (cdr a) (cdr b)))))))

(defun vm-page-test--face-on (text line)
  "The face put on the line of TEXT beginning with LINE, or nil."
  (car (seq-find (lambda (cell) (string-prefix-p line (cdr cell)))
                 (vm-page-test--faces-put-on text))))

(ert-deftest vm-page-test-quoted-text-wears-a-face-per-level ()
  "Each level of quoting gets its own face, and deeper wears the last one.

emacs-vm/vm#811.  This came from the u-vm-color add-on, which was dropped;
the citation and signature colouring is VM's own now.  `vm-citation-faces'
holds five by default, so text quoted six deep wears the fifth."
  (let ((text vm-page-test--quoted-body))
    (should (equal 'vm-citation-1 (vm-page-test--face-on text "> once")))
    (should (equal 'vm-citation-2 (vm-page-test--face-on text ">> twice")))
    (should (equal 'vm-citation-5 (vm-page-test--face-on text "> > > > > >")))
    ;; the initials some readers put before the angle bracket are part of the
    ;; prefix, not text quoted a level deeper
    (should (equal 'vm-citation-1 (vm-page-test--face-on text "MD> initials")))
    ;; and unquoted text is left alone
    (should (equal nil (vm-page-test--face-on text "Some text.")))
    (should (equal nil (vm-page-test--face-on text "plain again")))))

(ert-deftest vm-page-test-the-signature-wears-its-own-face ()
  "Everything after the last \"-- \" line is the signature.
That is the separator RFC 3676 describes and what mail readers write."
  (should (equal 'vm-signature
                 (vm-page-test--face-on vm-page-test--quoted-body "-- ")))
  ;; a body with no separator has no signature to colour
  (should (equal nil (vm-page-test--faces-put-on "just a body\nand more\n"))))

(ert-deftest vm-page-test-the-citation-faces-are-configurable ()
  "`vm-citation-faces' decides how many levels are told apart.
One face colours every level alike; none turns citation colouring off and
leaves the signature alone, which is what the docstring promises."
  (let ((vm-citation-faces '(vm-citation-1)))
    (should (equal 'vm-citation-1
                   (vm-page-test--face-on vm-page-test--quoted-body ">> twice"))))
  (let ((vm-citation-faces nil))
    (should (equal nil (vm-page-test--face-on vm-page-test--quoted-body "> once")))
    (should (equal 'vm-signature
                   (vm-page-test--face-on vm-page-test--quoted-body "-- ")))))

(ert-deftest vm-page-test-body-faces-are-off-unless-asked-for ()
  "`vm-fontify-body-maybe' does nothing with `vm-enable-body-faces' nil.
Off by default because it changes how every message looks."
  (should-not (default-value 'vm-enable-body-faces))
  (with-temp-buffer
    (insert vm-page-test--quoted-body)
    (let ((vm-enable-body-faces nil)
          (vm-message-pointer nil))
      (vm-fontify-body-maybe))
    (should (equal nil (seq-filter (lambda (o) (overlay-get o 'vm-highlight))
                                   (overlays-in (point-min) (point-max)))))))

(provide 'vm-page-test)

;;; vm-page-test.el ends here
