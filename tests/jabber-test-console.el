;;; jabber-test-console.el --- Console retention tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real console EWOCs and shared chat truncation without a connection.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-console)
(require 'jabber-chatbuffer)

(defmacro jabber-test-console--with-buffer (&rest body)
  "Run BODY in a disposable console with no file logging."
  (declare (indent 0) (debug t))
  `(let ((jabber-console-name-format " *jabber-test-console-%s*")
         (jabber-debug-log-xml nil)
         buffer)
     (unwind-protect
         (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                    (lambda (_) "console@example.invalid")))
           (setq buffer (jabber-console-create-buffer 'test-connection))
           (with-current-buffer buffer ,@body))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun jabber-test-console--append (count)
  "Append COUNT real console entries."
  (dotimes (i count)
    (jabber-process-console 'test-connection "recv"
                            (format "<presence id='%d'/>\n" i))))

(ert-deftest jabber-test-console-retention-option ()
  "Honor the console cap independently of the chat cap, including one."
  (dolist (cap '(1 8))
    (let ((jabber-console-truncate-lines cap)
          (jabber-log-lines-to-keep 1000))
      (jabber-test-console--with-buffer
        (jabber-test-console--append 20)
        (should (< (length (ewoc-collect jabber-console-ewoc #'identity)) 5))
        (should (string-match-p "id='19'" (buffer-string)))
        (should-not (string-match-p "id='0'" (buffer-string)))
        (should (= jabber-log-lines-to-keep 1000))))))

(ert-deftest jabber-test-console-zero-retains-history ()
  "Zero disables truncation even when the chat cap is small."
  (let ((jabber-console-truncate-lines 0)
        (jabber-log-lines-to-keep 1))
    (jabber-test-console--with-buffer
      (jabber-test-console--append 20)
      (should (= (length (ewoc-collect jabber-console-ewoc #'identity)) 20)))))

(ert-deftest jabber-test-console-append-preserves-draft-positions ()
  "Append and real deletion preserve hidden and visible draft positions."
  (dolist (cap '(0 8 3000))
    (dolist (visibility '(hidden selected unselected))
      (let ((jabber-console-truncate-lines cap))
        (jabber-test-console--with-buffer
          (save-window-excursion
            (delete-other-windows)
            (jabber-test-console--append 10)
            (goto-char (point-max))
            (insert "<message>draft</message>")
            (backward-char 5)
            (let* ((offset (- (point) jabber-point-insert))
                   (draft (jabber-chat--input-string))
                   (window (unless (eq visibility 'hidden)
                             (set-window-buffer (selected-window) buffer)
                             (selected-window)))
                   (other (when window (split-window-below))))
              (when other
                (set-window-buffer other buffer)
                (set-window-point other (+ jabber-point-insert 2))
                (when (eq visibility 'unselected)
                  (select-window other)
                  (switch-to-buffer (get-buffer-create " *console-other*"))))
              (with-current-buffer buffer
                (goto-char (+ jabber-point-insert offset))
                (jabber-test-console--append 20)
                (should (equal (jabber-chat--input-string) draft))
                (should (= (- (point) jabber-point-insert) offset))
                (when window
                  (should (= (- (window-point window) jabber-point-insert)
                             offset)))
              (when (and other (eq (window-buffer other) buffer))
                (should (= (- (window-point other) jabber-point-insert) 2))))))))))
  (when-let* ((buffer (get-buffer " *console-other*"))) (kill-buffer buffer)))

(ert-deftest jabber-test-console-truncate-preserves-chat-undo ()
  "Shared truncation leaves chat draft undo aligned after deleting history."
  (with-temp-buffer
    (jabber-chat-mode)
    (let ((jabber-chat-encryption 'plaintext)
          (jabber-log-lines-to-keep 4))
      (jabber-chat-mode-setup 'test-connection
                              (lambda (data) (insert (cadr data) "\n")))
      (dotimes (i 20)
        (jabber-chat-ewoc-enter (list :notice (format "history %d" i))))
      (goto-char (point-max))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (insert "draft")
      (undo-boundary)
      (let ((before jabber-point-insert)
            (position (marker-position jabber-point-insert)))
        (jabber-truncate-top (current-buffer))
        (should (< jabber-point-insert position))
        (should (eq before jabber-point-insert))
        (should (equal (jabber-chat--input-string) "draft"))
        (undo 1)
        (should (equal (jabber-chat--input-string) ""))
        (should (string-match-p "history 19" (buffer-string)))))))

(ert-deftest jabber-test-console-native-typing-and-undo ()
  "Keyboard edits and undo stay in the draft across append and truncation."
  (let ((jabber-console-truncate-lines 8))
    (jabber-test-console--with-buffer
      (save-window-excursion
        (switch-to-buffer buffer)
        (jabber-test-console--append 20)
        (goto-char (point-max))
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (should (eq (key-binding "a") 'self-insert-command))
        (should (eq (key-binding (kbd "C-c C-i")) 'jabber-info-menu))
        (should (eq (key-binding (kbd "RET")) 'jabber-chat-buffer-send))
        (save-excursion
          (goto-char (ewoc-location (ewoc-nth jabber-console-ewoc 0)))
          (should-error (insert "X") :type 'text-read-only))
        (execute-kbd-macro "draft")
        (undo-boundary)
        (execute-kbd-macro (kbd "C-b C-b"))
        (jabber-test-console--append 20)
        (execute-kbd-macro "X")
        (should (equal (jabber-chat--input-string) "draXft"))
        (undo-boundary)
        (execute-kbd-macro (kbd "C-/"))
        (should (equal (jabber-chat--input-string) "draft"))
        (should (string-match-p "id='19'" (buffer-string)))))))

(ert-deftest jabber-test-console-retained-reader-window ()
  "Truncation preserves another window's surviving history and viewport."
  (let ((jabber-console-truncate-lines 0))
    (jabber-test-console--with-buffer
      (save-window-excursion
        (switch-to-buffer buffer)
        (delete-other-windows)
        (jabber-test-console--append 30)
        (goto-char (point-max))
        (insert "draft")
        (let* ((other (split-window-below))
               (node (ewoc-nth jabber-console-ewoc -2))
               (anchor (copy-marker (ewoc-location node)))
               (jabber-console-truncate-lines 20))
          (set-window-buffer other buffer)
          (set-window-point other anchor)
          (set-window-start other anchor t)
          (jabber-test-console--append 1)
          (should (= (window-point other) anchor))
          (should (= (window-start other) anchor))
          (should (string-match-p "id='28'"
                                  (buffer-substring anchor jabber-point-insert)))
          (should-not (string-match-p "id='1'" (buffer-string)))
          (set-marker anchor nil))))))

(ert-deftest jabber-test-console-transcript-separators-read-only ()
  "Protect every transcript character, including native EWOC separators."
  (dolist (xml '("<presence/>" "<presence/>\n"
                 (message ((id . "xml")) (body nil "body"))))
    (let ((jabber-console-truncate-lines 0))
      (jabber-test-console--with-buffer
        (save-window-excursion
          (switch-to-buffer buffer)
          (goto-char jabber-point-insert)
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (execute-kbd-macro "draft")
          (undo-boundary)
          (dolist (truncate '(nil t))
            (let ((jabber-console-truncate-lines (if truncate 8 0)))
              (dotimes (_ 10)
                (jabber-process-console 'test-connection "recv" xml)))
            (let ((count (length (ewoc-collect jabber-console-ewoc #'identity))))
              (if truncate (should (< count 10)) (should (= count 10))))
            (let ((before (buffer-string))
                  (boundary (marker-position jabber-point-insert))
                  (separator (save-excursion
                               (goto-char jabber-point-insert)
                               (forward-line -1)
                               (1- (point)))))
              (should (eq (char-after separator) ?\n))
              (should-not inhibit-read-only)
              ;; This newline used to be inserted after the printer's guard.
              (goto-char separator)
              (should-error (execute-kbd-macro "X") :type 'text-read-only)
              (goto-char separator)
              (should-error (execute-kbd-macro (kbd "C-d"))
                            :type 'text-read-only)
              (goto-char (1+ separator))
              (should-error (execute-kbd-macro (kbd "DEL"))
                            :type 'text-read-only)
              (cl-loop for position from (point-min) below boundary do
                       (should (get-text-property position 'read-only))
                       (goto-char position)
                       (should-error (insert "X") :type 'text-read-only)
                       (should-error (delete-char 1) :type 'text-read-only))
              (should (equal (buffer-string) before))
              (should (= jabber-point-insert boundary))
              (should (equal (jabber-chat--input-string) "draft"))
              ;; The first draft position must remain writable and undoable.
              (goto-char jabber-point-insert)
              (execute-kbd-macro "X")
              (should (equal (jabber-chat--input-string) "Xdraft"))
              (undo-boundary)
              (execute-kbd-macro (kbd "C-/"))
              (should (equal (buffer-string) before))
              (should (= (point) jabber-point-insert)))))))))

(provide 'jabber-test-console)
;;; jabber-test-console.el ends here
