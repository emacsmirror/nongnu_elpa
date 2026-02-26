;;; gnosis-review.el --- Review system for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

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

;; Review system for gnosis spaced repetition.
;;
;; This module handles:
;; - Scheduling: due/overdue thema detection (`gnosis-review-is-due-p',
;;   `gnosis-review-get-due-themata', `gnosis-review-get-overdue-themata')
;; - Review display: keimenon, images, clozes, answers, hints, parathema
;; - Type-specific review logic: MCQ, basic, cloze, MC-cloze
;; - Review session management and actions (next, override, suspend,
;;   edit, quit, view-link)
;; - Algorithm bridge: computing next intervals and gnosis scores
;; - Monkeytype integration for typing practice
;; - Link view mode for viewing org-gnosis nodes during review

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'org-gnosis)

;;; Review vars

(defvar gnosis-review-types '("Due themata"
			      "Due themata of deck"
			      "Due themata of specified tag(s)"
			      "Overdue themata"
			      "Due themata (Without Overdue)"
			      "All themata of deck"
			      "All themata of tag(s)"))

(defvar gnosis-review-themata nil
  "Review themata.")

(defvar gnosis-review-buffer-name "*gnosis*"
  "Review buffer name.")

;;; Display functions

(defun gnosis-display-keimenon (str)
  "Display STR as keimenon."
  (with-current-buffer gnosis-review-buffer-name
    (erase-buffer)
    (insert "\n" (gnosis-format-string str))
    (gnosis-insert-separator)
    (gnosis-apply-center-buffer-overlay)))

(defun gnosis-display-image (keimenon)
  "Display image link from KEIMENON in new window."
  (let ((image-path (and (string-match "\\[file:\\(.*?\\)\\]" keimenon)
			 (match-string 1 keimenon))))
    (when image-path
      (find-file-other-window image-path)
      (switch-to-buffer-other-window gnosis-review-buffer-name))))

(defun gnosis-display-cloze-string (str clozes hints correct false)
  "Display STR with CLOZES and HINTS.

Applies highlighting for CORRECT & FALSE."
  (let* ((cloze-str (gnosis-cloze-create str clozes))
	 (str-with-hints (gnosis-cloze-add-hints cloze-str hints))
	 (str-with-c-answers
	  (gnosis-utils-highlight-words str-with-hints correct 'gnosis-face-correct))
	 (final (gnosis-cloze-mark-false str-with-c-answers false)))
    (gnosis-display-keimenon final)))

(defun gnosis-display-basic-answer (answer success user-input)
  "Display ANSWER.

When SUCCESS nil, display USER-INPUT as well"
  (with-current-buffer gnosis-review-buffer-name
      (goto-char (point-max))
  (insert "\n\n"
	  (propertize "Answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize answer 'face 'gnosis-face-correct))
  (when gnosis-center-content
    (gnosis-center-current-line))
  ;; Insert user wrong answer
  (when (not success)
    (insert "\n"
	    (propertize "Your answer:" 'face 'gnosis-face-directions)
	    " "
	    (propertize user-input 'face 'gnosis-face-false))
    (when gnosis-center-content
      (gnosis-center-current-line)))))

(defun gnosis-display-hint (hint)
  "Display HINT."
  (let ((hint (or hint "")))
    (unless (string-empty-p hint)
      (goto-char (point-max))
      (and (not (string-empty-p hint))
	   (insert "\n" (gnosis-format-string (propertize hint 'face 'gnosis-face-hint))))
      (gnosis-insert-separator))))

(defun gnosis-display-cloze-user-answer (user-input &optional false)
  "Display USER-INPUT answer for cloze thema upon failed review.

If FALSE t, use gnosis-face-false face"
  (goto-char (point-max))
  (insert "\n\n"
	  (propertize "Your answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize user-input 'face
		      (if false 'gnosis-face-false 'gnosis-face-correct)))
  (when gnosis-center-content
    (gnosis-center-current-line))
  (newline))

(defun gnosis-display-correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ thema."
  (goto-char (point-max))
  (insert (gnosis-format-string
	   (format "%s %s\n%s %s"
		   (propertize "Correct Answer:" 'face 'gnosis-face-directions)
		   (propertize answer 'face 'gnosis-face-correct)
		   (propertize "Your answer:" 'face 'gnosis-face-directions)
		   (propertize user-choice 'face (if (string= answer user-choice)
						     'gnosis-face-correct
						   'gnosis-face-false))))
	  "\n")
  (gnosis-insert-separator))

(defun gnosis-display-parathema (parathema)
  "Display PARATHEMA."
  (when (and parathema (not (string-empty-p parathema)))
    (search-backward "----") ; search back for separator
    (forward-line 1)
    (insert "\n" (gnosis-format-string (gnosis-org-format-string parathema)) "\n")))

(defun gnosis-display-next-review (id success)
  "Display next interval of thema ID for SUCCESS."
  (with-current-buffer gnosis-review-buffer-name
    (let* ((interval (car (gnosis-review-algorithm id success)))
	   (next-review-msg (format "\n\n%s %s"
				    (propertize "Next review:" 'face 'gnosis-face-directions)
				    (propertize
				     (replace-regexp-in-string
				      "[]()[:space:]]"
				      (lambda (match)
					(if (string= match " ") "/" ""))
				      (format "%s" interval) t t)
				     'face 'gnosis-face-next-review))))
      (if (search-backward "Next review" nil t)
	  ;; Delete previous result, and override with new this should
	  ;; occur only when used for overriding review result.
          (progn (delete-region (point) (progn (end-of-line) (point)))
		 (insert (propertize (replace-regexp-in-string "\n" "" next-review-msg)
				     'face (if success 'gnosis-face-correct
					     'gnosis-face-false))))
	;; Default behaviour
	(goto-char (point-max))
	(insert (gnosis-format-string next-review-msg))))))

;;; Link view mode

(defun gnosis-get-linked-nodes (id)
  "Return the title of linked org-gnosis node(s) for thema ID."
  (let* ((links (gnosis-select 'dest 'links `(= source ,id) t))
	 (org-gnosis-nodes (cl-loop for node-id in links
				    collect (org-gnosis-select 'title 'nodes `(= id ,node-id) t))))
    (and links (apply #'append org-gnosis-nodes))))

(defun gnosis-view-linked-node (id)
  "Visit linked node(s) for thema ID."
  (let* ((node (gnosis-completing-read "Select node: " (gnosis-get-linked-nodes id) t)))
    (window-configuration-to-register :gnosis-link-view)
    (org-gnosis-find node)
    (gnosis-link-view-mode)))

(defun gnosis-link-view--exit ()
  "Exit link view mode."
  (interactive nil gnosis-link-view-mode)
  (gnosis-link-view-mode -1)
  (jump-to-register :gnosis-link-view)
  (exit-recursive-edit))

(defvar-keymap gnosis-link-view-mode-map
  :doc "Keymap for `gnosis-link-view-mode'."
  "C-c C-c" #'gnosis-link-view--exit)

(define-minor-mode gnosis-link-view-mode "Gnosis Link View."
  :interactive nil
  :lighter " Gnosis Link View"
  :keymap gnosis-link-view-mode-map
  (if gnosis-link-view-mode
      (setq-local header-line-format
		  (substitute-command-keys
		   " Return to review with: \\[gnosis-link-view--exit]"))
    (setq-local header-line-format nil)))

;;; Due/scheduling

(defun gnosis-review-is-due-p (thema-id)
  "Check if thema with value of THEMA-ID for id is due for review.

Check if it's suspended, and if it's due today."
  (and (not (gnosis-suspended-p thema-id))
       (gnosis-review-is-due-today-p thema-id)))

(defun gnosis-review-is-due-today-p (id)
  "Return t if thema with ID is due today.

This function ignores if thema is suspended.  Refer to
`gnosis-review-is-due-p' if you need to check for suspended value as
well."
  (let ((next-rev (gnosis-get 'next-rev 'review-log `(= id ,id))))
    (gnosis-past-or-present-p next-rev)))

(defun gnosis-review-get--due-themata ()
  "Return due thema IDs & due dates."
  (let* ((today (gnosis--date-to-int (gnosis-algorithm-date)))
	 (old-themata (cl-loop for thema in
			       (gnosis-select '[id next-rev] 'review-log
					      '(and (> n 0)
						    (= suspend 0))
					      nil)
			       when (<= (gnosis--date-to-int (cadr thema)) today)
			       collect thema))
	 (new-themata (cl-loop for thema in
			       (gnosis-select '[id next-rev] 'review-log
					      '(and (= n 0)
						    (= suspend 0))
					      nil)
			       when (<= (gnosis--date-to-int (cadr thema)) today)
			       collect thema)))
    (if gnosis-review-new-first
	(append (cl-subseq new-themata 0 gnosis-new-themata-limit) old-themata)
      (append old-themata (cl-subseq new-themata 0 gnosis-new-themata-limit)))))

(defun gnosis-review-get-due-themata ()
  "Return all due thema IDs."
  (mapcar #'car (gnosis-review-get--due-themata)))

(defun gnosis-review-get-overdue-themata (&optional thema-ids)
  "Return overdue themata for current DATE.

Optionally, provide THEMA-IDS of which the overdue ones will be returned."
  (cl-loop for thema in (or thema-ids (gnosis-review-get--due-themata))
	   when (not (equal (cadr thema) (gnosis-algorithm-date)))
	   collect (car thema)))

;;; Algorithm bridge

(defun gnosis-review-last-interval (id)
  "Return last review interval for thema ID."
  (let* ((last-rev (gnosis-get 'last-rev 'review-log `(= id ,id)))
	 (rev-date (gnosis-get 'next-rev 'review-log `(= id ,id))))
    (gnosis-algorithm-date-diff last-rev rev-date)))

(defun gnosis-review-algorithm (id success)
  "Return next review date & gnosis for thema with value of id ID.

SUCCESS is a boolean value, t for success, nil for failure.

Returns a list of the form ((yyyy mm dd) (ef-increase ef-decrease ef-total))."
  (let ((amnesia (gnosis-get-thema-amnesia id))
	(gnosis (gnosis-get 'gnosis 'review `(= id ,id)))
	(t-success (gnosis-get 't-success 'review-log `(= id ,id))) ;; total successful reviews
	(c-success (gnosis-get 'c-success 'review-log `(= id ,id))) ;; consecutive successful reviews
	(c-fails (gnosis-get 'c-fails 'review-log `(= id ,id))) ;; consecutive failed reviews
	;; (t-fails (gnosis-get 't-fails 'review-log `(= id ,id))) ;; total failed reviews
	;; (review-num (gnosis-get 'n 'review-log `(= id ,id))) ;; total reviews
	;; (last-interval (max (gnosis-review--get-offset id) 1))
	(last-interval (gnosis-review-last-interval id))) ;; last interval
    (list
     (gnosis-algorithm-next-interval
      :last-interval last-interval
      :gnosis-synolon (nth 2 gnosis)
      :success success
      :successful-reviews t-success
      :c-fails c-fails
      :lethe (gnosis-get-thema-lethe id)
      :amnesia amnesia
      :proto (gnosis-get-thema-proto id))
     (gnosis-algorithm-next-gnosis
      :gnosis gnosis
      :success success
      :epignosis (gnosis-get-thema-epignosis id)
      :agnoia (gnosis-get-thema-agnoia id)
      :anagnosis (gnosis-get-thema-anagnosis id)
      :c-successes c-success
      :c-failures c-fails
      :lethe (gnosis-get-thema-lethe id)))))

(defun gnosis-review--update (id success)
  "Update review-log for thema ID.

SUCCESS is a boolean value, t for success, nil for failure."
  (let* ((result (gnosis-review-algorithm id success))
	 (next-rev (car result))
	 (gnosis-score (cadr result))
	 (log (car (gnosis-select '[n c-success c-fails t-success t-fails]
				  'review-log `(= id ,id))))
	 (n (nth 0 log))
	 (c-success (nth 1 log))
	 (c-fails (nth 2 log))
	 (t-success (nth 3 log))
	 (t-fails (nth 4 log)))
    (gnosis-review-increment-activity-log (not (> n 0)))
    ;; Single review-log UPDATE
    (emacsql gnosis-db
	     "UPDATE review_log SET last_rev = $s1, next_rev = $s2, n = $s3, c_success = $s4, c_fails = $s5, t_success = $s6, t_fails = $s7 WHERE id = $s8"
	     (gnosis-algorithm-date) next-rev (1+ n)
	     (if success (1+ c-success) 0)
	     (if success 0 (1+ c-fails))
	     (if success (1+ t-success) t-success)
	     (if success t-fails (1+ t-fails))
	     id)
    ;; Single review UPDATE
    (gnosis-update 'review `(= gnosis ',gnosis-score) `(= id ,id))))

(defun gnosis-review-result (id success)
  "Update review thema ID results for SUCCESS."
  (gnosis-review--update id success)
  (setf gnosis-due-themata-total (length (gnosis-review-get-due-themata))))

;;; Type-specific review

(defun gnosis-review-mcq (id)
  "Review MCQ thema with ID."
  (gnosis-display-image (gnosis-get 'keimenon 'themata `(= id ,id)))
  (gnosis-display-keimenon (gnosis-org-format-string
			    (gnosis-get 'keimenon 'themata `(= id ,id))))
  (let* ((answer (car (gnosis-get 'answer 'themata `(= id ,id))))
	 (user-choice (gnosis-mcq-answer id))
	 (success (string= answer user-choice)))
    (gnosis-display-correct-answer-mcq answer user-choice)
    (gnosis-display-parathema (gnosis-get 'parathema 'extras `(= id ,id)))
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-basic (id)
  "Review basic type thema for ID."
  (let* ((hypothesis (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
	 (keimenon (gnosis-get 'keimenon 'themata `(= id ,id)))
	 (answer (car (gnosis-get 'answer 'themata `(= id ,id)))))
    (gnosis-display-image keimenon)
    (gnosis-display-keimenon (gnosis-org-format-string keimenon))
    (gnosis-display-hint hypothesis)
    (let* ((answer answer)
	   (user-input (read-string "Answer: "))
	   (success (gnosis-compare-strings answer user-input)))
      (gnosis-display-basic-answer answer success user-input)
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review id success)
      success)))

(defun gnosis-review-cloze--input (clozes &optional user-input)
  "Prompt for USER-INPUT during cloze review.

CLOZES is a list of possible correct answers.

Returns a cons; ='(position . user-input) if correct,
='(nil . user-input) if incorrect."
  (let* ((user-input (or user-input (read-string "Answer: ")))
         (position (cl-position user-input clozes :test #'gnosis-compare-strings)))
    (cons position user-input)))

(defun gnosis-review-cloze (id)
  "Review cloze type thema for ID."
  (let* ((keimenon (gnosis-get 'keimenon 'themata `(= id ,id)))
         (all-clozes (gnosis-get 'answer 'themata `(= id ,id)))
         (all-hints (gnosis-get 'hypothesis 'themata `(= id ,id)))
         (revealed-clozes '()) ;; List of revealed clozes
         (unrevealed-clozes all-clozes)
         (unrevealed-hints all-hints)
         (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
         (success t))
    ;; Initially display the sentence with no reveals
    (gnosis-display-cloze-string keimenon unrevealed-clozes unrevealed-hints nil nil)
    (catch 'done
      (while unrevealed-clozes
        (let* ((input (gnosis-review-cloze--input unrevealed-clozes))
               (position (car input))
               (matched-cloze (when position (nth position unrevealed-clozes)))
               (matched-hint (when (and position (< position (length unrevealed-hints)))
                               (nth position unrevealed-hints))))
          (if matched-cloze
              ;; Correct answer - move cloze from unrevealed to revealed
              (progn
                ;; Add to revealed clozes list, preserving original order
                (setq revealed-clozes
                      (cl-sort (cons matched-cloze revealed-clozes)
                               #'< :key (lambda (cloze)
                                          (cl-position cloze all-clozes))))
                ;; Remove from unrevealed lists by position
                (setq unrevealed-clozes (append (cl-subseq unrevealed-clozes 0 position)
                                               (cl-subseq unrevealed-clozes (1+ position))))
                (when (and matched-hint (< position (length unrevealed-hints)))
		  (setq unrevealed-hints (append (cl-subseq unrevealed-hints 0 position)
                                                (cl-subseq unrevealed-hints (1+ position)))))
                ;; Display with updated revealed/unrevealed lists
                (gnosis-display-cloze-string keimenon unrevealed-clozes unrevealed-hints
                                           revealed-clozes nil))
            ;; Incorrect answer
            (gnosis-display-cloze-string keimenon nil nil
                                       revealed-clozes unrevealed-clozes)
            (gnosis-display-cloze-user-answer (cdr input))
            (setq success nil)
            (throw 'done nil)))))
    (gnosis-display-parathema parathema)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-mc-cloze (id)
  "Review mc-cloze type thema for ID."
  (let* ((keimenon (gnosis-get 'keimenon 'themata `(= id ,id)))
	 (cloze (gnosis-get 'answer 'themata `(= id ,id)))
	 (options (gnosis-get 'hypothesis 'themata `(= id ,id)))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
	 (user-input)
	 (success))
    (gnosis-display-cloze-string keimenon cloze nil nil nil)
    (setq user-input (gnosis-completing-read "Select answer: "
					     (gnosis-shuffle options)))
    (if (string= user-input (car cloze))
	(progn
	  (gnosis-display-cloze-string keimenon nil nil cloze nil)
	  (setq success t))
      (gnosis-display-cloze-string keimenon nil nil nil cloze)
      (gnosis-display-correct-answer-mcq (car cloze) user-input))
    (gnosis-display-parathema parathema)
    (gnosis-display-next-review id success)
    success))

(defun gnosis-review-is-thema-new-p (id)
  "Return t if thema with ID is new."
  (let ((reviews (car (gnosis-select 'n 'review-log `(= id ,id) t))))
    (not (> reviews 0))))

;;; Activity log

(defun gnosis-review-increment-activity-log (new? &optional date)
  "Increment activity log for DATE by one.

If NEW? is non-nil, increment new themata log by 1."
  (let* ((current-total-value (gnosis-get-date-total-themata))
	 (inc-total (cl-incf current-total-value))
	 (current-new-value (gnosis-get-date-new-themata))
	 (inc-new (cl-incf current-new-value))
	 (date (or date (gnosis-algorithm-date))))
    (gnosis-update 'activity-log `(= reviewed-total ,inc-total) `(= date ',date))
    (and new? (gnosis-update 'activity-log `(= reviewed-new ,inc-new) `(= date ',date)))))

(defun gnosis-history-clear ()
  "Delete all activity log entries."
  (interactive)
  (when (y-or-n-p "Delete all activity log?")
    (emacsql gnosis-db [:delete :from activity-log])))

;;; Session management

(defun gnosis-review--display-thema (id)
  "Display thema with ID and call the appropriate review func."
  (let* ((type (gnosis-get 'type 'themata `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s" (downcase type)))))
    (if (fboundp func-name)
        (progn
	  (unless (eq major-mode 'gnosis-mode)
	    (pop-to-buffer-same-window (get-buffer-create gnosis-review-buffer-name))
            (gnosis-mode)
	    (gnosis-review-update-header 0))
	  (window-configuration-to-register :gnosis-pre-image)
          (funcall func-name id))
      (error "Malformed thema type: '%s'" type))))

(defun gnosis-review-process-thema (thema &optional thema-count)
  "Process review for THEMA and update session statistics.

Displays the thema, processes the review result, and updates the
header.  Returns the incremented THEMA-COUNT after processing.

This is a helper function for `gnosis-review-session'."
  (let ((success (gnosis-review--display-thema thema))
	(thema-count (or thema-count 0)))
    (cl-incf thema-count)
    (unless success (gnosis-monkeytype-answer thema))
    (gnosis-review-actions success thema thema-count)
    ;; Use jump-to-register after first review.
    (and (not (null (get-register :gnosis-pre-image))) (jump-to-register :gnosis-pre-image))
    (setq gnosis-review-themata (remove thema gnosis-review-themata))
    (gnosis-review-update-header thema-count (length gnosis-review-themata))
    thema-count))

(defun gnosis-review-update-header (reviewed-count &optional remaining-reviews)
  "Update the review session header with current stats.

REVIEWED-COUNT: Total number of items that have been reviewed in
current session.
REMAINING-REVIEWS: Total number of remaining items to be reviewed."
  (with-current-buffer (get-buffer-create gnosis-review-buffer-name)
    (let ((remaining-reviews (or remaining-reviews (1+ (length gnosis-review-themata)))))
      (setq-local header-line-format
                  (gnosis-center-string
		   (format "%s %s %s"
                           (propertize (number-to-string reviewed-count)
                                       'face 'font-lock-type-face)
			   (propertize "|" 'face 'font-lock-comment-face)
                           (propertize (number-to-string remaining-reviews)
				       'face 'gnosis-face-false)))))))

(defun gnosis-review-session (themata &optional due thema-count)
  "Start review session for THEMATA.
THEMATA: List of thema ids
DUE: If due is non-nil, session will loop for due themata.
THEMA-COUNT: Total themata to be commited for session."
  (let ((thema-count (or thema-count 0)))
    (if (null themata)
        (message "No themata for review.")
      (setf gnosis-review-themata themata)
      (catch 'review-loop
        (cl-loop for thema in themata
                 do (setq thema-count (gnosis-review-process-thema thema thema-count))
                 finally
                 (and due (gnosis-review-session
                           (gnosis-collect-thema-ids :due t) t thema-count))))
      (gnosis-dashboard)
      (gnosis-review-commit thema-count))))

(defun gnosis-review-commit (thema-num)
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one.  It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number THEMA-NUM."
  (let ((git (executable-find "git"))
	(default-directory gnosis-dir))
    (unless git
      (error "Git not found, please install git"))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-git-create-repo))
    (unless gnosis-testing
      (shell-command
       (format "%s add gnosis.db" git))
      (gnosis--shell-cmd-with-password
       (format "%s commit -m 'Total themata reviewed: %d'" git thema-num)))
    (sit-for 0.1)
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))
    (message "Review session finished.  %d themata reviewed." thema-num)))

;;; Review actions

(defun gnosis-review-action--edit (success thema thema-count)
  "Edit THEMA during review.

Save current contents of *gnosis-edit* buffer, if any, and start
editing THEMA with it's new contents.

After done editing, call `gnosis-review-actions' with SUCCESS THEMA
THEMA-COUNT."
  (gnosis-edit-thema thema)
  (setf gnosis-review-editing-p t)
  (recursive-edit)
  (gnosis-review-actions success thema thema-count))

(defun gnosis-review-action--quit (success thema)
  "Quit review session.

Update result for THEMA review with SUCCESS and commit session for THEMA-COUNT.

This function should be used with `gnosis-review-actions', to finish
the review session."
  (gnosis-review-result thema success)
  ;; Break the review loop of `gnosis-review-session'
  (throw 'review-loop t))

(defun gnosis-review-action--suspend (success thema thema-count)
  "Suspend/Unsuspend THEMA.

This function should be used with `gnosis-review-actions', which
should be recursively called using SUCCESS, THEMA, THEMA-COUNT."
  (gnosis-toggle-suspend-themata (list thema))
  (gnosis-review-actions success thema thema-count))

(defun gnosis-review-action--override (success thema thema-count)
  "Override current review result for SUCCESS.

This function should be used with `gnosis-review-actions', which will
be called with new SUCCESS value plus THEMA & THEMA-COUNT."
  (setf success (if success nil t))
  (gnosis-display-next-review thema success)
  (gnosis-review-actions success thema thema-count))

(defun gnosis-review-action--view-link (success thema thema-count)
  "View linked node(s) for THEMA."
  (if (gnosis-get-linked-nodes thema)
    (progn (gnosis-view-linked-node thema)
	   (recursive-edit))
    (message (format "No linked nodes for thema: %d" thema))
    (sleep-for 0.5))
  (gnosis-review-actions success thema thema-count))

(defun gnosis-review-actions (success id thema-count)
  "Specify action during review of thema.

SUCCESS: Review result
ID: Thema ID
THEMA-COUNT: Total themata reviewed

To customize the keybindings, adjust `gnosis-review-keybindings'."
  (let* ((prompt
	  "Action: %sext, %sverride result, %suspend, %selete, %sdit thema, %siew link, %suit: ")
	 (choice (read-char-choice
		  (apply #'format prompt
			 (mapcar
			  (lambda (str) (propertize str 'face 'match))
			  '("n" "o" "s" "d" "e" "v" "q")))
		  '(?n ?o ?s ?d ?e ?v ?q))))
    (pcase choice
      (?n (gnosis-review-result id success))
      (?o (gnosis-review-action--override success id thema-count))
      (?s (gnosis-review-action--suspend success id thema-count))
      (?d (gnosis-delete-thema id))
      (?e (gnosis-review-action--edit success id thema-count))
      (?v (gnosis-review-action--view-link success id thema-count))
      (?q (gnosis-review-action--quit success id)))))

;;; Monkeytype integration

(defun gnosis-monkeytype-session (themata &rest _)
  "Start monkeytype session for THEMATA ids."
  (cl-assert (listp themata) nil "Themata must be a list of ids")
  (catch 'monkeytype-loop
    (cl-loop for thema in themata
	     do (gnosis-monkeytype-thema thema))))

(defun gnosis-monkeytype-start ()
  "Gnosis Monkeytype Session"
  (interactive)
  (gnosis-review #'gnosis-monkeytype-session))

(defun gnosis-monkeytype-thema (thema)
  "Process monkeytyping for THEMA id.

This is used to type the keimenon of thema, with the answers highlighted.
To monkeytype only the wrong answers use `gnosis-monkeytype-answer'."
  (let* ((thema-context (gnosis-select '[keimenon type answer] 'themata `(= id ,thema) t))
	 (keimenon (replace-regexp-in-string
		    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" ;; remove links
		    (nth 0 thema-context)))
	 (type (nth 1 thema-context))
	 (answer (cl-loop for answer in (nth 2 thema-context)
			  collect (gnosis-utils-trim-quotes answer))))
    (cond ((string= type "basic")
	   (gnosis-monkeytype (concat keimenon "\n" (car answer)) type
			      answer))
	  (t (gnosis-monkeytype keimenon type answer)))))

(defun gnosis-monkeytype-answer (thema)
  "Monkeytype answer for THEMA id."
  (let* ((thema-context (gnosis-select '[type answer] 'themata `(= id ,thema) t))
	 (type (nth 0 thema-context))
	 (answer (cl-loop for answer in (nth 1 thema-context)
			  collect (gnosis-utils-trim-quotes answer))))
    (gnosis-monkeytype (mapconcat #'identity answer " ") type answer)))

;;; Entry points

;;;###autoload
(defun gnosis-review (&optional fn)
  "Start gnosis review session.

FN: Review function, defaults to `gnosis-review-session'"
  (interactive)
  (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata)))
  (set-register :gnosis-pre-image nil)
  (let ((review-type (gnosis-completing-read "Review: " gnosis-review-types))
	(fn (or fn #'gnosis-review-session)))
    (pcase review-type
      ("Due themata"
       (funcall fn (gnosis-collect-thema-ids :due t) t))
      ("Due themata of deck"
       (funcall fn (gnosis-collect-thema-ids :due t :deck (gnosis--get-deck-id))))
      ("Due themata of specified tag(s)"
       (funcall fn (gnosis-collect-thema-ids :due t :tags t)))
      ("Overdue themata"
       (funcall fn (gnosis-review-get-overdue-themata)))
      ("Due themata (Without Overdue)"
       (funcall fn (cl-set-difference (mapcar #'car (gnosis-review-get--due-themata))
				      (gnosis-review-get-overdue-themata))))
      ("All themata of deck"
       (funcall fn (gnosis-collect-thema-ids :deck (gnosis--get-deck-id))))
      ("All themata of tag(s)"
       (funcall fn (gnosis-collect-thema-ids :tags t))))))

(defun gnosis-review--select-topic ()
  "Prompt for topic from org-gnosis database and return it's id."
  (let* ((topic-title (gnosis-completing-read "Select topic: "
					      (org-gnosis-select 'title 'nodes)))
	 (topic-id (caar (org-gnosis-select 'id 'nodes `(= title ,topic-title)))))
    topic-id))

(defun gnosis-collect-nodes-at-depth (node-id &optional fwd-depth back-depth)
  "Collect node IDs reachable from NODE-ID within depth limits.
FWD-DEPTH is max hops for forward links (default 0).
BACK-DEPTH is max hops for backlinks (default 0).
Returns a deduplicated list including NODE-ID itself."
  (let ((fwd-depth (or fwd-depth 0))
	(back-depth (or back-depth 0))
	(max-depth (max fwd-depth back-depth))
	(visited (make-hash-table :test 'equal))
	(queue (list node-id)))
    (puthash node-id t visited)
    (dotimes (level max-depth)
      (when queue
	(let* ((qvec (vconcat queue))
	       (neighbors (append
			   (when (< level fwd-depth)
			     (org-gnosis-select 'dest 'links
						`(in source ,qvec) t))
			   (when (< level back-depth)
			     (org-gnosis-select 'source 'links
						`(in dest ,qvec) t))))
	       (next-queue nil))
	  (dolist (neighbor neighbors)
	    (unless (gethash neighbor visited)
	      (puthash neighbor t visited)
	      (push neighbor next-queue)))
	  (setq queue next-queue))))
    (hash-table-keys visited)))

;;;###autoload
(defun gnosis-review-topic (&optional node-id fwd-depth back-depth)
  "Review themata linked to topic NODE-ID.
FWD-DEPTH and BACK-DEPTH control forward/backlink traversal depth.
With prefix arg, prompt for depths."
  (interactive
   (list nil
	 (when current-prefix-arg (read-number "Forward link depth: " 1))
	 (when current-prefix-arg (read-number "Backlink depth: " 0))))
  (let* ((node-id (or node-id (gnosis-review--select-topic)))
	 (fwd-depth (or fwd-depth 0))
	 (back-depth (or back-depth 0))
	 (node-title (car (org-gnosis-select 'title 'nodes
					     `(= id ,node-id) t)))
	 (node-ids (if (or (> fwd-depth 0) (> back-depth 0))
		       (gnosis-collect-nodes-at-depth
			node-id fwd-depth back-depth)
		     (list node-id)))
	 (gnosis-questions (gnosis-select 'source 'links
					  `(in dest ,(vconcat node-ids)) t)))
    (if (and gnosis-questions
	     (y-or-n-p
	      (format "Review %s thema(s) for '%s'%s?"
		      (length gnosis-questions) node-title
		      (if (> (length node-ids) 1)
			  (format " (%d nodes, fwd:%d back:%d)"
				  (length node-ids) fwd-depth back-depth)
			""))))
	(gnosis-review-session gnosis-questions)
      (message "No thema found for %s (id:%s)" node-title node-id))))

(provide 'gnosis-review)
;;; gnosis-review.el ends here
