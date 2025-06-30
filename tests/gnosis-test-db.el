;;; gnosis-test-db.el --- Gnosis testing module  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; Development module for gnosis, creates a testing database env.

;;; Code:

(require 'gnosis)

(defvar gnosis-test-image "anatomy/typic-vertebra-superior-01.png"
  "Random image for testing")

(defun gnosis-test-random-items (list x)
  "Select X random items from LIST."
  (let ((shuffled-list (copy-sequence list))
        selected-items)
    (dotimes (_ x)
      (let* ((index (random (length shuffled-list)))
             (item (nth index shuffled-list)))
        (setq selected-items (cons item selected-items))
        (setq shuffled-list (append (butlast shuffled-list index)
				    (nthcdr (1+ index) shuffled-list)))))
    selected-items))

(defun gnosis-test-db ()
  "Begin/End testing env.

If ask nil, leave testing env"
  (interactive)
  (let* ((ask (y-or-n-p "Start testing database (n for exit)?"))
	 (testing-dir (expand-file-name "testing" gnosis-dir))
	 (testing-db (expand-file-name "testing.db" testing-dir)))
    (if ask
	(progn
	  (unless (file-exists-p testing-dir)
	    (make-directory testing-dir))
	  (setf gnosis-db (emacsql-sqlite-open testing-db))
	  (dolist (table '(decks notes review review-log extras activity-log tags links))
	    (condition-case nil
		(gnosis--drop-table table)
	      (error (message "No %s table to drop." table))))
	  (setf gnosis-testing t)
	  (gnosis-db-init)
	  (message "Development env is ready for testing."))
      (setf gnosis-db (emacsql-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
      (setf gnosis-testing nil)
      (message "Exited development env."))))


(provide 'gnosis-test-db)
;;; gnosis-test-db.el ends here
