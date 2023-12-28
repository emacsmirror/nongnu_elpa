
;; Random functions to make development/testing of gnosis.el easier

;;; Code:

(require 'gnosis)

(defvar gnosis-dev-tags '("anatomy" "thoracic" "serratus-anterior"
			  "biochemistry" "informatics" "amino-acids"
			  "microbiology" "gram-positive" "gram-negative"
			  "fungi" "parasites"))

(defun gnosis-dev-random-items (list x)
  "Select X random items from LIST."
  (let ((shuffled-list (copy-sequence list))
        selected-items)
    (dotimes (_ x)
      (let* ((index (random (length shuffled-list)))
             (item (nth index shuffled-list)))
        (setq selected-items (cons item selected-items))
        (setq shuffled-list (append (butlast shuffled-list index) (nthcdr (1+ index) shuffled-list)))))
    selected-items))
(defun gnosis-dev-test ()
  "Start testing env."
  (interactive)
  (let ((ask (y-or-n-p "Start development env?")))
    (if ask
	(progn (setf gnosis-testing-dir (concat gnosis-dir "/testing"))
	       (unless (file-exists-p gnosis-testing-dir)
		 (make-directory gnosis-testing-dir))
	       (setf gnosis-db (emacsql-sqlite-open (concat gnosis-testing-dir "/testing.db")))
	       (gnosis-init))
      (setf gnosis-db (emacsql-sqlite-open (concat gnosis-dir "/" "gnosis.db"))))))

(provide 'gnosis-dev)
;;; gnosis-dev.el ends here
