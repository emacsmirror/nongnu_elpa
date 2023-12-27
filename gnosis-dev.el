
;; Random functions to make development/testing of gnosis.el easier

;;; Code:

(require 'gnosis)

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
