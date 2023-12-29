
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

(defun gnosis-dev-add-fields ()
  "Add random inputs to test."
  (when (y-or-n-p "Add MCQ type?")
    (dotimes (_ 3)
      (gnosis-add-note--mcq :deck "Anatomy"
			    :question "A 37-year-old man is admitted to the
emergency department after a severe car crash. After examining the
patient the emergency medicine physician concludes that the serratus
anterior muscle is damaged. Which of the following nerves innervates
the serratus anterior muscle?"
			    :choices '("Long thoracic" "Axillary" "Spinal accessory" "Dorsal scapular" "Thoracodorsal")
			    :correct-answer 1
			    :extra "The long thoracic is the only nerve that
innervates the serratus anterior. The axillary nerve innervates the
deltoid, the spinal accessory nerve innervates the sternocleidomastoid
and trapezius, the dorsal scapular nerve supplies the rhomboid muscles
and levator scapulae, and the latissimus dorsi is the muscle supplied
by the thoracodorsal nerve."
			    :tags (gnosis-dev-random-items gnosis-dev-tags 2))))
  (when (y-or-n-p "Add Basic type questions?")
    (dotimes (_ 3)
      (gnosis-add-note--basic :deck "Anatomy"
			      :question "A question"
			      :hint "hint"
			      :answer "answer"
			      :extra "extra"
			      :tags (gnosis-dev-random-items gnosis-dev-tags 2))))
  (when (y-or-n-p "Add single cloze type?")
    (dotimes (_ 3)
      (gnosis-add-note--cloze :deck "Anatomy"
			      :note "this is a {c1:note}"
			      :hint "note"
			      :tags (gnosis-dev-random-items gnosis-dev-tags 2)
			      :extra "extra")))
  (when (y-or-n-p "Add mulit cloze type?")
    (dotimes (_ 3)
      (gnosis-add-note--cloze :deck "Anatomy"
			      :note "this is a {c1:note} with multiple {c1:clozes}"
			      :hint "note"
			      :tags (gnosis-dev-random-items gnosis-dev-tags 2)
			      :extra "extra"))))

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
      (setf gnosis-db (emacsql-sqlite-open (concat gnosis-dir "/" "gnosis.db"))))
    (message "Adding testing values...")
    (gnosis-dev-add-fields)
    (message "Done.")))

(provide 'gnosis-dev)
;;; gnosis-dev.el ends here
