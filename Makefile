EMACS := emacs
EMACSQ := $(EMACS) -Q
BATCH := $(EMACSQ) --batch \
	  --eval '(setq vc-handled-backends nil)' \
	  --eval '(setq org-startup-folded nil)' \
	  --eval '(setq org-element-cache-persistent nil)' \
	  --eval '(setq gc-cons-threshold (* 50 1000 1000))'

README.html: README.org
	$(BATCH) --find-file $< --eval "(require 'ox-html)" \
	  --eval '(org-html-export-to-html nil nil nil t)'
