PKG = fj

CP = cp
LN = ln

EMACS = emacs
MAKEINFO = makeinfo
INSTALL-INFO = install-info
ORG_DIR = $(word 1,$(wildcard $(HOME)/.emacs.d/elpa/org-9*))
ORG_PATH = -L $(ORG_DIR)
ORG_ARGS = --batch $(ORG_PATH) -l org -l ol-man -l fj
ORG_EVAL1 = --funcall org-texinfo-export-to-texinfo
ORG_EVAL2 = --funcall org-texinfo-export-to-info

CASK ?= cask

## ################################################################

.PHONY: infoclean tests

info: $(PKG).info dir

infoclean:
	rm -f $(PKG).org $(PKG).texi $(PKG).info dir

## ################################################################

tests:
	$(CASK) emacs -batch -load test/ert-helper.el -f ert-run-tests-batch-and-exit

build:
	$(CASK) build

clean:
	$(CASK) clean-elc

## ################################################################

%.info: %.org
	@printf "Generating $@\n"
	$(EMACS) $(ORG_ARGS) $< $(ORG_EVAL2)

dir: $(PKG).info
	printf "Generating $@\n"
	echo $^ | xargs -n 1 $(INSTALL-INFO) --dir=$@

$(PKG).org: readme.org
	$(CP) $< $@
