PKG = mastodon

CP = cp
LN = ln

EMACS = emacs
MAKEINFO = makeinfo
INSTALL-INFO = install-info
ORG_DIR = $(word 1,$(wildcard $(HOME)/.emacs.d/elpa/org-9*))
ORG_PATH = -L $(ORG_DIR)
ORG_ARGS = --batch $(ORG_PATH) -l org -l ol-man
ORG_EVAL1 = --funcall org-texinfo-export-to-texinfo
ORG_EVAL2 = --funcall org-texinfo-export-to-info

## ################################################################

.PHONY: infoclean tests testsclean

all: $(PKG).info dir

infoclean:
	rm -f $(PKG).org $(PKG).texi $(PKG).info dir

tests:
	cask emacs -batch -load test/ert-helper.el -f ert-run-tests-batch-and-exit

testsclean:
	cask clean-elc && rm -f stubfile.plstore~

## ################################################################

# May look at this in the future
#
# %.info: %.texi
#	@printf "Generating $@\n"
#	$(MAKEINFO) --no-split $< -o $@
#
# %.texi: %.org
#	@printf "Generating $@\n"
#	$(EMACS) $(ORG_ARGS) $@ $(ORG_EVAL1)

%.info: %.org
	@printf "Generating $@\n"
	$(EMACS) $(ORG_ARGS) $< $(ORG_EVAL2)

dir: $(PKG).info
	printf "Generating $@\n"
	echo $^ | xargs -n 1 $(INSTALL-INFO) --dir=$@

$(PKG).org: README.org
	$(CP) $< $@
