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

CLIFF = $$(git-cliff -l)
CLIFF_U = $$(git-cliff -u)
CAT = $$(cat changelog.md)

CLIFF_CAT = $(CLIFF)$(CAT)
## ################################################################

.PHONY: infoclean tests changes

info: $(PKG).info dir

infoclean:
	rm -f $(PKG).org $(PKG).texi $(PKG).info dir

changes:
	echo "$(CLIFF)\n\n$(CAT)" > changelog.md && pandoc -i changelog.md -o changelog.org

changes-unreleased:
	echo "$(CLIFF_U)\n\n$(CAT)" > changelog.md && pandoc -i changelog.md -o changelog.org

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
