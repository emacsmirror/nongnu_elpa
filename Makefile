.POSIX:
.PHONY: all doc clean
.SUFFIXES: .el .elc

EMACS = emacs
ORG := doc/gnosis.org
TEXI := doc/gnosis.texi
INFO := doc/gnosis.info


all: doc

doc:	$(ORG)
	$(EMACS) --batch \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-texinfo) (org-texinfo-export-to-info) (save-buffer))" \
	--kill

clean:
	rm -f $(TEXI) $(INFO)
