run: doomgeneric_emacs.so
	@[ -n "$(wildcard *.wad)" ] || { echo "No *.wad file found. For example copy doom1.wad into this directory."; exit 1; }
	emacs -batch -f batch-byte-compile doom.el && emacs -Q -l doom.elc -e doom

doomgeneric_emacs.so: doomgeneric.zip
	echo "741be927b33eb650dcb20ff6a45251ab3e5cadfbb548e8207525441229f16c44 doomgeneric.zip" | sha256sum -c -
	rm -rf doomgeneric_tmp doomgeneric
	unzip -q doomgeneric.zip '*/doomgeneric/*' -d doomgeneric_tmp
	mv doomgeneric_tmp/*/doomgeneric .
	rm -rf doomgeneric_tmp
	cp emacs-module.h doomgeneric_emacs.c Makefile.emacs doomgeneric/
	cd doomgeneric && $(MAKE) -f Makefile.emacs -j$$(nproc)
	cp doomgeneric/doomgeneric_emacs.so .
	rm -rf doomgeneric_tmp doomgeneric

doomgeneric.zip:
	curl -L -o doomgeneric.zip https://github.com/minad/doom-on-emacs/archive/dcb7a8dbc7a16ce3dda29382ac9aae9d77d21284.zip
