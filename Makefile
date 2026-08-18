run: doomgeneric_emacs.so
	@[ -n "$(wildcard *.wad)" ] || [ -n "$(wildcard /usr/share/games/doom/*.wad)" ] || { echo "No *.wad file found"; exit 1; }
	emacs -batch -f batch-byte-compile doom-game.el && emacs -Q -l doom-game.elc -e doom

doomgeneric_emacs.so: doomgeneric.zip
	rm -rf doomgeneric_tmp doomgeneric
	unzip -q doomgeneric.zip '*/doomgeneric/*' -d doomgeneric_tmp
	mv doomgeneric_tmp/*/doomgeneric .
	rm -rf doomgeneric_tmp
	cp emacs-module.h doomgeneric_emacs.c Makefile.emacs doomgeneric/
	cd doomgeneric && $(MAKE) -f Makefile.emacs -j$$(nproc)
	cp doomgeneric/doomgeneric_emacs.so .
	rm -rf doomgeneric_tmp doomgeneric

doomgeneric.zip:
	curl -# -L -o doomgeneric_tmp.zip https://github.com/minad/doom-on-emacs/archive/dcb7a8dbc7a16ce3dda29382ac9aae9d77d21284.zip
	echo "741be927b33eb650dcb20ff6a45251ab3e5cadfbb548e8207525441229f16c44 doomgeneric_tmp.zip" | sha256sum -c - || { rm -f doomgeneric_tmp.zip; exit 1; }
	mv doomgeneric_tmp.zip doomgeneric.zip

clean:
	rm -f *.elc *.so doomgeneric*.zip
