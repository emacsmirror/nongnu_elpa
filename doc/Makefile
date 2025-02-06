CSS_REF ?= https://wip.jroi.dev/css/style.css

.PHONY: all
all: radio.info radio.pdf radio.html

radio.info: radio.texi
	makeinfo --output=radio.info radio.texi

radio.pdf: radio.texi
	makeinfo --pdf --output=radio.pdf radio.texi

radio.html: radio.texi
	makeinfo --html --no-split --css-ref=$(CSS_REF) --output=radio.html radio.texi

.PHONY: clean
clean:
	rm -f radio.info radio.pdf radio.html
