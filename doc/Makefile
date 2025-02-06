CSS_REF ?= https://wip.jroi.dev/css/style.css

.PHONY: all
all: radio.info radio.pdf radio_html

radio.info: radio.texi
	makeinfo --output=radio.info radio.texi

radio.pdf: radio.texi
	makeinfo --pdf --output=radio.pdf radio.texi

radio_html: radio.texi
	makeinfo --html --css-ref=$(CSS_REF) --output=radio_html radio.texi

.PHONY: clean
clean:
	rm -rf radio.info radio.pdf radio_html
