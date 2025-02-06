# Copyright (C) 2025 Roi Martin
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
