
CLIFF = $$(git-cliff -l)
CLIFF_U = $$(git-cliff -u)
CAT = $$(cat changelog.md)
## ################################################################

.PHONY: changes changes-unreleased

changes:
	echo "$(CLIFF)\n\n$(CAT)" > changelog.md && pandoc -i changelog.md -o changelog.org

changes-unreleased:
	echo "$(CLIFF_U)\n\n$(CAT)" > changelog.md && pandoc -i changelog.md -o changelog.org
