## [0.53] - 2026-11-05

### 📚 Documentation

- Changelog

### ⚙️ Miscellaneous Tasks

- Add dir-locals.
- Cliff.toml -- comment ad hoc filters.
- Makefile, info build/clean, add info files. FIX #355


## [0.52] - 2026-11-05

### 🚀 Features

- Support links to comments (target/anchor links). FIX #345, #343
- Add reply string prior to quote-text in reply comment. #334, FIX #344
- Handle ! references in tag-regex. FIX #347
- Add ! for pr refs. FIX #347.
- Browse url in diff view. FIX #354
- Remove all labels, #349
- Add cliff.toml and changelog for git-cliff.
- Read multiple labels in compose/tabulated list/list issues filter. FIX #349, #350
- Display commit URLs as abbreviated hashes, link to diff. FIX #351

### 🐛 Bug Fixes

- Blockquote tags: use add-face-text-property to not kludge code props. FIX #341
- Fix goto item by number.
- Tag-regex must be digit after # or !.
- Compose font lock use fj-repo-tag-regex. #347
- Tag regex, make slash opt.
- Fix foregejo typos
- Rx regexes, use 1+, alnum
- Cl-remove-duplicates not remove-duplicates.

### 📚 Documentation

- Document quoting using active region in item comments, #343.
- Readme indexes

### 💼 Other

- Item header: propertize assignees, state.
- Add highlight face to code tags. #341
- Quoting in compose: convert code prop to backticks. #341
- Merge pull request 'fix foregejo typos' (#353) from hazery/fj.el:refactor/fix-foregejo-typos into dev
