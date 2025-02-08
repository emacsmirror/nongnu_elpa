
##########
Change Log
##########

- Development (2025-02-09)

  - Fix #4: Message in site-file causes multi-process diff to fail.
  - Fix ``ydiff`` defaulting to 80 column width.
  - Fix an error, attempting to use a closed buffer when ``diff-ansi-method`` is set to ``'progressive``.

- Version 0.2 (2022-07-08)

  - Add ``diff-ansi-verbose-progress`` to support showing progress for progressive ansi conversion.
  - Fix error running the ``progressive`` timer that could be canceled by external actions.

- Version 0.1 (2021-10-21)

  Initial release.
