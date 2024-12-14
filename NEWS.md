## 1.0 (2024-12-14)

- `eldoc-diffstat` is now available on [NonGNU ELPA][].
- `eldoc-diffstat-setup` has been replaced by a minor mode
  `eldoc-diffstat-mode`; there's also a globalized minor mode
  `global-eldoc-diffstat-mode` that (by default) will turn on diffstat output in
  all supported major modes.
- `eldoc-diffstat-lines` can be used to truncate/pad output to a specific number
  of lines.
- When this package is loaded, relevant buffer-navigating commands in magit
  buffers are set up to trigger eldoc.

[NonGNU ELPA]: https://elpa.nongnu.org/nongnu/eldoc-diffstat.html
