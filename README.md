![build](https://github.com/Anoncheg1/selected-window-contrast/workflows/melpazoid/badge.svg)
[![MELPA](https://melpa.org/packages/selected-window-contrast-badge.svg)](http://melpa.org/#/selected-window-contrast)
![build-stable](https://github.com/Anoncheg1/selected-window-contrast/workflows/melpazoid-release/badge.svg)
[![MELPA Stable](https://stable.melpa.org/packages/selected-window-contrast-badge.svg)](https://stable.melpa.org/#/selected-window-contrast)


# selected-window-contrast - highlighter for current window.

**Version 0.4**
Emacs package, highlights cursor position and selected window by adjusting the brightness of the text and background. The changes are relative, so the functionality works correctly even if the theme is changed.

Able to highlights modeline - configured separately.

# Installation from MELPA
1) Add to `~/.emacs`

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

2) Install via
- by `M-x package-install RET selected-window-contrast RET`
- or `M-x package-list-packages` and by clicking at it.

# Installation from Github directly
1) `git clone --depth 1 https://github.com/Anoncheg1/selected-window-contrast`

2) Add to `~/.emacs`
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "full_path_to_folder/selected-window-contrast")
```

# Configuration
```Elisp
(add-to-list 'load-path "path_to/selected-window-contrast") ; optional
(require 'selected-window-contrast)

(setopt selected-window-contrast-bg-selected 0.95)
(setopt selected-window-contrast-bg-others 0.75)
(setopt selected-window-contrast-text-selected 0.9)
(setopt selected-window-contrast-text-others 0.6)
(add-hook 'buffer-list-update-hook #'selected-window-contrast-highlight-selected-window)
```

To disable highlighting cursor position use: `(setopt selected-window-contrast-cursor-flag nil)`

# Usage for modeline
Dont require hook and configured statically.
```elisp
(progn
  ;; - reset mode-line to default. (optional)
  (set-face-attribute 'mode-line-active nil
                      :background
                      (face-attribute 'mode-line :background)
                      :foreground
                      (face-attribute 'mode-line :foreground))
  ;; - set backgound color (optional)
  (set-face-attribute 'mode-line-active nil :background "cyan4")
  ;; - increate contrast (optional)
  (selected-window-contrast-change-modeline 0.7 0.7)
  ) ; press C-x C-e to execute and test
```
# One time usage
Change contrast in current buffer ``` (selected-window-contrast-change-window 0.7 0.7) ```

# How this works
1) We use buffer-list-update-hook for "cotrast highlightling", that triggered at selectiong window, for "rectangle highlighting" we use `window-selection-change-functions` that demand to be set per buffer.
1) We get color with ```face-attribute (selected-frame)``` for foreground and backgraound.
2) Convert color to HSL
3) adjust brightness in direction of foreground-background average
4) convert color to RGB, then to HEX
5) apply color with ```(buffer-face-set (list :background bg :foreground fg)```. For modeline: ```(set-face-attribute 'mode-line-active```


# Original idea:
```Elisp
(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#111"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)
```
from https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe

# Screenshot
At left - selected window, at right "others" blured window.

![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/selected-window-contrast.png)

# Packages for highlighting window
- https://melpa.org/#/selected-window-accent-mode
- https://melpa.org/#/hiwin
- https://melpa.org/#/solaire-mode
- https://melpa.org/#/auto-dim-other-buffers

# Other packages
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix		https://github.com/Anoncheg1/ediffnw
- Dired history	https://github.com/Anoncheg1/dired-hist
- Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
- Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
- Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
- Restore buffer state		https://github.com/Anoncheg1/emacs-unmodified-buffer1
- Russian celendar		https://github.com/Anoncheg1/emacs-russian-calendar
- outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
- AI chat blocks for Org-mode	https://github.com/Anoncheg1/emacs-oai

# Donate, sponsor author
You can sponsor author crypto money directly with crypto currencies:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25

![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/BTC-1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25.png)

- USDT (Tether TRX-TRON) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN

![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/USDT-TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN.png)

- TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D
