[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/eldoc-mouse-nov.svg)](https://elpa.nongnu.org/nongnu/eldoc-mouse-nov.html)
# eldoc-mouse-nov
Preview content of link of epub file in popup for mouse hover when Emacs nov-mode is used.
### Demo Video
<video src="https://github.com/user-attachments/assets/f4833969-14d9-4724-bafe-0e66384f84c8" controls></video>
### Installation
1. make sure `eldoc-mouse` is installed.
``` elisp
(use-package eldoc-mouse :ensure t
  :bind (:map eldoc-mouse-mode-map
         ("<f1> <f1>" . eldoc-mouse-pop-doc-at-cursor))
  :hook (eglot-managed-mode emacs-lisp-mode nov-mode))
```
2. configure `eldoc-mouse-nov`. Now eldoc-mouse-nov is available on NonGnu ELPA
``` elisp
(use-package eldoc-mouse-nov
     :ensure t
     :after (eldoc-mouse)
     :hook (nov-mode))
```
## Requirements

    Emacs 27.1 or higher
    nov-mode
    eldoc-mouse 3.0.2 or higher

### License

This package is licensed under the GNU General Public License v3 (GPL-3.0-or-later). See the LICENSE file for details.
Contributing

### Contribution
Feel free to open issues and pull requests for improvements. If you encounter any bugs or have feature requests, please create an issue on the GitHub Issues page.

Author

Huang Feiyu sibadake1@163.com
