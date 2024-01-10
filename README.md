
# Gnosis 

## About
*Gnosis is currently under development*

Gnosis is a [Spaced
Repetition](https://en.wikipedia.org/wiki/Spaced_repetition) package
for GNU Emacs.

## Installation
### Straight.el
``` emacs-lisp
(straight-use-package 
 '(gnosis :type git
	      :host nil
	      :repo "https://git.thanosapollo.org/gnosis"))
```

### Manual
``` shell
$ git clone https://git.thanosapollo.org/gnosis
```

*Add this to your emacs configuration:*

``` emacs-lisp
   (add-to-list 'load-path "/path/to/gnosis")
   (load-file "~/path/to/gnosis.el")
   (require 'gnosis)
```
