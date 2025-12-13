# geiser-gauche
[![MELPA](https://melpa.org/packages/geiser-gauche-badge.svg)](https://melpa.org/#/geiser-gauche)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://spdx.org/licenses/BSD-3-Clause.html)

[Gauche Scheme](http://practical-scheme.net/gauche/) support for [Geiser](https://gitlab.com/jaor/geiser/). 

**Table of contents**
- [Description](#description)
- [Requirements](#requirements)
- [Installation and setup](#installation-and-setup)
- [Credits](#credits)
- [License](#license)

## Description

This package adds support for the Gauche Scheme implementation to Geiser, a generic Scheme interaction mode for the GNU Emacs editor. 

Currently the following Geiser features are supported:
+ evaluation of sexps, definitions, regions and whole buffers,
+ loading Scheme files,
+ macroexpansion,
+ identifier completion,
+ short symbol and module documentation,
+ detailed symbol and module documentation lookup in the Gauche Info manual.

## Requirements

+ Gauche 0.9.6 or later.
+ Geiser 0.11.2 or later.

## Installation and setup

1. Install the package, which is available in the [MELPA package
repository](https://melpa.org) and can be installed using Emacs’s built-in
package manager, `package.el`.
2. Add Gauche to Geiser's list of active Scheme implementations in your Emacs init file. This can be done, e.g., by adding
```emacs-lisp
(with-eval-after-load 'geiser-impl
  (add-to-list 'geiser-active-implementations 'gauche))
```
or, using `use-package`
```emacs-lisp
(use-package geiser-gauche
  :after geiser
  :init (add-to-list 'geiser-active-implementations 'gauche))
```
Naturally, if the `geiser-active-implementations` variable is already customized in the init file then one can simply add `'gauche` to the set value, e.g. replace 
```emacs-lisp
(setq geiser-active-implementations '(guile chez))
```
with 
```emacs-lisp
(setq geiser-active-implementations '(guile chez gauche))
```
3. If Gauche's `gosh` binary is not in the path for Emacs then the variable
   `geiser-gauche-binary` also has to be set in the init file, e.g., with
```emacs-lisp
(setq geiser-gauche-binary /path/to/gosh)
```

## Credits
Thanks to [Jao](https://gitlab.com/jaor) and his co-developers for creating and maintaining Geiser, and for helping the development of this package. Thanks also to Shiro Kawai and his co-developers for Gauche, the _raison d'être_ of this package.

I'd also like to thank Fabian Brosda, Toshi Umehara and Visuwesh for their code contributions.

## License
Copyright (C) 2020-2025 András Simonyi

Authors: András Simonyi

This program is free software; you can redistribute and/or modify it under the terms of the BSD 3-Clause "New" or "Revised" License. You should have received a copy of the license along with this program. If not, see https://spdx.org/licenses/BSD-3-Clause.html.
