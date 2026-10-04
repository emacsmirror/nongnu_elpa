# Installing VM

## From a package

VM releases are available as a NonGNU ELPA package:

```
M-x package-install <ret> vm <ret>
```

Nothing else is needed: the package system compiles the lisp, installs the
manual and arranges the autoloads.

## From source

The source is at <https://gitlab.com/emacs-vm/vm>.

VM needs GNU Emacs 28.1 or later.

### 0. Generate the configure script

`configure` is not in the repository, so from a git checkout run:

```
autoconf
```

### 1. Configure

```
./configure [options]
```

| option | |
| --- | --- |
| `--with-emacs` | the emacs to compile with; may be a path |
| `--prefix` | installation prefix, default `/usr/local` |
| `--with-other-dirs` | directories holding extra emacs-lisp libraries to load while compiling, separated by semicolons |
| `--with-lispdir` | where the lisp files go |
| `--with-etcdir` | where the data files, such as pixmaps, go |
| `--with-docdir` | where the doc files go |
| `--infodir` | where the info files go |

Defaults: lisp in `${prefix}/share/emacs/site-lisp`, data in
`${prefix}/share/vm`, doc files with the data files, info in
`${prefix}/share/info`.

A `.elc` can be stale between versions of Emacs, and gives strange failures
at startup when it is.

Examples:

```
./configure --with-other-dirs=/absolute/path/to/bbdb/lisp
./configure --with-other-dirs="/absolute/path/to/bbdb/lisp;/absolute/path/to/emacs-w3m"
```

VM 8.1.1 and older had `--with-pixmapdir`, now `--with-etcdir`.

### 2. Build

```
make
```

Byte compiler warnings can be ignored.  Messages from `make` itself mean
something is wrong or missing, such as a library.

### 3. Install

If VM has been installed from source before, remove the old files first:

```
make uninstall
```

An earlier install leaves behind any lisp file that this version has renamed
or dropped, along with its `.elc`, and Emacs goes on loading it.  That is a
common cause of odd behaviour after an upgrade, and it does not happen with a
package install, which replaces the lot.

Run it from the tree you installed *from*, if you still have it: `uninstall`
removes the files that build knows about, so the old tree is what knows the
old file names.  If the old tree is gone, delete the installed lisp directory
by hand -- `--with-lispdir` says where, `${prefix}/share/emacs/site-lisp/vm`
by default -- and then install.

Then either use VM from where you built it, or install it.

#### From the build directory

Add the `lisp` and `info` directories to Emacs's search paths.  For a build
in `~/vm`:

```elisp
(add-to-list 'load-path (expand-file-name "~/vm/lisp"))
(add-to-list 'Info-default-directory-list (expand-file-name "~/vm/info"))
(require 'vm-autoloads)
```

Remove any old VM autoloads from your init file; VM arranges its own.

For the manual, add a line to the `dir` file of a user-maintained info
directory, creating `~/vm/info/dir` if you have none:

```
* VM: (vm.info).                  VM Mail Reader
```

#### Into the system directories

```
make install
```

This puts the files where `configure` chose.

VM is then ready.  `C-h i` opens the Emacs Info system, where the VM manual
teaches the rest.

## Companion packages

VM works without these, but reads mail better with them.

* **BBDB**, an address book that runs inside Emacs and can record the
  addresses it sees in your mail.  Compile VM with BBDB in the lisp path,
  then:

  ```elisp
  (require 'bbdb)
  (bbdb-initialize 'vm)
  ```

* **HTML rendering.**  VM can use emacs-w3m or Emacs/W3 inside Emacs, or the
  `lynx` and `w3m` programs as converters to plain text.  It picks the best
  of what it finds, or you can say:

  ```elisp
  (setq vm-mime-text/html-handler 'emacs-w3m)
  ```

  The other values are `emacs-w3`, `w3m` and `lynx`.
