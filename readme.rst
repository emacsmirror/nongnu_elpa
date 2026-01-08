#####################
Emacs Doc Show Inline
#####################

Shows doc-strings inline (in the source code) for projects that contain doc-strings in their public headers.

External doc-strings are displayed above functions & variables with syntax highlighting,
with a modified background color so they are distinct from the editable code.

.. PNG images.

.. figure:: https://codeberg.org/attachments/250a5183-e1f1-40a2-a4ab-de871f49c77e
   :target: https://codeberg.org/attachments/a6d8b446-33e1-4132-b0b4-d1259c62a7a9

   Source file (left) showing public doc-strings inline (right).

Available via `melpa <https://melpa.org/#/doc-show-inline>`__.


Motivation
==========

Supporting having doc-strings always visible alongside implementations,
without having to switch files to be aware of their intended usage while browsing or editing a code-base.


Usage
=====

This package provides a minor mode which can be enabled via a mode hook or toggled via a key shortcut,
which may be preferable especially for very large doc-strings which could be intrusive.


Customization
-------------

``doc-show-inline-idle-delay-init`` (float ``1.0``)
   The idle delay to use before activating for the first time.

``doc-show-inline-idle-delay`` (float ``0.75``)
   The idle delay to use before updating doc-strings.

``doc-show-inline-exclude-blank-lines`` (integer ``0``)
   Exclude doc-strings when there are at least this many blank lines between the declaration
   and the doc-string (zero disables).

``doc-show-inline-exclude-regexp`` (regexp / nil ``nil``)
   Optionally exclude comments that contain text matching this regular expression.

   This can be useful for excluding comments which define function grouping or other kinds of file sections,
   which aren't intended to represent documentation for the following symbol.

``doc-show-inline-face-background-highlight`` (float ``-0.04``)
   The tint to use when generating the background color for doc-string overlays.
   The value is between -1.0 and 1.0 where negative numbers darken, positive numbers brighten.

``doc-show-inline-face``
   The face to use for the background (other faces get higher priority).

   When left unchanged the background will be calculated based on the current theme using
   ``doc-show-inline-face-background-highlight``.

``doc-show-inline-use-logging`` (boolean ``nil``)
   When non-nil, log details about lookups to ``*doc-show-inline-log*``,
   use this to troubleshoot any issue when a doc-string can't be found unexpectedly.


Advanced Customization
----------------------

``doc-show-inline-buffer-hook``
   Run this hook when loading a new buffer.

   This replaces major mode hooks as well as ``after-change-major-mode-hook``.
   This is done to avoid enabling unnecessary minor-modes only for the purpose of extracting comments.

``doc-show-inline-fontify-hook``
   Run before extraction, functions take two arguments for the comment range.
   Intended for any additional highlighting such as tags or spell checking text,
   note that highlighting from overlays are also supported.


Example
^^^^^^^

For simply extracting comments, there is no need to use either of these hooks.
However you may want to spell check the comments or highlight tags such as ``TODO``.

The following example shows how text can be spell-checked and tags can be highlighted
via the `spell-fu <https://melpa.org/#/spell-fu>`__ & `hl-prog-extra <https://melpa.org/#/hl-prog-extra>`__  packages.

.. code-block:: elisp

   ;; Enable modes that support comment highlighting.
   (add-hook 'doc-show-inline-buffer-hook
     (lambda ()
       (hl-prog-extra-mode)
       (spell-fu-mode)))

   ;; Ensure the comment region is spell checked.
   (add-hook 'doc-show-inline-fontify-hook
     (lambda (beg end)
       (spell-fu-region beg end)))


Function Configuration
----------------------

These functions can be overridden to adjust behavior.

``doc-show-inline-filter``
   Takes the symbol and returns non-nil if the symbol should have its documentation displayed.
   In general it's harmless to set this to ``'identity`` which won't filter out anything.

   However some symbols such as ``#define`` statements in C/C++ don't make sense to handle,
   so by default they are ignored to avoid unnecessary overhead.

``doc-show-inline-locations`` (symbol ``'doc-show-inline-locations-default``)
   A function that returns a list of ``(symbol . point)`` cons cells,
   containing symbols and their starting positions in the current buffer.

   By default ``imenu`` is used for this purpose.

``doc-show-inline-extract-doc`` (symbol ``'doc-show-inline-extract-doc-default``)
   The function to extract the doc-string given the destination buffer.
   The buffer and point will be the destination (the header file for example).
   The function must return a ``(BEG . END)`` cons cell representing the range or nil on failure.

   By default this scans backwards for the comment above the function.


Details
=======

- This package only directly depends on ``imenu`` and ``xref``,
  so any languages that store function definitions in external files will work.
- While there are no direct dependencies on ``lsp-mode``,
  this has been tested using ``lsp-mode`` and ``eglot`` with both ``ccls`` and ``clangd`` backends.
- Documentation overlays are added while idle.
- Any comment before the function is considered its doc-string,
  *(unless it's the trailing comment of a non-blank line).*


Installation
============

The package is `available in melpa <https://melpa.org/#/doc-show-inline>`__ as ``doc-show-inline``.

Example ``use-package`` that enables for C/C++ modes.

.. code-block:: elisp

   (use-package doc-show-inline
     :commands (doc-show-inline-mode)

     :config
     (define-key c-mode-map (kbd "C-;") 'doc-show-inline-mode)
     (define-key c++-mode-map (kbd "C-;") 'doc-show-inline-mode)

     :hook ((c-mode . doc-show-inline-mode)
            (c++-mode . doc-show-inline-mode)))
