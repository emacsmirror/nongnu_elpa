###################
Idle Highlight Mode
###################

Simple symbol highlighting package for Emacs.

Available via `melpa <https://melpa.org/#/idle-highlight-mode>`__.


Motivation
==========

The aim for this package is to be fast and light.

While there are many similar packages (see `Other Packages`_) that have the same basic functionality,
they tend towards being heavier by supporting advanced functionality.

For example running operations on every key-stroke or marking symbols over the entire buffer
which can cause poor performance editing large files.

The following guarantees ensure good performance:

- Only update when idle.
- Only operate on visible regions.
- The idle timer is only enabled as needed
  *(it's turned off when the active buffer isn't using ``idle-highlight-mode``).*


Usage
=====

Commands
--------

``idle-highlight-mode``
   Enable idle highlight mode for this buffer.
``idle-highlight-global-mode``
   Enable idle highlight mode for all buffers.


Customization
-------------

Global Settings
^^^^^^^^^^^^^^^

``idle-highlight``
   Face used for highlighting the symbol.
``idle-highlight-exceptions``: ``nil``
   A list of words to exclude from highlighting.

   This may also be set to a function that takes the word as an argument,
   returning non-nil to exclude the word.
``idle-highlight-exceptions-face``: ``'(font-lock-keyword-face font-lock-string-face)``
   Faces to exclude from highlighting (defaults to ignore keywords & strings).

   This may also be set to a function that takes a list of faces,
   returning non-nil to exclude the word.
``idle-highlight-exceptions-syntax``: ``^w_``
   Syntax table to ignore.

   see documentation for ``skip-syntax-forward``, use ``nil`` to skip this check.
``idle-highlight-exclude-point``: ``nil``
   When non-nil, don't highlight the symbol under the cursor.
``idle-highlight-before-point``
   When non-nil, also highlight words directly before the cursor.
``idle-highlight-visible-buffers``: ``nil``
   Apply the current highlighting to all visible buffers.
``idle-highlight-idle-time``: ``0.35``
   Delay before highlighting (in seconds).
``idle-highlight-ignore-modes``: ``nil``
   A list of modes that won't enable idle-highlight from ``idle-highlight-global-mode``.


Buffer Local Settings
^^^^^^^^^^^^^^^^^^^^^

``idle-highlight-global-ignore-buffer``
   When not ``nil``, the buffer won't enable idle-highlight from ``idle-highlight-global-mode``.

   This may also be a function that takes a single buffer argument,
   where returning ``nil`` will enable idle-highlight, anything else will not.

   This example shows idle-highlight being disabled for ORG mode and for read-only buffers.

   .. code-block:: elisp

      (setq idle-highlight-ignore-modes (list 'org-mode))
      (setq idle-highlight-global-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf)))

      (idle-highlight-global-mode)


Installation
============

.. code-block:: elisp

   (use-package idle-highlight-mode
     :config (setq idle-highlight-idle-time 0.2)

     :hook ((prog-mode text-mode) . idle-highlight-mode))

Hints
-----

You may wish to set this to a different value for each mode, e.g:

.. code-block:: elisp

   (add-hook 'after-change-major-mode-hook
     (lambda ()
       (when (derived-mode-p 'c-mode)
         (setq-local idle-highlight-exceptions '("unsigned" "signed" "long" "int" "short" "char")))
       (when (derived-mode-p 'python-mode)
         (setq-local idle-highlight-exceptions '("list" "tuple" "int" "float" "str" "bool")))))


Other Packages
==============

- `auto-highlight-symbol <https://melpa.org/#/auto-highlight-symbol>`__.
- `highlight-symbol <https://melpa.org/#/highlight-symbol>`__.
- `symbol-overlay <https://melpa.org/#/symbol-overlay>`__.
