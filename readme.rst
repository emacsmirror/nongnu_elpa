##################
Emacs (Re)Complete
##################

Immediate completion, without prompting.

Unlike most completion, ``recomplete`` performs the completion action,
calling again to cycle over options.

This has the following characteristics:

- Completion options are displayed in the echo-area.
- Only ever adds a single undo step.
- Supports user defined completion *(needs to be documented)*.

Available via `melpa <https://melpa.org/#/recomplete>`__.


Motivation
==========

To be able to quickly complete or correct words in cases when the first solution is acceptable.

If the first answer isn't desired, other items can still be selected conveniently for short lists.


Usage
=====

This package exposes the following interactive functions:

- ``recomplete-ispell-word``
- ``recomplete-case-style``
- ``recomplete-case-style-symbol``
- ``recomplete-dabbrev``


On successive calls these commands cycle to the next item.
To cycle in the reverse press ``[keyboard-quit]`` (Ctrl-G),
which causes the next completion command to reverse the direction.

A numeric prefix argument controls how many steps to cycle in a single press (default 1).
Negative values cycle backward, and values that exceed the number of options wrap via modulo.


Built-In Completions
--------------------

``ispell-word``
   This uses ``ispell-word`` to provide a list of options for correcting spelling.
``case-style``
   This cycles between different styles of case:

   :Pascal Case: ``TheRedFoxJumpedOverTheLazyDog``
   :Snake Case: ``the_red_fox_jumped_over_the_lazy_dog``
   :Kebab Case: ``the-red-fox-jumped-over-the-lazy-dog``
``case-style-symbol``
   The same as ``case-style`` however this operates on the current ``symbol``.
   *Kebab Case* is only used when the language considers ``-`` part of a symbol (Lisp, e.g. not C/C++/Python).
``dabbrev``
   Provides similar functionality to ``dabbrev-completion``.


Key Bindings
------------

You will need to map these to keys yourself.

Key binding example for Emacs default layout, using ``Alt-P``:

.. code-block:: elisp

   (global-set-key (kbd "M-p") 'recomplete-ispell-word)

Key binding example for evil-mode layout, using ``Alt-Z``:

.. code-block:: elisp

   (global-unset-key (kbd "M-z"))
   (define-key evil-normal-state-map (kbd "M-z") 'recomplete-ispell-word)

If you want to bind a key directly to cycling in the reverse direction
it can be done using ``-1`` for the prefix argument.

Key binding example, using ``Alt-Shift-P``:

.. code-block:: elisp

   (global-set-key (kbd "M-P")
     (lambda ()
       (interactive)
       (let ((current-prefix-arg '(-1)))
         (call-interactively 'recomplete-ispell-word))))


Customization
-------------

``recomplete-single-line-display``
   Limit display completion options to a single line, centered around the current item.


Details
=======

- Results are cached for fast execution.
- Any non ``recomplete`` command breaks the completion chain.


TODO
====

- Document writing custom completions,
  although existing examples can be used for reference.


Limitations
===========

Calling ``recomplete`` from macros (including ``evil-repeat``)
runs the complete action but doesn't support cycling.


Installation
============

The package is `available in melpa <https://melpa.org/#/recomplete>`__ as ``recomplete``.

.. code-block:: elisp

   (use-package recomplete)

Combined with key bindings, for evil-mode:

.. code-block:: elisp

   (use-package recomplete
     :config
     (define-key evil-normal-state-map (kbd "M-z") 'recomplete-ispell-word))


Other Packages
==============

`flyspell <https://www.emacswiki.org/emacs/FlySpell>`__
   This package provides ``flyspell-auto-correct-word`` which supports cycling over corrections.
   Although it has the down-side of adding undo steps while cycling over options,
   and it is not generalized for different kinds of corrections/completion.
