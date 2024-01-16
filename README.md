
# Gnosis (γνῶσις)

## About

Gnosis (γνῶσις), pronounced "noh-sis", meaning knowledge in Greek, is
a [Spaced Repetition](https://en.wikipedia.org/wiki/Spaced_repetition)
package for GNU Emacs.

### Differences with other SRS
Gnosis does not implement flashcard type review sessions where the
user rates his own answer on an arbitrary scale. Instead implements
"note" types that require user input. Some of these note types, like
the `MCQ` *multiple choice question*, even allow for simulating
real-life exams.

Unlike other SRS implementations for GNU Emacs that rely on
`org-mode`, which I've found not an ideal option for a database
replacement. Instead utilizes an
[sqlite](https://www.sqlite.org/index.html) database, thanks to
[emacsql](https://github.com/magit/emacsql), enabling efficient data
management and manipulation. 

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


## Adding notes

Creating notes for gnosis can be done with a simple
`M-x gnosis-add-note`


Everything is done through the mini-buffer. My personal workflow takes
advantage of that by not really leaving `gnosis-add-note` prompt until
I finish studying a chapter/section, using
[pdf-tools](https://github.com/vedang/pdf-tools "Emacs pdf-tools") or
[nov.el](https://depp.brause.cc/nov.el/ "Emacs nov.el").

### Creating cloze type notes
Cloze type note questions are formatted similarly to Anki's syntax, like so:
> {c1:Cyproheptadine} is a(n) {c2:5-HT2} receptor antagonist used to treat {c2:serotonin syndrome}


*NOTE*: You can also format clozes like Anki if you so prefer; e.g
`{{c1::Cyproheptadine}}`

+ For each `cX`-tag there will be created a cloze type note, the above
  example creates 2 cloze type notes.

+ Each `cX` tag can have multiple clozes, but each cloze must be a
  *UNIQUE* word (or a unique combination of words) in given note.

## Customizing gnosis algorithm

### `gnosis-algorithm-interval`

This is a list of 2 numbers, for the first 2 intervals after a
successful review. For example:

``` emacs-lisp
(setq gnosis-algorithm-interval '(1 3))
```

After first successfully reviewing a note, you will see it again
tomorrow, if you successfully review said note again, the next review
will be after 3 days.

### `gnosis-algorithm-ef`

This is gnosis "easiness factor", it's basically a list that consists
of 3 items. The first item is the increase factor, used to increase
the easiness factor upon successful review. Second item refers to the
decrease factor, used to decrease the easiness factor upon an
unsuccessful review. The third item is the initial total easiness
factor, used to calculate the next interval.

The basic's of how this is used is that it's being multiplied with the
last interval, e.g if you last reviewed a note 6 days ago, and the
easiness factor of this note is 2.0, your next interval would be 6 *
2.0. The next easiness will be 2.0 + increase-factor as well.

For example:

``` emacs-lisp
(setq gnosis-algorithm-ef '(0.3 0.3 1.3))
```

### `gnsois-algorithm-ff`

This is the value of gnosis forgetting factor(ff), it needs to be a floating number below 1. The 101 is that it's used to calculate the next interval upon an unsuccessful review, by being multiplied with last interval, if for a note with a value of last-interval of 6 days and a ff of 0.5, upon an unsuccessful review the next interval will be 6 * 0.5

For example:

``` emacs-lisp
(setq gnosis-algorithm-ff 0.5)
```
