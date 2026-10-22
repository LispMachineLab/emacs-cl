emacs-cl
========

Emacs Common Lisp is an implementation of Common Lisp, written in Emacs Lisp.

This is the LispMachineLab fork of [Lars Brinkhoff's
emacs-cl](https://github.com/larsbrinkhoff/emacs-cl).  It runs on
Emacs 30 and 31, where upstream stopped at Emacs 24, and is maintained
to run existing Common Lisp programs unchanged: MYCIN of Norvig's
*Paradigms of Artificial Intelligence Programming* and Winograd's
SHRDLU run on it in [LispM Desktop](https://github.com/LispMachineLab/lispm-desktop).

Common Lisp runs in a batch Emacs beside the one you work in, so that
it neither changes your Emacs nor holds it up:

- `M-x emacs-cl-repl` opens a Common Lisp listener.
- `emacs-cl-start` starts a program in a comint buffer of your own:

  ```elisp
  (emacs-cl-start (get-buffer-create "*MYCIN*")
                  :load '("auxfns.lisp")
                  :eval '("(requires \"mycin\" \"mycin-r\")" "(mycin)"))
  ```

The first start compiles Common Lisp into `emacs-cl-build-directory`
(`~/.emacs.d/emacs-cl/`), which takes a few seconds; it is compiled
again only when the sources or Emacs's version change.  To use it
inside your Emacs instead, load `cl/load-cl.el`.

`make test` runs the tests of the package, `make check` Common Lisp's
own tests in `cl/tests.lisp`.  See README for the original notes on
the implementation.
