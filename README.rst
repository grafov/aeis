Set of scripts for simple Emacs customization.

It's not kind of a «starter kit» bloated with packages unused by you. The `init-d' offers simple skeleton for further Emacs customizing.

**Development version. Not for users yet.**


Directory structure
===================

::

  .emacs.d/
    |
    +-- .cache (mandatory)
    |
    +-- init-d (mandatory)
    |
    +-- site-lisp (mandatory)
    |
    +-- 23 (optional)
    |    |
    |    +- init-d
    |    |
    |    \-- site-lisp
    |
    \-- 24 (optional)
    ...


* init-d — init-scripts implicitly loading a required features from `load-path`
* site-lisp — appendix to `load-path` for local installed modules (use lazy load here)
* 23, 24 — configuration (init-scripts & modules) specific for these versions of Emacs
