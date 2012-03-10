AEIS is «Another Emacs Initialization Set».

It's not a kind of a «starter kit» bloated with packages unused by you. AEIS just offers skeleton for further Emacs customizing.

Planned features:

* API for simple installing/deinstalling/enabling/disabling init scripts for installed packages `[in progress]`
* UI for control init-system
* support for different Emacs versions for single .emacs.d
* caching of init-d starter scripts in one big file `[done]`

**Developers preview. Not for users yet.**


Directories structure
=====================

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
* 23, 24 — configuration (init-scripts & modules) specific for these major versions of Emacs
