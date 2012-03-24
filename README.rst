AEIS is «Another Emacs Initialization Set»
==========================================

**Developers preview. Not for users yet.**

AEIS purpose is ordering a chaos of `emacs.d` scripts and extensions loaded and
installed by hands or with package-managers. It's not a kind of a «starter
kit» bloated with packages unused by you. AEIS just offers skeleton for
further Emacs customizing.

Planned features
----------------

 * API for simple installing/deinstalling/enabling/disabling init scripts for installed packages `[in progress]`
 * UI for control init-system
 * support for different Emacs versions for single .emacs.d
 * caching of init-d starter scripts in one big file `[done]`


Directories structure
---------------------

::

  .emacs.d/ (USER-EMACS-DIRECTORY)
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


Interactive functions
---------------------

* init-version — shows version of AEIS
* init-disable — disable selected script (autocompleted by script names from init-d) and reset init cache
* init-enable — enable selected script (autocompleted by script names from init-d) and reset init cache
* init-reload-all — rebuild cache and reload all init-scripts
* init-edit-script — visit init-script file

No keybindings yet.
