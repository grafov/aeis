;; Another Emacs Initialization Set

;; Copyright © 2012 Alexander I.Grafov

;; Author: Alexander I.Grafov <github@axel.pp.ru>
;; Version: 0.2
;; Keywords: environment initialization
;; URL: http://github.com/siberianlaika/aeis

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Usage:

;; Simple rules are:
;;
;; - place your extensions to 'site-lisp' with `autoload'
;; - place init-scripts calling these extensions to 'init-d'

;;; Commentary:


;;; Code:

;; Set load path
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-to-load-path '("" "site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

;; Load scripts from init-d
(require 'init-d)
(init-d)

;; Provide rest of customization
(require 'init-locale)
(require 'init-keybindings)

;; Variables configured via UI
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(provide 'init)
;;