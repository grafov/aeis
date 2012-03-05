;;-----------------------------------------------------------------------------
;; Set load path
;;-----------------------------------------------------------------------------
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-to-load-path '("." "site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

;;----------------------------------------------------------------------------
;; Load init scripts for specific features and modes
;;----------------------------------------------------------------------------
;; scripts loaded in filenames order (use prefix numbers for sorting,
;; i.e. 10-init-things.el, 50-my-xxx-mode.el etc.)
(let ((user-init-path (concat user-emacs-directory "init-d/*.el")))
(condition-case nil
    (mapc 'load (sort (file-expand-wildcards user-init-path) 'string<))
  (error nil))
)

;;----------------------------------------------------------------------------
;; Load keybindings
;;----------------------------------------------------------------------------
(require 'init-keybindings)

(require 'init-locale)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;;;(if (not (file-exists-p "/var/run/emacs/axel/emacs.pid"))
;;;		(server-start))

(provide 'init)
;;

