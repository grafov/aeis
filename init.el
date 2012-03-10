;; Another Emacs Initialization Set

;; Set load path
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-to-load-path '("" "site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-d)

(init-d)

(require 'init-keybindings)
(require 'init-locale)

;; Variables configured via the interactive 'customize' interface
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(delete-other-windows) ;; hide *Compiler log* output
(provide 'init)
;;