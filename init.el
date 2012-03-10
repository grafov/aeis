(defvar init-path (concat user-emacs-directory "init-d")
	"Directory for init-scripts.")

;;-----------------------------------------------------------------------------
;; Set load path
;;-----------------------------------------------------------------------------
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-to-load-path '("." "site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-helpers)
;(require 'init-ui)

;;----------------------------------------------------------------------------
;; Load init scripts for specific features and modes
;;----------------------------------------------------------------------------
;; Scripts loaded in filenames order (use prefix numbers for sorting,
;; i.e. 10-init-things.el, 50-my-xxx-mode.el etc.)
;;
;; Firstly try to load precached file consists of all active scripts.
(let ((init-cache-file (concat user-emacs-directory ".cache/init-scripts.el")))
	(if (file-readable-p init-cache-file)
			(load init-cache-file nil "Load init-scripts…")
		(progn (make-directory (concat user-emacs-directory ".cache") t)
					 (with-temp-file init-cache-file
						 (dolist (script (init-list-active))
							 (if (byte-compile-file script)
									 (insert-file-contents script))))
						 (byte-compile-file init-cache-file t))))

;;----------------------------------------------------------------------------
;; Load other important features
;;----------------------------------------------------------------------------
(require 'init-keybindings)
(require 'init-locale)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(provide 'init)
;;